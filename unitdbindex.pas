{ LazGitGui: An interface to git status with some additional tools
             and with a familiar git gui interface.

  Copyright (C) 2023 Jesus Reyes Aguilar (jesusrmx@gmail.com)

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.

  Handles the log database
}
unit unitdbindex;

{$mode ObjFPC}{$H+}

{$define Debug}

{$ifdef Debug}
  {.$define DebugTopoFilter}
  {.$define ReportGetParentsMap}
{$endif}

interface

uses
  Classes, SysUtils, Math, DateUtils, fgl, LazLogger,
  unitgittypes, unitifaces, unitgitutils, unittoposort;

const
  FIELD_DATE        = 0;
  FIELD_PARENTOID   = 1;
  FIELD_COMMITOID   = 2;
  FIELD_AUTHOR      = 3;
  FIELD_EMAIL       = 4;
  FIELD_SUBJECT     = 5;

  PGM_VERSION       = $0001;
  PGM_SIGNATURE     = $1A2A;

  LINE_SOURCE_COLUMN  = -1;

type

  TLogItem = record
    CommiterDate: Int64;
    ParentOID, CommitOID,
    Author,
    Email,
    Subject: RawByteString;
  end;

const
  SIZEOF_INDEX = sizeof(TIndexRecord);

type

  { TDbIndex }

  TDbIndex = class(TMyInterfacedObject, IDbIndex)
  private
    fUpdated: Boolean;
    fHead: Boolean;
    fMaxBufferSize: Integer;
    fBuffer: Pchar;
    function GetFileName(aIndex: Integer): string;
  private
    fDir: string;
    fCacheStream: TFileStream;
    fIndexStream: TFileStream;
    fMaxRecords: Integer;
    fOldCount: Integer;
    fOldIndexSize: SizeInt;
    fCacheUpdate: boolean;
    fOldIndexOffset: Int64;
    //fLoadedItemIndex: Integer;
    //fItem: TLogItem;
    fReadOnly: boolean;
    fFilter: TIntArray;
    fCursorLock: TRTLCriticalSection;
    function GetAcceptingNewRecords: boolean;
    function GetActive: boolean;
    function GetInfo: string;
    //procedure RecoverIndex;
    procedure ReIndex;
    function  GetRealIndex(var aIndex:Integer; unfiltered:boolean): Integer;
    procedure ItemFromCacheBuffer(buffer: pchar; out aItem: TLogItem);
    procedure ItemFromLogBuffer(aBuf: PChar; out aItem: TLogItem);
    procedure DumpItem(aIndex: Integer; var item: TLogItem);
    procedure ThreadStart(aHead: boolean);
    procedure ThreadDone;
    procedure ThreadStore(buf: pchar; size: Integer);
    function  GetCachedItem(aIndex: Integer; unfiltered, tryToFix:boolean; out aItem:TLogItem): boolean;
    function  ReadIndexBuffer(realIndex:Integer): boolean;
    function  SetupFilter(arr: TIntArray): boolean;
  public
    constructor Create(dir: string);
    destructor Destroy; override;
    class procedure CacheBufferFromItem(const aItem: TLogItem; out aBuf: PChar; out len: word);

    procedure Open;
    function LoadItem(aIndex: Integer; out aItem: TLogItem; unfiltered:boolean=false): boolean;
    procedure SetFilter(arr: TIntArray);
    procedure ReplaceFilter(arr: TIntArray);
    function FindCommitSha(sha: string; startAt:Integer=-1): Integer;
    function Count(unfiltered: boolean = false): Integer;
    function GetIndex(aIndex: Integer): Integer;

    //property Item: TLogItem read fItem;
    property Info: string read GetInfo;
    property ReadOnly: boolean read fReadOnly write fReadOnly;
    property AcceptingNewRecords: boolean read GetAcceptingNewRecords;
    property MaxRecords: Integer read fMaxRecords write fMaxRecords;
    property Active: boolean read GetActive;
    property Updated: boolean read fUpdated;
    property Filter: TIntArray read fFilter;
  end;

  function GetParentsMap(fDb: TDbIndex): TParentsMap;
  procedure ReportGetParentsMap(parMap: TParentsMap);
  procedure ClearParentsMap(map: TParentsMap);

  procedure ReportRelatives(fIndexArray: TItemIndexArray);

var
  gblInvalidateCache: boolean = false;
  gblRecordsToUpdate: Integer = 25;
  gblCacheScreens: Integer = 11;
  gblRecordsToRowCount: Integer = 10;
  gblCutterMode: boolean = false;
  gblTopologicalMode: boolean = true;
  gblAllowDeleteChanged: boolean = false;

implementation

const
  BASE_FILENAME     = 'lazgitgui';
  FILENAME_INFO     = 1;
  FILENAME_INDEX    = 2;
  FILENAME_CACHE    = 3;
  FILENAME_INDEXTMP = 4;


procedure ReportGetParentsMap(parMap: TParentsMap);
var
  pmi: PParentsMapItem;
  mind: TIntArray = nil;
  i, j: Integer;
begin
  SetLength(mind, parMap.Count);
  DebugLn;
  DebugLn('PARENTS');
  for i:=0 to parMap.Count-1 do begin
    pmi := parMap.Data[i];
    mind[pmi^.n] := i;
  end;
  for i:=0 to Length(mind)-1 do begin
    pmi := parMap.Data[mind[i]];
    DbgOut('%3d: %.16x => ',[pmi^.n, parMap.Keys[mind[i]]]);
    for j:=0 to Length(pmi^.parents)-1 do
      DbgOut('%.16x ',[pmi^.parents[j].commit]);
    DebugLn;
  end;

  ReportTicks('Reporting Parents OID');
end;

// Given a Index database, create a map where the key is the commit OID
// (a CommitOID limited to 16 characters packed in a QWord) and the data
// it's an array of parents of that commit. This map can be used later
// to find indexes inheritance.
//
// TODO: check if using full OIDs (strings) is really slower than using QWords
function GetParentsMap(fDb: TDbIndex): TParentsMap;
var
  i, j, k, aIndex: Integer;
  pmi, found: PParentsMapItem;
  elements: TParentElementArray;
  lost: array of PParentsMapItem;
  aItem: TLogItem;
begin

  // we scan the db from older to newer commits and supposedly the
  // parents of a newer commit have already seen, however sometimes we
  // found parents that we have not seen yet, we record those in
  // the lost array. At the end we resolve all missing parents.
  lost := nil;

  // the parents map is created, as we use map.find, the map have
  // to be sorted.
  result := TParentsMap.Create;
  result.Sorted := true;

  // scan the db index from older to newer commits so we see the
  // items that will become the parents of the next items.
  for i:=fDb.Count-1 downto 0 do begin

    // load a item whose properties are available through 'Item'
    fDb.LoadItem(i, aItem);
    with aItem do begin

      // create a new map item for the Ith element, the key is
      // the current (limited) commit OID. The data is the current
      // db index and initialy no parents.
      new(pmi);
      pmi^.n := i;
      pmi^.parents := nil;
      pmi^.lostandfound := -1;
      result.Add(OIDToQWord(CommitOID), pmi);

      // convert the list of parents of the current commit (a space separated
      // list of string sha1s) into an array of limited commit OIDs
      elements := OIDToParentElements(parentOID, Length(CommitOID));

      // we expect to find all parents, so pre allocate an array big enough.
      SetLength(pmi^.parents, Length(elements));

      // we need a counter so we know if all parents have already seen
      k := 0;

      // process the list of parents and try to locate the corresponding
      // db indices from the already seen commits. If not found, mark it.
      for j:=0 to Length(elements)-1 do begin
        if result.Find(elements[j].commit, aIndex) then begin
          found := result.Data[aIndex];
          pmi^.parents[j].n := found^.n;
          pmi^.parents[j].commit := elements[j].commit;
          inc(k);
        end else begin
          {$IFDEF Debug}
          DebugLn('At %d [%d] (%.16x) parent %d (%.16x) is missing',
            [i, fdb.GetIndex(i), OIDToQWord(CommitOID), j, elements[j].commit]);
          {$ENDIF}
          pmi^.parents[j].n := -1;
          pmi^.parents[j].commit := elements[j].commit;
        end;
      end;

      // if the ith index has missing parents, add it to the lost list.
      if Length(pmi^.parents)<>k then begin
        aIndex := Length(lost);
        SetLength(lost, aIndex+1);
        lost[aIndex] := pmi;
      end;

    end;
    finalize(aItem);
  end;

  // Now that we have processed all db indices, try to find lost parents
  for i := 0 to Length(lost)-1 do begin
    pmi := lost[i];
    for k:=0 to Length(pmi^.parents)-1 do
      // is this a missing parent?
      if pmi^.parents[k].n<0 then begin
        // yes, now try to find it
        if result.Find(pmi^.parents[k].commit, aIndex) then begin
          // found, mark it as not lost
          found := result.Data[aIndex];
          pmi^.parents[k].n := found^.n;
          if pmi^.lostandfound>=0 then
            DebugLn('At %d lost parent %d (%.16x) lost&found at %d, found again at %d',
              [pmi^.n, k, pmi^.parents[k].commit, pmi^.lostandfound, found^.n]);
          pmi^.lostandfound := found^.n;
          {$IFDEF Debug}
          DebugLn('At %d lost parent %d (%.16x) found to be at %d [%d]',
            [pmi^.n, k, pmi^.parents[k].commit, found^.n, fdb.GetIndex(found^.n)]);
          {$ENDIF}
        end;
      end;
  end;

  {$ifdef Debug}
  ReportTicks('GetParentsMap');
  {$ifdef ReportGetParentsMap}
  ReportGetParentsMap(result);
  {$endif}
  {$endif}
end;

// free the resources contained in the map
procedure ClearParentsMap(map: TParentsMap);
var
  pmi: PParentsMapItem;
  i: Integer;
begin
  for i:=0 to map.Count-1 do begin
    pmi := map.Data[i];
    pmi^.parents := nil;
    dispose(pmi);
  end;
  map.free;
  {$IFDEF Debug}
  ReportTicks('ClearingMap');
  {$ENDIF}
end;

procedure ReportRelatives(fIndexArray: TItemIndexArray);
var
  i, j, mxp: Integer;
  s: string;
begin
  mxp := 0;
  for i:=0 to Length(fIndexArray)-1 do begin
    if Length(fIndexArray[i].parents)>mxp then
      mxp := Length(fIndexArray[i].parents);
  end;

  DebugLn;
  DebugLn('Relatives report');
  for i:=0 to Length(fIndexArray)-1 do begin
    DbgOut('%4d [%4d]: ', [i, fIndexArray[i].index]);
    s := '';
    for j:=0 to Length(fIndexArray[i].parents)-1 do
      s += format('%4d ', [fIndexArray[i].parents[j]]);
    DbgOut(s.PadRight((mxp+1)*4));
    DbgOut(' | ');
    for j:=0 to Length(fIndexArray[i].childs)-1 do
      DbgOut('%4d ', [fIndexArray[i].childs[j]]);
    DebugLn;
  end;
  ReportTicks('Reporting Relatives');
end;



{ TDbIndex }

//procedure TDbIndex.RecoverIndex;
//var
//  w: word;
//  IndxRec: TIndexRecord;
//  n: Integer;
//begin
//  DebugLn('Recovering Index');
//  n := 0;
//  fIndexStream.Clear;
//  fCacheStream.Position := SizeOf(cardinal);
//  while fCacheStream.Position<fCacheStream.Size do begin
//    IndxRec.offset := fCacheStream.Position;
//    w := fCacheStream.ReadWord;
//    IndxRec.size := w + SizeOf(word);
//    fIndexStream.WriteBuffer(indxRec, SIZEOF_INDEX);
//    inc(n);
//    fCacheStream.Seek(w, soFromCurrent);
//  end;
//  DebugLn('Recovered %d records vs %d',[fIndexStream.Size div SIZEOF_INDEX, n]);
//  DebugLn('Cache: Size=%d Position=%d',[fCacheStream.Size, fCacheStream.Position]);
//  SaveIndexStream;
//end;

function TDbIndex.GetAcceptingNewRecords: boolean;
begin
  result := not fReadOnly;
end;

function TDbIndex.GetActive: boolean;
begin
  result := fCacheStream<>nil;
end;

function TDbIndex.GetInfo: string;
begin
  result := format('%d',[Count(true)]);
end;

procedure TDbIndex.ReIndex;
var
  newSize: Integer;
  buf, p, q: Pbyte;
  tmp, x: TStream;
begin
  EnterCriticalSection(fCursorLock);
  try
    tmp := TMemoryStream.Create;
    // copy the new part at the start
    newSize := fIndexStream.Size - fOldIndexOffset;
    fIndexStream.Position := fOldIndexOffset;
    tmp.CopyFrom(fIndexStream, newSize);

    // now copy the old data
    fIndexStream.Position := 0;
    tmp.CopyFrom(fIndexStream, fOldIndexOffset);

    // the re-indexed stream is now ready, swap streams pointers for a moment
    x := tmp; tmp := fIndexStream; TStream(fIndexStream) := x;

    tmp.Size := 0;
    fIndexStream.Position := 0;
    tmp.CopyFrom(fIndexStream, fIndexStream.Size);

    // the re-indexed stream is now ready, finally swap streams pointers
    x := tmp; tmp := fIndexStream; TStream(fIndexStream) := x;

  finally
    tmp.Free;
    LeaveCriticalSection(fCursorLock);
  end;
end;

function TDbIndex.GetRealIndex(var aIndex: Integer; unfiltered: boolean
  ): Integer;
begin
  if unfiltered then begin
    if aIndex<0 then
      aIndex := (fIndexStream.Size div SIZEOF_INDEX)-1;
    result := aIndex;
  end else
  if fFilter<>nil then begin
    if (aIndex<0) or (aIndex>Length(fFilter)-1) then
      aIndex := Length(fFilter)-1;
    result := fFilter[aIndex];
  end else begin
    if aIndex<0 then
      // get the oldest item
      aIndex := Count - 1;
    result := aIndex;
  end;
end;

function TDbIndex.GetFileName(aIndex: Integer): string;
begin
  result := fDir + BASE_FILENAME;
  case aIndex of
    FILENAME_INDEX: result += '.logindex';
    FILENAME_CACHE: result += '.logcache';
    FILENAME_INDEXTMP: result += '.logindextmp';
  end;
end;

class procedure TDbIndex.CacheBufferFromItem(const aItem: TLogItem; out
  aBuf: PChar; out len: word);
var
  p: pchar;
  rlen: word;

  procedure StoreString(s:RawByteString; byteSize:boolean );
  var
    b: byte;
    w: word;
    l, n: Integer;
  begin
    l := Length(s);
    if byteSize then begin
      b := min(l, high(byte));
      Move(b, p^, sizeof(byte));
      inc(p, sizeof(byte));
      n := b;
    end else begin
      w := min(l, high(word));
      Move(w, p^, sizeOf(word));
      inc(p, sizeof(word));
      n := w;
    end;

    if n>0 then begin
      Move(s[1], p^, n);
      inc(p, n);
    end;
  end;

begin

  len := SizeOf(Word);
  len += SizeOf(aItem.CommiterDate);
  len += Min(Length(aItem.ParentOID), High(word)) + 2;
  len += Min(Length(aItem.CommitOID), High(Byte)) + 1;
  len += Min(Length(aItem.Author), High(Byte)) + 1;
  len += Min(Length(aItem.Email), High(Byte)) + 1;
  len += Min(Length(aItem.Subject), High(word)) + 2;

  GetMem(aBuf, len);
  p := aBuf;

  // store record size not including the record size itself
  rlen := len - SizeOf(word);
  Move(rlen, p^, SizeOf(word));
  inc(p, sizeOf(word));

  // store commit date
  Move(aItem.CommiterDate, p^, SizeOf(aItem.CommiterDate));
  inc(p, SizeOf(aItem.CommiterDate));

  // store remaining fields
  StoreString(aItem.ParentOID, false);
  StoreString(aItem.CommitOID, true);
  StoreString(aItem.Author, true);
  StoreString(aItem.Email, true);
  StoreString(aItem.Subject, false);
end;

procedure TDbIndex.ItemFromCacheBuffer(buffer: pchar; out aItem: TLogItem);
var
  p: pchar;

  function NextByteString: RawByteString;
  var
    len: byte;
  begin
    len := PByte(p)^;
    SetLength(result, len);
    inc(p);
    Move(p^, result[1], len);
    inc(p, len);
  end;

  function NextWordString: RawByteString;
  var
    len: LongWord;
  begin
    len := PWord(p)^;
    SetLength(result, len);
    inc(p, SizeOf(Word));
    Move(p^, result[1], len);
    inc(p, len);
  end;

  function NextInt64: Int64;
  begin
    result := PInt64(p)^;
    inc(p, SizeOf(Int64));
  end;

begin
  p := buffer;
  inc(p, SizeOf(word));
  aItem.CommiterDate := NextInt64;
  aItem.ParentOID := NextWordString;
  aItem.CommitOID := NextByteString;
  aItem.Author := NextByteString;
  aItem.Email := NextByteString;
  aItem.Subject := NextWordString;
end;

procedure TDbIndex.ItemFromLogBuffer(aBuf: PChar; out aItem: TLogItem);
var
  p: PChar;

  function NextString: RawbyteString;
  var
    q: pchar;
  begin
    q := strpos(p, #2);
    if q<>nil then begin
      SetString(result, p, q-p);
      p := q + 1;
    end else
      result := p;
  end;

begin

  p := aBuf;
  aItem.CommiterDate := StrToInt64Def(NextString, 0);
  aItem.ParentOID := NextString;
  aItem.CommitOID := NextString;
  aItem.Author := NextString;
  aItem.Email := NextString;
  aItem.Subject := NextString;

end;

procedure TDbIndex.DumpItem(aIndex: Integer; var item: TLogItem);
var
  indxRec: TIndexRecord;
begin
  DebugLn;
  DebugLn('          Index: %d of %d',[aIndex, fIndexStream.Size div SIZEOF_INDEX]);
  DebugLn('    Commit Date: %d (%s)',[item.CommiterDate, DateTimeToStr(UnixToDateTime(item.CommiterDate, false))]);
  DebugLn('     Parent OID: %s',[item.ParentOID]);
  DebugLn('     Commit OID: %s',[item.CommitOID]);
  DebugLn('         Author: %s',[item.ParentOID]);
  DebugLn('          Email: %s',[item.Email]);
  DebugLn('        Subject: %s',[item.Subject]);
  //GetIndexRecord(aIndex, indxRec, false);
  //DebugLn('   Cache Offset: %d', [indxRec.offset]);
  //DebugLn('Cache Item Size: %d', [indxRec.size]);
end;

constructor TDbIndex.Create(dir: string);
begin
  inherited Create;
  fDir := IncludeTrailingPathDelimiter(dir);
  fMaxBufferSize := 1024 * 4;
  GetMem(fBuffer, fMaxBufferSize);
  InitCriticalSection(fCursorLock);
end;

destructor TDbIndex.Destroy;
begin
  DoneCriticalSection(fCursorLock);
  fIndexStream.Free;
  fCacheStream.Free;
  FreeMem(fBuffer);
  inherited Destroy;
end;

procedure TDbIndex.Open;
var
  aFileCache, aFileIndex: string;
  mode, aVersion: word;
  sig: cardinal;
begin

  if fCacheStream<>nil then begin
    if gblInvalidateCache then begin
      FreeAndNil(fIndexStream);
      FreeAndNil(fCacheStream);
    end else
      exit;
  end;

  aFileCache := GetFilename(FILENAME_CACHE);
  aFileIndex := GetFilename(FILENAME_INDEX);

  if gblInvalidateCache then begin
    DeleteFile(aFileCache);
    DeleteFile(aFileIndex);
    gblInvalidateCache := false;
  end;

  EnterCriticalSection(fCursorLock);
  try
    mode := fmOpenReadWrite + fmShareDenyWrite;
    if not FileExists(aFileCache) then begin
      if fReadOnly then
        raise Exception.CreateFmt('Cache file %s do not exists',[aFileCache]);
      mode += fmCreate;
    end;
    fCacheStream := TFileStream.Create(aFileCache, mode);

    if mode and fmCreate = fmCreate then begin
      sig := NToBE((PGM_SIGNATURE shl 16) or PGM_VERSION);
      fCacheStream.WriteDWord(sig);
    end
    else begin
      sig := BeToN(fCacheStream.ReadDWord);
      aVersion := sig and $FFFF;
      sig := sig shr 16;
      if (sig<>PGM_SIGNATURE) or (aVersion<PGM_VERSION) then begin
        if fReadOnly then
          raise Exception.CreateFmt('Invalid cache file %s',[aFileCache]);
        // this is an old cache file, recreate it
        DeleteFile(aFileIndex);
        sig := NToBE((PGM_SIGNATURE shl 16) or PGM_VERSION);
        fCacheStream.position := 0;
        fCacheStream.WriteDWord(sig);
        fCacheStream.Size := fCacheStream.Position;
      end;
    end;

    if fIndexStream=nil then begin
      mode := fmOpenReadWrite + fmShareDenyWrite;
      if not FileExists(aFileIndex) then begin
        if fReadOnly then
          raise Exception.CreateFmt('Index file %s do not exists',[aFileIndex]);
        mode += fmCreate;
      end;
      fIndexStream := TFileStream.Create(aFileIndex, mode);
    end;

  finally
    LeaveCriticalSection(fCursorLock);
  end;

end;

procedure TDbIndex.ThreadStart(aHead: boolean);
begin
  fUpdated := false;
  fOldCount := fIndexStream.Size div SIZEOF_INDEX;
  fOldIndexSize := fIndexStream.Size;
  fOldIndexOffset := fIndexStream.Size;
  fCacheUpdate := fCacheStream.Size>0;
  fHead := aHead;
end;

procedure TDbIndex.ThreadDone;
var
  records: Integer;
begin
  records := (fIndexStream.Size - fOldIndexSize) div SIZEOF_INDEX;
  fUpdated := (records<>0);
  if fUpdated then begin
    // there are changes in the index, if they are for 'head' and
    // arent brand new, re-index
    if fHead and (fOldIndexSize>0) then
      ReIndex;

    FileFlush(fIndexStream.Handle);
    FileFlush(fCacheStream.Handle);
  end;

  DebugLn('TDbIndex.ThreadDone: for %s there are %d new records',[BoolToStr(fHead, 'HEAD', 'TAIL'), records]);
end;

procedure TDbIndex.ThreadStore(buf: pchar; size: Integer);
var
  p, q, t: PChar;
  tmp, m: pbyte;
  field: Integer;
  aDate: Int64;
  cd: Integer;
  b: byte;
  w: word;
  indxRec: TIndexRecord;
begin
  field := 0;
  p := buf;
  {      unix date              commitOID
    buf: 10+1 + P(arentOID)+1 + 40+1 + A(uthor)+1 + E(mail)+1 + S(ubject)+1 = Size
         56 + P+A+E+S = Size  so  P+A+E+S = Size - 56

    tmp: 8 + 2+P + 1+40 + 1+A + 1+E + 2+S = tmpSize
         55 + P+A+E+S = tmpSize
         55 + Size  - 56 = tmpSize  so  tmpSize = Size - 1
  }
  GetMem(tmp, size + 11); // only size-1 is really needed, some extra wont hurt
  m := tmp;

  t := p + size;
  while (p<t) and (field<7) do begin
    q := strpos(p, #2);
    if q<>nil then begin
      case field of
        FIELD_DATE:
          begin
            q^ := #0;
            val(p, aDate, cd);
            q^ := #2;
            Move(aDate, m^, sizeof(aDate));
            inc(m, sizeof(aDate));
          end;
        FIELD_COMMITOID, FIELD_AUTHOR, FIELD_EMAIL:
          begin
            b := Min(High(byte), q-p);
            m^ := b; inc(m);
            Move(p^, m^, b); inc(m, b);
          end;
        FIELD_PARENTOID, FIELD_SUBJECT:
          begin
            w := Min(High(word), q-p);
            pword(m)^ := w; inc(m, 2);
            move(p^, m^, w); inc(m, w);
          end;
      end;
      inc(field);
    end else begin
      // should not happen!!!
    end;

    p := q + 1;
  end;

  w := m - tmp;

  EnterCriticalSection(fCursorLock);
  try
    fCacheStream.Seek(0, soFromEnd);
    indxRec.Offset := fCacheStream.Position;
    fCacheStream.WriteWord(w);
    fCacheStream.WriteBuffer(tmp^, w);

    indxRec.size := w + 2;
    fIndexStream.Seek(0, soFromEnd);
    fIndexStream.WriteBuffer(indxRec, SIZEOF_INDEX);
  finally
    LeaveCriticalSection(fCursorLock);
  end;

  freeMem(tmp);
end;

function TDbIndex.GetCachedItem(aIndex: Integer; unfiltered, tryToFix: boolean;
  out aItem: TLogItem): boolean;
var
  realIndex: Integer;
  indxRec: TIndexRecord;
  w: word;
begin
  result := (fIndexStream<>nil) and (fIndexStream.Size>0);
  if result then begin

    realIndex := GetRealIndex(aIndex, unfiltered);
    fIndexStream.Position := realIndex * SIZEOF_INDEX;
    fIndexStream.Read(indxRec{%H-}, SIZEOF_INDEX);

    result := indxRec.offset + indxRec.size <= fCacheStream.Size ;
    if result then begin

      if indxRec.size>fMaxBufferSize then begin
        fMaxBufferSize := indxRec.size * 2;
        ReallocMem(fBuffer, fMaxBufferSize);
      end;

      fCacheStream.Position := indxRec.offset;
      fCacheStream.Read(fBuffer^, indxRec.size);

      w := PWord(fBuffer)^;
      if indxRec.Size-2<>w then begin

        //if tryToFix then begin
        //  // the cache files are corrupt save what seems right, it will be fixed
        //  // the next time the log is requested.
        //  fCacheStream.Size := indxRec.offset;
        //  fIndexStream.Size := fIndexStream.Position - SIZEOF_INDEX;
        //  FileFlush(fIndexStream.Handle);
        //  FileFlush(fCacheStream.Handle);
        //end;

        raise Exception.CreateFmt('Inconsistent data at index %d, expected %d found %d',[aIndex, indxRec.Size-2, w]);
      end;

      if result then
        ItemFromCacheBuffer(fBuffer, aItem);
    end;

  end;
end;

// TODO: this should take a buffer as argument to be reallocated at the real
//       cache stream record size
function TDbIndex.ReadIndexBuffer(realIndex: Integer): boolean;
var
  indxRec: TIndexRecord;
begin

  fIndexStream.Position := realIndex * SIZEOF_INDEX;
  fIndexStream.Read(indxRec{%H-}, SIZEOF_INDEX);

  result := indxRec.offset + indxRec.size <= fCacheStream.Size ;
  if result then begin

    if indxRec.size>fMaxBufferSize then begin
      fMaxBufferSize := indxRec.size * 2;
      ReallocMem(fBuffer, fMaxBufferSize);
    end;

    fCacheStream.Position := indxRec.offset;
    fCacheStream.Read(fBuffer^, indxRec.size);

  end;
end;

function TDbIndex.SetupFilter(arr: TIntArray): boolean;
var
  maxIndex, i: Integer;
begin
  if arr=nil then begin
    result := false;
    fFilter := nil;
    exit;
  end;

  if (fIndexStream=nil) or (fIndexStream.Size=0) then
    raise Exception.Create('Trying to set a filter while the db is not initialized');
  maxIndex := Count(true) - 1;
  // check that indices are within the range of the index
  for i:=0 to Length(arr)-1 do
    if (arr[i]<0) or (arr[i]>maxIndex) then
      raise Exception.CreateFmt('The filter has an invalid entry at %d',[i]);
  // copy filter indices
  SetLength(fFilter, Length(arr));
  Move(arr[0], fFilter[0], Length(arr)*SizeOf(Integer));
  result := true;
end;

function TDbIndex.Count(unfiltered: boolean): Integer;
begin
  if fIndexStream=nil then
    exit(0);

  if not unfiltered and (fFilter<>nil) then
    result := Length(fFilter)
  else
    result := fIndexStream.Size div SIZEOF_INDEX;

  if (not unfiltered) and (fMaxRecords>0) and (result>fMaxRecords) then
    result := fMaxRecords;
end;

function TDbIndex.GetIndex(aIndex: Integer): Integer;
begin
  result := GetRealIndex(aIndex, fFilter=nil);
end;

function TDbIndex.LoadItem(aIndex: Integer; out aItem: TLogItem;
  unfiltered: boolean): boolean;
begin
  EnterCriticalSection(fCursorLock);
  //DebugLnEnter('LoadItem index=%d ThreadId=%d (%s)', [aIndex, ThreadId, BoolToStr(MainThreadID=ThreadId,'Main Thread', 'Other Thread')]);
  try
    result := GetCachedItem(aIndex, unfiltered, true, aItem);
  finally
    //DebugLnExit('TDbIndex.LoadItem DONE ', []);
    LeaveCriticalSection(fCursorLock);
  end;
end;

procedure TDbIndex.SetFilter(arr: TIntArray);
var
  i, j: Integer;
  map: TParentsMap;
  graph: TGraph;
  pmi: PParentsMapItem;
  stack: TIntStack;
  newFilter: TIntArray = nil;
begin

  if not SetupFilter(arr) then
    exit;

  if gblTopologicalMode then
    exit;

  {$IFDEF DebugTopoFilter}
  DumpIntArray('Initial Ordering:', fFilter);
  {$ENDIF}

  {$IFDEF DEBUG}
  ResetTicks;
  {$ENDIF}

  map := GetParentsMap(Self);

  graph := TGraph.Create(map.Count);
  try

    for i:=0 to map.Count-1 do begin
      pmi := map.Data[i];
      for j:=0 to Length(pmi^.parents)-1 do
        if (pmi^.n>=0) and (pmi^.parents[j].n>=0) then
          graph.AddEdge(pmi^.parents[j].n, pmi^.n);
    end;

    {$IFDEF DebugTopoFilter}
    with graph.Alg4List do begin
      SaveToFile('pretopo.txt');
      free;
    end;
    {$ENDIF}

    stack := graph.TopologicalSort;
    SetLength(newFilter, stack.count);

    {$IFDEF DebugTopoFilter}
    ok := Stack.Count=Length(fFilter);
    DebugLn;
    DebugLn('Topological Order: stack and filter sizes matches: %s',[dbgs(ok)]);
    {$ENDIF}
    for i:=stack.Count-1 downto 0 do begin
      {$IFDEF DebugTopoFilter}
      j := (stack.Count-1) - i;
      if stack[j]<>j then DbgOut('*%3d ', [stack[j]])
      else                DbgOut('%4d ', [stack[j]]);
      if (j+1) mod 20 = 0 then DebugLn;
      {$ENDIF}
      newFilter[i] := fFilter[stack[i]];
    end;
    {$IFDEF DebugTopoFilter}
    DebugLn;
    {$ENDIF}

    SetLength(fFilter, Length(newFilter));
    Move(newFilter[0], fFilter[0], Length(newFilter)*SizeOf(Integer));

    {$IFDEF DebugTopoFilter}
    DumpIntArray('Final Ordering:', fFilter);
    {$ENDIF}

  finally
    graph.Free;
    ClearParentsMap(map)
  end;
end;

procedure TDbIndex.ReplaceFilter(arr: TIntArray);
begin
  SetupFilter(arr);
end;

function TDbIndex.FindCommitSha(sha: string; startAt: Integer): Integer;
var
  i, aIndex, n, nailLen, realIndex: Integer;
  indexOffset: Int64;
  indxRec: TIndexRecord;
  b: byte;
  w: word;
  p, nail: pchar;
begin
  // find directly in the database the asked sha
  result := -1;
  if sha='' then
    exit;         // a commit cannot be empty
  sha := lowercase(sha);

  if (fIndexStream<>nil) and (fCacheStream<>nil) then begin
    nail := @sha[1];
    nailLen := Length(sha);
    if startAt<0 then startAt := 0;
    for i:=startAt to (FIndexStream.Size div SIZEOF_INDEX)-1 do begin

      aIndex := i;
      realIndex := GetRealIndex(aIndex, true);
      if not ReadIndexBuffer(realIndex) then
        exit;

      p := fBuffer;           // points to the record data
      inc(p, 2);              // skip record size
      inc(p, SizeOf(Int64));  // skip date
      w := PWord(p)^;         // read parents length
      inc(p, 2+w);            // skip parents
      b := PByte(p)^;         // read commit length
      inc(p);

      n := strlcomp(p, nail, Min(b, nailLen));
      if n=0 then begin
        result := i;
        exit;
      end;

    end;
  end;
end;

end.

