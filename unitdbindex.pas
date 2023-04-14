unit unitdbindex;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, DateUtils, LazLogger, unitifaces, fgl, unitgitutils;

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

  PIndexRecord = ^TIndexRecord;
  TIndexRecord = packed record
    offset: Int64;
    size: word;
  end;

  TIntArray = array of Integer;

  TParentsItem = record
    n: Integer;
    parents: TQWordArray;
    commit: QWord;
  end;

  TParentsArray = array of TParentsItem;

  TLineItemFlags = set of (lifInternal, lifToMerge, lifMerge, lifToBorn, lifBorn);
  TLineItem = record
    column: Integer;
    source: Integer;
    Flags:  TLineItemFlags;
  end;
  TLinesArray = array of TLineItem;

  TItemIndex = record
    index: Integer;
    parents, childs: TIntArray;
    column: Integer;
    lines: TLinesArray;
    first: boolean;
    last: boolean;
  end;
  TItemIndexArray = array of TItemIndex;

const
  SIZEOF_INDEX = sizeof(TIndexRecord);

type

  { TMyInterfacedObject }

  TMyInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

  { TDbIndex }

  TDbIndex = class(TMyInterfacedObject, IDbIndex)
  private
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
    fLoadedItemIndex: Integer;
    fItem: TLogItem;
    fReadOnly: boolean;
    fFilter: TIntArray;
    function GetAcceptingNewRecords: boolean;
    function GetCount: Integer;
    function GetInfo: string;
    //procedure RecoverIndex;
    procedure ReIndex;
    procedure GetIndexRecord(var aIndex:Integer; out indxRec: TIndexRecord);
    procedure CacheBufferFromItem(out aBuf: PChar; out len: word);
    procedure ItemFromCacheBuffer;
    procedure ItemFromLogBuffer(aBuf: PChar);
    procedure DumpItem;
    procedure ThreadStart(aHead: boolean);
    procedure ThreadDone;
    procedure ThreadStore(buf: pchar; size: Integer);
  public
    constructor Create(dir: string);
    destructor Destroy; override;

    procedure Open;
    function LoadItem(aIndex: Integer): boolean;
    procedure SetFilter(arr: TIntArray);
    procedure TopoSort;

    property Item: TLogItem read fItem;
    property Count: Integer read GetCount;
    property Info: string read GetInfo;
    property ReadOnly: boolean read fReadOnly write fReadOnly;
    property AcceptingNewRecords: boolean read GetAcceptingNewRecords;
    property MaxRecords: Integer read fMaxRecords write fMaxRecords;
  end;

  function GetParentsArray(db: TDbIndex): TParentsArray;
  procedure ClearParentsArray(var aList: TParentsArray);
  procedure FindRelatives(var items: TItemIndexArray; parArray: TParentsArray);
  function GetItemIndexes(db: TDbIndex; withColumns:boolean; out maxColumns:Integer): TItemIndexArray;

implementation

const
  BASE_FILENAME     = 'lazgitgui';
  FILENAME_INFO     = 1;
  FILENAME_INDEX    = 2;
  FILENAME_CACHE    = 3;
  FILENAME_INDEXTMP = 4;

function GetParentsArray(db: TDbIndex): TParentsArray;
var
  i: Integer;
begin
  SetLength(result, db.Count);
  for i:=0 to db.Count-1 do begin
    db.LoadItem(i);
    with db.Item do begin
      Result[i].n := i;
      Result[i].commit := OIDToQWord(CommitOID);
      Result[i].parents := OIDToParents(ParentOID, Length(CommitOID));
    end;
  end;
end;

// is this necessary?
procedure ClearParentsArray(var aList: TParentsArray);
var
  i: Integer;
begin
  for i:=0 to Length(aList)-1 do
    aList[i].parents := nil;
  aList := nil;
end;

procedure FindRelatives(var items: TItemIndexArray; parArray: TParentsArray);
var
  i, j, k, p: Integer;
begin
  SetLength(items, Length(parArray));

  for i:=0 to Length(items)-1 do begin
    items[i].index := i;
    items[i].column := -1;
    items[i].parents := nil;
    items[i].childs := nil;
    items[i].lines := nil;
  end;

  for i:=0 to Length(items)-1 do begin
    for p:=0 to Length(parArray[i].parents)-1 do
      for j:=0 to Length(parArray)-1 do begin
        if j=i then continue;
        if parArray[i].parents[p]=parArray[j].commit then begin
          // found a parent of index i at index j
          k := Length(items[i].parents);
          SetLength(items[i].parents, k+1);
          items[i].parents[k] := j;
          // this means i is a child of j
          k := Length(items[j].childs);
          SetLength(items[j].childs, k+1);
          items[j].childs[k] := i;
        end;

      end;
  end;
end;

function GetItemIndexes(db: TDbIndex; withColumns: boolean; out
  maxColumns: Integer): TItemIndexArray;
var
  parArray: TParentsArray;
  i, j, k, n, p, c, column: Integer;
  s: string;
  columns: array of
    record
      first: Integer;
      last: Integer;
    end;
begin
  parArray := GetParentsArray(db);
  DebugLn('PARENTS');
  for i:=0 to Length(parArray)-1 do begin
    DbgOut('%3d: %.16x => ',[parArray[i].n, parArray[i].commit]);
    for j:=0 to Length(parArray[i].parents)-1 do
      DbgOut('%.16x ',[parArray[i].parents[j]]);
    DebugLn;
  end;

  result := nil;
  FindRelatives(result, parArray);

  DebugLn('ItemIndexArray: %d',[Length(result)]);
  for i:=0 to Length(result)-1 do begin
    DbgOut('%3d. P(%d): ',[i, Length(result[i].parents)]);
    for j:=0 to Length(result[i].parents)-1 do
      dbgOut('%.3d ',[result[i].parents[j]]);
    DbgOut(' C(%d)',[Length(result[i].childs)]);
    for j:=0 to Length(result[i].childs)-1 do
      dbgOut('%.3d ',[result[i].childs[j]]);
    DebugLn;
  end;

  maxColumns := 0;
  if not withColumns then
    exit;

  column := -1;
  columns := nil;

  n := Length(result);
  while n>0 do begin

    inc(Column);
    SetLength(Columns, column + 1);
    Columns[column].first := -1;
    Columns[column].last := -1;

    // find the first index with no assigned column
    i := 0;
    while i<Length(result) do begin
      if result[i].column<0 then break;
      inc(i);
    end;
    if i=length(result) then
      break; // we are done

    // using 'i' index as a grand child, track down the first parent.
    repeat

      result[i].column := column;

      if Columns[column].first<0 then
        Columns[column].first := i;
      Columns[column].last := i;

      dec(n);
      if n=0 then
        break;

      // now starting with 'i' descend down until we find
      // the first parent that has no column assigned
      j := 0;
      while j<Length(result[i].parents) do begin
        p := result[i].parents[j];
        if result[p].column=-1 then
          break; // found
        inc(j);
      end;
      if j=Length(result[i].parents) then
        break;  // not found

      // the found parent is now the next index to process...
      i := p;

    until false;

  end;

  // assign columns to every index's lines. In other words
  // for each index find what will draw at each column
  // it will always draw a node at the .column position
  // and will draw a line at each .lines[k] column
  MaxColumns := 1;
  if Length(Columns)>1 then begin
    for i:=0 to Length(result)-1 do begin
      for j:=0 to Length(columns)-1 do begin
        // is the index i within the range of column j?
        if (i>=columns[j].first) and (i<=columns[j].last) then begin
          // yes
          if result[i].column=j then begin
            // ... but it's the same column, will always draw a node there
            if i=columns[j].first then result[i].first := true;
            if i=columns[j].last  then result[i].last := true;
            continue;
          end;
          // no, it have to draw a line at this column, it is within the
          // first and last, so it will be an internal line.
          k := Length(result[i].lines);
          SetLength(result[i].lines, k+1);
          result[i].lines[k].column := j;
          result[i].lines[k].source := LINE_SOURCE_COLUMN;
          result[i].lines[k].Flags := [lifInternal];
        end;
      end;
      if Length(result[i].lines)>MaxColumns then
        MaxColumns := Length(result[i].lines);
    end;

    // try to find the parent index of columns last index
    for j := 0 to Length(Columns)-1 do begin
      // Theory: the last index of a column couldnt be multiparent
      k := Columns[j].last;
      if result[k].parents=nil then
        continue; // this is the last index as it has no parents
      if length(result[k].parents)>1 then
        continue; // houston we have a problem and we don't know how to handle....
      // so k is the last index and our parent is ...
      p := result[k].parents[0];
      // now queue lines coloured by whatever the parent column is coloured
      for i:= k+1 to p do begin
        n := Length(result[i].lines);
        SetLength(result[i].lines, n+1);
        result[i].lines[n].column := j;
        result[i].lines[n].source := p;
        Include(result[i].lines[n].Flags, lifToBorn);
        if i=p then Include(result[i].lines[n].Flags, lifBorn);
        if n+1>MaxColumns then
          MaxColumns := n+1;
      end;
    end;

    // finally find merges if there is any ..
    for j := 1 to Length(Columns)-1 do begin
      // start find merges at index 'last'-1 descending down until index 'first',
      // a mergeable index is a child index with a column less than the current
      //k := Columns[j].last - 1;
      k := Columns[j].first + 1;
      while k>=Columns[j].first do begin
        for n in result[k].childs do begin
          if result[n].column<j then begin
            // index 'k' will merge at index 'n'.
            // Add 'merge' lines from 'n' to 'k-1' at the column 'j'
            for i:=n to k-1 do begin
              p := Length(result[i].lines);
              SetLength(result[i].lines, p+1);
              result[i].lines[p].column := j;
              result[i].lines[p].source := n;
              Include(result[i].lines[p].Flags, lifToMerge);
              if i=n then Include(result[i].lines[p].Flags, lifMerge);
              if p+1>MaxColumns then
                MaxColumns := p+1;
            end;
            // is now merged, can we merge to more than one branch?
            break;
          end;
        end;
        dec(k);
      end;
    end;


  end;

  maxColumns := Length(Columns);

  // report of columns
  DebugLn('Report of %d columns:',[Length(Columns)]);
  for i:=0 to Length(Columns)-1 do
    with columns[i] do begin
      DebugLn('Column %d: first=%d last=%d',[i, first, last]);
    end;

  //// now the result
  //SetLength(s, Length(columns));
  //for i:=0 to Length(result)-1 do begin
  //  for j := 0 to Length(columns)-1 do s[j+1] := ' ';
  //  for j := 0 to Length(result[i].lines)-1 do begin
  //    k := result[i].lines[j].column+1;
  //    if lifMerge in result[i].lines[j].Flags then      s[k] := '-'
  //    else if lifBorn in result[i].lines[j].Flags then  s[k] := '^'
  //    else                                              s[k] := '|';
  //  end;
  //  s[result[i].column+1] := '*';
  //  DebugLn('%s : Index %d', [s, i]);
  //end;

end;

{ TMyInterfacedObject }

function TMyInterfacedObject._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
end;

function TMyInterfacedObject._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
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

function TDbIndex.GetCount: Integer;
begin
  if fIndexStream<>nil then begin
    if fFilter<>nil then
      result := Length(fFilter)
    else
      result := fIndexStream.Size div SIZEOF_INDEX
  end else
    result := 0;

  if (fMaxRecords>0) and (result>fMaxRecords) then
    result := fMaxRecords;
end;

function TDbIndex.GetAcceptingNewRecords: boolean;
begin
  result := (fMaxRecords=0) or (Count<fMaxRecords);
end;

function TDbIndex.GetInfo: string;
begin
  result := format('%d',[Count]);
end;

procedure TDbIndex.ReIndex;
var
  newSize: Integer;
  buf, p, q: Pbyte;
  tmp, x: TStream;
begin
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

  tmp.Free;
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

procedure TDbIndex.GetIndexRecord(var aIndex: Integer; out indxRec: TIndexRecord
  );
var
  sizeofIndex: Integer;
  indexOffset: SizeInt;
  theIndex: Integer;
begin
  sizeofIndex := SIZEOF_INDEX;

  if fFilter<>nil then begin
    if (aIndex<0) or (aIndex>Length(fFilter)-1) then
      aIndex := Length(fFilter)-1;
    theIndex := fFilter[aIndex];
  end else begin
    if aIndex<0 then
      // get the oldest item
      aIndex := Count - 1;
    theIndex := aIndex;
  end;

  indexOffset := theIndex * sizeofIndex;

  fIndexStream.Position := indexOffset;
  fIndexStream.Read(indxRec{%H-}, sizeofIndex);
end;

procedure TDbIndex.CacheBufferFromItem(out aBuf: PChar; out len: word);
var
  p: pchar;
  rlen: word;

  procedure StoreString(s:RawByteString; byteSize:boolean );
  var
    b: byte;
    w: word;
    l: Integer;
  begin
    l := Length(s);
    if byteSize then begin
      b := min(l, high(byte));
      Move(b, p^, sizeof(byte));
      inc(p, sizeof(byte));
    end else begin
      w := min(l, high(word));
      Move(w, p^, sizeOf(word));
      inc(p, sizeof(word));
    end;

    if l>0 then begin
      Move(s[1], p^, l);
      inc(p, l);
    end;
  end;

begin

  len := SizeOf(Word);
  len += SizeOf(fItem.CommiterDate);
  len += Min(Length(fItem.ParentOID), High(word)) + 1;
  len += Min(Length(fItem.CommitOID), High(Byte)) + 1;
  len += Min(Length(fItem.Author), High(Byte)) + 1;
  len += Min(Length(fItem.Email), High(Byte)) + 1;
  len += Min(Length(fItem.Subject), High(word)) + 2;

  GetMem(aBuf, len);
  p := aBuf;

  // store record size not including the record size itself
  rlen := len - SizeOf(word);
  Move(rlen, p^, SizeOf(word));
  inc(p, sizeOf(word));

  // store commit date
  Move(fItem.CommiterDate, p^, SizeOf(fItem.CommiterDate));
  inc(p, SizeOf(fItem.CommiterDate));

  // store remaining fields
  StoreString(fItem.ParentOID, false);
  StoreString(fItem.CommitOID, true);
  StoreString(fItem.Author, true);
  StoreString(fItem.Email, true);
  StoreString(fItem.Subject, false);
end;

procedure TDbIndex.ItemFromCacheBuffer;
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

  Finalize(fItem);

  p := fBuffer;
  inc(p, SizeOf(word));
  fItem.CommiterDate := NextInt64;
  fItem.ParentOID := NextWordString;
  fItem.CommitOID := NextByteString;
  fItem.Author := NextByteString;
  fItem.Email := NextByteString;
  fItem.Subject := NextWordString;
end;

procedure TDbIndex.ItemFromLogBuffer(aBuf: PChar);
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

  Finalize(fItem);

  p := aBuf;
  fItem.CommiterDate := StrToInt64Def(NextString, 0);
  fItem.ParentOID := NextString;
  fItem.CommitOID := NextString;
  fItem.Author := NextString;
  fItem.Email := NextString;
  fItem.Subject := NextString;

end;

procedure TDbIndex.DumpItem;
var
  indxRec: TIndexRecord;
begin
  DebugLn;
  DebugLn('          Index: %d of %d',[fLoadedItemIndex, fIndexStream.Size div SIZEOF_INDEX]);
  DebugLn('    Commit Date: %d (%s)',[fItem.CommiterDate, DateTimeToStr(UnixToDateTime(fItem.CommiterDate))]);
  DebugLn('     Parent OID: %s',[fItem.ParentOID]);
  DebugLn('     Commit OID: %s',[fItem.CommitOID]);
  DebugLn('         Author: %s',[fItem.ParentOID]);
  DebugLn('          Email: %s',[fItem.Email]);
  DebugLn('        Subject: %s',[fItem.Subject]);
  GetIndexRecord(fLoadedItemIndex, indxRec);
  DebugLn('   Cache Offset: %d', [indxRec.offset]);
  DebugLn('Cache Item Size: %d', [indxRec.size]);
end;

constructor TDbIndex.Create(dir: string);
begin
  inherited Create;
  fDir := IncludeTrailingPathDelimiter(dir);
  fMaxBufferSize := 1024 * 4;
  GetMem(fBuffer, fMaxBufferSize);
  fLoadedItemIndex := -1;
end;

destructor TDbIndex.Destroy;
begin
  fIndexStream.Free;
  fCacheStream.Free;
  Finalize(fItem);
  FreeMem(fBuffer);
  inherited Destroy;
end;

procedure TDbIndex.Open;
var
  aFileCache, aFileIndex: string;
  mode, aVersion: word;
  sig: cardinal;
begin

  if fCacheStream=nil then begin

    aFileCache := GetFilename(FILENAME_CACHE);
    aFileIndex := GetFilename(FILENAME_INDEX);

    if fCacheStream=nil then begin

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

  end;
end;

procedure TDbIndex.ThreadStart(aHead: boolean);
begin
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
  if (records<>0) then begin
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
  field: Integer;
  currentOffset, finalOffset, aDate: Int64;
  cd: Integer;
  b: byte;
  w: word;
  indxRec: TIndexRecord;
begin
  currentOffset := fCacheStream.Size;
  fCacheStream.Seek(0, soFromEnd);
  fCacheStream.WriteWord(0);

  field := 0;
  p := buf;

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
            fCacheStream.WriteQWord(aDate);
          end;
        FIELD_COMMITOID, FIELD_AUTHOR, FIELD_EMAIL:
          begin
            b := Min(High(byte), q-p);
            fCacheStream.WriteByte(b);
            fCacheStream.WriteBuffer(p^, b);
          end;
        FIELD_PARENTOID, FIELD_SUBJECT:
          begin
            w := Min(High(word), q-p);
            fCacheStream.WriteWord(w);
            fCacheStream.WriteBuffer(p^, w);
          end;
      end;
      inc(field);
    end else begin
      // should not happen!!!
    end;

    p := q + 1;
  end;

  // fix record length
  finalOffset := fCacheStream.Position;
  fCacheStream.Position := currentOffset;
  fCacheStream.WriteWord(finalOffset - currentOffset - sizeOf(word));
  fCacheStream.Position := finalOffset;

  indxRec.Offset := currentOffset;
  indxRec.size := finalOffset - currentOffset;
  fIndexStream.Seek(0, soFromEnd);
  fIndexStream.WriteBuffer(indxRec, SIZEOF_INDEX);

end;

function TDbIndex.LoadItem(aIndex: Integer): boolean;
var
  indxRec: TIndexRecord;
  w: word;
begin
  result := (fIndexStream<>nil) and (fIndexStream.Size>0);
  if result then begin

    GetIndexRecord(aIndex, indxRec);

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

        // the cache files are corrupt save what seems right, it will be fixed
        // the next time the log is requested.
        fCacheStream.Size := indxRec.offset;
        fIndexStream.Size := fIndexStream.Position - SIZEOF_INDEX;
        FileFlush(fIndexStream.Handle);
        FileFlush(fCacheStream.Handle);

        raise Exception.CreateFmt('Inconsistent data at index %d, expected %d found %d',[aIndex, indxRec.Size-2, w]);
      end;

      ItemFromCacheBuffer;

      fLoadedItemIndex := aIndex;
    end;

  end;
end;

procedure TDbIndex.SetFilter(arr: TIntArray);
var
  maxIndex, i: Integer;
begin
  if arr=nil then
    fFilter := nil
  else begin
    if (fIndexStream=nil) or (fIndexStream.Size=0) then
      raise Exception.Create('Trying to set a filter while the db is not initialized');
    maxIndex := Count - 1;
    // check that indices are within the range of the index
    for i:=0 to Length(arr)-1 do
      if (arr[i]<0) or (arr[i]>maxIndex) then
        raise Exception.CreateFmt('The filter has an invalid entry at %d',[i]);
    // copy filter indices
    SetLength(fFilter, Length(arr));
    Move(arr[0], fFilter[0], Length(arr)*SizeOf(Integer));
  end;
end;

type
  // ref: https://gist.github.com/mikebsg01/52ad3ae8277e9c424823a79e4f7b0cf6

  TIntList = specialize TFpGList<Integer>;
  TIntStack = TIntList;
  TBoolArr = array of boolean;

  { TGraph }

  TGraph = class
  private
    fV: Integer;
    fAdj: array of TIntList;
    procedure TopologicalSortUtil(v: Integer; visited:TBoolArr; stack: TIntStack);
  public
    constructor Create(aV: Integer);
    destructor Destroy; override;
    procedure AddEdge(v, w: Integer);
    function TopologicalSort: TIntStack;
  end;

{ TGraph }

procedure TGraph.TopologicalSortUtil(v: Integer; visited: TBoolArr;
  stack: TIntStack);
var
  i: Integer;
begin
  // Mark the current node as visited.
  visited[v] := true;

  // Recur for all the vertices adjacent to this vertex
  for i := fAdj[v].First to fAdj[v].Last do
    if not visited[i] then
      TopologicalSortUtil(i, visited, stack);

  // Push current vertex to stack which stores result
  stack.Add(v);
end;

constructor TGraph.Create(aV: Integer);
var
  i: Integer;
begin
  inherited Create;
  fV := aV;
  SetLength(fAdj, aV);
  for i:=0 to aV-1 do
    fAdj[i] := TIntList.Create;
end;

destructor TGraph.Destroy;
var
  i: Integer;
begin
  for i:=0 to fV-1 do
    fAdj[i].Free;
  inherited Destroy;
end;

procedure TGraph.AddEdge(v, w: Integer);
begin
  fAdj[v].Add(w);
end;

function TGraph.TopologicalSort: TIntStack;
var
  stack: TIntStack;
  visited: TBoolArr;
  i: Integer;
begin

  result := TIntList.Create;
  SetLength(visited, fV);

  // Mark all the vertices as not visited
  for i:=0 to fV-1 do visited[i] := false;

  // Call the recursive helper function to store Topological Sort
  // starting from all vertices one by one
  for i:=0 to fV-1 do
    if not visited[i] then
      TopologicalSortUtil(i, visited, result);

end;

procedure TestGraph;
var
  graph: TGraph;
  stack: TIntStack;
  i: Integer;
begin
  graph  := TGraph.create(8);
  graph.addEdge(7, 6);
  graph.addEdge(7, 5);
  graph.addEdge(6, 4);
  graph.addEdge(6, 3);
  graph.addEdge(5, 4);
  graph.addEdge(5, 2);
  graph.addEdge(3, 1);
  graph.addEdge(2, 1);
  graph.addEdge(1, 0);
  stack := graph.TopologicalSort;
  graph.free;

  // Print contents of stack
  for i:=stack.Count-1 downto 0 do
    DbgOut('%d ',[stack[i]]);

  stack.Free;
end;

procedure TDbIndex.TopoSort;
var
  graph: TGraph;
  parArray: TParentsArray;
  i, j, x, dummy: Integer;
  arr: TIntArray;
  stack: TIntStack;
  indxArr: TItemIndexArray;
begin
  //TestGraph

  SetFilter(nil);

  indxArr := GetItemIndexes(self, true, dummy);

  // this listing clearly shows list of indices and parent indices each index
  // it have
  //for i:=0 to Length(indxArr)-1 do begin
  //  DbgOut('%3d (%d): ',[i, length(indxArr[i].parents)]);
  //  for j:=0 to Length(indxArr[i].parents)-1 do
  //    DbgOut('%3d ',[indxArr[i].parents[j]]);
  //  DebugLn;
  //end;

  //for external tests...
  //DebugLn('graph := TGraph.Create(%d);',[Length(indxArr)]);
  //for i:=0 to Length(indxArr)-1 do begin
  //  with indxArr[i] do begin
  //    for x in parents do
  //      DebugLn('graph.AddEdge(%d, %d);',[x, index]);
  //  end;
  //end;


  graph := TGraph.Create(Count);
  try

    for i:=0 to Length(indxArr)-1 do
      with indxArr[i] do begin
        for x in parents do
          graph.AddEdge(x, index)
      end;

    stack := graph.TopologicalSort;
    setLength(arr, stack.Count);

    // This tests demonstrates that the data is already ordered
    // topologically.
    //DebugLn;
    //x := 0;
    //for i:=stack.Count-1 downto 0 do begin
    //  DbgOut('%3d ',[stack[i]]);
    //  if (x+1) mod 20 = 0 then DebugLn;
    //  inc(x);
    //end;

    //SetFilter(arr);

  finally
    ClearParentsArray(parArray);
    graph.Free;
  end;
end;

end.

