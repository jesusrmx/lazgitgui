unit unitdbindex;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, DateUtils, LazLogger, unitifaces;

type

  TLogItem = record
    CommiterDate: Int64;
    ParentOID, CommitOID,
    Refs,
    Author,
    Email,
    Subject: RawByteString;
  end;

  PIndexRecord = ^TIndexRecord;
  TIndexRecord = packed record
    offset: Int64;
    size: word;
  end;

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
    fIndexStream: TMemoryStream;
    fOldCount: Integer;
    fOldIndexSize: SizeInt;
    fCacheUpdate: boolean;
    fOldIndexOffset: Int64;
    fLoadedItemIndex: Integer;
    fItem: TLogItem;
    function GetCount: Integer;
    function GetInfo: string;
    procedure RecoverIndex;
    procedure ReIndex;
    procedure SaveIndexStream;
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

    property Item: TLogItem read fItem;
    property Count: Integer read GetCount;
    property Info: string read GetInfo;
  end;

implementation

const
  BASE_FILENAME     = 'lazgitgui';
  FILENAME_INFO     = 1;
  FILENAME_INDEX    = 2;
  FILENAME_CACHE    = 3;

{ TMyInterfacedObject }

function TMyInterfacedObject._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
end;

function TMyInterfacedObject._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
end;

{ TDbIndex }

procedure TDbIndex.RecoverIndex;
var
  w: word;
  IndxRec: TIndexRecord;
  n: Integer;
begin
  DebugLn('Recovering Index');
  n := 0;
  fIndexStream.Clear;
  fCacheStream.Position := 0;
  while fCacheStream.Position<fCacheStream.Size do begin
    IndxRec.offset := fCacheStream.Position;
    w := fCacheStream.ReadWord;
    IndxRec.size := w + SizeOf(word);
    fIndexStream.WriteBuffer(indxRec, SIZEOF_INDEX);
    inc(n);
    fCacheStream.Seek(w, soFromCurrent);
  end;
  DebugLn('Recovered %d records vs %d',[fIndexStream.Size div SIZEOF_INDEX, n]);
  DebugLn('Cache: Size=%d Position=%d',[fCacheStream.Size, fCacheStream.Position]);
  SaveIndexStream;
end;

function TDbIndex.GetCount: Integer;
begin
  if fIndexStream<>nil then
    result := fIndexStream.Size div SIZEOF_INDEX
  else
    result := 0;
end;

function TDbIndex.GetInfo: string;
begin

end;

procedure TDbIndex.ReIndex;
var
  newSize: Integer;
  buf, p, q: Pbyte;
begin
  newSize := fIndexStream.Size - fOldIndexOffset;
  GetMem(buf, newSize);

  p := fIndexStream.Memory + fOldIndexOffset;
  q := fIndexStream.Memory + newSize;
  Move(p^, buf^, newSize);
  Move(fIndexStream.Memory^, q^, fIndexStream.Size - newSize);
  Move(buf^, fIndexStream.Memory^, newSize);

  FreeMem(buf);
end;

procedure TDbIndex.SaveIndexStream;
begin
  fIndexStream.SaveToFile(GetFilename(FILENAME_INDEX));
end;

function TDbIndex.GetFileName(aIndex: Integer): string;
begin
  result := fDir + BASE_FILENAME;
  case aIndex of
    FILENAME_INDEX: result += '.logindex';
    FILENAME_CACHE: result += '.logcache';
  end;
end;

procedure TDbIndex.GetIndexRecord(var aIndex: Integer; out indxRec: TIndexRecord
  );
var
  sizeofIndex: Integer;
  indexOffset: SizeInt;
begin
  sizeofIndex := SIZEOF_INDEX;

  if aIndex<0 then begin
    // get the oldest item
    aIndex := fIndexStream.Size div sizeofIndex - 1;
  end;

  indexOffset := aIndex * sizeofIndex;

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
  len += Min(Length(fItem.ParentOID), High(Byte)) + 1;
  len += Min(Length(fItem.CommitOID), High(Byte)) + 1;
  len += Min(Length(fItem.Author), High(Byte)) + 1;
  len += Min(Length(fItem.Email), High(Byte)) + 1;
  len += Min(Length(fItem.Refs), High(word)) + 2;
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
  StoreString(fItem.ParentOID, true);
  StoreString(fItem.CommitOID, true);
  StoreString(fItem.Author, true);
  StoreString(fItem.Email, true);
  StoreString(fItem.Refs, false);
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
  fItem.ParentOID := NextByteString;
  fItem.CommitOID := NextByteString;
  fItem.Author := NextByteString;
  fItem.Email := NextByteString;
  fItem.Refs := NextWordString;
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
  fItem.Refs := NextString;
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
  DebugLn('           Refs: %s',[fItem.Refs]);
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
  mode: word;
begin

  if fCacheStream=nil then begin

    aFileCache := GetFilename(FILENAME_CACHE);
    aFileIndex := GetFilename(FILENAME_INDEX);

    if fCacheStream=nil then begin
      mode := fmOpenReadWrite + fmShareDenyWrite;
      if not FileExists(aFileCache) then
        mode += fmCreate;
      fCacheStream := TFileStream.Create(aFileCache, mode);
    end;

    if fIndexStream=nil then begin
      fIndexStream := TMemoryStream.Create;
      if FileExists(aFileIndex) then
        fIndexStream.LoadFromFile(aFileIndex)
      else if fCacheStream.Size>0 then
        RecoverIndex;
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

    SaveIndexStream;

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
        0:
          begin
            q^ := #0;
            val(p, aDate, cd);
            q^ := #2;
            fCacheStream.WriteQWord(aDate);
          end;
        1..4:
          begin
            b := Min(High(byte), q-p);
            fCacheStream.WriteByte(b);
            fCacheStream.WriteBuffer(p^, b);
          end;
        5..6:
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

      ItemFromCacheBuffer;

      fLoadedItemIndex := aIndex;
    end;

  end;
end;

end.

