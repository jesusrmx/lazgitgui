unit unitdbindex;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, DateUtils, LazLogger, unitifaces;

const
  FIELD_DATE        = 0;
  FIELD_PARENTOID   = 1;
  FIELD_COMMITOID   = 2;
  FIELD_AUTHOR      = 3;
  FIELD_EMAIL       = 4;
  FIELD_SUBJECT     = 5;

  PGM_VERSION       = $0001;
  PGM_SIGNATURE     = $1A2A;

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

const
  SIZEOF_INDEX = sizeof(TIndexRecord);

type

  { TMyInterfacedObject }

  TMyInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

  TIntArray = array of integer;

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
    fReadOnly: boolean;
    fFilter: TIntArray;
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
    procedure SetFilter(arr: TIntArray);

    property Item: TLogItem read fItem;
    property Count: Integer read GetCount;
    property Info: string read GetInfo;
    property ReadOnly: boolean read fReadOnly write fReadOnly;
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
  fCacheStream.Position := SizeOf(cardinal);
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
  if fIndexStream<>nil then begin
    if fFilter<>nil then
      result := Length(fFilter)
    else
      result := fIndexStream.Size div SIZEOF_INDEX
  end else
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
      aIndex := fIndexStream.Size div sizeofIndex - 1;
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
      fIndexStream := TMemoryStream.Create;
      if FileExists(aFileIndex) then
        fIndexStream.LoadFromFile(aFileIndex)
      else if fReadOnly then
        raise Exception.CreateFmt('Index file %s do not exists',[aFileIndex])
      else if fCacheStream.Size>4 then
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

procedure TDbIndex.SetFilter(arr: TIntArray);
var
  maxIndex, i: Integer;
begin
  if arr=nil then
    fFilter := nil
  else begin
    if (fIndexStream=nil) or (fIndexStream.Size=0) then
      raise Exception.Create('Trying to set a filter while the db is not initialized');
    maxIndex := fIndexStream.Size div SIZEOF_INDEX - 1;
    // check that indices are within the range of the index
    for i:=0 to Length(arr)-1 do
      if (arr[i]<0) or (arr[i]>maxIndex) then
        raise Exception.CreateFmt('The filter has an invalid entry at %d',[i]);
    // copy filter indices
    SetLength(fFilter, Length(arr));
    Move(arr[0], fFilter[0], Length(arr)*SizeOf(Integer));
  end;
end;

end.

