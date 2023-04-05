unit unitlogcache;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

{.$define Capture}
{.$define CaptureChunks}
{$if defined(Debug) or defined(Capture) or defined(CaptureChunks)}
  {$define UseCounter}
{$endif}


interface

uses
  Classes, SysUtils, Math, DateUtils, LazLogger, unitgit, unitprocess, unitruncmd;

const

  LOGEVENT_OUTPUT = 1;
  LOGEVENT_RECORD = 2;
  LOGEVENT_DONE   = 3;
  LOGEVENT_END    = 4;

  FILENAME_INFO     = 1;
  FILENAME_INDEX    = 2;
  FILENAME_CACHE    = 3;

type
  TLogThread = class;

  TLogThreadEvent = procedure(sender: TObject; thread: TLogThread; event: Integer; var interrupt: boolean) of object;

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

  TLogState = (lsInactive, lsStart, lsGetFirst, lsGetLast, lsEnd);

  {
  #SEP="|"
  SEP="%x02"
  EOL="%x00"
  PRETTY="%ct$SEP%p$SEP%h$SEP%an$SEP%ae$SEP%D$SEP%s$EOL"
  #MAX="--max-count=1000"
  FORMAT="tformat"
  ALL="--all"

  git log $MAX --pretty="$FORMAT:$PRETTY" $ALL > format.log
  }

  { TLogThread }

  TLogThread = class(TThread)
  private
    fBuffer: PChar;
    fCommand: string;
    fHaveProgress: boolean;
    fOnOutput: TNotifyInterruptEvent;
    fResult: Integer;
    fErrorLog: string;
    fStartDir: string;
    fCmdLine: ^TCmdLine;
    fLogSize: Integer;
    {$IFDEF Capture}
    fCap: TMemoryStream;
    {$ENDIF}
    procedure Notify;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    property Command: string read fCommand write fCommand;
    property StartDir: string read fStartDir write fStartDir;
    property Result: Integer read fResult;
    property ErrorLog: string read fErrorLog;
    property Buffer: PChar read fBuffer;
    property BufferSize: Integer read fLogSize;
    property OnOutput: TNotifyInterruptEvent read fOnOutput write fOnOutput;
    property HaveProgress: boolean read fHaveProgress write fHaveProgress;
  end;

  { TLogCache }

  TLogCache = class
  private
    fGit: TGit;
    fCacheStream: TFileStream;
    fIndexStream: TMemoryStream;
    fBuffer: PChar;
    fLastReadItemIndex: SizeInt;
    fMaxBufferSize: Integer;
    fEntryReadSize: Integer;
    fItem: TLogItem;
    fLogState: TLogState;
    fNewDate, fOldDate: Int64;
    fOldDateIsStart: boolean;
    flogEvent: TLogThreadEvent;
    fOldIndexOffset: Int64;
    fOldCount: Integer;
    function GetCount: Integer;
    procedure OnLogThreadDone(Sender: TObject);
    procedure OnLogThreadOutput(sender: TObject; var interrupt: boolean);
    procedure GetIndexRecord(var aIndex:SizeInt; out indxRec: TIndexRecord);
    function ReadLogItem(aIndex:SizeInt): boolean;
    procedure CacheBufferFromItem(out aBuf: PChar; out len: word);
    procedure ItemFromCacheBuffer;
    procedure ItemFromLogBuffer(aBuf: PChar);
    procedure EnterLogState(aState: TLogState);
    procedure DoLogStateStart;
    procedure DoLogStateGetFirst;
    procedure DoLogStateGetLast;
    procedure DoLogStateEnd;
    procedure Run;
    function  GetFilename(aIndex: Integer): string;
    procedure ReIndexStream(stream:TMemoryStream; offset: Integer);
    procedure ReIndex;
    procedure OpenCache;
    procedure DumpItem;
    procedure SendEvent(thread: TLogThread; event: Integer; var interrupt:boolean);
  public
    constructor create(aLogEvent: TLogThreadEvent);
    destructor Destroy; override;
    procedure LoadCache;
    function LoadIndex(aIndex: Integer): boolean;

    property LogState: TLogState read fLogState;
    property Git: TGit read fGit write fGit;
    property Count: Integer read GetCount;
    property Item: TLogItem read fItem;
  end;


implementation

const
  LOG_CMD = '--pretty="format:%ct'#2'%P'#2'%H'#2'%an'#2'%ae'#2'%D'#2'%s'#2#3'" --all';

{ TLogThread }

procedure TLogThread.Notify;
var
  interrupt: boolean;
begin
  if assigned(fOnOutput) then begin
    {$IFDEF DEBUG}
    DebugLn('Notifying: %s',[DbgStr(fLine)]);
    {$ENDIF}
    interrupt := false;
    fOnOutput(Self, interrupt);
    if interrupt then
      terminate;
  end;
end;

constructor TLogThread.Create;
begin
  inherited Create(true);
  fCmdLine := new(PCmdLine, Init);
  {$IFDEF Capture}
  fCap := TMemoryStream.Create;
  {$ENDIF}
end;

destructor TLogThread.Destroy;
begin
  {$IFDEF DEBUG}
  debugln('TRunThread.Destroy: ');
  {$ENDIF}
  {$IFDEF Capture}
  fCap.Free;
  {$ENDIF}
  Dispose(fCmdLine);
  inherited Destroy;
end;

{$ifdef UseCounter}
var
  counter: Integer = 0;
{$endif}

procedure TLogThread.Execute;
var
  outText: string;

  procedure CollectOutputDummy(const buffer; size:Longint; var interrupt: boolean);
  begin

  end;

  procedure CollectOutput(const buffer; size:Longint; var interrupt: boolean);
  var
    aPos: Integer;
    p, q, t: pchar;
    {$ifdef CaptureChunks}
    M: TMemoryStream;
    {$endif}
  begin
    if terminated or interrupt then
      exit;
    {$IFDEF CaptureChunks}
    M := TMemoryStream.Create;
    M.WriteBuffer(buffer, size);
    M.SaveToFile(format('capture_%.2d_%d.chunk',[counter, size]));
    M.Free;
    {$ENDIF}
    {$IFDEF Capture}
    fCap.WriteBuffer(buffer, size);
    aPos := fCap.Position;
    fCap.SaveToFile(format('capture_%.2d_%d',[counter, size]));
    {$ENDIF}

    {$ifdef UseCounter}
    inc(counter);
    {$endif}

    {$IFDEF DEBUG}
    DebugLn('Collecting %d bytes, counter=%d',[size, counter]);
    {$ENDIF}

    {$if defined(Capture) or defined(CaptureChunks)}
    exit;
    {$endif}

    if outtext='' then
      p := @buffer
    else begin
      aPos := Length(outText);
      SetLength(outText, aPos + size);
      Move(Buffer, OutText[aPos+1], size);
      p := @outtext[1];
    end;

    t := p + size;
    while p<t do begin

      //inc(counter);

      q := strpos(p, #3);
      if q<>nil then begin

        if p^=#10 then inc(p);

        fBuffer := p;
        fLogSize := q-p;
        Synchronize(@Notify);

        interrupt := terminated;
        if interrupt then
          break;

        inc(p, fLogSize + 1);

      end else begin
        // can't find end of record in this chunk, copy the rest of
        // buffer in outext
        SetLength(outText, t-p);
        Move(p^, outText[1], t-p);
        break;
      end;

    end;

  end;
begin
  {$IFDEF DEBUG}
  DebugLnEnter('RunThread START Command=%s', [fCommand]);
  {$ENDIF}
  outText := '';
  //fCmdLine^.WaitOnExit := true;
  fCmdLine^.RedirStdErr := true;
  fResult := fCmdLine^.RunProcess(fCommand, fStartDir, @CollectOutput);
  if (outText<>'') and (not terminated) then begin
    Synchronize(@Notify);
  end;
  fErrorLog := fCmdLine^.ErrorLog;
  {$IFDEF DEBUG}
  DebugLnExit('RunThread DONE result=%d', [fResult]);
  {$ENDIF}
  Terminate;
end;

{ TLogCache }

function TLogCache.ReadLogItem(aIndex: SizeInt): boolean;
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
      fEntryReadSize := fCacheStream.Read(fBuffer^, indxRec.size);

      ItemFromCacheBuffer;

      fLastReadItemIndex := aIndex;
    end;

  end;
end;

procedure TLogCache.CacheBufferFromItem(out aBuf: PChar; out len: word);
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

procedure TLogCache.OnLogThreadOutput(sender: TObject; var interrupt: boolean);
var
  thread: TLogThread absolute sender;
  aBuffer: PChar;
  aLen: word;
  indxRec: TIndexRecord;
  notify: boolean;
begin
  notify := false;

  ItemFromLogBuffer(thread.Buffer);

  case fLogState of
    lsGetFirst, lsGetLast: begin
      CacheBufferFromItem(aBuffer, aLen);

      // remember the current index offset, we getting newer records
      // it would help to identify the re-index starting offset
      // for getting older records it's not used
      if fOldIndexOffset<0 then
        fOldIndexOffset := fIndexStream.Size;

      // Prepare and write the index entry
      indxRec.offset := fCacheStream.Size;
      indxRec.size := aLen;
      fIndexStream.Seek(0, soFromEnd);
      fIndexStream.WriteBuffer(indxRec, sizeOf(TIndexRecord));

      // write log record, write a record length at start
      // so it can be used to more quickly locate the start of the next
      // records (mainly to be used in index re-build and recovery tasks)
      fCacheStream.Seek(0, soFromEnd);
      fCacheStream.WriteBuffer(aBuffer^, aLen);

      // if this records are the first ever or if we are receiving
      // older records, notify. If we are receiving newer records
      // but there are existing records, do not notify as the index
      // is updated only after all newer records are received
      notify := (fOldIndexOffset=0) or (fLogState=lsGetLast);

      FreeMem(aBuffer);
    end;
  end;

  // check if it has been interrupted
  if notify then
    SendEvent(thread, LOGEVENT_RECORD, interrupt);

end;

procedure TLogCache.GetIndexRecord(var aIndex: SizeInt; out indxRec: TIndexRecord);
var
  sizeofIndex: Integer;
  indexOffset: SizeInt;
begin
  sizeofIndex := SizeOf(TIndexRecord);

  if aIndex<0 then begin
    // get the oldest item
    aIndex := fIndexStream.Size div sizeofIndex - 1;
  end;

  indexOffset := aIndex * sizeofIndex;

  fIndexStream.Position := indexOffset;
  fIndexStream.Read(indxRec{%H-}, sizeofIndex);
end;

procedure TLogCache.OnLogThreadDone(Sender: TObject);
var
  thread: TLogThread absolute Sender;
  dummy: boolean;
  aFileCache, aFileIndex: String;
  newState: TLogState;
begin

  aFileCache := GetFilename(FILENAME_CACHE);
  aFileIndex := GetFilename(FILENAME_INDEX);

  case fLogState of
    lsGetFirst:
      begin

        // some newer records were expected, if needed, update the
        // index so newer records appear at the top of the 'list'
        // if fOldIndexOffset=0 no re-index is needed as the received
        // records are the first ones and the current index is ok

        newState := lsEnd;

        if fOldIndexOffset>0 then begin

          // starting at fOldIndexOffset all next records should be
          // re-indexed
          ReIndex;

          if FileExists(aFileCache) then
            newState := lsGetLast;
        end;

        if fOldIndexOffset>=0 then begin
          // got some records and are ready
          // what record range was modified?
          dummy := false;
          SendEvent(thread, LOGEVENT_DONE, dummy);
        end;

        EnterLogState(newState);
      end;

    lsGetLast:
      begin

        // some older records were expected, if there are any
        // they will be at the end of the 'list', no re-index
        // is needed

        if fOldIndexOffset>0 then begin
          // got some records and are ready
          // what record range was modified?
          dummy := false;
          SendEvent(thread, LOGEVENT_DONE, dummy);
        end;

        EnterLogState(lsEnd);

      end;
  end;
end;

function TLogCache.GetCount: Integer;
begin
  if fIndexStream<>nil then
    result := fIndexStream.Size div SizeOf(TIndexRecord)
  else
    result := 0;
end;

procedure TLogCache.ItemFromCacheBuffer;
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

procedure TLogCache.ItemFromLogBuffer(aBuf: PChar);
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

constructor TLogCache.create(aLogEvent: TLogThreadEvent);
begin
  inherited Create;
  fMaxBufferSize := 1024 * 4;
  GetMem(fBuffer, fMaxBufferSize);
  fLogEvent := aLogEvent;
  fLastReadItemIndex := -1;
end;

destructor TLogCache.Destroy;
begin
  fIndexStream.Free;
  fCacheStream.Free;
  Finalize(fItem);
  FreeMem(fBuffer);
  inherited Destroy;
end;

procedure TLogCache.EnterLogState(aState: TLogState);
begin
  case aState of
    lsStart:    DoLogStateStart;
    lsGetFirst: DoLogStateGetFirst;
    lsGetLast:  DoLogStateGetLast;
    lsEnd:      DoLogStateEnd;
  end;
end;

procedure TLogCache.DoLogStateStart;
begin
  fLogState := lsStart;

  OpenCache;

  fNewDate := 0;
  fOldDate := 0;
  fOldDateIsStart := false;
  fOldIndexOffset := -1;
  fOldCount := fIndexStream.Size div SizeOf(TIndexRecord);

  if fIndexStream<>nil then begin
    if ReadLogItem(0) then
      fNewDate := fItem.CommiterDate;
  end;

  EnterLogState(lsGetFirst);
end;

procedure TLogCache.DoLogStateGetFirst;
begin
  fOldIndexOffset := -1;
  fLogState := lsGetFirst;
  Run;
end;

procedure TLogCache.DoLogStateGetLast;
var
  aRunFile: String;
begin
  fLogState := lsGetLast;

  OpenCache;

  // at the end of OpenCache fIndexStream must be ready
  if fIndexStream<>nil then begin

    // read the last log entry to find its date
    if ReadLogItem(-1) then
      fOldDate := fItem.CommiterDate;

    fOldIndexOffset := -1;

    Run;
  end else
    // no older records to retrive
    EnterLogState(lsEnd);
end;

procedure TLogCache.DoLogStateEnd;
var
  newCount: Int64;
  dummyInterrupt: boolean;
begin
  fLogState := lsEnd;

  newCount := fIndexStream.Size div SizeOf(TIndexRecord);
  if newCount<>fOldCount then begin

    // preserve the index
    fIndexStream.SaveToFile(GetFilename(FILENAME_INDEX));

    // flush TFileStream, how?....
    FileFlush(fCacheStream.Handle);
  end;

  dummyInterrupt := false;
  SendEvent(nil, LOGEVENT_END, dummyInterrupt);
end;

procedure TLogCache.Run;
var
  thread: TLogThread;
  cmd: string;
  aRunFile: String;
begin

  // prepare git log command
  cmd := fGit.Exe + ' log ' + LOG_CMD;

  case fLogState of
    lsGetFirst:
      begin
        if fNewDate<>0 then
          cmd += ' --after=' + IntToStr(fNewDate + 1);  // newest date found + 1 second
      end;

    lsGetLast:
      begin
        if fOldDate>0 then
          cmd += ' --before=' + IntToStr(fOldDate - 1); // oldest date found - 1 second
      end;

  end;

  // launch the log thread
  thread := TLogThread.Create;
  thread.Command := cmd;
  thread.StartDir := fGit.TopLevelDir;
  thread.FreeOnTerminate := true;
  thread.OnOutput := @OnLogThreadOutput;
  thread.OnTerminate := @OnLogThreadDone;
  thread.Start;
end;

function TLogCache.GetFilename(aIndex: Integer): string;
begin
  result := fGit.TopLevelDir + '.git' + PathDelim + 'lazgitgui.';
  case aIndex of
    FILENAME_INDEX:     result += 'logindex';
    FILENAME_CACHE:     result += 'logcache';
  end;
end;

procedure TLogCache.ReIndexStream(stream: TMemoryStream; offset: Integer);
var
  sizeofIndex, i: Integer;
  indx: TIndexRecord;
begin
  sizeofIndex := SizeOf(TIndexRecord);
  for i:=1 to stream.Size div sizeofIndex do begin
    // read existing record
    stream.Position := (i-1) * sizeofIndex;
    stream.Read(indx, sizeofIndex);
    // update with new offset
    indx.offset += offset;
    // write back the record
    stream.Position := (i-1) * sizeofIndex;
    stream.Write(indx, sizeofIndex);
  end;
end;

procedure TLogCache.ReIndex;
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

procedure TLogCache.OpenCache;
var
  aFileCache, aFileIndex: String;
  mode: Word;
begin
  aFileCache := GetFilename(FILENAME_CACHE);
  aFileIndex := GetFilename(FILENAME_INDEX);

  if fIndexStream=nil then begin
    fIndexStream := TMemoryStream.Create;
    if FileExists(aFileIndex) then
      fIndexStream.LoadFromFile(aFileIndex);
  end;

  if fCacheStream=nil then begin
    mode := fmOpenReadWrite + fmShareDenyWrite;
    if not FileExists(aFileCache) then
      mode += fmCreate;
    fCacheStream := TFileStream.Create(aFileCache, mode);
  end;

end;

procedure TLogCache.DumpItem;
var
  indxRec: TIndexRecord;
begin
  DebugLn;
  DebugLn('          Index: %d of %d',[fLastReadItemIndex, fIndexStream.Size div SizeOf(TIndexRecord)]);
  DebugLn('    Commit Date: %d (%s)',[fItem.CommiterDate, DateTimeToStr(UnixToDateTime(fItem.CommiterDate))]);
  DebugLn('     Parent OID: %s',[fItem.ParentOID]);
  DebugLn('     Commit OID: %s',[fItem.CommitOID]);
  DebugLn('         Author: %s',[fItem.ParentOID]);
  DebugLn('          Email: %s',[fItem.Email]);
  DebugLn('           Refs: %s',[fItem.Refs]);
  DebugLn('        Subject: %s',[fItem.Subject]);
  GetIndexRecord(fLastReadItemIndex, indxRec);
  DebugLn('   Cache Offset: %d', [indxRec.offset]);
  DebugLn('Cache Item Size: %d', [indxRec.size]);
end;

procedure TLogCache.SendEvent(thread: TLogThread; event: Integer;
  var interrupt: boolean);
begin
  if assigned(fLogEvent) then begin
    fLogEvent(self, thread, event, interrupt);
  end;
end;

procedure TLogCache.LoadCache;
begin

  // start cache update anyway
  EnterLogState(lsStart);
end;

function TLogCache.LoadIndex(aIndex: Integer): boolean;
begin
  result := ReadLogItem(aIndex);
end;


end.

