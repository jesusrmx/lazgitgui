unit unitlog;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

{.$define CaptureOutput}
{.$define Capture}
{.$define CaptureChunks}

interface

uses
  Classes, SysUtils, Math, DateUtils, LazLogger, SynEdit, unitansiescapes, unitgit,
  unitprocess, unitruncmd;

const

  LOGEVENT_OUTPUT = 1;
  LOGEVENT_DONE   = 2;

  FILENAME_INFO     = 1;
  FILENAME_INDEX    = 2;
  FILENAME_CACHE    = 3;
  FILENAME_RUN      = 4;
  FILENAME_RUNINDEX = 5;

type
  TLogThread = class;

  TLogThreadEvent = procedure(sender: TObject; thread: TLogThread; event: Integer; var interrupt: boolean) of object;
  TRunThreadEvent = procedure(sender: TObject; thread: TRunThread; event: Integer; var interrupt: boolean) of object;

  TLogItem = record
    CommiterDate: Int64;
    ParentOID, CommitOID,
    Refs,
    Author,
    Email,
    Subject: RawByteString;
  end;

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
    fOnOutput: TOutputStringEvent;
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
    property OnOutput: TOutputStringEvent read fOnOutput write fOnOutput;
    property HaveProgress: boolean read fHaveProgress write fHaveProgress;
  end;

  { TLogCache }

  TLogCache = class
  private
    fGit: TGit;
    fCacheStream, fRunStream: TFileStream;
    fIndexStream, fIndexRun: TMemoryStream;
    fBuffer: PChar;
    fMaxBufferSize: Integer;
    fEntryReadSize: Integer;
    fItem: TLogItem;
    fLogState: TLogState;
    fNewDate, fOldDate: Int64;
    fOldDateIsStart: boolean;
    flogEvent: TLogThreadEvent;
    function GetCount: Integer;
    procedure OnLogThreadDone(Sender: TObject);
    procedure OnLogThreadOutput(sender: TObject; var interrupt: boolean);
    function ReadLogItem(aIndex:SizeInt): boolean;
    procedure CacheBufferFromItem(out aBuf: PChar; out len: Integer);
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
    procedure OpenCache;
    procedure ClearRunStreams;
  public
    constructor create(aLogEvent: TLogThreadEvent);
    destructor Destroy; override;
    procedure LoadCache;

    property Git: TGit read fGit write fGit;
    property Count: Integer read GetCount;
  end;

  { TLogHandler }

  TLogHandler = class
  private
    fGit: TGit;
    fEdit: TSynEdit;
    fAnsiHandler: TAnsiEscapesHandler;
    flogEvent: TRunThreadEvent;
    fLogCache: TLogCache;
    procedure LogDone(Sender: TObject);
    procedure LogOutput(sender: TObject; var interrupt: boolean);
    procedure OnLogCacheEvent(sender: TObject; thread: TLogThread;
      event: Integer; var interrupt: boolean);
    {$IFDEF CaptureOutput}
    fCap: TMemoryStream;
    {$ENDIF}
  public
    constructor create(theEditor: TSynEdit; aLogEvent: TRunThreadEvent);
    destructor Destroy; override;
    procedure ShowLog;

    property Git: TGit read fGit write fGit;
  end;

implementation

const
  FILE_INFO_SIGNATURE = 'lazgitgui-v1.0';
  LOG_CMD = '--pretty="format:%ct'#2'%p'#2'%h'#2'%an'#2'%ae'#2'%D'#2'%s'#2#3'" --all';

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

var
  counter: Integer = 0;

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
    s: string;
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

    inc(counter);
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
  indexOffset: QWord;
  indxRec: TIndexRecord;
  sizeofIndex: Integer;
begin
  result := fIndexStream<>nil;
  if result then begin

    sizeofIndex := SizeOf(TIndexRecord);

    if aIndex<0 then begin
      // get the oldest item
      aIndex := fIndexStream.Size div sizeofIndex - 1;
    end;

    indexOffset := aIndex * sizeofIndex;

    fIndexStream.Position := indexOffset;
    fIndexStream.Read(indxRec{%H-}, sizeofIndex);

    result := indxRec.offset + indxRec.size <= fCacheStream.Size ;
    if result then begin

      if indxRec.size>fMaxBufferSize then begin
        fMaxBufferSize := indxRec.size * 2;
        ReallocMem(fBuffer, fMaxBufferSize);
      end;

      fCacheStream.Position := indxRec.offset;
      fEntryReadSize := fCacheStream.Read(fBuffer^, indxRec.size);

      ItemFromCacheBuffer;
    end;

  end;
end;

procedure TLogCache.CacheBufferFromItem(out aBuf: PChar; out len: Integer);
var
  p: pchar;

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

  len := SizeOf(fItem.CommiterDate);
  len += Min(Length(fItem.ParentOID), High(Byte)) + 1;
  len += Min(Length(fItem.CommitOID), High(Byte)) + 1;
  len += Min(Length(fItem.Author), High(Byte)) + 1;
  len += Min(Length(fItem.Email), High(Byte)) + 1;
  len += Min(Length(fItem.Refs), High(word)) + 2;
  len += Min(Length(fItem.Subject), High(word)) + 2;

  GetMem(aBuf, len);
  p := aBuf;

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
  aLen: Integer;
  indxRec: TIndexRecord;
begin
  {$IFDEF CaptureOutput}
  if thread.Line<>'' then
    fCap.WriteBuffer(thread.Line[1], Length(thread.Line));
  fCap.WriteBuffer(thread.LineEnding[1], Length(thread.LineEnding));
  {$ENDIF}

  ItemFromLogBuffer(thread.Buffer);

  CacheBufferFromItem(aBuffer, aLen);

  case fLogState of
    lsGetFirst: begin
      // write log index record
      indxRec.offset := fRunStream.Position;
      indxRec.size := aLen;
      fIndexRun.WriteBuffer(indxRec, sizeOf(TIndexRecord));
      // write log record
      fRunStream.WriteBuffer(aBuffer^, aLen);
    end;
  end;

  FreeMem(aBuffer);

  // check if it has been interrupted
  if assigned(fLogEvent) then begin
    fLogEvent(self, thread, LOGEVENT_OUTPUT, interrupt);
    if interrupt then
      exit;
  end;

end;

procedure AppendFile(toFile, fromFile: string);
var
  fTo, fFrom: TFileStream;
begin
  fTo := TFileStream.Create(toFile, fmOpenWrite + fmShareDenyWrite);
  fFrom := TFileStream.Create(fromFile, fmOpenRead + fmShareDenyNone);
  try
    fTo.Seek(0, soFromEnd);
    fTo.CopyFrom(fFrom, fFrom.Size);
  finally
    fFrom.Free;
    fTo.Free;
  end;
end;

procedure TLogCache.OnLogThreadDone(Sender: TObject);
var
  thread: TLogThread absolute Sender;
  dummy: boolean;
  aFileCache, aFileRun, aFileIndex: String;
  runStreamSize, cacheStreamSize: Int64;
begin

  aFileCache := GetFilename(FILENAME_CACHE);
  aFileRun := GetFilename(FILENAME_RUN);
  aFileIndex := GetFilename(FILENAME_INDEX);

  case fLogState of
    lsGetFirst:
      begin
        runStreamSize := fRunStream.Size;
        if runStreamSize=0 then begin
          // nothing was received, if there is already a cache
          // try to get the oldest records
          if FileExists(aFileCache) then begin
            EnterLogState(lsGetLast);
            exit;
          end;
        end;

        // access the cache or the index is forbidden starting here !!!
        // fLocked := true;

        if not FileExists(aFileCache) then begin
          // close active files if any
          FreeAndNil(fRunStream);
          FreeAndNil(fCacheStream);

          // The cache file do not exists, the received records are the new cache
          RenameFile(aFileRun, aFileCache);
          fIndexRun.SaveToFile(aFileIndex);
          FreeAndNil(fIndexRun);
          // now queue a re-open
          FreeAndNil(fIndexStream);

          // not necessary to get new records,
          EnterLogState(lsEnd);
        end else begin

          // the received records are the first records in the new cache
          fCacheStream.Position := 0;
          fRunStream.Seek(0, soFromEnd);
          fRunStream.CopyFrom(fCacheStream, fCacheStream.Size);

          // free & save run stream, stop using cachestream
          FreeAndNil(fRunStream);
          FreeAndNil(fCacheStream);

          // delete old cache file, new cache file is run cache file
          DeleteFile(aFileCache);
          RenameFile(aFileRun, aFileCache);

          // a re-index of the existing records is in order
          ReIndexStream(fIndexStream, runStreamSize);

          // merge indexes, run index is first
          fIndexStream.Position := 0;
          fIndexRun.Seek(0, soFromEnd);
          fIndexRun.CopyFrom(fIndexStream, fIndexStream.Size);
          fIndexRun.SaveToFile(aFileIndex);

          // fIndexRun is up to date swapping pointers with fIndexStream
          // would do it, but fCacheStream needs to be reopened again
          // and it will reload only if fIndexStream=nil, so queue a re-open
          FreeAndNil(fIndexRun);
          FreeAndNil(fIndexStream);

          // got newer records, now try to get older records...
          EnterLogState(lsGetLast);
        end;

      end;

    lsGetLast:
      begin

        runStreamSize := fRunStream.Size;
        if runStreamSize=0 then begin
          // nothing was received, nothing more to do
          EnterLogState(lsEnd);
          exit;
        end;
        cacheStreamSize := fCacheStream.Size;

        // access the cache or the index is forbidden starting here !!!
        // fLocked := true;

        // the received records are the last records in the new cache
        fRunStream.Position := 0;
        fCacheStream.Seek(0, soFromEnd);
        fCacheStream.CopyFrom(fRunStream, fRunStream.Size);

        // Run stream is not needed anymore
        FreeAndNil(fRunStream);
        DeleteFile(aFileRun);

        // the running index must be re-indexed
        ReIndexStream(fIndexRun, cacheStreamSize);

        // merge the runing index with the existing index
        fIndexRun.Position := 0;
        fIndexStream.Seek(0, soFromEnd);
        fIndexStream.CopyFrom(fIndexRun, fIndexRun.Size);
        // fIndexRun stream is not needed anymore
        FreeAndNil(fIndexRun);
        // Save the updated index
        fIndexStream.SaveToFile(aFileIndex);

        // access the cache or the index is safe now
        // fLocked := false;

        // we are done ...
        EnterLogState(lsEnd);
      end;
  end;

  {$IFDEF CaptureOutput}
  fCap.SaveToFile('colorido.bin');
  fCap.Free;
  {$endif}
  if assigned(fLogEvent) then begin
    dummy := false;
    fLogEvent(Self, thread, LOGEVENT_DONE, dummy);
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
end;

destructor TLogCache.Destroy;
begin
  fIndexStream.Free;
  fCacheStream.Free;
  ClearRunStreams;
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

  fNewDate := 0;
  fOldDate := 0;
  fOldDateIsStart := false;

  if fIndexStream<>nil then begin
    if ReadLogItem(0) then
      fNewDate := fItem.CommiterDate;
  end;

  EnterLogState(lsGetFirst);
end;

procedure TLogCache.DoLogStateGetFirst;
begin
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

    Run;
  end else
    // no older records to retrive
    EnterLogState(lsEnd);
end;

procedure TLogCache.DoLogStateEnd;
begin
  fLogState := lsEnd;

  OpenCache;
end;

procedure TLogCache.Run;
var
  thread: TLogThread;
  cmd: string;
  aRunFile: String;
begin

  // make sure run streams start clean
  ClearRunStreams;
  aRunFile := GetFilename(FILENAME_RUN);
  fRunStream := TFileStream.Create(aRunFile, fmCreate + fmShareExclusive);
  fIndexRun := TMemoryStream.Create;

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
    FILENAME_INDEX:     result += 'index';
    FILENAME_CACHE:     result += 'cache';
    FILENAME_RUN:       result += 'run';
    FILENAME_RUNINDEX:  result += 'runindex';
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

procedure TLogCache.OpenCache;
var
  aFileCache, aFileIndex: String;
begin
  aFileCache := GetFilename(FILENAME_CACHE);
  aFileIndex := GetFilename(FILENAME_INDEX);
  if FileExists(aFileIndex) then begin
    if fIndexStream=nil then begin
      // streams are closed, create and open
      fIndexStream := TMemoryStream.Create;
      fIndexStream.LoadFromFile(aFileIndex);
      fCacheStream := TFileStream.Create(aFileCache, fmOpenRead + fmShareDenyNone);
      // now is no locked anymore
      // fLocked := false;
    end;
  end;
end;

procedure TLogCache.ClearRunStreams;
begin
  fRunStream.Free;
  fIndexRun.Free;
end;

procedure TLogCache.LoadCache;
begin

  OpenCache;

  // start cache update anyway
  EnterLogState(lsStart);
end;

{ TLogHandler }

procedure TLogHandler.LogOutput(sender: TObject; var interrupt: boolean);
var
  thread: TRunThread absolute sender;
begin
  {$IFDEF CaptureOutput}
  if thread.Line<>'' then
    fCap.WriteBuffer(thread.Line[1], Length(thread.Line));
  fCap.WriteBuffer(thread.LineEnding[1], Length(thread.LineEnding));
  {$ENDIF}

  // check if it has been interrupted
  fLogEvent(self, thread, LOGEVENT_OUTPUT, interrupt);
  if interrupt then
    exit;

  fAnsiHandler.ProcessLine(thread.Line, thread.LineEnding);
end;

procedure TLogHandler.OnLogCacheEvent(sender: TObject; thread: TLogThread;
  event: Integer; var interrupt: boolean);
begin

end;

procedure TLogHandler.LogDone(Sender: TObject);
var
  thread: TRunThread absolute Sender;
  dummy: boolean;
begin
  {$IFDEF CaptureOutput}
  fCap.SaveToFile('colorido.bin');
  fCap.Free;
  {$endif}
  dummy := false;
  fLogEvent(Self, thread, LOGEVENT_DONE, dummy);
end;

constructor TLogHandler.create(theEditor: TSynEdit; aLogEvent: TRunThreadEvent);
begin
  inherited Create;
  fEdit := theEditor;
  fAnsiHandler := TAnsiEscapesHandler.Create(fEdit);
  fLogEvent := aLogEvent;
end;

destructor TLogHandler.Destroy;
begin
  fAnsiHandler.Free;
  fLogCache.Free;
  inherited Destroy;
end;

procedure TLogHandler.ShowLog;
begin

  if fLogCache=nil then begin
    fLogCache := TLogCache.create(@OnLogCacheEvent);
    fLogCache.Git := fGit;
  end;

  fLogCache.LoadCache;
end;

end.

