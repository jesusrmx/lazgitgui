unit unitlogcache;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

{.$define Capture}
{.$define CaptureChunks}
{.$define CaptureHeadTail}
{$if defined(Debug) or defined(Capture) or defined(CaptureChunks) or defined(CaptureHeadTail)}
  {$define UseCounter}
{$endif}


interface

uses
  Classes, SysUtils, Math, DateUtils, LazLogger, unitgit, unitprocess, unitifaces, unitdbindex;

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
    fCacheStream: TFileStream;
    fCaptureTo: string;
    fCommand: string;
    fDbIndex: IDbIndex;
    fHaveProgress: boolean;
    fHead, fCacheUpdate: boolean;
    fIndexStream: TMemoryStream;
    fOldIndexSize: Int64;
    fOnOutput: TNotifyInterruptEvent;
    fResult: Integer;
    fErrorLog: string;
    fStartDir: string;
    fCmdLine: ^TCmdLine;
    fBufferSize: Integer;
    fOldIndexOffset: Int64;
    {$IFDEF Capture}
    fCap: TMemoryStream;
    {$ENDIF}
    procedure Notify;
  protected
    procedure DoTerminate; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    property Command: string read fCommand write fCommand;
    property StartDir: string read fStartDir write fStartDir;
    property Result: Integer read fResult;
    property ErrorLog: string read fErrorLog;
    property Buffer: PChar read fBuffer;
    property BufferSize: Integer read fBufferSize;
    property OnOutput: TNotifyInterruptEvent read fOnOutput write fOnOutput;
    property HaveProgress: boolean read fHaveProgress write fHaveProgress;
    property CaptureTo: string read fCaptureTo write fCaptureTo;
    //
    property DbIndex: IDbIndex read fDbIndex write fDbIndex;
    property Head: boolean read fHead write fHead;
  end;

  { TLogCache }

  TLogCache = class
  private
    fDbIndex: TDbIndex;
    fGit: TGit;
    fLogState: TLogState;
    fNewDate, fOldDate: Int64;
    fOldDateIsStart: boolean;
    flogEvent: TLogThreadEvent;
    fInterrupted: boolean;
    procedure OnLogThreadDone(Sender: TObject);
    procedure OnLogThreadOutput(sender: TObject; var interrupt: boolean);
    procedure EnterLogState(aState: TLogState);
    procedure DoLogStateStart;
    procedure DoLogStateGetFirst;
    procedure DoLogStateGetLast;
    procedure DoLogStateEnd;
    procedure Run;
    procedure SendEvent(thread: TLogThread; event: Integer; var interrupt:boolean);
  public
    constructor create(aLogEvent: TLogThreadEvent);
    destructor Destroy; override;
    procedure LoadCache;

    property LogState: TLogState read fLogState;
    property Git: TGit read fGit write fGit;
    property DbIndex: TDbIndex read fDbIndex write fDbIndex;
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

procedure TLogThread.DoTerminate;
begin
  fDbIndex.ThreadDone;
  inherited DoTerminate;
end;

var
  runner: Integer = 0;

procedure TLogThread.Execute;
var
  outText: string;

  procedure CollectOutputDummy(const buffer; size:Longint; var interrupt: boolean);
  begin

  end;

  procedure CollectOutput(const buffer; size: Longint;
    var interrupt: boolean);
  var
    aPos, remain: Integer;
    p, q, t: pchar;
    merged, collected: boolean;
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

    {$if defined(UseCounter) and not defined(CaptureHeadTail)}
    inc(counter);
    {$endif}

    {$IFDEF DEBUG}
    DebugLn('Collecting %d bytes, counter=%d',[size, counter]);
    {$ENDIF}

    {$if defined(Capture) or defined(CaptureChunks)}
    exit;
    {$endif}

    remain := Length(outtext);
    if remain=0 then
      p := @buffer
    else begin
      SetLength(outText, remain + size);
      Move(Buffer, OutText[remain+1], size);
      p := @outtext[1];
      remain += size;
      size := remain;
    end;

    merged := remain>0;
    collected := false;

    inc(runner);

    t := p + size;
    while p<t do begin

      q := strpos(p, #3);
      if (q<>nil) and (q<t) then begin

        if p^=#10 then begin
          inc(p);
          dec(remain);
        end;

        fBuffer := p;
        fBufferSize := q-p;

        fDbIndex.ThreadStore(p, q-p);

        Synchronize(@Notify);

        interrupt := terminated;
        if interrupt then
          break;

        inc(p, fBufferSize + 1);
        dec(remain, fBufferSize + 1);

        collected := true;

      end else begin

        if not merged then begin
          // can't find end of record in this chunk, as we dont have
          // a previous outtext buffer, copy this chunk to outext
          SetLength(outText, t-p);
          Move(p^, outText[1], t-p);
          exit;
        end;

        break;
      end;

    end;

    if merged then begin
      // remove consumed chars from outtext
      Delete(outtext, 1, size - remain);
      // if the last operation collected the end of log line and
      // only remains a lf consume it too.
      if collected and (remain=1) and (outtext[1]=#10) then
        outText := '';
    end;

  end;

begin
  {$IFDEF DEBUG}
  DebugLnEnter('RunThread START Command=%s', [fCommand]);
  {$ENDIF}
  DebugLn('TLogThread: Execute: for %s Index: position=%d size=%d',[BoolToStr(fHead,'HEAD','TAIL'), fOldIndexOffset, fOldIndexSize]);
  outText := '';
  //fCmdLine^.WaitOnExit := true;
  fCmdLine^.RedirStdErr := true;
  fCmdLine^.CaptureFile := CaptureTo;
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

procedure TLogCache.OnLogThreadOutput(sender: TObject; var interrupt: boolean);
var
  thread: TLogThread absolute sender;
begin
  SendEvent(thread, LOGEVENT_RECORD, interrupt);
  fInterrupted := interrupt;
end;

procedure TLogCache.OnLogThreadDone(Sender: TObject);
var
  thread: TLogThread absolute Sender;
  dummy: boolean;
  newState: TLogState;
begin

  newState := lsEnd;

  case fLogState of
    lsGetFirst:
      begin
        if (not fInterrupted) and (fOldDate>0) then
          newState := lsGetLast;
      end;
  end;

  EnterLogState(newState);

  dummy := false;
  SendEvent(thread, LOGEVENT_DONE, dummy);
end;

constructor TLogCache.create(aLogEvent: TLogThreadEvent);
begin
  inherited Create;
  fLogEvent := aLogEvent;
end;

destructor TLogCache.Destroy;
begin
  fDbIndex.Free;
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

  fDbIndex.Open;

  fOldDateIsStart := false;
  fNewDate := 0;
  fOldDate := 0;

  if fDbIndex.LoadItem(0) then
    fNewDate := fDbIndex.Item.CommiterDate;

  if fDbIndex.LoadItem(-1) then
    fOldDate := fDbIndex.Item.CommiterDate;

  EnterLogState(lsGetFirst);
end;

procedure TLogCache.DoLogStateGetFirst;
begin
  if (fOldDate>0) or (fDbIndex.Count=0) then begin
    fLogState := lsGetFirst;
    Run;
  end else
    EnterLogState(lsEnd);
end;

procedure TLogCache.DoLogStateGetLast;
begin
  fLogState := lsGetLast;
  Run;
end;

procedure TLogCache.DoLogStateEnd;
var
  dummyInterrupt: boolean;
begin
  fLogState := lsEnd;

  dummyInterrupt := false;
  SendEvent(nil, LOGEVENT_END, dummyInterrupt);
end;

procedure TLogCache.Run;
var
  thread: TLogThread;
  cmd: string;
  aRunFile: String;
  db: IDbIndex;
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

  DebugLn('Launching for %s: %s',[BoolToStr(fLogState=lsGetFirst,'HEAD','TAIL'), cmd]);

  db := fDbIndex;
  db.ThreadStart(fLogState=lsGetFirst);

  // launch the log thread
  thread := TLogThread.Create;
  thread.Command := cmd;
  thread.StartDir := fGit.TopLevelDir;
  thread.FreeOnTerminate := true;
  thread.OnOutput := @OnLogThreadOutput;
  thread.OnTerminate := @OnLogThreadDone;
  thread.DbIndex := fDbIndex;
  thread.Head := fLogState=lsGetFirst;
  {$ifdef CaptureHeadTail}
  repeat
    Inc(Counter);
    if thread.Head then aRunFile := format('head_%.2d.bin', [Counter])
    else                aRunFile := format('tail_%.2d.bin', [Counter]);
  until not FileExists(aRunFile);
  thread.CaptureTo := aRunFile;
  {$endif}
  thread.Start;
end;

procedure TLogCache.SendEvent(thread: TLogThread; event: Integer;
  var interrupt: boolean);
begin
  if assigned(fLogEvent) then
    fLogEvent(self, thread, event, interrupt);
end;

procedure TLogCache.LoadCache;
begin
  if fDbIndex=nil then
    fDbIndex := TDbIndex.Create(fGit.TopLevelDir + '.git' + PathDelim);
  // start cache update anyway
  EnterLogState(lsStart);
end;

end.

