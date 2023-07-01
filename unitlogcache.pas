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

  Deals with updating the log database
}
unit unitlogcache;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}
{.$define Debug}

{.$define Capture}
{.$define CaptureChunks}
{.$define CaptureHeadTail}
{$if defined(Debug) or defined(Capture) or defined(CaptureChunks) or defined(CaptureHeadTail)}
  {$define UseCounter}
{$endif}


interface

uses
  Classes, SysUtils, Math, DateUtils, LazLogger,
  unitgittypes, unitgit, unitprocess, unitifaces, unitdbindex, unitgitmgr;

const

  LOGEVENT_START  = 1;
  LOGEVENT_OUTPUT = 2;
  LOGEVENT_RECORD = 3;
  LOGEVENT_DONE   = 4;
  LOGEVENT_END    = 5;

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

  { TCacheLimits }

  TCacheLimits = class
  private
    fConfig: IConfig;
    fDb: TDbIndex;
    fGit: IGit;
    fRangeStart: Integer;
    fRangeEnd: Integer;
    fShaStart: string[41];
    fShaEnd: string[41];
    fMaxCount: Integer;
    function GetIsFiltered: boolean;
  public
    constructor create;
    function IsRestricted: boolean;
    procedure Setup;
    procedure Filter;

    property Db: TDbIndex read fDb write fDb;
    property Config: IConfig read fConfig write fConfig;
    property Git: IGit read fGit write fGit;
    property IsFiltered: boolean read GetIsFiltered;
    property RangeStart: Integer read fRangeStart;
    property RangeEnd: Integer read fRangeEnd;
  end;

  { TLogCache }

  TLogCache = class
  private
    fConfig: IConfig;
    fDbIndex: TDbIndex;
    fGitMgr: TGitMgr;
    fGit: IGit;
    fLogState: TLogState;
    fNeedNewerRecords: Boolean;
    fNeedOlderRecords: Boolean;
    fNewDate, fOldDate: Int64;
    flogEvent: TLogThreadEvent;
    fInterrupted: boolean;
    fLimits: TCacheLimits;
    fExistingCache: boolean;
    function GetRangeEnd: Integer;
    function GetRangeStart: Integer;
    procedure OnLogThreadDone(Sender: TObject);
    procedure OnLogThreadOutput(sender: TObject; var interrupt: boolean);
    procedure EnterLogState(aState: TLogState);
    procedure DoLogStateStart;
    procedure DoLogStateGetFirst;
    procedure DoLogStateGetLast;
    procedure DoLogStateEnd;
    procedure Run;
    procedure SendEvent(thread: TLogThread; event: Integer; var interrupt:boolean);
    procedure SetGitConfig(AValue: IConfig);
    procedure SetGitMgr(AValue: TGitMgr);
  public
    constructor create(aLogEvent: TLogThreadEvent);
    destructor Destroy; override;
    procedure UpdateCache;
    procedure Open;

    property LogState: TLogState read fLogState;
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property DbIndex: TDbIndex read fDbIndex write fDbIndex;
    property Config: IConfig read fConfig write SetGitConfig;
    property RangeStart: Integer read GetRangeStart;
    property RangeEnd: Integer read GetRangeEnd;
    property NeedNewerRecords: boolean read fNeedNewerRecords write fNeedNewerRecords;
    property NeedOlderRecords: boolean read fNeedOlderRecords write fNeedOlderRecords;

  end;

implementation

const
  //LOG_CMD = '--pretty="format:%ct'#2'%P'#2'%H'#2'%an'#2'%ae'#2'%s'#2#3'" --all';
  LOG_CMD = '--pretty="format:%ct'#2'%P'#2'%H'#2'%an'#2'%ae'#2'%s'#2#3'" --exclude=refs/stash --all';
  //LOG_CMD = '--pretty="format:%ct'#2'%P'#2'%H'#2'%an'#2'%ae'#2'%s'#2#3'" --branches --remotes --tags';


{ TCacheLimits }

function TCacheLimits.GetIsFiltered: boolean;
begin
  result := (fRangeStart>0) and (fRangeEnd>0) and (fRangeEnd>=fRangeStart);
end;

constructor TCacheLimits.create;
begin
  fRangeStart := -1;
  fRangeEnd := -1;
  fShaStart := '';
  fShaEnd := '';
  fMaxCount := 0;
end;

function TCacheLimits.IsRestricted: boolean;
begin
  result := (fDb=nil) or (fMaxCount>0) or
            ((fRangeStart>0) and (fRangeEnd<fDb.Count(true)) and (fRangeEnd>fRangeStart));
end;

procedure TCacheLimits.Setup;
begin

  if fDb=nil then
    raise Exception.Create('Apply restriction on closed db');
  if fGit=nil then
    raise Exception.Create('Setup without git interface');

  fShaStart := fConfig.ReadString('CommitStart', '', fGit.TopLevelDir);
  fShaEnd := fConfig.ReadString('CommitEnd', '', fGit.TopLevelDir);
  if (fShaStart<>'') and (fShaEnd<>'') then begin
    // find the corresponding indexes
    fRangeStart := fDb.FindCommitSha(fShaStart);
    if fRangeStart>=0 then
      fRangeEnd := fDb.FindCommitSha(fShaEnd, fRangeStart);
  end else begin
    fRangeStart := fConfig.ReadInteger('RangeStart', -1, fGit.TopLevelDir);
    fRangeEnd := fConfig.ReadInteger('RangeEnd', -1, fGit.TopLevelDir);
  end;
  if (fRangeStart>=0) and (fRangeEnd>=0) and (fRangeEnd>=fRangeStart) then begin
    fDb.MaxRecords := fRangeEnd + 1;
  end else
    fDb.MaxRecords := fConfig.ReadInteger('MaxLogRecords', 10000, fGit.TopLevelDir);
end;

procedure TCacheLimits.Filter;
var
  arr: TIntArray;
  i: Integer;
begin
  if (fDb<>nil) and (fRangeStart>=0) and (fRangeEnd<fDb.Count(true)) and (fRangeEnd>fRangeStart) then begin
    SetLength(Arr, fRangeEnd-fRangeStart+1);
    for i:=0 to Length(arr)-1 do Arr[i] := i + fRangeStart;
    fDb.SetFilter(arr);
  end;
end;

{ TLogThread }

procedure TLogThread.Notify;
var
  interrupt: boolean;
begin
  if assigned(fOnOutput) then begin
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
  Dispose(fCmdLine, done);
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
  newState: TLogState;
begin

  SendEvent(thread, LOGEVENT_DONE, fInterrupted);

  newState := lsEnd;

  case fLogState of
    lsGetFirst:
      begin
        if (not fInterrupted) and (fOldDate>0) then
          newState := lsGetLast;
      end;
    lsGetLast:
      if not fInterrupted then begin
        // was not interrupted while getting older records
        // it means there will be no older records anymore
        fConfig.WriteBoolean('NeedOlderRecords', false, fGit.TopLevelDir);
      end;
  end;

  EnterLogState(newState);
end;

function TLogCache.GetRangeEnd: Integer;
begin
  result := fLimits.RangeEnd;
end;

function TLogCache.GetRangeStart: Integer;
begin
  result := fLimits.RangeStart;
end;

constructor TLogCache.create(aLogEvent: TLogThreadEvent);
begin
  inherited Create;
  fLogEvent := aLogEvent;
  fLimits := TCacheLimits.Create;
  fNeedNewerRecords := true;
  fNeedOlderRecords := true;
end;

destructor TLogCache.Destroy;
begin
  fLimits.Free;
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
var
  aItem: TLogItem;
begin
  fLogState := lsStart;

  fDbIndex.Open;

  fNewDate := 0;
  fOldDate := 0;

  if fDbIndex.LoadItem(0, aItem, true) then begin
    fNewDate := aItem.CommiterDate;
    Finalize(aItem);
  end;


  if fDbIndex.LoadItem(-1, aItem, true) then begin
    fOldDate := aItem.CommiterDate;
    Finalize(aItem);
  end else
    fNeedOlderRecords := true; // invalidate config value

  EnterLogState(lsGetFirst);
end;

procedure TLogCache.DoLogStateGetFirst;
var
  interrupt: boolean;
begin

  if fNeedNewerRecords then
    // If we already have some records, or if there is no records yet
    // request a set of newer records.
    if (fOldDate>0) or (fDbIndex.Count(true)=0) then begin
      // only start updating if db is accepting new records and there aren't set restrictions
      // TODO: restrictions should not block cache updating ....
      if fDbIndex.AcceptingNewRecords then begin

        // send a start event and check if we can proceed
        fLogState := lsGetFirst;
        interrupt := false;
        SendEvent(nil, LOGEVENT_START, interrupt);

        if not interrupt then begin
          // no interrupted, launch the 'getfirst' thread
          Run;
          exit;
        end;
      end;
    end;

  // for some reason, 'getfirst' thread was not launched, check 'getlast'
  EnterLogState(lsGetLast);
end;

procedure TLogCache.DoLogStateGetLast;
begin
  if fNeedOlderRecords then
    // only start updating if db is accepting new records and there aren't set restrictions
    // TODO: restrictions should not block cache updating ....
    if fDbIndex.AcceptingNewRecords then begin
      fLogState := lsGetLast;
      Run;
      exit;
    end;

  EnterLogState(lsEnd)
end;

procedure TLogCache.DoLogStateEnd;
var
  dummyInterrupt: boolean;
  i: Integer;
  arr: TIntArray;
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
  if gblTopologicalMode then
    cmd := fGitMgr.git.Exe + ' log --topo-order ' + LOG_CMD
  else
    cmd := fGitMgr.git.Exe + ' log ' + LOG_CMD;

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

  {$IFDEF DEBUG}
  DebugLn('Launching for %s: %s',[BoolToStr(fLogState=lsGetFirst,'HEAD','TAIL'), cmd]);
  {$ENDIF}

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

procedure TLogCache.SetGitConfig(AValue: IConfig);
begin
  if fConfig = AValue then Exit;
  fConfig := AValue;
  fNeedNewerRecords := fConfig.ReadBoolean('NeedNewerRecords', true, fGit.TopLevelDir);
  fNeedOlderRecords := fConfig.ReadBoolean('NeedOlderRecords', true, fGit.TopLevelDir);
end;

procedure TLogCache.SetGitMgr(AValue: TGitMgr);
begin
  if fGitMgr = AValue then Exit;
  fGitMgr := AValue;
  fGit := fGitMgr.Git;
  fLimits.Git := fGit;
end;

procedure TLogCache.UpdateCache;
begin
  Open;
  // start cache update anyway
  EnterLogState(lsStart);
end;

procedure TLogCache.Open;
var
  firstTime: boolean;
begin
  firstTime := fDbIndex=nil;
  if firstTime then begin
    fDbIndex := TDbIndex.Create(fGit.GitDir + '.git' + PathDelim);
    fLimits.Db := fDbIndex;
    fLimits.Config := Config;
  end;

  fDbIndex.Open;

  if firstTime then begin
    fLimits.Setup;
    fLimits.Filter;
  end;
end;

end.

