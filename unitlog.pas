unit unitlog;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

{.$define CaptureOutput}
{.$define Capture}
{.$define CaptureChunks}

interface

uses
  Classes, SysUtils, Math, DateUtils, LazLogger, SynEdit, unitansiescapes, unitgit,
  unitprocess, unitruncmd, unitlogcache;

type
  TRunThreadEvent = procedure(sender: TObject; thread: TRunThread; event: Integer; var interrupt: boolean) of object;

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
  LOG_CMD = '--pretty="format:%ct'#2'%p'#2'%h'#2'%an'#2'%ae'#2'%D'#2'%s'#2#3'" --all';

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

