unit unitlog;

{$mode ObjFPC}{$H+}
{.$define CaptureOutput}

interface

uses
  Classes, SysUtils, SynEdit, unitansiescapes, unitgit, unitruncmd;

const

  LOGEVENT_OUTPUT = 1;
  LOGEVENT_DONE   = 2;

type
  TLogEvent = procedure(sender: TObject; thread: TRunThread; event: Integer; var interrupt: boolean) of object;
  TLogItem = record
    CommitID: string;
    DateTime: TDateTime;
    Author: string;
    Subject: string;
  end;

  {
  #SEP="|"
  SEP="%x02"
  EOL="%x00"
  D="t"
  PRETTY="%p$SEP%h$SEP%an$SEP%ae$SEP%a$D$SEP%D$SEP%s$EOL"
  #MAX="--max-count=1000"
  FORMAT="tformat"
  ALL="--all"

  git log $MAX --pretty="$FORMAT:$PRETTY" $ALL > format.log
  }

  { TLogHandler }

  TLogHandler = class
  private
    fGit: TGit;
    fEdit: TSynEdit;
    fAnsiHandler: TAnsiEscapesHandler;
    flogEvent: TLogEvent;
    procedure LogDone(Sender: TObject);
    procedure LogOutput(sender: TObject; var interrupt: boolean);
    {$IFDEF CaptureOutput}
    fCap: TMemoryStream;
    {$ENDIF}
  public
    constructor create(theEditor: TSynEdit; aLogEvent: TLogEvent);
    destructor Destroy; override;
    procedure ShowLog;

    property Git: TGit read fGit write fGit;
  end;

implementation

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

constructor TLogHandler.create(theEditor: TSynEdit; aLogEvent: TLogEvent);
begin
  inherited Create;
  fEdit := theEditor;
  fAnsiHandler := TAnsiEscapesHandler.Create(fEdit);
  fLogEvent := aLogEvent;
end;

destructor TLogHandler.Destroy;
begin
  fAnsiHandler.Free;
  inherited Destroy;
end;

procedure TLogHandler.ShowLog;
var
  cmd: string;
  lst: TStringList;
begin
  fEdit.Clear;
  fAnsiHandler.Reset;

  // check if we have a log cache
  // do cache exists?
  //   No: Create it
  //   Yes: is cache up to date?
  //        No: Update it
  // show cached log

  if FileExists(fGit.TopLevelDir + 'lazgitgui.cache') then begin
    // is up to date?
    // the info file is a list of local refs with their latest oids
    lst := TStringList.Create;
    try
      lst.LoadFromFile(fGit.TopLevelDir + 'lazgitgui.info');
    finally
      lst.Free;
    end;
  end;

  {$IFDEF CaptureOutput}
  fCap := TMemoryStream.Create;
  {$ENDIF}
  cmd :=  fGit.Exe + ' ' +
          '-c color.ui=always ' +
          'log --oneline --graph --decorate --all';
  RunInThread(cmd, fGit.TopLevelDir, @LogOutput, @LogDone, true);
end;

end.

