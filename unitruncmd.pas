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

  Runs a command in a thread o runs a command presenting the user a window
  with the result.
}
unit unitruncmd;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}
{.$define Debug}

interface

uses
  Classes, SysUtils, Math, LazLogger, SynEdit, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ButtonPanel,
  unitcommon, unitifaces, unitconfig, unitprocess, unitansiescapes;

type
  TLineEnding = string[3];
  TCommandItemPreferredOutputType = (cipotString, cipotStream);
  TCommandItem = record
    description: string;
    command: string;
    RedirStdErr: boolean;
    PreferredOutputType: TCommandItemPreferredOutputType;
    Enviroment: string;
    tag: pointer;
  end;
  TCommandsArray = array of TCommandItem;
  TCommandProgressEvent = procedure(sender: TObject; item:TCommandItem; percent:single) of object;
  TCommandProgressEventNested = procedure(sender: TObject; item:TCommandItem; percent:single) is nested;


  // TODO: add crtitical sections where needed

  { TRunThread }

  TRunThread = class(TThread)
  private
    fCommand: string;
    fCommands: TCommandsArray;
    fHaveProgress: boolean;
    fLineEnding: TLineEnding;
    fOnCommandProgress: TCommandProgressEvent;
    fOnCommandProgressNested: TCommandProgressEventNested;
    fOnOutput: TNotifyInterruptEvent;
    fResult: Integer;
    fIndex: Integer;
    fErrorLog: string;
    fStartDir: string;
    fLine: RawByteString;
    fCurrentOutput: RawByteString;
    fCmdLine: TCmdLine;
    procedure Notify;
    procedure RunCommand;
    procedure RunCommandsArray;
    procedure DoCommandProgress;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    property Command: string read fCommand write fCommand;
    property Commands: TCommandsArray read fCommands write fCommands;
    property StartDir: string read fStartDir write fStartDir;
    property Result: Integer read fResult;
    property ErrorLog: string read fErrorLog;
    property CurrentOutput: RawByteString read fCurrentOutput;
    property OnOutput: TNotifyInterruptEvent read fOnOutput write fOnOutput;
    property HaveProgress: boolean read fHaveProgress write fHaveProgress;
    property LineEnding: TLineEnding read fLineEnding;
    property Line: string read fLine;
    property OnCommandProgress: TCommandProgressEvent read fOnCommandProgress write fOnCommandProgress;
    property OnCommandProgressNested: TCommandProgressEventNested read fOnCommandProgressNested write fOnCommandProgressNested;
  end;

  { TfrmRunCommand }

  TfrmRunCommand = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkCloseOk: TCheckBox;
    lblResult: TLabel;
    lblCaption: TLabel;
    txtOutput: TSynEdit;
    procedure chkCloseOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fCommand: string;
    fLastIndex: Integer;
    fCaretY, fCaretX: Integer;
    fResult: Integer;
    fRunThread: TRunThread;
    fStartDir: string;
    fAnsiHandler: TAnsiEscapesHandler;
    procedure OnDone(Sender: TObject);
    procedure OnOutput(sender: TObject; var interrupt: boolean);
  public
    property Command: string read fCommand write fCommand;
    property StartDir: string read fStartDir write fStartDir;
    property Result: Integer read fResult;
  end;

  function RunInteractive(command, startdir, title, caption:string): Integer;

  function RunInThread(Command, startDir: string; OnOutput: TNotifyInterruptEvent; OnDone: TNotifyEvent; startIt:boolean=true): TRunThread; overload;
  function RunInThread(Commands: TCommandsArray; startDir: string; OnProgress: TCommandProgressEvent; OnDone: TNotifyEvent; startIt:boolean=true; allowFails:boolean=false): TRunThread; overload;
  function RunInThread(Commands: TCommandsArray; startDir: string; OnProgress: TCommandProgressEventNested; OnDone: TNotifyEvent; startIt:boolean=true; allowFails:boolean=false): TRunThread; overload;
  function RunCommand(command, startDir: string; out cmdOut: RawByteString): Integer;
var
  frmRunCommand: TfrmRunCommand;

implementation

function RunInteractive(command, startdir, title, caption: string): Integer;
var
  rf: TfrmRunCommand;
begin
  rf := TfrmRunCommand.Create(Application);
  rf.Command := command;
  rf.StartDir := startdir;
  rf.lblCaption.Caption := Caption;
  rf.Caption := title;
  try
    rf.ShowModal;
    result := rf.Result;
  finally
    rf.Free;
  end;
end;

function RunInThread(Command, startDir: string; OnOutput: TNotifyInterruptEvent;
  OnDone: TNotifyEvent; startIt: boolean): TRunThread;
begin
  Result := TRunThread.Create;
  Result.Command := Command;
  Result.StartDir := startDir;
  Result.FreeOnTerminate := true;
  Result.OnOutput := OnOutput;
  Result.OnTerminate := OnDone;
  if startIt then
    Result.Start;
end;

function RunInThread(Commands: TCommandsArray; startDir: string;
  OnProgress: TCommandProgressEvent; OnDone: TNotifyEvent; startIt: boolean;
  allowFails: boolean): TRunThread;
begin
  Result := TRunThread.Create;
  Result.Commands := Commands;
  Result.StartDir := startDir;
  Result.FreeOnTerminate := true;
  Result.OnCommandProgress := OnProgress;
  Result.OnTerminate := OnDone;
  if startIt then
    Result.Start;
end;

function RunInThread(Commands: TCommandsArray; startDir: string;
  OnProgress: TCommandProgressEventNested; OnDone: TNotifyEvent;
  startIt: boolean; allowFails: boolean): TRunThread;
begin
  Result := TRunThread.Create;
  Result.Commands := Commands;
  Result.StartDir := startDir;
  Result.FreeOnTerminate := true;
  Result.OnCommandProgressNested := OnProgress;
  Result.OnTerminate := OnDone;
  if startIt then
    Result.Start;
end;

function RunCommand(command, startDir: string; out cmdOut: RawByteString
  ): Integer;
begin
  Result := CmdLine.RunProcess(command, startdir, cmdOut);
end;

{$R *.lfm}

{ TRunThread }

procedure TRunThread.Notify;
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

constructor TRunThread.Create;
begin
  inherited Create(true);
  fCmdLine := TCmdLine.Create;
end;

destructor TRunThread.Destroy;
begin
  {$IFDEF DEBUG}
  debugln('TRunThread.Destroy: ');
  {$ENDIF}
  fCmdLine.Free;
  inherited Destroy;
end;

procedure TRunThread.RunCommand;
var
  outText: string;

  function NextLineEnding(p: pchar): pchar;
  var
    x, y: pchar;
  begin
    x := strpos(p, #10);
    y := strpos(p, #13);
    if (x=nil) and (y=nil) then
      result := nil
    else if (x=nil) and (y<>nil) then
      result := y
    else if (x<>nil) and (y=nil) then
      result := x
    else if x<y then
      result := x
    else
      result := y;
  end;

  procedure CollectOutput(const buffer; size:Longint; var interrupt: boolean);
  var
    aPos: Integer;
    p, q: pchar;
  begin
    if terminated or interrupt then
      exit;
    {$IFDEF DEBUG}
    DebugLn('Collecting %d bytes',[size]);
    {$ENDIF}
    aPos := Length(outText);
    SetLength(outText, aPos + size);
    Move(Buffer, OutText[aPos+1], size);

    // buffer is guaranteed to end with #0
    while (outText<>'') and (not Terminated) do begin

      p := @outText[1];
      q := NextLineEnding(p);
      if q=nil then
        break;

      if (q^=#10) and ((q-1)^=#13) then
        dec(q);

      if (q^=#10) then
        fLineEnding := #10
      else if (q^=#13) and ((q+1)^=#10) then
        fLineEnding := #13#10
      else
        fLineEnding := #13;

      fLine := copy(outText, 1, q-p);

      Synchronize(@Notify);

      interrupt := terminated;
      if interrupt then
        break;

      delete(outText, 1, (q-p) + length(fLineEnding));

    end;
  end;

begin
  {$IFDEF DEBUG}
  DebugLnEnter('RunThread START Command=%s', [fCommand]);
  {$ENDIF}
  outText := '';
  //fCmdLine^.WaitOnExit := true;
  fCmdLine.RedirStdErr := true;
  fResult := fCmdLine.RunProcess(fCommand, fStartDir, @CollectOutput);
  if (outText<>'') and (not terminated) then begin
    fLine := outText;
    Synchronize(@Notify);
  end;
  fErrorLog := fCmdLine.ErrorLog;
  {$IFDEF DEBUG}
  DebugLnExit('RunThread DONE result=%d', [fResult]);
  {$ENDIF}
end;

procedure TRunThread.RunCommandsArray;
var
  M: TMemoryStream;
begin
  if (not Assigned(fOnCommandProgress)) and
     (not Assigned(fOnCommandProgressNested))
  then
    exit;
  fIndex := 0;
  while not terminated and (fIndex<Length(fCommands)) do begin
    fCmdLine.RedirStdErr := fCommands[fIndex].RedirStdErr;
    fCmdLine.Environment := fCommands[fIndex].Enviroment;
    try
      M := nil;
      if fCommands[fIndex].PreferredOutputType=cipotStream then begin
        M := TMemoryStream.Create;
        fResult := fCmdLine.RunProcess(fCommands[fIndex].command, fStartDir, M);
        fCommands[fIndex].tag := M;
      end else begin
        fResult := fCmdLine.RunProcess(fCommands[fIndex].command, fStartDir, fCurrentOutput);
      end;
      Synchronize(@DoCommandProgress);
    finally
      M.Free;
    end;
    inc(fIndex);
  end;
end;

procedure TRunThread.DoCommandProgress;
begin
  if not terminated and Assigned(fOnCommandProgress) then
    fOnCommandProgress(self, fCommands[fIndex], (fIndex+1)/Length(fCommands)*100)
  else if not terminated and Assigned(fOnCommandProgressNested) then
    fOnCommandProgressNested(self, fCommands[fIndex], (fIndex+1)/Length(fCommands)*100);
end;

procedure TRunThread.Execute;
begin
  if Length(fCommands)=0 then
    RunCommand
  else
    RunCommandsArray;
  Terminate;
end;

{ TfrmRunCommand }

procedure TfrmRunCommand.FormShow(Sender: TObject);
begin
  lblResult.Caption := rsStartingCommandPleaseWait;
  txtOutput.Clear;
  Application.ProcessMessages;

  fRunThread.Command := fCommand;
  fRunThread.StartDir := fStartDir;
  fRunThread.Start;
end;

procedure TfrmRunCommand.FormCreate(Sender: TObject);
begin

  fConfig.OpenConfig;

  fConfig.ReadWindow(Self, 'runcommandform', SECTION_GEOMETRY);
  fConfig.ReadFont(txtOutput.Font, 'RunCmdOutput', fpFixed, SECTION_FONTS);
  chkCloseOk.Checked := fConfig.ReadBoolean('CloseOnSuccess', false, 'RunCommandForm');

  fRunThread := TRunThread.Create;
  fRunThread.FreeOnTerminate := true;
  fRunThread.OnOutput := @OnOutput;
  fRunThread.OnTerminate := @OnDone;

  fLastIndex := -1;

  fAnsiHandler := TAnsiEscapesHandler.Create(txtOutput);

  fConfig.CloseConfig;
end;

procedure TfrmRunCommand.FormDestroy(Sender: TObject);
begin
  fAnsiHandler.Free;
end;

procedure TfrmRunCommand.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fConfig.WriteWindow(Self, 'runcommandform', SECTION_GEOMETRY);
end;

procedure TfrmRunCommand.chkCloseOkClick(Sender: TObject);
begin
  fConfig.WriteBoolean('CloseOnSuccess', chkCloseOk.Checked, 'RunCommandForm');
end;

procedure TfrmRunCommand.OnOutput(sender: TObject; var interrupt: boolean);
var
  thread: TRunThread absolute sender;
begin
  fAnsiHandler.ProcessLine(thread.Line, thread.LineEnding);
  lblResult.Caption := rsWorking;
end;

procedure TfrmRunCommand.OnDone(Sender: TObject);
begin
  {$IFDEF DEBUG}
  debugln('TfrmRunCommand.OnDone');
  {$ENDIF}
  fResult := fRunThread.Result;
  if fResult<=0 then begin
    lblResult.Color := clGreen;
    lblResult.Font.Color := clWhite;
    lblResult.Caption := rsSucceed;
    if chkCloseOk.Checked then Close;
  end else begin
    txtOutput.Append(fRunThread.ErrorLog);
    lblResult.Color := clRed;
    lblResult.Font.Color := clWhite;
    lblResult.Caption := rsFailed;
  end;
end;

end.

