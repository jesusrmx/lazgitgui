unit unitruncmd;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}
{$define Debug}

interface

uses
  Classes, SysUtils, LazLogger, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  unitprocess;

type
  TOutputStringEvent = procedure(sender: TObject; value: string; var interrupt:boolean) of object;

  { TRunThread }

  TRunThread = class(TThread)
  private
    fCommand: string;
    fOnOutput: TOutputStringEvent;
    fResult: Integer;
    fErrorLog: string;
    fStartDir: string;
    fLine: RawByteString;
    fCmdLine: ^TCmdLine;
    procedure Notify;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    property Command: string read fCommand write fCommand;
    property StartDir: string read fStartDir write fStartDir;
    property Result: Integer read fResult;
    property ErrorLog: string read fErrorLog;
    property OnOutput: TOutputStringEvent read fOnOutput write fOnOutput;
  end;

  { TfrmRunCommand }

  TfrmRunCommand = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkCloseOk: TCheckBox;
    lblResult: TLabel;
    lblCaption: TLabel;
    txtOutput: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fCommand: string;
    fResult: Integer;
    fRunThread: TRunThread;
    fStartDir: string;
    procedure OnDone(Sender: TObject);
    procedure OnOutput(sender: TObject; value: string; var interrupt: boolean);
    procedure SetCaption(AValue: string);
    procedure SetTitle(AValue: string);
  public
    property Command: string read fCommand write fCommand;
    property StartDir: string read fStartDir write fStartDir;
    property Title: string write SetTitle;
    property Caption: string write SetCaption;
    property Result: Integer read fResult;
  end;

  function RunInteractive(command, startdir, title, caption:string): Integer;

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
  rf.Title := title;
  rf.Caption := caption;
  try
    rf.ShowModal;
    result := rf.Result;
  finally
    rf.Free;
  end;
end;

{$R *.lfm}

{ TRunThread }

procedure TRunThread.Notify;
var
  interrupt: boolean;
begin
  if assigned(fOnOutput) then begin
    {$IFDEF DEBUG}
    DebugLn('Notifying: %s',[fLine]);
    {$ENDIF}
    interrupt := false;
    fOnOutput(Self, fLine, interrupt);
    if interrupt then
      terminate;
  end;
end;

constructor TRunThread.Create;
begin
  inherited Create(true);
  fCmdLine := new(PCmdLine, Init);
end;

destructor TRunThread.Destroy;
begin
  {$IFDEF DEBUG}
  debugln('TRunThread.Destroy: ');
  {$ENDIF}
  Dispose(fCmdLine);
  inherited Destroy;
end;

procedure TRunThread.Execute;
var
  outText: string;

  procedure CollectOutput(const buffer; size:Longint);
  var
    aPos: SizeInt;
  begin
    {$IFDEF DEBUG}
    DebugLn('Collecting %d bytes',[size]);
    {$ENDIF}
    aPos := Length(outText);
    SetLength(outText, aPos + size);
    Move(Buffer, OutText[aPos+1], size);

    aPos := pos(#10, outText);
    while (aPos>0) and not Terminated do begin
      fline := copy(outText, 1, aPos-1);
      if (aPos>2) and (fline[aPos-2]=#13) then
        delete(fLine, 1, apos-2);
      Synchronize(@Notify);
      Delete(outText, 1, aPos);
      aPos := pos(#10, outText);
    end;

  end;

begin
  {$IFDEF DEBUG}
  DebugLnEnter('RunThread START Command=%s', [fCommand]);
  {$ENDIF}
  outText := '';
  fCmdLine^.WaitOnExit := true;
  fResult := fCmdLine^.RunProcess(fCommand, fStartDir, @CollectOutput);
  if outText<>'' then begin
    fLine := outText;
    Synchronize(@Notify);
  end;
  fErrorLog := fCmdLine^.ErrorLog;
  {$IFDEF DEBUG}
  DebugLnExit('RunThread DONE result=%d', [fResult]);
  {$ENDIF}
end;

{ TfrmRunCommand }

procedure TfrmRunCommand.FormShow(Sender: TObject);
begin
  lblResult.Caption := 'Starting command, please wait ....';
  txtOutput.Clear;
  Application.ProcessMessages;

  fRunThread.Command := fCommand;
  fRunThread.StartDir := fStartDir;
  fRunThread.Start;
end;

procedure TfrmRunCommand.FormCreate(Sender: TObject);
begin
  fRunThread := TRunThread.Create;
  fRunThread.FreeOnTerminate := true;
  fRunThread.OnOutput := @OnOutput;
  fRunThread.OnTerminate := @OnDone;
end;

procedure TfrmRunCommand.SetCaption(AValue: string);
begin
  lblCaption.Caption := AValue;
end;

procedure TfrmRunCommand.OnOutput(sender: TObject; value: string;
  var interrupt: boolean);
begin
  txtOutput.Lines.Add(value);
  lblResult.Caption := 'Working ....';
end;

procedure TfrmRunCommand.OnDone(Sender: TObject);
begin
  {$IFDEF DEBUG}
  debugln('TfrmRunCommand.OnDone');
  {$ENDIF}
  if fRunThread.Result<=0 then begin
    lblResult.Color := clGreen;
    lblResult.Font.Color := clWhite;
    lblResult.Caption := 'Succeed';
  end else begin
    txtOutput.Append(fRunThread.ErrorLog);
    lblResult.Color := clRed;
    lblResult.Font.Color := clWhite;
    lblResult.Caption := 'Failed';
  end;
  txtOutput.SelLength := 0;
  txtOutput.SelStart := 0;
end;

procedure TfrmRunCommand.SetTitle(AValue: string);
begin
  Caption := AValue;
end;

end.

