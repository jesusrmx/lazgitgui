unit unitruncmd;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}
{$define Debug}

interface

uses
  Classes, SysUtils, Math, LazLogger, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  unitconfig, unitprocess;

type
  TOutputStringEvent = procedure(sender: TObject; var interrupt:boolean) of object;
  TLineEnding = string[3];

  { TRunThread }

  TRunThread = class(TThread)
  private
    fCommand: string;
    fHaveProgress: boolean;
    fLineEnding: TLineEnding;
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
    property HaveProgress: boolean read fHaveProgress write fHaveProgress;
    property LineEnding: TLineEnding read fLineEnding;
    property Line: string read fLine;
  end;

  { TfrmRunCommand }

  TfrmRunCommand = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkCloseOk: TCheckBox;
    lblResult: TLabel;
    lblCaption: TLabel;
    txtOutput: TMemo;
    procedure chkCloseOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fCommand: string;
    fLastIndex: Integer;
    fCaretY, fCaretX: Integer;
    fResult: Integer;
    fRunThread: TRunThread;
    fStartDir: string;
    fTitle: String;
    procedure OnDone(Sender: TObject);
    procedure OnOutput(sender: TObject; var interrupt: boolean);
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

  procedure CollectOutput(const buffer; size:Longint);
  var
    aPos: Integer;
    p, q: pchar;
  begin
    {$IFDEF DEBUG}
    DebugLn('Collecting %d bytes',[size]);
    {$ENDIF}
    aPos := Length(outText);
    SetLength(outText, aPos + size);
    Move(Buffer, OutText[aPos+1], size);

    // buffer is guaranteed to end with #0
    while outText<>'' do begin

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

      delete(outText, 1, (q-p) + length(fLineEnding));

    end;
  end;

begin
  {$IFDEF DEBUG}
  DebugLnEnter('RunThread START Command=%s', [fCommand]);
  {$ENDIF}
  outText := '';
  fCmdLine^.WaitOnExit := true;
  fCmdLine^.RedirStdErr := true;
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
  Caption := fTitle;
  txtOutput.Clear;
  Application.ProcessMessages;

  fRunThread.Command := fCommand;
  fRunThread.StartDir := fStartDir;
  fRunThread.Start;
end;

procedure TfrmRunCommand.FormCreate(Sender: TObject);
begin

  fConfig.ReadWindow(Self, 'runcommandform', SECTION_GEOMETRY);
  fConfig.ReadFont(txtOutput.Font, 'RunCmdOutput', fpFixed, SECTION_FONTS);
  chkCloseOk.Checked := fConfig.ReadBoolean('CloseOnSuccess', false, 'RunCommandForm');

  fRunThread := TRunThread.Create;
  fRunThread.FreeOnTerminate := true;
  fRunThread.OnOutput := @OnOutput;
  fRunThread.OnTerminate := @OnDone;

  fLastIndex := -1;
end;

procedure TfrmRunCommand.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fConfig.WriteWindow(Self, 'runcommandform', SECTION_GEOMETRY);
end;

procedure TfrmRunCommand.chkCloseOkClick(Sender: TObject);
begin
  fConfig.WriteBoolean('CloseOnSuccess', chkCloseOk.Checked, 'RunCommandForm');
end;

procedure TfrmRunCommand.SetCaption(AValue: string);
begin
  lblCaption.Caption := AValue;
end;

procedure TfrmRunCommand.OnOutput(sender: TObject; var interrupt: boolean);
var
  thread: TRunThread absolute sender;
begin

  if pos(#13, thread.LineEnding)>0 then
    fCaretX := 0;

  // get line at fCaretY
  while fCaretY+1>txtOutput.Lines.Count do
    txtOutput.Lines.Add('');

  txtOutput.Lines[fCaretY] := thread.Line;

  if pos(#10, thread.LineEnding)>0 then
    inc(fCaretY);

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
    if chkCloseOk.Checked then Close;
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
  fTitle := AValue;
end;

end.

