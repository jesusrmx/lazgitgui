unit unitruncmd;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  unitprocess;

type

  { TfrmRunCommand }

  TfrmRunCommand = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkCloseOk: TCheckBox;
    lblResult: TLabel;
    lblCaption: TLabel;
    txtOutput: TMemo;
    procedure FormShow(Sender: TObject);
  private
    fCommand: string;
    fResult: Integer;
    fStartDir: string;
    procedure SetCaption(AValue: string);
    procedure SetTitle(AValue: string);
    function Run(cmd, startDir: string): Integer;
  public
    property Command: string read fCommand write fCommand;
    property StartDir: string read fStartDir write fStartDir;
    property Title: string write SetTitle;
    property Caption: string write SetCaption;
    property Result: Integer read fResult;
  end;

var
  frmRunCommand: TfrmRunCommand;

implementation

{$R *.lfm}

{ TfrmRunCommand }

procedure TfrmRunCommand.FormShow(Sender: TObject);
begin
  fResult := Run(fCommand, fStartDir);
end;

procedure TfrmRunCommand.SetCaption(AValue: string);
begin
  lblCaption.Caption := AValue;
end;

procedure TfrmRunCommand.SetTitle(AValue: string);
begin
  Caption := AValue;
end;

function TfrmRunCommand.Run(cmd, startDir: string): Integer;
var
  outText: string;

  procedure OnOutput(const buffer; size:Longint);
  var
    aPos: SizeInt;
    line: RawByteString;
  begin
    aPos := Length(outText);
    SetLength(outText, aPos + size);
    Move(Buffer, OutText[aPos+1], size);

    aPos := pos(#10, outText);
    while aPos>0 do begin
      line := copy(outText, 1, aPos-1);
      if (aPos>2) and (line[aPos-2]=#13) then
        delete(line, 1, apos-2);
      txtOutput.Lines.Add(line);
      Application.ProcessMessages;
      Delete(outText, 1, aPos);
      aPos := pos(#10, outText);
    end;

  end;
begin
  outText := '';
  lblResult.Caption := 'Working, please wait ....';
  txtOutput.Clear;
  Application.ProcessMessages;
  result := cmdLine.RunProcess(cmd, startDir, @OnOutput);
  if result<=0 then begin
    lblResult.Color := clGreen;
    lblResult.Font.Color := clWhite;
    lblResult.Caption := 'Succeed';
  end else begin
    lblResult.Color := clRed;
    lblResult.Font.Color := clWhite;
    lblResult.Caption := 'Failed';
  end;
  Application.ProcessMessages;
end;

end.

