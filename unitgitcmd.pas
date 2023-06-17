unit unitgitcmd;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  unitcommon, unitconfig, unitifaces;

type

  { TfrmGitCmd }

  TfrmGitCmd = class(TForm)
    panBtns: TButtonPanel;
    chkRemember: TCheckBox;
    lblInfo: TLabel;
    txtGitCmd: TEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtGitCmdChange(Sender: TObject);
  private
    procedure UpdateInfo;
  public

  end;

var
  frmGitCmd: TfrmGitCmd;

implementation

{$R *.lfm}

{ TfrmGitCmd }

procedure TfrmGitCmd.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fConfig.WriteWindow(Self, 'gitcmdfrm', SECTION_GEOMETRY);
end;

procedure TfrmGitCmd.FormCreate(Sender: TObject);
begin
  fConfig.ReadWindow(Self, 'gitcmdfrm', SECTION_GEOMETRY);
end;

procedure TfrmGitCmd.FormShow(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TfrmGitCmd.txtGitCmdChange(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TfrmGitCmd.UpdateInfo;
var
  s: string;
begin
  panBtns.OKButton.Enabled := false;
  s := txtGitCmd.Text;
  DebugLn('text=%s',[s]);
  if Trim(s)='' then begin
    lblInfo.Caption := rsTheCommandIsEmpty;
    exit;
  end;
  if pos('git', s)<>1 then begin
    lblInfo.Caption := rsCommandMustStartWithGit;
    exit;
  end;
  if trim(s)='git' then begin
    lblInfo.Caption := rsIncompleteCommand;
    exit;
  end;
  lblInfo.Caption := '';
  panBtns.OKButton.Enabled := true;
end;

end.

