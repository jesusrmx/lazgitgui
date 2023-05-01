unit unitcheckouttag;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  unitgitutils, unitgitmgr;

type

  { TfrmCheckouTag }

  TfrmCheckouTag = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkCreateBranch: TCheckBox;
    Label1: TLabel;
    lblTag: TLabel;
    lblHint: TLabel;
    lblBranch: TLabel;
    txtBranchName: TEdit;
    procedure chkCreateBranchClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtBranchNameChange(Sender: TObject);
  private
    fGitMgr: TGitMgr;
    fTagName: string;
    procedure CheckOkButton;
    procedure SetGitMgr(AValue: TGitMgr);
  public
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property TagName: string read fTagName write fTagName;
  end;

var
  frmCheckouTag: TfrmCheckouTag;

implementation

{$R *.lfm}

{ TfrmCheckouTag }

procedure TfrmCheckouTag.chkCreateBranchClick(Sender: TObject);
begin
  lblBranch.Enabled := chkCreateBranch.Checked;
  txtBranchName.Enabled := lblBranch.Enabled;
  CheckOkButton;
end;

procedure TfrmCheckouTag.FormShow(Sender: TObject);
begin
  lblTag.Caption := fTagName;
  CheckOkButton;
end;

procedure TfrmCheckouTag.txtBranchNameChange(Sender: TObject);
begin
  CheckOkButton;
end;

procedure TfrmCheckouTag.CheckOkButton;
var
  s: string;
begin
  ButtonPanel1.OKButton.Enabled := false;
  if chkCreateBranch.Checked then begin
    s := Trim(txtBranchName.Text);
    if s='' then begin
      lblHint.Caption := 'Branch name is empty';
      exit;
    end;
    if PosAny([' ','\','~','^',':','*','[','@'], s)>0 then begin
      lblHint.Caption := 'Invalid character in branch name';
      exit;
    end;
    if fGitMgr.IndexOfLocalBranch(s)>=0 then begin
      lblHint.Caption := 'Branch name already exists';
      exit;
    end;

    lblHint.Caption := format('will do: git checkout -b %s %s',[s, lblTag.Caption]);

  end else begin
    lblHint.Caption := format('will do: git checkout %s'^M'Repo will be left in detached HEAD state',[lblTag.Caption]);
  end;
  ButtonPanel1.OKButton.Enabled := true;
end;

procedure TfrmCheckouTag.SetGitMgr(AValue: TGitMgr);
begin
  if fGitMgr = AValue then Exit;
  fGitMgr := AValue;
end;

end.

