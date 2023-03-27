unit unitnewtag;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ButtonPanel;

type

  { TfrmNewTag }

  TfrmNewTag = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkAnnotated: TCheckBox;
    lblHint: TLabel;
    lblInfo: TLabel;
    txtMsg: TEdit;
    lblMsg: TLabel;
    txtName: TEdit;
    Label1: TLabel;
    procedure chkAnnotatedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
  private
    fOid: string;
    procedure CheckOkButton;
    procedure OnTxtNameChanged(Data: PtrInt);
  public
    property Oid: string read fOid write fOid;
  end;

var
  frmNewTag: TfrmNewTag;

implementation

{$R *.lfm}

type
  TSetOfChar = set of char;

function PosAny(chars: TSetOfChar; s:string): Integer;
var
  i: Integer;
begin
  result := 0;
  for i:=1 to Length(s) do
    if s[i] in chars then begin
      result := i;
      break;
    end;
end;

{ TfrmNewTag }

procedure TfrmNewTag.chkAnnotatedClick(Sender: TObject);
begin
  lblMsg.Enabled := chkAnnotated.Checked;
  txtMsg.Enabled := lblMsg.Enabled;
end;

procedure TfrmNewTag.FormShow(Sender: TObject);
begin
  lblInfo.Caption := fOid;
  CheckOkButton;
end;

procedure TfrmNewTag.txtNameChange(Sender: TObject);
begin
  CheckOkButton;
end;

procedure TfrmNewTag.CheckOkButton;
var
  s: string;
begin
  ButtonPanel1.OKButton.Enabled := false;
  s := Trim(txtName.Text);
  if s='' then begin
    lblHint.Caption := 'Tag name is empty';
    exit;
  end;
  if PosAny([' ','\','~','^',':','*','[','@'], s)>0 then begin
    lblHint.Caption := 'Invalid character in tag name';
    exit;
  end;
  ButtonPanel1.OKButton.Enabled := true;
  lblHint.caption := '';
end;

procedure TfrmNewTag.OnTxtNameChanged(Data: PtrInt);
begin
  CheckOkButton;
end;

end.

