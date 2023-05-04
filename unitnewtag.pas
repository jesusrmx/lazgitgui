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

  New tag unit
}
unit unitnewtag;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ButtonPanel, unitgitutils;

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

