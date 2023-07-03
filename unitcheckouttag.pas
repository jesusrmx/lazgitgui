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

  Checkout tag unit
}

unit unitcheckouttag;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  unitgitutils, unitgitmgr, unitcommon;

type

  { TfrmCheckouTag }

  TfrmCheckouTag = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkCreateBranch: TCheckBox;
    lblCaption: TLabel;
    lblTag: TLabel;
    lblHint: TLabel;
    lblBranch: TLabel;
    txtBranchName: TEdit;
    procedure chkCreateBranchClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtBranchNameChange(Sender: TObject);
  private
    fGitMgr: TGitMgr;
    fIsCommit: boolean;
    fTagName: string;
    procedure CheckOkButton;
    procedure SetGitMgr(AValue: TGitMgr);
  public
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property TagName: string read fTagName write fTagName;
    property IsCommit: boolean read fIsCommit write fIsCommit;
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
  if fIsCommit then begin
    Caption := rsCheckOutACommit;
    lblCaption.Caption := rsCommit;
    lblTag.Color := clDefault;
  end;
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
      lblHint.Caption := rsBranchNameIsEmpty;
      exit;
    end;
    if PosAny([' ','\','~','^',':','*','[','@'], s)>0 then begin
      lblHint.Caption := rsInvalidCharacterInBranchName;
      exit;
    end;
    if fGitMgr.IndexOfLocalBranch(s)>=0 then begin
      lblHint.Caption := rsBranchNameAlreadyExists;
      exit;
    end;

    lblHint.Caption := format(rsWillDoGitCheckoutBSS, [s, lblTag.Caption]);

  end else begin
    lblHint.Caption := format(rsWillDoGitCheckoutSDetached, [lblTag.Caption]);
  end;
  ButtonPanel1.OKButton.Enabled := true;
end;

procedure TfrmCheckouTag.SetGitMgr(AValue: TGitMgr);
begin
  if fGitMgr = AValue then Exit;
  fGitMgr := AValue;
end;

end.

