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

  Form to type a git command for direct run.
}
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

