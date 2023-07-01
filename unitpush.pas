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

  Form to push with options
}
unit unitpush;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase,
  Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls, Buttons, ExtCtrls,
  unitcommon, unitgittypes, unitifaces, unitconfig, unitgitmgr, unitremotes,
  unitruncmd;

type

  { TfrmPush }

  TfrmPush = class(TForm, IObserver)
    chkOptions: TCheckGroup;
    comboRemote: TComboBox;
    btnRemotes: TSpeedButton;
    txtURL: TEdit;
    gpoDest: TGroupBox;
    Label1: TLabel;
    lblInfo: TLabel;
    lstBranches: TListBox;
    panBtns: TButtonPanel;
    radUrl: TRadioButton;
    radRemote: TRadioButton;
    procedure btnRemotesClick(Sender: TObject);
    procedure chkOptionsItemClick(Sender: TObject; Index: integer);
    procedure comboRemoteSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstBranchesSelectionChange(Sender: TObject; User: boolean);
    procedure radRemoteClick(Sender: TObject);
    procedure txtURLChange(Sender: TObject);
  private
    fCommand: String;
    fGit: IGit;
    fGitMgr: TGitMgr;
    procedure SetGitMgr(AValue: TGitMgr);
    procedure UpdateInfo;
    procedure LoadBranches;
    procedure ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
    procedure Invalid(msg: string);
    procedure LoadRemotes(forceUpdate: boolean = false);
  public
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
  end;

var
  frmPush: TfrmPush;

implementation

{$R *.lfm}

{ TfrmPush }

procedure TfrmPush.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fConfig.ReadWindow(self, 'pushfrm', SECTION_GEOMETRY);
  if ModalResult=mrOk then begin
    canClose := RunInteractive(fGit.Exe + ' ' + fCommand, fGit.TopLevelDir, 'Push with options', fCommand)<=0;
  end;
end;

procedure TfrmPush.chkOptionsItemClick(Sender: TObject; Index: integer);
begin
  UpdateInfo;
end;

procedure TfrmPush.btnRemotesClick(Sender: TObject);
var
  F: TfrmRemotes;
  curRemote: TCaption;
  i: Integer;
begin
  F := TfrmRemotes.Create(self);
  F.GitMgr := GitMgr;
  F.ReadOnly := false;
  try
    if F.ShowModal=mrOk then begin
      curRemote := comboRemote.Text;
      LoadRemotes(true);
      i := comboRemote.Items.IndexOf(CurRemote);
      comboRemote.ItemIndex := i;
      UpdateInfo;
    end;
  finally
    F.Free;
  end;
end;

procedure TfrmPush.comboRemoteSelect(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TfrmPush.FormCreate(Sender: TObject);
begin
  fConfig.WriteWindow(Self, 'pushfrm', SECTION_GEOMETRY);
end;

procedure TfrmPush.FormDestroy(Sender: TObject);
begin
  GitMgr := nil;
end;

procedure TfrmPush.FormShow(Sender: TObject);
begin
  fGitMgr.UpdateRefList;
  LoadRemotes;
  LoadBranches;
end;

procedure TfrmPush.lstBranchesSelectionChange(Sender: TObject; User: boolean);
var
  info: PRefInfo;
  i, j: Integer;
  origin: string;
begin
  try
    i := lstBranches.ItemIndex;
    if i>=0 then begin
      info := PRefInfo(lstBranches.Items.Objects[i]);
      if info^.upstream<>'' then begin
        j := pos('/', info^.upstream);
        if j>0 then begin
          origin := copy(info^.upstream, 1, j-1);
          j := comboRemote.Items.IndexOf(origin);
          if j>=0 then begin
            comboRemote.ItemIndex := j;
            exit;
          end;
        end;
      end;
      comboRemote.ItemIndex := -1;
    end;
  finally
    UpdateInfo;
  end;

end;

procedure TfrmPush.radRemoteClick(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TfrmPush.txtURLChange(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TfrmPush.SetGitMgr(AValue: TGitMgr);
begin
  if fGitMgr = AValue then Exit;

  if fGitMgr<>nil then
    fGitMgr.RemoveObserver(Self);

  fGitMgr := AValue;

  if fGitMgr<>nil then begin
    fGitMgr.AddObserver(Self);
    fGit := fGitMgr.Git;
  end else
    fGit := nil;
end;

procedure TfrmPush.UpdateInfo;
var
  i: Integer;
  s: string;
begin
  panBtns.OKButton.Enabled := false;
  fCommand := 'push';

  if chkOptions.Checked[0] then fCommand += ' --force';
  if chkOptions.Checked[1] then fCommand += ' --tags';

  if radRemote.Checked then begin
    s := trim(comboRemote.Text);
    if s='' then begin
      Invalid(rsTheTargetRemoteRepositoryIsNotSelected);
      exit;
    end;
  end else begin
    s := trim(txtUrl.Text);
    if s='' then begin
      Invalid(rsInvalidTargetUrl);
      exit;
    end;
  end;
  fCommand += ' ' + s;

  // branch to push
  i := lstBranches.ItemIndex;
  if (i<0) or (i>=lstBranches.Count) then begin
    Invalid(rsABranchIsNotSelected);
    exit;
  end;
  fCommand += ' ' + lstBranches.Items[i];

  lblInfo.Font.Color := clBlack;
  lblInfo.Caption := 'git ' + fCommand;

  panBtns.OKButton.Enabled := true;
end;

procedure TfrmPush.LoadBranches;
var
  refList: TStringList;
  info: PRefInfo;
  i, j: Integer;
begin
  refList := fGitMgr.RefList;
  for i:=0 to refList.Count-1 do begin
    info := PRefInfo(refList.Objects[i]);
    if info^.subType=rostLocal then begin
      j := lstBranches.Items.AddObject(refList[i], TObject(info));
      if info^.head then
        lstBranches.ItemIndex := j;
    end;
  end;
  if lstBranches.ItemIndex>=0 then
    lstBranches.TopIndex := lstBranches.ItemIndex;

  UpdateInfo;
end;

procedure TfrmPush.ObservedChanged(Sender: TObject; what: Integer; data: PtrInt
  );
begin
  case what of
    GITMGR_EVENT_REFLISTCHANGED:
      begin
        if Data>0 then begin
          //DebugLn(fGit.ErrorLog);
          ShowMessage(rsErrorWhileGettingListOfBranches);
          Close;
          exit;
        end;
      end;
  end;
end;

procedure TfrmPush.Invalid(msg: string);
begin
  lblInfo.Font.color := clRed;
  lblInfo.Caption := msg;
end;

procedure TfrmPush.LoadRemotes(forceUpdate: boolean);
var
  i: Integer;
begin
  if forceUpdate or (fGitMgr.Remotes=nil) then
    fGitMgr.UpdateRemotes;
  comboRemote.Clear;
  for i:=0 to Length(fGitMgr.Remotes)-1 do
    comboRemote.Items.Add(fGitMgr.Remotes[i].name);
end;

end.

