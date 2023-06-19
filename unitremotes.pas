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

  Remotes manager.
}
unit unitremotes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Types, LazLoggerBase,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel, Buttons,
  unitcommon, unitifaces, unitgittypes, unitconfig, unitgitmgr, unitruncmd;

type

  TRemotesEditAction = (reaNoChange, reaNew, reaDel, reaChange);
  TRemotesEditItem = record
    name, fetch, push: string;
    orgName, orgFetch, orgPush: string;
    action, oldAction: TRemotesEditAction;
    fetchRemote: boolean;
    done: boolean;
    importTags: boolean;
    onlyBranches: boolean;
    branches: string;
  end;

  TRemotesEditArray = array of TRemotesEditItem;

  { TfrmRemotes }

  TfrmRemotes = class(TForm)
    chkOnlyBranches: TCheckBox;
    chkImportTags: TCheckBox;
    chkFetch: TCheckBox;
    chkSameURL: TCheckBox;
    btnAdd: TSpeedButton;
    btnDel: TSpeedButton;
    txtBranches: TEdit;
    txtName: TEdit;
    lblInfo: TLabel;
    txtFetch: TEdit;
    txtPush: TEdit;
    panBtns: TButtonPanel;
    lstRemotes: TListBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure chkFetchClick(Sender: TObject);
    procedure chkImportTagsClick(Sender: TObject);
    procedure chkOnlyBranchesClick(Sender: TObject);
    procedure chkSameURLClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstRemotesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lstRemotesSelectionChange(Sender: TObject; User: boolean);
    procedure txtBranchesChange(Sender: TObject);
    procedure txtFetchChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtPushChange(Sender: TObject);
  private
    fGitMgr: TGitMgr;
    fReadOnly: boolean;
    fRemotes: TRemotesEditArray;
    function ExecuteActions: boolean;
    function GetIndex: Integer;
    procedure SetGitMgr(AValue: TGitMgr);
    procedure SetReadOnly(AValue: boolean);
    procedure UpdateRemotesList(updItemIndex:Boolean=true);
    procedure UpdateInfo;
    procedure UpdateControls;
    procedure Invalid(msg: string);
    procedure Changed;
    function GetNewCmd(aIndex: Integer): string;
  protected
    property CurrentIndex: Integer read GetIndex;
  public
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property ReadOnly: boolean read fReadOnly write SetReadOnly;
    property Remotes: TRemotesEditArray read fRemotes;
  end;

var
  frmRemotes: TfrmRemotes;

implementation

{$R *.lfm}

{
  NOTE. although the gui is designed for changing both the fetch and the push
        urls, there is a not in the git remote documentation stating that
        both urls must be always equal.

        see: https://git-scm.com/docs/git-remote set-url section.
}

{ TfrmRemotes }

procedure TfrmRemotes.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fConfig.ReadWindow(self, 'remotesfrm', SECTION_GEOMETRY);
  if ModalResult=mrOk then
    CanClose := ExecuteActions;
end;

procedure TfrmRemotes.chkSameURLClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmRemotes.btnAddClick(Sender: TObject);
var
  j: SizeInt;
begin
  j := CurrentIndex;
  if (j>=0) and (fRemotes[j].action=reaDel) then begin
    // simply undo deletion. TODO: what was the previous state?
    fRemotes[j].action := fRemotes[j].oldAction;
    lstRemotes.Invalidate;
    UpdateControls;
    exit;
  end;

  j := Length(fRemotes);
  SetLength(fRemotes, j+1);
  fRemotes[j].name := rsNewRemoteName;
  fRemotes[j].action := reaNew;
  fRemotes[j].fetchRemote := true;
  fRemotes[j].done := false;
  fRemotes[j].importTags := false;
  fRemotes[j].onlyBranches := false;

  UpdateRemotesList(false);
  lstRemotes.ItemIndex := j;

  ActiveControl := txtName;
  txtName.SelectAll;
end;

procedure TfrmRemotes.btnDelClick(Sender: TObject);
var
  i: Integer;
begin
  i := CurrentIndex;
  if fRemotes[i].action=reaNew then begin
    Delete(fRemotes, i, 1);
    UpdateRemotesList;
  end else begin
    fRemotes[i].oldAction := fRemotes[i].action;
    fRemotes[i].action := reaDel;
    fRemotes[i].done := false;
  end;
  lstRemotes.Invalidate;
  UpdateControls;
  UpdateInfo;
end;

procedure TfrmRemotes.chkFetchClick(Sender: TObject);
var
  i: Integer;
begin
  i := CurrentIndex;
  if i>=0 then
    fRemotes[i].fetchRemote := chkFetch.Checked;
  UpdateInfo;
end;

procedure TfrmRemotes.chkImportTagsClick(Sender: TObject);
var
  i: Integer;
begin
  i := CurrentIndex;
  if i>=0 then
    fRemotes[i].importTags := chkImportTags.Checked;
  UpdateInfo;
end;

procedure TfrmRemotes.chkOnlyBranchesClick(Sender: TObject);
var
  i: Integer;
begin
  i := CurrentIndex;
  if i>=0 then
    fRemotes[i].onlyBranches := chkOnlyBranches.Checked;
  UpdateControls;
  UpdateInfo;
end;

procedure TfrmRemotes.FormCreate(Sender: TObject);
begin
  fConfig.ReadWindow(Self, 'remotesfrm', SECTION_GEOMETRY);
  ReadOnly := true;
end;

procedure TfrmRemotes.FormShow(Sender: TObject);
begin
  UpdateRemotesList;
end;

procedure TfrmRemotes.lstRemotesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  lb: TListBox;
  aCanvas: TCanvas;
  isFocused, sel: Boolean;
  aColor: TColor;
begin
  if index<0 then
    exit;

  lb := TListBox(Control);

  isFocused := lb.Focused;
  sel := (index=lb.ItemIndex);

  aCanvas := lb.Canvas;
  if isFocused then
    if sel then aColor := clHighlight
    else        aColor := lb.Color
  else begin
    if sel then aColor := clForm
    else        aColor := lb.Color;
  end;

  aCanvas.Brush.Color := aColor;
  aCanvas.FillRect(aRect);

  if fRemotes[index].done then begin
    if fRemotes[index].action=reaDel then aColor := clTeal
    else                                  aColor := clGreen;
  end else
  if fRemotes[index].action=reaDel then
    aColor := clGrayText
  else begin
    if isFocused then
      if sel then aColor := clHighlightText
      else        aColor := clBlack
    else          aColor := clBlack;
  end;

  lb.Canvas.Font.Color := aColor;
  lb.Canvas.Brush.Style := bsClear;
  lb.Canvas.TextRect(aRect, aRect.Left + 22, aRect.Top, lb.Items[index]);
end;

procedure TfrmRemotes.lstRemotesSelectionChange(Sender: TObject; User: boolean);
var
  i: Integer;
begin
  i := CurrentIndex;
  if i>=0 then begin
    with fRemotes[i] do begin
      txtName.Text := name;
      txtFetch.Text := fetch;
      txtPush.Text := push;
      chkSameURL.Checked := true {fetch=push};
      btnDel.Enabled := not ReadOnly and not (action=reaDel);
      chkFetch.Checked := fetchRemote;
    end;
    UpdateControls;
  end else begin
    btnDel.Enabled := false;
  end;

  UpdateInfo;
end;

procedure TfrmRemotes.txtBranchesChange(Sender: TObject);
var
  i: Integer;
begin
  i := CurrentIndex;
  if i>=0 then begin
    fRemotes[i].Branches := txtBranches.Text;
    UpdateInfo;
  end;
end;

procedure TfrmRemotes.txtFetchChange(Sender: TObject);
var
  i: Integer;
begin
  i := CurrentIndex;
  if i>=0 then begin
    fRemotes[i].fetch := txtFetch.Text;
    if chkSameURL.Checked then begin
      txtPush.Text := fRemotes[i].fetch;
      fRemotes[i].Push := fRemotes[i].fetch;
    end;
    Changed;
  end;
end;

procedure TfrmRemotes.txtNameChange(Sender: TObject);
var
  i: Integer;
begin
  i := CurrentIndex;
  if i>=0 then begin
    fRemotes[i].name := txtName.Text;
    lstRemotes.Items[i] := fRemotes[i].name;
    Changed;
  end;
end;

procedure TfrmRemotes.txtPushChange(Sender: TObject);
var
  i: Integer;
begin
  i := CurrentIndex;
  if i>=0 then begin
    fRemotes[i].push := txtPush.Text;
    Changed;
  end;
end;

procedure TfrmRemotes.SetGitMgr(AValue: TGitMgr);
var
  i: Integer;
begin
  if fGitMgr = AValue then Exit;

  fGitMgr := AValue;

  if fGitMgr<>nil then begin
    SetLength(fRemotes, Length(fGitMgr.Remotes));
    for i:=0 to Length(fRemotes)-1 do begin
      fRemotes[i].name := fGitMgr.Remotes[i].name;
      fRemotes[i].fetch := fGitMgr.Remotes[i].fetch;
      fRemotes[i].push := fGitMgr.Remotes[i].push;
      fRemotes[i].orgName  := fRemotes[i].name;
      fRemotes[i].orgFetch := fRemotes[i].fetch;
      fRemotes[i].orgPush  := fRemotes[i].push;
      fRemotes[i].action := reaNoChange;
      fRemotes[i].done := false;
    end;
  end else
    fRemotes := nil;

  UpdateRemotesList;
end;

function TfrmRemotes.GetIndex: Integer;
begin
  result := lstRemotes.ItemIndex;
end;

function TfrmRemotes.ExecuteActions: boolean;
var
  i: Integer;
  fGit: IGit;
  cmd: string;
begin
  result := false;
  fGit := fGitMgr.Git;
  for i := 0 to Length(fRemotes) - 1 do
  with fRemotes[i] do
    if not done then begin
      case action of
        reaNew:
          begin
            cmd := GetNewCmd(i);
            done := RunInteractive(fGit.Exe + cmd, fGit.TopLevelDir, 'Adding a new remote', cmd)<=0;
            if not done then
              exit;
          end;
        reaDel:
          begin
            cmd := ' remote remove ' + name;
            done := RunInteractive(fGit.Exe + cmd, fGit.TopLevelDir, 'Removing remote', cmd)<=0;
            if not done then
              exit;
          end;
        reaChange:
           begin
             if orgName<>name then begin
               // the remote name has changed....
               cmd := ' remote rename ' + orgName + ' ' + Name;
               done := RunInteractive(fGit.Exe + cmd, fGit.TopLevelDir, 'Renaming a remote', cmd)<=0;
               if not done then
                 exit;
             end;
             if orgFetch<>fetch then begin
               // the fetch url has changed ...
               cmd := ' remote set-url ' + name + ' ' + fetch;
               done := RunInteractive(fGit.Exe + cmd, fGit.TopLevelDir, 'Renaming a remote', cmd)<=0;
               if not done then
                 exit;
             end;
           end;
       end;
    end;
  result := true;
end;

procedure TfrmRemotes.SetReadOnly(AValue: boolean);
begin
  if fReadOnly = AValue then Exit;
  fReadOnly := AValue;
  UpdateControls;
end;

procedure TfrmRemotes.UpdateRemotesList(updItemIndex: Boolean);
var
  i: Integer;
begin
  lstRemotes.Clear;

  for i:=0 to Length(fRemotes)-1 do
    lstRemotes.AddItem(fRemotes[i].name, nil);

  if updItemIndex then begin
    if lstRemotes.Count>0 then
      lstRemotes.ItemIndex := 0;
  end;
end;

procedure TfrmRemotes.UpdateInfo;
var
  s: string;
  i: Integer;
  arr: TStringArray;
begin
  panBtns.OkButton.Enabled := false;

  for i:=0 to Length(fRemotes)-1 do
  with fRemotes[i] do
    if action in [reaNew, reaChange] then begin
      if (name='') or (name=rsNewRemoteName) then begin
        Invalid(format(rsInvalidRemoteName, [i+1]));
        exit;
      end;
      if (Fetch='') then begin
        Invalid(format(rsInvalidFetchURL, [QuotedStr(name)]));
        exit;
      end;
      if (Push='') then begin
        Invalid(format(rsInvalidPushURL, [QuotedStr(name)]));
        exit;
      end;
      if onlyBranches then begin
        if (branches='') then begin
          Invalid(rsTheBranchesListIsEmpty);
          exit;
        end;
        arr := branches.Split([',']);
        for s in arr do begin
          if (s='') or (pos(' ', s)>0) then begin
            Invalid(format(rsSIsAnInvalidBranchName, [QuotedStr(s)]));
            exit;
          end;
        end;
      end;
    end;

  lblInfo.Caption := '';
  lblInfo.Font.Color := clBlack;

  i := CurrentIndex;
  if (i>=0) then
    with fRemotes[i] do
      case action of
        reaNew: lblInfo.Caption := 'git' + GetNewCmd(i);
        reaDel: lblInfo.Caption := 'git remote remove ' + name;
        reaChange:
          begin
            s := '';
            if orgName<>name then
              s+= 'git remote rename ' + orgName + ' ' + Name;
            if orgFetch<>fetch then begin
              if s<>'' then s += ' | ';
              s += 'git remote set-url ' + name + ' ' + fetch;
            end;
            lblInfo.Caption := s;
          end;
      end;

  panBtns.OkButton.Enabled := true;
end;

procedure TfrmRemotes.UpdateControls;
var
  i: Integer;
  isInvalid: Boolean;
begin
  i := CurrentIndex;
  isInvalid := (i<0) or (fRemotes[i].action=reaDel) or (fRemotes[i].done);

  btnAdd.Visible := not fReadOnly;
  btnDel.Visible := not fReadOnly;

  txtName.ReadOnly := fReadonly;
  txtName.Enabled := not isInvalid;
  txtFetch.Enabled := not fReadOnly and (i>=0) and not isInvalid;
  txtPush.Enabled := not fReadOnly and (i>=0) and (not chkSameURL.Checked) and not IsInvalid;
  chkSameURL.Enabled := false {not fReadOnly and (i>=0) and not IsInvalid};
  btnAdd.Enabled := (i>=0);
  btnDel.Enabled := (i>=0) and not isInvalid;

  chkFetch.Enabled := (i>=0) and (not isInvalid) and (fRemotes[i].action=reaNew);
  chkImportTags.Enabled := chkFetch.Enabled;
  chkOnlyBranches.Enabled := chkFetch.Enabled;
  txtBranches.Enabled := chkFetch.Enabled and chkOnlyBranches.Checked;
end;

procedure TfrmRemotes.Invalid(msg: string);
begin
  lblInfo.Font.Color := clRed;
  lblInfo.Caption := msg;
end;

procedure TfrmRemotes.Changed;
var
  i: Integer;
begin
  i := CurrentIndex;
  if fRemotes[i].action=reaNoChange then
    fRemotes[i].action := reaChange;
  UpdateInfo;
end;

function TfrmRemotes.GetNewCmd(aIndex: Integer): string;
var
  arr: TStringArray;
  branch: String;
begin
  with fRemotes[aIndex] do begin
    result := ' remote add';
    if onlyBranches then begin
      arr := branches.Split([',']);
      for branch in arr do
        result += ' -t ' + Trim(branch);
    end;
    if fetchRemote then result += ' -f';
    if importTags then
      result += ' --tags';

    result += ' ' + name + ' ' + fetch;
  end;
end;

end.

