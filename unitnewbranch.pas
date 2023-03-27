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

  New branch unit
}
unit unitnewbranch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ButtonPanel, ExtCtrls, unitconfig, unitprocess, unitgit;

type

  { TfrmNewBranch }

  TfrmNewBranch = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkFetch: TCheckBox;
    chkSwitchTo: TCheckBox;
    txtCustomOptions: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblHint: TLabel;
    txtInfo: TMemo;
    Panel1: TPanel;
    tabSource: TTabControl;
    txtName: TEdit;
    lstSource: TListBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstSourceClick(Sender: TObject);
    procedure tabSourceChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
  private
    fBranchName, fReference: string;
    fGit: TGit;
    fRefs: TStringList;
    fType: Integer;
    function GetFetch: boolean;
    function GetSwitch: boolean;
    procedure ShowTabIndex(aIndex: Integer);
    procedure CheckOkButton;
    function AlreadyExists(aName: string; subType: TRefObjectSubType): boolean;
    procedure EvaluateOutcome;
    procedure ShowInfo;
  public
    function GetBranchCommandOptions: string;
    property BranchName: string read fBranchName;
    property Git: TGit read fGit write fGit;
    property Switch: boolean read GetSwitch;
    property Fetch: boolean read GetFetch;
  end;

var
  frmNewBranch: TfrmNewBranch;

implementation

{$R *.lfm}

const
  BF_REFNAME  = 0;
  BF_OBJNAME  = 1;
  BF_UPSTREAM = 2;
  BF_CURRENT  = 3;
  BF_WORKTREE = 4;
  BF_SUBJECT  = 5;

  BT_INVALID            = 0;
  BT_NEWLOCAL_BRANCH    = 1;
  BT_NEWLOCAL_TRACKING  = 2;
  BT_NEWLOCAL_TAG       = 3;

{ TfrmNewBranch }

procedure TfrmNewBranch.FormCreate(Sender: TObject);
begin
  fConfig.ReadWindow(Self, 'newbranchform', SECTION_GEOMETRY);
  fRefs := TStringList.Create;
end;

procedure TfrmNewBranch.FormDestroy(Sender: TObject);
begin
  ClearRefList(fRefs);
  fRefs.Free;
end;

procedure TfrmNewBranch.FormShow(Sender: TObject);
begin

  if fGit.RefList(fRefs, '', [
      '%(refname:short)',
      '%(refname:rstrip=-2)',
      '%(objecttype)',
      '%(objectname)',
      '%(upstream:short)',
      '%(HEAD)',
      '%(worktreepath)',
      '%(contents)',
      '%(authorname)',
      '%(authordate)',
      '%(committerdate)',
      '%(creatordate)',
      '%(*objecttype)',
      '%(*objectname)',
      '%(*authorname)',
      '%(*authordate)',
      '%(*contents)'
      ])>0
  then begin
    DebugLn(fGit.ErrorLog);
    ShowMessage('Error while getting list of branches');
    Close;
    exit;
  end;

  ShowTabIndex(0);
end;

procedure TfrmNewBranch.lstSourceClick(Sender: TObject);
begin
  ShowInfo;
  CheckOkButton;
end;

procedure TfrmNewBranch.tabSourceChange(Sender: TObject);
begin
  ShowTabIndex(tabSource.TabIndex)
end;

procedure TfrmNewBranch.txtNameChange(Sender: TObject);
begin
  CheckOkButton;
end;

procedure TfrmNewBranch.ShowTabIndex(aIndex: Integer);
var
  branchLine: TStringList;
  i, j: Integer;
  info: PRefInfo;
  ok: boolean;
begin

  chkFetch.Enabled := (aIndex=1);

  lstSource.Items.BeginUpdate;
  try

    lstSource.Clear;

    for i:=0 to fRefs.Count-1 do begin
      info := PRefInfo(fRefs.Objects[i]);
      ok := false;
      case aIndex of
        0: ok := info^.subType=rostLocal;// (info^.objType=rotCommit) and (not info^.isTracking);
        1: ok := (info^.subType=rostTracking) and (RightStr(info^.refName, 4)<>'HEAD'); // (info^.objType=rotCommit) and info^.isTracking and (RightStr(info^.refName, 4)<>'HEAD');
        2: ok := (info^.subType=rostTag);
        else exit;
      end;
      if ok then begin
        j := lstSource.Items.AddObject(fRefs[i], TObject(info));
        if (aIndex=0) and info^.head then
          lstSource.ItemIndex := j;
      end;
    end;

    ShowInfo;
    CheckOkButton;

  finally
    lstSource.Items.EndUpdate;
  end;
end;

function TfrmNewBranch.GetSwitch: boolean;
begin
  result := chkSwitchTo.Checked;
end;

function TfrmNewBranch.GetFetch: boolean;
begin
  result := chkFetch.Checked;
end;

procedure TfrmNewBranch.CheckOkButton;
begin
  EvaluateOutcome;
  ButtonPanel1.OKButton.Enabled :=
    (fType=BT_NEWLOCAL_BRANCH) or
    (fType=BT_NEWLOCAL_TRACKING) or
    (fType=BT_NEWLOCAL_TAG);
end;

function TfrmNewBranch.AlreadyExists(aName: string; subType: TRefObjectSubType
  ): boolean;
var
  i: Integer;
  info: PRefInfo;
begin
  result := false;
  aName := lowercase(aName);
  for i:=0 to fRefs.Count-1 do begin
    info := PRefInfo(fRefs.Objects[i]);
    if (info^.subType=subtype) and (aName=info^.refName) then begin
      result := true;
      break;
    end;
  end;
end;

procedure TfrmNewBranch.EvaluateOutcome;
var
  aIndex: Integer;
  info: PRefInfo;
  p: SizeInt;
  opts: string;

  procedure AlreadyExisting;
  begin
    lblHint.caption := format('Branch ''%s'' alredy exists',[fBranchName]);
  end;

begin
  fType := BT_INVALID;

  lblHint.Font.Color := clRed;
  fBranchName := Trim(txtName.Text);
  aIndex := lstSource.ItemIndex;
  if aIndex<0 then begin
    info := nil;
    fReference := '';
  end else begin
    info := PRefInfo(lstSource.Items.Objects[aIndex]);
    fReference := lstSource.Items[aIndex];
  end;

  case tabSource.TabIndex of
    0: // branch based on a existing local branch tab
      begin
        if fBranchName<>'' then begin
          if AlreadyExists(fBranchName, rostLocal) then begin
            AlreadyExisting;
            exit;
          end;
          if aIndex<0 then begin
            lblHint.caption := 'No refering branch selected';
            exit;
          end;
          fType := BT_NEWLOCAL_BRANCH
        end else begin
          lblHint.Caption := 'Branch name is empty';
          exit;
        end;
      end;

    1: // branch based on a tracking branch tab
      begin
        if aIndex<0 then begin
          lblHint.caption := 'No refering branch selected';
          exit;
        end;
        if fBranchName='' then begin
          if info=nil then begin
            lblHint.caption := 'Not refers to a tracking branch';
            exit;
          end;
          p := pos('/', fReference);
          fBranchName := copy(fReference, p+1, MaxInt);
        end;
        if AlreadyExists(fBranchName, rostLocal) then begin
          AlreadyExisting;
          exit;
        end;
        fType := BT_NEWLOCAL_TRACKING;
      end;

    2:  // branch based on a tag tab
      begin
        if fBranchName<>'' then begin
          if AlreadyExists(fBranchName, rostLocal) then begin
            AlreadyExisting;
            exit;
          end;
          if aIndex<0 then begin
            lblHint.caption := 'No refering tag selected';
            exit;
          end;
          fType := BT_NEWLOCAL_TAG;
        end else begin
          lblHint.Caption := 'Branch name is empty';
          exit;
        end;
      end;
  end;

  lblHint.Font.Color := clGreen;
  opts := GetBranchCommandOptions;
  lblHint.Caption := format('git branch %s', [opts]);
end;

procedure TfrmNewBranch.ShowInfo;
var
  aIndex: Integer;
  info: PRefInfo;
  s: string;
begin

  txtInfo.Lines.BeginUpdate;
  try
    txtInfo.Clear;
    aIndex := lstSource.ItemIndex;
    if aIndex<0 then
      exit;
    info := PRefInfo(lstSource.Items.Objects[aIndex]);

    s := info^.refName;
    if info^.upstream<>'' then
      s := s + ' -> ' + info^.upstream;
    txtInfo.Lines.Add(s);
    if info^.worktreepath<>'' then
      txtInfo.Lines.Add('worktree: '+info^.worktreepath);

    txtInfo.Lines.Add('');
    if (info^.objType=rotTag) and (info^.refered<>nil) then begin
      txtInfo.Lines.Add('Tag: %s',[info^.objName]);
      txtInfo.Lines.Add('%s (%s)',[info^.authorName, DateTimeToGitFmt(info^.authorDate)]);
      txtInfo.Lines.Add(info^.subject);
      txtInfo.Lines.Add('');
      info := info^.refered;
    end;
    txtInfo.Lines.Add('Commit: %s',[info^.objName]);
    txtInfo.Lines.Add('%s (%s)',[info^.authorName, DateTimeToGitFmt(info^.authorDate)]);
    txtInfo.Lines.Add(info^.subject);
  finally
    txtInfo.Lines.EndUpdate;
  end;

end;

function TfrmNewBranch.GetBranchCommandOptions: string;
begin
  if fType = BT_INVALID then begin
    result := 'Invalid';
    exit;
  end;

  result := txtCustomOptions.Text;
  if result<>'' then begin
    result += ' ';
  end else begin
    if fType=BT_NEWLOCAL_TRACKING then
      result += '-t ';
  end;

  result += Sanitize(fBranchName, false) + ' ' + Sanitize(fReference, false);
end;

procedure TfrmNewBranch.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fConfig.WriteWindow(Self, 'newbranchform', SECTION_GEOMETRY);
end;

end.

