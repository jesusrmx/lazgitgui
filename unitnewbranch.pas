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
  Classes, SysUtils, LCLType, LazLogger,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ButtonPanel, ExtCtrls, unitconfig, unitgittypes, unitifaces, unitprocess, unitgitutils,
  unitgitmgr, unitcommon;

type

  { TfrmNewBranch }

  TfrmNewBranch = class(TForm, IObserver)
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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lstSourceClick(Sender: TObject);
    procedure tabSourceChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
  private
    fBranchName, fReference: string;
    fCommitInfo: String;
    fCommit: string;
    fGitMgr: TGitMgr;
    fGit: IGit;
    fType: Integer;
    fStartIndex: Integer;
    function GetFetch: boolean;
    function GetSwitch: boolean;
    procedure SetCommit(AValue: string);
    procedure SetCommitInfo(AValue: string);
    procedure SetGitMgr(AValue: TGitMgr);
    procedure ShowTabIndex(aIndex: Integer);
    procedure CheckOkButton;
    procedure EvaluateOutcome;
    procedure ShowInfo;
    procedure ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
  public
    function GetBranchCommandOptions: string;
    property BranchName: string read fBranchName;
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property Switch: boolean read GetSwitch;
    property Fetch: boolean read GetFetch;
    property Commit: string read fCommit write SetCommit;
    property CommitInfo: string write SetCommitInfo;
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
  BT_NEWLOCAL_COMMIT    = 4;

  TABINDEX_LOCALBRANCH  = 0;
  TABINDEX_TRACKING     = 1;
  TABINDEX_TAG          = 2;
  TABINDEX_COMMIT       = 3;

{ TfrmNewBranch }

procedure TfrmNewBranch.FormCreate(Sender: TObject);
begin
  fConfig.ReadWindow(Self, 'newbranchform', SECTION_GEOMETRY);
  // it looks like tabcontrol tabs are not translated automatically, do it here
  tabSource.Tabs[0] := rsLocalBranches;
  tabSource.Tabs[1] := rsTrackingBranches;
  tabSource.Tabs[2] := rsTags;
  tabSource.Tabs[3] := rsCommit;
end;

procedure TfrmNewBranch.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_ESCAPE then
    close;
end;

procedure TfrmNewBranch.FormShow(Sender: TObject);
begin
  fGitMgr.UpdateRefList;
  tabSource.TabIndex := fStartIndex;
  ShowTabIndex(fStartIndex);
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
  refList: TStringList;
  i, j: Integer;
  info: PRefInfo;
  ok: boolean;
begin

  chkFetch.Enabled := (aIndex=TABINDEX_TRACKING);

  lstSource.Items.BeginUpdate;
  try

    lstSource.Clear;

    if aIndex=TABINDEX_COMMIT then begin
      lstSource.Items.AddObject(fCommit, nil);
      lstSource.ItemIndex := 0;
    end else begin
      refList := fGitMgr.RefList;
      for i:=0 to refList.Count-1 do begin
        info := PRefInfo(refList.Objects[i]);
        ok := false;
        case aIndex of
          0: ok := info^.subType=rostLocal;// (info^.objType=rotCommit) and (not info^.isTracking);
          1: ok := (info^.subType=rostTracking) and (RightStr(info^.refName, 4)<>'HEAD'); // (info^.objType=rotCommit) and info^.isTracking and (RightStr(info^.refName, 4)<>'HEAD');
          2: ok := (info^.subType=rostTag);
          else exit;
        end;
        if ok then begin
          j := lstSource.Items.AddObject(refList[i], TObject(info));
          if (aIndex=0) and info^.head then
            lstSource.ItemIndex := j;
        end;
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

procedure TfrmNewBranch.SetCommit(AValue: string);
begin
  if fCommit = AValue then Exit;
  fCommit := AValue;
  fStartIndex := TABINDEX_COMMIT;
end;

procedure TfrmNewBranch.SetCommitInfo(AValue: string);
begin
  fCommitInfo := AValue;
end;

procedure TfrmNewBranch.SetGitMgr(AValue: TGitMgr);
begin
  if fGitMgr = AValue then Exit;

  if fGitMgr<>nil then
    fGitMgr.RemoveObserver(Self);

  fGitMgr := AValue;

  if fGitMgr<>nil then begin
    fGitMgr.AddObserver(Self);
    fGit := fGitMgr.Git;
  end;
end;

function TfrmNewBranch.GetFetch: boolean;
begin
  result := chkFetch.Checked;
end;

procedure TfrmNewBranch.CheckOkButton;
begin
  EvaluateOutcome;
  ButtonPanel1.OKButton.Enabled :=
    (fType in [BT_NEWLOCAL_BRANCH, BT_NEWLOCAL_TRACKING,
              BT_NEWLOCAL_TAG, BT_NEWLOCAL_COMMIT]);
end;

procedure TfrmNewBranch.EvaluateOutcome;
var
  aIndex: Integer;
  info: PRefInfo;
  p: SizeInt;
  opts: string;

  procedure AlreadyExisting;
  begin
    lblHint.caption := format(rsBranchSAlredyExists, [fBranchName]);
  end;

begin
  fType := BT_INVALID;

  lblHint.Font.Color := clRed;
  fBranchName := Trim(txtName.Text);
  aIndex := lstSource.ItemIndex;
  if (aIndex<0) or (tabSource.TabIndex=TABINDEX_COMMIT) then begin
    info := nil;
    fReference := '';
  end else begin
    info := PRefInfo(lstSource.Items.Objects[aIndex]);
    fReference := lstSource.Items[aIndex];
  end;

  case tabSource.TabIndex of
    TABINDEX_LOCALBRANCH: // branch based on a existing local branch tab
      begin
        if fBranchName<>'' then begin
          if fGitMgr.IndexOfLocalBranch(fBranchName)>=0 then begin
            AlreadyExisting;
            exit;
          end;
          if aIndex<0 then begin
            lblHint.caption := rsNoReferingBranchSelected;
            exit;
          end;
          fType := BT_NEWLOCAL_BRANCH
        end else begin
          lblHint.Caption := rsBranchNameIsEmpty;
          exit;
        end;
      end;

    TABINDEX_TRACKING: // branch based on a tracking branch tab
      begin
        if aIndex<0 then begin
          lblHint.caption := rsNoReferingBranchSelected;
          exit;
        end;
        if fBranchName='' then begin
          if info=nil then begin
            lblHint.caption := rsNotRefersToATrackingBranch;
            exit;
          end;
          p := pos('/', fReference);
          fBranchName := copy(fReference, p+1, MaxInt);
        end;
        if fGitMgr.IndexOfLocalBranch(fBranchName)>=0 then begin
          AlreadyExisting;
          exit;
        end;
        fType := BT_NEWLOCAL_TRACKING;
      end;

    TABINDEX_TAG:  // branch based on a tag tab
      begin
        if fBranchName<>'' then begin
          if fGitMgr.IndexOfLocalBranch(fBranchName)>=0 then begin
            AlreadyExisting;
            exit;
          end;
          if aIndex<0 then begin
            lblHint.caption := rsNoReferingTagSelected;
            exit;
          end;
          fType := BT_NEWLOCAL_TAG;
        end else begin
          lblHint.Caption := rsBranchNameIsEmpty;
          exit;
        end;
      end;

    TABINDEX_COMMIT:
      begin
        if fBranchName<>'' then begin
          if fCommit='' then begin
            lblHint.Caption := rsNoCommitSet;
            exit;
          end;
          fType := BT_NEWLOCAL_COMMIT;
          fReference := fCommit;
        end else begin
          lblHint.Caption := rsBranchNameIsEmpty;
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

    if info=nil then begin
      txtInfo.Text := fCommitInfo;
      exit;
    end;

    s := info^.refName;
    if info^.upstream<>'' then
      s := s + ' -> ' + info^.upstream;
    txtInfo.Lines.Add(s);
    if info^.worktreepath<>'' then
      txtInfo.Lines.Add(rsWorktreeS, [info^.worktreepath]);

    txtInfo.Lines.Add('');
    if (info^.objType=rotTag) and (info^.refered<>nil) then begin
      txtInfo.Lines.Add(rsTagS2, [info^.objName]);
      txtInfo.Lines.Add('%s (%s)',[info^.authorName, DateTimeToGitFmt(info^.authorDate)]);
      txtInfo.Lines.Add(info^.subject);
      txtInfo.Lines.Add('');
      info := info^.refered;
    end;
    txtInfo.Lines.Add(rsCommitS, [info^.objName]);
    txtInfo.Lines.Add('%s (%s)',[info^.authorName, DateTimeToGitFmt(info^.authorDate)]);
    txtInfo.Lines.Add(info^.subject);
  finally
    txtInfo.Lines.EndUpdate;
  end;

end;

procedure TfrmNewBranch.ObservedChanged(Sender: TObject; what: Integer;
  data: PtrInt);
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

function TfrmNewBranch.GetBranchCommandOptions: string;
begin
  if fType = BT_INVALID then begin
    result := rsInvalid;
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
  if fGitMgr<>nil then
    fGitMgr.RemoveObserver(Self);
end;

end.

