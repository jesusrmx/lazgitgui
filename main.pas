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

  Main unit
}
unit main;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, {$ifdef linux}gtk2, gdk2,{$endif}
  LazLogger, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList, SynEdit, StrUtils, FileUtil, Clipbrd,
  lclType, Menus, Buttons, ComCtrls, Types,
  unitgittypes, unitifaces, unitconfig, unitprocess, unithighlighterhelper,
  unitentries, unitgitutils, unitcommon, unitdebug,
  unitnewbranch, unitruncmd, unitsyneditextras,
  unitnewtag, LConvEncoding, unitdbindex,
  unitgitmgr, unitcheckouttag, unitformlog, unitcustomcmds,
  unitcustcmdform, unittextchunks, unitgitcmd, unitpush, unitclone;

type

  { TfrmMain }

  TfrmMain = class(TForm, IObserver)
    actCommit: TAction;
    actFetch: TAction;
    actInsertBranchName: TAction;
    actAddCmd: TAction;
    actGitCmd: TAction;
    actClone: TAction;
    actRepoInfo: TAction;
    actRestoreCommitMsg: TAction;
    actNewLog: TAction;
    actPushDialog: TAction;
    actQuit: TAction;
    actPull: TAction;
    actPush: TAction;
    actRescan: TAction;
    ActionList1: TActionList;
    btnNewLog: TSpeedButton;
    btnRescan: TButton;
    btnStageChanged: TButton;
    btnSignOff: TButton;
    btnCommit: TButton;
    btnPush: TButton;
    btnPushDlg: TButton;
    imgList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblTag: TLabel;
    lblMerging: TLabel;
    lblRemote: TLabel;
    lblAheadBehind: TLabel;
    lblBranch: TLabel;
    lstUnstaged: TListBox;
    lstStaged: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuMain: TMainMenu;
    panCommitState: TPanel;
    panBranch: TPanel;
    panPush: TPanel;
    panStatus: TPanel;
    panFileState: TPanel;
    panUnstaged: TPanel;
    panStagedContainer: TPanel;
    panStaged: TPanel;
    popBranch: TPopupMenu;
    popLists: TPopupMenu;
    popCommands: TPopupMenu;
    prgBar: TProgressBar;
    btnRepoInfo: TSpeedButton;
    dlgSave: TSaveDialog;
    Separator1: TMenuItem;
    splitterMain: TSplitter;
    barCustomCmds: TToolBar;
    btnGitCmd: TToolButton;
    btnAddCustomCmd: TToolButton;
    txtComment: TMemo;
    panLeft: TPanel;
    panContent: TPanel;
    panCommit: TPanel;
    panCommitButtons: TPanel;
    splitterStaged: TSplitter;
    splitterCommit: TSplitter;
    txtDiff: TSynEdit;
    procedure actAddCmdExecute(Sender: TObject);
    procedure actCloneExecute(Sender: TObject);
    procedure actCommitExecute(Sender: TObject);
    procedure actFetchExecute(Sender: TObject);
    procedure actGitCmdExecute(Sender: TObject);
    procedure actInsertBranchNameExecute(Sender: TObject);
    procedure actNewLogExecute(Sender: TObject);
    procedure actPullExecute(Sender: TObject);
    procedure actPushDialogExecute(Sender: TObject);
    procedure actPushExecute(Sender: TObject);
    procedure actQuitExecute(Sender: TObject);
    procedure actRepoInfoExecute(Sender: TObject);
    procedure actRescanExecute(Sender: TObject);
    procedure actRestoreCommitMsgExecute(Sender: TObject);
    procedure btnGitCmdArrowClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lblBranchClick(Sender: TObject);
    procedure lblBranchContextPopup(Sender: TObject; {%H-}MousePos: TPoint;
      var {%H-}Handled: Boolean);
    procedure lblInfoOldClick(Sender: TObject);
    procedure lstUnstagedContextPopup(Sender: TObject; MousePos: TPoint;
      var {%H-}Handled: Boolean);
    procedure lstUnstagedDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lstUnstagedMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
  private
    fGitMgr: TGitMgr;
    fGit: IGit;
    fClickedIndex: Integer;
    fDir: string;
    fPopPoint: TPoint;
    fListAlwaysDrawSelection: boolean;
    fhlHelper: THighlighterHelper;
    fCustomCommands: TCustomCommandsMgr;
    procedure CreateBranch(const binfo: PBranchInfo);
    function  CreatePatchFromListbox(const lb: TListbox): TStringList;
    procedure DelayedShowMenu({%H-}Data: PtrInt);
    procedure DoGitDiff(Data: PtrInt);
    procedure DoItemAction(Data: PtrInt);
    procedure DoCommit;
    procedure DoClone;
    procedure DoNewLog;
    procedure DoPush;
    procedure DoFetch;
    procedure DoPull;
    procedure DoRepoInfo;
    procedure OnCreatePatchClick(Sender: TObject);
    procedure OnCopyPatchClick(Sender: TObject);
    procedure OnCustomCommandClick(Sender: TObject);
    procedure OnDebugLoggerInterceptor(Sender: TObject; S: string;
      var Handled: Boolean);
    procedure OnMRECommand(Sender: TObject);
    procedure OnPopupItemClick(Sender: TObject);
    procedure OnBranchSwitch(Data: PtrInt);
    procedure OnIgnoreFileClick(Sender: TObject);
    procedure OnIgnoreTypeClick(Sender: TObject);
    procedure OnReloadBranchMenu({%H-}Data: PtrInt);
    procedure OnRepoInfoStatusDone(Sender: TObject);
    procedure OnRestoreFileClick(Sender: TObject);
    procedure OnDeleteFilesClick(Sender: TObject);
    procedure OnStageAllClick(Sender: TObject);
    procedure OnStageItemClick(Sender: TObject);
    procedure OnUnstageItemClick(Sender: TObject);
    function  OpenDirectory(aDir: string): boolean;
    procedure UpdateBranch;
    procedure RestoreGui;
    procedure SaveGui;
    procedure ItemAction(sender: TListbox; aIndex: Integer);
    procedure UpdateBranchMenu;
    procedure ShowError;
    procedure ViewFile(filename: string);
    procedure ComingSoon;
    function MakeMenuItemUnstagedEntryArray(mi: TMenuItem): TPFileEntryArray;
    function MakeMenuItemStagedEntryArray(mi: TMenuItem): TPFileEntryArray;
    procedure NewBranch;
    procedure InvalidateBranchMenu;
    procedure NewTag;
    procedure CheckMenuDivisorInLastPosition(pop:TPopupMenu);
    procedure ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
    procedure ShowNewTagForm(commit: string);
    procedure ShowSwitchToTagForm(aTag: string);
    procedure ShowSwitchToCommitForm(aCommit: string);
    procedure SwitchTo(cmd: string);
    procedure SaveCommitMessage;
    procedure RestoreCommitMessage;
    procedure UpdateCommandsBar;
  public

  end;

var
  frmMain: TfrmMain;
  targetDir: string = '';

implementation

{$R *.lfm}

const
  MENU_INVALID                = -1;

  MENU_BRANCH_NEW             = 1;
  MENU_BRANCH_RELOAD          = 2;
  MENU_BRANCH_SWITCH          = 3;
  MENU_BRANCH_NEW_TAG         = 4;

  MENU_LIST_VIEW_UNTRACKED    = 14;
  MENU_LIST_VIEW_IGNORED      = 15;
  MENU_LIST_VIEW_TRACKED      = 16;
  MENU_LIST_STAGE_CHANGED     = 17;
  MENU_LIST_STAGE_ALL         = 18;
  MENU_LIST_UNSTAGE_ALL       = 19;

  LIST_TAG_ALL_SELECTED       = -1;
  LIST_TAG_ALL_CHANGED        = -2;

  BIN_BUFSIZE                 = 1024;


function AddPopItem(pop: TPopupMenu; caption:string; onClick:TNotifyEvent; tag: Ptrint): TMenuItem;
begin
  if caption='-' then begin
    if pop.Items[pop.Items.Count-1].Caption='-' then
      exit;
  end;
  result := TMenuItem.Create(pop.Owner);
  result.Caption := caption;
  result.OnClick := onClick;
  result.Tag := tag;
  pop.Items.Add(result);
end;

function GetSelectedIndex(lb: TListbox): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=0 to lb.Count-1 do
    if lb.Selected[i] then begin
      result := i;
      break;
    end;
end;

function CountListItemsOfEntryType(lb: TListbox; unstaged, onlySel:boolean; entrySet: TSetOfEntryType): Integer;
var
  Entry: PFileEntry;
  i: Integer;
begin
  result := 0;
  for i:=0 to lb.Count-1 do
    if (not OnlySel) or lb.Selected[i] then begin
      Entry := PFileEntry(lb.Items.Objects[i]);
      if (Entry<>nil) then begin
        if (unstaged and (Entry^.EntryTypeUnStaged in entrySet)) or
           (not unstaged and (Entry^.EntryTypeStaged in entrySet)) then
        inc(result);
      end;
    end;
end;

function AreAllSelectedItemsOfEntryType(lb: TListbox; unstaged:boolean; entrySet: TSetOfEntryType): boolean;
var
  n: Integer;
begin
  n := CountListItemsOfEntryType(lb, unstaged, true, entrySet);
  result := (n>0) and (n=lb.SelCount);
end;

function FirstSelectedItemExcluding(lb: TListBox; unstaged:boolean; entryset: TSetOfEntryType): Integer;
var
  i: Integer;
  Entry: PFileEntry;
  inSet: Boolean;
begin
  result := -1;
  for i:=0 to lb.Count-1 do
    if lb.Selected[i] then begin
      Entry := PFileEntry(lb.Items.Objects[i]);
      if (Entry<>nil) then begin
        inSet := (unstaged and (Entry^.EntryTypeUnStaged in entrySet)) or
               (not unstaged and (Entry^.EntryTypeStaged in entrySet));
        if not inSet then begin
          result := i;
          break;
        end;
      end;
    end;
end;

procedure ChangeItemSelectionOfEntryType(lb: TListbox; unstaged:boolean; unselect:boolean; entryset: TSetOfEntryType);
var
  i: Integer;
  Entry: PFileEntry;
  inSet: boolean;
begin
  for i:=0 to lb.Count-1 do begin
    Entry := PFileEntry(lb.Items.Objects[i]);
    if (Entry<>nil) then begin
      inSet := (unstaged and (Entry^.EntryTypeUnStaged in entrySet)) or
             (not unstaged and (Entry^.EntryTypeStaged in entrySet));
      if inSet then
        lb.Selected[i] := not unselect
    end;
  end;
end;

{ TfrmMain }

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if not OpenDirectory(targetDir) then begin
    Application.Terminate;
    exit;
  end;
  fTextLinks.LoadFromConfig(fGit.TopLevelDir);
  UpdateCommandsBar;
end;

procedure TfrmMain.lblBranchClick(Sender: TObject);
begin
  if popBranch.Items.Count=0 then
    UpdateBranchMenu;
  popBranch.PopUp;
end;

procedure TfrmMain.OnPopupItemClick(Sender: TObject);
var
  mi: TMenuItem;
begin
  mi := TMenuItem(sender);

  case mi.tag of
    MENU_BRANCH_NEW:
        NewBranch;

    MENU_BRANCH_NEW_TAG:
        NewTag;

    MENU_BRANCH_RELOAD:
      begin
        fPopPoint := popBranch.PopupPoint;
        Application.QueueAsyncCall(@OnReloadBranchMenu, 0);
      end;

    MENU_BRANCH_SWITCH:
      begin
        Application.QueueAsyncCall(@OnBranchSwitch, PtrInt(Sender));
      end;

    MENU_LIST_VIEW_UNTRACKED:
      begin
        fGitMgr.ViewUntrackedFiles := mi.checked;
        fConfig.ViewUntrackedFiles := fGitMgr.ViewUntrackedFiles;
        fGitMgr.UpdateStatus;
      end;

    MENU_LIST_VIEW_IGNORED:
      begin
        fGitMgr.ViewIgnoredFiles := mi.checked;
        fConfig.ViewIgnoredFiles := fGitMgr.ViewIgnoredFiles;
        fGitMgr.UpdateStatus;
      end;

    //MENU_LIST_VIEW_TRACKED:
    //  begin
    //    fGitMgr.ViewTrackedFiles := mi.Checked;
    //    fConfig.ViewTrackedFiles := fGitMgr.ViewTrackedFiles;
    //    fGitMgr.UpdateStatus;
    //  end;

    else
      ComingSoon;
  end;
end;

procedure TfrmMain.OnBranchSwitch(Data: PtrInt);
var
  mi: TMenuItem;
begin
  mi := TMenuItem(TObject(Data));
  SwitchTo(mi.Caption);
  InvalidateBranchMenu;
end;

procedure TfrmMain.OnIgnoreFileClick(Sender: TObject);
var
  ignored: string;
  mi: TMenuItem absolute Sender;
begin
  // what file?
  ignored := lstUnstaged.Items[mi.Tag];
  if fGitMgr.AddToIgnoreFile(ignored, false, true) then
    fGitMgr.UpdateStatus
  else
    ShowMessage(Format(rsSIsAlreadyInIgnoredList, [ignored]));
end;

procedure TfrmMain.OnIgnoreTypeClick(Sender: TObject);
var
  ignored: string;
  mi: TMenuItem absolute Sender;
begin
  // what file?
  ignored := lstUnstaged.Items[mi.Tag];
  if fGitMgr.AddToIgnoreFile(ignored, true, true) then
    fGitMgr.UpdateStatus
  else
    ShowMessage(Format(rsTheTypeSIsAlreadyInTheIgnoredList, [ExtractFileExt(ignored)]));
end;

procedure TfrmMain.OnReloadBranchMenu(Data: PtrInt);
begin
  UpdateBranchMenu;
  popBranch.PopUp(fPopPoint.x, fPopPoint.y);
end;

procedure TfrmMain.OnRepoInfoStatusDone(Sender: TObject);
  function FilterLocal(info: PRefInfo): boolean;
  begin
    result := info^.subType=rostLocal;
  end;
  function FilterTracking(info: PRefInfo): boolean;
  begin
    result := info^.subType=rostTracking;
  end;
  function FilterTags(info: PRefInfo): boolean;
  begin
    result := info^.subType=rostTag;
  end;

var
  i: Integer;
  s: string;
  log: TLazLoggerFile;
  refItems: TRefInfoArray;
  cmdOut: RawByteString;
begin
  log := DebugLogger;
  fGitMgr.UpdateRefList;
  fGitMgr.UpdateRemotes;
  txtDiff.Clear;
  Log.OnDebugLn := @OnDebugLoggerInterceptor;
  try
    DebugLnEnter('Repository Information:');
    DebugLn('Date: %s', [DateTimeToStr(Now)]);
    DebugLn('Top Level Directory: %s', [fGit.TopLevelDir]);
    DebugLn('Repo .git Directory: %s', [fGit.GitDir]);
    DebugLn('Git program: %s', [fGit.Exe]);
    DebugLn('Git version: %s', [fGit.Version]);
    DebugLnExit('');
    DebugLnEnter('Status:');
    DebugLn('Branch: %s',[fGitMgr.Branch]);
    DebugLn('Tracking Branch: %s',[fGitMgr.Upstream]);
    DebugLn('Commits Ahead: %d',[fGitMgr.CommitsAhead]);
    DebugLn('Commits Behind: %d',[Abs(fGitMgr.CommitsBehind)]);
    DebugLn('Branch Commit Sha: %s',[fGitMgr.BranchOID]);
    DebugLn('Tag: %s',[fGitMgr.LastTag]);
    DebugLn('Commits Since Tag %s: %d',[fGitMgr.LastTag, fGitMgr.LastTagCommits]);
    DebugLn('Tag Commit Sha: %s',[fGitMgr.LastTagOID]);
    DebugLn('Is Merging: %s',[dbgs(fGitMgr.Merging)]);
    DebugLn('Has Merging Conflict: %s',[dbgs(fGitMgr.MergingConflict)]);
    DebugLnExit('');
    DebugLnEnter('Local Branches:');
    refItems := fGitMgr.RefsFilter('', @FilterLocal);
    for i := 0 to Length(refItems)-1 do
    with refItems[i]^ do begin
      s := '';
      if Head then s := ' (HEAD)';
      if upstream<>'' then  DebugLn('%s %s -> %s%s', [objName, refName, upstream, s])
      else                  DebugLn('%s %s%s',[objName, refName, s]);
    end;
    DebugLnExit('');
    DebugLnEnter('Tracking branches:');
    refItems := fGitMgr.RefsFilter('', @FilterTracking);
    for i := 0 to Length(refItems)-1 do
    with refItems[i]^ do
      DebugLn('%s %s',[objName, refName]);
    DebugLnExit('');
    DebugLnEnter('Tags:');
    refItems := fGitMgr.RefsFilter('', @FilterTags);
    for i := 0 to Length(refItems)-1 do
    with refItems[i]^ do
      DebugLn('%s %s',[objName, refName]);
    DebugLnExit('');
    DebugLnEnter('Remotes:');
    for i := 0 to Length(fGitMgr.Remotes)-1 do
      DebugLn('%s %s', [fGitMgr.Remotes[i].name, fGitMgr.Remotes[i].fetch]);
    DebugLnExit('');
    if gblReportLooseObjects then begin
      DebugLnEnter('Loose Objects:');
      if fGit.Any('count-objects -v -H', cmdOut)<=0 then DebugLnMultiline(cmdOut)
      else                                               DebugLnMultiline(fGit.LogError);
      DebugLnExit('');
    end;
  finally
    Log.OnDebugLn := nil;
  end;
end;

procedure TfrmMain.OnRestoreFileClick(Sender: TObject);
var
  mi: TMenuItem;
  entryArray: TPFileEntryArray;
  aFile: String;
  res: TModalResult;
begin
  mi := TMenuItem(Sender);
  entryArray := MakeMenuItemUnstagedEntryArray(mi);

  if Length(entryArray)=1 then
    aFile := MakePathList(entryArray, false)
  else
    aFile := format(rsDFiles, [Length(entryArray)]);

  // this is necessary because we normally hide lists selection
  // when they are unfocused, but in this case the next dialog
  // will unfocus the lists and we want to see the selection of
  // files that will be restored.
  fListAlwaysDrawSelection := true;

  res := QuestionDlg(rsRestoringWorkFiles, format(rsRestoringWorkFilesWarning, [aFile]), mtWarning,
    [mrYes, rsDiscardChanges, mrCancel, rsCancel], 0 );

  fListAlwaysDrawSelection := false;

  if res<>mrYes then
    exit;

  {$IFDEF DEBUG}
  aFile := MakePathList(entryArray);
  DebugLn('Restoring: ',aFile);
  {$ENDIF}

  fGit.ResetLogError;
  if fGit.Restore(entryArray, false)<=0 then
    fGitMgr.UpdateStatus;
  txtDiff.Text := fGit.LogError;
end;

procedure TfrmMain.OnDeleteFilesClick(Sender: TObject);
var
  mi: TMenuItem absolute sender;
  aFile: string;
  res: TModalResult;
  i: Integer;
  ok, someDeleted: boolean;
begin

  if mi.Tag>=0 then
    aFile := lstUnstaged.Items[mi.Tag]
  else
    aFile := format(rsDFiles, [lstUnstaged.SelCount]);

  res := QuestionDlg(rsDeletingWorkFiles, format(rsDeletingWorkFilesWarning, [aFile]), mtWarning,
    [mrYes, rsDeleteFiles, mrCancel, rsCancel], 0 );

  if res<>mrYes then
    exit;

  if mi.Tag>=0 then begin
    ok := DeleteFile(fGit.TopLevelDir + aFile);
    someDeleted := ok;
  end else begin
    someDeleted := false;
    for i := 0 to lstUnstaged.Count-1 do
      if lstUnstaged.Selected[i] then begin
        ok := DeleteFile(fGit.TopLevelDir + lstUnstaged.Items[i]);
        someDeleted := someDeleted or ok;
        if not ok then
          break;
      end;
  end;

  if not ok then
    ShowMessage(rsSomeFilesCouldNotBeDeleted);

  if someDeleted then
    fGitMgr.UpdateStatus;
end;

procedure TfrmMain.OnStageAllClick(Sender: TObject);
var
  mi: TMenuItem;
  cmd, cmdOut: RawByteString;
begin
  cmd := '';
  mi := TMenuItem(sender);
  case mi.tag of
    MENU_LIST_STAGE_CHANGED:  cmd := 'add -u';
    MENU_LIST_STAGE_ALL:      cmd := 'add -A';
    MENU_LIST_UNSTAGE_ALL:    cmd := 'reset';
  end;

  if cmd<>'' then begin
    fGit.ResetLogError;
    if fGit.Any(cmd, cmdOut)<=0 then
      fGitMgr.UpdateStatus;
    txtDiff.Text := fGit.LogError;
  end;
end;

procedure TfrmMain.OnStageItemClick(Sender: TObject);
var
  mi: TMenuItem;
  entryArray: TPFileEntryArray;
begin
  mi := TMenuItem(Sender);
  entryArray := MakeMenuItemUnstagedEntryArray(mi);

  fGit.ResetLogError;
  if fGit.Add(entryArray)<=0 then
    fGitMgr.UpdateStatus;
  txtDiff.Text := fGit.LogError;
end;

procedure TfrmMain.OnUnstageItemClick(Sender: TObject);
var
  mi: TMenuItem;
  entryArray: TPFileEntryArray;
begin
  mi := TMenuItem(Sender);
  entryArray := MakeMenuItemStagedEntryArray(mi);

  fGit.ResetLogError;
  if fGit.Restore(entryArray, true)<=0 then
    fGitMgr.UpdateStatus;
  txtDiff.Text := fGit.LogError;
end;

procedure TfrmMain.lblBranchContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

  if popBranch.Items.Count>0 then
    exit;

  UpdateBranchMenu;
end;

procedure TfrmMain.lblInfoOldClick(Sender: TObject);
begin
  //fLogCache.NotifyMe;
end;

procedure TfrmMain.lstUnstagedContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  lb: TListBox absolute Sender;
  mi: TMenuItem;
  Entry: PFileEntry;
  isUnstaged: Boolean;
  aIndex, selCount: Integer;
  aFile: string;

  procedure AddViewItems;
  begin
    if isUnstaged then begin
      mi := AddPopItem(popLists, rsViewUntrackedFiles, @OnPopupItemClick, MENU_LIST_VIEW_UNTRACKED);
      mi.AutoCheck := true;
      mi.Checked := fConfig.ViewUntrackedFiles;
      mi := AddPopItem(popLists, rsViewIgnoredFiles, @OnPopupItemClick, MENU_LIST_VIEW_IGNORED);
      mi.AutoCheck := true;
      mi.Checked := fConfig.ViewIgnoredFiles;
      mi := AddPopItem(popLists, rsViewTrackedFiles, @OnPopupItemClick, MENU_LIST_VIEW_TRACKED);
      mi.AutoCheck := true;
      mi.Checked := fConfig.ViewTrackedFiles;
      mi.Enabled := false;
    end;
  end;

  procedure AddStageFile;
  begin
    AddPopItem(popLists, format(rsStageS, [aFile]), @OnStageItemClick, aIndex);
  end;

  procedure AddUnstageFile;
  begin
    AddPopItem(popLists, format(rsUnstageS, [aFile]), @OnUnstageItemClick, aIndex);
  end;

  procedure AddStageAll;
  begin
    AddPopItem(popLists, rsStageChanged, @OnStageAllClick, MENU_LIST_STAGE_CHANGED);
    AddPopItem(popLists, rsStageAll, @OnStageAllClick, MENU_LIST_STAGE_ALL);
  end;

  procedure AddUnstageAll;
  begin
    AddPopItem(popLists, rsUnstageAll, @OnStageAllClick, MENU_LIST_UNSTAGE_ALL);
  end;

  procedure AddIgnoreUntracked;
  var
    ext: string;
  begin
    AddPopItem(popLists, '-', nil, 0);
    AddPopItem(popLists, format(rsAddSToIgnoreList, [aFile]), @OnIgnoreFileClick, aIndex);
    ext :=ExtractFileExt(aFile);
    if ext<>'' then
      AddPopItem(popLists, format(rsAddSFilesToIgnoreList, [ext]), @OnIgnoreTypeClick, aIndex);
  end;

  procedure AddRestoreFiles;
  var
    n: Integer;
  begin
    if isUnstaged then begin
      AddPopItem(popLists, '-', nil, 0);
      // this covers the one (index>=0) or all selected (index<0)
      if ((aIndex>=0)and(Entry^.EntryTypeUnStaged in ChangedInWorktreeSet)) or
         AreAllSelectedItemsOfEntryType(lstUnstaged, true, ChangedInWorktreeSet)
      then
        AddPopItem(popLists, format(rsRestoreS, [aFile]), @OnRestoreFileClick, aIndex);
      // we need an option for all changed
      n := CountListItemsOfEntryType(lstUnstaged, true, false, ChangedInWorktreeSet);
      if n > 0 then
        AddPopItem(popLists, rsRestoreAllChanged, @OnRestoreFileClick, LIST_TAG_ALL_CHANGED);
    end;
  end;

  procedure CheckWhatToProcess;
  begin
    if selCount=1 then begin
      aFile := ExtractFileName(lb.GetSelectedText);
      aIndex := GetSelectedIndex(lb);
      Entry := PFileEntry(lb.Items.Objects[aIndex])
    end else begin
      aFile := format(rsDFiles, [selCount]);
      aIndex := LIST_TAG_ALL_SELECTED;
      Entry := nil;
    end;
  end;

  procedure AddDeleteFiles;
  var
    safeCount: Integer;
    n, i: Integer;
    entrySet: TSetOfEntryType;
  begin
    if isUnstaged and (SelCount>0) then begin
      // check if there is only (in the index) already deleted files
      entrySet := [etUntracked];
      if gblAllowDeleteChanged then
        entrySet := entrySet + ChangedInWorktreeSet - DeletedInWorktreeSet;
      safeCount := CountListItemsOfEntryType(lstUnstaged, true, true, entrySet);
      if SelCount=safeCount then begin
        AddPopItem(popLists, '-', nil, 0);
        AddPopItem(popLists, format(rsDeleteS, [aFile]), @OnDeleteFilesClick, aIndex)
      end;
      //else begin
      //  n := SelCount - safeCount;
      //  if n=1 then
      //    AddPopItem(popLists, format('Delete ''%s'' denied. Is changed', [aFile]), nil, aIndex)
      //  else
      //    AddPopItem(popLists, format('Delete ''%s'' denied. %d Are changed', [aFile, n]), nil, aIndex);
      //end;
    end;
  end;

  procedure AddCreateAPatch;
  begin
    if SelCount>0 then begin
      AddPopItem(popLists, '-', nil, 0);
      AddPopItem(popLists, format(rsCreateAPatchFileFromS, [aFile]), @OnCreatePatchClick, PtrInt(lb));
      AddPopItem(popLists, format(rsCopyPatchFromSToTheClipboard, [aFile]), @OnCopyPatchClick, PtrInt(lb));
    end;
  end;

begin

  isUnstaged := Sender=lstUnstaged;
  aIndex := lb.GetIndexAtXY(MousePos.x, MousePos.y);
  if aIndex>=0 then begin
    aFile := ExtractFileName(lb.Items[aIndex]);
    Entry := PFileEntry(lb.Items.Objects[aIndex])
  end else begin
    aFile := '';
    Entry := nil;
    aIndex := LIST_TAG_ALL_SELECTED;
  end;

  popLists.Items.Clear;
  selCount := lb.SelCount;

  mi := AddPopItem(popLists, '', nil, 0);
  mi.Action := actRescan;
  AddPopItem(popLists, '-', nil, 0);

  if selCount=0 then begin

    if Entry<>nil then begin
      if isUnstaged then begin
        AddStageFile;
        AddStageAll;
      end
      else begin
        AddUnstageFile;
        AddUnstageAll;
      end;
      AddRestoreFiles;
      if Entry^.EntryKind in [ekUntracked, ekIgnored] then
        AddIgnoreUntracked;
      AddPopItem(popLists, '-', nil, 0);
    end else
    if lb.Count>0 then begin
      if isUnstaged then begin
        AddStageAll;
        AddRestoreFiles;
        AddPopItem(popLists, '-', nil, 0);
      end else
        AddUnstageAll;
    end;

    AddViewItems;

  end else begin

    CheckWhatToProcess;

    if isUnstaged then begin
      AddStageFile;
      AddStageAll;
      AddRestoreFiles;
      AddDeleteFiles;
      AddCreateAPatch;

      if (Entry<>nil) and (Entry^.EntryKind in [ekUntracked, ekIgnored]) then
        AddIgnoreUntracked;

      AddPopItem(popLists, '-', nil, 0);
      AddViewItems;
    end else begin
      AddCreateAPatch;
      AddUnstageFile;
    end;

  end;

  CheckMenuDivisorInLastPosition(popLists);

end;

procedure TfrmMain.ItemAction(sender: TListbox; aIndex: Integer);
var
  Entry: PFileEntry;
  cmdOut: RawByteString;
begin
  Entry := PFileEntry(Sender.Items.Objects[aIndex]);
  if sender=lstUnstaged then begin
    WriteStr(cmdOut, Entry^.EntryTypeUnStaged);
    case Entry^.EntryTypeUnStaged of
      etUntracked, etNotUpdatedA,
      etWorktreeChangedSinceIndex..etTypeChangedInWorktreeSinceIndexC:
        begin
          fGit.ResetLogError;
          if fGit.Add(Entry)<=0 then
            fGitMgr.UpdateStatus;
          txtDiff.Text := fGit.LogError;
        end;
      etDeletedInWorktree:
        begin
          fGit.ResetLogError;
          if fGit.Rm(Entry)<=0 then
            fGitMgr.UpdateStatus;
          txtDiff.Text := fGit.LogError;
        end;
      // other merge conflicts:
      // see:
      //etUnmergedDeletedByUs:
      //  begin
      //  end
      else
        ShowMessageFmt(rsNotYetImplementedForUnstagedS, [cmdOut]);
    end;
  end else begin
    WriteStr(cmdOut, Entry^.EntryTypeStaged);
    case Entry^.EntryTypeStaged of
      etAddedToIndex..etAddedToIndexD,
      etUpdatedInIndex..etTypeChangedInIndexD,
      etRenamedInIndex..etRenamedInIndexD,
      etDeletedFromIndex:
        begin
          fGit.ResetLogError;
          if fGit.Restore(entry, true)<=0 then
            fGitMgr.UpdateStatus;
          txtDiff.Text := fGit.LogError;
        end;
      else
        ShowMessageFmt(rsNotYetImplementedForStagedS, [cmdOut]);
    end;
  end;
end;

procedure TfrmMain.UpdateBranchMenu;
var
  list, branchLine: TStringList;
  i: integer;
  mi: TMenuItem;
  ref: PRefInfo;

  function FilterLocalBranches(info: PRefInfo): boolean;
  begin
    result := (info^.subType=rostLocal);
  end;

begin
  InvalidateBranchMenu;

  AddPopItem(popBranch, rsNewBranch, @OnPopupItemClick, MENU_BRANCH_NEW);
  AddPopItem(popBranch, rsNewTag, @OnPopupItemClick, MENU_BRANCH_NEW_TAG);
  AddPopItem(popBranch, rsReload, @OnPopupItemClick, MENU_BRANCH_RELOAD);
  AddPopItem(popBranch, '-', nil, MENU_INVALID);

  try

    fGitMgr.UpdateRefList;

    for ref in fGitMgr.RefsFilter('', @FilterLocalBranches) do begin
      mi := AddPopItem(popBranch, ref^.refName, @OnPopupItemClick, MENU_BRANCH_SWITCH);
      mi.GroupIndex := 1;
      mi.AutoCheck := true;
      mi.Checked := ref^.head;
      if mi.Checked then
        mi.OnClick := nil;
      mi.RadioItem := true;
    end;

  finally
    CheckMenuDivisorInLastPosition(popBranch);
  end;
end;

procedure TfrmMain.ShowError;
begin
  txtDiff.Text := cmdLine.ErrorLog;
end;

function isBinBuffer(Buffer:pbyte; size: SizeInt): boolean;
var
  p,q: pbyte;
begin
  result := true;
  p := Buffer;
  q := p + Size;
  while (p<q) do begin
    if not (p^ in [9,10,13,32..255]) then
      exit;
    inc(p);
  end;
  result := false;
end;

procedure SampleOfFile(filename: string; out stream: TStream);
var
  F: TFileStream;
  readbytes: Int64;
  buffer: pbyte;

  procedure StoreString(msg:string);
  begin
    stream := TStringStream.Create(msg);
  end;

begin

  if not FileExists(filename) then begin
    StoreString(format(rsSDoesNotExists, [filename]));
    exit;
  end;

  //{$ifdef Unix}
  //// use the file command for identifying the file...
  //cmdLine.RunProcess('file ' + filename, '', cmdOut);
  // check for ASCII
  //{$else}
  //{$endif}

  F := TFileStream.Create(filename, fmOpenRead + fmShareDenyNone);
  GetMem(buffer, BIN_BUFSIZE);
  readBytes := F.Read(buffer^, BIN_BUFSIZE);
  if IsBinBuffer(buffer, readBytes) then begin
    StoreString(format(rsTheFileSIsBinaryDBytes, [filename, F.Size]));
    F.Free;
  end else
    stream := F;

end;

procedure TfrmMain.ViewFile(filename: string);
var
  stream: TStream;
begin
  SampleOfFile(filename, stream);
  stream.Position := 0;
  txtDiff.Lines.LoadFromStream(stream);
  stream.Free;
end;

procedure TfrmMain.ComingSoon;
begin
  ShowMessage(rsThisFeatureWillBeImplementedASAP);
end;

function TfrmMain.MakeMenuItemUnstagedEntryArray(mi: TMenuItem): TPFileEntryArray;
var
  aIndex: PtrInt;
  i, n: Integer;
  entry: PFileEntry;
begin
  n := 0;
  result := nil;
  SetLength(result, lstUnstaged.Count);

  aIndex := mi.Tag;
  for i:=0 to lstUnstaged.Count-1 do begin
    entry := PFileEntry(lstUnstaged.Items.Objects[i]);
    if (aIndex=i) or
       ((aIndex=LIST_TAG_ALL_SELECTED) and lstUnstaged.Selected[i]) or
       ((aIndex=LIST_TAG_ALL_CHANGED) and (entry^.EntryTypeUnStaged in ChangedInWorktreeSet))
    then begin
      result[n] := entry;
      inc(n);
      if aIndex=i then break;
    end
  end;

  SetLength(result, n);
end;

function TfrmMain.MakeMenuItemStagedEntryArray(mi: TMenuItem): TPFileEntryArray;
var
  aIndex: PtrInt;
  i, n: Integer;
  entry: PFileEntry;
begin
  n := 0;
  result := nil;
  SetLength(result, lstStaged.Count);

  aIndex := mi.Tag;
  for i:=0 to lstStaged.Count-1 do begin
    entry := PFileEntry(lstStaged.Items.Objects[i]);
    if (aIndex=i) or
       ((aIndex=LIST_TAG_ALL_SELECTED) and lstStaged.Selected[i]) or
       ((aIndex=LIST_TAG_ALL_CHANGED) and (entry^.EntryTypeStaged in ChangedInIndexSet))
    then begin
      result[n] := entry;
      inc(n);
      if aIndex=i then break;
    end
  end;

  SetLength(result, n);
end;

procedure TfrmMain.NewBranch;
var
  f: TfrmNewBranch;
begin
  f := TfrmNewBranch.Create(Self);
  f.GitMgr := fGitMgr;
  try
    if f.ShowModal=mrOk then
      fGitMgr.QueueNewBranch(self, f.BranchName, f.GetBranchCommandOptions, f.Switch, f.Fetch);
  finally
    f.Free;
  end;
end;

procedure TfrmMain.InvalidateBranchMenu;
begin
  popBranch.Items.Clear;
end;

procedure TfrmMain.NewTag;
begin
  fGitMgr.QueueNewTag(fGitMgr.BranchOID);
end;

procedure TfrmMain.CheckMenuDivisorInLastPosition(pop: TPopupMenu);
begin
  if pop.Items.Count>1 then begin
    if pop.Items[pop.Items.Count-1].Caption='-' then
      pop.Items.Delete(pop.Items.Count-1);
  end;
end;

procedure TfrmMain.ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
var
  info: PTagInfo;
  binfo: PBranchInfo;
begin
  case what of

    GITMGR_EVENT_UpdateStatus:
      begin
        prgBar.Visible := false;
        if data>0 then
          ShowError
        else begin
          lstUnstaged.Items.Assign(fGitMgr.UnstagedList);
          lstStaged.Items.Assign(fGitMgr.StagedList);
          UpdateBranch;
        end;
      end;

    GITMGR_EVENT_NEWTAG:
      begin
        info := {%H-}PTagInfo(data);
        ShowNewTagForm(info^.data);
        Finalize(info^.data);
        dispose(info);
      end;

    GITMGR_EVENT_SWITCHTOTAG:
      begin
        info := {%H-}PTagInfo(data);
        ShowSwitchToTagForm(info^.data);
        finalize(info^.data);
        dispose(info);
      end;

    GITMGR_EVENT_SWITCHTOCOMMIT:
      begin
        info := {%H-}PTagInfo(data);
        ShowSwitchToCommitForm(info^.data);
        finalize(info^.data);
        dispose(info);
      end;

    GITMGR_EVENT_NEWBRANCH:
      begin
        binfo := {%H-}PBranchInfo(data);
        CreateBranch(binfo);
      end;

    GITMGR_EVENT_LOADREPOSITORY:
      begin
        info := {%H-}PTagInfo(data);
        OpenDirectory(info^.data);
        Dispose(info);
      end;
  end;
end;

procedure TfrmMain.ShowNewTagForm(commit: string);
var
  f: TfrmNewTag;
begin
  f := TfrmNewTag.Create(Self);
  f.Oid := commit;
  try
    if f.ShowModal=mrOk then begin
      if fGit.Tag(f.txtName.Text, f.Oid, f.chkAnnotated.checked, f.txtMsg.Text)>0 then
        ShowError
      else begin
        fGitMgr.ForceTagDescription;
        fGitMgr.UpdateStatus;
        fGitMgr.UpdateRefList;
      end;
    end;
  finally
    f.free;
  end;
end;

procedure TfrmMain.ShowSwitchToTagForm(aTag: string);
var
  f: TfrmCheckouTag;
  cmd: string;
begin
  f := TfrmCheckouTag.Create(Self);
  f.TagName := aTag;
  f.GitMgr := fGitMgr;
  try
    if f.ShowModal=mrOk then begin
      cmd := aTag;
      if f.chkCreateBranch.Checked then
        cmd := '-b ' + Trim(f.txtBranchName.Text) + ' ' + cmd;
      SwitchTo(cmd);
    end;
  finally
    f.free;
  end;
end;

procedure TfrmMain.ShowSwitchToCommitForm(aCommit: string);
var
  f: TfrmCheckouTag;
  cmd: string;
begin
  f := TfrmCheckouTag.Create(Self);
  f.IsCommit := true;
  f.TagName := aCommit;
  f.GitMgr := fGitMgr;
  try
    if f.ShowModal=mrOk then begin
      cmd := aCommit;
      if f.chkCreateBranch.Checked then
        cmd := '-b ' + Trim(f.txtBranchName.Text) + ' ' + cmd;
      SwitchTo(cmd);
    end;
  finally
    f.free;
  end;
end;

procedure TfrmMain.SwitchTo(cmd: string);
begin
  if fGit.Switch(cmd)>0 then
    ShowError
  else begin
    fGitMgr.ForceTagDescription;
    fGitMgr.UpdateStatus;
    fGitMgr.UpdateRefList;
    InvalidateBranchMenu;
  end;
end;

procedure TfrmMain.SaveCommitMessage;
begin
  fConfig.WriteString('CommitMsg', ReplaceEOLs(txtComment.Text, true), fGit.TopLevelDir);
end;

procedure TfrmMain.RestoreCommitMessage;
begin
  txtComment.Text := ReplaceEOLs(fConfig.ReadString('CommitMsg', '', fGit.TopLevelDir), false);
end;

procedure TfrmMain.UpdateCommandsBar;
var
  btn: TToolButton;
  i: Integer;
begin

  // remove the current buttons
  while barCustomCmds.ButtonCount>2 do
    barCustomCmds.Buttons[2].Free;

  // add a divider
  if fCustomCommands.Count>0 then begin
    btn := TToolButton.Create(self);
    btn.Style := tbsDivider;
    btn.Parent := barCustomCmds;
  end;

  // add the new buttons
  for i:=0 to fCustomCommands.Count-1 do
    with fCustomCommands[i] do begin
      btn := TToolButton.Create(self);
      if description='-' then
        btn.Style := tbsDivider
      else begin
        btn.Style := tbsButton;
        btn.Tag := i;
        btn.OnClick := @OnCustomCommandClick;
        btn.Hint := description;
        btn.Caption := description;
        btn.Parent := barCustomCmds;
      end;
    end;

  // layout buttons, this trick makes them appear in order
  btnAddCustomCmd.Left := 0;
  btnGitCmd.Left := 0;
  for i:=barCustomCmds.ButtonCount-1 downto 0 do
    barCustomCmds.Buttons[i].Left := 0;

end;

function OwnerDrawStateToStr(State: TOwnerDrawState): string;
  procedure Add(st: string);
  begin
    if result<>'' then result += ',';
    result += st;
  end;
var
  stateItem: TOwnerDrawStateType;
  s: string;
begin
  for stateItem in State do begin
    WriteStr(s, stateItem);
    Add(s);
  end;
  result := '[' + result + ']';
end;

procedure TfrmMain.lstUnstagedDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  lb: TListBox;
  entry: PFileEntry;
  aCanvas: TCanvas;
  aColor: TColor;
  sel, isFocused: boolean;
begin
  if index<0 then
    exit;

  lb := TListBox(Control);
  entry := PFileEntry(lb.Items.Objects[index]);

  isFocused := lb.Focused or fListAlwaysDrawSelection;
  sel := (odSelected in State) and isFocused;

  aCanvas := lb.Canvas;
  if isFocused then
    if sel then aColor := clHighlight
    else        aColor := lb.Color
  else          aColor := lb.Color;

  aCanvas.Brush.Color := aColor;
  aCanvas.FillRect(aRect);

  if isFocused then
    if sel then aColor := clHighlightText
    else        aColor := clBlack
  else          aColor := clBlack;

  lb.Canvas.Font.Color := aColor;
  lb.Canvas.Brush.Style := bsClear;
  lb.Canvas.TextRect(aRect, aRect.Left + 22, aRect.Top, lb.Items[index]);

  index := 0;
  if lb=lstUnstaged then
    case entry^.EntryTypeUnStaged of
      etNotUpdatedA:                                              index := 2;
      etWorktreeChangedSinceIndex..etWorktreeChangedSinceIndexT:  index := 6;
      etDeletedInWorktree..etDeletedInWorktreeC:                  index := 3;
      etIgnored:                                                  index := 5;
      etUnmergedAddedByUs..etUnmergedBothModified:                index := 11;
    end
  else
    case entry^.EntryTypeStaged of
      etUpdatedInIndex..etUpdatedInIndexD:                        index := 7;
      etAddedToIndex..etAddedToIndexD:                            index := 2;
      etDeletedFromIndex:                                         index := 3;
      etCopiedInIndex..etCopiedInIndexD:                          index := 4;
      etRenamedInIndex..etRenamedInIndexD:                        index := 10;
    end;

  imgList.Draw(lb.Canvas, aRect.Left + 2, aRect.Top + 1, index);
end;

procedure TfrmMain.lstUnstagedMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  aIndex: Integer;
begin
  if button=mbLeft then begin
    aIndex := TListBox(Sender).GetIndexAtXY(x, y);
    if aIndex>=0 then begin
      if (x>0) and (x < 20) then begin
        fClickedIndex := aIndex;
        Application.QueueAsyncCall(@DoItemAction, ptrInt(Sender));
      end else
        Application.QueueAsyncCall(@DoGitDiff, PtrInt(Sender));
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  prgBar.Style := pbstMarquee;

  fhlHelper := THighlighterHelper.create;

  fGitMgr := TGitMgr.Create;
  fGitMgr.Config := fConfig;
  fGitMgr.AddObserver(Self);

  fGit := fGitMgr.Git;

  fConfig.OpenConfig;

  if not fGitMgr.Initialize then begin
    fConfig.CloseConfig;
    DebugLn('Error: Could not find git command');
    Application.Terminate;
    exit;
  end;

  //DebugLn('git=', fGit.Exe);

  txtDiff.Clear;

  fConfig.ReadFont(txtDiff.Font, 'viewer', fpFixed, SECTION_FONTS);

  AddPopupMenu(txtDiff);

  panFileState.Caption := '';

  RestoreGui;

  fConfig.ReadPreferences;
  fGitMgr.ViewUntrackedFiles := fConfig.ViewUntrackedFiles;
  fGitMgr.ViewIgnoredFiles := fConfig.ViewIgnoredFiles;
  fGitMgr.ViewTrackedFiles := fConfig.ViewTrackedFiles;
  fGitMgr.ShowTags := fConfig.ShowTags;

  if fConfig.ReadBoolean('NeedsMenuWorkaround'{$ifdef Linux},true{$endif}) then begin
    Self.Menu := nil;
    Application.QueueAsyncCall(@DelayedShowMenu, 0);
  end;

  fCustomCommands := TCustomCommandsMgr.create;
  fCustomCommands.LoadFromConfig;

  fTextLinks := TTextLinks.Create;

  fConfig.MenuMRE(popCommands, false, @OnMRECommand, '', SECTION_MRECOMMANDS);

  gblCutterMode := fConfig.ReadBoolean('CutterMode', gblCutterMode);
  gblTopologicalMode := fConfig.ReadBoolean('TopologicalMode', gblTopologicalMode);
  gblAllowDeleteChanged := fConfig.ReadBoolean('AllowDeleteChanged', gblAllowDeleteChanged);
  gblReportLooseObjects := fConfig.ReadBoolean('ReportLooseObjects', gblReportLooseObjects);

  fConfig.CloseConfig;

  gblInvalidateCache := Application.HasOption('ClearCache');
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveGui;
end;

procedure TfrmMain.actRescanExecute(Sender: TObject);
begin
  fGitMgr.UpdateStatus;
end;

procedure TfrmMain.actRestoreCommitMsgExecute(Sender: TObject);
var
  s: String;
  i: SizeInt;
begin
  RestoreCommitMessage;
  if txtComment.Lines.Count>0 then begin
    s := txtComment.Lines[0];
    i := RPos(':', S);
    if i>0 then begin
      txtComment.SelStart := i;
      txtComment.SelLength := Length(s);
    end;
  end;
end;

procedure TfrmMain.btnGitCmdArrowClick(Sender: TObject);
begin
  ShowMessage('Arrow click');
end;

// clipboard empty on exit workaround
// reference: https://wiki.lazarus.freepascal.org/Clipboard#How_to_fix_empty_GTK2_clipboard_on_exit
procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{$ifdef linux}
var
  c: PGtkClipboard;
  t: string;
{$endif}
begin
  {$ifdef linux}
  c := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  t := Clipboard.AsText;
  gtk_clipboard_set_text(c, PChar(t), Length(t));
  gtk_clipboard_store(c);
  {$endif}
end;

procedure TfrmMain.actCommitExecute(Sender: TObject);
begin
  DoCommit;
end;

procedure TfrmMain.actAddCmdExecute(Sender: TObject);
var
  F: TfrmCustomCommands;
begin
  F := TfrmCustomCommands.Create(Self);
  F.Commands := fCustomCommands;
  F.AddNew := true;
  try
    if F.ShowModal=mrOk then
      UpdateCommandsBar;
  finally
    F.Free;
  end;
end;

procedure TfrmMain.actCloneExecute(Sender: TObject);
begin
  DoClone;
end;

procedure TfrmMain.actFetchExecute(Sender: TObject);
begin
  DoFetch;
end;

procedure TfrmMain.actGitCmdExecute(Sender: TObject);
var
  F: TfrmGitCmd;
  s: string;
begin
  F := TfrmGitCmd.Create(Self);
  try
    if F.ShowModal=mrOk then begin
      s := StringReplace(F.txtGitCmd.Text, 'git', fGit.Exe, []);
      if RunInteractive(s, fGit.TopLevelDir, 'Run a git command', F.txtGitCmd.Text)<=0 then begin
        if F.chkRemember.Checked then
          fConfig.MenuMRE(popCommands, true, @OnMRECommand, F.txtGitCmd.Text, SECTION_MRECOMMANDS);
      end
    end;
  finally
    F.Free;
  end;
end;

procedure TfrmMain.actInsertBranchNameExecute(Sender: TObject);
begin
  txtComment.SelText := fGitMgr.Branch + ': ';
end;

procedure TfrmMain.actNewLogExecute(Sender: TObject);
begin
  DoNewLog;
end;

procedure TfrmMain.actPullExecute(Sender: TObject);
begin
  DoPull;
end;

procedure TfrmMain.actPushDialogExecute(Sender: TObject);
var
  F: TfrmPush;
begin
  F := TfrmPush.Create(Self);
  F.GitMgr := fGitMgr;
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TfrmMain.actPushExecute(Sender: TObject);
begin
  DoPush;
end;

procedure TfrmMain.actQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actRepoInfoExecute(Sender: TObject);
begin
  DoRepoInfo;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fCustomCommands.Free;
  frmLog.Free;
  fGit := nil;
  fGitMgr.RemoveObserver(Self);
  fhlHelper.Free;
  fGitMgr.Free;
  fTextLinks.Free;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
var
  aFile, cmd: String;
  F: TFileStream;
  readbytes: Int64;
  buffer: pchar;
  serialCount, simpleCount: Integer;
  res: TModalResult;
  isDiff: boolean;
begin
  GetMem(buffer, BIN_BUFSIZE+1);
  try
    serialCount := 0;
    simpleCount := 0;

    // check dropped files are all text diff files
    for aFile in Filenames do begin
      F := TFileStream.Create(aFile, fmOpenRead + fmShareDenyNone);
      try
        readBytes := F.Read(buffer^, BIN_BUFSIZE);
        buffer[readBytes] := #0;
        if IsBinBuffer(pbyte(buffer), readBytes) then
          exit;
        isDiff := strpos(buffer, '@@ ')<>nil;
        if isDiff and (strlcomp('From ', buffer, 5)=0) and (strpos(buffer, 'From: ')<>nil) then
          inc(serialCount)
        else if isDiff then
          inc(simpleCount);
      finally
        F.Free;
      end;
    end;

    // we can process either a simple diff for git apply
    // or a group of serial diff files for git am
    if (serialCount>0) then begin
      ShowMessage(rsApplyingSerialPatchesIsNotYetImplemented);
      exit;
    end;

    if (simpleCount<>Length(Filenames)) or (SimpleCount>1) then begin
      ShowMessage(rsInvalidAmountOfPatchesTooApply);
      exit;
    end;

    res := QuestionDlg(rsPatchingTheWorkArea,
      format(rsYouAreTryingToApply, [QuotedStr(ExtractFileName(Filenames[0]))]),
       mtConfirmation, [mrYes, rsApplyPatch, mrCancel, rsCancel], 0 );

    if res=mrYes then begin
      cmd := fGit.Exe + ' apply '+ Sanitize(Filenames[0]);

      if RunInteractive(cmd, fGit.TopLevelDir, rsApplyingAPatch, cmd)<=0 then
        fGitMgr.UpdateStatus();
    end;
  finally
    FreeMem(buffer);
  end;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_ESCAPE then
    close;
end;

function TfrmMain.OpenDirectory(aDir: string): boolean;
begin
  result := false;
  aDir := ExpandFileName(aDir);
  fGit.OpenDir(aDir);
  if fGit.TopLevelDir='' then begin
    ShowMessageFmt(rsCouldnTGetToplevelDirectoryOfS, [aDir]);
    exit;
  end;
  fDir := aDir;
  fGitMgr.UpdateStatus;
  fGitMgr.UpdateRemotes;
  prgBar.Visible := true;
  if frmLog<>nil then
    frmLog.clear;
  result := true;
end;

procedure TfrmMain.DoItemAction(Data: PtrInt);
begin
  ItemAction(TListBox(Data), fClickedIndex);
end;

procedure TfrmMain.DoCommit;
begin
  if lstStaged.Count=0 then begin
    ShowMessage(rsYouHaveToStageSomething);
    exit;
  end;
  if Trim(txtComment.Text)='' then begin
    ShowMessage(rsYourCommitMessageIsEmpt);
    exit;
  end;
  if fGit.Commit(txtComment.Text,'')>0 then
    ShowError
  else begin
    SaveCommitMessage;
    fGitMgr.ForceTagDescription;
    fGitMgr.UpdateStatus;
    fGitMgr.UpdateRefList;
    txtDiff.Clear;
    txtComment.Clear;
  end;
end;

procedure TfrmMain.DoClone;
var
  F: TfrmClone;
begin
  F := TfrmClone.Create(Self);
  F.GitMgr := fGitMgr;
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TfrmMain.DoNewLog;
begin

  if ssShift in KeyboardStateToShiftState then
    gblInvalidateCache := true;

  if frmLog=nil then begin
    frmLog := TfrmLog.Create(Self);
    frmLog.GitMgr := fGitMgr;
    frmLog.HlHelper := fHlHelper;
  end;

  if actNewLog.Checked then begin
    frmLog.Show;
    frmLog.BringToFront;
  end else begin
    frmLog.close
  end;
end;

procedure TfrmMain.DoPush;
var
  res: TModalResult;
  cmd, aRemote: string;
  n: Integer;
begin
  //if fConfig.ReadBoolean('FetchBeforePush', false) then
  //  doFetch;
  if fGitMgr.CommitsBehind<0 then begin
    res := QuestionDlg(rsPushingYourCommits, rsThereAreCommitsBehind, mtConfirmation,
      [mrYes, 'Push', mrCancel, 'Cancel'], 0 );
    if res<>mrYes then
      exit;
  end;

  aRemote := '';

  if (fGitMgr.Upstream='') then begin

    if fGitMgr.Remotes=nil then
      fGitMgr.UpdateRemotes;

    if fGitMgr.Remotes=nil then begin
      ShowMessage(rsThisRepositoryHasNoRemotes);
      exit;
    end;

    n := Length(fGitMgr.Remotes);
    if n>1 then begin
      ShowMessageFmt(rsSHasNoTrackingAndThere, [fGitMgr.Branch, n, fGitMgr.RemotesList]);
     exit;
    end;

    aRemote := fGitMgr.Remotes[0].name;

    res := QuestionDlg( rsPushingBranchWithout,
             format(rsDoYouWantToPushSToSA, [fGitMgr.Branch, aRemote]), mtConfirmation,
             [mrYes, rsYesDoIt, mrCancel, rsCancel], 0 );
    if res<>mrYes then
      exit;

    cmd := ' push --progress --set-upstream '+aRemote+' '+fGitMgr.Branch;

  end else
    cmd := ' push --progress';

  RunInteractive(fGit.Exe + cmd, fGit.TopLevelDir, format(rsPushingToRemoteS, [aRemote]), 'Push');
  fGitMgr.UpdateStatus;
  fGitMgr.UpdateRefList;
end;

procedure TfrmMain.DoFetch;
begin
  RunInteractive(fGit.Exe + ' -c color.ui=always fetch', fGit.TopLevelDir, rsFetchingFromRemote, 'Fetch');
  fGitMgr.UpdateStatus;
  fGitMgr.UpdateRefList;
end;

procedure TfrmMain.DoPull;
begin
  RunInteractive(fGit.Exe + ' -c color.ui=always pull', fGit.TopLevelDir, rsPullingFromRemote, 'Pull');
  fGitMgr.UpdateStatus;
  fGitMgr.UpdateRefList;
end;

procedure TfrmMain.DoRepoInfo;
begin
  fGitMgr.UpdateStatus(@OnRepoInfoStatusDone);
end;

procedure TfrmMain.OnCreatePatchClick(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
  lb: TListbox;
  diff: TStringList;
begin
  lb := TListBox(mi.Tag);
  diff := CreatePatchFromListbox(lb);
  //txtDiff.Text := diff.Text;
  if dlgSave.Execute then
    diff.SaveToFile(dlgSave.FileName);
  diff.free;
end;

procedure TfrmMain.OnCopyPatchClick(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
  lb: TListbox;
  diff: TStringList;
begin
  lb := TListBox(mi.Tag);
  diff := CreatePatchFromListbox(lb);
  //txtDiff.Text := diff.Text;
  Clipboard.AsText := diff.Text;
  diff.Free;
end;

procedure TfrmMain.OnCustomCommandClick(Sender: TObject);
var
  c: TComponent absolute Sender;
  cmd: TCustomCmdItem;
  res: TModalResult;
  s: String;
begin
  cmd := fCustomCommands[c.Tag];
  if cmd.Ask then begin
    res := QuestionDlg(
      rsExecutingACustomCommand,
      format(rsYouAreAboutToExecute, [QuotedStr(cmd.description), cmd.command]),
      mtConfirmation, [mrYes, rsYesDoIt, mrCancel, rsCancel], 0 );
    if res<>mrYes then
      exit;
  end;
  if pos('git ', cmd.command)=1 then begin
    s := StringReplace(cmd.command, 'git', fGit.Exe, []);
    if cmd.RunInDlg then
      RunInteractive(s, fGit.TopLevelDir, rsExecutingACustomCommand, cmd.description)
    else
      RunInThread(s, fGit.TopLevelDir, nil, nil);
    if cmd.updatestatus then begin
      fGitMgr.UpdateStatus;
      fGitMgr.UpdateRefList;
    end;
  end;
end;

procedure TfrmMain.OnDebugLoggerInterceptor(Sender: TObject; S: string;
  var Handled: Boolean);
begin
  txtDiff.Lines.Add(s);
  handled := true;
end;

procedure TfrmMain.OnMRECommand(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
  s: string;
begin
  s := StringReplace(mi.Caption, 'git', fGit.Exe, []);
  RunInteractive(s, fGit.TopLevelDir, 'Run a git command', mi.Caption);
end;

procedure TfrmMain.DoGitDiff(Data: PtrInt);
var
  lb: TListbox absolute Data;
  res: Integer;
  entry: PFileEntry;
  unstaged: Boolean;
begin
  if lb.ItemIndex>=0 then begin
    entry := PFileEntry(lb.Items.Objects[lb.ItemIndex]);
    panFileState.Caption := EntryTypeToStr(entry^.x, entry^.y);
    unstaged := lb=lstUnstaged;
    if unstaged and (entry^.EntryTypeUnStaged in [etUntracked, etIgnored]) then begin
      fhlHelper.SetHighlighter(txtDiff, entry^.path);
      ViewFile(fGit.TopLevelDir + entry^.path);
    end else begin
      fhlHelper.SetHighlighter(txtDiff, 'x.diff');
      res := fGit.Diff(entry, unstaged, txtDiff.Lines);
      if res>0 then
        ShowError;
    end;
  end;
end;

procedure TfrmMain.DelayedShowMenu(Data: PtrInt);
begin
  Self.Menu := mnuMain;
end;

procedure TfrmMain.CreateBranch(const binfo: PBranchInfo);
var
  cmdOut: RawByteString;
  needReflistUpdate: boolean;
  needUpdateStatus: boolean;

  procedure BranchError;
  begin
    if binfo^.sender=self then
      ShowError
    else
      ShowMessage(fGit.ErrorLog);
  end;

begin
  try
    needReflistUpdate := false;
    needUpdateStatus := false;

    if fGit.Any('branch ' + binfo^.Command, cmdOut)>0 then begin
      BranchError;
      exit;
    end;

    // there is a new branch, the next time branch list
    // is requested, recreate it
    InvalidateBranchMenu;

    needReflistUpdate := true;

    if not binfo^.Switch then
      exit;
    if fGit.Switch(binfo^.name)>0 then begin
      BranchError;
      exit;
    end;

    needUpdateStatus := true;
    // switched to the new branch, even if fetching from the
    // upstream is not requested, force an update status
    if not binfo^.Fetch then
      exit;
    if fGit.Any('fetch', cmdOut)>0 then begin
      BranchError;
      exit;
    end;

  finally
    dispose(binfo);
    if needUpdateStatus then fGitMgr.UpdateStatus;
    if needReflistUpdate then fGitMgr.UpdateRefList;
  end;

end;

function TfrmMain.CreatePatchFromListbox(const lb: TListbox): TStringList;
var
  i: Integer;
  s: string;
  stagedList: string;
  unstagedList: string;
  untrackedList, list: TStringList;
  entry: PFileEntry;
  isStaged, succeed: boolean;

  procedure DoDiff(cmdfmt: string; args:array of const; isUntracked: boolean);
  var
    cmd: string;
    Code: Integer;
  begin
    cmd := format(cmdfmt, args);
    list.clear;
    Code := fGit.Diff(cmd, list);
    succeed := Code<=0;
    {$ifdef MSWindows}
    // this is crazy, in windows it apparently fails with code=1 when diffing
    // an untracked file, but the output is ok, if we blindly check the code
    // for success it wont work...
    // could it related to the use of /dev/null?
    succeed := succeed or ((isUntracked) and (code=1));
    {$endif}
    if succeed then begin
      //DebugLn(list.Text);
      result.AddStrings(list);
    end else begin
      DebugLn(fGit.ErrorLog);
      DebugLn(list.Text);
    end;
  end;

  procedure Add(var strList:string; what:string);
  begin
    if strList<>'' then strList += ' ';
    strList += what;
  end;

begin

  result := nil;

  isStaged := (lb = lstStaged);

  untrackedList := TStringList.Create;
  list := TStringList.Create;
  try

    unstagedList := '';
    stagedList := '';
    for i := 0 to lb.Count - 1 do begin
      if lb.Selected[i] then begin
        s := lb.Items[i];
        if isStaged then
          Add(stagedList, s)
        else begin
          entry := PFileEntry(lb.Items.Objects[i]);
          if entry^.EntryTypeUnStaged = etUntracked then
            untrackedList.Add(s)
          else
            Add(unstagedList, s);
        end;
      end;
    end;

    result := TStringList.Create;

    if isStaged then
      DoDiff('--cached -- %s',[stagedList], false)
    else begin
      if unstagedList<>'' then
        DoDiff('-- %s',[unstagedList], false);
      for s in untrackedList do begin
        DoDiff('--no-index -- /dev/null %s', [s], true);
        if not succeed then
          break;
      end;
    end;

    if not succeed then
      txtDiff.Text := fGit.ErrorLog;

  finally
    list.Free;
    untrackedList.Free;
  end;
end;

procedure TfrmMain.UpdateBranch;
var
  s: string;
  ahead, behind: boolean;
  i: Integer;
begin
  ahead := fGitMgr.CommitsAhead>0;
  behind := fGitMgr.CommitsBehind<0;

  label1.Visible := not ahead and not behind;
  lblBranch.Caption := fGitMgr.Branch;
  lblBranch.Hint := fGitMgr.Branch + LineEnding + fGitMgr.BranchOID;

  s := '';
  if fGitMgr.Merging then begin
    s += '(';
    if fGitMgr.MergingConflict then s += rsMERGINGCONFLICT
    else                            s += rsMERGING;
    s += ')';
  end;
  lblMerging.Caption := s;

  s := '';
  if ahead then s += format(rsDCommitsAhead, [fGitMgr.CommitsAhead]);
  if ahead and behind then s += ', ';
  if behind then s += format(rsDCommitsBehind, [ - fGitMgr.CommitsBehind]);
  if ahead or behind then s += ' ' + rsOf;

  if s='' then s:=' ';
  lblAheadBehind.Caption := s;

  label2.Visible := (not ahead and not behind) and (fGitMgr.Upstream<>'');
  lblRemote.Caption := fGitMgr.Upstream;

  if fConfig.ShowTags then begin
    if fGitMgr.LastTag='' then begin
      lblTag.Caption := rsNoTagAvailable;
      lblTag.Hint := '';
      label3.Caption := '';
    end else begin
      lblTag.Caption := fGitMgr.LastTag;
      if fGitMgr.LastTagCommits=0 then
        label3.Caption := rsAtTag
      else
        label3.Caption := format(rsDCommitsSince, [fGitMgr.LastTagCommits]);
      lblTag.Hint := fGitMgr.LastTagOID;
    end;
  end else begin
    lblTag.Caption :='';
    lblTag.Hint := '';
    label3.Caption := '';
  end;

  i := fGitMgr.RemoteIndex;
  if i>=0 then begin
    with fGitMgr.Remotes[i] do begin
      if Fetch=Push then
        lblRemote.Hint := format('Url: %s',[Fetch])
      else
        lblRemote.Hint := format('Fetch Url: %s'^M'Push Url: %s', [Fetch, Push]);
    end;
  end else
    lblRemote.Hint := '';

  txtDiff.Clear;

  Caption := 'LazGitGUI v1.0 (git '+fGit.Version+') - ' + fGit.TopLevelDir;
end;

procedure TfrmMain.RestoreGui;
begin
  fConfig.OpenConfig;

  fConfig.ReadWindow(Self, 'mainform', SECTION_GEOMETRY);

  lstUnstaged.Height := fConfig.ReadInteger('lstUnstaged.Height', lstUnstaged.Height, SECTION_GEOMETRY);
  panLeft.Width := fConfig.ReadInteger('panleft.width', panLeft.Width, SECTION_GEOMETRY);
  pancommit.Height := fConfig.ReadInteger('pancommit.height', pancommit.Height, SECTION_GEOMETRY);

  fConfig.CloseConfig;
end;

procedure TfrmMain.SaveGui;
begin
  fConfig.OpenConfig;
  fConfig.WriteWindow(Self, 'mainform', SECTION_GEOMETRY);
  fConfig.WriteInteger('lstUnstaged.Height', lstUnstaged.Height, SECTION_GEOMETRY);
  fConfig.WriteInteger('panleft.width', panLeft.Width, SECTION_GEOMETRY);
  fConfig.WriteInteger('pancommit.height', pancommit.Height, SECTION_GEOMETRY);
  fConfig.CloseConfig;
end;

end.

