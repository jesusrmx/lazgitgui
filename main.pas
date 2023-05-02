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

interface

uses
  Classes, SysUtils, Math, LazLogger, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList, synEditTypes, SynEdit, SynHighlighterDiff,
  StrUtils, FileUtil, lclType, Menus, Buttons, Grids, ComCtrls, Types, fgl,
  unitgittypes, unitifaces, unitconfig, unitprocess, unitentries, unitgitutils, {unitgit,}
  unitnewbranch, unitruncmd, unitansiescapes,
  unitnewtag, unitlogcache, unitlog, LConvEncoding, unitdbindex,
  unitframelog, unitgitmgr, unitcheckouttag;

type

  { TfrmMain }

  TfrmMain = class(TForm, IObserver)
    actCommit: TAction;
    actFetch: TAction;
    actNewLog: TAction;
    actPushDialog: TAction;
    actLog: TAction;
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
    frmLog: TframeLog;
    imgList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblInfo: TLabel;
    lblTag: TLabel;
    lblMerging: TLabel;
    lblRemote: TLabel;
    lblAheadBehind: TLabel;
    lblBranch: TLabel;
    lstUnstaged: TListBox;
    lstStaged: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuMain: TMainMenu;
    panCommitState: TPanel;
    panBranch: TPanel;
    panLogNew: TPanel;
    panPush: TPanel;
    panLog: TPanel;
    panStatus: TPanel;
    panFileState: TPanel;
    panUnstaged: TPanel;
    panStagedContainer: TPanel;
    panStaged: TPanel;
    popBranch: TPopupMenu;
    btnLog: TSpeedButton;
    popLists: TPopupMenu;
    btnStop: TSpeedButton;
    splitterMain: TSplitter;
    SynDiffSyn1: TSynDiffSyn;
    txtLog: TSynEdit;
    txtComment: TMemo;
    panLeft: TPanel;
    panContent: TPanel;
    panCommit: TPanel;
    panCommitButtons: TPanel;
    splitterStaged: TSplitter;
    splitterCommit: TSplitter;
    txtDiff: TSynEdit;
    procedure actCommitExecute(Sender: TObject);
    procedure actFetchExecute(Sender: TObject);
    procedure actLogExecute(Sender: TObject);
    procedure actNewLogExecute(Sender: TObject);
    procedure actPullExecute(Sender: TObject);
    procedure actPushDialogExecute(Sender: TObject);
    procedure actPushExecute(Sender: TObject);
    procedure actQuitExecute(Sender: TObject);
    procedure actRescanExecute(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblBranchClick(Sender: TObject);
    procedure lblBranchContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure lblInfoClick(Sender: TObject);
    procedure lstUnstagedContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure lstUnstagedDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lstUnstagedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fGitMgr: TGitMgr;
    fGit: IGit;
    fClickedIndex: Integer;
    fDir: string;
    fItemIndices: TItemIndexArray;
    fLogHandler: TLogHandler;
    fPopPoint: TPoint;
    fListAlwaysDrawSelection: boolean;
    procedure DelayedShowMenu(Data: PtrInt);
    procedure DoGitDiff(Data: PtrInt);
    procedure DoItemAction(Data: PtrInt);
    procedure DoCommit;
    procedure DoLog;
    procedure DoNewLog;
    procedure DoPush;
    procedure DoFetch;
    procedure DoPull;
    procedure OnLogEvent(sender: TObject; thread: TRunThread; event: Integer;
      var interrupt: boolean);
    procedure OnLogCacheEvent(sender: TObject; thread: TLogThread; event: Integer;
      var interrupt: boolean);
    procedure OnPopupItemClick(Sender: TObject);
    procedure OnBranchSwitch(Data: PtrInt);
    procedure OnIgnoreFileClick(Sender: TObject);
    procedure OnIgnoreTypeClick(Sender: TObject);
    procedure OnReloadBranchMenu(Data: PtrInt);
    procedure OnRestoreFileClick(Sender: TObject);
    procedure OnStageAllClick(Sender: TObject);
    procedure OnStageItemClick(Sender: TObject);
    procedure OnUnstageItemClick(Sender: TObject);
    procedure OpenDirectory(aDir: string);
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
    procedure FindParents;
    procedure UpdateGridRows;
    procedure ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
    procedure ShowNewTagForm(commit: string);
    procedure ShowSwitchToTagForm(aTag: string);
    procedure SwitchTo(cmd: string);
  public

  end;

var
  frmMain: TfrmMain;
  targetDir: string = '';

implementation

{$R *.lfm}

resourcestring
  rsNewBranch = 'New Branch';
  rsNewTag = 'Create a tag at this point';
  rsReload = 'Reload';
  rsPushingYourCommits = 'Pushing your commits';
  rsThereAreCommitsBehind = 'There are commits behind, are you sure you want to push?';
  rsRestoringWorkFiles = 'Restoring working files';
  rsRestoringWorkFilesWarning =
    'You are about to discard changes in %s,'^M^M+
    'data will be lost and this action cannot be undone'^M^M+
    'Are you sure to continue?';




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
  MENU_LIST_STAGE_SELECTION   = 20;
  MENU_LIST_UNSTAGE_SELECTION = 21;

  LIST_TAG_ALL_SELECTED       = -1;
  LIST_TAG_ALL_CHANGED        = -2;

  VIEWER_BUFSIZE      = 1024*4;
  BIN_BUFSIZE         = 1024;


function AddPopItem(pop: TPopupMenu; caption:string; onClick:TNotifyEvent; tag: Integer): TMenuItem;
begin
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

{ TfrmMain }

procedure TfrmMain.FormShow(Sender: TObject);
begin
  OpenDirectory(targetDir);
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
  if fGit.AddToIgnoreFile(ignored, false, true) then
    fGitMgr.UpdateStatus
  else
    ShowMessage(Format('''%s'' is already in ignored list',[ignored]));
end;

procedure TfrmMain.OnIgnoreTypeClick(Sender: TObject);
var
  ignored: string;
  mi: TMenuItem absolute Sender;
begin
  // what file?
  ignored := lstUnstaged.Items[mi.Tag];
  if fGit.AddToIgnoreFile(ignored, true, true) then
    fGitMgr.UpdateStatus
  else
    ShowMessage(Format('The type ''*%s'' is already in the ignored list',[ExtractFileExt(ignored)]));
end;

procedure TfrmMain.OnReloadBranchMenu(Data: PtrInt);
begin
  UpdateBranchMenu;
  popBranch.PopUp(fPopPoint.x, fPopPoint.y);
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
    aFile := format('%d files',[Length(entryArray)]);

  // this is necessary because we normally hide lists selection
  // when they are unfocused, but in this case the next dialog
  // will unfocus the lists and we want to see the selection of
  // files that will be restored.
  fListAlwaysDrawSelection := true;

  res := QuestionDlg(rsRestoringWorkFiles, format(rsRestoringWorkFilesWarning, [aFile]), mtWarning,
    [mrYes, 'Discard changes', mrCancel, 'Cancel'], 0 );

  fListAlwaysDrawSelection := false;

  if res<>mrYes then
    exit;

  {$IFDEF DEBUG}
  aFile := MakePathList(entryArray);
  DebugLn('Restoring: ',aFile);
  {$ENDIF}

  if fGit.Restore(entryArray, false)>0 then
    ShowError
  else
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
    if fGit.Any(cmd, cmdOut)>0 then
      ShowError
    else begin
      fGitMgr.UpdateStatus;
      //if cmdOut<>'' then
      //  txtDiff.Text := cmdOut;
    end;
  end;
end;

procedure TfrmMain.OnStageItemClick(Sender: TObject);
var
  mi: TMenuItem;
  entryArray: TPFileEntryArray;
begin
  mi := TMenuItem(Sender);
  entryArray := MakeMenuItemUnstagedEntryArray(mi);

  if fGit.Add(entryArray)>0 then
    ShowError
  else
    fGitMgr.UpdateStatus;
end;

procedure TfrmMain.OnUnstageItemClick(Sender: TObject);
var
  mi: TMenuItem;
  entryArray: TPFileEntryArray;
begin
  mi := TMenuItem(Sender);
  entryArray := MakeMenuItemStagedEntryArray(mi);

  if fGit.Restore(entryArray, true)>0 then
    ShowError
  else
    fGitMgr.UpdateStatus;
end;

procedure TfrmMain.lblBranchContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

  if popBranch.Items.Count>0 then
    exit;

  UpdateBranchMenu;
end;

procedure TfrmMain.lblInfoClick(Sender: TObject);
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
      mi := AddPopItem(popLists, 'View Untracked Files', @OnPopupItemClick, MENU_LIST_VIEW_UNTRACKED);
      mi.AutoCheck := true;
      mi.Checked := fConfig.ViewUntrackedFiles;
      mi := AddPopItem(popLists, 'View Ignored Files', @OnPopupItemClick, MENU_LIST_VIEW_IGNORED);
      mi.AutoCheck := true;
      mi.Checked := fConfig.ViewIgnoredFiles;
      mi := AddPopItem(popLists, 'View Tracked Files', @OnPopupItemClick, MENU_LIST_VIEW_TRACKED);
      mi.AutoCheck := true;
      mi.Checked := fConfig.ViewTrackedFiles;
    end;
  end;

  procedure AddStageFile;
  begin
    AddPopItem(popLists, format('Stage ''%s''',[aFile]), @OnStageItemClick, aIndex);
  end;

  procedure AddUnstageFile;
  begin
    AddPopItem(popLists, format('Unstage ''%s''',[aFile]), @OnUnstageItemClick, aIndex);
  end;

  procedure AddStageAll;
  begin
    AddPopItem(popLists, 'Stage Changed', @OnStageAllClick, MENU_LIST_STAGE_CHANGED);
    AddPopItem(popLists, 'Stage All', @OnStageAllClick, MENU_LIST_STAGE_ALL);
  end;

  procedure AddUnstageAll;
  begin
    AddPopItem(popLists, 'Unstage All', @OnStageAllClick, MENU_LIST_UNSTAGE_ALL);
  end;

  procedure AddIgnoreUntracked;
  var
    ext: string;
  begin
    AddPopItem(popLists, '-', nil, 0);
    AddPopItem(popLists, format('Add ''%s'' to ignore list', [aFile]), @OnIgnoreFileClick, aIndex);
    ext :=ExtractFileExt(aFile);
    if ext<>'' then
      AddPopItem(popLists, format('Add ''*%s'' Files to ignore list', [ext]), @OnIgnoreTypeClick, aIndex);
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
        AddPopItem(popLists, format('Restore ''%s''', [aFile]), @OnRestoreFileClick, aIndex);
      // we need an option for all changed
      n := CountListItemsOfEntryType(lstUnstaged, true, false, ChangedInWorktreeSet);
      if n > 0 then
        AddPopItem(popLists, 'Restore All Changed', @OnRestoreFileClick, LIST_TAG_ALL_CHANGED);
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
      if Entry^.EntryKind in [ekUntracked, ekIgnored] then
        AddIgnoreUntracked;
      AddRestoreFiles;
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

    if selCount=1 then begin
      aFile := ExtractFileName(lb.GetSelectedText);
      aIndex := GetSelectedIndex(lb);
      Entry := PFileEntry(lb.Items.Objects[aIndex])
    end else begin
      aFile := format('%d files',[selCount]);
      aIndex := LIST_TAG_ALL_SELECTED;
      Entry := nil;
    end;

    if isUnstaged then begin
      AddStageFile;
      AddStageAll;

      if (Entry<>nil) and (Entry^.EntryKind in [ekUntracked, ekIgnored]) then
        AddIgnoreUntracked;

      AddRestoreFiles;

      AddPopItem(popLists, '-', nil, 0);
      AddViewItems;
    end else begin
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
      etUntracked,
      etWorktreeChangedSinceIndex..etTypeChangedInWorktreeSinceIndexC:
        begin
          if fGit.Add(Entry)>0 then
            ShowError
          else
            fGitMgr.UpdateStatus;
        end;
      etDeletedInWorktree:
        begin
          if fGit.Rm(Entry)>0 then
            ShowError
          else
            fGitMgr.UpdateStatus;
        end;
      // other merge conflicts:
      // see:
      //etUnmergedDeletedByUs:
      //  begin
      //  end
      else
        ShowMessage('Not yet implemented for Unstaged: '+cmdOut);
    end;
  end else begin
    WriteStr(cmdOut, Entry^.EntryTypeStaged);
    case Entry^.EntryTypeStaged of
      etAddedToIndex..etAddedToIndexD,
      etUpdatedInIndex..etTypeChangedInIndexD,
      etRenamedInIndex..etRenamedInIndexD,
      etDeletedFromIndex:
        begin
          if fGit.Restore(entry, true)>0 then
            ShowError
          else
            fGitMgr.UpdateStatus;
        end;
      else
        ShowMessage('Not yet implemented for Staged: '+cmdOut);
    end;
  end;
end;

procedure TfrmMain.UpdateBranchMenu;
var
  list, branchLine: TStringList;
  n, i: integer;
  p, q: pchar;
  mi: TMenuItem;
begin
  InvalidateBranchMenu;

  AddPopItem(popBranch, rsNewBranch, @OnPopupItemClick, MENU_BRANCH_NEW);
  AddPopItem(popBranch, rsNewTag, @OnPopupItemClick, MENU_BRANCH_NEW_TAG);
  AddPopItem(popBranch, rsReload, @OnPopupItemClick, MENU_BRANCH_RELOAD);
  AddPopItem(popBranch, '-', nil, MENU_INVALID);

  try
    list := TStringList.Create;

    if fGit.BranchList(list, [
        '%(refname:short)',
        '%(objecttype)',
        '%(upstream:short)',
        '%(HEAD)'])>0 then
    begin
      ShowError;
      exit;
    end;

    try
      branchLine := TStringList.Create;
      branchLine.StrictDelimiter := true;
      branchLine.Delimiter := '|';

      for i:=0 to list.Count-1 do begin
        BranchLine.DelimitedText := list[i];
        if pos('/', branchLine[0])<>0 then
          continue;

        mi := AddPopItem(popBranch, branchLine[0], @OnPopupItemClick, MENU_BRANCH_SWITCH);
        mi.GroupIndex := 1;
        mi.AutoCheck := true;
        mi.Checked := branchLine[3]='*';
        if mi.Checked then
          mi.OnClick := nil;
        mi.RadioItem := true;

      end;

    finally
      branchLine.free;
    end;
  finally
    list.Free;
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
    StoreString(format('%s does not exists', [filename]));
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
    StoreString(format('The file ''%s'' is binary, %d bytes',[filename, F.Size]));
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
  ShowMessage('This feature will be implemented ASAP');
end;

function TfrmMain.MakeMenuItemUnstagedEntryArray(mi: TMenuItem): TPFileEntryArray;
var
  aIndex: PtrInt;
  i, n: Integer;
  entry: PFileEntry;
begin
  n := 0;
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
  cmdOut: RawByteString;
begin
  f := TfrmNewBranch.Create(Self);
  f.GitMgr := fGitMgr;
  try
    if f.ShowModal=mrOk then begin

      if fGit.Any('branch ' + f.GetBranchCommandOptions, cmdOut)>0 then begin
        ShowError;
        exit;
      end;

      // there is a new branch, the next time branch list
      // is requested, recreate it
      InvalidateBranchMenu;

      if not f.Switch then
        exit;
      if fGit.Switch(f.BranchName)>0 then begin
        ShowError;
        exit;
      end;

      // switched to the new branch, even if fetching from the
      // upstream is not requested, force an update status
      try
        if not f.Fetch then
          exit;
        if fGit.Any('fetch', cmdOut)>0 then begin
          ShowError;
          exit;
        end;
      finally
        fGitMgr.UpdateStatus;
        fGitMgr.UpdateRefList;
      end;

    end;
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
  fGitMgr.QueueNewTag(fGit.BranchOID);
end;

procedure TfrmMain.CheckMenuDivisorInLastPosition(pop: TPopupMenu);
begin
  if pop.Items.Count>1 then begin
    if pop.Items[pop.Items.Count-1].Caption='-' then
      pop.Items.Delete(pop.Items.Count-1);
  end;
end;

procedure TfrmMain.FindParents;
var
  parents: TParentsArray;
begin

end;

procedure TfrmMain.UpdateGridRows;
begin

end;

procedure TfrmMain.ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
var
  info: PTagInfo;
begin
  case what of
    GITMGR_EVENT_UpdateStatus:
      begin
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
        info := PTagInfo(data);
        ShowNewTagForm(info^.data);
        Finalize(info^.data);
        dispose(info);
      end;
    GITMGR_EVENT_SWITCHTOTAG:
      begin
        info := PTagInfo(data);
        ShowSwitchToTagForm(info^.data);
        finalize(info^.data);
        dispose(info);
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
  fConfig.ReadFont(txtLog.Font, 'log', fpFixed, SECTION_FONTS);

  panFileState.Caption := '';

  RestoreGui;

  txtLog.Color := clBlack;
  txtLog.Font.Color := clWhite;

  frmLog.Config := fConfig;
  frmLog.GitMgr := fGitMgr;
  frmLog.OnLogCacheEvent := @OnLogCacheEvent;

  fLogHandler := TLogHandler.Create(txtLog, @OnLogEvent);
  fLogHandler.GitMgr := fGitMgr;

  fConfig.ReadPreferences;
  fGitMgr.ViewUntrackedFiles := fConfig.ViewUntrackedFiles;
  fGitMgr.ViewIgnoredFiles := fConfig.ViewIgnoredFiles;
  fGitMgr.ViewTrackedFiles := fConfig.ViewTrackedFiles;
  fGitMgr.ShowTags := fConfig.ShowTags;

  if fConfig.ReadBoolean('NeedsMenuWorkaround') then begin
    Self.Menu := nil;
    Application.QueueAsyncCall(@DelayedShowMenu, 0);
  end;

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

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  btnStop.Tag := 1;
end;

procedure TfrmMain.actCommitExecute(Sender: TObject);
begin
  DoCommit;
end;

procedure TfrmMain.actFetchExecute(Sender: TObject);
begin
  DoFetch;
end;

procedure TfrmMain.actLogExecute(Sender: TObject);
begin
  DoLog;
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
begin
  ComingSoon;
end;

procedure TfrmMain.actPushExecute(Sender: TObject);
begin
  DoPush;
end;

procedure TfrmMain.actQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fGitMgr.RemoveObserver(self);
  frmLog.Clear;
  fLogHandler.Free;
  fGitMgr.Free;
end;

procedure TfrmMain.OpenDirectory(aDir: string);
begin
  aDir := ExpandFileName(aDir);
  fGit.OpenDir(aDir);
  if fGit.TopLevelDir='' then begin
    ShowMessage('Couldn''t get toplevel directory of '+aDir);
    Application.Terminate;
    exit;
  end;
  fDir := aDir;
  fGitMgr.UpdateStatus;
end;

procedure TfrmMain.DoItemAction(Data: PtrInt);
begin
  ItemAction(TListBox(Data), fClickedIndex);
end;

procedure TfrmMain.DoCommit;
begin
  if lstStaged.Count=0 then begin
    ShowMessage('You have to stage something in order to commit');
    exit;
  end;
  if fGit.Commit(txtComment.Text,'')>0 then
    ShowError
  else begin
    fGitMgr.ForceTagDescription;
    fGitMgr.UpdateStatus;
    txtDiff.Clear;
    txtComment.Clear;
  end;
end;

procedure TfrmMain.DoLog;
var
  cmd: string;
begin
  if actLog.Checked then begin
    panLog.Visible := true;
    panStatus.Visible := false;
    panLogNew.Visible := false;
    btnStop.Visible := true;
    btnStop.Tag := 0;

    fLogHandler.ShowLog;

  end else begin
    panLog.Visible := false;
    panLogNew.Visible := false;
    panStatus.Visible := true;
  end;
end;

procedure TfrmMain.DoNewLog;
var
  cmd: string;
begin
  //CacheRefs;
  //exit;

  if actNewLog.Checked then begin
    panLog.Visible := false;
    panLogNew.Visible := true;
    panStatus.Visible := false;

    frmLog.GitMgr := fGitMgr;
    frmLog.Active := true;

  end else begin

    frmLog.GitMgr := nil;

    panLog.Visible := false;
    panLogNew.Visible := false;
    panStatus.Visible := true;
  end;
end;

procedure TfrmMain.DoPush;
var
  res: TModalResult;
  L: TStringList;
  cmd: string;
begin
  //if fConfig.ReadBoolean('FetchBeforePush', false) then
  //  doFetch;
  if fGit.CommitsBehind<0 then begin
    res := QuestionDlg(rsPushingYourCommits, rsThereAreCommitsBehind, mtConfirmation,
      [mrYes, 'Push', mrCancel, 'Cancel'], 0 );
    if res<>mrYes then
      exit;
  end;

  if (fGit.Upstream='') then begin
    L := fGit.GetRemotesList;
    try
      if L.Count=0 then begin
        ShowMessage('This repository has no remotes defined'^M+
                    'I''m not yet prepared to handle this');
        exit;
      end;
      if L.Count>1 then begin
        ShowMessage(fGit.Branch + ' has no tracking and there are '^M+
                    IntToStr(l.Count)+' remotes ('+L.CommaText+')'^M+
                    'I''m not yet prepared to handle this');
       exit;
      end;

      res := QuestionDlg(
        'Pushing branch without tracking information',
        'Do you want to push "'+fGit.Branch+'" to "'+L[0]+'"'+LineEnding+
        'And setup tracking information? I will do:'^M+LineEnding+LineEnding+
        'git push --set-upstream '+L[0]+' '+fGit.Branch, mtConfirmation,
        [mrYes, 'yes, do it', mrCancel, 'Cancel'], 0 );
      if res<>mrYes then
        exit;

      cmd := ' push --progress --set-upstream '+L[0]+' '+fGit.Branch;

    finally
      L.Free;
    end;

  end else
    cmd := ' push --progress';

  RunInteractive(fGit.Exe + cmd, fGit.TopLevelDir, 'Pushing to remote: ', 'Push');
  fGitMgr.UpdateStatus;
end;

procedure TfrmMain.DoFetch;
begin
  RunInteractive(fGit.Exe + ' -c color.ui=always fetch', fGit.TopLevelDir, 'Fetching from remote: ', 'Fetch');
  fGitMgr.UpdateStatus;
end;

procedure TfrmMain.DoPull;
begin
  RunInteractive(fGit.Exe + ' -c color.ui=always pull', fGit.TopLevelDir, 'pulling from remote: ', 'Pull');
  fGitMgr.UpdateStatus;
end;

procedure TfrmMain.OnLogEvent(sender: TObject; thread: TRunThread;
  event: Integer; var interrupt: boolean);
begin
  case event of
    LOGEVENT_OUTPUT:
      interrupt := btnStop.Visible and (btnStop.Tag=1);

    LOGEVENT_DONE:
      btnStop.Visible := false;
  end;
end;

procedure TfrmMain.OnLogCacheEvent(sender: TObject; thread: TLogThread;
  event: Integer; var interrupt: boolean);
begin
  case event of

    LOGEVENT_START:
      begin
        btnStop.Visible := true;
        btnStop.Tag := 0;
      end;

    LOGEVENT_RECORD:
      begin
        if frmLog.LogCache.LogState=lsGetFirst then lblInfo.Font.Color := clGreen
        else                                  lblInfo.Font.Color := clRed;
        lblInfo.Caption := format('%s',[frmLog.LogCache.DbIndex.Info]);
        lblInfo.Visible := true;
        interrupt := btnStop.Visible and (btnStop.Tag=1);
      end;

    LOGEVENT_END:
      begin
        btnStop.Visible := false;
        lblInfo.Visible := false;
      end;

    LOGEVENT_DONE:
      begin
      end;
  end;
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
    if unstaged and (entry^.EntryTypeUnStaged in [etUntracked, etIgnored]) then
      ViewFile(fGit.TopLevelDir + entry^.path)
    else begin
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

procedure TfrmMain.UpdateBranch;
var
  s: string;
  ahead, behind: boolean;
begin
  ahead := fGit.CommitsAhead>0;
  behind := fGit.CommitsBehind<0;

  label1.Visible := not ahead and not behind;
  lblBranch.Caption := fGit.Branch;
  lblBranch.Hint := fGit.Branch + LineEnding + fGit.BranchOID;

  s := '';
  if fGit.Merging then begin
    s += '(MERGING';
    if fGit.MergingConflict then s += ' CONFLICT';
    s += ')';
  end;
  lblMerging.Caption := s;

  s := '';
  if ahead then s += format('%d commits ahead', [fGit.CommitsAhead]);
  if ahead and behind then s += ', ';
  if behind then s += format('%d commits behind', [-fGit.CommitsBehind]);
  if ahead or behind then s += ' of';

  if s='' then s:=' ';
  lblAheadBehind.Caption := s;

  label2.Visible := (not ahead and not behind) and (fGit.Upstream<>'');
  lblRemote.Caption := fGit.Upstream;

  if fConfig.ShowTags then begin
    if fGit.LastTag='' then begin
      lblTag.Caption :='No Tag available';
      lblTag.Hint := '';
      label3.Caption := '';
    end else begin
      lblTag.Caption := fGit.LastTag;
      if fGit.LastTagCommits=0 then
        label3.Caption := 'At tag'
      else
        label3.Caption := format('%d commits since',[fGit.LastTagCommits]);
      lblTag.Hint := fGit.LastTagOID;
    end;
  end else begin
    lblTag.Caption :='';
    lblTag.Hint := '';
    label3.Caption := '';
  end;


  txtDiff.Clear;

  Caption := 'LazGitGUI - [git '+fGit.Version+'](' + fGit.TopLevelDir + ')';
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
var
  isMaximized: Boolean;
begin
  fConfig.OpenConfig;

  fConfig.WriteWindow(Self, 'mainform', SECTION_GEOMETRY);
  fConfig.WriteInteger('lstUnstaged.Height', lstUnstaged.Height, SECTION_GEOMETRY);
  fConfig.WriteInteger('panleft.width', panLeft.Width, SECTION_GEOMETRY);
  fConfig.WriteInteger('pancommit.height', pancommit.Height, SECTION_GEOMETRY);

  fConfig.CloseConfig;
end;

end.

