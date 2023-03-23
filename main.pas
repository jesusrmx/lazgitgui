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
{.$define CaptureOutput}

interface

uses
  Classes, SysUtils, Math, LazLogger, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList, synEditTypes, SynEdit, SynHighlighterDiff,
  StrUtils, FileUtil, unitconfig, unitprocess, unitentries, unitgit, Types,
  lclType, Menus, Buttons, unitnewbranch, unitruncmd, unitansiescapes,
  LConvEncoding;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actCommit: TAction;
    actFetch: TAction;
    actLog: TAction;
    actQuit: TAction;
    actPull: TAction;
    actPush: TAction;
    actRescan: TAction;
    ActionList1: TActionList;
    btnRescan: TButton;
    btnStageChanged: TButton;
    btnSignOff: TButton;
    btnCommit: TButton;
    btnPush: TButton;
    imgList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
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
    panLog: TPanel;
    panStatus: TPanel;
    panFileState: TPanel;
    panUnstaged: TPanel;
    panStagedContainer: TPanel;
    panStaged: TPanel;
    popBranch: TPopupMenu;
    btnLog: TSpeedButton;
    popLists: TPopupMenu;
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
    procedure actPullExecute(Sender: TObject);
    procedure actPushExecute(Sender: TObject);
    procedure actQuitExecute(Sender: TObject);
    procedure actRescanExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblBranchClick(Sender: TObject);
    procedure lblBranchContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure lstUnstagedContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure lstUnstagedDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lstUnstagedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fGit: TGit;
    fClickedIndex: Integer;
    fDir: string;
    fPopPoint: TPoint;
    fAnsiHandler: TAnsiEscapesHandler;
    fListAlwaysDrawSelection: boolean;
    {$IFDEF CaptureOutput}
    fCap: TMemoryStream;
    {$ENDIF}
    procedure DoGitDiff(Data: PtrInt);
    procedure DoItemAction(Data: PtrInt);
    procedure DoCommit;
    procedure DoLog;
    procedure DoPush;
    procedure DoFetch;
    procedure DoPull;
    procedure OnBranchMenuClick(Sender: TObject);
    procedure OnBranchSwitch(Data: PtrInt);
    procedure OnIgnoreFileClick(Sender: TObject);
    procedure OnIgnoreTypeClick(Sender: TObject);
    procedure OnLogDone(Sender: TObject);
    procedure OnLogOutput(sender: TObject; var interrupt: boolean);
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
    procedure UpdateStatus;
    procedure ShowError;
    procedure ViewFile(filename: string);
    procedure ComingSoon;
    function MakeMenuItemUnstagedEntryArray(mi: TMenuItem): TPFileEntryArray;
  public

  end;

var
  frmMain: TfrmMain;
  targetDir: string = '';

implementation

{$R *.lfm}

resourcestring
  rsNewBranch = 'New Branch';
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

  MENU_LIST_VIEW_UNTRACKED    = 4;
  MENU_LIST_VIEW_IGNORED      = 5;
  MENU_LIST_VIEW_UNCHANGED    = 6;
  MENU_LIST_STAGE_CHANGED     = 7;
  MENU_LIST_STAGE_ALL         = 8;
  MENU_LIST_UNSTAGE_ALL       = 9;
  MENU_LIST_STAGE_SELECTION   = 10;
  MENU_LIST_UNSTAGE_SELECTION = 11;

  LIST_TAG_ALL_SELECTED           = -1;
  LIST_TAG_ALL_CHANGED            = -2;


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

procedure TfrmMain.OnBranchMenuClick(Sender: TObject);
var
  mi: TMenuItem;
  f: TfrmNewBranch;
begin
  mi := TMenuItem(sender);

  case mi.tag of
    MENU_BRANCH_NEW:
      begin
        f := TfrmNewBranch.Create(Self);
        f.Git := fGit;
        try
          if f.ShowModal=mrOk then begin
            ShowMessage('Creating a branch');
          end;
        finally
          f.Free;
        end;
      end;
    MENU_BRANCH_RELOAD:
      begin
        fPopPoint := popBranch.PopupPoint;
        Application.QueueAsyncCall(@OnReloadBranchMenu, 0);
      end;
    MENU_BRANCH_SWITCH:
      begin
        Application.QueueAsyncCall(@OnBranchSwitch, PtrInt(Sender));
      end;

    else
      ComingSoon;
  end;
end;

procedure TfrmMain.OnBranchSwitch(Data: PtrInt);
var
  mi: TMenuItem;
begin
  mi := TMenuItem(TObject(Data));
  if fGit.Switch(mi.Caption)>0 then
    ShowError
  else
    UpdateStatus;
end;

procedure TfrmMain.OnIgnoreFileClick(Sender: TObject);
var
  ignored: string;
  mi: TMenuItem absolute Sender;
begin
  // what file?
  ignored := lstUnstaged.Items[mi.Tag];
  if fGit.AddToIgnoreFile(ignored, false, true) then
    UpdateStatus
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
    UpdateStatus
  else
    ShowMessage(Format('The type ''*%s'' is already in the ignored list',[ExtractFileExt(ignored)]));
end;

procedure TfrmMain.OnLogDone(Sender: TObject);
var
  thread: TRunThread absolute Sender;
begin
  {$IFDEF CaptureOutput}
  fCap.SaveToFile('colorido.bin');
  fCap.Free;
  {$endif}
end;

procedure TfrmMain.OnLogOutput(sender: TObject; var interrupt: boolean);
var
  thread: TRunThread absolute sender;
begin
  {$IFDEF CaptureOutput}
  if thread.Line<>'' then
    fCap.WriteBuffer(thread.Line[1], Length(thread.Line));
  fCap.WriteBuffer(thread.LineEnding[1], Length(thread.LineEnding));
  {$ENDIF}
  fAnsiHandler.ProcessLine(thread.Line, thread.LineEnding);
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
  list, aFile: String;
  res: TModalResult;
begin
  mi := TMenuItem(Sender);
  entryArray := MakeMenuItemUnstagedEntryArray(mi);
  list := MakePathList(entryArray, false);

  if Length(entryArray)=1 then
    aFile := list
  else
    aFile := format('%d files',[Length(entryArray)]);

  // this is necessary because we normally hide lists selection
  // when they are unfocused, but in this case the next dialog
  // will unfocus the lists and we want to see what files that
  // are to be restored.
  fListAlwaysDrawSelection := true;

  res := QuestionDlg(rsRestoringWorkFiles, format(rsRestoringWorkFilesWarning, [aFile]), mtWarning,
    [mrYes, 'Discard changes', mrCancel, 'Cancel'], 0 );
  fListAlwaysDrawSelection := false;
  if res<>mrYes then
    exit;

  list := MakePathList(entryArray);

  DebugLn('Restoring: ',list);
  //if fGit.Restore(entryArray, false)>0 then
  //  ShowError
  //else
  //  UpdateStatus;
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
      UpdateStatus;
      if cmdOut<>'' then
        txtDiff.Text := cmdOut;
    end;
  end;
end;

procedure TfrmMain.OnStageItemClick(Sender: TObject);
var
  mi: TMenuItem;
  i, aIndex: Integer;
  entryArray: TPFileEntryArray;
begin
  mi := TMenuItem(Sender);
  entryArray := MakeMenuItemUnstagedEntryArray(mi);

  if fGit.Add(entryArray)>0 then
    ShowError
  else
    UpdateStatus;
end;

procedure TfrmMain.OnUnstageItemClick(Sender: TObject);
begin
  ComingSoon;
end;

procedure TfrmMain.lblBranchContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

  if popBranch.Items.Count>0 then
    exit;

  UpdateBranchMenu;
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
      mi := AddPopItem(popLists, 'View Untracked Files', @OnBranchMenuClick, MENU_LIST_VIEW_UNTRACKED);
      mi.AutoCheck := true;
      mi.Checked := fConfig.ReadBoolean('ViewUntracked', true);
      mi := AddPopItem(popLists, 'View Ignored Files', @OnBranchMenuClick, MENU_LIST_VIEW_IGNORED);
      mi.AutoCheck := true;
      mi.Checked := fConfig.ReadBoolean('ViewIgnored');
      mi := AddPopItem(popLists, 'View Unchanged Files', @OnBranchMenuClick, MENU_LIST_VIEW_UNCHANGED);
      mi.AutoCheck := true;
      mi.Checked := fConfig.ReadBoolean('ViewChanged');
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

  if popLists.Items.Count>1 then begin
    if popLists.Items[popLists.Items.Count-1].Caption='-' then
      popLists.Items.Delete(popLists.Items.Count-1);
  end;

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
            UpdateStatus;
        end;
      etDeletedInWorktree:
        begin
          if fGit.Rm(Entry)>0 then
            ShowError
          else
            UpdateStatus;
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
            UpdateStatus;
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
  popBranch.Items.Clear;

  AddPopItem(popBranch, rsNewBranch, @OnBranchMenuClick, MENU_BRANCH_NEW);
  AddPopItem(popBranch, rsReload, @OnBranchMenuClick, MENU_BRANCH_RELOAD);

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

        if popBranch.Items.Count=2 then
          AddPopItem(popBranch, '-', nil, MENU_INVALID);

        mi := AddPopItem(popBranch, branchLine[0], @OnBranchMenuClick, MENU_BRANCH_SWITCH);
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
  end;
end;

procedure TfrmMain.UpdateStatus;
begin
  lstUnstaged.Items.BeginUpdate;
  lstStaged.Items.BeginUpdate;
  try
    if fGit.Status(lstUnstaged.Items, lstStaged.Items)>0 then
      ShowError
    else
      UpdateBranch;
  finally
    lstStaged.Items.EndUpdate;
    lstUnstaged.Items.EndUpdate;
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
var
  s, v: string;
  aQuality: TFontQuality;
begin
  fGit := TGit.Create;

  fConfig.OpenConfig;

  s := ''; v := '';
  if Application.HasOption('git') then
    s := Application.GetOptionValue('git');
  if s='' then begin
    s := fConfig.ReadString('git');
    v := fConfig.ReadString('gitversion');
  end;

  fGit.SetupExe(s, v);

  if fGit.Exe='' then begin
    fConfig.CloseConfig;
    DebugLn('Error: Could not find git command');
    Application.Terminate;
    exit;
  end;

  if (fGit.Exe<>s) or (fGit.Version<>v) then begin
    fConfig.WriteString('git', fGit.Exe);
    fConfig.WriteString('gitversion', fGit.Version);
  end;

  //DebugLn('git=', fGit.Exe);

  txtDiff.Clear;

  fConfig.ReadFont(txtDiff.Font, 'viewer', fpFixed, SECTION_FONTS);
  fConfig.ReadFont(txtLog.Font, 'log', fpFixed, SECTION_FONTS);

  panFileState.Caption := '';

  RestoreGui;

  txtLog.Color := clBlack;
  txtLog.Font.Color := clWhite;
  fAnsiHandler := TAnsiEscapesHandler.Create(txtLog);

  fConfig.CloseConfig;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveGui;
end;

procedure TfrmMain.actRescanExecute(Sender: TObject);
begin
  UpdateStatus;
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

procedure TfrmMain.actPullExecute(Sender: TObject);
begin
  DoPull;
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
  fAnsiHandler.Free;
  fGit.Free;
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
  UpdateStatus;
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
    UpdateStatus;
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
    txtLog.Clear;
    fAnsiHandler.Reset;
    {$IFDEF CaptureOutput}
    fCap := TMemoryStream.Create;
    {$ENDIF}
    cmd :=  fGit.Exe + ' ' +
            '-c color.ui=always ' +
            'log --oneline --graph --decorate --all';
    RunInThread(cmd, fGit.TopLevelDir, @OnLogOutput, @OnLogDone, true);
  end else begin
    panLog.Visible := false;
    panStatus.Visible := true;
  end;
end;

procedure TfrmMain.DoPush;
var
  res: TModalResult;
begin
  //if fConfig.ReadBoolean('FetchBeforePush', false) then
  //  doFetch;
  if fGit.CommitsBehind<0 then begin
    res := QuestionDlg(rsPushingYourCommits, rsThereAreCommitsBehind, mtConfirmation,
      [mrYes, 'Push', mrCancel, 'Cancel'], 0 );
    if res<>mrYes then
      exit;
  end;

  RunInteractive(fGit.Exe + ' push --progress', fGit.TopLevelDir, 'Pushing to remote: ', 'Push');
  UpdateStatus;
end;

procedure TfrmMain.DoFetch;
begin
  RunInteractive(fGit.Exe + ' -c color.ui=always fetch', fGit.TopLevelDir, 'Fetching from remote: ', 'Fetch');
  UpdateStatus;
end;

procedure TfrmMain.DoPull;
begin
  RunInteractive(fGit.Exe + ' -c color.ui=always pull', fGit.TopLevelDir, 'pulling from remote: ', 'Pull');
  UpdateStatus;
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

procedure TfrmMain.UpdateBranch;
var
  s: string;
  ahead, behind: boolean;
begin
  ahead := fGit.CommitsAhead>0;
  behind := fGit.CommitsBehind<0;

  label1.Visible := not ahead and not behind;
  lblBranch.Caption := fGit.Branch;
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

  actPush.Enabled := ahead;

  txtDiff.Clear;

  Caption := '[git '+fGit.Version+'](' + fGit.TopLevelDir + ')';
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

