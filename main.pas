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
  StrUtils, FileUtil, unitconfig, unitprocess, unitentries, unitgit, Types,
  lclType, Menus, Buttons, Grids, unitnewbranch, unitruncmd, unitansiescapes,
  unitnewtag, unitlogcache, unitlog, LConvEncoding, fgl, unitdbindex;

type
  TRefsMap = specialize TFPGMap<string, TRefInfoArray>;

  { TfrmMain }

  TfrmMain = class(TForm)
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
    gridLog: TDrawGrid;
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
    procedure gridLogDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
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
    fGit: TGit;
    fClickedIndex: Integer;
    fDir: string;
    fItemIndices: TItemIndexArray;
    fLogCache: TLogCache;
    fLogHandler: TLogHandler;
    fPopPoint: TPoint;
    fListAlwaysDrawSelection: boolean;
    fLastDescribedTag: string;
    fDescribed: boolean;
    fSeenRefs: TRefsMap;
    fGraphColumns: Integer;
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
    procedure UpdateStatus;
    procedure ShowError;
    procedure ViewFile(filename: string);
    procedure ComingSoon;
    function MakeMenuItemUnstagedEntryArray(mi: TMenuItem): TPFileEntryArray;
    function MakeMenuItemStagedEntryArray(mi: TMenuItem): TPFileEntryArray;
    procedure NewBranch;
    procedure InvalidateBranchMenu;
    procedure NewTag;
    procedure CheckMenuDivisorInLastPosition(pop:TPopupMenu);
    procedure CacheRefs;
    procedure FindParents;
    procedure UpdateGridRows;
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

  GRAPH_LEFT_PADDING          = 12;
  GRAPH_RIGHT_PADDING         = 12;
  GRAPH_LINE_WIDTH            = 2;
  GRAPH_NODE_RADIUS           = 4;
  GRAPH_COLUMN_SEPARATOR      = 18;


  VIEWER_BUFSIZE      = 1024*4;
  BIN_BUFSIZE         = 1024;


const
  GRAPH_MAX_COLORS = 5;
  GraphColumnsColors:array[0..GRAPH_MAX_COLORS-1] of TColor =
    (clBlue, clFuchsia, clMaroon, clRed, clGreen);

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

procedure TfrmMain.gridLogDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  aIndex, x, x1, x2, y, y1, y2, i, j, w, n: Integer;
  s: RawByteString;
  arr: TRefInfoArray;
  aBrushColor, aFontColor: TColor;
  r: TRect;
  db: TDbIndex;
begin
  if aRow>=gridLog.FixedRows then begin
    aIndex := aRow - gridLog.FixedRows;
    db := fLogCache.DbIndex;
    if db.LoadItem(aIndex) then begin
      x := aRect.Left + 7;
      case gridLog.Columns[aCol].Title.Caption of
        'RecNo':
          begin
            if fLogCache.RangeStart>=0 then
              s := format('%4d %4d',[aRow-1, aRow+fLogCache.RangeStart-1])
            else
              s := IntToStr(aRow-1);
          end;
        //'Graph':
        //  if (Length(fItemIndices)>0) and (aIndex<Length(fItemIndices)) then begin
        //
        //    with fItemIndices[aIndex] do begin
        //      gridLog.canvas.Pen.Width := GRAPH_LINE_WIDTH;
        //      w := aRect.Left + GRAPH_LEFT_PADDING;
        //
        //      y1 := aRect.Top;
        //      y2 := aRect.Bottom;
        //      y := y1 + (y2-y1) div 2;
        //
        //      j := column;
        //      for i:=0 to Length(lines)-1 do begin
        //        //if lines[i].source=LINE_SOURCE_COLUMN then begin
        //          n := lines[i].column;
        //          gridLog.Canvas.Pen.Style := psSolid;
        //          if column=n then j := i;
        //        //end else begin
        //        //  n := lines[i].column; //fItemIndices[lines[i].source].column;
        //        //  gridLog.Canvas.Pen.Style := psDash;
        //        //end;
        //        gridLog.Canvas.Pen.Color := GraphColumnsColors[ n mod GRAPH_MAX_COLORS];
        //        x := w + i * GRAPH_COLUMN_SEPARATOR;
        //
        //        if lifMerge in lines[i].Flags then begin
        //          // draw a merge line with origin at source and dest at this point
        //          gridLog.Canvas.Line(x, y, x, y2);
        //          gridLog.Canvas.Line(x-GRAPH_NODE_RADIUS-1, y, x, y);
        //          //x1 := w + fItemIndices[lines[i].source].column * GRAPH_COLUMN_SEPARATOR;
        //          //gridLog.Canvas.Line(x1, y, x, y);
        //        end else
        //        if lifBorn in lines[i].Flags then begin
        //          // draw a new born line with origin at source and dest at this point
        //          gridLog.Canvas.Line(x, y1, x, y);
        //          gridLog.Canvas.Line(x-GRAPH_NODE_RADIUS-1, y, x, y);
        //          //x1 := w + fItemIndices[lines[i].source].column * GRAPH_COLUMN_SEPARATOR;
        //          //gridLog.Canvas.Line(x1, y, x, y);
        //        end else
        //          gridlog.Canvas.Line(x, y1, x, y2);
        //      end;
        //
        //      x := w + j * GRAPH_COLUMN_SEPARATOR;
        //
        //      if first and (childs=nil) then y1 := y;
        //      if last and (parents=nil) then y2 := y;
        //
        //      gridLog.Canvas.Pen.Color := GraphColumnsColors[Column mod GRAPH_MAX_COLORS];
        //      gridlog.Canvas.Line(x, y1, x, y2);
        //
        //      gridLog.Canvas.Brush.Color := GraphColumnsColors[Column mod GRAPH_MAX_COLORS];
        //      gridLog.Canvas.Brush.Style := bsSolid;
        //      gridLog.canvas.Pen.Width :=0;
        //      if (Length(childs)>1) or (Length(parents)>1) then
        //        gridLog.Canvas.FillRect(x-GRAPH_NODE_RADIUS, y-GRAPH_NODE_RADIUS, x+GRAPH_NODE_RADIUS, y+GRAPH_NODE_RADIUS)
        //      else
        //        gridLog.canvas.EllipseC(x, y, GRAPH_NODE_RADIUS, GRAPH_NODE_RADIUS);
        //      gridLog.Canvas.Pen.Width := 1;
        //    end;
        //  end;

        'Subject':
          begin
            s := db.Item.Subject;
            if (fSeenRefs<>nil) and fSeenRefs.Find(db.Item.CommitOID, n ) then begin
              arr := fSeenRefs.Data[n];

              for i:=0 to Length(arr)-1 do begin
                w := gridLog.Canvas.TextWidth(arr[i]^.refName) + 6;

                case arr[i]^.subType of
                  rostLocal:
                    begin
                      if arr[i]^.head then aBrushColor := clRed
                      else                 aBrushColor := clGreen; //$00C300;
                      aFontColor := clWhite;
                    end;
                  rostTracking:
                    begin
                      aBrushColor := $AADDFF;
                      aFontColor := clBlack;
                    end;
                  rostTag:
                    begin
                      aBrushColor := clYellow;
                      aFontColor := clBlack;
                    end;
                end;

                r := Rect(x, aRect.Top+1, x + w, aRect.Bottom-1);

                gridLog.Canvas.Brush.Style := bsSolid;
                gridLog.Canvas.Pen.Color := clBlack;
                gridLog.Canvas.Brush.Color := aBrushColor;
                gridLog.Canvas.Rectangle(r);
                gridLog.Canvas.Font.Color := aFontColor;
                gridLog.Canvas.Brush.Style := bsClear;
                gridLog.Canvas.TextOut(r.Left + 3, r.Top-1, arr[i]^.refName);

                x += w + 2;
                if i=Length(arr)-1 then
                  x += 5;
              end;
              gridLog.Canvas.Font.Color := gridLog.Font.Color;
              gridLog.Canvas.Brush.Color := gridlog.Color;

            end;
          end;
        'Author': s := db.Item.Author;
        'SHA1': s := db.Item.CommitOID;
        'Date': s := IntToStr(db.Item.CommiterDate);
        else  s := '';
      end;

      gridLog.Canvas.TextOut(x, aRect.Top, s);
    end;
  end;
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
        fConfig.ViewUntrackedFiles := mi.checked;
        UpdateStatus;
      end;

    MENU_LIST_VIEW_IGNORED:
      begin
        fConfig.ViewIgnoredFiles := mi.checked;
        UpdateStatus;
      end;

    //MENU_LIST_VIEW_TRACKED:
    //  begin
    //    fConfig.ViewTrackedFiles := mi.Checked;
    //    UpdateStatus;
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
  if fGit.Switch(mi.Caption)>0 then
    ShowError
  else begin
    fDescribed := false;
    UpdateStatus;
    InvalidateBranchMenu;
  end;
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
    UpdateStatus;
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
    UpdateStatus;
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
    UpdateStatus;
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

procedure TfrmMain.UpdateStatus;
var
  cmdout: RawByteString;
begin
  lstUnstaged.Items.BeginUpdate;
  lstStaged.Items.BeginUpdate;
  try

    // get the more recent tag
    if fConfig.ShowTags and (not fDescribed) then begin
      fGit.Describe('', cmdout);
      fLastDescribedTag := cmdOut;
      fDescribed := true;
    end;

    if fConfig.ViewIgnoredFiles then fGit.IgnoredMode:='traditional' else fGit.IgnoredMode:='no';
    if fConfig.ViewUntrackedFiles then fGit.UntrackedMode:='all' else fGit.UntrackedMode:='no';

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
  f.Git := fGit;
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
        UpdateStatus;
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
var
  f: TfrmNewTag;
begin
  f := TfrmNewTag.Create(Self);
  f.Oid := fGit.BranchOID;
  try
    if f.ShowModal=mrOk then begin
      if fGit.Tag(f.txtName.Text, f.chkAnnotated.checked, f.txtMsg.Text)>0 then
        ShowError
      else begin
        fDescribed := false;
        UpdateStatus;
      end;
    end;
  finally
    f.free;
  end;
end;

procedure TfrmMain.CheckMenuDivisorInLastPosition(pop: TPopupMenu);
begin
  if pop.Items.Count>1 then begin
    if pop.Items[pop.Items.Count-1].Caption='-' then
      pop.Items.Delete(pop.Items.Count-1);
  end;
end;

procedure TfrmMain.CacheRefs;
var
  i, j, aIndex: Integer;
  info: PRefInfo;
  arr: TRefInfoArray;
  exists: boolean;
begin
  fGit.UpdateRefList;

  if fSeenRefs=nil then begin
    fSeenRefs := TRefsMap.Create;
    fSeenRefs.Sorted := true;
  end else
    fSeenRefs.Clear;

  for i:= 0 to fGit.RefList.Count-1 do begin
    info := PRefInfo(fGit.RefList.Objects[i]);
    exists := fSeenRefs.Find(info^.objName, aIndex);
    if exists then
      arr := fSeenRefs.KeyData[info^.objName]
    else
      arr := nil;

    j := Length(arr);
    SetLength(arr, j+1);
    arr[j] := info;

    if not exists then
      fSeenRefs.Add(info^.objName, arr)
    else
      fSeenRefs[info^.objName] := arr;
  end;

  //DebugLn;
  //for i:=0 to fGit.RefList.Count-1 do begin
  //  info := PRefInfo(fGit.RefList.Objects[i]);
  //  DebugLn('%2d. %s %s',[i, info^.objName, info^.refName]);
  //end;
  //DebugLn;
  //for i:=0 to fSeenRefs.Count-1 do begin
  //  arr := fSeenRefs.Data[i];
  //  DebugLn('%d. %s : %d refs', [i, fSeenRefs.Keys[i], Length(arr)]);
  //  for j:=0 to Length(Arr)-1 do
  //    DebugLn('   %s',[arr[j]^.refName]);
  //end;
end;

procedure TfrmMain.FindParents;
var
  parents: TParentsArray;
begin

end;

procedure TfrmMain.UpdateGridRows;
var
  col: TGridColumn;
  i: Integer;
begin
  gridLog.RowCount := fLogCache.DbIndex.Count + gridLog.FixedRows;
  fItemIndices := GetItemIndexes(fLogCache.DbIndex, true, fGraphColumns);
  col := gridLog.Columns.ColumnByTitle('Graph');
  i := gridLog.Columns.IndexOf(col);
  gridLog.Columns[i].Width := GRAPH_LEFT_PADDING + (fGraphColumns-1)*GRAPH_COLUMN_SEPARATOR + GRAPH_RIGHT_PADDING;
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
  fGit := TGit.Create;
  fGit.Config := fConfig;

  fConfig.OpenConfig;

  if not fGit.Initialize then begin
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

  fLogCache := TLogCache.Create(@OnLogCacheEvent);
  fLogCache.Git := fGit;
  fLogCache.Config := fConfig;
  fLogHandler := TLogHandler.Create(txtLog, @OnLogEvent);
  fLogHandler.Git := fGit;

  fConfig.ReadPreferences;

  if fConfig.ReadBoolean('NeedsMenuWorkaround') then begin
    Self.Menu := nil;
    Application.QueueAsyncCall(@DelayedShowMenu, 0);
  end;

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
  fLogHandler.Free;
  fLogCache.Free;
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
    btnStop.Visible := true;
    btnStop.Tag := 0;

    gridLog.RowCount := (gridLog.Height div gridLog.DefaultRowHeight) *  2;

    CacheRefs;

    fLogCache.LoadCache;

  end else begin
    panLog.Visible := false;
    panLogNew.Visible := false;
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
var
  s: string;
begin
  case event of
    LOGEVENT_OUTPUT:
      interrupt := btnStop.Visible and (btnStop.Tag=1);

    LOGEVENT_RECORD:
      begin
        if fLogCache.LogState=lsGetFirst then lblInfo.Font.Color := clGreen
        else                                  lblInfo.Font.Color := clRed;
        lblInfo.Caption := format('%s',[fLogCache.DbIndex.Info]);
        lblInfo.Visible := true;
        interrupt := btnStop.Visible and (btnStop.Tag=1);

        if fLogCache.DbIndex.Count<gridLog.height div gridLog.DefaultRowHeight then begin
          if fLogCache.DbIndex.Count mod 3 = 0 then
            gridLog.Invalidate;
        end;
      end;

    LOGEVENT_END:
      begin
        DebugLn('End event received');
        btnStop.Visible := false;
        lblInfo.Visible := false;
        UpdateGridRows;
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

