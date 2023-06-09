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

  This the log gui.
}
unit unitframelog;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

{

Intersting lazarus ranges for checking the graph ...

// merges in two consecutive log entries
// the second (the oldest) do not show merge line
88242f81521e6b90341743a3670c61276d7a38c9
2766af494532fac2dca258ea1ae90d6a2ebc4315

a05cbeffee39cc4f96e995c5ed271725ab07d31e
69682be8b0aeecc2cc408da08c046b2d74a01610

506d7ccf857bd38ef6f38d5f63e164f65bb95c9f
1440736e76de55d5e57e0eff1faa20d32017a4d9

87bcd888adeb9b108f8ad3d802f6f6a1ea3ffc03
89a9d84d34e72b4e360abb4f87bdff18a946547b

3d6bf3c2c1b9c3cd52d917261e09f151a2ac9d38
e2ad6b3d8d778da2fd37cab79e12896dc33d537f


b36000fc26b024eb8e4930f75d7373660210cf9d
bbac02ab05eaf9dae6fc5fd86a5766637dad2f45

6422aaa213f024bd7772109b3a17dfee45c5c364
c4109375a599264d818df2d265dab104ff8271a4

}

interface

uses
  Classes, SysUtils, dateUtils, fgl, LazLogger, SynEdit, SynHighlighterDiff,
  SynHighlighterPas, SynHighlighterXML, Graphics, Forms, Dialogs, Controls,
  Grids, ExtCtrls, ComCtrls, Menus, Types, Clipbrd, ActnList, Buttons, StdCtrls,
  unitgittypes, unitlogcache, unitdbindex, unitgitutils, unitifaces, unitruncmd,
  unitgitmgr, unitcommitbrowser, unitvfs, unithighlighterhelper, unitgraphbuild,
  unitfilehistory, unitnewbranch, unitreset;

const
  GRAPH_LEFT_PADDING          = 12;
  GRAPH_RIGHT_PADDING         = 12;
  GRAPH_LINE_WIDTH            = 2;
  GRAPH_NODE_RADIUS           = 4;
  GRAPH_COLUMN_SEPARATOR      = 18;

  ARROWLEN_X                  = GRAPH_NODE_RADIUS;
  ARROWLEN_Y                  = GRAPH_NODE_RADIUS;

  COLTAG_INDEX                = 0;
  COLTAG_GRAPH                = 1;
  COLTAG_SUBJECT              = 2;
  COLTAG_AUTHOR               = 3;
  COLTAG_DATE                 = 4;
  COLTAG_SHA1                 = 5;

type

  TDummyDirNode = class(TTreeNode)
  end;

  { TframeLog }

  TframeLog = class(TFrame, IObserver)
    actGotoHead: TAction;
    actGotoParent: TAction;
    actGotoChild: TAction;
    actShowFileHistory: TAction;
    actReload: TAction;
    actShowChanges: TAction;
    actLstLog: TActionList;
    btnStop: TSpeedButton;
    gridLog: TDrawGrid;
    lblInfo: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    mnuGotoHead: TMenuItem;
    panBrowser: TPanel;
    panMode: TPanel;
    panLogTools: TPanel;
    panFiles: TPanel;
    popLog: TPopupMenu;
    mnuSeparatorLast: TMenuItem;
    mnuSeparatorFirst: TMenuItem;
    btnShowChanges: TSpeedButton;
    btnReload: TSpeedButton;
    radTree: TRadioButton;
    radPatch: TRadioButton;
    btnShowFileHistory: TSpeedButton;
    splitChanges: TSplitter;
    Splitter2: TSplitter;
    treeFiles: TTreeView;
    txtViewer: TSynEdit;
    procedure actGotoChildExecute(Sender: TObject);
    procedure actGotoHeadExecute(Sender: TObject);
    procedure actGotoParentExecute(Sender: TObject);
    procedure actReloadExecute(Sender: TObject);
    procedure actShowChangesExecute(Sender: TObject);
    procedure actShowFileHistoryExecute(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure gridLogContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure gridLogDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure gridLogHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure gridLogSelection(Sender: TObject; aCol, aRow: Integer);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure radPatchMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure treeFilesExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure treeFilesSelectionChanged(Sender: TObject);
  private
    fActive: boolean;
    fConfig: IConfig;
    fGit: IGit;
    fGitMgr: TGitMgr;
    fhlHelper: THighlighterHelper;
    fItemIndices: TItemIndexArray;
    fLogCache: TLogCache;
    fGraphColumns: Integer;
    fWithArrows: boolean;
    fCommitBrowser: TCommitBrowser;
    fCurrentItem: TLogItem;
    procedure OnContextPopLogClick(Sender: TObject);
    procedure OnCreateBranchClick(Sender: TObject);
    procedure OnDeleteBranchClick(Sender: TObject);
    procedure OnDeleteRemoteBranchClick(Sender: TObject);
    procedure OnGraphBuilderDone(Sender: TObject);
    procedure OnLogEvent(sender: TObject; thread: TLogThread; event: Integer; var interrupt: boolean);
    procedure OnDeleteTagClick(sender: TObject);
    procedure OnResetBranchClick(Sender: TObject);
    procedure OnSwitchBranchClick(Sender: TObject);
    procedure OnSwitchTagClick(sender: TObject);
    procedure OnCreateTagClick(sender: TObject);
    procedure OnMergeBranchClick(Sender: TObject);
    procedure CopyToClipboard(what: Integer);
    function  LocateCommit(const commit: QWord): boolean;
    procedure LocateHead;
    procedure LocateParent;
    procedure LocateChild;
    function  LocateItemIndex(aIndex: Integer): boolean;
    procedure AddMergeBranchMenu;
    procedure AddTagsMenu;
    procedure AddExtraMenus;
    procedure SetActive(AValue: boolean);
    procedure SetGitMgr(AValue: TGitMgr);
    procedure ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
    procedure ShowChanges(aRow: Integer);
    procedure HideChanges;
    procedure PopulateTree(node: PvfsNode; treeNode: TTreeNode);
    procedure ReloadTreeFile;
    procedure UpdateCommitBrowser(aMode: TCommitBrowserMode);
    function  CommitBrowserModeFromGui:TCommitBrowserMode;
    function  GetAllCommitInfo: string;
    procedure CreateDummyDirNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure UpdateMode(Data: PtrInt);
    function  ColumnByTag(aTag: Integer): TGridColumn;
  public
    procedure Clear;
    procedure UpdateGridRows;

    property LogCache: TLogCache read fLogCache write fLogCache;
    property Config: IConfig read fConfig write fConfig;
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property HlHelper: THighlighterHelper read fhlHelper write fhlHelper;
    property Active: boolean read fActive write SetActive;

  end;

implementation

{$R *.lfm}

const
  GRAPH_MAX_COLORS = 5;
  GraphColumnsColors:array[0..GRAPH_MAX_COLORS-1] of TColor =
    (clBlue, clFuchsia, clMaroon, clRed, clGreen);

const
  COPY_ALL_INFO     = 1;
  COPY_SHA          = 2;

  ALL_INFO_TEMPLATE =
    'parents: %s' + LineEnding +
    'commit: %s' + LineEnding +
    'references: %s' + LineEnding +
    'Author: %s <%s>' + LineEnding +
    'Commit Date: %s' + LineEnding +
    'Message: ' + LineEnding+LineEnding+
    '%s';

procedure DrawLine(canvas: TCanvas; x1, y1, x2, y2: Integer; withArrow, destNode:boolean);
var
  x, y, o: Integer;
  w: Integer;
begin
  canvas.Line(x1, y1, x2, y2);
  if withArrow then begin
    o := canvas.pen.width;
    canvas.pen.width := 1;
    x := x2;
    y := y2;

    if destNode then
      w := GRAPH_NODE_RADIUS
    else
      w := GRAPH_LINE_WIDTH;

    if x2<x1 then begin
      x := x + w;
      canvas.Line(x, y - GRAPH_LINE_WIDTH, x + ARROWLEN_X, y - ARROWLEN_Y - GRAPH_LINE_WIDTH - 1);
      canvas.Line(x, y, x + ARROWLEN_X, y + ARROWLEN_Y + 1);
    end else begin
      x := x - w;
      canvas.Line(x, y - GRAPH_LINE_WIDTH, x - ARROWLEN_X, y - ARROWLEN_Y - GRAPH_LINE_WIDTH - 1);
      canvas.Line(x, y, x - ARROWLEN_X, y + ARROWLEN_Y + 1);
    end;
    canvas.pen.width := o;
  end;
end;

{ TframeLog }

procedure TframeLog.gridLogDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  aIndex, x, x1, x2, y, y1, y2, i, j, w, n: Integer;
  s: RawByteString;
  arr: TRefInfoArray;
  aBrushColor, aFontColor: TColor;
  r: TRect;
  db: TDbIndex;
  flags:  TLineItemFlags;
  aItem: TLogItem;
begin

  if fGitMgr=nil then
    exit;

  if aRow>=gridLog.FixedRows then begin
    aIndex := aRow - gridLog.FixedRows;
    db := fLogCache.DbIndex;
    if db=nil then begin
      exit;
    end;
    if db.LoadItem(aIndex, aItem) then begin
      x := aRect.Left + 7;
      case gridLog.Columns[aCol].tag of
        COLTAG_INDEX:
          begin
            if fLogCache.RangeStart>=0 then
              s := format('%4d %4d',[aRow-1, aRow+fLogCache.RangeStart-1])
            else
              s := IntToStr(aRow-1);
          end;
        COLTAG_GRAPH:
          if (Length(fItemIndices)>0) and (aIndex<Length(fItemIndices)) then begin

            with fItemIndices[aIndex] do begin
              gridLog.canvas.Pen.Width := GRAPH_LINE_WIDTH;
              w := aRect.Left + GRAPH_LEFT_PADDING;

              j := column;
              for i:=Length(lines)-1 downto 0 do begin
                y1 := aRect.Top;
                y2 := aRect.Bottom;
                y := y1 + (y2-y1) div 2;
                flags := lines[i].Flags;
                //if lines[i].source=LINE_SOURCE_COLUMN then begin
                  n := lines[i].column;
                  gridLog.Canvas.Pen.Style := psSolid;
                  if column=n then j := i;
                //end else begin
                //  n := lines[i].column; //fItemIndices[lines[i].source].column;
                //  gridLog.Canvas.Pen.Style := psDash;
                //end;
                gridLog.Canvas.Pen.Color := GraphColumnsColors[ n mod GRAPH_MAX_COLORS];
                gridLog.Canvas.Brush.Color := GraphColumnsColors[ n mod GRAPH_MAX_COLORS];
                x := w + n * GRAPH_COLUMN_SEPARATOR;

                if lifNode in flags then begin
                  if (lifFirst in flags) and (childs=nil) then y1 := y;
                  if (lifLast  in flags) and (parents=nil) then y2 := y;

                  gridlog.Canvas.Line(x, y1, x, y2);
                  gridLog.Canvas.Brush.Style := bsSolid;
                  gridLog.canvas.Pen.Style := psClear;
                  if (Length(childs)>1) or (Length(parents)>1) then
                    gridLog.Canvas.FillRect(x-GRAPH_NODE_RADIUS, y-GRAPH_NODE_RADIUS, x+GRAPH_NODE_RADIUS, y+GRAPH_NODE_RADIUS)
                  else
                    gridLog.canvas.EllipseC(x, y, GRAPH_NODE_RADIUS, GRAPH_NODE_RADIUS);
                  gridLog.canvas.Pen.Style := psSolid;
                end else begin

                  if [lifInternal, lifToMerge, lifToBorn] * flags <> [] then
                    gridlog.Canvas.Line(x, y1, x, y2);

                  if lifMerge in flags then begin
                    // draw a merge line
                    gridLog.Canvas.Line(x, y, x, y2);
                    x1 := w + fItemIndices[lines[i].source].column * GRAPH_COLUMN_SEPARATOR;
                    DrawLine(gridLog.Canvas, x, y, x1, y, fWithArrows, true);
                  end;

                  if lifBorn in flags then begin
                    // draw a new born line with origin at source and dest at this point
                    gridLog.Canvas.Line(x, y1, x, y);
                    x1 := w + fItemIndices[lines[i].source].column * GRAPH_COLUMN_SEPARATOR;
                    DrawLine(gridLog.Canvas, x1, y, x, y, fWithArrows, false);
                  end;
                end;
              end;
              gridLog.Canvas.Pen.Width := 1;
            end;
          end;

        COLTAG_SUBJECT:
          begin
            s := aItem.Subject;
            if (fGit.RefsMap<>nil) and fGit.RefsMap.Find(aItem.CommitOID, n ) then begin
              arr := fGit.RefsMap.Data[n];

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
        COLTAG_AUTHOR: s := aItem.Author;
        COLTAG_SHA1: s := aItem.CommitOID;
        COLTAG_DATE: s := DateTimeToStr(UnixToDateTime(aItem.CommiterDate, false));
        else  s := '';
      end;

      gridLog.Canvas.Brush.Style := bsClear;
      gridLog.Canvas.TextOut(x, aRect.Top, s);
      gridLog.Canvas.Brush.Style := bsSolid;

      finalize(aItem);
    end;
  end;
end;

procedure TframeLog.gridLogHeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
  col: TGridColumn;
begin
  if isColumn then begin
    col := gridLog.Columns[Index];
    fConfig.WriteInteger('frmlog.grid.coltag'+IntToStr(col.tag)+'.width', col.Width, SECTION_GEOMETRY);
  end;
end;

procedure TframeLog.gridLogSelection(Sender: TObject; aCol, aRow: Integer);
var
  aIndex: Integer;
begin
  aIndex := gridLog.Row - gridLog.FixedRows;
  actGotoParent.Enabled := (aIndex>=0) and (aIndex<Length(fItemIndices)) and (Length(fItemIndices[aIndex].parents)>0);
  actGotoChild.Enabled := (aIndex>=0) and (aIndex<Length(fItemIndices)) and (Length(fItemIndices[aIndex].childs)>0);

  if panBrowser.Visible then
    ShowChanges(aRow)
end;

procedure TframeLog.gridLogContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  p: TPoint;
  aRow, aIndex, mIndex: Integer;
  mi: TMenuItem;
begin
  // check if triggered by keyboard
  if (MousePos.x=-1) and (MousePos.y=-1) then aRow := gridLog.Row
  else                                        aRow := gridLog.MouseToCell(MousePos).y;
  aIndex := aRow - gridLog.FixedRows;

  // do not show the context popup if the grid is invalid
  Handled := (gridLog.RowCount=gridLog.FixedRows) or (aIndex<0) or not LocateItemIndex(aIndex);

  if not Handled then begin
    mIndex := mnuSeparatorFirst.Tag + 1;
    mnuSeparatorFirst.Tag := mIndex;
    // all is ok, cleanup
    mIndex := mnuSeparatorFirst.MenuIndex+1;
    while mIndex<>mnuSeparatorLast.MenuIndex do
      popLog.Items.Delete(mIndex);

    AddTagsMenu;

    AddMergeBranchMenu;

    AddExtraMenus;
  end;
end;

procedure TframeLog.actGotoHeadExecute(Sender: TObject);
begin
  LocateHead;
end;

procedure TframeLog.actGotoChildExecute(Sender: TObject);
begin
  LocateChild;
end;

procedure TframeLog.actGotoParentExecute(Sender: TObject);
begin
  LocateParent;
end;

procedure TframeLog.actReloadExecute(Sender: TObject);
begin
  gitMgr.UpdateRefList;
end;

procedure TframeLog.actShowChangesExecute(Sender: TObject);
begin
  panBrowser.Visible := actShowChanges.Checked;
  if panBrowser.Visible then begin
    ShowChanges(gridLog.Row);
    splitChanges.Top := panBrowser.Top - 1;
  end else
    HideChanges;
end;

procedure TframeLog.actShowFileHistoryExecute(Sender: TObject);
var
  node: TTreeNode;
  aFile: string;
  f: TfrmFileHistory;
begin
  node := treeFiles.Selected;

  if fCommitBrowser.Mode=cbmPatch then
    aFile := node.Text
  else
    aFile := node.GetTextPath;

  f := TfrmFileHistory.Create(Application);
  f.GitMgr := fGitMgr;
  f.HlHelper := fhlHelper;
  f.FilePath := aFile;
  f.Show;
end;

procedure TframeLog.btnStopClick(Sender: TObject);
begin
  btnStop.Tag := 1;
end;

procedure TframeLog.MenuItem2Click(Sender: TObject);
begin
  CopyToClipboard(COPY_ALL_INFO);
end;

procedure TframeLog.MenuItem3Click(Sender: TObject);
begin
  CopyToClipboard(COPY_SHA)
end;

procedure TframeLog.radPatchMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button=mbLeft then
    Application.QueueAsyncCall(@UpdateMode, 0);
end;

procedure TframeLog.treeFilesExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  N:TTreeNode;
  vfsNode, child: PvfsNode;
begin
  //DebugLn('Expanding node %s',[Node.Text]);
  N := Node.GetFirstChild;
  if N is TDummyDirNode then begin
    AllowExpansion := false;
    if Node.Data<>nil then begin
      // ask the commit browser we need expanding this node
      vfsNode := fCommitBrowser.ExpandVfs(Node.GetTextPath);
      // this vfsNode should correspond to the data of treenode 'node'
      // add the childs of vfsNode as childs of 'node'
      AllowExpansion := vfsNode<>nil;
      if AllowExpansion then begin
        N.Delete;
        child := vfsNode^.childs;
        while child<>nil do begin
          PopulateTree(child, Node);
          child := child^.next;
        end;
      end;
    end;
  end;
end;
procedure TframeLog.treeFilesSelectionChanged(Sender: TObject);
var
  node: TTreeNode;
  info: PInfoNode;
  isTree: boolean;
begin
  node := treeFiles.Selected;
  if node<>nil then begin
    isTree := true;
    info := PInfoNode(Node.Data);
    if fCommitBrowser.Mode=cbmPatch then begin
      isTree := node.GetPrevSibling=nil;
      txtViewer.TopLine := info^.line
    end else
    if info<>nil then begin
      isTree := info^.filetype='tree';
      fhlHelper.SetHighlighter(txtViewer, node.Text);
      fGit.Show(info^.fileTree, txtViewer.Lines);
      if isTree then begin
        txtViewer.Lines.Delete(0);
        txtViewer.Lines.Delete(0);
      end;
    end;
    actShowFileHistory.Enabled := not isTree;
  end;
end;

procedure TframeLog.OnLogEvent(sender: TObject; thread: TLogThread;
  event: Integer; var interrupt: boolean);
var
  s: string;
begin

  case event of

    LOGEVENT_START:
      begin
        btnStop.Visible := true;
        btnStop.Tag := 0;
      end;

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
        btnStop.Visible := false;
        lblInfo.Visible := false;

        if fLogCache.DbIndex.Updated then begin
          UpdateGridRows;
          LocateHead;
        end;
      end;

    LOGEVENT_DONE:
      begin
      end;
  end;
end;

procedure TframeLog.OnDeleteTagClick(sender: TObject);
var
  mi: TMenuItem absolute Sender;
  info: PRefInfo;
  s: string;
begin
  info := {%H-}PRefInfo(PtrInt(mi.Tag));
  if info<>nil then begin
    if fGit.DeleteTag(info^.refName)>0 then
      // error
    else begin
      fGitMgr.ForceTagDescription;
      fGitMgr.UpdateStatus;
      fGitMgr.UpdateRefList;
    end;
  end;
end;

procedure TframeLog.OnResetBranchClick(Sender: TObject);
var
  f: TfrmReset;
  fullReload: Boolean;
  cmd: String;
  aType: Integer;
begin
  f := TfrmReset.Create(Self);
  f.branch := fGitMgr.Branch;
  f.Subject := fCurrentItem.Subject;
  f.Commit := fCurrentItem.CommitOID;
  try
    if f.ShowModal=mrOk then begin
      aType := f.radResetType.ItemIndex;
      case aType of
        RESETTYPE_SOFT:
          begin
            cmd := '--soft';
            fullReload := false;
          end;
        RESETTYPE_MIXED:
          begin
            cmd := '--mixed';
            fullReload := false;
          end;
        RESETTYPE_HARD:
          begin
            cmd := '--hard';
            fullReload := true;
          end;
      end;

      cmd +=  ' ' + fCurrentItem.CommitOID;

      if RunInteractive(fGit.Exe + ' reset ' + cmd, fGit.TopLevelDir, 'Reset a branch', fGitMgr.Branch)>0 then
        //
      else begin
        fGitMgr.ForceTagDescription;
        fGitMgr.UpdateStatus;
        gblInvalidateCache := fullReload;   // TODO: trigger invalidate cache immediately
        fGitMgr.UpdateRefList;
      end;

    end;
  finally
    f.Free;
  end;
end;

procedure TframeLog.OnSwitchBranchClick(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
  info: PRefInfo;
begin
  info := {%H-}PRefInfo(mi.Tag);
  if info<>nil then begin
    if fGit.Switch(info^.refName)>0 then
      //
    else begin
      fGitMgr.ForceTagDescription;
      fGitMgr.UpdateStatus;
      fGitMgr.UpdateRefList;
    end;
  end;
end;

procedure TframeLog.OnSwitchTagClick(sender: TObject);
var
  mi: TMenuItem absolute Sender;
  info: PRefInfo;
begin
  info := {%H-}PRefInfo(mi.Tag);
  if info<>nil then
    fGitMgr.QueueSwitchTag(info^.refName);
end;

procedure TframeLog.OnCreateTagClick(sender: TObject);
begin
  fGitMgr.QueueNewTag(fCurrentItem.CommitOID);
end;

procedure TframeLog.OnMergeBranchClick(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
  s: string;
  info: PRefInfo;
begin
  info := PRefInfo(mi.Tag);
  if info<>nil then begin

    s := 'git merge ' + info^.refName;
    if RunInteractive(fGit.Exe + ' merge '+ info^.refName, fGit.TopLevelDir, 'Merging branches', s)>0 then begin
      // an error occurred
    end else begin
      // queue update status
      fGitMgr.UpdateStatus;
      // update refs
      fGitMgr.UpdateRefList;
    end;

  end;
end;

procedure TframeLog.OnContextPopLogClick(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
begin
  ShowMessage(format('You really got me: %s',[mi.Caption]));
end;

procedure TframeLog.OnCreateBranchClick(Sender: TObject);
var
  f: TfrmNewBranch;
begin
  f := TfrmNewBranch.Create(Self);
  f.GitMgr := fGitMgr;
  f.CommitInfo := GetAllCommitInfo;
  f.Commit := fCurrentItem.CommitOID;
  try
    if f.ShowModal=mrOk then
      fGitMgr.QueueNewBranch(self, f.BranchName, f.GetBranchCommandOptions, f.Switch, f.Fetch);
  finally
    f.Free;
  end;
end;

procedure TframeLog.OnDeleteBranchClick(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
  info: PRefInfo;
  cmdout: RawByteString;
begin
  info := {%H-}PRefInfo(mi.Tag);
  if info<>nil then begin
    if fGit.Any('branch -d ' + info^.refName, cmdout)>0 then
      //
    else begin
      fGitMgr.ForceTagDescription;
      fGitMgr.UpdateStatus;
      fGitMgr.UpdateRefList;
    end;
  end;
end;

procedure TframeLog.OnDeleteRemoteBranchClick(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
  info: PRefInfo;
  cmd: string;
  cmdout: RawByteString;
begin
  info := {%H-}PRefInfo(mi.Tag);
  if info<>nil then begin
    cmd := stringReplace(info^.refName, '/', ' -d ', []);
    if RunInteractive(fGit.Exe + ' push ' + cmd, fGit.TopLevelDir, 'Deleting remote branch', info^.refName)>0 then
      //
    else begin
      fGitMgr.ForceTagDescription;
      fGitMgr.UpdateStatus;
      fGitMgr.UpdateRefList;
    end;
  end;
end;

procedure TframeLog.OnGraphBuilderDone(Sender: TObject);
var
  thread: TGraphBuilderThread absolute sender;
  col: TGridColumn;
  i: Integer;
begin
  lblInfo.Visible := false;

  fItemIndices := thread.IndexArray;
  fGraphColumns := thread.MaxColumns;

  col := ColumnByTag(COLTAG_GRAPH);
  i := gridLog.Columns.IndexOf(col);
  gridLog.Columns[i].Width := GRAPH_LEFT_PADDING + (fGraphColumns-1)*GRAPH_COLUMN_SEPARATOR + GRAPH_RIGHT_PADDING;

  gridLog.Invalidate;
end;

procedure TframeLog.CopyToClipboard(what: Integer);
var
  s: String;
begin

  s := '';
  case what of
    COPY_ALL_INFO:
      s := GetAllCommitInfo;
    COPY_SHA:
      s := fCurrentItem.CommitOID;
  end;

  if s<>'' then
    clipboard.AsText := s;
end;

function TframeLog.LocateCommit(const commit: QWord): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Length(fItemIndices) - 1 do begin
    if commit = fItemIndices[i].commit then begin
      gridLog.Row :=  gridLog.FixedRows + i;
      result := true;
      break;
    end;
  end;
end;

procedure TframeLog.LocateHead;
var
  commit: QWord;
begin
  commit := OIDToQWord(fGitmgr.BranchOID);
  LocateCommit(commit);
end;

procedure TframeLog.LocateParent;
var
  aIndex: Integer;
begin
  aIndex := gridLog.Row - gridLog.FixedRows;
  if Length(fItemIndices[aIndex].parents)>0 then begin
    aIndex := fItemIndices[aIndex].parents[0];
    LocateCommit(fItemIndices[aIndex].commit);
  end;
end;

procedure TframeLog.LocateChild;
var
  aIndex: Integer;
begin
  aIndex := gridLog.Row - gridLog.FixedRows;
  if Length(fItemIndices[aIndex].childs)>0 then begin
    aIndex := fItemIndices[aIndex].childs[0];
    LocateCommit(fItemIndices[aIndex].commit);
  end;
end;

function TframeLog.LocateItemIndex(aIndex: Integer): boolean;
begin
  result := (aIndex>=0) and (aIndex<fLogCache.DbIndex.Count);
  if result then begin
    Finalize(fCurrentItem);
    result := fLogCache.DbIndex.LoadItem(aIndex, fCurrentItem);
  end;
end;

procedure TframeLog.AddMergeBranchMenu;
var
  headCommit, curCommit: QWord;
  mi: TMenuItem;
  n, i, j: Integer;
  refItems: TRefInfoArray;
  curBranch: string;

  function FilterLocal(info: PRefInfo): boolean;
  begin
    result := (info^.subType=rostLocal) and not info^.head;
  end;

  function FilterTracking(info: PRefInfo): boolean;
  begin
    result := (info^.subType=rostTracking) and not info^.head;
  end;

begin
  headcommit := OIDToQWord(fGitMgr.BranchOID);
  curCommit := OIDToQWord(fCurrentItem.CommitOID);
  if (headCommit=curCommit) then
    exit;

  mi := TMenuItem.Create(Self.Owner);
  mi.Caption := '-';
  popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

  mi := TMenuItem.Create(Self.Owner);
  mi.Caption := 'Create a branch at this commit';
  mi.OnClick := @OnCreateBranchClick;
  mi.Tag := 0;
  popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

  refItems := fGit.RefsFilter(fCurrentItem.CommitOID, @FilterLocal);
  for i := 0 to Length(refItems)-1 do begin
    mi := TMenuItem.Create(Self.Owner);
    mi.Caption := format('Merge %s to %s',[QuotedStr(refItems[i]^.refName), QuotedStr(fGitMgr.Branch)]);
    mi.OnClick := @OnMergeBranchClick;
    mi.Tag := PtrInt(refItems[i]);
    popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

    mi := TMenuItem.Create(Self.Owner);
    mi.Caption := format('Switch to %s',[QuotedStr(refItems[i]^.refName)]);
    mi.OnClick := @OnSwitchBranchClick;
    mi.Tag := PtrInt(refItems[i]);
    popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

    mi := TMenuItem.Create(Self.Owner);
    mi.Caption := format('Delete branch %s',[QuotedStr(refItems[i]^.refName)]);
    mi.OnClick := @OnDeleteBranchClick;
    mi.Tag := PtrInt(refItems[i]);
    popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

    if refItems[i]^.head then
      curBranch := refItems[i]^.refName;
  end;

  refItems := fGit.RefsFilter(fCurrentItem.CommitOID, @FilterTracking);
  for i := 0 to Length(refItems)-1 do begin
    if refItems[i]^.refName.EndsWith('HEAD') then
      continue;
    if refItems[i]^.refName.EndsWith('/' + curBranch) then
      continue;

    mi := TMenuItem.Create(Self.Owner);
    mi.Caption := format('Delete remote branch %s',[QuotedStr(refItems[i]^.refName)]);
    mi.OnClick := @OnDeleteRemoteBranchClick;
    mi.Tag := PtrInt(refItems[i]);
    popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);
  end;
end;

procedure TframeLog.AddTagsMenu;
var
  headCommit, curCommit: QWord;
  mi: TMenuItem;
  n, i, j: Integer;
  refItems: TRefInfoArray;

  function Filter(info: PRefInfo): boolean;
  begin
    result := (info^.subType=rostTag);
  end;

begin
  headcommit := OIDToQWord(fGitMgr.BranchOID);
  curCommit := OIDToQWord(fCurrentItem.CommitOID);

  mi := TMenuItem.Create(Self.Owner);
  mi.Caption := 'Create a tag at this commit';
  mi.OnClick := @OnCreateTagClick;
  mi.Tag := 0;
  popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

  refItems := fGit.RefsFilter(fCurrentItem.CommitOID, @Filter);
  for i := 0 to Length(refItems)-1 do begin

    mi := TMenuItem.Create(Self.Owner);
    mi.Caption := format('Switch to %s',[QuotedStr(refItems[i]^.refName)]);
    mi.OnClick := @OnSwitchTagClick;
    mi.Tag := PtrInt(refItems[i]);
    popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

    mi := TMenuItem.Create(Self.Owner);
    mi.Caption := format('Delete tag %s',[QuotedStr(refItems[i]^.refName)]);
    mi.OnClick := @OnDeleteTagClick;
    mi.Tag := PtrInt(refItems[i]);
    popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);
  end;


end;

procedure TframeLog.AddExtraMenus;
var
  headcommit, curCommit: QWord;
  mi: TMenuItem;
begin

  headcommit := OIDToQWord(fGitMgr.BranchOID);
  curCommit := OIDToQWord(fCurrentItem.CommitOID);
  if (headCommit=curCommit) then
    exit;

  mi := TMenuItem.Create(Self.Owner);
  mi.Caption := '-';
  popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

  mi := TMenuItem.Create(Self.Owner);
  mi.Caption := format('Reset %s to this commit',[QuotedStr(fGitMgr.Branch)]);
  mi.OnClick := @OnResetBranchClick;
  mi.Tag := 0;
  popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);
end;

procedure TframeLog.SetActive(AValue: boolean);
begin
  if fActive = AValue then Exit;

  Finalize(fCurrentItem);

  if not fActive then begin

    fWithArrows := fConfig.ReadBoolean('DrawArrows', true);

    if fLogCache=nil then begin
      fLogCache := TLogCache.Create(@OnLogEvent);
      fLogCache.GitMgr := fGitMgr;
      fLogCache.Config := fConfig;

      fLogCache.Open;
      if fLogCache.DbIndex.Count>0 then begin
        UpdateGridRows;
        Application.ProcessMessages;
      end;
    end;

  end;

  fActive := AValue;

  if fActive then
    fGitMgr.UpdateRefList;

end;

procedure TframeLog.SetGitMgr(AValue: TGitMgr);
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

procedure TframeLog.ObservedChanged(Sender: TObject; what: Integer; data: PtrInt
  );
var
  aMode: TCommitBrowserMode;
begin
  case what of

    GITMGR_EVENT_REFLISTCHANGED:
      if fActive then begin
        // reflist has changed, update the grid to reflect annotations.
        gridLog.Invalidate;
        // queue a cache update
        fLogCache.UpdateCache;
      end;

    COMMITBROWSER_EVENT_RELOAD: begin
      treeFiles.Items.Clear;
      aMode := CommitBrowserModeFromGui;
      fhlHelper.SetHighlighter(txtViewer, 'x.diff');
      if aMode=cbmPatch then
        txtViewer.Lines.Assign(fCommitBrowser.Diff)
      else
        txtViewer.Lines.Text := GetAllCommitInfo;
      if data<>0 then
        ReloadTreeFile;
    end;
  end;
end;

procedure TframeLog.ShowChanges(aRow: Integer);
var
  aIndex: Integer;
  aMode: TCommitBrowserMode;
begin

  aIndex := aRow - gridLog.FixedRows;
  if not LocateItemIndex(aIndex) then begin
    txtViewer.Lines.Text := 'Unable to locate db index';
    exit;
  end;

  actShowFileHistory.Enabled := false;

  if fCommitBrowser=nil then begin
    fCommitBrowser := TCommitBrowser.Create;
    fCommitBrowser.GitMgr := fGitMgr;
    fCommitBrowser.Config := fConfig;
    fCommitBrowser.ObserverMgr.AddObserver(self);
  end;

  fCommitBrowser.Commit := fCurrentItem.CommitOID;
  aMode := CommitBrowserModeFromGui;
  UpdateCommitBrowser(aMode);
end;

procedure TframeLog.HideChanges;
begin
  if fCommitBrowser<>nil then
    fCommitBrowser.ObserverMgr.RemoveObserver(Self);
end;

procedure TframeLog.PopulateTree(node: PvfsNode;
  treeNode: TTreeNode);
var
  child: PvfsNode;
  info: PInfoNode;
  oldCreateNodeClass: TTVCreateNodeClassEvent;
begin
  if node=nil then exit;

  treeNode := treeFiles.Items.AddChildObject(treeNode, node^.Name, node^.Data);
  //treeNode := treeFiles.Items.AddObject(treeNode, node^.Name, node^.Data);

  info := node^.data;
  if (info<>nil) and (info^.filetype='tree') then begin
    oldCreateNodeClass := treeFiles.OnCreateNodeClass;
    TreeFiles.OnCreateNodeClass := @CreateDummyDirNodeClass;
    TreeFiles.Items.AddChild(treeNode, '');
    TreeFiles.OnCreateNodeClass  := oldCreateNodeClass;
  end else begin
    child := node^.Childs;
    while child<>nil do begin
      PopulateTree(child, treeNode);
      child := child^.Next;
    end;
  end;
end;

procedure TframeLog.ReloadTreeFile;
var
  sibling: PvfsNode;
begin
  sibling := fCommitBrowser.vfs.root;
  while sibling<>nil do begin
    PopulateTree(sibling, nil);
    sibling := sibling^.Next;
  end;
end;

procedure TframeLog.UpdateCommitBrowser(aMode: TCommitBrowserMode);
begin
  treeFiles.ShowLines := aMode=cbmTree;
  fCommitBrowser.Mode := aMode;
  fCommitBrowser.ApplyMode;
end;

function TframeLog.CommitBrowserModeFromGui: TCommitBrowserMode;
begin
  if radPatch.Checked then
    result := cbmPatch
  else
    result := cbmTree;
end;

function TframeLog.GetAllCommitInfo: string;
var
  n: Integer;
  arr: TRefInfoArray;
  ref: PRefInfo;
  dt: TDateTime;
begin
  result := '';
  with fCurrentItem do begin
    if (fGit.RefsMap<>nil) and fGit.RefsMap.Find(CommitOID, n ) then begin
      arr := fGit.RefsMap.Data[n];
      for ref in arr do begin
        if result<>'' then result+=', ';
        if ref^.subType=rostTag then
          result += 'tag: ';
        result += ref^.refName
      end;
    end;
    dt := UnixToDateTime(CommiterDate, false);
    result := format(ALL_INFO_TEMPLATE, [
      ParentOID, CommitOID, result,
      Author, Email,
      format('%s (%d)',[DateTimeToStr(dt), CommiterDate]),
      Subject]);
  end;
end;

procedure TframeLog.CreateDummyDirNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TDummyDirNode;
end;

procedure TframeLog.UpdateMode(Data: PtrInt);
var
  amode: TCommitBrowserMode;
begin
  aMode := CommitBrowserModeFromGui;
  UpdateCommitBrowser(aMode);
end;

function TframeLog.ColumnByTag(aTag: Integer): TGridColumn;
var
  i: Integer;
begin
  result := nil;
  for i:=0 to gridLog.Columns.Count-1 do
    if gridLog.Columns[i].Tag=aTag then begin
      result := gridLog.Columns[i];
      break;
    end;
end;

procedure TframeLog.Clear;
begin
  FreeAndNil(fLogCache);
  FreeAndNil(fCommitBrowser);
end;

procedure TframeLog.UpdateGridRows;
var
  gBuild: TGraphBuilderThread;
begin
  gridLog.RowCount := fLogCache.DbIndex.Count + gridLog.FixedRows;

  lblInfo.Caption := 'Building graph..';
  lblInfo.Font.Color := clBlue;
  lblInfo.Visible := true;

  gBuild := TGraphBuilderThread.Create(fLogCache.DbIndex);
  gBuild.WithColumns := true;
  gBuild.FreeOnTerminate := true;
  gBuild.OnTerminate := @OnGraphBuilderDone;
  gBuild.Start;
end;

end.

