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

{$define CutterMode}

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
  Classes, SysUtils, dateUtils, fgl, lclIntf, LazLogger, SynEdit, SynHighlighterDiff,
  SynHighlighterPas, SynHighlighterXML, Graphics, Forms, Dialogs, Controls, StrUtils,
  Grids, ExtCtrls, ComCtrls, Menus, Types, Clipbrd, ActnList, Buttons, StdCtrls,
  graphutil,
  unitgittypes, unitlogcache, unitdbindex, unitgitutils, unitifaces, unitruncmd,
  unitgitmgr, unitcommitbrowser, unitvfs, unithighlighterhelper, unitgraphbuild,
  unitfilehistory, unitnewbranch, unitreset, unitcommon, unittextchunks, unitlinkmgr;

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
    btnPrev: TSpeedButton;
    btnNext: TSpeedButton;
    lblGraphBuild: TLabel;
    txtSearch: TEdit;
    gridLog: TDrawGrid;
    lblInfo: TLabel;
    mnuCopy: TMenuItem;
    MenuItem2: TMenuItem;
    mnuCopySha: TMenuItem;
    mnuGotoParent: TMenuItem;
    mnuGotoChild: TMenuItem;
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
    btnFilter: TSpeedButton;
    btnSearch: TSpeedButton;
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
    procedure btnFilterClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure gridLogContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure gridLogDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure gridLogHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure gridLogSelection(Sender: TObject; aCol, aRow: Integer);
    procedure MenuItem2Click(Sender: TObject);
    procedure mnuCopyShaClick(Sender: TObject);
    procedure radPatchMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure treeFilesExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure treeFilesSelectionChanged(Sender: TObject);
    procedure txtSearchChange(Sender: TObject);
    procedure txtSearchKeyPress(Sender: TObject; var Key: char);
  private
    fActive: boolean;
    fCachingScreen: Boolean;
    fConfig: IConfig;
    fFiltered: boolean;
    fGit: IGit;
    fGitMgr: TGitMgr;
    fhlHelper: THighlighterHelper;
    fItemIndices: TItemIndexArray;
    fLogCache: TLogCache;
    fGraphColumns: Integer;
    fRecvCount: Integer;
    fScreenRows: Integer;
    fWithArrows: boolean;
    fCommitBrowser: TCommitBrowser;
    fCurrentItem: TLogItem;
    fLastSelectedCommit: QWord;
    fLinkMgr: TLinkMgr;
    fRangeFirstCommit: string;
    procedure CheckSearchButtons;
    procedure LaunchGraphBuildingThread;
    procedure OnContextPopLogClick(Sender: TObject);
    procedure OnCopyCommitRange(Sender: TObject);
    procedure OnCreateBranchClick(Sender: TObject);
    procedure OnDeleteBranchClick(Sender: TObject);
    procedure OnDeleteRemoteBranchClick(Sender: TObject);
    procedure OnGraphBuilderDone(Sender: TObject);
    procedure OnLinkClick(sender: TObject; link: TTextChunksItem);
    procedure OnLogEvent(sender: TObject; thread: TLogThread; event: Integer; var interrupt: boolean);
    procedure OnDeleteTagClick(sender: TObject);
    procedure OnResetBranchClick(Sender: TObject);
    procedure OnSwitchBranchClick(Sender: TObject);
    procedure OnSwitchTagClick(sender: TObject);
    procedure OnCreateTagClick(sender: TObject);
    procedure OnMergeBranchClick(Sender: TObject);
    {$ifdef CutterMode}
    procedure OnCutLogRange(Sender: TObject);
    {$endif}
    procedure CopyToClipboard(what: Integer);
    function  LocateCommit(const commit: QWord): boolean;
    procedure LocateHead;
    procedure LocateParent;
    procedure LocateChild;
    function  LocateItemIndex(aIndex: Integer): boolean;
    procedure AddMergeBranchMenu;
    procedure AddTagsMenu;
    procedure AddExtraMenus;
    procedure AddCopyExtraMenus;
    procedure SetActive(AValue: boolean);
    procedure SetFiltered(AValue: boolean);
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
    procedure SearchOrFilter(txt: string);
    procedure SearchLog(txt: string; forward: boolean; startRow:Integer=-1; searchIn:TSetOfByte=[]);
    procedure FilterLog(txt: string);
    procedure LayoutLabels;
    function UnixTimestampToStr(ts: Int64): string;
    function CommitListFromIndexArray(arr: TIntArray; prefix:string=''; separator:string=','): string;
  protected
    property Filtered: boolean read fFiltered write SetFiltered;
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
  COPY_RANGE_FIRST  = 3;
  COPY_RANGE_LAST   = 4;

  ALL_INFO_TEMPLATE =
    'Commit: %s' + LineEnding +
    'Parents: %s' + LineEnding +
    'Childs: %s' + LineEnding +
    'References: %s' + LineEnding +
    'Author: %s <%s>' + LineEnding +
    'Commit Date: %s' + LineEnding +
    'Message: ' + LineEnding + LineEnding +
    '    %s';

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
  aColor: TColor;
  r: TRect;
  db: TDbIndex;
  flags:  TLineItemFlags;
  aItem: TLogItem;
  Chunks: TTextChunks;
  chunk: TTextChunksItem;
  aStyle: TFontStyles;
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
      x := aRect.Left + LOGCELL_LEFTMARGIN;
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
                aColor := GraphColumnsColors[ n mod GRAPH_MAX_COLORS];
                x := w + n * GRAPH_COLUMN_SEPARATOR;

                if lifNode in flags then begin

                  if ifReorder in iFlags then
                    aColor := GetHighLightColor(aColor, 90);
                  gridLog.Canvas.Pen.Color := aColor;
                  gridLog.Canvas.Brush.Color := aColor;

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

                  gridLog.Canvas.Pen.Color := aColor;
                  gridLog.Canvas.Brush.Color := aColor;

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
            Chunks := GetTextChunks(gridLog.Canvas, aRect, x, fGit.RefsMap, aItem.CommitOID, aItem.Subject);
            for chunk in Chunks do begin
              gridLog.Canvas.Brush.Style := chunk.brushStyle;
              gridLog.Canvas.Brush.Color := chunk.brushColor;
              gridLog.Canvas.Pen.Style := chunk.penStyle;
              gridLog.Canvas.Pen.Color := chunk.penColor;
              gridLog.Canvas.Pen.Width := chunk.penWidth;
              if chunk.itemType=tcitBox then gridLog.Canvas.Rectangle(chunk.r);
              gridLog.Canvas.Brush.Style := bsClear;
              gridLog.Canvas.Font.Color := chunk.fontColor;
              aStyle := gridLog.Canvas.Font.Style;
              if chunk.itemType=tcitLink then Include(aStyle, fsUnderline)
              else                            Exclude(aStyle, fsUnderline);
              gridLog.Canvas.Font.Style := aStyle;
              gridLog.Canvas.TextOut(chunk.r.Left + 3, chunk.r.Top, chunk.text);
            end;
            gridLog.Canvas.Brush.Style := bsSolid;
            gridLog.Canvas.Pen.Style := psSolid;
          end;

        COLTAG_AUTHOR: s := aItem.Author;
        COLTAG_SHA1: s := aItem.CommitOID;
        COLTAG_DATE: s := UnixTimestampToStr(aItem.CommiterDate);
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
  inRange: Boolean;
  aItem: TLogItem;
begin
  aIndex := gridLog.Row - gridLog.FixedRows;
  inRange := (aIndex>=0) and (aIndex<Length(fItemIndices));
  actGotoParent.Enabled := not filtered and inRange and (Length(fItemIndices[aIndex].parents)>0);
  actGotoChild.Enabled := not filtered and inRange and (Length(fItemIndices[aIndex].childs)>0);
  if filtered then begin
    // fItemIndices[aIndex].commit won't help here because when it's
    // filtered fItemIndices is not valid for the current records
    fLogCache.DbIndex.LoadItem(aIndex, aItem);
    if inRange then fLastSelectedCommit := OIDToQWord(aItem.CommitOID)
    else            fLastSelectedCommit := 0;
  end;

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

  actGotoParent.Enabled := not Filtered;
  actGotoHead.Enabled := not Filtered;
  actGotoChild.Enabled := not Filtered;

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

    AddCopyExtraMenus;
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

procedure TframeLog.btnFilterClick(Sender: TObject);
begin
  CheckSearchButtons;
  if (not Filtered) and (btnFilter.Down) and (txtSearch.Text<>'') then
    FilterLog(txtSearch.Text);
end;

procedure TframeLog.btnNextClick(Sender: TObject);
begin
  SearchLog(txtSearch.Text, true);
end;

procedure TframeLog.btnPrevClick(Sender: TObject);
begin
  SearchLog(txtSearch.Text, false);
end;

procedure TframeLog.btnSearchClick(Sender: TObject);
begin
  CheckSearchButtons;
end;

procedure TframeLog.btnStopClick(Sender: TObject);
begin
  btnStop.Tag := 1;
end;

procedure TframeLog.MenuItem2Click(Sender: TObject);
begin
  CopyToClipboard(COPY_ALL_INFO);
end;

procedure TframeLog.mnuCopyShaClick(Sender: TObject);
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

procedure TframeLog.txtSearchChange(Sender: TObject);
begin
  CheckSearchButtons;
end;

procedure TframeLog.txtSearchKeyPress(Sender: TObject; var Key: char);
begin
  if key=#13 then begin
    SearchOrFilter(txtSearch.Text);
    key := #0;
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
        fRecvCount := 0;
        fScreenRows := gridLog.height div gridLog.DefaultRowHeight - 1;
        lblInfo.Caption := 'start';
        lblInfo.Visible := true;
        if fLogCache.LogState=lsGetFirst then lblInfo.Font.Color := clGreen
        else                                  lblInfo.Font.Color := clRed;
        LayoutLabels;
      end;

    LOGEVENT_RECORD:
      begin
        if (fRecvCount=0) or (fRecvCount mod gblRecordsToUpdate = 0) then begin
          lblInfo.Caption := format('%s',[fLogCache.DbIndex.Info]);
          interrupt := btnStop.Visible and (btnStop.Tag=1);
        end;

        if (fRecvCount<fScreenRows*gblCacheScreens) and (gridLog.RowCount<fRecvCount) then begin
          fCachingScreen := true;
          if fRecvCount mod gblRecordsToRowCount = 0 then begin
            gridLog.RowCount := fLogCache.DbIndex.Count + gridLog.FixedRows;
            //if gridLog.RowCount>fScreenRows then
            //  LaunchGraphBuildingThread;
          end;
        end else
        if fCachingScreen then begin
          if gridLog.RowCount>fScreenRows then begin
            fCachingScreen := false;
            LaunchGraphBuildingThread;
          end;
        end;

        inc(fRecvCount);
      end;

    LOGEVENT_END:
      begin
        lblInfo.Caption := format('%s',[fLogCache.DbIndex.Info]);
        sleep(250);
        btnStop.Visible := false;
        lblInfo.Visible := false;
        LayoutLabels;
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

      if RunInteractive(fGit.Exe + ' reset ' + cmd, fGit.TopLevelDir, rsResetABranch, fGitMgr.Branch) > 0 then
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
    if RunInteractive(fGit.Exe + ' merge ' + info^.refName, fGit.TopLevelDir, rsMergingBranches, s) > 0 then begin
      // an error occurred
    end else begin
      // queue update status
      fGitMgr.UpdateStatus;
      // update refs
      fGitMgr.UpdateRefList;
    end;

  end;
end;

{$ifdef CutterMode}
procedure Tframelog.OnCutLogRange(Sender: TObject);
var
  arr: TIntArray = nil;
  i, ini, fin, cnt, p: Integer;
  M, Ix: TMemoryStream;
  sig: cardinal;
  aItem, bItem: TLogItem;
  aBuf: PChar;
  aLen: word;
  indxRec: TIndexRecord;
begin

  SetLength(arr, Length(fLogCache.DbIndex.Filter));
  Move(fLogCache.DbIndex.Filter[0], arr[0], Length(arr)*SizeOf(Integer));

  ini := gridLog.Selection.Top - gridLog.FixedRows;
  fin := gridLog.Selection.Bottom - gridLog.FixedRows;

  if Length(fItemIndices[ini].childs) >1 then raise Exception.Create('Ini has multiple childs');
  if Length(fItemIndices[fin].parents)>1 then raise Exception.Create('Fin has multiple parents');

  for i:=ini+1 to fin-1 do begin
    if Length(fItemIndices[i].childs) >1 then raise Exception.Create('The range crosses a split');
    if Length(fItemIndices[i].parents)>1 then raise Exception.Create('The range crosses a merge');
  end;

  cnt := fin - ini + 1;

  fItemIndices[ini - 1].parents := [ fin + 1 - cnt ];
  fItemIndices[ini - 1].iflags := [ ifBeforeCut];
  fItemIndices[fin + 1].childs  := [ ini - 1 ];
  fItemIndices[fin + 1].iflags := [ifAfterCut];

  Delete(arr, ini, cnt);
  Delete(fItemIndices, ini, cnt);
  fLogCache.DbIndex.ReplaceFilter(arr);
  gridLog.RowCount := gridLog.RowCount - cnt;

  gridLog.Row := ini + gridLog.FixedRows;

  M := TMemoryStream.Create;
  Ix := TMemoryStream.Create;
  try

    sig := NToBE((PGM_SIGNATURE shl 16) or PGM_VERSION);
    M.WriteDWord(sig);

    for i:=0 to Length(fItemIndices)-1 do begin
      fLogCache.DbIndex.LoadItem(fItemIndices[i].index, aItem);
      if ifBeforeCut in fItemIndices[i].iFlags then begin
        p := fItemIndices[i].parents[0];
        fLogCache.DbIndex.LoadItem(fItemIndices[p].index, bItem);
        aItem.ParentOID := bItem.CommitOID;
      end;

      TDbIndex.CacheBufferFromItem(aItem, aBuf, aLen);
      indxRec.offset := M.Position;
      M.WriteBuffer(aBuf^, aLen);
      FreeMem(aBuf);

      indxRec.size := aLen;
      Ix.WriteBuffer(indxRec, SIZEOF_INDEX);
    end;

    M.SaveToFile('lazgitgui.logcache');
    Ix.SaveToFile('lazgitgui.logindex');

  finally
    M.Free;
    Ix.Free;
  end;

  gridLog.Invalidate;

end;
{$endif}

procedure TframeLog.OnContextPopLogClick(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
begin
  ShowMessageFmt('You really got me: %s',[mi.Caption]);
end;

procedure TframeLog.OnCopyCommitRange(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
begin
  if mi.Tag = 0 then CopyToClipboard(COPY_RANGE_FIRST)
  else               CopyToClipboard(COPY_RANGE_LAST);
end;

procedure TframeLog.CheckSearchButtons;
begin
  btnPrev.Enabled := btnSearch.Down and (Length(txtSearch.Text)>0);
  btnNext.Enabled := btnPrev.Enabled;
end;

procedure TframeLog.LaunchGraphBuildingThread;
var
  gBuild: TGraphBuilderThread;
begin
  lblGraphBuild.Caption := rsBuildingGraph;
  lblGraphBuild.Font.Color := clBlue;
  lblGraphBuild.Visible := true;
  LayoutLabels;

  gBuild := TGraphBuilderThread.Create(fLogCache.DbIndex);
  gBuild.WithColumns := true;
  gBuild.FreeOnTerminate := true;
  gBuild.OnTerminate := @OnGraphBuilderDone;
  gBuild.Start;
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
    if RunInteractive(fGit.Exe + ' push ' + cmd, fGit.TopLevelDir, rsDeletingRemoteBranch, info^.refName) > 0 then
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
  lblGraphBuild.Visible := false;
  LayoutLabels;

  fItemIndices := thread.IndexArray;
  fGraphColumns := thread.MaxColumns;

  col := ColumnByTag(COLTAG_GRAPH);
  i := gridLog.Columns.IndexOf(col);
  gridLog.Columns[i].Width := GRAPH_LEFT_PADDING + (fGraphColumns-1)*GRAPH_COLUMN_SEPARATOR + GRAPH_RIGHT_PADDING;

  gridLog.Invalidate;
end;

procedure TframeLog.OnLinkClick(sender: TObject; link: TTextChunksItem);
begin
  case link.linkAction of
    'goto':
        SearchLog(link.linkDest, true, 0,[SEARCHIN_COMMIT]);

    'open':
      begin
        if link.linkDest='' then  OpenURL(link.text)
        else                      OpenURL(link.linkDest);
      end;
  end;
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
    COPY_RANGE_FIRST:
      begin
        // do not translate !
        fRangeFirstCommit := fCurrentItem.CommitOID;
        s := 'CommitStart=' + fRangeFirstCommit;
      end;
    COPY_RANGE_LAST:
      begin
        // do not translate !
        s := format('CommitStart=%s'+ LineEnding +'CommitEnd=%s' ,
          [fRangeFirstCommit, fCurrentItem.CommitOID]);
        fRangeFirstCommit := '';
      end;
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
  mi.Caption := rsCreateABranchAtThisCommit;
  mi.OnClick := @OnCreateBranchClick;
  mi.Tag := 0;
  popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

  refItems := fGit.RefsFilter(fCurrentItem.CommitOID, @FilterLocal);
  for i := 0 to Length(refItems)-1 do begin
    mi := TMenuItem.Create(Self.Owner);
    mi.Caption := format(rsMergeSToS, [QuotedStr(refItems[i]^.refName), QuotedStr(fGitMgr.Branch)]);
    mi.OnClick := @OnMergeBranchClick;
    mi.Tag := PtrInt(refItems[i]);
    popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

    mi := TMenuItem.Create(Self.Owner);
    mi.Caption := format(rsSwitchToS, [QuotedStr(refItems[i]^.refName)]);
    mi.OnClick := @OnSwitchBranchClick;
    mi.Tag := PtrInt(refItems[i]);
    popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

    mi := TMenuItem.Create(Self.Owner);
    mi.Caption := format(rsDeleteBranchS, [QuotedStr(refItems[i]^.refName)]);
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
    mi.Caption := format(rsDeleteRemoteBranchS, [QuotedStr(refItems[i]^.refName)]);
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
  mi.Caption := rsCreateATagAtThisCommit;
  mi.OnClick := @OnCreateTagClick;
  mi.Tag := 0;
  popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

  refItems := fGit.RefsFilter(fCurrentItem.CommitOID, @Filter);
  for i := 0 to Length(refItems)-1 do begin

    mi := TMenuItem.Create(Self.Owner);
    mi.Caption := format(rsSwitchToTagS, [QuotedStr(refItems[i]^.refName)]);
    mi.OnClick := @OnSwitchTagClick;
    mi.Tag := PtrInt(refItems[i]);
    popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

    mi := TMenuItem.Create(Self.Owner);
    mi.Caption := format(rsDeleteTagS, [QuotedStr(refItems[i]^.refName)]);
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
  mi.Caption := format(rsResetSToThisCommit, [QuotedStr(fGitMgr.Branch)]);
  mi.OnClick := @OnResetBranchClick;
  mi.Tag := 0;
  popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

  {$ifdef CutterMode}
  mi := TMenuItem.Create(Self.Owner);
  mi.Caption := 'Cut Log Range';
  mi.OnClick := @OnCutLogRange;
  mi.Tag := 0;
  popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);
  {$endif}
end;

procedure TframeLog.AddCopyExtraMenus;
var
  mi: TMenuItem;
  mIndex: Integer;
begin

  // cleanup destination
  mIndex := mnuCopy.Count-1;
  while mIndex>mnuCopySha.MenuIndex do begin
    mnuCopy.Delete(mIndex);
    dec(mIndex);
  end;

  mi := TMenuItem.Create(Self.Owner);
  if fRangeFirstCommit='' then begin
    mi.Caption := 'Copy SHA''s range - First (newer)';
    mi.Tag := 0;
  end else begin
    mi.Caption := 'Copy SHA''s range - Last (older)';
    mi.Tag := 1;
  end;
  mi.OnClick := @OnCopyCommitRange;
  mnuCopy.Insert(mnuCopySha.MenuIndex + 1, mi);
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

    if fLinkMgr=nil then begin
      fLinkMgr := TLinkMgr.Create(gridLog, COLTAG_SUBJECT);
      fLinkMgr.LogCache := fLogCache;
      fLinkMgr.Git := fGit;
      fLinkMgr.OnLinkClick := @OnLinkClick;
    end;

    {$ifdef CutterMode}
    gridLog.Options := gridLog.Options + [goRangeSelect];
    {$endif}

  end;

  fActive := AValue;

  if fActive then
    fGitMgr.UpdateRefList;

end;

procedure TframeLog.SetFiltered(AValue: boolean);
var
  col: TGridColumn;
begin
  if fFiltered = AValue then Exit;
  fFiltered := AValue;

  // todo: save only the row the first time is filtered
  if fFiltered then begin
    btnFilter.Tag := gridLog.Row;
  end else
    fLogCache.DbIndex.SetFilter(nil);

  gridLog.RowCount := fLogCache.DbIndex.Count + gridLog.FixedRows;

  col := ColumnByTag(COLTAG_GRAPH);
  col.Visible := fFiltered=false;

  if not fFiltered then begin
    if fLastSelectedCommit>0 then LocateCommit(fLastSelectedCommit)
    else                          gridLog.Row := btnFilter.Tag;
  end;
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

        // force cache invalidation
        if ssShift in KeyboardStateToShiftState then
          gblInvalidateCache := true;

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
    txtViewer.Lines.Text := rsUnableToLocateDbIndex;
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
  n, aIndex: Integer;
  arr: TRefInfoArray;
  ref: PRefInfo;
  dt: TDateTime;
  parents, childs: string;
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

    aIndex := gridLog.Row - gridLog.FixedRows;
    if (aIndex>=0) and (aIndex<Length(fItemIndices)) then begin
      parents := CommitListFromIndexArray(fItemIndices[aIndex].parents, '', ', ');
      childs  := CommitListFromIndexArray(fItemIndices[aIndex].childs, '', ', ');
    end else begin
      parents := ParentOID;
      childs := '';
    end;

    dt := UnixToDateTime(CommiterDate, false);
    result := format(ALL_INFO_TEMPLATE, [
      CommitOID, parents, childs, result,
      Author, Email,
      format('%s (%d)',[DateTimeToStr(dt), CommiterDate]),
      Subject]);
  end;
end;

function TframeLog.CommitListFromIndexArray(arr: TIntArray; prefix: string;
  separator: string): string;
var
  aIndex: Integer;
begin
  result := '';
  for aIndex in arr do begin
    if result<>'' then result += separator;
    result += prefix + Lowercase(format('%.16x', [fItemIndices[aIndex].commit]));
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

procedure TframeLog.SearchOrFilter(txt: string);
begin
  if btnSearch.Down then  SearchLog(txt, true)
  else                    FilterLog(txt);
end;

procedure TframeLog.SearchLog(txt: string; forward: boolean; startRow: Integer;
  searchIn: TSetOfByte);
var
  L: TStringList;
  i, delta, aRow, aIndex, anyRow, count: Integer;
  aItem: TLogItem;
  found: boolean;
begin

  if SearchIn=[] then
    SearchIn := [SEARCHIN_COMMIT, SEARCHIN_AUTHOR, SEARCHIN_SUBJECT, SEARCHIN_DATE];

  L := TStringList.Create;
  try
    L.DelimitedText := lowercase(txt);
    if forward then delta := 1 else delta := -1;
    if startRow<0 then aRow := gridLog.Row
    else               aRow := startRow;

    anyRow := -1;
    aRow := aRow + delta;
    while (aRow >= gridLog.FixedRows) and (aRow <= gridLog.RowCount-1) do begin
      aIndex := aRow - gridLog.FixedRows;
      fLogCache.DbIndex.LoadItem(aIndex, aItem);

      count := 0;
      for i:=0 to L.Count-1 do begin

        found := (SEARCHIN_COMMIT in SearchIn) and (pos(L[i], aItem.CommitOID)>0);
        if not found then
          found := (SEARCHIN_AUTHOR in SearchIn) and (pos(L[i], lowercase(aItem.author))>0);
        if not found then
          found := (SEARCHIN_SUBJECT in SearchIn) and (pos(L[i], lowercase(aItem.Subject))>0);
        if not found then
          found := (SEARCHIN_DATE in SearchIn) and (pos(L[i], lowercase(UnixTimestampToStr(aItem.CommiterDate)))>0);

        if found then begin
          if (anyRow<0) then
            anyRow := aIndex;
          inc(count);
        end;

      end;

      if count=L.Count then begin
        // found all terms
        gridLog.Row := aRow;
        btnNext.Enabled := true;
        btnPrev.Enabled := true;
        exit;
      end;

      aRow := aRow + delta;
    end;

    // not found, but check anyRow
    if anyRow>=gridLog.FixedRows then begin
      // partial find
      gridLog.Row := aRow;
      btnNext.Enabled := true;
      btnPrev.Enabled := true;
    end;

    ShowMessageFmt('Could''t find "%s"',[txt]);

  finally
    L.Free;
  end;
end;

procedure TframeLog.FilterLog(txt: string);
var
  L: TStringList;
  i, aIndex, count, total: Integer;
  aItem: TLogItem;
  found: boolean;
  filterArray: TIntArray;
  needle: String;
begin

  if txt='' then begin
    Filtered := false;
    exit;
  end;

  L := TStringList.Create;
  try
    L.DelimitedText := lowercase(txt);

    filterArray := nil;
    SetLength(filterArray, fLogCache.DbIndex.Count{(true)});

    total := 0;
    aIndex := 0;
    while aIndex<fLogCache.DbIndex.Count{(true)} do begin
      fLogCache.DbIndex.LoadItem(aIndex, aItem{, true});

      count := 0;
      for i:=0 to L.Count-1 do begin
        needle := L[i];
        found := pos(needle, aItem.CommitOID)>0;
        if not found then
          found := pos(needle, lowercase(aItem.author))>0;
        if not found then
          found := pos(needle, lowercase(aItem.Subject))>0;
        if not found then
          found := pos(needle, lowercase(UnixTimestampToStr(aItem.CommiterDate)))>0;

        if found then
          inc(count);
      end;

      if L.Count=count then begin
        filterArray[total] := fLogCache.DbIndex.GetIndex(aIndex);
        inc(total);
      end;

      aIndex += 1;
    end;

    SetLength(filterArray, total);
    if total=0 then
      exit;

    fLogCache.DbIndex.SetFilter(filterArray);
    fFiltered := false;
    Filtered := true;
  finally
    L.Free;
  end;
end;

procedure TframeLog.LayoutLabels;
var
  aControl: TControl;
begin

  if lblInfo.Visible then begin
    if btnStop.Visible then aControl := btnStop
    else                    aControl := btnReload;
    lblInfo.AnchorSideRight.Control := aControl;
  end;

  if lblGraphBuild.Visible then begin
    if lblInfo.Visible then aControl := lblInfo else
    if btnStop.Visible then aControl := btnStop
    else                    aControl := btnReload;
    lblGraphBuild.AnchorSideRight.Control := aControl;
  end;

end;

function TframeLog.UnixTimestampToStr(ts: Int64): string;
begin
  // TODO: an option to pick user date time format
  result := DateTimeToStr(UnixToDateTime(ts, false));
end;

procedure TframeLog.Clear;
begin
  FreeAndNil(fLogCache);
  FreeAndNil(fCommitBrowser);
  FreeAndNil(fLinkMgr);
end;

procedure TframeLog.UpdateGridRows;
begin
  gridLog.RowCount := fLogCache.DbIndex.Count + gridLog.FixedRows;

  LaunchGraphBuildingThread;
end;

end.

