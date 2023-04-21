unit unitframelog;

{$mode ObjFPC}{$H+}

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
  Classes, SysUtils, dateUtils, fgl,
  LazLogger, SynEdit, Graphics, Forms, Dialogs, Controls, Grids,
  ExtCtrls, ComCtrls, Menus, Types, Clipbrd, ActnList,
  unitgittypes, unitlogcache, unitdbindex, unitgitutils, unitifaces,
  unitruncmd, unitgitmgr;

const
  GRAPH_LEFT_PADDING          = 12;
  GRAPH_RIGHT_PADDING         = 12;
  GRAPH_LINE_WIDTH            = 2;
  GRAPH_NODE_RADIUS           = 4;
  GRAPH_COLUMN_SEPARATOR      = 18;

  ARROWLEN_X                  = GRAPH_NODE_RADIUS;
  ARROWLEN_Y                  = GRAPH_NODE_RADIUS;

type

  TRefsMap = specialize TFPGMap<string, TRefInfoArray>;
  TRefItem = record
    mapIndex: Integer;
    arrIndex: Integer;
  end;

  { TframeLog }

  TframeLog = class(TFrame, IObserver)
    actGotoHead: TAction;
    actShowChanges: TAction;
    actLstLog: TActionList;
    gridLog: TDrawGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuGotoHead: TMenuItem;
    panBrowser: TPanel;
    panLogTools: TPanel;
    panFiles: TPanel;
    popLog: TPopupMenu;
    mnuSeparatorLast: TMenuItem;
    mnuSeparatorFirst: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TreeView1: TTreeView;
    txtViewer: TSynEdit;
    procedure actGotoHeadExecute(Sender: TObject);
    procedure actShowChangesExecute(Sender: TObject);
    procedure gridLogContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure gridLogDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
  private
    fActive: boolean;
    fConfig: IConfig;
    fGit: IGit;
    fGitMgr: TGitMgr;
    fItemIndices: TItemIndexArray;
    fLogCache: TLogCache;
    fOnLogCacheEvent: TLogThreadEvent;
    fGraphColumns: Integer;
    fRefItems: array of TRefItem;
    fWithArrows: boolean;
    procedure OnContextPopLogClick(Sender: TObject);
    procedure OnLogEvent(sender: TObject; thread: TLogThread; event: Integer; var interrupt: boolean);
    procedure CopyToClipboard(what: Integer);
    procedure LocateHead;
    function LocateItemIndex(aIndex: Integer): boolean;
    procedure AddMergeBranchMenu;
    procedure OnMergeBranchClick(Sender: TObject);
    procedure SetActive(AValue: boolean);
    procedure SetGitMgr(AValue: TGitMgr);
    procedure ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
  public
    procedure Clear;
    procedure UpdateGridRows;

    property LogCache: TLogCache read fLogCache write fLogCache;
    property Config: IConfig read fConfig write fConfig;
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property OnLogCacheEvent: TLogThreadEvent read fOnLogCacheEvent write fOnLogCacheEvent;
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

procedure DrawLine(canvas: TCanvas; x1, y1, x2, y2: Integer; withArrow:boolean);
var
  x, y, o: Integer;
begin
  canvas.Line(x1, y1, x2, y2);
  if withArrow then begin
    o := canvas.pen.width;
    canvas.pen.width := 1;
    x := x2;
    y := y2;
    if x2<x1 then begin
      x := x + GRAPH_NODE_RADIUS;
      canvas.Line(x, y - GRAPH_LINE_WIDTH, x + ARROWLEN_X, y - ARROWLEN_Y - GRAPH_LINE_WIDTH - 1);
      canvas.Line(x, y, x + ARROWLEN_X, y + ARROWLEN_Y + 1);
    end else begin
      x := x - GRAPH_LINE_WIDTH;
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
        'Graph':
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
                    DrawLine(gridLog.Canvas, x, y, x1, y, fWithArrows);
                  end;

                  if lifBorn in flags then begin
                    // draw a new born line with origin at source and dest at this point
                    gridLog.Canvas.Line(x, y1, x, y);
                    x1 := w + fItemIndices[lines[i].source].column * GRAPH_COLUMN_SEPARATOR;
                    DrawLine(gridLog.Canvas, x1, y, x, y, fWithArrows);
                  end;
                end;
              end;
              gridLog.Canvas.Pen.Width := 1;
            end;
          end;

        'Subject':
          begin
            s := db.Item.Subject;
            if (fGit.RefsMap<>nil) and fGit.RefsMap.Find(db.Item.CommitOID, n ) then begin
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
        'Author': s := db.Item.Author;
        'SHA1': s := db.Item.CommitOID;
        'Date': s := IntToStr(db.Item.CommiterDate);
        else  s := '';
      end;

      gridLog.Canvas.Brush.Style := bsClear;
      gridLog.Canvas.TextOut(x, aRect.Top, s);
      gridLog.Canvas.Brush.Style := bsSolid;
    end;
  end;
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
    aIndex := mnuSeparatorFirst.Tag + 1;
    mnuSeparatorFirst.Tag := aIndex;
    // all is ok, cleanup
    mIndex := mnuSeparatorFirst.MenuIndex+1;
    while mIndex<>mnuSeparatorLast.MenuIndex do
      popLog.Items.Delete(mIndex);
    // add sample menus just for testing
    mi := TMenuItem.Create(Self.Owner);
    mi.Action := actShowChanges;
    popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);

    AddMergeBranchMenu;
  end;
end;

procedure TframeLog.actGotoHeadExecute(Sender: TObject);
begin
  LocateHead;
end;

procedure TframeLog.actShowChangesExecute(Sender: TObject);
begin
  ShowMessage('Showing Changes');
end;

procedure TframeLog.MenuItem2Click(Sender: TObject);
begin
  CopyToClipboard(COPY_ALL_INFO);
end;

procedure TframeLog.MenuItem3Click(Sender: TObject);
begin
  CopyToClipboard(COPY_SHA)
end;

procedure TframeLog.OnLogEvent(sender: TObject; thread: TLogThread;
  event: Integer; var interrupt: boolean);
var
  s: string;
begin

  if Assigned(fOnLogCacheEvent) then
    fOnLogCacheEvent(Sender, thread, event, interrupt);

  case event of

    LOGEVENT_RECORD:
      begin
        if fLogCache.DbIndex.Count<gridLog.height div gridLog.DefaultRowHeight then begin
          if fLogCache.DbIndex.Count mod 3 = 0 then
            gridLog.Invalidate;
        end;
      end;

    LOGEVENT_END:
      begin
        UpdateGridRows;
        LocateHead;
      end;

    LOGEVENT_DONE:
      begin
      end;
  end;
end;

procedure TframeLog.OnContextPopLogClick(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
begin
  ShowMessage(format('You really got me: %s',[mi.Caption]));
end;

procedure TframeLog.CopyToClipboard(what: Integer);
var
  n: Integer;
  s: String;
  dt: TDateTime;
  arr: TRefInfoArray;
  ref: PRefInfo;
begin

    s := '';
    with fLogCache.DbIndex.Item do
      case what of
        COPY_ALL_INFO:
          begin
            s := '';
            if (fGit.RefsMap<>nil) and fGit.RefsMap.Find(CommitOID, n ) then begin
              arr := fGit.RefsMap.Data[n];
              for ref in arr do begin
                if s<>'' then s+=', ';
                if ref^.subType=rostTag then
                  s += 'tag: ';
                s += ref^.refName
              end;
            end;
            dt := UnixToDateTime(CommiterDate, false);
            s := format(ALL_INFO_TEMPLATE, [
              ParentOID, CommitOID, s,
              Author, Email,
              format('%s (%d)',[DateTimeToStr(dt), CommiterDate]),
              Subject]);
          end;
        COPY_SHA:
          s := CommitOID;
      end;

    if s<>'' then
      clipboard.AsText := s;
end;

procedure TframeLog.LocateHead;
var
  commit: QWord;
  i: Integer;
begin
  commit := OIDToQWord(fGit.BranchOID);
  for i:=0 to Length(fItemIndices)-1 do begin
    if commit=fItemIndices[i].commit then begin
      gridLog.Row :=  gridLog.FixedRows + i;
      break;
    end;
  end;
end;

function TframeLog.LocateItemIndex(aIndex: Integer): boolean;
begin
  result := (aIndex>=0) and (aIndex<fLogCache.DbIndex.Count);
  result := result and fLogCache.DbIndex.LoadItem(aIndex);
end;

procedure TframeLog.AddMergeBranchMenu;
var
  headCommit, curCommit: QWord;
  mi: TMenuItem;
  n, i, j: Integer;
  arr: TRefInfoArray;
begin
  headcommit := OIDToQWord(fGit.BranchOID);
  curCommit := OIDToQWord(fLogCache.DbIndex.Item.CommitOID);
  if (headCommit=curCommit) or (fGit.RefsMap=nil) then
    exit;
  if fGit.RefsMap.Find(fLogCache.DbIndex.Item.CommitOID, n ) then begin
    arr := fGit.RefsMap.Data[n];
    fRefItems := nil;
    for i:=0 to Length(arr)-1 do begin
      if arr[i]^.subType=rostLocal then begin
        if arr[i]^.head then continue;

        j := Length(fRefItems);
        SetLength(fRefItems, j+1);
        fRefItems[j].mapIndex := n;
        fRefItems[j].arrIndex := i;

        mi := TMenuItem.Create(Self.Owner);
        mi.Caption := format('Merge %s to %s',[QuotedStr(arr[i]^.refName), QuotedStr(fGit.Branch)]);
        mi.OnClick := @OnMergeBranchClick;
        mi.Tag := j;
        popLog.Items.Insert(mnuSeparatorLast.MenuIndex, mi);
      end;
    end;
  end;
end;

procedure TframeLog.OnMergeBranchClick(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
  j: Integer;
  arr: TRefInfoArray;
  s: string;
begin
  j := mi.Tag;
  if (j>=0) and (j<Length(fRefItems)) then begin
    arr := fGit.RefsMap.Data[ fRefItems[j].mapIndex ];
    j := fRefItems[j].arrIndex;
    s := 'git merge '+ arr[j]^.refName;
    if RunInteractive(fGit.Exe + ' merge '+ arr[j]^.refName, fGit.TopLevelDir, 'Merging branches', s)>0 then begin
      // an error occurred
    end else begin
      //// queue update status
      //fGitMgr.UpdateStatus;
      // update refs
      fGitMgr.UpdateRefList;
    end;
  end;
end;

procedure TframeLog.SetActive(AValue: boolean);
begin
  if fActive = AValue then Exit;

  if not fActive then begin

    if fLogCache=nil then begin
      fLogCache := TLogCache.Create(@OnLogEvent);
      fLogCache.GitMgr := fGitMgr;
      fLogCache.Config := fConfig;
    end;

    fWithArrows := fConfig.ReadBoolean('DrawArrows', true);

    gridLog.RowCount := (gridLog.Height div gridLog.DefaultRowHeight) *  2;

  end;

  if AValue then
    fGitMgr.UpdateRefList;

  fActive := AValue;
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
  end;
end;

procedure TframeLog.ObservedChanged(Sender: TObject; what: Integer; data: PtrInt
  );
begin
  case what of
    GITMGR_EVENT_REFLISTCHANGED:
      if fActive then begin
        fLogCache.LoadCache;
        //// update graph (reload the log)
        //fItemIndices := nil;
        //UpdateGridRows;
      end;
  end;
end;

procedure TframeLog.Clear;
begin
  FreeAndNil(fLogCache);
end;

procedure TframeLog.UpdateGridRows;
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

end.

