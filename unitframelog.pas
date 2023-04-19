unit unitframelog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dateUtils, fgl,
  LazLogger, SynEdit, Graphics, Forms, Dialogs, Controls, Grids,
  ExtCtrls, ComCtrls, Menus, Types, Clipbrd,
  unitlogcache, unitdbindex, unitgitutils, unitgit, unitifaces;

const
  GRAPH_LEFT_PADDING          = 12;
  GRAPH_RIGHT_PADDING         = 12;
  GRAPH_LINE_WIDTH            = 2;
  GRAPH_NODE_RADIUS           = 4;
  GRAPH_COLUMN_SEPARATOR      = 18;

type

  TRefsMap = specialize TFPGMap<string, TRefInfoArray>;

  { TframeLog }

  TframeLog = class(TFrame)
    gridLog: TDrawGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    panBrowser: TPanel;
    panLogTools: TPanel;
    panFiles: TPanel;
    popLog: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TreeView1: TTreeView;
    txtViewer: TSynEdit;
    procedure gridLogContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure gridLogDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
  private
    fConfig: IConfig;
    fGit: TGit;
    fItemIndices: TItemIndexArray;
    fLogCache: TLogCache;
    fOnLogCacheEvent: TLogThreadEvent;
    fPopupMousePos: TPoint;
    fGraphColumns: Integer;
    procedure OnLogEvent(sender: TObject; thread: TLogThread; event: Integer; var interrupt: boolean);
    procedure CopyToClipboard(what: Integer; aRow: Integer);
    function GetPopMenuRow: Integer;
    procedure LocateHead;
  public
    procedure Start;
    procedure Clear;
    procedure UpdateGridRows;

    property LogCache: TLogCache read fLogCache write fLogCache;
    property Config: IConfig read fConfig write fConfig;
    property Git: TGit read fGit write fGit;
    property OnLogCacheEvent: TLogThreadEvent read fOnLogCacheEvent write fOnLogCacheEvent;

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
                    // draw a merge line with origin at source and dest at this point
                    gridLog.Canvas.Line(x, y, x, y2);
                    gridLog.Canvas.Line(x-GRAPH_NODE_RADIUS-1, y, x, y);
                    x1 := w + fItemIndices[lines[i].source].column * GRAPH_COLUMN_SEPARATOR;
                    gridLog.Canvas.Line(x1, y, x, y);
                  end;

                  if lifBorn in flags then begin
                    // draw a new born line with origin at source and dest at this point
                    gridLog.Canvas.Line(x, y1, x, y);
                    gridLog.Canvas.Line(x-GRAPH_NODE_RADIUS-1, y, x, y);
                    x1 := w + fItemIndices[lines[i].source].column * GRAPH_COLUMN_SEPARATOR;
                    gridLog.Canvas.Line(x1, y, x, y);
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

      gridLog.Canvas.TextOut(x, aRect.Top, s);
    end;
  end;
end;

procedure TframeLog.gridLogContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  fPopupMousePos := MousePos;
end;

procedure TframeLog.MenuItem2Click(Sender: TObject);
begin
  CopyToClipboard(COPY_ALL_INFO, GetPopMenuRow);
end;

procedure TframeLog.MenuItem3Click(Sender: TObject);
begin
  CopyToClipboard(COPY_SHA, GetPopMenuRow)
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

procedure TframeLog.CopyToClipboard(what: Integer; aRow: Integer);
var
  aIndex, n: Integer;
  s: String;
  dt: TDateTime;
  arr: TRefInfoArray;
  ref: PRefInfo;
begin
  aIndex := aRow - gridLog.FixedRows;
  if (aIndex>=0) and (aIndex<fLogCache.DbIndex.Count) then begin
    if not fLogCache.DbIndex.LoadItem(aIndex) then  begin
      ShowMessage('Unable to locate the log record');
      exit;
    end;

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
end;

function TframeLog.GetPopMenuRow: Integer;
var
  p: TPoint;
begin
  p := gridLog.MouseToCell(fPopupMousePos);
  result := p.y;
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

procedure TframeLog.Start;
begin
  if fLogCache=nil then begin
    fLogCache := TLogCache.Create(@OnLogEvent);
    fLogCache.Git := fGit;
    fLogCache.Config := fConfig;
  end;

  gridLog.RowCount := (gridLog.Height div gridLog.DefaultRowHeight) *  2;

  fGit.UpdateRefList;

  fLogCache.LoadCache;

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
