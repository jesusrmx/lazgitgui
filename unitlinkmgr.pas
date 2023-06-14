unit unitlinkmgr;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Grids,
  unitifaces, unittextchunks, unitlogcache, unitdbindex;

const
  LOGCELL_LEFTMARGIN  = 7;

type

  TLinkClickEvent = procedure(sender:TObject; link: TTextChunksItem) of object;

  { TLinkMgr }

  TLinkMgr = class
  private
    fClickingLink: Boolean;
    fGit: IGit;
    fGrid: TDrawGrid;
    fLogCache: TLogCache;
    fOnLinkClick: TLinkClickEvent;
    fRowTextChunks: TTextChunks;
    fLastHoverRow: LongInt;
    fPointedChunkIndex: Integer;
    fColTag: Integer;
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  public
    constructor create(aGrid: TDrawGrid; aColTag: Integer);

    property LogCache: TLogCache read fLogCache write fLogCache;
    property Git: IGit read fGit write fGit;
    property OnLinkClick: TLinkClickEvent read fOnLinkClick write fOnLinkClick;
  end;

implementation

{ TLinkMgr }

constructor TLinkMgr.create(aGrid: TDrawGrid; aColTag: Integer);
begin
  inherited Create;
  fGrid := aGrid;
  fGrid.OnMouseDown := @GridMouseDown;
  fGrid.OnMouseMove := @GridMouseMove;
  fGrid.OnMouseUp   := @GridMouseUp;
  fGrid.OnSelectCell := @GridSelectCell;
  fColTag := aColTag;
end;

procedure TLinkMgr.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fClickingLink := (fGrid.Cursor=crHandPoint) and
                   (fPointedChunkIndex>=0) and (fPointedChunkIndex<Length(fRowTextChunks));
end;

procedure TLinkMgr.GridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  col: TGridColumn;
  aCol, aRow: Longint;
  aIndex, i: Integer;
  aItem: TLogItem;
  aRect: TRect;
  aType: TTextChunksItemType;
  aCursor: TCursor;
begin
  fGrid.MouseToCell(x, y, aCol, aRow);
  if aRow<=fGrid.FixedRows then
    exit;
  col := fGrid.Columns[aCol];
  if (col<>nil) and (col.Tag=fColTag) then begin
    if aRow<>fLastHoverRow then begin
      aIndex := aRow - fGrid.FixedRows;
      fLogCache.DbIndex.LoadItem(aIndex, aItem);
      aRect := fGrid.CellRect(aCol, aRow);
      fRowTextChunks := GetTextChunks(fGrid.Canvas, aRect, aRect.Left + LOGCELL_LEFTMARGIN, fGit.RefsMap, aItem);
    end;

    for i:=0 to Length(fRowTextChunks)-1 do
    with fRowTextChunks[i] do begin
      if r.Contains(Point(x, y)) then begin
        case itemType of
          tcitNone: aCursor := crDefault;
          tcitBox:  aCursor := crNoDrop;
          tcitLink: aCursor := crHandPoint;
        end;
        if fGrid.Cursor<>aCursor then begin
          fGrid.Cursor := aCursor;
          fPointedChunkIndex := i;
        end;
        exit;
      end;
    end;
    fLastHoverRow := aRow;
  end;
  if fGrid.Cursor<>crDefault then
    fGrid.Cursor := crDefault;
end;

procedure TLinkMgr.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  s: string;
begin
  if fClickingLink then begin
    fClickingLink := false;
    if Assigned(fOnLinkClick) then
      fOnLinkClick(Self, fRowTextChunks[fPointedChunkIndex]);
  end;
end;

procedure TLinkMgr.GridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := not fClickingLink;
end;

end.

