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

  Column Scroller.
  Given a drawgrid, this class handles a column built-in scroller so
  If the column is given a max width and the column width is wider
  it handles scrolling in this column
}
unit unitcolumnscroller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  LazLoggerBase, Graphics, Controls, Grids,
  unitifaces, unittextchunks, unitlogcache, unitdbindex, unitgitmgr;

type

  { TSlider }

  TSlider = class
  private
    fBase: TRect;
    fColTag: Integer;
    fDrag: Boolean;
    fDragX: Integer;
    fDragY: Integer;
    fSliderCol, fSliderRow: Integer;
    fGrid: TDrawGrid;
    fDirty: boolean;
    fVisible: boolean;
    fVisibleCol: LongInt;
    fVisibleRow: LongInt;
    fRect: TRect;
    fActive: boolean;
    fEnabled: boolean;
    fWidth: Integer;
    function IsSliderCell(aCol, aRow: Integer): boolean;
    function IsSliderVisible(aCol: Integer): boolean;
    procedure SetEnabled(AValue: boolean);
    procedure SetWidth(AValue: Integer);
    procedure UpdateBounds(aRect: TRect);
    procedure UpdateCol;
  public
    constructor create(aGrid: TDrawGrid; aColTag: Integer);

    procedure Draw(aRect: TRect; aCol,aRow: Integer);
    procedure MouseDown(x, y: Integer);
    procedure MouseMove(x, y: Integer);
    procedure MouseUp(x, y: Integer);
    procedure Invalidate;

    property Active: boolean read fActive;
    property Enabled: boolean read fEnabled write SetEnabled;
    property Width: Integer read fWidth write SetWidth;
  end;

  { TColumnScroller }

  TColumnScroller = class
  private
    fGrid: TDrawGrid;
    fOffsetX: integer;
    fOrgOnDrawCell: TOnDrawCell;
    fOrgOnMouseDown: TMouseEvent;
    fOrgOnMouseMove: TMouseMoveEvent;
    fOrgOnMouseUp: TMouseEvent;
    fOrgOnMouseWheel: TMouseWheelEvent;
    fOrgOnSelectCell: TOnSelectCellEvent;
    fSlider: TSlider;
    fWidth: Integer;
    function GetWidth: Integer;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create(aGrid: TDrawGrid; aColTag: Integer);

    property OffsetX: integer read fOffsetX write fOffsetX;
    property Width: Integer read GetWidth write SetWidth;
  end;

implementation

const
  SLIDER_COLOR_INACTIVE     = clMoneyGreen; // TColor($100010);
  SLIDER_COLOR_ACTIVE       = clSkyBlue; //TColor($800080);

function ColumnByTag(aGrid:TDrawGrid; aTag: Integer): TGridColumn;
var
  i: Integer;
begin
  result := nil;
  for i:=0 to aGrid.Columns.Count-1 do
    if aGrid.Columns[i].Tag=aTag then begin
      result := aGrid.Columns[i];
      break;
    end;
end;

{ TSlider }

function TSlider.IsSliderCell(aCol, aRow: Integer): boolean;
begin
  result := (aCol=fSliderCol) and (aRow=fSliderRow);
end;

function TSlider.IsSliderVisible(aCol: Integer): boolean;
begin
  result := aCol=fSliderCol;
end;

procedure TSlider.SetEnabled(AValue: boolean);
begin
  if fEnabled = AValue then Exit;
  fEnabled := AValue;
  Invalidate;
end;

procedure TSlider.SetWidth(AValue: Integer);
begin
  if fWidth = AValue then Exit;
  fDirty := true;
  fWidth := AValue;
  UpdateCol;
  Invalidate;
end;

constructor TSlider.create(aGrid: TDrawGrid; aColTag: Integer);
begin
  inherited Create;
  fGrid := aGrid;
  fDirty := true;
  fVisible := false;
  fEnabled := true;
  fColTag := aColTag;
end;

procedure TSlider.UpdateBounds(aRect: TRect);
begin
  fRect := aRect;
  fBase := aRect;

  fRect.Left := fRect.Left + aRect.Width div 4;
  fRect.Right := fRect.Right - aRect.Width div 4;
  fRect.Bottom := fRect.Bottom - 2;
  fRect.Top := fRect.Bottom - 7;

  fDirty := false;
end;

procedure TSlider.UpdateCol;
var
  column: TGridColumn;
begin
  column := ColumnByTag(fGrid, fColTag);
  if (column<>nil) then
    fSliderCol := fGrid.FixedCols + column.Index;
end;

procedure TSlider.Draw(aRect: TRect; aCol, aRow: Integer);
begin

  if not fEnabled then
    exit;

  fSliderRow := fGrid.TopRow + fGrid.VisibleRowCount - 1;
  if IsSliderCell(aCol, aRow) then begin
    if fDirty then UpdateBounds(aRect);
    if fVisible then begin
      fGrid.Canvas.Brush.Style := bsSolid;
      if fActive then fGrid.Canvas.Brush.Color := SLIDER_COLOR_ACTIVE
      else            fGrid.Canvas.Brush.Color := SLIDER_COLOR_INACTIVE;
      fGrid.Canvas.FillRect(fRect);
    end else begin
      fGrid.Canvas.Pen.Color := SLIDER_COLOR_INACTIVE;
      fGrid.Canvas.Pen.Width := 2;
      // draw scroll left arrow
      fGrid.Canvas.Line(aRect.Left + 10, aRect.Top    + 4, aRect.Left, aRect.Top + 9);
      fGrid.Canvas.Line(aRect.Left + 10, aRect.Bottom - 4, aRect.Left, aRect.Top + 9);
      // draw scroll right arrow
      fGrid.Canvas.Line(aRect.Right - 12, aRect.Top    + 4, aRect.Right-2, aRect.Top + 9);
      fGrid.Canvas.Line(aRect.Right - 12, aRect.Bottom - 4, aRect.Right-2, aRect.Top + 9);

      fGrid.Canvas.Pen.Width := 1;
    end;
  end;
end;

procedure TSlider.MouseDown(x, y: Integer);
begin
  fDrag := fActive;
  fDragX := x;
  fDragY := y;
end;

procedure TSlider.MouseMove(x, y: Integer);
var
  aCol, aRow, orgLeft: Longint;
  newVisible, ok: boolean;
  delta: Integer;
begin

  if fDrag then begin
    delta := x - fDragX;
    if Abs(delta)>2 then begin

      orgLeft := fRect.Left;
      fRect.Offset(delta, 0);
      if fRect.Right > fBase.Right then
        fRect.Offset(fBase.Right - fRect.Right, 0)
      else
      if fRect.Left < fBase.Left then
        fRect.Offset(fBase.Left - fRect.Left, 0);

      if orgLeft<>fRect.Left then
        Invalidate;

      fDragX := x;
      fDragY := y;
    end;
    exit;
  end;

  fGrid.MouseToCell(x, y, aCol, aRow);

  newVisible := IsSliderVisible(aCol);

  if newVisible <> fVisible then begin
    if fVisible then begin
      aCol := fVisibleCol;
      aRow := fVisibleRow;
    end else begin
      aCol := fSliderCol;
      aRow := fSliderRow;
    end;

    fGrid.InvalidateCell(aCol, aRow);

    fVisible := newVisible;
    if fVisible then begin
      fVisibleCol := aCol;
      fVisibleRow := aRow;
    end;
  end;

  if fVisible and IsSliderCell(aCol, aRow) then begin
    ok := fRect.Contains(Point(x, y));
    if ok<>fActive then begin
      fActive := ok;
      fGrid.InvalidateCell(aCol, aRow);
    end;
  end;
end;

procedure TSlider.MouseUp(x, y: Integer);
begin
  if fActive then
    Invalidate;
  fDrag := false;
end;

procedure TSlider.Invalidate;
begin
  fGrid.InvalidateCell(fSliderCol, fSliderRow);
end;

{ TColumnScroller }

procedure TColumnScroller.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(fOrgOnMouseDown) then
    fOrgOnMouseDown(Sender, Button, Shift, X, Y);

  fSlider.MouseDown(x, y);
end;

procedure TColumnScroller.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  r: TRect;
begin
  if Assigned(fOrgOnDrawCell) then
    fOrgOnDrawCell(Sender, aCol, aRow, aRect, aState);

  fSlider.Draw(aRect, aCol, aRow);
end;

function TColumnScroller.GetWidth: Integer;
begin
  result := fSlider.Width;
end;

procedure TColumnScroller.GridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(fOrgOnMouseMove) then
    fOrgOnMouseMove(Sender, Shift, X, Y);

  fSlider.MouseMove(x, y);
end;

procedure TColumnScroller.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(fOrgOnMouseUp) then
    fOrgOnMouseUp(Sender, Button, Shift, X, Y);

  fSlider.MouseUp(x, y);
end;

procedure TColumnScroller.GridMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  fSlider.Invalidate;
end;

procedure TColumnScroller.GridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := not fSlider.Active;
end;

procedure TColumnScroller.SetWidth(AValue: Integer);
begin
  fSlider.Width := Avalue;
end;

constructor TColumnScroller.Create(aGrid: TDrawGrid; aColTag: Integer);
begin
  fGrid := aGrid;

  fOrgOnMouseDown := fGrid.OnMouseDown;
  fOrgOnMouseMove := fGrid.OnMouseMove;
  fOrgOnMouseUp := fGrid.OnMouseUp;
  fOrgOnDrawCell := fGrid.OnDrawCell;
  fOrgOnSelectCell := fGrid.OnSelectCell;
  fOrgOnMouseWheel := fGrid.OnMouseWheel;

  fGrid.OnMouseDown   := @GridMouseDown;
  fGrid.OnMouseMove   := @GridMouseMove;
  fGrid.OnMouseUp     := @GridMouseUp;
  fGrid.OnDrawCell    := @GridDrawCell;
  fGrid.OnSelectCell  := @GridSelectCell;
  fGrid.OnMouseWheel  := @GridMouseWheel;

  fSlider := TSlider.Create(fGrid, aColTag);
end;

end.

