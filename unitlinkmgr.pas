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

  Link Manager.
  Given a drawgrid and a reference colum, this class gets the text in the
  pointed column, it finds and reacts to links in such text.
}
unit unitlinkmgr;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Grids,
  unitifaces, unittextchunks, unitlogcache, unitdbindex, unitgitmgr;

const
  LOGCELL_LEFTMARGIN  =   CHUNKWIDTH_SPACING + CHUNKWIDTH_SEPARATOR;


type

  TLinkClickEvent = procedure(sender:TObject; link: TTextChunksItem) of object;
  TGetLogItemDataEvent = procedure(sender:TObject; aIndex:Integer; out aCommit, aSubject:RawByteString) of object;

  { TLinkMgr }

  TLinkMgr = class
  private
    fClickingLink: Boolean;
    fGitMgr: TGitMgr;
    fGrid: TDrawGrid;
    fLogCache: TLogCache;
    fOnGetLogItemData: TGetLogItemDataEvent;
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
    procedure GetLogItemData(aIndex: Integer; out aCommit, aSubject: RawByteString);
  public
    constructor create(aGrid: TDrawGrid; aColTag: Integer);
    destructor destroy; override;

    property LogCache: TLogCache read fLogCache write fLogCache;
    property GitMgr: TGitMgr read fGitMgr write fGitMgr;
    property OnLinkClick: TLinkClickEvent read fOnLinkClick write fOnLinkClick;
    property OnGetLogItemData: TGetLogItemDataEvent read fOnGetLogItemData write fOnGetLogItemData;
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

destructor TLinkMgr.destroy;
begin
  fRowTextChunks := nil;
  inherited destroy;
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
  aCommit, aSubject: RawByteString;
begin
  fGrid.MouseToCell(x, y, aCol, aRow);
  if aRow<fGrid.FixedRows then
    exit;
  col := fGrid.Columns[aCol];
  if (col<>nil) and (col.Tag=fColTag) then begin
    if aRow<>fLastHoverRow then begin
      aIndex := aRow - fGrid.FixedRows;
      aRect := fGrid.CellRect(aCol, aRow);
      GetLogItemData(aIndex, aCommit, aSubject);
      fRowTextChunks := GetTextChunks(fGrid.Canvas, aRect, aRect.Left + LOGCELL_LEFTMARGIN,
      fGitMgr.RefsMap, aCommit, aSubject);
    end;

    for i:=0 to Length(fRowTextChunks)-1 do
    with fRowTextChunks[i] do begin
      if r.Contains(Point(x, y)) then begin
        if itemType=tcitLink then aCursor := crHandPoint
        else                      aCursor := crDefault;
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

procedure TLinkMgr.GetLogItemData(aIndex: Integer; out aCommit,
  aSubject: RawByteString);
var
  aItem: TLogItem;
begin
  if Assigned(fOnGetLogItemData) then
    fOnGetLogItemData(self, aIndex, aCommit, aSubject)
  else begin
    fLogCache.DbIndex.LoadItem(aIndex, aItem);
    aCommit := aItem.CommitOID;
    aSubject := aItem.Subject;
  end;
end;

end.

