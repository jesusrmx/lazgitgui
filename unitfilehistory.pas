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

  Shows the file history and changes
}
unit unitfilehistory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, LCLType,
  Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls, SynEdit,
  unitcommon, unitgittypes, unitconfig, unitifaces, unitgitutils, unitgitmgr,
  unithighlighterhelper, unittextchunks, unitlinkmgr, unitsyneditextras;

type

  THistoryFlags = set of (hfChange, hfCreate, hfRename, hfDelete, hfCopy);
  THistoryItem = record
    Date,
    CommitOID,
    Author,
    Subject, Filename, Refs: RawByteString;
    Flags: THistoryFlags;
  end;

  { TfrmFileHistory }

  TfrmFileHistory = class(TForm, IObserver)
    grid: TDrawGrid;
    Splitter1: TSplitter;
    txtDiff: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure gridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure gridHeaderSized(Sender: TObject; IsColumn: Boolean; Index: Integer
      );
    procedure gridSelection(Sender: TObject; aCol, aRow: Integer);
  private
    fFilePath: string;
    fGit: IGit;
    fGitMgr: TGitMgr;
    fhlHelper: THighlighterHelper;
    fHistory: array of THistoryItem;
    fLastRow: Integer;
    fLinkMgr: TLinkMgr;
    procedure OnGetLogItemData(sender: TObject; aIndex: Integer; out aCommit,
      aSubject: RawByteString);
    procedure OnLinkClick(sender: TObject; link: TTextChunksItem);
    procedure ProcessFilePath;
    procedure SetFilePath(AValue: string);
    procedure SetGitMgr(AValue: TGitMgr);
    procedure ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
    procedure SetHlHelper(AValue: THighlighterHelper);
    procedure DoRowSelection(aRow: Integer);
    procedure SearchLog(txt: string; forward: boolean; startRow:Integer=-1; searchIn:TSetOfByte=[]);
  public
    property FilePath: string read fFilePath write SetFilePath;
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property HlHelper: THighlighterHelper read fhlHelper write SetHlHelper;
  end;

var
  frmFileHistory: TfrmFileHistory;

implementation

{$R *.lfm}

const
  COLTAG_DATE                 = 0;
  COLTAG_AUTHOR               = 1;
  COLTAG_SUBJECT              = 2;
  COLTAG_SHA1                 = 3;

{ TfrmFileHistory }

procedure TfrmFileHistory.FormCloseQuery(Sender: TObject; var CanClose: Boolean
  );
begin
  fConfig.WriteWindow(Self, 'frmFileHistory', SECTION_GEOMETRY);
  fConfig.WriteInteger('frmFileHistory.grid.height', grid.Height, SECTION_GEOMETRY);
end;

procedure TfrmFileHistory.FormCreate(Sender: TObject);
begin
  AddPopupMenu(txtDiff);
end;

procedure TfrmFileHistory.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfrmFileHistory.FormDestroy(Sender: TObject);
begin
  GitMgr := nil;
  fLinkMgr.Free;
end;

procedure TfrmFileHistory.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_ESCAPE then
    close;
end;

procedure TfrmFileHistory.FormShow(Sender: TObject);
var
  col: TCollectionItem;
  gcol: TGridColumn;
  i: Integer;
begin
  for i:=0 to grid.Columns.Count-1 do
    grid.Columns[i].Tag := i;

  fConfig.OpenConfig;
  fConfig.ReadWindow(Self, 'frmFileHistory', SECTION_GEOMETRY);
  fConfig.ReadInteger('frmFileHistory.grid.height', grid.Height, SECTION_GEOMETRY);
  for col in grid.Columns do begin
    gcol := TGridColumn(col);
    if gcol.SizePriority=0 then
      gcol.Width := fConfig.ReadInteger('frmFileHistory.grid.coltag'+IntToStr(gcol.tag)+'.width', gcol.width, SECTION_GEOMETRY);
  end;
  fConfig.CloseConfig;

  fLinkMgr := TLinkMgr.Create(grid, COLTAG_SUBJECT);
  fLinkMgr.Git := fGit;
  fLinkMgr.OnLinkClick := @OnLinkClick;
  fLinkMgr.OnGetLogItemData := @OnGetLogItemData;
end;

procedure TfrmFileHistory.gridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  s: RawByteString;
  aIndex, x: Integer;
  Chunks: TTextChunks;
  chunk: TTextChunksItem;
  aStyle: TFontStyles;
begin
  if (fGitMgr=nil) or (aRow<grid.FixedRows) then
    exit;
  aIndex := aRow - grid.FixedRows;
  x := aRect.Left + 7;
  case grid.Columns[aCol].tag of
    COLTAG_DATE:    s := fHistory[aIndex].Date;
    COLTAG_AUTHOR:  s := fHistory[aIndex].Author;
    COLTAG_SUBJECT:
      begin
        Chunks := GetTextChunks(grid.Canvas, aRect, x, fGit.RefsMap, fHistory[aIndex].CommitOID, fHistory[aIndex].Subject);
        for chunk in Chunks do begin
          grid.Canvas.Brush.Style := chunk.brushStyle;
          grid.Canvas.Brush.Color := chunk.brushColor;
          grid.Canvas.Pen.Style := chunk.penStyle;
          grid.Canvas.Pen.Color := chunk.penColor;
          grid.Canvas.Pen.Width := chunk.penWidth;
          if chunk.itemType=tcitBox then grid.Canvas.Rectangle(chunk.r);
          grid.Canvas.Brush.Style := bsClear;
          grid.Canvas.Font.Color := chunk.fontColor;
          aStyle := grid.Canvas.Font.Style;
          if chunk.itemType=tcitLink then Include(aStyle, fsUnderline)
          else                            Exclude(aStyle, fsUnderline);
          grid.Canvas.Font.Style := aStyle;
          grid.Canvas.TextOut(chunk.r.Left + 3, chunk.r.Top, chunk.text);
        end;
        grid.Canvas.Brush.Style := bsSolid;
        grid.Canvas.Pen.Style := psSolid;
        exit;
      end;
    COLTAG_SHA1:    s := fHistory[aIndex].CommitOID;
  end;
  grid.Canvas.Brush.Style := bsClear;
  grid.Canvas.TextOut(x, aRect.Top, s);
  grid.Canvas.Brush.Style := bsSolid;
end;

procedure TfrmFileHistory.gridHeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
  col: TGridColumn;
begin
  if isColumn then begin
    col := grid.Columns[Index];
    fConfig.WriteInteger('frmFileHistory.grid.coltag'+IntToStr(col.Tag)+'.width', col.Width, SECTION_GEOMETRY);
  end;
end;

procedure TfrmFileHistory.gridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  DoRowSelection(aRow);
end;

procedure TfrmFileHistory.SetGitMgr(AValue: TGitMgr);
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

procedure TfrmFileHistory.SetFilePath(AValue: string);
begin
  if fFilePath = AValue then Exit;
  fFilePath := AValue;

  ProcessFilePath;
end;

procedure TfrmFileHistory.ProcessFilePath;
var
  i, j, k: Integer;
  L: TStringList;
  item: THistoryItem;
  o, s: RawByteString;
  p, q, r, m, n, t: pchar;

  function NextEOL: boolean;
  begin
    n := strpos(p, #10);
    result := n<>nil;
    if result then
      n^ := #0;
  end;

  function GetString(start:pchar; len:Integer): string;
  begin
    SetString(Result, start, len);
  end;

  function AdvanceLine(findEOL:boolean=true): boolean;
  begin
    p := n + 1;
    while (p<t) and (p^ in [#10, #13]) do
      inc(p);
    if findEOL then
      result := NextEOL
    else
      result := true;
  end;

begin
  Caption := format(rsHistoryOfS, [fFilePath]);
  L := TStringList.Create;
  try
    // parse this: .....
    if fGit.Any('log --follow --stat --summary -z -- ' + fFilePath, o)>0 then
      exit; // error

    p := @o[1];
    t := p + Length(o);
    while p < t do begin

      // prepare location of next record
      r := p + strlen(p) + 1;

      // locate EOL
      if not NextEOL then break;

      // Find start of record
      if strlcomp(p, 'commit ', 7)<>0 then
        break;

      // found, save commit oid
      SetString(item.CommitOID, p+7, 40);
      inc(p, 47);
      // save refs if there is any
      item.Refs := Trim(GetString(p, n - p));
      if not AdvanceLine then break;

      // skip 'Author: '
      q := strpos(p + 8, ' ');
      if q=nil then break;
      item.Author := GetString(p + 8, q - (p + 8));
      if not AdvanceLine then break;

      // skip 'Date: ';
      item.Date := Trim(GetString(p + 6, (n - (p + 6))));
      if not AdvanceLine then break;

      // skip empty line
      while (p<t) and (p^ in [#10, #13]) do inc(p);

      // collect subject lines
      item.Flags := [];
      item.Subject := '';
      while (p<t) and (StrLComp(p, '    ', 4)=0) do begin
        item.Subject += Trim(GetString(p, n-p));
        AdvanceLine;
        if p+1=n then break;
      end;

      q := strpos(p, '|');
      if q=nil then break;

      (q-1)^ := #0;
      q := strpos(p, '=>');
      if q<>nil then
        p := q + 2;
      item.Filename := Trim(GetString(p, strlen(p)));
      if not AdvanceLine then break;

      // skip line 'x file changed, y insertions, z deletions
      if not AdvanceLine(false) then break;

      if strlcomp(p, ' rename', 7)=0 then Include(item.flags, hfRename) else
      if strlcomp(p, ' delete', 7)=0 then Include(item.flags, hfDelete) else
      if strlcomp(p, ' copy', 5)=0   then Include(item.flags, hfCopy) else
      if strlcomp(p, ' create', 7)=0 then Include(item.flags, hfCreate)
      else                                Include(item.Flags, hfChange);

      j := Length(fHistory);
      SetLength(fHistory, j+1);
      fHistory[j] := item;

      p := r;
    end;

    grid.RowCount := grid.FixedRows + Length(fHistory);

    fLastRow := -1;
    DoRowSelection(grid.FixedRows);

  finally
    L.Free;
  end;
end;

procedure TfrmFileHistory.OnLinkClick(sender: TObject; link: TTextChunksItem);
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

procedure TfrmFileHistory.OnGetLogItemData(sender: TObject; aIndex: Integer;
  out aCommit, aSubject: RawByteString);
begin
  aCommit := fHistory[aIndex].CommitOID;
  aSubject := fHistory[aIndex].Subject;
end;

procedure TfrmFileHistory.ObservedChanged(Sender: TObject; what: Integer;
  data: PtrInt);
begin
  case what of
    GITMGR_EVENT_REFLISTCHANGED:
      begin
        grid.Invalidate;
      end;
  end;

end;

procedure TfrmFileHistory.SetHlHelper(AValue: THighlighterHelper);
begin
  if fhlHelper = AValue then Exit;
  fhlHelper := AValue;
  if fhlHelper<>nil then
    fhlHelper.SetHighlighter(txtDiff, 'x.diff');
end;

procedure TfrmFileHistory.DoRowSelection(aRow: Integer);
var
  aIndex: Integer;
  L: TStringList;
  s: string;
begin
  if (fLastRow<>aRow) and (Length(fHistory)>0) then begin
    aIndex := aRow - grid.FixedRows;
    L := TStringList.Create;
    try
      fGit.Show(fHistory[aIndex].CommitOID + ' -- ' + fHistory[aIndex].Filename, L);
      while (L.Count>0) do begin
        s := L[0];
        L.Delete(0);
        if pos('diff --git', s)=1 then
          break;
      end;
      txtDiff.Lines.Assign(L);
    finally
      L.Free;
    end;
    fLastRow := aRow;
  end;
end;

procedure TfrmFileHistory.SearchLog(txt: string; forward: boolean;
  startRow: Integer; searchIn: TSetOfByte);
var
  L: TStringList;
  i, delta, aRow, aIndex, anyRow, count: Integer;
  found: boolean;
begin

  if SearchIn=[] then
    SearchIn := [SEARCHIN_COMMIT, SEARCHIN_AUTHOR, SEARCHIN_SUBJECT];

  L := TStringList.Create;
  try
    L.DelimitedText := lowercase(txt);
    if forward then delta := 1 else delta := -1;
    if startRow<0 then aRow := grid.Row
    else               aRow := startRow;

    anyRow := -1;
    aRow := aRow + delta;
    while (aRow >= grid.FixedRows) and (aRow <= grid.RowCount-1) do begin
      aIndex := aRow - grid.FixedRows;

      count := 0;
      for i:=0 to L.Count-1 do begin

        found := (SEARCHIN_COMMIT in SearchIn) and (pos(L[i], fHistory[aIndex].CommitOID)>0);
        if not found then
          found := (SEARCHIN_AUTHOR in SearchIn) and (pos(L[i], lowercase(fHistory[aIndex].author))>0);
        if not found then
          found := (SEARCHIN_SUBJECT in SearchIn) and (pos(L[i], lowercase(fHistory[aIndex].Subject))>0);

        if found then begin
          if (anyRow<0) then
            anyRow := aIndex;
          inc(count);
        end;

      end;

      if count=L.Count then begin
        // found all terms
        grid.Row := aRow;
        //btnNext.Enabled := true;
        //btnPrev.Enabled := true;
        exit;
      end;

      aRow := aRow + delta;
    end;

    // not found, but check anyRow
    if anyRow>=grid.FixedRows then begin
      // partial find
      grid.Row := aRow;
      //btnNext.Enabled := true;
      //btnPrev.Enabled := true;
    end;

    ShowMessageFmt('Could''t find "%s"',[txt]);

  finally
    L.Free;
  end;
end;

end.

