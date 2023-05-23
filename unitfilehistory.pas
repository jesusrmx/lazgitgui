unit unitfilehistory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, StrUtils, Forms, Controls, Graphics, Dialogs, Grids,
  ExtCtrls, SynEdit,
  unitconfig, unitifaces, unitgitmgr, unithighlighterhelper;

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
    procedure FormDestroy(Sender: TObject);
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
    procedure ProcessFilePath;
    procedure SetFilePath(AValue: string);
    procedure SetGitMgr(AValue: TGitMgr);
    procedure ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
    procedure SetHlHelper(AValue: THighlighterHelper);
    procedure DoRowSelection(aRow: Integer);
  public
    property FilePath: string read fFilePath write SetFilePath;
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property HlHelper: THighlighterHelper read fhlHelper write SetHlHelper;
  end;

var
  frmFileHistory: TfrmFileHistory;

implementation

{$R *.lfm}

{ TfrmFileHistory }

procedure TfrmFileHistory.FormCloseQuery(Sender: TObject; var CanClose: Boolean
  );
begin
  fConfig.WriteWindow(Self, 'frmFileHistory', SECTION_GEOMETRY);
  fConfig.WriteInteger('frmFileHistory.grid.height', grid.Height, SECTION_GEOMETRY);
end;

procedure TfrmFileHistory.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfrmFileHistory.FormDestroy(Sender: TObject);
begin
  GitMgr := nil;
end;

procedure TfrmFileHistory.FormShow(Sender: TObject);
var
  col: TCollectionItem;
  gcol: TGridColumn;
begin
  fConfig.OpenConfig;
  fConfig.ReadWindow(Self, 'frmFileHistory', SECTION_GEOMETRY);
  fConfig.ReadInteger('frmFileHistory.grid.height', grid.Height, SECTION_GEOMETRY);
  for col in grid.Columns do begin
    gcol := TGridColumn(col);
    if gcol.SizePriority=0 then
      gcol.Width := fConfig.ReadInteger('frmFileHistory.grid.'+gcol.Title.caption+'.width', gcol.width, SECTION_GEOMETRY);
  end;
  fConfig.CloseConfig;
end;

procedure TfrmFileHistory.gridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  s: RawByteString;
  aIndex, x: Integer;
begin
  if (fGitMgr=nil) or (aRow<grid.FixedRows) then
    exit;
  aIndex := aRow - grid.FixedRows;
  x := aRect.Left + 7;
  case grid.Columns[aCol].Title.Caption of
    'Date':     s := fHistory[aIndex].Date;
    'Author':   s := fHistory[aIndex].Author;
    'Subject':  s := fHistory[aIndex].Subject;
    'Commit':   s := fHistory[aIndex].CommitOID;
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
    fConfig.WriteInteger('frmFileHistory.grid.'+col.Title.caption+'.width', col.Width, SECTION_GEOMETRY);
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
  Caption := 'History of ' + fFilePath;
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
  if fLastRow<>aRow then begin
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

end.

