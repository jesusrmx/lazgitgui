unit unitfilehistory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  SynEdit,
  unitconfig, unitifaces, unitgitmgr, unithighlighterhelper;

type

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
  private
    fFilePath: string;
    fGit: IGit;
    fGitMgr: TGitMgr;
    fhlHelper: THighlighterHelper;
    procedure LoadFilePath(Data: PtrInt);
    procedure SetGitMgr(AValue: TGitMgr);
    procedure ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
  public
    property FilePath: string read fFilePath write fFilePath;
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property HlHelper: THighlighterHelper read fhlHelper write fhlHelper;
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
begin
  fConfig.ReadWindow(Self, 'frmFileHistory', SECTION_GEOMETRY);
  fConfig.ReadInteger('frmFileHistory.grid.height', grid.Height, SECTION_GEOMETRY);
  Application.QueueAsyncCall(@LoadFilePath, 0);
end;

procedure TfrmFileHistory.gridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if (fGitMgr=nil) or (aRow<grid.FixedRows) then
    exit;
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

procedure TfrmFileHistory.LoadFilePath(Data: PtrInt);
begin
  Caption := 'History of ' + fFilePath;
  fhlHelper.SetHighlighter(txtDiff, 'x.diff');
  fGit.Log('--follow -p -- ' + fFilePath, txtDiff.Lines);
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

end.

