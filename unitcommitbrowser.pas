unit unitcommitbrowser;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, strutils, unitgittypes, unitgitutils, unitifaces, unitgitmgr,
  unitvfs;

const
  COMMITBROWSER_EVENT_RELOAD    =  20;

type

  TCommitBrowserMode = (cbmPatch, cbmTree);

  PInfoNode = ^TInfoNode;
  TInfoNode = record
    //Name: string;
    line: Integer;
    filemode: string[7];
    filetype: string[10];
    fileTree: string[41];
  end;

  { TCommitBrowser }

  TCommitBrowser = class
  private
    fCommit: String;
    fConfig: IConfig;
    fFileDiff: TStringList;
    fGit: IGit;
    fGitMgr: TGitMgr;
    fMode: TCommitBrowserMode;
    fObserverMgr: TObserverMgr;
    fVfs: TVirtualFileSystem;
    procedure OnDisposeVfsNode(Sender: TObject; aName: TvfsString; Data: pointer);
    procedure SetGitMgr(AValue: TGitMgr);
    procedure SetMode(AValue: TCommitBrowserMode);
    procedure ApplyMode;
    procedure Clear;
    procedure ScanFilenames;
    procedure ScanTree(treestr: RawByteString);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(commit: string);

    property ObserverMgr: TObserverMgr read fObserverMgr;
    property Diff: TStringList read fFileDiff;
    property Vfs: TVirtualFileSystem read fVfs;

    property Mode: TCommitBrowserMode read fMode write SetMode;
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property Config: IConfig read fConfig write fConfig;
  end;

implementation

{ TCommitBrowser }

procedure TCommitBrowser.SetGitMgr(AValue: TGitMgr);
begin
  if fGitMgr = AValue then Exit;
  fGitMgr := AValue;
  fGit := fGitMgr.Git;
end;

procedure TCommitBrowser.OnDisposeVfsNode(Sender: TObject; aName: TvfsString;
  Data: pointer);
var
  info: PInfoNode;
begin
  info := Data;
  if info<>nil then begin
    Finalize(info^);
    dispose(info);
  end;
end;

procedure TCommitBrowser.SetMode(AValue: TCommitBrowserMode);
begin
  if fMode = AValue then Exit;
  fMode := AValue;
  ApplyMode;
end;

procedure TCommitBrowser.ApplyMode;
var
  lines: TStringList;
  treestr: RawByteString;
begin

  Clear;

  if fMode=cbmPatch then begin
    lines := TStringList.Create;
    try

      if fGit.Show(fCommit, lines)>0 then begin
        fFileDiff.Text := fGit.ErrorLog;
        fObserverMgr.NotifyObservers(self, COMMITBROWSER_EVENT_RELOAD, 0);
      end else begin
        // process lines
        fFileDiff.Assign(lines);
        ScanFilenames;
        fObserverMgr.NotifyObservers(self, COMMITBROWSER_EVENT_RELOAD, 1);
      end;

    finally
      lines.Free;
    end;

  end else begin

    if fGit.Any('ls-tree -r --full-tree -z ' + fCommit, treestr)>0 then begin
      fFileDiff.Text := fGit.ErrorLog;
      fObserverMgr.NotifyObservers(self, COMMITBROWSER_EVENT_RELOAD, 0);
    end else begin
      ScanTree(treestr);
      fObserverMgr.NotifyObservers(self, COMMITBROWSER_EVENT_RELOAD, 1);
    end;

  end;
end;

procedure TCommitBrowser.Clear;
begin
  fVfs.Clear;
end;

procedure TCommitBrowser.ScanFilenames;
var
  i, j: Integer;
  s: string;
  info: PInfoNode;

  procedure OnNewNode(sender: TObject; aName:TvfsString; isDir:boolean; var data: Pointer);
  begin
    data := info;
  end;

begin
  if fFileDiff.Count>0 then begin

    fVfs.Clear;
    fVfs.OnNewNodeNested := @OnNewNode;

    // the first node will be the diff header
    New(Info);
    info^.line := 0;
    //info^.Name := 'comments';
    fVfs.AddPath('comments');

    for i:=0 to fFileDiff.Count-1 do begin
      s := fFileDiff[i];
      if pos('diff --git a/', s)=1 then begin
        j := pos(' b/', s);
        if j=0 then continue;

        // found a file, register it
        New(info);
        Info^.line := i;
        fVfs.AddPath(copy(s, j + 3, Length(s)));
      end;
    end;

    fVfs.OnNewNodeNested := nil;

  end;
end;

procedure TCommitBrowser.ScanTree(treestr: RawByteString);
var
  info: PInfoNode;

  procedure OnNewNode(sender: TObject; aName:TvfsString; isDir:boolean; var data: Pointer);
  begin
    if not isDir then
      data := info;
  end;

var
  p, q, r: pchar;
  len: Integer;
  aPath: string;
begin
  if treestr='' then
    exit;

  fVfs.Clear;
  fVfs.OnNewNodeNested := @OnNewNode;

  p := @treestr[1];
  while p^<>#0 do begin
    r := p;
    new(info);
    q := strpos(r, ' '); SetString(info^.filemode, r, q-r); r := q + 1;
    q := strpos(r, ' '); SetString(info^.filetype, r, q-r); r := q + 1;
    q := strpos(r, #9 ); SetString(info^.filetree, r, q-r); r := q + 1;

    SetString(aPath, r, strlen(r));

    fVfs.AddPath(aPath);

    len := strlen(p);
    p := p + len + 1;
  end;

  fVfs.OnNewNodeNested := nil;
end;

constructor TCommitBrowser.Create;
begin
  inherited Create;
  fObserverMgr := TObserverMgr.Create;
  fFileDiff := TStringList.Create;
  fVfs := TVirtualFileSystem.Create;
  fVfs.OnDisposeNode := @OnDisposeVfsNode;
end;

destructor TCommitBrowser.Destroy;
begin
  Clear;
  fVfs.Free;
  fFileDiff.Free;
  fObserverMgr.Free;
  inherited Destroy;
end;

procedure TCommitBrowser.Load(commit: string);
begin
  fCommit := commit;
  ApplyMode;
end;

end.

