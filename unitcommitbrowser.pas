unit unitcommitbrowser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unitgittypes, unitgitutils, unitifaces, unitgitmgr;

const
  COMMITBROWSER_EVENT_RELOAD    =  20;

type

  TCommitBrowserMode = (cbmPatch, cbmTree);

  TInfoNode = record
    Name: string;
    line: Integer;
  end;

  PFileTreeNode = ^TFileTreeNode;
  TFileTreeNode = record
    Info: TInfoNode;
    Prev, Next, Childs: PFileTreeNode;
  end;

  { TCommitBrowser }

  TCommitBrowser = class
  private
    fCommit: String;
    fConfig: IConfig;
    fFileDiff: TStringList;
    fFileTree: PFileTreeNode;
    fGit: IGit;
    fGitMgr: TGitMgr;
    fMode: TCommitBrowserMode;
    fObserverMgr: TObserverMgr;
    procedure SetGitMgr(AValue: TGitMgr);
    procedure SetMode(AValue: TCommitBrowserMode);
    procedure ApplyMode;
    procedure Clear;
    procedure ScanFilenames;
    function NewNode(prev: PFileTreeNode): PFileTreeNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(commit: string);

    property ObserverMgr: TObserverMgr read fObserverMgr;
    property Tree: PFileTreeNode read fFileTree;
    property Diff: TStringList read fFileDiff;

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

procedure TCommitBrowser.SetMode(AValue: TCommitBrowserMode);
begin
  if fMode = AValue then Exit;
  fMode := AValue;
  ApplyMode;
end;

procedure TCommitBrowser.ApplyMode;
var
  lines: TStringList;
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
  end;
end;

procedure TCommitBrowser.Clear;

  procedure FreeNode(var aNode: PFileTreeNode);
  begin
    Finalize(aNode^.Info);
    Dispose(aNode);
    aNode := nil;
  end;

  procedure DisposeNode(aNode: PFileTreeNode);
  var
    cur: PFileTreeNode;
  begin
    while aNode<>nil do begin
      if aNode^.Childs<>nil then begin
        DisposeNode(aNode^.Childs);
        FreeNode(aNode^.Childs);
      end;
      cur := aNode;
      aNode := aNode^.Next;
      FreeNode(cur);
    end;
  end;

begin
  DisposeNode(fFileTree);
end;

procedure TCommitBrowser.ScanFilenames;
var
  Node, last: PFileTreeNode;
  i, j: Integer;
  s: string;
begin
  if fFileDiff.Count>0 then begin

    // the first node will be the diff header
    Node := NewNode(nil);
    Node^.Info.Name := 'comments';
    fFileTree := Node;

    for i:=0 to fFileDiff.Count-1 do begin
      s := fFileDiff[i];
      if pos('diff --git a/', s)=1 then begin
        j := pos(' b/', s);
        if j=0 then continue;

        // found a file, register it
        last := Node;
        Node := NewNode(Node);
        last^.Next := Node;

        Node^.Info.Name := copy(s, j + 3, Length(s));
        Node^.Info.line := i;
      end;
    end;

  end;
end;

function TCommitBrowser.NewNode(prev: PFileTreeNode): PFileTreeNode;
begin
  new(result);
  result^.Info.Name := '';
  result^.Info.line := 0;
  result^.Next := nil;
  result^.Prev := prev;
  result^.Childs := nil;
end;

constructor TCommitBrowser.Create;
begin
  inherited Create;
  fObserverMgr := TObserverMgr.Create;
  fFileDiff := TStringList.Create;
end;

destructor TCommitBrowser.Destroy;
begin
  Clear;
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

