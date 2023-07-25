unit unitrepovars;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  unitgitmgr;

type

  { TRepoVars }

  TRepoVars = class
  private
    type
      TVarsMap = specialize TFPGMap<string, string>;
  private
    //fVars: array of TVarItem;
    fVars: TVarsMap;
    fGitMgr: TGitMgr;
    procedure Update;
  public
    destructor Destroy; override;
    function ReplaceVars(cmd: string): string;
    property GitMgr: TGitMgr read fGitMgr write fGitMgr;
  end;

implementation

{ TRepoVars }

procedure TRepoVars.Update;
var
  aRemote, aRemoteBranch, aRemoteURL: String;
  i: Integer;
begin
  if fVars=nil then begin
    fVars := TVarsMap.Create;
    fVars.Sorted := true;
  end;

  aRemote := fGitMgr.Upstream;
  i := pos('/', aRemote);
  if i>0 then begin
    aRemoteBranch := copy(aRemote, i+1, MAXINT);
    aRemote := copy(aRemote, 1, i-1);
  end else begin
    aRemoteBranch := '';
  end;

  i := fGitMgr.RemoteIndex;
  if i<0 then begin
    fGitMgr.UpdateRemotes;
    i := fGitMgr.RemoteIndex;
  end;
  if i>=0 then aRemoteURL := fGitMgr.Remotes[i].fetch
  else         aRemoteURL := '';

  fVars.AddOrSetData('branch', fGitMgr.Branch);
  fVars.AddOrSetData('branchoid', fGitMgr.BranchOID);
  fVars.AddOrSetData('upstream', fGitMgr.Upstream);
  fVars.AddOrSetData('remote', aRemote);
  fVars.AddOrSetData('remotebranch', aRemoteBranch);
  fVars.AddOrSetData('remoteurl', aRemoteURL);
  fVars.AddOrSetData('tag', fGitMgr.LastTag);
  fVars.AddOrSetData('tagoid', fGitMgr.LastTagOID);

end;

destructor TRepoVars.Destroy;
begin
  fVars.Free;
  inherited Destroy;
end;

function TRepoVars.ReplaceVars(cmd: string): string;
var
  i: Integer;
begin
  result := cmd;
  Update;
  for i := 0 to fVars.Count-1 do
    result := StringReplace(result, '$'+fVars.Keys[i], fVars.Data[i], [rfReplaceAll, rfIgnoreCase]);
end;

end.

