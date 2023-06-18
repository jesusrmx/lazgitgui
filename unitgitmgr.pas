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

  Git manager.
  It creates the git instance and provides the git interface to several parts
  that need it. Starts operations and distribute results.
}
unit unitgitmgr;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLogger,
  unitgittypes, unitgit, unitifaces, unitruncmd, unitentries;

const
  GITMGR_EVENT_UPDATESTATUS         = 1;
  GITMGR_EVENT_REFLISTCHANGED       = 2;
  GITMGR_EVENT_NEWTAG               = 3;
  GITMGR_EVENT_SWITCHTOTAG          = 4;
  GITMGR_EVENT_NEWBRANCH            = 5;

type

  { TObserverMgr }

  TObserverMgr = class
  private
    fObservers: array of IObserver;
    function IndexOf(who: IObserver): Integer;
  public
    procedure AddObserver(who: IObserver);
    procedure RemoveObserver(who: IObserver);
    procedure NotifyObservers(sender: TObject; what: Integer; data: PtrInt);
  end;

  { TGitMgr }

  TGitMgr = class
  private
    fBranch: string;
    fBranchOID: string;
    fCommitsAhead: Integer;
    fCommitsBehind: Integer;
    fEntries: TFPList;
    fLastTag: string;
    fLastTagOID: string;
    fLastTagCommits: Integer;
    fMerging, fMergingConflict: boolean;
    fConfig: IConfig;
    fGit: TGit;
    fShowTags: boolean;
    fUpstream: string;
    fViewIgnoredFiles: boolean;
    fViewTrackedFiles: boolean;
    fViewUntrackedFiles: boolean;
    fDescribed: boolean;
    fLastDescribedTag: string;
    fUnstagedList, fStagedList: TStringList;
    fObserverMgr: TObserverMgr;
    fRemotes: TRemotesArray;
    function GetGit: IGit;
    function GetRemotesList: string;
    procedure OnCommandsDone(Sender: TObject);
    procedure SetConfig(AValue: IConfig);
    procedure OnCommandProgress(sender: TObject; item: TCommandItem; percent: single);
    function  GetRemoteIndex: Integer;
  public
    constructor create;
    destructor destroy; override;
    function Initialize: boolean;
    procedure UpdateStatus;
    procedure UpdateRefList;
    procedure AddObserver(who: IObserver);
    procedure RemoveObserver(who: IObserver);
    function IndexOfLocalBranch(aName: string): Integer;
    procedure QueueNewTag(commit: string);
    procedure QueueSwitchTag(tagName: string);
    procedure QueueNewBranch(sender: TObject; branchName, command: string; switch, fetch:boolean);
    procedure ForceTagDescription;
    procedure UpdateRemotes;

    property CommitsAhead: Integer read fCommitsAhead;
    property CommitsBehind: Integer read fCommitsBehind;
    property Branch: string read fBranch;
    property BranchOID: string read fBranchOID;
    property Upstream: string read fUpstream;
    property LastTag: string read fLastTag;
    property LastTagCommits: Integer read fLastTagCommits;
    property LastTagOID: string read fLastTagOID;
    property Merging: boolean read fMerging;
    property MergingConflict: boolean read fMergingConflict;

    property Git: IGit read GetGit;
    property Config: IConfig read fConfig write SetConfig;
    property ShowTags: boolean read fShowTags write fShowTags;
    property ViewIgnoredFiles: boolean read fViewIgnoredFiles write fViewIgnoredFiles;
    property ViewUntrackedFiles: boolean read fViewUntrackedFiles write fViewUntrackedFiles;
    property ViewTrackedFiles: boolean read fViewTrackedFiles write fViewTrackedFiles;
    property UnstagedList: TStringList read fUnstagedList;
    property StagedList: TStringList read fStagedList;
    property Remotes: TRemotesArray read fRemotes;
    property RemotesList: string read GetRemotesList;
    property RemoteIndex: Integer read GetRemoteIndex;
  end;

implementation

{ TObserverMgr }

function TObserverMgr.IndexOf(who: IObserver): Integer;
var
  i: Integer;
begin
  for i:=0 to Length(fObservers)-1 do
    if fObservers[i]=who then
      exit(i);
  result := -1;
end;

procedure TObserverMgr.AddObserver(who: IObserver);
var
  i: SizeInt;
begin
  i := IndexOf(who);
  if i<0 then begin
    i := Length(fObservers);
    SetLength(fObservers, i+1);
    fObservers[i] := who
  end;
end;

procedure TObserverMgr.RemoveObserver(who: IObserver);
var
  i: Integer;
begin
  i := IndexOf(who);
  if i>=0 then
    Delete(fObservers, i, 1);
end;

procedure TObserverMgr.NotifyObservers(sender: TObject; what: Integer;
  data: PtrInt);
var
  who: IObserver;
begin
  for who in fObservers do
    who.ObservedChanged(sender, what, data);
end;

{ TGitMgr }

procedure TGitMgr.SetConfig(AValue: IConfig);
begin
  if fConfig = AValue then Exit;
  fConfig := AValue;
  fGit.Config := fConfig;
end;

function TGitMgr.GetGit: IGit;
begin
  result := fGit;
end;

function TGitMgr.GetRemotesList: string;
var
  r: TRemoteInfo;
begin
  result := '';
  for r in fRemotes do begin
    if result<>'' then result += ',';
    result += r.name;
  end;
end;

constructor TGitMgr.create;
begin
  inherited Create;
  fGit := TGit.Create;
  fUnstagedList := TStringList.Create;
  fStagedList := TStringList.Create;
  fObserverMgr := TObserverMgr.Create;
  fEntries := TFpList.Create;
end;

destructor TGitMgr.destroy;
begin
  ClearEntries(fEntries);
  fEntries.Free;
  fObserverMgr.Free;
  fStagedList.Free;
  fUnstagedList.Free;
  fGit.Free;
  inherited destroy;
end;

function TGitMgr.Initialize: boolean;
begin
  result := fGit.Initialize;
end;

procedure TGitMgr.OnCommandProgress(sender: TObject; item: TCommandItem; percent: single);
var
  thread: TRunThread absolute sender;
  cmd: string;
  p: Integer;
  head, tail: pchar;
  M: TMemoryStream;
begin
  DebugLn('%3f%%: %s',[percent, item.description]);
  case item.description of
    'describe':
      begin
        fLastTag := '';
        fLastTagCommits := 0;
        fLastTagOID := '';
        if thread.Result<=0 then begin
          // tag-commits-'g'OID
          cmd := thread.CurrentOutput;
          p := cmd.LastIndexOf('-g');
          if p>=0 then begin
            fLastTagOID := Trim(copy(cmd, p+3, MAXINT));
            delete(cmd, p+1, MAXINT);
            p := cmd.LastIndexOf('-');
            if p>=0 then begin
              fLastTagCommits := StrToIntDef(copy(cmd, p+2, MAXINT), 0);
              fLastTag := copy(cmd, 1, p);
            end;
          end;
        end;
      end;

    'merging':
      begin
        fMerging := thread.Result=0;
      end;

    'status':
      begin
        M := TMemoryStream(item.tag);
        head := M.Memory;
        tail := head + M.Size;
        //M.SaveToFile('laststatus.txt');

        ParseBranches(head, tail, fBranch, fBranchOID, fUpstream, fCommitsAhead, fCommitsBehind);
        ParseStatus(head, tail, fUnstagedList, fStagedList, fEntries, fMergingConflict);

        fObserverMgr.NotifyObservers(self, GITMGR_EVENT_UPDATESTATUS, 0);
      end;
  end;
end;

function TGitMgr.GetRemoteIndex: Integer;
var
  arr: TStringArray;
  i: Integer;
begin
  if (fUpstream<>'') and (fRemotes<>nil) then begin
    arr := fUpstream.Split('/');
    if arr<>nil then
      for i:=0 to Length(fRemotes)-1 do
        if fRemotes[i].name=arr[0] then begin
          result := i;
          exit;
        end;
  end;
  result := -1;
end;


procedure TGitMgr.OnCommandsDone(Sender: TObject);
var
  thread: TRunThread absolute sender;
begin

end;

procedure TGitMgr.UpdateStatus;
var
  i: Integer;
  commands: TCommandsArray;
begin

  commands := nil;

  // get the more recent tag
  if fShowTags and (not fDescribed) then begin
    i := Length(commands);
    SetLength(commands, i+1);
    commands[i].description := 'describe';
    commands[i].command := fGit.Exe + ' describe --tags';
    commands[i].RedirStdErr := false;
    commands[i].PreferredOutputType := cipotString;
  end;

  //result := FileExists(fTopLevelDir + '.git/MERGE_HEAD');
  // ref: https://stackoverflow.com/a/55192451
  //cmdLine.StdOutputClosed := true;
  i := Length(commands);
  SetLength(commands, i+1);
  commands[i].description := 'merging';
  commands[i].command := fGit.Exe + ' rev-list -1 MERGE_HEAD';
  commands[i].RedirStdErr := true;
  commands[i].PreferredOutputType := cipotString;

  i := Length(commands);
  SetLength(commands, i+1);
  commands[i].description := 'status';
  commands[i].command := fGit.Exe +
    format(' status -b --long --porcelain=2 --ahead-behind --ignored=%s --untracked-files=%s -z',
      [BoolToStr(fViewIgnoredFiles, 'traditional', 'no'),
       BoolToStr(fViewUntrackedFiles, 'all', 'no')]);
  commands[i].RedirStdErr := false;
  commands[i].PreferredOutputType := cipotStream;

  RunInThread(commands, fGit.TopLevelDir, @OnCommandProgress, @OnCommandsDone);

  //fObserverMgr.NotifyObservers(self, GITMGR_EVENT_UPDATESTATUS, res);
end;

procedure TGitMgr.UpdateRefList;
var
  res: Integer;
begin
  res := fGit.UpdateRefList;
  fObserverMgr.NotifyObservers(Self, GITMGR_EVENT_REFLISTCHANGED, res);
end;

procedure TGitMgr.AddObserver(who: IObserver);
begin
  fObserverMgr.AddObserver(who);
end;

procedure TGitMgr.RemoveObserver(who: IObserver);
begin
  fObserverMgr.RemoveObserver(who);
end;

function TGitMgr.IndexOfLocalBranch(aName: string): Integer;
var
  i: Integer;
  info: PRefInfo;
begin
  result := -1;
  aName := lowercase(aName);
  for i:=0 to fGit.RefList.Count-1 do begin
    info := PRefInfo(fGit.RefList.Objects[i]);
    if (info^.subType=rostLocal) and (aName=info^.refName) then begin
      result := i;
      break;
    end;
  end;
end;

procedure TGitMgr.QueueNewTag(commit: string);
var
  info: PTagInfo;
begin
  new(info);
  info^.data := commit;
  fObserverMgr.NotifyObservers(Self, GITMGR_EVENT_NEWTAG, PtrInt(info));
end;

procedure TGitMgr.QueueSwitchTag(tagName: string);
var
  info: PTagInfo;
begin
  new(info);
  info^.data := tagName;
  fObserverMgr.NotifyObservers(Self, GITMGR_EVENT_SWITCHTOTAG, PtrInt(info));
end;

procedure TGitMgr.QueueNewBranch(sender: TObject; branchName, command: string;
  switch, fetch: boolean);
var
  info: PBranchInfo;
begin
  new(info);
  info^.sender := sender;
  info^.name := branchName;
  info^.command := command;
  info^.switch := switch;
  info^.fetch := fetch;
  fObserverMgr.NotifyObservers(self, GITMGR_EVENT_NEWBRANCH, PtrInt(info));
end;

procedure TGitMgr.ForceTagDescription;
begin
  fDescribed := false;
end;

procedure TGitMgr.UpdateRemotes;
var
  cmdOut: RawByteString;
  L: TStringList;
  i, j: Integer;
  arr: TStringArray;
begin
  if fGit.Any('remote -v show', cmdOut)<=0 then begin
    fRemotes := nil;
    L := TStringList.Create;
    try
      L.Text := cmdOut;
      if L.Count div 2 > 0 then begin
        i := 0;
        while i<L.Count do begin

          j := Length(fRemotes);
          SetLength(fRemotes, j+1);

          arr := L[i].Split([' ', #9]);
          fRemotes[j].name := arr[0];
          fRemotes[j].fetch := arr[1];
          inc(i);

          arr := L[i].Split([' ', #9]);
          fRemotes[j].push := arr[1];
          inc(i);
        end;
      end;
    finally
      L.Free;
    end;
  end;
end;

end.

