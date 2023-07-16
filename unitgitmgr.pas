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
  unitgittypes, unitgitutils, unitgit, unitifaces, unitprocess, unitruncmd,
  unitentries;

const
  GITMGR_EVENT_UPDATESTATUS         = 1;
  GITMGR_EVENT_REFLISTCHANGED       = 2;
  GITMGR_EVENT_NEWTAG               = 3;
  GITMGR_EVENT_SWITCHTOTAG          = 4;
  GITMGR_EVENT_SWITCHTOCOMMIT       = 5;
  GITMGR_EVENT_NEWBRANCH            = 6;
  GITMGR_EVENT_LOADREPOSITORY       = 7;

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
    fWithStatusAheadBehihd: Boolean;
    fWithStatusPorcelainV2: Boolean;
    fWithStatusIgnored: Boolean;
    fInternalRefList: TStringList;
    fRefsMap: TRefsMap;
    function GetGit: IGit;
    function GetRemotesList: string;
    procedure OnCommandsDone(Sender: TObject);
    procedure SetConfig(AValue: IConfig);
    procedure OnCommandProgress(sender: TObject; item: TCommandItem; percent: single);
    function  GetRemoteIndex: Integer;
    function RefListEnabledField(aField: string): boolean;
    procedure UpdateRefsMap;
    function GetRefList: TStringList;
    function GetRefsMap: TRefsMap;
  public
    constructor create;
    destructor destroy; override;

    function  AddToIgnoreFile(aFile:string; justType:boolean; global:boolean): boolean;
    function  Initialize: boolean;
    procedure UpdateStatus(ondone: TNotifyEvent = nil);
    procedure UpdateRefList;
    procedure AddObserver(who: IObserver);
    procedure RemoveObserver(who: IObserver);
    function  IndexOfLocalBranch(aName: string): Integer;
    procedure QueueNewTag(commit: string);
    procedure QueueSwitchTag(tagName: string);
    procedure QueueSwitchToCommit(aCommit: string);
    procedure QueueNewBranch(sender: TObject; branchName, command: string; switch, fetch:boolean);
    procedure ForceTagDescription;
    procedure UpdateRemotes;
    function  FillRefList(list: TStrings; pattern:string; fields:array of string): Integer; overload;
    function  RefsFilter(commitOID: string; filter: TRefFilterProc): TRefInfoArray;
    procedure LoadRepository(aDir: string);

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
    property RefList: TStringList read GetRefList;
    property RefsMap: TRefsMap read GetRefsMap;
  end;

implementation

procedure ClearRefList(list: TStrings);
var
  i: Integer;
  info: PRefInfo;
begin
  if List<>nil then begin
    for i:=0 to list.Count-1 do begin
      info := PRefInfo(list.Objects[i]);
      if info^.refered<>nil then begin
        Finalize(info^.refered^);
        Dispose(info^.refered);
      end;
      Finalize(info^);
      Dispose(info);
    end;
    list.clear;
  end;
end;

function CleanRefField(const aField: string; out opt:string): string;
var
  i: SizeInt;
begin
  opt := '';
  result := aField;
  delete(result, 1, 2);
  i := pos(':', result);
  if i<=0 then
    i := pos(')', result)
  else
    opt := copy(result, i+1, Length(result)-i-1);
  if i>0 then
    delete(result, i, Length(result))
end;

function StrToRefObjType(objType: string): TRefObjectType;
begin
  case objType of
    'commit': result := rotCommit;
    'blob': result := rotBlob;
    'tree': result := rotTree;
    'tag': result := rotTag;
  end;
end;

function ParseRefList(M: TMemoryStream; list: TStrings; sep:string; fields:array of string): boolean;
var
  p, q, r, t: pchar;
  info, refered: PRefInfo;
  fieldIndex, n: Integer;
  field, value, opt, item: string;
  haveReferedFields: boolean;
begin

  haveReferedFields := false;
  for field in fields do
    if pos('%(*', field)>0 then begin
      haveReferedFields := true;
      break;
    end;

  result := false;
  p := m.Memory;
  t := p + m.size;

  while p<t do begin

    // find length of single line "record"
    n := strlen(p);
    q := p + n;

    new(info);
    info^.subType := rostOther;
    info^.isTracking := false;

    if haveReferedFields and (strpos(p, pchar('refs/tags/'))<>nil) then new(refered)
    else                                                                refered := nil;
    info^.refered := refered;

    // each returned field should correspond to every fields[] requested
    fieldIndex := 0;
    while (p<q) and (fieldIndex<Length(fields)) do begin

      // get field value, the start of separator is the end of the field
      r := strpos(p, pchar(sep));
      if r=nil then begin
        DebugLn('Error: field separator not found at %d',[t-p]);
        dispose(info);
        exit;
      end else
        r^ := #0;

      value := p;
      if fieldIndex=0 then
        item := value;

      // match requested fields
      field := CleanRefField(fields[fieldIndex], opt);
      case field of
        'refname':
          begin
            case opt of
              '':
                begin
                  if pos('refs/tags', value)=1 then info^.subType := rostTag else
                  if pos('refs/heads', value)=1 then info^.subType := rostLocal else
                  if pos('refs/remotes', value)=1 then info^.subType := rostTracking;
                end;
              'short': info^.refName := value;
              'rstrip=-2':
                case value of
                  'refs/tags': info^.subType := rostTag;
                  'refs/heads': info^.subType := rostLocal;
                  'refs/remotes': info^.subType := rostTracking;
                end;
            end;
          end;
        'objecttype': info^.objType := StrToRefObjType(value);
        'objectname':
          begin
            info^.objName := value;
            info^.objNameInt := OIDToQWord(value);
          end;
        'upstream': info^.upstream := value;
        'push': info^.push := value;
        'HEAD': info^.head := value='*';
        'worktreepath': info^.worktreepath := value;
        'contents': info^.subject := value;
        'subject': info^.subject := value;
        'authorname': info^.authorName := value;
        'authordate': info^.authorDate := GitDateToDateTime(value);
        'committerdate': info^.committerDate := GitDateToDateTime(value);
        'creatordate': info^.creatorDate := GitDateToDateTime(value);
        '*objecttype': if refered<>nil then refered^.objType := StrToRefObjType(value);
        '*objectname': if refered<>nil then refered^.objName := value;
        '*authorname': if refered<>nil then refered^.authorName := value;
        '*authordate': if refered<>nil then refered^.authorDate := GitDateToDateTime(value);
        '*contents': if refered<>nil then refered^.subject := value;
        '*subject': if refered<>nil then refered^.subject := value;
      end;

      inc(fieldIndex);
      p := r + Length(sep);
    end;

    p := q + 1;
    // skip the eol that for-each-ref always add
    while (p<t) and (p^ in [#10, #13]) do inc(p);

    if info^.subType=rostOther then
      case info^.objType of
        rotCommit:
          if info^.isTracking then  info^.subType := rostTracking
          else                      info^.subType := rostLocal;
        rotTag:                     info^.subType := rostTag;
      end;

    list.addObject(item, TObject(info));
  end;

  result := true;
end;

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
  if fRefsMap<>nil then
    fRefsMap.Clear;
  fRefsMap.Free;
  ClearRefList(fInternalRefList);
  fInternalRefList.Free;
  fGit.Free;
  inherited destroy;
end;

function TGitMgr.AddToIgnoreFile(aFile: string; justType: boolean;
  global: boolean): boolean;
var
  l: TStringList;
  aPath, gitIgnoreFile: string;
begin
  result := false;

  l := TStringList.Create;
  try
    aPath := fGit.TopLevelDir;
    if not global then
      aPath += ExtractFilePath(aFile);
    aPath += '.gitignore';

    if FileExists(aPath) then
      l.LoadFromFile(aPath);

    if justType then begin
      aFile := ExtractFileExt(aFile);
      if aFile='' then
        exit; // refuse to add '*.' to ignore list
      aFile := '*' + aFile;
    end else
      aFile := ExtractFileName(aFile);

    result := l.IndexOf(aFile)<0;
    if result then begin
      l.Add(aFile);
      l.SaveToFile(aPath);
    end;

  finally
    l.Free;
  end;
end;

function TGitMgr.Initialize: boolean;
begin
  result := fGit.Initialize;
  fWithStatusPorcelainV2 := false;
  fWithStatusAheadBehihd := false;
  fWithStatusIgnored     := false;
  if result then begin
    fWithStatusPorcelainV2 := fGit.AtLeastVersion('2.11');
    fWithStatusAheadBehihd := fGit.AtLeastVersion('2.17');
    fWithStatusIgnored     := fGit.AtLeastVersion('2.21'); // TODO: check this..
  end;
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
        ParseStatus(head, tail, fUnstagedList, fStagedList, fEntries, fMergingConflict, not fWithStatusPorcelainV2);

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

function TGitMgr.RefListEnabledField(aField: string): boolean;
begin
  result := true;
  if pos('%(worktreepath', aField)=1 then
    // TODO: check the real version worktreepath appeared
    //       here I'm only registering the version it worked for me.
    result := fGit.AtLeastVersion('2.25')

  else if pos(':rstrip', aField)>0 then
    result := fGit.AtLeastVersion('2.13');
end;

procedure TGitMgr.UpdateRefsMap;
var
  i, j, aIndex: Integer;
  info: PRefInfo;
  arr: TRefInfoArray;
  exists: boolean;
  s, sub: string;
begin
  if fRefsMap=nil then begin
    fRefsMap := TRefsMap.Create;
    fRefsMap.Sorted := true;
  end else
    fRefsMap.Clear;

  for i:= 0 to RefList.Count-1 do begin
    info := PRefInfo(RefList.Objects[i]);
    exists := fRefsMap.Find(info^.objName, aIndex);
    if exists then
      arr := fRefsMap.Data[aIndex]
    else
      arr := nil;

    j := Length(arr);
    SetLength(arr, j+1);
    arr[j] := info;

    if info^.objType=rotTag then s := info^.refered^.objName
    else                         s := info^.objName;

    if not exists then
      fRefsMap.Add(s, arr)
    else
      fRefsMap[s] := arr;
  end;

  //DebugLn;
  //for i:=0 to RefList.Count-1 do begin
  //  info := PRefInfo(RefList.Objects[i]);
  //  WriteStr(s, info^.objType);
  //  WriteStr(sub, info^.subType);
  //  DebugLn('%2d. %40s | %12s %12s | %s',[i, info^.objName, s, sub, info^.refName]);
  //  if info^.refered<>nil then begin
  //    info := info^.refered;
  //    WriteStr(s, info^.objType);
  //    WriteStr(sub, info^.subType);
  //    DebugLn(' -> %40s | %12s %12s | %s',[info^.objName, s, sub, info^.refName]);
  //  end;
  //end;
  //DebugLn;
  //for i:=0 to fRefsMap.Count-1 do begin
  //  arr := fRefsMap.Data[i];
  //  DebugLn('%d. %s : %d refs', [i, fRefsMap.Keys[i], Length(arr)]);
  //  for j:=0 to Length(Arr)-1 do
  //    DebugLn('   %s',[arr[j]^.refName]);
  //end;
end;

function TGitMgr.GetRefList: TStringList;
begin
  if fInternalRefList=nil then
    fInternalRefList := TStringList.Create;
  result := fInternalRefList;
end;

function TGitMgr.GetRefsMap: TRefsMap;
begin
  result := fRefsMap;
end;

procedure TGitMgr.OnCommandsDone(Sender: TObject);
var
  thread: TRunThread absolute sender;
begin

end;

procedure TGitMgr.UpdateStatus(ondone: TNotifyEvent);
var
  i: Integer;
  commands: TCommandsArray;
  s: string;
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
    commands[i].Enviroment := '';
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
  commands[i].Enviroment := '';

  i := Length(commands);
  SetLength(commands, i+1);
  commands[i].description := 'status';
  s := fGit.Exe + ' status -b --long --porcelain';
  if fWithStatusPorcelainV2 then s += '=2';
  if fWithStatusAheadBehihd then s += ' --ahead-behind';
  if fWithStatusIgnored then s += ' --ignored=' + BoolToStr(fViewIgnoredFiles, 'traditional', 'no');
  s += format(' --untracked-files=%s -z', [BoolToStr(fViewUntrackedFiles, 'all', 'no')]);
  commands[i].command := s;
  commands[i].RedirStdErr := false;
  commands[i].PreferredOutputType := cipotStream;
  commands[i].Enviroment := 'LANG=C';

  if ondone=nil then
    ondone := @OnCommandsDone;

  RunInThread(commands, fGit.TopLevelDir, @OnCommandProgress, ondone);

  //fObserverMgr.NotifyObservers(self, GITMGR_EVENT_UPDATESTATUS, res);
end;

// Update the internal reflist
procedure TGitMgr.UpdateRefList;
var
  res: Integer;
begin
  GetRefList;

  res := FillRefList(
      fInternalRefList, '', [
      '%(refname:short)',
      '%(refname)',
      '%(objecttype)',
      '%(objectname)',
      '%(upstream:short)',
      '%(HEAD)',
      '%(worktreepath)',
      '%(contents)',
      '%(authorname)',
      '%(authordate)',
      '%(committerdate)',
      '%(creatordate)',
      '%(*objecttype)',
      '%(*objectname)',
      '%(*authorname)',
      '%(*authordate)',
      '%(*contents)'
      ]);

  UpdateRefsMap;

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
  for i:=0 to RefList.Count-1 do begin
    info := PRefInfo(RefList.Objects[i]);
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

procedure TGitMgr.QueueSwitchToCommit(aCommit: string);
var
  info: PTagInfo;
begin
  new(info);
  info^.data := aCommit;
  fObserverMgr.NotifyObservers(Self, GITMGR_EVENT_SWITCHTOCOMMIT, PtrInt(info));
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

function TGitMgr.FillRefList(list: TStrings; pattern: string;
  fields: array of string): Integer;
var
  cmd, field: String;
  M: TMemoryStream;
begin

  ClearRefList(list);

  M := TMemoryStream.Create;
  try
    cmd := '';
    for field in fields do
      if RefListEnabledField(field) then
        cmd += field + '%02';

    cmd := ' for-each-ref --format="' + cmd + '%00"';
    if pattern<>'' then
      cmd += ' ' + pattern;

    //DebugLn('Command: ', cmd);

    result := cmdLine.RunProcess(fGit.Exe + cmd, fGit.TopLevelDir, M);
    if result>0 then
      exit;
    //M.SaveToFile('lookatme.reflist');
    ParseRefList(M, list, #2, fields);
  finally
    M.Free;
  end;
end;

function TGitMgr.RefsFilter(commitOID: string; filter: TRefFilterProc
  ): TRefInfoArray;
var
  aIndex, i: Integer;
  arr: TRefInfoArray;
  info: PRefInfo;

  procedure AddInfo;
  var
    j: Integer;
  begin
    j := Length(result);
    SetLength(result, j+1);
    result[j] := info;
  end;

begin
  result := nil;
  if not Assigned(filter) then
    exit;
  // if commit is not empty and refsMap is loaded, use it
  if (commitOID<>'') and (fRefsMap<>nil) then begin
    if fRefsMap.Find(commitOID, aIndex ) then begin
      arr := fRefsMap.Data[aIndex];
      for info in arr do
        if Filter(info) then AddInfo
      //for i:=0 to Length(arr)-1 do begin
      //  info := arr[i];
      //  if Filter(info) then AddInfo;
      //end;
    end;
  end else
  // use the internalRefList if loaded
  if (fInternalRefList<>nil) then begin
    for i := 0 to fInternalRefList.Count-1 do begin
      info := PRefInfo(fInternalRefList.Objects[i]);
      if (commitOID='') or (commitOID=info^.objName) then begin
        if Filter(info) then AddInfo;
      end;
    end;
  end;

end;

procedure TGitMgr.LoadRepository(aDir: string);
var
  info: PTagInfo;
begin
  new(info);
  info^.data := aDir;
  fObserverMgr.NotifyObservers(Self, GITMGR_EVENT_LOADREPOSITORY, PtrInt(info));
end;

end.

