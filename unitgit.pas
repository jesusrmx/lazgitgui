unit unitgit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateUtils, lazlogger, unitprocess, unitentries;

type

  TRefObjectType = (rotBlob, rotTree, rotCommit, rotTag);
  TRefObjectSubType = (rostLocal, rostTracking, rostTag, rostOther);

  PRefInfo = ^TRefInfo;
  TRefInfo = record
    refName: string;
    objType: TRefObjectType;
    objName: string;
    upstream: string;
    push: string;
    head: boolean;
    worktreepath: string;
    subject: string; // for branch list
    authorName: string;
    authorDate: TDateTime;
    commiterDate: TDateTime;
    creatorDate: TDateTime;
    isTracking: boolean;
    subType: TRefObjectSubType;
    refered: PRefInfo;
  end;

  { TGit }

  TGit = class
  private
    fBranch: String;
    fGitCommand: string;
    fTopLevelDir: string;
    fMerging, fMergingConflict: Boolean;
    fUpstream: String;
    fUntrackedMode: string;
    fIgnoredMode: string;
    fCommitsAhead: Integer;
    fCommitsBehind: Integer;
    fEntries: TFPList;
    fVersion: String;
    function GetErrorLog: RawByteString;
    procedure GitStatusBranch(var head: pchar; tail: pchar);
    procedure GitStatusFiles(var head: pchar; tail: pchar; lstUnstaged, lstStaged: TStrings);
    function  TryGitIn(aPath: string): boolean;
    function GitMerging: boolean;
    function GetVersion(gitCmd:string; out aVersion:string): boolean;
  public
    constructor create;
    destructor destroy; override;
    procedure SetupExe(aExeFile, aVersion: string);
    procedure Clear;
    function Status(unstagedList, stagedList: TStrings): Integer;
    function Diff(entry: PFileEntry; Unstaged:boolean; Lines:TStrings): Integer;
    function Add(entry: PFileEntry): Integer;
    function Rm(entry: PFileEntry): Integer;
    function Restore(entry: PFileEntry; staged: boolean): Integer;
    function BranchList(list: TStrings; opts:array of string): Integer;
    function RefList(list: TStrings; pattern:string; opts:array of string; eolRepl:string=#1): Integer;
    function Switch(branchName: string): Integer;
    function OpenDir(aDir: string): Integer;

    property Exe: string read fGitCommand;
    property CommitsAhead: Integer read fCommitsAhead;
    property CommitsBehind: Integer read fCommitsBehind;
    property Branch: string read fBranch;
    property Merging: boolean read fMerging;
    property MergingConflict: boolean read fMergingConflict;
    property Upstream: string read fUpstream;
    property TopLevelDir: string read fTopLevelDir;
    property ErrorLog: RawByteString read GetErrorLog;
    property Version: string read FVersion;
  end;

  procedure ClearRefList(list: TStrings);
  function GitDateToDateTime(s: string): TDateTime;
  function DateTimeToGitFmt(d: TDateTime): string;

implementation

const
  {$ifdef MsWindows}
  EXE_EXTENSION = '.exe';
  {$else}
  EXE_EXTENSION = '';
  {$endif}

function FindPattern(var head:pchar; tail: pchar; pattern: string): boolean;
var
  len: SizeInt;
  p: pchar;
begin
  result := false;
  while head<tail do begin
    p := strpos(head, pchar(pattern));
    if p<>nil then begin
      head := p;
      result := true;
      break;
    end;
    len := strlen(head);
    head := head + len + 1;
  end;
end;

procedure ClearRefList(list: TStrings);
var
  i: Integer;
  info: PRefInfo;
begin
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

{ TGit }

constructor TGit.create;
begin
  inherited create;
  fUntrackedMode := 'all';
  fIgnoredMode := 'no';
  fEntries := TFpList.Create;
end;

destructor TGit.destroy;
begin
  clear;
  fEntries.Free;
  inherited destroy;
end;

procedure TGit.SetupExe(aExeFile, aVersion: string);
begin
  fGitCommand := aExeFile;
  if (fGitCommand='') or (not FileExists(fGitCommand)) then
    fGitCommand := FindDefaultExecutablePath('git' + EXE_EXTENSION);

  {$ifdef MsWindows}
  if (fGitCommand='') then begin
    // try some known git locations
    if not TryGitIn(GetEnvironmentVariable('ProgramFiles') + '\git\bin\') then
    if not TryGitIn(GetEnvironmentVariable('ProgramW6432') + '\git\bin\') then
    if not TryGitIn(GetEnvironmentVariable('SystemDrive') + '\msysgit\bin\') then
    if not TryGitIn(GetEnvironmentVariable('HOMEDRIVE') + '\msysgit\bin\') then
  end;
  {$endif}

  if fGitCommand<>'' then begin
    // if version is given assume it's ok (it should come from config file)
    if (aVersion='') and not GetVersion(fGitCommand, aVersion) then
      exit;
    fVersion := aVersion;
  end;
end;

procedure TGit.Clear;
var
  i: Integer;
  entry: PFileEntry;
begin
  if fEntries<>nil then begin
    for i:=0 to fEntries.Count-1 do begin
      entry := PFileEntry(fEntries[i]);
      if entry<>nil then
        Dispose(entry)
    end;
    fEntries.Clear;
  end;
end;

function TGit.Status(unstagedList, stagedList: TStrings): Integer;
var
  aCommand: string;
  M: TMemoryStream;
  head, tail: PChar;
begin
  DebugLn('Status ----------------------------------------------');
  M := TMemoryStream.Create;
  try
    aCommand := format('%s status -b --long --porcelain=2 --ahead-behind --ignored=%s --untracked-files=%s -z',
      [fGitCommand, fIgnoredMode, fUntrackedMode]);

    result := cmdLine.RunProcess(aCommand, fTopLevelDir, M);
    head := M.Memory;
    tail := head + M.Size;
    fMergingConflict := false;
    fMerging := GitMerging;
    GitStatusBranch(head, tail);
    GitStatusFiles(head, tail, unstagedList, stagedList);
  finally
    M.Free;
  end;
end;

procedure TGit.GitStatusBranch(var head: pchar; tail: pchar);
var
  ab: string;
  i, n: Integer;
begin
  fBranch := '';
  fUpstream := '';
  fCommitsAhead := 0;
  fCommitsBehind := 0;
  ab := '';

  // scan header lines
  while (head<tail) and (head^='#') do begin

    n := strlen(head);

    if (fBranch='') and (strlcomp(head, '# branch.head', 13)=0) then begin
      SetString(fBranch, head + 14, n - 14);
    end else
    if (fUpstream='') and (strlcomp(head, '# branch.upstream', 17)=0) then begin
      SetString(fUpstream, head + 18, n - 18);
    end else
    if (ab='') and (strlcomp(head, '# branch.ab', 11)=0) then begin
      SetString(ab, head + 12, n - 12);
      i := pos(' ', ab);
      fCommitsAhead := StrToIntDef(copy(ab, 1, i-1), 0);
      fCommitsBehind := StrToIntDef(copy(ab, i+1, Length(ab)), 0);
    end;

    inc(head, n + 1);
  end;
end;

function TGit.GetErrorLog: RawByteString;
begin
  result := cmdLine.ErrorLog;
end;

procedure TGit.GitStatusFiles(var head: pchar; tail: pchar; lstUnstaged,
  lstStaged: TStrings);
var
  n: Integer;
  entry: PFileEntry;
  start: pchar;
begin
  // clear lists
  Clear;
  lstUnstaged.Clear;
  lstStaged.Clear;

  // scan header lines
  while (head<tail) do begin

    start := head;
    n := strlen(head);
    DebugLn(start);

    case head^ of
      '1': ParseOrdinaryChanged(head, tail, entry);
      '2': ParseRenamedCopied(head, tail, entry);
      'u':
        begin
          fMergingConflict := true;
          ParseUnmerged(head, tail, entry);
        end;
      '?',
      '!': ParseOther(head, tail, entry);
      else entry := nil;
    end;

    if entry<>nil then begin
      fEntries.Add(entry);

      // staged list
      case entry^.EntryTypeStaged of
        etUpdatedInIndex..etDeletedFromIndex:
          lstStaged.AddObject(entry^.path, TObject(entry));
        etRenamedInIndex..etCopiedInIndexD:
          lstStaged.AddObject(entry^.origPath + ' -> ' + entry^.path, TObject(entry));
      end;

      // unstaged list
      case entry^.EntryTypeUnStaged of
        etUnknown:;
        etUpdatedInIndex..etCopiedInIndexD:;
        etIndexAndWorktreeMatchesM..etIndexAndWorktreeMatchesC:;
        else
          lstUnstaged.AddObject(entry^.path, TObject(entry));
      end;

    end;

    head := start + n + 1;
  end;
end;

function TGit.TryGitIn(aPath: string): boolean;
begin
  aPath := aPath + 'git' + EXE_EXTENSION;
  result := FileExists(aPath);
end;

function TGit.Diff(entry: PFileEntry; Unstaged: boolean; Lines: TStrings
  ): Integer;
var
  aCommand, arg: string;
  M: TMemoryStream;
  head, tail: PChar;
begin

  M := TMemoryStream.Create;
  try
    if Unstaged then  arg := ''
    else              arg := '--cached ';
    aCommand := format('%s diff %s%s', [fGitCommand, arg, Entry^.path]);
    //cmdLine.waitOnExit := true;
    result := cmdLine.RunProcess(aCommand, fTopLevelDir, M);
    if result=0 then begin
      M.Position := 0;
      lines.LoadFromStream(M);
    end;
  finally
    M.Free;
  end;
end;

function TGit.Add(entry: PFileEntry): Integer;
var
  cmdOut: RawByteString;
begin
  result := cmdLine.RunProcess(fGitCommand+' add '+ entry^.path, fTopLevelDir, cmdOut);
end;

function TGit.Rm(entry: PFileEntry): Integer;
var
  cmdOut: RawByteString;
begin
  result := cmdLine.RunProcess(fGitCommand+' rm '+ Entry^.path, fTopLevelDir, cmdOut);
end;

function TGit.Restore(entry: PFileEntry; staged: boolean): Integer;
var
  args: string;
  cmdOut: RawByteString;
begin
  args := '';
  if staged then args += '--staged ';
  result := cmdLine.RunProcess(fGitCommand+' restore '+args+' '+Entry^.path, fTopLevelDir, cmdOut);
end;

function TGit.BranchList(list: TStrings; opts: array of string): Integer;
var
  opt, cmd: String;
begin
  list.Clear;
  cmd := '';
  for opt in opts do begin
    if cmd<>'' then cmd += '|';
    cmd += opt;
  end;
  cmd := ' branch -vv --format="' + cmd + '" -a';
  result := cmdLine.RunProcess(fGitCommand + cmd, fTopLevelDir, List);
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

function GitDateToDateTime(s: string): TDateTime;
  function MonthToInt(m: string): Integer;
  begin
    case m of
      'Jan': result := 1;
      'Feb': result := 2;
      'Mar': result := 3;
      'Apr': result := 4;
      'May': result := 5;
      'Jun': result := 6;
      'Jul': result := 7;
      'Aug': result := 8;
      'Sep': result := 9;
      'Oct': result := 10;
      'Nov': result := 11;
      'Dec': result := 12;
      else   result := 0;
    end;
  end;
var
  i, mmm, dd, yyyy: Integer;
  tt: string;
begin
  // Mon Dec 6 00:39:46 2021 +0100
  if (Length(s)<29) or (Length(s)>30) then
    exit(0);

  mmm := MonthToInt(copy(s, 5, 3));
  dd := StrToIntDef(Trim(copy(s, 9, 2)), -1);
  if dd<0 then
    exit(0);

  i := 10;
  if s[11]=' ' then inc(i);
  delete(s, 1, i);
  tt := copy(s, 1, 8);
  yyyy := StrToIntDef(copy(s, 10, 4), -1);
  if yyyy<0 then
    exit(0);
  delete(s, 1, 14);
  insert(':', s, 4);

  s := format('%.4d-%.2d-%.2dT%sZ%s',[yyyy, mmm, dd, tt, s]);
  result := ISO8601ToDateDef(s, 0);
  result := UniversalTimeToLocal(result);
end;

function DateTimeToGitFmt(d: TDateTime): string;
begin
  //result := FormatDateTime('ddd mmm d hh:nn:ss yyyy', d);
  result := DateTimeToStr(d);
end;

function ParseRefList(M: TMemoryStream; list: TStrings; sep:string; eolrepl:string; opts:array of string): boolean;
var
  p, q, r, t, start: pchar;
  info, refered: PRefInfo;
  fieldIndex, n, i: Integer;
  arg, value, fld, item: string;
  haveReferedFields: boolean;
begin

  haveReferedFields := false;
  for arg in opts do
    if (arg<>'') and (pos('%(*', arg)>0) then begin
      haveReferedFields := true;
      break;
    end;

  result := false;
  p := m.Memory;
  t := p + m.size;

  while p<t do begin

    // find length of single line "record"
    start := p;
    n := strlen(p);
    q := p + n;

    new(info);
    if haveReferedFields and (strpos(p, pchar('tag'+sep))<>nil) then new(refered)
    else                                                             refered := nil;
    info^.refered := refered;

    // each returned field should correspond to every opts[] requested
    fieldIndex := 0;
    while (p<q) and (fieldIndex<Length(opts)) do begin

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
      fld := CleanRefField(opts[fieldIndex], arg);
      case fld of
        'refname':
          begin
            info^.refName := value;
            info^.isTracking := ((arg='') and (pos('refs/remotes', value)=1)) or
                                ((arg='short') and (pos('/', value)>0));
          end;
        'objecttype': info^.objType := StrToRefObjType(value);
        'objectname': info^.objName := value;
        'upstream': info^.upstream := value;
        'push': info^.push := value;
        'HEAD': info^.head := value='*';
        'worktreepath': info^.worktreepath := value;
        'contents':
          begin
            value := StringReplace(value, #13#10, eolRepl, [rfReplaceAll]);
            value := StringReplace(value, #10, eolRepl, [rfReplaceAll]);
            info^.subject :=  value;
          end;
        'authorname': info^.authorName := value;
        'authordate': info^.authorDate := GitDateToDateTime(value);
        'committerdate': info^.commiterDate := GitDateToDateTime(value);
        'creatordate': info^.creatorDate := GitDateToDateTime(value);
        '*objecttype': if refered<>nil then refered^.objType := StrToRefObjType(value);
        '*objectname': if refered<>nil then refered^.objName := value;
        '*authorname': if refered<>nil then refered^.authorName := value;
        '*authordate': if refered<>nil then refered^.authorDate := GitDateToDateTime(value);
        '*contents': if refered<>nil then refered^.subject := value;
      end;

      inc(fieldIndex);
      p := r + Length(sep);
    end;

    p := q + 1;
    // skip the eol that for-each-ref always add
    while (p<t) and (p^ in [#10, #13]) do inc(p);

    list.addObject(item, TObject(info));
  end;


  result := true;
end;

function TGit.RefList(list: TStrings; pattern: string; opts: array of string;
  eolRepl: string): Integer;
var
  cmd, s: String;
  M: TMemoryStream;
begin

  ClearRefList(list);

  M := TMemoryStream.Create;
  try
    cmd := '';
    for s in opts do
      cmd += s + '%02';

    cmd := ' for-each-ref --format="' + cmd + '%00"';
    if pattern<>'' then
      cmd += ' ' + pattern;

    //DebugLn('Command: ', cmd);

    result := cmdLine.RunProcess(fGitCommand + cmd, fTopLevelDir, M);
    if result>0 then
      exit;
    //M.SaveToFile('lookatme.txt');
    ParseRefList(M, list, #2, eolRepl, opts);
  finally
    M.Free;
  end;
end;

function TGit.Switch(branchName: string): Integer;
var
  cmdOut: RawByteString;
  cmd: String;
begin
  cmd := ' checkout ' + branchName;
  result := cmdLine.RunProcess(fGitCommand + cmd, fTopLevelDir, cmdOut);
end;

function TGit.OpenDir(aDir: string): Integer;
var
  cmdOut: RawByteString;
begin
  result := 0;
  if (fTopLevelDir='') or (pos(fTopLevelDir, aDir)<>1) then begin
    if DirectoryExists(aDir + '.git') then
      fTopLevelDir := IncludeTrailingPathDelimiter(aDir)
    else begin
      result := cmdLine.RunProcess(fGitCommand + ' rev-parse --show-toplevel', aDir, cmdOut);
      if result<>0 then
        DebugLn('Error getting top level directory: (%d) %s', [cmdLine.ExitCode, cmdLine.ErrorLog])
      else
        fTopLevelDir := IncludeTrailingPathDelimiter(SetDirSeparators(Trim(cmdOut)));
    end;
  end;
end;

function TGit.GitMerging: boolean;
var
  cmdOut: RawByteString;
begin
  //result := FileExists(fTopLevelDir + '.git/MERGE_HEAD');
  // ref: https://stackoverflow.com/a/55192451
  // todo: close output and close stderr
  result := cmdLine.RunProcess(fGitCommand + ' rev-list -1 MERGE_HEAD', fTopLevelDir, cmdOut) = 0;
end;

function TGit.GetVersion(gitCmd: string; out aVersion: string): boolean;
var
  cmdOut: RawByteString;
begin
  cmdLine.RunProcess(gitCmd + ' --version', GetCurrentDir, cmdOut);
  result := pos('git version', cmdOut)=1;
  if result then
    aVersion := Trim(copy(cmdOut, 13, 256))
  else
    aVersion := '';
end;

end.

