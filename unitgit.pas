unit unitgit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, lazlogger, unitprocess, unitentries;

type

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

