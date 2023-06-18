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

  Git interface unit
}
unit unitgit;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, FileUtil, DateUtils, lazlogger, unitgittypes,
  unitifaces, unitprocess,
  unitentries, unitgitutils;

const
  SECTION_GIT = 'lazgitgui.cfg';

type

  { TGit }

  TGit = class(TMyInterfacedObject, IGit)
  private
    fConfig: IConfig;
    fGitCommand: string;
    fTopLevelDir: string;
    fGitCommonDir: string;
    fUntrackedMode: string;
    fIgnoredMode: string;
    fVersion: String;
    fLastTag: string;
    fLastTagOID: string;
    fLastTagCommits: Integer;
    fInternalRefList: TStringList;
    fRefsMap: TRefsMap;
    fLogError: RawByteString;
    function GetErrorLog: RawByteString;
    function GetExe: string;
    function GetLogError: RawBytestring;
    function GetRefList: TStringList; overload;
    function GetRefsMap: TRefsMap;
    function GetTopLevelDir: string;
    function GetGitCommonDir: string;
    function GetVersion: string;
    function  TryGitIn(aPath: string): boolean;
    function GetVersion(gitCmd:string; out aVersion:string): boolean;
    function AtLeastVersion(aVer: string): boolean;
    function RefListEnabledField(aField: string): boolean;
    procedure SetupExe(aExeFile, aVersion: string);
    procedure UpdateRefsMap;
    procedure PushError;
  public
    constructor create;
    destructor destroy; override;
    function Initialize: boolean;
    function Diff(entry: PFileEntry; Unstaged:boolean; Lines:TStrings): Integer;
    function Add(entry: PFileEntry): Integer; overload;
    function Add(entryArray: TPFileEntryArray): Integer; overload;
    function Rm(entry: PFileEntry): Integer;
    function Restore(entry: PFileEntry; staged: boolean): Integer; overload;
    function Restore(entryArray: TPFileEntryArray; staged: boolean): Integer; overload;
    function Reset(opts: string; out outMsg:RawByteString): Integer;
    function BranchList(list: TStrings; opts:array of string): Integer;
    function FillRefList(list: TStrings; pattern:string; fields:array of string): Integer; overload;
    function Switch(branchName: string): Integer;
    function OpenDir(aDir: string): Integer;
    function Commit(msg, opts: string): Integer;
    function Push(repo, opts: string; callback:TOutputEventNested): Integer;
    function Log(opts: string; Lines:TStrings): Integer;
    function Any(cmd: string; out cmdout:RawByteString): Integer;
    function Tag(tagName, tagCommit:string; annotated:boolean; tagMsg:string): Integer;
    function DeleteTag(tagName: string): Integer;
    function AddToIgnoreFile(aFile:string; justType:boolean; global:boolean): boolean;
    function UpdateRefList: Integer;
    function RefsFilter(commitOID: string; filter: TRefFilterProc): TRefInfoArray;
    function Show(obj: string; lines: TStrings): Integer;
    procedure ResetLogError;

    property ErrorLog: RawByteString read GetErrorLog;
    property LogError: RawBytestring read GetLogError;
    property UntrackedMode: string read fUntrackedMode write fUntrackedMode;
    property IgnoredMode: string read fIgnoredMode write fIgnoredMode;
    property Config: IConfig read fConfig write fConfig;

    property Exe: string read GetExe;
    property RefList: TStringList read GetRefList;
    property RefsMap: TRefsMap read GetRefsMap;
    property TopLevelDir: string read GetTopLevelDir;
    property GitCommonDir: string read GetGitCommonDir;
    property Version: string read GetVersion;
  end;

  procedure ClearRefList(list: TStrings);

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

{ TGit }

constructor TGit.create;
begin
  inherited create;
  fUntrackedMode := 'all';
  fIgnoredMode := 'no';
end;

destructor TGit.destroy;
begin
  if fRefsMap<>nil then
    fRefsMap.Clear;
  fRefsMap.Free;
  ClearRefList(fInternalRefList);
  fInternalRefList.Free;
  inherited destroy;
end;

function TGit.Initialize: boolean;
var
  arg, aFile, aVersion: string;
  i: Integer;
begin

  aFile := ''; aVersion := '';
  for i:=1 to ParamCount do begin
    arg := paramStr(i);
    if pos('--git=', arg)=1 then begin
      aFile := copy(arg, 7, Length(arg));
      break;
    end;
  end;

  if (aFile='') and (fConfig<>nil) then begin
    aFile := fConfig.ReadString('git', '', SECTION_GIT);
    aVersion := fConfig.ReadString('gitversion', '', SECTION_GIT);
  end;

  SetupExe(aFile, aVersion);

  result := Exe<>'';
  if result and (fConfig<>nil) then begin
    if (Exe<>aFile) or (Version<>aVersion) then begin
      fConfig.WriteString('git', Exe, SECTION_GIT);
      fConfig.WriteString('gitversion', Version, SECTION_GIT);
    end;
  end;

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

procedure TGit.UpdateRefsMap;
var
  i, j, aIndex: Integer;
  info: PRefInfo;
  arr: TRefInfoArray;
  exists: boolean;
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

    if not exists then
      fRefsMap.Add(info^.objName, arr)
    else
      fRefsMap[info^.objName] := arr;
  end;

  //DebugLn;
  //for i:=0 to RefList.Count-1 do begin
  //  info := PRefInfo(RefList.Objects[i]);
  //  DebugLn('%2d. %s %s',[i, info^.objName, info^.refName]);
  //end;
  //DebugLn;
  //for i:=0 to fRefsMap.Count-1 do begin
  //  arr := fRefsMap.Data[i];
  //  DebugLn('%d. %s : %d refs', [i, fRefsMap.Keys[i], Length(arr)]);
  //  for j:=0 to Length(Arr)-1 do
  //    DebugLn('   %s',[arr[j]^.refName]);
  //end;
end;

procedure TGit.PushError;
begin
  if cmdLine.ErrorLog<>'' then begin
    if fLogError<>'' then
      fLogError += LineEnding;
    fLogError += cmdLine.ErrorLog;
  end;
end;

function TGit.GetErrorLog: RawByteString;
begin
  result := cmdLine.ErrorLog;
end;

function TGit.GetExe: string;
begin
  result := fGitCommand;
end;

function TGit.GetLogError: RawBytestring;
begin
  result := fLogError;
end;

function TGit.GetRefList: TStringList;
begin
  if fInternalRefList=nil then
    fInternalRefList := TStringList.Create;
  result := fInternalRefList;
end;

function TGit.GetRefsMap: TRefsMap;
begin
  result := fRefsMap;
end;

function TGit.GetTopLevelDir: string;
begin
  result := fTopLevelDir;
end;

function TGit.GetGitCommonDir: string;
begin
  result := fGitCommonDir;
end;

function TGit.GetVersion: string;
begin
  result := FVersion;
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
begin

  M := TMemoryStream.Create;
  try
    if Unstaged then  arg := ''
    else              arg := '--cached ';
    aCommand := format('%s diff %s-- %s', [fGitCommand, arg, Sanitize(Entry^.path)]);
    //cmdLine.waitOnExit := true;
    result := cmdLine.RunProcess(aCommand, fTopLevelDir, M);
    if M.Size>0 then begin
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
  result := cmdLine.RunProcess(fGitCommand+' add '+ Sanitize(entry^.path), fTopLevelDir, cmdOut);
  PushError;
end;

function TGit.Add(entryArray: TPFileEntryArray): Integer;
var
  list: string;
  cmdOut: RawByteString;
begin
  list := MakePathList(entryArray);
  result := cmdLine.RunProcess(fGitCommand+' add '+ list, fTopLevelDir, cmdOut);
  PushError;
end;

function TGit.Rm(entry: PFileEntry): Integer;
var
  cmdOut: RawByteString;
begin
  result := cmdLine.RunProcess(fGitCommand+' rm '+ Sanitize(Entry^.path), fTopLevelDir, cmdOut);
  PushError;
end;

function TGit.Restore(entry: PFileEntry; staged: boolean): Integer;
var
  args: string;
  cmdOut: RawByteString;
begin
  if AtLeastVersion('2.23') then begin
    args := ' restore ';
    if staged then args += '--staged ';
  end else begin
    if staged then args := ' reset HEAD '
    else           args := ' checkout -- ';
  end;
  result := cmdLine.RunProcess(fGitCommand+args+' '+Sanitize(Entry^.path), fTopLevelDir, cmdOut);
  PushError;
end;

function TGit.Restore(entryArray: TPFileEntryArray; staged: boolean): Integer;
var
  list, args: string;
  cmdOut: RawByteString;
begin
  list := MakePathList(entryArray);
  if AtLeastVersion('2.23') then begin
    args := ' restore ';
    if staged then args += '--staged -- ';
  end else begin
    if staged then args := ' reset HEAD -- '
    else           args := ' checkout -- ';
  end;
  args += list;
  result := cmdLine.RunProcess(fGitCommand+args, fTopLevelDir, cmdOut);
  PushError;
end;

function TGit.Reset(opts: string; out outMsg: RawByteString): Integer;
begin
  opts := ' reset ' + opts;
  result := cmdLine.RunProcess(fGitCommand+opts, fTopLevelDir, outMsg);
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

    if haveReferedFields and (strpos(p, pchar('refs/tags'+sep))<>nil) then  new(refered)
    else                                                                    refered := nil;
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

function TGit.FillRefList(list: TStrings; pattern: string; fields: array of string): Integer;
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

    result := cmdLine.RunProcess(fGitCommand + cmd, fTopLevelDir, M);
    if result>0 then
      exit;
    //M.SaveToFile('lookatme.reflist');
    ParseRefList(M, list, #2, fields);
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
    if DirectoryExists(aDir + '.git') then begin
      fTopLevelDir := IncludeTrailingPathDelimiter(aDir);
      fGitCommonDir := fTopLevelDir;
    end else begin
      cmdOut := ExcludeTrailingPathDelimiter(aDir);
      if ExtractFileName(cmdOut)='.git' then begin
        fTopLevelDir := ExtractFilePath(cmdOut);
        fGitCommonDir := fTopLevelDir;
      end else begin

        fGitCommonDir := '';
        result := cmdLine.RunProcess(fGitCommand + ' rev-parse --show-toplevel', aDir, cmdOut);
        if result>0 then begin
          DebugLn('Error getting top level directory: (%d) %s', [cmdLine.ExitCode, cmdLine.ErrorLog]);
          exit;
        end;
        fTopLevelDir := IncludeTrailingPathDelimiter(SetDirSeparators(Trim(cmdOut)));
        if DirectoryExists(fTopLevelDir + '.git') then
          fGitCommonDir := fTopLevelDir
        else begin
          if cmdLine.RunProcess(fGitCommand + ' rev-parse --git-common-dir', aDir, cmdOut)>0 then begin
            DebugLn('Error getting git common directory: (%d) %s', [cmdLine.ExitCode, cmdLine.ErrorLog]);
            fTopLevelDir := '';
            exit;
          end;
          fGitCommonDir := ExtractFilePath(trim(cmdout));
        end;
      end;
    end;
  end;
end;

function TGit.Commit(msg, opts: string): Integer;
var
  cmd: string;
  cmdOut: RawByteString;
begin
  cmd := ' commit -m ' + QuoteMsg(msg);
  if opts<>'' then
    cmd += ' '+opts;
  result := cmdLine.RunProcess(fGitCommand + cmd, fTopLevelDir, cmdOut);
end;

function TGit.Push(repo,opts: string; callback:TOutputEventNested): Integer;
var
  cmd: string;
begin
  cmd := ' push '+repo;
  if opts<>'' then
    cmd += ' ' +opts;
  result := cmdLine.RunProcess(fGitCommand + cmd, fTopLevelDir, callback);
end;

function TGit.Log(opts: string; Lines:TStrings): Integer;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    result := cmdLine.RunProcess(fGitCommand + ' log ' + opts, fTopLevelDir, M);
    if M.Size>0 then begin
      M.Position := 0;
      lines.LoadFromStream(M);
    end;
  finally
    M.Free;
  end;
end;

function TGit.Any(cmd: string; out cmdout: RawByteString): Integer;
begin
  result := cmdLine.RunProcess(fGitCommand + ' ' + cmd, fTopLevelDir, cmdOut);
  PushError;
end;

function TGit.Tag(tagName, tagCommit: string; annotated: boolean; tagMsg: string): Integer;
var
  cmd: String;
  cmdOut: RawByteString;
begin
  cmd := ' tag ';
  if annotated then begin
    cmd += '-a ';
    if tagMsg<>'' then
      cmd += '-m ' + QuoteMsg(tagMsg) + ' ';
  end;
  cmd += tagName;
  if tagCommit<>'' then
    cmd += ' ' + tagCommit;
  result := cmdLine.RunProcess(fGitCommand + cmd, fTopLevelDir, cmdOut);
end;

function TGit.DeleteTag(tagName: string): Integer;
var
  cmd: String;
  cmdOut: RawByteString;
begin
  cmd := ' tag -d ' + tagName;
  result := cmdLine.RunProcess(fGitCommand + cmd, fTopLevelDir, cmdOut);
end;

function TGit.AddToIgnoreFile(aFile: string; justType: boolean; global: boolean
  ): boolean;
var
  l: TStringList;
  aPath, gitIgnoreFile: string;
begin
  result := false;

  l := TStringList.Create;
  try
    aPath := fTopLevelDir;
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

// Update the internal reflist
function TGit.UpdateRefList: Integer;
begin

  GetRefList;

  result := FillRefList(
      fInternalRefList, '', [
      '%(refname:short)',
      '%(refname:rstrip=-2)',
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
end;

function TGit.RefsFilter(commitOID: string; filter: TRefFilterProc
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

function TGit.Show(obj: string; lines: TStrings): Integer;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    result := cmdLine.RunProcess(fGitCommand + ' show ' + Obj, fTopLevelDir, M);
    if M.Size>0 then begin
      M.Position := 0;
      lines.LoadFromStream(M);
    end;
  finally
    M.Free;
  end;
end;

procedure TGit.ResetLogError;
begin
  fLogError := '';
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

// compares if the current version is equal or bigger than aVer
function TGit.AtLeastVersion(aVer: string): boolean;
var
  cur: String;
begin
  cur := copy(fVersion, 1, Length(aVer));
  result := cur>=aVer;
end;

function TGit.RefListEnabledField(aField: string): boolean;
begin
  result := true;
  if pos('%(worktreepath', aField)=1 then
    // TODO: check the real version worktreepath appeared
    //       here I'm only registering the version it worked for me.
    result := AtLeastVersion('2.25');

end;

end.

