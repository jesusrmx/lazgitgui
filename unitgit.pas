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
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
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
    fGitDir: string;
    fVersion: String;
    fLogError: RawByteString;
    function GetErrorLog: RawByteString;
    function GetExe: string;
    function GetLogError: RawBytestring;
    function GetTopLevelDir: string;
    function GetGitDir: string;
    function GetVersion: string;
    function TryGitIn(aPath: string): boolean;
    function GetVersion(gitCmd:string; out aVersion:string): boolean;
    procedure SetupExe(aExeFile, aVersion: string);
    procedure PushError;
  public
    constructor create;
    function AtLeastVersion(aVer: string): boolean;
    function Initialize: boolean;
    function Diff(entry: PFileEntry; Unstaged:boolean; Lines:TStrings): Integer; overload;
    function Diff(cmd: string; Lines:TStrings): Integer; overload;
    function Add(entry: PFileEntry): Integer; overload;
    function Add(entryArray: TPFileEntryArray): Integer; overload;
    function Rm(entry: PFileEntry): Integer;
    function Restore(entry: PFileEntry; staged: boolean): Integer; overload;
    function Restore(entryArray: TPFileEntryArray; staged: boolean): Integer; overload;
    function Reset(opts: string; out outMsg:RawByteString): Integer;
    function Switch(branchName: string): Integer;
    function OpenDir(aDir: string): Integer;
    function Commit(msg, opts: string): Integer;
    function Push(repo, opts: string; callback:TOutputEventNested): Integer;
    function Log(opts: string; Lines:TStrings): Integer;
    function Any(cmd: string; out cmdout:RawByteString): Integer;
    function Tag(tagName, tagCommit:string; annotated:boolean; tagMsg:string): Integer;
    function DeleteTag(tagName: string): Integer;
    function Show(obj: string; lines: TStrings): Integer;
    procedure ResetLogError;

    property ErrorLog: RawByteString read GetErrorLog;
    property LogError: RawBytestring read GetLogError;
    property Config: IConfig read fConfig write fConfig;

    property Exe: string read GetExe;
    property TopLevelDir: string read GetTopLevelDir;
    property GitDir: string read GetGitDir;
    property Version: string read GetVersion;
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

function TGit.GetTopLevelDir: string;
begin
  result := fTopLevelDir;
end;

function TGit.GetGitDir: string;
begin
  result := fGitDir;
end;

function TGit.GetVersion: string;
begin
  result := FVersion;
end;

function TGit.TryGitIn(aPath: string): boolean;
begin
  aPath := aPath + 'git' + EXE_EXTENSION;
  result := FileExists(aPath);
  if result then
    fGitCommand := aPath;
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

function TGit.Diff(cmd: string; Lines: TStrings): Integer;
var
  aCommand: string;
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    aCommand := format('%s diff %s', [fGitCommand, cmd]);
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
      fGitDir := fTopLevelDir;
    end else begin
      cmdOut := ExcludeTrailingPathDelimiter(aDir);
      if ExtractFileName(cmdOut)='.git' then begin
        fTopLevelDir := ExtractFilePath(cmdOut);
        fGitDir := fTopLevelDir;
      end else begin

        fGitDir := '';
        result := cmdLine.RunProcess(fGitCommand + ' rev-parse --show-toplevel', aDir, cmdOut);
        if result>0 then begin
          DebugLn('Error getting top level directory: (%d) %s', [cmdLine.ExitCode, cmdLine.ErrorLog]);
          exit;
        end;
        fTopLevelDir := IncludeTrailingPathDelimiter(SetDirSeparators(Trim(cmdOut)));
        if DirectoryExists(fTopLevelDir + '.git') then
          fGitDir := fTopLevelDir
        else begin
          if cmdLine.RunProcess(fGitCommand + ' rev-parse --git-common-dir', aDir, cmdOut)>0 then begin
            DebugLn('Error getting git common directory: (%d) %s', [cmdLine.ExitCode, cmdLine.ErrorLog]);
            fTopLevelDir := '';
            exit;
          end;
          fGitDir := ExtractFilePath(trim(cmdout));
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

end.

