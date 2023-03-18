program lazgitgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  SysUtils, Classes, Interfaces, lazfileutils,// this includes the LCL widgetset
  Forms, main, unitconfig, unitprocess, unitentries, unitgit, unitnewbranch,
  unitruncmd
  { you can add units after this };

{$R *.res}

{.$define TestParams}

{$ifdef TestParams}

procedure Dump(msg: string; l: TStringList);
var
  s: string;
begin
  WriteLn(msg);
  for s in l do
    WriteLn('  |', s,'|');
  l.clear;
end;

var
  l: TStringList;
  cmd, arg: string;
{$endif}
begin

  {$ifdef TestParams}
  l := TStringList.Create;
  try
    WriteLn;
    {$ifdef MsWindows}
    Write('Simulating ');
    {$endif}
    WriteLn('Linux/MacOS');
    cmd := '/home/prog/files good/git.exe';
    arg := ' commit -m "This isn''t a \"text\" ''message'' of 1\" inch long"';
    SplitParameters(cmd + arg, l);
    dump('Using SplitParameters', l);
    SplitCmdLineParams(cmd + arg, l, true);
    dump('Using SplitCmdLineParams with ReadBackSlash=TRUE', l);
    SplitCmdLineParams(cmd + arg, l, false);
    dump('Using SplitCmdLineParams with ReadBackSlash=FALSE', l);

    WriteLn;
    {$ifndef MsWindows}
    Write('Simulating ');
    {$endif}
    WriteLn('Windows');
    cmd := 'c:\home\prog\files good\git.exe';
    SplitParameters(cmd + arg, l);
    dump('Using SplitParameters', l);
    SplitCmdLineParams(cmd + arg, l, true);
    dump('Using SplitCmdLineParams with ReadBackSlash=TRUE', l);
    SplitCmdLineParams(cmd + arg, l, false);
    dump('Using SplitCmdLineParams with ReadBackSlash=FALSE', l);
  finally
    l.Free;
  end;

  {$else}

  Setup;

  if ParamCount=0 then begin
    WriteLn(stdErr, 'A file or directory is needed in the command line');
    exit;
  end;

  targetDir := ParamStr(ParamCount);
  if FileExists(targetDir) then
    targetDir := ExtractFilePath(targetDir)
  else
  if not DirectoryExists(targetDir) then begin
    WriteLn(stdErr, 'No file or directory '+targetDir+' exists');
    exit;
  end;

  targetDir := IncludeTrailingPathDelimiter(targetDir);

  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmRunCommand, frmRunCommand);
  Application.Run;

  {$endif}

end.

