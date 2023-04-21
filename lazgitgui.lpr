{ LazGitGui: An interface to git status with some additional tools
             with a familiar git gui interface.

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

  Program file
}
program lazgitgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  SysUtils, Classes, Interfaces, // this includes the LCL widgetset
  Forms, main, unitconfig, unitprocess, unitentries, unitgit, unitnewbranch,
  unitruncmd, unitansiescapes, unitnewtag, unitlog, unitdebug, unitlogcache,
  unitdbindex, unitgitutils, unitdrafts, unitframelog, unitgitmgr, unitgittypes
  { you can add units after this };

{$R *.res}

{.$define drafts}

begin

  {$ifdef drafts}
  //TestParams;
  AnalizeColumns;
  {$else}

  fConfig := TConfig.Create;

  TDebugging.Config := fConfig;
  TDebugging.Setup;

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
  Application.Run;

  {$endif}

end.

