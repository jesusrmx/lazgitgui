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

  Setup the debug logger
}
unit unitdebug;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lazlogger, unitifaces;

type

  { TDebugging }

  TDebugging = class
  private
    class var fConfig: IConfig;
  public
    class procedure Setup;
    class property Config: IConfig read fConfig write fConfig;
  end;

  procedure DebugLnMultiline(s: string);

implementation

procedure DebugLnMultiline(s: string);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.Text := s;
    for s in L do
      DebugLn(s);
  finally
    L.Free;
  end;
end;

{ TDebugging }

class procedure TDebugging.Setup;
var
  aFile, s: string;
  i: Integer;
begin

  aFile := '';
  for i:=1 to paramCount do begin
    s := paramStr(i);
    if pos('--logfile=', s)=1 then begin
      aFile := copy(s, 11, Length(s));
      break;
    end;
  end;

  if aFile='' then
    aFile := fConfig.ReadString('logfile', '');

  if aFile<>'' then begin
    aFile := ExpandFileName(aFile);
    if not DirectoryExists(ExtractFilePath(aFile)) then
      ForceDirectories(ExtractFilePath(aFile));
    if FileExists(aFile) then
      DeleteFile(aFile);
  end;

  DebugLogger.LogName := aFile;
  DebugLogger.Init;
end;

end.

