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

implementation

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
end;

end.

