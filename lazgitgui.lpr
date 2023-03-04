program lazgitgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  SysUtils, Interfaces, // this includes the LCL widgetset
  Forms, main, unitconfig, unitprocess
  { you can add units after this };

{$R *.res}

begin

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
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

