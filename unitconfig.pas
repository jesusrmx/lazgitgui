unit unitconfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IniFiles, LazLogger, Graphics, Forms;

const
  SECTION_DEFAULT = 'options';
  SECTION_GEOMETRY = 'geometry';
  SECTION_FONTS = 'fonts';

type

  { TConfig }

  TConfig = object
  private
    fConfigFile: string;
    fConfigFileOpenCount: Integer;
    fIniFile: TIniFile;
    procedure CheckConfigFile;
  public
    procedure OpenConfig;
    procedure CloseConfig;
    procedure ReadWindow(aForm: TForm; aKey:string; section:string=SECTION_DEFAULT);
    procedure WriteWindow(aForm: TForm; aKey:string; section:string=SECTION_DEFAULT);
    procedure ReadFont(aFont: TFont; aKey:string; defPitch:TFontPitch=fpFixed; section:string=SECTION_DEFAULT);
    function ReadString(aKey:string; default:string=''; section:string=SECTION_DEFAULT): string;
    function ReadBoolean(aKey:string; default:boolean=false; section:string=SECTION_DEFAULT): boolean;
    function ReadInteger(aKey:string; default:Integer=0; section:string=SECTION_DEFAULT): Integer;
    procedure WriteString(aKey:string; avalue:string; section:string=SECTION_DEFAULT);
    procedure WriteBoolean(aKey:string; avalue:boolean; section:string=SECTION_DEFAULT);
    procedure WriteInteger(aKey:string; avalue:Integer; section:string=SECTION_DEFAULT);
  end;

  procedure Setup;

var
  fConfig: TConfig;

implementation

procedure Setup;
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

{ TConfig }

procedure TConfig.CheckConfigFile;
begin
  if fConfigFile='' then begin
    fConfigFile := GetAppConfigFile(false, true);
    ForceDirectories(ExtractFilePath(fConfigFile));
  end;
end;

procedure TConfig.OpenConfig;
begin
  if fConfigFileOpenCount=0 then begin
    CheckConfigFile;
    fIniFile := TIniFile.Create(fConfigFile);
  end;
  inc(fConfigFileOpenCount);
end;

procedure TConfig.CloseConfig;
begin
  dec(fConfigFileOpenCount);
  if fConfigFileOpenCount<=0 then begin
    if fIniFIle<>nil then
      FreeAndNil(fIniFile);
    fConfigFileOpenCount := 0;
  end;
end;

procedure TConfig.ReadWindow(aForm: TForm; aKey: string; section: string);
begin
  aForm.Left :=   fConfig.ReadInteger(aKey + '.Left',    aForm.Left,   section);
  aForm.Top :=    fConfig.ReadInteger(aKey + '.Top',     aForm.Top,    section);
  aForm.Width :=  fConfig.ReadInteger(aKey + '.Width',   aForm.Width,  section);
  aForm.Height := fConfig.ReadInteger(aKey + '.Height',  aForm.Height, section);
  if fConfig.ReadBoolean(aKey + '.maximized', false, section) then
    aForm.WindowState := wsMaximized;
end;

procedure TConfig.WriteWindow(aForm: TForm; aKey: string; section: string);
var
  isMaximized: Boolean;
begin
  isMaximized := aForm.WindowState=wsMaximized;
  fConfig.WriteBoolean(aKey + '.maximized', isMaximized, SECTION_GEOMETRY);
  if not isMaximized then begin
    fConfig.WriteInteger(aKey + '.Left', aForm.Left, SECTION_GEOMETRY);
    fConfig.WriteInteger(aKey + '.Top', aForm.Top, SECTION_GEOMETRY);
    fConfig.WriteInteger(aKey + '.Width', aForm.Width, SECTION_GEOMETRY);
    fConfig.WriteInteger(aKey + '.Height', aForm.Height, SECTION_GEOMETRY);
  end;
end;

procedure TConfig.ReadFont(aFont: TFont; aKey: string; defPitch: TFontPitch;
  section: string);
var
  s: String;
  aQuality: TFontQuality;
begin
  with aFont do begin
    s := Name;
    aQuality := Quality;
    if defPitch=fpFixed then begin
      {$ifdef Darwin}
      s := 'Menlo';
      aQuality := fqAntialiased;
      {$endif}
      {$ifdef MsWindows}
      s := 'Courier New';
      {$endif}
    end;
    Name := fConfig.ReadString(aKey+'.font.name', s, section);
    Size := fConfig.ReadInteger(aKey+'.font.size', 10, section);
    if fConfig.ReadBoolean(aKey+'.font.antialiased', aQuality=fqAntialiased, section) then
      Quality := fqAntialiased
    else
      Quality := fqNonAntialiased;
  end;
end;

function TConfig.ReadString(aKey: string; default: string; section: string
  ): string;
begin
  OpenConfig;
  result := fIniFile.ReadString(section, aKey, default);
  CloseConfig;
end;

function TConfig.ReadBoolean(aKey: string; default: boolean; section: string
  ): boolean;
begin
  OpenConfig;
  result := fIniFile.ReadBool(section, aKey, default);
  CloseConfig;
end;

function TConfig.ReadInteger(aKey: string; default: Integer; section: string
  ): Integer;
begin
  OpenConfig;
  result := fIniFile.ReadInteger(Section, aKey, default);
  CloseConfig;
end;

procedure TConfig.WriteString(aKey: string; avalue: string; section: string);
begin
  OpenConfig;
  fIniFile.WriteString(section, aKey, aValue);
  CloseConfig;
end;

procedure TConfig.WriteBoolean(aKey: string; avalue: boolean; section: string);
begin
  OpenConfig;
  fIniFile.WriteBool(Section, aKey, aValue);
  CloseConfig;
end;

procedure TConfig.WriteInteger(aKey: string; avalue: Integer; section: string);
begin
  OpenConfig;
  fIniFile.WriteInteger(Section, aKey, aValue);
  CloseConfig;
end;

end.

