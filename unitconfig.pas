unit unitconfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

const
  DEF_SECTION = 'options';

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
    function ReadString(aKey:string; default:string=''; section:string=DEF_SECTION): string;
    procedure WriteString(aKey:string; avalue:string; section:string=DEF_SECTION);
  end;

implementation

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

function TConfig.ReadString(aKey: string; default: string; section: string
  ): string;
begin
  OpenConfig;
  result := fIniFile.ReadString(section, aKey, default);
  CloseConfig;
end;

procedure TConfig.WriteString(aKey: string; avalue: string; section: string);
begin
  OpenConfig;
  fIniFile.WriteString(section, aKey, aValue);
  CloseConfig;
end;

end.

