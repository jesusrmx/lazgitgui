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

  Config unit
}
unit unitconfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, {$ifdef UseINI}IniFiles,{$endif} fpjson, jsonparser,
  LazLogger, Graphics, Menus, Forms,
  unitcommon, unitifaces;

type

  { TConfig }

  TConfig = class(TInterfacedObject, IConfig)
  private
    fConfigFile: string;
    fConfigFileOpenCount: Integer;
    {$IFDEF UseINI}
    fIniFile: TIniFile;
    {$ENDIF}
    fJsonFile: TJsonObject;
    fShowTags: boolean;
    fViewIgnoredFiles: boolean;
    fViewUntrackedFiles: boolean;
    fViewTrackedFiles: boolean;
    procedure CheckConfigFile;
    procedure SetShowTags(AValue: boolean);
    procedure SetViewIgnoredFiles(AValue: boolean);
    procedure SetViewTrackedFiles(AValue: boolean);
    procedure SetViewUntrackedFiles(AValue: boolean);
    function  GetLastParent(var aKey: string): TJsonObject;
    function  GetObject(section: string; var aKey: string): TJsonObject; overload;
    function  GetArray(aKey: string): TJsonArray;
    function  GetObject(aKey: string): TJsonObject; overload;
  public
    procedure OpenConfig;
    procedure CloseConfig;
    procedure ReadWindow(aForm: TForm; aKey:string; section:string=SECTION_DEFAULT);
    procedure WriteWindow(aForm: TForm; aKey:string; section:string=SECTION_DEFAULT);
    procedure ReadFont(aFont: TFont; aKey:string; defPitch:TFontPitch=fpFixed; section:string=SECTION_DEFAULT);
    function MenuMRE(aMRE: TComponent; save: boolean; onclick:TNotifyEvent; aCaption: string; section:string=SECTION_DEFAULT): TMenuItem;
    function ReadString(aKey:string; default:string=''; section:string=SECTION_DEFAULT): string;
    function ReadBoolean(aKey:string; default:boolean=false; section:string=SECTION_DEFAULT): boolean;
    function ReadInteger(aKey:string; default:Integer=0; section:string=SECTION_DEFAULT): Integer;
    procedure WriteString(aKey:string; avalue:string; section:string=SECTION_DEFAULT);
    procedure WriteBoolean(aKey:string; avalue:boolean; section:string=SECTION_DEFAULT);
    procedure WriteInteger(aKey:string; avalue:Integer; section:string=SECTION_DEFAULT);
    {$IFDEF UseINI}
    procedure ReadSection(section:string; strings:TStrings);
    procedure WriteSection(section:string; strings:TStrings);
    {$ENDIF}
    procedure ReadPreferences;

    function  ReadArray(section: string): TJsonArray;
    procedure WriteArray(section: string; arr: TJsonArray);

    property ViewUntrackedFiles: boolean read fViewUntrackedFiles write SetViewUntrackedFiles;
    property ViewIgnoredFiles: boolean read fViewIgnoredFiles write SetViewIgnoredFiles;
    property ViewTrackedFiles: boolean read fViewTrackedFiles write SetViewTrackedFiles;
    property ShowTags: boolean read fShowTags write SetShowTags;
  end;

var
  fConfig: TConfig;

implementation


procedure JSonToFile(Obj: TJSONData; aFilename: string; Opt: TFormatOptions = DefaultFormat);
var
  l: TStringList;
begin
  l := TStringList.Create;
  l.Text := Obj.FormatJSON(Opt);
  l.SaveToFile(aFilename);
  l.Free;
end;

function JsonFromFile(aFilename: string): TJsonObject;
var
  stream: TFileStream;
  obj: TJsonData;
begin
  if not FileExists(aFilename) then
    result := TJsonObject.Create
  else begin
    stream := TFileStream.Create(aFilename, fmOpenRead);
    try
      obj := GetJSON(stream);
      if obj is TJsonObject then
        result := TJsonObject(obj)
      else begin
        result := TJsonObject.Create;
        result.Add(ExtractFileName(aFilename), obj);
      end;
    finally
      stream.free;
    end;
  end;
end;

{ TConfig }

procedure TConfig.CheckConfigFile;
begin
  if fConfigFile='' then begin
    fConfigFile := GetAppConfigFile(false, true);
    ForceDirectories(ExtractFilePath(fConfigFile));
  end;
end;

procedure TConfig.SetShowTags(AValue: boolean);
begin
  if fShowTags = AValue then Exit;
  fShowTags := AValue;
  WriteBoolean('ShowTags', fShowTags);
end;

procedure TConfig.SetViewIgnoredFiles(AValue: boolean);
begin
  if fViewIgnoredFiles = AValue then Exit;
  fViewIgnoredFiles := AValue;
  WriteBoolean('ViewIgnored', fViewIgnoredFiles);
end;

procedure TConfig.SetViewTrackedFiles(AValue: boolean);
begin
  if fViewTrackedFiles = AValue then Exit;
  fViewTrackedFiles := AValue;
  WriteBoolean('ViewTracked', fViewTrackedFiles);
end;

procedure TConfig.SetViewUntrackedFiles(AValue: boolean);
begin
  if fViewUntrackedFiles = AValue then Exit;
  fViewUntrackedFiles := AValue;
  WriteBoolean('ViewUntracked', fViewUntrackedFiles);
end;

function TConfig.GetLastParent(var aKey: string): TJsonObject;
var
  i: Integer;
  arr: TStringArray;
  obj: TJSONObject;
begin
  arr := aKey.Split('.');

  result := fJsonFile;
  for i := 0 to Length(arr)-2 do begin
    obj := result.Get(arr[i], TJsonObject(nil));
    if obj=nil then begin
      obj := TJsonObject.Create;
      result.Objects[arr[i]] := obj;
    end;
    result := obj;
  end;

  if length(arr)>1 then
    aKey := arr[Length(arr)-1];
end;

function TConfig.GetObject(section: string; var aKey: string): TJsonObject;
var
  p: SizeInt;
  s: string;
  obj: TJsonObject;
begin
  if (Section<>SECTION_GEOMETRY) and (Section<>SECTION_FONTS) then begin
    result := fJsonFile.Get(section, TJsonObject(nil));
    if result=nil then begin
      result := TJsonObject.Create;
      fJsonFile.Objects[section] := result;
    end;
  end else begin
    result := fJsonFile;
    p := pos('.', aKey);
    while p>0 do begin
      s := copy(aKey, 1, p-1);
      obj := result.Get(s, TJsonObject(nil));
      if obj=nil then begin
        obj := TJsonObject.Create;
        result.Objects[s] := obj;
      end;
      result := obj;
      delete(aKey, 1, p);
      p := pos('.', aKey);
    end;
  end;

end;

function TConfig.GetArray(aKey: string): TJsonArray;
var
  obj: TJSONObject;
begin
  obj := GetLastParent(aKey);
  if obj<>nil then
    result := obj.Get(aKey, TJsonArray(nil))
  else
    result := nil;
end;

function TConfig.GetObject(aKey: string): TJsonObject;
begin
  result := GetLastParent(aKey);
  if result<>nil then
    result := result.Get(aKey, TJsonObject(nil));
end;

procedure TConfig.OpenConfig;
begin
  if fConfigFileOpenCount=0 then begin
    CheckConfigFile;
    {$IFDEF UseINI}
    fIniFile := TIniFile.Create(fConfigFile);
    {$ENDIF}
    fJsonFile := JsonFromFile(fConfigFile+'.json');
  end;
  inc(fConfigFileOpenCount);
end;

procedure TConfig.CloseConfig;
begin
  dec(fConfigFileOpenCount);
  if fConfigFileOpenCount<=0 then begin
    {$IFDEF UseINI}
    if fIniFIle<>nil then
      FreeAndNil(fIniFile);
    {$ENDIF}
    JSonToFile(fJsonFile, fConfigFile+'.json');
    FreeAndNil(fJsonFile);
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

function TConfig.MenuMRE(aMRE: TComponent; save: boolean;
  onclick: TNotifyEvent; aCaption: string; section: string): TMenuItem;
var
  i, n: Integer;
  menu: TMenuItem;
  arr: TJSONArray;

  procedure NewMRE(newCaption:string);
  begin
    result := TMenuItem.Create(aMRE.Owner);
    result.Caption := newCaption;
    result.OnClick := onclick;
    menu.Insert(0, result);
  end;

begin

  if save and (aCaption='') then
    exit(nil);

  if (not (aMRE is TMenuItem)) and (not (aMRE is TMenu)) then
    raise Exception.Create('aMRE must be either a T[Pop]Menu or a TMenuItem');

  if aMRE is TMenu then
    menu := TMenu(aMRE).Items
  else
    menu := TMenuItem(aMRE);

  OpenConfig;
  if Save then begin

    i := menu.IndexOfCaption(aCaption);
    if i<0 then
      NewMRE(aCaption)
    else
      result := menu.Items[i];

    result.MenuIndex := 0;

    {$IFDEF UseINI}
    WriteInteger('MRECount', menu.Count, section);
    for i := 1 to menu.Count do
      WriteString(IntToStr(i), menu.Items[i - 1].Caption, section);
    {$ENDIF}

    arr := TJsonArray.Create;
    for i:=1 to menu.Count do
      arr.Add(menu.Items[i-1].Caption);

    WriteArray(section, arr);

  end else begin

    menu.Clear;
    {$IFDEF UseINI}
    n := ReadInteger('MRECount', 0, section);
    for i:=n downto 1 do
      NewMRE(ReadString(IntToStr(i), '' , section));
    {$ELSE}
    arr := ReadArray(section);
    if arr<>nil then
      for i:=0 to arr.Count-1 do
        NewMRE(arr.Strings[i]);
    {$ENDIF}

  end;
  CloseConfig;
end;

function TConfig.ReadString(aKey: string; default: string; section: string
  ): string;
var
  obj: TJSONObject;
begin
  OpenConfig;
  obj := GetObject(section, aKey);
  {$IFDEF UseINI}
  result := fIniFile.ReadString(section, aKey, default);
  //if result<>default then
    obj.Strings[aKey] := result;
  {$ENDIF}
  result := obj.Get(aKey, default);
  CloseConfig;
end;

function TConfig.ReadBoolean(aKey: string; default: boolean; section: string
  ): boolean;
var
  obj: TJSONObject;
begin
  OpenConfig;
  obj := GetObject(section, aKey);
  {$IFDEF UseINI}
  result := fIniFile.ReadBool(section, aKey, default);
  //if result<>default then
    obj.Booleans[aKey] := result;
  {$ENDIF}
  result := obj.Get(aKey, default);
  CloseConfig;
end;

function TConfig.ReadInteger(aKey: string; default: Integer; section: string
  ): Integer;
var
  obj: TJSONObject;
begin
  OpenConfig;
  obj := GetObject(section, aKey);
  {$IFDEF UseINI}
  result := fIniFile.ReadInteger(Section, aKey, default);
  //if result<>default then
    obj.Integers[aKey] := result;
  {$ENDIF}
  result := obj.Get(aKey, default);
  CloseConfig;
end;

procedure TConfig.WriteString(aKey: string; avalue: string; section: string);
var
  obj: TJSONObject;
begin
  OpenConfig;
  obj := GetObject(section, aKey);
  {$IFDEF UseINI}
  fIniFile.WriteString(section, aKey, aValue);
  {$ENDIF}
  obj.Strings[aKey] := aValue;
  CloseConfig;
end;

procedure TConfig.WriteBoolean(aKey: string; avalue: boolean; section: string);
var
  obj: TJSONObject;
begin
  OpenConfig;
  obj := GetObject(section, aKey);
  {$IFDEF UseINI}
  fIniFile.WriteBool(Section, aKey, aValue);
  {$ENDIF}
  obj.Booleans[aKey] := aValue;
  CloseConfig;
end;

procedure TConfig.WriteInteger(aKey: string; avalue: Integer; section: string);
var
  obj: TJSONObject;
begin
  OpenConfig;
  obj := GetObject(section, aKey);
  {$IFDEF UseINI}
  fIniFile.WriteInteger(Section, aKey, aValue);
  {$ENDIF}
  obj.Integers[aKey] := aValue;
  CloseConfig;
end;

{$IFDEF UseINI}
procedure TConfig.ReadSection(section: string; strings: TStrings);
begin
  OpenConfig;
  fIniFile.ReadSectionRaw(section, strings);
  CloseConfig;
end;

procedure TConfig.WriteSection(section: string; strings: TStrings);
var
  i, p: Integer;
  s, key, value: String;
begin
  OpenConfig;
  if fIniFile.SectionExists(section) then
    fIniFile.EraseSection(section);
  if (strings<>nil) then begin
    for i:=0 to strings.count-1 do begin
      s := strings[i];
      p := pos('=', s);
      if p=0 then begin
        key := IntToStr(i+1);
        value := s;
      end else begin
        key := trim(copy(s, 1, p-1));
        value := trim(copy(s, p+1, MAXINT));
      end;
      fIniFile.WriteString(section, key, value);
    end;
  end;
  CloseConfig;
end;
{$ENDIF}

procedure TConfig.ReadPreferences;
begin
  fViewUntrackedFiles := ReadBoolean('ViewUntracked', true);
  fViewIgnoredFiles := ReadBoolean('ViewIgnored', false);
  fViewTrackedFiles := ReadBoolean('ViewTracked', false);
  fShowTags := ReadBoolean('ShowTags', true);
end;

function TConfig.ReadArray(section: string): TJsonArray;
begin
  result := GetArray(section);
end;

procedure TConfig.WriteArray(section: string; arr: TJsonArray);
var
  obj: TJSONObject;
begin
  obj := GetLastParent(section);
  obj.Arrays[section] := arr;
end;

end.

