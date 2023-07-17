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

  Custom Commands manager.

}
unit unitcustomcmds;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson,
  LazLoggerBase,
  unitcommon, unitgitutils, unitconfig;

const
  NEWCOMMAND_DESC = '<New Command>';

type

  TCustomCmdItem = record
    description: string;
    command: string;
    RunInDlg: boolean;
    image: string;
    Ask: boolean;
    UpdateStatus: boolean;
  end;

  { TCustomCommandsMgr }

  TCustomCommandsMgr = class
  private
    function GetCommand(aIndex: Integer): TCustomCmdItem;
    function GetCount: Integer;
    procedure SetCommand(aIndex: Integer; AValue: TCustomCmdItem);
  public
    commands: array of TCustomCmdItem;
    destructor destroy; override;
    procedure clear;
    procedure LoadFromConfig;
    procedure SaveToConfig;
    procedure Assign(mgr: TCustomCommandsMgr);
    function  Add(descr:string=NEWCOMMAND_DESC; cmd:string='';
                  inDlg:boolean=false; img:string=''; ask:boolean=true;
                  upd:boolean=true): Integer;
    procedure Delete(aIndex: Integer);
    procedure Exchange(a, b: Integer);

    property Count: Integer read GetCount;
    property Command[aIndex:Integer]: TCustomCmdItem read GetCommand write SetCommand; default;
  end;

implementation

{ TCustomCommandsMgr }

function TCustomCommandsMgr.GetCommand(aIndex: Integer): TCustomCmdItem;
begin
  result := commands[aIndex];
end;

function TCustomCommandsMgr.GetCount: Integer;
begin
  result := Length(commands);
end;

procedure TCustomCommandsMgr.SetCommand(aIndex: Integer; AValue: TCustomCmdItem
  );
begin
  commands[aIndex] := AValue;
end;

destructor TCustomCommandsMgr.destroy;
begin
  clear;
  inherited destroy;
end;

procedure TCustomCommandsMgr.clear;
begin
  commands := nil;
end;

procedure TCustomCommandsMgr.LoadFromConfig;
var
  i: Integer;
  List, L: TStringList;
  arr: TJSONArray;
  item: TJSONEnum;
  obj: TJSONObject;
  cmd: TCustomCmdItem;
begin
  List := TStringList.Create;
  L := TStringList.Create;
  fConfig.OpenConfig;
  try
    arr := fConfig.ReadArray('CustomCommands');
    if arr=nil then
      commands := nil
    else begin
      SetLength(commands, arr.Count);
      for item in arr do begin
        obj := TJsonObject(item.Value);
        i := item.KeyNum;
        commands[i].description   := obj.Get('Description', '');
        commands[i].command       := obj.Get('Command', '');
        commands[i].RunInDlg      := obj.Get('RunInDlg', true);
        commands[i].Image         := obj.Get('Image', '');
        commands[i].Ask           := obj.Get('Ask', true);
        commands[i].UpdateStatus  := obj.Get('UpdateStatus', true);
      end;
    end;

    fConfig.ReadSection('CustomCommands', List);

    SetLength(commands, List.Count);

    for i:=0 to List.Count-1 do begin
      DecodeDelimitedText( List.Values[IntToStr(i+1)], CMDSEP, L);
      if L.Count>=4 then begin
        commands[i].description := L[0];
        commands[i].command := L[1];
        commands[i].RunInDlg := L[2]='1';
        commands[i].image := L[3];
      end;
      if L.Count>=5 then
        commands[i].Ask := L[4]='1'
      else
        commands[i].Ask := true;
      if L.Count>=6 then
        commands[i].UpdateStatus:= L[5]='1'
      else
        commands[i].UpdateStatus := true;
    end;

    if (arr=nil) and (Length(commands)>0) then
      SaveToConfig;

  finally
    fConfig.CloseConfig;
    L.Free;
    List.Free;
  end;
end;

procedure TCustomCommandsMgr.SaveToConfig;
var
  i: Integer;
  L, List: TStringList;
  arr: TJsonArray;
  obj: TJSONObject;
begin
  fConfig.OpenConfig;
  List := TStringList.Create;
  L := TStringList.Create;
  try
    for i:=1 to 6 do L.Add('');
    for i:=0 to Count-1 do begin
      L[0] := commands[i].description;
      L[1] := commands[i].command;
      L[2] := BoolToStr(commands[i].RunInDlg, '1', '0');
      L[3] := commands[i].image;
      L[4] := BoolToStr(commands[i].Ask, '1', '0');
      L[5] := BoolToStr(commands[i].UpdateStatus, '1', '0');
      List.Add(IntToStr(i+1)+'='+EncodeDelimitedText(CMDSEP, L));
    end;
    fConfig.WriteSection('CustomCommands', List);

    arr := TJsonArray.Create;
    for i:=0 to Count-1 do begin
      obj := TJsonObject.Create;
      obj.Add('Description', commands[i].description);
      obj.Add('Command', commands[i].command);
      obj.Add('RunInDlg', commands[i].RunInDlg);
      obj.Add('Image', commands[i].image);
      obj.Add('Ask', commands[i].Ask);
      obj.Add('UpdateStatus', commands[i].UpdateStatus);
      arr.Add(obj);
    end;
    fConfig.WriteArray('CustomCommands', arr);

  finally
    L.Free;
    fConfig.CloseConfig;
  end;
end;

procedure TCustomCommandsMgr.Assign(mgr: TCustomCommandsMgr);
var
  i: Integer;
begin
  SetLength(commands, mgr.count);
  for i:=0 to mgr.Count-1 do
    commands[i] := mgr.commands[i];
end;

function TCustomCommandsMgr.Add(descr: string; cmd: string; inDlg: boolean;
  img: string; ask: boolean; upd: boolean): Integer;
begin
  result := Count;
  SetLength(commands, result + 1);
  commands[result].description := descr;
  commands[result].command := cmd;
  commands[result].RunInDlg := inDlg;
  commands[result].image := img;
  commands[result].Ask := ask;
  commands[result].UpdateStatus := upd;
end;

procedure TCustomCommandsMgr.Delete(aIndex: Integer);
begin
  if (aIndex>=0) and (aIndex<Count) then
    System.Delete(commands, aIndex, 1)
end;

procedure TCustomCommandsMgr.Exchange(a, b: Integer);
var
  cmd: TCustomCmdItem;
begin
  cmd := commands[a];
  commands[a] := commands[b];
  commands[b] := cmd;
end;

end.

