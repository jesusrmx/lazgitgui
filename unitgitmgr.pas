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

  Git manager.
  It creates the git instance and provides the git interface to several parts
  that need it. Starts operations and distribute results.
}
unit unitgitmgr;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unitgittypes, unitgit, unitifaces;

const
  GITMGR_EVENT_UPDATESTATUS         = 1;
  GITMGR_EVENT_REFLISTCHANGED       = 2;
  GITMGR_EVENT_NEWTAG               = 3;
  GITMGR_EVENT_SWITCHTOTAG          = 4;

type

  { TObserverMgr }

  TObserverMgr = class
  private
    fObservers: array of IObserver;
    function IndexOf(who: IObserver): Integer;
  public
    procedure AddObserver(who: IObserver);
    procedure RemoveObserver(who: IObserver);
    procedure NotifyObservers(sender: TObject; what: Integer; data: PtrInt);
  end;

  { TGitMgr }

  TGitMgr = class
  private
    fConfig: IConfig;
    fGit: TGit;
    fShowTags: boolean;
    fViewIgnoredFiles: boolean;
    fViewTrackedFiles: boolean;
    fViewUntrackedFiles: boolean;
    fDescribed: boolean;
    fLastDescribedTag: string;
    fUnstagedList, fStagedList: TStringList;
    fObserverMgr: TObserverMgr;
    function GetGit: IGit;
    procedure SetConfig(AValue: IConfig);
  public
    constructor create;
    destructor destroy; override;
    function Initialize: boolean;
    procedure UpdateStatus;
    procedure UpdateRefList;
    procedure AddObserver(who: IObserver);
    procedure RemoveObserver(who: IObserver);
    function IndexOfLocalBranch(aName: string): Integer;
    procedure QueueNewTag(commit: string);
    procedure QueueSwitchTag(tagName: string);
    procedure ForceTagDescription;

    property Git: IGit read GetGit;
    property Config: IConfig read fConfig write SetConfig;
    property ShowTags: boolean read fShowTags write fShowTags;
    property ViewIgnoredFiles: boolean read fViewIgnoredFiles write fViewIgnoredFiles;
    property ViewUntrackedFiles: boolean read fViewUntrackedFiles write fViewUntrackedFiles;
    property ViewTrackedFiles: boolean read fViewTrackedFiles write fViewTrackedFiles;
    property UnstagedList: TStringList read fUnstagedList;
    property StagedList: TStringList read fStagedList;
  end;

implementation

{ TObserverMgr }

function TObserverMgr.IndexOf(who: IObserver): Integer;
var
  i: Integer;
begin
  for i:=0 to Length(fObservers)-1 do
    if fObservers[i]=who then
      exit(i);
  result := -1;
end;

procedure TObserverMgr.AddObserver(who: IObserver);
var
  i: SizeInt;
begin
  i := IndexOf(who);
  if i<0 then begin
    i := Length(fObservers);
    SetLength(fObservers, i+1);
    fObservers[i] := who
  end;
end;

procedure TObserverMgr.RemoveObserver(who: IObserver);
var
  i: Integer;
begin
  i := IndexOf(who);
  if i>=0 then
    Delete(fObservers, i, 1);
end;

procedure TObserverMgr.NotifyObservers(sender: TObject; what: Integer;
  data: PtrInt);
var
  who: IObserver;
begin
  for who in fObservers do
    who.ObservedChanged(sender, what, data);
end;

{ TGitMgr }

procedure TGitMgr.SetConfig(AValue: IConfig);
begin
  if fConfig = AValue then Exit;
  fConfig := AValue;
  fGit.Config := fConfig;
end;

function TGitMgr.GetGit: IGit;
begin
  result := fGit;
end;

constructor TGitMgr.create;
begin
  inherited Create;
  fGit := TGit.Create;
  fUnstagedList := TStringList.Create;
  fStagedList := TStringList.Create;
  fObserverMgr := TObserverMgr.Create;
end;

destructor TGitMgr.destroy;
begin
  fObserverMgr.Free;
  fStagedList.Free;
  fUnstagedList.Free;
  fGit.Free;
  inherited destroy;
end;

function TGitMgr.Initialize: boolean;
begin
  result := fGit.Initialize;
end;

procedure TGitMgr.UpdateStatus;
var
  cmdout: RawByteString;
  res: Integer;
begin
  // get the more recent tag
  if fShowTags and (not fDescribed) then begin
    fGit.Describe('', cmdout);
    fLastDescribedTag := cmdOut;
    fDescribed := true;
  end;

  if fViewIgnoredFiles then fGit.IgnoredMode:='traditional' else fGit.IgnoredMode:='no';
  if fViewUntrackedFiles then fGit.UntrackedMode:='all' else fGit.UntrackedMode:='no';

  res := fGit.Status(fUnstagedList, fStagedList);

  fObserverMgr.NotifyObservers(self, GITMGR_EVENT_UPDATESTATUS, res);
end;

procedure TGitMgr.UpdateRefList;
var
  res: Integer;
begin
  res := fGit.UpdateRefList;
  fObserverMgr.NotifyObservers(Self, GITMGR_EVENT_REFLISTCHANGED, res);
end;

procedure TGitMgr.AddObserver(who: IObserver);
begin
  fObserverMgr.AddObserver(who);
end;

procedure TGitMgr.RemoveObserver(who: IObserver);
begin
  fObserverMgr.RemoveObserver(who);
end;

function TGitMgr.IndexOfLocalBranch(aName: string): Integer;
var
  i: Integer;
  info: PRefInfo;
begin
  result := -1;
  aName := lowercase(aName);
  for i:=0 to fGit.RefList.Count-1 do begin
    info := PRefInfo(fGit.RefList.Objects[i]);
    if (info^.subType=rostLocal) and (aName=info^.refName) then begin
      result := i;
      break;
    end;
  end;
end;

procedure TGitMgr.QueueNewTag(commit: string);
var
  info: PTagInfo;
begin
  new(info);
  info^.data := commit;
  fObserverMgr.NotifyObservers(Self, GITMGR_EVENT_NEWTAG, PtrInt(info));
end;

procedure TGitMgr.QueueSwitchTag(tagName: string);
var
  info: PTagInfo;
begin
  new(info);
  info^.data := tagName;
  fObserverMgr.NotifyObservers(Self, GITMGR_EVENT_SWITCHTOTAG, PtrInt(info));
end;

procedure TGitMgr.ForceTagDescription;
begin
  fDescribed := false;
end;

end.

