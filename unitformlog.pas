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

  Shows the git Log, tree, changes and file history.
}
unit unitformlog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  unitifaces, unitconfig, unithighlighterhelper, unitgitmgr, unitframelog;

type

  { TfrmLog }

  TfrmLog = class(TForm)
    fLog: TframeLog;
    StatusBar1: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fGitMgr: TGitMgr;
    fhlHelper: THighlighterHelper;
    procedure SetGitMgr(AValue: TGitMgr);
    procedure SetHlHelper(AValue: THighlighterHelper);

  public
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property HlHelper: THighlighterHelper read fhlHelper write SetHlHelper;
  end;

var
  frmLog: TfrmLog = nil;

implementation

{$R *.lfm}

{ TfrmLog }

procedure TfrmLog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmLog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fConfig.OpenConfig;
  fConfig.WriteWindow(Self, 'frmlog', SECTION_GEOMETRY);
  fConfig.WriteInteger('frmlog.panbrowser.height', flog.panBrowser.Height, SECTION_GEOMETRY);
  fConfig.WriteInteger('frmlog.panfiles.width', flog.panFiles.Width, SECTION_GEOMETRY);
  fConfig.CloseConfig;
end;

procedure TfrmLog.FormCreate(Sender: TObject);
begin
  fConfig.OpenConfig;
  fLog.Config := fConfig;
  fConfig.ReadWindow(Self, 'frmlog', SECTION_GEOMETRY);
  fLog.panBrowser.Height := fConfig.ReadInteger('frmlog.panbrowser.height', fLog.panBrowser.Height, SECTION_GEOMETRY);
  fLog.panFiles.Width := fConfig.ReadInteger('frmlog.panfiles.width', fLog.panFiles.width, SECTION_GEOMETRY);
  fConfig.CloseConfig;
end;

procedure TfrmLog.FormDestroy(Sender: TObject);
begin
  fLog.Clear;
  GitMgr := nil;
end;

procedure TfrmLog.FormShow(Sender: TObject);
begin
  fLog.Active := true;
end;

procedure TfrmLog.SetGitMgr(AValue: TGitMgr);
begin
  if fGitMgr = AValue then Exit;
  fGitMgr := AValue;

  fLog.GitMgr := fGitMgr;
end;

procedure TfrmLog.SetHlHelper(AValue: THighlighterHelper);
begin
  if fhlHelper = AValue then Exit;
  fhlHelper := AValue;

  fLog.HlHelper := fhlHelper;
end;

end.
