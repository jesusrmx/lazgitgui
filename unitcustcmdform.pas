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

  Custom commands editor.
}
unit unitcustcmdform;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  Buttons, ExtCtrls, EditBtn, unitgittypes, unitconfig, unitcustomcmds;

type

  { TfrmCustomCommands }

  TfrmCustomCommands = class(TForm)
    bPanel: TButtonPanel;
    chkStatus: TCheckBox;
    chkAsk: TCheckBox;
    chkInDialog: TCheckBox;
    btnAdd: TSpeedButton;
    btnDel: TSpeedButton;
    lblInfo: TLabel;
    txtImage: TFileNameEdit;
    GroupBox1: TGroupBox;
    imgCmd: TImage;
    txtDescription: TEdit;
    txtCommand: TEdit;
    lbCommands: TListBox;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure chkInDialogClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbCommandsSelectionChange(Sender: TObject; User: boolean);
    procedure txtDescriptionChange(Sender: TObject);
    procedure txtImageAcceptFileName(Sender: TObject; var Value: String);
  private
    fAddNew: boolean;
    fCommands, fNewCommands: TCustomCommandsMgr;
    fEventsDisabled: Boolean;
    fCurrentItem: Integer;
    procedure SetCommands(AValue: TCustomCommandsMgr);
    procedure NewCommand;
    procedure FillList;
    procedure Changed;
    procedure GuiToCommand;
    procedure CommandToGui;
    procedure UpdateCommandImage;
    procedure CheckControls;
    procedure ExchangeCommands(dst: Integer);
  public
    property Commands: TCustomCommandsMgr read fCommands write SetCommands;
    property AddNew: boolean write fAddNew;
  end;

var
  frmCustomCommands: TfrmCustomCommands;

implementation

{$R *.lfm}

{ TfrmCustomCommands }

procedure TfrmCustomCommands.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  fConfig.WriteWindow(Self, 'customcmdform', SECTION_GEOMETRY);
  if ModalResult=mrOk then begin
    fCommands.Assign(fNewCommands);
    fCommands.SaveToConfig;
  end;
end;

procedure TfrmCustomCommands.chkInDialogClick(Sender: TObject);
begin
  if not fEventsDisabled then begin
    GuiToCommand;
    Changed;
  end;
end;

procedure TfrmCustomCommands.btnAddClick(Sender: TObject);
begin
  NewCommand;
  Self.ActiveControl := txtDescription;
end;

procedure TfrmCustomCommands.btnDelClick(Sender: TObject);
begin
  if fCurrentItem>=0 then begin
    fNewCommands.Delete(fCurrentItem);
    fCurrentItem := min(fNewCommands.Count-1, fCurrentItem);
    FillList;
    Changed;
  end;
end;

procedure TfrmCustomCommands.btnDownClick(Sender: TObject);
begin
  ExchangeCommands(fCurrentItem + 1);
end;

procedure TfrmCustomCommands.btnUpClick(Sender: TObject);
begin
  ExchangeCommands(fCurrentItem - 1);
end;

procedure TfrmCustomCommands.FormCreate(Sender: TObject);
begin
  fConfig.ReadWindow(Self, 'customcmdform', SECTION_GEOMETRY);
  fNewCommands := TCustomCommandsMgr.Create;
end;

procedure TfrmCustomCommands.FormDestroy(Sender: TObject);
begin
  fNewCommands.Free;
end;

procedure TfrmCustomCommands.FormShow(Sender: TObject);
begin
  if fAddNew then
    NewCommand
  else
    FillList;
end;

procedure TfrmCustomCommands.lbCommandsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if not fEventsDisabled then begin
    if lbCommands.ItemIndex>=0 then begin
      fCurrentItem := lbCommands.ItemIndex;
      CommandToGui;
      CheckControls;
    end;
  end;
end;

procedure TfrmCustomCommands.txtDescriptionChange(Sender: TObject);
begin
  if not fEventsDisabled then begin
    GuiToCommand;
    if Sender=txtDescription then
      lbCommands.Items[fCurrentItem] := txtDescription.Text;
    Changed;
  end;
end;

procedure TfrmCustomCommands.txtImageAcceptFileName(Sender: TObject;
  var Value: String);
var
  cmd: TCustomCmdItem;
begin
  cmd := fNewCommands[fCurrentItem];
  cmd.image := value;
  fNewCommands[fCurrentItem] := cmd;
  UpdateCommandImage;
  Changed;
end;

procedure TfrmCustomCommands.SetCommands(AValue: TCustomCommandsMgr);
begin
  if fCommands = AValue then Exit;
  fCommands := AValue;
  fNewCommands.Assign(fCommands);
end;

procedure TfrmCustomCommands.NewCommand;
begin
  fCurrentItem := fNewCommands.Add;
  FillList;
end;

procedure TfrmCustomCommands.FillList;
var
  i: Integer;
begin
  lbCommands.Clear;
  for i:=0 to fNewCommands.Count-1 do
    lbCommands.Items.Add(fNewCommands[i].description);
  lbCommands.ItemIndex := fCurrentItem;

  CheckControls;
end;

procedure TfrmCustomCommands.Changed;
  procedure Error(i: Integer; msg:string);
  begin
    lblInfo.Caption := format('command %d: %s',[i+1, msg]);
  end;

var
  i: Integer;
  cmd: TCustomCmdItem;
begin
  bPanel.OKButton.Enabled := false;
  for i:=0 to fNewCommands.Count-1 do begin
    cmd := fNewCommands[i];
    if cmd.description='' then begin
      Error(i, 'Description is empty');
      exit;
    end;
    if cmd.description=NEWCOMMAND_DESC then begin
      Error(i, 'Invalid description');
      exit;
    end;
    if pos('git ', cmd.command)<>1 then begin
      Error(i, 'Command must start with ''git ''');
      exit;
    end;
    if cmd.command='git ' then begin
      Error(i, 'Incomplete command');
      exit;
    end;

  end;
  lblInfo.Caption := '';
  bPanel.OKButton.Enabled := true;
end;

procedure TfrmCustomCommands.GuiToCommand;
var
  cmd: TCustomCmdItem;
begin
  cmd.description := txtDescription.Text;
  cmd.command := txtCommand.Text;
  cmd.RunInDlg := chkInDialog.Checked;
  cmd.image := txtImage.Text;
  cmd.Ask := chkAsk.Checked;
  cmd.UpdateStatus := chkStatus.Checked;
  fNewCommands[fCurrentItem] := cmd;
end;

procedure TfrmCustomCommands.CommandToGui;
begin
  fEventsDisabled := true;
  txtDescription.Text := fNewCommands[fCurrentItem].description;
  txtCommand.Text := fNewCommands[fCurrentItem].command;
  chkInDialog.Checked := fNewCommands[fCurrentItem].RunInDlg;
  txtImage.Text := fNewCommands[fCurrentItem].image;
  chkAsk.Checked := fNewCommands[fCurrentItem].Ask;
  chkStatus.Checked := fNewCommands[fCurrentItem].UpdateStatus;
  UpdateCommandImage;
  fEventsDisabled := false;
end;

procedure TfrmCustomCommands.UpdateCommandImage;
var
  img: string;
begin
  img := fNewCommands[fCurrentItem].image;
  if (img='') or (not FileExists(img)) then
    imgCmd.Picture.Clear
  else
    imgCmd.Picture.LoadFromFile(img)
end;

procedure TfrmCustomCommands.CheckControls;
begin
  btnUp.Enabled := (fNewCommands.Count>1) and (fCurrentItem>0);
  btnDown.Enabled := (fNewCommands.Count>1) and (fCurrentItem<fNewCommands.Count-1);
  btnAdd.Enabled := true;
  btnDel.Enabled := (fNewCommands.Count>=1) and (fCurrentItem>=0);
  txtDescription.Enabled := fNewCommands.Count>0;
  txtImage.Enabled := txtDescription.Enabled;
  txtCommand.Enabled := txtDescription.Enabled;
  chkInDialog.Enabled := txtDescription.Enabled;
  chkAsk.Enabled := txtDescription.Enabled;
  chkStatus.Enabled := txtDescription.Enabled;
  imgCmd.Enabled := txtDescription.Enabled;
end;

procedure TfrmCustomCommands.ExchangeCommands(dst: Integer);
begin
  fNewCommands.Exchange(fCurrentItem, dst);
  lbCommands.Items.Exchange(fCurrentItem, dst);
  fCurrentItem := dst;
  fEventsDisabled := true;
  lbCommands.ItemIndex := fCurrentItem;
  fEventsDisabled := false;
  CheckControls;
  Changed;
end;

end.

