unit unitcustcmdform;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  Buttons, ExtCtrls, EditBtn, unitgittypes, unitconfig, unitcustomcmds;

type

  { TfrmCustomCommands }

  TfrmCustomCommands = class(TForm)
    bPanel: TButtonPanel;
    chkInDialog: TCheckBox;
    btnAdd: TSpeedButton;
    btnDel: TSpeedButton;
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
    procedure chkInDialogClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbCommandsSelectionChange(Sender: TObject; User: boolean);
    procedure txtDescriptionKeyPress(Sender: TObject; var Key: char);
    procedure txtImageAcceptFileName(Sender: TObject; var Value: String);
    procedure txtImageEditingDone(Sender: TObject);
  private
    fAddNew: boolean;
    fCommands, fNewCommands: TCustomCommandsMgr;
    fCurrentItem: Integer;
    procedure SetCommands(AValue: TCustomCommandsMgr);
    procedure NewCommand;
    procedure FillList;
    procedure Changed;
    procedure GuiToCommand;
    procedure UpdateCurrentItem;
    procedure UpdateCommandImage;
    procedure CheckControls;
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
end;

procedure TfrmCustomCommands.chkInDialogClick(Sender: TObject);
begin
  GuiToCommand;
  Changed;
end;

procedure TfrmCustomCommands.btnAddClick(Sender: TObject);
begin
  NewCommand;
end;

procedure TfrmCustomCommands.btnDelClick(Sender: TObject);
begin
  if fCurrentItem>=0 then begin
    fNewCommands.Delete(fCurrentItem);
    if fNewCommands.Count=0 then
      fCurrentItem := -1;
    FillList;
    Changed;
  end;
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
  fCurrentItem := lbCommands.ItemIndex;
  UpdateCurrentItem;
end;

procedure TfrmCustomCommands.txtDescriptionKeyPress(Sender: TObject;
  var Key: char);
begin
  Changed;
end;

procedure TfrmCustomCommands.txtImageAcceptFileName(Sender: TObject;
  var Value: String);
begin
  GuiToCommand;
  UpdateCommandImage;
  Changed;
end;

procedure TfrmCustomCommands.txtImageEditingDone(Sender: TObject);
begin
  GuiToCommand;
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
begin
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
  fNewCommands[fCurrentItem] := cmd;
end;

procedure TfrmCustomCommands.UpdateCurrentItem;
begin
  txtDescription.Text := fNewCommands[fCurrentItem].description;
  txtCommand.Text := fNewCommands[fCurrentItem].command;
  chkInDialog.Checked := fNewCommands[fCurrentItem].RunInDlg;
  txtImage.Text := fNewCommands[fCurrentItem].image;
  UpdateCommandImage;
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
  imgCmd.Enabled := txtDescription.Enabled;
end;

end.

