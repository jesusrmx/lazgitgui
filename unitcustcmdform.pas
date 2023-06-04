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
  if not fEventsDisabled then begin
    GuiToCommand;
    Changed;
  end;
end;

procedure TfrmCustomCommands.btnAddClick(Sender: TObject);
begin
  NewCommand;
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
  CheckControls;
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
  fNewCommands[fCurrentItem] := cmd;
end;

procedure TfrmCustomCommands.UpdateCurrentItem;
begin
  fEventsDisabled := true;
  txtDescription.Text := fNewCommands[fCurrentItem].description;
  txtCommand.Text := fNewCommands[fCurrentItem].command;
  chkInDialog.Checked := fNewCommands[fCurrentItem].RunInDlg;
  txtImage.Text := fNewCommands[fCurrentItem].image;
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
  imgCmd.Enabled := txtDescription.Enabled;
end;

end.

