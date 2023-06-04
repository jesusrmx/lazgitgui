unit unitcustcmdform;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  Buttons, ExtCtrls, unitgittypes, unitconfig;

type

  { TfrmCustomCommands }

  TfrmCustomCommands = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkInDialog: TCheckBox;
    GroupBox1: TGroupBox;
    imgCmd: TImage;
    txtDescription: TEdit;
    txtCommand: TEdit;
    ListBox1: TListBox;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure NewCommand;
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

procedure TfrmCustomCommands.FormCreate(Sender: TObject);
begin
  fConfig.ReadWindow(Self, 'customcmdform', SECTION_GEOMETRY);
end;

procedure TfrmCustomCommands.NewCommand;
begin
end;

end.

