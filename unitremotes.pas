unit unitremotes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel, Buttons,
  unitcommon, unitifaces, unitgittypes, unitconfig, unitgitmgr;

type

  { TfrmRemotes }

  TfrmRemotes = class(TForm)
    chkSameURL: TCheckBox;
    btnAdd: TSpeedButton;
    btnDel: TSpeedButton;
    lblInfo: TLabel;
    txtFetch: TEdit;
    txtPush: TEdit;
    panBtns: TButtonPanel;
    lstRemotes: TListBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure chkSameURLClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstRemotesSelectionChange(Sender: TObject; User: boolean);
    procedure txtFetchChange(Sender: TObject);
  private
    fGitMgr: TGitMgr;
    fReadOnly: boolean;
    fRemotes: TRemotesArray;
    procedure SetGitMgr(AValue: TGitMgr);
    procedure SetReadOnly(AValue: boolean);
    procedure UpdateRemotesList;
    procedure CheckSameURL;
  public
    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
    property ReadOnly: boolean read fReadOnly write SetReadOnly;
  end;

var
  frmRemotes: TfrmRemotes;

implementation

{$R *.lfm}

{ TfrmRemotes }

procedure TfrmRemotes.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fConfig.ReadWindow(self, 'remotesfrm', SECTION_GEOMETRY);
end;

procedure TfrmRemotes.chkSameURLClick(Sender: TObject);
begin
  CheckSameUrl;
end;

procedure TfrmRemotes.btnAddClick(Sender: TObject);
begin
  //
end;

procedure TfrmRemotes.btnDelClick(Sender: TObject);
begin
  //
end;

procedure TfrmRemotes.FormCreate(Sender: TObject);
begin
  fConfig.WriteWindow(Self, 'remotesfrm', SECTION_GEOMETRY);
  ReadOnly := true;
end;

procedure TfrmRemotes.FormShow(Sender: TObject);
begin
  UpdateRemotesList;
end;

procedure TfrmRemotes.lstRemotesSelectionChange(Sender: TObject; User: boolean);
var
  i: Integer;
begin
  i := lstRemotes.ItemIndex;
  if i>=0 then begin
    with fRemotes[i] do begin
      txtFetch.Text := fetch;
      txtPush.Text := push;
      chkSameURL.Checked := fetch=push;
    end;
    btnDel.Enabled := not ReadOnly;
    CheckSameURL;
  end else begin
    btnDel.Enabled := false;
  end;
end;

procedure TfrmRemotes.txtFetchChange(Sender: TObject);
begin
  if chkSameURL.Checked then
    txtPush.Text := txtFetch.Text;
end;

procedure TfrmRemotes.SetGitMgr(AValue: TGitMgr);
var
  i: Integer;
begin
  if fGitMgr = AValue then Exit;

  fGitMgr := AValue;

  if fGitMgr<>nil then begin
    SetLength(fRemotes, Length(fGitMgr.Remotes));
    for i:=0 to Length(fRemotes)-1 do
      fRemotes[i] := fGitMgr.Remotes[i];
  end else
    fRemotes := nil;

  UpdateRemotesList;
end;

procedure TfrmRemotes.SetReadOnly(AValue: boolean);
begin
  if fReadOnly = AValue then Exit;
  fReadOnly := AValue;
  btnAdd.Visible := not fReadOnly;
  btnDel.Visible := not fReadOnly;
  txtFetch.Enabled := not fReadOnly;
  txtPush.Enabled := not fReadOnly;
  chkSameURL.Enabled := not fReadOnly;
end;

procedure TfrmRemotes.UpdateRemotesList;
var
  i: Integer;
begin
  lstRemotes.Clear;
  for i:=0 to Length(fRemotes)-1 do
    lstRemotes.AddItem(fRemotes[i].name, nil);
  if lstRemotes.Count>0 then
    lstRemotes.ItemIndex := 0;
end;

procedure TfrmRemotes.CheckSameURL;
begin
  txtPush.Enabled := not chkSameURL.Checked;
end;

end.

