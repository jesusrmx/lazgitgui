unit unitremotes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LazLoggerBase,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel, Buttons,
  unitcommon, unitifaces, unitgittypes, unitconfig, unitgitmgr, Types;

type

  TRemotesEditAction = (reaNoChange, reaNew, reaDel, reaChange);
  TRemotesEditItem = record
    name, fetch, push: string;
    orgName, orgFetch, orgPush: string;
    action, oldAction: TRemotesEditAction;
  end;

  TRemotesEditArray = array of TRemotesEditItem;

  { TfrmRemotes }

  TfrmRemotes = class(TForm)
    chkSameURL: TCheckBox;
    btnAdd: TSpeedButton;
    btnDel: TSpeedButton;
    txtName: TEdit;
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
    procedure lstRemotesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lstRemotesSelectionChange(Sender: TObject; User: boolean);
    procedure txtFetchChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
  private
    fGitMgr: TGitMgr;
    fReadOnly: boolean;
    fRemotes: TRemotesEditArray;
    function GetIndex: Integer;
    procedure SetGitMgr(AValue: TGitMgr);
    procedure SetReadOnly(AValue: boolean);
    procedure UpdateRemotesList(updItemIndex:Boolean=true);
    procedure CheckSameURL;
    procedure UpdateInfo;
    procedure UpdateControls;
    procedure Invalid(msg: string);
  protected
    property CurrentIndex: Integer read GetIndex;
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
  UpdateControls;
end;

procedure TfrmRemotes.btnAddClick(Sender: TObject);
var
  j: SizeInt;
begin
  j := CurrentIndex;
  if (j>=0) and (fRemotes[j].action=reaDel) then begin
    // simply undo deletion. TODO: what was the previous state?
    fRemotes[j].action := fRemotes[j].oldAction;
    lstRemotes.Invalidate;
    UpdateControls;
    exit;
  end;

  j := Length(fRemotes);
  SetLength(fRemotes, j+1);
  fRemotes[j].name := rsNewRemoteName;
  fRemotes[j].action := reaNew;
  UpdateRemotesList(false);
  lstRemotes.ItemIndex := j;
end;

procedure TfrmRemotes.btnDelClick(Sender: TObject);
var
  i: Integer;
begin
  i := CurrentIndex;
  fRemotes[i].oldAction := fRemotes[i].action;
  fRemotes[i].action := reaDel;
  lstRemotes.Invalidate;
  UpdateControls;
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

procedure TfrmRemotes.lstRemotesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  lb: TListBox;
  aCanvas: TCanvas;
  isFocused, sel: Boolean;
  aColor: TColor;
begin
  if index<0 then
    exit;

  lb := TListBox(Control);

  isFocused := lb.Focused;
  sel := (odSelected in State) and isFocused;

  aCanvas := lb.Canvas;
  if lb.Focused then
    if sel then aColor := clHighlight
    else        aColor := lb.Color
  else          aColor := lb.Color;

  aCanvas.Brush.Color := aColor;
  aCanvas.FillRect(aRect);

  if fRemotes[index].action=reaDel then
    aColor := clGrayText
  else begin
    if isFocused then
      if sel then aColor := clHighlightText
      else        aColor := clBlack
    else          aColor := clBlack;
  end;

  lb.Canvas.Font.Color := aColor;
  lb.Canvas.Brush.Style := bsClear;
  lb.Canvas.TextRect(aRect, aRect.Left + 22, aRect.Top, lb.Items[index]);
end;

procedure TfrmRemotes.lstRemotesSelectionChange(Sender: TObject; User: boolean);
var
  i: Integer;
begin
  i := CurrentIndex;
  if i>=0 then begin
    with fRemotes[i] do begin
      txtName.Text := name;
      txtFetch.Text := fetch;
      txtPush.Text := push;
      chkSameURL.Checked := fetch=push;
      btnDel.Enabled := not ReadOnly and not (action=reaDel);
    end;
    UpdateControls;
  end else begin
    btnDel.Enabled := false;
  end;

  UpdateInfo;
end;

procedure TfrmRemotes.txtFetchChange(Sender: TObject);
begin
  if chkSameURL.Checked then
    txtPush.Text := txtFetch.Text;
end;

procedure TfrmRemotes.txtNameChange(Sender: TObject);
var
  i: Integer;
begin
  i := CurrentIndex;
  fRemotes[i].name := txtName.Text;
  lstRemotes.Items[i] := fRemotes[i].name;
end;

procedure TfrmRemotes.SetGitMgr(AValue: TGitMgr);
var
  i: Integer;
begin
  if fGitMgr = AValue then Exit;

  fGitMgr := AValue;

  if fGitMgr<>nil then begin
    SetLength(fRemotes, Length(fGitMgr.Remotes));
    for i:=0 to Length(fRemotes)-1 do begin
      fRemotes[i].name := fGitMgr.Remotes[i].name;
      fRemotes[i].fetch := fGitMgr.Remotes[i].fetch;
      fRemotes[i].push := fGitMgr.Remotes[i].push;
      fRemotes[i].orgName  := fRemotes[i].name;
      fRemotes[i].orgFetch := fRemotes[i].fetch;
      fRemotes[i].orgPush  := fRemotes[i].push;
      fRemotes[i].action := reaNoChange;
    end;
  end else
    fRemotes := nil;

  UpdateRemotesList;
end;

function TfrmRemotes.GetIndex: Integer;
begin
  result := lstRemotes.ItemIndex;
end;

procedure TfrmRemotes.SetReadOnly(AValue: boolean);
begin
  if fReadOnly = AValue then Exit;
  fReadOnly := AValue;
  UpdateControls;
end;

procedure TfrmRemotes.UpdateRemotesList(updItemIndex: Boolean);
var
  i: Integer;
begin
  lstRemotes.Clear;

  for i:=0 to Length(fRemotes)-1 do
    lstRemotes.AddItem(fRemotes[i].name, nil);

  if updItemIndex then begin
    if lstRemotes.Count>0 then
      lstRemotes.ItemIndex := 0;
  end;
end;

procedure TfrmRemotes.CheckSameURL;
begin

end;

procedure TfrmRemotes.UpdateInfo;
var
  s: string;
begin
  panBtns.OkButton.Enabled := false;
  s := Trim(txtName.Text);
  if (s='') or (pos('<', s)>0) then begin
    Invalid(rsInvalidRemoteName);
    exit;
  end;
  lblInfo.Caption := '';
  lblInfo.Font.Color := clBlack;
  panBtns.OkButton.Enabled := true;
end;

procedure TfrmRemotes.UpdateControls;
var
  i: Integer;
  isInvalid: Boolean;
begin
  i := CurrentIndex;
  isInvalid := (i<0) or (fRemotes[i].action=reaDel);

  btnAdd.Visible := not fReadOnly;
  btnDel.Visible := not fReadOnly;

  txtName.ReadOnly := fReadonly;
  txtName.Enabled := not isInvalid;
  txtFetch.Enabled := not fReadOnly and (i>=0) and not isInvalid;
  txtPush.Enabled := not fReadOnly and (i>=0) and (not chkSameURL.Checked) and not IsInvalid;
  chkSameURL.Enabled := not fReadOnly and (i>=0) and not IsInvalid;
  btnAdd.Enabled := (i>=0);
  btnDel.Enabled := (i>=0) and not isInvalid;
end;

procedure TfrmRemotes.Invalid(msg: string);
begin
  lblInfo.Font.Color := clRed;
  lblInfo.Caption := msg;
end;

end.

