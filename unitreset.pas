unit unitreset;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ButtonPanel,
  unitifaces, unitconfig, unitcommon;

const
  RESETTYPE_SOFT  = 0;
  RESETTYPE_MIXED = 1;
  RESETTYPE_HARD  = 2;

type

  { TfrmReset }

  TfrmReset = class(TForm)
    ButtonPanel1: TButtonPanel;
    lblInfo: TLabel;
    lblPrompt: TLabel;
    radResetType: TRadioGroup;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure radResetTypeClick(Sender: TObject);
  private
    fBranch: string;
    fCommit: string;
    fSubject: string;
    procedure SetBranch(AValue: string);
    procedure SetCommit(AValue: string);
    procedure SetSubject(AValue: string);
    procedure UpdateInfo;
  public
    property Branch: string read fBranch write SetBranch;
    property Commit: string read fCommit write SetCommit;
    property Subject: string read fSubject write SetSubject;
  end;

var
  frmReset: TfrmReset;

implementation

{$R *.lfm}

{ TfrmReset }

procedure TfrmReset.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fConfig.WriteWindow(Self, 'resetfrm', SECTION_GEOMETRY);
end;

procedure TfrmReset.FormCreate(Sender: TObject);
begin
  fConfig.ReadWindow(Self, 'resetfrm', SECTION_GEOMETRY);
end;

procedure TfrmReset.FormShow(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TfrmReset.radResetTypeClick(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TfrmReset.SetBranch(AValue: string);
begin
  if fBranch = AValue then Exit;
  fBranch := AValue;
  Caption := format(rsResetBranchS, [aValue]);
end;

procedure TfrmReset.SetCommit(AValue: string);
begin
  if fCommit = AValue then Exit;
  fCommit := AValue;
  lblPrompt.Caption := format(rsYouAreResettingBranch, [QuotedStr(fBranch),
      QuotedStr(copy(fCommit, 1, 16)), fSubject]);
end;

procedure TfrmReset.SetSubject(AValue: string);
begin
  if fSubject = AValue then Exit;
  fSubject := AValue;
end;

procedure TfrmReset.UpdateInfo;
begin
  case radResetType.ItemIndex of
    RESETTYPE_SOFT:   lblInfo.Caption := rsResetSoft;
    RESETTYPE_MIXED:  lblInfo.Caption := rsResetMixed;
    RESETTYPE_HARD:   lblInfo.Caption := rsResetHard;
  end;
end;

end.

