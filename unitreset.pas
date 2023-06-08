unit unitreset;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ButtonPanel,
  unitifaces, unitconfig;

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
  Caption := 'Reset branch ' + aValue
end;

procedure TfrmReset.SetCommit(AValue: string);
begin
  if fCommit = AValue then Exit;
  fCommit := AValue;
  lblPrompt.Caption :=
    'You are resetting branch ' + QuotedStr(fBranch) +
    ' to revision ' + QuotedStr(copy(fCommit, 1, 16)) + ':' + LineEnding +
    fSubject + LineEnding +
    'Do you want to proceed?';
end;

procedure TfrmReset.SetSubject(AValue: string);
begin
  if fSubject = AValue then Exit;
  fSubject := AValue;
end;

procedure TfrmReset.UpdateInfo;
begin
  case radResetType.ItemIndex of
    RESETTYPE_SOFT:
      lblInfo.Caption := 'Neither the working copy nor the index are altered.' + LineEnding + LineEnding + LineEnding +
                         'git doc: ' + lineEnding +
                         'Does not touch the index file or the working tree at' +
                         ' all (but resets the head to <commit>, just like all ' +
                         'modes do). This leaves all your changed files "Changes to be' +
                         ' committed", as git status would put it.';
    RESETTYPE_MIXED:
      lblInfo.Caption := 'Reset index, don''t touch working copy.' + LineEnding + LineEnding + LineEnding +
                         'git doc: ' + LineEnding +
                         'Resets the index but not the working tree (i.e., the ' +
                         'changed files are preserved but not marked for commit)' +
                         ' and reports what has not been updated. This is the ' +
                         'default action.';
    RESETTYPE_HARD:
      lblInfo.Caption := 'Reset index and working copy.' + LineEnding +
                         'LOCAL CHANGES WILL BE LOST!' + LineEnding + LineEnding + LineEnding +
                         'git doc: '+ LineEnding +
                         'Resets the index and working tree. Any changes to tracked ' +
                         'files in the working tree since <commit> are discarded. Any' +
                         ' untracked files or directories in the way of writing any ' +
                         'tracked files are simply deleted.';
  end;
end;

end.

