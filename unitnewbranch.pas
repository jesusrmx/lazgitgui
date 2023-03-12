unit unitnewbranch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ButtonPanel, ExtCtrls, unitconfig, unitprocess, unitgit;

type

  { TfrmNewBranch }

  TfrmNewBranch = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Panel1: TPanel;
    tabSource: TTabControl;
    txtName: TEdit;
    lstSource: TListBox;
    radNew: TRadioButton;
    radTracking: TRadioButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure radTrackingClick(Sender: TObject);
    procedure tabSourceChange(Sender: TObject);
  private
    fGit: TGit;
    fLocalBranches, fTrackingBranches, fTags: TStringList;
    fType: Integer;
    procedure ShowTabIndex(aIndex: Integer);
    procedure CheckOkButton;
    function AlreadyExists(aName: string; lst:TStrings): boolean;
    procedure EvaluateOutcome;
  public
    property Git: TGit read fGit write fGit;
  end;

var
  frmNewBranch: TfrmNewBranch;

implementation

{$R *.lfm}

const
  BF_REFNAME  = 0;
  BF_OBJNAME  = 1;
  BF_UPSTREAM = 2;
  BF_CURRENT  = 3;
  BF_WORKTREE = 4;
  BF_SUBJECT  = 5;

  BT_INVALID            = 0;
  BT_NEWLOCAL_BRANCH    = 1;
  BT_NEWLOCAL_TRACKING  = 2;
  BT_NEWLOCAL_TAG       = 3;

{ TfrmNewBranch }

procedure TfrmNewBranch.FormCreate(Sender: TObject);
begin
  fConfig.ReadWindow(Self, 'newbranchform', SECTION_GEOMETRY);
  fLocalBranches := TStringList.Create;
  fTrackingBranches := TStringList.Create;
  fTags := TStringList.Create;
end;

procedure TfrmNewBranch.FormDestroy(Sender: TObject);
begin
  fLocalBranches.Free;
  fTrackingBranches.Free;
  fTags.Free;
end;

procedure TfrmNewBranch.FormShow(Sender: TObject);
var
  list, branchLine: TStringList;
  s: String;
begin

  try
    list := TStringList.Create;
    branchLine := TStringList.Create;
    branchLine.StrictDelimiter := true;
    branchLine.Delimiter := '|';

    if fGit.BranchList(list, [
        '%(refname:short)',
        '%(objecttype)',
        '%(upstream:short)',
        '%(HEAD)',
        '%(worktreepath)',
        '%(contents:subject)'])>0
    then begin
      ShowMessage('Error while getting list of branches');
      Close;
      exit;
    end;

    fLocalBranches.clear;
    fTrackingBranches.clear;
    fTags.Clear;

    for s in list do begin
      branchLine.DelimitedText := s;
      case branchLine[BF_OBJNAME] of
        'tag':
          fTags.Add(s);

        'commit':
          if pos('/', branchLine[BF_REFNAME])>0 then
            fTrackingBranches.Add(s)
          else
            fLocalBranches.Add(s);
      end;
    end;

    ShowTabIndex(0);

  finally
    list.free;
  end;
end;

procedure TfrmNewBranch.radTrackingClick(Sender: TObject);
begin
  if radTracking.Checked then begin
    tabSource.TabIndex := 1;
    ShowTabIndex(1);
  end;
  CheckOkButton;
end;

procedure TfrmNewBranch.tabSourceChange(Sender: TObject);
begin
  ShowTabIndex(tabSource.TabIndex)
end;

procedure TfrmNewBranch.ShowTabIndex(aIndex: Integer);
var
  branchLine: TStringList;
  list: TStringList;
  i: Integer;
begin
  branchLine := TStringList.Create;
  branchLine.StrictDelimiter := true;
  branchLine.Delimiter := '|';
  lstSource.Items.BeginUpdate;
  try

    lstSource.Clear;

    case aIndex of
      0: list := fLocalBranches;
      1: list := fTrackingBranches;
      2: list := fTags;
      else exit;
    end;

    for i:=0 to list.Count-1 do begin
      branchLine.DelimitedText := list[i];
      lstSource.Items.Add(branchLine[BF_REFNAME]);
    end;

  finally
    lstSource.Items.EndUpdate;
    branchLine.Free;
  end;
end;

procedure TfrmNewBranch.CheckOkButton;
begin
  EvaluateOutcome;
  ButtonPanel1.OKButton.Enabled :=
    (fType=BT_NEWLOCAL_BRANCH) or
    (fType=BT_NEWLOCAL_TRACKING) or
    (fType=BT_NEWLOCAL_TAG);
end;

function TfrmNewBranch.AlreadyExists(aName: string; lst: TStrings): boolean;
var
  i: Integer;
begin
  result := false;
  aName := lowercase(aName);
  for i:=0 to lst.Count-1 do begin
    if pos(aName + '|', lowercase(lst[i]))=1 then begin
      result := true;
      break;
    end;
  end;
end;

procedure TfrmNewBranch.EvaluateOutcome;
begin
  fType := BT_INVALID;
end;

procedure TfrmNewBranch.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fConfig.WriteWindow(Self, 'newbranchform', SECTION_GEOMETRY);
end;

end.

