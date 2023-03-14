unit unitnewbranch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ButtonPanel, ExtCtrls, unitconfig, unitprocess, unitgit;

type

  { TfrmNewBranch }

  TfrmNewBranch = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Label2: TLabel;
    lblHint: TLabel;
    txtInfo: TMemo;
    Panel1: TPanel;
    tabSource: TTabControl;
    txtName: TEdit;
    lstSource: TListBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstSourceClick(Sender: TObject);
    procedure tabSourceChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
  private
    fBranchName: TCaption;
    fGit: TGit;
    fRefs: TStringList;
    fType: Integer;
    procedure ShowTabIndex(aIndex: Integer);
    procedure CheckOkButton;
    function AlreadyExists(aName: string; subType: TRefObjectSubType): boolean;
    procedure EvaluateOutcome;
    procedure ShowInfo;
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
  fRefs := TStringList.Create;
end;

procedure TfrmNewBranch.FormDestroy(Sender: TObject);
begin
  ClearRefList(fRefs);
  fRefs.Free;
end;

procedure TfrmNewBranch.FormShow(Sender: TObject);
begin

  if fGit.RefList(fRefs, '', [
      '%(refname:short)',
      '%(objecttype)',
      '%(objectname)',
      '%(upstream:short)',
      '%(HEAD)',
      '%(worktreepath)',
      '%(contents)',
      '%(authorname)',
      '%(authordate)',
      '%(committerdate)',
      '%(creatordate)',
      '%(*objecttype)',
      '%(*objectname)',
      '%(*authorname)',
      '%(*authordate)',
      '%(*contents)'
      ])>0
  then begin
    DebugLn(fGit.ErrorLog);
    ShowMessage('Error while getting list of branches');
    Close;
    exit;
  end;

  ShowTabIndex(0);
end;

procedure TfrmNewBranch.lstSourceClick(Sender: TObject);
begin
  ShowInfo;
  CheckOkButton;
end;

procedure TfrmNewBranch.tabSourceChange(Sender: TObject);
begin
  ShowTabIndex(tabSource.TabIndex)
end;

procedure TfrmNewBranch.txtNameChange(Sender: TObject);
begin
  CheckOkButton;
end;

procedure TfrmNewBranch.ShowTabIndex(aIndex: Integer);
var
  branchLine: TStringList;
  list: TStringList;
  i, j: Integer;
  info: PRefInfo;
  ok: boolean;
begin
  branchLine := TStringList.Create;
  branchLine.StrictDelimiter := true;
  branchLine.Delimiter := '|';
  lstSource.Items.BeginUpdate;
  try

    lstSource.Clear;

    for i:=0 to fRefs.Count-1 do begin
      info := PRefInfo(fRefs.Objects[i]);
      ok := false;
      case aIndex of
        0: ok := (info^.objType=rotCommit) and (not info^.isTracking);
        1: ok := (info^.objType=rotCommit) and info^.isTracking;
        2: ok := (info^.objType=rotTag);
        else exit;
      end;
      if ok then begin
        j := lstSource.Items.AddObject(fRefs[i], TObject(info));
        if (aIndex=0) and info^.head then
          lstSource.ItemIndex := j;
      end;
    end;

    ShowInfo;
    CheckOkButton;

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

function TfrmNewBranch.AlreadyExists(aName: string; subType: TRefObjectSubType
  ): boolean;
var
  i: Integer;
  info: PRefInfo;
begin
  result := false;
  aName := lowercase(aName);
  for i:=0 to fRefs.Count-1 do begin
    info := PRefInfo(fRefs.Objects[i]);
    if (info^.subType=subtype) and (aName=info^.refName) then begin
      result := true;
      break;
    end;
  end;
end;

procedure TfrmNewBranch.EvaluateOutcome;
var
  aIndex: Integer;
  info: PRefInfo;
begin
  fType := BT_INVALID;

  lblHint.Font.Color := clRed;
  fBranchName := Trim(txtName.Text);
  aIndex := lstSource.ItemIndex;
  if aIndex<0 then  info := nil
  else              info := PRefInfo(lstSource.Items.Objects[aIndex]);
  case tabSource.TabIndex of
    0: // branch based on a existing local branch tab
      begin
        if fBranchName<>'' then begin
          if AlreadyExists(fBranchName, rostLocal) then begin
            lblHint.caption := 'Alredy Exists';
            exit;
          end;
          if aIndex<0 then begin
            lblHint.caption := 'No refering branch selected';
            exit;
          end;
          fType := BT_NEWLOCAL_BRANCH
        end else begin
          lblHint.Caption := 'Branch name is empty';
          exit;
        end;
      end;
    1: // branch based on a tracking branch tab
      begin
        if aIndex<0 then begin
          lblHint.caption := 'No refering branch selected';
          exit;
        end;
        if fBranchName='' then begin
          if info=nil then begin
            lblHint.caption := 'Not refers to a tracking branch';
            exit;
          end;
          fBranchName := info^.upstream;
          if AlreadyExists(fBranchName, rostLocal) then begin
            lblHint.caption := 'Alredy Exists';
            exit;
          end;
          fType := BT_NEWLOCAL_TRACKING;
        end;
      end;
    2:  // branch based on a tag tab
      begin
        if fBranchName<>'' then begin
          if AlreadyExists(fBranchName, rostLocal) then begin
            lblHint.caption := 'Alredy Exists';
            exit;
          end;
          if aIndex<0 then begin
            lblHint.caption := 'No refering tag selected';
            exit;
          end;
          fType := BT_NEWLOCAL_TAG;
        end else begin
          lblHint.Caption := 'Branch name is empty';
          exit;
        end;
      end;
  end;
  lblHint.Font.Color := clGreen;
  lblHint.Caption := 'Feasible';
end;

procedure TfrmNewBranch.ShowInfo;
var
  aIndex: Integer;
  info: PRefInfo;
  s: string;
begin

  txtInfo.Lines.BeginUpdate;
  try
    txtInfo.Clear;
    aIndex := lstSource.ItemIndex;
    if aIndex<0 then
      exit;
    info := PRefInfo(lstSource.Items.Objects[aIndex]);

    s := info^.refName;
    if info^.upstream<>'' then
      s := s + ' -> ' + info^.upstream;
    txtInfo.Lines.Add(s);
    if info^.worktreepath<>'' then
      txtInfo.Lines.Add('worktree: '+info^.worktreepath);

    txtInfo.Lines.Add('');
    if (info^.objType=rotTag) and (info^.refered<>nil) then begin
      txtInfo.Lines.Add('Tag: %s',[info^.objName]);
      txtInfo.Lines.Add('%s (%s)',[info^.authorName, DateTimeToGitFmt(info^.authorDate)]);
      txtInfo.Lines.Add(info^.subject);
      txtInfo.Lines.Add('');
      info := info^.refered;
    end;
    txtInfo.Lines.Add('Commit: %s',[info^.objName]);
    txtInfo.Lines.Add('%s (%s)',[info^.authorName, DateTimeToGitFmt(info^.authorDate)]);
    txtInfo.Lines.Add(info^.subject);
  finally
    txtInfo.Lines.EndUpdate;
  end;

end;

procedure TfrmNewBranch.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fConfig.WriteWindow(Self, 'newbranchform', SECTION_GEOMETRY);
end;

end.

