unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ActnList, SynEdit, SynHighlighterDiff, StrUtils, FileUtil, unitconfig, unitprocess,
  unitentries, unitgit, Types, lclType, Menus;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actRescan: TAction;
    ActionList1: TActionList;
    btnRescan: TButton;
    btnStageChanged: TButton;
    btnSignOff: TButton;
    btnCommit: TButton;
    btnPush: TButton;
    imgList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lblMerging: TLabel;
    lblRemote: TLabel;
    lblAheadBehind: TLabel;
    lblBranch: TLabel;
    lstUnstaged: TListBox;
    lstStaged: TListBox;
    panCommitState: TPanel;
    panBranch: TPanel;
    panFileState: TPanel;
    panUnstaged: TPanel;
    panStagedContainer: TPanel;
    panStaged: TPanel;
    popBranch: TPopupMenu;
    splitterMain: TSplitter;
    SynDiffSyn1: TSynDiffSyn;
    txtComment: TMemo;
    panLeft: TPanel;
    panContent: TPanel;
    panCommit: TPanel;
    panCommitButtons: TPanel;
    splitterStaged: TSplitter;
    splitterCommit: TSplitter;
    txtDiff: TSynEdit;
    procedure actRescanExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblBranchClick(Sender: TObject);
    procedure lblBranchContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure lstUnstagedDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lstUnstagedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fGit: TGit;
    fClickedIndex: Integer;
    fDir: string;
    fPopPoint: TPoint;
    procedure DoGitDiff(Data: PtrInt);
    procedure DoItemAction(Data: PtrInt);
    procedure OnBranchMenuClick(Sender: TObject);
    procedure OnReloadBranchMenu(Data: PtrInt);
    procedure OpenDirectory(aDir: string);
    procedure UpdateBranch;
    procedure RestoreGui;
    procedure SaveGui;
    procedure ItemAction(sender: TListbox; aIndex: Integer);
    procedure UpdateBranchMenu;
    procedure UpdateStatus;
  public

  end;

var
  frmMain: TfrmMain;
  targetDir: string = '';

implementation

{$R *.lfm}

resourcestring
  rsNewBranch = 'New Branch';
  rsReload = 'Reload';

const
  MENU_INVALID        = -1;

  MENU_BRANCH_NEW     = 1;
  MENU_BRANCH_RELOAD  = 2;
  MENU_BRANCH_SWITCH  = 3;

{ TfrmMain }

procedure TfrmMain.FormShow(Sender: TObject);
begin
  OpenDirectory(targetDir);
end;

procedure TfrmMain.lblBranchClick(Sender: TObject);
begin
  if popBranch.Items.Count=0 then
    UpdateBranchMenu;
  popBranch.PopUp;
end;

procedure TfrmMain.OnBranchMenuClick(Sender: TObject);
var
  mi: TMenuItem;
begin
  mi := TMenuItem(sender);
  case mi.tag of
    MENU_BRANCH_NEW: ShowMessage('Creating a new local branch');
    MENU_BRANCH_RELOAD:
      begin
        fPopPoint := popBranch.PopupPoint;
        Application.QueueAsyncCall(@OnReloadBranchMenu, 0);
      end;
    MENU_BRANCH_SWITCH: ShowMessage('Switching to branch '+mi.Caption)
  end;
end;

procedure TfrmMain.OnReloadBranchMenu(Data: PtrInt);
begin
  UpdateBranchMenu;
  popBranch.PopUp(fPopPoint.x, fPopPoint.y);
end;

procedure TfrmMain.lblBranchContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

  if popBranch.Items.Count>0 then
    exit;

  UpdateBranchMenu;
end;

procedure TfrmMain.ItemAction(sender: TListbox; aIndex: Integer);
var
  Entry: PFileEntry;
  cmdOut: RawByteString;
begin
  Entry := PFileEntry(Sender.Items.Objects[aIndex]);
  if sender=lstUnstaged then begin
    WriteStr(cmdOut, Entry^.EntryTypeUnStaged);
    case Entry^.EntryTypeUnStaged of
      etUntracked:
        begin
          fGit.Add(Entry);
          UpdateStatus;
        end;
      etDeletedInWorktree:
        begin
          fGit.Rm(Entry);
          UpdateStatus;
        end;
      // other merge conflicts:
      // see:
      etUnmergedDeletedByUs:
        begin
        end
      else
        ShowMessage('Not yet implemented for Unstaged: '+cmdOut);
    end;
  end else begin
    WriteStr(cmdOut, Entry^.EntryTypeStaged);
    case Entry^.EntryTypeStaged of
      etAddedToIndex..etAddedToIndexD,
      etRenamedInIndex..etRenamedInIndexD,
      etDeletedFromIndex:
        begin
          fGit.Restore(entry, true);
          UpdateStatus;
        end;
      else
        ShowMessage('Not yet implemented for Staged: '+cmdOut);
    end;
  end;
end;

procedure TfrmMain.UpdateBranchMenu;
var
  list, branchLine: TStringList;
  n, i: integer;
  p, q: pchar;
  mi: TMenuItem;
begin
  popBranch.Items.Clear;

  mi := TMenuItem.Create(Self);
  mi.Caption := rsNewBranch;
  mi.OnClick := @OnBranchMenuClick;
  mi.Tag := MENU_BRANCH_NEW;
  popBranch.Items.Add(mi);

  mi := TMenuItem.Create(Self);
  mi.Caption := rsReload;
  mi.OnClick := @OnBranchMenuClick;
  mi.Tag := MENU_BRANCH_RELOAD;
  popBranch.Items.Add(mi);

  try
    list := TStringList.Create;

    if fGit.BranchList(list, [
        '%(refname:short)',
        '%(objecttype)',
        '%(upstream:short)',
        '%(HEAD)',
        '%(worktreepath)',
        '%(contents:subject)'])>0 then
    begin
      txtDiff.Text := fGit.ErrorLog;
      exit;
    end;

    try
      branchLine := TStringList.Create;
      branchLine.StrictDelimiter := true;
      branchLine.Delimiter := '|';

      for i:=0 to list.Count-1 do begin
        BranchLine.DelimitedText := list[i];
        if pos('/', branchLine[0])<>0 then
          continue;

        if popBranch.Items.Count=2 then begin
          mi := TMenuItem.Create(Self);
          mi.Caption := '-';
          mi.Tag := MENU_INVALID;
          popBranch.Items.Add(mi);
        end;

        mi := TMenuItem.Create(Self);
        if branchLine[2]='' then
          mi.Caption := branchLine[0]
        else
          mi.Caption := branchLine[0]; //+ ' -> ' + branchLine[2];
        mi.GroupIndex := 1;
        mi.AutoCheck := true;
        mi.Checked := branchLine[3]='*';
        if not mi.Checked then
          mi.OnClick := @OnBranchMenuClick;
        mi.Tag := MENU_BRANCH_SWITCH;
        mi.RadioItem := true;
        popBranch.Items.Add(mi);

      end;

    finally
      branchLine.free;
    end;
  finally
    list.Free;
  end;
end;

procedure TfrmMain.UpdateStatus;
begin
  lstUnstaged.Items.BeginUpdate;
  lstStaged.Items.BeginUpdate;
  try
    if fGit.Status(lstUnstaged.Items, lstStaged.Items)<>0 then
      txtDiff.Text := fGit.ErrorLog;
    UpdateBranch;
  finally
    lstStaged.Items.EndUpdate;
    lstUnstaged.Items.EndUpdate;
  end;
end;

function OwnerDrawStateToStr(State: TOwnerDrawState): string;
  procedure Add(st: string);
  begin
    if result<>'' then result += ',';
    result += st;
  end;
var
  stateItem: TOwnerDrawStateType;
  s: string;
begin
  for stateItem in State do begin
    WriteStr(s, stateItem);
    Add(s);
  end;
  result := '[' + result + ']';
end;

procedure TfrmMain.lstUnstagedDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  lb: TListBox;
  x, y: Integer;
  ts: TTextStyle;
  entry: PFileEntry;
  aCanvas: TCanvas;
  aColor: TColor;
  sel: boolean;
begin
  if index<0 then
    exit;

  lb := TListBox(Control);
  entry := PFileEntry(lb.Items.Objects[index]);

  sel := (odSelected in State) and lb.Focused;

  aCanvas := lb.Canvas;
  if lb.Focused then
    if sel then aColor := clHighlight
    else        aColor := lb.Color
  else          aColor := lb.Color;
  aCanvas.Brush.Color := aColor;
  aCanvas.FillRect(aRect);

  if lb.Focused then
    if sel then aColor := clHighlightText
    else        aColor := clBlack
  else          aColor := clBlack;

  lb.Canvas.Font.Color := aColor;
  ts := lb.Canvas.TextStyle;
  ts.Alignment := taLeftJustify;
  ts.Layout := tlCenter;
  lb.Canvas.Brush.Style := bsClear;
  lb.Canvas.TextRect(aRect, aRect.Left + 22, aRect.Top, lb.Items[index]);

  index := 0;
  if lb=lstUnstaged then
    case entry^.EntryTypeUnStaged of
      etWorktreeChangedSinceIndex..etWorktreeChangedSinceIndexT:  index := 6;
      etDeletedInWorktree..etDeletedInWorktreeC:                  index := 3;
      etIgnored:                                                  index := 5;
      etUnmergedAddedByUs..etUnmergedBothModified:                index := 11;
    end
  else
    case entry^.EntryTypeStaged of
      etUpdatedInIndex..etUpdatedInIndexD:                        index := 7;
      etAddedToIndex..etAddedToIndexD:                            index := 2;
      etDeletedFromIndex:                                         index := 3;
      etCopiedInIndex..etCopiedInIndexD:                          index := 4;
      etRenamedInIndex..etRenamedInIndexD:                        index := 10;
    end;

  imgList.Draw(lb.Canvas, aRect.Left + 2, aRect.Top + 1, index);
end;

procedure TfrmMain.lstUnstagedMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  aIndex: Integer;
begin
  aIndex := TListBox(Sender).GetIndexAtXY(x, y);
  if aIndex>=0 then begin
    if (x>0) and (x < 20) then begin
      fClickedIndex := aIndex;
      Application.QueueAsyncCall(@DoItemAction, ptrInt(Sender));
    end else
      Application.QueueAsyncCall(@DoGitDiff, PtrInt(Sender));
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  s: string;
  aQuality: TFontQuality;
begin
  fGit := TGit.Create;

  fConfig.OpenConfig;

  s := '';
  if Application.HasOption('git') then
    s := Application.GetOptionValue('git');
  if s='' then
    s := fConfig.ReadString('git');

  fGit.SetupExe(s);

  if fGit.Exe='' then begin
    fConfig.CloseConfig;
    DebugLn('Error: Could not find git command');
    Application.Terminate;
    exit;
  end;

  fConfig.WriteString('git', fGit.Exe);

  //DebugLn('git=', fGit.Exe);

  txtDiff.Clear;

  with txtDiff.Font do begin
    s := Name;
    aQuality := Quality;
    {$ifdef Darwin}
    s := 'Menlo';
    aQuality := fqAntialiased;
    {$endif}
    {$ifdef MsWindows}
    s := 'Courier New';
    {$endif}
    Name := fConfig.ReadString('font.name', s, 'Viewer');
    Size := fConfig.ReadInteger('font.size', 10, 'Viewer');
    if fConfig.ReadBoolean('font.antialiased', aQuality=fqAntialiased, 'Viewer') then
      Quality := fqAntialiased
    else
      Quality := fqNonAntialiased;
  end;

  panFileState.Caption := '';

  RestoreGui;

  fConfig.CloseConfig;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveGui;
end;

procedure TfrmMain.actRescanExecute(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fGit.Free;
end;

procedure TfrmMain.OpenDirectory(aDir: string);
begin
  aDir := ExpandFileName(aDir);
  fGit.OpenDir(aDir);
  fDir := aDir;
  UpdateStatus;
end;

procedure TfrmMain.DoItemAction(Data: PtrInt);
begin
  ItemAction(TListBox(Data), fClickedIndex);
end;

procedure TfrmMain.DoGitDiff(Data: PtrInt);
var
  lb: TListbox absolute Data;
  res: Integer;
  entry: PFileEntry;
begin
  if lb.ItemIndex>=0 then begin
    entry := PFileEntry(lb.Items.Objects[lb.ItemIndex]);
    panFileState.Caption := EntryTypeToStr(entry^.x, entry^.y);
    res := fGit.Diff(entry, lb=lstUnstaged, txtDiff.Lines);
    if res<>0 then
      txtDiff.Text := format('Error getting diff: %d%s%s',[res, LineEnding, fGit.ErrorLog]);
  end;
end;

procedure TfrmMain.UpdateBranch;
var
  s: string;
  ahead, behind: boolean;
begin
  ahead := fGit.CommitsAhead>0;
  behind := fGit.CommitsBehind<0;

  label1.Visible := not ahead and not behind;
  lblBranch.Caption := fGit.Branch;
  s := '';
  if fGit.Merging then begin
    s += '(MERGING';
    if fGit.MergingConflict then s += ' CONFLICT';
    s += ')';
  end;
  lblMerging.Caption := s;

  s := '';
  if ahead then s += format('%d commits ahead', [fGit.CommitsAhead]);
  if ahead and behind then s += ', ';
  if behind then s += format('%d commits behind', [-fGit.CommitsBehind]);
  if ahead or behind then s += ' of';

  if s='' then s:=' ';
  lblAheadBehind.Caption := s;

  label2.Visible := (not ahead and not behind) and (fGit.Upstream<>'');
  lblRemote.Caption := fGit.Upstream;

  Caption := '(' + fGit.TopLevelDir + ')';

end;

procedure TfrmMain.RestoreGui;
begin
  fConfig.OpenConfig;

  Left :=   fConfig.ReadInteger('mainform.Left',    Left,   SECTION_GEOMETRY);
  Top :=    fConfig.ReadInteger('mainform.Top',     Top,    SECTION_GEOMETRY);
  Width :=  fConfig.ReadInteger('mainform.Width',   Width,  SECTION_GEOMETRY);
  Height := fConfig.ReadInteger('mainform.Height',  Height, SECTION_GEOMETRY);
  if fConfig.ReadBoolean('mainform.maximized', false, SECTION_GEOMETRY) then
    WindowState := wsMaximized;
  lstUnstaged.Height := fConfig.ReadInteger('lstUnstaged.Height', lstUnstaged.Height, SECTION_GEOMETRY);
  panLeft.Width := fConfig.ReadInteger('panleft.width', panLeft.Width, SECTION_GEOMETRY);
  pancommit.Height := fConfig.ReadInteger('pancommit.height', pancommit.Height, SECTION_GEOMETRY);

  fConfig.CloseConfig;
end;

procedure TfrmMain.SaveGui;
var
  isMaximized: Boolean;
begin
  fConfig.OpenConfig;

  isMaximized := WindowState=wsMaximized;
  fConfig.WriteBoolean('mainform.maximized', isMaximized, SECTION_GEOMETRY);
  if not isMaximized then begin
    fConfig.WriteInteger('mainform.Left', Left, SECTION_GEOMETRY);
    fConfig.WriteInteger('mainform.Top', Top, SECTION_GEOMETRY);
    fConfig.WriteInteger('mainform.Width', Width, SECTION_GEOMETRY);
    fConfig.WriteInteger('mainform.Height', Height, SECTION_GEOMETRY);
  end;
  fConfig.WriteInteger('lstUnstaged.Height', lstUnstaged.Height, SECTION_GEOMETRY);
  fConfig.WriteInteger('panleft.width', panLeft.Width, SECTION_GEOMETRY);
  fConfig.WriteInteger('pancommit.height', pancommit.Height, SECTION_GEOMETRY);

  fConfig.CloseConfig;
end;

end.

