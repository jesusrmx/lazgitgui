unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, LazLogger, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList, synEditTypes, SynEdit, SynHighlighterDiff,
  StrUtils, FileUtil, unitconfig, unitprocess, unitentries, unitgit, Types,
  lclType, Menus, unitnewbranch, unitruncmd, LConvEncoding;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actCommit: TAction;
    actFetch: TAction;
    actQuit: TAction;
    actPull: TAction;
    actPush: TAction;
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
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuMain: TMainMenu;
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
    procedure actCommitExecute(Sender: TObject);
    procedure actFetchExecute(Sender: TObject);
    procedure actPullExecute(Sender: TObject);
    procedure actPushExecute(Sender: TObject);
    procedure actQuitExecute(Sender: TObject);
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
    procedure DoCommit;
    procedure DoPush;
    procedure DoFetch;
    procedure DoPull;
    procedure OnBranchMenuClick(Sender: TObject);
    procedure OnBranchSwitch(Data: PtrInt);
    procedure OnReloadBranchMenu(Data: PtrInt);
    procedure OpenDirectory(aDir: string);
    procedure UpdateBranch;
    procedure RestoreGui;
    procedure SaveGui;
    procedure ItemAction(sender: TListbox; aIndex: Integer);
    procedure UpdateBranchMenu;
    procedure UpdateStatus;
    procedure ShowError;
    procedure ViewFile(filename: string);
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
  rsPushingYourCommits = 'Pushing your commits';
  rsThereAreCommitsBehind = 'There are commits behind, are you sure you want to push?';


const
  MENU_INVALID        = -1;

  MENU_BRANCH_NEW     = 1;
  MENU_BRANCH_RELOAD  = 2;
  MENU_BRANCH_SWITCH  = 3;

  VIEWER_BUFSIZE      = 1024*4;
  BIN_BUFSIZE         = 1024;

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
  f: TfrmNewBranch;
begin
  mi := TMenuItem(sender);
  case mi.tag of
    MENU_BRANCH_NEW:
      begin
        f := TfrmNewBranch.Create(Self);
        f.Git := fGit;
        try
          if f.ShowModal=mrOk then begin
            ShowMessage('Creating a branch');
          end;
        finally
          f.Free;
        end;
      end;
    MENU_BRANCH_RELOAD:
      begin
        fPopPoint := popBranch.PopupPoint;
        Application.QueueAsyncCall(@OnReloadBranchMenu, 0);
      end;
    MENU_BRANCH_SWITCH:
      begin
        Application.QueueAsyncCall(@OnBranchSwitch, PtrInt(Sender));
      end;
  end;
end;

procedure TfrmMain.OnBranchSwitch(Data: PtrInt);
var
  mi: TMenuItem;
begin
  mi := TMenuItem(TObject(Data));
  if fGit.Switch(mi.Caption)>0 then
    ShowError
  else
    UpdateStatus;
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
      etUntracked,
      etWorktreeChangedSinceIndex..etTypeChangedInWorktreeSinceIndexC:
        begin
          if fGit.Add(Entry)>0 then
            ShowError
          else
            UpdateStatus;
        end;
      etDeletedInWorktree:
        begin
          if fGit.Rm(Entry)>0 then
            ShowError
          else
            UpdateStatus;
        end;
      // other merge conflicts:
      // see:
      //etUnmergedDeletedByUs:
      //  begin
      //  end
      else
        ShowMessage('Not yet implemented for Unstaged: '+cmdOut);
    end;
  end else begin
    WriteStr(cmdOut, Entry^.EntryTypeStaged);
    case Entry^.EntryTypeStaged of
      etAddedToIndex..etAddedToIndexD,
      etUpdatedInIndex..etTypeChangedInIndexD,
      etRenamedInIndex..etRenamedInIndexD,
      etDeletedFromIndex:
        begin
          if fGit.Restore(entry, true)>0 then
            ShowError
          else
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
        '%(HEAD)'])>0 then
    begin
      ShowError;
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
    if fGit.Status(lstUnstaged.Items, lstStaged.Items)>0 then
      ShowError
    else
      UpdateBranch;
  finally
    lstStaged.Items.EndUpdate;
    lstUnstaged.Items.EndUpdate;
  end;
end;

procedure TfrmMain.ShowError;
begin
  txtDiff.Text := cmdLine.ErrorLog;
end;

function isBinBuffer(Buffer:pbyte; size: SizeInt): boolean;
var
  p,q: pbyte;
begin
  result := true;
  p := Buffer;
  q := p + Size;
  while (p<q) do begin
    if not (p^ in [9,10,13,32..255]) then
      exit;
    inc(p);
  end;
  result := false;
end;

procedure SampleOfFile(filename: string; out stream: TStream);
var
  F: TFileStream;
  readbytes: Int64;
  buffer: pbyte;

  procedure StoreString(msg:string);
  begin
    stream := TStringStream.Create(msg);
  end;

begin

  if not FileExists(filename) then begin
    StoreString(format('%s does not exists', [filename]));
    exit;
  end;

  //{$ifdef Unix}
  //// use the file command for identifying the file...
  //cmdLine.RunProcess('file ' + filename, '', cmdOut);
  // check for ASCII
  //{$else}
  //{$endif}

  F := TFileStream.Create(filename, fmOpenRead + fmShareDenyNone);
  GetMem(buffer, BIN_BUFSIZE);
  readBytes := F.Read(buffer^, BIN_BUFSIZE);
  if IsBinBuffer(buffer, readBytes) then begin
    StoreString(format('The file ''%s'' is binary, %d bytes',[filename, F.Size]));
    F.Free;
  end else
    stream := F;

end;

procedure TfrmMain.ViewFile(filename: string);
var
  stream: TStream;
begin
  SampleOfFile(filename, stream);
  stream.Position := 0;
  txtDiff.Lines.LoadFromStream(stream);
  stream.Free;
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
  if button=mbLeft then begin
    aIndex := TListBox(Sender).GetIndexAtXY(x, y);
    if aIndex>=0 then begin
      if (x>0) and (x < 20) then begin
        fClickedIndex := aIndex;
        Application.QueueAsyncCall(@DoItemAction, ptrInt(Sender));
      end else
        Application.QueueAsyncCall(@DoGitDiff, PtrInt(Sender));
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  s, v: string;
  aQuality: TFontQuality;
begin
  fGit := TGit.Create;

  fConfig.OpenConfig;

  s := ''; v := '';
  if Application.HasOption('git') then
    s := Application.GetOptionValue('git');
  if s='' then begin
    s := fConfig.ReadString('git');
    v := fConfig.ReadString('gitversion');
  end;

  fGit.SetupExe(s, v);

  if fGit.Exe='' then begin
    fConfig.CloseConfig;
    DebugLn('Error: Could not find git command');
    Application.Terminate;
    exit;
  end;

  if (fGit.Exe<>s) or (fGit.Version<>v) then begin
    fConfig.WriteString('git', fGit.Exe);
    fConfig.WriteString('gitversion', fGit.Version);
  end;

  //DebugLn('git=', fGit.Exe);

  txtDiff.Clear;

  fConfig.ReadFont(txtDiff.Font, 'viewer', fpFixed, SECTION_FONTS);

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

procedure TfrmMain.actCommitExecute(Sender: TObject);
begin
  DoCommit;
end;

procedure TfrmMain.actFetchExecute(Sender: TObject);
begin
  DoFetch;
end;

procedure TfrmMain.actPullExecute(Sender: TObject);
begin
  DoPull;
end;

procedure TfrmMain.actPushExecute(Sender: TObject);
begin
  DoPush;
end;

procedure TfrmMain.actQuitExecute(Sender: TObject);
begin
  Close;
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

procedure TfrmMain.DoCommit;
begin
  if lstStaged.Count=0 then begin
    ShowMessage('You have to stage something in order to commit');
    exit;
  end;
  if fGit.Commit(txtComment.Text,'')>0 then
    ShowError
  else begin
    UpdateStatus;
    txtDiff.Clear;
    txtComment.Clear;
  end;
end;

procedure TfrmMain.DoPush;
var
  res: TModalResult;
begin
  //if fConfig.ReadBoolean('FetchBeforePush', false) then
  //  doFetch;
  if fGit.CommitsBehind<0 then begin
    res := QuestionDlg(rsPushingYourCommits, rsThereAreCommitsBehind, mtConfirmation,
      [mrYes, 'Push', mrCancel, 'Cancel'], 0 );
    if res<>mrYes then
      exit;
  end;

  RunInteractive(fGit.Exe + ' push --progress', fGit.TopLevelDir, 'Pushing to remote: ', 'Push');
  UpdateStatus;
end;

procedure TfrmMain.DoFetch;
begin
  RunInteractive(fGit.Exe + ' fetch', fGit.TopLevelDir, 'Fetching from remote: ', 'Fetch');
end;

procedure TfrmMain.DoPull;
begin
  RunInteractive(fGit.Exe + ' pull', fGit.TopLevelDir, 'pulling from remote: ', 'Pull');
end;

procedure TfrmMain.DoGitDiff(Data: PtrInt);
var
  lb: TListbox absolute Data;
  res: Integer;
  entry: PFileEntry;
  unstaged: Boolean;
begin
  if lb.ItemIndex>=0 then begin
    entry := PFileEntry(lb.Items.Objects[lb.ItemIndex]);
    panFileState.Caption := EntryTypeToStr(entry^.x, entry^.y);
    unstaged := lb=lstUnstaged;
    if unstaged and (entry^.EntryTypeUnStaged in [etUntracked, etIgnored]) then
      ViewFile(fGit.TopLevelDir + entry^.path)
    else begin
      res := fGit.Diff(entry, unstaged, txtDiff.Lines);
      if res>0 then
        ShowError;
    end;
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

  actPush.Enabled := ahead;

  txtDiff.Clear;

  Caption := '[git '+fGit.Version+'](' + fGit.TopLevelDir + ')';
end;

procedure TfrmMain.RestoreGui;
begin
  fConfig.OpenConfig;

  fConfig.ReadWindow(Self, 'mainform', SECTION_GEOMETRY);

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

  fConfig.WriteWindow(Self, 'mainform', SECTION_GEOMETRY);
  fConfig.WriteInteger('lstUnstaged.Height', lstUnstaged.Height, SECTION_GEOMETRY);
  fConfig.WriteInteger('panleft.width', panLeft.Width, SECTION_GEOMETRY);
  fConfig.WriteInteger('pancommit.height', pancommit.Height, SECTION_GEOMETRY);

  fConfig.CloseConfig;
end;

end.

