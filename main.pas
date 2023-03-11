unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ActnList, SynEdit, SynHighlighterDiff, StrUtils, FileUtil, unitconfig, unitprocess,
  unitentries, Types, lclType, Menus;

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
    fBranch: String;
    fClickedIndex: Integer;
    fGitCommand: string;
    fDir, fTopLevelDir: string;
    fMerging, fMergingConflict: Boolean;
    fUpstream: String;
    fUntrackedMode: string;
    fIgnoredMode: string;
    fCommitsAhead: Integer;
    fCommitsBehind: Integer;
    fEntries: TFPList;
    procedure DoGitDiff(Data: PtrInt);
    procedure DoItemAction(Data: PtrInt);
    procedure OnBranchClick(Sender: TObject);
    procedure OpenDirectory(aDir: string);
    procedure GitStatus;
    procedure GitStatusBranch(var head: pchar; tail: pchar);
    procedure GitStatusFiles(var head: pchar; tail: pchar);
    procedure UpdateBranch;
    procedure Clear;
    function  TryGitIn(aPath: string): boolean;
    procedure RestoreGui;
    procedure SaveGui;
    procedure GitDiff(Sender: TObject);
    procedure ItemAction(sender: TListbox; aIndex: Integer);
    function GitMerging: boolean;
    procedure UpdateBranchMenu;
  public

  end;

var
  frmMain: TfrmMain;
  targetDir: string = '';

implementation

{$R *.lfm}

const
  {$ifdef MsWindows}
  EXE_EXTENSION = '.exe';
  {$else}
  EXE_EXTENSION = '';
  {$endif}

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

procedure TfrmMain.OnBranchClick(Sender: TObject);
var
  mi: TMenuItem;
begin
  mi := TMenuItem(sender);
  case mi.tag of
    0: ShowMessage('Creating a new local branch');
    1: ShowMessage('Switching to branch '+mi.Caption)
  end;
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
          cmdLine.RunProcess(fGitCommand+' add '+ Entry^.path, fTopLevelDir, cmdOut);
          GitStatus;
        end;
      etDeletedInWorktree:
        begin
          cmdLine.RunProcess(fGitCommand+' rm '+ Entry^.path, fTopLevelDir, cmdOut);
          GitStatus;
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
          cmdLine.RunProcess(fGitCommand+' restore --staged '+ Entry^.path, fTopLevelDir, cmdOut);
          GitStatus;
        end;
      else
        ShowMessage('Not yet implemented for Staged: '+cmdOut);
    end;
  end;
end;

function TfrmMain.GitMerging: boolean;
var
  cmdOut: RawByteString;
begin
  //result := FileExists(fTopLevelDir + '.git/MERGE_HEAD');
  // ref: https://stackoverflow.com/a/55192451
  // todo: close output and close stderr
  result := cmdLine.RunProcess(fGitCommand + ' rev-list -1 MERGE_HEAD', fDir, cmdOut) = 0;
end;

procedure TfrmMain.UpdateBranchMenu;
var
  cmd: string;
  list, branchLine: TStringList;
  cmdOut: RawByteString;
  n, i: integer;
  p, q: pchar;
  mi: TMenuItem;
begin
  cmd := ' branch -vv ' +
         '--format="' +
         '%(refname:short)|' +
         '%(objecttype)|' +
         '%(upstream:short)|' +
         '%(HEAD)|' +
         '%(worktreepath)|' +
         '%(contents:subject)' +
         '" -a';
  // fill branch list

  popBranch.Items.Clear;

  mi := TMenuItem.Create(Self);
  mi.Caption := 'New Branch';
  mi.OnClick := @OnBranchClick;
  mi.Tag := 0;
  popBranch.Items.Add(mi);

  try
    list := TStringList.Create;
    branchLine := TStringList.Create;
    branchLine.StrictDelimiter := true;
    branchLine.Delimiter := '|';
    if cmdLine.RunProcess(fGitCommand + cmd, fTopLevelDir, cmdOut) = 0 then begin
      list.Text := cmdOut;
      for i:=0 to list.Count-1 do begin
        BranchLine.DelimitedText := list[i];
        if pos('/', branchLine[0])<>0 then
          continue;

        if popBranch.Items.Count=1 then begin
          mi := TMenuItem.Create(Self);
          mi.Caption := '-';
          mi.Tag := -1;
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
          mi.OnClick := @OnBranchClick;
        mi.Tag := 1;
        mi.RadioItem := true;
        popBranch.Items.Add(mi);

      end;
    end else
      DebugLn('Unable to get branch list, error: ', cmdLine.ErrorLog);
  finally
    list.free;
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
  aFont: string;
  aQuality: TFontQuality;
begin
  fUntrackedMode := 'all';
  fIgnoredMode := 'no';

  fGitCommand := fConfig.ReadString('git');
  if (fGitCommand='') or (not FileExists(fGitCommand)) then begin
    fGitCommand := FindDefaultExecutablePath('git' + EXE_EXTENSION);
    if fGitCommand<>'' then begin
      if not TryGitIn(ExtractFilePath(fGitCommand)) then
        fGitCommand := '';
    end;
  end;
  {$ifdef MsWindows}
  if (fGitCommand='') then begin
    // try some known git locations
    if not TryGitIn(GetEnvironmentVariable('ProgramFiles') + '\git\bin\') then
    if not TryGitIn(GetEnvironmentVariable('ProgramW6432') + '\git\bin\') then
    if not TryGitIn(GetEnvironmentVariable('SystemDrive') + '\msysgit\bin\') then
    if not TryGitIn(GetEnvironmentVariable('HOMEDRIVE') + '\msysgit\bin\') then
      ;
  end;
  {$endif}

  if fGitCommand='' then begin
    DebugLn('Error: Could not find git command');
    Application.Terminate;
    exit;
  end;

  //DebugLn('git=', fGitCommand);
  fEntries := TFpList.Create;

  txtDiff.Clear;

  with txtDiff.Font do begin
    aFont := Name;
    aQuality := Quality;
    {$ifdef Darwin}
    aFont := 'Menlo';
    aQuality := fqAntialiased;
    {$endif}
    {$ifdef MsWindows}
    aFont := 'Courier New';
    {$endif}
    Name := fConfig.ReadString('font.name', aFont, 'Viewer');
    Size := fConfig.ReadInteger('font.size', 10, 'Viewer');
    if fConfig.ReadBoolean('font.antialiased', aQuality=fqAntialiased, 'Viewer') then
      Quality := fqAntialiased
    else
      Quality := fqNonAntialiased;
  end;

  panFileState.Caption := '';

  RestoreGui;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveGui;
end;

procedure TfrmMain.actRescanExecute(Sender: TObject);
begin
  GitStatus;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Clear;
end;

procedure TfrmMain.OpenDirectory(aDir: string);
var
  cmdOut: RawByteString;
begin
  aDir := ExpandFileName(aDir);
  if (fTopLevelDir='') or (pos(fTopLevelDir, aDir)<>1) then begin
    if DirectoryExists(aDir + '.git') then
      fTopLevelDir := IncludeTrailingPathDelimiter(aDir)
    else begin
      if cmdLine.RunProcess(fGitCommand + ' rev-parse --show-toplevel', aDir, cmdOut)<>0 then
        DebugLn('Error getting top level directory: (%d) %s', [cmdLine.ExitCode, cmdLine.ErrorLog])
      else
        fTopLevelDir := IncludeTrailingPathDelimiter(SetDirSeparators(Trim(cmdOut)));
    end;
  end;
  fDir := aDir;
  GitStatus;
end;

procedure TfrmMain.DoItemAction(Data: PtrInt);
begin
  ItemAction(TListBox(Data), fClickedIndex);
end;

procedure TfrmMain.DoGitDiff(Data: PtrInt);
begin
  GitDiff(TObject(Data));
end;

procedure TfrmMain.GitStatus;
var
  aCommand: string;
  M: TMemoryStream;
  head, tail: PChar;
begin
  DebugLn('Status ----------------------------------------------');
  M := TMemoryStream.Create;
  try
    aCommand := format('%s status -b --long --porcelain=2 --ahead-behind --ignored=%s --untracked-files=%s -z',
      [fGitCommand, fIgnoredMode, fUntrackedMode]);

    cmdLine.RunProcess(aCommand, fDir, M);
    head := M.Memory;
    tail := head + M.Size;
    fMergingConflict := false;
    fMerging := GitMerging;
    GitStatusBranch(head, tail);
    GitStatusFiles(head, tail);
    UpdateBranch;
  finally
    M.Free;
  end;
end;

function FindPattern(var head:pchar; tail: pchar; pattern: string): boolean;
var
  len: SizeInt;
  p: pchar;
begin
  result := false;
  while head<tail do begin
    p := strpos(head, pchar(pattern));
    if p<>nil then begin
      head := p;
      result := true;
      break;
    end;
    len := strlen(head);
    head := head + len + 1;
  end;
end;

procedure TfrmMain.GitStatusBranch(var head: pchar; tail: pchar);
var
  ab: string;
  i, n: Integer;
begin
  fBranch := '';
  fUpstream := '';
  fCommitsAhead := 0;
  fCommitsBehind := 0;
  ab := '';

  // scan header lines
  while (head<tail) and (head^='#') do begin

    n := strlen(head);

    if (fBranch='') and (strlcomp(head, '# branch.head', 13)=0) then begin
      SetString(fBranch, head + 14, n - 14);
    end else
    if (fUpstream='') and (strlcomp(head, '# branch.upstream', 17)=0) then begin
      SetString(fUpstream, head + 18, n - 18);
    end else
    if (ab='') and (strlcomp(head, '# branch.ab', 11)=0) then begin
      SetString(ab, head + 12, n - 12);
      i := pos(' ', ab);
      fCommitsAhead := StrToIntDef(copy(ab, 1, i-1), 0);
      fCommitsBehind := StrToIntDef(copy(ab, i+1, Length(ab)), 0);
    end;

    inc(head, n + 1);
  end;
end;

procedure TfrmMain.GitStatusFiles(var head: pchar; tail: pchar);
var
  n: Integer;
  entry: PFileEntry;
  start: pchar;
begin
  // clear lists
  Clear;

  // scan header lines
  while (head<tail) do begin

    start := head;
    n := strlen(head);
    DebugLn(start);

    case head^ of
      '1': ParseOrdinaryChanged(head, tail, entry);
      '2': ParseRenamedCopied(head, tail, entry);
      'u':
        begin
          fMergingConflict := true;
          ParseUnmerged(head, tail, entry);
        end;
      '?',
      '!': ParseOther(head, tail, entry);
      else entry := nil;
    end;

    if entry<>nil then begin
      fEntries.Add(entry);

      // staged list
      case entry^.EntryTypeStaged of
        etUpdatedInIndex..etDeletedFromIndex:
          lstStaged.Items.AddObject(entry^.path, TObject(entry));
        etRenamedInIndex..etCopiedInIndexD:
          lstStaged.Items.AddObject(entry^.origPath + ' -> ' + entry^.path, TObject(entry));
      end;

      // unstaged list
      case entry^.EntryTypeUnStaged of
        etUnknown:;
        etUpdatedInIndex..etCopiedInIndexD:;
        etIndexAndWorktreeMatchesM..etIndexAndWorktreeMatchesC:;
        else
          lstUnstaged.Items.AddObject(entry^.path, TObject(entry));
      end;

    end;

    head := start + n + 1;
  end;
end;

procedure TfrmMain.UpdateBranch;
var
  s: string;
  ahead, behind: boolean;
begin
  ahead := fCommitsAhead>0;
  behind := fCommitsBehind<0;

  label1.Visible := not ahead and not behind;
  lblBranch.Caption := fBranch;
  s := '';
  if fMerging then begin
    s += '(MERGING';
    if fMergingConflict then s += ' CONFLICT';
    s += ')';
  end;
  lblMerging.Caption := s;

  s := '';
  if ahead then s += format('%d commits ahead', [fCommitsAhead]);
  if ahead and behind then s += ', ';
  if behind then s += format('%d commits behind', [-fCommitsBehind]);
  if ahead or behind then s += ' of';

  if s='' then s:=' ';
  lblAheadBehind.Caption := s;

  label2.Visible := (not ahead and not behind) and (fUpstream<>'');
  lblRemote.Caption := fUpstream;

  Caption := '(' + fTopLevelDir + ')';

end;

procedure TfrmMain.Clear;
var
  i: Integer;
  entry: PFileEntry;
begin
  if fEntries<>nil then begin
    for i:=0 to fEntries.Count-1 do begin
      entry := PFileEntry(fEntries[i]);
      if entry<>nil then
        Dispose(entry)
    end;
    fEntries.Clear;
  end;
  lstUnstaged.Clear;
  lstStaged.Clear;
end;

function TfrmMain.TryGitIn(aPath: string): boolean;
var
  outputStr: RawByteString;
begin
  aPath := aPath + 'git' + EXE_EXTENSION;
  result := FileExists(aPath);
  if result then begin
    cmdLine.RunProcess(aPath + ' --version', GetCurrentDir, outputStr);
    result := pos('git version', outputStr)=1;
    if result then begin
      fGitCommand := aPath;
      fConfig.WriteString('git', fGitCommand);
    end;
  end;
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

procedure TfrmMain.GitDiff(Sender: TObject);
var
  aCommand, arg: string;
  M: TMemoryStream;
  head, tail: PChar;
  srcUnstaged: boolean;
  Entry: PFileEntry;
  aIndex: Integer;
begin
  srcUnstaged := Sender=lstUnstaged;
  aIndex := TListBox(Sender).ItemIndex;
  if aIndex<0 then exit;
  Entry := PFileEntry(TListBox(Sender).Items.Objects[aIndex]);
  panFileState.Caption := EntryTypeToStr(Entry^.x, Entry^.y);

  M := TMemoryStream.Create;
  try
    if srcUnstaged then arg := ''
    else                arg := '--cached ';
    aCommand := format('%s diff %s%s', [fGitCommand, arg, Entry^.path]);
    cmdLine.waitOnExit := true;
    aIndex := cmdLine.RunProcess(aCommand, fTopLevelDir, M);
    if aIndex<>0 then begin
      txtDiff.Text := format('Error getting diff: %d',[aIndex]);
    end else begin
      M.Position := 0;
      txtDiff.Lines.LoadFromStream(M);
    end;
  finally
    M.Free;
  end;
end;

end.

