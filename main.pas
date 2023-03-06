unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ActnList, SynEdit, SynHighlighterDiff, FileUtil, unitconfig, unitprocess,
  unitentries, Types, lclType;

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
    procedure lstUnstagedClick(Sender: TObject);
    procedure lstUnstagedDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    fBranch: String;
    fConfig: TConfig;
    fGitCommand: string;
    fDir: string;
    fUpstream: String;
    fUntrackedMode: string;
    fIgnoredMode: string;
    fCommitsAhead: Integer;
    fCommitsBehind: Integer;
    fEntries: TFPList;
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

procedure TfrmMain.lstUnstagedClick(Sender: TObject);
begin
  GitDiff(Sender);
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
  lb.Canvas.TextRect(aRect, aRect.Left + 22, aRect.Top, entry^.path);

  index := 0;
  if lb=lstUnstaged then
    case entry^.EntryTypeUnStaged of
      etWorktreeChangedSinceIndex..etWorktreeChangedSinceIndexT:  index := 6;
      etIgnored:                                                  index := 5;
    end
  else
    case entry^.EntryTypeStaged of
      etUpdatedInIndex..etUpdatedInIndexD:                        index := 7;
      etAddedToIndex..etAddedToIndexD:                            index := 2;
      etDeletedFromIndex:                                         index := 3;
      etCopiedInIndex..etCopiedInIndexD:                          index := 4;
    end;

  imgList.Draw(lb.Canvas, aRect.Left + 2, aRect.Top + 1, index);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
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
    WriteLn(StdErr, 'Could not find git command');
    Application.Terminate;
    exit;
  end;

  //WriteLn('git=', fGitCommand);
  fEntries := TFpList.Create;

  txtDiff.Clear;

  {$ifdef Darwin}
  with txtDiff.Font do begin
    Name := 'Menlo';
    Quality := fqAntialiased;
    size := 10;
  end;
  {$endif}

  RestoreGui;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveGui;
end;

procedure TfrmMain.actRescanExecute(Sender: TObject);
begin
  WriteLn('Rescan');
  GitStatus;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Clear;
end;

procedure TfrmMain.OpenDirectory(aDir: string);
begin
  fDir := aDir;
  GitStatus;
end;

procedure TfrmMain.GitStatus;
var
  aCommand: string;
  M: TMemoryStream;
  head, tail: PChar;
begin
  M := TMemoryStream.Create;
  try
    aCommand := format('%s status -b --long --porcelain=2 --ahead-behind --ignored=%s --untracked-files=%s -z',
      [fGitCommand, fIgnoredMode, fUntrackedMode]);

    RunProcess(aCommand, fDir, M);
    head := M.Memory;
    tail := head + M.Size;
    GitStatusBranch(head, tail);
    GitStatusFiles(head, tail);
    Caption := 'Current Directory: ' + ExpandFileName(fDir);
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

  UpdateBranch;
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
    WriteLn(start);

    case head^ of
      '1': ParseOrdinaryChanged(head, tail, entry);
      '2': ParseRenamedCopied(head, tail, entry);
      'u': ParseUnmerged(head, tail, entry);
      '?',
      '!': ParseOther(head, tail, entry);
      else entry := nil;
    end;

    if entry<>nil then begin
      fEntries.Add(entry);

      // staged list
      case entry^.EntryTypeStaged of
        etUpdatedInIndex..etCopiedInIndexD:
          lstStaged.Items.AddObject(entry^.path, TObject(entry));
      end;

      // unstaged list
      case entry^.EntryTypeUnStaged of
        etUpdatedInIndex..etCopiedInIndexD:;
        etIndexAndWorktreeMatchesM..etIndexAndWorktreeMatchesC:;
        else lstUnstaged.Items.AddObject(entry^.path, TObject(entry));
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
  if ahead then s += format('%d commits ahead', [fCommitsAhead]);
  if ahead and behind then s += ', ';
  if behind then s += format('%d commits behind', [-fCommitsBehind]);
  if ahead or behind then s += ' of';

  if s='' then s:=' ';
  lblAheadBehind.Caption := s;

  label2.Visible := not ahead and not behind;
  lblRemote.Caption := fUpstream;
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
    RunProcess(aPath + ' --version', GetCurrentDir, outputStr);
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

  M := TMemoryStream.Create;
  try
    if srcUnstaged then arg := ''
    else                arg := '--cached ';
    aCommand := format('%s diff %s%s', [fGitCommand, arg, Entry^.path]);
    RunProcess(aCommand, fDir, M);
    M.Position := 0;
    txtDiff.Lines.LoadFromStream(M);
  finally
    M.Free;
  end;
end;

end.

