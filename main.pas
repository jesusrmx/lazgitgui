unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, SynEdit, FileUtil, unitconfig, unitprocess, unitentries;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnRescan: TButton;
    btnStageChanged: TButton;
    btnSignOff: TButton;
    btnCommit: TButton;
    btnPush: TButton;
    ListBox1: TListBox;
    ListBox2: TListBox;
    panCommitState: TPanel;
    panBranch: TPanel;
    panFileState: TPanel;
    panUnstaged: TPanel;
    panStagedContainer: TPanel;
    panStaged: TPanel;
    txtComment: TMemo;
    panLeft: TPanel;
    panContent: TPanel;
    panCommit: TPanel;
    panCommitButtons: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    txtDiff: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    procedure OpenDirectory(aDir: string);
    procedure GitStatus;
    procedure GitStatusBranch(var head: pchar; tail: pchar);
    procedure GitStatusFiles(var head: pchar; tail: pchar);
    procedure UpdateBranch;
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

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fUntrackedMode := 'normal';
  fIgnoredMode := 'no';

  fGitCommand := fConfig.ReadString('git');
  if (fGitCommand='') or (not FileExists(fGitCommand)) then begin
    fGitCommand := FindDefaultExecutablePath('git' + EXE_EXTENSION);
  end;

  //WriteLn('git=', fGitCommand);

end;

procedure TfrmMain.OpenDirectory(aDir: string);
begin
  fDir := aDir;
  GitStatus;
end;

procedure TfrmMain.GitStatus;
var
  aList: TStringlist;
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
  // scan header lines
  while (head<tail) do begin

    start := head;
    n := strlen(head);
    //WriteLn(start);

    case head^ of
      '1': ParseOrdinaryChanged(head, tail, entry);
      '2': ParseRenamedCopied(head, tail, entry);
      'u': ParseUnmerged(head, tail, entry);
      '?',
      '!': ParseOther(head, tail, entry);
      else entry := nil;
    end;

    if entry<>nil then begin
      // add entries

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
  s := '';
  if not ahead and not behind then
    s += 'Branch: ';
  s += fBranch;
  if ahead then s += format(' %d commits ahead', [fCommitsAhead]);
  if ahead and behind then s += '/';
  if behind then s += format(' %d commits behind', [-fCommitsBehind]);
  if ahead or behind then s += ' of';
  s += ' ';
  if not ahead and not behind then
    s += 'Upstream: ';
  s += fUpstream;
  panBranch.Caption :=  s;
end;

end.

