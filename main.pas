unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, SynEdit, FileUtil, unitconfig, unitprocess;

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
    procedure GitStatusBranch(aList: TStrings);
    procedure GitStatusFiles(aList: TStrings);
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
begin
  aList := TStringList.Create;
  try
    aCommand := format('%s status -b --long --porcelain=2 --ahead-behind --ignored=%s --untracked-files=%s',
      [fGitCommand, fIgnoredMode, fUntrackedMode]);
    RunProcess(aCommand, fDir, aList);
    GitStatusBranch(aList);
    GitStatusFiles(aList);
  finally
    aList.Free;
  end;
end;

procedure TfrmMain.GitStatusBranch(aList: TStrings);
var
  s: string;
  i, n: Integer;
begin
  fBranch := '';
  fUpstream := '';
  fCommitsAhead := 0;
  fCommitsBehind := 0;
  if (aList<>nil) then
    for i:=0 to aList.Count do begin
      s := aList[i];
      if (fBranch='') and (pos('# branch.head', s)=1) then
        fBranch := copy(s, 15, 255)
      else
      if (fUpstream='') and (pos('# branch.upstream', s)=1) then
        fUpstream := copy(s, 19, 255)
      else
      if pos('# branch.ab', s)=1 then begin
        s := copy(s, 13, 255);
        n := pos(' ', s);
        fCommitsAhead := StrToIntDef(copy(s, 1, n-1), 0);
        fCommitsBehind := StrToIntDef(copy(s, n+1, Length(s)), 0);
        break;
      end;
    end;
  UpdateBranch;
end;

procedure TfrmMain.GitStatusFiles(aList: TStrings);
var
  i: Integer;
  s: string;
begin
  //WriteLn('Getting status files from: ');
  //WriteLn(aList.Text);
  for i:=4 to aList.Count-1 do begin
    s := aList[i];
    if s='' then
      continue;
    case s[1] of
      '1': // changed tracked entries
        begin

        end;
      '2': // renamed or copied entries
        begin

        end;
      'u': // unmerged entries
        begin

        end;
    end;
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

