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
    aCommand := format('%s status -b --long --porcelain=2 --ignored=%s --untracked-mode=%s',
      [fGitCommand, fIgnoredMode, fUntrackedMode]);
    RunProcess(aCommand, fDir, aList);
    GitStatusBranch(aList);
    GitStatusFiles(aList);
  finally
    aList.Free;
  end;
end;

procedure TfrmMain.GitStatusBranch(aList: TStrings);
begin
  fBranch := '';
  fUpstream := '';
  if (aList<>nil) and (aList.Count>2) then begin
    fBranch := copy(aList[1], 15, 255);
    fUpstream := copy(aList[2], 19, 255);
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
      '1':
        begin

        end;
    end;
  end;
end;

procedure TfrmMain.UpdateBranch;
begin
  panBranch.Caption := 'Branch: ' + fBranch + ' Upstream: ' + fUpstream;
end;

end.

