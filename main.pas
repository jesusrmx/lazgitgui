unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTF8Process, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, SynEdit, FileUtil, unitconfig;

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
    fConfig: TConfig;
    fGitCommand: string;
    procedure OpenDirectory(aDir: string);
    procedure GitStatus(aDir: string);
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

  fGitCommand := fConfig.ReadString('git');
  if (fGitCommand='') or (not FileExists(fGitCommand)) then begin
    fGitCommand := FindDefaultExecutablePath('git' + EXE_EXTENSION);
  end;

end;

procedure TfrmMain.OpenDirectory(aDir: string);
begin
  GitStatus(aDir);
end;

procedure TfrmMain.GitStatus(aDir: string);
var
  process: TProcessUTF8;
  Params: TStringlist;
begin
  //Git(fGitCommand, status -b -long --porcelain=2',

  Params := TStringList.Create;
  Params.Add('--C ');
  Params.Add('"'+aDir+'"');
  Params.Add('status');
  Params.Add('-b');
  Params.Add('--long');
  Params.Add('--porcelain=2');
  Process := TProcessUTF8.Create(nil);
  try
    Process.Executable := fGitCommand;
    Process.Parameters.Assign(Params);
    Process.Execute;
  finally
    Process.Free;
    Params.Free;
  end;

end;

end.

