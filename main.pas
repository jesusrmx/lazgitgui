unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SynEdit;

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
    procedure FormShow(Sender: TObject);
  private
    fTargetDir: String;
    procedure OpenDirectory(aDir: string);
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormShow(Sender: TObject);
var
  aDir: string;
begin
  if ParamCount>0 then begin
    aDir := ParamStr(ParamCount);
    if not DirectoryExists(aDir) then begin
       ShowMessage('Directory '+aDir+' does not exists');
       exit;
    end;
    OpenDirectory(aDir);
  end;
end;

procedure TfrmMain.OpenDirectory(aDir: string);
begin
  fTargetDir := aDir;
  ShowMessage('Updating repository data for: '+aDir);
end;

end.

