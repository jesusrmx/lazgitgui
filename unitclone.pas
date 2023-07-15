unit unitclone;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, URIParser, RegExpr,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ButtonPanel, Buttons,
  Clipbrd, EditBtn,
  unitcommon, unitconfig, unitifaces, unitruncmd, unitgitmgr;

type

  { TfrmClone }

  TfrmClone = class(TForm)
    lblRepoName: TLabel;
    lblUrl: TLabel;
    lblDir: TLabel;
    txtRepoName: TEdit;
    panBtns: TButtonPanel;
    lblInfo: TLabel;
    btnLocalRepoDir: TSpeedButton;
    btnBrowseDir: TSpeedButton;
    selDir: TSelectDirectoryDialog;
    txtDir: TEdit;
    txtUrl: TEdit;
    procedure btnBrowseDirClick(Sender: TObject);
    procedure btnLocalRepoDirClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtUrlChange(Sender: TObject);
  private
    fCommand: string;
    fGitMgr: TGitMgr;
    fRepoName: String;
    fUrl: String;
    fUrlRegExpr: string;
    fCloneDir: string;
    procedure UpdateInfo;
    procedure Invalid(msg: string);
    function  UrlFromClipboard: string;
    function GetURL: string;
    function ProcessRegExUrl(aUrl: string): string;
    function IsValidUrl(aUrl: string; out aRepoName: string): boolean;
    function SelectDirectory: boolean;
  public
    property GitMgr: TGitMgr read fGitMgr write fGitMgr;
  end;

var
  frmClone: TfrmClone;

implementation

{$R *.lfm}

const
  URLREG_EXPRESSION = '((https?|git|ssh|file):\/\/(?:[^\s])+)';

{ TfrmClone }

procedure TfrmClone.FormCreate(Sender: TObject);
begin
  fConfig.ReadWindow(self, 'clonefrm', SECTION_GEOMETRY);
  fUrlRegExpr := fConfig.ReadString('CloneUrlRegExpr', URLREG_EXPRESSION);
  fCloneDir := fConfig.ReadString('CloneDir', GetUserDir);
  fRepoName := fConfig.ReadString('CloneRepo');
end;

procedure TfrmClone.FormShow(Sender: TObject);
var
  aUrl, aRepoName: String;
begin
  aUrl := UrlFromClipboard;
  if not IsValidUrl(aUrl, aRepoName) then begin
    aUrl := fConfig.ReadString('CloneUrl');
  end else
  if fRepoName='' then
    fRepoName := aRepoName;

  txtDir.OnChange := nil;
  txtDir.Text := fCloneDir;
  txtDir.OnChange := @txtUrlChange;

  txtUrl.OnChange := nil;
  txtUrl.Text := aUrl;
  txtUrl.OnChange := @txtUrlChange;

  txtRepoName.OnChange := nil;
  txtRepoName.Text := fRepoName;
  txtRepoName.OnChange := @txtUrlChange;

  UpdateInfo;
end;

procedure TfrmClone.txtUrlChange(Sender: TObject);
begin
  UpdateInfo;
end;

function TfrmClone.GetURL: string;
var
  aUrl: string;
begin

  aUrl := Trim(txtUrl.Text);
  result := ProcessRegExUrl(aUrl);

  if result<>aUrl then begin
    txtUrl.OnChange := nil;
    txtUrl.Text := result;
    txtUrl.OnChange := @txtUrlChange;
  end;

  if result.EndsWith('/') then
    delete(result, Length(result), 1);
end;

function TfrmClone.ProcessRegExUrl(aUrl: string): string;
var
  reg: TRegExpr;
begin
  reg := TRegExpr.Create(fUrlRegExpr);
  try
    reg.InputString := aUrl;
    if reg.Exec(1) then
      result := reg.Match[1]
    else
      result := aUrl;
  finally
    reg.Free;
  end;
end;

function TfrmClone.IsValidUrl(aUrl: string; out aRepoName: string): boolean;
var
  Uri: TURI;
begin
  aRepoName := '';

  Uri := ParseUri(aUrl);

  if (Uri.Protocol='file') then begin
    result := (Uri.Path<>'') and (Uri.Document<>'');
    exit;
  end;

  result := (Uri.Host<>'') and (Uri.Protocol<>'') and (uri.Path<>'');

  if Uri.Document<>'' then
    aRepoName := ChangeFileExt(uri.Document, '');
end;

function TfrmClone.SelectDirectory: boolean;
begin
  if selDir.Tag=0 then begin
    selDir.InitialDir := fCloneDir;
    selDir.Tag := 1;
  end;
  result := SelDir.Execute;
end;

procedure TfrmClone.UpdateInfo;
var
  s, aReponame, dir: String;
begin
  panBtns.OKButton.Enabled := false;

  dir := Trim(txtDir.Text);
  while dir.EndsWith('/') do
    delete(dir, Length(dir), 1);
  fCloneDir := ExtractFilePath(dir);

  fCommand := 'clone --progress';

  s := GetUrl;
  if s='' then begin
    Invalid(rsTheRepositoryURLIsEmpty);
    exit;
  end;

  if not isValidURL(s, aReponame) then begin
    Invalid(rsInvalidCloneURL);
    exit;
  end;

  fUrl := s;

  if fCloneDir='' then begin
    Invalid(rsInvalidDirectory);
    exit;
  end;

  if fRepoName='' then begin
    Invalid(rsInvalidRepositoryName);
    exit;
  end;

  fCommand += ' ' + fUrl + ' ' + fRepoName;

  lblInfo.Font.Color := clBlack;
  lblInfo.Caption := 'git ' + fCommand + LineEnding +
                     rsInto + ExcludeTrailingPathDelimiter(fCloneDir);

  panBtns.OKButton.Enabled := true;
end;

procedure TfrmClone.Invalid(msg: string);
begin
  lblInfo.Font.color := clRed;
  lblInfo.Caption := msg;
end;

function TfrmClone.UrlFromClipboard: string;
begin

  result := Clipboard.AsText;
  if result='' then
    exit;

  result := ProcessRegExUrl(result);
end;

procedure TfrmClone.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  aDir: string;
begin
  fConfig.WriteWindow(Self, 'clonefrm', SECTION_GEOMETRY);
  if ModalResult=mrOk then begin

    aDir := ExtractFilePath(fCloneDir);
    if not FileExists(aDir) then
      ForceDirectories(aDir);

    if RunInteractive(fGitMgr.Git.Exe + ' ' + fCommand, aDir, 'Push with options', fCommand)<=0 then begin
      fConfig.WriteString('CloneUrl', fUrl);
      fConfig.WriteString('CloneDir', fCloneDir);
    end;
  end;
end;

procedure TfrmClone.btnBrowseDirClick(Sender: TObject);
begin
  if SelectDirectory then
    txtDir.Text := IncludeTrailingPathDelimiter(selDir.FileName);
end;

procedure TfrmClone.btnLocalRepoDirClick(Sender: TObject);
begin
  if SelectDirectory then
    txtUrl.Text := FileNameToUri(IncludeTrailingPathDelimiter(selDir.FileName), false);
end;

end.

