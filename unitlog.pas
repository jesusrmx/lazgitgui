{ LazGitGui: An interface to git status with some additional tools
             and with a familiar git gui interface.

  Copyright (C) 2023 Jesus Reyes Aguilar (jesusrmx@gmail.com)

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.

  This shows the textual representation of the log.
}

unit unitlog;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

{.$define CaptureOutput}
{.$define Capture}
{.$define CaptureChunks}

interface

uses
  Classes, SysUtils, Math, DateUtils, LazLogger, SynEdit,
  unitansiescapes, unitifaces, unitgitmgr, unitprocess, unitruncmd, unitlogcache;

type
  TRunThreadEvent = procedure(sender: TObject; thread: TRunThread; event: Integer; var interrupt: boolean) of object;

  { TLogHandler }

  TLogHandler = class
  private
    fGit: IGit;
    fEdit: TSynEdit;
    fAnsiHandler: TAnsiEscapesHandler;
    fGitMgr: TGitMgr;
    flogEvent: TRunThreadEvent;
    procedure LogDone(Sender: TObject);
    procedure LogOutput(sender: TObject; var interrupt: boolean);
    procedure SetGitMgr(AValue: TGitMgr);
    {$IFDEF CaptureOutput}
    fCap: TMemoryStream;
    {$ENDIF}
  public
    constructor create(theEditor: TSynEdit; aLogEvent: TRunThreadEvent);
    destructor Destroy; override;
    procedure ShowLog;

    property GitMgr: TGitMgr read fGitMgr write SetGitMgr;
  end;

implementation

const
  LOG_CMD = '--pretty="format:%ct'#2'%p'#2'%h'#2'%an'#2'%ae'#2'%D'#2'%s'#2#3'" --all';

procedure AppendFile(toFile, fromFile: string);
var
  fTo, fFrom: TFileStream;
begin
  fTo := TFileStream.Create(toFile, fmOpenWrite + fmShareDenyWrite);
  fFrom := TFileStream.Create(fromFile, fmOpenRead + fmShareDenyNone);
  try
    fTo.Seek(0, soFromEnd);
    fTo.CopyFrom(fFrom, fFrom.Size);
  finally
    fFrom.Free;
    fTo.Free;
  end;
end;

{ TLogHandler }

procedure TLogHandler.LogOutput(sender: TObject; var interrupt: boolean);
var
  thread: TRunThread absolute sender;
begin
  {$IFDEF CaptureOutput}
  if thread.Line<>'' then
    fCap.WriteBuffer(thread.Line[1], Length(thread.Line));
  fCap.WriteBuffer(thread.LineEnding[1], Length(thread.LineEnding));
  {$ENDIF}

  // check if it has been interrupted
  fLogEvent(self, thread, LOGEVENT_OUTPUT, interrupt);
  if interrupt then
    exit;

  fAnsiHandler.ProcessLine(thread.Line, thread.LineEnding);
end;

procedure TLogHandler.SetGitMgr(AValue: TGitMgr);
begin
  if fGitMgr = AValue then Exit;
  fGitMgr := AValue;
  fGit := fGitMgr.Git;
end;

procedure TLogHandler.LogDone(Sender: TObject);
var
  thread: TRunThread absolute Sender;
  dummy: boolean;
begin
  {$IFDEF CaptureOutput}
  fCap.SaveToFile('colorido.bin');
  fCap.Free;
  {$endif}
  dummy := false;
  fLogEvent(Self, thread, LOGEVENT_DONE, dummy);
end;

constructor TLogHandler.create(theEditor: TSynEdit; aLogEvent: TRunThreadEvent);
begin
  inherited Create;
  fEdit := theEditor;
  fAnsiHandler := TAnsiEscapesHandler.Create(fEdit);
  fLogEvent := aLogEvent;
end;

destructor TLogHandler.Destroy;
begin
  fAnsiHandler.Free;
  inherited Destroy;
end;

procedure TLogHandler.ShowLog;
var
  cmd: string;
begin
  fEdit.Clear;
  fAnsiHandler.Reset;

  cmd :=  fGit.Exe + ' ' +
          '-c color.ui=always ' +
          'log --oneline --graph --decorate --all';
  RunInThread(cmd, fGit.TopLevelDir, @LogOutput, @LogDone, true);
end;

end.

