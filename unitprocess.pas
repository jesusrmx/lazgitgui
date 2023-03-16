unit unitprocess;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}
{.$define Debug}

interface

uses
  Classes, SysUtils, Process, UTF8Process, LazLogger;

type
  TOutputEvent = procedure(const aBuffer; aSize:longint) is nested;

  { TCmdLine }

  TCmdLine = object
  private
    fStdErrorClosed: boolean;
    fStdInputClosed: boolean;
    fStdOutputClosed: boolean;
    fWaitOnExit: boolean;
    fErrorLog: string;
    fLastCommand: string;
    fExitCode: Integer;
  public
    function RunProcess(const aCommand, startDir: string; callback:TOutputEvent): Integer; overload;
    function RunProcess(const aCommand, startDir: string; cmdOutput: TStrings): Integer; overload;
    function RunProcess(const aCommand, startDir: string; stream: TStream): Integer; overload;
    function RunProcess(const aCommand, startDir: string; out cmdOutput: RawByteString): Integer; overload;

    property ErrorLog: string read fErrorLog;
    property ExitCode: Integer read fExitCode;
    property LastCommand: string read fLastCommand;
    // these properties are disabled automatically after the next command is executed
    property WaitOnExit: boolean read fWaitOnExit write fWaitOnExit;
    property StdErrorClosed: boolean read fStdErrorClosed write fStdErrorClosed;
    property StdOutputClosed: boolean read fStdOutputClosed write fStdOutputClosed;
    property StdInputClosed: boolean read fStdInputClosed write fStdInputClosed;
  end;

var
  cmdLine: TCmdLine;

implementation

const
  BUFSIZE = 1024 * 2;

function TCmdLine.RunProcess(const aCommand, startDir: string; callback: TOutputEvent
  ): Integer;
var
  Process: TProcessUTF8;
  Buffer, Tail: PByte;
  BytesRead: LongInt;
  opts: TProcessOptions;
  Err: TStringStream;
begin

  if not Assigned(callback) then
    raise Exception.Create('RunProcess without callback');

  fLastCommand := aCommand;
  Err := TStringStream.Create('');
  Process := TProcessUTF8.Create(nil);
  GetMem(Buffer, BUFSIZE + 1);
  Tail := Buffer + BUFSIZE;
  Tail^ := 0;
  try
    Process.ParseCmdLine(aCommand);
    Process.CurrentDirectory := startDir;
    opts := [poUsePipes, poNoConsole];
    if fWaitOnExit then Include(opts, poWaitOnExit);
    Process.Options := opts;
    if StdErrorClosed then Process.CloseStderr;
    if StdOutputClosed then Process.CloseOutput;
    if StdInputClosed then Process.CloseInput;
    {$IFDEF DEBUG}
    DebugLn('Command: ', aCommand);
    DebugLn('CurrentDir: ', startDir);
    {$ENDIF}
    Process.Execute;

    if not StdOutputClosed then
      repeat
        BytesRead := Process.Output.Read(Buffer^, BUFSIZE);
        Callback(Buffer^, BytesRead);
      until BytesRead=0;

    // collect any reported error
    if not StdErrorClosed then begin
      while Process.StdErr.NumBytesAvailable>0 do begin
        BytesRead := Process.Stderr.Read(Buffer^, BUFSIZE);
        Err.WriteBuffer(Buffer^, BytesRead);
      end;
      fErrorLog := Err.DataString;
    end;

    {$IFDEF DEBUG}
    DebugLn('Exit: Status=%d Code=%d', [Process.ExitStatus, Process.ExitCode]);
    DebugLn('StdErr: %s',[fErrorLog]);
    {$ENDIF}

    {$ifdef MSWindows}
    fExitCode := Process.ExitCode;
    {$else}
    fExitCode := Process.ExitStatus;
    {$endif}
    result := fExitCode;
  finally
    Process.Free;
    FreeMem(Buffer);
    Err.Free;
    fWaitOnExit := false;
    fStdErrorClosed := false;
    fStdOutputClosed := false;
    fStdInputClosed := false;
  end;
end;

function TCmdLine.RunProcess(const aCommand, startDir: string; cmdOutput: TStrings
  ): Integer;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    result := RunProcess(aCommand, startDir, M);
    M.Position := 0;
    cmdOutput.LoadFromStream(M);
  finally
    M.Free;
  end;

end;

function TCmdLine.RunProcess(const aCommand, startDir: string; stream: TStream): Integer;
  procedure CollectOutput(const buffer; size:Longint);
  begin
    stream.WriteBuffer(Buffer, size);
  end;
begin
  result := RunProcess(aCommand, startDir, @CollectOutput);
end;

function TCmdLine.RunProcess(const aCommand, startDir: string; out
  cmdOutput: RawByteString): Integer;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    cmdOutput := '';
    result := RunProcess(aCommand, startDir, M);
    SetString(cmdOutput, M.Memory, M.Size);
  finally
    M.Free;
  end;
end;

end.

