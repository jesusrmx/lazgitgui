unit unitprocess;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, Process, UTF8Process, LazLogger;

type
  TOutputEvent = procedure(const aBuffer; aSize:longint) is nested;

  TCmdLine = object
  private
    fWaitOnExit: boolean;
    fErrorLog: string;
    fLastCommand: string;
    fExitCode: Integer;
  public
    function RunProcess(const aCommand, startDir: string; callback:TOutputEvent): Integer; overload;
    function RunProcess(const aCommand, startDir: string; cmdOutput: TStrings): Integer; overload;
    function RunProcess(const aCommand, startDir: string; stream: TStream): Integer; overload;
    function RunProcess(const aCommand, startDir: string; out cmdOutput: RawByteString): Integer; overload;

    property WaitOnExit: boolean read fWaitOnExit write fWaitOnExit;
    property ErrorLog: string read fErrorLog;
    property ExitCode: Integer read fExitCode;
    property LastCommand: string read fLastCommand;
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
    if waitOnExit then Include(opts, poWaitOnExit);
    Process.Options := opts;
    DebugLn('  ExecName: ', Process.Executable);
    DebugLn('Parameters: ', Process.Parameters.CommaText);
    DEbugLn('CurrentDir: ', startDir);
    Process.Execute;
    repeat
      BytesRead := Process.Output.Read(Buffer^, BUFSIZE);
      Callback(Buffer^, BytesRead);
    until BytesRead=0;

    // collect any reported error
    while Process.StdErr.NumBytesAvailable>0 do begin
      BytesRead := Process.Stderr.Read(Buffer^, BUFSIZE);
      Err.WriteBuffer(Buffer^, BytesRead);
    end;
    fErrorLog := Err.DataString;

    DebugLn('Exit: Status=%d Code=%d', [Process.ExitStatus, Process.ExitCode]);
    DebugLn('StdErr: %s',[fErrorLog]);

    {$ifdef MSWindows}
    fExitCode := Process.ExitCode;
    {$else}
    fExitCode := Process.ExitStatus;
    {$endif}
    result := fExitCode;
  finally
    fWaitOnExit := false;
    Process.Free;
    FreeMem(Buffer);
    Err.Free;
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

