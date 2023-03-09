unit unitprocess;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, Process, UTF8Process, LazLogger;

type
  TOutputEvent = procedure(const aBuffer; aSize:longint) is nested;

  function RunProcess(const aCommand, startDir: string; callback:TOutputEvent): Integer; overload;
  function RunProcess(const aCommand, startDir: string; cmdOutput: TStrings): Integer; overload;
  function RunProcess(const aCommand, startDir: string; stream: TStream): Integer; overload;
  function RunProcess(const aCommand, startDir: string; out cmdOutput: RawByteString): Integer; overload;

implementation

const
  BUFSIZE = 1024 * 2;

function RunProcess(const aCommand, startDir: string; callback: TOutputEvent
  ): Integer;
var
  Process: TProcessUTF8;
  Buffer, Tail: PByte;
  BytesRead: LongInt;
begin

  if not Assigned(callback) then
    raise Exception.Create('RunProcess without callback');

  Process := TProcessUTF8.Create(nil);
  GetMem(Buffer, BUFSIZE + 1);
  Tail := Buffer + BUFSIZE;
  Tail^ := 0;
  try
    Process.ParseCmdLine(aCommand);
    Process.CurrentDirectory := startDir;
    Process.Options := [poUsePipes, poNoConsole, poWaitOnExit];
    DebugLn('ApplicationName: ', Process.Executable);
    DebugLn('Parameters: ', Process.Parameters.CommaText);
    Process.Execute;
    repeat
      BytesRead := Process.Output.Read(Buffer^, BUFSIZE);
      CallBack(Buffer^, BytesRead);
    until BytesRead=0;
    DebugLn('Exit: Status=%d Code=%d', [Process.ExitStatus, Process.ExitCode]);
    {$ifdef MSWindows}
    result := Process.ExitCode;
    {$else}
    result := Process.ExitStatus;
    {$endif}
  finally
    Process.Free;
    FreeMem(Buffer);
  end;
end;

function RunProcess(const aCommand, startDir: string; cmdOutput: TStrings
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

function RunProcess(const aCommand, startDir: string; stream: TStream): Integer;
  procedure CollectOutput(const buffer; size:Longint);
  begin
    stream.WriteBuffer(Buffer, size);
  end;
begin
  result := RunProcess(aCommand, startDir, @CollectOutput);
end;

function RunProcess(const aCommand, startDir: string; out
  cmdOutput: RawByteString): Integer;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    result := RunProcess(aCommand, startDir, M);
    SetString(cmdOutput, M.Memory, M.Size);
  finally
    M.Free;
  end;
end;

end.

