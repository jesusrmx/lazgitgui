unit unitprocess;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, Process, UTF8Process;

type
  TOutputEvent = procedure(const aBuffer; aSize:longint) is nested;

  procedure RunProcess(const aCommand, startDir: string; callback:TOutputEvent); overload;
  procedure RunProcess(const aCommand, startDir: string; cmdOutput: TStrings); overload;
  procedure RunProcess(const aCommand, startDir: string; stream: TStream); overload;
  procedure RunProcess(const aCommand, startDir: string; out cmdOutput: RawByteString); overload;

implementation

const
  BUFSIZE = 1024 * 2;

procedure RunProcess(const aCommand, startDir: string; callback:TOutputEvent);
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
    Process.Options := [poUsePipes, poNoConsole];
    Process.Execute;
    repeat
      BytesRead := Process.Output.Read(Buffer^, BUFSIZE);
      CallBack(Buffer^, BytesRead);
    until BytesRead=0;
  finally
    Process.Free;
    FreeMem(Buffer);
  end;
end;

procedure RunProcess(const aCommand, startDir: string; cmdOutput: TStrings);
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    RunProcess(aCommand, startDir, M);
    M.Position := 0;
    cmdOutput.LoadFromStream(M);
  finally
    M.Free;
  end;

end;

procedure RunProcess(const aCommand, startDir: string; stream: TStream);
  procedure CollectOutput(const buffer; size:Longint);
  begin
    stream.WriteBuffer(Buffer, size);
  end;
begin
  RunProcess(aCommand, startDir, @CollectOutput);
end;

procedure RunProcess(const aCommand, startDir: string; out
  cmdOutput: RawByteString);
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    RunProcess(aCommand, startDir, M);
    SetString(cmdOutput, M.Memory, M.Size);
  finally
    M.Free;
  end;
end;

end.

