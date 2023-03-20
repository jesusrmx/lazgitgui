unit unitprocess;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}
{$define Debug}

interface

uses
  Classes, SysUtils, Process, UTF8Process, LazLogger;

type
  TOutputEvent = procedure(const aBuffer; aSize:longint) is nested;

  { TCmdLine }

  PCmdLine = ^TCmdLine;
  TCmdLine = object
  private
    fRedirStdErr: boolean;
    fStdErrorClosed: boolean;
    fStdInputClosed: boolean;
    fStdOutputClosed: boolean;
    fWaitOnExit: boolean;
    fErrorLog: string;
    fLastCommand: string;
    fExitCode: Integer;
  public

    constructor Init;

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
    property RedirStdErr: boolean read fRedirStdErr write fRedirStdErr;
  end;

  function SplitParameters(Params: string; ParamList: TStrings): boolean;

var
  cmdLine: TCmdLine;

implementation

const
  BUFSIZE = 1024 * 2;

// copied and modified from: LazUtils, LazFileUtils.SplitCmdLineParams
// todo: add a parameter to the original function for preserving quotes
// and \x pairs
function SplitParameters(Params: string; ParamList: TStrings): boolean;
// split spaces, quotes are parsed as single parameter and are preserved
// #0 is always end
type
  TMode = (mNormal,mApostrophe,mQuote);
var
  p: Integer;
  Mode: TMode;
  Param: String;
begin
  p:=1;
  while p<=length(Params) do
  begin
    // skip whitespace
    while (p<=length(Params)) and (Params[p] in [' ',#9,#10,#13]) do inc(p);
    if (p>length(Params)) or (Params[p]=#0) then
      break;
    //writeln('SplitCmdLineParams After Space p=',p,'=[',Params[p],']');
    // read param
    Param:='';
    Mode:=mNormal;
    while p<=length(Params) do
    begin
      case Params[p] of
      #0:
        break;
      '\':
        begin
          Param+=Params[p];
          inc(p);
          if (p>length(Params)) or (Params[p]=#0) then
            break;
          Param+=Params[p];
          inc(p);
        end;
      '''':
        begin
          Param+=Params[p];
          inc(p);
          case Mode of
          mNormal:
            Mode:=mApostrophe;
          mApostrophe:
            Mode:=mNormal;
          end;
        end;
      '"':
        begin
          Param+=Params[p];
          inc(p);
          case Mode of
          mNormal:
            Mode:=mQuote;
          mQuote:
            Mode:=mNormal;
          end;
        end;
      ' ',#9,#10,#13:
        begin
          if Mode=mNormal then break;
          Param+=Params[p];
          inc(p);
        end;
      else
        Param+=Params[p];
        inc(p);
      end;
    end;
    //writeln('SplitCmdLineParams Param=#'+Param+'#');
    ParamList.Add(Param);
  end;
  result := Mode=mNormal;
end;

constructor TCmdLine.Init;
begin
  fStdErrorClosed := false;
  fStdInputClosed := false;
  fStdOutputClosed := false;
  fWaitOnExit := false;
  fErrorLog := '';
  fLastCommand := '';
  fExitCode := 0;
  fRedirStdErr := false;
end;

function TCmdLine.RunProcess(const aCommand, startDir: string; callback: TOutputEvent): Integer;
var
  Process: TProcessUTF8;
  Buffer, Tail: PByte;
  BytesRead: LongInt;
  opts: TProcessOptions;
  Err: TStringStream;
  s: string;

  procedure ParseCommandLine;
  var
    List: TStringList;
  begin
    List:=TStringList.Create;
    try
      SplitParameters(aCommand, List);
      if List.Count>0 then begin
        Process.Executable:=List[0];
        List.Delete(0);
      end else begin
        Process.Executable:='';
      end;
      Process.Parameters.Assign(List);
    finally
      List.Free;
    end;
  end;

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
    {$ifdef MsWindows}
    ParseCommandLine;
    {$else}
    Process.ParseCmdLine(aCommand, true);
    {$endif}

    Process.CurrentDirectory := startDir;
    opts := [poUsePipes, poNoConsole];
    if fWaitOnExit then Include(opts, poWaitOnExit);
    if fRedirStdErr then Include(opts, poStderrToOutPut);
    Process.Options := opts;
    if StdErrorClosed then Process.CloseStderr;
    if StdOutputClosed then Process.CloseOutput;
    if StdInputClosed then Process.CloseInput;
    {$IFDEF DEBUG}
    DebugLn('Command: ', fLastCommand);
    DebugLn('Arguments: ');
    for s in Process.Parameters do
      DebugLn('  |', s,'|');
    DebugLn('CurrentDir: ', startDir);
    DebugLn('Options: Closed Streams: %s%s%s, redirStdErr=%s',[
      BoolToStr(StdErrorClosed,'StdERR ',''),
      BoolToStr(StdOutputClosed,'Output ',''),
      BoolToStr(StdInputClosed,'Input ',''),
      BoolToStr(RedirStdErr,'RedirStdErr ','')
    ]);
    {$ENDIF}
    Process.Execute;

    if not StdOutputClosed then
      repeat
        BytesRead := Process.Output.Read(Buffer^, BUFSIZE);
        Callback(Buffer^, BytesRead);
      until BytesRead=0;

    // collect any reported error
    if not StdErrorClosed and (not (poStderrToOutPut in Process.Options)) then begin
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

