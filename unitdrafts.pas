unit unitdrafts;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, StrUtils, LazLogger, lazfileutils, unitprocess, unitvfs;

  procedure AnalizeColumns;
  procedure TestParams;
  procedure TestVfs;

implementation

type
  TColumn = record
    first, last: Integer;
  end;
  TColumns = array of TColumn;

procedure AnalizeColumns;
var
  L: TStringList;
  i, j, k, n: Integer;
  columns: TColumns;
  aMin, aMax: Integer;
  colCount, maxcolCount: Integer;
  s, t: String;
  isMax: boolean;
begin
  L := TStringList.Create;
  try
    aMin := MaxInt;
    aMax := 0;
    L.LoadFromFile('columns.lazarus.txt');
    for i:=0 to L.Count-1 do begin
      s := L[i];
      j := pos('first=', s);
      k := pos('last=', s);
      if (j>0) and (k>0) then begin
        n := Length(columns);
        SetLength(columns, n+1);
        t := copy(s, j+6, k-(j+7));
        columns[n].first := StrToIntDef(t, -1);
        columns[n].last  := StrToIntDef(copy(s, k+5, Length(s)), -1);
        if columns[n].first<aMin then aMin := columns[n].first;
        if columns[n].last>aMax then aMax := columns[n].last;
      end;
    end;

    maxcolCount := 0;
    for i:=aMin to aMax do begin
      s := '';
      colCount := 0;
      for j:=0 to Length(Columns)-1 do begin
        if (i>=columns[j].first) and (i<=columns[j].last) then begin
          // index i belongs to this column
          s += format('c%.3d ',[j]);
          inc(colCount);
        end;
      end;
      isMax := false;
      if maxcolcount<colcount then begin
        maxColCount := colCount;
        isMax := true;
      end;
      DebugLn('%.6d %2d %s %s',[i, colCount, BoolToStr(isMax, 'MX', '  '), s]);
    end;

    DebugLn('Found %d columns, from %d to %d',[Length(columns), aMin, aMax]);
    DebugLn('from %d to %d maxColumns per index %d',[ aMin, aMax, maxcolCount]);

  finally
    L.Free;
  end;
end;

procedure Dump(msg: string; l: TStringList);
var
  s: string;
begin
  WriteLn(msg);
  for s in l do
    WriteLn('  |', s,'|');
  l.clear;
end;

procedure TestParams;
var
  l: TStringList;
  cmd, arg: string;
begin
  l := TStringList.Create;
  try
    WriteLn;
    {$ifdef MsWindows}
    Write('Simulating ');
    {$endif}
    WriteLn('Linux/MacOS');
    cmd := '/home/prog/files good/git.exe';
    arg := ' commit -m "This isn''t a \"text\" ''message'' of 1\" inch long"';
    SplitParameters(cmd + arg, l);
    dump('Using SplitParameters', l);
    SplitCmdLineParams(cmd + arg, l, true);
    dump('Using SplitCmdLineParams with ReadBackSlash=TRUE', l);
    SplitCmdLineParams(cmd + arg, l, false);
    dump('Using SplitCmdLineParams with ReadBackSlash=FALSE', l);

    WriteLn;
    {$ifndef MsWindows}
    Write('Simulating ');
    {$endif}
    WriteLn('Windows');
    cmd := 'c:\home\prog\files good\git.exe';
    SplitParameters(cmd + arg, l);
    dump('Using SplitParameters', l);
    SplitCmdLineParams(cmd + arg, l, true);
    dump('Using SplitCmdLineParams with ReadBackSlash=TRUE', l);
    SplitCmdLineParams(cmd + arg, l, false);
    dump('Using SplitCmdLineParams with ReadBackSlash=FALSE', l);
  finally
    l.Free;
  end;
end;

procedure TestVfs;

type
  PFileData = ^TFileData;
  TFileData = record
    Counter: integer;
  end;

var
  counter: Integer = 0;
  vfs: TVirtualFileSystem;
  fileData: PfileData;

  procedure OnNewNode(sender: TObject; aName: TvfsString; isDir:boolean; var data:pointer);
  begin
    data := nil;
    if not isDir then begin
      inc(counter);
      New(fileData);
      fileData^.Counter := counter;
      data := fileData;
    end;
  end;

  procedure OnDisposeNode(sender: TObject; aName: TvfsString; data: pointer);
  var
    p: PFileData;
  begin
    p := data;
    if p<>nil then
      Dispose(p);
  end;

begin
  vfs := TVirtualFileSystem.Create;
  vfs.OnNewNodeNested := @OnNewNode;
  vfs.OnDisposeNodeNested := @OnDisposeNode;
  try
    vfs.AddPath('file1.txt');
    vfs.AddPath('file2.txt');
    vfs.AddPath('file3.txt');
    vfs.AddPath('Abc/Lios/archivo.txt');
    vfs.AddPath('Abc/Lios/Otro/archivo1.txt');
    vfs.AddPath('Abc/Lios/archivo2.txt');
    vfs.AddPath('Abc/archivo3.txt');
    vfs.AddPath('archivo4.txt');

    vfs.Dump;
  finally
    vfs.Free;
  end;
end;

end.

