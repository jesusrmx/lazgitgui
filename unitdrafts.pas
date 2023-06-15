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

  Unit for doing misc tests.
}
unit unitdrafts;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, StrUtils, LazLogger, lazfileutils, RegExpr,
  unitconfig, unitprocess, unitvfs, unittextchunks;

  procedure AnalizeColumns;
  procedure TestParams;
  procedure TestVfs;
  procedure TestLinks;
  procedure TestRegExpr;

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

procedure TestLinks;
var
  fLinks: TTextLinks;
  chunks: TTextChunks;
  chunk: TTextChunksItem;
  s, aDest: string;
begin
  {
  Example config:
  delimiter=|
  links=3
  link1=Bug tracker links|#(\d{1,5})|https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/$1|open|clBlue
  link2=url links|(?:https?):\/\/(?:[^\s])+||open|clBlue
  link3=Commit Ids|\s([0-9a-fA-F]{6,})|$1|goto|clFuchsia
  }
  fConfig := TConfig.Create;
  fLinks := TTextLinks.Create;
  fLinks.LoadFromConfig('/home/prog/dev/lazarus/');
  fLinks.Dump;
  chunks := nil;
  //s := '#12345 es uno de los bugs: #65432, #98760 mencionados en https://sitio.org/path/x.html y mas';
  //s := 'one bug #65432, #98760 mencionados en https://sitio.org/path/x.html y #12345';
  s := 'https://sitio.org/path/x.html Uno de los bugs #12345, #65432, #98760 mencionados';
  DebugLn('TEXT: ',s);
  fLinks.FindLinks(s, chunks);
  for chunk in chunks do begin
    WriteStr(s, chunk.itemType);
    if (chunk.itemType=tcitLink) then begin
      aDest := chunk.linkDest;
      if aDest='' then
        aDest := chunk.text;
      DebugLn('LinkIndex=%2d type=%s %s -> %s (%s)',[chunk.linkIndex, QuotedStr(s), QuotedStr(chunk.text),aDest, chunk.linkAction])
    end else
      DebugLn('LinkIndex=%2d type=%s %s',[chunk.linkIndex, QuotedStr(s), QuotedStr(chunk.text)]);
  end;
  fLinks.Free;
  fConfig.Free;
end;

procedure TestRegExpr;
var
  s, expression, replace, sample: string;
  L: TStringList;
  i: Integer;
  reg: TRegExpr;
  offset, apos, alen: Integer;
  DEL:Char;
begin
  L := TStringList.Create;
  L.StrictDelimiter:=true;
  DEL := '|';
  //DEL := #2;
  L.Delimiter:=DEL;
  reg := TRegExpr.Create;
  try
    s := 'Commit Ids'+DEL+'\s([0-9a-fA-F]{6,})'+DEL+'$1'+DEL+'goto'+DEL+'clFuchsia';
    L.DelimitedText:=s;
    for i:=0 to L.Count-1 do
      DebugLn('%d: %s',[i, L[i]]);
    expression := L[1];
    replace := L[2];
    sample := 'Docs: LCL/lclintf. Adds a deprecation notice to the OffsetRect topic for changes in f3afdc8d.';
    DebugLn('Testing %s', [s]);
    DebugLn('With: %s',[sample]);
    DebugLn('Expression: %s', [expression]);
    DebugLn('Replace: %s',[replace]);
    reg.InputString:=sample;
    reg.Expression:=expression;
    offset := 1;
    while reg.Exec(offset) do begin
      apos := reg.MatchPos[0];
      aLen := reg.MatchLen[0];
      if replace<>'' then
        s := reg.Substitute(replace)
      else
        s := '';
      DebugLn('Match %s found at %d len=%d replace=%s',[QuotedStr(reg.Match[0]), aPos, aLen, Quotedstr(s)]);
      // try match more links past the previous one.
      offset :=  apos + aLen;
    end;

    {  RESULTS UNDER MACOS (M1)
    // Lazarus 2.3.0 (rev main-2_3-3436-gda12f4b7d5) FPC 3.2.2 aarch64-darwin-cocoa
    0: Commit Ids
    1: \s([0-9a-fA-F]{6,})
    2: $1
    3: goto
    4: clFuchsia
    Testing Commit Ids|\s([0-9a-fA-F]{6,})|$1|goto|clFuchsia
    With: Docs: LCL/lclintf. Adds a deprecation notice to the OffsetRect topic for changes in f3afdc8d.
    Expression: \s([0-9a-fA-F]{6,})
    Replace: $1
    Match ' ' found at 6 len=1 replace=''
    Match ' Add' found at 19 len=4 replace='Add'
    Match ' a' found at 24 len=2 replace='a'
    Match ' de' found at 26 len=3 replace='de'
    Match ' ' found at 38 len=1 replace=''
    Match ' ' found at 45 len=1 replace=''
    Match ' ' found at 48 len=1 replace=''
    Match ' ' found at 52 len=1 replace=''
    Match ' ' found at 63 len=1 replace=''
    Match ' f' found at 69 len=2 replace='f'
    Match ' c' found at 73 len=2 replace='c'
    Match ' ' found at 81 len=1 replace=''
    Match ' f3afdc8d' found at 84 len=9 replace='f3afdc8d'
    }

    { RESULTS UNDER WINDOWS
    // Lazarus 2.3.0 (rev Unversioned directory) FPC 3.2.2 i386-win32-win32/win64
    0: Commit Ids
    1: \s([0-9a-fA-F]{6,})
    2: $1
    3: goto
    4: clFuchsia
    Testing Commit Ids|\s([0-9a-fA-F]{6,})|$1|goto|clFuchsia
    With: Docs: LCL/lclintf. Adds a deprecation notice to the OffsetRect topic for changes in f3afdc8d.
    Expression: \s([0-9a-fA-F]{6,})
    Replace: $1
    Match ' f3afdc8d' found at 84 len=9 replace='f3afdc8d'
    }

    { RESULTS UNDER MACOS intel
    // Lazarus 2.3.0 (rev main-2_3-3608-gd939016d4e) FPC 3.2.2 x86_64-darwin-cocoa
    0: Commit Ids
    1: \s([0-9a-fA-F]{6,})
    2: $1
    3: goto
    4: clFuchsia
    Testing Commit Ids|\s([0-9a-fA-F]{6,})|$1|goto|clFuchsia
    With: Docs: LCL/lclintf. Adds a deprecation notice to the OffsetRect topic for changes in f3afdc8d.
    Expression: \s([0-9a-fA-F]{6,})
    Replace: $1
    Match ' f3afdc8d' found at 84 len=9 replace='f3afdc8d'
    }
    { RESULTS UNDER LINUX intel
    // Lazarus 2.3.0 (rev main-2_3-3997-gee82f625c3) FPC 3.2.3 x86_64-linux-gtk2
    0: Commit Ids
    1: \s([0-9a-fA-F]{6,})
    2: $1
    3: goto
    4: clFuchsia
    Testing Commit Ids|\s([0-9a-fA-F]{6,})|$1|goto|clFuchsia
    With: Docs: LCL/lclintf. Adds a deprecation notice to the OffsetRect topic for changes in f3afdc8d.
    Expression: \s([0-9a-fA-F]{6,})
    Replace: $1
    Match ' f3afdc8d' found at 84 len=9 replace='f3afdc8d'
    }
  finally
    L.Free;
  end;
end;

end.

