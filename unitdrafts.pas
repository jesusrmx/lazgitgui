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
  Classes, SysUtils, StrUtils, LazLogger, lazfileutils,
  unitconfig, unitprocess, unitvfs, unittextchunks;

  procedure AnalizeColumns;
  procedure TestParams;
  procedure TestVfs;
  procedure TestLinks;

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
  links=2
  link1=Bug tracker links&sep;#(\d{1,5})&sep;https://sitio/$1&sep;open
  link2=url links&sep;(?:https?):\/\/(?:[^\s])+&sep;&sep;open
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

end.

