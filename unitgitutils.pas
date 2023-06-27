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

  Several utilities.
}

unit unitgitutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, LazLogger,
  unitgittypes, unitentries;

  function OIDToQWord(oid: string): QWord;
  function OIDToParents(oid: string; oidlen: Integer): TQWordArray;
  function OIDToParentElements(oid: string; oidlen: Integer): TParentElementArray;

  function QuoteMsg(msg: string): string;
  function MakePathList(entryArray: TPFileEntryArray; sanitizeItems:boolean=true): string;
  function Sanitize(aPath: RawbyteString; force:boolean=true): RawbyteString;
  function GitDateToDateTime(s: string): TDateTime;
  function DateTimeToGitFmt(d: TDateTime): string;

  procedure ResetTicks(globalToo:boolean=false);
  procedure ReportTicks(msg:string; globalToo:boolean=false);

  function PosAny(chars: TSetOfChar; s:string): Integer;
  function ReplaceEOLs(s: string; encode:boolean): string;
  function EncodeDelimitedText(delimiter:string; strings:TStrings): string;
  procedure DecodeDelimitedText(s:string; delimiter:string; strings:TStrings);

  procedure DumpIntArray(msg: string; arr: TIntArray);
  function  IntArrayToStr(arr: TIntArray; delimiter:string=' '): string;

implementation

var
  startTicks, globalTicks: QWord;

function OIDToQWord(oid: string): QWord;
begin
  if oid='' then
    result := 0
  else
    result := StrToQWord('$' + copy(oid, 1, 16))
end;

function OIDToParents(oid: string; oidlen: Integer): TQWordArray;
var
  i: Integer;
begin
  result := nil;
  while oid<>'' do begin
    if oid[1]=' ' then delete(oid, 1, 1);
    i := Length(result);
    SetLength(result, i+1);
    result[i] := OIDToQWord(copy(oid, 1, oidlen));
    delete(oid, 1, oidlen);
  end;
end;

function OIDToParentElements(oid: string; oidlen: Integer): TParentElementArray;
var
  i: Integer;
begin
  result := nil;
  while oid<>'' do begin
    if oid[1]=' ' then delete(oid, 1, 1);
    i := Length(result);
    SetLength(result, i+1);
    result[i].n := -1;
    result[i].commit := OIDToQWord(copy(oid, 1, oidlen));
    delete(oid, 1, oidlen);
  end;
end;

function Sanitize(aPath: RawbyteString; force: boolean): RawbyteString;
begin
  if force or (pos(' ', aPath)>0) then begin
    {$ifdef MsWindows}
    result := '"' + aPath + '"';
    {$else}
    result := StringReplace(aPath, ' ', '\ ', [rfReplaceAll]);
    {$endif}
  end else
    result := aPath;
end;

function QuoteMsg(msg: string): string;
begin
  result := StringReplace(msg, '"', '\"', [rfReplaceAll]);
  result := '"' + result + '"';
end;

function MakePathList(entryArray: TPFileEntryArray; sanitizeItems: boolean
  ): string;
var
  entry: PFileEntry;
  procedure Add(aPath:string);
  begin
    if result<>'' then result += ' ';
    if sanitizeItems then
      result += Sanitize(aPath)
    else
      result += aPath;
  end;
begin
  result := '';
  for entry in entryArray do
    Add(entry^.path);
end;

function GitDateToDateTime(s: string): TDateTime;
  function MonthToInt(m: string): Integer;
  begin
    case m of
      'Jan': result := 1;
      'Feb': result := 2;
      'Mar': result := 3;
      'Apr': result := 4;
      'May': result := 5;
      'Jun': result := 6;
      'Jul': result := 7;
      'Aug': result := 8;
      'Sep': result := 9;
      'Oct': result := 10;
      'Nov': result := 11;
      'Dec': result := 12;
      else   result := 0;
    end;
  end;
var
  i, mmm, dd, yyyy: Integer;
  tt: string;
begin
  // Mon Dec 6 00:39:46 2021 +0100
  if (Length(s)<29) or (Length(s)>30) then
    exit(0);

  mmm := MonthToInt(copy(s, 5, 3));
  dd := StrToIntDef(Trim(copy(s, 9, 2)), -1);
  if dd<0 then
    exit(0);

  i := 10;
  if s[11]=' ' then inc(i);
  delete(s, 1, i);
  tt := copy(s, 1, 8);
  yyyy := StrToIntDef(copy(s, 10, 4), -1);
  if yyyy<0 then
    exit(0);
  delete(s, 1, 14);
  insert(':', s, 4);

  s := format('%.4d-%.2d-%.2dT%sZ%s',[yyyy, mmm, dd, tt, s]);
  result := ISO8601ToDateDef(s, 0);
  result := UniversalTimeToLocal(result);
end;

function DateTimeToGitFmt(d: TDateTime): string;
begin
  //result := FormatDateTime('ddd mmm d hh:nn:ss yyyy', d);
  result := DateTimeToStr(d);
end;

procedure ResetTicks(globalToo: boolean);
begin
  startTicks := GetTickCount64;
  if globalToo then
    globalTicks := startTicks
end;

procedure ReportTicks(msg: string; globalToo: boolean);
var
  curTicks: QWord;
begin
  curTicks := GetTickCount64;
  if globalToo then begin
    DebugLn('%s took %d ms, global %d ms',[msg, curTicks - startTicks, curTicks - globalTicks]);
    startTicks := curTicks;
    globalTicks := startTicks;
  end else begin
    DebugLn('%s took %d ms',[msg, curTicks - startTicks]);
    startTicks := curTicks;
  end;
end;

function PosAny(chars: TSetOfChar; s: string): Integer;
var
  i: Integer;
begin
  result := 0;
  for i:=1 to Length(s) do
    if s[i] in chars then begin
      result := i;
      break;
    end;
end;

function ReplaceEOLs(s: string; encode: boolean): string;
begin
  if encode then begin
    result := StringReplace(s, #13#10, '&#1310;', [rfReplaceAll]);
    result := StringReplace(result, #10, '&#10;', [rfReplaceAll]);
    result := StringReplace(result, #13, '&#13;', [rfReplaceAll]);
  end;
    result := StringReplace(s, '&#13;', #13, [rfReplaceAll]);
    result := StringReplace(result, '&#10', #10, [rfReplaceAll]);
    result := StringReplace(result, '&#1310', #1310, [rfReplaceAll]);
end;

function EncodeDelimitedText(delimiter: string; strings: TStrings): string;
var
  s: string;

  procedure Add;
  begin
    if result<>'' then
      result += delimiter;
    result += s;
  end;

begin
  result := '';
  for s in strings do
    add;
end;

procedure DecodeDelimitedText(s: string; delimiter: string; strings: TStrings);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.Delimiter := #2;
    L.StrictDelimiter := true;
    L.DelimitedText := StringReplace(s, delimiter, #2, [rfReplaceAll]);
    strings.Assign(L);
  finally
    L.Free;
  end;
end;

procedure DumpIntArray(msg: string; arr: TIntArray);
var
  i: Integer;
begin
  DebugLn;
  DebugLn(msg);
  for i:=0 to Length(arr)-1 do begin
    DbgOut('%3d ',[arr[i]]);
    if (i+1) mod 20 = 0 then DebugLn;
  end;
  DebugLn;
end;

function IntArrayToStr(arr: TIntArray; delimiter: string): string;
var
  i: Integer;
begin
  result := '';
  for i:=0 to Length(arr)-1 do begin
    if result<>'' then result += delimiter;
    result += arr[i].ToString;
  end;
end;

end.

