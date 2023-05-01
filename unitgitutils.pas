unit unitgitutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, LazLogger, unitentries;

type
  TQWordArray = array of QWord;

  TParentElement = record
    n: Integer;
    commit: Qword;
  end;
  TParentElementArray = array of TParentElement;

  PParentsMapItem = ^TParentsMapItem;
  TParentsMapItem = record
    n: Integer;
    parents: TParentElementArray;
  end;

  TSetOfChar = set of char;

  function OIDToQWord(oid: string): QWord;
  function OIDToParents(oid: string; oidlen: Integer): TQWordArray;
  function OIDToParentElements(oid: string; oidlen: Integer): TParentElementArray;

  function QuoteMsg(msg: string): string;
  function MakePathList(entryArray: TPFileEntryArray; sanitizeItems:boolean=true): string;
  function Sanitize(aPath: RawbyteString; force:boolean=true): RawbyteString;
  function GitDateToDateTime(s: string): TDateTime;
  function DateTimeToGitFmt(d: TDateTime): string;

  procedure ResetTicks;
  procedure ReportTicks(msg:string);

  function PosAny(chars: TSetOfChar; s:string): Integer;

implementation

var
  startTicks: QWord;

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

procedure ResetTicks;
begin
  startTicks := GetTickCount64;
end;

procedure ReportTicks(msg: string);
var
  curTicks: QWord;
begin
  curTicks := GetTickCount64;
  DebugLn('%s took %d ms',[msg, curTicks - startTicks]);
  startTicks := curTicks;
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

end.

