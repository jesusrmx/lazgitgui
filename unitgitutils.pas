unit unitgitutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, unitentries;

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

  function OIDToQWord(oid: string): QWord;
  function OIDToParents(oid: string; oidlen: Integer): TQWordArray;
  function OIDToParentElements(oid: string; oidlen: Integer): TParentElementArray;

  function QuoteMsg(msg: string): string;
  function MakePathList(entryArray: TPFileEntryArray; sanitizeItems:boolean=true): string;
  function Sanitize(aPath: RawbyteString; force:boolean=true): RawbyteString;

  procedure ResetTicks;
  procedure ReportTicks(msg:string);

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


end.

