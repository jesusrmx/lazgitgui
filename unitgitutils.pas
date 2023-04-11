unit unitgitutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TQWordArray = array of QWord;

  function OIDToQWord(oid: string): QWord;
  function OIDToParents(oid: string; oidlen: Integer): TQWordArray;

implementation


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


end.

