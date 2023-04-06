program logcache;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, LazLogger, unitLogCache, unitgit, unitifaces, unitprocess, unitentries
  { you can add units after this };



function GetCacheStr(stream: TStream; aOffset: Int64; aSize:Integer; skip:boolean=true):string;
var
  buf: pchar;
  p, q, t: pchar;
  num: Integer;
  date: Int64;
  s: string;
  b: byte;
  w: word;
begin
  result := '';

  stream.position := aOffset;
  GetMem(buf, aSize);
  try
    try
      stream.Read(buf^, aSize);
      p := buf;
      t := p + aSize;
      if skip then
        inc(p, 2); // skip record size
      num := 0;
      date := PInt64(p)^; inc(p, SizeOf(Int64));
      result += 'Date: ' + IntToStr(date) + ', ';
      inc(num);
      while p<t do begin
        case Num of
          1..4:
            begin
              b := PByte(p)^; inc(p);
              SetString(s, p, b); inc(p, b);
              //case num of
              //  1: result += 'Parent OID: ' + s + ', ';
              //  2: result += 'Commit OID: ' + s + ', ';
              //  3: result += 'Author: ' + s + ', ';
              //  4: result += 'E-mail: ' + s + ', ';
              //end;
            end;
          5..6:
            begin
              w := PWord(p)^; inc(p, 2);
              SetString(s, p, w); inc(p, w);
              case num of
                //5: result += 'Refs: ' + s + ', ';
                6: result += 'Subject: ' + copy(s, 1, 60) + ', ';
              end;
            end;
        end;
        inc(num);
      end;
    except
    end;
  finally
    FreeMem(buf);
  end;
end;


var
  fIndexStream: TMemoryStream;
  fCacheStream: TFileStream;
  fGit: TGit;
  afile, aDir, aIndexFile, aCacheFile: string;
  found: boolean;
  indxRec: TIndexRecord;
  n: Integer;
  next: Int64;
  w: word;
begin
  if ParamCount=0 then begin
    DebugLn('a file or directory of a git repository is needed');
    halt(1);
  end;

  aFile := ExpandFileName(ParamStr(1));
  if FileExists(aFile) then
    aDir := ExtractFilePath(aFile)
  else
    aDir := IncludeTrailingPathDelimiter(aFile);

  found := false;
  while DirectoryExists(aDir) do begin
    found := DirectoryExists(aDir + '.git');
    if found then break;
    aDir := ExtractFilePath(ExcludeTrailingPathDelimiter(aDir));
  end;

  if not found then begin
    DebugLn('a git root directory could not be found for ', aFile);
    halt(2);
  end;

  aIndexFile := aDir + '.git' + PathDelim + 'lazgitgui.logindex';
  aCacheFile := ChangeFileExt(aIndexFile, '.logcache');

  if not FileExists(aCacheFile) then begin
    DebugLn('couldnt find the cache file');
    halt(3);
  end;

  if not FileExists(aIndexFile) then begin
    DebugLn('couldnt find the cache file');
    halt(3);
  end;

  fIndexStream := TMemoryStream.Create;
  fIndexStream.LoadFromFile(aIndexFile);

  fCacheStream := TFileStream.Create(aCacheFile, fmOpenRead + fmShareDenyNone);

  DebugLn('Index file: size=%d indices=%d',[fIndexStream.Size, fIndexStream.Size div SIZEOF_INDEX]);
  DebugLn('Cache file: size=%d',[fCacheStream.Size]);

  DebugLn;
  DebugLn('Summary...');
  next := 0;
  n := 0;
  while fIndexStream.Position<fIndexStream.Size do begin
    fIndexStream.Read(IndxRec, SIZEOF_INDEX);
    if (Next=0) or (fIndexStream.Position=fIndexStream.Size) or (Next<>IndxRec.offset) then begin
      aDir := GetCacheStr(fCacheStream, IndxRec.offset, IndxRec.size);
      DebugLn('%5d. %8d %4d %s',[n+1, IndxRec.offset, IndxRec.size, aDir]);
    end;
    Next := IndxRec.Offset + IndxRec.Size;
    inc(n);
  end;

  DebugLn;
  DebugLn('Listing in index order');
  n := 0;
  fIndexStream.Position := 0;
  while fIndexStream.Position<fIndexStream.Size do begin
    fIndexStream.Read(IndxRec, SIZEOF_INDEX);
    aDir := GetCacheStr(fCacheStream, IndxRec.offset, IndxRec.size);
    DebugLn('%5d. %8d %4d %s',[n+1, IndxRec.offset, IndxRec.size, aDir]);
    inc(n);
  end;

  DebugLn;
  DebugLn('Listing directly from cacheBuffer');
  n := 0;
  fCacheStream.Position := 0;
  while fCacheStream.Position<fCacheStream.Size do begin
    next := fCacheStream.Position;
    w := fCacheStream.ReadWord;
    aDir := GetCacheStr(fCacheStream, fCacheStream.Position, w, false);
    DebugLn('%5d. %8d %4d %s',[n+1, next, w + 2, aDir]);
    inc(n);
  end;



end.

