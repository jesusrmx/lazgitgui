program logcache;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, DateUtils, LazLogger, unitLogCache, unitdbindex, unitgit,
  unitifaces, unitprocess, unitentries
  { you can add units after this };

{

  Generate simple '|' separated list:
    -d -e -a +p -i --oidlen=10 --sublen=72 -ct -int -inc -hdr -st -inh

}

const
  CUT_AT = 80;
  SEP = '|';

var
  OIDLen: integer = 40;
  SubjLen: integer = CUT_AT;
  withHumanDate: boolean = true;
  withDate: boolean = true;
  withParentOID: boolean = false;
  withCommitOID: boolean = true;
  withAuthor: boolean  = true;
  withEmail: boolean = true;
  withRefs: boolean = false;
  withSubject: boolean = true;
  withCutTag: boolean = true;

  withIndexInfo: boolean = true;
  withStats: boolean = true;
  withRecNum: boolean = true;
  withHeaders: boolean = true;
  withIncOffsets: boolean = true;
  withInheritance: boolean = true;
  withIntervals: boolean = true;


function Shorten(s: string): string;
var
  len: Integer;
begin
  len := Length(s);
  if len<=SubjLen then
    result := s
  else begin
    result := copy(s, 1, SubjLen);
    if withCutTag then
      result += ' 8< ~ ' + IntToStr(len - SubjLen) + ' more.';
  end;
end;

function GetDateStr(date: Int64): string;
begin
  result := IntToStr(date);
end;

function GetHumanDate(date: Int64): string;
var
  dt: TDateTime;
begin
  dt := UnixToDateTime(date, false);
  result := FormatDateTime('dd-mmm-yy hh:nn:ss am/pm', dt);
end;

function GetCachedItem(stream: TStream; aOffset: Int64; aSize:Integer; out item: TLogItem; skip:boolean=true): boolean;
var
  buf: pchar;
  p, q, t: pchar;
  num: Integer;
  date: Int64;
  s: string;
  b: byte;
  w: word;
begin
  stream.Position := aOffset;

  GetMem(buf, aSize);
  try
    try
      stream.Read(buf^, aSize);
      p := buf;
      t := p + aSize;
      if skip then
        inc(p, 2); // skip record size
      num := 0;
      item.CommiterDate := PInt64(p)^; inc(p, SizeOf(Int64));
      inc(num);
      while p<t do begin
        case Num of
          1..4:
            begin
              b := PByte(p)^; inc(p);
              SetString(s, p, b); inc(p, b);
              case num of
                1: Item.ParentOID := s;
                2: Item.CommitOID := s;
                3: Item.Author := s;
                4: Item.Email := s;
              end;
            end;
          5..6:
            begin
              w := PWord(p)^; inc(p, 2);
              SetString(s, p, w); inc(p, w);
              case num of
                5: Item.Refs := s;
                6: Item.Subject := s;
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

function GetCacheStr(stream: TStream; aOffset: Int64; aSize:Integer; skip:boolean=true):string;
var
  item: TLogItem;

  procedure Add(title, value: RawByteString);
  begin
    if result<>'' then
      result += SEP;
    //result += ' ' + title + ': ';
    result += value;
  end;

begin
  result := '';

  GetCachedItem(stream, aOffset, aSize, item, skip);

  if withIndexInfo then Add('Index', format('%8d %4d',[aOffset, aSize]));
  if withDate      then Add('Date', GetDateStr(item.CommiterDate));
  if withHumanDate then Add('Date', GetHumanDate(item.CommiterDate));
  if withParentOID then Add('Parent OID', copy(item.ParentOID, 1, OIDLen));
  if withCommitOID then Add('Commit OID', copy(item.CommitOID, 1, OIDLen));
  if withAuthor    then Add('Author', item.Author);
  if withEmail     then Add('E-mail', item.Email);
  if withRefs      then Add('Refs', item.Refs);
  if withSubject   then Add('Subject', shorten(item.Subject));
end;

procedure ProcessParameters;

  function GetBool(par:string; def:boolean): boolean;
  var
    s: string;
    i: Integer;
  begin
    i := 1;
    while i<paramcount do begin
      s := paramstr(i);
      if (pos('--', s)<>1) then begin
        if (s[1] in ['-','+']) and SameText(copy(s, 2, length(s)), par) then begin
          result := s[1]='+';
          exit;
        end;
      end;
      inc(i);
    end;
    result := def;
  end;

  function GetValue(par:string; def:string): string;
  var
    s: string;
    i, j: Integer;
  begin
    i := 1;
    while i<paramcount do begin
      s := paramstr(i);
      if pos('--', s)=1 then begin
        j := pos('=', s);
        if j=0 then
          raise Exception.CreateFmt('Invalid option %s',[s]);
        if sametext(copy(s, 3, j-3), par) then begin
          result := Copy(s, j+1, length(s));
          exit;
        end;
      end else if s[1]='-' then begin
        if SameText(copy(s, 2, Length(s)-1), par) then begin
          if i>=paramcount-1 then
            raise Exception.CreateFmt('Option %s needs a value',[par]);
          inc(i);
          result := paramStr(i);
          exit;
        end;
      end;
      inc(i);
    end;
    result := def;
  end;

  function GetIntValue(par:string; def: Integer): Integer;
  begin
    result := StrToIntDef(GetValue(par, ''), def);
  end;

begin

  OIDLen := GetIntValue('OIDLen', OIDLen);
  SubjLen := GetIntValue('SubLen', SubjLen);
  withHumanDate := GetBool('hd', withHumanDate);
  withDate := GetBool('d', withDate);
  withParentOID := GetBool('p', withParentOID);
  withCommitOID := GetBool('h', withCommitOID);
  withAuthor := GetBool('a', withAuthor);
  withEmail := GetBool('e', withEmail);
  withRefs := GetBool('r', withRefs);
  withSubject := GetBool('s', withSubject);
  withIndexInfo := GetBool('i', withIndexInfo);
  withCutTag := GetBool('ct', withCutTag);

  withStats  := GetBool('st', withStats);
  withRecNum := GetBool('rn', withRecNum);
  withHeaders := GetBool('hdr', withHeaders);
  withIncOffsets := GetBool('inc', withIncOffsets);
  withInheritance := GetBool('inh', withInheritance);
  withIntervals := GetBool('int', withIntervals);

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
  nextoid: string;
  w: word;
  Item: TLogItem;
begin
  if ParamCount=0 then begin
    DebugLn('a file or directory of a git repository is needed');
    halt(1);
  end;

  aFile := ExpandFileName(ParamStr(ParamCount));
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
    DebugLn('couldnt find the index file');
    halt(4);
  end;

  ProcessParameters;

  fIndexStream := TMemoryStream.Create;
  fIndexStream.LoadFromFile(aIndexFile);

  fCacheStream := TFileStream.Create(aCacheFile, fmOpenRead + fmShareDenyNone);

  if withStats then begin
    DebugLn('Index file: size=%d indices=%d',[fIndexStream.Size, fIndexStream.Size div SIZEOF_INDEX]);
    DebugLn('Cache file: size=%d',[fCacheStream.Size]);
  end;

  if withIncOffsets then begin
    if withHeaders then begin
      DebugLn;
      DebugLn('Summary of incremental offsets...');
    end;
    next := 0;
    n := 0;
    while fIndexStream.Position<fIndexStream.Size do begin
      fIndexStream.Read(IndxRec, SIZEOF_INDEX);
      if (Next=0) or (fIndexStream.Position=fIndexStream.Size) or (Next<>IndxRec.offset) then begin
        aDir := GetCacheStr(fCacheStream, IndxRec.offset, IndxRec.size);
        if withRecNum then
          DbgOut('%8d%s',[n+1,SEP]);
        DebugLn('%s',[aDir]);
      end;
      Next := IndxRec.Offset + IndxRec.Size;
      inc(n);
    end;
  end;

  if withIntervals then begin
    if withHeaders then begin
      DebugLn;
      DebugLn('Summary of date intervals');
    end;
    next := MAXINT;
    n := 0;
    fIndexStream.Position := 0;
    while fIndexStream.Position<fIndexStream.Size do begin
      fIndexStream.Read(IndxRec, SIZEOF_INDEX);
      GetCachedItem(fCacheStream, IndxRec.offset, IndxRec.size, Item);

      if (Next=MAXINT) or (fIndexStream.Position=fIndexStream.Size) or (Next<Item.CommiterDate) then begin
        aDir := GetCacheStr(fCacheStream, IndxRec.offset, IndxRec.size);
        if withRecNum then
          DbgOut('%8d%s',[n+1,SEP]);
        DebugLn('%s',[aDir]);
      end;
      Next := Item.CommiterDate;
      inc(n);
    end;
  end;

  if withInheritance then begin
    if withHeaders then begin
      DebugLn;
      DebugLn('Summary of inheritance');
    end;
    nextoid := '';
    n := 0;
    fIndexStream.Position := 0;
    while fIndexStream.Position<fIndexStream.Size do begin
      fIndexStream.Read(IndxRec, SIZEOF_INDEX);
      GetCachedItem(fCacheStream, IndxRec.offset, IndxRec.size, Item);

      if (Next=MAXINT) or (fIndexStream.Position=fIndexStream.Size) or (Nextoid<>Item.CommitOID) then begin
        aDir := GetCacheStr(fCacheStream, IndxRec.offset, IndxRec.size);
        if withRecNum then
          DbgOut('%8d%s',[n+1,SEP]);
        DebugLn('%s',[aDir]);
      end;
      nextoid := item.ParentOID;
      inc(n);
    end;
  end;

  if withHeaders then begin
    DebugLn;
    DebugLn('Listing in index order');
  end;
  n := 0;
  fIndexStream.Position := 0;
  while fIndexStream.Position<fIndexStream.Size do begin
    fIndexStream.Read(IndxRec, SIZEOF_INDEX);
    aDir := GetCacheStr(fCacheStream, IndxRec.offset, IndxRec.size);
    if withRecNum then
      DbgOut('%8d%s',[n+1,SEP]);
    DebugLn('%s',[aDir]);
    inc(n);
  end;

  //DebugLn;
  //DebugLn('Listing directly from cacheBuffer');
  //n := 0;
  //fCacheStream.Position := 0;
  //while fCacheStream.Position<fCacheStream.Size do begin
  //  next := fCacheStream.Position;
  //  w := fCacheStream.ReadWord;
  //  aDir := GetCacheStr(fCacheStream, fCacheStream.Position, w, false);
  //  DebugLn('%5d. %8d %4d %s',[n+1, next, w + 2, aDir]);
  //  inc(n);
  //end;



end.

