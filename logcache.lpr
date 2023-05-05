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
  {.$define UseIndex}

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
  withInOrder: boolean = true;
  withTopo: boolean = true;
  withReindex: boolean = true;

type
  PIndexDateRecord = ^TIndexDateRecord;
  TIndexDateRecord = packed record
    offset: Int64;
    size: word;
    Date: Int64;
  end;


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

function GetParentOIDList(parentOID:string; OIDLen: Integer): string;
var
  L: TStringList;
  i: Integer;
begin
  result := '';
  L := TStringList.Create;
  L.DelimitedText := ParentOID;
  for i:=0 to L.Count-1 do begin
    if result<>'' then result += ' ';
    result += copy(L[i], 1, OIDLen);
  end;
  L.Free;
end;

function GetCachedItem(indexStream, stream: TStream; aOffset: Int64; aSize:Integer; out item: TLogItem; skip:boolean=true): boolean;
var
  buf: pchar;
  p, q, t: pchar;
  num: Integer;
  date: Int64;
  s: string;
  b: byte;
  w: word;
begin

  result := false;

  if aOffset+aSize>stream.Size then
    raise Exception.CreateFmt('Corruption detected (read beyond Limits) Index: Offset=%d Cache: Offset=%d Size=%d',
      [indexStream.position - SIZEOF_INDEX, aOffset, stream.size]);

  stream.Position := aOffset;
  GetMem(buf, aSize);
  try

    stream.Read(buf^, aSize);

    p := buf;
    t := p + aSize;

    if pword(p)^+2<>aSize then
      raise Exception.CreateFmt('Corruption detected (size mismatch) Index: Offset=%d Size=%d CacheRecord: Offset=%d Size=%d',
        [indexStream.position - SIZEOF_INDEX, SIZEOF_INDEX, aOffset, w]);

    try

      if skip then
        inc(p, 2); // skip record size

      num := FIELD_DATE;
      item.CommiterDate := PInt64(p)^; inc(p, SizeOf(Int64));
      inc(num);
      while p<t do begin
        case Num of
          FIELD_COMMITOID, FIELD_AUTHOR, FIELD_EMAIL:
            begin
              b := PByte(p)^; inc(p);
              SetString(s, p, b); inc(p, b);
              case num of
                FIELD_COMMITOID: Item.CommitOID := s;
                FIELD_AUTHOR: Item.Author := s;
                FIELD_EMAIL: Item.Email := s;
              end;
            end;
          FIELD_PARENTOID, FIELD_SUBJECT:
            begin
              w := PWord(p)^; inc(p, 2);
              SetString(s, p, w); inc(p, w);
              case num of
                FIELD_PARENTOID: Item.ParentOID := s;
                FIELD_SUBJECT: Item.Subject := s;
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

function GetItemStr(item: TLogItem): string;
  procedure Add(title, value: RawByteString);
  begin
    if result<>'' then
      result += SEP;
    //result += ' ' + title + ': ';
    result += value;
  end;
begin
  result := '';

  if withDate      then Add('Date', GetDateStr(item.CommiterDate));
  if withHumanDate then Add('Date', GetHumanDate(item.CommiterDate));
  if withParentOID then Add('Parent OID', GetParentOIDList(item.ParentOID, OIDLen));
  if withCommitOID then Add('Commit OID', copy(item.CommitOID, 1, OIDLen));
  if withAuthor    then Add('Author', item.Author);
  if withEmail     then Add('E-mail', item.Email);
  if withSubject   then Add('Subject', shorten(item.Subject));
end;

function GetCacheStr(aOffset:Int64; aSize: Integer; item: TLogItem):string;

  procedure Add(title, value: RawByteString);
  begin
    if result<>'' then
      result += SEP;
    //result += ' ' + title + ': ';
    result += value;
  end;

begin
  result := '';

  if withIndexInfo then Add('Index', format('%8d %4d',[aOffset, aSize]));
  result += GetItemStr(item);
end;

function GetBoolParameter(par:string; def:boolean): boolean;
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

function GetStringParameter(par:string; def:string): string;
var
  s: string;
  i, j: Integer;
begin
  i := 1;
  while i<=paramcount do begin
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

function GetIntParameter(par:string; def: Integer): Integer;
begin
  result := StrToIntDef(GetStringParameter(par, ''), def);
end;

procedure ProcessParameters;
begin

  OIDLen := GetIntParameter('OIDLen', OIDLen);
  SubjLen := GetIntParameter('SubLen', SubjLen);
  withHumanDate := GetBoolParameter('hd', withHumanDate);
  withDate := GetBoolParameter('d', withDate);
  withParentOID := GetBoolParameter('p', withParentOID);
  withCommitOID := GetBoolParameter('h', withCommitOID);
  withAuthor := GetBoolParameter('a', withAuthor);
  withEmail := GetBoolParameter('e', withEmail);
  withRefs := GetBoolParameter('r', withRefs);
  withSubject := GetBoolParameter('s', withSubject);
  withIndexInfo := GetBoolParameter('i', withIndexInfo);
  withCutTag := GetBoolParameter('ct', withCutTag);

  withStats  := GetBoolParameter('st', withStats);
  withRecNum := GetBoolParameter('rn', withRecNum);
  withHeaders := GetBoolParameter('hdr', withHeaders);
  withIncOffsets := GetBoolParameter('inc', withIncOffsets);
  withInheritance := GetBoolParameter('inh', withInheritance);
  withIntervals := GetBoolParameter('int', withIntervals);
  withInOrder := GetBoolParameter('ord', withInOrder);
  withTopo := GetBoolParameter('topo', withTopo);
  withReindex := GetBoolParameter('reindex', withReindex);

end;

var
  found: boolean;
  aFile, aDir: string;

procedure CheckDir;
begin
  if ParamCount=0 then begin
    DebugLn('a file or directory of a git repository is needed');
    halt(1);
  end;

  aDir := GetStringParameter('dir', '');
  if aDir<>'' then begin
    aDir := ExpandFilename(aDir);
    if not DirectoryExists(aDir) then
      aDir := ''
    else
      aDir := IncludeTrailingPathDelimiter(aDir);
  end;

  if aDir='' then begin
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

    aDir := aDir + '.git' + PathDelim;

  end;

end;

procedure GetFilenames(out aIndexFile, aCacheFile: string);
begin
  aIndexFile := aDir + 'lazgitgui.logindex';
  aCacheFile := ChangeFileExt(aIndexFile, '.logcache');
end;


function CompareIndices(Item1, Item2: Pointer): Integer;
var
  itemA: PIndexDateRecord absolute Item1;
  itemB: PIndexDateRecord absolute Item2;
begin
  result := itemB^.Date - itemA^.Date;
end;

procedure CheckForCorruption(var fIndexStream: TMemorystream; var fCacheStream: TFileStream);
var
  indxRec: TIndexRecord;
  Item: TLogItem;
  i, m: Integer;
  failed: Boolean;
  lastSize: Word;
  dummy, aIndexFile: string;
  ticks: QWord;
  L: TList;
  idRec: PIndexDateRecord;
begin
  DebugLn('Checking for Corruption');
  failed := false;
  i := 0;
  while fIndexStream.Position<fIndexStream.Size do begin
    fIndexStream.Read(IndxRec, SIZEOF_INDEX);
    if IndxRec.offset<fCacheStream.Size then begin
      fCacheStream.Position := IndxRec.Offset;
      lastSize := fCacheStream.ReadWord;
      if IndxRec.size-2<>lastSize then begin
        DebugLn('Corruption detected at item %d indexOffset=%d cacheOffset=%d expected=%d found=%d',
          [i, fIndexStream.Position-SIZEOF_INDEX, IndxRec.offset, IndxRec.size-2, lastSize]);
        failed := true;
        break;
      end;

    end else begin
      DebugLn('Can''t reach item %d offset %d beyond cache size %d',[i, IndxRec.offset, fCacheStream.Size]);
      failed := true;
      break;
    end;
    inc(i);
  end;

  if not failed and (fCacheStream.Position + lastSize<>fCacheStream.Size) then begin
    DebugLn('CacheFile size inconsistency: Offset=%d Size=%d',[fCacheStream.Position, fCacheStream.Size]);
    failed := true;
  end;

  if not failed then
    exit;

  DebugLn('Trying to reconstruct the index');

  ticks := GetTickCount64;
  L := TList.Create;
  //  the signature is ok or we wouldnt be here
  fCacheStream.Position := 4;
  fIndexStream.clear;
  while fCacheStream.Position<fCacheStream.Size do begin
    lastSize := fCacheStream.ReadWord;
    new(idRec);
    idRec^.offset := fCacheStream.Position - 2;
    idRec^.size := lastSize + 2;
    idRec^.Date := fCacheStream.ReadQWord;
    L.Add(idRec);
    fCacheStream.Seek(lastSize - sizeOf(Int64), soFromCurrent);
  end;
  L.Sort(@CompareIndices);
  for i:=0 to L.Count-1 do begin
    idRec := PIndexDateRecord(L[i]);
    fIndexStream.Write(idRec^, SIZEOF_INDEX);
    dispose(idRec);
  end;
  L.Free;

  GetFilenames(aIndexFile, dummy);
  fIndexStream.SaveToFile(aIndexFile);
  DebugLn('Reindexig took %d ms',[GetTickCount64 - ticks]);

  fIndexStream.position := 0;
  fCacheStream.Position := 4;

  DebugLn('Index stream was recovered: %d records', [fIndexStream.Size div SIZEOF_INDEX]);
end;

procedure DirectAccessDateIntervals(fIndexStream: TMemoryStream; fCacheStream: TFileStream);
var
  n: Integer;
  next: Int64;
  indxRec: TIndexRecord;
  Item: TLogItem;
begin
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
      GetCachedItem(fIndexStream, fCacheStream, IndxRec.offset, IndxRec.size, Item);

      if (Next=MAXINT) or (fIndexStream.Position=fIndexStream.Size) or (Next<Item.CommiterDate) then begin
        aDir := GetCacheStr(IndxRec.offset, IndxRec.size, Item);
        if withRecNum then
          DbgOut('%8d%s',[n+1,SEP]);
        DebugLn('%s',[aDir]);
      end;
      Next := Item.CommiterDate;
      inc(n);
    end;
  end;
end;

procedure UseDirectAccess;
var
  fIndexStream: TMemoryStream;
  fCacheStream: TFileStream;
  fGit: TGit;
  aIndexFile, aCacheFile: string;
  indxRec: TIndexRecord;
  n: Integer;
  next: Int64;
  nextoid: string;
  w: word;
  Item: TLogItem;
  sig, ver: word;
begin

  DebugLn('Using Direct Access');

  GetFilenames(aIndexFile, aCacheFile);

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
  if withStats then
    DebugLn('Index file: size=%d indices=%d',[fIndexStream.Size, fIndexStream.Size div SIZEOF_INDEX]);

  fCacheStream := TFileStream.Create(aCacheFile, fmOpenRead + fmShareDenyNone);
  if withStats then
    DebugLn('Cache file: size=%d',[fCacheStream.Size]);
  if fCacheStream.Size<4 then begin
    DebugLn('Invalid file cache size %d', [fCacheStream.Size]);
    Halt(6);
  end;

  sig := BeToN(fCacheStream.ReadWord);
  ver := BeToN(fCacheStream.ReadWord);
  if withStats then
    DebugLn('File Cache Signature=%.4x Version=%d',[sig, ver]);
  if sig<>PGM_SIGNATURE then begin
    DebugLn('Invalid file signature %.4x',[sig]);
    Halt(5);
  end;

  CheckForCorruption(fIndexStream, fCacheStream);


  if withIncOffsets then begin
    if withHeaders then begin
      DebugLn;
      DebugLn('Summary of incremental offsets...');
    end;
    next := 0;
    n := 0;
    try
      fIndexStream.Position := 0;
      while fIndexStream.Position<fIndexStream.Size do begin
        fIndexStream.Read(IndxRec, SIZEOF_INDEX);
        GetCachedItem(fIndexStream, fCacheStream, IndxRec.offset, IndxRec.size, Item);

        if (Next=0) or (fIndexStream.Position=fIndexStream.Size) or (Next<>IndxRec.offset) then begin
          aDir := GetCacheStr(IndxRec.offset, IndxRec.size, Item);
          if withRecNum then
            DbgOut('%8d%s',[n+1,SEP]);
          DebugLn('%s',[aDir]);
        end;
        Next := IndxRec.Offset + IndxRec.Size;
        inc(n);
      end;
    except
      on E:Exception do begin
        DebugLn('At Index: Offset=%d Cache: Offset=%d error: %s',[fIndexStream.Position-SIZEOF_INDEX, IndxRec.offset, E.Message]);
        halt(10);
      end;
    end;
  end;

  DirectAccessDateIntervals(fIndexStream, fCacheStream);

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
      GetCachedItem(fIndexStream, fCacheStream, IndxRec.offset, IndxRec.size, Item);
      if (Next=0) or (fIndexStream.Position=fIndexStream.Size) or (Nextoid<>Item.CommitOID) then begin
        aDir := GetCacheStr(IndxRec.offset, IndxRec.size, Item);
        if withRecNum then
          DbgOut('%8d%s',[n+1,SEP]);
        DebugLn('%s',[aDir]);
      end;
      nextoid := item.ParentOID;
      inc(n);
    end;
  end;

  if withInOrder then begin
    if withHeaders then begin
      DebugLn;
      DebugLn('Listing in index order');
    end;
    n := 0;
    fIndexStream.Position := 0;
    while fIndexStream.Position<fIndexStream.Size do begin
      fIndexStream.Read(IndxRec, SIZEOF_INDEX);
      GetCachedItem(fIndexStream, fCacheStream, IndxRec.offset, IndxRec.size, Item);
      aDir := GetCacheStr(IndxRec.offset, IndxRec.size, Item);
      if withRecNum then
        DbgOut('%8d%s',[n+1,SEP]);
      DebugLn('%s',[aDir]);
      inc(n);
    end;
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
end;

{$ifdef UseIndex}
procedure DoIntervals(db: TDbIndex);
var
  i: Integer;
  next: Int64;
begin
  if withIntervals then begin
    if withHeaders then begin
      DebugLn;
      DebugLn('Summary of date intervals');
    end;
    next := MAXINT;
    for i:=0 to db.Count-1 do begin
      db.LoadItem(i);
      if (Next=MAXINT) or (i=db.Count-1) or (Next<db.Item.CommiterDate) then begin
        aDir := GetItemStr(db.Item);
        if withRecNum then
          DbgOut('%8d%s',[i+1,SEP]);
        DebugLn('%s',[aDir]);
      end;
      Next := db.Item.CommiterDate;
    end;
  end;
end;

procedure DoInheritance(db: TDbIndex);
var
  i: Integer;
  nextoid, aDir: string;
begin
  if withInheritance then begin
    if withHeaders then begin
      DebugLn;
      DebugLn('Summary of inheritance');
    end;
    nextoid := '';
    for i:=0 to db.Count-1 do begin
      db.LoadItem(i);
      if (i=0) or (i=db.Count-1) or (Nextoid<>db.Item.CommitOID) then begin
        aDir := GetItemStr(db.Item);
        if withRecNum then
          DbgOut('%8d%s',[i+1,SEP]);
        DebugLn('%s',[aDir]);
      end;
      nextoid := db.Item.ParentOID;
    end;
  end;
end;

procedure DoInOrder(db: TDbIndex);
var
  i: Integer;
  aDir: string;
begin
  if withInOrder then begin
    if withHeaders then begin
      DebugLn;
      DebugLn('Listing in index order');
    end;
    for i:=0 to db.Count-1 do begin
      db.LoadItem(i);
      aDir := GetItemStr(db.Item);
      if withRecNum then
        DbgOut('%8d%s',[i+1,SEP]);
      DebugLn('%s',[aDir]);
    end;
  end;
end;

procedure DoTopo(db: TDbIndex);
var
  i: Integer;
  aDir: string;
begin
  if withTopo then begin
    db.TopoSort;
    //if withHeaders then begin
    //  DebugLn;
    //  DebugLn('Topological listing');
    //end;
    //for i:=0 to db.Count-1 do begin
    //  db.LoadItem(i);
    //  aDir := GetItemStr(db.Item);
    //  if withRecNum then
    //    DbgOut('%8d%s',[i+1,SEP]);
    //  DebugLn('%s',[aDir]);
    //end;
    //
    //DoInheritance(db);
  end;
end;

procedure ProcessDbIndex(db: TDbIndex);
var
  i: Integer;
  next: Int64;
  nextoid: string;
begin
  ProcessParameters;

  if withStats then begin
    DebugLn('DbIndex has %d log records',[db.Count]);
  end;


  if withIncOffsets then begin
    if withHeaders then begin
      DebugLn;
      DebugLn('Summary of incremental offsets...');
    end;
    DebugLn('Not available while using dbIndex');
  end;

  DoIntervals(db);
  DoInheritance(db);
  DoInOrder(db);

  //DebugLn;
  //DebugLn('Filtered listing');
  //db.SetFilter([0, 233, 0]);
  //for i:=0 to db.Count-1 do begin
  //  db.LoadItem(i);
  //  aDir := GetItemStr(db.Item);
  //  if withRecNum then
  //    DbgOut('%8d%s',[i+1,SEP]);
  //  DebugLn('%s',[aDir]);
  //end;
  DoTopo(db);
end;

function UseDbIndex: boolean;
var
  db: TDbIndex;
begin
  DebugLn('Using DbIndex');

  result := false;
  db := TDbIndex.Create(aDir);
  db.ReadOnly := true;
  try
    try
      db.Open;
      ProcessDbIndex(db);
    except
      on E:Exception do begin
        DebugLn('Error while loading dbindex: %s',[E.Message]);
        exit;
      end;
    end;
  finally
    db.Free;
  end;
end;
{$endif}

begin
  CheckDir;

  {$ifdef UseIndex}
  UseDbIndex;
  {$else}
  UseDirectAccess;
  {$endif}
end.

