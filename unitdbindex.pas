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

  Handles the log database
}
unit unitdbindex;

{$mode ObjFPC}{$H+}

{.$define Debug}

{$ifdef Debug}
{.$define DumpCommitsAndParents}
{$endif}

interface

uses
  Classes, SysUtils, Math, DateUtils, LazLogger, unitifaces, fgl, unitgitutils;

const
  FIELD_DATE        = 0;
  FIELD_PARENTOID   = 1;
  FIELD_COMMITOID   = 2;
  FIELD_AUTHOR      = 3;
  FIELD_EMAIL       = 4;
  FIELD_SUBJECT     = 5;

  PGM_VERSION       = $0001;
  PGM_SIGNATURE     = $1A2A;

  LINE_SOURCE_COLUMN  = -1;

type

  TLogItem = record
    CommiterDate: Int64;
    ParentOID, CommitOID,
    Author,
    Email,
    Subject: RawByteString;
  end;

  PIndexRecord = ^TIndexRecord;
  TIndexRecord = packed record
    offset: Int64;
    size: word;
  end;

  TIntArray = array of Integer;

  TParentsItem = record
    n: Integer;
    parents: TQWordArray;
    commit: QWord;
  end;

  TParentsArray = array of TParentsItem;

  TParentsMap = specialize TFPGMap<QWord, PParentsMapItem>;

  TLineItemFlag = (lifNode, lifToMerge, lifMerge, lifFirst, lifInternal, lifLast,  lifToBorn, lifBorn);
  TLineItemFlags = set of TLineItemFlag;
  TLineItem = record
    column, columnIndex: Integer;
    source: Integer;
    Flags:  TLineItemFlags;
  end;
  TLinesArray = array of TLineItem;

  TItemIndex = record
    index: Integer;
    commit: QWord;
    parents, childs: TIntArray;
    column, section: Integer;
    lines: TLinesArray;
    //first: boolean;
    //last: boolean;
  end;
  TItemIndexArray = array of TItemIndex;

const
  SIZEOF_INDEX = sizeof(TIndexRecord);

type

  { TDbIndex }

  TDbIndex = class(TMyInterfacedObject, IDbIndex)
  private
    fHead: Boolean;
    fMaxBufferSize: Integer;
    fBuffer: Pchar;
    function GetFileName(aIndex: Integer): string;
  private
    fDir: string;
    fCacheStream: TFileStream;
    fIndexStream: TFileStream;
    fMaxRecords: Integer;
    fOldCount: Integer;
    fOldIndexSize: SizeInt;
    fCacheUpdate: boolean;
    fOldIndexOffset: Int64;
    fLoadedItemIndex: Integer;
    fItem: TLogItem;
    fReadOnly: boolean;
    fFilter: TIntArray;
    fCursorLock: TRTLCriticalSection;
    function GetAcceptingNewRecords: boolean;
    function GetActive: boolean;
    function GetInfo: string;
    //procedure RecoverIndex;
    procedure ReIndex;
    procedure GetIndexRecord(var aIndex:Integer; out indxRec: TIndexRecord; unfiltered:boolean);
    procedure CacheBufferFromItem(out aBuf: PChar; out len: word);
    procedure ItemFromCacheBuffer;
    procedure ItemFromLogBuffer(aBuf: PChar);
    procedure DumpItem;
    procedure ThreadStart(aHead: boolean);
    procedure ThreadDone;
    procedure ThreadStore(buf: pchar; size: Integer);
    function GetCachedItem(aIndex: Integer; unfiltered, tryToFix:boolean): boolean;
  public
    constructor Create(dir: string);
    destructor Destroy; override;

    procedure Open;
    function LoadItem(aIndex: Integer; unfiltered:boolean=false): boolean;
    procedure SetFilter(arr: TIntArray);
    procedure TopoSort;
    function FindCommitSha(sha: string; startAt:Integer=-1): Integer;
    function Count(unfiltered: boolean = false): Integer;

    property Item: TLogItem read fItem;
    property Info: string read GetInfo;
    property ReadOnly: boolean read fReadOnly write fReadOnly;
    property AcceptingNewRecords: boolean read GetAcceptingNewRecords;
    property MaxRecords: Integer read fMaxRecords write fMaxRecords;
    property Active: boolean read GetActive;
  end;

  function GetItemIndexes(db: TDbIndex; withColumns:boolean; out maxColumns:Integer): TItemIndexArray;

var
  gblInvalidateCache: boolean = false;

implementation

const
  BASE_FILENAME     = 'lazgitgui';
  FILENAME_INFO     = 1;
  FILENAME_INDEX    = 2;
  FILENAME_CACHE    = 3;
  FILENAME_INDEXTMP = 4;

type
  TColumnSection = record
    index, column: Integer;
    first, head: Integer;
    last, tail: Integer;
    count: Integer;
  end;
  TColumnSectionArray = array of TColumnSection;

  TColumn = record
    index: Integer;
    Sections: TColumnSectionArray;
  end;
  TColumnArray = array of TColumn;

{$IFDEF Debug}
procedure ReportColumns(msg:string; columns: TColumnArray);
var
  col, sec: Integer;
begin
  // all columns have now their real extensions, report them
  DebugLn;
  DebugLn('''%s'', report of %d columns:',[msg, Length(Columns)]);
  for col:=0 to Length(Columns)-1 do begin
    DebugLn('Column %2d: Sections: %d',[col, Length(columns[col].sections)]);
    for sec:=0 to Length(columns[col].Sections)-1 do
      with columns[col].sections[sec] do
        DebugLn('  Section %d: tip=%3d first=%3d last=%3d tail=%3d -> count=%3d',[sec, col, head, first, last, tail, count]);
  end;
end;

{$ifdef DumpCommitsAndParents}
procedure ReportCommitsAndParents(parMap: TParentsMap);
var
  pmi: PParentsMapItem;
  mind: TIntArray;
  i, j: Integer;
begin
  SetLength(mind, parMap.Count);
  DebugLn;
  DebugLn('PARENTS');
  for i:=0 to parMap.Count-1 do begin
    pmi := parMap.Data[i];
    mind[pmi^.n] := i;
  end;
  for i:=0 to Length(mind)-1 do begin
    pmi := parMap.Data[mind[i]];
    DbgOut('%3d: %.16x => ',[pmi^.n, parMap.Keys[mind[i]]]);
    for j:=0 to Length(pmi^.parents)-1 do
      DbgOut('%.16x ',[pmi^.parents[j].commit]);
    DebugLn;
  end;

  ReportTicks('Reporting Parents OID');
end;
{$endif}

function dbgs(lif: TLineItemFlags): string; overload;
  procedure Add(s:string);
  begin
    if result<>'' then result += ' ';
    result += s;
  end;
var
  i: TLineItemFlag;
  s: string;
begin
  result := '';
  for i in lif do begin
    WriteStr(s, i);
    Add(s);
  end;
  //(lifNode, lifToMerge, lifMerge, lifFirst, lifInternal, lifLast,  lifToBorn, lifBorn);
end;

procedure ReportItemIndexArray(result: TItemIndexArray);
var
  i, j, k, n, p: Integer;
  s: string;
begin
  DebugLn;
  DebugLn('ItemIndexArray: %d',[Length(result)]);

  p := 0; n:= 0;
  for i:=0 to Length(result)-1 do begin
    if Length(result[i].parents)>p then
      p:=Length(result[i].parents);
  end;
  p := p * 4;

  for i:=0 to Length(result)-1 do begin
    DbgOut('%3d. |%2d| P(%d): ',[i, result[i].column, Length(result[i].parents)]);
    k := Length(result[i].parents);
    s := ''; for j:=0 to k-1 do s += format('%3d ',[result[i].parents[j]]);
    DbgOut('%'+IntToStr(p)+'s',[s]);
    DbgOut(' C(%d)',[Length(result[i].childs)]);
    for j:=0 to Length(result[i].childs)-1 do dbgOut('%3d ',[result[i].childs[j]]);
    DebugLn;
  end;
end;

procedure ReportResult(items: TItemIndexArray; columns: TColumnArray);
var
  i, j, k: Integer;
  s, l: string;
begin
  DebugLn;
  DebugLn('The Result');
  SetLength(s, Length(columns));
  for i:=0 to Length(items)-1 do begin
    l := ' Lines:';
    for j := 0 to Length(columns)-1 do s[j+1] := ' ';
    for j := 0 to Length(items[i].lines)-1 do begin
      k := items[i].lines[j].column+1;
      if lifNode in items[i].lines[j].Flags then       s[k] := '*'
      else if lifMerge in items[i].lines[j].Flags then s[k] := '-'
      else if lifBorn in items[i].lines[j].Flags then  s[k] := '^'
      else                                             s[k] := '|';
      l += Format(' %d: [%s]',[j, dbgs(items[i].lines[j].Flags)]);
    end;
    DebugLn('%.6d] %s [%s', [i, s, l]);
  end;
end;

{$ENDIF}

//procedure SortColumns(var columns: TColumnArray);
//
//  function CompareColumns(a, b: Integer): Integer;
//  begin
//    // first compare columns element size (in descending order)
//    result := columns[b].count - columns[a].count;
//    if result=0 then
//      result := columns[a].index - columns[b].index;
//  end;
//
//  procedure ExchangeColumns(i, j: Integer);
//  var
//    Q: TColumn;
//  begin
//    Q := columns[I];
//    columns[I] := columns[J];
//    columns[J] := Q;
//  end;
//
//  procedure QuickSort(L,R: Integer);
//  var
//    I,J: Integer;
//    P,Q: Integer;
//  begin
//    repeat
//      I:=L;
//      J:=R;
//      P:=(L+R) div 2;
//      repeat
//        while CompareColumns(P, I)>0 do I:=I+1;
//        while CompareColumns(P, J)<0 do J:=J-1;
//        if I<=J then begin
//
//          if I<>J then
//            ExchangeColumns(I, J);
//
//          if P=I then
//            P:=J
//          else if P=J then
//            P:=I;
//
//          I:=I+1;
//          J:=J-1;
//        end;
//      until I>J;
//
//      if L<J then
//        QuickSort(L,J);
//
//      L:=I;
//    until I>=R;
//  end;
//
//begin
//  QuickSort(0, Length(Columns)-1);
//end;

function GetParentsMap(db: TDbIndex): TParentsMap;
var
  i, j, k, aIndex: Integer;
  pmi, found: PParentsMapItem;
  elements: TParentElementArray;
  lost: TIntArray;
begin
  lost := nil;
  result := TParentsMap.Create;
  result.Sorted := true;
  for i:=db.Count-1 downto 0 do begin
    db.LoadItem(i);
    with db.Item do begin
      new(pmi);
      pmi^.n := i;
      pmi^.parents := nil;
      result.Add(OIDToQWord(CommitOID), pmi);
      //
      elements := OIDToParentElements(parentOID, Length(CommitOID));
      SetLength(pmi^.parents, Length(elements));
      k := 0;
      for j:=0 to Length(elements)-1 do begin
        if result.Find(elements[j].commit, aIndex) then begin
          found := result.Data[aIndex];
          pmi^.parents[k].n := found^.n;
          pmi^.parents[k].commit := elements[j].commit;
          inc(k);
        end else begin
          {$IFDEF Debug}
          DebugLn('At %d parent %d (%.16x) is missing',[i, j, elements[j].commit]);
          {$ENDIF}
          pmi^.parents[k].n := -1;
          pmi^.parents[k].commit := elements[j].commit;
        end;
      end;
      if Length(pmi^.parents)<>k then begin
        aIndex := Length(lost);
        SetLength(lost, aIndex+1);
        lost[aIndex] := i;
      end;
    end;
  end;

  // try to find lost parents
  for i := 0 to Length(Lost)-1 do begin
    db.LoadItem(lost[i]);
    with db.Item do begin
      if not result.Find(OIDToQWord(CommitOID), j) then
        continue; // should not happen
      pmi := result.Data[j];
      for k:=0 to Length(pmi^.parents)-1 do
        if pmi^.parents[k].n<0 then begin
          if result.Find(pmi^.parents[k].commit, aIndex) then begin
            found := result.Data[aIndex];
            pmi^.parents[k].n := found^.n;
            {$IFDEF Debug}
            DebugLn('At %d found missing parent %d (%.16x)',[lost[i], k, pmi^.parents[k].commit]);
            {$ENDIF}
          end;
        end;
    end;
  end;

  {$ifdef Debug}
  ReportTicks('GetParentsMap');
  {$ifdef DumpCommitsAndParents}
  ReportCommitsAndParents(result);
  {$endif}
  {$endif}
end;

procedure ClearParentsMap(map: TParentsMap);
var
  pmi: PParentsMapItem;
  i: Integer;
begin
  for i:=0 to map.Count-1 do begin
    pmi := map.Data[i];
    pmi^.parents := nil;
    dispose(pmi);
  end;
  map.free;
  {$IFDEF Debug}
  ReportTicks('ClearingMap');
  {$ENDIF}
end;

procedure FindRelativesMap(var result: TItemIndexArray; parMap: TParentsMap);
var
  i, j, k, m, p, q: Integer;
  pmi: PParentsMapItem;
begin
  SetLength(result, parMap.Count);

  for i:=0 to Length(result)-1 do begin
    result[i].index := i;
    result[i].column := -1;
    result[i].parents := nil;
    result[i].childs := nil;
    result[i].lines := nil;
  end;

  for m:=0 to parMap.Count-1 do begin
    pmi := parMap.Data[m];
    i := pmi^.n;
    result[i].commit := parMap.Keys[m];
    //SetLength(result[i].parents, Length(pmi^.parents));
    for p := 0 to Length(pmi^.parents)-1 do
      if pmi^.parents[p].n>=0 then begin
        q := Length(result[i].parents);
        SetLength(result[i].parents, q+1);
        j := pmi^.parents[p].n;
        // found a parent of index i at index j
        result[i].parents[q] := j;
        // this means i is a child of j
        k := Length(result[j].childs);
        SetLength(result[j].childs, k+1);
        result[j].childs[k] := i;
      end;
  end;

  {$IFDEF Debug}
  ReportTicks('FindingRelatives');
  {$ENDIF}
end;

function FindSource(items: TItemIndexArray; ref:Integer; inChilds:boolean): Integer;
var
  arr: TIntArray;
  i, j: Integer;
begin
  result := LINE_SOURCE_COLUMN;
  if inChilds then arr := items[ref].childs
  else             arr := items[ref].parents;
  for i:=0 to Length(arr)-1 do begin
    j := arr[i];
    if items[j].column<>items[ref].column then begin
      result := j;
      break;
    end;
  end;
end;

procedure FindInternalMerges(items: TItemIndexArray; i: Integer; var columns: TColumnArray; j: Integer);
var
  a, dest, cur: Integer;
  Section: TColumnSection;
begin
  with columns[j] do begin
    // is not the first nor the last it should be an internal node
    // is this a merging node? what is the merging dest?
    dest := FindSource(items, i, true);

    // the source 'dest' must be in another column.
    if dest=LINE_SOURCE_COLUMN then
      exit;

    // dest must not be the 'last' of the others column's section
    // that case is handled in that column's section
    a := items[dest].column; cur := items[dest].section;
    if (a>=0) and (cur>=0) then begin
      section := columns[a].Sections[cur];
      if (dest>=section.last) then
        exit;
    end;

    for Section in Sections do
      with Section do begin
        // does it belongs to this section?
        if (dest<first) or (dest>last) then
          continue; // no

        if dest>first then begin
          // yes, tag from 'dest' to 'i'
          cur := dest;
          while cur<>i do begin
            for a := 0 to Length(items[cur].lines)-1 do begin
              if items[cur].lines[a].column=j then begin
                // found the right line
                if cur=dest then Include(items[cur].lines[a].flags, lifMerge)
                else             Include(items[cur].lines[a].flags, lifToMerge);
                items[cur].lines[a].source := dest;
              end;
            end;
            inc(cur);
          end;
        end;

      end;
  end;
end;

procedure AssignColumns(var result: TItemIndexArray; var columns: TColumnArray);
var
  i, j, n, p, column, section: Integer;
begin

  column := -1;
  i := 0;

  n := Length(result);
  while n>0 do begin

    // if starting from the top, create a new column
    // if i<>0 we are trying to find more sections in this column
    if i=0 then begin
      inc(Column);
      SetLength(Columns, column + 1);
      Columns[column].index := column;
    end;

    // find the first index with no assigned column
    while i<Length(result) do begin
      if result[i].column<0 then break;
      inc(i);
    end;
    if i=length(result) then begin
      // finished this column, check if all indexes were processed
      if n>0 then begin
        // not yet, continue with another column
        i := 0;
        continue;
      end;
      break; // we are done
    end;

    // start a new section
    section := Length(Columns[column].Sections);
    SetLength(Columns[column].Sections, section + 1);

    Columns[column].Sections[section].index := section;
    Columns[column].Sections[section].column := column;
    Columns[column].Sections[section].first := -1;
    Columns[column].Sections[section].last := -1;

    // using 'i' index as a grand child, track down the first parent.
    repeat

      result[i].column := column;
      result[i].section := section;

      if Columns[column].Sections[section].first<0 then begin
        Columns[column].Sections[section].first := i;
        Columns[column].Sections[section].head := -1;
        Columns[column].Sections[section].tail := -1;
        Columns[column].Sections[section].count := 0;
      end;
      Columns[column].Sections[section].last := i;

      dec(n);
      if n=0 then
        break;

      // now starting with 'i' descend down until we find
      // the first parent that has no assigned column
      j := 0;  p := -1;
      while j<Length(result[i].parents) do begin
        p := result[i].parents[j];
        if result[p].column=-1 then
          break; // found
        inc(j);
      end;
      if j=Length(result[i].parents) then begin
        // have no parents or all parents have a column
        // so we are at the end of a section, and 'i' is 'last'
        // TODO: NOTE: if 'p' is valid, then it's the 'tail'
        //             so the section should include it.

        // at what index 'i' should continue?
        // it should be past the assigned 'p's parent
        if (p>=0) and (p+1<Length(result)-1) then
          i := p + 1
        else
          i := 0;

        break;
      end;

      // the found parent is now the next index to process...
      i := p;

    until false;

  end;

  {$IFDEF Debug}
  ReportTicks('ColumnsIdexing');
  ReportColumns('After columns indexing', columns);
  ReportTicks('ReportingColumnsIdexing');
  {$ENDIF}
end;

procedure FindHeadsAndTails(var result: TItemIndexArray; var columns: TColumnArray);
var
  j, k, p, section: Integer;
begin
  // all indexes have now assigned columns, find heads and tails
  for j:=0 to Length(columns)-1 do begin

    for section := 0 to Length(columns[j].Sections)-1 do begin
      k := columns[j].Sections[section].first;
      for p in result[k].childs do begin
        if result[p].column<j then begin
          columns[j].Sections[section].head := p;
          break;
        end;
      end;
      if columns[j].Sections[section].head<0 then
        columns[j].Sections[section].head := k;

      k := columns[j].Sections[section].last;
      for p in result[k].parents do begin
        if result[p].column<j then begin
          columns[j].Sections[section].tail := p;
          break;
        end;
      end;
      if columns[j].Sections[section].tail<0 then
        columns[j].Sections[section].tail := k;

      columns[j].Sections[section].count := columns[j].Sections[section].tail - columns[j].Sections[section].head + 1;
    end;
  end;

  {$IFDEF Debug}
  ReportTicks('FindHeadsAndTails');
  ReportColumns('After FindHeadsAndTails', columns);
  ReportTicks('Reporting Columns after FindHeadsAndTails');
  {$ENDIF}
end;

procedure MapLinesAndColumns(var result: TItemIndexArray; var columns: TColumnArray; out maxColumns: Integer);
var
  i, j, k, section: Integer;
  flags: TLineItemFlags;
begin
  MaxColumns := 1;
  if Length(Columns)>=1 then
    for j:=0 to Length(columns)-1 do
      for section := 0 to Length(columns[j].Sections)-1 do
        with columns[j].Sections[section] do begin

          // distribute nodes and lines
          for i:= head to tail do begin

            k := Length(result[i].lines);
            SetLength(result[i].lines, k+1);
            if k+1>MaxColumns then
              MaxColumns := k+1;
            result[i].lines[k].column := j;
            result[i].lines[k].columnIndex := k;

            flags := [];
            if result[i].column=j then begin
              // a node should be drawn here
              Include(flags, lifNode);
              // what about the tip?
              if (i=first) then Include(flags, lifFirst) else
              if (i=last)  then Include(flags, lifLast)
              else              FindInternalMerges(result, i, columns, j)
            end else
            // a line should be drawn here, what kind of line?
            if (i>=head) and (i<first) then begin
              result[i].lines[k].source := FindSource(result, first, true);
              if i=head then Include(flags, lifMerge)
              else           Include(flags, lifToMerge);
            end else
            if (i>first) and (i<=last) then begin
              result[i].lines[k].source := LINE_SOURCE_COLUMN;
              Include(flags, lifInternal);
            end else
            if (i>last) and (i<=tail) then begin
              result[i].lines[k].source := FindSource(result, last, false);
              if i=tail then Include(flags, lifBorn)
              else           Include(flags, lifToBorn);
            end;

            result[i].lines[k].Flags := flags;

          end;
        end;

  {$IFDEF Debug}
  ReportTicks('MapLinesAndColumns');
  DebugLn('Max concurrent columns: %d',[MaxColumns]);
  {$ENDIF}
end;

function GetItemIndexes(db: TDbIndex; withColumns: boolean; out
  maxColumns: Integer): TItemIndexArray;
var
  parMap: TParentsMap;
  columns: TColumnArray;
begin
  {$IFDEF Debug}
  resetTicks;
  {$ENDIF}

  parMap := GetParentsMap(db);

  result := nil;
  FindRelativesMap(result, parMap);

  // parMap is not needed anymore
  ClearParentsMap(parMap);

  maxColumns := 0;
  if not withColumns then
    exit;

  columns := nil;
  AssignColumns(result, columns);

  FindHeadsAndTails(result, columns);

  //SortColumns(columns);

  {$IFDEF Debug}
  //ReportTicks('SortColumns');
  //ReportColumns('After sorting', columns);
  {$ENDIF}

  MapLinesAndColumns(result, columns, maxColumns);

  maxColumns := Length(Columns);

  {$IFDEF Debug}
  ReportItemIndexArray(result);

  //// now the result
  ReportResult(result, columns);
  {$ENDIF}

end;

{ TDbIndex }

//procedure TDbIndex.RecoverIndex;
//var
//  w: word;
//  IndxRec: TIndexRecord;
//  n: Integer;
//begin
//  DebugLn('Recovering Index');
//  n := 0;
//  fIndexStream.Clear;
//  fCacheStream.Position := SizeOf(cardinal);
//  while fCacheStream.Position<fCacheStream.Size do begin
//    IndxRec.offset := fCacheStream.Position;
//    w := fCacheStream.ReadWord;
//    IndxRec.size := w + SizeOf(word);
//    fIndexStream.WriteBuffer(indxRec, SIZEOF_INDEX);
//    inc(n);
//    fCacheStream.Seek(w, soFromCurrent);
//  end;
//  DebugLn('Recovered %d records vs %d',[fIndexStream.Size div SIZEOF_INDEX, n]);
//  DebugLn('Cache: Size=%d Position=%d',[fCacheStream.Size, fCacheStream.Position]);
//  SaveIndexStream;
//end;

function TDbIndex.GetAcceptingNewRecords: boolean;
begin
  result := not fReadOnly and ((fMaxRecords=0) or (Count(true)<fMaxRecords));
end;

function TDbIndex.GetActive: boolean;
begin
  result := fCacheStream<>nil;
end;

function TDbIndex.GetInfo: string;
begin
  result := format('%d',[Count]);
end;

procedure TDbIndex.ReIndex;
var
  newSize: Integer;
  buf, p, q: Pbyte;
  tmp, x: TStream;
begin
  EnterCriticalSection(fCursorLock);
  try
    tmp := TMemoryStream.Create;
    // copy the new part at the start
    newSize := fIndexStream.Size - fOldIndexOffset;
    fIndexStream.Position := fOldIndexOffset;
    tmp.CopyFrom(fIndexStream, newSize);

    // now copy the old data
    fIndexStream.Position := 0;
    tmp.CopyFrom(fIndexStream, fOldIndexOffset);

    // the re-indexed stream is now ready, swap streams pointers for a moment
    x := tmp; tmp := fIndexStream; TStream(fIndexStream) := x;

    tmp.Size := 0;
    fIndexStream.Position := 0;
    tmp.CopyFrom(fIndexStream, fIndexStream.Size);

    // the re-indexed stream is now ready, finally swap streams pointers
    x := tmp; tmp := fIndexStream; TStream(fIndexStream) := x;

  finally
    tmp.Free;
    LeaveCriticalSection(fCursorLock);
  end;
end;

function TDbIndex.GetFileName(aIndex: Integer): string;
begin
  result := fDir + BASE_FILENAME;
  case aIndex of
    FILENAME_INDEX: result += '.logindex';
    FILENAME_CACHE: result += '.logcache';
    FILENAME_INDEXTMP: result += '.logindextmp';
  end;
end;

procedure TDbIndex.GetIndexRecord(var aIndex: Integer; out
  indxRec: TIndexRecord; unfiltered: boolean);
var
  indexOffset: SizeInt;
  theIndex: Integer;
begin

  if unfiltered then begin
    if aIndex<0 then
      aIndex := (fIndexStream.Size div SIZEOF_INDEX)-1;
    theIndex := aIndex;
  end else
  if fFilter<>nil then begin
    if (aIndex<0) or (aIndex>Length(fFilter)-1) then
      aIndex := Length(fFilter)-1;
    theIndex := fFilter[aIndex];
  end else begin
    if aIndex<0 then
      // get the oldest item
      aIndex := Count - 1;
    theIndex := aIndex;
  end;

  indexOffset := theIndex * SIZEOF_INDEX;

  EnterCriticalSection(fCursorLock);
  try
    fIndexStream.Position := indexOffset;
    fIndexStream.Read(indxRec{%H-}, SIZEOF_INDEX);
  finally
    LeaveCriticalSection(fCursorLock);
  end;
end;

procedure TDbIndex.CacheBufferFromItem(out aBuf: PChar; out len: word);
var
  p: pchar;
  rlen: word;

  procedure StoreString(s:RawByteString; byteSize:boolean );
  var
    b: byte;
    w: word;
    l: Integer;
  begin
    l := Length(s);
    if byteSize then begin
      b := min(l, high(byte));
      Move(b, p^, sizeof(byte));
      inc(p, sizeof(byte));
    end else begin
      w := min(l, high(word));
      Move(w, p^, sizeOf(word));
      inc(p, sizeof(word));
    end;

    if l>0 then begin
      Move(s[1], p^, l);
      inc(p, l);
    end;
  end;

begin

  len := SizeOf(Word);
  len += SizeOf(fItem.CommiterDate);
  len += Min(Length(fItem.ParentOID), High(word)) + 1;
  len += Min(Length(fItem.CommitOID), High(Byte)) + 1;
  len += Min(Length(fItem.Author), High(Byte)) + 1;
  len += Min(Length(fItem.Email), High(Byte)) + 1;
  len += Min(Length(fItem.Subject), High(word)) + 2;

  GetMem(aBuf, len);
  p := aBuf;

  // store record size not including the record size itself
  rlen := len - SizeOf(word);
  Move(rlen, p^, SizeOf(word));
  inc(p, sizeOf(word));

  // store commit date
  Move(fItem.CommiterDate, p^, SizeOf(fItem.CommiterDate));
  inc(p, SizeOf(fItem.CommiterDate));

  // store remaining fields
  StoreString(fItem.ParentOID, false);
  StoreString(fItem.CommitOID, true);
  StoreString(fItem.Author, true);
  StoreString(fItem.Email, true);
  StoreString(fItem.Subject, false);
end;

procedure TDbIndex.ItemFromCacheBuffer;
var
  p: pchar;

  function NextByteString: RawByteString;
  var
    len: byte;
  begin
    len := PByte(p)^;
    SetLength(result, len);
    inc(p);
    Move(p^, result[1], len);
    inc(p, len);
  end;

  function NextWordString: RawByteString;
  var
    len: LongWord;
  begin
    len := PWord(p)^;
    SetLength(result, len);
    inc(p, SizeOf(Word));
    Move(p^, result[1], len);
    inc(p, len);
  end;

  function NextInt64: Int64;
  begin
    result := PInt64(p)^;
    inc(p, SizeOf(Int64));
  end;

begin

  Finalize(fItem);

  p := fBuffer;
  inc(p, SizeOf(word));
  fItem.CommiterDate := NextInt64;
  fItem.ParentOID := NextWordString;
  fItem.CommitOID := NextByteString;
  fItem.Author := NextByteString;
  fItem.Email := NextByteString;
  fItem.Subject := NextWordString;
end;

procedure TDbIndex.ItemFromLogBuffer(aBuf: PChar);
var
  p: PChar;

  function NextString: RawbyteString;
  var
    q: pchar;
  begin
    q := strpos(p, #2);
    if q<>nil then begin
      SetString(result, p, q-p);
      p := q + 1;
    end else
      result := p;
  end;

begin

  Finalize(fItem);

  p := aBuf;
  fItem.CommiterDate := StrToInt64Def(NextString, 0);
  fItem.ParentOID := NextString;
  fItem.CommitOID := NextString;
  fItem.Author := NextString;
  fItem.Email := NextString;
  fItem.Subject := NextString;

end;

procedure TDbIndex.DumpItem;
var
  indxRec: TIndexRecord;
begin
  DebugLn;
  DebugLn('          Index: %d of %d',[fLoadedItemIndex, fIndexStream.Size div SIZEOF_INDEX]);
  DebugLn('    Commit Date: %d (%s)',[fItem.CommiterDate, DateTimeToStr(UnixToDateTime(fItem.CommiterDate))]);
  DebugLn('     Parent OID: %s',[fItem.ParentOID]);
  DebugLn('     Commit OID: %s',[fItem.CommitOID]);
  DebugLn('         Author: %s',[fItem.ParentOID]);
  DebugLn('          Email: %s',[fItem.Email]);
  DebugLn('        Subject: %s',[fItem.Subject]);
  GetIndexRecord(fLoadedItemIndex, indxRec, false);
  DebugLn('   Cache Offset: %d', [indxRec.offset]);
  DebugLn('Cache Item Size: %d', [indxRec.size]);
end;

constructor TDbIndex.Create(dir: string);
begin
  inherited Create;
  fDir := IncludeTrailingPathDelimiter(dir);
  fMaxBufferSize := 1024 * 4;
  GetMem(fBuffer, fMaxBufferSize);
  fLoadedItemIndex := -1;
  InitCriticalSection(fCursorLock);
end;

destructor TDbIndex.Destroy;
begin
  DoneCriticalSection(fCursorLock);
  fIndexStream.Free;
  fCacheStream.Free;
  Finalize(fItem);
  FreeMem(fBuffer);
  inherited Destroy;
end;

procedure TDbIndex.Open;
var
  aFileCache, aFileIndex: string;
  mode, aVersion: word;
  sig: cardinal;
begin

  if fCacheStream<>nil then
    exit;

  aFileCache := GetFilename(FILENAME_CACHE);
  aFileIndex := GetFilename(FILENAME_INDEX);

  if gblInvalidateCache then begin
    DeleteFile(aFileCache);
    DeleteFile(aFileIndex);
    gblInvalidateCache := false;
  end;

  mode := fmOpenReadWrite + fmShareDenyWrite;
  if not FileExists(aFileCache) then begin
    if fReadOnly then
      raise Exception.CreateFmt('Cache file %s do not exists',[aFileCache]);
    mode += fmCreate;
  end;
  fCacheStream := TFileStream.Create(aFileCache, mode);

  if mode and fmCreate = fmCreate then begin
    sig := NToBE((PGM_SIGNATURE shl 16) or PGM_VERSION);
    fCacheStream.WriteDWord(sig);
  end
  else begin
    sig := BeToN(fCacheStream.ReadDWord);
    aVersion := sig and $FFFF;
    sig := sig shr 16;
    if (sig<>PGM_SIGNATURE) or (aVersion<PGM_VERSION) then begin
      if fReadOnly then
        raise Exception.CreateFmt('Invalid cache file %s',[aFileCache]);
      // this is an old cache file, recreate it
      DeleteFile(aFileIndex);
      sig := NToBE((PGM_SIGNATURE shl 16) or PGM_VERSION);
      fCacheStream.position := 0;
      fCacheStream.WriteDWord(sig);
      fCacheStream.Size := fCacheStream.Position;
    end;
  end;

  if fIndexStream=nil then begin
    mode := fmOpenReadWrite + fmShareDenyWrite;
    if not FileExists(aFileIndex) then begin
      if fReadOnly then
        raise Exception.CreateFmt('Index file %s do not exists',[aFileIndex]);
      mode += fmCreate;
    end;
    fIndexStream := TFileStream.Create(aFileIndex, mode);
  end;

end;

procedure TDbIndex.ThreadStart(aHead: boolean);
begin
  fOldCount := fIndexStream.Size div SIZEOF_INDEX;
  fOldIndexSize := fIndexStream.Size;
  fOldIndexOffset := fIndexStream.Size;
  fCacheUpdate := fCacheStream.Size>0;
  fHead := aHead;
end;

procedure TDbIndex.ThreadDone;
var
  records: Integer;
begin
  records := (fIndexStream.Size - fOldIndexSize) div SIZEOF_INDEX;
  if (records<>0) then begin
    // there are changes in the index, if they are for 'head' and
    // arent brand new, re-index
    if fHead and (fOldIndexSize>0) then
      ReIndex;

    FileFlush(fIndexStream.Handle);
    FileFlush(fCacheStream.Handle);
  end;

  DebugLn('TDbIndex.ThreadDone: for %s there are %d new records',[BoolToStr(fHead, 'HEAD', 'TAIL'), records]);
end;

procedure TDbIndex.ThreadStore(buf: pchar; size: Integer);
var
  p, q, t: PChar;
  tmp, m: pbyte;
  field: Integer;
  aDate: Int64;
  cd: Integer;
  b: byte;
  w: word;
  indxRec: TIndexRecord;
begin

  field := 0;
  p := buf;
  {      unix date              commitOID
    buf: 10+1 + P(arentOID)+1 + 40+1 + A(uthor)+1 + E(mail)+1 + S(ubject)+1 = Size
         56 + P+A+E+S = Size  so  P+A+E+S = Size - 56

    tmp: 8 + 2+P + 1+40 + 1+A + 1+E + 2+S = tmpSize
         55 + P+A+E+S = tmpSize
         55 + Size  - 56 = tmpSize  so  tmpSize = Size - 1
  }
  GetMem(tmp, size + 11); // only size-1 is really needed, some extra wont hurt
  m := tmp;

  t := p + size;
  while (p<t) and (field<7) do begin
    q := strpos(p, #2);
    if q<>nil then begin
      case field of
        FIELD_DATE:
          begin
            q^ := #0;
            val(p, aDate, cd);
            q^ := #2;
            Move(aDate, m^, sizeof(aDate));
            inc(m, sizeof(aDate));
          end;
        FIELD_COMMITOID, FIELD_AUTHOR, FIELD_EMAIL:
          begin
            b := Min(High(byte), q-p);
            m^ := b; inc(m);
            Move(p^, m^, b); inc(m, b);
          end;
        FIELD_PARENTOID, FIELD_SUBJECT:
          begin
            w := Min(High(word), q-p);
            pword(m)^ := w; inc(m, 2);
            move(p^, m^, w); inc(m, w);
          end;
      end;
      inc(field);
    end else begin
      // should not happen!!!
    end;

    p := q + 1;
  end;

  w := m - tmp;

  EnterCriticalSection(fCursorLock);
  try
    fCacheStream.Seek(0, soFromEnd);
    indxRec.Offset := fCacheStream.Position;
    fCacheStream.WriteWord(w);
    fCacheStream.WriteBuffer(tmp^, w);

    indxRec.size := w + 2;
    fIndexStream.Seek(0, soFromEnd);
    fIndexStream.WriteBuffer(indxRec, SIZEOF_INDEX);
  finally
    LeaveCriticalSection(fCursorLock);
  end;

  freeMem(tmp);
end;

function TDbIndex.GetCachedItem(aIndex: Integer; unfiltered, tryToFix: boolean): boolean;
var
  indxRec: TIndexRecord;
  w: word;
begin
  result := (fIndexStream<>nil) and (fIndexStream.Size>0);
  if result then begin

    GetIndexRecord(aIndex, indxRec, unfiltered);

    result := indxRec.offset + indxRec.size <= fCacheStream.Size ;
    if result then begin

      if indxRec.size>fMaxBufferSize then begin
        fMaxBufferSize := indxRec.size * 2;
        ReallocMem(fBuffer, fMaxBufferSize);
      end;

      EnterCriticalSection(fCursorLock);
      try
        fCacheStream.Position := indxRec.offset;
        fCacheStream.Read(fBuffer^, indxRec.size);
      finally
        LeaveCriticalSection(fCursorLock);
      end;

      w := PWord(fBuffer)^;
      if indxRec.Size-2<>w then begin

        if tryToFix then begin
          // the cache files are corrupt save what seems right, it will be fixed
          // the next time the log is requested.
          EnterCriticalSection(fCursorLock);
          try
            fCacheStream.Size := indxRec.offset;
            fIndexStream.Size := fIndexStream.Position - SIZEOF_INDEX;
            FileFlush(fIndexStream.Handle);
            FileFlush(fCacheStream.Handle);
          finally
            LeaveCriticalSection(fCursorLock);
          end;
        end;

        raise Exception.CreateFmt('Inconsistent data at index %d, expected %d found %d',[aIndex, indxRec.Size-2, w]);
      end;

    end;

  end;
end;

function TDbIndex.Count(unfiltered: boolean): Integer;
begin
  if fIndexStream=nil then
    exit(0);

  if not unfiltered and (fFilter<>nil) then
    result := Length(fFilter)
  else
    result := fIndexStream.Size div SIZEOF_INDEX;

  if (not unfiltered) and (fMaxRecords>0) and (result>fMaxRecords) then
    result := fMaxRecords;
end;

function TDbIndex.LoadItem(aIndex: Integer; unfiltered: boolean): boolean;
begin
  result := GetCachedItem(aIndex, unfiltered, true);
  if result then begin
    ItemFromCacheBuffer;
    fLoadedItemIndex := aIndex;
  end;
end;

procedure TDbIndex.SetFilter(arr: TIntArray);
var
  maxIndex, i: Integer;
begin
  if arr=nil then
    fFilter := nil
  else begin
    if (fIndexStream=nil) or (fIndexStream.Size=0) then
      raise Exception.Create('Trying to set a filter while the db is not initialized');
    maxIndex := Count - 1;
    // check that indices are within the range of the index
    for i:=0 to Length(arr)-1 do
      if (arr[i]<0) or (arr[i]>maxIndex) then
        raise Exception.CreateFmt('The filter has an invalid entry at %d',[i]);
    // copy filter indices
    SetLength(fFilter, Length(arr));
    Move(arr[0], fFilter[0], Length(arr)*SizeOf(Integer));
  end;
end;

type
  // ref: https://gist.github.com/mikebsg01/52ad3ae8277e9c424823a79e4f7b0cf6

  TIntList = specialize TFpGList<Integer>;
  TIntStack = TIntList;
  TBoolArr = array of boolean;

  { TGraph }

  TGraph = class
  private
    fV: Integer;
    fAdj: array of TIntList;
    procedure TopologicalSortUtil(v: Integer; visited:TBoolArr; stack: TIntStack);
  public
    constructor Create(aV: Integer);
    destructor Destroy; override;
    procedure AddEdge(v, w: Integer);
    function TopologicalSort: TIntStack;
  end;

{ TGraph }

procedure TGraph.TopologicalSortUtil(v: Integer; visited: TBoolArr;
  stack: TIntStack);
var
  i: Integer;
begin
  // Mark the current node as visited.
  visited[v] := true;

  // Recur for all the vertices adjacent to this vertex
  for i := fAdj[v].First to fAdj[v].Last do
    if not visited[i] then
      TopologicalSortUtil(i, visited, stack);

  // Push current vertex to stack which stores result
  stack.Add(v);
end;

constructor TGraph.Create(aV: Integer);
var
  i: Integer;
begin
  inherited Create;
  fV := aV;
  SetLength(fAdj, aV);
  for i:=0 to aV-1 do
    fAdj[i] := TIntList.Create;
end;

destructor TGraph.Destroy;
var
  i: Integer;
begin
  for i:=0 to fV-1 do
    fAdj[i].Free;
  inherited Destroy;
end;

procedure TGraph.AddEdge(v, w: Integer);
begin
  fAdj[v].Add(w);
end;

function TGraph.TopologicalSort: TIntStack;
var
  stack: TIntStack;
  visited: TBoolArr;
  i: Integer;
begin

  result := TIntList.Create;
  SetLength(visited, fV);

  // Mark all the vertices as not visited
  for i:=0 to fV-1 do visited[i] := false;

  // Call the recursive helper function to store Topological Sort
  // starting from all vertices one by one
  for i:=0 to fV-1 do
    if not visited[i] then
      TopologicalSortUtil(i, visited, result);

end;

procedure TestGraph;
var
  graph: TGraph;
  stack: TIntStack;
  i: Integer;
begin
  graph  := TGraph.create(8);
  graph.addEdge(7, 6);
  graph.addEdge(7, 5);
  graph.addEdge(6, 4);
  graph.addEdge(6, 3);
  graph.addEdge(5, 4);
  graph.addEdge(5, 2);
  graph.addEdge(3, 1);
  graph.addEdge(2, 1);
  graph.addEdge(1, 0);
  stack := graph.TopologicalSort;
  graph.free;

  // Print contents of stack
  for i:=stack.Count-1 downto 0 do
    DbgOut('%d ',[stack[i]]);

  stack.Free;
end;

procedure TDbIndex.TopoSort;
var
  graph: TGraph;
  parArray: TParentsArray;
  i, j, x, dummy: Integer;
  arr: TIntArray;
  stack: TIntStack;
  indxArr: TItemIndexArray;
begin
  //TestGraph

  SetFilter(nil);

  indxArr := GetItemIndexes(self, true, dummy);

  // this listing clearly shows list of indices and parent indices each index
  // it have
  //for i:=0 to Length(indxArr)-1 do begin
  //  DbgOut('%3d (%d): ',[i, length(indxArr[i].parents)]);
  //  for j:=0 to Length(indxArr[i].parents)-1 do
  //    DbgOut('%3d ',[indxArr[i].parents[j]]);
  //  DebugLn;
  //end;

  //for external tests...
  //DebugLn('graph := TGraph.Create(%d);',[Length(indxArr)]);
  //for i:=0 to Length(indxArr)-1 do begin
  //  with indxArr[i] do begin
  //    for x in parents do
  //      DebugLn('graph.AddEdge(%d, %d);',[x, index]);
  //  end;
  //end;


  graph := TGraph.Create(Count);
  try

    for i:=0 to Length(indxArr)-1 do
      with indxArr[i] do begin
        for x in parents do
          graph.AddEdge(x, index)
      end;

    stack := graph.TopologicalSort;
    setLength(arr, stack.Count);

    // This tests demonstrates that the data is already ordered
    // topologically.
    //DebugLn;
    //x := 0;
    //for i:=stack.Count-1 downto 0 do begin
    //  DbgOut('%3d ',[stack[i]]);
    //  if (x+1) mod 20 = 0 then DebugLn;
    //  inc(x);
    //end;

    //SetFilter(arr);

  finally
    graph.Free;
  end;
end;

function TDbIndex.FindCommitSha(sha: string; startAt: Integer): Integer;
var
  i, n, nailLen: Integer;
  indexOffset: Int64;
  indxRec: TIndexRecord;
  b: byte;
  w: word;
  p, nail: pchar;
begin
  // find directly in the database the asked sha
  result := -1;
  if sha='' then
    exit;         // a commit cannot be empty
  sha := lowercase(sha);

  if (fIndexStream<>nil) and (fCacheStream<>nil) then begin
    nail := @sha[1];
    nailLen := Length(sha);
    if startAt<0 then startAt := 0;
    for i:=startAt to (FIndexStream.Size div SIZEOF_INDEX)-1 do begin

      if not GetCachedItem(i, true, false) then
        exit;

      p := fBuffer;           // points to the record data
      inc(p, 2);              // skip record size
      inc(p, SizeOf(Int64));  // skip date
      w := PWord(p)^;         // read parents length
      inc(p, 2+w);            // skip parents
      b := PByte(p)^;         // read commit length
      inc(p);

      n := strlcomp(p, nail, Min(b, nailLen));
      if n=0 then begin
        result := i;
        exit;
      end;

    end;
  end;
end;

end.

