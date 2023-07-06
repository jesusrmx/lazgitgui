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

  Constructs the graph information in a thread.
}

unit unitgraphbuild;

{$mode ObjFPC}{$H+}

{.$define Debug}

{$ifdef Debug}
  {.$define ReportGetParentsMap}
  {.$define ReportRelatives}
  {.$define ReportTGraph}
  {.$define ReportColumns}
  {.$define ReportItemIndexArray}
  {.$define ReportGraph}
{$endif}

interface

uses
  Classes, SysUtils, Math, LazLogger,
  unitgittypes, unitgitutils, unitdbindex, unittoposort;

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

  { TGraphBuilderThread }

  TGraphBuilderThread = class(TThread)
  private
    fDb: TDbIndex;
    fEnd: Integer;
    fIndexArray: TItemIndexArray;
    fMaxColumns: Integer;
    fStart: Integer;
    fWithColumns: boolean;
    procedure FindRelativesMap(parMap: TParentsMap);
    function FindSource(ref:Integer; inChilds:boolean): Integer;
    procedure FindInternalMerges(i: Integer; var columns: TColumnArray; j: Integer);
    procedure AssignColumns(var columns: TColumnArray);
    procedure FindHeadsAndTails(var columns: TColumnArray);
    procedure MapLinesAndColumns(var columns: TColumnArray; out maxColumns: Integer);
    procedure SetEnd(AValue: Integer);
    procedure TopoSort(map: TParentsMap);
  public
    constructor Create(db: TDbIndex);
    procedure Execute; override;

    property IndexArray: TItemIndexArray read fIndexArray;
    property WithColumns: boolean read fWithColumns write fWithColumns;
    property MaxColumns: Integer read fMaxColumns;
    property StartIndex: Integer read fStart write fStart;
    property EndIndex: Integer read fEnd write SetEnd;
  end;

implementation

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


{$IFDEF Debug}

{$ifdef ReportColumns}
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
{$endif}

{$ifdef ReportTGraph}
procedure ReportTGraph(fIndexArray: TItemIndexArray);
var
  i, x, c: Integer;
begin
  DebugLn('graph := TGraph.Create(%d);',[Length(fIndexArray)]);
  c := 0;
  for i:=0 to Length(fIndexArray)-1 do begin
    with fIndexArray[i] do begin
      for x in parents do begin
        DebugLn('graph.AddEdge(%d, %d);',[x, index]);
        inc(c);
      end;
    end;
  end;
  DebugLn;
  DebugLn('Algs4 file');
  DebugLn('%d',[Length(fIndexArray)]);
  DebugLn('%d',[c]);
  for i:=0 to Length(fIndexArray)-1 do
    with fIndexArray[i] do begin
      for x in parents do
        DebugLn('%d %d',[x, index]);
    end;
  DebugLn('Algs4 file end');

  ReportTicks('Reporting TGraph + Algs4 file');
end;

{$endif}

{$ifdef ReportItemIndexArray}
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
{$endif}

{$ifdef ReportGraph}

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

procedure ReportGraph(items: TItemIndexArray; columns: TColumnArray);
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
{$endif}

{$ENDIF}

{ TGraphBuilderThread }

procedure TGraphBuilderThread.FindRelativesMap(parMap: TParentsMap);
var
  i, j, k, m, p, q: Integer;
  pmi: PParentsMapItem;
begin
  SetLength(fIndexArray, fDb.Count);

  for i:=0 to Length(fIndexArray)-1 do begin
    if not (ifAllocated in fIndexArray[i].iflags) then begin
      fIndexArray[i].index := i;
      fIndexArray[i].column := -1;
      fIndexArray[i].iflags := [ifAllocated];
    end;
  end;

  for m:=0 to parMap.Count-1 do begin
    pmi := parMap.Data[m];
    i := pmi^.n;
    fIndexArray[i].commit := parMap.Keys[m];
    //SetLength(fIndexArray[i].parents, Length(pmi^.parents));
    for p := 0 to Length(pmi^.parents)-1 do
      if pmi^.parents[p].n>=0 then begin
        q := Length(fIndexArray[i].parents);
        SetLength(fIndexArray[i].parents, q+1);
        j := pmi^.parents[p].n;
        // found a parent of index i at index j
        fIndexArray[i].parents[q] := j;
        // this means i is a child of j
        k := Length(fIndexArray[j].childs);
        SetLength(fIndexArray[j].childs, k+1);
        fIndexArray[j].childs[k] := i;
      end;

    if pmi^.lostandfound>=0 then begin
      // a parent was lost because it was delivered by git log
      // as sooner as it's child (some log entries have the same timestamp)
      // so a lost&found parent should have an index less than the item.
      if pmi^.lostandfound>=i then
        DebugLn('Invalid order for item %d and lost&found %d',[i, pmi^.lostandfound])
      else
        for j:=pmi^.lostandfound to i do begin
          include(fIndexArray[j].iflags, ifReorder);
          if j=pmi^.lostandfound then Include(fIndexArray[j].iflags, ifFirstReorder);
          if j=i then Include(fIndexArray[j].iflags, ifLastReorder);
        end;
    end;
  end;

  {$IFDEF Debug}
  ReportTicks('FindingRelatives');
  {$ifdef ReportRelatives}
  ReportRelatives(fIndexArray);
  {$endif}
  {$ifdef ReportTGraph}
  ReportTGraph(fIndexArray);
  {$endif}
  {$ENDIF}
end;

function TGraphBuilderThread.FindSource(ref: Integer; inChilds: boolean
  ): Integer;
var
  arr: TIntArray;
  i, j: Integer;
begin
  result := LINE_SOURCE_COLUMN;
  if inChilds then arr := fIndexArray[ref].childs
  else             arr := fIndexArray[ref].parents;
  for i:=0 to Length(arr)-1 do begin
    j := arr[i];
    if fIndexArray[j].column<>fIndexArray[ref].column then begin
      result := j;
      break;
    end;
  end;
end;

procedure TGraphBuilderThread.FindInternalMerges(i: Integer;
  var columns: TColumnArray; j: Integer);
var
  a, dest, cur: Integer;
  Section: TColumnSection;
begin
  with columns[j] do begin
    // is not the first nor the last it should be an internal node
    // is this a merging node? what is the merging dest?
    dest := FindSource(i, true);

    // the source 'dest' must be in another column.
    if dest=LINE_SOURCE_COLUMN then
      exit;

    // dest must not be the 'last' of the others column's section
    // that case is handled in that column's section
    a := fIndexArray[dest].column; cur := fIndexArray[dest].section;
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
          cur := Min(dest, i);
          while cur<>i do begin
            for a := 0 to Length(fIndexArray[cur].lines)-1 do begin
              if fIndexArray[cur].lines[a].column=j then begin
                // found the right line
                if cur=dest then Include(fIndexArray[cur].lines[a].flags, lifMerge)
                else             Include(fIndexArray[cur].lines[a].flags, lifToMerge);
                fIndexArray[cur].lines[a].source := dest;
              end;
            end;
            inc(cur);
          end;
        end;

      end;
  end;
end;

procedure TGraphBuilderThread.AssignColumns(var columns: TColumnArray);
var
  i, j, n, p, column, section: Integer;
begin

  column := -1;
  i := 0;

  n := Length(fIndexArray);
  while n>0 do begin

    // if starting from the top, create a new column
    // if i<>0 we are trying to find more sections in this column
    if i=0 then begin
      inc(Column);
      SetLength(Columns, column + 1);
      Columns[column].index := column;
    end;

    // find the first index with no assigned column
    while i<Length(fIndexArray) do begin
      if fIndexArray[i].column<0 then break;
      inc(i);
    end;
    if i=length(fIndexArray) then begin
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

      fIndexArray[i].column := column;
      fIndexArray[i].section := section;

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
      while j<Length(fIndexArray[i].parents) do begin
        p := fIndexArray[i].parents[j];
        if fIndexArray[p].column=-1 then
          break; // found
        inc(j);
      end;
      if j=Length(fIndexArray[i].parents) then begin
        // have no parents or all parents have a column
        // so we are at the end of a section, and 'i' is 'last'
        // TODO: NOTE: if 'p' is valid, then it's the 'tail'
        //             so the section should include it.

        // at what index 'i' should continue?
        // it should be past the assigned 'p's parent
        if (p>=0) and (p+1<Length(fIndexArray)-1) then
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
  {$IFDEF ReportColumns}
  ReportColumns('After columns indexing', columns);
  ReportTicks('ReportingColumnsIdexing');
  {$ENDIF}
  {$ENDIF}
end;

procedure TGraphBuilderThread.FindHeadsAndTails(var columns: TColumnArray);
var
  j, k, p, section: Integer;
begin
  // all indexes have now assigned columns, find heads and tails
  for j:=0 to Length(columns)-1 do begin

    for section := 0 to Length(columns[j].Sections)-1 do begin
      k := columns[j].Sections[section].first;
      for p in fIndexArray[k].childs do begin
        if fIndexArray[p].column<j then begin
          columns[j].Sections[section].head := p;
          break;
        end;
      end;
      if columns[j].Sections[section].head<0 then
        columns[j].Sections[section].head := k;

      k := columns[j].Sections[section].last;
      for p in fIndexArray[k].parents do begin
        if fIndexArray[p].column<j then begin
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
  {$ifdef ReportColumns}
  ReportColumns('After FindHeadsAndTails', columns);
  ReportTicks('Reporting Columns after FindHeadsAndTails');
  {$endif}
  {$ENDIF}
end;

procedure TGraphBuilderThread.MapLinesAndColumns(var columns: TColumnArray;
  out maxColumns: Integer);
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

            k := Length(fIndexArray[i].lines);
            SetLength(fIndexArray[i].lines, k+1);
            if k+1>MaxColumns then
              MaxColumns := k+1;
            fIndexArray[i].lines[k].column := j;
            fIndexArray[i].lines[k].columnIndex := k;

            flags := [];
            if fIndexArray[i].column=j then begin
              // a node should be drawn here
              Include(flags, lifNode);
              // what about the tip?
              if (i=first) then Include(flags, lifFirst) else
              if (i=last)  then Include(flags, lifLast)
              else              FindInternalMerges(i, columns, j)
            end else
            // a line should be drawn here, what kind of line?
            if (i>=head) and (i<first) then begin
              fIndexArray[i].lines[k].source := FindSource(first, true);
              if i=head then Include(flags, lifMerge)
              else           Include(flags, lifToMerge);
            end else
            if (i>first) and (i<=last) then begin
              fIndexArray[i].lines[k].source := LINE_SOURCE_COLUMN;
              Include(flags, lifInternal);
            end else
            if (i>last) and (i<=tail) then begin
              fIndexArray[i].lines[k].source := FindSource(last, false);
              if i=tail then Include(flags, lifBorn)
              else           Include(flags, lifToBorn);
            end;

            fIndexArray[i].lines[k].Flags := flags;

          end;
        end;

  {$IFDEF Debug}
  ReportTicks('MapLinesAndColumns');
  DebugLn('Max concurrent columns: %d',[MaxColumns]);
  {$ENDIF}
end;

procedure TGraphBuilderThread.SetEnd(AValue: Integer);
begin
  if fEnd=MAXINT then
    fEnd := fDb.Count-1;

  if fEnd = AValue then
    Exit;

  fEnd := AValue;
end;

procedure TGraphBuilderThread.TopoSort(map: TParentsMap);
var
  graph: TGraph;
  i, j, x: Integer;
  stack: TIntStack;
  pmi: PParentsMapItem;
  //arr: TIntArray = nil;
begin

  graph := TGraph.Create(map.Count);
  try

    for i:=0 to map.Count-1 do begin
      pmi := map.Data[i];
      for j:=0 to Length(pmi^.parents)-1 do
        if (pmi^.n>=0) and (pmi^.parents[j].n>=0) then
          graph.AddEdge(pmi^.n, pmi^.parents[j].n);
    end;

    stack := graph.TopologicalSort;
    //SetLength(arr, stack.Count);

    {$IFDEF DEBUG}
    DebugLn;
    DebugLn('Topological Ordering:');
    {$ENDIF}
    for i:=stack.Count-1 downto 0 do begin
      x := (stack.Count-1) - i;
      {$IFDEF DEBUG}
      DbgOut('%3d ',[stack[i]]);
      if (x+1) mod 20 = 0 then DebugLn;
      {$ENDIF}
      //arr[x] := stack[i];
    end;
    {$IFDEF DEBUG}
    //DebugLn;
    //DebugLn('New Index ordering:');
    //for i:=0 to Length(arr)-1 do begin
    //  DbgOut('%3d ',[arr[i]]);
    //  if (i+1) mod 20 = 0 then DebugLn;
    //end;
    DebugLn;

    ReportTicks('TopoSort');
    {$ENDIF}

    //fDb.SetFilter(arr);

  finally
    graph.Free;
  end;
end;

constructor TGraphBuilderThread.Create(db: TDbIndex);
begin
  inherited create(true);
  fDb := db;
end;

procedure TGraphBuilderThread.Execute;
var
  parMap: TParentsMap;
  columns: TColumnArray;
begin
  {$IFDEF Debug}
  resetTicks(true);
  {$ENDIF}

  parMap := GetParentsMap(fDb, fStart, fEnd);

  FindRelativesMap(parMap);

  // parMap is not needed anymore
  ClearParentsMap(parMap);

  fMaxColumns := 0;
  if not fWithColumns then
    exit;

  columns := nil;
  AssignColumns(columns);

  FindHeadsAndTails(columns);

  MapLinesAndColumns(columns, fMaxColumns);

  fMaxColumns := Length(Columns);

  {$IFDEF Debug}
  {$ifdef ReportItemIndexArray}
  ReportItemIndexArray(fIndexArray);
  {$endif}
  {$ifdef ReportGraph}
  ReportGraph(fIndexArray, columns);
  {$endif}
  ReportTicks('Graph Building', true);
  {$ENDIF}

  Terminate;

end;

end.

