unit unittoposort;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazLoggerBase;

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


implementation

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

end.

