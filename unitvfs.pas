unit unitvfs;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, LazLogger, LazFileUtils;

type
  TvfsString = RawByteString;

  { TVirtualFileSystem }

  TVirtualFileSystem = class
  public
    type
      PNode = ^TNode;
      TNode = record
        name: TvfsString;
        Data: Pointer;
        prev, next, parent, childs: PNode;
      end;
      TNewNodeEvent = procedure(Sender: TObject; aName:TvfsString; isDir:boolean; var data:Pointer) of object;
      TNewNodeEventNested = procedure(Sender: TObject; aName:TvfsString; isDir:boolean; var data:Pointer) is nested;
      TDisposeNodeEvent = procedure(Sender: TObject; aName:TvfsString; Data: pointer) of object;
      TDisposeNodeEventNested = procedure(Sender: TObject; aName:TvfsString; Data: pointer) is nested;
  private
    fOnDisposeNode: TDisposeNodeEvent;
    fOnDisposeNodeNested: TDisposeNodeEventNested;
    fOnNewNode: TNewNodeEvent;
    fOnNewNodeNested: TNewNodeEventNested;
    fPlain: boolean;
    fRoot: PNode;
    function LastSibling(sibling: PNode): PNode;
    function FindNode(parent: PNode; aName: TvfsString): PNode;
    function FindTopLvlNode(aName: TvfsString): PNode;
    function AddNode(parent: PNode; aName: TvfsString; isDir:boolean): PNode;
    function GetPath(node: PNode): string;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure AddPath(aPath: string);
    procedure Dump;

    property root: PNode read fRoot;
    property Plain: boolean read fPlain write fPlain;
    property OnNewNode: TNewNodeEvent read fOnNewNode write fOnNewNode;
    property OnNewNodeNested: TNewNodeEventNested read fOnNewNodeNested write fOnNewNodeNested;
    property OnDisposeNode: TDisposeNodeEvent read fOnDisposeNode write fOnDisposeNode;
    property OnDisposeNodeNested: TDisposeNodeEventNested read fOnDisposeNodeNested write fOnDisposeNodeNested;
  end;

implementation

{ TVirtualFileSystem }

procedure TVirtualFileSystem.Clear;

  procedure DeleteNode(node: PNode);
  begin
    //DebugLn('disposing: %s Data=%s',[node^.name, BoolToStr(node^.Data<>nil,'Not nil', 'nil')]);
    if Node^.Data<>nil then begin
      if Assigned(fOnDisposeNodeNested) then
        fOnDisposeNodeNested(self, node^.name, node^.data)
      else
      if Assigned(fOnDisposeNode) then
        fOnDisposeNode(self, node^.name, node^.data);
    end;
    Dispose(Node);
  end;

  procedure DisposeNode(node: PNode);
  var
    child, next, parent: PNode;
  begin
    if node^.childs=nil then begin
      DeleteNode(node);
      exit;
    end;

    parent := node;
    child := node^.childs;
    while child<>nil do begin
      next := child^.next;
      DisposeNode(child);
      child := next;
    end;

    DeleteNode(parent);
  end;

var
  node, next: PNode;
begin
  node := fRoot;
  while node<>nil do begin
    next := node^.next;
    DisposeNode(node);
    node := next;
  end;
  fRoot := nil;
end;

procedure TVirtualFileSystem.AddPath(aPath: string);
var
  level: Integer;
  s: TvfsString;
  w, p, pini, c, t, x: pchar;
  ch: Char;
  Parent, Node: PNode;
  isDir: Boolean;
begin

  Parent := nil;
  if fPlain then begin
    AddNode(parent, aPath, false);
    exit;
  end;

  level:=0;
  pIni := @aPath[1];
  t := pIni;
  p := pIni;
  inc(t, Length(aPath));
  while p<t do begin
    w := p;
    // find next separator
    while (p<t) and (not (p^ in ['\','/'])) do inc(p);
    c := p;
    // skip separators
    while (p<t) and (p^ in ['\','/']) do inc(p);
    // c-w='word', p-w='word with separators'
    x := p;

    SetString(s, w, c-w);
    isDir := c^ in ['\','/'];

    //Len := p-pini;
    Node := AddNode(Parent, s, isDir);

    inc(level);
    Parent := Node;
  end;
end;

procedure TVirtualFileSystem.Dump;
var
  s: string;

  procedure PrintLeaf(node: PNode);
  var
    child: PNode;
  begin
    if node^.childs=nil then begin
      s := GetPath(Node);
      DebugLn(s);
      exit;
    end;
    child := node^.childs;
    while child<>nil do begin
      PrintLeaf(child);
      child := child^.next;
    end;
  end;
var
  node: PNode;
begin
  node := fRoot;
  while node<>nil do begin
    PrintLeaf(node);
    node := node^.Next;
  end;
end;

function TVirtualFileSystem.LastSibling(sibling: PNode): PNode;
begin
  result := nil;
  while sibling<>nil do begin
    result := sibling;
    sibling := sibling^.Next;
  end;
end;

function TVirtualFileSystem.FindNode(parent: PNode; aName: TvfsString): PNode;
begin
  Result:=Parent^.Childs;
  while (Result<>nil) and (CompareFilenames(Result^.Name, aName)<>0) do
    Result:=Result^.Next;
end;

function TVirtualFileSystem.FindTopLvlNode(aName: TvfsString): PNode;
begin
  result := fRoot;
  while result<>nil do begin
    if result^.Name=aName then
      exit;
    result := result^.Next;
  end;
end;

function TVirtualFileSystem.AddNode(parent: PNode; aName: TvfsString;
  isDir: boolean): PNode;
var
  prev: PNode;
  data: pointer;
begin
  if Parent=nil then
    result := FindTopLvlNode(aName)
  else
    result := FindNode(Parent, aName);

  if result=nil then
  begin

    if Parent=nil then
      prev := LastSibling(fRoot)
    else
      prev := LastSibling(Parent^.Childs);

    data := nil;
    if assigned(fOnNewNodeNested) then
      fOnNewNodeNested(self, aName, isDir, data);
    if Assigned(fOnNewNode) then
      fOnNewNode(self, aName, isDir, data);

    New(result);
    result^.parent := parent;
    result^.Name := aName;
    result^.Data := Data;
    result^.Prev := Prev;
    result^.Next := nil;
    result^.childs := nil;
    if (parent<>nil) and (parent^.childs=nil) then
      parent^.childs := result;
    if Prev<>nil then
      Prev^.Next := result;

    if fRoot=nil then
      fRoot := result;
  end;
end;

function TVirtualFileSystem.GetPath(node: PNode): string;
begin
  result := '';
  while node<>nil do begin
    if result<>'' then
      result := '/' + result;
     result := node^.name + result;
     node := node^.parent;
  end;
end;

destructor TVirtualFileSystem.Destroy;
begin
  Clear;
  inherited Destroy;
end;

end.

