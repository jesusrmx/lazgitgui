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

  Virtual File System unit.

  This unit defines the class TVirtualFileSystem which can be feed with succesive
  AddPath() instructions and it will construct a hierarchical tree of files and
  directories.
}
unit unitvfs;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, lazlogger, LazFileUtils;

type
  TvfsString = RawByteString;

  PvfsNode = ^TvfsNode;
  TvfsNode = record
    name: TvfsString;
    Data: Pointer;
    prev, next, parent, childs: PvfsNode;
  end;

  TNewNodeEvent = procedure(Sender: TObject; aName:TvfsString; isDir:boolean; var data:Pointer) of object;
  TNewNodeEventNested = procedure(Sender: TObject; aName:TvfsString; isDir:boolean; var data:Pointer) is nested;
  TDisposeNodeEvent = procedure(Sender: TObject; aName:TvfsString; Data: pointer) of object;
  TDisposeNodeEventNested = procedure(Sender: TObject; aName:TvfsString; Data: pointer) is nested;

  { TVirtualFileSystem }

  TVirtualFileSystem = class
  private
    fOnDisposeNode: TDisposeNodeEvent;
    fOnDisposeNodeNested: TDisposeNodeEventNested;
    fOnNewNode: TNewNodeEvent;
    fOnNewNodeNested: TNewNodeEventNested;
    fPlain: boolean;
    fRoot: PvfsNode;
    function LastSibling(sibling: PvfsNode): PvfsNode;
    function FindChildNode(parent: PvfsNode; aName: TvfsString): PvfsNode;
    function FindTopLvlNode(aName: TvfsString): PvfsNode;
    function AddNode(parent: PvfsNode; aName: TvfsString; isDir:boolean): PvfsNode;
    function GetPath(node: PvfsNode): string;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure AddPath(aPath: string; parent: PvfsNode = nil);
    function FindPath(aPath: string): PvfsNode;
    procedure Dump;
    function NodeToStr(node: PvfsNode): string;

    property root: PvfsNode read fRoot;
    property Plain: boolean read fPlain write fPlain;
    property OnNewNode: TNewNodeEvent read fOnNewNode write fOnNewNode;
    property OnNewNodeNested: TNewNodeEventNested read fOnNewNodeNested write fOnNewNodeNested;
    property OnDisposeNode: TDisposeNodeEvent read fOnDisposeNode write fOnDisposeNode;
    property OnDisposeNodeNested: TDisposeNodeEventNested read fOnDisposeNodeNested write fOnDisposeNodeNested;
  end;

implementation

var
  pathHead:pchar=nil;
  pathTail:pchar=nil;
  pathLevel:Integer=0;

function WalkPath(out part:TvfsString; out isDir:boolean; wantRoot:boolean=false): boolean;
var
  w,c,x: pchar;
begin
  result := (pathHead<>nil) and (pathHead<pathTail);
  if result then begin
    w := pathHead;
    // find next separator
    while (pathHead<pathTail) and (not (pathHead^ in ['\','/'])) do inc(pathHead);
    c := pathHead;
    // skip separators
    while (pathHead<pathTail) and (pathHead^ in ['\','/']) do inc(pathHead);
    // c-w='word', p-w='word with separators'
    x := pathHead;

    if (pathLevel=0) and wantRoot then
      SetString(part, w, x-w)
    else
      SetString(part, w, c-w);
    isDir := c^ in ['\','/'];

    inc(pathLevel);
  end;
end;

function BeginWalkPath(aPath: TvfsString; out part:TvfsString; out isDir:boolean; wantRoot:boolean=false): boolean;
begin
  pathLevel := 0;
  result := aPath<>'';
  if result then begin
    pathHead := @aPath[1];
    pathTail := pathHead + Length(aPath);
    result := WalkPath(part, isDir, wantRoot);
  end else begin
    pathHead := nil;
    pathTail := nil;
  end;
end;

{ TVirtualFileSystem }

procedure TVirtualFileSystem.Clear;

  procedure DeleteNode(node: PvfsNode);
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

  procedure DisposeNode(node: PvfsNode);
  var
    child, next, parent: PvfsNode;
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
  node, next: PvfsNode;
begin
  node := fRoot;
  while node<>nil do begin
    next := node^.next;
    DisposeNode(node);
    node := next;
  end;
  fRoot := nil;
end;

procedure TVirtualFileSystem.AddPath(aPath: string; parent: PvfsNode);
var
  level: Integer;
  s: TvfsString;
  w, p, pini, c, t, x: pchar;
  ch: Char;
  Node: PvfsNode;
  isDir: Boolean;
begin
  if fPlain then begin
    AddNode(parent, aPath, false);
    exit;
  end;

  if BeginWalkPath(aPath, s, isDir) then
    repeat
      Node := AddNode(Parent, s, isDir);
      Parent := Node;
    until not WalkPath(s, isDir);

  {

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
  }
end;

function TVirtualFileSystem.FindPath(aPath: string): PvfsNode;
var
  part: TvfsString;
  isDir: boolean;
begin
  result := nil;
  if BeginWalkPath(aPath, part, isDir) then
    repeat
      if result=nil then result := FindTopLvlNode(part)
      else               result := FindChildNode(result, part);
    until (result=nil) or not WalkPath(part, isDir);
end;

procedure TVirtualFileSystem.Dump;
var
  s: string;

  procedure PrintLeaf(node: PvfsNode);
  var
    child: PvfsNode;
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
  node: PvfsNode;
begin
  node := fRoot;
  while node<>nil do begin
    PrintLeaf(node);
    node := node^.Next;
  end;
end;

function TVirtualFileSystem.LastSibling(sibling: PvfsNode): PvfsNode;
begin
  result := nil;
  while sibling<>nil do begin
    result := sibling;
    sibling := sibling^.Next;
  end;
end;

function TVirtualFileSystem.FindChildNode(parent: PvfsNode; aName: TvfsString): PvfsNode;
begin
  Result:=Parent^.Childs;
  while (Result<>nil) and (CompareFilenames(Result^.Name, aName)<>0) do
    Result:=Result^.Next;
end;

function TVirtualFileSystem.FindTopLvlNode(aName: TvfsString): PvfsNode;
begin
  result := fRoot;
  while result<>nil do begin
    if result^.Name=aName then
      exit;
    result := result^.Next;
  end;
end;

function TVirtualFileSystem.AddNode(parent: PvfsNode; aName: TvfsString;
  isDir: boolean): PvfsNode;
var
  prev: PvfsNode;
  data: pointer;
begin
  if Parent=nil then
    result := FindTopLvlNode(aName)
  else
    result := FindChildNode(Parent, aName);

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

function TVirtualFileSystem.GetPath(node: PvfsNode): string;
begin
  result := '';
  while node<>nil do begin
    if result<>'' then
      result := '/' + result;
     result := node^.name + result;
     node := node^.parent;
  end;
end;

function TVirtualFileSystem.NodeToStr(node: PvfsNode): string;
begin
  if node=nil then
    result := 'nil'
  else
    with node^ do begin
      result := QuotedStr(name) + '(';
      if prev<>nil then
        result += format('prv=%s ', [prev^.name]);
      if next<>nil then
        result += format('nxt=%s ', [next^.name]);
      if parent<>nil then
        result += format('dad=%s ', [parent^.name]);
      if childs<>nil then
        result += format('kid=%s ', [childs^.name]);
      if data<>nil then
        result += 'DAT';
      result += ')';
    end;
end;

destructor TVirtualFileSystem.Destroy;
begin
  Clear;
  inherited Destroy;
end;

end.

