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

  utility interfaces
}
unit unitifaces;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, unitgittypes, unitentries;

const
  SECTION_DEFAULT   = 'options';
  SECTION_GEOMETRY  = 'geometry';

type

  IConfig = interface ['{8D6B4CF1-6777-4208-BAD7-395E0936F1D9}']
    procedure OpenConfig;
    procedure CloseConfig;
    function ReadString(aKey:string; default:string=''; section:string=SECTION_DEFAULT): string;
    function ReadBoolean(aKey:string; default:boolean=false; section:string=SECTION_DEFAULT): boolean;
    function ReadInteger(aKey:string; default:Integer=0; section:string=SECTION_DEFAULT): Integer;
    procedure WriteString(aKey:string; avalue:string; section:string=SECTION_DEFAULT);
    procedure WriteBoolean(aKey:string; avalue:boolean; section:string=SECTION_DEFAULT);
    procedure WriteInteger(aKey:string; avalue:Integer; section:string=SECTION_DEFAULT);
  end;

  IDbIndex = interface ['{F173FEE3-9521-42F4-9632-C6EF2B84750C}']
    procedure ThreadStart(aHead: boolean);
    procedure ThreadDone;
    procedure ThreadStore(buf: pchar; size: Integer);
  end;

  { IGit }

  IGit = interface ['{2BC4C993-13AA-44BD-9C8A-11038CD50904}']
    function Switch(branchName: string): Integer;
    function Restore(entry: PFileEntry; staged: boolean): Integer; overload;
    function Restore(entryArray: TPFileEntryArray; staged: boolean): Integer; overload;
    function Any(cmd: string; out cmdout:RawByteString): Integer;
    function Add(entry: PFileEntry): Integer; overload;
    function Add(entryArray: TPFileEntryArray): Integer; overload;
    function Rm(entry: PFileEntry): Integer;
    function Tag(tagName, tagCommit:string; annotated:boolean; tagMsg:string): Integer;
    function DeleteTag(tagName: string): Integer;
    function OpenDir(aDir: string): Integer;
    function Commit(msg, opts: string): Integer;
    function Diff(entry: PFileEntry; Unstaged:boolean; Lines:TStrings): Integer;
    function Show(obj: string; lines: TStrings): Integer;
    function Log(opts: string; Lines:TStrings): Integer;
    //
    function GetExe: string;
    function GetTopLevelDir: string;
    function GetGitCommonDir: string;
    function GetVersion: string;
    function GetErrorLog: RawByteString;
    function GetLogError: RawByteString;
    procedure ResetLogError;

    property Exe: string read GetExe;
    property TopLevelDir: string read GetTopLevelDir;
    property GitCommonDir: string read GetGitCommonDir;
    property Version: string read GetVersion;
    property ErrorLog: RawByteString read GetErrorLog;
    property LogError: RawByteString read GetLogError;
  end;

  IObserver = interface ['{6ADBB399-EA69-4FD7-AEF3-E969FB09AC37}']
    procedure ObservedChanged(Sender:TObject; what: Integer; data: PtrInt);
  end;

  { TMyInterfacedObject }

  TMyInterfacedObject = class(IUnknown)
  protected
    function QueryInterface(constref IID: TGUID; out Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

implementation

{ TMyInterfacedObject }

function TMyInterfacedObject._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  result := -1;
end;

function TMyInterfacedObject._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  result := -1;
end;

function TMyInterfacedObject.QueryInterface(constref IID: TGUID; out Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

end.

