unit unitifaces;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, unitgittypes, unitentries;

const
  SECTION_DEFAULT = 'options';

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
    function AddToIgnoreFile(aFile:string; justType:boolean; global:boolean): boolean;
    function Restore(entry: PFileEntry; staged: boolean): Integer; overload;
    function Restore(entryArray: TPFileEntryArray; staged: boolean): Integer; overload;
    function Any(cmd: string; out cmdout:RawByteString): Integer;
    function Add(entry: PFileEntry): Integer; overload;
    function Add(entryArray: TPFileEntryArray): Integer; overload;
    function Rm(entry: PFileEntry): Integer;
    function BranchList(list: TStrings; opts:array of string): Integer;
    function Describe(opts: string; out cmdOut:RawByteString): Integer;
    function Tag(tagName, tagCommit:string; annotated:boolean; tagMsg:string): Integer;
    function DeleteTag(tagName: string): Integer;
    function OpenDir(aDir: string): Integer;
    function Commit(msg, opts: string): Integer;
    function Diff(entry: PFileEntry; Unstaged:boolean; Lines:TStrings): Integer;
    function Show(obj: string; lines: TStrings): Integer;
    //
    function GetBranch: string;
    function GetBranchOID: string;
    function GetExe: string;
    function GetCommitsAhead: Integer;
    function GetCommitsBehind: Integer;
    function GetLastTag: string;
    function GetLastTagCommits: Integer;
    function GetLastTagOID: string;
    function GetMerging: boolean;
    function GetMergingConflict: boolean;
    function GetRefsMap: TRefsMap;
    function GetRefList: TStringList;
    function GetRemotesList: TStringList;
    function GetTopLevelDir: string;
    function GetUpstream: string;
    function GetVersion: string;
    function GetErrorLog: RawByteString;
    function GetLogError: RawByteString;
    function RefsFilter(commitOID: string; filter: TRefFilterProc): TRefInfoArray;
    procedure ResetLogError;

    property Branch: string read GetBranch;
    property BranchOID: string read GetBranchOID;
    property CommitsBehind: Integer read GetCommitsBehind;
    property CommitsAhead: Integer read GetCommitsAhead;
    property Exe: string read GetExe;
    property LastTag: string read GetLastTag;
    property LastTagCommits: Integer read GetLastTagCommits;
    property LastTagOID: string read GetLastTagOID;
    property Merging: boolean read GetMerging;
    property MergingConflict: boolean read GetMergingConflict;
    property RefList: TStringList read GetRefList;
    property RefsMap: TRefsMap read GetRefsMap;
    property TopLevelDir: string read GetTopLevelDir;
    property Upstream: string read GetUpstream;
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

