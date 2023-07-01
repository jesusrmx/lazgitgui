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

  Git status entries unit
}
unit unitentries;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLogger;

type

  TEntryKind = (
    ekUnknown,
    ekOrdinaryChanged,
    ekRenamedCopied,
    ekUnmerged,
    ekUntracked,
    ekIgnored
  );

  TEntryType = (
    etUnknown,

    etNotUpdatedA,
    etNotUpdatedM,
    etNotUpdatedD,

    etUpdatedInIndex,
    etUpdatedInIndexM,
    etUpdatedInIndexT,
    etUpdatedInIndexD,
    etTypeChangedInIndex,
    etTypeChangedInIndexM,
    etTypeChangedInIndexT,
    etTypeChangedInIndexD,
    etAddedToIndex,
    etAddedToIndexM,
    etAddedToIndexT,
    etAddedToIndexD,
    etDeletedFromIndex,
    etRenamedInIndex,
    etRenamedInIndexM,
    etRenamedInIndexT,
    etRenamedInIndexD,
    etCopiedInIndex,
    etCopiedInIndexM,
    etCopiedInIndexT,
    etCopiedInIndexD,

    etIndexAndWorktreeMatchesM,
    etIndexAndWorktreeMatchesT,
    etIndexAndWorktreeMatchesA,
    etIndexAndWorktreeMatchesR,
    etIndexAndWorktreeMatchesC,

    etWorktreeChangedSinceIndex,
    etWorktreeChangedSinceIndexM,
    etWorktreeChangedSinceIndexT,
    etWorktreeChangedSinceIndexA,
    etWorktreeChangedSinceIndexR,
    etWorktreeChangedSinceIndexC,
    etTypeChangedInWorktreeSinceIndex,
    etTypeChangedInWorktreeSinceIndexM,
    etTypeChangedInWorktreeSinceIndexT,
    etTypeChangedInWorktreeSinceIndexA,
    etTypeChangedInWorktreeSinceIndexR,
    etTypeChangedInWorktreeSinceIndexC,
    etDeletedInWorktree,
    etDeletedInWorktreeM,
    etDeletedInWorktreeT,
    etDeletedInWorktreeA,
    etDeletedInWorktreeR,
    etDeletedInWorktreeC,
    etRenamedInWorkTree,
    etCopiedInWorkTree,

    etUnmergedAddedByUs,
    etUnmergedBothAdded,
    etUnmergedBothDeleted,
    etUnmergedDeletedByUs,
    etUnmergedDeletedByThem,
    etUnmergedAddedByThem,
    etUnmergedBothModified,

    etUntracked,
    etIgnored
  );

  TSetOfEntryType = set of TEntryType;

  TSubmoduleType = set of (smtSubmodule, smtCommitChanged, smtTrackedChanges, smtUntrackedChanges);

  PFileEntry = ^TFileEntry;
  TFileEntry = record
    EntryKind: TEntryKind;
    EntryTypeStaged: TEntryType;
    EntryTypeUnStaged: TEntryType;
    SubModule: TSubmoduleType;
    x, y: char;
    m1, m2, m3, mW: Integer;    // m1=mH and m2=mI for ekOrdinaryChanged and ekRenamedCopied
    h1, h2, h3: string[41];     // h1=hH and h2=hI for ekOrdinaryChanged and ekRenamedCopied
    xScore: string[7];
    path: string;
    origPath: string;
  end;
  TPFileEntryArray = array of PFileEntry;

procedure ParsePorcelainV1(var head:pchar; tail: pchar; out entry: PFileEntry);
procedure ParseOrdinaryChanged(var head: pchar; tail: pchar; out entry: PFileEntry);
procedure ParseRenamedCopied(var head: pchar; tail: pchar; out entry: PFileEntry);
procedure ParseUnmerged(var head: pchar; tail: pchar; out entry: PFileEntry);
procedure ParseOther(var head: pchar; tail: pchar; out entry: PFileEntry);
procedure ParseBranches(var head: pchar; tail: pchar; out fBranch, fBranchOID, fUpstream: string; out fCommitsAhead, fCommitsBehind: Integer);
procedure ParseStatus(var head: pchar; tail: pchar; lstUnstaged, lstStaged: TStrings; fEntries:TfpList; out fMergingConflict:boolean; v1: boolean);

function EntryTypeToStr(X, Y: char): string;
procedure DumpEntry(Entry: PFileEntry);
procedure ClearEntries(fEntries: TfpList);

const
  ChangedInWorktreeSet = [ etWorktreeChangedSinceIndex .. etCopiedInWorkTree ];
  DeletedInWorktreeSet = [ etDeletedInWorktree .. etRenamedInWorkTree ];
  ChangedInIndexSet = [ etUpdatedInIndex .. etCopiedInIndexD ];
  UnmergedSet = [ etUnmergedAddedByUs .. etUnmergedBothModified ];

implementation

{
X          Y     Meaning
-------------------------------------------------
         [AMD]   not updated
M        [ MTD]  updated in index
T        [ MTD]  type changed in index
A        [ MTD]  added to index
D                deleted from index
R        [ MTD]  renamed in index
C        [ MTD]  copied in index
[MTARC]          index and work tree matches
[ MTARC]    M    work tree changed since index
[ MTARC]    T    type changed in work tree since index
[ MTARC]    D    deleted in work tree
	    R    renamed in work tree
	    C    copied in work tree
-------------------------------------------------
D           D    unmerged, both deleted
A           U    unmerged, added by us
U           D    unmerged, deleted by them
U           A    unmerged, added by them
D           U    unmerged, deleted by us
A           A    unmerged, both added
U           U    unmerged, both modified
-------------------------------------------------
?           ?    untracked
!           !    ignored
-------------------------------------------------
}

resourcestring
  rsLazGitGuiUnmodified = 'Unmodified';
  rsLazGitGuiModifiedNotStaged = 'Modified, not staged';
  rsLazGitGuiStagedForCommit = 'Staged for commit';
  rsLazGitGuiPortionsStagedForCommit = 'Portions staged for commit';
  rsLazGitGuiStagedForCommitMissing = 'Staged for commit, missing';
  rsLazGitGuiFileTypeChangedNotStaged = 'File type changed, not staged';
  rsLazGitGuiFileTypeChangedOldTypeStagedForCommit = 'File type changed, old type staged for commit';
  rsLazGitGuiFileTypeChangedStaged = 'File type changed, staged';
  rsLazGitGuiFileTypeChangeStagedModificationNotStaged = 'File type change staged, Modification not staged';
  rsLazGitGuiFileTypeChangeFileMissing = 'File type change staged, Missing';
  rsLazGitGuiUntrackedNotStaged = 'Untracked, not staged';
  rsLazGitGuiMissing = 'Missing';
  rsLazGitGuiStagedForRemoval = 'Staged for removal';
  rsLazGitGuiStagedForRemovalStillPresent = 'Staged for removal, still present';
  rsLazGitGuiStagedForRename = 'Staged for Rename';
  rsLazGitGuiRequiresMergeResolution = 'Requires merge resolution';
  rsLazGitGuiFileStateMissingDescription = 'File state missing description';
  rsLazGitGuiIgnored = 'Ignored';
  rsLazGitGuiUnmergedBothDeleted = 'Unmerged, Both deleted';
  rsLazGitGuiUnmergedAddedByUs = 'Unmerged, Added by Us';
  rsLazGitGuiUnmergedDeletedByThem = 'Unmerged, Deleted by Them';
  rsLazGitGuiUnmergedAddedByThem = 'Unmerged, Added by Them';
  rsLazGitGuiUnmergedDeletedByUs = 'Unmerged, Deleted by Us';
  rsLazGitGuiUnmergedBothAdded  = 'Unmerged, Both added';
  rsLazGitGuiUnmergedBothModified = 'Unmerged, Both Modified';

function XYToEntryType(x, y: char; staged:boolean; merging:boolean=false): TEntryType;
begin
  if x=' ' then x := '.';
  if y=' ' then y := '.';
  result := etUnknown;
  if staged and (not merging) then begin
    case x+y of
      '.A': result := etNotUpdatedA;
      '.M': result := etNotUpdatedM;
      '.D': result := etNotUpdatedD;
      '.R': result := etRenamedInWorkTree;
      '.C': result := etCopiedInWorkTree;
      'M.': result := etUpdatedInIndex;
      'MM': result := etUpdatedInIndexM;
      'MT': result := etUpdatedInIndexT;
      'MD': result := etUpdatedInIndexD;
      'T.': result := etTypeChangedInIndex;
      'TM': result := etTypeChangedInIndexM;
      'TT': result := etTypeChangedInIndexT;
      'TD': result := etTypeChangedInIndexD;
      'A.': result := etAddedToIndex;
      'AM': result := etAddedToIndexM;
      'AT': result := etAddedToIndexT;
      'AD': result := etAddedToIndexD;
      'D.': result := etDeletedFromIndex;
      'R.': result := etRenamedInIndex;
      'RM': result := etRenamedInIndexM;
      'RT': result := etRenamedInIndexT;
      'RD': result := etRenamedInIndexD;
      'C.': result := etCopiedInIndex;
      'CM': result := etCopiedInIndexM;
      'CT': result := etCopiedInIndexT;
      'CD': result := etCopiedInIndexD;
    end;

  end else begin

    if merging then
      case x+y of
        'DD': result := etUnmergedBothDeleted;
        'AU': result := etUnmergedAddedByUs;
        'UD': result := etUnmergedDeletedByThem;
        'UA': result := etUnmergedAddedByThem;
        'DU': result := etUnmergedDeletedByUs;
        'AA': result := etUnmergedBothAdded;
        'UU': result := etUnmergedBothModified;
      end
    else begin
      case x+y of
        'M.': result := etIndexAndWorktreeMatchesM;
        'T.': result := etIndexAndWorktreeMatchesT;
        'A.': result := etIndexAndWorktreeMatchesA;
        'R.': result := etIndexAndWorktreeMatchesR;
        'C.': result := etIndexAndWorktreeMatchesC;
        '.M': result := etWorktreeChangedSinceIndex;
        'MM': result := etWorktreeChangedSinceIndexM;
        'TM': result := etWorktreeChangedSinceIndexT;
        'AM': result := etWorktreeChangedSinceIndexA;
        'RM': result := etWorktreeChangedSinceIndexR;
        'CM': result := etWorktreeChangedSinceIndexC;
        '.T': result := etTypeChangedInWorktreeSinceIndex;
        'MT': result := etTypeChangedInWorktreeSinceIndexM;
        'TT': result := etTypeChangedInWorktreeSinceIndexT;
        'AT': result := etTypeChangedInWorktreeSinceIndexA;
        'RT': result := etTypeChangedInWorktreeSinceIndexR;
        'CT': result := etTypeChangedInWorktreeSinceIndexC;
        '.D': result := etDeletedInWorktree;
        'MD': result := etDeletedInWorktreeM;
        'TD': result := etDeletedInWorktreeT;
        'AD': result := etDeletedInWorktreeA;
        'RD': result := etDeletedInWorktreeR;
        'CD': result := etDeletedInWorktreeC;
        '??':  result := etUntracked;
        '!!':  result := etIgnored;
      end;
    end;
  end;
end;

function EntryTypeToStr(X, Y: char): string;
begin
  // ref: https://github.com/git/git/blob/master/git-gui/git-gui.sh (c. 2098)
  case x+y of
    '..': result := rsLazGitGuiUnmodified;
    '.M': result := rsLazGitGuiModifiedNotStaged;
    'M.': result := rsLazGitGuiStagedForCommit;
    'MM': result := rsLazGitGuiPortionsStagedForCommit;
    'MD': result := rsLazGitGuiStagedForCommitMissing;
    '.T': result := rsLazGitGuiFileTypeChangedNotStaged;
    'MT': result := rsLazGitGuiFileTypeChangedOldTypeStagedForCommit;
    'AT': result := rsLazGitGuiFileTypeChangedOldTypeStagedForCommit;
    'T.': result := rsLazGitGuiFileTypeChangedStaged;
    'TM': result := rsLazGitGuiFileTypeChangeStagedModificationNotStaged;
    'TD': result := rsLazGitGuiFileTypeChangeFileMissing;
    '.O': result := rsLazGitGuiUntrackedNotStaged; // 'O'='?' ???
    'A.': result := rsLazGitGuiStagedForCommit;
    'AM': result := rsLazGitGuiPortionsStagedForCommit;
    'AD': result := rsLazGitGuiStagedForCommitMissing;
    '.D': result := rsLazGitGuiMissing;
    'D.': result := rsLazGitGuiStagedForRemoval;
    'DO': result := rsLazGitGuiStagedForRemovalStillPresent; // 'O'='?' ???
    'R.': result := rsLazGitGuiStagedForRename;

    'DD': result := rsLazGitGuiUnmergedBothDeleted;
    'AU': result := rsLazGitGuiUnmergedAddedByUs;
    'UD': result := rsLazGitGuiUnmergedDeletedByThem;
    'UA': result := rsLazGitGuiUnmergedAddedByThem;
    'DU': result := rsLazGitGuiUnmergedDeletedByUs;
    'AA': result := rsLazGitGuiUnmergedBothAdded;
    'UU': result := rsLazGitGuiUnmergedBothModified;
    '.U': result := rsLazGitGuiRequiresMergeResolution;
    'U.': result := rsLazGitGuiRequiresMergeResolution;
    'UM': result := rsLazGitGuiRequiresMergeResolution;
    'UT': result := rsLazGitGuiRequiresMergeResolution;

    '?.': result := rsLazGitGuiUntrackedNotStaged;
    '!.': result := rsLazGitGuiIgnored;
    else  result := rsLazGitGuiFileStateMissingDescription;
  end;
end;

procedure DumpEntry(Entry: PFileEntry);
var
  s: string;
begin
  with Entry^ do begin
    WriteStr(s, EntryKind);
    DbgOut('Entry: kind: %s', [s]);
    WriteStr(s, EntryTypeStaged);
    DbgOut(' TypeStaged: %s', [s]);
    WriteStr(s, EntryTypeUnStaged);
    DbgOut(' TypeUnStaged: %s', [s]);
    DbgOut(' SubModule: [');
    if smtSubmodule in SubModule then DbgOut('smtSubmodule ');
    if smtCommitChanged in SubModule then DbgOut('smtCommitChanged ');
    if smtUnTrackedChanges in SubModule then dbgOut('smtUntrackedChanged ');
    if smtTrackedChanges in SubModule then dbgOut('smtTrackedChanged ');
    DbgOut('] XY=%s%s',[X,Y]);
    DbgOut(' FileModes HI3W: %s %s %s %s ', [OctStr(m1, 5), OctStr(m2, 5), OctStr(m3, 5), OctStr(mW, 5)]);
    DbgOut(' ObjNames: HI3: %s %s %s ', [h1, h2, h3]);
    DbgOut(' XScore: %s',[xScore]);
    DbgOut(' Path: %s',[path]);
    DebugLn(' OrigPath: %s', [OrigPath]);
  end;
end;

procedure ClearEntries(fEntries: TfpList);
var
  i: Integer;
  entry: PFileEntry;
begin
  if fEntries<>nil then begin
    for i:=0 to fEntries.Count-1 do begin
      entry := PFileEntry(fEntries[i]);
      if entry<>nil then
        Dispose(entry)
    end;
    fEntries.Clear;
  end;
end;

function NextField(var head:pchar): string;
var
  p: pchar;
begin
  p := strpos(head, ' ');
  if p<>nil then begin
    SetString(result, head, p-head);
    head := p + 1;
  end else
    result := '';
end;

procedure ParsePorcelainV1(var head: pchar; tail: pchar; out entry: PFileEntry);
var
  q: pchar;
begin
  // <XY> <path> [ <origpath ]
  new(Entry);
  entry^.EntryTypeStaged := XYToEntryType(head^, (head+1)^, true);
  entry^.EntryTypeUnStaged := XYToEntryType(head^, (head+1)^, false);
  entry^.x := head^;
  entry^.y := (head+1)^;

  if (entry^.EntryTypeStaged in UnmergedSet) or (entry^.EntryTypeUnStaged in UnmergedSet) then
    entry^.EntryKind := ekUnmerged
  else if (entry^.x='R') or (entry^.y='R') then
    entry^.EntryKind := ekRenamedCopied // todo: Check porcelain v1 EntryKind ekRenamedCopied
  else if (entry^.x='?') or (entry^.y='?') then
    entry^.EntryKind := ekUntracked
  else if (entry^.x='!') or (entry^.y='!') then
    entry^.EntryKind := ekIgnored
  else
    entry^.EntryKind := ekOrdinaryChanged;  // todo: Check porcelain v1 EntryKind ekOrdinaryChanged

  inc(head, 3);
  q := strpos(head, ' ');
  if (q<>nil) then begin
    entry^.EntryKind := ekRenamedCopied;
    SetString(entry^.path, head, q-head);
    inc(head, q-head);
    entry^.origPath := head;
  end else
    entry^.path := head
end;

procedure ParseOrdinaryChanged(var head: pchar; tail: pchar; out
  entry: PFileEntry);
var
  s: string;
  n: Integer;
begin
  New(entry);
  entry^.EntryKind := ekOrdinaryChanged;
  // 1 <XY> <sub> <mH> <mI> <mW> <hH> <hI> <path>
  Inc(head, 2);
  entry^.EntryTypeStaged := XYToEntryType(head^, (head+1)^, true);
  entry^.EntryTypeUnStaged := XYToEntryType(head^, (head+1)^, false);
  entry^.x := head^;
  entry^.y := (head+1)^;

  Inc(head, 3);
  entry^.SubModule := [];
  if head^='S' then begin
    include(entry^.Submodule, smtSubmodule);
    if (head+1)^ = 'C' then include(entry^.Submodule, smtCommitChanged);
    if (head+2)^ = 'M' then include(entry^.Submodule, smtTrackedChanges);
    if (head+3)^ = 'U' then include(entry^.Submodule, smtUntrackedChanges);
  end;

  inc(head, 5);
  s := NextField(head);
  entry^.m1 := StrToInt('&' + s);
  s := NextField(head);
  entry^.m2 := StrToInt('&' + s);
  s := NextField(head);
  entry^.mW := StrToInt('&' + s);

  entry^.h1 := NextField(head);
  entry^.h2 := NextField(head);

  n := strlen(head);
  entry^.path := head;
  inc(head, n+1);
  entry^.origPath := '';
end;

procedure ParseRenamedCopied(var head: pchar; tail: pchar; out entry: PFileEntry
  );
var
  s: string;
  n: Integer;
begin
  New(entry);
  entry^.EntryKind := ekRenamedCopied;

  // 2 <XY> <sub> <mH> <mI> <mW> <hH> <hI> <X><score> <path><sep><origPath>
  Inc(head, 2);
  entry^.EntryTypeStaged := XYToEntryType(head^, (head+1)^, true);
  entry^.EntryTypeUnStaged := XYToEntryType(head^, (head+1)^, false);
  entry^.x := head^;
  entry^.y := (head+1)^;

  Inc(head, 3);
  entry^.SubModule := [];
  if head^='S' then begin
    include(entry^.Submodule, smtSubmodule);
    if (head+1)^ = 'C' then include(entry^.Submodule, smtCommitChanged);
    if (head+2)^ = 'M' then include(entry^.Submodule, smtTrackedChanges);
    if (head+3)^ = 'U' then include(entry^.Submodule, smtUntrackedChanges);
  end;

  inc(head, 5);
  s := NextField(head);
  entry^.m1 := StrToInt('&' + s);
  s := NextField(head);
  entry^.m2 := StrToInt('&' + s);
  s := NextField(head);
  entry^.mW := StrToInt('&' + s);

  entry^.h1 := NextField(head);
  entry^.h2 := NextField(head);
  entry^.xScore := NextField(head);

  entry^.path := head;
  n := strlen(head);
  inc(head, n+1);
  entry^.origPath := head;
  n := strlen(head);
  inc(head, n+1);
end;

procedure ParseUnmerged(var head: pchar; tail: pchar; out entry: PFileEntry);
var
  s: string;
begin
  New(entry);
  entry^.EntryKind := ekUnmerged;

  // u <XY> <sub> <m1> <m2> <m3> <mW> <h1> <h2> <h3> <path>
  Inc(head, 2);
  entry^.EntryTypeStaged := XYToEntryType(head^, (head+1)^, true, true);
  entry^.EntryTypeUnStaged := XYToEntryType(head^, (head+1)^, false, true);
  entry^.x := head^;
  entry^.y := (head+1)^;

  Inc(head, 3);
  entry^.SubModule := [];
  if head^='S' then begin
    include(entry^.Submodule, smtSubmodule);
    if (head+1)^ = 'C' then include(entry^.Submodule, smtCommitChanged);
    if (head+2)^ = 'M' then include(entry^.Submodule, smtTrackedChanges);
    if (head+3)^ = 'U' then include(entry^.Submodule, smtUntrackedChanges);
  end;

  inc(head, 5);
  s := NextField(head);
  entry^.m1 := StrToInt('&' + s);
  s := NextField(head);
  entry^.m2 := StrToInt('&' + s);
  s := NextField(head);
  entry^.m3 := StrToInt('&' + s);
  s := NextField(head);
  entry^.mW := StrToInt('&' + s);

  entry^.h1 := NextField(head);
  entry^.h2 := NextField(head);
  entry^.h2 := NextField(head);

  entry^.path := head;

end;

procedure ParseOther(var head: pchar; tail: pchar; out entry: PFileEntry);
begin
  New(entry);
  entry^.x := head^;
  entry^.y := '.';

  case head^ of
    '?':
      begin
        entry^.EntryKind := ekUntracked;
        entry^.EntryTypeUnStaged := etUntracked;
        entry^.EntryTypeStaged := etUnknown;
      end;
    '!':
      begin
        entry^.EntryKind := ekIgnored;
        entry^.EntryTypeUnStaged := etIgnored;
        entry^.EntryTypeStaged := etUnknown;
      end
    else
      entry^.EntryKind := ekUnknown;
  end;

  inc(head, 2);
  entry^.path := head;
end;

function GetNextString(var head: pchar; var s: string; nextdelim: pchar): boolean;
var
  q: pchar;
  n: Integer;
begin
  q := strpos(head, nextdelim);
  result := q<>nil;
  if result then begin
    SetString(s, head, q-head);
    inc(head, q-head + strlen(nextdelim));
    result := true;
  end else begin
    n := strlen(head);
    SetString(s, head, n);
    inc(head, n);
  end;

end;

procedure ParseBranches(var head: pchar; tail: pchar; out fBranch, fBranchOID,
  fUpstream: string; out fCommitsAhead, fCommitsBehind: Integer);
var
  ab: string;
  i, n: Integer;
  q: pchar;
begin
  fBranch := '';
  fBranchOID := '';
  fUpstream := '';
  fCommitsAhead := 0;
  fCommitsBehind := 0;
  ab := '';

  // scan header lines
  while (head<tail) and (head^='#') do begin

    n := strlen(head);

    if (fBranchOID='') and (strlcomp(head, '# branch.oid', 12)=0) then begin
      SetString(fBranchOID, head + 13, n - 13);
    end else
    if (fBranch='') and (strlcomp(head, '# branch.head', 13)=0) then begin
      SetString(fBranch, head + 14, n - 14);
    end else
    if (fUpstream='') and (strlcomp(head, '# branch.upstream', 17)=0) then begin
      SetString(fUpstream, head + 18, n - 18);
    end else
    if (ab='') and (strlcomp(head, '# branch.ab', 11)=0) then begin
      SetString(ab, head + 12, n - 12);
      i := pos(' ', ab);
      fCommitsAhead := StrToIntDef(copy(ab, 1, i-1), 0);
      fCommitsBehind := StrToIntDef(copy(ab, i+1, Length(ab)), 0);
    end else
    if (fBranch='') and (strlcomp(head, '## ', 3)=0) then begin
      q := head;
      inc(q, 3);
      if GetNextString(q, fBranch, '...') then
      if GetNextString(q, fUpstream, ' [') then
      if GetNextString(q, ab, ']') then begin
        // ahead x behind y
        // ahead x
        i := pos('behind ', ab);
        if i>0 then begin
          fCommitsBehind := -StrToIntDef(copy(ab, i+7, Length(ab)), 0);
          delete(ab, i, length(ab));
        end;
        i := pos('ahead ', ab);
        if i>0 then
          fCommitsAhead := StrToIntDef(copy(ab, i+6, length(ab)), 0);
        // gone
      end;
    end;

    inc(head, n + 1);
  end;
end;

procedure ParseStatus(var head: pchar; tail: pchar; lstUnstaged,
  lstStaged: TStrings; fEntries: TfpList; out fMergingConflict: boolean;
  v1: boolean);
var
  n: Integer;
  entry: PFileEntry;
  start: pchar;
  needResetHead: boolean;
begin
  // clear lists
  ClearEntries(fEntries);
  lstUnstaged.Clear;
  lstStaged.Clear;
  fMergingConflict := false;

  // scan header lines
  while (head<tail) do begin

    start := head;
    n := strlen(head);
    //DebugLn(start);
    needResetHead := true;

    if v1 then begin

      ParsePorcelainV1(head, tail, entry);

    end else
      case head^ of
        '1': ParseOrdinaryChanged(head, tail, entry);
        '2':
          begin
            ParseRenamedCopied(head, tail, entry);
            needResetHead := false;
          end;
        'u':
          begin
            fMergingConflict := true;
            ParseUnmerged(head, tail, entry);
          end;
        '?',
        '!': ParseOther(head, tail, entry);
        else entry := nil;
      end;

    if entry<>nil then begin
      fEntries.Add(entry);

      // staged list
      case entry^.EntryTypeStaged of
        etUpdatedInIndex..etDeletedFromIndex:
          lstStaged.AddObject(entry^.path, TObject(entry));
        etRenamedInIndex..etCopiedInIndexD:
          lstStaged.AddObject(entry^.origPath + ' -> ' + entry^.path, TObject(entry));
      end;

      // unstaged list
      case entry^.EntryTypeUnStaged of
        etUnknown:;
        etUpdatedInIndex..etCopiedInIndexD:;
        etIndexAndWorktreeMatchesM..etIndexAndWorktreeMatchesC:;
        else
          lstUnstaged.AddObject(entry^.path, TObject(entry));
      end;

    end;

    if needResetHead then
      head := start + n + 1;
  end;
end;

end.

