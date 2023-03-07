unit unitentries;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

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

procedure ParseOrdinaryChanged(var head: pchar; tail: pchar; out entry: PFileEntry);
procedure ParseRenamedCopied(var head: pchar; tail: pchar; out entry: PFileEntry);
procedure ParseUnmerged(var head: pchar; tail: pchar; out entry: PFileEntry);
procedure ParseOther(var head: pchar; tail: pchar; out entry: PFileEntry);

function EntryTypeToStr(X, Y: char): string;


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

function XYToEntryType(x, y: char; staged:boolean): TEntryType;
begin
  result := etUnknown;
  if staged then begin
    case x of
      '.':
        case y of
          'A': result := etNotUpdatedA;
          'M': result := etNotUpdatedM;
          'D': result := etNotUpdatedD;
          'R': result := etRenamedInWorkTree;
          'C': result := etCopiedInWorkTree;
        end;
      'M':
        case y of
          '.': result := etUpdatedInIndex;
          'M': result := etUpdatedInIndexM;
          'T': result := etUpdatedInIndexT;
          'D': result := etUpdatedInIndexD;
        end;
      'T':
        case y of
          '.': result := etTypeChangedInIndex;
          'M': result := etTypeChangedInIndexM;
          'T': result := etTypeChangedInIndexT;
          'D': result := etTypeChangedInIndexD;
        end;
      'A':
        case y of
          '.': result := etAddedToIndex;
          'M': result := etAddedToIndexM;
          'T': result := etAddedToIndexT;
          'D': result := etAddedToIndexD;
          'U': result := etUnmergedAddedByUs;
          'A': result := etUnmergedBothAdded;
        end;
      'D':
        case y of
          '.': result := etDeletedFromIndex;
          'D': result := etUnmergedBothDeleted;
          'U': result := etUnmergedDeletedByUs;

        end;
      'R':
        case y of
          '.': result := etRenamedInIndex;
          'M': result := etRenamedInIndexM;
          'T': result := etRenamedInIndexT;
          'D': result := etRenamedInIndexD;
        end;
      'C':
        case y of
          '.': result := etCopiedInIndex;
          'M': result := etCopiedInIndexM;
          'T': result := etCopiedInIndexT;
          'D': result := etCopiedInIndexD;
        end;
      'U':
        case y of
          'D': result := etUnmergedDeletedByThem;
          'A': result := etUnmergedAddedByThem;
          'U': result := etUnmergedBothModified;
        end;
      '?':  result := etUntracked;
      '!':  result := etIgnored;
    end;
  end else begin
    case y of
      '.':
        case x of
          'M': result := etIndexAndWorktreeMatchesM;
          'T': result := etIndexAndWorktreeMatchesT;
          'A': result := etIndexAndWorktreeMatchesA;
          'R': result := etIndexAndWorktreeMatchesR;
          'C': result := etIndexAndWorktreeMatchesC;
        end;
      'M':
        case x of
          '.': result := etWorktreeChangedSinceIndex;
          'M': result := etWorktreeChangedSinceIndexM;
          'T': result := etWorktreeChangedSinceIndexT;
          'A': result := etWorktreeChangedSinceIndexA;
          'R': result := etWorktreeChangedSinceIndexR;
          'C': result := etWorktreeChangedSinceIndexC;
        end;
      'T':
        case x of
          '.': result := etTypeChangedInWorktreeSinceIndex;
          'M': result := etTypeChangedInWorktreeSinceIndexM;
          'T': result := etTypeChangedInWorktreeSinceIndexT;
          'A': result := etTypeChangedInWorktreeSinceIndexA;
          'R': result := etTypeChangedInWorktreeSinceIndexR;
          'C': result := etTypeChangedInWorktreeSinceIndexC;
        end;
      'D':
        case x of
          '.': result := etDeletedInWorktree;
          'M': result := etDeletedInWorktreeM;
          'T': result := etDeletedInWorktreeT;
          'A': result := etDeletedInWorktreeA;
          'R': result := etDeletedInWorktreeR;
          'C': result := etDeletedInWorktreeC;
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
    '.U': result := rsLazGitGuiRequiresMergeResolution;
    'U.': result := rsLazGitGuiRequiresMergeResolution;
    'UU': result := rsLazGitGuiRequiresMergeResolution;
    'UM': result := rsLazGitGuiRequiresMergeResolution;
    'UD': result := rsLazGitGuiRequiresMergeResolution;
    'UT': result := rsLazGitGuiRequiresMergeResolution;
    '?.': result := rsLazGitGuiUntrackedNotStaged;
    '!.': result := rsLazGitGuiIgnored;
    else  result := rsLazGitGuiFileStateMissingDescription;
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

  entry^.xScore := NextField(head);

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
end;

procedure ParseUnmerged(var head: pchar; tail: pchar; out entry: PFileEntry);
var
  s: string;
begin
  New(entry);
  entry^.EntryKind := ekUnmerged;

  // u <XY> <sub> <m1> <m2> <m3> <mW> <h1> <h2> <h3> <path>
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

end.

