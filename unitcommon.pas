unit unitcommon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  SECTION_DEFAULT   = 'options';
  SECTION_GEOMETRY  = 'geometry';
  SECTION_FONTS     = 'fonts';

const
  CMDSEP = '&sep;';

  SEARCHIN_COMMIT     = 1;
  SEARCHIN_AUTHOR     = 2;
  SEARCHIN_SUBJECT    = 3;
  SEARCHIN_DATE       = 4;

  SECTION_MRECOMMANDS = 'MRECommands';

resourcestring
  rsNewBranch = 'New Branch';
  rsNewTag = 'Create a tag at this point';
  rsReload = 'Reload';
  rsPushingYourCommits = 'Pushing your commits';
  rsThereAreCommitsBehind = 'There are commits behind, are you sure you want to push?';
  rsRestoringWorkFiles = 'Restoring working files';
  rsRestoringWorkFilesWarning =
    'You are about to discard changes in %s,'^M^M+
    'data will be lost and this action cannot be undone'^M^M+
    'Are you sure to continue?';
  rsDeletingWorkFiles = 'Deleting untracked files';
  rsDeletingWorkFilesWarning =
    'You are about to delete %s,'^M^M+
    'This action cannot be undone'^M^M+
    'Are you sure to continue?';
  rsSIsAlreadyInIgnoredList = '''%s'' is already in ignored list';
  rsTheTypeSIsAlreadyInTheIgnoredList = 'The type ''*%s'' is already in the ignored list';
  rsDFiles = '%d files';
  rsDiscardChanges = 'Discard changes';
  rsDeleteFiles = 'Delete Files';
  rsCancel = 'Cancel';
  rsViewUntrackedFiles = 'View Untracked Files';
  rsViewIgnoredFiles = 'View Ignored Files';
  rsViewTrackedFiles = 'View Tracked Files';
  rsStageS = 'Stage ''%s''';
  rsUnstageS = 'Unstage ''%s''';
  rsStageChanged = 'Stage Changed';
  rsStageAll = 'Stage All';
  rsUnstageAll = 'Unstage All';
  rsAddSToIgnoreList = 'Add ''%s'' to ignore list';
  rsAddSFilesToIgnoreList = 'Add ''*%s'' Files to ignore list';
  rsRestoreS = 'Restore ''%s''';
  rsRestoreAllChanged = 'Restore All Changed';
  rsNotYetImplementedForUnstagedS = 'Not yet implemented for Unstaged: %s';
  rsNotYetImplementedForStagedS = 'Not yet implemented for Staged: %s';
  rsSDoesNotExists = '%s does not exists';
  rsTheFileSIsBinaryDBytes = 'The file ''%s'' is binary, %d bytes';
  rsThisFeatureWillBeImplementedASAP = 'This feature will be implemented ASAP';
  rsCouldnTGetToplevelDirectoryOfS = 'Couldn''t get toplevel directory of %s';
  rsYouHaveToStageSomething = 'You have to stage something in order to commit';
  rsYourCommitMessageIsEmpt = 'Your commit message is empty!';
  rsThisRepositoryHasNoRemotes = 'This repository has no remotes defined'^M+
                                 'I''m not yet prepared to handle this';
  rsSHasNoTrackingAndThere = '%s has no tracking and there are '^M+
                             '%d remotes (%s)'^M+
                             'I''m not yet prepared to handle this';
  rsPushingBranchWithout = 'Pushing branch without tracking information';
  rsDoYouWantToPushSToSA = 'Do you want to push "%s" to "%s"'^M+
                           'and setup tracking information? I will do:'^M^M+
                           'git push --set-upstream %1:s %0:s';
  rsYesDoIt = 'yes, do it';
  rsPushingToRemoteS = 'Pushing to remote: %s';
  rsFetchingFromRemote = 'Fetching from remote: ';
  rsPullingFromRemote = 'pulling from remote: ';
  rsExecutingACustomCommand = 'Executing a custom command';
  rsYouAreAboutToExecute = 'You are about to execute %s: '^M^M+
                           'command: %s'^M^M+
                           'Do you want to proceed?';
  rsMERGING = 'MERGING';
  rsMERGINGCONFLICT = 'MERGING CONFLICT';
  rsDCommitsAhead = '%d commits ahead';
  rsDCommitsBehind = '%d commits behind';
  rsOf = 'of';
  rsNoTagAvailable = 'No Tag available';
  rsAtTag = 'At tag';
  rsDCommitsSince = '%d commits since';
  rsResetABranch = 'Reset a branch';
  rsMergingBranches = 'Merging branches';
  rsDeletingRemoteBranch = 'Deleting remote branch';
  rsCreateABranchAtThisCommit = 'Create a branch at this commit';
  rsMergeSToS = 'Merge %s to %s';
  rsSwitchToS = 'Switch to %s';
  rsDeleteBranchS = 'Delete branch %s';
  rsDeleteRemoteBranchS = 'Delete remote branch %s';
  rsCreateATagAtThisCommit = 'Create a tag at this commit';
  rsSwitchToTagS = 'Switch to tag %s';
  rsDeleteTagS = 'Delete tag %s';
  rsResetSToThisCommit = 'Reset %s to this commit';
  rsUnableToLocateDbIndex = 'Unable to locate db index';
  rsBuildingGraph = 'Building graph..';
  rsBranchNameIsEmpty = 'Branch name is empty';
  rsInvalidCharacterInBranchName = 'Invalid character in branch name';
  rsBranchNameAlreadyExists = 'Branch name already exists';
  rsWillDoGitCheckoutBSS = 'will do: git checkout -b %s %s';
  rsWillDoGitCheckoutSDetached = 'will do: git checkout %s'^M'Repo will be left in detached HEAD state';
  rsCommandDS = 'command %d: %s';
  rsDescriptionIsEmpty = 'Description is empty';
  rsInvalidDescription = 'Invalid description';
  rsCommandMustStartWithGit = 'Command must start with ''git ''';
  rsIncompleteCommand = 'Incomplete command';
  rsHistoryOfS = 'History of %s';
  rsLocalBranches = 'Local Branches';
  rsTrackingBranches = 'Tracking Branches';
  rsTags = 'Tags';
  rsCommit = 'Commit';
  rsBranchSAlredyExists = 'Branch ''%s'' alredy exists';
  rsNoReferingBranchSelected = 'No refering branch selected';
  rsNotRefersToATrackingBranch = 'Not refers to a tracking branch';
  rsNoReferingTagSelected = 'No refering tag selected';
  rsNoCommitSet = 'No commit set';
  rsWorktreeS = 'worktree: %s';
  rsTagS2 = 'Tag: %s';
  rsCommitS = 'Commit: %s';
  rsErrorWhileGettingListOfBranches = 'Error while getting list of branches';
  rsInvalid = 'Invalid';
  rsTagNameIsEmpty = 'Tag name is empty';
  rsInvalidCharacterInTagName = 'Invalid character in tag name';
  rsResetBranchS = 'Reset branch %s';
  rsYouAreResettingBranch = 'You are resetting branch %s to revision %s:'^M+
                            '%s'^M+
                            'Do you want to proceed?';
  rsResetSoft = 'Neither the working copy nor the index are altered.'^M^M^M+
    'git doc:'^M+
    'Does not touch the index file or the working tree at all (but '+
    'resets the head to <commit>, just like all modes do). This leaves all '+
    'your changed files "Changes to be committed", as git status would put it.';
  rsResetMixed = 'Reset index, don''t touch working copy.'^M^M^M+
    'git doc: '^M+
    'Resets the index but not the working tree (i.e., the ' +
    'changed files are preserved but not marked for commit)' +
    ' and reports what has not been updated. This is the ' +
    'default action.';
  rsResetHard = 'Reset index and working copy.'^M+
    'LOCAL CHANGES WILL BE LOST!'^M^M^M+
    'git doc: '^M+
    'Resets the index and working tree. Any changes to tracked ' +
    'files in the working tree since <commit> are discarded. Any' +
    ' untracked files or directories in the way of writing any ' +
    'tracked files are simply deleted.';
  rsStartingCommandPleaseWait = 'Starting command, please wait ....';
  rsWorking = 'Working ....';
  rsSucceed = 'Succeed';
  rsFailed = 'Failed';
  rsTheCommandIsEmpty = 'The command is empty';
  rsABranchIsNotSelected = 'A branch is not selected';
  rsTheTargetRemoteRepositoryIsNotSelected = 'The target remote repository is '
    +'not selected';
  rsInvalidTargetUrl = 'Invalid target url';
  rsNewRemoteName = '<New Remote Name>';
  rsInvalidRemoteName = 'remote %d has an invalid name';
  rsInvalidFetchURL = '%s has an invalid Fetch URL';
  rsInvalidPushURL = '%s has an invalid Push URL';
  rsTheBranchesListIsEmpty = 'The branches list is empty';
  rsSIsAnInvalidBranchName = '%s is an invalid branch name';
  rsCopy = 'Copy';
  rsSelectAll = 'Select All';
  rsDeleteS = 'Delete ''%s''';
  rsSomeFilesCouldNotBeDeleted = 'Some files could not be deleted';
  rsCheckOutACommit = 'Check out a commit';
  rsSwitchToThisCommit = 'Switch to this commit';
  rsCreateAPatchFileFromS = 'Create a patch file from %s';
  rsCopyPatchFromSToTheClipboard = 'Copy patch from %s to the clipboard';
  rsApplyingSerialPatchesIsNotYetImplemented = 'Applying serial patches is not'
    +' yet implemented';
  rsInvalidAmountOfPatchesTooApply = 'Invalid amount of patches too apply';
  rsPatchingTheWorkArea = 'Patching the work area';
  rsYouAreTryingToApply = 'You are trying to apply the patch file: %s to '+
    'your working area.'^M^M+
    'This operation will not make a commit.'^M^M+
    'Do you want to apply the patch?';
  rsApplyPatch = 'Apply patch';
  rsApplyingAPatch = 'Applying a patch';
  rsTheRepositoryURLIsEmpty = 'The repository URL is empty';
  rsInvalidCloneURL = 'Invalid clone URL';
  rsInvalidDirectory = 'Invalid directory';
  rsInvalidRepositoryName = 'Invalid repository name';
  rsInto = 'into: ';
  rsInvalidFileUrl = 'Invalid file url';
  rsTheFileUrlPointsToAnInvalidDirectory = 'The file url points to an invalid '
    +'directory';

implementation

end.

