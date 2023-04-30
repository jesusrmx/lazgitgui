unit unitgittypes;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, fgl;

type
  TRefObjectType = (rotBlob, rotTree, rotCommit, rotTag);
  TRefObjectSubType = (rostLocal, rostTracking, rostTag, rostOther);

  PRefInfo = ^TRefInfo;
  TRefInfo = record
    refName: string;
    objType: TRefObjectType;
    objName: string;
    objNameInt: QWord;
    upstream: string;
    push: string;
    head: boolean;
    worktreepath: string;
    subject: string; // for branch list
    authorName: string;
    authorDate: TDateTime;
    committerDate: TDateTime;
    creatorDate: TDateTime;
    isTracking: boolean;
    subType: TRefObjectSubType;
    refered: PRefInfo;
  end;
  TRefInfoArray = array of PRefInfo;

  TRefsMap = specialize TFPGMap<string, TRefInfoArray>;

  TRefItem = record
    info: PRefInfo;
    mapIndex: Integer;
  end;

  TRefItemArray = array of TRefItem;

  TRefFilterProc = function(info: PRefInfo): boolean is nested;

  PNewTagInfo = ^TNewTagInfo;
  TNewTagInfo = record
    commit: string;
  end;

implementation

end.

