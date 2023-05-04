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

}
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

  PTagInfo = ^TTagInfo;
  TTagInfo = record
    data: string;
  end;

implementation

end.

