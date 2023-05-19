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

  Highlighter helper.
  Keeps a record of several highlighters and find and set an highlighter to
  a synedit depending on the supplied file type.
}
unit unithighlighterhelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynEdit, SynHighlighterDiff,
  SynHighlighterPas, SynHighlighterXML, SynHighlighterHTML,
  synhighlighterunixshellscript, SynHighlighterCpp, SynHighlighterJScript,
  SynHighlighterLFM, SynHighlighterJava, SynHighlighterCss, SynHighlighterPHP,
  SynHighlighterIni, SynHighlighterBat;

type

  { THighlighterHelper }

  THighlighterHelper = class
  private
    fList: TStringList;
    procedure register(hlclass: TSynCustomHighlighterClass; extraFilter:string='');
    function GetHighlighterForExt(ext: string): Integer;
  public
    constructor create;
    procedure SetHighlighter(edit:TSynEdit; fileType: string);
    destructor Destroy; override;
  end;

implementation

{ THighlighterHelper }

procedure THighlighterHelper.register(hlclass: TSynCustomHighlighterClass;
  extraFilter: string);
var
  hl: TSynCustomHighlighter;
  defFilter, s: String;
  i, f: Integer;
begin
  if fList.IndexOf(hlClass.ClassName)<0 then begin
    hl := hlClass.Create(nil);
    fList.AddObject(hlclass.ClassName, hl);
    if (extraFilter<>'') then begin

      f := pos('|', extraFilter);
      if f=0 then exit;

      defFilter := hl.DefaultFilter;
      if defFilter='' then begin
        hl.DefaultFilter := extraFilter;
        exit;
      end;

      delete(extraFilter, 1, f);
      extraFilter := ';' + extraFilter;
      i := pos(')', defFilter);
      if i>0 then begin
        Insert(extraFilter, defFilter, i);
        defFilter += extraFilter;
        hl.DefaultFilter := defFilter;
      end;
    end;
  end;
end;

function THighlighterHelper.GetHighlighterForExt(ext: string): Integer;
var
  hl: TSynCustomHighlighter;
  i: Integer;
begin
  result :=-1;
  for i:=0 to fList.Count-1 do begin
    hl := TSynCustomHighlighter(fList.Objects[i]);
    if pos(ext, hl.DefaultFilter)>0 then begin
      result := i;
      break;
    end;
  end;
end;

constructor THighlighterHelper.create;
begin
  inherited Create;
  fList := TStringList.Create;
  Register(TSynDiffSyn, '|*.rej');
  Register(TSynFreePascalSyn, '|*.lpr;*.pp');
  Register(TSynHTMLSyn);
  Register(TSynUNIXShellScriptSyn);
  Register(TSynCppSyn);
  Register(TSynJScriptSyn);
  Register(TSynLFMSyn);
  Register(TSynJavaSyn);
  Register(TSynCssSyn);
  Register(TSynPHPSyn);
  Register(TSynIniSyn);
  Register(TSynXMLSyn, '|*.lpi;*.svg;*.plist');
  Register(TSynBatSyn);
end;

procedure THighlighterHelper.SetHighlighter(edit: TSynEdit; fileType: string);
var
  ext: String;
  i: Integer;
begin
  ext := lowercase(ExtractFileExt(fileType));
  if (ext='.diff') or (ext='.patch') or (ext='.dif') then i := 0
  else                                                    i := GetHighlighterForExt(ext);
  if i<0 then
    edit.Highlighter := nil
  else begin
    edit.Gutter.Visible := true;
    edit.Gutter.LineNumberPart().Visible := i>0;
    edit.Highlighter := TSynCustomHighlighter(fList.Objects[i]);
  end;
end;

destructor THighlighterHelper.Destroy;
var
  i: Integer;
begin
  for i:=0 to fList.Count-1 do
    fList.Objects[i].Free;
  fList.Free;
  inherited Destroy;
end;

end.

