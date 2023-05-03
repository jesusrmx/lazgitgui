unit unithighlighterhelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynEdit, SynHighlighterDiff,
  SynHighlighterPas, SynHighlighterXML, SynHighlighterHTML,
  synhighlighterunixshellscript, SynHighlighterCpp, SynHighlighterJScript,
  SynHighlighterLFM, SynHighlighterJava, SynHighlighterCss, SynHighlighterPHP,
  SynHighlighterIni;

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
  Register(TSynDiffSyn);
  Register(TSynFreePascalSyn, '|*.lpr');
  Register(TSynHTMLSyn);
  Register(TSynUNIXShellScriptSyn);
  Register(TSynCppSyn);
  Register(TSynJScriptSyn);
  Register(TSynLFMSyn);
  Register(TSynJavaSyn);
  Register(TSynCssSyn);
  Register(TSynPHPSyn);
  Register(TSynIniSyn);
  Register(TSynXMLSyn, '|*.lpi');
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
  else
    edit.Highlighter := TSynCustomHighlighter(fList.Objects[i]);
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

