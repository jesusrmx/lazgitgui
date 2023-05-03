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
    procedure register(hlclass: TSynCustomHighlighterClass);
  public
    constructor create;
    procedure SetHighlighter(edit:TSynEdit; fileType: string);
    destructor Destroy; override;
  end;

implementation

{ THighlighterHelper }

procedure THighlighterHelper.register(hlclass: TSynCustomHighlighterClass);
var
  hl: TSynCustomHighlighter;
begin
  if fList.IndexOf(hlClass.ClassName)<0 then begin
    hl := hlClass.Create(nil);
    fList.AddObject(hlclass.ClassName, hl);
  end;
end;

constructor THighlighterHelper.create;
begin
  inherited Create;
  fList := TStringList.Create;
  Register(TSynDiffSyn);
  Register(TSynFreePascalSyn);
  Register(TSynHTMLSyn);
  Register(TSynUNIXShellScriptSyn);
  Register(TSynCppSyn);
  Register(TSynJScriptSyn);
  Register(TSynLFMSyn);
  Register(TSynJavaSyn);
  Register(TSynCssSyn);
  Register(TSynPHPSyn);
  Register(TSynIniSyn);
  Register(TSynXMLSyn);
end;

procedure THighlighterHelper.SetHighlighter(edit: TSynEdit; fileType: string);
begin
  edit.Highlighter := TSynCustomHighlighter(fList.Objects[0]);
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

