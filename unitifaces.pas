unit unitifaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

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

implementation

end.

