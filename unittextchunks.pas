unit unittextchunks;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StrUtils, RegExpr,
  unitcommon, unitgittypes, unitgitutils, unitconfig, unitdbindex;

type
  TTextChunksItemType = (tcitNone, tcitBox, tcitLink);
  TTextChunksItem = record
    itemType: TTextChunksItemType;
    r: TRect;
    brushStyle: TBrushStyle;
    brushColor: TColor;
    penStyle: TPenStyle;
    penColor: TColor;
    penWidth: Integer;
    fontColor: TColor;
    text: string;
    linkIndex: Integer;
  end;
  TTextChunks = array of TTextChunksItem;

  TTextLinkItem = record
    Name: string;
    Pattern: string;
    Replace: string;
    action: string;
  end;
  TTextLinksArray = array of TTextLinkItem;

  PTextPos = ^TTextPos;
  TTextPos = record
    pos: Integer;
    Len: Integer;
  end;

  { TTextLinks }

  TTextLinks = class
  private
    fLinks: TTextLinksArray;
    function GetCount: Integer;
  public
    procedure LoadFromConfig(section: string);
    procedure FindLinks(canvas:TCanvas; x: Integer; text:string; var chunks: TTextChunks);
    property count: Integer read GetCount;
  end;

  function GetTextChunks(canvas: TCanvas; aRect:TRect; x: integer; refsMap: TRefsMap; aItem: TLogItem): TTextChunks;

var
  fTextLinks: TTextLinks = nil;

implementation

function GetTextChunks(canvas: TCanvas; aRect:TRect; x: integer; refsMap: TRefsMap; aItem: TLogItem): TTextChunks;
var
  n, i, j, w: Integer;
  arr: TRefInfoArray;
  item: TTextChunksItem;
  s: String;
begin

  j := 0;
  result := nil;

  // Get refs items into chunksitems
  if (refsMap<>nil) and refsMap.Find(aItem.CommitOID, n ) then begin
    arr := refsMap.Data[n];

    for i:=0 to Length(arr)-1 do begin

      w := canvas.TextWidth(arr[i]^.refName) + 6;

      case arr[i]^.subType of
        rostLocal:
          begin
            if arr[i]^.head then item.brushColor := clRed
            else                 item.brushColor := clGreen;
            item.fontColor := clWhite;
          end;
        rostTracking:
          begin
            item.brushColor := $AADDFF;
            item.fontColor := clBlack;
          end;
        rostTag:
          begin
            item.brushColor := clYellow;
            item.fontColor := clBlack;
          end;
      end;

      item.r := Rect(x, aRect.Top+1, x + w, aRect.Bottom-1);
      item.penWidth := 1;
      item.brushStyle := bsSolid;
      item.penStyle := psSolid;
      item.penColor := clBlack;
      item.text := arr[i]^.refName;
      item.itemType := tcitBox;

      j := Length(result);
      SetLength(result, j+1);
      result[j] := item;

      x += w + 2;
      if i=Length(arr)-1 then
        x += 5;
    end;

  end;

  // Get textchunks derived from the subject
  item.brushStyle := bsClear;
  item.brushColor := clWhite;
  item.penStyle := psClear;
  item.penColor := clBlack;
  item.penWidth := 1;
  item.fontColor := clBlack;

  if (fTextLinks<>nil) and (fTextLinks.Count>0) then
    fTextLinks.FindLinks(canvas, x, aItem.Subject, result)
  else begin
    item.text := aItem.Subject;
    item.r := rect(x, aRect.Top+1, aRect.Right, aRect.Bottom-1);
    item.itemType := tcitNone;
    j := Length(result);
    SetLength(result, j+1);
    result[j] := item;
  end;

end;

{ TTextLinks }

function TTextLinks.GetCount: Integer;
begin
  result := Length(fLinks);
end;

procedure TTextLinks.LoadFromConfig(section: string);
var
  n, i: Integer;
  s: string;
  L: TStringList;
begin
  L := TStringList.Create;
  fConfig.OpenConfig;
  try
    n := fConfig.ReadInteger('links', 0, section);
    SetLength(fLinks, n);

    for i:=1 to n do begin
      s := fConfig.ReadString('link'+IntToStr(i), '', section);
      DecodeDelimitedText( s, CMDSEP, L);
      if L.Count>=4 then begin
        fLinks[i].name := L[0];
        fLinks[i].pattern := L[1];
        fLinks[i].replace := L[2];
        fLinks[i].action := L[3];
      end;
    end;

  finally
    fConfig.CloseConfig;
    L.Free;
  end;
end;

function CompareTextPositions(a, b: Pointer): Integer;
var
  aPos: PTextPos absolute a;
  bPos: PTextPos absolute b;
begin
  result := aPos^.pos - bPos^.pos;
end;

procedure TTextLinks.FindLinks(canvas: TCanvas; x: Integer; text: string;
  var chunks: TTextChunks);
var
  L: TfpList;
  reg: TRegExpr;
  txtPos: PTextPos;
  i, offset: Integer;
begin
  L := TfpList.Create;
  reg := TRegExpr.create;
  try
    for i:=0 to Count-1 do begin
      reg.Expression := fLinks[i].Pattern;
      offset := 1;
      while reg.Exec(offset) do begin
        new(txtPos);
        txtPos^.pos := reg.MatchPos[0];
        txtPos^.Len := reg.MatchLen[0];
        L.Add(txtPos);
        offset := reg.MatchLen[0] + 1;
      end;
    end;
    L.Sort(@CompareTextPositions);

  finally
    reg.free;
    L.Free;
  end;
end;

end.

