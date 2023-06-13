unit unittextchunks;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, Graphics, StrUtils, RegExpr,
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
    linkDest: string;
    linkAction: string;
    linkColor: string;
  end;
  TTextChunks = array of TTextChunksItem;

  TTextLinkItem = record
    Name: string;
    Pattern: string;
    Replace: string;
    action: string;
    color: string;
  end;
  TTextLinksArray = array of TTextLinkItem;

  PTextPos = ^TTextPos;
  TTextPos = record
    pos: Integer;
    Len: Integer;
    text: string;
    action: string;
    color: string;
  end;

  { TTextLinks }

  TTextLinks = class
  private
    fLinks: TTextLinksArray;
    function GetCount: Integer;
  public
    procedure LoadFromConfig(section: string);
    procedure FindLinks(text:string; var chunks: TTextChunks);
    procedure Dump;
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

  if (fTextLinks<>nil) and (fTextLinks.Count>0) then begin

    // links were configured, find links in the subject
    n := length(result);
    fTextLinks.FindLinks(aItem.Subject, result);

    // for each found link, calc it's physical coords and setup color properties
    for i:=n to Length(result)-1 do begin
      //WriteStr(s, result[i].itemType);
      //DebugLn('Chunk %2d: type=%s %s -> %s (%s)',[i, s, result[i].Text, result[i].linkDest, result[i].linkAction]);
      result[i].r := Rect(x, aRect.Top+1, x + canvas.TextWidth(result[i].text), aRect.Bottom-1);
      result[i].brushStyle := bsClear;
      result[i].PenStyle := psClear;
      if result[i].itemType=tcitNone then result[i].FontColor := clBlack
      else                                result[i].FontColor := StringToColorDef(result[i].linkColor, clRed);
      x := result[i].r.Right;
    end;

  end else begin

    // no links are were configured, whole subject is a 'none' fragment
    item.brushStyle := bsClear;
    item.penStyle := psClear;
    item.fontColor := clBlack;
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
      if L.Count>=5 then begin
        fLinks[i-1].name := L[0];
        fLinks[i-1].pattern := L[1];
        fLinks[i-1].replace := L[2];
        fLinks[i-1].action := L[3];
        fLinks[i-1].color := L[4];
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

procedure TTextLinks.FindLinks(text: string; var chunks: TTextChunks);
var
  L: TfpList;
  reg: TRegExpr;
  txtPos: PTextPos;
  i, j, offset: Integer;
  s: string;

  procedure Add(start, len, aIndex: Integer; linkText, linkAction, linkColor: string);
  var
    n: Integer;
  begin
    n := Length(chunks);
    SetLength(chunks, n+1);
    chunks[n].text := copy(text, start, len);
    if aIndex<0 then begin
      chunks[n].itemType := tcitNone;
      chunks[n].linkIndex := -1;
      chunks[n].linkDest := '';
      chunks[n].linkColor := linkColor;
    end else begin
      chunks[n].itemType := tcitLink;
      chunks[n].linkIndex := n;
      chunks[n].linkDest := linkText;
      chunks[n].linkAction := linkAction;
      chunks[n].linkColor := linkColor;
    end;
  end;

begin
  L := TfpList.Create;
  reg := TRegExpr.create;
  reg.InputString := text;
  try
    // for each configured link try to find matching chunks
    for i:=0 to Count-1 do begin
      s := fLinks[i].Pattern;
      //DebugLn('Trying %d %s',[i+1, s]);
      reg.Expression := s;
      offset := 1;
      while reg.Exec(offset) do begin
        // a link has been matched, record its position, length, replacement and action
        new(txtPos);
        txtPos^.pos := reg.MatchPos[0];
        txtPos^.Len := reg.MatchLen[0];
        if fLinks[i].Replace<>'' then
          txtPos^.text := reg.Substitute(fLinks[i].Replace);
        txtPos^.action := fLinks[i].action;
        txtPos^.color  := fLinks[i].color;
        L.Add(txtPos);
        // try match more links past the previous one.
        offset :=  txtPos^.pos + txtPos^.Len;
      end;
    end;

    // if any chunks were found, split 'text' according to their position
    if L.Count>0 then begin
      // matched chunks can be in any order depending on how the links
      // were declared, sort them by position.
      L.Sort(@CompareTextPositions);
      {
      DebugLn('For %s found %d links',[QuotedStr(text), L.Count]);
      for i:=0 to L.Count-1 do begin
        txtPos := L[i];
        DebugLn('%d: at %d: %s',[i+1, txtPos^.pos, copy(text, txtPos^.pos, txtPos^.Len)]);
      end;
      }
      // for any found chunk, check if there is any previous 'none' fragment, report both
      offset := 1;
      for i:=0 to L.Count-1 do begin
        txtPos := L[i];
        if txtPos^.pos - offset>0 then
          Add(offset,  txtPos^.pos - offset, -1, '', '', '');
        Add(txtPos^.pos, txtPos^.Len, i, txtPos^.text, txtPos^.action, txtPos^.color);
        offset := txtPos^.Pos + txtPos^.Len;
      end;
      // check if there is a 'none' fragment at the end
      if offset<Length(text) then
        Add(offset, MAXINT, -1, '', '', '');
    end else
      // if no chunks were found, report the whole 'text' as a none fragment
      Add(1, MAXINT, -1, '', '', '');
  finally
    reg.free;
    L.Free;
  end;
end;

procedure TTextLinks.Dump;
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    DebugLn('%d: Name=%s Pattern=%s Replace=%s Action=%s',
      [i, QuotedStr(fLinks[i].Name), QuotedStr(fLinks[i].Pattern), QuotedStr(fLinks[i].Replace), QuotedStr(fLinks[i].Action)]);
  end;
end;

end.

