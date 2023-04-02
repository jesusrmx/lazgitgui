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

  Ansi escapes highlighter helper for synedit.
}
unit unitansiescapes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, Graphics, SynEdit, SynHighlighterPosition,
  SynEditHighlighter;

type

  TLineEnding = string[3];

  { TAnsiEscapesHandler }

  TAnsiEscapesHandler = class
  private
    fEditor: TSynEdit;
    fBackground, fForeground: TColor;
    fStyles: TFontStyles;
    fHighlighter: TSynPositionHighlighter;
    fDefTokenKind: TtkTokenKind;
    fCodes: TStringList;
    fCol, fRow: Integer;
    fCurKind: TtkTokenKind;
    function GetAttribute(par,int,fin: string): TtkTokenKind;
    function TokenIdToAttributesStr(tokenId: TtkTokenKind): string;
  public
    constructor Create(aEditor: TSynEdit);
    destructor Destroy; override;
    procedure ProcessLine(aLine: string; le: TLineEnding);
    procedure SaveToFile(aFile:string);
    procedure LoadFromFile(aFile:string);
    procedure Reset;
    procedure Dump;
  end;

implementation

type
  TSetOfChar = set of char;

function GetField(var p: pchar; t: pchar; range:TSetOfChar; num:Integer=MaxInt): string;
var
  n: Integer;
begin
  n := 0;
  SetLength(result, 255);
  while (p<t) and (p^ in range) and (n<num) do begin
    inc(n);
    result[n] := p^;
    inc(p);
  end;
  SetLength(result, n);
end;

var
  counter: integer = 0;

procedure CodeToAttrs(code: Rawbytestring; var fg, bg: Integer; var st:TFontStyles);
var
  c: Integer;
begin
  c := StrToIntDef(code, -1);
  // ref: https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters
  case c of
    1 : Include(st, fsBold);                      // Bold or increased intensity
    3 : Include(st, fsItalic);                    // Italic
    4 : Include(st, fsUnderline);                 // Underline
    7 : begin c := bg; bg := fg; fg := c; end;    // Inverse
    9 : Include(st, fsStrikeOut);                 // Crossed-out, or strike
    22 : Exclude(st, fsBold);                     // Normal intensity
    23 : Exclude(st, fsItalic);                   // Neither italic, nor blackletter
    24 : Exclude(st, fsUnderline);                // Not underlined
    27 : begin c := bg; bg := fg; fg := c; end;   // Not reversed
    29 : Exclude(st, fsStrikeOut);                // Not crossed out
    30 : fg := clBlack;
    31 : fg := clRed;
    32 : if fsbold in st then fg := clLime else fg := clGreen;
    33 : fg := clYellow;
    34 : fg := clBlue;
    35 : fg := clPurple;
    36 : fg := clAqua;
    37 : fg := clWhite;
    39 : fg := clBlack;                           // Default foreground color
    40 : bg := clBlack;
    41 : bg := clRed;
    42 : bg := clGreen;
    43 : bg := clYellow;
    44 : bg := clBlue;
    45 : bg := clPurple;
    46 : bg := clAqua;
    47 : bg := clWhite;
    49 : bg := clWhite;                           // Default background color
  end;
  {
  // 2 : Not implemented;             // Faint, decreased intensity, or dim
  // 5 : Not Implemented;             // Slow blink
  // 6 : Not Implemented;             // Rapid blink
  // 8 : Not Implemented;             // Conceal or hide
  10 :  	Primary (default) font
  '11–19 	Alternative font 	Select alternative font n − 10
  ;
  '20 	Fraktur (Gothic) 	Rarely supported
  '21 	Doubly underlined; or: not bold 	Double-underline per ECMA-48,[5]:8.3.117 but instead disables bold intensity on several terminals, including in the Linux kernel's console before version 4.17.[43]
  25 	Not blinking 	Turn blinking off
  26 	Proportional spacing 	ITU T.61 and T.416, not known to be used on terminals
  28 	Reveal 	Not concealed
  38 	Set foreground color 	Next arguments are 5;n or 2;r;g;b
  48 	Set background color 	Next arguments are 5;n or 2;r;g;b
  49 	Default background color 	Implementation defined (according to standard)
  50 	Disable proportional spacing 	T.61 and T.416
  51 	Framed 	Implemented as "emoji variation selector" in mintty.[44]
  52 	Encircled
  53 	Overlined 	Not supported in Terminal.app
  54 	Neither framed nor encircled
  55 	Not overlined
  58 	Set underline color 	Not in standard; implemented in Kitty, VTE, mintty, and iTerm2.[40][41] Next arguments are 5;n or 2;r;g;b.
  59 	Default underline color 	Not in standard; implemented in Kitty, VTE, mintty, and iTerm2.[40][41]
  60 	Ideogram underline or right side line 	Rarely supported
  61 	Ideogram double underline, or double line on the right side
  62 	Ideogram overline or left side line
  63 	Ideogram double overline, or double line on the left side
  64 	Ideogram stress marking
  65 	No ideogram attributes 	Reset the effects of all of 60–64
  73 	Superscript 	Implemented only in mintty[44]
  74 	Subscript
  75 	Neither superscript nor subscript
  '90–97 	Set bright foreground color 	Not in standard; originally implemented by aixterm[29]
  '100–107 	Set bright background color
  }
end;

function TripleToStr(fg, bg: TColor; st: TFontStyles): string;
begin
  result := format('fg=%s,bg=%s,st=[',[ColorToString(fg), ColorToString(bg)]);
  if fsBold in st then result+='B';
  if fsItalic in st then result+='I';
  if fsUnderline in st then result+='U';
  if fsStrikeOut in st then result+='S';
  result+=']';
end;

{ TAnsiEscapesHandler }

function TAnsiEscapesHandler.GetAttribute(par, int, fin: string): TtkTokenKind;
var
  fg, bg: TColor;
  p, q, t: pchar;
  i: Integer;
  st: TFontStyles;
  attr : string;
  code: RawByteString;
begin
  if fin='m' then begin
    attr := par + int + fin;
    {$IFDEF DEBUG}
    dbgOut('attr=''%s'' ',[attr]);
    {$ENDIF}
    i := fCodes.IndexOf(attr);
    if i<0 then begin
      {$IFDEF DEBUG}
      dbgOut('not found, ');
      {$ENDIF}
      if par<>'' then begin
        fg := fForeground;
        bg := fBackground;
        st := fStyles;
        // scan desired parameters
        p := @par[1];
        t := p + Length(par);
        while p<t do begin
          q := strpos(p, ';');
          if q<>nil then SetString(code, p, q-p)
          else           code := p;
          CodeToAttrs(code, fg, bg, st);
          if q=nil then break;
          p := q+1;
        end;
      end;
      result := fHighlighter.CreateTokenID(attr, fg, bg, st);
      fForeground := fg;
      fBackground := bg;
      fStyles := st;
      i := fCodes.AddObject(attr, TObject(PtrInt(result)));
      {$IFDEF DEBUG}
      DbgOut('new token %d at %d with %s',[result, i, TripleToStr(fg, bg, st)]);
      {$ENDIF}
    end else begin
      result := PtrInt(fCodes.Objects[i]);
      {$IFDEF DEBUG}
      DbgOut('old token %d at %d',[result, i]);
      {$ENDIF}
    end;
  end else begin
    result := fDefTokenKind;
    {$IFDEF DEBUG}
    DbgOut('not "m", Using default token');
    {$ENDIF}
  end;
end;

function TAnsiEscapesHandler.TokenIdToAttributesStr(tokenId: TtkTokenKind
  ): string;
var
  attrs: TSynHighlighterAttributes;
begin
  attrs := fHighlighter.GetCopiedAttribute(tokenId);
  if attrs=nil then exit('nil');
  result := TripleToStr(attrs.Foreground, attrs.Background, attrs.Style);
end;

constructor TAnsiEscapesHandler.Create(aEditor: TSynEdit);
begin
  inherited Create;
  fEditor := aEditor;
  fHighlighter := TSynPositionHighlighter.Create(aEditor.Owner);
  fCodes := TStringList.Create;
  fCodes.Duplicates := dupError;
  fCodes.Sorted := true;

  fBackground := fEditor.Color;
  fForeground := fEditor.Font.Color;
  fDefTokenKind := fHighlighter.CreateTokenID('m', fForeground, fBackground, []);

  fCodes.AddObject('m', TObject(PtrInt(fDefTokenKind)));
  fEditor.Highlighter := fHighlighter;

  reset;
end;

destructor TAnsiEscapesHandler.Destroy;
begin
  fCodes.Free;
  inherited Destroy;
end;

procedure TAnsiEscapesHandler.ProcessLine(aLine: string; le: TLineEnding);
var
  p, q, t: pchar;
  parBytes, intBytes, finBytes: string;
  txt, portion: string;
  len: Integer;
begin
  {$IFDEF DEBUG}
  DebugLnEnter('TForm1.ProcessLine INIT: fLine=%s', [dbgstr(fLine)]);
  {$ENDIF}
  txt := '';
  fCol := 0;

  p := @aLine[1];
  t := p + length(aLine);
  repeat
    q := strpos(p, #27'[');

    if q<>nil then SetString(portion, p, q-p)
    else           portion := p;
    len := Length(portion);

    // from fCol to fCol+len
    fHighlighter.AddToken(fRow, fCol+len, fCurKind);

    //WriteLn('Text: ''', p, '''');
    txt += portion;
    inc(counter);

    if q=nil then
      break;

    inc(fCol, len);

    // advance CSI
    p := q + 2;

    // bytes
    parBytes := GetField(p, t, [#$30..#$3F]);
    intBytes := GetField(p, t, [#$20..#$2F]);
    finBytes := GetField(p, t, [#$40..#$7E], 1);

    fCurKind := GetAttribute(parBytes, intBytes, finBytes);

    {$IFDEF DEBUG}
    DebugLn(' ''%s''', [portion]);
    {$ENDIF}
  until (p>=t);

  if pos(#13, le)>0 then
    fCol := 0;

  while fRow+1>fEditor.Lines.Count do
    fEditor.Lines.Add('');

  fEditor.Lines[fRow] := txt;

  if pos(#10, le)>0 then
    inc(fRow);

  //fEditor.Lines.Add(txt);
  {$IFDEF DEBUG}
  DebugLnExit('TForm1.ProcessLine DONE: txt=%s', [txt]);
  {$ENDIF}
end;

function SortMe(List: TStringList; Index1, Index2: Integer): Integer;
begin
  result := PtrInt(List.Objects[Index2]) - PtrInt(List.Objects[Index1]);
end;

function FontStylesToByte(styles: TFontStyles): byte;
begin
  //(fsBold, fsItalic, fsUnderline, fsStrikeOut);
  result := 0;
  if fsBold in styles       then result := result or (1 shl 0);
  if fsItalic in styles     then result := result or (1 shl 1);
  if fsUnderline in styles  then result := result or (1 shl 2);
  if fsStrikeOut in styles  then result := result or (1 shl 3);
end;

function ByteToFontStyles(styles: byte): TFontStyles;
begin
  result := [];
  if styles and (1 shl 0) <> 0 then Include(result, fsBold);
  if styles and (1 shl 1) <> 0 then Include(result, fsItalic);
  if styles and (1 shl 2) <> 0 then Include(result, fsUnderline);
  if styles and (1 shl 3) <> 0 then Include(result, fsStrikeOut);
end;

procedure TAnsiEscapesHandler.SaveToFile(aFile: string);
var
  L: TStringList;
  i, j: Integer;
  M: TMemoryStream;
  tokenId: TtkTokenKind;
  attrs: TSynHighlighterAttributes;
  c: TColor;
  Tokens: PPositionTokens;
begin
  // save token list
  L := TStringList.Create;
  L.Assign(fCodes);
  L.CustomSort(@SortMe);
  M := TMemoryStream.Create;

  M.WriteDWord($97538642);
  M.WriteWord(L.Count);
  for i:=0 to L.Count-1 do begin
    M.WriteAnsiString(L[i]);
    tokenId := TtkTokenKind(PtrInt(L.Objects[i]));
    attrs := fHighlighter.GetCopiedAttribute(tokenId);
    c := attrs.Foreground; M.Write(c, SizeOf(c));
    c := attrs.Background; M.Write(c, SizeOf(c));
    M.WriteByte(FontStylesToByte(attrs.Style));
  end;
  M.WriteWord(fEditor.Lines.Count);
  for i:=0 to fEditor.Lines.Count do begin
    Tokens := fHighlighter.Tokens[i+1];
    if Tokens=nil then begin
      M.WriteWord(0);
      continue;
    end;
    M.WriteWord(Tokens^.Count);
    for j:=0 to Tokens^.Count-1 do begin
      M.WriteWord(Tokens^.Tokens[j].Column);
      M.WriteWord(-Tokens^.Tokens[j].Kind);
    end;
  end;
  fEditor.Lines.SaveToStream(M);
  M.SaveToFile(aFile);
  M.Free;
  L.Free;
end;

procedure TAnsiEscapesHandler.LoadFromFile(aFile: string);
var
  M: TMemoryStream;
  w: Word;
  dw: Cardinal;
  i, j, n, t, col: Integer;
  fg, bg: TColor;
  code: string;
  styles:byte;
  kind: TtkTokenKind;
begin
  M := TMemoryStream.Create;
  try
    M.LoadFromFile(aFile);
    dw := M.ReadDWord;
    if dw<>$97538642 then raise Exception.Create('Invalid file');

    fEditor.Clear;
    fCodes.Clear;
    fHighlighter.ClearAllTokens;
    fHighlighter.ClearAllCopiedAttributes;

    n := M.ReadWord;
    for i:=0 to n-1 do begin
      code := M.ReadAnsiString;
      M.Read(fg, SizeOf(fg));
      M.Read(bg, SizeOf(bg));
      Styles := M.ReadByte;
      kind := fHighlighter.CreateTokenID(code, fg, bg, ByteToFontStyles(styles));
      if i=0 then
        fDefTokenKind := kind;
    end;

    n := M.ReadWord;
    for i:=0 to n-1 do begin
      t := M.ReadWord;
      for j:=1 to t do begin
        col := M.ReadWord;
        kind := M.ReadWord;
        fHighlighter.AddToken(i, col, -kind);
      end;
    end;

    fEditor.Lines.LoadFromStream(M);

  finally
    M.Free;
  end;
end;

procedure TAnsiEscapesHandler.Reset;
begin
  fCurKind := fDefTokenKind;
  fRow := 0;
  fHighlighter.ClearAllTokens;
end;

procedure TAnsiEscapesHandler.Dump;
var
  attrs: TSynHighlighterAttributes;
  Tokens: PPositionTokens;
  i, j: Integer;
  tokenId: TtkTokenKind;
begin
  DebugLn;
  DebugLn('Code Table, %d entries', [fCodes.Count]);
  for i:=0 to fCodes.Count-1 do begin
    tokenId := TtkTokenKind(PtrInt(fCodes.Objects[i]));
    DebugLn('%2d %3d %s => %s',[i, tokenId, fCodes[i], TokenIdToAttributesStr(tokenId)]);
  end;

  DebugLn;
  DebugLn('Highlighter: %d attributes', [fHighlighter.AttrCount]);
  for i:=0 to fHighlighter.AttrCount-1 do begin
    attrs := fHighlighter.Attribute[i];
    DebugLn('%2d %s',[i, attrs.Name]);
  end;
  DebugLn;
  DebugLn('Dumping Tokens on %d lines',[fEditor.Lines.Count]);
  for i:=0 to fEditor.Lines.Count-1 do begin
    Tokens := fHighlighter.Tokens[i+1];
    DbgOut('%.3d ',[i+1]);
    if Tokens=nil then begin
      DebugLn('Nil');
      continue;
    end else
      DebugLn('%d tokens:',[Tokens^.Count]);
    for j:=0 to Tokens^.Count-1 do begin
      with Tokens^.Tokens[j] do begin
        DebugLn('%2d col=%d id=%d',[j, Column, Kind]);
      end;
    end;

  end;
end;

end.

