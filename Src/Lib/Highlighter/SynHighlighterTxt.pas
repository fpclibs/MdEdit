{
 *******************************************************************************
 *                                                                             *
 *  LGPL with linking exception (like Lazarus). See the file license.md,       *
 *  included in this distribution, for details about the copyright.            *
 *                                                                             *
 *  This program is distributed in the hope that it will be useful,            *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                       *
 *                                                                             *
 *  Sources: Andreas Peter Luft, January 10 2021                               *
 *                                                                             *
 *******************************************************************************
}
unit SynHighlighterTxt;

//{$I SynEdit.inc}

interface
uses
  Classes, SysUtils, Graphics, SynEditHighlighter, SynEditStrConst,
  fgl, FormDebug;

type

  TtkTokenKind = (
    tkWord,
    tkNumber,
    tkSpace,
    tkTab,
    tkSymbol,
    tkEscape,
    tkUnknown,
    tkLineStart,
    tkLineEnd
  );

  TSctToken = record
    Pos: Integer;
    Len: Integer;
    Tok: TtkTokenKind;
  end;

  TTk = ^TSctToken;

  TTokenList = specialize TFPGList<TTk>;


  { TSynTokenizer }

  TSynTokenizer = class

    constructor Create;
    destructor Destroy; override;

  protected
    mTokenList: TTokenList;
    mPos: Integer;

  protected
    function  GetItem(index: Integer): TTk;
    procedure SetItem(index: Integer; tk: TTk);

  public
    function Count: Integer;
    function Pos: Integer;
    function Pos(index: Integer): Boolean;
    procedure Clear;
    function Last: TTk;
    function This: TTk;
    function Next: TTk;
    property Items[index: Integer]: TTk read GetItem write SetItem; default;
    procedure Tokenize(mLine: PChar; posEnd: Integer);
    function TokenList: TTokenList;

  protected

end;


type

  { TList<T> }

  generic TList<T> = class
    Items: array of T;
    procedure Add(Value: T);
    procedure Remove(Index: Integer);
    procedure RemoveToEnd(Index: Integer);
    function Count: Integer;
    function Copy: TList;
    procedure Clear;
  end;


  TItemList  = specialize TList<Integer>;
  TRangeList = specialize TFPGList<TItemList>;


  { TSynTxtSyn }

  TSynTxtSyn = class(TSynCustomHighlighter)
  protected
    posBgn, posEnd: Integer;
    linAct: String;

    mToken: TSynTokenizer;
    mLine: PChar;
    mLineNumber: Integer;
    mTokenPos: Integer;
    mFlagEmptyLine: Boolean;
    mCountSetLine: Integer;

    mDbgLineBreak: Integer;

    attriHead:       TSynHighlighterAttributes;
    attriWord:       TSynHighlighterAttributes;
    attriNumber:     TSynHighlighterAttributes;
    attriSpace:      TSynHighlighterAttributes;
    attriTab:        TSynHighlighterAttributes;
    attriSymbol:     TSynHighlighterAttributes;
    attriEscape:     TSynHighlighterAttributes;
    attriUnknown:    TSynHighlighterAttributes;
    attriString:     TSynHighlighterAttributes;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer; bgn: Integer);
    procedure Next; override;
    function  GetEol: Boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function  CreateHighlighterAttributes(AName:PString; Foreground,
      Background: TColor; FontStyles: TFontStyles): TSynHighlighterAttributes;
    function  GetToken: String; override;
    function  GetTokenPos: Integer; override;
    function  GetTokenKind: integer; override;
    function  GetDefaultAttribute({%H-}Index: integer): TSynHighlighterAttributes; override;
    function  GetRange: Pointer; override;

    class function GetLanguageName: string; override;

  protected
    function GetSampleSource : String; override;

end;


implementation


{ Class TList<> }

{-------------------------------------------------------------------------------
  @NAME: Add
 ------------------------------------------------------------------------------}
procedure TList.Add(Value: T);
begin
  SetLength(Items, Count() + 1);
  Items[Count() - 1] := Value;
end;


{-------------------------------------------------------------------------------
  @NAME: Count
 ------------------------------------------------------------------------------}
function TList.Count: Integer;
begin
  Result := Length(Items);
end;


{-------------------------------------------------------------------------------
  @NAME: Copy
 ------------------------------------------------------------------------------}
function TList.Copy(): TList;
var
  i:Integer;
begin
  Result := TList.Create();
  if Not (Length(Items) > 0) then exit;

  for i:=0 to (Length(Items)-1) do Result.Add(Items[i]);
end;


{-------------------------------------------------------------------------------
  @NAME: Clear
 ------------------------------------------------------------------------------}
procedure TList.Clear;
begin
    if Length(Items) > 0 then
      SetLength(Items, 0);
end;


{-------------------------------------------------------------------------------
  @NAME: Remove
 ------------------------------------------------------------------------------}
procedure TList.Remove(Index: Integer);
var
  i: Integer;
begin
  if Index = (Length(Items)-1) then
  begin
    SetLength(Items, Length(Items)-2);
    exit;
  end;
  if (Index > 0) And (Index < (Length(Items))) then
  begin
    for i:=Index to (Length(Items)-1) do
    begin
      Items[i] := Items[i+1];
    end;
    SetLength(Items, (Length(Items)-1));
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: RemoveToEnd
 ------------------------------------------------------------------------------}
procedure TList.RemoveToEnd(Index: Integer);
begin
  if (Index >= 0) And (Index < Length(Items)) then
  begin
    SetLength(Items, Index);
  end;
end;



{ Class TSynTokenizer }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSynTokenizer.Create;
begin
  inherited Create;
  mTokenList := TTokenList.Create;
  mPos := 0;
end;


{-------------------------------------------------------------------------------
  @NAME: Destroy
 ------------------------------------------------------------------------------}
destructor TSynTokenizer.Destroy;
begin
  Clear;
  mTokenList.Free;

  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  @NAME: GetItem
 ------------------------------------------------------------------------------}
function  TSynTokenizer.GetItem(index: Integer): TTk;
begin
  if (index >= 0) And (index < mTokenList.Count) then
  begin
    Result := mTokenList.Items[index];
    exit;
  end;
  Result := Nil;
end;


{-------------------------------------------------------------------------------
  @NAME: SetItem
 ------------------------------------------------------------------------------}
procedure TSynTokenizer.SetItem(index: Integer; tk: TTk);
begin
  mTokenList.Items[index] := tk;
end;


{-------------------------------------------------------------------------------
  @NAME: Count
 ------------------------------------------------------------------------------}
function TSynTokenizer.Count: Integer;
begin
  Result := mTokenList.Count;
end;


{-------------------------------------------------------------------------------
  @NAME: Pos
 ------------------------------------------------------------------------------}
function TSynTokenizer.Pos: Integer;
begin
  Result := mPos;
end;


{-------------------------------------------------------------------------------
  @NAME: Pos
 ------------------------------------------------------------------------------}
function TSynTokenizer.Pos(index: Integer): Boolean;
begin
  Result := False;
  if Not ( (index >= 0) And (index < mTokenList.Count) ) then  exit;

  mPos   := index;
  Result := True;
end;


{-------------------------------------------------------------------------------
  @NAME: Clear
 ------------------------------------------------------------------------------}
procedure TSynTokenizer.Clear;
var
  i: Integer;
begin
  For i := 1 to mTokenList.Count do
  begin
    Dispose(mTokenList[i-1]);
  end;
  mTokenList.Clear;
end;


{-------------------------------------------------------------------------------
  @NAME: Last
 ------------------------------------------------------------------------------}
function TSynTokenizer.Last: TTk;
begin
  if mPos > 0 then
    Result := mTokenList[mPos-1]
  else Result := mTokenList[mPos];
end;


{-------------------------------------------------------------------------------
  @NAME: This
 ------------------------------------------------------------------------------}
function TSynTokenizer.This: TTk;
begin
  Result := mTokenList[mPos];
end;


{-------------------------------------------------------------------------------
  @NAME: Next
 ------------------------------------------------------------------------------}
function TSynTokenizer.Next: TTk;
begin
  if mPos+1 < mTokenList.Count then
    Result := mTokenList[mPos+1]
  else Result := mTokenList[mPos];
end;


{-------------------------------------------------------------------------------
  @NAME: Tokenize
 ------------------------------------------------------------------------------}
procedure TSynTokenizer.Tokenize(mLine: PChar; posEnd: Integer);
var
  tkToken : TtkTokenKind;
  l       : Integer;
  sTk     : TTk;
  posBgn  : Integer;
begin
  Clear;

  New(sTk);
  sTk^.Tok := tkLineStart;
  sTk^.Pos := 0;
  sTk^.Len := 0;
  mTokenList.Add(sTk);

  l := length(mLine);
  while  posEnd < l do
  begin

    tkToken := tkUnknown;
    posBgn  := posEnd;

    // At Spacespaces
    if (mLine[posEnd] in [#9]) then
    begin
      // Space or Tab
      tkToken := tkTab;
      inc (posEnd);
    end
    else if mLine[posEnd] in [' '] then
    // At Spacespaces
    while (mLine[posEnd] in [' ']) and not (mLine[posEnd] in [#9]) do
    begin
      // Space or Tab
      tkToken := tkSpace;
      inc(posEnd);
      if posEnd - posBgn > 3 then begin tkToken := tkTab; break; end;
    end
    else if (posEnd <= l) and (mLine[posEnd] in [#48..#57]) then
    // At Numbers
    while (posEnd <= l) and (mLine[posEnd] in [#48..#57]) do
    begin
      // Space or Tab
      tkToken := tkNumber;
      inc(posEnd);
    end
    else if (mLine[posEnd] in ['\']) then
    // At Escapes
    while (mLine[posEnd] in ['\']) do
    begin
      // get Escapes
      if posBgn <> posEnd then break;
      tkToken := tkSymbol;
      inc(posEnd);
      if LowerCase(mLine[posEnd]) in ['\', '`', '*', '>', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!'] then
      begin
        tkToken := tkEscape;
        inc(posEnd);
      end;
      break;
    end
    else if (posEnd <= l) and not(mLine[posEnd] in [#9, '\', ' ']) then
    // At None-Space
    while ( (posEnd <= l) and (not (mLine[posEnd] in [#9, '\', ' ', #0]) ) ) do
    begin
      // split Word and Symbols
      if LowerCase(mLine[posEnd]) in ['@', '<', '>', '/', '`', '"', '*', '_', '=', '''', '(', ')', '{', '}', '[', ']', '#', '+', '-', ',', '.', '?', '!', '~', ':', ';', '|'] then
      begin
        if posBgn <> posEnd then break;
        tkToken := tkSymbol;
        inc(posEnd);
        break;
      end
      else
      begin
        tkToken := tkWord;
        inc(posEnd);
      end;
    end;
    New(sTk);
    sTk^.Tok := tkToken;
    sTk^.Pos := posBgn;
    sTk^.Len := posEnd-posBgn;
    mTokenList.Add(sTk);
  end;

  New(sTk);
  sTk^.Tok := tkLineEnd;
  sTk^.Pos := l;
  sTk^.Len := 0;

  mTokenList.Add(sTk);
  posEnd := 0;
  posBgn := 0;
end;


{-------------------------------------------------------------------------------
  @NAME: TokenList
 ------------------------------------------------------------------------------}
function TSynTokenizer.TokenList: TTokenList;
begin
  Result := mTokenList;
end;



{ Class TSynTxtSyn }

{-------------------------------------------------------------------------------
  @NAME: Create

  @INFO:
  Class Constructor. Here you must initialize the attributes to use.
 ------------------------------------------------------------------------------}
constructor TSynTxtSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  mToken := TSynTokenizer.Create;
  mFlagEmptyLine := False;

  mDbgLineBreak := -1;

  attriHead := TSynHighlighterAttributes.Create('Head', 'Head');
  attriHead.Foreground := TColor($FF8000);
  attriHead.Background := TColor($FFFFFF);
  attriHead.Style := [fsBold];
  AddAttribute(attriHead);

  attriNumber := TSynHighlighterAttributes.Create('Number', 'Number');
  attriNumber.Foreground := TColor($FF8000);
  attriNumber.Background := TColor($FFFFFF);
  attriNumber.Style := [];
  AddAttribute(attriNumber);

  attriString := TSynHighlighterAttributes.Create('String', 'String');
  attriString.Foreground := TColor($FF8000);
  attriString.Background := TColor($FFFFFF);
  attriString.Style := [fsBold];
  AddAttribute(attriString);

  attriWord := TSynHighlighterAttributes.Create('Word', 'Word');
  attriWord.Foreground := TColor($000000);
  attriWord.Background := TColor($FFFBFA);
  attriWord.Style := [];
  AddAttribute(attriWord);

  attriSpace := TSynHighlighterAttributes.Create('Space', 'Space');
  AddAttribute(attriSpace);

  attriTab := TSynHighlighterAttributes.Create('Tab', 'Tab');
  AddAttribute(attriTab);

  attriSymbol := TSynHighlighterAttributes.Create('Symbol', 'Symbol');
  attriSymbol.Foreground := TColor($3355EE);
  attriSymbol.Background := TColor($FFFFFF);
  attriSymbol.Style := [];
  AddAttribute(attriSymbol);

  attriEscape := TSynHighlighterAttributes.Create('Escape', 'Escape');
  attriEscape.Foreground := TColor($0000FF);
  attriEscape.Background := TColor($FFFFFF);
  attriEscape.Style := [];
  AddAttribute(attriEscape);

  attriUnknown := TSynHighlighterAttributes.Create('Unknown', 'Unknown');
  attriUnknown.Foreground := TColor($FF8000);
  attriUnknown.Background := TColor($FFFFFF);
  attriUnknown.Style := [fsBold];
  AddAttribute(attriUnknown);
end;


{-------------------------------------------------------------------------------
  @NAME: Destroy

  @INFO:
  Class Destructor.
 ------------------------------------------------------------------------------}
destructor TSynTxtSyn.Destroy;
begin
  mToken.Destroy;

  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  @NAME: SetLine

  @INFO:
  It is called by the editor, every time you need to update the information of
  Colored on a line. After calling this function, it is expected that
  GetTokenEx, return the current token. And also after each call to
  "Next".
 ------------------------------------------------------------------------------}
procedure TSynTxtSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  mLine       := PChar(NewValue);
  mLineNumber := LineNumber;

  linAct := NewValue;   // copy the current line

  posBgn := 0;
  posEnd := 0;
  mTokenPos := 0;
  mCountSetLine := 1;

  OFormDebug.Append:='SetLine: ' + IntToStr(mLineNumber + 1);

  mToken.Tokenize(mLine, posEnd);
  posBgn := 1;
end;


{-------------------------------------------------------------------------------
  @NAME: SetLine

  @INFO:
  It is called by the editor, every time you need to update the information of
  Colored on a line. After calling this function, it is expected that
  GetTokenEx, return the current token. And also after each call to
  "Next".
 ------------------------------------------------------------------------------}
procedure TSynTxtSyn.SetLine(const NewValue: String; LineNumber: Integer; bgn: Integer);
begin
  mLine       := PChar(NewValue);
  mLineNumber := LineNumber;

  linAct := NewValue;   // copy the current line

//  dec(bgn); if bgn < 0 then bgn := 0;
  posBgn := 0;
  posEnd := 0;
  mTokenPos := 0;
  mCountSetLine := 1;

  OFormDebug.Append:='SetLine: ' + IntToStr(mLineNumber + 1);

  mToken.Tokenize(mLine, bgn);
  posBgn := 1;
end;


{-------------------------------------------------------------------------------
  @NAME: Next

  @INFO:
  It is called by SynEdit, to access the next Token. And it is executed by
  each token of the current line. In this example you will always move a
  character.
 ------------------------------------------------------------------------------}
procedure TSynTxtSyn.Next;
begin
 if mCountSetLine > 1 then
  begin
    mCountSetLine := 0;
  end;
  Inc(posBgn);
  OFormDebug.Append:='Next: ' + IntToStr(posBgn);
end;


{-------------------------------------------------------------------------------
  @NAME: GetEol

  @INFO:
  Indicates when the end of the line has been reached
 ------------------------------------------------------------------------------}
function TSynTxtSyn.GetEol: Boolean;
var
  sTkP : TTk;   // Pos  Tk
begin
  Result := true;
  if mToken.Pos(posBgn) then
  begin
    sTkP := mToken.This;
    Result := false;
    if sTkP^.Tok = tkLineEnd then
    begin
       Result := true;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenEx

  @INFO:
  Returns information about the current token
 ------------------------------------------------------------------------------}
procedure TSynTxtSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
var
  sTkP : TTk;   // Pos  Tk
begin
  mToken.Pos(posBgn);
  sTkP := mToken.This;
  TokenLength := sTkP^.Len;
  TokenStart  := mLine + sTkP^.Pos;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenAttribute

  @INFO:
  Returns information about the current token
 ------------------------------------------------------------------------------}
function TSynTxtSyn.GetTokenAttribute: TSynHighlighterAttributes;
var
  sTkP : TTk;   // Pos  Tk
begin
  mToken.Pos(posBgn);
  sTkP := mToken.This;

  case sTkP^.Tok of
    tkWord:      begin Result := attriWord;      end;
    tkNumber:    begin Result := attriNumber;    end;
    tkSpace:     begin Result := attriSpace;     end;
    tkTab:       begin Result := attriTab;       end;
    tkSymbol:    begin Result := attriSymbol;    end;
    tkEscape:    begin Result := attriEscape;    end;
    tkUnknown:   begin Result := attriUnknown;   end;
    else Result := nil;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: GetToken

  @INFO:
  The following functions are used by SynEdit for the management of keys,
  brackets, parentheses and quotes. They are not crucial for token coloring,
  but they must respond well.
 ------------------------------------------------------------------------------}
function TSynTxtSyn.GetToken: String;
var
  sTkP : TTk;   // Pos  Tk
begin
  mToken.Pos(posBgn);
  sTkP := mToken.This;

  SetString(Result, (mLine + sTkP^.Pos), sTkP^.Len);
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenPos
 ------------------------------------------------------------------------------}
function TSynTxtSyn.GetTokenPos: Integer;
var
  sTkP : TTk;   // Pos  Tk
begin
  mToken.Pos(posBgn);
  sTkP := mToken.This;
  Result := sTkP^.Pos;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenKind
 ------------------------------------------------------------------------------}
function TSynTxtSyn.GetTokenKind: integer;
var
  sTkP : TTk;   // Pos  Tk
begin
  mToken.Pos(posBgn);
  sTkP := mToken.This;
  Result := integer(sTkP^.Tok);
end;


{-------------------------------------------------------------------------------
  @NAME: GetRange
 ------------------------------------------------------------------------------}
function TSynTxtSyn.GetRange: Pointer;
var
  sTkP : TTk;   // Pos  Tk
begin
  mToken.Pos(posBgn);
  sTkP := mToken.This;
  Result := {%H-}Pointer(Integer(sTkP^.Tok));
end;


{-------------------------------------------------------------------------------
  @NAME: GetDefaultAttribute
 ------------------------------------------------------------------------------}
function TSynTxtSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  Result := attriUnknown;
end;


{-------------------------------------------------------------------------------
  @NAME: GetLanguageName
 ------------------------------------------------------------------------------}
class function TSynTxtSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPo;
end;


{-------------------------------------------------------------------------------
  @NAME: CreateHighlighterAttributes
 ------------------------------------------------------------------------------}
 function TSynTxtSyn.CreateHighlighterAttributes(AName:PString; Foreground,
                                                Background: TColor;
                                                FontStyles: TFontStyles) :
TSynHighlighterAttributes;
begin
  Result:=TSynHighlighterAttributes.Create(AName);
  if Foreground<>clNone then Result.Foreground:=ForeGround;
  if Background<>clNone then Result.Background:=Background;
  Result.Style:=FontStyles;
end;


{-------------------------------------------------------------------------------
  @NAME: GetSampleSource
 ------------------------------------------------------------------------------}
function TSynTxtSyn.GetSampleSource: String;
begin
  Result:='\documentclass[a4paper]{article}'+#13#10+
          '% LaMd sample source'+#13#10+
          '\begin{document}'+#13#10+
          'Here is a formula: $ (2x + 3)*5y $'+#13#10+
          '\end{document}';
end;



initialization

  RegisterPlaceableHighlighter(TSynTxtSyn);

end.
