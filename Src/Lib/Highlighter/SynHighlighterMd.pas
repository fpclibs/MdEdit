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
unit SynHighlighterMd;

interface

uses
  Classes, SysUtils, Graphics, SynEditHighlighter, fgl, SynTokenizerMd,
  FormDebug;

type

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

  { Class for creating a highlighter }

  TSynMdSyn = class(TSynCustomHighlighter)

  protected
    posBgn, posEnd: Integer;
    linAct: String;

    mToken: TSynTokenizerMd;
    mRangeList:  TRangeList;
    mRangeItems: TItemList;
    mLine: PChar;
    mLineNumber: Integer;
    mTokenPos: Integer;
    mRange:    TRangeState;
    mRangeOld: TRangeState;
    mRangeEnd: Integer;
    mTabCount: Integer;
    mFlagEmptyLine: Boolean;

    mDbgLineBreak: Integer;

    attriWordHead:     TSynHighlighterAttributes;
    attriSymbolHead:   TSynHighlighterAttributes;
    attriHeadEm:       TSynHighlighterAttributes;
    attriWord:         TSynHighlighterAttributes;
    attriWordEm:       TSynHighlighterAttributes;
    attriWordBold:     TSynHighlighterAttributes;
    attriWordEmBold:   TSynHighlighterAttributes;
    attriSymbolEm:     TSynHighlighterAttributes;
    attriSymbolBold:   TSynHighlighterAttributes;
    attriSymbolEmBold: TSynHighlighterAttributes;
    attriNumber:       TSynHighlighterAttributes;
    attriSpace:        TSynHighlighterAttributes;
    attriTab:          TSynHighlighterAttributes;
    attriSymbol:       TSynHighlighterAttributes;
    attriEscape:       TSynHighlighterAttributes;
    attriUnknown:      TSynHighlighterAttributes;
    attriComment:      TSynHighlighterAttributes;
    attriString:       TSynHighlighterAttributes;
    attriLink:         TSynHighlighterAttributes;
    attriLinkText:     TSynHighlighterAttributes;
    attriCode:         TSynHighlighterAttributes;
    attriCodeBlock:    TSynHighlighterAttributes;

    attriCodeBlockSymbol: TSynHighlighterAttributes;
    attriCodeBlockNumber: TSynHighlighterAttributes;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function  GetEol: Boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
              override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function  CreateHighlighterAttributes(AName:PString; Foreground,
                                                     Background: TColor;
                                                     FontStyles: TFontStyles) :
              TSynHighlighterAttributes;
    function  GetToken: String; override;
    function  GetTokenPos: Integer; override;
    function  GetTokenKind: integer; override;

    function  GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    procedure ClearRangeList;

    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    class function GetLanguageName: string; override;

  protected
    function GetSampleSource : String; override;

    function RangeText(deep: Integer): Boolean;
    function RangeAstEm(deep: Integer): Boolean;
    function RangeUcsEm(deep: Integer): Boolean;
    function RangeAstBold(deep: Integer): Boolean;
    function RangeUcsBold(deep: Integer): Boolean;
    function RangeHead(deep: Integer): Boolean;
    function RangeCode(deep: Integer): Boolean;
    function RangeDblCode(deep: Integer): Boolean;
    function RangeCodeBlock(deep: Integer): Boolean;
    function RangeCodeSpace(deep: Integer): Boolean;
    function RangeList({%H-}deep: Integer): Boolean;
    function RangeBlock({%H-}deep: Integer): Boolean;
    function RangeNumList(deep: Integer): Boolean;
    function RangeBulletList(deep: Integer): Boolean;
    function RangeEmptyLine(deep: Integer): Boolean;

    function CheckAsterisksBegin: Boolean;
    function CheckUnderscoresBegin: Boolean;
    function CheckAstEmBegin: Boolean;
    function CheckAstEmEnd: Boolean;
    function CheckUcsEmBegin: Boolean;
    function CheckUcsEmEnd: Boolean;
    function CheckAstBoldBegin: Boolean;
    function CheckAstBoldEnd: Boolean;
    function CheckUcsBoldBegin: Boolean;
    function CheckUcsBoldEnd: Boolean;
    function CheckHeadBegin: Boolean;
    function CheckHeadEnd: Boolean;
    function CheckCodeBlockBegin: Boolean;
    function CheckCodeBlockEnd: Boolean;
    function CheckCodeSpaceBegin: Boolean;
    function CheckCodeSpaceEnd: Boolean;
    function CheckCodeBegin: Boolean;
    function CheckCodeEnd: Boolean;
    function CheckDblCodeEnd: Boolean;
    function CheckNumListBegin: Boolean;
    function CheckNumListEnd: Boolean;
    function CheckEmptyLineBegin: Boolean;
    function CheckEmptyLineEnd: Boolean;
    function CheckTabDeep( var p: Integer): Boolean;
    function CheckBulletListBegin: Boolean;
    function CheckBulletListEnd: Boolean;

    procedure DebugNotify(Sender: TObject);

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




{ Class TSynMdSyn }

{-------------------------------------------------------------------------------
  @NAME: Create

  @INFO:
  Class Constructor. Here you must initialize the attributes to use.
 ------------------------------------------------------------------------------}
constructor TSynMdSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OFormDebug.AddNotify(@DebugNotify);

  mToken := TSynTokenizerMd.Create;
  mRangeList := TRangeList.Create;
  mRangeItems := TItemList.Create;
  mTabCount   := 0;
  mFlagEmptyLine := False;

  mDbgLineBreak := -1;

  ClearRangeList;

  attriComment := TSynHighlighterAttributes.Create('Comment', 'Comment');
  attriComment.Style := [fsItalic];
  AddAttribute(attriComment);

  attriWordHead := TSynHighlighterAttributes.Create('WordHead', 'WordHead');
  attriWordHead.Foreground := TColor($FF8000);
  attriWordHead.Background := TColor($FFFFFF);
  attriWordHead.Style := [fsBold];
  AddAttribute(attriWordHead);

  attriSymbolHead := TSynHighlighterAttributes.Create('SymbolHead', 'SymbolHead');
  attriSymbolHead.Foreground := TColor($FF8000);
  attriSymbolHead.Background := TColor($FFFFFF);
  attriSymbolHead.Style := [fsBold];
  AddAttribute(attriSymbolHead);

  attriHeadEm := TSynHighlighterAttributes.Create('HeadEm', 'HeadEm');
  attriHeadEm.Foreground := TColor($FF8000);
  attriHeadEm.Background := TColor($FFFFFF);
  attriHeadEm.Style := [fsBold, fsItalic];
  AddAttribute(attriHeadEm);

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
  attriWord.Background := TColor($FFFFFF);
  attriWord.Style := [];
  AddAttribute(attriWord);

  attriWordEm := TSynHighlighterAttributes.Create('WordEm', 'WordEm');
  attriWordEm.Foreground := TColor($000000);
  attriWordEm.Background := TColor($FFFFFF);
  attriWordEm.Style := [fsItalic];
  AddAttribute(attriWordEm);

  attriWordBold := TSynHighlighterAttributes.Create('WordBold', 'WordBold');
  attriWordBold.Foreground := TColor($000000);
  attriWordBold.Background := TColor($FFFFFF);
  attriWordBold.Style := [fsBold];
  AddAttribute(attriWordBold);

  attriWordEmBold := TSynHighlighterAttributes.Create('WordEmBold', 'WordEmBold');
  attriWordEmBold.Foreground := TColor($000000);
  attriWordEmBold.Background := TColor($FFFFFF);
  attriWordEmBold.Style := [fsBold, fsItalic];
  AddAttribute(attriWordEmBold);

  attriSymbolEm := TSynHighlighterAttributes.Create('SymbolEm', 'SymbolEm');
  attriSymbolEm.Foreground := TColor($000000);
  attriSymbolEm.Background := TColor($FFFFFF);
  attriSymbolEm.Style := [fsItalic];
  AddAttribute(attriSymbolEm);

  attriSymbolBold := TSynHighlighterAttributes.Create('SymbolBold', 'SymbolBold');
  attriSymbolBold.Foreground := TColor($000000);
  attriSymbolBold.Background := TColor($FFFFFF);
  attriSymbolBold.Style := [fsBold];
  AddAttribute(attriSymbolBold);

  attriSymbolEmBold := TSynHighlighterAttributes.Create('SymbolEmBold', 'SymbolEmBold');
  attriSymbolEmBold.Foreground := TColor($000000);
  attriSymbolEmBold.Background := TColor($FFFFFF);
  attriSymbolEmBold.Style := [fsBold, fsItalic];
  AddAttribute(attriSymbolEmBold);

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

  attriLink := TSynHighlighterAttributes.Create('Link', 'Link');
  attriLink.Foreground := TColor($00FF00);
  attriLink.Background := TColor($FFFFFF);
  attriLink.Style := [fsBold];
  AddAttribute(attriLink);

  attriLinkText := TSynHighlighterAttributes.Create('LinkText', 'LinkText');
  attriLinkText.Foreground := TColor($00FF00);
  attriLinkText.Background := TColor($FFFFFF);
  attriLinkText.Style := [fsBold];
  AddAttribute(attriLinkText);

  attriCode := TSynHighlighterAttributes.Create('Code', 'Code');
  attriCode.Foreground := TColor($2244DD);
  attriCode.Background := TColor($FFFFFF);
  attriCode.Style := [];
  AddAttribute(attriCode);

  attriCodeBlock := TSynHighlighterAttributes.Create('CodeBlock', 'CodeBlock');
  attriCodeBlock.Foreground := TColor($AA0000);
  attriCodeBlock.Background := TColor($FFFFFF);
  attriCodeBlock.Style := [];
  AddAttribute(attriCodeBlock);

  attriCodeBlockSymbol := TSynHighlighterAttributes.Create('CodeBlockSymbol', 'CodeBlockSymbol');
  attriCodeBlockSymbol.Foreground := TColor($3355EE);
  attriCodeBlockSymbol.Background := TColor($FFFFFF);
  attriCodeBlockSymbol.Style := [];
  AddAttribute(attriCodeBlockSymbol)
  ;
  attriCodeBlockNumber := TSynHighlighterAttributes.Create('CodeBlockNumber', 'CodeBlockNumber');
  attriCodeBlockNumber.Foreground := TColor($FF8000);
  attriCodeBlockNumber.Background := TColor($FFFFFF);
  attriCodeBlockNumber.Style := [];
  AddAttribute(attriCodeBlockNumber);
end;


{-------------------------------------------------------------------------------
  @NAME: Destroy

  @INFO:
  Class Destructor.
 ------------------------------------------------------------------------------}
destructor TSynMDSyn.Destroy;
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
procedure TSynMdSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  mLine       := PChar(NewValue);
  mLineNumber := LineNumber;

  linAct := NewValue;   // copy the current line

  posBgn := 0;
  posEnd := 0;
  mTokenPos := 0;
  mRangeEnd := -1;
  mTabCount := 0;
  mFlagEmptyLine := False;

  mToken.Tokenize(mLine);
  posBgn := 0;
end;


{-------------------------------------------------------------------------------
  @NAME: Next

  @INFO:
  It is called by SynEdit, to access the next Token. And it is executed by
  each token of the current line. In this example you will always move a
  character.
 ------------------------------------------------------------------------------}
procedure TSynMdSyn.Next;
var
  i: Integer;
  s: String;
begin
  mTabCount := 0;
  Inc(posBgn);

  if mRangeItems.Count > 2 then
  begin

    s := '<Next> (Line:' + IntToStr(mLineNumber + 1) +'), (posBgn:' + posBgn.ToString() + '), (RL.Count:' + mRangeList.Count.ToString() + ')';
    OFormDebug.Append := s;

    s := 'RI OLD(';
    for i := 2 To mRangeItems.Count-1 do
    begin
      s := s + rangeNames[Integer(mRangeItems.Items[i])] + ', ';
      mRange := TRangeState(mRangeItems.Items[i]);
    end;
    s := s + ')';
    OFormDebug.Append := s;

    if (mLineNumber+1) = mDbgLineBreak then
    begin
      mLineNumber := mLineNumber;
    end;

    case TRangeState(mRangeItems.Items[2]) of

      rsText:      begin RangeText(2);      end;
      rsUnknown:   begin RangeText(2);      end;
    end;
  end
  else
  begin
    // UnKnown
  end;

  s := 'RI NEW(';
  for i := 2 To mRangeItems.Count-1 do
  begin
    s := s + rangeNames[Integer(mRangeItems.Items[i])] + ', ';
  end;
  s := s + ')';
  OFormDebug.Append := s;
  s := '</Next>';
  OFormDebug.Append := s;
end;


{-------------------------------------------------------------------------------
  @NAME: RangeText

  @INFO:
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeText(deep: Integer): Boolean;
var
  sTkL, sTkP, sTkN : TTk;
  p : Integer;
begin
  Result := true;

  // No CheckTextEnd

  // CheckRangeDeep
  if mRangeItems.Count > deep+1 then
  begin
    case TRangeState(mRangeItems.Items[deep+1]) of

      rsUnknown:    begin RangeText(deep+1);      end;
      rsEmptyLine:  begin if Not RangeEmptyLine(deep+1) then begin exit; end else mRangeEnd := -1; end;
      rsHead:       begin RangeHead(deep+1);       exit; end;
      rsAstEm:      begin RangeAstEm(deep+1);      exit; end;
      rsAstBold:    begin RangeAstBold(deep+1);    exit; end;
      rsUcsEm:      begin RangeUcsEm(deep+1);      exit; end;
      rsUcsBold:    begin RangeUcsBold(deep+1);    exit; end;
      rsDblCode:    begin RangeDblCode(deep+1);    exit; end;
      rsCode:       begin RangeCode(deep+1);       exit; end;
      rsCodeBlock:  begin RangeCodeBlock(deep+1);  exit; end;
      rsCodeSpace:  begin if Not RangeCodeSpace(deep+1)  then exit; end;
      rsNumList:    begin if Not RangeNumList(deep+1)    then exit; end;
      rsBulletList: begin if Not RangeBulletList(deep+1) then exit; end;
    end;
  end
  else
  begin
    // UnKnown
  end;
//    if mFlagSetLine then mFlagSetLine := False

  while mToken.Pos(posBgn) do
  begin
    sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

    mTokenPos := sTkP^.Pos;
    posEnd := sTkP^.Pos + sTkP^.Len;

    if Not (mRangeEnd = -1) then
    begin
      if posBgn <= mRangeEnd then
      begin
        sTkP^.Rng := mRangeOld;
        break;
      end
      else mRangeEnd := -1;
    end;

    if Not (mRange = rsText) then exit;

    mRange := rsUnknown;

    if (sTkL^.Tok = tkLineStart) And ( (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkTab) ) then
    begin
      p := 1;
      if CheckEmptyLineBegin() then begin sTkP^.Rng := mRange; break; end;
      if Not ( CheckTabDeep(p) ) then
            if CheckCodeSpaceBegin() then begin sTkP^.Rng := mRange; break; end;
    end;
    if ( sTkL^.Tok = tkNumber ) And ( sTkP^.Tok = tkSymbol ) And ( mLine[mTokenPos] = '.' ) then
    begin
      if CheckNumListBegin() then begin sTkP^.Rng := mRange; break; end;
    end;
    p := 1;
    if ( CheckTabDeep(p) ) then
    begin
      if ( sTkP^.Tok = tkSymbol ) And ( mLine[mTokenPos] = '~' ) then
      begin
        if CheckCodeBlockBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( sTkP^.Tok = tkSymbol ) And ( mLine[mTokenPos] = '#' ) then
      begin
        if CheckHeadBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( sTkP^.Tok = tkSymbol )   And
         ( ( mLine[mTokenPos] = ':' ) Or ( mLine[mTokenPos] = '*' ) Or ( mLine[mTokenPos] = '+' ) Or ( mLine[mTokenPos] = '-' ) ) And
         ( (sTkN^.Tok = tkSpace) Or (sTkN^.Tok = tkTab) ) then
      begin
        if CheckBulletListBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
    end;
    if ( sTkP^.Tok = tkSymbol ) then
    begin
      if ( mLine[mTokenPos] = '`' ) then
      begin
        if CheckCodeBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( mLine[mTokenPos] = '*' ) then
      begin
        if CheckAsterisksBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( mLine[mTokenPos] = '_' ) then
      begin
        if CheckUnderscoresBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
    end;
    if (sTkL^.Tok = tkLineStart) And ( sTkP^.Tok = tkSymbol ) And ( mLine[mTokenPos] = '>' ) then
    begin
      if CheckCodeSpaceBegin() then begin sTkP^.Rng := mRange; break; end;
    end;
    if (sTkL^.Tok = tkLineStart) And ( sTkP^.Tok = tkLineEnd ) then
    begin
      if CheckEmptyLineBegin() then begin sTkP^.Rng := mRange; break; end;
    end;
    if mRange = rsUnknown then mRange := rsText;
    sTkP^.Rng := mRange;
    break;
  end;
  if Not (TRangeState(mRangeItems.Items[deep]) = rsText)
      And (mRange = rsText) then
  begin
    mRangeItems.Add(Integer(mRange));
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: RangeAstEm
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeAstEm(deep: Integer): Boolean;
var
  sTkL, sTkP : TTk;   // Pos  Tk
begin
  Result := False;
  mLine  := mLine;

  if Not mToken.Pos(posBgn) then exit;
  sTkL := mToken.Last; sTkP := mToken.This;

  if Not (mRangeEnd = -1) then
  begin
    if posBgn <= (mRangeEnd+1) then
    begin
      sTkP^.Rng := mRange;
      exit;
    end
    else mRangeEnd := -1;
  end;

  // CheckRangeSectionEnd
  if (posBgn < 3) And (CheckEmptyLineBegin Or CheckBulletListBegin() Or CheckNumListBegin() ) then
  begin
    mRangeItems.RemoveToEnd(deep);
    sTkP^.Rng := rsAstBold;
    Result := True;
    exit;
  end;

  // CheckRangeDeep (Ende Bedingungen)
  if mRangeItems.Count > deep+1 then
  begin
    case TRangeState(mRangeItems.Items[deep+1]) of

      rsEmptyLine: begin RangeEmptyLine(deep+1);        end;
      rsCode:      begin RangeCode(deep+1);       exit; end;
      rsCodeSpace: begin RangeCodeSpace(deep+1);  exit; end;
      rsAstEm:
        begin
          if Not RangeAstEm(deep+1) then
          begin
            exit;
          end
          else
          begin
            mRangeEnd := -1;
            exit;
          end;
        end;
    end;
  end;

  if ( sTkL^.Tok = tkLineStart ) then
  begin
    if CheckEmptyLineBegin() then begin sTkP^.Rng := mRange; exit; end;
  end;

  repeat
  begin
    sTkP := mToken.This;;

    mRangeOld := mRange;
    sTkP^.Rng := mRange;
    mTokenPos := sTkP^.Pos;

    if CheckAstEmEnd() then
    begin
      mRangeItems.RemoveToEnd(deep);
      sTkP^.Rng := rsAstEm;
      Result := True;
     exit;
    end;
    mRange := mRangeOld;

    if ( sTkP^.Tok = tkSymbol ) then
    begin
      if ( mLine[mTokenPos] = '`' ) then
      begin
        if CheckCodeBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( mLine[mTokenPos] = '*' ) then
      begin
        if CheckAsterisksBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( mLine[mTokenPos] = '_' ) then
      begin
        if CheckUnderscoresBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
    end;
    posEnd := sTkP^.Pos + sTkP^.Len;
    break;
  end
  until Not mToken.Pos(posBgn);
end;


{-------------------------------------------------------------------------------
  @NAME: RangeUcsEm
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeUcsEm(deep: Integer): Boolean;
var
  sTkL, sTkP : TTk;   // Pos  Tk
begin
  Result := False;
  mLine  := mLine;

  if Not mToken.Pos(posBgn) then exit;
  sTkL := mToken.Last; sTkP := mToken.This;

  if Not (mRangeEnd = -1) then
  begin
    if posBgn <= (mRangeEnd+1) then
    begin
      sTkP^.Rng := mRange;
      exit;
    end
    else mRangeEnd := -1;
  end;

  // CheckRangeSectionEnd
  if (posBgn < 3) And (CheckEmptyLineBegin Or CheckBulletListBegin() Or CheckNumListBegin() ) then
  begin
    mRangeItems.RemoveToEnd(deep);
    sTkP^.Rng := rsAstBold;
    Result := True;
    exit;
  end;

  // CheckRangeDeep (Ende Bedingungen)
  if mRangeItems.Count > deep+1 then
  begin
    case TRangeState(mRangeItems.Items[deep+1]) of

      rsEmptyLine: begin RangeEmptyLine(deep+1);        end;
      rsDblCode:   begin RangeDblCode(deep+1);    exit; end;
      rsCode:      begin RangeCode(deep+1);       exit; end;
      rsCodeSpace: begin RangeCodeSpace(deep+1);  exit; end;
      rsUcsEm:
        begin
          if Not RangeUcsEm(deep+1) then
          begin
            exit;
          end
          else
          begin
            mRangeEnd := -1;
            exit;
          end;
        end;
    end;
  end;

  if ( sTkL^.Tok = tkLineStart ) then
  begin
    if CheckEmptyLineBegin() then begin sTkP^.Rng := mRange; exit; end;
  end;

  repeat
  begin
    sTkP := mToken.This;;

    mRangeOld := mRange;
    sTkP^.Rng := mRange;
    mTokenPos := sTkP^.Pos;

    if CheckUcsEmEnd() then
    begin
      mRangeItems.RemoveToEnd(deep);
      sTkP^.Rng := rsUcsEm;
      Result := True;
     exit;
    end;
    mRange := mRangeOld;

    if ( sTkP^.Tok = tkSymbol ) then
    begin
      if ( mLine[mTokenPos] = '`' ) then
      begin
        if CheckCodeBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( mLine[mTokenPos] = '*' ) then
      begin
        if CheckAsterisksBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( mLine[mTokenPos] = '_' ) then
      begin
        if CheckUnderscoresBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
    end;
    posEnd := sTkP^.Pos + sTkP^.Len;
    break;
  end
  until Not mToken.Pos(posBgn);
end;


{-------------------------------------------------------------------------------
  @NAME: RangeAstBold
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeAstBold(deep: Integer): Boolean;
var
  sTkL, sTkP : TTk;   // Pos  Tk
begin
  Result := False;
  mLine := mLine;

  if Not mToken.Pos(posBgn) then exit;
  sTkL := mToken.Last; sTkP := mToken.This;

  if Not (mRangeEnd = -1) then
  begin
    if posBgn <= (mRangeEnd+1) then
    begin
      sTkP^.Rng := mRange;
      exit;
    end
    else mRangeEnd := -1;
  end;

  // CheckRangeSectionEnd
  if (posBgn < 3) And (CheckEmptyLineBegin Or CheckBulletListBegin() Or CheckNumListBegin() ) then
  begin
    mRangeItems.RemoveToEnd(deep);
    sTkP^.Rng := rsAstBold;
    Result := True;
    exit;
  end;

  // CheckRangeDeep
  if mRangeItems.Count > deep+1 then
  begin
    case TRangeState(mRangeItems.Items[deep+1]) of

      rsEmptyLine: begin RangeEmptyLine(deep+1);  exit; end;
      rsDblCode:   begin RangeDblCode(deep+1);    exit; end;
      rsCode:      begin RangeCode(deep+1);       exit; end;
      rsCodeSpace: begin RangeCodeSpace(deep+1);  exit; end;
      rsAstEm:
        begin
          if Not RangeAstEm(deep+1) then
          begin
            exit;
          end
          else
          begin
            mRangeEnd := -1;
            exit;
          end;
        end;
    end;
  end;

  if ( sTkL^.Tok = tkLineStart ) then
  begin
    if CheckEmptyLineBegin() then begin sTkP^.Rng := mRange; end;
  end;

  repeat
  begin
    sTkP := mToken.This;

    mRangeOld := mRange;
    sTkP^.Rng := mRange;
    mTokenPos := sTkP^.Pos;

    if CheckAstBoldEnd() then
    begin
      mRangeItems.RemoveToEnd(deep);
      sTkP^.Rng := rsAstBold;
      Result := True;
    exit;
    end;
    mRange := mRangeOld;

    if ( sTkP^.Tok = tkSymbol ) then
    begin
      if ( mLine[mTokenPos] = '`' ) then
      begin
        if CheckCodeBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( mLine[mTokenPos] = '*' ) then
      begin
        if CheckAsterisksBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( mLine[mTokenPos] = '_' ) then
      begin
        if CheckUnderscoresBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
    end;
    posEnd := sTkP^.Pos + sTkP^.Len;
    break;
  end
  until Not mToken.Pos(posBgn);
end;


{-------------------------------------------------------------------------------
  @NAME: RangeUcsBold
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeUcsBold(deep: Integer): Boolean;
var
  sTkL, sTkP : TTk;   // Pos  Tk
begin
  Result := False;
  mLine  := mLine;

  if Not mToken.Pos(posBgn) then exit;
  sTkL := mToken.Last; sTkP := mToken.This;

  if Not (mRangeEnd = -1) then
  begin
    if posBgn <= (mRangeEnd+1) then
    begin
      sTkP^.Rng := mRange;
      exit;
    end
    else mRangeEnd := -1;
  end;

  // CheckRangeSectionEnd
  if (posBgn < 3) And (CheckEmptyLineBegin Or CheckBulletListBegin() Or CheckNumListBegin() ) then
  begin
    mRangeItems.RemoveToEnd(deep);
    sTkP^.Rng := rsAstBold;
    Result := True;
    exit;
  end;

    // CheckRangeDeep
  if mRangeItems.Count > deep+1 then
  begin
    case TRangeState(mRangeItems.Items[deep+1]) of

      rsEmptyLine: begin RangeEmptyLine(deep+1);  exit; end;
      rsDblCode:   begin RangeDblCode(deep+1);    exit; end;
      rsCode:      begin RangeCode(deep+1);       exit; end;
      rsCodeSpace: begin RangeCodeSpace(deep+1);  exit; end;
      rsUcsEm:
        begin
          if Not RangeUcsEm(deep+1) then
          begin
            exit;
          end
          else
          begin
            mRangeEnd := -1;
            exit;
          end;
        end;
    end;
  end;

  if ( sTkL^.Tok = tkLineStart ) then
  begin
    if CheckEmptyLineBegin() then begin sTkP^.Rng := mRange; end;
  end;

  repeat
  begin
    sTkP := mToken.This;;

    mRangeOld := mRange;
    sTkP^.Rng := mRange;
    mTokenPos := sTkP^.Pos;

    if CheckUcsBoldEnd() then
    begin
      mRangeItems.RemoveToEnd(deep);
      sTkP^.Rng := rsUcsBold;
      Result := True;
    exit;
    end;
    mRange := mRangeOld;

    if ( sTkP^.Tok = tkSymbol ) then
    begin
      if ( mLine[mTokenPos] = '`' ) then
      begin
        if CheckCodeBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( mLine[mTokenPos] = '*' ) then
      begin
        if CheckAsterisksBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
      if ( mLine[mTokenPos] = '_' ) then
      begin
        if CheckUnderscoresBegin() then begin sTkP^.Rng := mRange; break; end;
      end;
    end;
    posEnd := sTkP^.Pos + sTkP^.Len;
    break;
  end
  until Not mToken.Pos(posBgn);
end;





{-------------------------------------------------------------------------------
  @NAME: RangeHead
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeHead(deep: Integer): Boolean;
var
  sTkP : TTk;   // Pos  Tk
begin
  Result := false;

  while mToken.Pos(posBgn) do
  begin
    sTkP := mToken.This;

    sTkP^.Rng := mRange;
    mRangeOld := mRange;
    if CheckHeadEnd() then
    begin
      mRange := rsText;
      mRangeItems.RemoveToEnd(deep);
      Result := True;
    end;
    break;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: RangeCode
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeCode(deep: Integer): Boolean;
var
  sTkP : TTk;   // Pos  Tk
begin
  Result := false;

  while mToken.Pos(posBgn) do
  begin
    sTkP := mToken.This;

    sTkP^.Rng := mRange;
    mRangeOld := mRange;
    if CheckCodeEnd() then
    begin
      mRange := rsText;
      mRangeItems.RemoveToEnd(deep);
      Result := True;
    end;
    break;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: RangeDblCode
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeDblCode(deep: Integer): Boolean;
var
  sTkP : TTk;   // Pos  Tk
begin
  Result := false;

  while mToken.Pos(posBgn) do
  begin
    sTkP := mToken.This;

    sTkP^.Rng := mRange;
    mRangeOld := mRange;
    if CheckDblCodeEnd() then
    begin
      mRange := rsText;
      mRangeItems.RemoveToEnd(deep);
      Result := True;
    end;
    break;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: RangeCodeBlock
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeCodeBlock(deep: Integer): Boolean;
var
  sTkP : TTk;   // Pos  Tk
begin
  Result := false;

  while mToken.Pos(posBgn) do
  begin
    sTkP := mToken.This;

    sTkP^.Rng := mRange;
    mRangeOld := mRange;
    if CheckCodeBlockEnd() then
    begin
      mRange := rsText;
      mRangeItems.RemoveToEnd(deep);
      Result := True;
    end;
    break;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: RangeCodeSpace
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeCodeSpace(deep: Integer): Boolean;
var
  sTkP : TTk;   // Pos  Tk
begin
  Result := false;

  while mToken.Pos(posBgn) do
  begin
    sTkP := mToken.This;

    sTkP^.Rng := mRange;
    mRangeOld := mRange;
    if CheckCodeSpaceEnd() then
    begin
      mRange := rsText;
      mRangeItems.RemoveToEnd(deep);
      Result := True;
    end;
    sTkP^.Rng := mRange;
    break;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: RangeList
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeList(deep: Integer): Boolean;
begin
  Result := true;
end;


{-------------------------------------------------------------------------------
  @NAME: RangeNumList
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeNumList(deep: Integer): Boolean;
var
  sTkP : TTk;   // Pos  Tk
begin
  Result := False;
  inc(mTabCount);
  mLine := mLine;

  // CheckRangeDeep
  if mRangeItems.Count > deep+1 then
  begin
    case TRangeState(mRangeItems.Items[deep+1]) of

      rsText:      begin RangeText(deep+1);      end;
      rsEmptyLine: begin if Not RangeEmptyLine(deep+1) then exit; end;
      rsHead:      begin RangeHead(deep+1);      exit; end;
      rsDblCode:   begin RangeDblCode(deep+1);   exit; end;
      rsCode:      begin RangeCode(deep+1);      exit; end;
      rsCodeBlock: begin RangeCodeBlock(deep+1); exit; end;
      rsCodeSpace: begin if Not RangeCodeSpace(deep+1) then exit; end;
    end;
  end;
  while mToken.Pos(posBgn) do
  begin
    sTkP := mToken.This;

    mRangeOld := sTkP^.Rng;
    if CheckNumListEnd() then
    begin
      mRange := rsText;
      mRangeItems.RemoveToEnd(deep);
      sTkP^.Rng := mRange;
      Result := True;
      dec(mTabCount);
      mRangeEnd := -1;
     exit;
    end;
    break;
  end;
  if Not (mRangeItems.Count > deep+1) then
  begin
    mRange := rsText;
    RangeText(deep);
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: RangeBulletList
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeBulletList(deep: Integer): Boolean;
var
  sTkP : TTk;   // Pos  Tk
begin
  Result := False;
  inc(mTabCount);
  mLine := mLine;

  // CheckRangeDeep
  if mRangeItems.Count > deep+1 then
  begin
    case TRangeState(mRangeItems.Items[deep+1]) of

      rsText:      begin RangeText(deep+1);      end;
      rsEmptyLine: begin if Not RangeEmptyLine(deep+1) then exit; end;
      rsHead:      begin RangeHead(deep+1);      exit; end;
      rsDblCode:   begin RangeDblCode(deep+1);   exit; end;
      rsCode:      begin RangeCode(deep+1);      exit; end;
      rsCodeBlock: begin RangeCodeBlock(deep+1); exit; end;
      rsCodeSpace: begin if Not RangeCodeSpace(deep+1) then exit; end;
    end;
  end;
  while mToken.Pos(posBgn) do
  begin
    sTkP := mToken.This;;

    mRangeOld := sTkP^.Rng;
    if CheckBulletListEnd() then
    begin
      mRange := rsText;
      mRangeItems.RemoveToEnd(deep);
      sTkP^.Rng := mRange;
      Result := True;
      dec(mTabCount);
      mRangeEnd := -1;
     exit;
    end;
    break;
  end;
  if Not (mRangeItems.Count > deep+1) then
  begin
    mRange := rsText;
    RangeText(deep);
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: RangeBlock
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeBlock(deep: Integer): Boolean;
begin
  Result := true;

end;


{-------------------------------------------------------------------------------
  @NAME: RangeEmptyLine
 ------------------------------------------------------------------------------}
function TSynMdSyn.RangeEmptyLine(deep: Integer): Boolean;
var
  sTkP : TTk;   // Pos  Tk
begin
  Result := False;
  while mToken.Pos(posBgn) do
  begin
    sTkP := mToken.This;

    sTkP^.Rng := mRange;
    mRangeOld := mRange;
    if CheckEmptyLineEnd() then
    begin
      mRangeItems.RemoveToEnd(deep);
      Result := True;
    end;
    break;
  end;
end;





{-------------------------------------------------------------------------------
  @NAME: CheckAsterisksBegin

  @INFO:
  Stern Kursiv und Fett identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckAsterisksBegin: Boolean;
var
  sTkL : TTk;
  sTkP : TTk;
  {%H-}sTkN : TTk;
  p    : Integer;
  Len  : LongInt;
  FlagNoL, FlagNoR : Boolean;
begin
  Len := 0;
  Result := false;
  p := posBgn;
  FlagNoL := False; FlagNoR := False;

  if Not mToken.Pos(p) then exit;

  repeat
  begin
    sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

    if p = posBgn then
    begin
      if ( (sTkL^.Tok = tkLineStart) Or (sTkL^.Tok = tkTab)  Or (sTkL^.Tok = tkSpace) ) then
      begin
        FlagNoL := True;
      end;
    end;

    if Not ( ( sTkP^.Tok = tkSymbol ) And ( mLine[sTkP^.Pos] = '*' ) ) then
    begin
      break;
    end;
    Inc(Len);
    if Len > 1 then break;
    Inc(p);
  end
  until Not mToken.Pos(p);

  if (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkLineEnd) then
  begin
    FlagNoR := True;
  end;

  mRange := rsText;
  if Len = 0 then begin mRange := rsUnKnown; exit; end;
  if (Len = 1) And Not (FlagNoR And FlagNoL) then
  begin
    mRange := rsAstEm;
    mRangeItems.Add(Integer(mRange));
    mRangeEnd := p-1;
  end;
  if Len = 2 then
  begin
    if Not (FlagNoR And FlagNoL) then
    begin
      mRange := rsAstBold;
      mRangeItems.Add(Integer(mRange));
      mRangeEnd := p-1;
    end
    else
    begin
      mRange := rsAstEm;
      mRangeItems.Add(Integer(mRange));
      mRangeEnd := p-2;
    end;
  end;
  Result := true;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckUnderscoresBegin

  @INFO:
  Stern Kursiv identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckUnderscoresBegin: Boolean;
var
  sTkL : TTk;
  sTkP : TTk;
  {%H-}sTkN : TTk;
  p    : Integer;
  Len  : LongInt;
  FlagNoL, FlagNoR : Boolean;
begin
  Len := 0;
  Result := false;
  p := posBgn;
  FlagNoL := False; FlagNoR := False;

  if Not mToken.Pos(p) then exit;

  repeat
  begin
    sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

    if p = posBgn then
    begin
      if ( (sTkL^.Tok = tkLineStart) Or (sTkL^.Tok = tkTab)  Or (sTkL^.Tok = tkSpace) ) then
      begin
        FlagNoL := True;
      end;
    end;

    if Not ( ( sTkP^.Tok = tkSymbol ) And ( mLine[sTkP^.Pos] = '_' ) ) then
    begin
      break;
    end;
    Inc(Len);
    if Len > 1 then break;
    Inc(p);
  end
  until Not mToken.Pos(p);

  if (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkLineEnd) then
  begin
    FlagNoR := True;
  end;

  mRange := rsText;
  if Len = 0 then begin mRange := rsUnKnown; exit; end;
  if (Len = 1) And Not (FlagNoR And FlagNoL) then
  begin
    mRange := rsUcsEm;
    mRangeItems.Add(Integer(mRange));
    mRangeEnd := p-1;
  end;
  if Len = 2 then
  begin
    if Not (FlagNoR And FlagNoL) then
    begin
      mRange := rsUcsBold;
      mRangeItems.Add(Integer(mRange));
      mRangeEnd := p-1;
    end
    else
    begin
    mRange := rsUcsEm;
      mRangeItems.Add(Integer(mRange));
      mRangeEnd := p-2;
    end;
  end;
  Result := true;
End;


{-------------------------------------------------------------------------------
  @NAME: CheckAstEmBegin

  @INFO:
  Stern Kursiv identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckAstEmBegin: Boolean;
begin
  Result := false;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckAstEmEnd

  @INFO:
  Stern Kursiv Ende identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckAstEmEnd: Boolean;
var
  sTkL : TTk;
  sTkP : TTk;
  {%H-}sTkN : TTk;
  p    : Integer;
  Len  : LongInt;
  FlagNoL, FlagNoR : Boolean;
begin
  Len := 0;
  Result := false;
  p := posBgn;
  FlagNoL := False; FlagNoR := False;

  if Not mToken.Pos(p) then exit;

  sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

  // Nur für Test bis Zeilenende
  if ( (sTkL^.Tok = tkLineStart) And (mFlagEmptyLine) ) then
  begin
    Result := true;
    exit;
  end;

  repeat
  begin
    sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

    if p = posBgn then
    begin
      if ( (sTkL^.Tok = tkLineStart) Or (sTkL^.Tok = tkTab)  Or (sTkL^.Tok = tkSpace) ) then
      begin
        FlagNoL := True;
      end;
    end;

    if Not ( ( sTkP^.Tok = tkSymbol ) And ( mLine[sTkP^.Pos] = '*' ) ) then
    begin
      break;
    end;
    Inc(Len);
    if Len > 1 then break;
    Inc(p);
  end
  until Not mToken.Pos(p);

  if (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkLineEnd) then
  begin
    FlagNoR := True;
  end;

  mRange := rsText;
  if Len = 0 then begin mRange := rsUnKnown; exit; end;
  if (Len > 0) And Not (FlagNoR And FlagNoL) then
  begin
    mRangeEnd := posBgn;
    Result := true;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckUcsEmBegin

  @INFO:
  Unterstrich Kursiv identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckUcsEmBegin: Boolean;
begin
   Result := false;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckUcsEmEnd

  @INFO:
  Unterstrich Kursiv Ende identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckUcsEmEnd: Boolean;
var
  sTkL : TTk;
  sTkP : TTk;
  {%H-}sTkN : TTk;
  p    : Integer;
  Len  : LongInt;
  FlagNoL, FlagNoR : Boolean;
begin
  Len := 0;
  Result := false;
  p := posBgn;
  FlagNoL := False; FlagNoR := False;

  if Not mToken.Pos(p) then exit;

  sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

  // Nur für Test bis Zeilenende
  if ( (sTkL^.Tok = tkLineStart) And (mFlagEmptyLine) ) then
  begin
    Result := true;
    exit;
  end;

  repeat
  begin
    sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

    if p = posBgn then
    begin
      if ( (sTkL^.Tok = tkLineStart) Or (sTkL^.Tok = tkTab)  Or (sTkL^.Tok = tkSpace) ) then
      begin
        FlagNoL := True;
      end;
    end;

    if Not ( ( sTkP^.Tok = tkSymbol ) And ( mLine[sTkP^.Pos] = '_' ) ) then
    begin
      break;
    end;
    Inc(Len);
    if Len > 1 then break;
    Inc(p);
  end
  until Not mToken.Pos(p);

  if (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkLineEnd) then
  begin
    FlagNoR := True;
  end;

  mRange := rsText;
  if Len = 0 then begin mRange := rsUnKnown; exit; end;
  if (Len > 0) And Not (FlagNoR And FlagNoL) then
  begin
    mRangeEnd := posBgn;
    Result := true;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckAstBoldBegin

  @INFO:
  Doppel-Stern Fett identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckAstBoldBegin: Boolean;
begin
  Result := false;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckAstBoldEnd

  @INFO:
  Doppel-Stern Fett Ende identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckAstBoldEnd: Boolean;
var
  sTkL : TTk;
  sTkP : TTk;
  {%H-}sTkN : TTk;
  p    : Integer;
  Len  : LongInt;
  FlagNoL, FlagNoR : Boolean;
begin
  Len := 0;
  Result := false;
  p := posBgn;
  FlagNoL := False; FlagNoR := False;

  if Not mToken.Pos(p) then exit;

  sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

  // Nur für Test bis Zeilenende
  if ( (sTkL^.Tok = tkLineStart) And (mFlagEmptyLine) ) then
  begin
    Result := true;
    exit;
  end;

  repeat
  begin
    sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

    if p = posBgn then
    begin
      if ( (sTkL^.Tok = tkLineStart) Or (sTkL^.Tok = tkTab)  Or (sTkL^.Tok = tkSpace) ) then
      begin
        FlagNoL := True;
      end;
    end;

    if Not ( ( sTkP^.Tok = tkSymbol ) And ( mLine[sTkP^.Pos] = '*' ) ) then
    begin
      break;
    end;
    Inc(Len);
    if Len > 1 then break;
    Inc(p);
  end
  until Not mToken.Pos(p);

  if (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkLineEnd) then
  begin
    FlagNoR := True;
  end;

  mRange := rsText;
  if Len = 0 then begin mRange := rsUnKnown; exit; end;
  if (Len > 1) And Not (FlagNoR And FlagNoL) then
  begin
    mRangeEnd := posBgn+1;
    Result := true;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckUcsBoldBegin

  @INFO:
  Doppel-Unterstrich Fett identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckUcsBoldBegin: Boolean;
begin
  Result := false;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckUcsBoldEnd

  @INFO:
  Doppel-Unterstrich Fett Ende identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckUcsBoldEnd: Boolean;
var
  sTkL : TTk;
  sTkP : TTk;
  {%H-}sTkN : TTk;
  p    : Integer;
  Len  : LongInt;
  FlagNoL, FlagNoR : Boolean;
begin
  Len := 0;
  Result := false;
  p := posBgn;
  FlagNoL := False; FlagNoR := False;

  if Not mToken.Pos(p) then exit;

  sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

  // Nur für Test bis Zeilenende
  if ( (sTkL^.Tok = tkLineStart) And (mFlagEmptyLine) ) then
  begin
    Result := true;
    exit;
  end;

  repeat
  begin
    sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

    if p = posBgn then
    begin
      if ( (sTkL^.Tok = tkLineStart) Or (sTkL^.Tok = tkTab)  Or (sTkL^.Tok = tkSpace) ) then
      begin
        FlagNoL := True;
      end;
    end;

    if Not ( ( sTkP^.Tok = tkSymbol ) And ( mLine[sTkP^.Pos] = '_' ) ) then
    begin
      break;
    end;
    Inc(Len);
    if Len > 1 then break;
    Inc(p);
  end
  until Not mToken.Pos(p);

  if (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkLineEnd) then
  begin
    FlagNoR := True;
  end;

  mRange := rsText;
  if Len = 0 then begin mRange := rsUnKnown; exit; end;
  if (Len > 1) And Not (FlagNoR And FlagNoL) then
  begin
    mRangeEnd := posBgn+1;
    Result := true;
  end;
end;





{-------------------------------------------------------------------------------
  @NAME: CheckHeadBegin

  @INFO:
  Ueberschrift identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckHeadBegin: Boolean;
var
  sTkP : TTk;   // Pos  Tk
  p    : Integer;
  Len  : LongInt;
begin
  Len := 0;
  Result := false;
  p := posBgn;

  while mToken.Pos(p) do
  begin
    sTkP := mToken.This;

    if Not ( ( sTkP^.Tok = tkSymbol ) And ( mLine[sTkP^.Pos] = '#' ) ) then
    begin
      break;
    end;
    Inc(Len);
    Inc(p);
  end;
  if Not ( sTkP^.Tok = tkSpace ) then
  begin
    exit;
  end;

  if Len = 0 then begin mRange := rsUnKnown; exit; end;
  if Len = 1 then mRange := rsHead;
  if Len = 2 then mRange := rsHead;
  if Len = 3 then mRange := rsHead;
  if Len = 4 then mRange := rsHead;
  if Len = 5 then mRange := rsHead;
  if Len > 5 then mRange := rsHead;

  mRangeItems.Add(Integer(mRange));
  Result := true;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckHeadEnd

  @INFO:
  Ueberschrift identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckHeadEnd: Boolean;
var
  sTkN : TTk;   // Next Tk
  p    : Integer;
begin
  Result := false;
  p := posBgn;
  if mToken.Pos(p) then
  begin
    sTkN := mToken.Next;

    if ( sTkN^.Tok = tkLineEnd ) then
    begin
      Result := true;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckCodeBlockBegin

  @INFO:
  CodeBlock identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckCodeBlockBegin: Boolean;
var
  sTkP : TTk;   // Pos  Tk
  p    : Integer;
  Len  : LongInt;
begin
  Len := 0;
  Result := false;
  p := mLineNumber;
  p := posBgn;

  while mToken.Pos(p) do
  begin
    sTkP := mToken.This;

    if Not ( ( sTkP^.Tok = tkSymbol ) And ( mLine[sTkP^.Pos] = '~' ) ) then
    begin
      break;
    end;
    inc(Len);
    Inc(p);
  end;

  if Len = 0 then begin mRange := rsUnKnown; exit; end;
  if Len > 2 then
  begin
    mRange := rsCodeBlock;
    mRangeItems.Add(Integer(mRange));
    Result := true;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckCodeBlockEnd

  @INFO:
  CodeBlock identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckCodeBlockEnd: Boolean;
var
  sTkP : TTk;   // Pos  Tk
  p    : Integer;
  Len  : LongInt;
begin
  Len := 0;
  Result := false;
  p := mLineNumber;
  p := posBgn;

  if Not ( p = 1 ) then exit;

  if Not CheckTabDeep(p) then exit;

  if ( p = 0 ) then p := 1;
  while mToken.Pos(p) do
  begin
    sTkP := mToken.This;

    if  (sTkP^.Tok = tkTab) or (sTkP^.Tok = tkSpace) then
    begin
      break;
    end;
    if Not ( mLine[sTkP^.Pos] = '~' ) then
    begin
      break;
    end;
    inc(Len);
    Inc(p);
  end;

  if Len > 2 then begin

    mRange := rsText;
    Result := true;
    mRangeEnd := p - 1;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckCodeSpaceBegin

  @INFO:
  CodeBlock identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckCodeSpaceBegin: Boolean;
var
  sTkL, sTkP : TTk;
  p    : Integer;
  Len  : LongInt;
begin
  Len := 0;
  Result := false;
  p := mLineNumber;
  p := posBgn;

  if Not ( p = 1 ) then exit;

  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last; sTkP := mToken.This;

    if ( p = 1 ) and ( Not ( sTkL^.Tok = tkLineStart ) or
       Not ( ( sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkTab) Or ( (sTkP^.Tok = tkSymbol) And (mLine[sTkP^.Pos] = '>') ) ) ) then
    begin
      exit;
    end;
    if Not ( (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkTab) Or ( (sTkP^.Tok = tkSymbol) And (mLine[sTkP^.Pos] = '>') ) ) then
    begin
      break;
    end;
    if (sTkP^.Tok = tkTab  ) then Len := Len + 4;
    if (sTkP^.Tok = tkSpace) then Len := Len + sTkP^.Len;

    if (sTkP^.Tok = tkSymbol) And (mLine[sTkP^.Pos] = '>') then
    begin
      if Len > 4 then
      begin
        Len := Len;
      end;
      Len := 0;
    end;
    Inc(p);
  end;

  if Len = 0 then begin mRange := rsUnKnown; exit; end;
  if Len > 3 then
  begin
    mRange := rsCodeSpace;
    mRangeItems.Add(Integer(mRange));
    Result := true;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckCodeSpaceEnd

  @INFO:
  CodeBlock identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckCodeSpaceEnd: Boolean;
var
  sTkL, sTkP : TTk;
  p    : Integer;
  tab  : Integer;
begin
  Result := false;
  p := mLineNumber;
  p := posBgn;
  tab := 0;

  if Not ( p = 1 ) then exit;

  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last; sTkP := mToken.This;

    if Not ( (sTkP^.Tok = tkSpace) Or (sTkP^.Tok = tkTab) Or ( (sTkP^.Tok = tkSymbol) And (mLine[sTkP^.Pos] = '>') ) ) then
    begin
      break;
    end;
    Inc(p);
    if sTkL^.Tok = tkTab then inc(tab);
  end;

  if tab > mTabCount then exit;

  mRange := rsText;
  Result := true;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckCodeBegin

  @INFO:
  Code identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckCodeBegin: Boolean;
var
  sTkL, sTkP: TTk;
  p    : Integer;
  Len  : LongInt;
begin
  Len := 0;
  Result := false;
  p := mLineNumber;
  p := posBgn;

  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last; sTkP := mToken.This;

    if Not ( ( sTkL^.Tok = tkLineStart ) or ( sTkL^.Tok = tkSpace ) or ( sTkL^.Tok = tkTab )  or ( sTkL^.Tok = tkSymbol ) ) or
       Not ( sTkP^.Tok = tkSymbol ) then
    begin
      break;
    end;
    if Not ( mLine[sTkP^.Pos] = '`' ) then break;
    Len := Len + sTkP^.Len;
    Inc(p);
    if Len > 1 then break;
  end;

  if Len = 0 then begin exit; end;
  if Len = 1 then
  begin
    mRange := rsCode;
    mRangeItems.Add(Integer(mRange));
    Result := true;
    exit;
  end;
  if Len = 2 then
  begin
    mRangeEnd := p-1;
    mRange := rsDblCode;
    mRangeItems.Add(Integer(mRange));
    Result := true;
    exit;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckCodeEnd

  @INFO:
  Code identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckCodeEnd: Boolean;
var
  sTkP : TTk;   // Pos  Tk
  sTkN : TTk;   // Next Tk
  p    : Integer;
begin
  Result := false;
  p := mLineNumber;
  p := posBgn;

  while mToken.Pos(p) do
  begin
    sTkP := mToken.This; sTkN := mToken.Next;

    if Not ( ( sTkN^.Tok = tkLineEnd ) or ( sTkN^.Tok = tkSpace ) or ( sTkN^.Tok = tkTab )  or ( sTkN^.Tok = tkSymbol ) ) or
       Not ( sTkP^.Tok = tkSymbol ) then
    begin
      exit;
    end;
    if Not ( mLine[sTkP^.Pos] = '`' ) then
    begin
      break;
    end
    else
    begin
      mRangeEnd := p - 1;
      mRange := rsText;
      Result := true;
      break;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckDblCodeEnd

  @INFO:
  Code identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckDblCodeEnd: Boolean;
var
  sTkP : TTk;   // Pos  Tk
  sTkN : TTk;   // Next Tk
  p    : Integer;
begin
  Result := false;
  p := mLineNumber;
  p := posBgn;

  while mToken.Pos(p) do
  begin
    sTkP := mToken.This; sTkN := mToken.Next;

    if Not ( ( sTkN^.Tok = tkLineEnd ) or ( sTkN^.Tok = tkSpace ) or ( sTkN^.Tok = tkTab )  or ( sTkN^.Tok = tkSymbol ) ) or
       Not ( ( sTkP^.Tok = tkSymbol ) And ( sTkP^.Tok = tkSymbol ) ) then
    begin
      exit;
    end;
    if Not ( mLine[sTkP^.Pos] = '`' ) then
    begin
      break;
    end;
    if Not ( mLine[sTkN^.Pos] = '`' ) then
    begin
      break;
    end;
    mRangeEnd := p+1;
    mRange := rsText;
    Result := true;
    break;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckNumListBegin

  @INFO:
  Nummerierte Listen identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckNumListBegin: Boolean;
var
  sTkL,  sTkP, sTkN : TTk;
  p    : Integer;
  tab  : Integer;
begin
 Result := false;
  p := mLineNumber;
  p := 1;
  tab := 0;

  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

    if Not ( ( sTkL^.Tok = tkLineStart )
          or ( sTkL^.Tok = tkSpace )
          or ( sTkL^.Tok = tkTab )
          or ( ( sTkL^.Tok = tkSymbol) and (mLine[sTkL^.Pos] = '>') ) ) then
    begin
      break;
    end;
    if sTkL^.Tok = tkTab then inc(tab);
    inc(p);
  end;
  if Not ( sTkL^.Tok = tkNumber ) then exit;

  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

    if Not ( ( sTkL^.Tok = tkNumber )
           And ( (sTkP^.Tok = tkSymbol) and (mLine[sTkP^.Pos] = '.') )
           And ( (sTkN^.Tok = tkSpace) Or  (sTkN^.Tok = tkTab) ) ) then
    begin
      break;
    end
    else
    begin
      mRange := rsNumList;
      mRangeItems.Add(Integer(mRange));
      Result := true;
      break;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckNumListEnd

  @INFO:
  Nummerierte Listen identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckNumListEnd: Boolean;
var
  sTkL, sTkN : TTk;
  p    : Integer;
  tab  : Integer;
begin
  Result := false;
  p := mLineNumber;
  p := posBgn;
  tab := 0;

  if (p > 1) then exit;

  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last; sTkN := mToken.Next;

    if Not ( ( sTkL^.Tok = tkLineStart )
          or ( sTkL^.Tok = tkSpace )
          or ( sTkL^.Tok = tkTab )
          or ( ( sTkL^.Tok = tkSymbol) and (mLine[sTkL^.Pos] = '>') ) ) then
    begin
      break;
    end;
    if sTkL^.Tok = tkTab then inc(tab);
    inc(p);
  end;

  if (mTabCount < 2) then
  begin
     if Not (tab = 0) Or (sTkL^.Tok = tkSpace) then exit;
  end
  else
  begin
     if (tab+1) >= mTabCount then exit;
  end;

  if (Not mFlagEmptyLine) then
  begin
    if CheckBulletListBegin() Or CheckNumListBegin() then
    begin
      mRangeEnd := p - 1;
      mRange := rsText;
      Result := true;
    end;
    exit;
  end;

  if CheckEmptyLineBegin() then
  begin
    exit;
  end;

  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last;  sTkN := mToken.Next;

    if ( ( sTkN^.Tok = tkLineEnd ) or ( sTkN^.Tok = tkSpace ) or ( sTkN^.Tok = tkTab ) ) then
    begin
      inc(p);
      Continue;
    end
    else
    begin
      mRangeEnd := p - 1;
      mRange := rsText;
      Result := true;
      break;
    end;
  end;

end;


{-------------------------------------------------------------------------------
  @NAME: CheckBulletListBegin

  @INFO:
  Nummerierte Listen identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckBulletListBegin: Boolean;
var
  sTkL, sTkP, sTkN : TTk;
  p    : Integer;
  tab  : Integer;
begin
 Result := false;
  p := mLineNumber;
  p := 1;
  tab := 0;

  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

    if Not ( ( sTkL^.Tok = tkLineStart )
          or ( sTkL^.Tok = tkSpace )
          or ( sTkL^.Tok = tkTab )
          or ( ( sTkL^.Tok = tkSymbol) and (mLine[sTkL^.Pos] = '>') ) ) then
    begin
      break;
    end;
    if sTkL^.Tok = tkTab then inc(tab);
    inc(p);
  end;
  if Not ( sTkL^.Tok = tkSymbol ) then exit;

  if p > 0 then dec(p);
  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last; sTkP := mToken.This; sTkN := mToken.Next;

    if (sTkL^.Tok = tkSymbol) Or
       Not ( ( (sTkP^.Tok = tkSymbol) And ( (mLine[sTkP^.Pos] = '*') Or (mLine[sTkP^.Pos] = '-') Or (mLine[sTkP^.Pos] = '+') )
       And ( (sTkN^.Tok = tkSpace) Or  (sTkN^.Tok = tkTab) ) ) ) then
    begin
      break;
    end
    else
    begin
      mRange := rsBulletList;
      mRangeItems.Add(Integer(mRange));
      Result := true;
      break;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckBulletListEnd

  @INFO:
  Nummerierte Listen identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckBulletListEnd: Boolean;
var
  sTkL, sTkN : TTk;
  p    : Integer;
  tab  : Integer;
begin
  Result := false;
  p := mLineNumber;
  p := posBgn;
  tab := 0;

  if (p > 1) then exit;

  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last; sTkN := mToken.Next;

    if Not ( ( sTkL^.Tok = tkLineStart )
          or ( sTkL^.Tok = tkSpace )
          or ( sTkL^.Tok = tkTab )
          or ( ( sTkL^.Tok = tkSymbol) and (mLine[sTkL^.Pos] = '>') ) ) then
    begin
      break;
    end;
    if sTkL^.Tok = tkTab then inc(tab);
    inc(p);
  end;

  if (mTabCount < 2) then
  begin
     if Not (tab = 0) Or (sTkL^.Tok = tkSpace) then exit;
  end
  else
  begin
     if (tab+1) >= mTabCount then exit;
  end;

  if (Not mFlagEmptyLine) then
  begin
    if CheckNumListBegin() OR CheckBulletListBegin() then
    begin
      mRangeEnd := p - 1;
      mRange := rsText;
      Result := true;
    end;
    exit;
  end;

  if CheckEmptyLineBegin() then
  begin
    exit;
  end;

  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last; sTkN := mToken.Next;

    if ( ( sTkN^.Tok = tkSpace ) or ( sTkN^.Tok = tkTab ) ) then
    begin
      inc(p);
      Continue;
    end
    else
    begin
      mRangeEnd := p - 1;
      mRange := rsText;
      Result := true;
      break;
    end;
  end;

end;


{-------------------------------------------------------------------------------
  @NAME: CheckEmptyLineBegin

  @INFO:
  Code identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckEmptyLineBegin: Boolean;
var
  sTkP : TTk;   // Pos  Tk
  p    : Integer;
begin
  Result := false;
  p := mLineNumber;
  p := 1;

  while mToken.Pos(p) do
  begin
    sTkP := mToken.This;

    if Not ( ( sTkP^.Tok = tkLineEnd ) or ( sTkP^.Tok = tkSpace ) or ( sTkP^.Tok = tkTab )  ) then
    begin
      exit;
    end;
    inc(p);
  end;
  mRange := rsEmptyLine;
  mRangeItems.Add(Integer(mRange));
  Result := true;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckEmptyLineEnd

  @INFO:
  Code identifizieren
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckEmptyLineEnd: Boolean;
var
  sTkP : TTk;   // Pos  Tk
  p    : Integer;
begin
  Result := false;
  p := mLineNumber;
  p := posBgn;

  if posBgn > 1 then exit;
  while mToken.Pos(p) do
  begin
    sTkP := mToken.This;

    if Not ( ( sTkP^.Tok = tkLineEnd ) or ( sTkP^.Tok = tkSpace ) or ( sTkP^.Tok = tkTab ) ) then
    begin
      mRange := rsText;
      if mRangeItems.Count-2 > -1 then
      begin
        mRange := TRangeState(mRangeItems.Items[mRangeItems.Count-2]);
        sTkP^.Rng := mRange;
      end;
      mRangeEnd := p - 1;
      Result := true;
      mFlagEmptyLine := True;
      break;
    end;
    inc(p);
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckTabDeep

  @INFO:
  Nummerierte Listen CheckTabDeep
 ------------------------------------------------------------------------------}
function TSynMdSyn.CheckTabDeep( var p: Integer): Boolean;
var
  sTkL : TTk;   // Last Tk
  tab  : Integer;
begin
  Result := false;
  tab := 0;

  if p <> 1 then exit;

  while mToken.Pos(p) do
  begin
    sTkL := mToken.Last;

    if Not ( ( sTkL^.Tok = tkLineStart )
          or ( sTkL^.Tok = tkSpace )
          or ( sTkL^.Tok = tkTab )
          or ( ( sTkL^.Tok = tkSymbol) and (mLine[sTkL^.Pos] = '>') ) ) then
    begin
      break;
    end;
    if sTkL^.Tok = tkTab then inc(tab);
    inc(p);
  end;
  dec(p);
  if tab <= mTabCount then Result := true;
end;



{-------------------------------------------------------------------------------
  @NAME: GetEol

  @INFO:
  Indicates when the end of the line has been reached
 ------------------------------------------------------------------------------}
function TSynMdSyn.GetEol: Boolean;
var
  sTkP : TTk;   // Pos  Tk
begin
  Result := True;
  if mToken.Pos(posBgn) then
  begin
    sTkP := mToken.This;
    Result := False;
    if sTkP^.Tok = tkLineEnd then
    begin
      Result := true;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenEx

  @INFO:
  Devuelve información sobre el token actual
 ------------------------------------------------------------------------------}
procedure TSynMdSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
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
function TSynMdSyn.GetTokenAttribute: TSynHighlighterAttributes;
var
  sTkP : TTk;   // Pos  Tk
  flagItalic : Boolean;
  flagBold   : Boolean;
  flagHead   : Boolean;
  i : Integer;
  range: TRangeState;
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

  //  Comment

  case sTkP^.Rng of
    rsComment:   begin Result := attriComment;   end;
    rsHead:      begin Result := attriWordHead;  end;
    rsDblCode:   begin Result := attriCode;      end;
    rsCode:      begin Result := attriCode;      end;
    rsCodeBlock: begin Result := attriCodeBlock; end;
    rsCodeSpace: begin Result := attriCodeBlock; end;
  end;

  if Result = attriCodeBlock then
  begin
    case sTkP^.Tok of
      tkNumber:    begin Result := attriCodeBlockNumber;    end;
      tkSymbol:    begin Result := attriCodeBlockSymbol;    end;
      else Result := attriCodeBlock;
    end;
  end;

  if Result = attriWordHead then
  begin
    case sTkP^.Tok of
      tkWord: begin end;
      else Result := attriSymbolHead;
    end;
  end;
  //  EmBold

  flagItalic := False; flagBold := False; flagHead := False;
  case sTkP^.Rng of
    rsAstEm:   begin flagItalic := True; end;
    rsUcsEm:   begin flagItalic := True; end;
    rsAstBold: begin flagBold   := True; end;
    rsUcsBold: begin flagBold   := True; end;
  end;

  if Not( flagItalic Or flagBold ) then begin exit; end;

  for i:=0 to mRangeItems.Count-1 do
  begin
    range := TRangeState(mRangeItems.Items[i]);
    case range of
      rsAstEm:   begin flagItalic := True; end;
      rsUcsEm:   begin flagItalic := True; end;
      rsAstBold: begin flagBold   := True; end;
      rsUcsBold: begin flagBold   := True; end;
      rsHead:    begin flagHead   := True; end;
    end;
  end;

  while (true) do
  begin
    if flagHead And flagItalic then begin Result := attriHeadEm; break; end;
    if flagHead And flagBold   then begin Result := attriWordHead;   break; end;
    if Not flagHead And flagItalic And flagBold then begin Result := attriWordEmBold; break; end;
    if Not flagHead And flagItalic then begin Result := attriWordEm; break; end;
    if Not flagHead And flagBold then begin Result := attriWordBold; break; end;
    break;
  end;

  if Result = attriWordEm then
  begin
    case sTkP^.Tok of
      tkWord: begin end;
      else Result := attriSymbolEm;
    end;
  end;
  if Result = attriWordBold then
  begin
    case sTkP^.Tok of
      tkWord: begin end;
      else Result := attriSymbolBold;
    end;
  end;
  if Result = attriWordEmBold then
  begin
    case sTkP^.Tok of
      tkWord: begin end;
      else Result := attriSymbolEmBold;
    end;
  end;

  if Result = attriWordHead then
  begin
    case sTkP^.Tok of
      tkWord: begin end;
      else Result := attriSymbolHead;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: GetToken

  @INFO:
  The following functions are used by SynEdit for the management of keys,
  brackets, parentheses and quotes. They are not crucial for token coloring,
  but they must respond well.
 ------------------------------------------------------------------------------}
function TSynMdSyn.GetToken: String;
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
function TSynMdSyn.GetTokenPos: Integer;
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
function TSynMdSyn.GetTokenKind: integer;
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
function TSynMdSyn.GetRange: pointer;
var
  i, j: Integer;
  items: TItemList;
  found: Boolean;
  index: Integer;
  str: String;
begin
  index := -1;

  for i:=0 to mRangeList.Count-1 do
  begin
    found := false;
    items := mRangeList.Items[i];
    if items.Count = mRangeItems.Count then
    begin
      found := true;
      for j:=0 to items.Count-1 do
      begin
        if Not (items.Items[j] = mRangeItems.Items[j]) then
        begin
          found := false;
          break;
        end;
      end;
    end;
    if found then
    begin
      index := i;
      break;
    end;
  end;

  str := ' Save (Line:' + IntToStr(mLineNumber + 1) + '):';
  for j:=0 to mRangeItems.Count-1 do
  begin
    if j > 2 then str := str + ', ' + rangeNames[mRangeItems.items[j]]
    else if j > 1 then str := str + ' ' + rangeNames[mRangeItems.items[j]];
  end;
  str := str + ' [' + index.ToString() + ']';

  if (index = -1) then
  begin
    index       := mRangeList.Add(mRangeItems);
    mRangeItems := mRangeItems.Copy;
    str := str + ' [' + index.ToString() + ']';
  end;
  OFormDebug.Append:=str;

  Result := {%H-}Pointer(index);
end;


{-------------------------------------------------------------------------------
  @NAME: SetRange
 ------------------------------------------------------------------------------}
procedure TSynMdSyn.SetRange(Value: Pointer);
var
  j: Integer;
  items: TItemList;
  index: Integer;
  str: String;
begin
  mRangeItems.Clear;

  index := {%H-}Integer(Value);

  if index < mRangeList.Count then
  begin
    items := mRangeList.Items[index];
  end
  else
  begin
    items := mRangeList.Items[0];
  end;
  mRangeItems.Clear;
   for j:=0 to items.Count-1 do
  begin
    mRangeItems.Add(items.items[j]);
  end;

  str := ' Load (LineOld:' + IntToStr(mLineNumber + 1) + '):';
  for j:=0 to mRangeItems.Count-1 do
  begin
    if j > 2 then str := str + ', ' + rangeNames[mRangeItems.items[j]]
    else if j > 1 then str := str + ' ' + rangeNames[mRangeItems.items[j]];
  end;
  str := str + ' [' + index.ToString() + ']';
  OFormDebug.Append:=str;

  mRange := TRangeState(mRangeItems.items[mRangeItems.Count - 1]);
end;


{-------------------------------------------------------------------------------
  @NAME: ResetRange
 ------------------------------------------------------------------------------}
procedure TSynMdSyn.ResetRange;
var
  str: String;
begin
  OFormDebug.Append:='TSynMdSyn.ResetRange';
  mRangeItems.Clear;
  mRangeItems.Add(0);
  mRangeItems.Add(0);
  mRangeItems.Add(Integer(rsText));

  str := ' Reset (Line:' + IntToStr(mLineNumber + 1) + ')';
  OFormDebug.Append:=str;

  mRange := rsText;
end;


{-------------------------------------------------------------------------------
  @NAME: ClearRangeList
 ------------------------------------------------------------------------------}
procedure TSynMdSyn.ClearRangeList;
begin
  OFormDebug.Append:='TSynMdSyn.ClearRangeList';

  mRangeList.Clear;
  mRangeItems.Clear;

  mRangeItems.Add(0);
  mRangeItems.Add(0);
  mRangeItems.Add(Integer(rsText));
  mRangeList.Add(mRangeItems);

  mRangeItems := TItemList.Create;

  mRangeItems.Add(0);
  mRangeItems.Add(0);
  mRangeItems.Add(Integer(rsText));

  mRange := rsText;
end;


{-------------------------------------------------------------------------------
  @NAME: GetLanguageName
 ------------------------------------------------------------------------------}
class function TSynMdSyn.GetLanguageName: string;
begin
  Result := 'Markdown' // SYNS_LangMarkdown;
end;


{-------------------------------------------------------------------------------
  @NAME: GetDefaultAttribute
 ------------------------------------------------------------------------------}
function TSynMdSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
{
  attriWordHead:     TSynHighlighterAttributes;
  attriSymbolHead:   TSynHighlighterAttributes;
  attriHeadEm:       TSynHighlighterAttributes;
  attriWord:         TSynHighlighterAttributes;
  attriWordEm:       TSynHighlighterAttributes;
  attriWordBold:     TSynHighlighterAttributes;
  attriWordEmBold:   TSynHighlighterAttributes;
  attriSymbolEm:     TSynHighlighterAttributes;
  attriSymbolBold:   TSynHighlighterAttributes;
  attriSymbolEmBold: TSynHighlighterAttributes;
  attriNumber:       TSynHighlighterAttributes;
  attriSpace:        TSynHighlighterAttributes;
  attriTab:          TSynHighlighterAttributes;
  attriSymbol:       TSynHighlighterAttributes;
  attriEscape:       TSynHighlighterAttributes;
  attriUnknown:      TSynHighlighterAttributes;
  attriComment:      TSynHighlighterAttributes;
  attriString:       TSynHighlighterAttributes;
  attriLink:         TSynHighlighterAttributes;
  attriLinkText:     TSynHighlighterAttributes;
  attriCode:         TSynHighlighterAttributes;
  attriCodeBlock:    TSynHighlighterAttributes;

  attriCodeBlockSymbol: TSynHighlighterAttributes;
  attriCodeBlockNumber: TSynHighlighterAttributes;
}

  case Index of
    SYN_ATTR_COMMENT:    Result := attriComment;
    SYN_ATTR_IDENTIFIER: Result := attriWord;
    SYN_ATTR_KEYWORD:    Result := attriString;
    SYN_ATTR_STRING:     Result := attriString;
    SYN_ATTR_WHITESPACE: Result := attriSpace;
    SYN_ATTR_SYMBOL:     Result := attriSymbol;
    SYN_ATTR_NUMBER:     Result := attriNumber;
    SYN_ATTR_DIRECTIVE:  Result := attriUnknown;
    SYN_ATTR_ASM:        Result := attriUnknown;
    SYN_ATTR_VARIABLE:   Result := attriUnknown;

    else Result := nil;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: CreateHighlighterAttributes
 ------------------------------------------------------------------------------}
 function TSynMdSyn.CreateHighlighterAttributes(AName:PString; Foreground,
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
function TSynMdSyn.GetSampleSource: String;
begin
  Result:='\documentclass[a4paper]{article}'+#13#10+
          '% LaMd sample source'+#13#10+
          '\begin{document}'+#13#10+
          'Here is a formula: $ (2x + 3)*5y $'+#13#10+
          '\end{document}';
end;


procedure TSynMdSyn.DebugNotify(Sender: TObject);
var
  ary: TStringArray;
begin
  ary := OFormDebug.Command.Split(':');

  if Length(ary) > 0 then
  begin
    if ary[0] = 'Line' then
    begin
      if Length(ary) > 1 then
      begin
        mDbgLineBreak := StrToInt(ary[1]);
        OFormDebug.Append:='TSynMdSyn Line:' + mDbgLineBreak.ToString() + ' OK!';
      end;
    end;
  end;
end;


initialization

  RegisterPlaceableHighlighter(TSynMdSyn);

end.
