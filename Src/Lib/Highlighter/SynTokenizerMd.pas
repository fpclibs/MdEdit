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
unit SynTokenizerMd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  fgl;

const

  rangeNames: Array[0..16] of String = (
    'rsUnknown',
    'rsText',
    'rsAstEm',
    'rsUcsEm',
    'rsAstBold',
    'rsUcsBold',
    'rsHead',
    'rsCode',
    'rsDblCode',
    'rsCodeBlock',
    'rsCodeSpace',
    'rsList',
    'rsNumList',
    'rsBulletList',
    'rsBlockQuote',
    'rsEmptyLine',
    'rsComment'
  );

type

  TRangeState = (
    rsUnknown,
    rsText,
    rsAstEm,      // asterisks = AstEm             // Italic
    rsUcsEm,      // underscores = UcsEm           // Italic
    rsAstBold,    // double asterisks = AstBold    // Bold
    rsUcsBold,    // double underscores = UcsBold  // Bold
    rsHead,
    rsCode,
    rsDblCode,
    rsCodeBlock,
    rsCodeSpace,
    rsList,
    rsNumList,
    rsBulletList,
    rsBlockQuote,
    rsEmptyLine,
    rsComment
  );

  TtkTokenKind = (
    tkWord,
    tkNumber,
    tkSpace,
    tkTab,
    tkSymbol,
    tkEscape,
    tkBlockquote,
    tkUnknown,
    tkLineStart,
    tkLineEnd
  );

  TSctToken = record
    Pos: Integer;
    Len: Integer;
    Tok: TtkTokenKind;
    Rng: TRangeState;
  end;

  TTk = ^TSctToken;

  TTokenList = specialize TFPGList<TTk>;


  { Class TSynTokenizerMd }

  TSynTokenizerMd = class

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
    procedure Tokenize(mLine :PChar);
    function TokenList: TTokenList;

  protected

end;



implementation


{ Class TSynTokenizerMd }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSynTokenizerMd.Create;
begin
  inherited Create;
  mTokenList := TTokenList.Create;
  mPos := 0;
end;


{-------------------------------------------------------------------------------
  @NAME: Destroy
 ------------------------------------------------------------------------------}
destructor TSynTokenizerMd.Destroy;
begin
  Clear;
  mTokenList.Free;

  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  @NAME: GetItem
 ------------------------------------------------------------------------------}
function  TSynTokenizerMd.GetItem(index: Integer): TTk;
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
procedure TSynTokenizerMd.SetItem(index: Integer; tk: TTk);
begin
  mTokenList.Items[index] := tk;
end;


{-------------------------------------------------------------------------------
  @NAME: Count
 ------------------------------------------------------------------------------}
function TSynTokenizerMd.Count: Integer;
begin
  Result := mTokenList.Count;
end;


{-------------------------------------------------------------------------------
  @NAME: Pos
 ------------------------------------------------------------------------------}
function TSynTokenizerMd.Pos: Integer;
begin
  Result := mPos;
end;


{-------------------------------------------------------------------------------
  @NAME: Pos
 ------------------------------------------------------------------------------}
function TSynTokenizerMd.Pos(index: Integer): Boolean;
begin
  Result := False;
  if Not ( (index >= 0) And (index < mTokenList.Count) ) then  exit;

  mPos   := index;
  Result := True;
end;


{-------------------------------------------------------------------------------
  @NAME: Clear
 ------------------------------------------------------------------------------}
procedure TSynTokenizerMd.Clear;
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
  @NAME: TSctToken
 ------------------------------------------------------------------------------}
function TSynTokenizerMd.Last: TTk;
begin
  if mPos > 0 then
    Result := mTokenList[mPos-1]
  else Result := mTokenList[mPos];
end;


{-------------------------------------------------------------------------------
  @NAME: TSctToken
 ------------------------------------------------------------------------------}
function TSynTokenizerMd.This: TTk;
begin
  Result := mTokenList[mPos];
end;


{-------------------------------------------------------------------------------
  @NAME: TSctToken
 ------------------------------------------------------------------------------}
function TSynTokenizerMd.Next: TTk;
begin
 if mPos+1 < mTokenList.Count then
   Result := mTokenList[mPos+1]
 else Result := mTokenList[mPos];
end;


{-------------------------------------------------------------------------------
  @NAME: Tokenize
 ------------------------------------------------------------------------------}
procedure TSynTokenizerMd.Tokenize(mLine :PChar);
var
  tkToken : TtkTokenKind;
  l       : Integer;
  sTk     : TTk;
  posEnd  : Integer;
  posBgn  : Integer;
begin
  Clear;

  New(sTk);
  sTk^.Tok := tkLineStart;
  sTk^.Rng := rsText;
  sTk^.Pos := 0;
  sTk^.Len := 0;
  mTokenList.Add(sTk);
  posEnd := 0;

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
    while (posEnd <= l) and not(mLine[posEnd] in [#9, '\', ' ', #0]) do
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
    sTk^.Rng := rsText;
    sTk^.Pos := posBgn;
    sTk^.Len := posEnd-posBgn;
    mTokenList.Add(sTk);
  end;

  New(sTk);
  sTk^.Tok := tkLineEnd;
  sTk^.Rng := rsText;
  sTk^.Pos := l;
  sTk^.Len := 0;

  mTokenList.Add(sTk);
  posEnd := 0;
  posBgn := 0;
end;


{-------------------------------------------------------------------------------
  @NAME: TokenList
 ------------------------------------------------------------------------------}
function TSynTokenizerMd.TokenList: TTokenList;
begin
  Result := mTokenList;
end;



end.

