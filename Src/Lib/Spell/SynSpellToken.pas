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
unit SynSpellToken;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynHighlighterTxt, SynEditHighlighter, FormDebug;

type

  { TList<T> }

  generic TList<T> = class

  public
    Items: array of T;
    procedure Add(Value: T);
    procedure Remove(Index: Integer);
    procedure RemoveToEnd(Index: Integer);
    function Count: Integer;
    function Copy: TList;
    procedure Clear;

  end;


type

  TSctTkSpell = record
    x:       Integer;
    y:       Integer;
    Len:     Integer;
    Att:     TSynHighlighterAttributes;
    Checked: Boolean;
    Passed:  Boolean;
  end;

  TPTkSpell = ^TSctTkSpell;
  TTkSpell  = specialize TList<TPTkSpell>;

  TTkLines  = specialize TList<TTkSpell>;

  TAttList  = specialize TList<TSynHighlighterAttributes>;


type

  { TSynSpellToken }

  TSynSpellToken = class

  private
    mLineNumber: Integer;
    mSpll: TPTkSpell;
    mTkSpell: TTkSpell;
    mLnSpell: TTkLines;
    mFlagInvalidate: Boolean;
    mFlagDirty: Boolean;
    mFlagToSpell: Boolean;
    mFlagLineDirty: Boolean;
    mTokenCnt: Integer;
    mAttList: TAttList;

    mHighlighter: TSynCustomHighlighter;
    mSynTxtSynTxt: TSynTxtSyn;
    mAttributes: TStringList;
    mAttIndex: Integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure SetLine(const {%H-}NewValue: String; LineNumber: Integer);
    procedure Next;
    procedure Clear;

    Function IsSplited: Boolean;

    procedure SplitedNext;
    function SplitedToken: string;
    procedure SplitedTokenEx(out TokenStart: PChar; out TokenLength: integer);
    function SplitedTokenAttribute: TSynHighlighterAttributes;
    function SplitedTokenID: TtkTokenKind;
    function SplitedTokenKind: integer;
    function SplitedTokenPos: Integer;

    function  GetTokenPosByPos(p: TPoint): TPTkSpell;
    function  GetToken(p: TPoint): String;
    procedure SetTokenChecked(p: TPoint);
    procedure SetTokenPassed(p: TPoint; {%H-}flag: Boolean);
    procedure Attributes(list: Array of String);
    procedure Initialize;
    procedure ColorSchame;

    property  Invalidate: Boolean read mFlagInvalidate write mFlagInvalidate;
    property  Dirty: Boolean read mFlagDirty write mFlagDirty;
    property  Highlighter: TSynCustomHighlighter read mHighlighter write mHighlighter;

  protected
    procedure CheckToken;

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
    SetLength(Items, Length(Items)-1);
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



{ Class TSynSpellToken }

{-------------------------------------------------------------------------------
  @NAME: Create

  @INFO:
  Class Constructor. Here you must initialize the attributes to use.
 ------------------------------------------------------------------------------}
constructor TSynSpellToken.Create;
begin
  mSpll := New(TPTkSpell);
  mTkSpell := TTkSpell.Create;
  mLnSpell := TTkLines.Create;
  mFlagDirty := True;
  mFlagToSpell := False;
  mFlagLineDirty := False;
  mFlagInvalidate := False;
  mSpll^.Att := Nil;
  mSynTxtSynTxt := Nil;
  mAttributes := TStringList.Create;
  mAttributes.Clear;
  mAttributes.AddStrings([]);
  mAttList := TAttList.Create;
  mAttList.Clear;
  mAttIndex := -1;
end;


{-------------------------------------------------------------------------------
  @NAME: Destroy
 ------------------------------------------------------------------------------}
destructor TSynSpellToken.Destroy;
begin

end;


{-------------------------------------------------------------------------------
  @NAME: Attributes
 ------------------------------------------------------------------------------}
procedure TSynSpellToken.Attributes(list: Array of String);
begin
  mAttributes.Clear;
  mAttributes.AddStrings(list);
end;


{-------------------------------------------------------------------------------
  @NAME: Initialize
 ------------------------------------------------------------------------------}
procedure TSynSpellToken.Initialize;
var
  att, attq, attd: TSynHighlighterAttributes;
  z, i, j: Integer;
  s: String;
begin
  if (mHighlighter = Nil) then Exit;

  mSynTxtSynTxt := TSynTxtSyn.Create(mHighlighter);

  z := mSynTxtSynTxt.AttrCount;

  i := 0;
  mAttList.Clear;
  while i < mHighlighter.AttrCount do
  begin
    attq := mHighlighter.Attribute[i];
    Inc(i);
    s := attq.Name;
    s:=s;

    if mAttributes.IndexOf(attq.Name) < 0 then continue;

    for j := 0 to z-1 do
    begin
      attd := mSynTxtSynTxt.Attribute[j];

      OFormDebug.Append := 'Att: ' + attq.Name + '_' + attd.Name + '  cnt: ' + mAttList.Count.ToString();

      att :=  mHighlighter.AddSpecialAttribute(attq.Name + '_' + attd.Name, attq.Name + '_' + attd.Name);

      att.Foreground := attq.Foreground;
      att.Background := attq.Background;
      att.FrameColor := attq.FrameColor;
      att.FrameEdges := attq.FrameEdges;
      att.FrameStyle := attq.FrameStyle;
      att.Style := attq.Style;

      mAttList.Add(att);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: ColorSchame
 ------------------------------------------------------------------------------}
procedure TSynSpellToken.ColorSchame;
var
  att, attq, attd: TSynHighlighterAttributes;
  z, i, j, k: Integer;
  s: String;
begin
  if (mHighlighter = Nil) then Exit;
  if (mSynTxtSynTxt = Nil) then Exit;

  z := mSynTxtSynTxt.AttrCount;

  i := 0;
  while i < mHighlighter.AttrCount do
  begin
    attq := mHighlighter.Attribute[i];
    Inc(i);
    s := attq.Name;
    s:=s;

    if mAttributes.IndexOf(attq.Name) < 0 then continue;

    for j := 0 to z-1 do
    begin
      attd := mSynTxtSynTxt.Attribute[j];

      OFormDebug.Append := 'Att: ' + attq.Name + '_' + attd.Name + '  cnt: ' + mAttList.Count.ToString();

      s:= attq.Name + '_' + attd.Name;

      k := 0;
      while k < mHighlighter.AttrCount do
      begin
        att := mHighlighter.Attribute[k];
        Inc(k);
        if att.Name <> s then continue;

        att.Foreground := attq.Foreground;
        att.Background := attq.Background;
        att.FrameColor := attq.FrameColor;
        att.FrameEdges := attq.FrameEdges;
        att.FrameStyle := attq.FrameStyle;

        att.Style := attq.Style;
      end;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: SetLine
 ------------------------------------------------------------------------------}
procedure TSynSpellToken.SetLine(const NewValue: String; LineNumber: Integer);
var
  i: Integer;
begin
  mLineNumber := LineNumber;

  for i := mLnSpell.Count to LineNumber+1 do
  begin
    mLnSpell.Add(TTkSpell.Create);
  end;
  mTkSpell := mLnSpell.Items[LineNumber];

  // if Not mFlagInvalidate then mTkSpell.Clear;
  if mFlagDirty And Not(mFlagInvalidate) then mTkSpell.Clear;

  mFlagLineDirty := False;
  mSpll^.y := mLineNumber;
  mTokenCnt := 0;
  Next;
end;


{-------------------------------------------------------------------------------
  @NAME: Next
 ------------------------------------------------------------------------------}
procedure TSynSpellToken.Next;
var
  att: TSynHighlighterAttributes;
  pos: Integer;
  str: String;
  i, d: Integer;
  snx: String;
begin
  att := mHighlighter.GetTokenAttribute;
  if Not (att = Nil) then
  begin
    d := mAttributes.IndexOf(att.Name);
    if (d >= 0) then
    begin
      mAttIndex := d;
      pos := mHighlighter.GetTokenPos;
      str := '';
      for i := 1 to pos do str := str + ' ';
      str := str + mHighlighter.GetToken;

      mFlagToSpell := True;
      mSynTxtSynTxt.SetLine(str, mLineNumber, pos);

      snx := mSynTxtSynTxt.GetToken;
      OFormDebug.Append := 'It´s a ' + att.Name + ': "' + str + '" - "' + snx + '"';
    end;
  end;
  If Not (mHighlighter.GetEol = true) then
  begin
    CheckToken;
  end
  else  // ToDo: Zeile (Anzahl Token) Ungültig?
  begin
    i := mTkSpell.Count;
    if (mTokenCnt+1 < mTkSpell.Count) then
    begin
      mTkSpell.RemoveToEnd(mTokenCnt);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: Clear
 ------------------------------------------------------------------------------}
procedure TSynSpellToken.Clear;
begin
  mTkSpell.Clear;
  mLnSpell.Clear;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckToken
 ------------------------------------------------------------------------------}
procedure TSynSpellToken.CheckToken;
var
  att: TSynHighlighterAttributes;
  newSpll: TPTkSpell;
  s: String;
begin
  att := mHighlighter.GetTokenAttribute;
  if att = Nil then exit;

  s := '';
  s := s + 'Line:'  + IntToStr(mLineNumber + 1) +
           ' Col: ' + IntToStr(mHighlighter.GetTokenPos + 1) +
           ' "'     + mHighlighter.GetToken +
           '" Len: '+ Length(mHighlighter.GetToken).ToString() +
           ' Att: ' + att.Name;

  OFormDebug.Append := s;

  mSpll^.Len := Length(mHighlighter.GetToken);
  mSpll^.x := mHighlighter.GetTokenPos;

  if Not (att.Name.Contains('Word')) And Not (att.Name = 'Word' ) then  // ToDo: Zeile (Anzahl Token) Ungültig?
  begin
    if (mTokenCnt+1 > mTkSpell.Count) then exit;

    if (mTkSpell.Items[mTokenCnt]^.x = mSpll^.x) And
       (mTkSpell.Items[mTokenCnt]^.y = mSpll^.y) then
    begin
      mFlagLineDirty := true;
      mTkSpell.RemoveToEnd(mTokenCnt);
    end;
    exit;
  end;

  Inc(mTokenCnt);

  mSpll^.Att := att;

  if Not(mSpll^.Att = Nil) And ((mFlagDirty) Or (mFlagLineDirty)) then
  begin
    mTkSpell.Add(mSpll);
  end
  else
  begin
    if mTokenCnt <= mTkSpell.Count then // ToDo: Zeile (Anzahl Token) Ungültig?
    begin
      if (mTkSpell.Items[mTokenCnt-1]^.x = mSpll^.x) And
         (mTkSpell.Items[mTokenCnt-1]^.y = mSpll^.y) And
         (mTkSpell.Items[mTokenCnt-1]^.Len = mSpll^.Len) then
      begin
        mTkSpell.Items[mTokenCnt-1]^.Att := mSpll^.Att;
      end
      else
      begin
        mFlagLineDirty := true;
        mTkSpell.RemoveToEnd(mTokenCnt-1);
        mTkSpell.Add(mSpll);
      end;
    end
    else
    begin
      mTkSpell.Add(mSpll);
    end;
  end;

  newSpll := New(TPTkSpell);
  newSpll^.Passed := False;
  newSpll^.Checked := False;
  newSpll^.Len := 0;
  newSpll^.x := 0;
  newSpll^.y := mLineNumber;
  newSpll^.Att := att;

  mSpll := newSpll;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenPosByPos
 ------------------------------------------------------------------------------}
function TSynSpellToken.GetTokenPosByPos(p: TPoint): TPTkSpell;
var
  tkLine  : TTkSpell;
  spell   : TPTkSpell;
  i, x, y : Integer;
begin

  Result := Nil;

  x := p.x - 1; y := p.y - 1;
  if (y < 0 ) Or (mLnSpell.Count < y) then exit;

  tkLine := mLnSpell.Items[y];

  for i:=0 to tkLine.Count-1 do
  begin
    spell := tkLine.Items[i];
    if (spell^.x <= x) And ( (spell^.x + spell^.Len) > x) then
    begin
      Result := spell;
      break;
    end;
   end;
end;


{-------------------------------------------------------------------------------
  @NAME: GetToken
 ------------------------------------------------------------------------------}
function TSynSpellToken.GetToken(p: TPoint): String;
var
  spell : TPTkSpell;
  x, y: Integer;
begin
  Result := '';

  spell := GetTokenPosByPos(p);

  x := p.x - 1; y := p.y - 1;
  if (spell = Nil) Or (y < 0 ) Or (mHighlighter.CurrentLines.Count < y) then exit;

  Result := mHighlighter.CurrentLines[y].Substring(x, spell^.Len);
end;


{-------------------------------------------------------------------------------
  @NAME: SetTokenChecked
 ------------------------------------------------------------------------------}
procedure TSynSpellToken.SetTokenChecked(p: TPoint);
var
  tkLine  : TTkSpell;
  spell   : TPTkSpell;
  i, x, y : Integer;
begin
  x := p.x - 1; y := p.y - 1;
  if (y < 0 ) Or (mLnSpell.Count < y) then exit;

  tkLine := mLnSpell.Items[y];

  for i:=0 to tkLine.Count-1 do
  begin
    spell := tkLine.Items[i];
    if (spell^.x <= x) And ( (spell^.x + spell^.Len) > x) then
    begin
      tkLine.Items[i]^.Checked := True;
      break;
    end;
   end;
end;


{-------------------------------------------------------------------------------
  @NAME: SetTokenPassed
 ------------------------------------------------------------------------------}
procedure TSynSpellToken.SetTokenPassed(p: TPoint; flag: Boolean);
var
  tkLine  : TTkSpell;
  spell   : TPTkSpell;
  i, x, y : Integer;
begin
  x := p.x - 1; y := p.y - 1;
  if (y < 0 ) Or (mLnSpell.Count < y) then exit;

  tkLine := mLnSpell.Items[y];

  for i:=0 to tkLine.Count-1 do
  begin
    spell := tkLine.Items[i];
    if (spell^.x <= x) And ( (spell^.x + spell^.Len) > x) then
    begin
      tkLine.Items[i]^.Passed  := True;
      break;
    end;
   end;
end;


{-------------------------------------------------------------------------------
  @NAME: IsSplited
 ------------------------------------------------------------------------------}
Function TSynSpellToken.IsSplited: Boolean;
begin
  Result := false;
  if mFlagToSpell then Result := true;
end;


{-------------------------------------------------------------------------------
  @NAME: SplitedNext
 ------------------------------------------------------------------------------}
procedure TSynSpellToken.SplitedNext;
begin
  if IsSplited then
  begin
    mSynTxtSynTxt.Next;
    CheckToken;
    If mSynTxtSynTxt.GetEol = true then
    begin
      mFlagToSpell := False;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: SplitedToken
 ------------------------------------------------------------------------------}
function TSynSpellToken.SplitedToken: string;
begin
  if IsSplited then
  begin
    Result := mSynTxtSynTxt.GetToken;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: SplitedTokenEx
 ------------------------------------------------------------------------------}
procedure TSynSpellToken.SplitedTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  if IsSplited then
  begin
    mSynTxtSynTxt.GetTokenEx(TokenStart, TokenLength);
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: SplitedTokenAttribute
 ------------------------------------------------------------------------------}
function TSynSpellToken.SplitedTokenAttribute: TSynHighlighterAttributes;
var
  attT, attH: TSynHighlighterAttributes;
  z, i, p, idx: Integer;
begin
  Result := Nil;
  if Not IsSplited then exit;
  if mSynTxtSynTxt.GetTokenAttribute = Nil then exit;

  if mAttIndex < 0 then begin Result := mSynTxtSynTxt.GetTokenAttribute; exit; end;

  p:=0; z := mSynTxtSynTxt.AttrCount-1;
  for i := 0 to z do
  begin
    attT := mSynTxtSynTxt.GetTokenAttribute;
    attH := mSynTxtSynTxt.Attribute[i];
    if attT = attH then begin p:= i; break; end;
  end;
  idx := (mAttIndex * mSynTxtSynTxt.AttrCount) + p;
  Result := mAttList.Items[idx];
end;


{-------------------------------------------------------------------------------
  @NAME: SplitedTokenID
 ------------------------------------------------------------------------------}
function TSynSpellToken.SplitedTokenID: TtkTokenKind;
begin
  if IsSplited then
  begin
    Result := tkUnknown;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: SplitedTokenKind
 ------------------------------------------------------------------------------}
function TSynSpellToken.SplitedTokenKind: integer;
begin
  if IsSplited then
  begin
    Result := mSynTxtSynTxt.GetTokenKind;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: SplitedTokenPos
 ------------------------------------------------------------------------------}
function TSynSpellToken.SplitedTokenPos: Integer;
begin
  if IsSplited then
  begin
    Result := mSynTxtSynTxt.GetTokenPos;
  end;
end;



end.

