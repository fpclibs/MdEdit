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
unit SynSpellcheckHighlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditHighlighter, SynSpellToken,

  SynHighlighterTxt, SynHighlighterMd,
  SynHighlighterPas, SynHighlighterCpp, SynHighlighterJScript,
  SynHighlighterHTML, SynHighlighterXML, SynHighlighterPython;


type

  { TSynSpellTxtSyn: Class for creating Spell Text highlighter }

  TSynSpellTxtSyn = class(TSynTxtSyn)
  private
    mSpellToken: TSynSpellToken;

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;

    property  SpellToken: TSynSpellToken read mSpellToken write mSpellToken;

end;


  { TSynSpellMdSyn: Class for creating Spell Text highlighter }

  TSynSpellMdSyn = class(TSynMdSyn)
  private
    mSpellToken: TSynSpellToken;

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;

    property  SpellToken: TSynSpellToken read mSpellToken write mSpellToken;

end;


  { TSynSpellCppSyn: Class for creating Spell Cpp highlighter }

  TSynSpellCppSyn = class(TSynCppSyn)
  private
    mSpellToken: TSynSpellToken;

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;

    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;

    property  SpellToken: TSynSpellToken read mSpellToken write mSpellToken;

end;


  { TSynSpellHtmlSyn: Class for creating Spell Html highlighter }

  TSynSpellHtmlSyn = class(TSynHtmlSyn)
  private
    mSpellToken: TSynSpellToken;

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;

    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;

    property  SpellToken: TSynSpellToken read mSpellToken write mSpellToken;

end;


  { TSynSpellJsSyn: Class for creating Spell Js highlighter }

  TSynSpellJsSyn = class(TSynJScriptSyn)
  private
    mSpellToken: TSynSpellToken;

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;

    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;

    property  SpellToken: TSynSpellToken read mSpellToken write mSpellToken;

end;


  { TSynSpellPasSyn: Class for creating Spell Pas highlighter }

  TSynSpellPasSyn = class(TSynPasSyn)
  private
    mSpellToken: TSynSpellToken;

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;

    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;

    property  SpellToken: TSynSpellToken read mSpellToken write mSpellToken;

end;


  { TSynSpellPySyn: Class for creating Spell Py highlighter }

  TSynSpellPySyn = class(TSynPythonSyn)
  private
    mSpellToken: TSynSpellToken;

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;

    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;

    property  SpellToken: TSynSpellToken read mSpellToken write mSpellToken;

end;


  { TSynSpellXmlSyn: Class for creating Spell Xml highlighter }

  TSynSpellXmlSyn = class(TSynXMLSyn)
  private
    mSpellToken: TSynSpellToken;

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;

    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;

    property  SpellToken: TSynSpellToken read mSpellToken write mSpellToken;

end;



implementation


{ Class TSynSpellTxtSyn }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSynSpellTxtSyn.Create(AOwner: TComponent);
begin
  mSpellToken := Nil;
  inherited Create(AOwner);
end;


{-------------------------------------------------------------------------------
  @NAME: SetLine
 ------------------------------------------------------------------------------}
procedure TSynSpellTxtSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);
  if Not(mSpellToken = Nil) then mSpellToken.SetLine(NewValue, LineNumber);
end;


{-------------------------------------------------------------------------------
  @NAME: Next
 ------------------------------------------------------------------------------}
procedure TSynSpellTxtSyn.Next;
begin
  if Not(mSpellToken = Nil) then mSpellToken.Next;
  inherited Next;
end;



{ Class TSynSpellMdSyn }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSynSpellMdSyn.Create(AOwner: TComponent);
begin
  mSpellToken := Nil;
  inherited Create(AOwner);
end;


{-------------------------------------------------------------------------------
  @NAME: SetLine
 ------------------------------------------------------------------------------}
procedure TSynSpellMdSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);
  if Not(mSpellToken = Nil) then mSpellToken.SetLine(NewValue, LineNumber);
end;


{-------------------------------------------------------------------------------
  @NAME: Next
 ------------------------------------------------------------------------------}
procedure TSynSpellMdSyn.Next;
begin
  if Not(mSpellToken = Nil) then mSpellToken.Next;
  inherited Next;
end;



{ Class TSynSpellCppSyn }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSynSpellCppSyn.Create(AOwner: TComponent);
var
  att: TSynHighlighterAttributes;
begin
  mSpellToken := Nil;
  inherited Create(AOwner);
  att := GetDefaultAttribute(SYN_ATTR_COMMENT);
  att.Foreground:=TColor($00C000);
  att := GetDefaultAttribute(SYN_ATTR_STRING);
  att.Foreground:=TColor($C00000);
end;


{-------------------------------------------------------------------------------
  @NAME: SetLine
 ------------------------------------------------------------------------------}
procedure TSynSpellCppSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);
  if Not(mSpellToken = Nil) then mSpellToken.SetLine(NewValue, LineNumber);
end;


{-------------------------------------------------------------------------------
  @NAME: Next
 ------------------------------------------------------------------------------}
procedure TSynSpellCppSyn.Next;
begin
  if (mSpellToken = Nil) then begin inherited Next; exit;
  end;

  if NOT mSpellToken.IsSplited then
  begin
    inherited Next;
    mSpellToken.Next;
    if mSpellToken.IsSplited then exit;
  end;

  if mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedNext;
    if mSpellToken.IsSplited then exit else inherited Next;
  end
end;


{-------------------------------------------------------------------------------
  @NAME: GetToken
 ------------------------------------------------------------------------------}
function TSynSpellCppSyn.GetToken: string;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedToken;
    exit;
  end;
  Result := inherited GetToken;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenEx
 ------------------------------------------------------------------------------}
procedure TSynSpellCppSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedTokenEx(TokenStart, TokenLength);
    exit;
  end;
  inherited GetTokenEx(TokenStart, TokenLength);
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenAttribute
 ------------------------------------------------------------------------------}
function TSynSpellCppSyn.GetTokenAttribute: TSynHighlighterAttributes;
var
  att: TSynHighlighterAttributes;
begin
  Result := inherited GetTokenAttribute;
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    att := mSpellToken.SplitedTokenAttribute;
    Result := att;
    exit;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenKind
 ------------------------------------------------------------------------------}
function TSynSpellCppSyn.GetTokenKind: integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenKind;
    exit;
  end;
  Result := inherited GetTokenKind;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenPos
 ------------------------------------------------------------------------------}
function TSynSpellCppSyn.GetTokenPos: Integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenPos;
    exit;
  end;
  Result := inherited GetTokenPos;
end;



{ Class TSynSpellHtmlSyn }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSynSpellHtmlSyn.Create(AOwner: TComponent);
begin
  mSpellToken := Nil;
  inherited Create(AOwner);
end;


{-------------------------------------------------------------------------------
  @NAME: SetLine
 ------------------------------------------------------------------------------}
procedure TSynSpellHtmlSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);
  if Not(mSpellToken = Nil) then mSpellToken.SetLine(NewValue, LineNumber);
end;


{-------------------------------------------------------------------------------
  @NAME: Next
 ------------------------------------------------------------------------------}
procedure TSynSpellHtmlSyn.Next;
begin
  if (mSpellToken = Nil) then begin inherited Next; exit;
  end;

  if NOT mSpellToken.IsSplited then
  begin
    inherited Next;
    mSpellToken.Next;
    if mSpellToken.IsSplited then exit;
  end;

  if mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedNext;
    if mSpellToken.IsSplited then exit else inherited Next;
  end
end;


{-------------------------------------------------------------------------------
  @NAME: GetToken
 ------------------------------------------------------------------------------}
function TSynSpellHtmlSyn.GetToken: string;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedToken;
    exit;
  end;
  Result := inherited GetToken;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenEx
 ------------------------------------------------------------------------------}
procedure TSynSpellHtmlSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedTokenEx(TokenStart, TokenLength);
    exit;
  end;
  inherited GetTokenEx(TokenStart, TokenLength);
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenAttribute
 ------------------------------------------------------------------------------}
function TSynSpellHtmlSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenAttribute;
    exit;
  end;
  Result := inherited GetTokenAttribute;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenKind
 ------------------------------------------------------------------------------}
function TSynSpellHtmlSyn.GetTokenKind: integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenKind;
    exit;
  end;
  Result := inherited GetTokenKind;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenPos
 ------------------------------------------------------------------------------}
function TSynSpellHtmlSyn.GetTokenPos: Integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenPos;
    exit;
  end;
  Result := inherited GetTokenPos;
end;



{ Class TSynSpellJsSyn }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSynSpellJsSyn.Create(AOwner: TComponent);
var
  att: TSynHighlighterAttributes;
begin
  mSpellToken := Nil;
  inherited Create(AOwner);
  att := GetDefaultAttribute(SYN_ATTR_COMMENT);
  att.Foreground:=TColor($00C000);
  att := GetDefaultAttribute(SYN_ATTR_STRING);
  att.Foreground:=TColor($C00000);
end;


{-------------------------------------------------------------------------------
  @NAME: SetLine
 ------------------------------------------------------------------------------}
procedure TSynSpellJsSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);
  if Not(mSpellToken = Nil) then mSpellToken.SetLine(NewValue, LineNumber);
end;


{-------------------------------------------------------------------------------
  @NAME: Next
 ------------------------------------------------------------------------------}
procedure TSynSpellJsSyn.Next;
begin
  if (mSpellToken = Nil) then begin inherited Next; exit;
  end;

  if NOT mSpellToken.IsSplited then
  begin
    inherited Next;
    mSpellToken.Next;
    if mSpellToken.IsSplited then exit;
  end;

  if mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedNext;
    if mSpellToken.IsSplited then exit else inherited Next;
  end
end;


{-------------------------------------------------------------------------------
  @NAME: GetToken
 ------------------------------------------------------------------------------}
function TSynSpellJsSyn.GetToken: string;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedToken;
    exit;
  end;
  Result := inherited GetToken;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenEx
 ------------------------------------------------------------------------------}
procedure TSynSpellJsSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedTokenEx(TokenStart, TokenLength);
    exit;
  end;
  inherited GetTokenEx(TokenStart, TokenLength);
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenAttribute
 ------------------------------------------------------------------------------}
function TSynSpellJsSyn.GetTokenAttribute: TSynHighlighterAttributes;
var
  att: TSynHighlighterAttributes;
begin
  Result := inherited GetTokenAttribute;
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    att := mSpellToken.SplitedTokenAttribute;
    Result := att;
    exit;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenKind
 ------------------------------------------------------------------------------}
function TSynSpellJsSyn.GetTokenKind: integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenKind;
    exit;
  end;
  Result := inherited GetTokenKind;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenPos
 ------------------------------------------------------------------------------}
function TSynSpellJsSyn.GetTokenPos: Integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenPos;
    exit;
  end;
  Result := inherited GetTokenPos;
end;



{ Class TSynSpellPasSyn }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSynSpellPasSyn.Create(AOwner: TComponent);
var
  att: TSynHighlighterAttributes;
begin
  mSpellToken := Nil;
  inherited Create(AOwner);
  att := GetDefaultAttribute(SYN_ATTR_COMMENT);
  att.Foreground:=TColor($00C000);
  att := GetDefaultAttribute(SYN_ATTR_STRING);
  att.Foreground:=TColor($0000C0);
end;


{-------------------------------------------------------------------------------
  @NAME: SetLine
 ------------------------------------------------------------------------------}
procedure TSynSpellPasSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);
  if Not(mSpellToken = Nil) then mSpellToken.SetLine(NewValue, LineNumber);
end;


{-------------------------------------------------------------------------------
  @NAME: Next
 ------------------------------------------------------------------------------}
procedure TSynSpellPasSyn.Next;
begin
  if (mSpellToken = Nil) then begin inherited Next; exit;
  end;

  if NOT mSpellToken.IsSplited then
  begin
    inherited Next;
    mSpellToken.Next;
    if mSpellToken.IsSplited then exit;
  end;

  if mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedNext;
    if mSpellToken.IsSplited then exit else inherited Next;
  end
end;


{-------------------------------------------------------------------------------
  @NAME: GetToken
 ------------------------------------------------------------------------------}
function TSynSpellPasSyn.GetToken: string;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedToken;
    exit;
  end;
  Result := inherited GetToken;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenEx
 ------------------------------------------------------------------------------}
procedure TSynSpellPasSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedTokenEx(TokenStart, TokenLength);
    exit;
  end;
  inherited GetTokenEx(TokenStart, TokenLength);
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenAttribute
 ------------------------------------------------------------------------------}
function TSynSpellPasSyn.GetTokenAttribute: TSynHighlighterAttributes;
var
  att: TSynHighlighterAttributes;
begin
  Result := inherited GetTokenAttribute;
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    att := mSpellToken.SplitedTokenAttribute;
    Result := att;
    exit;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenKind
 ------------------------------------------------------------------------------}
function TSynSpellPasSyn.GetTokenKind: integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenKind;
    exit;
  end;
  Result := inherited GetTokenKind;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenPos
 ------------------------------------------------------------------------------}
function TSynSpellPasSyn.GetTokenPos: Integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenPos;
    exit;
  end;
  Result := inherited GetTokenPos;
end;



{ Class TSynSpellPySyn }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSynSpellPySyn.Create(AOwner: TComponent);
var
  att: TSynHighlighterAttributes;
begin
  mSpellToken := Nil;
  inherited Create(AOwner);
  att := GetDefaultAttribute(SYN_ATTR_COMMENT);
  att.Foreground:=TColor($00C000);
  //att := GetDefaultAttribute(SYN_ATTR_STRING); // No Default String there
  //att.Foreground:=TColor($C00000);
end;


{-------------------------------------------------------------------------------
  @NAME: SetLine
 ------------------------------------------------------------------------------}
procedure TSynSpellPySyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);
  if Not(mSpellToken = Nil) then mSpellToken.SetLine(NewValue, LineNumber);
end;


{-------------------------------------------------------------------------------
  @NAME: Next
 ------------------------------------------------------------------------------}
procedure TSynSpellPySyn.Next;
begin
  if (mSpellToken = Nil) then begin inherited Next; exit;
  end;

  if NOT mSpellToken.IsSplited then
  begin
    inherited Next;
    mSpellToken.Next;
    if mSpellToken.IsSplited then exit;
  end;

  if mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedNext;
    if mSpellToken.IsSplited then exit else inherited Next;
  end
end;


{-------------------------------------------------------------------------------
  @NAME: GetToken
 ------------------------------------------------------------------------------}
function TSynSpellPySyn.GetToken: string;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedToken;
    exit;
  end;
  Result := inherited GetToken;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenEx
 ------------------------------------------------------------------------------}
procedure TSynSpellPySyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedTokenEx(TokenStart, TokenLength);
    exit;
  end;
  inherited GetTokenEx(TokenStart, TokenLength);
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenAttribute
 ------------------------------------------------------------------------------}
function TSynSpellPySyn.GetTokenAttribute: TSynHighlighterAttributes;
var
  att: TSynHighlighterAttributes;
begin
  Result := inherited GetTokenAttribute;
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    att := mSpellToken.SplitedTokenAttribute;
    Result := att;
    exit;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenKind
 ------------------------------------------------------------------------------}
function TSynSpellPySyn.GetTokenKind: integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenKind;
    exit;
  end;
  Result := inherited GetTokenKind;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenPos
 ------------------------------------------------------------------------------}
function TSynSpellPySyn.GetTokenPos: Integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenPos;
    exit;
  end;
  Result := inherited GetTokenPos;
end;



{ Class TSynSpellXmlSyn }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSynSpellXmlSyn.Create(AOwner: TComponent);
begin
  mSpellToken := Nil;
  inherited Create(AOwner);
end;


{-------------------------------------------------------------------------------
  @NAME: SetLine
 ------------------------------------------------------------------------------}
procedure TSynSpellXmlSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);
  if Not(mSpellToken = Nil) then mSpellToken.SetLine(NewValue, LineNumber);
end;


{-------------------------------------------------------------------------------
  @NAME: Next
 ------------------------------------------------------------------------------}
procedure TSynSpellXmlSyn.Next;
begin
  if (mSpellToken = Nil) then begin inherited Next; exit;
  end;

  if NOT mSpellToken.IsSplited then
  begin
    inherited Next;
    mSpellToken.Next;
    if mSpellToken.IsSplited then exit;
  end;

  if mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedNext;
    if mSpellToken.IsSplited then exit else inherited Next;
  end
end;


{-------------------------------------------------------------------------------
  @NAME: GetToken
 ------------------------------------------------------------------------------}
function TSynSpellXmlSyn.GetToken: string;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedToken;
    exit;
  end;
  Result := inherited GetToken;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenEx
 ------------------------------------------------------------------------------}
procedure TSynSpellXmlSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    mSpellToken.SplitedTokenEx(TokenStart, TokenLength);
    exit;
  end;
  inherited GetTokenEx(TokenStart, TokenLength);
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenAttribute
 ------------------------------------------------------------------------------}
function TSynSpellXmlSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenAttribute;
    exit;
  end;
  Result := inherited GetTokenAttribute;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenKind
 ------------------------------------------------------------------------------}
function TSynSpellXmlSyn.GetTokenKind: integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenKind;
    exit;
  end;
  Result := inherited GetTokenKind;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTokenPos
 ------------------------------------------------------------------------------}
function TSynSpellXmlSyn.GetTokenPos: Integer;
begin
  if Not(mSpellToken = Nil) And mSpellToken.IsSplited then
  begin
    Result := mSpellToken.SplitedTokenPos;
    exit;
  end;
  Result := inherited GetTokenPos;
end;



end.

