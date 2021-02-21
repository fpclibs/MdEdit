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
 *  Sources: Convert parts of Lazarus IDE unit to fpcLibs                      *
 *           Andreas Peter Luft, January 10 2021                               *
 *                                                                             *
 *******************************************************************************
}
unit CnfSyneditHighlighterDef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLazSyntaxHighlighter =
  ( lshNone, lshText, lshMd, lshHTML, lshCSS,  lshXML, lshJScript
  );

const
  LazSyntaxHighlighterNames: array[TLazSyntaxHighlighter] of String =
  ( 'None',
    'Text',
    'Markdown',
    'HTML',
    'CSS',
    'XML',
    'JScript'
  );

function GetSyntaxHighlighterCaption(h: TLazSyntaxHighlighter): string;
function StrToLazSyntaxHighlighter(const s: String): TLazSyntaxHighlighter;


implementation

{-------------------------------------------------------------------------------
  GetSyntaxHighlighterCaption
 ------------------------------------------------------------------------------}
function GetSyntaxHighlighterCaption(h: TLazSyntaxHighlighter): string;
begin
  if h=lshMd then
    Result:='Markdown'
  else
    Result:=LazSyntaxHighlighterNames[h];
end;


{-------------------------------------------------------------------------------
  StrToLazSyntaxHighlighter
 ------------------------------------------------------------------------------}
function StrToLazSyntaxHighlighter(const s: String): TLazSyntaxHighlighter;
begin
  for Result := Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do
    if (CompareText(s, LazSyntaxHighlighterNames[Result]) = 0) then
      exit;
  Result := lshMd;
end;



end.

