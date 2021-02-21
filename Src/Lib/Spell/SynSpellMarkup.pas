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
unit SynSpellMarkup;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, SynEdit, SynEditTypes, SynEditMiscClasses, SynEditMarkup,
  SynEditHighlighter, SynSpellToken, Hunspell, FormDebug, AppSettings,
  FormSpellCheckDlg,

  {$ifdef unix} cthreads, {$endif} SysUtils;


type

   TCheckNextEvent = procedure(var eof: Boolean) of Object;

   { TMyThread }

   TMyThread = class(TThread)

   private
     mEof: Boolean;
     fOnCheckNext: TCheckNextEvent;

   protected
     procedure Execute; override;
     property OnCheckNext: TCheckNextEvent read fOnCheckNext write fOnCheckNext;
     procedure CheckNext;

   public
     Constructor Create(CreateSuspended : boolean);

   end;



type

  { TSynSpellMarkup }

  TSynSpellMarkup = class(TSynEditMarkup)

  private
    mHunspell : THunspell;
    mFlagDirty: Boolean;
    mFlagThreadStart: Boolean;
    mSpellCount: Integer;
    mFlagInvalidate: Boolean;
    mYlast: Integer;
    mSuspend: Boolean;
    mDictsLine: Integer;

    mMyThread: TMyThread;
    mTkDicts: TTkSpell;
    mTkSpell: TTkSpell;
    mTkCheck: TTkSpell;

    mSpellToken: TSynSpellToken;
    mAppCnf : TAppSettings;
    mFlagCamelCase : Boolean;
    mFlagUpperCase : Boolean;

  private
    procedure SetDirtyFlag(flag: Boolean);

  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;

    function GetMarkupAttributeAtRowCol(const aRow: Integer;
      const aStartCol: TLazSynDisplayTokenBound;
      const {%H-}AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;

    procedure GetNextMarkupColAfterRowCol(const {%H-}aRow: Integer;
      const {%H-}aStartCol: TLazSynDisplayTokenBound;
      const {%H-}AnRtlInfo: TLazSynDisplayRtlInfo; out {%H-}ANextPhys, {%H-}ANextLog: Integer); override;

    procedure BeginMarkup; override;
    procedure EndMarkup; override;
    property  FlagDirty: Boolean read mFlagDirty write SetDirtyFlag;

    function  WrongWordAt(p: TPoint; var bgn: Integer; var len: Integer; var passed: Boolean): String;
    procedure SuggestWords(Word: string; list: TStrings);

    Function  DoSpellCheck(): Boolean;
    procedure DoneSpellCheck(Sender: TObject);
    procedure DoCheckNext(var eof: Boolean);
    procedure DoCheckSpellNext(var eof: Boolean);

    property  SpellToken: TSynSpellToken read mSpellToken write mSpellToken;

  public
    procedure OnSettingsChange(Sender: TObject);

  private
    function  CeckWordUpperCase(word: string): Integer;
    procedure DoSettingsNotify(Sender: TObject);
    procedure DebugNotify(Sender: TObject);
    procedure InitHunspell;

end;


implementation


{ TMyThread }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TMyThread.Create(CreateSuspended : boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := False;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckNext
  @INFO: Diese Methode wird vom MainThread ausgeführt und kann deshalb auf alle
         GUI-Elemente zugreifen.
 ------------------------------------------------------------------------------}
procedure TMyThread.CheckNext;
begin
  if Assigned(fOnCheckNext) then fOnCheckNext(mEof);
end;


{-------------------------------------------------------------------------------
  @NAME: Execute
 ------------------------------------------------------------------------------}
procedure TMyThread.Execute;
begin
  while (not Terminated) do
  begin
    mEof := False;
    while (not Terminated) and (Not mEof) do
    begin
      mEof := True;
      Synchronize(@CheckNext);
    end;
    Suspended := True;
  end;
end;



{ TSynSpellMarkup }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSynSpellMarkup.Create(ASynEdit : TSynEditBase);
begin
  inherited Create(ASynEdit);

  mTkDicts := TTkSpell.Create;
  mTkSpell := TTkSpell.Create;
  mTkCheck := TTkSpell.Create;

  OFormDebug.AddNotify(@DebugNotify);

  mMyThread := TMyThread.Create(True);
  mMyThread.OnCheckNext := @DoCheckNext;

  mFlagThreadStart  := False;

  mSpellToken := Nil;
  mHunspell   := Nil;
  mFlagDirty  := False;
  mSuspend    := False;
  mDictsLine  := 0;
  mFlagCamelCase := False;
  mFlagUpperCase := False;

  mAppCnf  := TAppSettings.Create();
  mAppCnf.AddNotify(@DoSettingsNotify);
  OFormSpellCheckDlg.AddNotify(@DoSettingsNotify);

  InitHunspell;
end;


{-------------------------------------------------------------------------------
  @NAME: DoSettingsNotify
 ------------------------------------------------------------------------------}
procedure TSynSpellMarkup.DoSettingsNotify(Sender: TObject);
begin
  SetDirtyFlag(True);
  OnSettingsChange(Self);
  if Not (mSpellToken = Nil) then mSpellToken.Clear;
end;


{-------------------------------------------------------------------------------
  @NAME: TSynSpellMarkup
 ------------------------------------------------------------------------------}
destructor TSynSpellMarkup.Destroy;
begin
  mHunspell.Free;
  mHunspell := Nil;

  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  @NAME: OnSettingsChange
 ------------------------------------------------------------------------------}
procedure TSynSpellMarkup.OnSettingsChange(Sender: TObject);
var
  syn: TSynEdit;
begin
  InitHunspell;
  syn := TSynEdit(self.SynEdit);
  syn.Invalidate;
end;


{-------------------------------------------------------------------------------
  @NAME: InitHunspell
 ------------------------------------------------------------------------------}
procedure TSynSpellMarkup.InitHunspell;
var
  filename: String;
begin
  if Not (mHunspell = Nil) then mHunspell.Free;
  mHunspell := Nil;

  mFlagCamelCase := mAppCnf.Read('SpellCheckDlg/CamelCase', False);
  mFlagUpperCase := mAppCnf.Read('SpellCheckDlg/UpperCase', False);

  if mAppCnf.Read('SpellCheckDlg/AutoSpell', False) = False then exit;

  mHunspell := THunspell.Create();

  if Not (mHunspell.ErrorMessage = '') then
  begin
    mHunspell.Free;
    mHunspell := Nil;
  end;
  filename := mAppCnf.Read('SpellCheckDlg/ActiveDig', '');

  if not mHunspell.SetDictionary(filename) then
  begin
    mHunspell.Free;
    mHunspell := Nil;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: SetDirtyFlag
 ------------------------------------------------------------------------------}
Procedure TSynSpellMarkup.SetDirtyFlag(flag: Boolean);
var
  syn: TSynEdit;
begin
  syn := TSynEdit(self.SynEdit);
  if syn = Nil then exit;

  mFlagDirty := flag;
  if Not(mSpellToken = Nil) then mSpellToken.Dirty:=flag;
  if mFlagDirty then
  begin
    mSuspend := True;
    mTkSpell.Clear;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: BeginMarkup
 ------------------------------------------------------------------------------}
procedure TSynSpellMarkup.BeginMarkup;
begin
  mSuspend := False;
  inherited BeginMarkup;
end;


{-------------------------------------------------------------------------------
  @NAME: EndMarkup
 ------------------------------------------------------------------------------}
procedure TSynSpellMarkup.EndMarkup;
var
  syn  : TSynEdit;
begin
  inherited EndMarkup;

  syn := TSynEdit(self.SynEdit);
  if syn = Nil then exit;

  if mFlagDirty then
  begin
    mFlagDirty := False;
    if Not(mSpellToken = Nil) then mSpellToken.Dirty := False;
  end;
  mFlagThreadStart := False;
  if mFlagInvalidate = False then DoSpellCheck;
  mFlagInvalidate := False;

  if Not(mSpellToken = Nil) then mSpellToken.Invalidate := False;
end;


{-------------------------------------------------------------------------------
  @NAME: GetMarkupAttributeAtRowCol
 ------------------------------------------------------------------------------}
function TSynSpellMarkup.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound;
  const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
var
  p : TPoint;
  att: TSynHighlighterAttributes;
  spll: TPTkSpell;
begin
  Result := Nil;
  p.x := aStartCol.Logical; p.y := aRow;

  if Not(mSpellToken = Nil) then spll := mSpellToken.GetTokenPosByPos(p);

  if spll = Nil then exit;

  att  := spll^.Att;

  if att = Nil then exit;

  if (spll^.Checked) And Not(spll^.Passed) then
  begin
    Result := TSynSelectedColor.Create;
    Result.Foreground := att.Foreground;
    Result.Background := att.Background;
    Result.FrameStyle := TSynLineStyle(slsWaved);
    Result.FrameColor := TColor($0000FF);
    Result.FrameEdges := TSynFrameEdges(sfeBottom);
  end;

  if Not mFlagInvalidate then mTkSpell.Add(spll);
end;


{-------------------------------------------------------------------------------
  @NAME: GetNextMarkupColAfterRowCol
 ------------------------------------------------------------------------------}
Procedure TSynSpellMarkup.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound;
  const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys, ANextLog: Integer);
begin

end;


{-------------------------------------------------------------------------------
  @NAME: DoSpellCheck
 ------------------------------------------------------------------------------}
function TSynSpellMarkup.DoSpellCheck(): Boolean;
var
  mTkBuf : TTkSpell;
begin
  Result := False;

  if mTkSpell.Count = 0 then exit;

  if mFlagThreadStart then exit;
  mFlagThreadStart := True;

  mTkBuf   := mTkCheck;
  mTkCheck := mTkSpell;
  mTkSpell := mTkBuf;

  mTkSpell.Clear;
  mSpellCount := 0;

  mYlast := -1;
  mMyThread.Start;
  Result := True;
end;


{-------------------------------------------------------------------------------
  @NAME: DoneSpellCheck
 ------------------------------------------------------------------------------}
Procedure TSynSpellMarkup.DoneSpellCheck(Sender: TObject);
var
  syn  : TSynEdit;
begin
  mFlagThreadStart := False;

  syn := TSynEdit(self.SynEdit);
  mFlagInvalidate := True;
  syn.Invalidate;

  syn := TSynEdit(self.SynEdit);
  if syn = Nil then exit;

  if Not(mSpellToken = Nil) then mSpellToken.Invalidate := True;
end;


{-------------------------------------------------------------------------------
  @NAME: DoCheckNext
 ------------------------------------------------------------------------------}
Procedure TSynSpellMarkup.DoCheckNext(var eof: Boolean);
begin
  eof := True;
  if mSpellCount < mTkCheck.Count then
  begin
    if Not mSuspend then DoCheckSpellNext(eof);
  end
  else
  begin
    DoneSpellCheck(Self);
  end;
  Inc(mSpellCount);
end;


{-------------------------------------------------------------------------------
  @NAME: DoCheckSpellNext
 ------------------------------------------------------------------------------}
Procedure TSynSpellMarkup.DoCheckSpellNext(var eof: Boolean);
var
  x, y, l : Integer;
  p       : TPoint;
  syn     : TSynEdit;
  spll    : TPTkSpell;
  word    : String;
begin
  syn  := TSynEdit(self.SynEdit);
  if (syn = Nil) then exit;

  eof  := True;
  if mSpellCount >= mTkCheck.Count then exit;

  eof  := False;
  spll := TPTkSpell(mTkCheck.Items[mSpellCount]);

  x    := spll^.x; y := spll^.y; l := spll^.Len;
  p.x  := spll^.x+1; p.y := spll^.y+1;

  if (y < 0) Or (y >= syn.Lines.Count) then exit;

  word := syn.Lines[y].Substring(x, l);

  if Not(mSpellToken = Nil) then
  begin
    mSpellToken.SetTokenChecked(p);
    if (mHunspell = Nil) then
    begin
      mSpellToken.SetTokenPassed(p, True);
      exit;
    end;
    l := 0;
    if mFlagCamelCase Or mFlagUpperCase then l := CeckWordUpperCase(word);

    if mFlagCamelCase then if l > 0 then
    begin
      mSpellToken.SetTokenPassed(p, True);
      exit;
    end;

    if mFlagUpperCase then if (l  > 0) And (l = word.Length-1) then
    begin
      mSpellToken.SetTokenPassed(p, True);
      exit;
    end;
    if (mHunspell.Spell(word)) then mSpellToken.SetTokenPassed(p, True);
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: WrongWordAt
 ------------------------------------------------------------------------------}
function TSynSpellMarkup.WrongWordAt(p: TPoint; var bgn: Integer; var len: Integer; var passed: Boolean): String;
var
  x, y, l : Integer;
  spll    : TPTkSpell;
  syn     : TSynEdit;
begin
  Result := '';
  passed := True;

  if mFlagDirty then exit;
  bgn := 0;
  len := 0;

  syn  := TSynEdit(self.SynEdit);
  if syn = Nil then exit;

  if Not(mSpellToken = Nil) then spll := mSpellToken.GetTokenPosByPos(p);

  if spll = Nil then exit;

  x := spll^.x; y := spll^.y; l := spll^.Len;

  if (x > p.x) Or (x + l < p.x) Or (y < 0) Or
     (y >= syn.Lines.Count) then exit;

  Result := syn.Lines[y].Substring(x, l);

  if spll^.Checked then passed := spll^.Passed;
  bgn := x + 1; len := l;
end;


{-------------------------------------------------------------------------------
  @NAME: SuggestWords
 ------------------------------------------------------------------------------}
Procedure TSynSpellMarkup.SuggestWords(Word: string; list: TStrings);
begin
  list.Clear;
  if Not Assigned(mHunspell) then exit;
  mHunspell.Suggest(word, list);
end;


{-------------------------------------------------------------------------------
  @NAME: CeckWordUpperCase
 ------------------------------------------------------------------------------}
function TSynSpellMarkup.CeckWordUpperCase(word: string): Integer;
var
  str: String;
  i: Integer;
begin
  Result := 0;
  str := Word.LowerCase(Word);
  for i:=1 to Word.Length-1 do begin if str.Chars[i] <> word.Chars[i] then Inc(Result) end;
end;


{-------------------------------------------------------------------------------
  @NAME: DebugNotify
 ------------------------------------------------------------------------------}
procedure TSynSpellMarkup.DebugNotify(Sender: TObject);
var
  ary  : TStringArray;
  i    : Integer;
  Spll : TPTkSpell;
  mDbgLineBreak: Integer;
begin
  ary := OFormDebug.Command.Split(':');
  mDbgLineBreak := -1;

  if Length(ary) > 0 then
  begin
    if ary[0] = 'List' then
    begin
      if Length(ary) > 1 then
      begin
        mDbgLineBreak := StrToInt(ary[1]);
        OFormDebug.Append:='TSynSpellMarkup List:' + mDbgLineBreak.ToString() + ' OK!';
      end;
    end;
  end;
  for i:=0 to mTkSpell.Count-1 do
  begin
    Spll := TPTkSpell(mTkSpell.Items[i]);
    OFormDebug.Append:='y: ' + Spll^.y.ToString() + ' x: ' + Spll^.x.ToString() + ' Len: ' + Spll^.Len.ToString();
  end;
end;



end.

