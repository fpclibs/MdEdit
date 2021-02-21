{
 ******************************************++***********************************
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
unit FormGopage;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, Graphics, Forms, Controls, Buttons, StdCtrls, ExtCtrls, Spin;

type

  { TFormGoPage }

  TFormGoPage = class(TForm)

    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Bevel1: TBevel;
    PageNum: TSpinEdit;

  public
    procedure PageNumEnter(Sender: TObject);
    procedure PageNumKeyDown(Sender: TObject; var Key: Word;  {%H-}Shift: TShiftState);

  end;


var
  FormGoPage: TFormGoPage;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


{ TFormGoPage }

{-------------------------------------------------------------------------------
  PageNumEnter
 ------------------------------------------------------------------------------}
procedure TFormGoPage.PageNumEnter(Sender: TObject);
begin
  PageNum.SelectAll;
end;


{-------------------------------------------------------------------------------
  PageNumKeyDown
 ------------------------------------------------------------------------------}
procedure TFormGoPage.PageNumKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if Key = 13 then
  Begin
  Key := 0;
  OKBtn.Click;
  end;
end;



end.
