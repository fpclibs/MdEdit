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
unit FrameSpellCheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, FrameSettingsBase;

type

  { TFrameSpellCheck }

  TFrameSpellCheck = class(TFrameSettingsBase)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Memo1: TMemo;

  public
    procedure SetSettings();override;
    procedure GetSettings();override;
    Function  GetTreePath: TStrArray; override;

  end;



implementation

{$R *.lfm}


{ TFrameSpellCheck }

{-------------------------------------------------------------------------------
  @NAME: SetSettings
 ------------------------------------------------------------------------------}
procedure TFrameSpellCheck.SetSettings();
begin

end;

{-------------------------------------------------------------------------------
  @NAME: GetSettings
 ------------------------------------------------------------------------------}
procedure TFrameSpellCheck.GetSettings();
begin

end;


{-------------------------------------------------------------------------------
  @NAME: GetTreePath
 ------------------------------------------------------------------------------}
Function  TFrameSpellCheck.GetTreePath: TStrArray;
var
  ary: Array[0..2] of String = ('Rechtschreibung', 'Einstellungen', 'Details');
begin
  Result := ary;
end;



end.

