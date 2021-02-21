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
unit CnfSynedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, AppSettings, CnfSyneditFrameColours, CnfSyneditFrameGeneral,
  CnfSyneditFrameDisplay, CnfSyneditOptions, CnfSyneditFrameThemes,
  Language, SysUtils;

type

  { TEditorConfigBridge }

  TEditorConfigBridge = class

  private { Members }
    mSettings: TAppSettings;
    mEditorOldOptions: TEditorOptions;

  public   { Class Publics }
    property EditorOptions: TEditorOptions read mEditorOldOptions;

    constructor Create(settings : TAppSettings);

  end;


implementation


{ TEditorConfigBridge }

{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TEditorConfigBridge.Create(settings : TAppSettings);
var
  frameColours : TCnfSyneditFrameColours;
  frameGeneral : TCnfSyneditFrameGeneral;
  frameOptions : TCnfSyneditFrameDisplay;
  frameEThemes : TCnfSyneditFrameThemes;
begin

  mSettings := settings;

  mSettings.Services.AddService(TEditorOptions.Create(mSettings));

  frameGeneral := TCnfSyneditFrameGeneral.Create(Nil);
  frameOptions := TCnfSyneditFrameDisplay.Create(Nil);
  frameColours := TCnfSyneditFrameColours.Create(Nil);
  frameEThemes := TCnfSyneditFrameThemes.Create (Nil);

  mEditorOldOptions := TEditorOptions(mSettings.Services.FindFirstByClass(TEditorOptions));

  mEditorOldOptions.GeneralPage := frameGeneral;
  mEditorOldOptions.OptionsPage := frameOptions;
  mEditorOldOptions.ColoursPage := frameColours;
  mEditorOldOptions.EThemesPage := frameEThemes;

  mSettings.AddFrame(frameGeneral);
  mSettings.AddFrame(frameEThemes);
  mSettings.AddFrame(frameOptions);
  mSettings.AddFrame(frameColours);

end;



end.

