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
unit FrameSettingsInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Storage, Controls;

const
  SSettingsInterface = '{3FB1C775-F5AA-4D4C-B10C-D8139D742068}';

type

  TStrArray = Array Of String;

type

  ISettingsInterface = interface [SSettingsInterface]

    procedure GetSettings;
    procedure SetSettings;
    procedure SetCancel;
    procedure Free;
    Function GetTreePath: TStrArray;

    procedure _Settings(value: TAppStorage);
    function _Settings: TAppStorage;
    property Settings: TAppStorage read _Settings write _Settings;

    procedure _BoundsRect(value: TRect);
    function _BoundsRect: TRect;
    property BoundsRect: TRect read _BoundsRect write _BoundsRect;

    procedure _Parent(value: TWinControl);
    function _Parent: TWinControl;
    property Parent: TWinControl read _Parent write _Parent;

    procedure _Visible(value: Boolean);
    function _Visible: Boolean;

    property Visible: Boolean read _Visible write _Visible;

  end;


implementation


end.

