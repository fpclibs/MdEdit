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
 *  Sources: Paul Ishenin and Mike Thompson, March 24 2016                     *
 *           Andreas Peter Luft, January 10 2021                               *
 *                                                                             *
 *******************************************************************************
}
Unit AppVersion;

{$mode objfpc}

Interface

Uses
  Classes, SysUtils {$IFDEF VER3}, lclplatformdef{$ENDIF};

// Surfacing general defines and lookups
Function GetCompiledDate: String;
Function GetCompilerInfo: String;
Function GetTargetInfo: String;
Function GetOS: String;
Function GetCPU: String;
Function GetLCLVersion: String;
Function GetWidgetSet: String;

// Exposing resource and version info compiled into exe
Function VersionMajor:   Integer;
Function VersionMinor:   Integer;
Function VersionRelease: Integer;

Function GetResourceStrings(oStringList : TStringList) : Boolean;
Function GetFileVersion: String;
Function GetProductVersion: String;
Function GetFileDescription: String;
Function GetComments: String;
Function GetCopyright: String;

Const
  WIDGETSET_GTK        = 'GTK widget set';
  WIDGETSET_GTK2       = 'GTK 2 widget set';
  WIDGETSET_WIN        = 'Win32/Win64 widget set';
  WIDGETSET_WINCE      = 'WinCE widget set';
  WIDGETSET_CARBON     = 'Carbon widget set';
  WIDGETSET_QT         = 'QT widget set';
  WIDGETSET_fpGUI      = 'fpGUI widget set';
  WIDGETSET_OTHER      = 'Other gui';

Implementation

Uses
  resource, versiontypes, versionresource, LCLVersion, InterfaceBase;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetWidgetSet()
 @ RESULT: Name des verwendeten Widget-Set
 *
 @ INFO:
 * Gibt den Name des verwendeten Widget-Set zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function GetWidgetSet: String;
Begin
  Case WidgetSet.LCLPlatform Of
    lpGtk:   Result := WIDGETSET_GTK;
    lpGtk2:  Result := WIDGETSET_GTK2;
    lpWin32: Result := WIDGETSET_WIN;
    lpWinCE: Result := WIDGETSET_WINCE;
    lpCarbon:Result := WIDGETSET_CARBON;
    lpQT:    Result := WIDGETSET_QT;
    lpfpGUI: Result := WIDGETSET_fpGUI;
  Else
    Result := WIDGETSET_OTHER;
  End;
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetCompilerInfo()
 @ RESULT: Version des Compilers
 *
 @ INFO:
 * Gibt den Versionsstring des Compilers zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function GetCompilerInfo: String;
begin
  Result := 'FPC ' + {$I %FPCVERSION%};
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetTargetInfo()
 @ RESULT: Informationen zur Zielplatform
 *
 @ INFO:
 * Gibt Informationen zur Zielplatform zurueck (CPU und OS).
 *
 * ---------------------------------------------------------------------------*)
Function GetTargetInfo: String;
Begin
  Result := {$I %FPCTARGETCPU%} + ' - ' + {$I %FPCTARGETOS%};
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetOS()
 @ RESULT: Informationen zum Oberating System
 *
 @ INFO:
 * Gibt Informationen zum Oberating System zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function GetOS: String;
Begin
  Result := {$I %FPCTARGETOS%};
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetCPU()
 @ RESULT: Informationen zum CPU-Typ
 *
 @ INFO:
 * Gibt Informationen zum CPU-Typ zurueck.
 *
 * ---------------------------------------------------------------------------*)
function GetCPU: String;
begin
  Result := {$I %FPCTARGETCPU%};
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetLCLVersion()
 @ RESULT: Version der Lazarus Component Library
 *
 @ INFO:
 * Gibt die Version der Lazarus Component Library zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function GetLCLVersion: String;
Begin
  Result := 'LCL ' + lcl_version;
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetCompiledDate()
 @ RESULT: Datum und Uhrzeit der Erstellung
 *
 @ INFO:
 * Gibt das Datum und die Uhrzeit der Erstellung zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function GetCompiledDate: String;
Var
  sDate, sTime: String;
Begin
  sDate := {$I %DATE%};
  sTime := {$I %TIME%};

  Result := sDate + ' at ' + sTime;
End;


{ Routines to expose TVersionInfo data }

Type
  TVersionInfo = Class

  private
    FBuildInfoAvailable : Boolean;
    FVersResource       : TVersionResource;

    Function GetFixedInfo: TVersionFixedInfo;
    Function GetStringFileInfo: TVersionStringFileInfo;
    Function GetVarFileInfo: TVersionVarFileInfo;

  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Load(Instance: THandle);

    Property BuildInfoAvailable: Boolean Read FBuildInfoAvailable;
    Property FixedInfo: TVersionFixedInfo Read GetFixedInfo;
    Property StringFileInfo: TVersionStringFileInfo Read GetStringFileInfo;
    Property VarFileInfo: TVersionVarFileInfo Read GetVarFileInfo;
  End;


Var
  FInfo: TVersionInfo;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   CreateInfo()
 *
 @ INFO:
 * Erzeugt eine Instanz von TVersionInfo, falls noch keine erzeugt wurde.
 *
 * ---------------------------------------------------------------------------*)
Procedure CreateInfo;
Begin
  If Not Assigned(FInfo) Then
  Begin
    FInfo := TVersionInfo.Create;
    FInfo.Load(HINSTANCE);
  End;
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   VersionMajor()
 @ RESULT: Major-Version der Anwendung
 *
 @ INFO:
 * Gibt die Major-Version der Anwendung zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function VersionMajor: Integer;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := FInfo.FixedInfo.FileVersion[0]
  Else
    Result := 0;
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   VersionMinor()
 @ RESULT: Minor-Version der Anwendung
 *
 @ INFO:
 * Gibt die Minor-Version der Anwendung zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function VersionMinor: Integer;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := FInfo.FixedInfo.FileVersion[1]
  Else
    Result := 0;
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   VersionRelease()
 @ RESULT: Release-Version der Anwendung
 *
 @ INFO:
 * Gibt die Release-Version der Anwendung zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function VersionRelease: Integer;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := FInfo.FixedInfo.FileVersion[2]
  Else
    Result := 0;
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetResourceStrings()
 @ PARAM:  oStringList: String-Liste die befuellt werden soll
 @ RESULT: True bei Erfolg, sonst False
 *
 @ INFO:
 * Fuellt eine String-Liste mit den Strings aus der Ressource der Anwendung.
 *
 * ---------------------------------------------------------------------------*)
Function GetResourceStrings(oStringList: TStringList): Boolean;
Var
  i, j   : Integer;
  oTable : TVersionStringTable;
begin
  CreateInfo;

  oStringList.Clear;
  Result := False;

  If FInfo.BuildInfoAvailable Then
  Begin
    Result := True;
    For i := 0 To FInfo.StringFileInfo.Count-1 Do
    Begin
      oTable := FInfo.StringFileInfo.Items[i];

      For j := 0 To oTable.Count - 1 Do
        If Trim(oTable.ValuesByIndex[j]) <> '' Then
          oStringList.Values[oTable.Keys[j]] := oTable.ValuesByIndex[j];
    end;
  end;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   ProductVersionToString()
 @ PARAM:  PV: Array mit Versionsnummern
 @ RESULT: Versions-String
 *
 @ INFO:
 * Gibt den zusammengesetzten String mit den Versionsnummern zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function ProductVersionToString(PV: TFileProductVersion): String;
Begin
  Result := Format('%d.%d.%d.%d', [PV[0], PV[1], PV[2], PV[3]]);
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetProductVersion()
 @ RESULT: Versionsstring des Produkts
 *
 @ INFO:
 * Gibt den Versionsstring des Produkts zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function GetProductVersion: String;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := ProductVersionToString(FInfo.FixedInfo.ProductVersion)
  Else
    Result := 'No build information available';
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetFileVersion()
 @ RESULT: Versionsstring der Anwendung
 *
 @ INFO:
 * Gibt den Versionsstring der Anwendung zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function GetFileVersion: String;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := ProductVersionToString(FInfo.FixedInfo.FileVersion)
  Else
    Result := 'No build information available';
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetFileDescription()
 @ RESULT: String mit den Daten der Programmbeschreibung
 *
 @ INFO:
 * Gibt einen String mit den Daten der Programmbeschreibung zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function GetFileDescription: String;
Var
  oStringList: TStringList;
Begin
  CreateInfo;
  Result := '';
  oStringList := TStringList.Create;
  If FInfo.BuildInfoAvailable Then
  begin
    if GetResourceStrings(oStringList) then
    begin
      Result := oStringList.Values['FileDescription'];
    end;
  end
  Else
    Result := 'No build information available';
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetComments()
 @ RESULT: String mit den Daten des Kommantars
 *
 @ INFO:
 * Gibt einen String mit den Daten des Kommantars zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function GetComments: String;
Var
  oStringList: TStringList;
Begin
  CreateInfo;
  Result := '';
  oStringList := TStringList.Create;
  If FInfo.BuildInfoAvailable Then
  begin
    if GetResourceStrings(oStringList) then
    begin
      Result := oStringList.Values['Comments'];
    end;
  end
  Else
    Result := 'No build information available';
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetCopyright()
 @ RESULT: String mit den Daten des Copyright
 *
 @ INFO:
 * Gibt einen String mit den Daten des Copyright zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function GetCopyright: String;
Var
  oStringList: TStringList;
Begin
  CreateInfo;
  Result := '';
  oStringList := TStringList.Create;
  If FInfo.BuildInfoAvailable Then
  begin
    if GetResourceStrings(oStringList) then
    begin
      Result := oStringList.Values['LegalCopyright'];
    end;
  end
  Else
    Result := 'No build information available';
End;


{ TVersionInfo }

(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetFixedInfo()
 @ RESULT: Zugriff auf die Struktur FixedInfo
 *
 @ INFO:
 * Gibt den Zugriff auf die Struktur FixedInfo zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
Begin
  Result := FVersResource.FixedInfo;
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetStringFileInfo()
 @ RESULT: Zugriff auf die Struktur StringFileInfo
 *
 @ INFO:
 * Gibt den Zugriff auf die Struktur StringFileInfo zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
Begin
  Result := FVersResource.StringFileInfo;
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetVarFileInfo()
 @ RESULT: Zugriff auf die Struktur VarFileInfo
 *
 @ INFO:
 * Gibt den Zugriff auf die Struktur VarFileInfo zurueck.
 *
 * ---------------------------------------------------------------------------*)
Function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
Begin
  Result := FVersResource.VarFileInfo;
End;


{-------------------------------------------------------------------------------
  Class Create in TVersionInfo
 ------------------------------------------------------------------------------}
Constructor TVersionInfo.Create;
Begin
  Inherited Create;

  FVersResource := TVersionResource.Create;
  FBuildInfoAvailable := False;
End;


{-------------------------------------------------------------------------------
  Class Destroy in TVersionInfo
 ------------------------------------------------------------------------------}
Destructor TVersionInfo.Destroy;
Begin
  FVersResource.Free;

  Inherited Destroy;
End;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   Load()
 @ PARAM:  Instance: HINSTANCE
 *
 @ INFO:
 * Laden der VersionInfo aus der Ressource von HINSTANCE.
 *
 * ---------------------------------------------------------------------------*)
Procedure TVersionInfo.Load(Instance: THandle);
Var
  Stream: TResourceStream;
  ResID: Integer;
  Res: TFPResourceHandle;
Begin
  FBuildInfoAvailable := False;
  ResID := 1;

  // Defensive code to prevent failure if no resource available...
  Res := FindResource(Instance, {%H-}PChar(PtrInt(ResID)), {%H-}PChar(RT_VERSION));
  If Res = 0 Then
    Exit;

  Stream := TResourceStream.CreateFromID(Instance, ResID, {%H-}PChar(RT_VERSION));
  Try
    FVersResource.SetCustomRawDataStream(Stream);

    // access some property to load from the stream
    FVersResource.FixedInfo;

    // clear the stream
    FVersResource.SetCustomRawDataStream(nil);

    FBuildInfoAvailable := True;
  Finally
    Stream.Free;
  End;
End;


Initialization
  FInfo := nil;


Finalization
  If Assigned(FInfo) Then FInfo.Free;


End.


