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
unit CnfSyneditOptions;

{$IFDEF Windows}
  {$IFnDEF WithoutWinIME}
    {$DEFINE WinIME}
  {$ENDIF}
{$ENDIF}

interface

uses
  // RTL, FCL
  Classes, SysUtils, TypInfo,
  // LCL
  Graphics, LCLProc, LResources, Forms, Dialogs, ComCtrls,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8, LazStringUtils,
  // Synedit
  SynEdit, SynEditAutoComplete, SynEditKeyCmds, SynEditTypes,
  SynEditMiscClasses, SynBeautifier, SynEditTextTrimmer, SynEditMouseCmds,
  SynPluginTemplateEdit, SynPluginSyncroEdit,
  SynGutter, SynGutterBase, SynGutterCodeFolding, SynGutterLineNumber,
  SynCompletion, SynEditMarkupBracket, SynEditMarkupHighAll,
  SynEditHighlighter, SynEditHighlighterFoldBase,
  SynHighlighterHTML, SynHighlighterCss, SynHighlighterXML,
  SynHighlighterJScript, SynEditMarkupFoldColoring, SynEditMarkup,
  // Spellcheck
  SynSpellcheckHighlighter,
  // Cnf
  CnfSyneditHighlighterDef, Storage,
  // Diverse
  Language, FormDebug, LCLType;

const
  DefaultCompletionLongLineHintType = sclpExtendRightOnly;
  DefaultEditorDisableAntiAliasing = false;

  CrLf = LineEnding;

type

  TCmpStrType =
  (
    cstCaseSensitive,
    cstCaseInsensitive,
    cstFilename
  );

type
 TPreviewMdSyn  = TSynSpellMdSyn;
  TSrcIDEHighlighter = TSynCustomHighlighter;
  TSynHighlightElement = TSynHighlighterAttributes;
  TCustomSynClass = class of TSrcIDEHighlighter;

  TLazSynPluginTemplateMultiCaret = class(TForm)   end;
  TLazSynPluginTemplateEditForm = class(TForm)     end;
  TLazSynPluginTemplateEditFormOff = class(TForm)  end;
  TLazSynPluginSyncroEditFormSel = class(TForm)    end;
  TLazSynPluginSyncroEditForm = class(TForm)       end;
  TLazSynPluginSyncroEditFormOff = class(TForm)    end;

  TColorSchemeAttributeFeature =
  (
    hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior,
    hafStyle, hafStyleMask,
    hafFrameStyle, hafFrameEdges,
    hafMarkupFoldColor // for the MarkupFoldColor module
  );
  TColorSchemeAttributeFeatures = set of TColorSchemeAttributeFeature;

type
  TAdditionalHilightAttribute =
  (
    ahaNone,              ahaTextBlock,          ahaExecutionPoint,
    ahaEnabledBreakpoint, ahaDisabledBreakpoint, ahaInvalidBreakpoint,
    ahaUnknownBreakpoint, ahaErrorLine,          ahaIncrementalSearch,
    ahaHighlightAll,      ahaBracketMatch,       ahaMouseLink,
    ahaLineNumber,        ahaLineHighlight,      ahaModifiedLine,
    ahaCodeFoldingTree,   ahaHighlightWord,      ahaFoldedCode,
    ahaFoldedCodeLine,    ahaHiddenCodeLine,
    ahaWordGroup,         ahaTemplateEditCur,    ahaTemplateEditSync,
    ahaTemplateEditOther, ahaSyncroEditCur,      ahaSyncroEditSync,
    ahaSyncroEditOther,   ahaSyncroEditArea,     ahaGutterSeparator,
    ahaGutter,            ahaRightMargin,        ahaSpecialVisibleChars,
    ahaTopInfoHint,       ahaCaretColor,
    ahaIfDefBlockInactive, ahaIfDefBlockActive, ahaIfDefBlockTmpActive,
    ahaIfDefNodeInactive, ahaIfDefNodeActive, ahaIfDefNodeTmpActive,
    ahaIdentComplWindow, ahaIdentComplWindowBorder, ahaIdentComplWindowSelection, ahaIdentComplWindowHighlight,
    ahaOutlineLevel1Color, ahaOutlineLevel2Color, ahaOutlineLevel3Color, ahaOutlineLevel4Color, ahaOutlineLevel5Color, ahaOutlineLevel6Color, ahaOutlineLevel7Color, ahaOutlineLevel8Color, ahaOutlineLevel9Color, ahaOutlineLevel10Color
  );

type
  TAhaGroupName = (
    agnDefault, agnLanguage, agnText, agnLine, agnGutter, agnTemplateMode, agnSyncronMode,
    agnIfDef, agnIdentComplWindow, agnOutlineColors
  );

const
  SynEditPreviewIncludeOptions = [eoNoCaret, eoNoSelection];
  SynEditPreviewExcludeOptions = [eoDragDropEditing, eoDropFiles,
                                  eoScrollPastEof];
  SynEditPreviewIncludeOptions2 = [];
  SynEditPreviewExcludeOptions2 = [eoAlwaysVisibleCaret];

  DefaultCodeTemplatesFilename = 'lazarus.dci'; // in directory GetPrimaryConfigPath

  // Do not localize: those are used for the config XML
  ahaXmlNames: array[TAdditionalHilightAttribute] of String =
  (
    '',                    'Text block',                'Execution point',
    'Enabled breakpoint',  'Disabled breakpoint',       'Invalid breakpoint',
    'Unknown breakpoint',  'Error line',                'Incremental search match',
    'Highlight all',       'Brackets highlight',        'Mouse link',
    'Line number',         'Line highlight',            'Modified line',
    'Code folding tree',   'Highlight current word',    'Folded code',
    'Folded code Line',    'Hidden code Line',
    'Word-Brackets',       'TemplateEdit Current',      'TemplateEdit Sync',
    'TemplateEdit Cells',  'SyncronEdit Current Cells', 'SyncronEdit Syncron Cells',
    'SyncronEdit Other Cells', 'SyncronEdit Range',
    '', // scaGutterSeparator => uses RTTI only
    '', // ahaGutter
    '', // ahaRightMargin
    '', // ahaSpecialVisibleChars
    '', // ahaTopInfoHint
    '', // ahaCaretColor
    '', '', '',  // ahaIfDefBlockInactive, ahaIfDefBlockActive, ahaIfDefBlockTmpActive
    '', '', '',  // ahaIfDefNodeInactive, ahaIfDefNodeActive, ahaIfDefNodeTmpActive
    '', '', '', '', // ahaIdentComplWindow, ahaIdentComplWindowBorder, ahaIdentComplWindowSelection, ahaIdentComplWindowHighlight
    '', '', '', '', '', '', '', '', '', '' // ahaOutlineLevel1Color..ahaOutlineLevel10Color
  );

  ahaGroupMap: array[TAdditionalHilightAttribute] of TAhaGroupName = (
    { ahaNone }                agnText,
    { ahaTextBlock }           agnText,
    { ahaExecutionPoint }      agnLine,
    { ahaEnabledBreakpoint }   agnLine,
    { ahaDisabledBreakpoint }  agnLine,
    { ahaInvalidBreakpoint }   agnLine,
    { ahaUnknownBreakpoint }   agnLine,
    { ahaErrorLine }           agnLine,
    { ahaIncrementalSearch }   agnText,
    { ahaHighlightAll }        agnText,
    { ahaBracketMatch }        agnText,
    { ahaMouseLink }           agnText,
    { ahaLineNumber }          agnGutter,
    { ahaLineHighlight }       agnLine,
    { ahaModifiedLine }        agnGutter,
    { ahaCodeFoldingTree }     agnGutter,
    { ahaHighlightWord }       agnText,
    { ahaFoldedCode }          agnGutter,
    { ahaFoldedCodeLine }      agnGutter,
    { ahaHiddenCodeLine }      agnGutter,
    { ahaWordGroup }           agnText,
    { ahaTemplateEditCur }     agnTemplateMode,
    { ahaTemplateEditSync }    agnTemplateMode,
    { ahaTemplateEditOther }   agnTemplateMode,
    { ahaSyncroEditCur }       agnSyncronMode,
    { ahaSyncroEditSync }      agnSyncronMode,
    { ahaSyncroEditOther }     agnSyncronMode,
    { ahaSyncroEditArea }      agnSyncronMode,
    { ahaGutterSeparator }     agnGutter,
    { ahaGutter }              agnGutter,
    { ahaRightMargin}          agnGutter,
    { ahaSpecialVisibleChars } agnText,
    { ahaTopInfoHint }         agnLine,
    { ahaCaretColor }          agnText,
    { ahaIfDefBlockInactive }  agnIfDef,
    { ahaIfDefBlockActive }    agnIfDef,
    { ahaIfDefBlockTmpActive } agnIfDef,
    { ahaIfDefNodeInactive }   agnIfDef,
    { ahaIfDefNodeActive }     agnIfDef,
    { ahaIfDefNodeTmpActive }  agnIfDef,
    { ahaIdentComplWindow }           agnIdentComplWindow,
    { ahaIdentComplWindowBorder }     agnIdentComplWindow,
    { ahaIdentComplWindowSelection }  agnIdentComplWindow,
    { ahaIdentComplWindowHighlight }  agnIdentComplWindow,
    { ahaOutlineLevel1Color }  agnOutlineColors,
    { ahaOutlineLevel2Color }  agnOutlineColors,
    { ahaOutlineLevel3Color }  agnOutlineColors,
    { ahaOutlineLevel4Color }  agnOutlineColors,
    { ahaOutlineLevel5Color }  agnOutlineColors,
    { ahaOutlineLevel6Color }  agnOutlineColors,
    { ahaOutlineLevel7Color }  agnOutlineColors,
    { ahaOutlineLevel8Color }  agnOutlineColors,
    { ahaOutlineLevel9Color }  agnOutlineColors,
    { ahaOutlineLevel10Color } agnOutlineColors

  );
  ahaSupportedFeatures: array[TAdditionalHilightAttribute] of TColorSchemeAttributeFeatures =
  (
    { ahaNone }               [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaTextBlock }          [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaExecutionPoint }     [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaEnabledBreakpoint }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaDisabledBreakpoint } [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaInvalidBreakpoint }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaUnknownBreakpoint }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaErrorLine }          [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIncrementalSearch }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaHighlightAll }       [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaBracketMatch }       [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaMouseLink }          [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaLineNumber }         [hafBackColor, hafForeColor, hafFrameColor, hafStyle],
    { ahaLineHighlight }      [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaModifiedLine }       [hafBackColor, hafForeColor, hafFrameColor],
    { ahaCodeFoldingTree }    [hafBackColor, hafForeColor, hafFrameColor],
    { ahaHighlightWord }      [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaFoldedCode }         [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaFoldedCodeLine }     [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaHiddenCodeLine }     [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaWordGroup }          [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaTemplateEditCur }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaTemplateEditSync }   [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaTemplateEditOther }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaSyncroEditCur }      [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaSyncroEditSync }     [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaSyncroEditOther }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaSyncroEditArea }     [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaGutterSeparator }    [hafBackColor, hafForeColor],
    { ahaGutter }             [hafBackColor],
    { ahaRightMargin}         [hafForeColor],
    { ahaSpecialVisibleChars }[hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaTopInfoHint }        [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaCaretColor }         [hafBackColor, hafForeColor],
    { ahaIfDefBlockInactive } [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefBlockActive }   [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefBlockTmpActive }[hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefNodeInactive }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefNodeActive }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefNodeTmpActive } [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIdentComplWindow }   [hafBackColor, hafForeColor],
    { ahaIdentComplWindowBorder }    [hafForeColor],
    { ahaIdentComplWindowSelection } [hafBackColor, hafForeColor],
    { ahaIdentComplWindowHighlight } [hafForeColor],
    { ahaFoldLevel1Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel2Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel3Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel4Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel5Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel6Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel7Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel8Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel9Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel10Color }   [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor]
  );


var
  AdditionalHighlightAttributes: array[TAdditionalHilightAttribute] of String;
  AdditionalHighlightGroupNames: array[TAhaGroupName] of String;

type
  (* ***  ColorSchemes  *** *)

  { TQuickStringlist }

  TQuickStringlist=class(TStringlist)
    Function DoCompareText(const s1,s2 : string) : PtrInt; override;
  end;

  TColorScheme = class;
  TColorSchemeLanguage = class;

  { TColorSchemeAttribute }

  TColorSchemeAttribute = class(TSynHighlighterAttributesModifier)
  private
    FFeatures: TColorSchemeAttributeFeatures;
    FGroup: TAhaGroupName;
    FMarkupFoldLineAlpha: Byte;
    FMarkupFoldLineColor: TColor;
    FMarkupFoldLineStyle: TSynLineStyle;
    FOwner: TColorSchemeLanguage;
    FUseSchemeGlobals: Boolean;
    function GetIsUsingSchemeGlobals: Boolean;
    function OldAdditionalAttributeName(NewAha: String): string;
    procedure SetMarkupFoldLineAlpha(AValue: Byte);
    procedure SetMarkupFoldLineColor(AValue: TColor);
    procedure SetMarkupFoldLineStyle(AValue: TSynLineStyle);
  protected
    procedure Init; override;
  public
    constructor Create(ASchemeLang: TColorSchemeLanguage; attribName: PString; aStoredName: String = '');
    function IsEnabled: boolean; override;
    procedure ApplyTo(aDest: TSynHighlighterAttributes; aDefault: TColorSchemeAttribute = nil);
    procedure Assign(Src: TPersistent); override;
    function Equals(Other: TColorSchemeAttribute): Boolean; reintroduce;
    function GetStoredValuesForAttrib: TColorSchemeAttribute; // The IDE default colors from the resources
    function GetSchemeGlobal: TColorSchemeAttribute;
    procedure LoadFromXml(aSettings: TAppStorage; aPath: String;
                          Defaults: TColorSchemeAttribute; Version: Integer);
    procedure LoadFromXmlV1(aSettings: TAppStorage; aPath: String;
                            Defaults: TColorSchemeAttribute);
    procedure SaveToXml(aSettings: TAppStorage; aPath: String;
                        Defaults: TColorSchemeAttribute);
    property Group: TAhaGroupName read FGroup write FGroup;
    property IsUsingSchemeGlobals: Boolean read GetIsUsingSchemeGlobals;
    property Features: TColorSchemeAttributeFeatures read FFeatures write FFeatures;
  published
    property UseSchemeGlobals: Boolean read FUseSchemeGlobals write FUseSchemeGlobals;
    // For markup fold color
    property MarkupFoldLineColor: TColor read FMarkupFoldLineColor write SetMarkupFoldLineColor default clNone; // clDefault will take Color[].Frame or Color[].Foreground
    property MarkupFoldLineStyle: TSynLineStyle read FMarkupFoldLineStyle write SetMarkupFoldLineStyle default slsSolid;
    property MarkupFoldLineAlpha: Byte read FMarkupFoldLineAlpha write SetMarkupFoldLineAlpha default 0;
  end;

  { TColorSchemeLanguage }

  TColorSchemeLanguage = class(TObject)
  private
    FDefaultAttribute: TColorSchemeAttribute;
    FAttributes: TQuickStringlist; // TColorSchemeAttribute
    FHighlighter: TSynCustomHighlighter;
    FLanguage: TLazSyntaxHighlighter;
    FOwner: TColorScheme;
    FLanguageName: String;
    FIsSchemeDefault: Boolean;
    FFormatVersion: integer;
    function GetAttribute(Index: String): TColorSchemeAttribute;
    function GetAttributeAtPos(Index: Integer): TColorSchemeAttribute;
    function GetAttributeByEnum(Index: TAdditionalHilightAttribute): TColorSchemeAttribute;
    function GetName: String;
    function AhaToStoredName(aha: TAdditionalHilightAttribute): String;
  public
    constructor Create(const AGroup: TColorScheme; const ALang: TLazSyntaxHighlighter;
                       IsSchemeDefault: Boolean = False);
    constructor CreateFromXml(const AGroup: TColorScheme; const ALang: TLazSyntaxHighlighter;
                              aSettings: TAppStorage; aPath: String;
                              IsSchemeDefault: Boolean = False);
    destructor  Destroy; override;
    procedure Clear;
    procedure Assign(Src: TColorSchemeLanguage); reintroduce;
    function Equals(Other: TColorSchemeLanguage): Boolean; reintroduce;
    function GetStoredValuesForLanguage: TColorSchemeLanguage; // The IDE default colors from the resources
    function IndexOfAttr(AnAttr: TColorSchemeAttribute): Integer;
    procedure LoadFromXml(aSettings: TAppStorage; aPath: String; Defaults: TColorSchemeLanguage;
              ColorVersion: Integer; aOldPath: String = '');
    procedure SaveToXml(aSettings: TAppStorage; aPath: String; Defaults: TColorSchemeLanguage);
    procedure ApplyTo(ASynEdit: TSynEdit); // Write markup, etc
    procedure ApplyColorsTo(ASynEdit: TSynEdit); // Write markup, etc
    procedure ApplyTo(AHLighter: TSynCustomHighlighter);
    function  AttributeCount: Integer;
    property  Name: String read GetName;
    property  Language: TLazSyntaxHighlighter read FLanguage;
    property  LanguageName: String read FLanguageName;
    property  Attribute[Index: String]: TColorSchemeAttribute read GetAttribute;
    property  AttributeByEnum[Index: TAdditionalHilightAttribute]: TColorSchemeAttribute
              read GetAttributeByEnum;
    property  AttributeAtPos[Index: Integer]: TColorSchemeAttribute read GetAttributeAtPos;
    property  DefaultAttribute: TColorSchemeAttribute read FDefaultAttribute;
    property  Highlighter: TSynCustomHighlighter read FHighlighter;
  end;

  { TColorScheme }

  TColorScheme = class(TObject)
  private
    FName: String;
    FColorSchemes: Array [TLazSyntaxHighlighter] of TColorSchemeLanguage;
    FDefaultColors: TColorSchemeLanguage;
    function GetColorScheme(Index: TLazSyntaxHighlighter): TColorSchemeLanguage;
    function GetColorSchemeBySynClass(Index: TClass): TColorSchemeLanguage;
  public
    constructor Create(AName: String);
    constructor CreateFromXml(aSettings: TAppStorage; const AName, aPath: String);
    destructor  Destroy; override;
    procedure Assign(Src: TColorScheme); reintroduce;
    function GetStoredValuesForScheme: TColorScheme; // The IDE default colors from the resources
    procedure LoadFromXml(aSettings: TAppStorage; aPath: String; Defaults: TColorScheme; aOldPath: String = '');
    procedure LoadSchemeFromXml(aSettings: TAppStorage; aPath: String; Defaults: TColorScheme; {%H-}aScheme: String);
    procedure SaveToXml(aSettings: TAppStorage; aPath: String; Defaults: TColorScheme);
    property  Name: string read FName write FName;
    property  DefaultColors: TColorSchemeLanguage read FDefaultColors;
    property  ColorScheme[Index: TLazSyntaxHighlighter]: TColorSchemeLanguage read GetColorScheme;
    property  ColorSchemeBySynClass[Index: TClass]: TColorSchemeLanguage read GetColorSchemeBySynClass;
  end;

  { TColorSchemeFactory }

  TColorSchemeFactory = class(TObject)
  private
    FMappings: TQuickStringlist; // TColorScheme
    function GetColorSchemeGroup(Index: String): TColorScheme;
    function GetColorSchemeGroupAtPos(Index: Integer): TColorScheme;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Clear;
    procedure Assign(Src: TColorSchemeFactory); reintroduce;
    procedure LoadFromXml(aSettings: TAppStorage; aPath: String; Defaults: TColorSchemeFactory; aOldPath: String = '');
    procedure LoadSchemeFromXml(aSettings: TAppStorage; aPath: String; aDefault, aScheme: String);
    procedure SaveToXml(aSettings: TAppStorage; aPath: String; Defaults: TColorSchemeFactory);
    procedure RegisterScheme(aSettings: TAppStorage; AName, aPath: String);
    procedure GetRegisteredSchemes(AList: TStrings);
    property  ColorSchemeGroup[Index: String]: TColorScheme read GetColorSchemeGroup;
    property  ColorSchemeGroupAtPos[Index: Integer]: TColorScheme read GetColorSchemeGroupAtPos;
  end;

type

  EditorOldOptionsDividerInfo = record
    Name: String;      // Name for display
    Xml: String;       // Name for XML
    BoolOpt: Boolean;  // Checkbox only
    MaxLevel: Integer;
  end;
  EditorOldOptionsDividerInfoList = Array [0..999] of EditorOldOptionsDividerInfo;
  PEditorOptionsDividerInfoList = ^EditorOldOptionsDividerInfoList;

  EditorOldOptionsDividerRecord = record
    Count: Integer;
    Info: PEditorOptionsDividerInfoList;
  end;


const
  dlgConstDivPasUnitSectionName   = 'Unit sections';
  dlgConstDivPasUsesName          = 'Uses clause';
  dlgConstDivPasVarGlobalName     = 'Var/Type';
  dlgConstDivPasVarLocalName      = 'Var/Type (local)';
  dlgConstDivPasStructGlobalName  = 'Class/Struct';
  dlgConstDivPasStructLocalName   = 'Class/Struct (local)';
  dlgConstDivPasProcedureName     = 'Procedure/Function';
  dlgConstDivPasBeginEndName      = 'Begin/End';
  dlgConstDivPasTryName           = 'Try/Except';

var
  dlgStrAddHiAttrDefault: String  = 'Default Text';

  (* When adding new entries, ensure that resourcestrings are re-assigned in InitLocale *)
  EditorOptionsDividerInfoPas: Array [0..8] of EditorOldOptionsDividerInfo
  = (
      (Name: dlgConstDivPasUnitSectionName;  Xml: 'Sect';    BoolOpt: True;  MaxLevel: 1),
      (Name: dlgConstDivPasUsesName;         Xml: 'Uses';    BoolOpt: True;  MaxLevel: 0),
      (Name: dlgConstDivPasVarGlobalName;    Xml: 'GVar';    BoolOpt: True;  MaxLevel: 1),
      (Name: dlgConstDivPasVarLocalName;     Xml: 'LVar';    BoolOpt: False; MaxLevel: 0),
      (Name: dlgConstDivPasStructGlobalName; Xml: 'GStruct'; BoolOpt: False; MaxLevel: 1),
      (Name: dlgConstDivPasStructLocalName;  Xml: 'LStruct'; BoolOpt: False; MaxLevel: 0),
      (Name: dlgConstDivPasProcedureName;    Xml: 'Proc';    BoolOpt: False; MaxLevel: 1),
      (Name: dlgConstDivPasBeginEndName;     Xml: 'Begin';   BoolOpt: False; MaxLevel: 0),
      (Name: dlgConstDivPasTryName;          Xml: 'Try';     BoolOpt: False; MaxLevel: 0)
    );

const

  (* When adding new entries, ensure that resourcestrings are re-assigned in InitLocale *)
  EditorOptionsDividerDefaults: array[TLazSyntaxHighlighter] of
    EditorOldOptionsDividerRecord =
    ( (Count: 0; Info: nil), // none
      (Count: 0; Info: nil), // text
      (Count: 0; Info: nil), // md
      (Count: 0; Info: nil), // html
      (Count: 0; Info: nil), // css
      (Count: 0; Info: nil), // xml
      (Count: 0; Info: nil) // jscript
    );

type

  EditorOldOptionsFoldInfo = record
    Name: String;      // Name for display
    Xml: String;       // Name for XML
    Index: Integer;    // FHighlighter.FoldConf[index]
    Enabled: Boolean;
  end;
  EditorOldOptionsFoldInfoList = Array [0..999] of EditorOldOptionsFoldInfo;
  PEditorOptionsFoldInfoList = ^EditorOldOptionsFoldInfoList;

  EditorOldOptionsFoldRecord = record
    Count: Integer;
    HasMarkup: Boolean;
    Info: PEditorOptionsFoldInfoList;
  end;

type

  { TSynEditMouseActionKeyCmdHelper }

  TSynEditMouseActionKeyCmdHelper = class(TSynEditMouseAction)
  private
    function GetOptionKeyCmd: TSynEditorCommand;
    procedure SetOptionKeyCmd(const AValue: TSynEditorCommand);
  published
    property Option: TSynEditorCommand read GetOptionKeyCmd write SetOptionKeyCmd;
  end;

const
  dlgConstFoldPasProcedure       = 'Procedure';
  dlgConstFoldLocalPasVarType    = 'Var/Type (local)';
  dlgConstFoldPasProcBeginEnd    = 'Begin/End (procedure)';
  dlgConstFoldPasBeginEnd        = 'Begin/End (nested)';
  dlgConstFoldPasRepeat          = 'Repeat';
  dlgConstFoldPasCase            = 'Case';
  dlgConstFoldPasTry             = 'Try';
  dlgConstFoldPasExcept          = 'Except/Finally';
  dlgConstFoldPasAsm             = 'Asm';
  dlgConstFoldPasProgram         = 'Program';
  dlgConstFoldPasUnit            = 'Unit';
  dlgConstFoldPasUnitSection     = 'Unit section';
  dlgConstFoldPasUses            = 'Uses';
  dlgConstFoldPasVarType         = 'Var/Type (global)';
  dlgConstFoldPasClass           = 'Class/Object';
  dlgConstFoldPasClassSection    = 'public/private';

  dlgConstFoldPasRecord          = 'Record';
  dlgConstFoldPasIfDef           = '{$IfDef}';
  dlgConstFoldPasUserRegion      = '{%Region}';
  dlgConstFoldPasAnsiComment     = 'Comment (* *)';
  dlgConstFoldPasBorComment      = 'Comment { }';
  dlgConstFoldPasSlashComment    = 'Comment //';
  dlgConstFoldPasNestedComment   = 'Nested Comment';
  dlgConstFoldPasIfThen          = 'If/Then/Else';
  dlgConstFoldPasForDo           = 'For/Do';
  dlgConstFoldPasWhileDo         = 'While/Do';
  dlgConstFoldPasWithDo          = 'With/Do';

  dlgConstFoldLfmObject          = 'Object (inherited, inline)';
  dlgConstFoldLfmList            = 'List <>';
  dlgConstFoldLfmItem            = 'Item';

  dlgConstFoldXmlNode            = 'Node';
  dlgConstFoldXmlComment         = 'Comment';
  dlgConstFoldXmlCData           = 'CData';
  dlgConstFoldXmlDocType         = 'DocType';
  dlgConstFoldXmlProcess         = 'Processing Instruction';

  dlgConstFoldHtmlNode           = 'Node';
  dlgConstFoldHtmlComment        = 'Comment';
  dlgConstFoldHtmlAsp            = 'ASP';
  lisConstFile                   = 'File';
  dlgConstFoldDiffChunk          = 'Chunk';
  dlgConstFoldDiffChunkSect      = 'Chunk section';




const

  EditorOptionsFoldInfoXML: Array [0..4] of EditorOldOptionsFoldInfo
  = (
      ( Name:    dlgConstFoldXmlNode;
        Xml:    'Node';
        Index:   ord(cfbtXmlNode);
        Enabled: True
      ),
      ( Name:    dlgConstFoldXmlComment;
        Xml:    'Comment';
        Index:   ord(cfbtXmlComment);
        Enabled: True
      ),
      ( Name:    dlgConstFoldXmlCData;
        Xml:    'CData';
        Index:   ord(cfbtXmlCData);
        Enabled: True
      ),
      ( Name:    dlgConstFoldXmlDocType;
        Xml:    'DocType';
        Index:   ord(cfbtXmlDocType);
        Enabled: True
      ),
      ( Name:    dlgConstFoldXmlProcess;
        Xml:    'ProcessInstr';
        Index:   ord(cfbtXmlProcess);
        Enabled: True
      )
    );

  EditorOptionsFoldInfoHTML: Array [0..2] of EditorOldOptionsFoldInfo
  = (
      ( Name:    dlgConstFoldHtmlNode;
        Xml:    'Node';
        Index:   ord(cfbtHtmlNode);
        Enabled: True
      ),
      ( Name:    dlgConstFoldHtmlComment;
        Xml:    'Comment';
        Index:   ord(cfbtXmlComment);
        Enabled: True
      ),
      ( Name:    dlgConstFoldHtmlAsp;
        Xml:    'ASP';
        Index:   ord(cfbtHtmlAsp);
        Enabled: True
      )
    );

  (* When adding new entries, ensure that resourcestrings are re-assigned in InitLocale *)
  EditorOptionsFoldDefaults: array[TLazSyntaxHighlighter] of
    EditorOldOptionsFoldRecord =
    ( (Count:  0; HasMarkup: False; Info: nil), // none
      (Count:  0; HasMarkup: False; Info: nil), // text
      (Count:  0; HasMarkup: False; Info: nil), // md
      (Count:  3; HasMarkup: True; Info: @EditorOptionsFoldInfoHTML[0]), // html
      (Count:  0; HasMarkup: False; Info: nil), // css
      (Count:  5; HasMarkup: True; Info: @EditorOptionsFoldInfoXML[0]), // xml
      (Count:  0; HasMarkup: False; Info: nil) // jscript
    );

const
  EditorOptsFormatVersion = 12;
  EditorMouseOptsFormatVersion = 1;

  LazSyntaxHighlighterClasses: array[TLazSyntaxHighlighter] of
    TCustomSynClass =
    (nil, nil, TSynSpellMdSyn, TSynHTMLSyn, TSynCssSyn, TSynXMLSyn,
    TSynJScriptSyn);


{ Comments }
const
  DefaultCommentTypes: array[TLazSyntaxHighlighter] of TCommentType = (
    comtNone,  // lshNone
    comtNone,  // lshText
    comtMd,    // lshMarkdown
    comtHtml,  // lshHTML
    comtCPP,   // lshCss
    comtHtml,  // lshXML
    comtCPP   // lshJScript
    );

const
  SynEditDefaultOptions = SYNEDIT_DEFAULT_OPTIONS - [eoShowScrollHint]
                                                  + [eoHalfPageScroll, eoTabIndent];
  SynEditDefaultOptions2 = SYNEDIT_DEFAULT_OPTIONS2;

  EditorOptionsMinimumFontSize = 5;

type

  TEditOptLanguageInfo = class
  private
    MappedAttributes: TStringList; // map attributes to pascal
  protected
    procedure prepare(Syntax :  TLazSyntaxHighlighter); virtual;
  public
    SynClass: TCustomSynClass;
    TheType:  TLazSyntaxHighlighter;
    FileExtensions: String; // divided by semicolon, e.g. 'pas;pp;inc'
    DefaultFileExtensions: string;
    ColorScheme: String;
    SampleSource: String;
    AddAttrSampleLines: array[TAdditionalHilightAttribute] of Integer; // first line = 1
    DefaultCommentType: TCommentType;
    CaretXY: TPoint;
    constructor Create;
    destructor Destroy; override;
    function GetDefaultFilextension: String;
    procedure SetBothFilextensions(const Extensions: string);
    function SampleLineToAddAttr(Line: Integer): TAdditionalHilightAttribute;
  end;


  { TEditOptLangMdInfo }

  TEditOptLangMdInfo = class(TEditOptLanguageInfo)
  protected
     procedure prepare(Syntax :  TLazSyntaxHighlighter); override;
     function getSampleSource:string;
     function getMappedAttributes: tStringList;
  end;


  { TEditOptLangCssInfo }

  TEditOptLangCssInfo = class(TEditOptLanguageInfo)
  protected
     procedure prepare(Syntax :  TLazSyntaxHighlighter); override;
     function getSampleSource:string;
     function getMappedAttributes: tStringList;
  end;


  { TEditOptLangHtmlInfo }

  TEditOptLangHtmlInfo = class(TEditOptLanguageInfo)
  protected
     procedure prepare(Syntax :  TLazSyntaxHighlighter); override;
     function getSampleSource:string;
     function getMappedAttributes: tStringList;
  end;


  { TEditOptLangXmlInfo }

  TEditOptLangXmlInfo = class(TEditOptLanguageInfo)
  protected
     procedure prepare(Syntax :  TLazSyntaxHighlighter); override;
     function getSampleSource:string;
     function getMappedAttributes: tStringList;
  end;


  { TEditOptLangJsInfo }

  TEditOptLangJsInfo = class(TEditOptLanguageInfo)
  protected
     procedure prepare(Syntax :  TLazSyntaxHighlighter); override;
     function getSampleSource:string;
     function getMappedAttributes: tStringList;
  end;


  { TEditOptLangList }

  TEditOptLangList = class(TList)
  private
    function GetInfos(Index: Integer): TEditOptLanguageInfo;
  public
    constructor Create;
    procedure Clear; override;
    destructor Destroy; override;
    function FindByName(const Name: String): Integer;
    function FindByClass(CustomSynClass: TCustomSynClass): Integer;
    function FindByHighlighter(Hilighter: TSynCustomHighlighter): Integer;
    function FindByType(AType: TLazSyntaxHighlighter): Integer;
    function GetDefaultFilextension(AType: TLazSyntaxHighlighter): String;
    function GetInfoByType(AType: TLazSyntaxHighlighter): TEditOptLanguageInfo;
    property Items[Index: Integer]: TEditOptLanguageInfo read GetInfos; default;
  end;

  TEditorOptions = class;
  TMouseOptGutterLeftType = (
    moGLDownClick,
    moglUpClickAndSelect,
    moglUpClickAndSelectRighHalf   // Changes and fold gutter (parts close to the text)
  );
  TMouseOptButtonActionOld = (
    mbaNone,
    mbaSelect, mbaSelectColumn, mbaSelectLine,
    mbaSelectWords,
    mbaSelectSetWord, mbaSelectSetLineSmart, mbaSelectSetLineFull, mbaSelectSetPara,
    mbaPaste,
    mbaDeclarationJump,
    mbaDeclarationOrBlockJump,
    mbaAddHistoryPoint,
    mbaHistoryBack, mbaHistoryForw,
    mbaSetFreeBookmark,
    mbaZoomReset,
    mbaContextMenu,
    mbaContextMenuDebug,
    mbaContextMenuTab,

    mbaMultiCaretToggle,

    // Old values, needed to load old config
    moTCLNone, moTMIgnore,
    moTMPaste,
    moTMDeclarationJump, moTCLJump,
    moTCLJumpOrBlock
  );

  TMouseOptButtonAction = mbaNone..mbaMultiCaretToggle;

const
  MouseOptButtonActionOld: Array [moTCLNone..moTCLJumpOrBlock] of TMouseOptButtonActionOld = (
    mbaNone, mbaNone,
    mbaPaste,
    mbaDeclarationJump, mbaDeclarationJump,
    mbaDeclarationOrBlockJump
  );

type
  TMouseOptWheelAction = (
    mwaNone,
    mwaScroll, mwaScrollSingleLine,
    mwaScrollPage, mwaScrollPageLessOne, mwaScrollHalfPage,
    mwaScrollHoriz, mwaScrollHorizSingleLine,
    mwaScrollHorizPage, mwaScrollHorizPageLessOne, mwaScrollHorizHalfPage,
    mwaZoom
  );

  { TEditorMouseOptions }

  TEditorMouseOptions = class(TPersistent)
  private
    FGutterLeft: TMouseOptGutterLeftType;
    FTextDrag: Boolean;
    FTextRightMoveCaret: Boolean;
    FUserSchemes: TQuickStringlist;
  private
    FCustomSavedActions: Boolean;
    FGutterActionsChanges: TSynEditMouseActions;
    FMainActions, FSelActions, FTextActions: TSynEditMouseActions;
    FSelectOnLineNumbers: Boolean;
    FName: String;
    FGutterActions: TSynEditMouseActions;
    FGutterActionsFold, FGutterActionsFoldExp, FGutterActionsFoldCol: TSynEditMouseActions;
    FGutterActionsLines: TSynEditMouseActions;
    FGutterActionsOverView, FGutterActionsOverViewMarks: TSynEditMouseActions;
    FSelectedUserScheme: String;
    // left multi click
    FTextDoubleLeftClick: TMouseOptButtonAction;
    FTextTripleLeftClick: TMouseOptButtonAction;
    FTextQuadLeftClick: TMouseOptButtonAction;
    FTextShiftDoubleLeftClick: TMouseOptButtonAction;
    FTextAltDoubleLeftClick: TMouseOptButtonAction;
    FTextCtrlDoubleLeftClick: TMouseOptButtonAction;
    // left + modifier click
    FTextShiftLeftClick: TMouseOptButtonAction;
    FTextAltLeftClick: TMouseOptButtonAction;
    FTextCtrlLeftClick: TMouseOptButtonActionOld;
    FTextAltCtrlLeftClick: TMouseOptButtonAction;
    FTextShiftAltLeftClick: TMouseOptButtonAction;
    FTextShiftCtrlLeftClick: TMouseOptButtonAction;
    FTextShiftAltCtrlLeftClick: TMouseOptButtonAction;
    // middle click
    FTextMiddleClick: TMouseOptButtonActionOld;
    FTextAltMiddleClick: TMouseOptButtonAction;
    FTextCtrlMiddleClick: TMouseOptButtonAction;
    FTextAltCtrlMiddleClick: TMouseOptButtonAction;
    FTextShiftAltMiddleClick: TMouseOptButtonAction;
    FTextShiftAltCtrlMiddleClick: TMouseOptButtonAction;
    FTextShiftCtrlMiddleClick: TMouseOptButtonAction;
    FTextShiftMiddleClick: TMouseOptButtonAction;
    // right
    FTextAltCtrlRightClick: TMouseOptButtonAction;
    FTextAltRightClick: TMouseOptButtonAction;
    FTextCtrlRightClick: TMouseOptButtonAction;
    FTextRightClick: TMouseOptButtonAction;
    FTextShiftAltCtrlRightClick: TMouseOptButtonAction;
    FTextShiftAltRightClick: TMouseOptButtonAction;
    FTextShiftCtrlRightClick: TMouseOptButtonAction;
    FTextShiftRightClick: TMouseOptButtonAction;
    // extra-1 click
    FTextAltCtrlExtra1Click: TMouseOptButtonAction;
    FTextAltExtra1Click: TMouseOptButtonAction;
    FTextCtrlExtra1Click: TMouseOptButtonAction;
    FTextExtra1Click: TMouseOptButtonAction;
    FTextShiftAltCtrlExtra1Click: TMouseOptButtonAction;
    FTextShiftAltExtra1Click: TMouseOptButtonAction;
    FTextShiftCtrlExtra1Click: TMouseOptButtonAction;
    FTextShiftExtra1Click: TMouseOptButtonAction;
    // extra-2 click
    FTextAltCtrlExtra2Click: TMouseOptButtonAction;
    FTextAltExtra2Click: TMouseOptButtonAction;
    FTextCtrlExtra2Click: TMouseOptButtonAction;
    FTextExtra2Click: TMouseOptButtonAction;
    FTextShiftAltCtrlExtra2Click: TMouseOptButtonAction;
    FTextShiftAltExtra2Click: TMouseOptButtonAction;
    FTextShiftCtrlExtra2Click: TMouseOptButtonAction;
    FTextShiftExtra2Click: TMouseOptButtonAction;
    FVersion: Integer;
    // wheel
    FWheel: TMouseOptWheelAction;
    FAltWheel: TMouseOptWheelAction;
    FCtrlWheel: TMouseOptWheelAction;
    FShiftWheel: TMouseOptWheelAction;
    FShiftAltWheel: TMouseOptWheelAction;
    FShiftCtrlWheel: TMouseOptWheelAction;
    FAltCtrlWheel: TMouseOptWheelAction;
    FShiftAltCtrlWheel: TMouseOptWheelAction;

    procedure ClearUserSchemes;
    function GetUserSchemeNames(Index: Integer): String;
    function GetUserSchemes(Index: String): TEditorMouseOptions;
    function GetUserSchemesAtPos(Index: Integer): TEditorMouseOptions;
    function  GetSelectedUserSchemeIndex: Integer;
    procedure SetSelectedUserScheme(const AValue: String);
    procedure SetSelectedUserSchemeIndex(const AValue: Integer);
    procedure AssignActions(Src: TEditorMouseOptions);
    procedure SetTextCtrlLeftClick(AValue: TMouseOptButtonActionOld);
    procedure SetTextMiddleClick(AValue: TMouseOptButtonActionOld);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;

    procedure ResetGutterToDefault;
    procedure ResetTextToDefault;
    procedure ResetToUserScheme;
    procedure AssignEx(Src: TEditorMouseOptions; WithUserSchemes: Boolean);
    procedure Assign(Src: TEditorMouseOptions); reintroduce;
    function  IsPresetEqualToMouseActions: Boolean;
    function  CalcCustomSavedActions: Boolean;
    procedure LoadFromXml(aSettings: TAppStorage; aPath: String; aOldPath: String; FileVersion: Integer);
    procedure SaveToXml(aSettings: TAppStorage; aPath: String);
    procedure ImportFromXml(aSettings: TAppStorage; aPath: String);
    procedure ExportToXml(aSettings: TAppStorage; aPath: String);
    procedure LoadUserSchemes;
    function  UserSchemeCount: Integer;
    function  IndexOfUserScheme(SchemeName: String): Integer;

    property Name: String read FName;
    property UserSchemes[Index: String]: TEditorMouseOptions read GetUserSchemes;
    property UserSchemesAtPos[Index: Integer]: TEditorMouseOptions read GetUserSchemesAtPos;
    property UserSchemeNames[Index: Integer]: String read GetUserSchemeNames;
    property SelectedUserSchemeIndex: Integer
             read GetSelectedUserSchemeIndex write SetSelectedUserSchemeIndex;

    property MainActions: TSynEditMouseActions read FMainActions;
    property SelActions: TSynEditMouseActions read FSelActions;
    property TextActions: TSynEditMouseActions read FTextActions;
    property GutterActions: TSynEditMouseActions read FGutterActions;
    property GutterActionsFold: TSynEditMouseActions read FGutterActionsFold;
    property GutterActionsFoldExp: TSynEditMouseActions read FGutterActionsFoldExp;
    property GutterActionsFoldCol: TSynEditMouseActions read FGutterActionsFoldCol;
    property GutterActionsLines: TSynEditMouseActions read FGutterActionsLines;
    property GutterActionsChanges: TSynEditMouseActions read FGutterActionsChanges;
    property GutterActionsOverView: TSynEditMouseActions read FGutterActionsOverView;
    property GutterActionsOverViewMarks: TSynEditMouseActions read FGutterActionsOverViewMarks;
  published
    property GutterLeft: TMouseOptGutterLeftType read FGutterLeft write FGutterLeft
             default moglUpClickAndSelect;
    property SelectOnLineNumbers: Boolean read FSelectOnLineNumbers write FSelectOnLineNumbers
             default True;
    property TextDrag: Boolean read FTextDrag write FTextDrag
             default True;
    property TextRightMoveCaret: Boolean read FTextRightMoveCaret  write FTextRightMoveCaret
             default False;
    // left multi click
    property TextDoubleLeftClick: TMouseOptButtonAction read FTextDoubleLeftClick write FTextDoubleLeftClick
             default mbaSelectSetWord;
    property TextTripleLeftClick: TMouseOptButtonAction read FTextTripleLeftClick write FTextTripleLeftClick
             default mbaSelectSetLineSmart;
    property TextQuadLeftClick: TMouseOptButtonAction read FTextQuadLeftClick write FTextQuadLeftClick
             default mbaSelectSetPara;
    property TextShiftDoubleLeftClick: TMouseOptButtonAction read FTextShiftDoubleLeftClick write FTextShiftDoubleLeftClick
             default mbaNone;
    property TextCtrlDoubleLeftClick: TMouseOptButtonAction read FTextCtrlDoubleLeftClick write FTextCtrlDoubleLeftClick
             default mbaNone;
    property TextAltDoubleLeftClick: TMouseOptButtonAction read FTextAltDoubleLeftClick write FTextAltDoubleLeftClick
             default mbaNone;
    // left + modifier click
    property TextShiftLeftClick: TMouseOptButtonAction read FTextShiftLeftClick write FTextShiftLeftClick
             default mbaNone;  // continue selection
    property TextCtrlLeftClick: TMouseOptButtonActionOld read FTextCtrlLeftClick write SetTextCtrlLeftClick
             default mbaDeclarationJump;
    property TextAltLeftClick: TMouseOptButtonAction read FTextAltLeftClick write FTextAltLeftClick
             default mbaSelectColumn;
    property TextShiftCtrlLeftClick: TMouseOptButtonAction read FTextShiftCtrlLeftClick write FTextShiftCtrlLeftClick
             default mbaMultiCaretToggle;  // continue selection
    property TextShiftAltLeftClick: TMouseOptButtonAction read FTextShiftAltLeftClick write FTextShiftAltLeftClick
             default mbaNone;  // continue selection
    property TextAltCtrlLeftClick: TMouseOptButtonAction read FTextAltCtrlLeftClick write FTextAltCtrlLeftClick
             default mbaNone;
    property TextShiftAltCtrlLeftClick: TMouseOptButtonAction read FTextShiftAltCtrlLeftClick write FTextShiftAltCtrlLeftClick
             default mbaNone;
    // middle click
    property TextMiddleClick: TMouseOptButtonActionOld read FTextMiddleClick write SetTextMiddleClick
             default mbaPaste;
    property TextShiftMiddleClick: TMouseOptButtonAction read FTextShiftMiddleClick write FTextShiftMiddleClick
             default mbaNone;
    property TextAltMiddleClick: TMouseOptButtonAction read FTextAltMiddleClick write FTextAltMiddleClick
             default mbaNone;
    property TextCtrlMiddleClick: TMouseOptButtonAction read FTextCtrlMiddleClick write FTextCtrlMiddleClick
             default mbaZoomReset;
    property TextShiftAltMiddleClick: TMouseOptButtonAction read FTextShiftAltMiddleClick write FTextShiftAltMiddleClick
             default mbaNone;
    property TextShiftCtrlMiddleClick: TMouseOptButtonAction read FTextShiftCtrlMiddleClick write FTextShiftCtrlMiddleClick
             default mbaNone;
    property TextAltCtrlMiddleClick: TMouseOptButtonAction read FTextAltCtrlMiddleClick write FTextAltCtrlMiddleClick
             default mbaNone;
    property TextShiftAltCtrlMiddleClick: TMouseOptButtonAction read FTextShiftAltCtrlMiddleClick write FTextShiftAltCtrlMiddleClick
             default mbaNone;
    // right click
    property TextRightClick: TMouseOptButtonAction read FTextRightClick write FTextRightClick
             default mbaContextMenu;
    property TextShiftRightClick: TMouseOptButtonAction read FTextShiftRightClick write FTextShiftRightClick
             default mbaNone;
    property TextAltRightClick: TMouseOptButtonAction read FTextAltRightClick write FTextAltRightClick
             default mbaNone;
    property TextCtrlRightClick: TMouseOptButtonAction read FTextCtrlRightClick write FTextCtrlRightClick
             default mbaContextMenuTab;
    property TextShiftAltRightClick: TMouseOptButtonAction read FTextShiftAltRightClick write FTextShiftAltRightClick
             default mbaNone;
    property TextShiftCtrlRightClick: TMouseOptButtonAction read FTextShiftCtrlRightClick write FTextShiftCtrlRightClick
             default mbaNone;
    property TextAltCtrlRightClick: TMouseOptButtonAction read FTextAltCtrlRightClick write FTextAltCtrlRightClick
             default mbaNone;
    property TextShiftAltCtrlRightClick: TMouseOptButtonAction read FTextShiftAltCtrlRightClick write FTextShiftAltCtrlRightClick
             default mbaNone;
    // extra-1 click
    property TextExtra1Click: TMouseOptButtonAction read FTextExtra1Click write FTextExtra1Click
             default mbaHistoryBack;
    property TextShiftExtra1Click: TMouseOptButtonAction read FTextShiftExtra1Click write FTextShiftExtra1Click
             default mbaNone;
    property TextAltExtra1Click: TMouseOptButtonAction read FTextAltExtra1Click write FTextAltExtra1Click
             default mbaNone;
    property TextCtrlExtra1Click: TMouseOptButtonAction read FTextCtrlExtra1Click write FTextCtrlExtra1Click
             default mbaNone;
    property TextShiftAltExtra1Click: TMouseOptButtonAction read FTextShiftAltExtra1Click write FTextShiftAltExtra1Click
             default mbaNone;
    property TextShiftCtrlExtra1Click: TMouseOptButtonAction read FTextShiftCtrlExtra1Click write FTextShiftCtrlExtra1Click
             default mbaNone;
    property TextAltCtrlExtra1Click: TMouseOptButtonAction read FTextAltCtrlExtra1Click write FTextAltCtrlExtra1Click
             default mbaNone;
    property TextShiftAltCtrlExtra1Click: TMouseOptButtonAction read FTextShiftAltCtrlExtra1Click write FTextShiftAltCtrlExtra1Click
             default mbaNone;
    // extra-2 click
    property TextExtra2Click: TMouseOptButtonAction read FTextExtra2Click write FTextExtra2Click
             default mbaHistoryForw;
    property TextShiftExtra2Click: TMouseOptButtonAction read FTextShiftExtra2Click write FTextShiftExtra2Click
             default mbaNone;
    property TextAltExtra2Click: TMouseOptButtonAction read FTextAltExtra2Click write FTextAltExtra2Click
             default mbaNone;
    property TextCtrlExtra2Click: TMouseOptButtonAction read FTextCtrlExtra2Click write FTextCtrlExtra2Click
             default mbaNone;
    property TextShiftAltExtra2Click: TMouseOptButtonAction read FTextShiftAltExtra2Click write FTextShiftAltExtra2Click
             default mbaNone;
    property TextShiftCtrlExtra2Click: TMouseOptButtonAction read FTextShiftCtrlExtra2Click write FTextShiftCtrlExtra2Click
             default mbaNone;
    property TextAltCtrlExtra2Click: TMouseOptButtonAction read FTextAltCtrlExtra2Click write FTextAltCtrlExtra2Click
             default mbaNone;
    property TextShiftAltCtrlExtra2Click: TMouseOptButtonAction read FTextShiftAltCtrlExtra2Click write FTextShiftAltCtrlExtra2Click
             default mbaNone;
    //
    property Wheel: TMouseOptWheelAction read FWheel write FWheel
             default mwaScroll;
    property CtrlWheel: TMouseOptWheelAction read FCtrlWheel write FCtrlWheel
             default mwaZoom;
    property AltWheel: TMouseOptWheelAction read FAltWheel write FAltWheel
             default mwaScrollPageLessOne;
    property ShiftWheel: TMouseOptWheelAction read FShiftWheel write FShiftWheel
             default mwaScrollSingleLine;
    property ShiftAltWheel: TMouseOptWheelAction read FShiftAltWheel write FShiftAltWheel
             default mwaNone;
    property ShiftCtrlWheel: TMouseOptWheelAction read FShiftCtrlWheel write FShiftCtrlWheel
             default mwaNone;
    property AltCtrlWheel: TMouseOptWheelAction read FAltCtrlWheel write FAltCtrlWheel
             default mwaNone;
    property ShiftAltCtrlWheel: TMouseOptWheelAction read FShiftAltCtrlWheel write FShiftAltCtrlWheel
             default mwaNone;

    // the flag below is set by CalcCustomSavedActions
    property CustomSavedActions: Boolean read FCustomSavedActions write FCustomSavedActions;
    property SelectedUserScheme: String read FSelectedUserScheme write SetSelectedUserScheme;
    property Version : Integer read FVersion write FVersion;
  end;

  { TEditorMouseOptionPresets }

  TEditorMouseOptionPresets = class
  private
    FPreset: TQuickStringlist;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  EditorOldOptionsEditAccessInViewState =
    (eoeaIgnoreInView,           // Find any editor
     eoeaInViewOnly,             // Only editors, with the jump-target in their current visible area
     eoeaInViewSoftCenterOnly    // Only editors, with the jump-target in their current visible soft center (exclude up to 5 lines top/bottom)
    );
  EditorOldOptionsEditAccessLockedState =
    (eoeaIgnoreLock,             // Find any editor
     eoeaLockedOnly,             // Only use locked Editor (e.g for InView = eoeaInViewOnly)
     eoeaUnlockedOnly,           // Only use unlocked Editors (default)
     eoeaLockedFirst,            // Search locked Editors first (each group according to Order)
     eoeaLockedLast              // Search locked Editoes last
    );
  EditorOldOptionsEditAccessOrder =
    (eoeaOrderByEditFocus,       // prefer the editors in the order they were last focused
     eoeaOrderByWindowFocus,     // prefer the editors in the order their window was last focused
     eoeaOrderByOldestEditFocus, // Reverse order by last focused
     eoeaOrderByOldestWindowFocus,
     eoeaOnlyCurrentEdit,        // search only the current-active editor (and only if it has the correct file)
     eoeaOnlyCurrentWindow,      // search only the current window (if it has an editor for the desired file)
     eoeaOrderByListPref         // follow global setting on the list
    );
  EditorOldOptionsEditAccessOpenNew =
    (eoeaNoNewTab,                     // Do not open a new tab, if none found
     eoeaNewTabInExistingWindowOnly,   // Open a new tab in existing (last focus) window, if possible
     eoeaNewTabInNewWindowOnly,        // Open a new tab in new window
     eoeaNewTabInExistingOrNewWindow   // Open a new tab in existing or new window
    );
  EditorOldOptionsEditAccessDefaultEntry = record
    SearchLocked: EditorOldOptionsEditAccessLockedState;
    SearchInView: EditorOldOptionsEditAccessInViewState;
    SearchOrder: EditorOldOptionsEditAccessOrder;
    SearchOpenNew: EditorOldOptionsEditAccessOpenNew;
    Enabled: Boolean;
    ID: String;
    Caption, Desc: String;
  end;
  EditorOldOptionsEditAccessDefaults = Array [0..8] of EditorOldOptionsEditAccessDefaultEntry;

const
  // captions and desc are set in EditorOldOptions.Create
  EditorOptionsEditAccessDefaults: EditorOldOptionsEditAccessDefaults =
  ( // Find locked - InView
    (SearchLocked: eoeaLockedOnly;        SearchInView:  eoeaInViewOnly;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNoNewTab;
     Enabled:      True;                  ID:            'Locked_InView';
     Caption: '';                         Desc: '' ),
    // Find unlocked
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaInViewSoftCenterOnly;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNoNewTab;
     Enabled:      False;                 ID:            'UnLocked_InSoftView';
     Caption: '';                         Desc: '' ),
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNoNewTab;
     Enabled:      True;                  ID:            'UnLocked';
     Caption: '';                         Desc: '' ),
    // open new tab
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNewTabInExistingWindowOnly;
     Enabled:      False;                 ID:            'UnLocked_OpenNewInOldWin';
     Caption: '';                         Desc: '' ),
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNewTabInNewWindowOnly;
     Enabled:      False;                 ID:            'UnLocked_OpenNewInNewWin';
     Caption: '';                         Desc: '' ),
    // Ignore locks
    (SearchLocked: eoeaIgnoreLock;        SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByOldestEditFocus; SearchOpenNew: eoeaNoNewTab;
     Enabled:      False;                 ID:            'IgnLocked_OldEdit';
     Caption: '';                         Desc: '' ),
    (SearchLocked: eoeaIgnoreLock;        SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOnlyCurrentEdit;   SearchOpenNew: eoeaNoNewTab;
     Enabled:      False;                 ID:            'IgnLocked_OnlyActEdit';
     Caption: '';                         Desc: '' ),
    (SearchLocked: eoeaIgnoreLock;        SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOnlyCurrentWindow; SearchOpenNew: eoeaNoNewTab;
     Enabled:      False;                 ID:            'IgnLocked_OnlyActWin';
     Caption: '';                         Desc: '' ),
    // Fallback (must be last)
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNewTabInExistingOrNewWindow;
     Enabled:      True;                  ID:            'UnLocked_OpenNewInAnyWin';
     Caption: '';                         Desc: '' )
  );
  EditorOptionsEditAccessUserDef: EditorOldOptionsEditAccessDefaultEntry =
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNoNewTab;
     Enabled:      True;                  ID:            '';
     Caption: '';                         Desc: '' );

type

  EditorOldOptionsEditAccessOrderList = class;

  { EditorOldOptionsEditAccessOrderEntry }

  EditorOldOptionsEditAccessOrderEntry = class(TPersistent)
  private
    FId: String;
    FList: EditorOldOptionsEditAccessOrderList;
    FCaption: String;
    FDesc: String;
    FEnabled: Boolean;
    FIsFallback: Boolean;
    FDefaults: EditorOldOptionsEditAccessOrderEntry;
    FSearchInView: EditorOldOptionsEditAccessInViewState;
    FSearchLocked: EditorOldOptionsEditAccessLockedState;
    FSearchOpenNew: EditorOldOptionsEditAccessOpenNew;
    FSearchOrder: EditorOldOptionsEditAccessOrder;
    procedure AssignFrom(AValue: EditorOldOptionsEditAccessDefaultEntry);
    procedure SetEnabled(const AValue: Boolean);
  public
    constructor Create(AList: EditorOldOptionsEditAccessOrderList);
    destructor Destroy; override;
    procedure Assign(Src: EditorOldOptionsEditAccessOrderEntry); reintroduce;
    procedure InitFrom(AValue: EditorOldOptionsEditAccessDefaultEntry);
  public
    function RealSearchOrder: EditorOldOptionsEditAccessOrder;
    property Defaults: EditorOldOptionsEditAccessOrderEntry read FDefaults;
    property ID: String read FId write FId;
    property IsFallback: Boolean read FIsFallback;
    property Desc: String read FDesc write FDesc;
  //published
    property Caption: String
             read FCaption write FCaption;
  published
    property Enabled: Boolean
             read FEnabled write SetEnabled;
  public
    property SearchLocked: EditorOldOptionsEditAccessLockedState
             read FSearchLocked write FSearchLocked;
    property SearchInView: EditorOldOptionsEditAccessInViewState
             read FSearchInView write FSearchInView;
    property SearchOrder: EditorOldOptionsEditAccessOrder
             read FSearchOrder write FSearchOrder;
    property SearchOpenNew: EditorOldOptionsEditAccessOpenNew
             read FSearchOpenNew write FSearchOpenNew;
    //property IgnoreTopLineAdjustment;
  end;

  { EditorOldOptionsEditAccessOrderList }

  EditorOldOptionsEditAccessOrderList = class(TPersistent)
  private
    FList: TFPList;
    FSearchOrder: EditorOldOptionsEditAccessOrder;
    function GetItems(Index: Integer): EditorOldOptionsEditAccessOrderEntry;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure InitDefaults;
    procedure Assign(Src: EditorOldOptionsEditAccessOrderList); reintroduce;
    procedure LoadFromXMLConfig(aSettings: TAppStorage; Path: String);
    procedure SaveToXMLConfig(aSettings: TAppStorage; Path: String);
    function Count: Integer;
    property Items[Index: Integer]: EditorOldOptionsEditAccessOrderEntry
             read GetItems; default;
  published
    property SearchOrder: EditorOldOptionsEditAccessOrder
             read FSearchOrder write FSearchOrder;
  end;

const
  EditorUserDefinedWordsKeyCatName = 'User defined word markup';
  MARKUP_USER_DEF_PRIOR = 3500;

var
  EditorUserDefinedWordsGlobalId: string = 'a';


type

  TEditorOptions = class(TPersistent)
  private
    FBlockTabIndent: Integer;
    FCompletionLongLineHintInMSec: Integer;
    FCompletionLongLineHintType: TSynCompletionLongHintType;
    FMultiCaretDeleteSkipLineBreak: Boolean;
    FPasExtendedKeywordsMode: Boolean;
    FHideSingleTabInWindow: Boolean;
    FTopInfoView: boolean;
    {$IFDEF WinIME}
    FUseMinimumIme: Boolean;
    {$ENDIF}
    mSettings: TAppStorage;

    // general options
    fFindTextAtCursor: Boolean;
    fShowTabCloseButtons: Boolean;
    FMultiLineTab: Boolean;
    fShowTabNumbers: Boolean;
    fUseTabHistory: Boolean;
    fTabPosition: TTabPosition;
    fSynEditOptions: TSynEditorOptions;
    fSynEditOptions2: TSynEditorOptions2;
    fUndoAfterSave: Boolean;
    fUseSyntaxHighlight: Boolean;
    FCopyWordAtCursorOnCopyNone: Boolean;
    FShowGutterHints: Boolean;
    fBlockIndent: Integer;
    fBlockIndentType: TSynBeautifierIndentType;
    FTrimSpaceType: TSynEditStringTrimmingType;
    fUndoLimit: Integer;
    fTabWidth:  Integer;
    FBracketHighlightStyle: TSynEditBracketHighlightStyle;
    FMultiCaretOnColumnSelect: Boolean;

    // Display options
    fVisibleRightMargin: Boolean;
    fVisibleGutter: Boolean;
    fShowLineNumbers: Boolean;
    fShowOnlyLineNumbersMultiplesOf: integer;
    FShowOverviewGutter: boolean;
    fGutterWidth: Integer;
    FGutterSeparatorIndex: Integer;
    fRightMargin: Integer;
    fEditorFont:  String;
    fEditorFontSize:   Integer;
    fExtraCharSpacing: Integer;
    fExtraLineSpacing: Integer;
    fDisableAntialiasing: Boolean;
    FDoNotWarnForFont: string;

    // Key Mappings options
    fKeyMappingScheme: String;

    // Mouse Mappings options
    FUserMouseSettings: TEditorMouseOptions;
    FTempMouseSettings: TEditorMouseOptions;

    // Color options
    fHighlighterList: TEditOptLangList;
    FUserColorSchemeSettings: TColorSchemeFactory;

    // Markup Current Word
    FMarkupCurWordTime: Integer;
    FMarkupCurWordFullLen: Integer;
    FMarkupCurWordNoKeyword: Boolean;
    FMarkupCurWordTrim: Boolean;
    FMarkupCurWordNoTimer: Boolean;

    // Code tools options (MG: these will move to an unit of their own)
    fAutoBlockCompletion: Boolean;
    fAutoCodeParameters: Boolean;
    fAutoDelayInMSec: Integer;
    FAutoRemoveEmptyMethods: Boolean;
    fAutoToolTipExprEval: Boolean;
    fAutoToolTipSymbTools: Boolean;
    FDbgHintAutoTypeCastClass: Boolean;
    fCodeTemplateFileNameRaw: String;
    fCTemplIndentToTokenStart: Boolean;
    fAutoDisplayFuncPrototypes: Boolean;

    // Code Folding
    FUseCodeFolding: Boolean;
    FUseMarkupWordBracket: Boolean;
    FUseMarkupOutline: Boolean;
    FReverseFoldPopUpOrder: Boolean;

    // Multi window
    FMultiWinEditAccessOrder: EditorOldOptionsEditAccessOrderList;
    FCtrlMiddleTabClickClosesOthers: Boolean;
    FShowFileNameInCaption: Boolean;

    // Comment Continue
    FAnsiCommentContinueEnabled: Boolean;
    FAnsiCommentMatch: String;
    FAnsiCommentMatchMode: TSynCommentMatchMode;
    FAnsiCommentPrefix: String;
    FAnsiIndentMode: TSynCommentIndentFlags;
    FAnsiIndentAlignMax: integer;

    FCurlyCommentContinueEnabled: Boolean;
    FCurlyCommentMatch: String;
    FCurlyCommentMatchMode: TSynCommentMatchMode;
    FCurlyCommentPrefix: String;
    FCurlyIndentMode: TSynCommentIndentFlags;
    FCurlyIndentAlignMax: integer;

    FSlashCommentContinueEnabled: Boolean;
    FSlashCommentMatch: String;
    FSlashCommentMatchMode: TSynCommentMatchMode;
    FSlashCommentPrefix: String;
    FSlashIndentMode: TSynCommentIndentFlags;
    FSlashCommentExtend: TSynCommentExtendMode;
    FSlashIndentAlignMax: integer;

    FStringBreakAppend: String;
    FStringBreakEnabled: Boolean;
    FStringBreakPrefix: String;

    FDefaultValues: TEditorOptions;

    mGeneralPage: TFrame;
    mOptionsPage: TFrame;
    mColoursPage: TFrame;
    mEThemesPage: TFrame;

    function GetCodeTemplateFileNameExpand:String;
  protected
    function GetTabPosition: TTabPosition;
    procedure DebugNotify(Sender: TObject);
  public
    class function GetGroupCaption:string;
    class function GetInstance: TPersistent;
    procedure DoAfterWrite(Restore: boolean);
  public
    constructor Create(settings: TAppStorage);
    constructor CreateDefaultOnly;
    destructor Destroy; override;
    procedure SetSettings(appCnf: TAppStorage);
    procedure Init;
    procedure Load;
    procedure Save;
    procedure Setup;

    procedure TranslateResourceStrings;
    function GetAdditionalAttributeName(aha:TAdditionalHilightAttribute): string;
    function GetSynEditOptionName(SynOption: TSynEditorOption): string;
    function GetSynBeautifierIndentName(IndentType: TSynBeautifierIndentType): string;
    function GetSynBeautifierIndentType(IndentName: String): TSynBeautifierIndentType;
    function GetTrimSpaceName(IndentType: TSynEditStringTrimmingType): string;
    function GetTrimSpaceType(IndentName: String): TSynEditStringTrimmingType;

    procedure AssignKeyMapTo({%H-}ASynEdit: TSynEdit; {%H-}SimilarEdit: TSynEdit = nil); // Or copy fromSimilarEdit
    procedure GetHighlighterSettings(Syn: TSrcIDEHighlighter); // read highlight settings from config file
    procedure GetSynEditSettings(ASynEdit: TSynEdit; SimilarEdit: TSynEdit = nil); // read synedit settings from config file
    procedure GetSynEditPreviewSettings(APreviewEditor: TObject);
    procedure ApplyFontSettingsTo(ASynEdit: TSynEdit);

    function ExtensionToLazSyntaxHighlighter(Ext: String): TLazSyntaxHighlighter;
    function CreateSyn(LazSynHilighter: TLazSyntaxHighlighter): TSrcIDEHighlighter;
    function ReadColorScheme(const LanguageName: String): String;
    function ReadPascalColorScheme: String;
    procedure WriteColorScheme(const LanguageName, SynColorScheme: String);
    procedure ReadHighlighterSettings(Syn: TSrcIDEHighlighter;
                                      SynColorScheme: String);

    procedure ReadHighlighterFoldSettings(Syn: TSrcIDEHighlighter; ReadForOptions: Boolean = False);
    procedure ReadDefaultsForHighlighterFoldSettings(Syn: TSrcIDEHighlighter);
    procedure WriteHighlighterFoldSettings(Syn: TSrcIDEHighlighter);

    procedure ReadHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
    procedure ReadDefaultsForHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
    procedure WriteHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);

    procedure SetMarkupColor(Syn: TSrcIDEHighlighter;
                             AddHilightAttr: TAdditionalHilightAttribute;
                             aMarkup: TSynSelectedColor);
    procedure SetMarkupColors(aSynEd: TSynEdit);
  public
    // general options
    property Settings: TAppStorage
      read mSettings write SetSettings;
    property SynEditOptions: TSynEditorOptions
      read fSynEditOptions write fSynEditOptions default SynEditDefaultOptions;
    property SynEditOptions2: TSynEditorOptions2
      read fSynEditOptions2 write fSynEditOptions2 default SynEditDefaultOptions2;
    property ShowTabCloseButtons: Boolean
      read fShowTabCloseButtons write fShowTabCloseButtons;
  published
    property MultiLineTab: Boolean
      read FMultiLineTab write FMultiLineTab default False;
  public
    property HideSingleTabInWindow: Boolean
      read FHideSingleTabInWindow write FHideSingleTabInWindow;
    property ShowTabNumbers: Boolean read fShowTabNumbers write fShowTabNumbers;
    property UndoAfterSave: Boolean read fUndoAfterSave
      write fUndoAfterSave default True;
    property FindTextAtCursor: Boolean
      read fFindTextAtCursor write fFindTextAtCursor default True;
    property UseSyntaxHighlight: Boolean
      read fUseSyntaxHighlight write fUseSyntaxHighlight default True;
    property CopyWordAtCursorOnCopyNone: Boolean
      read FCopyWordAtCursorOnCopyNone write FCopyWordAtCursorOnCopyNone;
    property ShowGutterHints: Boolean read FShowGutterHints
      write FShowGutterHints;
    property BlockIndent: Integer
      read fBlockIndent write fBlockIndent default 2;
    property BlockTabIndent: Integer
      read FBlockTabIndent write FBlockTabIndent default 0;
    property BlockIndentType: TSynBeautifierIndentType
      read fBlockIndentType write fBlockIndentType default sbitCopySpaceTab;
    property TrimSpaceType: TSynEditStringTrimmingType
      read FTrimSpaceType write FTrimSpaceType default settLeaveLine;
    property UndoLimit: Integer read fUndoLimit write fUndoLimit default 32767;
    property TabWidth: Integer read fTabWidth write fTabWidth default 8;
    property BracketHighlightStyle: TSynEditBracketHighlightStyle read FBracketHighlightStyle write FBracketHighlightStyle default sbhsBoth;

    // Display options
    property VisibleRightMargin: Boolean
      read fVisibleRightMargin write fVisibleRightMargin default True;
    property VisibleGutter: Boolean read fVisibleGutter
      write fVisibleGutter default True;
    property ShowLineNumbers: Boolean read fShowLineNumbers
      write fShowLineNumbers default False;
    property ShowOnlyLineNumbersMultiplesOf: integer read fShowOnlyLineNumbersMultiplesOf
      write fShowOnlyLineNumbersMultiplesOf;
    property GutterWidth: Integer
      read fGutterWidth write fGutterWidth default 30;
    property GutterSeparatorIndex: Integer read FGutterSeparatorIndex
      write FGutterSeparatorIndex default 3;
    property RightMargin: Integer
      read fRightMargin write fRightMargin default 80;
    property EditorFont: String read fEditorFont write fEditorFont;
    property EditorFontSize: Integer
      read fEditorFontSize write fEditorFontSize;
    property ExtraCharSpacing: Integer
      read fExtraCharSpacing write fExtraCharSpacing default 0;
    property ExtraLineSpacing: Integer
      read fExtraLineSpacing write fExtraLineSpacing default 1;
    property DisableAntialiasing: Boolean
      read fDisableAntialiasing write fDisableAntialiasing default DefaultEditorDisableAntiAliasing;
    property DoNotWarnForFont: string
      read FDoNotWarnForFont write FDoNotWarnForFont;

    // Key Mappings
    property KeyMappingScheme: String
      read fKeyMappingScheme write fKeyMappingScheme;

    // Mouse Mappings
    // Current saved config
    property UserMouseSettings: TEditorMouseOptions read FUserMouseSettings;
    // Used by the 2 Mouse-option pages, so they share data (un-saved)
    property TempMouseSettings: TEditorMouseOptions read FTempMouseSettings;

    // Color options
    property HighlighterList: TEditOptLangList read fHighlighterList;
    property UserColorSchemeGroup: TColorSchemeFactory read FUserColorSchemeSettings;

    // Markup Current Word
    property MarkupCurWordTime: Integer
      read FMarkupCurWordTime write FMarkupCurWordTime default 1500;
    property MarkupCurWordFullLen: Integer
      read FMarkupCurWordFullLen write FMarkupCurWordFullLen default 3;
    property MarkupCurWordNoKeyword: Boolean
      read FMarkupCurWordNoKeyword write FMarkupCurWordNoKeyword default False;
    property MarkupCurWordTrim: Boolean
      read FMarkupCurWordTrim write FMarkupCurWordTrim default True;
    property MarkupCurWordNoTimer: Boolean
      read FMarkupCurWordNoTimer write FMarkupCurWordNoTimer default False;

    // Code Tools options
    property AutoBlockCompletion: Boolean
      read fAutoBlockCompletion write FAutoBlockCompletion default True;
    property AutoCodeParameters: Boolean
      read fAutoCodeParameters write fAutoCodeParameters default True;
    property AutoToolTipExprEval: Boolean
      read fAutoToolTipExprEval write fAutoToolTipExprEval default True; // debugger hints
    property AutoToolTipSymbTools: Boolean
      read fAutoToolTipSymbTools write fAutoToolTipSymbTools default True; // declaration hints
    property AutoDisplayFunctionPrototypes: Boolean
      read fAutoDisplayFuncPrototypes write fAutoDisplayFuncPrototypes default True;

  published
    property DbgHintAutoTypeCastClass: Boolean
      read FDbgHintAutoTypeCastClass write FDbgHintAutoTypeCastClass default True; // declaration hints
  public
    property AutoDelayInMSec: Integer read fAutoDelayInMSec
      write fAutoDelayInMSec default 1000;
    property CodeTemplateFileNameRaw: String
      read fCodeTemplateFileNameRaw write fCodeTemplateFileNameRaw;
    property CodeTemplateFileNameExpand:String
      read GetCodeTemplateFileNameExpand;
    property CodeTemplateIndentToTokenStart: Boolean
      read fCTemplIndentToTokenStart write fCTemplIndentToTokenStart;
    property AutoRemoveEmptyMethods: Boolean read FAutoRemoveEmptyMethods
      write FAutoRemoveEmptyMethods default False;
    property CompletionLongLineHintInMSec: Integer
      read FCompletionLongLineHintInMSec write FCompletionLongLineHintInMSec;
  published
    property CompletionLongLineHintType: TSynCompletionLongHintType
      read FCompletionLongLineHintType write FCompletionLongLineHintType
      default sclpExtendRightOnly;

  public
    // Code Folding
    property UseCodeFolding: Boolean
      read FUseCodeFolding write FUseCodeFolding default True;
    property UseMarkupWordBracket: Boolean
      read FUseMarkupWordBracket write FUseMarkupWordBracket default True;
    property UseMarkupOutline: Boolean
      read FUseMarkupOutline write FUseMarkupOutline default False;

    // Multi window
    property MultiWinEditAccessOrder: EditorOldOptionsEditAccessOrderList
      read FMultiWinEditAccessOrder write FMultiWinEditAccessOrder;

  published { use RTTIConf}
    property TabPosition: TTabPosition
      read fTabPosition write fTabPosition default tpTop;
    // General - Misc
    {$IFDEF WinIME}
    property UseMinimumIme: Boolean read FUseMinimumIme write FUseMinimumIme default False;
    {$ENDIF}
    // Display
    property ShowOverviewGutter: boolean
      read FShowOverviewGutter write FShowOverviewGutter default True;
    property TopInfoView: boolean
      read FTopInfoView write FTopInfoView default True;
    // Code Folding
    property ReverseFoldPopUpOrder: Boolean
      read FReverseFoldPopUpOrder write FReverseFoldPopUpOrder default True;
    property UseTabHistory: Boolean read fUseTabHistory write fUseTabHistory;
    property MultiCaretOnColumnSelect: Boolean
      read FMultiCaretOnColumnSelect write FMultiCaretOnColumnSelect default True;
    property MultiCaretDeleteSkipLineBreak: Boolean
      read FMultiCaretDeleteSkipLineBreak write FMultiCaretDeleteSkipLineBreak default False;

    // Highlighter Pas
    property PasExtendedKeywordsMode: Boolean
      read FPasExtendedKeywordsMode write FPasExtendedKeywordsMode default False;

    // Multi window
    property CtrlMiddleTabClickClosesOthers: Boolean
      read FCtrlMiddleTabClickClosesOthers write FCtrlMiddleTabClickClosesOthers default True;

    property ShowFileNameInCaption: Boolean
      read FShowFileNameInCaption write FShowFileNameInCaption default False;

    // Commend Continue
    property AnsiCommentContinueEnabled: Boolean
      read FAnsiCommentContinueEnabled write FAnsiCommentContinueEnabled;
    property AnsiCommentMatch: String
      read FAnsiCommentMatch write FAnsiCommentMatch;
    property AnsiCommentPrefix: String
      read FAnsiCommentPrefix write FAnsiCommentPrefix;
    property AnsiCommentMatchMode: TSynCommentMatchMode
      read FAnsiCommentMatchMode write FAnsiCommentMatchMode;
    property AnsiIndentMode: TSynCommentIndentFlags
      read FAnsiIndentMode write FAnsiIndentMode;
    property AnsiIndentAlignMax: integer
      read FAnsiIndentAlignMax write FAnsiIndentAlignMax;

    property CurlyCommentContinueEnabled: Boolean
      read FCurlyCommentContinueEnabled write FCurlyCommentContinueEnabled;
    property CurlyCommentMatch: String
      read FCurlyCommentMatch write FCurlyCommentMatch;
    property CurlyCommentPrefix: String
      read FCurlyCommentPrefix write FCurlyCommentPrefix;
    property CurlyCommentMatchMode: TSynCommentMatchMode
      read FCurlyCommentMatchMode write FCurlyCommentMatchMode;
    property CurlyIndentMode: TSynCommentIndentFlags
      read FCurlyIndentMode write FCurlyIndentMode;
    property CurlyIndentAlignMax: integer
      read FCurlyIndentAlignMax write FCurlyIndentAlignMax;

    property SlashCommentContinueEnabled: Boolean
      read FSlashCommentContinueEnabled write FSlashCommentContinueEnabled;
    property SlashCommentMatch: String
      read FSlashCommentMatch write FSlashCommentMatch;
    property SlashCommentPrefix: String
      read FSlashCommentPrefix write FSlashCommentPrefix;
    property SlashCommentMatchMode: TSynCommentMatchMode
      read FSlashCommentMatchMode write FSlashCommentMatchMode;
    property SlashIndentMode: TSynCommentIndentFlags
      read FSlashIndentMode write FSlashIndentMode;
    property SlashCommentExtend: TSynCommentExtendMode
      read FSlashCommentExtend write FSlashCommentExtend;
    property SlashIndentAlignMax: integer
      read FSlashIndentAlignMax write FSlashIndentAlignMax;

    property StringBreakEnabled: Boolean read FStringBreakEnabled write FStringBreakEnabled;
    property StringBreakAppend: String read FStringBreakAppend write FStringBreakAppend;
    property StringBreakPrefix: String read FStringBreakPrefix write FStringBreakPrefix;

    // Frame Handling
    property GeneralPage: TFrame read mGeneralPage write mGeneralPage;
    property OptionsPage: TFrame read mOptionsPage write mOptionsPage;
    property ColoursPage: TFrame read mColoursPage write mColoursPage;
    property EThemesPage: TFrame read mEThemesPage write mEThemesPage;

  end;


procedure RepairEditorFontSize(var FontSize: integer);

function BuildBorlandDCIFile(ACustomSynAutoComplete: TCustomSynAutoComplete): Boolean;
function ColorSchemeFactory: TColorSchemeFactory;
function UserSchemeDirectory(CreateIfNotExists: Boolean = False): String;
function HighlighterListSingleton: TEditOptLangList;
procedure InitLocale();


implementation

const
  ValidAttribChars = ['a'..'z', 'A'..'Z', '_', '0'..'9'];

  // several language types can be redirected. For example there are FreePascal
  // and Delphi, but currently both are hilighted with the FreePascal
  // highlighter
  CompatibleLazSyntaxHilighter: array[TLazSyntaxHighlighter] of
    TLazSyntaxHighlighter = (
    lshNone,
    lshText,
    lshMd,
    lshHTML,
    lshCSS,
    lshXML,
    lshJScript
    );

var
  DefaultColorSchemeName: String;


{-------------------------------------------------------------------------------
  FontHeightToSize
 ------------------------------------------------------------------------------}
function FontHeightToSize(Height: Integer): Integer;
var
  AFont: TFont;
begin
  AFont := TFont.Create;
  AFont.Height := Height;
  Result := AFont.Size;
  AFont.Free;
end;


{-------------------------------------------------------------------------------
  TransformScheme
 ------------------------------------------------------------------------------}
function TransformScheme(hl: TObject): TClass;
var
  sc: TClass;
  clsName: String;
begin
  sc := TClass(hl);
  clsName := hl.ClassName;

  if (clsName = 'TSynSpellTxtSyn') then
  begin
    sc :=  hl.ClassParent;
    clsName := sc.ClassName;
  end;

  if (clsName = 'TSynSpellPasSyn') then
  begin
    sc := hl.ClassParent;
    clsName := sc.ClassName;
  end;

  if (clsName = 'TSynSpellCppSyn') then
  begin
    sc := hl.ClassParent;
    clsName := sc.ClassName;
  end;

  if clsName = 'TSynTxtSyn' then
  begin

  end;
  Result := sc;
end;



{ TSynEditMouseActionKeyCmdHelper }

{-------------------------------------------------------------------------------
  GetOptionKeyCmd
 ------------------------------------------------------------------------------}
function TSynEditMouseActionKeyCmdHelper.GetOptionKeyCmd: TSynEditorCommand;
begin
  Result := inherited Option;
end;


{-------------------------------------------------------------------------------
  SetOptionKeyCmd
 ------------------------------------------------------------------------------}
procedure TSynEditMouseActionKeyCmdHelper.SetOptionKeyCmd(
  const AValue: TSynEditorCommand);
begin
  inherited Option := AValue;
end;


{-------------------------------------------------------------------------------
  RepairEditorFontSize
 ------------------------------------------------------------------------------}
procedure RepairEditorFontSize(var FontSize: integer);
begin
  if ((FontSize >= 0) and (FontSize <=  EditorOptionsMinimumFontSize))
  or ((FontSize  < 0) and (FontSize >= -EditorOptionsMinimumFontSize)) then
    FontSize := SynDefaultFontSize;
end;


{-------------------------------------------------------------------------------
  BuildBorlandDCIFile
 ------------------------------------------------------------------------------}
function BuildBorlandDCIFile(
  ACustomSynAutoComplete: TCustomSynAutoComplete): Boolean;
  // returns if something has changed
var
  sl: TStringList;
  i, sp, ep: Integer;
  Token, Comment, Value: String;
  Attributes: TStrings;
begin
  Result := False;
  sl     := TStringList.Create;
  try
    for i := 0 to ACustomSynAutoComplete.Completions.Count - 1 do
    begin
      Token := ACustomSynAutoComplete.Completions[i];
      Comment := ACustomSynAutoComplete.CompletionComments[i];
      Value := ACustomSynAutoComplete.CompletionValues[i];
      sl.Add('[' + Token + ' | ' + Comment + ']');
      Attributes:=ACustomSynAutoComplete.CompletionAttributes[i];
      if (Attributes<>nil) and (Attributes.Count>0) then begin
        sl.Add(CodeTemplateAttributesStartMagic);
        sl.AddStrings(Attributes);
        sl.Add(CodeTemplateAttributesEndMagic);
      end;
      sp    := 1;
      ep    := 1;
      while ep <= length(Value) do
        if Value[ep] in [#10, #13] then
        begin
          sl.Add(copy(Value, sp, ep - sp));
          inc(ep);
          if (ep <= length(Value)) and (Value[ep] in [#10, #13]) and
            (Value[ep] <> Value[ep - 1]) then
            inc(ep);
          sp := ep;
        end
        else
          inc(ep);
      if (ep > sp) or ((Value <> '') and (Value[length(Value)] in [#10, #13])) then
        sl.Add(copy(Value, sp, ep - sp));
    end;
    if ACustomSynAutoComplete.AutoCompleteList.Equals(sl) = False then
    begin
      Result := True;
      ACustomSynAutoComplete.AutoCompleteList := sl;
    end;
  finally
    sl.Free;
  end;
end;


{-------------------------------------------------------------------------------
  TColorSchemeFactory
 ------------------------------------------------------------------------------}
// The lazy-man color scheme factory
function ColorSchemeFactory: TColorSchemeFactory;
const
  Singleton: TColorSchemeFactory = nil;
var
  FileList: TStringList;
  i, j, c: Integer;
  settings: TAppStorage;
  n: String;

  procedure AddFromResource(AResName, ASchemeName: String);
  var
    r: TLResource;
    Stream : TStringStream;
    flag: Boolean;
  begin
    r:=LazarusResources.Find(AResName);
    if r=nil then exit;

    Stream := TStringStream.Create(r.Value);
    settings:= TAppStorage.Create('');
    settings.ReadFromStream(Stream);

    flag := false;
    flag := settings.HasPath('MdEdit/ColorSchemes/Globals/Scheme/', false);
    flag :=  Not flag;

    Singleton.RegisterScheme(settings, ASchemeName, 'MdEdit/ColorSchemes/');

    FreeAndNil(settings);
    FreeAndNil(Stream);
  end;
begin
  if not Assigned(Singleton) then
  begin
    InitLocale;

    Singleton := TColorSchemeFactory.Create;
    // register all built-in color schemes

    AddFromResource('ColorSchemeLightMode', 'LightMode');
    AddFromResource('ColorSchemeDarkMode', 'DarkMode');
    DefaultColorSchemeName := 'LightMode';

    if DirectoryExistsUTF8(UserSchemeDirectory(False)) then begin
      FileList := FindAllFiles(UserSchemeDirectory(False), '*.xml', False);
      for i := 0 to FileList.Count - 1 do begin
        settings := nil;
        try
          settings := TAppStorage.Create(FileList[i]);
          c := settings.Read('MdEdit/ColorSchemes/Names/Count', 0);
          for j := 0 to c-1 do begin
            n := settings.Read('MdEdit/ColorSchemes/Names/Item'+IntToStr(j+1)+'/Value', '');
            if n <> '' then
              Singleton.RegisterScheme(settings, n, 'MdEdit/ColorSchemes/');
          end;
        except
          if LStr <> Nil then
            ShowMessage(Format(LStr('dlgUserSchemeError', 'Failed to load user-scheme file %s'), [FileList[i]]));
        end;
        settings.Free;
      end;
      FileList.Free;
    end;
  end;
  Result := Singleton;
end;


{-------------------------------------------------------------------------------
  GetPrimaryConfigPath
 ------------------------------------------------------------------------------}
function GetPrimaryConfigPath: String;
begin
  Result:= '.';
end;


{-------------------------------------------------------------------------------
  UserSchemeDirectory
 ------------------------------------------------------------------------------}
function UserSchemeDirectory(CreateIfNotExists: Boolean): String;
begin
  Result := AppendPathDelim(GetPrimaryConfigPath) + 'userschemes';
  If CreateIfNotExists and (not DirectoryExistsUTF8(Result)) then
    CreateDirUTF8(Result);
end;


{-------------------------------------------------------------------------------
  HighlighterListSingleton
 ------------------------------------------------------------------------------}
function HighlighterListSingleton: TEditOptLangList;
const
  Singleton: TEditOptLangList = nil;
begin
  if not Assigned(Singleton) then
    Singleton := TEditOptLangList.Create;
  Result := Singleton;
end;


{-------------------------------------------------------------------------------
  StrToValidXMLName
 ------------------------------------------------------------------------------}
function StrToValidXMLName(const s: String): String;
var
  i: Integer;
begin
  Result := s;
  // replace invalid characters
  for i := 1 to length(Result) do
    if (not (Result[i] in ValidAttribChars)) then
      Result[i] := '_';
end;


{-------------------------------------------------------------------------------
  InitLocale
 ------------------------------------------------------------------------------}
procedure InitLocale();
const
  InitDone: Boolean = False;
begin
  if InitDone then exit;
  InitDone := true;
  EditorOptionsEditAccessDefaults[0].Caption := LStr('dlgEditAccessCaptionLockedInView', 'Locked, if text in view');
  EditorOptionsEditAccessDefaults[0].Desc    := LStr('dlgEditAccessDescLockedInView',
    'This option will use a locked (and only a locked) Editor '+
    'which does not need to scroll in order to display the target jump point '+
    '(target jump point is already in visible screen area).');
  EditorOptionsEditAccessDefaults[1].Caption := LStr('dlgEditAccessCaptionUnLockedInSoftView', 'Unlocked, if text in centered view');
  EditorOptionsEditAccessDefaults[1].Desc    := LStr('dlgEditAccessDescUnLockedInSoftView',
    'This option will use a not locked Editor '+
    'which does not need to scroll in order to display the target jump point '+
    '(target jump point is already in visible screen center area, excluding 2-5 lines at the top/bottom).');
  EditorOptionsEditAccessDefaults[2].Caption := LStr('dlgEditAccessCaptionUnLocked', 'Unlocked');
  EditorOptionsEditAccessDefaults[2].Desc    := LStr('dlgEditAccessDescUnLocked', 'This option will use any not locked Editor.');
  EditorOptionsEditAccessDefaults[3].Caption := LStr('dlgEditAccessCaptionUnLockedOpenNewInOldWin', 'New tab in existing window');
  EditorOptionsEditAccessDefaults[3].Desc    := LStr('dlgEditAccessDescUnLockedOpenNewInOldWin',
    'If no unlocked tab is found, then this option will open a new Tab in an existing '+
    '(and only in an existing) Window. '+
    'A tab is only opened if there is a window that has no editor for the target file yet.');
  EditorOptionsEditAccessDefaults[4].Caption := LStr('dlgEditAccessCaptionUnLockedOpenNewInNewWin', 'New tab in new window');
  EditorOptionsEditAccessDefaults[4].Desc    := LStr('dlgEditAccessDescUnLockedOpenNewInNewWin',
    'If no unlocked tab is found, then this option will open a new Tab in a new '+
    'Window (even if other existing windows could be used for the new tab). '+
    'This option will always succeed, further options are never tested.');
  EditorOptionsEditAccessDefaults[5].Caption := LStr('dlgEditAccessCaptionIgnLockedOldEdit', 'Ignore Locks, use longest unused editor');
  EditorOptionsEditAccessDefaults[5].Desc    := LStr('dlgEditAccessDescIgnLockedOldEdit',
    'This option will use the longest unused editor for the file, '+
    'even if it is locked and/or needs scrolling. '+
    'The determination of the longest unused editor does not look at the order in which the windows were focused, '+
    'even if this is set by the setting for "same criteria order". ' +
    'This option will always succeed, further options are never tested.');
  EditorOptionsEditAccessDefaults[6].Caption := LStr('dlgEditAccessCaptionIgnLockedOnlyActEdit', 'Ignore Locks, if editor is current');
  EditorOptionsEditAccessDefaults[6].Desc    := LStr('dlgEditAccessDescIgnLockedOnlyActEdit',
    'This option will check if the current active editor has the target file ' +
    'and if it is, it will use the current editor, even if it is locked and/or needs scrolling.');
  EditorOptionsEditAccessDefaults[7].Caption := LStr('dlgEditAccessCaptionIgnLockedOnlyActWin', 'Ignore Locks, if editor in current window');
  EditorOptionsEditAccessDefaults[7].Desc    := LStr('dlgEditAccessDescIgnLockedOnlyActWin',
    'This option will check if there is an editor for the target file in the current window '+
    'and if there is, it will use this editor, even if it is locked and/or needs scrolling.');
  EditorOptionsEditAccessDefaults[8].Caption := LStr('dlgEditAccessCaptionUnLockedOpenNewInAnyWin', 'New tab, existing or new window');
  EditorOptionsEditAccessDefaults[8].Desc    := LStr('dlgEditAccessDescUnLockedOpenNewInAnyWin',
    'This option will open a new Tab in an existing or new Window if no unlocked tab is found. '+
    'This option will always succeed, further options are never tested.');

  EditorOptionsFoldInfoHTML[0].Name                           := LStr('dlgFoldHtmlNode',                'Node');
  EditorOptionsFoldInfoHTML[1].Name                           := LStr('dlgFoldHtmlComment',             'Comment');
  EditorOptionsFoldInfoHTML[2].Name                           := LStr('dlgFoldHtmlAsp',                 'ASP');

  EditorOptionsFoldInfoXML[0].Name                            := LStr('dlgFoldXmlNode',                 'Node');
  EditorOptionsFoldInfoXML[1].Name                            := LStr('dlgFoldXmlComment',              'Comment');
  EditorOptionsFoldInfoXML[2].Name                            := LStr('dlgFoldXmlCData',                'CData');
  EditorOptionsFoldInfoXML[3].Name                            := LStr('dlgFoldXmlDocType',              'DocType');
  EditorOptionsFoldInfoXML[4].Name                            := LStr('dlgFoldXmlProcess',              'Processing Instruction');

  EditorOptionsDividerInfoPas[0].Name                         := LStr('dlgDivPasUnitSectionName',       'Unit sections');
  EditorOptionsDividerInfoPas[1].Name                         := LStr('dlgDivPasUsesName',              'Uses clause');
  EditorOptionsDividerInfoPas[2].Name                         := LStr('dlgDivPasVarGlobalName',         'Var/Type');
  EditorOptionsDividerInfoPas[3].Name                         := LStr('dlgDivPasVarLocalName',          'Var/Type (local)');
  EditorOptionsDividerInfoPas[4].Name                         := LStr('dlgDivPasStructGlobalName',      'Class/Struct');
  EditorOptionsDividerInfoPas[5].Name                         := LStr('dlgDivPasStructLocalName',       'Class/Struct (local)');
  EditorOptionsDividerInfoPas[6].Name                         := LStr('dlgDivPasProcedureName',         'Procedure/Function');
  EditorOptionsDividerInfoPas[7].Name                         := LStr('dlgDivPasBeginEndName',          'Begin/End');
  EditorOptionsDividerInfoPas[8].Name                         := LStr('dlgDivPasTryName',               'Try/Except');

  AdditionalHighlightAttributes[ahaNone]                      := '';
  AdditionalHighlightAttributes[ahaTextBlock]                 := LStr('dlgAddHiAttrTextBlock',          'Text block');
  AdditionalHighlightAttributes[ahaExecutionPoint]            := LStr('dlgAddHiAttrExecutionPoint',     'Execution point');
  AdditionalHighlightAttributes[ahaEnabledBreakpoint]         := LStr('dlgAddHiAttrEnabledBreakpoint',  'Enabled breakpoint');
  AdditionalHighlightAttributes[ahaDisabledBreakpoint]        := LStr('dlgAddHiAttrDisabledBreakpoint', 'Disabled breakpoint');
  AdditionalHighlightAttributes[ahaInvalidBreakpoint]         := LStr('dlgAddHiAttrInvalidBreakpoint',  'Invalid breakpoint');
  AdditionalHighlightAttributes[ahaUnknownBreakpoint]         := LStr('dlgAddHiAttrUnknownBreakpoint',  'Unknown breakpoint');
  AdditionalHighlightAttributes[ahaErrorLine]                 := LStr('dlgAddHiAttrErrorLine',          'Error line');
  AdditionalHighlightAttributes[ahaIncrementalSearch]         := LStr('dlgAddHiAttrIncrementalSearch',  'Incremental search');
  AdditionalHighlightAttributes[ahaHighlightAll]              := LStr('dlgAddHiAttrHighlightAll',       'Incremental others');
  AdditionalHighlightAttributes[ahaBracketMatch]              := LStr('dlgAddHiAttrBracketMatch',       'Brackets highlight');
  AdditionalHighlightAttributes[ahaMouseLink]                 := LStr('dlgAddHiAttrMouseLink',          'Mouse link');
  AdditionalHighlightAttributes[ahaLineNumber]                := LStr('dlgAddHiAttrLineNumber',         'Line number');
  AdditionalHighlightAttributes[ahaLineHighlight]             := LStr('dlgAddHiAttrLineHighlight',      'Current line highlight');
  AdditionalHighlightAttributes[ahaModifiedLine]              := LStr('dlgAddHiAttrModifiedLine',       'Modified line');
  AdditionalHighlightAttributes[ahaCodeFoldingTree]           := LStr('dlgAddHiAttrCodeFoldingTree',    'Code folding tree');
  AdditionalHighlightAttributes[ahaHighlightWord]             := LStr('dlgAddHiAttrHighlightWord',      'Highlight current word');
  AdditionalHighlightAttributes[ahaFoldedCode]                := LStr('dlgAddHiAttrFoldedCode',         'Folded code marker');
  AdditionalHighlightAttributes[ahaFoldedCodeLine]            := LStr('dlgAddHiAttrFoldedCodeLine',     'Fold start-line');
  AdditionalHighlightAttributes[ahaHiddenCodeLine]            := LStr('dlgAddHiAttrHiddenCodeLine',     'Hide start-line');
  AdditionalHighlightAttributes[ahaWordGroup]                 := LStr('dlgAddHiAttrWordGroup',          'Word-Brackets');
  AdditionalHighlightAttributes[ahaTemplateEditCur]           := LStr('dlgAddHiAttrTemplateEditCur',    'Active Cell');
  AdditionalHighlightAttributes[ahaTemplateEditSync]          := LStr('dlgAddHiAttrTemplateEditSync',   'Syncronized Cells');
  AdditionalHighlightAttributes[ahaTemplateEditOther]         := LStr('dlgAddHiAttrTemplateEditOther',  'Other Cells');
  AdditionalHighlightAttributes[ahaSyncroEditCur]             := LStr('dlgAddHiAttrSyncroEditCur',      'Active Cell');
  AdditionalHighlightAttributes[ahaSyncroEditSync]            := LStr('dlgAddHiAttrSyncroEditSync',     'Syncronized Cells');
  AdditionalHighlightAttributes[ahaSyncroEditOther]           := LStr('dlgAddHiAttrSyncroEditOther',    'Other Cells');
  AdditionalHighlightAttributes[ahaSyncroEditArea]            := LStr('dlgAddHiAttrSyncroEditArea',     'Selected Area');
  AdditionalHighlightAttributes[ahaGutterSeparator]           := LStr('dlgAddHiAttrGutterSeparator',    'Gutter Separator');
  AdditionalHighlightAttributes[ahaGutter]                    := LStr('dlgGutter',                      'Gutter');
  AdditionalHighlightAttributes[ahaRightMargin]               := LStr('dlgRightMargin',                 'Right margin');
  AdditionalHighlightAttributes[ahaSpecialVisibleChars]       := LStr('dlgAddHiSpecialVisibleChars',    'Visualized Special Chars');
  AdditionalHighlightAttributes[ahaTopInfoHint]               := LStr('dlgTopInfoHint',                 'Current Class/Proc Hint');
  AdditionalHighlightAttributes[ahaCaretColor]                := LStr('dlgCaretColor',                  'Caret');
  AdditionalHighlightAttributes[ahaIfDefBlockInactive]        := LStr('dlgIfDefBlockInactive',          'Inactive $IFDEF code');
  AdditionalHighlightAttributes[ahaIfDefBlockActive]          := LStr('dlgIfDefBlockActive',            'Active $IFDEF code');
  AdditionalHighlightAttributes[ahaIfDefBlockTmpActive]       := LStr('dlgIfDefBlockTmpActive',         'Included mixed state $IFDEF code');
  AdditionalHighlightAttributes[ahaIfDefNodeInactive]         := LStr('dlgIfDefNodeInactive',           'Inactive $IFDEF node');
  AdditionalHighlightAttributes[ahaIfDefNodeActive]           := LStr('dlgIfDefNodeActive',             'Active $IFDEF node');
  AdditionalHighlightAttributes[ahaIfDefNodeTmpActive]        := LStr('dlgIfDefNodeTmpActive',          'Included mixed state $IFDEF node');
  AdditionalHighlightGroupNames[agnIfDef]                     := LStr('dlgAddHiAttrGroupIfDef',         'IfDef');

  AdditionalHighlightAttributes[ahaIdentComplWindow]          := LStr('dlgAddHiAttrDefaultWindow',      'Default Text / Window');
  AdditionalHighlightAttributes[ahaIdentComplWindowBorder]    := LStr('dlgAddHiAttrWindowBorder',       'Window border');
  AdditionalHighlightAttributes[ahaIdentComplWindowSelection] := LStr('dlgBlockGroupOptions',           'Selection');
  AdditionalHighlightAttributes[ahaIdentComplWindowHighlight] := LStr('dlgAddHiAttrHighlightPrefix',    'Highlight prefix');
  AdditionalHighlightGroupNames[agnIdentComplWindow]          := LStr('dlgIdentifierCompletion',        'Identifier Completion');

  AdditionalHighlightAttributes[ahaOutlineLevel1Color]        := LStr('dlgAddHiAttrOutlineLevel01Color', 'Level  1');
  AdditionalHighlightAttributes[ahaOutlineLevel2Color]        := LStr('dlgAddHiAttrOutlineLevel02Color', 'Level  2');
  AdditionalHighlightAttributes[ahaOutlineLevel3Color]        := LStr('dlgAddHiAttrOutlineLevel03Color', 'Level  3');
  AdditionalHighlightAttributes[ahaOutlineLevel4Color]        := LStr('dlgAddHiAttrOutlineLevel04Color', 'Level  4');
  AdditionalHighlightAttributes[ahaOutlineLevel5Color]        := LStr('dlgAddHiAttrOutlineLevel05Color', 'Level  5');
  AdditionalHighlightAttributes[ahaOutlineLevel6Color]        := LStr('dlgAddHiAttrOutlineLevel06Color', 'Level  6');
  AdditionalHighlightAttributes[ahaOutlineLevel7Color]        := LStr('dlgAddHiAttrOutlineLevel07Color', 'Level  7');
  AdditionalHighlightAttributes[ahaOutlineLevel8Color]        := LStr('dlgAddHiAttrOutlineLevel08Color', 'Level  8');
  AdditionalHighlightAttributes[ahaOutlineLevel9Color]        := LStr('dlgAddHiAttrOutlineLevel09Color', 'Level  9');
  AdditionalHighlightAttributes[ahaOutlineLevel10Color]       := LStr('dlgAddHiAttrOutlineLevel10Color', 'Level 10');
  AdditionalHighlightGroupNames[agnOutlineColors]             := LStr('dlgAddHiAttrGroupOutlineColors',  'Outline Colors');

  AdditionalHighlightGroupNames[agnDefault]                   := LStr('dlgAddHiAttrGroupDefault',       'Global');
  AdditionalHighlightGroupNames[agnText]                      := LStr('dlgAddHiAttrGroupText',          'Text');
  AdditionalHighlightGroupNames[agnLine]                      := LStr('dlgAddHiAttrGroupLine',          'Line');
  AdditionalHighlightGroupNames[agnTemplateMode]              := LStr('dlgAddHiAttrGroupTemplateEdit',  'Template Edit');
  AdditionalHighlightGroupNames[agnSyncronMode]               := LStr('dlgAddHiAttrGroupSyncroEdit',    'Syncron Edit');
  AdditionalHighlightGroupNames[agnGutter]                    := LStr('dlgAddHiAttrGroupGutter',        'Gutter');
end;



{ TEditOptLanguageInfo }

{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TEditOptLanguageInfo.Create;
begin
  inherited Create;
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TEditOptLanguageInfo.Destroy;
begin
  MappedAttributes.Free;
  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  SampleLineToAddAttr
 ------------------------------------------------------------------------------}
function TEditOptLanguageInfo.SampleLineToAddAttr(
  Line: Integer): TAdditionalHilightAttribute;
begin
  if Line < 1 then
    exit(ahaNone);
  for Result := Low(TAdditionalHilightAttribute)
    to High(TAdditionalHilightAttribute) do
    if (Result <> ahaNone) and (AddAttrSampleLines[Result] = Line) then
      exit;
  Result := ahaNone;
end;


{-------------------------------------------------------------------------------
  GetDefaultFilextension
 ------------------------------------------------------------------------------}
function TEditOptLanguageInfo.GetDefaultFilextension: String;
var
  p: Integer;
begin
  // read the first file extension
  p := 1;
  while (p <= length(FileExtensions)) and (FileExtensions[p] <> ';') do
    inc(p);
  if p > 1 then
    Result := '.' + copy(FileExtensions, 1, p - 1)
  else
    Result := '';
end;


{-------------------------------------------------------------------------------
  SetBothFilextensions
 ------------------------------------------------------------------------------}
procedure TEditOptLanguageInfo.SetBothFilextensions(const Extensions: string);
begin
  FileExtensions:=Extensions;
  DefaultFileExtensions:=Extensions;
end;


{-------------------------------------------------------------------------------
  prepare
 ------------------------------------------------------------------------------}
procedure TEditOptLanguageInfo.prepare(Syntax: TLazSyntaxHighlighter);
begin
  TheType := Syntax;
  DefaultCommentType := DefaultCommentTypes[TheType];
  SynClass := LazSyntaxHighlighterClasses[TheType];
end;



{ TEditOptLangMdInfo }

{-------------------------------------------------------------------------------
  prepare
 ------------------------------------------------------------------------------}
procedure TEditOptLangMdInfo.prepare(Syntax: TLazSyntaxHighlighter);
begin
  inherited Prepare(syntax);
  SetBothFilextensions('md');
  SampleSource := getSampleSource;
  AddAttrSampleLines[ahaTextBlock] := 5;
  CaretXY := Point(1,1);
  MappedAttributes := getMappedAttributes;;
end;


{-------------------------------------------------------------------------------
  getSampleSource
 ------------------------------------------------------------------------------}
function TEditOptLangMdInfo.getSampleSource: string;
begin
  result :=
      '# Markdown'                                                      + CrLf +
      '---'                                                             + CrLf +
      '__This  is `strong` and__ _em_.'                                 + CrLf +
      ''                                                                + CrLf +
      'So is ***this*** result: 10 + 10 = 20.00'                        + CrLf +
      ''                                                                + CrLf +
      'Here´s how you put `` `backticks` `` in a code span.'            + CrLf +
      ''                                                                + CrLf +
      '1. List'                                                         + CrLf +
      '    1. *Lorem ipsum dolor sit '                                  + CrLf +
      '       amet*, consetetur sadipscing elitr'                       + CrLf +
      ''                                                                + CrLf +
      '~~~'                                                             + CrLf +
      '   code_block = (10.00 * 5.00);'                                 + CrLf +
      '~~~'                                                             + CrLf +
      ''                                                                + CrLf +
      '## These should all get escaped:'                                + CrLf +
      '>  Hash: \#'                                                     + CrLf +
      '>  Backslash: \\'                                                + CrLf +
      ''                                                                + CrLf ;
end;


{-------------------------------------------------------------------------------
  getMappedAttributes
 ------------------------------------------------------------------------------}
function TEditOptLangMdInfo.getMappedAttributes: tStringList;
begin
    result:=tStringList.create;
    with result do
    begin
      Add('Comment=Comment');
      Add('Selector=Reserved_word');
      Add('Identifier=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
      Add('Number=Number');
      Add('Key=Key');
      Add('String=String');
    end;
end;



{ TEditOptLangCssInfo }

{-------------------------------------------------------------------------------
  prepare
 ------------------------------------------------------------------------------}
procedure TEditOptLangCssInfo.prepare(Syntax: TLazSyntaxHighlighter);
begin
  inherited Prepare(syntax);
  TheType := lshCSS;
  DefaultCommentType := DefaultCommentTypes[TheType];
  SynClass := LazSyntaxHighlighterClasses[TheType];

  SetBothFilextensions('css');
  SampleSource := getSampleSource;
  AddAttrSampleLines[ahaTextBlock] := 4;
  CaretXY := Point(1,1);
  MappedAttributes := getMappedAttributes;;
end;


{-------------------------------------------------------------------------------
  getSampleSource
 ------------------------------------------------------------------------------}
function TEditOptLangCssInfo.getSampleSource: string;
begin
  result :=
    '.field :hover {'                                                   + CrLf +
    '   display:inline;'                                                + CrLf +
    '   border:10px;'                                                   + CrLf +
    '   color: #555;'                                                   + CrLf +
    '/* comment */'                                                     + CrLf +
    '}'                                                                 + CrLf +
    ''                                                                  + CrLf ;
end;


{-------------------------------------------------------------------------------
  getMappedAttributes
 ------------------------------------------------------------------------------}
function TEditOptLangCssInfo.getMappedAttributes: tStringList;
begin
    result:=tStringList.create;
    with result do
    begin
      Add('Comment=Comment');
      Add('Selector=Reserved_word');
      Add('Identifier=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
      Add('Number=Number');
      Add('Key=Key');
      Add('String=String');
    end;
end;


{ TEditOptLangHtmlInfo }

{-------------------------------------------------------------------------------
  prepare
 ------------------------------------------------------------------------------}
procedure TEditOptLangHtmlInfo.prepare(Syntax: TLazSyntaxHighlighter);
begin
  inherited Prepare(syntax);
  TheType := lshHTML;
  DefaultCommentType := DefaultCommentTypes[TheType];
  SynClass := LazSyntaxHighlighterClasses[TheType];

  SetBothFilextensions('htm;html;xhtml');
  SampleSource := getSampleSource;
  AddAttrSampleLines[ahaTextBlock] := 11;
  CaretXY := Point(1,1);
  MappedAttributes := getMappedAttributes;;
end;


{-------------------------------------------------------------------------------
  getSampleSource
 ------------------------------------------------------------------------------}
function TEditOptLangHtmlInfo.getSampleSource: string;
begin
  result :=
  '<html>'                                                              + CrLf +
  '<title>Editor Sample source for html</title>'                        + CrLf +
  '<body bgcolor=#ffffff background="bg.jpg">'                          + CrLf +
  '<!-- Comment -->'                                                    + CrLf +
  '<img src="edito.jpg">'                                               + CrLf +
  '<p>'                                                                 + CrLf +
  '  Some Text'                                                         + CrLf +
  '  Ampersands: &nbsp;F&nbsp;P&nbsp;C'                                 + CrLf +
  '</p>'                                                                + CrLf +
  '<invalid_tag>'                                                       + CrLf +
  '<!-- Text Block -->'                                                 + CrLf +
  '</body>'                                                             + CrLf +
  '</html>'                                                             + CrLf +
  ''                                                                    + CrLf ;
end;


{-------------------------------------------------------------------------------
  getMappedAttributes
 ------------------------------------------------------------------------------}
function TEditOptLangHtmlInfo.getMappedAttributes: tStringList;
begin
    result:=tStringList.create;
    with result do
    begin
      Add('Comment=Comment');
      Add('Space=Space');
      Add('Identifier=Identifier');
      Add('Symbol=Symbol');
      Add('Number=Number');
      Add('Key=Key');
      Add('String=String');
    end;
end;


{ TEditOptLangXmlInfo }

{-------------------------------------------------------------------------------
  prepare
 ------------------------------------------------------------------------------}
procedure TEditOptLangXmlInfo.prepare(Syntax: TLazSyntaxHighlighter);
begin
  inherited Prepare(syntax);
  TheType := lshXML;
  DefaultCommentType := DefaultCommentTypes[TheType];
  SynClass := LazSyntaxHighlighterClasses[TheType];

  SetBothFilextensions('xml;xsd;xsl;xslt;dtd;lpi;lps;lpk;wsdl;svg');
  SampleSource := getSampleSource;
  AddAttrSampleLines[ahaTextBlock] := 8;
  CaretXY := Point(1,1);
  MappedAttributes := getMappedAttributes;;
end;


{-------------------------------------------------------------------------------
  getSampleSource
 ------------------------------------------------------------------------------}
function TEditOptLangXmlInfo.getSampleSource: string;
begin
  result :=
    '<?xml version="1.0"?>'                                             + CrLf +
    '<!DOCTYPE root ['                                                  + CrLf +
    '  ]>'                                                              + CrLf +
    '<!-- Comment -->'                                                  + CrLf +
    '<root version="&test;">'                                           + CrLf +
    '  <![CDATA[ **CDATA section** ]]>'                                 + CrLf +
    '</root>'                                                           + CrLf +
    '<!-- Text Block -->'                                               + CrLf +
    ''                                                                  + CrLf ;
end;


{-------------------------------------------------------------------------------
  getMappedAttributes
 ------------------------------------------------------------------------------}
function TEditOptLangXmlInfo.getMappedAttributes: tStringList;
begin
    result:=tStringList.create;
    with result do
    begin
      Add('Element=Reserved_word');
      Add('Comment=Comment');
      Add('Text=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
    end;
end;


{ TEditOptLangJsInfo }

{-------------------------------------------------------------------------------
  prepare
 ------------------------------------------------------------------------------}
procedure TEditOptLangJsInfo.prepare(Syntax: TLazSyntaxHighlighter);
begin
  inherited Prepare(syntax);
  TheType := lshJScript;
  DefaultCommentType := DefaultCommentTypes[TheType];
  SynClass := LazSyntaxHighlighterClasses[TheType];

  SetBothFilextensions('js');
  SampleSource := getSampleSource;
  AddAttrSampleLines[ahaTextBlock] := 2;
  CaretXY := Point(1,1);
  MappedAttributes := getMappedAttributes;;
end;


{-------------------------------------------------------------------------------
  getSampleSource
 ------------------------------------------------------------------------------}
function TEditOptLangJsInfo.getSampleSource: string;
begin
  result :=
    '/* JScript */'                                                     + CrLf +
    'var semafor={'                                                     + CrLf +
    '  semafor:0,'                                                      + CrLf +
    '  timer:null,'                                                     + CrLf +
    '  name:"Name",'                                                    + CrLf +
    '  clear: function(){'                                              + CrLf +
    '    try{'                                                          + CrLf +
    '      this.semafor=0;'                                             + CrLf +
    '      clearTimeout(this.timer);'                                   + CrLf +
    '    }  catch (e)  { }'                                             + CrLf +
    '  }'                                                               + CrLf +
    '};'                                                                + CrLf +
    ''                                                                  + CrLf +
    '/* Text Block */'                                                  + CrLf +
    ''                                                                  + CrLf ;
end;


{-------------------------------------------------------------------------------
  getMappedAttributes
 ------------------------------------------------------------------------------}
function TEditOptLangJsInfo.getMappedAttributes: tStringList;
begin
    result:=tStringList.create;
    with result do
    begin
      Add('Comment=Comment');
      Add('Documentation=Comment');
      Add('Identifier=Identifier');
      Add('Reserved_word=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
end;


{ TEditOptLangList }

{-------------------------------------------------------------------------------
  GetInfos
 ------------------------------------------------------------------------------}
function TEditOptLangList.GetInfos(Index: Integer): TEditOptLanguageInfo;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.Create('TEditOptLangList.GetInfos Index '
      + IntToStr(Index) + ' out of bounds. Count=' + IntToStr(Count));
  Result := TEditOptLanguageInfo(inherited Items[Index]);
end;


{-------------------------------------------------------------------------------
  Clear
 ------------------------------------------------------------------------------}
procedure TEditOptLangList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited Clear;
end;


{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TEditOptLangList.Create;
var
  NewInfo: TEditOptLanguageInfo;
begin
  inherited Create;

  { create the meta information for each available highlighter.
    Please keep the pascal highlighter at the top. The rest can be ordered as you
    like.
  }

  // create info for Markdown
  NewInfo := TEditOptLangMdInfo.Create;
  NewInfo.Prepare(lshMd);
  Add(NewInfo);

  // create info for html
  NewInfo := TEditOptLangHtmlInfo.Create;
  NewInfo.Prepare(lshHTML);
  Add(NewInfo);

  // create info for CSS
  NewInfo := TEditOptLangCssInfo.Create;
  NewInfo.Prepare(lshCSS);
  Add(NewInfo);

  // create info for XML
  NewInfo := TEditOptLangXmlInfo.Create;
  NewInfo.Prepare(lshXML);
  Add(NewInfo);

  // create info for JScript
  NewInfo := TEditOptLangJsInfo.Create;
  NewInfo.Prepare(lshJScript);
  Add(NewInfo);
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TEditOptLangList.Destroy;
begin
  Clear;
  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  FindByName
 ------------------------------------------------------------------------------}
function TEditOptLangList.FindByName(const Name: String): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (UTF8CompareText(
      Items[Result].SynClass.GetLanguageName, Name) <> 0) do
    dec(Result);
end;


{-------------------------------------------------------------------------------
  FindByClass
 ------------------------------------------------------------------------------}
function TEditOptLangList.FindByClass(
  CustomSynClass: TCustomSynClass): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Items[Result].SynClass <> CustomSynClass) do
    dec(Result);
end;


{-------------------------------------------------------------------------------
  FindByHighlighter
 ------------------------------------------------------------------------------}
function TEditOptLangList.FindByHighlighter(Hilighter:
  TSynCustomHighlighter): Integer;
begin
  if Hilighter <> Nil then
    Result := FindByClass(TCustomSynClass(Hilighter.ClassType))
  else
    Result := -1;
end;


{-------------------------------------------------------------------------------
  FindByType
 ------------------------------------------------------------------------------}
function TEditOptLangList.FindByType(AType: TLazSyntaxHighlighter): Integer;
begin
  AType := CompatibleLazSyntaxHilighter[AType];
  Result := Count - 1;
  while (Result >= 0) and (Items[Result].TheType <> AType) do
    dec(Result);
end;


{-------------------------------------------------------------------------------
  GetDefaultFilextension
 ------------------------------------------------------------------------------}
function TEditOptLangList.GetDefaultFilextension(
  AType: TLazSyntaxHighlighter): String;
var
  i: Integer;
begin
  i := FindByType(AType);
  if i >= 0 then
    Result := Items[i].GetDefaultFilextension
  else
    Result := '';
end;


{-------------------------------------------------------------------------------
  GetInfoByType
 ------------------------------------------------------------------------------}
function TEditOptLangList.GetInfoByType(AType: TLazSyntaxHighlighter
  ): TEditOptLanguageInfo;
var
  i: LongInt;
begin
  i:=FindByType(AType);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;



{ TEditorMouseOptions }

{-------------------------------------------------------------------------------
  ClearUserSchemes
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.ClearUserSchemes;
begin
  while FUserSchemes.Count > 0 do begin
    FUserSchemes.Objects[0].Free;
    FUserSchemes.Delete(0);
  end;
end;


{-------------------------------------------------------------------------------
  GetUserSchemeNames
 ------------------------------------------------------------------------------}
function TEditorMouseOptions.GetUserSchemeNames(Index: Integer): String;
begin
  Result := TEditorMouseOptions(FUserSchemes.Objects[Index]).Name;
end;


{-------------------------------------------------------------------------------
  GetUserSchemes
 ------------------------------------------------------------------------------}
function TEditorMouseOptions.GetUserSchemes(Index: String): TEditorMouseOptions;
var
  i: Integer;
begin
  i := IndexOfUserScheme(Index);
  if i >= 0 then
    Result := UserSchemesAtPos[i]
  else
    Result := nil;
end;


{-------------------------------------------------------------------------------
  GetUserSchemesAtPos
 ------------------------------------------------------------------------------}
function TEditorMouseOptions.GetUserSchemesAtPos(Index: Integer): TEditorMouseOptions;
begin
  Result := TEditorMouseOptions(FUserSchemes.Objects[Index]);
end;


{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TEditorMouseOptions.Create;
begin
  inherited Create;
  Reset;
  FMainActions                := TSynEditMouseActions.Create(nil);
  FSelActions                 := TSynEditMouseActions.Create(nil);
  FTextActions                := TSynEditMouseActions.Create(nil);
  FGutterActions              := TSynEditMouseActions.Create(nil);
  FGutterActionsFold          := TSynEditMouseActions.Create(nil);
  FGutterActionsFoldExp       := TSynEditMouseActions.Create(nil);
  FGutterActionsFoldCol       := TSynEditMouseActions.Create(nil);
  FGutterActionsLines         := TSynEditMouseActions.Create(nil);
  FGutterActionsChanges       := TSynEditMouseActions.Create(nil);
  FGutterActionsOverView      := TSynEditMouseActions.Create(nil);
  FGutterActionsOverViewMarks := TSynEditMouseActions.Create(nil);
  FUserSchemes := TQuickStringlist.Create;
  FVersion := 0;
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TEditorMouseOptions.Destroy;
begin
  ClearUserSchemes;
  FUserSchemes.Free;
  FMainActions.Free;
  FTextActions.Free;
  FSelActions.Free;
  FGutterActions.Free;
  FGutterActionsFold.Free;
  FGutterActionsFoldExp.Free;
  FGutterActionsFoldCol.Free;
  FGutterActionsLines.Free;
  FGutterActionsChanges.Free;
  FGutterActionsOverView.Free;
  FGutterActionsOverViewMarks.Free;
  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  Reset
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.Reset;
begin
  FCustomSavedActions          := False;
  FGutterLeft                  := moglUpClickAndSelect;
  // left multi
  FTextDoubleLeftClick         := mbaSelectWords;
  FTextTripleLeftClick         := mbaSelectSetLineSmart;
  FTextQuadLeftClick           := mbaSelectSetPara;
  FTextShiftDoubleLeftClick    := mbaNone;
  FTextAltDoubleLeftClick      := mbaNone;
  FTextCtrlDoubleLeftClick     := mbaNone;
  // left
  FTextAltLeftClick            := mbaSelectColumn;
  FTextCtrlLeftClick           := mbaDeclarationJump;
  FTextAltCtrlLeftClick        := mbaNone;
  FTextShiftLeftClick          := mbaNone;
  FTextShiftAltLeftClick       := mbaNone;
  FTextShiftCtrlLeftClick      := mbaMultiCaretToggle;
  FTextShiftAltCtrlLeftClick   := mbaNone;
  // middle
  FTextMiddleClick             := mbaPaste;
  FTextAltMiddleClick          := mbaNone;
  FTextCtrlMiddleClick         := mbaZoomReset;
  FTextShiftMiddleClick        := mbaNone;
  FTextAltCtrlMiddleClick      := mbaNone;
  FTextShiftAltMiddleClick     := mbaNone;
  FTextShiftAltCtrlMiddleClick := mbaNone;
  FTextShiftCtrlMiddleClick    := mbaNone;
  // wheel
  FWheel                       := mwaScroll;
  FCtrlWheel                   := mwaZoom;
  FAltWheel                    := mwaScrollPageLessOne;
  FShiftWheel                  := mwaScrollSingleLine;
  FAltCtrlWheel                := mwaNone;
  FShiftCtrlWheel              := mwaNone;
  FShiftAltWheel               := mwaNone;
  FShiftAltCtrlWheel           := mwaNone;
  // right
  FTextRightClick              := mbaContextMenu;
  FTextAltCtrlRightClick       := mbaNone;
  FTextAltRightClick           := mbaNone;
  FTextCtrlRightClick          := mbaContextMenuTab;
  FTextShiftAltCtrlRightClick  := mbaNone;
  FTextShiftAltRightClick      := mbaNone;
  FTextShiftCtrlRightClick     := mbaNone;
  FTextShiftRightClick         := mbaNone;
  // extra-1 click
  FTextExtra1Click             := mbaHistoryBack;
  FTextAltCtrlExtra1Click      := mbaNone;
  FTextAltExtra1Click          := mbaNone;
  FTextCtrlExtra1Click         := mbaNone;
  FTextShiftAltCtrlExtra1Click := mbaNone;
  FTextShiftAltExtra1Click     := mbaNone;
  FTextShiftCtrlExtra1Click    := mbaNone;
  FTextShiftExtra1Click        := mbaNone;
  // extra-2 click
  FTextExtra2Click             := mbaHistoryForw;
  FTextAltCtrlExtra2Click      := mbaNone;
  FTextAltExtra2Click          := mbaNone;
  FTextCtrlExtra2Click         := mbaNone;
  FTextShiftAltCtrlExtra2Click := mbaNone;
  FTextShiftAltExtra2Click     := mbaNone;
  FTextShiftCtrlExtra2Click    := mbaNone;
  FTextShiftExtra2Click        := mbaNone;

  FTextRightMoveCaret          := False;
  FTextDrag                    := True;
  FSelectOnLineNumbers         := True;
end;


{-------------------------------------------------------------------------------
  ResetGutterToDefault
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.ResetGutterToDefault;

  procedure AddStartSel(List: TSynEditMouseActions);
  begin
    with List do begin
      AddCommand(emcStartSelections,   True, mbXLeft, ccAny, cdDown, [],               [ssShift], emcoSelectionStart);
      AddCommand(emcStartSelections,   True, mbXLeft, ccAny, cdDown, [ssShift],        [ssShift], emcoSelectionContinue);
    end;
  end;

var
  CDir: TSynMAClickDir;
  R: TSynMAUpRestrictions;
begin
  FGutterActions.Clear;
  FGutterActionsFold.Clear;
  FGutterActionsFoldExp.Clear;
  FGutterActionsFoldCol.Clear;
  FGutterActionsLines.Clear;
  FGutterActionsChanges.Clear;
  FGutterActionsOverView.Clear;
  FGutterActionsOverViewMarks.Clear;

  with FGutterActions do begin
    AddCommand(emcContextMenu,         False, mbXRight,  ccSingle, cdUp, [], []);
  end;
  with FGutterActionsFold do begin
    AddCommand(emcCodeFoldContextMenu, False, mbXRight,  ccSingle, cdUp, [], []);
  end;

  CDir := cdDown;
  R := [];
  if FGutterLeft = moglUpClickAndSelect then begin
    CDir := cdUp;
    R := crRestrictAll;
    AddStartSel(FGutterActions);
  end;

  with FGutterActions do begin
    AddCommand(emcOnMainGutterClick,   False, mbXLeft,   ccAny,    CDir, R, [], []);  // breakpoint
  end;

  if FGutterLeft in [moglUpClickAndSelect, moglUpClickAndSelectRighHalf] then begin
    CDir := cdUp;
    R := crRestrictAll;
    AddStartSel(FGutterActionsChanges);
  end;

  with FGutterActionsChanges do begin
    if FGutterLeft = moGLDownClick then
      AddCommand(emcNone,   False, mbXLeft,   ccAny,    cdDown, [], []);
    AddCommand(emcNone,   False, mbXLeft,   ccAny,    cdUp, [], []);
  end;

  if FGutterLeft = moglUpClickAndSelectRighHalf then begin
    if not FSelectOnLineNumbers then
      AddStartSel(FGutterActionsLines);
    AddStartSel(FGutterActionsFold);
  end;

  if FSelectOnLineNumbers then begin
    with FGutterActionsLines do begin
      AddCommand(emcStartLineSelectionsNoneEmpty,   True, mbXLeft, ccAny, cdDown, [],               [ssShift], emcoSelectionStart);
      AddCommand(emcStartLineSelectionsNoneEmpty,   True, mbXLeft, ccAny, cdDown, [ssShift],        [ssShift], emcoSelectionContinue);
      AddCommand(emcNone,   False, mbXLeft,   ccAny,    cdUp, [], []);
    end;
  end;

  with FGutterActionsFold do begin
    AddCommand(emcNone,                False, mbXLeft,   ccAny,    CDir, R, [], []);
  end;
  with FGutterActionsFoldCol do begin
    AddCommand(emcCodeFoldCollaps,     False, mbXLeft,   ccAny,    CDir, R, [ssAlt],   [ssAlt, SYNEDIT_LINK_MODIFIER], emcoCodeFoldCollapsOne);
    AddCommand(emcCodeFoldExpand,      False, mbXLeft,   ccAny,    CDir, R, [SYNEDIT_LINK_MODIFIER],  [ssAlt, SYNEDIT_LINK_MODIFIER], emcoCodeFoldExpandAll);
    AddCommand(emcCodeFoldExpand,      False, mbXLeft,   ccAny,    CDir, R, [],        [],              emcoCodeFoldExpandOne);
    // TODO: why depend on FTextMiddleClick?
    if FTextMiddleClick <> mbaNone then
      AddCommand(emcCodeFoldCollaps,   False, mbXMiddle, ccAny,    CDir, R, [],       [],               emcoCodeFoldCollapsOne);
    // do not allow selection, over colapse/expand icons. Those may depend cursor pos (e.g. hide selected lines)
    if CDir = cdUp then
      AddCommand(emcNone,              False, mbXLeft,   ccAny,    cdDown, [], []);
  end;
  with FGutterActionsFoldExp do begin
    AddCommand(emcCodeFoldCollaps,     False, mbXLeft,   ccAny,    CDir, R, [],       [SYNEDIT_LINK_MODIFIER], emcoCodeFoldCollapsOne);
    AddCommand(emcCodeFoldCollaps,     False, mbXLeft,   ccAny,    CDir, R, [SYNEDIT_LINK_MODIFIER], [SYNEDIT_LINK_MODIFIER], emcoCodeFoldCollapsAll);
    // TODO: why depend on FTextMiddleClick?
    if FTextMiddleClick <> mbaNone then
      AddCommand(emcCodeFoldCollaps,   False, mbXMiddle, ccAny,    CDir, R, [],       [],       emcoCodeFoldCollapsOne);
    // do not allow selection, over colapse/expand icons. Those may depend cursor pos (e.g. hide selected lines)
    if CDir = cdUp then
      AddCommand(emcNone,              False, mbXLeft,   ccAny,    cdDown, [], []);
  end;

  with FGutterActionsOverViewMarks do begin
    R := R - [crLastDownPosSameLine];
    if R <> [] then
      R := R + [crAllowFallback];
    AddCommand(emcOverViewGutterGotoMark, True, mbXLeft, ccAny,  CDir, R, [], [ssShift, ssCtrl, ssAlt]);
  end;
  with FGutterActionsOverView do begin
    if R <> [] then
      R := R + [crLastDownPosSearchAll];
    AddCommand(emcOverViewGutterScrollTo, True, mbXLeft, ccAny,  CDir, R, [], [ssShift, ssCtrl, ssAlt]);
  end;

end;


{-------------------------------------------------------------------------------
  ResetTextToDefault
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.ResetTextToDefault;

  procedure AddBtnClick(AnAction: TMouseOptButtonAction; const AButton: TSynMouseButton;
    AShift, AShiftMask: TShiftState; AddLinkDummy: Boolean = False;
    ASelContShift: TShiftState = []; AClickCount: TSynMAClickCount = ccSingle;
    AMoveCaret: Boolean = True; ADir: TSynMAClickDir = cdUp);

      procedure AddSelCommand(const ACmd: TSynEditorMouseCommand);
      begin
        AShiftMask := AShiftMask + ASelContShift;
        FTextActions.AddCommand(  ACmd, True, AButton, AClickCount, cdDown, AShift,               AShiftMask, emcoSelectionStart);
        if ASelContShift <> [] then
          FTextActions.AddCommand(ACmd, True, AButton, AClickCount, cdDown, AShift+ASelContShift, AShiftMask, emcoSelectionContinue);
      end;

  begin
    with FTextActions do begin
      case AnAction of
        mbaNone: {nothing};
        mbaSelect:       AddSelCommand(emcStartSelections);
        mbaSelectColumn: AddSelCommand(emcStartColumnSelections);
        mbaSelectLine:   AddSelCommand(emcStartLineSelections);
        mbaSelectWords:  AddCommand(emcStartSelectWords,  True, AButton, AClickCount, cdDown, AShift, AShiftMask, emcoSelectionStart);
        mbaSelectSetWord:
            AddCommand(emcSelectWord,       True,  AButton, AClickCount, ADir, AShift, AShiftMask);
        mbaSelectSetLineSmart:
            AddCommand(emcSelectLine,       True,  AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectLineSmart);
        mbaSelectSetLineFull:
            AddCommand(emcSelectLine,       True,  AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectLineFull);
        mbaSelectSetPara:
            AddCommand(emcSelectPara,       True,  AButton, AClickCount, ADir, AShift, AShiftMask);
        mbaPaste:            // TODOS act on up? but needs to prevent selection on down
            AddCommand(emcPasteSelection,   True,  AButton, AClickCount, cdDown,  AShift, AShiftMask, 0, 0, 0, True);
        mbaDeclarationJump,
        mbaDeclarationOrBlockJump: begin
            if AddLinkDummy then
              AddCommand(emcMouseLink,      False, AButton, AClickCount, ADir,    [SYNEDIT_LINK_MODIFIER], [SYNEDIT_LINK_MODIFIER], emcoMouseLinkShow, 999);
            AddCommand(emcMouseLink,        False, AButton, AClickCount, ADir,    AShift, AShiftMask);
            if AnAction = mbaDeclarationOrBlockJump then
              AddCommand(emcSynEditCommand, True,  AButton, AClickCount, ADir,    AShift, AShiftMask, 1000, 1);
          end;
        mbaAddHistoryPoint:
          AddCommand(emcSynEditCommand,     True,  AButton, AClickCount, ADir, AShift, AShiftMask, 1001);
        mbaHistoryBack:
          AddCommand(emcSynEditCommand,     False, AButton, AClickCount, ADir, AShift, AShiftMask, 1002);
        mbaHistoryForw:
          AddCommand(emcSynEditCommand,     False, AButton, AClickCount, ADir, AShift, AShiftMask, 1003);
        mbaSetFreeBookmark:
          AddCommand(emcSynEditCommand,     True,  AButton, AClickCount, ADir, AShift, AShiftMask, 1004);
        mbaZoomReset: begin
            AddCommand(emcWheelZoomNorm,    False, AButton, AClickCount, ADir, AShift, AShiftMask);
            FMainActions.AddCommand(emcWheelZoomNorm,    False,  AButton, AClickCount, ADir, AShift, AShiftMask);
          end;
        mbaContextMenu:
            AddCommand(emcContextMenu, AMoveCaret, AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectionCaretMoveNever);
        mbaContextMenuDebug:
            AddCommand(emcContextMenu, True,       AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectionCaretMoveOutside, 0, 1);
        mbaContextMenuTab:
            AddCommand(emcContextMenu, True,       AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectionCaretMoveOutside, 0, 2);
        mbaMultiCaretToggle:
          begin
            // Toggle
          end;
      end;
    end;
  end;

  procedure AddWheelAct(AnAction: TMouseOptWheelAction; const AShift, AShiftMask: TShiftState);
  var
    opt: TSynEditorMouseCommandOpt;
    opt2: integer;
  begin
    opt2 := 0;
    with FMainActions do begin
      case AnAction of
        mwaNone: {nothing};
        mwaScroll:                 opt := emcoWheelScrollSystem;
        mwaScrollSingleLine:       opt := emcoWheelScrollLines;
        mwaScrollPage:             opt := emcoWheelScrollPages;
        mwaScrollPageLessOne:      opt := emcoWheelScrollPagesLessOne;
        mwaScrollHalfPage: begin
                                   opt := emcoWheelScrollPages;
                                   opt2 := 50;
          end;
        mwaScrollHoriz:            opt := emcoWheelScrollSystem;
        mwaScrollHorizSingleLine:  opt := emcoWheelScrollLines;
        mwaScrollHorizPage:        opt := emcoWheelScrollPages;
        mwaScrollHorizPageLessOne: opt := emcoWheelScrollPagesLessOne;
        mwaScrollHorizHalfPage: begin
                                   opt := emcoWheelScrollPages;
                                   opt2 := 50;
          end;
        mwaZoom: begin
            AddCommand(emcWheelZoomOut, False,  mbXWheelDown, ccAny, cdDown, AShift, AShiftMask);
            AddCommand(emcWheelZoomIn,  False,  mbXWheelUp,   ccAny, cdDown, AShift, AShiftMask);
          end;
      end;

      if AnAction in [mwaScroll, mwaScrollSingleLine, mwaScrollPage, mwaScrollPageLessOne, mwaScrollHalfPage] then begin
        AddCommand(emcWheelVertScrollDown,       False,  mbXWheelDown, ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
        AddCommand(emcWheelVertScrollUp,         False,  mbXWheelUp,   ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
      end;
      if AnAction in [mwaScrollHoriz, mwaScrollHorizSingleLine, mwaScrollHorizPage, mwaScrollHorizPageLessOne, mwaScrollHorizHalfPage] then begin
        AddCommand(emcWheelHorizScrollDown,       False,  mbXWheelDown, ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
        AddCommand(emcWheelHorizScrollUp,         False,  mbXWheelUp,   ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
      end;

    end;
  end;

var
  ModKeys, SelKey: TShiftState;
begin
  FMainActions.Clear;
  FSelActions.Clear;
  FTextActions.Clear;

  // Left Btn
  ModKeys := [ssShift];
  if FTextAltLeftClick           <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextCtrlLeftClick          <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextAltCtrlLeftClick       <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltLeftClick      <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextShiftCtrlLeftClick     <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltCtrlLeftClick  <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextAltDoubleLeftClick     <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextCtrlDoubleLeftClick    <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];

  if FTextShiftLeftClick = mbaNone
  then SelKey := [ssShift]
  else SelKey := [];
  AddBtnClick(mbaSelect,                  mbXLeft,   [],                      ModKeys, False, SelKey);
  AddBtnClick(FTextShiftLeftClick,        mbXLeft,   [ssShift],               ModKeys, False, SelKey);

  if FTextShiftCtrlLeftClick = mbaNone
  then SelKey := [ssShift]
  else SelKey := [];
  AddBtnClick(FTextCtrlLeftClick,         mbXLeft,   [SYNEDIT_LINK_MODIFIER],          ModKeys, False, SelKey);
  AddBtnClick(FTextShiftCtrlLeftClick,    mbXLeft,   [ssShift, SYNEDIT_LINK_MODIFIER], ModKeys, False, SelKey);

  if FTextShiftAltLeftClick = mbaNone
  then SelKey := [ssShift]
  else SelKey := [];
  AddBtnClick(FTextAltLeftClick,          mbXLeft,   [ssAlt],                 ModKeys, False, SelKey);
  AddBtnClick(FTextShiftAltLeftClick,     mbXLeft,   [ssShift, ssAlt],               ModKeys, False, SelKey);

  if FTextShiftAltCtrlLeftClick = mbaNone
  then SelKey := [ssShift]
  else SelKey := [];
  AddBtnClick(FTextAltCtrlLeftClick,      mbXLeft, [ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, SelKey);
  AddBtnClick(FTextShiftAltCtrlLeftClick, mbXLeft, [ssShift, ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, SelKey);

  SelKey := [];
  AddBtnClick(FTextDoubleLeftClick,        mbXLeft,   [], ModKeys, False, SelKey, ccDouble);
  AddBtnClick(FTextTripleLeftClick,        mbXLeft,   [], ModKeys, False, SelKey, ccTriple);
  AddBtnClick(FTextQuadLeftClick,          mbXLeft,   [], ModKeys, False, SelKey, ccQuad);
  AddBtnClick(FTextShiftDoubleLeftClick,   mbXLeft,   [ssShift],               ModKeys, False, SelKey, ccDouble);
  AddBtnClick(FTextCtrlDoubleLeftClick,    mbXLeft,   [SYNEDIT_LINK_MODIFIER], ModKeys, False, SelKey, ccDouble);
  AddBtnClick(FTextAltDoubleLeftClick,     mbXLeft,   [ssAlt],                 ModKeys, False, SelKey, ccDouble);


  SelKey := [];
  ModKeys := [];
  if FTextShiftMiddleClick         <> mbaNone then ModKeys := ModKeys + [ssShift];
  if FTextCtrlMiddleClick          <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextAltMiddleClick           <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextAltCtrlMiddleClick       <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftCtrlMiddleClick     <> mbaNone then ModKeys := ModKeys + [ssShift] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltMiddleClick      <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt];
  if FTextShiftAltCtrlMiddleClick  <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt] + [SYNEDIT_LINK_MODIFIER];
  AddBtnClick(FTextMiddleClick,     mbXMiddle, [], ModKeys, FTextCtrlMiddleClick = mbaNone);
  AddBtnClick(FTextShiftMiddleClick,mbXMiddle, [ssShift], ModKeys);
  AddBtnClick(FTextAltMiddleClick,  mbXMiddle, [ssAlt], ModKeys);
  AddBtnClick(FTextCtrlMiddleClick, mbXMiddle, [SYNEDIT_LINK_MODIFIER], ModKeys);
  AddBtnClick(FTextAltCtrlMiddleClick,      mbXMiddle, [ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys);
  AddBtnClick(FTextShiftCtrlMiddleClick,    mbXMiddle, [ssShift, SYNEDIT_LINK_MODIFIER], ModKeys);
  AddBtnClick(FTextShiftAltMiddleClick,     mbXMiddle, [ssShift, ssAlt], ModKeys);
  AddBtnClick(FTextShiftAltCtrlMiddleClick, mbXMiddle, [ssShift, ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys);

  SelKey := [];
  ModKeys := [];
  if FTextShiftRightClick         <> mbaNone then ModKeys := ModKeys + [ssShift];
  if FTextCtrlRightClick          <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextAltRightClick           <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextAltCtrlRightClick       <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftCtrlRightClick     <> mbaNone then ModKeys := ModKeys + [ssShift] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltRightClick      <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt];
  if FTextShiftAltCtrlRightClick  <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt] + [SYNEDIT_LINK_MODIFIER];
  AddBtnClick(FTextRightClick,     mbXRight, [], ModKeys, FTextCtrlRightClick = mbaNone, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextShiftRightClick,mbXRight, [ssShift], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextAltRightClick,  mbXRight, [ssAlt], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextCtrlRightClick, mbXRight, [SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextAltCtrlRightClick,      mbXRight, [ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextShiftCtrlRightClick,    mbXRight, [ssShift, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextShiftAltRightClick,     mbXRight, [ssShift, ssAlt], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextShiftAltCtrlRightClick, mbXRight, [ssShift, ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, FTextRightMoveCaret);

  SelKey := [];
  ModKeys := [];
  if FTextShiftExtra1Click         <> mbaNone then ModKeys := ModKeys + [ssShift];
  if FTextCtrlExtra1Click          <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextAltExtra1Click           <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextAltCtrlExtra1Click       <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftCtrlExtra1Click     <> mbaNone then ModKeys := ModKeys + [ssShift] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltExtra1Click      <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt];
  if FTextShiftAltCtrlExtra1Click  <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt] + [SYNEDIT_LINK_MODIFIER];
  AddBtnClick(FTextExtra1Click,     mbXExtra1, [], ModKeys, FTextCtrlExtra1Click = mbaNone, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftExtra1Click,mbXExtra1, [ssShift], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextAltExtra1Click,  mbXExtra1, [ssAlt], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextCtrlExtra1Click, mbXExtra1, [SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextAltCtrlExtra1Click,      mbXExtra1, [ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftCtrlExtra1Click,    mbXExtra1, [ssShift, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftAltExtra1Click,     mbXExtra1, [ssShift, ssAlt], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftAltCtrlExtra1Click, mbXExtra1, [ssShift, ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);

  // TODO: on w32 extra btn do not call mouse up
  SelKey := [];
  ModKeys := [];
  if FTextShiftExtra2Click         <> mbaNone then ModKeys := ModKeys + [ssShift];
  if FTextCtrlExtra2Click          <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextAltExtra2Click           <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextAltCtrlExtra2Click       <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftCtrlExtra2Click     <> mbaNone then ModKeys := ModKeys + [ssShift] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltExtra2Click      <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt];
  if FTextShiftAltCtrlExtra2Click  <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt] + [SYNEDIT_LINK_MODIFIER];
  AddBtnClick(FTextExtra2Click,     mbXExtra2, [], ModKeys, FTextCtrlExtra2Click = mbaNone, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftExtra2Click,mbXExtra2, [ssShift], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextAltExtra2Click,  mbXExtra2, [ssAlt], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextCtrlExtra2Click, mbXExtra2, [SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextAltCtrlExtra2Click,      mbXExtra2, [ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftCtrlExtra2Click,    mbXExtra2, [ssShift, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftAltExtra2Click,     mbXExtra2, [ssShift, ssAlt], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftAltCtrlExtra2Click, mbXExtra2, [ssShift, ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);

  ModKeys := [];
  if FShiftWheel         <> mwaNone then ModKeys := ModKeys + [ssShift];
  if FCtrlWheel          <> mwaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FAltWheel           <> mwaNone then ModKeys := ModKeys + [ssAlt];
  if FAltCtrlWheel       <> mwaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FShiftCtrlWheel     <> mwaNone then ModKeys := ModKeys + [ssShift] + [SYNEDIT_LINK_MODIFIER];
  if FShiftAltWheel      <> mwaNone then ModKeys := ModKeys + [ssShift, ssAlt];
  if FShiftAltCtrlWheel  <> mwaNone then ModKeys := ModKeys + [ssShift, ssAlt] + [SYNEDIT_LINK_MODIFIER];
  AddWheelAct(FWheel, [], []);
  AddWheelAct(FCtrlWheel,  [ssCtrl],  ModKeys);
  AddWheelAct(FAltWheel,   [ssAlt],   ModKeys);
  AddWheelAct(FShiftWheel, [ssShift], ModKeys);
  AddWheelAct(FAltCtrlWheel,      [ssAlt, ssCtrl], ModKeys);
  AddWheelAct(FShiftCtrlWheel,    [ssShift, ssCtrl], ModKeys);
  AddWheelAct(FShiftAltWheel,     [ssShift, ssAlt], ModKeys);
  AddWheelAct(FShiftAltCtrlWheel, [ssShift, ssAlt, ssCtrl], ModKeys);

  if FTextDrag then
    with FSelActions do begin
      AddCommand(emcStartDragMove, False, mbXLeft, ccSingle, cdDown, [], [], emcoNotDragedNoCaretOnUp);
    end;
    FTextActions.AddCommand(emcNone, True, mbXLeft, ccSingle, cdUp, [], [], 0, 99);
end;


{-------------------------------------------------------------------------------
  ResetToUserScheme
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.ResetToUserScheme;
var
  i: LongInt;
begin
  i := SelectedUserSchemeIndex;
  if i < 0 then exit;
  AssignActions(UserSchemesAtPos[i]);
end;


{-------------------------------------------------------------------------------
  AssignActions
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.AssignActions(Src: TEditorMouseOptions);
begin
  FMainActions.Assign                (Src.MainActions);
  FSelActions.Assign                 (Src.SelActions);
  FTextActions.Assign                (Src.TextActions);
  FGutterActions.Assign              (Src.GutterActions);
  FGutterActionsFold.Assign          (Src.GutterActionsFold);
  FGutterActionsFoldExp.Assign       (Src.GutterActionsFoldExp);
  FGutterActionsFoldCol.Assign       (Src.GutterActionsFoldCol);
  FGutterActionsLines.Assign         (Src.GutterActionsLines);
  FGutterActionsChanges.Assign       (Src.GutterActionsChanges);
  FGutterActionsOverView.Assign      (Src.GutterActionsOverView);
  FGutterActionsOverViewMarks.Assign (Src.GutterActionsOverViewMarks);
end;


{-------------------------------------------------------------------------------
  SetTextCtrlLeftClick
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.SetTextCtrlLeftClick(AValue: TMouseOptButtonActionOld);
begin
  // upgrade old values
  if AValue in [low(MouseOptButtonActionOld)..high(MouseOptButtonActionOld)] then
    AValue := MouseOptButtonActionOld[AValue];
  if FTextCtrlLeftClick = AValue then Exit;
  FTextCtrlLeftClick := AValue;
end;


{-------------------------------------------------------------------------------
  SetTextMiddleClick
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.SetTextMiddleClick(AValue: TMouseOptButtonActionOld);
begin
  // upgrade old values
  if AValue in [low(MouseOptButtonActionOld)..high(MouseOptButtonActionOld)] then
    AValue := MouseOptButtonActionOld[AValue];
  if FTextMiddleClick = AValue then Exit;
  FTextMiddleClick := AValue;
end;


{-------------------------------------------------------------------------------
  AssignEx
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.AssignEx(Src: TEditorMouseOptions; WithUserSchemes: Boolean);
var
  i: Integer;
begin
  FName                        := Src.FName;

  FGutterLeft                  := Src.GutterLeft;
  FSelectOnLineNumbers         := Src.SelectOnLineNumbers;
  FTextDrag                    := Src.TextDrag;
  FTextRightMoveCaret          := Src.TextRightMoveCaret;
  FSelectedUserScheme          := Src.FSelectedUserScheme;
  // left multi click
  FTextDoubleLeftClick         := Src.TextDoubleLeftClick;
  FTextTripleLeftClick         := Src.TextTripleLeftClick;
  FTextQuadLeftClick           := Src.TextQuadLeftClick;
  FTextShiftDoubleLeftClick    := Src.TextShiftDoubleLeftClick;
  FTextAltDoubleLeftClick      := Src.TextAltDoubleLeftClick;
  FTextCtrlDoubleLeftClick     := Src.TextCtrlDoubleLeftClick;
  // left + modifier click
  FTextAltLeftClick            := Src.TextAltLeftClick;
  FTextCtrlLeftClick           := Src.TextCtrlLeftClick;
  FTextAltCtrlLeftClick        := Src.TextAltCtrlLeftClick;
  FTextShiftLeftClick          := Src.TextShiftLeftClick;
  FTextShiftAltLeftClick       := Src.TextShiftAltLeftClick;
  FTextShiftCtrlLeftClick      := Src.TextShiftCtrlLeftClick;
  FTextShiftAltCtrlLeftClick   := Src.TextShiftAltCtrlLeftClick;
  // middle click
  FTextMiddleClick             := Src.TextMiddleClick;
  FTextAltMiddleClick          := Src.TextAltMiddleClick;
  FTextCtrlMiddleClick         := Src.TextCtrlMiddleClick;
  FTextShiftMiddleClick        := Src.TextShiftMiddleClick;
  FTextAltCtrlMiddleClick      := Src.TextAltCtrlMiddleClick;
  FTextShiftAltMiddleClick     := Src.TextShiftAltMiddleClick;
  FTextShiftCtrlMiddleClick    := Src.TextShiftCtrlMiddleClick;
  FTextShiftAltCtrlMiddleClick := Src.TextShiftAltCtrlMiddleClick;
  // wheel
  FWheel                       := Src.Wheel;
  FCtrlWheel                   := Src.CtrlWheel;
  FAltWheel                    := Src.AltWheel;
  FShiftWheel                  := Src.ShiftWheel;
  FAltCtrlWheel                := Src.AltCtrlWheel;
  FShiftCtrlWheel              := Src.ShiftCtrlWheel;
  FShiftAltWheel               := Src.ShiftAltWheel;
  FShiftAltCtrlWheel           := Src.ShiftAltCtrlWheel;
  // right
  FTextAltCtrlRightClick       := Src.TextAltCtrlRightClick;
  FTextAltRightClick           := Src.TextAltRightClick;
  FTextCtrlRightClick          := Src.TextCtrlRightClick;
  FTextRightClick              := Src.TextRightClick;
  FTextShiftAltCtrlRightClick  := Src.TextShiftAltCtrlRightClick;
  FTextShiftAltRightClick      := Src.TextShiftAltRightClick;
  FTextShiftCtrlRightClick     := Src.TextShiftCtrlRightClick;
  FTextShiftRightClick         := Src.TextShiftRightClick;
  // extra-1 click
  FTextAltCtrlExtra1Click      := Src.TextAltCtrlExtra1Click;
  FTextAltExtra1Click          := Src.TextAltExtra1Click;
  FTextCtrlExtra1Click         := Src.TextCtrlExtra1Click;
  FTextExtra1Click             := Src.TextExtra1Click;
  FTextShiftAltCtrlExtra1Click := Src.TextShiftAltCtrlExtra1Click;
  FTextShiftAltExtra1Click     := Src.TextShiftAltExtra1Click;
  FTextShiftCtrlExtra1Click    := Src.TextShiftCtrlExtra1Click;
  FTextShiftExtra1Click        := Src.TextShiftExtra1Click;
  // extra-2 click
  FTextAltCtrlExtra2Click      := Src.TextAltCtrlExtra2Click;
  FTextAltExtra2Click          := Src.TextAltExtra2Click;
  FTextCtrlExtra2Click         := Src.TextCtrlExtra2Click;
  FTextExtra2Click             := Src.TextExtra2Click;
  FTextShiftAltCtrlExtra2Click := Src.TextShiftAltCtrlExtra2Click;
  FTextShiftAltExtra2Click     := Src.TextShiftAltExtra2Click;
  FTextShiftCtrlExtra2Click    := Src.TextShiftCtrlExtra2Click;
  FTextShiftExtra2Click        := Src.TextShiftExtra2Click;

  AssignActions(Src);

  if WithUserSchemes then begin
    ClearUserSchemes;
    for i := 0 to Src.FUserSchemes.Count - 1 do begin
      FUserSchemes.AddObject(Src.FUserSchemes[i], TEditorMouseOptions.Create);
      TEditorMouseOptions(FUserSchemes.Objects[i]).Assign
        ( TEditorMouseOptions(Src.FUserSchemes.Objects[i]) );
    end;
  end;
end;


{-------------------------------------------------------------------------------
  Assign
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.Assign(Src: TEditorMouseOptions);
begin
  AssignEx(Src, True);
end;


{-------------------------------------------------------------------------------
  IsPresetEqualToMouseActions
 ------------------------------------------------------------------------------}
function TEditorMouseOptions.IsPresetEqualToMouseActions: Boolean;
var
  Temp: TEditorMouseOptions;
  i: Integer;
begin
  i := SelectedUserSchemeIndex;
  Temp := TEditorMouseOptions.Create;
  Temp.AssignEx(self, i >= 0);
  if i >= 0 then
  begin
    Temp.ResetToUserScheme;
  end
  else
  begin
    Temp.ResetTextToDefault;
    Temp.ResetGutterToDefault;
  end;
  Result :=
    Temp.MainActions.Equals               (self.MainActions)           and
    Temp.SelActions.Equals                (self.SelActions)            and
    Temp.TextActions.Equals               (self.TextActions)           and
    Temp.GutterActions.Equals             (self.GutterActions)         and
    Temp.GutterActionsFold.Equals         (self.GutterActionsFold)     and
    Temp.GutterActionsFoldCol.Equals      (self.GutterActionsFoldCol)  and
    Temp.GutterActionsFoldExp.Equals      (self.GutterActionsFoldExp)  and
    Temp.GutterActionsLines.Equals        (self.GutterActionsLines)    and
    Temp.GutterActionsChanges.Equals      (Self.GutterActionsChanges)  and
    Temp.GutterActionsOverView.Equals     (Self.GutterActionsOverView) and
    Temp.GutterActionsOverViewMarks.Equals(Self.GutterActionsOverViewMarks);
  Temp.Free;
end;


{-------------------------------------------------------------------------------
  CalcCustomSavedActions
 ------------------------------------------------------------------------------}
function TEditorMouseOptions.CalcCustomSavedActions: Boolean;
begin
  Result := not IsPresetEqualToMouseActions;
  FCustomSavedActions := Result;
end;


{-------------------------------------------------------------------------------
  LoadFromXml
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.LoadFromXml(aSettings: TAppStorage; aPath: String;
  aOldPath: String; FileVersion: Integer);

  Procedure LoadMouseAct(Path: String; MActions: TSynEditMouseActions);
  var
    c, i: Integer;
    MAct: TSynEditMouseActionKeyCmdHelper;
    //ErrShown: Boolean;
  begin
    //ErrShown := False;
    MActions.Clear;
    MAct := TSynEditMouseActionKeyCmdHelper.Create(nil);

    c := aSettings.Read(Path + 'Count', 0);
    for i := 0 to c - 1 do
    begin
      try
        MActions.IncAssertLock;
        try
          // If the object would ever be extended, old configs will not have all properties.
          Mact.Clear;
          aSettings.ReadObject(Path + 'M' + IntToStr(i) + '/', MAct);
          MActions.Add.Assign(MAct);
        finally
          MActions.DecAssertLock;
        end;
        MActions.AssertNoConflict(MAct);
      except
        MActions.Delete(MActions.Count-1);

      end;
    end;
    MAct.Free;
  end;

var
  AltColumnMode: Boolean;
  TextDoubleSelLine: Boolean;
begin
  Reset;
  if FileVersion < 11 then
    FGutterLeft := moGLDownClick;
  AltColumnMode := False;
  TextDoubleSelLine := False;
  if aOldPath <> '' then
  begin
    // Read deprecated value
    // It is on by default, so only if a user switched it off, actions is required
    if not aSettings.Read(aOldPath + 'DragDropEditing', True) then
      TextDrag := False;
    aSettings.DeleteValue(aOldPath + 'DragDropEditing');

    if aSettings.Read(aOldPath + 'AltSetsColumnMode', False) then
      AltColumnMode := True;
    aSettings.DeleteValue(aOldPath + 'AltSetsColumnMode');

    if not aSettings.Read(aOldPath + 'CtrlMouseLinks', True) then
      TextCtrlLeftClick := mbaNone;
    aSettings.DeleteValue(aOldPath + 'CtrlMouseLinks');

    if aSettings.Read(aOldPath + 'DoubleClickSelectsLine', False) then
      TextDoubleSelLine := True;
    aSettings.DeleteValue(aOldPath + 'DoubleClickSelectsLine');
  end;

  //AltColumnMode, before TextAltLeftClick
  if (not AltColumnMode) then
    AltColumnMode := aSettings.Read(aPath + 'Default/AltColumnMode', True);
  aSettings.DeleteValue(aPath + 'Default/AltColumnMode');

  if (not AltColumnMode) then
    TextAltLeftClick := mbaNone;

  if aSettings.Read(aPath + 'Default/TextDoubleSelLine', TextDoubleSelLine) then
  begin
    FTextDoubleLeftClick       := mbaSelectSetLineSmart;
    FTextTripleLeftClick       := mbaSelectSetLineFull;
  end;
  aSettings.DeleteValue(aPath + 'Default/TextDoubleSelLine');

  CustomSavedActions := False;
  aSettings.ReadObject(aPath + 'Default/', Self);

  if (FSelectedUserScheme <> '') and (UserSchemes[FSelectedUserScheme] = nil) then
    FSelectedUserScheme := '';

  if CustomSavedActions then
  begin
    // Load
    LoadMouseAct(aPath + 'Main/',                  MainActions);
    LoadMouseAct(aPath + 'MainText/',              TextActions);
    LoadMouseAct(aPath + 'MainSelection/',         SelActions);
    LoadMouseAct(aPath + 'Gutter/',                GutterActions);
    LoadMouseAct(aPath + 'GutterFold/',            GutterActionsFold);
    LoadMouseAct(aPath + 'GutterFoldExp/',         GutterActionsFoldExp);
    LoadMouseAct(aPath + 'GutterFoldCol/',         GutterActionsFoldCol);
    LoadMouseAct(aPath + 'GutterLineNum/',         GutterActionsLines);
    LoadMouseAct(aPath + 'GutterLineChange/',      GutterActionsChanges);
    LoadMouseAct(aPath + 'GutterOverView/',        GutterActionsOverView);
    LoadMouseAct(aPath + 'GutterOverViewMarks/',   GutterActionsOverViewMarks);

    if Version < 1 then
    begin
      try
        FMainActions.AddCommand(emcWheelVertScrollDown, False,  mbXWheelDown, ccAny, cdDown, [], []);
        FMainActions.AddCommand(emcWheelVertScrollUp,   False,  mbXWheelUp,   ccAny, cdDown, [], []);
      except
      end;
    end;
  end
  else
  if (FSelectedUserScheme <> '') then
  begin
    ResetToUserScheme;
  end
  else
  begin
    ResetTextToDefault;
    ResetGutterToDefault;
  end;
end;


{-------------------------------------------------------------------------------
  SaveToXml
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.SaveToXml(aSettings: TAppStorage; aPath: String);

  Procedure SaveMouseAct(Path: String; MActions: TSynEditMouseActions);
  var
    i, OldCnt: Integer;
    MAct: TSynEditMouseActionKeyCmdHelper;
  begin
    MAct := TSynEditMouseActionKeyCmdHelper.Create(nil);
    OldCnt := aSettings.Read(Path + 'Count', 0);
    for i := 0 to MActions.Count - 1 do begin
      if MActions[i].Command = emcSynEditCommand then
      begin
        MAct.Assign(MActions[i]);
        aSettings.WriteObject(Path + 'M' + IntToStr(i) + '/', MAct);
      end else
        aSettings.WriteObject(Path + 'M' + IntToStr(i) + '/', MActions[i]);
    end;
    aSettings.WriteOrDefault(Path + 'Count', MActions.Count,0);
    for i := MActions.Count to OldCnt do
      aSettings.DeletePath(Path + 'M' + IntToStr(i));
    MAct.Free;
  end;

var
  DefMouseSettings: TEditorMouseOptions;
begin
  FVersion := EditorMouseOptsFormatVersion;
  DefMouseSettings := TEditorMouseOptions.Create;
  CalcCustomSavedActions;
  aSettings.WriteObject(aPath + 'Default/', Self, DefMouseSettings);
  DefMouseSettings.Free;
  if CustomSavedActions then
  begin
    // Save full settings / based on empty
    SaveMouseAct(aPath + 'Main/',               MainActions);
    SaveMouseAct(aPath + 'MainText/',           TextActions);
    SaveMouseAct(aPath + 'MainSelection/',      SelActions);
    SaveMouseAct(aPath + 'Gutter/',             GutterActions);
    SaveMouseAct(aPath + 'GutterFold/',         GutterActionsFold);
    SaveMouseAct(aPath + 'GutterFoldExp/',      GutterActionsFoldExp);
    SaveMouseAct(aPath + 'GutterFoldCol/',      GutterActionsFoldCol);
    SaveMouseAct(aPath + 'GutterLineNum/',      GutterActionsLines);
    SaveMouseAct(aPath + 'GutterLineChange/',   GutterActionsChanges);
    SaveMouseAct(aPath + 'GutterOverView/',     GutterActionsOverView);
    SaveMouseAct(aPath + 'GutterOverViewMarks/',GutterActionsOverViewMarks);
  end
  else
  begin
    // clear unused entries
    aSettings.DeletePath(aPath + 'Main');
    aSettings.DeletePath(aPath + 'MainSelection');
    aSettings.DeletePath(aPath + 'Gutter');
    aSettings.DeletePath(aPath + 'GutterFold');
    aSettings.DeletePath(aPath + 'GutterFoldExp');
    aSettings.DeletePath(aPath + 'GutterFoldCol');
    aSettings.DeletePath(aPath + 'GutterLineNum');
  end;
end;


{-------------------------------------------------------------------------------
  ImportFromXml
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.ImportFromXml(aSettings: TAppStorage; aPath: String);

  Procedure LoadMouseAct(Path: String; MActions: TSynEditMouseActions);
  var
    i, c: Integer;
    MAct: TSynEditMouseActionKeyCmdHelper;
  begin
    MActions.Clear;
    MAct := TSynEditMouseActionKeyCmdHelper.Create(nil);
    c := aSettings.Read(Path + 'Count', 0);
    for i := 0 to c - 1 do
    begin
      try
        MActions.IncAssertLock;
        try
          Mact.Clear;
          aSettings.ReadObject(Path + 'M' + IntToStr(i) + '/', MAct);
          MActions.Add.Assign(MAct);
        finally
          MActions.DecAssertLock;
        end;
        MActions.AssertNoConflict(MAct);
      except
        MActions.Delete(MActions.Count-1);
        ShowMessage(LStr('dlgMouseOptErrorDupText', 'This entry conflicts with an existing entry') + LineEnding
                   + Path + 'M' + IntToStr(i) + LineEnding + MAct.DisplayName );
      end;
    end;
    Mact.Free;
  end;

begin
  LoadMouseAct(aPath + 'Main/',               MainActions);
  LoadMouseAct(aPath + 'MainText/',           TextActions);
  LoadMouseAct(aPath + 'MainSel/',            SelActions);
  LoadMouseAct(aPath + 'Gutter/',             GutterActions);
  LoadMouseAct(aPath + 'GutterFold/',         GutterActionsFold);
  LoadMouseAct(aPath + 'GutterFoldExp/',      GutterActionsFoldExp);
  LoadMouseAct(aPath + 'GutterFoldCol/',      GutterActionsFoldCol);
  LoadMouseAct(aPath + 'GutterLineNum/',      GutterActionsLines);
  LoadMouseAct(aPath + 'GutterLineChange/',   GutterActionsChanges);
  LoadMouseAct(aPath + 'GutterOverView/',     GutterActionsOverView);
  LoadMouseAct(aPath + 'GutterOverViewMarks/',GutterActionsOverViewMarks);
end;


{-------------------------------------------------------------------------------
  ExportToXml
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.ExportToXml(aSettings: TAppStorage; aPath: String);
var
  MAct: TSynEditMouseActionKeyCmdHelper;

  Procedure SaveMouseAct(Path: String; MActions: TSynEditMouseActions);
  var
    i: Integer;
  begin
    for i := 0 to MActions.Count - 1 do
      if MActions[i].Command = emcSynEditCommand then
      begin
        MAct.Assign(MActions[i]);
        aSettings.WriteObject(Path + 'M' + IntToStr(i) + '/', MAct);
      end
      else
        aSettings.WriteObject(Path + 'M' + IntToStr(i) + '/', MActions[i]);
    aSettings.WriteOrDefault(Path + 'Count', MActions.Count,0);
  end;

begin
  MAct := TSynEditMouseActionKeyCmdHelper.Create(nil);
  SaveMouseAct(aPath + 'Main/',               MainActions);
  SaveMouseAct(aPath + 'MainText/',           TextActions);
  SaveMouseAct(aPath + 'MainSel/',            SelActions);
  SaveMouseAct(aPath + 'Gutter/',             GutterActions);
  SaveMouseAct(aPath + 'GutterFold/',         GutterActionsFold);
  SaveMouseAct(aPath + 'GutterFoldExp/',      GutterActionsFoldExp);
  SaveMouseAct(aPath + 'GutterFoldCol/',      GutterActionsFoldCol);
  SaveMouseAct(aPath + 'GutterLineNum/',      GutterActionsLines);
  SaveMouseAct(aPath + 'GutterLineChange/',   GutterActionsChanges);
  SaveMouseAct(aPath + 'GutterOverView/',     GutterActionsOverView);
  SaveMouseAct(aPath + 'GutterOverViewMarks/',GutterActionsOverViewMarks);
  MAct.Free;
end;


{-------------------------------------------------------------------------------
  LoadUserSchemes
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.LoadUserSchemes;
var
  i, j, k, c: Integer;
  FileList: TStringList;
  settings: TAppStorage;
  n: String;
begin
  ClearUserSchemes;
  if DirectoryExistsUTF8(UserSchemeDirectory(False)) then
  begin
    FileList := FindAllFiles(UserSchemeDirectory(False), '*.xml', False);
    for i := 0 to FileList.Count - 1 do
    begin
      settings := nil;
      try
        settings := TAppStorage.Create(FileList[i]);
        c := settings.Read('MdEdit/MouseSchemes/Names/Count', 0);
        for j := 0 to c-1 do
        begin
          n := settings.GetValue('MdEdit/MouseSchemes/Names/Item'+IntToStr(j+1)+'/Value', '');
          if n <> '' then
          begin
            k := FUserSchemes.AddObject(UTF8UpperCase(n), TEditorMouseOptions.Create);
            TEditorMouseOptions(FUserSchemes.Objects[k]).FName := n;
            TEditorMouseOptions(FUserSchemes.Objects[k]).ImportFromXml
              (settings, 'MdEdit/MouseSchemes/Scheme' + n + '/');
          end;
        end;
      except
        ShowMessage(Format(LStr('dlgUserSchemeError', 'Failed to load user-scheme file %s'), [FileList[i]]));
      end;
      settings.Free;
    end;
    FileList.Free;
  end;
end;


{-------------------------------------------------------------------------------
  UserSchemeCount
 ------------------------------------------------------------------------------}
function TEditorMouseOptions.UserSchemeCount: Integer;
begin
  Result := FUserSchemes.Count;
end;


{-------------------------------------------------------------------------------
  IndexOfUserScheme
 ------------------------------------------------------------------------------}
function TEditorMouseOptions.IndexOfUserScheme(SchemeName: String): Integer;
begin
  Result := FUserSchemes.IndexOf(UTF8UpperCase(SchemeName));
end;


{-------------------------------------------------------------------------------
  GetSelectedUserSchemeIndex
 ------------------------------------------------------------------------------}
function TEditorMouseOptions.GetSelectedUserSchemeIndex: Integer;
begin
  if FSelectedUserScheme = '' then
    Result := -1
  else
    Result := IndexOfUserScheme(FSelectedUserScheme);
end;


{-------------------------------------------------------------------------------
  SetSelectedUserScheme
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.SetSelectedUserScheme(const AValue: String);
begin
  if FSelectedUserScheme = AValue then exit;
  FSelectedUserScheme := AValue;
  ResetToUserScheme;
end;


{-------------------------------------------------------------------------------
  SetSelectedUserSchemeIndex
 ------------------------------------------------------------------------------}
procedure TEditorMouseOptions.SetSelectedUserSchemeIndex(const AValue: Integer);
begin
  if AValue < 0 then
    SelectedUserScheme := ''
  else
    SelectedUserScheme := TEditorMouseOptions(FUserSchemes.Objects[AValue]).Name;
end;



{ TEditorMouseOptionPresets }

{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TEditorMouseOptionPresets.Create;
var
  FileList: TStringList;
  settings: TAppStorage;
  i, j, c: Integer;
  n: String;
begin
  FPreset := TQuickStringlist.Create;

  if DirectoryExistsUTF8(UserSchemeDirectory(False)) then begin
    FileList := FindAllFiles(UserSchemeDirectory(False), '*.xml', False);
    for i := 0 to FileList.Count - 1 do begin
      settings := nil;
      try
        settings := TAppStorage.Create(FileList[i]);
        c := settings.Read('MdEdit/MouseSchemes/Names/Count', 0);
        for j := 0 to c-1 do begin
          n := settings.Read('MdEdit/MouseSchemes/Names/Item'+IntToStr(j+1)+'/Value', '');
          if n <> '' then begin
          end;
        end;
      except
        ShowMessage(Format(LStr('dlgUserSchemeError', 'Failed to load user-scheme file %s'), [FileList[i]]));
      end;
      settings.Free;
    end;
    FileList.Free;
  end;

end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TEditorMouseOptionPresets.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FPreset);
end;



{ EditorOldOptionsEditAccessOrderList }

{-------------------------------------------------------------------------------
  GetItems
 ------------------------------------------------------------------------------}
function EditorOldOptionsEditAccessOrderList.GetItems(Index: Integer):
  EditorOldOptionsEditAccessOrderEntry;
begin
  Result := EditorOldOptionsEditAccessOrderEntry(FList[Index]);
end;


{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor EditorOldOptionsEditAccessOrderList.Create;
begin
  Flist := TFPList.Create;
  FSearchOrder := eoeaOrderByEditFocus;
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor EditorOldOptionsEditAccessOrderList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  Clear
 ------------------------------------------------------------------------------}
procedure EditorOldOptionsEditAccessOrderList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  FList.Clear;
end;


{-------------------------------------------------------------------------------
  InitDefaults
 ------------------------------------------------------------------------------}
procedure EditorOldOptionsEditAccessOrderList.InitDefaults;
var
  i: Integer;
  Entry: EditorOldOptionsEditAccessOrderEntry;
begin
  for i := 0 to high(EditorOptionsEditAccessDefaults) do
  begin
    Entry := EditorOldOptionsEditAccessOrderEntry.Create(Self);
    Entry.InitFrom(EditorOptionsEditAccessDefaults[i]);
    FList.Add(Entry);
  end;
  Entry.FIsFallback := True;
end;


{-------------------------------------------------------------------------------
  Assign
 ------------------------------------------------------------------------------}
procedure EditorOldOptionsEditAccessOrderList.Assign(Src: EditorOldOptionsEditAccessOrderList);
var
  i: Integer;
  Entry: EditorOldOptionsEditAccessOrderEntry;
begin
  Clear;
  FSearchOrder := Src.FSearchOrder;
  for i := 0 to Src.Count - 1 do
  begin
    Entry := EditorOldOptionsEditAccessOrderEntry.Create(Self);
    Entry.Assign(Src[i]);
    FList.Add(Entry);
  end;
end;


{-------------------------------------------------------------------------------
  LoadFromXMLConfig
 ------------------------------------------------------------------------------}
procedure EditorOldOptionsEditAccessOrderList.LoadFromXMLConfig(aSettings: TAppStorage; Path: String);
var
  i: Integer;
  def: EditorOldOptionsEditAccessOrderList;
begin
  def := EditorOldOptionsEditAccessOrderList.Create;
  aSettings.ReadObject(Path + 'Main/', self, def);
  def.Free;
  Path := Path + 'Entry/';
  for i := 0 to Count - 1 do
    aSettings.ReadObject(Path + Items[i].ID + '/', Items[i], Items[i].FDefaults);
end;


{-------------------------------------------------------------------------------
  SaveToXMLConfig
 ------------------------------------------------------------------------------}
procedure EditorOldOptionsEditAccessOrderList.SaveToXMLConfig(aSettings: TAppStorage; Path: String);
var
  i: Integer;
  def: EditorOldOptionsEditAccessOrderList;
begin
  def := EditorOldOptionsEditAccessOrderList.Create;
  aSettings.WriteObject(Path + 'Main/', Self, def);
  def.Free;
  Path := Path + 'Entry/';
  for i := 0 to Count - 1 do
    aSettings.WriteObject(Path + Items[i].ID + '/', Items[i], Items[i].FDefaults);
end;


{-------------------------------------------------------------------------------
  Count
 ------------------------------------------------------------------------------}
function EditorOldOptionsEditAccessOrderList.Count: Integer;
begin
  Result := FList.Count;
end;


{ EditorOldOptionsEditAccessOrderEntry }

{-------------------------------------------------------------------------------
  AssignFrom
 ------------------------------------------------------------------------------}
procedure EditorOldOptionsEditAccessOrderEntry.AssignFrom(AValue: EditorOldOptionsEditAccessDefaultEntry);
begin
  FId      := AValue.ID;
  FCaption := AValue.Caption;
  FDesc    := AValue.Desc;
  FEnabled := AValue.Enabled;
  FSearchInView  := AValue.SearchInView;
  FSearchLocked  := AValue.SearchLocked;
  FSearchOpenNew := AValue.SearchOpenNew;
  FSearchOrder   := AValue.SearchOrder;
end;


{-------------------------------------------------------------------------------
  SetEnabled
 ------------------------------------------------------------------------------}
procedure EditorOldOptionsEditAccessOrderEntry.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue or FIsFallback;
end;


{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor EditorOldOptionsEditAccessOrderEntry.Create(AList: EditorOldOptionsEditAccessOrderList);
begin
  inherited Create;
  FList := AList;
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor EditorOldOptionsEditAccessOrderEntry.Destroy;
begin
  FreeAndNil(FDefaults);
  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  Assign
 ------------------------------------------------------------------------------}
procedure EditorOldOptionsEditAccessOrderEntry.Assign(Src: EditorOldOptionsEditAccessOrderEntry);
begin
  FId            := Src.FID;
  FCaption       := Src.FCaption;
  FDesc          := Src.FDesc;
  FEnabled       := Src.FEnabled;
  FIsFallback    := Src.FIsFallback;
  FSearchInView  := Src.FSearchInView;
  FSearchLocked  := Src.FSearchLocked;
  FSearchOpenNew := Src.FSearchOpenNew;
  FSearchOrder   := Src.FSearchOrder;

  FreeAndNil(FDefaults);
  if Src.FDefaults <> nil then
  begin
    FDefaults := EditorOldOptionsEditAccessOrderEntry.Create(nil);
    FDefaults.Assign(Src.FDefaults);
  end;
end;


{-------------------------------------------------------------------------------
  InitFrom
 ------------------------------------------------------------------------------}
procedure EditorOldOptionsEditAccessOrderEntry.InitFrom(AValue: EditorOldOptionsEditAccessDefaultEntry);
begin
  AssignFrom(AValue);
  FDefaults := EditorOldOptionsEditAccessOrderEntry.Create(nil);
  FDefaults.AssignFrom(AValue);
end;


{-------------------------------------------------------------------------------
  RealSearchOrder
 ------------------------------------------------------------------------------}
function EditorOldOptionsEditAccessOrderEntry.RealSearchOrder: EditorOldOptionsEditAccessOrder;
begin
  Result := SearchOrder;
  if Result = eoeaOrderByListPref then begin
    if FList = nil then Result := eoeaOrderByEditFocus;
    Result := FList.SearchOrder;
    if Result = eoeaOrderByListPref then Result := eoeaOrderByEditFocus;
  end;
end;



{ TEditorOptions }

{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TEditorOptions.Create(settings: TAppStorage);
begin
  mSettings := settings;

  inherited Create;

  OFormDebug.AddNotify( @DebugNotify );

  InitLocale;
  Init;

  fCodeTemplateFileNameRaw := TrimFilename(AppendPathDelim(GetPrimaryConfigPath)+DefaultCodeTemplatesFilename);
  FMultiWinEditAccessOrder := EditorOldOptionsEditAccessOrderList.Create;
  FMultiWinEditAccessOrder.InitDefaults;

  FDefaultValues := TEditorOptions.CreateDefaultOnly;
end;


{-------------------------------------------------------------------------------
  CreateDefaultOnly
 ------------------------------------------------------------------------------}
constructor TEditorOptions.CreateDefaultOnly;
begin
  inherited Create;
  Init;
  FDefaultValues := nil;
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TEditorOptions.Destroy;
begin
  FreeAndNil(FUserColorSchemeSettings);
  FreeAndNil(FMultiWinEditAccessOrder);
  FUserMouseSettings.Free;
  FTempMouseSettings.Free;
  FreeAndNil(FDefaultValues);
  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  Init
 ------------------------------------------------------------------------------}
procedure TEditorOptions.Init;
begin
  // General options
  fShowTabCloseButtons := True;
  FMultiLineTab := False;
  FHideSingleTabInWindow := False;
  fTabPosition := tpTop;
  FCopyWordAtCursorOnCopyNone := True;
  FShowGutterHints := True;
  fBlockIndent := 2;
  FBlockTabIndent := 0;
  fBlockIndentType := sbitSpace;
  FTrimSpaceType := settEditLine;
  fUndoLimit := 32767;
  fTabWidth := 8;
  FBracketHighlightStyle := sbhsBoth;
  FGutterSeparatorIndex := 3;
  fSynEditOptions := SynEditDefaultOptions;
  fSynEditOptions2 := SynEditDefaultOptions2;
  FMultiCaretOnColumnSelect := True;
  FMultiCaretDeleteSkipLineBreak := False;

  // Display options
  fEditorFont := SynDefaultFontName;
  fEditorFontSize := SynDefaultFontSize;
  fDisableAntialiasing := DefaultEditorDisableAntiAliasing;
  FShowOverviewGutter := True;
  FTopInfoView := True;


  // Mouse Mappings
  FUserMouseSettings := TEditorMouseOptions.Create;
  FTempMouseSettings := TEditorMouseOptions.Create;
  FUserMouseSettings.LoadUserSchemes;

  // Color options
  fHighlighterList := HighlighterListSingleton;
  FUserColorSchemeSettings := TColorSchemeFactory.Create;
  FUserColorSchemeSettings.Assign(ColorSchemeFactory);

  FMarkupCurWordTime := 1500;
  FMarkupCurWordFullLen := 3;
  FMarkupCurWordNoKeyword := True;
  FMarkupCurWordTrim := True;
  FMarkupCurWordNoTimer := False;

  // hints
  FDbgHintAutoTypeCastClass := True;

  // Code Tools options
  FCompletionLongLineHintType := DefaultCompletionLongLineHintType;
  FAutoDisplayFuncPrototypes := True;

  // Code folding
  FReverseFoldPopUpOrder := True;

  // pas highlighter
  FPasExtendedKeywordsMode := False;

  // Multi window
  FCtrlMiddleTabClickClosesOthers := True;
  FShowFileNameInCaption := False;

  // Comment
  FAnsiCommentContinueEnabled := False;
  FAnsiCommentMatch := '^\s?(\*)';
  FAnsiCommentMatchMode := scmMatchAtAsterisk;
  FAnsiCommentPrefix := '$1';
  FAnsiIndentMode := [sciAddTokenLen, sciAddPastTokenIndent,
                      sciAlignOnlyTokenLen, sciAlignOnlyPastTokenIndent,
                      sciMatchOnlyPastTokenIndent
                     ];
  FAnsiIndentAlignMax := 40;


  FCurlyCommentContinueEnabled := False;
  FCurlyCommentMatch := '^\s?(\*)';
  FCurlyCommentMatchMode := scmMatchAfterOpening;
  FCurlyCommentPrefix := '$1';
  FCurlyIndentMode := [sciAddTokenLen, sciAddPastTokenIndent,
                      sciAlignOnlyTokenLen, sciAlignOnlyPastTokenIndent,
                      sciMatchOnlyPastTokenIndent
                     ];
  FCurlyIndentAlignMax := 40;

  FSlashCommentContinueEnabled := False;
  FSlashCommentMatch := '^\s?(\*)';
  FSlashCommentMatchMode := scmMatchAfterOpening;
  FSlashCommentPrefix := '$1';
  FSlashIndentMode := [sciAddTokenLen, sciAddPastTokenIndent,
                      sciAlignOnlyTokenLen, sciAlignOnlyPastTokenIndent,
                      sciMatchOnlyPastTokenIndent
                     ];
  FSlashCommentExtend := sceMatching;
  FSlashIndentAlignMax := 40;

  FStringBreakEnabled := False;
  FStringBreakAppend  := ' +';
  FStringBreakPrefix  := '';
end;


{-------------------------------------------------------------------------------
  SetSettings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.SetSettings(appCnf: TAppStorage);
begin
  mSettings := appCnf;
end;


{-------------------------------------------------------------------------------
  Load
 ------------------------------------------------------------------------------}
procedure TEditorOptions.Load;
// load options from XML file
var
  SynEditOpt: TSynEditorOption;
  SynEditOptName: String;
  i: Integer;
  SynEditOpt2: TSynEditorOption2;
  FileVersion: LongInt;
  DefOpts: TSynEditorOptions;
begin
  try
    FileVersion:=Settings.Read('EditorOptions/Version', EditorOptsFormatVersion);

    Settings.ReadObject('EditorOptions/Misc/', Self, FDefaultValues);

    // general options
    DefOpts := SynEditDefaultOptions;
    if (FileVersion < 10) then DefOpts := DefOpts - [eoTabIndent];
    for SynEditOpt := Low(TSynEditorOption) to High(TSynEditorOption) do
    begin
      SynEditOptName := GetSynEditOptionName(SynEditOpt);
      if SynEditOptName <> '' then
      begin
        if Settings.Read('EditorOptions/General/Editor/' + SynEditOptName, SynEditOpt in DefOpts) then
        begin
          Include(fSynEditOptions, SynEditOpt)
        end
        else
        begin
          Exclude(fSynEditOptions, SynEditOpt);
        end;
      end;
    end;
    for SynEditOpt2 := Low(TSynEditorOption2) to High(TSynEditorOption2) do
    begin
      case SynEditOpt2 of

        eoCaretSkipsSelection: SynEditOptName := 'CaretSkipsSelection';
        eoCaretSkipTab:        SynEditOptName := 'CaretSkipTab';
        eoAlwaysVisibleCaret:  SynEditOptName := 'AlwaysVisibleCaret';
        eoEnhanceEndKey:       SynEditOptName := 'EnhanceEndKey';
        eoFoldedCopyPaste:     SynEditOptName := 'FoldedCopyPaste';
        eoPersistentBlock:     SynEditOptName := 'PersistentBlock';
        eoOverwriteBlock:      SynEditOptName := 'OverwriteBlock';
        eoAutoHideCursor:      SynEditOptName := 'AutoHideCursor';

        eoCaretMoveEndsSelection, eoPersistentCaretStopBlink:
          WriteStr(SynEditOptName, SynEditOpt2);
        else
          SynEditOptName := '';
      end;

      if SynEditOptName <> '' then
      begin
        if Settings.Read('EditorOptions/General/Editor/' + SynEditOptName, SynEditOpt2 in SynEditDefaultOptions2) then
        begin
          Include(fSynEditOptions2, SynEditOpt2)
        end
        else
        begin
          Exclude(fSynEditOptions2, SynEditOpt2);
        end;
      end;
    end;

    fShowTabCloseButtons              := Settings.Read('EditorOptions/General/Editor/ShowTabCloseButtons', True);
    FHideSingleTabInWindow            := Settings.Read('EditorOptions/General/Editor/HideSingleTabInWindow', False);
    fShowTabNumbers                   := Settings.Read('EditorOptions/General/Editor/ShowTabNumbers', False);
    FCopyWordAtCursorOnCopyNone       := Settings.Read('EditorOptions/General/Editor/CopyWordAtCursorOnCopyNone', True);
    FShowGutterHints                  := Settings.Read('EditorOptions/General/Editor/ShowGutterHints', True);
    fUndoAfterSave                    := Settings.Read('EditorOptions/General/Editor/UndoAfterSave', True);
    fFindTextAtCursor                 := Settings.Read('EditorOptions/General/Editor/FindTextAtCursor', True);
    fUseSyntaxHighlight               := Settings.Read('EditorOptions/General/Editor/UseSyntaxHighlight', True);
    fBlockIndent                      := Settings.Read('EditorOptions/General/Editor/BlockIndent', 2);
    FBlockTabIndent                   := Settings.Read('EditorOptions/General/Editor/BlockTabIndent', 0);
    fBlockIndentType                  := GetSynBeautifierIndentType(Settings.Read('EditorOptions/General/Editor/BlockIndentType', 'SpaceIndent'));
    FTrimSpaceType                    := GetTrimSpaceType(Settings.Read('EditorOptions/General/Editor/SpaceTrimType', 'EditLine'));
    fUndoLimit                        := Settings.Read('EditorOptions/General/Editor/UndoLimit', 32767);
    fTabWidth                         := Settings.Read('EditorOptions/General/Editor/TabWidth', 8);
    FBracketHighlightStyle            := TSynEditBracketHighlightStyle(Settings.Read('EditorOptions/General/Editor/BracketHighlightStyle', 2));

    // Display options
    fVisibleRightMargin               := Settings.Read('EditorOptions/Display/VisibleRightMargin', True);
    fVisibleGutter                    := Settings.Read('EditorOptions/Display/VisibleGutter', True);
    if FileVersion < 4 then
    begin
      fShowLineNumbers                := Settings.Read('EditorOptions/Display/ShowLineNumbers', False);
      fShowOnlyLineNumbersMultiplesOf := Settings.Read('EditorOptions/Display/ShowOnlyLineNumbersMultiplesOf', 1);
    end
    else
    begin
      fShowLineNumbers                := Settings.Read('EditorOptions/Display/ShowLineNumbers', True);
      fShowOnlyLineNumbersMultiplesOf := Settings.Read('EditorOptions/Display/ShowOnlyLineNumbersMultiplesOf', 5);
    end;
    fGutterWidth                      := Settings.Read('EditorOptions/Display/GutterWidth', 30);
    FGutterSeparatorIndex             := Settings.Read('EditorOptions/Display/GutterSeparatorIndex', 3);
    fRightMargin                      := Settings.Read('EditorOptions/Display/RightMargin', 80);
    fEditorFont                       := Settings.Read('EditorOptions/Display/EditorFont', SynDefaultFontName);

    if FileVersion < 8 then
    begin
      fEditorFontSize                 := Settings.Read('EditorOptions/Display/EditorFontHeight', SynDefaultFontHeight);
      fEditorFontSize                 := FontHeightToSize(fEditorFontSize);
    end
    else
    begin
      fEditorFontSize                 := Settings.Read('EditorOptions/Display/EditorFontSize', SynDefaultFontSize);
    end;
    RepairEditorFontSize(fEditorFontSize);
    fExtraCharSpacing                 := Settings.Read('EditorOptions/Display/ExtraCharSpacing', 0);
    fExtraLineSpacing                 := Settings.Read('EditorOptions/Display/ExtraLineSpacing', 1);
    fDisableAntialiasing              := Settings.Read('EditorOptions/Display/DisableAntialiasing', FileVersion<7);
    FDoNotWarnForFont                 := Settings.Read('EditorOptions/Display/DoNotWarnForFont', '');

    // Color options
    for i := 0 to HighlighterList.Count - 1 do
    begin
      // color attributes are stored in the highlighters
      HighlighterList[i].FileExtensions := Settings.Read('EditorOptions/Color/Lang' +
                                           StrToValidXMLName(HighlighterList[i].SynClass.GetLanguageName) +
                                           '/FileExtensions/Value', HighlighterList[i].DefaultFileExtensions);
    end;
    FMarkupCurWordTime                := Settings.Read('EditorOptions/Display/MarkupCurrentWord/Time', 1500);
    FMarkupCurWordFullLen             := Settings.Read('EditorOptions/Display/MarkupCurrentWord/FullLen', 3);

    // check deprecated value
    if not Settings.Read('EditorOptions/Display/MarkupCurrentWord/FullWord', True) then
    begin
      FMarkupCurWordFullLen := 0;
    end;
    Settings.DeleteValue('EditorOptions/Display/MarkupCurrentWord/FullWord');

    FMarkupCurWordNoKeyword           := Settings.Read('EditorOptions/Display/MarkupCurrentWord/NoKeyword', True);
    FMarkupCurWordTrim                := Settings.Read('EditorOptions/Display/MarkupCurrentWord/Trim', True);
    FMarkupCurWordNoTimer             := Settings.Read('EditorOptions/Display/MarkupCurrentWord/NoTimer', False);
    FShowFileNameInCaption            := Settings.Read('EditorOptions/Display/ShowFileNameInCaption', False);

    // Code Tools options
    fAutoBlockCompletion              := Settings.Read('EditorOptions/CodeTools/AutoBlockCompletion', True);
    fAutoDisplayFuncPrototypes        := Settings.Read('EditorOptions/CodeTools/AutoDisplayFuncPrototypes', True);
    fAutoCodeParameters               := Settings.Read('EditorOptions/CodeTools/AutoCodeParameters', True);
    fAutoToolTipExprEval              := Settings.Read('EditorOptions/CodeTools/AutoToolTipExprEval', True);
    fAutoToolTipSymbTools             := Settings.Read('EditorOptions/CodeTools/AutoToolTipSymbTools', True);
    fAutoDelayInMSec                  := Settings.Read('EditorOptions/CodeTools/AutoDelayInMSec', 1000);
    fCodeTemplateFileNameRaw          := Settings.Read('EditorOptions/CodeTools/CodeTemplateFileName' , TrimFilename(AppendPathDelim(GetPrimaryConfigPath) + DefaultCodeTemplatesFilename));
    fCTemplIndentToTokenStart         := Settings.Read('EditorOptions/CodeTools/CodeTemplateIndentToTokenStart/Value', False);
    fAutoRemoveEmptyMethods           := Settings.Read('EditorOptions/CodeTools/AutoRemoveEmptyMethods', False);
    FCompletionLongLineHintInMSec     := Settings.Read('EditorOptions/CodeTools/CompletionLongLineHintInMSec', 0);
    FCompletionLongLineHintType       := DefaultCompletionLongLineHintType;

    Settings.ReadObject('EditorOptions/CodeTools/CompletionLongLineHintType/', Self, Self, 'CompletionLongLineHintType');

    // Code Folding
    FUseCodeFolding                   := Settings.Read('EditorOptions/CodeFolding/UseCodeFolding', True);
    FUseMarkupWordBracket             := Settings.Read('EditorOptions/CodeFolding/UseMarkupWordBracket', True);
    FUseMarkupOutline                 := Settings.Read('EditorOptions/CodeFolding/UseMarkupOutline', False);

    FUserMouseSettings.LoadFromXml(Settings, 'EditorOptions/Mouse/', 'EditorOptions/General/Editor/', FileVersion);
    FMultiWinEditAccessOrder.LoadFromXMLConfig(Settings, 'EditorOptions/MultiWin/');
    UserColorSchemeGroup.LoadFromXml(Settings, 'EditorOptions/Color/', ColorSchemeFactory, 'EditorOptions/Display/');

  except
    on E: Exception do
      DebugLn('[EditorOldOptions.Load] ERROR: ', e.Message);
  end;
end;


{-------------------------------------------------------------------------------
  Save
 ------------------------------------------------------------------------------}
procedure TEditorOptions.Save;
// save options to XML file
var
  SynEditOpt: TSynEditorOption;
  SynEditOptName: String;
  i: Integer;
  SynEditOpt2: TSynEditorOption2;

begin
  try
    Settings.Write('EditorOptions/Version', EditorOptsFormatVersion);

    Settings.WriteObject('EditorOptions/Misc/', Self, FDefaultValues);

    // general options
    for SynEditOpt := Low(TSynEditorOption) to High(TSynEditorOption) do
    begin
      SynEditOptName := GetSynEditOptionName(SynEditOpt);
      if SynEditOptName <> '' then
      begin
        Settings.WriteOrDefault('EditorOptions/General/Editor/' + SynEditOptName,
          SynEditOpt in fSynEditOptions, SynEditOpt in SynEditDefaultOptions);
      end;
    end;
    // general options
    for SynEditOpt2 := Low(TSynEditorOption2) to High(TSynEditorOption2) do
    begin
      case SynEditOpt2 of

        eoCaretSkipsSelection:  SynEditOptName := 'CaretSkipsSelection';
        eoCaretSkipTab:         SynEditOptName := 'CaretSkipTab';
        eoAlwaysVisibleCaret:   SynEditOptName := 'AlwaysVisibleCaret';
        eoEnhanceEndKey:        SynEditOptName := 'EnhanceEndKey';
        eoFoldedCopyPaste:      SynEditOptName := 'FoldedCopyPaste';
        eoPersistentBlock:      SynEditOptName := 'PersistentBlock';
        eoOverwriteBlock:       SynEditOptName := 'OverwriteBlock';
        eoAutoHideCursor:       SynEditOptName := 'AutoHideCursor';

        eoCaretMoveEndsSelection, eoPersistentCaretStopBlink:
          WriteStr(SynEditOptName, SynEditOpt2);
        else
          SynEditOptName := '';
      end;
      if SynEditOptName <> '' then
        Settings.WriteOrDefault('EditorOptions/General/Editor/' + SynEditOptName,
          SynEditOpt2 in fSynEditOptions2, SynEditOpt2 in SynEditDefaultOptions2);
    end;

    Settings.WriteOrDefault('EditorOptions/General/Editor/ShowTabCloseButtons',        fShowTabCloseButtons, True);
    Settings.WriteOrDefault('EditorOptions/General/Editor/HideSingleTabInWindow',      FHideSingleTabInWindow, False);
    Settings.WriteOrDefault('EditorOptions/General/Editor/ShowTabNumbers',             fShowTabNumbers, False);
    Settings.WriteOrDefault('EditorOptions/General/Editor/CopyWordAtCursorOnCopyNone', FCopyWordAtCursorOnCopyNone, True);
    Settings.WriteOrDefault('EditorOptions/General/Editor/ShowGutterHints',            FShowGutterHints, True);
    Settings.WriteOrDefault('EditorOptions/General/Editor/UndoAfterSave',              fUndoAfterSave, True);
    Settings.WriteOrDefault('EditorOptions/General/Editor/FindTextAtCursor',           fFindTextAtCursor, True);
    Settings.WriteOrDefault('EditorOptions/General/Editor/UseSyntaxHighlight',         fUseSyntaxHighlight, True);
    Settings.WriteOrDefault('EditorOptions/General/Editor/BlockIndent',                fBlockIndent, 2);
    Settings.WriteOrDefault('EditorOptions/General/Editor/BlockTabIndent',             FBlockTabIndent, 0);
    Settings.WriteOrDefault('EditorOptions/General/Editor/BlockIndentType',            GetSynBeautifierIndentName(fBlockIndentType), 'SpaceIndent');
    Settings.WriteOrDefault('EditorOptions/General/Editor/SpaceTrimType',              GetTrimSpaceName(FTrimSpaceType), 'EditLine');
    Settings.WriteOrDefault('EditorOptions/General/Editor/UndoLimit',                  fUndoLimit, 32767);
    Settings.WriteOrDefault('EditorOptions/General/Editor/TabWidth',                   fTabWidth, 8);
    Settings.WriteOrDefault('EditorOptions/General/Editor/BracketHighlightStyle',      Ord(FBracketHighlightStyle), 2);

    // Display options
    Settings.WriteOrDefault('EditorOptions/Display/VisibleRightMargin',                fVisibleRightMargin, True);
    Settings.WriteOrDefault('EditorOptions/Display/VisibleGutter',                     fVisibleGutter, True);
    Settings.WriteOrDefault('EditorOptions/Display/ShowLineNumbers',                   fShowLineNumbers, True);
    Settings.WriteOrDefault('EditorOptions/Display/ShowOnlyLineNumbersMultiplesOf',    fShowOnlyLineNumbersMultiplesOf, 5);
    Settings.WriteOrDefault('EditorOptions/Display/GutterWidth',                       fGutterWidth, 30);
    Settings.WriteOrDefault('EditorOptions/Display/GutterSeparatorIndex',              fGutterSeparatorIndex, 3);
    Settings.WriteOrDefault('EditorOptions/Display/RightMargin',                       fRightMargin, 80);
    Settings.WriteOrDefault('EditorOptions/Display/EditorFont',                        fEditorFont, SynDefaultFontName);
    Settings.DeleteValue('EditorOptions/Display/EditorFontHeight');                    // unused old value
    Settings.WriteOrDefault('EditorOptions/Display/EditorFontSize',                    fEditorFontSize, SynDefaultFontSize);
    Settings.WriteOrDefault('EditorOptions/Display/ExtraCharSpacing',                  fExtraCharSpacing, 0);
    Settings.WriteOrDefault('EditorOptions/Display/ExtraLineSpacing',                  fExtraLineSpacing, 1);
    Settings.WriteOrDefault('EditorOptions/Display/DisableAntialiasing',               fDisableAntialiasing, DefaultEditorDisableAntiAliasing);
    Settings.WriteOrDefault('EditorOptions/Display/DoNotWarnForFont',                  FDoNotWarnForFont, '');

    // Color options
    for i := 0 to HighlighterList.Count - 1 do
    begin
      Settings.WriteOrDefault('EditorOptions/Color/Lang' + StrToValidXMLName(HighlighterList[i].SynClass.GetLanguageName) +
        '/FileExtensions/Value', HighlighterList[i].FileExtensions, HighlighterList[i].DefaultFileExtensions);
      // color attributes are stored in the highlighters
    end;

    Settings.WriteOrDefault('EditorOptions/Display/MarkupCurrentWord/Time',            FMarkupCurWordTime, 1500);
    Settings.WriteOrDefault('EditorOptions/Display/MarkupCurrentWord/FullLen',         FMarkupCurWordFullLen, 3);
    Settings.WriteOrDefault('EditorOptions/Display/MarkupCurrentWord/NoKeyword',       FMarkupCurWordNoKeyword, True);
    Settings.WriteOrDefault('EditorOptions/Display/MarkupCurrentWord/Trim',            FMarkupCurWordTrim, True);
    Settings.WriteOrDefault('EditorOptions/Display/MarkupCurrentWord/NoTimer',         FMarkupCurWordNoTimer, False);
    Settings.WriteOrDefault('EditorOptions/Display/ShowFileNameInCaption',             FShowFileNameInCaption, False);

    // Code Tools options
    Settings.WriteOrDefault('EditorOptions/CodeTools/AutoBlockCompletion',             fAutoBlockCompletion, True);
    Settings.WriteOrDefault('EditorOptions/CodeTools/AutoDisplayFuncPrototypes',       fAutoDisplayFuncPrototypes, True);
    Settings.WriteOrDefault('EditorOptions/CodeTools/AutoCodeParameters',              fAutoCodeParameters, True);
    Settings.WriteOrDefault('EditorOptions/CodeTools/AutoToolTipExprEval',             fAutoToolTipExprEval, True);
    Settings.WriteOrDefault('EditorOptions/CodeTools/AutoToolTipSymbTools',            fAutoToolTipSymbTools, True);
    Settings.WriteOrDefault('EditorOptions/CodeTools/AutoDelayInMSec',                 fAutoDelayInMSec, 1000);
    Settings.WriteOrDefault('EditorOptions/CodeTools/CodeTemplateFileName',            fCodeTemplateFileNameRaw, '');
    Settings.WriteOrDefault('EditorOptions/CodeTools/CodeTemplateIndentToTokenStart/Value', fCTemplIndentToTokenStart, False);
    Settings.WriteOrDefault('EditorOptions/CodeTools/AutoRemoveEmptyMethods',          fAutoRemoveEmptyMethods, False);
    Settings.WriteOrDefault('EditorOptions/CodeTools/CompletionLongLineHintInMSec',    FCompletionLongLineHintInMSec, 0);

    Settings.WriteObject('EditorOptions/CodeTools/CompletionLongLineHintType/',         Self, nil, 'CompletionLongLineHintType');

    // Code Folding
    Settings.WriteOrDefault('EditorOptions/CodeFolding/UseCodeFolding',                FUseCodeFolding, True);
    Settings.WriteOrDefault('EditorOptions/CodeFolding/UseMarkupWordBracket',          FUseMarkupWordBracket, True);
    Settings.WriteOrDefault('EditorOptions/CodeFolding/UseMarkupOutline',              FUseMarkupOutline, False);

    FUserMouseSettings.SaveToXml(Settings, 'EditorOptions/Mouse/');

    FMultiWinEditAccessOrder.SaveToXMLConfig(Settings, 'EditorOptions/MultiWin/');
    UserColorSchemeGroup.SaveToXml(Settings, 'EditorOptions/Color/', ColorSchemeFactory);

    InvalidateFileStateCache;
    Settings.Flush;
  except
    on E: Exception do
      DebugLn('[EditorOldOptions.Save] ERROR: ', e.Message);
  end;
end;


{-------------------------------------------------------------------------------
  Setup
 ------------------------------------------------------------------------------}
procedure TEditorOptions.Setup();
begin

end;


{-------------------------------------------------------------------------------
  TranslateResourceStrings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.TranslateResourceStrings;
begin

end;


{-------------------------------------------------------------------------------
  GetAdditionalAttributeName
 ------------------------------------------------------------------------------}
function TEditorOptions.GetAdditionalAttributeName(aha:TAdditionalHilightAttribute): string;
begin
  Result:=GetEnumName(TypeInfo(TAdditionalHilightAttribute), ord(aha));
end;


{-------------------------------------------------------------------------------
  GetGroupCaption
 ------------------------------------------------------------------------------}
class function TEditorOptions.GetGroupCaption: string;
begin
  Result := LStr('dlgGroupEditor', 'Editor');
end;


{-------------------------------------------------------------------------------
  GetInstance
 ------------------------------------------------------------------------------}
class function TEditorOptions.GetInstance: TPersistent;
begin
  Result := TPersistent(self);
end;


{-------------------------------------------------------------------------------
  DoAfterWrite
 ------------------------------------------------------------------------------}
procedure TEditorOptions.DoAfterWrite(Restore: boolean);
begin
  if not Restore then
    Save;
  inherited;
end;


{-------------------------------------------------------------------------------
  GetSynEditOptionName
 ------------------------------------------------------------------------------}
function TEditorOptions.GetSynEditOptionName(SynOption: TSynEditorOption): string;
begin
  case SynOption of
    eoAutoIndent:         Result := 'AutoIndent';
    eoBracketHighlight:   Result := 'BracketHighlight';
    eoEnhanceHomeKey:     Result := 'EnhanceHomeKey';
    eoGroupUndo:          Result := 'GroupUndo';
    eoHalfPageScroll:     Result := 'HalfPageScroll';
    eoKeepCaretX:         Result := 'KeepCaretX';
    eoPersistentCaret:    Result := 'PersistentCaret';
    eoScrollByOneLess:    Result := 'ScrollByOneLess';
    eoScrollPastEof:      Result := 'ScrollPastEof';
    eoScrollPastEol:      Result := 'ScrollPastEol';
    eoShowScrollHint:     Result := 'ShowScrollHint';
    eoShowSpecialChars:   Result := 'ShowSpecialChars';
    eoSmartTabs:          Result := 'SmartTabs';
    eoTabsToSpaces:       Result := 'TabsToSpaces';
    eoTabIndent:          Result := 'TabIndent';
    eoTrimTrailingSpaces: Result := 'TrimTrailingSpaces';
    else
      Result := '';
  end;
end;


{-------------------------------------------------------------------------------
  GetSynBeautifierIndentName
 ------------------------------------------------------------------------------}
function TEditorOptions.GetSynBeautifierIndentName(IndentType: TSynBeautifierIndentType): string;
begin
  case IndentType of
    sbitSpace:            Result := 'SpaceIndent';
    sbitCopySpaceTab:     Result := 'CopySpaceTabIndent';
    sbitPositionCaret:    Result := 'PositionIndent';
    else
      WriteStr(Result, IndentType);
  end;
end;


{-------------------------------------------------------------------------------
  GetSynBeautifierIndentType
 ------------------------------------------------------------------------------}
function TEditorOptions.GetSynBeautifierIndentType(IndentName: String): TSynBeautifierIndentType;
begin
  Result := sbitSpace;
  if IndentName = 'CopySpaceTabIndent'    then Result := sbitCopySpaceTab
  else
  if IndentName = 'PositionIndent'        then Result := sbitPositionCaret
  else
  if IndentName = 'sbitConvertToTabSpace' then Result := sbitConvertToTabSpace
  else
  if IndentName = 'sbitConvertToTabOnly'  then Result := sbitConvertToTabOnly;
end;


{-------------------------------------------------------------------------------
  GetTrimSpaceName
 ------------------------------------------------------------------------------}
function TEditorOptions.GetTrimSpaceName(IndentType: TSynEditStringTrimmingType): string;
begin
  Result := '';
  case IndentType of
    settLeaveLine:  Result := 'LeaveLine';
    settEditLine:   Result := 'EditLine';
    settMoveCaret:  Result := 'MoveCaret';
    settIgnoreAll:  Result := 'PosOnly';
  end;
end;


{-------------------------------------------------------------------------------
  GetTrimSpaceType
 ------------------------------------------------------------------------------}
function TEditorOptions.GetTrimSpaceType(IndentName: String): TSynEditStringTrimmingType;
begin
  Result := settLeaveLine;
  if IndentName = 'EditLine'  then Result := settEditLine
  else
  if IndentName = 'MoveCaret' then Result := settMoveCaret
  else
  if IndentName = 'PosOnly'   then Result := settIgnoreAll;
end;


{-------------------------------------------------------------------------------
  AssignKeyMapTo
 ------------------------------------------------------------------------------}
procedure TEditorOptions.AssignKeyMapTo(ASynEdit: TSynEdit; SimilarEdit: TSynEdit);
begin

end;


{-------------------------------------------------------------------------------
  CreateSyn
 ------------------------------------------------------------------------------}
function TEditorOptions.CreateSyn(LazSynHilighter: TLazSyntaxHighlighter): TSrcIDEHighlighter;
begin
  if LazSyntaxHighlighterClasses[LazSynHilighter] <> Nil then
  begin
    Result := LazSyntaxHighlighterClasses[LazSynHilighter].Create(Nil);
    GetHighlighterSettings(Result);
  end
  else
    Result := Nil;
end;


{-------------------------------------------------------------------------------
  ReadColorScheme
 ------------------------------------------------------------------------------}
function TEditorOptions.ReadColorScheme(const LanguageName: String): String;
(* The name of the currently chosen color-scheme for that language *)
begin
  if LanguageName = '' then
  begin
    Result := ColorSchemeFactory.ColorSchemeGroupAtPos[0].Name;
    exit;
  end;
  Result := Settings.Read( 'EditorOptions/Color/Lang' + StrToValidXMLName(LanguageName) + '/ColorScheme/Value', '');

  if ColorSchemeFactory.ColorSchemeGroup[Result] = nil then
    Result := '';
  if Result = '' then
    Result := ReadPascalColorScheme;
end;


{-------------------------------------------------------------------------------
  ReadPascalColorScheme
 ------------------------------------------------------------------------------}
function TEditorOptions.ReadPascalColorScheme: String;
(* The name of the currently chosen color-scheme for pascal code *)
begin
  Result := Settings.Read('EditorOptions/Color/ColorScheme', '');
  if ColorSchemeFactory.ColorSchemeGroup[Result] = nil then
    Result := '';
  if (Result = '') then
  begin
    if DefaultColorSchemeName <> '' then
      Result := DefaultColorSchemeName
    else
      Result := ColorSchemeFactory.ColorSchemeGroupAtPos[0].Name;
  end;
end;


{-------------------------------------------------------------------------------
  WriteColorScheme
 ------------------------------------------------------------------------------}
procedure TEditorOptions.WriteColorScheme(const LanguageName, SynColorScheme: String);
begin
  if (LanguageName = '') or (SynColorScheme = '') then exit;

  // TODO: delete Logic
  Settings.Write('EditorOptions/Color/Lang' + StrToValidXMLName(LanguageName) + '/ColorScheme/Value', SynColorScheme);
  Settings.Write('EditorOptions/Color/Version', EditorOptsFormatVersion);
end;


{-------------------------------------------------------------------------------
  ReadHighlighterSettings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.ReadHighlighterSettings(Syn: TSrcIDEHighlighter;
  SynColorScheme: String);
var
  Scheme: TColorScheme;
  LangScheme: TColorSchemeLanguage;
begin
  // initialize with defaults
  if SynColorScheme = '' then
    SynColorScheme := ReadColorScheme(Syn.LanguageName);
  if (SynColorScheme = '') or (Syn.LanguageName = '') then
    exit;

  Scheme := UserColorSchemeGroup.ColorSchemeGroup[SynColorScheme];
  if Scheme = nil then
    exit;
  LangScheme := Scheme.ColorSchemeBySynClass[Syn.ClassType];
  if LangScheme = nil then
    exit;

  LangScheme.ApplyTo(Syn);
end;


{-------------------------------------------------------------------------------
  ReadHighlighterFoldSettings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.ReadHighlighterFoldSettings(Syn: TSrcIDEHighlighter; ReadForOptions: Boolean);
var
  ConfName: String;
  Path: String;
  i, h, idx: Integer;
  TheFoldInfo: EditorOldOptionsFoldRecord;
  DefHl, FoldHl: TSynCustomFoldHighlighter;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;

  if (syn is TSynCustomFoldHighlighter) then begin
    DefHl := TSynCustomFoldHighlighter(TCustomSynClass(Syn.ClassType).Create(nil));
    try
      ReadDefaultsForHighlighterFoldSettings(DefHl);
      FoldHl := TSynCustomFoldHighlighter(Syn);
      TheFoldInfo := EditorOptionsFoldDefaults[HighlighterList[h].TheType];
      for i := 0 to TheFoldInfo.Count - 1 do begin
        idx := TheFoldInfo.Info^[i].Index;
        ConfName := TheFoldInfo.Info^[i].Xml;
        Path := 'EditorOptions/FoldConfig/Lang' +
          StrToValidXMLName(Syn.LanguageName) + '/Type' + ConfName + '/' ;
      // try reading the old config first
      FoldHl.FoldConfig[idx].Enabled :=
        Settings.Read(Path + 'Enabled/Value', FoldHl.FoldConfig[idx].Enabled);
      Settings.ReadObject(Path + 'Settings/', FoldHl.FoldConfig[idx], DefHl.FoldConfig[idx]);

        (* if ReadForOptions=True then Enabled appies only to fmFold,fmHide.
           This allows to store what selection was previously active *)
        if not ReadForOptions then begin
          if (not FoldHl.FoldConfig[idx].Enabled) or (not FUseCodeFolding) then
            FoldHl.FoldConfig[idx].Modes := FoldHl.FoldConfig[idx].Modes - [fmFold, fmHide];
          if (not FUseMarkupWordBracket) then
            FoldHl.FoldConfig[idx].Modes := FoldHl.FoldConfig[idx].Modes - [fmMarkup];
          if (not FUseMarkupOutline) then
            FoldHl.FoldConfig[idx].Modes := FoldHl.FoldConfig[idx].Modes - [fmOutline];

          FoldHl.FoldConfig[idx].Enabled := FoldHl.FoldConfig[idx].Modes <> [];
        end;
      end;
    finally
      DefHl.Free;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  ReadDefaultsForHighlighterFoldSettings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.ReadDefaultsForHighlighterFoldSettings(Syn: TSrcIDEHighlighter);
var
  i, h: Integer;
  TheFoldInfo: EditorOldOptionsFoldRecord;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  if (syn is TSynCustomFoldHighlighter) then begin
    TheFoldInfo := EditorOptionsFoldDefaults[HighlighterList[h].TheType];
    for i := 0 to TheFoldInfo.Count - 1 do
      with TSynCustomFoldHighlighter(Syn).FoldConfig[TheFoldInfo.Info^[i].Index] do begin
        Enabled := TheFoldInfo.Info^[i].Enabled;
      end;
  end;
end;


{-------------------------------------------------------------------------------
  WriteHighlighterFoldSettings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.WriteHighlighterFoldSettings(Syn: TSrcIDEHighlighter);
var
  DefSyn: TSrcIDEHighlighter;
  i, h:   Integer;
  Path:   String;
  ConfName: String;
  TheFoldInfo: EditorOldOptionsFoldRecord;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;

  DefSyn := TCustomSynClass(Syn.ClassType).Create(Nil);
  try
    ReadDefaultsForHighlighterFoldSettings(DefSyn);

    if (syn is TSynCustomFoldHighlighter) then begin
      TheFoldInfo := EditorOptionsFoldDefaults[HighlighterList[h].TheType];
      for i := 0 to TheFoldInfo.Count - 1 do begin
        ConfName := TheFoldInfo.Info^[i].Xml;
        Path := 'EditorOptions/FoldConfig/Lang' +
          StrToValidXMLName(Syn.LanguageName) + '/Type' + ConfName + '/' ;
        Settings.DeletePath(Path + 'Enabled/');
        Settings.WriteObject(Path + 'Settings/',
          TSynCustomFoldHighlighter(Syn).FoldConfig[TheFoldInfo.Info^[i].Index],
          TSynCustomFoldHighlighter(DefSyn).FoldConfig[TheFoldInfo.Info^[i].Index]);
      end;
    end;

  finally
    DefSyn.Free;
  end;
end;


{-------------------------------------------------------------------------------
  ReadHighlighterDivDrawSettings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.ReadHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
var
  TheInfo: EditorOldOptionsDividerRecord;
  Conf: TSynDividerDrawConfig;
  ConfName: String;
  Path: String;
  i, h: Integer;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  TheInfo := EditorOptionsDividerDefaults[HighlighterList[h].TheType];

  ReadDefaultsForHighlighterDivDrawSettings(Syn);

  // read settings, that are different from the defaults
  for i := 0 to TheInfo.Count - 1 do begin
    Conf := Syn.DividerDrawConfig[i];
    ConfName := TheInfo.Info^[i].Xml;
    Path := 'EditorOptions/DividerDraw/Lang' + StrToValidXMLName(Syn.LanguageName) + '/Type' + ConfName + '/' ;
    Conf.MaxDrawDepth := Settings.Read(Path + 'MaxDepth/Value',  Conf.MaxDrawDepth);
    Conf.TopColor     := Settings.Read(Path + 'TopColor/Value',  Conf.TopColor);
    Conf.NestColor    := Settings.Read(Path + 'NestColor/Value', Conf.NestColor);
  end;
end;


{-------------------------------------------------------------------------------
  ReadDefaultsForHighlighterDivDrawSettings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.ReadDefaultsForHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
var
  TheInfo: EditorOldOptionsDividerRecord;
  i, h: Integer;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  TheInfo := EditorOptionsDividerDefaults[HighlighterList[h].TheType];
  for i := 0 to TheInfo.Count - 1 do begin
    Syn.DividerDrawConfig[i].MaxDrawDepth := TheInfo.Info^[i].MaxLeveL;
    Syn.DividerDrawConfig[i].TopColor := clDefault;
    Syn.DividerDrawConfig[i].NestColor := clDefault;
  end;
end;


{-------------------------------------------------------------------------------
  WriteHighlighterDivDrawSettings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.WriteHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
var
  DefSyn: TSrcIDEHighlighter;
  i, h:   Integer;
  Path:   String;
  Conf, DefConf: TSynDividerDrawConfig;
  TheInfo: EditorOldOptionsDividerRecord;
  ConfName: String;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  TheInfo := EditorOptionsDividerDefaults[HighlighterList[h].TheType];

  DefSyn := TCustomSynClass(Syn.ClassType).Create(Nil);
  try
    ReadDefaultsForHighlighterDivDrawSettings(DefSyn);
    for i := 0 to TheInfo.Count - 1 do begin
      Conf := Syn.DividerDrawConfig[i];
      DefConf := DefSyn.DividerDrawConfig[i]; // default value
      ConfName := TheInfo.Info^[i].Xml;
      Path := 'EditorOptions/DividerDraw/Lang' + StrToValidXMLName(Syn.LanguageName) + '/Type' + ConfName + '/' ;
      Settings.WriteOrDefault(Path + 'MaxDepth/Value',  Conf.MaxDrawDepth, DefConf.MaxDrawDepth);
      Settings.WriteOrDefault(Path + 'TopColor/Value',  Conf.TopColor,     DefConf.TopColor);
      Settings.WriteOrDefault(Path + 'NestColor/Value', Conf.NestColor,    DefConf.NestColor);
    end;

  finally
    DefSyn.Free;
  end;
end;


{-------------------------------------------------------------------------------
  GetHighlighterSettings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.GetHighlighterSettings(Syn: TSrcIDEHighlighter);
// read highlight settings from config file
begin
  ReadHighlighterSettings(Syn, '');
  ReadHighlighterFoldSettings(Syn);
  ReadHighlighterDivDrawSettings(Syn);
end;


{-------------------------------------------------------------------------------
  SetMarkupColors
 ------------------------------------------------------------------------------}
procedure TEditorOptions.SetMarkupColors(aSynEd: TSynEdit);
var
  Scheme: TColorSchemeLanguage;
  SchemeGrp: TColorScheme;
  SynColorScheme: String;
  hl: TSynCustomHighlighter;
begin
  // Find current color scheme for default colors
  if (aSynEd.Highlighter = nil) then begin
    aSynEd.Color := clWhite;
    aSynEd.Font.Color := clBlack;
    exit;
  end;

  // get current colorscheme:
  SynColorScheme := ReadColorScheme(aSynEd.Highlighter.LanguageName);
  SchemeGrp := UserColorSchemeGroup.ColorSchemeGroup[SynColorScheme];
  if SchemeGrp = nil then
    exit;
  hl := TSynCustomHighlighter(TransformScheme(TObject(aSynEd.Highlighter)));

  if      hl.ClassName = 'TSynHTMLSyn'    then Scheme := SchemeGrp.GetColorScheme(lshHTML)
  else if hl.ClassName = 'TSynSpellMdSyn' then Scheme := SchemeGrp.GetColorScheme(lshMd)
  else Scheme := Nil;

  if Assigned(Scheme) then Scheme.ApplyTo(aSynEd);

  if Assigned(Scheme) then Scheme.ApplyColorsTo(aSynEd);
end;


{-------------------------------------------------------------------------------
  SetMarkupColor
 ------------------------------------------------------------------------------}
procedure TEditorOptions.SetMarkupColor(Syn : TSrcIDEHighlighter;
  AddHilightAttr : TAdditionalHilightAttribute; aMarkup : TSynSelectedColor);
var
  SynColorScheme: String;
  SchemeGrp: TColorScheme;
  Scheme: TColorSchemeLanguage;
  Attrib: TColorSchemeAttribute;
begin
  if assigned(Syn) then
  begin
    SynColorScheme := ReadColorScheme(Syn.LanguageName);
    SchemeGrp := UserColorSchemeGroup.ColorSchemeGroup[SynColorScheme];
    if SchemeGrp = nil then
      exit;
    Scheme := SchemeGrp.ColorSchemeBySynClass[Syn.ClassType];
  end else begin
    SchemeGrp := UserColorSchemeGroup.ColorSchemeGroup[DefaultColorSchemeName];
    if SchemeGrp = nil then
      exit;
    Scheme := SchemeGrp.DefaultColors;
  end;

  Attrib := Scheme.AttributeByEnum[AddHilightAttr];
  if Attrib <> nil then
  begin
    Attrib.ApplyTo(aMarkup);
    exit;
  end;

  // set default
  aMarkup.Foreground := clNone;
  aMarkup.Background := clNone;
  aMarkup.FrameColor := clNone;
  aMarkup.FrameEdges := sfeAround;
  aMarkup.FrameStyle := slsSolid;
  aMarkup.Style      := [];
  aMarkup.StyleMask  := [];
end;


{-------------------------------------------------------------------------------
  ApplyFontSettingsTo
 ------------------------------------------------------------------------------}
procedure TEditorOptions.ApplyFontSettingsTo(ASynEdit: TSynEdit);
begin
  ASynEdit.Font.Size := fEditorFontSize;// set size before name for XLFD !
  ASynEdit.Font.Name := fEditorFont;
  if fDisableAntialiasing then
    ASynEdit.Font.Quality := fqNonAntialiased
  else
    ASynEdit.Font.Quality := fqDefault;
end;


{-------------------------------------------------------------------------------
  ExtensionToLazSyntaxHighlighter
 ------------------------------------------------------------------------------}
function TEditorOptions.ExtensionToLazSyntaxHighlighter(Ext: String): TLazSyntaxHighlighter;
var
  s, CurExt: String;
  LangID, StartPos, EndPos: Integer;
begin
  Result := lshNone;
  if (Ext = '') or (Ext = '.') or (HighlighterList = Nil) then
    exit;
  Ext := lowercase(Ext);
  if (Ext[1] = '.') then
    Ext := copy(Ext, 2, length(Ext) - 1);
  LangID := 0;
  while LangID < HighlighterList.Count do
  begin
    s := HighlighterList[LangID].FileExtensions;
    StartPos := 1;
    while StartPos <= length(s) do
    begin
      Endpos := StartPos;
      while (EndPos <= length(s)) and (s[EndPos] <> ';') do
        inc(EndPos);
      CurExt := copy(s, Startpos, EndPos - StartPos);
      if (CurExt <> '') and (CurExt[1] = '.') then
        CurExt := copy(CurExt, 2, length(CurExt) - 1);
      if lowercase(CurExt) = Ext then
      begin
        Result := HighlighterList[LangID].TheType;
        exit;
      end;
      Startpos := EndPos + 1;
    end;
    inc(LangID);
  end;
end;


{-------------------------------------------------------------------------------
  GetSynEditSettings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.GetSynEditSettings(ASynEdit: TSynEdit;
  SimilarEdit: TSynEdit);
// read synedit settings from config file
// if SimilarEdit is given it is used for speed up
var
  MarkCaret: TSynEditMarkupHighlightAllCaret;
  b: TSynBeautifierPascal;
  Markup: TSynEditMarkup;
begin
  // general options
  ASynEdit.BeginUpdate(False);
  try
    ASynEdit.Options := fSynEditOptions;
    ASynEdit.Options2 := fSynEditOptions2;
    ASynEdit.BlockIndent := fBlockIndent;
    ASynEdit.BlockTabIndent := FBlockTabIndent;
    (ASynEdit.Beautifier as TSynBeautifier).IndentType := fBlockIndentType;
    if ASynEdit.Beautifier is TSynBeautifierPascal then
    begin
      b := ASynEdit.Beautifier as TSynBeautifierPascal;

      if FAnsiCommentContinueEnabled then begin
        b.AnsiCommentMode := sccPrefixMatch;
        b.AnsiIndentMode := FAnsiIndentMode;
        b.AnsiMatch := FAnsiCommentMatch;
        b.AnsiPrefix := FAnsiCommentPrefix;
        b.AnsiMatchLine := sclMatchPrev;
        b.AnsiMatchMode := AnsiCommentMatchMode;
        b.AnsiCommentIndent := sbitCopySpaceTab;
        b.AnsiIndentFirstLineMax := AnsiIndentAlignMax;
      end
      else begin
        b.AnsiCommentMode := sccNoPrefix;
        b.AnsiIndentMode := [];
      end;

      if FCurlyCommentContinueEnabled then begin
        b.BorCommentMode := sccPrefixMatch;
        b.BorIndentMode := FCurlyIndentMode;
        b.BorMatch := FCurlyCommentMatch;
        b.BorPrefix := FCurlyCommentPrefix;
        b.BorMatchLine := sclMatchPrev;
        b.BorMatchMode := CurlyCommentMatchMode;
        b.BorCommentIndent := sbitCopySpaceTab;
        b.BorIndentFirstLineMax := CurlyIndentAlignMax;
      end
      else begin
        b.BorCommentMode := sccNoPrefix;
        b.BorIndentMode := [];
      end;

      if FSlashCommentContinueEnabled then begin
        b.SlashCommentMode := sccPrefixMatch;
        b.SlashIndentMode := FSlashIndentMode;
        b.SlashMatch := FSlashCommentMatch;
        b.SlashPrefix := FSlashCommentPrefix;
        b.SlashMatchLine := sclMatchPrev;
        b.SlashMatchMode := SlashCommentMatchMode;
        b.SlashCommentIndent := sbitCopySpaceTab;
        b.ExtendSlashCommentMode := FSlashCommentExtend;
        b.SlashIndentFirstLineMax := SlashIndentAlignMax;
      end
      else begin
        b.SlashCommentMode := sccNoPrefix;
        b.SlashIndentMode := [];
      end;

      b.StringBreakEnabled := FStringBreakEnabled;
      b.StringBreakAppend  := FStringBreakAppend;
      b.StringBreakPrefix  := FStringBreakPrefix;

    end;

    ASynEdit.TrimSpaceType := FTrimSpaceType;
    ASynEdit.TabWidth := fTabWidth;
    ASynEdit.BracketHighlightStyle := FBracketHighlightStyle;

    // Display options
    ASynEdit.Gutter.Visible := fVisibleGutter;
    ASynEdit.Gutter.AutoSize := true;
    ASynEdit.Gutter.LineNumberPart.Visible := fShowLineNumbers;
    ASynEdit.Gutter.LineNumberPart(0).ShowOnlyLineNumbersMultiplesOf :=
      fShowOnlyLineNumbersMultiplesOf;
    ASynEdit.RightGutter.Visible := ShowOverviewGutter;

    ASynEdit.Gutter.CodeFoldPart.Visible := FUseCodeFolding;
    if not FUseCodeFolding then
      ASynEdit.UnfoldAll;
    ASynEdit.Gutter.CodeFoldPart.ReversePopMenuOrder := ReverseFoldPopUpOrder;

    ASynEdit.Gutter.Width := fGutterWidth;
    ASynEdit.Gutter.SeparatorPart.Visible := FGutterSeparatorIndex <> -1;
    if FGutterSeparatorIndex <> -1 then
    ASynEdit.Gutter.SeparatorPart(0).Index := FGutterSeparatorIndex;

    ASynEdit.RightEdge := fRightMargin;
    if fVisibleRightMargin then
      ASynEdit.Options := ASynEdit.Options - [eoHideRightMargin]
    else
      ASynEdit.Options := ASynEdit.Options + [eoHideRightMargin];

    ApplyFontSettingsTo(ASynEdit);
    //debugln(['EditorOldOptions.GetSynEditSettings ',ASynEdit.font.height]);

    ASynEdit.ExtraCharSpacing := fExtraCharSpacing;
    ASynEdit.ExtraLineSpacing := fExtraLineSpacing;
    ASynEdit.MaxUndo := fUndoLimit;
    // The Highlighter on the SynEdit will have been initialized with the configured
    // values already (including all the additional-attributes.
    // Just copy the colors from the SynEdit's highlighter to the SynEdit's Markup and co
    SetMarkupColors(ASynEdit);

    MarkCaret := TSynEditMarkupHighlightAllCaret(ASynEdit.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
    if assigned(MarkCaret) then
    begin
      if FMarkupCurWordNoTimer then
        MarkCaret.WaitTime := 0
      else
        MarkCaret.WaitTime := FMarkupCurWordTime;
      MarkCaret.FullWord := FMarkupCurWordFullLen > 0;
      MarkCaret.FullWordMaxLen := FMarkupCurWordFullLen;
      MarkCaret.IgnoreKeywords := FMarkupCurWordNoKeyword;
      MarkCaret.Trim := FMarkupCurWordTrim;
    end;

    Markup := ASynEdit.MarkupByClass[TSynEditMarkupFoldColors];
    if (Markup <> nil) then
    begin
      Markup.Enabled := FUseMarkupOutline;
    end;

    AssignKeyMapTo(ASynEdit, SimilarEdit);

    ASynEdit.MouseOptions := [emUseMouseActions];
    ASynEdit.MouseActions.Assign(FUserMouseSettings.MainActions);
    ASynEdit.MouseSelActions.Assign(FUserMouseSettings.SelActions);
    ASynEdit.MouseTextActions.Assign(FUserMouseSettings.TextActions);
    ASynEdit.Gutter.MouseActions.Assign(FUserMouseSettings.GutterActions);
    if ASynEdit.Gutter.CodeFoldPart <> nil then
    begin
      ASynEdit.Gutter.CodeFoldPart.MouseActions.Assign(FUserMouseSettings.GutterActionsFold);
      ASynEdit.Gutter.CodeFoldPart.MouseActionsCollapsed.Assign(FUserMouseSettings.GutterActionsFoldCol);
      ASynEdit.Gutter.CodeFoldPart.MouseActionsExpanded.Assign(FUserMouseSettings.GutterActionsFoldExp);
    end;
    if ASynEdit.Gutter.LineNumberPart <> nil then
    begin
      ASynEdit.Gutter.LineNumberPart.MouseActions.Assign(FUserMouseSettings.GutterActionsLines);
    end;
    if ASynEdit.Gutter.ChangesPart<> nil then
      ASynEdit.Gutter.ChangesPart.MouseActions.Assign(FUserMouseSettings.GutterActionsChanges);

    if (ASynEdit.Gutter.SeparatorPart <> nil) and (GutterSeparatorIndex = 2) and ShowLineNumbers then
      ASynEdit.Gutter.SeparatorPart.MouseActions.Assign(FUserMouseSettings.GutterActionsLines)
    else
    if (ASynEdit.Gutter.SeparatorPart <> nil) and (GutterSeparatorIndex >= 2) then
      ASynEdit.Gutter.SeparatorPart.MouseActions.Assign(FUserMouseSettings.GutterActionsChanges);
    if ASynEdit.RightGutter.LineOverviewPart <> nil then begin
      ASynEdit.RightGutter.LineOverviewPart.MouseActions.Assign(FUserMouseSettings.GutterActionsOverView);
      ASynEdit.RightGutter.LineOverviewPart.MouseActionsForMarks.Assign(FUserMouseSettings.GutterActionsOverViewMarks);
    end;
  finally
    ASynEdit.EndUpdate;
  end;
end;


{-------------------------------------------------------------------------------
  GetCodeTemplateFileNameExpand
 ------------------------------------------------------------------------------}
function TEditorOptions.GetCodeTemplateFileNameExpand:String;
begin
  result:=fCodeTemplateFileNameRaw;
end;


{-------------------------------------------------------------------------------
  Boolean
 ------------------------------------------------------------------------------}
function TEditorOptions.GetTabPosition: TTabPosition;
begin
  Result := fTabPosition;
end;


{-------------------------------------------------------------------------------
  GetSynEditPreviewSettings
 ------------------------------------------------------------------------------}
procedure TEditorOptions.GetSynEditPreviewSettings(APreviewEditor: TObject);
// read synedit setings from config file
var
  ASynEdit: TSynEdit;
begin
  if not (APreviewEditor is TSynEdit) then
    exit;
  ASynEdit := TSynEdit(APreviewEditor);

  // Get real settings
  GetSynEditSettings(ASynEdit);

  // Change to preview settings
  ASynEdit.Options := ASynEdit.Options
    - SynEditPreviewExcludeOptions + SynEditPreviewIncludeOptions;
  ASynEdit.Options2 := ASynEdit.Options2 - SynEditPreviewExcludeOptions2;
  ASynEdit.ReadOnly := True;
end;



{ TColorSchemeAttribute }

{-------------------------------------------------------------------------------
  OldAdditionalAttributeName
 ------------------------------------------------------------------------------}
function TColorSchemeAttribute.OldAdditionalAttributeName(NewAha: String): string;
var
  AttriIdx: Integer;
begin
  AttriIdx := GetEnumValue(TypeInfo(TAdditionalHilightAttribute), NewAha);
  if AttriIdx < 0
    then Result := NewAha
    else Result := ahaXmlNames[TAdditionalHilightAttribute(AttriIdx)];
end;


{-------------------------------------------------------------------------------
  SetMarkupFoldLineAlpha
 ------------------------------------------------------------------------------}
procedure TColorSchemeAttribute.SetMarkupFoldLineAlpha(AValue: Byte);
begin
  if FMarkupFoldLineAlpha = AValue then Exit;
  FMarkupFoldLineAlpha := AValue;
  Changed;
end;


{-------------------------------------------------------------------------------
  SetMarkupFoldLineColor
 ------------------------------------------------------------------------------}
procedure TColorSchemeAttribute.SetMarkupFoldLineColor(AValue: TColor);
begin
  if FMarkupFoldLineColor = AValue then Exit;
  FMarkupFoldLineColor := AValue;
  Changed;
end;


{-------------------------------------------------------------------------------
  SetMarkupFoldLineStyle
 ------------------------------------------------------------------------------}
procedure TColorSchemeAttribute.SetMarkupFoldLineStyle(AValue: TSynLineStyle);
begin
  if FMarkupFoldLineStyle = AValue then Exit;
  FMarkupFoldLineStyle := AValue;
  Changed;
end;


{-------------------------------------------------------------------------------
  Init
 ------------------------------------------------------------------------------}
procedure TColorSchemeAttribute.Init;
begin
  inherited Init;
  FFeatures := [hafBackColor, hafForeColor, hafFrameColor, hafStyle, hafFrameStyle, hafFrameEdges];
  FMarkupFoldLineColor := clNone;
  FMarkupFoldLineStyle := slsSolid;
  FMarkupFoldLineAlpha := 0;
end;


{-------------------------------------------------------------------------------
  GetIsUsingSchemeGlobals
 ------------------------------------------------------------------------------}
function TColorSchemeAttribute.GetIsUsingSchemeGlobals: Boolean;
begin
  Result := FUseSchemeGlobals and (GetSchemeGlobal <> nil);
end;


{-------------------------------------------------------------------------------
  GetSchemeGlobal
 ------------------------------------------------------------------------------}
function TColorSchemeAttribute.GetSchemeGlobal: TColorSchemeAttribute;
begin
  Result := nil;
  if (FOwner <> nil) and (FOwner.FOwner<> nil) and
     (FOwner.FOwner.FDefaultColors <> nil)
  then
    Result := FOwner.FOwner.FDefaultColors.Attribute[StoredName];
  if Result = Self then
    Result := nil;
end;


{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TColorSchemeAttribute.Create(ASchemeLang: TColorSchemeLanguage;
  attribName: PString; aStoredName: String = '');
begin
  inherited Create(attribName, aStoredName);
  FOwner := ASchemeLang;
  FUseSchemeGlobals := True;
end;


{-------------------------------------------------------------------------------
  IsEnabled
 ------------------------------------------------------------------------------}
function TColorSchemeAttribute.IsEnabled: boolean;
begin
  Result := (inherited IsEnabled) or (FMarkupFoldLineColor <> clNone);
end;


{-------------------------------------------------------------------------------
  ApplyTo
 ------------------------------------------------------------------------------}
procedure TColorSchemeAttribute.ApplyTo(aDest: TSynHighlighterAttributes;
  aDefault: TColorSchemeAttribute);
var
  Src: TColorSchemeAttribute;
begin
  if Not Assigned(aDest) then exit;
  Src := Self;
  if IsUsingSchemeGlobals then
    Src := GetSchemeGlobal;
  aDest.BeginUpdate;
  try
    aDest.Background := Src.Background;
    aDest.Foreground := Src.Foreground;
    aDest.FrameColor := Src.FrameColor;
    aDest.FrameEdges := Src.FrameEdges;
    aDest.FrameStyle := Src.FrameStyle;
    aDest.Style      := Src.Style;
    if hafStyleMask in Src.Features then
      aDest.StyleMask  := Src.StyleMask
    else
      aDest.StyleMask  := [low(TFontStyle)..high(TFontStyle)];

    if aDest is TSynHighlighterAttributesModifier then begin
      TSynHighlighterAttributesModifier(aDest).ForeAlpha := Src.ForeAlpha;
      TSynHighlighterAttributesModifier(aDest).BackAlpha := Src.BackAlpha;
      TSynHighlighterAttributesModifier(aDest).FrameAlpha := Src.FrameAlpha;

      if hafPrior in Src.Features then begin
        TSynHighlighterAttributesModifier(aDest).ForePriority      := Src.ForePriority;
        TSynHighlighterAttributesModifier(aDest).BackPriority      := Src.BackPriority;
        TSynHighlighterAttributesModifier(aDest).FramePriority     := Src.FramePriority;
        TSynHighlighterAttributesModifier(aDest).BoldPriority      := Src.BoldPriority;
        TSynHighlighterAttributesModifier(aDest).ItalicPriority    := Src.ItalicPriority;
        TSynHighlighterAttributesModifier(aDest).UnderlinePriority := Src.UnderlinePriority;
      end;
    end;

    if not (aDest is TSynSelectedColor) then begin
      if aDefault <> nil then begin
        if aDefault.IsUsingSchemeGlobals then
          aDefault := aDefault.GetSchemeGlobal;
        if Background = clDefault then
          aDest.Background := aDefault.Background;
        if Foreground = clDefault then
          aDest.Foreground := aDefault.Foreground;
        if FrameColor = clDefault then begin
          aDest.FrameColor := aDefault.FrameColor;
          aDest.FrameEdges := aDefault.FrameEdges;
          aDest.FrameStyle := aDefault.FrameStyle;
        end;
      end;

      if aDest is TColorSchemeAttribute then
        TColorSchemeAttribute(aDest).Group := Src.Group;
    end;
  finally
    aDest.EndUpdate;
  end;
end;


{-------------------------------------------------------------------------------
  Assign
 ------------------------------------------------------------------------------}
procedure TColorSchemeAttribute.Assign(Src: TPersistent);
begin
  inherited Assign(Src);

 FFeatures := [hafBackColor, hafForeColor, hafFrameColor, hafStyle, hafFrameStyle, hafFrameEdges];
  if Src is TSynHighlighterAttributesModifier then
    FFeatures := FFeatures + [hafAlpha, hafPrior,hafStyleMask];

  if Src is TColorSchemeAttribute then begin
    FGroup := TColorSchemeAttribute(Src).FGroup;
    FUseSchemeGlobals := TColorSchemeAttribute(Src).FUseSchemeGlobals;
    FFeatures := TColorSchemeAttribute(Src).FFeatures;

    FMarkupFoldLineColor := TColorSchemeAttribute(Src).FMarkupFoldLineColor;;
    FMarkupFoldLineStyle := TColorSchemeAttribute(Src).FMarkupFoldLineStyle;;
    FMarkupFoldLineAlpha := TColorSchemeAttribute(Src).FMarkupFoldLineAlpha;;
  end;

end;


{-------------------------------------------------------------------------------
  Equals
 ------------------------------------------------------------------------------}
function TColorSchemeAttribute.Equals(Other: TColorSchemeAttribute): Boolean;
begin
  Result := (FGroup      = Other.FGroup) and
            (FUseSchemeGlobals = Other.FUseSchemeGlobals) and
            // ignore resourcestring Name and Caption
            (StoredName = Other.StoredName) and
            (Background  = Other.Background) and
            (Foreground  = Other.Foreground) and
            (FrameColor  = Other.FrameColor) and
            ( (FrameColor = clNone) or
              ( (FrameStyle = Other.FrameStyle) and
                (FrameEdges = Other.FrameEdges)
              )
            ) and
            (Style       = Other.Style) and
            (StyleMask   = Other.StyleMask) and
            (Features   = Other.Features);
  end;


{-------------------------------------------------------------------------------
  GetStoredValuesForAttrib
 ------------------------------------------------------------------------------}
function TColorSchemeAttribute.GetStoredValuesForAttrib: TColorSchemeAttribute;
begin
  Result := nil;
  if (FOwner <> nil) and (FOwner.GetStoredValuesForLanguage <> nil) then
    Result := FOwner.GetStoredValuesForLanguage.Attribute[StoredName];
end;


{-------------------------------------------------------------------------------
  LoadFromXml
 ------------------------------------------------------------------------------}
procedure TColorSchemeAttribute.LoadFromXml(aSettings: TAppStorage; aPath: String;
  Defaults: TColorSchemeAttribute; Version: Integer);
var
  AttriName, Path: String;
  fs: TFontStyles;
begin
  // FormatVersion >= 2
  (* Note: This is currently always called with a default, so the nil handling isn't needed*)
  AttriName := OldAdditionalAttributeName(StoredName);
  if (Version < 5) and (AttriName <> '') then begin
    // Read Version 2-4, 4 if exist, or keep values
    Path := aPath + StrToValidXMLName(AttriName) + '/';

    if aSettings.HasChildPaths(Path) then begin
      if (Defaults <> nil) then
        self.Assign(Defaults);
      Defaults := Self;

      BackGround := aSettings.Read(Path + 'Background', Defaults.Background);
      ForeGround := aSettings.Read(Path + 'Foreground', Defaults.Foreground);
      FrameColor := aSettings.Read(Path + 'FrameColor', Defaults.FrameColor);

      fs   := [];
      if aSettings.Read(Path + 'Style/Bold', fsBold in Defaults.Style) then
        Include(fs, fsBold);
      if aSettings.Read(Path + 'Style/Italic', fsItalic in Defaults.Style) then
        Include(fs, fsItalic);
      if aSettings.Read(Path + 'Style/Underline', fsUnderline in Defaults.Style) then
        Include(fs, fsUnderline);
      Style := fs;
      fs   := [];
      if aSettings.Read(Path + 'StyleMask/Bold', fsBold in Defaults.StyleMask) then
        Include(fs, fsBold);
      if aSettings.Read(Path + 'StyleMask/Italic', fsItalic in Defaults.StyleMask) then
        Include(fs, fsItalic);
      if aSettings.Read(Path + 'StyleMask/Underline', fsUnderline in Defaults.StyleMask) then
        Include(fs, fsUnderline);
      StyleMask := fs;
    end;
  end;

  // Read the Version >= 5 if exist, or keep values
  if StoredName = '' then exit;
  Path := aPath + StrToValidXMLName(StoredName) + '/';
  if (Version <= 5) and (Defaults = nil) then
    Defaults := GetSchemeGlobal;

  if aSettings.HasPath(Path, False) then
  begin
    aSettings.ReadObject(Path, Self, Defaults);

    if (Version <= 5) then
      UseSchemeGlobals := False;
  end
  else
  begin
    if (Defaults <> Self) and (Defaults <> nil) then
    begin
      // do not copy (Stored)Name or Features ...
      Background := Defaults.Background;
      Foreground := Defaults.Foreground;
      FrameColor := Defaults.FrameColor;
      FrameEdges := Defaults.FrameEdges;
      FrameStyle := Defaults.FrameStyle;
      Style      := Defaults.Style;
      StyleMask  := Defaults.StyleMask;
      UseSchemeGlobals := Defaults.UseSchemeGlobals;
      ForePriority      := Defaults.ForePriority;
      BackPriority      := Defaults.BackPriority;
      FramePriority     := Defaults.FramePriority;
      BoldPriority      := Defaults.BoldPriority;
      ItalicPriority    := Defaults.ItalicPriority;
      UnderlinePriority := Defaults.UnderlinePriority;
    end;
    if (Version <= 5) and (Defaults = Self) then     // Data was loaded above (Vers < 5)
      UseSchemeGlobals := False;
  end;
end;


{-------------------------------------------------------------------------------
  LoadFromXmlV1
 ------------------------------------------------------------------------------}
procedure TColorSchemeAttribute.LoadFromXmlV1(aSettings: TAppStorage; aPath: String;
  Defaults: TColorSchemeAttribute);
var
  fs: TFontStyles;
begin
  // FormatVersion = 1 (only pascal colors)
  if Defaults = nil then
    Defaults := Self;
  if StoredName = '' then exit;
  aPath := aPath + StrToValidXMLName(StoredName) + '/';
  BackGround := aSettings.Read(aPath + 'Background', Defaults.Background);
  ForeGround := aSettings.Read(aPath + 'Foreground', Defaults.Foreground);
  FrameColor := aSettings.Read(aPath + 'FrameColor', Defaults.FrameColor);

  fs := [];
  if aSettings.Read(aPath + 'Bold', fsBold in Defaults.Style) then
    Include(fs, fsBold);
  if aSettings.Read(aPath + 'Italic', fsItalic in Defaults.Style) then
    Include(fs, fsItalic);
  if aSettings.Read(aPath + 'Underline', fsUnderline in Defaults.Style) then
    Include(fs, fsUnderline);
  Style := fs;
  StyleMask := [];
end;


{-------------------------------------------------------------------------------
  SaveToXml
 ------------------------------------------------------------------------------}
procedure TColorSchemeAttribute.SaveToXml(aSettings: TAppStorage; aPath: String;
  Defaults: TColorSchemeAttribute);
var
  AttriName: String;
begin
  if StoredName = '' then
    exit;
  // Delete Version <= 4
  AttriName := OldAdditionalAttributeName(StoredName);
  if AttriName <> '' then
    aSettings.DeletePath(aPath + StrToValidXMLName(AttriName));

  aSettings.WriteObject(aPath + StrToValidXMLName(StoredName) + '/', Self, Defaults);
end;


{ TColorSchemeLanguage }

{-------------------------------------------------------------------------------
  GetAttribute
 ------------------------------------------------------------------------------}
function TColorSchemeLanguage.GetAttribute(Index: String): TColorSchemeAttribute;
var
  Idx: Integer;
begin
  Idx := FAttributes.IndexOf(UpperCase(Index));
  if Idx = -1 then
    Result := nil
  else
    Result := TColorSchemeAttribute(FAttributes.Objects[Idx]);
end;


{-------------------------------------------------------------------------------
  GetAttributeAtPos
 ------------------------------------------------------------------------------}
function TColorSchemeLanguage.GetAttributeAtPos(Index: Integer): TColorSchemeAttribute;
begin
  Result := TColorSchemeAttribute(FAttributes.Objects[Index]);
end;


{-------------------------------------------------------------------------------
  GetAttributeByEnum
 ------------------------------------------------------------------------------}
function TColorSchemeLanguage.GetAttributeByEnum(Index: TAdditionalHilightAttribute): TColorSchemeAttribute;
begin
  Result := Attribute[AhaToStoredName(Index)];
end;


{-------------------------------------------------------------------------------
  GetName
 ------------------------------------------------------------------------------}
function TColorSchemeLanguage.GetName: String;
begin
  Result := FOwner.Name;
end;


{-------------------------------------------------------------------------------
  AhaToStoredName
 ------------------------------------------------------------------------------}
function TColorSchemeLanguage.AhaToStoredName(aha: TAdditionalHilightAttribute): String;
begin
  Result := GetEnumName(TypeInfo(TAdditionalHilightAttribute), ord(aha));
end;


{-------------------------------------------------------------------------------
  GetStoredValuesForLanguage
 ------------------------------------------------------------------------------}
function TColorSchemeLanguage.GetStoredValuesForLanguage: TColorSchemeLanguage;
begin
  Result := nil;
  if (FOwner <> nil) and (FOwner.GetStoredValuesForScheme <> nil) then
    Result := FOwner.GetStoredValuesForScheme.ColorScheme[FLanguage];
end;


{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TColorSchemeLanguage.Create(const AGroup: TColorScheme;
  const ALang: TLazSyntaxHighlighter; IsSchemeDefault: Boolean = False);
begin
  inherited Create;
  FIsSchemeDefault := IsSchemeDefault;
  FAttributes := TQuickStringlist.Create;
  FOwner := AGroup;
  FHighlighter := nil;
  FLanguage := ALang;
  if LazSyntaxHighlighterClasses[ALang] <> nil then
  begin
    FHighlighter := LazSyntaxHighlighterClasses[ALang].Create(nil);
    FLanguageName := FHighlighter.LanguageName;
  end;
  FDefaultAttribute := TColorSchemeAttribute.Create(Self, @dlgStrAddHiAttrDefault, 'ahaDefault');
  FDefaultAttribute.Features := [hafBackColor, hafForeColor];
  FDefaultAttribute.Group := agnDefault;
  FAttributes.AddObject(UpperCase(FDefaultAttribute.StoredName), FDefaultAttribute);
  FAttributes.Sorted := true;
end;


{-------------------------------------------------------------------------------
  CreateFromXml
 ------------------------------------------------------------------------------}
constructor TColorSchemeLanguage.CreateFromXml(const AGroup: TColorScheme;
  const ALang: TLazSyntaxHighlighter; aSettings: TAppStorage; aPath: String;
  IsSchemeDefault: Boolean);
var
  csa: TColorSchemeAttribute;
  i: Integer;
  aha: TAdditionalHilightAttribute;
  FormatVersion: longint;
begin
  Create(AGroup, ALang, IsSchemeDefault);   // don't call inherited Create

  FAttributes.Sorted := False;
  if FHighlighter <> nil then
  begin
    for i := 0 to FHighlighter.AttrCount - 1 do
    begin
      csa := TColorSchemeAttribute.Create(Self,
                                   FHighlighter.Attribute[i].Caption,
                                   FHighlighter.Attribute[i].StoredName
                                   );
      csa.Assign(FHighlighter.Attribute[i]);
      csa.Group := agnLanguage;
      FAttributes.AddObject(UpperCase(csa.StoredName), csa);
    end;
  end;

  for aha := Low(TAdditionalHilightAttribute) to High(TAdditionalHilightAttribute) do
  begin
    if aha = ahaNone then continue;
    csa := TColorSchemeAttribute.Create(Self, @AdditionalHighlightAttributes[aha],
                                        AhaToStoredName(aha)
                                       );
    csa.Features := ahaSupportedFeatures[aha];
    csa.Group    := ahaGroupMap[aha];
    FAttributes.AddObject(UpperCase(csa.StoredName), csa);
  end;

  FAttributes.Sorted := true;
  FormatVersion := aSettings.Read(aPath + 'Version', 0);

  LoadFromXml(aSettings, aPath, nil, FormatVersion);
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TColorSchemeLanguage.Destroy;
begin
  Clear;
  FreeAndNil(FHighlighter);
  FreeAndNil(FAttributes);
end;


{-------------------------------------------------------------------------------
  Clear
 ------------------------------------------------------------------------------}
procedure TColorSchemeLanguage.Clear;
var
  i: Integer;
begin
  if Assigned(FAttributes) then
    for i := 0 to FAttributes.Count - 1 do
      TColorSchemeAttribute(FAttributes.Objects[i]).Free;
  FAttributes.Clear;
end;


{-------------------------------------------------------------------------------
  Assign
 ------------------------------------------------------------------------------}
procedure TColorSchemeLanguage.Assign(Src: TColorSchemeLanguage);
var
  i, j: Integer;
  Attr: TColorSchemeAttribute;
  NewList: TQuickStringlist;
begin
  // Do not clear old list => external references to Attributes may exist
  FLanguage := Src.FLanguage;
  FLanguageName := src.FLanguageName;
  FDefaultAttribute := nil;
  NewList := TQuickStringlist.Create;
  for i := 0 to Src.AttributeCount - 1 do
  begin
    j := FAttributes.IndexOf(UpperCase(Src.AttributeAtPos[i].StoredName));
    if j >= 0 then
    begin
      Attr := TColorSchemeAttribute(FAttributes.Objects[j]);
      FAttributes.Delete(j);
    end
    else
      Attr := TColorSchemeAttribute.Create(Self,
                                       Src.AttributeAtPos[i].Caption,
                                       Src.AttributeAtPos[i].StoredName);
    Attr.Assign(Src.AttributeAtPos[i]);
    NewList.AddObject(UpperCase(Attr.StoredName), Attr);
    if Src.AttributeAtPos[i] = Src.DefaultAttribute then
      FDefaultAttribute := Attr;
  end;
  Clear;
  FreeAndNil(FAttributes);
  FAttributes := NewList;
  FAttributes.Sorted := true;
end;


{-------------------------------------------------------------------------------
  Equals
 ------------------------------------------------------------------------------}
function TColorSchemeLanguage.Equals(Other: TColorSchemeLanguage): Boolean;
var
  i: Integer;
begin
  Result := (FLanguage = Other.FLanguage) and
            (FAttributes.Count = Other.FAttributes.Count);
  i := FAttributes.Count - 1;
  while Result and (i >= 0) do
  begin
    Result := Result and
              (Other.Attribute[AttributeAtPos[i].StoredName] <> nil) and
              AttributeAtPos[i].Equals(Other.Attribute[AttributeAtPos[i].StoredName]);
    dec(i);
  end;
end;


{-------------------------------------------------------------------------------
  IndexOfAttr
 ------------------------------------------------------------------------------}
function TColorSchemeLanguage.IndexOfAttr(AnAttr: TColorSchemeAttribute): Integer;
begin
  Result := FAttributes.IndexOfObject(AnAttr);
end;


{-------------------------------------------------------------------------------
  LoadFromXml
 ------------------------------------------------------------------------------}
procedure TColorSchemeLanguage.LoadFromXml(aSettings: TAppStorage; aPath: String;
  Defaults: TColorSchemeLanguage; ColorVersion: Integer; aOldPath: String);
var
  Def: TColorSchemeAttribute;
  FormatVersion: longint;
  TmpPath: String;
  i: Integer;
  EmptyDef: TColorSchemeAttribute;
begin
  if not FIsSchemeDefault then
    TmpPath := aPath + 'Lang' + StrToValidXMLName(FLanguageName) + '/'
  else
    TmpPath := aPath;
  if aSettings.HasChildPaths(TmpPath) then
  begin
    FormatVersion := aSettings.Read(TmpPath + 'Version', 0);
    if FormatVersion > ColorVersion then
      FormatVersion := ColorVersion;
    if FIsSchemeDefault and (FormatVersion < 6) then
      FormatVersion := 6;
  end
  else
  begin
    FormatVersion := 6;
  end;
  FFormatVersion := FormatVersion;
  // TODO: Kontrollieren
  TmpPath := TmpPath + 'Scheme' + StrToValidXMLName(Name) + '/';

  if (aOldPath <> '') and (FormatVersion > 1) then
  begin
    // convert some old data (loading user settings only):
    // aOldPath should be 'EditorOptions/Display/'
    if aSettings.Read(aOldPath + 'RightMarginColor', '') <> '' then
      aSettings.Write(TmpPath + 'ahaRightMargin/Foreground',
                          aSettings.Read(aOldPath + 'RightMarginColor', 0)
                         );
    if aSettings.Read(aOldPath + 'GutterColor', '') <> '' then
      aSettings.Write(TmpPath + 'ahaGutter/Background',
                          aSettings.Read(aOldPath + 'GutterColor', 0)
                         );
  end;

  //   Attribute has SchemeDefault => Save diff to SchemeDefault
  //     SchemeDefault_Attri.UseSchemeGlobals must be TRUE => so it serves as default
  //   Attribute hasn't SchemeDefault => Save diff to empty
  if (Defaults = nil) then
    EmptyDef := TColorSchemeAttribute.Create(Self, nil, '')
  else
    EmptyDef := nil;

  for i := 0 to AttributeCount - 1 do
  begin
    if Defaults <> nil then
      Def := Defaults.Attribute[AttributeAtPos[i].StoredName]
    else
    begin
      if AttributeAtPos[i].GetSchemeGlobal <> nil then
        Def := AttributeAtPos[i].GetSchemeGlobal
      else
        Def := EmptyDef;
    end;

    if FormatVersion < 2 then
    begin
      AttributeAtPos[i].LoadFromXmlV1(aSettings, aPath, Def)
    end
    else
    begin
      AttributeAtPos[i].LoadFromXml(aSettings, TmpPath, Def, FormatVersion);
    end;

    if (ColorVersion < 9) and (AttributeAtPos[i].StoredName = AhaToStoredName(ahaMouseLink)) then
    begin
      AttributeAtPos[i].FrameColor := AttributeAtPos[i].Foreground;
      AttributeAtPos[i].Background := clNone;
      AttributeAtPos[i].Style := [];
      AttributeAtPos[i].StyleMask := [];
      AttributeAtPos[i].FrameStyle := slsSolid;
      AttributeAtPos[i].FrameEdges := sfeBottom;
    end;

    if (ColorVersion < 12) and (AttributeAtPos[i].Group = agnOutlineColors) then begin
      AttributeAtPos[i].MarkupFoldLineColor := AttributeAtPos[i].Foreground;
    end
  end;
  FreeAndNil(EmptyDef);

  // Version 5 and before stored the global background on the Whitespace attribute.
  // If a whitespace Attribute was loaded (UseSchemeGlobals=false) then copy it
  if (FormatVersion <= 5) and (DefaultAttribute <> nil) and
      (FHighlighter <> nil) and (FHighlighter.WhitespaceAttribute <> nil) and
      (Attribute[Highlighter.WhitespaceAttribute.StoredName] <> nil) and
      (not Attribute[Highlighter.WhitespaceAttribute.StoredName].UseSchemeGlobals)
  then
    DefaultAttribute.Background := Attribute[Highlighter.WhitespaceAttribute.StoredName].Background;
end;


{-------------------------------------------------------------------------------
  SaveToXml
 ------------------------------------------------------------------------------}
procedure TColorSchemeLanguage.SaveToXml(aSettings: TAppStorage; aPath: String;
  Defaults: TColorSchemeLanguage);
var
  Def: TColorSchemeAttribute;
  i: Integer;
  EmptyDef: TColorSchemeAttribute;
begin
  if (FLanguageName = '') and (not FIsSchemeDefault) then
    exit;
  if not FIsSchemeDefault then
    aPath := aPath + 'Lang' + StrToValidXMLName(FLanguageName) + '/';
  if (Defaults <> nil) and Self.Equals(Defaults) then begin
    aSettings.DeletePath(aPath + 'Scheme' + StrToValidXMLName(Name));
    if not FIsSchemeDefault then begin
      if not aSettings.HasChildPaths(aPath) then
        aSettings.DeletePath(aPath);
    end;
    exit;
  end;
  aSettings.Write(aPath + 'Version', EditorOptsFormatVersion);
  aPath := aPath + 'Scheme' + StrToValidXMLName(Name) + '/';

  if (Defaults = nil) then
    EmptyDef := TColorSchemeAttribute.Create(Self, nil, '')
  else
    EmptyDef := nil;

  for i := 0 to AttributeCount - 1 do begin
    if Defaults <> nil then
      Def := Defaults.Attribute[AttributeAtPos[i].StoredName]
    else begin
      if AttributeAtPos[i].GetSchemeGlobal <> nil then
        Def := AttributeAtPos[i].GetSchemeGlobal
      else
        Def := EmptyDef;
    end;
//    Def := Nil;
    AttributeAtPos[i].SaveToXml(aSettings, aPath, Def);
  end;
  FreeAndNil(EmptyDef);
end;


{-------------------------------------------------------------------------------
  ApplyColorsTo
 ------------------------------------------------------------------------------}
procedure TColorSchemeLanguage.ApplyColorsTo(ASynEdit: TSynEdit);
var
  i, j: Integer;
  att    : TSynHighlighterAttributes;
  newAtt : TColorSchemeAttribute;
begin
  for i := 0 to Self.AttributeCount-1 do
  begin
    newAtt := Self.AttributeAtPos[i];
    if Not Assigned(newAtt) then continue;

    if newAtt.Name ='Comment'    then newAtt.ApplyTo(ASynEdit.Highlighter.CommentAttribute);
    if newAtt.Name ='Identifier' then newAtt.ApplyTo(ASynEdit.Highlighter.IdentifierAttribute);
    if newAtt.Name ='Keyword'    then newAtt.ApplyTo(ASynEdit.Highlighter.KeywordAttribute);
    if newAtt.Name ='String'     then newAtt.ApplyTo(ASynEdit.Highlighter.StringAttribute);
    if newAtt.Name ='Symbol'     then newAtt.ApplyTo(ASynEdit.Highlighter.SymbolAttribute);
    if newAtt.Name ='Space'      then newAtt.ApplyTo(ASynEdit.Highlighter.WhitespaceAttribute);

    for j := 0 to ASynEdit.Highlighter.AttrCount-1 do
    begin
      att := ASynEdit.Highlighter.Attribute[j];
      if Not Assigned(att) then continue;

      if att.Name = newAtt.Name then
      begin
        newAtt.ApplyTo(att);
        break;
      end;
    end;
    if j > ASynEdit.Highlighter.AttrCount-1 then
    begin
       att := ASynEdit.Highlighter.AddSpecialAttribute(newAtt.Caption, newAtt.Name);
       newAtt.ApplyTo(att);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  ApplyTo
 ------------------------------------------------------------------------------}
procedure TColorSchemeLanguage.ApplyTo(ASynEdit: TSynEdit);

  procedure SetMarkupColor(aha: TAdditionalHilightAttribute; aMarkup : TSynSelectedColor);
  var Attrib: TColorSchemeAttribute;
  begin
    Attrib := AttributeByEnum[aha];
    if Attrib <> nil then
      Attrib.ApplyTo(aMarkup)
    else
      DefaultAttribute.ApplyTo(aMarkup);
  end;

  procedure SetMarkupColorByClass(aha: TAdditionalHilightAttribute; aClass: TSynEditMarkupClass);
  begin
    if Assigned(ASynEdit.MarkupByClass[aClass]) then
      SetMarkupColor(aha, ASynEdit.MarkupByClass[aClass].MarkupInfo);
  end;

  procedure SetGutterColorByClass(aha: TAdditionalHilightAttribute; aClass: TSynGutterPartBaseClass);
  begin
    if Assigned(ASynEdit.Gutter.Parts.ByClass[aClass, 0]) then
      SetMarkupColor(aha, ASynEdit.Gutter.Parts.ByClass[aClass, 0].MarkupInfo);
  end;

  function GetUsedAttr(aha: TAdditionalHilightAttribute): TColorSchemeAttribute;
  begin
    Result := AttributeByEnum[aha];
    if Assigned(Result) and Result.IsUsingSchemeGlobals then
      Result := Result.GetSchemeGlobal;
  end;

var
  Attri: TColorSchemeAttribute;
  i, c, j: Integer;
  aha: TAdditionalHilightAttribute;
begin
  ASynEdit.BeginUpdate;
  try
    try
      Attri := DefaultAttribute;
      if Attri.IsUsingSchemeGlobals then
        Attri := Attri.GetSchemeGlobal;
      if (Attri.Background = clNone) or (Attri.Background = clDefault)
        then aSynEdit.Color := clWhite
        else aSynEdit.Color := Attri.Background;
      if (Attri.Foreground = clNone) or (Attri.Foreground = clDefault)
        then aSynEdit.Font.Color := clBlack
        else aSynEdit.Font.Color := Attri.Foreground;
    except
      aSynEdit.Color := clWhite;
      aSynEdit.Font.Color := clBlack;
    end;

    Attri := GetUsedAttr(ahaGutter);
    if Attri <> nil then
    begin
      aSynEdit.Gutter.Color := Attri.Background;
    end;

    Attri := GetUsedAttr(ahaLineNumber);
    if Attri <> nil then
    begin
      aSynEdit.Gutter.LineNumberPart.MarkupInfo.Background := Attri.Background;
      aSynEdit.Gutter.LineNumberPart.MarkupInfo.Foreground := Attri.Foreground;
      aSynEdit.Gutter.LineNumberPart.MarkupInfo.StyleMask  := Attri.StyleMask;
      aSynEdit.Gutter.LineNumberPart.MarkupInfo.Style      := Attri.Style;
      aSynEdit.Gutter.LineNumberPart.MarkupInfo.FrameStyle := Attri.FrameStyle;
      aSynEdit.Gutter.LineNumberPart.MarkupInfo.FrameEdges := Attri.FrameEdges;
      aSynEdit.Gutter.LineNumberPart.MarkupInfo.FrameColor := Attri.FrameColor;
    end;

    Attri := GetUsedAttr(ahaCodeFoldingTree);
    if Attri <> nil then
    begin
      aSynEdit.Gutter.CodeFoldPart.MarkupInfo.Background := Attri.Background;
      aSynEdit.Gutter.CodeFoldPart.MarkupInfo.Foreground := Attri.Foreground;
      aSynEdit.Gutter.CodeFoldPart.MarkupInfo.StyleMask  := Attri.StyleMask;
      aSynEdit.Gutter.CodeFoldPart.MarkupInfo.Style      := Attri.Style;
      aSynEdit.Gutter.CodeFoldPart.MarkupInfo.FrameStyle := Attri.FrameStyle;
      aSynEdit.Gutter.CodeFoldPart.MarkupInfo.FrameEdges := Attri.FrameEdges;
      aSynEdit.Gutter.CodeFoldPart.MarkupInfo.FrameColor := Attri.FrameColor;
    end;

    Attri := GetUsedAttr(ahaGutterSeparator);
    if Attri <> nil then
    begin
      aSynEdit.Gutter.SeparatorPart.MarkupInfo.Background := Attri.Background;
      aSynEdit.Gutter.SeparatorPart.MarkupInfo.Foreground := Attri.Foreground;
    end;

    Attri := GetUsedAttr(ahaRightMargin);
    if Attri <> nil then
    begin
      aSynEdit.RightEdgeColor := Attri.Foreground;
    end;

    SetMarkupColor(ahaTextBlock,         aSynEdit.SelectedColor);
    SetMarkupColor(ahaIncrementalSearch, aSynEdit.IncrementColor);
    SetMarkupColor(ahaHighlightAll,      aSynEdit.HighlightAllColor);
    SetMarkupColor(ahaBracketMatch,      aSynEdit.BracketMatchColor);
    SetMarkupColor(ahaMouseLink,         aSynEdit.MouseLinkColor);
    SetMarkupColor(ahaFoldedCode,        aSynEdit.FoldedCodeColor);
    SetMarkupColor(ahaFoldedCodeLine,    aSynEdit.FoldedCodeLineColor);
    SetMarkupColor(ahaHiddenCodeLine,    aSynEdit.HiddenCodeLineColor);
    SetMarkupColor(ahaLineHighlight,     aSynEdit.LineHighlightColor);

    i := aSynEdit.PluginCount - 1;
    while (i >= 0) and not(aSynEdit.Plugin[i] is TSynPluginTemplateEdit) do
      dec(i);
    if i >= 0 then begin
      SetMarkupColor(ahaTemplateEditOther,TSynPluginTemplateEdit(aSynEdit.Plugin[i]).MarkupInfo);
      SetMarkupColor(ahaTemplateEditCur,  TSynPluginTemplateEdit(aSynEdit.Plugin[i]).MarkupInfoCurrent);
      SetMarkupColor(ahaTemplateEditSync, TSynPluginTemplateEdit(aSynEdit.Plugin[i]).MarkupInfoSync);
    end;
    i := aSynEdit.PluginCount - 1;
    while (i >= 0) and not(aSynEdit.Plugin[i] is TSynPluginSyncroEdit) do
      dec(i);
    if i >= 0 then begin
      SetMarkupColor(ahaSyncroEditOther, TSynPluginSyncroEdit(aSynEdit.Plugin[i]).MarkupInfo);
      SetMarkupColor(ahaSyncroEditCur,   TSynPluginSyncroEdit(aSynEdit.Plugin[i]).MarkupInfoCurrent);
      SetMarkupColor(ahaSyncroEditSync,  TSynPluginSyncroEdit(aSynEdit.Plugin[i]).MarkupInfoSync);
      SetMarkupColor(ahaSyncroEditArea,  TSynPluginSyncroEdit(aSynEdit.Plugin[i]).MarkupInfoArea);
    end;
    i := aSynEdit.MarkupCount - 1;
    while (i >= 0) and not(aSynEdit.Markup[i] is TSynEditMarkupFoldColors) do
      dec(i);
    if i >= 0 then begin
      TSynEditMarkupFoldColors(aSynEdit.Markup[i]).ColorCount := 10;
      j := 0;
      c := 0;
      for aha := ahaOutlineLevel1Color to ahaOutlineLevel10Color do begin
        Attri := GetUsedAttr(aha);
        if Attri = nil then Continue;
        if (Attri.IsEnabled) or
           (FFormatVersion >= 12)
        then begin
          SetMarkupColor(aha, TSynEditMarkupFoldColors(aSynEdit.Markup[i]).Color[j]);

          TSynEditMarkupFoldColors(aSynEdit.Markup[i]).LineColor[j].Color := Attri.MarkupFoldLineColor;
          TSynEditMarkupFoldColors(aSynEdit.Markup[i]).LineColor[j].Style := Attri.MarkupFoldLineStyle;
          TSynEditMarkupFoldColors(aSynEdit.Markup[i]).LineColor[j].Alpha := Attri.MarkupFoldLineAlpha;
          TSynEditMarkupFoldColors(aSynEdit.Markup[i]).LineColor[j].Priority := Attri.FramePriority;
          inc(j);
          if Attri.IsEnabled then
            c := j;
        end;
      end;
      TSynEditMarkupFoldColors(aSynEdit.Markup[i]).ColorCount := c; // discard unused colors at the end
    end;
  finally
    ASynEdit.EndUpdate;
  end;
end;


{-------------------------------------------------------------------------------
  ApplyTo
 ------------------------------------------------------------------------------}
procedure TColorSchemeLanguage.ApplyTo(AHLighter: TSynCustomHighlighter);
var
  i: Integer;
  Attr: TColorSchemeAttribute;
begin
  AHLighter.BeginUpdate;
  try
    for i := 0 to AHLighter.AttrCount - 1 do begin
      Attr := Attribute[AHLighter.Attribute[i].StoredName];
      if Attr <> nil then
        Attr.ApplyTo(AHLighter.Attribute[i], DefaultAttribute);
    end;
  finally
    AHLighter.EndUpdate;
  end;
end;


{-------------------------------------------------------------------------------
  AttributeCount
 ------------------------------------------------------------------------------}
function TColorSchemeLanguage.AttributeCount: Integer;
begin
  Result := FAttributes.Count;
end;


{ TColorScheme }

{-------------------------------------------------------------------------------
  GetColorScheme
 ------------------------------------------------------------------------------}
function TColorScheme.GetColorScheme(Index: TLazSyntaxHighlighter): TColorSchemeLanguage;
begin
  Result := FColorSchemes[CompatibleLazSyntaxHilighter[Index]];
end;


{-------------------------------------------------------------------------------
  GetColorSchemeBySynClass
 ------------------------------------------------------------------------------}
function TColorScheme.GetColorSchemeBySynClass(Index: TClass): TColorSchemeLanguage;
var
  i: TLazSyntaxHighlighter;
  tc: TClass;
begin
  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
  begin
    if LazSyntaxHighlighterClasses[i] = Nil then continue;

    if LazSyntaxHighlighterClasses[i] = Index then
      exit(FColorSchemes[CompatibleLazSyntaxHilighter[i]]);

    tc := Index.ClassParent;
    if tc = LazSyntaxHighlighterClasses[i] then
    begin
      OFormDebug.Append := Index.ClassName + '  ' + LazSyntaxHighlighterClasses[i].ClassName + '  ' + tc.ClassName;
      exit(FColorSchemes[CompatibleLazSyntaxHilighter[i]]);
    end;

  end;
  Result := nil;
end;


{-------------------------------------------------------------------------------
  GetStoredValuesForScheme
 ------------------------------------------------------------------------------}
function TColorScheme.GetStoredValuesForScheme: TColorScheme;
begin
  Result := ColorSchemeFactory.ColorSchemeGroup[Name];
end;


{-------------------------------------------------------------------------------
  Boolean
 ------------------------------------------------------------------------------}
constructor TColorScheme.Create(AName: String);
begin
  inherited Create;
  FName := AName;
end;


{-------------------------------------------------------------------------------
  CreateFromXml
 ------------------------------------------------------------------------------}
constructor TColorScheme.CreateFromXml(aSettings: TAppStorage; const AName, aPath: String);
var
  i: TLazSyntaxHighlighter;
begin
  Create(AName);
  FDefaultColors := TColorSchemeLanguage.CreateFromXml(Self, lshNone, aSettings, aPath  + 'Globals/', True);

  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    // do not create duplicates
    if CompatibleLazSyntaxHilighter[i] = i then
      FColorSchemes[i] := TColorSchemeLanguage.CreateFromXml(Self, i, aSettings, aPath)
    else
      FColorSchemes[i] := nil;
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TColorScheme.Destroy;
var
  i: TLazSyntaxHighlighter;
begin
  inherited Destroy;
  FreeAndNil(FDefaultColors);
  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    FreeAndNil(FColorSchemes[i]);
end;


{-------------------------------------------------------------------------------
  Assign
 ------------------------------------------------------------------------------}
procedure TColorScheme.Assign(Src: TColorScheme);
var
  i: TLazSyntaxHighlighter;
begin
  if Src.FDefaultColors = nil then
    FreeAndNil(FDefaultColors)
  else
  if (FDefaultColors = nil) then
    FDefaultColors := TColorSchemeLanguage.Create(Self, lshNone, True);
  if FDefaultColors <> nil then
    FDefaultColors.Assign(Src.FDefaultColors);
  for i := low(FColorSchemes) to high(FColorSchemes) do
  begin
    if Src.FColorSchemes[i] = nil then
    begin
      FreeAndNil(FColorSchemes[i]);
    end
    else
    begin
      if FColorSchemes[i] = nil then
        FColorSchemes[i] := TColorSchemeLanguage.Create(Self, i);
      FColorSchemes[i].Assign(Src.FColorSchemes[i]);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  LoadFromXml
 ------------------------------------------------------------------------------}
procedure TColorScheme.LoadFromXml(aSettings: TAppStorage; aPath: String;
  Defaults: TColorScheme; aOldPath: String);
var
  i: TLazSyntaxHighlighter;
  Def: TColorSchemeLanguage;
  FormatVersion: longint;
begin
  FormatVersion := aSettings.Read(aPath + 'Version', 0);
  if Defaults <> nil then
    Def := Defaults.DefaultColors
  else
    Def := nil;
  FDefaultColors.LoadFromXml(aSettings, aPath + 'Globals/', Def, FormatVersion);
  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    if ColorScheme[i] <> nil then
    begin
      if Defaults <> nil then
        Def := Defaults.ColorScheme[i]
      else
        Def := nil;
      ColorScheme[i].LoadFromXml(aSettings, aPath, Def, FormatVersion, aOldPath);
    end;
end;


{-------------------------------------------------------------------------------
  LoadSchemeFromXml
 ------------------------------------------------------------------------------}
procedure TColorScheme.LoadSchemeFromXml(aSettings: TAppStorage; aPath: String;
  Defaults: TColorScheme; aScheme: String);
var
  i: TLazSyntaxHighlighter;
  Def: TColorSchemeLanguage;
  FormatVersion: longint;
begin
  FormatVersion := aSettings.Read(aPath + 'Version', 0);
  if Defaults <> nil then
    Def := Defaults.DefaultColors
  else
    Def := nil;

  // Bereich Globale Einstellungen laden
  FDefaultColors.LoadFromXml(aSettings, aPath + 'Globals/', Def, FormatVersion);

  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    if ColorScheme[i] <> nil then
    begin
      if Defaults <> nil then
        Def := Defaults.ColorScheme[i]
      else
        Def := nil;
      ColorScheme[i].LoadFromXml(aSettings, aPath, Def, FormatVersion, '');
    end;
end;


{-------------------------------------------------------------------------------
  SaveToXml
 ------------------------------------------------------------------------------}
procedure TColorScheme.SaveToXml(aSettings: TAppStorage; aPath: String;
  Defaults: TColorScheme);
var
  i: TLazSyntaxHighlighter;
  Def: TColorSchemeLanguage;
begin
  if Defaults <> nil then
    Def := Defaults.DefaultColors
  else
    Def := nil;
  FDefaultColors.SaveToXml(aSettings, aPath + 'Globals/', Def);
  if not aSettings.HasChildPaths(aPath + 'Globals') then
    aSettings.DeletePath(aPath + 'Globals');
  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    if ColorScheme[i] <> nil then
    begin
      if Defaults <> nil then
        Def := Defaults.ColorScheme[i]
      else
        Def := nil;
      ColorScheme[i].SaveToXml(aSettings, aPath, Def);
    end;
  aSettings.Write(aPath + 'Version', EditorOptsFormatVersion);
end;


{ TColorSchemeFactory }

{-------------------------------------------------------------------------------
  GetColorSchemeGroup
 ------------------------------------------------------------------------------}
function TColorSchemeFactory.GetColorSchemeGroup(Index: String): TColorScheme;
var
  Idx: integer;
begin
  Idx := FMappings.IndexOf(UpperCase(Index));
  if Idx = -1 then
    Result := nil
  else
    Result := TColorScheme(FMappings.Objects[Idx]);
end;


{-------------------------------------------------------------------------------
  GetColorSchemeGroupAtPos
 ------------------------------------------------------------------------------}
function TColorSchemeFactory.GetColorSchemeGroupAtPos(Index: Integer): TColorScheme;
begin
  Result := TColorScheme(FMappings.Objects[Index]);
end;


{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TColorSchemeFactory.Create;
begin
  inherited Create;
  FMappings := TQuickStringlist.Create;
  FMappings.Sorted := true;
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TColorSchemeFactory.Destroy;
begin
  Clear;
  FreeAndNil(FMappings);
  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  Clear
 ------------------------------------------------------------------------------}
procedure TColorSchemeFactory.Clear;
var
  i: Integer;
begin
  if Assigned(FMappings) then
  begin
    for i := 0 to FMappings.Count - 1 do
      TColorScheme(FMappings.Objects[i]).Free;
    FMappings.Clear;
  end;
end;


{-------------------------------------------------------------------------------
  Assign
 ------------------------------------------------------------------------------}
procedure TColorSchemeFactory.Assign(Src: TColorSchemeFactory);
var
  lMapping: TColorScheme;
  i: Integer;
begin
  FMappings.Sorted := False;
  Clear;
  for i := 0 to Src.FMappings.Count - 1 do
  begin
    lMapping := TColorScheme.Create(Src.ColorSchemeGroupAtPos[i].Name);
    lMapping.Assign(Src.ColorSchemeGroupAtPos[i]);
    FMappings.AddObject(UpperCase(lMapping.Name), lMapping);
  end;
  FMappings.Sorted := true;
end;


{-------------------------------------------------------------------------------
  LoadFromXml
 ------------------------------------------------------------------------------}
procedure TColorSchemeFactory.LoadFromXml(aSettings: TAppStorage; aPath: String;
  Defaults: TColorSchemeFactory; aOldPath: String);
var
  i: Integer;
  Def: TColorScheme;
begin
  for i := 0 to FMappings.Count - 1 do
  begin
    if Defaults <> nil then
      Def := Defaults.ColorSchemeGroupAtPos[i]
    else
      Def := nil;
    ColorSchemeGroupAtPos[i].LoadFromXml(aSettings, aPath, Def, aOldPath);
  end;
  // all Schemes have read (and relocated) the old values
  if aOldPath <> '' then
  begin
    aSettings.DeletePath(aOldPath + 'RightMarginColor');
    aSettings.DeletePath(aOldPath + 'GutterColor');
  end;
end;


{-------------------------------------------------------------------------------
  LoadSchemeFromXml
 ------------------------------------------------------------------------------}
procedure TColorSchemeFactory.LoadSchemeFromXml(aSettings: TAppStorage;
  aPath: String; aDefault, aScheme: String);
var
  i: Integer;
  Def: TColorScheme;
  oldName: String;
begin
  Def := ColorSchemeGroup[aDefault];

  for i := 0 to FMappings.Count - 1 do
  begin
    oldName := ColorSchemeGroupAtPos[i].Name;
    if oldName <> aDefault then Continue;

    ColorSchemeGroupAtPos[i].Name := aScheme;
    ColorSchemeGroupAtPos[i].LoadSchemeFromXml(aSettings, aPath, Def, aScheme);
    ColorSchemeGroupAtPos[i].Name := oldName;
  end;
end;


{-------------------------------------------------------------------------------
  SaveToXml
 ------------------------------------------------------------------------------}
procedure TColorSchemeFactory.SaveToXml(aSettings: TAppStorage; aPath: String;
  Defaults: TColorSchemeFactory);
var
  i: Integer;
  Def: TColorScheme;
begin
  for i := 0 to FMappings.Count - 1 do begin
    if Defaults <> nil then
      Def := Defaults.ColorSchemeGroupAtPos[i]
    else
      Def := nil;
    ColorSchemeGroupAtPos[i].SaveToXml(aSettings, aPath, Def);
  end
end;


{-------------------------------------------------------------------------------
  RegisterScheme
 ------------------------------------------------------------------------------}
procedure TColorSchemeFactory.RegisterScheme(aSettings: TAppStorage; AName, aPath: String);
var
  i, j: integer;
  lMapping: TColorScheme;
begin
  i := FMappings.IndexOf(UpperCase(AName));
  if i <> -1 then begin
    j := 0;
    while i >= 0 do begin
      inc(j);
      i := FMappings.IndexOf(UpperCase(AName+'_'+IntToStr(j)));
    end;
    AName := AName+'_'+IntToStr(j);
  end;
  lMapping := TColorScheme.CreateFromXml(aSettings, AName, aPath);
  FMappings.AddObject(UpperCase(AName), lMapping);
end;


{-------------------------------------------------------------------------------
  GetRegisteredSchemes
 ------------------------------------------------------------------------------}
procedure TColorSchemeFactory.GetRegisteredSchemes(AList: TStrings);
var
  i: integer;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    for i := 0 to FMappings.Count - 1 do
      AList.Add(TColorScheme(FMappings.Objects[i]).Name);
  finally
    AList.EndUpdate;
  end;
end;


{ TQuickStringlist }

{-------------------------------------------------------------------------------
  DoCompareText
 ------------------------------------------------------------------------------}
function TQuickStringlist.DoCompareText(const s1, s2: string): PtrInt;
var
  i, l: Integer;
begin
  Result := length(s1) - length(s2);
  if Result <> 0 then
    exit;
  i := 1;
  if Result < 0 then
    l := length(s1)
  else
    l := length(s2);
  while i <= l do begin
    Result := ord(s1[i]) - ord(s2[i]);
    if Result <> 0 then
      exit;
    inc(i);
  end;
  Result := 0;
end;


{-------------------------------------------------------------------------------
  DebugNotify
 ------------------------------------------------------------------------------}
procedure TEditorOptions.DebugNotify(Sender: TObject);
var
  ary: TStringArray;
  mDbgLineBreak: Integer;
begin
  ary := OFormDebug.Command.Split(':');

  if Length(ary) > 0 then
  begin
    if ary[0] = 'Opt' then
    begin
      if Length(ary) > 1 then
      begin
        mDbgLineBreak := StrToInt(ary[1]);
        OFormDebug.Append:='TEditorOptions Opt:' + mDbgLineBreak.ToString() + ' OK!';
      end;
    end;
  end;
end;





initialization

{$I Resources/CnfSyneditOptions.lrs}

finalization
  ColorSchemeFactory.Free;
  HighlighterListSingleton.Free;


end.
