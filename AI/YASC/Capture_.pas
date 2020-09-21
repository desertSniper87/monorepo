{
Sokoban Puzzle and Skin Capture
Version 1.4 - Septamber 5, 2017
Copyright (c) 2011-17 by Brian Damgaard, Denmark

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Capture_;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Menus, ComCtrls, ExtDlgs,
  Spin, ToolWin, ImgList,
  IniFile_, SokUtil_,SokFile_, Misc_, Bitmap_;

////////////////////////////////////////////////////////////////////////////////
// Color quantization type declarations
// The color quantization implementation is based upon the article
// "Color Image Quantization by Pairwise Clustering" by
// Luiz Velho,  Jonas Gomes,  Marcos Vinicius, and Rayol Sobreiro.
type
  TCaptureColorItem = record
    Color : TColor;
    Count : Integer; // the frequency field would suffice, but since the floating point numbers better be 8-bytes aligned, there is room for keeping the count too
    Frequency : Double;
    Error : Double; // accumulated quantization error
  end;
  TCaptureColorItems = array[ 0 .. ( MaxInt div SizeOf( TCaptureColorItem ) ) - 1 ] of TCaptureColorItem;
  PCaptureColorItems = ^TCaptureColorItems;
  TCaptureColors = record
    Colors           : PCaptureColorItems;
    Count            : Integer; // number of colors in the set
    Capacity         : Integer; // length of the 'Colors' vector
    RGBComponentMask : Integer; // reduces the number of bits in each RGB component for the colors added to the set; typically, the last 3 bits are dropped, thereby reducing the palette to a manageable number of colors, and in effect performing an initial color quantization
    end;

////////////////////////////////////////////////////////////////////////////////
// Color analysis type declarations
  TPixel = Integer;
  PPixel = ^TPixel;
  TPixels = array[ 0 .. ( MaxInt div SizeOf( TPixel ) ) - 1 ] of TPixel;
  PPixels = ^TPixels;

////////////////////////////////////////////////////////////////////////////////
// Skin  capture information
const
  SOKOBAN_SKIN_IDENTIFICATION : array[ 0 .. 13 ] of AnsiChar =
    ( 'S', 'o', 'k', 'o', 'b', 'a', 'n', ' ', 's', 'k', 'i', 'n', #0, #0 );     // 12 8-bit characters + 2 trailing 8-bit null characters
type
  TBoardSquareType = ( bstUndefined, bstBox, bstBoxOnGoal, bstFloor, bstGoal, bstPlayer, bstPlayerOnGoal, bstWall ); // 'bstUndefined' must come first
  TBoardSquareTypeCounts = array[ TBoardSquareType ] of Integer;
  TBoardSquareTypeSet = set of TBoardSquareType;
  TSkinCaptureInformationHeader = packed record // the field sequence is fixed and cannot change; otherwise, the funtion 'ReadSkinCaptureInformation()' must be modified
    Identification : array[ 0.. SizeOf( SOKOBAN_SKIN_IDENTIFICATION ) - 1 ] of AnsiChar; // including two 8-bit trailing null character terminators
    HeaderByteSize : Int16; // for version detection
    TotalByteSize : Integer;
    ColumnWidth : Int16;
    RowHeight : Int16;
    ItemCount : Integer; // sum of 'CAPTURED_SKIN_TILE_COUNT' + the number of additional integral image values
    ItemOffsets : TBoardSquareTypeCounts; // offset per square type for the additional integral image values; the first 'CAPTURED_SKIN_TILE_COUNT' values are reserved for the values from the captured skin, starting with the 17 wall elements
    CapturedSquareTypes : Cardinal; // captured square types, as opposed to automatically created square types; the wall cap is the only wall element which is updated by this application
  end;
  TSkinCaptureInformation = packed record
    Header : TSkinCaptureInformationHeader;
    Items : array[ 0 .. ( ( MaxInt - SizeOf( TSkinCaptureInformationHeader ) ) div SizeOf( TPixel ) ) - 1 ] of TPixel; // the first 'CAPTURED_SKIN_TILE_COUNT' values are reserved for the values from the captured skin, starting with the 17 wall elements
    end;
  PSkinCaptureInformation = ^TSkinCaptureInformation;
////////////////////////////////////////////////////////////////////////////////

const
  CAPTURE_MAX_BOARD_HEIGHT                            = 64;
  CAPTURE_MAX_BOARD_WIDTH                             = CAPTURE_MAX_BOARD_HEIGHT;
  CAPTURE_MAX_BOARD_SIZE                              = (CAPTURE_MAX_BOARD_WIDTH + 2 ) * ( CAPTURE_MAX_BOARD_HEIGHT + 2 ); {'0' left/top border; '+1': right/bottom border}
  IMAGE_INDEX_ZOOM_IN                                 = 34;
  IMAGE_INDEX_ZOOM_OUT                                = 35;
  MAX_EDITOR_HISTORY_ITEMS                            = 100;
  MAX_IMAGE_HEIGHT                                    = 4096;
  MAX_IMAGE_WIDTH                                     = 4096;
  MIN_BOARD_SQUARE_SIZE_PIXELS                        = 4;
  MIN_BOARD_SIZE_PIXELS                               = 3 * MIN_BOARD_SQUARE_SIZE_PIXELS;

type
  TAnalyseImageResult = ( airGradientDirections, airHorzLines, airIntegralImage, airIntegralImage2, airVertLines );
  TAnalyseImageResultSet = set of TAnalyseImageResult;
  TCaptureBoardSquareItem = ( bsiBox, bsiFloor, bsiGoal, bsiPlayer, bsiWall );
  TCaptureBoardSquareItemSet = set of TCaptureBoardSquareItem;
  TCaptureBoard = record
     BoxCount : Integer;
     GoalCount : Integer;
     Height : Integer;
     PlayerPos : TPoint;
     Squares : array[ 0 .. CAPTURE_MAX_BOARD_WIDTH + 1, 0 .. CAPTURE_MAX_BOARD_HEIGHT + 1 ] of TCaptureBoardSquareItemSet;
     Width : Integer;
     end;
  TCaptureEditorCursorType = (ctCell, ctEraser, ctSelection, ctLeftLine, ctTopLine, ctRightLine, ctBottomLine, ctColumnLine, ctRowLine ); // the order cannot change
  TCaptureEditorHistory = record
    ItemIndex : Integer;
    Items : array[ 0 .. MAX_EDITOR_HISTORY_ITEMS ] of String; // not '0 .. MAX_EDITOR_HISTORY_ITEMS - 1'; the last item is unused and only present for convenient '(Index+=1) mod High( Items )' wrap-around calculation
    StackBottom : Integer;
    StackTop : Integer;
    TransactionIndex : Integer; // transaction in progress; '-1' = none;
  end;
  TCaptureEditor = record
    Board : TCaptureBoard;
    CapturedSquareTypes : Cardinal; // square types extracted from an image; e.g., 'player-on-floor'; the tiles in a skin are either extracted from an image or automatically created by the application
    CompletedStep : TCaptureStep;
    DrawingTool:TDrawingTool;
    DrawingToolsEnabled:Boolean;
    HasSelection:Boolean;  // the selection is active
    HasBoard:Boolean; // the selection contains a board fragment
    History : TCaptureEditorHistory;
    CursorRect:TRect; // current cursor position in pixels
    DragPoint : TPoint;
    EditorSkinBitmap : TBitmap;
    EditorSkinTileCount : Integer;
    Cursor : TCaptureEditorCursorType;
    GridColor : TColor;
    GridShadowColor : TColor;
    InputFileName : String;
    IsDragging:Boolean;
    IsResizing:Boolean;
    IsSelectingANewArea:Boolean;
    Modified : Boolean;
    MouseButton:TMouseButton;
    MouseButtonDown:Boolean;
    PuzzleFileName : String;
    SizeHandle:TGrabHandle;
    SkinFileName : String; // don't confuse 'TCaptureForm.Editor.SkinFileName' and 'TCaptureForm.EditorSkinFileName'
    SkinFileTime : TFileTime;
    SpinEditChangeIsUserInput : Integer; // reversed logic: 0: user input or treat as user input; non-zero: a change of a spin edit value is performed by the program, not by the user
    StartDragPoint : TPoint;
    Step : TCaptureStep;
    ToggledItemPos : TPoint;
    UserWidth, UserHeight : Integer; // the last rectangle size entered by the user; it isn't clamped to columns and rows like 'WidthSpinEdit.Value' and 'HeightSpinEdit.Value'
    Zoom : Integer; // must be 100, 200, or 400 to match the implemented menu items
    end;
  TCaptureSettings = record
    CheckForDuplicatesWhenAppendingNewLevelsToCollections : Boolean;
    ColorLevels : Integer;
    FindBoardTimeLimitMS : TTimeMS;
    MatchThresholdPct : Integer; //  a skin must match at least this percentage of the squares on the board before the matching is considered successful
    end;

  TCaptureForm = class(TForm)
    MainMenu: TMainMenu;
    CaptureHelpContentsItem: TMenuItem;
    CaptureHelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    RightPanel: TPanel;
    OpenPictureDialog1: TOpenPictureDialog;
    ColorDialog1: TColorDialog;
    CaptureButtonsAndSkinImage: TImage;
    CaptureHelpMenu: TMenuItem;
    SkinsComboBox: TComboBox;

    procedure FormCreate(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure FilePrint(Sender: TObject);
    procedure FilePrintSetup(Sender: TObject);
    procedure FileExit(Sender: TObject);
    procedure EditUndo(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure WindowTile(Sender: TObject);
    procedure WindowCascade(Sender: TObject);
    procedure WindowArrange(Sender: TObject);
    procedure HelpContents(Sender: TObject);
    procedure HelpSearch(Sender: TObject);
    procedure HelpHowToUse(Sender: TObject);
    procedure HelpAbout(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CaptureImage1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CaptureImage1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CaptureImage1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure ZoomItemClick(Sender: TObject);
    procedure LoadImageFromUpscaledViewOfImage( Sender: TObject );
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PreviousStepButtonClick(Sender: TObject);
    procedure NextStepButtonClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CaptureEditSelectAllItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SettingsColorLevelItemClick(Sender: TObject);
    procedure SettingsIgnoreSmallColorDifferencesClick(Sender: TObject);
    procedure CaptureViewColorQuantizationItemClick(Sender: TObject);
    procedure EditMenuItemDrawingToolClick(Sender: TObject);
    procedure EditRedoItemClick(Sender: TObject);
    procedure CaptureSettingsUseAutomaticCompletionItemClick(Sender: TObject);
    procedure WallCapCheckBoxClick(Sender: TObject);
    procedure CaptureViewImageAnalysisResultsItemClick(Sender: TObject);
    procedure SettingsImageAnalysisItemsClick(Sender: TObject);
    procedure SettingsSkinExportFormatTypeItemClick( Sender: TObject);
    procedure CaptureSettingsGridColorItemClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure SettingsSkinExportFormatObjectBackgroundClick(
      Sender: TObject);
    procedure CaptureSettingsSnapToNearbyEdgesItemClick(Sender: TObject);
    procedure CaptureSettingsKeepAuthorAndDesignerNamesItemClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure CaptureScrollBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure CaptureScrollBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure SpinEditChange(Sender: TObject);
    procedure SpinEditExit(Sender: TObject);
    procedure SettingsMenuItemDefaultSkinClick(Sender: TObject);
    procedure SettingsMenuItemSkinClick( Sender : TObject );
  private
    BackgroundBitmap : TBitmap;
    BoardSquareColors : array[ 0 .. CAPTURE_MAX_BOARD_WIDTH + 1, 0 .. CAPTURE_MAX_BOARD_HEIGHT + 1 ] of TColor;
    Colors : TCaptureColors;
    EditorSkinFileName : String; // don't confuse 'TCaptureForm.Editor.SkinFileName' and 'TCaptureForm.EditorSkinFileName'
    GradientDirections : PPixels;
    HorzLines : PPixels;
    fDefaultMatchingSkinFileName: String;
    fInitialized : Boolean;
    fMatchingSkinFileName : String;
    fSkinCaptureInformation : PSkinCaptureInformation;
    fSkinInitialDirectory : String;
    IntegralImage, IntegralImage2 : PPixels;
    IntegralImageWidth, IntegralImageHeight : Integer;
//  Log : TextFile;
//  LogEnabled : Boolean;
    OriginalBitmap : TBitmap;
    PuzzleInitialDirectory : String;
    SingleRowSkinBitmap : TBitmap;
    SkinBitmap : TBitmap;
    StatusText : String;
    Timestamp : TTimestamp;
    Timestamps : array[ 0 .. CAPTURE_MAX_BOARD_WIDTH + 1, 0 .. CAPTURE_MAX_BOARD_HEIGHT + 1 ] of Cardinal;
    VertLines : PPixels;
    UndoBitmap : TBitmap;
    procedure Clear;
    procedure ClearSelection;
    function  CloseCapture : Boolean;
    //procedure MouseToCell(X__, Y__: Integer; var Col__, Row__: Integer);
    procedure HideCursor(UpdateScreenCursor__:Boolean);
    procedure ShowCursor(Cursor__: TCaptureEditorCursorType; CursorRect__: TRect);
    procedure ScrollInView( Rect__: TRect );
    procedure SaveUndoBitmap;
    procedure ClearHistory;
    procedure ClearBoard;
    procedure ShowGrid( const Rect__ : TRect );
    procedure ColRowToXY(Col__, Row__: Integer; var X__, Y__: Integer);
    function  BitmapColorQuantization(Bitmap__: TBitmap; const Rect__: TRect; PaletteColorCount__: Integer; DoNotMergeDifferentColorGroups__ : Boolean; var Colors__: TCaptureColors): Boolean;
    function  SetColorLevel( Value__ : Integer ) : Boolean;
    function  RectangleClampedToColumnsRows(const Rect__: TRect; Columns__, Rows__: Integer; var NewRect__: TRect): Boolean;
    function  AnalyseBoardColors: Boolean;
    function  AnalyseImage( Bitmap__ : TBitmap; var Width__, Height__ : Integer; Results__ : TAnalyseImageResultSet; var HorzLines__, VertLines__, IntegralImage__, IntegralImage2__, GradientDirections__ : PPixels ) : Boolean;
    function  FindBoard( Bitmap__ : TBitmap;
                         BitmapRect__ : TRect;
                         Width__, Height__ : Integer;
                         HorzLines__, VertLines__, IntegralImage__, IntegralImage2__ : PPixels;
                         TimeLimitMS__ : TTimeMS;
                         var BoardRect__: TRect;
                         var ColCount__, RowCount__: Integer) : Boolean;
//  procedure XYToColRow( X__, Y__ : Integer; var Col__, Row__ : Integer); // 0-based col,row
    procedure XYToColRow(X__, Y__: Integer; var Col__, Row__: Integer);
    procedure SetCaptureDrawingToolHint;
    function  ColRowToRect(Col__, Row__: Integer): TRect;
    function  CreateEditorSkin( Bitmap__ : TBitmap; TileCount__, ColWidth__, RowHeight__ : Integer ) : Boolean;
    function  CreateSkinFromImage: Boolean;
    function  BoardSquareValue(Col__, Row__: Integer): TCaptureBoardSquareItemSet;
    function  BeginTransaction: Boolean;
    function  EndTransaction(Commit__: Boolean): Boolean;
    procedure ShowSquare( Col__,Row__:Integer; const Board__ : TCaptureBoard; Bitmap__, BackgroundBitmap__ : TBitmap );
    function  ShowBoard : Boolean;
    function  EditorAutomaticCompletion( Col__, Row__: Integer; SquareValue__ : TCaptureBoardSquareItemSet ): Integer;
    function  NeighborWalls( Col__, Row__: Integer; const Board__ : TCaptureBoard ): Integer;
    procedure WriteSkinSettings;
    procedure UpdateScrollBoxRange;
    function  HelpAboutText: String;
    function  ScaledPosition(Value__: Integer): Integer;
    function  ScaledSize(Value__: Integer): Integer;
    function  NormalizedPosition(Value__: Integer): Integer;
    function  NormalizedSize(Value__: Integer): Integer;
    function  NormalizedRect(const Rect__: TRect): TRect;
    function  NormalizedBoardRect: TRect;
    function  ScaledBoardRect: TRect;
    function  ScaledRect(const Rect__: TRect): TRect;
    function  SetSpinEditValue(SpinEdit__: TSpinEdit; Value__: Integer): Integer;
    procedure CountStraightLinePixels(Width__, Height__: Integer; HorzLines__, VertLines__: PPixels; const Rect__: TRect);
    function  FindBoardUsingUnscaledSkin(MatchingSkinBitmap__: TBitmap;
                                         SkinCaptureInformation__: PSkinCaptureInformation;
                                         SourceRect__: TRect;
                                         MatchThresholdPct__ : Integer;
                                         var BoardRect__: TRect;
                                         var Board__: TCaptureBoard): Boolean;
    function  FindBoardUsingScaledSkin  (Bitmap__: TBitmap;
                                         MatchingSkinBitmap__: TBitmap;
                                         MatchingSkinCaptureInformation__: PSkinCaptureInformation;
                                         const SourceRect__: TRect;
                                         ColCount__, RowCount__ : Integer;
                                         MatchThresholdPct__: Integer;
                                         var BoardRect__: TRect;
                                         var Board__: TCaptureBoard): Boolean;
    function  IntegralImageValueToBoardSquareType(Value__: TPixel; SkinCaptureInformation__: PSkinCaptureInformation): TBoardSquareType;
    function  ReadSkinCaptureInformation(BitMap__: TBitMap;
                                         JustReadColumnsAndRows__: Boolean;
                                         var ColumnCount__, RowCount__: Integer;
                                         var SkinCaptureInformation__: PSkinCaptureInformation): Boolean;
    function  SkinTileIndexToBoardSquareType(Index__: Integer; SkinCaptureInformation__: PSkinCaptureInformation): TBoardSquareType;
    function  BoardSquareItemSetToSkinTileIndex( Items__: TCaptureBoardSquareItemSet): Integer;
    function  BoardSquareToSkinTileIndex(Col__, Row__: Integer; const Board__: TCaptureBoard): Integer;
    function  LoadImageFromFile(const FileName__: String; var Bitmap__: TBitmap): Boolean;
    function  BoardSquareTypeToBoardSquareItemSet( BoardSquareType__: TBoardSquareType): TCaptureBoardSquareItemSet;
    procedure ColRowToNormalizedXY(Col__, Row__: Integer; var X__, Y__: Integer);
  protected
    function  AverageCellColor(Col__, Row__: Integer): TColor;
    function  BoardSquareGradientDirectionsHashValue(Col__, Row__: Integer): TColor;
    procedure DisplayPixels(const Caption__: String; Width__, Height__: Integer; Pixels__: PPixels; Scale__: Double);
    procedure OnMaximize(var Msg: TMessage); Message MSG_MAXIMIZE;
    procedure SetMatchingStringFileName( FileName__ :String );
  public
    DefaultSkinFileNameText : String;
    Editor : TCaptureEditor;
    Settings : TCaptureSettings;
    StatusBarPanel0Widths : array[ Boolean ] of Integer;
    procedure ApplicationOnActivate(Sender: TObject);
    procedure ApplicationOnDeactivate(Sender: TObject);
    procedure ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
    function  BoardAsText : String;
    function  BoardAsTextWithBorder( var BoardAsText__ : String; var BoardWidth__, BoardHeight__ : Integer ) : Boolean;
    function  CheckBoard( var Board__: TCaptureBoard; Bitmap__, BackgroundBitmap__ : TBitmap; AutomaticCompletion__ : Boolean; var BoardRect__ : TRect; var OpenSides__ : Boolean ): Boolean;
    function  CreateSingleRowSkinFromSkin(SkinBitmap__: TBitmap; ColWidth__, RowHeight__: Integer; var SingleRowSkinBitmap__: TBitmap): Boolean;
    function  CreateSkinCaptureInformation( SingleRowSkinBitmap__ : TBitmap; ColWidth__, RowHeight__ : Integer; UpdateMatchingSkin__ : Boolean; var MatchingSkinImagesChanged__ : Boolean ) : Boolean;
    function  CreateSkinCaptureInformationUpdatingMatchingSkin( var MatchingSkinImagesChanged__ : Boolean ) : Boolean;
    procedure EditCapture(Sender: TObject);
    function  FindBoardUsingMatchingSkin(const FileName__: String; MatchThresholdPct__ : Integer; ShowMessages__ : Boolean): Boolean;
    procedure Initialize;
    function  LoadImage(FileName: String): Boolean;
    function  LoadSettingsFromBitmap( Bitmap__ : TBitmap ) : Boolean;
    function  LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    function  MinHeight: Integer;
    function  MinWidth: Integer;
    function  ReadColumnsAndRows(Bitmap__: TBitmap; var ColCount__,RowCount__: Integer): Boolean;
    function  Save(Sender: TObject):Boolean;
    function  SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
    procedure SetSkinHints;
    procedure ShowImage(Zoom: Integer);
    function  ShowTitle: String;
    procedure ShowStatus;
    procedure SkinBitBtnClick(Sender: TObject);
    function  WriteSkinCaptureInformation(BitMap__: TBitmap; SkinCaptureInformation__: PSkinCaptureInformation): Boolean;

    property  DefaultMatchingSkinFileName: String read fDefaultMatchingSkinFileName;
    property  Initialized : Boolean read fInitialized;
    property  MatchingSkinFileName : String read fMatchingSkinFileName write SetMatchingStringFileName;
    property  SkinCaptureInformation : PSkinCaptureInformation read fSkinCaptureInformation;
    property  SkinInitialDirectory : String read fSkinInitialDirectory write fSkinInitialDirectory;
  end;

var
  CaptureForm: TCaptureForm = nil;

function  CaptureScreen( var FileName__ : String ) : Boolean;

implementation

{$R *.DFM}

uses Clipbrd, jpeg, Math, PNG_,
     Pack_,Text_,Options_,Main_,Tools_, Skins_, Open1_;

const
  ALL_DIRECTIONS = [ Up, Left, Down, Right ];
  BLACK_LIMIT = 32;
  CAPTURE_INIFILE_SECTION = 'Capture';   // don't localize
  CAPTURED_SKIN_TILE_COUNT = 31;
  CAPTURED_SKIN_TILE_WALL_CAP_INDEX = 16;
  DEFAULT_COLOR_LEVELS = 16;
  DEFAULT_SKIN_MATCHING_THRESHOLD_PERCENT = 55;
  DISCARD_COLOR_COMPONENT_LOW_BITS_COUNT = 3;
  MIN_AVAILABLE_VIRTUAL_MEMORY_FOR_ZOOM_400 = 300 * ONE_MEBI;                 // an arbitrary limit
  MAX_ZOOM_200_IMAGE_SIZE = ( IMAGE_WIDTH_4K + 2 ) * ( IMAGE_HEIGHT_4K + 2 ); // an arbitrary limit. must be bigger  than 'MAX_ZOOM_400_IMAGE_SIZE'. "+2": the capture tool adds two extra columns and rows
  MAX_ZOOM_400_IMAGE_SIZE = ( 2600 + 2 ) * ( 1600 + 2 );                      // an arbitrary limit. must be smaller than 'MAX_ZOOM_200_IMAGE_SIZE'. "+2": the capture tool adds two extra columns and rows
  PUZZLE_EXAMPLE_BOARD_AS_TEXT = '?#############?##   ## ##   ###  # @   +    ##      #   #  ## ### ###  #  ##      #   #  ### $   .   * ##?#############?'; // "?": UNDEFINED_SQUARE_CHAR
  PUZZLE_EXAMPLE_BOARD_HEIGHT = 8;
  PUZZLE_EXAMPLE_BOARD_WIDTH = 15;
  SINGLE_ROW_SKIN_TILE_COUNT = 32; // including one extra tile for settings
  SIZING_RECT_WIDTH = 8; //
  UNDEFINED_SQUARE_CHAR    = '?';
  WALL_NONE                = 0; // wall neighbors; for a wall square, the sum matches the element index in the built-in editor skin
  WALL_UP                  = 1;
  WALL_RIGHT               = 2;
  WALL_DOWN                = 4;
  WALL_LEFT                = 8;

function  CaptureScreen( var FileName__ : String ) : Boolean; // doesn't work;
const CAPTUREBLT = $40000000;
var ScreenWidth, ScreenHeight : Integer;
    DesktopWindow : HWND;
    DesktopDC, CaptureDC : HDC;
    CaptureBitmap : HBitmap;
    OldBitMap : HGDIOBJ;

  function SaveBitMap( const FileName__ : String; BitMap__ : HBitMap ) : Boolean;
  var
    HandleDC : HDC;
    FileHandle : THandle;
    Buffer : Pointer;
    BmpFileHeader : BITMAPFILEHEADER;
    BmpInformation : BITMAPINFO;
    BytesWritten : DWORD;

  begin
    Result := False;
    HandleDC := 0;
    FileHandle := INVALID_HANDLE_VALUE;
    Buffer := nil;
    ZeroMemory( Addr( BmpFileHeader ), SizeOf( BmpFileHeader ) );
    ZeroMemory( Addr( BmpInformation ), SizeOf( BmpInformation ) );
    BmpInformation.bmiHeader.biSize := SizeOf( BmpInformation.bmiHeader );
    try    try      HandleDC := GetDC( 0 );
       	            Result := ( HandleDC <> 0 ) and
                              ( GetDIBits( HandleDC, Bitmap__, 0, 0, nil, BmpInformation, DIB_RGB_COLORS ) <> 0 );
                    if BmpInformation.bmiHeader.biSizeImage <=0  then
                       BmpInformation.bmiHeader.biSizeImage :=
                          ( ( BmpInformation.bmiHeader.biWidth * BmpInformation.bmiHeader.biBitCount + 31 ) div 32 ) * 4 * Abs( BmpInformation.bmiHeader.biHeight );
                    if Result then
                       GetMem( Buffer, BmpInformation.bmiHeader.biSizeImage );
                    Result := Result and Assigned( Buffer );
                    BmpInformation.bmiHeader.biCompression := BI_RGB;
		    Result := Result and
                              ( GetDIBits( HandleDC, Bitmap__ , 0, BmpInformation.bmiHeader.biHeight, Buffer, BmpInformation, DIB_RGB_COLORS ) <> 0 ); // "GetDIBits()" fails most of the time here; reason unknown
                    if Result then begin
                       BmpFileHeader.bfReserved1 := 0;
		       BmpFileHeader.bfReserved2 := 0;
		       BmpFileHeader.bfSize      := SizeOf( BmpFileHeader ) + SizeOf( BmpInformation.bmiHeader ) + BmpInformation.bmiHeader.biSizeImage;
		       BmpFileHeader.bfType      := Ord( 'M' ) shl 8 + Ord( 'B' );
		       bmpFileHeader.bfOffBits   := Sizeof( BmpFileHeader ) + SizeOf( BmpInformation.bmiHeader );
                       FileHandle := CreateFile( PChar( FileName__ ), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );
                       Result := FileHandle <> INVALID_HANDLE_VALUE;
                       end;
                    Result := Result and
                              WriteFile( FileHandle, BmpFileHeader , SizeOf( BmpFileHeader  ), BytesWritten, nil ) and
                              ( BytesWritten = SizeOf( BmpFileHeader ) ) and
                              WriteFile( FileHandle, BmpInformation.bmiHeader, SizeOf( BmpInformation.bmiHeader ), BytesWritten, nil ) and
                              ( BytesWritten = SizeOf( BmpInformation.bmiHeader ) ) and
                              WriteFile( FileHandle, Buffer^, BmpInformation.bmiHeader.biSizeImage, BytesWritten, nil ) and
                              ( BytesWritten = BmpInformation.bmiHeader.biSizeImage );
           finally  if ( FileHandle <> INVALID_HANDLE_VALUE ) and ( not CloseHandle( FileHandle ) ) then Result := False;
                    if Assigned( Buffer ) then FreeMem( Buffer );
                    if ( HandleDC <> 0 ) and ( ReleaseDC( 0, HandleDC ) = 0 ) then Result := False;
                    if not Result then Windows.DeleteFile( PChar( FileName__ ) );
           end;
    except on E:Exception do Result := Error( E.Message, '' );
    end;
  end;

begin // 'CaptureScreen'
  Result := False;
  FileName__ := '';
  try
    ScreenWidth := GetSystemMetrics( SM_CXSCREEN );
    ScreenHeight := GetSystemMetrics( SM_CYSCREEN );
    DesktopWindow := GetDesktopWindow;
    DesktopDC := GetDC( DesktopWindow );
     if DesktopDC <> 0 then begin
        CaptureDC := CreateCompatibleDC( DesktopDC );
        if CaptureDC <> 0 then begin
           CaptureBitmap :=CreateCompatibleBitmap( DesktopDC, ScreenWidth, ScreenHeight );
           if CaptureBitmap <> 0 then begin
              OldBitMap := SelectObject( CaptureDC, CaptureBitmap );
              if ( OldBitMap <> 0 ) and ( OldBitMap <> HGDI_ERROR ) then begin
                 Result := BitBlt( CaptureDC, 0, 0, ScreenWidth, ScreenHeight, DesktopDC, 0, 0, SRCCOPY or CAPTUREBLT );
                 OldBitMap := SelectObject( CaptureDC, OldBitMap );
                 if  ( OldBitMap = 0 ) or ( OldBitMap = HGDI_ERROR ) then Result := False;
                 if Result then begin
                    FileName__ := MakeNewFileName( TEXT_SCREEN_CAPTURE, BMP_FILE_EXT, False);
                    Result := ( FileName__ <> '' ) and SaveBitmap( FileName__, CaptureBitmap );
                    end;
                 end;
              DeleteObject( CaptureBitmap );
              end;
           DeleteDC( CaptureDC );
           end;
        ReleaseDC( DesktopWindow, DesktopDC );
        end;
  except on E:Exception do Result := Error( E.Message, '' );
  end;
  if not Result then FileName__ := '';
end;

function  CalculateAverageColor( const Rect__ : TRect; Bitmap__ : TBitmap ) : TColor;
var X, Y, Count : Integer; P :PRGB;
    SumR, SumG, SumB : Int64;
begin // preconditions:  [Rect__.Left, Rect__.Top] >= [0, 0], and Assigned( Bitmap__ )
  SumR := 0; SumG := 0; SumB := 0; Count := 0;
  for Y := Rect__.Top to Pred( Min( Rect__.Bottom, Bitmap__.Height ) ) do begin
      P := Bitmap__.ScanLine[ Y ];
      Inc( P, Rect__.Left );
      for X := Rect__.Left to Pred( Min( Rect__.Right, Bitmap__.Width ) ) do begin
          Inc( SumR, P^.r );
          Inc( SumG, P^.g );
          Inc( SumB, P^.b );
          Inc( Count );
          Inc( P );
          end;
      end;
  if   Count > 0 then
       Result := RGBComponentsToColor( SumR div Count, SumG div Count, SumB div Count )
  else Result := clBlack;
end;

function  CalculateGradientDirectionsHashValue( const Rect__ : TRect; GradientDirectionsWidth__ : Integer; GradientDirections__ : PPixels ) : TColor;
// for convenience, the result is an rgb color, so it can OR'ed with an existing color where the 4 low bits in each rgb component have been reserved for the hash value
var Width, Height : Integer;

  function  HashValue( Rect__ : TRect ) : Integer;
  const
    DIRECTION_MAP : array[ 0 .. 15 ] of Byte = // mapping 16 compas directions to east/north/west/south
      ( 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 0, 0 );
  var
    Index, Col, Row : Integer;
    Pixel : PPixel;
    HistogramBins : array[ 0 .. 3 ] of Integer;
  begin
    Result := 0;
    with Rect__ do
      if ( Left < Right ) and ( Top < Bottom ) then begin
         ZeroMemory( Addr( HistogramBins ) , Sizeof( HistogramBins ) );
         // count number of pixels with east/north/west/south gradient directions
         for Row := Top to Pred( Bottom ) do begin
             Pixel := PPixel( GradientDirections__ );
             Inc( Pixel, Left + ( Row * GradientDirectionsWidth__ ) );
             for Col := Left to Pred( Right ) do begin
                 Inc( HistogramBins[ DIRECTION_MAP[ Pixel^ div 16 ] ] );
                 Inc( Pixel );
                 end;
             end;
         // return a 0..15 value depending on the shape of the histogram;
         // the bit for a bin is '1' if the bin has more members than the next,
         // otherwise the bit is '0';
         for Index := Low( HistogramBins ) to High( HistogramBins ) do
             if HistogramBins[ Index ] > HistogramBins[ Succ( Index ) mod ( High( HistogramBins ) + 1 ) ] then
                Inc( Result, 1 shl Index );
         end
  end;

begin
  with Rect__ do begin
    Width  := RectWidth ( Rect__ );
    Height := RectHeight( Rect__ );
    Result := ( HashValue( Rect( Left + 2              , Top + 2               , Left + ( Width div 2 ), Top + ( Height div 2 ) ) ) shl  0 ) + // top-left  quadrant
              ( HashValue( Rect( Left + ( Width div 2 ), Top + 2               , Right - 2             , Top + ( Height div 2 ) ) ) shl  8 ) + // top-right quadrant
              ( HashValue( Rect( Left + 2              , Top + ( Height div 2 ), Right - 2             , Bottom - 2             ) ) shl 16 );  // bottom-half rectangle
    end;
end;

function  ScaleRect( Width__, Height__, OriginalWidth__, OriginalHeight__ : Integer; const Rect__ : TRect )  : TRect;
begin
  with Result do begin
    Left   := ( Rect__.Left   * Width__  ) div OriginalWidth__;
    Top    := ( Rect__.Top    * Height__ ) div OriginalHeight__;
    Right  := ( Rect__.Right  * Width__  ) div OriginalWidth__;
    Bottom := ( Rect__.Bottom * Height__ ) div OriginalHeight__;
    end;
end;

function  ColorToText( Color : TColor ) : String;
var RGB :TRGB;
begin
  RGB := ColorToRGB( Color );
  with RGB do Result := '[' + IntToStr( b ) + ',' + IntToStr( g ) + ',' + IntToStr( r ) + ']';
end;

function  CreatePixels( Width__, Height__ : Integer; var Pixels__ : PPixels ) : Boolean;
begin
  Pixels__                            := nil;
  try    GetMem( Pixels__, Width__ * Height__ * SizeOf( Pixels__^[ 0 ] ) );
  except on E:Exception do begin
         if Assigned( Pixels__ ) then FreeMem( Pixels__ );
         Pixels__                     := nil;
         end;
  end;
  Result                              := Assigned( Pixels__ );
end;

function  BitmapToGrayscalePixels( Bitmap__ : TBitmap; Pixels__ : PPixels; ColorChannelMask__ : Byte ) : Boolean;
var X, Y : Integer; P : PRGB; Pixel : PPixel;
begin // preconditions: 'Bitmap__' has pixelformat 24-bit rgb;
      //                'Pixels__' has room for all the pixels in the bitmap;
      //                'ColorChannelMask__' = 0 means unmasked, i.e., the mask is really High( ColorChannelMask__ )
  with Bitmap__ do begin
    if ColorChannelMask__ = 0 then // for convenience: '0' means unmasked
       ColorChannelMask__ := High( ColorChannelMask__ );
    Result :=  ( PixelFormat = pf24Bit ); // and ( Width <= MAX_IMAGE_WIDTH + 2 ) and ( Height <= MAX_IMAGE_HEIGHT + 2 );
    if Result then begin
       Pixel := PPixel( Pixels__ );
       for Y:=0 to Pred(Height) do begin
           P:=ScanLine[Y];
           for X:=0 to Pred(Width) do begin
               //Pixel^ := ( P^.r + P^.g +  P^.b ) div 3; // average
               Pixel^ := Round( ( 0.2126 * ( P^.b and ColorChannelMask__ ) ) +
                                ( 0.7152 * ( P^.g and ColorChannelMask__ ) ) +
                                ( 0.0722 * ( P^.r and ColorChannelMask__ ) ) ); // luminance Rec. 709 RGB coefficients: 0.2126, 0.7152, and 0.0722; in this program, RGB is really BGR, hence, the discrepancy between the formula and the code;
               Inc(P);
               Inc( Pixel );
               end;
           end;
       end;
    end;
end;

function  CalculateIntegralImage( Width__, Height__ : Integer; Input__, Output__ : PPixels ) : Integer;
var Col, Row : Integer; Input, Output : PPixel;
begin
  Input             := PPixel( Input__ );
  Output            := PPixel( Output__ );
  Move( Input^, Output^, Width__ * Height__ * SizeOf( Output__^[ 0 ] ) ); // copy pixels to integral mage
  for Col           := 1 to Pred( Width__ ) do // calculate top row
      Inc( Output__^[ Col ], Output__^[ Pred( Col ) ] );
  for Row           := 1 to Pred( Height__ ) do // calculate left column
      Inc( Output__^[ 0 + ( Row * Width__ ) ], Output__^[ 0 + ( Pred( Row ) * Width__ ) ] );
  for Row           := 1 to Pred( Height__ ) do // calculate remaining rows and columns
      for Col       := 1 to Pred( Width__ )  do
          Inc( Output__^[       Col   + (       Row   * Width__ ) ],
               Output__^[ Pred( Col ) + (       Row   * Width__ ) ] +
               Output__^[       Col   + ( Pred( Row ) * Width__ ) ] -
               Output__^[ Pred( Col ) + ( Pred( Row ) * Width__ ) ] );
  if   ( Width__ > 0 ) and ( Height__ > 0 ) then
       Result := Output__^[ Pred( Width__ ) + ( Pred( Height__ ) * Width__ ) ]
  else Result := 0;
end;

function  ChangeBlackPixels( Bitmap__ : TBitmap; BlackLimit__ : Integer ) : Integer;
var BlackLimitTimes3, Col, Row : Integer; Pixel : PRGB; RGBColor : TRGB;
begin // changes near-black pixels to non-black pixels;
      // black may have a special meaning as mask color or background color in
      // the Sokoban application; normally, it doesn't make a visual difference
      // when black pixels are made a little brighter;
  Result := 0;
  // the criteria for being a near-black pixel is
  // ( ( r + g + b ) div 3 ) <= black limit;
  // impose an upper limit on the black limit to ensure that it never causes an
  // overflow when the black limit is added to the near-black pixels below;
  BlackLimit__ := Min( BlackLimit__, ( High( RGBColor.r ) div 3 ) - 1 );
  BlackLimitTimes3 := 3 * BlackLimit__;
  for Row   := 0 to Pred(Bitmap__.Height ) do begin
      Pixel := PRGB( Bitmap__.ScanLine[ Row ] );
      for   Col := 0 to Pred(  Bitmap__.Width ) do begin
            RGBColor := Pixel^;
            if   ( RGBColor.r + RGBColor.g + RGBColor.b ) <= BlackLimitTimes3 then begin
                 Inc( RGBColor.r, BlackLimit__ );
                 Inc( RGBColor.g, BlackLimit__ );
                 Inc( RGBColor.b, BlackLimit__ );
                 Pixel^  := RGBColor;
                 Inc( Result );
                 end;
            Inc( Pixel );
            end;
      end;
end;

function  GetIntegralImage( Left__, Top__, Right__, Bottom__, IntegralImageWidth__ : Integer; IntegralImage__ : PPixels ) : Integer;
var Index : Integer;
begin // the rectangle parameters adhere to the convention for image rectangles
      // where 'left' and 'top' are inclusive, and 'right' and 'bottom' are
      // exclusive;
      // preconditions: 'right' > 0 and 'bottom' > 0;
  Dec( Left__ ); Dec( Top__ ); Dec( Right__ ); Dec( Bottom__ );
  Index       := Right__  + ( Bottom__  * IntegralImageWidth__  ); // bottom-right corner, inclusive
  Result      := IntegralImage__^[ Index ];
  if Left__   >= 0 then begin
     Dec(        Result, IntegralImage__^[ Left__  + ( Bottom__ * IntegralImageWidth__ ) ] );
     if Top__ >= 0 then
        Inc(     Result, IntegralImage__^[ Left__  + ( Top__    * IntegralImageWidth__ ) ] );
     end;
  if Top__    >= 0 then
     Dec(        Result, IntegralImage__^[ Right__ + ( Top__    * IntegralImageWidth__ ) ] );
end;

function  GetColorIndex( Color__ : TColor; var Colors__ : TCaptureColors) : Integer;
begin
  Result := Pred( Colors__.Count );
  while ( Result >= 0 ) and ( Color__ <> Colors__.Colors^[ Result ].Color ) do
        Dec( Result );
end;

function  GetColorIndexRGB( Color__ : TColor; var Colors__ : TCaptureColors) : Integer;
begin // finds index of the color in table, ignoring alpha channel (which is used for color grouping in this application)
  Color__ := Color__ and RGB_MASK;
  Result := Pred( Colors__.Count );
  while ( Result >= 0 ) and ( Color__ <> ( Colors__.Colors^[ Result ].Color and RGB_MASK ) ) do
        Dec( Result );
end;

procedure InitializeColors( var Colors__ : TCaptureColors );
begin
  ZeroMemory( Addr( Colors__ ), SizeOf( Colors__ ) );
  Colors__.RGBComponentMask := ( not 7 ); // drops the last 3 bits in each rgb color component, thereby reducing the number of colors in the palette to a manageable size
end;

procedure FinalizeColors( var Colors__ : TCaptureColors );
begin
  FreeMem( Colors__.Colors );
  InitializeColors( Colors__ );
end;

function  SetColorTableCapacity( Capacity__ : Integer; var Colors__ : TCaptureColors ) : Boolean;
begin
  with Colors__ do
    try
      ReallocMem( Colors, Capacity__ * SizeOf( Colors^[ 0 ] ) );
      Capacity := Capacity__;
      Count := Min( Count, Capacity__ );
      Result := True;
    except on E:Exception do Result := False;
    end;
end;

function  TruncatedColor( Color__ : TColor; RGBComponentMask__ : Integer ) : TColor;
var RGB : TRGB;
begin // the rgb component mask typically drops the 3 low bits in each rgb color component
  RGB    := ColorToRGB( Color__ );
  RGB.r  := RGB.r and RGBComponentMask__;
  RGB.g  := RGB.g and RGBComponentMask__;
  RGB.b  := RGB.b and RGBComponentMask__;
  Result := RGBToColor( RGB );
end;

function  AddColorToTable( Color__ : TColor; var Colors__ :TCaptureColors ) : Integer;
var Count : Integer;
begin // adds a new color to the table, or updates the count for an existing
      // color; returns the current table index of the color, or -1 if the
      // color couldn't be added to the table;
  if   ( Colors__.Count < Colors__.Capacity )
       or
       // expand the table so there is room for a sentinel
       ( ( Colors__.Capacity < High( Colors__.Capacity ) )
         and
         SetColorTableCapacity( Colors__.Capacity +
                                Min( High( Colors__.Capacity ) - Colors__.Capacity,
                                     25 * ( Max( 1, Colors__.Capacity div 100 ) ) ), // + 25%
                                Colors__ ) ) then begin
       Colors__.Colors^[ Colors__.Count ].Color := Color__; // sentinel
       Colors__.Colors^[ Colors__.Count ].Count := 0;
       Result := 0; // search from the first color in the table
       while ( Result  <  Colors__.Count ) and
             ( Color__ <> Colors__.Colors^[ Result ].Color ) do
             Inc( Result );
       Inc(  Colors__.Colors^[ Result ].Count ); // found
       if    Result = Colors__.Count then
             Inc( Colors__.Count ); // new color
       // keep colors sorted in descending order on frequency
       Count := Colors__.Colors^[ Result ].Count;
       while ( Result > 0 ) and
             ( Count  >
               Colors__.Colors^[ Pred( Result )  ].Count ) do begin
             Colors__.Colors^[ Result ].Color := Colors__.Colors^[ Pred( Result ) ].Color;
             Colors__.Colors^[ Result ].Count := Colors__.Colors^[ Pred( Result ) ].Count;
             Dec( Result );
             Colors__.Colors^[ Result ].Color := Color__;
             Colors__.Colors^[ Result ].Count := Count;
             end;
       end
  else Result := -1; // color not added to the table
end;

function  BitmapHistogram( Bitmap__       : TBitmap;
                           const Rect__   : TRect;
                           var   Colors__ : TCaptureColors ) : Boolean;
// preconditions: the Bitmap is a 24-bits-per-pixel bitmap;
//                the color table has been initialized;
var
  X, Y : Integer; P : PRGB;
begin
  Result := True;
  for Y := Rect__.Top to Pred( Rect__.Bottom ) do begin // for each row
      P := Bitmap__.Scanline[ Y ];
      Inc( P, Rect__.Left );
      for X := Rect__.Left to Pred( Rect__.Right ) do begin // for each column
          Result := Result and
                    ( AddColorToTable( TruncatedColor( RGBToColor( P^ ), Colors__.RGBComponentMask ), Colors__ ) >= 0 );
          Inc( P ); // next pixel in row
          end;
      end;
end;

procedure ClearBoard( var Board__ : TCaptureBoard );
var Col, Row : Integer;
begin
  with Board__ do begin
    if ( Width <> 0 ) or ( Height <> 0 ) then
       ZeroMemory( Addr( Board__ ), SizeOf( Board__ ) );

    PlayerPos.X := -1; PlayerPos.Y := -1;

    if Squares[ 0, 0 ] <> [ bsiWall ] then begin // 'True': initialize the board by adding a wall-filled border
       for Col := 0 to CAPTURE_MAX_BOARD_WIDTH + 1 do begin
           Squares[ Col                        , 0                            ] := [ bsiWall ];
           Squares[ Col                        , CAPTURE_MAX_BOARD_HEIGHT + 1 ] := [ bsiWall ];
           end;
       for Row := 0 to CAPTURE_MAX_BOARD_HEIGHT + 1 do begin
           Squares[ 0                          , Row                          ] := [ bsiWall ];
           Squares[ CAPTURE_MAX_BOARD_WIDTH + 1, Row                          ] := [ bsiWall ];
           end;
       end;
     end;
end;

function  BoardSquareToChar( Value__ : TCaptureBoardSquareItemSet; UndefinedBoardSquare__ : Char ) : Char;
begin
  if                        bsiWall    in Value__ then Result := WALL_CH
  else if                   bsiGoal    in Value__ then
            if              bsiPlayer  in Value__ then Result := PLAYER_GOAL_CH
            else if         bsiBox     in Value__ then Result := BOX_GOAL_CH
                 else       Result     := GOAL_CH
       else if              bsiPlayer  in Value__ then Result := PLAYER_CH
            else if         bsiBox     in Value__ then Result := BOX_CH
                 else  if   bsiFloor   in Value__ then Result := FLOOR_CH
                       else                            Result := UndefinedBoardSquare__;
end;

function  CharToBoardSquare( Value__ : Char ) : TCaptureBoardSquareItemSet;
begin
  Result := [];
  case Value__ of
    WALL_CH        : Result := [ bsiWall ];
    FLOOR_CH       : Result := [ bsiFloor ];
    PLAYER_CH      : Result := [ bsiFloor, bsiPlayer ];
    BOX_CH         : Result := [ bsiFloor, bsiBox ];
    GOAL_CH        : Result := [ bsiFloor, bsiGoal ];
    PLAYER_GOAL_CH : Result := [ bsiFloor, bsiGoal, bsiPlayer ];
    BOX_GOAL_CH    : Result := [ bsiFloor, bsiGoal, bsiBox ];
    else             Result := [];
  end;
end;

function  BoardToText( const Board__ : TCaptureBoard; UndefinedBoardSquare__ : Char ) : String;
var Col, Row : Integer;
begin
  with Board__ do begin
    try    SetLength( Result, Width * Height );
           for Row := 1 to Height do
               for Col := 1 to Width do
                   Result[ Col + ( Pred( Row ) * Width ) ] := BoardSquareToChar( Squares[ Col, Row ], UndefinedBoardSquare__ );
    except on E:Exception do begin
           Error( E.Message, '' );
           Result := '';
           end;
    end;
    end;
end;

function  PaddedTextToLines( const Text__ : String; PaddedLineWidth__ : Integer ) : String;
var Index : Integer; Line : String;
begin // input : text with fixed    line width and without line separators
      // output: text with variable line width and with    line separators
  Result := '';
  try    for Index := 0 to Pred( Length( Text__ ) div PaddedLineWidth__ ) do begin
             Line := TrimRight( Copy( Text__, Succ( Index * PaddedLineWidth__ ), PaddedLineWidth__ ) );
             if Result <> '' then Result := Result + NL;
             Result := Result + Line;
             end;
  except on E:Exception do begin
         Error( E.Message, '' );
         Result := '';
         end;
  end;
end;

function  BoardToTextLines( const Board__ : TCaptureBoard; UndefinedBoardSquare__ : Char ) : String;
begin
  Result := PaddedTextToLines( BoardToText( Board__, UndefinedBoardSquare__ ), Board__.Width );
end;

function  TextToBoard( const Text__ : String; var Board__ : TCaptureBoard ) : Boolean;
var Col, Row : Integer;
begin
  with Board__ do begin
    Result := Width * Height = Length( Text__ );
    if Result then begin
       PlayerPos.X := -1; PlayerPos.Y := -1;
       for Col := 1 to Width do
           for Row := 1 to Height do begin
               Squares[ Col, Row ] := CharToBoardSquare( Text__[ Col + ( Pred( Row ) * Width ) ] );
               if bsiPlayer in Squares[ Col, Row ] then begin
                  PlayerPos.X := Pred( Col );
                  PlayerPos.Y := Pred( Row );
                  end;
               end;
       end;
    end;
end;

procedure TCaptureForm.FormCreate(Sender: TObject);
begin
  fInitialized := False;
  DefaultSkinFileNameText := SkinText;
  PuzzleInitialDirectory := ''; SkinInitialDirectory := '';
  MatchingSkinFileName := ''; EditorSkinFileName := '';
  ZeroMemory( Addr( Editor ), SizeOf( Editor ) );
  Editor.GridColor := DEFAULT_SELECTION_CURSOR_PEN_COLOR;
  Editor.GridShadowColor := DEFAULT_SELECTION_CURSOR_SHADOW_COLOR;
  Settings.CheckForDuplicatesWhenAppendingNewLevelsToCollections := True;
  Settings.FindBoardTimeLimitMS := 1000;
  Settings.MatchThresholdPct := DEFAULT_SKIN_MATCHING_THRESHOLD_PERCENT;
  if Assigned( ToolsForm ) then begin
     ToolsForm.CaptureSettingsMenuItemSnapToNearbyEdges.Checked := False;
     ToolsForm.CaptureSettingsMenuItemKeepAuthorAndDesignerNames.Checked := False;
     end;
  // full initialization is deferred until the "Tools" form is opened
end;

procedure TCaptureForm.FormDestroy(Sender: TObject);
begin
  Clear;
  FreeMem( HorzLines ); FreeMem( VertLines );
  FreeMem( IntegralImage ); FreeMem( IntegralImage2 ); FreeMem( GradientDirections );
  FreeMem( SkinCaptureInformation );
  FinalizeColors( Colors );
end;

procedure TCaptureForm.Initialize;
var Index, CaptionWidth, MaxCaptionWidth : Integer;
begin
  if not Initialized then begin
     fInitialized := True;
     try
            // the Delphi documentation states:
            // "if 'Flat' is set to True, TToolBar requires version 4.70 or
            // later of COMCTL32.DLL at both design time and runtime."

            // whether 'try...except helps at all hasn't been tested
            // but it's worth a try
            if Assigned( ToolsForm ) then begin
               ToolsForm.CaptureToolBarTop.Flat:=True;
               //ToolsForm.CaptureEditToolBarLeft.Flat:=True;
               end;

     except on E:Exception do;
     end;

     ToolsForm.CaptureScrollBox.ControlStyle      := ToolsForm.CaptureScrollBox.ControlStyle + [csOpaque]; // avoids flicker

     ToolsForm.PuzzleInformationPanel.Height      := ToolsForm.BottomPanel.ClientHeight - ToolsForm.PuzzleInformationPanel.Top -2;
     ToolsForm.SkinInformationPanel  .Height      := ToolsForm.PuzzleInformationPanel.Height;

     ToolsForm.CaptureImagePanel     .Top         := ToolsForm.CaptureToolBarTop.Top + ToolsForm.CaptureToolBarTop.Height;
     ToolsForm.CaptureBoardPanel     .Top         := ToolsForm.CaptureImagePanel.Top;
     ToolsForm.CaptureGridPanel      .Top         := ToolsForm.CaptureImagePanel.Top;
     ToolsForm.CaptureSquaresPanel   .Top         := ToolsForm.CaptureImagePanel.Top;
     ToolsForm.CapturePuzzlePanel    .Top         := ToolsForm.CaptureImagePanel.Top;
     ToolsForm.CaptureSkinPanel      .Top         := ToolsForm.CaptureImagePanel.Top;
     ToolsForm.SkinInformationPanel  .Top         := ToolsForm.PuzzleInformationPanel.Top;
     ToolsForm.CaptureEditLevelBitBtn.Top         := ToolsForm.CaptureEditToolBarLeft.Top + ToolsForm.CaptureEditToolButtonErase.Top + ToolsForm.CaptureEditToolButtonErase.Height + 8 + 4;
     ToolsForm.SkinBitBtn3           .Top         := ToolsForm.CaptureEditLevelBitBtn.Top+ToolsForm.CaptureEditLevelBitBtn.Height;
     ToolsForm.CaptureSettingsMenuItemSkin.Hint   := Format( TEXT_FORMAT_LOAD_EDITOR_SKIN_HINT, [ TEXT_DEFAULT_EDITOR_DEFAULT_SKIN_FILE_NAME ] );
     ToolsForm.PreviousStepButton2 .BringToFront;
     ToolsForm.NextStepButton2     .BringToFront;
     HorzLines := nil; VertLines := nil; IntegralImage := nil; IntegralImage2 := nil; GradientDirections := nil;
     fSkinCaptureInformation := nil;
     BackgroundBitmap := nil;
     OriginalBitmap := nil;
     UndoBitmap := nil;
     SingleRowSkinBitmap := nil;
     SkinBitmap := nil;
     Editor.InputFileName := '';
     Editor.PuzzleFileName := '';
     Editor.SkinFileName := '';
     ToolsForm.PuzzleAuthorEdit.Text := '';
     ToolsForm.SkinDesignerEdit.Text := '';
     Editor.EditorSkinBitmap := nil;
     if     BitmapCreate( BackgroundBitmap, 0, 0 ) and
            BitmapCreate( OriginalBitmap  , 0, 0 ) then begin
            end
     else   fInitialized := False; //raise Exception.Create( 'Initialization failed.' ); // panic exit
     try    ToolsForm.CaptureImage1.Picture.Bitmap := TBitmap.Create;
            fDefaultMatchingSkinFileName := StrWithTrailingPathDelimiter( MainForm.ApplicationDataPath ) + TEXT_CAPTURE_HISTORY_FILE_NAME + BMP_FILE_EXT;
     except on E:Exception do fInitialized := Error( E.Message, '' );
     end;
     InitializeColors( Colors );
     Clear;
     FormResize( Self );
     SetColorLevel( DEFAULT_COLOR_LEVELS );
     ToolsForm.CaptureSettingsMenuItemUseAutomaticCompletion.Checked := True;
     ToolsForm.CaptureSettingsColorQuantizationIgnoreSmallColorDifferences.Checked := True;
     ToolsForm.CaptureViewMenuItemShowColorQuantization.Checked := True;
     ToolsForm.CaptureViewMenuItemShowImageAnalysisResults.Checked := False;
     ToolsForm.CaptureSettingsMenuItemStraightLines.Checked := True;
     ToolsForm.CaptureSettingsMenuItemCommonSkinFormat.Checked := True;
     ToolsForm.CaptureSettingsMenuItemObjectsOnFloorsAndGoals.Checked := True;
     ToolsForm.CaptureSettingsMenuItemObjectsOnABlackBackground.Checked := False;
     ToolsForm.CaptureSettingsMenuItemObjectsOnABlackBackground.Tag := Integer( clBlack ); // fill color; always black
     Self.Caption := TEXT_CAPTURE_APPLICATION_TITLE;
     ToolsForm.CaptureFileMenuItemSave.Caption := TEXT_SAVE_PUZZLE;
     ToolsForm.CaptureFileMenuItemSaveAs.Caption := TEXT_SAVE_PUZZLE_AS;
     ToolsForm.CaptureFileMenuItemSave.Hint := ToolsForm.CaptureFileMenuItemSave.Caption;
     ToolsForm.CaptureEditMenuItemEditLevel.Hint := ToolsForm.CaptureEditLevelBitBtn.Hint;
     ToolsForm.CaptureFileMenuItemSaveAs.Hint := ToolsForm.SavePuzzleAsBitBtn.Hint;
     ToolsForm.CaptureToolButtonSaveAs.Hint := ToolsForm.CaptureFileMenuItemSaveAs.Caption;
     ToolsForm.CaptureEditMenuItemCopy.Hint := HintCopyBitmapToClipboardText;
     ToolsForm.CaptureToolButtonCopy.Hint := ToolsForm.CaptureEditMenuItemCopy.Hint;
     ToolsForm.PreviousStepButton2.Top := ToolsForm.CaptureImagePanel.Top + 2;
     ToolsForm.PreviousStepButton2.Left := Succ( ToolsForm.CaptureImagePanel.Left );
     ToolsForm.PreviousStepButton2.Glyph := ToolsForm.PreviousStepButton.Glyph;
     ToolsForm.PreviousStepButton2.Hint := ToolsForm.PreviousStepButton.Hint;
     ToolsForm.NextStepButton2.Top := ToolsForm.PreviousStepButton2.Top;
     ToolsForm.NextStepButton2.Glyph := ToolsForm.NextStepButton.Glyph;
     ToolsForm.NextStepButton2.Hint := ToolsForm.NextStepButton.Hint;
     ToolsForm.CaptureSelectAllBitBtn.Hint := ToolsForm.CaptureEditMenuItemSelectAll.Hint;

     ToolsForm.PreviousStepButton  .Height      := Max( ToolsForm.PreviousStepButton.Height , ToolsForm.PreviousStepButton.Glyph.Height  + 6 );
     ToolsForm.NextStepButton      .Height      := ToolsForm.PreviousStepButton.Height;
     ToolsForm.NextStepButton      .Top         := Max( ToolsForm.PreviousStepButton.Top + ToolsForm.PreviousStepButton.Height, ToolsForm.CaptureStateMemo.Top + ToolsForm.CaptureStateMemo.Height - ToolsForm.NextStepButton.Height );
     ToolsForm.PreviousStepButton2 .Width       := Max( ToolsForm.PreviousStepButton2.Width , ToolsForm.PreviousStepButton2.Glyph.Width  + 8 );
     ToolsForm.PreviousStepButton2 .Height      := Max( ToolsForm.PreviousStepButton2.Height, ToolsForm.PreviousStepButton2.Glyph.Height + 6 );
     ToolsForm.NextStepButton2     .Width       := ToolsForm.PreviousStepButton2.Width;
     ToolsForm.NextStepButton2     .Height      := ToolsForm.PreviousStepButton2.Height;
     ToolsForm.NextStepButton2     .Top         := ToolsForm.PreviousStepButton2.Top;
     ToolsForm.NextStepButton2     .Left        := ToolsForm.CaptureImagePanel.Left + ToolsForm.CaptureImagePanel.Width - ToolsForm.NextStepButton2.Width - 3;

     MaxCaptionWidth      := ToolsForm.NextStepButton2.Left - ( ToolsForm.PreviousStepButton2.Left + ToolsForm.PreviousStepButton2.Width ) - 8;
//   MaxCaptionWidth      := 3* ( ( ToolsForm.NextStepButton2.Left - ( ToolsForm.PreviousStepButton2.Left + ToolsForm.PreviousStepButton2.Width ) ) div 4 );
     repeat CaptionWidth  := Max(                     ToolsForm.ImageLabel  .Canvas.TextWidth( ToolsForm.ImageLabel  .Caption ),
                                  Max(                ToolsForm.BoardLabel  .Canvas.TextWidth( ToolsForm.BoardLabel  .Caption ),
                                       Max(           ToolsForm.GridLabel   .Canvas.TextWidth( ToolsForm.GridLabel   .Caption ),
                                            Max(      ToolsForm.SquaresLabel.Canvas.TextWidth( ToolsForm.SquaresLabel.Caption ),
                                                 Max( ToolsForm.PuzzleLabel .Canvas.TextWidth( ToolsForm.PuzzleLabel .Caption ),
                                                      ToolsForm.SkinLabel   .Canvas.TextWidth( ToolsForm.SkinLabel   .Caption ) ) ) ) ) );
            if CaptionWidth > MaxCaptionWidth then begin
               ToolsForm.ImageLabel  .Font.Height := Abs( ToolsForm.ImageLabel  .Font.Height ) - 1;
               ToolsForm.BoardLabel  .Font.Height := Abs( ToolsForm.BoardLabel  .Font.Height ) - 1;
               ToolsForm.GridLabel   .Font.Height := Abs( ToolsForm.GridLabel   .Font.Height ) - 1;
               ToolsForm.SquaresLabel.Font.Height := Abs( ToolsForm.SquaresLabel.Font.Height ) - 1;
               ToolsForm.PuzzleLabel .Font.Height := Abs( ToolsForm.PuzzleLabel .Font.Height ) - 1;
               ToolsForm.SkinLabel   .Font.Height := Abs( ToolsForm.SkinLabel   .Font.Height ) - 1;
               end;
     until  ( CaptionWidth <= MaxCaptionWidth ) or ( Abs( ToolsForm.ImageLabel.Font.Height ) <= 8 );

     ToolsForm.ImageLabel  .Align                 := alNone; // leave the automatic alignment mode and adjust the vertical position so the texts are centered vertically between the arrow buttons
     ToolsForm.BoardLabel  .Align                 := alNone;
     ToolsForm.GridLabel   .Align                 := alNone;
     ToolsForm.SquaresLabel.Align                 := alNone;
     ToolsForm.PuzzleLabel .Align                 := alNone;
     ToolsForm.SkinLabel   .Align                 := alNone;
     ToolsForm.ImageLabel  .Top                   := ToolsForm.ImageLabel  .Top + ( Max( 0, ( ToolsForm.PreviousStepButton2.Height - ToolsForm.ImageLabel.Height ) div 2 ) );
     ToolsForm.BoardLabel  .Top                   := ToolsForm.ImageLabel  .Top;
     ToolsForm.GridLabel   .Top                   := ToolsForm.ImageLabel  .Top;
     ToolsForm.SquaresLabel.Top                   := ToolsForm.ImageLabel  .Top;
     ToolsForm.PuzzleLabel .Top                   := ToolsForm.ImageLabel  .Top;
     ToolsForm.SkinLabel   .Top                   := ToolsForm.ImageLabel  .Top;

     Timestamp := High( Timestamp );

     if   EditorSkinFileName = '' then
          SettingsMenuItemDefaultSkinClick( nil )
     else SettingsMenuItemSkinClick( nil );

     if Mouse.WheelPresent then begin
        with ToolsForm.CaptureEditToolBarLeft do
          for Index:=0 to Pred(ButtonCount) do with Buttons[Index] do
            Hint:=Hint+HintMouseWheelChangesDrawingToolText;
        ToolsForm.PreviousStepButton .Hint := ToolsForm.PreviousStepButton.Hint + HintMouseWheelChangesCaptureStepText;
        ToolsForm.NextStepButton     .Hint := ToolsForm.NextStepButton    .Hint + HintMouseWheelChangesCaptureStepText;
        ToolsForm.PreviousStepButton2.Hint := ToolsForm.PreviousStepButton.Hint;
        ToolsForm.NextStepButton2    .Hint := ToolsForm.NextStepButton    .Hint;
        end;

     ToolsForm.StatusBar1.Font.Assign(ToolsForm.Font);
     Index := 9;
     while ( ( Index < MAX_IMAGE_WIDTH  )
             or
             ( Index < MAX_IMAGE_HEIGHT ) )
           and
           ( Index < High( Index ) div 10 ) do
           Index := ( Index * 10 ) + 9;
     StatusBarPanel0Widths[ False ] := Max( ToolsForm.Canvas.TextWidth( SPACE + SPACE + Format( FORMAT_IMAGE_POSITION_AND_SELECTION, [ Index, Index, Index, Index ] ) ),
                                            ToolsForm.StatusBar1.Panels[0].Width );
     Index := 9;
     while ( Index < MAX_BOXES ) and ( Index < High( Index ) div 10 ) do Index := ( Index * 10 ) + 9;
     StatusBarPanel0Widths[ True  ] := Max( ToolsForm.Canvas.TextWidth( SPACE + SPACE + Format( TEXT_FORMAT_BOXES_AND_GOALS_AND_PLAYERS, [ Index, Index, 1 ] ) ),
                                            ToolsForm.StatusBar1.Panels[0].Width );
     //PostMessage( Self.Handle, MSG_MAXIMIZE, 0, 0 );
{
     LoadImage( 'C:\Temp\Untitled 25.bmp' );
     ToolsForm.LeftSpinEdit.Value := 164;
     ToolsForm.TopSpinEdit.Value := 60;
     ToolsForm.WidthSpinEdit.Value := 510;
     ToolsForm.HeightSpinEdit.Value := 390;
     NextStepButtonClick ( Self );
     ToolsForm.ColumnsSpinEdit.Value := 17;
     ToolsForm.RowsSpinEdit.Value := 13;
     ToolsForm.CaptureScrollBox.VertScrollBar.Position := 0;
     NextStepButtonClick ( Self );
     FindBoardUsingMatchingSkin( 'C:\users\brian\documents\sokoban\sokoban yasc\skins\common skins\AntiqueDesk3 - Gerry Wiseman.bmp', Settings.MatchThresholdPct, True );
}     
     end;
end;

procedure TCaptureForm.ApplicationOnActivate( Sender : TObject );
begin
  if   Editor.MouseButtonDown then CaptureImage1MouseUp(nil,mbLeft,[],0,0)
  else SetCaptureDrawingToolHint;
  //IgnoreKeyUp:=True; IgnoreMouseUp:=True;
  Editor.MouseButtonDown:=False;
  ShowStatus;
end;

procedure TCaptureForm.ApplicationOnDeactivate(Sender: TObject);
begin
//if not Editor.Selection.Enabled then HideCursor(False);
//IgnoreKeyUp:=True; IgnoreMouseUp:=True;
end;

procedure TCaptureForm.ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if   (Msg.Message = WM_MOUSEWHEEL) then begin
       if Msg.WParam>=0 then CaptureScrollBoxMouseWheelUp  (Self,[],Point(0,0),Handled)
       else                  CaptureScrollBoxMouseWheelDown(Self,[],Point(0,0),Handled);
       end
  else Handled := False;
end;

procedure TCaptureForm.OnMaximize(var Msg: TMessage); //Message MSG_MAXIMIZE;
begin
  WindowState := wsMaximized;
end;

procedure TCaptureForm.ShowHint(Sender: TObject);
begin
  ToolsForm.StatusBar1.Panels[ 1 ].Text := Application.Hint;
end;

function TCaptureForm.BoardSquareValue(Col__,Row__:Integer):TCaptureBoardSquareItemSet;
begin
  if   (Col__>=0) and (Col__<Editor.Board.Width ) and
       (Row__>=0) and (Row__<Editor.Board.Height) then
       Result:=Editor.Board.Squares[Succ(Col__),Succ(Row__)]
  else Result:=[]; //[bsiFloor];
end;

procedure TCaptureForm.SetCaptureDrawingToolHint;
var Col,Row:Integer; Point:TPoint; Value: TCaptureBoardSquareItemSet;
begin // the hint depends on the current editor cursor state
  with Editor do with CursorRect do
    if (Left<Right) and (Top<Bottom) and
       (Right>0) and (Bottom>0) and
       DrawingToolsEnabled then begin
       if        (DrawingTool=dtNone) or
                 (DrawingTool=dtErase) then
                 ToolsForm.CaptureImage1.Hint:=HintDrawingToolText[DrawingTool]
       else if   DrawingTool=dtPlayer then begin
                 Point:=ToolsForm.CaptureImage1.ScreenToClient(Mouse.CursorPos);
                 XYToColRow(Point.X,Point.Y,Col,Row);
                 Value:=BoardSquareValue(Col,Row);
                 if        not (bsiPlayer in Value) then
                           ToolsForm.CaptureImage1.Hint:=HintDrawingToolText[DrawingTool]+HintDrawingToolRightClickEraseText
                 else if   not (bsiGoal in Value) then
                           ToolsForm.CaptureImage1.Hint:=HintDrawingToolText[dtGoal]+HintDrawingToolRightClickEraseText
                      else ToolsForm.CaptureImage1.Hint:=HintDrawingToolEraseGoalText +HintDrawingToolRightClickEraseText;
                 end
            else if   DrawingTool=dtBox then begin
                      Point:=ToolsForm.CaptureImage1.ScreenToClient(Mouse.CursorPos);
                      XYToColRow(Point.X,Point.Y,Col,Row);
                      Value:=BoardSquareValue(Col,Row);
                      if        not (bsiBox in Value) then
                                ToolsForm.CaptureImage1.Hint:=HintDrawingToolText[DrawingTool]+HintDrawingToolRightClickEraseText
                      else if   not (bsiGoal in Value) then
                                ToolsForm.CaptureImage1.Hint:=HintDrawingToolText[dtGoal]+HintDrawingToolRightClickEraseText
                           else ToolsForm.CaptureImage1.Hint:=HintDrawingToolEraseGoalText +HintDrawingToolRightClickEraseText;
                      end
            else if   DrawingTool=dtGoal then begin
                      Point:=ToolsForm.CaptureImage1.ScreenToClient(Mouse.CursorPos);
                      XYToColRow(Point.X,Point.Y,Col,Row);
                      Value:=BoardSquareValue(Col,Row);
                      if        not (bsiGoal in Value) then
                                ToolsForm.CaptureImage1.Hint:=HintDrawingToolText[DrawingTool]+HintDrawingToolRightClickEraseText
                      else if   not (bsiBox in Value) then
                                ToolsForm.CaptureImage1.Hint:=HintDrawingToolText[dtBox] +HintDrawingToolRightClickEraseText
                           else ToolsForm.CaptureImage1.Hint:=HintDrawingToolEraseBoxText+HintDrawingToolRightClickEraseText;
                      end
            else ToolsForm.CaptureImage1.Hint:=HintDrawingToolText[DrawingTool]+HintDrawingToolRightClickEraseText;
       end
    else ToolsForm.CaptureImage1.Hint:='';
end;

procedure TCaptureForm.EditMenuItemDrawingToolClick(Sender: TObject);
var Col,Row:Integer; Point:TPoint;
begin
        if Sender is TToolButton then with Sender as TToolButton do begin
           Editor.DrawingTool:=TDrawingTool(Tag);
           ToolsForm.CaptureEditToolBarLeft.Buttons[Pred(Tag)].Down:=True;
           if   Sender=ToolsForm.CaptureEditToolButtonErase then
                Editor.Cursor:=ctEraser
           else Editor.Cursor:=ctCell;

           ToolsForm.CaptureImage1.Cursor:=Succ(Ord(Editor.DrawingTool));

           Point:=ToolsForm.CaptureImage1.ScreenToClient(Mouse.CursorPos);
           XYToColRow(Point.X,Point.Y,Col,Row);
           if ( Col >= 0 ) and ( Row >= 0 ) then begin
              ShowCursor(Editor.Cursor, ColRowToRect( Col, Row ));
              SetCaptureDrawingToolHint;

              if PtInRect(Rect(0,0,ToolsForm.CaptureImage1.Picture.Bitmap.Width,ToolsForm.CaptureImage1.Picture.Bitmap.Height),Point)
                 then begin
                 ToolsForm.StatusBar1.Panels[ 1 ].Text := GetLongHint(ToolsForm.CaptureImage1.Hint);
                 CaptureImage1MouseMove(Sender,[],Point.X,Point.Y);
                 //CaptureImage1.Cursor:=Succ(Ord(Editor.DrawingTool));
                 //if Screen.Cursor <> CaptureImage1.Cursor then
                 //   Screen.Cursor := CaptureImage1.Cursor;
                 end
              else HideCursor(True);
              end
           else begin
              ToolsForm.CaptureImage1.Cursor := crDefault;
              HideCursor( True );
              end;

           ShowStatus;
           end;
end;

procedure TCaptureForm.FileNew(Sender: TObject);
begin
  { Add code to create a new file }
  if Editor.Modified then CloseCapture;
  if not Editor.Modified then Clear;
end;


function  TCaptureForm.LoadImageFromFile( const FileName__ : String; var Bitmap__ : TBitmap ) : Boolean;
var Pict:TPicture;
begin
  Result := False; Bitmap__ := nil; Pict := nil;
  if ( FileName__ <> '' ) and FileExists( FileName__ ) then
     try
       try     Pict := TPicture.Create;
               try    Pict.LoadFromFile( FileName__ );
                      Result := True;
               except on E:Exception do
                         Result := IsPNGFile( FileName__ ) and
                                   MainForm.PNGImageLoader.LoadFromFile( FileName__, Pict);
               end;
               if   Result and
                    BitmapCreate ( Bitmap__, Pict.Width, Pict.Height ) then
                    BitMap__.Canvas.Draw( 0, 0, Pict.Graphic)
               else Result:=False;
       except on E:Exception do begin
                 Result:=False; Bitmap__.Free; Bitmap__ := nil;
                 end;
       end;
     finally Pict.Free;
             if not Result then begin Bitmap__.Free; Bitmap__ := nil; end;
     end;
end;

function  TCaptureForm.LoadImage( FileName : String ) : Boolean;
// if filename = '', then the function loads an image from the clipboard, if any
var ColCount, RowCount, Index, W, H : Integer;
    R, R100: TRect;
    AnalyseImageResultSet : TAnalyseImageResultSet;
    B : TBitmap;

  function CaptureScreen( Bitmap__ : TBitmap ) : Boolean;
  const CAPTUREBLT  =  $40000000;
  var   DC          :  HDC;
  begin
    Result          := False;
    DC              := 0;
    try
      try  DC       := GetDC( GetDesktopWindow );
           Result   := Assigned( Bitmap__ ) and ( DC <> 0 );
           if Result then begin
              Bitmap__.Width  := GetDeviceCaps (DC, HORZRES) ;
              Bitmap__.Height := GetDeviceCaps (DC, VERTRES) ;
              Result          := BitBlt( Bitmap__.Canvas.Handle, 0, 0, Bitmap__.Width, Bitmap__.Height, DC, 0, 0, SRCCOPY or CAPTUREBLT );
              end;
      finally if DC <> 0 then ReleaseDC (GetDesktopWindow, DC ) ;
              if not Result then
                 Msg( TEXT_SCREEN_CAPTURE_FAILED, Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+Trim(ToolsForm.PageControl1.ActivePage.Caption), MB_ICONINFORMATION + MB_OK );
      end;
    except on E:Exception do Result := Error( E.Message, '' );
    end;
  end;

  function LoadFromFile(const FileName:String; Pict:TPicture):Boolean;
  begin
    try Pict.LoadFromFile(FileName);
        Result:=True;
    except on E:Exception do
           if   IsPNGFile(FileName) and
                MainForm.PNGImageLoader.LoadFromFile(FileName,Pict) then
                Result:=True
           else raise;
    end;
  end;

begin // 'LoadImage'
  Result := False;
  if ( FileName = FILE_NAME_EXT_DELIMITER ) and // 'True': load the upscaled view of the image as a new image
     ( Editor.Zoom > 100 ) and
     Assigned( BackgroundBitMap ) and
     ( ToolsForm.CaptureImage1.Picture.Graphic is TBitmap ) then
     Editor.Zoom := 100; // '100': don't let "Clear()" resize the image on the screen
  Clear;
  try
    try
        // get the old bitmaps off the screen
        ToolsForm.CaptureImage1.Picture.Bitmap.Width       := 0;
        ToolsForm.CaptureImage1.Picture.Bitmap.Height      := 0;
        ToolsForm.CaptureImage2.Picture.Bitmap.Width       := 0;
        ToolsForm.CaptureImage2.Picture.Bitmap.Height      := 0;
        ToolsForm.CaptureScrollBox.Hide;
        ToolsForm.CaptureScrollBox.HorzScrollBar.Position  := 0;
        ToolsForm.CaptureScrollBox.VertScrollBar.Position  := 0;
        ToolsForm.CaptureScrollBox.Show;

        if        FileName = '' then begin
                  if Clipboard.HasFormat( CF_Bitmap ) then begin
                     ToolsForm.CaptureImage1.Picture.Assign( Clipboard );
                     Result := True;
                     FileName := TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER;
                     end;
                  end
        else if   FileName = FILE_NAME_PATH_DELIMITER then begin
                  Result := CaptureScreen( ToolsForm.CaptureImage1.Picture.Bitmap );
                  if Result then FileName := TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER;
                  end
        else if   ( FileName = FILE_NAME_EXT_DELIMITER ) and // 'True': load the upscaled view of the image as a new image
                  Assigned( BackgroundBitMap ) and
                  ( ToolsForm.CaptureImage1.Picture.Graphic is TBitmap ) then begin
                  with ToolsForm.CaptureImage1.Picture.Bitmap do with Canvas do begin
                    Width  := Max( 0, BackgroundBitMap.Width  - 2 );
                    Height := Max( 0, BackgroundBitMap.Height - 2 );
                    R      := Rect( 0, 0, Width, Height );
                    CopyRect( R, BackgroundBitMap.Canvas, R );
                    end;
                  Result   := True;
                  if Result then FileName := TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER;
                  end
             else Result:= LoadFromFile( FileName, ToolsForm.CaptureImage1.Picture );

        if Result then begin
           if (not ( ToolsForm.CaptureImage1.Picture.Graphic is TBitmap )) or
              (ToolsForm.CaptureImage1.Picture.Bitmap.PixelFormat<>pf24Bit) then begin
              B := TBitmap.Create;
              try
                B.PixelFormat := pf24Bit;
                B.Width := ToolsForm.CaptureImage1.Picture.Graphic.Width;
                B.Height := ToolsForm.CaptureImage1.Picture.Graphic.Height;
                B.Canvas.Draw( 0, 0, ToolsForm.CaptureImage1.Picture.Graphic );
                ToolsForm.CaptureImage1.Picture.Assign( B );
              finally B.Free;
              end;
              end;

           if ( Editor.Step = csImage ) and Editor.HasSelection then // this cannot happen, i.e., inserting the clipboard contents inside a selected image region is in effect disabled
              // paste the image from the clipboard into the selected area only
              with ToolsForm.CaptureImage1.Picture.Bitmap do begin
                SaveUndoBitmap;
                R100 := NormalizedRect( Editor.CursorRect );
                R := Rect( 0, 0, Min( Width, RectWidth( R100 ) ), Min( Height, RectHeight( R100 ) ) );
                OriginalBitmap.Canvas.CopyMode := cmSrcCopy;
                OriginalBitmap.Canvas.CopyRect( Rect( R100.Left, R100.Top, R100.Left + RectWidth( R ), R100.Top + RectHeight( R ) ), Canvas, R );
                ChangeBlackPixels( OriginalBitmap, BLACK_LIMIT );
                ShowImage( Editor.Zoom );
                end
           else begin
              ToolsForm.CaptureImage1.Picture.Bitmap.Width  := ToolsForm.CaptureImage1.Picture.Bitmap.Width  + 2;
              ToolsForm.CaptureImage1.Picture.Bitmap.Height := ToolsForm.CaptureImage1.Picture.Bitmap.Height + 2;
              BitmapClampSize( ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height, W, H );
              if W <> ToolsForm.CaptureImage1.Picture.Bitmap.Width  then ToolsForm.CaptureImage1.Picture.Bitmap.Width  := W;
              if H <> ToolsForm.CaptureImage1.Picture.Bitmap.Height then ToolsForm.CaptureImage1.Picture.Bitmap.Height := H;
              with OriginalBitmap do with Canvas do begin
                CopyMode  := cmSrcCopy;
                Width     := ToolsForm.CaptureImage1.Picture.Bitmap.Width;
                Height    := ToolsForm.CaptureImage1.Picture.Bitmap.Height;
                R         := Rect( 0, 0, Width, Height );
                CopyRect( R, ToolsForm.CaptureImage1.Picture.Bitmap.Canvas, R );
                // fill the extra 2 pixel columns/rows by duplicating the last column/row
                if Width > 2 then
                   for Index := Width - 2 to Width - 1 do
                       CopyRect( Rect( Index, 0, Succ( Index ), Height ), OriginalBitmap.Canvas, Rect( Pred( Index ), 0, Index, Height ) );
                if Height > 2 then
                   for Index := Height - 2 to Height - 1 do
                       CopyRect( Rect( 0, Index, Width, Succ( Index ) ), OriginalBitmap.Canvas, Rect( 0, Pred( Index ), Width, Index ) );
                ChangeBlackPixels( OriginalBitmap, BLACK_LIMIT );
                end;
              Editor.InputFileName := FileName;
              Editor.CompletedStep := csImage;
              Editor.Step := csBoard;
              ClearSelection;
              ShowTitle;
              ShowImage( 100 );
              //CaptureScrollBox.Update;
              AnalyseImageResultSet := [ airHorzLines, airVertLines, airIntegralImage{, airGradientDirections} ];
              if ToolsForm.CaptureSettingsColorQuantizationIgnoreSmallColorDifferences.Checked then
                 Include( AnalyseImageResultSet, airIntegralImage2 );
              if AnalyseImage( OriginalBitmap, IntegralImageWidth, IntegralImageHeight, AnalyseImageResultSet, HorzLines, VertLines, IntegralImage, IntegralImage2, GradientDirections ) and
                 FindBoard( OriginalBitmap,
                            Rect( 0, 0, OriginalBitmap.Width, OriginalBitmap.Height ),
                            IntegralImageWidth, IntegralImageHeight, HorzLines, VertLines, IntegralImage, IntegralImage2,
                            Settings.FindBoardTimeLimitMS,
                            R, ColCount, RowCount ) then begin
                 Editor.CursorRect       := R;
                 Editor.UserWidth        := NormalizedSize( RectWidth( R ) );
                 Editor.UserHeight       := NormalizedSize( RectHeight( R ) );
                 Editor.HasSelection     := True;
                 Editor.CompletedStep    := csColumnsRows;
                 ToolsForm.ColumnsSpinEdit.Text    := IntToStr( ColCount );
                 ToolsForm.RowsSpinEdit.Text       := IntToStr( RowCount );
                 ShowGrid( Editor.CursorRect );
                 SetSkinHints;
                 end;
              ShowImage( 100 );
              if ( OriginalBitmap.Width  >  MIN_BOARD_SIZE_PIXELS ) and
                 ( OriginalBitmap.Height >  MIN_BOARD_SIZE_PIXELS ) then
                 Editor.Cursor := ctLeftLine;
              ShowStatus;
              end;
           end;
    except on E:Exception do begin
              Msg( 'Loading the image failed.' , '', MB_ICONERROR );
              Clear;
              ShowStatus;
              Result := False;
              end;
    end;
  finally //if Result then CaptureImage1.Visible := True;
          if not Result then OpenPictureDialog1.FileName := '';
  end;
end;

procedure TCaptureForm.FileOpen(Sender: TObject);
begin
  if Editor.Modified then CloseCapture;
  if not Editor.Modified then with OpenPictureDialog1 do
     try
       Title := TEXT_LOAD_IMAGE_CAPTION;
       if ( InitialDir =  '' ) or ( not DirectoryExists( InitialDir ) ) then
          InitialDir := StrWithoutTrailingPathDelimiter( GetFolderPath( CSIDL_MYPICTURES ) );
       if ( InitialDir =  '' ) or ( not DirectoryExists( InitialDir ) ) then
          InitialDir := StrWithoutTrailingPathDelimiter( GetFolderPath( CSIDL_PERSONAL ) );
       FileName := '';
       if Execute then begin
          InitialDir := StrWithoutTrailingPathDelimiter( ExtractFilePath( FileName ) );
          if   LoadImage( FileName ) then
               FindBoardUsingMatchingSkin( MatchingSkinFileName, Settings.MatchThresholdPct, False );
          end;
     except on E:Exception do Error( E.Message, '' );
     end;
end;

function  TCaptureForm.Save(Sender: TObject):Boolean;
var B, Done, IsNew : Boolean; OriginalFileName, OriginalPuzzleTitle, s, s1 : String; R : TRect;

    function  SaveLevelAsTextFile ( const FileName__, Title__, Author__ : String ) : Boolean;
    var W, H : Integer; CapturedBoardAsText : String; F : TextFile;
    begin
      try
        if BoardAsTextWithBorder( CapturedBoardAsText, W, H ) then begin
           AssignFile(F,FileName__);
           Rewrite(F);
           try     Write( F, PaddedTextToLines( CapturedBoardAsText, W ) );
                   Writeln( F );
                   if Title__  <> '' then Writeln( F, ToolsForm.TitleLabel.Caption  + SPACE + Title__  );
                   if Author__ <> '' then Writeln( F, ToolsForm.AuthorLabel.Caption + SPACE + Author__ );
           finally CloseFile(F);
                   Result := True;
           end
           end
        else raise Exception.Create( TEXT_TASK_FAILED );
      except on E:Exception do Result := Error( E.Message, '' );
      end;
    end;

  function  SaveLevel( const FileName__ : String; var Done__ : Boolean ) : Boolean;
  var Text, TemporaryFileName : String; Level, MatchingBoardLevel, NewLevel,PreviousLevel : TLevel; SokoFile1, SokoFile2 : TSokoFile;
  begin
      Result := False; Done__ := False; SokoFile1 := nil; SokoFile2 := nil;
      TemporaryFileName := '';
      try
        try
                if StrEqual( ExtractFileExt( FileName__ ), SOKOBAN_FILE_NAME_EXT ) and
                   FileExists( FileName__ ) and
                   CreateObject( otSokoFile, TNode( SokoFile1 ) ) and
                   SokoFile1.IsASokobanFile( FileName__ ) and
                   CreateObject( otSokoFile, TNode( SokoFile2 ) ) and
                   SokoFile2.SetName( Misc_.MakeNewFileName( FileName__, SOKOBAN_FILE_NAME_EXT, False ) ) and
                   ( SokoFile2.Name <> '' ) and
                   SaveLevelAsTextFile( SokoFile2.Name, ToolsForm.PuzzleTitleEdit.Text, ToolsForm.PuzzleAuthorEdit.Text ) then begin
                   TemporaryFileName := SokoFile2.Name;
                   SokoFile1.AddFileFormatDescriptionToFiles := MainForm.SokoFile.AddFileFormatDescriptionToFiles;

                   if SokoFile2.LoadFromFile( SokoFile2.Name ) and
                      ( SokoFile2.Levels.Count = 1 ) then begin
                      NewLevel := TLevel( SokoFile2.Levels.First );
                      if   NewLevel.Name <> ''                                then ToolsForm.PuzzleTitleEdit.Text := NewLevel.Name;
                      if   NewLevel.Notes.Lines.ReadString( KEY_TITLE, Text ) then ToolsForm.PuzzleTitleEdit.Text := Text;
                      ToolsForm.PuzzleTitleEdit.Text := TextThatCannotBeInterpretedAsMoves( Trim( StrToFileName( ToolsForm.PuzzleTitleEdit.Text ) ) );

                      MatchingBoardLevel := nil;
                      if   IsAnIniFileSectionFileName( Editor.PuzzleFileName ) and
                           StrEqual( FileName__, ExtractIniFileName( Editor.PuzzleFileName ) ) then begin // 'True': the most recently saved (version of the) level was saved in this file, which may be a level collection instead of a single-level text file
                           Level := SokoFile1.GetLevelByName( ExtractSectionName( Editor.PuzzleFileName ) );
                           if Assigned( Level ) then begin // True: 'Level' contains the most recently saved (version of the) level from the capture tool, or more precisely, the names match
                              if (not NewLevel.BoardAsTextLines.IsEqual( Level.BoardAsTextLines ) )
                                 and
                                 ( Msg( Format( LevelExistsText__, [ ExtractSectionName( Editor.PuzzleFileName ) ] ) + NL + NL + OverwriteItText,
                                        SaveDialog.Title,
                                        MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON2 ) <> IDYES ) then begin
                                 Done__ := True;
                                 end
                              else begin
                                 Result := NewLevel.SnapshotsAsText.IsEmpty; // the new level shouldn't have any snapshots or solutions as this time
                                 if not Result then Done__ := not Error( TEXT_TASK_FAILED, '' ); // this sets 'Done__' = True'
                                 if not Level.Notes.Lines.WriteString( KEY_TITLE , ToolsForm.PuzzleTitleEdit .Text ) then Result := False; // update the old notes with the new title
                                 if not Level.Notes.Lines.WriteString( KEY_AUTHOR, ToolsForm.PuzzleAuthorEdit.Text ) then Result := False; // update the old notes with the new author
                                 if Result then begin
                                    SwapNodes( TNode( Level.Notes ) , TNode( NewLevel.Notes ) ); // keep the old notes from 'Level' after they have been updated with the new title and the new author
                                    PreviousLevel := TLevel( SokoFile1.Levels.Prev( Level ) );
                                    SokoFile1.Levels.Remove( Level, False ); // tentatively remove the previously saved version of the level
                                    if Settings.CheckForDuplicatesWhenAppendingNewLevelsToCollections then
                                       MatchingBoardLevel := SokoFile1.GetLevelByBoard( NewLevel.BoardAsTextLines ); // search for a matching board (simple matching)
                                    if Assigned( MatchingBoardLevel ) then begin // 'True': there is a level in the collection with an identical board
                                       Result := False; Done__ := True;
                                       SokoFile1.Levels.InsertAfter( Level, PreviousLevel ); // put the most recently saved version of the level back in the collection, so it will be destroyed normally when the owning 'TSokoFile' is destroyed
                                       Msg( Format( CannotSaveLevelToFileBecauseItContainsAnIdenticalLevelText__, [ FileName__, MatchingBoardLevel.Name ] ), ToolsForm.Caption, MB_ICONINFORMATION + MB_OK );
                                       end
                                    else begin
                                       SwapNodes( TNode( Level.SnapshotsAsText ) , TNode( NewLevel.SnapshotsAsText ) ); // keep the existing snapshots and solutions, if any
                                       Level.Free; // destroy the previously saved version of the level
                                       SokoFile1.Levels.InsertAfter( SokoFile2.Levels.Remove( NewLevel, False ), PreviousLevel ); // transfer the new version of the level to the existing file
                                       SokoFile1.Modified := True; // 'True': the collection has been updated
                                       end;
                                    end;
                                 end;
                              end;
                           end;

                      if   ( not Result ) and ( not Done__ ) then begin // 'True' : the level hasn't been transferred to the existing file yet, and a level with an identical board hasn't been found in the collection
                           if not Assigned( MatchingBoardLevel ) and Settings.CheckForDuplicatesWhenAppendingNewLevelsToCollections then // 'True': a level with a matching board hasn't been searched yet
                              MatchingBoardLevel := SokoFile1.GetLevelByBoard( NewLevel.BoardAsTextLines ); // search for a matching board (simple matching)
                           if Assigned( MatchingBoardLevel )  then begin // a level with an identical board exists;
                              //load title and author information from the level with the identical board
                              //if   MatchingBoardLevel.Notes.Lines.ReadString( KEY_TITLE , Text ) then
                              //     ToolsForm.PuzzleTitleEdit.Text := Text
                              //else ToolsForm.PuzzleTitleEdit.Text := MatchingBoardLevel.Name;
                              //ToolsForm.PuzzleTitleEdit.Text := TextThatCannotBeInterpretedAsMoves( Trim( StrToFileName( ToolsForm.PuzzleTitleEdit.Text ) ) );
                              //if   MatchingBoardLevel.Notes.Lines.ReadString( KEY_AUTHOR, Text ) then ToolsForm.PuzzleAuthorEdit.Text := Text;
                              //Result := True;
                              Result := False; Done__ := True;
                              Msg( Format( CannotSaveLevelToFileBecauseItContainsAnIdenticalLevelText__, [ FileName__, MatchingBoardLevel.Name ] ), ToolsForm.Caption, MB_ICONINFORMATION + MB_OK );
                              end
                           else begin // the level doesn't exist in this collection
                              Result := NewLevel.SetName( SokoFile1.Levels.MakeUniqueName( NewLevel.Name, '', True ) ) and
                                        ( NewLevel.Name <> '' );
                              if Result then begin
                                 ToolsForm.PuzzleTitleEdit.Text := TextThatCannotBeInterpretedAsMoves( Trim( StrToFileName( NewLevel.Name ) ) );
                                 Result := NewLevel.Notes.Lines.WriteString( KEY_TITLE , ToolsForm.PuzzleTitleEdit.Text );
                                 if Result then SokoFile1.Levels.Add( SokoFile2.Levels.Remove( NewLevel, False ) ); // transfer the new level to the collection
                                 end;
                              if not Result then Done__ := not Error( TEXT_TASK_FAILED, '' ); // this sets 'Done__' = True'
                              end;

                           SokoFile1.Modified := Result;
                           end;
                      end;

                   Result := Result and SokoFile1.Close; // save the updates performed by this function, if any
                   end;

                if ( not Result ) and ( not Done__ ) and ( not FileExists( FileName__ ) ) then
                   Result := SaveLevelAsTextFile( FileName__, ToolsForm.PuzzleTitleEdit.Text, ToolsForm.PuzzleAuthorEdit.Text );
                if Result then Done__ := True;

        finally if Assigned( SokoFile1 ) then SokoFile1.Modified := False;
                if Assigned( SokoFile2 ) then SokoFile2.Modified := False;
                SokoFile1.Free; SokoFile2.Free;
                if TemporaryFileName <> '' then DeleteFile( TemporaryFileName ); // delete the temporary file with the new level
        end;
      except on E:Exception do begin
             Result := Error( E.Message, '' );
             Done__ := True;
             end;
      end;
  end; // SaveLevel

  function  SaveSkin( const FileName__ : String; var Done__ : Boolean ) : Boolean;
  var
    i : Integer;
    R : TRect;
    MatchingSkinCaptureInformation : PSkinCaptureInformation; // skin capture information from a matching skin image
    Bitmap : TBitmap;
    CapturedBoard : TCaptureBoard;
  begin // 'SaveSkin' checks if the file with 'FileName__' is a matching
        // previously captured skin; in that case, the user is offered the
        // choice to update the existing skin, instead of overwriting it;
    Result := False; Done__ := False;
    //OriginalMatchinSkinFileName := MatchingSkinFileName;
    Bitmap := nil; MatchingSkinCaptureInformation := nil;
    try
      try
          if ( Assigned( SingleRowSkinBitmap )
               or
               CreateSingleRowSkinFromSkin( SkinBitmap, ToolsForm.ColWidthSpinEdit.Value, ToolsForm.RowHeightSpinEdit.Value, SingleRowSkinBitmap )
             )
             and
             LoadImageFromFile         ( FileName__, Bitmap                                          ) and
             ReadSkinCaptureInformation( Bitmap, False, i, i, MatchingSkinCaptureInformation         ) and
             ( MatchingSkinCaptureInformation.Header.ColumnWidth = ToolsForm.ColWidthSpinEdit.Value  ) and
             ( MatchingSkinCaptureInformation.Header.RowHeight   = ToolsForm.RowHeightSpinEdit.Value ) and
             FindBoardUsingUnscaledSkin( nil, MatchingSkinCaptureInformation, NormalizedBoardRect, Settings.MatchThresholdPct, R, CapturedBoard ) and // 'True': the skin still matches the board, at least to some extent
             ( ( Abs( R.Left - ToolsForm.LeftSpinEdit.Value ) mod Max( 1, ToolsForm.ColWidthSpinEdit .Value ) ) = 0 ) and
             ( ( Abs( R.Top  - ToolsForm.TopSpinEdit .Value ) mod Max( 1, ToolsForm.RowHeightSpinEdit.Value ) ) = 0 ) and // 'True': the matching skin is still aligned with the cells on the board
             ( FileHasFileTime( FileName__, Editor.SkinFileTime ) // 'True': the application has created the file, or loaded it earlier so it's the current skin
               or
               ( Msg( Format( TEXT_FORMAT_IMAGE_IS_A_MATCHING_SKIN, [ AbbreviatedFilePath( FileName__, MainForm.MyDocumentsFolder ) ] ),
                      SaveDialog.Title,
                      MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON1 )
                 = IDYES
               )
             ) then begin
             MatchingSkinFileName := FileName__; // the selected file is a matching skin
             Result := CreateSkinFromImage; // this updates the existing file with new skin tiles and new integral image values, if any
             ShowImage( Editor.Zoom ); // new skin tiles may have been loaded from the matching skin, hence, refresh the display
             Done__ := True;
             end;
      except on E:Exception do begin
                Done__ := True;
                Result := Error( E.Message, '' );
                end;
      end;
    finally Bitmap.Free; FreeMem( MatchingSkinCaptureInformation );
    end;
  end;

begin // 'Save'
  Result:= ( ( Editor.Step = csSavePuzzle ) and CheckBoard( Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap, False, R, B ) )
           or
           ( ( Editor.Step = csSaveSkin   ) and ( ( Assigned( SkinBitmap ) and Assigned( SingleRowSkinBitmap ) ) or CreateSkinFromImage ) );
  IsNew := False; Done := False;
  OriginalFileName := ''; OriginalPuzzleTitle := ToolsForm.PuzzleTitleEdit.Text;
  try
      if Result then begin
         SaveDialog.Filter := AllFilesFilterText;
         SaveDialog.FilterIndex := 1;

         ToolsForm.PuzzleTitleEdit .Text := TextThatCannotBeInterpretedAsMoves( StrToFileName( Trim( ToolsForm.PuzzleTitleEdit.Text ) ) );
         ToolsForm.PuzzleAuthorEdit.Text := Trim( ToolsForm.PuzzleAuthorEdit.Text );

         if Editor.Step = csSavePuzzle then begin
            if   IsAnIniFileSectionFileName( Editor.PuzzleFileName ) then
                 SaveDialog.FileName := ExtractIniFileName ( Editor.PuzzleFileName )
            else SaveDialog.FileName := Editor.PuzzleFileName;
            if   SaveDialog.FileName =  '' then
                 SaveDialog.FileName := ToolsForm.PuzzleTitleEdit.Text;
            SaveDialog.Title := Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+Trim(ToolsForm.PageControl1.ActivePage.Caption) + SUB_TITLE_SEPARATOR + TEXT_SAVE_PUZZLE;
            if PuzzleInitialDirectory = '' then
               PuzzleInitialDirectory := StrWithoutTrailingPathDelimiter( StrWithTrailingPathDelimiter( MainForm.ApplicationDataPath ) + DEFAULT_LEVEL_DIRECTORY );
            SaveDialog.InitialDir := PuzzleInitialDirectory;
            end
         else begin
            SaveDialog.FileName:=Editor.SkinFileName;
            if SaveDialog.FileName = '' then
               SaveDialog.FileName := StrToFileName( Trim( ToolsForm.SkinTitleEdit.Text ) );
            SaveDialog.Title := Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+Trim(ToolsForm.PageControl1.ActivePage.Caption) + SUB_TITLE_SEPARATOR + TEXT_SAVE_SKIN_CAPTION;
            if SkinInitialDirectory = '' then
               SkinInitialDirectory := StrWithoutTrailingPathDelimiter( MainForm.Skins.DefaultSkinPath );
            SaveDialog.InitialDir := SkinInitialDirectory;
            end;

         if   ( Sender = ToolsForm.CaptureFileMenuItemSaveAs ) or
              ( Sender = ToolsForm.CaptureToolButtonSaveAs ) or
              ( Sender = ToolsForm.SavePuzzleAsBitBtn ) or
              ( Sender = ToolsForm.SaveSkinAsBitBtn ) or
              ( SaveDialog.FileName = '' ) then begin
              IsNew := True;
              if SaveDialog.FileName='' then begin
                 if Editor.Step = csSavePuzzle then begin
                    s:=TEXT_LEVEL; s1 := SOKOBAN_FILE_NAME_EXT;
                    end
                 else begin
                    if Trim( DefaultSkinFileNameText ) = '' then
                       DefaultSkinFileNameText := SkinText;
                    s := DefaultSkinFileNameText; s1 := BMP_FILE_EXT;
                    end;
                 if SaveDialog.InitialDir =  '' then
                    SaveDialog.InitialDir := StrWithoutTrailingPathDelimiter( ExtractFilePath( Editor.InputFileName ) );
                 if SaveDialog.InitialDir =  '' then
                    SaveDialog.InitialDir := StrWithoutTrailingPathDelimiter( MainForm.ApplicationDataPath );
                 s  := MakeNewFileName( StrWithTrailingPathDelimiter( SaveDialog.InitialDir ) + s, s1, Editor.Step = csSavePuzzle );
                 if   s <> '' then
                      begin SaveDialog.FileName:=s;
                      end
                 else begin Result:=False; Error(DiskFullText, Application.Title);
                      end;
                 end;
              end;
         end;

      if Result then with SaveDialog do begin
         if ExtractFilePath( FileName ) <> '' then
            InitialDir := StrWithoutTrailingPathDelimiter( ExtractFilePath( FileName ) );
         if ( InitialDir = '' ) or ( not DirectoryExists( InitialDir ) ) then
            InitialDir := StrWithoutTrailingPathDelimiter(GetFolderPath(CSIDL_PERSONAL));

         OriginalFileName:=FileName; // including path
         FileName:=StrToFileName(ExtractFileName(FileName)); // excluding path

         if ( not IsNew ) or Execute then begin
                      if ExtractFilePath(FileName)='' then
                         FileName:=StrWithTrailingPathDelimiter(InitialDir)+FileName;

                      if  Editor.Step = csSaveSkin then
                          FileName := ChangeFileExt(FileName, BMP_FILE_EXT   );

                      if ExtractFileExt(FileName)='' then
                         if   Editor.Step = csSavePuzzle then
                              FileName := ChangeFileExt(FileName, SOKOBAN_FILE_NAME_EXT )
                         else FileName := ChangeFileExt(FileName, BMP_FILE_EXT   );

                      if DirectoryExists(FileName) then begin
                         Msg(Format(DirectoryExistsText__,[ FileName]),Title,MB_OK+MB_ICONINFORMATION);
                         Result:=False;
                         end
                      else begin
                         Result := False;
                         if Editor.Step = csSavePuzzle then begin
                            if ToolsForm.PuzzleTitleEdit.Text = '' then ToolsForm.PuzzleTitleEdit.Text := Trim( StrToFileName( ChangeFileExt( ExtractFileName( FileName ), '' ) ) );
                            if ToolsForm.PuzzleTitleEdit.Text = '' then ToolsForm.PuzzleTitleEdit.Text := TEXT_LEVEL;
                            Result := SaveLevel( FileName, Done ); // 'True': the level has been added to an existing collection, or updated in an existing collection
                            OriginalFileName := ''; // ensure that the "file already exists - overwrite it?" check below is triggered if the level hasn't been saved by 'SaveLevel()'
                            end
                         else begin
                            Result := SaveSkin( FileName, Done ); // 'True': the file contains a matching skin which now has been updated and loaded as the current skin
                            if Result and Done then
                               Sender := ToolsForm.PlaySkinBitBtn; // quick-and-dirty: let the "save-current-skin" code further down save the skin again, but ensure that the right fork is chosen, i.e., the one saving the skin in "Common Sokoban Skin Format" as opposed to the single row skin version
                            end;

                         if ( not Result ) and // 'True': not saved yet
                            ( not Done ) and
                            ( not StrEqual( OriginalFileName,FileName ) ) and
                            FileExists( FileName ) and
                            ( Msg( Format( FileExistsText__, [ AbbreviatedFilePath( FileName, MainForm.MyDocumentsFolder ) ] ) + NL + NL + OverwriteItText,
                                 Title,
                                 MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON2 )
                              <> IDYES ) then
                            Result := False // cancel save operation
                          else begin
                            InitialDir:=StrWithoutTrailingPathDelimiter( ExtractFilePath( FileName ) );
                            if ( not Result ) and Done then begin // cancel save operation
                               end
                            else begin // update puzzle name or skin name depending on the task, and perform the save operation further down if it hasn't been saved already
                              if   Editor.Step = csSavePuzzle then
                                   Editor.PuzzleFileName := MakeIniFileSectionFileName( FileName, ToolsForm.PuzzleTitleEdit.Text )
                              else Editor.SkinFileName   := FileName;
                              Result := True;
                              end;
                            end;
                         end;
            end
         else Result:=False; // cancel save operation
         end;

      if Result then
         try
           if Editor.Step = csSavePuzzle then begin
              if   not Done then // 'True': the level hasn't been saved yet
                   Result := SaveLevelAsTextFile( SaveDialog.FileName, ToolsForm.PuzzleTitleEdit.Text, ToolsForm.PuzzleAuthorEdit.Text );
              if   Result then
                   PuzzleInitialDirectory := StrWithoutTrailingPathDelimiter( ExtractFilePath( SaveDialog.FileName ) )
              else Editor.PuzzleFileName  := '';
              end
           else begin
              WriteSkinSettings;
              if        Sender = ToolsForm.PlaySkinBitBtn then
                        begin SkinBitmap         .SaveToFile( Editor.SkinFileName );
                              GetFileTime( Editor.SkinFileName, Editor.SkinFileTime );
                              MatchingSkinFileName := Editor.SkinFileName; // newest saved skin
                              AddItemOrMoveItemToTopOfComboBox(CaptureForm.SkinsComboBox,SKINS_CAPACITY,AbbreviatedFilePath(Editor.SkinFileName,MainForm.MyDocumentsFolder),False);
                        end
              else if   ToolsForm.CaptureSettingsMenuItemCommonSkinFormat.Checked then
                        begin SkinBitmap         .SaveToFile( Editor.SkinFileName );
                              GetFileTime( Editor.SkinFileName, Editor.SkinFileTime );
                              MatchingSkinFileName := Editor.SkinFileName; // newest saved skin
                              AddItemOrMoveItemToTopOfComboBox(CaptureForm.SkinsComboBox,SKINS_CAPACITY,AbbreviatedFilePath(Editor.SkinFileName,MainForm.MyDocumentsFolder),False);
                        end
                   else begin SingleRowSkinBitmap.SaveToFile( Editor.SkinFileName );
                              ClearFileTime( Editor.SkinFileTime );
                              if StrEqual( MatchingSkinFileName, Editor.SkinFileName ) then
                                 MatchingSkinFileName := '';
                        end;
              SkinInitialDirectory := StrWithoutTrailingPathDelimiter(ExtractFilePath(SaveDialog.FileName));
              end;
         except on E:Exception do begin
                   if Editor.Step = csSavePuzzle then
                      Editor.PuzzleFileName  := ''
                   else begin
                      Editor.SkinFileName    := '';
                      ClearFileTime( Editor.SkinFileTime );
                      end;
                   Result := Error( E.Message, '' );
                   end;
         end;
  finally
    if not Result then
       ToolsForm.PuzzleTitleEdit.Text := OriginalPuzzleTitle;
    ShowTitle;
    ShowStatus;
  end;
end;

procedure TCaptureForm.FileSave(Sender: TObject);
begin
  if      Editor.Step = csSavePuzzle then begin
          if   Editor.PuzzleFileName <> '' then
               Save( ToolsForm.CaptureFileMenuItemSave )
          else Save( ToolsForm.CaptureFileMenuItemSaveAs );
          end
  else if Editor.Step = csSaveSkin   then begin
          if   Editor.SkinFileName   <> '' then
               Save( ToolsForm.CaptureFileMenuItemSave )
          else Save( ToolsForm.CaptureFileMenuItemSaveAs );
          end;                       
end;

procedure TCaptureForm.FileSaveAs(Sender: TObject);
begin
  Save( Sender );
end;

procedure TCaptureForm.FilePrint(Sender: TObject);
begin
  //if PrintDialog.Execute then
  begin
    { Add code to print current file }
  end;
end;

procedure TCaptureForm.FilePrintSetup(Sender: TObject);
begin
  //PrintSetupDialog.Execute;
end;

procedure TCaptureForm.FileExit(Sender: TObject);
begin
  Close;
end;

procedure TCaptureForm.EditUndo(Sender: TObject);
var Index : Integer; Text : String;
begin
  if ToolsForm.CaptureEditMenuItemUndo.Enabled then
     if Editor.Step <= Pred( csBoardSquares ) then begin
        if Assigned( UndoBitmap ) then begin
           OriginalBitmap.Assign( UndoBitmap );
           UndoBitmap.Free; UndoBitmap := nil;
           ShowImage( Editor.Zoom );
           end
        end
     else with Editor.History do
        if ItemIndex      <> StackBottom then begin
           Index          := ItemIndex;
           if   ItemIndex >  Low( Items ) then
                Dec( ItemIndex )
           else ItemIndex := Pred( High( Items ) ); // update before calling 'ShowBoard' and 'ShowStatus'
           Text := BoardToText( Editor.Board, UNDEFINED_SQUARE_CHAR ); // take a snapshot of the current board
           if   ( Length( Text ) = Editor.Board.Width * Editor.Board.Height ) and
                TextToBoard( Items[ Index ], Editor.Board ) then begin
                Items[ Index ] := Text; // store the board state as it was before the undo operation, so 'redo' has the correct information
                ShowBoard;
                end
           else ClearHistory;
           end;
end;

procedure TCaptureForm.EditRedoItemClick(Sender: TObject);
var Text : String;
begin
  if ToolsForm.CaptureEditMenuItemRedo.Enabled then
     if Editor.Step <= Pred( csBoardSquares ) then begin
        if Assigned( UndoBitmap ) then begin
           OriginalBitmap.Assign( UndoBitmap );
           UndoBitmap.Free; UndoBitmap := nil;
           ShowImage( Editor.Zoom );
           end
        end
     else with Editor.History do
        if ItemIndex      <> StackTop then begin
           ItemIndex      := Succ( ItemIndex ) mod High( Items ); // update before calling 'ShowBoard' and 'ShowStatus'
           Text           := BoardToText( Editor.Board, UNDEFINED_SQUARE_CHAR ); // take a snapshot of the current board
           if   ( Length( Text ) = Editor.Board.Width * Editor.Board.Height ) and
                TextToBoard( Items[ ItemIndex ], Editor.Board ) then begin
                Items[ ItemIndex ] := Text; // store the board state as it was before the redo operation, so 'undo' has the correct information
                ShowBoard;
                end
           else ClearHistory;
           end;
end;

procedure TCaptureForm.SaveUndoBitmap;
begin
  with Editor do
    try
      if not Assigned( UndoBitmap ) then UndoBitmap := TBitmap.Create;
      if Assigned( OriginalBitmap ) then UndoBitmap.Assign( OriginalBitmap )
      else raise Exception.Create( '' ); // delete 'undo bitmap', if any
    except on E:Exception do ClearHistory;
    end;
end;

procedure TCaptureForm.EditCut(Sender: TObject);
var NewPosition : Integer; R: TRect;
begin
  if ( not ( Sender is TMenuItem ) ) and
     ( ToolsForm.ActiveControl is TEdit ) and
     ( TEdit( ToolsForm.ActiveControl ).SelLength > 0 ) then
     with ToolsForm.ActiveControl as TEdit do
       try
         if SelLength = 0 then begin
            //Clipboard.AsText := Text;
            //Text := '';
            end
         else begin
            Clipboard.AsText := Copy( Text, Succ( SelStart ), SelLength );
            NewPosition := SelStart;
            Text := Copy( Text, 1, SelStart ) + Copy( Text, Succ( SelStart + SelLength ), MaxInt );
            SelLength := 0; SelStart := NewPosition;
            end;
       except on E:Exception do Error( E.Message, '' );
       end
  else
     if ToolsForm.CaptureEditMenuItemCut.Enabled then with Editor do begin
        EditCopy( Sender );
        if HasSelection then
           R := NormalizedRect( CursorRect )
        else
           R:= Rect( 0, 0, OriginalBitmap.Width - 2, OriginalBitmap.Height - 2 );
        with OriginalBitmap do with Canvas do begin
          SaveUndoBitmap;
          Brush.Style := bsSolid;
          Brush.Color := clWhite;
          FillRect( R );
          ShowImage( Zoom );
          end;
        end;
end;

procedure TCaptureForm.EditCopy(Sender: TObject);
var W, H : Integer; R: TRect; Text: String; B: TBitmap;
begin
  if ( not ( Sender is TMenuItem ) ) and
     ( ToolsForm.ActiveControl is TEdit ) and
     ( TEdit( ToolsForm.ActiveControl ).SelLength > 0 ) then
     with ToolsForm.ActiveControl as TEdit do
       try
         if   SelLength = 0 then
              Clipboard.AsText := Text
         else Clipboard.AsText := Copy( Text, Succ( SelStart ), SelLength );
       except on E:Exception do Error( E.Message, '' );
       end
  else
     if ToolsForm.CaptureEditMenuItemCopy.Enabled then with Editor do
        if Step < csSavePuzzle then begin
           if HasSelection or ( Step >= csBoardSquares ) then
              R := NormalizedBoardRect
           else
              R:= Rect( 0, 0, OriginalBitmap.Width - 2, OriginalBitmap.Height - 2 );

           if BitmapCreate( B, RectWidth( R ), RectHeight( R ) ) then
              try     B.Canvas.CopyRect( Rect( 0, 0, B.Width, B.Height ), OriginalBitmap.Canvas, R );
                      try    Clipboard.Assign( B );
                      except on E:Exception do Error( E.Message, '' );
                      end
              finally B.Free;
                      ShowStatus;
              end;
           end
        else
          if Step = csSavePuzzle then begin
             if BoardAsTextWithBorder( Text, W, H ) then begin
                Text := PaddedTextToLines( Text, W );
                if MainForm.CopyLevelToClipboardFillFloors then
                   Text := SokUtil_.StrSubstituteAll( Text, FLOOR_CH, MainForm.CopyLevelToClipboardFloorFillCharacter );
                if Trim( ToolsForm.PuzzleTitleEdit.Text ) <> '' then
                   Text := Text + NL + ToolsForm.TitleLabel.Caption  + SPACE + Trim( ToolsForm.PuzzleTitleEdit.Text );
                if Trim( ToolsForm.PuzzleAuthorEdit.Text ) <> '' then
                   Text := Text + NL + ToolsForm.AuthorLabel.Caption + SPACE + Trim( ToolsForm.PuzzleAuthorEdit.Text );
                Clipboard.AsText := Text;
                end
             else Error( TEXT_TASK_FAILED, ToolsForm.CaptureEditMenuItemCopy.Caption );
             end
          else if Step = csSaveSkin then
                  if Assigned( SkinBitmap ) or CreateSkinFromImage then
                     try    Clipboard.Assign( SkinBitmap );
                     except on E:Exception do Error( E.Message, '' );
                     end
                  else Error( TEXT_TASK_FAILED, ToolsForm.CaptureEditMenuItemCopy.Caption );
end;

procedure TCaptureForm.EditPaste(Sender: TObject);
var NewPosition : Integer; ClipboardText : String;
begin
  if ( not ( Sender is TMenuItem ) ) and
     ( ToolsForm.ActiveControl is TEdit ) and
     Clipboard.HasFormat( CF_TEXT )  then
     with ToolsForm.ActiveControl as TEdit do
       try
         ClipboardText := Clipboard.AsText;
         NewPosition := SelStart + Length( ClipboardText );
         Text := Copy( Text, 1, SelStart ) + ClipboardText + Copy( Text, Succ( SelStart + SelLength ), MaxInt );
         SelLength := 0; SelStart := NewPosition;
       except on E:Exception do Error( E.Message, '' );
       end
  else
     if ( ( Editor.Step >= csBoard ) and ( Editor.Step < csColumnsRows ) and Editor.HasSelection )
        or
        CloseCapture then
        if LoadImage( '' ) then begin
           Editor.InputFileName := TITLE_ILLEGAL_FIRST_CHARACTER + NewText + TITLE_ILLEGAL_FIRST_CHARACTER;
           ShowTitle;
           FindBoardUsingMatchingSkin( MatchingSkinFileName, Settings.MatchThresholdPct, False );
           end;
end;

procedure TCaptureForm.EditCapture(Sender: TObject);
//var OriginalWindowState : TWindowState;
begin
  //OriginalWindowState := WindowState;
  Application.Minimize;
  try     //WindowState := wsMinimized;
          SleepEx( 500, False );  // 0.5 second is probably enough time for a minimize animation to finish
          if LoadImage( FILE_NAME_PATH_DELIMITER ) then begin // 'LoadImageFromFileOrClipboard()' treats 'FILE_NAME_PATH_DELIMITER' as a "capture screen" command
             Editor.InputFileName := TITLE_ILLEGAL_FIRST_CHARACTER + NewText + TITLE_ILLEGAL_FIRST_CHARACTER;
             ShowTitle;
             FindBoardUsingMatchingSkin( MatchingSkinFileName, Settings.MatchThresholdPct, False );
             end;
  finally //WindowState := OriginalWindowState;
          Application.Restore;
  end;
end;

procedure TCaptureForm.WindowTile(Sender: TObject);
begin
  Tile;
end;

procedure TCaptureForm.WindowCascade(Sender: TObject);
begin
  Cascade;
end;

procedure TCaptureForm.WindowArrange(Sender: TObject);
begin
  ArrangeIcons;
end;

procedure TCaptureForm.HelpContents(Sender: TObject);
begin
  Msg( HelpAboutText + NL + NL + TEXT_CAPTURE_HELP_1 + TEXT_CAPTURE_HELP_2 + TEXT_CAPTURE_HELP_3, CaptureForm.Caption, MB_OK );
end;

procedure TCaptureForm.HelpSearch(Sender: TObject);
const
  EmptyString: PChar = '';
begin
  Application.HelpCommand(HELP_PARTIALKEY, Longint(EmptyString));
end;

procedure TCaptureForm.HelpHowToUse(Sender: TObject);
begin
  Application.HelpCommand(HELP_HELPONHELP, 0);
end;

procedure TCaptureForm.HelpAbout(Sender: TObject);
begin
  Msg( HelpAboutText, CaptureForm.Caption, MB_OK );
end;

function TCaptureForm.HelpAboutText : String;
begin
  Result := TEXT_CAPTURE_APPLICATION_TITLE + NL + VersionText + ': ' + TEXT_CAPTURE_VERSION_NUMBER + NL + TEXT_COPYRIGHT +': (c) ' + TEXT_CAPTURE_COPYRIGHT_YEAR + ' '+ TEXT_CAPTURE_COPYRIGHT_HOLDER +
            NL + NL + TEXT_CAPTURE_EDITOR_SKIN_COPYRIGHT;
end;

procedure TCaptureForm.FormActivate(Sender: TObject);
begin
  if Editor.MouseButtonDown then CaptureImage1MouseUp(nil,mbLeft,[],0,0);
  SetCaptureDrawingToolHint;
  ShowStatus;
  StatusText := '';
end;

procedure TCaptureForm.FormDeactivate(Sender: TObject);
begin
//if not Editor.Selection.Enabled then HideCursor(False);
//IgnoreKeyUp:=True; IgnoreMouseUp:=True;
end;

procedure TCaptureForm.Clear;
var OriginalDrawingTool : TDrawingTool; OriginalGridColor, OriginalGridShadowColor : TColor;
begin
  if Editor.Zoom > 100 then
     ShowImage( 100 ); // reduce the memory footprint of various bitmaps assigned to the screen components
  Editor.InputFileName := ''; Editor.PuzzleFileName := ''; Editor.SkinFileName := ''; // free strings before zero-filling the editor information
  ToolsForm.PuzzleTitleEdit.Text := '';
  if not ToolsForm.CaptureSettingsMenuItemKeepAuthorAndDesignerNames.Checked then
     ToolsForm.PuzzleAuthorEdit.Text := '';
  ToolsForm.SkinTitleEdit.Text := '';
  if not ToolsForm.CaptureSettingsMenuItemKeepAuthorAndDesignerNames.Checked then
     ToolsForm.SkinDesignerEdit.Text := '';
  SingleRowSkinBitmap.Free; SingleRowSkinBitmap := nil;
  SkinBitmap.Free; SkinBitmap := nil;
  OriginalDrawingTool := dtWall; //Editor.DrawingTool;
  OriginalGridColor := Editor.GridColor;
  OriginalGridShadowColor := Editor.GridShadowColor;
  ClearHistory;
  Editor.EditorSkinBitmap.Free; Editor.EditorSkinBitmap := nil;
  ZeroMemory( Addr( Editor ), SizeOf( Editor ) );
  Editor.Zoom := 100;
  Editor.Step := csImage; Editor.CompletedStep := csNone;
  ToolsForm.CaptureToolButtonZoom.ImageIndex := IMAGE_INDEX_ZOOM_IN;
  Editor.DrawingToolsEnabled:=False;
  Editor.DrawingTool := OriginalDrawingTool;
  Editor.GridColor := OriginalGridColor; Editor.GridShadowColor := OriginalGridShadowColor;
  ClearBoard;
  ClearSelection;
  ToolsForm.CaptureImage1.Cursor := crDefault;
  if Screen.Cursor <> crDefault then
     Screen.Cursor := crDefault;
  ToolsForm.CaptureStateMemo.Width := ToolsForm.BottomPanel.ClientWidth - ToolsForm.CaptureStateMemo.Left - ToolsForm.PreviousStepButton.Left;
  if Assigned( MainForm ) and (not MainForm.ShutdownApplication) then ShowTitle;
end;

procedure TCaptureForm.ClearHistory;
var Index : Integer;
begin
  if Assigned( UndoBitmap ) then begin
     UndoBitmap.Free; UndoBitmap := nil;
     end;
  with Editor.History do begin
    for Index := Low( Items ) to High( Items ) do
        Items[ Index ] := '';
    ZeroMemory( Addr( Editor.History ), SizeOf( Editor.History ) );
    TransactionIndex := Pred( Low( Items ) );
    end;
  ShowStatus;
end;

procedure TCaptureForm.ClearBoard;
begin
  Capture_.ClearBoard( Editor.Board );
  ClearHistory;
  if Editor.CompletedStep          >= csBoardSquares then
     Editor.CompletedStep          := Pred( csBoardSquares);
  Timestamp                        := High( Timestamp ); // initializes the calculation of the pusher's reachable squares in 'CheckBoard'
  Colors.Count                     := 0;
  Editor.DrawingTool               := dtWall;
  ToolsForm.WallCapCheckBox.Checked          := True;
  ToolsForm.WallCapCheckBox.Tag              := 0; // '0': 'checked' isn't a user value or a value from a matching skin
  SetSpinEditValue( ToolsForm.OuterWallCutLeftSpinEdit, 0 );
  SetSpinEditValue( ToolsForm.OuterWallCutTopSpinEdit, 0 );
  SetSpinEditValue( ToolsForm.OuterWallCutRightSpinEdit, 0 );
  SetSpinEditValue( ToolsForm.OuterWallCutBottomSpinEdit, 0 );
end;

procedure TCaptureForm.ClearSelection;
begin
  HideCursor( True );
  ZeroMemory( Addr( Editor.CursorRect    ), SizeOf( Editor.CursorRect    ) );
  ZeroMemory( Addr( Editor.DragPoint     ), SizeOf( Editor.DragPoint     ) );
  Editor.UserWidth := 0;
  Editor.UserHeight := 0;
  Editor.Cursor := ctSelection;
  StatusText := '';
  ClearBoard;
  if Editor.CompletedStep >= csBoard then
     Editor.CompletedStep := Pred( csBoard );
  ToolsForm.LockColumnWidthAndRowHeightCheckBox.Checked := False;
  SetSpinEditValue( ToolsForm.LeftSpinEdit, 0 );
  SetSpinEditValue( ToolsForm.LeftSpinEdit2, 0 );
  SetSpinEditValue( ToolsForm.TopSpinEdit, 0 );
  SetSpinEditValue( ToolsForm.TopSpinEdit2, 0 );  
  SetSpinEditValue( ToolsForm.WidthSpinEdit, 0 );
  SetSpinEditValue( ToolsForm.HeightSpinEdit, 0 );
  SetSpinEditValue( ToolsForm.ColumnsSpinEdit, 0 );
  SetSpinEditValue( ToolsForm.RowsSpinEdit, 0 );
  SetSpinEditValue( ToolsForm.ColWidthSpinEdit, 0 );
  SetSpinEditValue( ToolsForm.RowHeightSpinEdit, 0 );
  ShowGrid( ScaledBoardRect );
  SetSkinHints;
  ZeroMemory( Addr( Editor.CursorRect    ), SizeOf( Editor.CursorRect    ) );
end;

procedure TCaptureForm.ShowStatus;
var W, H : Integer; B : Boolean; R : TRect;

 function  EditorCursorToDirection( EditorCursor__ : TCaptureEditorCursorType ) : TDirection;
 begin
   case EditorCursor__ of
     ctLeftLine         : Result := SokUtil_.Left;
     ctTopLine          : Result := Up;
     ctBottomLine       : Result := Down
     else                 Result := Right; // catch all
   end;
 end;

begin // 'ShowStatus'
  ToolsForm.CaptureImagePanel.Visible := Editor.Step = csImage;
  ToolsForm.CaptureBoardPanel.Visible := ( Editor.Step >= csBoard ) and ( Editor.Step < csColumnsRows );
  ToolsForm.CaptureGridPanel.Visible := ( Editor.Step >= csColumnsRows ) and ( Editor.Step < csBoardSquares );
  ToolsForm.CaptureSquaresPanel.Visible := Editor.Step = csBoardSquares;
  ToolsForm.CapturePuzzlePanel.Visible := Editor.Step = csSavePuzzle;
  ToolsForm.CaptureSkinPanel.Visible := Editor.Step = csSaveSkin;
  ToolsForm.CaptureScrollBox.Visible := Editor.Step > csImage;
  ToolsForm.PuzzleInformationPanel.Visible := Editor.Step = csSavePuzzle;
  ToolsForm.SkinInformationPanel.Visible := Editor.Step = csSaveSkin;


  ToolsForm.CaptureFileMenuItemNew.Enabled := ( Editor.Step > csImage ) or ( Editor.InputFileName <> '' );
  ToolsForm.CaptureToolButtonNew.Enabled := ToolsForm.CaptureFileMenuItemNew.Enabled;
  ToolsForm.CaptureToolButtonOpen.Enabled := ToolsForm.CaptureFileMenuItemOpen.Enabled;
  ToolsForm.OpenBitBtn.Enabled := ToolsForm.CaptureToolButtonOpen.Enabled;
  ToolsForm.CaptureFileMenuItemSave.Enabled := ( ( Editor.Step = csSavePuzzle )
                                                 //and
                                                 //( Editor.PuzzleFileName <> '' )
                                               )
                                               or
                                               ( ( Editor.Step = csSaveSkin   )
                                                 //and
                                                 //( Editor.SkinFileName   <> '' )
                                               );
  ToolsForm.CaptureFileMenuItemSave.Hint := ToolsForm.CaptureFileMenuItemSave.Caption;
  ToolsForm.CaptureEditMenuItemEditLevel.Visible := Editor.Step = csBoardSquares;
  ToolsForm.CaptureEditMenuItemEditLevelSeparator.Visible := ToolsForm.CaptureEditMenuItemEditLevel.Visible;
  ToolsForm.CaptureToolButtonSave.Enabled := ToolsForm.CaptureFileMenuItemSave.Enabled;
  ToolsForm.SavePuzzleBitBtn.Enabled :=ToolsForm.CaptureToolButtonSave.Enabled and ( Editor.Step = csSavePuzzle );
  ToolsForm.SaveSkinBitBtn.Enabled :=ToolsForm.CaptureToolButtonSave.Enabled and ( Editor.Step = csSaveSkin );
  ToolsForm.CaptureFileMenuItemSaveAs.Enabled := Editor.Step >= csSavePuzzle;
//ToolsForm.CaptureFileSaveAsItem.Hint := ToolsForm.CaptureFileSaveAsItem.Caption;
  ToolsForm.CaptureFileMenuItemNewSkin.Enabled := ( ( MatchingSkinFileName  <> '' ) and ( not StrEqual( MatchingSkinFileName, DefaultMatchingSkinFileName ) ) )
                                                  or
                                                  ( ( Editor.SkinFileName   <> '' ) and ( not StrEqual( Editor.SkinFileName , DefaultMatchingSkinFileName ) ) );
  ToolsForm.CaptureToolButtonSaveAs.Enabled := ToolsForm.CaptureFileMenuItemSaveAs.Enabled;
  ToolsForm.CaptureToolButtonSaveAs.Hint := ToolsForm.CaptureFileMenuItemSaveAs.Hint;
  ToolsForm.SavePuzzleAsBitBtn.Enabled := ToolsForm.CaptureToolButtonSaveAs.Enabled and ( Editor.Step = csSavePuzzle );
  ToolsForm.SaveSkinAsBitBtn.Enabled :=ToolsForm.CaptureToolButtonSaveAs.Enabled and ( Editor.Step = csSaveSkin );
  if Editor.History.TransactionIndex < Low( Editor.History.Items ) then
     ToolsForm.CaptureEditMenuItemUndo.Enabled := ( ( Editor.Step >= csBoard ) and ( Editor.Step < csColumnsRows ) and Assigned( UndoBitmap ) )
                                                  or
                                                  ( ( Editor.Step = csBoardSquares ) and ( Editor.History.ItemIndex <> Editor.History.StackBottom ) );
  ToolsForm.CaptureToolButtonUndo.Enabled := ToolsForm.CaptureEditMenuItemUndo.Enabled;
  if Editor.History.TransactionIndex < Low( Editor.History.Items ) then
     ToolsForm.CaptureEditMenuItemRedo.Enabled := //( ( Editor.Step >= sBoard ) and ( Editor.Step < sColumnsRows ) and Assigned( UndoBitmap ) )
                                                  //or
                                                  ( ( Editor.Step = csBoardSquares ) and ( Editor.History.ItemIndex <> Editor.History.StackTop ) );
  ToolsForm.CaptureToolButtonRedo.Enabled := ToolsForm.CaptureEditMenuItemRedo.Enabled;
  if Editor.Step < csBoardSquares then
     ToolsForm.CaptureEditMenuItemEditLevel.Enabled := False;
  ToolsForm.CaptureEditLevelBitBtn.Enabled := ToolsForm.CaptureEditMenuItemEditLevel.Enabled;
  ToolsForm.CaptureToolButtonPlay.Enabled := ( ( Editor.Step = csSavePuzzle ) and ( Editor.Board.Width <= MAX_BOARD_WIDTH ) and ( Editor.Board.Height <= MAX_BOARD_HEIGHT ) and ToolsForm.CaptureEditMenuItemEditLevel.Enabled )
                                             or
                                             ( Editor.Step = csSaveSkin );
  ToolsForm.PlayPuzzleBitBtn.Enabled := ToolsForm.CaptureToolButtonPlay.Enabled;
  ToolsForm.CaptureEditMenuItemCut.Enabled := ToolsForm.CaptureImage1.Visible and ( Editor.Step >= csBoard ) and ( Editor.Step < csColumnsRows );
  ToolsForm.CaptureToolButtonCut.Enabled := ToolsForm.CaptureEditMenuItemCut.Enabled;
  ToolsForm.CaptureEditMenuItemCopy.Enabled := ToolsForm.CaptureScrollBox.Visible and ToolsForm.CaptureImage1.Visible;
  ToolsForm.CaptureToolButtonCopy.Enabled := ToolsForm.CaptureEditMenuItemCopy.Enabled;
  ToolsForm.CopyPuzzleBitBtn.Enabled := ToolsForm.CaptureToolButtonCopy.Enabled and ( Editor.Step = csSavePuzzle );
  ToolsForm.CopySkinBitBtn.Enabled := ToolsForm.CaptureToolButtonCopy.Enabled and ( Editor.Step = csSaveSkin );
  ToolsForm.CaptureEditMenuItemPaste.Enabled := Clipboard.HasFormat( CF_Bitmap );
  ToolsForm.CaptureToolButtonPaste.Enabled := ToolsForm.CaptureEditMenuItemPaste.Enabled;
  ToolsForm.PasteBitBtn.Enabled := ToolsForm.CaptureToolButtonPaste.Enabled;
  ToolsForm.CaptureEditMenuItemCapture.Enabled := True;
  ToolsForm.CaptureEditMenuItemSelectAll.Enabled := ( Editor.Step >= csBoard ) and ( Editor.Step < csColumnsRows ) and
                                                    Assigned( OriginalBitmap ) and
                                                    ( ( ToolsForm.WidthSpinEdit.Value <> OriginalBitmap.Width - 2 ) or ( ToolsForm.HeightSpinEdit.Value <> OriginalBitmap.Height -2 ) );
  ToolsForm.CaptureSelectAllBitBtn.Enabled := ToolsForm.CaptureEditMenuItemSelectAll.Enabled;
  ToolsForm.CaptureEditMenuItemMatchingSkin.Enabled := ( ( Editor.Step >= csBoard ) and ( Editor.Step <= csBoardSquares ) );
  ToolsForm.CaptureViewMenuItem100.Enabled := ToolsForm.CaptureScrollBox.Visible and ToolsForm.CaptureImage1.Visible;
  ToolsForm.CaptureViewMenuItem200.Enabled := ToolsForm.CaptureViewMenuItem100.Enabled and ( not ( ToolsForm.CaptureSettingsMenuItemObjectsOnABlackBackground.Checked and ( Editor.Step >= csSaveSkin ) ) ) and ( ( OriginalBitMap.Width * OriginalBitMap.Height ) <= MAX_ZOOM_200_IMAGE_SIZE ); ;
  ToolsForm.CaptureViewMenuItem400.Enabled := ToolsForm.CaptureViewMenuItem200.Enabled and Assigned( OriginalBitMap ) and ( ( OriginalBitMap.Width * OriginalBitMap.Height ) <= MAX_ZOOM_400_IMAGE_SIZE ) and ( GetAvailableVirtualMemoryByteSize >= MIN_AVAILABLE_VIRTUAL_MEMORY_FOR_ZOOM_400 );
  ToolsForm.CaptureToolButtonZoom.Enabled  := ToolsForm.CaptureViewMenuItem100.Enabled and ToolsForm.CaptureViewMenuItem200.Enabled;
  if Editor.Zoom = 200 then
     if   ToolsForm.CaptureViewMenuItem400.Enabled then
          if   ToolsForm.CaptureToolButtonZoom.ImageIndex =  IMAGE_INDEX_ZOOM_OUT then
               ToolsForm.CaptureToolButtonZoom.ImageIndex := IMAGE_INDEX_ZOOM_IN
          else
     else if   ToolsForm.CaptureToolButtonZoom.ImageIndex =  IMAGE_INDEX_ZOOM_IN then
               ToolsForm.CaptureToolButtonZoom.ImageIndex := IMAGE_INDEX_ZOOM_OUT;
  ToolsForm.CaptureViewMenuItemLoadImageFromUpscaledViewOfImage.Enabled := ToolsForm.CaptureViewMenuItem100.Enabled and ( Editor.Zoom > 100 ) and ( Editor.Step < csSaveSkin );
  ToolsForm.CaptureSelectAllBitBtn.Visible := ToolsForm.CaptureSelectAllBitBtn.Top + ToolsForm.CaptureSelectAllBitBtn.Height < ToolsForm.CaptureBoardPanel.Height - 8;
  ToolsForm.LockColumnWidthAndRowHeightCheckBox.Visible := ToolsForm.LockColumnWidthAndRowHeightCheckBox.Top + ToolsForm.LockColumnWidthAndRowHeightCheckBox.Height < ToolsForm.CaptureGridPanel.Height - 8;
  ToolsForm.LeftSpinEdit2.Visible := ToolsForm.LeftSpinEdit2.Top + ToolsForm.LeftSpinEdit2.Height < ToolsForm.CaptureGridPanel.Height - 8;
  ToolsForm.LeftLabel2.Visible := ToolsForm.LeftSpinEdit2.Visible;
  ToolsForm.TopSpinEdit2.Visible := ToolsForm.TopSpinEdit2.Top + ToolsForm.TopSpinEdit2.Height < ToolsForm.CaptureGridPanel.Height - 8;
  ToolsForm.TopLabel2.Visible := ToolsForm.TopSpinEdit2.Visible;
  ToolsForm.SkinBitBtn2.Visible := ToolsForm.SkinBitBtn2.Top + ToolsForm.SkinBitBtn2.Height < ToolsForm.CaptureGridPanel.Height - 8;
  ToolsForm.SkinBitBtn3.Visible := ToolsForm.SkinBitBtn3.Top + ToolsForm.SkinBitBtn3.Height < ToolsForm.CaptureSquaresPanel.Height - 8;
  ToolsForm.CaptureEditLevelBitBtn.Visible := ToolsForm.CaptureEditLevelBitBtn.Top + ToolsForm.CaptureEditLevelBitBtn.Height < ToolsForm.CaptureSquaresPanel.Height - 8;
  if Editor.Cursor < ctLeftLine then
     if   Editor.Step <> csBoardSquares then
          if   Editor.Step <> csBoard then
               ToolsForm.CaptureStateMemo.Text := TEXT_CAPTURE_STATE_HINTS[ Editor.Step ]
          else ToolsForm.CaptureStateMemo.Text := TEXT_CAPTURE_STATE_HINTS[ Editor.Step ] + NL + TEXT_CAPTURE_STATE_HINT_ARROW_KEYS
     else ToolsForm.CaptureStateMemo.Text := TEXT_CAPTURE_STATE_HINTS[ Editor.Step ] + NL + TEXT_CAPTURE_STATE_HINT_BOARD_SIDE_2
  else if Editor.Step = csBoard then begin
          //StateMemo.Text := TEXT_STATE_HINT_BOARD_SIDES;
          ToolsForm.CaptureStateMemo.Text := Format( TEXT_FORMAT_SELECT_BOARD_SIZE, [ TEXT_RECTANGLE_SIDE[ EditorCursorToDirection( Editor.Cursor ) ] ] ) +
                                             NL + TEXT_CAPTURE_STATE_HINT_BOARD_SIDE_1 + NL + TEXT_CAPTURE_STATE_HINT_BOARD_SIDE_2;
          end
       else if Editor.Step = csColumnsRows then
               ToolsForm.CaptureStateMemo.Text := TEXT_CAPTURE_STATE_HINT_GRID_LINES
            else ToolsForm.CaptureStateMemo.Text := '';
  if   Editor.Step <= Pred( csBoardSquares ) then
       ToolsForm.CaptureImage1.Hint := ToolsForm.CaptureStateMemo.Text
  else ToolsForm.CaptureImage1.Hint := '';
  ToolsForm.CaptureScrollBox.Hint := ToolsForm.CaptureImage1.Hint;
  //ViewZoom100Item.Enabled := CaptureImage1.Visible and ( Editor.Zoom <> 100 );
  //ViewZoom200Item.Enabled := CaptureImage1.Visible and ( not ViewZoom100Item.Enabled );
  //ViewZoom400Item.Enabled := ViewZoom200Item.Enabled;
  ToolsForm.PreviousStepButton.Enabled := ( Editor.Step > csImage ) or
                                          ( ( Editor.CompletedStep = High( Editor.CompletedStep ) ) and ( Editor.Step = csImage ) ) ;
  ToolsForm.PreviousStepButton2.Enabled := ToolsForm.PreviousStepButton.Enabled;
//PreviousStepButton.Visible := PreviousStepButton.Enabled;
  if      ( Editor.Step >= csBoard ) and ( Editor.Step < csColumnsRows ) then with Editor do begin
          if ( ToolsForm.WidthSpinEdit.Value >= MIN_BOARD_SIZE_PIXELS ) and ( ToolsForm.HeightSpinEdit.Value >= MIN_BOARD_SIZE_PIXELS ) then begin
             if   CompletedStep < csBoard  then
                  CompletedStep := csBoard;
             end
          else CompletedStep := csImage;
          HasSelection := ( Editor.Cursor = ctSelection ) and ( ToolsForm.WidthSpinEdit.Value > 0 ) and ( ToolsForm.HeightSpinEdit.Value > 0 );
          if ( not HasSelection ) and ( Editor.Cursor = ctSelection ) and
             ( OriginalBitmap.Width  > MIN_BOARD_SIZE_PIXELS ) and ( OriginalBitmap.Height > MIN_BOARD_SIZE_PIXELS ) then begin
             if      ToolsForm.WidthSpinEdit .Value = 0 then Editor.Cursor := ctLeftLine
             else if ToolsForm.HeightSpinEdit.Value = 0 then Editor.Cursor := ctTopLine;
             if Editor.Cursor <> ctSelection then begin
                Editor.CursorRect := Rect ( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height );
                HideCursor( True );
                end;
             end;
          end
  else if ( Editor.Step >= csColumnsRows ) and ( Editor.Step < csBoardSquares ) then with Editor do begin
          if   ( ToolsForm.ColumnsSpinEdit.Value > 1 ) and ( ToolsForm.RowsSpinEdit.Value > 1 ) and
               ( ToolsForm.ColWidthSpinEdit .Value >= MIN_BOARD_SQUARE_SIZE_PIXELS ) and
               ( ToolsForm.RowHeightSpinEdit.Value >= MIN_BOARD_SQUARE_SIZE_PIXELS ) then begin
               if CompletedStep =  Pred( csColumnsRows  ) then begin
                  CompletedStep := Pred( csBoardSquares );
                  SetSkinHints;
                  end;
               end
          else if CompletedStep <> Pred( csColumnsRows ) then begin
                  CompletedStep := Pred( csColumnsRows );
                  SetSkinHints;
                  end;
          end
       else if Editor.Step = csBoardSquares then begin
               if   CheckBoard( Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap, False, R, B ) then
                    if   Editor.CompletedStep <  csBoardSquares then
                         Editor.CompletedStep := csBoardSquares
                    else
               else Editor.CompletedStep := Pred( csBoardSquares );
               W := RectWidth ( R );
               H := RectHeight( R );
               ToolsForm.CaptureEditMenuItemEditLevel.Enabled :=
                 ( W >= MIN_BOARD_WIDTH  ) and
                 ( W <= ( MAX_BOARD_WIDTH  - ( 2 * Ord( B ) ) ) ) and // '2 * ord( B )': reverse space for a border if one is needed to create not only a playable level, but also a guaranteed readable level
                 ( H >= MIN_BOARD_HEIGHT ) and
                 ( H <= ( MAX_BOARD_HEIGHT - ( 2 * Ord( B ) ) ) );
               ToolsForm.CaptureEditLevelBitBtn.Enabled := ToolsForm.CaptureEditMenuItemEditLevel.Enabled;
               end
            else if Editor.Step = csSavePuzzle then begin
                    if   Editor.PuzzleFileName = '' then
                         ToolsForm.PlayPuzzleBitBtn.Hint := HintPlayCapturedLevelText
                    else ToolsForm.PlayPuzzleBitBtn.Hint := HintSaveAndPlayCapturedLevelText;
                    ToolsForm.CaptureToolButtonPlay.Hint := ToolsForm.PlayPuzzleBitBtn.Hint;
                    end;

  ToolsForm.NextStepButton.Enabled      := ( Editor.Step <= Editor.CompletedStep )
                                           //and
                                           //( Editor.Step <  High( Editor.Step ) )
                                           ;
  ToolsForm.NextStepButton2.Enabled     := ToolsForm.NextStepButton.Enabled;

  Editor.DrawingToolsEnabled  := Editor.Step = csBoardSquares;

  ToolsForm.SettingsMenuItemWindowSizeMaximized.Checked:=ToolsForm.WindowState=wsMaximized;
  ToolsForm.CaptureSettingsMenuItemWindowSizeMaximized.Checked := ToolsForm.SettingsMenuItemWindowSizeMaximized.Checked;

  if   Editor.Step >= csBoard then
       ToolsForm.StatusBar1.Panels[ 0 ].Text := StatusText
  else ToolsForm.StatusBar1.Panels[ 0 ].Text := '';
end;

procedure TCaptureForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var Index : Integer; MouseCursorPosition, APoint : TPoint;
begin
  if      Key = VK_F1       then
  else if Key = VK_F2       then FileOpen( Sender )
  else if Key = VK_F3       then
  else if Key = VK_F4       then
  else if Key = VK_F5       then begin
          if ToolsForm.CaptureImage1.Visible then ZoomItemClick( Self );
          end
  else if Key = VK_F8       then begin
          if Editor.Step >= csBoard then
             if not ToolsForm.CaptureViewMenuItemShowImageAnalysisResults.Checked then
                CaptureViewImageAnalysisResultsItemClick( Self )
             else begin
               Index := Pred( ToolsForm.CaptureSettingsMenuItemImageAnalysis.Count );
               while ( Index > 0 ) and
                     ( not ToolsForm.CaptureSettingsMenuItemImageAnalysis.Items[ Index ].Checked ) do
                     Dec( Index );
               if Index >= 0 then
                  if   ssCtrl in Shift then // 'True': cycle direction: backwards
                       if   Index > 0 then
                            SettingsImageAnalysisItemsClick( ToolsForm.CaptureSettingsMenuItemImageAnalysis.Items[ Pred( Index ) ] )
                       else SettingsImageAnalysisItemsClick( ToolsForm.CaptureSettingsMenuItemImageAnalysis.Items[ Pred( ToolsForm.CaptureSettingsMenuItemImageAnalysis.Count ) ] )
                  else      SettingsImageAnalysisItemsClick( ToolsForm.CaptureSettingsMenuItemImageAnalysis.Items[ Succ( Index ) mod ToolsForm.CaptureSettingsMenuItemImageAnalysis.Count ] );
               end;
          end
  else if Key = VK_PRIOR    then PreviousStepButtonClick( Sender )
  else if Key = VK_NEXT     then NextStepButtonClick( Sender )
  else if Shift = [ ssCtrl] then begin
          if      Key = Ord( ACCEL_CHAR_NEW ) then FileNew( Sender )
          else if Key = Ord( ACCEL_CHAR_OPEN ) then FileOpen( Sender )
          else if Key = Ord( ACCEL_CHAR_SAVE ) then FileSave( Sender )
//        else if Key = Ord( ACCEL_CHAR_SAVE_AS ) then
          else if Key = Ord( ACCEL_CHAR_UNDO_EDIT ) then EditUndo( Sender )
          else if Key = Ord( ACCEL_CHAR_REDO_EDIT ) then EditRedoItemClick( Sender )
          else if Key = Ord( ACCEL_CHAR_CUT_TO_CLIPBOARD ) then EditCut( Sender )
          else if Key = Ord( ACCEL_CHAR_COPY_TO_CLIPBOARD ) then EditCopy( Sender )
          else if Key = Ord( ACCEL_CHAR_PASTE_FROM_CLIPBOARD ) then EditPaste( Sender )
          else if Key = Ord( ACCEL_CHAR_SELECT_ALL ) then CaptureEditSelectAllItemClick( Sender )
          else if Key = Ord( ACCEL_CHAR_CAPTURE_SCREEN ) then EditCapture( Sender )
          else;
          end;

 with Editor do // while dragging the grid, or entering left/top/right/bottom/column/row lines, nudge the mouse position into place by using the arrow keys
    if ( Step >= csBoard ) and
       ( Step <= csColumnsRows ) and
       ( not DrawingToolsEnabled ) then begin
       MouseCursorPosition := ToolsForm.CaptureImage1.ScreenToClient(Mouse.CursorPos);
       APoint := MouseCursorPosition;
       if      Key = VK_LEFT  then
               APoint.X := Max( 0, Min( ToolsForm.CaptureImage1.Width  - 2, ScaledPosition( Pred( NormalizedPosition( APoint.X ) ) ) ) )
       else if Key = VK_RIGHT then
               APoint.X := Max( 0, Min( ToolsForm.CaptureImage1.Width  - 2, ScaledPosition( Succ( NormalizedPosition( APoint.X ) ) ) ) )
       else if Key = VK_UP    then
               APoint.Y := Max( 0, Min( ToolsForm.CaptureImage1.Height - 2, ScaledPosition( Pred( NormalizedPosition( APoint.Y ) ) ) ) )
       else if Key = VK_DOWN  then
               APoint.Y := Max( 0, Min( ToolsForm.CaptureImage1.Height - 2, ScaledPosition( Succ( NormalizedPosition( APoint.Y ) ) ) ) );

       if      ( MouseCursorPosition.X <> APoint.X ) or ( MouseCursorPosition.Y <> APoint.Y ) then
               if      ( not HasSelection ) and
                       ( not MouseButtonDown ) and
                       ( Cursor >= ctLeftLine ) and ( Cursor <= ctRowLine ) then begin
                       Mouse.CursorPos := ToolsForm.CaptureImage1.ClientToScreen(APoint);
                       CaptureImage1MouseMove(Sender, [], APoint.X, APoint.Y);
                       end
               else if HasSelection and
                       MouseButtonDown and
                       (MouseButton=mbLeft) and
                       IsDragging and
                       ( Cursor = ctSelection ) then begin
                       Mouse.CursorPos := ToolsForm.CaptureImage1.ClientToScreen(APoint);
                       CaptureImage1MouseMove(Sender, [], APoint.X, APoint.Y);
                       end;
       end;

  if ( Shift=[] ) and (Key<>0) and ( not ( ToolsForm.ActiveControl is TEdit ) ) then
     for Index:=0 to Pred(ToolsForm.PageControl1.PageCount) do with ToolsForm.PageControl1 do
         if (Length(Pages[Index].Caption)>TAB_SHEET_SPACES) and
            (Key=Ord(Pages[Index].Caption[TAB_SHEET_SPACES+1])) and
            (Pages[Index]<>ActivePage) then begin
            ActivePage:=Pages[Index];
            ToolsForm.PageControl1Change(ToolsForm.PageControl1);
            break;
            end;

end;

function  CellToRect( Col__ , Row__, ColWidth__, RowHeight__ : Integer ) : TRect; // 0-based col, row
begin
  Result := Rect( Col__ * ColWidth__ , Row__ * RowHeight__, Succ( Col__ ) * ColWidth__ , Succ( Row__ ) * RowHeight__ );
end;

function  QuadrantToRect( Quadrant__, ColWidth__, RowHeight__ : Integer; const Rect__ : TRect  ) : TRect;
begin               // counter-clockwise quadrant order :  10
  Result := Rect__; //                                     23
  with Result do
    case Quadrant__ of
      0  : begin Inc( Left , ColWidth__ div 2 ); Dec( Bottom, RowHeight__ div 2 ); end;
      1  : begin Dec( Right, ColWidth__ div 2 ); Dec( Bottom, RowHeight__ div 2 ); end;
      2  : begin Dec( Right, ColWidth__ div 2 ); Inc( Top   , RowHeight__ div 2 ); end;
      3  : begin Inc( Left , ColWidth__ div 2 ); Inc( Top   , RowHeight__ div 2 ); end;
    end;
end;

function ClipRect(var Rect__:TRect; const ClippingRect__:TRect; ClearZeroAreaRect__ : Boolean):Boolean;
begin
  with Rect__ do begin
    if Left  <ClippingRect__.Left   then Left  :=ClippingRect__.Left;
    if Top   <ClippingRect__.Top    then Top   :=ClippingRect__.Top;
    if Right >ClippingRect__.Right  then Right :=ClippingRect__.Right;
    if Bottom>ClippingRect__.Bottom then Bottom:=ClippingRect__.Bottom;
    Result:=(Left<Right) and (Top<Bottom);
    if ( not Result ) and ClearZeroAreaRect__ then ZeroMemory( Addr( Rect__ ), SizeOf( Rect__ ) );
    end;
end;

procedure TCaptureForm.XYToColRow( X__, Y__ : Integer; var Col__, Row__ : Integer); // 0-based columns, rows
begin
  with Editor do
    if ( ToolsForm.ColWidthSpinEdit.Value > 0 ) and ( ToolsForm.RowHeightSpinEdit.Value > 0 ) then begin
       X__        := NormalizedPosition( X__ );
       Y__        := NormalizedPosition( Y__ );
       if   X__   >= ToolsForm.LeftSpinEdit.Value then
            Col__ := ( X__- ToolsForm.LeftSpinEdit.Value ) div ToolsForm.ColWidthSpinEdit.Value
       else Col__ := Pred( ( X__ - ToolsForm.LeftSpinEdit.Value ) div ToolsForm.ColWidthSpinEdit.Value );
       if   Y__   >= ToolsForm.TopSpinEdit.Value  then
            Row__ := ( Y__ - ToolsForm.TopSpinEdit.Value ) div ToolsForm.RowHeightSpinEdit.Value
       else Row__ := Pred( ( Y__ - ToolsForm.TopSpinEdit.Value  ) div ToolsForm.RowHeightSpinEdit.Value );
       end
   else begin
     Col__ := 0; Row__ := 0;
     end;
end;

procedure TCaptureForm.ColRowToNormalizedXY( Col__,Row__:Integer; var X__,Y__:Integer); // 0-based columns, rows
begin
  X__ := ToolsForm.LeftSpinEdit.Value + ( Col__ * ToolsForm.ColWidthSpinEdit .Value );
  Y__ := ToolsForm.TopSpinEdit .Value + ( Row__ * ToolsForm.RowHeightSpinEdit.Value );
end;

procedure TCaptureForm.ColRowToXY( Col__,Row__:Integer; var X__,Y__:Integer); // 0-based columns, rows
begin
  X__ := ScaledPosition( ToolsForm.LeftSpinEdit.Value + ( Col__ * ToolsForm.ColWidthSpinEdit .Value ) );
  Y__ := ScaledPosition( ToolsForm.TopSpinEdit .Value + ( Row__ * ToolsForm.RowHeightSpinEdit.Value ) );
end;

function  TCaptureForm.ColRowToRect( Col__, Row__ : Integer) : TRect; // 0-based columns, rows
begin
  with Result do begin
    ColRowToXY( Col__, Row__, Left, Top );
    Right  := Left + ScaledSize( ToolsForm.ColWidthSpinEdit.Value );
    Bottom := Top  + ScaledSize( ToolsForm.RowHeightSpinEdit.Value );
    end;
end;

function  TCaptureForm.ScaledPosition( Value__ : Integer ) : Integer;
begin // returns the value scaled according to the current image view zoom factor
//Result  := Max( 0, ( ( Value__ * Editor.Zoom ) div 100 ) - ( Editor.Zoom div 200 ) ); // "- ( Editor.Zoom div 200 )": pixel center correction
  Result  := ( ( Value__ * Editor.Zoom ) div 100 ) - ( Editor.Zoom div 200 ); // "- ( Editor.Zoom div 200 )": pixel center correction
end;

function  TCaptureForm.ScaledSize( Value__ : Integer ) : Integer;
begin // returns the value scaled according to the current image view zoom factor
  Result  := ( Value__ * Editor.Zoom ) div 100;
end;

function  TCaptureForm.NormalizedPosition( Value__ : Integer ) : Integer;
begin // returns the normalized value according to the current image view zoom factor
  Result := ( ( ( Value__ + ( Editor.Zoom div 200 ) ) * 100 ) div Editor.Zoom ); // "+ ( Editor.Zoom div 200 )": pixel center correction
end;

function  TCaptureForm.NormalizedSize( Value__ : Integer ) : Integer;
begin // returns the normalized value according to the current image view zoom factor
  Result := ( Value__  * 100 ) div Editor.Zoom;
end;

function  TCaptureForm.NormalizedRect( const Rect__ :TRect ): TRect;
begin
  if Editor.Zoom = 100 then
     Result := Rect__
  else with Editor do with Result do begin
     Left   := NormalizedPosition( Rect__.Left   );
     Top    := NormalizedPosition( Rect__.Top    );
     Right  := NormalizedPosition( Rect__.Right  );
     Bottom := NormalizedPosition( Rect__.Bottom );
     end;
end;

function  TCaptureForm.ScaledRect( const Rect__ :TRect ): TRect;
begin
  if Editor.Zoom = 100 then
     Result := Rect__
  else with Editor do with Result do begin
     Left   := ScaledPosition( Rect__.Left   );
     Top    := ScaledPosition( Rect__.Top    );
     Right  := ScaledPosition( Rect__.Right  );
     Bottom := ScaledPosition( Rect__.Bottom );
     end;
end;

function  TCaptureForm.NormalizedBoardRect : TRect;
begin
  Result := Rect( ToolsForm.LeftSpinEdit.Value, ToolsForm.TopSpinEdit.Value, ToolsForm.LeftSpinEdit.Value + ToolsForm.WidthSpinEdit.Value, ToolsForm.TopSpinEdit.Value + ToolsForm.HeightSpinEdit.Value );
end;

function  TCaptureForm.ScaledBoardRect : TRect;
begin
  Result := ScaledRect( NormalizedBoardRect );
end;

function  TCaptureForm.RectangleClampedToColumnsRows( const Rect__ : TRect; Columns__, Rows__ : Integer; var NewRect__ : TRect ) : Boolean;
var X, Y : Integer;
begin // returns 'True' if the rectangle needs clamping to match the number of columns and rows
  if        Columns__ >  0 then
            X         := RectWidth ( Rect__ ) mod Columns__
  else      X         := 0;
  if        Rows__    >  0 then
            Y         := RectHeight( Rect__ ) mod Rows__
  else      Y         := 0;
  NewRect__           := Rect__;
  Dec( NewRect__.Right , X );
  Dec( NewRect__.Bottom, Y );
  Result              := ( X <> 0 ) or ( Y <> 0 );
end;

function  TCaptureForm.CheckBoard( var Board__ : TCaptureBoard; Bitmap__, BackgroundBitmap__ : TBitmap; AutomaticCompletion__ : Boolean;
                                   var BoardRect__ : TRect;
                                   var OpenSides__ : Boolean ) : Boolean; // if 'OpenSides__' is 'True', then at least one of the sides isn't closed by walls or boxes at goals
const SIGNIFICANT_BOARD_OBJECTS : TCaptureBoardSquareItemSet = [ bsiWall, bsiBox, bsiGoal, bsiPlayer ]; // significant board objects, i.e., plain floor squares don't count
var Col, Row, PlayerCount, UnreachableFloorCount : Integer; First, Last, Items : TCaptureBoardSquareItemSet;

  function CalculateFloorSquares( const PlayerPos__ : TPoint ) : TPoint;
  // the floor squares are the player's reachable squares when there are no
  // boxes on the board;
  type
    TQueueItem =packed record Square : TPoint; end;
    TQueue = record
      Bottom, Top : Integer;
      Items : array[ 0 .. CAPTURE_MAX_BOARD_SIZE + 1 ] of TQueueItem;
    end;
  var
    Col, Row : Integer;
    PlayerSquare, NeighborSquare:TPoint;
    Direction:TDirection;
    Queue:TQueue;
  begin
    Result.X := Succ( PlayerPos__.X ); Result.Y := Succ( PlayerPos__.Y ); // 1-based coordinates
    if Timestamp < High( Timestamp ) - 10 then
       Inc( Timestamp )
    else begin
       Timestamp := 1;
       ZeroMemory( Addr( Timestamps ), SizeOf( Timestamps ) );
       // make a border around the board
       for Col := 0 to Succ( Board__.Width  ) do begin
           Timestamps[ Col, 0                      ] := High( Timestamp );
           Timestamps[ Col, Succ( Board__.Height ) ] := High( Timestamp );
           end;
       for Row := 0 to Succ( Board__.Height ) do begin
           Timestamps[ 0, Row                      ] := High( Timestamp );
           Timestamps[ Succ( Board__.Width ), Row  ] := High( Timestamp );
           end;
       end;

    Queue.Bottom := 0;
    if   ( PlayerPos__.X >= 0 ) and ( PlayerPos__.Y >= 0 ) then begin
         Queue.Top := 1; Queue.Items[ 1 ].Square := Result;
         TimeStamps[ Result.X, Result.Y ] := Timestamp;
         end
    else Queue.Top := 0;

    while ( Queue.Bottom < Queue.Top ) do begin
      Inc( Queue.Bottom );
      PlayerSquare := Queue.Items[ Queue.Bottom ].Square;

      for Direction := Low( Direction ) to High( Direction ) do begin
          NeighborSquare.X := PlayerSquare.X + DIRECTION_XY[ Direction, ColAxis ];
          NeighborSquare.Y := PlayerSquare.Y + DIRECTION_XY[ Direction, RowAxis ];
          if ( TimeStamps[ NeighborSquare.X, NeighborSquare.Y] < TimeStamp ) and
             (not ( bsiWall in Board__.Squares[ NeighborSquare.X, NeighborSquare.Y ] ) ) then begin
             Inc( Queue.Top );
             Queue.Items[ Queue.Top ].Square := NeighborSquare;
             TimeStamps[ NeighborSquare.X, NeighborSquare.Y ] := TimeStamp;
             if (   NeighborSquare.Y <= Result.Y )
                and
                ( ( NeighborSquare.Y <  Result.Y )
                  or
                  ( NeighborSquare.X <  Result.X ) ) then
                  Result := NeighborSquare; // normalized player position
             end;
          end;
      end;
    Dec( Result.X ); Dec( Result.Y ); // normalized player position, 0-based coordinates
  end;

  function  IsWallOrBoxOnGoal( Items__ : TCaptureBoardSquareItemSet ) : Boolean;
  begin
    Result := ( bsiWall in Items__ ) or
              ( ( [ bsiBox, bsiGoal ] * Items__ ) = [ bsiBox, bsiGoal ] );
  end;

begin // CheckBoard
  with Board__ do begin

    BoxCount := 0;
    GoalCount := 0;
    PlayerCount := 0;
    UnreachableFloorCount := 0;
    BoardRect__ := Classes.Rect( Max( CAPTURE_MAX_BOARD_HEIGHT, MAX_BOARD_HEIGHT ),
                                 Max( CAPTURE_MAX_BOARD_WIDTH , MAX_BOARD_WIDTH  ),
                                 0,
                                 0 );
    OpenSides__ := False;

    Result := ( PlayerPos.X >= 0 ) and ( PlayerPos.Y >= 0 );
    if   Result then
         CalculateFloorSquares( PlayerPos )
    else Timestamp := High( Timestamp );

    for Col := 1 to Width do
        for Row := 1 to Height do begin
            Items           := Squares[ Col, Row ];
            if bsiBox       in Items then Inc( BoxCount        );
            if bsiGoal      in Items then Inc( GoalCount       );
            if bsiPlayer    in Items then Inc( PlayerCount     );
            if bsiWall      in Items then begin end;
            if Result then
               if      Timestamps[ Col, Row ] = Timestamp then // 'True': the pusher can reach the square
                       Result := Items <> [] // '[]': the user must explicitly mark all squares the pusher can reach, including empty floor squares; that's an easy way to ensure that the skin-capture has information about floor squares as well as the other cell types
               else if bsiFloor in Items then
                       Inc( UnreachableFloorCount );
            end;

    if AutomaticCompletion__ and Result and ( PlayerCount = 1 ) then
       if      ( BoxCount = Pred( GoalCount ) ) and ( bsiGoal in Squares[ Succ( PlayerPos.X ), Succ( PlayerPos.Y ) ] ) then begin // 'True': assume the pusher falsely has been identified as a "pusher-on-goal"
               Exclude( Squares[ Succ( PlayerPos.X ), Succ( PlayerPos.Y ) ], bsiGoal );
               Dec( GoalCount );
               end
       else if ( BoxCount = Succ( GoalCount ) ) and ( not ( bsiGoal in Squares[ Succ( PlayerPos.X ), Succ( PlayerPos.Y ) ] ) ) then begin // 'True': assume the pusher falsely has been identified as a "pusher-on-floor"
               Include( Squares[ Succ( PlayerPos.X ), Succ( PlayerPos.Y ) ], bsiGoal );
               Inc( GoalCount );
               end;

    Result := Result and ( BoxCount = GoalCount ) and ( PlayerCount = 1 );

    if ( Result or ( not Assigned( Bitmap__ ) ) )
       and
       ( UnreachableFloorCount <> 0 ) then begin // clean up; the user doesn't need to specify the contents of unreachable floor squares
       for Col := 1 to Width do
           for Row := 1 to Height do
               if ( bsiFloor in Squares[ Col, Row ] ) and
                  ( Timestamps[ Col, Row ] <> Timestamp ) then begin // 'True': the pusher cannot reach the floor square
                  Items               := Squares[ Col, Row ];
                  if bsiBox           in Items then Dec( BoxCount        );
                  if bsiGoal          in Items then Dec( GoalCount       );
                  if bsiPlayer        in Items then Dec( PlayerCount     );
                  Squares[ Col, Row ] := [];
                  if Assigned( Bitmap__ ) and Assigned( BackgroundBitmap__ ) then // 'True': display the cleansed floor squares
                     ShowSquare( Pred( Col ), Pred( Row ), Board__, Bitmap__, BackgroundBitmap__ ); // 'Pred()': 0-based coordinates
                  end;
       Result := ( BoxCount = GoalCount ) and ( PlayerCount = 1 );
       end;

    // calculate board boundaries
    for Col := 1 to Width do
        for Row := 1 to Height do
            if ( Squares[ Col, Row ] * SIGNIFICANT_BOARD_OBJECTS ) <> [] then begin
               if Col <  BoardRect__.Left   then BoardRect__.Left   := Col;
               if Col >  BoardRect__.Right  then BoardRect__.Right  := Col;
               if Row <  BoardRect__.Top    then BoardRect__.Top    := Row;
               if Row >  BoardRect__.Bottom then BoardRect__.Bottom := Row;
               end;

    repeat
      // check if some of the sides are open; a Sokoban reader may not be able
      // to read a level if there are open sides; for instance, Sokoban YASC
      // requires the left side to be closed;
      if OpenSides__ then begin //
         // this is the second time through the loop; drop the board trimming
         // and use the entire board;
         OpenSides__ := False;
         BoardRect__ := Rect( 1, 1, Width, Height );
         end;
      for Col := BoardRect__.Left to BoardRect__.Right do begin
          First := []; Last := [];
          for Row := BoardRect__.Top to BoardRect__.Bottom do begin
              Items                                    := Squares[ Col, Row ];
              if ( Items * SIGNIFICANT_BOARD_OBJECTS ) <> [] then begin
                 if First                              =  [] then
                    First                              := Items;
                 Last                                  := Items;
                 end;
              end;
          if not IsWallOrBoxOnGoal( First ) then OpenSides__ := True;
          if not IsWallOrBoxOnGoal( Last  ) then OpenSides__ := True;
          end;
      for Row := BoardRect__.Top to BoardRect__.Bottom do begin
          First := []; Last := [];
          for Col := BoardRect__.Left to BoardRect__.Right do begin
              Items                                    := Squares[ Col, Row ];
              if ( Items * SIGNIFICANT_BOARD_OBJECTS ) <> [] then begin
                 if First                              =  [] then
                    First                              := Items;
                 Last                                  := Items;
                 end;
              end;
          if not IsWallOrBoxOnGoal( First ) then OpenSides__ := True;
          if not IsWallOrBoxOnGoal( Last  ) then OpenSides__ := True;
          end;
    until ( not OpenSides__ ) or 
          IsEqualRects( BoardRect__, Rect( 1, 1, Width, Height ) ); // if there are open sides, then don't trim the board; trimming may remove empty squares needed by the player to move around on the board;

    if   BoardRect__.Right  > 0 then Inc( BoardRect__.Right )  // make right  exclusive
    else BoardRect__ := Classes.Rect( 0, 0, 0, 0 );
    if   BoardRect__.Bottom > 0 then Inc( BoardRect__.Bottom ) // make bottom exclusive
    else BoardRect__ := Classes.Rect( 0, 0, 0, 0 );

    if ( Editor.Step = csBoardSquares ) and Assigned( Bitmap__ ) then // 'Bitmap__': quick-and-dirty: no bitmap argument is interpreted as a flag telling that the function should have no side effects
       StatusText := Format( TEXT_FORMAT_BOXES_AND_GOALS_AND_PLAYERS, [ BoxCount, GoalCount, PlayerCount ] );
    //if Result then Clipboard.AsText := BoardAsText;
    end;
end;

function  TCaptureForm.BeginTransaction : Boolean;
begin
  with Editor.History do begin
    Result                       := TransactionIndex >= Low( Items ); // 'True': transaction already in progress
    if not Result then begin // start transaction
       StackTop                  := Succ( StackTop    ) mod High( Items );
       if Succ( StackTop ) mod High( Items ) =  StackBottom then
          StackBottom            := Succ( StackBottom ) mod High( Items ); // keep one item free in order to distinguish between an empty history and a full history
       TransactionIndex          := StackTop;
       Items[ TransactionIndex ] := BoardToText( Editor.Board, UNDEFINED_SQUARE_CHAR );
       Result                    := Length( Items[ TransactionIndex ] ) = Editor.Board.Width * Editor.Board.Height;
       if not Result then EndTransaction( False );
       end;
    end;
end;

function  TCaptureForm.EndTransaction( Commit__ : Boolean ) : Boolean;
begin
  Result := True;
  with Editor.History do
    if TransactionIndex          >= Low( Items ) then begin
       if Commit__ then begin
          ItemIndex              := StackTop;
          end
       else begin
          if   StackTop          >  Low( Items ) then
               Dec( StackTop )
         else StackTop           := Pred( High( Items ) );
         end;
       TransactionIndex          := Pred( Low( Editor.History.Items ) );
       end;
end;

procedure TCaptureForm.HideCursor(UpdateScreenCursor__:Boolean);
begin
  with Editor do begin
    if   (UpdateScreenCursor__) and (Screen.Cursor<>ToolsForm.CaptureImage1.Cursor) then
         Screen.Cursor:=ToolsForm.CaptureImage1.Cursor;
    if   MouseButtonDown and ( MouseButton = mbMiddle ) then // do nothing
    else MouseButtonDown:=False;
    SizeHandle:=ghNull; IsDragging:=False; IsSelectingANewArea:=False;
    HasSelection:=False;
    ShowCursor(Cursor, Rect( 0, 0, 0, 0 ) );
    if Editor.Step >= csBoardSquares then
       SetCaptureDrawingToolHint
    else
       if Editor.Cursor < ctLeftLine then begin
          if Editor.CompletedStep >= csBoard then
             Editor.CompletedStep := csImage;
          if Editor.Step          >  csBoard then
             Editor.Step          := csBoard;
          end;
    ShowStatus;
   end;
end;

procedure TCaptureForm.ShowCursor(Cursor__:TCaptureEditorCursorType; CursorRect__:TRect);
const
  ARROW_PIXEL_SIZE = 7;
  CURSOR_LINE_ARROW_DIRECTION : array[ TCaptureEditorCursorType ] of TDirection =
    ( Down, Down, Down, Right, Down, SokUtil_.Left, Up, SokUtil_.Left, Up );
var Col, Row, X, Y : Integer; R, ClipR: TRect;

  procedure ShowCursor(const CursorRect__:TRect; PenWidth__,Size__:Integer; PenColor__,ShadowColor__:TColor; Corners__:TCornerTypeSet);
  var i,dx,dy,PenWidth, ColCount, RowCount:Integer; Points:array[0..7] of TPoint;
  begin // precondition: the cursor-rectangle is a valid rectangle on the destination canvas
    with Editor do with ToolsForm.CaptureImage1.Picture.Bitmap.Canvas do begin
         //LockWindowUpdate(Handle);
         Brush.Style   := bsSolid;
         Pen.Color     := PenColor__;
         Pen.Mode      := PmCopy;
         Pen.Style     := psSolid;
         Pen.Width     := 1;
         PenWidth      := PenWidth__;
         if (ScaledSize(ToolsForm.ColWidthSpinEdit .Value) <= 2*PenWidth+2) or
            (ScaledSize(ToolsForm.RowHeightSpinEdit.Value) <= 2*PenWidth+2) then
            PenWidth :=1;

         with CursorRect__ do begin
           Points[0].X:=Left;                         Points[0].Y:=Top;                                 // top    left
           Points[1].X:=Right;                        Points[1].Y:=Points[0].Y;                         // top    right
//         if Cursor<> ctSelection then               Dec( Points[1].X );
           Points[2].X:=Points[0].X;                  Points[2].Y:=Bottom;                              // bottom left
//         if Cursor<> ctSelection then               Dec( Points[2].Y );
           Points[3].X:=Points[1].X;                  Points[3].Y:=Points[2].Y;                         // bottom right
           end;

         if Size__<>0 then begin
            dx:=Max(1,Min(PenWidth+Size__,Succ((Points[1].X-Points[0].X) div 2)));    // 'Succ':  'LineTo' draws up to but not including the destination point
            dy:=Max(1,Min(PenWidth+Size__,Succ((Points[2].Y-Points[0].Y) div 2)));
            end
         else begin
           dx:=Max(1,Succ((Points[1].X-Points[0].X) div 2)); // 'Succ':  'LineTo' draws up to but not including the destination point
           dy:=Max(1,Succ((Points[2].Y-Points[0].Y) div 2));
           end;

         if True then begin
            Pen.Color:=ShadowColor__;
            for i:=0 to 3 do with Points[i+4] do begin
                X:=Succ(Points[i].X); Y:=Succ(Points[i].Y);
                end;

            for i:=1 to PenWidth do begin
                // draw the cursor manually one line at a time with pen.width = 1;
                // that way the program doesn't depend on drawing conventions when pen.width <> 1
                if ctTopLeft in Corners__ then with Points[4] do begin {Inc(X); Inc(Y);} MoveTo(X,Y); LineTo(X+dx,Y); MoveTo(X,Y); LineTo(X,Y+dy); Inc(X); Inc(Y); end; // top    left
                if ctTopLeft in Corners__ then with Points[5] do begin {Dec(X); Inc(Y);} MoveTo(X,Y); LineTo(X-dx,Y); MoveTo(X,Y); LineTo(X,Y+dy); Dec(X); Inc(Y); end; // top    right
                if ctTopLeft in Corners__ then with Points[6] do begin {Inc(X); Dec(Y);} MoveTo(X,Y); LineTo(X+dx,Y); MoveTo(X,Y); LineTo(X,Y-dy); Inc(X); Dec(Y); end; // bottom left
                if ctTopLeft in Corners__ then with Points[7] do begin {Dec(X); Dec(Y);} MoveTo(X,Y); LineTo(X-dx,Y); MoveTo(X,Y); LineTo(X,Y-dy); Dec(X); Dec(Y); end; // bottom right
                Dec(dx); Dec(dy);
                end;

            Inc(dx,PenWidth); Inc(dy,PenWidth); // restore values so they're ready for drawing the cursor
            Pen.Color:=PenColor__;
            end;

         for i:=1 to PenWidth do begin
             // draw the cursor manually with pen.width = 1;
             // that way the program doesn't depend on drawing conventions when pen.width <> 1
             with Points[0] do begin {Inc(X); Inc(Y);} MoveTo(X,Y); LineTo(X+dx,Y); MoveTo(X,Y); LineTo(X,Y+dy); Inc(X); Inc(Y); end; // top    left
             with Points[1] do begin {Dec(X); Inc(Y);} MoveTo(X,Y); LineTo(X-dx,Y); MoveTo(X,Y); LineTo(X,Y+dy); Dec(X); Inc(Y); end; // top    right
             with Points[2] do begin {Inc(X); Dec(Y);} MoveTo(X,Y); LineTo(X+dx,Y); MoveTo(X,Y); LineTo(X,Y-dy); Inc(X); Dec(Y); end; // bottom left
             with Points[3] do begin {Dec(X); Dec(Y);} MoveTo(X,Y); LineTo(X-dx,Y); MoveTo(X,Y); LineTo(X,Y-dy); Dec(X); Dec(Y); end; // bottom right
             Dec(dx); Dec(dy);
             end;

         if ( Editor.Cursor =  ctSelection ) and
            ( ( Editor.Step >= csColumnsRows ) or ( Editor.CompletedStep >= csColumnsRows ) ) then begin
            ColCount := ToolsForm.ColumnsSpinEdit.Value; RowCount := ToolsForm.RowsSpinEdit.Value;
            if ColCount + RowCount <> 0 then begin
               dx := RectWidth ( CursorRect__ ) div Max( 1, ColCount );
               dy := RectHeight( CursorRect__ ) div Max( 1, RowCount );
               Pen.Color:=ShadowColor__;
               if NormalizedSize( dx ) >= MIN_BOARD_SQUARE_SIZE_PIXELS then
                  for i := 1 to Pred( ColCount ) do begin
                      MoveTo( CursorRect__.Left + i * DX + 1, CursorRect__.Top + 1 ); LineTo( CursorRect__.Left + i * DX + 1, CursorRect__.Bottom );
                      end;
               if NormalizedSize( dy ) >= MIN_BOARD_SQUARE_SIZE_PIXELS then
                  for i := 1 to Pred( RowCount ) do begin
                      MoveTo( CursorRect__.Left + 1, CursorRect__.Top + i * DY + 1 ); LineTo( CursorRect__.Right,  CursorRect__.Top + i * DY + 1 );
                      end;
               Pen.Color:=PenColor__;
               if NormalizedSize( dx ) >= MIN_BOARD_SQUARE_SIZE_PIXELS then
                  for i := 1 to Pred( ColCount ) do begin
                      MoveTo( CursorRect__.Left + i * dx    , CursorRect__.Top     ); LineTo( CursorRect__.Left + i * dx    , CursorRect__.Bottom );
                      end;
               if NormalizedSize( dy ) >= MIN_BOARD_SQUARE_SIZE_PIXELS then
                  for i := 1 to Pred( RowCount ) do begin
                      MoveTo( CursorRect__.Left    , CursorRect__.Top + i * dy     ); LineTo( CursorRect__.Right,  CursorRect__.Top + i * dy     );
                      end;
               end;
            end;
         //LockWindowUpdate(0);
         end;
   end;

   procedure ShowLine( var CursorRect__ : TRect; PenColor__,ShadowColor__:TColor; ArrowDirection__ : TDirection );
   var Index, Index2 : Integer; R : TRect;

    procedure DrawArrow( X__, Y__ : Integer );
    begin
      BitMap_.DrawArrow( ToolsForm.CaptureImage1.Picture.Bitmap.Canvas,
                         X__, Y__, ARROW_PIXEL_SIZE, ArrowDirection__,
                         PenColor__,ShadowColor__ );
    end;

    procedure DrawLine( X1__, Y1__, X2__, Y2__ : Integer; CompleteShadowLine__ : Boolean );
    begin
      with ToolsForm.CaptureImage1.Picture.Bitmap do with Canvas do begin
        Pen.Color := ShadowColor__;
        MoveTo( Succ( X1__ ), Succ( Y1__ ) );
        if             CompleteShadowLine__ then
                       LineTo( Succ( X2__ ), Succ( Y2__ ) )
        else if        X1__ = X2__ then
                       LineTo( Succ( X1__ ), Y2__ )
             else if   Y1__ = Y2__ then
                       LineTo( X2__, Succ( Y1__ ) )
                  else LineTo( Succ( X2__ ), Succ( Y2__ ) ); // last shadow pixel suppression not implemented
        Pen.Color := PenColor__;
        MoveTo( X1__, Y1__ );
        LineTo( X2__, Y2__ );
        end;
    end;

   begin // 'ShowLine'
     with ToolsForm.CaptureImage1.Picture.Bitmap.Canvas do with CursorRect__ do begin
       Pen.Color     := PenColor__;
       Pen.Mode      := PmCopy;
       Pen.Style     := psSolid;
       Pen.Width     := 1;

       CursorRect__  := ScaledBoardRect;
       R             := CursorRect__;
       if Editor.Cursor = ctRightLine  then Left   := Right;
       if Editor.Cursor = ctBottomLine then Top    := Bottom;
       if Left          = Right        then Right  := ToolsForm.CaptureImage1.Picture.Bitmap.Width  - 2;
       if Top           = R.Bottom     then Bottom := ToolsForm.CaptureImage1.Picture.Bitmap.Height - 2;

       if R.Left     =  R.Right  then R.Right  := ToolsForm.CaptureImage1.Picture.Bitmap.Width  - 2;
       if R.Top      =  R.Bottom then R.Bottom := ToolsForm.CaptureImage1.Picture.Bitmap.Height - 2;
       with R do begin
         if Editor.Cursor   >= ctLeftLine   then DrawLine( Left , Top   , Left          , Bottom, True );
         if Editor.Cursor   >= ctTopLine    then DrawLine( Left , Top   , Right         , Top   , True );
         if Editor.Cursor   >= ctRightLine  then DrawLine( Right, Top   , Right         , Bottom, True );
         if Editor.Cursor   >= ctBottomLine then DrawLine( Left , Bottom, Succ( Right ) , Bottom, True );
         if ( Editor.Cursor >= ctColumnLine ) and ( ToolsForm.ColWidthSpinEdit.Value > 0 ) then
            for Index := 1 to Pred( ToolsForm.ColumnsSpinEdit.Value ) do
                DrawLine( Left + ( Index * ScaledSize( ToolsForm.ColWidthSpinEdit.Value ) ), Top, Left + ( Index * ScaledSize( ToolsForm.ColWidthSpinEdit.Value ) ), Bottom, False );
         if ( Editor.Cursor >= ctColumnLine ) and ( ToolsForm.RowHeightSpinEdit.Value > 0 ) then
            for Index := 1 to Pred( ToolsForm.RowsSpinEdit.Value ) do
                for Index2  := 0 to Pred( ToolsForm.ColumnsSpinEdit.Value ) do
                    DrawLine( Left + ( Index2         * ScaledSize( ToolsForm.ColWidthSpinEdit.Value ) ),
                              Top  + ( Index          * ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) ),
                              Left + ( Succ( Index2 ) * ScaledSize( ToolsForm.ColWidthSpinEdit.Value ) ),
                              Top  + ( Index          * ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) ),
                              False );
         end;

       if Editor.Cursor = ctColumnLine then begin
          with ToolsForm.CaptureImage1.ScreenToClient( Mouse.CursorPos ) do
            if   ( X <= Left + ( ( Right - Left ) div 2 ) ) or ( Y < 0 ) then
                 Inc( Left, ScaledSize( ToolsForm.ColWidthSpinEdit.Value ) )
            else Left := Right - ScaledSize( ToolsForm.ColWidthSpinEdit.Value );
          //DrawLine( Left, Top, Left, Bottom, False );
          end;
       if Editor.Cursor = ctRowLine then begin
          with ToolsForm.CaptureImage1.ScreenToClient( Mouse.CursorPos ) do
            if   ( Y <= Top + ( ( Bottom - Top  ) div 2 ) ) or ( X < 0 ) then
                 Inc( Top, ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) )
            else Top := Bottom - ScaledSize( ToolsForm.RowHeightSpinEdit.Value );
          //DrawLine( Left , Top, Right, Top, False );
          //for Index := 0 to Pred( ColumnsSpinEdit.Value ) do
          //    DrawLine( Left + ( Index * ScaledSize( ColWidthSpinEdit.Value ) ), Top, Left + ( Succ( Index ) * ScaledSize( ColWidthSpinEdit.Value ) ), Top, False );
          end;

       with CursorRect__ do
         case ArrowDirection__ of
           Up              : DrawArrow( Left + ( ( Right - Left ) div 2 ), Top + 1 );
           SokUtil_.Left   : DrawArrow( Left + 1, Top + ( ( Bottom - Top ) div 2 ) );
           Down            : DrawArrow( Left + ( ( Right - Left ) div 2 ), Top - 1 );
           SokUtil_.Right  : DrawArrow( Left - 1, Top + ( ( Bottom - Top ) div 2 ) );
           end;
       end;
   end;

begin // ShowCursor
  with Editor do with CursorRect do begin
    if   Cursor < ctLeftLine then
         ClipR := Classes.Rect(0,0,ToolsForm.CaptureImage1.Picture.Bitmap.Width-2,ToolsForm.CaptureImage1.Picture.Bitmap.Height-2)
    else ClipR := Classes.Rect(0,0,ToolsForm.CaptureImage1.Picture.Bitmap.Width  ,ToolsForm.CaptureImage1.Picture.Bitmap.Height  );

    if RectWidth( CursorRect ) > 0 then begin
       if Cursor < ctLeftLine then
          R := Classes.Rect( Left, Top, Right + 2 , Bottom + 2) // '+2' : hardwired cursor width
       else begin
          R := Classes.Rect( Max( 0, Min( Left, ScaledPosition( ToolsForm.LeftSpinEdit.Value ) ) - ARROW_PIXEL_SIZE - 2 ), 0,  ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height );
          end;
       ToolsForm.CaptureImage1.Picture.Bitmap.Canvas.CopyRect( R, BackgroundBitmap.Canvas, R ); // restore background
       if Editor.Step >= csBoardSquares then begin
          XYToColRow( CursorRect.Left, CursorRect.Top, Col, Row );
          if ( Col >= 0 ) and ( Col < Editor.Board.Width ) and
             ( Row >= 0 ) and ( Row < Editor.Board.Height ) then
             for Y := -1 to 1 do
                 for X := -1 to 1 do
                     ShowSquare( Col + X, Row + Y, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap ); // repair cursor lines in neighbor squares and update any modified seamless wall squares in the neighborhood
          end;
       end;
    Cursor:=Cursor__;
    if Editor.Zoom <> 100 then begin
       // sub-zero coordinates may be caused by the pixel center correction in
       // 'ScaledPosition()' and 'NormalizedPosition()'; if necessary, move the
       // rectangle so it hasn't sub-zero top-left coordinates;
       if CursorRect__.Left < 0 then CursorRect__ := RectPlusOffset( CursorRect__, -CursorRect__.Left, 0 );
       if CursorRect__.Top  < 0 then CursorRect__ := RectPlusOffset( CursorRect__, 0, -CursorRect__.Top );
       end;
    CursorRect:=CursorRect__;
    //CellToPos(CursorRect.Left,CursorRect.Top,CursorRectPixels.Left,CursorRectPixels.Top);
    //CellToPos(CursorRect.Right,CursorRect.Bottom,CursorRectPixels.Right,CursorRectPixels.Bottom);
    ClipRect(CursorRect,ClipR, Cursor < ctLeftLine );

    if ( Editor.Step >= csBoardSquares ) and ( Editor.Step <= csSavePuzzle ) then begin
       XYToColRow( CursorRect__.Left, CursorRect__.Top, Col, Row );
       if ( Col >= 0 ) and ( Col < Editor.Board.Width ) and
          ( Row >= 0 ) and ( Row < Editor.Board.Height ) and
          ( Editor.Board.Squares[ Succ( Col), Succ(Row) ] <> [] ) then
          ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
       end;

//  Caption:=Format('[%d:%d]',[CursorRect.Left,CursorRect.Top]);

    if ( Cursor < ctLeftLine ) or ( Cursor > ctRowLine ) then begin
       if (Left < Right ) and
          (Top  < Bottom) then begin
               if ( Left <= Right - 2 ) and ( Top <= Bottom - 2 ) then
                  case Cursor of
                    ctCell       : ShowCursor(   CursorRect,1,4,Editor.GridColor ,Editor.GridShadowColor,[ctTopLeft,ctTopRight,ctBottomLeft,ctBottomRight]);
                    ctEraser     : with ToolsForm.Editor.Cursors[Tools_.ctEraser] do
                                     ShowCursor (CursorRect,1,4,PenColor,         ShadowColor,           [ctTopLeft,ctTopRight,ctBottomLeft,ctBottomRight]);
                    ctSelection  : if HasSelection then
                                      ShowCursor(CursorRect,1,0,Editor.GridColor ,Editor.GridShadowColor,[ctTopLeft,ctTopRight,ctBottomLeft,ctBottomRight]);
                  end;
          end
       else
          if ( Left < Right ) or ( Top < Bottom ) then
             if   Editor.Cursor < ctLeftLine then
                  if   Editor.Step <= Pred( csBoardSquares ) then
                       ClearSelection
                  else HideCursor(True)
             else HideCursor(True);
       end
    else
       if ( RectWidth( CursorRect          ) > 0 ) or ( RectHeight( CursorRect          ) > 0 ) or
          ( RectWidth( NormalizedBoardRect ) = 0 ) or ( RectHeight( NormalizedBoardRect ) = 0 ) then // '0': grid data incomplete at this point
          ShowLine( CursorRect, Editor.GridColor, Editor.GridShadowColor, CURSOR_LINE_ARROW_DIRECTION[ Cursor ] );
    end;
end;

function  TCaptureForm.NeighborWalls( Col__, Row__ : Integer; const Board__ : TCaptureBoard ) : Integer;
begin // directions up, right, down, left = 1, 2, 4, 8 respectively; for a wall square, the sum matches the element index in the built-in editor skin
  with Board__ do begin
    Result := WALL_NONE;
    if ( Row__ >       1      ) and ( bsiWall in Squares[       Col__  , Pred( Row__ ) ] ) then Inc( Result, WALL_UP );
    if ( Col__ <       Width  ) and ( bsiWall in Squares[ Succ( Col__ ),       Row__   ] ) then Inc( Result, WALL_RIGHT );
    if ( Row__ <       Height ) and ( bsiWall in Squares[       Col__  , Succ( Row__ ) ] ) then Inc( Result, WALL_DOWN );
    if ( Col__ >       1      ) and ( bsiWall in Squares[ Pred( Col__ ),       Row__   ] ) then Inc( Result, WALL_LEFT );
    end;
end;

procedure TCaptureForm.ShowSquare(Col__,Row__:Integer; const Board__ : TCaptureBoard; Bitmap__, BackgroundBitmap__ : TBitmap ); // 0-based cell-coordinate columns and rows
var Index, X, Y : Integer; DestRect, SourceRect : TRect;

  function TileIndexToRect( Index__ : Integer ): TRect;
  begin
    if   ( Index__ >= 0 ) and ( Index__ < Editor.EditorSkinTileCount ) then with Result do begin
         Left      := Index__ * ( Editor.EditorSkinBitmap.Width div Editor.EditorSkinTileCount );
         Top       := 0;
         Right     := Left + ScaledSize( ToolsForm.ColWidthSpinEdit.Value );
         Bottom    := Editor.EditorSkinBitmap.Height;
         end
    else Result    := Rect( 0, 0, 0, 0 );
  end;

  procedure ShowWallCaps( Col__, Row__ : Integer);
  var X, Y : Integer; R : TRect;
  begin
    Inc( Col__ ); Inc( Row__ );
    if ( bsiWall in Board__.Squares[ Col__, Row__ ] ) // 'True': a wall square; check diagonals
       and
       ( ( Editor.Step < csSaveSkin ) or ToolsForm.WallCapCheckBox.Checked ) then
       for Y := Pred( Row__ ) to Succ( Row__ ) do
           if ( Y <> Row__ ) and ( Y > 0 ) and ( Y <= Width ) then
              for X := Pred( Col__ ) to Succ( Col__ ) do
                  if ( X <> Col__ ) and ( X > 0 ) and ( X <= Width ) and
                     ( bsiWall in Board__.Squares[ X    , Y     ] ) and
                     ( bsiWall in Board__.Squares[ X    , Row__ ] ) and
                     ( bsiWall in Board__.Squares[ Col__, Y     ] ) then begin
                     if Editor.Step < csSaveSkin then
                        ColRowToXY( Pred( Col__ ), Pred( Row__ ), R.Left, R.Top )
                     else begin
                        R.Left := ScaledPosition( Col__ * ToolsForm.ColWidthSpinEdit .Value );
                        R.Top  := ScaledPosition( Row__ * ToolsForm.RowHeightSpinEdit.Value );
                        end;
                     Inc( R.Left, ( ( X - Col__ ) * ScaledSize( ToolsForm.ColWidthSpinEdit.Value  ) ) div 2 );
                     Inc( R.Top , ( ( Y - Row__ ) * ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) ) div 2 );
                     R.Right  := R.Left + ScaledSize( ToolsForm.ColWidthSpinEdit.Value );
                     R.Bottom := R.Top  + ScaledSize( ToolsForm.RowHeightSpinEdit.Value );
                     Bitmap__.Canvas.CopyRect( R, Editor.EditorSkinBitmap.Canvas, TileIndexToRect( 16 ) );
                     end;
  end;

  procedure ShowWall( Col__, Row__ : Integer; DirectionSet__ : TDirectionSet; SourceRect__, DestRect__ : TRect );

    function Take( Side__ : TDirection; const Rect__ : TRect ) : TRect;
    begin // takes a slice of the rectangle according to the size of the wall cut for the given side
      Result := Rect__;
      with Result do
        case Side__ of
          Up                 : Bottom := Top    + ScaledSize( ToolsForm.OuterWallCutTopSpinEdit   .Value );
          SokUtil_.Left      : Right  := Left   + ScaledSize( ToolsForm.OuterWallCutLeftSpinEdit  .Value );
          Down               : Top    := Bottom - ScaledSize( ToolsForm.OuterWallCutBottomSpinEdit.Value );
          SokUtil_.Right     : Left   := Right  - ScaledSize( ToolsForm.OuterWallCutRightSpinEdit .Value );
        end;
    end;

    function Drop( Side__ : TDirection; const Rect__ : TRect ) : TRect;
    begin // drops a slice of the rectangle according to the size of the wall cut for the given side
      Result := Rect__;
      with Result do
        case Side__ of
          Up                 : Inc(      Top    , ScaledSize( ToolsForm.OuterWallCutTopSpinEdit   .Value ) );
          SokUtil_.Left      : Inc(      Left   , ScaledSize( ToolsForm.OuterWallCutLeftSpinEdit  .Value ) );
          Down               : Dec(      Bottom , ScaledSize( ToolsForm.OuterWallCutBottomSpinEdit.Value ) );
          SokUtil_.Right     : Dec(      Right  , ScaledSize( ToolsForm.OuterWallCutRightSpinEdit .Value ) );
        end;
    end;

  begin // 'ShowWall'
    with Board__ do begin
      if ( SokUtil_.Left  in DirectionSet__ ) and ( Squares[ Pred( Col__ ),       Row__   ] = [] ) then begin
         Inc( SourceRect__.Left  , ScaledSize( ToolsForm.OuterWallCutLeftSpinEdit  .Value ) );
         Inc( DestRect__  .Left  , ScaledSize( ToolsForm.OuterWallCutLeftSpinEdit  .Value ) );
         Exclude( DirectionSet__, SokUtil_.Left );
         end;
      if ( Up    in DirectionSet__ ) and ( Squares[       Col__  , Pred( Row__ ) ] = [] ) then begin
         Inc( SourceRect__.Top   , ScaledSize( ToolsForm.OuterWallCutTopSpinEdit   .Value ) );
         Inc( DestRect__  .Top   , ScaledSize( ToolsForm.OuterWallCutTopSpinEdit   .Value ) );
         Exclude( DirectionSet__, Up );
         end;
      if ( Right in DirectionSet__ ) and ( Squares[ Succ( Col__ ),       Row__   ] = [] ) then begin
         Dec( SourceRect__.Right , ScaledSize( ToolsForm.OuterWallCutRightSpinEdit .Value ) );
         Dec( DestRect__  .Right , ScaledSize( ToolsForm.OuterWallCutRightSpinEdit .Value ) );
         Exclude( DirectionSet__, Right );
         end;
      if ( Down  in DirectionSet__ ) and ( Squares[       Col__  , Succ( Row__ ) ] = [] ) then begin
         Dec( SourceRect__.Bottom, ScaledSize( ToolsForm.OuterWallCutBottomSpinEdit.Value ) );
         Dec( DestRect__  .Bottom, ScaledSize( ToolsForm.OuterWallCutBottomSpinEdit.Value ) );
         Exclude( DirectionSet__, Down );
         end;
      if ( ( DirectionSet__ * [ SokUtil_.Left , Up   ] ) = [ SokUtil_.Left , Up   ] ) and ( Squares[ Pred( Col__ ), Pred( Row__ )  ] = [] ) then begin
         ShowWall( Col__, Row__, DirectionSet__ - [ Up  , Right ], Drop( Up  , Take( SokUtil_.Left , SourceRect__ ) ), Drop( Up  , Take( SokUtil_.Left , DestRect__ ) ) );
         SourceRect__ := Drop( SokUtil_.Left , SourceRect__ ); DestRect__   := Drop( SokUtil_.Left , DestRect__ ); Exclude( DirectionSet__, SokUtil_.Left );
         end;
      if ( ( DirectionSet__ * [ SokUtil_.Left , Down ] ) = [ SokUtil_.Left , Down ] ) and ( Squares[ Pred( Col__ ), Succ( Row__ )  ] = [] ) then begin
         ShowWall( Col__, Row__, DirectionSet__ - [ Down, Right ], Drop( Down, Take( SokUtil_.Left , SourceRect__ ) ), Drop( Down, Take( SokUtil_.Left , DestRect__ ) ) );
         SourceRect__ := Drop( SokUtil_.Left , SourceRect__ ); DestRect__   := Drop( SokUtil_.Left , DestRect__ ); Exclude( DirectionSet__, SokUtil_.Left );
         end;
      if ( ( DirectionSet__ * [ Right, Down ] ) = [ Right, Down ] ) and ( Squares[ Succ( Col__ ), Succ( Row__ )  ] = [] ) then begin
         ShowWall( Col__, Row__, DirectionSet__ - [ Down, SokUtil_.Left  ], Drop( Down, Take( Right, SourceRect__ ) ), Drop( Down, Take( Right, DestRect__ ) ) );
         SourceRect__ := Drop( Right, SourceRect__ ); DestRect__   := Drop( Right, DestRect__ ); Exclude( DirectionSet__, Right );
         end;
      if ( ( DirectionSet__ * [ Right, Up   ] ) = [ Right, Up   ] ) and ( Squares[ Succ( Col__ ), Pred( Row__ )  ] = [] ) then begin
         ShowWall( Col__, Row__, DirectionSet__ - [ Up  , SokUtil_.Left  ], Drop( Up  , Take( Right, SourceRect__ ) ), Drop( Up  , Take( Right, DestRect__ ) ) );
         SourceRect__ := Drop( Right, SourceRect__ ); DestRect__   := Drop( Right, DestRect__ ); Exclude( DirectionSet__, Right );
         end;

      Bitmap__.Canvas.CopyRect( DestRect__, Editor.EditorSkinBitmap.Canvas, SourceRect__ );
      end;
  end;

begin // 'ShowSquare'
  if ( Col__ >= 0) and ( Col__ < Board__.Width  ) and
     ( Row__ >= 0) and ( Row__ < Board__.Height ) then begin
     if Editor.Step < csSaveSkin then
        ColRowToXY( Col__, Row__, X, Y )
     else begin
        X := ScaledPosition( Succ( Col__ ) * ToolsForm.ColWidthSpinEdit .Value );
        Y := ScaledPosition( Succ( Row__ ) * ToolsForm.RowHeightSpinEdit.Value );
        end;
     DestRect:=Classes.Rect( X, Y, X + ScaledSize( ToolsForm.ColWidthSpinEdit.Value ), Y + ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) );
     if ClipRect(DestRect,Classes.Rect(0,0,Bitmap__.Width-2,Bitmap__.Height-2), True) and
        Assigned( Editor.EditorSkinBitmap ) and
        (Editor.EditorSkinTileCount > 0 ) then
        with Bitmap__.Canvas do begin
          Index := BoardSquareToSkinTileIndex( Succ( Col__ ) , Succ( Row__ ), Board__ );
          if   Index >= 0 then begin // show square contents
               SourceRect := TileIndexToRect( Index );
               if        ( Editor.Step >= csSaveSkin ) and
                         ( bsiWall in Board__.Squares[ Succ( Col__ ), Succ( Row__ ) ] ) then
                         ShowWall( Succ( Col__ ), Succ( Row__), ALL_DIRECTIONS, SourceRect, DestRect ) // show wall square with outer wall cuts
               else if   ToolsForm.CaptureSettingsMenuItemObjectsOnABlackBackground.Checked and
                         ( Editor.Step >= csSaveSkin ) and
                         ( ( [ bsiBox, bsiPlayer ] * Board__.Squares[ Succ( Col__ ), Succ( Row__ ) ] ) <> [] ) then begin
                         if   bsiGoal in Board__.Squares[ Succ( Col__ ), Succ( Row__ ) ] then
                              CopyRect( DestRect, Editor.EditorSkinBitmap.Canvas, TileIndexToRect( 20 ) )
                         else CopyRect( DestRect, Editor.EditorSkinBitmap.Canvas, TileIndexToRect( 17 ) );
                         BitBltTransparent( Bitmap__, DestRect.Left, DestRect.Top, Editor.EditorSkinBitmap, SourceRect, TColor( ToolsForm.CaptureSettingsMenuItemObjectsOnABlackBackground.Tag ), 5, clBlack, clBlack, clBlack, clBlack );
                         end
                    else CopyRect( DestRect, Editor.EditorSkinBitmap.Canvas, SourceRect );
               ShowWallCaps( Col__, Row__ );
               end
          else if Assigned( BackgroundBitmap__ ) then
                  // restore background
                  if ToolsForm.CaptureViewMenuItemShowColorQuantization.Checked and
                     ( Editor.Step = csBoardSquares ) and
                     ( Colors.Count > 0 ) and
                     ( Board__.Squares[ Succ( Col__), Succ( Row__ ) ] = [] ) and
                     MakeStaticWorkBitmap( ScaledSize( ToolsForm.ColWidthSpinEdit.Value ), ScaledSize( ToolsForm.RowHeightSpinEdit.Value ), True ) then begin // restore background and show color quantization result for the square
                     //Brush.Color := BoardSquareColors[ Col__, Row__ ];
                     // FillRect( Rect( X + 2 , Y + 2, X + Editor.ColWidth, Y + Editor.RowHeight ) );
                     //BitmapAlphaBlendColor( CaptureImage1.Picture.Bitmap, BoardSquareColors[ Col__, Row__ ], 75, Rect( X + 2 , Y + 2, X + Editor.ColWidth - 2, Y + Editor.RowHeight - 2 ) );
                     StaticWorkBitmap.Canvas.CopyRect( Rect( 0, 0, ScaledSize( ToolsForm.ColWidthSpinEdit.Value ), ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) ), BackgroundBitmap__.Canvas, DestRect );
                     BitmapAlphaBlendColor( StaticWorkBitmap,
                                            BoardSquareColors[ Col__, Row__ ],
                                            50,
                                            Rect( 1 , 1, ScaledSize( ToolsForm.ColWidthSpinEdit.Value ) - 1, ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) - 1 ) );
                     CopyRect( DestRect, StaticWorkBitmap.Canvas, Rect( 0, 0, ScaledSize( ToolsForm.ColWidthSpinEdit.Value ) , ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) ) );
                     end
                  else CopyRect( DestRect, BackgroundBitmap__.Canvas, DestRect ); // restore background
          end;
     end;
end;

function  TCaptureForm.ShowBoard : Boolean;
var Col, Row : Integer; Board : TCaptureBoard;

  procedure MakeGradientBitmap( Bitmap__ : TBitmap; TopColor__, BottomColor__ : TColor );
  var Y : Integer; Value : Double; SourceRect, DestRect : TRect; B : TBitmap;

    function Sinusiod( Value__, Frequency__, Phase__, Amplitude__, Bias__ : Double ) : Double;
    begin
      Result := Amplitude__ * Sin( 2 * PI * ( ( Frequency__ * Value__ ) + ( Phase__ / 360.0 ) ) ) + Bias__;
    end;

  begin // 'MakeGradientBitmap'
    if ( Bitmap__.Width > 0 ) and ( Bitmap__.Height > 0 ) and
       BitmapCreate( B, Bitmap__.Width, Bitmap__.Height ) then
       try
          // fill the left column with a vertical gradient line
          for Y := 0 to Pred( B.Height ) do begin
              Value := Abs( Sinusiod( ( Y / B.Height ), 0.5, 0.0, 1.0, 0.0 ) );
              //Color := Trunc( 255.0 * Value );
              //Color := Color + ( Color shl 8 ) + ( Color shl 16 );
              //B.Canvas.Pixels[ 0, Y ] := Color;
              //for X := 0 to Pred( Bitmap__.Width ) do begin
              //    B.Canvas.Pixels[ X, Y ] := Color;
              //    end;
              B.Canvas.Pixels[ 0, Y ] :=   LinearInterpolation( Value,   TopColor__          and $ff,    BottomColor__          and $ff )           +
                                         ( LinearInterpolation( Value, ( TopColor__ shr  8 ) and $ff,  ( BottomColor__ shr  8 ) and $ff )  shl  8 ) +
                                         ( LinearInterpolation( Value, ( TopColor__ shr 16 ) and $ff,  ( BottomColor__ shr 16 ) and $ff )  shl 16 );
              end;
          // duplicate the vertical gradient line across the entire bitmap
          SourceRect := Rect( 0, 0, 1, B.Height );
          DestRect   := Rect( 1, 0, 2, B.Height );
          while DestRect.Left < B.Width do with B do with Canvas do begin
            CopyRect( DestRect, B.Canvas, SourceRect );
            DestRect  .Left  := DestRect.Right;
            DestRect.  Right := Min( 2 * DestRect.Right, B.Width );
            SourceRect.Right := RectWidth( DestRect );
            end;
          SourceRect := Rect( 0, 0, B.Width, B.Height );
          Bitmap__.Canvas.CopyMode :=cmSrcCopy;
          Bitmap__.Canvas.CopyRect(  SourceRect, B.Canvas, SourceRect );
       finally B.Free;
       end;
  end;

  function  LoadEditorSkin( var FileName__ : String ) : Boolean;
  var Bitmap : TBitmap;
  begin
    Result := False; Bitmap := nil;
    try     try Result := BitmapCreate( Bitmap, 1, 1);
                if Result then begin
                   Bitmap.LoadFromFile( FileName__ );
                   Result := ( Bitmap.Width = SINGLE_ROW_SKIN_TILE_COUNT * Bitmap.Height );
                   if   Result then begin
                        Result := CreateEditorSkin( Bitmap, CaptureButtonsAndSkinImage.Picture.Bitmap.Width div CaptureButtonsAndSkinImage.Picture.Bitmap.Height, 0, 0 );
                        if   Result then
                        else Error( TEXT_TASK_FAILED, '' );
                        end
                   else Msg(Format(FileNotALegalCaptureToolEditorSkinText__, [FileName__]),Caption,MB_OK+MB_ICONINFORMATION);
                   end;
            except on E:Exception do Result := Error( E.Message, '' );
            end;
    finally Bitmap.Free;
            if not Result then
               FileName__ := ''; // go back to using the built-in skin
    end;
  end;

begin // 'ShowBoard'
  Result := False;
  if ( Editor.Step >= csBoardSquares ) and ( Editor.Step <= csSavePuzzle ) then begin
     Result := Assigned( Editor.EditorSkinBitmap )
               or
               ( ( EditorSkinFileName <> '' )
                 and
                 LoadEditorSkin( EditorSkinFileName )
               )
               or
               CreateEditorSkin( CaptureButtonsAndSkinImage.Picture.Bitmap,
                                 CaptureButtonsAndSkinImage.Picture.Bitmap.Width div CaptureButtonsAndSkinImage.Picture.Bitmap.Height, 0, 0 );

     if Result then
        for Col := 0 to Pred( Editor.Board.Width ) do
            for Row := 0 to Pred( Editor.Board.Height ) do
                ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
     end
  else if Editor.Step >= csSaveSkin then begin
          ZeroMemory( Addr( Board ), SizeOf( Board ) );
          Board.Width                          := PUZZLE_EXAMPLE_BOARD_WIDTH;
          Board.Height                         := PUZZLE_EXAMPLE_BOARD_HEIGHT;
          Result                               := TextToBoard( PUZZLE_EXAMPLE_BOARD_AS_TEXT, Board )
                                                  and
                                                  ( Assigned( Editor.EditorSkinBitmap )
                                                    or
                                                    ( Assigned( SingleRowSkinBitmap )
                                                      and
                                                      ( ToolsForm.ColWidthSpinEdit.Value > 0 )
                                                      and
                                                      CreateEditorSkin( SingleRowSkinBitmap, SingleRowSkinBitmap.Width div ToolsForm.ColWidthSpinEdit.Value, 0, 0 )
                                                    )
                                                  );
          if Result then
             try
               ToolsForm.CaptureImage2.Picture.Bitmap.Width     := ScaledSize( ToolsForm.ColWidthSpinEdit.Value  ) * ( 2 + Board.Width  );
               ToolsForm.CaptureImage2.Picture.Bitmap.Height    := ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) * ( 2 + Board.Height );
               ToolsForm.CaptureImage2.ClientWidth              := ToolsForm.CaptureImage2.Picture.Bitmap.Width;
               ToolsForm.CaptureImage2.ClientHeight             := ToolsForm.CaptureImage2.Picture.Bitmap.Height;
               with ToolsForm.CaptureImage2.Picture.Bitmap do with Canvas do begin
                 Brush.Style                   := bsSolid;
                 Brush.Color                   := clDkGray;
                 //FillRect( Rect( 0, 0, Width, Height ) );
                 MakeGradientBitmap( ToolsForm.CaptureImage2.Picture.Bitmap, clDkGray, clLtGray );

                 Pen.Style                     := psSolid;
                 Pen.Width                     := 1;
                 Pen.Color                     := clBlack; MoveTo( 1, 1 ); LineTo( Width        , 1 ); MoveTo( 1, 1 ); LineTo( 1, Height                 ); MoveTo( 1,       Pred( Height )   ); LineTo( Width        ,       Pred( Height )   ); MoveTo(       Pred( Width )  , 1 ); LineTo(       Pred( Width )  ,       Height   );
                 Pen.Color                     := clWhite; MoveTo( 0, 0 ); LineTo( Pred( Width ), 0 ); MoveTo( 0, 0 ); LineTo( 0, Pred( Pred( Height ) ) ); MoveTo( 0, Pred( Pred( Height ) ) ); LineTo( Pred( Width ), Pred( Pred( Height ) ) ); MoveTo( Pred( Pred( Width ) ), 0 ); LineTo( Pred( Pred( Width ) ), Pred( Height ) );

                 Font.Height                   := Max( 10, Min( 20, ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) div 2 ) );
                 Windows.SetBkMode ( Canvas.Handle, Windows.TRANSPARENT );
                 Font.Color                    := clBlack;
                 TextOut( 5, 5, TEXT_PUZZLE_EXAMPLE );
                 Font.Color                    := clWhite;
                 TextOut( 4, 4, TEXT_PUZZLE_EXAMPLE );
                 Windows.SetBkMode ( Canvas.Handle, Windows.OPAQUE );
                 end;

               for Col := 0 to Pred( Board.Width ) do
                   for Row := 0 to Pred( Board.Height ) do
                       ShowSquare( Col, Row, Board, ToolsForm.CaptureImage2.Picture.Bitmap, nil );

             except on E:Exception do Result   := False;
             end;

          ToolsForm.CaptureImage2.Visible := Result;
          UpdateScrollBoxRange;
          end;
  ShowStatus;
end;

function  TCaptureForm.BoardAsText : String;
begin
  Result := Capture_.BoardToText( Editor.Board, FLOOR_CH );
end;

function  TCaptureForm.BoardAsTextWithBorder( var BoardAsText__ : String; var BoardWidth__, BoardHeight__ : Integer ) : Boolean;
var Col, Row, Border : Integer; OpenSides : Boolean; BoardRect : TRect;
begin // adds a border if the level isn't entirely surrounded by walls and/or boxes at goals
  Result := False;
  BoardAsText__ := '';
  BoardWidth__  := 0;
  BoardHeight__ := 0;
  CheckBoard( Editor.Board, nil, nil, False, BoardRect, OpenSides );
  BoardWidth__  := RectWidth( BoardRect );
  BoardHeight__ := RectHeight( BoardRect );
  if     OpenSides then
         Border := 1
  else   Border := 0;
  if ( BoardWidth__ > 0 ) and ( BoardHeight__ > 0 ) then
     try    Inc( BoardWidth__ , 2 * Border );
            Inc( BoardHeight__, 2 * Border );
            SetLength( BoardAsText__, BoardWidth__ * BoardHeight__ );
            if  OpenSides then
                for Col := 1 to Length( BoardAsText__ ) do BoardAsText__[ Col ] := WALL_CH;
            for Row := BoardRect.Top to Pred( BoardRect.Bottom ) do begin
                for Col := BoardRect.Left to Pred( BoardRect.Right ) do
                    BoardAsText__[ Border + Succ( Col - BoardRect.Left ) + ( ( Border + Pred( Succ( Row - BoardRect.Top ) ) ) * BoardWidth__ ) ] := BoardSquareToChar( Editor.Board.Squares[ Col, Row ], FLOOR_CH );
                end;
            Result := True;
     except on E:Exception do begin
            Result := Error( E.Message, '' );
            end;
     end;
end;

function  TCaptureForm.EditorAutomaticCompletion( Col__, Row__ : Integer; SquareValue__ : TCaptureBoardSquareItemSet ) : Integer;
var X, Y : Integer; Color : TColor;
begin
  Result := 0;
  if ToolsForm.CaptureSettingsMenuItemUseAutomaticCompletion.Checked and ( Colors.Count <> 0 ) then with Editor.Board do
     if not ( bsiPlayer in Squares[ Succ( Col__) ,  Succ( Row__ )  ] ) then begin
        Color := BoardSquareColors[ Col__, Row__ ];
        for Y := 1 to Height do
            for X := 1 to Width do
                if ( Squares[ X, Y ] =  SquareValue__ ) and ( BoardSquareColors[ Pred( X ), Pred( Y ) ] = Color ) then begin
                   Squares  [ X, Y ] := Squares[ Succ( Col__) , Succ( Row__ ) ];
                   Inc( Result );
                   end;
        if Result <> 0 then begin
           if Editor.CompletedStep >= csSavePuzzle then
              Editor.CompletedStep := Pred( csSavePuzzle );
           ShowBoard;
           end;
        end;
end;

procedure TCaptureForm.CaptureImage1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ( Editor.Step >= csBoard ) and
     ( Editor.Step <= csBoardSquares ) then with Editor do begin
     if MouseButtonDown and ( MouseButton <> mbMiddle ) then
        CaptureImage1MouseUp(Sender,Button,Shift,X,Y);
     MouseButton:=Button; MouseButtonDown:=True;
     X := X - ( X mod ( Zoom div 100 ) );
     Y := Y - ( Y mod ( Zoom div 100 ) );
     DragPoint.X := X; DragPoint.Y := Y;
     StartDragPoint := DragPoint;
     SizeHandle:=ghNull; IsDragging:=False;
     IsSelectingANewArea:=False;
     ToggledItemPos.X:=-1; ToggledItemPos.Y:=-1;

     if Button=mbLeft then begin
        if Cursor <> ctSelection then begin
           with Editor.History do TransactionIndex := Pred( Low( Items ) );
           CaptureImage1MouseMove(Sender,Shift,X,Y);
           end
        else begin
           if HasSelection then begin
              SizeHandle:=PointToGrabHandle(Point(X,Y),CursorRect, SIZING_RECT_WIDTH);
              if Screen.Cursor<>GrabHandleToCursor(SizeHandle) then
                 Screen.Cursor:=GrabHandleToCursor(SizeHandle);
              IsDragging:=(SizeHandle=ghNull) and PtInRect(CursorRect,DragPoint);
              if SizeHandle=ghNull then begin
                 if IsDragging then begin
                    if ( RectWidth ( CursorRect ) = ToolsForm.CaptureImage1.Picture.Bitmap.Width-2 ) and
                       ( RectHeight( CursorRect ) = ToolsForm.CaptureImage1.Picture.Bitmap.Height-2 ) then
                       // undo a "select-all"
                       CaptureEditSelectAllItemClick( ToolsForm.CaptureEditMenuItemSelectAll );
                    end;

                 if IsDragging then begin
                    ShowGrid(CursorRect);
                    end
                 else
                    if ( Editor.Step >= csBoard ) and ( Editor.Step < csColumnsRows ) then begin
                       Editor.HasSelection:=False;
                       SizeHandle:=ghNull; IsDragging:=False;
                       Editor.CompletedStep := csImage;
                       MouseButtonDown:=False;
                       ClearSelection;
                       //Image1MouseDown(Sender,Button,Shift,X,Y); // go directly to selecting a new area
                       //IsSelectingANewArea := True;
                       end
                    else begin
                       // clicking outside the selected area while entering the
                       // number of columns and rows;
                       // keep the existing selected area;
                       MouseButtonDown:=False; IsSelectingANewArea:=False;
                       end;
                  end
              else with CursorRect do with DragPoint do begin
                 X:=Max(Left,Min(Right,X)); Y:=Max(Top,Min(Bottom,Y)); // ensure that the drag-point is inside the cursor-rectangle
                 ShowGrid(CursorRect);
                 end;
              end
           else
              if True then begin
                 if (X>=0) and (X<ToolsForm.CaptureImage1.Picture.Bitmap.Width-2) and
                    (Y>=0) and (Y<ToolsForm.CaptureImage1.Picture.Bitmap.Height-2) then begin
                    Editor.HasSelection:=True; // switch from highlighting current cell to selecting an area
                    ShowGrid(Rect(X,Y,Succ(X), Succ(Y)));
                    end;
                 end;
           CaptureImage1MouseMove(Sender,Shift,X,Y);
           end;
        end
     else begin
        if (Button=mbRight) and
           (Cursor=ctSelection) and
           Editor.HasSelection then begin
           MouseButtonDown:=False;
           ShowStatus;
           //EditPopupMenuRightClickPosition:=EditImage1.ClientToScreen(Point(X,Y));
           //with EditPopupMenuRightClickPosition do EditPopupMenuRightClick.Popup(X,Y);
           end
        else if Button = mbMiddle then begin
                end
             else begin
                if HasSelection then begin
                   HasSelection:=False;
                   MouseButtonDown:=False;
                   end;
                ShowCursor(Cursor,Rect(X,Y,Succ(X), Succ(Y)));
                CaptureImage1MouseMove(Sender,Shift,X,Y);
                end;
        end;
     ShowStatus;
     end
  else begin
     Editor.MouseButtonDown:=True; Editor.MouseButton:=Button;
     end;
end;

procedure TCaptureForm.CaptureImage1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var i, DX, DY, Col, Row : Integer; P : TPoint; R, R1 : TRect; C : TCursor; Items : TCaptureBoardSquareItemSet;
begin // 'Image1MouseMove'
  if ( Editor.Step >= csBoard ) and
     ( Editor.Step <= csBoardSquares ) and
     ( X >= 0 ) and
     ( Y >= 0 ) and
     ( X < ToolsForm.CaptureImage1.Picture.Bitmap.Width  - 2 ) and
     ( Y < ToolsForm.CaptureImage1.Picture.Bitmap.Height - 2 ) then with Editor do begin
     //DX:=X; DY:=Y;
     X := Min( Max(0, X - ( X mod ( Zoom div 100 ) ) ), ToolsForm.CaptureImage1.Picture.Bitmap.Width  - 2 ); // ensure that the rectangle has an exact match at zoom 100%
     Y := Min( Max(0, Y - ( Y mod ( Zoom div 100 ) ) ), ToolsForm.CaptureImage1.Picture.Bitmap.Height - 2 );
     //ToolsForm.Caption:=IntToStr(DX)+SPACE+IntToStr(DY)+SPACE+IntToStr(X)+SPACE+IntToStr(Y)+SPACE+IntToStr(NormalizedPosition(X))+SPACE+IntToStr(NormalizedPosition(Y));
     if ( Editor.Step >= csBoard ) and ( Editor.Step <= csColumnsRows ) then
        ToolsForm.StatusBar1.Panels[ 0 ].Text := Format('[%d:%d]',[ NormalizedPosition( X ), NormalizedPosition( Y ) ]);
     if (SizeHandle<>ghNull) and
        (Screen.Cursor<>GrabHandleToCursor(SizeHandle)) then
        Screen.Cursor:=GrabHandleToCursor(SizeHandle);

     if MouseButtonDown and ( Editor.Cursor < ctLeftLine ) then begin
        if Cursor<>ctSelection then begin
           if Screen.Cursor<>ToolsForm.CaptureImage1.Cursor then Screen.Cursor:=ToolsForm.CaptureImage1.Cursor;
           XYToColRow( X, Y, Col, Row );

           if (Col>=0) and (Col<Editor.Board.Width) and
              (Row>=0) and (Row<Editor.Board.Height) then begin
              R := ColRowToRect( Col, Row );

              Items:=BoardSquareValue(Col,Row);

              if MouseButton=mbLeft then begin

                 case DrawingTool of

                   dtNone      :;

                   dtWall      : if (not (bsiWall in Items)) and BeginTransaction then begin
                                    if bsiPlayer in Items then with Editor.Board.PlayerPos do begin
                                       Exclude( Editor.Board.Squares[ Succ(X), Succ(Y) ], bsiPlayer );
                                       ShowSquare( X, Y, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                       X:=-1; Y:=-1;
                                       end;
                                    Editor.Board.Squares[ Succ(Col), Succ(Row) ] := [ bsiWall ];
                                    ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                    EditorAutomaticCompletion( Col, Row, [] );
                                    Editor.CompletedStep := Pred( csBoardSquares );
                                    end;

                   dtBox       : if (not (bsiBox in Items)) and BeginTransaction then begin
                                    if bsiPlayer in Items then with Editor.Board.PlayerPos do begin
                                       Exclude( Editor.Board.Squares[ Succ(X), Succ(Y) ], bsiPlayer );
                                       ShowSquare( X, Y, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                       X:=-1; Y:=-1; Items := [];
                                       end;
                                    Editor.Board.Squares[ Succ(Col), Succ(Row) ] := Items + [ bsiFloor, bsiBox ] - [ bsiWall, bsiPlayer ];
                                    ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                    Editor.ToggledItemPos.X:=Col; Editor.ToggledItemPos.Y:=Row;
                                    if   bsiGoal in Items then
                                         EditorAutomaticCompletion( Col, Row, Items )
                                    else EditorAutomaticCompletion( Col, Row, [] );
                                    Editor.CompletedStep := Pred( csBoardSquares );
                                    end
                                 else
                                    if (Col<>Editor.ToggledItemPos.X) or
                                       (Row<>Editor.ToggledItemPos.Y) then begin
                                       if   bsiGoal in Items then
                                            Exclude( Editor.Board.Squares[ Succ(Col), Succ(Row) ], bsiGoal )
                                       else Include( Editor.Board.Squares[ Succ(Col), Succ(Row) ], bsiGoal );
                                       ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                       Editor.ToggledItemPos.X:=Col; Editor.ToggledItemPos.Y:=Row;
                                       EditorAutomaticCompletion( Col, Row, Items );
                                       Editor.CompletedStep := Pred( csBoardSquares );
                                       end;

                   dtGoal      : if (not (bsiGoal in Items)) and BeginTransaction then begin
                                    Editor.Board.Squares[ Succ(Col), Succ(Row) ] := Items + [ bsiFloor, bsiGoal ] - [ bsiWall ];
                                    ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                    Editor.ToggledItemPos.X:=Col; Editor.ToggledItemPos.Y:=Row;
                                    if   bsiBox in Items then
                                         EditorAutomaticCompletion( Col, Row, Items )
                                    else EditorAutomaticCompletion( Col, Row, []    );
                                    Editor.CompletedStep := Pred( csBoardSquares );
                                    end
                                 else
                                    if (Col<>Editor.ToggledItemPos.X) or
                                       (Row<>Editor.ToggledItemPos.Y) then begin
                                       if bsiBox in Items then begin
                                          Exclude( Editor.Board.Squares[ Succ(Col), Succ(Row) ], bsiBox );
                                          end
                                       else begin
                                          if bsiPlayer in Items then with Editor.Board.PlayerPos do begin
                                             Exclude( Editor.Board.Squares[ Succ(X), Succ(Y) ], bsiPlayer );
                                             ShowSquare( X, Y, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                             X:=-1; Y:=-1;
                                             Items := [];
                                             end;
                                          Include( Editor.Board.Squares[ Succ(Col), Succ(Row) ], bsiBox );
                                          end;
                                       ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                       Editor.ToggledItemPos.X:=Col; Editor.ToggledItemPos.Y:=Row;
                                       EditorAutomaticCompletion( Col, Row, Items );
                                       Editor.CompletedStep := Pred( csBoardSquares );
                                       end;

                   dtPlayer    : if (not (bsiPlayer in Items)) and BeginTransaction then begin
                                    if Editor.Board.PlayerPos.X <> -1 then with Editor.Board.PlayerPos do begin
                                       Exclude( Editor.Board.Squares[ Succ(X), Succ(Y) ], bsiPlayer );
                                       ShowSquare( X, Y, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                       X:=-1; Y:=-1;
                                       end;
                                    Editor.Board.Squares[ Succ(Col), Succ(Row) ] := Items + [ bsiFloor, bsiPlayer ] - [ bsiWall, bsiBox ];
                                    Editor.Board.PlayerPos.X := Col; Editor.Board.PlayerPos.Y := Row;
                                    ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                    Editor.ToggledItemPos.X:=Col; Editor.ToggledItemPos.Y:=Row;
                                    EditorAutomaticCompletion( Col, Row, [] );
                                    Editor.CompletedStep := Pred( csBoardSquares );
                                    end
                                 else
                                    if (Col<>Editor.ToggledItemPos.X) or
                                       (Row<>Editor.ToggledItemPos.Y) then begin
                                       if   bsiGoal in Items then
                                            Exclude( Editor.Board.Squares[ Succ(Col), Succ(Row) ], bsiGoal )
                                       else Include( Editor.Board.Squares[ Succ(Col), Succ(Row) ], bsiGoal );
                                       ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                       Editor.ToggledItemPos.X:=Col; Editor.ToggledItemPos.Y:=Row;
                                       EditorAutomaticCompletion( Col, Row, [] );
                                       Editor.CompletedStep := Pred( csBoardSquares );
                                       end;
                   dtFloor     : if (Items <> [ bsiFloor ]) and BeginTransaction then begin
                                    if bsiPlayer in Items then with Editor.Board.PlayerPos do begin
                                       Exclude( Editor.Board.Squares[ Succ(X), Succ(Y) ], bsiPlayer );
                                       ShowSquare( X, Y, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                       X:=-1; Y:=-1;
                                       end;
                                    Editor.Board.Squares[ Succ(Col), Succ(Row) ] := [ bsiFloor ];
                                    ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                    Editor.ToggledItemPos.X:=Col; Editor.ToggledItemPos.Y:=Row;
                                    EditorAutomaticCompletion( Col, Row, [] );
                                    Editor.CompletedStep := Pred( csBoardSquares );
                                    end;
                   dtErase     : if (Items <> [] ) and BeginTransaction then begin
                                    if bsiPlayer in Items then with Editor.Board.PlayerPos do begin
                                       X:=-1; Y:=-1;
                                       end;
                                    Editor.Board.Squares[ Succ(Col), Succ(Row) ] := []; //[ bsiFloor ];
                                    ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                                    Editor.CompletedStep := Pred( csBoardSquares );
                                    end;

                   else;
                 end; // case
                 end
              else
                 if (MouseButton=mbRight) and
                    (Items <> []) and BeginTransaction then begin
                   if bsiPlayer in Items then with Editor.Board.PlayerPos do begin
                       X:=-1; Y:=-1;
                       end;
                    Editor.Board.Squares[ Succ(Col), Succ(Row) ] := []; //[ bsiFloor ];
                    ShowSquare( Col, Row, Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap );
                    Editor.CompletedStep := Pred( csBoardSquares );
                    end;
              end
           else R := Rect( 0, 0, 0, 0 );

           ShowCursor(Cursor,R);
           ShowStatus;
           end
        else begin // cursor = ctSelection
           if      IsDragging then begin
                   if Screen.Cursor<>crDrag then Screen.Cursor:=crDrag;
                   DX:=X-DragPoint.X; DY:=Y-DragPoint.Y;
                   if (DX<>0) or (DY<>0) then with CursorRect do begin
                      if  ( ( Abs( X - StartDragPoint.X ) > 10 ) or ( Abs( Y - StartDragPoint.Y ) > 10 ) ) and // the user probably didn't just nudge the grid into place
                          ( Editor.CompletedStep >= csBoardSquares ) then begin
                          Editor.CompletedStep   := Pred( csBoardSquares );
                          if Editor.Board.Width  <> 0 then
                             ClearBoard;
                          end;
                      R:=Rect(Left+DX,Top+DY,Right+DX,Bottom+DY);
                      R1 := R;
                      ClipRect (R1,Rect(0,0,ToolsForm.CaptureImage1.Picture.Bitmap.Width-2,ToolsForm.CaptureImage1.Picture.Bitmap.Height-2), True);

                      i:=1+1+SIZING_RECT_WIDTH+8;
                      if  // ensure the selected area doesn't shrink
                          ( RectWidth ( R ) = RectWidth ( R1 ) ) and
                          ( RectHeight( R ) = RectHeight( R1 ) ) and
                           // ensure there are some pixels where the user later can grab the rectangle with the mouse
                          (R1.Left  <ToolsForm.CaptureImage1.Picture.Bitmap.Width -i) and
                          (R1.Top   <ToolsForm.CaptureImage1.Picture.Bitmap.Height-i) and
                          (R1.Right >i) and
                           (R1.Bottom>i) and
                          (R.Left<R.Right) and
                          (R.Top<R.Bottom) then begin
                          ShowGrid(R);
                          UserWidth := NormalizedSize( RectWidth( CursorRect ) );
                          UserHeight := NormalizedSize( RectHeight( CursorRect ) );
                          end;
                      DragPoint:=Point(X,Y);
                      end;
                   end
           else if SizeHandle<>ghNull then with CursorRect do begin // 'True': resizing the selected area
                   P:=DragPoint; R:=CursorRect;
                   if (GrabHandleResizeRect(SizeHandle,X,Y,P,R)<>0)  then begin
                      Editor.CompletedStep := csImage;
                      if (R.Left>=0) and (R.Top>=0) and
                         (R.Right <=ToolsForm.CaptureImage1.Picture.Bitmap.Width -2) and
                         (R.Bottom<=ToolsForm.CaptureImage1.Picture.Bitmap.Height-2) and
                         (R.Left<R.Right) and
                         (R.Top<R.Bottom) then begin
                         if ToolsForm.LockColumnWidthAndRowHeightCheckBox.Checked and
                            ( ToolsForm.ColWidthSpinEdit.Value > 0 ) and
                            ( ToolsForm.RowHeightSpinEdit.Value > 0 ) then begin

                            if R.Left <> CursorRect.Left  then begin
                               Col := ( CursorRect.Right - X )   div ScaledSize( ToolsForm.ColWidthSpinEdit.Value );
                               X   :=   CursorRect.Right - ( Col *   ScaledSize( ToolsForm.ColWidthSpinEdit.Value ) );
                               if ( Col <> ToolsForm.ColumnsSpinEdit.Value ) and
                                  ( Col >  0 ) and
                                  ( Col <= CAPTURE_MAX_BOARD_WIDTH ) and
                                  ( X   >= 0 ) then begin
                                  ClearBoard;
                                  SetSpinEditValue( ToolsForm.LeftSpinEdit, NormalizedPosition( X ) );
                                  ToolsForm.ColumnsSpinEdit.Value := Col;
                                  end;
                               end;

                            if R.Top <> CursorRect.Top  then begin
                               Row := ( CursorRect.Bottom - Y )   div ScaledSize( ToolsForm.RowHeightSpinEdit.Value );
                               Y   :=   CursorRect.Bottom - ( Row *   ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) );
                               if ( Row <> ToolsForm.RowsSpinEdit.Value ) and
                                  ( Row >  0 ) and
                                  ( Row <= CAPTURE_MAX_BOARD_HEIGHT ) and
                                  ( Y   >= 0 ) then begin
                                  ClearBoard;
                                  SetSpinEditValue( ToolsForm.TopSpinEdit, NormalizedPosition( Y ) );
                                  ToolsForm.RowsSpinEdit.Value := Row;
                                  end;
                               end;

                            if R.Right <> CursorRect.Right  then begin
                               Col := ( X - CursorRect.Left ) div ScaledSize( ToolsForm.ColWidthSpinEdit.Value );
                               if ( Col <> ToolsForm.ColumnsSpinEdit.Value ) and
                                  ( Col >  0 ) and
                                  ( Col <= CAPTURE_MAX_BOARD_WIDTH ) and
                                  ( CursorRect.Left + ( Col * ScaledSize( ToolsForm.ColWidthSpinEdit.Value ) ) <= ( ToolsForm.CaptureImage1.Picture.Bitmap.Width - 2 ) ) then begin
                                  ClearBoard;
                                  ToolsForm.ColumnsSpinEdit.Value := Col;
                                  end;
                               end;

                            if R.Bottom <> CursorRect.Bottom then begin
                               Row := ( Y - CursorRect.Top  ) div ScaledSize( ToolsForm.RowHeightSpinEdit.Value );
                               if  ( Row <> ToolsForm.RowsSpinEdit.Value ) and
                                   ( Row >  0 ) and
                                   ( Row <= CAPTURE_MAX_BOARD_HEIGHT ) and
                                   ( CursorRect.Top + ( Row * ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) ) <= ( ToolsForm.CaptureImage1.Picture.Bitmap.Height - 2 ) ) then begin
                                   ClearBoard;
                                   ToolsForm.RowsSpinEdit.Value := Row;
                                   end;
                                end;

                            R := CursorRect;
	                    end;

                         DragPoint:=P;
                         ShowGrid( R );
                         UserWidth := NormalizedSize( RectWidth( CursorRect ) );
                         UserHeight := NormalizedSize( RectHeight( CursorRect ) );
                         end;
                      end;
                   end
           else if Editor.HasSelection then begin
                   if MouseButton=mbLeft then begin
                      if Screen.Cursor<>crSize then Screen.Cursor:=crSize;
                      R:=Rect( Max(0,Min(DragPoint.X,X)),
                                Max(0,Min(DragPoint.Y,Y)),
                               Min(ToolsForm.CaptureImage1.Picture.Bitmap.Width -2 ,Succ(Max(DragPoint.X,X))),
                               Min(ToolsForm.CaptureImage1.Picture.Bitmap.Height-2, Succ(Max(DragPoint.Y,Y))));
                      if (R.Left <>CursorRect.Left ) or (R.Top   <>CursorRect.Top   ) or
                         (R.Right<>CursorRect.Right) or (R.Bottom<>CursorRect.Bottom) then begin
                         if ( ( Abs( X - StartDragPoint.X ) > 10 ) or ( Abs( Y - StartDragPoint.Y ) > 10 ) ) and // the user probably didn't just nudge the grid into place
                            ( Editor.CompletedStep >= csBoardSquares ) then begin
                            Editor.CompletedStep   := Pred( csBoardSquares );
                            if Editor.Board.Width <> 0 then
                               ClearBoard;
                            end;
                         ShowGrid( R );
                         UserWidth := NormalizedSize( RectWidth( CursorRect ) );
                         UserHeight := NormalizedSize( RectHeight( CursorRect ) );
                         end;
                      end;
                   end
                else begin
                   if MouseButton=mbRight then begin
                      end;
                   ShowGrid(Rect(0,0,0,0));
                   end;
           end;
        end
     else begin // no mouse button pressed
        //if Screen.Cursor=crDrag then Screen.Cursor:=EditImage1.Cursor;
        if Screen.Cursor<>ToolsForm.CaptureImage1.Cursor then Screen.Cursor:=ToolsForm.CaptureImage1.Cursor;
        SizeHandle:=ghNull;

        if Editor.HasSelection and ( not Editor.DrawingToolsEnabled ) then begin
           SizeHandle:=PointToGrabHandle(Point(X,Y),CursorRect,SIZING_RECT_WIDTH);
           if SizeHandle=ghNull then begin
              if Screen.Cursor<>ToolsForm.CaptureImage1.Cursor then Screen.Cursor:=ToolsForm.CaptureImage1.Cursor;
              end
           else begin
              C:=GrabHandleToCursor(SizeHandle);
              if C<>Screen.Cursor then Screen.Cursor:=C;
              end;
           end
        else if Editor.DrawingToolsEnabled then begin
                if (X<>CursorRect.Left) or
                   (Y<>CursorRect.Top) or
                   (CursorRect.Right=0) then begin // '0': there is no cursor on the screen at the moment
                   if Sender<>nil then begin
                      XYToColRow( X, Y, Col, Row );
                      R := ColRowToRect( Col, Row );
                      ClipRect( R, ScaledBoardRect, True );
                      if ( RectWidth ( R ) >= ScaledSize( ToolsForm.ColWidthSpinEdit.Value ) ) and
                         ( RectHeight( R ) >= ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) ) then begin
                         if ToolsForm.CaptureImage1.Cursor  <> Succ(Ord(Editor.DrawingTool)) then
                            ToolsForm.CaptureImage1.Cursor  := Succ(Ord(Editor.DrawingTool));
                         ShowCursor(Cursor, R );
                          end
                      else begin
                         ToolsForm.CaptureImage1.Cursor := crDefault;
                         HideCursor( True );
                         end;
                      end;
                   end;
                end
             else
               if Editor.Cursor >= ctLeftLine then with Editor do begin
                  R := ScaledBoardRect;
                  if R.Left = R.Right  then R.Right  := ToolsForm.CaptureImage1.Picture.Bitmap.Width  - 2;
                  if ( R.Top  = R.Bottom ) or ( Editor.Cursor = ctBottomLine ) then R.Bottom := ToolsForm.CaptureImage1.Picture.Bitmap.Height - 2;

                  case Editor.Cursor of
                    ctLeftLine   : begin R := Rect( Min( X, R.Right ), 0, Min( X, R.Right ) + 2, R.Bottom );
                                         SetSpinEditValue( ToolsForm.LeftSpinEdit, NormalizedPosition( R.Left ) );
                                   end;
                    ctTopLine    : begin R := Rect( R.Left, Min( Y, R.Bottom ), R.Right, Min( Y, R.Bottom ) + 2 );
                                         SetSpinEditValue( ToolsForm.TopSpinEdit, NormalizedPosition( R.Top ) );
                                   end;
                    ctRightLine  : begin Y := ScaledSize( MIN_BOARD_SIZE_PIXELS );
                                         R := Rect( Max( X, R.Left + Y ), R.Top, Max( X, R.Left + Y ) + 2, R.Bottom );
                                         SetSpinEditValue( ToolsForm.WidthSpinEdit, NormalizedPosition( R.Left ) - ToolsForm.LeftSpinEdit.Value );
                                   end;
                    ctBottomLine : begin X := ScaledSize( MIN_BOARD_SIZE_PIXELS );
                                         R := Rect( R.Left, Min( R.Bottom, Max( Y, R.Top + X ) ), R.Right, Min( R.Bottom, Max( Y, R.Top + X ) ) + 2 );
                                         SetSpinEditValue( ToolsForm.HeightSpinEdit, NormalizedPosition( R.Top ) - ToolsForm.TopSpinEdit.Value );
                                         ShowStatus;
                                   end;
                    ctColumnLine : begin Y := ScaledSize( MIN_BOARD_SQUARE_SIZE_PIXELS );
                                         R := Rect( Max( R.Left + Y , Min( X, R.Right - Y ) ), R.Top, Max( R.Left + Y , Min( X, R.Right - Y ) ) + 2, R.Bottom );
                                         SetSpinEditValue( ToolsForm.ColumnsSpinEdit, 1 );
                                         X := NormalizedPosition( R.Left );
                                         if   X - ToolsForm.LeftSpinEdit.Value <= ToolsForm.LeftSpinEdit.Value + ToolsForm.WidthSpinEdit.Value - X then
                                              SetSpinEditValue( ToolsForm.ColWidthSpinEdit, X - ToolsForm.LeftSpinEdit.Value )
                                         else SetSpinEditValue( ToolsForm.ColWidthSpinEdit, ToolsForm.LeftSpinEdit.Value + ToolsForm.WidthSpinEdit.Value - X );
                                         SetSpinEditValue( ToolsForm.ColumnsSpinEdit, Max( 1, ( ToolsForm.WidthSpinEdit.Value + 2 ) div Max( 1, ToolsForm.ColWidthSpinEdit.Value ) ) );
                                   end;
                    ctRowLine    : begin X := ScaledSize( MIN_BOARD_SQUARE_SIZE_PIXELS );
                                         R := Rect( R.Left, Max( R.Top + X , Min( Y, R.Bottom - X ) ), R.Right, Max( R.Top + X , Min( Y, R.Bottom - X ) ) + 2 );
                                         SetSpinEditValue( ToolsForm.RowsSpinEdit, 1 );
                                         Y := NormalizedPosition( R.Top );
                                         if   Y - ToolsForm.TopSpinEdit.Value <= ToolsForm.TopSpinEdit.Value + ToolsForm.HeightSpinEdit.Value - Y then
                                              SetSpinEditValue( ToolsForm.RowHeightSpinEdit, Y - ToolsForm.TopSpinEdit.Value )
                                         else SetSpinEditValue( ToolsForm.RowHeightSpinEdit, ToolsForm.TopSpinEdit.Value + ToolsForm.HeightSpinEdit.Value - Y );
                                         SetSpinEditValue( ToolsForm.RowsSpinEdit, Max( 1, ( ToolsForm.HeightSpinEdit.Value + 2 ) div Max( 1, ToolsForm.RowHeightSpinEdit.Value ) ) );
                                   end;
                    else           begin R := Rect( 0, 0, 0, 0 );
                                   end;
                  end;
                  ShowCursor( Editor.Cursor, R );
                  end;
        end;

     if HasSelection then with CursorRect do begin
        StatusText :=
          Format( FORMAT_IMAGE_POSITION_AND_SELECTION,
                  [ NormalizedPosition( Left ),
                    NormalizedPosition( Top  ),
                    NormalizedSize( Right - Left ),
                    NormalizedSize( Bottom - Top ) ] );
        if Editor.Step >= csColumnsRows then
           ToolsForm.StatusBar1.Panels[ 0 ].Text := StatusText;
        end
     else begin
        StatusText := '';
        end;
     SetCaptureDrawingToolHint;
     end
  else begin
    ToolsForm.CaptureImage1.Cursor := crDefault;
    if Screen.Cursor <> crDefault then Screen.Cursor := crDefault;
    if Editor.Step >= csBoardSquares then HideCursor( True );
    end;
end;

procedure TCaptureForm.CaptureImage1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var ColWidth, ColCount, GridWidth, RowHeight, RowCount, GridHeight : Integer;
    Point:TPoint; R: TRect;

  function  FindBestMatch( Multiplier__, Multiplicand__, Product__, Range__ : Integer;
                           var NewMultiplier__, NewMultiplicand__, NewProduct__ : Integer ) : Integer;
  var Multiplicand, Product, BestError, Error : Integer;
  begin // finds the best "multiplier * multiplicand = product" match varying the multiplicand and the product with "Range__";
        // returns the best multiplicand;
        // preconditions: all input parameters >= 0;
    NewMultiplier__                       := Multiplier__;
    NewMultiplicand__                     := Multiplicand__;
    NewProduct__                          := Product__;
    BestError                             := Abs( Product__ - ( Multiplier__ * Multiplicand__ ) );
    if ( Multiplier__   > 0 ) and
       ( Multiplicand__ > 0 ) and
       ( Product__      > 0 ) and
       ( Range__        > 0 ) and
       ( Multiplier__   <= High( Integer ) - Range__ ) and
       ( Multiplicand__ <= High( Integer ) - Range__ ) and
       ( Product__      <= High( Integer ) - Range__ ) and
       ( Multiplier__   <= High( Integer ) div Multiplicand__ ) then
       for Product                        := Product__ - Range__ to Product__ + Range__ do
           if Product                     >  0 then
              for Multiplicand            := Multiplicand__ - Range__ to Multiplicand__ + Range__ do
                  if Multiplicand         >  0 then begin
                     Multiplier__         := Product div Multiplicand;
                     Error                := Abs( Product - ( Multiplier__ * Multiplicand ) );
                     if Error             <  BestError then begin
                        BestError         := Error;
                        NewMultiplier__   := Multiplier__;
                        NewMultiplicand__ := Multiplicand;
                        NewProduct__      := Product;
                        end;
                    end;
    Result                                := NewMultiplicand__;
  end;

  function  FindNearestHorizontalLine( Row__, Range__ : Integer ) : Integer;
  var Index, BestScore, Score : Integer;
  begin
    Result                := Row__;
    if   ( OriginalBitmap.Height = IntegralImageHeight ) and Assigned( HorzLines ) then begin
         BestScore        := 0;
         for Index        := Max( 0, Row__ - Range__ ) to Min( IntegralImageHeight - 3, Row__ + Range__ ) do begin
             Score        := Succ( Range__ - Abs( Index - Row__ ) );
             Score        := HorzLines^[ Pred( IntegralImageWidth ) + ( Index * IntegralImageWidth ) ] * Score * Score;
             if Score     >  BestScore then begin
//              Result    := Succ( Index ); // 'Succ': the edge is probably registered one pixel before the beginning of the new region in the image
                Result    := Index;
                BestScore := Score;
                end;
             end;
         end;
  end;

  function  FindNearestVerticalLine( Column__, Range__ : Integer ) : Integer;
  var Index, BestScore, Score, LastRowIndex : Integer;
  begin
    Result                := Column__;
    LastRowIndex          := Pred( IntegralImageHeight ) * IntegralImageWidth; // index of the first column in the last row
    if   ( OriginalBitmap.Width = IntegralImageWidth ) and Assigned( VertLines ) then begin
         BestScore        := 0;
         for Index        := Max( 0, Column__ - Range__ ) to Min( IntegralImageWidth - 3, Column__ + Range__ ) do begin
             Score        := Succ( Range__ - Abs( Index - Column__ ) );
             Score        := VertLines^[ Index + LastRowIndex ] * Score * Score;
             if Score     >  BestScore then begin
//              Result    := Succ( Index ); // 'Succ': the edge is probably registered one pixel before the beginning of the new region in the image
                Result    := Index;
                BestScore := Score;
                end;
             end;
         end;
  end;

begin // 'Image1MouseUp'
  with Editor do begin
    if Screen.Cursor<>ToolsForm.CaptureImage1.Cursor then Screen.Cursor:=ToolsForm.CaptureImage1.Cursor;
//    X := Min( Max(0, X - ( X mod ( Zoom div 100 ) ) ), CaptureImage1.Picture.Bitmap.Width  - 2 ); // ensure that the rectangle has an exact match at zoom 100%
//    Y := Min( Max(0, Y - ( Y mod ( Zoom div 100 ) ) ), CaptureImage1.Picture.Bitmap.Height - 2 );
    //IgnoreKeyUp:=False; IgnoreMouseUp:=False;

    if ( Editor.Step >= csBoard ) and ( Editor.Step <= csBoardSquares ) then begin

       if IsDragging and (Sender<>nil) then begin
          end;

       if MouseButtonDown then with CursorRect do begin
          MouseButtonDown:=False; SizeHandle:=ghNull; IsDragging:=False;
          if //IsSelectingANewArea and
             (Cursor=ctSelection) and Editor.HasSelection then begin
             if ( (Left+ScaledSize(MIN_BOARD_SIZE_PIXELS) > Right) or (Top+ScaledSize(MIN_BOARD_SIZE_PIXELS) > Bottom) ) then begin // 'True': too small to be a proper board
                ClearSelection; //HideCursor( True );
                CompletedStep := csImage;
                ShowStatus;
                if ( OriginalBitmap.Width  > MIN_BOARD_SIZE_PIXELS ) and
                   ( OriginalBitmap.Height > MIN_BOARD_SIZE_PIXELS ) then
                   Editor.Cursor := ctLeftLine;
                end
             else begin
               R := ScaledBoardRect;
               if    ( CompletedStep <= csImage ) or
                     ( Left   <> R.Left ) or
                     ( Top    <> R.Top ) or
                     ( Right  <> R.Right ) or
                     ( Bottom <> R.Bottom ) then begin
                     if CompletedStep <  csBoard then
                        CompletedStep := Pred( csColumnsRows );
                     if ( ( Editor.Step >= csBoard )
                          and
                          ( Editor.Step <  csColumnsRows )
                        )
                        and
                        ( ( RectWidth ( R ) <> RectWidth ( CursorRect ) )
                          or
                          ( RectHeight( R ) <> RectHeight( CursorRect ) ) ) then
                        CompletedStep := Pred( csColumnsRows );
                     UserWidth := NormalizedSize( RectWidth( CursorRect ) );
                     UserHeight := NormalizedSize( RectHeight( CursorRect ) );
                     if   ( ( Editor.Step >= csColumnsRows ) or ( Editor.CompletedStep >= csColumnsRows ) ) and
                          RectangleClampedToColumnsRows( NormalizedRect( CursorRect ), ToolsForm.ColumnsSpinEdit.Value, ToolsForm.RowsSpinEdit.Value, R ) then
                          ShowGrid( ScaledRect( R ) )
                     else ShowGrid( CursorRect );
                     UserWidth := NormalizedSize( RectWidth( CursorRect ) );
                     UserHeight := NormalizedSize( RectHeight( CursorRect ) );
                     //NextStepButton.SetFocus;
                     end;
               end;
             end
          else if Cursor >= ctLeftLine then begin
                  case Cursor of
                    ctLeftLine      : begin //LeftSpinEdit.Value := NormalizedPosition( X );
                                            HideCursor( True );
                                            if ToolsForm.CaptureSettingsMenuItemSnapToNearbyEdges.Checked then
                                               SetSpinEditValue( ToolsForm.LeftSpinEdit, FindNearestVerticalLine( ToolsForm.LeftSpinEdit.Value, 1 ) );
                                            Inc( Cursor );
                                      end;
                    ctTopLine       : begin //TopSpinEdit.Value := NormalizedPosition( Y );
                                            CursorRect := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
                                            HideCursor( True );
                                            if ToolsForm.CaptureSettingsMenuItemSnapToNearbyEdges.Checked then
                                               SetSpinEditValue( ToolsForm.TopSpinEdit, FindNearestHorizontalLine( ToolsForm.TopSpinEdit.Value, 1 ) );
                                            Inc( Cursor );
                                      end;
                    ctRightLine     : begin //WidthSpinEdit.Value  := NormalizedPosition( CursorRect.Left ) - LeftSpinEdit.Value; // + 1;
                                            CursorRect := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
                                            HideCursor( True );
                                            if ToolsForm.CaptureSettingsMenuItemSnapToNearbyEdges.Checked then
                                               SetSpinEditValue( ToolsForm.WidthSpinEdit, FindNearestVerticalLine( ToolsForm.LeftSpinEdit.Value + ToolsForm.WidthSpinEdit.Value, 1 ) - ToolsForm.LeftSpinEdit.Value );
                                            Inc( Cursor );
                                            UserWidth := ToolsForm.WidthSpinEdit.Value;
                                      end;
                    ctBottomLine    : begin //HeightSpinEdit.Value  := NormalizedPosition( CursorRect.Top ) - TopSpinEdit.Value; // + 1;
                                            CursorRect := Rect( 0, 0,ToolsForm. CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
                                            HideCursor( True );
                                            if ToolsForm.CaptureSettingsMenuItemSnapToNearbyEdges.Checked then
                                               SetSpinEditValue( ToolsForm.HeightSpinEdit, FindNearestHorizontalLine( ToolsForm.TopSpinEdit.Value + ToolsForm.HeightSpinEdit.Value, 1 ) - ToolsForm.TopSpinEdit.Value );
                                            Inc( Cursor );
                                            Step := csColumnsRows;
                                            UserHeight := ToolsForm.HeightSpinEdit.Value;
                                      end;
                    ctColumnLine    : begin CursorRect := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
                                            HideCursor( True );
                                            SetSpinEditValue( ToolsForm.ColWidthSpinEdit, FindBestMatch( ToolsForm.ColumnsSpinEdit.Value, ToolsForm.ColWidthSpinEdit.Value, ToolsForm.WidthSpinEdit.Value, 1, ColCount, ColWidth, GridWidth ) );
                                            SetSpinEditValue( ToolsForm.ColumnsSpinEdit,  ColCount );
                                            SetSpinEditValue( ToolsForm.WidthSpinEdit, ToolsForm.ColumnsSpinEdit.Value * ToolsForm.ColWidthSpinEdit.Value );
                                            Inc( Cursor );
                                            // check if the board squares seem like having identical column width and row height
                                            GridHeight := ToolsForm.HeightSpinEdit.Value;
                                            RowHeight := Max( 1, ColWidth );
                                            RowCount := ( GridHeight + Pred( RowHeight ) ) div RowHeight;
                                            if FindBestMatch( RowCount, RowHeight, GridHeight, 1, RowCount, RowHeight, GridHeight ) = ColWidth then begin
                                               SetSpinEditValue( ToolsForm.RowHeightSpinEdit, RowHeight );
                                               SetSpinEditValue( ToolsForm.RowsSpinEdit, RowCount );
                                               SetSpinEditValue( ToolsForm.HeightSpinEdit, ToolsForm.RowsSpinEdit.Value * ToolsForm.RowHeightSpinEdit.Value );
                                               MouseButtonDown := True;
                                               CaptureImage1MouseUp( Sender, Button, Shift, X, Y );
                                               end;
                                      end;
                    ctRowLine       : begin CursorRect := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
                                            HideCursor( True );
                                            SetSpinEditValue( ToolsForm.RowHeightSpinEdit, FindBestMatch( ToolsForm.RowsSpinEdit.Value, ToolsForm.RowHeightSpinEdit.Value, ToolsForm.HeightSpinEdit.Value, 1, RowCount, RowHeight, GridHeight ) );
                                            SetSpinEditValue( ToolsForm.RowsSpinEdit, RowCount );
                                            SetSpinEditValue( ToolsForm.HeightSpinEdit, ToolsForm.RowsSpinEdit.Value * ToolsForm.RowHeightSpinEdit.Value );
                                            Cursor := ctSelection;
                                            HasSelection := True;
                                            ShowGrid( ScaledBoardRect );
                                            //NextStepButtonClick( Sender );
                                      end;
                  end;
                  if Cursor >= ctLeftLine then begin
                     Point:=ToolsForm.CaptureImage1.ScreenToClient(Mouse.CursorPos);
                     CaptureImage1MouseMove(Sender,Shift,Point.X,Point.Y);
                     end;
                  end;
          IsSelectingANewArea:=False; ToggledItemPos.X:=-1; ToggledItemPos.Y:=-1;
          end
       else begin
          MouseButtonDown:=False; SizeHandle:=ghNull; IsDragging:=False;
          IsSelectingANewArea:=False; ToggledItemPos.X:=-1; ToggledItemPos.Y:=-1;
          Point:=ToolsForm.CaptureImage1.ScreenToClient(Mouse.CursorPos);
          CaptureImage1MouseMove(Sender,Shift,Point.X,Point.Y);
          end;

       if Editor.Cursor<>ctSelection then with Editor.History do
          EndTransaction( True );
       //if Editor.Selection.Enabled then SetCaptureDrawingToolHint;
       ShowStatus;
       end
    else MouseButtonDown := False;
    end;
end;

function  TCaptureForm.SetSpinEditValue( SpinEdit__ : TSpinEdit; Value__ : Integer ) : Integer;
begin // updates the spin edit value; returns the actual spin edit value after the update, which may differ from the specified value because of [min..max] clamping
  Inc( Editor.SpinEditChangeIsUserInput );
  try     SpinEdit__.Value := Value__;
  finally Dec( Editor.SpinEditChangeIsUserInput );
          SpinEdit__.Tag := SpinEdit__.Value;
  end;
  Result := SpinEdit__.Value;
end;

procedure TCaptureForm.SpinEditChange(Sender: TObject);
var OriginalTag : Integer; R :TRect;
begin
  if Sender is TSpinEdit then with Sender as TSpinEdit do begin
     Inc( Editor.SpinEditChangeIsUserInput );
     try
       OriginalTag                                        := Tag;
       ToolsForm.LeftSpinEdit.MaxValue                    := Max( 0, OriginalBitmap.Width  - 2 - MIN_BOARD_SIZE_PIXELS );
       ToolsForm.LeftSpinEdit2.MaxValue                   := ToolsForm.LeftSpinEdit.MaxValue;
       ToolsForm.TopSpinEdit.MaxValue                     := Max( 0, OriginalBitmap.Height - 2 - MIN_BOARD_SIZE_PIXELS );
       ToolsForm.TopSpinEdit2.MaxValue                    := ToolsForm.TopSpinEdit.MaxValue;
       ToolsForm.WidthSpinEdit.MaxValue                   := OriginalBitmap.Width  - 2 - ToolsForm.LeftSpinEdit.Value; // NormalizedSize( CaptureImage1.Picture.Bitmap.Width   - 2 - Editor.SelectionRect.Left ); // clamping to [ min-value .. max-value ] interval
       ToolsForm.HeightSpinEdit.MaxValue                  := OriginalBitmap.Height - 2 - ToolsForm.TopSpinEdit.Value; //NormalizedSize( CaptureImage1.Picture.Bitmap.Height  - 2 - Editor.SelectionRect.Top  );
       ToolsForm.ColumnsSpinEdit.MaxValue                 := Min( CAPTURE_MAX_BOARD_WIDTH , ToolsForm.WidthSpinEdit .MaxValue );
       ToolsForm.RowsSpinEdit.MaxValue                    := Min( CAPTURE_MAX_BOARD_HEIGHT, ToolsForm.HeightSpinEdit.MaxValue );
       if Editor.Cursor = ctSelection then begin
          ToolsForm.ColWidthSpinEdit.MaxValue             := ToolsForm.WidthSpinEdit .MaxValue div Max( 1, ToolsForm.ColumnsSpinEdit.Value ); // NormalizedSize( CaptureImage1.Picture.Bitmap.Width   - 2 - Editor.SelectionRect.Left ) div Max( 1, ColumnsSpinEdit.Value );
          ToolsForm.RowHeightSpinEdit.MaxValue            := ToolsForm.HeightSpinEdit.MaxValue div Max( 1, ToolsForm.RowsSpinEdit   .Value ); //NormalizedSize( CaptureImage1.Picture.Bitmap.Height  - 2 - Editor.SelectionRect.Top  ) div Max( 1, RowsSpinEdit   .Value );
          end
       else begin
          ToolsForm.ColWidthSpinEdit.MaxValue             := ToolsForm.WidthSpinEdit.MaxValue  div 2;
          ToolsForm.RowHeightSpinEdit.MaxValue            := ToolsForm.HeightSpinEdit.MaxValue div 2;
          end;
       ToolsForm.OuterWallCutLeftSpinEdit.MaxValue        := Max( 0, ( ToolsForm.ColWidthSpinEdit.Value - 2 ) div 2 ); // '-2': first and last pixel in the row isn't used for outer wall cut pixel codes
       ToolsForm.OuterWallCutRightSpinEdit.MaxValue       := ToolsForm.OuterWallCutLeftSpinEdit.MaxValue;
       ToolsForm.OuterWallCutTopSpinEdit.MaxValue         := Min( ToolsForm.RowHeightSpinEdit.Value, ToolsForm.OuterWallCutLeftSpinEdit.MaxValue );
       ToolsForm.OuterWallCutBottomSpinEdit.MaxValue      := ToolsForm.OuterWallCutTopSpinEdit.MaxValue;

       try    if  Trim(Text)='' then begin // the text is allowed to the blank; when the spinedit control looses focus, a blank text is changed to '0'
                  if Text<>'' then Text:='';
                  end
              else begin
                 if (StrToInt(Text)=0) then begin // 'StrToInt': check if it's a legal integer value; if not, then 'StrToInt' raises an 'EConvertError' exception
                    end;
                 if MinValue<MaxValue then
                    if      Value<MinValue then
                            Value:=MinValue
                    else if Value>MaxValue then
                            Value:=MaxValue;
                 if Value<>0 then
                    while (Text<>'') and (Text[1]='0') do Text:=Copy(Text,2,MaxInt); // trim unnecessary leading zero characters
                 end;
       except on E:EConvertError do Text:=IntToStr(Tag); // restore the last valid value
       end;
       if Trim(Text)<>'' then begin
          //Self.Modified:=Self.Modified or (Value<>Tag);
          Tag:=Value;
          end
       else begin
          //Self.Modified:=Self.Modified or (Tag<>MinValue);
          Tag:=MinValue;
          end;

       Editor.Board.Width                       := ToolsForm.ColumnsSpinEdit.Value;
       Editor.Board.Height                      := ToolsForm.RowsSpinEdit   .Value;

       if (      Sender = ToolsForm.LeftSpinEdit        ) or ( Sender = ToolsForm.LeftSpinEdit2 ) or
          (      Sender = ToolsForm.TopSpinEdit         ) or ( Sender = ToolsForm.TopSpinEdit2 ) or
          (      Sender = ToolsForm.WidthSpinEdit       ) or ( Sender = ToolsForm.HeightSpinEdit ) or
          (      Sender = ToolsForm.ColumnsSpinEdit     ) or ( Sender = ToolsForm.RowsSpinEdit ) or
          (      Sender = ToolsForm.ColWidthSpinEdit    ) or ( Sender = ToolsForm.RowHeightSpinEdit ) then begin
          Colors.Count                          := 0; // color quantization needs recalculation
          if Editor.CompletedStep                    >= csSavePuzzle then
             Editor.CompletedStep                    := Pred( csSavePuzzle );
          if ( ( Sender = ToolsForm.ColumnsSpinEdit     ) or ( Sender = ToolsForm.RowsSpinEdit ) or ( Abs( Value - OriginalTag ) > 10 ) ) // '>10' the user probably didn't just nudge the grid into place
             and
             ( Editor.CompletedStep             >= Pred( csBoardSquares ) ) then begin
               Editor.CompletedStep             := Pred( csBoardSquares );
               ClearBoard;
               end;
          if ( Sender = ToolsForm.ColWidthSpinEdit ) or ( Sender = ToolsForm.RowHeightSpinEdit ) then begin
               Editor.EditorSkinBitmap.Free; Editor.EditorSkinBitmap := nil; Editor.EditorSkinTileCount := 0;
               end;

          if ( Editor.Step                           >= csBoard)
             or
             ( Editor.CompletedStep                  >= csBoard ) then with Editor do begin
             if      Sender                          =  ToolsForm.LeftSpinEdit   then begin
                     if Editor.SpinEditChangeIsUserInput = 1 then begin
                        if ToolsForm.LeftSpinEdit.Value + ToolsForm.WidthSpinEdit.Value > OriginalBitmap.Width  - 2 then
                           ToolsForm.WidthSpinEdit.Value := OriginalBitmap.Width  - 2 - ToolsForm.LeftSpinEdit.Value;
                        UserWidth                    := ToolsForm.WidthSpinEdit.Value;
                        ShowStatus;
                        Editor.CursorRect            := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
                        ShowCursor( Editor.Cursor, ScaledBoardRect );
                        end;
                     ToolsForm.LeftSpinEdit2.Value   := ToolsForm.LeftSpinEdit.Value;
                     end
             else if Sender                          =  ToolsForm.LeftSpinEdit2  then begin
                     Dec( Editor.SpinEditChangeIsUserInput );
                     try     ToolsForm.LeftSpinEdit.Value   := ToolsForm.LeftSpinEdit2.Value;
                     finally Inc( Editor.SpinEditChangeIsUserInput );
                     end;
                     end
             else if Sender                          =  ToolsForm.TopSpinEdit    then begin
                     if Editor.SpinEditChangeIsUserInput = 1 then begin
                        if ToolsForm.TopSpinEdit.Value + ToolsForm.HeightSpinEdit.Value > OriginalBitmap.Height - 2 then
                           ToolsForm.HeightSpinEdit.Value      := OriginalBitmap.Height - 2 - ToolsForm.TopSpinEdit.Value;
                        UserHeight                   := ToolsForm.HeightSpinEdit.Value;
                        if Editor.Cursor             =  ctLeftLine then Editor.Cursor := ctTopLine;
                        ShowStatus;
                        Editor.CursorRect            := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
                        ShowCursor( Editor.Cursor, ScaledBoardRect );
                        end;
                     ToolsForm.TopSpinEdit2.Value    := ToolsForm.TopSpinEdit.Value;
                     end
             else if Sender                          =  ToolsForm.TopSpinEdit2  then begin
                     Dec( Editor.SpinEditChangeIsUserInput );
                     try     ToolsForm.TopSpinEdit.Value := ToolsForm.TopSpinEdit2.Value;
                     finally Inc( Editor.SpinEditChangeIsUserInput );
                     end;
                     end
             else if Sender                          =  ToolsForm.WidthSpinEdit  then begin
                     if Editor.SpinEditChangeIsUserInput = 1 then begin
                        if ToolsForm.WidthSpinEdit.Value       <  MIN_BOARD_SIZE_PIXELS then
                           ToolsForm.WidthSpinEdit.Value       := MIN_BOARD_SIZE_PIXELS;
                        //if HeightSpinEdit.Value    <  MIN_BOARD_SIZE_PIXELS then
                        //   HeightSpinEdit.Value    := MIN_BOARD_SIZE_PIXELS;
                        UserWidth                    := ToolsForm.WidthSpinEdit.Value;
                        if Editor.CompletedStep      >= csColumnsRows then
                           if   ToolsForm.WidthSpinEdit.Value  >= OriginalTag then
                                ToolsForm.WidthSpinEdit.Value  := ToolsForm.WidthSpinEdit.Value + ToolsForm.ColumnsSpinEdit.Value - ( ToolsForm.WidthSpinEdit.Value mod Max( 1, ToolsForm.ColumnsSpinEdit.Value ) )
                           else ToolsForm.WidthSpinEdit.Value  := ToolsForm.WidthSpinEdit.Value                                   - ( ToolsForm.WidthSpinEdit.Value mod Max( 1, ToolsForm.ColumnsSpinEdit.Value ) );
                        if ( Editor.Cursor >= ctLeftLine ) and ( Editor.Cursor < ctRightLine ) then Editor.Cursor := ctRightLine;
                        ShowStatus;
                        Editor.CursorRect       := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
                        ShowCursor( Editor.Cursor, ScaledBoardRect );
                        end;
                     end
             else if Sender                          =  ToolsForm.HeightSpinEdit then begin
                     if Editor.SpinEditChangeIsUserInput = 1 then begin
                        if ToolsForm.WidthSpinEdit.Value       <  MIN_BOARD_SIZE_PIXELS then
                           ToolsForm.WidthSpinEdit.Value       := MIN_BOARD_SIZE_PIXELS;
                        if ToolsForm.HeightSpinEdit.Value      <  MIN_BOARD_SIZE_PIXELS then
                           ToolsForm.HeightSpinEdit.Value      := MIN_BOARD_SIZE_PIXELS;
                        UserHeight                   := ToolsForm.HeightSpinEdit.Value;
                        if Editor.CompletedStep      >= csColumnsRows then
                           if   ToolsForm.HeightSpinEdit.Value >= OriginalTag then
                                ToolsForm.HeightSpinEdit.Value := ToolsForm.HeightSpinEdit.Value + ToolsForm.RowsSpinEdit.Value  - ( ToolsForm.HeightSpinEdit.Value mod Max( 1, ToolsForm.RowsSpinEdit.Value ) )
                           else ToolsForm.HeightSpinEdit.Value := ToolsForm.HeightSpinEdit.Value                                 - ( ToolsForm.HeightSpinEdit.Value mod Max( 1, ToolsForm.RowsSpinEdit.Value ) );
                        if ( Editor.Cursor >= ctLeftLine ) and ( Editor.Cursor < ctBottomLine ) then Editor.Cursor := ctBottomLine;
                        ShowStatus;
                        Editor.CursorRect            := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
                        ShowCursor( Editor.Cursor, ScaledBoardRect );
                        end;
                     end;
             end;

          if ( Editor.Step                           >= csColumnsRows )
             or
             ( Editor.CompletedStep                  >= csColumnsRows ) then begin
             R := NormalizedBoardRect; R.Right       := R.Left + Editor.UserWidth; R.Bottom := R.Top + Editor.UserHeight; // 'UserWidth/Height' is the last rectangle entered by the user which hasn't been clamped to columns and rows
             RectangleClampedToColumnsRows( R, ToolsForm.ColumnsSpinEdit.Value, ToolsForm.RowsSpinEdit.Value, R );
             if      Sender                          =  ToolsForm.ColumnsSpinEdit then begin
                     if ( ToolsForm.ColumnsSpinEdit.Value > 0 ) and ( ToolsForm.ColWidthSpinEdit.Value > 0 ) and ( Editor.SpinEditChangeIsUserInput = 1 ) then begin
                        if   ToolsForm.LockColumnWidthAndRowHeightCheckBox.Checked then begin
                             if ToolsForm.ColumnsSpinEdit.Value * ToolsForm.ColWidthSpinEdit.Value > ToolsForm.WidthSpinEdit.MaxValue then
                                ToolsForm.ColumnsSpinEdit.Value := Max( 1, ToolsForm.WidthSpinEdit.MaxValue div ToolsForm.ColWidthSpinEdit.Value );
                             Editor.UserWidth                  := ToolsForm.ColumnsSpinEdit.Value * ToolsForm.ColWidthSpinEdit.Value;
                             end
                        else ToolsForm.ColWidthSpinEdit.Value  := RectWidth ( R ) div ToolsForm.ColumnsSpinEdit.Value;
                        ToolsForm.WidthSpinEdit.Value          := ToolsForm.ColumnsSpinEdit.Value * ToolsForm.ColWidthSpinEdit.Value;
                        end
                     else if ( ToolsForm.ColumnsSpinEdit.Value  =  0 ) and ( ToolsForm.ColWidthSpinEdit.Value > 0 ) and ( Editor.SpinEditChangeIsUserInput = 1 ) then
                             ToolsForm.ColumnsSpinEdit.Value   := 1;
                     end
             else if Sender                          =  ToolsForm.RowsSpinEdit then begin
                     if ( ToolsForm.RowsSpinEdit.Value    > 0 ) and ( ToolsForm.RowHeightSpinEdit.Value   > 0 ) and ( Editor.SpinEditChangeIsUserInput = 1 ) then begin
                        if   ToolsForm.LockColumnWidthAndRowHeightCheckBox.Checked then begin
                             if ToolsForm.RowsSpinEdit.Value * ToolsForm.RowHeightSpinEdit.Value > ToolsForm.HeightSpinEdit.MaxValue then
                                ToolsForm.RowsSpinEdit.Value := Max( 1, ToolsForm.HeightSpinEdit.MaxValue div ToolsForm.RowHeightSpinEdit.Value );
                             Editor.UserHeight                 := ToolsForm.RowsSpinEdit.Value * ToolsForm.RowHeightSpinEdit.Value;
                             end
                        else ToolsForm.RowHeightSpinEdit.Value := RectHeight( R ) div ToolsForm.RowsSpinEdit.Value;
                        ToolsForm.HeightSpinEdit.Value         := ToolsForm.RowsSpinEdit.Value * ToolsForm.RowHeightSpinEdit.Value;
                        end
                     else if ( ToolsForm.RowsSpinEdit.Value    =  0 ) and ( ToolsForm.RowHeightSpinEdit.Value > 0 ) and ( Editor.SpinEditChangeIsUserInput = 1 ) then
                             ToolsForm.RowsSpinEdit.Value      := 1;
                     end
             else if Sender                          =  ToolsForm.ColWidthSpinEdit then begin
                     if Editor.SpinEditChangeIsUserInput = 1 then begin
                        if ToolsForm.ColWidthSpinEdit.Value    <  MIN_BOARD_SQUARE_SIZE_PIXELS then
                           ToolsForm.ColWidthSpinEdit.Value    := MIN_BOARD_SQUARE_SIZE_PIXELS;
                        if Editor.Cursor             <> ctSelection then
                           SetSpinEditValue( ToolsForm.ColumnsSpinEdit, Max( 1, Min( OriginalBitmap.Width - 2 - ToolsForm.LeftSpinEdit.Value, ToolsForm.WidthSpinEdit.Value + 1 + ToolsForm.ColumnsSpinEdit.Value ) div Max( 1, ToolsForm.ColWidthSpinEdit.Value ) ) );
                        if ( ToolsForm.ColumnsSpinEdit.Value   >  0 ) and ( ToolsForm.ColWidthSpinEdit.Value > 0 ) and ( Editor.Cursor <> ctColumnLine ) then begin
                           ToolsForm.WidthSpinEdit.Value       := ToolsForm.ColumnsSpinEdit.Value * ToolsForm.ColWidthSpinEdit.Value;
                           Editor.UserWidth          := ToolsForm.WidthSpinEdit.Value;
                           end;
                        ShowStatus;
                        Editor.CursorRect            := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
                        ShowCursor( Editor.Cursor, ScaledBoardRect );
                        end;
                     end
             else if Sender                          =  ToolsForm.RowHeightSpinEdit then begin
                     if Editor.SpinEditChangeIsUserInput = 1 then begin
                        if Editor.Cursor             =  ctColumnLine then begin
                           if ToolsForm.ColWidthSpinEdit.Value <  MIN_BOARD_SQUARE_SIZE_PIXELS then
                           ToolsForm.ColWidthSpinEdit.Value    := MIN_BOARD_SQUARE_SIZE_PIXELS;
                           SetSpinEditValue( ToolsForm.ColumnsSpinEdit, Max( 1, ( Max( Editor.UserWidth, ToolsForm.WidthSpinEdit.Value ) + 2 ) div Max( 1, ToolsForm.ColWidthSpinEdit.Value ) ) );
                           ToolsForm.WidthSpinEdit.Value       := ToolsForm.ColumnsSpinEdit.Value * ToolsForm.ColWidthSpinEdit.Value;
                           Editor.UserWidth          := ToolsForm.WidthSpinEdit.Value;
                           Editor.Cursor             := ctRowLine;
                           end;
                        //if ColWidthSpinEdit.Value  <  MIN_BOARD_SQUARE_SIZE_PIXELS then
                        //   ColWidthSpinEdit.Value  := MIN_BOARD_SQUARE_SIZE_PIXELS;
                        if ToolsForm.RowHeightSpinEdit.Value   <  MIN_BOARD_SQUARE_SIZE_PIXELS then
                           ToolsForm.RowHeightSpinEdit.Value   := MIN_BOARD_SQUARE_SIZE_PIXELS;
                        if Editor.Cursor             <> ctSelection then
                           SetSpinEditValue( ToolsForm.RowsSpinEdit, Max( 1, Min( OriginalBitmap.Height - 2 - ToolsForm.TopSpinEdit.Value, ToolsForm.HeightSpinEdit.Value + 1 + ToolsForm.RowsSpinEdit.Value ) div Max( 1, ToolsForm.RowHeightSpinEdit.Value ) ) );
                        if ( ToolsForm.RowsSpinEdit.Value      >  0 ) and ( ToolsForm.RowHeightSpinEdit.Value > 0 ) and ( Editor.Cursor <> ctRowLine ) then begin
                           ToolsForm.HeightSpinEdit.Value      := ToolsForm.RowsSpinEdit.Value * ToolsForm.RowHeightSpinEdit.Value;
                           Editor.UserHeight         := ToolsForm.HeightSpinEdit.Value;
                           end;
                        ShowStatus;
                        Editor.CursorRect            := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
                        ShowCursor( Editor.Cursor, ScaledBoardRect );
                        if Editor.Cursor             =  ctRowLine then begin // leave 'row height' entry mode when the user changes the row height spin edit manually
                           ToolsForm.HeightSpinEdit.Value      := ToolsForm.RowsSpinEdit.Value * ToolsForm.RowHeightSpinEdit.Value;
                           Editor.UserHeight         := ToolsForm.HeightSpinEdit.Value;
                           Editor.Cursor             := ctSelection;
                           Editor.HasSelection       := True;
                           ShowStatus;
                           ShowCursor( Editor.Cursor, ScaledBoardRect );
                           end;
                        end;
                     end;
             end;

          if ( Editor.Cursor < ctLeftLine ) and
             ( Editor.SpinEditChangeIsUserInput = 1 ) then begin
             if   ( ( Editor.Step >= csColumnsRows ) or ( Editor.CompletedStep >= csColumnsRows ) ) and
                  RectangleClampedToColumnsRows( NormalizedBoardRect, ToolsForm.ColumnsSpinEdit.Value, ToolsForm.RowsSpinEdit.Value, R ) then begin
                  ToolsForm.LeftSpinEdit.Value   := R.Left;
                  ToolsForm.TopSpinEdit.Value    := R.Top;
                  ToolsForm.WidthSpinEdit.Value  := RectWidth ( R );
                  ToolsForm.HeightSpinEdit.Value := RectHeight( R );
                  end;
             end;

          if Editor.SpinEditChangeIsUserInput = 1 then
             ShowGrid( ScaledBoardRect );
          end
       else
          if ( Editor.Step = csSaveSkin )
             and
             (
              (    Sender = ToolsForm.OuterWallCutLeftSpinEdit        ) or ( Sender = ToolsForm.OuterWallCutTopSpinEdit ) or
              (    Sender = ToolsForm.OuterWallCutRightSpinEdit       ) or ( Sender = ToolsForm.OuterWallCutBottomSpinEdit )
             ) then begin
             if Value < 0 then
                Value := 0;
             WriteSkinSettings;
             ShowImage( Editor.Zoom );
             end;

     finally
       Dec( Editor.SpinEditChangeIsUserInput );

       ToolsForm.WidthSpinEdit.MaxValue                   := 0; // remove the maximum values again so other parts of the program can set the values
       ToolsForm.HeightSpinEdit.MaxValue                  := 0;
       ToolsForm.ColumnsSpinEdit.MaxValue                 := 0;
       ToolsForm.RowsSpinEdit.MaxValue                    := 0;
       ToolsForm.ColWidthSpinEdit.MaxValue                := 0;
       ToolsForm.RowHeightSpinEdit.MaxValue               := 0;
       ToolsForm.OuterWallCutLeftSpinEdit.MaxValue        := 0;
       ToolsForm.OuterWallCutTopSpinEdit.MaxValue         := 0;
       ToolsForm.OuterWallCutRightSpinEdit.MaxValue       := 0;
       ToolsForm.OuterWallCutBottomSpinEdit.MaxValue      := 0;
     end;
     end
  else
     ShowStatus;
end;

procedure TCaptureForm.SpinEditExit(Sender: TObject);
var s:String;
begin
  if Sender is TSpinEdit then with Sender as TSpinEdit do
     try    s:=IntToStr(Value);      // trim unnecessary leading zero characters, if any, and interpret a blank text as the minimum value; 'IntToStr' raises an 'EConvertError' if the characters in the spinedit text field isn't a valid integer
            if s<>Text then Text:=s; // update the text, if necessary
     except on E:EConvertError do Text:=IntToStr(MinValue);
     end;
end;

procedure TCaptureForm.FormResize(Sender: TObject);
var Limit : Integer;
begin
  ToolsForm.CaptureImagePanel.Left             := 0;

  ToolsForm.CaptureImagePanel.Height           := ToolsForm.TabSheetCapture.ClientHeight - ToolsForm.BottomPanel.Height {- ToolsForm.StatusBar1.Height} - ToolsForm.CaptureImagePanel.Top;
  ToolsForm.CaptureBoardPanel.Left             := ToolsForm.CaptureImagePanel.Left;
  ToolsForm.CaptureBoardPanel.Height           := ToolsForm.CaptureImagePanel.Height;
  ToolsForm.CaptureGridPanel.Left              := ToolsForm.CaptureImagePanel.Left;
  ToolsForm.CaptureGridPanel.Height            := ToolsForm.CaptureImagePanel.Height;
  ToolsForm.CaptureSquaresPanel.Left           := ToolsForm.CaptureImagePanel.Left;
  ToolsForm.CaptureSquaresPanel.Height         := ToolsForm.CaptureImagePanel.Height;
  ToolsForm.CapturePuzzlePanel.Left            := ToolsForm.CaptureImagePanel.Left;
  ToolsForm.CapturePuzzlePanel.Height          := ToolsForm.CaptureImagePanel.Height;
  ToolsForm.CaptureSkinPanel.Left              := ToolsForm.CaptureImagePanel.Left;
  ToolsForm.CaptureSkinPanel.Height            := ToolsForm.CaptureImagePanel.Height;

  ToolsForm.CaptureEditToolBarLeft.Left        := ( ToolsForm.CaptureSquaresPanel.ClientWidth - ToolsForm.CaptureEditToolBarLeft.Width ) div 2;
  ToolsForm.CaptureEditToolBarLeft.Height      := ToolsForm.CaptureSquaresPanel.Height - ToolsForm.CaptureEditToolBarLeft.Top - 4;

  ToolsForm.CaptureScrollBox.Left              := ToolsForm.CaptureImagePanel.Left + ToolsForm.CaptureImagePanel.Width;
  ToolsForm.CaptureScrollBox.Top               := ToolsForm.CaptureToolBarTop.Top  + ToolsForm.CaptureToolBarTop.Height;
//CaptureScrollBox.Width                       := RightPanel.Left - ( ImagePanel.Left + ImagePanel.Width );
//CaptureScrollBox.Height                      := BottomPanel.Top - ( TopPanel.Top + TopPanel.Height );
  ToolsForm.CaptureScrollBox.Width             := ToolsForm.TabSheetCapture.ClientWidth  {- RightPanel.Width} - ( ToolsForm.CaptureImagePanel.Left + ToolsForm.CaptureImagePanel.Width + 2 );
  ToolsForm.CaptureScrollBox.Height            := ToolsForm.CaptureImagePanel.Height;
  ToolsForm.PuzzleInformationPanel.Left        := ToolsForm.BottomPanel.ClientWidth - ToolsForm.PuzzleInformationPanel.Width -2;
  ToolsForm.SkinInformationPanel.Left          := ToolsForm.PuzzleInformationPanel.Left;

//CaptureScrollBox.SetBounds( ImagePanel.Left + ImagePanel.Width, TopPanel.Top + TopPanel.Height, RightPanel.Left - ( ImagePanel.Left + ImagePanel.Width ), BottomPanel.Top - ( TopPanel.Top + TopPanel.Height ) );
  if   Editor.Step < csSavePuzzle then
       ToolsForm.CaptureStateMemo.Width :=           ToolsForm.BottomPanel.ClientWidth - ToolsForm.CaptureStateMemo.Left - ToolsForm.PreviousStepButton.Left
//else StateMemo.Width := Min( 200, BottomPanel.ClientWidth - StateMemo.Left - PreviousStepButton.Left );
  else ToolsForm.CaptureStateMemo.Width := ToolsForm.PuzzleInformationPanel.Left - ToolsForm.CaptureStateMemo.Left;
  with ToolsForm.CaptureImage1 do begin
    if RectWidth( Editor.CursorRect ) = 0 then begin
       ToolsForm.CaptureScrollBox.HorzScrollBar.Position := 0;
       ToolsForm.CaptureScrollBox.VertScrollBar.Position := 0;
       end;
    end;

  with ToolsForm do begin // only some of the components on the form risk being invisible; avoid that partially visible components ruin the layout by overwriting panel borders
    Limit := CaptureImagePanel.ClientHeight - 4;
    with WallCutLeftLabel do Visible := Top + Height < Limit;
    with WallCutTopLabel do Visible := Top + Height < Limit;
    with WallCutRightLabel do Visible := Top + Height < Limit;
    with WallCutBottomLabel do Visible := Top + Height < Limit;
    with OuterWallCutLeftSpinEdit do Visible := Top + Height < Limit;
    with OuterWallCutTopSpinEdit do Visible := Top + Height < Limit;
    with OuterWallCutRightSpinEdit do Visible := Top + Height < Limit;
    with OuterWallCutBottomSpinEdit do Visible := Top + Height < Limit;
    end;

  ScrollInView( ScaledBoardRect );
  ShowBoard;
end;

function  TCaptureForm.MinHeight:Integer;
begin
  Result:=480;
end;

function  TCaptureForm.MinWidth:Integer;
begin
  Result:=640;
end;

procedure TCaptureForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize := ( NewWidth >= MinWidth ) and ( NewHeight >= MinHeight );
end;

function  TCaptureForm.CloseCapture : Boolean;
var i : Integer; s, s2 : String;
begin // not in production
  Result := not Editor.Modified;
  if not Result then begin
        s2 := Editor.InputFileName;
        if   StrBeginsWith( s2, TITLE_ILLEGAL_FIRST_CHARACTER ) then
             s := CaptureNewText    +NL+NL+DoYouWantToSaveItText
        else s := CaptureChangedText+NL+NL+DoYouWantToSaveItText;
        i:=MB_YESNOCANCEL;
        case Msg(s,
                 Application.Title + ' - ' + s2,
                 i+MB_ICONQUESTION) of
          IDYES    : begin //if Sender=EditForm then
                           //   Result:=EditForm.IsALegalLevel;
                           Result := Save( Self ) and ( not Editor.Modified );
                     end;
          IDNO     : begin
                       Clear;
                     end;
          IDCANCEL : Result:=False;
        end; // case
        end
  else begin
     Clear;
     end;
end;

procedure TCaptureForm.ScrollInView( Rect__ : TRect );
var P1, P2 : TPoint;
begin
  if Editor.HasSelection or ( Editor.Cursor >= ctTopLine ) or ( Editor.Step >= csBoardSquares ) then with ToolsForm.CaptureScrollBox do begin
     if RectWidth ( Rect__ ) = 0 then
        Rect__.Right  := ToolsForm.CaptureImage1.Picture.Bitmap.Width  - 2;
     if RectHeight( Rect__ ) = 0 then
        Rect__.Bottom := ToolsForm.CaptureImage1.Picture.Bitmap.Height - 2;
     P1.X := Rect__.Left + ( RectWidth ( Rect__ ) div 2 );
     P1.Y := Rect__.Top  + ( RectHeight( Rect__ ) div 2 );
     if ClientWidth > 0 then with HorzScrollBar do begin
        if ( Position <= Rect__.Left ) and ( Position + ClientWidth  >= Rect__.Right  ) then // the rectangle is in view horizontally
        else begin
          P2.X := Max( 0, Min( Range - ClientWidth, P1.X - ( ClientWidth div 2 ) ) );
          Position := Max( 0, P2.X - 10 );
          end;
        end;
     if ClientHeight > 0 then with VertScrollBar do begin
        if ( Position <= Rect__.Top  ) and ( Position + ClientHeight >= Rect__.Bottom ) then // the rectangle is in view vertically
        else begin
          P2.Y := Max( 0, Min( Range - ClientHeight, P1.Y - ( ClientHeight div 2 ) ) );
          Position := Max( 0, P2.Y - 10 );
          end;
        end;
     end;
end;

function  TCaptureForm.CreateEditorSkin( Bitmap__ : TBitmap; TileCount__,ColWidth__, RowHeight__ : Integer ) : Boolean;
var Index, TileWidth, TileHeight : Integer; B: Boolean; R : TRect;
    B1, B2 : TBitmap;
begin // scales the editor skin image to fit the board size; the tiles must be
      // scaled individually to avoid smearing;
      // the input bitmap is a single row skin image;
  B1 := nil; B2 := nil;
  try
    try
      Editor.EditorSkinBitmap.Free; Editor.EditorSkinBitmap := nil;
      Editor.EditorSkinTileCount := TileCount__;
      TileHeight     := Bitmap__.Height;
      TileWidth      := Bitmap__.Width div Editor.EditorSkinTileCount;
      if ColWidth__  <= 0 then
         ColWidth__  := ScaledSize( ToolsForm.ColWidthSpinEdit .Value );
      if RowHeight__ <= 0 then
         RowHeight__ := ScaledSize( ToolsForm.RowHeightSpinEdit.Value );
      Result         := ( ColWidth__  > 0 ) and
                        ( RowHeight__ > 0 ) and
                        BitmapCreate( Editor.EditorSkinBitmap, ColWidth__ * Editor.EditorSkinTileCount, RowHeight__  ) and
                        BitmapCreate( B1                     , TileWidth                              , TileHeight   );
                        BitmapCreate( B2                     , ColWidth__                             , RowHeight__  );
      if Result then
         for Index := 0 to Pred( Editor.EditorSkinTileCount ) do begin
             if ( TileWidth = ColWidth__ ) and ( TileHeight = RowHeight__ ) then with Editor.EditorSkinBitmap.Canvas do
                CopyRect(    Rect( Index * ColWidth__, 0, Succ( Index ) * ColWidth__, RowHeight__ ),
                             Bitmap__.Canvas,
                             Rect( Index * TileWidth , 0, Succ( Index ) * TileWidth , TileHeight  ) )
             else begin
                B1.Canvas.CopyRect( Rect( 0, 0, TileWidth, TileHeight ), Bitmap__.Canvas, Rect( Index * TileWidth, 0, Succ( Index ) * TileWidth, TileHeight ) );
                Result := Result and BitmapScale( B2, B1, 5, True, False, clBlack, 0, ivStretch, 0, 0, R, B );
                if Result then with Editor.EditorSkinBitmap.Canvas do
                   CopyRect( Rect( Index * ColWidth__, 0, Succ( Index ) * ColWidth__, RowHeight__ ),
                             B2.Canvas,
                             Rect( 0                 , 0, ColWidth__                , RowHeight__ ) );
                end;
             end;
       //if Result then
       //   Editor.EditorSkinBitmap.SaveToFile( 'tmp1.bmp' );

    finally B1.Free; B2.Free;
    end;
  except on E:Exception do begin
    Editor.EditorSkinBitmap.Free; Editor.EditorSkinBitmap := nil; Editor.EditorSkinTileCount := 0;
    Result := Error( E.Message, '' );
    end;
  end;
end;

procedure TCaptureForm.UpdateScrollBoxRange;
var XRange, YRange : Integer;
begin
  XRange := ToolsForm.CaptureImage1.Picture.Width;
  YRange := ToolsForm.CaptureImage1.Picture.Height;
  if ToolsForm.CaptureImage2.Visible then begin
     ToolsForm.CaptureImage2.ClientWidth  := ToolsForm.CaptureImage2.Picture.Width;
     ToolsForm.CaptureImage2.ClientHeight := ToolsForm.CaptureImage2.Picture.Height;
     XRange := Max( XRange, ToolsForm.CaptureImage2.Picture.Width );
     Inc( YRange, ToolsForm.CaptureImage2.Picture.Height );
     end;
  if ToolsForm.Memo1.Visible then
     Inc( YRange, ToolsForm.Memo1.Height );
  ToolsForm.CaptureScrollBox.HorzScrollBar.Range := XRange;
  ToolsForm.CaptureScrollBox.VertScrollBar.Range := YRange;
end;

procedure TCaptureForm.ShowImage( Zoom : Integer );
var R: TRect; B: TBitmap;
begin // precondition: 'Zoom' is 100, 200, or 400
  if Zoom <> Editor.Zoom then with Editor.CursorRect do begin
     Editor.CursorRect := NormalizedRect( Editor.CursorRect );
     Editor.Zoom := Zoom;
     Editor.CursorRect := ScaledRect( Editor.CursorRect );
     Editor.EditorSkinBitmap.Free; Editor.EditorSkinBitmap := nil; Editor.EditorSkinTileCount := 0;
     end;

  with ToolsForm.CaptureImage1.Picture.Bitmap do with Canvas do begin
    if ( Editor.Step < csSaveSkin ) or ( not Assigned( SkinBitmap ) ) then begin
       B := OriginalBitmap;
       Width  := ScaledSize( B.Width  - 2 ) + 2;
       Height := ScaledSize( B.Height - 2 ) + 2;
       end
    else begin
       B := SkinBitmap;
       Width  := ScaledSize( B.Width );
       Height := ScaledSize( B.Height );
       end;
    R := Rect( 0, 0, Width, Height );
    CopyMode := cmSrcCopy;
    if      Zoom = 100 then begin
            CopyRect( R, B.Canvas, R );
            ToolsForm.CaptureToolButtonZoom.ImageIndex := IMAGE_INDEX_ZOOM_IN;
            ToolsForm.CaptureViewMenuItem100.Checked := True;
            end
    else if Zoom = 200 then begin
            Canvas.StretchDraw( R, B );
            if   ToolsForm.CaptureViewMenuItem400.Enabled and
                 ( GetAvailableVirtualMemoryByteSize >= MIN_AVAILABLE_VIRTUAL_MEMORY_FOR_ZOOM_400 ) then
                 ToolsForm.CaptureToolButtonZoom.ImageIndex := IMAGE_INDEX_ZOOM_IN
            else ToolsForm.CaptureToolButtonZoom.ImageIndex := IMAGE_INDEX_ZOOM_OUT;
            ToolsForm.CaptureViewMenuItem200.Checked := True;
            end
         else begin
            Canvas.StretchDraw( R, B );
            ToolsForm.CaptureToolButtonZoom.ImageIndex := IMAGE_INDEX_ZOOM_OUT;
            ToolsForm.CaptureViewMenuItem400.Checked := True;
            end;

    if B = OriginalBitmap then begin // fill the 2 extra pixel columns/rows
       Pen.Style := psSolid;
       Pen.Width := 1; // '1': draw lines individually to avoid considering drawing conventions when pen width > 2
       Pen.Color := clBtnFace;
       MoveTo( Width - 2, 0  ); LineTo( Width - 2, Height );
       MoveTo( Width - 1, 0  ); LineTo( Width - 1, Height );
       MoveTo( 0, Height - 2 ); LineTo( Width - 1, Height - 2 );
       MoveTo( 0, Height - 1 ); LineTo( Width - 1, Height - 1 );
       end;
    end;

  with BackgroundBitmap do with Canvas do begin
    Width  := ToolsForm.CaptureImage1.Picture.Bitmap.Width;
    Height := ToolsForm.CaptureImage1.Picture.Bitmap.Height;
    R      := Rect( 0, 0, Width, Height );
    CopyRect( R, ToolsForm.CaptureImage1.Picture.Bitmap.Canvas, R );
    end;

  ToolsForm.CaptureImage1.ClientWidth  := ToolsForm.CaptureImage1.Picture.Width;
  ToolsForm.CaptureImage1.ClientHeight := ToolsForm.CaptureImage1.Picture.Height;
  UpdateScrollBoxRange;

  if Editor.HasSelection or ( Editor.Cursor >= ctLeftLine ) then with Editor do begin
     ShowCursor( Cursor, CursorRect );
     ScrollInView( CursorRect );
     end
  else if Editor.Step >= csBoardSquares then begin
          if Editor.Step < csSaveSkin then
             ScrollInView( ScaledBoardRect );
          end
       else begin
          ToolsForm.CaptureScrollBox.HorzScrollBar.Position := 0;
          ToolsForm.CaptureScrollBox.VertScrollBar.Position := 0;
          end;
  ShowBoard;
  if Editor.Cursor >= ctLeftLine then
     ShowCursor( Editor.Cursor, Editor.CursorRect );
end;

procedure TCaptureForm.ZoomItemClick(Sender: TObject);
begin
  if ToolsForm.CaptureImage1.Visible
     and
     ToolsForm.CaptureViewMenuItem100.Enabled
     and
     ToolsForm.CaptureViewMenuItem200.Enabled
//   and
//   ToolsForm.CaptureViewMenuItem400.Enabled
     and
     ( not ( ( Sender = ToolsForm.CaptureViewMenuItem100 ) and ( Editor.Zoom = 100 ) ) )
     and
     ( not ( ( Sender = ToolsForm.CaptureViewMenuItem200 ) and ( Editor.Zoom = 200 ) ) )
     and
     ( not ( ( Sender = ToolsForm.CaptureViewMenuItem400 ) and ( Editor.Zoom = 400 ) ) )
     then begin
     ToolsForm.CaptureScrollBox.Hide;
     try
       Screen.Cursor := crHourGlass;
       try
         if             Sender      = ToolsForm.CaptureViewMenuItem100       then ShowImage( 100 )
         else if        Sender      = ToolsForm.CaptureViewMenuItem200       then ShowImage( 200 )
         else if        Sender      = ToolsForm.CaptureViewMenuItem400       then ShowImage( 400 )
              else if   ( Editor.Zoom =  100 )                               then ShowImage( 200 )
              else if   ( Editor.Zoom =  200 ) and
                        ToolsForm.CaptureViewMenuItem400.Enabled and
                        ( GetAvailableVirtualMemoryByteSize >=
                          MIN_AVAILABLE_VIRTUAL_MEMORY_FOR_ZOOM_400 )        then ShowImage( 400 )
                   else                                                           ShowImage( 100 );
        except on E : Exception do ShowImage( 100 ); // something went wrong, e.g., there wasn't enough memory for zoom factor 400; try again, this time resetting the zoom factor to 100;
        end;
     finally
       Screen.Cursor := crDefault;
       ToolsForm.CaptureScrollBox.Show;
     end;
     end;
end;

procedure TCaptureForm.LoadImageFromUpscaledViewOfImage( Sender: TObject );
begin
  if Assigned( BackgroundBitMap ) and
     ( Editor.Zoom > 100 ) and
     ( ToolsForm.CaptureImage1.Picture.Graphic is TBitmap ) then
     LoadImage( FILE_NAME_EXT_DELIMITER ); // "FILE_NAME_EXT_DELIMITER": "LoadImage" recognizes is as a special value
end;

procedure TCaptureForm.CaptureScrollBoxMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Editor.MouseButtonDown and ( Editor.MouseButton = mbMiddle ) then begin
     NextStepButtonClick( Sender );
     Handled := True;
     end
  else
     if Editor.DrawingToolsEnabled then begin
        if (Editor.DrawingTool>dtWall) then begin
           EditMenuItemDrawingToolClick(ToolsForm.CaptureEditToolBarLeft.Buttons[Ord(Editor.DrawingTool)-2]);
           Handled := True;
           end;
        end
     else with ToolsForm.CaptureScrollBox.VertScrollBar do
        if Position > 0 then begin
           Position := Max( 0, Position - Max( 8, ( Range div 20 ) ) );
           Handled := True;
           end;
end;

procedure TCaptureForm.CaptureScrollBoxMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Editor.MouseButtonDown and ( Editor.MouseButton = mbMiddle ) then begin
     PreviousStepButtonClick( Sender );
     Handled := True;
     end
  else
     if Editor.DrawingToolsEnabled then begin
        if (Editor.DrawingTool<dtErase) then begin
           EditMenuItemDrawingToolClick(ToolsForm.CaptureEditToolBarLeft.Buttons[Ord(Editor.DrawingTool)]);
           Handled := True;
           end;
        end
     else with ToolsForm.CaptureScrollBox.VertScrollBar do
        if Position < Range then begin
           Position := Position + Max( 8, ( Range div 20 ) );
           Handled := True;
           end;
end;

procedure TCaptureForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  CaptureScrollBoxMouseWheelDown(Sender, Shift, MousePos, Handled);
end;

procedure TCaptureForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  CaptureScrollBoxMouseWheelUp(Sender, Shift, MousePos, Handled);
end;

procedure TCaptureForm.PreviousStepButtonClick(Sender: TObject);
var Result : Boolean; EditorCursor : TCaptureEditorCursorType; R : TRect;
begin
  if ToolsForm.PreviousStepButton.Enabled then with Editor do begin
     StatusText := '';
     if      Step = csBoardSquares then begin
             ToolsForm.CaptureImage1.Cursor := crDefault;
             HideCursor( True );
             HasSelection := True;
             Editor.Cursor := ctSelection;
             CursorRect := ScaledBoardRect;
             ToolsForm.CaptureEditMenuItemCopy.Hint := HintCopyBitmapToClipboardText;
             ToolsForm.CaptureToolButtonCopy.Hint := ToolsForm.CaptureEditMenuItemCopy.Hint;
             end;

     if      Cursor >= ctLeftLine then begin
             CursorRect := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove line artifacts from the screen
             HideCursor( True );
             Dec( Cursor );
             EditorCursor := Cursor; // take a snapshot of the cursor value; the global variable isn't stable during the following updates
             if EditorCursor = ctColumnLine then begin
                SetSpinEditValue( ToolsForm.RowHeightSpinEdit, 0 );
                SetSpinEditValue( ToolsForm.RowsSpinEdit, 1 );
                ShowGrid( ScaledBoardRect );
                SetSpinEditValue( ToolsForm.RowHeightSpinEdit, 0 );
                end;
             if EditorCursor = ctBottomLine then begin // go back to the 'Board' step
                Dec( Step );
                HasSelection := True;
                Cursor := ctSelection;
                ShowGrid( ScaledBoardRect );
                end
             else begin
                if EditorCursor < ctBottomLine then begin
                   SetSpinEditValue( ToolsForm.HeightSpinEdit, 0 );
                   SetSpinEditValue( ToolsForm.RowsSpinEdit, 1 );
                   end;
                if EditorCursor < ctRightLine  then begin
                   SetSpinEditValue( ToolsForm.WidthSpinEdit, 0 );
                   SetSpinEditValue( ToolsForm.ColumnsSpinEdit, 1 );
                   end;
                if EditorCursor < ctTopLine  then
                   SetSpinEditValue( ToolsForm.TopSpinEdit, 0 );
                if EditorCursor < ctLeftLine then
                   SetSpinEditValue( ToolsForm.LeftSpinEdit, 0 );

                if EditorCursor <  ctLeftLine   then
                   Step   := Pred( csBoard )
                else if   EditorCursor < ctColumnLine then
                          Step := csBoard
                     else Step := csColumnsRows;
                if Step <= csBoard then
                   Cursor := EditorCursor;
                ShowStatus;

                R := ScaledBoardRect;
                if Editor.Cursor = ctRightLine  then R.Left   := R.Right;
                if Editor.Cursor = ctBottomLine then R.Top    := R.Bottom;
                if R.Left        = R.Right      then R.Right  := ToolsForm.CaptureImage1.Picture.Bitmap.Width  - 2;
                if R.Top         = R.Bottom     then R.Bottom := ToolsForm.CaptureImage1.Picture.Bitmap.Height - 2;
                ShowCursor( Cursor, R );
                end;
             end
     else    if      Step >  Succ( csNone ) then
                     Dec( Step )
             else if CompletedStep = High( CompletedStep ) then
                     Step := High( CompletedStep );


     if      Step = csBoard then begin
             SetSkinHints;
             end
     else if Step = csColumnsRows then begin
             SetSkinHints;
             end
     else if Step = csBoardSquares then begin
             ToolsForm.CaptureStateMemo.Width := ToolsForm.BottomPanel.ClientWidth - ToolsForm.CaptureStateMemo.Left - ToolsForm.PreviousStepButton.Left;
             ToolsForm.CaptureEditMenuItemCopy.Hint := HintCopyBitmapToClipboardText;
             EditMenuItemDrawingToolClick( ToolsForm.CaptureEditToolBarLeft.Buttons[ Pred( Ord( Editor.DrawingTool ) ) ] );
             with ToolsForm.CaptureImage1.ScreenToClient( Mouse.CursorPos ) do
               CaptureImage1MouseMove(Sender,[],X,Y);
             ShowImage( Editor.Zoom );
             SetSkinHints;
             end
     else if Step = csSavePuzzle then begin
             ToolsForm.CaptureFileMenuItemSave.Caption := TEXT_SAVE_PUZZLE;
             ToolsForm.CaptureFileMenuItemSaveAs.Caption := TEXT_SAVE_PUZZLE_AS;
             ToolsForm.CaptureFileMenuItemSaveAs.Hint := ToolsForm.SavePuzzleAsBitBtn.Hint;
             ToolsForm.CaptureEditMenuItemCopy.Hint := HintCopyPuzzleToClipboardText;
             ToolsForm.CaptureToolButtonPlay.Hint := ToolsForm.PlayPuzzleBitBtn.Hint;
             ToolsForm.CaptureViewMenuItemShowImageAnalysisResults.Checked:=False;
             Editor.EditorSkinBitmap.Free; Editor.EditorSkinBitmap := nil; Editor.EditorSkinTileCount := 0;
             ToolsForm.CaptureScrollBox.Hide;
             try     ToolsForm.CaptureImage2.Hide;
                     ShowImage( Editor.Zoom );
             finally ToolsForm.CaptureScrollBox.Show;
             end;
             end
     else if Step =  csSaveSkin then begin
             Step := Succ( csNone ); Result := True;
             repeat try    NextStepButtonClick( Sender );
                    except on E:Exception do Result := Error( E.Message, '' );
                    end;
             until  ( Step = csSaveSkin ) or ( not Result );
             end;
     ToolsForm.CaptureToolButtonSave.Hint   := ToolsForm.CaptureFileMenuItemSave.Hint;
     ToolsForm.CaptureToolButtonSaveAs.Hint := ToolsForm.CaptureFileMenuItemSaveAs.Hint;
     ToolsForm.CaptureToolButtonCopy.Hint   := ToolsForm.CaptureEditMenuItemCopy.Hint;
     StatusText                   := '';
     ToolsForm.StatusBar1.Panels[ 0 ].Width := StatusBarPanel0Widths[ Editor.Step = csBoardSquares ];
     if HasSelection then ShowCursor( Cursor, CursorRect );
     ShowTitle;
     ShowStatus;
     if Screen.ActiveForm = ToolsForm then
        ToolsForm.CaptureStateMemo.SetFocus;
     end;
end;

procedure TCaptureForm.NextStepButtonClick(Sender: TObject);
var Result : Boolean; Point : TPoint;
begin
  if ToolsForm.NextStepButton.Enabled then begin
     StatusText := '';
     if   Editor.Step <  High( Editor.Step ) then
          Inc( Editor.Step )
     else Editor.Step := Succ( csNone ) ;
     ToolsForm.StatusBar1.Panels[ 0 ].Width := StatusBarPanel0Widths[ Editor.Step = csBoardSquares ];

     if      Editor.Step = csImage then begin
             Editor.Step := High( Editor.Step ); Result := True;
             repeat try PreviousStepButtonClick( Sender );
                    except on E:Exception do Result :=  Error( E.Message, '' );
                    end;
             until  ( Editor.Step = csImage ) or ( not Result );
             end
     else if Editor.Step = csBoard then begin
             if Editor.CompletedStep = csImage then
                ClearSelection;
             ScrollInView( Editor.CursorRect );
             ShowGrid( Editor.CursorRect );
             SetSkinHints;
             end
     else if Editor.Step = csColumnsRows then with Editor do begin
             Editor.SizeHandle := ghNull;
             Editor.Cursor := ctSelection;
             if   Editor.CompletedStep = Pred( csColumnsRows ) then begin
                  Editor.HasSelection := False;
                  Editor.Cursor := ctColumnLine;
                  SetSpinEditValue( ToolsForm.ColumnsSpinEdit, 1 );
                  SetSpinEditValue( ToolsForm.RowsSpinEdit, 1 );
                  SetSpinEditValue( ToolsForm.ColWidthSpinEdit, 0 );
                  SetSpinEditValue( ToolsForm.RowHeightSpinEdit, 0 );
                  end
             else ShowGrid( Editor.CursorRect );
             SetSkinHints;
             end
     else if Editor.Step = csBoardSquares then begin
             Timestamp := High( Timestamp ); // initializes the calculation of the pusher's reachable squares in 'CheckBoard'
             Editor.CursorRect := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width, ToolsForm.CaptureImage1.Picture.Bitmap.Height ); // remove any grid line artifacts from the screen
             HideCursor( True );
             EditMenuItemDrawingToolClick( ToolsForm.CaptureEditToolBarLeft.Buttons[ Pred( Ord( Editor.DrawingTool ) ) ] );
             Point:=ToolsForm.CaptureImage1.ScreenToClient(Mouse.CursorPos);
             CaptureImage1MouseMove(Sender,[],Point.X,Point.Y);
             Editor.Board.Width := ToolsForm.ColumnsSpinEdit.Value;
             Editor.Board.Height := ToolsForm.RowsSpinEdit.Value;
             if   Colors.Count = 0 then
                  AnalyseBoardColors;
             if   ShowBoard then begin
                  end
             else PreviousStepButtonClick( Sender );
             SetSkinHints;
             end
     else if Editor.Step = csSavePuzzle then begin
             ToolsForm.CaptureImage1.Cursor := crDefault;
             ToolsForm.CaptureStateMemo.Width := ToolsForm.PuzzleInformationPanel.Left - ToolsForm.CaptureStateMemo.Left;
             ToolsForm.CaptureEditMenuItemCopy.Hint := HintCopyPuzzleToClipboardText;
             if Editor.CompletedStep <  csSavePuzzle then
                Editor.CompletedStep := csSavePuzzle;
             ShowImage( Editor.Zoom );
             // by advancing to the "save puzzle" step, the user acknowledges
             // that the board is a correct representation of the board depicted
             // by the image;
             // maybe the user doesn't advance to the next step, "save skin",
             // but if the board was identified by using a matching skin, this
             // skin may require updating by new square types (e.g., "box-on-goal")
             // which appear in the current image, but not in the image the
             // matching skin originally was extracted from;
             // this update is a part of the "create skin" function, hence,
             // create the skin now instead of waiting to the next step;
             if   ( Assigned( SkinBitmap )
                    and
                    Assigned( SingleRowSkinBitmap )
                    and
                    ( Editor.CompletedStep >= csSaveSkin ) )
                  or
                  CreateSkinFromImage then
                  Editor.CompletedStep := csSaveSkin;
             end
     else if Editor.Step = csSaveSkin then begin
             ToolsForm.CaptureFileMenuItemSave.Caption := TEXT_SAVE_SKIN;
             ToolsForm.CaptureFileMenuItemSaveAs.Caption := TEXT_SAVE_SKIN_AS;
             ToolsForm.CaptureFileMenuItemSaveAs.Hint := ToolsForm.CaptureFileMenuItemSaveAs.Caption;
             ToolsForm.CaptureEditMenuItemCopy.Hint := HintCopySkinToClipboardText;
             ToolsForm.CaptureToolButtonPlay.Hint := ToolsForm.PlaySkinBitBtn.Hint;
             ToolsForm.CaptureViewMenuItemShowImageAnalysisResults.Checked:=False;
             if   ( Assigned( SkinBitmap )
                    and
                    Assigned( SingleRowSkinBitmap )
                    and
                    ( Editor.CompletedStep >= csSaveSkin ) )
                  or
                  CreateSkinFromImage then begin
                  Editor.CompletedStep := csSaveSkin;
                  Editor.EditorSkinBitmap.Free; Editor.EditorSkinBitmap := nil; Editor.EditorSkinTileCount := 0;
                  if ToolsForm.CaptureSettingsMenuItemObjectsOnABlackBackground.Checked then
                     Editor.Zoom := 100;
                  ToolsForm.CaptureScrollBox.Hide;
                  try     ShowImage( Editor.Zoom );
                  finally ToolsForm.CaptureScrollBox.Show;
                  end;
                  end
             else PreviousStepButtonClick( Sender );
             end;
     ToolsForm.CaptureToolButtonSave.Hint   := ToolsForm.CaptureFileMenuItemSave.Hint;
     ToolsForm.CaptureToolButtonSaveAs.Hint := ToolsForm.CaptureFileMenuItemSaveAs.Hint;
     ToolsForm.CaptureToolButtonCopy.Hint   := ToolsForm.CaptureEditMenuItemCopy.Hint;
     StatusText                             := '';
     ShowTitle;
     ShowStatus;
     if Screen.ActiveForm = ToolsForm then
        ToolsForm.CaptureStateMemo.SetFocus;
     end;
end;

procedure TCaptureForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //IgnoreKeyUp:=False; IgnoreMouseUp:=False;
  Editor.MouseButton:=Button;
  Editor.MouseButtonDown:=Button=mbMiddle;
  ShowStatus;
end;

procedure TCaptureForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
    if Screen.Cursor  <> crDefault then begin
       Screen.Cursor  := crDefault;
       ToolsForm.CaptureImage1.Cursor  := crDefault;
       if ( Editor.Cursor >= ctLeftLine ) or (Editor.Step >= csBoardSquares ) then
          HideCursor( True );
       end;
end;

procedure TCaptureForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
  if ( Editor.Cursor >= ctLeftLine ) and Editor.MouseButtonDown then with Editor do begin // in order to let the user select the entire image, it's necessary to treat a mouse-click outside the image as a command to set a value for the currently selected side of the rectangle
     Editor.MouseButtonDown := True;
     case Editor.Cursor of
       ctLeftLine   : if ToolsForm.LeftSpinEdit  .Value <= 10 then CaptureImage1MouseUp( Sender, Button, Shift, 0, 0 );
       ctTopLine    : if ToolsForm.TopSpinEdit   .Value <= 10 then CaptureImage1MouseUp( Sender, Button, Shift, 0, 0 );
       ctRightLine  : if ToolsForm.WidthSpinEdit .Value >= OriginalBitmap.Width  - 2 - 10 then CaptureImage1MouseUp( Sender, Button, Shift, OriginalBitmap.Width  - 2, 0 );
       ctBottomLine : if ToolsForm.HeightSpinEdit.Value >= OriginalBitmap.Height - 2 - 10 then CaptureImage1MouseUp( Sender, Button, Shift, OriginalBitmap.Height - 2, 0 );
     end;
     end;
  if Editor.MouseButtonDown then begin
     Editor.MouseButtonDown:=False;
     ShowStatus;
     end;
end;

procedure TCaptureForm.CaptureEditSelectAllItemClick(Sender: TObject);
begin
  if ( not ( Sender is TMenuItem ) ) and
     ( ToolsForm.ActiveControl is TEdit ) then with ToolsForm.ActiveControl as TEdit do
     SelectAll
  else
     if ToolsForm.CaptureEditMenuItemSelectAll.Enabled then with Editor do begin
        if ( RectWidth ( ScaledBoardRect ) = ToolsForm.CaptureImage1.Picture.Bitmap.Width  - 2 ) and
           ( RectHeight( ScaledBoardRect ) = ToolsForm.CaptureImage1.Picture.Bitmap.Height - 2 ) then begin
           ClearSelection;
           if ( OriginalBitmap.Width  > MIN_BOARD_SIZE_PIXELS ) and
              ( OriginalBitmap.Height > MIN_BOARD_SIZE_PIXELS ) then begin
              Editor.Cursor := ctLeftLine;
              ShowStatus;
              with ToolsForm.CaptureImage1.ScreenToClient( Mouse.CursorPos ) do
                CaptureImage1MouseMove(Sender,[],X,Y);
              end;
           end
        else begin
           HideCursor( True );
           HasSelection := True;
           Cursor := ctSelection;
           CursorRect := Rect( 0, 0, ToolsForm.CaptureImage1.Picture.Bitmap.Width - 2, ToolsForm.CaptureImage1.Picture.Bitmap.Height - 2 );
           UserWidth := NormalizedSize( RectWidth( CursorRect ) );
           UserHeight := NormalizedSize( RectHeight( CursorRect ) );
           ShowGrid( CursorRect );
           if Editor.CompletedStep <  Pred( csColumnsRows ) then
              Editor.CompletedStep := Pred( csColumnsRows );
           MouseButtonDown := True;
           CaptureImage1MouseUp( Self, mbLeft, [], 0, 0 );
           end;
    end;
end;

procedure TCaptureForm.CaptureSettingsSnapToNearbyEdgesItemClick(Sender: TObject);
begin
  with ToolsForm.CaptureSettingsMenuItemSnapToNearbyEdges do Checked := not Checked;
end;

procedure TCaptureForm.CaptureSettingsUseAutomaticCompletionItemClick(Sender: TObject);
begin
  with ToolsForm.CaptureSettingsMenuItemUseAutomaticCompletion do Checked := not Checked;
end;

procedure TCaptureForm.CaptureSettingsKeepAuthorAndDesignerNamesItemClick(Sender: TObject);
begin
  with ToolsForm.CaptureSettingsMenuItemKeepAuthorAndDesignerNames do Checked := not Checked;
end;

procedure TCaptureForm.ShowGrid( const Rect__ : TRect );
begin
      if   ( ( Editor.Step >= Pred( csColumnsRows ) ) or ( Editor.CompletedStep >= Pred( csColumnsRows ) ) ) then begin
           SetSpinEditValue( ToolsForm.LeftSpinEdit  , NormalizedPosition( Rect__.Left ) );
           SetSpinEditValue( ToolsForm.TopSpinEdit   , NormalizedPosition( Rect__.Top ) );
           SetSpinEditValue( ToolsForm.WidthSpinEdit , NormalizedSize( RectWidth ( Rect__ ) ) );
           SetSpinEditValue( ToolsForm.HeightSpinEdit, NormalizedSize( RectHeight( Rect__ ) ) );
           end;
      if   ( ( Editor.Step >= Pred( csBoardSquares ) ) or ( Editor.CompletedStep >= Pred( csBoardSquares ) ) ) then
           SetSpinEditValue( ToolsForm.ColWidthSpinEdit, ToolsForm.WidthSpinEdit.Value  div Max( 1, ToolsForm.ColumnsSpinEdit.Value ) );
      if   ( ( Editor.Step >= Pred( csBoardSquares ) ) or ( Editor.CompletedStep >= Pred( csBoardSquares ) ) ) then
           SetSpinEditValue( ToolsForm.RowHeightSpinEdit, Max( 1, ToolsForm.HeightSpinEdit.Value div Max( 1, ToolsForm.RowsSpinEdit.Value ) ) );

      if   ( ( Editor.Step >= csColumnsRows ) or ( Editor.CompletedStep >= csColumnsRows ) ) and
           ( ( ToolsForm.ColWidthSpinEdit.Value < MIN_BOARD_SQUARE_SIZE_PIXELS ) or ( ToolsForm.RowHeightSpinEdit.Value < MIN_BOARD_SQUARE_SIZE_PIXELS ) ) and
           ( ( Editor.Cursor < ctLeftLine ) or ( Editor.Cursor > ctRowLine ) ) then
           ClearSelection
      else ShowStatus;

      ShowCursor( Editor.Cursor, ScaledBoardRect ) ;
end;

function TCaptureForm.ShowTitle : String;
var SubSection : String;
begin
  Result := '';
  if      ( Editor.Step = csImage      ) then
  else if ( Editor.Step < csSavePuzzle ) and ( Editor.InputFileName  <> '' ) then
          Result := Editor.InputFileName
  else if ( Editor.Step = csSavePuzzle ) then begin
          SubSection :=ToolsForm.PuzzleLabel.Caption;
          Result := Editor.PuzzleFileName;
          if Result = '' then
             Result := TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER;
          end
  else if ( Editor.Step = csSaveSkin   ) then begin
          SubSection :=ToolsForm.SkinLabel.Caption;
          Result := Editor.SkinFileName;
          if Result = '' then
             Result := TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER;
          end;
  ToolsForm.ShowTitle( SubSection, Result );
  Self.Caption := ToolsForm.Caption;
end;

procedure TCaptureForm.WallCapCheckBoxClick(Sender: TObject);
begin
  if Editor.CompletedStep >= csSaveSkin then begin
     ToolsForm.WallCapCheckBox.Tag  := 1; // '1': 'checked' is a user value or a value from a matching skin
     WriteSkinSettings;
     ShowImage( Editor.Zoom );
     end;
end;

function TCaptureForm.LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
begin
  Result:=True;
  with IniFile do begin
    ToolsForm.CaptureViewMenuItemShowImageAnalysisResults.Checked :=IniFile.ReadBool(CAPTURE_INIFILE_SECTION,'ShowImageAnalysisResults',ToolsForm.CaptureViewMenuItemShowImageAnalysisResults.Checked);
    ToolsForm.CaptureSettingsMenuItemSnapToNearbyEdges.Checked := IniFile.ReadBool(CAPTURE_INIFILE_SECTION,'SnapToNearbyEdges',ToolsForm.CaptureSettingsMenuItemSnapToNearbyEdges.Checked);
    ToolsForm.CaptureSettingsMenuItemKeepAuthorAndDesignerNames.Checked := IniFile.ReadBool(CAPTURE_INIFILE_SECTION,'KeepAuthorAndDesignerNames',ToolsForm.CaptureSettingsMenuItemKeepAuthorAndDesignerNames.Checked);
    ToolsForm.CaptureSettingsColorQuantizationIgnoreSmallColorDifferences.Checked := IniFile.ReadBool(CAPTURE_INIFILE_SECTION,'IgnoreSmallColorDifferences',ToolsForm.CaptureSettingsColorQuantizationIgnoreSmallColorDifferences.Checked);
    Editor.GridColor:=TColor(IniFile.ReadInteger(CAPTURE_INIFILE_SECTION,'GridColor',Editor.GridColor));
    Editor.GridShadowColor:=TColor(IniFile.ReadInteger(CAPTURE_INIFILE_SECTION,'GridShadowColor',Editor.GridShadowColor));
    EditorSkinFileName:=KeepDataPathUpToDate(IniFile.ReadString(CAPTURE_INIFILE_SECTION,'EditorSkinFileName',EditorSkinFileName));
    if StrEqual( EditorSkinFileName, DEFAULT_VALUE ) then EditorSkinFileName := '';
    OpenPictureDialog1.InitialDir:=IniFile.ReadString(CAPTURE_INIFILE_SECTION,'ImageFolder',OpenPictureDialog1.InitialDir);
    PuzzleInitialDirectory:=KeepDataPathUpToDate(IniFile.ReadString(CAPTURE_INIFILE_SECTION,'LevelFolder',PuzzleInitialDirectory));
    SkinInitialDirectory:=KeepDataPathUpToDate(IniFile.ReadString(CAPTURE_INIFILE_SECTION,'SkinFolder',SkinInitialDirectory));
    MatchingSkinFileName:=KeepDataPathUpToDate(IniFile.ReadString(CAPTURE_INIFILE_SECTION,'MatchingSkinFileName',MatchingSkinFileName));
    DefaultSkinFileNameText:=IniFile.ReadString(CAPTURE_INIFILE_SECTION,'DefaultSkinFileNameText',DefaultSkinFileNameText);
    Settings.CheckForDuplicatesWhenAppendingNewLevelsToCollections:=IniFile.ReadBool(CAPTURE_INIFILE_SECTION,'CheckForDuplicatesWhenAppendingNewLevelsToCollections',Settings.CheckForDuplicatesWhenAppendingNewLevelsToCollections);
    LoadComboBoxFromIniFile(IniFile,CAPTURE_INIFILE_SECTION+SUB_TITLE_SEPARATOR+SETTINGS_INIFILE_SECTION_SKINS,SKINS_CAPACITY,False,True,False,False,SkinsComboBox);
    end;
end;

function TCaptureForm.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
begin
  Result:=True;
  with IniFile do begin
    IniFile.WriteBool(CAPTURE_INIFILE_SECTION,'ShowImageAnalysisResults',ToolsForm.CaptureViewMenuItemShowImageAnalysisResults.Checked);
    IniFile.WriteBool(CAPTURE_INIFILE_SECTION,'SnapToNearbyEdges',ToolsForm.CaptureSettingsMenuItemSnapToNearbyEdges.Checked);
    IniFile.WriteBool(CAPTURE_INIFILE_SECTION,'KeepAuthorAndDesignerNames',ToolsForm.CaptureSettingsMenuItemKeepAuthorAndDesignerNames.Checked);
    IniFile.WriteBool(CAPTURE_INIFILE_SECTION,'IgnoreSmallColorDifferences',ToolsForm.CaptureSettingsColorQuantizationIgnoreSmallColorDifferences.Checked);
    IniFile.WriteInteger(CAPTURE_INIFILE_SECTION,'GridColor',Editor.GridColor);
    IniFile.WriteInteger(CAPTURE_INIFILE_SECTION,'GridShadowColor',Editor.GridShadowColor);
    IniFile.WriteString(CAPTURE_INIFILE_SECTION,'EditorSkinFileName',EditorSkinFileName);
    if ( OpenPictureDialog1.InitialDir <> '' ) and ( not DirectoryExists( OpenPictureDialog1.InitialDir ) ) then OpenPictureDialog1.InitialDir := '';
    IniFile.WriteString(CAPTURE_INIFILE_SECTION,'ImageFolder',OpenPictureDialog1.InitialDir);
    if ( PuzzleInitialDirectory <> '' ) and ( not DirectoryExists( PuzzleInitialDirectory ) ) then PuzzleInitialDirectory := '';
    IniFile.WriteString(CAPTURE_INIFILE_SECTION,'LevelFolder',PuzzleInitialDirectory);
    if ( SkinInitialDirectory <> '' ) and ( not DirectoryExists( SkinInitialDirectory ) ) then SkinInitialDirectory := '';
    IniFile.WriteString(CAPTURE_INIFILE_SECTION,'SkinFolder',SkinInitialDirectory);
    if ( MatchingSkinFileName <> '' ) and ( not FileExists( MatchingSkinFileName ) ) then MatchingSkinFileName := '';
    IniFile.WriteString(CAPTURE_INIFILE_SECTION,'MatchingSkinFileName',MatchingSkinFileName);
    IniFile.WriteString(CAPTURE_INIFILE_SECTION,'DefaultSkinFileNameText',DefaultSkinFileNameText);
    IniFile.WriteBool(CAPTURE_INIFILE_SECTION,'CheckForDuplicatesWhenAppendingNewLevelsToCollections',Settings.CheckForDuplicatesWhenAppendingNewLevelsToCollections);
    SaveComboBoxToIniFile(IniFile,CAPTURE_INIFILE_SECTION+SUB_TITLE_SEPARATOR+SETTINGS_INIFILE_SECTION_SKINS,SKINS_CAPACITY,SkinsComboBox);
    end;
end;

procedure TCaptureForm.WriteSkinSettings;
const
  SOKOBAN_YASC_DEFAULT_SKIN_SETTINGS =
    '[Graphics - Board - Figures - Player on goal square]' + NL +
    'Use "Player" image for move animation=Yes' + NL +
    '' + NL +
    '[Graphics - Board - Figures - Box on goal square]' + NL +
    'Use "Box" image for move animation=Yes' + NL +
    '' + NL +
    '[Graphics - Board - Figures]' + NL +
    'Maximum zoom factor (%)=200';
  SOKOBAN_YASC_NO_WALL_CAP_SETTINGS =
    '[' + INIFILE_SECTION_WALL +']' + NL +
    KEY_WALL_TYPE + '=' + WallTypeSeamlessWallsNoWallCapText;
  SOKOBAN_YASC_OUTER_WALL_TRIMMING =
    '[' + INIFILE_SECTION_WALL_TRIMMING +']' + NL +
    'Left=%d' + NL +
    'Top=%d' + NL +
    'Right=%d' + NL +
    'Bottom=%d';
  SOKOBAN_YASC_OBJECTS_WITH_UNIFORM_BACKGROUND_COLOR_1 =
    '[Graphics - Board - Figures - Player - Image]' + NL +
    'Mask image using background color=Yes' + NL +
    '[Graphics - Board - Figures - Player on goal square - Image]' + NL +
    'Mask image using background color=Yes';
  SOKOBAN_YASC_OBJECTS_WITH_UNIFORM_BACKGROUND_COLOR_2 =
    '[Graphics - Board - Figures - Box - Image]' + NL +
    'Mask image using background color=Yes' + NL +
    '[Graphics - Board - Figures - Box on goal square - Image]' + NL +
    'Mask image using background color=Yes';
var
  Index, MaxTextLength : Integer;
  SettingsRect :TRect;
  Header, Text, WallCutText, ErrorMessage : String;
begin
  if Assigned( SkinBitmap ) then with SkinBitmap do with Canvas do begin
     SettingsRect  := Rect( ( 3 * Width  ) div 4, ( 3 * Height ) div 4, Width, Height );
     MaxTextLength := ( PIXEL_BYTE_SIZE[ SkinBitmap.PixelFormat ] * ( RectWidth( SettingsRect ) - 2 ) * ( RectHeight( SettingsRect ) - 3 ) ) - PIXEL_BYTE_SIZE[ SkinBitmap.PixelFormat ] - 1;
     Brush.Color   := clDkGray;
     FillRect( SettingsRect );

     ToolsForm.SkinTitleEdit.Text := Trim( ToolsForm.SkinTitleEdit.Text );
     ToolsForm.SkinDesignerEdit.Text := Trim( ToolsForm.SkinDesignerEdit.Text );
     if   ToolsForm.SkinTitleEdit.Text <> '' then
          Header := ToolsForm.TitleLabel.Caption + SPACE + ToolsForm.SkinTitleEdit.Text
     else Header := '';
     if   ToolsForm.SkinDesignerEdit.Text <> '' then begin
          if Header <> '' then Header := Header + NL;
          Header := Header + ToolsForm.DesignerLabel.Caption + SPACE + ToolsForm.SkinDesignerEdit.Text;
          end;

     Text := '';
     if not ToolsForm.WallCapCheckBox.Checked then
        Text := SOKOBAN_YASC_NO_WALL_CAP_SETTINGS;
     if ToolsForm.CaptureSettingsMenuItemObjectsOnABlackBackground.Checked then begin
        if Text <> '' then Text := Text + NL + NL;
        Text := Text +SOKOBAN_YASC_OBJECTS_WITH_UNIFORM_BACKGROUND_COLOR_1 + NL + SOKOBAN_YASC_OBJECTS_WITH_UNIFORM_BACKGROUND_COLOR_2;
        end;
     if ToolsForm.OuterWallCutLeftSpinEdit.Value +
        ToolsForm.OuterWallCutTopSpinEdit.Value +
        ToolsForm.OuterWallCutRightSpinEdit.Value +
        ToolsForm.OuterWallCutBottomSpinEdit.Value <> 0 then begin
        WallCutText := Format( SOKOBAN_YASC_OUTER_WALL_TRIMMING,
                               [ ToolsForm.OuterWallCutLeftSpinEdit.Value,
                                 ToolsForm.OuterWallCutTopSpinEdit.Value,
                                 ToolsForm.OuterWallCutRightSpinEdit.Value,
                                 ToolsForm.OuterWallCutBottomSpinEdit.Value ] );
        if MaxTextLength >= ( Length( Text ) + ( 2 * Length( NL ) ) + Length( WallCutText ) ) * SizeOf( Char ) then begin
           if Text <> '' then Text := Text + NL + NL;
           Text := Text + WallCutText;
           end;
        end;

     if MaxTextLength >= ( Length( Text ) + ( 2 * Length( NL ) ) + Length( SOKOBAN_YASC_DEFAULT_SKIN_SETTINGS ) ) * SizeOf( Char ) then begin
        if Text <> '' then Text := NL + NL + Text;
        Text := SOKOBAN_YASC_DEFAULT_SKIN_SETTINGS + Text;
        end;

     if ( Header <> '' ) and
        ( MaxTextLength >= ( Length( Header ) + ( 2 * Length( NL ) ) + Length( Text ) ) * SizeOf( Char ) ) then begin
        if Text <> '' then Text := NL + NL + Text;
        Text := Header + Text;
        end;

     if not SetBitmapText( SkinBitmap, SettingsRect, Text, ErrorMessage ) then begin // do nothing
        end;

     for Index := 0 to Pred( ToolsForm.OuterWallCutLeftSpinEdit.Value )  do with SettingsRect do
         Pixels[ Succ( Left ) + Index, Bottom - 2 ] := clLtGray;
     for Index := 0 to Pred( ToolsForm.OuterWallCutRightSpinEdit.Value ) do with SettingsRect do
         Pixels[ Right - 2 - Index, Bottom - 2 ] := clBlack;
     for Index := 0 to Pred( ToolsForm.OuterWallCutTopSpinEdit.Value )  do with SettingsRect do
         Pixels[ Succ( Left ) + Index, Bottom - 1 ] := clBlack;
     for Index := 0 to Pred( ToolsForm.OuterWallCutBottomSpinEdit.Value ) do with SettingsRect do
         Pixels[ Right - 2 - Index, Bottom - 1 ] := clLtGray;

     if Assigned( SingleRowSkinBitmap ) then with SingleRowSkinBitmap do with Canvas do // update the single row skin version too
        CopyRect( Rect( Width - RectWidth( SettingsRect ), 0, Width, Height ), SkinBitmap.Canvas, SettingsRect );
     end;
end;

function  TCaptureForm.CreateSingleRowSkinFromSkin( SkinBitmap__ : TBitmap; ColWidth__, RowHeight__ : Integer; var SingleRowSkinBitmap__ : TBitmap ) : Boolean;
var Index :Integer;

    function  MakeWallTilesFrom3x2WallBlock(ColWidth__,RowHeight__:Integer;
                                            SourceBitmap__  :TBitmap;
                                            DestBitmap__    :TBitmap ) : Boolean;

      function BottomRect(Col,Row:Integer):TRect;
      begin
        Result:=CellToRect(Col,Row,ColWidth__,RowHeight__);
        Inc(Result.Top,(Result.Bottom-Result.Top) div 2);
      end;

      function LeftRect(Col,Row:Integer):TRect;
      begin
        Result:=CellToRect(Col,Row,ColWidth__,RowHeight__);
        Dec(Result.Right,(Result.Right-Result.Left) div 2);
      end;

      function RightRect(Col,Row:Integer):TRect;
      begin
        Result:=CellToRect(Col,Row,ColWidth__,RowHeight__);
        Inc(Result.Left,(Result.Right-Result.Left) div 2);
      end;

      function TopRect(Col,Row:Integer):TRect;
      begin
        Result:=CellToRect(Col,Row,ColWidth__,RowHeight__);
        Dec(Result.Bottom,(Result.Bottom-Result.Top) div 2);
      end;

    begin // 'MakeWallTilesFrom3x2WallBlock'
      with DestBitmap__.Canvas do begin
        //Brush.Color:=RGBToColor(MaskBitmapColor); Brush.Style:=FillRect(Rect(0,0,Bitmap.Width,Bitmap.Height));
        CopyMode:=cmSrcCopy;

        // 00: no wall neighbours, i.e., a single wall square
        CopyRect(CellToRect( 0,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(1,3,ColWidth__,RowHeight__));

        // 01: wall above
        CopyRect(   TopRect( 1,0),SourceBitmap__.Canvas,   TopRect( 0,3));
        CopyRect(BottomRect( 1,0),SourceBitmap__.Canvas,BottomRect( 1,3));

        // 02: wall to the right
        CopyRect(  LeftRect( 2,0),SourceBitmap__.Canvas, LeftRect(  1,3));
        CopyRect( RightRect( 2,0),SourceBitmap__.Canvas, RightRect( 1,2));

        // 03: wall above and to the right
        CopyRect(CellToRect( 3,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(0,2,ColWidth__,RowHeight__));
        CopyRect(  LeftRect( 3,0),  DestBitmap__.Canvas,  LeftRect( 1,0));
        CopyRect(BottomRect( 3,0),  DestBitmap__.Canvas,BottomRect( 2,0));

        // 04: wall below
        CopyRect(   TopRect( 4,0),SourceBitmap__.Canvas,   TopRect( 1,3));
        CopyRect(BottomRect( 4,0),SourceBitmap__.Canvas,BottomRect( 0,3));

        // 05: wall above and below
        CopyRect(CellToRect( 5,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(0,3,ColWidth__,RowHeight__));

        // 06: wall below and to the right
        CopyRect(CellToRect( 6,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(0,2,ColWidth__,RowHeight__));
        CopyRect(   TopRect( 6,0),  DestBitmap__.Canvas,   TopRect( 2,0));
        CopyRect(  LeftRect( 6,0),  DestBitmap__.Canvas,  LeftRect( 4,0));

        // 07: wall above, below, and to the right
        CopyRect(  LeftRect( 7,0),SourceBitmap__.Canvas,  LeftRect( 0,3));
        CopyRect( RightRect( 7,0),SourceBitmap__.Canvas, RightRect( 0,2));

        // 08: wall to the left
        CopyRect(  LeftRect( 8,0),SourceBitmap__.Canvas,  LeftRect( 1,2));
        CopyRect( RightRect( 8,0),SourceBitmap__.Canvas, RightRect( 1,3));

        // 09: wall above and to the left
        CopyRect(CellToRect( 9,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(0,2,ColWidth__,RowHeight__));
        CopyRect( RightRect( 9,0),  DestBitmap__.Canvas, RightRect( 1,0));
        CopyRect(BottomRect( 9,0),  DestBitmap__.Canvas,BottomRect( 8,0));

        // 10: wall to the left and to the right
        CopyRect(CellToRect(10,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(1,2,ColWidth__,RowHeight__));

        // 11: wall above, to the left, and to the right
        CopyRect(CellToRect(11,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(0,2,ColWidth__,RowHeight__));
        CopyRect(BottomRect(11,0),SourceBitmap__.Canvas,BottomRect( 1,2));

        // 12: wall below and to the left
        CopyRect(CellToRect(12,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(0,2,ColWidth__,RowHeight__));
        CopyRect(   TopRect(12,0),  DestBitmap__.Canvas,   TopRect( 8,0));
        CopyRect( RightRect(12,0),  DestBitmap__.Canvas, RightRect( 4,0));

        // 13: wall above, below, and to the left
        CopyRect(CellToRect(13,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(0,2,ColWidth__,RowHeight__));
        CopyRect( RightRect(13,0),SourceBitmap__.Canvas, RightRect( 0,3));

        // 14: wall below, to the left, and to the right
        CopyRect(CellToRect(14,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(0,2,ColWidth__,RowHeight__));
        CopyRect(   TopRect(14,0),SourceBitmap__.Canvas,   TopRect( 1,2));

        // 15: walls on all 4 sides
        CopyRect(CellToRect(15,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(0,2,ColWidth__,RowHeight__));

        // 16: wall cap

        {$IFDEF WALL_CAP_QUADRANTS_ABCD}
          CopyRect(CellToRect(16,0,ColWidth__,RowHeight__),SourceBitmap__.Canvas,CellToRect(2,2,ColWidth__,RowHeight__));
        {$ELSE}
          // the tile consists of 4 quadrants:
          // AB   or  10
          // CD       23
          // in order to cover the gap, the tile is shown halfway between the
          // cells hence, the quadrants are re-arranged like this:
          // DC   or  32
          // BA       01

          CopyRect( QuadrantToRect( 0, ColWidth__, RowHeight__, CellToRect( 16, 0, ColWidth__, RowHeight__ ) ), SourceBitmap__.Canvas, QuadrantToRect( 2, ColWidth__, RowHeight__, CellToRect( 2, 2, ColWidth__, RowHeight__ ) ) );
          CopyRect( QuadrantToRect( 1, ColWidth__, RowHeight__, CellToRect( 16, 0, ColWidth__, RowHeight__ ) ), SourceBitmap__.Canvas, QuadrantToRect( 3, ColWidth__, RowHeight__, CellToRect( 2, 2, ColWidth__, RowHeight__ ) ) );
          CopyRect( QuadrantToRect( 2, ColWidth__, RowHeight__, CellToRect( 16, 0, ColWidth__, RowHeight__ ) ), SourceBitmap__.Canvas, QuadrantToRect( 0, ColWidth__, RowHeight__, CellToRect( 2, 2, ColWidth__, RowHeight__ ) ) );
          CopyRect( QuadrantToRect( 3, ColWidth__, RowHeight__, CellToRect( 16, 0, ColWidth__, RowHeight__ ) ), SourceBitmap__.Canvas, QuadrantToRect( 1, ColWidth__, RowHeight__, CellToRect( 2, 2, ColWidth__, RowHeight__ ) ) );
        {$ENDIF}
        end;
      Result:=True;
    end; // MakeWallTilesFrom3x2WallBlock

begin // 'CreateSingleRowSkinFromSkin'
  Result := False; SingleRowSkinBitmap__ := nil;
  if Assigned( SkinBitmap__ ) and
     ( SkinBitmap__.Width  >= 4 * ColWidth__  ) and
     ( SkinBitmap__.Height >= 4 * RowHeight__ ) and
     ( ColWidth__          >  0 ) and ( RowHeight__ > 0 ) and
     BitmapCreate( SingleRowSkinBitmap__, SINGLE_ROW_SKIN_TILE_COUNT * ColWidth__, RowHeight__ )
     then with SingleRowSkinBitmap__.Canvas do
     try    Result := MakeWallTilesFrom3x2WallBlock( ColWidth__, RowHeight__,   SkinBitmap__, SingleRowSkinBitmap__ );
            for Index := 0 to 5 do
                CopyRect( CellToRect( Index + 17, 0, ColWidth__, RowHeight__ ), SkinBitmap__.Canvas, CellToRect( Index mod 3, Index div 3, ColWidth__, RowHeight__ ) );
            for Index := 0 to 3 do // directional pusher-on-floor: use default pusher
                CopyRect( CellToRect( Index + 23, 0, ColWidth__, RowHeight__ ), SkinBitmap__.Canvas, CellToRect( 1, 0                    , ColWidth__, RowHeight__ ) );
            for Index := 0 to 3 do // directional pusher-on-goal:  use default pusher
                CopyRect( CellToRect( Index + 27, 0, ColWidth__, RowHeight__ ), SkinBitmap__.Canvas, CellToRect( 1, 1                    , ColWidth__, RowHeight__ ) );
            //SingleRowSkinBitmap__.SaveToFile( 't10.bmp' );
     except on E:Exception do begin
            SingleRowSkinBitmap__.Free; SingleRowSkinBitmap__ := nil;
            Result := Error( E.Message, '' );
            end;
     end;
end; // CreateSingleRowSkinFromSkin

function  TCaptureForm.CreateSkinFromImage : Boolean;
var
  ColWidth, RowHeight, OriginalZoom : Integer;
  MatchingSkinImagesChanged : Boolean;

  function  CreateSkin : Boolean;
  var Col, Row, Quadrant, Neighbors : Integer;
      HasWallCap : Boolean;
      Items : TCaptureBoardSquareItemSet;
      R, R1, R2 : TRect;
      Tiles : array[ 0..3, 0..3, 0..3 ] of TRect; // columns, rows, quadrants

    function  ColRowQuadrantToRect( Col__, Row__, Quadrant__ : Integer ) : TRect;
    begin
      Result := CellToRect( Col__, Row__, ColWidth, RowHeight );
      if ( Quadrant__ < 0 )
         or
         ( ( RectWidth ( Tiles[ Col__, Row__, Quadrant__ ] ) = ColWidth  )
           and
           ( RectHeight( Tiles[ Col__, Row__, Quadrant__ ] ) = RowHeight )
         ) then begin
         // the "quadrant" doesn't really contain a quadrant; it covers the entire square;
         end
      else Result := QuadrantToRect( Quadrant__, ColWidth, RowHeight, Result );
    end;

    function  HasWallNeighbors( Neighbors__, Directions__ : Integer ) : Boolean;
    begin
      Result := ( ( Neighbors__ and Directions__ ) = Directions__ )
                and
                ( ( Directions__ <> WALL_NONE )
                  or
                  ( Neighbors__  =  WALL_NONE ) );
    end;

    function  IsBoardSquare( Col__, Row__ : Integer ) : Boolean;
    begin
      Result := ( Col__ > 0 ) and ( Col__ <= Editor.Board.Width  ) and
                ( Row__ > 0 ) and ( Row__ <= Editor.Board.Height ) and
                ( ( [ bsiFloor, bsiWall ] * Editor.Board.Squares[ Col__, Row__ ] ) <> [] );
    end;

    function  IsFloor( Col__, Row__ : Integer ) : Boolean;
    begin
      Result := ( Col__ > 0 ) and ( Col__ <= Editor.Board.Width  ) and
                ( Row__ > 0 ) and ( Row__ <= Editor.Board.Height ) and
                ( bsiFloor in Editor.Board.Squares[ Col__, Row__ ] );

    end;

    function  IsTopLeftSquareInA2Times2WallQuadrant( Col__, Row__ : Integer ) : Boolean;
    begin
      with Editor.Board do
        Result := ( Col__ > 0 ) and
                  ( Row__ > 0 ) and
                  ( bsiWall in Squares[ Col__, Row__ ] ) and
                  ( Col__ < Width ) and
                  ( Row__ < Height ) and
                  ( bsiWall in Squares[ Succ( Col__ ),       Row__   ] ) and
                  ( bsiWall in Squares[       Col__  , Succ( Row__ ) ] ) and
                  ( bsiWall in Squares[ Succ( Col__ ), Succ( Row__ ) ] );
    end;

    function  RegisterTile( SkinCol__, SkinRow__, SkinQuadrant__, SourceCol__, SourceRow__, SourceQuadrant__ : Integer; SourceRect__ : TRect ) : Boolean;

       function  IsInteriorSquare( Col__, Row__ : Integer ) : Boolean;
       var X, Y : Integer;
       begin
         Result := True;
         for X := -1 to 1 do
             for Y := -1 to 1 do
                 if   ( Col__ + X > 0 ) and ( Col__ + X <= Editor.Board.Width  ) and
                      ( Row__ + Y > 0 ) and ( Row__ + Y <= Editor.Board.Height ) and
                      ( ( Editor.Board.Squares[ Col__ + X, Row__ + Y ] * [ bsiFloor, bsiWall ] ) <> [] ) then
                 else Result := False; // the neighbor square in this direction is outside the board, or more precisely, the player cannot reach this neighbor square
       end;

       function  IsInteriorQuadrant( Col__, Row__, Quadrant__ : Integer ) : Boolean;
       begin
         case Quadrant__ of
           0    : Result := IsBoardSquare(       Col__  , Pred( Row__ ) ) and IsBoardSquare( Succ( Col__ ), Row__         );
           1    : Result := IsBoardSquare( Pred( Col__ ),       Row__   ) and IsBoardSquare(       Col__  , Pred( Row__ ) );
           2    : Result := IsBoardSquare( Pred( Col__ ),       Row__   ) and IsBoardSquare(       Col__  , Succ( Row__ ) );
           else   Result := IsBoardSquare(       Col__  , Succ( Row__ ) ) and IsBoardSquare( Succ( Col__ ), Row__         );
           end;
       end;

    begin
      if SkinQuadrant__ < 0 then begin // 'True': the input is a full tile; register its 4 quadrants individually;
         Result := False;
         for SkinQuadrant__ := 0 to 3 do
             if RegisterTile( SkinCol__, SkinRow__, SkinQuadrant__, SourceCol__, SourceRow__, SkinQuadrant__, SourceRect__ ) then
                Result := True;
         if Result and ( SourceQuadrant__ = 0 ) then
            // special: with [ skin quadrant, source quadrant ] = [ -1, 0 ], the
            // tile is registered as a complete tile, i.e., the complete
            // rectangle is stored in the quadrant 0 slot; this can be used for
            // objects like floors and goals which always consist of full tiles,
            // as opposed to walls which are the only objects split in
            // quadrants;
            Tiles[ SkinCol__, SkinRow__, SourceQuadrant__ ] := SourceRect__;
         end
      else begin
         Result := ( Tiles[ SkinCol__, SkinRow__, SkinQuadrant__ ].Right = 0 ) // 'True': no previous entry; save this one
                   or
                   IsInteriorQuadrant( SourceCol__, SourceRow__, SourceQuadrant__ ); // 'True': the source quadrant isn't on the border, hence, it's more safe to use this tile because it cannot contain pixels from the outside
         if Result then
            Tiles[ SkinCol__, SkinRow__, SkinQuadrant__ ] := QuadrantToRect( SourceQuadrant__, ColWidth, RowHeight, SourceRect__ ); // 'QuadrantToRect()': only grab the specified quadrant
         end
    end;

    function  MakeObject( SkinCol__  , SkinRow__  , SkinBackgroundCol__  , SkinBackgroundRow__,
                          SourceCol__, SourceRow__, SourceBackgroundCol__, SourceBackgroundRow__ : Integer;
                          SourceBitmap__ : TBitmap ) : Boolean;
    var ObjectBitmap, MaskBitmap, BackgroundBitmap : TBitmap; R :TRect;
    begin // "lift" the object up from the background and put it in place on the target background
      Result := ( RectWidth( Tiles[ SkinBackgroundCol__  , SkinBackgroundRow__  , 0 ] ) = ColWidth ) and
                ( RectWidth( Tiles[ SourceCol__          , SourceRow__          , 0 ] ) = ColWidth ) and
                ( RectWidth( Tiles[ SourceBackgroundCol__, SourceBackgroundRow__, 0 ] ) = ColWidth );
      if Result then begin
         ObjectBitmap := nil; MaskBitmap := nil; BackgroundBitmap := nil;
         try     Result := BitmapCreate( ObjectBitmap    , ColWidth, RowHeight ) and
                           BitmapCreate( MaskBitmap      , ColWidth, RowHeight ) and
                           BitmapCreate( BackgroundBitmap, ColWidth, RowHeight );
                 if Result then begin
                    R := Rect( 0, 0, ColWidth, RowHeight );
                    ObjectBitmap    .Canvas.CopyRect( R, SourceBitmap__.Canvas, Tiles[ SourceCol__, SourceRow__, 0 ] );
                    BackgroundBitmap.Canvas.CopyRect( R, SourceBitmap__.Canvas, Tiles[ SourceBackgroundCol__, SourceBackgroundRow__, 0 ] );
                    BitmapMaskBackground            ( ObjectBitmap, MaskBitmap, BackgroundBitmap, 10, False );
                    BackgroundBitmap.Canvas.CopyRect( R, SourceBitmap__.Canvas, Tiles[ SkinBackgroundCol__, SkinBackgroundRow__, 0 ] );
                    BackgroundBitmap.Canvas.CopyMode  := cmSrcAnd;   // and-ing
                    BackgroundBitmap.Canvas.Draw    ( 0, 0, MaskBitmap );
                    BackgroundBitmap.Canvas.CopyMode  := cmSrcPaint; // or-ing
                    BackgroundBitmap.Canvas.Draw    ( 0, 0, ObjectBitmap );
                    SkinBitmap      .Canvas.CopyRect( ColRowQuadrantToRect( SkinCol__, SkinRow__, -1 ),
                                                      BackgroundBitmap.Canvas, R );

                    if ToolsForm.CaptureSettingsMenuItemObjectsOnABlackBackground.Checked then with SkinBitmap do with Canvas do begin // 'True': make a uniformly colored background
                       Brush.Style  := bsSolid;
                       Brush.Color  := clBlack;
                       FillRect( ColRowQuadrantToRect( SkinCol__, SkinRow__, - 1 ) );
                       CopyMode     := cmsrcPaint;
                       Draw( SkinCol__ * ColWidth, SkinRow__ * RowHeight, ObjectBitmap );
                       CopyMode     := cmsrcCopy;
                       BackgroundBitmap.Canvas.Brush.Color := InvertedColor( TColor( ToolsForm.CaptureSettingsMenuItemObjectsOnABlackBackground.Tag ) );
                       BackgroundBitmap.Canvas.FillRect( R );
                       MaskBitmap.Canvas.CopyMode := cmDstInvert;
                       MaskBitmap.Canvas.CopyRect(R, MaskBitmap.Canvas, R); // invert the mask
                       MaskBitmap.Canvas.CopyMode := cmSrcPaint;
                       MaskBitmap.Canvas.Draw( 0, 0, BackgroundBitmap );
                       CopyMode     := cmMergePaint;
                       Draw( SkinCol__ * ColWidth, SkinRow__ * RowHeight, MaskBitmap );
                       CopyMode     := cmsrcCopy;

                       Tiles[ SkinCol__, SkinRow__, 0 ].Right := -1; // '-1': don't copy the object (if any) from the original image to the skin
                       end;
                    end;
         finally ObjectBitmap.Free; MaskBitmap.Free; BackgroundBitmap.Free;
         end;
         end;
    end;

  begin // 'CreateSkin'
    Result := True;
    ZeroMemory( Addr( Tiles ), SizeOf( Tiles ) );
    HasWallCap := False;
    Editor.CapturedSquareTypes := 0;

    with SkinBitmap do with Canvas do begin
      Brush.Color := clDkGray;
      FillRect( Rect( 0, 0, Width, Height ) );
      end;

    for Col := 1 to Editor.Board.Width do
        for Row := 1 to Editor.Board.Height do begin
            R :=ColRowToRect( Pred( Col ), Pred( Row ) );
            Items := Editor.Board.Squares[ Col, Row ];
            if bsiWall in Items then with Editor.Board do begin
               Neighbors := NeighborWalls( Col, Row, Editor.Board );

               // build the "up-down-left-right-neighbors" tile
               if HasWallNeighbors( Neighbors, WALL_UP   + WALL_RIGHT ) and IsFloor( Succ( Col ), Pred( Row ) ) and ( not IsTopLeftSquareInA2Times2WallQuadrant( Col, Pred( Row ) ) ) then
                  RegisterTile( 0, 2, 0, Col, Row, 0, R );
               if HasWallNeighbors( Neighbors, WALL_UP   + WALL_LEFT  ) and IsFloor( Pred( Col ), Pred( Row ) ) and ( not IsTopLeftSquareInA2Times2WallQuadrant( Pred( Col ), Pred( Row ) ) ) then
                  RegisterTile( 0, 2, 1, Col, Row, 1, R );
               if HasWallNeighbors( Neighbors, WALL_LEFT + WALL_DOWN  ) and IsFloor( Pred( Col ), Succ( Row ) ) and ( not IsTopLeftSquareInA2Times2WallQuadrant( Pred( Col ), Row ) ) then
                  RegisterTile( 0, 2, 2, Col, Row, 2, R );
               if HasWallNeighbors( Neighbors, WALL_DOWN + WALL_RIGHT ) and IsFloor( Succ( Col ), Succ( Row ) ) and ( not IsTopLeftSquareInA2Times2WallQuadrant( Col, Row ) ) then
                  RegisterTile( 0, 2, 3, Col, Row, 3, R );

               // build the "no-neighbors" tile
               if Neighbors = WALL_LEFT  + WALL_DOWN then
                  RegisterTile( 1, 3, 0, Col, Row, 0, R );
               if Neighbors = WALL_RIGHT + WALL_DOWN then
                  RegisterTile( 1, 3, 1, Col, Row, 1, R );
               if Neighbors = WALL_RIGHT + WALL_UP then
                  RegisterTile( 1, 3, 2, Col, Row, 2, R );
               if Neighbors = WALL_LEFT  + WALL_UP then
                  RegisterTile( 1, 3, 3, Col, Row, 3, R );

               // build the "up-down" tile, part 1/2
               if Neighbors = WALL_UP + WALL_DOWN + WALL_LEFT then begin
                  RegisterTile( 0, 3, 0, Col, Row, 0, R );
                  RegisterTile( 0, 3, 3, Col, Row, 3, R );
                  end;
               if Neighbors = WALL_UP + WALL_DOWN + WALL_RIGHT then begin
                  RegisterTile( 0, 3, 1, Col, Row, 1, R );
                  RegisterTile( 0, 3, 2, Col, Row, 2, R );
                  end;
               if Neighbors = WALL_UP then begin
                  RegisterTile( 0, 3, 0, Col, Row, 0, R );
                  RegisterTile( 0, 3, 1, Col, Row, 1, R );
                  end;
               if Neighbors = WALL_DOWN then begin
                  RegisterTile( 0, 3, 2, Col, Row, 2, R );
                  RegisterTile( 0, 3, 3, Col, Row, 3, R );
                  end;

               // build the "left-right" tile, part 1/2
               if Neighbors = WALL_LEFT + WALL_RIGHT + WALL_DOWN then begin
                  RegisterTile( 1, 2, 0, Col, Row, 0, R );
                  RegisterTile( 1, 2, 1, Col, Row, 1, R );
                  end;
               if Neighbors = WALL_LEFT + WALL_RIGHT + WALL_UP then begin
                  RegisterTile( 1, 2, 2, Col, Row, 2, R );
                  RegisterTile( 1, 2, 3, Col, Row, 3, R );
                  end;
               if Neighbors = WALL_LEFT then begin
                  RegisterTile( 1, 2, 1, Col, Row, 1, R );
                  RegisterTile( 1, 2, 2, Col, Row, 2, R );
                  end;
               if Neighbors = WALL_RIGHT then begin
                  RegisterTile( 1, 2, 0, Col, Row, 0, R );
                  RegisterTile( 1, 2, 3, Col, Row, 3, R );
                  end;
               end;
            end;

    for Col := 1 to Editor.Board.Width do
        for Row := 1 to Editor.Board.Height do begin
            R :=ColRowToRect( Pred( Col ), Pred( Row ) );
            Items := Editor.Board.Squares[ Col, Row ];
            if bsiWall in Items then with Editor.Board do begin
               Neighbors := NeighborWalls( Col, Row, Editor.Board );

               // collect complete wall tiles like "no-wall-neighbors" and "up-down-neighbors-only", if any
               if Neighbors = WALL_NONE then
                  RegisterTile( 1, 3, -1, Col, Row, -1, R );
               if Neighbors = WALL_UP + WALL_DOWN then
                  RegisterTile( 0, 3, -1, Col, Row, -1, R );
               if Neighbors = WALL_LEFT + WALL_RIGHT then
                  RegisterTile( 1, 2, -1, Col, Row, -1, R );
               if ( Neighbors = ( WALL_LEFT + WALL_RIGHT + WALL_UP + WALL_DOWN ) ) and
                  ( not IsTopLeftSquareInA2Times2WallQuadrant( Pred( Col ), Pred( Row ) ) ) and
                  ( not IsTopLeftSquareInA2Times2WallQuadrant( Col, Pred( Row ) ) ) and
                  ( not IsTopLeftSquareInA2Times2WallQuadrant( Pred( Col ), Row ) ) and
                  ( not IsTopLeftSquareInA2Times2WallQuadrant( Col, Row ) ) then
                  RegisterTile( 0, 2, -1, Col, Row, -1, R );
               if IsTopLeftSquareInA2Times2WallQuadrant( Col, Row ) then begin // 'True': there is a 2 x 2 wall quadrant on the board; assume the image contains a wall cap;
                  Tiles[ 2, 2, 0 ] := QuadrantToRect( 0, ColWidth, RowHeight, Rect( R.Left, R.Bottom, R.Right, R.Bottom + RowHeight ) );
                  Tiles[ 2, 2, 1 ] := QuadrantToRect( 1, ColWidth, RowHeight, Rect( R.Right, R.Bottom, R.Right + ColWidth, R.Bottom + RowHeight ) );
                  Tiles[ 2, 2, 2 ] := QuadrantToRect( 2, ColWidth, RowHeight, Rect( R.Right, R.Top, R.Right + ColWidth, R.Bottom ) );
                  Tiles[ 2, 2, 3 ] := QuadrantToRect( 3, ColWidth, RowHeight, R );
                  HasWallCap := True;
                  Editor.CapturedSquareTypes := Editor.CapturedSquareTypes or ( 1 shl Pred( CAPTURED_SKIN_TILE_WALL_CAP_INDEX ) );
                  end;
               end;
            end;

    for Col := 1 to Editor.Board.Width do
        for Row := 1 to Editor.Board.Height do begin
            R :=ColRowToRect( Pred( Col ), Pred( Row ) );
            Items := Editor.Board.Squares[ Col, Row ];
            if bsiWall in Items then with Editor.Board do begin
               Neighbors := NeighborWalls( Col, Row, Editor.Board );

               // build the "up-down" tile, part 2/2
               if Neighbors = WALL_UP then begin
                  if Tiles[ 0, 3, 3 ].Right = 0 then
                     RegisterTile( 0, 3, 3, Col, Row, 0, R );
                  if Tiles[ 0, 3, 2 ].Right = 0 then
                     RegisterTile( 0, 3, 2, Col, Row, 1, R );
                  end;
               if Neighbors = WALL_DOWN then begin
                  if Tiles[ 0, 3, 1 ].Right = 0 then
                     RegisterTile( 0, 3, 1, Col, Row, 2, R );
                  if Tiles[ 0, 3, 0 ].Right = 0 then
                     RegisterTile( 0, 3, 0, Col, Row, 3, R );
                  end;

               // build the "left-right" tile, part 2/2
               if Neighbors = WALL_LEFT then begin
                  if Tiles[ 1, 2, 0 ].Right = 0 then
                     RegisterTile( 1, 2, 0, Col, Row, 1, R );
                  if Tiles[ 1, 2, 3 ].Right = 0 then
                     RegisterTile( 1, 2, 3, Col, Row, 2, R );
                  end;
               if Neighbors = WALL_RIGHT then begin
                  if Tiles[ 1, 2, 1 ].Right = 0 then
                     RegisterTile( 1, 2, 1, Col, Row, 0, R );
                  if Tiles[ 1, 2, 2 ].Right = 0 then
                     RegisterTile( 1, 2, 2, Col, Row, 3, R );
                  end;
               end;
            end;

    for Col := 1 to Editor.Board.Width do
        for Row := 1 to Editor.Board.Height do begin
            R :=ColRowToRect( Pred( Col ), Pred( Row ) );
            Items := Editor.Board.Squares[ Col, Row ];
            if bsiPlayer in Items then begin
               if bsiGoal in Items then begin
                  Tiles[ 1, 1, 0 ] := R;
                  Editor.CapturedSquareTypes := Editor.CapturedSquareTypes or ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiPlayer, bsiGoal ] ) );
                  end
               else begin
                  Tiles[ 1, 0, 0 ] := R;
                  Editor.CapturedSquareTypes := Editor.CapturedSquareTypes or ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiPlayer ] ) );
                  end;
               end;
            if bsiBox in Items then begin
               if bsiGoal in Items then begin
                  Tiles[ 2, 1, 0 ] := R;
                  Editor.CapturedSquareTypes := Editor.CapturedSquareTypes or ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiBox, bsiGoal ] ) );
                  end
               else begin
                  Tiles[ 2, 0, 0 ] := R;
                  Editor.CapturedSquareTypes := Editor.CapturedSquareTypes or ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiBox ] ) );
                  end;
               end;
            if bsiGoal in Items then begin
               if bsiBox in Items then begin
                  Tiles[ 2, 1, 0 ] := R;
                  Editor.CapturedSquareTypes := Editor.CapturedSquareTypes or ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiBox, bsiGoal ] ) );
                  end
               else if not ( bsiPlayer in Items ) then begin
                       Tiles[ 0, 1, 0 ] := R;
                       Editor.CapturedSquareTypes := Editor.CapturedSquareTypes or ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiGoal ] ) );
                       end;
               end;
            if [ bsiFloor ] = Items then begin
                  RegisterTile( 0, 0, -1, Col, Row, 0, R );
                  Editor.CapturedSquareTypes := Editor.CapturedSquareTypes or ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiFloor ] ) );
               end;
            end;

    if Tiles[ 0, 0, 0 ].Right = 0 then             // empty floor
       Tiles[ 0, 0, 0 ] := Tiles[ 0, 1, 0 ];       // empty floor          := goal

    if Tiles[ 0, 1, 0 ].Right = 0 then             // empty goal
       Tiles[ 0, 1, 0 ] := Tiles[ 0, 0, 0 ];       // empty goal           := floor

    if Tiles[ 1, 0, 0 ].Right = 0 then             // no player-on-floor
       if not MakeObject( 1, 0, 0, 0, 1, 1, 0, 1, OriginalBitmap ) then
          Tiles[ 1, 0, 0 ] := Tiles[ 1, 1, 0 ];    // player-on-floor      := player-on-goal

    if Tiles[ 1, 1, 0 ].Right = 0 then             // no player-on-goal
       if not MakeObject( 1, 1, 0, 1, 1, 0, 0, 0, OriginalBitmap ) then
          Tiles[ 1, 1, 0 ] := Tiles[ 1, 0, 0 ];    // player-on-goal       := player on floor

    if Tiles[ 2, 0, 0 ].Right = 0 then             // no box on-floor
       if not MakeObject( 2, 0, 0, 0, 2, 1, 0, 1, OriginalBitmap ) then
          Tiles[ 2, 0, 0 ] := Tiles[ 2, 1, 0 ];    // box-on-floor         := box-on-goal

    if Tiles[ 2, 1, 0 ].Right = 0 then             // no box-on-goal
       if not MakeObject( 2, 1, 0, 1, 2, 0, 0, 0, OriginalBitmap ) then
          Tiles[ 2, 1, 0 ] := Tiles[ 2, 0, 0 ];    // box-on-goal          := box-on-floor

    if ToolsForm.CaptureSettingsMenuItemObjectsOnABlackBackground.Checked then begin
       // make pushers and boxes with a uniformly colored background if this hasn't been done already
       if Tiles[ 1, 0, 0 ].Right > 0 then             // player-on-floor
          MakeObject( 1, 0, 0, 0, 1, 0, 0, 0, OriginalBitmap );
       if Tiles[ 1, 1, 0 ].Right > 0 then             // player-on-goal
          MakeObject( 1, 1, 0, 1, 1, 1, 0, 1, OriginalBitmap );
       if Tiles[ 2, 0, 0 ].Right > 0 then             // box on-floor
          MakeObject( 2, 0, 0, 0, 2, 0, 0, 0, OriginalBitmap );
       if Tiles[ 2, 1, 0 ].Right = 0 then             // no box-on-goal
          MakeObject( 2, 1, 0, 1, 2, 1, 0, 1, OriginalBitmap );
       end;

    if Tiles[ 2, 2, 0 ].Right = 0 then begin       // empty wallcap        := color or wall
       for Quadrant := 0 to 3 do
           if Result then begin
              R1 := Tiles[ 0, 2, Quadrant ];
              R2 := Tiles[ 1, 3, Quadrant ];
              if RectWidth( R1 ) = ColWidth then
                 R1 := QuadrantToRect( Quadrant, ColWidth, RowHeight, R1 );
              if RectWidth( R2 ) = ColWidth then
                 R2 := QuadrantToRect( Quadrant, ColWidth, RowHeight, R2 );
              if ( R1.Right > 0 ) and ( R2.Right > 0 ) then
                 Result := BitmapEqual( OriginalBitmap, OriginalBitmap, R1, R2, 95 ); // '95': 95% equality threshold
              end;
       if Result then begin // 'True': the "up-down-left-right-neighbors" wall tile and the "no-neighbors" wall tile are identical; the skin probably has tiled walls instead of seamless walls; in that case, the wallcap is simply the wall tile itself;
          for Quadrant := 0 to 3 do
              Tiles[ 2, 2, Quadrant ] := Tiles[ 1, 3, Quadrant]; // use "no-neighbors" wall tile as wallcap
          end
       else with SkinBitmap do with Canvas do begin // fill the wallcap with a uniform color taken from the middle of one of the wall tiles
              R := Tiles[ 0, 2, 1 ];    // "up-down-left-right-neighbors"
              if R.Right = 0 then
                 R := Tiles[ 1, 3, 0 ]; // "no-neighbors"
              if R.Right = 0 then
                 R := Tiles[ 1, 2, 0 ]; // "left-right-neighbors"
              if R.Right = 0 then
                 R := Tiles[ 0, 3, 0 ]; // "up-down-neighbors"

              if RectWidth( R ) = ColWidth then
                 Dec( R.Right, ColWidth div 2 );
              if RectHeight( R ) = RowHeight then
                 Dec( R.Bottom, RowHeight div 2 );
              if ( R.Right > 0 ) and ( R.Bottom > 0 ) then begin
                 Brush.Color := OriginalBitmap.Canvas.Pixels[ Pred( R.Right ), Pred( R.Bottom ) ];
                 for Quadrant := 0 to 3 do
                     FillRect(  ColRowQuadrantToRect( 2, 2, Quadrant ) );
                 end;
              end;

       Result := True;
       end;

    for Col := 0 to 3 do
        for Row := 0 to 3 do
            for Quadrant := 0 to 3 do
                if Tiles[ Col, Row, Quadrant ].Right > 0 then
                   SkinBitmap.Canvas.CopyRect( ColRowQuadrantToRect( Col, Row, Quadrant ), OriginalBitmap.Canvas, Tiles[ Col, Row, Quadrant ] );

    //ChangeBlackPixels( SkinBitmap, 16 );

    if ( not HasWallCap ) and
       ( ToolsForm.WallCapCheckBox.Tag = 0 ) then // '0': 'checked' isn't a user value or a value from a matching skin
       ToolsForm.WallCapCheckBox.Checked := False;

    WriteSkinSettings;

    //SkinBitmap.SaveToFile( 't10.bmp' );
    //OriginalBitmap.SaveToFile( 't11.bmp' );
  end; // CreateSkin

  function  CreateSingleRowSkin : Boolean;
  begin
    SingleRowSkinBitmap.Free; SingleRowSkinBitmap := nil;
    Result := CreateSingleRowSkinFromSkin( SkinBitmap, ColWidth, RowHeight, SingleRowSkinBitmap );
    if not Result then begin
       SingleRowSkinBitmap.Free; SingleRowSkinBitmap := nil;
       end;
  end; // CreateSingleRowSkin

  function LoadMatchingSkin : Boolean;
  var
    i : Integer;
    MatchingSkinImagesChanged : Boolean;
    R : TRect;
    MatchingSkinCaptureInformation : PSkinCaptureInformation; // skin capture information from a matching skin image
    MatchingSkinBitmap : TBitmap;                             // a matching skin used for extracting a board from the loaded image
    CapturedBoard : TCaptureBoard;

  begin
    Result := False;
    MatchingSkinBitmap := nil; MatchingSkinCaptureInformation := nil;
    try
      if LoadImageFromFile         ( MatchingSkinFileName, MatchingSkinBitmap                        ) and
         ReadSkinCaptureInformation( MatchingSkinBitmap, False, i, i, MatchingSkinCaptureInformation ) and
         ( MatchingSkinCaptureInformation.Header.ColumnWidth = ToolsForm.ColWidthSpinEdit .Value     ) and
         ( MatchingSkinCaptureInformation.Header.RowHeight   = ToolsForm.RowHeightSpinEdit.Value     ) and
         ( MatchingSkinBitmap.Width  >= 4 * ToolsForm.ColWidthSpinEdit .Value ) and // the matching skin must have 4 x 4 tiles or more; this is a precondition for 'CreateSingleRowSkin'
         ( MatchingSkinBitmap.Height >= 4 * ToolsForm.RowHeightSpinEdit.Value ) and
         FindBoardUsingUnscaledSkin( nil, MatchingSkinCaptureInformation, NormalizedBoardRect, Settings.MatchThresholdPct, R, CapturedBoard ) and // 'True': the skin still matches the board, at least to some extent
         ( ( Abs( R.Left - ToolsForm.LeftSpinEdit.Value ) mod Max( 1, ToolsForm.ColWidthSpinEdit .Value ) ) = 0 ) and
         ( ( Abs( R.Top  - ToolsForm.TopSpinEdit .Value ) mod Max( 1, ToolsForm.RowHeightSpinEdit.Value ) ) = 0 ) then begin    // 'True': the matching skin is still aligned with the cells on the board
         SkinBitmap.Free; SkinBitmap := MatchingSkinBitmap; MatchingSkinBitmap := nil; // use the matching skin graphics instead of the graphics from the currently loaded image
         Editor.CapturedSquareTypes  := MatchingSkinCaptureInformation. Header.CapturedSquareTypes; // update the captured square types according to the graphics in the matching skin
         Result := CreateSingleRowSkin and
                   CreateSkinCaptureInformationUpdatingMatchingSkin( MatchingSkinImagesChanged ) and
                   WriteSkinCaptureInformation ( SkinBitmap, SkinCaptureInformation );
         if Result then
            try    WriteSkinSettings;
                   SkinBitmap.SaveToFile( MatchingSkinFileName );
                   GetFileTime( MatchingSkinFileName, Editor.SkinFileTime );
            except on E:Exception do Result := Error( E.Message, '' );
            end;
         end;
    finally MatchingSkinBitmap.Free; FreeMem( MatchingSkinCaptureInformation );
            if   Result then
                 Editor.SkinFileName := MatchingSkinFileName // use the matching skin as the current skin
            else ClearFileTime( Editor.SkinFileTime );
    end;
  end; // LoadMatchingSkin

begin // CreateSkinFromImage
  ColWidth     := ToolsForm.ColWidthSpinEdit.Value;
  RowHeight    := ToolsForm.RowHeightSpinEdit.Value;
  OriginalZoom := Editor.Zoom;
  Result       := ( ColWidth > 0 ) and ( RowHeight > 0 );

  try
    if  Result then begin
        SingleRowSkinBitmap.Free; SingleRowSkinBitmap := nil;
        SkinBitmap.Free; SkinBitmap := nil;
        Result := BitmapCreate( SkinBitmap, 4 * ColWidth, 4 * RowHeight );
        if Result then begin
           Screen.Cursor := crHourGlass;
           Editor.Zoom   := 100;
           Result        := CreateSkin and
                            CreateSingleRowSkin;

           if Result and
              CreateSkinCaptureInformationUpdatingMatchingSkin( MatchingSkinImagesChanged ) then begin
              WriteSkinCaptureInformation( SkinBitmap, SkinCaptureInformation );

              if ( MatchingSkinFileName <> '' )
                 and
                 ( not StrEqual( MatchingSkinFileName, DefaultMatchingSkinFileName ) )
                 and
                 ( MatchingSkinImagesChanged
                   or
                   ( not StrEqual( MatchingSkinFileName, Editor.SkinFileName ) )
                 ) then
                 // use the matching skin graphics instead of the graphics from
                 // the currently loaded image;
                 LoadMatchingSkin;
              end;
           end;
	end;

  finally Screen.Cursor := crDefault;
          Editor.Zoom   := OriginalZoom;
          // FinalizeColors( Colors );
          //ShowImage( Editor.Zoom );
          if not Result then begin
             SingleRowSkinBitmap.Free; SingleRowSkinBitmap := nil;
             SkinBitmap.Free; SkinBitmap := nil; Editor.SkinFileName := '';
             ClearFileTime( Editor.SkinFileTime );
             Error( TEXT_CREATE_SKIN + ': ' + TEXT_TASK_FAILED, '' );
             end;
  end;
end;

function  TCaptureForm.SetColorLevel( Value__ : Integer ) : Boolean;
var Index : Integer;
begin
  Result := False;
  with ToolsForm.CaptureSettingsMenuItemColorQuantization do
    for Index := 0 to Pred( Count ) do
        if Items[ Index ].Tag = Value__ then begin
           Items[ Index ].Checked := True;
           if Settings.ColorLevels <> Value__ then begin
              Settings.ColorLevels := Value__;
              Colors.Count := 0;
              end;
           Result := True;
           end;

  if Editor.Step >= csBoardSquares then begin
     if ( ToolsForm.CaptureViewMenuItemShowColorQuantization.Checked or ToolsForm.CaptureSettingsMenuItemUseAutomaticCompletion.Checked )
        and
        ( Colors.Count = 0 ) then
        AnalyseBoardColors;
     if Editor.Step < csSaveSkin then
        ShowBoard;
     end;
end;

procedure TCaptureForm.SettingsColorLevelItemClick(Sender: TObject);
begin
  ToolsForm.CaptureViewMenuItemShowColorQuantization.Checked := True;
  SetColorLevel( TMenuItem( Sender ).Tag );
end;

procedure TCaptureForm.SettingsIgnoreSmallColorDifferencesClick(Sender: TObject);
begin
  with ToolsForm.CaptureSettingsColorQuantizationIgnoreSmallColorDifferences do Checked := not Checked;
end;

procedure TCaptureForm.CaptureViewColorQuantizationItemClick(Sender: TObject);
begin
  with ToolsForm.CaptureViewMenuItemShowColorQuantization do Checked := not Checked;
  SetColorLevel( Settings.ColorLevels );
end;

procedure TCaptureForm.CaptureViewImageAnalysisResultsItemClick(Sender: TObject);
var AnalyseImageResultSet : TAnalyseImageResultSet;
begin
  with ToolsForm.CaptureViewMenuItemShowImageAnalysisResults do begin
    AnalyseImageResultSet := [ airHorzLines, airVertLines, airIntegralImage{, airGradientDirections} ];
    if   ToolsForm.CaptureSettingsColorQuantizationIgnoreSmallColorDifferences.Checked then
         Include( AnalyseImageResultSet, airIntegralImage2 );
    if   Assigned( Sender ) then
         Checked := not Checked;
    if   Checked and
         ( Editor.Step >= csBoard ) and
         AnalyseImage( OriginalBitmap, IntegralImageWidth, IntegralImageHeight, AnalyseImageResultSet, HorzLines, VertLines, IntegralImage, IntegralImage2, GradientDirections ) then begin
         end
    else if True {Editor.Step < csSaveSkin} then begin
            ToolsForm.CaptureImage2.Hide;
            if ( ToolsForm.CaptureScrollBox.VertScrollBar.Range <= ToolsForm.CaptureScrollBox.ClientHeight ) then begin
               // work-around: for some unknown reason, 'CaptureImage2' doesn't disappear from the screen if the vertical scrollbar wasn't needed while having both 'CaptureImage1' and 'CaptureImage2' on the screen
               try     ToolsForm.CaptureScrollBox.Hide;
               finally ToolsForm.CaptureScrollBox.Show;
               end;
               end;
            if ( not Checked ) and Assigned( Sender ) and ( Editor.Step >= csSaveSkin ) then
               ShowBoard;
            end;
    UpdateScrollBoxRange;
    end;
end;

procedure TCaptureForm.SettingsImageAnalysisItemsClick(
  Sender: TObject);
begin
  if Sender is TMenuItem then with Sender as TMenuItem do
     Checked := True;
  ToolsForm.CaptureViewMenuItemShowImageAnalysisResults.Checked := True;
  CaptureViewImageAnalysisResultsItemClick( nil );
end;

procedure TCaptureForm.SettingsSkinExportFormatTypeItemClick( Sender: TObject);
begin
  if Sender is TMenuItem then with Sender as TMenuItem do Checked := True;
end;

procedure TCaptureForm.SettingsSkinExportFormatObjectBackgroundClick(
  Sender: TObject);
begin
  if Sender is TMenuItem then with Sender as TMenuItem do Checked := True;

  if Editor.CompletedStep >= csSaveSkin then
     Editor.CompletedStep := Pred( csSaveSkin );
  if Editor.Step >= csSaveSkin then begin
     if   CreateSkinFromImage then begin
          Editor.CompletedStep := csSaveSkin;
          Editor.EditorSkinBitmap.Free; Editor.EditorSkinBitmap := nil; Editor.EditorSkinTileCount := 0;
          try
            if Editor.Zoom <> 100 then
               ToolsForm.CaptureScrollBox.Hide;
            if ToolsForm.CaptureSettingsMenuItemObjectsOnABlackBackground.Checked then
               Editor.Zoom := 100;
            ShowImage( Editor.Zoom );
          finally
            ToolsForm.CaptureScrollBox.Show;
          end;
          end
     else PreviousStepButtonClick( Sender );
     end;
end;

procedure TCaptureForm.CaptureSettingsGridColorItemClick(Sender: TObject);
begin
  with ColorDialog1 do begin
    Color := Editor.GridColor;
    if Execute then begin
       Editor.GridColor := Color;
       if Editor.HasSelection and
          ( Editor.Step >= csBoard ) and (Editor.Step < csBoardSquares ) then with Editor do
          ShowCursor( Cursor, CursorRect );
       end;
    end;
end;

procedure TCaptureForm.SetMatchingStringFileName( FileName__ :String );
begin
  fMatchingSkinFileName := Trim( FileName__ );
  if   ( MatchingSkinFileName = '' ) or StrEqual( MatchingSkinFileName, DefaultMatchingSkinFileName ) then
       ToolsForm.CaptureFileMenuItemNewSkin.Hint := TEXT_HINT_CREATE_NEW_SKIN
  else ToolsForm.CaptureFileMenuItemNewSkin.Hint := TEXT_HINT_CREATE_NEW_SKIN + COLON + SPACE + StrWithDoubleQuotes( AbbreviatedFilePath( MatchingSkinFileName, MainForm.MyDocumentsFolder ) );
  ShowStatus;
end;

function  CalculateColorDistanceSquared( Color1__, Color2__ : TColor) : Integer;
var Distance : Integer;
begin // returns | c1 - c2 | ^ 2, i.e., the color distance squared
  Distance :=  (   Color1__          and $ff ) - (   Color2__          and $ff );
  Result := Distance * Distance;
  Distance :=  ( ( Color1__ shr  8 ) and $ff ) - ( ( Color2__ shr  8 ) and $ff );
  Inc( Result, Distance * Distance );
  Distance :=  ( ( Color1__ shr 16 ) and $ff ) - ( ( Color2__ shr 16 ) and $ff );
  Inc( Result, Distance * Distance );
end;

function  TCaptureForm.BitmapColorQuantization( Bitmap__                         : TBitmap;
                                                const Rect__                     : TRect;
                                                PaletteColorCount__              : Integer; // maximum palette size
                                                DoNotMergeDifferentColorGroups__ : Boolean;
                                                var Colors__                     : TCaptureColors ) :Boolean;
// preconditions: the bitmap is a 24-bits-per-pixel bitmap;
//                the color table has been initialized;
// if the color table is non-empty, then it's assumed to contain the bitmap
// histogram, in which case it isn't calculated anew;
// if 'DoNotMergeDifferentColorGroups__' is 'True', some of the colors belong to
// different groups; a color with a non-zero group number is never merged with
// a color having a different non-zero group number; the group numbers can be
// 1..127 and are stored in the alpha-channel of each color;
const
  MAXIMUM_DOUBLE_VALUE = 1.7976931348623158E+308;
type
  TDoubleVector = array[ 0 .. ( MaxInt div SizeOf( Double ) ) - 1 ] of Double;
  PErrorMatrixRow = ^TDoubleVector;
  TErrorMatrix = array[ 0 .. ( MaxInt div SizeOf( PErrorMatrixRow ) ) - 1] of PErrorMatrixRow;
  PErrorMatrix = ^TErrorMatrix;
var
  Index, Index1, Index2, PixelCount : Integer;
  Color : TColor;
  Errors : PErrorMatrix;

  function  CalculateError( Index1__, Index2__ : Integer; var Colors__ : TCaptureColors ) : Double;
  var F1, F2 : Double;
  begin
    with Colors__ do begin
      F1     := Colors^[ Index1__ ].Frequency;
      F2     := Colors^[ Index2__ ].Frequency;
      Result := ( ( ( F1 * F2 * F2 ) + ( F2 * F1 * F1 ) )
                  /
                  ( ( F1 + F2 ) * ( F1 + F2 ) )
                ) *
                CalculateColorDistanceSquared( Colors^[ Index1__ ].Color,
                                               Colors^[ Index2__ ].Color );
      if      Result < 1E-75 then Result := 1E-75 // make an attempt to avoid overflows/underflows in further calculations
      else if Result > 1E+75 then Result := 1E+75
      end;
  end;

  function  GetError( Index1__, Index2__ : Integer; var Errors__ : PErrorMatrix ) : Double;
  begin // the error matrix is a lower triangular matrix
    if   Index1__ >= Index2__ then
         Result   := Errors__^[ Index1__ ]^[ Index2__ ]
    else Result   := Errors__^[ Index2__ ]^[ Index1__ ];
  end;

  procedure SetError( Index1__, Index2__ : Integer; Value__ : Double; var Errors__ : PErrorMatrix );
  begin // the error matrix is a lower triangular matrix
    if   Index1__ =  Index2__ then
         ToolsForm.StatusBar1.Panels[ 0 ].Text := IntToStr( Index1__ );
    if   Index1__ >= Index2__ then
         Errors__^[ Index1__ ]^[ Index2__ ] := Value__
    else Errors__^[ Index2__ ]^[ Index1__ ] := Value__;
  end;

  function InitializeErrors( Count__ : Integer; var Errors__ : PErrorMatrix ) : Boolean;
  var Index : Integer;
  begin
    try
      Errors__ := AllocMem( Count__ * SizeOf( Errors__^[ 0 ] ) );
      for Index := 0 to Pred( Count__ ) do begin
          Errors__^[ Index ] := AllocMem( Succ( Index ) * SizeOf( Errors__^[ Index ]^[ 0 ] ) );
          end;
      Result := True;
    except on E:Exception do Result := False;
    end;
  end;

  procedure FinalizeErrors( Count__ : Integer; var Errors__ : PErrorMatrix );
  var Index : Integer;
  begin
    for Index := Pred( Count__ ) downto 0 do
        FreeMem( Errors__^[ Index ] );
    FreeMem( Errors__ ); Errors__ := nil;
  end;

  procedure CalculateErrors( var Colors__ : TCaptureColors; var Errors__ : PErrorMatrix );
  var i, j : Integer;
  begin
    for i := 0 to  Pred( Colors__.Count ) do
        for j := 0 to Pred( i ) do
            SetError( i, j, CalculateError( i, j, Colors__ ), Errors__ );
  end;

  function  SelectColorPair( var Index1__, Index2__ : Integer; var Colors__ : TCaptureColors; Errors__ : PErrorMatrix ) : Boolean;
  var i, j : Integer; MinimumError : Double;
  begin
    // the function should always succeed, but just to ensure that inaccurate
    // floating point calculations don't cause havoc to the logic, the function
    // explicitly checks that it returns a color pair;
    Result := False;
    MinimumError := MAXIMUM_DOUBLE_VALUE;
    for i := 0 to Pred( Colors__.Count ) do
        for j := 0 to Pred( i ) do
            if ( Errors__^[ i ]^[ j ] < MinimumError )
               and
               ( ( not DoNotMergeDifferentColorGroups__ )
                 or
                 ( GetAlphaChannel( Colors__.Colors^[ i ].Color ) = 0 )
                 or
                 ( GetAlphaChannel( Colors__.Colors^[ j ].Color ) = 0 )
                 or
                 ( GetAlphaChannel( Colors__.Colors^[ i ].Color ) = GetAlphaChannel( Colors__.Colors^[ j ].Color ) )
               ) then begin
               MinimumError := Errors__^[ i ]^[ j ];
               Index1__     := i;
               Index2__     := j;
               Result       := True;
               end;
  end;

  function  MergeColors( Index1__, Index2__ : Integer; var Colors__ : TCaptureColors ) : TColor;
  var RGB, RGB1, RGB2: TRGB; F1, F2, FA, FB : Double;
  begin
    with Colors__ do begin
      F1     := Colors^[ Index1__ ].Frequency;
      F2     := Colors^[ Index2__ ].Frequency;
      FA     := F1 / ( F1 + F2 );
      FB     := F2 / ( F1 + F2 );
      RGB1   := ColorToRGB( Colors^[ Index1__ ].Color );
      RGB2   := ColorToRGB( Colors^[ Index2__ ].Color );
      RGB.r  := Max( 0, Min( 255, Trunc( ( FA * RGB1.r  ) + ( FB * RGB2.r ) ) ) );
      RGB.g  := Max( 0, Min( 255, Trunc( ( FA * RGB1.g  ) + ( FB * RGB2.g ) ) ) );
      RGB.b  := Max( 0, Min( 255, Trunc( ( FA * RGB1.b  ) + ( FB * RGB2.b ) ) ) );
      Result := RGBToColor( RGB );
      if DoNotMergeDifferentColorGroups__ then // 'True': keep color group numbers
         Result := SetAlphaChannel(
                     GetAlphaChannel( Colors^[ Index1__ ].Color ) or // 'or': the colors are assumed to be compatible, meaning they have identical group numbers, or one of them has no group number
                     GetAlphaChannel( Colors^[ Index2__ ].Color ),
                     Result );
      end;
  end;

  procedure ReplaceColors( Index1__, Index2__ : Integer; Color__ : TColor; var Colors__ : TCaptureColors; Errors__ : PErrorMatrix );
  var Index : Integer;
  begin
    // note that replacement works even if the new color already exists
    // elsewhere in the table; one of the next rounds will select the identical
    // colors for merging because they minimize the quantization error;
    with Colors__ do begin
      with Colors^[ Index1__ ] do begin // reuse item 'Index1__'
        Color            := Color__;
        Frequency        := Frequency + Colors^[ Index2__ ].Frequency;
        Error            := GetError( Index1__, Index2__, Errors__ ) + Error + Colors^[ Index2__ ].Error;
        Inc( Count, Colors^[ Index2__ ].Count ); // unused
        end;

      Dec( Count ); // decrement the number of colors

      Colors^[ Index2__ ] := Colors^[ Count ]; // move last table item to 'Index2__'

      if Index2__ < Count then begin
         // move the calculated errors for the table item which has moved to 'Index2__'
         for Index := 0 to Pred( Index2__ ) do
             SetError( Index2__, Index,    GetError( Count, Index, Errors__ ), Errors__ );
         for Index := Succ( Index2__ ) to Pred( Count ) do
             SetError( Index,    Index2__, GetError( Count, Index, Errors__ ), Errors__ );
         end;

      // calculate quantization error for all [ new-color x color ] pairs
      for Index := 0 to Pred( Count ) do
          if Index <> Index1__ then
             SetError( Index1__, Index, CalculateError( Index1__, Index, Colors__ ), Errors__ );
      end;
  end;

  procedure ApplyQuantizationToBitmap24( Bitmap__ : TBitmap; const Rect__ : TRect; var Colors__ : TCaptureColors );
  var X, Y, Index : Integer;
      BitmapColor, QuantizedColor : TColor; Distance, MinimumDistance : Double; P : PRGB;
  begin
    for Y := Rect__.Top to Pred( Rect__.Bottom ) do begin
        P := Bitmap__.ScanLine[ Y ];
        Inc( P, Rect__.Left );
        for X := Rect__.Left to Pred( Rect__.Right ) do begin
            BitmapColor := RGBToColor( P^ );
            MinimumDistance := MAXIMUM_DOUBLE_VALUE;
            for Index := 0 to Pred( Colors__.Count ) do begin
                QuantizedColor := Colors__.Colors^[ Index ].Color;
                Distance := CalculateColorDistanceSquared( BitmapColor, QuantizedColor );
                if Distance < MinimumDistance then begin
                   MinimumDistance := Distance;
                   P^ := ColorToRGB( QuantizedColor );
                   end;
                end;
            Inc( P );
            end;
        end;
  end;

  procedure ApplyQuantizationToBitmap32( Bitmap__ : TBitmap; const Rect__ : TRect; var Colors__ : TCaptureColors );
  var X, Y, Index, BitmapColorGroup, QuantizedColorGroup : Integer;
      BitmapColor, QuantizedColor : TColor; Distance, MinimumDistance : Double; P : PRGBA;
  begin
    for Y := Rect__.Top to Pred( Rect__.Bottom ) do begin
        P := Bitmap__.ScanLine[ Y ];
        Inc( P, Rect__.Left );
        for X := Rect__.Left to Pred( Rect__.Right ) do begin
            BitmapColor := TColor( P^ );
            BitmapColorGroup := GetAlphaChannel( BitmapColor );
            MinimumDistance := MAXIMUM_DOUBLE_VALUE;
            for Index := 0 to Pred( Colors__.Count ) do begin
                QuantizedColor := Colors__.Colors^[ Index ].Color;
                QuantizedColorGroup := GetAlphaChannel( QuantizedColor );
                Distance := CalculateColorDistanceSquared( BitmapColor, QuantizedColor );
                if ( Distance              < MinimumDistance ) and
                   ( ( BitmapColorGroup    = 0 )
                     or
                     ( QuantizedColorGroup = 0)
                     or
                     ( BitmapColorGroup    = QuantizedColorGroup )
                   ) then begin
                   MinimumDistance := Distance;
                   SetAlphaChannel( BitmapColorGroup or QuantizedColorgroup, QuantizedColor ); // 'or': the colors are compatible at this point, meaning they have identical group numbers, or one of them has no group number
                   P^ := TRGBA( QuantizedColor );
                   end;
                end;
            Inc( P ); // advance to next bitmap pixel
            end;
        end;
  end;

begin // BitmapColorQuantization
  Result := Assigned (Bitmap__ )
            and
            ( ( Bitmap__.PixelFormat = pf24Bit )
              or
              ( Bitmap__.PixelFormat = pf32Bit )
            )
            and
            ( PaletteColorCount__ > 0 )
            and
            ( ( Colors__.Count <> 0 ) // '<>0': histogram already calculated
              or
              ( ( Bitmap__.PixelFormat = pf24Bit ) // calculating histogram only implemented for 24-bit bitmaps
                and
                BitmapHistogram( Bitmap__, Rect__, Colors__ )
              )
            );
  if Result then begin
     // calculate total number of pixels in image
     PixelCount := 0;
     for Index := 0 to Pred( Colors__.Count ) do
         Inc( PixelCount, Colors__.Colors^[ Index ].Count );

     // calculate color frequencies
     for Index := 0 to Pred( Colors__.Count ) do
         with Colors__.Colors^[ Index ] do begin
           Frequency := Count / PixelCount;
           Error     := 0.0;
           end;

     // initialize (create) the [ color x color] lower triangular error matrix
     Result := InitializeErrors( Colors__.Count, Errors );
     if Result then begin
        Screen.Cursor := crHourGlass;
        ToolsForm.StatusBar1.Panels[ 1 ].Text := TEXT_ANALYSING_IMAGE_PLEASE_WAIT;
        try     // calculate quantization error for all [ color x color ] pairs
                // of colors in the input set
          try
                 CalculateErrors( Colors__, Errors );

                 while ( Colors__.Count > PaletteColorCount__ ) and
                       Result do begin
                       Result := SelectColorPair( Index1, Index2, Colors__, Errors );
                       if Result then begin
                          Color := MergeColors( Index1, Index2, Colors__ );
                          ReplaceColors( Index1, Index2, Color, Colors__, Errors );
                          if Colors__.Count mod 16 = 0 then begin
                             ToolsForm.StatusBar1.Panels[ 0 ].Text := IntToStr( Colors__.Count ) + SPACE + TEXT_COLORS;
                             ToolsForm.StatusBar1.Update;
                             end;
                          end;
                       end;

                 if   Result then
                      if   Bitmap__.PixelFormat = pf24Bit then
                           ApplyQuantizationToBitmap24( Bitmap__, Rect__ , Colors__ )
                      else ApplyQuantizationToBitmap32( Bitmap__, Rect__ , Colors__ )
                 else if   DoNotMergeDifferentColorGroups__ and ( Bitmap__.PixelFormat = pf32Bit ) then // 'True': update even though full quantization failed
                           ApplyQuantizationToBitmap32( Bitmap__, Rect__ , Colors__ );

          except on E:Exception do begin
                    Colors__.Count := 0;
                    Result := False;
                    end;
          end;
        finally // finalize (destroy) the sparse error matrix
                FinalizeErrors( Colors__.Count, Errors );
                ToolsForm.StatusBar1.Panels[ 0 ].Text := '';
                ToolsForm.StatusBar1.Panels[ 1 ].Text := '';
                Screen.Cursor := crDefault;
        end;
        end;
     end;
end;

function  TCaptureForm.AverageCellColor( Col__, Row__ : Integer ) : TColor;
var X, Y : Integer;
begin
  ColRowToNormalizedXY( Col__, Row__, X, Y  );
  Result := CalculateAverageColor( Rect( X + 2, Y + 2, X + ToolsForm.ColWidthSpinEdit.Value - 2, Y + ToolsForm.RowHeightSpinEdit.Value - 2 ), OriginalBitmap ); // '+2' and '-2': omit 2 outer pixel columns/rows
end;

function  TCaptureForm.AnalyseBoardColors : Boolean;
var
  a, b, i, j, m, n, x, y, Count, MarginX, MarginY, OriginalZoom : Integer;
  SumR, SumG, SumB : Int64;
  P : PRGB;
begin
  Colors.Count := 0;
  OriginalZoom := Editor.Zoom;
  Result       := ( ToolsForm.ColWidthSpinEdit.Value > 0 ) and ( ToolsForm.RowHeightSpinEdit.Value > 0 );

  try
    if Result then begin
       //InitializeColors( Colors );

       Screen.Cursor := crHourGlass;
       Editor.Zoom   := 100;
       a := 0; b := 0; // [a, b] : next free pixel in background bitmap
       if        ToolsForm.ColWidthSpinEdit.Value  >= 6 then
                 MarginX   := 2
       else if   ToolsForm.ColWidthSpinEdit.Value  >= 4 then
                 MarginX   := 1
            else MarginX   := 0;
       if        ToolsForm.RowHeightSpinEdit.Value >= 6 then
                 MarginY   := 2
       else if   ToolsForm.RowHeightSpinEdit.Value >= 4 then
                 MarginY   := 1
            else MarginY   := 0;

       with Editor do with OriginalBitmap do with Canvas do
         for i := 0 to Pred( ToolsForm.ColumnsSpinEdit.Value ) do
             for j := 0 to Pred( ToolsForm.RowsSpinEdit.Value ) do begin
                 ColRowToXY( i, j, x, y );
                 Inc( x, MarginX ); // drop outer pixel rows/columns
                 Inc( y, MarginY );
                 SumR := 0; SumG := 0; SumB := 0; Count := 0;
                 for m := MarginY to Pred( ToolsForm.RowHeightSpinEdit.Value ) - MarginY do begin
                     P := ScanLine[ y + m ];
                     Inc( P, x );
                     for n := MarginX to Pred( ToolsForm.ColWidthSpinEdit.Value ) - MarginX do begin
                         Inc( SumR, P^.r );
                         Inc( SumG, P^.g );
                         Inc( SumB, P^.b );
                         Inc( Count );
                         Inc( P );
                         end;
                     end;
                 Count                     := Max( 1, Count );
                 BoardSquareColors[ i, j ] := RGBComponentsToColor( SumR div Count, SumG div Count, SumB div Count ); // average color
                 Result                    := Result and ( AddColorToTable( BoardSquareColors[ i, j ], Colors ) >= 0 );

                 // use the background bitmap to store a rectangle with the cell
                 // colors;
                 BackgroundBitmap.Canvas.Pixels[ a, b ] := BoardSquareColors[ i, j ];
                 Inc( a );
                 if a >= BackgroundBitmap.Width then begin
                    a := 0; Inc( b );
                    end;
                 end;

       if Result then begin
          if a > 0 then begin
             // the last pixel row with cell colors isn't complete; include the
             // entire row;
             a := BackgroundBitmap.Width;
             Inc( b );
             // the color quantization function takes a bitmap rectangle as
             // input/output; since the bitmap histogram counts already have been
             // calculated - instead of leaving it to the color quantization
             // function - it doesn't matter that the last pixel row with cell
             // colors contains uninitialized pixels;
             end;
          // perform color quantization
          Result := BitmapColorQuantization( BackgroundBitmap, Rect( 0, 0, a, b ), Settings.ColorLevels, False, Colors );
          // use the calculated palette as cell colors
          if Result then begin
             a := 0; b := 0;
             for i := 0 to Pred( ToolsForm.ColumnsSpinEdit.Value ) do
                 for j := 0 to Pred( ToolsForm.RowsSpinEdit.Value ) do begin
                     BoardSquareColors[ i, j ] := BackgroundBitmap.Canvas.Pixels[ a, b ];
                     Inc( a );
                     if a >= BackgroundBitmap.Width then begin
                        a := 0; Inc( b );
                        end;
                     end;
             end;
          end;

       //Memo1.Clear;
       //with Colors do
       //  for Index := 0 to Pred( Count ) do with Colors^[ Index ] do
       //      Memo1.Lines.Add( IntToStr( Index ) + ': ' + IntToStr( Count ) + ': ' + ColorToText( Color ) );
       end;

  finally Screen.Cursor := crDefault;
          Editor.Zoom   := OriginalZoom;
          // FinalizeColors( Colors );
          //ShowImage( Editor.Zoom );
  end;
end;

function  TCaptureForm.BoardSquareGradientDirectionsHashValue( Col__, Row__ : Integer ) : TColor;
begin // for conveniency, the result is an rgb color, so it can OR'ed with an existing color which has reserved the 4 low bits in each rgb component for the hash value
  if   Assigned( GradientDirections ) and ( IntegralImageWidth > 0 ) then
       Result := CalculateGradientDirectionsHashValue( NormalizedRect( ColRowToRect( Col__, Row__ ) ), IntegralImageWidth, GradientDirections )
  else Result := 0;
end;

procedure TCaptureForm.DisplayPixels( const Caption__ : String; Width__, Height__ : Integer; Pixels__ : PPixels; Scale__ : Double );
var Col, Row : Integer; Pixel : PPixel; TextLine : String;
begin
  if Caption__ <> '' then begin
     ToolsForm.Memo1.Lines.Add( '' );
     ToolsForm.Memo1.Lines.Add( Caption__ );
     end;

  Pixel := PPixel( Pixels__ );
  for Row := 0 to Pred( Height__ - 0 ) do begin
      TextLine := Format( '%6d: ', [ Row ] );
      for Col := 0 to Pred( Width__ - 0 ) do begin
          TextLine := TextLine + Format( '%8d', [ Round( ( Pixel^ {and 255} ) * Scale__ ) ] );
          Inc( Pixel );
          end;
      ToolsForm.Memo1.Lines.Add( TextLine );
      end;
end;

function  TCaptureForm.AnalyseImage( Bitmap__ : TBitmap; var Width__, Height__ : Integer; Results__ : TAnalyseImageResultSet; var HorzLines__, VertLines__, IntegralImage__, IntegralImage2__, GradientDirections__ : PPixels ) : Boolean;
type // returns the horizontal lines, the vertical lines, and the integral image;
//TPixel = Integer;
//PPixel = ^TPixel;
//TPixels = array[ 0 .. ( MaxInt div SizeOf( TPixel ) ) -1 ] of TPixel;
//PPixels = ^TPixels;
  TFilterElement = Integer;
  PFilterElement = ^TFilterElement;
  TFilter = array[ 0 .. ( MaxInt div SizeOf( TFilterElement ) ) -1 ] of TFilterElement;
  PFilter = ^TFilter;
  THistogram = record
    Total : Integer;
    Bins  : array[ 0 .. 255 ] of Integer;
    end;
const
//MAX_IMAGE_HEIGHT = 1080
//MAX_IMAGE_WIDTH  = 1920

  GaussianBlurFilter: array[ 0 .. 24 ] of TFilterElement =
    ( 1,  4,  7,  4, 1,
      4, 16, 26, 16, 4,
      7, 26, 41, 26, 7,
      4, 16, 26, 16, 4,
      1,  4,  7,  4, 1 ); // sum = 273
  HorizontalLinesFilter : array[ 0 .. 8 ] of TFilterElement =
    ( -1, -1, -1,  2,  2,  2, -1, -1, -1 );
  VerticalLinesFilter : array[ 0 .. 8 ] of TFilterElement =
    ( -1,  2, -1, -1,  2, -1, -1,  2, -1 );
  HorizontalAndVerticalLinesFilter : array[ 0 .. 8 ] of TFilterElement =
    ( -2,  1, -2,  1,  4,  1, -2,  1, -2 );
  SobelFilterX : array[ 0 .. 8 ] of TFilterElement =
    ( -1,  0,  1, -2,  0,  2, -1,  0,  1 );
  SobelFilterY : array[ 0 .. 8 ] of TFilterElement =
    ( -1, -2, -1,  0,  0,  0,  1,  2,  1 );
  // NRIGO filter ( Noise Robust Isotropic Gradient Operators):
  // http://www.holoborodko.com/pavel/image-processing/edge-detection/
  NRIGO75FilterX : array[ 0 .. 34 ] of TFilterElement =
    ( -1,  -4,  -5, 0,  5,  4, 1,
      -4, -16, -20, 0, 20, 16, 4,
      -6, -24, -30, 0, 30, 24, 6,
      -4, -16, -20, 0, 20, 16, 4,
      -1,  -4,  -5, 0,  5,  4, 1 );
  NRIGO75FilterY : array[ 0 .. 34 ] of TFilterElement =
    ( -1,  -4,  -6,  -4, -1,
      -4, -16, -24, -16, -4,
      -5, -20, -30, -20, -5,
       0,   0,   0,   0,  0,
       5,  20,  30,  20,  5,
       4,  16,  24,  16,  4,
       1,   4,   6,   4,  1 );

  TWO_PI : Extended = 2.0 * PI;
var
  W, H{, HighThresholdPct}{, LowThresholdPct}{, LineLengthThreshold} : Integer;
  TimeMS : TTimeMS;
  B : TBitmap;
  GrayPixels, GaussianPixels, DerivativesX, DerivativesY, GradientMagnitudes : PPixels;
  R: TRect;

  procedure CalculateHistogram( Width__, Height__ : Integer; Pixels__ : PPixels; var Histogram__ : THistogram );
  var Col, Row : Integer; Pixel : PPixel;
  begin // precondition: 0 <= image cell values <= 255
    ZeroMemory( Addr( Histogram__ ), SizeOf( Histogram__ ) );
    Pixel := PPixel( Pixels__ );
    for Row := 0 to Pred( Height__ ) do
        for Col := 0 to Pred( Width__ ) do begin
            Inc( Histogram__.Total );
            Inc( Histogram__.Bins[ Pixel^ and 255  ] );
            Inc( Pixel );
            end;
  end;

  function  CalculateHistogramPercentile( Percent__ : Integer ; const Histogram__ :THistogram ) : Integer;
  var Index, Sum : Integer;
  begin
    Result := 0;
    Sum    := 0;
    for Index := Low( Histogram__.Bins ) to High( Histogram__.Bins ) do begin
        Inc( Sum, Histogram__.Bins[ Index ] );
        if ( ( Sum * 100 ) div Histogram__.Total ) >= Percent__ then begin
           Result := Index;
           exit; // quick-and-dirty exit when found
           end;
        end;
  end;

  procedure FindMinimumAndMaximumPixel( Width__, Height__ : Integer; Pixels__ : PPixels; var Minimum__, Maximum__ : Integer );
  var Index : Integer; Value : TPixel; Pixel : PPixel;
  begin
    Minimum__ := High( Minimum__ );
    Maximum__ := Low ( Maximum__ );
    Pixel     := PPixel( Pixels__ );
    for Index := 0 to Pred( Width__ * Height__ ) do begin
        Value := Pixel^;
        if Value < Minimum__ then Minimum__ := Value;
        if Value > Maximum__ then Maximum__ := Value;
        Inc( Pixel );
        end;
  end;

  procedure DisplayHistogram( const Histogram__ : THistogram );
  var Index, Count : Integer;
  begin
    Count := 0;
    for Index := Low( Histogram__.Bins ) to High( Histogram__.Bins ) do begin
        Inc( Count, Histogram__.Bins[ Index ] );
        ToolsForm.Memo1.Lines.Add( IntToStr( Index ) + ': ' + IntToStr( Histogram__.Bins[ Index ] ) + '    ' + IntToStr( ( Count * 100 ) div Histogram__.Total ) + '%' );
        end;
  end;

  function ContrastEnhancement( Width__, Height__ : Integer; Pixels__ : PPixels ) : Boolean;
  var Col, Row, Sum, Index, NewIndex : Integer;
      Pixel : PPixel;
      Histogram, NewColors : THistogram;
  begin // precondition: 0 <= image cell values <= 255
    Result := True;
    CalculateHistogram( Width__, Height__, Pixels__, Histogram );
    Sum := 0;
    for Index := Low( Histogram.Bins ) to High( Histogram.Bins ) do begin
        Inc( Sum, Histogram.Bins[ Index ] );
        NewIndex := Low( Histogram.Bins ) + ( ( ( High( Histogram.Bins ) - Low( Histogram.Bins ) ) * Sum ) div Histogram.Total );
        NewColors.Bins[ Index ] := NewIndex;
        end;

    Pixel := PPixel( Pixels__ );
    for Row := 0 to Pred( Height__ ) do
        for Col := 0 to Pred( Width__ ) do begin
            Pixel^ := NewColors.Bins[ Pixel^ and 255 ];
            Inc( Pixel );
            end;
  end;

  function  NormalizePixels( Width__, Height__ : Integer; Pixels__ : PPixels ) : Boolean;
  var Col, Row, Range, Minimum, Maximum : Integer; Pixel : PPixel;
  begin
    FindMinimumAndMaximumPixel( Width__, Height__, Pixels__, Minimum, Maximum );
    Range := Maximum - Minimum;
    Result := ( ( Minimum <> 0 ) or ( Maximum <> 255 ) ) and ( Range > 0 );
    if Result then begin // 'True': normalize the values to 0..255
       Pixel := PPixel( Pixels__ );
       for Row := 0 to Pred( Height__ ) do
           for Col := 0 to Pred( Width__ ) do begin
               Pixel^ := ( ( Pixel^ - Minimum ) * 255 ) div Range;
               Inc( Pixel );
               end;
       end;
  end;

  function  ColorizePixels( Width__, Height__ : Integer; Pixels__, MaskPixels__ : PPixels ) : Boolean;
  var Col, Row : Integer; Pixel, MaskPixel : PPixel;
  begin // 'ColorizePixels'
        // precondition: the pixels have been normalized to [0..255] values
    Result       := True;
    Pixel        := PPixel( Pixels__ );
    MaskPixel    := PPixel( MaskPixels__ );
    for Row      := 0 to Pred( Height__ ) do
        for Col  := 0 to Pred( Width__ ) do begin
            if   MaskPixel^ =  0 then
                 Pixel^     := 0
            else Pixel^     := TColor( Integer( RGBToColor( HSVToRGB( ( 90.0 + ( ( Pixel^ and 255 ) * 360 ) div 255 ), 1.0, 1.0 ) ) ) + ( 1 shl 24 ) ); // '(1 shl 24)' : flag for not being a black-white pixel value
            Inc( Pixel );
            Inc( MaskPixel );
            end;
  end;

  function  ArcTan2( Y, X : Extended ) : Extended;
  const HALF_PI : Extended = PI / 2.0;
  begin // returns 'arctan' mapped to the interval [0 .. 2 * PI);
        // precondition: -264 < Y, X < 264
    if   X         <> 0.0 then Result :=   Math.ArcTan2( Y, X )
    else if      Y >  0.0 then Result :=   HALF_PI
         else if Y <  0.0 then Result := - HALF_PI
              else             Result :=   0.0; // undefined; just return 0.0
    if   Result    <  0.0 then Result := Result + TWO_PI;
  end;

  function  CopyPixels( Width__, Height__ : Integer; Source__, Destination__ : PPixels ) : Boolean;
  begin
    Result := Assigned( Source__ ) and Assigned( Destination__ );
    if Result then
       Move( Source__^, Destination__^, Width__ * Height__ * SizeOf( TPixel ) );
  end;

  function  PixelsToBitmap( Width__, Height__ : Integer; Pixels__ : PPixels; Bitmap__ : TBitmap; Normalize__ : Boolean; const Caption__ : String) : Boolean;
  var Col, Row : Integer; P :PRGB; Pixel : PPixel;
  begin
    if Normalize__ then
       NormalizePixels( Width__, Height__, Pixels__ );
    with Bitmap__ do
      try    Width := Width__;
             Height := Height__;
             PixelFormat := pf24Bit;
             Pixel := PPixel( Pixels__ );
             for Row := 0 to Pred( Height ) do begin
                 P := ScanLine[ Row ];
                 for Col := 0 to Pred( Width ) do begin
                     if Pixel^ <= 255 then begin
                        P^.r := Pixel^;
                        P^.g := P^.r;
                        P^.b := P^.r;
                        end
                     else begin
                        P^.r :=   Pixel^          and 255;
                        P^.g := ( Pixel^ shr  8 ) and 255;
                        P^.b := ( Pixel^ shr 16 ) and 255;
                       end;
                     Inc( P );
                     Inc( Pixel );
                     end;
                 end;
             if Caption__ <> '' then with Canvas do begin
                Font.Height                   := Max( 8, Min( 20, ScaledSize( ToolsForm.RowHeightSpinEdit.Value ) div 2 ) );
                //BitmapAlphaBlendColor( Bitmap__, clBlack, 50, Rect ( 2, 3, Width - 2, 10 + Font.Height ) );
                Windows.SetBkMode ( Handle, Windows.TRANSPARENT );
                Font.Color                    := clBlack;
                TextOut( 5, 5, Caption__ );
                Font.Color                    := clLtGray;
                TextOut( 4, 4, Caption__ );
                Windows.SetBkMode ( Canvas.Handle, Windows.OPAQUE );
                end;
             Result := True;
    except on E:Exception do Result := False;
    end;
  end;

  function  ApplyThreshold( Width__, Height__ : Integer; Input__, Output__ : PPixels; Threshold__ : Integer; IsAPercentThreshold__, MakeBlackWhiteImage__ : Boolean ) : Integer;
  var Col, Row : Integer; Input, Output : PPixel; Histogram : THistogram;
  begin
    if IsAPercentThreshold__ then begin
       CalculateHistogram( Width__, Height__, Input__, Histogram );
       Threshold__ := CalculateHistogramPercentile( 100 - Threshold__, Histogram );
       end;

    Input := PPixel( Input__ );
    Output := PPixel( Output__ );
    for Row := 0 to Pred( Height__ ) do
        for Col := 0 to Pred( Width__ ) do begin
            if         Input^  <  Threshold__ then
                       Output^ := 0
            else  if   MakeBlackWhiteImage__ then
                       Output^ := 255
                  else OutPut^ := Input^;
            Inc( Input );
            Inc( Output );
            end;

    Result := Threshold__;
  end;

  function  NonMaximalSuppression( Width__, Height__ : Integer; GradientMagnitudes__, GradientDirections__, Pixels__ : PPixels ) : Boolean;
  var Col, Row, GradientValue : Integer; Gradient, Pixel, Direction : PPixel;
  begin
    Result    := True;
    Gradient  := PPixel( GradientMagnitudes__ );
    Direction := PPixel( GradientDirections__ );
    Pixel     := PPixel( Pixels__ ); // output
    for Row := 0 to Pred( Height__ - 2 ) do begin
        for Col := 0 to Pred( Width__ - 2 ) do begin
            GradientValue := Gradient^;
            if ( Row > 0 ) and
               ( Col > 0 )
               then begin
               if GradientValue <> 0 then
                  // compare the gradient magnitude with the magnitudes of the
                  // 2 neighboring pixels in the gradient direction
                  case Direction^ div 16 of
                    0, 7,  8, 15 : // east, west
                                   if   ( GradientValue >= GradientMagnitudes__^[ Succ( Col ) +       Row   * Width__ ] )
                                        and
                                        ( GradientValue >= GradientMagnitudes__^[ Pred( Col ) +       Row   * Width__ ] )
                                        then
                                        Pixel^ := GradientValue
                                   else Pixel^ := 0;
                    1, 2,  9, 10 : // north-east, south-west
                                   if   ( GradientValue >= GradientMagnitudes__^[ Succ( Col ) + Pred( Row ) * Width__ ] )
                                        and
                                        ( GradientValue >= GradientMagnitudes__^[ Pred( Col ) + Succ( Row ) * Width__ ] )
                                        then
                                        Pixel^ := GradientValue
                                   else Pixel^ := 0;
                    3, 4, 11, 12 : // north, south
                                   if   ( GradientValue >= GradientMagnitudes__^[ Col         + Pred( Row ) * Width__ ] )
                                        and
                                        ( GradientValue >= GradientMagnitudes__^[ Col         + Succ( Row ) * Width__ ] )
                                        then
                                        Pixel^ := GradientValue
                                   else Pixel^ := 0;
                    5, 6, 13, 14 : // north-west, south-east
                                   if   ( GradientValue >= GradientMagnitudes__^[ Pred( Col ) + Pred( Row ) * Width__ ] )
                                        and
                                        ( GradientValue >= GradientMagnitudes__^[ Succ( Col ) + Succ( Row ) * Width__ ] )
                                        then
                                        Pixel^ := GradientValue
                                   else Pixel^ := 0;
                  end
               else Pixel^ := GradientValue;
               end
            else Pixel^ := Gradient^;
            Inc( Gradient );
            Inc( Direction );
            Inc( Pixel );
            end;
        Inc( Gradient , 2 );
        Inc( Direction, 2 );
        Inc( Pixel    , 2 );
        end;
  end;

  procedure ApplyHysteresisThreshold( Width__, Height__ : Integer; Pixels__, GradientDirections__ : PPixels; HighThresholdPct__, LowThresholdPct__ : Integer );
  var LowThreshold, HighThreshold, Col, Row,PixelValue : Integer;
      Again : Boolean;
      Pixel, Direction : PPixel; Histogram : THistogram;
  begin // ApplyHysteresisThreshold
    CalculateHistogram( Width__, Height__, Pixels__, Histogram );
    //Dec( Histogram.Total, Histogram.Bins[ 0 ] ); // don't consider the black pixels
    //Histogram.Bins[ 0 ] := 0;
    HighThreshold := CalculateHistogramPercentile( 100 - HighThresholdPct__, Histogram );
    LowThreshold  := CalculateHistogramPercentile( 100 - LowThresholdPct__ , Histogram );

//  if ( LowThreshold <> 0 ) and ( HighThreshold <> 0 ) then
       repeat
         Again := False;
         Pixel := PPixel( Pixels__ );
         Direction := PPixel( GradientDirections__ );
         for Row := 0 to Pred( Height__ )  do
             for Col := 0 to Pred( Width__ ) do begin
                 PixelValue := Pixel^;
                 if PixelValue <> 0 then
                    if      PixelValue <  LowThreshold then
                            Pixel^     := 0
                    else if PixelValue >= HighThreshold then
                            Pixel^     := 255
                         else begin
                           Pixel^      := 0;
                           // check whether the candidate pixel has a
                           // neighbor pixel in the edge direction with a
                           // value above the high threshold;
                           case Direction^ div 16 of
                             0, 7, 8, 15  : // east, west gradient => north, south edge
                                            begin if   ( Row > 0 )
                                                       and
                                                       ( HighThreshold <= Pixels__^[ Col         + Pred( Row ) * Width__ ] )
                                                       then begin
                                                       Pixel^ := 255;
                                                       Again  := True;
                                                       end;
                                                  if   ( Row < Pred( Height__ ) )
                                                       and
                                                       ( HighThreshold <= Pixels__^[ Col         + Succ( Row ) * Width__ ] )
                                                       then begin
                                                       Pixel^ := 255;
                                                       Again  := True;
                                                       end;
                                            end;
                             1, 2, 9, 10  : // north-east, south-west gradient => north-west, south-east edge
                                            begin if   ( Col > 0 ) and ( Row > 0 )
                                                       and
                                                       ( HighThreshold <=  Pixels__^[ Pred( Col ) + Pred( Row ) * Width__ ] )
                                                       then begin
                                                       Pixel^ := 255;
                                                       Again  := True;
                                                       end;
                                                  if   ( Col < Pred( Width__ ) ) and ( Row < Pred( Height__ ) )
                                                       and
                                                       ( HighThreshold <=  Pixels__^[ Succ( Col ) + Succ( Row ) * Width__ ] )
                                                       then begin
                                                       Pixel^ := 255;
                                                       Again  := True;
                                                       end;
                                            end;
                             3, 4, 11, 12 : // north, south gradient => east, west edge
                                            begin if   ( Col < Pred( Width__ ) )
                                                       and
                                                       ( HighThreshold <=  Pixels__^[ Succ( Col ) +       Row   * Width__ ] )
                                                       then begin
                                                       Pixel^ := 255;
                                                       Again  := True;
                                                       end;
                                                  if   ( Col > 0 )
                                                       and
                                                       ( HighThreshold <= Pixels__^[ Pred( Col ) +        Row   * Width__ ] )
                                                       then begin
                                                       Pixel^ := 255;
                                                       Again  := True;
                                                       end;
                                            end;
                             5, 6, 13, 14 : // north-west, south-east gradient => south-west, north-east edge
                                            begin if   ( Col > 0 ) and ( Row < Pred( Height__ ) )
                                                       and
                                                       ( HighThreshold <=  Pixels__^[ Pred( Col ) + Succ( Row ) * Width__ ] )
                                                       then begin
                                                       Pixel^ := 255;
                                                       Again  := True;
                                                       end;
                                                  if   ( Col < Pred( Width__ ) ) and ( Row > 0 )
                                                       and
                                                       ( HighThreshold <=  Pixels__^[ Succ( Col ) + Pred( Row ) * Width__ ] )
                                                       then begin
                                                       Pixel^ := 255;
                                                       Again  := True;
                                                       end;
                                            end;
                           end;
                           end;
                 Inc( Pixel );
                 Inc( Direction );
                 end;
       until not Again;
  end;

  function  ApplyFilter( Width__, Height__             : Integer;
                         Input__                       : PPixels;
                         FilterWidth__, FilterHeight__ : Integer;
                         Filter__                      : PFilter;
                         Output__                      : PPixels ) : Boolean;
  var FW, FH, DX, DY, Col, Row, Sum, FilterSum, Index : Integer;
      FilterElement : PFilterElement; FilterSumInverse : Double;
      Pixel : PPixel;
  begin // precondition: the bitmap is a grayscale bitmap
    Result := True;
    if Result then begin
       //ZeroMemory( Addr( Output__^ ), Width__ * Height__ * SizeOf( Output__^[ 0 ] ) );
       FW := FilterWidth__  div 2; // filter width  = 2*FW + 1
       FH := FilterHeight__ div 2; // filter height = 2*FH + 1
       FilterSum := 0;
       for  Index := 0 to Pred( FilterWidth__ * FilterHeight__ ) do
            Inc( FilterSum, Filter__^[ Index ] );
       if   FilterSum = 0 then
            FilterSumInverse := 0.0
       else FilterSumInverse := 1.0 / FilterSum;

       Pixel := PPixel( Output__ );
       for Row := 0 to Pred( FH ) do begin // top rows
           for Col := 0 to Pred( Width__ ) do begin
               Pixel^ := Input__^[ Col + ( Row * Width__ ) ];
               Inc( Pixel );
               end;
           end;
       for Row := FH to Pred( Height__ - FH ) do begin // filtered rows
           for Col := 0 to Pred( FW ) do begin // left columns
               Pixel^ := Input__^[ Col + ( Row * Width__ ) ];
               Inc( Pixel );
               end;
           for Col := FW to Pred( Width__ - FW ) do begin // filtered columns
               Sum := 0;
               FilterElement := PFilterElement( Filter__ );
               for DY := -FH to FH do
                   for DX := - FW to FW do begin
                       Inc( Sum, FilterElement^ * Input__^[ ( Col + DX ) + ( Width__ * ( Row + DY ) ) ] );
                       Inc( FilterElement );
                       end;
               if FilterSum <> 0 then
                  Sum := Round( Sum * FilterSumInverse );
               Pixel^ := Sum;
               Inc( Pixel );
               end;
           for Col := Width__ - FW to Pred( Width__ ) do begin // right columns
               Pixel^ := Input__^[ Col + ( Row * Width__ ) ];
               Inc( Pixel );
               end;
           end;
       for Row := Height__ - FH to Pred( Height__ ) do // bottom rows
           for Col := 0 to Pred( Width__ ) do begin
               Pixel^ := Input__^[ Col + ( Row * Width__ ) ];
               Inc( Pixel );
               end;
       if Pixel <> PPixel( Cardinal( Output__ ) + Cardinal( Width__ * Height__ * SizeOf( TPixel ) ) ) then
          Msg( 'Internal error', 'Apply filter', MB_OK )
       end;
  end;

  function  CalculateGradients( Width__, Height__              : Integer;
                                DerivativesX__, DerivativesY__ : PPixels;
                                GradientMagnitudes__           : PPixels;
                                GradientDirections__           : PPixels ) : Boolean;
  var DX, DY, Col, Row, Index, Magnitude : Integer;
      Angle : Double;
  begin
    Result := True;
    for Row := 0 to Pred( Height__ ) do
        for Col := 0 to Pred( Width__ ) do begin
            Index                          := Col + ( Width__ * Row );
            DX                             := DerivativesX__^[ Index ];
            DY                             := DerivativesY__^[ Index ];
            Magnitude                      := Trunc( Sqrt( ( DX * DX ) + ( DY * DY ) ) );
            if Magnitude                   >  255 then // for general image processing, it's not correct to clamp the magnitude to 0..255 this way, but for this special purpose, it seems to work satisfactory
               Magnitude                   := 255;
            GradientMagnitudes__^[ Index ] := Magnitude;
            Angle                          := ArcTan2( -DY, DX ); // '-DY' : in the computer image coordinate system, the Y axis grows from top to bottom
            GradientDirections__^[ Index ] := Round ( ( 255.0 * Angle ) / TWO_PI );
            end;
    NormalizePixels( Width__, Height__, GradientMagnitudes__ ); // not strictly necessary when the magnitudes are clamped to 0..255 above
  end;

  function  ApplyStraightLineThreshold( Width__, Height__ : Integer; Pixels__, HorzLines__, VertLines__ : PPixels; Length__, CornerDistance__, SmallLines__ : Integer ) : Boolean;
  var Col, Row, ThisCol, ThisRow, Index, ThisIndex, HCount, VCount, HIndex, VIndex, HLength,VLength, X, Y, DX, DY : Integer;
           Pixel, VLine, HLine, This : PPixel;
  begin // finds horizontal and vertical lines in the image after it has been transformed to a black-and-white-only image;
    Result  := True;
    ZeroMemory( Addr( HorzLines__^ ), Width__ * Height__ * SizeOf( HorzLines__^[ 0 ] ) );
    ZeroMemory( Addr( VertLines__^ ), Width__ * Height__ * SizeOf( VertLines__^[ 0 ] ) );
    Pixel   := PPixel( Pixels__ );
    HLine   := PPixel( HorzLines__ );
    VLine   := PPixel( VertLines__ );
    Index   := 0;
    for Row := 0 to Pred( Height__ - 2 )  do begin
        for Col := 0 to Pred( Width__ - 2 ) do begin
            if Pixel^ <> 0 then begin
               if HLine^    =  0 then begin
                  ThisIndex := Index;
                  This      := Pixel;
                  ThisCol   := Col;
                  repeat HorzLines__^[ ThisIndex  ] := Index; // all but the first pixel member of the line points back to the beginning of the line
                         Inc( ThisIndex );
                         Inc( ThisCol );
                         Inc( This );
                  until  ( ThisCol >= Width__ - 2 ) or ( This^ = 0 );
                  HLine^ := -( ThisCol - Col );               // the first pixel of the line contains the negated line length
                  end;
               if VLine^    =  0 then begin
                  ThisIndex := Index;
                  This      := Pixel;
                  ThisRow   := Row;
                  repeat VertLines__^[ ThisIndex  ] := Index; // all but the first pixel member of the line points back to the beginning of the line
                         Inc( ThisIndex, Width__ );
                         Inc( ThisRow );
                         Inc( This, Width__ );
                  until  ( ThisRow >= Height__ - 2 ) or ( This^ = 0 );
                  VLine^ := -( ThisRow - Row );               // the first pixel of the line contains the negated line length
                  end;
               end;
            Inc( Pixel );
            Inc( HLine );
            Inc( VLine );
            Inc( Index );
            end;
        Inc( Pixel, 2 );
        Inc( HLine, 2 );
        Inc( VLine, 2 );
        Inc( Index, 2 );
        end;

    if Length__ > 0 then begin
       Index := 0;
       for Row := 0 to Pred( Height__ - 2 )  do begin
           for Col := 0 to Pred( Width__ - 2 ) do begin
               HIndex               := HorzLines__^[ Index ];
               if   HIndex          <  0 then // 'True': this pixel is the start of the line
                    HIndex          := Index;
               HLength              := -HorzLines__^[ HIndex ];
               if   ( HLength       >  0 ) and ( HLength <  Length__ ) then
                    for ThisIndex   := Index to Index + Pred( HLength ) do begin
                        HorzLines__^[ ThisIndex ] := 0;

                        VIndex      := VertLines__^[ ThisIndex ];
                        if VIndex   < 0 then
                           VIndex   :=  ThisIndex;
                        VLength     := -VertLines__^[ VIndex ];
                        if VLength  <= Length__ then // 'True': neither the horizontal line nor the vertical line is long enough to protect the pixel; remove it;
                           Pixels__^[ ThisIndex ] := 0;
                        end;

               VIndex               := VertLines__^[ Index ];
               if   VIndex          <  0 then // 'True': this pixel is the start of the line
                    VIndex          := Index;
               VLength              := -VertLines__^[ VIndex ];
               if   ( VLength       >  0 ) and ( VLength <  Length__ ) then
                    for ThisIndex   := 0 to Pred( VLength ) do begin
                        VertLines__^[ Index + ( ThisIndex * Width__ ) ] := 0;

                        HIndex      := HorzLines__^[ Index + ( ThisIndex * Width__ ) ];
                        if Hindex   <  0 then
                           HIndex   := Index + ( ThisIndex * Width__ );
                        HLength     := -HorzLines__^[ HIndex ];
                        if HLength  <= Length__ then // 'True': neither the horizontal line nor the vertical line is long enough to protect the pixel; remove it;
                           Pixels__^[ Index + ( ThisIndex * Width__ ) ] := 0;
                        end;
               Inc( Index );
               end;
           Inc( Index, 2 );
           end;
       end;

    if SmallLines__ > Length__ then begin
       Index := 0;
       for Row := 0 to Pred( Height__ - 2 )  do begin
           for Col := 0 to Pred( Width__ - 2 ) do begin
               HIndex               := HorzLines__^[ Index ];
               if   HIndex          <  0 then // 'True': this pixel is the start of the line
                    HIndex          := Index;
               HLength              := -HorzLines__^[ HIndex ];
               if   ( HLength       >  0 ) and ( HLength <= SmallLines__ ) then begin
                    VIndex          := 0;
                    for HIndex      := 0 to Pred( Width__ - 2 ) do
                        if ( HorzLines__^[ HIndex + ( Row * Width__ ) ] = - HLength ) and ( HIndex <> Col ) then begin
                           VIndex   := 1; break;
                           end;
                    if VIndex       =  0 then
                       for HIndex   := 0 to Pred( HLength ) do begin
                           HorzLines__^[ Index + HIndex ] := 0;
                           Pixels__   ^[ Index + HIndex ] := 0;
                           end;
                    end;

               VIndex               := VertLines__^[ Index ];
               if   VIndex          <  0 then // 'True': this pixel is the start of the line
                    VIndex          := Index;
               VLength              := -VertLines__^[ VIndex ];
               if   ( VLength       >  0 ) and ( VLength <=  SmallLines__ ) then begin
                    HIndex          := 0;
                    for VIndex      := 0 to Pred( Height__ - 2 ) do
                        if ( VertLines__^[ Col + ( VIndex * Width__ ) ] = - VLength ) and ( VIndex <> Row ) then begin
                           HIndex   := 1; break;
                           end;
                    if HIndex       =  0 then
                       for VIndex   := 0 to Pred( VLength ) do begin
                           VertLines__^[ Index + ( VIndex * Width__ ) ] := 0;
                           Pixels__   ^[ Index + ( VIndex * Width__ ) ] := 0;
                           end;
                    end;
               Inc( Index );
               end;
           Inc( Index, 2 );
           end;
       end;

    if CornerDistance__ > 0 then begin
       Index := 0;
       for Row := 0 to Pred( Height__ - 2 )  do begin
           for Col := 0 to Pred( Width__ - 2 ) do begin
               HIndex     :=  HorzLines__^[ Index ];
               if HIndex  <  0 then
                  HIndex  := Index;
               HLength    := -HorzLines__^[ HIndex ];
               VIndex     :=  VertLines__^[  Index ];
               if VIndex  <  0 then
                  VIndex  := Index;
               VLength    := -VertLines__^[ VIndex ];

               if ( HLength + VLength ) <> 0 then // 'True': the pixel is a member of a line
                  if ( Index =  HIndex ) or // start of line
                     ( Index =  VIndex ) or // start of line
                     ( Index =  Pred( HIndex + HLength ) ) or // end of line
                     ( Index =  VIndex + ( Pred( VLength ) * Width__ ) ) // end of line
                     then begin
                     HCount := 0;
                     VCount := 0;
                     for DY := - CornerDistance__ to CornerDistance__ do begin
                         Y  := Row + DY;
                         if ( Y >= 0 ) and ( Y < Height__ - 2 ) then begin
                            for DX := -CornerDistance__ to CornerDistance__ do begin
                                X := Col + DX;
                                if ( X >= 0 ) and ( X < Width__ - 2 ) then begin
                                   ThisIndex := X + ( Y * Width__ );
                                   Inc( HCount, Abs( HorzLines__^[ ThisIndex ] ) ); // 'Abs': if non-zero, there is a line there
                                   Inc( VCount, Abs( VertLines__^[ ThisIndex ] ) ); // 'Abs': if non-zero, there is a line there
                                   end;
                                end;
                            end;
                         end;

                     if ( VCount = 0 ) and ( HLength > 0 ) then begin
                        for DX := 0 to Pred( HLength ) do begin
                            HorzLines__^[ HIndex + DX ] := 0;
                            Pixels__   ^[ HIndex + DX ] := 0;
                            end;
                        end;

                     if ( HCount = 0 ) and ( VLength > 0 ) then begin
                        for DY := 0 to Pred( VLength ) do begin
                            VertLines__^[ VIndex + ( DY * Width__ ) ] := 0;
                            Pixels__   ^[ VIndex + ( DY * Width__ ) ] := 0;
                            end;
                        end;
                     end;

               Inc( Index );
               end;
           Inc( Index, 2 );
           end;
       end;
  end;

begin // AnalyseImage
  TimeMS := GetTickCount;
  FreeMem( HorzLines__ ); FreeMem( VertLines__ );
  FreeMem( IntegralImage__ ); FreeMem( IntegralImage2__ ); FreeMem( GradientDirections__ );
  HorzLines__ := nil; VertLines__ := nil;
  IntegralImage__ := nil; IntegralImage2__ := nil; GradientDirections__ := nil;
  try
    B := nil;
    GrayPixels := nil; GaussianPixels := nil; DerivativesX := nil; DerivativesY := nil; GradientMagnitudes := nil; W := 0; H := 0;
    //HighThresholdPct := 12; //LowThresholdPct := 15;
    //LineLengthThreshold := 6;
    try
      Screen.Cursor := crHourGlass;
      ToolsForm.StatusBar1.Panels[ 1 ].Text := TEXT_ANALYSING_IMAGE_PLEASE_WAIT;

      //if CreatePixels( MAX_IMAGE_WIDTH + 2 , MAX_IMAGE_HEIGHT + 2, PPixels( IntegralImage__ ) ) then begin
      //   for W := 0 to Pred( ( MAX_IMAGE_WIDTH + 2 ) * ( MAX_IMAGE_HEIGHT + 2 ) ) do IntegralImage__^[ W ] := 255;
      //   Memo1.Lines.Add( IntToStr( CalculateIntegralImage( MAX_IMAGE_WIDTH, MAX_IMAGE_HEIGHT, PPixels( IntegralImage__ ), IntegralImage__ ) ) );
      //   end;

      W := Bitmap__.Width; H:= Bitmap__.Height;
      if W >  MAX_IMAGE_WIDTH + 2 then begin
         H := MulDiv( H, MAX_IMAGE_WIDTH + 2, W );
         W := MAX_IMAGE_WIDTH + 2;
         end;
      if H >  MAX_IMAGE_HEIGHT + 2 then begin
         W := MulDiv( W, MAX_IMAGE_HEIGHT + 2, H );
         H := MAX_IMAGE_HEIGHT + 2 ;
         end;

      Result := CreatePixels( W, H, DerivativesX ) and // create derivatives first because they are reused for storing the straight lines in the image and returned to the caller
                CreatePixels( W, H, DerivativesY ) and
                CreatePixels( W, H, GaussianPixels ) and
                CreatePixels( W, H, GrayPixels ) and
                CreatePixels( W, H, GradientMagnitudes ) and
                CreatePixels( W, H, GradientDirections__ ) and
                BitmapCreate( B, W, H ) and
                ( ( not ( airIntegralImage2 in Results__ ) )
                  or
                  CreatePixels( W, H, IntegralImage2__ )
                );
      if Result then with B do with Canvas do begin
         R := Rect( 0, 0, Width, Height );
         StretchDraw( R, Bitmap__ );

         if airIntegralImage2 in Results__ then begin
            BitmapToGrayscalePixels( B, GrayPixels, High( RGB_BLACK.r ) - ( ( 1 shl DISCARD_COLOR_COMPONENT_LOW_BITS_COUNT ) - 1 ) );
            if CalculateIntegralImage( W, H, GrayPixels, IntegralImage2__ ) <= 0 then begin
               FreeMem( IntegralImage2__ ); IntegralImage2__ := nil;
               end;
            end;

         BitmapToGrayscalePixels( B, GrayPixels, 0 );
         ToolsForm.CaptureImage2.Picture.Bitmap.Assign( B );
         CopyPixels( W, H, GrayPixels, GaussianPixels );
         Result := Result
                   and
                   //ContrastEnhancement( W, H, GrayPixels )
                   //and
                   ApplyFilter( W, H, GrayPixels,     5, 5, PFilter( Addr( GaussianBlurFilter ) ), GaussianPixels )
                   and
                   ApplyFilter( W, H, GaussianPixels, 3, 3, PFilter( Addr( SobelFilterX       ) ), DerivativesX   )
                   and
                   ApplyFilter( W, H, GaussianPixels, 3, 3, PFilter( Addr( SobelFilterY       ) ), DerivativesY   )
                   and
                   CalculateGradients( W, H, DerivativesX, DerivativesY, GradientMagnitudes, GradientDirections__ );

         if Result then begin
            if ToolsForm.CaptureSettingsMenuItemGrayscale           .Checked then Result := PixelsToBitmap( W, H, GrayPixels           , B, False, ToolsForm.CaptureSettingsMenuItemGrayscale           .Caption );
            if ToolsForm.CaptureSettingsMenuItemGaussianBlur        .Checked then Result := PixelsToBitmap( W, H, GaussianPixels       , B, False, ToolsForm.CaptureSettingsMenuItemGaussianBlur        .Caption );

            NonMaximalSuppression   ( W, H, GradientMagnitudes, GradientDirections__, GaussianPixels );
            ApplyHysteresisThreshold( W, H, GaussianPixels    , GradientDirections__, 10, 20 );

            if ToolsForm.CaptureSettingsMenuItemEdgeThinning        .Checked then Result := PixelsToBitmap( W, H, GaussianPixels       , B, False, ToolsForm.CaptureSettingsMenuItemEdgeThinning        .Caption );
            if ToolsForm.CaptureSettingsMenuItemXGradients          .Checked then Result := PixelsToBitmap( W, H, DerivativesX         , B, True , ToolsForm.CaptureSettingsMenuItemXGradients          .Caption );
            if ToolsForm.CaptureSettingsMenuItemYGradients          .Checked then Result := PixelsToBitmap( W, H, DerivativesY         , B, True , ToolsForm.CaptureSettingsMenuItemYGradients          .Caption );

            if ( ( Results__ * [ airHorzLines, airVertLines ] ) <> [] ) or
               ToolsForm.CaptureSettingsMenuItemStraightLines.Checked then begin
               ApplyStraightLineThreshold( W, H, GaussianPixels, DerivativesX, DerivativesY, 8, 8, 12 ); // 'DerivativesX' and 'DerivativesY' are reused for storing the straight lines; they are returned to the caller;
               CountStraightLinePixels   ( W, H, DerivativesX  , DerivativesY, Rect( 0, 0, W - 2, H - 2 ) );
               end;

            if ToolsForm.CaptureSettingsMenuItemGradientMagnitudes  .Checked then Result := PixelsToBitmap( W, H, GradientMagnitudes   , B, True , ToolsForm.CaptureSettingsMenuItemGradientMagnitudes  .Caption );
            if ToolsForm.CaptureSettingsMenuItemGradientOrientations.Checked then Result := CopyPixels    ( W, H, GradientDirections__ , GaussianPixels ) and
                                                                                            ColorizePixels( W, H, GaussianPixels       , GradientMagnitudes ) and
                                                                                            PixelsToBitmap( W, H, GaussianPixels       , B, False, ToolsForm.CaptureSettingsMenuItemGradientOrientations.Caption );
            if ToolsForm.CaptureSettingsMenuItemStraightLines       .Checked then Result := PixelsToBitmap( W, H, GaussianPixels       , B, False, ToolsForm.CaptureSettingsMenuItemStraightLines       .Caption );

            Result := Result
                      and
                      ( ( not ( airIntegralImage in Results__ ) )
                        or
                        ( CalculateIntegralImage( W, H, GrayPixels, GaussianPixels ) > 0 ) ); // use 'GaussianPixels' for the integral image

            //B.Width := B.Width - 2;
            //B.Height := B.Height - 2;
            //B.SaveToFile( 't10.bmp' );

            if Result then begin
               ToolsForm.CaptureImage2.Picture.Bitmap.Assign( B );
               ToolsForm.CaptureImage2.Visible := ToolsForm.CaptureViewMenuItemShowImageAnalysisResults.Checked;
               UpdateScrollBoxRange;
               end;
            end;
         end;
    finally Screen.Cursor := crDefault;
            ToolsForm.StatusBar1.Panels[ 0 ].Text := '';
            ToolsForm.StatusBar1.Panels[ 1 ].Text := '';
            B.Free;
            FreeMem( GradientMagnitudes );
            FreeMem( GrayPixels );
            HorzLines__ := DerivativesX; VertLines__ := DerivativesY; IntegralImage__ := GaussianPixels; // return the calculated data to the caller
            Width__ := W; Height__ := H;
            if not ( airHorzLines          in Results__ ) then begin FreeMem( HorzLines__          ); HorzLines__          := nil; end;
            if not ( airVertLines          in Results__ ) then begin FreeMem( VertLines__          ); VertLines__          := nil; end;
            if not ( airIntegralImage      in Results__ ) then begin FreeMem( IntegralImage__      ); IntegralImage__      := nil; end;
            if not ( airIntegralImage2     in Results__ ) then begin FreeMem( IntegralImage2__     ); IntegralImage2__     := nil; end;
            if not ( airGradientDirections in Results__ ) then begin FreeMem( GradientDirections__ ); GradientDirections__ := nil; end;

    end;
    TimeMS := CalculateElapsedTimeMS( TimeMS, GetTickCount );
    if TimeMS > 10000 then begin
       //Msg( 'Image analysis time: ' + IntToStr( TimeMS ), '', MB_OK+MB_ICONINFORMATION );
       end;
  except on E:Exception do Result := Error( E.Message, '' );
  end;
end;

procedure TCaptureForm.CountStraightLinePixels( Width__, Height__ : Integer; HorzLines__, VertLines__ : PPixels; const Rect__ : TRect );
var Col, Row, Index, HIndex, VIndex, HLength,VLength : Integer;
    VLine, HLine : PPixel;

  function Intersect( Start__, End__, Min__, Max__ : Integer ) : Integer;
  begin
    if   ( End__   >  Min__ ) and
         ( Start__ <  Max__ ) then
         Result    := Min( End__, Max__ ) - Max( Start__, Min__ )
    else Result    := 0;
  end;

begin // CountStraightLinePixels
  for Index := Pred( Height__ ) * Width__ to Pred( Height__ * Width__ ) do
      VertLines__^[ Index ] := 0; // last row in each column := 0
  HLine   := PPixel( HorzLines__ );
  VLine   := PPixel( VertLines__ );
  for Row := 0 to Pred( Height__ - 2 )  do begin
      HIndex := Pred( Width__  ) + ( Row              * Width__ ); // last column in row
      VIndex :=                      Pred( Height__ ) * Width__;   // last row    in column
      HorzLines__^[ HIndex ] := 0;
      for Col := 0 to Pred( Width__ - 2 ) do begin
          HLength := -HLine^;
          if HLength > 0 then
             Inc( HorzLines__^[ HIndex ], Intersect( Col, Col + HLength, Rect__.Left, Rect__.Right  ) );
          VLength := -VLine^;
          if VLength > 0 then
             Inc( VertLines__^[ VIndex ], Intersect( Row, Row + VLength, Rect__.Top , Rect__.Bottom ) );
          Inc( HLine  );
          Inc( VLine  );
          Inc( VIndex );
          end;
      Inc( HLine,  2 );
      Inc( VLine,  2 );
      end;
  //DisplayPixels( 'Horz Lines', Width__, Height__, HorzLines__, 1.0 );
  //DisplayPixels( 'Vert Lines', Width__, Height__, VertLines__, 1.0 );
  //Clipboard.AsText := Memo1.Lines.Text;
end;

function  TCaptureForm.FindBoard( Bitmap__ : TBitmap;
                                  BitmapRect__ : TRect; // the area to investigate
                                  Width__, Height__ : Integer; // dimensions of the possibly scaled horizontal and vertical lines "image" and the integral image
                                  HorzLines__, VertLines__, IntegralImage__, IntegralImage2__ : PPixels;
                                  TimeLimitMS__ : TTimeMS;
                                  var BoardRect__ : TRect; // output
                                  var ColCount__, RowCount__ : Integer // output
                                ) : Boolean;
var
  ColWidth, RowHeight : Integer;
  TimeMS : TTimeMS;

  function  AnalyseLines( const Rect__ : TRect ) : Boolean;
  const MAX_GRID_SIZE = 255;

  var HGrid, VGrid : PPixels;

    function TimeCheck( StartTimeMS__, TimeLimitMS__ : TTimeMS ) : Boolean;
    begin // returns 'True' if there is more time
      Result := CalculateElapsedTimeMS( StartTimeMS__, GetTickCount ) < TimeLimitMS__;
    end;

    function  AnalyseHorzLines( Width__, Height__ : Integer; HorzLines__ : PPixels; const Rect__ : TRect; var HGrid__ : PPixels ) : Boolean;
    var Count, Count2, Row, Row2, Row3, DY : Integer;
    begin
      Result := CreatePixels( MAX_GRID_SIZE, Height__, HGrid__ );
      if Result then begin
         ZeroMemory( Addr( HGrid__^ ), MAX_GRID_SIZE * Height__ * SizeOf( HGrid__^[ 0 ] ) );
         for Row := Rect__.Top to Pred( Rect__.Bottom ) do begin
             Count := HorzLines__^[ Pred( Width__ ) + ( Row * Width__ ) ];
             if Count <> 0 then begin
                for Row2 := Row + 8 to Pred( Rect__.Bottom ) do begin
                    Count2 := HorzLines__^[ Pred( Width__ ) + ( Row2 * Width__ ) ];
                    if Count2 <> 0 then begin
                       DY := Row2 - Row;
                       if DY < MAX_GRID_SIZE then
                          for Row3 := Rect__.Top to Pred( Rect__.Bottom ) do begin
                              if ( ( Abs( Row3 - Row ) mod DY ) = 0 )
                                 and
                                 ( HorzLines__^[ Pred( Width__ ) + ( Row3 * Width__ ) ] <> 0 )
                                 and
                                 ( Row <> Row3 ) then begin
                                 Inc( HGrid__^[ DY + ( Row3 * MAX_GRID_SIZE ) ], Count );
                                 end;
                              end;
                       end;
                    end;
                end;
             end;
         end;
    end;

    function  AnalyseVertLines( Width__, Height__ : Integer; HorzLines__ : PPixels; const Rect__ : TRect; var VGrid__ : PPixels ) : Boolean;
    var Count, Count2, Col, Col2, Col3, DX : Integer;
    begin
      Result := CreatePixels( Width__, MAX_GRID_SIZE, VGrid__ );
      if Result then begin
         ZeroMemory( Addr( VGrid__^ ), Width__ * MAX_GRID_SIZE * SizeOf( VGrid__^[ 0 ] ) );
         for Col := Rect__.Left to Pred( Rect__.Right ) do begin
             Count := VertLines__^[ Col + ( Pred( Height__ ) * Width__ ) ];
             if Count <> 0 then begin
                for Col2 := Col + 8 to Pred( Rect__.Right ) do begin
                    Count2 := VertLines__^[ Col2 + ( Pred( Height__ ) * Width__ ) ];
                    if Count2 <> 0 then begin
                       DX := Col2 - Col;
                       if DX < MAX_GRID_SIZE then
                          for Col3 := Rect__.Left to Pred( Rect__.Right ) do begin
                              if ( ( Abs( Col3 - Col ) mod DX ) = 0 )
                                 and
                                 ( VertLines__^[ Col3 + ( Pred( Height__ ) * Width__ ) ] <> 0 )
                                 and
                                 ( Col <> Col3 ) then begin
                                 Inc( VGrid__^[ Col3 + ( DX * MAX_GRID_SIZE ) ], Count );
                                 end;
                              end;
                       end;
                    end;
                end;
             end;
         end;
    end;

    function  FindColumnWidthAndRowHeight( Width__, Height__ : Integer; HGrid__, VGrid__ : PPixels; const Rect__ : TRect; var ColWidth__, RowHeight__ : Integer ) : Boolean;

      function  StraightLinePixelsInColumn( Col__ : Integer ) : Integer;
      begin
        Result := VertLines__^[ Col__ + ( Pred( Height__ ) * Width__ ) ];
        //if Result <> 0 then Result := 1;
      end;

      function  StraightLinePixelsInRow( Row__ : Integer ) : Integer;
      begin
        Result := HorzLines__^[ Pred( Width__ ) + ( Row__  * Width__ ) ];
        //if Result <> 0 then Result := 1;
      end;

      function  VotesForColumnWidthInColumn( ColWidth__, Col__ : Integer ) : Integer;
      begin
        Result := VGrid__^[ Col__ + ( ColWidth__ * MAX_GRID_SIZE ) ];
      end;

      function  VotesForRowHeightInRow( RowHeight__, Row__ : Integer ) : Integer;
      begin
        Result := HGrid__^[ RowHeight__ + ( Row__ * MAX_GRID_SIZE ) ];
      end;

      function  ColWidthVotes( ColWidth__ : Integer ) : Integer;
      var Col, Votes : Integer;
      begin
        Result          := 0;
        if ColWidth__   <  Min( Height__, MAX_GRID_SIZE ) then
           for Col      := Rect__.Left to Pred( Rect__.Right ) do begin
               Votes    := VotesForColumnWidthInColumn( ColWidth__, Col );
               if Votes >  0 then
                  Inc( Result, Votes );
               end;
      end;

      function  RowHeightVotes( RowHeight__ : Integer ) : Integer;
      var Row, Votes : Integer;
      begin
        Result          := 0;
        if RowHeight__  <  Min( Width__, MAX_GRID_SIZE ) then
           for Row      := Rect__.Top to Pred( Rect__.Bottom ) do begin
               Votes    := VotesForRowHeightInRow( RowHeight__, Row );
               if Votes >  0 then
                  Inc( Result, Votes );
               end;
      end;

      function  ColWidthVotes__( ColWidth__ : Integer ) : Integer;
      var Index, Col, Votes, Votes2 : Integer;
      begin
        Result            := 0;
        if ( ColWidth__   >  0 ) and
           ( ColWidth__   <  Min( Height__, MAX_GRID_SIZE ) ) then
           for Col        := Rect__.Left to Pred( Rect__.Right ) do begin
               Votes      := VotesForColumnWidthInColumn( ColWidth__, Col );
               if Votes   >  0 then begin
                  // only count columns which can be combined with this one using the specified column width
                  Votes   := StraightLinePixelsInColumn( Col );
                  for     Index     := 1 to Min( CAPTURE_MAX_BOARD_WIDTH, ( Pred( Rect__.Right ) - Col ) div ColWidth__  ) do begin
                          Votes2    := VotesForColumnWidthInColumn( ColWidth__, Col + ( Index * ColWidth__ ) );
                          if Votes2 >  0 then
                             Inc( Votes, StraightLinePixelsInColumn( Col + ( Index * ColWidth__ ) ) );
                          end;
                  if      Votes  >  Result then
                          Result := Votes;
                  end;
               end;
      end;

      function  RowHeightVotes__( RowHeight__ : Integer ) : Integer;
      var Index, Row, Votes, Votes2 : Integer;
      begin
        Result            := 0;
        if ( RowHeight__  > 0 ) and
           ( RowHeight__  <  Min( Width__, MAX_GRID_SIZE ) ) then
           for Row        := Rect__.Top to Pred( Rect__.Bottom ) do begin
               Votes      := VotesForRowHeightInRow( RowHeight__, Row );
               if Votes   >  0 then begin
                  // only count rows which can be combined with this one using the specified row height
                  Votes   := StraightLinePixelsInRow( Row );
                  for     Index     := 1 to Min( CAPTURE_MAX_BOARD_HEIGHT, ( Pred( Rect__.Bottom ) - Row ) div RowHeight__ ) do begin
                          Votes2    := VotesForRowHeightInRow( RowHeight__, Row + ( Index * RowHeight__ ) );
                          if Votes2 >  0 then
                             Inc( Votes, StraightLinePixelsInRow( Row + ( Index * RowHeight__ ) ) );
                          end;
                  if      Votes  >  Result then
                          Result := Votes;
                  end;
               end;
      end;

      function  MaximumVotesRowHeight : Integer;
      var Index, MaximumVotes, Votes : Integer;
      begin
        Result              := 0;
        MaximumVotes        := 0;
        for index           := 0 to MAX_GRID_SIZE - 1 do begin
            Votes           := RowHeightVotes__( Index );
            if Votes        >  MaximumVotes then begin
               MaximumVotes := Votes;
               Result       := Index;
               end;
            end;
      end;

      function  MaximumVotesColWidth : Integer;
      var Index, MaximumVotes, Votes : Integer;
      begin
        Result              := 0;
        MaximumVotes        := 0;
        for index           := 0 to MAX_GRID_SIZE - 1 do begin
            Votes           := ColWidthVotes__( Index );
            if Votes        >  MaximumVotes then begin
               MaximumVotes := Votes;
               Result       := Index;
               end;
            end;
      end;

      procedure DisplayVotes;
      var Index : Integer;
      begin
        //DisplayPixels( 'HGrid', MAX_GRID_SIZE, Height__, HGrid, 1.0 );
        //DisplayPixels( 'VGrid', Width__, MAX_GRID_SIZE, VGrid, 1.0 );

        ToolsForm.Memo1.Lines.Add( '' );
        ToolsForm.Memo1.Lines.Add( 'Column width votes' );
        Index := MAX_GRID_SIZE - 1;
        while ( Index >= 0 ) and ( ColWidthVotes( Index ) <= 0 )  do Dec( Index );
        for Index := 0 to Index do
            ToolsForm.Memo1.Lines.Add( Format( '%4d: %8d', [ Index, ColWidthVotes__( Index ) ] ) );

        ToolsForm.Memo1.Lines.Add( '' );
        ToolsForm.Memo1.Lines.Add( 'Row height votes' );
        Index := MAX_GRID_SIZE - 1;
        while ( Index >= 0 ) and ( RowHeightVotes( Index ) <= 0 )  do Dec( Index );
        for Index := 0 to Index do
            ToolsForm.Memo1.Lines.Add( Format( '%4d: %8d', [ Index, RowHeightVotes__( Index ) ] ) );

        ToolsForm.Memo1.Lines.Add( '' );
        ToolsForm.Memo1.Lines.Add( 'Grid cell size: ' + IntToStr( ColWidth__ ) + ' x ' + IntToStr( RowHeight__ ) );

        Clipboard.AsText := ToolsForm.Memo1.Lines.Text;
      end;

      function  Check( var ColWidth__, RowHeight__ : Integer ): Boolean;
      begin
        Result := ( ColWidth__ > 0 ) and ( RowHeight__ > 0 );
        if   Result then begin
             ToolsForm.Memo1.Lines.Add( 'Grid cell size: ' + IntToStr( ColWidth__ ) + ' x ' + IntToStr( RowHeight__ ) );
             end
        else ColWidth__ := 0;
      end;

    begin // FindColumnWidthAndRowHeight
      repeat  ColWidth__  := MaximumVotesColWidth;
              RowHeight__ := MaximumVotesRowHeight;
              Result      := Check( ColWidth__, RowHeight__ );
      until   Result or ( ColWidth__ = 0 ) or ( RowHeight__ = 0);
      DisplayVotes;
    end;

  begin // AnalyseLines
    HGrid  := nil; VGrid := nil;
    try
      CountStraightLinePixels   ( Width__, Height__, HorzLines__, VertLines__, Rect__ );
      Result := AnalyseHorzLines( Width__, Height__, HorzLines__, Rect__, HGrid ) and
                AnalyseVertLines( Width__, Height__, VertLines__, Rect__, VGrid ) and
                FindColumnWidthAndRowHeight( Width__, Height__, HGrid, VGrid, Rect__, ColWidth, RowHeight );
      if Result then begin
         end;
    finally FreeMem( HGrid ); FreeMem( VGrid );
    end;
  end;

begin // 'FindBoard' tries to locate the Sokoban board in the image; the search is based on the horizontal and vertical straight lines in the image;
      // the function is experimental and is not activated in the production version of the application;
  Result                      := False;
  TimeMS                      := GetTickCount;
  try     Screen.Cursor       := crHourGlass;
          BoardRect__         := Rect( 0, 0, 0, 0 );
          ColCount__          := 0;
          RowCount__          := 0;
          ToolsForm.Memo1.Lines.Clear;
          if False and
             Assigned( Bitmap__ ) and Assigned( HorzLines__ ) and Assigned( VertLines__ ) and Assigned( IntegralImage__ ) then with BitmapRect__   do begin
             BitmapRect__     := ScaleRect( Width__, Height__, Bitmap__.Width, Bitmap__.Height, BitmapRect__ );
             Result           := AnalyseLines( BitmapRect__ );
             end;
          if Result then begin
             //if Left          >= Right  - MIN_BOARD_SIZE_PIXELS then Result := False;
             //if Top           >= Bottom - MIN_BOARD_SIZE_PIXELS then Result := False;
             //Result           := Result and ClipRect( BoardRect, Rect ( 0, 0, Bitmap__.Width - 2, Bitmap__.Height - 2 ) );
             //BoardRect__      := BoardRect;
             //ColCount__       := RectWidth ( BoardRect ) div ColWidth;
             //RowCount__       := RectHeight( BoardRect ) div RowHeight;
             //Right            := Left + ColCount__ * ColWidth;
             //Bottom           := Top  + RowCount__ * RowHeight;
             end;

  finally Screen.Cursor := crDefault;
          ToolsForm.StatusBar1.Panels[ 1 ].Text := '';
  end;
  TimeMS := CalculateElapsedTimeMS( TimeMS, GetTickCount );
  if TimeMS > 10000 then begin
     //Msg( 'Image analysis time: ' + IntToStr( TimeMS ), '', MB_OK+MB_ICONINFORMATION );
     end;
end;

procedure TCaptureForm.SkinBitBtnClick(Sender: TObject);
var //Index:Integer;
    oInitialDir : String;
begin
  with OpenPictureDialog1 do begin
    oInitialDir := InitialDir;
    try
      try
        Title := TEXT_LOAD_SKIN_CAPTION;
        InitialDir := SkinInitialDirectory;
        if ( InitialDir =  '' ) or ( not DirectoryExists( InitialDir ) ) then
           InitialDir := StrWithoutTrailingPathDelimiter( MainForm.Skins.DefaultSkinPath );
        if ( InitialDir =  '' ) or ( not DirectoryExists( InitialDir ) ) then
           InitialDir := StrWithoutTrailingPathDelimiter( GetFolderPath( CSIDL_MYPICTURES ) );
        if ( InitialDir =  '' ) or ( not DirectoryExists( InitialDir ) ) then
           InitialDir := StrWithoutTrailingPathDelimiter( GetFolderPath( CSIDL_PERSONAL ) );

        FileName:='';

        //for Index := 0 to Pred( SkinsComboBox.Items.Count ) do
        //    if FileName = '' then begin
        //       FileName := ExpandedFilePath( SkinsComboBox.Items[Index], MainForm.MyDocumentsFolder );
        //       if   FileExists( FileName ) then
        //            InitialDir := ExtractFilePath( FileName )
        //       else FileName := '';
        //       end
        //    else break;

        if Execute then begin
           SkinInitialDirectory := StrWithoutTrailingPathDelimiter( ExtractFilePath( FileName ) );
           if not StrEqual( FileName, DefaultMatchingSkinFileName ) then
              AddItemOrMoveItemToTopOfComboBox(SkinsComboBox,SKINS_CAPACITY,AbbreviatedFilePath(FileName,MainForm.MyDocumentsFolder),False);
           FindBoardUsingMatchingSkin( OpenPictureDialog1.FileName, Settings.MatchThresholdPct, True );
           end;
      except on E:Exception do Error( E.Message, '' );
      end;
    finally InitialDir := oInitialDir;
    end;
    end;
end;

function  TCaptureForm.CreateSkinCaptureInformation( SingleRowSkinBitmap__ : TBitmap; ColWidth__, RowHeight__ : Integer; UpdateMatchingSkin__ : Boolean; var MatchingSkinImagesChanged__ : Boolean ) : Boolean;
var i, ItemCount, OldItemCount, Col, Row, Index, NewItemIndex : Integer;
    NewMatchingSkinFileName : String;
    BoardSquareTypeIndex : TBoardSquareType;
    R : TRect;
    SkinGrayScalePixels, SkinIntegralImage : PPixels;
    Items     : array[ 0 .. 2 * ( CAPTURED_SKIN_TILE_COUNT + ( CAPTURE_MAX_BOARD_WIDTH * CAPTURE_MAX_BOARD_HEIGHT ) ) ] of TPixel;           // '+CAPTURED_SKIN_TILE_COUNT': the captured skin elements; they don't necessarily appear in the source image
    ItemTypes : array[ 0 .. 2 * ( CAPTURED_SKIN_TILE_COUNT + ( CAPTURE_MAX_BOARD_WIDTH * CAPTURE_MAX_BOARD_HEIGHT ) ) ] of TBoardSquareType; // '+CAPTURED_SKIN_TILE_COUNT': the captured skin elements; they don't necessarily appear in the source image
    MatchingSkinCaptureInformation : PSkinCaptureInformation; // skin capture information from a matching skin image
    MatchingSkinBitmap : TBitmap;                             // a matching skin used for extracting a board from the loaded image
    CapturedBoard : TCaptureBoard;

  function  AddItem( Value__ : TPixel; BoardSquareType__ : TBoardSquareType ) : Integer;
  begin
    if ( ItemCount < High( Items ) ) and
       // filter out undefined square types unless it's one of the first
       // 'CAPTURED_SKIN_TILE_COUNT' values which match the graphics in the skin;
       ( ( BoardSquareType__ <> bstUndefined ) or ( ItemCount < CAPTURED_SKIN_TILE_COUNT ) ) then begin
       Result := ItemCount;
       Items[ Result ] := Value__; // new value or sentinel value
       if ItemCount >= CAPTURED_SKIN_TILE_COUNT then begin // 'True': the value doesn't come from one of the captured skin elements; instead, it may be a new additional value; search through the existing values for a match
          Result := 0;
          while Items[ Result ] <> Value__ do
            Inc( Result );
          end;
       if Result = ItemCount then begin // 'True': its' a new value which is added to the table
          ItemTypes[ Result ] := BoardSquareType__;
          Inc( ItemCount );
          end;
       end
    else Result := -1; // table full
  end;

  function  CreateSkinCaptureInformationDataStructure( ItemCount__ : Integer; var SkinCaptureInformation__ : PSkinCaptureInformation ) : Boolean;
  var ByteSize : Integer; //Index : TObjectTypes;
  begin
    //Count  := 0;
    //for Index := Low( ObjectCounts__ ) to High( ObjectCounts__ ) do Inc( Count, ObjectCounts__[ Index ] );
    ByteSize := SizeOf( SkinCaptureInformation__^.Header ) + ( ItemCount__ * SizeOf( SkinCaptureInformation__^.Items[ 0 ] ) );
    try    GetMem( SkinCaptureInformation__, ByteSize );
           ZeroMemory( Addr( SkinCaptureInformation__^ ),    ByteSize );
           SkinCaptureInformation__^.Header.TotalByteSize := ByteSize;
    except on E:Exception do SkinCaptureInformation__     := nil;
    end;
    Result := Assigned( SkinCaptureInformation__ );
  end;

  function  UpdateMatchingSkin( var MatchingSkinFileName__            : String;
                                var MatchingSkinBitmap__              : TBitmap;
                                var MatchingSkinCaptureInformation__  : PSkinCaptureInformation;
                                var MatchingSkinImagesChanged__       : Boolean ) : Boolean;
  var
    Index : Integer;
    BitValue, CapturedSquareTypes : Cardinal;
    NewSquareTypes : TBoardSquareTypeSet;                     // square types found in the loaded image, but not in the original matching skin
    OldSquareTypes : TBoardSquareTypeSet;                     // square types found in the original matching skin, but not in the loaded image

    procedure Copy( Col__, Row__ : Integer );
    var R : TRect;
    begin
      R := CellToRect( Col__ , Row__, SkinCaptureInformation.Header.ColumnWidth, SkinCaptureInformation.Header.RowHeight);
      if (MatchingSkinBitMap__.Width>=R.Right) and (MatchingSkinBitMap__.Height>=R.Bottom) then
         MatchingSkinBitmap__.Canvas.CopyRect( R, SkinBitmap.Canvas, R );
    end;

  begin // 'UpdateMatchingSkin' : if the board has been extracted from the loaded
        // image by using a matching skin, this skin may need updating with square
        // types found in the currently loaded image; for instance, the current
        // image may have a box-on-goal square, whereas the matching skin happened
        // to be built from an image which only had a box-on-floor image;
        // preconditions: the matching skin, if any, has been loaded and it has
        // been verified that it still matches the image;
        // if there is no matching skin, a pseudo skin in created with just the
        // skin capture information; that way, the skin from the current image
        // is automatically used for matching the next time the user loads an
        // image;
    Result := ( MatchingSkinFileName__ <> '' ) and
              Assigned( MatchingSkinBitmap__ ) and Assigned( MatchingSkinCaptureInformation__ ) and
              Assigned( SkinCaptureInformation );
    MatchingSkinImagesChanged__ := False;
    try
      if Result then begin

         // check for updates of the matching skin with square types (e.g., "box-on-goal") which weren't available when the matching skin was extracted from another image
         NewSquareTypes := []; OldSquareTypes := [];
         if MatchingSkinCaptureInformation__.Header.CapturedSquareTypes <> SkinCaptureInformation.Header.CapturedSquareTypes then begin
            Index := CAPTURED_SKIN_TILE_WALL_CAP_INDEX;
            repeat BitValue := Cardinal( 1 shl Index );
                   if ( ( BitValue and         MatchingSkinCaptureInformation__.Header.CapturedSquareTypes ) =  0 )
                      and
                      ( ( BitValue and         SkinCaptureInformation.Header.CapturedSquareTypes           ) <> 0 ) then
                      Include( NewSquareTypes, SkinTileIndexToBoardSquareType( Index, nil ) );
                   if ( ( BitValue and         MatchingSkinCaptureInformation__.Header.CapturedSquareTypes ) <> 0 )
                      and
                      ( ( BitValue and         SkinCaptureInformation.Header.CapturedSquareTypes           ) =  0 ) then
                      Include( OldSquareTypes, SkinTileIndexToBoardSquareType( Index, nil ) );
                   Inc( Index );
            until  Index >= CAPTURED_SKIN_TILE_COUNT;
            if ( NewSquareTypes <> [] )
               and
               (  StrEqual( MatchingSkinFileName__, DefaultMatchingSkinFileName ) // 'True': pseudo skin with skin capture information from the most recently loaded image; the only tile in the pseudo skin is the one with the skin capture information;
                  or
                  StrEqual( MatchingSkinFileName__, Editor.SkinFileName )
                  or
                  True // 'True': force an update of the matching skin; the user is not given the choice to not update the matching skin; it would be both confusing and unpractical; the message window may be hidden below the main window so it appears as if the application has stopped responding
                  or
                  ( Msg( Format( TEXT_FORMAT_NEW_IMAGE_CONTAINS_SQUARE_TYPES_NOT_FOUND_IN_MATCHING_SKIN, [ AbbreviatedFilePath( MatchingSkinFileName__, MainForm.MyDocumentsFolder ) ] ),
                         '', MB_ICONQUESTION + MB_YESNO ) = ID_YES )
               ) then begin
               if bstWall         in NewSquareTypes then begin // the wall cap is the only wall element which is checked and updated by this application
                  Copy( 2, 2 );
                  Inc( MatchingSkinCaptureInformation__.Header.CapturedSquareTypes, ( 1 shl CAPTURED_SKIN_TILE_WALL_CAP_INDEX ) );
                  end;
               if bstFloor        in NewSquareTypes then begin // floor
                  Copy( 0, 0 );
                  Inc( MatchingSkinCaptureInformation__.Header.CapturedSquareTypes, ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiFloor ] ) ) );
                  end;
               if bstGoal         in NewSquareTypes then begin // goal
                  Copy( 0, 1 );
                  Inc( MatchingSkinCaptureInformation__.Header.CapturedSquareTypes, ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiGoal ] ) ) );
                  end;
               if bstPlayer       in NewSquareTypes then begin // player-on-floor
                  Copy( 1, 0 );
                  Inc( MatchingSkinCaptureInformation__.Header.CapturedSquareTypes, ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiPlayer ] ) ) );
                  end;
               if bstPlayerOnGoal in NewSquareTypes then begin // player-on-goal
                  Copy( 1, 1 );
                  Inc( MatchingSkinCaptureInformation__.Header.CapturedSquareTypes, ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiPlayer, bsiGoal ] ) ) );
                  end;
               if bstBox          in NewSquareTypes then begin // box-on-floor
                  Copy( 2, 0 );
                  Inc( MatchingSkinCaptureInformation__.Header.CapturedSquareTypes, ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiBox ] ) ) );
                  end;
               if bstBoxOnGoal    in NewSquareTypes then begin // box-on-floor
                  Copy( 2, 1 );
                  Inc( MatchingSkinCaptureInformation__.Header.CapturedSquareTypes, ( 1 shl BoardSquareItemSetToSkinTileIndex( [ bsiBox, bsiGoal ] ) ) );
                  end;
               end
            else begin
               NewSquareTypes := [];
               if ( OldSquareTypes <> [] )
                  and
                  ( StrEqual( MatchingSkinFileName__, DefaultMatchingSkinFileName ) // 'True': pseudo skin with skin capture information from the most recently loaded image; the only tile in the pseudo skin is the one with the skin capture information;
                    or
                    StrEqual( MatchingSkinFileName__, Editor.SkinFileName ) ) then begin
                  if ( OldSquareTypes * [ bstPlayer, bstPlayerOnGoal, bstBox, bstBoxOnGoal ] ) <> [] then
                     MatchingSkinImagesChanged__ := True; // fake that the matching skin has been updated, so the caller loads and uses the matching skin
                  end;
               end;
            end;

         for Index := 0 to Pred( Min( MatchingSkinCaptureInformation__.Header.ItemCount, SkinCaptureInformation.Header.ItemCount ) ) do
             if MatchingSkinCaptureInformation__.Items[ Index ] <> SkinCaptureInformation.Items[ Index ] then // 'True': the item sets may differ; trigger an update
                MatchingSkinCaptureInformation__.Header.ItemCount := -1;
         if ( NewSquareTypes <> [] ) or ( MatchingSkinCaptureInformation__.Header.ItemCount <> SkinCaptureInformation.Header.ItemCount ) then begin
            // write the new capture information with the accumulated set of known square types (represented by their integral image values)
            CapturedSquareTypes := SkinCaptureInformation.Header.CapturedSquareTypes;
            try     SkinCaptureInformation.Header.CapturedSquareTypes := MatchingSkinCaptureInformation__.Header.CapturedSquareTypes;
                    Result := WriteSkinCaptureInformation( MatchingSkinBitMap__, SkinCaptureInformation );
                    if Result then begin
                       if NewSquareTypes <> [] then
                          MatchingSkinImagesChanged__ := True;
                       MatchingSkinBitmap__.SaveToFile( MatchingSkinFileName__ );
                       end;
            finally SkinCaptureInformation.Header.CapturedSquareTypes := CapturedSquareTypes;
            end;
            end;
         end
      else begin
         MatchingSkinBitmap__.Free; MatchingSkinBitmap := nil;
         try     Result := Assigned( SkinCaptureInformation ) and
                           BitmapCreate( MatchingSkinBitmap__, SkinCaptureInformation.Header.ColumnWidth, SkinCaptureInformation.Header.RowHeight ) and
                           WriteSkinCaptureInformation( MatchingSkinBitmap__, SkinCaptureInformation );
                 if Result then begin
                    MatchingSkinFileName__ := DefaultMatchingSkinFileName;
                    MatchingSkinBitmap__.SaveToFile( MatchingSkinFileName__ );
                    end;
         finally MatchingSkinBitmap__.Free; MatchingSkinBitmap__ := nil;
                 if not Result then MatchingSkinFileName__ := '';
         end;
         end;
    except on E:Exception do Result := Error( E.Message, '' );
    end
  end; // UpdateMatchingSkin

begin // 'CreateSkinCaptureInformation'
  Result := False;
  MatchingSkinImagesChanged__ := False;
  FreeMem( fSkinCaptureInformation ); fSkinCaptureInformation := nil;
  SkinGrayScalePixels := nil; SkinIntegralImage := nil;
  MatchingSkinBitmap := nil; MatchingSkinCaptureInformation := nil;
  try
    if ( ColWidth__  <= High( SkinCaptureInformation^.Header.ColumnWidth ) ) and
       ( RowHeight__ <= High( SkinCaptureInformation^.Header.RowHeight   ) ) and
       Assigned( SingleRowSkinBitmap__ ) and
       CreatePixels( SingleRowSkinBitmap__.Width, SingleRowSkinBitmap__.Height, SkinGrayScalePixels ) and
       BitmapToGrayscalePixels( SingleRowSkinBitmap__, SkinGrayScalePixels, 0 ) and // '0': all color channel bits, i.e., unmasked
       CreatePixels( SingleRowSkinBitmap__.Width, SingleRowSkinBitmap__.Height, SkinIntegralImage ) and
       ( ( CalculateIntegralImage( SingleRowSkinBitmap__.Width, SingleRowSkinBitmap__.Height, SkinGrayScalePixels, SkinIntegralImage ) > 0 ) or True ) then begin
       ItemCount := 0;
       // add integral values for the captured skin elements
       for Index := 0 to Pred( CAPTURED_SKIN_TILE_COUNT ) do
           AddItem( GetIntegralImage( Index * ColWidth__, 0, Succ( Index ) * ColWidth__, RowHeight__, SingleRowSkinBitmap__.Width, SkinIntegralImage ),
                    SkinTileIndexToBoardSquareType( Index, nil ) );

       // add additional integral values from the source image, i.e., for squares which don't match the captured skin; for instance, wall squares partially covered by a wall cap are not represented in the captured skin (only the wall cap itself);
       for Col := 1 to Editor.Board.Width do
           for Row := 1 to Editor.Board.Height do begin
               Index := BoardSquareToSkinTileIndex( Col, Row, Editor.Board );
               if Index >= 0 then with R do begin // 'True': the square is non-empty, i.e., it's a part of the board
                  R := NormalizedRect( ColRowToRect( Pred( Col ), Pred( Row ) ) );
                  AddItem( GetIntegralImage( Left, Top, Right, Bottom, IntegralImageWidth, IntegralImage ),
                           SkinTileIndexToBoardSquareType( Index, nil ) );
                  end;
               end;

       if ToolsForm.CaptureSettingsColorQuantizationIgnoreSmallColorDifferences.Checked and
          BitmapToGrayscalePixels( SingleRowSkinBitmap__, SkinGrayScalePixels, High( RGB_BLACK.r ) - ( ( 1 shl DISCARD_COLOR_COMPONENT_LOW_BITS_COUNT ) - 1 ) ) and
          ( ( CalculateIntegralImage( SingleRowSkinBitmap__.Width, SingleRowSkinBitmap__.Height, SkinGrayScalePixels, SkinIntegralImage ) > 0 ) or True ) then begin
          // add integral values for the captured skin elements, ignoring small color differences by discarding the low order bits for each color channel
          for Index := 0 to Pred( CAPTURED_SKIN_TILE_COUNT ) do
              AddItem( GetIntegralImage( Index * ColWidth__, 0, Succ( Index ) * ColWidth__, RowHeight__, SingleRowSkinBitmap__.Width, SkinIntegralImage ),
                       SkinTileIndexToBoardSquareType( Index, nil ) );
          end;

       if Assigned( IntegralImage2 ) then // ' True': integral image values have been calculated, ignoring small color differences by discarding the low order bits for each color channel
          // add additional integral values from the source image, i.e., for squares which don't match the captured skin; for instance, wall squares partially covered by a wall cap are not represented in the captured skin (only the wall cap itself);
          for Col := 1 to Editor.Board.Width do
              for Row := 1 to Editor.Board.Height do begin
                  Index := BoardSquareToSkinTileIndex( Col, Row, Editor.Board );
                  if Index >= 0 then with R do begin // 'True': the square is non-empty, i.e., it's a part of the board
                     R := NormalizedRect( ColRowToRect( Pred( Col ), Pred( Row ) ) );
                     AddItem( GetIntegralImage( Left, Top, Right, Bottom, IntegralImageWidth, IntegralImage2 ),
                              SkinTileIndexToBoardSquareType( Index, nil ) );
                     end;
                  end;

       if UpdateMatchingSkin__ then
          // add additional integral values from a matching skin, if any
          if LoadImageFromFile         ( MatchingSkinFileName, MatchingSkinBitmap                        ) and
             ReadSkinCaptureInformation( MatchingSkinBitmap, False, i, i, MatchingSkinCaptureInformation ) and
             ( MatchingSkinCaptureInformation.Header.ColumnWidth = ColWidth__                            ) and
             ( MatchingSkinCaptureInformation.Header.RowHeight   = RowHeight__                           ) and
             FindBoardUsingUnscaledSkin( nil, MatchingSkinCaptureInformation, NormalizedBoardRect, Settings.MatchThresholdPct, R, CapturedBoard ) and // 'True': the skin still matches the board, at least to some extent
             ( ( Abs( R.Left - ToolsForm.LeftSpinEdit.Value ) mod Max( 1, ToolsForm.ColWidthSpinEdit .Value ) ) = 0 ) and
             ( ( Abs( R.Top  - ToolsForm.TopSpinEdit .Value ) mod Max( 1, ToolsForm.RowHeightSpinEdit.Value ) ) = 0 ) then begin    // 'True': the matching skin is still aligned with the cells on the board
             for Index := 0 to Pred( MatchingSkinCaptureInformation.Header.ItemCount ) do begin
                 OldItemCount := ItemCount;
                 NewItemIndex := AddItem( MatchingSkinCaptureInformation.Items[ Index ], SkinTileIndexToBoardSquareType( Index, MatchingSkinCaptureInformation ) );
                 if ( NewItemIndex >= CAPTURED_SKIN_TILE_COUNT ) and // 'True': the value isn't identical to one of the integral image values matching the skin tiles
                    ( NewItemIndex <> OldItemCount ) then begin      // 'True': the value wasn't added to the set as a new member
                    end;
                 end;
             end
          else MatchingSkinFileName := '';

       if CreateSkinCaptureInformationDataStructure( ItemCount, fSkinCaptureInformation ) then begin
          for Index := Low( SOKOBAN_SKIN_IDENTIFICATION ) to High( SOKOBAN_SKIN_IDENTIFICATION ) do with SkinCaptureInformation^.Header do
              Identification[ Index ] := SOKOBAN_SKIN_IDENTIFICATION[ Index ];
          SkinCaptureInformation^.Header.HeaderByteSize           := SizeOf( SkinCaptureInformation^.Header ); // for version detection
          SkinCaptureInformation^.Header.ColumnWidth              := ColWidth__;
          SkinCaptureInformation^.Header.RowHeight                := RowHeight__;
          SkinCaptureInformation^.Header.CapturedSquareTypes      := Editor.CapturedSquareTypes;
          // transfer integral values for the captured skin elements
          for Index := 0 to Pred( CAPTURED_SKIN_TILE_COUNT ) do begin
              SkinCaptureInformation^.Items[ SkinCaptureInformation^.Header.ItemCount ] := Items[ Index ];
              Inc( SkinCaptureInformation^.Header.ItemCount );
              end;
          // transfer the additional integral image values to the skin capture information, sorting them in descending sequence on board square type; that way, walls come first, and they typically require more additional integral image values than the other square types
          for BoardSquareTypeIndex := High( SkinCaptureInformation^.Header.ItemOffsets ) downto
                                      Low ( SkinCaptureInformation^.Header.ItemOffsets ) do begin // reversed order, so the 'undefined' value, which is the lowest value, can be used later as sentinel during a search for the square type assigned to a given integral image value
              SkinCaptureInformation^.Header.ItemOffsets[ BoardSquareTypeIndex ] := SkinCaptureInformation^.Header.ItemCount;
              if BoardSquareTypeIndex <> Low ( SkinCaptureInformation^.Header.ItemOffsets ) then
                 for Index := CAPTURED_SKIN_TILE_COUNT to Pred( ItemCount ) do
                     if  ItemTypes[ Index ] = BoardSquareTypeIndex then begin
                         SkinCaptureInformation^.Items[ SkinCaptureInformation^.Header.ItemCount ] := Items[ Index ];
                         Inc( SkinCaptureInformation^.Header.ItemCount );
                         end;
              end;
          Result := True;

          if UpdateMatchingSkin__ then begin
             NewMatchingSkinFileName := MatchingSkinFileName;
             if      UpdateMatchingSkin( NewMatchingSkinFileName, MatchingSkinBitmap, MatchingSkinCaptureInformation, MatchingSkinImagesChanged__ ) then
             else if MatchingSkinImagesChanged__ then // 'True': the matching skin should have been updated with new tiles, but it failed
                     Result := False;
             MatchingSkinFileName := NewMatchingSkinFileName;
             end;
          end;
       end;
  finally FreeMem( SkinGrayScalePixels ); FreeMem( SkinIntegralImage );
          MatchingSkinBitmap.Free; FreeMem( MatchingSkinCaptureInformation );
  end;
end;

function  TCaptureForm.CreateSkinCaptureInformationUpdatingMatchingSkin( var MatchingSkinImagesChanged__ : Boolean ) : Boolean;
begin // precondition: the single-row skin has been created
  Result := ( Editor.CompletedStep > csBoardSquares ) and
            CreateSkinCaptureInformation( SingleRowSkinBitmap, ToolsForm.ColWidthSpinEdit.Value, ToolsForm.RowHeightSpinEdit.Value, True, MatchingSkinImagesChanged__ );
end;

function  TCaptureForm.WriteSkinCaptureInformation( BitMap__ : TBitmap; SkinCaptureInformation__ : PSkinCaptureInformation ) : Boolean;
var ColumnWidthBytes, TileSizeBytes, BitmapWidthBytes, ByteCountDown, X, Y : Integer;
    BoardSquareTypeIndex : TBoardSquareType;
    OriginalHeader : TSkinCaptureInformationHeader;
    Source, Destination : PByte;
begin
   Result :=  Assigned( SkinCaptureInformation__ ) and
              Assigned( Bitmap__ ) and
              ( PIXEL_BYTE_SIZE[ Bitmap__.PixelFormat ]     > 0 ) and
              ( SkinCaptureInformation__.Header.ColumnWidth > 0 ) and
              ( SkinCaptureInformation__.Header.RowHeight   > 0 ) and
              (   Bitmap__.Width  >=  SkinCaptureInformation__.Header.ColumnWidth ) and
              (   Bitmap__.Height >=  SkinCaptureInformation__.Header.RowHeight   ) and
              ( ( Bitmap__.Width  mod SkinCaptureInformation__.Header.ColumnWidth ) = 0 );
   if Result then with Bitmap__ do with Canvas do begin
      ColumnWidthBytes := SkinCaptureInformation__.Header.ColumnWidth * PIXEL_BYTE_SIZE[ PixelFormat ];
      TileSizeBytes    := ColumnWidthBytes * SkinCaptureInformation__.Header.RowHeight;
      OriginalHeader   := SkinCaptureInformation__.Header; // local copy of the header; the original header will be temporarily modified if the information must be truncated in order to fit the available space in the skin
      try
        // prune the vector to fit the available space; the maximum number of
        // tiles available for the data is 3 because if it's a skin with 4x4
        // tiles, then the bottom-right tile contains the textual skin settings;
        // however, only 2 tiles are used, so there still is one tile free for
        // other purposes;
        SkinCaptureInformation__.Header.ItemCount      := Min(    SkinCaptureInformation__.Header.ItemCount, ( ( Min( 2, ( Bitmap__.Height div SkinCaptureInformation__.Header.RowHeight ) ) * TileSizeBytes ) - SizeOf( SkinCaptureInformation__.Header ) ) div SizeOf( SkinCaptureInformation__.Items[ 0 ] ) );
        SkinCaptureInformation__.Header.TotalByteSize  := SizeOf( SkinCaptureInformation__.Header ) + ( SkinCaptureInformation__.Header.ItemCount * SizeOf( SkinCaptureInformation__.Items[ 0 ] ) );
        for BoardSquareTypeIndex := Low ( SkinCaptureInformation__.Header.ItemOffsets ) to
                                    High( SkinCaptureInformation__.Header.ItemOffsets ) do
            SkinCaptureInformation__.Header.ItemOffsets[ BoardSquareTypeIndex ] := Min( SkinCaptureInformation__.Header.ItemCount, SkinCaptureInformation__.Header.ItemOffsets[ BoardSquareTypeIndex ] );
        Result := SkinCaptureInformation__.Header.ItemCount >= CAPTURED_SKIN_TILE_COUNT; // 'True': there is at least room for the integral image values from the captured skin tiles

        // first clear the destination rectangle
        Brush.Style := bsSolid;
        Brush.Color := clDkGray;
        FillRect( CellToRect( Pred( Width div SkinCaptureInformation__.Header.ColumnWidth ), 0, SkinCaptureInformation__.Header.ColumnWidth, SkinCaptureInformation__.Header.RowHeight ) ); // top-most tile in the right-most column

        if Result then begin
           BitmapWidthBytes            := Width * PIXEL_BYTE_SIZE[ PixelFormat ]; // total number of bytes per scanline
           ByteCountDown               := SkinCaptureInformation__.Header.TotalByteSize;
           Source                      := Addr( SkinCaptureInformation__.Header );
           for Y                       := 0 to Pred( Bitmap__.Height ) do
               if ByteCountDown        >  0 then begin
                  Destination          := PByte( Cardinal( ScanLine[ Y ] ) + Cardinal( Pred( BitmapWidthBytes ) ) ); // '+': write the information backwards
                  for X                := 0 to Pred( ColumnWidthBytes ) do
                      if ByteCountDown >  0 then begin
                         Destination^  := Source^;
                         Inc( Source );
                         Dec( Destination ); // 'Dec': write the information backwards
                         Dec( ByteCountDown );
                         end
                      else break;
                  end
               else break;
           end;
      finally
        SkinCaptureInformation__.Header := OriginalHeader; // restore the original header
      end;
      end;
end;

function  TCaptureForm.ReadSkinCaptureInformation( BitMap__ : TBitMap;
                                                   JustReadColumnsAndRows__ : Boolean;
                                                   var ColumnCount__, RowCount__ : Integer;
                                                   var SkinCaptureInformation__ : PSkinCaptureInformation ) : Boolean;
type
  TReader = record
    Position : PByte;
    X, Y : Integer;
  end;
var
  TileSizeBytes, ColumnWidthBytes, BitmapWidthBytes, ByteCountDown, HeaderByteSize, TotalByteSize, ColumnWidth, RowHeight, X, Y, Index : Integer;
  Source, Destination : PByte;
  Reader : TReader;

  procedure InitializeReader( var Reader__ : TReader );
  begin
    Reader__.Position := nil;
    Reader__.X := -1; Reader__.Y := -1;
  end;

  function  GetByte( var Reader__ : TReader ) : Integer;
  begin
    with Reader__ do begin
      if X >= 0 then begin
         Result := Position^;
         Dec( Position ); // 'Dec': the information is written backwards
         Dec( X );
         end
      else begin
         if   Y        <  Bitmap__.Height then
              Inc( Y );
         if   ( Y      <  Bitmap__.Height ) and ( BitmapWidthBytes > 0 ) then begin
              X        := Pred( ColumnWidthBytes );
              Position := PByte( Cardinal( Bitmap__.ScanLine[ Y ] ) + Cardinal( Pred( BitmapWidthBytes ) ) ); // '+': the information is written backwards
              Result   := GetByte( Reader__ );
              end
         else Result   := -1; // end of data
         end;
      end;
  end;

  function ReadInteger( var Reader__ : TReader; ByteSize__ : Integer ) : Integer;
  var Index, Factor : Integer;
  begin // reads an integer of the given byte size without overflow detection and without sign detection
    Result := 0; Factor := 1;
    for Index := 0 to Pred( ByteSize__ ) do begin
        Result := Result  + ( Factor * GetByte( Reader__ ) );
        Factor := Factor * 256;
        end;
  end;

begin // 'ReadSkinCaptureInformation'
  SkinCaptureInformation__ := nil;
  Result :=  Assigned( BitMap__                            ) and
             ( Bitmap__.Width                          > 0 ) and
             ( BitMap__.Height                         > 0 ) and
             ( PIXEL_BYTE_SIZE[ BitMap__.PixelFormat ] > 0 );
   if Result then with Bitmap__ do with Canvas do begin
      BitmapWidthBytes   := Width * PIXEL_BYTE_SIZE[ PixelFormat ]; // total number of bytes per scanline
      ColumnWidthBytes   := BitmapWidthBytes; // temporary value until the real column width can be calculated after reading the column width in pixels
      Result := BitmapWidthBytes >= SizeOf( SkinCaptureInformation__.Header.Identification ) +
                                    SizeOf( SkinCaptureInformation__.Header.HeaderByteSize ) +
                                    SizeOf( SkinCaptureInformation__.Header.TotalByteSize ) +
                                    SizeOf( SkinCaptureInformation__.Header.ColumnWidth ) +
                                    SizeOf( SkinCaptureInformation__.Header.RowHeight ); // these leading fields must be readable without line-breaks
      if Result then begin
         InitializeReader( Reader );
         Index  := 0;
         while ( Index             < SizeOf( SOKOBAN_SKIN_IDENTIFICATION          ) ) and
               ( GetByte( Reader ) = Ord   ( SOKOBAN_SKIN_IDENTIFICATION[ Index ] ) ) do
               Inc( Index );
         Result := Index = SizeOf( SOKOBAN_SKIN_IDENTIFICATION );
         if Result then begin
            HeaderByteSize   := ReadInteger( Reader, SizeOf( SkinCaptureInformation__.Header.HeaderByteSize ) );
            TotalByteSize    := ReadInteger( Reader, SizeOf( SkinCaptureInformation__.Header.TotalByteSize  ) );
            ColumnWidth      := ReadInteger( Reader, SizeOf( SkinCaptureInformation__.Header.ColumnWidth    ) );
            ColumnWidthBytes := ColumnWidth * PIXEL_BYTE_SIZE[ PixelFormat ];
            RowHeight        := ReadInteger( Reader, SizeOf( SkinCaptureInformation__.Header.RowHeight      ) );
            TileSizeBytes    := ColumnWidthBytes * RowHeight;
            Result           := ( HeaderByteSize = SizeOf( SkinCaptureInformation__.Header ) ) and
                                ( ColumnWidth               > 0      ) and ( RowHeight                >  0      ) and
                                ( ColumnWidth               <= Width ) and ( RowHeight                <= Height ) and
                                ( ( Width mod ColumnWidth ) = 0      ) and ( ( Height mod RowHeight ) =  0      ) and
                                ( TotalByteSize             <= TileSizeBytes * Min( 2, ( Height div RowHeight ) ) ); // '2': maximum 2 tiles in the last column are reserved for the skin capture information
            if Result then
               if JustReadColumnsAndRows__ then begin
                  ColumnCount__ := Width  div ColumnWidth;
                  RowCount__    := Height div RowHeight;
                  end
               else begin
                  try    GetMem( SkinCaptureInformation__, TotalByteSize );
                  except on E:Exception do SkinCaptureInformation__ := nil;
                  end;
                  Result := Assigned( SkinCaptureInformation__ );
                  if Result then begin
                     ByteCountDown               := TotalByteSize;
                     Destination                 := Addr( SkinCaptureInformation__.Header );
                     for Y                       := 0 to Pred( Bitmap__.Height ) do
                         if ByteCountDown        >  0 then begin
                            Source               := PByte( Cardinal( ScanLine[ Y ] ) + Cardinal( Pred( BitmapWidthBytes ) ) ); // '+': data has been written backwards
                            for X                := 0 to Pred( ColumnWidthBytes ) do
                                if ByteCountDown >  0 then begin
                                   Destination^  := Source^;
                                   Dec( Source );  // 'Dec': data has been written backwards
                                   Inc( Destination );
                                   Dec( ByteCountDown );
                                   end
                                else break;
                            end
                         else break;
                     Result := ( TotalByteSize = SizeOf( SkinCaptureInformation__.Header ) + ( SkinCaptureInformation__.Header.ItemCount * SizeOf( SkinCaptureInformation__.Items[ 0 ] ) ) )
                               and
                               ( SkinCaptureInformation__.Header.ItemCount = SkinCaptureInformation__.Header.ItemOffsets[ bstUndefined ] );
                     if not Result then begin
                        FreeMem( SkinCaptureInformation__ ); SkinCaptureInformation__ := nil;
                        end;
                     end;
                  end;
            end;
         end;
      end;
end;

function  TCaptureForm.ReadColumnsAndRows( Bitmap__ : TBitmap; var ColCount__, RowCount__ : Integer ) : Boolean;
var SkinCaptureInformation : PSkinCaptureInformation;
begin
  Result := ReadSkinCaptureInformation( Bitmap__, True, ColCount__, RowCount__, SkinCaptureInformation );
end;

function  TCaptureForm.IntegralImageValueToBoardSquareType( Value__ : TPixel; SkinCaptureInformation__ : PSkinCaptureInformation ) : TBoardSquareType;
var Index : Integer;
begin
  Result            := bstUndefined;
  if Assigned( SkinCaptureInformation__ ) then
     for Index      := 0 to Pred( SkinCaptureInformation__^.Header.ItemCount ) do
         if Value__ =  SkinCaptureInformation__^.Items[ Index ] then begin
            Result  := SkinTileIndexToBoardSquareType( Index, SkinCaptureInformation__ );
            exit; // quick-and-dirty exit when a matching item has been found
            end;
end;

function  TCaptureForm.FindBoardUsingUnscaledSkin(
            MatchingSkinBitmap__     : TBitmap;
            SkinCaptureInformation__ : PSkinCaptureInformation;
            SourceRect__             : TRect;
            MatchThresholdPct__      : Integer;
            var   BoardRect__        : TRect;
            var   Board__            : TCaptureBoard ) : Boolean;
// precondition: the integral image values have been calculated for the loaded image, i.e., 'IntegralImage', 'IntegralImage2', 'IntegralImageWidth', and 'IntegralImageHeight' are up to date
const
  BLOOM_FILTER_TABLE_BYTE_SIZE = 1024; // must be a non-negative 2^N integer
var
  Count, HighestSizeIndex, Index, Neighbor, Col, Row, X, Y : Integer;
  B : Boolean;
  Items : TCaptureBoardSquareItemSet;
  R, SquareRect, ScaledSkinBoardRect : TRect;
  Parent, Size : PPixels;
  ScaledSkinBoard : TCaptureBoard;
  BloomFilterByteVector : array[ 0 .. BLOOM_FILTER_TABLE_BYTE_SIZE - 1 ] of Byte;
  BloomFilter : TBloomFilter;

  function  GetBoardSquareTypeAtXY( X__, Y__ : Integer) : TBoardSquareType;
  var IntegralValue, IntegralValue2 : Integer;
  begin // returns the board square type (e.g., "bstBoxOnGoal") according to
        // the integral image value for the square at the given top-left
        // position;
        // precondition: the square is inside the image, e.g., the bottom-right
        // coordinates of the square don't cause a range error;
    IntegralValue          := GetIntegralImage(
                                X__,
                                Y__,
                                X__ + SkinCaptureInformation__.Header.ColumnWidth,
                                Y__ + SkinCaptureInformation__.Header.RowHeight,
                                IntegralImageWidth, IntegralImage );
    if   BloomFilterLookupNumber( BloomFilter, IntegralValue ) then
         Result            := IntegralImageValueToBoardSquareType(
                                IntegralValue,
                                SkinCaptureInformation__ )
    else Result            := bstUndefined;

    if ( Result            =  bstUndefined ) and
       Assigned( IntegralImage2 ) then begin
       IntegralValue2      := GetIntegralImage(
                                X__,
                                Y__,
                                X__ + SkinCaptureInformation__.Header.ColumnWidth,
                                Y__ + SkinCaptureInformation__.Header.RowHeight,
                                IntegralImageWidth, IntegralImage2 );
       if ( IntegralValue2 <> IntegralValue ) and
          BloomFilterLookupNumber( BloomFilter, IntegralValue2 ) then
          Result           := IntegralImageValueToBoardSquareType(
                                IntegralValue2,
                                SkinCaptureInformation__ );
        end;
  end;

  function  GetItemsAtXY( X__, Y__ : Integer) : TCaptureBoardSquareItemSet;
  begin // returns the set of board square items (e.g., "[ bsiBox, bsiFloor ]")
        // according to the integral image value for the square at the given
        // top-left position;
        // preconditions: the square is inside the image, e.g., the bottom-right
        // coordinates of the square don't cause a range error;
    Result := BoardSquareTypeToBoardSquareItemSet( GetBoardSquareTypeAtXY( X__, Y__ ) );
  end;

  function  GetItemsAtColRow( Col__, Row__ : Integer) : TCaptureBoardSquareItemSet;
  begin // returns the set of board square items (e.g., "[ bsiBox, bsiFloor ]")
        // according to the integral image value for the square;
        // preconditions
        // * the board rectangle is available in 'BoardRect__';
        // * column and row is inside the board, i.e., the coordinates doesn't
        //   cause a range error;
    Result := GetItemsAtXY( BoardRect__.Left + ( Col__ * SkinCaptureInformation__.Header.ColumnWidth ),
                            BoardRect__.Top  + ( Row__ * SkinCaptureInformation__.Header.RowHeight   ) )
  end;

  function  IsEmptyCol( Col__ : Integer ) : Boolean;
  var Row : Integer;
  begin // preconditions:
        // * the board rectangle  is  available in 'BoardRect__';
        // * the board dimensions are available in 'Board__.Width' and 'Board__.Height';
    Result := True; Row := 0;
    while Result and ( Row < Board__.Height ) do begin
          Result := GetItemsAtColRow( Col__, Row ) = [ bsiFloor ]; // only floors count as empty at this point; 'undefined' doesn't because the user must fill in the undefined squares;
          Inc( Row );
          end;
  end;

  function  IsEmptyRow( Row__ : Integer ) : Boolean;
  var Col : Integer;
  begin // preconditions:
        // * the board rectangle  is  available in 'BoardRect__';
        // * the board dimensions are available in 'Board__.Width' and 'Board__.Height';
    Result := True; Col := 0;
    while Result and ( Col < Board__.Width ) do begin
          Result := GetItemsAtColRow( Col, Row__ ) = [ bsiFloor ]; // only floors count as empty at this point; 'undefined' doesn't because the user must fill in the undefined squares;
          Inc( Col );
          end;
  end;

  // Union-Find algorithm data and functions:
  // Data      : 'Parent', 'Size', and 'HighestSizeIndex';
  // Functions : 'Initialize', 'Root', and 'Unify';
  function Initialize( Width__, Height__      : Integer;
                       var HighestSizeIndex__ : Integer;
                       var Parent__, Size__   : PPixels ) : Boolean;
  var Index : Integer;
  begin
    HighestSizeIndex__ := 0; Parent__ := nil; Size__ := nil;
    Result := ( Width__ > 0 ) and ( Height__ > 0 ) and
              CreatePixels( Width__, Height__, Parent__ ) and
              CreatePixels( Width__, Height__, Size__   );
    if Result then begin
       // make each item its own 1-element tree
       for Index := 0 to Pred( Width__ * Height__ ) do Parent__[ Index ] := Index;
       // initialize tree sizes to "0"; one could say that the initial value
       // should be "1", but it's more efficient to initialize the vector
       // elements with "0"; furthermore, the size fields are also used as flag
       // values, where a non-zero value means that the integral image value for
       // the square-sized rectangle starting at the pixel position matches one
       // of the registered square type integral image values, e.g.,
       // a "box-on-goal" value or a "player-on-floor" value;
       ZeroMemory( Addr( Size__^ ), Width__ * Height__ * SizeOf( Size__^[ 0 ] ) );
       end
    else begin
       FreeMem( Parent__ ); Parent__ := nil;
       FreeMem( Size__   ); Size__   := nil;
       end;
  end;

  function  Root( Index__ : Integer ) : Integer;
  begin // returns the root of the tree, following the 'Parent' links
    Result                 := Index__;
    while Parent[ Result ] <> Result do begin
      Parent[ Result ]     := Parent[ Parent[ Result ] ]; // path compression
      Result               := Parent[ Result ];
      end;
  end;

  procedure Unify( I__, J__ : Integer );
  begin // merges the smaller tree into the larger tree
    I__ := Root( I__ );
    J__ := Root( J__ );
    if I__ <> J__ then begin
       if Size   [ I__ ] <  Size[ J__ ] then begin
          Parent [ I__ ] := J__; Inc( Size[ J__ ], Size[ I__ ] );
          if Size[ J__ ] >  Size[ HighestSizeIndex ] then HighestSizeIndex := J__;
          end
       else begin
          Parent [ J__ ] := I__; Inc( Size[ I__ ], Size[ J__ ] );
          if Size[ I__ ] >  Size[ HighestSizeIndex ] then HighestSizeIndex := I__;
          end;
       end;
  end;

begin // 'FindBoardUsingUnscaledSkin'
  Result := False;
  Parent := nil; Size := nil;
  try
    if   Assigned( SkinCaptureInformation__ ) and
         ( SkinCaptureInformation__.Header.ColumnWidth >  0 ) and
         ( SkinCaptureInformation__.Header.RowHeight   >  0 ) and
         Assigned( IntegralImage ) and
         ( IntegralImageWidth                          >= SkinCaptureInformation__.Header.ColumnWidth ) and
         ( IntegralImageHeight                         >= SkinCaptureInformation__.Header.RowHeight   ) and
         Assigned( OriginalBitmap ) and
         ( IntegralImageWidth                          =  OriginalBitmap.Width  ) and
         ( IntegralImageHeight                         =  OriginalBitmap.Height ) and
         ( SourceRect__.Left                           <  SourceRect__.Right    ) and
         ( SourceRect__.Top                            <  SourceRect__.Bottom   ) and
         ( SourceRect__.Left                           >= 0                     ) and
         ( SourceRect__.Right                          <= IntegralImageWidth    ) and
         ( SourceRect__.Top                            >= 0                     ) and
         ( SourceRect__.Bottom                         <= IntegralImageHeight   ) and
         Initialize( IntegralImageWidth, IntegralImageHeight, HighestSizeIndex, Parent, Size ) then begin

         // increase the size of the selected rectangle a little
         with SourceRect__ do begin Dec( Left, MIN_BOARD_SQUARE_SIZE_PIXELS ); Dec( Top, MIN_BOARD_SQUARE_SIZE_PIXELS ); Inc( Right, MIN_BOARD_SQUARE_SIZE_PIXELS ); Inc( Bottom, MIN_BOARD_SQUARE_SIZE_PIXELS ); end;
         ClipRect( SourceRect__, Rect( 0, 0, OriginalBitmap.Width - 2, OriginalBitmap.Height - 2), True );

         // make a Bloom filter with the skin object values
         BloomFilterInitialize( BloomFilter, SizeOf( BloomFilterByteVector ), Addr( BloomFilterByteVector ) );
         for Index := 0 to Pred( SkinCaptureInformation__.Header.ItemCount ) do
             BloomFilterAddNumber( BloomFilter, SkinCaptureInformation__.Items[ Index ] );

         // visit each pixel position in the integral image and check if the position might be the top-left position of a board square
         for Y     := SourceRect__.Top  to  ( SourceRect__.Bottom - SkinCaptureInformation__.Header.RowHeight   ) do begin
             for X := SourceRect__.Left to  ( SourceRect__.Right  - SkinCaptureInformation__.Header.ColumnWidth ) do begin
                 if GetBoardSquareTypeAtXY( X, Y ) <> bstUndefined then begin // 'True': the integral image value for the rectangle in the image matches a skin tile, hence, this pixel position may be the top-left position of a board square
                    Index := X + ( Y * IntegralImageWidth ); // 2D coordinates to 1-dimensional vector index
                    Size[ Index ] := 1; // the pixel position is a new board square candidate

                    if X >= SkinCaptureInformation__.Header.ColumnWidth then begin // check left neighbor and unify groups
                       Neighbor := Index - SkinCaptureInformation__.Header.ColumnWidth;
                       if Size[ Neighbor ] <> 0 then Unify( Index, Neighbor );     // '<> 0' : the neighbor to the left is also a board square candidate
                       end;
                    if Y >= SkinCaptureInformation__.Header.RowHeight then begin   // check upper neighbor and unify groups
                       Neighbor := Index - ( IntegralImageWidth * SkinCaptureInformation__.Header.RowHeight );
                       if Size[ Neighbor ] <> 0 then Unify( Index, Neighbor );     // '<> 0' : the neighbor above       is also a board square candidate
                       end;
                    end;
                 end;
             end;

         Result := Size[ HighestSizeIndex ] >= 9; // '9': the smallest reasonable Sokoban level is 3 x 3 squares
         if Result then begin
            // find the board rectangle in the image
            BoardRect__ := Rect( MaxInt, MaxInt, 0 , 0 );
            for Index := 0 to Pred( IntegralImageWidth * IntegralImageHeight ) do
                if Root( Index )      =  HighestSizeIndex then begin
                   SquareRect .Top    := Index div IntegralImageWidth;
                   SquareRect .Left   := Index - ( SquareRect.Top * IntegralImageWidth );
                   SquareRect .Right  := SquareRect.Left + SkinCaptureInformation__.Header.ColumnWidth;
                   SquareRect .Bottom := SquareRect.Top  + SkinCaptureInformation__.Header.RowHeight;
                   BoardRect__.Left   := Min( BoardRect__.Left  , SquareRect.Left   );
                   BoardRect__.Top    := Min( BoardRect__.Top   , SquareRect.Top    );
                   BoardRect__.Right  := Max( BoardRect__.Right , SquareRect.Right  );
                   BoardRect__.Bottom := Max( BoardRect__.Bottom, SquareRect.Bottom );
                   end;
            // fill in the board information
            ZeroMemory( Addr( Board__ ), SizeOf( Board__ ) );
            Capture_.ClearBoard( Board__ );
            Board__.Width  := ( BoardRect__.Right  - BoardRect__.Left ) div SkinCaptureInformation__.Header.ColumnWidth;
            Board__.Height := ( BoardRect__.Bottom - BoardRect__.Top  ) div SkinCaptureInformation__.Header.RowHeight;

            // trim board for empty columns and rows
            while ( Board__.Width  > 0 ) and IsEmptyCol( 0                      ) do begin Dec( Board__.Width  ); Inc( BoardRect__.Left  , SkinCaptureInformation__.Header.ColumnWidth ); end;
            while ( Board__.Width  > 0 ) and IsEmptyCol( Pred( Board__.Width  ) ) do begin Dec( Board__.Width  ); Dec( BoardRect__.Right , SkinCaptureInformation__.Header.ColumnWidth ); end;
            while ( Board__.Height > 0 ) and IsEmptyRow( 0                      ) do begin Dec( Board__.Height ); Inc( BoardRect__.Top   , SkinCaptureInformation__.Header.RowHeight   ); end;
            while ( Board__.Height > 0 ) and IsEmptyRow( Pred( Board__.Height ) ) do begin Dec( Board__.Height ); Dec( BoardRect__.Bottom, SkinCaptureInformation__.Header.RowHeight   ); end;

            Result         := ( Board__.Width >= 3                       ) and ( Board__.Height >= 3                        ) and
                              ( Board__.Width <= CAPTURE_MAX_BOARD_WIDTH ) and ( Board__.Height <= CAPTURE_MAX_BOARD_HEIGHT );
            if Result then begin
               Count := 0;
               for Col := 0 to Pred( Board__.Width ) do
                   for Row := 0 to Pred( Board__.Height ) do begin
                       Items        := GetItemsAtColRow( Col, Row );
                       if Items     <> []    then Inc( Count ); // count identified squares
                       if bsiBox    in Items then Inc( Board__.BoxCount  );
                       if bsiGoal   in Items then Inc( Board__.GoalCount );
                       if bsiPlayer in Items then begin
                          Exclude( Board__.Squares[ Succ( Board__.PlayerPos.X ), Succ( Board__.PlayerPos.Y ) ], bsiPlayer ); // remove old player, if any; 1-based coordinates
                          Board__.PlayerPos.X := Col; // 0-based coordinates
                          Board__.PlayerPos.Y := Row;
                          end;
                       Board__.Squares[ Succ( Col ), Succ( Row ) ] := Items;
                       end;
               Result := Count >= ( ( Board__.Width * Board__.Height ) * MatchThresholdPct__ ) div 100;

               if not Result then begin // 'True': try harder; there may be so many squares outside the board (but inside the rectangle ) that it pushed the number of identified squares below the threshold
                  Timestamp := High( Timestamp );
                  Result    := CheckBoard( Board__, nil, nil, True, R, B );
                  end;
               if ( not Result ) and ( Board__.PlayerPos.X >= 0) and ( Board__.PlayerPos.Y >= 0 ) then begin // 'True': try harder; if there is a pusher on the board inside a wall contour, then the squares outside the board are empty after the call to 'CheckBoard'
                  for Col := 1 to Board__.Width do
                      for Row := 1 to Board__.Height do
                          if   ( TimeStamps[ Col, Row ] = Timestamp ) or
                               ( bsiWall in Board__.Squares[ Col, Row ] ) then begin
                               // a wall, or a floor the player can reach
                               end
                          else Inc( Count ); // a square outside the board
                  Result := Count >= ( ( Board__.Width * Board__.Height ) * MatchThresholdPct__ ) div 100;
                  end;

               if Result and ( Board__.PlayerPos.X >= 0) and ( Board__.PlayerPos.Y >= 0 ) and Assigned( MatchingSkinBitmap__ ) then begin
                  // check if the board is complete
                  CheckBoard( Board__, nil, nil, True, R , B );
                  for Col := 1 to Board__.Width do
                      for Row := 1 to Board__.Height do
                          if ( Timestamps[ Col, Row ] = Timestamp ) and         // 'True': the pusher can reach the square
                             ( Board__.Squares[ Col, Row ] = [] ) then begin    // 'True': the pusher-reachable square is undefined, i.e., the board is incomplete
                             Result := False;
                             end;
                  if not Result then begin // 'True': the board is incomplete, even though it has been accepted
                     // try to match the board and the skin, using the method required for matching when the skin needs scaling
                     Result := FindBoardUsingScaledSkin( OriginalBitmap, MatchingSkinBitmap__, SkinCaptureInformation__, BoardRect__, Board__.Width, Board__.Height, MatchThresholdPct__, ScaledSkinBoardRect, ScaledSkinBoard ) and
                               ( Board__.Width  = ScaledSkinBoard.Width ) and
                               ( Board__.Height = ScaledSkinBoard.Height );
                     if Result then begin
                        Timestamp := High( Timestamp );
                        CheckBoard( Board__, nil, nil, True, R, B  ); // recalculate timestamps for pusher-reachable squares, so they match the captured board 'Board__'
                        for Col := 1 to Board__.Width do
                            for Row := 1 to Board__.Height do
                                if ( Timestamps[ Col, Row ] = Timestamp ) and   // 'True': the pusher can reach the square
                                   ( Board__.Squares[ Col, Row ] = [] ) and     // 'True': the pusher-reachable square is undefined
                                   ( not ( bsiPlayer in ScaledSkinBoard.Squares[ Col, Row ] ) ) then
                                   Board__.Squares[ Col, Row ] := ScaledSkinBoard.Squares[ Col, Row ]; // use the board square value from the matching performed with the scaled version skin
                        end;
                     end;

                  Result := True;
                  end;
               end;
            end;
         end;
  finally Timestamp := High( Timestamp );
          FreeMem( Parent ); FreeMem( Size );
  end;
end; // FindBoardUsingUnscaledSkin

function  TCaptureForm.FindBoardUsingScaledSkin(
            Bitmap__                          : TBitmap; // input image
            MatchingSkinBitmap__              : TBitmap;
            MatchingSkinCaptureInformation__  : PSkinCaptureInformation;
            const SourceRect__                : TRect;
            ColCount__, RowCount__            : Integer;
            MatchThresholdPct__               : Integer;
            var   BoardRect__                 : TRect;
            var   Board__                     : TCaptureBoard ) : Boolean;
const
//RGB_COMPONENT_MASK : Integer = $F0; // drop 4 low bits in each rgb color component for the average colors in order to make room for gradient direction hash values
  RGB_COMPONENT_MASK = ( not 3 );
type
  TSkinTileColors = array[ 0 .. CAPTURED_SKIN_TILE_COUNT - 1 ] of TColor;
var
  Count, Index, BestIndex, Distance, MinimumDistance, MinimumPusherDistance, Col, Row, ColWidth, RowHeight, OriginalZoom{, W, H} : Integer;
  B : Boolean;
  Color : TColor;
  Items : TCaptureBoardSquareItemSet;
  ColorBitmap, MatchingSingleRowSkinBitmap : TBitmap;
  {HL, VL, II, MatchingSkinGradientDirections : PPixels;}
  R : TRect;
  SkinTileColors : TSkinTileColors;

  procedure LoadSkinTileColorsFromQuantizedColors( ColorBitmap__ : TBitmap; var SkinTileColors__ : TSkinTileColors );
  var Index : Integer; SkinTileColor : PRGBA;
  begin // // load quantized colors for the scaled skin into a table for efficient lookup
    SkinTileColor    := ColorBitmap__.Scanline[ 0 ];
    //Inc( SkinTileColor, ColCount * RowCount ); // offset of the first scaled skin tile in the color bitmap
    for Index := Low( SkinTileColors__ ) to High( SkinTileColors__ ) do begin
        SkinTileColors__[ Index ] := TColor( SkinTileColor^ );
        Inc( SkinTileColor );
        end;
  end;

  function  ColorToSkinTileIndex( Color__ : TColor; const SkinTileColors__ : TSkinTileColors ) : Integer;
  begin
    Result := Low( SkinTileColors__ );
    while ( Result  <= High( SkinTileColors__ ) ) and
          ( Color__ <> SkinTileColors__[ Result ] ) do
          Inc( Result );
    if Result > High( SkinTileColors__ ) then
       Result := - 1;
  end;

  function  ColorToBoardSquareItemSet( Color__ : TColor; const SkinTileColors__ : TSkinTileColors ) : TCaptureBoardSquareItemSet;
  begin
    Result := BoardSquareTypeToBoardSquareItemSet( SkinTileIndexToBoardSquareType( ColorToSkinTileIndex( Color__, SkinTileColors__ ), nil ) );
  end;

  function  AllBoardSquaresHaveBeenAssignedASkinTileColor( ColorBitmap__ : TBitmap ) : Boolean;
  var Index : Integer; P : PRGBA; SkinTileColors : TSkinTileColors;
  begin // 'ColorBitmap__' contains quantized colors for all board squares and scaled skin tiles;
    Result := True;
    // load quantized scaled skin tile colors into a table for efficient lookup
    LoadSkinTileColorsFromQuantizedColors( ColorBitmap__, SkinTileColors );
    // check if all board squares have been assigned a color which matches one
    // of the scaled skin tile colors
    P := ColorBitmap__.Scanline[ 0 ];
    Inc( P, CAPTURED_SKIN_TILE_COUNT ); // first color for the board squares
    for Index := 0 to Pred( ColCount__ * RowCount__ ) do
        if   Result then begin
             Result := ColorToSkinTileIndex( TColor( P^ ), SkinTileColors ) >= 0;
             Inc( P ); // advance to next board square color
             end
        else break ; // quick-and-dirty exit when a unmatched square has been found
  end;

begin // 'FindBoardUsingScaledSkin' scales the skin to fit the board dimensions,
      // and then try to match the board squares and the scaled skin by means of
      // color quantization;
  Result := False;
  ColorBitmap := nil; MatchingSingleRowSkinBitmap := nil;
  {MatchingSkinGradientDirections := nil; HL := nil; VL := nil;  II := nil;}
  if ColCount__ <= 0 then
     ColCount__ := Max( 1, ToolsForm.ColumnsSpinEdit.Value );
  if RowCount__ <= 0 then
     RowCount__ := Max( 1, ToolsForm.RowsSpinEdit   .Value );
  ColWidth      := RectWidth ( SourceRect__ ) div ColCount__;
  RowHeight     := RectHeight( SourceRect__ ) div RowCount__;
  OriginalZoom  := Editor.Zoom;
  try
    //Screen.Cursor := crHourGlass;
    Editor.Zoom := 100;

    if   Assigned( MatchingSkinBitmap__ ) and
         Assigned( MatchingSkinCaptureInformation__ ) and
         Assigned( Bitmap__ ) and
         ( MatchingSkinCaptureInformation__.Header.ColumnWidth > 0 ) and
         ( MatchingSkinCaptureInformation__.Header.RowHeight   > 0 ) and
         ( MatchingSkinBitmap__.Width  >= 4 * MatchingSkinCaptureInformation__.Header.ColumnWidth  ) and
         ( MatchingSkinBitmap__.Height >= 4 * MatchingSkinCaptureInformation__.Header.RowHeight    ) and
         ( MatchingSkinBitmap__.PixelFormat = pf24Bit ) and
         ( Bitmap__.PixelFormat = pf24Bit ) and
         ( ColCount__        >= 3 ) and
         ( RowCount__        >= 3 ) and
         ( ColCount__        <= CAPTURE_MAX_BOARD_WIDTH  ) and
         ( RowCount__        <= CAPTURE_MAX_BOARD_HEIGHT ) and
         ( ColWidth          >= MIN_BOARD_SQUARE_SIZE_PIXELS ) and
         ( RowHeight         >= MIN_BOARD_SQUARE_SIZE_PIXELS ) and
         ( SourceRect__.Left <  SourceRect__.Right  ) and
         ( SourceRect__.Top  <  SourceRect__.Bottom ) and
         ( SourceRect__.Left >= 0 ) and ( SourceRect__.Right  <= Bitmap__.Width  ) and
         ( SourceRect__.Top  >= 0 ) and ( SourceRect__.Bottom <= Bitmap__.Height ) and
         ( ColWidth          >  4 ) and ( RowHeight > 4 ) and // calculating the average color for a cell omits the 2 outer pixel columns and rows; this helps a little to make up for small inaccuracies in the user's selected region
         Bitmap32Create( ColorBitmap, ( ColCount__ * RowCount__ ) + CAPTURED_SKIN_TILE_COUNT, 1 ) and
         CreateSingleRowSkinFromSkin( MatchingSkinBitmap__       , MatchingSkinCaptureInformation__.Header.ColumnWidth, MatchingSkinCaptureInformation__.Header.RowHeight, MatchingSingleRowSkinBitmap ) and
         CreateEditorSkin           ( MatchingSingleRowSkinBitmap, MatchingSingleRowSkinBitmap.Width div MatchingSkinCaptureInformation__.Header.ColumnWidth, ColWidth, RowHeight ) {and} // scales the skin and installs it as the skin used for displaying the board
         {AnalyseImage              ( Editor.EditorSkinBitmap, W, H, [ airGradientDirections ], HL, VL, II, MatchingSkinGradientDirections )}
         then
         try    Result := True;

                // initialize board information;
                BoardRect__    := SourceRect__;
                ZeroMemory( Addr( Board__ ), SizeOf( Board__ ) );
                Capture_.ClearBoard( Board__ );
                Board__.Width      := ColCount__;
                Board__.Height     := RowCount__;
                Timestamp          := High( Timestamp );

                // calculate average colors for scaled skin tiles
                for Index := Low( SkinTileColors ) to High( SkinTileColors ) do begin
                    R := CellToRect( Index, 0, ColWidth, RowHeight );
                    SkinTileColors[ Index ] := CalculateAverageColor( Rect( R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2 ), Editor.EditorSkinBitmap );
                    end;

                // calculate average colors for board squares, and find their nearest matching skin tile color
                MinimumPusherDistance := High( MinimumPusherDistance );
                for Row := 0 to Pred( RowCount__ ) do
                    for Col := 0 to Pred( ColCount__ ) do begin
                        R.Left := SourceRect__.Left + ( Col * ColWidth  );
                        R.Top  := SourceRect__.Top  + ( Row * RowHeight );
                        Color  := CalculateAverageColor( Rect( R.Left + 2, R.Top + 2, R.Left + ColWidth - 2, R.Top + RowHeight - 2 ), Bitmap__ );
                        BestIndex := 0;
                        MinimumDistance := High( MinimumDistance );
                        for Index := Low( SkinTileColors ) to High( SkinTileColors ) do begin
                            Distance := CalculateColorDistanceSquared( Color, SkinTileColors[ Index ] );
                            if Distance < MinimumDistance then begin
                               BestIndex := Index;
                               MinimumDistance := Distance;
                               end;
                            end;
                        Items        := BoardSquareTypeToBoardSquareItemSet( SkinTileIndexToBoardSquareType( BestIndex, nil ) );
//                      if Items     <> []    then Inc( Count ); // count identified squares
                        if bsiBox    in Items then Inc( Board__.BoxCount  );
                        if bsiGoal   in Items then Inc( Board__.GoalCount );
                        if bsiPlayer in Items then begin
                           if   MinimumDistance < MinimumPusherDistance then begin // 'True': first or new best pusher match
                                MinimumPusherDistance := MinimumDistance;
                                Exclude( Board__.Squares[ Succ( Board__.PlayerPos.X ), Succ( Board__.PlayerPos.Y ) ], bsiPlayer ); // remove old player, if any; 1-based coordinates
                                Board__.PlayerPos.X := Col; // 0-based coordinates
                                Board__.PlayerPos.Y := Row;
                                end
                           else Items := [ bsiFloor ] ; // earlier pusher match was better; drop this one
                           end;
                        Board__.Squares[ Succ( Col ), Succ( Row ) ] := Items; // 1-based coordinates
                        end;
                Result := CheckBoard( Board__, nil, nil, True, R, B );

                if not Result then begin // no complete board found; accept the board anyway if there is a reasonable number of filled squares
                   Count := 0;
                   for Row := 1 to Board__.Height do
                       for Col := 1 to Board__.Width do
                           if Board__.Squares[ Col, Row ] <> [] then
                              Inc( Count );
                   Result := Count >= ( ( Board__.Width * Board__.Height ) * MatchThresholdPct__ ) div 100;
                   if ( not Result ) and ( Board__.PlayerPos.X >= 0) and ( Board__.PlayerPos.Y >= 0 ) then begin // 'True': try harder; if there is a pusher on the board inside a wall contour, then the squares outside the board are empty after the call to 'CheckBoard'
                      for Col := 0 to Pred( Board__.Width ) do
                          for Row := 0 to Pred( Board__.Height ) do
                              if   ( TimeStamps[ Col, Row ] = Timestamp ) or
                                   ( bsiWall in Board__.Squares[ Col, Row ] ) then begin
                                   // a wall, or a floor the player can reach
                                   end
                              else Inc( Count ); // a square outside the board
                      Result := Count >= ( ( Board__.Width * Board__.Height ) * MatchThresholdPct__ ) div 100;
                      end;
                   end;

         except on E:Exception do Result := Error( E.Message, '' );
         end
  finally //Screen.Cursor := crDefault;
          Editor.Zoom := OriginalZoom;
          Colors.Count := 0;
          Timestamp := High( Timestamp );
          ColorBitmap.Free; MatchingSingleRowSkinBitmap.Free;
          {FreeMem( MatchingSkinGradientDirections ); FreeMem( HL ); FreeMem( VL ); FreeMem( II );}
          Editor.EditorSkinBitmap.Free; Editor.EditorSkinBitmap := nil; Editor.EditorSkinTileCount := 0; // recreate the skin used for displaying the board
          ShowImage( Editor.Zoom );
  end;
end; // FindBoardUsingScaledSkin

function  TCaptureForm.FindBoardUsingMatchingSkin( const FileName__ : String; MatchThresholdPct__ : Integer; ShowMessages__ : Boolean ) : Boolean;
var
  i : Integer;
  B : Boolean;
  //TimeMS : TTimeMS;
  OriginalZoom : Integer; OriginalInputFileName : String;
  R : TRect;
  CapturedBoardRect : TRect; CapturedBoard : TCaptureBoard;
  MatchingSkinBitmap : TBitmap;                             // the skin used for extracting a board from the loaded image
  MatchingSkinCaptureInformation : PSkinCaptureInformation; // skin capture information from the matching skin image

  function  Msg( const Text__, RemoveFileName__ : String ) : Boolean;
  var Index : Integer;
  begin
    Result := False;
    if ShowMessages__ then SokUtil_.Msg( Text__, ToolsForm.Caption , MB_ICONINFORMATION + MB_OK );
    if RemoveFileName__ <> '' then begin
       Index := SkinsComboBox.Items.IndexOf( AbbreviatedFilePath( RemoveFileName__, MainForm.MyDocumentsFolder ) );
       if Index >= 0 then SkinsComboBox.Items.Delete( Index );
       end;
  end;

begin // 'FindBoardUsingMatchingSkin'
  Result := False; MatchingSkinBitmap := nil; MatchingSkinCaptureInformation := nil;
  //TimeMS := GetTimeMS;
  try
     if FileName__  <> '' then begin
        if   FileExists( FileName__ ) then
             if   LoadImageFromFile( FileName__, MatchingSkinBitmap ) then
                  if   ReadSkinCaptureInformation( MatchingSkinBitmap, False, i, i, MatchingSkinCaptureInformation ) then begin

                       if        Editor.CompletedStep >= csBoard then
                                 // board position and size is known
                            if   Editor.CompletedStep >= csColumnsRows then
                                 // board position and size if known, and so it the number of columns and rows;
                                           // first try the unscaled skin
                                 Result := FindBoardUsingUnscaledSkin(                 MatchingSkinBitmap, MatchingSkinCaptureInformation  , NormalizedBoardRect        ,       MatchThresholdPct__, CapturedBoardRect, CapturedBoard )
                                           or
                                           // then try to scale the skin so it fits the column with and row height entered by the user
                                           FindBoardUsingScaledSkin  ( OriginalBitmap, MatchingSkinBitmap, MatchingSkinCaptureInformation  , NormalizedBoardRect        , 0, 0, MatchThresholdPct__, CapturedBoardRect, CapturedBoard )
                            else
                                 // board position and size is known, but not the number of columns and rows
                                 Result := FindBoardUsingUnscaledSkin(                 MatchingSkinBitmap, MatchingSkinCaptureInformation  , NormalizedBoardRect        ,       MatchThresholdPct__, CapturedBoardRect, CapturedBoard )
                                           or
                                           FindBoardUsingUnscaledSkin(                 MatchingSkinBitmap, MatchingSkinCaptureInformation  , Rect( 0, 0, OriginalBitmap.Width - 2, OriginalBitmap.Height - 2 ),
                                                                                                                                                                                MatchThresholdPct__, CapturedBoardRect, CapturedBoard )
                       else      // the board dimensions and the number of columns and rows are unknown
                                 Result := FindBoardUsingUnscaledSkin(                 MatchingSkinBitmap, MatchingSkinCaptureInformation  , Rect( 0, 0, OriginalBitmap.Width - 2, OriginalBitmap.Height - 2 ),
                                                                                                                                                                                MatchThresholdPct__, CapturedBoardRect, CapturedBoard );

                       if Result then begin
                          OriginalZoom := Editor.Zoom;
                          OriginalInputFileName := Editor.InputFileName;
                          Clear;
                          Editor.InputFileName := OriginalInputFileName;
                          Editor.Zoom := OriginalZoom;
                          Editor.Step := csBoard; // step: board
                          ToolsForm.LeftSpinEdit.Value := CapturedBoardRect.Left;
                          ToolsForm.TopSpinEdit.Value := CapturedBoardRect.Top;
                          ToolsForm.WidthSpinEdit.Value := RectWidth( CapturedBoardRect );
                          ToolsForm.HeightSpinEdit.Value := RectHeight( CapturedBoardRect );
                          NextStepButtonClick ( Self ); // step : grid
                          ToolsForm.ColumnsSpinEdit.Value := CapturedBoard.Width;
                          ToolsForm.RowsSpinEdit.Value := CapturedBoard.Height;
                          if ToolsForm.ColWidthSpinEdit.Value  *  ToolsForm.ColumnsSpinEdit  .Value <>  ToolsForm.WidthSpinEdit .Value then begin
                             ToolsForm.ColWidthSpinEdit.Value  := ToolsForm.WidthSpinEdit    .Value div Max( 1, ToolsForm.ColumnsSpinEdit.Value ); // just to be sure
                             ToolsForm.WidthSpinEdit   .Value  := ToolsForm.ColWidthSpinEdit .Value *   ToolsForm.ColumnsSpinEdit         .Value;
                             end;
                          if ToolsForm.RowHeightSpinEdit.Value *  ToolsForm.RowsSpinEdit     .Value <>  ToolsForm.HeightSpinEdit.Value then begin
                             ToolsForm.RowHeightSpinEdit.Value := ToolsForm.HeightSpinEdit   .Value div Max( 1, ToolsForm.RowsSpinEdit    .Value ); // just to be sure
                             ToolsForm.HeightSpinEdit   .Value := ToolsForm.RowHeightSpinEdit.Value *   ToolsForm.RowsSpinEdit            .Value;
                             end;
                          ToolsForm.CaptureScrollBox.VertScrollBar.Position := 0;
                          NextStepButtonClick ( Self ); // step: board squares
                          Result := BeginTransaction;
                          if Result then begin
                             Editor.Board := CapturedBoard;
                             Result := EndTransaction( True );
                             end;
                          ScrollInView( ScaledBoardRect );
                          ShowBoard;
                          if Result then begin
                             if ( MatchingSkinCaptureInformation.Header.ColumnWidth <> ToolsForm.ColWidthSpinEdit .Value ) or
                                ( MatchingSkinCaptureInformation.Header.RowHeight   <> ToolsForm.RowHeightSpinEdit.Value ) then
                                MatchingSkinFileName := '' // the matching skin was scaled to fit the board extracted from the image; this mean the matching skin isn't identical to the one which can be produced from the currently loaded image
                             else begin
                                MatchingSkinFileName := FileName__;
                                LoadSettingsFromBitmap( MatchingSkinBitmap );
                                ToolsForm.WallCapCheckBox.Tag  := 1; // '1': 'checked' is a user value, or a value from a matching skin
                                end;
                             if CheckBoard( Editor.Board, ToolsForm.CaptureImage1.Picture.Bitmap, BackgroundBitmap, False, R, B )
                                //and
                                //( MatchingSkinCaptureInformation.Header.ColumnWidth = ToolsForm.ColWidthSpinEdit .Value )
                                //and
                                //( MatchingSkinCaptureInformation.Header.RowHeight   = ToolsForm.RowHeightSpinEdit.Value ) // if matching with a scaled skin is unreliable, then only advance when it was the unscaled skin matched; (empirical results: matching with scaled skins seems to be be good enough, so there is no need to differentiate between the two types of matching here)
                                then begin
                                Editor.CompletedStep := csBoardSquares;
                                NextStepButtonClick ( Self ); // step: save puzzle
                                end;
                             end
                          else Msg( TEXT_TASK_FAILED, '' );
                          end
                       else // matching failed
                          if   Editor.CompletedStep < csColumnsRows then
                               Msg( Format( TEXT_FORMAT_EXTRACT_BOARD_USING_SKIN_FAILED, [ AbbreviatedFilePath( FileName__, MainForm.MyDocumentsFolder ) ] ) + NL + NL + TEXT_EXTRACT_BOARD_USING_UNSCALED_SKIN_FAILED_HINT, '' )
                          else Msg( Format( TEXT_FORMAT_EXTRACT_BOARD_USING_SKIN_FAILED, [ AbbreviatedFilePath( FileName__, MainForm.MyDocumentsFolder ) ] ), '' );
                       end
                  else Msg( Format( TEXT_FORMAT_IMAGE_HAS_NO_SKIN_CAPTURE_INFORMATION, [ AbbreviatedFilePath( FileName__, MainForm.MyDocumentsFolder ) ] ), FileName__ )
             else Msg( Format( OpenFileFailedShortText__, [ FileName__ ] ), FileName__ )
        else Msg( Format( TEXT_FILE_NOT_FOUND_FORMAT, [ FileName__ ] ), FileName__ );
        end
     else if ( not ShowMessages__ ) and
             // 'ShowMessages__' = 'False' entails the extraction has been
             // launched automatically by the application when the user opened
             // an image; try to use the information from the last capture task,
             // if any;
             FileExists( DefaultMatchingSkinFileName ) then
             Result := FindBoardUsingMatchingSkin( DefaultMatchingSkinFileName, MatchThresholdPct__, ShowMessages__ );
   finally MatchingSkinBitmap.Free; FreeMem( MatchingSkinCaptureInformation );
           //if not Result then MatchingSkinFileName := '';
   end;
   //if not Result and ( FileName__ <> '' ) then
   //   Index := SkinsComboBox.Items.IndexOf( AbbreviatedFilePath( FileName__,MainForm.MyDocumentsFolder ) );
   //   if Index >= 0 then SkinsComboBox.Delete( Index );
   //   end;
   //TimeMS := CalculateElapsedTimeMS( TimeMS, GetTimeMS );
   //SokUtil_.Msg( IntToStr( TimeMS ), ToolsForm.Caption , MB_ICONINFORMATION + MB_OK );
end; // FindBoardUsingMatchingSkin

function  TCaptureForm.BoardSquareTypeToBoardSquareItemSet( BoardSquareType__ : TBoardSquareType ) : TCaptureBoardSquareItemSet;
begin
  case BoardSquareType__ of
    bstUndefined          : Result := [];
    bstBox                : Result := [ bsiBox, bsiFloor ];
    bstBoxOnGoal          : Result := [ bsiBox, bsiGoal, bsiFloor ];
    bstFloor              : Result := [ bsiFloor ];
    bstGoal               : Result := [ bsiGoal, bsiFloor ];
    bstPlayer             : Result := [ bsiPlayer, bsiFloor ];
    bstPlayerOnGoal       : Result := [ bsiPlayer, bsiGoal, bsiFloor ];
    bstWall               : Result := [ bsiWall ];
  end;
end;

function  TCaptureForm.SkinTileIndexToBoardSquareType( Index__ : Integer; SkinCaptureInformation__ : PSkinCaptureInformation ) : TBoardSquareType;
begin
  if   Index__ >= 0 then
       case Cardinal( Index__ ) of // 'Cardinal': for efficient case dispatch; the compiler can ignore negative integers
         0 .. 15                            : Result := bstWall;
         16                                 : Result := bstWall;    // wall cap; it's not a square itself but only a graphic floating on top of 4 wall squares; even so, it's treated as a wall here because its graphic may be identical to a wall element, e.g., when the skin has a tiled wall as opposed to a seamless wall
         17                                 : Result := bstFloor;
         18                                 : Result := bstPlayer;
         19                                 : Result := bstBox;
         20                                 : Result := bstGoal;
         21                                 : Result := bstPlayerOnGoal;
         22                                 : Result := bstBoxOnGoal;
         23 .. 26                           : Result := bstPlayer;       // directional player
         27 .. CAPTURED_SKIN_TILE_COUNT - 1 : Result := bstPlayerOnGoal; // directional player-on-goal
         else  if   Assigned( SkinCaptureInformation__ ) then begin
                    Result          := High( SkinCaptureInformation__^.Header.ItemOffsets ); // 'High': the items are sorted in descending order on the board square type
                    while ( Result  >  Low( Result ) ) and // 'Low' = 'bstUndefined ', i.e., the index is out of bounds if the 'while' loop hits the bottom
                          ( Index__ >= SkinCaptureInformation__^.Header.ItemOffsets[ Pred( Result ) ] ) do
                          Dec( Result );
                   end
              else Result := bstUndefined;
       end
  else Result := bstUndefined;
end;

function  TCaptureForm.BoardSquareItemSetToSkinTileIndex( Items__ : TCaptureBoardSquareItemSet ) : Integer;
begin
  if                       bsiWall   in Items__ then
                           Result    := 0
  else if                  bsiBox    in Items__ then
            if             bsiGoal   in Items__ then
                           Result    := 22
            else           Result    := 19
       else if             bsiGoal   in Items__ then
                 if        bsiPlayer in Items__ then
                           Result    := 30
                 else      Result    := 20
            else if        bsiPlayer in Items__ then
                           Result    := 26
                 else if   bsiFloor  in Items__ then
                           Result    := 17
                      else Result    := -1;
end;

function  TCaptureForm.BoardSquareToSkinTileIndex( Col__, Row__ : Integer; const Board__ : TCaptureBoard ) : Integer;
var Items : TCaptureBoardSquareItemSet;
begin
  Items := Board__.Squares[ Col__, Row__ ];
  if    bsiWall in Items then
        Result := NeighborWalls( Col__, Row__, Board__ )
  else  Result := BoardSquareItemSetToSkinTileIndex( Items );
end;

function  TCaptureForm.LoadSettingsFromBitmap( Bitmap__ : TBitmap ) : Boolean;
var Value, ColWidth, RowHeight : Integer; Text : String;
    SkinPixelCodes : TSkinPixelCodes; Strings : TStringList;
begin
  Result    := False;
  ColWidth  := ToolsForm.ColWidthSpinEdit.Value;
  RowHeight := ToolsForm.RowHeightSpinEdit.Value;
  if ( Bitmap__.Width  >= 4 * ColWidth ) and
     ( Bitmap__.Height >= 4 * RowHeight ) then
     try
       Strings := TStringList.Create;
       try
         ZeroMemory( Addr( SkinPixelCodes ), SizeOf( SkinPixelCodes ) );
         if MainForm.Skins.GetPixelCodes( Bitmap__, 3, 3, ColWidth, RowHeight, SkinPixelCodes ) then begin // get wall cut information from pixel codes
            ToolsForm.OuterWallCutLeftSpinEdit  .Value := SkinPixelCodes.LineLengths[ rsBottom, 0 ];
            ToolsForm.OuterWallCutTopSpinEdit   .Value := SkinPixelCodes.LineLengths[ rsBottom, 2 ];
            ToolsForm.OuterWallCutRightSpinEdit .Value := SkinPixelCodes.LineLengths[ rsBottom, 1 ];
            ToolsForm.OuterWallCutBottomSpinEdit.Value := SkinPixelCodes.LineLengths[ rsBottom, 3 ];
            end;

         if GetBitMapText( Bitmap__, CellToRect( 3, 3, ColWidth, RowHeight ), True, Strings, Text ) and
            InitializeIniFileReader then begin
            // get wall cut information from text
            Text := IniFile_.ReadString( Strings, EQUAL, INIFILE_SECTION_WALL_TRIMMING, KEY_RECTANGLE_SIDES[   rsLeft   ], '' );
            if ( Text <> '' ) and SafeStrToInt( Text, False, Value ) and ( Value < ColWidth  div 2 ) then ToolsForm.OuterWallCutLeftSpinEdit  .Value := Value;
            Text := IniFile_.ReadString( Strings, EQUAL, INIFILE_SECTION_WALL_TRIMMING, KEY_RECTANGLE_SIDES[   rsTop    ], '' );
            if ( Text <> '' ) and SafeStrToInt( Text, False, Value ) and ( Value < RowHeight div 2 ) then ToolsForm.OuterWallCutTopSpinEdit   .Value := Value;
            Text := IniFile_.ReadString( Strings, EQUAL, INIFILE_SECTION_WALL_TRIMMING, KEY_RECTANGLE_SIDES[   rsRight  ], '' );
            if ( Text <> '' ) and SafeStrToInt( Text, False, Value ) and ( Value < ColWidth div 2 ) then ToolsForm.OuterWallCutRightSpinEdit .Value := Value;
            Text := IniFile_.ReadString( Strings, EQUAL, INIFILE_SECTION_WALL_TRIMMING, KEY_RECTANGLE_SIDES[   rsBottom ], '' );
            if ( Text <> '' ) and SafeStrToInt( Text, False, Value ) and ( Value < RowHeight div 2 ) then ToolsForm.OuterWallCutBottomSpinEdit.Value := Value;
            // get wall cap information
            Text := IniFile_.ReadString( Strings, EQUAL, INIFILE_SECTION_WALL, KEY_WALL_TYPE, '' );
            ToolsForm.WallCapCheckBox.Checked := not StrEqual( Text, WallTypeText[ wtSeamlessWallNoCap ] )
            end;
       finally InitializeIniFileReader; // reset the key-value string reader
               Strings.Free;
       end;
     except on E:Exception do Result := Error( E.Message, '' );
     end;
end;

procedure TCaptureForm.SettingsMenuItemDefaultSkinClick( Sender : TObject );
begin
  EditorSkinFileName := '';
  try    if   BitmapToImageList( CaptureButtonsAndSkinImage.Picture.Bitmap, CaptureButtonsAndSkinImage.Picture.Bitmap, ToolsForm.CaptureImageList1 ) then begin
              Editor.EditorSkinBitmap.Free; Editor.EditorSkinBitmap := nil; Editor.EditorSkinTileCount := 0;
              ShowImage( Editor.Zoom );
              end
         else Error(TEXT_TASK_FAILED,Application.Title);
  except on E:Exception do Error( E.Message, '' );
  end;
end;

procedure TCaptureForm.SettingsMenuItemSkinClick( Sender : TObject );
const MIN_TILE_WIDTH=16; MIN_TILE_HEIGHT=MIN_TILE_WIDTH;
var DefaultSkinFileName,oFileName:String; B:TBitMap;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if ToolsForm.Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else with ToolsForm.OpenPictureDialog1 do begin
       Title:=Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+SkinText;
       DefaultExt := GraphicExtension(TBitmap);
       Filter := GraphicFilter(TBitmap);

       DefaultSkinFileName:=StrWithTrailingPathDelimiter( ToolsSkinPath ) + StrWithBrackets( TEXT_DEFAULT_EDITOR_DEFAULT_SKIN_FILE_NAME ) + BMP_FILE_EXT;
       try
         if (Sender<>nil) then
            try    if FileExists(DefaultSkinFileName) then DeleteFile(DefaultSkinFileName);
                   if not FileExists(DefaultSkinFileName) then
                      CaptureButtonsAndSkinImage.Picture.BitMap.SaveToFile(DefaultSkinFileName);
            except on E:Exception do Error(E.Message,Caption);
            end;

         if   EditorSkinFileName='' then
              FileName:=DefaultSkinFileName
         else FileName:=EditorSkinFileName;
         oFileName:=FileName;
         InitialDir:=ExtractFilePath(FileName);

         if ((Sender=nil)
             or
             Execute
            )
            and
            (FileName<>'')
            and
            (FileExists(FileName)
             or
             StrEqual(FileName,DefaultSkinFileName)
            ) then begin
            Refresh;
            if StrEqual(FileName,DefaultSkinFileName) then
               SettingsMenuItemDefaultSkinClick(Sender)
            else
               if BitMapCreate(B,1,1) then
                  try     try
                                 B.LoadFromFile(FileName);
                                 if (B.Height>=MIN_TILE_HEIGHT) and
                                    (B.Width >=MIN_TILE_WIDTH*SINGLE_ROW_SKIN_TILE_COUNT) and
                                    (B.Width = SINGLE_ROW_SKIN_TILE_COUNT*B.Height) then begin
                                    if BitmapToImageList( B, CaptureButtonsAndSkinImage.Picture.Bitmap, ToolsForm.CaptureImageList1 ) then begin
                                       EditorSkinFileName := FileName;
                                       end
                                    else begin
                                       Error(TEXT_TASK_FAILED,Application.Title);
                                       SettingsMenuItemDefaultSkinClick(Sender);
                                       end;
                                    end
                                 else begin
                                    Msg(Format(FileNotALegalCaptureToolEditorSkinText__, [FileName]),Caption,MB_OK+MB_ICONINFORMATION);
                                    SettingsMenuItemDefaultSkinClick(Sender);
                                    end;
                          except on E:Exception do begin
                                    Error(E.Message,Caption);
                                    SettingsMenuItemDefaultSkinClick(Sender);
                                    end;
                          end;
                  finally B.Free;
                          Editor.EditorSkinBitmap.Free; Editor.EditorSkinBitmap := nil; Editor.EditorSkinTileCount := 0;
                          ShowImage( Editor.Zoom );
                  end;
            end;
       finally
         if Sender<>nil then DeleteFile(DefaultSkinFileName);
       end;
       end;
end;

procedure TCaptureForm.SetSkinHints;
begin
  if Editor.CompletedStep < csColumnsRows then with ToolsForm do begin
     CaptureEditMenuItemMatchingSkin           .Hint := TEXT_HINT_MATCHING_SKIN_EXTRACT_BOARD_FROM_IMAGE + SPACE + TEXT_HINT_MATCHING_SKIN_MUST_HAVE_IDENTICAL_DIMENSIONS + SPACE + TEXT_HINT_MATCHING_SKIN_MAY_BE_UPDATED;
     CaptureEditMenuItemMatchingSkinLoadSkin   .Hint := TEXT_HINT_MATCHING_SKIN_LOAD_SKIN                + SPACE + TEXT_HINT_MATCHING_SKIN_MUST_HAVE_IDENTICAL_DIMENSIONS + SPACE + TEXT_HINT_MATCHING_SKIN_MAY_BE_UPDATED;
     CaptureEditMenuItemMatchingSkinRecentSkins.Hint := TEXT_HINT_MATCHING_SKIN_LOAD_RECENTLY_USED_SKIN  + SPACE + TEXT_HINT_MATCHING_SKIN_MUST_HAVE_IDENTICAL_DIMENSIONS + SPACE + TEXT_HINT_MATCHING_SKIN_MAY_BE_UPDATED;
     end
  else with ToolsForm do begin
     CaptureEditMenuItemMatchingSkin           .Hint := TEXT_HINT_MATCHING_SKIN_EXTRACT_BOARD_FROM_IMAGE + SPACE + TEXT_HINT_MATCHING_SKIN_MAY_HAVE_DIFFERENT_DIMENSIONS  + SPACE + TEXT_HINT_MATCHING_SKIN_MAY_BE_UPDATED;
     CaptureEditMenuItemMatchingSkinLoadSkin   .Hint := TEXT_HINT_MATCHING_SKIN_LOAD_SKIN                + SPACE + TEXT_HINT_MATCHING_SKIN_MAY_HAVE_DIFFERENT_DIMENSIONS  + SPACE + TEXT_HINT_MATCHING_SKIN_MAY_BE_UPDATED;
     CaptureEditMenuItemMatchingSkinRecentSkins.Hint := TEXT_HINT_MATCHING_SKIN_LOAD_RECENTLY_USED_SKIN  + SPACE + TEXT_HINT_MATCHING_SKIN_MAY_HAVE_DIFFERENT_DIMENSIONS  + SPACE + TEXT_HINT_MATCHING_SKIN_MAY_BE_UPDATED;
     end;

  ToolsForm.SkinBitBtn                         .Hint := ToolsForm.CaptureEditMenuItemMatchingSkin           .Hint;
  ToolsForm.CaptureMenuItemLoadSkin            .Hint := ToolsForm.CaptureEditMenuItemMatchingSkinLoadSkin   .Hint;
  ToolsForm.CaptureMenuItemRecentSkins         .Hint := ToolsForm.CaptureEditMenuItemMatchingSkinRecentSkins.Hint;
  ToolsForm.SkinBitBtn2                        .Hint := ToolsForm.SkinBitBtn                                .Hint;
  ToolsForm.SkinBitBtn3                        .Hint := ToolsForm.SkinBitBtn                                .Hint;
end;

end.

