unit Misc_;

interface

{$WARNINGS OFF}
uses Windows,Classes,Forms,Graphics,Grids,Controls,StdCtrls,ComCtrls,Dialogs,Messages,FileCtrl,
     IniFile_,SokUtil_,SokFile_;
{$WARNINGS ON}
const
  ACCEL_CHAR_NUM_UP            = '8';
  ACCEL_CHAR_NUM_DOWN          = '2';
  ACCEL_CHAR_NUM_LEFT          = '4';
  ACCEL_CHAR_NUM_RIGHT         = '6';
  ACCEL_CHAR_NUM_NEW           = '7';
  ACCEL_CHAR_NUM_OPEN_PRIOR    = '9';
  ACCEL_CHAR_NUM_OPEN_PRIOR1   = '-';
  ACCEL_CHAR_NUM_REDO_ALL      = '1';
  ACCEL_CHAR_NUM_OPEN_NEXT     = '3';
  ACCEL_CHAR_NUM_OPEN_NEXT1    = '+';
  ACCEL_CHAR_NUM_UNDO1         = '.';
  ACCEL_CHAR_NUM_UNDO2         = ',';
  ACCEL_CHAR_NUM_REDO          = '0';

  VK_ALT                       = VK_MENU; // sic
  VK_BROWSER_BACK              = $A6;

  ALL_FILES_FILTER             = '*.*';
  APPLICATION_MUTEX_NAME       = 'BDSokobanYASCMutex'; // don't localize

  BMP_FILE_EXT                 = '.bmp';
  CD_AUDIO_FILE_EXT            = '.cda';
  DLL_FILE_EXT                 = '.dll';
  EDITOR_LOG_FILE_EXT          = '.sed'; // sokoban editor logfile (transaction history)
  GIF_FILE_EXT                 = '.gif';
  INI_FILE_EXT                 = '.ini';
  JPG_FILE_EXT                 = '.jpg';
  LOG_FILE_EXT                 = '.log';
  PALETTE_FILE_EXT             = '.pal';
  PLAYLIST_FILE_EXT            = '.mpl';
  PNG_FILE_EXT                 = '.png';
  RAR_FILE_EXT                 = '.rar';
  RTF_FILE_EXT                 = '.rtf';
  SETTINGS_FILE_EXT            = '.set';
  SKIN_SCRIPT_FILE_EXT         = '.sks';
  SOKOBAN_FOR_WINDOWS_SKIN_FILE_EXT
                               = '.skn';
  TEMPORARY_FILE_EXT           = '.$$$';
  TEXT_FILE_EXT                = '.txt';
  TIF_FILE_EXT                 = '.tif';
  WAV_FILE_EXT                 = '.wav';
  WMF_FILE_EXT                 = '.wmf';
  ZIP_FILE_EXT                 = '.zip';

  BITMAP_FILES_FILTER          = '*.bmp';
  CD_AUDIO_TRACK_TEXT          = 'Track'; // don't localize
  COLOR_AQUAMARINE             = $D4FF7F; // BGR, not RGB
  COLOR_LEMONCHIFFON           = $CDFAFF; // BGR, not RGB
  COMMAND_LINE_PARAMETER_DOCUMENTS_FOLDER
                               = '/documents';
  COMMAND_LINE_PARAMETER_INSTALL
                               = '/install'; // don't localize
  DEFAULT_APPLICATION_FOLDER_NAME
                               = 'BDSokobanYASC'; // must match the folder name in the installation program ('bd' is an attempt to guard against name-clashes)
  DEFAULT_CURSOR : TCursor     = crArrow; //crDefault; //crArrow;
  DEFAULT_DUPLICATE_LEVELS_BACKGROUND_COLOR_1
                               = $D4F1EC; // BGR, not RGB
  DEFAULT_DUPLICATE_LEVELS_BACKGROUND_COLOR_2
                               = $B8D1CD; // BGR, not RGB
  DEFAULT_GAME_WINDOW_FONT_SIZE: array[Boolean] of Integer = (8,10); // for low-resolution screen and high-resolution screen respectively
//DEFAULT_SCREEN_SAVER_AND_MONITOR_POWER_ENABLED = False;
  DEFAULT_SCREEN_SAVER_ENABLED = False;
  DEFAULT_SHADOW_COLOR_ENABLED = TColor($604040); // BGR, not RGB; grey-blue shadow
  DEFAULT_SHADOW_COLOR_FOCUSED = TColor($80FFFF); // BGR, not RGB; light-yellow shadow
  DEFAULT_TILE_GRIDLINE_WIDTH  = 1;
  DEFAULT_MUSIC_FILES_FILTER   = '*.mid;*.rmi;*.mp3;*.wma;*.cda'; //'*.mid;*.rmi;*.wav;*.mp3;*.wma;*.cda';
  DEFAULT_SOUND_FILES_FILTER   = '*.wav';
  DEFAULT_WINDOWS_98_COLOR_BTN_FACE
                               = clSilver;
  DEFAULT_WINDOWS_VISTA_COLOR_BTN_FACE
                               = TColor($F0F0F0); //  BGR, not RGB;
  DEFAULT_WINDOWS_XP_COLOR_BTN_FACE
                               = TColor($D8E9EC); //  BGR, not RGB;
  DOCUMENTS_DIRECTORY          = 'Documents';
  ELLIPSES                     = '...';
  ENLARGE_BUTTONS_AND_STATUS_BAR_IMAGES_WINDOW_WIDTH_THRESHOLD
                               = 800;
  ENLARGE_BUTTONS_AND_STATUS_BAR_IMAGES_WINDOW_HEIGHT_THRESHOLD
                               = 600;
  ES_AWAYMODE_REQUIRED         = $00000040; // ES_XXX: SetThreadExecutionState flags
  ES_CONTINUOUS                = $80000000;
  ES_DISPLAY_REQUIRED          = $00000002;
  ES_SYSTEM_REQUIRED           = $00000001;
  FONT_HEIGHT_TEST_STRING      = 'ABCGgMQqWwYy';
  FORMAT_BOARD_DIMENSIONS_AND_BOXES
                               = '[%d x %d - %d]'; // '[Columns x Rows - Boxes]' or '[Rows x Columns - Boxes]'
  FORMAT_BOARD_DIMENSIONS_AND_BOXES_AND_FLOORS
                               = '[%d x %d - %d - %d]'; // '[Columns x Rows - Boxes - Floors]' or '[Rows x Columns - Boxes - Floors]'
  FORMAT_BOARD_DIMENSIONS      = '%d x %d'; // 'Columns x Rows' or 'Rows x Columns'
  FORMAT_BOARD_POSITION        = '[%d:%d]';
  FORMAT_IMAGE_POSITION_AND_SELECTION
                               = '[%d:%d] - %dx%d';
  FORMAT_SPECIFIER_START       = '%';
  FORMAT_SPECIFIER_DECIMAL     = 'd';
  FRACTALS_DIRECTORY           = 'Fractals';
  HALF_MILLION                 = 500*1000;
  HIGH_RESOLUTION_SCREEN_WIDTH_THRESHOLD
                               = 1280; // a screen resolution less than this threshold is considered a low-resolution screen
  HKCU                         = 'HKEY_CURRENT_USER';
  HKCU_SOFTWARE                = 'Software';
  HKLM                         = 'HKEY_LOCAL_MACHINE';
  IMAGE_FILES_FILTER           = '*.bmp;*.jpg;*.jpeg;*.png;*.emf;*.wmf';
  KEY_CURRENT_VERSION          = 'CurrentVersion';
  KEY_DATE                     = 'Date';
  KEY_MEDIAPATH                = 'MediaPath';
  MAX_BOOKMARKS                = 100;
  MIN_FPS                      = 15;  // minimum acceptable frames per second
  MIN_MAIN_WINDOW_WIDTH        = 330;
  MIN_MAIN_WINDOW_HEIGHT       = 250;
  MOUSE_WHEEL_UP_DOWN_SCROLL_LINES
                               = 3;
  MPLAYER_DISPLAY_TEXT_MAX_SPEED_PPS
                               = 40;            // max 40 pixels per second
  MPLAYER_FRAME_TIME_SLOW_MS   : Cardinal = 40; // 40 milliseconds; i.e., 25 fps
  MPLAYER_FRAME_TIME_FAST_MS   : Cardinal = 20; // 20 milliseconds; i.e., 50 fps
  MPLAYER_POSITION_FRAME_TIME_MS
                               : Cardinal = 500; // 500 milliseconds; i.e., update 2 times per second

  MSG_CLOSE                    = WM_USER+$1101;
  MSG_GENERATOR_REFRESH        = WM_USER+$1102;
  MSG_MAXIMIZE                 = WM_USER+$1103;
  MSG_MPLAYER_ON_IDLE          = WM_USER+$1104;
  MSG_OPTIMIZER_REFRESH        = WM_USER+$1105;
  MSG_PLUGIN                   = WM_USER+$1106;
  MSG_REFRESH                  = WM_USER+$1107;
  MSG_REPLAY                   = WM_USER+$1108;
  MSG_SHOW_DEADLOCKED_BOXES    = WM_USER+$1109;
  MSG_SHUTDOWN                 = WM_USER+$110A;
  MSG_SOLVER_REFRESH           = WM_USER+$110B;
  MSG_STATUS                   = WM_USER+$110C;
  MSG_TEST                     = WM_USER+$110D;
  MSG_UPDATE_DEAD_SQUARES      = WM_USER+$110E;

  ONE_THOUSAND                 = 1000;
  ONE_KILO                     = 1024;
  ONE_MEBI                     = ONE_KILO*ONE_KILO;
  ONE_MILLION                  = ONE_THOUSAND*ONE_THOUSAND;

  PALETTE_FILES_FILTER         = '*.pal';
  PLUGIN_FILES_FILTER          = '*.dll';
  REGINIFILE_WINDOWS_PATH      = 'Software\Microsoft\Windows';
  REGINIFILE_CURRENT_VERSION_PATH
                               = REGINIFILE_WINDOWS_PATH+'\'+KEY_CURRENT_VERSION;
  REGINIFILE_UNINSTALL_PATH    = REGINIFILE_CURRENT_VERSION_PATH+'\Uninstall';
  RELEASE_NOTES_FILENAME       = 'Releases.rtf';
  SIMULATED_KEYBOARD_EVENT_KEY = VK_F24; // a key unused by the application

  SOKOBAN_FOR_WINDOWS_DEFAULT_DIRECTORY_NAME    // default directory at the time of writing;
                               = 'Sokoban 3'; // the directory name is used for skin type detection only, so it's no harm if it fails; it just means that the program may miss changing skin type automatically
  SOKOBAN_FOR_WINDOWS_FILE_NAME_GUESS1 // if this file is in a directory, then it's probably the home-directory for the 'Sokoban for Windows' program
                               = 'SokoSkin.exe';
  SOKOBAN_FOR_WINDOWS_FILE_NAME_GUESS2
                               = 'Default.skn';
  STATUS_BAR_MESSAGE_PAUSE_MS  = 2000; // milli seconds; makes a pause that gives the user a chance to read a message on a statusbar before it is overwritten by, say, a mouse-over hint-text
  UNRAR_PROGRAM_NAME           ='UnRAR.exe';
  UNZIP_PROGRAM_NAME           ='Unzip.exe';
  VIEWER_MAX_FADE_TIME_MS      = 5000;
  VIEWER_MAX_TIME_DELAY_MS     = 60000;
  VK_OEM_PERIOD                = $BE; // 190
  YSOKOBAN_PROGRAM_TITLE       = 'YSokoban';

type
  Int8                         = ShortInt;
  PByte                        = ^Byte;
  PPointer                     = ^Pointer;
  TAlternatingOptimizations    = (aoPrimarySecondary,aoBoxLinesPrimary,aoPrimarySecondaryBoxLines,aoBoxLinesPrimarySecondary);
  TAntiAliasing                = (aaNone,aaBilinear,aaFilter);
  TActivity                    = (acNone,acMandala,acImage,{acFireworks,}acFractals);
  TBoardDimensionsAsText       = (bdColRow,bdRowCol);
  TCaptureStep                 = (csNone, csImage, csBoard, csColumnsRows, csBoardSquares, csSavePuzzle, csSaveSkin);
  TColorTheme                  = (ctUndefined,ctBlue,ctGreen,ctOrange,ctRed); // order fixed, the second is the default
  TCornerType                  = (ctTopLeft,ctTopRight,ctBottomLeft,ctBottomRight);
  TCornerTypeSet               = set of TCornerType;
  TCursorType                  = (ctArrow,ctCross,ctDrag,ctHand);
  TDefaultSkinWallType         = (dsSeamlessWalls,dsTiledWalls,dsBrickWalls);
  TDisplayDriverChangedAction  = (ddShutdown,ddContinue);
  TDisplayLayer                = (dlText,dlFocus,dlImage,dlBackground, // order fixed, from top-layer to bottom-layer
                                  dlFadeFrom,dlFadeTo);
                                  // kludge: last 2 layers are temporary bitmaps used by the imageviewer load-thread,
                                  // but since they must match the dimensions of the other bitmaps,
                                  // it's easier to create them along with the other bitmaps in 'Display_'
  TDisplayBitMapTower          = array[Ord(Low(TDisplayLayer))..Ord(High(TDisplayLayer))] of TBitMap;
  PDisplayBitMapTower          = ^TDisplayBitMapTower;
  TDrawingTool                 = (dtNone,dtWall,dtBox,dtGoal,dtPlayer,dtFloor,dtErase,dtSelect); // order must not change
  TDuplicateLevelsStringGridColumn
                               = (dlcRowNo,dlcLevelName,dlcFileName,dlcDirectoryName);
  TDuplicateLevelsStatusInfoItem
                               = (dlScanning,dlScanningFolder,dlScanningFile,dlScanned,dlScannedLevels,dlScannedDuplicates,dlScannedFiles,dlScannedDirectories,dlScannedSolutions);
  TFileTaskItemType            = (ftitUndefined,ftitFolder,ftitFile,ftitLevel);
  TFileTaskType                = (ftCopy,ftMove,ftDelete,ftRename,ftNewFolder,ftNewPackFile,ftNewPlayList,ftNewTextFile,ftSaveAs);
  TFormColors                  = record
    BackgroundColor,
    BackgroundTextColor,
    HighlightedTextColor,
    ButtonColor,
    ButtonTextColor,
    FocusedButtonColor,
    FocusedButtonTextColor,
    GrayedButtonColor,
    GrayedButtonTextColor,
    WindowColor,
    WindowTextColor,
    FocusedWindowColor,
    FocusedWindowTextColor     : TColor;
  end;
  TGenerateLevelColumn         = (glcSelect,glcNo,glcLevelName,glcPushes,glcFitness,glcBirth,glcChildren,glcTime); // order must not change; the overlapping columns must match 'TOptimizeSolutionColumn'
  TGeneratorCallBackInfoItem   = (gcbiiStatus,gcbiiStates,gcbiiOpen,gcbiiTime);
  TGeneratorStatusInfoItem     = (gsiiBestCandidate,gsiiCandidateCount,gsiiInactivityCount,gsiiTime,gsiiMemoryFull);
  TGrabHandle                  = (ghNull, ghBottomLeft, ghLeft,  ghTopLeft, ghTop, ghTopRight, ghRight, ghBottomRight, ghBottom);
  TImageView                   = (ivNone,ivFill,ivStretch,ivCenter,ivTile,ivFloorTile); // ivFloorTile: must be the last one 
  TInBetweenFramesType         = (ibftDuplicate,ibftInterpolate);
  TInt8                        = ShortInt;
  TLowMediumHigh               = (lmhLow,lmhMedium,lmhHigh);
  UInt64                       = Int64; {Delphi 4 has no unsigned 64-bit integer}
  DWORDLONG                    = UInt64;
  PMemoryStatusEx              = ^TMemoryStatus;
  TMemoryStatusEx              = record
    dwLength                   : DWORD;
    ullMemoryLoad              : DWORD;
    ullTotalPhys               : DWORDLONG;
    ullAvailPhys               : DWORDLONG;
    ullTotalPageFile           : DWORDLONG;
    ullAvailPageFile           : DWORDLONG;
    ullTotalVirtual            : DWORDLONG;
    ullAvailVirtual            : DWORDLONG;
    ullAvailExtendedVirtual    : DWORDLONG;
  end;
  TMPlayerDirTask              = (dtCD,dtFolder,dtPlaylist,dtNew,dtSave,dtSaveAs,dtAdd,dtDelete);
  TMPlayerTitleMode            = (tmNone,tmFixedTop,tmFixedBottom,tmAnimateTop,tmAnimateBottom);
  TMusicSource                 = (msCD,msFolder,msPlayList);
  TOnEscapeKeyAction           = (oekaExitApplication,oekaMinimize,oekaRestartGame,oekaDoNothing);
  TOnLeftClickEmptySquareAction= (olcesaShowBoxesThatCanGoToSquare,olcesaMovePlayerToSquare);
  TOnRightClickAction          = (orcaLoadSnapshotIfAnyElseRestartGame,orcaRestartGame,orcaUndoMove);
  tOnShiftKeyAction            = (oskaOpenSnapshotsAndSolutionsWindow,oskaSHIFTisUPandUPisDOWN);
  TOpenTask                    = (otNone,otGame,otSound,otMusic,otMusicPath,otMusicPlayer,otImage,otPalette,otSolver,otOptimizer);
  TOpenSubTask                 = (osNone,osLevelEditor,osGeneratorCandidateSet,osPluginTaskQueue,osTile,osSkin,osWall);
  TOptimizeSolutionColumn      = (oscSelect,oscNo,oscLevelName,oscSnapshotName,oscMetrics,oscOptimization); // order must not change; the overlapping columns must match 'TSolveLevelColumn'
  TPluginButtonState           = (pbsCancel,pbsTerminating,pbsRun);
  TPluginCallBackInfoItem      = (pcbiiStatus,pcbiiMoves,pcbiiPushes,pcbiiStates,pcbiiTime);
  TPluginThreadPriority        = tpIdle..tpNormal;
  TPluginResult                = (prOK, // the values and their order must match the Sokoban dll-plugin specification, e.g., in 'YASSdll.dpr'
                                  prConstraintsViolation,
                                  prInvalidLevel,
                                  prUnsolvable,
                                  prUnsolved,
                                  prGameTooLong,
                                  prInvalidSettings,
                                  prFailed,
                                  prTimeout,
                                  prTerminatedByUser);
  TQuadrantType                = (qTopLeft,qTopRight,qBottomLeft,qBottomRight);
  TQuadrantSet                 = set of TQuadrantType;
  TRectangleSide               = (rsTop,rsLeft,rsBottom,rsRight); // order must not change; texts and mapping sides to directions depend on the ordering
  TSkinExportFormatType        = (seft4x4,seft4x8); // skin export base format, columns x rows
  TSolveLevelColumn            = (slcSelect,slcNo,slcLevelName,{slcSnapshotName}slcMetrics); // order must not change; the overlapping columns must match 'TOptimizeSolutionColumn'
  TStringArray                 = array of String;
  TUndefinedOrBoolean          = (ubUndefined,ubFalse,ubTrue);
  TWallType                    = (wtTiledWall,wtSeamlessWallNoCap,wtSeamlessWallWithCap);

const
  CURSOR_TYPE_TO_CURSOR        : array[TCursorType] of TCursor
                               = (crArrow,crCross,crDrag,crHandPoint);


function  AbbreviatedFilePath(const Path__,LeadingPath__:String):String; {throws EOutOfMemory}
function  AddItemOrMoveItemToTopOfComboBox(var ComboBox:TComboBox; Capacity:Integer; const Item:String; FormatItemAsNameAndPath:Boolean):Integer;
procedure AddItemToCommaSeparatedText(const Item__:String; var Text__:string); {throws EOutOfMemory}
//function  Align( Value__, Alignment__ : Integer ) : Integer;
function  ApplicationHiglightedTextColor:TColor;
function  Base64Decode(const InputString__:String; var OutputBuffer__:PByte; var OutputSize__:Integer; const Last2EncodingCharacters__:String):Boolean;
function  Base64DecodeToFile(const InputString__,FileName__:String):Boolean;
function  Base64Encode(InputBuffer__:PByte; InputSize__:Integer; var OutputString__:String; const Last2EncodingCharacters__:String):Boolean;
function  Base64EncodeFile(const InputFileName__,OutputFileName__:String; LineLength__:Integer):Boolean;
function  BoardDimensionsAndBoxesAndFloorsAsText(ColCount__,RowCount__,BoxCount__,FloorCount__:Integer; BoardDimensionsAsText__:TBoardDimensionsAsText; BoardDimensionsWithFloorCount__:Boolean):String;
function  BoardDimensionsAsText(ColCount__,RowCount__:Integer; BoardDimensionsAsText__:TBoardDimensionsAsText):String;
function  CellToRect(Col__,Row__,ColWidth__,RowHeight__:Integer):TRect; // 0-based col,row
function  ClipRect(var Rect__:TRect; const ClippingRect__:TRect):Boolean;
function  ContrastColor(Color:TColor):TColor;
function  CopyAndDeleteFile(const ExistingFileName__,NewFileName__:String):Boolean;
function  DeleteFilesDialog(OpenDialog__:TOpenDialog; Form__:TForm; StatusPanel__:TStatusPanel):Boolean;
function  DirectionToWallType(Direction__:TDirection):Integer;
procedure DirectoryListBoxScrollInView(DirectoryListBox:TDirectoryListBox; Index:Integer);
procedure DoNothing;
function  EnableDisableScreenSaver(Enabled__:Boolean):Boolean;
procedure ExchangeLists(var List1,List2:Classes.TList);
function  ExecuteAndWait(const CommandLine__:String; ShowWindow__:Word; TimeOut__:DWORD): DWORD;
function  ExpandedFilePath(const Path__,LeadingPath__:String):String; {throws EOutOfMemory}
function  ExtractFileNameWithoutExt(const FileName:String):String;
//procedure ExtractLevelsSoko15;
//procedure ExtractLevelsWGuy;
//procedure ExtractLevelsWinSoko;
function  ExtractMaskFromFilter(const Filter:String; FilterIndex:Integer):String;
procedure ExtractSokoFunTiles;
function  FileAge(const FileName__:String): Integer;
function  FileExists(const FileName__:String): Boolean;
function  FileHasMoved(var FileName__:String):Boolean;
procedure FileListBoxScrollInView(FileListBox:TFileListBox; Index:Integer);
function  FileStartsWith(const FileName,S:String):Boolean;
//function  FindPriorNextFileName(const FileName:String; Prior:Boolean):String;
function  FormatItemAsNameAndPath(const Item__:String; var Name__,Path__:String):String;
function  FormatTimeMS(TimeMS__:TTimeMS):String;
function  FontsIdentical(Font1,Font2:TFont):Boolean;
procedure GridScrollInView(Grid:TStringGrid; ARow:Integer);
function  GetAnotherInstanceWindowHandle(var AnotherInstanceWindowHandle__:HWND):Boolean;
function  GetApplicationVirtualFolder:String;
function  GetAvailablePhysicalMemoryByteSize:Cardinal;
function  GetAvailableVirtualMemoryByteSize:Cardinal;
function  GetCurrentThreadStack( var Address__ : Pointer; var ByteSize__ : UInt ) : Boolean;
procedure GetDrivesOfType(Type1,Type2:Integer; var ComboBox:TComboBox);
function  GetPhysicalMemoryByteSize:Cardinal;
function  GetFolderPath(Folder__:Integer):String;
function  GetMediaPath(Folder__:Integer):String;
function  GetScreenSaverEnabled(var Enabled__,PasswordProtected__:Boolean):Boolean;
function  GridColIndexOf(const Grid:TStringGrid; ColNo:Integer; const s:String):Integer;
procedure HeapStatistics;
function  HorizontalScrollBarHeight:Integer;
function  IntToHex(Value: Integer; Digits: Integer): String;
function  IntToStrOrBlank(Int64__:Int64):String; {throws EOutOfMemory}
function  IsABase64EncodedText(const Text__,Last2EncodingCharacters__:String):Boolean;
function  IsAHighResolutionScreen:Boolean;
function  IsANewerFile(const OldVersionFileName__,NewVersionFileName__:String):Boolean;
function  IsANewFileName(const FileName:String):Boolean;
function  IsEqualRects(const R1,R2:TRect):Boolean;
function  IsKeyPressed(VirtualKeyCode__:Integer):Boolean;
function  IsWindowsDefaultColorBtnFace(Color:TColor):Boolean;
function  IsWindowsVistaOrNewerOperatingSystem:Boolean;
function  KeepDataPathUpToDate(Path__:String):String;
function  KeepPluginsPathUpToDate(Path__:String):String;
function  LinearInterpolation( Value__ : Double; Low__, High__ : Integer ) : Integer;
function  ListToStrings(List:SokUtil_.TList; Strings:TStrings):Boolean;
function  LoadComboBoxFromIniFile(const IniFile:TIniFile; const Section:String; Capacity:Integer; IsADirectoryComboBox,IsAFileComboBox,CalculateDropDownWidth,FormatItemAsNameAndPath:Boolean; ComboBox:TComboBox):Boolean;
function  LoadComboBoxFromStrings(Min__,Max__:Integer; const Texts__:array of string; Canvas__:TCanvas; ComboBox__:TComboBox):Boolean;
function  LoadFontFromIniFile(const IniFile:TIniFile; const Section,KeyPrefix:String; Font:TFont):Boolean;
function  LoadFormColorsFromIniFile(const IniFile:TIniFile; const Section:String; var FormColors:TFormColors):Boolean;
function  LoadStringGridColumnWidthsFromIniFile(const IniFile:TIniFile; const Section:String; var StringGrid:TSTringGrid):Boolean;
function  LoCase(Ch:Char):Char;
procedure MakeAllColumnsFullyVisible(StringGrid__:TStringGrid; MinColWidth__,VariableCol__:Integer);
function  MakeNewFileName(const FileName,Extension:String; UseParentheses:Boolean):String;
procedure MakeUniqueGrayedButtonTextColor(var FormColors:TFormColors);
function  MaxTextExtent(Canvas:TCanvas; const Strings:array of String):TSize;
function  MoveFileOrFolder(const ExistingFileOrFolderName__,NewFileOrFolderName__:String):Boolean;
function  MoveOrCopyFile(const ExistingFileName__,NewFileName__:String):Boolean;
function  MovesPerSecondText(MovesPerSecond__:Integer):String;
function  NetworkVolume(DriveChar: Char): string;
function  PathCompactPath(Canvas:TCanvas; const Path:String; PixelWidth:Integer):String;
//procedure PlaySound(const FileName:String);
function  QuickSort(Items:Pointer; ItemCount,ItemSize:Cardinal; CompareFunction:TCompareFunction):Boolean;
function  RangeCheck(Value,Min,Max:Integer):Boolean;
function  RegGetKey(Root:HKey; const Path,Section,Key,DefaultValue:String; var Value:String):Boolean;
function  RegSetKey(Root:HKey; const Path,Section,Key,Value:String):Boolean;
function  RGB_BGR(const s:String):String;
function  RectHeight(const R:TRect):Integer;
function  RectPlusOffset(const Rect:TRect; X,Y:Integer):TRect;
function  RectQuadrant(const Rect__:TRect; Quadrant__:TQuadrantType):TRect;
function  RectToStr(const Rect__:TRect):String;
function  RectWidth (const R:TRect):Integer;
function  RunNo:Integer;
function  SafeStrToInt(const S:String; Hex:Boolean; var Value:Integer):Boolean;
function  SaveComboBoxToIniFile(const IniFile:TIniFile; const Section:String; Capacity:Integer; const ComboBox:TComboBox):Boolean;
function  SaveFontToIniFile(const IniFile:TIniFile; const Section,KeyPrefix:String; const Font:TFont):Boolean;
function  SaveFormColorsToIniFile(const IniFile:TIniFile; const Section:String; const FormColors:TFormColors):Boolean;
function  SaveStringGridColumnWidthsToIniFile(const IniFile:TIniFile; const Section:String; const StringGrid:TSTringGrid):Boolean;
procedure ScaleKeepingAspectRatio(Width__,Height__,MaxWidth__,MaxHeight__:Integer; var NewWidth__,NewHeight__:Integer);
function  Sign(i:Integer):Integer;
procedure SimulateKeyboardEvent;
procedure SetDefaultDirectory;
procedure SetDefaultFormColors(var FormColors:TFormColors);
procedure SetComboBoxDropDownWidth(ComboBox:TComboBox; Index:Integer; FormatItemAsNameAndPath:Boolean);
function  SetCPUAffinity(UseOnlyOneProcessor__:Boolean):Boolean;
function  SetThreadExecutionState(Flags__:Cardinal):Boolean;
procedure SetGroupBoxCaption(Canvas__:TCanvas; GroupBox__:TGroupBox; const Caption__:String);
function  StrAnsiPosCI(const SubStr__,Str__:String):Integer;
function  StrBeginsWithChar(const s:String; Ch:Char):Boolean;
function  StrBeginsWithDriveLetter(const Path__:String):Boolean;
function  StrHasAcceleratorChar(const Str__:String; ToUpperCase__:Boolean; var Key__:Char):Boolean;
function  StrHexDigits(const Str__:String):String;
function  StrIndexOfCI(var s:String; const Strings:array of String):Integer; overload;
function  StrIndexOfCI(var s:String; const Strings:TStrings):Integer; overload;
function  StringsToList(Strings:TStrings; List:SokUtil_.TList):Boolean;
function  StrLastPos(const SubString__,String__:String):Integer;
function  StrLine(Length__:Integer):String; // throws EOutOfMemory;
function  StrRemoveChar(const s:String; Ch:Char):String;
procedure StrSubstituteLineSeparatorsBySpaces(Text__:PChar);
function  StrToBool(const s:String):Boolean;
function  StrToFileName(const s:String):String;
function  StrToRect(const s:String; var Rect__:TRect):Boolean;
function  StrWithQuotedAmpersands(const s:String):String;
//function  StrSubstituteChar(const s:String; Old,New:Char):String;
function  TitleWithOptionalSubTitle(const Title__,SubTitle__:String):String;
function  VerticalScrollBarWidth:Integer;
function  VisualFileName(const FileName:String):String;
function  VolumeID(DriveChar: Char): String;
procedure Z;

// folder identifier constants
const
  CSIDL_DESKTOP	                       = 0;
  CSIDL_INTERNET                       = 1;
  CSIDL_PROGRAMS	               = 2;
  CSIDL_CONTROLS	               = 3;
  CSIDL_PRINTERS	               = 4;
  CSIDL_PERSONAL	               = 5;
  CSIDL_FAVORITES	               = 6;
  CSIDL_STARTUP	                       = 7;
  CSIDL_RECENT	                       = 8;
  CSIDL_SENDTO	                       = 9;
  CSIDL_BITBUCKET	               = 10;
  CSIDL_STARTMENU	               = 11;
  CSIDL_MYDOCUMENTS                    = 12;
  CSIDL_MYMUSIC                        = 13;
  CSIDL_MYVIDEO                        = 14;
  CSIDL_DESKTOPDIRECTORY	       = 16;
  CSIDL_DRIVES	                       = 17;
  CSIDL_NETWORK	                       = 18;
  CSIDL_NETHOOD	                       = 19;
  CSIDL_FONTS	                       = 20;
  CSIDL_TEMPLATES	               = 21;
  CSIDL_COMMON_STARTMENU	       = 22;
  CSIDL_COMMON_PROGRAMS	               = 23;
  CSIDL_COMMON_STARTUP	               = 24;
  CSIDL_COMMON_DESKTOPDIRECTORY	       = 25;
  CSIDL_APPDATA                        = 26;
  CSIDL_PRINTHOOD                      = 27;
  CSIDL_LOCAL_APPDATA                  = 28;
  CSIDL_ALTSTARTUP                     = 29;
  CSIDL_COMMON_ALTSTARTUP	       = 30;
  CSIDL_COMMON_FAVORITES	       = 31;
  CSIDL_INTERNET_CACHE                 = 32;
  CSIDL_COOKIES	                       = 33;
  CSIDL_HISTORY	                       = 34;
  CSIDL_COMMON_APPDATA	               = 35;
  CSIDL_WINDOWS	                       = 36;
  CSIDL_SYSTEM	                       = 37;
  CSIDL_PROGRAM_FILES	               = 38;
  CSIDL_MYPICTURES	               = 39;
  CSIDL_PROFILE	                       = 40;
  CSIDL_SYSTEMX86	               = 41;
  CSIDL_PROGRAM_FILESX86	       = 42;
  CSIDL_PROGRAM_FILES_COMMON           = 43;
  CSIDL_PROGRAM_FILES_COMMONX86        = 44;
  CSIDL_COMMON_TEMPLATES	       = 45;
  CSIDL_COMMON_DOCUMENTS	       = 46;
  CSIDL_COMMON_ADMINTOOLS	       = 47;
  CSIDL_ADMINTOOLS	               = 48;
  CSIDL_CONNECTIONS	               = 49;
  CSIDL_COMMON_MUSIC	               = 53;
  CSIDL_COMMON_PICTURES	               = 54;
  CSIDL_COMMON_VIDEO	               = 55;
  CSIDL_RESOURCES	               = 56;
  CSIDL_RESOURCES_LOCALIZED	       = 57;
  CSIDL_COMMON_OEM_LINKS	       = 58;
  CSIDL_CDBURN_AREA	               = 59;
  CSIDL_COMPUTERSNEARME	               = 61;
  CSIDL_FLAG_DONT_VERIFY	       = $4000;
  CSIDL_FLAG_CREATE	               = $8000;
  CSIDL_FLAG_MASK	               = $FF00;

var
  LogFileCount:Integer=0;

implementation

uses SysUtils,Registry,{,MMSystem (for 'PlaySound')}
     Pack_,Game_,Text_,Main_;

procedure GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx); stdcall; external 'kernel32.dll' name 'GlobalMemoryStatusEx';

function  AbbreviatedFilePath(const Path__,LeadingPath__:String):String; {throws EOutOfMemory}
var s:String;
begin
  try    Result:=Path__; s:=StrWithoutTrailingPathDelimiter(LeadingPath__);
         if (s<>'') and StrBeginsWith(Result,s) and (Pos(FILE_NAME_PATH_DELIMITER,s)<>0) then begin
            Result:=ELLIPSES+FILE_NAME_PATH_DELIMITER+ExtractFileName(s)+Copy(Result,Succ(Length(s)),MaxInt);
            if Length(Result)>=Length(Path__) then Result:=Path__;
            end;
  except on E:Exception do Result:=Path__;
  end;
end;

function  AddItemOrMoveItemToTopOfComboBox(var ComboBox:TComboBox; Capacity:Integer; const Item:String; FormatItemAsNameAndPath:Boolean):Integer;
var i:Integer; s,s1:String; AnObject:TObject;
begin // returns old index if the item already is a member, otherwise 0
  Result:=0;

  if   IsAnIniFileSectionFileName(Item) then with ComboBox do begin // the name refers to a member of a file; delete the old member from the list, if any
       s:=AnsiLowerCase(ExtractIniFileName(Item));
       for i:=Pred(Items.Count) downto 0 do
           if StrBeginsWith(AnsiLowerCase(Items[i]),s) then begin
              Items.Delete(i); Result:=i; break;
              end;
       s1:=ExtractSectionName(Item);
       for i:=1 to Length(s1) do
           if s1[i]=DOUBLE_QUOTE then s1[i]:=QUOTE; // filenames cannot have double-quotes
       s:=ExtractFileNameWithoutPathAndExtension(s);
       if   StrEqual(s,s1) or StrEqual(s1,TEXT_LEVEL) then
            s:=ExtractIniFileName(Item) // don't store the member name when there (probably) only is one member in the file
       else s:=Item; // store the full filename and member-name
       end
  else s:=Item;

  i:=0;
  if s<>'' then with ComboBox do begin
     while    (i<Items.Count) and (AnsiCompareText(s,Items[i])<>0) do Inc(i);
     if       i=Items.Count then begin
              Items.Insert(0,s);
              SetComboBoxDropDownWidth(ComboBox,0,FormatItemAsNameAndPath);
              if Items.Count>Capacity then Items.Delete(Pred(Items.Count));
              end
     else if  i>0 then begin {'True': the item already exists}
              AnObject:=Items.Objects[i];
              Items.Delete(i); Items.InsertObject(0,s,AnObject);
              Result:=i;
              end;
     ItemIndex:=0;
     end;
end;

procedure AddItemToCommaSeparatedText(const Item__:String; var Text__:string); {throws EOutOfMemory}
begin
  if Item__<>'' then
     if   Text__<>'' then
          Text__:=Text__+COMMA+SPACE+AnsiLowerCase(Item__)
     else Text__:=Item__;
end;

function  Align( Value__, Alignment__ : Integer ) : Integer;
begin // returns the smallest value >= "Value__" which is a multiple of
      // "Alignment__", except when that value overflows the return value data
      // type;
      // precondition : the value and the alignment are non-negative integers;
  if   Alignment__ <> 0 then
       Result      := Value__ + ( ( Alignment__ - ( Value__ mod Alignment__ ) ) mod Alignment__ )
  else Result      := Value__;
end;

function  ApplicationHiglightedTextColor:TColor;
begin // returns a color for highlighted text on normal background (clBtnFace)
  if   IsWindowsDefaultColorBtnFace(clBtnFace) then
       Result:=clBlue
  else Result:=clBtnText; // "clHighlight": current background color of selected text ("clHighlight" isn't guaranteed to have enough contrast to the default background color, hence, settle for the safe choice, the normal "clBtnText")
end;

const
  Base64EncodingPadding : Char ='=';

function Base64Decode(const InputString__:String; var OutputBuffer__:PByte; var OutputSize__:Integer; const Last2EncodingCharacters__:String):Boolean;
var i,n,Count,InputSize:Integer; Ch:Char; InputPointer:PChar; OutputPointer:PByte;
    Input:array[0..3] of Integer; Base64DecodingTable:array[Char] of Int8;
begin // precondition: 'Char' = byte (8 bits), and subsequently, type 'String' = array of 'Char'
  OutputBuffer__:=nil; OutputSize__:=0;

  for    Ch:=Low(Base64DecodingTable) to High(Base64DecodingTable) do Base64DecodingTable[Ch]:=-1;
  for    Ch:='A' to 'Z' do Base64DecodingTable[Ch]:=00+Ord(Ch)-Ord('A');
  for    Ch:='a' to 'z' do Base64DecodingTable[Ch]:=26+Ord(Ch)-Ord('a');
  for    Ch:='0' to '9' do Base64DecodingTable[Ch]:=52+Ord(Ch)-Ord('0');
  if Length(Last2EncodingCharacters__)<>2 then begin // 'True': use standard encoding characters
     Base64DecodingTable['+']:=62;
     Base64DecodingTable['/']:=63;
     end
  else begin
     Base64DecodingTable[Last2EncodingCharacters__[1]]:=62;
     Base64DecodingTable[Last2EncodingCharacters__[2]]:=63;
     end;

  try    Result:=True;

         InputSize:=Length(InputString__);
         while (InputSize<>0) // trim trailing illegal characters, if any
               and
               (InputString__[InputSize]<>Base64EncodingPadding)
               and
               (Base64DecodingTable[InputString__[InputSize]]=-1)
               do Dec(InputSize);

         n:=InputSize;
         for i:=1 to InputSize do
             if InputString__[i]<=SPACE then Dec(n); // drop control characters like CR and LF

         OutputSize__:=(n div 4)*3;

         for i:=0 to 1 do // drop base64 encoding padding, if any
             if (InputSize<>0) and
                (InputString__[InputSize]=Base64EncodingPadding) then begin
                Dec(InputSize); Dec(OutputSize__);
                end;

         if OutputSize__<>0 then begin
            GetMem(OutputBuffer__,OutputSize__);
            if OutputBuffer__<>nil then begin

               InputPointer:=Addr(InputString__[1]); OutputPointer:=OutputBuffer__;
               n:=0; Count:=0;
               while InputSize>0 do begin
                 Ch:=InputPointer^; Inc(InputPointer); Dec(InputSize);
                 i:=Base64DecodingTable[Ch];
                 if i<>-1 then
                    case n of
                      0: begin Input[0]:=i; Inc(n); end;
                      1: begin Input[1]:=i; Inc(n);
                               OutputPointer^:=(Input[0] shl 2) or (Input[1] shr 4);
                               Inc(OutputPointer); Inc(Count);
                         end;
                      2: begin Input[2]:=i; Inc(n);
                               OutputPointer^:=(Input[1] shl 4) or (Input[2] shr 2);
                               Inc(OutputPointer); Inc(Count);
                         end;
                      3: begin Input[3]:=i; n:=0;
                               OutputPointer^:=(Input[2] shl 6) or (Input[3]);
                               Inc(OutputPointer); Inc(Count);
                         end;
                    end // case
                 else if Ch>SPACE then begin // silently drop control characters like CR and LF
                         InputSize:=-1; n:=-1; // exit loop and fail
                         end;
                 end;

               if        (n<>0) and (InputSize<>0) then
                         raise Exception.Create(InvalidCompressedDataText)
               else if   Count<>OutputSize__ then
                         //raise Exception.Create(InternalErrorText)
                         raise Exception.Create(InvalidCompressedDataText);
               end

            else raise Exception.Create(TEXT_MEMORY_FULL);
            end;

  except on E:Exception do begin
         if OutputBuffer__<>nil then FreeMem(OutputBuffer__);
         OutputSize__:=0;
         Result:=Error(E.Message,Application.Title+' - Base64Decode');
         end;
  end;
end;

function Base64DecodeToFile(const InputString__,FileName__:String):Boolean;
var i,BufferSize:Integer; Buffer:PByte; F:File;
begin
  Result:=False;
  try
    if Base64Decode(InputString__,Buffer,BufferSize,'') then
       try     AssignFile(F,FileName__);
               try     Rewrite(F,1);
                       BlockWrite(F,Buffer^,BufferSize,i);
                       if   BufferSize=i then Result:=True
                       else raise Exception.Create(TEXT_WRITE_FILE_ERROR);
               finally CloseFile(F);
               end;
       finally if Buffer<>nil then FreeMem(Buffer);
       end
  except
    on E:Exception do Result:=Error(E.Message,Application.Title+' - Base64 Decode to File');
  end;
end;

var
  Base64EncodingTable:array[0..63] of Char = (
    'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
    'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
    '0','1','2','3','4','5','6','7','8','9','+','/');

function Base64Encode(InputBuffer__:PByte; InputSize__:Integer; var OutputString__:String; const Last2EncodingCharacters__:String):Boolean;
var n,OutputPos,OutputSize:Integer;
    Input:array[0..2] of Integer; Output:array[0..3] of Char;
begin
  OutputString__:='';
  try    Result:=Assigned(InputBuffer__) and (InputSize__>=0);
         if  Result then begin
             OutputSize:=InputSize__ div 3; // 'OutputSize' is a misnomer at this time; here it contains the number of 4-character groups
             if (InputSize__ mod 3)<>0 then Inc(OutputSize); // last characters + padding
             Result:=OutputSize<=((High(OutputSize) div 4) div SizeOf(Char));
             if Result then begin
                OutputSize:=OutputSize*4; // 3 input bytes (3*8=24 bits) are encoded as 4 6-bit numbers (4x6=24) and they are represented by characters
                SetLength(OutputString__,OutputSize); // set the proper length of the output string
                OutputPos:=0;

                if Length(Last2EncodingCharacters__)<>2 then begin // 'True': use standard encoding characters
                   Base64EncodingTable[62]:='+';
                   Base64EncodingTable[63]:='/';
                   end
                else begin
                   Base64EncodingTable[62]:=Last2EncodingCharacters__[1];
                   Base64EncodingTable[63]:=Last2EncodingCharacters__[2];
                   end;

                while InputSize__<>0 do begin

                  Input[0]:=0; Input[1]:=0; Input[2]:=0;

                  Input[0]   :=InputBuffer__^; Inc(InputBuffer__); Dec(InputSize__); n:=1;

                  if InputSize__<>0 then begin
                     Input[1]:=InputBuffer__^; Inc(InputBuffer__); Dec(InputSize__); Inc(n);
                     end;

                  if InputSize__<>0 then begin
                     Input[2]:=InputBuffer__^; Inc(InputBuffer__); Dec(InputSize__); Inc(n);
                     end;

                  Output[0] := Base64EncodingTable[  Input[0]          shr 2];
                  Output[1] := Base64EncodingTable[((Input[0] and   3) shl 4) or (Input[1] shr 4)];
                  Output[2] := Base64EncodingTable[((Input[1] and  $F) shl 2) or (Input[2] shr 6)];
                  Output[3] := Base64EncodingTable[  Input[2] and $3F];

                  if      n<2 then begin
                          Output[2]:=Base64EncodingPadding;
                          Output[3]:=Base64EncodingPadding;
                          end
                  else if n<3 then Output[3]:=Base64EncodingPadding;

                  Inc(OutputPos); OutputString__[OutputPos]:=Output[0];
                  Inc(OutputPos); OutputString__[OutputPos]:=Output[1];
                  Inc(OutputPos); OutputString__[OutputPos]:=Output[2];
                  Inc(OutputPos); OutputString__[OutputPos]:=Output[3];

                  end;
                end
             else raise Exception.Create(Format(DataTooLargeText__,['Stream contents']));
             end
         else raise Exception.Create(TEXT_INVALID_DATA);
  except on E:Exception do Result:=Error(E.Message,Application.Title+' - Base64Encode');
  end;
end;

function Base64EncodeFile(const InputFileName__,OutputFileName__:String; LineLength__:Integer):Boolean;
var Pos,FileSize:Integer; s:String; N:SokUtil_.TNode; L:SokUtil_.TList; Buffer:PByte;
begin
  Result:=False;
  try    N:=nil; L:=nil;
         try if CreateObject(otNode,N) and
                CreateObject(otList,TNode(L)) and
                N.LoadFromFile(InputFileName__) then begin
                FileSize:=Length(N.Text);
                if   FileSize>0 then Buffer:=Addr(N.Text[1])
                else Buffer:=nil;
                Result:=Base64Encode(Buffer,FileSize,s,'');
                Pos:=1;
                if LineLength__<=0 then LineLength__:=Length(s);
                while (Pos<=Length(s)) and Result do begin
                  Result:=(L.AddTextLine(Copy(s,Pos,LineLength__),False)<>nil);
                  Inc(Pos,LineLength__);
                  end;
                Result:=L.SaveToFile(OutputFileName__);
                end
             else raise Exception.Create(TEXT_TASK_FAILED);
         finally N.Free; L.Free;
         end;
  except on E:Exception do Result:=Error(E.Message,Application.Title+' - Base64EncodeToFile');
  end;
end;

function  BoardDimensionsAndBoxesAndFloorsAsText(ColCount__,RowCount__,BoxCount__,FloorCount__:Integer; BoardDimensionsAsText__:TBoardDimensionsAsText; BoardDimensionsWithFloorCount__:Boolean):String;
begin
  if   BoardDimensionsWithFloorCount__ then
       if   BoardDimensionsAsText__=bdColRow then
            Result:=Format(FORMAT_BOARD_DIMENSIONS_AND_BOXES_AND_FLOORS,[ColCount__,RowCount__,BoxCount__,FloorCount__])
       else Result:=Format(FORMAT_BOARD_DIMENSIONS_AND_BOXES_AND_FLOORS,[RowCount__,ColCount__,BoxCount__,FloorCount__])
  else if   BoardDimensionsAsText__=bdColRow then
            Result:=Format(FORMAT_BOARD_DIMENSIONS_AND_BOXES,[ColCount__,RowCount__,BoxCount__])
       else Result:=Format(FORMAT_BOARD_DIMENSIONS_AND_BOXES,[RowCount__,ColCount__,BoxCount__]);
end;

function  BoardDimensionsAsText(ColCount__,RowCount__:Integer; BoardDimensionsAsText__:TBoardDimensionsAsText):String;
begin
  if   BoardDimensionsAsText__=bdColRow then
       Result:=Format(FORMAT_BOARD_DIMENSIONS,[ColCount__,RowCount__])
  else Result:=Format(FORMAT_BOARD_DIMENSIONS,[RowCount__,ColCount__]);
end;

function  CellToRect(Col__,Row__,ColWidth__,RowHeight__:Integer):TRect; // 0-based col,row
begin
  Result:=Rect(Col__*ColWidth__,Row__*RowHeight__,Succ(Col__)*ColWidth__,Succ(Row__)*RowHeight__);
end;

function ClipRect(var Rect__:TRect; const ClippingRect__:TRect):Boolean;
begin
  with Rect__ do begin
    if Left  <ClippingRect__.Left   then Left  :=ClippingRect__.Left;
    if Top   <ClippingRect__.Top    then Top   :=ClippingRect__.Top;
    if Right >ClippingRect__.Right  then Right :=ClippingRect__.Right;
    if Bottom>ClippingRect__.Bottom then Bottom:=ClippingRect__.Bottom;
    Result:=(Left<Right) and (Top<Bottom);
    if not Result then FillChar(Rect__,SizeOf(Rect__),0);
    end;
end;

function  ContrastColor(Color:TColor):TColor;
var c,r,b,g:Integer;
begin
  c:=ColorToRGB(Color);
  r:=255-( c         and $FF);
  g:=255-((c shr  8) and $FF);
  b:=255-((c shr 16) and $FF);
  if   (Abs(128-r)<32) and (Abs(128-g)<32) and (Abs(128-b)<32) then
       Result:=clWhite // the original color and the inverse color are too close: use white instead
  else Result:=TColor(r+(g shl 8)+(b shl 16));
//  Result:=TColor($00FFFFFF and (not ColorToRGB(Color)));
end;

function  CopyAndDeleteFile(const ExistingFileName__,NewFileName__:String):Boolean;
begin // using CopyAndDeleteFile' instead of 'MoveFileOrFolder' drops the security attributes
   Result:=FileExists(ExistingFileName__) and
           Windows.CopyFile(PChar(ExistingFileName__),PChar(NewFileName__),True);
   if Result then begin
      SleepEx(10,False);
      Result:=SetFileAttributes(PChar(NewFileName__),FILE_ATTRIBUTE_NORMAL);
      if Result then DeleteFile(ExistingFileName__);
      end;
end;

function  DeleteFilesDialog(OpenDialog__:TOpenDialog; Form__:TForm; StatusPanel__:TStatusPanel):Boolean;
var i:Integer; MessageText:string;
begin // Returns 'True' is files were selected for deletion; this does not reflect whether the files really were deleted (the user may answer "no" to the confirmation question)
  with OpenDialog__ do begin
    FileName:='';
    Options:=Options+[ofAllowMultiSelect,ofFileMustExist];

    if Assigned(StatusPanel__) then StatusPanel__.Text:=GetLongHint(SelectMultipleFilesHint);
    Result:=Execute and (Files.Count>0);
    if Assigned(StatusPanel__) then StatusPanel__.Text:='';

    if Result then begin
       if Assigned(Form__) then Form__.Update; // to repaint the form after the dialog has gone

       if   Files.Count=1 then
            if   FileExists(Files[0]) then
                 MessageText:=DeleteFileQuestionText+NL+' "'+FileName+'" ?'
            else MessageText:=''
       else begin MessageText:=DeleteFilesQuestionText+NL+
                  ' "'+Files[0]+'",'+NL+
                  ' "'+Files[1]+'"';
                  if Files.Count=2 then MessageText:=MessageText+' ?'
                  else MessageText:=MessageText+' ... ?';
            end;
       if (MessageText<>'') and
          (Msg(MessageText+NL+NL+NoUndoText,Title,MB_ICONQUESTION+MB_YESNO+MB_DEFBUTTON2)=IDYES) then
          for i:=Pred(Files.Count) downto 0 do
              if FileExists(Files[i]) then
                 if (not DeleteFile(PChar(Files[i]))) then
                    if   Assigned(Form__) then
                         Error(Format(TEXT_FILE_DELETE_FAILED_FORMAT,[Files[i]]),Form__.Caption)
                    else Error(Format(TEXT_FILE_DELETE_FAILED_FORMAT,[Files[i]]),Title);
       end;
    end;
end;

function  DirectionToWallType(Direction__:TDirection):Integer;
begin
  case Direction__ of
    Up   : Result:=BT_WALL_ABOVE;
    Left : Result:=BT_WALL_TO_THE_LEFT;
    Down : Result:=BT_WALL_BELOW;
    Right: Result:=BT_WALL_TO_THE_RIGHT;
    else   raise Exception.Create(TEXT_RANGE_ERROR+': Misc_.DirectionToWallType');
  end; // case;
end;

procedure DirectoryListBoxScrollInView(DirectoryListBox:TDirectoryListBox; Index:Integer);
var i,j,VisibleItemCount:Integer;
begin
  with DirectoryListBox do begin
    VisibleItemCount:=ClientHeight div Max(1,ItemHeight);
    if (TopIndex+VisibleItemCount<=Index) or (TopIndex>Index) then
       begin i:=0; j:=Max(1,VisibleItemCount);
             while i+VisibleItemCount<=Index do Inc(i,j);
             TopIndex:=Min(i,Pred(Items.Count));
       end;
    end;
end;

procedure DoNothing;
begin
end;

function  EnableDisableScreenSaver(Enabled__:Boolean):Boolean;
begin
  if Enabled__ then begin
     Result:=SetThreadExecutionState(ES_CONTINUOUS);
     if not SystemParametersInfo(SPI_SETSCREENSAVEACTIVE,1,nil,SPIF_SENDWININICHANGE) then Result:=False;
     end
  else begin
     // it's assumed that when the screen saver is disabled, then the application also wants to prevent the computer from going to sleep
     Result:=SetThreadExecutionState(ES_AWAYMODE_REQUIRED or ES_CONTINUOUS or ES_DISPLAY_REQUIRED or ES_SYSTEM_REQUIRED);
     if not SystemParametersInfo(SPI_SETSCREENSAVEACTIVE,0,nil,SPIF_SENDWININICHANGE) then Result:=False;
     end;
end;

function  ExecuteAndWait(const CommandLine__:String; ShowWindow__:Word; TimeOut__:DWORD): DWORD;
var // Executes an application and waits for it to terminate;
    // Example: ExecuteAndWait('C:\Windows\Notepad.exe',Windows.SW_NORMAL,Windows.INFINITE);
  StartupInfo:TStartupInfo;
  ProcessInfo:TProcessInformation;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do begin
       cb := SizeOf(TStartupInfo);
       dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
       wShowWindow := ShowWindow__;
       end;
  if   CreateProcess(nil,PChar(CommandLine__),nil,nil,False,CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS, nil,nil,StartupInfo,ProcessInfo) then begin
       Result:=WaitForSingleObject(ProcessInfo.hProcess,TimeOut__); {'TimeOut__': milliseconds or INFINITE}
       GetExitCodeProcess(ProcessInfo.hProcess, Result);
       CloseHandle(ProcessInfo.hProcess);
       CloseHandle(ProcessInfo.hThread);
       end
  else Result:=GetLastError; {an error occured during 'CreateProcess'}
end;

procedure ExchangeLists(var List1,List2:Classes.TList);
var t:Classes.TList;
begin
  t:=List1; List1:=List2; List2:=t;
end;

function  ExpandedFilePath(const Path__,LeadingPath__:String):String; {throws EOutOfMemory}
var s:String;
begin
  Result:=Path__;
  if (Length(Result)<>0) and (Result[1]=PERIOD) then begin
     s:=ELLIPSES+FILE_NAME_PATH_DELIMITER+ExtractFileName(StrWithoutTrailingPathDelimiter(LeadingPath__));
     if StrBeginsWith(Result,s) then
        Result:=StrWithoutTrailingPathDelimiter(LeadingPath__)+Copy(Result,Succ(Length(s)),MaxInt);
     end;
end;

function  ExtractFileNameWithoutExt(const FileName:String):String;
begin
  Result:=ChangeFileExt(ExtractFileName(FileName),'');
  if (Result<>'') and (Result[Length(Result)]='.') then
     Delete(Result,Length(Result),1);
end;

{
procedure ExtractLevelsSoko15;
const PACK_FILE_NAME='c:\compiler\delphi4\pg\sokoban\Soko15.xsb';
var i,j:Integer; s:String; oCursor:TCursor;
    SL1,SL2:TStringList; Pack:TPack; Game:TGame;
begin
  SL1:=TStringList.Create;
  SL2:=TStringList.Create;
  Pack:=TPack.Create;
  Game:=TGame.Create;

  oCursor:=Screen.Cursor;
  try      Screen.Cursor:=crHourGlass;
           SL1.LoadFromFile('screens.dat');
           Pack.New(PACK_FILE_NAME);
           Pack.Close;
           for i:=0 to Pred(SL1.Count) do begin
               if (SL1.Strings[i]<>'') and
                  (SL1.Strings[i][1]='>') then begin
                  s:=StrWithBrackets(Trim(Copy(SL1.Strings[i],2,MaxInt)));
                  j:=Succ(i);
                  SL2.Clear;
                  while (j<SL1.Count) and
                        ((SL1.Strings[j]='') or (SL1.Strings[j][1]<>'>')) do begin
                        SL2.Add(SL1.Strings[j]); Inc(j);
                        end;
                  SL2.SaveToFile('temp1.xsb');
                  //SleepEx(100,False);
                  if Game.LoadFromFileOrPackOrClipBoard('temp1.xsb') then with Game do begin
                     TrimBoard(Board,PlayerPos,BoardWidth,BoardHeight);
                     SaveToFile(PACK_FILE_NAME+FILE_NAME_PATH_DELIMITER+s);
                     end;
                  //exit;
                  end;
               end;
  finally  Screen.Cursor:=oCursor;
           Pack.Free; SL1.Free; SL2.Free; Game.Free;
  end;
end;

procedure ExtractLevelsWGuy;
type TParseState=(psText,psLevel);
var i,Count,Size:Integer; Pack,Level,s:String; State:TParseState;
    f:File; o:TextFile; Buf,BufEnd,p:PChar;
begin
  Buf:=nil; Size:=0; Pack:=''; Count:=0; State:=psText; s:='';
  try
    AssignFile(f,'C:\temp\temp\WGuySrc\Levels\level.data');
    Reset(f,1);
    Size:=FileSize(f);
    GetMem(Buf,Size+2); BufEnd:=Buf+Size; p:=Buf;
    BlockRead(f,Buf^,Size,i);
    if i<>Size then raise Exception.Create('File read error');
    Close(f);
    while p<BufEnd do begin
      case State of
        psText   : begin if p^=CR then begin
                            Inc(p); State:=psLevel;
                            if s<>'' then begin
                               Pack:=s; Count:=0; s:='';
                               end;
                            end
                         else s:=s+p^;
                         Inc(p);
                   end;
        psLevel  : begin if   p^=NULL then begin
                              Inc(Count);
                              AssignFile(o,Format('Levels\%s %3.3d.sok',[Pack,Count]));
                              Rewrite(o);
                              Write(o,Level);
                              CloseFile(o);
                              Level:='';
                              Inc(p,3);
                              if (p<BufEnd) then begin
                                 if (p^<>SPACE) and (p^<>'#') then
                                    State:=psText;
                                 end;
                              end
                         else begin Level:=Level+p^;
                                    Inc(p);
                              end;
                   end;
      end; // case
      end;
  finally
    if Buf<>nil then FreeMem(Buf,Size+2);
  end;
end;
}
procedure ExtractLevelsWinSoko;
//var i,j,k,Len,No,x,y,AuthorCount:Integer; Ch:Char; Rle:TRle;
//    PlayerPos:TPoint;
//    s:String;
//    f:TextFile;
//    o:TextFile;
//    RoomAuthor:array[0..10000] of Integer;
//    Author:array[0..1000] of String;
begin{
  AuthorCount:=0;
  FillChar(RoomAuthor,SizeOf(RoomAuthor),0);
  try
    AssignFile(f,'C:\spil\sokoban\WinSoko\Roomsok.rdf');
    Reset(f);
    AssignFile(o,'WinSoko.xsb');
    Rewrite(o);
    Writeln(o,'[',PACKAGE_FILE_SECTION,']');
    Writeln(o,'Date Created="',DateTimeToStr(Now),'"');
    Writeln(o,'');
    try
//    Writeln(o,'[*Author*]');
      Reset(f);
      while not Eof(f) do begin
        Readln(f,s);
        if Copy(s,1,2)='#!' then begin
           i:=3; j:=AnsiPos('(c)',s);
           if j<>0 then begin
              Inc(AuthorCount);
              while i<j do begin
                while (i<j) and ((s[i]<'0') or (s[i]>'9')) do Inc(i);
                if i<j then begin
                   x:=0;
                   repeat x:=x*10+Ord(s[i])-Ord('0');
                          Inc(i);
                   until (i>=j) or (s[i]<'0') or (s[i]>'9');
                   y:=x;
                   while (i<j) and (s[i]=SPACE) do Inc(i);
                   if (i<j) and (s[i]='-') then begin
                      Inc(i);
                      while (i<j) and ((s[i]<'0') or (s[i]>'9')) do Inc(i);
                      if i<j then begin
                         y:=0;
                         repeat y:=y*10+Ord(s[i])-Ord('0');
                                Inc(i);
                         until (i>=j) or (s[i]<'0') or (s[i]>'9');
                         end;
                      end;
                   for k:=x to y do
                       if (k>=Low(RoomAuthor)) and
                          (k<=High(RoomAuthor)) then
                          RoomAuthor[k]:=AuthorCount;
                   end;
                end;
              s:=Copy(s,j,Length(s));
              k:=AnsiPos('!',s);
//            if k<>0 then s[k]:='|';
//            Writeln(o,Format('%d="%s"',[AuthorCount,s]));
              if k<>0 then
                 s:=Trim(System.Copy(s,1,Pred(k)))+C_STYLE_IN_STRING_NEWLINE+Trim(System.Copy(s,Succ(k),MaxInt));
              if AuthorCount<=High(Author) then Author[AuthorCount]:=s;
              end;
           end;
        end;
//    Writeln(o);

      Reset(f);
      while not Eof(f) do begin
        Readln(f,s); Len:=Length(s);
        if Copy(s,1,5)='Room_' then begin
           No:=0; i:=6;
           while (s[i]>='0') and (s[i]<='9') do begin
             No:=No*10+(Ord(s[i])-Ord('0')); Inc(i);
             end;
           if (i<Len) and (s[i]='=') and
              (s[Succ(i)]='"') and (s[Len]='"') then begin
              s:=Copy(s,i+2,Len-i-2); Len:=Length(s);
              PlayerPos.x:=0; PlayerPos.y:=0;
              for i:=1 to 2 do
                  if   s[i]>='a' then
                       PlayerPos.y:=PlayerPos.y*16+Ord(s[i])-Ord('a')+10
                  else PlayerPos.y:=PlayerPos.y*16+Ord(s[i])-Ord('0');
              for i:=3 to 4 do
                  if   s[i]>='a' then
                       PlayerPos.x:=PlayerPos.x*16+Ord(s[i])-Ord('a')+10
                  else PlayerPos.x:=PlayerPos.x*16+Ord(s[i])-Ord('0');
              RleInit(Rle); x:=0; y:=1;
              for i:=5 to Len do begin
                  Ch:=s[i];
                  case Ch of
                    '0'    : Ch:=FLOOR_NON_BLANK_CH;
                    '1'    : Ch:=GOAL_CH;
                    '2'    : Ch:=WALL_CH;
                    '3'    : Ch:=BOX_CH;
                    '4'    : Ch:=BOX_GOAL_CH;
                    'f'    : begin Ch:=SOBOBAN_STYLE_IN_STRING_NEWLINE; Inc(y); x:=-1;
                             end;
                    else   raise Exception.Create(Format('Unknown character in room %d: %s',[No,Ch]));
                  end; // case
                  Inc(x);
                  if (x=PlayerPos.x) and (y=PlayerPos.y) then
                     if Ch=FLOOR_NON_BLANK_CH then Ch:=PLAYER_CH
                     else if Ch=GOAL_CH then Ch:=PLAYER_GOAL_CH;
                  RleAdd(Rle,Ch);
                  end;
              RleFlush(Rle);
              Writeln(o,Format('[Room %d]',[No]));
              Writeln(o,Format('Problem="%s"',[Rle.Str]));
              if (No>=Low(RoomAuthor)) and
                 (No<=High(RoomAuthor)) and
                 (RoomAuthor[No]<>0) and
                 (RoomAuthor[No]<=High(Author)) then begin
//               Writeln(o,Format('Author="%d"',[RoomAuthor[No]]));
                 Writeln(o,Format('Information="%s"',[Author[RoomAuthor[No]]]));
                 end;
              end;
           end;

        end;
    finally
      Close(f);
      Close(o);
    end;
  except
  on E:Exception do Error(E.Message);
  end;}
end;

function  ExtractMaskFromFilter(const Filter:String; FilterIndex:Integer):String;
var i:Integer;
begin
  if FilterIndex<=0 then FilterIndex:=1;
  Inc(FilterIndex,FilterIndex);
  Result:=Filter; i:=AnsiPos(BAR,Result);
  while (i>0) and (FilterIndex>1) do begin
    Delete(Result,1,i); Dec(FilterIndex); i:=AnsiPos(BAR,Result);
    end;
  if i>0 then Delete(Result,i,MaxInt);
end;

procedure ExtractSokoFunTiles;
var i:Integer; b1,b2:TBitMap;

  function  XYRect(X,Y,Width,Height:Integer):TRect;
  begin
    Result:=Rect(X*Width,Y*Width,Succ(X)*Width,Succ(Y)*Width);
  end;

  function  XYRect1(X,Y,Width,Height:Integer):TRect;
  begin
    Result:=Rect(Succ(X*Width),Succ(Y*Width),Succ(Succ(X)*Width),Succ(Succ(Y)*Width));
  end;

begin // ExtractSokoFunTiles
  b1:=TBitMap.Create;
  b2:=TBitMap.Create;
  b2.Width:=7*51+1; b2.Height:=5*51+1;
  b2.Canvas.Brush.Color:=clLtGray;
  b2.Canvas.FillRect(Rect(0,0,b2.Width,b2.Height));
  b2.Canvas.Pen.Color:=clYellow;
  for i:=0 to 7 do with b2.Canvas do begin
      MoveTo(i*51,0); LineTo(i*51,b2.Height);
      end;
  for i:=0 to 5 do with b2.Canvas do begin
      MoveTo(0,i*51); LineTo(b2.Width,i*51);
      end;

  b1.LoadFromFile('c:\spil\sokoban\sokofun\tiles 4.bmp');

  b2.Canvas.CopyRect(XYRect1( 0,0,51,51),b1.Canvas,XYRect(0,0,51,51)); // men
  b2.Canvas.CopyRect(XYRect1( 1,0,51,51),b1.Canvas,XYRect(7,6,51,51));

  b2.Canvas.CopyRect(XYRect1( 0,1,51,51),b1.Canvas,XYRect(0,2,51,51)); // goals
  b2.Canvas.CopyRect(XYRect1( 1,1,51,51),b1.Canvas,XYRect(2,2,51,51));

  b2.Canvas.CopyRect(XYRect1( 0,2,51,51),b1.Canvas,XYRect(0,4,51,51)); // boxes
  b2.Canvas.CopyRect(XYRect1( 0,3,51,51),b1.Canvas,XYRect(1,4,51,51)); // boxes on goal

  b2.Canvas.CopyRect(XYRect1( 2,2,51,51),b1.Canvas,XYRect(4,4,51,51)); // colored boxes
  b2.Canvas.CopyRect(XYRect1( 3,2,51,51),b1.Canvas,XYRect(5,4,51,51));
  b2.Canvas.CopyRect(XYRect1( 4,2,51,51),b1.Canvas,XYRect(6,4,51,51));
  b2.Canvas.CopyRect(XYRect1( 5,2,51,51),b1.Canvas,XYRect(7,4,51,51));


  b2.Canvas.CopyRect(XYRect1( 0,4,51,51),b1.Canvas,XYRect(5,2,51,51)); // walls
  b2.Canvas.CopyRect(XYRect1( 1,4,51,51),b1.Canvas,XYRect(6,7,51,51));
  b2.Canvas.CopyRect(XYRect1( 2,4,51,51),b1.Canvas,XYRect(4,2,51,51));

  b1.LoadFromFile('c:\spil\sokoban\sokofun\tiles 7.bmp');

  b2.Canvas.CopyRect(XYRect1( 1,2,51,51),b1.Canvas,XYRect(0,4,51,51)); // boxes
  b2.Canvas.CopyRect(XYRect1( 1,3,51,51),b1.Canvas,XYRect(1,4,51,51)); // boxes on goal

  b1.LoadFromFile('c:\spil\sokoban\sokofun\tiles.bmp');

  b2.Canvas.CopyRect(XYRect1( 2,0,51,51),b1.Canvas,Rect(6*51+1,6*51-1,7*51+1,7*51-1));
//                                                XYRect(6,6,51,51)); // men

  b2.Canvas.CopyRect(XYRect1( 2,1,51,51),b1.Canvas,XYRect(0,2,51,51)); // targets

  b2.Canvas.CopyRect(XYRect1( 6,2,51,51),b1.Canvas,XYRect(0,4,51,51)); // boxes
  b2.Canvas.CopyRect(XYRect1( 6,3,51,51),b1.Canvas,XYRect(1,4,51,51)); // boxes on goal

  b2.Canvas.CopyRect(XYRect1( 2,3,51,51),b1.Canvas,XYRect(4,4,51,51)); // colored boxes
  b2.Canvas.CopyRect(XYRect1( 3,3,51,51),b1.Canvas,XYRect(5,4,51,51)); // on goals
  b2.Canvas.CopyRect(XYRect1( 4,3,51,51),b1.Canvas,XYRect(6,4,51,51));
  b2.Canvas.CopyRect(XYRect1( 5,3,51,51),b1.Canvas,XYRect(7,4,51,51));

  b2.Canvas.CopyRect(XYRect1( 4,4,51,51),b1.Canvas,XYRect(5,2,51,51)); // walls

  b1.LoadFromFile('c:\spil\sokoban\sokofun\tiles6.bmp');

  b2.Canvas.CopyRect(XYRect1( 3,4,51,51),b1.Canvas,XYRect(4,2,51,51)); // walls

  with b2.Canvas do begin
    Pen.Color:=clBlack;
    MoveTo(52,1); LineTo(52,51); MoveTo(103,1); LineTo(103,2);
    end;

  b2.SaveToFile('SokoFunTiles.bmp');
  b1.Free;
  b2.Free;
end;

function  FileAge(const FileName__:String): Integer;
begin
  {$WARNINGS OFF}
    Result:=SysUtils.FileAge(FileName__); {Symbol 'FileAge' is deprecated}
  {$WARNINGS ON}
end;

function  FileExists(const FileName__:String): Boolean;
begin
  try    Result:=SysUtils.FileExists(FileName__); // returns false on all types of exceptions
  except Result:=False;
  end;
end;

function  FileHasMoved(var FileName__:String):Boolean;
var s:String;
begin // check if a file has moved from the program folder to the user's document folder
  Result:=False;
  s:=ExtractFilePath(Application.ExeName);
  if StrBeginsWith(FileName__,s) then begin
     s:=StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath)+Copy(FileName__,Succ(Length(s)),MaxInt);
     Result:=FileExists(s);
     if Result then begin
        //Msg('The file:'+NL+FileName__+NL+'has moved to:'+NL+s,Application.Title,MB_OK+MB_ICONINFORMATION);
        FileName__:=s;
        end;
     end;
end;

procedure FileListBoxScrollInView(FileListBox:TFileListBox; Index:Integer);
var i,j,VisibleItemCount:Integer;
begin
  with FileListBox do begin
    VisibleItemCount:=ClientHeight div Max(1,ItemHeight);
    if (TopIndex+VisibleItemCount<=Index) or (TopIndex>Index) then
       begin i:=0; j:=Max(1,VisibleItemCount);
             while i+VisibleItemCount<=Index do Inc(i,j);
             TopIndex:=Min(i,Pred(Items.Count));
       end;
    end;
end;

function  FileStartsWith(const FileName,S:String):Boolean;
var i,j:Integer; f:File; Buf:array[0..1023] of Char;
begin
  Result:=FileExists(FileName);
  if Result then
     try    FillChar(Buf,SizeOf(Buf),0);
            i:=Min(SizeOf(Buf)-SizeOf(Buf[0]),Length(s));
            AssignFile(f,FileName); Reset(f,1);
            try     BlockRead(f,Buf,i,j);
            finally CloseFile(f);
            end;
            Result:=(i=j) and (AnsiCompareText(s,String(Buf))=0);
     except on E:Exception do begin Result:=False; Error(E.Message,Application.Title); end;
     end;
end;

{// not used, because 'FindNext' does not sort the entries; use a 'TFileListBox' instead
function FindPriorNextFileName(const FileName:String; Prior:Boolean):String;
var FindResult:Integer;
    Path,FileNameWithoutPath,FirstFileName,PriorFileName:String;
    SearchRec:TSearchRec;
begin
  Result:=''; FirstFileName:=''; PriorFileName:='';
  Path:=ExtractFilePath(FileName);
  FileNameWithoutPath:=Copy(FileName,Succ(Length(Path)),Length(FileName));
  FindResult:=FindFirst(Path+'*'+ExtractFileExt(FileName),faAnyFile,SearchRec);
  if FindResult=0 then
     try
             FirstFileName:=SearchRec.FindData.CFileName;
             repeat
               if AnsiCompareText(FileNameWithoutPath,SearchRec.Name)=0 then begin
                  if   Prior then begin
                       if   PriorFileName='' then // first file
                            begin FindResult:=FindNext(SearchRec);
                                  while FindResult=0 do begin
                                    PriorFileName:=SearchRec.FindData.CFileName;
                                    FindResult:=FindNext(SearchRec);
                                    end;
                            end;
                       if PriorFileName<>'' then Result:=Path+PriorFileName;
                       end
                  else begin FindResult:=FindNext(SearchRec);
                             if   FindResult=0 then
                                  Result:=Path+SearchRec.FindData.CFileName
                             else if PriorFileName<>'' then
                                     Result:=Path+FirstFileName;
                       end;
                  end
               else begin PriorFileName:=SearchRec.FindData.CFileName;
                          FindResult:=FindNext(SearchRec);
                    end;
             until (FindResult<>0) or (Result<>'');
             end;
     finally FindClose(SearchRec);
     end;
end;
}

function  FontsIdentical(Font1,Font2:TFont):Boolean;
begin
  Result:=(Font1.Name =Font2.Name ) and
          (Font1.Size =Font2.Size ) and
          (Font1.Style=Font2.Style) and
          (Font1.Color=Font2.Color);
end;

function  FormatItemAsNameAndPath(const Item__:String; var Name__,Path__:String):String;
const MIN_PATH_LENGTH_FOR_PRETTY_PRINTING=Succ(Length(ELLIPSES+FILE_NAME_PATH_DELIMITER));
begin
  if IsAnIniFileSectionFileName(Item__) then begin
     Name__:=ExtractIniFileName(Item__);
     Path__:=ExtractFilePath(Name__);
     Name__:=Copy(Name__,Succ(Length(Path__)),MaxInt)+' : '+ExtractSectionName(Item__);
     end
  else begin
     Path__:=ExtractFilePath(Item__);
     Name__:=Copy(Item__,Succ(Length(Path__)),MaxInt);
     end;
  if Length(Path__)>=MIN_PATH_LENGTH_FOR_PRETTY_PRINTING then
     Result:=Name__+'  ('+Path__+')'
  else begin
     Name__:=Path__+Name__;
     Path__:='';
     Result:=Name__;
     end;
end;

function FormatTimeMS(TimeMS__:TTimeMS):String;
var Hours,Minutes,Seconds:Cardinal;
begin
  // convert time to seconds, rounded
  if High(TimeMS__)-500>=TimeMS__ then Inc(TimeMS__,500);
  Seconds:=TimeMS__ div 1000;
  Hours  :=Seconds  div (60*60); Dec(Seconds,Hours  *60*60);
  Minutes:=Seconds  div 60;      Dec(Seconds,Minutes*60);
  Result :=Format('%.2d:%2.2d:%2.2d',[Hours,Minutes,Seconds]);
end;

var
  AnotherInstanceWindowHandle:HWND; {global variable for 'GetAnotherInstanceWindowHandle'}

function  GetAnotherInstanceWindowHandle(var AnotherInstanceWindowHandle__:HWND):Boolean; {Returns 'True' if another instance exists}

    function EnumerateWindows(Handle__:HWND; ApplicationDefinedValue__:LParam): Boolean; StdCall;
    const // caution: following constants must match definitions in 'TApplication' and 'TMainForm'
      SOKOBAN_MAIN_WINDOW_NAME         = 'TMainForm';
      SOKOBAN_MAIN_WINDOW_TITLE_PREFIX = 'Sokoban - ';
      TEXT_BUFFER_SIZE                 = 8*1024;
    var Length:Integer; TextBuffer:array[0..TEXT_BUFFER_SIZE] of Char;
    begin
      Result := True;
      Length:=GetClassName(Handle__,TextBuffer,TEXT_BUFFER_SIZE);
      if (Length>0) and (AnsiStrLComp(TextBuffer,SOKOBAN_MAIN_WINDOW_NAME,System.Length(SOKOBAN_MAIN_WINDOW_NAME))=0) then begin
         Length:=GetWindowText(Handle__, TextBuffer, TEXT_BUFFER_SIZE);
         if (Length > 0) and
            (AnsiStrLComp(TextBuffer, SOKOBAN_MAIN_WINDOW_TITLE_PREFIX,System.Length(SOKOBAN_MAIN_WINDOW_TITLE_PREFIX))=0) and
            ((not Assigned(MainForm)) or (Handle__<>MainForm.Handle)) then begin
            AnotherInstanceWindowHandle:=Handle__;
            Result:=False;      // Stop 'EnumerateWindows'
            end;
         end;
    end;

begin {GetAnotherInstanceWindowHandle}
  AnotherInstanceWindowHandle:=0;
  EnumWindows(Addr(EnumerateWindows),0);
  AnotherInstanceWindowHandle__:=AnotherInstanceWindowHandle;
  Result:=AnotherInstanceWindowHandle__<>0;
end;

function  GetApplicationVirtualFolder:String;
begin
  Result:=StrWithTrailingPathDelimiter(GetFolderPath(CSIDL_LOCAL_APPDATA))+'VirtualStore\Program Files\'+DEFAULT_APPLICATION_FOLDER_NAME;
end;

function  GetAvailablePhysicalMemoryByteSize:Cardinal;
var MemoryStatusEx:TMemoryStatusEx;
begin
  MemoryStatusEx.dwLength:=SizeOf(MemoryStatusEx);
  GlobalMemoryStatusEx(MemoryStatusEx);
  if   MemoryStatusEx.ullAvailPhys<=High(Result) then
       Result:=MemoryStatusEx.ullAvailPhys
  else Result:=High(Result);
end;

function  GetAvailableVirtualMemoryByteSize:Cardinal;
var MemoryStatusEx:TMemoryStatusEx;
begin
  MemoryStatusEx.dwLength:=SizeOf(MemoryStatusEx);
  GlobalMemoryStatusEx(MemoryStatusEx);
  if   MemoryStatusEx.ullAvailVirtual<=High(Result) then
       Result:=MemoryStatusEx.ullAvailVirtual
  else Result:=High(Result);
end;

function  GetCurrentThreadStack( var Address__  : Pointer;
                                 var ByteSize__ : UInt ) : Boolean;
var MemoryBasicInformation : TMemoryBasicInformation;
begin // returns the stack address and size for the current thread; the true
      // size is smaller because the operating system reserves a guard memory
      // page for catching stack overflows;
  Result := VirtualQuery( Addr( MemoryBasicInformation ), // address of region
                          MemoryBasicInformation,         // information buffer
                          SizeOf( MemoryBasicInformation ) ) <> 0;
  if Result then begin
     Address__  := MemoryBasicInformation.AllocationBase;
     ByteSize__ := UInt( MemoryBasicInformation.BaseAddress ) -
                   UInt( MemoryBasicInformation.AllocationBase );
     end
  else begin
     Address__  := nil;
     ByteSize__ := 0;
     end;
end;

procedure GetDrivesOfType(Type1,Type2:Integer; var ComboBox:TComboBox);
var i:Integer;Drive:Char;
begin
  ComboBox.Items.Clear;
  for Drive:='A' to 'Z' do begin
      i:=GetDriveType(PChar(Drive+':\'));
      if (i=Type1) or (i=Type2) then  // if any drive: <>1
         ComboBox.Items.Add(Drive+':');
      end;
  if ComboBox.Items.Count>0 then ComboBox.ItemIndex:=0;
end;

function  GetPhysicalMemoryByteSize:Cardinal;
var MemoryStatus:TMemoryStatus;
begin
  MemoryStatus.dwLength:=SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result:=MemoryStatus.dwTotalPhys;
end;

function  GetFolderPath(Folder__:Integer):String;
const
  SHGFP_TYPE_CURRENT = 0;
  SHGFP_TYPE_DEFAULT = 1;
var
  DLLHandle          :THandle;
  PathCharBuffer     :array[0..MAX_PATH] of Char;
  SHGetFolderPath    :function(hwndOwner:HWND; nFolder:Integer; hToken:THANDLE; Flags:Cardinal; pszPath:PChar):HResult; stdcall;
begin
  Result:='';
  try    DLLHandle:=LoadLibrary('shell32.dll');
         if DLLHandle<>0 then
            try     SHGetFolderPath:=GetProcAddress(DLLHandle,'SHGetFolderPathA');
                    if Assigned(SHGetFolderPath) then begin
                       PathCharBuffer[Low(PathCharBuffer)]:=NULL_CHAR;
                       SHGetFolderPath(0,
                                       Folder__,
                                       0,
                                       SHGFP_TYPE_CURRENT,
                                       PathCharBuffer);
                       if StrLen(PathCharBuffer)<>0 then begin // 'True': the buffer is non-empty if 'SHGetFolderPath' succeeds
                          Result:=PathCharBuffer;
                          end;
                       end;
            finally FreeLibrary(DLLHandle);
            end;
  except on E:Exception do Result:='';
  end;
end;

function  GetMediaPath(Folder__:Integer):String;
begin
  Result:=StrWithoutTrailingPathDelimiter(GetFolderPath(Folder__));
  if (Result='') or (not DirectoryExists(Result)) then begin
     Result:=StrWithoutTrailingPathDelimiter(GetFolderPath(CSIDL_PERSONAL)); // try 'My Documents'
     if (Result='') or (not DirectoryExists(Result)) then
        if   RegGetKey(HKEY_LOCAL_MACHINE,REGINIFILE_WINDOWS_PATH,KEY_CURRENT_VERSION,KEY_MEDIAPATH,'',Result) and // fallback to 'Windows\Media'
             DirectoryExists(Result) then
             begin Result:=AnsiLowerCase(StrWithTrailingPathDelimiter(Result));
                   //if Result<>'' then PlaySound(Result+SOUND_NOTIFY_FILENAME);
             end
        else Result:='';
     end;
end;

function  GetScreenSaverEnabled(var Enabled__,PasswordProtected__:Boolean):Boolean;
const SPI_GETSCREENSAVESECURE=$76;
var   BooleanReturnValue:BOOL;
begin
  Result:=SystemParametersInfo(SPI_GETSCREENSAVEACTIVE,0,PBOOL(Addr(BooleanReturnValue)),0);
  Enabled__:=BooleanReturnValue;
  if   SystemParametersInfo   (SPI_GETSCREENSAVESECURE,0,PBOOL(Addr(BooleanReturnValue)),0) then
       PasswordProtected__:=BooleanReturnValue
  else PasswordProtected__:=False; // this can be wrong; some old Windows systems supports password protection, but not the 'SPI_GETSCREENSAVESECURE' parameter for the 'SystemParametersInfo' function
end;

function  GridColIndexOf(const Grid:TStringGrid; ColNo:Integer; const s:String):Integer;
begin
  with Grid do
    for Result:=0 to Pred(Grid.RowCount) do
        if AnsiCompareText(Cells[ColNo,Result],s)=0 then exit;
  Result:=-1;
end;

procedure GridScrollInView(Grid:TStringGrid; ARow:Integer);
//var i,j:Integer;
begin
  with Grid do
    if (TopRow+VisibleRowCount<=ARow) or (TopRow>ARow) then
       begin //i:=FixedRows; j:=Max(1,VisibleRowCount);
             //while i+VisibleRowCount<=ARow do Inc(i,j);
             //TopRow:=Min(i,Pred(RowCount));
             TopRow:=Max(FixedRows,
                         Min(RowCount-VisibleRowCount,
                             ARow-(Max(0,Pred(VisibleRowCount)) div 2))); // center the row
       end;
end;

function  HorizontalScrollBarHeight:Integer;
begin
  Result:=GetSystemMetrics(SM_CYHSCROLL);
end;


procedure HeapStatistics;
{$WARNINGS OFF}
  var HeapStatus:THeapStatus; // Warning: Symbol THeapStatus is deprecated
{$WARNINGS ON}
begin
 {$WARNINGS OFF}
   HeapStatus:=GetHeapStatus; // Warning: Symbol 'GetHeapStatus' is specific to a platform
 {$WARNINGS ON}
   with HeapStatus do
      begin
         Application.MessageBox(
           PChar(Format(HeapStatisticsFormatText__,
                        //[TotalAddrSpace,TotalUnCommitted,TotalCommitted,
                        // TotalAllocated,TotalFree,FreeSmall,FreeBig,
                        // OverHead])),
                        [FreeSmall,FreeBig,TotalFree,OverHead,
                         TotalAllocated,TotalCommitted,TotalUnCommitted,
                         TotalAddrSpace,GetAvailableVirtualMemoryByteSize])
                ),
           PChar(Application.Title+SUB_TITLE_SEPARATOR+HeapStatisticsText),
           MB_OK);
      end;
end;

function  IntToHex(Value: Integer; Digits: Integer): String;
begin
  Result:=SysUtils.IntToHex(Value,Digits);
  if Digits<Length(Result) then Delete(Result,1,Length(Result)-Digits);
end;

function  IntToStrOrBlank(Int64__:Int64):String; {throws EOutOfMemory}
begin
  if   Int64__<>0 then Result:=Format('%d',[Int64__])
  else Result:='';
end;

function IsABase64EncodedText(const Text__,Last2EncodingCharacters__:String):Boolean;
var i,Length:Integer; Ch:Char;
begin
  Length:=System.Length(Text__);
  Result:=Length<>0;
  for i:=1 to Length do begin
      Ch:=Text__[i];
      if           ((Ch>='A') and (Ch<='Z'))
                   or
                   ((Ch>='a') and (Ch<='z'))
                   or
                   ((Ch>='0') and (Ch<='9'))
//                 or
//                 (Ch='+')
//                 or
//                 (Ch='/')
                   or
                   ((Ch=Base64EncodingPadding)
                    and
                    ((i=Length)
                     or
                     ((i=Pred(Length))
                      and
                      (Text__[Length]=Base64EncodingPadding)
                     )))
                   then begin
                   // ok
                   end
      else if      Last2EncodingCharacters__='' then begin // 'True': use standard encoding characters
                   if  (Ch='+') or (Ch='/') then begin
                       // ok
                       end
                   else begin
                      Result:=False; exit; // quick and dirty exit
                      end;
                   end
           else if System.Pos(Ch,Last2EncodingCharacters__)<>0 then begin
                   // ok
                   end
                else begin
                   Result:=False; exit; // quick and dirty exit
                   end
      end;
end;

function  IsAHighResolutionScreen:Boolean;
begin
  Result:=Screen.DeskTopWidth>=HIGH_RESOLUTION_SCREEN_WIDTH_THRESHOLD;
end;

function  IsEqualRects(const R1,R2:TRect):Boolean;
begin
  with R1 do Result:=(Left =R2.Left ) and (Top   =R2.Top   ) and
                     (Right=R2.Right) and (Bottom=R2.Bottom);
end;

function  IsANewerFile(const OldVersionFileName__,NewVersionFileName__:String):Boolean;
var FileAge1,FileAge2:Integer;
begin // returns 'True' if the file 'NewVersionFileName__' is newer than the file 'OldVersionFileName__'
  FileAge1:=FileAge(OldVersionFileName__);
  FileAge2:=FileAge(NewVersionFileName__);
  if   FileAge1<>-1 then
       if   FileAge2<>-1 then
            Result:=FileDateToDateTime(FileAge2)>FileDateToDateTime(FileAge1)
       else Result:=False
  else Result:=FileAge2<>-1;
end;

function  IsANewFileName(const FileName:String):Boolean;
var s:String;
begin
  Result:=(FileName='') or
          (FileName[               1]=TITLE_ILLEGAL_FIRST_CHARACTER) or
          (FileName[Length(FileName)]=TITLE_ILLEGAL_FIRST_CHARACTER);
  if not Result then begin
     s:=FileName;
     if IsAnIniFileSectionFileName(s) then s:=ExtractIniFileName(s);
     s:=ExtractFileNameWithoutPathAndExtension(s);
     Result:=(s<>'') and
             (s[                   1]=TITLE_ILLEGAL_FIRST_CHARACTER) and
             (s[Length(s)           ]=TITLE_ILLEGAL_FIRST_CHARACTER);
     end;
end;

function  IsKeyPressed(VirtualKeyCode__:Integer):Boolean;
begin
  Result:=(GetKeyState(VirtualKeyCode__) and $80)<>0;
end;

function  IsWindowsDefaultColorBtnFace(Color:TColor):Boolean;
begin
  Result:=(ColorToRgb(Color)=ColorToRGB(DEFAULT_WINDOWS_98_COLOR_BTN_FACE)) or
          (ColorToRgb(Color)=ColorToRGB(DEFAULT_WINDOWS_VISTA_COLOR_BTN_FACE)) or
          (ColorToRgb(Color)=ColorToRGB(DEFAULT_WINDOWS_XP_COLOR_BTN_FACE));
end;

function IsWindowsVistaOrNewerOperatingSystem:Boolean;
var osVerInfo: TOSVersionInfo;
begin
  with osVerInfo do begin
    dwOSVersionInfoSize:=SizeOf(osVerInfo);
    Result:=GetVersionEx(osVerInfo) and
            (dwPlatformId=VER_PLATFORM_WIN32_NT) and
            (dwMajorVersion>=6);
    end;
end;

function  KeepDataPathUpToDate(Path__:String):String;
begin
  if   (MainForm.OldApplicationDataPath<>'') and
       StrBeginsWith(Path__,MainForm.OldApplicationDataPath) then
       Result:=MainForm.ApplicationDataPath+Copy(Path__,Succ(Length(MainForm.OldApplicationDataPath)),MaxInt)
  else Result:=Path__;
end;

function  KeepPluginsPathUpToDate(Path__:String):String;
begin
  if   (MainForm.OldPluginsPath<>'') and
       StrBeginsWith(Path__,MainForm.OldPluginsPath) then
       Result:=MainForm.PluginsPath+Copy(Path__,Succ(Length(MainForm.OldPluginsPath)),MaxInt)
  else Result:=Path__;
end;

function  LinearInterpolation( Value__ : Double; Low__, High__ : Integer ) : Integer;
begin // precondition: 0.0 <= Value__ <= 1.0
  Result := Trunc( ( Value__ * High__ ) + ( ( 1.0 - Value__ ) * Low__ ) );
end;

function  ListToStrings(List:SokUtil_.TList; Strings:TStrings):Boolean;
var Nodes:TNode;
begin
  Strings.BeginUpdate;
  try     Result:=True; Nodes:=List.Items; Strings.Clear;
          try    while Nodes<>nil do begin
                  Strings.Add(Nodes.Text); Nodes:=Nodes.Next;
                  end;
          except on E:Exception do Result:=Error(E.Message,'');
          end;
          if not Result then Strings.Clear;
  finally Strings.EndUpdate;
  end;
end;

function  LoadComboBoxFromIniFile(const IniFile:TIniFile; const Section:String; Capacity:Integer; IsADirectoryComboBox,IsAFileComboBox,CalculateDropDownWidth,FormatItemAsNameAndPath:Boolean; ComboBox:TComboBox):Boolean;
var i:Integer; s,s1:String;
begin
  Result:=True; ComboBox.Clear;
  for i:=1 to Capacity do begin
        s:=IniFile.ReadString(Section,IntToStr(i),'');
        if   IsADirectoryComboBox then begin
             s:=StrWithoutTrailingPathDelimiter(s);
             if s<>'' then begin
                if MainForm.OldApplicationDataPath<>'' then
                   s:=StrWithoutTrailingPathDelimiter(KeepDataPathUpToDate   (StrWithTrailingPathDelimiter(s)));
                if MainForm.OldPluginsPath<>'' then
                   s:=StrWithoutTrailingPathDelimiter(KeepPluginsPathUpToDate(StrWithTrailingPathDelimiter(s)));
                end;
             end;
        if   s<>'' then begin
             s:=KeepDataPathUpToDate(KeepPluginsPathUpToDate(s));
             if   ComboBox.Items.IndexOf(s)=-1 then
                  if        IsADirectoryComboBox then
                            if DirectoryExists(ExpandedFilePath(s,MainForm.MyDocumentsFolder)) then ComboBox.Items.Add(s)
                            else
                  else if   IsAFileComboBox then begin
                            if   IsAnIniFileSectionFileName(s) then
                                 s1:=ExtractIniFileName(s)
                            else s1:=s;
                            if FileExists(ExpandedFilePath(s1,MainForm.MyDocumentsFolder)) then
                               ComboBox.Items.Add(s);
                            end
                       else ComboBox.Items.Add(s);
             end
        else break;
        end;
    if ComboBox.Items.Count>0 then ComboBox.ItemIndex:=0;
    if CalculateDropDownWidth then
       for i:=0 to Pred(ComboBox.Items.Count) do SetComboBoxDropDownWidth(ComboBox,i,FormatItemAsNameAndPath);
end;

function  LoadComboBoxFromStrings(Min__,Max__:Integer; const Texts__:array of string; Canvas__:TCanvas; ComboBox__:TComboBox):Boolean;
var i:Integer;
begin
  with ComboBox__ do begin
    Result:=True; Items.Clear; Text:='';
    for i:=Min__ to Max__ do Items.Add(Texts__[i]);
    i:=SokUtil_.Max(Width,
                    SokUtil_.Min(Screen.DeskTopWidth,
                                 MaxTextExtent(Canvas__,Texts__).cx {kludge: for some unknown reason, it's not possible to use 'ComboBox__.Canvas' here}
                                 +VerticalScrollBarWidth+8));
    if i<>Tag then begin // 'Tag' is used to store dropdown width
       Perform(CB_SETDROPPEDWIDTH,i,0);
       Tag:=i;
       end;
    end;
end;

function LoadStringGridColumnWidthsFromIniFile(const IniFile:TIniFile; const Section:String; var StringGrid:TSTringGrid):Boolean;
var ACol:Integer;
begin
  Result:=True;
  with StringGrid do
    for ACol:=0 to Pred(ColCount) do ColWidths[ACol]:=Max(2*VerticalScrollBarWidth,IniFile.ReadInteger(Section,'Column '+IntToStr(Succ(ACol)),ColWidths[ACol]));
end;

function  LoadFontFromIniFile(const IniFile:TIniFile; const Section,KeyPrefix:String; Font:TFont):Boolean;
var FontStyles:TFontStyles;
begin
  Result:=True;
  with IniFile do begin
    Font.Name:=ReadString(Section ,KeyPrefix+'FontName',Font.Name);
    Font.Color:=TColor(ReadInteger(Section,KeyPrefix+'FontColor',Integer(Font.Color)));
    Font.Size:=ReadInteger(Section,KeyPrefix+'FontSize',Font.Size);
    FontStyles:=[];
    if ReadBool(Section,KeyPrefix+'FontBold'     ,fsBold      in Font.Style) then Include(FontStyles,fsBold);
    if ReadBool(Section,KeyPrefix+'FontItalic'   ,fsItalic    in Font.Style) then Include(FontStyles,fsItalic);
    if ReadBool(Section,KeyPrefix+'FontUnderline',fsUnderline in Font.Style) then Include(FontStyles,fsUnderline);
    Font.Style:=FontStyles;
    end;
end;

function  LoadFormColorsFromIniFile(const IniFile:TIniFile; const Section:String; var FormColors:TFormColors):Boolean;
begin
  with FormColors do
    try
           BackgroundColor        :=TColor(IniFile.ReadInteger(Section,'BackgroundColor',Integer(BackgroundColor)));
           BackgroundTextColor    :=TColor(IniFile.ReadInteger(Section,'BackgroundTextColor',Integer(BackgroundTextColor)));
           HighlightedTextColor   :=TColor(IniFile.ReadInteger(Section,'HighlightedTextColor',Integer(HighlightedTextColor)));
           ButtonColor            :=TColor(IniFile.ReadInteger(Section,'ButtonColor',Integer(ButtonColor)));
           ButtonTextColor        :=TColor(IniFile.ReadInteger(Section,'ButtonTextColor',Integer(ButtonTextColor)));
           FocusedButtonColor     :=TColor(IniFile.ReadInteger(Section,'FocusedButtonColor',Integer(FocusedButtonColor)));
           FocusedButtonTextColor :=TColor(IniFile.ReadInteger(Section,'FocusedButtonTextColor',Integer(FocusedButtonTextColor)));
           GrayedButtonColor      :=TColor(IniFile.ReadInteger(Section,'GrayedButtonColor',Integer(GrayedButtonColor)));
           GrayedButtonTextColor  :=TColor(IniFile.ReadInteger(Section,'GrayedButtonTextColor',Integer(GrayedButtonTextColor)));
           WindowColor            :=TColor(IniFile.ReadInteger(Section,'WindowColor',Integer(WindowColor)));
           WindowTextColor        :=TColor(IniFile.ReadInteger(Section,'WindowTextColor',Integer(WindowTextColor)));
           FocusedWindowColor     :=TColor(IniFile.ReadInteger(Section,'FocusedWindowColor',Integer(FocusedWindowColor)));
           FocusedWindowTextColor :=TColor(IniFile.ReadInteger(Section,'FocusedWindowTextColor',Integer(FocusedWindowTextColor)));
           Result:=True;
    except on E:Exception do Result:=Error(E.Message,Application.Title);
    end;
end;

function  LoCase(Ch:Char):Char;
begin
  if (Ch>='A') and (Ch<='Z') then Result:=Chr(Ord(Ch)-Ord('A')+Ord('a'))
  else Result:=Ch;
end;

procedure MakeAllColumnsFullyVisible(StringGrid__:TStringGrid; MinColWidth__,VariableCol__:Integer);
var ACol,i,j,W:Integer;
begin
  with StringGrid__ do begin
    LeftCol:=FixedCols;

    if (VariableCol__>FixedCols) and (VariableCol__<ColCount) then begin
       W:=ClientWidth-ColCount*GridLineWidth;
       for ACol:=0 to Pred(ColCount)  do Dec(W,ColWidths[ACol]);
//     if  VisibleRowCount<RowCount then Dec(W,VerticalScrollBarWidth);
       if  W<>0 then ColWidths[VariableCol__]:=Max(ColWidths[VariableCol__]+W,MinColWidth__);
       end;

    if VisibleColCount<ColCount then begin
       repeat i:=-1; j:=-1; // find the column with highest width
              for ACol:=0 to Pred(ColCount) do
                  if (ColWidths[ACol]>j) and (ColWidths[ACol]>MinColWidth__) then begin
                     i:=ACol; j:=ColWidths[ACol];
                     end;
              if i>=0 then
                 repeat ColWidths[i]:=Pred(ColWidths[i]);
                 until  (VisibleColCount=ColCount) or (ColWidths[i]<=MinColWidth__);
       until  (VisibleColCount=ColCount) or (i<0);
       if (i>=0) and
          (GridLineWidth>0) and
          (ColWidths[i]-GridLineWidth>=MinColWidth__) then
          ColWidths[i]:=ColWidths[i]-GridLineWidth; // make the gridline after the last column visible
       end;
    end;
end;

function  MakeNewFileName(const FileName,Extension:String; UseParentheses:Boolean):String;
var i:Integer; s:String;
begin // use SPACE as 'Extension' parameter to produce a file name without an extension; if 'Extension' = '' then the produced filename contains a number as extension, e.g., 'FileName.999'
  s:=Trim(FileName);
  for i:=1 to Length(s) do // kludge: a filename cannot contain double-quotes
      if s[i]=DOUBLE_QUOTE then s[i]:=QUOTE;
  i:=Length(s);
  if ( i > 0 )
     and
     ( (s[ i ] = RIGHT_PAREN )
       or
       ( ( s[i] >= '0' ) and ( s[i] <= '9' ) ) ) then // skip old version numbers formatted as "FileName (999)" or "FileName 999"
     while (i>0)
           and
           ((s[i]=LEFT_PAREN)  or
            (s[i]=RIGHT_PAREN) or
            (s[i]=SPACE)       or
            ((s[i]>='0') and (s[i]<='9'))) do Dec(i);
  s:=Copy(s,1,i);

  i:=0;
  repeat Inc(i);
         if             Extension='' then
                        Result:=ChangeFileExt(s,PERIOD+IntToStr(i))
         else if        i=1 then
                        Result:=ExtractFilePath(s)+ExtractFileNameWithoutExt(s)+Trim(Extension)
              else if   UseParentheses then
                        Result:=ExtractFilePath(s)+ExtractFileNameWithoutExt(s)+
                                SPACE+LEFT_PAREN+IntToStr(i)+RIGHT_PAREN+Trim(Extension)
                   else Result:=ExtractFilePath(s)+ExtractFileNameWithoutExt(s)+
                                SPACE+IntToStr(i)+Trim(Extension);

  until  (not (FileExists(Result) or DirectoryExists(Result))) or (i=MaxInt);
  if i=MaxInt then Result:='';
end;

procedure MakeUniqueGrayedButtonTextColor(var FormColors:TFormColors);
var i:Integer;
begin // kludge: grayed button text color must be different from other button text colors
  i:=1;
  with FormColors do
    while (GrayedButtonTextColor=ButtonTextColor) or
          (GrayedButtonTextColor=FocusedButtonTextColor) do begin
      if (ColorToRGB(GrayedButtonTextColor) and $ff)=$ff then i:=-1;
      GrayedButtonTextColor:=TColor(ColorToRGB(GrayedButtonTextColor)+i);
      end;
end;

function MaxTextExtent(Canvas:TCanvas; const Strings:array of String):TSize;
var i:Integer; Size:TSize;
begin
  Result.cx:=0; Result.cy:=0;
  for i:=Low(Strings) to High(Strings) do with Result do begin
      Size:=Canvas.TextExtent(Strings[i]);
      cx:=Max(cx,Size.cx);
      cy:=Max(cy,Size.cy);
      end;
end;

function  MoveFileOrFolder(const ExistingFileOrFolderName__,NewFileOrFolderName__:String):Boolean;
begin // constraint: a folder cannot move to a different volume
  Result:=Windows.MoveFile(PChar(ExistingFileOrFolderName__),PChar(NewFileOrFolderName__));
end;

function  MoveOrCopyFile(const ExistingFileName__,NewFileName__:String):Boolean;
begin
   Result:=False;
   if FileExists(ExistingFileName__) then begin
      Misc_.MoveFileOrFolder(ExistingFileName__,NewFileName__); SleepEx(10,False);
      if not FileExists(NewFileName__) then begin
         Windows.CopyFile(PChar(ExistingFileName__),PChar(NewFileName__),True); SleepEx(10,False);
         end;
      Result:=FileExists(NewFileName__) and
              SetFileAttributes(PChar(NewFileName__),FILE_ATTRIBUTE_NORMAL);
      end;
end;

function  MovesPerSecondText(MovesPerSecond__:Integer):String;
begin
 if   MovesPerSecond__=1 then
      Result:=IntToStr(MovesPerSecond__)+SPACE+MovePerSecondText
 else Result:=IntToStr(MovesPerSecond__)+SPACE+Text_.MovesPerSecondText;
end;

function  NetworkVolume(DriveChar: Char): string;
var // copy from 'FileCtrl'
  Buf: Array [0..MAX_PATH] of Char;
  DriveStr: array [0..3] of Char;
  BufferSize: DWORD;
begin
  BufferSize := sizeof(Buf);
  DriveStr[0] := UpCase(DriveChar);
  DriveStr[1] := ':';
  DriveStr[2] := #0;
  if WNetGetConnection(DriveStr, Buf, BufferSize) = WN_SUCCESS then
  begin
    SetString(Result, Buf, BufferSize);
    if DriveChar < 'a' then
      Result := AnsiUpperCaseFileName(Result)
    else
      Result := AnsiLowerCaseFileName(Result);
  end
  else
    Result := VolumeID(DriveChar);
end;

function  PathCompactPath(Canvas:TCanvas; const Path:String; PixelWidth:Integer):String;
var LeftPath,s:String;

  function Recurse(const LeftPath,RightPath:String; var ResultPath:String):Boolean;
  var Folder:String;
  begin
    Folder:=ExtractFileName(LeftPath);
    if Folder='' then begin
       ResultPath:=LeftPath;
       if ExtractFileName(RightPath)=RightPath then begin
          ResultPath:=StrWithTrailingPathDelimiter(ResultPath)+RightPath; Result:=True; // 'True': done
          end
       else begin
          if   ResultPath<>ELLIPSES then
               ResultPath:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(ResultPath)+ELLIPSES)
          else ResultPath:=StrWithTrailingPathDelimiter(ResultPath);
          Result:=False;
          end;
       end
    else begin
      Result:=Recurse(StrWithoutTrailingPathDelimiter(ExtractFilePath(LeftPath)),StrWithTrailingPathDelimiter(Folder)+RightPath,ResultPath);
      if not Result then begin
         Folder:=StrWithTrailingPathDelimiter(ResultPath)+RightPath; // 'Folder' is not necessarily really a folder here; it's just a vacant string variable; 'RightPath' typically contains right-side folders and a file name
         Result:=Canvas.TextWidth(Folder)<=PixelWidth;
         if Result then ResultPath:=Folder; // 'True': done, i.e., the path consisting of the left-side ellipses and the right-side folder names + file name (if any) doesn't exceed the pixel width limit
         end;
      end;
  end;

begin // 'PathCompactPath'
  // the windows 'PathCompactPath' function requires the 'shlwapi.dll' DLL file;
  // even though it's available on all but the oldest Windows versions, it may
  // not be available on all platforms, e.g., a Windows emulator, hence this
  // private function
  if Canvas.TextWidth(Path)<=PixelWidth then Result:=Path
  else begin
     s:=StrWithoutTrailingPathDelimiter(Path);
     LeftPath:=StrWithoutTrailingPathDelimiter(ExtractFilePath(s));
     if   LeftPath='' then Result:=s
     else Recurse(LeftPath,ExtractFileName(s),Result);
     if Length(s)<Length(Path) then Result:=StrWithTrailingPathDelimiter(Result);
     end;
end;

//procedure PlaySound(const FileName:String);
//begin
//  MMSystem.PlaySound(PChar(FileName),0,SND_FILENAME+SND_NOSTOP+SND_NODEFAULT+SND_NOWAIT);
//end;

function  QuickSort(Items:Pointer; ItemCount,ItemSize:Cardinal; CompareFunction:TCompareFunction):Boolean;
//
// This is a safe and highly optimized implementation of QuickSort:
// 1. It is safe because it is non-recursive.
//    (A 'standard textbook' implementation uses a recursive 'Partition'
//    procedure, which might crash due to a stack overflow).
//
//    Recursion is avoided by explicitly declaring a local stack.
//    This stack only needs a capacity of log2(High(ItemCount)) stack-items,
//    because the algorithm always pushes the largest interval
//    on the stack, and continues with sorting the smallest one.
//
//    Another safety-issue: All pointer-aritmetic calculations are
//    carefully guarded against overflow and underflow.
//
// 2. The pivot value is chosen as the median of 3 values. This
//    reduces the probability of selecting a bad value.
//
// 3. Insertion-sort is slightly faster than QuickSort for small
//    intervals, thus, QuickSort is only used for large intervals,
//    while a final insertion-sort handles the rest.
//
const
  BITS_PER_BYTE=8;
  STACK_CAPACITY=SizeOf(ItemCount)*BITS_PER_BYTE;
  MAX_ITEM_SIZE=65536;                                                 // modify this in order to sort larger items
  QUICKSORT_THRESHOLD_ITEMS=4;
type
  TStackItem=record Left,Right:Pointer; end;
  PPointer=^Pointer;
var
  Stack:array[0..STACK_CAPACITY+1] of TStackItem;                      // 0..+1 : first and last elements are not used, but pointers to them must be valid
  ItemBuffer:array[0..MAX_ITEM_SIZE-1] of Byte;                        // buffer used for swapping items
  Lo,Hi,Left,Right,Mid,LastItem:Pointer;
  StackTop:^TStackItem;
  LeftPartitionSize,RightPartitionSize,ThresholdBytes:Cardinal;
//i:Integer; Item1,Item2:Pointer;

  procedure Pop(var Left,Right:Pointer); {Inline}                      // unfortunately, 'Delphi-Pascal' cannot inline functions
  begin
    Dec(Cardinal(StackTop),SizeOf(StackTop^));                         // stack-pointer points after the top element
    Left:=StackTop.Left; Right:=StackTop.Right;
  end;

  procedure Push(Left,Right:Pointer); {Inline}
  begin
    StackTop.Left:=Left; StackTop.Right:=Right;
    Inc(Cardinal(StackTop),SizeOf(StackTop^));                         // stack-pointer points after the top element
  end;

  procedure Swap(Item1,Item2:Pointer); {Inline}
  begin
    System.Move(Item1^,ItemBuffer,ItemSize);
    System.Move(Item2^,Item1^    ,ItemSize);
    System.Move(ItemBuffer,Item2^,ItemSize);
  end;

begin // QuickSort
  Result:=(ItemCount<=1)
          or
          ((ItemSize<=MAX_ITEM_SIZE) and
           (ItemSize<=High(Cardinal(Items)) div ItemCount));
  if (ItemCount>1) and Result then begin
     StackTop      :=Addr(Stack[Low(Stack)]);                          // initialize stack-pointer
     Push(nil,nil);                                                    // first stack-element unused, but a pointer to it must be valid
     ThresholdBytes:=ItemSize*QUICKSORT_THRESHOLD_ITEMS;
     LastItem      :=Pointer(Cardinal(Items)+Pred(ItemCount)*ItemSize);

     Lo            :=Items;                                            // [Lo,Hi] is the interval to sort
     Hi            :=LastItem;
     repeat Left   :=Lo; Right := Hi;                                  // [Lo,Hi] is the interval to sort now

            // only quicksort intervals with more items than 'QUICKSORT_THRESHOLD_ITEMS'
            if Cardinal(Right) - Cardinal(Left) > ThresholdBytes then begin
               // calculating 'Mid' as '(Lo+Hi) div 2' might overflow; thus, it's calculated another way:
               Mid    :=Pointer(Cardinal(Left) +
                                ItemSize * ((Cardinal(Right)-Cardinal(Left)) div (ItemSize * 2)));
               if CompareFunction(Left,Mid  )  > 0 then Swap(Left,Mid);// sort items a,b
               if CompareFunction(Mid ,Right)  > 0 then begin          // sort items b,c
                  Swap(Mid,Right);
                  if CompareFunction(Left,Mid) > 0 then Swap(Left,Mid);// sort items a,b again
                  end;

               Inc(Cardinal(Left ),ItemSize);                          // skip testing first element against pivot (mid), since it's known to be '<='
               Dec(Cardinal(Right),ItemSize);                          // skip testing last  element against pivot (mid), since it's known to be '>='
               repeat  // the central loop
                 while CompareFunction(Left,Mid  ) < 0 do
                       Inc(Cardinal(Left ),ItemSize);                  // last  element will stop the loop, if no other elements will
                 while CompareFunction(Mid ,Right) < 0 do
                       Dec(Cardinal(Right),ItemSize);                  // first element will stop the loop, if no other elements will
                 if Cardinal(Left) <= Cardinal(Right) then begin
                    if Cardinal(Left) < Cardinal(Right) then begin
                       Swap(Left,Right);
                       if      Mid=Left  then Mid:=Right               // keep track of the position of the pivot element
                       else if Mid=Right then Mid:=Left;
                       end;
                    Inc(Cardinal(Left ),ItemSize);
                    Dec(Cardinal(Right),ItemSize);
                    end;
               until Cardinal(Left) > Cardinal(Right);                 // note: 'Left' and 'Right' are always legal, i.e., inside the vector, when 'ItemCount' > 1

               LeftPartitionSize    := Cardinal(Right) - Cardinal(Lo);
               RightPartitionSize   := Cardinal(Hi   ) - Cardinal(Left);

               if LeftPartitionSize >= RightPartitionSize then begin
                  if LeftPartitionSize  > 0 then begin
                     Push(Lo,Right); // sort [Lo,Right] later (left  partition)
                     Lo := Left;     // sort [Left,Hi ] now   (the smallest partition)
                     end
                  else
                     Pop(Lo,Hi);
                  end
               else begin            // right partion largest
                  Push(Left,Hi);     // sort [Left,Hi ] later (right partition)
                  Hi := Right;       // sort [Lo,Right] now   (the smallest partition)
                  end;
               end
            else Pop(Lo,Hi);

     until  StackTop=Addr(Stack[0]);                                   // until no more intervals on the stack

     if QUICKSORT_THRESHOLD_ITEMS<>0 then begin                         // if true: use insertion sort for small intervals
        // sort smallest item into place, so it can be used as a sentinel for the insertion sort
        Left := Items; Right := Left;
        repeat Inc(Cardinal(Right),ItemSize);
               if CompareFunction(Right,Left) < 0 then Left := Right;  // 'Left' points to smallest item
        until  (Right=LastItem) or                                     // until last item...
               (Cardinal(Right)=Cardinal(Items)+ThresholdBytes);       // or until 'theshold-items' has been checked
        if Left <> Items then Swap(Left,Items);                        // move smallest item to first position

        Left  := Items; Right := Left;                                 // insertion sort
        repeat Inc(Cardinal(Right),ItemSize);                          // 'Right' is current item
               while CompareFunction(Right,Left) < 0 do                // loop as long as 'Right' should be placed to the left of 'Left'
                 Dec(Cardinal(Left),ItemSize);
               Inc(Cardinal(Left),ItemSize);
               if Left <> Right then begin                             // right-item belongs at left-item's place
                  System.Move(Right^,ItemBuffer,ItemSize);             // 'Move' parameters: source,destination,bytes
                  // the following 'Move' (memory copy) is legal, because 'Move' handles overlaps correctly
                  System.Move(Left^ ,Pointer(Cardinal(Left)+ItemSize)^,Cardinal(Right)-Cardinal(Left));
                  System.Move(ItemBuffer,Left^,ItemSize);
                  Left := Right;
                  end;
        until  Right = LastItem;
        end;

{
     Item1:=Items; Item2:=Item1; Inc(Cardinal(Item2),ItemSize);        // testing the result, development only
     for i:=1 to Pred(ItemCount) do begin
         if CompareFunction(Item1,Item2)>0 then
            Msg('Internal Error','QuickSort',MB_OK);
         Item1:=Item2; Inc(Cardinal(Item2),ItemSize);
         end;
}
     end;
end; // QuickSort

{
// QuickSort Test

function CompareIntegers(a,b:Pointer):Integer;
begin
  Result:=PInteger(a)^-PInteger(b)^;
end;

const
  VECTOR_CAPACITY=1000000;
var
  Vector:array[0..VECTOR_CAPACITY-1] of Integer;

procedure QuickSortTest;
var
  i,Time:Cardinal;
begin
  RandSeed:=GetTickCount;
  for i:=Low(Vector) to High(Vector) do Vector[i]:=Random(VECTOR_CAPACITY);
  Time:=GetTickCount;
  QuickSort(Addr(Vector),VECTOR_CAPACITY,SizeOf(Vector[Low(Vector)]),CompareIntegers);
  Msg(IntToStr(GetTickCount-Time),MB_OK);
  for i:=Low(Vector) to Pred(High(Vector)) do
      if Vector[i]>Vector[Succ(i)] then
         raise Exception.Create('QuickSort: Internal Error');
end;
}

function  RangeCheck(Value,Min,Max:Integer):Boolean;
begin
  Result:=(Value>=Min) and (Value<=Max);
end;

function  RegGetKey(Root:HKey; const Path,Section,Key,DefaultValue:String; var Value:String):Boolean;
var IniFile:TRegIniFile;
begin
  Result:=False; Value:=DefaultValue;
  try IniFile:=TRegIniFile.Create(HKLM);
       try
         with IniFile do
           begin RootKey:=Root;
                 if OpenKeyReadOnly(Path) then 
                    Value:=ReadString(Section,Key,DefaultValue);
           end;
       finally IniFile.Free;
       end;
       Result:=True;
  except on E:Exception do Error(E.Message,Application.Title);
  end;
end;

function  RegSetKey(Root:HKey; const Path,Section,Key,Value:String):Boolean;
var IniFile:TRegIniFile; s:String;
begin
  Result:=False;
  try IniFile:=TRegIniFile.Create(HKLM);
       try
         with IniFile do
           begin RootKey:=Root;
                 if OpenKey(Path,False) then WriteString(Section,Key,Value)
                 else begin
                        if RootKey=HKEY_CURRENT_USER then s:=HKCU
                        else if RootKey=HKEY_LOCAL_MACHINE then s:=HKLM
                        else s:='?';
                        raise Exception.Create(RegistryKeyNotFoundText+NL+
                                s+'\'+Path);
                      end;
           end;
       finally IniFile.Free;
       end;
       Result:=True;
  except on E:Exception do Error(E.Message,Application.Title);
  end;
end;

function  RGB_BGR(const s:String):String;
var i:Integer; Ch:Char;
begin
  Result:=s;
  if Length(Result)>6 then Result:=Copy(Result,1,6)
  else while Length(Result)<6 do Result:='0'+Result;
  for i:=1 to 2 do
      begin Ch:=Result[i]; Result[i]:=Result[i+4]; Result[i+4]:=Ch; end;
end;

function  RectHeight(const R:TRect):Integer;
begin
  Result:=R.Bottom-R.Top;
end;

function  RectPlusOffset(const Rect:TRect; X,Y:Integer):TRect;
begin
  with Result do begin
    Left  :=Rect.Left  +X;
    Top   :=Rect.Top   +Y;
    Right :=Rect.Right +X;
    Bottom:=Rect.Bottom+Y;
    end;
end;

function  RectQuadrant(const Rect__:TRect; Quadrant__:TQuadrantType):TRect;
var W2,H2:Integer;
begin
  Result:=Rect__; W2:=Succ(RectWidth(Result)) div 2; H2:=Succ(RectHeight(Result)) div 2;
  with Result do
    case Quadrant__ of
      qTopLeft       : begin Right:=Left   + W2; Bottom:=Top   + H2; end;
      qTopRight      : begin Left :=Right  - W2; Bottom:=Top   + H2; end;
      qBottomLeft    : begin Top  :=Bottom - H2; Right :=Left  + W2; end;
      qBottomRight   : begin Top  :=Bottom - H2; Left  :=Right - W2; end;
    end; // case
end;

function  RectToStr(const Rect__:TRect):String;
begin
  with Rect__ do
    Result:=IntToStr(Left)+SPACE+IntToStr(Top)+SPACE+IntToStr(Right)+SPACE+IntToStr(Bottom);
end;

function  RectWidth (const R:TRect):Integer;
begin
  Result:=R.Right-R.Left;
end;

var RunNo__:Integer=0;

function  RunNo:Integer;
begin
  Inc(RunNo__); Result:=RunNo__;
end;
{
function  SafeStrToInt(const S:String; Hex:Boolean; var Value:Integer):Boolean;
begin
  try    if S<>'' then begin
            if   Hex and (S[1]<>'$') then
                 Value:=StrToInt('$'+s)
            else Value:=StrToInt(S);
            Result:=True;
            end
         else begin
            Value:=0; Result:=False;
            end;
  except on E:Exception do begin Value:=0; Result:=False; end;
  end;
end;
}
function  SafeStrToInt(const S:String; Hex:Boolean; var Value:Integer):Boolean;
var i,Digit,Len,Radix:Integer; Negated:Boolean; Ch:Char;
begin
  Value:=0; Len:=Length(S); Negated:=False; i:=1;
  while (i<=Len) and (S[i]<=SPACE) do Inc(i);
  if i<=Len then
     if      S[i]='+' then Inc(i)
     else if S[i]='-' then begin Negated:=True; Inc(i); end;
  Result:=i<=Len;
  if Result then begin
     if not Hex then Radix:=10
     else begin
        Radix:=16;
        if (i<Len) and (S[i]='$') then Inc(i);
        end;
     while (i<=Len) and Result do begin
       Ch:=S[i]; Digit:=Ord(Ch)-Ord('0'); Inc(i);
       if (Ch>='0') and (Ch<='9') then Result:=True
       else if Radix=16 then begin
               Ch:=UpCase(Ch);
               if   (Ch>='A') and (Ch<='F') then Digit:=10+Ord(Ch)-Ord('A')
               else Result:=False;
               end
            else Result:=False;
       if Result then
          if not Negated then
             if   Value <= (High(Value) - Digit) div Radix then
                  Value:=Value*Radix+Digit
             else Result:=False
          else
             if   Value >= (Low (Value) + Digit) div Radix then
                  Value:=Value*Radix-Digit
             else Result:=False;
       end;
    end;
end;

function  SaveComboBoxToIniFile(const IniFile:TIniFile; const Section:String; Capacity:Integer; const ComboBox:TComboBox):Boolean;
var i:Integer;
begin
  Result:=True;
  IniFile.EraseSection(Section);
  for i:=1 to Min(Capacity,ComboBox.Items.Count) do
      IniFile.WriteString(Section,IntToStr(i),ComboBox.Items[Pred(i)]);
end;

function  SaveFontToIniFile(const IniFile:TIniFile; const Section,KeyPrefix:String; const Font:TFont):Boolean;
begin
  Result:=True;
  with IniFile do begin
    WriteString (Section,KeyPrefix+'FontName'     ,Font.Name);
    WriteInteger(Section,KeyPrefix+'FontColor'    ,Integer(Font.Color));
    WriteInteger(Section,KeyPrefix+'FontSize'     ,Font.Size);
    WriteBool   (Section,KeyPrefix+'FontBold'     ,fsBold      in Font.Style);
    WriteBool   (Section,KeyPrefix+'FontItalic'   ,fsItalic    in Font.Style);
    WriteBool   (Section,KeyPrefix+'FontUnderline',fsUnderline in Font.Style);
    end;
end;

function  SaveFormColorsToIniFile(const IniFile:TIniFile; const Section:String; const FormColors:TFormColors):Boolean;
begin
  with FormColors do
    try
           IniFile.WriteInteger(Section,'BackgroundColor',Integer(BackgroundColor));
           IniFile.WriteInteger(Section,'BackgroundTextColor',Integer(BackgroundTextColor));
           IniFile.WriteInteger(Section,'ButtonColor',Integer(ButtonColor));
           IniFile.WriteInteger(Section,'ButtonTextColor',Integer(ButtonTextColor));
           IniFile.WriteInteger(Section,'FocusedButtonColor',Integer(FocusedButtonColor));
           IniFile.WriteInteger(Section,'FocusedButtonTextColor',Integer(FocusedButtonTextColor));
           IniFile.WriteInteger(Section,'GrayedButtonColor',Integer(GrayedButtonColor));
           IniFile.WriteInteger(Section,'GrayedButtonTextColor',Integer(GrayedButtonTextColor));
           IniFile.WriteInteger(Section,'WindowColor',Integer(WindowColor));
           IniFile.WriteInteger(Section,'WindowTextColor',Integer(WindowTextColor));
           IniFile.WriteInteger(Section,'FocusedWindowColor',Integer(FocusedWindowColor));
           IniFile.WriteInteger(Section,'FocusedWindowTextColor',Integer(FocusedWindowTextColor));
           IniFile.WriteInteger(Section,'HighlightedTextColor',Integer(HighlightedTextColor));
           Result:=True;
    except on E:Exception do Result:=Error(E.Message,Application.Title);
    end;
end;

function  SaveStringGridColumnWidthsToIniFile(const IniFile:TIniFile; const Section:String; const StringGrid:TSTringGrid):Boolean;
var ACol:Integer;
begin
 Result:=True;
 with StringGrid do
   for ACol:=0 to Pred(ColCount) do IniFile.WriteInteger(Section,'Column '+IntToStr(Succ(ACol)),ColWidths[ACol]);
end;

procedure ScaleKeepingAspectRatio(Width__,Height__,MaxWidth__,MaxHeight__:Integer; var NewWidth__,NewHeight__:Integer);
begin
  NewHeight__   :=MulDiv(Height__,MaxWidth__,Width__);
  NewWidth__    :=MaxWidth__;
  if NewHeight__> MaxHeight__ then begin
     NewWidth__ :=MulDiv(NewWidth__,MaxHeight__,NewHeight__);
     NewHeight__:=MaxHeight__;
     end;
end;

procedure SetDefaultDirectory;
begin
  SetCurrentDirectory(PChar(StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath)));
end;

procedure SetDefaultFormColors(var FormColors:TFormColors);
begin // color values: BGR, not RGB
  with FormColors do begin
    BackgroundColor         :=clNavy;//$EBCE87;              // skyblue //$ff901E; // dodgerblue
    BackgroundTextColor     :=clLime;//clBlack;
    HighlightedTextColor    :=clYellow;//clBlue;             //$2FFFAD; // greenyellow
    ButtonColor             :=$FF8000;//clGreen;             //EBCE87; // skyblue
    ButtonTextColor         :=clWhite;
    FocusedButtonColor      :=$00D7FF; // gold
    FocusedButtonTextColor  :=clBlue;
    GrayedButtonColor       :=clSilver;
    GrayedButtonTextColor   :=clGray;
    WindowColor             :=clBlack;//$CDFAFF; // lemonchiffon
    WindowTextColor         :=clLime;//clBlack;
    FocusedWindowColor      :=$2FFFAD; // greenyellow
    FocusedWindowTextColor  :=clBlack;
    end;
end;

procedure SetComboBoxDropDownWidth(ComboBox:TComboBox; Index:Integer; FormatItemAsNameAndPath:Boolean); // uses 'Tag' to store drop-down width
var i:Integer; ItemName,ItemPath:String;
begin // checks whether the item number 'Index' is a new longest item
  with ComboBox do
    if (Index>=0) and (Index<Items.Count)  then begin
       if   FormatItemAsNameAndPath then
            i:=Min(Screen.DeskTopWidth,MainForm.Canvas.TextWidth(Misc_.FormatItemAsNameAndPath(Items[Index],ItemName,ItemPath))+VerticalScrollBarWidth+8)
       else i:=Min(Screen.DeskTopWidth,MainForm.Canvas.TextWidth(                              Items[Index])                   +VerticalScrollBarWidth+8);
       if i>Tag then begin // 'Tag' is used to store dropdown width
          ComboBox.Perform(CB_SETDROPPEDWIDTH,i,0);
          Tag:=i;
          end;
       end;
end;

function SetCPUAffinity(UseOnlyOneProcessor__:Boolean):Boolean;
var
  CurrentProcess        : THandle;
  DLLHandle             : THandle;
  NewProcessAffinityMask: DWORD;
  ProcessAffinityMask   : DWORD;
  SystemAffinityMask    : DWORD;
  GetProcessAffinityMask: function(hProcess: THandle; var lpProcessAffinityMask, lpSystemAffinityMask: DWORD): BOOL; stdcall;
  SetProcessAffinityMask: function(hProcess: THandle; dwProcessAffinityMask: DWORD): BOOL; stdcall;
begin
  Result:=False;
  try    DLLHandle:=LoadLibrary('Kernel32.dll');
         if DLLHandle<>0 then
            try     GetProcessAffinityMask:=GetProcAddress(DLLHandle,'GetProcessAffinityMask');
                    SetProcessAffinityMask:=GetProcAddress(DLLHandle,'SetProcessAffinityMask');
                    if Assigned(GetProcessAffinityMask) and Assigned(SetProcessAffinityMask) then begin
                       CurrentProcess:=GetCurrentProcess;
                       if GetProcessAffinityMask(CurrentProcess,ProcessAffinityMask,SystemAffinityMask) then begin
                          if   UseOnlyOneProcessor__ then begin
                               NewProcessAffinityMask:=1; // search for the lowest numbered cpu among the ones the process is allowed to run on
                               while (NewProcessAffinityMask<>0) and ((NewProcessAffinityMask and ProcessAffinityMask)=0) do
                                     NewProcessAffinityMask:=NewProcessAffinityMask shl 1;
                               ProcessAffinityMask:=NewProcessAffinityMask;
                               end
                          else ProcessAffinityMask:=SystemAffinityMask; // allow the process to run on all cpus
                          if   ProcessAffinityMask<>0 then
                               Result:=SetProcessAffinityMask(CurrentProcess,ProcessAffinityMask);
                          end;
                       end;
            finally FreeLibrary(DLLHandle);
            end;
  except on E:Exception do Result:=False;
  end;
end;

procedure SetGroupBoxCaption(Canvas__:TCanvas; GroupBox__:TGroupBox; const Caption__:String);
var s:String;
begin
  s:=Caption__;
  while (Length(s)>4) and
        (Canvas__.TextWidth(s)>=GroupBox__.ClientWidth-16) do
        s:=Copy(s,1,Length(s)-4)+'...';
  if    s<>'' then
        if s<>GroupBox__.Caption then GroupBox__.Caption:=s
        else
  else  GroupBox__.Caption:=Caption__;
end;

function SetThreadExecutionState(Flags__:Cardinal):Boolean;
type
  TSetThreadExecutionState = function(Flags__:Cardinal):Cardinal; stdcall;
var
  DLLHandle:THandle;
  SetThreadExecutionState:TSetThreadExecutionState;
begin
  Result:=False;
  try    DLLHandle:=LoadLibrary('Kernel32.dll');
         if DLLHandle<>0 then
            try     SetThreadExecutionState:=GetProcAddress(DLLHandle,'SetThreadExecutionState');
                    if Assigned(SetThreadExecutionState) then begin
                       Result:=SetThreadExecutionState(Flags__)<>0;     // '<>0': the function call succeeded
                       if (not Result) and ((Flags__ and ES_AWAYMODE_REQUIRED)<>0) then
                          Result:=SetThreadExecutionState(Flags__ and (not ES_AWAYMODE_REQUIRED))<>0; // 'not ES_AWAYMODE_REQUIRED': the flag is not supported on some old Windows versions
                       //if not Result then Msg(TEXT_TASK_FAILED,'SetThreadExecutionState',MB_OK);
                       end;
            finally FreeLibrary(DLLHandle);
            end;
  except on E:Exception do Result:=False;
  end;
end;

function  Sign(i:Integer):Integer;
begin
  if i>0 then Result:=1
  else if i<0 then Result:=-1
  else Result:=0;
end;

procedure SimulateKeyboardEvent;
begin
  keybd_event(SIMULATED_KEYBOARD_EVENT_KEY,0,0,0);
  keybd_event(SIMULATED_KEYBOARD_EVENT_KEY,0,KEYEVENTF_KEYUP,0);
end;

function  StrHasAcceleratorChar(const Str__:String; ToUpperCase__:Boolean; var Key__:Char):Boolean;
var Index:Integer; S:String;
begin
  Index:=System.Pos(AMPERSAND,Str__);
  Result:=(Index<>0) and (Index<Length(Str__));
  if   Result then begin
       Key__:=Str__[Succ(Index)];
       if ToUpperCase__ then
          try    S:=AnsiUpperCase(Key__);
                 Key__:=S[1];
          except on E:Exception do Error(E.Message,Application.Title);
          end;
       end
  else Key__:=NULL_CHAR;
end;

function  StrAnsiPosCI(const SubStr__,Str__:String):Integer;
begin  // case insensitive 'AnsiPos'
  Result:=AnsiPos(AnsiLowerCase(SubStr__),AnsiLowerCase(Str__));
end;

function StrHexDigits(const Str__:String):String;
const HEX_DIGITS='01234567890abcdefABCDEF';
var i:Integer;
begin
  Result:=Str__;
  for i:=Length(Result) downto 1 do
      if AnsiPos(Result[i],HEX_DIGITS)=0 then Delete(Result,i,1);
end;

function  StrIndexOfCI(var s:String; const Strings:array of String):Integer;
var i:Integer;
begin
  for i:=Low(Strings) to High(Strings) do
      if AnsiCompareText(s,Strings[i])=0 then begin
         s:=Strings[i]; Result:=i; exit;
         end;
  Result:=-1;
end;

function StrIndexOfCI(var s:String; const Strings:TStrings):Integer;
var i:Integer;
begin
  if (s<>'') and (Strings<>nil) then with Strings do
     for i:=0 to Pred(Count) do
         if AnsiCompareText(s,Strings[i])=0 then begin
            s:=Strings[i]; Result:=i; exit;
            end;
  Result:=-1;
end;

function  StringsToList(Strings:TStrings; List:SokUtil_.TList):Boolean;
var i:Integer; n,Nodes:TNode;
begin
  Result:=True; Nodes:=nil;
  try    for i:=0 to Pred(Strings.Count) do
             if   Result and CreateObject(otNode,n) then begin
                  n.Next:=Nodes; Nodes:=n; Result:=n.SetText(Strings[i]);
                  end
             else Result:=False;
  except on E:Exception do Result:=Error(E.Message,'');
  end;
  if   Result then begin List.Clear; List.Items:=Nodes; List.Reverse; end
  else while Nodes<>nil do begin n:=Nodes.Next; Nodes.Destroy; Nodes:=n; end;
end;

function  StrLastPos(const SubString__,String__:String):Integer;
var Index,SubStringLength:Integer; s:String;
begin
  Result:=0;
  if (SubString__<>'') and (String__<>'') then begin
     s:=String__; SubStringLength:=Length(SubString__);
     Index:=AnsiPos(SubString__,s);
     while (Index<>0) and (s<>'') do begin
       Inc(Result,Index);
       s:=Copy(s,Index+SubStringLength,MaxInt);
       Index:=AnsiPos(SubString__,s);
       end;
     end;
end;

function StrLine(Length__:Integer):String;
var i:Integer;
begin
  SetLength(Result,Length__);
  for i:=1 to Length(Result) do Result[i]:=HYPHEN;
end;

function  StrRemoveChar(const s:String; Ch:Char):String;
var i:Integer;
begin
  Result:=s; i:=AnsiPos(Ch,Result);
  while i<>0 do begin Delete(Result,i,1); i:=AnsiPos(Ch,Result); end;
end;

function  StrBeginsWithChar(const s:String; Ch:Char):Boolean;
begin
  Result:=(s<>'') and (s[1]=Ch);
end;

function  StrBeginsWithDriveLetter(const Path__:String):Boolean;
var s:String;
begin
  s:=AnsiUpperCase(Path__);
  Result:=(Length(s)>=3) and
          (s[2]      =COLON) and
          (s[3]      =FILE_NAME_PATH_DELIMITER) and
          (s[1]     >='A') and
          (s[1]     <='Z');
end;

function  StrSubstituteChar(const s:String; Old,New:Char):String;
var i:Integer;
begin
  Result:=s;
  if Old<>New then begin
     i:=AnsiPos(Old,Result);
     while i<>0 do begin Result[i]:=New; i:=AnsiPos(Old,Result); end;
     end;
end;

procedure StrSubstituteLineSeparatorsBySpaces(Text__:PChar);
asm // precondition: the text buffer contains a null-terminated text string
// ->eax : pointer to text buffer
  push ebx
  push esi
  push edi

  mov esi,eax // esi := addr(text buffer)

  mov edi,eax // edi := addr(text buffer)
  mov ecx,0ffffffffH
  xor al,al
  repne scasb // search for null-terminator
  not ecx // one's complement of ecx
  dec ecx // ecx := length(text) + 1

  mov edi,esi // edi := addr(text buffer)
  inc esi // esi := addr(text buffer) + 1
  mov al,LF
  mov ah,CR
  mov bl,SPACE

@@loop:
  jecxz @@exit // end of text?
  repne scasb // search for LF
  jne @@exit
  mov [edi-1],bl // change LF to SPACE
  cmp [edi-2],ah // was it CR+LF?
  jnz @@loop
  cmp edi,esi // was LF the first character of the text?
  jz @@loop
  mov [edi-2],bl // change CR to SPACE
  jmp @@loop

@@exit:
  pop edi
  pop esi
  pop ebx
end;

function  StrToBool(const s:String):Boolean;
begin
  Result:=AnsiCompareText(s,BooleanText[True])=0;
end;

function  StrToFileName(const s:String):String;
var i:Integer; CharFilter:String;
begin
  Result:=s;
  for i:=1 to Length(Result) do // a filename cannot contain double-quotes
      if Result[i]=DOUBLE_QUOTE then Result[i]:=QUOTE;
  Result:=StrSubstituteCharacters(Result,SLASH,SPACE); // special: "/" is valid in level titles and snapshot titles, but not in file names
  CharFilter:=FILE_NAME_AND_PATH_NAME_ILLEGAL_CHARACTERS+
              LEFT_BRACKET+RIGHT_BRACKET+ // brackets are reserved for "filename/[levelname]" syntax
              TITLE_ILLEGAL_FIRST_CHARACTER+TITLE_ILLEGAL_CHARACTERS;
  for i:=1 to Length(CharFilter) do Result:=StrRemoveChar(Result,CharFilter[i]);
  for i:=1 to Length(Result) do
      if Result[i]<SPACE then Result[i]:=UNDERSCORE;
  if (Length(Result)>2) and (Result[1]=QUOTE) and (Result[Length(Result)]=QUOTE) then
     Result:=StrWithoutQuotes(Result);
end;

function StrToRect(const s:String; var Rect__:TRect):Boolean;
var i,Len,Field,Value:Integer; Ch:Char;
begin // a simple and not fool-proof implementation
  Len:=Length(s); Field:=0; i:=0;
  while i<Len do begin
    repeat Inc(i); Ch:=s[i];
           Result:=IsADigitChar(Ch);
    until  Result or (i=Len);
    if Result then begin
       Value:=0;
       repeat Value := Value *10 + Ord(Ch) - Ord('0');
              if   i<Len then begin
                   Inc(i); Ch:=s[i];
                   Result:=IsADigitChar(Ch);
                   end
              else Result:=False;
       until  not Result;
       Inc(Field);
       case Field of
         1: Rect__.Left  :=Value;
         2: Rect__.Top   :=Value;
         3: Rect__.Right :=Value;
         4: Rect__.Bottom:=Value;
       end; // case
       end;
    end;
  Result:=Field=4;
end;

function  StrWithQuotedAmpersands(const s:String):String;
var i:Integer;
begin
  Result:=s; i:=1;
  while (i<=Length(Result)) and (i<MaxInt) do begin
    if Result[i]=AMPERSAND then
       if   i=Length(Result) then
            if i<MaxInt-1 then Insert(AMPERSAND,Result,i)
            else
       else //if Result[Succ(i)]<>AMPERSAND then
                 begin
                   Inc(i); Insert(AMPERSAND,Result,i);
                 end;
    Inc(i);
    end;
end;

function StrWithoutTrailingCharacters(const Str__,Characters__:String):String; {throws EOutOfMemory}
var i:Integer;
begin
  i:=Length(Str__);
  while (i<>0) and (AnsiPos(Str__[i],Characters__)<>0) do Dec(i);
  Result:=System.Copy(Str__,1,i);
end;

function  TitleWithOptionalSubTitle(const Title__,SubTitle__:String):String;
begin
  try    if   SubTitle__<>'' then Result:=Title__+SUB_TITLE_SEPARATOR+SubTitle__
         else Result:=Title__;
  except on E:Exception do Result:=Title__;
  end;
end;

function  VerticalScrollBarWidth:Integer;
begin
  Result:=GetSystemMetrics(SM_CXVSCROLL);
end;

function  VisualFileName(const FileName:String):String;
begin
  if   IsAnIniFileSectionFileName(FileName) then
       Result:=ExtractSectionName(FileName)
  else Result:=ExtractFileNameWithoutExt(FileName);
end;

function VolumeID(DriveChar: Char): string;
var OldErrorMode: Integer; NotUsed, VolFlags: DWORD;
    Buffer: array [0..MAX_PATH] of Char;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try     Buffer[0] := #$00;
          if   GetVolumeInformation(PChar(DriveChar + ':\'), Buffer, DWORD(SizeOf(Buffer)),
                                    nil, NotUsed, VolFlags, nil, 0) then
               SetString(Result, Buffer, StrLen(Buffer))
          else Result := '';
          if   DriveChar < 'a' then
               Result := AnsiUpperCaseFileName(Result)
          else Result := AnsiLowerCaseFileName(Result);
          Result := StrWithBrackets(Result);
  finally SetErrorMode(OldErrorMode);
  end;
end;

procedure Z;
begin
end;

end.

