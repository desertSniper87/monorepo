unit Main_;

// IMPORTANT:
// Delphi 6 handles images differently than Delphi 4
// and unfortunately, it causes flicker.
// define 'DELPHI6' in order to avoid the problem.

{$DEFINE DELPHI6}

{///$DEFINE TEST}

interface

uses
  Windows, ShellApi,Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ComCtrls, Buttons, ExtCtrls,MPlayer,MMSystem,Menus,
  {$WARNINGS OFF}
    FileCtrl, // Warning: Unit 'FileCtrl' is specific to a platform
  {$WARNINGS ON}
  jpeg,
  Capture_,Misc_,Popup_,SokUtil_,SokFile_,SokGame_,Game_,GView_,
  Sound_,Music_,Pack_,Menu_,Status_,MPlayer1_,Skins_,Dead_,{FileAssoc_,}
  Plugin_,Generator_,Log_,MView_,PNG_;

{///$DEFINE MUSIC_PLAYER}

const
  DEFAULT_FORM_HEIGHT                   = 525;
  DEFAULT_FORM_WIDTH                    = 700;
  MAINFORM_TIMER_INTERVAL_MILLI_SECONDS = 1000;

type

  TCtrlEnterKeySet                      = set of (ceCtrl,ceEnter,ceCtrlUpWhileEnterDown);
  TLogFileTask                          = (lftClear,lftImport,lftIsEmpty);
  TTrackState                           = (tsSit,tsWait,tsTrack);

  TMainForm = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    DrawGrid1: TDrawGrid;
    StatusBar1: TStatusBar;
    Bevel1: TBevel;
    Image1: TImage;
    PanelMenu1: TPanel;
    BtnOpenPrior: TSpeedButton;
    BtnReset: TSpeedButton;
    BtnHelp: TSpeedButton;
    btnUndo: TSpeedButton;
    BtnOpen: TSpeedButton;
    BtnRedo: TSpeedButton;
    BtnExit: TSpeedButton;
    BtnSolution: TSpeedButton;
    BtnTools: TSpeedButton;
    BtnOptions: TSpeedButton;
    BtnRedoAll: TSpeedButton;
    BtnOpenNext: TSpeedButton;
    BtnOpenClipBoard: TSpeedButton;
    DefaultTileSetImage: TImage;
    BtnSave: TSpeedButton;
    BtnSaveAs: TSpeedButton;
    OpenClipboardImage: TImage;
    OpenPriorImage: TImage;
    OpenNextImage: TImage;
    ArrowRightImage: TImage;
    ArrowLeftImage: TImage;
    ArrowStartImage: TImage;
    ArrowEndImage: TImage;
    DefaultButtonSetImage: TImage;
    StatusPanel: TPanel;
    MenuPanel: TPanel;
    OpenMPlayerImage: TImage;
    MPlayerImage: TImage;
    MPlayerDisplayPanel: TPanel;
    SnapshotImage: TImage;
    BtnBookmarks: TSpeedButton;
    Viewer1Panel: TPanel;
    BtnViewer1Open: TSpeedButton;
    BtnViewer1Prior: TSpeedButton;
    BtnViewer1Next: TSpeedButton;
    BtnViewer1Size: TSpeedButton;
    BtnViewer1Settings: TSpeedButton;
    BtnViewer1SlideShow: TSpeedButton;
    FworksPanel: TPanel;
    BtnFworksSettings: TSpeedButton;
    ActivitiesPanel: TPanel;
    BtnActivitiesSettings: TSpeedButton;
    FractalsPanel: TPanel;
    BtnFractalsSettings: TSpeedButton;
    BtnFractalsColors: TSpeedButton;
    BtnFractalsSaveAs: TSpeedButton;
    BtnFractalsPrior: TSpeedButton;
    BtnFractalsNext: TSpeedButton;
    BtnFractalsReset: TSpeedButton;
    MandelbrotImage: TImage;
    FractalsSaveDialog: TSaveDialog;
    MandalaPanel: TPanel;
    BtnMandalaSettings: TSpeedButton;
    BtnMandalaStartStop: TSpeedButton;
    BtnMandalaSaveAs: TSpeedButton;
    MandalaSaveDialog: TSaveDialog;
    BtnActivitiesMandala: TSpeedButton;
    BtnActivitiesImages: TSpeedButton;
    BtnActivitiesFireworks: TSpeedButton;
    BtnActivitiesFractals: TSpeedButton;
    Memo1: TMemo;
    BtnSnapshots: TSpeedButton;
    PaintBox1: TPaintBox;
    BtnReverseMode: TSpeedButton;
    ReverseModeImage: TImage;
    NormalModeImage: TImage;
    ReverseModeBackgroundImage: TImage;
    StatusMenuPanel: TPanel;
    BtnStatusMenuReplay: TSpeedButton;
    BtnStatusMenuStop: TSpeedButton;
    PauseImage: TImage;
    BtnStatusMenuBrowse: TSpeedButton;
    TimerImage: TImage;
    BtnStatusMenuTimer: TSpeedButton;
    PopupMenu: TPopupMenu;
    MenuItemSave: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemSaveSeparator: TMenuItem;
    MenuItemSolution: TMenuItem;
    MenuItemSettingsSeparator: TMenuItem;
    MenuItemSettings: TMenuItem;
    MenuItemMusicplayer: TMenuItem;
    MenuItemHelp: TMenuItem;
    BtnMPlayer: TSpeedButton;
    MenuItemHelpSeparator: TMenuItem;
    MenuItemClose: TMenuItem;
    BtnMenu: TSpeedButton;
    MenuItemSolutionSeparator2: TMenuItem;
    MenuItemOpenLevelSeparator: TMenuItem;
    MenuItemOpenPreviousLevel: TMenuItem;
    MenuItemOpenNextLevel: TMenuItem;
    MenuItemOpenClipboard: TMenuItem;
    MenuItemToolsSeparator: TMenuItem;
    MenuItemTools: TMenuItem;
    BtnStatusMenuDeadlocks: TSpeedButton;
    RotateImage: TImage;
    BtnRotate: TSpeedButton;
    MenuItemRotateAndMirror: TMenuItem;
    PanelRotateAndFlip: TPanel;
    PanelRotateCounterclockwise: TPanel;
    PanelRotateClockwise: TPanel;
    PanelFlipVertically: TPanel;
    PanelFlipHorizontally: TPanel;
    PanelResetTransformations: TPanel;
    MenuItemSolutionMoves: TMenuItem;
    MenuItemSolutionPushes: TMenuItem;
    MenuItemSolutionSeparator1: TMenuItem;
    MenuItemClipboardSeparator: TMenuItem;
    MenuItemCopyToClipboard: TMenuItem;
    MenuItemWindowSize: TMenuItem;
    MenuItemWindowSizeDefault: TMenuItem;
    MenuItemWindowSizeDefaultCentered: TMenuItem;
    MenuItemWindowSizeMaximized: TMenuItem;
    BtnAdHoc: TButton;
    MenuItemNormalMode: TMenuItem;
    MenuItemReverseMode: TMenuItem;
    BtnStatusMenuSolver: TSpeedButton;
    BtnStatusMenuOptimizer: TSpeedButton;
    BtnStatusMenuGenerator: TSpeedButton;
    ApplicationTimer: TTimer;
    SplitViewImage: TImage;
    BtnSplitView: TSpeedButton;
    PanelMultiViewMenu: TPanel;
    PanelMultiViewCopyMovesToClipboard: TPanel;
    PanelMultiViewCopyContinuationMovesToClipboard: TPanel;
    PanelMultiViewPasteMovesFromClipboard: TPanel;
    PanelMultiViewMoveToSnapshots: TPanel;
    PanelMultiViewCloseView: TPanel;
    PanelMultiViewCombineSnapshotsToFormSolutions: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure BtnResetClick(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnSaveAsClick(Sender: TObject);
    procedure BtnUndoClick(Sender: TObject);
    procedure BtnRedoClick(Sender: TObject);
    procedure BtnRedoAllClick(Sender: TObject);
    procedure BtnSolutionClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormResize(Sender: TObject);
    procedure DrawGrid1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure DrawGrid1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BtnToolsClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure DrawGrid1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DrawGrid1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BtnOpenPriorOrNextClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure PaintBox1DblClick(Sender: TObject);
    procedure BtnOptionsClick(Sender: TObject);
    procedure BtnOpenClipBoardClick(Sender: TObject);
    procedure BtnMPlayerClick(Sender: TObject);
    procedure BtnBookmarksClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnViewer1OpenClick(Sender: TObject);
    procedure BtnViewer1PriorClick(Sender: TObject);
    procedure BtnViewer1NextClick(Sender: TObject);
    procedure BtnViewer1SizeClick(Sender: TObject);
    procedure BtnViewer1SettingsClick(Sender: TObject);
    procedure BtnViewer1SlideShowClick(Sender: TObject);
    procedure BtnFworksSettingsClick(Sender: TObject);
    procedure BtnActivitiesSettingsClick(Sender: TObject);
    procedure BtnFractalsSettingsClick(Sender: TObject);
    procedure BtnFractalsColorsClick(Sender: TObject);
    procedure BtnFractalsSaveAsClick(Sender: TObject);
    procedure BtnFractalsPriorClick(Sender: TObject);
    procedure BtnFractalsResetClick(Sender: TObject);
    procedure BtnFractalsNextClick(Sender: TObject);
    procedure BtnMandalaSettingsClick(Sender: TObject);
    procedure BtnMandalaStartStopClick(Sender: TObject);
    procedure BtnMandalaSaveAsClick(Sender: TObject);
    procedure BtnActivitiesClick(Sender: TObject);
    procedure BtnSnapshotsClick(Sender: TObject);
    procedure BtnReverseModeClick(Sender: TObject);
    procedure BtnStatusMenuStopClick(Sender: TObject);
    procedure BtnStatusMenuTimerClick(Sender: TObject);
    procedure BtnStatusMenuDeadlocksClick(Sender: TObject);
    procedure BtnStatusMenuPluginClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure BtnMenuClick(Sender: TObject);
    procedure BtnRotateClick(Sender: TObject);
    procedure PanelRotateAndFlipClick(Sender: TObject);
    procedure MenuItemCopyToClipboardClick(Sender: TObject);
    procedure MenuItemWindowSizeClick(Sender: TObject);
    procedure ApplicationTimerTimer(Sender: TObject);
    procedure BtnSplitViewClick(Sender: TObject);
    procedure PanelMultiViewMenuClick(Sender: TObject);

  protected
    procedure ApplicationOnActivate(Sender: TObject);
    procedure ApplicationOnDeactivate(Sender: TObject);
    procedure ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
    procedure OnCopyData(var Msg: TWMCopyData); Message WM_COPYDATA;
    procedure OnDisplayChange(var Msg: TMessage); Message WM_DISPLAYCHANGE;
    procedure OnMPlayerOnIdle(var Msg: TMessage); Message Misc_.MSG_MPLAYER_ON_IDLE;
    procedure OnPlugin(var Msg:TMessage); Message MSG_PLUGIN;
    procedure OnRefresh(var Msg: TMessage); Message Misc_.MSG_REFRESH;
    procedure OnReplay(var Msg:TMessage); Message MSG_REPLAY;
    procedure OnShowDeadlockedBoxes(var Msg:TMessage); Message MSG_SHOW_DEADLOCKED_BOXES;
    procedure OnShutdown(var Msg:TMessage); Message MSG_SHUTDOWN;
    procedure OnTest(var Msg:TMessage); Message MSG_TEST;
    procedure OnUpdateDeadSquares(var Msg: TWMCopyData); Message Misc_.MSG_UPDATE_DEAD_SQUARES;
    procedure SetDragCursorType(DragCursorType__:TCursorType);
    procedure SetSelectionCursorType(SelectionCursorType__:TCursorType);
    procedure SetTrackingCursorType(TrackingCursorType__:TCursorType);

//  procedure StopFlicker(var Msg: TWMEraseBkgnd); Message WM_ERASEBKGND; // not necessary when the program is compiled with Delphi 4, but some versions (like Delphi 6) needs it
  private
    { Private declarations }
    fApplicationDataPath:String; // includes trailing path delimiter
    fBevelVisible:Boolean;
    fDragCursorType:TCursorType;
    fHelpFileName:String;
    fSelectionCursorType:TCursorType;
    fTrackingCursorType:TCursorType;
    fTrackState:TTrackState;
    FirstTime:Boolean;
    fMakeScreenshots:Boolean;
    fMyDocumentsFolder:String;
    fOldApplicationDataPath:String; // includes trailing path delimiter
    fOldPluginsPath:String; // includes trailing path delimiter
    fPluginsPath:String; // includes trailing path delimiter
    FormResizeLevel:Integer;
    fScreenSaverEnabled:Boolean;
    IdleStartTime:Cardinal;
    InstallationFileAge:Integer;
    IsANewLevel:Boolean;
    LastGameAction:TGameAction;
    LastKey:Integer;
    LogFileLineCount:Integer;
    MouseOverPlayerStartTime:Integer;
    QuitWithoutSaving:Boolean;
    SelectionCursor:TCursor;
    ShowMenuCount:Cardinal;
    ShowStatusCount:Cardinal;
    SolutionLogFiles:SokUtil_.TList;
    StartHintCount:Integer;
    SwMaximize:Boolean;
    TrackingCursor:TCursor;
    WaitForKeyUpEvent:Boolean;
//  procedure DebugStatistics;
//  procedure DrawBevelRect(const R:TRect);
//  procedure ClearTrackBox(True);
    procedure AdHocTask;
    procedure BtnSkinClick(Sender: TObject);
    function  CopyLevelToClipboard(OppositeFillFloorsSetting__,RunLengthEncoding__,BoardOnly__,CurrentBoardOnly__,NormalizedLevel__:Boolean):Boolean;
    procedure CreateScreenshots;
    procedure ImportHuffmanBase64EncodedBoardFromClipboard;
    procedure InternetLevelLookup(NormalizeLevel__:Boolean);
    procedure CopyHuffmanBase64EncodedBoardToClipboard(NormalizeLevel__,Prompt__:Boolean);
    {$IFNDEF TEST}
      procedure MakeFilesFromResources(OldMajorVersionNumber,OldMinorVersionNumber:Integer);
    {$ENDIF}
    function  MakeNewLogFileName:String;
    function  MakeNormalizedLevel(Game__:TGame):Boolean;
    procedure MakeUndoButtonHint;
    function  ProcessLogFiles(LogFileTask__:TLogFileTask):Boolean;
    procedure SelectOptions(Sender: TObject);
    procedure SetBevelVisible(BevelVisible__:Boolean);
    procedure SetBoardTransformation2D(BoardTransformation2D__:TBoardTransformation2D);
    procedure SetDefaultFormSize;
    procedure SetScreenSaverEnabled(Enabled__:Boolean);
    procedure SetTrackState(TrackState__:TTrackState);
    procedure ShowHint(Sender: TObject);
    procedure GetDevInfos;
    procedure OnIdleHandler(Sender: TObject; var Done: Boolean);
  protected
    function  GetDefaultLevelFolder:String;
    procedure SetBusyMode(IsBusy__:Boolean);
  public
    { Public declarations }
    AddFileFormatDescriptionToFiles:Boolean;
    AutoAdvanceWhenSolved:Boolean;
    BoardDimensionsAsText:TBoardDimensionsAsText;
    BoardDimensionsInTitleLine:Boolean;
    BoardDimensionsWithFloorCount:Boolean;
    BevelColor:TColor;
    CalculateMinimumLowerBoundTimeLimitMS:TTimeMS;
    ConfirmRunningMultipleInstances:Boolean;
    OptimizeMovesBetweenPushes:Boolean;
    CollectionNameInTitleLine:Boolean;
    CopyLevelCollectionToClipboardIncludeLevelComments:Boolean;
    CopyLevelToClipboardFillFloors:Boolean;
    CopyLevelToClipboardFloorFillCharacter:Char;
    CopyLevelToClipboardPreserveCombinedMoves:Boolean;
    CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles:Boolean;
    CtrlEnterKeySet:TCtrlEnterKeyset;
    CurrentImageClientHeight:Integer;
    CurrentImageClientWidth:Integer;
    Deadlocks:TDeadlocks;
    DebugCount:array[0..9] of Integer;
    DisplayDriverChangedAction:TDisplayDriverChangedAction;
    ErrorRecoveryEnabled:Boolean;
//  FileAssoc:TFileAssoc;
    FormResizeCount:Integer;
    Game:TGame;
    GamePictures:TGamePictures;
    GameViewer:TGameViewer;
    Generator:TGenerator;
    GraphicMenuWidth:Integer;
    InternetLevelLookupFormatString:String;
    IgnoreKeyUp:Boolean;
    IgnoreMouseUp:Boolean;
    IgnoreShiftKey:Boolean;
    Initialized:Boolean;
    IsResizing:Boolean;
    LastGameReverseMode:Boolean;
    LastReplayAction:TGameAction;
//  Log:TLogFile;
    Menu:Menu_.TGameMenu;
    Modified:Boolean;
    MouseTrackingModeEnabled:Boolean;
    MPlayer:TMPlayer;
    MultiView:TMultiView;
    MultiViewPopupMenu:TColorPopupMenu;
    Music:TMusicManager;
    OnEscapeKeyAction:TOnEscapeKeyAction;
    OnLeftClickEmptySquareAction:TOnLeftClickEmptySquareAction;
    OnRightClickAction:TOnRightClickAction;
    OnShiftKeyAction:TOnShiftKeyAction;
    Optimizer:TOptimizerPlugin;
    PauseBeforeReplayMS:Integer;
    PNGImageLoader:TPNGImageLoader;
    ReplayStarterButton:TObject;
    RotateAndFlipPopupMenu:TColorPopupMenu;
    RunLengthEncodingFloor:Char;
//  ScreenSaverAndMonitorPowerOffEnabled:Boolean;
    SecondaryMetricsInSolutionButtonHint:Boolean;
    ShowBoardCoordinates:Boolean;
    ShowSimpleDeadSquaresColor:TColor;
    ShowSimpleDeadSquaresEnabled:Boolean;
    ShowSimpleDeadSquaresTransparencyPct:Integer;
    ShowSolutionMoves:Boolean;
    ShutDownApplication:Boolean;
    Skins:TSkins;
    SokoFile:TSokoFile;
    Solver:TSolverPlugin;
    Sound:TSoundManager;
    Status:TStatus;
    SwDebug:Boolean;
//  TestLogFile:TLogFile;
    TrackBox:TColRow;
    UndoRedoCombinedMoves:Boolean;
    UseOnlyOneProcessor:Boolean;
    function  CalculatePushesLowerBound( Sender : TObject ) : Integer;
    function  CheckAutoAdvanceWhenSolved:Boolean;
    procedure ClearTrackBox(HidePopupMenu:Boolean);
    function  CloseLevel(Sender: TObject):Boolean;
    procedure CloseForm;
    procedure DrawBoardBevel;
    function  InitGame(ResetLevel,LoadSnapshots,InvalidateGameViewer,PlayResetSound,CalculateDeadlocks,HidePopUpMenu,ShowGame:Boolean; SnapshotItemIndex:Integer):Boolean;
    function  FormRectToScreenRect(const Rect:TRect):TRect;
    function  LoadGame(const FileName:String; Verbose__:Boolean):Boolean;
    function  LoadSolutionsAndSnapshotsForCurrentGame(const FileName:String; Verbose__:Boolean; MultiViewItem__:TMultiViewItem; var GameReady:Boolean):Boolean;
    procedure MakeDynamicHints(Snapshot__:TSnapshot);
    procedure MakeBookmarksHint;
    procedure MakeSolutionButtonHint;
    function  CreateScreenshot:String;
    function  Msg(const Text__,Caption__:String; Flags__:Integer):Integer;
    function  Open(Sender: TObject; OpenSubTask:TOpenSubTask):Boolean;
    function  OpenPriorOrNext(Prior,Unsolved,Solved:Boolean; VirtualKeyCode:Integer):Boolean;
    function  Save(Sender: TObject):Boolean;
    function  SaveSnapshotToLogFile(const LevelName1__,LevelName2__,MovesAsText__:String):Boolean;
    procedure SetCursor(X,Y:Integer);
    procedure SetMessageHandlers;
    procedure ShowBoard;
    procedure ShowDeadlockedBoxes;
    procedure ShowGame;
    procedure ShowMenu;
    procedure ShowMove(const FromPos,ToPos:TPoint; BoxNo,TimeMS,MoveCount,PushCount:Integer; Undo,Jump,LastMove:Boolean);
    procedure ShowMoveInstantly(BoxNo:Integer; Undo,Jump,LastMove:Boolean);
    procedure ShowSnapshots(HidePopUpMenu,KeepSnapshotFormFocus:Boolean; SnapshotItemIndex:Integer);
    procedure ShowStatus;
    procedure ShowTitle(const FileName:String);
    procedure Timer1Timer(Sender: TObject);
    procedure UpdateCursor;

    property  ApplicationDataPath      :String          read  fApplicationDataPath; // includes trailing path delimiter
    property  BevelVisible             :Boolean         read  fBevelVisible        write SetBevelVisible;
    property  BusyMode                 :Boolean         write SetBusyMode;
    property  DefaultLevelFolder       :String          read  GetDefaultLevelFolder;
    property  DragCursorType           :TCursorType     read  fDragCursorType      write SetDragCursorType;
    property  HelpFileName             :String          read  fHelpFileName;
    property  MakeScreenshots          :Boolean         read  fMakeScreenshots;
    property  MyDocumentsFolder        :String          read  fMyDocumentsFolder;   // excludes trailing path delimiter
    property  OldApplicationDataPath   :String          read  fOldApplicationDataPath; // includes trailing path delimiter
    property  OldPluginsPath           :String          read  fOldPluginsPath; // includes trailing path delimiter
    property  PluginsPath              :String          read  fPluginsPath; // includes trailing path delimiter
    property  ScreenSaverEnabled       :Boolean         read  fScreenSaverEnabled  write SetScreenSaverEnabled;
    property  SelectionCursorType      :TCursorType     read  fSelectionCursorType write SetSelectionCursorType;
    property  TrackingCursorType       :TCursorType     read  fTrackingCursorType  write SetTrackingCursorType;
    property  TrackState               :TTrackState     read  fTrackState          write SetTrackState;
  end;

var
  IsFirstInstance                      :Boolean         = False;
  MainForm                             :TMainForm       = nil;

implementation

uses
  Clipbrd,//FileCtrl,
  Mandal2_,
  Text_,IniFile_,Help_,
  BitMap_,BitMap2_,Pict_,Open1_,Options_,
  MPlayer2_,Res_,Mandal1_,IView1_,Fractal_,Fworks_,Duplicates_,Snapshots_,Tools_,
  LSView_;

{$R *.DFM}
{$R Manifest.RES}

// note that 'AllowSetForegroundWindow' isn't implemented in Windows95/98/ME
// function AllowSetForegroundWindow(ProcessId__:DWORD): BOOL; stdcall; external user32 name 'AllowSetForegroundWindow';

const
  APPLICATION_TIMER_INTERVAL_MILLI_SECONDS  = 50 {seconds} * ONE_THOUSAND {milliseconds}; // if the screen saver is disabled, then the application timer interval must be < 1 minute, so the timer event can fake keyboard activity to prevent the screen saver from being activated
  DEFAULT_BOARD_DIMENSIONS_AS_TEXT          = bdColRow;
  DEFAULT_BOARD_DIMENSIONS_IN_TITLE_LINE    = False;
  DEFAULT_BEVEL_COLOR                       = clLtGray;
  DEFAULT_CONVERT_MOVES_TO_UNDO_MOVES       = True;
  DEFAULT_DISPLAY_DRIVER_CHANGED_ACTION     = ddContinue; //ddShutdown;
  DEFAULT_DRAG_CURSOR_TYPE                  = ctDrag;
  DEFAULT_ON_ESCAPE_KEY_ACTION              = oekaExitApplication;
  DEFAULT_ON_LEFT_CLICK_EMPTY_SQUARE_ACTION = olcesaShowBoxesThatCanGoToSquare;
  DEFAULT_ON_RIGHT_CLICK_ACTION             = orcaLoadSnapshotIfAnyElseRestartGame;
  DEFAULT_PAUSE_BEFORE_REPLAY_MS            = 1000;
  DEFAULT_SELECTION_CURSOR_TYPE             = ctArrow;
  DEFAULT_TRACKING_CURSOR_TYPE              = ctHand;
  DEFAULT_USE_ONLY_ONE_PROCESSOR            = False;
  DELAY_MOUSE_TRACKING_MILLI_SECS           = 1000;
  FORMAT_KEY_AUTHOR                         = '%Author';
  FORMAT_KEY_BOARD                          = '%Board';
  FORMAT_KEY_MD5                            = '%MD5';
  FORMAT_KEY_TITLE                          = '%Title';
  LOG_FILE_KEY_RENAME                       = '=>';
  MAX_LOG_FILE_LINE_COUNT                   = 1000; // avoids that files get too large
  MOUSE_TRACKING_NOT_STARTED                = 0;    // number cannot change
  MOUSE_TRACKING_START_TIMER                = 1;    // number cannot change
  MOUSE_TRACKING_STARTED                    = 2;    // number cannot change

procedure TMainForm.ShowHint(Sender: TObject);
begin
  Status.Hint:=GetLongHint(Application.Hint);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var i,OldMajorVersionNumber,OldMinorVersionNumber:Integer; //TimeMS:TTimeMS;
    PrimaryEditorHistoryFileExists:Boolean;
    s,s1,s2:String; IniFile:TIniFile; CursorHandle:HCursor; //L:SokUtil_.TList;
    p:TPictureType;
    {$IFNDEF TEST} F:TFont; {$ENDIF}

  function AddPluginsFromSokobanPP(const SokobanPPSubDirectory__,SokobanYASCDefaultPluginFileNameStub__:String; ComboBox__:TComboBox):Integer;
  var i:Integer; s,OldCurrentDir,Path:String;
  begin // finds plugins installed in the Sokoban++ plugin-subdirectory folder
    Result:=0; OldCurrentDir:=GetCurrentDir;
    if (RegGetKey(HKEY_CURRENT_USER ,REGINIFILE_UNINSTALL_PATH,'SokobanPP','UninstallString','',s) and (s<>''))
       or
       (RegGetKey(HKEY_LOCAL_MACHINE,REGINIFILE_UNINSTALL_PATH,'SokobanPP','UninstallString','',s) and (s<>'')) then
       try     Path:=StrWithTrailingPathDelimiter(ExtractFilePath(StrWithoutDoubleQuotes(s)))+SokobanPPSubDirectory__;
               if DirectoryExists(Path) then with OpenForm.FileListBox1 do begin
                  Mask:=PLUGIN_FILES_FILTER;
                  Directory:=Path; s:=SokobanYASCDefaultPluginFileNameStub__+DLL_FILE_EXT;
                  for i:=0 to Pred(Items.Count) do
                      if not StrEqual(Items[i],s) then begin
                         FileName:=StrWithTrailingPathDelimiter(Directory)+Items[i];
                         if (ComboBox__.Items.IndexOf(FileName)<0) and
                            FileExists(FileName) then
                            ComboBox__.Items.Add(FileName);
                         end;
                  end;
       finally SetCurrentDir(OldCurrentDir);
       end;
  end;

  function LookForExternalHelpFile:Boolean;
  var i:Integer;

    function IsANewFile(const FileName__:String):Boolean;
    var FileAge:Integer;
    begin // returns 'True' if the file isn't older than the application exe-file
      Result:=False;
      if (FileName__<>'') and FileExists(FileName__) then begin
         FileAge:=Misc_.FileAge(FileName__);
         Result:= (FileAge<>-1) and (InstallationFileAge<>-1) and
                  (FileDateToDateTime(FileAge)>=FileDateToDateTime(InstallationFileAge));
         end;
    end;

  begin // LookForExternalHelpFile
    Result:=IsANewFile(HelpFileName); // 'True': the current help file is not older than the installed exe-file
    if not Result then with OpenForm.FileListBox1 do begin
       // look for the first 'Sokoban-*.rtf' file in the application data folder
       Mask:=ExtractFileNameWithoutPathAndExtension(Application.ExeName)+'-*'+RTF_FILE_EXT;
       Directory:=StrWithoutTrailingPathDelimiter(ApplicationDataPath);
       for i:=0 to Pred(Items.Count) do
           if not Result then begin
              FileName:=StrWithTrailingPathDelimiter(Directory)+Items[i];
              if IsANewFile(FileName) then begin
                 fHelpFileName:=OpenForm.FileListBox1.FileName;
                 Result:=True;
                 end;
              end;
       end;
    if not Result then fHelpFileName:='';
  end;

begin // FormCreate
//  TimeMS                 :=GetTickCount;
    IsMultiThread          :=True; // ensure that the Delphi memory allocator is thread safe
    Visible                :=False; // 'False': don't show the form if the program only is installing itself; it would look like flicker because the program terminates itself after the installation
    StatusBar1.Font.Assign(Self.Font);
    Left                   :=Max(0,(Screen.Width -Width ) div 2);
    Top                    :=Max(0,(Screen.Height-Height) div 2);
    Caption                :=Application.Title;
    OpenDialog1.Title      :=Application.Title+SUB_TITLE_SEPARATOR+OpenDialog1.Title;
    SaveDialog1.Title      :=Application.Title+SUB_TITLE_SEPARATOR+SaveDialog1.Title;
    Image1.Align           :=alClient; Image1.Tag:=0; Image1.Cursor:=DEFAULT_CURSOR;
    Color                  :=clNavy;
    Bevel1.Width           :=10; // actually, this hides the bevel
    fBevelVisible          :=False; Game:=nil;

    // the longest text must be in place before 'MultiViewPopupMenu' is created
    // so the maximum panel with is calculated correctly;
    // (actually, this is not fool-proof since text width in theory doesn't need
    // to follow the number of characters, but this will have to do)
    if   Length(CombineWithOppositeDirectionSnapshotsToFormSolutionsText[False]) >=
         Length(CombineWithOppositeDirectionSnapshotsToFormSolutionsText[True ]) then
         PanelMultiViewCombineSnapshotsToFormSolutions.Caption:=CombineWithOppositeDirectionSnapshotsToFormSolutionsText[False]
    else PanelMultiViewCombineSnapshotsToFormSolutions.Caption:=CombineWithOppositeDirectionSnapshotsToFormSolutionsText[True ];

    //fApplicationDataPath:=StrWithoutTrailingPathDelimiter(GetFolderPath(CSIDL_APPDATA));

    s:='';
    if   (ParamCount>=2) and
         StrEqual(ParamStr(Pred(ParamCount)),COMMAND_LINE_PARAMETER_DOCUMENTS_FOLDER) and
         (ParamStr(ParamCount)<>'') then
         fMyDocumentsFolder:=ParamStr(ParamCount)
    else fMyDocumentsFolder:=StrWithoutTrailingPathDelimiter(GetFolderPath(CSIDL_PERSONAL));
    fMyDocumentsFolder     :=ExpandFileName(fMyDocumentsFolder);

    // the application is not geared to handle network files;
    // if the 'documents' folder is on a network, then try to store the
    // application data on the local computer instead;

    if not StrBeginsWithDriveLetter(fMyDocumentsFolder) then begin
       fMyDocumentsFolder:=ExpandFileName(StrWithoutTrailingPathDelimiter(GetFolderPath(CSIDL_LOCAL_APPDATA)));
       if not StrBeginsWithDriveLetter(fMyDocumentsFolder) then begin
          fMyDocumentsFolder:=ExtractFileDrive(Application.ExeName); // at this point 'fMyDocumentsFolder' only contains the drive part of the folder name
          if   (Length(fMyDocumentsFolder)>=2) and (fMyDocumentsFolder[2]=COLON) then begin
               fMyDocumentsFolder:=StrWithTrailingPathDelimiter(fMyDocumentsFolder)+LocalDocumentsFolderText;
               end
          else Halt(1); // panic exit; 'ExtractFileDrive' didn't return a drive letter for the application exe-file (the alternatives are '' or the '\\<servername>\<sharename>' part of an UNC filename)
          //fMyDocumentsFolder:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Application.ExeName)); // fall back to the '<drive>:\Program Files\Program folder' folder and hope that the Windows folder virtualization mechanism makes the program run at least partially correct
          end;
       end;

    fApplicationDataPath:=fMyDocumentsFolder;

{
    // find application data folder name and save its path in 'fApplicationDataPath', including a trailing path delimiter for conveniency
    s:='';
    s:=ExtractFileName(StrWithoutTrailingPathDelimiter(ExtractFilePath(Application.ExeName))); // get application folder name, e.g., 'Sokoban YASC'
    if s='' then s:=TEXT_APPLICATION_TITLE_LONG;
    fApplicationDataPath:='';
}
    if   fApplicationDataPath<>'' then
         fApplicationDataPath:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(fApplicationDataPath)+SokobanText)+TEXT_APPLICATION_TITLE_LONG;
    if   (fApplicationDataPath<>'') and (not DirectoryExists(fApplicationDataPath)) then begin
         ForceDirectories(fApplicationDataPath);
         end;
    if   (fApplicationDataPath<>'') and DirectoryExists(fApplicationDataPath) then
         fApplicationDataPath:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(fApplicationDataPath)+s) // use application data folder
    else fApplicationDataPath:=StrWithTrailingPathDelimiter(ExtractFilePath(Application.ExeName));                // alternatively, try to use the program folder

    s:=ApplicationDataPath+ExtractFileName(ChangeFileExt(Application.ExeName,INI_FILE_EXT));
//    DeleteFile(s); SleepEx(0,False);

    FirstTime:=not FileExists(s);

    if FirstTime then begin
       // no ini-file was found in the application data folder;
       // check if there is one in the program folder or the virtual store;
       s1:=ChangeFileExt(Application.ExeName,INI_FILE_EXT);
       if not CopyAndDeleteFile(s1,s) then begin
          s1:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(GetFolderPath(CSIDL_PROGRAM_FILES))+DEFAULT_APPLICATION_FOLDER_NAME)+ExtractFileName(s1);
          if not CopyAndDeleteFile(s1,s) then begin
             s2:=StrWithTrailingPathDelimiter(GetApplicationVirtualFolder)+ExtractFileName(s);
             if not CopyAndDeleteFile(s2,s) then begin
                end;
             end;
          {
          if FileExists(s) then begin
             if SokFile_.CreateObject(otList,TNode(L)) then
                try     try    if L.LoadFromFile(s) and
                                  (L.StrSubstitute(StrWithoutTrailingPathDelimiter(ExtractFilePath(s1)),StrWithoutTrailingPathDelimiter(ExtractFilePath(s )),'',True)<>0) then begin
                                  L.SaveToFile(s); SleepEx(10,False);
                                  end;
                        except on E:Exception do Error(E.Message,Application.Title);
                        end;
                finally L.Free;
                end;
             end;
          }
          end;

       end;

    if (ApplicationMutex<>0) and
       (WaitForSingleObject(ApplicationMutex,INFINITE)<>WAIT_OBJECT_0) then
       raise Exception.Create(TEXT_PROCESS_SYNCHRONIZATION_ERROR);
    try
      IniFile:=TIniFile.Create(s);
    finally
      if (ApplicationMutex<>0) and
         (not ReleaseMutex(ApplicationMutex)) then
         raise Exception.Create(TEXT_PROCESS_SYNCHRONIZATION_ERROR);
    end;

    try

      // check CPU affinity settings before any of the threads are created
      if IniFile.ReadBool('Application','UseOnlyOneProcessor',DEFAULT_USE_ONLY_ONE_PROCESSOR) then // note that the value is loaded from the inifile again later in this procedure after the default settings have been saved for the 'Options' window
         SetCPUAffinity(True) // only use a single cpu
      else begin // keep the existing cpu affinity settings
         end;

      fOldApplicationDataPath:=''; // '' unless it differs from the application data path for this session
      s:=IniFile.ReadString ('Application','ApplicationDataPath',fApplicationDataPath);
      if s<>'' then begin
         s:=StrWithTrailingPathDelimiter(s);
         if (not StrEqual(s,fApplicationDataPath)) then // 'True': application data path changed since last session
            fOldApplicationDataPath:=s;
         end;

      fPluginsPath:=ExtractFilePath(Application.ExeName)+StrWithTrailingPathDelimiter(DEFAULT_PLUGINS_DIRECTORY);
      fOldPluginsPath:=''; // '' unless it differs from the plugins path for this session
      s:=IniFile.ReadString ('Application','PluginsPath',fPluginsPath);
      if s<>'' then begin
         s:=StrWithTrailingPathDelimiter(s);
         if (not StrEqual(s,fPluginsPath)) then // 'True': plugins path changed since last session
            fOldPluginsPath:=s;
         end;

//    TestLogFile     :=TLogFile        .Create;
      SokoFile        :=TSokoFile       .Create; // must be created before any games are created; they all use 'MainForm.SokoFile'
      Game            :=TGame           .Create;
      GamePictures    :=TGamePictures   .Create;
      GameViewer      :=TGameViewer     .Create;
      OpenForm        :=TOpenForm       .Create(Application);
      Menu            :=TGameMenu       .Create(Image1,MenuPanel,DEFAULT_GAME_MENU_FONT_SIZE[IsAHighResolutionScreen]);
      Status          :=TStatus         .Create(Image1.Canvas,StatusPanel);
      Sound           :=TSoundManager   .Create(Self);
      Music           :=TMusicManager   .Create(Self);
      OptionsForm     :=TOptionsForm    .Create(Application);
      MPlayer         :=TMPlayer        .Create(Image1.Picture.BitMap.Canvas,MPlayerDisplayPanel);
      MPlayerForm     :=TMPlayerForm    .Create(Application);
      Viewer1         :=TViewer1        .Create(Viewer1Panel);
      Fractals        :=TFractals       .Create(FractalsPanel);
      Fireworks       :=TFireworks      .Create(3000,nil,Rect(0,0,0,0),MPLAYER_FRAME_TIME_SLOW_MS,FworksPanel);
      Mandala         :=TMandala        .Create(MandalaPanel);
      RotateAndFlipPopupMenu
                      :=TColorPopupMenu .Create(PanelRotateAndFlip,'RotateAndFlipMenu',PanelRotateAndFlipClick);
      SnapshotsForm   :=TSnapshotsForm  .Create(Application);
//    EditForm        :=TEditForm       .Create(Application);
      ToolsForm       :=TToolsForm      .Create(Application);
      LevelSetForm    :=TLevelSetForm   .Create(Application);
      Skins           :=TSkins          .Create;
      Deadlocks       :=TDeadlocks      .Create;
//    FileAssoc       :=TFileAssoc      .Create;
      Solver          :=TSolverPlugin   .Create(SolverText,DEFAULT_SOLVER_FILE_NAME_STUB,OpenForm.BtnSolveLevel,ToolsForm.SolverComboBox);
      Optimizer       :=TOptimizerPlugin.Create(OptimizerText,DEFAULT_OPTIMIZER_FILE_NAME_STUB,OpenForm.BtnOptimizeGames,ToolsForm.OptimizerComboBox);
      Generator       :=TGenerator      .Create(GeneratorText,'',ToolsForm.BtnGenerateLevels,nil); // create the generator before the form
      GeneratorForm   :=TGeneratorForm  .Create(Application);
      DuplicatesForm  :=TDuplicatesForm .Create(Application);
      SolutionLogFiles:=SokUtil_.TList  .Create;
      MultiView       :=TMultiView.Create(Image1);
      MultiViewPopupMenu
                      :=TColorPopupMenu .Create(PanelMultiViewMenu,'MultiViewMenu',PanelMultiViewMenuClick);
      PNGImageLoader  :=TPNGImageLoader .Create;
      CaptureForm     :=TCaptureForm    .Create(Application);
      HelpForm        :=THelpForm       .Create(Application);
//    Log             :=TLogFile        .Create;

      if (Length(ApplicationDataPath)<=1) or
//       (TestLogFile=nil) or
         (SokoFile=nil) or
         (Game=nil) or
         (GamePictures=nil) or (not GamePictures.Initialized) or
         (GameViewer  =nil) or
         (Sound=nil) or (Music=nil) or
         (OpenForm=nil) or (OpenForm.Game=nil) or
         (Menu=nil) or
         (Status=nil) or (Status.Menu=nil) or
         (OptionsForm=nil) or
         (MPlayer=nil) or (MPlayer.Display=nil) or
         (MPlayerForm=nil) or
         (Viewer1=nil) or (IView1_.Data=nil) or
         (Viewer1.Menu=nil) or (Viewer1.PictThread=nil) or
         (Fractals=nil) or (Fractals.Menu=nil) or
         (Fireworks=nil) or (Fireworks.Menu=nil) or
         (Mandala=nil) or (Mandala.Menu=nil) or (Mandala.Engine=nil) or
         (RotateAndFlipPopupMenu=nil) or
         (SnapshotsForm=nil) or
//       (EditForm=nil) or
         (ToolsForm=nil) or (ToolsForm.Game=nil) or (ToolsForm.GameViewer.BackgroundPict=nil) or (ToolsForm.GameViewer.SkinPict=nil) or (ToolsForm.GameViewer.CursorPict=nil) or (ToolsForm.SolveLevelsStringGrid.FixedRows=0) or (ToolsForm.OptimizeSolutionsStringGrid.FixedRows=0) or
         (ToolsForm.SolverTaskQueue=nil) or (ToolsForm.OptimizerTaskQueue=nil) or (ToolsForm.GeneratorTaskQueue=nil) or
         (LevelSetForm=nil) or (LevelSetForm.GameViewer.BackgroundPict=nil) or (LevelSetForm.GameViewer.SkinPict=nil) or
         (Skins=nil) or (Skins.ScriptList=nil) or (not Skins.Initialize) or
         (Deadlocks=nil) or (Deadlocks.Thread=nil) or
//       (FileAssoc=nil) or
         (Solver=nil) or    // the solver-thread is created on demand, hence, it's not checked here
         (Optimizer=nil) or // the optimizer-thread is created on demand, hence, it's not checked here
         (Generator=nil) or // the generator-thread is created on demand, hence, it's not checked here
         (GeneratorForm=nil) or
         (DuplicatesForm=nil) or
         (SolutionLogFiles=nil) or
         (MultiView=nil) or
         (MultiView.Font=nil) or (MultiView.Items=nil) or
         (MultiViewPopupMenu=nil)
         or
         (PNGImageLoader=nil)
         or
         (CaptureForm=nil)
         or
         (HelpForm=nil)
//       or
//       (Log=nil)
//       or
//       (not Log.New(ApplicationDataPath+'Log.txt'))
         then
         Halt(1); // panic exit: creating the application objects failed

    {$IFDEF DELPHI6}
      Image1.Stretch:=True; // 'True': forces ControlStyle := ControlStyle + [csOpaque]
      SnapshotsForm.ImageBoard.Stretch:=True;
      OpenForm.Image1.Stretch:=True;
      OpenForm.GameBoardImage.Stretch:=True;
      ToolsForm.EditImage1.Stretch:=True;
      ToolsForm.ImageBoard.Stretch:=True;
    {$ENDIF}

//    TestLogFile.New( 'C:\\Temp\\Log.txt' );
      Game.ShowGame:=ShowGame; Game.DeadlockDetection.Deadlocks:=Deadlocks;
      Modified:=False; fTrackState:=tsSit; MouseOverPlayerStartTime:=MOUSE_TRACKING_NOT_STARTED;
      Initialized:=False; SwMaximize:=False; TrackBox.X:=0; TrackBox.Y:=0;
      IgnoreKeyUp:=False; IgnoreMouseUp:=False; IgnoreShiftKey:=False; QuitWithoutSaving:=False; IsResizing:=False;
      ShowBoardCoordinates:=False; ShowMenuCount:=0; ShowStatusCount:=0; MouseTrackingModeEnabled:=False;
      OnEscapeKeyAction:=DEFAULT_ON_ESCAPE_KEY_ACTION;
      OnLeftClickEmptySquareAction:=DEFAULT_ON_LEFT_CLICK_EMPTY_SQUARE_ACTION;
      if   Mouse.WheelPresent then
           OnRightClickAction:=DEFAULT_ON_RIGHT_CLICK_ACTION
      else OnRightClickAction:=orcaUndoMove;
      OnShiftKeyAction:=oskaOpenSnapshotsAndSolutionsWindow;
      BevelColor:=DEFAULT_BEVEL_COLOR;
      PauseBeforeReplayMS:=DEFAULT_PAUSE_BEFORE_REPLAY_MS;
      ShowSolutionMoves:=True; UndoRedoCombinedMoves:=True;
      CtrlEnterKeySet:=[]; LastKey:=0;
      WaitForKeyUpEvent:=False;
      BoardDimensionsAsText:=DEFAULT_BOARD_DIMENSIONS_AS_TEXT;
      BoardDimensionsInTitleLine:=DEFAULT_BOARD_DIMENSIONS_IN_TITLE_LINE;
      BoardDimensionsWithFloorCount:=False;
      CollectionNameInTitleLine:=False;
      SecondaryMetricsInSolutionButtonHint:=False;
      ErrorRecoveryEnabled:=True; LogFileLineCount:=MAX_LOG_FILE_LINE_COUNT;
      ConfirmRunningMultipleInstances:=True;
      DragCursorType:=DEFAULT_DRAG_CURSOR_TYPE;
      SelectionCursorType:=DEFAULT_SELECTION_CURSOR_TYPE;
      TrackingCursorType:=DEFAULT_TRACKING_CURSOR_TYPE;
      OptimizeMovesBetweenPushes:=DEFAULT_CONVERT_MOVES_TO_UNDO_MOVES;
      AddFileFormatDescriptionToFiles:=DEFAULT_ADD_FILE_FORMAT_DESCRIPTION_TO_FILES;
      AutoAdvanceWhenSolved:=False;
      DisplayDriverChangedAction:=DEFAULT_DISPLAY_DRIVER_CHANGED_ACTION;
      ScreenSaverEnabled:=DEFAULT_SCREEN_SAVER_ENABLED;
      UseOnlyOneProcessor:=DEFAULT_USE_ONLY_ONE_PROCESSOR;
      CopyLevelCollectionToClipboardIncludeLevelComments:=False;
      CopyLevelToClipboardFillFloors:=False;
      CopyLevelToClipboardFloorFillCharacter:=HYPHEN; //UNDERSCORE;
      CopyLevelToClipboardPreserveCombinedMoves:=False;
      CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles:=True;
      RunLengthEncodingFloor:=HYPHEN;
      CurrentImageClientHeight:=0; CurrentImageClientWidth:=0;
      FillChar(DebugCount,SizeOf(DebugCount),0);
      FormResizeCount:=0; FormResizeLevel:=0; StartHintCount:=1;
      ShutDownApplication:=False;
      ShowSimpleDeadSquaresColor:=clBlack; ShowSimpleDeadSquaresEnabled:=False; ShowSimpleDeadSquaresTransparencyPct:=80;
      InstallationFileAge:=FileAge(Application.ExeName);
      InternetLevelLookupFormatString:='';
      fMakeScreenshots:=False;
      ApplicationTimer.Interval:=APPLICATION_TIMER_INTERVAL_MILLI_SECONDS;
      CalculateMinimumLowerBoundTimeLimitMS:=High(CalculateMinimumLowerBoundTimeLimitMS);

      OptionsForm.LoadData; // all settings have default values at this point
      OptionsForm.SaveAsDefaultValues;
      if FirstTime then
         OptionsForm.MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified := DefaultSkinTitleText + SUB_TITLE_Separator + StrRemoveChar( OptionsForm.MenuItemDefaultSkinRedBoxes.Caption, AMPERSAND );

      // the default value for player animations and box animations is 'disabled',
      // but the default skin has both types of animations;
      GameViewer.LegalMovesInfo.PlayerAnimationEnabled:=True;
      GameViewer.LegalMovesInfo.BoxAnimationEnabled:=True;
      GameViewer.SolutionsInfo.PlayerAnimationEnabled:=True;
      GameViewer.SolutionsInfo.BoxAnimationEnabled:=True;

      // the default bitmap mask percentage for the default skin deviates from
      // the default value;
      for p:=ptPlayer to ptBoxOnGoalAnimation do
          if not (p in [ptGoal,ptWall,ptFloor]) then
             GamePictures.Pictures[p].MaskBitMapPct:=DEFAULT_SKIN_MASK_BITMAP_PCT;

      OpenForm.SetPluginButtonState(OpenForm.BtnSolveLevel,pbsRun);
      OpenForm.SetPluginButtonState(OpenForm.BtnOptimizeGames,pbsRun);

      ToolsForm.OptimizerTaskQueue.Plugin:=Optimizer;
      ToolsForm.SolverTaskQueue.Plugin:=Solver;
      ToolsForm.GeneratorTaskQueue.Plugin:=Generator;

//    custom-made cursors
//    The program uses custom-made mouse-cursors for "arrow", "drag" and "hand"
//    instead of the mouse-cursors supplied by the Delphi programming
//    tool. Originally, this was a work-around to avoid a subtle problem with these
//    built-in mouse-cursors under Windows XP. They had different hotspot-points
//    than the default-cursor and when the cursor changed, this caused an
//    unpleasant jolt, as if either Windows or the Delphi programming tool had a
//    logical flaw. The new cursor appeared on the screen using the old cursor's
//    hotspot-point before it moved to the correct position, taking the new
//    hotspot-point into account.
//    This behavior was noticable and looked rather disturbing. As a
//    work-around, the program introduced custom-made mouse-cursors with the same
//    hotspot-point as the default mouse-cursor.
//
//    The successor of Windows XP, Windows Vista, went back to default cursors
//    with hot-spot [0,0], which happened to match the built-in Delphi
//    drag mouse-cursor, but even so, the jolt was this there;
//    in case anybody has an explanation or a work-around, this information will
//    be highly appreciated;

      CursorHandle:=LoadCursor(hInstance,ARROW_CURSOR_RES_NAME);
      if CursorHandle<>0 then Screen.Cursors[crArrow]:=CursorHandle;
      CursorHandle:=LoadCursor(hInstance,DRAG_CURSOR_RES_NAME);
      if CursorHandle<>0 then Screen.Cursors[crDrag]:=CursorHandle;
      CursorHandle:=LoadCursor(hInstance,HAND_CURSOR_RES_NAME);
      if CursorHandle<>0 then Screen.Cursors[crHandPoint]:=CursorHandle;

      if IniFile.ReadInteger('Installation','FileAge',InstallationFileAge)=-1 then
         // hack: while testing, edit 'Sokoban.ini' and set 'FileAge' = -1.
         // This way, recompiling the program does not trigger first-time procedures,
         // and you avoid the introduction screen after each compilation.
         InstallationFileAge:=-1;
      fHelpFileName:=IniFile.ReadString('Installation','HelpFileName','');

      s:=IniFile.ReadString('Sokoban','Version','');
      OldMajorVersionNumber:=1; OldMinorVersionNumber:=0; i:=0;
      ReadUnsignedInteger(s,i,OldMajorVersionNumber);
      ReadUnsignedInteger(s,i,OldMinorVersionNumber);

      FirstTime:=FirstTime or
                 (InstallationFileAge<>IniFile.ReadInteger('Installation','FileAge',-1));

      Game.FileName:=KeepDataPathUptoDate(Trim(IniFile.ReadString ('Game','FileName',Game.FileName)));
      if (Game.FileName<>'') and (ExtractFilePath(Game.FileName)='') then
         Game.FileName:=ApplicationDataPath+Game.FileName;

    {$IFNDEF TEST}

      Game.MoveAnimationEnabled:=IniFile.ReadBool('Game','MoveAnimationEnabled',Game.MoveAnimationEnabled);
      Game.SmoothMoveAnimationEnabled:=IniFile.ReadBool('Game','SmoothMoveAnimationEnabled',Game.SmoothMoveAnimationEnabled);
      Game.SessionSmoothMoveAnimationEnabled:=Game.SmoothMoveAnimationEnabled;
//    Game.SmoothMoveAnimationThresholdEnabled:=IniFile.ReadBool('Game','SmoothMoveAnimationThresholdEnabled',Game.SmoothMoveAnimationThresholdEnabled);
//    i:=IniFile.ReadInteger('Game','SmoothMoveAnimationThresholdMaxPixelsPerMove',Game.SmoothMoveAnimationThresholdMaxPixelsPerMove);
//    if (i>=1) and (i<=MAX_SMOOTH_ANIMATION_THRESHOLD_MAX_PIXELS_PER_MOVE) then
//       Game.SmoothMoveAnimationThresholdMaxPixelsPerMove:=i;
      Game.PlayerDirectionAnimationEnabled:=IniFile.ReadBool('Game','PlayerDirectionAnimationEnabled',Game.PlayerDirectionAnimationEnabled);
      i:=IniFile.ReadInteger('Game','AnimateDoMoveMS',Game.AnimateDoMoveMS);
      if (i>=0) and (i<=MAX_ANIMATION_TIME_MS) then Game.AnimateDoMoveMS:=i;
      i:=IniFile.ReadInteger('Game','AnimateUndoMoveMS',Game.AnimateUndoMoveMS);
      if (i>=0) and (i<=MAX_ANIMATION_TIME_MS) then Game.AnimateUndoMoveMS:=i;
      i:=IniFile.ReadInteger('Game','AnimateReplayMovesMS',Game.AnimateReplayMovesMS);
      if (i>=1000 div MAX_REPLAY_SPEED_MOVES_PER_SEC) and
         (i<=MAX_ANIMATION_TIME_MS) then
         Game.AnimateReplayMovesMS:=i;
      Game.AnimateMovesOnMouseWheelUpDown:=IniFile.ReadBool('Game','AnimateMovesOnMouseWheelUpDown',Game.AnimateMovesOnMouseWheelUpDown);
      Game.ForkliftDrivingEnabled:=IniFile.ReadBool('Game','ForkliftDrivingEnabled',Game.ForkliftDrivingEnabled);
      i:=IniFile.ReadInteger('Game','ForkliftDrivingDriveInReverseSquares',Game.ForkliftDrivingDriveInReverseSquares);
      if (i>=0) and (i<=100) then Game.ForkliftDrivingDriveInReverseSquares:=i;
      i:=IniFile.ReadInteger('Game','PathFindingMaxTimeMS',Game.PathFindingMaxTimeMS);
      if (i>=MIN_PATH_FINDING_MAX_TIME_MS) and (i<=MAX_PATH_FINDING_MAX_TIME_MS) then Game.PathFindingMaxTimeMS:=i;
      i:=IniFile.ReadInteger('Game','CalculatePushesLowerBoundTimeLimitMS',Self.Deadlocks.CalculatePushesLowerBoundTimeLimitMS);
      if (i>=MIN_PATH_FINDING_MAX_TIME_MS) and (i<=MAX_PUSHES_LOWER_BOUND_CALCULATION_MAX_TIME_MS) then
         Self.Deadlocks.CalculatePushesLowerBoundTimeLimitMS:=i;
      Game.PathFindingOptimizeMoves:=IniFile.ReadBool('Game','PathFindingOptimizeMoves',Game.PathFindingOptimizeMoves);
      i:=IniFile.ReadInteger('Game','PauseBeforeReplayMS',PauseBeforeReplayMS);
      if (i>=0) and (i<=5*PauseBeforeReplayMS) then PauseBeforeReplayMS:=i; // 5: arbitrary limit
      ShowSolutionMoves:=IniFile.ReadBool('Game','ShowSolutionMoves',ShowSolutionMoves);
      Game.SaveBestSolutionsAutomatically:=IniFile.ReadBool('Game','SaveSolutionsAutomatically',Game.SaveBestSolutionsAutomatically);
      Game.SaveOldSolutionsAfterFindingBetterOnes:=IniFile.ReadBool('Game','SaveOldSolutionsAfterFindingBetterOnes',Game.SaveOldSolutionsAfterFindingBetterOnes);
      Game.SaveOldSolutionsAfterClipboardImport:=IniFile.ReadBool('Game','SaveOldSolutionsAfterClipboardImport',Game.SaveOldSolutionsAfterClipboardImport);
      Game.SolutionsRequireAtLeastOnePush:=IniFile.ReadBool   ('Game','SolutionsRequireAtLeastOnePush',Game.SolutionsRequireAtLeastOnePush);
{
      with ScoreMetricsWeights do begin
        i:=IniFile.ReadInteger('Game','ScoreMetricsWeights.BoxChanges',BoxChanges);
        if (i>=0) and (i<=MAX_SCORE_METRICS_WEIGHT) then BoxChanges:=i;
        i:=IniFile.ReadInteger('Game','ScoreMetricsWeights.BoxLines',BoxLines);
        if (i>=0) and (i<=MAX_SCORE_METRICS_WEIGHT) then BoxLines:=i;
        i:=IniFile.ReadInteger('Game','ScoreMetricsWeights.PushingSessions',PushingSessions);
        if (i>=0) and (i<=MAX_SCORE_METRICS_WEIGHT) then PushingSessions:=i;
        end;
}
      Game.SecondaryMetricsInTitles:=IniFile.ReadBool('Game','SecondaryMetricsInTitles',Game.SecondaryMetricsInTitles);
      Game.SaveSnapshotsAutomatically:=IniFile.ReadBool('Game','SaveSnapshotsAutomatically',Game.SaveSnapshotsAutomatically);
      Game.ShowBoxStartPositionsAsGoalsInReverseMode:=IniFile.ReadBool('Game','ShowBoxStartPositionsAsGoalsInReverseMode',Game.ShowBoxStartPositionsAsGoalsInReverseMode);
//    LastGameReverseMode:=IniFile.ReadBool('Game','ReverseMode',Game.ReverseMode);
      LastGameReverseMode:=False; //it might be confusing for a casual user if the program starts in reverse mode; hence, the previous value isn't used

      with Game.DeadlockDetection do begin
        Enabled:=IniFile.ReadBool('Game','DeadlockDetectionEnabled',Enabled);
        i:=IniFile.ReadInteger('Game','DeadlockDetectionType',Ord(DeadlockDetectionType));
        if (i>=Ord(Low(DeadlockDetectionType))) and (i<=Ord(High(DeadlockDetectionType))) then
           DeadlockDetectionType:=TLowMediumHigh(i);
        BlockMoves:=IniFile.ReadBool('Game','DeadlockDetectionBlockMoves',BlockMoves);
        LogEnabled:=IniFile.ReadBool('Game','DeadlockDetectionLogEnabled',LogEnabled);
        if   Enabled and BlockMoves then
             Game.SimpleIllegalMovesMask:=SIMPLE_ILLEGAL_MOVES_MASK
        else Game.SimpleIllegalMovesMask:=0;
//      LogSquareGoalDistances:=IniFile.ReadBool('Game','LogSquareGoalDistances',LogSquareGoalDistances);
        ShowSimpleDeadSquaresColor:=IniFile.ReadInteger('Screen','DeadlockDetectionShowSimpleDeadSquaresColor',Integer(ShowSimpleDeadSquaresColor));
        ShowSimpleDeadSquaresEnabled:=IniFile.ReadBool('Screen','DeadlockDetectionShowSimpleDeadSquaresEnabled',ShowSimpleDeadSquaresEnabled);
        ShowSimpleDeadSquaresTransparencyPct:=Max(0,Min(100,IniFile.ReadInteger('Screen','DeadlockDetectionShowSimpleDeadSquaresTransparencyPct',ShowSimpleDeadSquaresTransparencyPct)));
        end;

      UndoRedoCombinedMoves:=IniFile.ReadBool('Game','UndoRedoCombinedMoves',UndoRedoCombinedMoves);
      OptimizeMovesBetweenPushes:=IniFile.ReadBool('Game','OptimizeMovesBetweenPushes',OptimizeMovesBetweenPushes);

      i:=IniFile.ReadInteger('Game','OnLeftClickEmptySquareAction',Ord(OnLeftClickEmptySquareAction));
      if (i>=Ord(Low(OnLeftClickEmptySquareAction))) and (i<=Ord(High(OnLeftClickEmptySquareAction))) then
         OnLeftClickEmptySquareAction:=TOnLeftClickEmptySquareAction(i);
      i:=IniFile.ReadInteger('Game','OnRightClickAction',Ord(OnRightClickAction));
      if (i>=Ord(Low(OnRightClickAction))) and (i<=Ord(High(OnRightClickAction))) then
         OnRightClickAction:=TOnRightClickAction(i);
      i:=IniFile.ReadInteger('Game','OnShiftKeyAction',Ord(OnShiftKeyAction));
      if (i>=Ord(Low(OnShiftKeyAction))) and (i<=Ord(High(OnShiftKeyAction))) then
         OnShiftKeyAction:=TOnShiftKeyAction(i);
      Game.TimingEnabled:=IniFile.ReadBool('Game','TimingEnabled',Game.TimingEnabled);
      Game.TimingIdleTimeThresholdEnabled:=IniFile.ReadBool('Game','TimingIdleTimeThresholdEnabled',Game.TimingIdleTimeThresholdEnabled);
      i:=1000*IniFile.ReadInteger('Game','TimingIdleTimeThresholdSeconds',Game.TimingIdleTimeThresholdMS div 1000);
      if (i>=0) and (i<=MAX_TIMING_IDLE_TIME_Threshold_MS) then Game.TimingIdleTimeThresholdMS:=i;
      Game.RestoreSaveGame:=IniFile.ReadBool('Game','RestoreSaveGameOnOpen',Game.RestoreSaveGame);
      AutoAdvanceWhenSolved:=IniFile.ReadBool('Game','AutoAdvanceWhenSolved',AutoAdvanceWhenSolved);
      MouseTrackingModeEnabled:=IniFile.ReadBool('Game','MouseTrackingModeEnabled',MouseTrackingModeEnabled);
      AddFileFormatDescriptionToFiles:=IniFile.ReadBool('Game','AddFileFormatDescriptionToFiles',AddFileFormatDescriptionToFiles);
      SokoFile.AddFileFormatDescriptionToFiles:=AddFileFormatDescriptionToFiles;
      Game.PrettyPrintGamesEnabled:=IniFile.ReadBool('Game','PrettyPrintGamesEnabled',Game.PrettyPrintGamesEnabled);
      CopyLevelToClipboardFillFloors:=IniFile.ReadBool('Game','CopyLevelToClipboardFillFloors',CopyLevelToClipboardFillFloors);
      s:=IniFile.ReadString ('Game','CopyLevelToClipboardFloorFillCharacter','');
      i:=StrIndexOfCI(s,FloorFillCharacter);
      if (i>=Low(FloorFillCharacter)) and (i<=High(FloorFillCharacter)) and (Length(s)>=3) then
         CopyLevelToClipboardFloorFillCharacter:=StrWithoutDoubleQuotes(s)[1];
      CopyLevelToClipboardPreserveCombinedMoves:=IniFile.ReadBool('Game','CopyLevelToClipboardPreserveCombinedMoves',CopyLevelToClipboardPreserveCombinedMoves);
      CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles:=IniFile.ReadBool('Game','CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles',CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles);
      CopyLevelCollectionToClipboardIncludeLevelComments:=IniFile.ReadBool('Game','CopyLevelCollectionToClipboardIncludeLevelComments',CopyLevelCollectionToClipboardIncludeLevelComments);
      s:=IniFile.ReadString ('Game','RunLengthEncodingFloor','');
      i:=StrIndexOfCI(s,FloorFillCharacter);
      if (i>=Low(FloorFillCharacter)) and (i<=High(FloorFillCharacter)) and (Length(s)>=3) then
         RunLengthEncodingFloor:=StrWithoutDoubleQuotes(s)[1];
      i:=IniFile.ReadInteger('Screen','BoardDimensionsAsText',Ord(BoardDimensionsAsText));
      if (i>=Ord(Low(BoardDimensionsAsText))) and (i<=Ord(High(BoardDimensionsAsText))) then
         BoardDimensionsAsText:=TBoardDimensionsAsText(i);
      BoardDimensionsWithFloorCount:=IniFile.ReadBool('Screen','BoardDimensionsWithFloorCount',BoardDimensionsWithFloorCount);
      i:=IniFile.ReadInteger('Screen','DisplayDriverChangedAction',Ord(DisplayDriverChangedAction));
      if (i>=Ord(Low(DisplayDriverChangedAction))) and (i<=Ord(High(DisplayDriverChangedAction))) then
         DisplayDriverChangedAction:=TDisplayDriverChangedAction(i);
      ConfirmRunningMultipleInstances:=IniFile.ReadBool('Screen','ConfirmRunningMultipleInstances',ConfirmRunningMultipleInstances);
      ScreenSaverEnabled:=IniFile.ReadBool('Screen','ScreenSaverEnabled',ScreenSaverEnabled);
      UseOnlyOneProcessor:=IniFile.ReadBool('Application','UseOnlyOneProcessor',UseOnlyOneProcessor);
      i:=IniFile.ReadInteger('Screen','SelectionCursorType',Ord(SelectionCursorType));
      if (i>=Ord(Low(SelectionCursorType))) and (i<=Ord(High(SelectionCursorType))) then
         SelectionCursorType:=TCursorType(i);
      i:=IniFile.ReadInteger('Screen','DragCursorType',Ord(DragCursorType));
      if (i>=Ord(Low(DragCursorType))) and (i<=Ord(High(DragCursorType))) then
         DragCursorType:=TCursorType(i);
      i:=IniFile.ReadInteger('Screen','TrackingCursorType',Ord(TrackingCursorType));
      if (i>=Ord(Low(TrackingCursorType))) and (i<=Ord(High(TrackingCursorType))) then
         TrackingCursorType:=TCursorType(i);
      i:=IniFile.ReadInteger('Screen','OnEscapeKeyAction',Ord(OnEscapeKeyAction));
      if (i>=Ord(Low(OnEscapeKeyAction))) and (i<=Ord(High(OnEscapeKeyAction))) then
         OnEscapeKeyAction:=TOnEscapeKeyAction(i);

      Left           :=Max(Screen.DeskTopLeft,Min(Screen.DeskTopLeft+Screen.DeskTopWidth -MIN_MAIN_WINDOW_WIDTH,IniFile.ReadInteger('MainWindow','Left',Left)));
      Top            :=Max(Screen.DeskTopTop ,Min(Screen.DeskTopTop +Screen.DeskTopHeight-MIN_MAIN_WINDOW_HEIGHT-40,IniFile.ReadInteger('MainWindow','Top',Top)));
      Width          :=Max(MIN_MAIN_WINDOW_WIDTH ,Min(Screen.DeskTopWidth-Left,IniFile.ReadInteger('MainWindow','Width',Width)));
      Height         :=Max(MIN_MAIN_WINDOW_HEIGHT,Min(Screen.DeskTopHeight-Top-40,IniFile.ReadInteger('MainWindow','Height',Height))); // -40: normally sufficient to avoid collision with start-menu
      BoardDimensionsInTitleLine:=IniFile.ReadBool('MainWindow','BoardDimensionsInTitleLine',BoardDimensionsInTitleLine);
      CollectionNameInTitleLine :=IniFile.ReadBool('MainWindow','CollectionNameInTitleLine',CollectionNameInTitleLine);
      SecondaryMetricsInSolutionButtonHint:=IniFile.ReadBool('MainWindow','SecondaryMetricsInSolutionButtonHint',SecondaryMetricsInSolutionButtonHint);
      ErrorRecoveryEnabled:=IniFile.ReadBool('MainWindow','ErrorRecoveryEnabled',ErrorRecoveryEnabled);
      ShowBoardCoordinates:=IniFile.ReadBool('MainWindow','ShowBoardCoordinates',ShowBoardCoordinates);

      fBevelVisible  :=IniFile.ReadBool('Screen','BevelVisible',BevelVisible);
      BevelColor     :=TColor(IniFile.ReadInteger('Screen','BevelColor',Integer(BevelColor)));
      SwMaximize     :=IniFile.ReadBool('Screen','Maximized',SwMaximize);
      StatusBar1.Color:=TColor(IniFile.ReadInteger('Screen','StatusBar.Color',Integer(StatusBar1.Color)));
      F:=StatusBar1.Font;
      LoadFontFromIniFile(IniFile,'Screen','StatusBar.',F);
      StatusBar1.Font:=F;
      MultiViewPopupMenu.MouseDownClick:=False; // 'mouse up' = 'click'

      InternetLevelLookupFormatString:=IniFile.ReadString('MainWindow','InternetLevelLookupFormatString',InternetLevelLookupFormatString);

      with StatusBar1 do begin
//      Color:=clNavy;
//      Font.Name:='Arial';
//      Font.Size:=8;
//      Font.Color:=clLime;//clWhite;
        end;

      GamePictures.LoadSettingsFromIniFile(IniFile);
      GameViewer.LoadSettingsFromIniFile(IniFile,GAME_VIEWER_INI_FILE_SECTION_NAME);

      OpenForm.LoadSettingsFromIniFile(IniFile);

      Menu.LoadSettingsFromIniFile(IniFile,'Menu');

      Status.LoadSettingsFromIniFile(IniFile,'Status');

      Sound.LoadSettingsFromIniFile(IniFile);
      Music.LoadSettingsFromIniFile(IniFile); // because of the way the program evolved, the sound volume is stored by 'MusicManager' even though the music player isn't supported by the current version of the program

      {$IFDEF MUSIC_PLAYER}
        MPlayer.LoadSettingsFromIniFile(IniFile);
        MPlayer.Display.LoadSettingsFromIniFile(IniFile);
        MPlayerForm.LoadSettingsFromIniFile(IniFile);
        Viewer1.LoadSettingsFromIniFile(IniFile);
        Fractals.LoadSettingsFromIniFile(IniFile);
//      Fireworks.LoadSettingsFromIniFile(IniFile);
        Mandala.LoadSettingsFromIniFile(IniFile);
      {$ENDIF}

      RotateAndFlipPopupMenu.LoadSettingsFromIniFile(IniFile);
      MultiViewPopupMenu.FormColors:=RotateAndFlipPopupMenu.FormColors; // use the same colors for all popup menus
      SnapshotsForm.LoadSettingsFromIniFile(IniFile);
//    EditForm.LoadSettingsFromIniFile(IniFile);
      ToolsForm.LoadSettingsFromIniFile(IniFile);
      LevelSetForm.LoadSettingsFromIniFile(IniFile,INTERNAL_CLIPBOARD_INIFILE_SECTION);
      Skins.LoadSettingsFromIniFile(IniFile);
//    FileAssoc.LoadSettingsFromIniFile(IniFile);

    {$ENDIF}

      Solver.LoadSettingsFromIniFile(INIFILE_SOLVER_SECTION,IniFile);
      Optimizer.LoadSettingsFromIniFile(INIFILE_OPTIMIZER_SECTION,IniFile);
      Generator.LoadSettingsFromIniFile(INIFILE_GENERATOR_SECTION,IniFile);
      OptionsForm.LoadSettingsFromIniFile(IniFile);
      ToolsForm.LoadSettingsFromIniFile(IniFile);
      GeneratorForm.LoadSettingsFromIniFile(IniFile); 
      DuplicatesForm.LoadSettingsFromIniFile(IniFile);
      MultiView.LoadSettingsFromIniFile(IniFile,MULTI_VIEW_INI_FILE_SECTION_NAME);
      CaptureForm.LoadSettingsFromIniFile(IniFile);
      HelpForm.LoadSettingsFromIniFile(IniFile);
      
      if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<477) then begin
         {version 1.477 split the folder history and the file history in two sets,}
         {so there is one set for 'Open levels' and another set for 'Open all other files'}
         s:=OpenForm.Name+SUB_TITLE_SEPARATOR+'FolderHistoryComboBox'; // old name
         LoadComboBoxFromIniFile(IniFile,s,MAX_FILE_HISTORY_ITEMS,True ,False,True,True,OpenForm.LevelsFolderHistoryComboBox);
         IniFile.EraseSection(s);
         s:=OpenForm.Name+SUB_TITLE_SEPARATOR+'FileHistoryComboBox';   // old name
         LoadComboBoxFromIniFile(IniFile,s,MAX_FILE_HISTORY_ITEMS,False,True ,True,True,OpenForm.LevelsFileHistoryComboBox  );
         IniFile.EraseSection(s);
         with OpenForm.LevelsFolderHistoryComboBox do // copy the old folder history to the new 'anything but levels' folder history
           for i:=Pred(Items.Count) downto 0 do
               AddItemOrMoveItemToTopOfComboBox(OpenForm.AnythingButLevelsFolderHistoryComboBox,MAX_FILE_HISTORY_ITEMS,Items[i],True);
         with OpenForm.LevelsFileHistoryComboBox do   // copy the old file history   to the new 'anything but levels' file history
           for i:=Pred(Items.Count) downto 0 do
               AddItemOrMoveItemToTopOfComboBox(OpenForm.AnythingButLevelsFileHistoryComboBox  ,MAX_FILE_HISTORY_ITEMS,Items[i],True);
         end;

    finally
      if (ApplicationMutex<>0) and
         (WaitForSingleObject(ApplicationMutex,INFINITE)<>WAIT_OBJECT_0) then
         raise Exception.Create(TEXT_PROCESS_SYNCHRONIZATION_ERROR);
      try
        IniFile.Free;
      finally
        if (ApplicationMutex<>0) and
           (not ReleaseMutex(ApplicationMutex)) then
           raise Exception.Create(TEXT_PROCESS_SYNCHRONIZATION_ERROR);
      end;
    end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<124) and
       (GameViewer.LegalMovesInfo.BoxCursor.PenWidth=2) then
       {version 1.124 introduced the box-cursor shadow and changed}
       {the default pen-width back from 2 to 1;}
       {a slim 1-pixel wide shadowed cursor looks more elegant than a}
       {2-pixel "fat" unshadowed cursor, and the shadow makes up for}
       {the loss of visibility that otherwise can make a 1-pixel line}
       {hard to see on a monitor with a high resolution}
       GameViewer.LegalMovesInfo.BoxCursor.PenWidth:=1;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<129) and
       (GamePictures.Pictures[ptFloor]<>nil) then with GamePictures.Pictures[ptFloor] do begin
       {version 1.129 introduced floor-tiles}
       FileName:=DEFAULT_VALUE;
       SourceRect:=GamePictures.DefaultRect[ptFloor];
       Visible:=False;
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<226) then with GamePictures do begin
       if WallCap.X=0 then WallCap.X:=-1; // '-1': default offset = midways between the squares
       if WallCap.Y=0 then WallCap.Y:=-1; // '-1': default offset = midways between the squares
       end;

    if (OldMajorVersionNumber<=1) and (OldMinorVersionNumber<267) then begin
       // SetDefaultScoreMetricsWeights; // default values changed in version 1.267; weights were abandoned in 1.424
       end;

    if (OldMajorVersionNumber<=1) and (OldMinorVersionNumber<275) then with Game do begin
       // default animation speed changed in version 1.275
       if   AnimateDoMoveMS   =50 then // '50' was the old default value
            AnimateDoMoveMS  :=DEFAULT_ANIMATE_DO_MOVE_MS;
       if   AnimateUndoMoveMS =25 then // '25' was the old default value
            AnimateUndoMoveMS:=DEFAULT_ANIMATE_UNDO_MOVE_MS;
       end;
    if (OldMajorVersionNumber<=1) and (OldMinorVersionNumber<281) then with GamePictures do with Pictures[ptWall] do begin
       // default wall changed to a seamless wall in 1.281
       if StrEqual(FileName,DEFAULT_VALUE) and
          CompareMem(Addr(SourceRect),Addr(DefaultRect[ptWall]),SizeOf(SourceRect)) then
          WallType:=wtSeamlessWallWithCap; // 'GamePictures.LoadPictures()' ensures that other settings are adjusted appropiately
       end;

    {$IFNDEF TEST}

      Sound.Initialize;
      Music.Initialize;

    {$ENDIF}

    OpenDialog1.Filter      :=OpenDialogFilterText;
    SaveDialog1.Filter      :=OpenDialog1.Filter;

    SetMessageHandlers;

    {$IFNDEF TEST}

      if not ((ParamCount>=1) and StrEqual(ParamStr(1),COMMAND_LINE_PARAMETER_INSTALL)) then begin
         if Menu.Visible then Menu.MakeButtons;
         ShowMenu;

         Status.MakeStatusBar;

         GamePictures.LoadPictures;
         if not GamePictures.Initialized then Game.FileName:='';
         end;

      if FirstTime then begin
         MakeFilesFromResources(OldMajorVersionNumber,OldMinorVersionNumber); // first time the application is activated
         AddPluginsFromSokobanPP('Solvers'   ,DEFAULT_SOLVER_FILE_NAME_STUB   ,ToolsForm.SolverComboBox);    // find solvers plugged into Sokoban++, if any
         AddPluginsFromSokobanPP('Optimizers',DEFAULT_OPTIMIZER_FILE_NAME_STUB,ToolsForm.OptimizerComboBox); // find optimizers plugged into Sokoban++, if any
         AddPluginsFromSokobanPP('Plugins'   ,DEFAULT_SOLVER_FILE_NAME_STUB   ,ToolsForm.SolverComboBox);    // find solvers plugged into Sokoban++; plugins that aren't solvers will be filtered out later
         AddPluginsFromSokobanPP('Plugins'   ,DEFAULT_OPTIMIZER_FILE_NAME_STUB,ToolsForm.OptimizerComboBox); // find optimizers plugged into Sokoban++; ; plugins that aren't optimizers will be filtered out later
         LookForExternalHelpFile;
         end;

      s:=StrWithTrailingPathDelimiter(Skins.ScriptsPath)+YASC_SETTINGS_RES_NAME+SKIN_SCRIPT_FILE_EXT;
      if FileExists(s) then // 'True': this is a development version
         MainForm.Skins.SettingsScriptFileName:=s;

      MakeUndoButtonHint;

      MenuItemNormalMode.Hint:=GameModeHintText[True];
      MenuItemReverseMode.Hint:=GameModeHintText[False];

    {$ENDIF}

    if Game.FileName='' then InitGame(True,False,False,False,True,True,True,0);

    Application.OnIdle:=OnIdleHandler;

    if IsFirstInstance then begin
       // process log files from prior sessions, if any
       if not ProcessLogFiles(lftIsEmpty) then ProcessLogFiles(lftImport);

       // delete secondary editor history files from prior sessions, if any;
       // with multiple instances of the program, each instance creates its own
       // editor history file for use during the entire session, but only one
       // editor history file survives now that only instance is opened;
       s:=StrWithoutTrailingPathDelimiter(ApplicationDataPath);
       if DirectoryExists(s) then with OpenForm.FileListBox1 do begin
          Directory:=s;
          Mask:=ExtractFileNameWithoutExt(Application.ExeName)+STAR+EDITOR_LOG_FILE_EXT;
          Update;

          // make the primary editor history filename
          s1:=StrWithTrailingPathDelimiter(ApplicationDataPath)+ExtractFileNameWithoutExt(Application.ExeName)+EDITOR_LOG_FILE_EXT;
          PrimaryEditorHistoryFileExists:=FileExists(s1);

          for i:=Pred(Items.Count) downto 0 do begin // for each editor history file...
              s:=StrWithTrailingPathDelimiter(Directory)+Items[i];
              if FileExists(s) and (not StrEqual(s,s1)) then begin  // 'True': the file exists, and it hasn't the primary editor history filename
                 if (not PrimaryEditorHistoryFileExists) then begin // 'True': rename the file to the default editor history filename
                    RenameFile(s,s1);
                    PrimaryEditorHistoryFileExists:=FileExists(s1);
                    if PrimaryEditorHistoryFileExists then s:='';   // 'True': rename succeeded, hence don't delete the file
                    end;
                 if s<>'' then                                      // 'True': delete the file
                    try    DeleteFile(s);
                    except on E:Exception do begin end;
                    end;
                 end;
              end;
          end;
       end;

//  TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTickCount);
//  Msg(IntToStr(TimeMS)+' milli seconds','Initialization',MB_OK);

//  Msg(IntToStr(SizeOf('')),'',MB_OK);
//  Log.Writeln('Start: '+DateTimeToStr(Now));
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  try
    Sound.Free; Sound:=nil;
    Music.Free; Music:=nil;
    Solver.Free; Solver:=nil;
    Optimizer.Free; Optimizer:=nil;
    Generator.Free; Generator:=nil;
    Viewer1.Free;
    Fractals.Free; Fireworks.Free; Mandala.Free;
    MultiView.Free; MultiView:=nil;   // the multi-viewer must be destroyed before 'GameViewer'
    GameViewer.Free; GameViewer:=nil; // the game-viewer must be destroyed before 'GamePictures'
    GamePictures.Free; GamePictures:=nil;
    Game.Free; Game:=nil;
    SokoFile.Free; SokoFile:=nil;
//  OpenForm.Free;                  // 'OpenForm'       is owned and destroyed by 'Application'
//  OptionsForm.Free;               // 'OptionsForm'    is owned and destroyed by 'Application'
//  MPlayerForm.Free;               // 'MPlayerForm'    is owned and destroyed by 'Application'
//  PopupForm.Free;                 // 'PopupForm'      is owned and destroyed by 'Application'
//  VarForm.Free;                   // 'VarForm'        is owned and destroyed by 'Application'
//  LevelSetForm.Free;              // 'LevelSetForm'   is owned and destroyed by 'Application'
//  ToolsForm.Free;                 // 'ToolsForm'      is owned and destroyed by 'Application'
//  GeneratorForm.Free;             // 'GeneratorForm'  is owned and destroyed by 'Application'
//  DuplicatesForm.Free;            // 'DuplicatesForm' is owned and destroyed by 'Application'
//  CaptureForm.Free;               // 'CaptureForm'    is owned and destroyed by 'Application'
//  HelpForm.Free;                  // 'HelpForm'       is owned and destroyed by 'Application'
    MPlayer.Display.Menu:=nil; {otherwise crash in: } MPlayer.Free; MPlayer:=nil;
    Menu.Free;
    RotateAndFlipPopupMenu.Free;
    MultiViewPopupMenu.Free;
    Status.Free;
    Skins.Free; Skins:=nil; // 'nil': 'OpenForm' tries to update skins during shutdown
    Deadlocks.Free;
//  FileAssoc.Free;
    PNGImageLoader.Free;
    SolutionLogFiles.Free; SolutionLogFiles:=nil;
//  Log.Free;
  except on E:Exception do begin
            // ignore exceptions
            end;
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
var i:Integer; s,s1:String; SearchRec:TSearchRec;
    Buffer:array[0..8*1024] of Char; FilePart:PChar;
begin // FormActivate
  // the documententation says that 'OnActivate' is called for the form when
  // it receives focus, but practice shows that 'OnActivate' isn't called when
  // the form gets focus as the result of closing another form; therefore,
  // don't rely on this procedure being called automatically

  if not Initialized then begin
     Visible:=True;

     GetDevInfos;

     if (not IsFirstInstance) and
        ConFirmRunningMultipleInstances and
        (Msg(ApplicationIsAlreadyRunningText,TEXT_APPLICATION_TITLE,MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)=ID_NO) then begin
        QuitWithoutSaving:=True;
        PostMessage(Self.Handle,MSG_SHUTDOWN,0,0);
        end
     else if GamePictures.Initialized then begin

             s:=Game.FileName;
             if       ParamCount>=1 then begin
                      s1:=ParamStr(1);
                      if   s1<>'' then begin
                           i:=GetFullPathName(Addr(s1[1]),SizeOf(Buffer),Buffer,FilePart);
                           if (i>0) and (i<SizeOf(Buffer)) then
                              SetString(s1,Buffer,i);
                           end;
                      s1:=ExpandFileName(s1);
                      if   IsAnIniFileSectionFileName(s1) then
                           if   FileExists(ExtractIniFileName(s1)) and
                                LoadGame(s1,True) then //
                           else Game.FileName:=''
                      else if   FileExists(s1)
                                and
                                LoadGame(s1,True) then //
                           else Game.FileName:='';

                      end;
             if       Game.FileName='' then
                      Game.FileName:=s; // restore game filename in case there wasn't a command-line parameter or in case loading that file failed

             if       Game.FileName<>'' then begin
                      if   IsAnIniFileSectionFileName(Game.FileName) then
                           try     if   FileExists(ExtractIniFileName(Game.FileName)) and
                                        LoadGame(Game.FileName,False) then //
                                   else begin s:=ExtractIniFileName(Game.FileName);
                                              if        FileHasMoved(s) and LoadGame(MakeIniFileSectionFileName(s,ExtractSectionName(Game.FileName)),False) then
                                              else if   FileExists  (s) and LoadGame(s,False) then
                                                   else Game.FileName:=''
                                        end;
                           finally Game.Verbose:=True;
                           end
                      else if   FileExists(Game.FileName)
                                and
                                LoadGame(Game.FileName,True) then begin
                                end
                           else begin s:=Game.FileName;
                                      if   FileHasMoved(s) and LoadGame(s,True) then
                                      else Game.FileName:='';
                                end
                      end;

             if      Game.FileName='' then begin
                     Game.FileName:=ApplicationDataPath+
                                      DEFAULT_LEVEL_DIRECTORY+FILE_NAME_PATH_DELIMITER+
                                      DEFAULT_LEVEL_FILE_NAME;
                     if   FileExists(Game.FileName)
                          and
                          LoadGame(Game.FileName,True) then //
                     else Game.FileName:='';
                     end;

             if      Game.FileName='' then begin
                     Game.FileName:=ApplicationDataPath+DEFAULT_LEVEL_FILE_NAME;
                     if   FileExists(Game.FileName)
                          and
                          LoadGame(Game.FileName,True) then //
                     else Game.FileName:='';
                     end;

             if      Game.FileName='' then begin
                     s:=ApplicationDataPath+DEFAULT_LEVEL_DIRECTORY;
                     if FindFirst(s+'\*'+SOKOBAN_FILE_NAME_EXT,faAnyFile,SearchRec)=0 then
                        Game.FileName:=s+'\'+SearchRec.Name;
                     FindClose(SearchRec);

                     if Game.FileName='' then begin
                        s:=ApplicationDataPath;
                        if FindFirst(s+'*'+SOKOBAN_FILE_NAME_EXT,faAnyFile,SearchRec)=0 then
                           Game.FileName:=s+SearchRec.Name;
                        FindClose(SearchRec);
                        end;

                     if (Game.FileName<>'') and (not LoadGame(Game.FileName,True)) then
                        Game.FileName:='';
                     end;

             if      (Game.FileName<>'') and
                     (Game.SaveGame=nil) and
                     (Game.ReverseMode<>LastGameReverseMode) then
                     BtnReverseModeClick(Self);
             end;

     if      Game.FileName='' then begin
             ShowTitle('');
             OpenDialog1.InitialDir:=ApplicationDataPath;
             end
     else if IsAnIniFileSectionFileName(Game.FileName) then
             OpenDialog1.InitialDir:=ExtractFilePath(ExtractIniFileName(Game.FileName))
     else    OpenDialog1.InitialDir:=ExtractFilePath(Game.FileName);
     SaveDialog1.InitialDir:=OpenDialog1.InitialDir;

     if SwMaximize then begin
        WindowState:=wsMaximized;
        end;

     with Music do // kludge: when 'Music.Player' is created on demand; the mainform suffers from flickering; at least avoid it the first time;
       if Player=nil then CreatePlayer(Music.PlayerNotify);

     //OptionsForm.TreeView1.FullCollapse;
     with OptionsForm.TreeView1 do begin
       Selected:=Items[0];
       for i:=Pred(Items.Count) downto 0 do begin
           //if Items[i].Expanded then Items[i].Collapse(False);
           end;
       end;

     AdHocTask;
     end;

  MakeDynamicHints(nil);
  if SnapshotsForm<>nil then SnapshotsForm.StatusBar1.SimpleText:='';
  CtrlEnterKeySet:=[]; LastKey:=0;
  IgnoreKeyUp:=True; IgnoreMouseUp:=True; IgnoreShiftKey:=False; // a 'KeyUp' or a 'MouseUp' event may come after a 'KeyDown' or 'MouseDown' on another form
  if Status<>nil then with Status do with Panels[spReplaySpeed] do begin
    ProgressBarLocked:=True; ProgressBarLockOnMouseUp:=False; // click the panel to activate it
    ProgressBarFocused:=False;
    ReplayMovesPerSec:=ReplayMovesPerSec; // if it's visible then show it non-focused
    end;
  ShowStatus;

//ExtractLevelsSoko15;
//ExtractLevelsWinSoko;
//if OpenForm<>nil then OpenForm.ShowModal;
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
  CtrlEnterKeySet:=[]; LastKey:=0;
  IgnoreKeyUp:=True; IgnoreMouseUp:=True; IgnoreShiftKey:=False; // a 'KeyUp' or a 'MouseUp' event may come after a 'KeyDown' or a 'MouseDown' on another form
  MultiView.HideDragRect;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CloseForm;
end;

procedure TMainForm.CloseForm;
var DateTime : TDateTime; s:String; IniFile:TIniFile;
begin
  MPlayer.Hide;
  if SnapshotsForm<>nil then with SnapshotsForm do begin
     ShowOnStartup:=Visible; Hide;
     end;

  Sound.Finalize;
  Music.Finalize;

  if not QuitWithoutSaving then begin
     s:=ApplicationDataPath+ExtractFileName(ChangeFileExt(Application.ExeName,INI_FILE_EXT));
     //DeleteFile(s);
     if (ApplicationMutex<>0) and
        (WaitForSingleObject(ApplicationMutex,INFINITE)<>WAIT_OBJECT_0) then begin
        // do nothing; it's better to exit normally;
        //raise Exception.Create(TEXT_PROCESS_SYNCHRONIZATION_ERROR);
        end;
     try
       IniFile:=TIniFile.Create(s);
       IniFile.WriteString ('Sokoban','Sokoban',TEXT_SOKOBAN_COPYRIGHT);
       IniFile.WriteString ('Sokoban','Program','Sokoban YASC Copyright (c) 2001-2019 by Brian Damgaard, Denmark');
       IniFile.WriteString ('Sokoban','Version','1.654');
       IniFile.WriteInteger('Installation','FileAge',InstallationFileAge);
       IniFile.WriteString ('Installation','HelpFileName',HelpFileName);
       IniFile.WriteBool   ('Application','UseOnlyOneProcessor',UseOnlyOneProcessor);
       IniFile.WriteString ('Application','ApplicationDataPath',StrWithoutTrailingPathDelimiter(fApplicationDataPath));
       IniFile.WriteString ('Application','PluginsPath',StrWithoutTrailingPathDelimiter(fPluginsPath));
       IniFile.WriteString ('Game','FileName',Game.LastValidFileName);
       IniFile.WriteBool   ('Game','MoveAnimationEnabled',Game.MoveAnimationEnabled);
       IniFile.WriteBool   ('Game','SmoothMoveAnimationEnabled',Game.SmoothMoveAnimationEnabled);
       IniFile.WriteBool   ('Game','PlayerDirectionAnimationEnabled',Game.PlayerDirectionAnimationEnabled);
//     IniFile.WriteBool   ('Game','SmoothMoveAnimationThresholdEnabled',Game.SmoothMoveAnimationThresholdEnabled);
//     IniFile.WriteInteger('Game','SmoothMoveAnimationThresholdMaxPixelsPerMove',Game.SmoothMoveAnimationThresholdMaxPixelsPerMove);
       IniFile.WriteInteger('Game','AnimateDoMoveMS',Game.AnimateDoMoveMS);
       IniFile.WriteInteger('Game','AnimateUndoMoveMS',Game.AnimateUndoMoveMS);
       IniFile.WriteInteger('Game','AnimateReplayMovesMS',Game.AnimateReplayMovesMS);
       IniFile.WriteBool   ('Game','AnimateMovesOnMouseWheelUpDown',Game.AnimateMovesOnMouseWheelUpDown);       
       IniFile.WriteBool   ('Game','ForkliftDrivingEnabled',Game.ForkliftDrivingEnabled);
       IniFile.WriteInteger('Game','ForkliftDrivingDriveInReverseSquares',Game.ForkliftDrivingDriveInReverseSquares);
       IniFile.WriteInteger('Game','PathFindingMaxTimeMS',Game.PathFindingMaxTimeMS);
       IniFile.WriteInteger('Game','CalculatePushesLowerBoundTimeLimitMS',Self.Deadlocks.CalculatePushesLowerBoundTimeLimitMS);
       IniFile.WriteBool   ('Game','PathFindingOptimizeMoves',Game.PathFindingOptimizeMoves);
       IniFile.WriteBool   ('Game','ShowSolutionMoves',ShowSolutionMoves);
       IniFile.WriteBool   ('Game','SaveSolutionsAutomatically',Game.SaveBestSolutionsAutomatically);
       IniFile.WriteBool   ('Game','SaveOldSolutionsAfterFindingBetterOnes',Game.SaveOldSolutionsAfterFindingBetterOnes);
       IniFile.WriteBool   ('Game','SaveOldSolutionsAfterClipboardImport',Game.SaveOldSolutionsAfterClipboardImport);
       IniFile.WriteBool   ('Game','SolutionsRequireAtLeastOnePush',Game.SolutionsRequireAtLeastOnePush);
//     IniFile.WriteInteger('Game','ScoreMetricsWeights.BoxChanges',ScoreMetricsWeights.BoxChanges);
//     IniFile.WriteInteger('Game','ScoreMetricsWeights.BoxLines',ScoreMetricsWeights.BoxLines);
//     IniFile.WriteInteger('Game','ScoreMetricsWeights.PushingSessions',ScoreMetricsWeights.PushingSessions);
       IniFile.WriteBool   ('Game','SecondaryMetricsInTitles',Game.SecondaryMetricsInTitles);
       IniFile.WriteBool   ('Game','SaveSnapshotsAutomatically',Game.SaveSnapshotsAutomatically);
       IniFile.WriteBool   ('Game','ShowBoxStartPositionsAsGoalsInReverseMode',Game.ShowBoxStartPositionsAsGoalsInReverseMode);
       IniFile.WriteBool   ('Game','ReverseMode',Game.ReverseMode);
       IniFile.WriteBool   ('Game','DeadlockDetectionEnabled',Game.DeadlockDetection.Enabled);
       IniFile.WriteInteger('Game','DeadlockDetectionType',Ord(Game.DeadlockDetection.DeadlockDetectionType));
       IniFile.WriteBool   ('Game','DeadlockDetectionBlockMoves',Game.DeadlockDetection.BlockMoves);
       IniFile.WriteBool   ('Game','DeadlockDetectionLogEnabled',Game.DeadlockDetection.LogEnabled);
//     IniFile.WriteBool   ('Game','LogSquareGoalDistances',Game.DeadlockDetection.LogSquareGoalDistances);
       IniFile.WriteInteger('Screen','DeadlockDetectionShowSimpleDeadSquaresColor',Integer(ShowSimpleDeadSquaresColor));
       IniFile.WriteBool   ('Screen','DeadlockDetectionShowSimpleDeadSquaresEnabled',ShowSimpleDeadSquaresEnabled);
       IniFile.WriteInteger('Screen','DeadlockDetectionShowSimpleDeadSquaresTransparencyPct',ShowSimpleDeadSquaresTransparencyPct);
       IniFile.WriteBool   ('Game','UndoRedoCombinedMoves',UndoRedoCombinedMoves);
       IniFile.WriteInteger('Game','OnLeftClickEmptySquareAction',Ord(OnLeftClickEmptySquareAction));
       IniFile.WriteInteger('Game','OnRightClickAction',Ord(OnRightClickAction));
       IniFile.WriteInteger('Game','OnShiftKeyAction',Ord(OnShiftKeyAction));
       IniFile.WriteBool   ('Game','TimingEnabled',Game.TimingEnabled);
       IniFile.WriteBool   ('Game','TimingIdleTimeThresholdEnabled',Game.TimingIdleTimeThresholdEnabled);
       IniFile.WriteInteger('Game','TimingIdleTimeThresholdSeconds',Game.TimingIdleTimeThresholdMS div 1000);
       IniFile.WriteBool   ('Game','OptimizeMovesBetweenPushes',OptimizeMovesBetweenPushes);
       IniFile.WriteBool   ('Game','RestoreSaveGameOnOpen',Game.RestoreSaveGame);
       IniFile.WriteBool   ('Game','AutoAdvanceWhenSolved',AutoAdvanceWhenSolved);
       IniFile.WriteBool   ('Game','MouseTrackingModeEnabled',MouseTrackingModeEnabled);
       IniFile.WriteBool   ('Game','AddFileFormatDescriptionToFiles',AddFileFormatDescriptionToFiles);
       IniFile.WriteBool   ('Game','PrettyPrintGamesEnabled',Game.PrettyPrintGamesEnabled);
       IniFile.WriteBool   ('Game','CopyLevelToClipboardFillFloors',CopyLevelToClipboardFillFloors);
       IniFile.WriteString ('Game','CopyLevelToClipboardFloorFillCharacter',StrWithDoubleQuotes(CopyLevelToClipboardFloorFillCharacter));
       IniFile.WriteBool   ('Game','CopyLevelToClipboardPreserveCombinedMoves',CopyLevelToClipboardPreserveCombinedMoves);
       IniFile.WriteBool   ('Game','CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles',CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles);
       IniFile.WriteBool   ('Game','CopyLevelCollectionToClipboardIncludeLevelComments',CopyLevelCollectionToClipboardIncludeLevelComments);
       IniFile.WriteString ('Game','RunLengthEncodingFloor',StrWithDoubleQuotes(RunLengthEncodingFloor));

       IniFile.WriteInteger('Screen','BoardDimensionsAsText',Ord(BoardDimensionsAsText));
       IniFile.WriteBool   ('Screen','BoardDimensionsWithFloorCount',BoardDimensionsWithFloorCount);
       IniFile.WriteInteger('Screen','DisplayDriverChangedAction',Ord(DisplayDriverChangedAction));
       IniFile.WriteBool   ('Screen','ConfirmRunningMultipleInstances',ConfirmRunningMultipleInstances);
       IniFile.WriteBool   ('Screen','ScreenSaverEnabled',ScreenSaverEnabled);
       IniFile.WriteInteger('Screen','SelectionCursorType',Ord(SelectionCursorType));
       IniFile.WriteInteger('Screen','DragCursorType',Ord(DragCursorType));
       IniFile.WriteInteger('Screen','TrackingCursorType',Ord(TrackingCursorType));
       IniFile.WriteInteger('Screen','OnEscapeKeyAction',Ord(OnEscapeKeyAction));
       IniFile.WriteBool   ('Screen','BevelVisible',BevelVisible);
       IniFile.WriteInteger('Screen','BevelColor',Integer(BevelColor));
       IniFile.WriteBool   ('Screen','Maximized',WindowState=wsMaximized);

       IniFile.WriteInteger('Screen','StatusBar.Color',Integer(StatusBar1.Color));
       SaveFontToIniFile(IniFile,'Screen','StatusBar.',StatusBar1.Font);

       IniFile.WriteString ('MainWindow','InternetLevelLookupFormatString',InternetLevelLookupFormatString);

       if  WindowState=wsNormal then begin
           IniFile.WriteInteger('MainWindow','Left',Left);
           IniFile.WriteInteger('MainWindow','Top',Top);
           IniFile.WriteInteger('MainWindow','Width',Width);
           IniFile.WriteInteger('MainWindow','Height',Height);
           end;

       IniFile.WriteBool('MainWindow','BoardDimensionsInTitleLine',BoardDimensionsInTitleLine);
       IniFile.WriteBool('MainWindow','CollectionNameInTitleLine',CollectionNameInTitleLine);
       IniFile.WriteBool('MainWindow','SecondaryMetricsInSolutionButtonHint',SecondaryMetricsInSolutionButtonHint);
       IniFile.WriteBool('MainWindow','ErrorRecoveryEnabled',ErrorRecoveryEnabled);
       IniFile.WriteBool('MainWindow','ShowBoardCoordinates',ShowBoardCoordinates);

       GamePictures.SaveSettingsToIniFile(IniFile);
       GameViewer.SaveSettingsToIniFile(IniFile,'GameViewer');
       Sound.SaveSettingsToIniFile(IniFile);
       Music.SaveSettingsToIniFile(IniFile);
       OpenForm.SaveSettingsToIniFile(IniFile);
       Menu.SaveSettingsToIniFile(IniFile,'Menu');
       Status.SaveSettingsToIniFile(IniFile,'Status');

       {$IFDEF MUSIC_PLAYER}
         MPlayer.SaveSettingsToIniFile(IniFile);
         MPlayer.Display.SaveSettingsToIniFile(IniFile);
         MPlayerForm.SaveSettingsToIniFile(IniFile);
         Viewer1.SaveSettingsToIniFile(IniFile);
         Fractals.SaveSettingsToIniFile(IniFile);
         //Fireworks.SaveSettingsToIniFile(IniFile);
         Mandala.SaveSettingsToIniFile(IniFile);
       {$ENDIF}

       RotateAndFlipPopupMenu.SaveSettingsToIniFile(IniFile);
       SnapshotsForm.SaveSettingsToIniFile(IniFile);
//     EditForm.SaveSettingsToIniFile(IniFile);
       if Assigned(Solver   ) then Solver   .SaveSettingsToIniFile(INIFILE_SOLVER_SECTION   ,IniFile);
       if Assigned(Optimizer) then Optimizer.SaveSettingsToIniFile(INIFILE_OPTIMIZER_SECTION,IniFile);
       if Assigned(Generator) then Generator.SaveSettingsToIniFile(INIFILE_GENERATOR_SECTION,IniFile);
       OptionsForm.SaveSettingsToIniFile(IniFile);
       ToolsForm.SaveSettingsToIniFile(IniFile);
       GeneratorForm.SaveSettingsToIniFile(IniFile);
       DuplicatesForm.SaveSettingsToIniFile(IniFile);
       LevelSetForm.SaveSettingsToIniFile(IniFile,INTERNAL_CLIPBOARD_INIFILE_SECTION);
       Skins.SaveSettingsToIniFile(IniFile);
       MultiView.SaveSettingsToIniFile(IniFile,MULTI_VIEW_INI_FILE_SECTION_NAME);
       CaptureForm.SaveSettingsToIniFile(IniFile);
       HelpForm.SaveSettingsToIniFile(IniFile);
//     FileAssoc.SaveSettingsToIniFile(IniFile);
       DateTime:=Now;
       IniFile.WriteString     ('Last session','End ' + KEY_DATE,SysUtils.DateToStr(DateTime));
       IniFile.WriteString     ('Last session','End ' + KEY_TIME,SysUtils.TimeToStr(DateTime));
       IniFile.Free;
//     TestLogFile.Close;
     finally
       if (ApplicationMutex<>0) and
          (not ReleaseMutex(ApplicationMutex)) then begin
          // do nothing; it's better to exit normally;
          //raise Exception.Create(TEXT_PROCESS_SYNCHRONIZATION_ERROR);
          end;
     end;
     end;

  try
    ToolsForm.SolverTaskQueue   .Plugin:=nil; // the task queue uses 'Solver',    hence, remove the reference before 'ToolsForm' is destroyed
    ToolsForm.OptimizerTaskQueue.Plugin:=nil; // the task queue uses 'Optimizer', hence, remove the reference before 'ToolsForm' is destroyed
    ToolsForm.GeneratorTaskQueue.Plugin:=nil; // the task queue uses 'Generator', hence, remove the reference before 'ToolsForm' is destroyed
    Solver   .Free; Solver   :=nil;           // the solver     uses 'OpenForm',  hence, destroy the solver    before 'OpenForm' is destroyed
    Optimizer.Free; Optimizer:=nil;           // the optimizer  uses 'OpenForm',  hence, destroy the optimizer before 'OpenForm' is destroyed
    Generator.Free; Generator:=nil;           // the generator  uses 'OpenForm',  hence, destroy the generator before 'OpenForm' is destroyed

    ProcessLogFiles(lftClear); // clear log files;
  except on E:Exception do begin
            // ignore exceptions
            //Msg(E.Message,'',MB_OK);
            end;
  end;

  ShutDownApplication:=True;
  ScreenSaverEnabled:=True;

//Log.Writeln('Stop: '+DateTimeToStr(Now));
end;

procedure TMainForm.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
const CURSOR_COLOR:TColor=clYellow;
var i:Integer; Canvas:TCanvas;
begin
  Inc(ACol); Inc(ARow);
//if Sender=EditForm.DrawGrid1 then begin
//   Canvas:=EditForm.DrawGrid1.Canvas;
//   i:=EditForm.Board[ACol,ARow];
//   end
//else
     begin
     Canvas:=DrawGrid1.Canvas;
     i:=Game.Board[ACol,ARow];
     end;

  with Canvas do
    begin
      Brush.Color:=Game.Color[FloorColor];
      Pen.Color  :=Game.Color[FloorColor];
      case i and (WALL+PLAYER+BOX+GOAL) of
        WALL         : begin //if Sender=EditForm.DrawGrid1 then begin
                             //   FrameRect(Rect);
                             //   Inc(Rect.Left);  Inc(Rect.Top);
                             //   Dec(Rect.Right); Dec(Rect.Bottom);
                             //   end;
                             Brush.Color:=Game.Color[WallColor];
                             FillRect(Rect);
                       end;
        PLAYER       : begin if TrackState=tsTrack then
                                Brush.Color:=CURSOR_COLOR;
                             FrameRect(Rect);
                             Brush.Color:=Game.Color[PlayerColor];
                             Inc(Rect.Left);  Inc(Rect.Top);
                             Dec(Rect.Right); Dec(Rect.Bottom);
                             FillRect(Rect);
                       end;
        BOX          : begin if (TrackBox.X=ACol) and (TrackBox.Y=ARow) then
                                Brush.Color:=CURSOR_COLOR;
                             FrameRect(Rect);
                             Brush.Color:=Game.Color[BoxColor];
                             Inc(Rect.Left);  Inc(Rect.Top);
                             Dec(Rect.Right); Dec(Rect.Bottom);
                             FillRect(Rect);
                       end;
        GOAL         : begin FrameRect(Rect);
                             Brush.Color:=Game.Color[FloorColor];
                             Inc(Rect.Left);  Inc(Rect.Top);
                             Dec(Rect.Right); Dec(Rect.Bottom);
                             FillRect(Rect);
                             Brush.Color:=Game.Color[FloorColor];
                             Pen.Color:=Game.Color[GoalColor];
                             Ellipse(Rect.Left+1,Rect.Top+1,Rect.Right-1,Rect.Bottom-1);
                       end;
        PLAYER+
        GOAL         : begin if TrackState=tsTrack then
                                Brush.Color:=CURSOR_COLOR;
                             FrameRect(Rect);
                             Brush.Color:=Game.Color[PlayerColor];
                             Inc(Rect.Left);  Inc(Rect.Top);
                             Dec(Rect.Right); Dec(Rect.Bottom);
                             FillRect(Rect);
                             Brush.Color:=Game.Color[PlayerColor];
                             Pen.Color:=Game.Color[FloorColor];
                             Ellipse(Rect.Left+1,Rect.Top+1,Rect.Right-1,Rect.Bottom-1);
                       end;
        BOX+
        GOAL         : begin if (TrackBox.X=ACol) and (TrackBox.Y=ARow) then
                                Brush.Color:=clYellow;
                             FrameRect(Rect);
                             Brush.Color:=Game.Color[BoxOnGoalColor];
                             Inc(Rect.Left);  Inc(Rect.Top);
                             Dec(Rect.Right); Dec(Rect.Bottom);
                             FillRect(Rect);
                             Brush.Color:=Game.Color[BoxOnGoalColor];
                             Pen.Color:=Game.Color[FloorColor];
                             Ellipse(Rect.Left+1,Rect.Top+1,Rect.Right-1,Rect.Bottom-1);
                       end;
        else {FLOOR}   begin Brush.Color:=Game.Color[FloorColor];
                             FillRect(Rect);
                       end;
      end; // case
    end;
end;

procedure TMainForm.ShowBoard;
begin
  //Color:=Game.Color[FloorColor];
  if   Image1.Visible then GameViewer.ShowBoard(True)
  else if DrawGrid1.Visible then DrawGrid1.Invalidate;
end;

procedure TMainForm.ShowGame;
begin
  if      Image1.Visible then
          GameViewer.LoadGame(Game)
          //GameViewer.Show
  else if DrawGrid1.Visible then with DrawGrid1 do begin
          ShowTitle(Game.FileName);
          if (ColCount<>Game.BoardWidth)  or
             (RowCount<>Game.BoardHeight) or
             (Bevel1.Visible<>{Game.PlayerCanReachTheBorder and} (Bevel1.Width>10)) then begin
             ColCount:=Game.BoardWidth;
             RowCount:=Game.BoardHeight;

             Visible:=(ColCount>=MIN_BOARD_WIDTH) and
                      (RowCount>=MIN_BOARD_HEIGHT);
             FormResize(Self);
             end;
          ShowBoard;
          ShowStatus;
          end;
end;

procedure TMainForm.ShowMove(const FromPos,ToPos:TPoint; BoxNo,TimeMS,MoveCount,PushCount:Integer; Undo,Jump,LastMove:Boolean);
begin
  if      Image1.Visible then
          GameViewer.ShowMove(FromPos,ToPos,BoxNo,TimeMS,MoveCount,PushCount,Undo,Jump,LastMove)
  else if DrawGrid1.Visible then ShowGame;
end;

procedure TMainForm.ShowMoveInstantly(BoxNo:Integer; Undo,Jump,LastMove:Boolean);
begin
  if   Image1.Visible then
       GameViewer.ShowMoveInstantly(BoxNo,Undo,Jump,LastMove)
  else if DrawGrid1.Visible then ShowGame;
end;

procedure TMainForm.ShowStatus;

  procedure SolutionAnimation;
  var Col,Row:Integer;
  begin
    with Game do
      if (GameState=gsSolved) and
         (Screen.ActiveForm=Self) and
         (MPlayer<>nil) and (not MPlayer.Visible) then begin
         for Col:=1 to BoardWidth do
             for Row:=1 to BoardHeight do
                 if   (Board[Col,Row] and BOX)=0 then
                      Board[Col,Row]:=Board[Col,Row] and (not SQUARE_SET)
                 else Board[Col,Row]:=Board[Col,Row] or SQUARE_SET;
         if GameViewer<>nil then
            GameViewer.LegalMovesInfo.Mask:=SQUARE_SET;
         end;
  end;

begin
  if Game<>nil then begin // otherwise the program is closing
     if Game.FileName='' then begin
       Status.MoveCount:='';
       Status.PushCount:='';
       Status.Modified :='';
       if SnapshotsForm<>nil then SnapshotsForm.Hide;
       if Assigned(MultiView) and Assigned(MultiView.Selected) then with MultiView.Selected do with Panels[mvipCaption] do
          ShowPanel(mvipCaption,'',Enabled,Rect,Alignment);
       end
     else begin
       if   Modified then
            Status.Modified:=OKChangedText[True] // must come before moves and pushes; reason unknown
       else Status.Modified:='';
       Status.MoveCount:=IntToStr(Game.History.Count);
       Status.PushCount:=IntToStr(Game.History.PushCount);
       if Assigned(MultiView.Selected) then MultiView.Selected.ShowCaptionWithMetrics(True);
       end;

     if ((not Modified) and
         (Status<>nil) and
         (Status.Panels[spHint].Rect.Left> Status.Panels[spModified].Rect.Left) and
         ((Status.Panels[spHint].Rect.Left<=Status.Panels[spReplaySpeed].Rect.Right)  // false: 'replay' is in progress
          or
          (Status.Panels[spReplaySpeed].Rect.Right=0) // '0': 'replay speed' panel hasn't been created yet
         ) and
         (RectWidth(Status.Panels[spHint].Rect)>0) and
         (Game.FileName<>'')
        )
        or
        (Modified
         and
         (Status.Panels[spReplaySpeed].Rect.Left<>0) and
         (Status.Panels[spReplaySpeed].Rect.Left<=Status.Panels[spModified].Rect.Right) and
         (Game.FileName<>'')
        ) then begin
        CurrentImageClientWidth:=0;
        FormResize(Self);
        end;

     BtnOpen               .Enabled:=(GamePictures<>nil) and GamePictures.Initialized;
     BtnOpenPrior          .Enabled:=Game.LastValidFileName<>'';
     BtnOpenNext           .Enabled:=BtnOpenPrior.Enabled;
     BtnOpenClipboard      .Enabled:=Clipboard.HasFormat(CF_TEXT) and GamePictures.Initialized;
     BtnSave               .Enabled:=(Game.GameState<>gsNull) {and (Game.History[htUserGame].Count<>0) and (not IsANewLevel)};
     BtnSaveAs             .Enabled:= Game.GameState<>gsNull;

     BtnRotate             .Enabled:=Game.FileName<>'';
     BtnSolution           .Enabled:=Game.BestSolutionMoves<>nil;
     BtnSplitView          .Enabled:=(Game.FileName<>'')
                                     and
                                     (MultiView<>nil)
                                     and
                                     ( (MultiView.IsEmpty
                                       and
                                       (RectWidth (MultiView.ClippingRect)>((4*MAX_BOARD_WIDTH )+32)) // just a ballpark limit based on the assumption that each square on the board should be at least 2x2 pixels + some slack for borders and headers; a calculation of an exact limit is too complicated and too much work
                                       and
                                       (RectHeight(MultiView.ClippingRect)>((4*MAX_BOARD_HEIGHT)+32)) // just a ballpark limit based on the assumption that each square on the board should be at least 2x2 pixels + some slack for borders and headers; a calculation of an exact limit is too complicated and too much work
                                      )
                                      or
                                      (Assigned(MultiView.Selected)
                                       and
                                       MultiView.Selected.Panels[mvipSplit].Enabled
                                      )
                                     );

     BtnReset              .Enabled:=Game.History.Count>Game.ForcedInitialJumps;
     BtnUndo               .Enabled:=BtnReset.Enabled;
     BtnSnapshots          .Enabled:=Game.FileName<>'';
     BtnBookmarks          .Enabled:=False;
//   BtnBookmarks          .Enabled:=BtnUndo.Enabled and (not BtnSnapshots.Enabled) and
//                                   ((Game.GameState<>gsSolved) or (Game.BookmarkCount>0));
     BtnRedo               .Enabled:=Game.History.Count<Game.History.Top;
     BtnRedoAll            .Enabled:=BtnRedo.Enabled;
     BtnReverseMode        .Enabled:=Game.FileName<>'';
     BtnTools              .Enabled:=(Game<>nil) and (GamePictures<>nil) and GamePictures.Initialized;

     ShowMenu;
     with Status do begin
       EnableDisableButtons;
       if   (Game.GameState=gsSolved) and
            (Game.History.Count>0) and
            (LastGameAction<>gaRedo) and
            (LastGameAction<>gaUndo) and
            (Menu<>nil) and
            (Self.Menu<>nil) then begin
            if Game.LastSolutionWasTheFirstOne then
               if   Self.Menu.BtnPreviousLevelIndex=-1 then
                    Hint:=GameSolvedText+HintOpenAnotherLevelShortText
               else Hint:=GameSolvedText+HintOpenAnotherLevelLongText
            else with Game do
               if   BestSolutionMoves<>nil then
                    if   BestSolutionPushes=nil then
                         Hint:=GameSolvedText+Format(BestSolutionText__ ,[BestSolutionMoves.MoveCount,BestSolutionMoves.PushCount])
                    else Hint:=GameSolvedText+Format(BestSolutionsText__,[BestSolutionMoves.MoveCount,BestSolutionMoves.PushCount,BestSolutionPushes.MoveCount,BestSolutionPushes.PushCount])
               else if   Self.Menu.BtnPreviousLevelIndex=-1 then
                         Hint:=GameSolvedText+HintOpenAnotherLevelShortText
                    else Hint:=GameSolvedText+HintOpenAnotherLevelLongText;
            end
       else if   Game.ReverseMode and
                 (Game.BoxOnTargetCount=Game.BoxCount) and
                 (Game.GameState=gsPlay) and
                 (Game.History.PushCount>0) then
                 Hint:=NotAReverseModeSolutionBecauseThePlayerIsNotInTheRightZoneText
            else Hint:='';
       end;

     if (Game.GameState=gsSolved) and (GameViewer<>nil) then
        with GameViewer.LegalMovesInfo do
          if (Mask=0) and (GameViewer.SolutionsInfo.PlayerAnimationEnabled or GameViewer.SolutionsInfo.BoxAnimationEnabled) then
             SolutionAnimation;
     end;
end;

procedure TMainForm.BtnResetClick(Sender: TObject);
begin
  if MPlayer.Visible then MPlayer.Hide;
  if Game.IsIdleAndStopReplayingAndBrowsing then begin
     Game.StopTimer;
     InitGame(True,False,False,True,False,True,True,0);
     end;
end;

procedure TMainForm.BtnOpenClick(Sender: TObject);
begin
  if Game.IsIdleAndStopReplayingAndBrowsing then
     if   MPlayer.Visible and MPlayer.Buttons[Ord(btOpen)].Visible then begin
          MPlayer.ItemIndex:=Ord(btOpen);
          MPlayer.Click(mbLeft);
          end
     else Open(Self,osNone);
end;

function  TMainForm.Open(Sender: TObject; OpenSubTask:TOpenSubTask):Boolean;
var oSnapshotsFormVisible:Boolean; Caption,FileName:String;
    oOnMessage:TMessageEvent;
begin
  Result:=False; ClearTrackBox(True);
  if Game.IsIdleAndStopReplayingAndBrowsing
     and
     ((Sender=ToolsForm.GeneratorMenuItemOpen)
      or
      CloseLevel(Sender)
     )
     and
     GamePictures.Initialized then
     try
       FileName:='';
       if (Sender=ToolsForm.GeneratorMenuItemOpen) and Generator.HasSokoFile then begin
          FileName:=Generator.SokoFile.Name;
          if IsANewFileName(FileName) or (not FileExists(FileName)) then FileName:='';
          end;
       if FileName='' then FileName:=Game.FileName;
       if IsANewFileName(FileName) then FileName:=Game.LastValidFileName;
       if Assigned(MultiView) and Assigned(MultiView.Selected) then MultiView.Selected.MakeSnapshot;
       oSnapshotsFormVisible:=SnapshotsForm.Visible;
       SnapshotsForm.Hide;
       oOnMessage                   :=Application.OnMessage;
       Application.OnMessage        :=nil;
       if Deadlocks<>nil then Deadlocks.Suspend;
       try     if Sender=ToolsForm.GeneratorMenuItemOpen then begin
                  OpenSubTask:=osGeneratorCandidateSet;
                  Caption:=OpenLevelOrGeneratorCandidateSetText;
                  end
               else if Sender = ToolsForm.PluginMenuItemImportTaskQueue then begin
                       OpenSubTask:=osPluginTaskQueue;
                       Caption := Trim( ToolsForm.PageControl1.ActivePage.Caption ) + SUB_TITLE_SEPARATOR + ImportTaskQueueText;
                       end
                    else begin
                       if OpenSubTask<>osLevelEditor then
                          OpenSubTask:=osNone;
                       Caption:=TEXT_OPEN_LEVEL;
                       end;
               if OpenForm.InitTask(otGame,OpenSubTask,Caption,Caption,FileName,
                                    OpenDialog1.Filter,'',Rect(0,0,0,0),clBlack,True,False,Rect(0,0,0,0)) then begin
                  if (Sender=ToolsForm) or (Sender=ToolsForm.GeneratorMenuItemOpen) then with OpenForm do begin
                     // kludge: drop-down-width isn't set correctly by 'OpenForm' itself if this is the first call; reason unknown
                     with LevelsFolderHistoryComboBox            do Perform(CB_SETDROPPEDWIDTH,Tag,0);
                     with LevelsFileHistoryComboBox              do Perform(CB_SETDROPPEDWIDTH,Tag,0);
                     with AnythingButLevelsFolderHistoryComboBox do Perform(CB_SETDROPPEDWIDTH,Tag,0);
                     with AnythingButLevelsFileHistoryComboBox   do Perform(CB_SETDROPPEDWIDTH,Tag,0);
                     end;
                  Result                 :=OpenForm.ShowModal=mrOk;
                  OpenDialog1.FilterIndex:=Succ(OpenForm.FilterComboBox1.ItemIndex);
                  if //(not Result)
                     //and
                     ((not Game.Snapshots.IsEmpty)
                      or
                      oSnapshotsFormVisible
                     )
                     and
//                   (not (Screen.ActiveForm=EditForm)) and
                     (not (Screen.ActiveForm=ToolsForm)) then SnapshotsForm.Show;
                  end;
       finally Application.OnHint   :=ShowHint;
               Application.OnMessage:=oOnMessage;
               IgnoreKeyUp          :=True;
               IgnoreMouseUp        :=True;
               IgnoreShiftKey       :=False;
               if Deadlocks<>nil then Deadlocks.Resume;
               MakeSolutionButtonHint;
       end;
       if Self.Visible then
          if      Image1   .Visible then Self     .SetFocus
          else if DrawGrid1.Visible then DrawGrid1.SetFocus;
     except on E:Exception do begin
            if Sender=ToolsForm.GeneratorMenuItemOpen then Generator.Clear;
            Result:=Error(E.Message,Application.Title);
            end;
     end;

  ShowStatus;
//if (EditForm <>nil) and EditForm .Visible then EditForm.ShowStatus;
  if (ToolsForm<>nil) and ToolsForm.Visible then ToolsForm.ShowStatus;
  Menu.SkipMouseOverSoundForItemIndex:=-1;
end;

procedure TMainForm.BtnSaveClick(Sender: TObject);
begin
  if Game.IsIdleAndStopReplayingAndBrowsing then
     if   IsANewFileName(Game.FileName) then
          Save(MainForm.BtnSaveAs)
     else Save(nil);
end;

procedure TMainForm.BtnSaveAsClick(Sender: TObject);
begin
  if Game.IsIdleAndStopReplayingAndBrowsing then Save(BtnSaveAs);
end;

function  TMainForm.Save(Sender: TObject):Boolean;
var Dir,OrgName,s,s1:String; IsNew:Boolean; OrgSender:TObject;
begin
  Result:=GamePictures.Initialized;
  Game.StopTimer; MPlayer.Hide; ClearTrackBox(True); IsNew:=True; OrgName:='';
  CtrlEnterKeySet:=[]; LastKey:=0; OrgSender:=Sender;
  if   Result then begin
       if   Sender=ToolsForm then
            SaveDialog1.FileName:=ToolsForm.Editor.FileName
       else SaveDialog1.FileName:=Game.FileName;
       if   Sender=BtnSaveAs then begin
            s:=SaveDialog1.FileName;
            if IsAnIniFileSectionFileName(s) then begin
               s1:=StrToFileName(ExtractSectionName(s));
               if s1='' then s1:=TEXT_LEVEL;
               if   Game.MakeNewFileName(ExtractFilePath(ExtractIniFileName(s)),s1,s) then
                    begin Sender:=Self;
                          SaveDialog1.FileName:=s;
                    end // activate save-dialog, and prompt for overwriting existing file
               else begin Result:=False; Error(DiskFullText,Application.Title);
                    end;
               end
            else Sender:=Self;
            end;

       IsNew:=IsANewFileName(SaveDialog1.FileName);
       end;

  if   Result and
       ((not IsAnIniFileSectionFileName(SaveDialog1.FileName))
        or
        IsNew
       ) then with SaveDialog1 do
       begin if IsNew then
                begin
                  Sender:=Self;
                  s1:='';
                  if IsAnIniFileSectionFileName(FileName) then begin
                     Dir:=ExtractFilePath(ExtractIniFileName(FileName));
                     s1:=StrToFileName(ExtractSectionName(FileName));
                     end
                  else begin
                     s:=StrWithoutTrailingPathDelimiter(ExtractFilePath(FileName));
                     if   (s<>'') and DirectoryExists(s) then
                          Dir:=s
                     else Dir:=OpenDialog1.InitialDir;
                     end;

                  if Dir  =''  then Dir:=ApplicationDataPath;
                  if (Dir<>'') then Dir:=StrWithTrailingPathDelimiter(Dir);
                  if s1   =''  then s1:=TEXT_LEVEL;

                  s:=Dir+s1+SOKOBAN_FILE_NAME_EXT;
                  if   (s1<>TEXT_LEVEL) and     // '<>': a level name extracted from an inifile section-name
                       (not FileExists(s)) then
                  else s:='';

                  if   (s<>'') or
                       Game.MakeNewFileName(Dir,s1,s) then
                       begin FileName:=s;
                             OrgName:='';
                             InitialDir:=ExtractFilePath(FileName);
                             FileName  :=ExtractFileNameWithOutExt(FileName);
                       end
                  else begin Msg(DiskFullText,'',MB_OK+MB_ICONERROR);
                             Result:=False;
                       end;

                  s:=ExtractFileExt(s);
                end
             else
                begin OrgName:=FileName;
                      InitialDir:=ExtractFilePath(FileName);
                      FileName  :=StrToFileName(ExtractFileName(FileName));
                      if ExtractFileNameWithoutPathAndExtension(FileName)='' then
                         FileName:=TEXT_LEVEL+SOKOBAN_FILE_NAME_EXT;
                      s:=ExtractFileExt(FileName);
                end;

             if (s<>'') and (Sender=Self) then begin // 's' = file extension
                if        StrEqual(s,SOKOBAN_FILE_NAME_EXT) or
                          StrEqual(s,XSB_FILE_NAME_EXT ) then FilterIndex:=1
                else if   StrEqual(s,TEXT_FILE_EXT) then FilterIndex:=2
                     else FilterIndex:=3;
                if (FilterIndex<>3) and (not IsNew) then FileName:=ChangeFileExt(FileName,'');
                end;

             if Result and
                ((Sender<>Self) or Execute) then
                begin
                      if (Sender<>Self) and
                         (ExtractFilePath(FileName)='') then
                         FileName:=StrWithTrailingPathDelimiter(InitialDir)+FileName;

                      if ExtractFileExt(FileName)='' then
                         if   FilterIndex=1 then
                              if   StrEqual(ChangeFileExt(FileName,XSB_FILE_NAME_EXT),OrgName) then
                                   FileName:=ChangeFileExt(FileName,XSB_FILE_NAME_EXT)
                              else FileName:=ChangeFileExt(FileName,SOKOBAN_FILE_NAME_EXT)
                         else if   FilterIndex=2 then
                                   FileName:=ChangeFileExt(FileName,TEXT_FILE_EXT)
                              else FileName:=ChangeFileExt(FileName,SOKOBAN_FILE_NAME_EXT);

                      if DirectoryExists(FileName) then begin
                         Msg(Format(DirectoryExistsText__,[AbbreviatedFilePath(FileName,MyDocumentsFolder)]),Title,MB_OK+MB_ICONINFORMATION);
                         Result:=False;
                         end
                      else
                         if (Sender=Self)
                            and
                            (AnsiCompareText(OrgName,FileName)<>0)
                            and
                            FileExists(FileName)
                            and
                            (Msg(Format(FileExistsText__,[AbbreviatedFilePath(FileName,MyDocumentsFolder)])+NL+NL+OverwriteItText,
                                 Title,
                                 MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)
                             <> IDYES) then
                             Result:=False              {cancel save operation}
                         else
                            if True then //FileNameOk(FileName) then
                               begin
                                 if   (OrgSender=ToolsForm) and IsNew then
                                      s:=ToolsForm.Editor.FileName
                                 else s:=Game.FileName;
                                 SaveSnapshotToLogFile  (s,FileName,''); // this doesn't really save a snapshot to the log file but records a "rename" operation
                                 Solver   .RenameLevels (s,FileName);
                                 Solver   .RenamePlugins(s,FileName);
                                 Optimizer.RenameLevels (s,FileName);
                                 Optimizer.RenamePlugins(s,FileName);
                                 Game.FileName:=FileName;
                                 Game.DisplayName:=ExtractFileNameWithoutPathAndExtension(FileName);
                                 ShowTitle(FileName);
                                 InitialDir:=ExtractFilePath(FileName);
                                 OpenDialog1.InitialDir:=InitialDir;
                                 OpenDialog1.FilterIndex:=FilterIndex;
                                 if ( not StrEqual( OrgName, Game.FileName ) )
                                    and
                                    Assigned( Game.SokoFile )
                                    and
                                    StrEqual( Game.FileName, Game.SokoFile.Name )
                                    then begin
                                    // the level is being save as a new file
                                    // which happens to have the same file name
                                    // as the most recently loaded level file;
                                    // close the most recently loaded level file
                                    // so 'Game.SaveGame()' further down merges
                                    // the current level into the existing file
                                    // instead of overwriting it;
                                    Result := Game.SokoFile.Close;
                                    end;
                               end
                            else
                               begin
                                 Result:=False;
                                 //ShowMessage(Format(FileNameInvalidText,[FileName]));
                               end;
                end
             else Result:=False;
       end;

  if   Result then begin
       if   Assigned(MultiView) and Assigned(MultiView.Selected) then MultiView.Selected.MakeSnapshot;
//     if   (EditForm <>nil) and EditForm .Modified then EditForm .SaveLevel;
       if   (ToolsForm<>nil) and ToolsForm.Modified then Result:=ToolsForm.SaveLevel(nil); // '.SaveLevel(nil)': move the edited level back to 'MainForm.Game'
       if   (Solver   <>nil) then Solver   .ImportGames(True); // save any pending solutions/snapshots for this level
       if   (Optimizer<>nil) then Optimizer.ImportGames(True); // save any pending solutions/snapshots for this level
       if   Result and Game.SaveToFile(SaveDialog1.FileName,True) then begin
            Modified:=False;
            if ToolsForm<>nil then ToolsForm.PluginLevelInfo.NewGamesCount:=0;
            if (not IsNew) and (OrgSender=BtnSaveAs) and Assigned(OpenForm) and
               (Length(Game.FileName)>3) then begin
               AddItemOrMoveItemToTopOfComboBox(OpenForm.LevelsFolderHistoryComboBox,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(ExtractFilePath(Game.FileName)),MainForm.MyDocumentsFolder),True);
               AddItemOrMoveItemToTopOfComboBox(OpenForm.LevelsFileHistoryComboBox  ,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(                                                Game.FileName  ,MainForm.MyDocumentsFolder),True);
               end;
            end
       else Result:=False;
       end;

  ShowStatus;
//if (EditForm <>nil) and EditForm .Visible then EditForm .ShowStatus;
  if (ToolsForm<>nil) and ToolsForm.Visible then ToolsForm.ShowStatus;
  if SnapshotsForm<>nil then SnapshotsForm.Grid.Cells[1,0]:=VisualFileName(Game.FileName);
  Menu.SkipMouseOverSoundForItemIndex:=-1;
end;

procedure TMainForm.BtnUndoClick(Sender: TObject);
begin
  if Game.IsIdleAndStopReplayingAndBrowsing then begin
     if MPlayer.Visible then MPlayer.Hide;
     ClearTrackBox(True); LastGameAction:=gaUndo; LastReplayAction:=gaUndo;
     if Sender<>nil then
        Menu.SkipMouseOverSoundForItemIndex:=Menu.BtnUndoIndex; // kludge: don't let the mouse-up event trigger a sound; it would stop the move-sound abruptly
     if   ceCtrl in CtrlEnterKeySet then
          Game.Undo((not UndoRedoCombinedMoves) and (Sender<>nil))
     else Game.Undo(     UndoRedoCombinedMoves  and (Sender<>nil));
     if   (Game.History.Count<=Game.ForcedInitialJumps) then Game.StopTimer
     else if Game.StartTimeMS=0 then Game.StartTimer;
     TrackState:=tsWait;
     if Sender<>BtnUndo then UpdateCursor;
     if Menu.ItemIndex=HOTSPOT_STATUSBAR_HINT_INDEX then with Menu do
        ShowButton(ItemIndex,bsFocusedEnabled,True);
     end;
end;

procedure TMainForm.BtnRedoClick(Sender: TObject);
begin
  if Game.IsIdleAndStopReplayingAndBrowsing then begin
     if MPlayer.Visible then MPlayer.Hide;
     ClearTrackBox(True); LastGameAction:=gaRedo; LastReplayAction:=gaRedo;
     if Sender<>nil then
        Menu.SkipMouseOverSoundForItemIndex:=Menu.BtnRedoIndex; // kludge: don't let the mouse-up event trigger a sound; it would stop the move-sound abruptly
     if   ceCtrl in CtrlEnterKeySet then
          Game.Redo(not UndoRedoCombinedMoves)
     else Game.Redo(UndoRedoCombinedMoves);
     TrackState:=tsWait;
     if Sender<>BtnRedo then UpdateCursor;
     if Menu.ItemIndex=HOTSPOT_STATUSBAR_HINT_INDEX then with Menu do
        ShowButton(ItemIndex,bsFocusedEnabled,True);
     end;
end;

procedure TMainForm.BtnRedoAllClick(Sender: TObject);
var oMoveAnimationEnabled:Boolean; oReplayAction:TGameAction;
begin
  oMoveAnimationEnabled:=Game.MoveAnimationEnabled;
  if Game.IsIdleAndStopReplayingAndBrowsing and
     (Screen.ActiveForm=Self) then
     try     Application.ShowHint:=False; // otherwise flickering occurs when Delphi/Windows finds it necessary to update 'Image1' after showing/hiding the ToolTips
             if MPlayer.Visible then MPlayer.Hide;
             ClearTrackBox(True);
             ReplayStarterButton:=Sender;
             Game.MoveAnimationEnabled:=Sender<>BtnRedoAll;
             if      Sender=BtnStatusMenuReplay then
                     Status.Menu.SkipMouseOverSoundForItemIndex:=Status.Menu.BtnStopIndex // kludge: don't let the mouse-up event trigger a sound; it would stop the move-sound abruptly
             else if Sender<>nil then
                     Menu.SkipMouseOverSoundForItemIndex:=Menu.BtnRedoAllIndex;           // kludge: don't let the mouse-up event trigger a sound; it would stop the move-sound abruptly
             if   (Sender=BtnStatusMenuReplay) and
                  (LastGameAction=gaNull) then
                  LastGameAction:=LastReplayAction
             else if   Sender=BtnUndo then
                       LastGameAction:=gaUndo
                  else LastGameAction:=gaRedo;
             LastReplayAction:=LastGameAction;
             if      Game.History.Count<=Game.ForcedInitialJumps then LastReplayAction:=gaRedo
             else if Game.History.Count =Game.History.Top        then LastReplayAction:=gaUndo;
                                   
             repeat oReplayAction:=LastReplayAction;

                    Game.ReplayInMainWindow(LastReplayAction=gaRedo);

             until  oReplayAction=LastReplayAction;

     finally Game.MoveAnimationEnabled:=oMoveAnimationEnabled;
             LastGameAction:=LastReplayAction;
             TrackState:=tsWait;
             Application.ShowHint:=True;
             IgnoreKeyUp:=True;
             IgnoreMouseUp:=True;
             IgnoreShiftKey:=False; 
             if   Sender=BtnRedoAll then
                  if (Game.StartTimeMS=0) and (Game.GameState<>gsSolved) then Game.StartTimer
                  else
             else UpdateCursor;
             if Menu.ItemIndex=HOTSPOT_STATUSBAR_HINT_INDEX then with Menu do
                ShowButton(ItemIndex,bsFocusedEnabled,True);
     end;
end;

procedure TMainForm.BtnSolutionClick(Sender: TObject);
var oCount:Integer; Solution:TSnapshot;
begin
  oCount:=0;
  if (Sender=BtnSolution) and IsKeyPressed(VK_SHIFT) and IsKeyPressed(VK_CONTROL) then
     Sender:=BtnOpenNext;
  repeat
    // Select type based on 'Sender': 'BtnSolution': default, 'nil': pushes, else moves
    if      Sender=MenuItemSolutionMoves  then Solution:=Game.BestSolutionMoves
    else if Sender=MenuItemSolutionPushes then Solution:=Game.BestSolutionPushes
    else if   (Game.BestSolutionPushes<>nil) and
              ((((Sender=BtnSolution) or (Sender=MenuItemSolution) or (Sender=BtnOpenNext))
                and
                (not ShowSolutionMoves)
               )
               or
               (Sender=nil)
              ) then
              Solution:=Game.BestSolutionPushes
         else Solution:=Game.BestSolutionMoves;

      if Game.IsIdleAndStopReplayingAndBrowsing and (Solution<>nil) then
         try
           Game.IsBusy:=True; Game.StopTimer;
           if MPlayer.Visible then MPlayer.Hide;
           ClearTrackBox(True);

           if Game.ReverseMode then begin
              BtnReverseModeClick(Self); oCount:=MAX_MOVES+1;
              end
           else if oCount<MAX_MOVES+2 then // 'True': first time through the loop
                   oCount:=Game.History.Count;

           Game.History.Top:=Solution.MoveTop;
           System.Move(Solution.Moves^,Game.History.Moves[Low(Game.History.Moves)],Solution.ByteSize(Solution.MoveTop));
         InitGame(True,False,False,False,False,True,True,0); Game.IsBusy:=True;
         Application.ProcessMessages; // to update the screen; hopefully, no important events happens before the following 'Replay' is launched
         if oCount>Game.ForcedInitialJumps then SleepEx(PauseBeforeReplayMS,False);
       finally
         Game.IsBusy:=False; LastGameAction:=gaRedo; BtnRedoAllClick(BtnSolution);

         if (Game.GameState=gsSolved) and (Sender=BtnOpenNext) and (not ShutDownApplication) and
            Game.IsIdleAndStopReplayingAndBrowsing and Assigned(Status) then begin
            Status.Hint:='';
            Game.IsBusy:=True;
            try     Application.ProcessMessages; // update status
            finally Game.IsBusy:=False;
            end;
            SleepEx(PauseBeforeReplayMS,False );
            if   OpenPriorOrNext(False,False,True,0) then
                 oCount:=MAX_MOVES+2 // pause again before replaying the solution for the newly loaded level
            else Sender:=nil; // exit loop
            end
         else Sender:=nil; // exit loop
       end
    else Sender:=nil; // exit loop
  until Sender<>BtnOpenNext;
end;

procedure TMainForm.BtnReverseModeClick(Sender: TObject);
begin
  if Game<>nil then begin
     Game.ToggleReverseMode;
     InitGame(False,False,False,False,False,True,True,0);
     end;
end;

procedure TMainForm.BtnHelpClick(Sender: TObject);
var oMPlayerVisible:Boolean; oOnMessage:TMessageEvent;
begin
  if Game.IsIdleAndStopReplayingAndBrowsing then begin
     Game.StopTimer;
     oMPlayerVisible:=MPlayer.Visible;
     MPlayer.Hide;
     oOnMessage:=Application.OnMessage;
     try     Application.OnMessage:=nil;
             ClearTrackBox(True);
             if HelpForm<>nil then with HelpForm do begin
                //if not Visible then Show
                //else if WindowState=wsMinimized then WindowState:=wsNormal
                //     else begin BringToFront; SetFocus; end;
                //HelpForm.RichEdit1.SetFocus;
                if   Visible then begin
                     if WindowState=wsMinimized then WindowState:=wsNormal;
                     BringToFront; SetFocus;
                     end
                else ShowModal;
                end;
     finally Application.OnMessage:=oOnMessage;
     end;
     if (WindowState<>wsMinimized) and
        (Screen.ActiveForm=Self) then
        if   oMPlayerVisible then MPlayer.Show
        else ShowStatus;
     Menu.SkipMouseOverSoundForItemIndex:=-1;
     end;
end;

procedure TMainForm.BtnExitClick(Sender: TObject);
var Result:Boolean;
begin
  Result:=False;
  try
          if Game.IsIdleAndStopReplayingAndBrowsing or ShutDownApplication then begin
             ClearTrackBox(True);
             if CloseLevel(BtnSave) then begin
                if Game<>nil then Game.Clear;
                Close; Result:=True;
                end;
             end;
  finally if (not Result) and ShutDownApplication then begin // 'ShutDownApplication': must close in all circumstances
             if Game<>nil then begin
                while Game.IsIdleAndStopReplayingAndBrowsing do begin end;
                Game.Clear;
                end;
             Modified:=False;
             if ToolsForm<>nil then ToolsForm.Modified:=False;
             Close;
             end;
  end;
end;

procedure TMainForm.ShowSnapshots(HidePopUpMenu,KeepSnapshotFormFocus:Boolean; SnapshotItemIndex:Integer);
begin
  with SnapshotsForm do begin
    if Self.Game.Snapshots.Count+Self.Game.BestSolutionsCount=0 then begin
       if (ItemCount<>0)
          or
          (Assigned(GameViewer)
           and
           Assigned(GameViewer.Pictures[ptScreenBackground])
           and
           (GameViewer.Pictures[ptScreenBackground].View=ivFloorTile)) then
          Clear;
       end
    else begin
       if Visible or (Self.Game.Snapshots.Count+Self.Game.BestSolutionsCount<>0) then begin
          Self.Game.IsBusy:=False;
          if      HidePopupMenu then FormActivate(Self) // 'Self.Game.IsBusy' must be false before calling 'FormActivate'
          else    FormActivate(nil);
          try     Grid.Row:=Max(Grid.FixedRows,Min(SnapshotItemIndex,Pred(Grid.RowCount)));
                  ShowSnapshot(Grid.Row);
                  if   Grid.Visible then begin
                       if not PanelGrid.Visible then PanelGrid.Show;
                       ActiveControl:=Grid;
                       end;
          finally if   KeepSnapshotFormFocus and
                       (Screen.ActiveForm=SnapshotsForm) and
                       (not ShutdownApplication) then
                  else FormDeactivate(Self);
          end;
          end;
       if (not Visible) and
          (Self.Game.Snapshots.Count<>0) and
          Self.Initialized and
//        ((EditForm =nil) or (not EditForm .Visible)) and
          ((ToolsForm=nil) or (not ToolsForm.Visible)) then begin
          Show;
          if Self.Visible then begin
             Self.SetFocus;
             Self.BringToFront;
             end;
          end;
       end;
    end;
end;

function  TMainForm.InitGame(ResetLevel,LoadSnapshots,InvalidateGameViewer,PlayResetSound,CalculateDeadlocks,HidePopUpMenu,ShowGame:Boolean; SnapshotItemIndex:Integer):Boolean;
var oCount:Integer;
begin
  fTrackState:=tsWait; MouseOverPlayerStartTime:=MOUSE_TRACKING_NOT_STARTED;
  TrackBox.X:=0; TrackBox.Y:=0;
  LastGameAction:=gaNull; LastReplayAction:=gaRedo; LastKey:=0;
  IsANewLevel:=IsANewFileName(Game.FileName); oCount:=Game.History.Count;
  Game.SetReverseMode(Game.ReverseMode); // ensure that target-masks are set correctly
  Game.ShowGame:=Self.ShowGame;
  if   InvalidateGameViewer then begin
       GameViewer.Modified:=True; // ensure that the screen is updated
       if (OpenForm<>nil) and (OpenForm.GameViewer<>nil) then
          OpenForm.GameViewer.Modified:=True;
       end;
  if   ResetLevel
       or
       ((not Game.SolutionsRequireAtLeastOnePush)  and
        (Game.StartBoxOnTargetCount=Game.BoxCount) and
        (Game.History.Count>0)
       ) then
       Game.Reset(True)
  else Game.CalculateScoreAndState; {game state may vary depending on the 'Settings' option "Control | Solutions | Solutions require at least one push'}
  with GameViewer.LegalMovesInfo do begin
    ItemIndex:=-1; CursorPos.x:=0; CursorPos.y:=0;
    end;

  Result:=(not ShowGame) or GameViewer.LoadGame(Game);

  if   (PlayResetSound) and
       (Sound.Enabled) and (Sound.Player[Sound_.stRestartGame]<>nil) and
       (Game.FileName<>'') and
       (oCount>Game.ForcedInitialJumps) then begin
       Sound.Play(Sound_.stRestartGame); // first play the sound after the gameviewer has loaded the game; otherwise the time interval between hearing the sound and seeing the new board can be noticeable
       Menu.SkipMouseOverSoundForItemIndex:=Menu.BtnResetIndex;
       end;

  if   LoadSnapshots then ShowSnapshots(HidePopUpMenu,False,SnapshotItemIndex);

  if   (Game.FileName<>'') and Game.DeadlockDetection.Enabled then
       if   CalculateDeadlocks then
            Deadlocks.LoadGame(True,True)
       else Deadlocks.LoadBoard
  else Deadlocks.Clear;

  MakeDynamicHints(nil);
  ShowStatus;
  UpdateCursor;
end;

procedure TMainForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize:=(not IsResizing)
          and
          ((Game=nil) or
           Game.IsIdleAndStopReplayingAndBrowsing or
           True // it is all right to resize the form, even during move animation
          );
end;

procedure TMainForm.FormResize(Sender: TObject);
var i,j,k:Integer;

  procedure DoResize;
  var i,j,ExtraColRow:Integer; oMPlayerVisible:Boolean;
  begin
    MultiView.DisappearedItemsCount:=0;
    try
      IsResizing:=True;
      Screen.Cursor:=crHourGlass;
      oMPlayerVisible:=MPlayer.Visible; MPlayer.Hide;

      if (RotateAndFlipPopupMenu<>nil) and RotateAndFlipPopupMenu.Visible then
         RotateAndFlipPopupMenu.Hide;
      if (MultiViewPopupMenu<>nil) and MultiViewPopupMenu.Visible then
         MultiViewPopupMenu.Hide;

      if   Game.PlayerCanReachTheBorder or Image1.Visible then ExtraColRow:=0
      else ExtraColRow:=1;

      if Menu<>nil then Menu.ItemIndex:=-1;
      if Status<>nil then begin
         Status.TopMargin:=0;
         if Status.Menu<>nil then Status.Menu.ItemIndex:=-1;
         end;

      if Image1.Visible then begin
         Image1.SetBounds(0,0,ClientWidth,ClientHeight);
         if Image1.Picture.BitMap.Width <>Image1.ClientWidth then
            Image1.Picture.BitMap.Width :=Image1.ClientWidth;
         if Image1.Picture.BitMap.Height<>Image1.ClientHeight then
            Image1.Picture.BitMap.Height:=Image1.ClientHeight;

         if (Image1.ClientWidth <>CurrentImageClientWidth) or
            (Image1.ClientHeight<>CurrentImageClientHeight) or
            (not GamePictures.Pictures[ptScreenBackground].Visible) then begin
            CurrentImageClientWidth :=Image1.ClientWidth;
            CurrentImageClientHeight:=Image1.ClientHeight;

            with GamePictures.Pictures[ptScreenBackground] do begin
              if BitMapResize(BitMap,Image1.ClientWidth,Image1.ClientHeight) and
                 Visible then
                 SetView(View,Image1.ClientWidth,Image1.ClientHeight,Color);
              if Visible and (BitMap<>nil) then
                 Draw(0,0,Image1.Picture.BitMap.Canvas)
              else with Image1.Picture.BitMap.Canvas do begin // no screen background picture
                 Brush.Color:=GamePictures.Pictures[ptScreenBackground].Color;
                 FillRect(Rect(0,0,Image1.ClientWidth,Image1.ClientHeight)); // clear rectangle
                 end;
              end;

            if Menu<>nil then begin
               Menu.MakeButtons;
               Menu.Invalidate;
               end;
            if Status<>nil then Status.Invalidate;
            end;

         if   (Status<>nil) and Status.Visible then
              i:=Status.RectForm.Top-Status.TopMargin
         else i:=Image1.ClientHeight;
         MultiView.ClippingRect:=Rect(GraphicMenuWidth,0,Image1.ClientWidth,i);
         GameViewer.SetWindow(Image1.Picture.BitMap.Canvas,MultiView.ClippingRect);

         if Game.FileName<>'' then GameViewer.LoadGame(Game);

         Game.SessionSmoothMoveAnimationEnabled:=Game.SmoothMoveAnimationEnabled;
         end
      else
         MultiView.ClippingRect:=Rect(GraphicMenuWidth,0,Image1.Picture.BitMap.Width,Image1.Picture.BitMap.Height);

      if DrawGrid1.Visible and (not Image1.Visible) then with DrawGrid1 do begin
         Hide;
         if (ColCount-ExtraColRow>0) and (RowCount-ExtraColRow>0) then begin
            DefaultColWidth :=Min((Self.ClientWidth-MAX_BOARD_WIDTH)
                                     div (ColCount-ExtraColRow),
                                   (Self.ClientHeight-PanelMenu1.Top-PanelMenu1.Height-StatusBar1.Height-MAX_BOARD_HEIGHT)
                                     div (RowCount-ExtraColRow));
            DefaultRowHeight:=DefaultColWidth;

            if ExtraColRow<>0 then begin
               ColWidths[0]:=DefaultColWidth div 2;
               ColWidths[ColCount-1]:=ColWidths[0];
               RowHeights[0]:=DefaultRowHeight div 2;
               RowHeights[RowCount-1]:=RowHeights[0];
               end;

            j:=0; for i:=0 to Pred(ColCount) do Inc(j,ColWidths[i]);
            Width:=j;
            j:=0; for i:=0 to Pred(RowCount) do Inc(j,RowHeights[i]);
            Height:=j;
            end;

         Left:=(Self.ClientWidth-Width) div 2;
         Top :=PanelMenu1.Top+PanelMenu1.Height+
                 ((Self.ClientHeight-Height-PanelMenu1.Top-PanelMenu1.Height-StatusBar1.Height) div 2);
         Show;

         with Bevel1 do begin
           Width  :=DrawGrid1.Width +8;
           Height :=DrawGrid1.Height+8;
           Left   :=DrawGrid1.Left  -4;
           Top    :=DrawGrid1.Top   -4;
           Visible:={Game.PlayerCanReachTheBorder and} (Width>10);
           end;
         end;

      if (not MultiView.IsEmpty) and Image1.Visible and (Game.FileName<>'') then // clear the background before the multiple views are shown on the screen
         MultiView.ShowBackground(MultiView.ClippingRect);

      MultiView.OnResize(nil);

      if Assigned(SnapshotsForm) and SnapshotsForm.Visible then with SnapshotsForm do
         EnableDisableButtons(True,Snapshots[Grid.Row]);

      if oMPlayerVisible and (WindowState<>wsMinimized) then MPlayer.Show;
    finally
      Screen.Cursor:=DEFAULT_CURSOR;
      IsResizing:=False;
      if MultiView.ReportDisappearedItems(MultiView.DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
         MultiView.DoReportDisappearedItems:=False;
    end;
  end;

begin // FormResize;
  if Initialized and (not IsResizing) then
     try
       IsResizing:=True;
       Inc(FormResizeCount);
       FormResizeLevel:=0;

       MultiView.HideDragRect;
       if (not MultiView.IsEmpty) and
          (GameViewer<>nil) and
          ((GameViewer.LegalMovesInfo.Mask<>0)
           or
           GameViewer.LegalMovesInfo.BoxCursor.Enabled
           or
           GameViewer.LegalMovesInfo.PlayerCursor.Enabled
          ) then
          GameViewer.HideLegalMoves;

       i:=Max(BtnOpenNext.Left+BtnOpenNext.Width,GraphicMenuWidth)+BtnOpenPrior.Left;
       if Status<>nil then with Status do begin
          if   OrgBitMap<>nil then Inc(i,Max(OrgBitMap.Width,Max(0,BodySlice)))
          else Inc(i,Max(0,BodySlice));
          end;
       i:=Max(i,GraphicMenuWidth+4+Max(MAX_BOARD_WIDTH,MAX_BOARD_HEIGHT)*4); // each square is at least 4 x 4 pixels

       if (Menu<>nil) and (Menu.Count<>0) and
          (Menu.BtnMPlayerIndex=Pred(Menu.Count)) then with Menu.MenuItems[Pred(Menu.Count)] do
          if Visible then Inc(i,RectWidth(Rect));

       j:=i;
       if PanelMenu1.Visible then Inc(j,PanelMenu1.Height);
       if StatusBar1.Visible then Inc(j,StatusBar1.Height);

       if (Menu<>nil) and (Menu.Count<>0) then begin
          if Menu.BtnMPlayerIndex=Pred(Menu.Count) then k:=Pred(Pred(Menu.Count))
          else k:=Pred(Menu.Count); // 'k'=number of the last button in the menu-panel to the left
          k:=Menu.MenuItems[k].Rect.Bottom+Menu.ButtonDY;
          //if Status<>nil then Inc(k,Status.Height+Menu.ButtonDY);
          j:=Min(Menu.MinMenuHeight,Max(j,k));
          k:=Menu.ButtonHeight+Menu.ButtonDY; // 'k': button row height, including separator
          while (j<Menu.MaxMenuHeight) and // round the height up to nearest number of button rows
                (ClientHeight>j+(k div 2)) do
                Inc(j,k);
          end;

       // it's unclear whether changing width and height always triggers a new 'FormResize';
       // hence, 'FormResizeLevel' controls that the resizing code always is executed.
       Inc(FormResizeLevel);
       IsResizing:=False;

       //{
       if (ClientWidth <i) or (ClientHeight<j) then
          Self.SetBounds(Left,Top,Max(i,ClientWidth)+(Width-ClientWidth),Max(j,ClientHeight)+(Height-ClientHeight));
       //}

       //Self.SetBounds(Left,Top,640,480); // activate this statement and disable the preceding 2 statements in order to produce a 640x480 screenshot; also disable "Save" and "Save as..." buttons in the menu

       if FormResizeLevel>0   then DoResize;

     finally
       IsResizing:=False;
     end;
end;

procedure TMainForm.ShowTitle(const FileName:String);
var s,s1,s2:String;
begin
  if not (Assigned(MultiView) and Assigned(MultiView.Opening)) then begin
     s:=Application.Title;
     if (Game<>nil) then begin
        if Game.ReverseMode then s:=s+SUB_TITLE_SEPARATOR+TEXT_REVERSE_MODE;
        if FileName<>'' then begin
           s:=s+SUB_TITLE_SEPARATOR;
           s1:=VisualFileName(FileName);
           if CollectionNameInTitleLine and IsAnIniFileSectionFileName(FileName) then begin
              s2:=ExtractFileNameWithoutExt(ExtractIniFileName(FileName));
              if not StrEqual(s1,s2) then
                 s:=s+s2+SPACE+COLON+SPACE;
              end;
           s:=s+s1;
           if BoardDimensionsInTitleLine then
              s:=s+ SUB_TITLE_SEPARATOR+
                 Misc_.BoardDimensionsAndBoxesAndFloorsAsText(Game.BoardWidth,Game.BoardHeight,Game.BoxCount,Game.FloorCount,BoardDimensionsAsText,BoardDimensionsWithFloorCount);
           end;
        if Game.BoardTransformation2D<>t2DRotate0DegreesClockwise then
           s:=s+SUB_TITLE_SEPARATOR+BoardTransformation2DToStr(Game.BoardTransformation2D);
        end;
     Caption:=s;
     end;
end;

procedure TMainForm.DrawGrid1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var oMoveAnimationEnabled:Boolean;
begin
  if (GameViewer<>nil) and
     ((GameViewer.LegalMovesInfo.Mask<>0)
      or
      GameViewer.LegalMovesInfo.BoxCursor.Enabled
      or
      GameViewer.LegalMovesInfo.PlayerCursor.Enabled
     ) then
     GameViewer.HideLegalMoves;

  if (Screen.ActiveForm=Self) and (not MPlayer.Visible) then
     if   Game.IsReplaying and (Status<>nil) and
          (Status.Panels[spReplaySpeed].RectForm.Top<Status.Panels[spReplaySpeed].RectForm.Bottom) then
          if LastReplayAction<>gaUndo then begin
             LastReplayAction:=gaUndo; Game.IsIdleAndStopReplayingAndBrowsing;
             end
          else begin
             if   Status.ReplayMovesPerSec>0 then
                  Status.ReplayMovesPerSec:=Pred(Status.ReplayMovesPerSec)
             else;//Status.ReplayMovesPerSec:=MAX_REPLAY_SPEED_MOVES_PER_SEC
             Status.ShowHintForReplayMovesPerSec;
             end
     else if Game.IsBrowsing then
             if   Game.BrowsePosition>0 then
                  Game.BrowsePosition:=Game.BrowsePosition-1
             else
     else if (not Game.IsReplaying) and
             Game.IsIdleAndStopReplayingAndBrowsing then begin
             oMoveAnimationEnabled:=Game.MoveAnimationEnabled;
             try     Game.MoveAnimationEnabled:=Game.MoveAnimationEnabled and Game.AnimateMovesOnMouseWheelUpDown;
                     BtnUndoClick(Sender);
             finally Game.MoveAnimationEnabled:=oMoveAnimationEnabled;
             end;
             if Assigned(MultiView) and Assigned(MultiView.Selected) then begin
                MousePos:=ScreenToClient(Mouse.CursorPos);
                MultiView.OnMouseMove(Sender,Shift,MousePos.X,MousePos.Y);
                end;
             end;
  LastKey:=0;
  Handled:=True;
end;

procedure TMainForm.DrawGrid1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var oMoveAnimationEnabled:Boolean;
begin
  if (GameViewer<>nil) and
     ((GameViewer.LegalMovesInfo.Mask<>0)
      or
      GameViewer.LegalMovesInfo.BoxCursor.Enabled
      or
      GameViewer.LegalMovesInfo.PlayerCursor.Enabled
     ) then
     GameViewer.HideLegalMoves;

  if (Screen.ActiveForm=Self) and (not MPlayer.Visible) then
     if   Game.IsReplaying and (Status<>nil) and
          (Status.Panels[spReplaySpeed].RectForm.Top<Status.Panels[spReplaySpeed].RectForm.Bottom) then
          if LastReplayAction<>gaRedo then begin
             LastReplayAction:=gaRedo; Game.IsIdleAndStopReplayingAndBrowsing;
             end
          else begin
             if   Status.ReplayMovesPerSec<MAX_REPLAY_SPEED_MOVES_PER_SEC then
                  Status.ReplayMovesPerSec:=Succ(Status.ReplayMovesPerSec)
             else; //Status.ReplayMovesPerSec:=0
             Status.ShowHintForReplayMovesPerSec;
             end
     else if Game.IsBrowsing then
             if   Game.BrowsePosition<Game.MaxBrowsePosition then
                  Game.BrowsePosition:=Game.BrowsePosition+1
             else
     else if (not Game.IsReplaying) and Game.IsIdleAndStopReplayingAndBrowsing then begin
             oMoveAnimationEnabled:=Game.MoveAnimationEnabled;
             try     Game.MoveAnimationEnabled:=Game.MoveAnimationEnabled and Game.AnimateMovesOnMouseWheelUpDown;
                     BtnRedoClick(nil);
             finally Game.MoveAnimationEnabled:=oMoveAnimationEnabled;
             end;
             if Assigned(MultiView) and Assigned(MultiView.Selected) then begin
                MousePos:=ScreenToClient(Mouse.CursorPos);
                MultiView.OnMouseMove(Sender,Shift,MousePos.X,MousePos.Y);
                end;
             end;
  LastKey:=0;
  Handled:=True;
end;

procedure TMainForm.DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var ACol,ARow,MoveCount,PlayerLineCount,GameViewerMask:Integer;
    Pt:TPoint; MouseRect:TRect;
    Handled,IsPreferredDirectionOK,TimeOut,OldEnabled:Boolean; Position:TColRow; // Moves:TMoves;
begin
  IgnoreMouseUp:=Game.IsBusy or Game.IsReplaying or Game.IsBrowsing;
  Handled:=False; LastKey:=0;

  if   GameViewer<>nil then with GameViewer do with LegalMovesInfo do begin
       GameViewerMask:=Mask; // remember which mask was active before the mask is cleared in 'GameViewer' by 'HideLegalMoves'
       if ((GameViewerMask<>0) or BoxCursor.Enabled or PlayerCursor.Enabled) and
          (Button<>mbRight) then
          HideLegalMoves;
       end
  else GameViewerMask:=0;

  if (RotateAndFlipPopupMenu<>nil) and RotateAndFlipPopupMenu.Visible then
     RotateAndFlipPopupMenu.Hide;
  if (MultiViewPopupMenu<>nil) and MultiViewPopupMenu.Visible then
     MultiViewPopupMenu.Hide;

  MultiView.HideDragRect;

  if Status<>nil then with Status do begin
     Hint:='';

     if Game.IsBrowsing  then begin
        Status.MouseMove(X,Y); Handled:=True;
        if   Button=mbLeft then Status.Click
        else Game.IsIdleAndStopReplayingAndBrowsing;
        end;

     if Panels[spReplaySpeed].RectForm.Top<Panels[spReplaySpeed].RectForm.Bottom then // checking 'Game.IsReplaying' doesn't suffice, replay-mode may be paused
        if       Button=mbLeft then begin
                 with Status.Panels[spReplaySpeed].RectForm do MouseRect:=Classes.Rect(Left,Top-Status.TopMargin,Right,Bottom);
                 if PtInRect(MouseRect,Point(X,Y)) then with Status do begin
                    Panels[spReplaySpeed].ProgressBarLocked:=not Panels[spReplaySpeed].ProgressBarLocked;
                    Panels[spReplaySpeed].ProgressBarFocused:=True;
                    ReplayMovesPerSec:=ReplayMovesPerSec;
                    if Sound.Enabled then Sound.Play(stMenuSelect);
                    MouseMove(X,Y); Handled:=True;
                    Panels[spReplaySpeed].ProgressBarLockOnMouseUp:=False;  // reset it the first time; 'Status.ReplaySpeedMouseMove' sets it to 'True' on changes
                    IgnoreMouseUp:=IgnoreMouseUp and Panels[spReplaySpeed].ProgressBarLocked;
                    end;
                 end
        else if  (Button=mbRight) and Game.IsReplaying then begin
                 if   LastReplayAction=gaUndo then LastReplayAction:=gaRedo // the result is that the following 'IsIdleAndStop...'
                 else LastReplayAction:=gaUndo;                             // doesn't cause the replay-loop to exit, but only to toggle the direction
                 end;

     end;

  if not Handled then
     if Game.IsIdleAndStopReplayingAndBrowsing then
        if MPlayer.Visible then
           if   MPlayer.MouseDown(Sender,Button,Shift,X,Y) then
                if Button=mbRight then IgnoreMouseUp:=True
                else {}
           else MPlayer.Hide
        else begin
           Game.MoveCountSinceLastKeyUpEvent:=0;

           if (not MultiView.IsEmpty) and
              (X>=GraphicMenuWidth)   and (Y<Status.RectForm.Top-Status.TopMargin) and
              MultiView.OnMouseDown(Sender,Button,Shift,X,Y,GameViewerMask) then begin // 'True': the multiple views manager has handled the 'mouse down' event
              ClearTrackBox(True);
              end
           else
              if (Button=mbLeft) and (Game.GameState=gsPlay) and
                 (X>=GraphicMenuWidth) and (Y<Status.RectForm.Top-Status.TopMargin) then begin
                 if      Image1   .Visible then GameViewer.MouseToCell(X,Y,ACol,ARow)
                 else if DrawGrid1.Visible then DrawGrid1 .MouseToCell(X,Y,ACol,ARow)
                 else begin ACol:=MaxInt-1; ARow:=MaxInt-1; end;
                 Inc(ACol); Inc(ARow); Position.x:=ACol; Position.y:=ARow; // 'Inc': 1-based columns and rows, as opposed to 'GameViewer.MouseToCell' which returns 0-based cells

                 if (ACol<=Game.BoardWidth) and (ARow<=Game.BoardHeight) then begin
                    if   (ACol=Game.PlayerPos.X) and
                         (ARow=Game.PlayerPos.Y) and
                         (TrackBox.X=0) then begin
                         ClearTrackBox(True);
                         if   TrackState=tsTrack then
                              TrackState:=tsSit
                         else if   (Game.GameState=gsPlay) and
                                   MouseTrackingModeEnabled then
                                   TrackState:=tsTrack
                              else;

                         if not MouseTrackingModeEnabled then begin
                            TrackBox.X:=ACol; TrackBox.Y:=ARow;

                            if   Image1.Visible then
                                 Image1.BeginDrag(False)
                            else if DrawGrid1.Visible then with DrawGrid1 do
                                    begin Invalidate;
                                          BeginDrag(False);
                                    end;

                            if (GameViewer.LegalMovesInfo.Enabled
                                or
                                GameViewer.LegalMovesInfo.PlayerCursor.Enabled
                               )
                               and
                               (//(Game.PlayerLegalJumps>0)                           // jumps allowed in reverse mode games,
                                //or                                                  // but it's better to show normal legal moves;
                                (Game.PlayerLegalMoves(0,0,ACol,ARow)>1)              // '1': current position counts as 1
                                or
                                (Game.ReverseMode
                                 and
                                 ((Game.FloorCount-Game.BoxCount-1)>0)                // any floor-squares the player can jump to?
                                 and
                                 ((Game.History.PushCount=0)
                                  or
                                  Game.JumpsAllowedAfterFirstBoxMoveInReverseMode
                                 )
                                )
                               ) then
                               GameViewer.ShowLegalMoves(PLAYER_LEGAL_MOVE,TrackBox,clBlack);
                            end;
                          end
                    else if (Game.Board[ACol,ARow] and BOX)<>0 then
                            if   (TrackBox.X=0) or
                                 (TrackBox.X<>ACol) or
                                 (TrackBox.Y<>ARow) then begin

                                 if (GameViewerMask=BOX_SET_TO_SQUARE) and
                                    ((Game.Board[ACol,ARow] and BOX_SET_TO_SQUARE)=BOX_SET_TO_SQUARE) and
                                    Game.BoxPath(Game.BoxPos[Game.Board[ACol,ARow] shr BOARD_FLAG_COUNT],
                                                 GameViewer.LegalMovesInfo.CursorPos,Game.PlayerPos,MoveCount,MovesOrHistory.Moves) then begin
                                    if  (TrackBox.X=0) and
                                        MouseTrackingModeEnabled then
                                        TrackState:=tsTrack
                                    else TrackState:=tsWait;
                                    if MPlayer.Visible then MPlayer.Hide;
                                    TrackBox.X:=0; TrackBox.Y:=0;
                                    SetCursor(X,Y);
                                    LastGameAction:=gaNull;
                                    Game.TryMoves(MoveCount,MovesOrHistory.Moves);
                                    end
                                 else begin
                                    if TrackBox.X<>0 then ClearTrackBox(True);

                                    TrackBox.X:=ACol; TrackBox.Y:=ARow;

                                    if      Image1.Visible then begin
                                            // first use 'SetCursor' to update the cursor.
                                            // the 'Image1.BeginDrag(True)' statement triggers a 'MouseUp' event.
                                            // without the 'SetCursor' statement, then if (and only if) the user just
                                            // selects the box by clicking it with the mouse instead of dragging the
                                            // box to a different location, then the 'BeginDrag' statement makes the
                                            // screen cursor shift back and forth between the normal cursor and the
                                            // drag cursor one time too many. this can be disturbing once the user
                                            // has noticed it.
                                            // the 'SetCursor' statement fixes this problem in the normal case,
                                            // where the user has only one open view of the level. with multiple
                                            // open views, "MultiView.IgnoreMouseUp__" must also be set to
                                            // "True" to avoid the problem.
                                            SetCursor(X,Y);
                                            MultiView.IgnoreMouseUp__:=True;
                                            Image1.BeginDrag(True);
                                            end
                                    else if DrawGrid1.Visible then with DrawGrid1 do
                                            begin Invalidate;
                                                  BeginDrag(False);
                                            end;

                                    if (GameViewer.LegalMovesInfo.Enabled
                                        or
                                        GameViewer.LegalMovesInfo.BoxCursor.Enabled
                                       )
                                       and
                                       (Game.BoxLegalMoves(ACol,ARow,TimeOut)>1) then // '1' current position counts as 1
                                       GameViewer.ShowLegalMoves(BOX_LEGAL_MOVE,TrackBox,clBlack);
                                    end;
                                 end
                            else ClearTrackBox(True)
                    else if (Game.Board[ACol,ARow] and FLOOR)<>0 then
                            if         (TrackBox.X=0) and
                                       ((Position.x<>Game.PlayerPos.X) or (Position.y<>Game.PlayerPos.Y)) then begin

                                       if OnLeftClickEmptySquareAction=olcesaShowBoxesThatCanGoToSquare then begin
                                          if ((GameViewerMask<>BOX_SET_TO_SQUARE)
                                              or
                                              (Position.x<>GameViewer.LegalMovesInfo.CursorPos.x)
                                              or
                                              (Position.y<>GameViewer.LegalMovesInfo.CursorPos.y)
                                             )
                                             and
                                             (Game.BoxesToSquare(Position,TimeOut)<>0) then begin
                                             GameViewer.ShowLegalMoves(BOX_SET_TO_SQUARE,Position,GameViewer.LegalMovesInfo.BoxCursor.Color);
                                             Status.Hint:='';
                                             end;
                                          end
                                       else // left-click-empty-square action is 'move player to square'
                                          if Game.PlayerPath(Game.PlayerPos,Position,False,(Game.History.Count>0) and (not (OptimizeMovesBetweenPushes and ((Game.History.Moves[Game.History.Count] and H_FLAG_BOX)=0))),TDirection(Game.History.Moves[Game.History.Count] and H_MASK_DIRECTION),MoveCount,PlayerLineCount,IsPreferredDirectionOK,PPlayerMoves(Addr(MovesOrHistory.Moves))) // try to find a path from the player position to the floor; 'not (OptimizeMovesBetweenPushes...': if in-between moves optimization is enabled and the latest move was a non-pushing player move, then show the path that visibly seems to be moves/lines optimal, and rely on the in-between moves optimizer to substitute it with the true best path
                                             or
                                             (Game.ReverseMode
                                              and
                                              Game.PlayerJump(Game.PlayerPos,Position,True,MoveCount,PlayerLineCount,MovesOrHistory.Moves)
                                             ) then begin
                                             if   (TrackBox.X=0) and
                                                  MouseTrackingModeEnabled then
                                                  TrackState:=tsTrack
                                             else TrackState:=tsWait;
                                             if MPlayer.Visible then MPlayer.Hide;
                                             TrackBox.X:=0; TrackBox.Y:=0; SetCursor(X,Y);
                                             LastGameAction:=gaNull;
                                             Game.TryMoves(MoveCount,MovesOrHistory.Moves);
                                             Status.Hint:='';
                                             end;

                                       if (SnapshotsForm<>nil) and SnapshotsForm.Visible and
                                          DrawGrid1.Visible then
                                          DrawGrid1.BeginDrag(False);
                                       end
                            else if    (((TrackBox.X=0)
                                         or
                                         ((TrackBox.X=Game.PlayerPos.X) and (TrackBox.Y=Game.PlayerPos.Y))
                                        )
                                        and
                                        (Game.PlayerPath(Game.PlayerPos,Position,False,(Game.History.Count>0) and (not (OptimizeMovesBetweenPushes and ((Game.History.Moves[Game.History.Count] and H_FLAG_BOX)=0))),TDirection(Game.History.Moves[Game.History.Count] and H_MASK_DIRECTION),MoveCount,PlayerLineCount,IsPreferredDirectionOK,PPlayerMoves(Addr(MovesOrHistory.Moves))) // try to find a path from the player position to the floor; 'not (OptimizeMovesBetweenPushes...': if in-between moves optimization is enabled and the latest move was a non-pushing player move, then show the path that visibly seems to be moves/lines optimal, and rely on the in-between moves optimizer to substitute it with the true best path
                                         or
                                         (Game.ReverseMode
                                          and
                                          Game.PlayerJump(Game.PlayerPos,Position,True,MoveCount,PlayerLineCount,MovesOrHistory.Moves)
                                         )
                                        )
                                      )
                                      or
                                      ((TrackBox.X<>0) and
                                       ((TrackBox.X<>Game.PlayerPos.X) or (TrackBox.Y<>Game.PlayerPos.Y)) and
                                       Game.BoxPath(TrackBox,Position,Game.PlayerPos,MoveCount,MovesOrHistory.Moves)) // try to find a path from the box position to the floor
                                       then begin
                                       if  (TrackBox.X=0) and
                                            MouseTrackingModeEnabled then
                                            TrackState:=tsTrack
                                       else TrackState:=tsWait;
                                       if MPlayer.Visible then MPlayer.Hide;
                                       TrackBox.X:=0; TrackBox.Y:=0; SetCursor(X,Y);
                                       LastGameAction:=gaNull;
                                       Game.TryMoves(MoveCount,MovesOrHistory.Moves);
                                       end
                                 else  begin ClearTrackBox(True);
                                             if (Game.GameState=gsPlay) and
                                                Sound.Enabled and
                                                (Sound.Player[stBlockedStep]<>nil) then
                                                Sound.Play(stBlockedStep);
                                             if (SnapshotsForm<>nil) and SnapshotsForm.Visible then
                                                DrawGrid1.BeginDrag(False);
                                       end
                    else begin ClearTrackBox(True);
                               if (GameViewerMask<>SQUARE_SET)
                                  and
                                  (((Game.Board[ACol,ARow] and WALL )<>0)
//                                 or
//                                 ((Game.Board[ACol,ARow] and FLOOR) =0) {if it isn't a wall or floor then it's an inactive square}
                                  )
                                  and
                                  (Game.CalculateMovableBoxes<>0)
                                  and
                                  (GameViewer<>nil) then begin
                                  StartHintCount:=0;
                                  GameViewer.ShowLegalMoves(SQUARE_SET,Game.BoxPos[0],GameViewer.LegalMovesInfo.BoxCursor.Color);
                                  Status.Hint:='';
                                  end;
                               if (SnapshotsForm<>nil) and SnapshotsForm.Visible and
                                  DrawGrid1.Visible then
                                  DrawGrid1.BeginDrag(False);
                         end;
                    end
                 else begin
                    if (TrackBox.X<>0) or
                       ((GameViewer<>nil) and (GameViewer.LegalMovesInfo.Mask<>0)) then
                       ClearTrackBox(True);
                    end;
                 end
              else if (Button=mbLeft) and
                      ((X<GraphicMenuWidth) or (Y>=Status.RectForm.Top-Status.TopMargin)) then begin
                      if (TrackBox.X<>0) or
                         ((GameViewer<>nil) and (GameViewer.LegalMovesInfo.Mask<>0)) then
                         ClearTrackBox(True);
                      if Menu <>nil then begin
                         // avoid calling 'Menu.MouseMove(X,Y)' because it would trigger a 'menu over' sound
                         //if Menu.MouseMove(X,Y)<>-1 then Menu.Click;
                         Menu.SkipMouseOverSoundForItemIndex:=Menu.ItemAtPos(X,Y);
                         Menu.ItemIndex:=Menu.SkipMouseOverSoundForItemIndex;
                         if Menu.ItemIndex<>-1 then Menu.Click;
                         end;
                      if Status<>nil then begin
                         Status.MouseMove(X,Y); Status.Click;
                         end;
                      end
              else if Button=mbRight then begin
                      if (TrackBox.X<>0) or
                         ((GameViewer<>nil) and (GameViewer.LegalMovesInfo.Mask<>0)) then begin
                         ClearTrackBox(True);
                         IgnoreMouseUp := True;
                         end;
                      end
              else if Button=mbMiddle then
                      if      (Menu<>nil) and (Menu.BtnRotateIndex>=0) and
                              PtInRect(Menu.MenuItems[Menu.BtnRotateIndex].Rect,Point(X,Y)) then
                              PanelRotateAndFlipClick(PanelResetTransformations)
                      else if (Menu<>nil) and (Menu.BtnSolutionIndex>=0) and
                              PtInRect(Menu.MenuItems[Menu.BtnSolutionIndex].Rect,Point(X,Y)) then
                              BtnStatusMenuPluginClick(BtnStatusMenuSolver)
                      else if (Menu<>nil) and (Menu.BtnToolsIndex>=0) and
                              PtInRect(Menu.MenuItems[Menu.BtnToolsIndex].Rect,Point(X,Y)) then
                              BtnStatusMenuPluginClick(BtnStatusMenuSolver)
                           else begin
                              Sound.StopAll;
                              BtnReverseModeClick(Sender);
                              end;
           end
     else
        if (Status<>nil) then begin
           Status.MouseMove(X,Y);
           if Button=mbLeft then Status.Click;
           end;
end;

procedure TMainForm.DrawGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//var ACol,ARow:Integer; TimeOut:Boolean;
begin
  if DrawGrid1.Dragging then DrawGrid1.EndDrag(False);
  LastKey:=0;

  if IgnoreMouseUp then begin
     IgnoreMouseUp:=False;
     end
  else
     if (MPlayer<>nil) and MPlayer.Visible then
        MPlayer.MouseUp(Sender,Button,Shift,X,Y)
     else
        if      Button=mbLeft then begin
                if (Status<>nil) and
                   Status.Panels[spReplaySpeed].ProgressBarLockOnMouseUp then with Status do begin
                   Panels[spReplaySpeed].ProgressBarLockOnMouseUp:=False;
                   Panels[spReplaySpeed].ProgressBarLocked:=True;
                   ReplayMovesPerSec:=ReplayMovesPerSec; // refresh display
                   if Panels[spReplaySpeed].RectForm.Top<Panels[spReplaySpeed].RectForm.Bottom then // just to be sure that the panel still is active
                      if Sound.Enabled then Sound.Play(stMenuSelect);
                   if Screen.Cursor<>DEFAULT_CURSOR then MultiView.SetCursor(X,Y);
                   end;

                if (MultiView<>nil) and
                   (not MultiView.IsEmpty) and
                   ((X>=GraphicMenuWidth) and (Y<Status.RectForm.Top-Status.TopMargin)
                    or
                    MultiView.IsDragging
                    or
                    MultiView.IsSizing
                   ) then
                   MultiView.OnMouseUp(Sender,Button,Shift,X,Y);
{
                if not GameViewer.ShowLegalMovesInfo.Enabled and // for testing only
                   // in production, it should be something like
                   // 'if <Enabled> = OnLiftDrop' where 'Enabled' can be 'OnClick', 'OnLiftDrop', or 'Never';
                   // unfortunately it doesn't work, because in 'MouseDown' the 'BeginDrag'-statement
                   // triggers a 'MouseUp' event, and the real 'MouseUp' event disappears
                   (Game.GameState=gsPlay) and
                   (X>=GraphicMenuWidth) and (Y<Status.Top) and
                   (not MouseTrackingModeEnabled) then begin
                   if Image1.Visible then         GameViewer.MouseToCell(X,Y,ACol,ARow)
                   else if DrawGrid1.Visible then DrawGrid1 .MouseToCell(X,Y,ACol,ARow)
                   else begin ACol:=MaxInt-1; ARow:=MaxInt-1; end;
                   Inc(ACol); Inc(ARow);
                   if (ACol<=Game.BoardWidth) and (ARow<=Game.BoardHeight) and
                      (ACol =TrackBox.X     ) and (ARow =TrackBox.Y      ) then
                      if   (ACol=Game.PlayerPos.X) and
                           (ARow=Game.PlayerPos.Y) then
                           if Game.PlayerShowLegalMoves(ACol,ARow)>1 then      // '1': current position counts as 1
                              GameViewer.ShowShowLegalMoves(PLAYER_POSSIBLE_MOVE,TrackBox)
                           else
                      else if Game.BoxShowLegalMoves(ACol,ARow,TimeOut)>1 then // '1': current position counts as 1
                              GameViewer.ShowShowLegalMoves(BOX_POSSIBLE_MOVE,TrackBox);
                   end;
}
                end
        else if Button=mbRight then begin
                Sound.StopAll;
                if             (MultiView<>nil) and
                               (not MultiView.IsEmpty) and
                               (X>=GraphicMenuWidth) and (Y<Status.RectForm.Top-Status.TopMargin) and
                               MultiView.OnMouseUp(Sender,Button,Shift,X,Y) then // 'True': the 'multiple views' manager handled the 'mouse up' event
                               ClearTrackBox(True)
                else if        (TrackBox.X<>0) or
                               ((GameViewer<>nil) and (GameViewer.LegalMovesInfo.Mask<>0)) then
                               ClearTrackBox(True)
                else if        (Game<>nil) and (Menu<>nil) and (Menu.BtnSolutionIndex>=0) and
                               PtInRect(Menu.MenuItems[Menu.BtnSolutionIndex].Rect,Point(X,Y)) and
                               ((Game.BestSolutionMoves<>nil) or (Game.BestSolutionPushes<>nil)) then
                               // show the opposite of the normally preferred solution
                               if   ShowSolutionMoves then BtnSolutionClick(nil)   // 'nil' : pushes
                               else BtnSolutionClick(Self)                         // 'Self': moves
                else if        (Game<>nil) and (Menu<>nil) and (Menu.BtnSettingsIndex>=0) and
                               PtInRect(Menu.MenuItems[Menu.BtnSettingsIndex].Rect,Point(X,Y)) and
                               Assigned(OptionsForm) then
                               if   ssShift in Shift then begin
                                    BtnSkinClick(BtnOpenPrior);
                                    IgnoreKeyUp:=True;    // this doesn't work here. even though the shift key was pressed before this "mouse up" event, the shift key generates "key down" events after the "mouse up" event has been processed here.
                                    IgnoreShiftKey:=True; // a work around to cope with the shift key generating "key down" events after this "mouse up" even has been processed
                                    end
                               else BtnSkinClick(BtnOpenNext)
                else if        OnRightClickAction=orcaUndoMove then
                               BtnUndoClick(Sender)
                     else if   BtnSnapshots.Enabled and
                               (SnapshotsForm<>nil) and (SnapshotsForm.Visible) and
                               (SnapshotsForm.Snapshots[SnapshotsForm.Grid.Row]<>nil) and
                               ((Game<>nil) and Game.IsIdleAndStopReplayingAndBrowsing) and
                               (OnRightClickAction=orcaLoadSnapshotIfAnyElseRestartGame) then begin
                               BtnSnapshotsClick(nil);
                               if BtnSnapshots.Enabled and
                                  (Menu<>nil) and
                                  (Menu.ItemIndex=Menu.BtnSnapshotsIndex) then
                                  Status.Hint:=GetLongHint(BtnSnapshots.Hint);
                               end
                          else BtnResetClick(Sender)
                end
             else begin // (mouse button <> mbLeft) and (mouse button <> mbRight)
                if MultiView<>nil then
                   MultiView.OnMouseUp(Sender,Button,Shift,X,Y);
                end;

end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  SwDebug:=not SwDebug;
end;

procedure TMainForm.BtnToolsClick(Sender: TObject);
var oSnapshotsFormVisible:Boolean; oWindowState:TWindowState;
    TabSheet:TTabSheet; ToolFlags:TToolFlagSet;
begin
  if (Game<>nil) and
     Game.IsIdleAndStopReplayingAndBrowsing and
     (GamePictures<>nil) and
     GamePictures.Initialized then begin
     Game.StopTimer;
     ClearTrackBox(True);
     MPlayer.Hide;
     if Assigned(MultiView) and Assigned(MultiView.Selected) then MultiView.Selected.MakeSnapshot;
     oSnapshotsFormVisible:=SnapshotsForm.Visible;
     SnapshotsForm.Hide;
     oWindowState:=WindowState; //WindowState:=wsMinimized;
     if Deadlocks<>nil then Deadlocks.Suspend;
     ToolsForm.SettingsModified:=False;
     TabSheet:=ToolsForm.PageControl1.ActivePage;
     ToolFlags:=[];
     if      (Sender=ToolsForm.EditMenuItemNew)
             or
             ((Sender=BtnTools)
              and
              (ceCtrl in CtrlEnterKeySet)
              and
              (Game.GameState<>gsNull)
             ) then begin
             TabSheet:=ToolsForm.TabSheetEditor;
             Include(ToolFlags,tfEditCurrentPosition);
             end
     else if Sender=ToolsForm.TabSheetEditor then
             TabSheet:=ToolsForm.TabSheetEditor
     else if Sender=ToolsForm.TabSheetSolver then
             TabSheet:=ToolsForm.TabSheetSolver
     else if Sender=ToolsForm.TabSheetOptimizer then
             TabSheet:=ToolsForm.TabSheetOptimizer
     else if Sender=ToolsForm.TabSheetGenerator then
             TabSheet:=ToolsForm.TabSheetGenerator
     else if Sender=ToolsForm.TabSheetCapture then
             TabSheet:=ToolsForm.TabSheetCapture;
     if ToolsForm.InitializeTask(Self,TabSheet,ToolFlags) then begin
        try     Self.Hide;
                //Result:=(EditForm.ShowModal=mrOk) or EditForm.Refresh;
                ToolsForm.ShowModal;
        finally SetMessageHandlers;
                Self.Show;
//              if EditForm <>nil then EditForm .Modified:=False;  // so 'Save' can trust 'EditForm .Modified' when it is called from the editor form
                if ToolsForm<>nil then ToolsForm.Modified:=False;  // so 'Save' can trust 'ToolsForm.Modified' when it is called from the tools  form
                IgnoreKeyUp:=True;
                IgnoreMouseUp:=True;
                IgnoreShiftKey:=False;
                CtrlEnterKeySet:=[]; LastKey:=0;
                BusyMode:=False;
                if (Deadlocks<>nil) and (not ShutDownApplication) then Deadlocks.Resume;
                if MainForm.WindowState<>oWindowState then begin
                   WindowState:=oWindowState; Update;
                   end;
                if Assigned(MainForm.Solver   ) then MainForm.Solver   .ImportGames(True);
                if Assigned(MainForm.Optimizer) then MainForm.Optimizer.ImportGames(True);
                if Assigned(MainForm.MultiView) then MainForm.MultiView.OnResize(nil);
                ShowStatus;
        end;
        if      ToolsForm.Editor.LevelHasChangedOrHasBeenModified then begin
                InitGame(Game.ReverseMode and (Game.History.Count=0),True,True,False,True,True,True,0);
                if SnapshotsForm<>nil then SnapshotsForm.Grid.Cells[1,0]:=VisualFileName(Game.FileName);
                end;
        end;

     ShowTitle(Game.FileName);
     //Application.OnHint:=ShowHint;
     Menu.SkipMouseOverSoundForItemIndex:=-1;

     if not ShutDownApplication then begin
        if oSnapshotsFormVisible or SnapshotsForm.ShowOnStartUp then begin
           if Assigned(Solver) then Solver.Enter;
           try     if Assigned(Optimizer) then Optimizer.Enter;
                   try     SnapshotsForm.Show;
                           if SnapshotsForm.WindowState=wsMinimized then
                              SnapshotsForm.WindowState:=wsNormal;
                           SnapshotsForm.FormDeactivate(Self);
                           Self.SetFocus;
                   finally if Assigned(Optimizer) then Optimizer.Leave;
                   end;
           finally if Assigned(Solver) then Solver.Leave;
                   SetMessageHandlers;
           end;
           end;
        if ToolsForm.SettingsModified then SelectOptions(nil);
        end
      else begin
         Msg(ApplicationShutdownText,Application.Title,0);
         Self.Update;
         BtnExitClick(Sender);
         end;

     end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var PreviousKey:Integer;

  function TryPull(Direction__:TDirection; PreviousKey__:Integer; var Key__:Word; var Shift__:TShiftState):Boolean;
  begin // if the player bumps into a box and then moves in the opposite direction, then this is interpreted as a command to move the player without pulling the box
    Result:=True; Key__:=0;
    if PreviousKey__<>DIRECTION_TO_KEY[OPPOSITE_DIRECTION[Direction__]] then with Game do begin
       if   (Board[PlayerPos.X+DIRECTION_XY[Direction__,ColAxis],PlayerPos.Y+DIRECTION_XY[Direction__,RowAxis]] and BOX)<>0 then begin
            if PreviousKey__<>DIRECTION_TO_KEY[Direction__] then begin // 'True': this is the first attempt to move in this direction; it will fail because it's blocked by the box, the next attempt will raise a normal warning about the illegal move, that is, the next attempt will trigger play the 'illegal move' sound
               Result:=False;
               Key__:=DIRECTION_TO_KEY[Direction__]; // 'Key__': upon return from this local function, the key will be stored in 'LastKey'; then the next move uses it to detect that the previous move bumped into the box
               if Sound.Enabled then Sound.Play(stMenuSelect); // use the 'menu select' sound to signal that "something has happened", that is, that the program is prepared to move the player in the opposite direction without pulling the box
               end;
            end;
       end
    else with Game do // the previous player move was in the opposite direction, i.e., it bumped into the box
       if (Board[PlayerPos.X+DIRECTION_XY[OPPOSITE_DIRECTION[Direction__],ColAxis],PlayerPos.Y+DIRECTION_XY[OPPOSITE_DIRECTION[Direction__],RowAxis]] and BOX)<>0 then
          Include(Shift__,ssCtrl); // include 'ssCtrl': don't pull the box
  end;

begin //exit;
  if Key<>SIMULATED_KEYBOARD_EVENT_KEY then begin
     IgnoreKeyUp:=False;

     if      Key=VK_RETURN  then Include(CtrlEnterKeySet,ceEnter)
     else if Key=VK_CONTROL then Include(CtrlEnterKeySet,ceCtrl);

     if (RotateAndFlipPopupMenu<>nil) and RotateAndFlipPopupMenu.Visible then
        RotateAndFlipPopupMenu.Hide;
     if (MultiViewPopupMenu<>nil) and MultiViewPopupMenu.Visible then
        MultiViewPopupMenu.Hide;

     if Game.IsReplaying and (Status<>nil) and
        (Status.Panels[spReplaySpeed].RectForm.Top<Status.Panels[spReplaySpeed].RectForm.Bottom) then
        if      (Shift<>[]) and
                ((Key=Ord(ACCEL_CHAR_UNDO          ))  or
                 (Key=Ord(ACCEL_CHAR_UNDO1         ))  or
                 (Key=Ord(ACCEL_CHAR_RESET         ))) then begin
//              if        LastReplayAction<>gaUndo then begin
//                        LastReplayAction:=gaUndo; Game.IsIdleAndStopReplayingAndBrowsing;
//                        end
//              else
                     if   Status.ReplayMovesPerSec>0 then
                          Status.ReplayMovesPerSec:=Pred(Status.ReplayMovesPerSec)
                     else ;//Status.ReplayMovesPerSec:=MAX_REPLAY_SPEED_MOVES_PER_SEC;
                Key:=0;
                end
        else if (Shift<>[]) and
                ((Key=Ord(ACCEL_CHAR_REDO          )) or
                 (Key=Ord(ACCEL_CHAR_REDO_ALL      ))) then begin
//              if        LastReplayAction<>gaRedo then begin
//                        LastReplayAction:=gaRedo; Game.IsIdleAndStopReplayingAndBrowsing;
//                        end
//              else
                     if   Status.ReplayMovesPerSec<MAX_REPLAY_SPEED_MOVES_PER_SEC then
                          Status.ReplayMovesPerSec:=Succ(Status.ReplayMovesPerSec)
                     else ;//Status.ReplayMovesPerSec:=0;
                Key:=0;
                end
        else    if Key=VK_CONTROL                     then Key:=0
        else    if Key=VK_SHIFT                       then Key:=0
        else;

     if (Key<>0) and Game.IsIdleAndStopReplayingAndBrowsing then begin
        PreviousKey:=LastKey; // 'PreviousKey': a local copy of 'LastKey' because the latter is cleared by 'ClearTrackBox' before the old value is used in this procedure
        ClearTrackBox(True);

        if      OnShiftKeyAction=oskaSHIFTisUPandUPisDOWN then
                // remap SHIFT -> up arrow and up arrow -> down arrow.
                // this is an ergonomical advantage for some keyboard layouts
                // where the up and down arrow keys are half-size keys and share
                // one slot in the layout.
                // The remapping applies to both shift keys. Ideally, the
                // remapping should be implemented for the right shift key only,
                // but for some unknown reason the application doesn't receive
                // information about which shift key was pressed.
                if      Key=VK_SHIFT                  then begin
                        Key:=VK_UP;
                        Exclude(Shift,ssShift);
                        IgnoreKeyUp:=True;
                        end
                else if Key=VK_UP                     then Key:=VK_DOWN;

        if      ((Key=VK_RETURN) or (Key=Ord(SPACE))) and (Shift=[]) and
                (MPlayer<>nil) and MPlayer.Visible    and
                (Mandala<>nil) and Mandala.Enabled    and
                (MPlayer.Display<>nil)                and
                (MPlayer.Display.Activity in [acMandala,acFractals]) then
                with MPlayer.Display do
                  if   Activity=acMandala             then begin
                       Status.Hint:='';
                       BtnMandalaStartStopClick(Self);
                       end
                  else if Activity=acFractals         then Fractals.ColorCycling:=not Fractals.ColorCycling
                  else
        else if Key=VK_UP                             then begin
                if (not Game.ReverseMode) or (Shift<>[]) or TryPull(SokUtil_.Up,PreviousKey,Key,Shift) then
                   if Shift=[]                        then Game.TryMove            (SokUtil_.Up   ,Game.History.Count,H_FLAG_BOX+MOVE_FLAG_KEYBOARD)
                   else                                    Game.TryMove            (SokUtil_.Up   ,Game.History.Count,0         +MOVE_FLAG_KEYBOARD);
                end
        else if Key=VK_DOWN                           then begin
                if (not Game.ReverseMode) or (Shift<>[]) or TryPull(SokUtil_.Down,PreviousKey,Key,Shift) then
                   if Shift=[]                        then Game.TryMove            (SokUtil_.Down ,Game.History.Count,H_FLAG_BOX+MOVE_FLAG_KEYBOARD)
                   else                                    Game.TryMove            (SokUtil_.Down ,Game.History.Count,0         +MOVE_FLAG_KEYBOARD);
                end
        else if Key=VK_LEFT                           then begin
                if (not Game.ReverseMode) or (Shift<>[]) or TryPull(SokUtil_.Left,PreviousKey,Key,Shift) then
                   if Shift=[]                        then Game.TryMove            (SokUtil_.Left ,Game.History.Count,H_FLAG_BOX+MOVE_FLAG_KEYBOARD)
                   else                                    Game.TryMove            (SokUtil_.Left ,Game.History.Count,0         +MOVE_FLAG_KEYBOARD);
                end
        else if Key=VK_RIGHT                          then begin
                if (not Game.ReverseMode) or (Shift<>[]) or TryPull(SokUtil_.Right,PreviousKey,Key,Shift) then
                   if Shift=[]                        then Game.TryMove            (SokUtil_.Right,Game.History.Count,H_FLAG_BOX+MOVE_FLAG_KEYBOARD)
                   else                                    Game.TryMove            (SokUtil_.Right,Game.History.Count,0         +MOVE_FLAG_KEYBOARD);
                end
        else if Key=Ord(ACCEL_CHAR_HELP            )  then BtnHelpClick            (Sender)
        else if Key=Ord(ACCEL_CHAR_SNAPSHOTS       )  then
                if Shift=[]                           then // BtnSnapshotsClick    (nil) // moved to 'KeyUp'
                else                                       // BtnSnapshotsClick    (Self)// moved to 'KeyUp'
        else if Key=Ord(ACCEL_CHAR_BOOKMARKS       )  then
                if Shift=[]                           then // BtnBookmarksClick    (nil) // moved to 'KeyUp'
                else                                       BtnBookmarksClick(Sender)
        else if MPlayer.Visible
                and
                ((Key=Ord(ACCEL_CHAR_PLAY          ))
                 or
                 (Key=Ord(ACCEL_CHAR_PAUSE         ))
                )
                and
                (MPlayer.Buttons[Ord(btPlay)].Visible
                 or
                 MPlayer.Buttons[Ord(btPlayPause)].Visible
                ) then begin
                if   MPlayer.Buttons[Ord(btPlay)].Visible then
                     MPlayer.ItemIndex:=Ord(btPlay)
                else MPlayer.ItemIndex:=Ord(btPlayPause);
                MPlayer.Click(mbLeft);
                end
        else if Key=Ord(ACCEL_CHAR_RESTART_GAME    )  then
                if (ssCtrl in Shift)                  and
                   (Game.History.Count>Game.ForcedInitialJumps)
                                                      then BtnRedoAllClick         (BtnUndo)
                else                                       BtnResetClick           (Sender)
        else if Key=Ord(ACCEL_CHAR_OPEN_PRIOR      )  then BtnOpenPriorOrNextClick (BtnOpenPrior)
        else if Key=Ord(ACCEL_CHAR_OPEN            )  then BtnOpenClick            (Sender)
        else if Key=Ord(ACCEL_CHAR_OPEN1           )  then BtnOpenClick            (Sender)
        else if Key=Ord(ACCEL_CHAR_OPEN_NEXT       )  then BtnOpenPriorOrNextClick (BtnOpenNext)
        else if Key=Ord(ACCEL_CHAR_PASTE_FROM_CLIPBOARD)
                                                      then BtnOpenClipboardClick   (Sender)
        else if Key=Ord(ACCEL_CHAR_OPEN_SNAPSHOTS  )  then
                if SnapshotsForm<>nil then begin
                   SnapshotsForm.Show; SnapshotsForm.SetFocus;
                   end
                else
        else if Key=Ord(ACCEL_CHAR_SAVE            )  then BtnSaveClick            (Sender)
        else if Key=Ord(ACCEL_CHAR_SAVE_AS         )  then BtnSaveAsClick          (Sender)
        else if Key=Ord(ACCEL_CHAR_OPTIONS         )  then BtnOptionsClick         (Sender)
        else if Key=Ord(ACCEL_CHAR_MENU            )  then
                if Menu.BtnMenuIndex<>-1              then BtnMenuClick            (Sender)
                else
        else if Key=Ord(ACCEL_CHAR_EXIT            )  then
                if MPlayer.Visible                    then MPlayer.Hide
                else if Shift<>[] then
                        if Assigned(MultiView) and    (not MultiView.IsEmpty) and False // 'False': it's too dangerous to let the keyboard shortcut close multiple views items; the user might not expect that, and then it can lead to loss of snapshots
                                                      then begin if Assigned(Game) and Game.IsIdleAndStopReplayingAndBrowsing then begin
                                                                    TrackState:=tsWait; ClearTrackBox(True);
                                                                    MultiView.CloseItem(MultiView.Selected,False);
                                                                    MultiView.DisappearedItemsCount:=0;
                                                                    MultiView.OnResize(nil);
                                                                    if MultiView.ReportDisappearedItems(MultiView.DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
                                                                       MultiView.DoReportDisappearedItems:=False;
                                                                    end;
                                                           end
                        else                               BtnExitClick            (Sender)
                     else
        else if (Key=Ord(ACCEL_CHAR_UNDO)          )  or
                (Key=Ord(ACCEL_CHAR_UNDO1)         )  then BtnUndoClick            (Sender)
        else if Key=Ord(ACCEL_CHAR_REDO            )  then BtnRedoClick            (Sender)
        else if Key=Ord(ACCEL_CHAR_REDO_ALL        )  then
                if (ssCtrl in Shift)                  and
                   (Game.History.Count<Game.History.Top)
                                                      then BtnRedoAllClick         (BtnRedo)
                else                                       BtnRedoAllClick         (BtnRedoAll)
        else if Key=Ord(ACCEL_CHAR_SOLUTION        )  then
                if (ssCtrl  in Shift)                 and
                   (ssShift in Shift)                 then BtnSolutionClick        (BtnOpenNext)
                else                                       BtnSolutionClick        (BtnSolution)
        else if (Key=Ord(ACCEL_CHAR_SOLUTION_MOVES))  and
                (ssAlt in Shift)                      then BtnSolutionClick        (Self)
        else if (Key=Ord(ACCEL_CHAR_SOLUTION_PUSHES)) and
                (ssAlt in Shift)                      then BtnSolutionClick        (nil)
        else if Key=Ord(ACCEL_CHAR_STOP_CALCULATING_DEADLOCKS)
                                                      then BtnStatusMenuDeadlocksClick(Sender)
        else if Key=Ord(ACCEL_CHAR_GAME_MODE       )  then BtnReverseModeClick     (Sender)
        else if Key=Ord(ACCEL_CHAR_GAME_MODE_NORMAL)  then
                if Game.ReverseMode                   then BtnReverseModeClick     (Sender)
                else
        else if (Key=Ord(ACCEL_CHAR_REPLAY)         ) and
                (Shift=[])                            then BtnRedoAllClick         (BtnStatusMenuReplay)
        else if Key=Ord(ACCEL_CHAR_ROTATE_FLIP_BOARD_1) then
                if      ssCtrl in Shift               then PanelRotateAndFlipClick (PanelRotateCounterClockwise)
                else if ssAlt  in Shift               then PanelRotateAndFlipClick (PanelFlipHorizontally)
                else
        else if Key=Ord(ACCEL_CHAR_ROTATE_FLIP_BOARD_2) then
                if      ssCtrl in Shift               then PanelRotateAndFlipClick (PanelRotateClockwise)
                else if ssAlt  in Shift               then PanelRotateAndFlipClick (PanelFlipVertically)
                else
        else if Key=Ord(ACCEL_CHAR_ROTATE_MIRROR_BOARD_RESET) then
                if      (ssCtrl in Shift) or
                        (ssAlt  in Shift)             then PanelRotateAndFlipClick (PanelResetTransformations)
                else
        else if Key=Ord(ACCEL_CHAR_COPY_TO_CLIPBOARD) then begin
                if      (ssCtrl in Shift) or
                        (ssAlt  in Shift)             then begin
                        CopyLevelToClipboard(ssAlt in Shift,ssShift in Shift,False,False,False);
                        IgnoreKeyUp:=True;
                        end;
                end
        else if Key=Ord(ACCEL_CHAR_COPY_BOARD_TO_CLIPBOARD) then begin
                if      (ssCtrl in Shift) or
                        (ssAlt  in Shift)             then begin
                        CopyLevelToClipboard((ssAlt in Shift) and (not (ssCtrl in Shift)),ssShift in Shift,True,True,(ssAlt in Shift) and (ssCtrl in Shift));
                        IgnoreKeyUp:=True;
                        end
                end
        else if (Key=Ord(ACCEL_CHAR_COPY_SOLUTION_MOVES_TO_CLIPBOARD))  and
                (ssCtrl in Shift)                     and
                (Game<>nil) and (Status<>nil)         then begin
                if   Game.CopySnapshotToClipboard(Game.BestSolutionMoves,ssShift in Shift) then with Game.BestSolutionMoves do
                     Status.Hint:=Format(SolutionCopiedToClipboardText__,[MoveCount,PushCount]);
                IgnoreKeyUp:=True;
                end
        else if (Key=Ord(ACCEL_CHAR_COPY_SOLUTION_PUSHES_TO_CLIPBOARD))  and
                (ssCtrl in Shift)                     and
                (Game<>nil) and (Status<>nil)         then begin
                if   Game.BestSolutionPushes<>nil then with Game.BestSolutionPushes do
                     if   Game.CopySnapshotToClipboard(Game.BestSolutionPushes,ssShift in Shift) then
                          Status.Hint:=Format(SolutionCopiedToClipboardText__,[MoveCount,PushCount])
                     else
                else if   Game.CopySnapshotToClipboard(Game.BestSolutionMoves,ssShift in Shift) then with Game.BestSolutionMoves do
                          Status.Hint:=Format(SolutionCopiedToClipboardText__,[MoveCount,PushCount]);
                IgnoreKeyUp:=True;
                end
        else if Key=Ord(ACCEL_CHAR_COPY_MOVES_TO_CLIPBOARD) then
                if      (ssCtrl in Shift) or
                        (ssAlt  in Shift)             then begin
                        Game.CopyMovesToClipboard(False,ssShift in Shift);
                        IgnoreKeyUp:=True;
                        end
                else
        else if Key=Ord(ACCEL_CHAR_COPY_CONTINUATION_MOVES_TO_CLIPBOARD) then
                if      (ssCtrl in Shift) or
                        (ssAlt  in Shift)             then begin
                        Game.CopyMovesToClipboard(True,ssShift in Shift);
                        IgnoreKeyUp:=True;
                        end
                else
        else if Key=Ord(ACCEL_CHAR_TOOLS)             then
                if   (ssShift in Shift) and
                     (ssCtrl  in Shift) then
                     if (Game<>nil) and
                        (Game.FileName<>'') and
                        (ToolsForm<>nil)              then BtnToolsClick           (ToolsForm.EditMenuItemNew)
                     else {}
                else if ToolsForm<>nil                then BtnToolsClick           (ToolsForm.EditMenuItemOpen)
                     else
        else if Key=Ord(ACCEL_CHAR_EDIT)              then
                if ToolsForm<>nil                     then BtnToolsClick           (ToolsForm.TabSheetEditor)
                else
        else if Key=Ord(ACCEL_CHAR_SOLVER)            then BtnStatusMenuPluginClick(BtnStatusMenuSolver)
        else if Key=Ord(ACCEL_CHAR_SPLIT_VIEW)        then BtnSplitViewClick       (Sender)
        else if Key=Ord(ACCEL_CHAR_LOWER_BOUND)       then
                if   ssCtrl in Shift                  then CalculatePushesLowerBound(Clipboard)
                else                                       CalculatePushesLowerBound(Self)
        else if Key=Ord(ACCEL_CHAR_SCREENSHOT) then
                if      ssCtrl  in Shift              then CreateScreenshot
                else if ssShift in Shift              then CreateScreenshots
                else
        else if Key=VK_F1                             then BtnHelpClick            (Sender)
        else if Key=VK_F2                             then
                if   Shift=[ssCtrl,ssAlt]             then ShowBoardCoordinates:=not ShowBoardCoordinates
                else BtnOptionsClick(Sender)
        else if (Key=VK_F3) and
                (OptionsForm<>nil)                    then
                if Shift=[]                           then BtnSkinClick            (OptionsForm.MenuItemDefaultSkinRedBoxes)
                else                                       BtnSkinClick            (OptionsForm.MenuItemDefaultSkinYellowBoxes)
        else if Key=VK_F4                             then
                if           Shift   =  []            then BtnSkinClick            (BtnOpenNext)
                else if      ssAlt   in Shift         then // shutdown
                     else if ssShift in Shift         then begin
                                                           BtnSkinClick            (BtnOpenPrior);
                                                           IgnoreKeyUp:=True;
                                                           end
                          else
        else if Key=VK_F5                             then begin
                if      Shift=[]                      then InternetLevelLookup(False)
                else if ssShift in Shift              then begin IgnoreKeyUp:=True; // avoid changing focus to the 'Snapshots' window
                                                                 if        ssCtrl in Shift then CopyHuffmanBase64EncodedBoardToClipboard(True,ssAlt in Shift)
                                                                 else if   ssAlt  in Shift then
                                                                      else InternetLevelLookup(True);
                                                           end
                else if ssCtrl  in Shift              then CopyHuffmanBase64EncodedBoardToClipboard(False,ssAlt in Shift)
                else if ssAlt   in Shift              then ImportHuffmanBase64EncodedBoardFromClipboard
                end

        {$IFDEF MUSIC_PLAYER}

        else if (Key=VK_F5) and (Shift=[])            then BtnMPlayerClick         (Sender)
        else if Key=VK_F6                             then
                if Music.Player<>nil then begin
                   if Shift=[] then
                      if Music.Player.Mode=mpPlaying
                                                      then Music.Stop(True)
                      else                                 Music.Play
                   else
                      if MPlayer.Visible then              MPlayer.Stop
                      else                                 Music.Rewind;
                   if MPlayer.Visible                 then with MPlayer do begin
                      ShowButtons; ShowStatus;
                      end;
                   end
                else
        else if Key=VK_F7                             then begin
                if Shift=[]                           then Music.PrevTrack
                else                                       Music.PrevFrame;
                if MPlayer.Visible                    then MPlayer.ShowButtons;
                if MPlayer.Visible then with MPlayer do begin
                   ShowButtons; ShowStatus;
                   end;
                end
        else if Key=VK_F8                             then begin
                if Shift=[]                           then Music.NextTrack
                else                                       Music.NextFrame;
                if MPlayer.Visible then with MPlayer do begin
                   ShowButtons; ShowStatus;
                   end;
                end

        {$ENDIF}

        else if Key=VK_F9                             then
                if   (ssShift in Shift) or
                     (ssCtrl  in Shift) then
                     if (Game<>nil) and
                        (Game.FileName<>'') and
                        (ToolsForm<>nil)              then BtnToolsClick           (ToolsForm.EditMenuItemNew)
                     else {}
                else if ToolsForm<>nil                then BtnToolsClick           (ToolsForm.EditMenuItemOpen)
                     else
        else if Key=VK_F10                            then
                if Shift=[]                           then HeapStatistics
                else if   ssCtrl in Shift then
                          if ssAlt in Shift then
                             if SokoFile<>nil         then begin
                                SokoFile.MakeBoardFromMovesEnabled:=not SokoFile.MakeBoardFromMovesEnabled;
                                if Status<>nil then Status.Hint:='Load levels: Make board from moves: '+DisabledEnabledText[SokoFile.MakeBoardFromMovesEnabled];
                                end
                             else
                          else
                             if GameViewer.FrameTime>0 then
                                Msg(Format('Fps: %d',[GameViewer.FPS]),Application.Title,MB_OK+MB_ICONINFORMATION)
                             else
                     else if ssShift in Shift         then
                             with Game do Msg(Format('%d  %d  %d  %d',[TimeStatistics[0],TimeStatistics[1],TimeStatistics[2],TimeStatistics[3]]),Application.Title+SUB_TITLE_SEPARATOR+TimeStatisticsText,MB_OK{+MB_ICONINFORMATION})
                          else
        else if Key=VK_F11                            then
                if Shift=[]                           then MenuItemWindowSizeClick (MenuItemWindowSizeDefault)
                else                                       MenuItemWindowSizeClick (MenuItemWindowSizeDefaultCentered)
        else if Key=VK_F12                            then MenuItemWindowSizeClick (MenuItemWindowSizeMaximized)
        else if Key=VK_ESCAPE                         then begin
                if      MPlayer.Visible               then MPlayer.Hide
                else if Screen.ActiveForm=Self        then
                        case OnEscapeKeyAction of
                          oekaExitApplication :
                            if (Game<>nil) and
                               Game.SaveBestSolutionsAutomatically and
                               Game.SaveSnapshotsAutomatically
                                                      then BtnExitClick            (Sender)
                            else
                               if   Msg(DoYouWantToExitTheProgramText,TEXT_APPLICATION_TITLE_LONG,MB_YESNO+MB_DEFBUTTON2)=IDYES then
                                    BtnExitClick      (Sender);
                          oekaMinimize :
                            if   WindowState          <> wsMinimized then begin
                                 //WindowState        := wsMinimized;                // minimizes to screen, leaving a small window on the screen where only a titlebar is visible, and where there is room for the titlebar buttons but not for the title itself
                                 SendMessage(Self.Handle,WM_SYSCOMMAND,SC_MINIMIZE,0); // the intention is to minimize to taskbar (as opposed to minimize to screen), and this seems to work
                                 end;
                          oekaRestartGame :
                            BtnResetClick             (Sender);       
                          else begin end; // do nothing
                        end;
                end
        else if (Key>=Ord('1')) and (Key<=Ord('8'  )) and
                ((ssCtrl in Shift) or (ssAlt in Shift))
                                                      then SetBoardTransformation2D(TBoardTransformation2D(Ord(Key)-Ord('1')))
        else if Key=Ord(ACCEL_CHAR_NUM_UP          )  then Game.TryMove            (SokUtil_.Up   ,Game.History.Count,H_FLAG_BOX+MOVE_FLAG_KEYBOARD)
        else if Key=Ord(ACCEL_CHAR_NUM_DOWN        )  then Game.TryMove            (SokUtil_.Down ,Game.History.Count,H_FLAG_BOX+MOVE_FLAG_KEYBOARD)
        else if Key=Ord(ACCEL_CHAR_NUM_LEFT        )  then Game.TryMove            (SokUtil_.Left ,Game.History.Count,H_FLAG_BOX+MOVE_FLAG_KEYBOARD)
        else if Key=Ord(ACCEL_CHAR_NUM_RIGHT       )  then Game.TryMove            (SokUtil_.Right,Game.History.Count,H_FLAG_BOX+MOVE_FLAG_KEYBOARD)
        else if Key=Ord(ACCEL_CHAR_NUM_NEW         )  then BtnResetClick           (Sender)
        else if Key=Ord(ACCEL_CHAR_NUM_OPEN_PRIOR  )  then BtnOpenPriorOrNextClick (BtnOpenPrior)
        else if Key=Ord(ACCEL_CHAR_NUM_OPEN_PRIOR1 )  then BtnOpenPriorOrNextClick (BtnOpenPrior)
        else if Key=Ord(ACCEL_CHAR_NUM_REDO_ALL    )  then
                if (ssCtrl in Shift)                  and
                   (Game.History.Count<Game.History.Top)
                                                      then BtnRedoAllClick         (BtnRedo)
                else                                       BtnRedoAllClick         (BtnRedoAll)
        else if Key=Ord(ACCEL_CHAR_NUM_OPEN_NEXT   )  then BtnOpenPriorOrNextClick (BtnOpenNext)
        else if Key=Ord(ACCEL_CHAR_NUM_OPEN_NEXT1  )  then BtnOpenPriorOrNextClick (BtnOpenNext)
        else if (Key=Ord(ACCEL_CHAR_NUM_UNDO1      )) or
                (Key=Ord(ACCEL_CHAR_NUM_UNDO2      )) then
                if (ssCtrl in Shift)                  and
                   (Game.History.Count>Game.ForcedInitialJumps)
                                                      then BtnRedoAllClick         (BtnUndo)
                else                                       BtnUndoClick            (Sender)
        else if Key=Ord(ACCEL_CHAR_NUM_REDO        )  then
                if (ssCtrl in Shift)                  and
                   (Game.History.Count<Game.History.Top)
                                                      then BtnRedoAllClick         (BtnRedo)
                else                                       BtnRedoClick            (nil)
        else if Key=VK_TAB                            then
        else if (Key=VK_SHIFT)    and IgnoreShiftKey  then IgnoreKeyUp:=True
        else if (Key=Ord(SPACE))  and // backdoor to enable/disable colorcycling; caution: use it on your own risk, fractals colorcycling is not stable; see note in 'Fractals_'
                (ssCtrl in Shift) and (ssShift in Shift) and
                (Fractals<>nil)   and (Mandala<>nil)  and
                (MPlayer<>nil)    and MPlayer.Visible and
                (MPlayer.Display<>nil)
                and (MPlayer.Display.Activity=acFractals) then with Fractals do begin
                ColorCyclingEnabled:=not ColorCyclingEnabled;
                ColorCycling:=ColorCyclingEnabled;
                end;
        end;

     LastKey:=Key;
     end
  else begin // simulated keyboard event
     //Caption:=TEXT_APPLICATION_TITLE+' - Simulated keyboard event down '+IntToStr(GetTimeMS);
     end;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Game<>nil) and (Key<>SIMULATED_KEYBOARD_EVENT_KEY) then begin
     Game.MoveCountSinceLastKeyUpEvent:=0;

     if not Game.IsBusy then begin // '(not Game.IsBusy)': *not*: '(Game.IsIdleAndStopReplayingAndBrowsing)'!

        if         (MPlayer<>nil) and MPlayer.Visible then //
        else if    Key=Ord(ACCEL_CHAR_SNAPSHOTS) then
                   if      (Shift=[])       and (CtrlEnterKeySet=[ceEnter]) then
                           if   (SnapshotsForm<>nil) and
                                SnapshotsForm.Visible and
                                SnapshotsForm.PanelNew.Enabled and
                                (SnapshotsForm.Snapshots[SnapshotsForm.Grid.Row]<>nil) then
                                BtnSnapshotsClick(nil)
                           else BtnResetClick(Sender)
                   else if (Shift=[ssCtrl]) and (ceCtrl in CtrlEnterKeySet) then BtnSnapshotsClick(Self)
                   else
        else if    (Key=VK_TAB) and (SnapshotsForm<>nil) and (BtnSnapshots.Enabled) then begin
                   if   Assigned(MultiView.Selected) and Game.IsIdleAndStopReplayingAndBrowsing then
                        try     Screen.Cursor:=crHourGlass;
                                MultiView.Sort(scLeft); MultiView.Sort(scTop);
                                if   ssShift in Shift then
                                     MultiView.Selected:=TMultiViewItem(MultiView.Items.PrevWithWrapAround(MultiView.Selected))
                                else MultiView.Selected:=TMultiViewItem(MultiView.Items.NextWithWrapAround(MultiView.Selected))
                        finally if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
                        end
                   else if   Shift=[] then
                             if   not SnapshotsForm.Visible then SnapshotsForm.Show
                             else SnapshotsForm.SetFocus;
                   end
        else if    Key=VK_SHIFT then begin
                   if not IgnoreKeyUp then
                      if   Shift=[] then begin
                           if (SnapshotsForm<>nil) and (BtnSnapshots.Enabled) then begin
                              if   not SnapshotsForm.Visible then SnapshotsForm.Show
                              else SnapshotsForm.SetFocus;
                              end;
                           end
                      else BtnReverseModeClick(Sender);
                   IgnoreShiftKey:=False;
                   end
        else if    Key=Ord(ACCEL_CHAR_BOOKMARKS) then
                   if (Shift=[]) and (CtrlEnterKeySet=[ceEnter]) then BtnBookmarksClick(nil)
                   else
        else;

        end;
     end;

  if      Key=VK_RETURN  then
          CtrlEnterKeySet:=CtrlEnterKeyset-[ceEnter,ceCtrlUpWhileEnterDown]
  else if Key=VK_CONTROL then
          if   ceEnter in CtrlEnterKeySet then
               CtrlEnterKeySet:=CtrlEnterKeyset-[ceCtrl]+[ceCtrlUpWhileEnterDown]
          else CtrlEnterKeySet:=CtrlEnterKeyset-[ceCtrl , ceCtrlUpWhileEnterDown]
  else if Key=SIMULATED_KEYBOARD_EVENT_KEY then begin
          //Caption:=Caption+' up '+IntToStr(GetTimeMS);
          end;
end;

function  TMainForm.CloseLevel(Sender: TObject):Boolean;
var i:Integer; s,s2:String;
begin
  Game.StopTimer;
  Result:=Game.IsIdleAndStopReplayingAndBrowsing;
  if Result then
     if Modified
//      or
//      ((Sender=EditForm ) and EditForm .Modified)
        or
        ((Sender=ToolsForm) and Assigned(ToolsForm) and ToolsForm.Modified)
        or
        ((not IsBlank(Self.Game.FileName)) and IsANewFileName(Self.Game.FileName) and Assigned(ToolsForm) and (Sender<>ToolsForm.GeneratorEditMenuItemEdit))
        then begin
        if   Assigned(ToolsForm) and (Sender=ToolsForm.GeneratorEditMenuItemEdit) then Sender:=ToolsForm;
        if   Sender=ToolsForm then s2:=ToolsForm.Editor.FileName
        else s2:=Self.Game.FileName;
        if   IsANewFileName(s2) then s:=ChangedNewText
        else s:=ChangedText;
        if   (Sender=BtnOptions) or ShutDownApplication then i:=MB_YESNO
        else i:=MB_YESNOCANCEL;
        case Msg(s,
                 Application.Title+SUB_TITLE_SEPARATOR+VisualFileName(s2),
                 i+MB_ICONQUESTION) of
          IDYES    : begin //if Sender=EditForm then
                           //   Result:=EditForm.IsALegalLevel;
                           if Sender=ToolsForm then
                              Result:=ToolsForm.IsALegalLevel(True,True,s);
                           if Result then Result:=Save(Sender);
                           if Result then Result:=Modified=False;
                     end;
          IDNO     : begin //if (EditForm<>nil) and EditForm.Visible then begin
                           //   EditForm.Modified:=False;
                           //   EditForm.FileName:='';
                           //   EditForm.ShowTitle;
                           //   EditForm.ShowStatus;
                           //   EditForm.DrawGrid1.Hide;
                           //   end;

                           if ToolsForm<>nil then with ToolsForm do begin
                              Modified:=False; PluginLevelInfo.NewGamesCount:=0;
                              end;

                           // save the current position in 'OpenForm.Game';
                           // this information is required in case the editor
                           // is trying to create a snapshot of the current
                           // position
                           OpenForm.Game.BoardWidth :=Game.BoardWidth;
                           OpenForm.Game.BoardHeight:=Game.BoardHeight;
                           OpenForm.Game.Board      :=Game.Board;
                           OpenForm.Game.SokoFileName:=''; // ensure that 'OpenForm.Game' doesn't try to save itself later; this is probably not necessary but it doesn't hurt
                           Result:=OpenForm.Game.SetName(Game.FileName);
                           if ToolsForm<>nil then
                              ToolsForm.ClearPluginReplayInfo;

                           if IsANewFileName(Game.FileName) then begin
                              if StrEqual(Game.FileName,Game.SokoFile.Name) then begin
                                 Game.SokoFile.Clear; Game.SokoFile.SetName('');
                                 end;
                              end;
                           MultiView.Clear;
                           Game.Clear; Modified:=False; ShowTitle('');
                           CurrentImageClientWidth:=0;
                           FormResize(Self); // removes the 'Modified' panel from the statusbar
                           ShowStatus;

                           if (ToolsForm<>nil) and ToolsForm.Visible then begin
                              if ToolsForm.Editor.History<>nil then
                                 ToolsForm.Editor.History.EndTransaction(False);
                              ToolsForm.EditMenuItemNewClick(BtnOpen);
                              end;
                     end;
          IDCANCEL : Result:=False;
        end; // case
        end
     else if (not IsBlank(Game.FileName)) then begin
             if Result then Result:=Game.CloseLevel(True);
             if Result then Modified:=False;
             end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var CandidateSetFileName:String;
begin
  if   Assigned(Generator) then CandidateSetFileName:=Generator.FileName
  else CandidateSetFileName:='';

  if   Assigned(ToolsForm) and ToolsForm.Visible and (not ShutDownApplication) then begin
       // catch this scenario: Open the "Tools" window, right-click the
       // application's icon on the taskbar, and click "Close (Alt+F4)".
       // why Windows allows the user to issue this command from the taskbar
       // is unknown. None of the other modal windows have this problem;
       Msg(PleaseCloseTheToolsWindowBeforeYouCloseTheApplicationText,Application.Title,0);
       CanClose:=False;
       end
  else begin CanClose:=CloseLevel(BtnSave) or ShutdownApplication;
             if CanClose and Assigned(Generator) then begin
                CanClose:=Generator.Shutdown or ShutdownApplication;
                if (not CanClose) and Assigned(Game) and (Game.GameState=gsNull) then
                   if   Game.LastValidFileName<>'' then
                        LoadGame(Game.LastValidFileName,False)
                   else InitGame(False,True,True,False,True,True,True,0);
                end;
       end;

  if CanClose and Assigned(Generator) and (Generator.FileName='') and
     (not IsANewFileName(CandidateSetFileName)) and FileExists(CandidateSetFileName) then
     Generator.FileName:=CandidateSetFileName; // restore the filename of the last opened file so it's available for 'TGenerator.SaveSettingsToIniFile()'
end;

procedure TMainForm.SetTrackState(TrackState__:TTrackState);
begin
  if fTrackState<>TrackState__ then begin
     fTrackState:=TrackState__;
     if   fTrackState=tsTrack then
          MouseOverPlayerStartTime:=MOUSE_TRACKING_STARTED;
     if DrawGrid1.Visible then DrawGrid1.Invalidate;
     end;
end;

procedure TMainForm.DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var ACol,ARow:Integer; Direction:TDirection;

  function MovePlayer(Col__,Row__:Integer):Boolean;
  var MoveCount,PlayerLineCount:Integer; IsPreferredDirectionOK,oOptimizeMovesBetweenPushes:Boolean;
      Position:TColRow; // Moves:TMoves;
  begin
    Position.X:=Col__; Position.Y:=Row__;
    Result:=MouseTrackingModeEnabled and
            (TrackBox.X=0) and (TrackBox.Y=0) and
            (Col__>=1) and (Col__<=Game.BoardWidth) and
            (Row__>=1) and (Row__<=Game.BoardHeight) and
            ((Game.Board[Col__,Row__] and (FLOOR+BOX))=FLOOR) and
            Game.PlayerPath(Game.PlayerPos,Position,False,(Game.History.Count>0) and (not (OptimizeMovesBetweenPushes and ((Game.History.Moves[Game.History.Count] and H_FLAG_BOX)=0))),TDirection(Game.History.Moves[Game.History.Count] and H_MASK_DIRECTION),MoveCount,PlayerLineCount,IsPreferredDirectionOK,PPlayerMoves(Addr(MovesOrHistory.Moves))); // try to find a path from the player position to the floor; 'not (OptimizeMovesBetweenPushes...': if in-between moves optimization is enabled and the latest move was a non-pushing player move, then show the path that visibly seems to be moves/lines optimal, and rely on the in-between moves optimizer to substitute it with the true best path
    if Result then begin
       if MPlayer.Visible then MPlayer.Hide;
       TrackBox.X:=0; TrackBox.Y:=0;
       LastGameAction:=gaNull;
       oOptimizeMovesBetweenPushes:=MainForm.OptimizeMovesBetweenPushes;
       try
         MainForm.OptimizeMovesBetweenPushes:=True;
         Game.TryMoves(MoveCount,MovesOrHistory.Moves);
       finally
         MainForm.OptimizeMovesBetweenPushes:=oOptimizeMovesBetweenPushes;
       end;
       TrackState:=tsTrack;
       end
  end;

begin // DrawGrid1MouseMove
  if not Game.IsBusy then begin
     if MPlayer.Visible and MPlayer.MouseMove(Sender,[],X,Y) then //
     else begin
        if RotateAndFlipPopupMenu.Visible then with PanelRotateAndFlip do
           if ((X<Left) or (Y<Top) or (X>=Left+Width) or (Y>=Top+Height)) and
              RotateAndFlipPopupMenu.MouseHasBeenOverMenu then
              RotateAndFlipPopupMenu.Hide; // close popup menu when the mouse isn't over it anymore
//         else
//      else if (Menu.BtnRotateIndex>=0) and
//              PtInRect(Menu.MenuItems[Menu.BtnRotateIndex].Rect,Point(X,Y)) then
//              BtnRotateClick(Sender); // open popup menu when the mouse is over the button

        if MultiViewPopupMenu.Visible then with PanelMultiViewMenu do
           if ((X<Left) or (Y<Top) or (X>=Left+Width) or (Y>=Top+Height)) and
              MultiViewPopupMenu.MouseHasBeenOverMenu then
              MultiViewPopupMenu.Hide; // close popup menu when the mouse isn't over it anymore

        if Game.GameState=gsPlay then begin
           if      Image1.Visible then
                   GameViewer.MouseToCell(X,Y,ACol,ARow)
           else if DrawGrid1 .Visible then
                   DrawGrid1 .MouseToCell(X,Y,ACol,ARow)
           else begin ACol:=-2; ARow:=-2; end;
           Inc(ACol); Inc(ARow);

           if ShowBoardCoordinates and
              (ACol>=1) and (ACol<=Game.BoardWidth) and (ARow>=1) and (ARow<=Game.BoardHeight) and
              (X>=GameViewer.Left) and (Y>=GameViewer.Top) then
              //MainForm.Status.Hint:=Format('[%d,%d] - %d',[ACol,ARow,Game.Board[ACol,ARow]]);
              MainForm.Status.Hint:=Format('[%d,%d]',[ACol,ARow]);

           if   (ACol=Game.PlayerPos.X) and
                (ARow=Game.PlayerPos.Y) then begin   // mouse over player
                if   (TrackState=tsWait)    and
                     (TrackBox.X=0)         and      // 'drag-and-drop' and 'lift-and-drop' has precedence
                     (not MPlayer.Visible)  and
                     MouseTrackingModeEnabled then
                     if   (MouseOverPlayerStartTime=MOUSE_TRACKING_STARTED) then
                          TrackState:=tsTrack
                     else if        MouseOverPlayerStartTime=MOUSE_TRACKING_NOT_STARTED then // do nothing
                          else if   MouseOverPlayerStartTime=MOUSE_TRACKING_START_TIMER then begin
                                    MouseOverPlayerStartTime:=GetTickCount;
                                    if MouseOverPlayerStartTime<=MOUSE_TRACKING_STARTED then
                                       MouseOverPlayerStartTime:=MOUSE_TRACKING_STARTED+1;   // skip the special values
                                    end
                               else if   (Cardinal(GetTickCount) >
                                          Cardinal(MouseOverPlayerStartTime +
                                                   DELAY_MOUSE_TRACKING_MILLI_SECS))
                                         or
                                         (Cardinal(GetTickCount) <  // clock wrap-around
                                          Cardinal(MouseOverPlayerStartTime)) then
                                         TrackState:=tsTrack // delay expired; start tracking
                                    else
                else;
                end
           else begin                             // mouse not over the player
                  if   TrackState=tsSit then
                       //TrackState:=tsWait       // prepare to track the mouse next time it is over the player
                  else if TrackState=tsWait then MovePlayer(ACol,ARow)
                  else if TrackState=tsTrack then
                          if DxDyToDirection(ACol-Game.PlayerPos.X,ARow-Game.PlayerPos.Y,Direction) then
                             if   ceCtrl in CtrlEnterKeySet then // [Ctrl] down: don't drag a box in reverse mode
                                  if   Game.TryMove(Direction,Game.History.Count,0+MOVE_FLAG_KEYBOARD) then //
                                  else TrackState:=tsWait
                             else if   Game.TryMove(Direction,Game.History.Count,H_FLAG_BOX+MOVE_FLAG_KEYBOARD) then //
                                  else TrackState:=tsWait
                          else
                             TrackState:=tsWait;
                  if MouseOverPlayerStartTime<>MOUSE_TRACKING_STARTED then
                     if   Game.History.Count>Game.ForcedInitialJumps then
                          MouseOverPlayerStartTime:=MOUSE_TRACKING_STARTED      // start tracking the mouse immidiately the next time the mouse is over the player
                     else MouseOverPlayerStartTime:=MOUSE_TRACKING_START_TIMER; // from the start position, delay tracking mode a short period, so moving the mouse doesn't start tracking mode by accident
                end;
           end
        else if TrackState=tsTrack then TrackState:=tsWait;
        end;
     end;

  if (not MultiView.IsEmpty) and
     (X>=GraphicMenuWidth) and (Y<Status.RectForm.Top-Status.TopMargin) and
     (not Game.IsBusy) then
     MultiView.OnMouseMove(Sender,Shift,X,Y);

  SetCursor(X,Y);

  if FirstTime then begin // 'True': this is the first time the application is activated after installation or re-installation;
     FirstTime:=False;    // kludge: Windows XP won't do a "ShowModal" before the main window is visible;
     BtnHelpClick(Self);  // therefore, the help window is shown here instead of in 'ApplicationOnActivate'
     if Status<>nil then begin
        Status.HintCount:=0; Status.Hint:=WelcomeText;
        end;
     end;
end;

procedure TMainForm.ClearTrackBox(HidePopupMenu:Boolean);
begin
  if (GameViewer<>nil) and
     ((GameViewer.LegalMovesInfo.Mask<>0)
      or
      GameViewer.LegalMovesInfo.BoxCursor.Enabled
      or
      GameViewer.LegalMovesInfo.PlayerCursor.Enabled
     ) then
     GameViewer.HideLegalMoves;

  if HidePopupMenu then begin
     if (RotateAndFlipPopupMenu<>nil) and RotateAndFlipPopupMenu.Visible then
        RotateAndFlipPopupMenu.Hide;
     if (MultiViewPopupMenu<>nil) and MultiViewPopupMenu.Visible then
        MultiViewPopupMenu.Hide;
     end;

  if TrackBox.X<>0 then begin
     TrackBox.X:=0; TrackBox.Y:=0;
     UpdateCursor;
     if Image1   .Visible then Image1   .Invalidate;
     if DrawGrid1.Visible then DrawGrid1.Invalidate;
     end;

  //MultiView.HideDragRect;

  LastGameAction:=gaNull; LastKey:=0;
end;

procedure TMainForm.DrawGrid1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var ACol,ARow,MoveCount,PlayerLineCount:Integer; IsPreferredDirectionOK:Boolean; Position:TColRow; // Moves:TMoves;
begin
  LastKey:=0;

  if      (Sender=Image1) and (Source=Image1) then
          GameViewer.MouseToCell(X,Y,ACol,ARow)
  else if (Sender=DrawGrid1) and (Source=DrawGrid1) then
          DrawGrid1.MouseToCell(X,Y,ACol,ARow)
  else if (Sender=Image1) and (SnapshotsForm<>nil) and (Source=SnapshotsForm.Grid) then begin
          ClearTrackBox(True);
          SnapshotsForm.PanelOpenClick(Sender);
          end
  else ClearTrackBox(True);
  if (TrackBox.X<>0) and (TrackBox.Y<>0) then begin
     Inc(ACol); Inc(ARow); Position.x:=ACol; Position.y:=ARow;
     if (ACol<=Game.BoardWidth) and (ARow<=Game.BoardHeight) and
        ((Game.Board[ACol,ARow] and (WALL+BOX))=0) then
        if (
            (
             ((TrackBox.X=Game.PlayerPos.X) and (TrackBox.Y=Game.PlayerPos.Y))
             and
             (Game.PlayerPath(Game.PlayerPos,Position,False,(Game.History.Count>0) and (not (OptimizeMovesBetweenPushes and ((Game.History.Moves[Game.History.Count] and H_FLAG_BOX)=0))),TDirection(Game.History.Moves[Game.History.Count] and H_MASK_DIRECTION),MoveCount,PlayerLineCount,IsPreferredDirectionOK,PPlayerMoves(Addr(MovesOrHistory.Moves))) // 'not (OptimizeMovesBetweenPushes...': if in-between moves optimization is enabled and the latest move was a non-pushing player move, then show the path that visibly seems to be moves/lines optimal, and rely on the in-between moves optimizer to substitute it with the true best path
              or
              (Game.ReverseMode
               and
               Game.PlayerJump(Game.PlayerPos,Position,True,MoveCount,PlayerLineCount,MovesOrHistory.Moves)
              )
             )
            )
            or
            (
             ((TrackBox.X<>Game.PlayerPos.X) or (TrackBox.Y<>Game.PlayerPos.Y))
             and
             Game.BoxPath(TrackBox,Position,Game.PlayerPos,MoveCount,MovesOrHistory.Moves)
            )
           ) then begin
           if MPlayer.Visible then MPlayer.Hide;
           TrackState:=tsWait; ClearTrackBox(True);
           Game.TryMoves(MoveCount,MovesOrHistory.Moves);
           end
        else begin
           if (Game.GameState=gsPlay) and
              Sound.Enabled and
              (Sound.Player[stBlockedStep]<>nil) then
              Sound.Play(stBlockedStep);
           if (ACol<>TrackBox.X) or (ARow<>TrackBox.Y) then ClearTrackBox(True);
           end
     else
        if (ACol<>TrackBox.X) or (ARow<>TrackBox.Y) then ClearTrackBox(True);
     end;
end;

procedure TMainForm.DrawGrid1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=((Sender=Image1            ) and (Source=Image1   ) and (TrackBox.X<>0) and (TrackBox.Y<>0))
          or
          ((Sender=DrawGrid1         ) and (Source=DrawGrid1) and (TrackBox.X<>0) and (TrackBox.Y<>0))
          or
          ((Sender=Image1            ) and (SnapshotsForm<>nil) and
           (Source=SnapshotsForm.Grid) and (SnapshotsForm.Snapshots[SnapshotsForm.Grid.Row]<>nil) and
           (X>=GraphicMenuWidth) and (Status <>nil) and (Y<Status.RectForm.Top-Status.TopMargin)
          );
end;

procedure TMainForm.ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if   (Msg.Message = WM_MOUSEWHEEL) then
       if   (MPlayer<>nil) and
            MPlayer.Visible and
            (MPlayer.Display<>nil) then begin
            if (MPlayer.Display.Activity=acImage) then begin
               if Msg.WParam>=0 then BtnViewer1PriorClick(Self)
               else                  BtnViewer1NextClick (Self);
               Handled:=True;
               end
            else if (MPlayer.Display.Activity=acFractals) then begin
               if Msg.WParam>=0 then BtnFractalsNextClick(Self)
               else                  BtnFractalsPriorClick (Self);
               Handled:=True;
               end;
            end
       else if Msg.WParam>=0 then DrawGrid1MouseWheelUp  (Self,[],Point(0,0),Handled)
            else                  DrawGrid1MouseWheelDown(Self,[],Point(0,0),Handled)
  else if (Msg.Message = WM_SYSCOMMAND) and
          (((Msg.wParam and $fff0) = SC_SCREENSAVE) {or ((Msg.wParam and $fff0) = SC_MONITORPOWER)})
          and
          (not ScreenSaverEnabled) then
          Handled := True // disable screensaver {and 'monitor power off'}
  else Handled:=False;
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Screen.Cursor<>DEFAULT_CURSOR then MultiView.SetCursor(X,Y);
end;

procedure TMainForm.SetCursor(X,Y:Integer);
begin
  if   (SnapshotsForm<>nil) and
       (SnapshotsForm.LastFocusedControl<>nil) then SnapshotsForm.FormMouseMove(nil,[],0,0);
  if   PtInRect(GameViewer.BoardRect,Point(X,Y)) and
       (not MultiView.IsDragging) and
       (not MultiView.IsSizing) and
       (not MPlayer.Visible) and
       (Screen.ActiveForm=Self) then begin
       if   TrackBox.X<>0 then
            if   Screen.Cursor<>DragCursor then
                 Screen.Cursor:=DragCursor
            else
       else if   TrackState=tsTrack then
                 if   Screen.Cursor<>TrackingCursor then
                      Screen.Cursor:=TrackingCursor
                 else
       else if   Screen.Cursor<>SelectionCursor then
                 Screen.Cursor:=SelectionCursor
            else;
       if Menu<>nil then Menu.ItemIndex:=-1;
       if (Status<>nil) and
          ((Status.Panels[spReplaySpeed].RectForm.Top<Status.Panels[spReplaySpeed].RectForm.Bottom)
           or
           (Status.Panels[spBrowse     ].RectForm.Top<Status.Panels[spBrowse     ].RectForm.Bottom)
          ) then
          Status.MouseMove(X,Y); // hide dynamic panels, if any
       if (StartHintCount<>0) and (Status<>nil) and (Status.HintCount>2) and
          (Game.GameState=gsPlay) and (not Game.IsBusy) and (not Game.IsBrowsing) and (not Game.IsReplaying) then begin
          if (GameViewer<>nil) and GameViewer.LegalMovesInfo.Enabled then
             if      StartHintCount=1 then begin // 'True': show the first tip
                     if GameViewer.LegalMovesInfo.Mask=0 then
                        Status.Hint:=WallHintText;
                     StartHintCount:=Status.HintCount;
                     end
             else if StartHintCount<>Status.HintCount then begin // 'True': the current hint is different from the first tip; show the second tip
                     if (GameViewer.LegalMovesInfo.Mask=0) and
                        (OnLeftClickEmptySquareAction=olcesaShowBoxesThatCanGoToSquare) then
                        Status.Hint:=FloorHintText;
                     StartHintCount:=0;
                     end;
          end;
       end
  else begin
         if Screen.Cursor<>DEFAULT_CURSOR then
            if   MPlayer.Visible and
                 (MPlayer.Display.Activity=acFractals) and
                 (Screen.Cursor<>crDefault) and
                 (Screen.Cursor<>DEFAULT_CURSOR) then //
            else if   (Screen.Cursor=DragCursor) and
                      (not MultiView.IsDragging) and
                      (Status<>nil) and
                      ((Status.Panels[spBrowse     ].RectForm.Top<Status.Panels[spBrowse     ].RectForm.Bottom)
                       or
                       (Status.Panels[spReplaySpeed].RectForm.Top<Status.Panels[spReplaySpeed].RectForm.Bottom)
                      ) then //
                 else MultiView.SetCursor(X,Y);
         if MPlayer.Visible then MPlayer.MouseMove(Self,[],X,Y);
         if Menu<>nil then
            if (X<GraphicMenuWidth) or (Y>GameViewer.BoardRect.Bottom) then begin
               if Menu.SkipMouseOverSoundForItemIndex=-1 then
                  Menu.SkipMouseOverSoundForItemIndex:=Menu.ItemIndex;
               Menu.MouseMove(X,Y);
               end
            else Menu.ItemIndex:=-1;
         if Status<>nil then Status.MouseMove(X,Y);
       end;
end;

procedure TMainForm.UpdateCursor;
var Point:TPoint;
begin
  Point:=ScreenToClient(Mouse.CursorPos);
  SetCursor(Point.X,Point.Y);
end;

procedure TMainForm.SetBevelVisible(BevelVisible__:Boolean);
begin
  if BevelVisible<>BevelVisible__ then begin
     fBevelVisible:=BevelVisible__;
     FormResize(Self);
     end;
end;
{
procedure  TMainForm.DrawBevelRect(const R:TRect);
var oColor:TColor;
begin
  with Image1.Picture.BitMap.Canvas do begin
    oColor:=Brush.Color;
    Brush.Color:=clGray ; FrameRect(Rect(R.Left+1,R.Top+1,R.Right,R.Bottom));
    Brush.Color:=clWhite; FrameRect(Rect(R.Left,R.Top,R.Right-1,R.Bottom-1));
    Brush.Color:=oColor;
    end;
end;
}
procedure TMainForm.BtnOpenPriorOrNextClick(Sender: TObject);
begin
  if Game.IsIdleAndStopReplayingAndBrowsing then
     OpenPriorOrNext(((Sender=BtnOpenPrior) or (Sender=MenuItemOpenPreviousLevel)),(ceCtrl in CtrlEnterKeySet),False,VK_CONTROL);
end;

function TMainForm.OpenPriorOrNext(Prior,Unsolved,Solved:Boolean; VirtualKeyCode:Integer):Boolean;
{if a non-zero 'VirtualKeyCode' argument is supplied (typically 'VK_CONTROL'),
 then the search only runs as long as this key is pressed;
 if 'VirtualKeyCode' is zero, then the search can be stopped by pressing
 [Escape], [Enter], [Space] or [Ctrl];
}
var Count,ItemIndex:Integer; FileName,PackFileName,SectionName:String; oCursor:TCursor;

  function  TryToFindASokobanFile(var FileName:String; var ItemIndex__:Integer):Boolean;
  var Count:Integer; s:String;
  begin
    s:=FileName; Count:=0;
    repeat s:=OpenForm.FindPriorNextFileName(s,Prior,ItemIndex__); Inc(Count);
           Result:=(not IsBlank(s)) and OpenForm.Game.SokoFile.IsASokobanFile(s);
           if Result then FileName:=s;
    until  Result or (s='') or StrEqual(s,FileName) or (Count>OpenForm.FileListBox1.Items.Count);
    if not Result then FileName:='';
  end;

  function  LoadGame2(var FileName:String):Boolean;
  var b:Boolean;
  begin
    OpenForm .Game.Clear;                                     // so 'ToolsForm.Game' doesn't try to update 'OpenForm.Game';
    ToolsForm.Game.Clear;
    ToolsForm.Game.HourGlassCursor:=True;
    ToolsForm.Game.Verbose:=True;
    ToolsForm.Game.SokoFileName:='';                          // avoid that 'ToolsForm' tries to save its own game
    ToolsForm.Game.FileName:=FileName;                        // transfer current filename to 'OpenForm', so it can take current path from this filename
    ToolsForm.Game.SolutionsRequireAtLeastOnePush:=True;      // so all snapshots, solutions, and savegame are loaded;
    Result:=ToolsForm.Game.LoadFromFileOrClipboard(FileName,nil,nil,b);
    if Result then begin
       FileName:=ToolsForm.Game.FileName;
       ToolsForm.Game.SolutionsRequireAtLeastOnePush:=Game.SolutionsRequireAtLeastOnePush;
       ToolsForm.Game.CalculateScoreAndState;
       end;
  end;

begin // OpenPriorOrNext
  Result:=False; ClearTrackBox(True); Count:=0; ItemIndex:=-1;
  if MPlayer.Visible then MPlayer.Hide;
  FileName:=Game.FileName;
  if IsANewFileName(FileName) then FileName:=Game.LastValidFileName;
  if Deadlocks<>nil then Deadlocks.Suspend;
  oCursor:=Screen.Cursor;
  try
    Screen.Cursor:=crHourGlass;

    if (FileName<>'') and CloseLevel(BtnSave) and (Status<>nil) then begin
       FileName:=Game.FileName;
       if IsANewFileName(FileName) then FileName:=Game.LastValidFileName;
       repeat
         if   IsAnIniFileSectionFileName(FileName) then begin
              PackFileName:=ExtractIniFileName(FileName);
              SectionName :=ExtractSectionName(FileName);
              if SokoFile.Open(PackFileName) then
                 if SokoFile.FindPriorOrNextLevelName(SectionName,Prior,False) then
                    FileName:=MakeIniFileSectionFileName(PackFileName,SectionName)
                 else begin
                   FileName:=PackFileName; TryToFindASokobanFile(FileName,ItemIndex);
                   end;
              end
         else if FileName<>'' then begin
                 TryToFindASokobanFile(FileName,ItemIndex);
                 end;

         if   FileName<>'' then begin
              if Prior and (not IsAnIniFileSectionFileName(FileName)) then
                 if Prior then FileName:=MakeIniFileSectionFileName(FileName,SECTION_NAME_LAST);

              if Unsolved or Solved then begin
                 Result:=LoadGame2(FileName);

                 if Result then begin
                    Inc(Count);
                    if      ToolsForm.Visible then
                            ToolsForm.StatusBar1.Panels[1].Text:=IntToStr(Count)+SPACE+ToolsForm.Game.DisplayName
                    else    Status.Hint:=IntToStr(Count)+SPACE+ToolsForm.Game.DisplayName;

                    try     Game.IsBusy:=True;
                            Application.ProcessMessages; // in order to allow the user to let go of the [Ctrl]-button.
                    finally Game.IsBusy:=False;
                    end;

                    if //(not IsKeyPressed(VK_CONTROL))
                       //(not (ceCtrl in CtrlEnterKeySet))
                       ((VirtualKeyCode<>0)
                        and
                        (not IsKeyPressed(VirtualKeyCode))
                       )
                       or
                       ((VirtualKeyCode= 0)
                        and
                        (IsKeyPressed(VK_ESCAPE) or IsKeyPressed(VK_RETURN) or IsKeypressed(VK_SPACE) or IsKeyPressed(VK_CONTROL))
                       )
                       or
                       (Unsolved
                        and
                        (ToolsForm.Game.BestSolutionMoves=nil)
                        and
                        ((ToolsForm.Game.GameState<>gsSolved)
                         or
                         Game.SolutionsRequireAtLeastOnePush
                        )
                       )
                       or
                       (Solved
                        and
                        ((ToolsForm.Game.BestSolutionMoves<>nil)
                         or
                         ((ToolsForm.Game.GameState=gsSolved)
                          and
                          (not Game.SolutionsRequireAtLeastOnePush)
                         )
                        )
                       )
                       or
                       StrEqual(FileName,Game.FileName) then begin
                       Unsolved:=False; Solved:=False; // force the 'if'-statement below to load this level
                       end;
                    end;
                 end;

              if not (Unsolved or Solved) then begin // 'True': load this level
                 Result:=LoadGame(FileName,True);
                 if Self.Visible then
                    if      Image1   .Visible then Self     .SetFocus
                    else if DrawGrid1.Visible then DrawGrid1.SetFocus;
                 end;
              end;
       until  (not Result) or (not (Unsolved or Solved));
       end;
  finally
    Screen.Cursor:=oCursor;
    if Deadlocks<>nil then Deadlocks.Resume;
  end;

  if not Result then Game.LastValidFileName:='';
  ShowStatus;
//if (EditForm <>nil) and EditForm .Visible then EditForm.ShowStatus;
  if (ToolsForm<>nil) and ToolsForm.Visible then ToolsForm.ShowStatus;
end;

procedure TMainForm.Image1DblClick(Sender: TObject);
var MouseCursorPos:TPoint;

  function IsAFloorSquare(Point:TPoint):Boolean;
  var Col,Row:Integer;
  begin
    GameViewer.MouseToCell(Point.X,Point.Y,Col,Row);
    Result:=(Col>=0) and (Col<Game.BoardWidth) and (Row>=0) and (Row<Game.BoardHeight) and
            ((Game.Board[Succ(Col),Succ(Row)] and FLOOR)<>0)
  end;

begin {ImageDblClick}
  MouseCursorPos:=Image1.ScreenToClient(Mouse.CursorPos);
  if      MPlayer.Visible or Game.IsBusy or Game.IsReplaying or Game.IsBrowsing or
          IsAFloorSquare(MouseCursorPos) then // do nothing
  else if (Game.GameState=gsSolved) or
          (Game.GameState=gsPlay) then
          if (MouseCursorPos.X>=GraphicMenuWidth) or
             (MouseCursorPos.Y>=Status.RectForm.Top-Status.TopMargin) or
             (Menu.ItemIndex<0) or
             (Menu.ItemIndex=Menu.BtnUndoIndex) or
             (Menu.ItemIndex=Menu.BtnRedoIndex) then
             PostMessage(Handle,MSG_REPLAY,Ord(LastReplayAction),0);
end;

procedure TMainForm.PaintBox1DblClick(Sender: TObject);
begin
  Image1DblClick(Sender);
end;

procedure TMainForm.BtnOptionsClick(Sender: TObject);
begin
  if Game.IsIdleAndStopReplayingAndBrowsing then SelectOptions(Self);
end;

procedure TMainForm.BtnOpenClipBoardClick(Sender: TObject);
var GameReady:Boolean;
begin                 
  ClearTrackBox(True);
  if Game.IsIdleAndStopReplayingAndBrowsing then
     if   Clipboard.HasFormat(CF_TEXT) then begin
          GameReady:=True;                           // here 'True' just means that the clipboard contents haven't been tested yet 

          if   Modified
               or
               Assigned(MultiView.Selected)
               or
               ((Sender=ToolsForm) and ToolsForm.Modified) then begin
               if LoadSolutionsAndSnapshotsForCurrentGame('',True,MultiView.Selected,GameReady) then begin
                  if   IsANewFileName(Game.FileName) then
                       Modified:=True                // the clipboard contained a single level, not a collection
                  else Game.Notes.Modified:=True;    // 'Notes.Modified' later triggers a silent 'SaveToFile', in contrast to 'MainForm.Modified' which asks the user before saving
                  ShowStatus;
                  GameReady:=False;                  // 'False': don't try to load the clipboard contents again in the statements below
                  end;
               end;

          if   GameReady and                         // 'True': either the clipboard contents haven't been tested yet, or the clipboard contained levels instead of solutions and snapshots for the current game
               CloseLevel(BtnSave) and
               LoadGame('',True) then begin
               if   IsANewFileName(Game.FileName) then
                    Modified:=True                   // the clipboard contained a single level, not a collection
               else Game.Notes.Modified:=True;       // 'Notes.Modified' later triggers a silent 'SaveToFile', in contrast to 'MainForm.Modified' which asks the user before saving
               ShowStatus;
               end
          end
     else Msg(TEXT_CLIPBOARD_NO_LEVEL_TEXT,
              Application.Title+SUB_TITLE_SEPARATOR+TEXT_OPEN_LEVEL_FROM_CLIPBOARD,
              MB_OK+MB_ICONINFORMATION);

  Menu.SkipMouseOverSoundForItemIndex:=-1;
  CtrlEnterKeySet:=[]; LastKey:=0;
end;

procedure TMainForm.DrawBoardBevel;
const BEVEL_SHADOW_COLOR=clGray;
var a,b,i,j,k,x1,y1,x2,y2,x3,y3,BorderWidth,ColWidth,RowHeight:Integer;
    More:Boolean;
    BT:SokFile_.TBoard;
begin
  ColWidth   :=GameViewer.ColWidth;
  RowHeight  :=GameViewer.RowHeight;
  BorderWidth:=GameViewer.BorderWidth; //BevelColor:=clYellow;
  if (BorderWidth<(ColWidth-2) div 2) and (BorderWidth<(RowHeight-2) div 2) then begin
     BT:=Game.BTSquare;
     repeat
       More:=False;
       for i:=1 to Game.BoardWidth do
           for j:=1 to Game.BoardHeight do
               if BT[i,j]<>0 then begin

                  k:=BT[i,j]; x2:=-1; y2:=0; x3:=-1; y3:=0;

                  if (k and BT_TOP)<>0 then begin
                     a:=i; b:=j; More:=True;
                     GameViewer.CellToPos(Pred(a),Pred(b),x1,y1); Dec(x1); Dec(y1);
                     x2:=x1+2;
                     Dec(y1,BorderWidth); y2:=y1;
                     if   (k and BT_DENT_TOP_LEFT)=0 then
                          Dec(x1,BorderWidth)
                     else Inc(x1,BorderWidth+2);

                     repeat Dec(BT[a,b],BT_TOP); Inc(x2,ColWidth); Inc(a);
                     until  (BT[a,b] and BT_TOP)=0;

                     if   (BT[Pred(a),b] and BT_DENT_TOP_RIGHT)=0 then
                          Inc(x2,BorderWidth+1)
                     else Dec(x2,BorderWidth+1);

                     Image1.Picture.BitMap.Canvas.Pixels[x1-1,y1]:=BEVEL_SHADOW_COLOR;
                     end

                  else if (k and BT_BOTTOM)<>0 then begin
                     a:=i; b:=j; More:=True;
                     GameViewer.CellToPos(Pred(a),Pred(b),x1,y1); Dec(x1);
                     x2:=x1+2;
                     Inc(y1,BorderWidth+RowHeight+1); y2:=y1;
                     if   (k and BT_DENT_BOTTOM_LEFT)=0 then
                          Dec(x1,BorderWidth)
                     else Inc(x1,BorderWidth+2);

                     repeat Dec(BT[a,b],BT_BOTTOM); Inc(x2,ColWidth); Inc(a);
                     until  (BT[a,b] and BT_BOTTOM)=0;

                     if   (BT[Pred(a),b] and BT_DENT_BOTTOM_RIGHT)=0 then
                          Inc(x2,BorderWidth+1)
                     else Dec(x2,BorderWidth+1);

                     Image1.Picture.BitMap.Canvas.Pixels[x2-1,y1-1]:=BEVEL_SHADOW_COLOR;

                     if (Game.BTSquare[Pred(a),b] and BT_LEFT)<>0 then begin
                        x3:=x1; y3:=y2-1;
                        end;

                     end

                  else if (k and BT_LEFT)<>0 then begin
                     a:=i; b:=j; More:=True;
                     GameViewer.CellToPos(Pred(a),Pred(b),x1,y1); Dec(x1);
                     y2:=y1+1;
                     Dec(x1,BorderWidth); x2:=x1;
                     if   (k and BT_DENT_TOP_LEFT)=0 then
                          Dec(y1,BorderWidth)
                     else Inc(y1,BorderWidth+1);

                     repeat Dec(BT[a,b],BT_LEFT); Inc(y2,RowHeight); Inc(b);
                     until  (BT[a,b] and BT_LEFT)=0;

                     if   (BT[a,Pred(b)] and BT_DENT_BOTTOM_LEFT)=0 then
                          Inc(y2,BorderWidth)
                     else Dec(y2,BorderWidth+1);

                     if (Game.BTSquare[a,Pred(b)] and BT_BOTTOM)<>0 then begin
                        x3:=x1; y3:=y2-1;
                        end;
                     end

                  else if (k and BT_RIGHT)<>0 then begin
                     a:=i; b:=j; More:=True;
                     GameViewer.CellToPos(Pred(a),Pred(b),x1,y1);
                     y2:=y1+1;
                     Inc(x1,BorderWidth+ColWidth+1); x2:=x1;
                     if   (k and BT_DENT_TOP_RIGHT)=0 then
                          Dec(y1,BorderWidth)
                     else Inc(y1,BorderWidth+1);

                     repeat Dec(BT[a,b],BT_RIGHT); Inc(y2,RowHeight); Inc(b);
                     until  (BT[a,b] and BT_RIGHT)=0;

                     if   (BT[a,Pred(b)] and BT_DENT_BOTTOM_RIGHT)=0 then
                          Inc(y2,BorderWidth)
                     else Dec(y2,BorderWidth+1);

                     if (Game.BTSquare[i,j] and BT_DENT_BOTTOM_RIGHT)<>0 then begin
                        x3:=x1; y3:=y2-2;
                        end;
                     end;

                  if x2<>-1 then with Image1.Picture.BitMap.Canvas do begin
                     Pen.Color:=BEVEL_SHADOW_COLOR;
                     MoveTo(x1,y1); LineTo(x2,y2);
                     Pen.Color:=BevelColor;
                     MoveTo(x1-1,y1-1); LineTo(x2-1,y2-1);
                     if x3<>-1 then Pixels[x3,y3]:=BevelColor;
                     end;
                  end;
     until not More;
     end;
end;

function  TMainForm.LoadGame(const FileName:String; Verbose__:Boolean):Boolean;
var ARow:Integer;
begin // precondition: the existing game, if any, has been saved, typically by calling 'CloseLevel'
  LoadSolutionsAndSnapshotsForCurrentGame(FileName,Verbose__,nil,Result);
  if   Result then begin
       MultiView.Clear;
       OpenForm.ExchangeGames(Game);
       SnapshotsForm.LoadSnapshots(nil);
       ARow:=SnapshotsForm.Grid.FixedRows;
       end
  else ARow:=SnapshotsForm.Grid.Row;
  InitGame(False,True,True,False,Result,True,True,ARow);

  if Result then begin
     if Game.RestoreSaveGame then MultiView.LoadSnapshots;
     if Initialized then // 'MultiView.LoadSnapshots' doesn't load multiple views before initialization has been performed; in particular, 'FormResize' must have been called in order to set up the board screen area which is stored in 'MultiView.ClippingRect'
        MultiView.DeleteScreenRegionInformationForAllSnapshots // screen region information is one-shot information for the multiple views manager; if multiple views aren't restored upon entry, then screen region information is considered garbage which should be removed
     end;
end;

function  TMainForm.LoadSolutionsAndSnapshotsForCurrentGame(const FileName:String; Verbose__:Boolean; MultiViewItem__:TMultiViewItem; var GameReady:Boolean):Boolean;
var oCursor:TCursor;
begin // postcondition: if 'Result' = 'False' and 'GameReady' = 'True' then 'OpenForm.Game' contains a game ready to be loaded
  Result:=False; GameReady:=False; oCursor:=Screen.Cursor;
  if GamePictures.Initialized then
     try
       Screen.Cursor:=crHourGlass;
       OpenForm.Game.HourGlassCursor:=True;
       OpenForm.Game.Verbose:=Verbose__;
       OpenForm.Game.SokoFileName:='';                          // avoid that 'OpenForm' tries to save its own game
       OpenForm.Game.FileName:=Game.FileName;                   // transfer current filename to 'OpenForm', so it can take current path from this filename
       OpenForm.Game.LastValidFileName:=Game.LastValidFileName; // transfer current last valid filename in order to get it back when game-objects are swapped
       OpenForm.Game.RestoreSaveGame:=Game.RestoreSaveGame;     // transfer settings for handling the saved game, if any
       OpenForm.Game.ResetSaveGameAndLoadItIfItIsANormalModeGame:=Game.ResetSaveGameAndLoadItIfItIsANormalModeGame;
       OpenForm.Game.SaveOldSolutionsAfterClipboardImport:=Game.SaveOldSolutionsAfterClipboardImport;
       OpenForm.Game.SecondaryMetricsInTitles:=Game.SecondaryMetricsInTitles;
       OpenForm.Game.SolutionsRequireAtLeastOnePush:=True;      // so the savegame and all snapshots and solutions are loaded;

       GameReady:=OpenForm.Game.LoadFromFileOrClipboard(FileName,Self.Game,MultiViewItem__,Result);
     finally
       Screen.Cursor:=oCursor;
     end;
end;

procedure TMainForm.ShowMenu;
begin
  PanelMenu1.Visible:=not Menu.Visible;
  if   Menu.Visible then begin
       GraphicMenuWidth:=Max(10,Menu.MenuWidth);
       Menu.Show;
       end
  else GraphicMenuWidth:=0;
end;
{
procedure TMainForm.DebugStatistics;
var i:Integer; s:String;
begin
  s:='Debug Counts'+NL+NL;
  for i:=Low(DebugCount) to High(DebugCount) do begin
      s:=s+IntToStr(i)+':'+TAB+IntToStr(DebugCount[i])+NL;
      end;
  if Msg(s+NL+NL+'Do you want to clear the information?',Application.Title+' - Debug Information',MB_YESNO+MB_ICONINFORMATION)=ID_YES then
     FillChar(DebugCount,SizeOf(DebugCount),0);
end;
}

procedure TMainForm.ApplicationOnActivate(Sender: TObject);
var oHintCount:Integer;
begin
  if   Status<>nil then oHintCount:=Status.HintCount
  else oHintCount:=0;

  ShowStatus;
//if (EditForm <>nil) and EditForm .Visible then EditForm.ShowStatus;
  if (ToolsForm<>nil) and ToolsForm.Visible then ToolsForm.ShowStatus;
  if (OptionsForm<>nil) and OptionsForm.Visible then OptionsForm.ShowStatus;
  if  Music  <>nil then Music.OnActivateApplication;
  if (MPlayer<>nil) and MPlayer.Visible then MPlayer.ShowVolume;
  if (not Initialized) and Self.Visible then begin // 'Visible' = 'False': the form hasn't been shown yet
     Initialized:=True;
     FormResize(Self);

     if FirstTime then begin // 'True': this is the first time the application is activated after installation or re-installation
        //BtnHelpClick(Self); // this doesn't work in Windows XP: it shows the help window non-modal even though the code says "ShowModal"
        end;

     if (Game.FileName<>'') and
        (SnapshotsForm.ShowOnStartup // if true: the snapshots window was open last time the application closed
         or
         ((SnapshotsForm.PanelName.Caption<>'')
          and
          (SnapshotsForm.ItemCount>Game.BestSolutionsCount)
         )
        ) then begin // if true: this game has some saved snapshots
        SnapshotsForm.Show;
        SnapshotsForm.FormDeactivate(Self);
        Self.SetFocus;
        end;

     if (Game.FileName<>'') and Game.RestoreSaveGame then MultiView.LoadSnapshots;
     MultiView.DeleteScreenRegionInformationForAllSnapshots; // screen region information is one-shot information for the multiple views manager; if multiple views aren't restored upon entry, then screen region information is considered garbage which should be removed

     oHintCount:=0; // show 'welcome' text
     //BtnMPlayerClick(Sender);
     end;
  if      Screen.ActiveForm=OpenForm      then OpenForm      .OnActivate
  else if Screen.ActiveForm=SnapshotsForm then SnapshotsForm .OnActivate(Sender);

  if Deadlocks<>nil then Deadlocks.Resume;

  if Status<>nil then begin
     Status.HintCount:=oHintCount;
     if Status.HintCount<=1 then begin
        Status.HintCount:=0; Status.Hint:=WelcomeText;
        end;
     end;

  CtrlEnterKeySet:=[]; LastKey:=0;
  if Game<>nil then Game.CalculatePathFindingTimeInterval;

  {$IFDEF TEST}
    PostMessage(Self.Handle,MSG_TEST,0,0);
  {$ENDIF}
end;

procedure TMainForm.ApplicationOnDeactivate(Sender: TObject);
begin
  if Game     <>nil then Game.StopTimer;
  if Music    <>nil then Music.OnDeactivateApplication;
  if MPlayer  <>nil then MPlayer.Hide;
  if Deadlocks<>nil then Deadlocks.Suspend;
  if (GameViewer<>nil) and
     ((GameViewer.LegalMovesInfo.Mask<>0)
      or
      GameViewer.LegalMovesInfo.BoxCursor.Enabled
      or
      GameViewer.LegalMovesInfo.PlayerCursor.Enabled
     ) then
     GameViewer.HideLegalMoves;
end;

procedure TMainForm.SelectOptions(Sender: TObject);
const WAIT_MS=250;
var oDeadlockDetectionEnabled,oMPlayerVisible,oSnapshotsFormVisible:Boolean;
    TimeIntervalMS:TTimeIntervalMS; p:TTreeNode;
    oMainFormWindowState:TWindowState; oForm:TForm;
begin
  Game.StopTimer;
  ShutDownApplication         :=False;
  oDeadlockDetectionEnabled   :=Game.DeadlockDetection.Enabled;
  oMPlayerVisible             :=MPlayer.Visible; MPlayer.Hide;
  oSnapshotsFormVisible       :=SnapshotsForm.Visible; SnapshotsForm.Hide;
  oForm                       :=Screen.ActiveForm;

  Music.Finalize;
  with Sound do begin
    if Player[stMenuSelect]<>nil then begin // give the menu-click sound a chance to finish
       TimeIntervalMS:=CalculateTimeIntervalMS(WAIT_MS);
       while (GetTickCount>=TimeIntervalMS.StartTimeMS) and
             (GetTickCount< TimeIntervalMS.StopTimeMS) do SleepEx(0,False);
       end;
    end;
  Sound.Finalize;

  ClearTrackBox(True);

  oMainFormWindowState:=Self.WindowState;
  if WindowState<>wsNormal then WindowState:=wsNormal;
  //WindowState:=wsMinimized;

  if Assigned(MultiView) and Assigned(MultiView.Selected) then MultiView.Selected.MakeSnapshot;

  if Deadlocks<>nil then Deadlocks.Suspend;

  try
    Application.OnActivate:=nil;
    Application.OnMessage :=nil;
    OptionsForm.LoadData;
    OptionsForm.MenuItemExportSkin.Tag := 0; // '0': 'Export skin...' hasn't invalidated the loaded game pictures and the game-viewers

    with OptionsForm.TreeView1 do
      if Selected<>nil then begin
         p:=Selected.Parent;
         while (p<>nil) and (not p.Expanded) do begin
           p.Expand(False); p:=p.Parent;
           end;
         end;

    if (Sender=nil) or (OptionsForm.ShowModal=mrOk) or ( OptionsForm.MenuItemExportSkin.Tag <> 0 ) then begin // 'Tag <> 0': 'Export skin...' has invalidated the loaded game pictures and the game-viewers
       //if Sender<>nil then Screen.ActiveForm.Refresh; // disabled for a subtle reason: if the user changes skin then a refresh shows the old skin first; even though refreshing is fast, the user will perceive it as if there is an unnecessary delay before the new skin appears.

       CurrentImageClientWidth  :=0;     // invalidate background, menu, and status
       Image1.Tag               :=0;     // force 'load font' for menu and status
       SnapshotsForm.Initialized:=False; // force a 'FormResize' next time the form is shown
       MPlayer.Initialized      :=False;

       GameViewer.Clear;                 // kludge: all game-viewers must be cleared before calling 'GamePictures.LoadPictures'
       OpenForm.GameViewer.Clear;
       OpenForm.GameViewer.Canvas:=nil;  // forces the game-viewer to rebuild background images the next time the window is opened

       GamePictures       .LoadPictures;

       GameViewer         .LoadPictures; // kludge: all game-viewers must load the pictures again
       OpenForm.GameViewer.LoadPictures;

       Menu.MakeButtons;                 // 'GraphicMenuWidth' is required by 'FormResize', hence this extra call to 'MakeButton' before calling 'FormResize'
       GraphicMenuWidth:=Max(10,Menu.MenuWidth);
       Status.MakeStatusBar;             // this is also required by 'FormResize'
       FormResize(Self);

       ToolsForm.Initialized:=False;
       if LevelSetForm<>nil then with LevelSetForm do with GameViewer do begin
         if BackgroundPict<>nil then BackgroundPict.OrgBitMap:=nil;
         if SkinPict      <>nil then SkinPict      .OrgBitMap:=nil;
         end;
       end;
  finally
    SetMessageHandlers;
    MPlayer.FpsOk           :=True;  // enable visualization (again)
    Viewer1.PictThread.FadeEnabled:=True;

    if (Deadlocks<>nil) and (Sender<>ToolsForm) then Deadlocks.Resume;

    Sound.Initialize;
    Music.Initialize;

    MPlayer.Display.InitializeActivity(Music.Enabled);

    if MouseTrackingModeEnabled then TrackState:=tsWait
    else TrackState:=tsSit;

    Game.SessionSmoothMoveAnimationEnabled:=Game.SmoothMoveAnimationEnabled;
    Game.UpdateBestSolutionNames;

    if   not Game.TimingEnabled then Game.ClearTimer;
    if   Game.DeadlockDetection.Enabled and Game.DeadlockDetection.BlockMoves then
         Game.SimpleIllegalMovesMask:=SIMPLE_ILLEGAL_MOVES_MASK
    else Game.SimpleIllegalMovesMask:=0;

    if   (Game.FileName<>'') and
         Game.DeadlockDetection.Enabled and
         (Sender<>ToolsForm) and (Sender<>OpenForm) and (Sender<>DuplicatesForm) then
         if   oDeadlockDetectionEnabled then {deadlocks ok}
         else Deadlocks.LoadGame(True,True)  {calculate deadlocks}
    else Deadlocks.Clear;

    if   WindowState<>oMainFormWindowState then begin
         WindowState:=oMainFormWindowState; Update;
         end;

    if   (not Game.SolutionsRequireAtLeastOnePush) and
         (Game.StartBoxOnTargetCount=Game.BoxCount) and
         (Game.History.Count>0) then
         BtnResetClick(Sender);

    if (not ShutDownApplication) and (not ToolsForm.Visible) then begin
       if oSnapshotsFormVisible or SnapshotsForm.ShowOnStartUp then begin
          SnapshotsForm.LoadBestSolutions;
          SnapshotsForm.Show;
          if SnapshotsForm.WindowState=wsMinimized then
             SnapshotsForm.WindowState:=wsNormal;
          SnapshotsForm.FormDeactivate(Self);
          if Self.Visible then Self.SetFocus;
          end;
       if oMPlayerVisible and (WindowState<>wsMinimized) then MPlayer.Show;
       if ShowSimpleDeadSquaresEnabled then
          PostMessage(MainForm.Handle,Misc_.MSG_UPDATE_DEAD_SQUARES,0,0);
       end;

    MultiViewPopupMenu.CalculateWidthAndHeight;
    RotateAndFlipPopupMenu.CalculateWidthAndHeight;

    MakeUndoButtonHint;
    MakeDynamicHints(nil);
    ShowStatus;
    UpdateCursor;
    Menu.SkipMouseOverSoundForItemIndex:=-1;

    if Screen.ActiveForm<>oForm then begin
       if OpenForm.Visible then OpenForm.BringToFront;
       oForm.SetFocus;
       end;

    if Game.SecondaryMetricsInTitles and Assigned(MultiView.Selected) then with MultiView.Selected do begin
       HasSecondaryMetricsCaption:=False;
       ShowCaptionWithSecondaryMetrics; // 'ShowStatus' may have reset the caption to 'moves/pushes' only; show all metrics again
       end;

    if ShutDownApplication then begin
       Msg(ApplicationShutdownText,Application.Title,0);
       BtnExitClick(Sender);
       end;
    end;
end;

procedure TMainForm.BtnMPlayerClick(Sender: TObject);
begin
  if Game.IsIdleAndStopReplayingAndBrowsing then begin
     TrackState:=tsWait; ClearTrackBox(True);
     if   (Sender<>BtnMPlayer) and (Sound<>nil) then Sound.StopAll;
     if   MPlayer.Visible then MPlayer.Hide
     else MPlayer.Show;
     end;
end;

{Die Lautstärke des AUX-Kanals mit der ID-Nummer "DeviceID" ermitteln:}
procedure GetVolume(DeviceID : integer;var VolL,VolR : word);
var Vol : longint;
begin
  AuxGetVolume(DeviceID,@Vol);
  VolL:=Vol mod $10000;  //Die Lautstärke des linken Kanals
  VolR:=Vol shr 16;        //Die Lautstärke des rechen Kanals
end;

{Die Lautstärke des AUX-Kanals mit der ID-Nummer "DeviceID" setzen:}
procedure SetVolume(DeviceID : integer; VolL,VolR : byte);
var Vol  : longint;
    Temp : real;
begin
  Temp:=VolR/255;
  Vol:=round($FFFF*Temp) shl 16;
  Temp:=VolL/255;
  Vol:=Vol+round($FFFF*Temp);
  AuxSetVolume(DeviceID,Vol);
end;


//Informationen zu allen verfügbaren AUX-Kanälen ermitteln
// und in Memo1 anzeigen:}
procedure TMainForm.GetDevInfos;
{
var AuxCaps   : TAuxCaps;
    LVol,RVol,
    VHi,VLo   : word;
    i,NumDevs : integer;
}
begin
{
  Memo1.Lines.Clear;
  Memo1.Lines.Add('wave out devices: '+IntToStr(waveOutGetNumDevs));
  Memo1.Lines.Add('midi out devices: '+IntToStr(midiOutGetNumDevs));
  Memo1.Lines.Add('aux  out devices: '+IntToStr(auxGetNumDevs));
  Memo1.Lines.Add('mixer devices: '+IntToStr(mixerGetNumDevs));
  NumDevs:=AuxGetNumDevs;
  if NumDevs=0 then
    Exit;
  for i:=0 to NumDevs-1 do begin
    AuxGetDevCaps(i, @AuxCaps, SizeOf(AuxCaps));
    Memo1.Lines.Add('DeviceID: '+IntToStr(i));
    VLo:=AuxCaps.vDriverVersion mod $100;
    VHi:=AuxCaps.vDriverVersion div $100;
    Memo1.Lines.Add('Version: '+IntToStr(VHi)+'.'+IntToStr(VLo));
    Memo1.Lines.Add('DeviceName: '+String(AuxCaps.szPName));
    if (AuxCaps.wTechnology and AUXCAPS_CDAUDIO)<>0 then
      Memo1.Lines.Add('DeviceType: CD-Audio')
    else
      Memo1.Lines.Add('DeviceType: Extern');
    GetVolume(i,LVol,RVol);
    Memo1.Lines.Add('Links: '+IntToStr(LVol));
    Memo1.Lines.Add('Rechts: '+IntToStr(RVol));
    Memo1.Lines.Add('');
  end;
}
end;

procedure TMainForm.BtnBookmarksClick(Sender: TObject);
begin
  if BtnBookmarks.Enabled then begin
     if MPlayer.Visible then MPlayer.Hide;
     if Game.IsIdleAndStopReplayingAndBrowsing then
        if   Sender<>nil then
             Game.SaveBookmark
        else Game.RestoreBookmark;
     end;
end;

procedure TMainForm.MakeBookmarksHint;
var i:Integer; s:String;
begin
  exit; // not in production anymore; superseded by the snapshot feature
  with Game do
    if BookmarkCount=0 then begin
       BtnBookmarks.Hint:=HintBookmarksLeftClickText+HintResetRightClickText;
       with BtnReset do
         if AnsiPos(EnterButtonText,Hint)=0 then begin
            i:=AnsiPos(LEFT_PAREN,Hint);
            if i<>0 then Hint:=System.Copy(Hint,1,i)+EnterButtonText+', '+System.Copy(Hint,Succ(i),MaxInt);
            end;
       end
    else begin
       if   BookmarkCount=MAX_BOOKMARKS then s:=BAR
       else s:=HintBookmarksLeftClickText;
       BtnBookmarks.Hint:=s+Format(HintBookmarksRightClickText,[Bookmark[BookmarkCount]]);
       with BtnReset do begin
         i:=AnsiPos(EnterButtonText,Hint);
         if i<>0 then Hint:=System.Copy(Hint,1,Pred(i))+System.Copy(Hint,i+Length(EnterButtonText)+2,MaxInt);
         end;
       end;
end;

procedure TMainForm.OnIdleHandler(Sender: TObject; var Done: Boolean);
var oFrameIndex:Integer; oVisible:Boolean; TimeNowMS:TTimeMS;
begin
  Timer1Timer(Sender);

  TimeNowMS:=GetTickCount;
  if TimeNowMS>IdleStartTime then begin
     if (Music<>nil)
        and
        (Music.Enabled)
        and
        (not Music.FileListOk)
        and
        (Screen.ActiveForm<>OptionsForm)
        and
        (not OptionsForm.Visible)
        then
        Music.CheckIfFileAppeared;

     if ((Screen.ActiveForm=Self) or (Screen.ActiveForm=SnapshotsForm)) and
        Assigned(Solver) and
        Assigned(Optimizer) and
        Assigned(Game) and
        (not Game.IsBusy) and
        (not Game.IsReplaying) and
        (not Game.IsBrowsing) and
        (not Game.IsLoading) and
        Assigned(GameViewer) and
        (GameViewer.LegalMovesInfo.Mask=0)
        then begin
        if (Solver.PendingResultsCount<>0) or (Optimizer.PendingResultsCount<>0) then begin
           with Solver    do if PendingResultsCount<>0 then ImportGames(True);
           with Optimizer do if PendingResultsCount<>0 then ImportGames(True);
           ShowStatus;
           end;
        if GameViewer.Modified and Assigned(MPlayer) and (not MPlayer.Visible) then begin
           oVisible:=RotateAndFlipPopupMenu.Visible;
           if Assigned(SnapshotsForm) and SnapshotsForm.Visible then with SnapshotsForm do
              if Grid.Row>=Grid.FixedRows then
                 ShowSnapshots(True,Screen.ActiveForm=SnapshotsForm,Grid.Row);

           with GameViewer do begin

             oFrameIndex:=PlayerFrameIndex; // remember the current player direction; loading the game may change it;
             LoadGame(Game);
             if Assigned(MultiView) and (not MultiView.IsEmpty) then with MultiView do begin
                DisappearedItemsCount:=0;
                OnResize(nil);
                if ReportDisappearedItems(DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
                   MultiView.DoReportDisappearedItems:=False;
                end;
             PlayerFrameIndex:=oFrameIndex; // restore the player direction
             end;
           ShowStatus;
           if oVisible then begin
              PanelResetTransformations.Enabled:=Game.BoardTransformation2D<>t2DRotate0DegreesClockwise;
              if not RotateAndFlipPopupMenu.Visible then
                 RotateAndFlipPopupMenu.Show;
              end;
           end;
        end;
     end;

  TimeNowMS:=GetTickCount;

  if   High(IdleStartTime)-TimeNowMS>=MPLAYER_POSITION_FRAME_TIME_MS then
       IdleStartTime:=TimeNowMS+MPLAYER_POSITION_FRAME_TIME_MS
  else IdleStartTime:=MPLAYER_POSITION_FRAME_TIME_MS; // clock wrap-around

  if   (GameViewer<>nil) and (GameViewer.LegalMovesInfo.Mask<>0) and
       (Game<>nil) and (not Game.IsBusy) and
       (Sender<>BtnStatusMenuSolver) and (Sender<>BtnStatusMenuOptimizer) then
       GameViewer.OnIdle;

//if Status<>nil then Status.Hint:='Idle '+IntToStr(TimeNowMS);
end;

procedure TMainForm.OnMPlayerOnIdle(var Msg:TMessage); // Message Misc_.MSG_PERFORM_MPLAYER_ON_IDLE;
begin
  MPlayer.MPlayerOnIdle;
end;

procedure TMainForm.OnPlugin(var Msg:TMessage); //Message MSG_PLUGIN;
var b:Boolean;
begin
  IdleStartTime:=0;
  if      Msg.LParam=Integer(Solver) then
          OnIdleHandler(BtnStatusMenuSolver,b)
  else if Msg.LParam=Integer(Optimizer) then
          OnIdleHandler(BtnStatusMenuOptimizer,b)
  else if Msg.LParam=Integer(Generator) then begin
          if Assigned(ToolsForm) and (Screen.ActiveForm=ToolsForm) then ToolsForm.ShowStatus;
          end;
  ShowStatus;
end;

procedure TMainForm.OnRefresh(var Msg:TMessage); //Message MSG_REFRESH;
begin
  FormResize(Self);
end;

procedure TMainForm.OnReplay(var Msg:TMessage); //Message MSG_REPLAY;
begin
  if   Msg.WParam=Ord(gaUndo) then BtnRedoAllClick(BtnUndo)
  else BtnRedoAllClick(BtnRedo);
end;

procedure TMainForm.OnShowDeadlockedBoxes(var Msg:TMessage); // Message Misc_.MSG_SHOW_DEADLOCKED_BOXES;
begin
  ShowDeadlockedBoxes;
end;

procedure TMainForm.OnShutdown(var Msg:TMessage); // Message MSG_SHUTDOWN;
begin
  Close;
end;

procedure TMainForm.OnTest(var Msg:TMessage); // Message MSG_TEST;
begin
  {$IFDEF TEST}
    //BtnOpenClick(Self);
    BtnToolsClick(ToolsForm.TabSheetEditor);
    //BtnToolsClick(ToolsForm.TabSheetSolver);
    //BtnToolsClick(ToolsForm.TabSheetOptimizer);
    //BtnToolsClick(ToolsForm.TabSheetGenerator);
    //BtnToolsClick(ToolsForm.TabSheetCapture);
    Close;
  {$ENDIF}
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if (Game<>nil) and
     (Game.StartTimeMS<>0) and (not Game.IsBusy) and
     Game.TimingIdleTimeThresholdEnabled then with Game do
     if CalculateElapsedTimeMS(StartTimeMS,GetTickCount)>TimingIdleTimeThresholdMS then
        ElapsedTimeMS; // in this case 'ElapsedTimeMS' stops the timer because 'time' > 'idle time Threshold'
end;

{$IFNDEF TEST}

procedure TMainForm.MakeFilesFromResources(OldMajorVersionNumber,OldMinorVersionNumber:Integer);
// for conveniency, all application data files (except additional skins) are
// stored as resources; this makes it possible to distribute the program as a single exe-file
const
  OLD_DEFAULT_STATUS_TRANSPARENCY_PCT = 40;
  OLD_DEFAULT_ZOOM_FACTOR_PCT = 999;

  OLD_TEXTS_A: array[0..4] of String = (
    'Show legal moves - Animation',
    'Player animation enabled',
    'Box animation enabled',
    'Box Animation enabled', // some built-in scripts had this spelling error
    'Animation - Image');
  NEW_TEXTS_A: array[0..4] of String = (
    'Show legal moves - Highlight animation',
    'Player highlight animation enabled',
    'Box highlight animation enabled',
    'Box highlight animation enabled',
    'Highlight animation - Image');

  OLD_TEXTS_B:array[0..1] of String = (
    'Left=0',
    'Top=0');
  NEW_TEXTS_B:array[0..1] of String = (
    'Left=-1',
    'Top=-1');

  OLD_TEXTS_C:array[0..0] of String = ( '[Graphics - Snapshots -' );
  NEW_TEXTS_C:array[0..0] of String = ( '[Graphics - Snapshots and solutions -' );

  OLD_TEXTS_D:array[0..3] of String = ( 'WallTop', '- no top'     ,'- with top'     ,'wall-top');
  NEW_TEXTS_D:array[0..3] of String = ( 'Wall cap','- no wall cap','- with wall cap','wall-cap');

  OLD_TEXTS_E:array[0..0] of String = ( 'Extrapolate' );
  NEW_TEXTS_E:array[0..0] of String = ( 'Interpolate' );

  OLD_TEXTS_F:array[0..1] of String = ('WallBorder'  , 'razor');
  NEW_TEXTS_F:array[0..1] of String = ('WallTrimming', 'trimming');

  OLD_TEXTS_G:array[0..0] of String = ( 'Games 4 Brains Animated.bmp' );  // the skin was renamed in version 1.506
  NEW_TEXTS_G:array[0..0] of String = ( 'Games 4 Brains Animated - Red.bmp' );

  WALL_TOP_OFFSET_INI_FILE_SECTION = '[Graphics - Board - Figures - Wall - WallTop offset, pixels]';
type
  TFileTransferType = (ftCopy,ftCopyNew,ftCopyAndDelete,ftMove);
var
  i,j:Integer; s,s1,s2,ApplicationPath,FileName,Path:String; OriginalCursor:TCursor;
  OldText1,NewText1:array[0..0] of String;

  function  GetApplicationPath:String;
  begin
    Result:=StrWithTrailingPathDelimiter(GetFolderPath(CSIDL_PROGRAM_FILES))+DEFAULT_APPLICATION_FOLDER_NAME; // the application folder (in the production version) is hardwired in the installation procedure to '<Program Files>\BDSokobanYASC'
    if not DirectoryExists(Result) then Result:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  end;

  procedure MacroExpandLevelNotesAndSolutionNotes(const FileName:String);
  var Node,Next:TNode;
  begin // Performs macroexpansion of the level notes and solution notes, and
        // delete the macro definitions from the file header.
    if SokoFile.Close and SokoFile.Open(FileName) then
       try      SokoFile.MacroExpandNotes(True,True);

                Node:=SokoFile.FileHeader.Lines.First;
                while Node<>nil do begin
                  Next:=Node.Next;
                  if ( (AnsiPos('=',Node.Text)<>0) and
                       (not StrBeginsWith(Node.Text,'::')) and
                       (AnsiPos(C_STYLE_NEWLINE,Node.Text)<>0) )                // 'True': the line probably contains a macro definition; this isn't fool proof, of course, but it will have to do
                     or
                     StrBeginsWith(Node.Text,':: Macros') then                  // 'True': the line looks like macro header
                     SokoFile.FileHeader.Lines.Remove(Node,True);
                  Node:=Next;
                end;
                SokoFile.FileHeader.Lines.TrimBlankLines;

                SokoFile.Modified:=True; // ensure the file is saved to disk by 'SokoFile.Close'; macroexpansions don't count as modifications, hence, set the 'Modified' flag manually, so 'SokoFile.Close saves the file to disk
                SokoFile.Close;
       finally  SokoFile.Clear; SokoFile.SetName('');
       end;
  end;

  procedure MakeFile(const FileName,RCName,RCType:String; Overwrite:Boolean);
  var i:Integer; TempFileName:String;
  begin
    if Overwrite or (not FileExists(FileName)) then
       ResourceSaveToFile(FileName,AnsiUpperCase(RCName),RCType)
    else if RCType=RC_LEVEL then begin
            TempFileName:=MakeUniqueFileName(ExtractFilePath(FileName),ExtractFileNameWithoutExt(FileName),TEMP_FILE_NAME_EXT);
            if (TempFileName<>'') and SokoFile.Close then
               try     if ResourceSaveToFile(TempFileName,AnsiUpperCase(RCName),RCType) and
                          SokoFile.Open(FileName) then begin
                          if RCName=SKINNER_RES_NAME then begin
                             SokoFile.Levels.StrSubstitute('David Skinner','David W. Skinner','',True); // names changed in version 1.127
                             if StrBeginsWith(Game.FileName,FileName) then
                                Game.FileName:=StrSubstitute(Game.FileName,'David Skinner','David W. Skinner',i);
                             end;
                          SokoFile.MergeTextFile(TempFileName,
                                                 (AnsiPos(YASGEN_RES_NAME ,TempFileName)<>0)  // YASGen  levels were 'beautified' in version 1.116
                                                 or
                                                 (AnsiPos(HOLLAND_RES_NAME,TempFileName)<>0), // Holland levels were 'beautified' in version 1.541
                                                 True                                         // delete unofficially released levels which don't appear in the new file and which haven't any snapshots
                                                 );

                          end;
               finally DeleteFile(TempFileName); SokoFile.Clear; SokoFile.SetName('');
               end;
            end;
  end; // MakeFilesFromResources.MakeFile

  function MoveFilesToNewDirectory(const OldDirectory__,NewDirectory__,FileNameFilter__:String; OverwriteExistingFiles__,MoveSystemFiles__:Boolean; FileTransferType__:TFileTransferType; var Count__:Integer):Boolean;
  // move files by renaming a directory doesn't work when the
  // installation package has created the new directory already;
  // therefore, the files are moved one at a time here;
  var i,j:Integer; s,ApplicationDefaultSkinPath:String;

    function TransferFile(const ExistingFileName__,NewFileName__:String; FileTransferType__:TFileTransferType; var Count__:Integer):Boolean;
    begin
      case FileTransferType__ of
        ftCopy,
        ftCopyNew      : Result:=Windows.CopyFile(PChar(ExistingFileName__),PChar(NewFileName__),True) and
                                 SetFileAttributes(PChar(NewFileName__),FILE_ATTRIBUTE_NORMAL);
        ftCopyAndDelete: Result:=CopyAndDeleteFile(ExistingFileName__,NewFileName__);
        ftMove         : Result:=MoveOrCopyFile   (ExistingFileName__,NewFileName__);
        else             Result:=Error(InternalErrorText+': Main_.TransferFile',Application.Title);
      end; // case
      if Result then Inc(Count__);
    end;

  begin // MoveFilesToNewDirectory
    // this function invalidates the contents of 'OpenForm.FileListBox1', hence, only use the function at a time when it doesn't matter
    try    ForceDirectories(NewDirectory__);
    except on E:Exception do;
    end;

    if DirectoryExists(OldDirectory__) and
       DirectoryExists(NewDirectory__) and
       (not StrEqual(OldDirectory__,NewDirectory__)) then with OpenForm.FileListBox1 do begin
       Mask:=FileNameFilter__;
       Directory:=OldDirectory__;
       ApplicationDefaultSkinPath:='';
       for i:=Pred(Items.Count) downto 0 do begin
           FileName:=StrWithTrailingPathDelimiter(OldDirectory__)+Items[i];
           if FileExists(FileName)
              and
              (MoveSystemFiles__
               or
               // the following list of executables and system files is clearly not exhaustive, but it will have to do;
               // (the '.dat' file-extension is included because the installation program creates an "unins999.dat" file in the application folder)
               (AnsiPos(AnsiLowerCase(ExtractFileExt(FileName)),'.bat,.exe,.dll,.pif,.scr,.dat')=0)
              ) then begin
              s:=StrWithTrailingPathDelimiter(NewDirectory__)+Items[i];
              if   FileExists(s) then begin
                   if   OverWriteExistingFiles__
                        or
                        ((FileTransferType__=ftCopyNew)
                         and
                         IsANewerFile(s,FileName)
                        )
                        then begin
                        DeleteFile(s);
                        TransferFile(FileName,s,FileTransferType__,Count__);
                        end
                   else if (FileTransferType__=ftCopyAndDelete)
                           or
                           (FileTransferType__=ftMove) then begin
                           if ApplicationDefaultSkinPath='' then // empty string: first time
                              // note that 'ApplicationDefaultSkinPath' is in the
                              // application folder, typically "C:\Program Files\BDSokobanYASC\Skins\Common Skins";
                              // it's not the skin folder in the user's "Documents"
                              ApplicationDefaultSkinPath:=StrSubstitute1(Skins.DefaultSkinPath,StrWithTrailingPathDelimiter(ApplicationDataPath),StrWithTrailingPathDelimiter(ApplicationPath),j);
                           if not StrBeginsWith(FileName,StrWithTrailingPathDelimiter(ApplicationDefaultSkinPath)) then
                              // theoretically, the user can loose files here, but given
                              // the way this  function is used in practice, name-clashes
                              // are unlikely to happen; therefore, the best thing to do
                              // is to clean up, even though it's not 100% correct;
                              try    DeleteFile(FileName);
                              except on E:Exception do begin end;
                              end
                           end;
                   end
              else TransferFile(FileName,s,FileTransferType__,Count__);
              end;
           end;

        if (FileTransferType__=ftCopyAndDelete) or (FileTransferType__=ftMove) then begin
           Directory:=NewDirectory__; // forces an update event
           Directory:=OldDirectory__; // back to the old directory
           if Items.Count=0 then begin // the old directory is empty
              Directory:=NewDirectory__;
              SetCurrentDir(StrWithoutTrailingPathDelimiter(ApplicationDataPath));
              SysUtils.RemoveDir(OldDirectory__);
              end;
           end;
        end;
    Result:=not DirectoryExists(OldDirectory__);
  end; // MakeFilesFromResources.MoveFilesToNewDirectory

  function StrSubstitutePathsInSettingsAndFileHistory(const OldPath__,NewPath__:String):Integer;
  var i:Integer; s,s1:String;

    procedure SubstitutePathsInComboBoxItems(ComboBox__:TComboBox; const OldFolder__,NewFolder__:String);
    var i:Integer;
    begin
    with ComboBox__ do
      for i:=0 to Pred(Items.Count) do
        if StrBeginsWith(Items[i],OldFolder__) then
           Items[i]:=NewFolder__+Copy(Items[i],Succ(Length(OldFolder__)),MaxInt);
    end; // MakeFilesFromResources.StrSubstitutePathsInSettingsAndFileHistory.SubstitutePathsInComboBoxItems

  begin // StrSubstitutePathsInSettingsAndFileHistory
    // note that tools filenames in the settings (e.g., solver and optimizer) aren't updated; it's mostly graphics filenames that are updated
    Result:=0; OptionsForm.LoadData;
    for i:=Ord(Low(TSettings)) to Ord(High(TSettings)) do begin
        s:=OptionsForm.SettingsString[TSettings(i)];
        if StrBeginsWith(s,OldPath__) and
           (i<>Ord(stToolsSolverFileName)) and
           (i<>Ord(stToolsOptimizerFileName)) then begin
           s:=NewPath__+Copy(s,Succ(Length(OldPath__)),MaxInt);
           OptionsForm.UpdateSettings(-i,s,nil);
           Inc(Result);
           end;
        end;
    if Result<>0 then OptionsForm.SaveData(True);

    // update game filename
    if StrBeginsWith(Game.FileName,OldPath__) and (Game.GameState=gsNull) then
       Game.FileName:=NewPath__+Copy(Game.FileName,Succ(Length(OldPath__)),MaxInt);

    // update file history
    s :=StrWithoutTrailingPathDelimiter(OldPath__);
    s1:=StrWithoutTrailingPathDelimiter(NewPath__);
    SubstitutePathsInComboBoxItems(OpenForm   .LevelsFolderHistoryComboBox           ,s,s1);
    SubstitutePathsInComboBoxItems(OpenForm   .LevelsFileHistoryComboBox             ,s,s1);
    SubstitutePathsInComboBoxItems(OpenForm   .AnythingButLevelsFolderHistoryComboBox,s,s1);
    SubstitutePathsInComboBoxItems(OpenForm   .AnythingButLevelsFileHistoryComboBox  ,s,s1);
    SubstitutePathsInComboBoxItems(MPlayerForm.ComboBoxFolder                        ,s,s1);
    SubstitutePathsInComboBoxItems(MPlayerForm.ComboBoxPlayList                      ,s,s1);
  end; // MakeFilesFromResources.StrSubstitutePathsInSettingsAndFileHistory

  function MoveFilesFromProgramFilesToMyDocuments(var ApplicationPath__:String):Boolean;
  var i,j,Count,FirstVirtualFolderIndex:Integer; s,VirtualFolder:String; SL:TStringList;
  begin // MoveFilesFromProgramFilesFolderToMyDocuments
    // move application data files to the user's "<My Documents>\Sokoban\Sokoban YASC" folder
    Count:=0;
    s:=GetApplicationPath;
    Result:=DirectoryExists(s)
            and
//          (not StrEqual(s,StrWithoutTrailingPathDelimiter(ApplicationDataPath))); // 'not StrEqual(...)': if the application data path is the same as the application folder, then there is nothing to move
            (not StrBeginsWith(ApplicationDataPath,StrWithTrailingPathDelimiter(s))); // 'not StrBeginsWith(...)': if the application data path is a subfolder of the application folder, then it's highly unlikely that a file transfer is intended
    if Result then begin
       ApplicationPath__:=StrWithTrailingPathDelimiter(s);

       try
         SL:=TStringList.Create;
         try
           // make a list with all subfolders of the application folder, including the application folder itself
           SL.Sorted:=False;
           SL.Add(StrWithoutTrailingPathDelimiter(ApplicationPath__)); i:=0;

           while i<SL.Count do begin // add all application folder subfolders
             OpenForm.DirectoryListBox1.Directory:=SL.Strings[i]; Inc(i); // update the directory listbox in order to find any subfolders in the selected folder
             if DirectoryExists(OpenForm.DirectoryListBox1.Directory) then
                for j:=0 to Pred(OpenForm.DirectoryListBox1.Items.Count) do begin
                    s:=OpenForm.DirectoryListBox1.GetItemPath(j);
                    if StrBeginsWith(s,ApplicationPath__) and
                       (SL.IndexOf(s)<0) then
                       SL.Add(s);
                    end;
             end;

           FirstVirtualFolderIndex:=SL.Count;
           VirtualFolder:=GetApplicationVirtualFolder;
           if DirectoryExists(VirtualFolder) then begin // include any folders and files created by the Windows virtualization of files in the 'Program Files\<Application>' folder
              i:=SL.Count; SL.Add(VirtualFolder);
              while i<SL.Count do begin // add all application folder subfolders
                OpenForm.DirectoryListBox1.Directory:=SL.Strings[i]; Inc(i); // update the directory listbox in order to find any subfolders in the selected folder
                if DirectoryExists(OpenForm.DirectoryListBox1.Directory) then
                   for j:=0 to Pred(OpenForm.DirectoryListBox1.Items.Count) do begin
                       s:=OpenForm.DirectoryListBox1.GetItemPath(j);
                       if StrBeginsWith(s,VirtualFolder) and
                          (SL.IndexOf(s)<0) then
                          SL.Add(s);
                       end;
                end;
              end;

           // move files in the program folder and its subfolders
           // (including files created by the file virtualization, if any)
           // to the document subfolder
           for i:=Pred(SL.Count) downto 0 do begin
               //Msg(StrWithoutTrailingPathDelimiter(ApplicationPath__    +SL.Strings[i])+NL+NL+
               //    StrWithoutTrailingPathDelimiter(ApplicationDataPath  +SL.Strings[i]),'',MB_OK);

               if i<FirstVirtualFolderIndex then s:=ApplicationPath__
               else s:=VirtualFolder;
               MoveFilesToNewDirectory(SL.Strings[i],
                                       StrSubstitute1(SL.Strings[i],
                                                      StrWithoutTrailingPathDelimiter(s),
                                                      StrWithoutTrailingPathDelimiter(ApplicationDataPath),
                                                      j),
                                       ALL_FILES_FILTER,
                                       False,False,ftCopyAndDelete,Count);
               end;
         finally
           SL.Free;
         end;
       except on E:Exception do Result:=Error(E.Message,Application.Title);
       end;
       end;

    if Result and (Count<>0) then begin
       //Msg(Format(UserDataMovedFromProgramFolderToUserFolderText__,
       //           [StrWithoutTrailingPathDelimiter(ApplicationPath__),
       //            StrWithoutTrailingPathDelimiter(ApplicationDataPath)]),
       //    TEXT_APPLICATION_TITLE_LONG,MB_ICONINFORMATION+MB_OK);
       Windows.MessageBox(0, // '0': so the message appears directly on the user's desktop; otherwise it won't show up in the task bar as an independent job, and the would not be any clue that there is a pending message if the user leaves the installation package window during the process
                          PChar(Format(UserDataMovedFromProgramFolderToUserFolderText__,
                                       [StrWithoutTrailingPathDelimiter(ApplicationPath__),
                                        StrWithoutTrailingPathDelimiter(ApplicationDataPath)])),
                          PChar(TEXT_APPLICATION_TITLE_LONG),
                          MB_ICONINFORMATION+MB_OK);
       end;
  end; // MakeFilesFromResources.MoveFilesFromProgramFilesToMyDocuments

  procedure AbbreviateItemsInComboBox(ComboBox__:TComboBox);
  var i:Integer;
  begin
    with ComboBox__ do
      for i:=0 to Pred(Items.Count) do Items[i]:=AbbreviatedFilePath(Items[i],MyDocumentsFolder);
  end; // MakeFilesFromResources.AbbreviateItemsInComboBox

  procedure MoveSolversToPlugins;
  var i:Integer; s,s1,s2:String;
  begin
    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<302) then begin
       // 1.302 generalized 'solver-plugins' to 'plugins' and changed the default plugin-directory from "...\Solvers" to "...\Plugins"
       s2:=StrWithTrailingPathDelimiter(ApplicationPath);
       s1:=s2+'Solvers';
       s2:=s2+DEFAULT_PLUGINS_DIRECTORY;
       MoveFilesToNewDirectory(s1,s2,ALL_FILES_FILTER,False,True,ftMove,i);
       with ToolsForm.SolverComboBox do
         for i:=Pred(Items.Count) downto 0 do begin
             s:=Items[i];
             if StrBeginsWith(s,s1) then begin
                s:=s2+Copy(s,Succ(Length(s1)),MaxInt);
                if (Items.IndexOf(s)<0) and FileExists(s) then Items[i]:=s;
                end;
             end;
       end;
  end; // MakeFilesFromResources.MoveSolversToPlugins

  function CopyCommonSkinsFromApplicationFolder:Integer;
  var i,j:Integer; s,ApplicationPath,ApplicationDefaultSkinPath:String; SL:TStringList;
  begin // the installation package puts common skins in the application folder; now copy them to the user's private skin folder
    Result:=0;
    ApplicationPath:=GetApplicationPath;
    if not DirectoryExists(ApplicationPath) then ApplicationPath:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
    if DirectoryExists(ApplicationPath) and
       (not StrEqual(ApplicationPath,StrWithoutTrailingPathDelimiter(ApplicationDataPath))) then begin // 'not StrEqual(...)': if the application data path is the same as the application folder, then there is nothing to move

       ApplicationDefaultSkinPath:=StrWithoutTrailingPathDelimiter(StrSubstitute1(Skins.DefaultSkinPath,StrWithTrailingPathDelimiter(ApplicationDataPath),StrWithTrailingPathDelimiter(ApplicationPath),i));
       if DirectoryExists(ApplicationDefaultSkinPath) then
          try
            SL:=TStringList.Create;
            try
              // make a list with all subfolders of the application common skins folder
              SL.Sorted:=False;
              SL.Add(StrWithoutTrailingPathDelimiter(ApplicationDefaultSkinPath)); i:=0;
              while i<SL.Count do begin // add all subfolders
                OpenForm.DirectoryListBox1.Directory:=SL.Strings[i]; Inc(i); // update the directory listbox in order to find any subfolders in the selected folder
                if DirectoryExists(OpenForm.DirectoryListBox1.Directory) then
                   for j:=0 to Pred(OpenForm.DirectoryListBox1.Items.Count) do begin
                       s:=OpenForm.DirectoryListBox1.GetItemPath(j);
                       if StrBeginsWith(s,ApplicationDefaultSkinPath) and
                          (SL.IndexOf(s)<0) then
                          SL.Add(s);
                       end;
                end;

              // copy files in the application common skins folder and its subfolders to the user's private folders
              for i:=Pred(SL.Count) downto 0 do
                  MoveFilesToNewDirectory(SL.Strings[i],
                                          StrSubstitute1(SL.Strings[i],
                                                         StrWithoutTrailingPathDelimiter(ApplicationDefaultSkinPath),
                                                         StrWithoutTrailingPathDelimiter(Skins.DefaultSkinPath),
                                                         j),
                                          ALL_FILES_FILTER,
                                          False,False,ftCopyNew,Result);
            finally
              SL.Free;
            end;
          except on E:Exception do Error(E.Message,Application.Title);
          end;
       end;
  end; // MakeFilesFromResources.CopyCommonSkinsFromApplicationFolder

begin // MakeFilesFromResources
  OriginalCursor:=Screen.Cursor;
  try
    Screen.Cursor:=crHourGlass;

    ApplicationPath:=StrWithTrailingPathDelimiter(ExtractFilePath(Application.ExeName));

    CopyCommonSkinsFromApplicationFolder; // the installation package puts the accompanying skins in the application folder; copy them to the user's private folder

    if (OldMajorVersionNumber<=1) and (OldMinorVersionNumber<364) then begin
       // move all application data files to the user's
       // "<Documents>\Sokoban\Sokoban YASC" folder;
       // after moving the files, some further processing is required; see
       // the code further down in this procedure
       Skins.ClearSkins;
       Skins.ClearScripts; // this forces users with own scripts to add them to the list again, but it's unlikely that this affects more than a handful of people, if any

       MoveSolversToPlugins; // generalize 'Solvers' to 'Plugins', if that hasn't been done already;

       MoveFilesFromProgramFilesToMyDocuments(ApplicationPath); // the function updates the 'ApplicationPath' used throughout this rest of the procedure
       end;

    Skins.MakeDirectories;

    s:=StrWithTrailingPathDelimiter(Skins.ScriptsPath);

    if (OldMajorVersionNumber<=1) and (OldMinorVersionNumber<173) then begin
       // version  1.173 renamed 'YSokoban Skins' to 'Common Skins'
       Skins.ClearSkins;
       Skins.ClearScripts;

       FileName:=s+'YSokoban.sks';
       if FileExists(FileName) then DeleteFile(FileName);
       MoveFilesToNewDirectory(StrWithTrailingPathDelimiter(SkinsPath)+'YSokoban',Skins.DefaultSkinPath,ALL_FILES_FILTER,False,False,ftMove,i);

       StrSubstitutePathsInSettingsAndFileHistory(StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(StrSubstitute1(SkinsPath,ApplicationDataPath,ApplicationPath,j))+'YSokoban'),
                                                  StrWithTrailingPathDelimiter(Skins.DefaultSkinPath));
       end;

    if (OldMajorVersionNumber<=1) and (OldMinorVersionNumber<302) then begin
       // version 1.302 moved the skin scripts from the application folder to a
       // dedicated skin scripts folder
       Skins.ClearSkins;
       Skins.ClearScripts; // this forces users with own scripts to add them to the list again, but it's unlikely that this affects more than a handful of people, if any
       end;

    for i:=Low(SKIN_TYPE_RES_NAME) to High(SKIN_TYPE_RES_NAME) do begin
        FileName:=SKIN_TYPE_RES_NAME[i];
        s1:=s+FileName+SKIN_SCRIPT_FILE_EXT;
        if ((AnsiPos(UNDERSCORE,FileName)<>0)
            or
            (FileName=SOKOBAN_PLUS_PLUS_RES_NAME)
           )
           and
           FileExists(s1) then begin
           if FileName=YASC_SETTINGS_RES_NAME then
              Skins.SettingsScriptFileName:=s1;
           FileName:=s1; // development version: don't duplicate the scripts with slightly different names
           end
        else begin
           if FileName=SOKOBAN_PLUS_PLUS_RES_NAME then FileName:=StrSubstitute(FileName,'P','+',j);
           FileName:=s+
                    Trim(StrSubstitute(FileName,UNDERSCORE,SPACE,j))+
                     SKIN_SCRIPT_FILE_EXT;
           MakeFile(FileName,SKIN_TYPE_RES_NAME[i],RC_TXT,True);
           end;
        Skins.AddScript(FileName);

        if (OldMajorVersionNumber<=1) and (OldMinorVersionNumber<144) then begin
           FileName:=ChangeFileExt(FileName,'')+' 2004'+SKIN_SCRIPT_FILE_EXT; // early versions included the year as a sort of timestamp
           if FileExists(FileName) then DeleteFile(FileName);
           end;
        end;

    if (OldMajorVersionNumber<=1) and (OldMinorVersionNumber<134) then begin
       s1:=s+SOKOBAN_BDE_OLD_SKIN_SCRIPT_FILE_NAME+SKIN_SCRIPT_FILE_EXT;
       if FileExists(s1) then DeleteFile(s1); // filename changed from 'Sokoban 3400' to 'Sokoban BDE'
       with RotateAndFlipPopupMenu.FormColors do
         if (ButtonColor=clGreen) and (ButtonTextColor=clWhite) then begin
            ButtonColor:=$FF8000; // default button-color was changed in 1.134
            MultiViewPopupMenu.FormColors.ButtonColor:=ButtonColor;
            end;
       end;

    OpenForm.SkinScriptsComboBox.ItemIndex:=Skins.IndexOfScript(Skins.SettingsScriptFileName);

    //MakeFile(ApplicationPath+FILE_ASSOC_PROGRAM_NAME,FILE_ASSOC_PROGRAM_RES_NAME,RC_EXE,True); // the file-association program must be installed by the INNO setup program which runs with administrator priviliges, meaning it can put files in the application folder

    Path:=ApplicationDataPath;

    MakeFile(Path+ExtractFileName(ChangeFileExt(Application.ExeName,RTF_FILE_EXT)),HELP_RES_NAME,RC_RTF,True);

    MakeFile(Path+LICENSE_RES_NAME+TEXT_FILE_EXT,LICENSE_RES_NAME,RC_TXT,True);

    MakeFile(Path+README_RES_NAME+TEXT_FILE_EXT,README_RES_NAME,RC_TXT,True);

    MakeFile(Path+RELEASE_NOTES_RES_NAME+RTF_FILE_EXT,RELEASE_NOTES_RES_NAME,RC_RTF,True);

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<129) then with OpenForm.FileListBox1 do begin
       // skins were introduced in version 1.129; move settings to the new directory 'Skins\YASC Settings'
       Directory:=Path;
       Mask:=STAR+SETTINGS_FILE_EXT;
       for i:=Pred(Items.Count) downto 0 do begin
           s:=StrWithTrailingPathDelimiter(OptionsForm.SettingsPath)+Items[i];
           if not FileExists(s) then Misc_.MoveFileOrFolder(Path+Items[i],s);
           end;
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<176) then begin
       // the term "Animation" was changed to "Highlights" in version 1.176 to avoid confusion with move-animation, e.g., during playback
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',OLD_TEXTS_A,NEW_TEXTS_A);
       OpenForm.StrSubstituteInFiles(Skins.ScriptsPath       ,STAR+SKIN_SCRIPT_FILE_EXT,'',OLD_TEXTS_A,NEW_TEXTS_A);
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<226) then begin
       // default wall cap offset changed in version 1.226 to '-1' instead of '0' so '0' literally means a zero-offset
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,WALL_TOP_OFFSET_INI_FILE_SECTION,OLD_TEXTS_B,NEW_TEXTS_B);
       OpenForm.StrSubstituteInFiles(Skins.ScriptsPath       ,STAR+SKIN_SCRIPT_FILE_EXT,WALL_TOP_OFFSET_INI_FILE_SECTION,OLD_TEXTS_B,NEW_TEXTS_B);
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<233) then begin
       // Antialiasing options (filtering may be a more correct term) got more informative names in version 1.233 and again in version 1.647
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',Antialiasing2Text,AntialiasingText);
       OpenForm.StrSubstituteInFiles(Skins.ScriptsPath       ,STAR+SKIN_SCRIPT_FILE_EXT,'',Antialiasing2Text,AntialiasingText);
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<236) then begin
       // The term "Variation" was changed to the better "Snapshot" throughout the application in 1.236
       SnapshotsForm.NormalModeSnapshotName  :=SNAPSHOT_TYPE_NAME[stSnapshot];
       SnapshotsForm.ReverseModeSnapshotName :=SNAPSHOT_TYPE_NAME[stReverseSnapshot];
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<241) then begin
       // 1.241 was the first version with full support for solver plugins; reset to default solver
       Solver.PluginFileName:=DEFAULT_VALUE;
       // default pen-color changed from black to light-gray in 1.241
       SnapshotsForm.InfoMemo.Color          :=clTeal;
       SnapshotsForm.InfoMemo.Font.Color     :=clLtGray;
       LevelSetForm .InfoMemo.Color          :=clTeal;
       LevelSetForm .InfoMemo.Font.Color     :=clLtGray;
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<243) then begin
       // the term "Highlight animation" was changed back to "Animation" in version 1.243. (See code above for version 1.176.)
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',NEW_TEXTS_A,OLD_TEXTS_A); // note that 'new' and 'old' deliberately are reversed here
       OpenForm.StrSubstituteInFiles(Skins.ScriptsPath       ,STAR+SKIN_SCRIPT_FILE_EXT,'',NEW_TEXTS_A,OLD_TEXTS_A); // note that 'new' and 'old' deliberately are reversed here
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<247) then begin
       // 1.246 was the first version with the final specification of the "Common Sokoban Solver Plugin Interface";
       // 1.247 was a polished version with a minor bugfix; reset to default solver
       Solver.PluginFileName:=DEFAULT_VALUE;
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<302) then begin
       // 1.302 generalized 'solver-plugins' to 'plugins' and changed the default plugin-directory from "...\Solvers" to "...\Plugins"
       MoveSolversToPlugins;
       // 1.302 moved the skin scripts from the application folder to a
       // dedicated skin scripts folder
       MoveFilesToNewDirectory(StrWithoutTrailingPathDelimiter(ApplicationDataPath),
                               StrWithoutTrailingPathDelimiter(Skins.ScriptsPath),
                               STAR+SKIN_SCRIPT_FILE_EXT,False,False,ftMove,i);
       MoveFilesToNewDirectory(StrWithoutTrailingPathDelimiter(ApplicationPath), // there shouldn't be any skin scripts in the application folder at this time, but the search doesn't hurt
                               StrWithoutTrailingPathDelimiter(Skins.ScriptsPath),
                               STAR+SKIN_SCRIPT_FILE_EXT,False,False,ftMove,i);
       for i:=Pred(Skins.ScriptList.Count) downto 0 do begin
           s:=Skins.Scripts[i,SKIN_SCRIPT_INDEX];
           if not FileExists(s) then begin
              s:=StrWithTrailingPathDelimiter(Skins.ScriptsPath)+ExtractFileName(s);
              if   FileExists(s) then
                   Skins.Scripts[i,SKIN_SCRIPT_INDEX]:=s
              else Skins.DeleteScript(i);
              end;
           end;
       with OpenForm.SkinsComboBox do
         for i:=Pred(OpenForm.SkinsComboBox.Items.Count) downto 0 do begin
             s:=Skins.Skins[i,SKIN_SCRIPT_INDEX];
             if not FileExists(s) then begin
                s:=StrWithTrailingPathDelimiter(Skins.ScriptsPath)+ExtractFileName(s);
                if FileExists(s) then Skins.Skins[i,SKIN_SCRIPT_INDEX]:=s;
                end;
             end;

       // 1.302 introduced different default font sizes for low resolution screens and high resolution screens
       if IsAHighResolutionScreen then begin
          if Assigned(Menu.MenuPanel) and (Menu.MenuPanel.Font.Size=DEFAULT_GAME_MENU_FONT_SIZE[False]) then
             Menu.MenuPanel.Font.Size:=DEFAULT_GAME_MENU_FONT_SIZE[True];
          if Assigned(Status.StatusPanel) and (Status.StatusPanel.Font.Size=DEFAULT_STATUS_PANEL_FONT_SIZE[False]) then
             Status.StatusPanel.Font.Size:=DEFAULT_STATUS_PANEL_FONT_SIZE[True];
          if Status.SubTextsFontSize=DEFAULT_STATUS_PANEL_SUBTEXT_FONT_SIZE[False] then
             Status.SubTextsFontSize:=DEFAULT_STATUS_PANEL_SUBTEXT_FONT_SIZE[True];
          end;

       // 1.302 introduced much more efficient pathfinding algorithms;
       // however, the box-to-square pathfinder didn't improve, and if the
       // board limit is raised later, then a more tight timelimit is required
       // in order to reduce the latency if the user either on purpose or by
       // happenstance launches the box-to-square calculation
       Game.PathFindingMaxTimeMS:=DEFAULT_PATH_FINDING_MAX_TIME_MS;
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<309) then begin
       // walltype names changed in 1.309
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',WallTypeText,OldWallTypeText);
       end;


    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<318) then begin
       // version 1.318 prepared the program for moving all application data files
       // to the user's "<My Documents>\Sokoban\Sokoban YASC" folder, but it wasn't
       // set into production; however, the accompanying abbreviation of the
       // pathnames in file-histories was enabled throughout the program;
       AbbreviateItemsInComboBox(OpenForm.LevelsFolderHistoryComboBox);
       AbbreviateItemsInComboBox(OpenForm.LevelsFileHistoryComboBox);
       AbbreviateItemsInComboBox(OpenForm.AnythingButLevelsFolderHistoryComboBox);
       AbbreviateItemsInComboBox(OpenForm.AnythingButLevelsFileHistoryComboBox);
       AbbreviateItemsInComboBox(MPlayerForm.ComboBoxFolder);
       AbbreviateItemsInComboBox(MPlayerForm.ComboBoxPlayList);
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<323) then begin
       // version 1.323 added the best solutions to the 'Snapshots' window,
       // and the window name changed accordingly to 'Snapshots and solutions';
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',OLD_TEXTS_C,NEW_TEXTS_C);
       OpenForm.StrSubstituteInFiles(Skins.ScriptsPath       ,STAR+SKIN_SCRIPT_FILE_EXT,'',OLD_TEXTS_C,NEW_TEXTS_C);
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<352) then begin
       // version 1.352 changed default 'Add date to solution notes' from 'False' to 'True'
       Optimizer.AddDateToSolutionNotes:=True;
       Solver   .AddDateToSolutionNotes:=True;
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<364) then begin
       // version 1.364 moved all application data files to the user's
       // "<My Documents>\Sokoban\Sokoban YASC" folder

       Game.FileName:=StrSubstitute(Game.FileName,ApplicationPath,ApplicationDataPath,i);

       // update filenames in settings and the file-history that refer to files in the application folder that now have moved to the documents subfolder
       StrSubstitutePathsInSettingsAndFileHistory(ApplicationPath,ApplicationDataPath);

       // with data files in <My Documents>, all file-names tend to be longer,
       // hence, it's preferable to abbreviate the names appearing in file-history comboboxes
       AbbreviateItemsInComboBox(OpenForm.LevelsFolderHistoryComboBox);
       AbbreviateItemsInComboBox(OpenForm.LevelsFileHistoryComboBox);
       AbbreviateItemsInComboBox(OpenForm.AnythingButLevelsFolderHistoryComboBox);
       AbbreviateItemsInComboBox(OpenForm.AnythingButLevelsFileHistoryComboBox);
       AbbreviateItemsInComboBox(MPlayerForm.ComboBoxFolder);
       AbbreviateItemsInComboBox(MPlayerForm.ComboBoxPlayList);

       // update all saved settings files
       OldText1[Low(OldText1)]:=StrWithoutTrailingPathDelimiter(ApplicationPath);
       NewText1[Low(NewText1)]:=StrWithoutTrailingPathDelimiter(ApplicationDataPath);
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',OldText1,NewText1); // note that solver and optimizer filename may be wrongly updated here; it is, however, unlikely to happen because the default solver and optimizer are listed as "[Standard]" instead of their actual filename, and at the time of writing, it was unlikely that there were other plugins in the "..\Plugins" folder;

       // move default solver/optimizer-plugin settings-file to the documents subfolder
       s1:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(ApplicationPath    )+DEFAULT_PLUGINS_DIRECTORY)+ChangeFileExt(DEFAULT_SOLVER_FILE_NAME_STUB,INI_FILE_EXT);
       s2:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(ApplicationDataPath)+DEFAULT_PLUGINS_DIRECTORY)+ChangeFileExt(DEFAULT_SOLVER_FILE_NAME_STUB,INI_FILE_EXT);
       if FileExists(s1) then begin
          ForceDirectories(StrWithoutTrailingPathDelimiter(ExtractFilePath(s2)));
          if Misc_.MoveFileOrFolder(s1,s2) or Windows.CopyFile(PChar(s1),PChar(s2),True) then begin
             SetFileAttributes(PChar(s2),FILE_ATTRIBUTE_NORMAL);
             end;
          end;

       // version 1.364 changed statuspanel subtexts from transparent to opaque by default;
       // this requires selecting an appropriate font color depending on the selected statuspanel image
       with Status do
         if SubTextsAreTransparent and (FileName=DEFAULT_VALUE) then with TileRect do begin
            if              (Left=1) and (Top=37) then begin
                            SubTextsAreTransparent:=False; SubTextsFontColor:=DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_BLUE;
                            end
            else if Left=132 then
                    if      Top=1 then begin
                            SubTextsAreTransparent:=False; SubTextsFontColor:=DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_GREEN;
                            end
                    else if Top=37 then begin
                            SubTextsAreTransparent:=False; SubTextsFontColor:=DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_RED;
                            end
                    else if Top=73 then begin
                            SubTextsAreTransparent:=False; SubTextsFontColor:=DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_ORANGE;
                            end
            end;
       end;
{
    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<447) then begin
       // wall caps changed name from 'WallTop' to 'Wall cap' in version 1.447.
       // see below in the entry for version 1.462, where this conversion is
       // made again.
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',OLD_TEXTS_D,NEW_TEXTS_D);
       OpenForm.StrSubstituteInFiles(Skins.ScriptsPath       ,STAR+SKIN_SCRIPT_FILE_EXT,'',OLD_TEXTS_D,NEW_TEXTS_D);
       end;
}
    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<457) then with DuplicatesForm do begin
       // the two default row colors for the level duplicate finder tool were
       // flipped in version 1.457, so the first of the duplicates appears on
       // the screen as highlighted.
       if (BackgroundColor1=DEFAULT_DUPLICATE_LEVELS_BACKGROUND_COLOR_2) and
          (BackgroundColor2=DEFAULT_DUPLICATE_LEVELS_BACKGROUND_COLOR_1) then begin
          BackgroundColor1        :=DEFAULT_DUPLICATE_LEVELS_BACKGROUND_COLOR_1;
          BackgroundColor2        :=DEFAULT_DUPLICATE_LEVELS_BACKGROUND_COLOR_2;
          end;
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<459) then begin
       // version 1.459 fixed a mathematical term misunderstanding related to
       // the calculation of the in-between frames for the "stand still"
       // animations. The calculation isn't an extrapolation but an interpolation.
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',OLD_TEXTS_E,NEW_TEXTS_E);
       OpenForm.StrSubstituteInFiles(Skins.ScriptsPath       ,STAR+SKIN_SCRIPT_FILE_EXT,'',OLD_TEXTS_E,NEW_TEXTS_E);
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<461) then begin
       // version 1.461 changed the option name 'Outer wall razor, pixels' to 'Outer wall trimming, pixels'.
       // the skin loader provides backward compatibility; see the function 'Skins_.TSkins.Parse.Parse.Parse.Statements.Statement.SectionStatement'.
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',OLD_TEXTS_F,NEW_TEXTS_F);
       OpenForm.StrSubstituteInFiles(Skins.ScriptsPath       ,STAR+SKIN_SCRIPT_FILE_EXT,'',OLD_TEXTS_F,NEW_TEXTS_F);
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<462) then begin
       // wall caps changed name from 'WallTop' to 'Wall cap' in version 1.447.
       // version 1.447 forgot to change 'wall-top' to 'wall-cap' in the skin
       // import scripts, hence, this new conversion in verson 1.462.
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',OLD_TEXTS_D,NEW_TEXTS_D);
       OpenForm.StrSubstituteInFiles(Skins.ScriptsPath       ,STAR+SKIN_SCRIPT_FILE_EXT,'',OLD_TEXTS_D,NEW_TEXTS_D);
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<506) then begin
       // version 1.506 renamed the skin "Games 4 Brains Animated.bmp" to
       // "Games 4 Brains Animated - Red.bmp".
       if FileExists(StrWithTrailingPathDelimiter(Skins.DefaultSkinPath)+NEW_TEXTS_G[Low(NEW_TEXTS_G)]) then begin
          DeleteFile(StrWithTrailingPathDelimiter(Skins.DefaultSkinPath)+OLD_TEXTS_G[Low(OLD_TEXTS_G)]); // delete the old skin in the user's 'Common skins' folder
          if (not StrEqual(GetApplicationPath,StrWithoutTrailingPathDelimiter(ApplicationDataPath))) then
             DeleteFile(StrWithTrailingPathDelimiter( // delete the old skin in the program's 'Common skins' folder
                          StrSubstitute1(Skins.DefaultSkinPath,
                                         StrWithTrailingPathDelimiter(ApplicationDataPath),
                                         StrWithTrailingPathDelimiter(GetApplicationPath),
                                         i))+
                        OLD_TEXTS_G[Low(OLD_TEXTS_G)]);

          OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',OLD_TEXTS_G,NEW_TEXTS_G);
          OpenForm.StrSubstituteInFiles(Skins.ScriptsPath       ,STAR+SKIN_SCRIPT_FILE_EXT,'',OLD_TEXTS_G,NEW_TEXTS_G);
          StrSubstitutePathsInSettingsAndFileHistory(StrWithTrailingPathDelimiter(Skins.DefaultSkinPath)+OLD_TEXTS_G[Low(OLD_TEXTS_G)],StrWithTrailingPathDelimiter(Skins.DefaultSkinPath)+NEW_TEXTS_G[Low(NEW_TEXTS_G)]);
          end;
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<556) then begin
       // version 1.556 added the YASO optimizer plugin as a multithreaded
       // substitute for the singlethreaded optimizer in the YASS solver and
       // optimizer plugin.
       if StrEqual(Optimizer.PluginFileName, Solver   .DefaultPluginFileName) then // 'True': YASS is the currently selected optimizer; change the selection to YASO
          Optimizer         .PluginFileName:=Optimizer.DefaultPluginFileName;
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<634) then begin
       // On the "Open" window menu, the default value for the option
       // "Show solutions?" was changed to "Yes" in version 1.634, so the list
       // with levels in a file includes a column showing the best found
       // solution for each level.
       // Earlier, that column was disabled by default in order to save screen
       // space, but many users probably never noticed that they could get a
       // very useful overview of their solved levels by enabling that column.
       if OpenForm.SolutionColumns=0 then
          OpenForm.SolutionColumns:=1;
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<647) then begin
       // Antialiasing options (filtering may be a more correct term) got more informative names in version 1.233 and again in version 1.647
       OpenForm.StrSubstituteInFiles(OptionsForm.SettingsPath,STAR+SETTINGS_FILE_EXT   ,'',Antialiasing3Text,AntialiasingText);
       OpenForm.StrSubstituteInFiles(Skins.ScriptsPath       ,STAR+SKIN_SCRIPT_FILE_EXT,'',Antialiasing3Text,AntialiasingText);

       // version 1.647 changed the default maximum skin zoom factor from 999%
       // to 200%. often, the image quality is too poor if the zoom factor
       // exceeds that limit.
       if Assigned( MainForm.GamePictures ) and
          (MainForm.GamePictures.MaxZoomFactorPct=OLD_DEFAULT_ZOOM_FACTOR_PCT) then
           MainForm.GamePictures.MaxZoomFactorPct:=DEFAULT_ZOOM_FACTOR_PCT;
       end;

    if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<654) then begin
       if Status.TransparencyPct =  OLD_DEFAULT_STATUS_TRANSPARENCY_PCT then
          Status.TransparencyPct := DEFAULT_STATUS_TRANSPARENCY_PCT;
       end;

//(*
    Path:=Path+DEFAULT_LEVEL_DIRECTORY;
    if not DirectoryExists(Path) then
       try    ForceDirectories(Path);
       except on Exception do;
       end;
    if DirectoryExists(Path) then begin
       for i:=Low(LEVEL_RES_NAME) to High(LEVEL_RES_NAME) do begin
           FileName:=Path+FILE_NAME_PATH_DELIMITER+StrSubstitute(LEVEL_RES_NAME[i],UNDERSCORE,SPACE,j)+SOKOBAN_FILE_NAME_EXT;

           if (LEVEL_RES_NAME[i]=CRAZY_MONK_RES_NAME) and
              (not FileExists(FileName)) then begin
              s:=Path+FILE_NAME_PATH_DELIMITER+StrSubstitute(CRAZY_MONK_OLD_RES_NAME,UNDERSCORE,SPACE,j)+SOKOBAN_FILE_NAME_EXT;
              if FileExists(s) then {the "Crazy Monk" level collection changed name in version 1.501}
                 if   SysUtils.RenameFile(s,FileName) then
                      StrSubstitutePathsInSettingsAndFileHistory(s,FileName)
                 else Msg(Format(FileRenameFailedText__,[s,FileName]),Application.Title,MB_OK);
              end;

           if (LEVEL_RES_NAME[i]=DU_PELOUX_RES_NAME) and
              (not FileExists(FileName)) then begin
              s:=Path+FILE_NAME_PATH_DELIMITER+StrSubstitute(DU_PELOUX_OLD_RES_NAME,UNDERSCORE,SPACE,j)+SOKOBAN_FILE_NAME_EXT;
              if FileExists(s) then {the "du Peloux"  level collection changed name in version 1.118}
                 if   SysUtils.RenameFile(s,FileName) then
                      StrSubstitutePathsInSettingsAndFileHistory(s,FileName)
                 else Msg(Format(FileRenameFailedText__,[s,FileName]),Application.Title,MB_OK);
              end;

           s:=ChangeFileExt(FileName,XSB_FILE_NAME_EXT);
           if FileExists(s) and (not FileExists(FileName)) then
              ConvertPackFileToTextFile(s); {convert an old-time Sokoban YASC packfile (*.xsb) to text format}

           MakeFile(FileName,LEVEL_RES_NAME[i],RC_LEVEL,False);

           if (OldMajorVersionNumber=1) and (OldMinorVersionNumber<556) then
              // version 1.556 deprecated the use of macros in level notes and
              // solution notes. Perform macroexpansion of the level notes and
              // solution notes, and delete the macro definitions from the file
              // header.
              MacroExpandLevelNotesAndSolutionNotes(FileName);
           end;
       end;

    {$IFDEF MUSIC_PLAYER}
      Path:=ApplicationDataPath+FRACTALS_DIRECTORY;
      ForceDirectories(Path);
      if  DirectoryExists(Path) then
          for i:=Low(PALETTE_RES_NAME) to High(PALETTE_RES_NAME) do
              MakeFile(Path+FILE_NAME_PATH_DELIMITER+PALETTE_RES_NAME[i]+PALETTE_FILE_EXT,PALETTE_RES_NAME[i],RC_PALETTE,False);
    {$ENDIF}
//*)
  finally
    Screen.Cursor:=OriginalCursor;
  end;
end;

{$ENDIF}

function  TMainForm.FormRectToScreenRect(const Rect:TRect):TRect;
begin
  Result:=Classes.Rect(Left+Rect.Left,Top+Rect.Top,Left+Rect.Right,Top+Rect.Bottom);
end;

procedure TMainForm.BtnActivitiesClick(Sender: TObject);
begin
  if Sender is TSpeedButton then begin
     MPlayer.Display.HideMenu(False);
     MPlayer.Display.Activity:=TActivity(TSpeedButton(Sender).Tag);
     MPlayer.ShowButton(Ord(btActivityMenu),-1,-1,bsEnabled,False,False);
     end;
end;

procedure TMainForm.BtnActivitiesSettingsClick(Sender: TObject);
begin
  {$IFDEF MUSIC_PLAYER}
    with OptionsForm.TreeView1 do begin
      Selected:=Items[Ord(stMusicPlayerDisplayActivitiesActivity)];
      Selected.Expand(False);
      end;
    BtnOptionsClick(Sender);
  {$ENDIF}
end;

procedure TMainForm.BtnMandalaStartStopClick(Sender: TObject);
begin
  Mandala.ToggleSuspended;
end;

procedure TMainForm.BtnMandalaSaveAsClick(Sender: TObject);
begin
  Mandala.SaveImageAs;
end;

procedure TMainForm.BtnMandalaSettingsClick(Sender: TObject);
begin
  {$IFDEF MUSIC_PLAYER}
    with OptionsForm.TreeView1 do
      if (Selected=nil)
         or
         (TSettings(Selected.AbsoluteIndex)<stMusicPlayerDisplayActivitiesActivitiesMandala)
         or
         (TSettings(Selected.AbsoluteIndex)>stMusicPlayerDisplayActivitiesActivitiesMandalaMenuFontUnderlined)
         then begin
         Selected:=Items[Ord(stMusicPlayerDisplayActivitiesActivitiesMandala)];
         Selected.Expand(False);
         end;
    BtnOptionsClick(Sender);
  {$ENDIF}
end;

procedure TMainForm.BtnViewer1OpenClick(Sender: TObject);
{$IFDEF MUSIC_PLAYER}
  var oOnHint:TNotifyEvent; oOnMessage:TMessageEvent; DefaultFileName,oFileName:String;
{$ENDIF}
begin
  {$IFDEF MUSIC_PLAYER}
    Viewer1.Suspend;

    DefaultFileName:=ApplicationDataPath+DEFAULT_VALUE+JPG_FILE_EXT;
    ResourceSaveToFile(DefaultFileName,BOARD_BACKGROUND_RES_NAME,RC_JPG);

    OpenForm.InitTask(otImage,osNone,
                      SettingsText,
                      OptionsForm.FullItemName(OptionsForm.TreeView1.Items[Ord(stMusicPlayerDisplayActivitiesActivitiesImageViewerFileName)]),
                      Viewer1.FileName,
                      IMAGE_FILES_FILTER,
                      DefaultFileName,
                      Classes.Rect(-1,-1,-1,-1),
                      TColor(-1),True,False,Classes.Rect(-1,-1,-1,-1));

    oFileName :=Viewer1.FileName;
    oOnHint   :=Application.OnHint;
    oOnMessage:=Application.OnMessage;
    try     if   OpenForm.ShowModal=mrOk then begin
                 if (oFileName=DEFAULT_VALUE) and
                    (OpenForm.CurrentFileName<>DEFAULT_VALUE) then begin
                    IView1_.Data.ResizeToWindowWidth:=False;
                    IView1_.Data.SlideShow:=True;
                    end;
                 MPlayer.Display.RestoreImageIfFading;
                 Viewer1.ShowImage(OpenForm.CurrentFileName);
                 end
            else if IView1_.Data.SlideShow then Viewer1.Resume;
    finally Application.OnMessage:=oOnMessage;
            Application.OnHint:=oOnHint;
    end;
    DeleteFile(ApplicationDataPath+DEFAULT_VALUE+BMP_FILE_EXT);
    DeleteFile(ApplicationDataPath+DEFAULT_VALUE+JPG_FILE_EXT);
    DeleteFile(ApplicationDataPath+DEFAULT_VALUE+WMF_FILE_EXT);
    if not MPlayer.Visible then
       MPlayer.Show;
  {$ENDIF}
end;

procedure TMainForm.BtnViewer1PriorClick(Sender: TObject);
begin
  MPlayer.ResetFps;
  Viewer1.PriorOrNext(True,Sender<>nil);
end;

procedure TMainForm.BtnViewer1NextClick(Sender: TObject);
begin
  MPlayer.ResetFps;
  Viewer1.PriorOrNext(False,Sender<>nil);
end;

procedure TMainForm.BtnViewer1SizeClick(Sender: TObject);
begin
  Viewer1.Suspend;
  IView1_.Data.ResizeToWindowWidth:=not IView1_.Data.ResizeToWindowWidth;
  MPlayer.ResetFps;
  Viewer1.ShowImage(Viewer1.FileName);
end;

procedure TMainForm.BtnViewer1SettingsClick(Sender: TObject);
begin
  {$IFDEF MUSIC_PLAYER}
    Viewer1.Suspend;
    with OptionsForm.TreeView1 do
      if (Selected=nil)
         or
         (TSettings(Selected.AbsoluteIndex)<stMusicPlayerDisplayActivitiesActivitiesImageViewer)
         or
         (TSettings(Selected.AbsoluteIndex)>stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuFontUnderlined)
         then begin
         Selected:=Items[Ord(stMusicPlayerDisplayActivitiesActivitiesImageViewer)];
         Selected.Expand(False);
         end;
    BtnOptionsClick(Sender);
  {$ENDIF}
end;

procedure TMainForm.BtnViewer1SlideShowClick(Sender: TObject);
begin
  MPlayer.Animation:=True;
  Viewer1.PictThread.SlideShow:=not Viewer1.PictThread.SlideShow;
  MPlayer.ResetFps;
  if   Viewer1.PictThread.SlideShow then BtnViewer1NextClick(nil)
  else if Viewer1.Menu.Visible      then Viewer1.Menu.Show;
end;

procedure TMainForm.BtnFworksSettingsClick(Sender: TObject);
begin
{
  with OptionsForm.TreeView1 do
    if (Selected=nil)
       or
       (TSettings(Selected.AbsoluteIndex)<stMusicPlayerDisplayActivitiesActivitiesFireworks)
       or
       (TSettings(Selected.AbsoluteIndex)>stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontUnderlined)
       then begin
       Selected:=Items[Ord(stMusicPlayerDisplayActivitiesActivitiesFireworks)];
       Selected.Expand(False);
       end;
}
  BtnOptionsClick(Sender);
end;

procedure TMainForm.BtnFractalsPriorClick(Sender: TObject);
begin
  Fractals.Prior;
end;

procedure TMainForm.BtnFractalsResetClick(Sender: TObject);
begin
  Fractals.Reset;
end;

procedure TMainForm.BtnFractalsNextClick(Sender: TObject);
begin
  Fractals.Next;
end;

procedure TMainForm.BtnFractalsColorsClick(Sender: TObject);
var oPaletteFileName:String; oOnHint:TNotifyEvent; oOnMessage:TMessageEvent;
    oColorCycling:Boolean;
begin
  oOnHint   :=Application.OnHint;
  oOnMessage:=Application.OnMessage;
  oPaletteFileName:=Fractals.PaletteFileName;
  oColorCycling:=Fractals.ColorCycling;
  try
    if Mandala<>nil then Mandala.Suspend;
    Fractals.ResetDragRect;
    if OpenForm.InitTask(otPalette,osNone,SettingsText,Application.Title+SUB_TITLE_SEPARATOR+FractalsMenuText[0]+SUB_TITLE_SEPARATOR+StrRemoveChar(FractalsMenuText[4],PERIOD),Fractals.PaletteFileName,PALETTE_FILES_FILTER,'',Rect(0,0,0,0),clBlack,True,False,Rect(0,0,0,0)) then begin
       if   OpenForm.ShowModal=mrOk then
            Fractals.PaletteFileName:=OpenForm.CurrentFileName
       else Fractals.PaletteFileName:=oPaletteFileName;
       end;
    Fractals.Finalize;
    Fractals.Show;
    Fractals.ColorCycling:=oColorCycling;
  finally
    Application.OnHint:=oOnHint;
    Application.OnMessage:=oOnMessage;
  end;
end;

procedure TMainForm.BtnFractalsSaveAsClick(Sender: TObject);
begin
  Fractals.SaveAs;
end;

procedure TMainForm.BtnFractalsSettingsClick(Sender: TObject);
begin
  {$IFDEF MUSIC_PLAYER}
    with OptionsForm.TreeView1 do
      if (Selected=nil)
         or
         (TSettings(Selected.AbsoluteIndex)<stMusicPlayerDisplayActivitiesActivitiesFractals)
         or
         (TSettings(Selected.AbsoluteIndex)>stMusicPlayerDisplayActivitiesActivitiesFractalsMenuFontUnderlined)
         then begin
         Selected:=Items[Ord(stMusicPlayerDisplayActivitiesActivitiesFractals)];
         Selected.Expand(False);
         end;
    BtnOptionsClick(Sender);
  {$ENDIF}
end;

procedure TMainForm.SetBusyMode(IsBusy__:Boolean);
var ThreadExecutionStateFlags:Cardinal;
begin
  IsBusy__:=(IsBusy__
             or
             // ensure that the plugins runs in 'busy' mode
             (Assigned(Solver   ) and Solver   .IsActive)
             or
             (Assigned(Optimizer) and Optimizer.IsActive)
             or
             (Assigned(Generator) and Generator.IsActive)
            )
            and
            (not ShutDownApplication);

  ThreadExecutionStateFlags:=ES_CONTINUOUS;
  if not ShutDownApplication then begin
     if IsBusy__               then Inc(ThreadExecutionStateFlags,ES_AWAYMODE_REQUIRED or ES_SYSTEM_REQUIRED);
     if not ScreenSaverEnabled then Inc(ThreadExecutionStateFlags,ES_DISPLAY_REQUIRED);
     end;
  SetThreadExecutionState(ThreadExecutionStateFlags);

  ApplicationTimer.Enabled:=(IsBusy__ or (not ScreenSaverEnabled)) and (not ShutDownApplication);
  if ApplicationTimer.Enabled and Initialized then
     ApplicationTimerTimer(Self); // if the timer is enabled, then trigger a timer event now; it's unnecessary but it doesn't hurt
end;

procedure TMainForm.SetDefaultFormSize;
var oCursor:TCursor;
begin
  if (WindowState=wsNormal) and
     ((Width<>DEFAULT_FORM_WIDTH) or (Height<>DEFAULT_FORM_HEIGHT)) then begin
     oCursor:=Screen.Cursor;
     try Screen.Cursor:=crHourGlass;
         Initialized  :=False;
         Width        :=DEFAULT_FORM_WIDTH;
         Height       :=DEFAULT_FORM_HEIGHT;
         Initialized  :=True;
         FormResize(Self);
     finally
       Initialized    :=True;
       Screen.Cursor  :=oCursor;
     end;
     end;
end;

procedure TMainForm.SetScreenSaverEnabled(Enabled__:Boolean);
begin
  fScreenSaverEnabled:=Enabled__;
  BusyMode:=False;
end;

procedure TMainForm.BtnSnapshotsClick(Sender: TObject);
var Index:Integer; SecondaryScoreMetrics:TSecondaryScoreMetrics;
    IdenticalSnapshot,Snapshot:TSnapshot;
begin
  if BtnSnapshots.Enabled then begin
     if MPlayer.Visible then MPlayer.Hide;
     if Game.IsIdleAndStopReplayingAndBrowsing then with SnapshotsForm do begin
        TrackState:=tsWait; ClearTrackBox(True);

        if Game.LookupSnapshot(Game.ReverseMode,Game.History.Count,Game.History.Top,PHistoryMoves(Addr(Game.History.Moves)),IdenticalSnapshot) then // 'True': an identical snapshot already exists
           Index:=SnapshotsForm.IndexOf(IdenticalSnapshot) // look up the existing snapshot in the grid in the "Snapshots" window
        else begin
           Index:=Grid.Row; IdenticalSnapshot:=nil;
           end;

        Snapshot:=Snapshots[Index];

{       // avoid saving 0-move snapshots;
        // to put this into production requires changes of the hint-text
        if   (Sender<>nil) and
             (Self.Game.History.Count<=Self.Game.ForcedInitialJumps) then begin
             if not Visible then Show
             else SetFocus;
             if Grid.Visible then Grid.SetFocus;
             end
        else
}
             if (Snapshot<>nil) and
                (Snapshot.ReverseMode=Game.ReverseMode) and
//              Self.Game.IsIdenticalBoard(Snapshot.Board,True,True)
                Self.Game.IsIdenticalBoxPosition(Snapshot.PlayerPos,Snapshot.BoxPos,True,True)
                and
                (((Sender<>nil) // non-nil: save snapshot
                  // if   current game is equal to, or worse than selected snapshot,
                  // then show snapshot instead of saving current game
                  and
                  ((Self.Game.History.Count    >=Snapshot.MoveCount)
                   and
                   (Self.Game.History.PushCount>=Snapshot.PushCount)
                   and
                   Self.Game.CalculateSecondaryScoreMetrics(SecondaryScoreMetrics)
                   and
                   (CompareSecondaryScoreMetrics(SecondaryScoreMetrics,Snapshot.SecondaryScoreMetrics)>=0) // '>=0': the game secondary metrics is worse than or equal to the snapshot secondary metrics
                   and
                   ((Snapshot.GameState<>gsSolved)
                    or
                    Assigned(IdenticalSnapshot)
                    // if   the current game state is a new unsaved solution,
                    // then save it now even if it's inferior to the selected
                    //      solution
                   )
                  )
                 )
                 or
                 ((Sender=nil) // nil: open snapshot
                  // if   current game is equal to, or better than selected snapshot,
                  // then show snapshot instead of loading selected snapshot
                  and
                  ((Self.Game.History.Count    <=Snapshot.MoveCount)
                   or
                   (Self.Game.History.PushCount<=Snapshot.PushCount)
                   or
                   (Self.Game.CalculateSecondaryScoreMetrics(SecondaryScoreMetrics)
                    and
                    (CompareSecondaryScoreMetrics(SecondaryScoreMetrics,Snapshot.SecondaryScoreMetrics)<=0) // '<=0': the game secondary metrics is better than or equal to the snapshot secondary metrics
                   )
                  )
                 )
                ) then begin
                if   not  Visible then Show
                else if   WindowState=wsMinimized then WindowState:=wsNormal
                     else SetFocus;
                if Index<>Grid.Row then Grid.Row:=Index;
                if Grid.Visible    then Grid.SetFocus;
                end
             else
                if Sender<>nil then begin
                   if (Sender=Self) or // 'Self': keyboard 'New' command
                      (AnsiPos(HintSnapshotsLeftClickText,BtnSnapshots.Hint)=1) then begin
                      FormActivate(Self);
                      try     PanelNewClick(Sender);
                      finally FormDeactivate(Self);
                      end;
                      end;
                   if not Visible then Show
                   else if WindowState=wsMinimized then WindowState:=wsNormal
                        else Self.SetFocus; // focus 'MainForm' (again)
                   end
                else
                   if   Snapshot<>nil then
                        if   SnapshotsForm.IsASolution(Snapshot) then begin
                             // it's a dilemma what to do when the currently
                             // selected snapshot is a solution;
                             // there are 4 possible actions:
                             // 1: load it as solutions normally are loaded
                             //    from the snapshow window, meaning it shows up
                             //    as the starting position; the user will think
                             //    the game has been reset, and that nothing was
                             //    loaded;
                             // 2. load the solution, but show the terminal
                             //    position; this is consistent with how
                             //    snapshots normally are loaded, and the user
                             //    will understand that the currently selected
                             //    item in the snapshot window has been loaded;
                             //    however, the user will be annoyed by seeing
                             //    the terminal position for the solution;
                             // 3. don't load the solution but reset the game
                             //    instead; this behavior can to some extent be
                             //    justified by stating that the option
                             //    "right-click opens currently selected
                             //    snapshot" applies to non-solution snapshots
                             //    only;
                             // 4. don't load anything but direct the focus to
                             //    the selected solution in the snapshot window;
                             // currently, the program implements the third
                             // action

                             {
                             // 1: open currently selected solution, showing the start position
                             PanelOpenClick(Sender);
                             }
                             {
                             // 2: open currently selected solution, showing the terminal position
                             (not implemented)
                             }

                             // 3: reset game
                             BtnResetClick (Sender);

                             {
                             // 4: focus solution in snapshot window
                             if not Visible then Show
                             else if WindowState=wsMinimized then WindowState:=wsNormal
                                  else SetFocus;
                             if Grid.Visible then Grid.SetFocus;
                             }

                             end
                        else PanelOpenClick(Sender) // open currently selected snapshot
                   else BtnResetClick (Sender); // reset game
        end;
     end;
end;

procedure TMainForm.MakeDynamicHints(Snapshot__:TSnapshot);
var LoadSnapshotIsOk,SaveSnapshotIsOk:Boolean;
begin // if 'Snapshot__' = 'nil' then the created hints refer to the currently selected (and visible) snapshot or solution in the 'Snapshots and Solutions' window, if any
  SaveSnapshotIsOk:=(SnapshotsForm<>nil) and SnapshotsForm.Visible and SnapshotsForm.PanelNew.Enabled and (not ShutDownApplication);
  if   SaveSnapshotIsOk then begin
       if Snapshot__=nil then Snapshot__:=SnapshotsForm.Snapshots[SnapshotsForm.Grid.Row]; // 'nil': make hints for the currently selected snapshot
       end
  else Snapshot__:=nil;
  LoadSnapshotIsOk:=SaveSnapshotIsOk and                                        // 'True': the 'Snapshots' window is visible, and adding new snapshots is enabled
                    Assigned(Snapshot__) and                                    // 'True': a snapshot or solution is selected in the 'Snapshots' window
                    (not SnapshotsForm.IsASolution(Snapshot__));                // 'True': the selected snapshot isn't a solution

  if   SaveSnapshotIsOk then
       if   LoadSnapshotIsOk then
            if   OnRightClickAction=orcaLoadSnapshotIfAnyElseRestartGame then
                 BtnSnapshots.Hint:=HintSnapshotsLeftClickText+HintSnapshotsRightClickText
            else BtnSnapshots.Hint:=HintSnapshotsLeftClickText
       else BtnSnapshots.Hint:=HintSnapshotsLeftClickText
  else BtnSnapshots.Hint:=HintSnapshotsText; // 'click to open window' message

  if   SaveSnapshotIsOk then
       if   LoadSnapshotIsOk then
            if   OnRightClickAction=orcaRestartGame then
                 BtnReset.Hint:=HintResetOnHOMEtext+HintRightClickIsAShortCutText
            else BtnReset.Hint:=HintResetOnHOMEtext
       else if   OnRightClickAction=orcaUndoMove then
                 BtnReset.Hint:=HintResetOnHOMEandENTERtext
            else BtnReset.Hint:=HintResetOnHOMEandENTERtext+HintRightClickIsAShortCutText
  else if   OnRightClickAction=orcaUndoMove then
            BtnReset.Hint:=HintResetOnHOMEandENTERtext
       else BtnReset.Hint:=HintResetOnHOMEandENTERtext+HintRightClickIsAShortCutText;

  MakeSolutionButtonHint;
end;

procedure TMainForm.MakeUndoButtonHint;
begin
  if   OnRightClickAction=orcaUndoMove then
       BtnUndo.Hint:=HintUndoText+HintRightClickIsAShortCutText
  else BtnUndo.Hint:=HintUndoText;
end;

procedure TMainForm.MakeSolutionButtonHint;
var s:String; BetterBuiltInSolutionMoves,BetterBuiltInSolutionPushes:TSnapshot;
begin
  if             Game=nil then s:=''
  else if        (Game.BestSolutionMoves<>nil) and (Game.BestSolutionPushes<>nil) then
                 if   ShowSolutionMoves then
                      s:=Format(HintShowSolutionMovesAndPushesText__,[Game.BestSolutionMoves.MoveCount,
                                                                      Game.BestSolutionMoves.PushCount,
                                                                      SecondaryMetricsFormattedAsATitleSuffix(SecondaryMetricsInSolutionButtonHint,Game.BestSolutionMoves.SecondaryScoreMetrics),
                                                                      Game.BestSolutionPushes.MoveCount,
                                                                      Game.BestSolutionPushes.PushCount,
                                                                      SecondaryMetricsFormattedAsATitleSuffix(SecondaryMetricsInSolutionButtonHint,Game.BestSolutionPushes.SecondaryScoreMetrics)])
                 else s:=Format(HintShowSolutionPushesAndMovesText__,[Game.BestSolutionPushes.MoveCount,
                                                                      Game.BestSolutionPushes.PushCount,
                                                                      SecondaryMetricsFormattedAsATitleSuffix(SecondaryMetricsInSolutionButtonHint,Game.BestSolutionPushes.SecondaryScoreMetrics),
                                                                      Game.BestSolutionMoves.MoveCount,
                                                                      Game.BestSolutionMoves.PushCount,
                                                                      SecondaryMetricsFormattedAsATitleSuffix(SecondaryMetricsInSolutionButtonHint,Game.BestSolutionMoves.SecondaryScoreMetrics)])
       else if   Game.BestSolutionMoves<>nil then s:=Format(HintShowSolution1Text__,[Game.BestSolutionMoves.MoveCount,
                                                                                     Game.BestSolutionMoves.PushCount,
                                                                                     SecondaryMetricsFormattedAsATitleSuffix(SecondaryMetricsInSolutionButtonHint,Game.BestSolutionMoves.SecondaryScoreMetrics)])
            else s:=HintNoSolutionAvailableText;

  if   (Game<>nil) and
       Game.IsABetterBuiltInSolutionAvailable(BetterBuiltInSolutionMoves,BetterBuiltInSolutionPushes) then
       if ((ShowSolutionMoves or (Game.BestSolutionPushes=nil)) and
           (Game.BestSolutionMoves     <>nil)  and
           (BetterBuiltInSolutionMoves <>nil))
          or
          ((not ShowSolutionMoves) and
           (Game.BestSolutionPushes    <>nil)  and
           (BetterBuiltInSolutionPushes<>nil)) then
          s:=s+HintBetterBuiltInSolutionAvailableText
       else
          s:=s+HintBuiltInSolutionAvailableText;

  if s<>'' then s:=s+HintOpenSolverText+HintReplaySolutionsForSolvedLevelsText;

  BtnSolution.Hint:=s;
end;

procedure TMainForm.SetDragCursorType(DragCursorType__:TCursorType);
begin
  fDragCursorType:=DragCursorType__;
  DragCursor:=CURSOR_TYPE_TO_CURSOR[DragCursorType];
  Image1.DragCursor:=DragCursor;
end;

procedure TMainForm.SetSelectionCursorType(SelectionCursorType__:TCursorType);
begin
  fSelectionCursorType:=SelectionCursorType__;
  SelectionCursor:=CURSOR_TYPE_TO_CURSOR[SelectionCursorType];
end;

procedure TMainForm.SetTrackingCursorType(TrackingCursorType__:TCursorType);
begin
  fTrackingCursorType:=TrackingCursorType__;
  TrackingCursor:=CURSOR_TYPE_TO_CURSOR[TrackingCursorType];
end;

procedure TMainForm.SetMessageHandlers;
begin
  Application.OnHint      :=ShowHint;
  Application.OnActivate  :=ApplicationOnActivate;
  Application.OnDeactivate:=ApplicationOnDeactivate;
  Application.OnMessage   :=ApplicationOnMessage;
end;

procedure TMainForm.OnCopyData(var Msg: TWMCopyData);
var s:String; Sender:TObject;
begin
  if Assigned(SnapshotsForm) and (Screen.ActiveForm=SnapshotsForm) then begin
     if SnapshotsForm.WindowState=wsMinimized then
        SnapshotsForm.WindowState:=wsNormal;
     SnapshotsForm.FormDeactivate(Self);
     if Self.Visible then Self.SetFocus;
     end;

  if (Screen.ActiveForm=Self) then
     try    Application.Restore;
            Application.BringToFront;
            if WindowState=wsMinimized then WindowState:=wsNormal;
            if MPlayer<>nil then MPlayer.Hide;

            SetString(s, PChar(Msg.CopyDataStruct^.lpData), Msg.CopyDataStruct^.cbData);
            if   Screen.ActiveForm=ToolsForm then Sender:=ToolsForm
            else Sender:=Self;
            if CloseLevel(Sender) then begin
               if   FileExists(s) and LoadGame(s,True) then //
               else Game.FileName:='';
               if Game.FileName='' then ShowTitle('');
               end;
     except on E:Exception do Error(E.Message,Application.Title);
     end
  else begin
     // The command is ignored if the main-window hasn't focus.
     // This is not strictly necessary in all circumstances,
     // but it would be gambling to trust that it works otherwise.
     // One situation where it wouldn't work, is when then
     // musicplayer's "Open" window has focus.
     // It's shown modally, and when it returns, the musicplayer
     // makes all sorts of changes of the message-handlers.
     end;
end;

procedure TMainForm.OnDisplayChange(var Msg: TMessage);
begin
  if DisplayDriverChangedAction=ddShutdown then begin
     Error(WindowsDisplayDriverChangedText,'');
     Close;
     end;
end;

procedure TMainForm.OnUpdateDeadSquares(var Msg: TWMCopyData); // Message Misc_.MSG_UPDATE_DEAD_SQUARES;
//var b:Boolean;
begin
  if Assigned(Deadlocks) then Deadlocks.LoadBoard; // 'LoadBoard' flags any additional dead squares found by the deadlock calculation
  IdleStartTime:=0; // trigger the 'OnIdle' handler as soon as possible
  // let the 'OnIdle' handler do the refresh if new dead squares were found by the deadlock calculation;
  // the 'OnIdle' handler refreshes the board at a time when it's appropriate, e.g., not during replay
  //OnIdleHandler(Self,b);
end;

procedure TMainForm.BtnStatusMenuStopClick(Sender: TObject);
begin
  // do nothing, but the procedure must be defined for triggering a menu-click sound
end;

procedure TMainForm.BtnStatusMenuTimerClick(Sender: TObject);
begin
  // do nothing, but the procedure must be defined for triggering a menu-click sound
end;

procedure TMainForm.BtnStatusMenuDeadlocksClick(Sender: TObject);
begin //
  if (Deadlocks<>nil) and (Deadlocks.Thread<>nil) then begin
     Deadlocks.Thread.State:=Ord(dlStopCalculation);
     Status.EnableDisableButtons;
     end;
end;

procedure TMainForm.BtnStatusMenuPluginClick(Sender: TObject);
begin
  if Game.IsIdleAndStopReplayingAndBrowsing then begin
     if Menu  <>nil then        Menu.ItemIndex:=-1;
     if Status<>nil then Status.Menu.ItemIndex:=-1;

     if      Sender=BtnStatusMenuSolver then begin // solver
             with OptionsForm.TreeView1 do begin
               Selected:=Items[Ord(stToolsSolverFileName)];
               // kludge:  select the item twice;
               // the first time ensures that the path is expanded,
               // but the existing 'OptionsForm.TreeView1.OnExpanded' changes
               // 'Selected' when it expands the items on the path down to the
               // specified item, hence, a second call to 'Selected:=' is
               // necessary to ensure that the correct item is selected;
               // this applies to all the selections in this procedure
               Selected:=Items[Ord(stToolsSolverFileName)];
               end;
             OptionsForm.UpdateSettings(-Ord(stToolsSolverFileName),Solver.PluginFileName,Self);
             BtnToolsClick(ToolsForm.TabSheetSolver);
             end
     else if Sender=BtnStatusMenuOptimizer then begin // optimizer
             with OptionsForm.TreeView1 do begin
               Selected:=Items[Ord(stToolsOptimizerFileName)];
               Selected:=Items[Ord(stToolsOptimizerFileName)];
               end;
             OptionsForm.UpdateSettings(-Ord(stToolsOptimizerFileName),Optimizer.PluginFileName,Self);
             BtnToolsClick(ToolsForm.TabSheetOptimizer);
             end
     else if Sender=BtnStatusMenuGenerator  then begin // generator
             with OptionsForm.TreeView1 do begin
               Selected:=Items[Ord(stToolsGeneratorPriority)];
               Selected:=Items[Ord(stToolsGeneratorPriority)];
               end;
             BtnToolsClick(ToolsForm.TabSheetGenerator);
             end
     else if Sender=ToolsForm then begin // use the 'Open' form to select a plugin
             with OptionsForm.TreeView1 do
               if ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver then begin
                  Selected:=Items[Ord(stToolsSolverFileName)];
                  Selected:=Items[Ord(stToolsSolverFileName)];
                  end
               else begin
                  Selected:=Items[Ord(stToolsOptimizerFileName)];
                  Selected:=Items[Ord(stToolsOptimizerFileName)];
                  end;
             OptionsForm.Panel1Click(Self);
             MakeDynamicHints(nil);
             ShowStatus;
             if      ShutDownApplication then begin
                     Msg(ApplicationShutdownText,Application.Title,0);
                     BtnExitClick(Sender);
                     end
             else if SnapshotsForm.ShowOnStartUp and (Screen.ActiveForm=Self) then begin
                     SnapshotsForm.Show;
                     if SnapshotsForm.WindowState=wsMinimized then
                        SnapshotsForm.WindowState:=wsNormal;
                     SnapshotsForm.FormDeactivate(Self);
                     if Self.Visible then Self.SetFocus;
                     end;
             end;
     end;
end;

procedure TMainForm.PopupMenuPopup(Sender: TObject);
begin //
end;

procedure TMainForm.BtnMenuClick(Sender: TObject);
var APoint:TPoint;
begin
  if Menu.BtnMenuIndex<>-1 then with Menu do begin
     MenuItemOpenPreviousLevel  .Visible:=BtnPreviousLevelIndex=-1;
     MenuItemOpenNextLevel      .Visible:=MenuItemOpenPreviousLevel.Visible;
     MenuItemOpenLevelSeparator .Visible:=MenuItemOpenPreviousLevel.Visible;
     MenuItemSave               .Visible:=(BtnSaveIndex=-1) and BtnSave.Enabled;
     MenuItemSaveAs             .Visible:=MenuItemSave.Visible;
     MenuItemSaveSeparator      .Visible:=MenuItemSave.Visible;
     MenuItemOpenClipboard      .Visible:=True;
     MenuItemCopyToClipboard    .Visible:=True;
     MenuItemClipboardSeparator .Visible:=MenuItemOpenClipboard.Visible or MenuItemCopyToClipboard.Visible;
     MenuItemRotateAndMirror    .Visible:=(BtnRotateIndex=-1) and BtnRotate.Enabled;
     MenuItemNormalMode         .Visible:=(BtnReverseModeIndex=-1) and BtnSave.Enabled and (Game<>nil) and Game.ReverseMode;
     MenuItemReverseMode        .Visible:=(BtnReverseModeIndex=-1) and BtnSave.Enabled and (Game<>nil) and (not Game.ReverseMode);
     MenuItemSolution           .Visible:=BtnSolutionIndex=-1;
     MenuItemSolutionSeparator1 .Visible:=MenuItemSolution.Visible;
     MenuItemSolutionSeparator2 .Visible:=MenuItemSolution.Visible;
     MenuItemTools              .Visible:=True;
     MenuItemToolsSeparator     .Visible:=MenuItemTools.Visible;
     MenuItemSettings           .Visible:=BtnSettingsIndex=-1;
     MenuItemWindowSize         .Visible:=True;
     MenuItemSettingsSeparator  .Visible:=True; //MenuItemSettings.Visible;
     MenuItemMusicPlayer        .Visible:={$IFDEF MUSIC_PLAYER} (BtnMPlayerIndex=-1) {$ELSE} False {$ENDIF};
     MenuItemHelp               .Visible:=MenuItemMusicPlayer.Visible;
     MenuItemHelpSeparator      .Visible:=MenuItemHelp.Visible;

     MenuItemOpenPreviousLevel  .Enabled:=BtnOpenPrior.Enabled;
     MenuItemOpenNextLevel      .Enabled:=BtnOpenNext.Enabled;
     MenuItemSave               .Enabled:=BtnSave.Enabled;
     MenuItemSaveAs             .Enabled:=BtnSaveAs.Enabled;
     MenuItemOpenClipboard      .Enabled:=BtnOpenClipboard.Enabled;
     MenuItemCopyToClipboard    .Enabled:=BtnSave.Enabled;

     MenuItemSolution           .Visible:=(BtnSolutionIndex=-1) and
                                          (Game.BestSolutionMoves=nil) or (Game.BestSolutionPushes=nil);
     MenuItemSolution           .Enabled:=BtnSolution.Enabled;

     MenuItemSolutionMoves      .Visible:=(BtnSolutionIndex=-1) and
                                          (not MenuItemSolution.Visible);
     MenuItemSolutionPushes     .Visible:=MenuItemSolutionMoves.Visible;

     MenuItemTools              .Enabled:=BtnTools.Enabled;
     MenuItemSettings           .Enabled:=BtnOptions.Enabled;
     MenuItemMusicPlayer        .Enabled:=BtnMPlayer.Enabled;
     MenuItemHelp               .Enabled:=BtnHelp.Enabled;
     MenuItemWindowSizeMaximized.Checked:=WindowState=wsMaximized;

     APoint:=ClientToScreen(Point(ButtonDX,MenuItems[BtnMenuIndex].Rect.Top));
     with APoint do PopupMenu.Popup(X,Y);
     end;
end;

procedure TMainForm.BtnRotateClick(Sender: TObject);
var APoint:TPoint;
begin
  if (RotateAndFlipPopupMenu<>nil) and (Menu<>nil) then with PanelRotateAndFlip do begin
     if   Menu.BtnRotateIndex<>-1 then with Menu do
          APoint:=Point(ButtonDX,Max(Menu.ButtonDY,MenuItems[BtnRotateIndex].Rect.Top-PanelRotateClockwise.Height))
     else APoint:=Point(Menu.ButtonDX,Menu.ButtonDY);
    Left:=APoint.X; Top:=APoint.Y;
    PanelResetTransformations.Enabled:=Game.BoardTransformation2D<>t2DRotate0DegreesClockwise;
    RotateAndFlipPopupMenu.Show;
    end;
end;

procedure TMainForm.PanelRotateAndFlipClick(Sender: TObject);
var oVisible:Boolean; oCursor:TCursor;
begin
  if (MPlayer<>nil) and MPlayer.Visible then MPlayer.Hide;
  ClearTrackBox(False);
  if (Game<>nil) and (Game.FileName<>'') and (Menu<>nil) and
     Game.IsIdleAndStopReplayingAndBrowsing and
     (RotateAndFlipPopupMenu<>nil) then begin
     oVisible:=RotateAndFlipPopupMenu.Visible;
     oCursor:=Screen.Cursor;
     try
       Screen.Cursor:=crHourGlass;
       if      Sender=PanelRotateCounterClockwise then
               Game.DoBoardTransformation2D(t2DRotate270DegreesClockwise,False)
       else if Sender=PanelRotateClockwise then
               Game.DoBoardTransformation2D(t2DRotate90DegreesClockwise,False)
       else if Sender=PanelFlipVertically then
               Game.DoBoardTransformation2D(t2DFlipVertically,False)
       else if Sender=PanelFlipHorizontally then
               Game.DoBoardTransformation2D(t2DFlipHorizontally,False)
       else if Sender=PanelResetTransformations then
               Game.DoBoardTransformation2D(BOARD_TRANSFORMATION_INVERSE[Game.BoardTransformation2D],False);
     finally Screen.Cursor:=oCursor;
     end;
     ShowTitle(Game.FileName);

     InitGame(False,True,True,False,True,False,True,SnapshotsForm.Grid.Row);
     PanelResetTransformations.Enabled:=Game.BoardTransformation2D<>t2DRotate0DegreesClockwise;
     RotateAndFlipPopupMenu.EnableDisableButton(PanelResetTransformations);
     if (Sender<>nil) and (Sender is TPanel) and (Status<>nil) then
        with Sender as TPanel do Status.Hint:=GetLongHint(Hint);
     if oVisible and (not RotateAndFlipPopupMenu.Visible) then
        RotateAndFlipPopupMenu.Show;
     MultiView.DisappearedItemsCount:=0;
     MultiView.OnResize(nil);
     if MultiView.ReportDisappearedItems(MultiView.DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
        MultiView.DoReportDisappearedItems:=False;
     end;
end;

procedure TMainForm.SetBoardTransformation2D(BoardTransformation2D__:TBoardTransformation2D);
begin
  if (Game<>nil) and Game.IsIdleAndStopReplayingAndBrowsing and
     (BoardTransformation2D__<>Game.BoardTransformation2D) then begin
     Game.DoBoardTransformation2D(BOARD_TRANSFORMATION_INVERSE[Game.BoardTransformation2D],False);
     Game.DoBoardTransformation2D(BoardTransformation2D__,False);
     PanelRotateAndFlipClick(nil);
     end;
end;

procedure TMainForm.BtnSplitViewClick(Sender: TObject);
begin
  if Game.IsIdleAndStopReplayingAndBrowsing and
     BtnSplitView.Enabled then with MultiView do begin
     SplitItem(Selected,SplitAxis,True);
     end;
end;

procedure TMainForm.PanelMultiViewMenuClick(Sender: TObject);
var Count:Cardinal; GameReady:Boolean;
begin
  if (MPlayer<>nil) and MPlayer.Visible then MPlayer.Hide;
  TrackState:=tsWait; ClearTrackBox(False);
  if (Game<>nil) and (Game.FileName<>'') and (Menu<>nil) and
     Game.IsIdleAndStopReplayingAndBrowsing and
     (MultiViewPopupMenu<>nil) then begin
     if      Sender=PanelMultiViewCopyMovesToClipboard then
             Game.CopyMovesToClipboard(False,False)
     else if Sender=PanelMultiViewCopyContinuationMovesToClipboard then
             Game.CopyMovesToClipboard(True,False)
     else if Sender=PanelMultiViewPasteMovesFromClipboard then begin
             ClearTrackBox(True);
             if Game.IsIdleAndStopReplayingAndBrowsing then
                if   Clipboard.HasFormat(CF_TEXT) then begin
                     if LoadSolutionsAndSnapshotsForCurrentGame('',False,MultiView.Selected,GameReady) then begin
                        if   IsANewFileName(Game.FileName) then
                             Modified:=True
                        else Game.Notes.Modified:=True; // 'Notes.Modified' later triggers a silent 'SaveToFile', in contrast to 'MainForm.Modified' which asks the user before saving
                        ShowStatus;
                        end;
                     end
                else Msg(TEXT_CLIPBOARD_NO_TEXT,
                         Application.Title+SUB_TITLE_SEPARATOR+TEXT_OPEN_LEVEL_FROM_CLIPBOARD,
                         MB_OK+MB_ICONINFORMATION);
             end
     else if (Sender=PanelMultiViewCombineSnapshotsToFormSolutions) then
             MultiView.CombineSnapshotsToFormSolutions(True)
     else if (Sender=PanelMultiViewMoveToSnapshots) and
             Assigned(MultiView.Selected) and
             MultiView.Selected.MakeSnapshot and
             MultiView.Selected.SaveSnapshot(Count) then with MultiView do begin
             CloseItem(Selected,False);
             DisappearedItemsCount:=0;
             OnResize(nil); // recalculate information and maximize all view items so they fill out the screen
             if MultiView.ReportDisappearedItems(MultiView.DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
                MultiView.DoReportDisappearedItems:=False;
             end
     else if (Sender=PanelMultiViewCloseView) and
             Assigned(MultiView.Selected) then with MultiView do begin
             CloseItem(Selected,False);
             DisappearedItemsCount:=0;
             OnResize(nil); // recalculate information and maximize all view items so they fill out the screen
             if MultiView.ReportDisappearedItems(MultiView.DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
                MultiView.DoReportDisappearedItems:=False;
             end;

     MultiViewPopupMenu.Hide;
     end;
end;

procedure TMainForm.ShowDeadlockedBoxes;
var i,Col,Row:Integer; s:String;
begin
  if (Game<>nil) and (GameViewer<>nil) and (Game.DeadlockDetection.Deadlocks<>nil) and
     (Screen.ActiveForm=Self) and (not MPlayer.Visible) and
     Game.IsIdleAndStopReplayingAndBrowsing and
     (Game.DeadlockDetection.Deadlocks.DeadlockedBoxCount<>0) then with Game do with DeadlockDetection.Deadlocks do begin
     StartHintCount:=0;

     for Col:=1 to BoardWidth do
         for Row:=1 to BoardHeight do Board[Col,Row]:=Board[Col,Row] and (not BOX_SET_DEADLOCK);

     for i:=1 to DeadlockedBoxCount do with DeadlockedBoxSquares[i] do
         Inc(Board[x,y],BOX_SET_DEADLOCK);

     if GameViewer.LegalMovesInfo.DeadlocksEnabled then
        GameViewer.ShowLegalMoves(BOX_SET_DEADLOCK,BoxPos[0],GameViewer.LegalMovesInfo.DeadlocksColor);

     s:=DeadlockedPositionText;
     if History.Count>0 then s:=s+SPACE+PleaseBacktrackText;
     if OverflowingDeadlockSet<>0 then
        s:=s+SPACE+LEFT_PAREN+IntToStr(OverflowingDeadlockSet)+RIGHT_PAREN;
     Status.Hint:=s;

     Sound.Play(stBlockedStep);

//   if DeadlockDetection.Deadlocks.DeadlockStats<>'' then
//      Status.Hint:=DeadlockDetection.Deadlocks.DeadlockStats;
     end;
end;

procedure TMainForm.BtnSkinClick(Sender: TObject);
begin // actually, there is no skin-button but only a keyboard shortcut
 if (Game<>nil) and Game.IsIdleAndStopReplayingAndBrowsing and
    (Screen.ActiveForm=Self) and
    (OpenForm<>nil) and (OptionsForm<>nil) then begin
    if MPlayer<>nil then MPlayer.Hide;
    ClearTrackBox(True);
    if WindowState<>wsNormal then WindowState:=wsNormal;
    Update;
    OptionsForm.LoadData;
    if (Sender=BtnOpenNext) or (Sender=BtnOpenPrior) or (Sender=nil) then
       OptionsForm.MenuItemLoadSkinClick(Sender)
    else begin
       OptionsForm.SetDefaultSkinMenuItemCheckMark(OptionsForm.MenuItemDefaultSkinSeamlessWalls);
       OptionsForm.MenuItemDefaultSkinClick(Sender);
       end;
    if OptionsForm.ModalResult=mrOk then SelectOptions(nil);
    end;
end;

function TMainForm.CopyLevelToClipboard(OppositeFillFloorsSetting__,RunLengthEncoding__,BoardOnly__,CurrentBoardOnly__,NormalizedLevel__:Boolean):Boolean;
var FloorFillChar:Char;
begin
  if        RunLengthEncoding__ then
            FloorFillChar:=FLOOR_CH // don't fill the floors before run-len encoding; it uses 'RunLengthEncodingFloor' itself
  else if   (     CopyLevelToClipboardFillFloors  and (not OppositeFillFloorsSetting__))
            or
            ((not CopyLevelToClipboardFillFloors) and      OppositeFillFloorsSetting__ ) then
            FloorFillChar:=CopyLevelToClipboardFloorFillCharacter
       else FloorFillChar:=FLOOR_CH;
  if   (Game<>nil) and (Game.GameState<>gsNull) and Game.IsIdleAndStopReplayingAndBrowsing then begin
       if MPlayer.Visible then MPlayer.Hide;
       if   BoardOnly__ and CurrentBoardOnly__ and NormalizedLevel__ then begin
            if   Assigned(OpenForm) and MakeNormalizedLevel(OpenForm.Game) then
                 Result:=OpenForm.Game.CopyToClipboard(FloorFillChar,CopyLevelToClipboardPreserveCombinedMoves,RunLengthEncoding__,BoardOnly__,CurrentBoardOnly__)
            else Result:=Error(TEXT_TASK_FAILED,Application.Title);
            end
       else Result:=Game.CopyToClipboard(FloorFillChar,CopyLevelToClipboardPreserveCombinedMoves,RunLengthEncoding__,BoardOnly__,CurrentBoardOnly__);
       end
  else Result:=False;
end;

procedure TMainForm.MenuItemCopyToClipboardClick(Sender: TObject);
begin
  CopyLevelToClipboard(Sender=nil,False,False,False,False);
end;

procedure TMainForm.MenuItemWindowSizeClick(Sender: TObject);
begin
  if      Sender=MenuItemWindowSizeDefault then begin
          if WindowState<>wsNormal then WindowState:=wsNormal;
          SetDefaultFormSize;
          end
  else if Sender=MenuItemWindowSizeDefaultCentered then begin
          MenuItemWindowSizeClick(MenuItemWindowSizeDefault);
          Left:=Max(Screen.DeskTopLeft,(Screen.DeskTopLeft+Screen.DeskTopWidth -Width ) div 2);
          Top :=Max(Screen.DeskTopTop ,(Screen.DeskTopTop +Screen.DeskTopHeight-Height) div 2);
          end
  else if Sender=MenuItemWindowSizeMaximized then begin
          if   WindowState=wsNormal then
               WindowState:=wsMaximized
          else WindowState:=wsNormal;
          MenuItemWindowSizeMaximized.Checked:=WindowState=wsMaximized;
          end;
end;

function  TMainForm.GetDefaultLevelFolder:String;
begin // precondition: 'ApplicationDataPath' has been initialized during form creation
  Result:=StrWithTrailingPathDelimiter(ApplicationDataPath)+DEFAULT_LEVEL_DIRECTORY;
end;

procedure TMainForm.ImportHuffmanBase64EncodedBoardFromClipboard;
var BufferByteSize,Len,Position:Integer; Result:Boolean;
    ClipboardText,BoardAsText,TextLine:String;
    Buffer:PByte; Node:TNode; Lines1,Lines2:TList;
begin
  if   Clipboard.HasFormat(CF_TEXT) then begin
       ClipboardText:=Clipboard.AsText;
       try     Lines1:=nil; Lines2:=nil;
               if CreateObject(otList,TNode(Lines1)) and
                  CreateObject(otList,TNode(Lines2)) then
                  try     Result:=Lines1.LoadFromText(ClipboardText);
                          while Result and (not Lines1.IsEmpty) do begin
                            Node:=Lines1.Pop;
                            if   (not IsAMoveTextLine(Node.Text)) // this is not fool-proof; a Huffman-Base64 encoded board could theoretically look like a moves-line, but the chances for that are probably small; doing it this way means that the import works as normal, with the only difference that boards are Huffman-Base64 encoded
                                 and
                                 IsABase64EncodedText(Node.Text,HUFFMAN_BASE64_ENCODING_LAST_2_CHARACTERS)
                                 then begin
                                 if   Base64Decode(Node.Text,Buffer,BufferByteSize,HUFFMAN_BASE64_ENCODING_LAST_2_CHARACTERS) then
                                      try     BoardAsText:=Game.HuffmanDecodeBoard(Buffer,BufferByteSize);
                                              Result:=BoardAsText<>'';
                                              if Result then begin
                                                 Position:=0; Len:=Length(BoardAsText);
                                                 while Result and ReadLine(BoardAsText,Len,Position,TextLine) do begin
                                                   Result:=CreateObject(otNode,Node);
                                                   if Result then begin
                                                      Lines2.Add(Node);
                                                      Node.Text:=TextLine;
                                                      end;
                                                   end;
                                                 end;
                                      finally FreeMem(Buffer);
                                      end
                                 else Result:=False;
                                 end
                            else Lines2.Add(Node);
                            end;

                          if   Result then Result:=Lines2.ToText(NL,Lines2.Text);

                          if   Result then begin
                               ClipBoard.AsText:=Lines2.Text;
                               BtnOpenClipBoardClick(Self);
                               end
                          else Msg(HuffmanBase64ImportFailedText,
                                   Application.Title+SUB_TITLE_SEPARATOR+HuffmanBase64ImportText,
                                   MB_OK+MB_ICONINFORMATION);

                  finally Lines1.Free; Lines2.Free;
                  end
       finally Clipboard.AsText:=ClipboardText;
       end;
       end
  else BtnOpenClipBoardClick(Self);
end;

function  TMainForm.MakeNormalizedLevel(Game__:TGame):Boolean;
var HashValue:THashTableHashKey;
begin // creates a normalized version of the current game board and stores it in 'Game__'
  Result:=Assigned(Game) and Assigned(Game__) and (Game<>Game__) and Assigned(DuplicatesForm) and
          (Game.GameState<>gsNull) and Game.IsIdleAndStopReplayingAndBrowsing;
  if Result then begin
     Game__.Clear;
     Game__.BoardWidth    :=Game.BoardWidth;
     Game__.BoardHeight   :=Game.BoardHeight;
     Game__.StartBoard    :=Game.Board;
     Game__.PlayerStartPos:=Game.PlayerPos;
     Game__.SokoFileName  :=''; // ensure that 'Game__' doesn't try to save itself later; this is not necessary after calling 'Game__.Clear' but it doesn't hurt
     with Game__ do NormalizeBoard(True,True,True,False,False,BoardWidth,BoardHeight,StartBoard,PlayerStartPos,History);
     DuplicatesForm.NormalizeRotationsAndReflectionsAndPlayerPosition(Game__,True,HashValue);
     end;
end;

procedure TMainForm.InternetLevelLookup(NormalizeLevel__:Boolean);
var Position:Integer; Result:Boolean;
    Author,HuffmanBase64EncodedBoard,Text:String;
begin
  if MPlayer.Visible then MPlayer.Hide;
  Result:=(Game.GameState<>gsNull) and (InternetLevelLookupFormatString<>'') and Game.IsIdleAndStopReplayingAndBrowsing;
  if Result then begin
     Text:=InternetLevelLookupFormatString;
     Position:=AnsiPos(FORMAT_KEY_TITLE,Text);
     if Position<>0 then Text:=Copy(Text,1,Pred(Position))+Game.Name+Copy(Text,Position+Length(FORMAT_KEY_TITLE),MaxInt);
     Position:=AnsiPos(FORMAT_KEY_AUTHOR,Text);
     if Position<>0 then begin
        if Game.Notes.Lines.ReadString(KEY_AUTHOR,Author) then
        else if SokoFile.FileHeader.Lines.ReadString(KEY_AUTHOR,Author) then
        else Author:='';
        Text:=Copy(Text,1,Pred(Position))+Author+Copy(Text,Position+Length(FORMAT_KEY_AUTHOR),MaxInt);
        end;
     Position:=AnsiPos(FORMAT_KEY_BOARD,Text);
     if Position<>0 then begin
        if   NormalizeLevel__ then
             if   Assigned(OpenForm) and MakeNormalizedLevel(OpenForm.Game) then
                  HuffmanBase64EncodedBoard:=OpenForm.Game.HuffmanBase64EncodedBoard
             else HuffmanBase64EncodedBoard:=''
        else HuffmanBase64EncodedBoard:=Game.HuffmanBase64EncodedBoard;
        if   HuffmanBase64EncodedBoard<>'' then
             Text:=Copy(Text,1,Pred(Position))+HuffmanBase64EncodedBoard+Copy(Text,Position+Length(FORMAT_KEY_BOARD),MaxInt)
        else Result:=False;
        end;
     Position:=AnsiPos(FORMAT_KEY_MD5,Text);
     if Position<>0 then begin
        Text:=Copy(Text,1,Pred(Position))+FORMAT_KEY_MD5+COLON+SPACE+TEXT_NOT_IMPLEMENTED+Copy(Text,Position+Length(FORMAT_KEY_MD5),MaxInt)
        end;
     if Result then begin
        ShellExecute(Self.Handle, 'open', PChar(Text), nil, nil, SW_SHOWNORMAL);
        end
     else
        Msg(TEXT_TASK_FAILED,
            Application.Title+SUB_TITLE_SEPARATOR+InternetLevelLookupText,
            MB_OK+MB_ICONINFORMATION);
     end;
end;

procedure TMainForm.CopyHuffmanBase64EncodedBoardToClipboard(NormalizeLevel__,Prompt__:Boolean);
var Col,Row,CharacterCount:Integer; HuffmanBase64EncodedBoard:String;
begin
  if MPlayer.Visible then MPlayer.Hide;
  if (Game.GameState<>gsNull) and Game.IsIdleAndStopReplayingAndBrowsing then begin
     if   NormalizeLevel__ then
          if   Assigned(OpenForm) and MakeNormalizedLevel(OpenForm.Game) then
               HuffmanBase64EncodedBoard:=OpenForm.Game.HuffmanBase64EncodedBoard
          else HuffmanBase64EncodedBoard:=''
     else HuffmanBase64EncodedBoard:=Game.HuffmanBase64EncodedBoard;

     CharacterCount:=Game.BoardWidth*Game.BoardHeight; // calculate the number of characters to represent the uncompressed board, discounting right-trimmed squares
     for Row:=1 to Game.BoardHeight do begin
         Col:=Game.BoardWidth;
         while (Col>0) and ((Game.StartBoard[Col,Row] and (GOAL + BOX + PLAYER + WALL))=0) do begin
               Dec(CharacterCount); Dec(Col);
               end;
         end;
     Inc(CharacterCount,Pred(Game.BoardHeight)); // 'rows - 1' line-separators are required for an uncompressed representation of the board; a line-separator is not required after the last row, hence, 'rows - 1' and not 'rows'

     if   HuffmanBase64EncodedBoard<>'' then begin
          if (not Prompt__)
             or
             (Msg(Format(HuffmanBase64EncodedBoardMessageText__,[HuffmanBase64EncodedBoard,CharacterCount,Length(HuffmanBase64EncodedBoard),((Length(HuffmanBase64EncodedBoard)*100)+(CharacterCount div 2)) div CharacterCount]),
                  Application.Title+SUB_TITLE_SEPARATOR+HuffmanBase64EncodingText,
                  MB_YESNOCANCEL+MB_ICONQUESTION+MB_DEFBUTTON2)=ID_YES)
             then begin
             ClipBoard.AsText:=HuffmanBase64EncodedBoard;
             if Assigned(Status) then Status.Hint:=HuffmanBase64EncodedBoardCopiedToClipboardText;
             end;
          end
     else Msg(TEXT_TASK_FAILED,Application.Title+SUB_TITLE_SEPARATOR+HuffmanBase64EncodingText,MB_OK+MB_ICONINFORMATION);
     end;
end;

function  TMainForm.MakeNewLogFileName:String;
begin
  Result:=MakeNewFileName(StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(ApplicationDataPath)+LogText)+LogText,'',True);
end;

function  TMainForm.ProcessLogFiles(LogFileTask__:TLogFileTask):Boolean;
var i,j,BoardWidth,BoardHeight,FailCount,ImportCount:Integer;
    ReverseMode:Boolean;
    s:String;
    oCursor:TCursor;
    PlayerPos:TColRow; Board:TBoard; History:THistory;
    SL:TStringList; L:SokUtil_.TList;
    Level:TLevel; Node:TNode; ExtendedSnapshotAsText:TExtendedSnapshotAsText;

  function ExtractLoggedLevelName(const LevelName__:String):String;
  begin
    if   ExtractSectionName(LevelName__)<>STAR then // 'True': the level name contains file name and level name formatted as "filename\[levelname]"
         Result:=LevelName__
    else Result:=ExtractIniFileName(LevelName__); // the level name looks like this: "filename\[*]"; the true level name is just the file name, i.e., "[*]" is just there to conform to "filename\[levelname]" name format
  end;

begin {ProcessLogFiles; Precondition: 'OpenFile.FileListBox1' is free to use}
  Result:=True;
  s:=StrWithoutTrailingPathDelimiter(ExtractFilePath(MakeNewLogFileName));
  if DirectoryExists(s) then with OpenForm.FileListBox1 do begin
     Directory:=s;
     Mask:=ALL_FILES_FILTER;
     Update;

     try
       oCursor:=Screen.Cursor;
       SL:=nil; L:=nil; Level:=nil; ExtendedSnapshotAsText:=nil;
       FailCount:=0; ImportCount:=0;
       try     Screen.Cursor:=crHourGlass;

               case LogFileTask__ of
                    lftClear       : while not SolutionLogFiles.IsEmpty do begin
                                       if (SolutionLogFiles.First.Text<>'') and
                                          FileExists(SolutionLogFiles.First.Text) then
                                          try    DeleteFile(SolutionLogFiles.First.Text);
                                          except on E:Exception do begin end;
                                          end;
                                       SolutionLogFiles.Pop.Free;
                                       end;

                    lftImport      : for i:=Pred(Items.Count) downto 0 do begin
                                         FileName:=StrWithTrailingPathDelimiter(Directory)+Items[i];
                                         if FileExists(FileName) then begin
                                            if     not Assigned(L) then
                                                   L:=SokUtil_.TList.Create;

                                            try    if L.LoadFromFile(FileName) then begin
                                                      if   Assigned(MainForm.Optimizer.SokoFile) and
                                                           Assigned(MainForm.Optimizer.PluginThread) then begin
                                                           if not Assigned(Level) then begin
                                                              Level:=TLevel.Create; // create level
                                                              MainForm.Optimizer.SokoFile.Levels.Push(Level);

                                                              ExtendedSnapshotAsText:=TExtendedSnapshotAsText.Create; // create snapshot for the level
                                                              Level.SnapshotsAsText.Push(ExtendedSnapshotAsText);

                                                              Node:=TNode.Create; // create text line for the snapshot
                                                              ExtendedSnapshotAsText.MovesAsTextLines.Push(Node);
                                                              end;

                                                           while not L.IsEmpty do begin
                                                              if      IsAnIniFileSectionFileName(L.First.Text) then begin // 'True': the text line contains a level name formatted as "filename\[levelname]"
                                                                      s:=ExtractLoggedLevelName(L.First.Text);
                                                                      if   Level.SetName(s) then
                                                                      else raise Exception.Create(TEXT_MEMORY_FULL);

                                                                      if Assigned(L.First.Next) and
                                                                         StrBeginsWith(L.First.Next.Text,LOG_FILE_KEY_RENAME) then begin // 'True': this is a "rename" operation, not a logged snapshot
                                                                         L.Pop.Free; // skip the old name; it's stored in 's' at the moment
                                                                         if Assigned(L.First.Next) and
                                                                            IsAnIniFileSectionFileName(L.First.Next.Text) then begin // 'True: the line after "=>" contains the new level name
                                                                            L.Pop.Free; // skip the "=>" rename key

                                                                            if   Level.SetName(ExtractLoggedLevelName(L.First.Text)) then // set 'Level.Name' = new level name
                                                                            else raise Exception.Create(TEXT_MEMORY_FULL);

                                                                            // check if some of the already processed snapshots had the old name
                                                                            if (Level.Name<>'') and Assigned(SL) then
                                                                               for j:=Pred(Pred(SL.Count)) downto 0 do // 'Pred(Pred...': each snapshot on the string list is represented by a name line, a moves line, and a preceding blank line except for the first snapshot
                                                                                   if StrEqual(s,SL.Strings[i]) then begin // 'True': this line contains the old level name
                                                                                      ExtendedSnapshotAsText.MovesAsTextLines.First.Text:=SL.Strings[Succ(j)];
                                                                                      if MainForm.Optimizer.ImportGame(Level) then begin
                                                                                         Inc(ImportCount);
                                                                                         Dec(FailCount);
                                                                                         if j+2<SL.Count then SL.Delete(j+2); // delete blank line after the member
                                                                                         SL.Delete(Succ(j));                  // delete the line with the moves
                                                                                         SL.Delete(j);                        // delete the line with the name
                                                                                         end;
                                                                                      end;
                                                                            end;
                                                                         end;
                                                                      end
                                                              else if (L.First.Text<>'') and (Level.Name<>'') then begin // assume that the node contains a solution
                                                                      ExtendedSnapshotAsText.MovesAsTextLines.First.Text:=L.First.Text;
                                                                      if MainForm.Optimizer.ImportGame(Level) then begin
                                                                         Inc(ImportCount);
                                                                         end
                                                                      else begin
                                                                         Inc(FailCount);
                                                                         if not Assigned(SL) then SL:=TStringList.Create;
                                                                         if SL.Count<>0 then SL.Add('');
                                                                         SL.Add(Level.Name);
                                                                         SL.Add(ExtendedSnapshotAsText.MovesAsTextLines.First.Text);
                                                                         end;
                                                                      if not Level.SetName('') then
                                                                         raise Exception.Create(TEXT_MEMORY_FULL);
                                                                      end;
                                                              L.Pop.Free;
                                                              end;
                                                           end
                                                      else raise Exception.Create(TEXT_MEMORY_FULL);
                                                      end;
                                            except on E:Exception do Result:=Error(E.Message,Application.Title);
                                            end;

                                            try    DeleteFile(FileName); // delete the file even if something went wrong; this is not strictly correct but otherwise, the user may not be able to launch the application due to problems with a file that could not be processed for some obscure reason (e.g., a too big file)
                                            except on E:Exception do begin end;
                                            end;
                                            end;
                                         end;
                    lftIsEmpty         : Result:=OpenForm.FileListBox1.Items.Count=0;
               end; // case

               Screen.Cursor:=oCursor;

               if ImportCount+FailCount<>0 then begin
                  s:=LastSessionClosedUnexpectedlyText; i:=MB_OK+MB_ICONINFORMATION;
                  if ImportCount<>0 then
                     s:=s+NL+NL+Format(LastSessionClosedUnexpectedlyText2,[ImportCount]);
                  if FailCount  <>0 then begin
                     s:=s+NL+NL+Format(LastSessionClosedUnexpectedlyText3,[FailCount])+LastSessionClosedUnexpectedlyText4;
                     i:=MB_YESNO+MB_ICONINFORMATION;
                     end;
                  if Msg(s,Application.Title,i)=ID_YES then begin
                     if  not Assigned(Level) then begin // 'True': this should not happen
                         Level:=TLevel.Create;
                         MainForm.Optimizer.SokoFile.Levels.Push(Level);
                         end;

                     // for all orphaned solutions, try to construct boards
                     // based on the moves and add the boards to the text copied
                     // to the clipboard; this makes it easier for the user to
                     // recognize the levels;
                     for i:=Pred(Pred(SL.Count)) downto 0 do
                         if ExtractFilePath(Sl.Strings[i])<>'' then begin // 'True': the line (probably) contains a level name
                            ExtendedSnapshotAsText.MovesAsTextLines.First.Text:=SL.Strings[Succ(i)];
                            if ExtendedSnapshotAsText.TextLinesToMoves(History,ReverseMode) and
                               MakeBoardFromMoves(PHistoryMoves(Addr(History.Moves)),History.Count,ReverseMode,BoardWidth,BoardHeight,Board,PlayerPos) and
                               (BoardWidth*BoardHeight>MIN_BOARD_WIDTH*MIN_BOARD_HEIGHT) and
                               Level.BoardToTextLines(BoardWidth,BoardHeight,Board) then begin
                               Node:=Level.BoardAsTextLines.First; j:=i;
                               if Assigned(Node) then begin
                                  repeat Inc(j); SL.Insert(j,Node.Text);
                                         Node:=Node.Next;
                                  until not Assigned(Node);
                                  Inc(j); SL.Insert(j,BoardReconstructedFromMovesText);
                                  end;
                               end;
                            end;

                     ClipBoard.AsText:=SL.Text;
                     end;
                  end;

       finally Screen.Cursor:=oCursor;
               SL.Free; L.Free;
               if Assigned(Level) then MainForm.Optimizer.SokoFile.Levels.Remove(Level,True);
       end;

     except on E:Exception do Result:=Error(E.Message,Application.Title);
     end;
     end;
end;

function  TMainForm.SaveSnapshotToLogFile(const LevelName1__,LevelName2__,MovesAsText__:String):Boolean; // if 'LevelName2__' isn't blank, then the function logs a "rename" operation instead of a snapshot
var NewFileCountDown:Integer; F:TextFile;

  function MakeIniFileSectionFileName(const LevelName__:String):String;
  begin
    if   IsAnIniFileSectionFileName(LevelName__) then // 'True': the level name is already formatted as "filename\[levelname]"
         Result:=LevelName__
    else Result:=Pack_.MakeIniFileSectionFileName(LevelName__,STAR); // the level name is just a file name; emit "filename\[*]" so it conforms to the "filename\[levelname]" name format
  end;

begin {$I-} // SaveSnaphotToLogFile
  Result:=ErrorRecoveryEnabled and (Length(LevelName1__)>2); // '>2': the level name has the format filename + '[' + levelname +']', hence, '[]' represents an empty name
  if Result then
     try    if LogFileLineCount>=MAX_LOG_FILE_LINE_COUNT then begin
               if not Assigned(SolutionLogFiles.AddTextLine('',True)) then // push a new blank "log file name" on the list; the "repeat" loop below fills it in with a proper file name
                  raise Exception.Create(TEXT_MEMORY_FULL);
               end;

            NewFileCountDown:=50;
            if IOResult<>0 then begin end; // clear IOResult

            repeat
                   if   SolutionLogFiles.First.Text='' then begin // 'True': make a new log file
                        LogFileLineCount:=0;
                        Dec(NewFileCountDown); // bail out if creating a new file fails too many times
                        SolutionLogFiles.First.Text:=MakeNewLogFileName;
                        if   SolutionLogFiles.First.Text<>'' then begin
                             ForceDirectories(StrWithoutTrailingPathDelimiter(ExtractFilePath(SolutionLogFiles.First.Text)));
                             AssignFile(F,SolutionLogFiles.First.Text);
                             Rewrite(F);
                             Result:=IOResult=0;
                             end
                        else Result:=False;
                        end
                   else begin
                        AssignFile(F,SolutionLogFiles.First.Text);
                        Append(F);
                        if IOResult<>0 then Rewrite(F);
                        Result:=IOResult=0;
                        end;

                   if   Result then
                        try     if LevelName2__='' then begin // 'True': log snapshot
                                   Writeln(F,MakeIniFileSectionFileName(LevelName1__));
                                   Writeln(F,MovesAsText__);
                                   Inc(LogFileLineCount,2);
                                   end
                                else begin // log a "rename" operation
                                   Writeln(F,MakeIniFileSectionFileName(LevelName1__));
                                   Writeln(F,LOG_FILE_KEY_RENAME);
                                   Writeln(F,MakeIniFileSectionFileName(LevelName2__));
                                   Inc(LogFileLineCount,3);
                                   end;
                                Writeln(F);
                                Inc(LogFileLineCount);
                        finally CloseFile(F); Result:=IOResult=0;
                        end;

                   if   not Result then
                        if      (SolutionLogFiles.First.Text='') or                 // 'True': no new file name could be created
                                (SolutionLogFiles.First.Text=MakeNewLogFileName) or // 'True': the current log file name is a new one, and it's a vacant file name which isn't occupied by another application or another instance of this application
                                (NewFileCountDown=0) then                           // 'True': the loop has tried several new file names, and it has to stop at some point
                                raise Exception.Create(TEXT_WRITE_FILE_ERROR)       // escape from the 'repeat' loop
                        else if not Assigned(SolutionLogFiles.AddTextLine('',True)) then // multiple instances might be competing for the the same file name, hence, before giving up, try to write the data to a new file with a different name;
                                raise Exception.Create(TEXT_MEMORY_FULL);           // escape from the 'repeat' loop

            until Result; // repeat the loop until the data has been written to a log file; in case of file write errors, the loop terminates by raising exceptions, hence, the simple 'until Result' termination criteria will do here

     except on E:Exception do Result:=Error(E.Message,Application.Title);
     end;
end; {$I+}

procedure TMainForm.ApplicationTimerTimer(Sender: TObject);
var Index:Integer; WindowHandle:HWND;
begin
  if (not ScreenSaverEnabled) and
     (not ShutdownApplication)
     //or
     //(Assigned(Solver   ) and Solver   .IsActive)
     //or
     //(Assigned(Optimizer) and Optimizer.IsActive)
     //or
     //(Assigned(Generator) and Generator.IsActive)
     then begin
     WindowHandle:=GetForegroundWindow();
     if   WindowHandle<>0 then begin
          for Index:=0 to Pred(Screen.FormCount) do
              if WindowHandle=Screen.Forms[Index].Handle then begin {'True': one of the forms belonging to this application has focus; note that this is not an exhaustive test; plugins (solver and optimizer plugins) may have their own forms which cannot be detected here}
                 //Screen.ActiveForm.Caption:=TEXT_APPLICATION_TITLE+' - Simulated keyboard event '+IntToStr(GetTimeMS);
                 SimulateKeyboardEvent;
                 break;
                 end;
          end
     else SimulateKeyboardEvent; // if 'GetForegroundWindow()' returns NULL then fake a keyboard event
     end;
end;

function  TMainForm.CheckAutoAdvanceWhenSolved:Boolean;

  function CheckAutoAdvance:Boolean;
  begin
    Result:=AutoAdvanceWhenSolved and
            (not ShutdownApplication) and
            Assigned(Game) and (Game.GameState=gsSolved) and
            (not (Game.IsBusy or Game.IsReplaying or Game.IsBrowsing or Game.IsLoading));
  end;

begin // CheckAutoAdvanceWhenSolved
  Result:=CheckAutoAdvance;
  if Result then begin
     Game.IsBusy:=True;
     try     Application.ProcessMessages;
     finally Game.IsBusy:=False;
     end;
     SleepEx(1000,False); // wait awhile, e.g., to give a glimpse of a solution animation, or to let a solution sound finish
     Result:=CheckAutoAdvance;
     end;
  if Result then
     Result:=OpenPriorOrNext(False,True,False,0);
end;

function  TMainForm.CalculatePushesLowerBound( Sender : TObject ) : Integer;
var Done : Boolean;
    Text : String;
    State : TDeadlocksThreadStateType;
begin
  Result := -1;
  if (Game <> nil) and
     ( not Game.ReverseMode ) and
     ( Game.GameState <> gsNull ) and
     Game.IsIdleAndStopReplayingAndBrowsing and
     Game.DeadlockDetection.Enabled and
     Assigned(Game.DeadlockDetection.Deadlocks) then begin
     State  := Game.DeadlockDetection.Deadlocks.ThreadState;
     Result := Game.DeadlockDetection.Deadlocks.PushesLowerBound;
     Status.Hint := '';
     if      Result =  PUSHES_LOWER_BOUND_ERROR_TIMEOUT then
             Text   := PluginResultText[ prTimeOut] + PERIOD + SPACE + SeeControlPathfindingSettingsText
     else if Result =  PUSHES_LOWER_BOUND_ERROR_NUMERIC_OVERFLOW then
             Text   := CalculationFailedBecauseOfNumericOverflowText
     else if Result = PUSHES_LOWER_BOUND_ERROR_TASK_FAILED_BUSY then
             if State = dlIdle  then Text := TEXT_TASK_FAILED
             else                    Text := DeadlockDetectionCalculationInProgressTryAgainText
     else if Result <  0        then Text := TEXT_TASK_FAILED
     else if Result >= Dead_.INFINITY
                                then Text := EitherNoSolutionOrNumericOverflowText
     else                            Text := Format( LowerBoundText__, [ Result] );
     if Result <> PUSHES_LOWER_BOUND_ERROR_TASK_ABANDONED then begin // 'True': the result is still valid, e.g., the user hasn't opened a different level
        //if ( Text <> '' ) and ( Text[ Length( Text ) ] <> PERIOD ) then
        //   Text := Text + PERIOD;
        //Msg( Text,
        //     // Self.Caption,
        //     Application.Title+SUB_TITLE_SEPARATOR+CalculatingPushesLowerBoundText,
        //     MB_OK );
        if Game.History.PushCount = 0 then begin
           IdleStartTime:=0; // trigger the 'OnIdle' actions now
           OnIdleHandler( Self, Done ); // avoids that performing 'OnIdle' actions immediately overwrites the "pushes lower bound: 9999" message on the status bar
           end;
        StartHintCount := 0; // don't show pending usage tips, if any
        Status.Hint := Text;
        if Sender  = Clipboard then
           Clipboard.AsText := IntToStr( Result );
        end;
     end;
end;

procedure TMainForm.CreateScreenshots;
begin // replays the loaded level from the current position and creates a screenshot for each move
  fMakeScreenshots:=Game.IsIdleAndStopReplayingAndBrowsing and (Game.GameState<>gsNull);
  if MakeScreenshots then
     try     BtnRedoAllClick(BtnStatusMenuReplay);
     finally fMakeScreenshots:=False;
     end;
end;

function  TMainForm.CreateScreenshot:String;
const MINIMUM_SCREENSHOT_MOVES_TOP=ONE_MILLION-1;
var X,Y,L:Integer; R:TRect; FolderName:String; Snapshot:TBitMap;
begin // creates and saves a screenshot of the current level position
  if MakeScreenshots or // 'MakeScreenshots': if 'True', then replay game is in progress, creating a screenshot for each move
     (Game.IsIdleAndStopReplayingAndBrowsing and (Game.GameState<>gsNull)) then begin
     FolderName:=StrWithTrailingPathDelimiter(ApplicationDataPath)+ScreenshotsText;
     ForceDirectories(FolderName);

     L:=Length(IntToStr(Max(Max(Game.History.Top,MAX_MOVES),MINIMUM_SCREENSHOT_MOVES_TOP)));
     Result:=IntToStr(Game.History.Count);
     while Length(Result)<L do Result:='0'+Result;
     Result:=StrWithTrailingPathDelimiter(FolderName)+Result+BMP_FILE_EXT;

     //R:=Rect(0,0,MainForm.Image1.Picture.BitMap.Width,MainForm.Image1.Picture.BitMap.Height);
     X:=Succ(GameViewer.ColWidth ) div 2;
     Y:=Succ(GameViewer.RowHeight) div 2;
     with GameViewer.BoardRect do R:=Rect(Left-X,Top-Y,Right+X,Bottom+Y);
     if ClipRect(R,GameViewer.WindowRect) then begin
        // adjust the image rectangle so all sides have identical margins
        with GameViewer.BoardRect do L:=Min( Min(Left-R.Left,R.Right -Right ),
                                             Min(Top -R.Top ,R.Bottom-Bottom));
        with GameViewer.BoardRect do R:=Rect(Left-L,Top-L,Right+L,Bottom+L);
        if ClipRect(R,GameViewer.WindowRect) and // clip the rectangle again, just to be sure that all the calculations were all right
           BitMapCreate(Snapshot, RectWidth(R), RectHeight(R)) then
           try     Snapshot.Canvas.CopyRect(Rect(0,0,RectWidth(R),RectHeight(R)),MainForm.Image1.Picture.BitMap.Canvas,R);
                   Snapshot.SaveToFile(Result);
           finally Snapshot.Free;
           end;
        end
     end
  else Result:='';
end;

{
procedure TMainForm.BtnAdHocClick(Sender: TObject);
var i:Integer; s,s1:String;
begin
  with Skins do begin
    if (ScriptFileName<>'') and (SkinFileName<>'') then
       with OpenForm.Texts[tSkinScript].Memo do begin
         s:=StrWithBrackets(ExtractFileNameWithoutPathAndExtension(SkinFileName));
         Lines.LoadFromFile(ScriptFileName);
         Lines.Add('');
         s1:='';
         for i:=0 to Length(s) do s1:=s1+'=';
         Lines.Add(s1);
         Lines.Add(s);
         Lines.Add(s1);
         Lines.Add('');
         Lines.Add('[Graphics - Board - Figures - Wall - Wall cap offset, pixels]');
         Lines.Add('Left='+IntToStr(GamePictures.WallCap.X));
         Lines.Add('Top=' +IntToStr(GamePictures.WallCap.Y));
         Lines.Add('');
         Lines.Add('[Graphics - Board - Figures - Wall - Outer wall razor, pixels]');
         Lines.Add('Left='+IntToStr(GamePictures.OuterWallRazor.Left));
         Lines.Add('Top='+IntToStr(GamePictures.OuterWallRazor.Top));
         Lines.Add('Right='+IntToStr(GamePictures.OuterWallRazor.Right));
         Lines.Add('Bottom='+IntToStr(GamePictures.OuterWallRazor.Bottom));
         Lines.Add('');
         Lines.Add('end');
         Lines.SaveToFile(ScriptFileName);
         end;
    end;
end;
}

function TMainForm.Msg(const Text__,Caption__:String; Flags__:Integer):Integer;
begin
  Result:=SokUtil_.Msg(Text__,Caption__,Flags__);
  CtrlEnterKeySet:=[]; LastKey:=0; // necessary because the messagebox may recieve pending 'key up' events
end;

function ContrastEnhancement(BitMap__:TBitMap):Boolean;
type
  TRGB=packed record b,g,r:Byte; end;
  PRGB=^TRGB;
  TRGBVector=array[0..MaxInt div SizeOf(TRGB)-1] of TRGB;
  PRGBVector=^TRGBVector;
var i,Col,Row,N:Integer; MaxF:Double; F:array[0..255] of Double; p:PRGB;
begin

  if (BitMap__<>nil) and
     (BitMap__.PixelFormat=pf24Bit) and
     (BitMap__.Width<>0) and
     (BitMap__.Height<>0) then
     with BitMap__ do with Canvas do begin
       Result:=True;
       BitMap__.SaveToFile('c:\t1.bmp');

       FillChar(F,SizeOf(F),0); MaxF:=0;

       for Row:=0 to Pred(Height) do begin
           p:=ScanLine[Row];
           for Col:=0 to Pred(Width) do begin
               i:=p^.b;
               F[i]:=F[i]+1.0;
               if F[i]>MaxF then MaxF:=F[i];
               Inc(p);
               end;
           end;

       N:=Width*Height;
       for i:=Low(F) to High(F) do F[i]:=F[i] / N;
       for i:=Succ(Low(F)) to High(F) do F[i]:=F[Pred(i)] + F[i];

       for Row:=0 to Pred(Height) do begin
           p:=ScanLine[Row];
           for Col:=0 to Pred(Width) do begin
               i:=p^.r; p^.r:=Trunc(F[i]*255);
               i:=p^.g; p^.g:=Trunc(F[i]*255);
               i:=p^.b; p^.b:=Trunc(F[i]*255);
               Inc(p);
               end;
           end;

       end
     else Result:=False;
end;
{
procedure TMainForm.StopFlicker(var Msg: TWMEraseBkgnd);
begin // not necessary when the program is compiled with Delphi 4, but some versions (like Delphi 6) need it
  Msg.Result := 1;
end;
}

// Ad hoc tasks
{
procedure TMainForm.AdHocTask;
var SokoFile1,SokoFile2:TSokoFile;
begin
  if CreateObject(otSokoFile,TNode(SokoFile1)) then
     try     if CreateObject(otSokoFile,TNode(SokoFile2)) then
                try     if SokoFile1.LoadFromFile('c:\Temp\t1.sok')
                           and
                           SokoFile2.LoadFromFile('c:\Temp\t2.sok') then begin
                           if SokoFile1.MergeSokoFile(SokoFile2,False) then
                              Msg('Done.','Merging Files',MB_OK);
                           end;
                finally SokoFile2.Free;
                end;
     finally SokoFile1.Free;
     end;
end;
}
{
procedure TMainForm.AdHocTask;
var B1,B2:TBitMap;
begin
  if BitMapCreate(B1,1,1) then
     try
       if BitMapCreate(B2,1,1) then
          try
            B1.LoadFromFile('c:\temp\t7.bmp');
            B2.LoadFromFile('c:\temp\t9.bmp');
            if BitMapCompare(B1,B2,True,clAqua)<>0 then begin
               B1.SaveToFile('c:\temp\tx.bmp');
               Msg('Fail',Application.Title,MB_OK);
               end;
          finally B2.Free;
          end;
     finally B1.Free;
     end;
end;
}
{
procedure TMainForm.AdHocTask;
var B1:TBitMap;
begin
  if BitMapCreate(B1,1,1) then
     try
       B1.LoadFromFile('c:\temp\t1.bmp');
       if not ConvertBitMapToByteCode(B1,'c:\temp\t2.pas') then
          Msg('Fail',Application.Title,MB_OK);
     finally B1.Free;
     end;
end;
}
{
procedure TMainForm.AdHocTask;
var a,i,j,k:Integer; p:PRGBVector; rgb:BitMap_.TRGB; Pict:TPict;
begin
  Pict:=TPict.Create;
  try
                 if Pict.LoadFromFile('c:\temp\t1.bmp') and
                    (Pict.OrgBitMap<>nil) or (Pict.MakeOrgBitMapFromPict) then begin
                    k:=0;
                    for i:=0 to Pred(Pict.OrgBitMap.Height) do
                        begin p:=Pict.OrgBitMap    .ScanLine[i];
                              for j:=0 to Pred(Pict.OrgBitMap.Width) do
                                  begin rgb:=p[j];
                                        if   (rgb.r<32) and (rgb.g<32) and (rgb.b<32) then
                                             //((Abs(rgb.r-MaskColor.r)+
                                             //  Abs(rgb.g-MaskColor.g)+
                                             //  Abs(rgb.b-MaskColor.b))*100) div (3*255) <= MaskPct then
                                             begin
                                                   //a:=-DELTA+Random(2*DELTA);
                                                   a:=32-Min(rgb.r,Min(rgb.g,rgb.b));
                                                   Inc(rgb.r,a); Inc(rgb.g,a); Inc(rgb.b,a);
                                                   //if Random(2)=0 then
                                                      //rgb.r:=rgb.r-DELTA+Random(2*DELTA);
                                                   //if Random(2)=0 then
                                                      //rgb.g:=rgb.g-DELTA+Random(2*DELTA);
                                                   //if Random(2)=0 then
                                                      //rgb.b:=rgb.b-DELTA+Random(2*DELTA);
                                                   p[j]:=rgb;
                                                   //p[j].r:=255; p[j].g:=0; p[j].b:=255;
                                                   Inc(k);
                                             end;
                                  end;
                        end;
                    if  k<>0 then begin
                        Pict.FileName:='c:\temp\t2.bmp';
                        try Pict.OrgBitMap.SaveToFile(Pict.FileName);
                        except on E:Exception do Error(E.Message,'');
                        end;
                        end;
                    end;
  finally Pict.Free;
  end;
end;
}
{
procedure TMainForm.AdHocTask;
begin
 ExecuteAndWait('C:\Windows\Notepad.exe',Windows.SW_NORMAL,Windows.INFINITE);
end;
}
{
procedure TMainForm.AdHocTask;
var i,Angle,W,H:Integer; B1,B2:TBitMap; BackGroundColor:TColor;
begin
  if BitMapCreate(B1,1,1) then
     try     B1.LoadFromFile('c:\Temp\t0.bmp');
             if BitMapCreate(B2,B1.Width,B1.Height) then
                try
                  BackgroundColor:=B1.Canvas.Pixels[0,0];
                  for i:=0 to 23 do begin
                      Angle:=i*15;
                      BitMapRotatedImageSize(B1.Width,B1.Height,Angle,W,H);
                      if BitMapResize(B2,W,H) and
                         BitMapRotate(B2,B1,Angle,BackgroundColor,False) then
                         B2.SaveToFile(Format('c:\Temp\t%3.3d.bmp',[Angle]));
                      end;
                finally B2.Free;
                end;
     finally B1.Free;
     end;
end;
}
{
procedure TMainForm.AdHocTask;
const PATH='c:\temp\sokoskinner';
var   i,j,n,ColCount,RowCount:Integer; s:String; SL:TStringList; SkinBitMap:TBitMap;
begin //exit;
  with OpenForm do begin
    SL:=TStringList.Create;
    try
      OptionsForm.LoadData;
      DirectoryListBox1.Directory:=PATH;
      DirectoryListBox1.Update;
      for i:=0 to Pred(DirectoryListBox1.Items.Count) do
          SL.Add( DirectoryListBox1.Items[i] );
      //SL.SaveToFile( 'c:\temp\t1.txt' );
      FileListBox1.Mask:='*.INI';
      for i:=3 to Pred( SL.Count ) do begin
          FileListBox1.Directory:=StrWithTrailingPathDelimiter(PATH)+SL.Strings[i];
          FileListBox1.Update;
          for j:=0 to Pred(FileListBox1.Items.Count) do begin
              s:=StrWithTrailingPathDelimiter(FileListBox1.Directory)+FileListBox1.Items[j];
              if MainForm.Skins.LoadFromFile(s, MainForm.Skins.Scripts[4,1]) then begin
                 OptionsForm.SaveData(True);
                 OptionsForm.LoadData; // 'LoadData' is necessary because 'SaveData' may change a few things
                 if OptionsForm.MakeSkin( SkinBitMap, ColCount, RowCount ) then
                    try     try    s := MakeNewFileName( 'c:\Temp\'+
                                                         StrSubstitute( StrSubstitute(ExtractFileNameWithoutPathAndExtension(s),
                                                                                      '(resized to fit)','',n),
                                                                        '(resize to fit)','',n)+
                                                         BMP_FILE_EXT,
                                                         BMP_FILE_EXT,
                                                         True);
                                   DrawSkinTitle( SkinBitMap, ColCount, RowCount, ExtractFileNameWithoutPathAndExtension( s ) );
                                   SkinBitMap.SaveToFile( s );
                            except on E:Exception do Msg(E.Message,'',MB_OK);
                            end;
                    finally SkinBitMap.Free;
                    end
                 else Msg(s,TEXT_TASK_FAILED,MB_OK);
                 end
              else Msg(s,TEXT_TASK_FAILED,MB_OK);
              end;
          end;
    finally
      SL.Free;
    end;
    end;
end;
}
{
procedure TMainForm.AdHocTask;
const PATH='C:\Temp';
var   i,j,W,H:Integer; R:TRect; s,t,ErrorText:String; SL,SL2:TStringList; BitMap:TBitMap;
begin //exit;
  with OpenForm do begin
    SL2:=TStringList.Create;
    try
      SL:=TStringList.Create;
      try
        SL2.Sorted:=False;
        SL.Sorted:=False;
        //SL2.LoadFromFile( 'C:\Download\t1.txt' );
        DirectoryListBox1.Directory:=PATH;
        DirectoryListBox1.Update;
        FileListBox1.Mask:='*.BMP';
        FileListBox1.Directory:=PATH;
        FileListBox1.Update;
        for i:=0 to Pred(FileListBox1.Items.Count) do
            if BitMapCreate( BitMap, 1, 1 ) then
               try
                 s:=StrWithTrailingPathDelimiter(FileListBox1.Directory)+FileListBox1.Items[i];
                 //Msg( s, '', MB_OK );
                 BitMap.LoadFromFile( s );
                 W:=BitMap.Width div 4;
                 H:=W;
                 R:=GridCellToRect( 3, 3, W, H, 0 );
                 SL.Clear;
                 if   GetBitMapText(BitMap,R,False, SL, ErrorText) then begin
                      SL.SaveToFile(ChangeFileExt(s,TEXT_FILE_EXT));
                      t := StrSubstitute( SL.Text, '', '', j);
                      if j>0 then with BitMap.Canvas do begin
                         Brush.Color:=Pixels[ R.Left, R.Top ];
                         Brush.Style:=bsSolid;
                         FillRect(R);
                         if   SetBitMapText( BitMap, R, t, ErrorText ) then
                              BitMap.SaveToFile( s )
                         else Error( ErrorText, '' );
                         end;
                      end
                 else Error( ErrorText, '' );
               finally BitMap.Free;
               end;
      finally SL.Free;
      end;
    finally SL2.Free;
    end;
    end;
end;
}
{
procedure TMainForm.AdHocTask;
var B1:TBitMap;
begin
  if BitMapCreate(B1,1,1) then
     try
       B1.LoadFromFile('c:\temp\t1.bmp');
       BitMapGrayImage(B1);
       B1.SaveToFile('c:\temp\t2.bmp');
     finally B1.Free;
     end;
end;
}
{
procedure TMainForm.AdHocTask;
var B1,B2:TBitMap; R :TRect; Cancel : Boolean;
begin
  if BitMapCreate(B1,1,1) then
     try     B1.LoadFromFile('c:\Temp\t1.bmp');
             if BitMapCreate(B2,( 3 * B1.Width ) div 4, ( 3 * B1.Height )  div 4) then
                try     R := Rect( 0, 0, B2.Width, B2.Height );
                        BitMapScale( B2, B1, 3, False, False, clBlack, 0, ivStretch, 0, 0, R, Cancel );
                        B2.SaveToFile('c:\Temp\t2.bmp');
                finally B2.Free;
                end;
     finally B1.Free;
     end;
end;
}
{
procedure TMainForm.AdHocTask;
const PATH='c:\Temp';
var a,b,i,j,W,H,X,Y,Count:Integer; R:TRect; Floor,Player,Skin:TPict;
begin exit;
  Skin:=TPict.Create;
  Floor:=TPict.Create;
  Player:=TPict.Create;
  Skin.LoadFromFile(PATH+'\t1.bmp');
  W:=Skin.OrgBitMap.Width div 4;
  H:=W;
  R:=Rect(0,0,W,H);
  Floor .LoadFromBitMapRect(Skin.OrgBitMap,R, Point(0,0),clGray);
  Player.LoadFromBitMapRect(Skin.OrgBitMap,CellToRect(3,4,W,H), Point(0,0),clGray);
  BitMapDump(Floor.OrgBitMap);
  BitMapDump(Player.OrgBitMap);

  BitMapResize(Player.MaskBitMap,W,H);
  BitMapMaskBackgroundForAllPixels(Player.OrgBitMap,Player.MaskBitMap,Floor.OrgBitMap,5,False);
  BitMapDump(Player.OrgBitMap);
  BitMapDump(Player.MaskBitMap);

  for X:=0 to Pred(W) do
      for Y:=0 to Pred(H) do with Player.OrgBitMap.Canvas do
          if   Pixels[X,Y]= clBlack then begin
               Pixels[X,Y]:=RGBToColor(__RGB(255,0,255));
               end;
  BitMapDump(Player.OrgBitMap);

  BitMapResize(Player.BitMap,W*H,2);
  Count:=0;
  X:=W div 2;
  Y:=H div 2;
  Player.BitMap.Canvas.Pixels[Count,0]:=X;
  Player.BitMap.Canvas.Pixels[Count,1]:=Y;
  Player.MaskBitMap.Canvas.Pixels[X,Y]:=clGreen;
  Inc(Count);
  while Count>0 do begin
    Dec(Count);
    X:=Player.BitMap.Canvas.Pixels[Count,0];
    Y:=Player.BitMap.Canvas.Pixels[Count,1];
    for i:=-1 to 1 do
        for j:=-1 to 1 do
            if (Abs(i)+Abs(j))=1 then begin
               a:=X+i;
               b:=Y+j;
               if (a>=0) and (a<W) and (b>=0) and (b<H) and
                  (Player.MaskBitMap.Canvas.Pixels[a,b]=clBlack) then begin
                  Player.BitMap.Canvas.Pixels[Count,0]:=a;
                  Player.BitMap.Canvas.Pixels[Count,1]:=b;
                  Player.MaskBitMap.Canvas.Pixels[a,b]:=clGreen;
                  Inc(Count);
                  end;
               end;
    end;
  BitMapDump(Player.MaskBitMap);

  Player.LoadFromBitMapRect(Skin.OrgBitMap,CellToRect(3,4,W,H), Point(0,0),clGray);
  BitMapDump(Player.OrgBitMap);
  BitMapResize(Player.MaskBitMap,W,H);
  Player.MaskBitMap.LoadFromFile('c:\temp\t0005.bmp');
  for X:=0 to Pred(W) do
      for Y:=0 to Pred(H) do with Player.MaskBitMap.Canvas do
          if   Pixels[X,Y]<>clGreen then
               Pixels[X,Y]:=clBlack
          else Pixels[X,Y]:=clWhite;

  BitMapDump(Player.MaskBitMap);
  BitMapApplyMask(Player.OrgBitMap,Player.MaskBitMap);
  BitMapDump(Player.OrgBitMap);

  Floor.Resize(W,H); // keep a copy of floor

  Floor.OrgBitMap.Canvas.CopyMode:=cmSrcAnd;
  Floor.OrgBitMap.Canvas.Draw(0,0,Player.MaskBitMap);
  Floor.OrgBitMap.Canvas.CopyMode:=cmSrcPaint;
  Floor.OrgBitMap.Canvas.Draw(0,0,Player.OrgBitMap);
  BitMapDump(Floor.OrgBitMap); // really player-on-floor

  Skin.OrgBitMap.Canvas.CopyRect(CellToRect(3,4,W,H), Floor.OrgBitMap.Canvas,R);
  BitMapDump(Skin.OrgBitMap);

  Floor.Free;
  Player.Free;
  Skin.Free;
  Halt;
end;
}
{
procedure TMainForm.AdHocTask;
const PATH='C:\Temp';
var   i:Integer; s,t:String; SL,SL2:TStringList; BitMap:TBitMap;

  function  ChangeTextColor( BitMap__ : TBitMap ) : Integer;
  var W, H, x, y : Integer; Color1, Color2 : TColor; TitleRect : TRect;
  begin
    Result := 0;
    W := BitMap__.Width div 4;
    H := BitMap__.Height div 8;
    TitleRect := CellToRect( 2, 3, W, H );
    Color1 := RGBToColor( __RGB( 240, 240, 240 ) );
    Color2 := RGBToColor( __RGB( 224, 224, 224 ) );
    for x := TitleRect.Left to Pred( TitleRect.Right ) do with BitMap__ do with Canvas do
        for y := TitleRect.Top to Pred( TitleRect.Bottom ) do
            if Pixels[ x, y ] =  Color1 then begin
               Pixels[ x, y ] := Color2;
               Inc( Result );
               end;
  end;

  function  MakeWaterMark( BitMap__ : TBitMap ) : Boolean;
  const Texts : Array[ 0 .. 2 ] of String = ( 'Soko', 'skin', 'ner' );
  //const Texts : Array[ 0 .. 0 ] of String = ( 'Sokoskinner' );
  var a, b, c, i, j, W, H, x, y, LeftMargin, LineSpacing: Integer; Cancel : Boolean; TextSize : TSize; R, TitleRect : TRect; B1, B2 : TBitMap;
  begin
    W := BitMap__.Width div 4;
    H := BitMap__.Height div 8;
    TitleRect := CellToRect( 2, 3, W, H );
    Result := BitMapCreate( B1, 2 * W, 2 * H );
    if Result then
       try     with B1.Canvas do begin
                 R                                    := Rect( 0, 0, B1.Width, B1.Height );
                 LeftMargin                           := 2;
                 Font.Height                          := 40;
                 repeat
                   LineSpacing                        := -4; //2;
                   repeat
                     Result                           := True;
                     Brush.Color                      := BitMap__.Canvas.Pixels[ TitleRect.Left, TitleRect.Top ];
                     Brush.Style                      := bsSolid;
                     FillRect( R );
                     Font.Name                        :='MS Sans Serif';
                     Font.Style                       := [];
                     Windows.SetBkMode ( Handle, Windows.TRANSPARENT );
                     y := 0;
                     for j := Low( Texts ) to High( Texts ) do begin
                         TextSize                     := TextExtent( Texts[ j ] );
                         //LeftMargin := ( B1.Width - TextSize.cx ) div 2;
                         if y = 0 then
                            y := ( B1.Height
                                   - ( ( TextSize.cy ) * ( 1 + High( Texts ) - Low( Texts ) ) ) // text lines
                                   - ( ( LineSpacing ) * (     High( Texts ) - Low( Texts ) ) ) // 1 gap less than the number of text lines
                                 ) div 2;
                         if ( LeftMargin                   < 1 ) or
                            ( ( LeftMargin + TextSize.cx ) > B1.Width  ) or
                            ( ( y          + TextSize.cy ) > B1.Height ) then
                            Result                    := False;
                         if Result then begin
                            Font.Color                := //RGBToColor( __RGB( 176, 176, 176 ) );
                                                         RGBToColor( __RGB( 160, 160, 160 ) );
                                                         //RGBToColor( __RGB( 144, 144, 144 ) );
                                                         //RGBToColor( __RGB( 96, 96, 96 ) );
                            TextOut( LeftMargin, y, Texts[ j ] );
                            Inc( y, TextSize.cy + LineSpacing );
                            end;
                         end;
                     Windows.SetBkMode ( Handle, Windows.OPAQUE );
                     Dec( LineSpacing );
                   until Result or ( LineSpacing      <  -1 );
                   Font.Height                        := Font.Height - 1;
                 until Result or ( Font.Height        <  6 );

                 if Result and BitMapCreate( B2, W, H ) then
                    try     Result := BitMapScale( B2, B1, 3, False, False, clBlack, 0, ivStretch, 0, 0, R, Cancel ) and ( not Cancel );
                            if Result then with BitMap__ do with Canvas do begin
                               for x := TitleRect.Left to Pred( TitleRect.Right ) do
                                   for y := TitleRect.Top to Pred( TitleRect.Bottom ) do begin
                                       a := x - TitleRect.Left;
                                       b := y - TitleRect.Top;
                                       for i := 0 to 0 do begin // number of counter-clockwise rotations
                                           c := a; a := Pred( H - b ); b := c;
                                           end;
                                       if Pixels[ x, y ] =  Pixels[ TitleRect.Left, TitleRect.Top ] then
                                          Pixels[ x, y ] := B2.Canvas.Pixels[ // x - TitleRect.Left, y - TitleRect.Top ];
                                                                              a, b ];
                                          end;
                               //CopyMode := cmSrcCopy;
                               //CopyRect( TitleRect, B2.Canvas, Rect( 0, 0, W, H ) );
                               //BitMapDump( BitMap__ );
                               end;
                            //BitMapDump( B2 );
                    finally B2.Free;
                    end
                 else Result := False;

                 //BitMapDump( B1 );
                 end;
       finally B1.Free;
       end
  end;
begin //exit;
  with OpenForm do begin
    SL2:=TStringList.Create;
    try
      SL:=TStringList.Create;
      try
        SL2.Sorted:=False;
        SL.Sorted:=False;
        //SL2.LoadFromFile( 'C:\Download\t1.txt' );
        DirectoryListBox1.Directory:=PATH;
        DirectoryListBox1.Update;
        FileListBox1.Mask:='*.BMP';
        FileListBox1.Directory:=PATH;
        FileListBox1.Update;
        for i:=0 to Pred(FileListBox1.Items.Count) do
            if BitMapCreate( BitMap, 1, 1 ) then
               try
                 s:=StrWithTrailingPathDelimiter(FileListBox1.Directory)+FileListBox1.Items[i];
                 //Msg( s, '', MB_OK );
                 t:=ExtractFileNameWithoutPathAndExtension( s );
                 BitMap.LoadFromFile( s );
                 if   DrawSkinTitle( BitMap, 4, 8, t ) and
                      MakeWatermark( BitMap ) then
                      BitMap.SaveToFile( s )
                 else Error( t, '' );
               finally BitMap.Free;
               end;
      finally SL.Free;
      end;
    finally SL2.Free;
    end;
    end;
end;
}
{
procedure TMainForm.AdHocTask;
const PATH='C:\Program Files\Compilers\Delphi4\Pg\Sokoban\Skins\Common Skins\SokoSkinner';
      PATH2='C:\Program Files\Games\Sokoban\Sokoban++\Skins\SokoSkinner Skins';
var   i:Integer; s,t:String; SL,SL2:TStringList;
begin //exit;
  with OpenForm do begin
    SL2:=TStringList.Create;
    try
      SL:=TStringList.Create;
      try
        SL2.Sorted:=False;
        SL.Sorted:=False;
        //SL2.LoadFromFile( 'C:\Download\t1.txt' );
        DirectoryListBox1.Directory:=PATH;
        DirectoryListBox1.Update;
        FileListBox1.Mask:='*.PNG';
        FileListBox1.Directory:=PATH;
        FileListBox1.Update;
        for i:=0 to Pred(FileListBox1.Items.Count) do begin
                 s:=StrWithTrailingPathDelimiter(FileListBox1.Directory)+FileListBox1.Items[i];
                 //Msg( s, '', MB_OK );
                 t:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(PATH2)+Trim(ChangeFileExt(FileListBox1.Items[i],'')))+'Wood2'+BMP_FILE_EXT;
                 if   FileExists(t) then
                      CopyFile(PChar(t),PChar('C:\Temp\'+ChangeFileExt(FileListBox1.Items[i],'')+BMP_FILE_EXT),False)
                 else Error( Format( TEXT_FILE_NOT_FOUND_FORMAT, [t] ), '' );
               end;
      finally SL.Free;
      end;
    finally SL2.Free;
    end;
    end;
end;
}
{
procedure TMainForm.AdHocTask;
var W,H:Integer;
    B1,B2,B3,B4:TBitMap;
begin
  if BitMapCreate(B1,1,1) then
     try
       if BitMapCreate(B2,1,1) then
          try
            if BitMapCreate(B3,1,1) then
               try
                 B1.LoadFromFile('c:\temp\t1.bmp');
                 B2.LoadFromFile('c:\temp5\t2.bmp');
                 W:=B1.Width div 4; H:=B1.Height div 8;
                 if BitMapResize(B3,W,H) then begin
                    B3.Canvas.CopyRect(Rect(0,0,W,H),B1.Canvas,CellToRect(0,1,W,H)); // goal

                    BitBltTransparent(B3, 0, 0, B2, CellToRect( 3, 0, W, H), RGBToColor( __RGB( 255, 0, 255 ) ), 0, clBlack, clBlack, clBlack, clBlack ); // box on magenta background
                    B3.SaveToFile('c:\temp\t3.bmp');
                    B1.Canvas.CopyRect(CellToRect(2,1,W,H),B3.Canvas,Rect(0,0,W,H)); // box on goal
                    B1.SaveToFile('c:\temp\t4.bmp');

                    B3.Canvas.CopyRect(Rect(0,0,W,H),B1.Canvas,CellToRect(0,1,W,H)); // goal
                    BitBltTransparent(B3, 0, 0, B2, CellToRect( 7, 0, W, H), RGBToColor( __RGB( 255, 0, 255 ) ), 0, clBlack, clBlack, clBlack, clBlack ); // player-down on magenta background
                    B3.SaveToFile('c:\temp\t5.bmp');

                    if BitMapCreate(B4,1,1) then
                       try
                         B4.LoadFromFile('c:\temp\t6.bmp');
                         BitBltTransparent(B3, 0, 0, B4, Rect( 0, 0, W, H), RGBToColor( __RGB( 255, 0, 255 ) ), 0, clBlack, clBlack, clBlack, clBlack ); // player-on-goal-down-delta on magenta background
                         B3.SaveToFile('c:\temp\t7.bmp');
                       finally B4.Free;
                       end;

                    B1.Canvas.CopyRect(CellToRect(1,1,W,H),B3.Canvas,Rect(0,0,W,H)); // player-down on goal
                    B1.Canvas.CopyRect(CellToRect(2,5,W,H),B3.Canvas,Rect(0,0,W,H)); // player-down on goal
                    B1.SaveToFile('c:\temp\t8.bmp');


                    end
                 else Error(TEXT_TASK_FAILED,'');
               finally B3.Free;
               end;
          finally B2.Free;
          end;
     finally B1.Free;
     end;
end;
}
{
procedure TMainForm.AdHocTask;
//const PATH='C:\Program Files\Compilers\Delphi4\Pg\Sokoban\Skins\Common Skins\SokoSkinner';
const PATH='C:\Temp';
var   i, W, H :Integer; s:String; Skin :TPict;

  function MaskImage( BitMap__ : TBitMap; const Object__, Background__ : TRect ) : Integer;
  var x, y : Integer; MaskColor : TColor;
  begin
    Result := 0;
    MaskColor := RGBToColor( __RGB( 255, 0, 255 ) ); // magenta
    for x := 0 to Pred( RectWidth( Object__ ) ) do
        for y := 0 to Pred( RectHeight( Object__ ) ) do with BitMap__.Canvas do
            if Pixels[ Object__    .Left + x, Object__    .Top + y ] =
               Pixels[ Background__.Left + x, Background__.Top + y ] then begin
               Pixels[ Object__    .Left + x, Object__    .Top + y ] := MaskColor;
               Inc ( Result );
               end;
  end;

  function MaskImage__( BitMap__ : TBitMap; const Object__, Background__ : TRect ) : Integer;
  var x, y, MaskPct : Integer; MaskLimit : Single; MaskColor : TColor; rgb1, rgb2 : TRGB; hsv1, hsv2 : THSVColor;
  begin
    Result := 0;
    MaskColor := RGBToColor( __RGB( 255, 0, 255 ) ); // magenta
    MaskPct := 1;
    MaskLimit := ( 2 * MaskPct ) / 100;
    for x := 0 to Pred( RectWidth( Object__ ) ) do
        for y := 0 to Pred( RectHeight( Object__ ) ) do with BitMap__.Canvas do begin
            rgb1 := ColorToRGB( Pixels[ Object__    .Left + x, Object__    .Top + y ] );
            rgb2 := ColorToRGB( Pixels[ Background__.Left + x, Background__.Top + y ] );
            RGBToHSV( rgb1, hsv1 );
            RGBToHSV( rgb2, hsv2 );
            if   ( ( Abs( hsv1.h - hsv2.h ) / 360.00)
                    +
                    ( Abs( hsv1.s - hsv2.s ) )
                 )
                 <= MaskLimit then begin
                 Pixels[ Object__    .Left + x, Object__    .Top + y ] := MaskColor;
                 Inc ( Result );
                 end;
            end;
  end;

begin //exit;
  with OpenForm do begin
    try
      Skin:=TPict.Create;
      try
        DirectoryListBox1.Directory:=PATH;
        DirectoryListBox1.Update;
//      FileListBox1.Mask:='*.PNG';
        FileListBox1.Mask:='*.BMP';
        FileListBox1.Directory:=PATH;
        FileListBox1.Update;
        for i:=0 to Pred(FileListBox1.Items.Count) do begin
                 s:=StrWithTrailingPathDelimiter(FileListBox1.Directory)+FileListBox1.Items[i];
                 //Msg( s, '', MB_OK );

                 if   Skin.LoadFromfile( s ) then with Skin do begin
                      if not Assigned( OrgBitMap ) then
                         MakeOrgBitMapFromPict;

                      W := OrgBitMap.Width div 4;
                      H := OrgBitMap.Height div 8;
                      if   BitMapResize( BitMap, W * 15, H ) then with BitMap do with Canvas do begin
                           CopyMode := cmSrcCopy;
                           Brush.Style := bsSolid;
                           Brush.Color := clGray;
                           FillRect( Rect( 0, 0, BitMap.Width,BitMap.Height ) );
                           CopyRect( CellToRect( 0,0,W,H), OrgBitMap.Canvas, CellToRect(0,2,W,H) ); // wall
                           CopyRect( CellToRect( 1,0,W,H), OrgBitMap.Canvas, CellToRect(0,0,W,H) ); // floor
                           CopyRect( CellToRect( 2,0,W,H), OrgBitMap.Canvas, CellToRect(0,1,W,H) ); // goal
                           CopyRect( CellToRect( 3,0,W,H), OrgBitMap.Canvas, CellToRect(2,0,W,H) ); // box-on-floor
                           CopyRect( CellToRect( 4,0,W,H), OrgBitMap.Canvas, CellToRect(2,1,W,H) ); // box-on-goal
                           CopyRect( CellToRect( 5,0,W,H), OrgBitMap.Canvas, CellToRect(0,4,W,H) ); // player-on-floor-up
                           CopyRect( CellToRect( 6,0,W,H), OrgBitMap.Canvas, CellToRect(1,4,W,H) ); // player-on-floor-left
                           CopyRect( CellToRect( 7,0,W,H), OrgBitMap.Canvas, CellToRect(2,4,W,H) ); // player-on-floor-down
                           CopyRect( CellToRect( 8,0,W,H), OrgBitMap.Canvas, CellToRect(3,4,W,H) ); // player-on-floor-right
                           CopyRect( CellToRect( 9,0,W,H), OrgBitMap.Canvas, CellToRect(0,5,W,H) ); // player-on-goal-up
                           CopyRect( CellToRect(10,0,W,H), OrgBitMap.Canvas, CellToRect(1,5,W,H) ); // player-on-goal-left
                           CopyRect( CellToRect(11,0,W,H), OrgBitMap.Canvas, CellToRect(2,5,W,H) ); // player-on-goal-down
                           CopyRect( CellToRect(12,0,W,H), OrgBitMap.Canvas, CellToRect(3,5,W,H) ); // player-on-goal-right
                           CopyRect( CellToRect(13,0,W,H), OrgBitMap.Canvas, CellToRect(3,2,W,H) ); // background image
                           CopyRect( CellToRect(14,0,W,H), OrgBitMap.Canvas, CellToRect(3,3,W,H) ); // Settings

                           MaskImage( BitMap, CellToRect( 3,0,W,H), CellToRect( 1,0,W,H) );         // box             - floor
                           MaskImage( BitMap, CellToRect( 4,0,W,H), CellToRect( 2,0,W,H) );         // box-on-goal     - goal
                           MaskImage( BitMap, CellToRect( 5,0,W,H), CellToRect( 1,0,W,H) );         // player-on-floor - floor
                           MaskImage( BitMap, CellToRect( 6,0,W,H), CellToRect( 1,0,W,H) );         // player-on-floor - floor
                           MaskImage( BitMap, CellToRect( 7,0,W,H), CellToRect( 1,0,W,H) );         // player-on-floor - floor
                           MaskImage( BitMap, CellToRect( 8,0,W,H), CellToRect( 1,0,W,H) );         // player-on-floor - floor
                           MaskImage( BitMap, CellToRect( 9,0,W,H), CellToRect( 2,0,W,H) );         // player-on-goal  - goal
                           MaskImage( BitMap, CellToRect(10,0,W,H), CellToRect( 2,0,W,H) );         // player-on-goal  - goal
                           MaskImage( BitMap, CellToRect(11,0,W,H), CellToRect( 2,0,W,H) );         // player-on-goal  - goal
                           MaskImage( BitMap, CellToRect(12,0,W,H), CellToRect( 2,0,W,H) );         // player-on-goal  - goal

                           MaskImage( BitMap, CellToRect( 9,0,W,H), CellToRect( 5,0,W,H) );         // player-on-goal  - player-on-floor up
                           MaskImage( BitMap, CellToRect(10,0,W,H), CellToRect( 6,0,W,H) );         // player-on-goal  - player-on-floor left
                           MaskImage( BitMap, CellToRect(11,0,W,H), CellToRect( 7,0,W,H) );         // player-on-goal  - player-on-floor down
                           MaskImage( BitMap, CellToRect(12,0,W,H), CellToRect( 8,0,W,H) );         // player-on-goal  - player-on-floor right

                           MaskImage( BitMap, CellToRect( 2,0,W,H), CellToRect( 1,0,W,H) );         // goal            - floor

                           BitMap.SaveToFile( 'C:\Temp5\' + ChangeFileExt( FileListBox1.Items[i], BMP_FILE_EXT ) );
                           end
                      else Error( s, TEXT_TASK_FAILED );
                      end
                 else Error( s, '' );
               end;
      finally Skin.Free;
      end;
    except on E : Exception do Error( E.Message, '' );
    end;
    end;
end;
}
{
procedure TMainForm.AdHocTask;
var a,i,j,k,MaskPct:Integer; p:PRGBVector; rgb:BitMap_.TRGB; Pict:TPict;
    MaskColor : TRGB;

  function  IsBackgroundPixel( Col__, Row__ : Integer ) : Boolean;
  var rgb : TRGB;
  begin
    Result := False;
    if ( Col__ >= 0 ) and ( Col__ < Pict.OrgBitMap.Width  ) and
       ( Row__ >= 0 ) and ( Row__ < Pict.OrgBitMap.Height ) then begin
       rgb := ColorToRGB( Pict.OrgBitMap.Canvas.Pixels[ Col__, Row__ ] );
       Result := ( rgb.r = 255 ) and ( rgb.g = 0 ) and ( rgb.b = 255 ); // magenta
       end;
  end;

begin
  Pict:=TPict.Create;
  try
                 if Pict.LoadFromFile('c:\temp\t1.bmp') and
                    (Pict.OrgBitMap<>nil) or (Pict.MakeOrgBitMapFromPict) then begin
                    k:=0;
                    MaskColor := ColorToRGB( clWhite );
                    //MaskPct := 10;
                    MaskPct := 20;
                    for i:=0 to Pred(Pict.OrgBitMap.Height) do
                        begin p:=Pict.OrgBitMap    .ScanLine[i];
                              for j:=0 to Pred(Pict.OrgBitMap.Width) do
                                  begin rgb:=p[j];
                                        if   // (rgb.r<32) and (rgb.g<32) and (rgb.b<32) then
                                             ((Abs(rgb.r-MaskColor.r)+
                                               Abs(rgb.g-MaskColor.g)+
                                               Abs(rgb.b-MaskColor.b))*100) div (3*255) <= MaskPct then
                                             begin
                                               if IsBackgroundPixel( j, Pred( i ) ) or
                                                  IsBackgroundPixel( Pred( j ), i ) or
                                                  IsBackgroundPixel( Succ( j ), i ) or
                                                  IsBackgroundPixel( j, Succ( i ) ) then begin
                                                   //a:=-DELTA+Random(2*DELTA);
                                                   a:=32-Min(rgb.r,Min(rgb.g,rgb.b));
                                                   Inc(rgb.r,a); Inc(rgb.g,a); Inc(rgb.b,a);
                                                   //if Random(2)=0 then
                                                      //rgb.r:=rgb.r-DELTA+Random(2*DELTA);
                                                   //if Random(2)=0 then
                                                      //rgb.g:=rgb.g-DELTA+Random(2*DELTA);
                                                   //if Random(2)=0 then
                                                      //rgb.b:=rgb.b-DELTA+Random(2*DELTA);
                                                   p[j]:=rgb;
                                                   p[j].r:=255; p[j].g:=0; p[j].b:=255;
                                                   //p[j].r:=0; p[j].g:=255; p[j].b:=0;
                                                   Inc(k);
                                                   end;
                                             end;
                                  end;
                        end;
                    if  k<>0 then begin
                        Pict.FileName:='c:\temp\t4.bmp';
                        try Pict.OrgBitMap.SaveToFile(Pict.FileName);
                        except on E:Exception do Error(E.Message,'');
                        end;
                        end;
                    end;
  finally Pict.Free;
  end;
end;
}

procedure TMainForm.AdHocTask;
begin
end;

end.

