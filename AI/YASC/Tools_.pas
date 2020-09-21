unit Tools_;

{$DEFINE YASC} // 'YASC' defined: compile the unit as a part of 'Sokoban YASC'

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, ExtCtrls, ToolWin, ImgList, Buttons,
  ExtDlgs, jpeg, Grids,
  {$IFDEF YASC}
    YASGen_,
  {$ENDIF}
  Misc_,Pict_,Res_,IniFile_,SokUtil_,SokFile_,SokGame_,Game_,Sprite_,
  Plugin_,Generator_, Generator2_, Spin;

const
  COLOR_DARK_BLUE                        = TColor($200000); {BGR, not RGB}
  DEFAULT_SELECTION_CURSOR_PEN_COLOR     = clWhite;
  DEFAULT_SELECTION_CURSOR_SHADOW_COLOR  = {clBlack;} COLOR_DARK_BLUE;

  MAX_CURSOR_PEN_WIDTH                   = 4; // must be even and >= 'DEFAULT_SQUARE_CURSOR_PEN_WIDTH' and >= 'DEFAULT_SELECTION_CURSOR_PEN_WIDTH'

  MSG_PLUGIN_REFRESH_CHECKBOXES          = 1; // 'Msg.WParam' parameters for the 'Misc_.MSG_OPTIMIZER_REFRESH' and MSG_SOLVER_REFRESH messages
  MSG_PLUGIN_REFRESH_COMPACT             = 2;
  MSG_PLUGIN_REFRESH_IMPORT              = 3; // e.g., import solutions from solver
  MSG_PLUGIN_REFRESH_ROW                 = 4;

  TAB_SHEET_SPACES                       = 10;

  TASK_QUEUE_LEFT_MARGIN_CHARACTERS      = SPACE;
  TASK_QUEUE_RIGHT_MARGIN_CHARACTERS     = SPACE;


  TILE_INDEX_PLAYER                      = 0; // toolsform skin tile-set mapping
  TILE_INDEX_PLAYER_ON_GOAL              = 1;
  TILE_INDEX_BOX                         = 2;
  TILE_INDEX_BOX_ON_GOAL                 = 3;
  TILE_INDEX_GOAL                        = 4;
  TILE_INDEX_WALL                        = 5;
  TILE_INDEX_FLOOR                       = 6;

type
  TAlternateOptimizationTaskType=(PrimarySecondary,SecondaryPrimary,BoxLinesPrimary,BoxLinesSecondary); // the order cannot change
  TAlternateOptimizationTask=record
    Metrics:TMetrics; // best result so far
    PrimaryMetric:TGameMetrics;
    SecondaryMetric:TGameMetrics; // when 'SecondaryMetric = 'PrimaryMetric' then there is only a primary optimization criteria
    SelectedRange:TSelectedRange; // selected range of pushes
    SerialNo:TSerialNo; // snapshot ID
    Transitions:set of TAlternateOptimizationTaskType; // remember transitions in order to support 'alternate once' and 'alternate repeatedly'
    TryAlternate:Boolean; // 'True': the alternate optimization task type hasn't beed tried yet
    TryBoxLines:Boolean; // 'True': the corresponding boxlines optimization task type hasn't been tried yet (only for primary/secondary and secondary/primary tasks)
    end;
  TAlternateOptimization=record
    CurrentTaskSerialNo:TSerialNo;
    HijackedSelectedRange:TSelectedRange;
    HijackedTaskOptimizationFlags:Integer;
    HijackedTaskSerialNo:TSerialNo;
    OriginalPrimaryMetric:TGameMetrics;
    OriginalSecondaryMetric:TGameMetrics;
    PartitionSolutionIntoSubIntervals:Boolean; // when 'True', it isn't really alternate optimizations which are in progress; instead, the optimizer works on consecutive subintervals of a solution;
    Tasks:array[TAlternateOptimizationTaskType] of TAlternateOptimizationTask;
    end;
  TCalculateGameInformation=procedure(Game__:TSokoGame);
  TSelection = record
    BoardAsText:String; // using a string is a convenient way to allocate the memory dynamically
    BoardHeight:Integer;
    BoardWidth:Integer;
    Enabled:Boolean;  // the selection is active
    HasBoard:Boolean; // the selection contains a board fragment
    NonFloorCellCount:Integer;
    Rect:TRect;
    end;
  TCornerType = (ctTopLeft,ctTopRight,ctBottomLeft,ctBottomRight);
  TCornerTypeSet = set of TCornerType;
  TCursorInfo = record
    PenColor:TColor;
    PenWidth:Integer;
    ShadowColor:TColor;
    Size:Integer;
    end;
  TEditorCursorType = (ctCell,ctEraser,ctSelection);

  TEditorHistoryAction=(ehaBoard,ehaBox,ehaErase,ehaGoal,ehaHeight,ehaPlayer,ehaMirror,ehaRotate,ehaSquare,ehaTranslate,ehaWall,ehaWidth);
  TEditorTransaction=record
    IsBoardBoundsOK:Boolean; // 'True': recalculation isn't necessary after completing all items in the transaction
    Result:Boolean; // 'True': all actions in current transaction until now have succeeded
    StartItemIndex:Integer;
    end;
  TEditorHistoryFileHeader=record
    ItemCount:Integer;
    ItemIndex:Integer; // last item in the current transaction
    MagicID1:Integer;
    MagicID2:Integer;
    TransactionCount:Integer;
    TransactionNumber:Integer; // current transaction
    end;
  TEditorHistoryItem=record
    Action:TEditorHistoryAction;
    Col:Integer;
    Parity:Integer;
    Row:Integer;
    Value:Integer;
    end;
  TMethod=procedure of object;

  TEditorHistory=class
  private
    CalculateGameInformation:TCalculateGameInformation;
    fFileName:String;
    FileHeader:TEditorHistoryFileHeader;
    fLastFileName:String;
    fStartPositionTransactionNumber:Integer;
    HistoryFile:file of Integer;
    Refresh    :TNotifyEvent;
    Transaction:TEditorTransaction;

    function    ReadFileHeader:Boolean;
    function    WriteFileHeader:Boolean;

  protected
    function    GetItem(Index__:Integer):TEditorHistoryItem;
    procedure   SetItemIndex(ItemIndex__:Integer);
    procedure   SetItem(Index__:Integer; const Item__:TEditorHistoryItem);
    procedure   SetPosition(Position__:Integer);

  public
    Game        :TSokoGame;

    constructor Create(Game__:TSokoGame; Refresh__:TNotifyEvent; CalculateGameInformation__:TCalculateGameInformation);
    destructor  Destroy; override;

    function    AddBoard:Boolean;
    function    AddItem(const Item__:TEditorHistoryItem):Boolean;
    function    BeginTransaction(ItemCount__:Integer):Boolean;
    function    CalculateBoardBounds:Boolean; // calculate board bounds and top-left justify the board;
    function    CanUndoSession:Boolean;
    function    Clear:Boolean;
    function    Close:Boolean;
    function    DoItem(const Item__:TEditorHistoryItem):Boolean;
    function    EndTransaction(Commit__:Boolean):Boolean;
    function    IsATransactionInProgress:Boolean;
    function    LoadBoard(Index__:Integer; var BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Boolean;
    function    Log:Boolean;
    function    MakeFileName:String;
    function    MakeItem(Action__:TEditorHistoryAction; Col__,Row__,Value__,Parity__:Integer):TEditorHistoryItem;
    function    New(const FileName__:String):Boolean;
    function    Open(const FileName__:String; var BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Boolean;
    function    UndoItem(Item__:TEditorHistoryItem):Boolean;
    function    UndoSession:Boolean;

    property    Count:Integer read FileHeader.TransactionCount;
    property    FileName:String read fFileName;
    property    StartPositionTransactionNumber:Integer read fStartPositionTransactionNumber;
    property    ItemCount:Integer read FileHeader.ItemCount;
    property    ItemIndex:Integer read FileHeader.ItemIndex write SetItemIndex;
    property    Items[Index__:Integer]:TEditorHistoryItem read GetItem write SetItem;
    property    Position:Integer read FileHeader.TransactionNumber write SetPosition;
  end;

  TToolFlags  = (tfEditCurrentPosition);
  TToolFlagSet= set of TToolFlags;

  TEditor = record
    BoardCellOrigin:TPoint; // board position in cell coordinates, 0-based columns and rows
    Cursor:TEditorCursorType;
    CursorRect:TRect; // current cursor position in cell coordinates
    CursorRectPixels:TRect; // current cursor position in pixels
    Cursors:array[TEditorCursorType] of TCursorInfo;
    DragPoint:TPoint;
    DrawingTool:TDrawingTool;
    DrawingToolCursorsEnabled:Boolean;
    DrawingToolsEnabled:Boolean;
    EditPopupMenuRightClickPosition:TPoint;
    FileName:String;
    History:TEditorHistory;
    IsDragging:Boolean;
    IsResizing:Boolean;
    IsSelectingANewArea:Boolean;
    LastCursorPict:TDrawingTool;
    LastValidFileName:String;
    LevelHasChangedOrHasBeenModified:Boolean;
    MouseButton:TMouseButton;
    MouseButtonDown:Boolean;
    MouseWheelPresent:Boolean;
    ToggledItemPos:TPoint;
    Selection:TSelection;
    SizeHandle:TGrabHandle;
    UseCursorPictures:Boolean;
    end;

  TReplayOption = (roBackwards,roForwards,roReplay,roStep,roHome,roEnd,roPosition);
  TReplayOptions = set of TReplayOption;

  TReplayInfo=record
    BoardAsText:TBoardAsText;     // the board; using a string is a convenient way to allocate the memory dynamically
    IsLoaded:Boolean;             // 'True': the board and the moves have been loaded into 'MainForm.OpenForm.Game'
    MoveCount:Integer;            // current position in the game
    MovesAsText:String;           // the moves; using a string is a convenient way to allocate the memory dynamically
    PluginFileName:String;        // the plugin (e.g., solver or optimizer) that produced the moves
    ReplayOptions:TReplayOptions; // replay control information
    SelectedRange:TSelectedRange; // [start, end, maximum range, _, _, _, repeat interval size]; the moves selected for optimization, start exclusive, end inclusive
    end;

  TPluginLevelInfo=record
    BoardAsText:TBoardAsText;     // the board; using a string is a convenient way to allocate the memory dynamically
    ErrorText:String;
    IsALegalLevel:Boolean;
    LevelName:String;
    NewGamesCount:Integer;        // new pending solutions/snapshots for the current level
    ReplayInfo:TReplayInfo;
    SelectedSquaresAsText:String; // selected squares, e.g., only optimize moves passing a set of squares; using a string is a convenient way to allocate the memory dynamically
    end;

  TToolsFormSprite = class(TSpriteN)
  public
    ItemIndex :Integer;
    function  SaveBackground(const Rect__:TRect; IsGoal__:Boolean):Boolean;
  end;

  TToolsGameViewer = record
    BackgroundInitialized:Boolean;
    BackgroundPict:TPict;
    BoardDimensionsAsText:TBoardDimensionsAsText;
    BoardImage:TImage;
    BoardPanel:TPanel;
    BoxSprite:TToolsFormSprite;
    CellsPixelOrigin:TPoint;
    ColCount:Integer;
    ColWidth:Integer;
    CursorPict:TPict;
    FloorTilesVisible:Boolean;
    FrameColor:TColor;
    FrameShadowColor:TColor;
    MouseSuperPosition:TPoint;
    PlayerSprite:TToolsFormSprite;
    RowCount:Integer;
    RowHeight:Integer;
    SkinPict:TPict;
    SkinInitialized:Boolean;
    SquareSetSelectedSquaresColor,SquareSetNotSelectedSquaresColor:TColor;
    SquareSetTransparencyPct:Integer;    
    end;

  TStringsDB = class(TStringList) // a primitive collection of reference-counted non-empty strings; in contrast to 'TStringList', the collection uses case-sensitivity as default setting
  private
    FreeList:Integer;
    function    Add0(const S__: String; CaseSensitive__: Boolean): Integer;
  public
    constructor Create;
    function    Add(const S__: String): Integer; override;
    function    AddCI(const S__: String): Integer; virtual; // case-insensitive add
    procedure   AdjustReferenceCount(Index__,Adjustment__:Integer);
    procedure   Clear; override;
    procedure   Delete(Index__: Integer); override;
    function    IndexOf(const S__: String): Integer; override;
    function    IndexOfCI(const S__: String): Integer; virtual; // case-insensitive look-up
  end;

  TStatisticsMetrics = array[TGameMetrics] of Int64;

  TTaskQueue = class
  private
    fHighlightedRowNumber:Integer;
    fItemsCheckBox:TCheckBox;
    fOldStringGridRow:Integer;
    fPlugin:TPlugin;
    fPopupMenu:TPopupMenu;
    fRunNo:Cardinal;
    fSelectedCount:Integer;
    fStatisticsMetrics:TStatisticsMetrics;
    fStatisticsSucceededCount:Integer;
    fStatisticsTaskCount:Integer;
//  fStatisticsText:TRichEdit;
    fStatisticsText:TMemo;
    fStatisticsTimeMS:Int64;
    fStringGrid:TStringGrid;
    function    GetCheckBoxForRow(Row__:Integer; var CheckBox__:TCheckBox):Boolean;
    function    HasCheckBoxInRow(Row__:Integer; var CheckBox__:TCheckBox):Boolean;
  protected
    function    GetLevels(Row__:Integer):TLevel;
    function    GetOptimizationFlags(Row__:Integer):Integer;
    function    GetRowCount:Integer;
    function    GetSelected(Row__:Integer):Boolean;
    procedure   SetHighlightedRowNumber(Row__:Integer);
    procedure   SetLevels(Row__:Integer; Level__:TLevel);
    procedure   SetOptimizationFlags(Row__,Value__:Integer);
    procedure   SetRowCount(RowCount__:Integer);
    procedure   SetSelected(Row__:Integer; Value__:Boolean);
    procedure   SwapRows(Row1__,Row2__:Integer; Show__:Boolean);
  public
    constructor Create(Plugin__:TPlugin; StringGrid__:TStringGrid; ItemsCheckBox__:TCheckBox; PopupMenu__:TPopupMenu);
    destructor  Destroy; override;
    function    Add(BoardWidth__,BoardHeight__:Integer; LevelTagFlags__:TLevelTagFlagsSet; SnapshotFlags__:Integer; const LevelName__,BoardAsText__:String; Snapshot__:TSnapshot; Selected__:Boolean; var NewLevel__:TLevel):Boolean;
    function    AddStatistics(const PluginName__,PluginTypeText__,LevelName__:String; TimeMS__:TTimeMS; TaskResult__:TPluginResult; MoveCount__,PushCount__:Integer; const TaskResultText__:String):Boolean;
    function    BestMatchOptimization(Optimization__:Integer):Integer;
    procedure   Clear; virtual;
    procedure   ClearStatistics;
    function    CopyStatisticsToClipboard:Boolean;
    function    DeleteRow(Row__:Integer):Boolean;
    procedure   DestroyCheckBox(Row__:Integer);
    procedure   FocusLevelName(const LevelName__:String; First__:Boolean);
    function    GetNextRunNo:Cardinal;
    function    ImportSolutionsForCurrentLevel(DeleteImportedLevels__:Boolean):Integer;
    procedure   ImportFromOtherPlugins(ImportSolverSolutions__:Boolean);
    procedure   InitializeItemsCheckBox;
    function    IsEmpty:Boolean;
    function    IsPluginLevel(Row__:Integer):Boolean;
    function    Lookup(BoardWidth__,BoardHeight__:Integer; const BoardAsText__:String; Snapshot__:TSnapshot; var Level__:TLevel):Boolean;
    function    LookupSnapshot(Snapshot__:TExtendedSnapshotAsText; var OldLevel__:TLevel; var OldSnapshot__:TExtendedSnapshotAsText):Boolean;
    function    MakeRow(var ARow__:Integer):Boolean;
    procedure   OnCheckBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   OnStringGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure   OnStringGridDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure   OnStringGridEnter(Sender: TObject);
    procedure   OnStringGridExit(Sender: TObject);
    procedure   OnStringGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure   OnStringGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   OnStringGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure   OnStringGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   OnStringGridTopLeftChanged(Sender: TObject);
    procedure   Reload(Clear__:Boolean);
    procedure   Refresh(Compact__:Boolean);
    procedure   RefreshRow(Row__:Integer);
    function    ResurrectLevel(Level__:TLevel):Boolean;
    function    SaveToFile( const FileName__ : String ) : Boolean;
    procedure   ScrollInView(Row__:Integer);

    property    HighlightedRowNumber:Integer read fHighlightedRowNumber write SetHighlightedRowNumber;
    property    ItemsCheckBox:TCheckBox read fItemsCheckBox;
    property    Levels[Row__:Integer]:TLevel read GetLevels write SetLevels;
    property    OldStringGridRow:Integer read fOldStringGridRow write fOldStringGridRow;
    property    OptimizationFlags[Row__:Integer]:Integer read GetOptimizationFlags write SetOptimizationFlags;
    property    Plugin:TPlugin read fPlugin write fPlugin;
    property    PopupMenu:TPopupMenu read fPopupMenu;
    property    RowCount:Integer read GetRowCount write SetRowCount;
    property    Selected[Row__:Integer]:Boolean read GetSelected write SetSelected;
    property    SelectedCount:Integer read fSelectedCount;
    property    StatisticsMetrics:TStatisticsMetrics read fStatisticsMetrics;
    property    StatisticsSucceededCount:Integer read fStatisticsSucceededCount;
    property    StatisticsTaskCount:Integer read fStatisticsTaskCount;
//  property    StatisticsText:TRichEdit read fStatisticsText;
    property    StatisticsText:TMemo read fStatisticsText;
    property    StatisticsTimeMS:Int64 read fStatisticsTimeMS;
    property    StringGrid:TStringGrid read fStringGrid;
  end;

  TOptimizerTaskQueue = class(TTaskQueue)
  public
    AlternateOptimization:TAlternateOptimization;

    constructor Create(Plugin__:TPlugin; StringGrid__:TStringGrid; ItemsCheckBox__:TCheckBox; PopupMenu__:TPopupMenu);

    procedure ClearAlternateOptimizationTasks;
    function  IsLevelSelectedForAlternatingOptimizations(Level__:TLevel):Boolean;
    function  RegisterResultForAlternatingOptimizationTasks(SourceSnapshot__,NewSnapshot__:TExtendedSnapshotAsText; RecursionDepth__:Integer):Boolean;
    function  RegisterResultForPartitioningSolutionIntoSubIntervals(SourceSnapshot__,NewSnapshot__:TExtendedSnapshotAsText):Boolean;
    function  RestoreHijackedTask:Boolean;
  end;

  TToolsForm = class(TForm)
    MainMenu: TMainMenu;
    EditMenuFile: TMenuItem;
    EditMenuEdit: TMenuItem;
    HelpMenu: TMenuItem;
    ExitMenu: TMenuItem;
    EditMenuItemNew: TMenuItem;
    EditMenuItemOpen: TMenuItem;
    EditMenuItemSave: TMenuItem;
    EditMenuItemSaveAs: TMenuItem;
    N1: TMenuItem;
    EditMenuItemExit: TMenuItem;
    EditMenuItemUndo: TMenuItem;
    EditMenuItemRedo: TMenuItem;
    N2: TMenuItem;
    EditMenuItemCut: TMenuItem;
    EditMenuItemCopy: TMenuItem;
    EditMenuItemPaste: TMenuItem;
    EditMenuItemDelete: TMenuItem;
    EditMenuItemSelectAll: TMenuItem;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    TabSheetEditor: TTabSheet;
    PanelBtn: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    EditToolButtonNew: TToolButton;
    EditToolButtonOpen: TToolButton;
    EditToolButtonSave: TToolButton;
    EditToolButtonSaveAs: TToolButton;
    EditToolButton5: TToolButton;
    EditImageList1: TImageList;
    EditToolButtonCut: TToolButton;
    EditToolButtonCopy: TToolButton;
    EditToolButtonPaste: TToolButton;
    EditToolButtonDelete: TToolButton;
    EditToolButton1: TToolButton;
    EditToolButtonUndo: TToolButton;
    EditToolButtonRedo: TToolButton;
    EditToolButton2: TToolButton;
    EditToolButtonHelp: TToolButton;
    EditToolButtonExit: TToolButton;
    N3: TMenuItem;
    SettingsMenu: TMenuItem;
    SettingsMenuItemButtons: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    EditToolBarLeft: TToolBar;
    EditToolButtonWall: TToolButton;
    EditToolButtonBox: TToolButton;
    EditToolButtonGoal: TToolButton;
    EditToolButtonPlayer: TToolButton;
    EditMenuObjects: TMenuItem;
    EditMenuItemWall: TMenuItem;
    EditMenuItemBox: TMenuItem;
    EditMenuItemGoal: TMenuItem;
    EditMenuItemPlayer: TMenuItem;
    EditToolBarRight: TToolBar;
    EditMenuItemErase: TMenuItem;
    EditToolButtonFloor: TToolButton;
    EditMenuItemSelect: TMenuItem;
    EditToolButtonErase: TToolButton;
    EditMenuFunctions: TMenuItem;
    EditMenuItemRotateClockwise: TMenuItem;
    EditMenuItemRotateCounterClockwise: TMenuItem;
    EditMenuItemFlipVertically: TMenuItem;
    EditMenuItemFlipHorizontally: TMenuItem;
    EditToolButtonRotateClockwise: TToolButton;
    EditToolButtonRotateCounterClockwise: TToolButton;
    EditToolButtonFlipVertically: TToolButton;
    EditToolButtonFlipHorizontally: TToolButton;
    SettingsMenuItemDefaultButtons: TMenuItem;
    N4: TMenuItem;
    SettingsMenuItemWindowSize: TMenuItem;
    SettingsMenuItemWindowSizeDefault: TMenuItem;
    SettingsMenuItemWindowSizeDefaultCentered: TMenuItem;
    SettingsMenuItemWindowSizeMaximized: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusLabel1: TLabel;
    StatusLabel2: TLabel;
    PanelBtnBevel1: TBevel;
    StatusLabel3: TLabel;
    EditImageDefaultSkin: TImage;
    EditImageDefaultButtons: TImage;
    EditPopupMenuRightClick: TPopupMenu;
    EditPopupMenuItemCut: TMenuItem;
    EditPopupMenuItemCopy: TMenuItem;
    EditPopupMenuItemPaste: TMenuItem;
    N5: TMenuItem;
    EditPopupMenuItemSelectAll: TMenuItem;
    EditPopupMenuItemDelete: TMenuItem;
    EditPopupMenuUndoRedo: TPopupMenu;
    EditMenuItemUndoRedoTransaction: TMenuItem;
    EditMenuItemUndoRedoAllTransactions: TMenuItem;
    EditMenuItemUndoRedoAllTransactionsSeparator: TMenuItem;
    EditMenuItemRefresh: TMenuItem;
    EditMenuView: TMenuItem;
    EditMenuItemHistoryLog: TMenuItem;
    EditPopupMenuOpen: TPopupMenu;
    N6: TMenuItem;
    EditMenuItemInternalClipboard: TMenuItem;
    EditToolButtonInternalClipboard: TToolButton;
    N7: TMenuItem;
    SettingsMenuItemDefaultSkin: TMenuItem;
    SettingsMenuItemSkin: TMenuItem;
    SettingsMenuItemBackground: TMenuItem;
    SettingsMenuItemDefaultBackground: TMenuItem;
    N8: TMenuItem;
    EditPopupMenuItemCopyToInternalClipboard: TMenuItem;
    EditMenuItemCopyToInternalClipboard: TMenuItem;
    N9: TMenuItem;
    SettingsMenuItemMoreSettings: TMenuItem;
    EditMenuItemGrid: TMenuItem;
    TabSheetSolver: TTabSheet;
    PluginMenuEdit: TMenuItem;
    PluginEditMenuItemUnselectAll: TMenuItem;
    PluginEditMenuItemSelectall: TMenuItem;
    N10: TMenuItem;
    PluginEditMenuItemDeleteSolutions: TMenuItem;
    PluginEditMenuItemPaste: TMenuItem;
    PluginEditMenuItemCopy: TMenuItem;
    PluginEditMenuItemCut: TMenuItem;
    PluginMenuPlugin: TMenuItem;
    PluginMenuItemRemove: TMenuItem;
    N11: TMenuItem;
    PluginMenuItemAbout: TMenuItem;
    PluginMenuItemSettings: TMenuItem;
    N12: TMenuItem;
    PluginMenuItemOpenPlugin: TMenuItem;
    PluginMenuFile: TMenuItem;
    PluginMenuItemExit: TMenuItem;
    N13: TMenuItem;
    PluginMenuItemSaveAs: TMenuItem;
    PluginMenuItemSave: TMenuItem;
    PluginMenuItemOpen: TMenuItem;
    PluginToolBar: TToolBar;
    PluginToolButtonOpen: TToolButton;
    PluginToolButtonSave: TToolButton;
    PluginToolButtonSaveAs: TToolButton;
    ToolButton5: TToolButton;
    PluginToolButtonCut: TToolButton;
    PluginToolButtonCopy: TToolButton;
    PluginToolButtonPaste: TToolButton;
    PluginToolButtonDelete: TToolButton;
    ToolButton10: TToolButton;
    PluginToolButtonHelp: TToolButton;
    PluginToolButtonExit: TToolButton;
    PluginLevelGroupBox: TGroupBox;
    BtnSolveLevel: TButton;
    PanelPluginLevelInfo: TPanel;
    PluginLevelMemo: TMemo;
    PluginLevelStringGrid: TStringGrid;
    PluginLevelFileNamePanel: TPanel;
    SolverGroupBox: TGroupBox;
    SolverComboBox: TComboBox;
    BtnSolverBrowse: TButton;
    CurrentSolverGroupBox: TGroupBox;
    BtnSolverSettings: TButton;
    BtnSolverAbout: TButton;
    LevelGroupBox: TGroupBox;
    LevelNamePanel: TPanel;
    PanelBoard: TPanel;
    ImageBoard: TImage;
    LevelMemo: TMemo;
    N14: TMenuItem;
    PluginMenuItemClearLog: TMenuItem;
    PanelToolTips: TPanel;
    PluginToolButtonReplay: TToolButton;
    PluginToolButtonStopReplay: TToolButton;
    ToolButton3: TToolButton;
    ImageReplaySpeed: TImage;
    TabSheetOptimizer: TTabSheet;
    OptimizerGroupBox: TGroupBox;
    OptimizerComboBox: TComboBox;
    BtnOptimizerBrowse: TButton;
    CurrentOptimizerGroupBox: TGroupBox;
    BtnOptimizerSettings: TButton;
    BtnOptimizerAbout: TButton;
    BtnOptimizeGames: TButton;
    OptimizeSolutionsGroupBox: TGroupBox;
    OptimizeSolutionsGridPanel: TPanel;
    OptimizeSolutionsStringGrid: TStringGrid;
    PluginToolButtonAdd: TToolButton;
    PluginMenuItemAdd: TMenuItem;
    OptimizerTaskQueueItemsCheckBox: TCheckBox;
    OptimizationComboBox: TComboBox;
    SelectBtn: TButton;
    PluginToolButtonAddAndDeleteSeparator: TToolButton;
    PluginEditMenuItemDeleteSeparator: TMenuItem;
    ToolButton1: TToolButton;
    PluginToolButtonOpenPrior: TToolButton;
    PluginToolButtonOpenNext: TToolButton;
    N15: TMenuItem;
    PluginMenuItemOpenPrior: TMenuItem;
    PluginMenuItemOpenNext: TMenuItem;
    TaskQueuePopupMenu: TPopupMenu;
    PopupMenuItemSort: TMenuItem;
    PopupMenuItemSortOnLevelNames: TMenuItem;
    PopupMenuItemSortOnSolutionNames: TMenuItem;
    PopupMenuItemSortOnMoves: TMenuItem;
    PopupMenuItemSortOnPushes: TMenuItem;
    PopupMenuItemSortOnBoxLines: TMenuItem;
    PopupMenuItemSortOnBoxChanges: TMenuItem;
    PopupMenuItemSortOnPushingSessions: TMenuItem;
    PopupMenuItemDeleteSolutions: TMenuItem;
    N16: TMenuItem;
    PopupMenuItemSortOnOptimizationSeparator: TMenuItem;
    PopupMenuItemSortOnOptimization: TMenuItem;
    PopupMenuItemSortOnSelected: TMenuItem;
    PluginMenuItemSort: TMenuItem;
    PluginMenuItemSortOnSelected: TMenuItem;
    PluginMenuItemSortOnLevelNames: TMenuItem;
    PluginMenuItemSortOnSolutionNames: TMenuItem;
    N18: TMenuItem;
    PluginMenuItemSortOnMoves: TMenuItem;
    PluginMenuItemSortOnPushes: TMenuItem;
    PluginMenuItemSortOnBoxlines: TMenuItem;
    PluginMenuItemSortOnBoxChanges: TMenuItem;
    PluginMenuItemSortOnPushingSessions: TMenuItem;
    PluginMenuItemSortOnOptimizationSeparator: TMenuItem;
    PluginMenuItemSortOnOptimization: TMenuItem;
    PluginMenuItemAddSeparator: TMenuItem;
    PluginEditMenuItemCopySolutionSeparator: TMenuItem;
    PluginEditMenuItemCopySolution: TMenuItem;
    PopupMenuItemCopySolutionSeparator: TMenuItem;
    PopupMenuItemCopySolution: TMenuItem;
    EditMenuItemFill: TMenuItem;
    EditMenuItemFillWithWalls: TMenuItem;
    EditMenuItemFillWithFloors: TMenuItem;
    EditMenuItemFillWithGoals: TMenuItem;
    EditMenuItemFillWithBoxes: TMenuItem;
    EditMenuItemFillWithBoxesOnGoals: TMenuItem;
    EditMenuItemFillWithWallBorderOpaque: TMenuItem;
    EditMenuItemFillWithWallBorderTransparent: TMenuItem;
    N21: TMenuItem;
    EditPopupMenuItemFill: TMenuItem;
    EditPopupMenuItemFillWithBoxesOnGoals: TMenuItem;
    EditPopupMenuItemFillWithBoxes: TMenuItem;
    EditPopupMenuItemFillWithGoals: TMenuItem;
    EditPopupMenuItemFillWithFloors: TMenuItem;
    EditPopupMenuItemFillWithWalls: TMenuItem;
    EditPopupMenuItemFillWithWallBorderOpaque: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    PluginEditMenuItemDeleteLevels: TMenuItem;
    SolveLevelsGroupBox: TGroupBox;
    SolveLevelsGridPanel: TPanel;
    SolveLevelsStringGrid: TStringGrid;
    BtnGeneralSolverSettings: TButton;
    SolverTaskQueueItemsCheckBox: TCheckBox;
    BtnGeneralOptimizerSettings: TButton;
    PopupMenuItemDeleteLevels: TMenuItem;
    PluginEditMenuItemStatisticsSeparator: TMenuItem;
    PluginEditMenuItemStatistics: TMenuItem;
    PluginEditMenuItemCopyStatisticsToClipboard: TMenuItem;
    PluginEditMenuItemClearStatistics: TMenuItem;
    EditToolBarTop: TToolBar;
    PopupMenuItemSortOnChronologicalOrder: TMenuItem;
    PluginMenuItemSortOnChronologicalOrder: TMenuItem;
    EditMenuItemNormalizeBoard: TMenuItem;
    EditPopupMenuItemRemoveRedundantWalls: TMenuItem;
    TabSheetGenerator: TTabSheet;
    GenerateLevelsGroupBox: TGroupBox;
    GenerateLevelsGridPanel: TPanel;
    GenerateLevelsStringGrid: TStringGrid;
    GeneratorTaskQueueItemsCheckBox: TCheckBox;
    BtnGenerateLevels: TButton;
    GeneratorGroupBox: TGroupBox;
    GeneratorNamePanel: TPanel;
    BtnGeneratorSettings: TButton;
    PanelGeneratorStatus: TPanel;
    GeneratorStatusStringGrid: TStringGrid;
    PluginToolButtonNewCandidateSet: TToolButton;
    GeneratorMenuFile: TMenuItem;
    GeneratorMenuItemExit: TMenuItem;
    N17: TMenuItem;
    GeneratorMenuItemSaveAs: TMenuItem;
    GeneratorMenuItemOpen: TMenuItem;
    GeneratorMenuItemNew: TMenuItem;
    N19: TMenuItem;
    GeneratorMenuItemDeleteFiles: TMenuItem;
    GeneratorMenuItemSave: TMenuItem;
    EditToolButtonGenerator: TToolButton;
    GeneratorMenuEdit: TMenuItem;
    GeneratorEditMenuItemCut: TMenuItem;
    GeneratorEditMenuItemCopy: TMenuItem;
    GeneratorEditMenuItemPaste: TMenuItem;
    N20: TMenuItem;
    GeneratorEditMenuItemDeleteCandidates: TMenuItem;
    GeneratorTaskQueuePopupMenu: TPopupMenu;
    GeneratorPopupMenuItemCut: TMenuItem;
    GeneratorPopupMenuItemCopy: TMenuItem;
    GeneratorPopupMenuItemPaste: TMenuItem;
    N25: TMenuItem;
    GeneratorPopupMenuItemDeleteCandidates: TMenuItem;
    N26: TMenuItem;
    GeneratorEditMenuItemEdit: TMenuItem;
    GeneratorEditMenuItemPlay: TMenuItem;
    PluginToolButtonSeparatorGeneratorEditPlay: TToolButton;
    PluginToolButtonGeneratorEdit: TToolButton;
    PluginToolButtonGeneratorPlay: TToolButton;
    N27: TMenuItem;
    GeneratorPopupMenuItemEdit: TMenuItem;
    GeneratorPopupMenuItemPlay: TMenuItem;
    EditImage1: TImage;
    EditMenuItemCopyToGenerator: TMenuItem;
    PluginMenuItemSortOnPlayerLines: TMenuItem;
    PopupMenuItemSortOnPlayerLines: TMenuItem;
    EditMenuItemMovePlayerAndBoxes: TMenuItem;
    EditMenuItemNormalizeBoardMakeRectangularBoard: TMenuItem;
    SettingsMenuAlternatingOptimizations: TMenuItem;
    SettingsMenuAlternatingOptimizationsEnabled: TMenuItem;
    SettingsMenuAlternatingOptimizationsRepeat: TMenuItem;
    EditMenuItemSeparator: TMenuItem;
    SettingsMenuAlternatingOptimizationsSeparator: TMenuItem;
    TaskQueuePopupMenuItemToggleSelectionSeparator: TMenuItem;
    TaskQueuePopupMenuItemToggleSelectionAboveItem: TMenuItem;
    TaskQueuePopupMenuItemToggleSelectionBelowItem: TMenuItem;
    N28: TMenuItem;
    GeneratorPopupMenuItemToggleSelectionAboveItem: TMenuItem;
    GeneratorPopupMenuItemToggleSelectionBelowItem: TMenuItem;
    SettingsMenuAlternatingOptimizationsType: TMenuItem;
    SettingsMenuAlternatingOptimizationsPrimarySecondary: TMenuItem;
    SettingsMenuAlternatingOptimizationsBoxLinesPrimary: TMenuItem;
    SettingsMenuAlternatingOptimizationsBoxLinesPrimarySecondary: TMenuItem;
    SettingsMenuAlternatingOptimizationsPrimarySecondaryBoxLines: TMenuItem;
    EditPopupMenuItemFillWithWallBorderTransparent: TMenuItem;
    EditMenuItemReplace: TMenuItem;
    EditMenuItemReplaceFindWhat: TMenuItem;
    EditMenuItemReplaceFindWalls: TMenuItem;
    EditMenuItemReplaceFindFloors: TMenuItem;
    EditMenuItemReplaceFindGoals: TMenuItem;
    EditMenuItemReplaceFindBoxes: TMenuItem;
    EditMenuItemReplaceFindBoxesOnGoals: TMenuItem;
    N29: TMenuItem;
    EditMenuItemReplaceReplaceWith: TMenuItem;
    EditMenuItemReplaceReplaceWithWalls: TMenuItem;
    EditMenuItemReplaceReplaceWithFloors: TMenuItem;
    EditMenuItemReplaceReplaceWithGoals: TMenuItem;
    EditMenuItemReplaceReplaceWithBoxes: TMenuItem;
    EditMenuItemReplaceReplaceWithBoxesOnGoals: TMenuItem;
    N30: TMenuItem;
    EditMenuItemReplaceDoIt: TMenuItem;
    EditPopupMenuItemReplace: TMenuItem;
    EditPopupMenuItemReplaceDoit: TMenuItem;
    N31: TMenuItem;
    EditPopupMenuItemReplaceReplaceWithBoxesOnGoals: TMenuItem;
    EditPopupMenuItemReplaceReplaceWithBoxes: TMenuItem;
    EditPopupMenuItemReplaceReplaceWithGoals: TMenuItem;
    EditPopupMenuItemReplaceReplaceWithFloors: TMenuItem;
    EditPopupMenuItemReplaceReplaceWithWalls: TMenuItem;
    EditPopupMenuItemReplaceReplaceWith: TMenuItem;
    N32: TMenuItem;
    EditPopupMenuItemReplaceFindBoxesOnGoals: TMenuItem;
    EditPopupMenuItemReplaceFindBoxes: TMenuItem;
    EditPopupMenuItemReplaceFindGoals: TMenuItem;
    EditPopupMenuItemReplaceFindFloors: TMenuItem;
    EditPopupMenuItemReplaceFindWalls: TMenuItem;
    EditPopupMenuItemReplaceFindWhat: TMenuItem;
    PopupMenuItemPaste: TMenuItem;
    TabSheetCapture: TTabSheet;
    EditToolButtonSelect: TToolButton;
    EditMenuItemFloor: TMenuItem;
    CaptureToolBarTop: TToolBar;
    CaptureToolButtonNew: TToolButton;
    CaptureToolButtonOpen: TToolButton;
    CaptureToolButtonSave: TToolButton;
    CaptureToolButtonSaveAs: TToolButton;
    CaptureToolButton5: TToolButton;
    CaptureToolButtonCut: TToolButton;
    CaptureToolButtonCopy: TToolButton;
    CaptureToolButtonPaste: TToolButton;
    CaptureToolButton1: TToolButton;
    CaptureToolButtonZoom: TToolButton;
    CaptureToolButton3: TToolButton;
    CaptureToolButtonUndo: TToolButton;
    CaptureToolButtonRedo: TToolButton;
    ToolButton2: TToolButton;
    CaptureToolButtonHelp: TToolButton;
    CaptureToolButtonExit: TToolButton;
    CaptureMenuFile: TMenuItem;
    CaptureFileMenuItemExit: TMenuItem;
    N33: TMenuItem;
    CaptureFileMenuItemPrintSetup: TMenuItem;
    CaptureFileMenuItemPrint: TMenuItem;
    N34: TMenuItem;
    CaptureFileMenuItemSaveAs: TMenuItem;
    CaptureFileMenuItemSave: TMenuItem;
    CaptureFileMenuItemOpen: TMenuItem;
    CaptureFileMenuItemNew: TMenuItem;
    CaptureMenuEdit: TMenuItem;
    CaptureEditMenuItemSelectAll: TMenuItem;
    N35: TMenuItem;
    CaptureEditMenuItemPaste: TMenuItem;
    CaptureEditMenuItemCopy: TMenuItem;
    CaptureEditMenuItemCut: TMenuItem;
    N36: TMenuItem;
    CaptureEditMenuItemRedo: TMenuItem;
    CaptureEditMenuItemUndo: TMenuItem;
    CaptureMenuView: TMenuItem;
    CaptureViewMenuItemShowImageAnalysisResults: TMenuItem;
    CaptureViewMenuItemShowColorQuantization: TMenuItem;
    N37: TMenuItem;
    CaptureViewMenuItem200: TMenuItem;
    CaptureViewMenuItem100: TMenuItem;
    CaptureMenuSettings: TMenuItem;
    CaptureSettingsMenuItemImageAnalysis: TMenuItem;
    CaptureSettingsMenuItemStraightLines: TMenuItem;
    CaptureSettingsMenuItemEdgeThinning: TMenuItem;
    CaptureSettingsMenuItemGradientOrientations: TMenuItem;
    CaptureSettingsMenuItemGradientMagnitudes: TMenuItem;
    CaptureSettingsMenuItemYGradients: TMenuItem;
    CaptureSettingsMenuItemXGradients: TMenuItem;
    CaptureSettingsMenuItemGaussianBlur: TMenuItem;
    CaptureSettingsMenuItemGrayscale: TMenuItem;
    CaptureSettingsMenuItemColorQuantization: TMenuItem;
    CaptureSettingsColorQuantization16ColorsItem: TMenuItem;
    CaptureSettingsColorQuantization8ColorsItem: TMenuItem;
    CaptureSettingsColorQuantization4ColorsItem: TMenuItem;
    CaptureSettingsColorQuantization2ColorsItem: TMenuItem;
    CaptureSettingsMenuItemGridColor: TMenuItem;
    N38: TMenuItem;
    CaptureSettingsMenuItemSkinExportFormat: TMenuItem;
    CaptureSettingsMenuItemObjectBackground: TMenuItem;
    N39: TMenuItem;
    CaptureSettingsMenuItemObjectsOnABlackBackground: TMenuItem;
    CaptureSettingsMenuItemObjectsOnFloorsAndGoals: TMenuItem;
    N40: TMenuItem;
    CaptureSettingsMenuItemSingleRowFormat: TMenuItem;
    CaptureSettingsMenuItemCommonSkinFormat: TMenuItem;
    N41: TMenuItem;
    CaptureSettingsMenuItemUseAutomaticCompletion: TMenuItem;
    CaptureSettingsMenuItemSnapToNearbyEdges: TMenuItem;
    N42: TMenuItem;
    CaptureSettingsMenuItemMoreSettings: TMenuItem;
    CaptureImageList1: TImageList;
    CaptureImagePanel: TPanel;
    ImageLabel: TLabel;
    OpenBitBtn: TBitBtn;
    PasteBitBtn: TBitBtn;
    CaptureBoardPanel: TPanel;
    BoardLabel: TLabel;
    LeftLabel: TLabel;
    TopLabel: TLabel;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    LeftSpinEdit: TSpinEdit;
    TopSpinEdit: TSpinEdit;
    WidthSpinEdit: TSpinEdit;
    HeightSpinEdit: TSpinEdit;
    CaptureGridPanel: TPanel;
    GridLabel: TLabel;
    ColumnsLabel: TLabel;
    RowsLabel: TLabel;
    ColumnWidthLabel: TLabel;
    RowHeightLabel: TLabel;
    ColumnsSpinEdit: TSpinEdit;
    ColWidthSpinEdit: TSpinEdit;
    RowsSpinEdit: TSpinEdit;
    RowHeightSpinEdit: TSpinEdit;
    CapturePuzzlePanel: TPanel;
    PuzzleLabel: TLabel;
    SavePuzzleBitBtn: TBitBtn;
    SavePuzzleAsBitBtn: TBitBtn;
    CopyPuzzleBitBtn: TBitBtn;
    CaptureSkinPanel: TPanel;
    SkinLabel: TLabel;
    WallCutLeftLabel: TLabel;
    WallCutTopLabel: TLabel;
    WallCutRightLabel: TLabel;
    WallCutBottomLabel: TLabel;
    Label11: TLabel;
    WallCapCheckBox: TCheckBox;
    SaveSkinBitBtn: TBitBtn;
    SaveSkinAsBitBtn: TBitBtn;
    CopySkinBitBtn: TBitBtn;
    OuterWallCutLeftSpinEdit: TSpinEdit;
    OuterWallCutTopSpinEdit: TSpinEdit;
    OuterWallCutRightSpinEdit: TSpinEdit;
    OuterWallCutBottomSpinEdit: TSpinEdit;
    CaptureSquaresPanel: TPanel;
    SquaresLabel: TLabel;
    CaptureEditToolBarLeft: TToolBar;
    CaptureEditToolButtonWall: TToolButton;
    CaptureEditToolButtonBox: TToolButton;
    CaptureEditToolButtonGoal: TToolButton;
    CaptureEditToolButtonPlayer: TToolButton;
    CaptureEditToolButtonFloor: TToolButton;
    CaptureEditToolButtonErase: TToolButton;
    BottomPanel: TPanel;
    PreviousStepButton: TBitBtn;
    NextStepButton: TBitBtn;
    CaptureStateMemo: TMemo;
    PuzzleInformationPanel: TPanel;
    TitleLabel: TLabel;
    AuthorLabel: TLabel;
    PuzzleTitleEdit: TEdit;
    PuzzleAuthorEdit: TEdit;
    SkinInformationPanel: TPanel;
    Label14: TLabel;
    DesignerLabel: TLabel;
    SkinTitleEdit: TEdit;
    SkinDesignerEdit: TEdit;
    PreviousStepButton2: TBitBtn;
    NextStepButton2: TBitBtn;
    CaptureScrollBox: TScrollBox;
    Memo1: TMemo;
    CaptureImage1: TImage;
    CaptureImage2: TImage;
    PlayPuzzleBitBtn: TBitBtn;
    CaptureToolButtonPlay: TToolButton;
    ToolButton6: TToolButton;
    PlaySkinBitBtn: TBitBtn;
    CaptureEditMenuItemCapture: TMenuItem;
    CaptureBitBtn: TBitBtn;
    SkinBitBtn: TBitBtn;
    CapturePopupMenu: TPopupMenu;
    CaptureMenuItemLoadSkin: TMenuItem;
    CaptureMenuItemRecentSkins: TMenuItem;
    N43: TMenuItem;
    CaptureMenuItemRecentSkinsClearRecentSkins: TMenuItem;
    SkinBitBtn3: TBitBtn;
    N44: TMenuItem;
    CaptureSettingsMenuItemWindowSize: TMenuItem;
    CaptureSettingsMenuItemWindowSizeMaximized: TMenuItem;
    CaptureSettingsMenuItemWindowSizeDefaultCentered: TMenuItem;
    CaptureSettingsMenuItemWindowSizeDefault: TMenuItem;
    N45: TMenuItem;
    CaptureFileMenuItemNewSkin: TMenuItem;
    CaptureSettingsMenuItemDefaultSkin: TMenuItem;
    CaptureSettingsMenuItemSkin: TMenuItem;
    N46: TMenuItem;
    N47: TMenuItem;
    CaptureEditMenuItemMatchingSkin: TMenuItem;
    CaptureEditMenuItemMatchingSkinLoadSkin: TMenuItem;
    CaptureEditMenuItemMatchingSkinRecentSkins: TMenuItem;
    CaptureEditMenuItemMatchingSkinRecentSkinsClearRecentSkins: TMenuItem;
    N48: TMenuItem;
    LockColumnWidthAndRowHeightCheckBox: TCheckBox;
    LeftLabel2: TLabel;
    LeftSpinEdit2: TSpinEdit;
    TopLabel2: TLabel;
    TopSpinEdit2: TSpinEdit;
    SkinBitBtn2: TBitBtn;
    CaptureSettingsMenuItemKeepAuthorAndDesignerNames: TMenuItem;
    TaskQueuePopupMenuItemResetOptimizationMethod: TMenuItem;
    PluginEditMenuItemResetOptimizationMethod: TMenuItem;
    PluginMenuItemSaveTaskQueueAs: TMenuItem;
    PluginMenuItemImportTaskQueue: TMenuItem;
    CaptureEditLevelBitBtn: TBitBtn;
    CaptureEditMenuItemEditLevelSeparator: TMenuItem;
    CaptureEditMenuItemEditLevel: TMenuItem;
    CaptureViewMenuItem400: TMenuItem;
    SettingsMenuAlternatingOptimizationsRepetitionSettingsSeparator: TMenuItem;
    SettingsMenuAlternatingOptimizationsRepetitionSettings: TMenuItem;
    SettingsMenuAlternatingOptimizationsRepeatOnAnyMetrics: TMenuItem;
    SettingsMenuAlternatingOptimizationsRepeatOnMovesPushesBoxLines: TMenuItem;
    TaskQueuePopupMenuItemCopyLevelCreatedFromSolutionSlice: TMenuItem;
    PluginEditMenuItemCopyLevelCreatedFromSolutionSlice: TMenuItem;
    PluginEditMenuItemSelectSolutionSliceToBeOptimized: TMenuItem;
    PluginEditMenuItemSelectAreaToBeOptimized: TMenuItem;
    TaskQueuePopupMenuItemSelectSolutionSliceToBeOptimized: TMenuItem;
    CaptureSelectAllBitBtn: TBitBtn;
    SettingsMenuAlternatingOptimizationsTrigger: TMenuItem;
    SettingsMenuAlternatingOptimizationsTriggerAtLeastOnce: TMenuItem;
    SettingsMenuAlternatingOptimizationsTriggerOnImprovements: TMenuItem;
    PluginEditMenuItemRepeatSolutionSlicing: TMenuItem;
    TaskQueuePopupMenuItemRepeatSolutionSlicing: TMenuItem;
    TaskQueuePopupMenuItemSelectAreaToBeOptimized: TMenuItem;
    PluginEditMenuItemOptimizationSeparator: TMenuItem;
    PopupMenuItemOptimizationSeparator: TMenuItem;
    PluginEditMenuItemSetOptimizationInterval: TMenuItem;
    TaskQueuePopupMenuItemSetOptimizationInterval: TMenuItem;
    PluginEditMenuItemClearBoardAreaSelectedForOptimization: TMenuItem;
    TaskQueuePopupMenuItemClearBoardAreaSelectedForOptimization: TMenuItem;
    N49: TMenuItem;
    CaptureSettingsColorQuantizationIgnoreSmallColorDifferences: TMenuItem;
    EditToolButtonNewLevelWithWalls: TToolButton;
    ToolButton7: TToolButton;
    ToolButtonNewLevelWithWallsWidth: TToolButton;
    ToolButtonNewLevelWithWallsHeight: TToolButton;
    EditPopupMenuNewLevelWithWallsWidthAndHeight: TPopupMenu;
    N50: TMenuItem;
    CaptureViewMenuItemLoadImageFromUpscaledViewOfImage: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExitMenuClick(Sender: TObject);
    procedure TabSheetEditorEnter(Sender: TObject);
    procedure TabSheetEditorExit(Sender: TObject);
    procedure PageControl1Enter(Sender: TObject);
    procedure PageControl1Exit(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure EditMenuItemNewClick(Sender: TObject);
    procedure EditMenuItemOpenClick(Sender: TObject);
    procedure EditMenuItemSaveClick(Sender: TObject);
    procedure EditMenuItemSaveAsClick(Sender: TObject);
    procedure EditMenuItemUndoClick(Sender: TObject);
    procedure EditMenuItemRedoClick(Sender: TObject);
    procedure EditMenuItemCutClick(Sender: TObject);
    procedure EditMenuItemCopyClick(Sender: TObject);
    procedure EditMenuItemPasteClick(Sender: TObject);
    procedure EditMenuItemDeleteOrFillOrReplaceClick(Sender: TObject);
    procedure EditMenuItemSelectAllClick(Sender: TObject);
    procedure HelpMenuClick(Sender: TObject);
    procedure SettingsMenuItemButtonsClick(Sender: TObject);
    procedure SettingsMenuItemDefaultButtonsClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure MenuItemWindowSizeClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EditImage1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditImage1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditImage1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditMenuItemRotateClick(Sender: TObject);
    procedure EditMenuItemFlipClick(Sender: TObject);
    procedure EditMenuItemDrawingToolClick(Sender: TObject);
    procedure EditPopupMenuUndoRedoPopup(Sender: TObject);
    procedure EditToolButtonUndoRedoMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure EditMenuItemRefreshClick(Sender: TObject);
    procedure EditMenuItemUndoRedoTransactionClick(Sender: TObject);
    procedure EditMenuItemHistoryLogClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure SettingsMenuItemSkinClick(Sender: TObject);
    procedure SettingsMenuItemDefaultSkinClick(Sender: TObject);
    procedure EditMenuItemInternalClipboardClick(Sender: TObject);
    procedure SettingsMenuItemBackgroundClick(Sender: TObject);
    procedure SettingsMenuItemDefaultBackgroundClick(Sender: TObject);
    procedure SaveDialog1TypeChange(Sender: TObject);
    procedure EditMenuItemCopyToInternalClipboardClick(Sender: TObject);
    procedure SettingsMenuItemMoreSettingsClick(Sender: TObject);
    procedure EditMenuItemGridClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure StringGridDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure OptimizeSolutionsStringGridDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton;
       Shift: TShiftState; X, Y: Integer);
    procedure SolveLevelMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure PluginEditMenuItemCopyClick(Sender: TObject);
    procedure PluginEditMenuItemPasteClick(Sender: TObject);
    procedure BtnPluginBrowseClick(Sender: TObject);
    procedure PluginMenuItemRemoveClick(Sender: TObject);
    procedure SolverComboBoxChange(Sender: TObject);
    procedure PluginMenuItemSettingsClick(Sender: TObject);
    procedure PluginMenuItemAboutClick(Sender: TObject);
    procedure BtnPluginClick(Sender: TObject);
    procedure PluginToolButtonReplayClick(Sender: TObject);
    procedure PluginToolButtonStopReplayClick(Sender: TObject);
    procedure StatusBar1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageReplaySpeedMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageReplaySpeedMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageReplaySpeedMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OptimizerComboBoxChange(Sender: TObject);
    procedure TabSheetPluginMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure OptimizationComboBoxKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OptimizationComboBoxExit(Sender: TObject);
    procedure OptimizationComboBoxChange(Sender: TObject);
    procedure OptimizeSolutionsStringGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure PluginEditMenuItemDeleteLevelsClick(Sender: TObject);
    procedure PluginMenuItemOpenPriorOrNextClick(Sender: TObject);
    procedure MenuItemSortClick(Sender: TObject);
    procedure PluginMenuItemAddClick(Sender: TObject);
    procedure PluginGroupBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PluginLevelStringGridTopLeftChanged(Sender: TObject);
    procedure PluginEditMenuItemCopySolutionClick(Sender: TObject);
    procedure EditMenuItemFillSelectedArea(Sender: TObject);
    procedure SolveLevelsStringGridDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure SolveLevelsStringGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure PluginEditMenuItemCopyStatisticsToClipboardClick(
      Sender: TObject);
    procedure PluginEditMenuItemClearStatisticsClick(Sender: TObject);
    procedure PluginLevelFileNamePanelClick(Sender: TObject);
    procedure EditMenuItemNormalizeBoardClick(Sender: TObject);
    procedure GenerateLevelsStringGridDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure GenerateLevelsStringGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure BtnGeneratorSettingsClick(Sender: TObject);
    procedure GeneratorMenuItemDeleteFilesClick(Sender: TObject);
    procedure GeneratorMenuItemNewClick(Sender: TObject);
    procedure GeneratorMenuItemSaveClick(Sender: TObject);
    procedure GeneratorMenuItemSaveAsClick(Sender: TObject);
    procedure GeneratorEditMenuItemCutClick(Sender: TObject);
    procedure GeneratorEditMenuItemCopyClick(Sender: TObject);
    procedure GeneratorEditMenuItemPasteClick(Sender: TObject);
    procedure PluginToolButtonCutClick(Sender: TObject);
    procedure PluginToolButtonCopyClick(Sender: TObject);
    procedure PluginToolButtonPasteClick(Sender: TObject);
    procedure GeneratorEditMenuItemEditClick(Sender: TObject);
    procedure GeneratorEditMenuItemPlayClick(Sender: TObject);
    procedure EditMenuItemCopyToGeneratorClick(Sender: TObject);
    procedure EditMenuItemMovePlayerAndBoxesClick(
      Sender: TObject);
    procedure TaskQueuePopupMenuItemToggleSelectionAboveOrBelowItemClick(
      Sender: TObject);
    procedure SettingsMenuAlternatingOptimizationsEnabledClick(
      Sender: TObject);
    procedure SettingsMenuAlternatingOptimizationsRepeatClick(
      Sender: TObject);
    procedure SettingsMenuAlternatingOptimizationsTypeClick(
      Sender: TObject);
    procedure EditMenuItemReplaceClick(Sender: TObject);
    procedure CaptureFileMenuItemNewClick(Sender: TObject);
    procedure CaptureViewZoomItemClick(Sender: TObject);
    procedure CaptureEditMenuItemUndoClick(Sender: TObject);
    procedure CaptureEditMenuItemRedoClick(Sender: TObject);
    procedure CaptureFileMenuItemOpenClick(Sender: TObject);
    procedure CaptureEditMenuItemCutClick(Sender: TObject);
    procedure CaptureEditMenuItemCopyClick(Sender: TObject);
    procedure CaptureEditMenuItemPasteClick(Sender: TObject);
    procedure CaptureEditMenuItemSelectAllClick(Sender: TObject);
    procedure CaptureViewMenuItemShowColorQuantizationClick(Sender: TObject);
    procedure CaptureViewMenuItemShowImageAnalysisResultsClick(
      Sender: TObject);
    procedure CaptureSettingsMenuItemSnapToNearbyEdgesClick(Sender: TObject);
    procedure CaptureSettingsMenuItemUseAutomaticCompletionClick(
      Sender: TObject);
    procedure CaptureSettingsSkinExportFormatItemClick(Sender: TObject);
    procedure CaptureSettingsSkinExportFormatObjectBackgroundItemClick(
      Sender: TObject);
    procedure CaptureSettingsMenuItemGridColorClick(Sender: TObject);
    procedure CaptureSettingsColorQuantizationColorsItemClick(
      Sender: TObject);
    procedure CaptureSettingsMenuItemImageAnalysisClick(Sender: TObject);
    procedure CaptureFileMenuItemSaveClick(Sender: TObject);
    procedure CaptureFileMenuItemSaveAsClick(Sender: TObject);
    procedure PreviousStepButtonClick(Sender: TObject);
    procedure NextStepButtonClick(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
    procedure SpinEditExit(Sender: TObject);
    procedure CaptureImage1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CaptureImage1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CaptureImage1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CaptureEditMenuItemDrawingToolClick(Sender: TObject);
    procedure WallCapCheckBoxClick(Sender: TObject);
    procedure CaptureToolButtonPlayClick(Sender: TObject);
    procedure CaptureEditCaptureItemClick(Sender: TObject);
    procedure SkinBitBtnClick(Sender: TObject);
    procedure CapturePopupMenuPopup(Sender: TObject);
    procedure CaptureEditMenuItemMatchingSkinRecentSkinsClearRecentSkinsClick(Sender: TObject);
    procedure CaptureEditMenuItemMatchingSkinLoadSkinClick(Sender: TObject);
    procedure CaptureFileMenuItemNewSkinClick(Sender: TObject);
    procedure CaptureSettingsMenuItemDefaultSkinClick(Sender: TObject);
    procedure CaptureSettingsMenuItemSkinClick(Sender: TObject);
    procedure CaptureMenuEditClick(Sender: TObject);
    procedure CaptureEditMenuItemEditLevelClick(Sender: TObject);
    procedure CaptureEditMenuItemMatchingSkinClick(Sender: TObject);
    procedure CaptureSettingsMenuItemKeepAuthorAndDesignerNamesClick(
      Sender: TObject);
    procedure PluginEditMenuItemResetOptimizationMethodClick(
      Sender: TObject);
    procedure SettingsMenuAlternatingOptimizationsRepetitionSettingsClick(
      Sender: TObject);
    procedure PluginEditMenuItemCopyLevelCreatedFromSolutionSliceClick(
      Sender: TObject);
    procedure ImageBoardMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function  ToggleSelectedArea(const Rect__:TRect):Boolean;
    procedure SettingsMenuAlternatingOptimizationsTriggerTypeClick(
      Sender: TObject);
    procedure PluginEditMenuItemRepeatSolutionSlicingClick(
      Sender: TObject);
    procedure PluginEditMenuItemSetOptimizationIntervalClick(
      Sender: TObject);
    procedure PluginEditMenuItemClearBoardAreaSelectedForOptimizationClick(
      Sender: TObject);
    procedure CaptureSettingsColorQuantizationIgnoreSmallColorDifferencesClick(
      Sender: TObject);
    procedure EditToolButtonNewLevelWithWallsClick(Sender: TObject);
    procedure ToolButtonNewLevelWithWallsWidthAndHeightClick(Sender: TObject);
    procedure EditPopupMenuNewLevelWithWallsWidthAndHeightPopup(
      Sender: TObject);
    procedure ToolButtonNewLevelWithWallsWidthAndHeightMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure EditPopupMenuNewLevelWithWallsWidthAndHeightItemClick(Sender: TObject);
    procedure CaptureViewMenuItemLoadImageFromUpscaledViewOfImageClick(
      Sender: TObject);
  protected
    { Protected declarations }
    procedure ApplicationOnActivate(Sender: TObject);
    procedure ApplicationOnDeactivate(Sender: TObject);
    procedure ApplicationOnHint(Sender: TObject);
    procedure ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
    function  GetReplaySpeedMovesPerSecond:Integer;
    procedure OnGeneratorRefresh(var Msg: TMessage); Message Misc_.MSG_GENERATOR_REFRESH;
    procedure OnOptimizerRefresh(var Msg: TMessage); Message Misc_.MSG_OPTIMIZER_REFRESH;
    procedure OnSolverRefresh(var Msg: TMessage); Message Misc_.MSG_SOLVER_REFRESH;
    procedure OnStatus(var Msg: TMessage); Message Misc_.MSG_STATUS;
    procedure OnTest(var Msg: TMessage); Message Misc_.MSG_TEST;
    {$IFNDEF YASC}
      function  OpenDialog(Sender: TObject):Boolean;
    {$ENDIF}
    function  SaveLevelToFile(FileName__:String):Boolean;
    procedure SetReplaySpeedMovesPerSecond(ReplaySpeedMovesPerSecond__:Integer);
  private
    { Private declarations }
    CanEscape:Boolean;
    DefaultFormHeight:Integer;
    DefaultFormWidth:Integer;
    fAlternatingOptimizations:TAlternatingOptimizations;
    fInitialized:Boolean;
    fSortMetric:TGameMetricsExtended;
    IgnoreKeyUp:Boolean;
    IgnoreMouseUp:Boolean;
    PluginGroupBoxesMinimumHeight:Integer;
    ReplaySpeedMilliSecondsPerMove:Integer;
    SokoFile:TSokoFile;

    function  AddColRowToBoard(Col__,Row__:Integer):Boolean;
//  procedure AnimateButtonMouseClick(Button__:TToolButton);
    function  BoardDimensionsAsText(ColCount__,RowCount__,BoxCount__,GoalCount__:Integer; BoardDimensionsAsText__:TBoardDimensionsAsText):String;
    function  CalculateGameInformation(TopLeftJustify__:Boolean):Boolean;
    procedure CellToPos(Col__,Row__:Integer; var X__,Y__:Integer); // 0-based col,row
    function  CellValue(Col__,Row__:Integer):Integer; // 0-based columns, rows
    procedure CleanSolverTaskQueue;
    function  ClickToFocusHighlightedRow(DoIt__:Boolean):Boolean;
    function  CloseEditors:Boolean;
    function  CloseEditorSelection(Commit__,AutoSize__:Boolean):Boolean;
    function  CloseLevel(Sender: TObject):Boolean;
    function  CopyLevelToClipboard(OppositeFillFloorsSetting__,RunLengthEncoding__:Boolean):Boolean;
    function  CutSelectionToEditorClipboard:Boolean;
    procedure EditImage1MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure EditImage1MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    function  ResizeImage(Image__:TImage):Boolean;
    function  EraseGameBoardSquare(Col__,Row__:Integer):Integer; // 1-based columns and rows; returns the pieces on this square before deletion
    procedure FillOptimizationComboBox;
    procedure Finalize;
    procedure HideCursor(UpdateScreenCursor__:Boolean);
    function  InitializeEditorHistory:Boolean;
    function  InitializeGameViewer(Center__:Boolean):Boolean;
    function  LevelGroupBoxMinimumWidth:Integer;
{$IFNDEF YASC}
    function  LoadLevelFromFile(var FileName__:String):Boolean;
{$ENDIF}
    function  LoadLevelFromEditor(ClearReplayGame__,Refresh__:Boolean):Boolean;
    function  MakeNewLevel(Sender: TObject):Boolean;
    function  MakeNewLevelFileName:String;
    function  MakeUniqueLevelFileName(const Path,FileNameStub:String; var FileName:String):Boolean;
    function  OpenPluginFromList(Plugin__:TPlugin; Index__:Integer):Boolean;
    function  PluginLevelGroupBoxMinimumWidth:Integer;
    function  PluginForCurrentTabSheet:TPlugin;
    function  PluginGroupBoxMinimumWidth:Integer;
    procedure MouseToCell(X__,Y__:Integer; var Col__,Row__:Integer);
    function  Replay(ReplayOptions__:TReplayOptions; Position__:Integer):Boolean;
    function  ResizePluginGroupBoxes(PluginLevelGroupBoxWidth__,PluginGroupBoxWidth__,LevelGroupBoxWidth__,PluginGroupBoxesHeight__:Integer; ShowBoard__:Boolean):Boolean;
    function  Save(Sender: TObject):Boolean;
    function  ScrollEditorCursorInView:Boolean;
    procedure SetDefaultFormSize;
    procedure SetDefaultStringGridColumnWidths;
    procedure SetCursor(Cursor__:TEditorCursorType; CursorRect__:TRect);
    procedure SetDrawingToolHint;
    function  SetGameBoardSquare(Col__,Row__,Value__:Integer):Integer; // 1-based columns and rows; returns the pieces on this square before updating
    procedure ShowGame(ShowStatus__:Boolean);
    procedure ShowGameBackground;
    procedure ShowSquares;
    procedure ShowSquare(Col__,Row__:Integer); // 0-based cell-coordinate columns and rows
    procedure ShowStatusEditor;
    procedure ShowStatusGenerator;
    procedure ShowStatusOptimizer;
    procedure ShowStatusPlugin(Plugin__:TPlugin; Row__:Integer);
    procedure ShowStatusSolver;
    function  SolveLevelsGroupBoxMinimumHeight:Integer;
    function  TextToBoard(const Text__:String; var BoardWidth__,BoardHeight__:Integer; var PlayerPos__:TPoint; var Board__:TBoard):Boolean; // 1-based columns and rows
    function  UpdateView:Boolean;
    function  UndoRedoMove(Redo__:Boolean):Boolean;
    procedure ShowReplaySpeed;
    procedure CaptureMenuItemLoadSkinFromHistoryClick(Sender: TObject);
    function  EnterBrowseMode: Boolean;
    procedure BrowseMouseMove(X, Y: Integer);
    procedure ShowOptimizationRange(CursorPosition:Integer);
    function  CreateLevelFromSolutionSlice(var Level__: TLevel): Boolean;
  protected
    AscendingOrDescendingSortingOrderSign:Integer;

    procedure SetAlternatingOptimizations(AlternatingOptimizations__:TAlternatingOptimizations);
    function  GetStatusText:String;
    procedure SetSortMetric(Metric__:TGameMetricsExtended);
    procedure SetStatusText(const Text__:String);
  public
    { Public declarations }
    ButtonsFileName:String;
    CollectionNameInTitleLine:Boolean;
    DeleteFilesFilterIndex:Integer;
    Editor:TEditor;
    FormResizeCount:Integer;
    Game:TGame;
    GameViewer:TToolsGameViewer;
    GeneratorTaskQueue:TTaskQueue;
    OptimizerTaskQueue:TOptimizerTaskQueue;
    Modified:Boolean;
    NewLevelCount:Integer;
    ReplaySpeedTrackBarBackgroundColor,ReplaySpeedTrackBarSliderColor,ReplaySpeedTrackBarFontColor,ReplaySpeedTrackBarShadowColor:TColor;
    PluginLevelInfo:TPluginLevelInfo;
    SettingsModified:Boolean;
    SolverTaskQueue:TTaskQueue;
    StringsDB:TStringsDB;

    function  BoardToText(BoardWidth__,BoardHeight__:Integer; const Board__:TBoard; const Rect__:TRect; var Text__:String; var NonFloorCellCount__:Integer):Boolean; // 1-based columns and rows
    function  CellRectToGameBoardRect(const Rect__:TRect):TRect; // input: 0-based columns, rows; output: 1-based columns, rows
    function  CheckSnapshots(Game__,TestGame__:TSokoGame; AddHistoryOnly__:Boolean; var SolutionCount__:Integer):Boolean;
    procedure ClearPluginReplayInfo;
    procedure HideCellCursor;
    function  HideEditors:Boolean;
    function  InitializeTask(Sender__:TObject; TabSheet__:TTabSheet; ToolFlags__:TToolFlagSet):Boolean;
    function  IsALegalLevel(ShowMessage__,CalculateDimensions__:Boolean; var ErrorText__:String):Boolean;
    function  IsAnEmptyRect(const Rect__:TRect):Boolean;
    function  IsToolsFormPluginLevel(BoardWidth__,BoardHeight__:Integer; const BoardAsText__,LevelName__:String):Boolean;
    procedure LeaveBrowseMode(RestorePosition:Boolean);
    function  LoadDefaultSkin:Boolean;
    function  LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    function  LoadSkin(const FileName__:String; BitMap__:TBitMap; Count__:Integer; Antialiasing__:TAntiAliasing; Masked__:Boolean; MaskColor__:TColor; MaskBitMapPct__:Integer):Boolean;
    procedure MakeAllColumnsFullyVisible;
    function  MinHeight:Integer;
    function  MinWidth:Integer;
    procedure OnFontChange;
    procedure PluginSettingsWindowOrAboutFactBox(Sender: TObject);
    function  RectPlusOffset(const Rect__:TRect; OffsetX__,OffsetY__:Integer):TRect;
    function  SaveDefaultButtonsToFile(var DefaultButtonsFileName__:String):Boolean;
    function  SaveDialog(Sender: TObject):Boolean;
    function  SaveLevel(Sender: TObject):Boolean;
    function  SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
    procedure SetMessageHandlers;
    procedure ShowOptimizationComboBox(Row__:Integer);
    procedure ShowStatus;
    procedure ShowTitle(const SubSection,FileName:String);

    property  Initialized:Boolean read fInitialized write fInitialized;
    property  AlternatingOptimizations:TAlternatingOptimizations read fAlternatingOptimizations write SetAlternatingOptimizations;
    property  ReplaySpeedMovesPerSecond:Integer read GetReplaySpeedMovesPerSecond write SetReplaySpeedMovesPerSecond;
    property  SortMetric:TGameMetricsExtended read fSortMetric write SetSortMetric;
    property  StatusText:String read GetStatusText write SetStatusText;
  end;

  function  BitMapToImageList(BitMap__,DefaultBitMap__:TBitMap; ImageList__:TImageList):Boolean;
  function  GrabHandleToCursor(GrabHandle:TGrabHandle):TCursor;
  function  GrabHandleResizeRect(var GrabHandle:TGrabHandle; X,Y:Integer; var ControlPoint:TPoint; var R:TRect):Integer;
  function  ImageListToBitMap(ImageList__:TImageList; var BitMap__:TBitMap):Boolean;
  function  PointToGrabHandle(const P:TPoint; const Rect:TRect; Delta:Integer):TGrabHandle;
    
var
  ToolsForm: TToolsForm = nil;

function ToolsSkinPath:String;

implementation

{$R *.DFM}

{$IFNDEF YASC}
  {$R WindowsXP.RES}
{$ENDIF}

uses Registry,Clipbrd,
     Text_,Pack_,BitMap_,LSView_
     {$IFDEF YASC}
       ,MView_,Snapshots_,Open1_,Skins_,GView_,Options_,Main_, MPlayer2_,
  Capture_
     {$ENDIF}
     ;

// constants
const
  BUTTON_MOUSE_CLICK_ANIMATION_MS        = 100; // delay in milli-seconds
  CURSOR_NAMES                           : array[TEditorCursorType] of String = ('Cell Cursor','Eraser Cursor','Selection Cursor'); // don't localize
//COLOR_DARK_BLUE                        = TColor($200000); {BGR, not RGB}
  COLOR_FADED_RED                        = TColor($2A01DF); {BGR, not RGB}
  DEFAULT_FRAME_COLOR                    = clDkGray;
  DEFAULT_FRAME_SHADOW_COLOR             = COLOR_DARK_BLUE;
  DEFAULT_SQUARE_CURSOR_PEN_COLOR        = clYellow;
  DEFAULT_SQUARE_CURSOR_PEN_WIDTH        = 1;
  DEFAULT_SQUARE_CURSOR_SHADOW_COLOR     = {clNavy;} COLOR_DARK_BLUE;
  DEFAULT_SQUARE_CURSOR_SIZE             = 8;
  DEFAULT_ERASE_CURSOR_PEN_COLOR         = COLOR_FADED_RED;
  DEFAULT_ERASE_CURSOR_SHADOW_COLOR      = COLOR_DARK_BLUE;
  DEFAULT_REPLAY_SPEED_BACKGROUND_COLOR  = TColor($00C000); {BGR, not RGB}
  DEFAULT_REPLAY_SPEED_COLOR             = TColor($008000); {BGR, not RGB}
  DEFAULT_REPLAY_SPEED_FONT_COLOR        = clWhite;
  DEFAULT_REPLAY_SPEED_SHADOW_COLOR      = clDkGray;
//DEFAULT_SELECTION_CURSOR_PEN_COLOR     = clWhite;
  DEFAULT_SELECTION_CURSOR_PEN_WIDTH     = 1;
//DEFAULT_SELECTION_CURSOR_SHADOW_COLOR  = {clBlack;} COLOR_DARK_BLUE;
  DRAWING_TOOL_CURSOR_PICTURE_SIZE       = 10; // pixels
  DRAWING_TOOL_TO_ITEMS                  : array[TDrawingTool] of Integer
                                         = (0,WALL,BOX,GOAL,PLAYER,FLOOR,0,0);
  EDITOR_GUTTER_SQUARE_COUNT             = 1; // extra squares around the currently used board
  EDITOR_HISTORY_FILE_HEADER_ITEM_SIZE   = SizeOf(TEditorHistoryFileHeader) div SizeOf(Integer); // must match 'TEditorHistoryFileHeader', 'ReadFileHeader', and 'WriteFileHeader'
  EDITOR_HISTORY_MAGIC_ID_1              = Ord('s')+(Ord('o') shl 8)+(Ord('k') shl 16)+(Ord(SPACE) shl 24);
  EDITOR_HISTORY_MAGIC_ID_2              = Ord('e')+(Ord('d') shl 8)+(Ord('i') shl 16)+(Ord('t'  ) shl 24);
  EDITOR_HISTORY_ACTION_NAMES            : array[TEditorHistoryAction] of String
                                         = ('Board','Box','Erase','Goal','Height','Player','Mirror','Rotate','Square','Move','Wall','Width');
  EDITOR_INIFILE_SECTION                 = 'Editor'; // don't localize
  ERASE_PICTURE_INDEX                    = 16; // 'EditImage1' index for the 'Erase' glyph
  FORMAT_BOARD_DIMENSIONS                = '[%d x %d - %d/%d]'; // '[Columns x Rows - Boxes/Goals]' or '[Rows x Columns - Boxes/Goals]'
  IMAGE_INDEX_PAUSE                      = 24;
  IMAGE_INDEX_REPLAY                     = 23;
  IMAGE_LIST_BACKGROUND_COLOR            = clWhite;
  MAX_EDITOR_HISTORY_ITEMS               = (High(Integer) div SizeOf(Integer))-EDITOR_HISTORY_FILE_HEADER_ITEM_SIZE-2;
  MAX_EDITOR_SQUARE_SIZE                 = 30;
  MAX_ITEMS_ON_PLUGIN_TASK_QUEUE         = 10000;
  MIN_BORDER_SIZE                        = 8;
  MIN_COL_WIDTH  {board cells}           = 2;//6;//14;//3*MAX_CURSOR_PEN_WIDTH; // must be even and must be big enough to ensure that items on the board are recognizable
  MIN_ROW_HEIGHT {board cells}           = MIN_COL_WIDTH; // must be even and must be big enough to ensure that items on the board are recognizable
  OPTIMIZATION_RANGE_MARGIN              = 4;  // pixels
  SIZING_RECT_WIDTH                      = 6;
  SUB_INTERVAL_OVERLAP_PCT               = 10; // subinterval overlap percent for "partition rest of solution into subintervals" optimization
  TASK_QUEUE_STATISTICS_HEADER_LINES_COUNT
                                         = 6;  // must match the actual number of header lines; see 'TTaskQueue.AddStatistics()'
  TASK_QUEUE_STATISTICS_LINE_LENGTH      = 86;
  TILE_INDEX_ERASE                       = 6;
  TOOLS_INIFILE_SECTION                  = 'ToolsForm';   // don't localize
  TOOLS_SKIN_TILE_COUNT                  = 7;

procedure CalculateGameInformation(Game__:TSokoGame);
var i,Col,Row:Integer;
begin
  with Game__ do begin
    PlayerPos.X:=0; PlayerPos.Y:=0; BoxCount:=0; GoalCount:=0;

    for Col:=1 to BoardWidth do
        for Row:=1 to BoardHeight do begin
            i:=Board[Col,Row];
            if (i and PLAYER)<>0 then
               if PlayerPos.X=0 then begin
                  PlayerPos.X:=Col; PlayerPos.Y:=Row;
                  end
               else Dec(Board[Col,Row],PLAYER);
            if (i and BOX )<>0 then Inc(BoxCount );
            if (i and GOAL)<>0 then Inc(GoalCount);
            end;
    end;
end;

function  GrabHandleToCursor(GrabHandle:TGrabHandle):TCursor;
begin
  case GrabHandle of
    ghTopLeft, ghBottomRight: Result := crSizeNWSE;
    ghTop, ghBottom         : Result := crSizeNS;
    ghTopRight, ghBottomLeft: Result := crSizeNESW;
    ghRight, ghLeft         : Result := crSizeWE;
    else                      Result := ToolsForm.EditImage1.Cursor;
  end; // case
end;

function  GrabHandleResizeRect(var GrabHandle:TGrabHandle; X,Y:Integer; var ControlPoint:TPoint; var R:TRect):Integer;
const HorizontallyOppositeGrabHandle : array[TGrabHandle] of TGrabHandle =
        (ghNull,ghTopLeft    ,ghLeft ,ghBottomLeft,ghBottom,
                ghBottomRight,ghRight,ghTopRight  ,ghTop);
      VerticallyOppositeGrabHandle  : array[TGrabHandle] of TGrabHandle =
        (ghNull,ghBottomRight,ghRight,ghTopRight ,ghTop,
                ghTopLeft    ,ghLeft,ghBottomLeft,ghBottom);

var DX, DY : Integer;

  procedure SwapInteger(var A, B: Integer);
  var Tmp: Integer;
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

begin // GrabResize
  DX :=  X - ControlPoint.X;
  DY :=  Y - ControlPoint.Y;
  ControlPoint.X := X;
  ControlPoint.Y := Y;

  case GrabHandle of
    ghTopLeft:     R := Rect(R.Left+DX, R.Top+DY, R.Right, R.Bottom);
    ghTop:         Inc(R.Top,DY);
    ghTopRight:    R := Rect(R.Left, R.Top+DY, R.Right+DX, R.Bottom);
    ghRight:       Inc(R.Right,DX);
    ghBottomRight: R := Rect(R.Left, R.Top, R.Right+DX, R.Bottom+DY);
    ghBottom:      Inc(R.Bottom,DY);
    ghBottomLeft:  R := Rect(R.Left+DX, R.Top, R.Right, R.Bottom+DY);
    ghLeft:        Inc(R.Left,DX);
  end; // case
  with R do
    begin if Top  > Bottom then begin SwapInteger(Top, Bottom);
                                      GrabHandle:=HorizontallyOppositeGrabHandle[GrabHandle];
                                end;
          if Left > Right  then begin SwapInteger(Left, Right);
                                      GrabHandle:=VerticallyOppositeGrabHandle[GrabHandle];
                                end;
    end;

  Result := Abs(DX) + Abs(DY);
end;

function  PointToGrabHandle(const P:TPoint; const Rect:TRect; Delta:Integer):TGrabHandle;
begin
  with Rect do with P do
    if //(X>Left-Delta) and (X<Right+Delta)
       //and
       //(Y>Top -Delta) and (Y<Bottom+Delta)
       (X>Left) and (X<Right ) and
       (Y>Top ) and (Y<Bottom) // only accept points inside the rectangle
       then
       if        Y<Top+Delta then
                 if        X<Left +Delta then Result:=ghTopLeft
                 else if   X>Right-Delta then Result:=ghTopRight
                      else Result:=ghTop
       else if   Y>Bottom-Delta then
                 if        X<Left +Delta then Result:=ghBottomLeft
                 else if   X>Right-Delta then Result:=ghBottomRight
                      else Result:=ghBottom
            else if        X<Left +Delta then Result:=ghLeft
                 else if   X>Right-Delta then Result:=ghRight
                      else Result:=ghNull
    else Result:=ghNull;
end;

function  ToolsSkinPath:String;
begin
  Result:=SkinsPath;
  if not DirectoryExists(Result) then MkDir(Result);
  Result:=StrWithTrailingPathDelimiter(Result)+ToolsWindowSkinsText;
  if not DirectoryExists(Result) then MkDir(Result);
  if not DirectoryExists(Result) then Result:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
  if not DirectoryExists(Result) then Result:=ExtractFilePath(Application.ExeName);
  Result:=StrWithoutTrailingPathDelimiter(Result);
end;

function  MetricTypesToOptimizationFlags(PrimaryMetric__,SecondaryMetric__:TGameMetrics):Integer;
begin
  Result:=PRIMARY_METRICS_FLAGS[PrimaryMetric__];
  if PrimaryMetric__<>SecondaryMetric__ then
     Inc(Result,SECONDARY_METRICS_FLAGS[SecondaryMetric__]);
end;

function  TToolsFormSprite.SaveBackground(const Rect__:TRect; IsGoal__:Boolean):Boolean;
var R1:TRect;
begin // the background for the sprite can be created from the background image and the floor tile
  with LevelSetForm.GameViewer do begin
    R1:=Rect(0,0,BoardImage.Picture.BitMap.Width,BoardImage.Picture.BitMap.Height);
    BackRect:=Rect__;
    Result:=ClipRect(BackRect,R1) and BackgroundInitialized and SkinInitialized and Assigned(Self.BackBitMap);
    if Result then begin
       if BackgroundInitialized and
          BackgroundPict.Visible then begin
          BackBitMap.Canvas.CopyRect(PictureSizeRect,BackgroundPict.BitMap.Canvas,BackRect);
          end
       else with BackBitMap do with Canvas do begin
          Brush.Style:=bsSolid; Brush.Color:=BackgroundPict.Color;
          FillRect(PictureSizeRect);
          end;
       if FloorTilesVisible then
          SkinPict.DrawRect(BackRect.Left-Rect__.Left,
                            BackRect.Top -Rect__.Top,
                            SkinPict.FrameRect(TILE_INDEX_FLOOR),
                            BackBitMap.Canvas);
       if IsGoal__ then
          SkinPict.DrawRect(BackRect.Left-Rect__.Left,
                            BackRect.Top -Rect__.Top,
                            SkinPict.FrameRect(TILE_INDEX_GOAL),
                            BackBitMap.Canvas);

       end;
    end;

end;

procedure TToolsForm.FormCreate(Sender: TObject);
//const ColorAlmostWhite=TColor($FFFEFF);
//var C,X,Y:Integer; B:TBitMap; Pict:TPict;
//var i:Integer; Pict:TPict;
var i,W:Integer; d:TDrawingTool;
    pcbii:TPluginCallBackInfoItem; gcbii:TGeneratorCallBackInfoItem; gsii:TGeneratorStatusInfoItem;
    osc:TOptimizeSolutionColumn; slc:TSolveLevelColumn; glc:TGenerateLevelColumn;
    s:String;
    ACheckBox:TCheckBox;
    {$IFNDEF YASC} IniFile:TIniFile;
    {$ENDIF}
begin
  OnFontChange;
  if biMinimize in BorderIcons then BorderIcons:=BorderIcons-[biMinimize]; // minimize: it may be impossible for the user to bring the application back on the screen again (a Microsoft Windows operating system bug when the general Windows text size settings is > 100%)    
  FillChar(GameViewer,SizeOf(GameViewer),0);
  Editor.FileName:=''; Editor.LastValidFileName:=''; Editor.Selection.BoardAsText:='';
  FillChar(Editor,SizeOf(Editor),0);
  Game:=nil; SokoFile:=nil; StringsDB:=nil; CanEscape:=True; IgnoreKeyUp:=False; IgnoreMouseUp:=False; NewLevelCount:=0; CollectionNameInTitleLine:=False;
  try    Game                     :=TGame.Create;
         GameViewer.BackgroundPict:=Pict_.TPict.Create;
         GameViewer.CursorPict    :=TPict.Create;
         GameViewer.SkinPict      :=TPict.Create;
         Editor.History           :=TEditorHistory.Create(Game,EditMenuItemRefreshClick,Tools_.CalculateGameInformation);
         StringsDB                :=TStringsDB.Create;
         OptimizerTaskQueue       :=TOptimizerTaskQueue.Create(nil,OptimizeSolutionsStringGrid,OptimizerTaskQueueItemsCheckBox,TaskQueuePopupMenu);
         SolverTaskQueue          :=TTaskQueue         .Create(nil,SolveLevelsStringGrid,SolverTaskQueueItemsCheckBox,TaskQueuePopupMenu);
         GeneratorTaskQueue       :=TTaskQueue         .Create(nil,GenerateLevelsStringGrid,GeneratorTaskQueueItemsCheckBox,GeneratorTaskQueuePopupMenu);

         {$IFNDEF YASC}
           LevelSetForm:=TLevelSetForm.Create(Self);
           SokoFile:=TSokoFile.Create;
           Game.SokoFile:=SokoFile;
         {$ENDIF}
  except on E:Exception do begin
            Finalize;
            raise;
            end;
  end;

  StatusBar1.Font.Assign(Self.Font);
  s:=IntToStr(MAX_MOVES);
  for i:=1 to Length(s) do s[i]:='9';
  i:=Canvas.TextWidth(s+SLASH+s+'MQZ');
  with StatusBar1.Panels[0] do
    if i>Width then Width:=i; // ensure there is room enough in the status field for the number of moves formatted as '999.../999...'
  StatusBar1.Tag := StatusBar1.Panels[0].Width; // store the with of panels[0]

  {$IFNDEF YASC}
    SetMessageHandlers;
  {$ENDIF}

//EditToolButtonInternalClipboard.Visible:=False;

  try
         // the Delphi documentation states:
         // "if 'Flat' is set to True, TToolBar requires version 4.70 or
         // later of COMCTL32.DLL at both design time and runtime."

         // whether 'try...except helps at all hasn't been tested
         // but it's worth a try

         EditToolBarTop.Flat:=True;
         EditToolBarLeft.Flat:=True;
         EditToolBarRight.Flat:=True;
         PluginToolBar.Flat:=True;
         CaptureToolBarTop.Flat:=True;

  except on E:Exception do;
  end;

  Left:=Max(0,(Screen.Width -Width ) div 2);
  Top :=Max(0,(Screen.Height-Height) div 2);

  OpenDialog1.Title:=Application.Title+SUB_TITLE_SEPARATOR+OpenDialog1.Title;
  SaveDialog1.Title:=Application.Title+SUB_TITLE_SEPARATOR+SaveDialog1.Title;
  OpenDialog1.Filter      :=OpenDialogFilterText;
  SaveDialog1.Filter      :=OpenDialog1.Filter;
  if SOKOBAN_FILE_NAME_EXT[1]=PERIOD then
     OpenDialog1.DefaultExt:=System.Copy(SOKOBAN_FILE_NAME_EXT,2,Pred(Length(SOKOBAN_FILE_NAME_EXT)));
  SaveDialog1.DefaultExt  :=OpenDialog1.DefaultExt;

  DefaultFormHeight:=Height; DefaultFormWidth:=Width;
  Modified:=False; SettingsModified:=False; fInitialized:=False; PluginLevelInfo.NewGamesCount:=0;
  DeleteFilesFilterIndex:=1;

  PanelToolTips.Visible:=False; PanelToolTips.Caption:=''; PanelToolTips.BringToFront;
  //with PanelToolTips do begin Color:=clLtGray; Font.Color:=clBlack; end;
  FormResizeCount:=0;

  ButtonsFileName:='';
  StatusLabel1.Caption:=''; StatusLabel2.Caption:=''; StatusLabel3.Caption:='';
  EditImageList1.BkColor:=IMAGE_LIST_BACKGROUND_COLOR;

  Editor.Cursor:=ctCell; Editor.MouseButtonDown:=False; Editor.SizeHandle:=ghNull;
  Editor.LastCursorPict:=dtNone; Editor.MouseWheelPresent:=Mouse.WheelPresent;
  Editor.DrawingToolCursorsEnabled:=True;
  Editor.DrawingToolsEnabled:=False;

  with Editor.Cursors[ctCell] do begin
    PenColor:=DEFAULT_SQUARE_CURSOR_PEN_COLOR;
    PenWidth:=DEFAULT_SQUARE_CURSOR_PEN_WIDTH;
    ShadowColor:=DEFAULT_SQUARE_CURSOR_SHADOW_COLOR;
    Size:=DEFAULT_SQUARE_CURSOR_SIZE;
    end;
  with Editor.Cursors[ctEraser] do begin
    //PenColor:=DEFAULT_ERASE_CURSOR_PEN_COLOR;
    PenColor:=DEFAULT_SQUARE_CURSOR_PEN_COLOR;
    PenWidth:=DEFAULT_SQUARE_CURSOR_PEN_WIDTH;
    //ShadowColor:=DEFAULT_ERASE_CURSOR_SHADOW_COLOR;
    ShadowColor:=DEFAULT_SQUARE_CURSOR_SHADOW_COLOR;
    Size:=DEFAULT_SQUARE_CURSOR_SIZE;
    end;
  with Editor.Cursors[ctSelection] do begin
    PenColor:=DEFAULT_SELECTION_CURSOR_PEN_COLOR;
    PenWidth:=DEFAULT_SELECTION_CURSOR_PEN_WIDTH;
    ShadowColor:=DEFAULT_SELECTION_CURSOR_SHADOW_COLOR;
    Size:=0;
    end;

  if Editor.MouseWheelPresent then with EditToolBarLeft do
     for i:=0 to Pred(ButtonCount) do with Buttons[i] do
         Hint:=Hint+HintMouseWheelChangesDrawingToolText;

  for d:=Low(d) to High(d) do
      if DRAWINGTOOL_CURSOR_NAME[d]<>'' then
         Screen.Cursors[Succ(Ord(d))]:=LoadCursor(hInstance,PChar(DRAWINGTOOL_CURSOR_NAME[d]));
  for d:=Low(d) to High(d) do
      if Screen.Cursors[Succ(Ord(d))]=0 then Screen.Cursors[Succ(Ord(d))]:=crDefault;

  EditMenuItemGrid.Checked:=False;
  EditMenuItemMovePlayerAndBoxes.Checked:=False;

  ReplaySpeedMilliSecondsPerMove:=DEFAULT_ANIMATE_REPLAY_MOVES_MS;
  ReplaySpeedTrackBarBackgroundColor:=DEFAULT_REPLAY_SPEED_BACKGROUND_COLOR;
  ReplaySpeedTrackBarSliderColor:=DEFAULT_REPLAY_SPEED_COLOR;
  ReplaySpeedTrackBarFontColor:=DEFAULT_REPLAY_SPEED_FONT_COLOR;
  ReplaySpeedTrackBarShadowColor:=DEFAULT_REPLAY_SPEED_SHADOW_COLOR;

//GameViewer.BackgroundPict.View:=ivFill;
//if GameViewer.BackgroundPict.LoadFromResource(BACKGROUND_RES_NAME,RC_JPG) then;
  GameViewer.FrameColor:=DEFAULT_FRAME_COLOR;
  GameViewer.FrameShadowColor:=DEFAULT_FRAME_SHADOW_COLOR;
  GameViewer.BoardDimensionsAsText:=bdColRow;
  with GameViewer.MouseSuperPosition do begin X:=0; Y:=0; end;
  GameViewer.BoardImage:=EditImage1;
  with GameViewer.BackgroundPict do begin
    AntiAliasing:=DEFAULT_BACKGROUND_ANTI_ALIASING;
    Color:=DEFAULT_PICTURE_COLOR[ptScreenBackground];
    View:=ivFill; Visible:=True;
    end;
  with GameViewer.SkinPict do begin
    AntiAliasing:=DEFAULT_OBJECT_ANTI_ALIASING; Masked:=True;
    MaskBitMapColor:=RGB_BLACK; MaskBitMapPct:=DEFAULT_MASK_BITMAP_PCT;
    end;
  with GameViewer do begin
    SquareSetSelectedSquaresColor    :=clWhite;
    SquareSetNotSelectedSquaresColor :=clBlack;
    SquareSetTransparencyPct         :=50;
    end;
  EditMenuItemFillWithWalls.Checked:=True; EditPopupMenuItemFillWithWalls.Checked:=True;
  EditMenuItemFillWithWallBorderOpaque.Tag:=Ord(False);
  EditMenuItemFillWithWallBorderTransparent.Tag:=Ord(False);
  EditMenuItemReplaceFindWalls.Checked:=True;
  EditMenuItemReplaceReplaceWithFloors.Checked:=True;
  CaptureSettingsMenuItemSnapToNearbyEdges.Checked := False;
  CaptureSettingsMenuItemKeepAuthorAndDesignerNames.Checked := False;

  {$IFNDEF YASC}
    try
      IniFile:=TIniFile.Create(ChangeFileExt(Application.ExeName,INI_FILE_EXT));
      try     LoadSettingsFromIniFile(IniFile);
              LevelSetForm.LoadSettingsFromIniFile(IniFile,INTERNAL_CLIPBOARD_INIFILE_SECTION);
      finally IniFile.Free;
      end;
    except on E:Exception do Error(E.Message,Application.Title);
    end;

    s:=Editor.FileName;

    EditToolButtonWall.Down:=True;
    EditMenuItemDrawingToolClick(EditMenuItemWall);
    EditMenuItemNewClick(nil);

    Editor.FileName:=s; // save the information from the ini-file
  {$ENDIF}

  for i:=0 to Pred(ComponentCount) do
      if      Components[i] is TPanel       then with Components[i] as TPanel       do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TLabel       then with Components[i] as TLabel       do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TButton      then with Components[i] as TButton      do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TBitBtn      then with Components[i] as TBitBtn      do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TToolButton  then with Components[i] as TToolButton  do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TRadioButton then with Components[i] as TRadioButton do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TComboBox    then with Components[i] as TComboBox    do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TGroupBox    then with Components[i] as TGroupBox    do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TListBox     then with Components[i] as TListBox     do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TMemo        then with Components[i] as TMemo        do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; ControlStyle := ControlStyle + [ csOpaque ]; end
      else if Components[i] is TEdit        then with Components[i] as TEdit        do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TImage       then with Components[i] as TImage       do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; ControlStyle := ControlStyle + [ csOpaque ]; end
      else if Components[i] is TScrollBox   then with Components[i] as TScrollBox   do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; ControlStyle := ControlStyle + [ csOpaque ]; end
      else if Components[i] is TStringGrid  then with Components[i] as TStringGrid  do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; ControlStyle := ControlStyle + [ csOpaque ]; end
      else if Components[i] is TStatusBar   then with Components[i] as TStatusBar   do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TToolBar     then with Components[i] as TToolBar     do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TPageControl then with Components[i] as TPageControl do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; ControlStyle := ControlStyle + [ csOpaque ]; end
      else if Components[i] is TTabSheet    then with Components[i] as TTabSheet    do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; ControlStyle := ControlStyle + [ csOpaque ]; end
//    else if Components[i] is TTrackBar    then with Components[i] as TTrackBar    do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end // 'TTrackBar' doesn't have 'OnMouseMove' and 'OnMouseUp', unfortunately
      else if Components[i] is TProgressBar then with Components[i] as TProgressBar do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TSpinEdit    then with Components[i] as TSpinEdit    do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove;    if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp;
                                                                                             if not Assigned(OnExit     ) then OnExit :=SpinEditExit;         if not Assigned(OnChange ) then OnChange := SpinEditChange;
                                                                                       end;
  PanelToolTips.OnMouseMove:=nil; // otherwise the tool tip panel may turn up blank when the mouse hovers over it

  SetLength(s,TAB_SHEET_SPACES); for i:=1 to Length(s) do s[i]:=SPACE;
  for i:=0 to Pred(PageControl1.PageCount) do with PageControl1.Pages[i] do
      Caption:=s+Caption+s; // increase the tabsheet captions so the user gets a larger clickable area

  PageControl1.ActivePage:=TabSheetEditor;

  with PluginLevelInfo do with BoardAsText do begin
    Board:=''; Height:=0; Width:=0; LevelName:=''; ErrorText:=''; IsALegalLevel:=False; NewGamesCount:=0; SelectedSquaresAsText:='';
    ClearPluginReplayInfo;
    end;

  if   Editor.MouseWheelPresent then
       PluginToolButtonReplay.Hint :=HintPluginReplayPauseText[False]+HintUseMouseWheelToStepText
  else PluginToolButtonReplay.Hint :=HintPluginReplayPauseText[False]+HintUse_INSERT_And_DELETE_ToStepText;

  PluginLevelMemo.Align:=alClient;
  PluginLevelStringGrid.Align:=alClient;
  OptimizeSolutionsStringGrid.Align:=alClient;
  SolveLevelsStringGrid.Align:=alClient;
  GenerateLevelsStringGrid.Align:=alClient;
  LevelMemo.Align:=alClient;
  GeneratorStatusStringGrid.Align:=alClient;
  with ImageBoard do begin Left:=0; Top:=0; Align:=alClient; end;
  with ImageReplaySpeed do begin Left:=0; Top:=0; Align:=alClient; Visible:=False; end;
  with BtnSolveLevel do BtnOptimizeGames.SetBounds(Left,Top,Width,Height);
  with BtnSolveLevel do BtnGenerateLevels.SetBounds(Left,Top,Width,Height);

  with PluginLevelGroupBox do Top:=Max(Top,PluginToolBar.Top+PluginToolBar.Height+4);
  with SolverGroupBox do Top:=PluginLevelGroupBox.Top;
  with OptimizerGroupBox do Top:=PluginLevelGroupBox.Top;
  with LevelGroupBox do Top:=PluginLevelGroupBox.Top;
  with GeneratorGroupBox do Top:=PluginLevelGroupBox.Top;
  LevelGroupBox.Width:=TabSheetSolver.ClientWidth-LevelGroupBox.Left-PluginLevelGroupBox.Left;
  PluginLevelGroupBox.Tag:=PluginLevelGroupBox.Width; // remember the original groupbox widths
  SolverGroupBox.Tag:=SolverGroupBox.Width;
  LevelGroupBox.Tag:=LevelGroupBox.Width;
  PluginGroupBoxesMinimumHeight:=0; // '0': minimum height not calculated yet
  ResizePluginGroupBoxes(PluginLevelGroupBox.Width-2,SolverGroupBox.Width+4,LevelGroupBox.Width-2,LevelGroupBox.Height,False); // '-2,+4,-2' : make small changes to ensure that positions and sizes for all the dependent components are updated

  W:=0;
  for gcbii:=Low(GeneratorCallBackInfoText) to High(GeneratorCallBackInfoText) do
      W:=Max(W,PluginLevelStringGrid.Canvas.TextWidth(GeneratorCallBackInfoText[gcbii]));
  for pcbii:=Low(PluginCallBackInfoText) to High(PluginCallBackInfoText) do
      W:=Max(W,PluginLevelStringGrid.Canvas.TextWidth(PluginCallBackInfoText[pcbii]));
  PluginLevelStringGrid.ColWidths[0]:=W+8;
  with PluginLevelStringGrid do begin
    LeftCol:=FixedCols;
    ColWidths[1]:=ClientWidth-ColCount*GridLineWidth-ColWidths[0];
    while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]); // for safety; this shouldn't happen
    end;

  W:=0;
  for gsii:=Low(GeneratorStatusInfoText) to High(GeneratorStatusInfoText) do with GeneratorStatusStringGrid do begin
      if   gsii<>gsiiMemoryFull then Cells[0,Ord(gsii)]:=GeneratorStatusInfoText[gsii]
      else Cells[0,Ord(gsii)]:='';
      W:=Max(W,Canvas.TextWidth(GeneratorStatusInfoText[gsii]));
      end;
  GeneratorStatusStringGrid.ColWidths[0]:=W+8;
  with GeneratorStatusStringGrid do begin
    LeftCol:=FixedCols;
    ColWidths[1]:=ClientWidth-ColCount*GridLineWidth-ColWidths[0];
    while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]); // for safety; this shouldn't happen
    end;

  PluginLevelStringGrid.Font.Color:=clBtnText; PluginLevelStringGrid.Color:=clBtnFace;
  GeneratorStatusStringGrid.Font.Color:=clBtnText; GeneratorStatusStringGrid.Color:=clBtnFace;
  OptimizeSolutionsStringGrid.Font.Color:=clBtnText; OptimizeSolutionsStringGrid.Color:=clBtnFace;
  SolveLevelsStringGrid.Font.Color:=clBtnText; SolveLevelsStringGrid.Color:=clBtnFace;
  GenerateLevelsStringGrid.Font.Color:=clBtnText; GenerateLevelsStringGrid.Color:=clBtnFace;
  SolverComboBox.Sorted:=False; // signals 'not initialized'
  SolverComboBox.Tag:=SolverComboBox.Width; // 'Tag' is used for storing minimum dropdown width
  //PanelBoard.Top:=OptimizeSolutionsGridPanel.Top;
  //LevelNamePanel.Top:=PanelBoard.Top;

  for osc:=Low(osc) to High(osc) do with OptimizeSolutionsStringGrid do Cells[Ord(osc),0]:=OptimizeSolutionsColumnHeadersText[osc];
  for slc:=Low(slc) to High(slc) do with SolveLevelsStringGrid       do Cells[Ord(slc),0]:=SolveLevelsColumnHeadersText      [slc];
  for glc:=Low(glc) to High(glc) do with GenerateLevelsStringGrid    do Cells[Ord(glc),0]:=GenerateLevelsColumnHeadersText   [glc];
  with PluginLevelStringGrid do DefaultRowHeight:=Self.Canvas.TextHeight(FONT_HEIGHT_TEST_STRING)+4;
  GeneratorStatusStringGrid.DefaultRowHeight:=PluginLevelStringGrid.DefaultRowHeight;

  with OptimizeSolutionsStringGrid do begin
    ScrollBars                                        :=ssVertical;
    //DefaultRowHeight                                :=ToolsForm.Canvas.TextHeight(FONT_HEIGHT_TEST_STRING)+4;
    //Font.Color                                      :=clBlack; Color:=COLOR_LEMONCHIFFON;
    DefaultRowHeight                                  :=Max(Max(PluginLevelStringGrid.DefaultRowHeight,OptimizerTaskQueueItemsCheckBox.Height+4),OptimizationComboBox.Height);

    SolveLevelsStringGrid.ScrollBars                  :=ssVertical;
    SolveLevelsStringGrid.DefaultRowHeight            :=DefaultRowHeight;

    GenerateLevelsStringGrid.ScrollBars               :=ssVertical;
    GenerateLevelsStringGrid.DefaultRowHeight         :=DefaultRowHeight;

    SetDefaultStringGridColumnWidths;
    end;
  with OptimizationComboBox do begin
    Parent:=OptimizerTaskQueue.StringGrid;
    Width :=OptimizerTaskQueue.StringGrid.ColWidths[Ord(oscOptimization)];
    Height:=OptimizerTaskQueue.StringGrid.DefaultRowHeight; // doesn't work
    OptimizerTaskQueue.StringGrid.DefaultRowHeight:=Height;
    Visible:=False; Tag:=0; Clear;
    end;
  OptimizerTaskQueue.InitializeItemsCheckBox; OptimizerTaskQueueItemsCheckBox.OnMouseDown:=Self.FormMouseDown;
  SolverTaskQueue   .InitializeItemsCheckBox; SolverTaskQueueItemsCheckBox   .OnMouseDown:=Self.FormMouseDown;
  GeneratorTaskQueue.InitializeItemsCheckBox; GeneratorTaskQueueItemsCheckBox.OnMouseDown:=Self.FormMouseDown;
  with SelectBtn do begin
    Height:=OptimizeSolutionsStringGrid.DefaultRowHeight-2*OptimizeSolutionsStringGrid.GridLineWidth;
    Width :=Height;
    Visible:=False;
    end;
  with PanelPluginLevelInfo do begin
    Height:=2*BevelWidth*(Ord(BevelInner<>bvNone)+Ord(BevelOuter<>bvNone))+(PluginLevelStringGrid.DefaultRowHeight+PluginLevelStringGrid.GridLineWidth)*PluginLevelStringGrid.RowCount-PluginLevelStringGrid.GridLineWidth;
    while PluginLevelStringGrid.VisibleRowCount<PluginLevelStringGrid.RowCount do Height:=Succ(Height); // for safety; this shouldn't happen
    PluginLevelGroupBox.Height:=Top+Height+Left;
    PluginGroupBoxesMinimumHeight:=PluginLevelGroupBox.Height;
    end;
  PanelGeneratorStatus.Top:=PanelPluginLevelInfo.Top;
  PanelGeneratorStatus.Height:=PanelPluginLevelInfo.Height;
  SolverGroupBox.Height:=PluginLevelGroupBox.Height;
  OptimizerGroupBox.Height:=PluginLevelGroupBox.Height;
  GeneratorGroupBox.Height:=PluginLevelGroupBox.Height;
  with CurrentSolverGroupBox do Height:=SolverGroupBox.ClientHeight-Top-Left;
  with BtnGeneralSolverSettings do begin
    Top:=SolverGroupBox.Height-Height-CurrentSolverGroupBox.Left;
    Width:=CurrentSolverGroupBox.Width;
    BtnGeneralOptimizerSettings.SetBounds(Left,Top,Width,Height);
    end;
  with CurrentSolverGroupBox do begin
       //Top:=PanelPluginLevelInfo.Top-6;
       Top:=BtnSolverBrowse.Top+BtnSolverBrowse.Height+2;
       Height:=BtnSolverAbout.Top+BtnSolverAbout.Height+Left;
       CurrentOptimizerGroupBox.SetBounds(Left,Top,Width,Height);
       end;
  LevelGroupBox.Height:=PluginLevelGroupBox.Height;
  with PanelBoard do Height:=LevelGroupBox.ClientHeight-Top-Left;
  with OptimizeSolutionsGroupBox do begin
    Height:=1;
    Top:=PluginLevelGroupBox.Top+PluginLevelGroupBox.Height+Left;
    Width:=TabSheetOptimizer.ClientHeight-2*Left;
    Height:=TabSheetOptimizer.ClientHeight-Top-Left;
    SolveLevelsGroupBox.Tag:=Height; // the first calculated height is also the minimum height; save it in 'SolveLevelsGroupBox.Tag'
    if SolveLevelsGroupBox.Height<Height then
       SolveLevelsGroupBox.Height:=Height; // so initializations in 'ResizePluginGroupBoxes' don't fail
    end;
  OptimizeSolutionsStringGrid.Col:=Ord(oscSelect);
  SolveLevelsStringGrid.Col:=Ord(slcSelect);
  GenerateLevelsStringGrid.Col:=Ord(glcSelect);
  ImageBoard.Tag:=0;
  ImageReplaySpeed.Tag:=0;
  PageControl1.Tag:=0;
  if OptimizeSolutionsStringGrid.FixedRows<>1 then raise Exception.Create(TEXT_TASK_FAILED); // levels are numbered 1..n and row numbers must match
  if SolveLevelsStringGrid.FixedRows<>1 then raise Exception.Create(TEXT_TASK_FAILED); // levels are numbered 1..n and row numbers must match
  if GenerateLevelsStringGrid.FixedRows<>1 then raise Exception.Create(TEXT_TASK_FAILED); // levels are numbered 1..n and row numbers must match
  EditMenuItemNormalizeBoard.Hint := HintNormalizeBoardText;
  EditMenuItemNormalizeBoardMakeRectangularBoard.Hint := HintNormalizeBoardMakeRectangularBoardText;
  PluginToolButtonOpenPrior.Hint:=PluginMenuItemOpenPrior.Hint;
  PluginToolButtonOpenNext .Hint:=PluginMenuItemOpenNext .Hint;
  PluginEditMenuItemDeleteSolutions.Hint:=HintRemoveSelectedSolutionsFromListText;
  PluginEditMenuItemDeleteLevels.Hint:=HintRemoveSelectedLevelsFromListText;
  TaskQueuePopupMenuItemResetOptimizationMethod.Hint:=PluginEditMenuItemResetOptimizationMethod.Hint;
  PluginMenuItemSort       .Hint:=PopupMenuItemSort      .Hint;
  PluginToolButtonGeneratorEdit.Hint:=GeneratorEditMenuItemEdit.Hint;
  PluginToolButtonGeneratorPlay.Hint:=GeneratorEditMenuItemPlay.Hint;
  TaskQueuePopupMenuItemCopyLevelCreatedFromSolutionSlice.Hint:=PluginEditMenuItemCopyLevelCreatedFromSolutionSlice.Hint;
  TaskQueuePopupMenuItemSelectSolutionSliceToBeOptimized.Hint:=PluginEditMenuItemSelectSolutionSliceToBeOptimized.Hint;
  TaskQueuePopupMenuItemSelectAreaToBeOptimized.Hint:=PluginEditMenuItemSelectAreaToBeOptimized.Hint;
  TaskQueuePopupMenuItemRepeatSolutionSlicing.Hint:=PluginEditMenuItemRepeatSolutionSlicing.Hint;
  TaskQueuePopupMenuItemSetOptimizationInterval.Hint:=PluginEditMenuItemSetOptimizationInterval.Hint;
  TaskQueuePopupMenuItemClearBoardAreaSelectedForOptimization.Hint:=PluginEditMenuItemClearBoardAreaSelectedForOptimization.Hint;
  GeneratorPopupMenuItemEdit.Hint:=GeneratorEditMenuItemEdit.Hint;
  GeneratorPopupMenuItemPlay.Hint:=GeneratorEditMenuItemPlay.Hint;
  EditToolButtonGenerator.Hint:=EditMenuItemCopyToGenerator.Hint;
  SortMetric:=gmMoves;
  GeneratorNamePanel.Caption:=MainForm.Generator.GeneratorNameAndVersion;

  CaptureViewMenuItemShowImageAnalysisResults.Checked := False;
  CaptureSettingsMenuItemSnapToNearbyEdges.Checked := True;

  SettingsMenuAlternatingOptimizationsEnabled.Checked:=False;
  SettingsMenuAlternatingOptimizationsEnabledClick(nil);
  AlternatingOptimizations:=aoPrimarySecondary;
  SettingsMenuAlternatingOptimizationsTriggerAtLeastOnce.Checked:=False;
  SettingsMenuAlternatingOptimizationsTriggerOnImprovements.Checked:=True;
  SettingsMenuAlternatingOptimizationsRepeat.Checked:=True;
  SettingsMenuAlternatingOptimizationsRepeatOnAnyMetrics.Checked:=True;
  SettingsMenuAlternatingOptimizationsRepeatOnMovesPushesBoxLines.Checked:=False;
  SettingsMenuAlternatingOptimizationsRepeatClick(nil);
  SettingsMenuAlternatingOptimizationsTriggerAtLeastOnce.Caption:=AlternatingOptimizationsTriggerTypeText[False];
  SettingsMenuAlternatingOptimizationsTriggerOnImprovements.Caption:=AlternatingOptimizationsTriggerTypeText[True];
  SettingsMenuAlternatingOptimizationsRepeatOnAnyMetrics.Caption:=AlternatingOptimizationsRepeatTypeText[False];
  SettingsMenuAlternatingOptimizationsRepeatOnMovesPushesBoxLines.Caption:=AlternatingOptimizationsRepeatTypeText[True];

  ToolButtonNewLevelWithWallsWidth .Tag:=((DEFAULT_RANDOM_BOARD_WIDTH +GENERATOR_BOARD_REGION_WIDTH -1) div GENERATOR_BOARD_REGION_WIDTH )*GENERATOR_BOARD_REGION_WIDTH; // store width and height in the tags
  ToolButtonNewLevelWithWallsHeight.Tag:=((DEFAULT_RANDOM_BOARD_HEIGHT+GENERATOR_BOARD_REGION_HEIGHT-1) div GENERATOR_BOARD_REGION_HEIGHT)*GENERATOR_BOARD_REGION_HEIGHT;

//  EditToolButtonSelect.Down:=True;
//  EditMenuItemDrawingToolClick(EditMenuItemSelect);

{
  C:=0;
  s:=ExtractFilePath(Application.ExeName)+STANDARD_EDIT_BUTTON_ICONS_FILENAME+'.'+GraphicExtension(TBitMap);
  B:=TBitMap.Create;
  B.LoadFromFile(s);
  for x:=0 to Pred(B.Width) do
      for y:=0 to Pred(B.Height) do
          if B.Canvas.Pixels[x,y]=clWhite then begin
             Inc(C);
             B.Canvas.Pixels[x,y]:=ColorAlmostWhite;
             end;
  B.SaveToFile(s);
  B.Free;
  Msg(IntToStr(C)+' white pixels','',MB_OK);
}
{
  Pict:=Pict_.TPict.Create;
  Pict.Antialiasing:=aaFilter;
  for i:=1 to 9 do
      if Pict.LoadFromFile('C:\Temp\YASC-00'+IntToStr(i)+'.bmp')=True then begin
         Pict.Resize(Pict.OrgBitMap.Width div 2,Pict.OrgBitMap.Height div 2);
         Pict.BitMap.SaveToFile('C:\Temp\YASC-00'+IntToStr(i)+'t.bmp');
         end;
  for i:=10 to 10 do
      if Pict.LoadFromFile('C:\Temp\YASC-0'+IntToStr(i)+'.bmp')=True then begin
         Pict.Resize(Pict.OrgBitMap.Width div 2,Pict.OrgBitMap.Height div 2);
         Pict.BitMap.SaveToFile('C:\Temp\YASC-0'+IntToStr(i)+'t.bmp');
         end;
  Pict.Free;
}
{
  C:=0;
  s:=ExtractFilePath(Application.ExeName)+'Editor Default Skin'+'.'+GraphicExtension(TBitMap);
  B:=TBitMap.Create;
  B.LoadFromFile(s);
  for x:=0 to Pred(B.Width) do
      for y:=0 to Pred(B.Height) do
          if B.Canvas.Pixels[x,y]=clBlack then begin
             Inc(C);
             B.Canvas.Pixels[x,y]:=clWhite;
             end;
  B.SaveToFile('t3.bmp');
  B.Free;
  Msg(IntToStr(C)+' white pixels','',MB_OK);
}
{
  s:='t3.bmp';
  Pict:=Pict_.TPict.Create;
  Pict.Antialiasing:=aaFilter;
  Pict.Masked:=True; Pict.MaskBitMapColor:=ColorToRGB(clWhite); Pict.MaskBitMapPct:=5;
  if Pict.LoadFromFile(s) and (Pict.OrgBitMap<>nil) then begin
     if Pict.ResizeFrames(30,30,TOOLS_SKIN_TILE_COUNT) then Pict.BitMap.SaveToFile('t4.bmp');
     end;
}
end;

procedure TToolsForm.FormDestroy(Sender: TObject);
{$IFNDEF YASC}
  var IniFile:TIniFile;
{$ENDIF}
begin
  if Editor.History<>nil then Editor.History.Close;

  {$IFNDEF YASC}
    try
      IniFile:=TIniFile.Create(ChangeFileExt(Application.ExeName,INI_FILE_EXT));
      try     SaveSettingsToIniFile(IniFile);
              LevelSetForm.SaveSettingsToIniFile(IniFile,INTERNAL_CLIPBOARD_INIFILE_SECTION);
      finally IniFile.Free;
      end;
    except on E:Exception do Error(E.Message,Application.Title);
    end;
  {$ENDIF}

  Finalize;
end;

procedure TToolsForm.Finalize;
begin
  if Game<>nil then begin Game.SokoFile:=nil; Game.Clear; end; // avoid that 'Game' tries to save current game

  with GameViewer do begin
    if CursorPict<>nil then CursorPict.OrgBitMap:=nil; {either 'GameViewer.SkinPict' or the application owns the tile-picture}
    if (SkinPict<>nil) and
       (SkinPict.OrgBitMap=EditImageDefaultSkin.Picture.BitMap) then
       SkinPict.OrgBitMap:=nil; {the default tile-set is owned by the application}
    end;

  if (Editor.History<>nil) and
     (Editor.History.FileName<>'') then DeleteFile(Editor.History.FileName);

  OptimizerTaskQueue.Free;        OptimizerTaskQueue       :=nil;
  SolverTaskQueue.Free;           SolverTaskQueue          :=nil;
  GeneratorTaskQueue.Free;        GeneratorTaskQueue       :=nil;  
  StringsDB.Free;                 StringsDB                :=nil;
  Editor.History.Free;            Editor.History           :=nil;
  GameViewer.BackgroundPict.Free; GameViewer.BackgroundPict:=nil;
  GameViewer.CursorPict.Free;     GameViewer.CursorPict    :=nil;
  GameViewer.SkinPict.Free;       GameViewer.SkinPict      :=nil;
  Game.Free;                      Game                     :=nil;

  {$IFNDEF YASC}
    SokoFile.Free;                SokoFile                 :=nil;
    LevelSetForm.Free;            LevelSetForm             :=nil;
  {$ENDIF}

  ToolsForm:=nil;
end;

procedure TToolsForm.ApplicationOnActivate(Sender: TObject);
begin
  if Initialized then begin
     if PageControl1.ActivePage=TabSheetEditor then begin
        if   Editor.MouseButtonDown then EditImage1MouseUp(nil,mbLeft,[],0,0)
        else SetDrawingToolHint;
        end;
     if Assigned(CaptureForm) then CaptureForm.ApplicationOnActivate(Sender);
     ShowStatus;
     end;
  IgnoreKeyUp:=True; IgnoreMouseUp:=True;
  Editor.MouseButtonDown:=False;
  if MainForm.Music<>nil then MainForm.Music.OnActivateApplication;
end;

procedure TToolsForm.ApplicationOnDeactivate(Sender: TObject);
begin
  if not Editor.Selection.Enabled then HideCursor(False);
  PanelToolTips.Visible:=False;
  IgnoreKeyUp:=True; IgnoreMouseUp:=True;
  if Assigned(CaptureForm) then CaptureForm.ApplicationOnDeactivate(Sender);
  if MainForm.Music<>nil then MainForm.Music.OnDeactivateApplication;
end;

procedure TToolsForm.FormActivate(Sender: TObject);
var i:Integer; s {$IFNDEF YASC},PackFileName {$ENDIF}:String;
begin
  SetMessageHandlers;
  ModalResult:=mrNone;
  IgnoreKeyUp:=True; IgnoreMouseUp:=True; // a 'KeyUp' or a 'MouseUp' event may come after a 'KeyDown' or 'MouseDown' on another form
  Editor.MouseButtonDown:=False;
  if Assigned(Game) then Game.IsBusy:=False;
  if Assigned(OpenForm) and Assigned(OpenForm.Game) then with OpenForm.Game do begin
     IsBusy:=False; IsReplaying:=False; IsBrowsing:=False;
     end;

  StatusText:='';

  if not Initialized then begin
     fInitialized:=True;

     if IsWindowsVistaOrNewerOperatingSystem then begin
        // kludge: dividers don't seem to work properly for flat toolbars on
        // Windows Vista and newer Windows versions;
        // dividers and even separators show up with a left-justified line;
        // as a work-around, change the dividers to separators; that way, the
        // erroneous line works as a divider; it doesn't look as good as a real
        // divider, but it will have to do;
        // the dilemma is that the toolbars must be flat, otherwise the
        // dropdown buttons don't show show up properly;
        with EditToolBarTop do
          for i:=0 to Pred(ButtonCount) do with Buttons[i] do
              if Style=tbsDivider then Style:=tbsSeparator;
        with PluginToolBar do
          for i:=0 to Pred(ButtonCount) do with Buttons[i] do
              if Style=tbsDivider then Style:=tbsSeparator;
        with CaptureToolBarTop do
          for i:=0 to Pred(ButtonCount) do with Buttons[i] do
              if Style=tbsDivider then Style:=tbsSeparator;
        end;

     if Sender<>nil then begin
        EditToolButtonWall.Down:=True;
        EditMenuItemDrawingToolClick(EditMenuItemWall);
        end;

     s:=GameViewer.BackgroundPict.FileName;
     if   s<>'' then
          if   FileExists(s) then
               SettingsMenuItemBackgroundClick(nil)
          else SettingsMenuItemDefaultBackgroundClick(nil)
     else if   GameViewer.BackgroundPict.LoadFromResource(BACKGROUND_RES_NAME,RC_JPG) then begin
               GameViewer.BackgroundPict.FileName:='';
               if GameViewer.BackgroundPict.OrgBitMap=nil then begin
                  if LevelSetForm<>nil then with LevelSetForm do with GameViewer do
                     if Assigned(BackgroundPict) then BackgroundPict.OrgBitMap:=nil;
                  GameViewer.BackgroundPict.MakeOrgBitMapFromPict;
                  end;
               end
          else begin GameViewer.BackgroundPict.Clear;
                     if LevelSetForm<>nil then with LevelSetForm do with GameViewer do
                        if Assigned(BackgroundPict) then BackgroundPict.OrgBitMap:=nil;
               end;

     s:=ButtonsFileName;
     if   (s<>'') and FileExists(s) then
          SettingsMenuItemButtonsClick(nil)
     else SettingsMenuItemDefaultButtonsClick(nil);

     s:=GameViewer.SkinPict.FileName;
     if   s<>'' then
          if   FileExists(s) then
               SettingsMenuItemSkinClick(nil)
          else LoadDefaultSkin
     else LoadSkin('',EditImageDefaultSkin.Picture.BitMap,TOOLS_SKIN_TILE_COUNT,GameViewer.SkinPict.Antialiasing,GameViewer.SkinPict.Masked,RGBToColor(GameViewer.SkinPict.MaskBitMapColor),GameViewer.SkinPict.MaskBitMapPct);

     {$IFDEF YASC}
       InitializeGameViewer(True);
       Self.ShowGame(True);
     {$ELSE}
       if (Editor.FileName<>'') and (Game<>nil) and (Game.SokoFile<>nil) then with Editor do begin
          if   IsAnIniFileSectionFileName(FileName) then
               PackFileName:=ExtractIniFileName(FileName)
          else PackFileName:=FileName;
          if   StrEqual(PackFileName,Game.SokoFile.Name) or
               FileExists(PackFileName) then
               if   LoadLevelFromFile(Editor.FileName) then begin
                    InitializeEditorHistory;
                    end
               else EditMenuItemNewClick(Sender)
          else EditMenuItemNewClick(Sender);
          end
       else EditMenuItemNewClick(Sender);
     {$ENDIF}
     if Assigned(CaptureForm) then CaptureForm.Initialize;
     PageControl1Change(nil);
     end
  else
      if Editor.MouseButtonDown then EditImage1MouseUp(nil,mbLeft,[],0,0);

  SetDrawingToolHint;
  ShowStatus;
  StatusText:='';

  if LevelSetForm<>nil then with LevelSetForm do begin
     StatusBar1.{Panels[1].Text}SimpleText :='';
     if ShowOnStartUp and (not Visible) and (not IsClosing) and (PageControl1.ActivePage=TabSheetEditor) then
        try     Show;
        finally Self.SetFocus;
        end;
     end;

  if Assigned(CaptureForm) then CaptureForm.FormActivate(Sender);
  //BtnGeneratorSettingsClick(Self);
end;

procedure TToolsForm.FormDeactivate(Sender: TObject);
begin
  if not Editor.Selection.Enabled then HideCursor(False);
  SetDrawingToolHint;
  ShowStatus;
  PanelToolTips.Visible:=False;
  IgnoreKeyUp:=True; IgnoreMouseUp:=True;
  if Assigned(CaptureForm) then CaptureForm.FormDeactivate(Sender);
end;

procedure TToolsForm.SetMessageHandlers;
begin
  Application.OnActivate   := ApplicationOnActivate;
  Application.OnDeactivate := ApplicationOnDeactivate;
  Application.OnHint       := ApplicationOnHint;
  Application.OnMessage    := ApplicationOnMessage;

  EditMenuItemDelete.ShortCut:=ShortCut(VK_DELETE,[]);
end;

function  TToolsForm.InitializeTask(Sender__:TObject; TabSheet__:TTabSheet; ToolFlags__:TToolFlagSet):Boolean;
var Col,Row:Integer;
begin
  Result:=False;
  MainForm.FormDeactivate(Sender__);

  Modified:=False; SettingsModified:=False; Editor.LevelHasChangedOrHasBeenModified:=False;
  Game.IsBusy:=False;
  with OpenForm.Game do begin IsBusy:=False; IsReplaying:=False; IsBrowsing:=False; end;
  PluginLevelInfo.ReplayInfo.IsLoaded:=False;

  {$IFDEF YASC}
    if Sender__=MainForm then begin
       Game.DeadlockDetection.DeadLocks:=nil; // so 'Game.Reset' doesn't use deadlock information
       MainForm.Status.Hint            :='';

       Game.SokoFile                   :=MainForm.Game.SokoFile;
       Game.SokoFileName               :=MainForm.Game.SokoFileName;
       SokoFile                        :=Game.SokoFile;
       Result                          :=Game.SetName(MainForm.Game.Name);
       if Editor.History<>nil then Editor.History.Game:=Game;

       GameViewer.BoardDimensionsAsText:=MainForm.BoardDimensionsAsText;
       Editor.FileName                 :=MainForm.Game.FileName;
       Editor.LastValidFileName        :=MainForm.Game.LastValidFileName;
       if Editor.FileName='' then Editor.FileName:=MakeNewLevelFileName;
       ShowTitle('',Editor.FileName);

       ClearBoard(Game.Board);
       Game.SetName(MainForm.Game.Name);
       Game.BoardWidth                 :=MainForm.Game.BoardWidth;
       Game.BoardHeight                :=MainForm.Game.BoardHeight;
       for Col:=1 to Game.BoardWidth do
           for Row:=1 to Game.BoardHeight do
               Game.Board[Col,Row]:=MainForm.Game.StartBoard[Col,Row] and BOARD_PIECES; // remove square flags, if any
       CalculateGameInformation(True);
       InitializeEditorHistory;

       if   Initialized then begin
            InitializeGameViewer(True);
            Self.ShowGame(True);
            end;

       if   PageControl1.ActivePage <> TabSheet__ then
            PageControl1.ActivePage:=TabSheet__;
       PageControl1.Tag:=0;
       PageControl1Change(nil);
       if   tfEditCurrentPosition in ToolFlags__ then
            Result:=Result and MakeNewLevel(MainForm);
       end;
  {$ENDIF}
end;

function  TToolsForm.InitializeEditorHistory:Boolean;
var W,H:Integer; oShowErrorMessages:TShowErrorMessages; EditorHistoryFileName:String; B:TBoard;
begin
  oShowErrorMessages:=ShowErrorMessages;
  try     ShowErrorMessages:=semNone;
          EditorHistoryFileName:=Editor.History.MakeFileName;
          if FileExists(EditorHistoryFileName) and
             Editor.History.Open(EditorHistoryFileName,W,H,B) and
             (W=Game.BoardWidth) and (H=Game.BoardHeight) and
             Game.IsAnIdenticalBoard(B,False,True) then begin
             // the editor history matches the currently loaded game
             end
          else begin
             ShowErrorMessages:=semAll;
             Editor.History.New(EditorHistoryFileName);
             end;
  finally ShowErrorMessages:=oShowErrorMessages;
  end;

  Result:=Editor.History.FileName<>'';
end;

procedure TToolsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key<>SIMULATED_KEYBOARD_EVENT_KEY then IgnoreKeyUp:=False;
//if ( PageControl1.ActivePage = TabSheetCapture ) and Assigned( CaptureForm ) then
//   CaptureForm.FormKeyDown( Sender, Key, Shift );
end;

procedure TToolsForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  var i:Integer;
begin
  if      Key=SIMULATED_KEYBOARD_EVENT_KEY then begin
          end
  else if IgnoreKeyUp then
          IgnoreKeyUp:=False
  else if (Key=VK_ESCAPE) or (Key=VK_BROWSER_BACK) then
          if   CanEscape then
               if   OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
                    (not Game.IsBusy) and
                    CloseEditors and
                    (not Modified) then
                    Close
               else
          else CanEscape:=True
  else if ( PageControl1.ActivePage = TabSheetCapture ) and Assigned( CaptureForm ) then
          CaptureForm.FormKeyUp( Sender, Key, Shift )
  else if Key=Ord(ACCEL_CHAR_CUT_TO_CLIPBOARD) then begin
          if PageControl1.ActivePage=TabSheetGenerator then begin
             if Shift=[ssCtrl] then GeneratorEditMenuItemCutClick(Self);
             end;
          end
  else if (Key=Ord(ACCEL_CHAR_COPY_TO_CLIPBOARD)) and (Shift<>[]) then begin
          if      (PageControl1.ActivePage=TabSheetSolver)
                  or
                  (PageControl1.ActivePage=TabSheetOptimizer)
                  then begin
                  if (ssCtrl in Shift) or (ssAlt in Shift) then
                     CopyLevelToClipboard(ssAlt in Shift,ssShift in Shift);
                  end
          else if PageControl1.ActivePage=TabSheetGenerator then begin
                  if Shift=[ssCtrl] then GeneratorEditMenuItemCopyClick(Self);
                  end;
          end
  else if Key=Ord(ACCEL_CHAR_PASTE_FROM_CLIPBOARD) then begin
          if      (PageControl1.ActivePage=TabSheetSolver) then begin
                  if Shift=[ssCtrl] then PluginEditMenuItemPasteClick(Self)
                  else if (Shift=[]) and
                          (Key=Ord(ACCEL_CHAR_SOLVE)) and
                          BtnSolveLevel.Enabled and
                          (OpenForm.BtnSolveLevel.Tag=Ord(pbsRun)) then begin
                          StatusText:='';
                          BtnPluginClick(Sender);
                          end;
                  end
          else if PageControl1.ActivePage=TabSheetOptimizer then begin
                  if Shift=[ssCtrl] then PluginEditMenuItemPasteClick(Self);
                  end
          else if PageControl1.ActivePage=TabSheetGenerator then begin
                  if Shift=[ssCtrl] then GeneratorEditMenuItemPasteClick(Self);
                  end;
          end
  else if ((Key=Ord(ACCEL_CHAR_PLAY)) or (Key=Ord(ACCEL_CHAR_PAUSE))) and
          ((PageControl1.ActivePage=TabSheetSolver)
           or
           (PageControl1.ActivePage=TabSheetOptimizer)
           or
           (PageControl1.ActivePage=TabSheetGenerator)
          )
          then
          PluginToolButtonReplayClick(Sender)
  else if (Key=Ord(ACCEL_CHAR_STOP)) and
          PluginToolButtonStopReplay.Enabled and
          ((PageControl1.ActivePage=TabSheetSolver)
           or
           (PageControl1.ActivePage=TabSheetOptimizer)
           or
           (PageControl1.ActivePage=TabSheetGenerator)
          ) then begin
          PluginToolButtonStopReplayClick(Sender);
          end
  else if (Key=Ord(ACCEL_CHAR_SOLVE)) and
          (PageControl1.ActivePage=TabSheetSolver) and
          BtnSolveLevel.Enabled and
          (ToolsForm.BtnSolveLevel.Tag=Ord(pbsRun)) then begin
          StatusText:='';
          BtnPluginClick(Sender);
          end
  else if (Key=Ord(ACCEL_CHAR_RESTART_GAME))
          and
          ((PageControl1.ActivePage=TabSheetSolver)
           or
           (PageControl1.ActivePage=TabSheetOptimizer)
          )
          and
          PluginToolButtonStopReplay.Enabled then begin
          PluginToolButtonStopReplayClick(Sender);
          end
  else if (Key=Ord(ACCEL_CHAR_REDO_ALL))
          and
          ((PageControl1.ActivePage=TabSheetSolver)
           or
           (PageControl1.ActivePage=TabSheetOptimizer)
          )
          and
          PluginToolButtonReplay.Enabled and
          OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then begin
          Replay([roEnd],0);
          end
  else if ((Key=Ord(ACCEL_CHAR_UNDO)) or (Key=Ord(ACCEL_CHAR_UNDO1)))
          and
          ((PageControl1.ActivePage=TabSheetSolver)
           or
           (PageControl1.ActivePage=TabSheetOptimizer)
          )
          and
          PluginToolButtonReplay.Enabled and
          OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then begin
          Replay([roStep,roBackwards],0);
          end
  else if (Key=Ord(ACCEL_CHAR_REDO))
          and
          ((PageControl1.ActivePage=TabSheetSolver)
           or
           (PageControl1.ActivePage=TabSheetOptimizer)
          )
          and
          PluginToolButtonReplay.Enabled and
          OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then begin
          Replay([roStep,roForwards],0);
          end
  else if (Key=Ord(ACCEL_CHAR_SAVE))
          and
          (ssCtrl in Shift)
          and
          ((PageControl1.ActivePage=TabSheetSolver)
           or
           (PageControl1.ActivePage=TabSheetOptimizer)
          )
          and
          PluginToolButtonSave.Enabled then
          EditMenuItemSaveClick(PluginToolButtonSave)
  else if ((Key=Ord(ACCEL_CHAR_SAVE_AS))
           or
           (KEY=VK_F2)
          )
          and
          (ssCtrl in Shift)
          and
          ((PageControl1.ActivePage=TabSheetSolver)
           or
           (PageControl1.ActivePage=TabSheetOptimizer)
          )
          and
          PluginToolButtonSaveAs.Enabled then
          EditMenuItemSaveAsClick(PluginToolButtonSaveAs)
  else if (Key=Ord(ACCEL_CHAR_ADD))
          and
          ((PageControl1.ActivePage=TabSheetSolver)
           or
           (PageControl1.ActivePage=TabSheetOptimizer)
          ) then
          PluginMenuItemAddClick(Sender)
//else if Assigned(Game) and Game.IsBrowsing and
//        (Key = VK_LEFT) or (Key = VK_RIGHT) then begin
//        if Key = VK_LEFT  then;
//        if Key = VK_RIGHT then;
//        end
  else if (Key=Ord(ACCEL_CHAR_NEW)) and (Shift=[ssCtrl,ssShift]) and
          (PageControl1.ActivePage=TabSheetEditor) then
          EditToolButtonNewLevelWithWallsClick(Sender)
  else if Shift=[] then
          for i:=0 to Pred(PageControl1.PageCount) do with PageControl1 do
              if (Length(Pages[i].Caption)>TAB_SHEET_SPACES) and
                 (Key=Ord(Pages[i].Caption[TAB_SHEET_SPACES+1])) and
                 (Pages[i]<>ActivePage) then begin
                 ActivePage:=Pages[i];
                 PageControl1Change(PageControl1);
                 break;
                 end;
end;

procedure TToolsForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IgnoreKeyUp:=False; IgnoreMouseUp:=False;
  Game.UserBreakWhileReplaying:=True;
  Editor.MouseButtonDown:=False;
  if (PageControl1.ActivePage=TabSheetCapture) and Assigned(CaptureForm)  then
     CaptureForm.FormMouseDown(Sender,Button,Shift,X,Y);
end;

procedure TToolsForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//var APoint:TPoint;
begin
{
  if (Sender is TControl) then with Sender as TControl do begin
     APoint := EditImage1.ScreenToClient(ClientToScreen(Point(X,Y)));
     end
  else APoint:=Point(-1,-1);
  with APoint do StatusLabel1.Caption:=Format('[%d:%d]',[X,Y]);
}
  if PageControl1.ActivePage=TabSheetEditor then with Editor do begin
     if MouseButtonDown or (not Editor.Selection.Enabled) then
       SetCursor(Cursor,Rect(0,0,0,0));
     end
  else if (PageControl1.ActivePage=TabSheetCapture) and Assigned(CaptureForm) then
          CaptureForm.FormMouseMove(Sender,Shift,X,Y);
  if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
  if PanelToolTips.Visible then PanelToolTips.Hide;
  if Assigned(Game) and Game.IsBrowsing then LeaveBrowseMode(True);
end;

procedure TToolsForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ImageBoard.Tag:=0;
  ImageReplaySpeed.Tag:=0;
  Editor.MouseButtonDown:=False;
  ImageBoard.Cursor:=crDefault;
  if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
  if (PageControl1.ActivePage=TabSheetCapture) and Assigned(CaptureForm)  then
     CaptureForm.FormMouseUp(Sender,Button,Shift,X,Y);
  if (Button=mbRight) and (not Modified) then Close;
end;

procedure TToolsForm.StatusBar1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var SecondaryScoreMetrics:TSecondaryScoreMetrics;
begin
  FormMouseMove(Sender,Shift,X,Y);
  if PluginToolButtonReplay.Enabled and
     PluginLevelInfo.ReplayInfo.IsLoaded and
     (not Game.IsBusy) then with OpenForm.Game do with SecondaryScoreMetrics do
     if   History.Count>0 then begin
          CalculateSecondaryScoreMetrics(SecondaryScoreMetrics);
          StatusBar1.Panels[0].Text:=IntToStr(History.Count)+SLASH+IntToStr(History.PushCount);
          StatusText:=Format(StatusBarHintText__,[BoxLines,BoxChanges,PushingSessions,PlayerLines]);
          end
     else begin StatusBar1.Panels[0].Text:=OKChangedText[Modified {$IFDEF YASC} or MainForm.Modified {$ENDIF}];
                StatusText:='';
          end;
end;

procedure TToolsForm.ImageBoardMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FormMouseDown(Sender,Button,Shift,X,Y);
  if (Button=mbRight) and
     (PageControl1.ActivePage=TabSheetOptimizer) and
     PluginToolButtonReplay.Enabled and
     (not PluginToolButtonStopReplay.Enabled) and
     Assigned(LevelSetForm) and
     Assigned(OpenForm) and Assigned(OpenForm.Game) then begin
     ImageBoard.Tag:=1;
     LevelSetForm.GameViewerMouseToCell(X,Y,Editor.DragPoint.X,Editor.DragPoint.Y);
     Editor.Selection.Rect:=Rect(0,0,0,0);
     ImageBoard.Cursor:=crDrag;
     if Screen.Cursor<>ImageBoard.Cursor then Screen.Cursor:=ImageBoard.Cursor;
     ControlMouseMove(Sender,Shift,X,Y);
     end;
end;

procedure TToolsForm.ControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var APoint:TPoint; ARect:TRect;
begin
  if PanelToolTips.Visible then PanelToolTips.Hide;
  if Assigned(Game) then begin
     if Game.IsBrowsing then LeaveBrowseMode(True);
     if Game.IsReplaying then begin
        if   Sender is TToolButton then with Sender as TToolButton do
             StatusText:=GetLongHint(Hint)
        else StatusText:=GetLongHint(ImageBoard.Hint);
        end;
     if (ImageBoard.Tag=1) and // '1': select/unselect area to be optimized
        (Sender=ImageBoard) then begin
        LevelSetForm.GameViewerMouseToCell(X,Y,X,Y);
        UnionRect(ARect,Rect(X,Y,Succ(X),Succ(Y)),Rect(Editor.DragPoint.X,Editor.DragPoint.Y,Succ(Editor.DragPoint.X),Succ(Editor.DragPoint.Y)));
        ToggleSelectedArea(ARect);
        end;
     end;

  if (Sender is TGroupBox) and (not Game.IsBusy) and (not Game.IsReplaying) then begin
     if      Sender=PluginLevelGroupBox then with PluginLevelGroupBox do begin
             if   Editor.MouseButtonDown and (Screen.Cursor=crHSplit) then begin
                  APoint:=ClientToScreen(Point(X,Y));
                  Y:=APoint.X-Editor.DragPoint.X;
                  if (Y<>0) and ((Y and 1)=0) then begin // 'and 1': even changes only, that is 2,4,6...
                     ResizePluginGroupBoxes(PluginLevelGroupBox.Width+Y,SolverGroupBox.Width-Y,LevelGroupBox.Width,PluginLevelGroupBox.Height,True);
                     Editor.DragPoint.X:=APoint.X;
                     end;
                  end
             else if (X>Width{-PluginLevelFileNamePanel.Left}) and // '>Width': the program changed from resizing by clicking inside the panels to clicking the gaps between the panels; see 'TabSheetPluginMouseMove'
                     ((PluginLevelGroupBox.Width>PluginLevelGroupBoxMinimumWidth)
                      or
                      (SolverGroupBox.Width>PluginGroupBoxMinimumWidth)
                     ) then begin
                     Hint:=HintChangePanelWidthText;
                     Screen.Cursor:=crHSplit;
                     end
                  else begin
                     if Hint<>'' then Hint:='';
                     if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
                     end;
             end
     else if (Sender=SolverGroupBox) or (Sender=OptimizerGroupBox) or (Sender=GeneratorGroupBox) then with SolverGroupBox do begin
             if   Editor.MouseButtonDown and (Screen.Cursor=crHSplit) then begin
                  APoint:=ClientToScreen(Point(X,Y));
                  Y:=APoint.X-Editor.DragPoint.X; //Editor.DragPoint.Y:=SolverGroupBox.Left;
                  if (Y<>0) and ((Y and 1)=0) then begin // 'and 1': even changes only, that is 2,4,6...
                     if   X<Width div 2 then
                          ResizePluginGroupBoxes(PluginLevelGroupBox.Width+Y,SolverGroupBox.Width-Y,LevelGroupBox.Width  ,PluginLevelGroupBox.Height,True)
                     else ResizePluginGroupBoxes(PluginLevelGroupBox.Width  ,SolverGroupBox.Width+Y,LevelGroupBox.Width-Y,PluginLevelGroupBox.Height,True);
                     Editor.DragPoint.X:=APoint.X;//-SolverGroupBox.Left+Editor.DragPoint.Y;
                     end;
                  end
             else if (Y>=BtnSolverBrowse.Top) // '>=BtnSolverBrowse.Top': because a combobox hasn't an 'OnMouseMove' event, so it's not so easy to reset the screen cursor to the default cursor if the mouse hovers over the combobox
                     and
                     (((X<0{SolverComboBox.Left}) // '<0': the program changed from resizing by clicking inside the panels to clicking the gaps between the panels; see 'TabSheetPluginMouseMove'
                       and
                       ((SolverComboBox.Width>PluginGroupBoxMinimumWidth)
                        or
                        (PluginLevelGroupBox.Width>PluginLevelGroupBoxMinimumWidth)
                       )
                      )
                      or
                      ((X>Width{-SolverComboBox.Left}) // '>Width': the program changed from resizing by clicking inside the panels to clicking the gaps between the panels; see 'TabSheetPluginMouseMove'
                       and
                       ((SolverComboBox.Width>PluginGroupBoxMinimumWidth)
                        or
                        (LevelGroupBox.Width>LevelGroupBoxMinimumWidth)
                       )
                      )
                     ) then begin
                     Hint:=HintChangePanelWidthText;
                     ApplicationOnHint(Sender);
                     Screen.Cursor:=crHSplit;
                     end
                  else begin
                     if Hint<>'' then Hint:='';
                     if StatusText<>'' then StatusText:='';
                     if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
                     end;
             OptimizerGroupBox.Hint:=SolverGroupBox.Hint;
             GeneratorGroupBox.Hint:=SolverGroupBox.Hint;
             end
     else if Sender=LevelGroupBox then with LevelGroupBox do begin
             if   Editor.MouseButtonDown and (Screen.Cursor=crHSplit) then begin
                  APoint:=ClientToScreen(Point(X,Y));
                  Y:=APoint.X-Editor.DragPoint.X; //Editor.DragPoint.Y:=LevelGroupBox.Left;
                  if (Y<>0) and ((Y and 1)=0) then begin // 'and 1': even changes only, i.e., 2,4,6...
                     ResizePluginGroupBoxes(PluginLevelGroupBox.Width,SolverGroupBox.Width+Y,LevelGroupBox.Width-Y,LevelGroupBox.Height,True);
                     Editor.DragPoint.X:=APoint.X;//-LevelGroupBox.Left+Editor.DragPoint.Y;
                     end;
                  end
             else if Editor.MouseButtonDown and (Screen.Cursor=crVSplit) then begin
                     APoint:=ClientToScreen(Point(X,Y));
                     X:=APoint.Y-Editor.DragPoint.Y; //Editor.DragPoint.Y:=LevelGroupBox.Left;
                     if (X<>0) and ((X and 1)=0) then begin // 'and 1': even changes only, i.e., 2,4,6...
                        ResizePluginGroupBoxes(PluginLevelGroupBox.Width,SolverGroupBox.Width,LevelGroupBox.Width,LevelGroupBox.Height+X,True);
                        Editor.DragPoint.Y:=APoint.Y;//-LevelGroupBox.Left+Editor.DragPoint.Y;
                        end;
                     end
             else if (X<0{LevelNamePanel.Left}) // '<0': the program changed from resizing by clicking inside the panels to clicking the gaps between the panels; see 'TabSheetPluginMouseMove'
                     and
                     (Y<LevelGroupBox.Height)
                     and
                     ((SolverComboBox.Width>PluginGroupBoxMinimumWidth)
                      or
                      (LevelGroupBox.Width>LevelGroupBoxMinimumWidth)
                     )
                     and
                     (Screen.Cursor<>crVSplit)
                     then begin
                     Hint:=HintChangePanelWidthText;
                     StatusText:=GetLongHint(Hint); // update the screen; setting the hint doesn't always trigger an update when the mouse comes from the "ImageBoard" control; reason unknown
                     Screen.Cursor:=crHSplit;
                     end
             else if (Y>LevelGroupBox.Height) and
                     (Y<LevelGroupBox.Height+PluginLevelGroupBox.Left) then begin
                     Hint:=HintChangePanelHeightText;
                     StatusText:=GetLongHint(Hint); // update the screen; setting the hint doesn't always trigger an update when the mouse comes from the "ImageBoard" control; reason unknown
                     Screen.Cursor:=crVSplit;
                     end
             else begin
               if Hint<>'' then Hint:='';
               if StatusText<>'' then StatusText:='';
               if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
               end;
             end;
     end
  else begin
     if   PageControl1.ActivePage = TabSheetCapture then FormMouseMove( Sender, Shift, X, Y );
     if   (Screen.Cursor<>crDefault) and
          (not ((Sender=ImageBoard) and (Screen.Cursor=ImageBoard.Cursor))) then
          Screen.Cursor:=crDefault;
     if   Sender is TGroupBox  then TGroupBox(Sender).Hint:=''
     else if Sender=ImageBoard then StatusText:=GetLongHint(ImageBoard.Hint);
     end;
end;

procedure TToolsForm.ControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Level:TLevel;
begin
  ImageReplaySpeed.Tag:=0;
  ImageBoard.Cursor:=crDefault;
  if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;

  if Assigned(Game) then
     if      Game.IsReplaying then with PluginLevelInfo.ReplayInfo do begin
             if Button=mbRight then
                if   roForwards in ReplayOptions then
                     ReplayOptions:=ReplayOptions-[RoForwards]+[roBackwards]
                else ReplayOptions:=ReplayOptions+[RoForwards]-[roBackwards];
             OpenForm.Game.IsIdleAndStopReplayingAndBrowsing;
             end
     else if Game.IsBrowsing then LeaveBrowseMode(True)
     else if (Sender=ImageBoard) and (Button=mbLeft) then
             PluginToolButtonReplayClick(Sender)
     else if PluginToolButtonStopReplay.Enabled and (Button=mbRight) then
             PluginToolButtonStopReplayClick(Sender);
  Editor.MouseButtonDown:=False;
  if ImageBoard.Tag<>0 then begin
     ImageBoard.Tag:=0;
     if   (Sender=ImageBoard) and (PageControl1.ActivePage=TabSheetOptimizer) and
          Assigned(OpenForm) and Assigned(OpenForm.Game) then with OptimizerTaskQueue do begin
          PluginLevelInfo.SelectedSquaresAsText:=SelectedBoardSquaresToText(OpenForm.Game.BoardWidth,OpenForm.Game.BoardHeight,OpenForm.Game.Board);
          if Assigned(Plugin) then Plugin.Enter;
          try
            Level:=Levels[StringGrid.Row];
            if   Assigned(Level) and (not Level.SnapshotsAsText.IsEmpty) and (Level.SnapshotsAsText.Last is TExtendedSnapshotAsText) then
                 with TExtendedSnapshotAsText(Level.SnapshotsAsText.Last) do
                      Notes.Lines.WriteString(KEY_SELECTED_SQUARES,PluginLevelInfo.SelectedSquaresAsText)
            else PluginLevelInfo.SelectedSquaresAsText:='';
            ShowStatus;
          finally if Assigned(Plugin) then Plugin.Leave;
          end;
          end
     else PluginLevelInfo.SelectedSquaresAsText:='';
     end;
  if (PageControl1.ActivePage=TabSheetCapture) and Assigned(CaptureForm)  then
     CaptureForm.FormMouseUp(Sender,Button,Shift,X,Y);
  if Button<>mbLeft then
     Mouse.CursorPos:=Mouse.CursorPos; //ensure that the mouse cursor type is updated now, and not first when them mouse moves
end;

function  TToolsForm.ToggleSelectedArea(const Rect__:TRect):Boolean;

  procedure Toggle;
  var FloorCount,SelectedSquaresCount,Col,Row:Integer;
  begin
    FloorCount:=0; SelectedSquaresCount:=0;
    for Col:=1 to OpenForm.Game.BoardWidth do
        for Row:=1 to OpenForm.Game.BoardHeight do
            if ((OpenForm.Game.Board[Col,Row] and (WALL+FLOOR))=FLOOR) then begin
               if (Col>Editor.Selection.Rect.Left) and (Col<=Editor.Selection.Rect.Right) and
                  (Row>Editor.Selection.Rect.Top ) and (Row<=Editor.Selection.Rect.Bottom) then
                  if   (OpenForm.Game.Board[Col,Row] and SQUARE_SET)=0 then
                       Inc(OpenForm.Game.Board[Col,Row],SQUARE_SET)
                  else Dec(OpenForm.Game.Board[Col,Row],SQUARE_SET);
               if (OpenForm.Game.Board[Col,Row] and SQUARE_SET)<>0 then
                  Inc(SelectedSquaresCount);
               Inc(FloorCount);
               end;
    if FloorCount=SelectedSquaresCount then // all the floor squares are selected; delete the marks;
       for Col:=1 to OpenForm.Game.BoardWidth do
           for Row:=1 to OpenForm.Game.BoardHeight do
               OpenForm.Game.Board[Col,Row]:=OpenForm.Game.Board[Col,Row] and (not SQUARE_SET);
  end;

begin
  Result:=not IsEqualRects(Rect__,Editor.Selection.Rect) and
          Assigned(OpenForm) and Assigned(OpenForm.Game);
  if Result then with OpenForm.Game do begin
     Toggle;
     Editor.Selection.Rect:=Rect__;
     Toggle;
     LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,ImageBoard,False);
     end;
end;

procedure TToolsForm.FormResize(Sender: TObject);
var i,W,H:Integer; b,Edit,oIsBusy:Boolean;
begin
  if (ToolsForm<>nil) // otherwise the program is shutting down
     //and
     //(not Game.IsBusy)
     then begin
     oIsBusy:=Game.IsBusy;
     CloseEditors;
     Edit:=HideEditors;
     try
       Game.IsBusy:=True;
       Inc(FormResizeCount);

       // editor
       PanelBtnBevel1.Left:=PanelBtn.ClientWidth-156;
       with PanelBtnBevel1 do Visible:=Left>=BtnHelp.Left+BtnHelp.Width+4;
       OnFontChange;
       GameViewer.BackgroundInitialized:=False;
       GameViewer.SkinInitialized:=False;
       Editor.UseCursorPictures:=False;
       InitializeGameViewer(True);
       Self.ShowGame(False);

       // plugin
       OpenForm.Game.IsIdleAndStopReplayingAndBrowsing;
       PluginLevelFileNamePanel.Font.Color:=ApplicationHiglightedTextColor;
       LevelNamePanel.Font.Color:=PluginLevelFileNamePanel.Font.Color;
       CurrentSolverGroupBox.Font.Color:=PluginLevelFileNamePanel.Font.Color;
       CurrentOptimizerGroupBox.Font.Color:=PluginLevelFileNamePanel.Font.Color;

       W:=TabSheetSolver.ClientWidth -LevelGroupBox.Left        -PluginLevelGroupBox.Left;
       H:=TabSheetSolver.ClientHeight-(SolveLevelsGroupBox.Top+SolveLevelsGroupBox.Height)-PluginLevelGroupBox.Left; // 'H' = unused height pixels; a negative number means the height of the group boxes is too high
       if (W<LevelGroupBoxMinimumWidth) or
          (H<>0) then begin
          i:=Max(SolveLevelsGroupBoxMinimumHeight,SolveLevelsGroupBox.Height+Min(0,H));  // 'SolveLevelsGroupBox' height after stealing up to 'H' pixels
          Inc(H,SolveLevelsGroupBox.Height-i);
          H:=Max(PluginGroupBoxesMinimumHeight,LevelGroupBox.Height+Min(0,H));
          W:=Max(0,LevelGroupBoxMinimumWidth-W); // difference to get up to minimum width
          i:=Max(SolverGroupBox.Width-W,PluginGroupBoxMinimumWidth);  // 'SolverGroupBox' width after stealing up to 'W' pixels
          Dec(W,SolverGroupBox.Width-i); // steal the rest 'W' from 'PluginLevelGroupBox' width
          ResizePluginGroupBoxes(Max(PluginLevelGroupBoxMinimumWidth,PluginLevelGroupBox.Width-W),i,LevelGroupBoxMinimumWidth,H,False);
          end;

       with PluginLevelStringGrid do begin
         LeftCol:=FixedCols;
         ColWidths[1]:=ClientWidth-Pred(ColCount)*GridLineWidth-ColWidths[0];
         while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]);
         end;
       with GeneratorStatusStringGrid do begin
         LeftCol:=FixedCols;
         ColWidths[1]:=ClientWidth-Pred(ColCount)*GridLineWidth-ColWidths[0];
         while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]);
         end;
       with LevelGroupBox do Width:=TabSheetSolver.ClientWidth-Left-PluginLevelGroupBox.Left;
       with LevelNamePanel do Width:=LevelGroupBox.ClientWidth-2*Left;
       with GeneratorNamePanel do Width:=GeneratorGroupBox.ClientWidth-2*Left;
       with PanelBoard do Width:=LevelNamePanel.Width;
       with OptimizeSolutionsGroupBox do begin
         Width:=TabSheetSolver.ClientWidth-2*Left;
         Height:=TabSheetSolver.ClientHeight-Top-Left;
         SolveLevelsGroupBox.SetBounds(Left,Top,Width,Height);
         GenerateLevelsGroupBox.SetBounds(Left,Top,Width,Height);
         end;
       with OptimizeSolutionsGridPanel do begin
         Width :=OptimizeSolutionsGroupBox.ClientWidth-2*Left;
         Height:=OptimizeSolutionsGroupBox.ClientHeight-Top-Left;
         SolveLevelsGridPanel.SetBounds(Left,Top,Width,Height);
         GenerateLevelsGridPanel.SetBounds(Left,Top,Width,Height);
         end;
       with OptimizeSolutionsStringGrid do begin
         //if   VisibleRowCount<RowCount then
         //     ScrollBars:=ssBoth
         //else ScrollBars:=ssVertical;
         ColWidths[Ord(oscNo)]:=Self.Canvas.TextWidth(IntToStr(Max(99999,Max(RowCount,SolveLevelsStringGrid.RowCount))))+8;
         for i:=0 to Pred(ColCount) do
             ColWidths[i]:=Max(ColWidths[i],2*VerticalScrollBarWidth);

         W:=ClientWidth-Succ(ColCount)*GridLineWidth;
         {
         for i:=0 to Pred(ColCount) do
             if (i<>Ord(oscLevelName)) and (i<>Ord(oscSnapShotName)) then Dec(W,ColWidths[i]);
         //ColWidths[Ord(oscLevelName)]:=Max(0,W div 2);
         //ColWidths[Ord(oscSnapShotName)]:=Max(0,W-ColWidths[Ord(oscLevelName)]);
         Dec(W,ColWidths[Ord(oscLevelName)]+ColWidths[Ord(oscSnapShotName)]);
         if W>0 then begin
           i:=W div 2;
           ColWidths[Ord(oscLevelName   )]:=ColWidths[Ord(oscLevelName    )]+i;
           ColWidths[Ord(oscSnapShotName)]:=ColWidths[Ord(oscSnapShotName)]+(W-i);
           end;
         }
         for i:=0 to Pred(ColCount) do
             if i<>Ord(oscLevelName) then Dec(W,ColWidths[i]);
         //ColWidths[Ord(oscLevelName)]:=Max(0,W div 2);
         Dec(W,ColWidths[Ord(oscLevelName)]);
         if W>0 then ColWidths[Ord(oscLevelName   )]:=ColWidths[Ord(oscLevelName)]+W;
         MakeAllColumnsFullyVisible;
         OptimizationComboBox.Top:=DefaultRowHeight+GridLineWidth;
         SelectBtn.Left:=OptimizationComboBox.Left+ColWidths[Ord(oscOptimization)]-SelectBtn.Width;
         Tag:=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // '-TOOL_TIP_FREE_ZONE_PIXELS': 'OnMouseMove' isn't triggered when the mouse is over the scrollbar, hence, this extra tooltip free zone
         if Assigned(OptimizerTaskQueue) then with OptimizerTaskQueue do begin
            ItemsCheckBox.Left:=(StringGrid.ColWidths[Ord(oscSelect)]-ItemsCheckBox.Width) div 2;
            OptimizerTaskQueue.Refresh(False);
            end;
         with ImageBoard do begin Width:=Parent.ClientWidth; Height:=Parent.ClientHeight; end;
         ResizeImage(ImageBoard);
         with ImageReplaySpeed do begin Width:=Parent.ClientWidth; Height:=Parent.ClientHeight; end;
         ResizeImage(ImageReplaySpeed); ShowReplaySpeed;

         if Assigned(LevelSetForm) then with LevelSetForm.GameViewer do
            if BoardImage=Self.ImageBoard then begin
               BackgroundInitialized:=False;
               SkinInitialized:=False;
               if   Game.IsReplaying or Game.IsBrowsing or
                    PluginToolButtonStopReplay.Enabled then with OpenForm.Game do begin
                    if (PluginLevelInfo.SelectedSquaresAsText<>'') then
                       SelectedSquaresAsTextToBoard(PluginLevelInfo.SelectedSquaresAsText,BoardWidth,BoardHeight,Board);
                    LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,BoardImage,False);
                    end
               else LoadLevelFromEditor(False,True);
               end;

         if Assigned(OptimizerTaskQueue) and
            (PageControl1.ActivePage=TabsheetOptimizer) then
            with OptimizerTaskQueue do begin
              ScrollInView(StringGrid.Row);
              if (StringGrid.Row>Succ(StringGrid.FixedRows)) or Assigned(Levels[StringGrid.Row]) then
                 OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,b);
              end;
         if Assigned(SolverTaskQueue) and
            (PageControl1.ActivePage=TabsheetSolver) then
            with SolverTaskQueue do begin
              ScrollInView(StringGrid.Row);
              if (StringGrid.Row>Succ(StringGrid.FixedRows)) or Assigned(Levels[StringGrid.Row]) then
                 SolveLevelsStringGridSelectCell(nil,0,StringGrid.Row,b);
              end;
         end;

       with GenerateLevelsStringGrid do begin
         for i:=0 to Pred(ColCount) do
             ColWidths[i]:=Max(ColWidths[i],2*VerticalScrollBarWidth);
         ColWidths[Ord(glcSelect   )]:=OptimizeSolutionsStringGrid.ColWidths[Ord(oscSelect)];
         ColWidths[Ord(glcNo       )]:=OptimizeSolutionsStringGrid.ColWidths[Ord(oscNo    )];
         ColWidths[Ord(glcFitness  )]:=Max(ColWidths[Ord(glcFitness)],ColWidths[Ord(glcPushes)]);
         W:=ClientWidth-Succ(ColCount)*GridLineWidth;
         for i:=0 to Pred(ColCount) do
             if (i<>Ord(glcLevelName)) then Dec(W,ColWidths[i]);
         //ColWidths[Ord(glcLevelName)]:=Max(0,W);
         Dec(W,ColWidths[Ord(glcLevelName)]);
         if W>0 then
            ColWidths[Ord(glcLevelName   )]:=ColWidths[Ord(glcLevelName    )]+W;
         MakeAllColumnsFullyVisible;
         Tag:=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // '-TOOL_TIP_FREE_ZONE_PIXELS': 'OnMouseMove' isn't triggered when the mouse is over the scrollbar, hence, this extra tooltip free zone
         if Assigned(GeneratorTaskQueue) then with GeneratorTaskQueue do begin
            ItemsCheckBox.Left:=(StringGrid.ColWidths[Ord(oscSelect)]-ItemsCheckBox.Width) div 2;
            GeneratorTaskQueue.Refresh(False);
            if (PageControl1.ActivePage=TabsheetGenerator) then with GeneratorTaskQueue do begin
                ScrollInView(StringGrid.Row);
                if   (StringGrid.Row>Succ(StringGrid.FixedRows)) or Assigned(Levels[StringGrid.Row]) then
                     GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,b)
                else GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,b);
                end;
            end;
         end;

       with SolveLevelsStringGrid do begin
         for i:=0 to Pred(ColCount) do
             ColWidths[i]:=Max(ColWidths[i],2*VerticalScrollBarWidth);
         ColWidths[Ord(slcSelect   )]:=OptimizeSolutionsStringGrid.ColWidths[Ord(oscSelect)];
         ColWidths[Ord(slcNo       )]:=OptimizeSolutionsStringGrid.ColWidths[Ord(oscNo    )];
         W:=ClientWidth-Succ(ColCount)*GridLineWidth;
         for i:=0 to Pred(ColCount) do
             if i<>Ord(slcLevelName) then Dec(W,ColWidths[i]);
         //ColWidths[Ord(slcLevelName)]:=Max(0,W);
         Dec(W,ColWidths[Ord(slcLevelName)]);
         if W>0 then
            ColWidths[Ord(slcLevelName   )]:=ColWidths[Ord(slcLevelName    )]+W;
         MakeAllColumnsFullyVisible;
         Tag:=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // '-TOOL_TIP_FREE_ZONE_PIXELS': 'OnMouseMove' isn't triggered when the mouse is over the scrollbar, hence, this extra tooltip free zone
         if Assigned(SolverTaskQueue) then with SolverTaskQueue do begin
            ItemsCheckBox.Left:=(StringGrid.ColWidths[Ord(oscSelect)]-ItemsCheckBox.Width) div 2;
            SolverTaskQueue.Refresh(False);
            if (PageControl1.ActivePage=TabsheetSolver) then with SolverTaskQueue do begin
               ScrollInView(StringGrid.Row);
               if (StringGrid.Row>Succ(StringGrid.FixedRows)) or Assigned(Levels[StringGrid.Row]) then
                   SolveLevelsStringGridSelectCell(nil,0,StringGrid.Row,b);
               end;
            end;
         end;

       if Assigned(CaptureForm) then CaptureForm.FormResize(Sender);

     finally Game.IsBusy:=oIsBusy;
             if Edit then with OptimizerTaskQueue.StringGrid do
                ShowOptimizationComboBox(Row);

             SettingsMenuItemWindowSizeMaximized.Checked:=Self.WindowState=wsMaximized; // doesn't work because 'Self.WindowState' hasn't been updated at this time
             CaptureSettingsMenuItemWindowSizeMaximized.Checked := SettingsMenuItemWindowSizeMaximized.Checked;
             PostMessage(Self.Handle,MSG_STATUS,0,0); // 'Self.WindowState' hasn't been updated yet, hence, post a message to update the status when the resizing has finished
     end;
     end;
//
//with EditToolBarLabel1 do begin
    //Width:=EditToolBar.ClientWidth-Left-4;
//  if   IsDefaultColorBtnFace(Color) then
//       Font.Color:=clBlue
//  else Font.Color:=clBtnText;
//  end;

//SetBounds(Left,Top,640,480); // activate this statement and set 'MinHeight:=480' in order to produce a 640x480 screenshot;
end;

procedure TToolsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Editor.History<>nil then Editor.History.Close;
  CleanSolverTaskQueue;
  if Assigned(MainForm.Generator) and (not MainForm.Generator.IsActive) then
     MainForm.Generator.ReleaseMemory;

  if LevelSetForm<>nil then with LevelSetForm do begin
     ShowOnStartUp:=Visible or
                    (ShowOnStartUp and (PageControl1.ActivePage<>TabSheetEditor));
     Hide;
     end;
  if Assigned( CaptureForm ) and
     ( CaptureForm.Editor.Zoom > 100 ) then
     CaptureForm.ShowImage( 100 ); // reduce the memory footprint of various bitmaps assigned to the screen components
end;

procedure TToolsForm.ExitMenuClick(Sender: TObject);
var CanClose:Boolean;
begin
//AnimateButtonMouseClick(EditToolButtonExit);
//
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then begin
     CanClose:=True;
     if Modified
        and
        ((Sender=EditMenuItemExit)
         or
         (Sender=PluginMenuItemExit)
         or
         (Sender=GeneratorMenuItemExit)
         or
         (Sender=EditToolButtonExit)
         or
         (Sender=PluginToolButtonExit)
         or
         (Sender=CaptureFileMenuItemExit)
         or
         (Sender=CaptureToolButtonExit)
        ) then begin
        // commit changes without asking the user for confirmation;
        // that is OK because the hint text on the statusbar says "Apply changes, if any, and return to previous window"
        HideCursor(False);
        CloseEditorSelection(True,False);
        CanClose:=Save(Self) and Editor.History.Close;
        if   CanClose then
             PluginLevelInfo.NewGamesCount:=0
        else Modified:=True;
        ShowTitle('',Editor.FileName);
        ShowStatus;
        end;
     if CanClose then Close;
     end;
end;

procedure TToolsForm.ApplicationOnHint(Sender: TObject);
begin
  StatusText := GetLongHint(Application.Hint);
end;

function  TToolsForm.GetStatusText:String;
begin
  Result:=StatusBar1.Panels[1].Text;
end;

procedure TToolsForm.SetStatusText(const Text__:String);
begin
  StatusBar1.Panels[1].Text:=Text__;
end;

procedure TToolsForm.TabSheetEditorEnter(Sender: TObject);
begin
  PageControl1Enter(Sender);
end;

procedure TToolsForm.TabSheetEditorExit(Sender: TObject);
begin
//
end;

procedure TToolsForm.PageControl1Enter(Sender: TObject);
begin
  if   ( PageControl1.ActivePage = TabSheetCapture ) and Assigned( CaptureForm ) then
       CaptureForm.ShowTitle
  else ShowTitle('',Editor.FileName);
end;

procedure TToolsForm.PageControl1Exit(Sender: TObject);
begin
//
end;

procedure TToolsForm.BtnOKClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then begin
     CloseEditorSelection(True,False);
     if   Modified then
          if Save(Self) then begin
             Modified:=False; Editor.FileName:='';
             ActiveControl:=BtnOK; ModalResult:=mrOk; Close; ModalResult:=mrOk;
             end
          else
     else BtnCancelClick(Sender);
     end;
end;

procedure TToolsForm.BtnCancelClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then begin
     CloseEditorSelection(True,False);
     if Editor.History<>nil then Editor.History.UndoSession;
     SnapshotsForm.ShowOnStartup:=False;
     if PanelBtn.Visible then ActiveControl:=BtnCancel;
     Modified:=False; Editor.FileName:='';
     ModalResult:=mrCancel; Close; ModalResult:=mrCancel;
     end;
end;

function TToolsForm.MakeNewLevel(Sender: TObject):Boolean;
var W,H,OldNewLevelCount:Integer;
    CapturedBoardAsText, ErrorText, MovesAsText : String; Level : TLevel;

  procedure ImportBoardFromGame(Game__:TGame);
  var Col,Row:Integer;
  begin
    Game.BoardWidth :=Game__.BoardWidth;
    Game.BoardHeight:=Game__.BoardHeight;
    for Col:=1 to Game.BoardWidth do
        for Row:=1 to Game.BoardHeight do begin
            Game.Board[Col,Row]:=Game__.Board[Col,Row] and BOARD_PIECES;
            if Game.Board[Col,Row]<>FLOOR then Modified:=True;
            end;
    CalculateGameInformation(True);
  end;

begin // 'MakeNewLevel'; precondition: 'Sender' = 'MainForm' only when this function is called from the function 'InitializeTask'
  Result:=False; OldNewLevelCount:=NewLevelCount;
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors and
     CloseEditorSelection(True,False) and
     OpenForm.Game.SetName('') and
     (((Sender<>GeneratorEditMenuItemEdit) and MainForm.CloseLevel(Self))
      or
      ((Sender= GeneratorEditMenuItemEdit) and MainForm.CloseLevel(GeneratorEditMenuItemEdit))
     )
     then with Game do begin
     NewLevelCount:=OldNewLevelCount;
     PluginToolButtonStopReplayClick(PluginToolButtonReplay);
     Modified:=False;
     PluginLevelInfo.NewGamesCount:=0; PluginLevelInfo.ReplayInfo.IsLoaded:=False;
     if   ( Sender = PlayPuzzleBitBtn ) or ( Sender = CaptureToolButtonPlay ) or ( Sender = CaptureEditLevelBitBtn ) then begin
          Editor.FileName := CaptureForm.Editor.PuzzleFileName;
          if      Editor.FileName = '' then begin
                  Editor.FileName := TextThatCannotBeInterpretedAsMoves( StrToFileName( Trim( ToolsForm.PuzzleTitleEdit.Text ) ) );
                  if      Editor.FileName =  '' then
                          Editor.FileName := MakeNewLevelFileName
                  else if ExtractFilePath( Editor.FileName ) = '' then
                          Editor.FileName := StrWithTrailingPathDelimiter( StrWithTrailingPathDelimiter( MainForm.ApplicationDataPath ) + DEFAULT_LEVEL_DIRECTORY ) + Editor.FileName;
                  Editor.FileName := MakeUniqueFileName( StrWithoutTrailingPathDelimiter( ExtractFilePath( Editor.FileName ) ), ChangeFileExt( ExtractFileName( Editor.FileName ), '' ), SOKOBAN_FILE_NAME_EXT );
                  end;

          end
     else Editor.FileName:=MakeNewLevelFileName;
     ShowTitle('',Editor.FileName);

     with EditPopupMenuOpen do
       while Items.Count<>0 do Items[0].Free;

     if Editor.History<>nil then with Editor.History do
        if   (Sender=MainForm.BtnOpen) and (PageControl1.ActivePage=TabSheetEditor) and UndoSession then
             Close
        else Clear;

     Game.Clear;
     ClearBoard(Game.Board);
     PlayerPos.X:=0; PlayerPos.Y:=0; BoxCount:=0; GoalCount:=0;
     BoardWidth:=0; BoardHeight:=0;
     Editor.BoardCellOrigin:=Point(1,1);
     Result:=True;

     if Sender=MainForm then begin
        if (MainForm.Game.Name<>'') or (MainForm.Game.FileName<>'') then // there is no particular reason to check both names, but it doesn't hurt
           // there is a game in the main window
           ImportBoardFromGame(MainForm.Game)
        else
           // the game in the main window is empty, probably because the user in
           // 'MainForm.CloseLevel' has chosen not to save a previously created
           // new level;
           // in that case, 'MainForm.CloseLevel' stored the current position
           // in 'OpenForm.Game'
//         if StrEqual(OpenForm.Game.Name,Editor.FileName) then
              ImportBoardFromGame(OpenForm.Game);
        end
     else if Sender=GeneratorEditMenuItemEdit then with GeneratorTaskQueue do begin
             if Assigned(Plugin) then Plugin.Enter;
             try     Level:=Levels[StringGrid.Row];
                     if   Assigned(Level) then begin
                          if   Level.SnapshotsAsText.IsEmpty or TSnapshotAsText(Level.SnapshotsAsText.First).MovesAsTextLines.IsEmpty then
                               MovesAsText      :=''
                          else MovesAsText      :=TSnapshotAsText(Level.SnapshotsAsText.First).MovesAsTextLines.First.Text;
                          if   OpenForm.Game.LoadFromBoardAsText(Level.Tag.BoardWidth,Level.Tag.BoardHeight,True,False,MovesAsText<>'',False,Level.BoardAsTextLines.First.Text)
                               and
                               OpenForm.Game.LoadSnapshotAsTextString(MovesAsText,True)
                               and
                               ((OpenForm.Game.History.Top>OpenForm.Game.ForcedInitialJumps)
                                 or
                                 (MovesAsText='')
                                ) then begin
                                ImportBoardFromGame(OpenForm.Game);
                                MainForm.Game.Clear;
                                SwapNodes(TNode(MainForm.Game.BestSolutionMoves),TNode(OpenForm.Game.BestSolutionMoves)); // transfer the best solution, if any, to 'MainForm.Game'
                                Result:=SaveLevel(nil) and Editor.History.Close; // transfer the board to 'MainForm.Game'
                                if Result then begin
                                   PluginLevelInfo.NewGamesCount:=0;
                                   Modified:=False; MainForm.Modified:=False;
                                   // resetting the modified flag is not strictly correct because the level
                                   // can get lost when the user loads another level from the generator;
                                   // however, doing it this way makes the generator work flow so much
                                   // easier that it's worth accepting this small inaccuracy;
                                   // the user can switch back and forth between candidates, view them in the
                                   // editor, and play them in the main window without facing "this level
                                   // is new. Do you want to save it?" questions all the time.
                                   //
                                   // the level is not totally unprotected; the program still asks the
                                   // "save it?" question if the user loads another level from disk or
                                   // exits the program from the main window;
                                   end;
                                end
                          else Result:=False;
                          end
                     else Result:=False;
             finally if Assigned(Plugin) then Plugin.Leave;
             end;
             end
          else if ( Sender = PlayPuzzleBitBtn ) or ( Sender = CaptureToolButtonPlay ) or ( Sender = CaptureEditLevelBitBtn ) then begin
                  if   ( Editor.FileName <> '' ) and
                       CaptureForm.BoardAsTextWithBorder( CapturedBoardAsText, W, H ) and
                       OpenForm.Game.LoadFromBoardAsText(W,H,False,False,False,False,CapturedBoardAsText) then begin
                       ImportBoardFromGame(OpenForm.Game);
                       MainForm.Game.Clear;

                       Result:=SaveLevel(nil) and Editor.History.Close; // transfer the board to 'MainForm.Game'
                       if Result then begin
                          PluginLevelInfo.NewGamesCount:=0;
                          if Trim( PuzzleTitleEdit.Text ) <> '' then
                             MainForm.Game.Notes.Lines.WriteString( KEY_TITLE,  Trim( PuzzleTitleEdit .Text ) );
                          if Trim( PuzzleAuthorEdit.Text ) <> '' then
                             MainForm.Game.Notes.Lines.WriteString( KEY_AUTHOR, Trim( PuzzleAuthorEdit.Text ) );
                          end;

                       if ( Sender = CaptureEditLevelBitBtn ) and
                          ( not IsALegalLevel( False, True, ErrorText ) ) then begin
                          MainForm.Game.Clear;   // ensure there isn't an invalid board in the main window
                          MainForm.Modified := False;
                          Self.Modified := True; // ensure the board is validated before the user is allowed to return to the main window
                          end;
                       end
                  else Result:=False;
                  end
               else if   Sender = EditToolButtonNewLevelWithWalls then begin
                         W:=ToolButtonNewLevelWithWallsWidth.Tag;
                         H:=ToolButtonNewLevelWithWallsHeight.Tag;
                         if CreateBoardPrefilledWithWalls(W,H,0,Game.Board) or // try twice before giving up
                            CreateBoardPrefilledWithWalls(W,H,0,Game.Board) then begin
                            Game.BoardWidth:=W; Game.BoardHeight:=H;
                            CalculateGameInformation(False);
                            end
                         else Result:=False;
                         end
                    else Result := False;

     HideCursor(False);
     InitializeGameViewer((Sender=MainForm) or (Sender=GeneratorEditMenuItemEdit) or (Sender=PlayPuzzleBitBtn) or (Sender=CaptureToolButtonPlay) or (Sender=CaptureEditLevelBitBtn) or (Sender=EditToolButtonNewLevelWithWalls));
     Self.ShowGame(True);
     end;
end;

procedure TToolsForm.EditMenuItemNewClick(Sender: TObject);
begin
  //if Sender<>nil then AnimateButtonMouseClick(EditToolButtonNew);
  if (Screen.ActiveForm=Self) and
     OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors then
     if      PageControl1.ActivePage=TabSheetEditor    then MakeNewLevel(Sender)
     else if PageControl1.ActivePage=TabSheetGenerator then GeneratorMenuItemNewClick(Sender);
end;

procedure TToolsForm.EditMenuItemOpenClick(Sender: TObject);
var H,W:Integer; oModified,Result:Boolean; oFileName:String; B:TBoard;
begin
//AnimateButtonMouseClick(EditToolButtonOpen);
  CloseEditorSelection(True,False);

  if (Screen.ActiveForm=Self) and
     OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     (not ((PageControl1.ActivePage=TabSheetGenerator) and Assigned(MainForm.Generator) and (MainForm.Generator.IsActive))) and
     ( PageControl1.ActivePage <> TabSheetCapture ) and
     CloseEditors
     // and
     //CloseLevel(Sender)
     then begin

     PluginToolButtonStopReplayClick(PluginToolButtonReplay);
     PluginLevelInfo.ReplayInfo.IsLoaded:=False;
     PageControl1.Tag:=0;

     if Editor.Selection.Enabled then begin
        Editor.Selection.Enabled:=False;
        with Editor.CursorRect do SetCursor(Editor.Cursor,Rect(0,0,0,0)); //Rect(Left,Top,Succ(Left),Succ(Top)));
        end;

     CleanSolverTaskQueue;

     if (PageControl1.ActivePage<>TabSheetSolver) and
        (PageControl1.ActivePage<>TabSheetOptimizer) and
        (PageControl1.ActivePage<>TabSheetGenerator) then
        LoadLevelFromEditor(False,False); // so 'PluginLevelInfo' contains the current level; if the user cancels 'Open', then it's necessary to restore the current game information partially based on 'PluginLevelInfo'
     CalculateGameInformation(False); ShowStatus;
     W:=Game.BoardWidth; H:=Game.BoardHeight; B:=Game.Board;

     {$IFDEF YASC}
       oFileName:=Editor.FileName;
       oModified:=Modified or MainForm.Modified;
       try
         if      PageControl1.ActivePage=TabSheetGenerator then
                 Result:=MainForm.Open(GeneratorMenuItemOpen,osGeneratorCandidateSet)
         else if Sender=PluginMenuItemImportTaskQueue then
                 Result:=MainForm.Open(PluginMenuItemImportTaskQueue,osPluginTaskQueue)
         else if Sender=PluginMenuItemOpenPrior then
                 Result:=MainForm.OpenPriorOrNext(True ,(PageControl1.ActivePage=TabSheetSolver) and IsKeyPressed(VK_CONTROL),False,VK_CONTROL)
         else if Sender=PluginMenuItemOpenNext then
                 Result:=MainForm.OpenPriorOrNext(False,(PageControl1.ActivePage=TabSheetSolver) and IsKeyPressed(VK_CONTROL),False,VK_CONTROL)
         else if PageControl1.ActivePage=TabSheetEditor then
                 //Result:=MainForm.Open(Self,osLevelEditor) // allowing the editor to open incomplete levels isn't fully implemented
                 Result:=MainForm.Open(Self,osNone)
         else    Result:=MainForm.Open(Self,osNone);

         if      PageControl1.ActivePage=TabSheetGenerator then begin
                 InitializeTask(MainForm,PageControl1.ActivePage,[]);
                 FormActivate(Sender);
                 PageControl1Change(nil);
                 end
         else if Sender = PluginMenuItemImportTaskQueue then begin
                 Game.BoardWidth:=W; Game.BoardHeight:=H; Game.Board:=B;
                 CalculateGameInformation(False);
                 FormActivate(Sender);
                 PageControl1Change(PluginMenuItemImportTaskQueue);
                 end
         else if Result then begin
                 PluginLevelInfo.NewGamesCount:=0;
                 if Editor.History<>nil then Editor.History.Close;
                 InitializeTask(MainForm,PageControl1.ActivePage,[]);
                 FormActivate(Sender);
                 end
         else if (MainForm.Game.FileName='') and
                 (oFileName<>'') and
                 (not IsANewFileName(oFileName)) and
                 MainForm.LoadGame(oFileName,True) then begin
                 PluginLevelInfo.NewGamesCount:=0;
                 InitializeTask(MainForm,PageControl1.ActivePage,[]);
                 FormActivate(Sender);
                 end
         else if (Sender<>PluginMenuItemOpenPrior) and
                 (Sender<>PluginMenuItemOpenNext) and  // i.e., if it's the result from calling 'MainForm.Open(Self)'
                 IsANewFileName(oFileName) and
                 (not StrEqual(MainForm.Game.FileName,oFileName)) and
                 (MainForm.Game.FileName<>'') and // probably the new level was saved, but the user cancelled opening another level
                 oModified and
                 (not (Modified or MainForm.Modified)) then begin
                 if StrEqual(Editor.FileName,PluginLevelInfo.LevelName) then
                    PluginLevelInfo.LevelName:=MainForm.Game.FileName;
                 Editor.FileName:=MainForm.Game.FileName;
                 ShowTitle('',Editor.FileName);
                 Game.BoardWidth:=W; Game.BoardHeight:=H; Game.Board:=B;
                 CalculateGameInformation(False);
                 end
         else    begin
                 Game.BoardWidth:=W; Game.BoardHeight:=H; Game.Board:=B;
                 CalculateGameInformation(False);
                 end;
       finally
         Game.IsBusy:=False;
         with OpenForm.Game do begin IsBusy:=False; IsReplaying:=False; IsBrowsing:=False; end;
         SetMessageHandlers;
         if      PageControl1.ActivePage=TabSheetOptimizer then
                 OptimizerTaskQueue.FocusLevelName(Editor.FileName,True)
         else if PageControl1.ActivePage=TabSheetSolver then
                 SolverTaskQueue.FocusLevelName(Editor.FileName,True)
         else if PageControl1.ActivePage=TabSheetGenerator then begin
                 end;

         OpenForm.PluginTimer.Enabled:=(Assigned(MainForm.Solver   ) and (MainForm.Solver   .StartTimeMS<>0))
                                       or
                                       (Assigned(MainForm.Optimizer) and (MainForm.Optimizer.StartTimeMS<>0))
                                       or
                                       (Assigned(MainForm.Generator) and (MainForm.Generator.StartTimeMS<>0));

         ShowStatus;
       end;

     {$ELSE}
       //EditMenuItemNewClick(Sender);
       OpenDialog(Sender);
     {$ENDIF}
     end;
end;

procedure TToolsForm.EditMenuItemSaveClick(Sender: TObject);
begin
//AnimateButtonMouseClick(EditToolButtonSave);
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
       if Screen.ActiveForm=Self then
          if PageControl1.ActivePage=TabSheetGenerator then
             GeneratorMenuItemSaveClick(Sender)
          else if PageControl1.ActivePage=TabSheetCapture then // do nothing; the 'save' keyboard shortcut is handled elsewhere
               else
                  if EditToolButtonSave.Enabled and
                     ((Modified {$IFDEF YASC} or MainForm.Modified {$ENDIF}) or (PluginLevelInfo.IsALegalLevel and (PluginLevelInfo.NewGamesCount<>0))) then
                     if   IsANewFileName(Editor.FileName) then
                          Save(EditMenuItemSaveAs)
                     else Save(EditMenuItemSave); // 'Self': don't ask before overwriting an existing file
end;

procedure TToolsForm.EditMenuItemSaveAsClick(Sender: TObject);
begin
//AnimateButtonMouseClick(EditToolButtonSaveAs);
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
        if Screen.ActiveForm=Self then
           if   PageControl1.ActivePage=TabSheetGenerator then
                GeneratorMenuItemSaveAsClick(Sender)
           else if PageControl1.ActivePage=TabSheetCapture then // do nothing; the 'save as' keyboard shortcut is handled elsewhere
                else if   Sender <> PluginMenuItemSaveTaskQueueAs then
                          Save( EditMenuItemSaveAs ) // 'Save as...': ask before overwriting an existing file
                     else Save( Sender ); // save task queue as a new level collection
end;

function  TToolsForm.Save(Sender: TObject):Boolean;
var b:Boolean; s:String;
begin
  HideCursor(False);
  Result:=OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
          (not Game.IsBusy) and
          CloseEditors;

  if Result then
     if   (Sender<>GeneratorMenuItemSave) and (Sender<>GeneratorMenuItemSaveAs) then
          if   Sender <> PluginMenuItemSaveTaskQueueAs then
               Result:=IsALegalLevel(True,True,s) and SaveDialog(Sender) and SaveLevel(Sender)
          else begin Result := SaveDialog(Sender);
                     if Result then
                        if        PageControl1.ActivePage = TabSheetSolver then
                                  Result := SolverTaskQueue.SaveToFile( SaveDialog1.FileName )
                        else if   PageControl1.ActivePage = TabSheetOptimizer then
                                  Result := OptimizerTaskQueue.SaveToFile( SaveDialog1.FileName )
                             else Result := False;
               end
     else Result:=SaveDialog(Sender) and MainForm.Generator.SaveToFile(MainForm.Generator.FileName);

  ShowTitle('',Editor.FileName); ShowStatus;
  if      PageControl1.ActivePage=TabSheetEditor then begin
          EditToolButtonSave.Hide;
          EditToolButtonSave.Show;
          end
  else if (PageControl1.ActivePage=TabSheetSolver)
          or
          (PageControl1.ActivePage=TabSheetOptimizer)
          or
          (PageControl1.ActivePage=TabSheetGenerator)
          then with LevelNamePanel do begin
          if   PageControl1.ActivePage<>TabSheetGenerator then
               Hint:=StrWithQuotedAmpersands(VisualFileName(Editor.FileName))
          else Hint:='';//VisualFileName(MainForm.Generator.FileName);

          Caption:=SPACE+Hint+SPACE; // the first space is for pretty-printing in the panel; the second space is for a better balance in the "mouse-over" tool-tip
          if (Sender=EditMenuItemSaveAs) or (PageControl1.ActivePage=TabSheetGenerator) then begin
             LoadLevelFromEditor(False,False);
             if        PageControl1.ActivePage=TabSheetSolver then
                       OpenForm.EnableDisablePluginControls(Self,MainForm.Solver,False)
             else if   PageControl1.ActivePage=TabSheetOptimizer then
                       OpenForm.EnableDisablePluginControls(Self,MainForm.Optimizer,False)
             else if   PageControl1.ActivePage=TabSheetGenerator then
                       OpenForm.EnableDisablePluginControls(Self,MainForm.Generator,False)
                  else raise Exception.Create('TToolsForm.Save(): Tabsheet not initialized');
             if        PageControl1.ActivePage=TabSheetOptimizer then with OptimizerTaskQueue do
                       OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,b)
             else if   PageControl1.ActivePage=TabSheetSolver then with SolverTaskQueue do
                       SolveLevelsStringGridSelectCell(nil,0,StringGrid.Row,b)
             else if   PageControl1.ActivePage=TabSheetGenerator then with GeneratorTaskQueue do begin
                       GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,b);
                       OpenForm.EnableDisablePluginControls(ToolsForm,MainForm.Generator,True);
                       end;
             ShowStatus;
             end;
          end;
end;

procedure TToolsForm.CaptureFileMenuItemNewSkinClick(Sender: TObject);
begin
  if Assigned( CaptureForm ) then with CaptureForm do begin
     MatchingSkinFileName := '';
     Editor.SkinFileName := '';
     ShowTitle;
     ShowStatus;
     end;
end;

procedure TToolsForm.EditMenuItemUndoClick(Sender: TObject);
var n:Integer;
begin
  //AnimateButtonMouseClick(EditToolButtonUndo);

  HideCursor(False);

  //EditToolButtonUndo.Hide; EditToolButtonUndo.Show;

  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     ( PageControl1.ActivePage = TabSheetEditor ) then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
        if Screen.ActiveForm=Self then begin
           if (Sender is TMenuItem) and (Sender<>EditMenuItemUndo)  then
              with Sender as TMenuItem do
                if   Tag>0 then n:=Min(Tag,Editor.History.Position)
                else n:=Editor.History.Position
           else n:=1;

           if (Editor.History.Position-n>=0) and (n<>0) then with Editor.History do begin
              Modified:=True;
              Position:=Position-n;
              end;
           end;
end;

procedure TToolsForm.EditMenuItemRedoClick(Sender: TObject);
var n:Integer;
begin
  //AnimateButtonMouseClick(EditToolButtonRedo);

  HideCursor(False);

  //EditToolButtonRedo.Hide; EditToolButtonRedo.Show;

  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     ( PageControl1.ActivePage = TabSheetEditor ) then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
        if Screen.ActiveForm=Self then begin
           if (Sender is TMenuItem) and (Sender<>EditMenuItemRedo) then
              with Sender as TMenuItem do
                if   Tag>0 then n:=Min(Tag,Editor.History.Count-Editor.History.Position)
                else n:=Editor.History.Count-Editor.History.Position
           else n:=1;

           if Editor.History.Position+n<=Editor.History.Count then with Editor.History do begin
              Modified:=True;
              Position:=Position+n;
              end;
           end;
end;

procedure TToolsForm.EditMenuItemCutClick(Sender: TObject);
var i:Integer; s:String;
begin
//AnimateButtonMouseClick(EditToolButtonCut);

  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     ( PageControl1.ActivePage <> TabSheetCapture ) then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
        if Screen.ActiveForm=Self then begin
           CloseEditorSelection(True,False);

           if BoardToText(Game.BoardWidth,Game.BoardHeight,Game.Board,CellRectToGameBoardRect(Editor.CursorRect),s,i) then
              try    Clipboard.AsText:=s;
                     EditMenuItemDeleteOrFillOrReplaceClick(Sender);
                     EditToolButtonCut.Hide; EditToolButtonCut.Show; // kludge: clicking a toolbutton and then disabling it leaves the button with a frame
              except on E:Exception do Error(E.Message,Application.Title);
              end;
           end;
end;

procedure TToolsForm.EditMenuItemCopyClick(Sender: TObject);
var i:Integer; s:String;
begin
//AnimateButtonMouseClick(EditToolButtonCopy);

  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     ( PageControl1.ActivePage <> TabSheetCapture ) then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
        if Screen.ActiveForm=Self then begin
           CloseEditorSelection(True,False);

           if BoardToText(Game.BoardWidth,Game.BoardHeight,Game.Board,CellRectToGameBoardRect(Editor.CursorRect),s,i) then
              try    Clipboard.AsText:=s;
                     StatusText:=CopiedSelectionToClipboardText;
              except on E:Exception do Error(E.Message,Application.Title);
              end;
           ShowStatus;
           end;
end;

procedure TToolsForm.EditMenuItemPasteClick(Sender: TObject);
var i,BoardWidth,BoardHeight,NonFloorCellCount:Integer; Result:Boolean;
    s,BoardAsText:String;
    PlayerPos:TPoint; PlayerPos2:TColRow; R:TRect; Board:TBoard; Item:TLevel;
begin
//AnimateButtonMouseClick(EditToolButtonPaste);

  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     ( PageControl1.ActivePage <> TabSheetCapture ) then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else begin
       if (Sender=LevelSetForm) and (LevelSetForm<>nil) then begin
          Result:=False; FillChar(PlayerPos2,SizeOf(PlayerPos2),0);
          Item:=LevelSetForm.Items[LevelSetForm.Grid.Row];

          if (Item<>nil) and
             (Item.BoardAsTextLines<>nil) then begin
             Item.TextLinesToBoard(Board,BoardWidth,BoardHeight,i,i,PlayerPos2,s);
             TrimBoard(0,0,Board,BoardWidth,BoardHeight,PlayerPos2);
             Result:=(BoardWidth>0) and (BoardHeight>0);
             end;

          PlayerPos.X:=PlayerPos2.X; PlayerPos.Y:=PlayerPos.Y;
          end
       else
          Result:=(Screen.ActiveForm=Self) and
                  Clipboard.HasFormat(CF_TEXT) and
                  TextToBoard(Clipboard.AsText,BoardWidth,BoardHeight,PlayerPos,Board);

       if Result and
          BoardToText(BoardWidth,BoardHeight,Board,Classes.Rect(1,1,Succ(BoardWidth),Succ(BoardHeight)),BoardAsText,NonFloorCellCount) and
          Editor.History.BeginTransaction(8*MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT) then begin

          if (Editor.Cursor=ctSelection) and
             (Editor.CursorRect.Left<Editor.CursorRect.Right) and
             (Editor.CursorRect.Top <Editor.CursorRect.Bottom) then begin
             R:=Editor.CursorRect;
             if (Succ(R.Left)=R.Right) and (Succ(R.Top)=R.Bottom) then begin
                R:=Rect(R.Left,R.Top,R.Left+BoardWidth,R.Top+BoardHeight);
                if (R.Left=0) and
                   (Editor.BoardCellOrigin.X<>0) and (BoardWidth <MAX_BOARD_WIDTH ) then with R do begin
                   Inc(Left); Inc(Right);
                   end;
                if (R.Top=0) and
                   (Editor.BoardCellOrigin.Y<>0) and (BoardHeight<MAX_BOARD_HEIGHT) then with R do begin
                   Inc(Top); Inc(Bottom);
                   end;
                end
             else begin
                R:=Rect(R.Left,R.Top,R.Left+Min(BoardWidth,R.Right-R.Left),R.Top+Min(BoardHeight,R.Bottom-R.Top));
                if PlayerPos.X<>0 then with PlayerPos do
                   if (X>R.Right-R.Left) or (Y>R.Bottom-R.Top) then begin
                      X:=0; Y:=0;
                      end;
                end;
             end
          else begin
             R:=Rect(0,0,BoardWidth,BoardHeight);
             if (Editor.BoardCellOrigin.X<>0) and (BoardWidth <MAX_BOARD_WIDTH ) then with R do begin
                Inc(Left); Inc(Right);
                end;
             if (Editor.BoardCellOrigin.Y<>0) and (BoardHeight<MAX_BOARD_HEIGHT) then with R do begin
                Inc(Top); Inc(Bottom);
                end;
             end;

          HideCursor(False);
          if Editor.DrawingTool<>dtSelect then EditMenuItemDrawingToolClick(EditMenuItemSelect);
          HideCursor(False);

          if Editor.History.BeginTransaction(8*MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT) then begin

             if (PlayerPos.X<>0) and (Game.PlayerPos.X<>0) then with Game.PlayerPos do
                with Editor.History do
                  if AddItem(MakeItem(ehaPlayer,X,Y,Game.Board[X,Y],Position)) then begin
                     Dec(Game.Board[X,Y],PLAYER);
                     ShowSquare(Editor.BoardCellOrigin.X+Pred(X),Editor.BoardCellOrigin.Y+Pred(Y));
                     X:=0; Y:=0;
                     end;

             if Editor.History.Transaction.Result then begin

                Editor.CursorRect                 :=R;
                ScrollEditorCursorInView;

                Editor.Selection.BoardAsText      :=BoardAsText;
                Editor.Selection.BoardWidth       :=BoardWidth;
                Editor.Selection.BoardHeight      :=BoardHeight;
                Editor.Selection.Enabled          :=True;
                Editor.Selection.HasBoard         :=True;
                Editor.Selection.NonFloorCellCount:=NonFloorCellCount;
                Editor.Selection.Rect             :=Editor.CursorRect;

                if (Editor.CursorRect.Left  <=0) or
                   (Editor.CursorRect.Top   <=0) or
                   (Editor.CursorRect.Right >=GameViewer.ColCount) or
                   (Editor.CursorRect.Bottom>=GameViewer.RowCount) then begin
                   InitializeGameViewer(False); ShowGameBackground;
                   ShowSquares;
                   end;

                SetCursor(ctSelection,Editor.Selection.Rect);
                end;
             end;

          SetDrawingToolHint;
          ShowStatus;
          end;
       end;
end;

procedure TToolsForm.EditMenuItemDeleteOrFillOrReplaceClick(Sender: TObject);
const REPLACE_OBJECT_TYPES:array[0..5] of Integer = (0,WALL,FLOOR,FLOOR+GOAL,FLOOR+BOX,FLOOR+BOX+GOAL); // menu-item tags must match the indices for these values
var i,C,R,Col,Row,OldSquareValue,NewSquareValue:Integer; Result:Boolean; WorkRect:TRect;
begin
  Result:=False; OldSquareValue:=0;
  if      Sender=EditToolButtonDelete then begin
          //AnimateButtonMouseClick(EditToolButtonDelete);
          end;

  if      (Sender=EditMenuItemFillWithWalls       ) or (Sender=EditPopupMenuItemFillWithWalls       ) then begin
          NewSquareValue:=WALL;
          EditMenuItemFillWithWalls.Checked:=True; EditPopupMenuItemFillWithWalls.Checked:=True;
          end
  else if (Sender=EditMenuItemFillWithFloors      ) or (Sender=EditPopupMenuItemFillWithFloors      ) then begin
          NewSquareValue:=FLOOR;
          EditMenuItemFillWithFloors.Checked:=True; EditPopupMenuItemFillWithFloors.Checked:=True;
          end
  else if (Sender=EditMenuItemFillWithGoals       ) or (Sender=EditPopupMenuItemFillWithGoals       ) then begin
          NewSquareValue:=FLOOR+GOAL;
          EditMenuItemFillWithGoals.Checked:=True; EditPopupMenuItemFillWithGoals.Checked:=True;
          end
  else if (Sender=EditMenuItemFillWithBoxes       ) or (Sender=EditPopupMenuItemFillWithBoxes       ) then begin
          NewSquareValue:=FLOOR+BOX;
          EditMenuItemFillWithBoxes.Checked:=True; EditPopupMenuItemFillWithBoxes.Checked:=True;
          end
  else if (Sender=EditMenuItemFillWithBoxesOnGoals) or (Sender=EditPopupMenuItemFillWithBoxesOnGoals) then begin
          NewSquareValue:=FLOOR+BOX+GOAL;
          EditMenuItemFillWithBoxesOnGoals.Checked:=True; EditPopupMenuItemFillWithBoxesOnGoals.Checked:=True;
          end
  else if (Sender=EditMenuItemFillWithWallBorderOpaque) or (Sender=EditPopupMenuItemFillWithWallBorderOpaque) then begin
          NewSquareValue:=WALL; Sender:=EditMenuItemFillWithWallBorderOpaque;
          EditMenuItemFillWithWallBorderOpaque.Checked:=True; EditPopupMenuItemFillWithWallBorderOpaque.Checked:=True;
          end
  else if (Sender=EditMenuItemFillWithWallBorderTransparent) or (Sender=EditPopupMenuItemFillWithWallBorderTransparent) then begin
          NewSquareValue:=WALL; Sender:=EditMenuItemFillWithWallBorderTransparent;
          EditMenuItemFillWithWallBorderTransparent.Checked:=True; EditPopupMenuItemFillWithWallBorderTransparent.Checked:=True;
          end
  else if (Sender=EditMenuItemReplaceDoIt) then with EditMenuItemReplace do begin
          NewSquareValue:=0;
          for i:=0 to Pred(Count) do with Items[i] do
              if Checked then
                 if      (Tag>0) and (    Tag <=High(REPLACE_OBJECT_TYPES)) then OldSquareValue:=REPLACE_OBJECT_TYPES[Tag]
                 else if (Tag<0) and (Abs(Tag)<=High(REPLACE_OBJECT_TYPES)) then NewSquareValue:=REPLACE_OBJECT_TYPES[Abs(Tag)];
          end
  else    begin NewSquareValue:=FLOOR; Sender:=nil; // 'nil': delete selection
                EditToolButtonDelete.Hide; EditToolButtonDelete.Show; // kludge: clicking a toolbutton and then disabling it leaves the button with a frame
          end;

  if      (Sender<>nil) and (Sender<>EditMenuItemReplaceDoIt) then begin
          if (Sender<>EditMenuItemFillWithWallBorderOpaque)
             and
             (Sender<>EditPopupMenuItemFillWithWallBorderOpaque) then
             EditMenuItemFillWithWallBorderOpaque.Tag:=Ord(False);
          if (Sender<>EditMenuItemFillWithWallBorderTransparent)
             and
             (Sender<>EditPopupMenuItemFillWithWallBorderTransparent) then
             EditMenuItemFillWithWallBorderTransparent.Tag:=Ord(False);
          end;

  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
        if (Screen.ActiveForm=Self) and
           ( PageControl1.ActivePage = TabSheetEditor ) and
           Editor.History.BeginTransaction(8*MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT) then with Editor do
           try
             Result:=True; //' True': commit any transactions generated by the following lines
             if Selection.HasBoard and (Sender=nil) then // 'Sender=nil': the task is to delete the selected area, i.e., the caller isn't one of the 'fill selected area' menu items
                CloseEditorSelection(False,False)
             else with WorkRect do begin
                CloseEditorSelection(True,False);

                if   (Sender<>EditMenuItemReplaceDoIt) or Editor.Selection.Enabled then
                     WorkRect:=CursorRect
                else WorkRect:=Classes.Rect(BoardCellOrigin.X,BoardCellOrigin.Y,BoardCellOrigin.X+Game.BoardWidth,BoardCellOrigin.Y+Game.BoardHeight);

                if Editor.History.BeginTransaction(4*MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT+2) // '+2': maximum number of translations added by 'AddColRowToBoard()'
                   and
                   ((Sender=nil)
                    or
                    (Sender=EditMenuItemReplaceDoIt)
                    or
                    (AddColRowToBoard(Left,Top)
                     and
                     AddColRowToBoard(Right,Bottom)
                    )
                   ) then begin
                   for Col:=Left to Pred(Right) do
                       for Row:=Top to Pred(Bottom) do begin
                           C:=Succ(Col-BoardCellOrigin.X); R:=Succ(Row-BoardCellOrigin.Y);
                           if (Sender=EditMenuItemFillWithWallBorderOpaque)
                              or
                              (Sender=EditMenuItemFillWithWallBorderTransparent)
                              then
                              if   (Col=Left) or (Col=Pred(Right)) or (Row=Top) or (Row=Pred(Bottom)) then
                                   NewSquareValue:=WALL
                              else if   Sender=EditMenuItemFillWithWallBorderOpaque then
                                        NewSquareValue:=FLOOR
                                   else NewSquareValue:=Game.Board[C,R];
                           if (C>=1) and (C<=Game.BoardWidth )
                              and
                              (R>=1) and (R<=Game.BoardHeight)
                              and
                              (Game.Board[C,R]<>NewSquareValue)
                              and
                              ((Sender<>EditMenuItemReplaceDoIt)
                               or
                               (Game.Board[C,R]=OldSquareValue)
                               or
                               ((Game.Board[C,R]=0)
                                and
                                (OldSquareValue=FLOOR)
                               )
                              )
                              and
                              Result then with History do
                              if   ((Game.Board[C,R]=FLOOR)
                                    or
                                    History.AddItem(History.MakeItem(ehaErase,C,R,Game.Board[C,R],History.Position))
                                   )
                                   and
                                   ((NewSquareValue=FLOOR)
                                    or
                                    History.AddItem(History.MakeItem(ehaSquare,C,R,NewSquareValue,History.Position))
                                   ) then
                                   SetGameBoardSquare(C,R,NewSquareValue)
                              else Result:=False;
                           end;
                   end;
                end;

             Editor.Selection.Enabled:=False;
             with CursorRect do SetCursor(Cursor,Rect(Left,Top,Succ(Left),Succ(Top)));

             if CalculateGameInformation(True) or (Sender=EditMenuItemReplaceDoIt) then
                UpdateView;

           finally Editor.History.EndTransaction(Result);
                   SetDrawingToolHint;
                   ShowStatus;
           end;
end;

procedure TToolsForm.EditMenuItemFillSelectedArea(
  Sender: TObject);
begin
  if Sender is TMenuItem then begin
     EditMenuItemDeleteOrFillOrReplaceClick(Sender); // reuse the 'Delete' function to fill the selected area
     end;
end;

procedure TToolsForm.EditMenuItemReplaceClick(Sender: TObject);
var i:Integer; oIsBusy:Boolean; AcceleratorCharEdit, AcceleratorCharReplace:Char;
begin
  if Assigned(Sender) and (Sender is TMenuItem) and
     Assigned(Game) and
     (not Game.IsBusy) and
     ( PageControl1.ActivePage = TabSheetEditor ) then with Sender as TMenuItem do
     if (Sender=EditMenuItemReplaceDoIt) or (Sender=EditPopupMenuItemReplaceDoIt) then begin
        EditMenuItemDeleteOrFillOrReplaceClick(EditMenuItemReplaceDoIt);
        end
     else begin
        if  Tag<>0 then Checked:=True;
        for i:=0 to Pred(EditPopupMenuItemReplace.Count) do
            if (Sender=EditPopupMenuItemReplace.Items[i]) and
               (i<EditMenuItemReplace.Count) then begin
               if Tag<>0 then EditMenuItemReplace.Items[i].Checked:=True; // transfer the selection from the popup menu to the main menu
               Sender:=nil; // 'nil': used here to indicate that the sender is the popup menu
               end;

       ShowStatusEditor;

       StrHasAcceleratorChar(EditMenuEdit.Caption,True,AcceleratorCharEdit);
       if   Assigned(Sender) then
            StrHasAcceleratorChar(EditMenuItemReplace     .Caption,True,AcceleratorCharReplace)
       else StrHasAcceleratorChar(EditPopupMenuItemReplace.Caption,True,AcceleratorCharReplace);
       oIsBusy:=Game.IsBusy;
       if ((AcceleratorCharEdit<>NULL_CHAR) or (not Assigned(Sender)))
          and
          (AcceleratorCharReplace<>NULL_CHAR) then
          try
            Game.IsBusy:=True;
            if Assigned(Sender) or True then begin // always use the main menu; re-opening the 'Replace' sub-menu in the popup menu doesn't work
               keybd_event( VK_MENU,                        MapVirtualKey( VK_MENU,                     0), 0, 0 );
               IgnoreKeyUp:=True;
               keybd_event( Ord(AcceleratorCharEdit),       MapVirtualKey( Ord(AcceleratorCharEdit   ), 0), 0, 0 );
               end
            else with Editor.EditPopupMenuRightClickPosition do begin
                 EditPopupMenuRightClick.Popup(X,Y); // open the menu itself works all right, but the first time it is opened here, the following keyboard events 'AcceleratorCharReplace' don't open the 'Replace' sub-menu; the reason is unknown
                 end;
            keybd_event   ( Ord(AcceleratorCharReplace),    MapVirtualKey( Ord(AcceleratorCharReplace), 0), 0, 0 );
            IgnoreKeyUp:=True;
            keybd_event   ( Ord(AcceleratorCharReplace),    MapVirtualKey( Ord(AcceleratorCharReplace), 0), KEYEVENTF_KEYUP, 0 );

            if Assigned(Sender) or True then begin
               keybd_event( Ord(AcceleratorCharEdit),       MapVirtualKey( Ord(AcceleratorCharEdit),    0), KEYEVENTF_KEYUP, 0 );
               IgnoreKeyUp:=True;
               keybd_event( VK_MENU,                        MapVirtualKey( VK_MENU,                     0), KEYEVENTF_KEYUP, 0 );
               end
            else begin
               end;
          finally Game.IsBusy:=oIsBusy; IgnoreKeyUp:=False;
          end;
       end;
end;

procedure TToolsForm.EditMenuItemNormalizeBoardClick(
  Sender: TObject);
var H,W,Col,Row:Integer; Result:Boolean; s:String; P:TPoint; P1:TColRow; B:TBoard; His:THistory;
begin
  if (Sender is TMenuItem) and TMenuItem(Sender).Enabled and
     (Screen.ActiveForm=Self) and ( PageControl1.ActivePage = TabSheetEditor ) then with Game do begin
     if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
        if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
        else begin
          if (not Editor.Selection.HasBoard) and
             (Editor.Cursor=ctSelection) and Editor.Selection.Enabled then
             CutSelectionToEditorClipboard;

          if Editor.Selection.HasBoard then begin // operate on selection
             if TextToBoard(Editor.Selection.BoardAsText,W,H,P,B) then begin
                Result:=SokFile_.NormalizeBoard(EditMenuItemMovePlayerAndBoxes.Checked,True,True,True,True,W,H,B,P1,His)<>0;
                if (Sender=EditMenuItemNormalizeBoardMakeRectangularBoard) and (MakeBoardRectangular(W,H,B)<>0) then
                   Result:=True;
                if Result then begin //'True': the board has changed
                   if BoardToText(W,H,B,Rect(1,1,Succ(W),Succ(H)),s,Editor.Selection.NonFloorCellCount) then begin
                      with Editor.CursorRect do
                        Editor.CursorRect             :=Rect(Left,Top,Left+W,Top+H);
                      ScrollEditorCursorInView;

                      Editor.Selection.BoardAsText    :=s;
                      Editor.Selection.BoardWidth     :=W;
                      Editor.Selection.BoardHeight    :=H;
                      Editor.Selection.HasBoard       :=True;
                      Editor.Selection.Rect           :=Editor.CursorRect;

                      with Editor.Selection.Rect do
                        if (Left  <=0) or
                           (Top   <=0) or
                           (Right >=GameViewer.ColCount) or
                           (Bottom>=GameViewer.RowCount) then begin
                           InitializeGameViewer(False); ShowGameBackground;
                           ShowSquares;
                           end;

                      SetCursor(Editor.Cursor,Editor.Selection.Rect);
                      end;
                   end;
                end;

             EditMenuItemRefreshClick(nil);
             end
          else begin // operate on the entire board
             CloseEditorSelection(True,False);
             B:=Game.Board; W:=Game.BoardWidth; H:=Game.BoardHeight;
             Result:=SokFile_.NormalizeBoard(EditMenuItemMovePlayerAndBoxes.Checked,True,True,True,True,W,H,B,P1,His)<>0;
             if (Sender=EditMenuItemNormalizeBoardMakeRectangularBoard) and (MakeBoardRectangular(W,H,B)<>0) then
                Result:=True;
             if Result and // 'Result' = 'True': the board has changed
                Editor.History.BeginTransaction(2*MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT+1) then with Editor do
                try
                   with Game.PlayerPos do
                     if (X<>0) and (Y<>0) and ((Game.Board[X,Y] and PLAYER)<>0) and ((B[X,Y] and PLAYER)=0) then begin
                        // the player has changed position; delete it now;
                        // otherwise the transaction risks getting lost if the player is moved by
                        // 'SetGameBoardSquare' inside the 'FOR col:=... FOR row:=...' loop below;
                        Result:=History.AddItem(History.MakeItem(ehaErase,X,Y,Game.Board[X,Y],History.Position));
                        if Result then SetGameBoardSquare(X,Y,Game.Board[X,Y] and (BOARD_PIECES-PLAYER));
                        end;

                   W:=Game.BoardWidth; H:=Game.BoardHeight; // take a snaphot of the old board dimensions; if the board has been trimmed, then its dimensions may shrink during the transfer of the new board 'B' to 'Game.Board'
                   for Col:=W downto 1 do
                       for Row:=H downto 1 do begin
                           if ((Game.Board[Col,Row] and BOARD_PIECES)<>(B[Col,Row] and BOARD_PIECES)) and
                              Result then begin
                              if   ((Game.Board[Col,Row] and BOARD_PIECES)=FLOOR)
                                   or
                                   History.AddItem(History.MakeItem(ehaErase,Col,Row,Game.Board[Col,Row],History.Position))
                                   then begin
                                   //if   (Col<=W) and (Row<=H) then begin
                                        if   ((B[Col,Row] and BOARD_PIECES)=FLOOR)
                                             or
                                             History.AddItem(History.MakeItem(ehaSquare,Col,Row,B[Col,Row] and BOARD_PIECES,History.Position)) then
                                             SetGameBoardSquare(Col,Row,B[Col,Row] and BOARD_PIECES)
                                        else Result:=False
                                        //end
                                   //else SetGameBoardSquare(Col,Row,FLOOR);
                                   end
                              else Result:=False;
                              end;
                           end;
                   Editor.Selection.Enabled:=False;
                   if   CalculateGameInformation(True) then
                        UpdateView;

                finally Editor.History.EndTransaction(Result);
                        SetDrawingToolHint;
                        EditMenuItemRefreshClick(Self);
                        Modified:=True; ShowStatus;
                end;
             end;
          end;
     end;
end;

procedure TToolsForm.EditMenuItemSelectAllClick(Sender: TObject);
begin
  if (Screen.ActiveForm=Self) and
     (Game.BoardWidth<>0) and
     (Game.BoardHeight<>0) and
     ( PageControl1.ActivePage = TabSheetEditor ) then with Editor do begin
     HideCursor(False);
     EditMenuItemDrawingToolClick(EditMenuItemSelectAll);
     end;
end;

procedure TToolsForm.EditMenuItemCopyToInternalClipboardClick(
  Sender: TObject);
begin //
  if (Screen.ActiveForm=Self) and
     OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors and
     ( PageControl1.ActivePage = TabSheetEditor ) and
     (LevelSetForm<>nil) then begin
     EditMenuItemInternalClipboardClick(Sender);
     if LevelSetForm.Visible then begin
        if  Screen.ActiveForm<>LevelSetForm then LevelSetForm.SetFocus;
        LevelSetForm.PanelNewClick(Sender);
        end;
     end;

end;

procedure TToolsForm.EditToolButtonNewLevelWithWallsClick(Sender: TObject);
begin
  if (Screen.ActiveForm=Self) and
     (PageControl1.ActivePage=TabSheetEditor) and
     OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors then
     MakeNewLevel(EditToolButtonNewLevelWithWalls);
end;

procedure TToolsForm.ToolButtonNewLevelWithWallsWidthAndHeightClick(
  Sender: TObject);
var ToolButton:TToolButton;
begin
  if Sender=ToolButtonNewLevelWithWallsWidth then begin
     EditPopupMenuNewLevelWithWallsWidthAndHeight.Tag:=0;
     ToolButton:=ToolButtonNewLevelWithWallsWidth;
     end
  else begin
     EditPopupMenuNewLevelWithWallsWidthAndHeight.Tag:=1;
     ToolButton:=ToolButtonNewLevelWithWallsHeight;
     end;
  with ClientToScreen( Point( TabsheetEditor.Left + EditToolBarTop.Left + ToolButton.Left, TabsheetEditor.Top + EditToolBarTop.Top + ToolButton.Top + ToolButton.Height + 2 ) ) do
    EditPopupMenuNewLevelWithWallsWidthAndHeight.Popup(X,Y);
end;

procedure TToolsForm.EditPopupMenuNewLevelWithWallsWidthAndHeightPopup(
  Sender: TObject);
var i,m,n,Value:Integer; MenuItem:TMenuItem;
begin
  with EditPopupMenuNewLevelWithWallsWidthAndHeight do begin
    if Tag=0 then begin
       m:=MAX_BOARD_WIDTH-2; // -2: wall border
       n:=GENERATOR_BOARD_REGION_WIDTH;
       Value:=ToolButtonNewLevelWithWallsWidth.Tag;
       end
    else begin
       m:=MAX_BOARD_HEIGHT-2; // -2: wall border;
       n:=GENERATOR_BOARD_REGION_HEIGHT;
       Value:=ToolButtonNewLevelWithWallsHeight.Tag;
       end;

    while Items.Count<(m div n) do
          try    MenuItem:=TMenuItem.Create(Self);
                 MenuItem.Tag:=Succ(Items.Count)*n;
                 MenuItem.Caption:=IntToStr(MenuItem.Tag);
                 MenuItem.OnClick:=Self.EditPopupMenuNewLevelWithWallsWidthAndHeightItemClick;
                 MenuItem.Visible:=True;
                 Items.Insert(Items.Count,MenuItem);
          except on E:Exception do begin
                    m:=0; Error(E.Message,Application.Title);
                    end;
          end;

    for i:=0 to Pred(Items.Count) do Items[i].Checked:=Items[i].Tag=Value;
    end;
end;

procedure TToolsForm.ToolButtonNewLevelWithWallsWidthAndHeightMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FormMouseMove(Sender,Shift,X,Y);
  if   Sender=ToolButtonNewLevelWithWallsWidth then
       EditPopupMenuNewLevelWithWallsWidthAndHeight.Tag:=0
  else EditPopupMenuNewLevelWithWallsWidthAndHeight.Tag:=1;
end;

procedure TToolsForm.EditPopupMenuNewLevelWithWallsWidthAndHeightItemClick(Sender: TObject);
var n:Integer;
begin
  HideCursor(False);
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     ( PageControl1.ActivePage = TabSheetEditor ) then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
        if Screen.ActiveForm=Self then
           if Sender is TMenuItem then
              with Sender as TMenuItem do begin
                n:=Tag;
                if   EditPopupMenuNewLevelWithWallsWidthAndHeight.Tag=0 then
                     ToolButtonNewLevelWithWallsWidth .Tag:=n
                else ToolButtonNewLevelWithWallsHeight.Tag:=n;
                end;
end;

procedure TToolsForm.HelpMenuClick(Sender: TObject);
begin
//AnimateButtonMouseClick(EditToolButtonHelp);
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then begin

     {$IFDEF YASC}
       MainForm.BtnHelpClick(Sender);
     {$ELSE}
       if Screen.ActiveForm=Self then
          Msg('Sokoban Editor - Development version 0.01'+NL+
              '(c) 2005 by Brian Damgaard'+NL+NL+
              'Limitations in this development version:'+NL+
              '"Open" only loads the first level in a collection.',
              Application.Title,
              MB_OK+MB_ICONINFORMATION);
     {$ENDIF}
     end;
end;

procedure TToolsForm.SettingsMenuItemBackgroundClick(Sender: TObject);
var DefaultBackgroundFileName,oFileName:String;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else with OpenPictureDialog1 do begin
       PluginToolButtonStopReplayClick(PluginToolButtonReplay);
       Title:=Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+BackgroundText;
       DefaultExt := Copy(JPG_FILE_EXT,2,MaxInt);
       Filter := IMAGE_FILES_FILTER;

       DefaultBackgroundFileName:=MainForm.ApplicationDataPath+DEFAULT_VALUE+JPG_FILE_EXT;
       if (Sender<>nil) then
          try    if FileExists(DefaultBackgroundFileName) then DeleteFile(DefaultBackgroundFileName);
                 if not FileExists(DefaultBackgroundFileName) then
                    if not ResourceSaveToFile(DefaultBackgroundFileName,BACKGROUND_RES_NAME,RC_JPG) then
                       raise Exception.Create(Format(SaveFileFailedText__,[DefaultBackgroundFileName]));
          except on E:Exception do Error(E.Message,Caption);
          end;

       if   GameViewer.BackgroundPict.FileName='' then
            FileName:=DefaultBackgroundFileName
       else FileName:=GameViewer.BackgroundPict.FileName;
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
           StrEqual(FileName,DefaultBackgroundFileName)
          ) then begin
          Refresh;
          if StrEqual(FileName,DefaultBackgroundFileName) then
             SettingsMenuItemDefaultBackgroundClick(Sender)
          else
                try     try    if GameViewer.BackgroundPict.LoadFromFile(FileName) then begin
                                  OptionsForm.SettingsString[stGraphicsToolsBoardBackgroundImageFileName]:=GameViewer.BackgroundPict.FileName;
                                  if StrEqual(oFileName,DefaultBackgroundFileName) then
                                     GameViewer.BackgroundPict.AntiAliasing:=aaFilter;
                                  GameViewer.BackgroundPict.Visible:=True;

                                  with GameViewer.BackgroundPict do
                                    if OrgBitMap=nil then MakeOrgBitMapFromPict;

                                  if (LevelSetForm<>nil) and (LevelSetForm.GameViewer.BackgroundPict<>nil) then begin
                                      LevelSetForm.GameViewer.BackgroundPict.OrgBitMap:=nil;
                                      if LevelSetForm.Visible then
                                         LevelSetForm.ShowItem(Max(LevelSetForm.Grid.Row,LevelSetForm.Grid.FixedRows));
                                      if (PageControl1.ActivePage=TabSheetSolver) or (PageControl1.ActivePage=TabSheetOptimizer) then with PluginLevelInfo do with BoardAsText do
                                         LevelSetForm.ShowBoard1(Width,Height,Board,SelectedSquaresAsText,ImageBoard);
                                      end;
                                  end
                               else begin
                                  Error(TEXT_TASK_FAILED,Application.Title);
                                  SettingsMenuItemDefaultBackgroundClick(Sender);
                                  end;
                        except on E:Exception do begin
                                  Error(E.Message,Caption);
                                  end;
                        end;
                finally GameViewer.BackgroundInitialized:=False;
                        InitializeGameViewer(True);
                        Self.ShowGame(True);
                end;
          end;
       if Sender<>nil then DeleteFile(DefaultBackgroundFileName);
       end;
end;

procedure TToolsForm.SettingsMenuItemDefaultBackgroundClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then begin
     PluginToolButtonStopReplayClick(PluginToolButtonReplay);
     if Sender<>nil then Refresh;
     if GameViewer.BackgroundPict<>nil then with GameViewer.BackgroundPict do begin
        if   LoadFromResource(BACKGROUND_RES_NAME,RC_JPG) then
        else Clear;
        end;
     GameViewer.BackgroundPict.AntiAliasing:=DEFAULT_BACKGROUND_ANTI_ALIASING;
     GameViewer.BackgroundPict.Color:=DEFAULT_PICTURE_COLOR[ptScreenBackground];
     GameViewer.BackgroundPict.View:=ivFill;
     GameViewer.BackgroundPict.FileName:='';
     OptionsForm.SettingsString[stGraphicsToolsBoardBackgroundImageFileName]:=DEFAULT_VALUE;
     GameViewer.BackgroundPict.Visible:=True;
     GameViewer.BackgroundInitialized:=False;
     InitializeGameViewer(True);
     Self.ShowGame(True);

     if (LevelSetForm<>nil) and (LevelSetForm.GameViewer.BackgroundPict<>nil) then begin
        LevelSetForm.GameViewer.BackgroundPict.OrgBitMap:=nil;
        if LevelSetForm.Visible then
           LevelSetForm.ShowItem(Max(LevelSetForm.Grid.Row,LevelSetForm.Grid.FixedRows));
        if ((PageControl1.ActivePage=TabSheetSolver) or (PageControl1.ActivePage=TabSheetOptimizer)) and
           (LevelSetForm<>nil) then with PluginLevelInfo do with BoardAsText do
           LevelSetForm.ShowBoard1(Width,Height,Board,SelectedSquaresAsText,ImageBoard);
        end;
     end;
end;

function  TToolsForm.SaveDefaultButtonsToFile(var DefaultButtonsFileName__:String):Boolean;
var B:TBitMap;
begin
  Result:=True;
  DefaultButtonsFileName__:=MainForm.ApplicationDataPath+DEFAULT_VALUE+BMP_FILE_EXT;
  try    if      FileExists(DefaultButtonsFileName__) then DeleteFile(DefaultButtonsFileName__);
         if      (EditImageList1.Count=0) and
                 (not BitMapToImageList(EditImageDefaultButtons.Picture.BitMap,EditImageDefaultButtons.Picture.BitMap,EditImageList1)) then
                 raise Exception.Create(TEXT_TASK_FAILED)
         else if (not FileExists(DefaultButtonsFileName__)) and
                 ImageListToBitMap(EditImageList1,B) then
                 try     B.SaveToFile(DefaultButtonsFileName__);
                 finally B.Free;
                 end;
  except on E:Exception do Result:=Error(E.Message,Caption);
  end;
end;

procedure TToolsForm.SettingsMenuItemButtonsClick(Sender: TObject);
var oWidth:Integer; DefaultButtonsFileName,oFileName:String; R:TRect; B:TBitMap;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else with OpenPictureDialog1 do begin
       Title:=Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+ButtonsText;
       DefaultExt := GraphicExtension(TBitmap);
       Filter := GraphicFilter(TBitmap);

       DefaultButtonsFileName:=MainForm.ApplicationDataPath+DEFAULT_VALUE+BMP_FILE_EXT;
       try
         if Sender<>nil then SaveDefaultButtonsToFile(DefaultButtonsFileName);

         if   ButtonsFileName='' then
              FileName:=DefaultButtonsFileName
         else FileName:=ButtonsFileName;
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
             StrEqual(FileName,DefaultButtonsFileName)
            ) then begin
            Refresh;
            if StrEqual(FileName,DefaultButtonsFileName) then
               SettingsMenuItemDefaultButtonsClick(Sender)
            else
               if BitMapCreate(B,1,1) then
                  try     try    B.LoadFromFile(FileName);
                                 if (B.Height=EditImageDefaultButtons.Picture.BitMap.Height) then begin
                                    if B.Width<EditImageDefaultButtons.Picture.BitMap.Width then begin
                                       oWidth:=B.Width;
                                       if BitMapResize(B,EditImageDefaultButtons.Picture.BitMap.Width,B.Height) then with B.Canvas do begin
                                          // fill up with with default button images
                                          CopyMode:=cmSrcCopy; R:=Rect(oWidth,0,B.Width,B.Height);
                                          CopyRect(R,EditImageDefaultButtons.Picture.BitMap.Canvas,R);
                                          end;
                                       end;

                                    if BitMapToImageList(B,EditImageDefaultButtons.Picture.BitMap,EditImageList1) then begin
                                       ButtonsFileName:=FileName;
                                       OptionsForm.SettingsString[stGraphicsToolsButtonsImageFileName]:=ButtonsFileName;
                                       end
                                    else begin
                                       Error(TEXT_TASK_FAILED,Application.Title);
                                       SettingsMenuItemDefaultButtonsClick(Sender);
                                       end;
                                    end
                                 else with EditImageDefaultButtons.Picture.BitMap do begin
                                    Msg(Format(FileNotALegalImageListText__,[FileName,Height,Height]),Caption,MB_OK+MB_ICONINFORMATION);
                                    SettingsMenuItemDefaultButtonsClick(Sender);
                                    end;

                          except on E:Exception do Error(E.Message,Caption);
                          end;
                  finally B.Free; Self.Refresh;
                  end;
            end;
       finally
         if Sender<>nil then DeleteFile(DefaultButtonsFileName);
       end;
       end;
end;

procedure TToolsForm.SettingsMenuItemDefaultButtonsClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else begin
       ButtonsFileName:='';
       OptionsForm.SettingsString[stGraphicsToolsButtonsImageFileName]:=DEFAULT_VALUE;
       if not BitMapToImageList(EditImageDefaultButtons.Picture.BitMap,EditImageDefaultButtons.Picture.BitMap,EditImageList1) then
          Error(TEXT_TASK_FAILED,Application.Title);
       if Sender<>nil then Refresh;
       end;
end;

procedure TToolsForm.SettingsMenuItemSkinClick(Sender: TObject);
const MIN_TILE_WIDTH=16; MIN_TILE_HEIGHT=MIN_TILE_WIDTH;
var DefaultSkinFileName,oFileName:String; B:TBitMap;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else with OpenPictureDialog1 do begin
       PluginToolButtonStopReplayClick(PluginToolButtonReplay);
       Title:=Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+SkinText;
       DefaultExt := GraphicExtension(TBitmap);
       Filter := GraphicFilter(TBitmap);

       DefaultSkinFileName:=StrWithTrailingPathDelimiter(ToolsSkinPath)+DEFAULT_VALUE+BMP_FILE_EXT;
       try
         if (Sender<>nil) then
            try    if FileExists(DefaultSkinFileName) then DeleteFile(DefaultSkinFileName);
                   if not FileExists(DefaultSkinFileName) then
                      EditImageDefaultSkin.Picture.BitMap.SaveToFile(DefaultSkinFileName);
            except on E:Exception do Error(E.Message,Caption);
            end;

         if   GameViewer.SkinPict.FileName='' then
              FileName:=DefaultSkinFileName
         else FileName:=GameViewer.SkinPict.FileName;
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
                                    (B.Width >=MIN_TILE_WIDTH*TOOLS_SKIN_TILE_COUNT) and
                                    (B.Width=TOOLS_SKIN_TILE_COUNT*B.Height) then begin
                                    if LoadSkin(FileName,B,TOOLS_SKIN_TILE_COUNT,GameViewer.SkinPict.Antialiasing,GameViewer.SkinPict.Masked,RGBToColor(GameViewer.SkinPict.MaskBitMapColor),GameViewer.SkinPict.MaskBitMapPct) then begin
                                       if (LevelSetForm<>nil) and LevelSetForm.Visible then
                                          LevelSetForm.ShowItem(Max(LevelSetForm.Grid.Row,LevelSetForm.Grid.FixedRows));
                                       if ((PageControl1.ActivePage=TabSheetSolver) or (PageControl1.ActivePage=TabSheetOptimizer)) and
                                          (LevelSetForm<>nil) then with PluginLevelInfo do with BoardAsText do begin
                                          LevelSetForm.ShowBoard1(Width,Height,Board,SelectedSquaresAsText,ImageBoard);
                                         end;
                                       end
                                    else begin
                                       Error(TEXT_TASK_FAILED,Application.Title);
                                       SettingsMenuItemDefaultSkinClick(Sender);
                                       end;
                                    end
                                 else begin
                                    Msg(Format(FileNotALegalToolsSkin1Text__+Copy(FileNotALegalToolsSkin2Text__,1,MaxInt),[MIN_TILE_WIDTH,MIN_TILE_HEIGHT,FileName]),Caption,MB_OK+MB_ICONINFORMATION);
                                    SettingsMenuItemDefaultSkinClick(Sender);
                                    end;
                          except on E:Exception do Error(E.Message,Caption);
                          end;
                  finally if   (GameViewer.SkinPict<>nil) and
                               (GameViewer.SkinPict.OrgBitMap=B) then // the bitmap was imported; don't destroy it
                          else B.Free;
                          InitializeGameViewer(True);
                          Self.ShowGame(True);
                  end;
            end;
       finally
         if Sender<>nil then DeleteFile(DefaultSkinFileName);
       end;
       end;
end;

procedure TToolsForm.SettingsMenuItemDefaultSkinClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else begin
       PluginToolButtonStopReplayClick(PluginToolButtonReplay);
       LoadDefaultSkin;
       InitializeGameViewer(True);
       Self.ShowGame(True);
       if (LevelSetForm<>nil) and LevelSetForm.Visible then
          LevelSetForm.ShowItem(Max(LevelSetForm.Grid.Row,LevelSetForm.Grid.FixedRows));
       if ((PageControl1.ActivePage=TabSheetSolver) or (PageControl1.ActivePage=TabSheetOptimizer)) and
          (LevelSetForm<>nil) then with PluginLevelInfo do with BoardAsText do
          LevelSetForm.ShowBoard1(Width,Height,Board,SelectedSquaresAsText,ImageBoard);
       end;
end;

procedure TToolsForm.SettingsMenuItemMoreSettingsClick(Sender: TObject);
var oInternalClipboardVisible:Boolean; A,B:Options_.TSettings;
    oCursor:TCursor; oWindowState:TWindowState;
    ABoolean:Boolean; SerialNo:TSerialNo;
    CurrentLevel:TLevel; CurrentTaskQueue:TTaskQueue;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then begin
     PluginToolButtonStopReplayClick(PluginToolButtonReplay);
     PluginLevelInfo.ReplayInfo.IsLoaded:=False;
     OptimizationComboBox.Clear;

     Refresh;

     oWindowState:=Self.WindowState;
     if WindowState<>wsNormal then WindowState:=wsNormal;

     with OptionsForm.TreeView1 do begin
       if Self.PageControl1.ActivePage=TabSheetSolver then begin
          A:=stToolsSolver;
          B:=stToolsSolverPriority;
          Items[Ord(stToolsSolverTimeLimit)].Expand(False);
          end
       else if Self.PageControl1.ActivePage=TabSheetOptimizer then begin
          A:=stToolsOptimizer;
          B:=stToolsOptimizerPriority;
          Items[Ord(stToolsOptimizerTimeLimit)].Expand(False);
          end
       else if Self.PageControl1.ActivePage=TabSheetGenerator then begin
          A:=stToolsGenerator;
          B:=stToolsGeneratorPriority;
          end
       else if Self.PageControl1.ActivePage=TabSheetCapture then begin
          A:=stToolsCapture;
          B:=stToolsCaptureGridColor;
          end
       else begin
          A:=stGraphicsTools;
          B:=stGraphicsToolsInternalClipboardTextsClipboardItemName;
          end;

       if (Selected=nil)
          or
          (TSettings(Selected.AbsoluteIndex)<A)
          or
          (TSettings(Selected.AbsoluteIndex)>B)
          then begin
          Selected:=Items[Ord(A)];
          Selected:=Items[Ord(A)];
          Selected.Expand(False);
          end;
       end;

     oInternalClipboardVisible:=(LevelSetForm<>nil) and LevelSetForm.Visible;
     if oInternalClipboardVisible then LevelSetForm.Hide;

     if      (PageControl1.ActivePage<>TabSheetSolver) and
             (PageControl1.ActivePage<>TabSheetOptimizer) and
             (PageControl1.ActivePage<>TabsheetGenerator) then
              LoadLevelFromEditor(True,True); // so 'PluginLevelInfo' is correct in case the user tries to select or launch a solver or an optimizer from the 'Open' window

     // get current item on the task queue if this is the solver or optimizer tabsheet
     CurrentTaskQueue:=nil; SerialNo:=0;
     if      PageControl1.ActivePage=TabSheetOptimizer then CurrentTaskQueue:=OptimizerTaskQueue
     else if PageControl1.ActivePage=TabSheetSolver    then CurrentTaskQueue:=SolverTaskQueue;
     if Assigned(CurrentTaskQueue) then with CurrentTaskQueue do begin
        Plugin.Enter;
        try     CurrentLevel:=Levels[StringGrid.Row];
                if   Assigned(CurrentLevel)
                     and
                     (not CurrentLevel.SnapshotsAsText.IsEmpty)
                     and
	             (CurrentLevel.SnapshotsAsText.Last is TExtendedSnapshotAsText)
                     and
                     (not TSnapshotAsText(CurrentLevel.SnapshotsAsText.Last).MovesAsTextLines.IsEmpty)
                     and
                     (TSnapshotAsText(CurrentLevel.SnapshotsAsText.Last).MovesAsTextLines.First.Text<>'') then
                     SerialNo:=TExtendedSnapshotAsText(CurrentLevel.SnapshotsAsText.Last).SerialNo
                else SerialNo:=0;
        finally Plugin.Leave;
        end;
        end;

     try     MainForm.BtnOptionsClick(Self);
     finally SetMessageHandlers;
             Game.IsBusy:=False;
             with OpenForm.Game do begin IsBusy:=False; IsReplaying:=False; IsBrowsing:=False; end;
             GameViewer.BoardDimensionsAsText:=MainForm.BoardDimensionsAsText;

             oCursor:=Screen.Cursor;
             try
               Screen.Cursor:=crHourGlass;

               if not Initialized then
                  FormActivate(nil)
               else begin
                  InitializeGameViewer(False);
                  Self.ShowGame(True);
                  end;

               if oInternalClipboardVisible and (not LevelSetForm.Visible) then
                  try     LevelSetForm.Show;
                  finally SetMessageHandlers;
                          SetFocus;
                  end;
             finally
               Screen.Cursor:=oCursor;
               if WindowState<>oWindowState then begin
                  WindowState:=oWindowState; Update;
                  end;
               if MainForm.ShutDownApplication then
                  try     BtnOKClick(Sender);
                  finally BtnCancelClick(Sender); // ensure that the window closes
                  end
               else begin
                  // focus the old current item on the task queue if this is the solver or optimizer tabsheet
                  if Assigned(CurrentTaskQueue) and (SerialNo<>0) then with CurrentTaskQueue do begin
                     Plugin.Enter;
                     try     CurrentLevel:=Plugin.SokoFile.GetLevelByExtendedSnapshotAsTextSerialNo(SerialNo);
                             if   Assigned(CurrentLevel) and (CurrentLevel=Levels[CurrentLevel.Flags]) then begin
                                  StringGrid.Row:=CurrentLevel.Flags;
                                  ScrollInView(StringGrid.Row);
                                  if      Plugin is TOptimizerPlugin then
                                          OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,ABoolean) // ends with a call to 'ShowStatus', hence, there is no reason to do it again here
                                  else if Plugin is TSolverPlugin then
                                          SolveLevelsStringGridSelectCell(nil,0,StringGrid.Row,ABoolean) // ends with a call to 'ShowStatus', hence, there is not reason to do it again here
                                  else if (Plugin is TGenerator) and (PageControl1.ActivePage=TabSheetGenerator) then
                                          GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,ABoolean); // ends with a call to 'ShowStatus', hence, there is not reason to do it again here
                                  end
                             else ShowStatus;
                    finally Plugin.Leave;
                    end;
                    end;
                  end;

               OpenForm.PluginTimer.Enabled:=(Assigned(MainForm.Solver   ) and (MainForm.Solver   .StartTimeMS<>0))
                                             or
                                             (Assigned(MainForm.Optimizer) and (MainForm.Optimizer.StartTimeMS<>0))
                                             or
                                             (Assigned(MainForm.Generator) and (MainForm.Generator.StartTimeMS<>0));
             end;
     end;
     end;
end;

procedure TToolsForm.EditMenuItemRotateClick(Sender: TObject);
var i,j,H,W,Col,Row:Integer; s:String; Transformation2D:TTransformation2D;
    P:TPoint; B,B2:TBoard; Item:TEditorHistoryItem; ToolButton:TToolButton;
begin
  if  (Sender=EditMenuItemRotateClockwise) or
      (Sender=EditToolButtonRotateClockwise) then begin
      ToolButton:=EditToolButtonRotateClockwise;
      Transformation2D:=t2DRotate90DegreesClockwise;
      end
  else begin
      ToolButton:=EditToolButtonRotateCounterClockwise;
      Transformation2D:=t2DRotate270DegreesClockwise;
      end;

  if ToolButton.Enabled then with Game do begin
     //AnimateButtonMouseClick(ToolButton);

     if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
        if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
        else begin
          if (not Editor.Selection.HasBoard) and
             (Editor.Cursor=ctSelection) and Editor.Selection.Enabled then
             CutSelectionToEditorClipboard;

          if Editor.Selection.HasBoard then begin // operate on selection
             if TextToBoard(Editor.Selection.BoardAsText,W,H,P,B2) then begin
                for Col:=1 to W do
                    for Row:=1 to H do begin
                        CalculateTransformation2D(Transformation2D,Col,Row,W,H,i,j);
                        B[i,j]:=B2[Col,Row];
                        end;
                i:=W; W:=H; H:=i;

                if BoardToText(W,H,B,Rect(1,1,Succ(W),Succ(H)),s,Editor.Selection.NonFloorCellCount) then begin
                   with Editor.CursorRect do
                     Editor.CursorRect             :=Rect(Left,Top,Left+W,Top+H);
                   ScrollEditorCursorInView;

                   Editor.Selection.BoardAsText    :=s;
                   Editor.Selection.BoardWidth     :=W;
                   Editor.Selection.BoardHeight    :=H;
                   Editor.Selection.HasBoard       :=True;
                   Editor.Selection.Rect           :=Editor.CursorRect;

                   with Editor.Selection.Rect do
                     if (Left  <=0) or
                        (Top   <=0) or
                        (Right >=GameViewer.ColCount) or
                        (Bottom>=GameViewer.RowCount) then begin
                        InitializeGameViewer(False); ShowGameBackground;
                        ShowSquares;
                        end;

                   SetCursor(Editor.Cursor,Editor.Selection.Rect);
                   end;
                end;

             EditMenuItemRefreshClick(nil);
             end
          else begin // operate on the entire board
             with Editor.History do Item:=MakeItem(ehaRotate,Game.BoardWidth,Game.BoardHeight,Ord(Transformation2D),Position);
             if (Game.BoardWidth<>0) and (Game.BoardHeight<>0) and
                Editor.History.BeginTransaction(1) and
                Editor.History.AddItem(Item) then
                try     Editor.History.DoItem(Item);
                        EditMenuItemRefreshClick(Self);
                        Transformation2D:=t2DRotate0DegreesClockwise; // signals 'OK'
                        Modified:=True; ShowStatus;
                finally Editor.History.EndTransaction(Transformation2D=t2DRotate0DegreesClockwise);
                end;
             end;
          end;
     end;
end;

procedure TToolsForm.EditMenuItemFlipClick(Sender: TObject);
var i,j,H,W,Col,Row:Integer; s:String;
    Transformation2D:TTransformation2D;
    P:TPoint; B,B2:TBoard; Item:TEditorHistoryItem; ToolButton:TToolButton;
begin
  //AnimateButtonMouseClick(EditToolButtonMirrorHorizontally);

  if  (Sender=EditMenuItemFlipVertically) or
      (Sender=EditToolButtonFlipVertically) then begin
      ToolButton:=EditToolButtonFlipVertically;
      Transformation2D:=t2DFlipVertically;
      end
  else begin
      ToolButton:=EditToolButtonFlipHorizontally;
      Transformation2D:=t2DFlipHorizontally;
      end;

  if ToolButton.Enabled then with Game do begin
     //AnimateButtonMouseClick(ToolButton);

     if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
        if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
        else begin
          if (not Editor.Selection.HasBoard) and
             (Editor.Cursor=ctSelection) and Editor.Selection.Enabled then
             CutSelectionToEditorClipboard;

          if Editor.Selection.HasBoard then begin // operate on selection
             if TextToBoard(Editor.Selection.BoardAsText,W,H,P,B2) then begin
                for Col:=1 to W do
                    for Row:=1 to H do begin
                        CalculateTransformation2D(Transformation2D,Col,Row,W,H,i,j);
                        B[i,j]:=B2[Col,Row];
                        end;

                if BoardToText(W,H,B,Rect(1,1,Succ(W),Succ(H)),s,Editor.Selection.NonFloorCellCount) then begin
                   with Editor.CursorRect do
                     Editor.CursorRect             :=Rect(Left,Top,Left+W,Top+H);

                   Editor.Selection.BoardAsText    :=s;
                   Editor.Selection.BoardWidth     :=W;
                   Editor.Selection.BoardHeight    :=H;
                   Editor.Selection.HasBoard       :=True;
                   Editor.Selection.Rect           :=Editor.CursorRect;

                   ShowSquares;
                   SetCursor(Editor.Cursor,Editor.Selection.Rect);
                   end;
                end;
             end
          else begin // operate on the entire board
             with Editor.History do Item:=MakeItem(ehaMirror,Game.BoardWidth,Game.BoardHeight,Ord(Transformation2D),Position);
             if (Game.BoardWidth<>0) and (Game.BoardHeight<>0) and
                Editor.History.BeginTransaction(1) and
                Editor.History.AddItem(Item) then
                try     Editor.History.DoItem(Item);
                        CalculateGameInformation(False);
                        ShowSquares;
                        with Editor do SetCursor(Cursor,CursorRect);
                        Transformation2D:=t2DRotate0DegreesClockwise; // signals 'OK'
                        Modified:=True; ShowStatus;
                finally Editor.History.EndTransaction(Transformation2D=t2DRotate0DegreesClockwise);
                end;
             end;
          end;

     ShowStatus;
     end;
end;

function  TToolsForm.MinHeight:Integer;
begin
  Result:=525; // must be so large that there is room for a full-sized board in the editor
  if Assigned( CaptureForm ) then Result := Max( Result, CaptureForm.MinHeight );
end;

function  TToolsForm.MinWidth:Integer;
begin
  Result:=600; // must be so large that there is room for a full-sized board in the editor and room for all buttons on the toolbar
  if Assigned( CaptureForm ) then Result := Max( Result, CaptureForm.MinWidth );
end;

procedure TToolsForm.ShowStatus;
begin
  with PageControl1 do
    try    if        ActivePage=TabSheetEditor                                then ShowStatusEditor
           else if   ActivePage=TabSheetSolver                                then ShowStatusSolver
           else if   ActivePage=TabSheetOptimizer                             then ShowStatusOptimizer
           else if   ActivePage=TabSheetGenerator                             then ShowStatusGenerator
           else if ( ActivePage=TabSheetCapture ) and Assigned( CaptureForm ) then CaptureForm.ShowStatus
           else;
    except on E:Exception do begin end; // e.g., to catch 'access denied' exceptions when the application is deactivated by a password-protected screen saver
    end;
end;

procedure TToolsForm.ShowStatusEditor;
var i,j:Integer; OK:Boolean;
begin
  with StatusBar1.Panels[0] do Text:=OKChangedText[Modified {$IFDEF YASC} or MainForm.Modified {$ENDIF}];

  EditMenuItemSaveAs.Enabled:=(Game<>nil) and (Game.BoardHeight<>0) and (Game.BoardWidth<>0) and (not Editor.Selection.Enabled);
  EditMenuItemSave.Enabled:=((Modified {$IFDEF YASC} or MainForm.Modified {$ENDIF}) or (StrEqual(PluginLevelInfo.LevelName,Editor.FileName) and PluginLevelInfo.IsALegalLevel and (PluginLevelInfo.NewGamesCount<>0)))
                            and
                            EditMenuItemSaveAs.Enabled
                            and
                            (not IsANewFileName(Editor.FileName));
  with Editor do EditMenuItemCut.Enabled:=(Cursor=ctSelection) and Editor.Selection.Enabled;
  EditMenuItemCopy.Enabled:=EditMenuItemCut.Enabled;
  EditMenuItemPaste.Enabled:=Clipboard.HasFormat(CF_TEXT);
  EditMenuItemDelete.Enabled:=EditMenuItemCut.Enabled;
  EditMenuItemSelectAll.Enabled:=(Game<>nil) and (Game.BoardHeight<>0) and (Game.BoardWidth<>0);
  EditMenuItemCopyToInternalClipboard.Enabled:=((not Editor.Selection.HasBoard) and EditMenuItemSelectAll.Enabled)
                                               or
                                               (Editor.Selection.HasBoard and (Editor.Selection.NonFloorCellCount<>0));
  EditMenuItemRefresh.Enabled:=Game<>nil;

  EditMenuItemUndo.Enabled:=(Editor.History<>nil) and (Editor.History.Position>0);
  EditMenuItemRedo.Enabled:=(Editor.History<>nil) and (Editor.History.Position<Editor.History.Count);

  with Editor do
    if (Cursor<>ctSelection) or (not Editor.Selection.Enabled) then begin
       OK:=(Game<>nil) and (Game.BoardWidth<>0) and (Game.BoardHeight<>0);
       EditMenuItemRotateClockWise.Enabled:=OK and
                                            (Game.BoardWidth<=MAX_BOARD_HEIGHT) and
                                            (Game.BoardHeight<=MAX_BOARD_WIDTH) and
                                            ((Game.BoardWidth>1) or (Game.BoardHeight>1));
       EditMenuItemRotateCounterClockWise.Enabled:=EditMenuItemRotateClockWise.Enabled;
       EditMenuItemFlipVertically  .Enabled:=OK and (Game.BoardHeight>1);
       EditMenuItemFlipHorizontally.Enabled:=OK and (Game.BoardWidth >1);
       EditMenuItemFill.Enabled:=False;
       EditMenuItemReplace.Enabled:=OK;
       EditMenuItemNormalizeBoard.Enabled:=OK and (Game.BoardWidth>3) and (Game.BoardHeight>3);
       end
    else with CursorRect do begin
       OK:=(Game<>nil) and (Left<Right) and (Top<Bottom);
       EditMenuItemRotateClockWise.Enabled:=OK and (Right-Left<=MAX_BOARD_HEIGHT) and (Bottom-Top<=MAX_BOARD_WIDTH);
       EditMenuItemRotateCounterClockWise.Enabled:=EditMenuItemRotateClockWise.Enabled;
       EditMenuItemFlipVertically  .Enabled:=OK and (Bottom-Top>1);
       EditMenuItemFlipHorizontally.Enabled:=OK and (Right-Left>1);
       EditMenuItemFill.Enabled:=OK;
       EditMenuItemReplace.Enabled:=OK;
       EditMenuItemNormalizeBoard.Enabled:=False; //OK and (Bottom-Top>3) and (Right-Left>3);
       EditMenuItemFillWithWallBorderOpaque.Enabled:=OK and (Bottom-Top>2) and (Right-Left>2);
       EditMenuItemFillWithWallBorderTransparent.Enabled:=EditMenuItemFillWithWallBorderOpaque.Enabled;
       if EditMenuItemFill.Enabled then begin
          with EditMenuItemFillWithWallBorderOpaque do
            if   Enabled then begin
                 if Tag=Ord(True) then begin
                    Checked:=True; Tag:=Ord(False);
                    EditMenuItemFillWithWalls.Checked:=False;
                    EditPopupMenuItemFillWithWalls.Checked:=False;
                    end;
                 end
            else if Checked then begin // select 'walls', but remember that the user's own last selection was 'wall border'
                    EditMenuItemFillWithWalls.Checked:=True;
                    EditPopupMenuItemFillWithWalls.Checked:=True;
                    Checked:=False; Tag:=Ord(True);
                    end;
          with EditMenuItemFillWithWallBorderTransparent do
            if   Enabled then begin
                 if Tag=Ord(True) then begin
                    Checked:=True; Tag:=Ord(False);
                    EditMenuItemFillWithWalls.Checked:=False;
                    EditPopupMenuItemFillWithWalls.Checked:=False;
                    end;
                 end
            else if Checked then begin // select 'walls', but remember that the user's own last selection was 'wall border'
                    EditMenuItemFillWithWalls.Checked:=True;
                    EditPopupMenuItemFillWithWalls.Checked:=True;
                    Checked:=False; Tag:=Ord(True);
                    end;
          end;
       end;

  EditPopupMenuItemReplace.Enabled:=EditMenuItemReplace.Enabled;     
  j:=0;
  for i:=0 to Pred(EditMenuItemReplace.Count) do
      with EditMenuItemReplace.Items[i] do
        if Checked then begin
           Inc(j,Tag); // 'find what' and 'replace with' cancel each other out for matching pairs; e.g., 'find what: walls' has Tag=1, and 'replace with: walls' has Tag=-1
           if i<EditPopupMenuItemReplace.Count then EditPopupMenuItemReplace.Items[i].Checked:=True;
           end;
  EditMenuItemReplaceDoIt.Enabled:=j<>0; // 'True': e.g., not a 'search and replace floors with floors' situation
  EditPopupMenuItemReplaceDoit.Enabled:=EditMenuItemReplaceDoIt.Enabled;


  EditMenuItemGrid                              .Visible:=PageControl1.ActivePage=TabSheetEditor;
  EditMenuItemMovePlayerAndBoxes                .Visible:=EditMenuItemGrid.Visible;
  EditMenuItemSeparator                         .Visible:=EditMenuItemGrid.Visible;
  EditMenuItemNormalizeBoardMakeRectangularBoard.Enabled:=EditMenuItemNormalizeBoard.Enabled;

  with EditToolBarTop do
    for i:=0 to Pred(ButtonCount) do with Buttons[i] do
        if MenuItem<>nil then Enabled:=MenuItem.Enabled;

  EditToolButtonOpen.Enabled:=EditMenuItemOpen.Enabled;
  EditToolButtonNew.Enabled:=EditMenuItemNew.Enabled;
  EditToolButtonSave.Enabled:=EditMenuItemSave.Enabled;
  EditToolButtonSaveAs.Enabled:=EditMenuItemSaveAs.Enabled;
  EditToolButtonCut.Enabled:=EditMenuItemCut.Enabled;
  EditToolButtonCopy.Enabled:=EditMenuItemCopy.Enabled;
  EditToolButtonPaste.Enabled:=EditMenuItemPaste.Enabled;
  EditToolButtonDelete.Enabled:=EditMenuItemDelete.Enabled;
  EditToolButtonUndo.Enabled:=EditMenuItemUndo.Enabled;
  EditToolButtonRedo.Enabled:=EditMenuItemRedo.Enabled;
  EditToolButtonRotateClockwise.Enabled:=EditMenuItemRotateClockwise.Enabled;
  EditToolButtonRotateCounterClockwise.Enabled:=EditMenuItemRotateCounterClockwise.Enabled;
  EditToolButtonFlipVertically.Enabled:=EditMenuItemFlipVertically.Enabled;
  EditToolButtonFlipHorizontally.Enabled:=EditMenuItemFlipHorizontally.Enabled;

  EditPopupMenuItemCut.Enabled:=EditMenuItemCut.Enabled;
  EditPopupMenuItemCopy.Enabled:=EditMenuItemCopy.Enabled;
  EditPopupMenuItemPaste.Enabled:=EditMenuItemPaste.Enabled;
  EditPopupMenuItemDelete.Enabled:=EditMenuItemDelete.Enabled;
  EditPopupMenuItemRemoveRedundantWalls.Enabled:=EditMenuItemNormalizeBoard.Enabled;
  EditPopupMenuItemSelectAll.Enabled:=EditMenuItemSelectAll.Enabled;
  EditPopupMenuItemCopyToInternalClipboard.Enabled:=EditMenuItemCopyToInternalClipboard.Enabled;
  EditPopupMenuItemFillWithWalls.Checked:=EditMenuItemFillWithWalls.Checked;
  EditPopupMenuItemFillWithWallBorderOpaque.Enabled:=EditMenuItemFillWithWallBorderOpaque.Enabled;
  EditPopupMenuItemFillWithWallBorderOpaque.Checked:=EditMenuItemFillWithWallBorderOpaque.Checked;
  EditPopupMenuItemFillWithWallBorderTransparent.Enabled:=EditMenuItemFillWithWallBorderTransparent.Enabled;
  EditPopupMenuItemFillWithWallBorderTransparent.Checked:=EditMenuItemFillWithWallBorderTransparent.Checked;

  SettingsMenuItemWindowSizeMaximized.Checked:=Self.WindowState=wsMaximized;
  CaptureSettingsMenuItemWindowSizeMaximized.Checked := SettingsMenuItemWindowSizeMaximized.Checked;

  if   not Editor.Selection.HasBoard then
       StatusLabel2.Caption:=Self.BoardDimensionsAsText(Game.BoardWidth,Game.BoardHeight,Game.BoxCount,Game.GoalCount,GameViewer.BoardDimensionsAsText)
  else StatusLabel2.Caption:=SPACE;

  if LevelSetForm<>nil then LevelSetForm.EnableOrDisableNewItem;

  EditToolButtonInternalClipboard.Enabled:=LevelSetForm<>nil;

  EditMenuItemCopyToGenerator.Enabled:=EditMenuItemSaveAs.Enabled and (Game.BoardWidth>=MIN_BOARD_WIDTH) and (Game.BoardHeight>=MIN_BOARD_HEIGHT) and {(not MainForm.Generator.IsActive) and} (not MainForm.Generator.IsFullCandidateSet);
  EditToolButtonGenerator.Enabled:=EditMenuItemCopyToGenerator.Enabled;
end;

procedure TToolsForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize:=//OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
          ((not Game.IsBusy) or Game.IsReplaying) and
          ((NewWidth>=MinWidth) and (NewHeight>=MinHeight));
end;

function TToolsForm.LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
var h,i,j,k,MinHeight,MinWidth:Integer; CursorType:TEditorCursorType;
begin
  Result:=True;
  with IniFile do begin
    {$IFNDEF YASC}
      Editor.FileName     :=ReadString(EDITOR_INIFILE_SECTION,'FileName','');

      i:=IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'BoardDimensionsAsText',Integer(GameViewer.BoardDimensionsAsText));
      if (i>=Ord(Low(GameViewer.BoardDimensionsAsText))) and (i<=Ord(High(GameViewer.BoardDimensionsAsText))) then
         GameViewer.BoardDimensionsAsText:=TBoardDimensionsAsText(i);

      Editor.DrawingToolCursorsEnabled:=IniFile.ReadBool(EDITOR_INIFILE_SECTION,'DrawingToolCursorsEnabled',Editor.DrawingToolCursorsEnabled);
    {$ENDIF}

    MinWidth              :=Self.MinWidth;
    MinHeight             :=Self.MinHeight;
    Left                  :=Max(0,Min(Screen.DeskTopWidth-MinWidth,IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'Left',Left)));
    Top                   :=Max(0,Min(Screen.DeskTopHeight-MinHeight-40,IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'Top',Top)));
    Width                 :=Max(MinWidth,Min(Screen.DeskTopWidth-Left,IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'Width',Width)));
    Height                :=Max(MinHeight,Min(Screen.DeskTopHeight-Top-40,IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'Height',Height))); // -40: normally sufficient to avoid collision with start-menu

    if   ReadBool(TOOLS_INIFILE_SECTION,'Maximized',WindowState=wsMaximized) then
         if   WindowState<>wsMaximized then WindowState:=wsMaximized
         else
    else if WindowState=wsMaximized then WindowState:=wsNormal;

    CollectionNameInTitleLine :=IniFile.ReadBool(TOOLS_INIFILE_SECTION,'CollectionNameInTitleLine',CollectionNameInTitleLine);

    with GameViewer.SkinPict do begin
      MaskBitMapPct:=DEFAULT_MASK_BITMAP_PCT;
      end;

    GameViewer.BackgroundPict.FileName         :=KeepDataPathUpToDate(IniFile.ReadString(TOOLS_INIFILE_SECTION,'Background.FileName',GameViewer.BackgroundPict.FileName));
    if StrEqual(GameViewer.BackgroundPict.FileName,DEFAULT_VALUE) then GameViewer.BackgroundPict.FileName:='';
    GameViewer.BackgroundPict.Color            :=TColor(IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'Background.Color',GameViewer.BackgroundPict.Color));
    i                                          :=IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'Background.Antialiasing',Ord(GameViewer.BackgroundPict.AntiAliasing));
    if (i>=Ord(Low(Misc_.TAntiAliasing))) and (i<=Ord(High(Misc_.TAntiAliasing))) then
       GameViewer.BackgroundPict.Antialiasing:=Misc_.TAntiAliasing(i);
    i                                          :=IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'Background.View',Ord(GameViewer.BackgroundPict.View));
    if (i>=Ord(Low(TImageView))) and (i<=Pred(Ord(High(TImageView)))) then // "Pred()": all except "Floor tile"
       GameViewer.BackgroundPict.View          :=TImageView(i);
    GameViewer.BackgroundPict.Visible          :=IniFile.ReadBool(TOOLS_INIFILE_SECTION,'Background.Visible',GameViewer.BackgroundPict.Visible);

    ButtonsFileName                            :=KeepDataPathUpToDate(IniFile.ReadString(TOOLS_INIFILE_SECTION,'Buttons.FileName',ButtonsFileName));
    if StrEqual(ButtonsFileName,DEFAULT_VALUE) then ButtonsFileName:='';

    GameViewer.SkinPict.FileName               :=KeepDataPathUpToDate(IniFile.ReadString(TOOLS_INIFILE_SECTION,'Skin.FileName',GameViewer.SkinPict.FileName));
    if StrEqual(GameViewer.SkinPict.FileName,DEFAULT_VALUE) then GameViewer.SkinPict.FileName:='';
    i                                          :=IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'Skin.Antialiasing',Ord(GameViewer.SkinPict.AntiAliasing));
    if (i>=Ord(Low(Misc_.TAntiAliasing))) and (i<=Ord(High(Misc_.TAntiAliasing))) then
       GameViewer.SkinPict.Antialiasing        :=Misc_.TAntiAliasing(i);
    GameViewer.SkinPict.Masked                 :=IniFile.ReadBool   (TOOLS_INIFILE_SECTION,'Skin.Masked',GameViewer.SkinPict.Masked);
    GameViewer.SkinPict.MaskBitMapColor        :=ColorToRGB(IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'Skin.BackgroundColor',Integer(RGBToColor(GameViewer.SkinPict.MaskBitMapColor))));
    GameViewer.SkinPict.MaskBitMapPct          :=Max(0,Min(100,IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'Skin.BackgroundColorTolerance',GameViewer.SkinPict.MaskBitMapPct)));
    GameViewer.SquareSetSelectedSquaresColor   :=TColor(IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'SquareSet.SelectedSquaresColor',GameViewer.SquareSetSelectedSquaresColor));
    GameViewer.SquareSetNotSelectedSquaresColor:=TColor(IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'SquareSet.NotSelectedSquaresColor',GameViewer.SquareSetNotSelectedSquaresColor));
    GameViewer.SquareSetTransparencyPct        :=Max(0,Min(100,IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'SquareSet.TransparencyPct',GameViewer.SquareSetTransparencyPct)));

    for CursorType:=Low(CursorType) to High(CursorType) do with Editor.Cursors[CursorType] do begin
        PenColor          :=TColor(IniFile.ReadInteger(EDITOR_INIFILE_SECTION,CURSOR_NAMES[CursorType]+'.PenColor',PenColor));
        PenWidth          :=Max(1,Min(MAX_CURSOR_PEN_WIDTH,IniFile.ReadInteger(EDITOR_INIFILE_SECTION,CURSOR_NAMES[CursorType]+'.PenWidth',PenWidth)));
        ShadowColor       :=TColor(IniFile.ReadInteger(EDITOR_INIFILE_SECTION,CURSOR_NAMES[CursorType]+'.ShadowColor',ShadowColor));
//      Size              :=Max(1,Min(100,IniFile.ReadInteger(EDITOR_INIFILE_SECTION,CURSOR_NAMES[CursorType]+'.Size',Size)));
        end;

    GameViewer.FrameColor:=TColor(IniFile.ReadInteger(EDITOR_INIFILE_SECTION,'FrameColor',GameViewer.FrameColor));
    GameViewer.FrameShadowColor:=TColor(IniFile.ReadInteger(EDITOR_INIFILE_SECTION,'FrameShadowColor',GameViewer.FrameShadowColor));
    EditMenuItemGrid.Checked:=IniFile.ReadBool(EDITOR_INIFILE_SECTION,'Grid',EditMenuItemGrid.Checked);
    EditMenuItemMovePlayerAndBoxes.Checked:=IniFile.ReadBool(EDITOR_INIFILE_SECTION,'EditMenuItemMovePlayerAndBoxes',EditMenuItemMovePlayerAndBoxes.Checked);

    i:=IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'ReplaySpeedMilliSecondsPerMove',ReplaySpeedMilliSecondsPerMove);
    if (i>=1000 div MAX_REPLAY_SPEED_MOVES_PER_SEC) and
       (i<=MAX_ANIMATION_TIME_MS) then
       ReplaySpeedMovesPerSecond:=Round(1000/i);
    ReplaySpeedTrackBarBackgroundColor:=TColor(IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'ReplaySpeedTrackBarBackgroundColor',ReplaySpeedTrackBarBackgroundColor));
    ReplaySpeedTrackBarSliderColor:=TColor(IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'ReplaySpeedTrackBarSliderColor',ReplaySpeedTrackBarSliderColor));
    ReplaySpeedTrackBarFontColor:=TColor(IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'ReplaySpeedTrackBarFontColor',ReplaySpeedTrackBarFontColor));
    ReplaySpeedTrackBarShadowColor:=TColor(IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'ReplaySpeedTrackBarShadowColor',ReplaySpeedTrackBarShadowColor));

    i:=IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'SortMetric',Ord(SortMetric));
    if (i>=Ord(Low (TGameMetrics))) and // not Low(SortMetric)..High(SortMetric) because 'SortMetric' is of type 'TGameMetricsExtended', and sorting is only applicable for 'TGameMetrics' values
       (i<=Ord(High(TGameMetrics))) then
       SortMetric:=TGameMetrics(i);

    h:=IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'PluginGroupBoxesHeight',LevelGroupBox.Height);
    i:=IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'LeftGroupBoxWidth',PluginLevelGroupBox.Width);
    j:=IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'MiddleGroupBoxWidth',SolverGroupBox.Width);
    k:=IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'RightGroupBoxWidth',LevelGroupBox.Width);
    if (h>=PluginGroupBoxesMinimumHeight)   and
       (i>=PluginLevelGroupBoxMinimumWidth) and
       (j>=PluginGroupBoxMinimumWidth)      and
       (k>=LevelGroupBoxMinimumWidth)       and
       ((WindowState=wsMaximized) // the window hasn't really been maximized yet; accept the groupbox settings even though they are not thoroughly validated
        or
        (i+j+k<Screen.DeskTopWidth-2*PluginLevelGroupBox.Left)) then begin // this is not fool-proof, but it will have to do
       ResizePluginGroupBoxes(i,j,k,h,False);
       with ImageBoard.Picture.BitMap do with Canvas do begin // there may be a short delay the first time the board image is shown; it looks better when the still empty image has the same color as the background
         Brush.Color:=PanelBoard.Color; Brush.Style:=bsSolid; FillRect(Rect(0,0,Width,Height));
         end;
       end;

    LoadStringGridColumnWidthsFromIniFile(IniFile,'Solve Levels',SolveLevelsStringGrid);
    LoadStringGridColumnWidthsFromIniFile(IniFile,'Optimize Solutions',OptimizeSolutionsStringGrid);
    LoadStringGridColumnWidthsFromIniFile(IniFile,'Generate Levels',GenerateLevelsStringGrid);

    ToolButtonNewLevelWithWallsWidth .Tag:=((Max(MIN_BOARD_WIDTH ,Min(MAX_BOARD_WIDTH ,IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'NewLevelWithWallsWidth' ,ToolButtonNewLevelWithWallsWidth .Tag))) + GENERATOR_BOARD_REGION_WIDTH  -1) div GENERATOR_BOARD_REGION_WIDTH )*GENERATOR_BOARD_REGION_WIDTH;
    ToolButtonNewLevelWithWallsHeight.Tag:=((Max(MIN_BOARD_HEIGHT,Min(MAX_BOARD_HEIGHT,IniFile.ReadInteger(TOOLS_INIFILE_SECTION,'NewLevelWithWallsHeight',ToolButtonNewLevelWithWallsHeight.Tag))) + GENERATOR_BOARD_REGION_HEIGHT -1) div GENERATOR_BOARD_REGION_HEIGHT)*GENERATOR_BOARD_REGION_HEIGHT;

    Result :=LoadFontFromIniFile(IniFile,TOOLS_INIFILE_SECTION,'Window',Self.Font) and Result;
    OnFontChange;
    end;
end;

function TToolsForm.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
var CursorType:TEditorCursorType;
begin
  Result:=True;
  with IniFile do begin
   {$IFNDEF YASC}
     if   IsANewFileName(Editor.FileName) then
          IniFile.WriteString(TOOLS_INIFILE_SECTION,'FileName','')
     else IniFile.WriteString(TOOLS_INIFILE_SECTION,'FileName',Editor.FileName);

     IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'BoardDimensionsAsText',Integer(GameViewer.BoardDimensionsAsText));

     IniFile.WriteBool(EDITOR_INIFILE_SECTION,'DrawingToolCursorsEnabled',Editor.DrawingToolCursorsEnabled);
   {$ENDIF}

   if  WindowState=wsNormal then begin
       WriteInteger(TOOLS_INIFILE_SECTION,'Left',Left);
       WriteInteger(TOOLS_INIFILE_SECTION,'Top',Top);
       WriteInteger(TOOLS_INIFILE_SECTION,'Width',Width);
       WriteInteger(TOOLS_INIFILE_SECTION,'Height',Height);
       end;
   WriteBool(TOOLS_INIFILE_SECTION,'Maximized',WindowState=wsMaximized);

   IniFile.WriteBool(TOOLS_INIFILE_SECTION,'CollectionNameInTitleLine',CollectionNameInTitleLine);

   if   GameViewer.BackgroundPict.FileName='' then
        IniFile.WriteString(TOOLS_INIFILE_SECTION,'Background.FileName',DEFAULT_VALUE)
   else IniFile.WriteString(TOOLS_INIFILE_SECTION,'Background.FileName',GameViewer.BackgroundPict.FileName);
   IniFile.WriteInteger    (TOOLS_INIFILE_SECTION,'Background.Color',GameViewer.BackgroundPict.Color);
   IniFile.WriteInteger    (TOOLS_INIFILE_SECTION,'Background.Antialiasing',Ord(GameViewer.BackgroundPict.AntiAliasing));
   IniFile.WriteInteger    (TOOLS_INIFILE_SECTION,'Background.View',Ord(GameViewer.BackgroundPict.View));
   IniFile.WriteBool       (TOOLS_INIFILE_SECTION,'Background.Visible',GameViewer.BackgroundPict.Visible);

   if   ButtonsFileName='' then
        IniFile.WriteString(TOOLS_INIFILE_SECTION,'Buttons.FileName',DEFAULT_VALUE)
   else IniFile.WriteString(TOOLS_INIFILE_SECTION,'Buttons.FileName',ButtonsFileName);

   if   GameViewer.SkinPict.FileName='' then
        IniFile.WriteString(TOOLS_INIFILE_SECTION,'Skin.FileName',DEFAULT_VALUE)
   else IniFile.WriteString(TOOLS_INIFILE_SECTION,'Skin.FileName',GameViewer.SkinPict.FileName);
   IniFile.WriteInteger    (TOOLS_INIFILE_SECTION,'Skin.Antialiasing',Ord(GameViewer.SkinPict.AntiAliasing));
   IniFile.WriteBool       (TOOLS_INIFILE_SECTION,'Skin.Masked',GameViewer.SkinPict.Masked);
   IniFile.WriteInteger    (TOOLS_INIFILE_SECTION,'Skin.BackgroundColor',Integer(RGBToColor(GameViewer.SkinPict.MaskBitMapColor)));
   IniFile.WriteInteger    (TOOLS_INIFILE_SECTION,'Skin.BackgroundColorTolerance',GameViewer.SkinPict.MaskBitMapPct);

   IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'SquareSet.SelectedSquaresColor',GameViewer.SquareSetSelectedSquaresColor);
   IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'SquareSet.NotSelectedSquaresColor',GameViewer.SquareSetNotSelectedSquaresColor);
   IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'SquareSet.TransparencyPct',GameViewer.SquareSetTransparencyPct);

   for CursorType:=Low(CursorType) to High(CursorType) do with Editor.Cursors[CursorType] do begin
       IniFile.WriteInteger(EDITOR_INIFILE_SECTION,CURSOR_NAMES[CursorType]+'.PenColor',PenColor);
       IniFile.WriteInteger(EDITOR_INIFILE_SECTION,CURSOR_NAMES[CursorType]+'.PenWidth',PenWidth);
       IniFile.WriteInteger(EDITOR_INIFILE_SECTION,CURSOR_NAMES[CursorType]+'.ShadowColor',ShadowColor);
//     IniFile.WriteInteger(EDITOR_INIFILE_SECTION,CURSOR_NAMES[CursorType]+'.Size',Size);
       end;

   IniFile.WriteInteger(EDITOR_INIFILE_SECTION,'FrameColor',GameViewer.FrameColor);
   IniFile.WriteInteger(EDITOR_INIFILE_SECTION,'FrameShadowColor',GameViewer.FrameShadowColor);
   IniFile.WriteBool(EDITOR_INIFILE_SECTION,'Grid',EditMenuItemGrid.Checked);
   IniFile.WriteBool(EDITOR_INIFILE_SECTION,'EditMenuItemMovePlayerAndBoxes',EditMenuItemMovePlayerAndBoxes.Checked);

   IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'ReplaySpeedMilliSecondsPerMove',ReplaySpeedMilliSecondsPerMove);
   IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'ReplaySpeedTrackBarBackgroundColor',ReplaySpeedTrackBarBackgroundColor);
   IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'ReplaySpeedTrackBarSliderColor',ReplaySpeedTrackBarSliderColor);
   IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'ReplaySpeedTrackBarFontColor',ReplaySpeedTrackBarFontColor);
   IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'ReplaySpeedTrackBarShadowColor',ReplaySpeedTrackBarShadowColor);

   IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'SortMetric',Ord(SortMetric));

   WriteInteger(TOOLS_INIFILE_SECTION,'PluginGroupBoxesHeight',PluginLevelGroupBox.Height);
   WriteInteger(TOOLS_INIFILE_SECTION,'LeftGroupBoxWidth',PluginLevelGroupBox.Width);
   WriteInteger(TOOLS_INIFILE_SECTION,'MiddleGroupBoxWidth',SolverGroupBox.Width);
   WriteInteger(TOOLS_INIFILE_SECTION,'RightGroupBoxWidth',LevelGroupBox.Width);

   SaveStringGridColumnWidthsToIniFile(IniFile,'Solve Levels',SolveLevelsStringGrid);
   SaveStringGridColumnWidthsToIniFile(IniFile,'Optimize Solutions',OptimizeSolutionsStringGrid);
   SaveStringGridColumnWidthsToIniFile(IniFile,'Generate Levels',GenerateLevelsStringGrid);

   IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'NewLevelWithWallsWidth' ,ToolButtonNewLevelWithWallsWidth .Tag);
   IniFile.WriteInteger(TOOLS_INIFILE_SECTION,'NewLevelWithWallsHeight',ToolButtonNewLevelWithWallsHeight.Tag);

   Result:=SaveFontToIniFile(IniFile,TOOLS_INIFILE_SECTION,'Window',Self.Font) and Result;
   end;
end;

function BitMapToImageList(BitMap__,DefaultBitMap__:TBitMap; ImageList__:TImageList):Boolean;
var i:Integer; B:TBitMap;
begin
  with ImageList__ do begin
    Result:=(BitMap__.Width>=DefaultBitMap__.Width) and
            (BitMap__.Height=DefaultBitMap__.Height);
    if Result then begin
       Height:=BitMap__.Height;
       Width:=Height;
       Result:=BitMapCreate(B,Width,Height);
       if Result then
          try     B.Canvas.Copymode:=cmSrcCopy;
                  Clear;
                  for i:=0 to Pred(BitMap__.Width div Height) do begin
                      B.Canvas.CopyRect(Rect(0,0,Width,Height),BitMap__.Canvas,Rect(i*Width,0,Succ(i)*Width,Height));
                      if AddMasked(B,B.Canvas.Pixels[0,Pred(Height)])<0 then Result:=False;
                      end;
          finally B.Free;
          end;
       end;
    end;
end;

function ImageListToBitMap(ImageList__:TImageList; var BitMap__:TBitMap):Boolean;
var i:Integer; B:TBitMap;
begin
  BitMap__:=nil; B:=nil;
  with ImageList__ do begin
    Result:=(Count<>0) and BitMapCreate(BitMap__,1,1) and BitMapCreate(B,1,1);
    if Result then
       try
         try
           for i:=0 to Pred(Count) do begin
               GetBitMap(i,B);
               if i=0 then BitMapResize(BitMap__,Count*B.Width,B.Height);
               if (BitMap__.Width>=Succ(i)*B.Width) and (BitMap__.Height>=B.Height) then
                  BitMap__.Canvas.Draw(i*B.Width,0,B);
               with B do with Canvas do begin
                 Brush.Style:=bsSolid; Brush.Color:=BkColor;
                 FillRect(Rect(0,0,Width,Height));
                 end;
               end;
         finally B.Free;
         end;
       except on E:Exception do begin
                 BitMap__.Free; BitMap__:=nil;
                 Result:=Error(E.Message,Application.Title);
                 end;
       end;
    end;
end;

procedure TToolsForm.SetDefaultFormSize;
var oCursor:TCursor;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors and
     (WindowState=wsNormal) //and
     //((Width<>DefaultFormWidth) or (Height<>DefaultFormHeight))
     then begin
     oCursor:=Screen.Cursor;
     try Screen.Cursor:=crHourGlass;
         fInitialized :=False;
         Width        :=DefaultFormWidth;
         Height       :=DefaultFormHeight;
         fInitialized :=True;
         SetDefaultStringGridColumnWidths; // restore default string grid column widths
         FormResize(Self);
         ResizePluginGroupBoxes(PluginLevelGroupBox.Tag,SolverGroupBox.Tag,LevelGroupBox.Tag,PluginGroupBoxesMinimumHeight,True); // restore original groupbox widths and heights
     finally
       fInitialized   :=True;
       Screen.Cursor  :=oCursor;
     end;
     end;
end;

procedure TToolsForm.MenuItemWindowSizeClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if      Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else if (Sender=SettingsMenuItemWindowSizeDefault) or (Sender=CaptureSettingsMenuItemWindowSizeDefault) then begin
             if WindowState<>wsNormal then WindowState:=wsNormal;
             SetDefaultFormSize;
             end
     else if (Sender=SettingsMenuItemWindowSizeDefaultCentered) or (Sender=CaptureSettingsMenuItemWindowSizeDefaultCentered) then begin
             MenuItemWindowSizeClick(SettingsMenuItemWindowSizeDefault);
             Left:=Max(0,(Screen.DeskTopWidth -Width ) div 2);
             Top :=Max(0,(Screen.DeskTopHeight-Height) div 2);
             end
     else if (Sender=SettingsMenuItemWindowSizeMaximized) or (Sender=CaptureSettingsMenuItemWindowSizeMaximized) then begin
             if   WindowState=wsNormal then
                  WindowState:=wsMaximized
             else WindowState:=wsNormal;
             end;
  SettingsMenuItemWindowSizeMaximized.Checked:=Self.WindowState=wsMaximized;
  CaptureSettingsMenuItemWindowSizeMaximized.Checked := SettingsMenuItemWindowSizeMaximized.Checked;
end;

function TToolsForm.LoadDefaultSkin:Boolean;
begin
  Result:=LoadSkin('',EditImageDefaultSkin.Picture.BitMap,TOOLS_SKIN_TILE_COUNT,DEFAULT_OBJECT_ANTI_ALIASING,True,clBlack,DEFAULT_MASK_BITMAP_PCT);
end;

function TToolsForm.LoadSkin(const FileName__:String; BitMap__:TBitMap; Count__:Integer; Antialiasing__:TAntiAliasing; Masked__:Boolean; MaskColor__:TColor; MaskBitMapPct__:Integer):Boolean;
begin // caution: 'GameViewer.SkinPict' becomes the new owner of 'BitMap__', unless 'BitMap__' is the default tiles, which is owned by the application
  Result:=False;
  GameViewer.SkinInitialized:=False; GameViewer.FloorTilesVisible:=False;
  Editor.UseCursorPictures:=False; Editor.LastCursorPict:=dtNone;
  if (LevelSetForm<>nil) and (LevelSetForm.GameViewer.SkinPict<>nil) then
     LevelSetForm.GameViewer.SkinPict.OrgBitMap:=nil;
  if GameViewer.SkinPict<>nil then with GameViewer do with SkinPict do begin
     if OrgBitMap=EditImageDefaultSkin.Picture.BitMap then OrgBitMap:=nil;  {default tile-set is owned by the application}
     Clear;

     if BitMap24BitPixelFormat(BitMap__,BitMap__) then begin
        GameViewer.SkinPict.FileName:=FileName__;
        if   GameViewer.SkinPict.FileName='' then
             OptionsForm.SettingsString[stGraphicsToolsBoardSkinImageFileName]:=DEFAULT_VALUE
        else OptionsForm.SettingsString[stGraphicsToolsBoardSkinImageFileName]:=GameViewer.SkinPict.FileName;

        FrameCount:=Count__; Antialiasing:=AntiAliasing__;
        Masked:=Masked__; MaskBitMapColor:=ColorToRGB(MaskColor__); MaskBitMapPct:=MaskBitMapPct__;
        OrgBitMap:=BitMap__; {note that 'GameViewer.SkinPict' now owns 'BitMap__'}
        GameViewer.FloorTilesVisible:=not BitMapIsColor(OrgBitMap,CellToRect(TILE_INDEX_FLOOR,0,OrgBitMap.Width div Count__,OrgBitMap.Height),MaskBitMapColor,MaskBitMapPct);
        Result:=True;
        end;
     end;
  if Result and (GameViewer.CursorPict<>nil) then with GameViewer do with CursorPict do begin
     OrgBitMap:=nil; {the tile-set picture is owned either by 'GameViewer.SkinPict' or the application}
     Clear;
     FrameCount:=Count__; Antialiasing:=AntiAliasing__;
     Masked:=Masked__; MaskBitMapColor:=ColorToRGB(MaskColor__); MaskBitMapPct:=MaskBitMapPct__;
     end;
end;

function  TToolsForm.CloseLevel(Sender: TObject):Boolean;
var i:Integer; s:String;
begin
  Result:=OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors;
  if Result then begin
     HideCursor(False);
     CloseEditorSelection(True,False);
     if Modified then begin
        if   IsANewFileName(Editor.FileName) then s:=ChangedNewText
        else s:=ChangedText;
        if   MainForm.ShutDownApplication then i:=MB_YESNO
        else i:=MB_YESNOCANCEL;
        case Msg(s,
                 Application.Title+SUB_TITLE_SEPARATOR+VisualFileName(Editor.FileName),
                 i+MB_ICONQUESTION) of
          IDYES    : begin Result:=Save(Self) and Editor.History.Close;
                           if   Result then PluginLevelInfo.NewGamesCount:=0
                           else Modified:=True;
                           ShowTitle('',Editor.FileName);
                           ShowStatus;
                     end;
          IDNO     : begin Game.SokoFileName:=''; // ensure that 'Game' doesn't try to save any garbage data later, e.g., from 'CheckSnapsnots' (don't call 'Game.Clear'; the editor history needs the board)
                           Modified:=False; PluginLevelInfo.NewGamesCount:=0;
                           Editor.FileName:='';
                           Editor.History.UndoSession;
                           Editor.History.Close;
                           ShowTitle('',Editor.FileName);
                           ShowStatus;
                     end;
          IDCANCEL : begin Result:=False;
                     end;
        end; // case
        end
     else begin
        {$IFDEF YASC}
          Game.SokoFileName:=''; // ensure that 'Game' doesn't try to save any garbage data later, e.g., from 'CheckSnapsnots' (don't call 'Game.Clear'; the editor history needs the board)
        {$ENDIF}
        if IsBlank(Editor.FileName) then begin
           Editor.History.Close; PluginLevelInfo.NewGamesCount:=0;
           end
        else begin
          Result:=Game.CloseLevel(True);
          if Result then begin
             Modified:=False; PluginLevelInfo.NewGamesCount:=0;
             Editor.History.Close;
             end
          else
             if IsAnIniFileSectionFileName(Editor.FileName) and
                (Game.SokoFile<>nil) and (Game.SokoFileName<>'') then begin {the collection filename may have changed if renaming failed}
                Editor.FileName:=MakeIniFileSectionFileName(Game.SokoFileName,ExtractSectionName(Editor.FileName));
                ShowTitle('',Editor.FileName);
                end;
          end;
        end;
     end;
end;

function  TToolsForm.IsALegalLevel(ShowMessage__,CalculateDimensions__:Boolean; var ErrorText__:String):Boolean;
var i,Col,Row,WidthLimit,HeightLimit:Integer;
begin
  with Game do begin

    PlayerPos.X:=0; PlayerPos.Y:=0; BoxCount:=0; GoalCount:=0;

    if CalculateDimensions__ then begin
       BoardWidth:=0;
       BoardHeight:=0;
       WidthLimit:=MAX_BOARD_WIDTH;
       HeightLimit:=MAX_BOARD_HEIGHT;
       end
    else begin
       WidthLimit:=BoardWidth;
       HeightLimit:=BoardHeight;
       end;

    for Col:=1 to WidthLimit do
        for Row:=1 to HeightLimit do begin
            i:=Board[Col,Row];
            if (i and PLAYER)<>0 then begin
               if   PlayerPos.X=0 then begin
                    PlayerPos.X:=Col; PlayerPos.Y:=Row;
                    end
               else Dec(i,PLAYER);
               end;
            if (i and BOX)<>0 then begin
               Inc(BoxCount);
               if BoxCount<=MAX_BOXES then with BoxPos[BoxCount] do begin
                  X:=Col; Y:=Row;
                  end;
               end;
            if (i and GOAL)<>0 then begin
               Inc(GoalCount);
               if GoalCount<=MAX_BOXES then with GoalPos[BoxCount] do begin
                  X:=Col; Y:=Row;
                  end;
               end;

            Board[Col,Row]:=i and BOARD_PIECES; // reset any flags

            if CalculateDimensions__ and
               ((i and (BOARD_PIECES-FLOOR))<>0) then begin
               if Col>BoardWidth  then BoardWidth :=Col;
               if Row>BoardHeight then BoardHeight:=Row;
               end;

            end;

    Result:=IsALegalBoard(Board,BoardWidth,BoardHeight,BoxCount,GoalCount,PlayerPos,True,True,True,Editor.BoardCellOrigin.Y,ErrorText__);
    if (not Result) and (ShowMessage__) then begin
       if PageControl1.ActivePage<>TabSheetEditor then begin
          PageControl1.ActivePage:=TabSheetEditor;
          PageControl1Change(TabSheetEditor);
          end;
       Msg(ErrorText__+NL+NL+PleaseMakeTheNecessaryChangesText,Caption,MB_OK+MB_ICONINFORMATION);
       end;
    end;
end;

procedure TToolsForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=CloseLevel(Sender);
end;

function TToolsForm.SaveLevel(Sender: TObject):Boolean;
var i,W,H:Integer; oIsBusy,DummyBoolean:Boolean; s:String; B:TBoard;
begin // precondition: the level is legal
  Result:=False;
  if (OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors)
     or
     (Sender=nil) then with Game do begin
     CloseEditors;
     PluginToolButtonStopReplayClick(PluginToolButtonReplay);
     PluginLevelInfo.ReplayInfo.IsLoaded:=False;
     CalculateGameInformation(False); ShowStatus;
     B:=Board; W:=BoardWidth; H:=BoardHeight; oIsBusy:=IsBusy;
     try
       IsBusy:=True;
       TrimBoard(MIN_BOARD_WIDTH,MIN_BOARD_HEIGHT,Board,BoardWidth,BoardHeight,PlayerPos);
       CalculateInternalData;
       Game.History.Count:=0; Game.History.Top:=0; // so 'CheckSnapshot' doesn't try to restore any old game from history

       {$IFDEF YASC}
          if (Sender=EditMenuItemSave) or (Sender=EditMenuItemSaveAs) then begin
             Board:=B; BoardWidth:=W; BoardHeight:=H;
             Result:=SaveLevel(nil); // 'nil': transfer the level to 'MainForm.Game'
             if   Result then
                  try
                    MainForm.Solver   .ImportGames(True);
                    MainForm.Optimizer.ImportGames((Screen.ActiveForm<>Self) or (PageControl1.ActivePage<>TabsheetOptimizer));
                    Result:=MainForm.Game.SaveToFile(MainForm.Game.FileName,True);
                    if   Result then begin
                         MainForm.Modified:=False; PluginLevelInfo.NewGamesCount:=0;
                         end
                    else Modified:=True;
                  finally
                    Game.SetName(MainForm.Game.Name);
                    Game.SokoFileName:=MainForm.Game.SokoFileName;
                    Editor.FileName:=MainForm.Game.FileName;
                    Editor.LastValidFileName:=MainForm.Game.LastValidFileName;
                    ShowTitle('',Editor.FileName);
                    if Editor.History<>nil then with Editor.History do begin
                       Close;
                       InitializeEditorHistory;
                       end;

                    if Assigned(OptimizerTaskQueue) and
                       (PageControl1.ActivePage=TabsheetOptimizer) then
                       with OptimizerTaskQueue do begin
                         Reload(False);
                         ScrollInView(StringGrid.Row);
                         if (StringGrid.Row>Succ(StringGrid.FixedRows)) or Assigned(Levels[StringGrid.Row]) then
                            OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,DummyBoolean);
                         end;
                    if Assigned(SolverTaskQueue) and
                       (PageControl1.ActivePage=TabsheetSolver) then
                       with SolverTaskQueue do begin
                         Reload(False);
                         ScrollInView(StringGrid.Row);
                         if (StringGrid.Row>Succ(StringGrid.FixedRows)) or Assigned(Levels[StringGrid.Row]) then
                            SolveLevelsStringGridSelectCell(nil,0,StringGrid.Row,DummyBoolean);
                         end;
                    if Assigned(GeneratorTaskQueue) and
                       (PageControl1.ActivePage=TabsheetGenerator) then
                       with GeneratorTaskQueue do begin
                         Reload(False);
                         ScrollInView(StringGrid.Row);
                         if   (StringGrid.Row>Succ(StringGrid.FixedRows)) or Assigned(Levels[StringGrid.Row]) then
                              GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,DummyBoolean)
                         else GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,DummyBoolean);
                         end;
                  end
             else Modified:=True;
             end
          else begin // transfer the game to 'MainForm.Game'
            s:=Editor.FileName;
            if (s='') or IsANewFileName(s) then begin
               s:=TEXT_LEVEL; MainForm.Game.Notes.Clear;
               end;
            if (Game.Name='') or IsANewFileName(Game.Name) then
               MainForm.Game.Notes.Clear;

            MainForm.Game.SetName(s);
            Game.SetName(s);
            MainForm.Game.FileName                 :=Editor.FileName;
            if   StrEqual(MainForm.Game.Name,TEXT_LEVEL) then begin           {special: if name = 'Level' then use filename as display-name}
                 if   IsAnIniFileSectionFileName(MainForm.Game.FileName) then
                      MainForm.Game.DisplayName:=ExtractSectionName(MainForm.Game.FileName)
                 else MainForm.Game.DisplayName:=ExtractFileNameWithoutPathAndExtension(MainForm.Game.FileName);
                 if IsBlank(MainForm.Game.DisplayName) then MainForm.Game.DisplayName:=MainForm.Game.Name;
                 end
            else if   IsAnIniFileSectionFileName(MainForm.Game.Name) then
                      MainForm.Game.DisplayName:=ExtractSectionName(MainForm.Game.Name)
                 else MainForm.Game.DisplayName:=ExtractFileNameWithoutPathAndExtension(MainForm.Game.Name);
            if Assigned(MainForm.Game.DeadlockDetection.Deadlocks) then
               MainForm.Game.DeadlockDetection.Deadlocks.Clear;
            MainForm.Game.Board                    :=Board;
            MainForm.Game.BoardWidth               :=BoardWidth;
            MainForm.Game.BoardHeight              :=BoardHeight;
            MainForm.Game.PlayerPos                :=PlayerPos;
            MainForm.Game.BoardTransformation2D    :=t2DRotate0DegreesClockwise;
            MainForm.Game.CalculateInternalData;
            MainForm.Game.CalculateScoreAndState;
            MainForm.Modified                      :=True;
            Editor.LevelHasChangedOrHasBeenModified:=True;
            Modified                               :=False;

            if CheckSnapshots(Game,MainForm.Game,False,i) then
               MainForm.Game.BoardTransformation2D:=t2DRotate0DegreesClockwise
            else begin
               MainForm.Game.ClearTimer; MainForm.Game.OriginalTimeMS:=0;
               end;

            if SnapshotsForm<>nil then begin
               if MainForm.Game.Snapshots.IsEmpty then begin
                  SnapshotsForm.Clear;
                  if MainForm.Game.BestSolutionsCount<>0 then SnapshotsForm.LoadSnapshots(nil);
                  end;
               SnapshotsForm.Grid.Cells[1,0]:=VisualFileName(MainForm.Game.FileName);
               end;
            Result                                 :=True;
            end;

       {$ELSE}
         if Modified then
            if CheckSnapshots(Game,Game,False,i) then
               MainForm.Game.BoardTransformation2D:=t2DRotate0DegreesClockwise;

         s:=Editor.FileName;
         if (s='') or IsANewFileName(s) then
             s:=TEXT_LEVEL;

         Result:=SaveLevelToFile(s);
         if Result then begin
            Editor.FileName:=s; Editor.LastValidFileName:=s;
            ShowTitle(Editor.FileName);
            Modified:=False;
            end;
       {$ENDIF}

     finally
       Board:=B; BoardWidth:=W; BoardHeight:=H;
       CalculateGameInformation(False); IsBusy:=oIsBusy;
       if Assigned(OpenForm) then
          if      PageControl1.ActivePage=TabSheetOptimizer then OpenForm.EnableDisablePluginControls(Self,MainForm.Optimizer,True)
          else if PageControl1.ActivePage=TabSheetSolver    then OpenForm.EnableDisablePluginControls(Self,MainForm.Solver   ,True)
          else if PageControl1.ActivePage=TabSheetGenerator then OpenForm.EnableDisablePluginControls(Self,MainForm.Generator,True);
       ShowStatus;
     end;
     end;
end;

procedure TToolsForm.ShowTitle(const SubSection,FileName:String);
var s,s1,s2:String;
begin
  s:=Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+Trim(PageControl1.ActivePage.Caption);
  if SubSection <> '' then s := s + SUB_TITLE_SEPARATOR + SubSection;
  if (FileName<>'')
     and
     ((PageControl1.ActivePage=TabSheetEditor)
      or
      (PageControl1.ActivePage=TabSheetSolver)
      or
      (PageControl1.ActivePage=TabSheetOptimizer)
      or
      (PageControl1.ActivePage=TabSheetGenerator)
      or
      (PageControl1.ActivePage=TabSheetCapture)
     )
     then
     if        PageControl1.ActivePage = TabSheetGenerator then begin
               if (MainForm.Generator.FileName<>'') then
                  s:=s+SUB_TITLE_SEPARATOR+VisualFileName(MainForm.Generator.FileName); // note: this shows the filename from the generator, not the passed 'FileName' parameter
               end
     else if   PageControl1.ActivePage = TabSheetCapture then begin
               if   not IsAnIniFileSectionFileName          ( FileName ) then
                    s:=s+SUB_TITLE_SEPARATOR+ExtractFileName( FileName )
               else s:=s+SUB_TITLE_SEPARATOR+VisualFileName ( FileName );
               end
     else if   IsAnIniFileSectionFileName( FileName ) then begin
               s:=s+SUB_TITLE_SEPARATOR;
               s1:=VisualFileName(FileName);
               if CollectionNameInTitleLine then begin
                  s2:=ExtractFileNameWithoutExt(ExtractIniFileName(FileName));
                  if not StrEqual(s1,s2) then
                     s:=s+s2+SPACE+COLON+SPACE;
                  end;
               s:=s+s1;
               end
          else s:=s+SUB_TITLE_SEPARATOR+VisualFileName(FileName);
  Caption:=s;
end;

{$IFNDEF YASC}
  function  TToolsForm.LoadLevelFromFile(var FileName__:String):Boolean;
  var Col,Row:Integer; ErrorStr,PackFileName,SectionName:String; oCursor:TCursor;
      Level:TLevel;
  begin
    Level:=nil; ErrorStr:='';

    try

      if IsAnIniFileSectionFileName(FileName__) then begin
         PackFileName:=ExtractIniFileName(FileName__);
         SectionName :=ExtractSectionName(FileName__);
         end
      else begin
         PackFileName:=FileName__; SectionName:=SECTION_NAME_FIRST;
         end;

      if   StrEqual(PackFileName,SokoFile.Name) or
           FileExists(PackFileName) then begin
           oCursor:=Screen.Cursor;
           try
             Screen.Cursor:=crHourGlass;

             if   SokoFile.Open(PackFileName) then
                  if   SokoFile.Levels.IsEmpty then
                       ErrorStr:=NoLevelsFoundText
                  else if        (SectionName=SECTION_NAME_FIRST) or IsBlank(SectionName) then
                                 Level:=TLevel(SokoFile.Levels.First)
                       else if   SectionName=SECTION_NAME_LAST  then
                                 Level:=TLevel(SokoFile.Levels.Last)
                            else begin
                                   Level:=TLevel(SokoFile.Levels.GetItemByName(SectionName));
                                   if   Level=nil then
                                        ErrorStr:=Format(PackFileMemberNotFoundText__,[SectionName,PackFileName]);
                                 end
             else ErrorStr:=TEXT_TASK_FAILED
           finally Screen.Cursor:=oCursor;
           end
           end

      else ErrorStr:=Format(TEXT_FILE_NOT_FOUND_FORMAT,[PackFileName]);

      Result:=Level<>nil;
      if Result then begin
         if   (SokoFile.Levels.Count=1) and
              (IsBlank(Level.Name) or StrEqual(Level.Name,TEXT_LEVEL)) then
              FileName__:=SokoFile.Name
         else FileName__:=MakeIniFileSectionFileName(SokoFile.Name,Level.Name);

         if   Game.LoadFromFile(Level.Name,SokoFile,ErrorStr) then begin
              for Col:=1 to MAX_BOARD_WIDTH do
                  for Row:=1 to MAX_BOARD_WIDTH do
                      if (Col>Game.BoardWidth) or (Row>Game.BoardHeight) then
                         Game.Board[Col,Row]:=FLOOR;

              CalculateGameInformation(True);

              Editor.BoardCellOrigin:=Point(1,1);
              HideCursor(False);
              Editor.FileName:=FileName__;
              ShowTitle(Editor.FileName);
              InitializeGameViewer(True);
              Self.ShowGame;

              Result:=True;
              end
         else if not Game.Verbose then raise Exception.Create(ErrorStr) // 'Verbose'='True': the error has already been reported
              else;
         end
      else raise Exception.Create(ErrorStr);

    except on E:Exception do begin
           Game.Clear; EditMenuItemNewClick(Self);
           Result:=Error(E.Message,Application.Title);
           end;
    end;
  end;

  function  TToolsForm.OpenDialog(Sender:TObject):Boolean;
  var s:String; Level:TLevel;

    function AddMenuItem(const Caption__:String):Boolean;
    var MenuItem:TMenuItem;
    begin
      Result:=True; exit; // not in production
      try
        MenuItem:=TMenuItem.Create(Self);
        EditPopupMenuOpen.Items.Add(MenuItem);
        MenuItem.Caption:=Caption__;
        //MenuItem.OnClick:=MenuItemLoadLevel;
        Result:=True;
      except on E:Exception do Result:=Error(E.Message,Application.Title);
      end;
    end;

  begin // OpenDialog
    Result:=False;
    if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
       if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
       else with OpenDialog1 do begin
         Title:=Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+PageControl1.ActivePage.Caption+SUB_TITLE_SEPARATOR+TEXT_OPEN_LEVEL;

         if not IsANewFileName(Editor.FileName) then
            InitialDir:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Editor.FileName));
         FileName:='';

         if Execute then begin
            Refresh;
            if FileExists(FileName) then begin
               with EditPopupMenuOpen do
                 while Items.Count<>0 do Items[0].Free;
               s:=FileName;
               if CloseLevel(Sender) then
                  if LoadLevelFromFile(s) then begin
                     Level:=TLevel(Game.SokoFile.Levels.First);
                     if (Level<>nil) and (Level.Next=nil) and (Level.Name=TEXT_LEVEL) then
                        AddMenuItem(Game.DisplayName)
                     else
                        while (Level<>nil) and AddMenuItem(Level.Name) do
                          Level:=TLevel(Level.Next);
                     Result:=True;
                     Editor.FileName:=s;
                     Editor.LastValidFileName:=s;
                     end
                  else begin
                     Game.Clear; EditMenuItemNewClick(Sender);
                     end;
               end
            else Msg(Format(FileNotFoundText__,[FileName]),Title,MB_OK+MB_ICONINFORMATION);
            end;
         end;
  end;
{$ENDIF}

function  TToolsForm.SaveDialog(Sender: TObject):Boolean;
var Dir,OrgName,s,s1:String; IsNew,IsCandidateSet, IsTaskQueue:Boolean;
begin // returns 'True' is the user completes the dialog and chooses a valid filename
  Result:=OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors;
  IsNew:=True;
  IsCandidateSet:=(Sender=GeneratorMenuItemSave) or (Sender=GeneratorMenuItemSaveAs);
  IsTaskQueue:=(Sender=PluginMenuItemSaveTaskQueueAs);

  if   Result then begin
       if ( not IsCandidateSet ) and ( not IsTaskQueue ) then begin
          SaveDialog1.Title:=Application.Title+SUB_TITLE_SEPARATOR+SaveLevelCaptionText;
          SaveDialog1.FileName:=Editor.FileName;
          end
       else
          if not IsTaskQueue then begin
             SaveDialog1.Title:=Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+Trim(PageControl1.ActivePage.Caption)+SUB_TITLE_SEPARATOR+SaveCandidateSetCaptionText;
             SaveDialog1.FileName:=MainForm.Generator.FileName;
             end
          else begin
             SaveDialog1.Title:=Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+Trim(PageControl1.ActivePage.Caption)+SUB_TITLE_SEPARATOR+SaveTaskQueueText;
             SaveDialog1.FileName := ''; //Trim(PageControl1.ActivePage.Caption) + SPACE + TaskQueueText;
             end;
       if        (Sender=EditMenuItemSave) or (Sender=GeneratorMenuItemSave) then
                 Sender:=Self
       else if   Sender=EditMenuItemSaveAs then begin
                 s:=SaveDialog1.FileName;
                 if IsAnIniFileSectionFileName(s) then begin
                    s1:=StrToFileName(ExtractSectionName(s));
                    if s1='' then s1:=TEXT_LEVEL;
                    if   MakeUniqueLevelFileName(ExtractFilePath(ExtractIniFileName(s)),s1,s) then
                         begin SaveDialog1.FileName:=s;
                         end
                    else begin Result:=False; Error(DiskFullText,Application.Title);
                         end;
                    end
                 else
                    if   not IsCandidateSet then
                         Sender:=EditMenuItemSave
                    else Sender:=GeneratorMenuItemSave;
                 end;

       IsNew:=IsANewFileName(SaveDialog1.FileName);
       end;

  if   Result
       and
       ((not IsAnIniFileSectionFileName(SaveDialog1.FileName))
        or IsNew
       ) then with SaveDialog1 do
       begin if IsNew then
                begin
                  //Sender:=EditMenuItemSaveAs; // No! if 'Sender=Self' the editor is handing over a modified level to the main window
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

                  if (Dir  ='') or (not DirectoryExists(Dir))  then Dir:=StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath)+DEFAULT_LEVEL_DIRECTORY;
                  if (Dir<>'') then Dir:=StrWithTrailingPathDelimiter(Dir);
                  if s1   =''  then
                     if   not IsCandidateSet then
                          if not IsTaskQueue then s1 := TEXT_LEVEL
                          else                    s1 := Trim(PageControl1.ActivePage.Caption) + SPACE + TaskQueueText
                     else                         s1 := MainForm.Generator.GeneratorName+SPACE+CandidateSetCaptionText;

                  s:=Dir+s1+SOKOBAN_FILE_NAME_EXT;
                  if   (s1<>TEXT_LEVEL) and     // '<>': a level name extracted from an inifile section-name
                       (not FileExists(s)) and
                       (not IsCandidateSet) and
                       (not IsTaskQueue) then
                  else s:='';

                  if   (s<>'') or
                       MakeUniqueLevelFileName(Dir,s1,s) then
                       begin FileName:=s;
                             OrgName:='';
                             InitialDir:=ExtractFilePath(FileName);
                             FileName  :=ExtractFileNameWithOutExt(FileName);

                       end
                  else begin Msg(DiskFullText,'',MB_OK+MB_ICONERROR);
                             Result:=False;
                       end;

                  s:=ExtractFileExt(s);

                  if IsCandidateSet and Assigned(MainForm.Generator) and MainForm.Generator.IsActive then
                     Sender:=Self; // 'Self': don't open the dialog; autosave the population during level generation;
                end
             else
                begin OrgName:=FileName;
                      InitialDir:=ExtractFilePath(FileName);
                      FileName  :=ExtractFileName(FileName);
                      s:=ExtractFileExt(FileName);
                end;

             if (s<>'') and (Sender<>Self) then begin // 's' = file extension
                if        StrEqual(s,SOKOBAN_FILE_NAME_EXT) or
                          StrEqual(s,XSB_FILE_NAME_EXT ) then FilterIndex:=1
                else if   StrEqual(s,TEXT_FILE_EXT) then FilterIndex:=2
                     else FilterIndex:=3;
                if (FilterIndex<>3) and (not IsNew) then FileName:=ChangeFileExt(FileName,'');
                end;

             if Result and
                ((Sender=Self) or Execute) then
                begin Refresh;
                      if ExtractFilePath(FileName)='' then
                         FileName:=StrWithTrailingPathDelimiter(InitialDir)+FileName;

                      if ExtractFileExt(FileName)='' then
                         if   FilterIndex=1 then
                              if   StrEqual(ChangeFileExt(FileName,XSB_FILE_NAME_EXT),OrgName) then
                                   FileName:=ChangeFileExt(FileName,XSB_FILE_NAME_EXT)
                              else FileName:=ChangeFileExt(FileName,SOKOBAN_FILE_NAME_EXT)
                         else if   FilterIndex=2 then
                                   FileName:=ChangeFileExt(FileName,TEXT_FILE_EXT)
                              else FileName:=ChangeFileExt(FileName,SOKOBAN_FILE_NAME_EXT);

                      if (Sender<>Self)
                         and
                         (AnsiCompareText(OrgName,FileName)<>0)
                         and
                         FileExists(FileName)
                         and
                         (Msg(Format(FileExistsText__,[FileName])+NL+NL+OverwriteItText,
                              Title,
                              MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)
                          <> IDYES) then
                          Result:=False              {cancel save operation}
                      else
                         if True then //FileNameOk(FileName) then
                            begin
                              if not ((Sender=Self) and IsNew) then begin
                                 //Game.FileName:=FileName;
                                 if not IsCandidateSet then begin
                                    if not IsTaskQueue then begin
                                       MainForm.SaveSnapshotToLogFile (Editor.FileName,FileName,''); // this doesn't really save a snapshot to the log file but records a "rename" operation
                                       MainForm.Solver   .RenameLevels(Editor.FileName,FileName);
                                       MainForm.Optimizer.RenameLevels(Editor.FileName,FileName);
                                       Editor.FileName:=FileName;
                                       Game.DisplayName:=ExtractFileNameWithoutPathAndExtension(Editor.FileName);
                                       end;
                                    end
                                 else begin
                                    Result:=MainForm.Generator.SokoFile.SetName(FileName);
                                    end;
                                 ShowTitle('',FileName);
                                 InitialDir:=ExtractFilePath(FileName);
                                 OpenDialog1.InitialDir:=InitialDir;
                                 OpenDialog1.FilterIndex:=FilterIndex;
                                 end
                              else if IsCandidateSet then
                                      Result:=MainForm.Generator.SokoFile.SetName(FileName);
                            end
                         else
                            begin
                              Result:=False;
                              //ShowMessage(Format(FileNameInvalidText,[FileName]));
                            end;
                end
             else Result:=False;
       end;
end;

procedure TToolsForm.SaveDialog1TypeChange(Sender: TObject);
begin // programmatic modifications of the save-dialog's fileName have no effect while the dialog is active; hence this procedure is useless
  with SaveDialog1 do begin
    if FileName<>'' then
       if      FilterIndex=1 then
               if   not StrEqual(ExtractFileExt(FileName),XSB_FILE_NAME_EXT) then
                    FileName:=ChangeFileExt(FileName,SOKOBAN_FILE_NAME_EXT) // doesn't work
               else //
       else if FilterIndex=2 then
               FileName:=ChangeFileExt(FileName,TEXT_FILE_EXT); // doesn't work
    end;
end;

function  TToolsForm.SaveLevelToFile(FileName__:String):Boolean;
var s,PackFileName,SectionName:String;
begin
  PackFileName:='';
  with Game do
    try
      if IsAnIniFileSectionFileName(FileName__) then begin // save level to a collection
         PackFileName:=ExtractIniFileName(FileName__);
         SectionName :=ExtractSectionName(FileName__);
         if   SokoFile.Open(PackFileName) then begin
              {$IFDEF YASC}
                 SokoFile.AddFileFormatDescriptionToFiles:=MainForm.AddFileFormatDescriptionToFiles;
              {$ELSE}
                 SokoFile.AddFileFormatDescriptionToFiles:=True;
              {$ENDIF}
              Result:=SetName(SectionName) and
                      TSokoGame(Game).SaveToFile(SokoFile,True,True,True,True,True);
              end
         else raise Exception.Create(Format(OpenFileFailedShortText__,[PackFileName]));
         end
      else begin                                           // save level to a single-level textfile
         {$IFDEF YASC}
            SokoFile.AddFileFormatDescriptionToFiles:=MainForm.AddFileFormatDescriptionToFiles;
         {$ELSE}
            SokoFile.AddFileFormatDescriptionToFiles:=False;
         {$ENDIF}
         if   SokoFile.New(FileName__) then begin
              if not Notes.Lines.ReadString(KEY_TITLE,s) then s:=TEXT_LEVEL;
              Result:=SetName(s) and
                      TSokoGame(Game).SaveToFile(SokoFile,True,True,True,True,True);
              end
         else raise Exception.Create(Format(OpenFileFailedShortText__,[FileName__]));
         end;
    except
      on E:Exception do begin
         if PackFileName='' then s:=FileName__
         else s:=PackFileName;
         Result:=Error(PChar(Format(SaveFileFailedText__,[s])+NL+NL+
                       TEXT_FAILURE_DESCRIPTION+NL+NL+E.Message),
                       SaveDialog1.Title+SUB_TITLE_SEPARATOR+ExtractFileName(s));
         end;
    end;
end;

function  TToolsForm.MakeNewLevelFileName:String;
var s:String;
begin
  if   NewLevelCount<High(NewLevelCount) then Inc(NewLevelCount)
  else NewLevelCount:=1;
  s:=Editor.FileName;
  if IsAnIniFileSectionFileName(s) then s:=ExtractIniFileName(s);
  Result:=ExtractFilePath(s);
  if Result='' then begin
     s:=Editor.LastValidFileName;
     if IsAnIniFileSectionFileName(s) then s:=ExtractIniFileName(s);
     Result:=ExtractFilePath(s);
     end;
  if Result='' then Result:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath)+DEFAULT_LEVEL_DIRECTORY);
  Result:=StrWithTrailingPathDelimiter(Result)+TITLE_ILLEGAL_FIRST_CHARACTER+NewText{+SPACE+IntToStr(NewLevelCount)}+TITLE_ILLEGAL_FIRST_CHARACTER;
end;

function  TToolsForm.MakeUniqueLevelFileName(const Path,FileNameStub:String; var FileName:String):Boolean;
begin
  FileName:=FileNameStub;
  if (FileName<>'') and (FileName[Length(FileName)]<>PERIOD) then
     FileName:=FileName+PERIOD;
  FileName:=Misc_.MakeNewFileName(
             StrWithTrailingPathDelimiter(Path)+FileName,
             SOKOBAN_FILE_NAME_EXT,
             True);
  Result:=FileName<>'';
end;

function TToolsForm.InitializeGameViewer(Center__:Boolean):Boolean;
var H,W:Integer; oAntialiasing:TAntiAliasing; oBoardCellOrigin:TPoint;
    R,R2:TRect; P:TPict;
begin
  Result:=False;
  Editor.UseCursorPictures:=False;
  if (Game<>nil) and
     (GameViewer.CursorPict<>nil) and
     (GameViewer.SkinPict<>nil) and
     (GameViewer.SkinPict.OrgBitMap<>nil) and
     (GameViewer.SkinPict.FrameCount>0) then with GameViewer do
     try
       W       :=EditImage1.ClientWidth -2*MIN_BORDER_SIZE;
       H       :=EditImage1.ClientHeight-2*MIN_BORDER_SIZE;

       if (Game.BoardWidth=0) and (Game.BoardHeight=0) and
          Editor.Selection.HasBoard then
          Editor.BoardCellOrigin:=Point(Editor.Selection.Rect.Left,Editor.Selection.Rect.Top);

       with Editor.BoardCellOrigin do R2:=Rect(X,Y,X+Game.BoardWidth,Y+Game.BoardHeight);
       if (not Editor.Selection.HasBoard) or
          (not UnionRect(R,R2,Editor.Selection.Rect)) then // 'R': union rectangle for board and selected area
          R:=R2; // 'R': board rectangle (there is no selected area)

       ColCount:=Min(MAX_BOARD_WIDTH ,Max(W div MAX_EDITOR_SQUARE_SIZE,Max(MIN_BOARD_WIDTH,R.Right-R.Left+2*EDITOR_GUTTER_SQUARE_COUNT)));
       RowCount:=Min(MAX_BOARD_HEIGHT,Max(H div MAX_EDITOR_SQUARE_SIZE,Max(MIN_BOARD_HEIGHT,R.Bottom-R.Top+2*EDITOR_GUTTER_SQUARE_COUNT)));

       ColWidth:=Min(MAX_EDITOR_SQUARE_SIZE,Min(W div ColCount,H div RowCount));

       if Odd(ColWidth) then Dec(ColWidth);
       ColWidth :=Max(MIN_COL_WIDTH ,ColWidth);
       RowHeight:=Max(MIN_ROW_HEIGHT,ColWidth);

       if      SkinPict.OrgBitMap.Width div SkinPict.FrameCount>Succ(SkinPict.OrgBitMap.Height) then begin
               RowHeight:=Max(1,ColWidth*SkinPict.OrgBitMap.Height div (SkinPict.OrgBitMap.Width div SkinPict.FrameCount));
               if Odd(RowHeight) and (RowHeight<ColWidth) then Inc(RowHeight);
               end
       else if SkinPict.OrgBitMap.Width<Pred(SkinPict.OrgBitMap.Height) then begin
               ColWidth:=Max(1,RowHeight*(SkinPict.OrgBitMap.Width div SkinPict.FrameCount) div SkinPict.OrgBitMap.Height);
               if Odd(ColWidth) and (ColWidth<RowHeight)  then Inc(ColWidth);
               end;

       ColCount:=Min(MAX_BOARD_WIDTH ,Max(ColCount,W div ColWidth ));
       RowCount:=Min(MAX_BOARD_HEIGHT,Max(RowCount,H div RowHeight));

       with Editor.BoardCellOrigin do begin
         oBoardCellOrigin:=Editor.BoardCellOrigin;
         W:=R.Right-R.Left; H:=R.Bottom-R.Top; // required columns, rows

         if X>=R.Left then begin
            if Center__ or (R.Right>=ColCount) then //(R.Right >=Pred(ColCount)) then
               X:=Max(0,(X-R.Left)+((Succ(ColCount)-W) div 2));
            if (X=0) and (R.Right <ColCount) then X:=1;
            end;
         if Y>=R.Top then begin
            if Center__ or (R.Bottom>=RowCount) then //(R.Bottom>=Pred(RowCount)) then
               Y:=Max(0,(Y-R.Top )+((Succ(RowCount)-H) div 2));
            if (Y=0) and (R.Bottom<RowCount) then Y:=1;
            end;

         Editor.CursorRect    :=RectPlusOffset(Editor.CursorRect    ,X-oBoardCellOrigin.X,Y-oBoardCellOrigin.Y);
         Editor.Selection.Rect:=RectPlusOffset(Editor.Selection.Rect,X-oBoardCellOrigin.X,Y-oBoardCellOrigin.Y);
         end;

       with Editor.Cursors[ctCell     ] do Size:=Max(MAX_CURSOR_PEN_WIDTH,Min(ColWidth,RowHeight) div 4);
       with Editor.Cursors[ctEraser   ] do Size:=Editor.Cursors[ctCell].Size;
       with Editor.Cursors[ctSelection] do Size:=Editor.Cursors[ctCell].Size;

       Editor.DrawingToolsEnabled:=ResizeImage(EditImage1);
       with EditImage1.Picture.BitMap do begin
         CellsPixelOrigin.X:=(Width -(ColCount*ColWidth )) div 2;
         CellsPixelOrigin.Y:=(Height-(RowCount*RowHeight)) div 2;
         end;

       SkinInitialized:=SkinPict.ResizeFrames(ColWidth,RowHeight,SkinPict.FrameCount,nil);

       if SkinInitialized and
          ((CursorPict.BitMap=nil) or (Editor.LastCursorPict=dtNone)) then
          with CursorPict do begin
            oAntialiasing:=Antialiasing; SkinInitialized:=False;
            try     OrgBitMap:=SkinPict.OrgBitMap;
                    W:=DRAWING_TOOL_CURSOR_PICTURE_SIZE;
                    H:=DRAWING_TOOL_CURSOR_PICTURE_SIZE*RowHeight div ColWidth;
                    if Odd(H) then Inc(H);
                    AntiAliasing:=aaBilinear;
                    SkinInitialized:=CursorPict.ResizeFrames(W,H,CursorPict.FrameCount,nil);
                    if SkinInitialized then Editor.LastCursorPict:=dtPlayer;

                    if SkinInitialized and (EditImageList1.Count>0)
                       and (EditImageList1.Width<>0) and (EditImageList1.Height<>0) then
                       try    P:=TPict.Create;
                              try     P.AntiAliasing:=aaBilinear;
                                      if BitMapResize(P.OrgBitMap,
                                                      EditImageList1.Height,// use 'Height' instead of 'Width' because the dodumentation says that 'Width' is the total width for all images but in practice it seems to be the width of a single image
                                                      //EditImageList1.Width {div EditImageList1.Count},
                                                      EditImageList1.Height) then begin
                                         EditImageList1.Draw(P.OrgBitMap.Canvas,0,0,ERASE_PICTURE_INDEX);
                                         P.Masked:=True;
                                         P.MaskBitMapPct:=MaskBitMapPct;
                                         P.MaskBitMapColor:=ColorToRGB(P.OrgBitMap.Canvas.Pixels[0,Pred(OrgBitMap.Height)]);
                                         if P.Resize(W,H) then begin
                                            if BitMap<>nil then begin
                                               BitMap.Canvas.CopyMode:=cmSrcCopy;
                                               BitMap.Canvas.CopyRect(FrameRect(TILE_INDEX_ERASE),P.BitMap.Canvas,Rect(0,0,P.BitMap.Width,P.BitMap.Height));
                                               Editor.LastCursorPict:=dtErase;
                                               end;
                                            if (MaskBitMap<>nil) and (P.MaskBitMap<>nil) then begin
                                               MaskBitMap.Canvas.CopyMode:=cmSrcCopy;
                                               MaskBitMap.Canvas.CopyRect(FrameRect(TILE_INDEX_ERASE),P.MaskBitMap.Canvas,Rect(0,0,P.MaskBitMap.Width,P.MaskBitMap.Height));
                                               end;
                                            end;
                                         end;
                              finally P.Free;
                              end;
                       except on E:Exception do SkinInitialized:=Error(E.Message,Application.Title);
                       end;
            finally OrgBitMap:=nil; AntiAliasing:=oAntialiasing;
            end;
            end;

       if (not SkinInitialized) and (CursorPict<>nil) then CursorPict.Clear;

       with BackgroundPict do
         BackgroundInitialized:=(BackgroundInitialized
                                 and
                                 (BitMap<>nil)
                                 and
                                 (EditImage1.Picture.BitMap.Width =BitMap.Width)
                                 and
                                 (EditImage1.Picture.BitMap.Height=BitMap.Height)
                                )
                                or
                                SetView(View,EditImage1.Picture.BitMap.Width,EditImage1.Picture.BitMap.Height,BackgroundPict.Color);
       Result:=BackgroundInitialized and SkinInitialized and Editor.DrawingToolsEnabled;

       with CursorPict do
         Editor.UseCursorPictures:=Result and
                                   (BitMap<>nil) and
                                   (FrameCount<>0) and
                                   ((BitMap.Width div FrameCount)<=ColWidth-2*Max(Editor.Cursors[ctCell].PenWidth,Editor.Cursors[ctSelection].PenWidth)-2-4) and
                                   (BitMap.Height<=RowHeight-2*Max(Editor.Cursors[ctCell].PenWidth,Editor.Cursors[ctSelection].PenWidth)-2-4);

     finally
     end
  else begin
     GameViewer.BackgroundInitialized:=False; GameViewer.SkinInitialized:=False;
     Editor.UseCursorPictures:=False;
     end;
end;

procedure TToolsForm.ShowGame(ShowStatus__:Boolean);
begin
  if Game<>nil then with GameViewer do begin
     ShowGameBackground;
     ShowSquares;
     EditImage1MouseUp(nil,mbLeft,[],0,0);
     with Editor do SetCursor(Cursor,CursorRect);
     SetDrawingToolHint;
     if ShowStatus__ then ShowStatus;
     end;
end;

procedure TToolsForm.ShowGameBackground;
begin
  with GameViewer do
    if BackgroundInitialized or InitializeGameViewer(False) then begin
       EditImage1.Canvas.CopyMode:=cmSrcCopy;
       if (BackgroundPict.BitMap<>nil) and
          BackgroundPict.Visible then EditImage1.Canvas.Draw(0,0,BackgroundPict.BitMap)
       else with EditImage1 do with Canvas do begin
          Brush.Style:=bsSolid; Brush.Color:=BackgroundPict.Color;
          FillRect(Rect(0,0,ClientWidth,ClientHeight));
          end;
       end;
end;

procedure TToolsForm.ShowSquares;
var Col,Row:Integer;
begin
  if (Game<>nil) and GameViewer.SkinInitialized then with Game do
     for Col:=-1 to GameViewer.ColCount do
         for Row:=-1 to GameViewer.RowCount do ShowSquare(Col,Row);
end;

procedure TToolsForm.ShowSquare(Col__,Row__:Integer); // 0-based cell-coordinate columns and rows
var i,j,C,R,X,Y:Integer; CornerSet:TCornerTypeSet; Rect:TRect;
begin
  with GameViewer do with EditImage1.Picture.BitMap.Canvas do
    if True then begin
       CellToPos(Col__,Row__,X,Y); Rect:=Classes.Rect(X,Y,X+ColWidth,Y+RowHeight);
       if Misc_.ClipRect(Rect,Classes.Rect(0,0,EditImage1.Picture.BitMap.Width,EditImage1.Picture.BitMap.Height)) then begin

          if    FloorTilesVisible and SkinInitialized and
                (Col__>=0) and (Col__<ColCount) and
                (Row__>=0) and (Row__<RowCount) then
                SkinPict.DrawRect(Rect.Left,Rect.Top,SkinPict.FrameRect(TILE_INDEX_FLOOR),EditImage1.Picture.BitMap.Canvas)
          else
             if BackgroundInitialized and BackgroundPict.Visible then begin
                CopyMode:=cmSrcCopy;
                CopyRect(Rect,BackgroundPict.BitMap.Canvas,Rect);
                end
             else begin
                Brush.Style:=bsSolid; Brush.Color:=BackgroundPict.Color;
                FillRect(Rect);
                end;

          if SkinInitialized and
             (Rect.Left+ColWidth=Rect.Right) and (Rect.Top+RowHeight=Rect.Bottom) then begin
             C:=Succ(Col__-Editor.BoardCellOrigin.X);
             R:=Succ(Row__-Editor.BoardCellOrigin.Y);

             if   (C>=1) and (C<=Game.BoardWidth ) and
                  (R>=1) and (R<=Game.BoardHeight) then
                  i:=Game.Board[C,R]
             else i:=0;

             with Editor do with Selection do
               if HasBoard and
                  (Col__>=Rect.Left) and (Col__<Rect.Right) and
                  (Row__>=Rect.Top ) and (Row__<Rect.Bottom) then
                  i:=CharToBoardSquareValue(BoardAsText[Succ((Col__-Rect.Left)+((Row__-Rect.Top)*(BoardWidth+Length(NL))))]);

             if i<>0 then begin
                j:=-1;
                if (i and WALL)<>0 then j:=TILE_INDEX_WALL
                else if (i and GOAL)<>0 then
                        if (i and BOX)<>0 then j:=TILE_INDEX_BOX_ON_GOAL
                        else if (i and PLAYER)<>0 then j:=TILE_INDEX_PLAYER_ON_GOAL
                             else j:=TILE_INDEX_GOAL
                     else
                        if (i and BOX)<>0 then j:=TILE_INDEX_BOX
                        else if (i and PLAYER)<>0 then j:=TILE_INDEX_PLAYER;

                if j<>-1 then begin
                   if (i and GOAL)<>0 then
                      SkinPict.DrawRect(Rect.Left,Rect.Top,SkinPict.FrameRect(TILE_INDEX_GOAL),EditImage1.Picture.BitMap.Canvas);
                   SkinPict.DrawRect(Rect.Left,Rect.Top,SkinPict.FrameRect(j),EditImage1.Picture.BitMap.Canvas);
                   end;
                end;
             end;

          if (CellsPixelOrigin.X>=2) and (CellsPixelOrigin.Y>=2) then with Rect do begin
             Pen.Style:=psSolid; Pen.Width:=1;

             if Col__=0 then begin
                if   Row__=0 then begin
                     Pen.Color:=FrameShadowColor;
                     X:=Pred(Left); Y:=Pred(Top);
                     MoveTo(X,Y); LineTo(X,Y+RowHeight);
                     MoveTo(X,Y); LineTo(X+ColWidth,Y);

                     Pen.Color:=FrameColor;
                     Dec(X); Dec(Y);
                     MoveTo(X,Y); LineTo(X,Y+RowHeight);
                     MoveTo(X,Y); LineTo(X+ColWidth,Y);
                     end;
                if   Row__=Pred(RowCount) then begin
                     Pen.Color:=FrameShadowColor;
                     X:=Pred(Left); Y:=Succ(Bottom);
                     MoveTo(X,Y); LineTo(X,Y-RowHeight);
                     MoveTo(X,Y); LineTo(X+ColWidth,Y);

                     Pen.Color:=FrameColor;
                     Dec(X); Dec(Y);
                     MoveTo(X,Y); LineTo(X,Y-RowHeight);
                     MoveTo(X,Y); LineTo(X+ColWidth,Y);
                     end;
                end;
             if Col__=Pred(ColCount) then begin
                if   Row__=0 then begin
                     Pen.Color:=FrameShadowColor;
                     X:=Succ(Right); Y:=Pred(Top);
                     MoveTo(X,Y); LineTo(X,Y+RowHeight);
                     MoveTo(X,Y); LineTo(X-ColWidth,Y);

                     Pen.Color:=FrameColor;
                     Dec(X); Dec(Y);
                     MoveTo(X,Y); LineTo(X,Y+RowHeight);
                     MoveTo(X,Y); LineTo(X-ColWidth,Y);
                     end;
                if   Row__=Pred(RowCount) then begin
                     Pen.Color:=FrameShadowColor;
                     X:=Succ(Right); Y:=Succ(Bottom);
                     MoveTo(X,Y); LineTo(X,Y-RowHeight);
                     MoveTo(X,Y); LineTo(X-ColWidth,Y);

                     Pen.Color:=FrameColor;
                     Dec(X); Dec(Y);
                     MoveTo(X,Y); LineTo(X,Y-RowHeight);
                     MoveTo(X,Y); LineTo(X-ColWidth,Y);
                     end;
                end;

             if EditMenuItemGrid.Checked and
                (Col__>=0) and (Col__<ColCount) and (Row__>=0) and (Row__<RowCount) and
                (ColCount>1) and (RowCount>1) then with Rect do begin
                CornerSet:=[ctTopLeft,ctTopRight,ctBottomLeft,ctBottomRight];
                if Col__=0 then
                   if      Row__=0 then CornerSet:=[ctBottomRight]
                   else if Row__=1 then Exclude(CornerSet,ctTopLeft)
                   else if Row__=RowCount-2 then Exclude(CornerSet,ctBottomLeft)
                   else if Row__=Pred(RowCount) then CornerSet:=[ctTopRight]
                   else
                else if Col__=1 then
                   if      Row__=0 then Exclude(CornerSet,ctTopLeft)
                   else if Row__=Pred(RowCount) then Exclude(CornerSet,ctBottomLeft)
                   else
                else if Col__=ColCount-2 then
                   if      Row__=0 then Exclude(CornerSet,ctTopRight)
                   else if Row__=Pred(RowCount) then Exclude(CornerSet,ctBottomRight)
                   else
                else if Col__=Pred(ColCount) then
                   if      Row__=0 then CornerSet:=[ctBottomLeft]
                   else if Row__=1 then Exclude(CornerSet,ctTopRight)
                   else if Row__=RowCount-2 then Exclude(CornerSet,ctBottomRight)
                   else if Row__=Pred(RowCount) then CornerSet:=[ctTopLeft];

                if ctTopLeft in CornerSet then begin
                   Pen.Color:=FrameShadowColor; MoveTo(Pred(Left),Pred(Top)); LineTo(Succ(Left),Pred(Top)); MoveTo(Pred(Left),Top); LineTo(Succ(Left),Top);
                   Pen.Color:=FrameColor; MoveTo(Pred(Left),Pred(Top)); LineTo(Left,Pred(Top));
                   end;
                if ctTopRight in CornerSet then begin
                   Pen.Color:=FrameShadowColor; MoveTo(Pred(Right),Pred(Top)); LineTo(Succ(Right),Pred(Top)); MoveTo(Pred(Right),Top); LineTo(Succ(Right),Top);
                   Pen.Color:=FrameColor; MoveTo(Pred(Right),Pred(Top)); LineTo(Right,Pred(Top));
                   end;
                if ctBottomLeft in CornerSet then begin
                   Pen.Color:=FrameShadowColor; MoveTo(Pred(Left),Pred(Bottom)); LineTo(Succ(Left),Pred(Bottom)); MoveTo(Pred(Left),Bottom); LineTo(Succ(Left),Bottom);
                   Pen.Color:=FrameColor; MoveTo(Pred(Left),Pred(Bottom)); LineTo(Left,Pred(Bottom));
                   end;
                if ctBottomRight in CornerSet then begin
                   Pen.Color:=FrameShadowColor; MoveTo(Pred(Right),Pred(Bottom)); LineTo(Succ(Right),Pred(Bottom)); MoveTo(Pred(Right),Bottom); LineTo(Succ(Right),Bottom);
                   Pen.Color:=FrameColor; MoveTo(Pred(Right),Pred(Bottom)); LineTo(Right,Pred(Bottom));
                   end;
                end;

             if      Col__=ColCount then begin
                     ShowSquare(Pred(ColCount),Row__);
                     if Row__<>0 then ShowSquare(Pred(ColCount),0);
                     ShowSquare(Pred(ColCount),Pred(RowCount));
                     end
             else if Col__=-1       then begin
                     ShowSquare(0,Row__);
                     if Row__<>0 then ShowSquare(0,0);
                     ShowSquare(0,Pred(RowCount));
                     end;
             if      Row__=RowCount then begin
                     ShowSquare(Col__,Pred(RowCount));
                     if Col__<>0 then ShowSquare(0,Pred(RowCount));
                     ShowSquare(Pred(ColCount),Pred(RowCount));
                     end
             else if Row__=-1       then begin
                     ShowSquare(Col__,0);
                     if Col__<>0 then ShowSquare(0,0);
                     ShowSquare(Pred(ColCount),0);
                     end;
             end;
          end;
       end;
end;

procedure TToolsForm.ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if   (Msg.Message = WM_MOUSEWHEEL) then begin
       if      PageControl1.ActivePage=TabSheetEditor then begin
               if Msg.WParam>=0 then EditImage1MouseWheelUp  (Self,[],Point(0,0),Handled)
               else                  EditImage1MouseWheelDown(Self,[],Point(0,0),Handled);
               end
       else if (PageControl1.ActivePage=TabSheetCapture) and Assigned(CaptureForm) then
               CaptureForm.ApplicationOnMessage(Msg,Handled)
{
       else if (Msg.hwnd=OptimizeSolutionsStringGrid.Handle) then with OptimizeSolutionsStringGrid do begin
               if Msg.WParam>=0 then begin
                  if Row>FixedRows   then Row:=Pred(Row);
                  end
               else begin
                  if Row<Pred(RowCount) then Row:=Succ(Row);
                  end;
               Handled:=True;
               end
}
       else if (Msg.hwnd=OptimizeSolutionsStringGrid.Handle) then begin
               end
       else if (Msg.hwnd=SolveLevelsStringGrid.Handle) then begin
               end
       else if (Msg.hwnd=GenerateLevelsStringGrid.Handle) then begin
               end
       else if (Msg.hwnd=OptimizationComboBox.Handle) then with OptimizationComboBox do with Items do begin
               if Msg.WParam>=0 then begin
                  if ItemIndex>0 then ItemIndex:=Pred(ItemIndex);
                  end
               else if ItemIndex<Pred(Count) then ItemIndex:=Succ(ItemIndex);
               StatusText:=Items[ItemIndex];
               Handled:=True;
               end
       else if (ActiveControl is TCheckBox) and
               (PageControl1.ActivePage=TabSheetOptimizer) and
               (Msg.hwnd=ActiveControl.Handle) then with OptimizerTaskQueue do with StringGrid do begin
               Plugin.Enter;
               try     ActiveControl:=StringGrid;
                       Col:=Ord(oscSelect);
                       if   Msg.WParam>=0 then
                            if Row>FixedRows then Row:=Pred(Row)
                            else
                       else if Row<Pred(RowCount) then Row:=Succ(Row);
               finally Plugin.Leave;
               end;
               end
       else if (PageControl1.ActivePage=TabSheetSolver)
               or
               (PageControl1.ActivePage=TabSheetOptimizer)
               or
               (PageControl1.ActivePage=TabSheetGenerator)
               then begin
               UndoRedoMove(Msg.WParam>=0);
               Handled:=True;
               end
       else Handled:=False;
       end
  else if   Msg.Message = WM_SYSCOMMAND then begin
            if        (((Msg.wParam and $fff0)= SC_SCREENSAVE) {or ((Msg.wParam and $fff0)= SC_MONITORPOWER)})
                      and
                      (not MainForm.ScreenSaverEnabled) then
                      Handled := True // disable screensaver {and 'monitor power off'}
            else if   ((Msg.wParam and $fff0)=SC_MINIMIZE) or ((Msg.wParam and $fff0)=SC_ICON) then begin
                      if Assigned(MainForm) and
                         MainForm.Visible and
                         (MainForm.WindowState<>wsMinimized) then begin
                         MainForm.WindowState:=wsMinimized;
                         MainForm.Update;
                         end;
                      if Assigned(SnapshotsForm) and
                         SnapshotsForm.Visible and
                         (SnapshotsForm.WindowState<>wsMinimized) then
                         SnapshotsForm.WindowState:=wsMinimized;
                      Handled:=False;
                      end
                 else Handled:=False;
            end
  else Handled:=False;
end;

procedure TToolsForm.EditImage1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  with Editor do begin
    //VertScrollBar.Position:=Max(0,Min(VertScrollBar.Position+Max(1,GameViewer.RowHeight),VertScrollBar.Range-ClientHeight));
    //HideCursor(False); //UpdateCursor;
    if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
       (not Game.IsBusy) and
       CloseEditors then
       if      PageControl1.ActivePage=TabSheetEditor then begin
               if DrawingTool<dtSelect then
                  EditMenuItemDrawingToolClick(EditMenuObjects.Items[Ord(DrawingTool)+Ord(DrawingTool=Pred(dtFloor))]);
               end
       else if PageControl1.ActivePage=TabSheetCapture then begin
               if CaptureForm.Editor.DrawingTool<dtErase then
                  EditMenuItemDrawingToolClick(EditMenuObjects.Items[Ord(DrawingTool)]);
               end;
    end;
  Handled:=True;
end;

procedure TToolsForm.EditImage1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  with Editor do begin
    //VertScrollBar.Position:=Max(0,VertScrollBar.Position-Max(1,GameViewer.RowHeight));
    //HideCursor(False); //UpdateCursor;
    if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
       (not Game.IsBusy) and
       CloseEditors and
       (DrawingTool>dtWall) and
       (PageControl1.ActivePage=TabSheetEditor) then
       EditMenuItemDrawingToolClick(EditMenuObjects.Items[Ord(DrawingTool)-2-Ord(DrawingTool=Succ(dtFloor))]);
    end;
  Handled:=True;
end;

procedure TToolsForm.CellToPos(Col__,Row__:Integer; var X__,Y__:Integer); // 0-based columns, rows
begin
  with GameViewer do begin
    X__:=CellsPixelOrigin.X+Col__*ColWidth;
    Y__:=CellsPixelOrigin.Y+Row__*RowHeight;
    end;
end;

function TToolsForm.CellRectToGameBoardRect(const Rect__:TRect):TRect; // input: 0-based columns, rows; output: 1-based columns, rows
begin
  with Editor do with Rect__ do begin
    Result.Left  :=Succ(Left-BoardCellOrigin.X);
    Result.Top   :=Succ(Top -BoardCellOrigin.Y);
    Result.Right :=Result.Left+Right-Left;
    Result.Bottom:=Result.Top +Bottom-Top;
    end;
end;

function TToolsForm.CellValue(Col__,Row__:Integer):Integer; // 0-based columns, rows
begin
  Dec(Col__,Pred(Editor.BoardCellOrigin.X));
  Dec(Row__,Pred(Editor.BoardCellOrigin.Y));
  if   (Col__>=1) and (Col__<=Game.BoardWidth ) and
       (Row__>=1) and (Row__<=Game.BoardHeight) then
       Result:=Game.Board[Col__,Row__]
  else Result:=FLOOR;
end;

procedure TToolsForm.MouseToCell(X__,Y__:Integer; var Col__,Row__:Integer); // 0-based col,row
begin
  with GameViewer do begin
    if   ColWidth<>0  then
         if X__>=CellsPixelOrigin.X then Col__:=(X__-CellsPixelOrigin.X) div ColWidth
         else Col__:=Pred((X__-CellsPixelOrigin.X) div ColWidth)
    else Col__:=-1;
    if   RowHeight<>0 then
         if Y__>=CellsPixelOrigin.Y then Row__:=(Y__-CellsPixelOrigin.Y) div RowHeight
         else Row__:=Pred((Y__-CellsPixelOrigin.Y) div RowHeight)
    else Row__:=-1;
    end;
end;

procedure TToolsForm.HideCursor(UpdateScreenCursor__:Boolean);
begin
  if Game<>nil then with Editor do begin
     CloseEditorSelection(True,False);
     if (UpdateScreenCursor__) and (Screen.Cursor<>EditImage1.Cursor) and (PageControl1.ActivePage=TabSheetEditor) then
        Screen.Cursor:=EditImage1.Cursor;
     MouseButtonDown:=False; SizeHandle:=ghNull; IsDragging:=False; IsSelectingANewArea:=False;
     Editor.Selection.Enabled:=False;
     SetCursor(Cursor,Rect(0,0,0,0));
     SetDrawingToolHint;
     ShowStatus;
     end;
end;

procedure TToolsForm.SetCursor(Cursor__:TEditorCursorType; CursorRect__:TRect);
var i,j,Col,Row:Integer; Rect:TRect;

  function DrawingToolToPictureTileIndex(DrawingTool__:TDrawingTool):Integer;
  const DEFAULT_TILE_SET_TILE_INDICES:array[TDrawingTool] of Integer =
          (-1,TILE_INDEX_WALL,TILE_INDEX_BOX,TILE_INDEX_GOAL,TILE_INDEX_PLAYER,-1,TILE_INDEX_ERASE,-1);
  begin
    Result:=DEFAULT_TILE_SET_TILE_INDICES[DrawingTool__];
  end;

  procedure ShowCursor(const CursorRectPixels__:TRect; PenWidth__,Size__:Integer; PenColor__,ShadowColor__:TColor; Corners__:TCornerTypeSet);
  var i,dx,dy,PenWidth:Integer; Points:array[0..7] of TPoint;
  begin // precondition: the cursor-rectangle is a legal rectangle on the destination canvas
    with EditImage1.Picture.BitMap.Canvas do with GameViewer do begin
         //LockWindowUpdate(Handle);
         Pen.Color     := PenColor__;
         Pen.Mode      := PmCopy;
         Pen.Style     := psSolid;
         Pen.Width     := 1;
         PenWidth      := PenWidth__;
         if (ColWidth<=2*PenWidth+2) or (RowHeight<=2*PenWidth+2) then PenWidth :=1;

         with CursorRectPixels__ do begin
           Points[0].X:=Left;                       Points[0].Y:=Top;             // top    left
           Points[1].X:=Right-2;                    Points[1].Y:=Points[0].Y;     // top    right, note: 1 pixel left for the shadow
           Points[2].X:=Points[0].X;                Points[2].Y:=Bottom-2;        // bottom left,  note: 1 pixel left for the shadow
           Points[3].X:=Points[1].X;                Points[3].Y:=Points[2].Y;     // bottom right
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
         //LockWindowUpdate(0);
         end;
   end;

begin // SetCursor
  with Editor do with CursorRect do begin
    for Col:=Left to Pred(Right) do
        for Row:=Top to Pred(Bottom) do // hide current cursor by showing the cells normally
            ShowSquare(Col,Row);

    Cursor:=Cursor__;
    CursorRect:=CursorRect__;
    CellToPos(CursorRect.Left,CursorRect.Top,CursorRectPixels.Left,CursorRectPixels.Top);
    CellToPos(CursorRect.Right,CursorRect.Bottom,CursorRectPixels.Right,CursorRectPixels.Bottom);
    ClipRect(CursorRectPixels,Classes.Rect(0,0,EditImage1.Picture.BitMap.Width,EditImage1.Picture.BitMap.Height));

//  Caption:=Format('[%d:%d]',[CursorRect.Left,CursorRect.Top]);

    if (CursorRectPixels.Left<=CursorRectPixels.Right -2) and
       (CursorRectPixels.Top <=CursorRectPixels.Bottom-2) then with CursorRect do begin
       if (Left<Right) and (Top<Bottom)
          and
          (Editor.Selection.Enabled
           or
           ((Left>=0) and (Top>=0) and
            (Right<=GameViewer.ColCount) and (Bottom<=GameViewer.RowCount)
           )
          )
          then begin
          j:=CellValue(Left,Top);
          if (Left>=0) and (Top>=0) and
             (Right<=GameViewer.ColCount) and (Bottom<=GameViewer.RowCount) and
             (DrawingTool>=dtWall) and(DrawingTool<=LastCursorPict) and
             Editor.UseCursorPictures and
             ((j and DRAWING_TOOL_TO_ITEMS[DrawingTool])=0)
             then with GameViewer.CursorPict do begin
             i:=DrawingToolToPictureTileIndex(DrawingTool);
             if ((i=TILE_INDEX_PLAYER) or (i=TILE_INDEX_BOX)) and
                ((j and GOAL)<>0) then Inc(i);
             Rect:=FrameRect(i);
             DrawRect(CursorRectPixels.Left+((GameViewer.ColWidth -(Rect.Right -Rect.Left)) div 2),
                      CursorRectPixels.Top +((GameViewer.RowHeight-(Rect.Bottom-Rect.Top )) div 2),
                      Rect,
                      EditImage1.Picture.BitMap.Canvas);
             end;

          with Editor.Selection do
            if HasBoard then
               for Col:=Left to Pred(Right) do
                   for Row:=Top to Pred(Bottom) do
                       ShowSquare(Col,Row);

          with Cursors[Cursor] do
            if        Editor.Selection.Enabled then
                      ShowCursor(CursorRectPixels,PenWidth,0,PenColor,ShadowColor,[ctTopLeft,ctTopRight,ctBottomLeft,ctBottomRight])
            else if   Cursor=ctEraser then
                      ShowCursor(CursorRectPixels,PenWidth,Size,PenColor,ShadowColor,[ctTopLeft,ctTopRight,ctBottomLeft,ctBottomRight])
                 else ShowCursor(CursorRectPixels,PenWidth,Size,Cursors[ctCell].PenColor,Cursors[ctCell].ShadowColor,[ctTopLeft,ctTopRight,ctBottomLeft,ctBottomRight]);
          end;
       end
    else
       if (CursorRect.Left<CursorRect.Right) or
          (CursorRect.Top <CursorRect.Bottom) then HideCursor(True);
    end;
end;

//procedure TToolsForm.AnimateButtonMouseClick(Button__:TToolButton);
//begin
//  if Button__.Enabled then with Button__ do begin
//     //StatusText := 'Pausing...'; StatusBar1.Refresh;
//     SleepEx(BUTTON_MOUSE_CLICK_ANIMATION_MS,False);
//     //StatusText := '';
//     end;
//end;

procedure TToolsForm.SetDrawingToolHint;
var Col,Row,Value:Integer; Point:TPoint;
begin // the hint depends on the current editor cursor state
  with Editor do with CursorRect do
    if (Left<GameViewer.ColCount) and (Top<GameViewer.RowCount) and
       (Right>0) and (Bottom>0) and
       DrawingToolsEnabled then begin
       if        (DrawingTool=dtNone) or
                 (DrawingTool=dtErase) then
                 EditImage1.Hint:=HintDrawingToolText[DrawingTool]
       else if   DrawingTool=dtSelect then
                 if   Editor.MouseButtonDown then
                      if   IsDragging then
                           EditImage1.Hint:=HintMoveRectText
                      else if   SizeHandle=ghNull then
                                EditImage1.Hint:=HintDrawingToolText[DrawingTool]+HintDrawingToolRightClickEraseText
                           else EditImage1.Hint:=HintResizeRect2Text
                 else if   SizeHandle=ghNull then
                           if   Editor.Selection.Enabled then
                                EditImage1.Hint:=HintDrawingToolMoveSelectedAreaText+HintDrawingToolRightClickMenuText
                           else EditImage1.Hint:=HintDrawingToolText[DrawingTool]+HintDrawingToolRightClickEraseText
                      else EditImage1.Hint:=HintSelectZoomRect2Text
            else if   DrawingTool=dtPlayer then begin
                      Point:=EditImage1.ScreenToClient(Mouse.CursorPos);
                      MouseToCell(Point.X,Point.Y,Col,Row);
                      Value:=CellValue(Col,Row);
                      if        Value and PLAYER=0 then
                                EditImage1.Hint:=HintDrawingToolText[DrawingTool]+HintDrawingToolRightClickEraseText
                      else if   (Value and GOAL)=0 then
                                EditImage1.Hint:=HintDrawingToolText[dtGoal]+HintDrawingToolRightClickEraseText
                           else EditImage1.Hint:=HintDrawingToolEraseGoalText +HintDrawingToolRightClickEraseText;
                      end
            else if   DrawingTool=dtBox then begin
                      Point:=EditImage1.ScreenToClient(Mouse.CursorPos);
                      MouseToCell(Point.X,Point.Y,Col,Row);
                      Value:=CellValue(Col,Row);
                      if        Value and BOX=0 then
                                EditImage1.Hint:=HintDrawingToolText[DrawingTool]+HintDrawingToolRightClickEraseText
                      else if   (Value and GOAL)=0 then
                                EditImage1.Hint:=HintDrawingToolText[dtGoal]+HintDrawingToolRightClickEraseText
                           else EditImage1.Hint:=HintDrawingToolEraseGoalText +HintDrawingToolRightClickEraseText;
                      end
            else if   DrawingTool=dtGoal then begin
                      Point:=EditImage1.ScreenToClient(Mouse.CursorPos);
                      MouseToCell(Point.X,Point.Y,Col,Row);
                      Value:=CellValue(Col,Row);
                      if        Value and GOAL=0 then
                                EditImage1.Hint:=HintDrawingToolText[DrawingTool]+HintDrawingToolRightClickEraseText
                      else if   (Value and BOX)=0 then
                                EditImage1.Hint:=HintDrawingToolText[dtBox] +HintDrawingToolRightClickEraseText
                           else EditImage1.Hint:=HintDrawingToolEraseBoxText+HintDrawingToolRightClickEraseText;
                      end
            else EditImage1.Hint:=HintDrawingToolText[DrawingTool]+HintDrawingToolRightClickEraseText;
       end
    else EditImage1.Hint:='';
end;

procedure TToolsForm.EditMenuItemDrawingToolClick(Sender: TObject);
var Col,Row:Integer; Point:TPoint;
begin // precondition: if  'Sender' = 'EditMenuItemSelectAll' then the board must not be empty
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
        if Sender is TMenuItem then with Sender as TMenuItem do begin
           CloseEditorSelection(True,False);
           Editor.Selection.Enabled:=False;
           Editor.DrawingTool:=TDrawingTool(Tag);
           EditToolBarLeft.Buttons[Pred(Tag)].Down:=True;
           if      Sender=EditMenuItemSelect then begin
                   Editor.Cursor:=ctSelection;
                   end
           else if Sender=EditMenuItemSelectAll then begin
                   Editor.Cursor:=ctSelection;
                   Editor.Selection.Enabled:=True;
                   end
           else if Sender=EditMenuItemErase then
                   Editor.Cursor:=ctEraser
           else Editor.Cursor:=ctCell;

           if   Editor.DrawingToolCursorsEnabled then
                EditImage1.Cursor:=Succ(Ord(Editor.DrawingTool))
           else EditImage1.Cursor:=crDefault;

           if (GameViewer.ColWidth>0) and (GameViewer.RowHeight>0) then begin
              Point:=EditImage1.ScreenToClient(Mouse.CursorPos);
              MouseToCell(Point.X,Point.Y,Col,Row);
              if   Sender<>EditMenuItemSelectAll then
                   SetCursor(Editor.Cursor,Rect(Col,Row,Succ(Col),Succ(Row)))
              else with Editor.BoardCellOrigin do SetCursor(ctSelection,Rect(X,Y,X+Game.BoardWidth,Y+Game.BoardHeight));
              SetDrawingToolHint;

              if PtInRect(Rect(0,0,EditImage1.Picture.BitMap.Width,EditImage1.Picture.BitMap.Height),Point)
                 //(Col>=0) and (Row>=0) and
                 //(Col<GameViewer.ColCount) and (Row<GameViewer.RowCount)
                 then begin
                 StatusText:=GetLongHint(EditImage1.Hint);
                 EditImage1MouseMove(Sender,[],Point.X,Point.Y);
                 end
              else if Sender<>EditMenuItemSelectAll then
                      HideCursor(False);
              end
           else
              SetDrawingToolHint;
           ShowStatus;
           end;
end;

procedure TToolsForm.EditImage1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Col,Row:Integer;
begin
  with Editor do begin
    if MouseButtonDown then EditImage1MouseUp(Sender,Button,Shift,X,Y);
    MouseButton:=Button; MouseButtonDown:=True;

    MouseToCell(X,Y,DragPoint.X,DragPoint.Y);
    SizeHandle:=ghNull; IsDragging:=False;
    IsSelectingANewArea:=False; ToggledItemPos.X:=-1; ToggledItemPos.Y:=-1;
    with GameViewer.MouseSuperPosition do begin X:=0; Y:=0; end;

    if Button=mbLeft then begin
       if Cursor<>ctSelection then begin
          Editor.History.BeginTransaction(0);
          EditImage1MouseMove(Sender,Shift,X,Y);
          end
       else begin
          if Editor.Selection.Enabled then begin
             SizeHandle:=PointToGrabHandle(Point(X,Y),CursorRectPixels,SIZING_RECT_WIDTH);
             if Screen.Cursor<>GrabHandleToCursor(SizeHandle) then
                Screen.Cursor:=GrabHandleToCursor(SizeHandle);
             IsDragging:=(SizeHandle=ghNull) and PtInRect(CursorRect,DragPoint);
             if SizeHandle=ghNull then begin
                if IsDragging and (not Selection.HasBoard) then begin
                   CutSelectionToEditorClipboard;
                   IsDragging:=Editor.Selection.HasBoard;
                   end;

                if IsDragging then begin
                   SetCursor(Cursor,CursorRect);
                   end
                else begin
                   CloseEditorSelection(True,False);
                   Editor.Selection.Enabled:=False; // switch from selecting an area to highligting current cell
                   SizeHandle:=ghNull; IsDragging:=False;
                   SetDrawingToolHint; MouseButtonDown:=False;
                   with DragPoint do SetCursor(Cursor,Rect(X,Y,Succ(X),Succ(Y)));
                   EditImage1MouseDown(Sender,Button,Shift,X,Y); // go directly to selecting a new area
                   IsSelectingANewArea:=True;
                   end
                end
             else with CursorRect do with DragPoint do begin
                CloseEditorSelection(True,False);
                X:=Max(Left,Min(Right,X)); Y:=Max(Top,Min(Bottom,Y)); // ensure that the drag-point is inside the cursor-rectangle
                SetCursor(Cursor,CursorRect);
                end;
             end
          else
             if DrawingToolsEnabled then begin
                MouseToCell(X,Y,Col,Row);
                if (Col>=0) and (Col<GameViewer.ColCount) and
                   (Row>=0) and (Row<GameViewer.RowCount) then begin
                   Editor.Selection.Enabled:=True; // switch from highlighting current cell to selecting an area
                   SetDrawingToolHint;
                   with DragPoint do SetCursor(Cursor,Rect(X,Y,Succ(X),Succ(Y)));
                   end;
                end;
          EditImage1MouseMove(Sender,Shift,X,Y);
          end;
       end
    else begin
       if (Button=mbRight) and
          (Cursor=ctSelection) and
          Editor.Selection.Enabled then begin
          MouseButtonDown:=False;
          ShowStatus;
          EditPopupMenuRightClickPosition:=EditImage1.ClientToScreen(Point(X,Y));
          with EditPopupMenuRightClickPosition do EditPopupMenuRightClick.Popup(X,Y);
          end
       else begin
          CloseEditorSelection(True,False);
          if Editor.Selection.Enabled then begin
             Editor.Selection.Enabled:=False;
             MouseButtonDown:=False;
             SetDrawingToolHint;
             end;
          with DragPoint do SetCursor(Cursor,Rect(X,Y,Succ(X),Succ(Y)));
          if Button=mbRight then Editor.History.BeginTransaction(0);
          EditImage1MouseMove(Sender,Shift,X,Y);
          end;
       end;
    ShowStatus;
    end;
end;

procedure TToolsForm.EditImage1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var i,Col,Row,X1,Y1,DX,DY:Integer; s:String; P:TPoint; R,R1:TRect; C:TCursor;
(*
// original 'ScrollInView' function;
// it works correctly except for the limitation that it cannot move
// selected areas with dimensions [max, max-1] and [max-1, max];
// the function has been superseded be the new one below, but the old code
// is retained
function ScrollInView(var R__:TRect; var Col__,Row__:Integer):Boolean;
  var i,j,DX,DY:Integer; Ok:Boolean;
  begin
    Result:=OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors;

    if Result then begin
       DX:=Max(0,-R__.Left); DY:=Max(0,-R__.Top);
       if   R__.Left=0 then
            if   Editor.BoardCellOrigin.X+Game.BoardWidth <MAX_BOARD_WIDTH  then
                 if   (Game.BoardWidth=0) and
                      Editor.Selection.HasBoard and
                      (Editor.Selection.NonFloorCellCount<>0) and
                      (R__.Right=Pred(GameViewer.ColCount)) then
                      DX:=0
                 else DX:=1
            else Result:=Editor.BoardCellOrigin.X+Game.BoardWidth=MAX_BOARD_WIDTH;
       if   R__.Top =0 then
            if   Editor.BoardCellOrigin.Y+Game.BoardHeight<MAX_BOARD_HEIGHT then
                 if   (Game.BoardHeight=0) and
                      Editor.Selection.HasBoard and
                      (Editor.Selection.NonFloorCellCount<>0) and
                      (R__.Bottom=Pred(GameViewer.RowCount)) then
                      DY:=0
                 else DY:=1
            else Result:=Editor.BoardCellOrigin.Y+Game.BoardHeight=MAX_BOARD_HEIGHT;

       if Result and ((DX>0) or (DY>0)) then begin
          Result:=(((Game.BoardWidth<>0) and (Game.BoardHeight<>0))
                   //or
                   //(Editor.Selection.HasBoard and (Editor.Selection.NonFloorCellCount<>0))
                  )
                  and
                  (Editor.BoardCellOrigin.X+Game.BoardWidth +DX<=MAX_BOARD_WIDTH )
                  and
                  (Editor.BoardCellOrigin.Y+Game.BoardHeight+DY<=MAX_BOARD_HEIGHT);

          if Result then
             try     Game.IsBusy:=True; Ok:=True;

                     with Editor.BoardCellOrigin do begin
                        Inc(X,DX);
                        if X+Game.BoardWidth >=Pred(GameViewer.ColCount) then Ok:=False;
                        Inc(Y,DY);
                        if Y+Game.BoardHeight>=Pred(GameViewer.RowCount) then Ok:=False;
                        end;

                     R__:=RectPlusOffset(R__,DX,DY);

                     if not Ok then begin
                        Editor.CursorRect:=R__;
                        Editor.Selection.Rect:=Editor.CursorRect;
                        InitializeGameViewer(False); ShowGameBackground;
                        MouseToCell(X,Y,Col__,Row__);
                        R__:=Editor.CursorRect;
                        end;

             finally ShowSquares;
                     Game.IsBusy:=False;
             end;
          end;
       end;

    if Result then begin

       DX:=Max(0,R__.Right-GameViewer.ColCount); DY:=Max(0,R__.Bottom-GameViewer.RowCount);
       if   R__.Right =GameViewer.ColCount then
            if   Editor.BoardCellOrigin.X+Game.BoardWidth <MAX_BOARD_WIDTH  then
                 if   (Game.BoardWidth=0) and
                      Editor.Selection.HasBoard and
                      (Editor.Selection.NonFloorCellCount<>0) and
                      (R__.Left=1) then
                      DX:=0
                 else DX:=1
            else Result:=Editor.BoardCellOrigin.X+Game.BoardWidth=MAX_BOARD_WIDTH;
       if   R__.Bottom=GameViewer.RowCount then
            if   Editor.BoardCellOrigin.Y+Game.BoardHeight<MAX_BOARD_HEIGHT then
                 if   (Game.BoardHeight=0) and
                      Editor.Selection.HasBoard and
                      (Editor.Selection.NonFloorCellCount<>0) and
                      (R__.Top=1) then
                      DY:=0
                 else DY:=1
            else Result:=Editor.BoardCellOrigin.Y+Game.BoardHeight=MAX_BOARD_HEIGHT;

       if Result and ((DX>0) or (DY>0)) then begin
          Result:=(((Game.BoardWidth<>0) and (Game.BoardHeight<>0))
                   //or
                   //(Editor.Selection.HasBoard and (Editor.Selection.NonFloorCellCount<>0))
                  )
                  and
                  (R__.Right -Editor.BoardCellOrigin.X<=MAX_BOARD_WIDTH )
                  and
                  (R__.Bottom-Editor.BoardCellOrigin.Y<=MAX_BOARD_HEIGHT);

          if Result then
             try     Game.IsBusy:=True; Ok:=True;

                     with Editor.BoardCellOrigin do begin
                        if X>DX then begin
                           Dec(X,DX);
                           R__:=RectPlusOffset(R__,-DX,0);
                           end
                        else begin
                           R__:=RectPlusOffset(R__,-X,0);
                           X:=0;
                           Ok:=False;
                           end;
                        if Y>DY then begin
                           Dec(Y,DY);
                           R__:=RectPlusOffset(R__,0,-DY);
                           end
                        else begin
                           R__:=RectPlusOffset(R__,0,-Y);
                           Y:=0;
                           Ok:=False;
                           end;
                        end;

                     if not Ok then begin
                        Editor.CursorRect:=R__;
                        Editor.Selection.Rect:=Editor.CursorRect;
                        InitializeGameViewer(False); ShowGameBackground;
                        MouseToCell(X,Y,Col__,Row__);
                        R__:=Editor.CursorRect;
                        end;

                     with Editor.BoardCellOrigin do begin
                       if DX>0 then i:=Max(0,GameViewer.ColCount-R__.Right)
                       else i:=0;
                       if DY>0 then j:=Max(0,GameViewer.RowCount-R__.Bottom)
                       else j:=0;
                       R__:=RectPlusOffset(R__,i,j); Inc(X,i); Inc(Y,j);

                       if   (DX>0) and (R__.Right -X<GameViewer.ColCount) then DX:=-1
                       else DX:=0;
                       if   (DY>0) and (R__.Bottom-Y<GameViewer.RowCount) then DY:=-1
                       else DY:=0;
                       R__:=RectPlusOffset(R__,DX,DY); Inc(X,DX); Inc(Y,DY);
                       end;

             finally ShowSquares;
                     Game.IsBusy:=False;
             end;
          end;
       end;
  end; // ScrollInView
*)
  function ScrollInView(var R__:TRect; var Col__,Row__:Integer):Boolean;
  var DX,DY,NewWidth,NewHeight,ColMargin,RowMargin:Integer;
      Reload:Boolean;
      BoardRect,BoardAndSelectedAreaRect:TRect;
  begin
    // the selected area has been removed from the game board, so at this point
    // 'Game.BoardWidth' and 'Game.BoardHeight' refer to the remains, if any

    Result:=OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors;

    if Result then begin
       with Editor.BoardCellOrigin do BoardRect:=Rect(X,Y,X+Game.BoardWidth,Y+Game.BoardHeight); // the area occupied by the board, if any
       UnionRect(BoardAndSelectedAreaRect,BoardRect,R__);
       NewWidth :=RectWidth (BoardAndSelectedAreaRect); // width and height of the board and the selected area
       NewHeight:=RectHeight(BoardAndSelectedAreaRect);
       Result:=(NewWidth<=MAX_BOARD_WIDTH) and (NewHeight<=MAX_BOARD_HEIGHT);
       if Result then begin // 'True': the board and the selected area don't exceed the maximum limits
          if   NewWidth <=MAX_BOARD_WIDTH -2 then ColMargin:=1 // 'True': there is room for an empty column both to the left and to the right
          else ColMargin:=0;
          if   NewHeight<=MAX_BOARD_HEIGHT-2 then RowMargin:=1 // 'True': there is room for an empty row both at the top and at the bottom
          else RowMargin:=0;

          DX:=Max(0,ColMargin-R__.Left);                                        // if DX > 0: the selected area must move right by this amount to stay in view
          DY:=Max(0,RowMargin-R__.Top);                                         // if DY > 0: the selected area must move down  by this amount to stay in view
          if DX=0 then DX:=Min(0,GameViewer.ColCount-ColMargin-R__.Right);      // if DX < 0: the selected area must move left  by Abs(DX)     to stay in view
          if DY=0 then DY:=Min(0,GameViewer.RowCount-RowMargin-R__.Bottom);     // if DY < 0: the selected area must move up    by Abs(DY)     to stay in view

          if (DX<>0) or (DY<>0) then // 'True': the selected area must move in order to stay in view
             try     Game.IsBusy:=True; Reload:=False;

                     with Editor.BoardCellOrigin do begin
                       Inc(X,DX);
                       Inc(Y,DY);
                       if X<ColMargin then begin // 'True': the board must move left to stay in view
                          Reload:=True; Inc(DX,ColMargin-X); X:=ColMargin;
                          end;
                       if Y<RowMargin then begin // 'True': the board must move down to stay in view
                          Reload:=True; Inc(DY,RowMargin-Y); Y:=RowMargin;
                          end;
                       if (X+Game.BoardWidth >GameViewer.ColCount-ColMargin) or // 'True': the board must be resized in order to fit on the screen
                          (Y+Game.BoardHeight>GameViewer.RowCount-RowMargin) then Reload:=True; // 'True': the board must be resized in order to fit on the screen
                       end;
                     R:=RectPlusOffset(R__,DX,DY);

                     if Reload then begin // 'True': refresh and maybe resize the editor area
                        Editor.CursorRect:=R__;
                        Editor.Selection.Rect:=Editor.CursorRect;
                        InitializeGameViewer(False); ShowGameBackground;
                        MouseToCell(X,Y,Col__,Row__);
                        R__:=Editor.CursorRect;
                        end;
(*
                     // leftover from the first implementation of 'ScrollInView';
                     // it's not in production because it hasn't been analyzed
                     // whether it still is necessary, wanted, or correct;

                     with Editor.BoardCellOrigin do begin
                       if   DX<0 then                                           // 'True': the selected area moved left to stay in view
                            i:=Max(0,GameViewer.ColCount-R__.Right)             // if the board was reloaded/resized, the column count may have increased now so the selected area isn't right-justified anymore; set 'i' so it can right-justify the selected area
                       else i:=0;
                       if   DY<0 then                                           // 'True': the selected area moved up to stay in view
                            j:=Max(0,GameViewer.RowCount-R__.Bottom)            // if the board was reloaded/resized, the column count may have increased now so the selected area isn't bottom-justified anymore; set 'j' so it can bottom-justify the selected area
                       else j:=0;
                       R__:=RectPlusOffset(R__,i,j); Inc(X,i); Inc(Y,j);

                       if   (DX<0) and (R__.Right -X<GameViewer.ColCount) then DX:=-1
                       else DX:=0;
                       if   (DY<0) and (R__.Bottom-Y<GameViewer.RowCount) then DY:=-1
                       else DY:=0;
                       R__:=RectPlusOffset(R__,DX,DY); Inc(X,DX); Inc(Y,DY);
                       end;
*)
             finally ShowSquares;
                     Game.IsBusy:=False;
             end;
          end;
       end;
  end; // ScrollInView

  function  UpdateView(KeepMouseButtonDown__:Boolean; var Col__,Row__:Integer):Boolean; // 0-based cell coordinate columns and rows
  const MARGIN=40;
  var Col,Row,DX,DY,oColCount,oRowCount:Integer; OK:Boolean; P1,P2,P3:TPoint;
      Item:TEditorHistoryItem;
  begin // returns 'True' if the board changes dimensions or position on the screen
    Result:=False; OK:=True;
    with Game do begin
      if (CellValue(Col__,Row__) and (WALL+BOX+GOAL+PLAYER))=0 then begin // empty square
         Col:=Succ(Col__-Editor.BoardCellOrigin.X);
         Row:=Succ(Row__-Editor.BoardCellOrigin.Y);
         DX:=0; DY:=0;

         if Col=1 then
            while (Col<=Game.BoardWidth ) and Game.IsAnEmptyCol(Col) do begin
              Inc(Col); Dec(DX);
              end;
         if Row=1 then
            while (Row<=Game.BoardHeight) and Game.IsAnEmptyRow(Row) do begin
              Inc(Row); Dec(DY);
              end;
         if (DX<>0) or (DY<>0) then begin
            Item:=Editor.History.MakeItem(ehaTranslate,DX,DY,0,Editor.History.Position);
            if Editor.History.AddItem(Item) then begin
               Editor.History.DoItem(Item); Result:=True;

               // left-justify...
               //Result:=True; OK:=False;
               //if (Game.BoardWidth<>0) and (Game.BoardHeight<>0) then begin
               //   Inc(Col__,DX); Inc(Row__,DY);
               //   end;

               // ... or keep the current view:
               if (Game.BoardWidth<>0) and (Game.BoardHeight<>0) then begin
                  Dec(Editor.BoardCellOrigin.X,DX); Dec(Editor.BoardCellOrigin.Y,DY);
                  end;

               end;
            end;

         if Col=Game.BoardWidth then
            while (Game.BoardWidth<>0) and Game.IsAnEmptyCol(Game.BoardWidth) do begin
              Dec(Game.BoardWidth); Result:=True;
              end;
         if Row=Game.BoardHeight then
            while (Game.BoardHeight<>0) and Game.IsAnEmptyRow(Game.BoardHeight) do begin
              Dec(Game.BoardHeight); Result:=True;
              end;

         if Result then
            if (GameViewer.ColWidth <MAX_EDITOR_SQUARE_SIZE)
               and
               (GameViewer.RowHeight<MAX_EDITOR_SQUARE_SIZE) then
               if      (Game.BoardWidth>MIN_BOARD_WIDTH)
                       and
                       (Game.BoardWidth +(2*EDITOR_GUTTER_SQUARE_COUNT)<GameViewer.ColCount)
                       then OK:=False
               else if (Game.BoardHeight>MIN_BOARD_HEIGHT)
                       and
                       (Game.BoardHeight+(2*EDITOR_GUTTER_SQUARE_COUNT)<GameViewer.RowCount)
                       then OK:=False;
         end
      else with Editor.BoardCellOrigin do begin // non-empty square; precondition: the square is already covered by the game board, usually ensured by a preceding call to 'AddColRowToBoard'
         if (Col__=0) and
            (Game.BoardWidth <MAX_BOARD_WIDTH ) then begin
            X:=1; Result:=True; Col__:=1;
            if X+Game.BoardWidth >=GameViewer.ColCount then OK:=False;
            if not KeepMouseButtonDown__ then
               EditImage1MouseUp(nil,Editor.MouseButton,Shift,0,0);
            end;
         if (Row__=0) and
            (Game.BoardHeight<MAX_BOARD_HEIGHT) then begin
            Y:=1; Result:=True; Row__:=1;
            if Y+Game.BoardHeight>=GameViewer.RowCount then OK:=False;
            if not KeepMouseButtonDown__ then
               EditImage1MouseUp(nil,Editor.MouseButton,Shift,0,0);
            end;
         if (Col__=Pred(GameViewer.ColCount)) and
            (Game.BoardWidth <MAX_BOARD_WIDTH ) then begin
            Result:=True;
            if X>1 then begin
               Dec(X); Dec(Col__);
               end
            else OK:=False;
            if not KeepMouseButtonDown__ then
               EditImage1MouseUp(nil,Editor.MouseButton,Shift,0,0);
            end;
         if (Row__=Pred(GameViewer.RowCount)) and
            (Game.BoardHeight<MAX_BOARD_HEIGHT) then begin
            Result:=True;
            if Y>1 then begin
               Dec(Y); Dec(Row__);
               end
            else OK:=False;
            if not KeepMouseButtonDown__ then
               EditImage1MouseUp(nil,Editor.MouseButton,Shift,0,0);
            end;
         end;

      if not OK then with Editor.BoardCellOrigin do begin
         //EditImage1MouseUp(nil,Editor.MouseButton,Shift,0,0);
         oColCount:=GameViewer.ColCount; oRowCount:=GameViewer.RowCount;

         Col:=X; Row:=Y;

         InitializeGameViewer(False); ShowGameBackground;

         DX:=X-Col; DY:=Y-Row;
         if (Game.BoardWidth<>0) and (Game.BoardHeight<>0) then begin
            Inc(Col__,DX); Inc(Row__,DY);
            end;

         if (GameViewer.ColCount<>oColCount) or
            (GameViewer.RowCount<>oRowCount) or
            (DX<>0) or
            (DY<>0) then
            if KeepMouseButtonDown__ then begin
               CellToPos(Col__,Row__,P2.X,P2.Y);
               Inc(P2.X,GameViewer.ColWidth div 2);
               Inc(P2.Y,GameViewer.RowHeight div 2);

               P1:=EditImage1.ScreenToClient(Mouse.CursorPos);
               Mouse.CursorPos:=EditImage1.ClientToScreen(P2);

               P3.X:=P2.X-(GameViewer.MouseSuperPosition.X+P2.X-P1.X);
               P3.Y:=P2.Y-(GameViewer.MouseSuperPosition.Y+P2.Y-P1.Y);
               P3:=EditImage1.ClientToScreen(P3);
               with P3 do
                 if (X<MARGIN) or (X>Screen.DeskTopWidth -MARGIN) or
                    (Y<MARGIN) or (Y>Screen.DeskTopHeight-MARGIN) then
                    // if the mouse position is changed further, it will
                    // be out of screen range when the mouse position is
                    // restored, but Windows clips the position;
                    // therefore, stop editing and restore the mouse position
                    // now;
                    // otherwise the user would need to lift the mouse
                    // physically in order to bring it back to the
                    // ergonimically most convenient physical zero-position
                    EditImage1MouseUp(nil,Editor.MouseButton,Shift,0,0)
                 else begin
                    Inc(GameViewer.MouseSuperPosition.X,P2.X-P1.X); // remember how much the program has tampered with the mouse cursor position
                    Inc(GameViewer.MouseSuperPosition.Y,P2.Y-P1.Y);
                    end;
               end
            else
               EditImage1MouseUp(nil,Editor.MouseButton,Shift,0,0);

         end;

      if Result then ShowSquares;
      end;
  end; // UpdateView

  procedure ToggleItem(TransactionType__:TEditorHistoryAction; ItemValue__:Integer; var ItemCount__,Col__,Row__:Integer);
  var X,Y,CellValue:Integer;
  begin // precondition: 'ItemValue__' is either 'GOAL' or 'BOX', and 'TransactionType__' is set accordingly
    if (Col__<>Editor.ToggledItemPos.X) or
       (Row__<>Editor.ToggledItemPos.Y) then begin

       X:=Succ(Col__-Editor.BoardCellOrigin.X);
       Y:=Succ(Row__-Editor.BoardCellOrigin.Y);
       CellValue:=Game.Board[X,Y];

       if (CellValue and ItemValue__)=0 then begin
          if Editor.History.AddItem(Editor.History.MakeItem(TransactionType__,X,Y,CellValue,Editor.History.Position)) then begin
             Inc(ItemCount__); Modified:=True;

             if (ItemValue__=BOX) and ((CellValue and PLAYER)<>0) then begin
                Game.PlayerPos.X:=0; Game.PlayerPos.Y:=0;
                Dec(CellValue,PLAYER);
                end;

             Game.Board[X,Y]:=CellValue or ItemValue__;
             end;
          end
       else
          if Editor.History.BeginTransaction(2) and
             Editor.History.AddItem(Editor.History.MakeItem(ehaErase ,X,Y,CellValue,Editor.History.Position)) and
             Editor.History.AddItem(Editor.History.MakeItem(ehaSquare,X,Y,CellValue and (not ItemValue__),Editor.History.Position)) then begin
             Dec(ItemCount__); Modified:=True;
             Game.Board[X,Y]:=CellValue and (not ItemValue__);
             end;

       ShowSquare(Col__,Row__);
       UpdateView(False,Col__,Row__);

       Editor.ToggledItemPos.X:=Col__; Editor.ToggledItemPos.Y:=Row__;

       end;
  end; // ToggleItem

begin // EditImage1MouseMove
  with GameViewer do with Editor do begin
    MouseToCell(X,Y,Col,Row);
    //Caption:=Format('[%d:%d]',[Succ(Col),Succ(Row)]);
    if (SizeHandle<>ghNull) and
       (Screen.Cursor<>GrabHandleToCursor(SizeHandle)) then
       Screen.Cursor:=GrabHandleToCursor(SizeHandle);

    if MouseButtonDown and DrawingToolsEnabled then begin
       if Cursor<>ctSelection then begin
          if Screen.Cursor<>EditImage1.Cursor then Screen.Cursor:=EditImage1.Cursor;

          if (Col>=0) and (Col<GameViewer.ColCount) and
             (Row>=0) and (Row<GameViewer.RowCount) then begin

             i:=CellValue(Col,Row);

             if MouseButton=mbLeft then begin

                case DrawingTool of

                  dtNone      :;

                  dtWall      : if ((i and WALL)=0) and
                                   AddColRowToBoard(Col,Row) and
                                   History.AddItem(History.MakeItem(ehaWall,Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y),i,History.Position)) then begin
                                   if (i and PLAYER)<>0 then begin
                                      Game.PlayerPos.X:=0; Game.PlayerPos.Y:=0;
                                      Modified:=True;
                                      end;
                                   if (i and BOX )<>0 then Dec(Game.BoxCount);
                                   if (i and GOAL)<>0 then Dec(Game.GoalCount);
                                   if (i and WALL)= 0 then Modified:=True;
                                   Game.Board[Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y)]:=WALL;
                                   ShowSquare(Col,Row);
                                   UpdateView(True,Col,Row);
                                   end;

                  dtBox       : if (i and BOX)=0 then begin
                                   if AddColRowToBoard(Col,Row) and
                                      History.AddItem(History.MakeItem(ehaBox,Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y),i,History.Position)) then begin
                                      if (i and PLAYER)<>0 then begin
                                         Game.PlayerPos.X:=0; Game.PlayerPos.Y:=0;
                                         Dec(i,PLAYER);
                                         Modified:=True;
                                         end;
                                      if (i and BOX)=0 then begin
                                         Inc(Game.BoxCount); Modified:=True;
                                         end;
                                      if (i and WALL)<>0 then i:=FLOOR;
                                      Game.Board[Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y)]:=i or BOX;
                                      ShowSquare(Col,Row);
                                      UpdateView(True,Col,Row);

                                      Editor.ToggledItemPos.X:=Col; Editor.ToggledItemPos.Y:=Row;
                                      end;
                                   end
                                else
                                   ToggleItem(ehaGoal,GOAL,Game.GoalCount,Col,Row);

                  dtGoal      : if (i and GOAL)=0 then begin
                                   if AddColRowToBoard(Col,Row) and
                                      History.AddItem(History.MakeItem(ehaGoal,Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y),i,History.Position)) then begin
                                      if (i and GOAL)=0 then begin
                                         Inc(Game.GoalCount); Modified:=True;
                                         end;
                                      if (i and WALL)<>0 then i:=FLOOR;
                                      Game.Board[Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y)]:=i or GOAL;
                                      ShowSquare(Col,Row);
                                      UpdateView(True,Col,Row);

                                      Editor.ToggledItemPos.X:=Col; Editor.ToggledItemPos.Y:=Row;
                                      end;
                                   end
                                else
                                   ToggleItem(ehaBox,BOX,Game.BoxCount,Col,Row);

                  dtPlayer    : if (i and PLAYER)=0 then begin
                                   if History.BeginTransaction(3)
                                      and
                                      AddColRowToBoard(Col,Row)
                                      and
                                      ((Game.PlayerPos.X=0)
                                       or
                                       (Game.PlayerPos.Y=0)
                                       or
                                       History.AddItem(History.MakeItem(ehaPlayer,Game.PlayerPos.X,Game.PlayerPos.Y,Game.Board[Game.PlayerPos.X,Game.PlayerPos.Y],History.Position))
                                      )
                                      and
                                      History.AddItem(History.MakeItem(ehaPlayer,Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y),i,History.Position)) then begin
                                      X1:=Game.PlayerPos.X; Y1:=Game.PlayerPos.Y;
                                      if (X1<>0) and (Y1<>0) then begin
                                         Game.Board[X1,Y1]:=Game.Board[X1,Y1] and (not PLAYER);
                                         ShowSquare(Editor.BoardCellOrigin.X+Pred(X1),Editor.BoardCellOrigin.Y+Pred(Y1));
                                         end;
                                      Modified:=True;
                                      Game.PlayerPos.X:=Succ(Col-Editor.BoardCellOrigin.X);
                                      Game.PlayerPos.Y:=Succ(Row-Editor.BoardCellOrigin.Y);
                                      if (i and BOX )<>0 then Dec(Game.BoxCount);
                                      if (i and WALL)<>0 then i:=FLOOR;
                                      Game.Board[Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y)]:=(i and (not BOX)) or PLAYER;
                                      ShowSquare(Col,Row);

                                      if   (X1<>0) and (Y1<>0) then begin // [X1,Y1]: the old player position
                                           if CalculateGameInformation(False) then ShowStatus;
                                           end;

                                      Col:=Editor.BoardCellOrigin.X+Pred(Game.PlayerPos.X);
                                      Row:=Editor.BoardCellOrigin.Y+Pred(Game.PlayerPos.Y);
                                      UpdateView(True,Col,Row);

                                      Editor.ToggledItemPos.X:=Col; Editor.ToggledItemPos.Y:=Row;
                                      end;
                                   end
                                else
                                   ToggleItem(ehaGoal,GOAL,Game.GoalCount,Col,Row);

                  dtFloor    :;

                  dtErase    : if ((i and BOARD_PIECES)<>FLOOR) and
                                  History.AddItem(History.MakeItem(ehaErase,Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y),i,History.Position)) then begin
                                  EraseGameBoardSquare(Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y));
                                  ShowSquare(Col,Row);
                                  UpdateView(False,Col,Row);
                                  end;
                  dtSelect   :;
                  else;
                end; // case
                end

             else
                if (MouseButton=mbRight) and
                   ((i and BOARD_PIECES)<>FLOOR) and
                   History.BeginTransaction(1) and
                   History.AddItem(History.MakeItem(ehaErase,Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y),i,History.Position)) then begin
                   EraseGameBoardSquare(Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y));
                   ShowSquare(Col,Row);
                   UpdateView(False,Col,Row);
                   end;
             end;

          SetCursor(Cursor,Rect(Col,Row,Succ(Col),Succ(Row)));
          ShowStatus;
          end
       else begin // cursor = ctSelection
          if      IsDragging then begin
                  if Screen.Cursor<>crDrag then Screen.Cursor:=crDrag;
                  DX:=Col-DragPoint.X; DY:=Row-DragPoint.Y;
                  if (DX<>0) or (DY<>0) then with CursorRect do begin
                     R:=Rect(Left+DX,Top+DY,Right+DX,Bottom+DY);

                     CellToPos(R.Left,R.Top,R1.Left,R1.Top);
                     CellToPos(R.Right,R.Bottom,R1.Right,R1.Bottom);
                     ClipRect (R1,Rect(0,0,EditImage1.Picture.BitMap.Width,EditImage1.Picture.BitMap.Height));

                     //Caption:=Format('%d,%d,%d,%d',[R.Left,R.Top,R.Right,R.Bottom]);

                     i:=Cursors[Cursor].PenWidth+1+SIZING_RECT_WIDTH+8;
                     if  // ensure there are some pixels where the user later can grab the rectangle with the mouse
                         (R1.Left  <Editimage1.Picture.BitMap.Width -i) and
                         (R1.Top   <Editimage1.Picture.BitMap.Height-i) and
                         (R1.Right >i) and
                         (R1.Bottom>i) and
                         (R.Left<R.Right) and
                         (R.Top<R.Bottom) and
                         ScrollInView(R,Col,Row)
                         then begin
                         Editor.Selection.Rect:=R;
                         DragPoint:=Point(Col,Row);
                         SetCursor(Cursor,R);
                         end;

                     DragPoint:=Point(Col,Row);
                     end;
                  end
          else if SizeHandle<>ghNull then with CursorRect do begin
                  P:=DragPoint; R:=CursorRect;
                  if (GrabHandleResizeRect(SizeHandle,Col,Row,P,R)<>0) and
                     (R.Left>=0) and (R.Top>=0) and
                     (R.Right<=GameViewer.ColCount) and
                     (R.Bottom<=GameViewer.RowCount) and
                     (R.Left<R.Right) and
                     (R.Top<R.Bottom)
                     then begin
                     DragPoint:=P; SetCursor(Cursor,R);
                     end;
                  end
          else if Editor.Selection.Enabled then begin
                  if MouseButton=mbLeft then begin
                     if Screen.Cursor<>crSize then Screen.Cursor:=crSize;
                     with DragPoint do R:=Rect(Max(0,Min(X,Col)),Max(0,Min(Y,Row)),Min(ColCount,Succ(Max(X,Col))),Min(RowCount,Succ(Max(Y,Row))));
                     if (R.Left <>CursorRect.Left ) or (R.Top   <>CursorRect.Top   ) or
                        (R.Right<>CursorRect.Right) or (R.Bottom<>CursorRect.Bottom) then
                        SetCursor(Cursor,R);
                     end;
                  end
               else begin
                  if MouseButton=mbRight then begin
                     if ((CellValue(Col,Row) and BOARD_PIECES)<>FLOOR) and
                        History.AddItem(History.MakeItem(ehaErase,Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y),Game.Board[Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y)],History.Position)) then begin
                        EraseGameBoardSquare(Succ(Col-Editor.BoardCellOrigin.X),Succ(Row-Editor.BoardCellOrigin.Y));
                        ShowSquare(Col,Row);
                        UpdateView(False,Col,Row);
                        end;
                     end;

                  SetCursor(Cursor,Rect(Col,Row,Succ(Col),Succ(Row)));
                  end;
          end;
       end
    else begin // no mouse button pressed
       //if Screen.Cursor=crDrag then Screen.Cursor:=EditImage1.Cursor;
       if Screen.Cursor<>EditImage1.Cursor then Screen.Cursor:=EditImage1.Cursor;
       SizeHandle:=ghNull;

       if Editor.Selection.Enabled then begin
          SizeHandle:=PointToGrabHandle(Point(X,Y),CursorRectPixels,SIZING_RECT_WIDTH);
          if SizeHandle=ghNull then begin
             if Screen.Cursor<>EditImage1.Cursor then Screen.Cursor:=EditImage1.Cursor;
             end
          else begin
             C:=GrabHandleToCursor(SizeHandle);
             if C<>Screen.Cursor then Screen.Cursor:=C;
             end;
          end
       else
          if (Col<>CursorRect.Left) or
             (Row<>CursorRect.Top) or
             (CursorRect.Right=0) then // '0': there is no cursor on the screen at the moment
             if Sender<>nil then SetCursor(Cursor,Rect(Col,Row,Succ(Col),Succ(Row)));
       end;

    if GameViewer.BoardDimensionsAsText=bdColRow then begin
       if   MouseButtonDown and IsDragging then with CursorRect do
            s:=Format(FORMAT_BOARD_POSITION,[Succ(Left),Succ(Top)])
       else s:=Format(FORMAT_BOARD_POSITION,[Succ(Col),Succ(Row)]);
       if Editor.Selection.Enabled then with CursorRect do
          s:=s+Format(' - %dx%d',[Right-Left,Bottom-Top]);
       end
    else begin
       if   MouseButtonDown and IsDragging then with CursorRect do
            s:=Format(FORMAT_BOARD_POSITION,[Succ(Top),Succ(Left)])
       else s:=Format(FORMAT_BOARD_POSITION,[Succ(Row),Succ(Col)]);
       if Editor.Selection.Enabled then with CursorRect do
          s:=s+Format(' - %dx%d',[Bottom-Top,Right-Left]);
       end;
    StatusLabel1.Caption:=s;
//  StatusLabel2.Caption:=Self.BoardDimensionsAsText(Game.BoardWidth,Game.BoardHeight,Game.BoxCount,Game.GoalCount,GameViewer.BoardDimensionsAsText);
//  StatusLabel3.Caption:=Format('%d/%d %d/%d [%dx%d]',[EditImage1.Picture.BitMap.Width,EditImage1.Picture.BitMap.Height,GameViewer.ColWidth,GameViewer.RowHeight,GameViewer.ColCount,GameViewer.RowCount]);
//  with Editor.BoardCellOrigin do StatusLabel3.Caption:=Format('[%d:%d] - [%d:%d]',[Succ(X),Succ(Y),X+Game.BoardWidth,Y+Game.BoardHeight]);

    SetDrawingToolHint;
    end;
end;

procedure TToolsForm.EditImage1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Point:TPoint;
begin
  with Editor do begin
    if Screen.Cursor<>EditImage1.Cursor then Screen.Cursor:=EditImage1.Cursor;
    IgnoreKeyUp:=False; IgnoreMouseUp:=False;
    
    if IsDragging and (Sender<>nil) then begin
       if ((GameViewer.ColWidth <MAX_EDITOR_SQUARE_SIZE)
           or
           (GameViewer.RowHeight<MAX_EDITOR_SQUARE_SIZE)
          ) then begin
          end;
       end;

    if MouseButtonDown then with CursorRect do begin
       MouseButtonDown:=False; SizeHandle:=ghNull; IsDragging:=False;
       if IsSelectingANewArea and
          (Cursor=ctSelection) and Editor.Selection.Enabled and
          (Left=Pred(Right)) and (Top=Pred(Bottom)) then begin
          Editor.Selection.Enabled:=False;
          SetCursor(Cursor,CursorRect);
          SetDrawingToolHint;
          end;
       IsSelectingANewArea:=False; ToggledItemPos.X:=-1; ToggledItemPos.Y:=-1;

       with GameViewer.MouseSuperPosition do
         if (X<>0) or (Y<>0) then begin
            Point:=EditImage1.ScreenToClient(Mouse.CursorPos);
            Dec(Point.X,X); Dec(Point.Y,Y);
            Mouse.CursorPos:=EditImage1.ClientToScreen(Point);
            end;
       end
    else begin
       MouseButtonDown:=False; SizeHandle:=ghNull; IsDragging:=False;
       IsSelectingANewArea:=False; ToggledItemPos.X:=-1; ToggledItemPos.Y:=-1;
       Point:=EditImage1.ScreenToClient(Mouse.CursorPos);
       EditImage1MouseMove(Sender,Shift,Point.X,Point.Y);
       end;

    with GameViewer.MouseSuperPosition do begin X:=0; Y:=0; end;

    if (Cursor<>ctSelection) and (Editor.History<>nil) then
       Editor.History.EndTransaction(True);

    if Editor.Selection.Enabled then SetDrawingToolHint;
    ShowStatus;
    end;
end;

function TToolsForm.AddColRowToBoard(Col__,Row__:Integer):Boolean;
var DX,DY:Integer; Item:TEditorHistoryItem;
begin
  Result:=True;
  with Editor do with BoardCellOrigin do
    if (Game.BoardWidth<>0) and (Game.BoardHeight<>0) then begin
       if Col__<X then begin DX:=X-Col__; Result:=False; end
       else DX:=0;
       if Row__<Y then begin DY:=Y-Row__; Result:=False; end
       else DY:=0;

       if (not Result) and History.BeginTransaction(1) then begin
          Item:=History.MakeItem(ehaTranslate,DX,DY,0,History.Position);
          if History.AddItem(Item) then begin
             History.DoItem(Item);
             Dec(X,DX); Dec(Y,DY);
             Result:=True;
             end;
          end;

       if Result then begin
          if Col__>=X+Game.BoardWidth  then Game.BoardWidth :=Succ(Col__-X);
          if Row__>=Y+Game.BoardHeight then Game.BoardHeight:=Succ(Row__-Y);
          end;
       end
    else begin
       X:=Col__; Y:=Row__; Game.BoardWidth:=1; Game.BoardHeight:=1;
       end;
end;

function TToolsForm.EraseGameBoardSquare(Col__,Row__:Integer):Integer; // 1-based columns and rows; returns the pieces on this square before deletion
begin
  with Game do begin
    Result:=(Board[Col__,Row__] and BOARD_PIECES);
    if (Result and PLAYER)<>0 then begin
       PlayerPos.X:=0; PlayerPos.Y:=0;
       Dec(Board[Col__,Row__],PLAYER); Modified:=True;
       end;
    if (Result and BOX)<>0 then begin
       Dec(BoxCount);
       Dec(Board[Col__,Row__],BOX); Modified:=True;
       end;
    if (Result and GOAL)<>0 then begin
       Dec(GoalCount);
       Dec(Board[Col__,Row__],GOAL); Modified:=True;
       end;
    if Board[Col__,Row__]<>FLOOR then begin
       Board[Col__,Row__]:=FLOOR; Modified:=True;
       end;
    end;
end;

function TToolsForm.SetGameBoardSquare(Col__,Row__,Value__:Integer):Integer; // 1-based columns and rows; returns the pieces on this square before updating
begin
  with Game do begin
    Result:=(Board[Col__,Row__] and BOARD_PIECES);
    if Value__<>Result then begin
       EraseGameBoardSquare(Col__,Row__);
       if (Value__ and PLAYER)<>0 then with PlayerPos do begin
          if (X<>0) and (Y<>0) then Board[X,Y]:=Board[X,Y] and (not PLAYER);
          X:=Col__; Y:=Row__;
          end;
       if (Value__ and BOX )<>0 then Inc(BoxCount);
       if (Value__ and GOAL)<>0 then Inc(GoalCount);
       Board[Col__,Row__]:=Value__; Modified:=True;
       if (Value__ and BOARD_PIECES)<>FLOOR then begin
          Game.BoardWidth :=Max(Game.BoardWidth ,Col__);
          Game.BoardHeight:=Max(Game.BoardHeight,Row__);
          end
       else begin
          if Col__=Game.BoardWidth  then
             while (Game.BoardWidth <>0) and IsAnEmptyCol(Game.BoardWidth ) do
               Dec(Game.BoardWidth );
          if Row__=Game.BoardHeight  then
             while (Game.BoardHeight<>0) and IsAnEmptyRow(Game.BoardHeight) do
               Dec(Game.BoardHeight);
          end;
       end;
    end;
end;

function  TToolsForm.BoardDimensionsAsText(ColCount__,RowCount__,BoxCount__,GoalCount__:Integer; BoardDimensionsAsText__:TBoardDimensionsAsText):String;
begin
  if   BoardDimensionsAsText__=bdColRow then
       Result:=Format(FORMAT_BOARD_DIMENSIONS,[ColCount__,RowCount__,BoxCount__,GoalCount__])
  else Result:=Format(FORMAT_BOARD_DIMENSIONS,[RowCount__,ColCount__,BoxCount__,GoalCount__]);
end;

function TToolsForm.ResizeImage(Image__:TImage):Boolean;
begin
  Result:=True;
  with Image__.Picture.BitMap do
    try
        if Width <>Image__.ClientWidth  then Width :=Image__.ClientWidth;
        if Height<>Image__.ClientHeight then Height:=Image__.ClientHeight;
        if (Width<>Image__.ClientWidth) or  (Height<>Image__.ClientHeight) then
           raise Exception.Create(TEXT_MEMORY_FULL);
    except on E:Exception do Result:=Error(E.Message,Application.Title);
    end;
end;

function TToolsForm.BoardToText(BoardWidth__,BoardHeight__:Integer; const Board__:TBoard; const Rect__:TRect;
                                var Text__:String; var NonFloorCellCount__:Integer):Boolean; // 1-based columns and rows
var i,Col,Row:Integer; FloorFillChar:Char; s:String;
begin
  Result:=False; Text__:=''; NonFloorCellCount__:=0;
  with Rect__ do
    if (Left<Right) and (Top<Bottom) then
       try    SetLength(s,Right-Left);
              if   MainForm.CopyLevelToClipboardFillFloors then
                   FloorFillChar:=MainForm.CopyLevelToClipboardFloorFillCharacter
              else FloorFillChar:=FLOOR_CH;

              for Row:=Top to Pred(Bottom) do begin
                  if (Row>=1) and (Row<=BoardHeight__) then begin
                     i:=1;
                     for Col:=Left to Pred(Right) do begin
                         if (Col>=1) and (Col<=BoardWidth__) then
                            case Board__[Col,Row] and (WALL+PLAYER+BOX+GOAL) of
                              WALL         : begin s[i]:=WALL_CH;        Inc(NonFloorCellCount__); end;
                              PLAYER       : begin s[i]:=PLAYER_CH;      Inc(NonFloorCellCount__); end;
                              PLAYER+GOAL  : begin s[i]:=PLAYER_GOAL_CH; Inc(NonFloorCellCount__); end;
                              BOX          : begin s[i]:=BOX_CH;         Inc(NonFloorCellCount__); end;
                              BOX+GOAL     : begin s[i]:=BOX_GOAL_CH;    Inc(NonFloorCellCount__); end;
                              GOAL         : begin s[i]:=GOAL_CH;        Inc(NonFloorCellCount__); end;
                              else {FLOOR}   s[i]:=FloorFillChar;
                            end {case}
                         else s[i]:=FloorFillChar;
                         Inc(i);
                         end;
                     end
                  else
                     for i:=1 to Length(s) do s[i]:=FloorFillChar;

                  if Text__<>'' then Text__:=Text__+NL;
                  Text__:=Text__+s;
                  end;

              Result:=True;
       except on E:Exception do Result:=Error(E.Message,Application.Title);
       end;
end;

function TToolsForm.TextToBoard(const Text__:String; var BoardWidth__,BoardHeight__:Integer; var PlayerPos__:TPoint; var Board__:TBoard):Boolean; // 1-based columns and rows
const MAX_DISPLAY_TEXT_LENGTH=80;
var Col,i,j,Len,Pass,Row:Integer; Ch:Char; ErrorStr,s,s2:String;

  function LoadLevel(const InputText__:String; var OutputText__:String):Boolean;
  var SokoFile:TSokoFile;
  begin
    Result:=False; OutputText__:=''; SokoFile:=nil;
    try     try    SokoFile:=TSokoFile.Create;
                   if SokoFile.LoadFromText(InputText__) and
                      (SokoFile.Levels.Count>=1) then
                      with TLevel(SokoFile.Levels.First) do
                        Result:=BoardAsTextLines.ToText(NL,OutputText__) and
                                (OutputText__<>'');
            except on E:Exception do Result:=Error(E.Message,Application.Title);
            end;
    finally SokoFile.Free;
    end;
  end;

begin // TextToBoard
  Result:=False;
  try // 'try..except' to catch string-operations throwing 'EOutOfMemory'
    s:=Text__; Len:=Length(s);
    if Len<>0 then begin
       Result:=True;
       BoardHeight__:=0; BoardWidth__:=0; PlayerPos__.X:=0; PlayerPos__.Y:=0;
       ClearBoard(Board__);
       i:=0; Col:=0; Row:=1; ErrorStr:=''; Pass:=1;
       while (i<Len) and Result do begin
         Inc(i); Ch:=s[i];
         j:=CharToBoardSquareValue(Ch);
         if      j<>0 then begin
                 if (Col<MAX_BOARD_WIDTH) and (Row<=MAX_BOARD_HEIGHT) then begin
                    Inc(Col);
                    if Col>BoardWidth__  then BoardWidth__ :=Col;
                    if Row>BoardHeight__ then BoardHeight__:=Row;
                    Board__[Col,Row]:=j;
                    if (j and PLAYER)<>0 then begin
                       if PlayerPos__.X<>0 then
                          Dec(Board__[PlayerPos__.X,PlayerPos__.Y],PLAYER);
                       PlayerPos__.X:=Col; PlayerPos__.Y:=Row;
                       end;
                    end
                 else begin
                    Result:=False;
                    ErrorStr:=TEXT_TEXT_TOO_LARGE;
                    end
                 end
         else if (Ch=CR) or (Ch=LF) then begin
                 Col:=0; Inc(Row);
                 while (i<Len) and
                       ((s[Succ(i)]=CR) or (s[Succ(i)]=LF)) do
                       Inc(i);
                 end
         else if Ch=EMAIL_QUOTE_CH then begin // drop leading '>' characters
                 if Col<>0 then begin
                    Result:=False;
                    ErrorStr:=Format(TEXT_ILLEGAL_CHAR_FORMAT,[Ch]);
                    end
                 end
         else begin
                 if (Pass=1) and LoadLevel(s,s2) then begin
                    s:=s2; Len:=Length(s);
                    BoardHeight__:=0; BoardWidth__:=0; PlayerPos__.X:=0; PlayerPos__.Y:=0;
                    ClearBoard(Board__);
                    i:=0; Col:=0; Row:=1; ErrorStr:=''; Pass:=2;
                    end
                 else begin
                    Result:=False;
                    ErrorStr:=Format(TEXT_ILLEGAL_CHAR_FORMAT,[Ch]);
                    end;
                 end;
         end;

       Result:=Result and (BoardWidth__>=1) and (BoardHeight__>=1);

       if ErrorStr<>'' then begin
          if Length(s)>MAX_DISPLAY_TEXT_LENGTH then
             Delete(s,MAX_DISPLAY_TEXT_LENGTH+1,Length(s));
          Msg(Format(InvalidBoardInClipboardText__,[s,ErrorStr]),Application.Title+SUB_TITLE_SEPARATOR+InsertClipboardContents,MB_OK+MB_ICONINFORMATION);
          end;

       if not Result then begin
          BoardHeight__:=0; BoardWidth__:=0; PlayerPos__.X:=0; PlayerPos__.Y:=0;
          end;
       end;
  except on E:Exception do begin
            BoardHeight__:=0; BoardWidth__:=0; PlayerPos__.X:=0; PlayerPos__.Y:=0;
            Result:=Error(E.Message,Application.Title+SUB_TITLE_SEPARATOR+InsertClipboardContents);
         end;
  end;
end;

function TToolsForm.RectPlusOffset(const Rect__:TRect; OffsetX__,OffsetY__:Integer):TRect;
begin
  with Rect__ do Result:=Rect(Left+OffsetX__,Top+OffsetY__,Right+OffsetX__,Bottom+OffsetY__);
end;

function TToolsForm.CloseEditorSelection(Commit__,AutoSize__:Boolean):Boolean;
var i,C,R,Count,Col,Row:Integer;
begin
  Result:=True; Count:=0;
  with Editor do with Selection do
    try
      if HasBoard then begin
         HasBoard:=False;

         if Commit__ and
            Editor.History.BeginTransaction(4*MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT) then begin
            for Col:=Rect.Left to Pred(Rect.Right) do
                for Row:=Rect.Top to Pred(Rect.Bottom) do
                    if (Col>=0) and (Col<MAX_BOARD_WIDTH) and
                       (Row>=0) and (Row<MAX_BOARD_HEIGHT) then begin
                       i:=CharToBoardSquareValue(BoardAsText[Succ((Col-Rect.Left)+((Row-Rect.Top)*(BoardWidth+Length(NL))))]);
                       if (i<>0) and
                          (i<>CellValue(Col,Row)) and
                          Result then
                          if AddColRowToBoard(Col,Row) then begin
                             C:=Succ(Col-Editor.BoardCellOrigin.X);
                             R:=Succ(Row-Editor.BoardCellOrigin.Y);
                             if ((Game.Board[C,R]=FLOOR)
                                 or
                                 History.AddItem(History.MakeItem(ehaErase ,C,R,Game.Board[C,R],History.Position))
                                )
                                and
                                History.AddItem(History.MakeItem(ehaSquare,C,R,i               ,History.Position))
                                then begin
                                Inc(Count);
                                SetGameBoardSquare(C,R,i);
                                end
                             else
                                Result:=False;
                             end
                          else Result:=False;
                       end;

            if CalculateGameInformation(AutoSize__) then UpdateView;
            end;

         for Col:=Rect.Left to Pred(Rect.Right) do
             for Row:=Rect.Top to Pred(Rect.Bottom) do
                 ShowSquare(Col,Row); // update screen

         //ShowSquares;
         SetCursor(Editor.Cursor,Editor.CursorRect);

         if (Count<>0) then begin
            Modified:=True;
            ShowStatus;
            end;
         end;
    finally
      if Commit__ then Result:=History.EndTransaction(Result);
    end;
end;

function TToolsForm.UpdateView:Boolean;
var H,W,Col,Row:Integer; OK:Boolean; BoundsRect,R:TRect;

  procedure MoveHorizontally(Delta__:Integer);
  begin
    with BoundsRect             do begin Inc(Left,Delta__); Inc(Right  ,Delta__); end;
    with Editor.CursorRect      do begin Inc(Left,Delta__); Inc(Right  ,Delta__); end;
    with Editor.Selection.Rect  do begin Inc(Left,Delta__); Inc(Right  ,Delta__); end;
    with Editor.BoardCellOrigin do       Inc(X   ,Delta__);
  end;

  procedure MoveVertically(Delta__:Integer);
  begin
    with BoundsRect             do begin Inc(Top  ,Delta__); Inc(Bottom,Delta__); end;
    with Editor.CursorRect      do begin Inc(Top  ,Delta__); Inc(Bottom,Delta__); end;
    with Editor.Selection.Rect  do begin Inc(Top  ,Delta__); Inc(Bottom,Delta__); end;
    with Editor.BoardCellOrigin do       Inc(Y    ,Delta__);
  end;

begin // returns 'True' if the board changes position on the screen
  Result:=False; OK:=True;
  with BoundsRect do begin
    with Editor.BoardCellOrigin do R:=Rect(X,Y,X+Game.BoardWidth,Y+Game.BoardHeight);
    if (not Editor.Selection.HasBoard) or
       (not UnionRect(BoundsRect,R,Editor.Selection.Rect)) then
       BoundsRect:=R;

    if (Left=0) and (Right-Left<MAX_BOARD_WIDTH) then begin
       MoveHorizontally(1); Result:=True;
       end;

    if (Top=0) and (Bottom-Top<MAX_BOARD_HEIGHT) then begin
       MoveVertically(1); Result:=True;
       end;

    if (Pred(Right)=GameViewer.ColCount)
       and
       (Right-Left<MAX_BOARD_WIDTH) then
       if Left<2 then OK:=False
       else begin
          MoveHorizontally(-1); Result:=True;
          end;

    if (Pred(Bottom)=GameViewer.RowCount)
       and
       (Bottom-Top<MAX_BOARD_HEIGHT) then
       if Top<2 then OK:=False
       else begin
          MoveVertically(-1); Result:=True;
          end;

    if (Pred(Right)>GameViewer.ColCount) or
       (Pred(Bottom)>GameViewer.RowCount) then OK:=False;

    if OK and
       (GameViewer.ColWidth <MAX_EDITOR_SQUARE_SIZE) and
       (GameViewer.RowHeight<MAX_EDITOR_SQUARE_SIZE) then begin
       W:=EditImage1.ClientWidth -2*MIN_BORDER_SIZE;
       H:=EditImage1.ClientHeight-2*MIN_BORDER_SIZE;
       Col:=Min(MAX_BOARD_WIDTH ,Max(W div MAX_EDITOR_SQUARE_SIZE,Max(MIN_BOARD_WIDTH,Right-Left+2*EDITOR_GUTTER_SQUARE_COUNT)));
       Row:=Min(MAX_BOARD_HEIGHT,Max(H div MAX_EDITOR_SQUARE_SIZE,Max(MIN_BOARD_HEIGHT,Bottom-Top+2*EDITOR_GUTTER_SQUARE_COUNT)));
       if (Abs(Col-GameViewer.ColCount)>2) or
          (Abs(Row-GameViewer.RowCount)>2) then OK:=False;
       end;

    if not OK then begin
       InitializeGameViewer(False); ShowGameBackground; Result:=True;

       with Editor.BoardCellOrigin do R:=Rect(X,Y,X+Game.BoardWidth,Y+Game.BoardHeight);
       if (not Editor.Selection.HasBoard) or
          (not UnionRect(BoundsRect,R,Editor.Selection.Rect)) then
          BoundsRect:=R;

       Col:=((Succ(GameViewer.ColCount)-(Right-Left)) div 2);
       if Col<>Left then begin MoveHorizontally(Col-Left); Result:=True; end;
       Row:=((Succ(GameViewer.RowCount)-(Bottom-Top)) div 2);
       if Row<>Top  then begin MoveVertically  (Row-Top ); Result:=True; end;
       end;

    ShowSquares;
    end;
end;

function TToolsForm.CalculateGameInformation(TopLeftJustify__:Boolean):Boolean;
var W,H:Integer; BoardBoundsRect:TRect; Item:TEditorHistoryItem;
begin // returns 'True' if the board changes dimensions or position on the screen
  Result:=False;
  Tools_.CalculateGameInformation(Game);

  with BoardBoundsRect do begin
    BoardBoundsRect:=Rect(1,1,MAX_BOARD_WIDTH+1,MAX_BOARD_HEIGHT+1);
    while (Left<Right)  and Game.IsAnEmptyCol(Pred(Right))  do Dec(Right);
    while (Top <Bottom) and Game.IsAnEmptyRow(Pred(Bottom)) do Dec(Bottom);

    while (Left<Right)  and Game.IsAnEmptyCol(Left)         do Inc(Left);
    while (Top <Bottom) and Game.IsAnEmptyRow(Top)          do Inc(Top);

    W:=Right-Left; H:=Bottom-Top;

    if (Left<Right) and (Top<Bottom) and
       ((Left>1) or (Top>1)) then begin

       Item:=Editor.History.MakeItem(ehaTranslate,-Pred(Left),-Pred(Top),0,Editor.History.Position);
       Editor.History.DoItem(Item);
       if   Editor.History.BeginTransaction(1) and
            Editor.History.AddItem(Item) then begin
            end
       else Editor.History.Clear; // the board must internally be top-left justified, hence, there is no way to go back

       if TopLeftJustify__ then BoardBoundsRect:=Rect(1,1,Succ(W),Succ(H));

       Inc(Editor.BoardCellOrigin.X,Pred(Left));
       Inc(Editor.BoardCellOrigin.Y,Pred(Top ));
       Result:=True;
       end;

    Result:=Result or (W<>Game.BoardWidth) or (H<>Game.BoardHeight);
    if Result then begin
       Game.BoardWidth:=W; Game.BoardHeight:=H;
       end;
    end;
end;

procedure TToolsForm.EditPopupMenuUndoRedoPopup(Sender: TObject);
var i,m,n:Integer; MenuItem:TMenuItem;
begin
  with EditPopupMenuUndoRedo do begin
    if   Tag=EditToolButtonUndo.Tag then
         m:=Editor.History.Position
    else m:=Editor.History.Count-Editor.History.Position;

    n:=-1;
    for i:=0 to Pred(Items.Count) do begin
        if   (Items[i].Tag>0)
             and
             ((n<0) or (Items[i].Tag>Items[n].Tag)) then
             n:=i;
        if   m>2 then
             Items[i].Visible:=Items[i].Tag<m
        else Items[i].Visible:=Items[i].Tag<=m;
        end;

    with EditMenuItemUndoRedoAllTransactions do begin
      if   (m<High(m)) and
           (m>2) then
           Caption:=Format(AllTransactionsText__,[m])
      else Caption:=AllTransactionsText;
      Visible:=m>2;
      EditMenuItemUndoRedoAllTransactionsSeparator.Visible:=Visible;
      end;

    while (n>=0) and
          (Items[n].Tag<=High(Items[n].Tag) div 2) and
          (Items[n].Tag<m)
          do
          try    MenuItem:=TMenuItem.Create(Self);
                 MenuItem.Tag:=Items[n].Tag*2;
                 MenuItem.Caption:=IntToStr(MenuItem.Tag)+SPACE+TransactionsText;
                 MenuItem.OnClick:=Self.EditMenuItemUndoRedoTransactionClick;
                 if   m>2 then
                      MenuItem.Visible:=MenuItem.Tag<m
                 else MenuItem.Visible:=MenuItem.Tag<=m;
                 Inc(n); Items.Insert(n,MenuItem);
          except on E:Exception do begin
                    n:=-1; Error(E.Message,Application.Title);
                    end;
          end;

    //for i:=0 to Pred(Items.Count) do Items[i].Visible:=True;
    end;
end;

procedure TToolsForm.EditToolButtonUndoRedoMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  FormMouseMove(Sender,Shift,X,Y);
  if Sender is TToolButton then EditPopupMenuUndoRedo.Tag:=TToolButton(Sender).Tag;
end;

procedure TToolsForm.EditMenuItemRefreshClick(Sender: TObject);
var Col,Row:Integer; oIsBusy:Boolean; Point:TPoint;
begin
  oIsBusy:=Game.IsBusy;

  if (Screen.ActiveForm=Self)
     and
     (EditMenuItemRefresh.Enabled
      or
      (Sender=nil)
      or
      (Sender=Editor.History)
     ) then
     try
       Game.IsBusy:=True;

       InitializeGameViewer((Sender<>nil){ and (Sender<>Editor.History)});
       ShowGameBackground; ShowSquares;

       with Editor do
         if   not Editor.Selection.Enabled then begin
              Point:=EditImage1.ScreenToClient(Mouse.CursorPos);
              MouseToCell(Point.X,Point.Y,Col,Row);
              SetCursor(Cursor,Rect(Col,Row,Succ(Col),Succ(Row)));
              end
         else SetCursor(Cursor,CursorRect);
     finally
       if Sender=Editor.History then Editor.History.EndTransaction(True);
       Game.IsBusy:=oIsBusy; SetDrawingToolHint; ShowStatus;
     end;
end;

function TToolsForm.CutSelectionToEditorClipboard:Boolean;
var C,R,Col,Row:Integer; oModified:Boolean;
begin
  Result:=False;
  with Editor do
    if (CursorRect.Left<CursorRect.Right) and (CursorRect.Top<CursorRect.Bottom) and
       BoardToText(Game.BoardWidth,Game.BoardHeight,Game.Board,CellRectToGameBoardRect(CursorRect),Editor.Selection.BoardAsText,Editor.Selection.NonFloorCellCount) then
       with Editor.Selection do begin
         BoardWidth :=CursorRect.Right-CursorRect.Left;
         BoardHeight:=CursorRect.Bottom-CursorRect.Top;
         Rect       :=CursorRect;
         HasBoard   :=(BoardWidth >=1) and
                      (BoardHeight>=1) and
                      Editor.History.BeginTransaction(4*BoardWidth*BoardHeight);
         if HasBoard then with Rect do begin
            oModified:=Modified;
            try      for Col:=Left to Pred(Right) do
                         for Row:=Top to Pred(Bottom) do begin
                             C:=Col-Pred(Editor.BoardCellOrigin.X);
                             R:=Row-Pred(Editor.BoardCellOrigin.Y);
                             if (C>=1) and (C<=Game.BoardWidth) and
                                (R>=1) and (R<=Game.BoardHeight) and
                                HasBoard then
                                if   Editor.History.AddItem(Editor.History.MakeItem(ehaErase,C,R,Game.Board[C,R],Editor.History.Position)) then
                                     EraseGameBoardSquare(C,R)
                                else HasBoard:=False;
                             end;
            finally Modified:=oModified;
                    CalculateGameInformation(False);
            end;
            end;
         Result:=HasBoard;
         end;
end;

function TToolsForm.ScrollEditorCursorInView:Boolean;
var i:Integer; BoardBoundsRect,R:TRect;
begin
  Result:=False;
  with Editor.CursorRect do begin

    if (Game.BoardWidth<>0) and (Game.BoardHeight<>0) then
       with Editor.BoardCellOrigin do BoardBoundsRect:=Rect(X,Y,X+Game.BoardWidth,Game.BoardHeight)
    else BoardBoundsRect:=Editor.CursorRect;

    if UnionRect(R,BoardBoundsRect,Editor.CursorRect) then begin
       i:=R.Right-GameViewer.ColCount;
       if i>0 then with Editor.CursorRect do begin Dec(Left,i); Dec(Right ,i); Result:=True; end;
       i:=R.Bottom-GameViewer.RowCount;
       if i>0 then with Editor.CursorRect do begin Dec(Top ,i); Dec(Bottom,i); Result:=True; end;
       end
    else begin
       Editor.CursorRect:=Rect(0,0,Right-Left,Bottom-Top); Result:=True;
       end;

    if Left<0 then begin Dec(Right  ,Left); Left:=0; Result:=True; end;
    if Top <0 then begin Dec(Bottom ,Top ); Top :=0; Result:=True; end;

    if (Left=0) and (Right-Left<MAX_BOARD_WIDTH -1) then begin Inc(Left); Inc(Right ); end;
    if (Top =0) and (Bottom-Top<MAX_BOARD_HEIGHT-1) then begin Inc(Top ); Inc(Bottom); end;
    end;
end;

procedure TToolsForm.EditMenuItemHistoryLogClick(Sender: TObject);
begin
  if Editor.History<>nil then Editor.History.Log;
end;

procedure TToolsForm.EditMenuItemUndoRedoTransactionClick(Sender: TObject);
begin
  if   EditPopupMenuUndoRedo.Tag=EditToolButtonUndo.Tag then
       EditMenuItemUndoClick(Sender)
  else EditMenuItemRedoClick(Sender);
end;

function TToolsForm.CheckSnapshots(Game__,TestGame__:TSokoGame; AddHistoryOnly__:Boolean; var SolutionCount__:Integer):Boolean;
// precondition: 'Game__.DeadlockDetection.Deadlocks'=nil
var i,j,k,oSimpleIllegalMovesMask:Integer;
    __IsAFreezingMove,oMoveAnimationEnabled,oReverseMode,oTimingEnabled:Boolean;
    oCursor:TCursor;
    oHistory:THistory; HistoryMoves:PHistoryMoves;
    T:TBoardTransformation2D; TSet:TBoardTransformation2DSet;
    V:TSnapshot; W:TMultiViewItem;

  function Replay(var MoveCount__   :Integer;
                  var MoveTop__     :Integer;
                  var MoveCapacity__:Integer;
                  var Moves__       :PHistoryMoves;

                  ReverseMode__     :Boolean;
                  GameState__       :TGameState;

                  var TSet__        :TBoardTransformation2DSet;

                  var PlayerPos__   :TColRow;
                  var BoxPos__      :TBoxPositions;
                  var PushCount__   :Integer;
                  var LastBoxNo__   :Integer):Boolean;
  var dx,dy,MoveIndex{,PlayerMoveCount,PreviousPushMoveIndex}:Integer;
      {PlayerPosAfterPreviousPush,PlayerPosBeforeNextPush:TColRow;}
      T:TBoardTransformation2D; Move:THistoryMove; {PlayerMoves:TMoves;}
  begin
    if (Moves__<>nil) and (MoveTop__>=0) and (TSet__<>[]) then // 'MoveTop__>=0': because 'Replay' must initialize the board even when the number of moves = 0
       for T:=Low(T) to High(T) do
           if T in TSet__ then with Game__ do begin
              ReverseMode:=ReverseMode__;
              History.Top:=0; Reset(True);                           {reset the game}

              //PreviousPushMoveIndex:=History.Count;
              //PlayerPosAfterPreviousPush:=PlayerPos;
              PlayerPos__:=PlayerPos; BoxPos__:=BoxPos;
              PushCount__:=History.PushCount; LastBoxNo__:=History.LastBoxNo;

              Result:=(ForcedInitialJumps=0) and                     {this function is not set up to handle forced initial jumps}
                      (History.Count=0);                             {'0': this should be redundant but better safe than sorry}

              MoveIndex:=History.Count;
              while Result and (MoveIndex<MoveTop__) and (GameState<>gsSolved) do begin
                Inc(MoveIndex);
                Move:=Moves__^[MoveIndex];
                Move:=(Move and (not H_MASK_DIRECTION)) or
                      Ord(T2D_DIRECTION[T,TDirection(Move and H_MASK_DIRECTION)]);
                dx:=DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),ColAxis];
                dy:=DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),RowAxis];

                if IsALegalMove(dx,dy,Move,__IsAFreezingMove) then begin {if next move is legal, do it}
                   DoMoveUpdateGame(dx,dy,-1,Move);
                   Result:=((Move and (not H_MASK_MOVE_SEPARATOR))=(History.Moves[History.Count] and (not H_MASK_MOVE_SEPARATOR)))
                           or
                           AddHistoryOnly__; {relaxed matching when the task is to add a game from history}

                   if Result and (MoveCount__>=0) then begin         {update}
                      Moves__^[MoveIndex]:=History.Moves[History.Count] or (Move and H_MASK_MOVE_SEPARATOR); {keep combined move separator}
                      if (History.Count=MoveCount__) or
                         ((GameState=gsSolved) and (History.Count<MoveCount__)) then begin
                         MoveCount__:=History.Count;
                         PlayerPos__:=PlayerPos; BoxPos__:=BoxPos;
                         PushCount__:=History.PushCount; LastBoxNo__:=History.LastBoxNo;
                         end;
                      end;

                   if (Move and H_FLAG_BOX)<>0 then begin
                      //PreviousPushMoveIndex:=History.Count;
                      //PlayerPosAfterPreviousPush:=PlayerPos;       {remember the player's position after the last push}
                      end;
                   end
                else begin
                   Result:=False;
                   (*
                   if (Move and H_FLAG_BOX)=0 then begin             {'True': an illegal player-move; try to find another path for the player}
                      PlayerPosBeforeNextPush:=PlayerPos;
                      while (MoveIndex<=MoveTop__) and ((Move and H_FLAG_BOX)=0) do begin
                        Inc(PlayerPosBeforeNextPush.X,dx);
                        Inc(PlayerPosBeforeNextPush.Y,dy);
                        Inc(MoveIndex);
                        if MoveIndex<=MoveTop__ then begin
                           Move:=Moves__^[MoveIndex];                {next move}
                           Move:=(Move and (not H_MASK_DIRECTION)) or
                                 Ord(T2D_DIRECTION[T,TDirection(Move and H_MASK_DIRECTION)]);
                           dx:=DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),0];
                           dy:=DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),1];
                           end;
                        end;
                      Dec(MoveIndex);                                {adjust the index for the main 'while'-loop}

                      with PlayerPosBeforeNextPush do begin
                        if (X>=1) and (X<=Game__.BoardWidth) and
                           (Y>=1) and (Y<=Game__.BoardHeight) and
                           ((Game__.Board[X,Y] and (WALL+BOX+FLOOR))=FLOOR) then begin
                           if Game__.PlayerPath(PlayerPosAfterPreviousPush,PlayerPosBeforeNextPush,MoveCount__>=0,PlayerMoveCount,PlayerMoves) then begin
                              //Result:=True;
                              // update game
                              end;
                           end;
                        end;
                      end;
                   *)
                   end;
                end;

              if Result
                 and
                 ((MoveIndex=MoveTop__)
                  or
                  (GameState=gsSolved)
                 )
                 and
                 ((GameState=GameState__)
                  or
                  ((GameState__=gsPlay)
                   and
                   ((GameState=gsSolved)
                    or
                    (GameState=gsStop)
                   )
                  )
                 ) then begin
                 end
              else
                 Exclude(TSet__,T);
              end;

    Result:=TSet__<>[];
  end; // Replay

  function CheckSnapshot(Snapshot__:TSnapshot; var TSet__:TBoardTransformation2DSet):Boolean;
  var i:Integer;
  begin
    if (Snapshot__<>nil) and (Snapshot__.Moves<>nil) then with Snapshot__ do begin
       i:=-1;
       Replay(i,MoveTop,MoveCapacity,Moves,ReverseMode,GameState,TSet__,PlayerPos,BoxPos,PushCount,LastBoxNo);
       end;
    Result:=TSet__<>[];
  end; // CheckSnapshot

  function TransformSnapshot(Snapshot__:TSnapshot; var TSet__:TBoardTransformation2DSet):Boolean;
  begin
    if (Snapshot__<>nil) and (Snapshot__.Moves<>nil) then with Snapshot__ do begin
       Replay(MoveCount,MoveTop,MoveCapacity,Moves,ReverseMode,GameState,TSet__,PlayerPos,BoxPos,PushCount,LastBoxNo);
       if (TSet__<>[]) and (TSet__<>[t2DRotate0DegreesClockwise]) then Modified:=True;
       if Game__.GameState=gsSolved then Inc(SolutionCount__);
       end;
    Result:=TSet__<>[];
  end; // TransformSnapshot

begin // CheckSnapshots
  Result:=False; SolutionCount__:=0;
  MainForm.Game.StopTimer;
  with Game__ do begin
    oMoveAnimationEnabled:=MainForm.Game.MoveAnimationEnabled;
    oSimpleIllegalMovesMask:=SimpleIllegalMovesMask;
    oTimingEnabled:=MainForm.Game.TimingEnabled;
    oHistory:=History; oReverseMode:=ReverseMode; oCursor:=Screen.Cursor;

    try
      Screen.Cursor:=crHourGlass; TSet:=[];
      for T:=Low(T) to High(T) do Include(TSet,T);
      SimpleIllegalMovesMask:=0; // replay games without blocking illegal moves

      with TestGame__ do begin
        i:=-1; j:=History.Top; k:=MAX_MOVES;
        if   Game__=TestGame__ then
             HistoryMoves:=Addr(oHistory.Moves)
        else HistoryMoves:=Addr( History.Moves);
        Replay(i,j,k,HistoryMoves,ReverseMode,gsPlay,TSet,PlayerPos,BoxPos,History.PushCount,History.LastBoxNo);

        if not AddHistoryOnly__ then begin
           // check all solutions and snapshots
           CheckSnapshot(BuiltinBestSolutionMoves,TSet);
           CheckSnapshot(BuiltinBestSolutionPushes,TSet);
           CheckSnapshot(BestSolutionMoves,TSet);
           CheckSnapshot(BestSolutionPushes,TSet);
           CheckSnapshot(SaveGame,TSet);

           V:=TSnapshot(Snapshots.First);
           while V<>nil do begin
             CheckSnapshot(V,TSet);
             V:=TSnapshot(V.Next);
             end;

           if (TestGame__=MainForm.Game) and Assigned(MainForm.MultiView) then begin
              W:=TMultiViewItem(MainForm.MultiView.Items.First);
              while W<>nil do begin
                if Assigned(W.Snapshot) then CheckSnapshot(W.Snapshot,TSet);
                W:=TMultiViewItem(W.Next);
                end;
              end;
           end;

        if TSet<>[] then
           for T:=Low(T) to High(T) do
               if T in TSet then begin
                  TSet:=[T]; // drop all other legal transformations
                  if Game__=TestGame__ then begin // restore the game history
                     Game__.History:=oHistory; Game__.ReverseMode:=oReverseMode;
                     end;
                  i:=History.Count; j:=History.Top; k:=MAX_MOVES; HistoryMoves:=Addr(History.Moves);
                  Replay(i,j,k,HistoryMoves,TestGame__.ReverseMode,gsPlay,TSet,PlayerPos,BoxPos,History.PushCount,History.LastBoxNo);
                  if Game__=TestGame__ then begin // save the transformed game history
                     oHistory.Moves:=History.Moves;
                     oHistory.PushCount:=History.PushCount;
                     end;
                  if Game__.GameState=gsSolved then Inc(SolutionCount__);

                  if not AddHistoryOnly__ then begin
                     TransformSnapshot(BuiltinBestSolutionMoves,TSet);
                     TransformSnapshot(BuiltinBestSolutionPushes,TSet);
                     TransformSnapshot(BestSolutionMoves,TSet);
                     TransformSnapshot(BestSolutionPushes,TSet);
                     TransformSnapshot(SaveGame,TSet);

                     V:=TSnapshot(Snapshots.First);
                     while V<>nil do begin
                       TransformSnapshot(V,TSet);
                       V:=TSnapshot(V.Next);
                       end;

                     if (TestGame__=MainForm.Game) and Assigned(MainForm.MultiView) then begin
                        W:=TMultiViewItem(MainForm.MultiView.Items.First);
                        while W<>nil do begin
                          if Assigned(W.Snapshot) then TransformSnapshot(W.Snapshot,TSet);
                          W:=TMultiViewItem(W.Next);
                          end;
                        end;
                     end;
                  end;

        if TSet<>[] then begin // 'True': checking the snapshots succeeded
           if Game__=TestGame__ then begin
              History:=oHistory; ReverseMode:=oReverseMode; // update the current history
              History.Count:=History.Top; // kludge: the following code-lines "replay" all moves in history, hence, the position must be updated to match the new state
              end;

           for j:=1 to BoardHeight do
               for i:=1 to BoardWidth do
                   Board[i,j]:=Board[i,j] and (BOARD_FLAGS_MASK-BOX-PLAYER); // remove boxes and player
           for i:=1 to BoxCount do with BoxPos[i] do // put boxes at their new positions
               Board[x,y]:=(Board[x,y] or BOX) + (i shl BOARD_FLAG_COUNT);
           if  PlayerPos.x<>0 then with PlayerPos do // put player at its new position
               Board[x,y]:=Board[x,y] or PLAYER;

           Result:=True;
           end
        else begin // one of more solutions/snapshots in ' TestGame__' are invalid; destroy them all
           FillChar(History,SizeOf( History),0);
           if not AddHistoryOnly__ then begin
              DeleteSnapshot(BuiltinBestSolutionMoves);
              DeleteSnapshot(BuiltinBestSolutionPushes);
              DeleteSnapshot(BestSolutionMoves);
              DeleteSnapshot(BestSolutionPushes);
              DeleteSnapshot(SaveGame);
              Snapshots.Clear;
              if (TestGame__=MainForm.Game) and Assigned(MainForm.MultiView) then
                 MainForm.MultiView.Clear;
              end;
           end;
        end;

    finally
      if Game__<>TestGame__ then begin // restore current position in 'Game__'; it probably only works when 'Game__' = 'MainForm.Game' or when the history is empty
         History:=oHistory; ReverseMode:=oReverseMode;
         SimpleIllegalMovesMask:=0; MainForm.Game.MoveAnimationEnabled:=False;
         i:=History.Count;
         Reset(False);
         try     if Game__ is TGame then with Game__ as TGame do IsReplaying:=True;
                 while (History.Count<i) and Redo(False) do begin end;
         finally if Game__ is TGame then with Game__ as TGame do begin
                    IsReplaying:=False;
                    if (Game__=MainForm.Game) and (History.Count<>oHistory.Count) and (@ShowGame<>nil) then
                       ShowGame; // something went terrible wrong and the program didn't replay all the history moves; repair the damage by showing the new current position
                    end;
                 History.Moves:=oHistory.Moves;
                 SeparateUndoneMoves;
         end;
         //if Game__  is TGame then with Game__ as TGame do
         //   if @ShowGame<>nil then ShowGame;
         end;
      MainForm.Game.TimingEnabled:=oTimingEnabled;
      MainForm.Game.MoveAnimationEnabled:=oMoveAnimationEnabled;
      SimpleIllegalMovesMask:=oSimpleIllegalMovesMask;
      Screen.Cursor:=oCursor;
    end;
    end;
end;

procedure TToolsForm.HideCellCursor;
begin
  if      Editor.Selection.Enabled then begin
          HideCursor(False);
          SetDrawingToolHint;
          ShowStatus;
          end
  else if Editor.CursorRect.Left<Editor.CursorRect.Right then begin
          HideCursor(False);
          SetDrawingToolHint;
          ShowStatus;
          end;
end;

procedure TToolsForm.EditMenuItemInternalClipboardClick(Sender: TObject);
begin
  if LevelSetForm<>nil then with LevelSetForm do begin
     if not Visible then Show
     else if   WindowState=wsMinimized then WindowState:=wsNormal
          else SetFocus;
     if PanelGrid.Visible and Grid.Visible then Grid.SetFocus;
     end;
end;

procedure TToolsForm.EditMenuItemCopyToGeneratorClick(Sender: TObject);
var oBoard:TBoard;

  function  MakeLevelLegal:Boolean;
  var i,j,k,Col,Row:Integer;
      Direction:TDirection; s:String;
      Left,Right:array[0..MAX_BOARD_HEIGHT+1] of Integer;

    procedure AddPlayer(Col__,Row__:Integer);
    begin
      with Game do begin
        Board[Col__,Row__]:=Board[Col__,Row__] or PLAYER;
        PlayerPos.X:=Col__; PlayerPos.Y:=Row__;
        end;
    end;

  begin // 'MakeLevelLegal'; try to make the level legal, e.g., by filling in a missing player and matching numbers of boxes and goals
    with Game do begin
      Tools_.CalculateGameInformation(Game);

      for Row:=1 to BoardHeight do
          for Col:=1 to BoardWidth do begin
              i:=Board[Col,Row];
              if (i and WALL)=0 then begin
                 if      (i and BOX)=0 then begin
                         if ((i and (GOAL+PLAYER))=GOAL) and (GoalCount>BoxCount) then begin
                            Inc(i,BOX); Inc(BoxCount);
                            end;
                         end
                 else if ((i and GOAL)=0) and (GoalCount<BoxCount) then begin
                         Inc(i,GOAL); Inc(GoalCount);
                         end;
                 Board[Col,Row]:=i;
                 end;
              end;

        if PlayerPos.X=0 then begin
           FillChar(Left ,SizeOf(Left ),0);
           FillChar(Right,SizeOf(Right),0);
           for Row:=1 to BoardHeight do // find first and last non-empty column in each row
               for Col:=1 to BoardWidth do begin
                   i:=Board[Col,Row];
                   if   ((i and WALL)<>0)
                        or
                        ((i and (BOX+GOAL))=(BOX+GOAL)) then begin
                        if Left[Row]=0 then Left[Row]:=Col;
                        Right[Row]:=Col;
                        end;
                   end;

           for k:=0 to 1 do
               for Row:=1 to BoardHeight do
                   for Col:=1 to BoardWidth do
                       if   PlayerPos.X=0 then begin
                            i:=Board[Col,Row];
                            if   k=0 then begin
                                 if ((i and (BOX+GOAL))<>0) then // try to add the player on top of a goal square, or adjacent to a goal square or a box square
                                    if   ((i and (WALL+BOX+GOAL+PLAYER))=GOAL) then
                                         AddPlayer(Col,Row)
                                    else for Direction:=Low(Direction) to High(Direction) do begin
                                             i:=Col+DIRECTION_XY[Direction,ColAxis];
                                             j:=Row+DIRECTION_XY[Direction,RowAxis];
                                             if (i>=1) and (i<=BoardWidth) and
                                                (j>=1) and (j<=BoardHeight) and
                                                (PlayerPos.X=0) and
                                                ((Board[i,j] and (WALL+BOX))=0) and
                                                (i>Left[j]) and (i<Right[j]) then
                                                AddPlayer(i,j);
                                             end;
                                 end
                             else if ((i and (WALL+BOX))=0) and
                                     (PlayerPos.X=0) and
                                     (Col>Left[Row]) and (Col<Right[Row]) then
                                     AddPlayer(Col,Row);
                             end
                       else break;
           end;

      Result:=IsALegalLevel(True,False,s);
      end;
  end;

begin // EditMenuItemCopyToGeneratorClick
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (PageControl1.ActivePage=TabSheetEditor) then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
        if (Screen.ActiveForm=Self) and
           EditMenuItemCopyToGenerator.Enabled and
           (Game.BoardWidth>=MIN_BOARD_WIDTH) and (Game.BoardHeight>=MIN_BOARD_HEIGHT) then begin
           CloseEditorSelection(True,False);
           oBoard:=Game.Board;
           try     if (MakeBoardRectangular(Game.BoardWidth,Game.BoardHeight,Game.Board)>=0) and {if the board isn't rectangular, then the generator may accidentally put the player outside the level}
                      MakeLevelLegal and
                      MainForm.Generator.LoadFromGame(Game) then begin
                      if not MainForm.Generator.Initialized then begin
                         MainForm.Generator.Initialized:=True;
                         MainForm.Generator.FileName:=TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER;
                         end;
                      StatusText:=CopiedToGeneratorText;
                      end;
           finally Game.Board:=oBoard; CalculateGameInformation(False);
                   ShowStatus;
           end;
           end;
end;

function TToolsForm.IsAnEmptyRect(const Rect__:TRect):Boolean;
var Col,Row:Integer;
begin
  Result:=True;
  with Rect__ do
    for Col:=Max(1,Left) to Min(Game.BoardWidth,Right) do
        for Row:=Max(1,Top) to Min(Game.BoardHeight,Bottom) do
            if (Game.Board[Col,Row] and BOARD_PIECES)<>FLOOR then begin
               Result:=False; exit;
               end;
end;

procedure TToolsForm.EditMenuItemGridClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then begin
     EditMenuItemGrid.Checked:=not EditMenuItemGrid.Checked;
     Refresh;
     ShowSquares;
     with Editor do SetCursor(Cursor,CursorRect);
     end;
end;

procedure TToolsForm.EditMenuItemMovePlayerAndBoxesClick(
  Sender: TObject);
begin
  with EditMenuItemMovePlayerAndBoxes do Checked:=not Checked;
end;

{$I-}

constructor TEditorHistory.Create(Game__:TSokoGame; Refresh__:TNotifyEvent; CalculateGameInformation__:TCalculateGameInformation);
begin
  Game:=Game__; Refresh:=Refresh__; CalculateGameInformation:=CalculateGameInformation__;
  fFileName:=''; fLastFileName:=''; Clear;
  if SizeOf(TEditorHistoryFileHeader) mod SizeOf(Integer)<>0 then
     raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,['TEditorHistory.Create: Size Error']));
end;

destructor TEditorHistory.Destroy;
begin
  Close;
  if FileName<>'' then // 'Close' didn't succeed; close the file, this time allowing loss of data
     try    CloseFile(HistoryFile); if IOResult<>0 then;
            fFileName:='';
     except on E:Exception do Error(E.Message,Application.Title);
     end;
end;

function TEditorHistory.AddBoard:Boolean;
var Col,Row:Integer;
begin
  if   BeginTransaction(Game.BoardWidth*Game.BoardHeight+2) then
       try     AddItem(MakeItem(ehaBoard,Game.BoardWidth,Game.BoardHeight,0,Position));
               for Col:=1 to Game.BoardWidth do
                   for Row:=1 to Game.BoardHeight do
                       AddItem(MakeItem(ehaSquare,Col,Row,Game.Board[Col,Row],Position));
               AddItem(MakeItem(ehaBoard,Game.BoardWidth,Game.BoardHeight,0,Position));
       finally Result:=EndTransaction(True);
       end
  else Result:=False;
end;

function TEditorHistory.AddItem(const Item__:TEditorHistoryItem):Boolean;
begin // adds 'Item__' to the history and updates 'ItemIndex' so it points to the new item
  try
    Result:=False;
    if   Transaction.Result then
         if   Succ(ItemIndex)<MAX_EDITOR_HISTORY_ITEMS then begin
              Items[Succ(ItemIndex)]:=Item__;
              ItemIndex:=Succ(ItemIndex);
              Result:=True;
              end
         else raise Exception.Create(EditorHistoryFullText);
  except on E:Exception do begin
         Transaction.Result:=False;
         Result:=Error(E.Message,Application.Title);
         end;
  end;
end;

function TEditorHistory.BeginTransaction(ItemCount__:Integer):Boolean;
begin // 'BeginTransaction' can be called more than once for the same transaction
  Result:=ItemIndex<MAX_EDITOR_HISTORY_ITEMS-ItemCount__;
  if Result then begin
     if FileName='' then begin
        New(MakeFileName);
        Result:=FileName<>'';
        end;
     if Result then
        if not IsATransactionInProgress then begin // new transaction
           Transaction.Result        :=True;
           Transaction.StartItemIndex:=ItemIndex;
           end
        else begin
           // continue current transaction
           end;
     end
  else begin // not room for adding 'ItemCount__' items
     Transaction.Result:=False; // note that a current transaction may still be active and waiting for an 'EndTransaction' call
     Error(EditorHistoryFullText,Application.Title);
     end;
end;

function TEditorHistory.CalculateBoardBounds:Boolean;
var W,H:Integer; BoardBoundsRect:TRect;
begin
  Result:=False;
  with BoardBoundsRect do begin
    BoardBoundsRect:=Rect(1,1,MAX_BOARD_WIDTH+1,MAX_BOARD_HEIGHT+1);
    while (Left<Right)  and Game.IsAnEmptyCol(Pred(Right))  do Dec(Right);
    while (Top <Bottom) and Game.IsAnEmptyRow(Pred(Bottom)) do Dec(Bottom);

    while (Left<Right)  and Game.IsAnEmptyCol(Left)         do Inc(Left);
    while (Top <Bottom) and Game.IsAnEmptyRow(Top)          do Inc(Top);

    W:=Right-Left; H:=Bottom-Top;

    if (Left<Right) and (Top<Bottom) and
       ((Left>1) or (Top>1)) then begin
       DoItem(MakeItem(ehaTranslate,-Pred(Left),-Pred(Top),0,Position));
       Result:=True;
       end;

    Result:=Result or (W<>Game.BoardWidth) or (H<>Game.BoardHeight);
    if Result then begin
       Game.BoardWidth:=W; Game.BoardHeight:=H;
       end;
    end;
end;

function TEditorHistory.CanUndoSession:Boolean;
begin
  Result:=(StartPositionTransactionNumber>=0) and
          (StartPositionTransactionNumber<=FileHeader.TransactionCount) ;
end;

function TEditorHistory.Clear:Boolean;
begin // 'Clear' keeps the open history-file, if any, but it's truncated to zero-length
  Result:=True;
  FillChar(FileHeader,SizeOf(FileHeader),0);
  Transaction.Result:=False; Transaction.StartItemIndex:=MAX_EDITOR_HISTORY_ITEMS;
  ItemIndex:=-1; fStartPositionTransactionNumber:=0;
  if FileName<>'' then
     try    Seek(HistoryFile,0); Truncate(HistoryFile); Result:=IOResult=0;
            if   Result then //
            else raise Exception.Create(TEXT_WRITE_FILE_ERROR);
     except on E:Exception do Result:=Error(E.Message,Application.Title);
     end;
end;

function  TEditorHistory.Close:Boolean;
var oItemIndex,oTransactionNumber:Integer;
begin
  Result:=True;
  if FileName<>'' then begin
     if (Game<>nil) and
        (Game.Name<>'') and
        (Game.BoardWidth <>0) and
        (Game.BoardHeight<>0) then begin
        oItemIndex:=ItemIndex; oTransactionNumber:=FileHeader.TransactionNumber;
        try     ItemIndex:=Pred(ItemCount);
                FileHeader.TransactionNumber:=FileHeader.TransactionCount;
                AddBoard;
        finally ItemIndex:=oItemIndex;
                FileHeader.TransactionNumber:=oTransactionNumber;
        end;
        end;

     Result:=WriteFileHeader;

     if Result then
        try    //Seek(HistoryFile,FilePos(HistoryFile));
               Truncate (HistoryFile); Result:=IOResult=0;
               CloseFile(HistoryFile); Result:=(IOResult=0) and Result;
               if   Result then fFileName:=''
               else raise Exception.Create(TEXT_CLOSE_FILE_ERROR);
        except on E:Exception do Result:=Error(E.Message,Application.Title);
        end;
     end;

  if Result then Clear;
end;

function TEditorHistory.DoItem(const Item__:TEditorHistoryItem):Boolean;
var i,C,R:Integer;

  function DeleteSquare(Col__,Row__:Integer; UpdateBoardBounds__:Boolean):Boolean;
  var i:Integer;
  begin
    Result:=False; i:=Game.Board[Col__,Row__];
    if (i and PLAYER)<>0 then begin
       Game.PlayerPos.X:=0; Game.PlayerPos.Y:=0;
       end;
    if (i and BOX )<>0 then Dec(Game.BoxCount);
    if (i and GOAL)<>0 then Dec(Game.GoalCount);
    Game.Board[Col__,Row__]:=FLOOR;

    if UpdateBoardBounds__
       and
       (
        ((Col__=1               ) and Game.IsAnEmptyCol(1               ))
        or
        ((Row__=1               ) and Game.IsAnEmptyRow(1               ))
        or
        ((Col__=Game.BoardWidth ) and Game.IsAnEmptyCol(Game.BoardWidth ))
        or
        ((Row__=Game.BoardHeight) and Game.IsAnEmptyRow(Game.BoardHeight))
       ) then
       Transaction.IsBoardBoundsOK:=False;
  end;

  procedure RotateFlip(Transformation2D__:TTransformation2D);
  var i,j,Col,Row,W,H:Integer; B:TBoard;
  begin
    B:=Game.Board; W:=Game.BoardWidth; H:=Game.BoardHeight;

    for Col:=1 to W do
        for Row:=1 to H do with Game do begin
            CalculateTransformation2D(Transformation2D__,Col,Row,W,H,i,j);
            Board[i,j]:=B[Col,Row];
            if (Board[i,j] and PLAYER)<>0 then with PlayerPos do begin
               X:=i; Y:=j;
               end;
            end;

   if (Transformation2D__=t2DRotate90DegreesClockwise) or
      (Transformation2D__=t2DRotate270DegreesClockwise) then begin
      Game.BoardWidth:=H; Game.BoardHeight:=W;
      for Row:=1 to MAX_BOARD_HEIGHT do
          for Col:=1 to MAX_BOARD_WIDTH do
             if (Row>Game.BoardHeight) or
                (Col>Game.BoardWidth) then
                Game.Board[Col,Row]:=FLOOR; // clean up
      end;
  end;

begin
  with Item__ do
    if ((Col<1)
        and
        ((Action<>ehaTranslate)
         or
         (Col<-MAX_BOARD_WIDTH)
        )
       )
       or
       (Col>MAX_BOARD_WIDTH)
       or
       ((Row<1)
        and
        ((Action<>ehaTranslate)
         or
         (Row<-MAX_BOARD_HEIGHT)
        )
       )
       or
       (Row>MAX_BOARD_HEIGHT) then
       Result:=False
    else begin
       Result:=True;
       if   Action<>ehaTranslate then i:=Game.Board[Col,Row]
       else i:=0;
       case Action of
         ehaBoard     :;
         ehaBox       : begin
                          if (i and PLAYER)<>0 then begin
                             Game.PlayerPos.X:=0; Game.PlayerPos.Y:=0;
                             Dec(i,PLAYER);
                             end;
                          if (i and BOX) = 0 then Inc(Game.BoxCount);
                          if (i and WALL)<>0 then i:=FLOOR;
                          Game.Board[Col,Row]:=i or BOX;
                          Game.BoardWidth :=Max(Game.BoardWidth ,Col);
                          Game.Boardheight:=Max(Game.BoardHeight,Row);
                        end;
         ehaErase     : DeleteSquare(Col,Row,True);
         ehaGoal      : begin
                          if (i and GOAL)= 0 then Inc(Game.GoalCount);
                          if (i and WALL)<>0 then i:=FLOOR;
                          Game.Board[Col,Row]:=i or GOAL;
                          Game.BoardWidth :=Max(Game.BoardWidth ,Col);
                          Game.Boardheight:=Max(Game.BoardHeight,Row);
                        end;
         ehaHeight    : if   Row>=0{MIN_BOARD_HEIGHT} then Game.BoardHeight:=Row
                        else Result:=False;
         ehaPlayer    : begin
                          with Game.PlayerPos do
                            Game.Board[X,Y]:=Game.Board[X,Y] and (not PLAYER);
                          Game.PlayerPos.X:=Col; Game.PlayerPos.Y:=Row;
                          if (i and BOX )<>0 then Dec(Game.BoxCount);
                          if (i and WALL)<>0 then i:=FLOOR;
                          Game.Board[Col,Row]:=(i and (not BOX)) or PLAYER;
                          Game.BoardWidth :=Max(Game.BoardWidth ,Col);
                          Game.Boardheight:=Max(Game.BoardHeight,Row);
                        end;
         ehaMirror    : if   (Ord(Value)=Ord(t2DFlipVertically)) or
                             (Ord(Value)=Ord(t2DFlipHorizontally)) then
                             RotateFlip(TTransformation2D(Value))
                        else Result:=False;
         ehaRotate    : if   (Ord(Value)>=Ord(t2DRotate0DegreesClockwise)) and
                             (Ord(Value)<=Ord(t2DRotate270DegreesClockwise)) then
                             RotateFlip(TTransformation2D(Value))
                        else Result:=False;
         ehaSquare    : begin
                          DeleteSquare(Col,Row,False);
                          if (Value and WALL)<>0 then i:=WALL
                          else if (Value and BOX)<>0 then begin
                                  Inc(Game.BoxCount); i:=FLOOR+BOX;
                                  if (Value and GOAL)<>0 then begin
                                     Inc(Game.GoalCount); Inc(i,GOAL);
                                     end;
                                  end
                               else begin
                                  i:=FLOOR;
                                  if (Value and PLAYER)<>0 then with Game.PlayerPos do begin
                                     Game.Board[X,Y]:=Game.Board[X,Y] and (not PLAYER);
                                     X:=Col; Y:=Row;
                                     Inc(i,PLAYER);
                                     end;
                                  if (Value and GOAL  )<>0 then begin
                                     Inc(Game.GoalCount); Inc(i,GOAL);
                                     end;
                                  end;
                          Game.Board[Col,Row]:=i;
                          if (i and (WALL+BOX+GOAL+PLAYER))<>0 then begin
                             Game.BoardWidth :=Max(Game.BoardWidth ,Col);
                             Game.Boardheight:=Max(Game.BoardHeight,Row);
                             end
                          else DeleteSquare(Col,Row,True);
                        end;
         ehaTranslate : begin
                          if Col>0 then begin
                             for R:=1 to MAX_BOARD_HEIGHT do begin
                                 for C:=MAX_BOARD_WIDTH downto Succ(Col) do
                                     Game.Board[C,R]:=Game.Board[C-Col,R];
                                 for i:=1 to Col do Game.Board[i,R]:=FLOOR;
                                 end;
                             with Game.PlayerPos do
                               if X<>0 then
                                  if X+Col<=MAX_BOARD_WIDTH then Inc(X,Col)
                                  else begin X:=0; Y:=0; end;
                             end;
                          if Col<0 then begin
                             for R:=1 to MAX_BOARD_HEIGHT do begin
                                 for C:=1 to MAX_BOARD_WIDTH+Col do
                                     Game.Board[C,R]:=Game.Board[C-Col,R];
                                 for i:=Succ(MAX_BOARD_WIDTH+Col) to MAX_BOARD_WIDTH do Game.Board[i,R]:=FLOOR;
                                 end;
                             with Game.PlayerPos do
                               if X<>0 then
                                  if X+Col>0 then Inc(X,Col)
                                  else begin X:=0; Y:=0; end;
                             end;
                          if Row>0 then begin
                             for C:=1 to MAX_BOARD_WIDTH do begin
                                 for R:=MAX_BOARD_HEIGHT downto Succ(Row) do
                                     Game.Board[C,R]:=Game.Board[C,R-Row];
                                 for i:=1 to Row do Game.Board[C,i]:=FLOOR;
                                 end;
                             with Game.PlayerPos do
                               if Y<>0 then
                                  if Y+Row<=MAX_BOARD_HEIGHT then Inc(Y,Row)
                                  else begin X:=0; Y:=0; end;
                             end;
                          if Row<0 then begin
                             for C:=1 to MAX_BOARD_WIDTH do begin
                                 for R:=1 to MAX_BOARD_HEIGHT+Row do
                                     Game.Board[C,R]:=Game.Board[C,R-Row];
                                 for i:=Succ(MAX_BOARD_HEIGHT+Row) to MAX_BOARD_HEIGHT do Game.Board[C,i]:=FLOOR;
                                 end;
                             with Game.PlayerPos do
                               if Y<>0 then
                                  if Y+Row>0 then Inc(Y,Row)
                                  else begin X:=0; Y:=0; end;
                             end;
                          Game.BoardWidth :=Max(0,Min(MAX_BOARD_WIDTH ,Game.BoardWidth +Col));
                          Game.BoardHeight:=Max(0,Min(MAX_BOARD_HEIGHT,Game.BoardHeight+Row));
                          if Assigned(CalculateGameInformation) then
                             CalculateGameInformation(Game);
                        end;
         ehaWall      : begin
                          DeleteSquare(Col,Row,False);
                          Game.Board[Col,Row]:=WALL;
                          Game.BoardWidth :=Max(Game.BoardWidth ,Col);
                          Game.Boardheight:=Max(Game.BoardHeight,Row);
                        end;
         ehaWidth     : if   Col>=0{MIN_BOARD_WIDTH} then Game.BoardWidth:=Col
                        else Result:=False;
         else           Result:=False;
       end; // case
       end;
end;

function  TEditorHistory.EndTransaction(Commit__:Boolean):Boolean;
begin
  Result:=True;
  try
    if ItemIndex>Transaction.StartItemIndex then // 'True': some items have been added to the history
       if Transaction.Result and Commit__ then with FileHeader do begin // everything went fine and the user wants to save it
          Inc(TransactionNumber);
          TransActionCount:=Max(TransactionCount,TransactionNumber);
          end
       else // the transaction failed or the user wants to drop it
          try     try    Transaction.IsBoardBoundsOK:=True;
                         while (ItemIndex>Transaction.StartItemIndex) and
                               UndoItem(Items[ItemIndex]) do
                               Dec(FileHeader.ItemIndex);
                         if not Transaction.IsBoardBoundsOK then CalculateBoardBounds;
                  except on E:Exception do begin
                         Clear; // something went terribly wrong; clear the history
                         Result:=Error(E.Message,Application.Title);
                         end;
                  end;
          finally ItemIndex:=Min(Transaction.StartItemIndex,Pred(ItemCount));
                  FileHeader.ItemCount:=Succ(ItemIndex);
                  FileHeader.TransactionCount:=FileHeader.TransactionNumber;
                  if Assigned(Refresh) then Refresh(Self);
          end;
  finally
    Transaction.Result:=False; Transaction.StartItemIndex:=MAX_EDITOR_HISTORY_ITEMS;
  end;
end;

function TEditorHistory.GetItem(Index__:Integer):TEditorHistoryItem;
var i:Integer;
begin
  if FileName<>'' then
     if (Index__>=0) and (Index__<ItemCount) then begin
        Seek(HistoryFile,Index__);
        Read(HistoryFile,i);
        if IOResult=0 then with Result do begin
           Parity:=(i shr 24) and $80;
           Col   :=(i shr 16) and $ff; if Col>MAX_BOARD_WIDTH  then Col:=MAX_BOARD_WIDTH -Col;
           Row   :=(i shr  8) and $ff; if Row>MAX_BOARD_HEIGHT then Row:=MAX_BOARD_HEIGHT-Row;
           Value :=(i and $ff);

           i     :=(i shr 24) and $7f;
           if   (i>=Ord(Low(Action))) and (i<=Ord(High(Action))) then
                Action:=TEditorHistoryAction(i)
           else raise Exception.Create(TEXT_INVALID_DATA);
           end
        else raise Exception.Create(TEXT_READ_FILE_ERROR);
        end
     else raise Exception.Create(TEXT_RANGE_ERROR)
  else raise Exception.Create(TEXT_NO_FILE_SPECIFIED);
end;

function TEditorHistory.IsATransactionInProgress:Boolean;
begin
  Result:=Transaction.StartItemIndex<>MAX_EDITOR_HISTORY_ITEMS;
end;

function TEditorHistory.LoadBoard(Index__:Integer; var BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Boolean;
var Col,Row:Integer; Item:TEditorHistoryItem;
begin
  BoardWidth__:=0; BoardHeight__:=0; ClearBoard(Board__);
  try
    Result:=True;
    if FileName<>'' then begin
       if   (Index__>=0) and (Index__<ItemCount) then begin
            Item:=Items[Index__];
            if (Item.Action=ehaBoard) and
               //(Item.Col>=MIN_BOARD_HEIGHT) and // allow boards with too few columns/rows in the editor
               (Item.Col<=MAX_BOARD_HEIGHT) and
               //(Item.Row>=MIN_BOARD_HEIGHT) and
               (Item.Row<=MAX_BOARD_HEIGHT) and
               (Item.Parity=0) then begin
               BoardWidth__:=Item.Col; BoardHeight__:=Item.Row;
               for Col:=1 to Game.BoardWidth do
                   for Row:=1 to Game.BoardHeight do begin
                       Inc(Index__);
                       Item:=Items[Index__];
                       if (Item.Action=ehaSquare) and
                          (Item.Col=Col) and
                          (Item.Row=Row) and
                          (Item.Parity=0) then
                          Board__[Item.Col,Item.Row]:=Item.Value and BOARD_PIECES
                       else
                          raise Exception.Create(TEXT_INVALID_DATA);
                       end;
               end
            else raise Exception.Create(TEXT_INVALID_DATA);
            end
       else raise Exception.Create(TEXT_RANGE_ERROR);
       end
    else raise Exception.Create(TEXT_NO_FILE_SPECIFIED);
  except on E:Exception do Result:=Error(E.Message,Application.Title);
  end;
end;

function  TEditorHistory.Log:Boolean;
var i:Integer; TextFile:System.TextFile;
begin
  Result:=True;
  try
    Assign(TextFile,MainForm.ApplicationDataPath+ExtractFileName(ChangeFileExt(Application.ExeName,LOG_FILE_EXT)));
    Rewrite(TextFile);
    try     if ItemIndex<0 then Writeln(TextFile,'> ');
            for i:=0 to Pred(ItemCount) do with Items[i] do begin
                if i=ItemIndex then Write(TextFile,'> ')
                else Write(TextFile,'  ');
                Write(TextFile,i:16,EDITOR_HISTORY_ACTION_NAMES[Action]:16,Col:4,Row:4,Value:8,Parity:4);
                Writeln(TextFile);
                end;
    finally CloseFile(TextFile);
            if IOResult<>0 then raise Exception.Create(TEXT_WRITE_FILE_ERROR);
    end;
  except on E:Exception do Result:=Error(E.Message,Application.Title);
  end;
end;

function TEditorHistory.MakeFileName:String;
begin
  try    if        fLastFileName<>'' then
                   Result:=fLastFileName // only one filename is used during the entire session
         else if   Main_.IsFirstInstance then
                   Result:=MainForm.ApplicationDataPath+ExtractFileName(ChangeFileExt(Application.ExeName,EDITOR_LOG_FILE_EXT))
              else Result:=MakeUniqueFileName(MainForm.ApplicationDataPath,ExtractFileNameWithoutExt(Application.ExeName)+TEMPORARY_FILE_EXT,EDITOR_LOG_FILE_EXT); // create a name like '...\Sokoban.$$$.sed'
  except on E:Exception do begin
            Error(E.Message,Application.Title);
            Result:='';
            end;
  end;
end;

function TEditorHistory.MakeItem(Action__:TEditorHistoryAction; Col__,Row__,Value__,Parity__:Integer):TEditorHistoryItem;
begin
  with Result do begin
    Action:=Action__;
    Col:=Col__;
    Parity:=Parity__;
    Row:=Row__;
    Value:=Value__;
    end;
end;

function TEditorHistory.New(const FileName__:String):Boolean;
begin
  Result:=Close and (FileName__<>'');
  try    if Result then begin
            AssignFile(HistoryFile,FileName__); Rewrite(HistoryFile);
            Result:=IOResult=0;
            if   Result then begin
                 fFileName:=FileName__;
                 fLastFileName:=fFileName;
                 end
            else raise Exception.Create(TEXT_CREATE_FILE_ERROR);
            end;
  except on E:Exception do begin
         Clear;
         Result:=Error(E.Message,Application.Title);
         end;
  end;
end;

function TEditorHistory.Open(const FileName__:String; var BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Boolean;
var i,j,C,R:Integer; Item:TEditorHistoryItem;
begin
  Result:=False; BoardWidth__:=-1; BoardHeight__:=-1;
  if Close then
     try
       AssignFile(HistoryFile,FileName__); Reset(HistoryFile);
       if   IOResult=0 then
            try     fFileName:=FileName__; fLastFileName:=fFileName;
                    if (System.FileSize(HistoryFile)=0) or
                       ReadFileHeader then begin
                       fStartPositionTransactionNumber:=FileHeader.TransactionNumber;
                       i:=System.FileSize(HistoryFile)-EDITOR_HISTORY_FILE_HEADER_ITEM_SIZE-1;
                       if i>=0 then begin
                          Item:=Items[i];
                          if Item.Action=ehaBoard then with Item do begin
                             BoardWidth__:=Col; BoardHeight__:=Row;
                             Dec(i,Succ(Col*Row));
                             if (i>=0) and
                                (BoardWidth__>=1) and (BoardWidth__<=MAX_BOARD_WIDTH) and
                                (BoardHeight__>=1) and (BoardHeight__<=MAX_BOARD_HEIGHT) then begin
                                Item:=Items[i];
                                if (Item.Action=ehaBoard) and
                                   (Item.Col=BoardWidth__) and
                                   (Item.Row=BoardHeight__) then begin
                                   ClearBoard(Board__); j:=i;
                                   for C:=1 to BoardWidth__ do
                                       for R:=1 to BoardHeight__ do begin
                                           Inc(j);
                                           with Items[j] do
                                             if   (Action=ehaSquare) and
                                                  (Col=C) and (Row=R) then
                                                  Board__[Col,Row]:=Value
                                             else raise Exception.Create(TEXT_INVALID_DATA);
                                           end;
                                   FileHeader.ItemCount:=i; ItemIndex:=Min(ItemIndex,Pred(ItemCount));
                                   Dec(FileHeader.TransactionCount);
                                   with FileHeader do TransactionNumber:=Min(TransactionNumber,TransactionCount);
                                   Seek(HistoryFile,i); Truncate(HistoryFile);
                                   if IOResult<>0 then raise Exception.Create(TEXT_WRITE_FILE_ERROR);
                                   Result:=True;
                                   end
                                else raise Exception.Create(TEXT_INVALID_DATA);
                                end
                             else raise Exception.Create(TEXT_INVALID_DATA);
                             end;
                          end
                       else Result:=True;
                       end;
            finally if not Result then begin
                       fFileName:=''; Clear;
                       BoardWidth__:=-1; BoardHeight__:=-1;
                       CloseFile(HistoryFile); if IOResult<>0 then;
                       DeleteFile(fLastFileName); if IOResult<>0 then;
                       fLastFileName:='';
                       end;
            end
       else raise Exception.Create(TEXT_OPEN_FILE_ERROR);
     except on E:Exception do begin
            Result:=Error(E.Message,Application.Title);
            end;
     end;
end;

function  TEditorHistory.ReadFileHeader:Boolean;
begin
  try
    if FileName<>'' then
       if System.FileSize(HistoryFile)>=EDITOR_HISTORY_FILE_HEADER_ITEM_SIZE then begin
          Seek(HistoryFile,System.FileSize(HistoryFile)-EDITOR_HISTORY_FILE_HEADER_ITEM_SIZE);
          Read(HistoryFile,FileHeader.ItemCount);
          Read(HistoryFile,FileHeader.ItemIndex);
          Read(HistoryFile,FileHeader.MagicID1);
          Read(HistoryFile,FileHeader.MagicID2);
          Read(HistoryFile,FileHeader.TransactionCount);
          Read(HistoryFile,FileHeader.TransactionNumber);

          if   IOResult=0 then
               if   (FileHeader.MagicID1=EDITOR_HISTORY_MAGIC_ID_1) and
                    (FileHeader.MagicID2=EDITOR_HISTORY_MAGIC_ID_2) and
                    (FileHeader.ItemCount>=0) and
                    (FileHeader.ItemCount=System.FileSize(HistoryFile)-EDITOR_HISTORY_FILE_HEADER_ITEM_SIZE) and
                    (FileHeader.ItemIndex>=-1) and
                    (FileHeader.ItemIndex<FileHeader.ItemCount) and
                    (FileHeader.TransactionCount>=0) and
                    (FileHeader.TransactionCount<=FileHeader.ItemCount) and
                    (FileHeader.TransactionNumber>=0) and
                    (FileHeader.TransactionNumber<=FileHeader.ItemCount) then
                    Result:=True
               else raise Exception.Create(Format(NotAnEditorHistoryFileText__,[FileName]))
          else raise Exception.Create(TEXT_READ_FILE_ERROR);
          end
       else raise Exception.Create(Format(NotAnEditorHistoryFileText__,[FileName]))
    else
       raise Exception.Create(TEXT_NO_FILE_SPECIFIED);
  except on E:Exception do Result:=Error(E.Message,Application.Title);
  end;
end;

procedure TEditorHistory.SetItemIndex(ItemIndex__:Integer);
begin
  if   (ItemIndex__>=-1) and (ItemIndex__<ItemCount) then
       FileHeader.ItemIndex:=ItemIndex__
  else raise Exception.Create(TEXT_RANGE_ERROR);
end;

procedure TEditorHistory.SetItem(Index__:Integer; const Item__:TEditorHistoryItem);
var i,j,C,R:Integer;
begin
  try
    if FileName<>'' then
       if Index__>=0 then
          if (Index__<MAX_EDITOR_HISTORY_ITEMS) then with Item__ do begin
             if Col>=0  then C:=Col
             else C:=MAX_BOARD_WIDTH-Col;
             if Row>=0  then R:=Row
             else R:=MAX_BOARD_HEIGHT-Row;
             i:=(Ord(Action) shl  24) + ((C and $ff) shl 16) + ((R and $ff) shl 8) + (Value and $ff);
             if (Parity and 1)<>0 then Inc(i,$80 shl 24);

             if Index__<ItemCount then begin // if 'True' then 'Item__' will overwrite an existing item in the history
                Seek(HistoryFile,Index__);
                Read(HistoryFile,j);
                if   IOResult=0 then begin
                     if i<>j then begin // 'i<>j': the old items after the current item aren't legal anymore
                        FileHeader.ItemCount       :=Index__;
                        FileHeader.TransactionCount:=FileHeader.TransactionNumber;
                        if FileHeader.TransactionNumber<StartPositionTransactionNumber then
                           fStartPositionTransactionNumber:=-1; // the start position cannot be restored anymore if the user cancels editing
                        end;
                     end
                else raise Exception.Create(TEXT_READ_FILE_ERROR);
                end;

             Seek(HistoryFile,Index__);
             Write(HistoryFile,i);
             if   IOResult=0 then
                  if Index__>=ItemCount then FileHeader.ItemCount:=Succ(Index__)
                  else
             else raise Exception.Create(TEXT_WRITE_FILE_ERROR);
             end
          else raise Exception.Create(Format(TEXT_FILE_TOO_LARGE_FORMAT,[FileName]))
       else raise Exception.Create(TEXT_RANGE_ERROR)
    else raise Exception.Create(TEXT_NO_FILE_SPECIFIED);
  except on E:Exception do begin
         Transaction.Result:=False;
         raise;
         end;
  end;
end;

procedure TEditorHistory.SetPosition(Position__:Integer);
var oCalculateGameInformation:TCalculateGameInformation; oCursor:TCursor;

  function RedoTransaction:Boolean;
  var Parity:Integer; Item:TEditorHistoryItem;
  begin // 'ItemIndex' points to the last item in the preceding transaction
    Result:=(ItemIndex>=-1) and (ItemIndex<Pred(ItemCount));
    if Result then
       try    Transaction.IsBoardBoundsOK:=True;
              Inc(FileHeader.ItemIndex); Item:=Items[ItemIndex]; Parity:=Item.Parity;
              repeat
                DoItem(Item);
                Inc(FileHeader.ItemIndex);
                if ItemIndex<ItemCount then Item:=Items[ItemIndex];
              until (ItemIndex=ItemCount) or (Item.Parity<>Parity);
              Dec(FileHeader.ItemIndex); // point to the last item in the transaction
              Inc(FileHeader.TransactionNumber);
              if not Transaction.IsBoardBoundsOK then CalculateBoardBounds;
       except on E:Exception do Result:=Error(E.Message,Application.Title);
       end;
  end;

  function UndoTransaction:Boolean;
  var Parity:Integer; Item:TEditorHistoryItem;
  begin // 'ItemIndex' points to the last item in the transaction
    Result:=(ItemIndex>=0) and (ItemIndex<ItemCount);
    if Result then
       try    Transaction.IsBoardBoundsOK:=True;
              Item:=Items[ItemIndex]; Parity:=Item.Parity;

              while (ItemIndex>=0) and (Item.Parity=Parity) do begin
                UndoItem(Item);
                Dec(FileHeader.ItemIndex);
                if ItemIndex>=0 then Item:=Items[ItemIndex];
                end;

              Dec(FileHeader.TransactionNumber);
              if not Transaction.IsBoardBoundsOK then CalculateBoardBounds;
       except on E:Exception do Result:=Error(E.Message,Application.Title);
       end;
  end;

begin // SetPosition
  if Position__<>Position then
     if (Position__>=0) and (Position__<=Count) then with FileHeader do begin
        if not IsATransactionInProgress then begin
           oCalculateGameInformation:=CalculateGameInformation;
           oCursor:=Screen.Cursor;
           try     Screen.Cursor:=crHourGlass;
                   CalculateGameInformation:=nil;
                   while (Position__>Position) and RedoTransaction do;
                   while (Position__<Position) and UndoTransaction do;
           finally Screen.Cursor:=oCursor;
                   CalculateGameInformation:=oCalculateGameInformation;
                   if Assigned(CalculateGameInformation) then
                      CalculateGameInformation(Game);
                   if Position__=Position then begin
                      if Assigned(Refresh) then Refresh(Self);
                      end
                   else begin
                      Clear;
                      if Assigned(Refresh) then Refresh(Self);
                      raise Exception.Create(UndoRedoFailedText);
                      end;
           end;
           end
        else raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,['Tools_.TEditorHistory.SetPosition: Undo/redo prohibited while adding a new transaction']));
        end
     else
        raise Exception.Create(TEXT_RANGE_ERROR);
end;

function TEditorHistory.UndoItem(Item__:TEditorHistoryItem):Boolean;
begin
  with Item__ do
    case Action of
      ehaBoard     : Result:=True;
      ehaBox       : begin Item__.Action:=ehaSquare; Result:=DoItem(Item__);
                     end;
      ehaErase     : begin Item__.Action:=ehaSquare; Result:=DoItem(Item__);
                     end;
      ehaGoal      : begin Item__.Action:=ehaSquare; Result:=DoItem(Item__);
                     end;
      ehaHeight    : begin Item__.Row:=Item__.Value; Result:=DoItem(Item__);
                     end;
      ehaPlayer    : begin Item__.Action:=ehaSquare; Result:=DoItem(Item__);
                     end;
      ehaMirror    : if   (Ord(Item__.Value)=Ord(t2DFlipVertically)) or
                          (Ord(Item__.Value)=Ord(t2DFlipHorizontally  )) then
                          Result:=DoItem(Item__)
                     else Result:=False;
      ehaRotate    : if   (Ord(Item__.Value)>=Ord(Low (TTransformation2D))) and
                          (Ord(Item__.Value)<=Ord(High(TTransformation2D))) then begin
                          Item__.Value:=Ord(BOARD_TRANSFORMATION_INVERSE[TTransformation2D(Item__.Value)]);
                          Result:=DoItem(Item__);
                          end
                     else Result:=False;
      ehaSquare    : begin Item__.Action:=ehaErase;  Result:=DoItem(Item__);
                     end;
      ehaTranslate : begin Item__.Col:=-Item__.Col; Item__.Row:=-Item__.Row;
                           Result:=DoItem(Item__);
                     end;
      ehaWall      : begin Item__.Action:=ehaSquare; Result:=DoItem(Item__);
                     end;
      ehaWidth     : begin Item__.Col:=Item__.Value; Result:=DoItem(Item__);
                     end;
      else           Result:=False;
    end;
end;

function TEditorHistory.UndoSession:Boolean;
begin
  Result:=CanUndoSession;
  if Result then
     try    Position:=StartPositionTransactionNumber;
     except on E:Exception do Result:=Error(E.Message,Application.Title);
     end;
end;

function  TEditorHistory.WriteFileHeader:Boolean;
var i:Integer;
begin
  try
    if FileName<>'' then
       if ItemCount<=MAX_EDITOR_HISTORY_ITEMS then begin
          Seek(HistoryFile,ItemCount);
          Write(HistoryFile,FileHeader.ItemCount);
          Write(HistoryFile,FileHeader.ItemIndex);
          i:=EDITOR_HISTORY_MAGIC_ID_1; Write(HistoryFile,i);
          i:=EDITOR_HISTORY_MAGIC_ID_2; Write(HistoryFile,i);
          Write(HistoryFile,FileHeader.TransactionCount);
          Write(HistoryFile,FileHeader.TransactionNumber);
          if   IOResult=0 then Result:=True
          else raise Exception.Create(TEXT_WRITE_FILE_ERROR);
          end
       else raise Exception.Create(EditorHistoryFullText)
    else
       raise Exception.Create(TEXT_NO_FILE_SPECIFIED);
  except on E:Exception do Result:=Error(E.Message,Application.Title);
  end;
end;

{$I+}

function TStringsDB.Add0(const S__: String; CaseSensitive__: Boolean): Integer;
begin
  try    if   CaseSensitive__ then
              Result:=IndexOf(S__)
         else Result:=IndexOfCI(S__);
         if Result>=0 then
            AdjustReferenceCount(Result,1)
         else begin
            if   FreeList>0 then begin // 'True': recycle the empty slot
                 Result:=FreeList;
                 FreeList:=Integer(Objects[Result]);
                 Strings[Result]:=S__;
                 end
            else Result:=Inherited Add(S__);
            Objects[Result]:=TObject(1);
            end;
  except on E:Exception do begin
            SokUtil_.Error(E.Message,Application.Title);
            Result:=-1;
            end;
  end;
end;

function TStringsDB.Add(const S__: String): Integer; // override;
begin // add item with case-sensitivity
  Result:=Add0(S__,True);
end;

function TStringsDB.AddCI(const S__: String): Integer;
begin // add item without case-sensitivity
  Result:=Add0(S__,False);
end;

procedure TStringsDB.AdjustReferenceCount(Index__,Adjustment__:Integer);
var i:Integer;
begin
  i:=Integer(Objects[Index__]);
  if i<>High(i) then begin // adjust the reference count unless it has reached the overflow value
     if   High(i)-Adjustment__>=i then Inc(i,Adjustment__)
     else i:=High(i);
     Objects[Index__]:=TObject(i);
     end;
end;

procedure TStringsDB.Clear; // override;
var i:Integer;
begin
  Inherited;
  FreeList:=0; // note that '0' is used as sentinel for the free list, not '-1'; this is possible because Strings[0] is protected against recycling; see below
  i:=Add(''); // add an empty string as the first item
  if i=0 then Objects[0]:=TObject(High(i)) // keep the empty string in the table at all times by setting its reference count to the overflow value
  else raise Exception.Create(TEXT_MEMORY_FULL);
end;

constructor TStringsDB.Create;
begin
  Inherited;
  Sorted:=False;
  Clear;
end;

procedure TStringsDB.Delete(Index__: Integer); // override;
var i:Integer;
begin
  if (Index__>=0) and (Index__<Count) then begin
     i:=Integer(Objects[Index__]);
     if i<High(i) then Dec(i); // decrease the reference count unless it has reached the overflow value
     if i<>0 then Objects[Index__]:=TObject(i)
     else begin // no more references to this item, hence, recycle it
        Strings[Index__]:='';
        Objects[Index__]:=TObject(FreeList);
        FreeList:=Index__;
        end
     end;
end;

function TStringsDB.IndexOf(const S__: String): Integer; // override
begin // in contrast to 'TStringList', 'TStringsDB' compares items using case-sensitivity
  for Result:=0 to Pred(Count) do
      if Strings[Result]=S__ then Exit;
  Result:=-1;
end;

function TStringsDB.IndexOfCI(const S__: String): Integer;
begin // look-up without case-sensitivity
  for Result:=0 to Pred(Count) do
      if StrEqual(S__,Strings[Result]) then Exit;
  Result:=-1;
end;

procedure TToolsForm.PageControl1Change(Sender: TObject);
var HasLockedSolver,HasLockedOptimizer,HasLockedGenerator:Boolean;

  procedure InitializePluginTabSheet(Plugin__:TPlugin);
  var i,Index:Integer; b:Boolean;
      pcbii:TPluginCallBackInfoItem; gcbii:TGeneratorCallBackInfoItem;
      s,OldPluginFileName:String; oCursor:TCursor; //TimeMS:TTimeMS;
  begin
    if (PluginLevelInfo.ReplayInfo.MovesAsText='') then
       for i:=0 to Pred(PluginLevelStringGrid.RowCount) do PluginLevelStringGrid.Cells[1,i]:='';
    PluginToolButtonOpen.Hint:=PluginMenuItemOpen.Hint;

    if Assigned(Plugin__) then with Plugin__ do begin

       BtnSolveLevel       .Visible :=Plugin__ is TSolverPlugin;
       BtnOptimizeGames    .Visible :=Plugin__ is TOptimizerPlugin;
       BtnGenerateLevels   .Visible :=Plugin__ is TGenerator;
       if BtnSolveLevel    .Visible then PluginLevelGroupBox.Caption:=SolveLevelText;
       if BtnOptimizeGames .Visible then PluginLevelGroupBox.Caption:=OptimizeSolutionText;
       if BtnGenerateLevels.Visible then PluginLevelGroupBox.Caption:=GenerateCandidateText;
       PluginToolBar.Parent:=PageControl1.ActivePage;
       PluginLevelGroupBox.Parent:=PageControl1.ActivePage;
       LevelGroupBox.Parent:=PageControl1.ActivePage;

       ShowTitle('',Editor.FileName);

       if Game.IsReplaying then
          Inc(FormResizeCount); // force an update so the new game shows up on the screen when the replay-loop exits
       LoadLevelFromEditor(True,True{(PageControl1.Tag<>Integer(TabSheetSolver)) and (PageControl1.Tag<>Integer(TabSheetOptimizer))});
       s:=DefaultPluginFileName;
       if Plugin__.IndexOf(s)<0 then Plugin__.Add(s);
       if Plugin__.IndexOf(PluginFileName)<0 then Plugin__.Add(PluginFileName);
       OldPluginFileName:=PluginFileName;

       if Assigned(ComboBox) then with ComboBox do with Items do begin
          if not Sorted then begin // 'True': the plugin tabsheet is opened for the first time; load all the registered plugins to verify that they really are plugins; additionally, get their names which may differ from the filenames
             //TimeMS:=GetTimeMS;
             oCursor:=Screen.Cursor;
             try
               Screen.Cursor:=crHourGlass;
               for Index:=Pred(Count) downto 0 do begin
                   s:=ExpandFileName(Items[Index]);
                   if Plugin__.Open(s) then begin
                      i:=StringsDB.AddCI(s);
                      if i>0 then begin
                         Objects[Index]:=TObject(i); // use 'Objects[]' to store references to the plugin filenames
                         s:=Plugin__.PluginName;
                         if s='' then s:=ExtractFileNameWithoutPathAndExtension(StringsDB.Strings[i]);
                         //else s:=s+'  ['+ExtractFileName(StringsDB.Strings[i])+']';
                         Items  [Index]:=s;
                         SetComboBoxDropDownWidth(ComboBox,Index,False);
                         end
                      else Delete(Index);
                      end
                   else Delete(Index);
                   end;
             finally
               Screen.Cursor:=oCursor;
               ComboBox.Sorted:=True;
               //TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS);
               //SokUtil_.Msg(IntToStr(TimeMS),'Time',MB_OK);
             end;
             end;
          ItemIndex:=Plugin__.IndexOf(OldPluginFileName);
          if (ItemIndex<0) and (Count>0) then ItemIndex:=0;
          if ItemIndex>=0 then OpenPluginFromList(Plugin__,ItemIndex);
          if Count     =0 then OpenPluginFromList(Plugin__,-1);
          with ComboBox do if Tag>Width then Perform(CB_SETDROPPEDWIDTH,Tag,0);
          end;

       if Assigned(OpenForm) then begin

          if      PageControl1.ActivePage=TabSheetSolver then with SolverTaskQueue do begin
                  LevelGroupBox.Caption:=TEXT_LEVEL;
                  for pcbii:=Low(PluginCallBackInfoText) to High(PluginCallBackInfoText) do
                      PluginLevelStringGrid.Cells[0,Ord(pcbii)]:=PluginCallBackInfoText[pcbii];

                  Reload(False);

                  if SolveLevelsGroupBox.Visible then begin
                     SolverTaskQueue.Refresh(True);
                     SolveLevelsStringGridSelectCell(nil,0,StringGrid.Row,b);
                     if SolveLevelsGroupBox.Visible then ActiveControl:=StringGrid;
                     end;
                  ScrollInView(StringGrid.Row);
                  if ActiveControl<>SolveLevelsStringGrid then
                     if   BtnSolveLevel.Enabled then ActiveControl:=BtnSolveLevel
                     else ActiveControl:=PluginLevelFileNamePanel;
                  PluginEditMenuItemCopySolution.Hint:=HintCopySolutionToClipboard;
                  PopupMenuItemCopySolution.Hint:=PluginEditMenuItemCopySolution.Hint;
                  PopupMenuItemPaste.Hint:=PluginEditMenuItemPaste.Hint;

                  PluginMenuItemAdd.Hint:=Format(HintAddLevelsToTaskQueueText__,[MAX_ITEMS_ON_PLUGIN_TASK_QUEUE])+CombineWith_CTRL_ToSearchForUnsolvedLevelsText;
                  //PluginMenuItemAdd.Hint:=HintAddLevelsToTaskQueueText+CombineWith_CTRL_ToSearchForUnsolvedLevelsText;
                  PluginToolButtonAdd.Hint:=PluginMenuItemAdd.Hint;
                  PopupMenuItemSort.Hint:=HintSortLevelsOnListText;
                  PluginMenuItemSortOnOptimization.Visible:=False;
                  PopupMenuItemDeleteLevels.Visible:=True;
                  PluginMenuItemOpenPrior.Hint:=HintOpenPreviousLevelText+CombineWith_CTRL_ToSearchForUnsolvedLevelsText;
                  PluginMenuItemOpenNext .Hint:=HintOpenNextLevelText    +CombineWith_CTRL_ToSearchForUnsolvedLevelsText;
                  PluginEditMenuItemDeleteLevels.Caption:=RemoveLevelsFromListText;
                  PluginEditMenuItemDeleteLevels.Hint:=HintRemoveSelectedLevelsFromListText;
                  PluginToolButtonDelete.Caption:=PluginEditMenuItemDeleteLevels.Caption;
                  PluginToolButtonDelete.Hint:=PluginEditMenuItemDeleteLevels.Hint;
                  PopupMenuItemDeleteLevels.Caption:=PluginEditMenuItemDeleteLevels.Caption;
                  PopupMenuItemDeleteLevels.Hint:=PluginEditMenuItemDeleteLevels.Hint;
                  PluginToolButtonSave.Hint:=PluginMenuItemSave.Hint;
                  PluginToolButtonSaveAs.Hint:=PluginMenuItemSaveAs.Hint;
                  PluginToolButtonCut.Hint:='';
                  PluginToolButtonCopy.Hint:=PluginEditMenuItemCopy.Hint;
                  PluginToolButtonPaste.Hint:=PluginEditMenuItemPaste.Hint;
                  end
          else if PageControl1.ActivePage=TabSheetOptimizer then with OptimizerTaskQueue do begin
                  LevelGroupBox.Caption:=TEXT_LEVEL;
                  for pcbii:=Low(PluginCallBackInfoText) to High(PluginCallBackInfoText) do
                      PluginLevelStringGrid.Cells[0,Ord(pcbii)]:=PluginCallBackInfoText[pcbii];
                  // first try to update the currently loaded level with any
                  // newly found solutions; that way, fewer solutions show
                  // up with a wrong 'Best Solution' name in the optimizer
                  // window; it is, however, far from being perfect, but it's
                  // better than nothing;

                  // (it's disabled because it feels too complicated to have
                  //  a partially updated level here in the 'Tools' window
                  // where the logic is based upon that solutions only are
                  // imported when the window closes, or when the user loads
                  // another level)

                  //SolverTaskQueue.ImportSolutionsForCurrentLevel(False);

                  ImportFromOtherPlugins(True);
                  Reload( Sender <> PluginMenuItemImportTaskQueue );

                  //if   BtnOptimizeGames.Enabled then ActiveControl:=BtnOptimizeGames
                  //else ActiveControl:=PluginLevelFileNamePanel;

                  OptimizationComboBox.Clear;
                  ActiveControl:=OptimizerTaskQueue.StringGrid;
                  OptimizerTaskQueue.Refresh(True);
                  OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,b);
                  ScrollInView(StringGrid.Row);
                  PluginEditMenuItemCopySolution.Hint:=HintCopySelectedSolutionToClipboard;
                  PopupMenuItemCopySolution.Hint:=PluginEditMenuItemCopySolution.Hint;
                  PopupMenuItemPaste.Hint:=PluginEditMenuItemPaste.Hint;
                  PluginMenuItemAdd.Hint:=Format(HintAddLevelSolutionsToTaskQueueText__+CombineWith_SHIFT_ToAddAllSolutionsText,[MAX_ITEMS_ON_PLUGIN_TASK_QUEUE]);
                  PluginToolButtonAdd.Hint:=PluginMenuItemAdd.Hint;
                  PluginMenuItemSort.Hint:=PopupMenuItemSort.Hint;
                  PopupMenuItemSort.Hint:=HintSortSolutionsOnListText;
                  PluginMenuItemSortOnOptimization.Visible:=True;
                  PopupMenuItemDeleteLevels.Visible:=False;
                  PluginMenuItemOpenPrior.Hint:=HintOpenPreviousLevelText;
                  PluginMenuItemOpenNext .Hint:=HintOpenNextLevelText;
                  PluginEditMenuItemDeleteSolutions.Hint:=HintRemoveSelectedSolutionsFromListText;
                  PluginToolButtonDelete.Caption:=PluginEditMenuItemDeleteSolutions.Caption;
                  PluginToolButtonDelete.Hint:=PluginEditMenuItemDeleteSolutions.Hint;
                  PopupMenuItemDeleteSolutions.Hint:=PluginEditMenuItemDeleteSolutions.Hint;
                  PluginToolButtonSave.Hint:=PluginMenuItemSave.Hint;
                  PluginToolButtonSaveAs.Hint:=PluginMenuItemSaveAs.Hint;
                  PluginToolButtonCut.Hint:='';
                  PluginToolButtonCopy.Hint:=PluginEditMenuItemCopy.Hint;
                  PluginToolButtonPaste.Hint:=PluginEditMenuItemPaste.Hint;
                  end
          else if PageControl1.ActivePage=TabSheetGenerator then with GeneratorTaskQueue do begin
                  LevelGroupBox.Caption:=CandidateText;
                  for gcbii:=Low(GeneratorCallBackInfoText) to High(GeneratorCallBackInfoText) do
                      PluginLevelStringGrid.Cells[0,Ord(gcbii)]:=GeneratorCallBackInfoText[gcbii];
                  for i:=Succ(Ord(High(GeneratorCallBackInfoText))) to Pred(PluginLevelStringGrid.RowCount) do
                      PluginLevelStringGrid.Cells[0,i]:='';

                  with MainForm.Generator do
                    if not Initialized then begin
                       Initialized:=True;
                       if (not IsANewFileName(FileName)) then
                          if   FileExists(FileName) then
                               LoadFromFile(FileName)
                          else FileName:='';
                       end;

                  Reload(True);

                  ActiveControl:=GeneratorTaskQueue.StringGrid;
                  GeneratorTaskQueue.Refresh(True);
                  GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,b);
                  ScrollInView(StringGrid.Row);
                  PluginEditMenuItemCopySolution.Hint:=HintCopySelectedSolutionToClipboard;
                  PopupMenuItemCopySolution.Hint:=PluginEditMenuItemCopySolution.Hint;
                  PopupMenuItemPaste.Hint:=PluginEditMenuItemPaste.Hint;
                  PluginMenuItemAdd.Hint:=Format(HintAddLevelSolutionsToTaskQueueText__,[MAX_ITEMS_ON_PLUGIN_TASK_QUEUE]);
                  PluginToolButtonAdd.Hint:=PluginMenuItemAdd.Hint;
                  PluginMenuItemSort.Hint:=PopupMenuItemSort.Hint;
                  PopupMenuItemSort.Hint:=HintSortSolutionsOnListText;
                  PluginMenuItemSortOnOptimization.Visible:=True;
                  PopupMenuItemDeleteLevels.Visible:=True;
                  PluginMenuItemOpenPrior.Hint:=HintOpenPreviousLevelText;
                  PluginMenuItemOpenNext .Hint:=HintOpenNextLevelText;
                  PluginEditMenuItemDeleteLevels.Caption:=DeleteCandidatesFromListText;
                  PluginEditMenuItemDeleteLevels.Hint:=HintDeleteSelectedCandidatesFromListText;
                  PluginToolButtonDelete.Caption:=PluginEditMenuItemDeleteLevels.Caption;
                  PluginToolButtonDelete.Hint:=PluginEditMenuItemDeleteLevels.Hint;
                  PopupMenuItemDeleteLevels.Caption:=PluginEditMenuItemDeleteLevels.Caption;
                  PopupMenuItemDeleteLevels.Hint:=PluginEditMenuItemDeleteLevels.Hint;
                  PluginToolButtonOpen.Hint:=GeneratorMenuItemOpen.Hint;
                  PluginToolButtonSave.Hint:=GeneratorMenuItemSave.Hint;
                  PluginToolButtonSaveAs.Hint:=GeneratorMenuItemSaveAs.Hint;
                  PluginToolButtonCut.Hint:=GeneratorEditMenuItemCut.Hint;
                  PluginToolButtonCopy.Hint:=GeneratorEditMenuItemCopy.Hint;
                  PluginToolButtonPaste.Hint:=GeneratorEditMenuItemPaste.Hint;
                  end;

          OpenForm.EnableDisablePluginControls(Self,Plugin__,True);

          PluginMenuItemSort.Hint:=PopupMenuItemSort.Hint;
           PluginToolButtonOpenPrior.Hint:=PluginMenuItemOpenPrior.Hint;
          PluginToolButtonOpenNext .Hint:=PluginMenuItemOpenNext .Hint;

          PopupMenuItemSort.Visible:=PageControl1.ActivePage<>TabSheetGenerator;
          PluginMenuItemSortOnSolutionNames.Visible:=PluginMenuItemSortOnOptimization.Visible;
          PluginMenuItemSortOnOptimizationSeparator.Visible:=PluginMenuItemSortOnOptimization.Visible;

          PopupMenuItemSortOnSolutionNames.Visible:=PluginMenuItemSortOnSolutionNames.Visible;
          PopupMenuItemSortOnOptimizationSeparator.Visible:=PluginMenuItemSortOnOptimizationSeparator.Visible;
          PopupMenuItemSortOnOptimization.Visible:=PluginMenuItemSortOnOptimization.Visible;

          PopupMenuItemDeleteSolutions.Visible:=not PopupMenuItemDeleteLevels.Visible;

          PluginEditMenuItemCopyLevelCreatedFromSolutionSlice.Visible:=PageControl1.ActivePage=TabSheetOptimizer;
          TaskQueuePopupMenuItemCopyLevelCreatedFromSolutionSlice.Visible:=PluginEditMenuItemCopyLevelCreatedFromSolutionSlice.Visible;
          PluginEditMenuItemSelectSolutionSliceToBeOptimized.Visible:=PluginEditMenuItemCopyLevelCreatedFromSolutionSlice.Visible;
          PluginEditMenuItemSelectAreaToBeOptimized.Visible:=PluginEditMenuItemSelectSolutionSliceToBeOptimized.Visible;
          TaskQueuePopupMenuItemSelectSolutionSliceToBeOptimized.Visible:=PluginEditMenuItemSelectSolutionSliceToBeOptimized.Visible;
          TaskQueuePopupMenuItemSelectAreaToBeOptimized.Visible:=PluginEditMenuItemSelectAreaToBeOptimized.Visible;
          PluginEditMenuItemSelectAreaToBeOptimized.Visible:=PluginEditMenuItemSelectSolutionSliceToBeOptimized.Visible;
          PluginEditMenuItemRepeatSolutionSlicing.Visible:=PluginEditMenuItemCopyLevelCreatedFromSolutionSlice.Visible;
          TaskQueuePopupMenuItemRepeatSolutionSlicing.Visible:=PluginEditMenuItemRepeatSolutionSlicing.Visible;
          PluginEditMenuItemSetOptimizationInterval.Visible:=PluginEditMenuItemRepeatSolutionSlicing.Visible;
          TaskQueuePopupMenuItemSetOptimizationInterval.Visible:=TaskQueuePopupMenuItemSetOptimizationInterval.Visible;
          PluginEditMenuItemClearBoardAreaSelectedForOptimization.Visible:=PluginEditMenuItemRepeatSolutionSlicing.Visible;
          TaskQueuePopupMenuItemClearBoardAreaSelectedForOptimization.Visible:=PluginEditMenuItemClearBoardAreaSelectedForOptimization.Visible;

          with Plugin__ do begin
            Enter;
            try     if (Plugin__ is TSolverPlugin) and Assigned(SolverTaskQueue) then with SolverTaskQueue do with StringGrid do begin
                       SolveLevelsGroupBox.Visible:=(RowCount>Succ(FixedRows))
                                                    or
                                                    (Assigned(Levels[FixedRows])
                                                     and
                                                     (not IsPluginLevel(FixedRows))
                                                    );
                       PluginLevelGroupBox.Caption:=PluginLevelGroupBoxText[False {SolveLevelsGroupBox.Visible}]; // use 'SolveLevelsGroupBox.Visible' as index to set the caption to singular/plural "Solve level/s"
                       end;

                    if (Plugin__ is TGenerator) and (not Plugin__.IsActive) then
                       OpenForm.SetPluginButtonState(BtnGenerateLevels,pbsRun);

                    OpenForm.PluginTimer.Enabled:=(Assigned(MainForm.Solver   ) and (MainForm.Solver   .StartTimeMS<>0))
                                                  or
                                                  (Assigned(MainForm.Optimizer) and (MainForm.Optimizer.StartTimeMS<>0))
                                                  or
                                                  (Assigned(MainForm.Generator) and (MainForm.Generator.StartTimeMS<>0));
            finally Leave;
            end;
            end;
          end;

       for i:=0 to Pred(PluginLevelStringGrid.RowCount) do PluginLevelStringGrid.Cells[1,i]:='';
       LevelNamePanel.Hint:='';
       end;

  end; // InitializePluginTabSheet

  procedure InitializeTabSheet;
  var i:Integer;
  begin
    EditMenuItemMovePlayerAndBoxes            .Visible:=PageControl1.ActivePage=TabSheetEditor;
    PluginToolButtonSeparatorGeneratorEditPlay.Visible:=PageControl1.ActivePage=TabSheetGenerator;

    with PageControl1 do begin
      if   ( ActivePage <> TabSheetCapture ) or ( not Assigned( CaptureForm ) ) then
           StatusBar1.Panels[0].Width := StatusBar1.Tag
      else ToolsForm.StatusBar1.Panels[ 0 ].Width := CaptureForm.StatusBarPanel0Widths[ CaptureForm.Editor.Step = csBoardSquares ];
      if (ActivePage<>TabSheetGenerator) and Assigned(MainForm.Generator) and (not MainForm.Generator.IsActive) then
         MainForm.Generator.ReleaseMemory;

      if ActivePage=TabSheetEditor then begin
         ShowTitle('',Editor.FileName);

         if   not PanelBtn.Visible then begin
              EditImage1.Visible:=False; // hiding the image while 'PanelBtn' is made visible seems to reduce flicker, in particular when the editor is launched from the 'Play' button on the generator tabsheet
              try
                      PanelBtn.Show;
                      if PanelBtn.Top           >  StatusBar1.Top then begin // 'True': repair the order of the components; because they both are bottom-aligned, the statusbar got above the panel
                         PanelBtn.Align         := alNone;
                         try     PanelBtn.Top   := StatusBar1.Top;
                         finally PanelBtn.Align := alBottom;
                         end;
                         end;
              finally EditImage1.Visible:=True;
              end;
              FormResize(Sender);
              end;

         if   LevelSetForm<>nil then with LevelSetForm do begin
              StatusBar1.{Panels[1].Text}SimpleText :='';
              if (Sender<>nil) and ShowOnStartUp and (not Visible) and (not IsClosing) then
                 try     Show;
                 finally Self.SetFocus;
                 end;
              end;

         if   Editor.DrawingToolCursorsEnabled then
              EditImage1.Cursor:=Succ(Ord(Editor.DrawingTool))
         else EditImage1.Cursor:=crDefault;

         CleanSolverTaskQueue;
         end
      else begin // active page <> editor
         HideCursor(True);

         with EditMenuEdit do
           for i:=0 to Pred(Count) do with Items[i] do
               if Caption<>'-' then Enabled:=False; // disable all editor shortcuts, e.g., Ctrl+A = 'select all'
         if ActivePage<>TabSheetCapture then with CaptureMenuEdit do
            for i:=0 to Pred(Count) do with Items[i] do
                if Caption<>'-' then Enabled:=False; // disable all editor shortcuts, e.g., Ctrl+A = 'select all'

         PluginToolButtonNewCandidateSet.Visible:=(ActivePage=TabSheetGenerator);
         PluginMenuItemAdd.Visible:=(ActivePage=TabSheetSolver) or (ActivePage=TabSheetOptimizer);
         PluginMenuItemAddSeparator.Visible:=PluginMenuItemAdd.Visible;
         PluginToolButtonAdd.Visible:=PluginMenuItemAdd.Visible;
         PluginEditMenuItemResetOptimizationMethod.Visible := (ActivePage=TabSheetOptimizer);
         TaskQueuePopupMenuItemResetOptimizationMethod.Visible := PluginEditMenuItemResetOptimizationMethod.Visible;

         with OptimizerTaskQueue do OldStringGridRow:=StringGrid.Row;
         with SolverTaskQueue    do OldStringGridRow:=StringGrid.Row;
         with GeneratorTaskQueue do OldStringGridRow:=StringGrid.Row;

         ImageReplaySpeed.Hint:=HintToolsWindowReplaySpeedText;

         if      LevelSetForm<>nil then with LevelSetForm do
                 if (Sender<>nil) and Visible then begin
                    ShowOnStartUp:=True;
                    Hide;
                    end;

         if      PanelBtn.Visible then begin
                 PanelBtn.Hide;
                 FormResize(Sender);
                 end;

         if      ActivePage=TabSheetSolver then begin
                 InitializePluginTabSheet(MainForm.Solver);
                 end
         else if ActivePage=TabSheetOptimizer then begin
                 InitializePluginTabSheet(MainForm.Optimizer);
                 end
         else if ActivePage=TabSheetGenerator then begin
                 InitializePluginTabSheet(MainForm.Generator);
                 end
         else if ( ActivePage = TabSheetCapture ) and Assigned( CaptureForm ) then begin
                 CaptureForm.ShowTitle;
                 if (not CaptureForm.Initialized ) and Initialized then begin // 'True': the capture form initialization failed; block the capture tool by simply sending the user to the editor tabsheet
                    Msg( TEXT_TASK_FAILED, Self.Caption, MB_ICONINFORMATION + MB_OK );
                    ActivePage := TabSheetEditor;
                    end;
                 end
              else begin
                 ShowTitle('','');
                 end;
         end;

      PageControl1  .Tag    :=Integer(PageControl1.ActivePage); // use 'Tag' to store the current page
      EditMenuFile  .Visible:=ActivePage=TabSheetEditor;
      EditMenuEdit  .Visible:=EditMenuFile.Visible;
      EditMenuView  .Visible:=EditMenuFile.Visible;

      PluginMenuFile.Visible:=(ActivePage=TabSheetSolver) or (ActivePage=TabSheetOptimizer);
      PluginMenuEdit.Visible:=PluginMenuFile.Visible;
      PluginMenuPlugin.Visible:=PluginMenuFile.Visible;
      PluginMenuItemOpenPrior.Visible:=(ActivePage=TabSheetSolver) or (ActivePage=TabSheetOptimizer);
      PluginMenuItemOpenNext.Visible:=PluginMenuItemOpenPrior.Visible;
      PluginToolButtonOpenPrior.Visible:=PluginMenuItemOpenPrior.Visible;
      PluginToolButtonOpenNext.Visible:=PluginMenuItemOpenNext.Visible;

      PluginToolButtonSeparatorGeneratorEditPlay.Visible:=PageControl1.ActivePage=TabSheetGenerator;
      PluginToolButtonGeneratorEdit.Visible:=PluginToolButtonSeparatorGeneratorEditPlay.Visible;
      PluginToolButtonGeneratorPlay.Visible:=PluginToolButtonSeparatorGeneratorEditPlay.Visible;

      SettingsMenuAlternatingOptimizations.Visible:=ActivePage=TabSheetOptimizer;
      SettingsMenuAlternatingOptimizationsSeparator.Visible:=SettingsMenuAlternatingOptimizations.Visible;

      GeneratorMenuFile.Visible:=ActivePage=TabSheetGenerator;
      GeneratorMenuEdit.Visible:=GeneratorMenuFile.Visible;

      CaptureMenuFile.Visible:=ActivePage=TabSheetCapture;
      CaptureMenuEdit.Visible:=CaptureMenuFile.Visible;
      CaptureMenuView.Visible:=CaptureMenuFile.Visible;
      CaptureMenuSettings.Visible:=CaptureMenuFile.Visible;
      SettingsMenu.Visible:= not CaptureMenuSettings.Visible;

      StatusLabel1.Caption:=''; StatusLabel2.Caption:=''; StatusLabel3.Caption:='';
      ShowStatus;
      end;
  end; // InitializeTabSheet

begin // PageControl1Change
  PluginToolButtonStopReplayClick(Sender);
  //HideEditors;
  EditImage1.Cursor:=crDefault;
  if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
  Editor.MouseButtonDown:=False;

  HasLockedSolver:=Assigned(MainForm.Solver) and MainForm.Solver.HasThread;
  if      HasLockedSolver then MainForm.Solver.Enter;
  try     HasLockedOptimizer:=Assigned(MainForm.Optimizer) and MainForm.Optimizer.HasThread;
          if      HasLockedOptimizer then MainForm.Optimizer.Enter;
          try     HasLockedGenerator:=Assigned(MainForm.Generator) and MainForm.Generator.HasThread;
                  if      HasLockedGenerator then MainForm.Generator.Enter;
                  try     InitializeTabSheet;
                  finally if HasLockedGenerator then MainForm.Generator.Leave;
                  end;
          finally if HasLockedOptimizer then MainForm.Optimizer.Leave;
          end;
  finally if HasLockedSolver then MainForm.Solver.Leave;
  end;
end;

procedure TToolsForm.ShowStatusOptimizer;
begin
  ShowStatusPlugin(MainForm.Optimizer,OptimizeSolutionsStringGrid.Row);
end;

procedure TToolsForm.ShowStatusSolver;
begin
  ShowStatusPlugin(MainForm.Solver,SolveLevelsStringGrid.Row);
end;

procedure TToolsForm.ShowStatusGenerator;
begin
  ShowStatusPlugin(MainForm.Generator,GenerateLevelsStringGrid.Row);
end;

procedure TToolsForm.ShowStatusPlugin(Plugin__:TPlugin; Row__:Integer);
const DYNAMIC_BORDER_ICONS:TBorderIcons=[{biSystemMenu,} biMinimize, biMaximize];
var HasLevel:Boolean;
begin
  EditMenuItemGrid.Visible:=False; EditMenuItemSeparator.Visible:=False;

  if ((Plugin__=MainForm.Solver) or (Plugin__=MainForm.Optimizer) or (Plugin__=MainForm.Generator))
     and
     Assigned(Plugin__) then begin
     with PluginLevelInfo do begin
       PluginToolButtonReplay.Enabled:=ReplayInfo.MovesAsText<>'';
       if   Game.IsReplaying and (not PluginToolButtonReplay.Enabled) then begin
            OpenForm.Game.IsIdleAndStopReplayingAndBrowsing;
            Game.IsReplaying:=False;
            end;
       if   Game.IsReplaying then begin
            if PluginToolButtonReplay.ImageIndex<>IMAGE_INDEX_PAUSE then begin
               PluginToolButtonReplay.ImageIndex:=IMAGE_INDEX_PAUSE;
               PluginToolButtonReplay.Hint      :=HintPluginReplayPauseText[True];
               PluginToolButtonReplay.Refresh;
               with PluginToolButtonReplay do
                 if PtInRect(Rect(0,0,Width,Height),ScreenToClient(Mouse.CursorPos)) then
                    StatusText:=GetLongHint(Hint);
               end;
            end
       else if PluginToolButtonReplay.ImageIndex<>IMAGE_INDEX_REPLAY then begin
               PluginToolButtonReplay.ImageIndex:=IMAGE_INDEX_REPLAY;
               if   Editor.MouseWheelPresent then
                    PluginToolButtonReplay.Hint :=HintPluginReplayPauseText[False]+HintUseMouseWheelToStepText
               else PluginToolButtonReplay.Hint :=HintPluginReplayPauseText[False]+HintUse_INSERT_And_DELETE_ToStepText;
               PluginToolButtonReplay.Refresh;
               with PluginToolButtonReplay do
                 if PtInRect(Rect(0,0,Width,Height),ScreenToClient(Mouse.CursorPos)) then
                    StatusText:=GetLongHint(Hint);
               end;
       PluginToolButtonStopReplay.Enabled:=PluginToolButtonReplay.Enabled and
                                           ReplayInfo.IsLoaded and
                                           (OpenForm.Game.History.Count>OpenForm.Game.ForcedInitialJumps);
       end;

     //if MainMenu.Items[0].Enabled<>(not Game.IsReplaying) then with MainMenu do begin
     //   for i:=0 to Pred(Items.Count) do Items[i].Enabled:=not Game.IsReplaying;
        //if   Game.IsReplaying then
        //     BorderIcons:=BorderIcons-DYNAMIC_BORDER_ICONS
        //else BorderIcons:=BorderIcons+DYNAMIC_BORDER_ICONS;
     //   end;

     // PluginMenuItemOpenPlugin.Enabled:=not Game.IsReplaying;

     if PageControl1.ActivePage<>TabSheetGenerator then begin
        HasLevel:=StrEqual(PluginLevelInfo.LevelName,Editor.FileName)
                  and
                  PluginLevelInfo.IsALegalLevel
                  and
                  (not Editor.Selection.Enabled);
        PluginMenuItemOpen.Enabled:=True;
        end
     else begin
        HasLevel:=(not GeneratorTaskQueue.IsEmpty);
        PluginToolButtonNewCandidateSet.Enabled:=(Plugin__.Button.Tag=Ord(pbsRun)) and (not MainForm.Generator.IsActive);
        GeneratorMenuItemNew.Enabled:=PluginToolButtonNewCandidateSet.Enabled;
        PluginMenuItemOpen.Enabled:=PluginToolButtonNewCandidateSet.Enabled;
        GeneratorMenuItemOpen.Enabled:=PluginMenuItemOpen.Enabled;
        end;

     if HasLevel then begin
        if        PageControl1.ActivePage=TabSheetOptimizer then begin
                  PluginMenuItemSaveAs.Enabled:=OptimizerTaskQueue.IsPluginLevel(Row__);
                  PluginMenuItemSaveTaskQueueAs.Enabled := not OptimizerTaskQueue.IsEmpty;
                  end
        else if   PageControl1.ActivePage=TabSheetSolver then begin
                  PluginMenuItemSaveAs.Enabled:=(not SolveLevelsGroupBox.Visible) or
                                                SolverTaskQueue.IsEmpty or
                                                SolverTaskQueue.IsPluginLevel(Row__);
                  PluginMenuItemSaveTaskQueueAs.Enabled := (not SolverTaskQueue.IsEmpty) and SolveLevelsGroupBox.Visible;
                  end
        else if   PageControl1.ActivePage=TabSheetGenerator then begin
                  PluginMenuItemSaveAs.Enabled:=PluginToolButtonNewCandidateSet.Enabled;
                  end
             else PluginMenuItemSaveAs.Enabled:=True;
        end
     else
        PluginMenuItemSaveAs.Enabled:=
          (PageControl1.ActivePage=TabSheetGenerator) and
          MainForm.Generator.HasSokoFile and
          (MainForm.Generator.SokoFile.Modified or (not IsANewFileName(SokoFile.Name))) and
          (MainForm.Generator.SokoFile.Name<>'') and // and (not MainForm.Generator.SokoFile.Levels.IsEmpty);
          PluginToolButtonNewCandidateSet.Enabled;

     PluginMenuItemOpenPrior.Enabled:=HasLevel
                                      and
                                      (not Modified)
                                      {$IFDEF YASC}
                                        and
                                        (not MainForm.Modified)
                                      {$ENDIF}
                                      and
                                      (not IsANewFileName(Editor.FileName));
     PluginMenuItemOpenNext.Enabled:=PluginMenuItemOpenPrior.Enabled;
     PluginMenuItemAdd.Enabled:=PluginMenuItemOpenPrior.Enabled
                                and
                                ((OptimizeSolutionsStringGrid.RowCount-OptimizeSolutionsStringGrid.FixedRows<MAX_ITEMS_ON_PLUGIN_TASK_QUEUE)
                                );

     if   not (Plugin__ is TGenerator) then
          PluginMenuItemSave.Enabled:=PluginMenuItemSaveAs.Enabled
                                      and
                                      ((Modified {$IFDEF YASC} or MainForm.Modified {$ENDIF}) or (PluginLevelInfo.IsALegalLevel and (PluginLevelInfo.NewGamesCount<>0)))
                                      and
                                      (not IsANewFileName(Editor.FileName))
     else with MainForm.Generator do PluginMenuItemSave.Enabled:=PluginMenuItemSaveAs.Enabled and SokoFile.Modified and (not IsANewFileName(SokoFile.Name));
     PluginEditMenuItemCopy.Enabled:=PluginMenuItemSaveAs.Enabled
                                     and
                                     (not (Modified {$IFDEF YASC} or MainForm.Modified {$ENDIF} or (PluginLevelInfo.IsALegalLevel and (PluginLevelInfo.NewGamesCount<>0))));
     PluginEditMenuItemPaste.Enabled:=(not (Plugin__ is TGenerator)) and Clipboard.HasFormat(CF_TEXT);
     GeneratorEditMenuItemPaste.Enabled:=(Plugin__ is TGenerator) and Clipboard.HasFormat(CF_TEXT) and (not Plugin__.IsActive);
     PluginMenuItemOpenPlugin.Enabled:=(Plugin__.Button.Tag=Ord(pbsRun));
     PluginMenuItemSettings.Enabled:=(Plugin__ is TGenerator) or (Assigned(Plugin__.ComboBox) and (Plugin__.ComboBox.ItemIndex>=0) and Plugin__.IsLoaded and Assigned(Plugin__.PluginFunctions.Settings) {and Assigned(Plugin__.Button) and (Plugin__.Button.Tag=Ord(pbsRun)) and (not Plugin__.IsRunningInAnotherThread)});
     PluginMenuItemAbout.Enabled:=Assigned(Plugin__.ComboBox) and (Plugin__.ComboBox.ItemIndex>=0) and Plugin__.IsLoaded and Assigned(Plugin__.PluginFunctions.ShowAbout);
     PluginMenuItemRemove.Enabled:=PluginMenuItemSettings.Enabled and (not StrEqual(DEFAULT_VALUE,Plugin__.PluginFileName));
     with OptimizeSolutionsStringGrid do PluginMenuItemClearLog.Enabled:=(RowCount>Succ(FixedRows)) or (Cells[Ord(oscLevelName),FixedRows]<>'');

     EditToolButtonOpen.Enabled:=PluginMenuItemOpen.Enabled;
     EditToolButtonSave.Enabled:=PluginMenuItemSave.Enabled;
     PluginToolButtonOpenPrior.Enabled:=PluginMenuItemOpenPrior.Enabled;
     PluginToolButtonOpen.Enabled:=PluginMenuItemOpen.Enabled;
     PluginToolButtonOpenNext.Enabled:=PluginMenuItemOpenNext.Enabled;
     PluginToolButtonAdd.Enabled:=PluginMenuItemAdd.Enabled;
     PluginToolButtonSave.Enabled:=PluginMenuItemSave.Enabled;
     PluginToolButtonSaveAs.Enabled:=PluginMenuItemSaveAs.Enabled;
     PluginToolButtonCopy.Enabled:=PluginEditMenuItemCopy.Enabled;
     PluginToolButtonPaste.Enabled:=PluginEditMenuItemPaste.Enabled or GeneratorEditMenuItemPaste.Enabled;
     //PluginToolButtonHelp.Enabled:=not Game.IsReplaying;
     //PluginToolButtonExit.Enabled:=not Game.IsReplaying;
     if      Plugin__ is TSolverPlugin then begin
             BtnSolveLevel.Enabled:=OpenForm.BtnSolveLevel.Enabled;
             BtnSolverBrowse.Enabled:=PluginMenuItemOpenPlugin.Enabled;
             BtnSolverSettings.Enabled:=PluginMenuItemSettings.Enabled;
             BtnSolverAbout.Enabled:=PluginMenuItemAbout.Enabled;
             SolverComboBox.Enabled:=PluginMenuItemOpenPlugin.Enabled;
             end
     else if Plugin__ is TOptimizerPlugin then begin
             BtnOptimizeGames.Enabled:=OpenForm.BtnOptimizeGames.Enabled;
             BtnOptimizerBrowse.Enabled:=PluginMenuItemOpenPlugin.Enabled;
             BtnOptimizerSettings.Enabled:=PluginMenuItemSettings.Enabled;
             BtnOptimizerAbout.Enabled:=PluginMenuItemAbout.Enabled;
             OptimizerComboBox.Enabled:=PluginMenuItemOpenPlugin.Enabled;
             end
     else if Plugin__ is TGenerator then begin
             BtnGenerateLevels.Enabled:=(Plugin__.Button.Tag<>Ord(pbsRun)) or (YASGen_.GA.Statistics.CandidatesCount<GA_DEFAULT_MAX_CANDIDATE_COUNT);
             BtnGeneratorSettings.Enabled:=PluginMenuItemSettings.Enabled;
             GeneratorMenuItemSave.Enabled:=PluginMenuItemSave.Enabled;
             GeneratorMenuItemSaveAs.Enabled:=PluginMenuItemSaveAs.Enabled;
             end;

     with OptimizerTaskQueue do with StringGrid do
       ItemsCheckBox.Enabled:=(RowCount>Succ(FixedRows))
                              or
                              (Assigned(Levels[FixedRows])
                               and
                               (not Levels[FixedRows].SnapshotsAsText.IsEmpty)
                               and
                               (not TSnapshotAsText(Levels[FixedRows].SnapshotsAsText.Last).MovesAsTextLines.IsEmpty)
                               and
                               (TSnapshotAsText(Levels[FixedRows].SnapshotsAsText.Last).MovesAsTextLines.First.Text<>'')
                              );
     with SolverTaskQueue do with StringGrid do
       ItemsCheckBox.Enabled:=(RowCount>Succ(FixedRows))
                              or
                              Assigned(Levels[FixedRows]);
     with GeneratorTaskQueue do with StringGrid do
       ItemsCheckBox.Enabled:=(RowCount>Succ(FixedRows))
                              or
                              Assigned(Levels[FixedRows]);

     if      PluginToolButtonReplay.Enabled then begin
             if        Game.IsReplaying then
                       ImageBoard.Hint:=IsReplayingText[roForwards in PluginLevelInfo.ReplayInfo.ReplayOptions]+HintToolsWindowReplayText
             else if   PluginToolButtonStopReplay.Enabled then
                       ImageBoard.Hint:=HintReplayInToolsWindowText+HintResetInToolsWindowText
                  else if   PageControl1.ActivePage=TabSheetOptimizer then
                            ImageBoard.Hint:=HintReplayInToolsWindowText+HintRightClickToSelectAreaText
                       else ImageBoard.Hint:=HintReplayInToolsWindowText;
             end
     else if ImageBoard.Hint<>'' then
             ImageBoard.Hint:='';
     if PtInRect(Rect(0,0,ImageBoard.Width,ImageBoard.Height),ImageBoard.ScreenToClient(Mouse.CursorPos)) then
        StatusText:=GetLongHint(ImageBoard.Hint);

     with StatusBar1.Panels[0] do
       if   Game.IsReplaying or Game.IsBrowsing or PluginToolButtonStopReplay.Enabled then
            with OpenForm.Game.History do Text:=IntToStr(Count)+SLASH+IntToStr(PushCount)
       else if   PageControl1.ActivePage<>TabSheetGenerator then
                 Text:=OKChangedText[Modified {$IFDEF YASC} or MainForm.Modified {$ENDIF}]
            else if Assigned(MainForm.Generator) and (not MainForm.Generator.IsActive) then
                    Text:=OkChangedText[MainForm.Generator.HasSokoFile and MainForm.Generator.SokoFile.Modified];

     PluginEditMenuItemCut.Visible:=(Plugin__ is TGenerator);
     PluginEditMenuItemCopySolution.Visible:=(Plugin__ is TSolverPlugin) or (Plugin__ is TOptimizerPlugin);
     PluginEditMenuItemCopySolutionSeparator.Visible:=PluginEditMenuItemCopySolution.Visible;
     PluginEditMenuItemDeleteLevels.Visible:=Plugin__ is TSolverPlugin;
     GeneratorEditMenuItemDeleteCandidates.Visible:=Plugin__ is TGenerator;
     PluginEditMenuItemDeleteSolutions.Visible:=Plugin__ is TOptimizerPlugin;
     PluginEditMenuItemOptimizationSeparator.Visible:=Plugin__ is TOptimizerPlugin;
     PopupMenuItemOptimizationSeparator.Visible:=PluginEditMenuItemOptimizationSeparator.Visible;
     PluginEditMenuItemResetOptimizationMethod.Visible:=PluginEditMenuItemOptimizationSeparator.Visible;
     TaskQueuePopupMenuItemResetOptimizationMethod.Visible := PluginEditMenuItemResetOptimizationMethod.Visible;
     PluginEditMenuItemDeleteSeparator.Visible:=PluginEditMenuItemDeleteLevels.Visible or PluginEditMenuItemDeleteSolutions.Visible;
     PluginToolButtonCut.Visible:=PluginEditMenuItemCut.Visible;
     PluginToolButtonDelete.Visible:=PluginEditMenuItemDeleteLevels.Visible or PluginEditMenuItemDeleteSolutions.Visible or GeneratorEditMenuItemDeleteCandidates.Visible;
     PopupMenuItemCopySolution.Visible:=PluginEditMenuItemCopySolution.Visible;
     PopupMenuItemCopySolutionSeparator.Visible:=PopupMenuItemCopySolution.Visible;

     PluginEditMenuItemCut.Enabled:=False;
     if        (Plugin__ is TSolverPlugin) and  Assigned(SolverTaskQueue) then with SolverTaskQueue do begin
               Plugin__.Enter;
               try     with StringGrid do begin
                         ItemsCheckBox.Enabled:=((RowCount>Succ(FixedRows))
                                                 or
                                                 Assigned(Levels[FixedRows])
                                                )
                                                and
                                                ((RowCount>Succ(FixedRows))
                                                 or
                                                 ((RowCount=Succ(FixedRows))
                                                  and
                                                  Assigned(Levels[FixedRows])
                                                  and
                                                  (not (ltfLocked in Levels[FixedRows].Tag.Flags))
                                                 )
                                                );
                         PopupMenuItemSort.Enabled:=RowCount>Succ(FixedRows);
                         end;

                       if PageControl1.ActivePage=TabSheetSolver then with Plugin__ do begin
                          PluginEditMenuItemDeleteLevels.Enabled:=SelectedCount<>0;
                          PluginEditMenuItemCopySolution.Enabled:=PluginToolButtonReplay.Enabled;
                          TaskQueuePopupMenuItemToggleSelectionAboveItem.Enabled:=Row__>StringGrid.FixedRows;
                          TaskQueuePopupMenuItemToggleSelectionBelowItem.Enabled:=Row__<Pred(StringGrid.RowCount);

                          if SolveLevelsGroupBox.Visible then with SolverTaskQueue do with StringGrid do begin
                             if IsEmpty
                                or
                                ((RowCount=Succ(FixedRows))
                                 and
                                 IsPluginLevel(FixedRows)
                                ) then begin
                                if ActiveControl=SolveLevelsStringGrid then
                                   if   BtnSolveLevel.Enabled then ActiveControl:=BtnSolveLevel
                                   else ActiveControl:=PluginLevelFileNamePanel;
                                SolveLevelsGroupBox.Visible:=False;
                                PluginLevelGroupBox.Caption:=PluginLevelGroupBoxText[False {SolveLevelsGroupBox.Visible}]; // use 'SolveLevelsGroupBox.Visible' as index to set the caption to singular/plural "Solve level/s"
                                end;
                             end
                          else with SolverTaskQueue do with StringGrid do begin
                             if (RowCount>Succ(FixedRows))
                                or
                                (Assigned(Levels[FixedRows])
                                 and
                                 (not IsPluginLevel(FixedRows))
                                ) then begin
                                SolveLevelsGroupBox.Visible:=True;
                                PluginLevelGroupBox.Caption:=PluginLevelGroupBoxText[False {SolveLevelsGroupBox.Visible}]; // use 'SolveLevelsGroupBox.Visible' as index to set the caption to singular/plural "Solve level/s"
                                ActiveControl:=SolveLevelsStringGrid;
                                end;
                             end;

                          if SolveLevelsGroupBox.Visible then with StringGrid do begin
                             if   SelectedCount<>0 then
                                  Cells[Ord(slcNo),0]:=IntToStr(SelectedCount{RowCount-FixedRows})+SPACE
                             else Cells[Ord(slcNo),0]:='';
                             if (PluginLevelFileNamePanel.Caption<>'') and
                                (Plugin__.ThreadState=ptsIdle) then
                                PluginLevelFileNamePanel.Caption:='';
                             end
                          else with PluginLevelInfo do
                            if //Plugin__.IsLoaded and
                               (PluginLevelFileNamePanel.Caption='') and
                               IsALegalLevel and
                               StrEqual(LevelName,Editor.FileName) then begin
                               LoadLevelFromEditor(True,True);
                               end;

                          PluginMenuItemSaveAs.Enabled:=(not SolveLevelsGroupBox.Visible) or
                                                        SolverTaskQueue.IsEmpty or
                                                        SolverTaskQueue.IsPluginLevel(Row__);
                          PluginMenuItemSaveTaskQueueAs.Enabled := (not SolverTaskQueue.IsEmpty) and SolveLevelsGroupBox.Visible;

                          PluginEditMenuItemStatistics.Visible:=Plugin__.StatisticsEnabled;
                          PluginEditMenuItemStatisticsSeparator.Visible:=PluginEditMenuItemStatistics.Visible;
                          if PluginEditMenuItemStatistics.Visible then
                             PluginEditMenuItemStatistics.Enabled:=(StatisticsTaskCount>0) and
                                                                   Assigned(StatisticsText) and
                                                                   (StatisticsText.Lines.Count>TASK_QUEUE_STATISTICS_HEADER_LINES_COUNT);
                          end;

               finally Plugin__.Leave;
               end;
               end
     else if   (Plugin__ is TOptimizerPlugin) and Assigned(OptimizerTaskQueue) then with OptimizerTaskQueue do begin
               Plugin__.Enter;
               try     with StringGrid do begin
                         ItemsCheckBox.Enabled:=((RowCount>Succ(FixedRows))
                                                 or
                                                 (Assigned(Levels[FixedRows])
                                                  and
                                                  (not Levels[FixedRows].SnapshotsAsText.IsEmpty)
                                                  and
                                                  (not TSnapshotAsText(Levels[FixedRows].SnapshotsAsText.Last).MovesAsTextLines.IsEmpty)
                                                  and
                                                  (TSnapshotAsText(Levels[FixedRows].SnapshotsAsText.Last).MovesAsTextLines.First.Text<>'')
                                                 )
                                                )
                                                and
                                                ((RowCount>Succ(FixedRows))
                                                 or
                                                 ((RowCount=Succ(FixedRows))
                                                  and
                                                  Assigned(Levels[FixedRows])
                                                  and
                                                  (not (ltfLocked in Levels[FixedRows].Tag.Flags))
                                                 )
                                                );
                         PopupMenuItemSort.Enabled:=RowCount>Succ(FixedRows);
                         end;
                       if PageControl1.ActivePage=TabSheetOptimizer then begin
                          PluginEditMenuItemDeleteSolutions.Enabled:=SelectedCount<>0;
                          PluginEditMenuItemCopySolution.Enabled:=Assigned(Levels[Row__]) and
                                                                  (not Levels[Row__].SnapshotsAsText.IsEmpty) and
                                                                  (TSnapshotAsText(Levels[Row__].SnapshotsAsText.Last).MovesAsTextLines.First.Text<>'');
                          PluginEditMenuItemResetOptimizationMethod.Enabled:=PluginEditMenuItemDeleteSolutions.Enabled;
                          TaskQueuePopupMenuItemResetOptimizationMethod.Enabled:=PluginEditMenuItemResetOptimizationMethod.Enabled;
                          TaskQueuePopupMenuItemToggleSelectionAboveItem.Enabled:=Row__>StringGrid.FixedRows;
                          TaskQueuePopupMenuItemToggleSelectionBelowItem.Enabled:=Row__<Pred(StringGrid.RowCount);

                          PluginMenuItemSaveTaskQueueAs.Enabled := ( not OptimizerTaskQueue.IsEmpty )
                                                                   and
                                                                   ( ( StringGrid.RowCount > Succ( StringGrid.FixedRows ) ) // 'True': more than one item on the list
                                                                     or
                                                                     PluginEditMenuItemCopySolution.Enabled // 'True': the single item on the list isn't an empty 'No solution' item
                                                                   );
                          PluginEditMenuItemStatistics.Visible:=Plugin__.StatisticsEnabled;
                          PluginEditMenuItemStatisticsSeparator.Visible:=PluginEditMenuItemStatistics.Visible;
                          if PluginEditMenuItemStatistics.Visible then
                             PluginEditMenuItemStatistics.Enabled:=(StatisticsTaskCount>0) and
                                                                   Assigned(StatisticsText) and
                                                                   (StatisticsText.Lines.Count>TASK_QUEUE_STATISTICS_HEADER_LINES_COUNT);
                          with PluginLevelInfo.ReplayInfo do PluginEditMenuItemCopyLevelCreatedFromSolutionSlice.Enabled:= ( SelectedRange[2] > 0 ) and ( SelectedRange[0] < SelectedRange[1] ) and ( ( SelectedRange[1] - SelectedRange[0] ) < SelectedRange[2] );
                          TaskQueuePopupMenuItemCopyLevelCreatedFromSolutionSlice.Enabled:=PluginEditMenuItemCopyLevelCreatedFromSolutionSlice.Enabled;
                          PluginEditMenuItemSelectSolutionSliceToBeOptimized.Enabled:=PluginEditMenuItemCopySolution.Enabled;
                          PluginEditMenuItemSelectAreaToBeOptimized.Enabled:=PluginEditMenuItemSelectSolutionSliceToBeOptimized.Enabled;
                          TaskQueuePopupMenuItemSelectSolutionSliceToBeOptimized.Enabled:=PluginEditMenuItemSelectSolutionSliceToBeOptimized.Enabled;
                          TaskQueuePopupMenuItemSelectAreaToBeOptimized.Enabled:=PluginEditMenuItemSelectAreaToBeOptimized.Enabled;
                          PluginEditMenuItemSelectAreaToBeOptimized.Enabled:=PluginEditMenuItemSelectSolutionSliceToBeOptimized.Enabled;
                          with PluginLevelInfo.ReplayInfo do PluginEditMenuItemRepeatSolutionSlicing.Enabled:=PluginEditMenuItemCopyLevelCreatedFromSolutionSlice.Enabled and ( SelectedRange[1] < SelectedRange[2] );
                          TaskQueuePopupMenuItemRepeatSolutionSlicing.Enabled:=PluginEditMenuItemRepeatSolutionSlicing.Enabled;
                          PluginEditMenuItemRepeatSolutionSlicing.Checked:=PluginEditMenuItemRepeatSolutionSlicing.Enabled and (PluginLevelInfo.ReplayInfo.SelectedRange[6]>0);
                          TaskQueuePopupMenuItemRepeatSolutionSlicing.Checked:=PluginEditMenuItemRepeatSolutionSlicing.Checked;
                          with PluginLevelInfo.ReplayInfo do PluginEditMenuItemSetOptimizationInterval.Enabled:=PluginEditMenuItemSelectSolutionSliceToBeOptimized.Enabled and ( SelectedRange[0] < SelectedRange[1] ) and (OptimizerTaskQueue.SelectedCount >= 2 );
                          TaskQueuePopupMenuItemSetOptimizationInterval.Enabled:=PluginEditMenuItemSetOptimizationInterval.Enabled;
                          with PluginLevelInfo.ReplayInfo do PluginEditMenuItemClearBoardAreaSelectedForOptimization.Enabled:=PluginLevelInfo.SelectedSquaresAsText<>'';
                          TaskQueuePopupMenuItemClearBoardAreaSelectedForOptimization.Enabled:=PluginEditMenuItemClearBoardAreaSelectedForOptimization.Enabled;

                          ImageReplaySpeed.Visible:=Assigned(Game) and (Game.IsReplaying or (PluginLevelInfo.ReplayInfo.MovesAsText<>''));
                          if not ImageReplaySpeed.Visible then ImageReplaySpeed.Tag:=0;
                          end;
               finally Plugin__.Leave;
               end;
               end
     else if   (Plugin__ is TGenerator) and Assigned(GeneratorTaskQueue) then with GeneratorTaskQueue do begin
               Plugin__.Enter;
               try     with StringGrid do begin
                         ItemsCheckBox.Enabled:=((RowCount>Succ(FixedRows))
                                                 or
                                                 (Assigned(Levels[FixedRows])
                                                  and
                                                  (not Levels[FixedRows].SnapshotsAsText.IsEmpty)
                                                  and
                                                  (not TSnapshotAsText(Levels[FixedRows].SnapshotsAsText.Last).MovesAsTextLines.IsEmpty)
                                                  // the level doesn't need a solution, hence, in contrast to the other task queues, the following 'not blank' test must be a part of the 'enabled' criterias
                                                  //and
                                                  //(TSnapshotAsText(Levels[FixedRows].SnapshotsAsText.Last).MovesAsTextLines.First.Text<>'')
                                                 )
                                                )
                                                and
                                                ((RowCount>Succ(FixedRows))
                                                 or
                                                 ((RowCount=Succ(FixedRows))
                                                  and
                                                  Assigned(Levels[FixedRows])
                                                  and
                                                  (not (ltfLocked in Levels[FixedRows].Tag.Flags))
                                                 )
                                                );
                         PopupMenuItemSort.Enabled:=RowCount>Succ(FixedRows);
                         end;
                       if PageControl1.ActivePage=TabSheetGenerator then begin
                          PluginEditMenuItemDeleteSolutions.Enabled:=SelectedCount<>0;
                          GeneratorPopupMenuItemToggleSelectionAboveItem.Enabled:=Row__>StringGrid.FixedRows;
                          GeneratorPopupMenuItemToggleSelectionBelowItem.Enabled:=Row__<Pred(StringGrid.RowCount);
                          PluginEditMenuItemStatistics.Visible:=Plugin__.StatisticsEnabled;
                          PluginEditMenuItemStatisticsSeparator.Visible:=PluginEditMenuItemStatistics.Visible;
                          if PluginEditMenuItemStatistics.Visible then
                             PluginEditMenuItemStatistics.Enabled:=(StatisticsTaskCount>0) and
                                                                   Assigned(StatisticsText) and
                                                                   (StatisticsText.Lines.Count>TASK_QUEUE_STATISTICS_HEADER_LINES_COUNT);
                          GeneratorEditMenuItemEdit.Enabled:=Assigned(Levels[Row__]);
                          GeneratorEditMenuItemPlay.Enabled:=GeneratorEditMenuItemEdit.Enabled;
                          PluginToolButtonGeneratorEdit.Enabled:=GeneratorEditMenuItemEdit.Enabled;
                          PluginToolButtonGeneratorPlay.Enabled:=GeneratorEditMenuItemPlay.Enabled;

                          GeneratorEditMenuItemCut.Enabled:=GeneratorTaskQueue.SelectedCount<>0;
                          GeneratorEditMenuItemCopy.Enabled:=GeneratorEditMenuItemCut.Enabled;
                          GeneratorEditMenuItemPaste.Enabled:=Clipboard.HasFormat(CF_TEXT) and (not Plugin__.IsActive);
                          GeneratorPopupMenuItemCut.Enabled:=GeneratorEditMenuItemCut.Enabled;
                          GeneratorPopupMenuItemCopy.Enabled:=GeneratorEditMenuItemCopy.Enabled;
                          GeneratorPopupMenuItemPaste.Enabled:=GeneratorEditMenuItemPaste.Enabled;
                          GeneratorPopupMenuItemEdit.Enabled:=GeneratorEditMenuItemEdit.Enabled;
                          GeneratorPopupMenuItemPlay.Enabled:=GeneratorEditMenuItemPlay.Enabled;
                          MainForm.Generator.ShowStatus;
                          end;
               finally Plugin__.Leave;
               end;
               end
          else PluginEditMenuItemCopySolution.Enabled:=False;

     PluginEditMenuItemDeleteSolutions.Enabled:=OptimizerTaskQueue.SelectedCount<>0;
     PluginEditMenuItemDeleteLevels       .Enabled:=((Plugin__ is TSolverPlugin   ) and (SolverTaskQueue   .SelectedCount<>0) and SolveLevelsGroupBox.Visible);
     GeneratorEditMenuItemDeleteCandidates.Enabled:=GeneratorEditMenuItemCut.Enabled;
     PluginToolButtonCut.Enabled:=GeneratorEditMenuItemCut.Enabled;
     PluginToolButtonDelete.Enabled:=((Plugin__ is TSolverPlugin   ) and PluginEditMenuItemDeleteLevels       .Enabled) or
                                     ((Plugin__ is TOptimizerPlugin) and PluginEditMenuItemDeleteSolutions    .Enabled) or
                                     ((Plugin__ is TGenerator      ) and GeneratorEditMenuItemDeleteCandidates.Enabled);

     PluginToolButtonAddAndDeleteSeparator.Visible:=PluginEditMenuItemDeleteSeparator.Visible or (Plugin__ is TGenerator);
     PopupMenuItemCopySolution.Enabled:=PluginEditMenuItemCopySolution.Enabled;
     PopupMenuItemPaste.Enabled:=PluginToolButtonPaste.Enabled;
     PopupMenuItemDeleteSolutions.Enabled:=PluginEditMenuItemDeleteSolutions.Enabled;
     PopupMenuItemDeleteLevels.Enabled:=PluginEditMenuItemDeleteLevels.Enabled;

     PluginMenuItemSort.Visible:=(PageControl1.ActivePage=TabSheetOptimizer) or (PageControl1.ActivePage=TabSheetSolver);
     PluginMenuItemSort.Enabled:=PopupMenuItemSort.Enabled;

     SettingsMenuItemWindowSizeMaximized.Checked:=Self.WindowState=wsMaximized;
     CaptureSettingsMenuItemWindowSizeMaximized.Checked := SettingsMenuItemWindowSizeMaximized.Checked;

     if Plugin__ is TGenerator then begin
        PluginToolButtonCopy.Enabled:=GeneratorEditMenuItemCopy.Enabled;
        GeneratorPopupMenuItemCut.Enabled:=GeneratorEditMenuItemCut.Enabled;
        GeneratorPopupMenuItemCopy.Enabled:=GeneratorEditMenuItemCopy.Enabled;
        GeneratorPopupMenuItemPaste.Enabled:=GeneratorEditMenuItemPaste.Enabled;
        GeneratorPopupMenuItemDeleteCandidates.Enabled:=GeneratorEditMenuItemDeleteCandidates.Enabled;
        end;
     end;
end;

procedure TToolsForm.StringGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var S:String;
begin
  if Sender is TStringGrid then with Sender as TStringGrid do begin
     Canvas.Font .Color:=Font.Color; //clWindowText;
     Canvas.Brush.Color:=Color; //clWindow;
     Canvas.FillRect(Rect);

     S:=Cells[ACol,ARow];
     if S<>'' then begin
        Dec(Rect.Right); Dec(Rect.Bottom);
        Windows.ExtTextOut(Canvas.Handle, Rect.Left + 2, Rect.Top + 2,
                           ETO_CLIPPED or ETO_OPAQUE, @Rect, PChar(S), Length(S), nil);
        Inc(Rect.Right); Inc(Rect.Bottom);
        end;
    end;
end;

procedure TToolsForm.OptimizeSolutionsStringGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var S:String;
begin
  with OptimizeSolutionsStringGrid do begin
    if ARow>=FixedRows then begin
       if ACol<>Ord(oscOptimization)
          then begin
          if ARow<>OptimizerTaskQueue.HighlightedRowNumber then begin
             if (ARow<>Row) then begin
                if (ActiveControl<>OptimizeSolutionsStringGrid) then begin
                   Canvas.Font .Color:=Font.Color; //clWindowText;
                   Canvas.Brush.Color:=Color; //clWindow;
                   end
                else begin
                   if (ARow<>Row) or (ACol<>Col) then begin
                      Canvas.Font .Color:=Font.Color; //clWindowText;
                      Canvas.Brush.Color:=Color; //clWindow;
                      end
                   else begin
                      Canvas.Font .Color:=clHighlightText;
                      Canvas.Brush.Color:=clHighlight;
                      end;
                   end;
                end
             else begin
                Canvas.Font .Color:=clHighlightText;
                Canvas.Brush.Color:=clHighlight;
                end;
             end
          else begin
             if //not ((ActiveControl=OptimizeSolutionsStringGrid) and (ACol=Col) and (ARow=Row)) then begin
                (ACol<>Col) or (ARow<>Row) then begin
                Canvas.Font .Color:=clInactiveCaption;
                Canvas.Brush.Color:=clInactiveCaptionText;
                end
             else begin
                Canvas.Font .Color:=clHighlightText;
                Canvas.Brush.Color:=clHighlight;
                end;
             end;
          end
       else
          if ARow<>OptimizerTaskQueue.HighlightedRowNumber then begin
             if True or (ARow<>Row) then begin
                if Cells[Ord(oscOptimization),ARow]<>'' then begin
                   Canvas.Font .Color:=clWindowText;
                   Canvas.Brush.Color:=clWindow;
                   end
                else begin
                   Canvas.Font .Color:=Font.Color; //clWindowText;
                   Canvas.Brush.Color:=Color; //clWindow;
                   end;
                end
             else begin
                if RowCount > Succ( FixedRows ) then begin
                   Canvas.Font .Color:=clHighlightText;
                   Canvas.Brush.Color:=clHighlight;
                   end
                else begin
                   Canvas.Font .Color:=clWindowText;
                   Canvas.Brush.Color:=clWindow;
                   end;
                end;
             end
          else begin
             if //not ((ActiveControl=OptimizeSolutionsStringGrid) and (ACol=Col) and (ARow=Row)) then begin
                (ACol<>Col) or (ARow<>Row) then begin
                Canvas.Font .Color:=clInactiveCaption;
                Canvas.Brush.Color:=clInactiveCaptionText;
                end
             else begin
                Canvas.Font .Color:=clHighlightText;
                Canvas.Brush.Color:=clHighlight;
                end;
             end;
       end
    else begin
       Canvas.Font .Color:=Font.Color; //clWindowText;
       Canvas.Brush.Color:=Color; //clWindow;
       end;

    Canvas.FillRect(Rect);

    S:=Cells[ACol,ARow];
    if S<>'' then begin
       Dec(Rect.Right); Dec(Rect.Bottom);

       Windows.SetBkMode(Canvas.Handle, Windows.OPAQUE);

       if      ARow<FixedRows then begin
               if      ACol=Ord(oscNo) then begin
                       Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_RIGHT or DT_SINGLELINE,nil);
                       //SetTextAlign(Canvas.Handle,TA_RIGHT);
                       //Windows.ExtTextOut(Canvas.Handle, Rect.Right-2, Rect.Top + 2,
                       //           ETO_CLIPPED or ETO_OPAQUE, @Rect, PChar(S), Length(S), nil);
                       end
               else if True or ((ACol<>Ord(oscLevelName)) and (ACol<>Ord(oscSnapShotName))) then begin
                       Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_CENTER or DT_SINGLELINE,nil);
                       //SetTextAlign(Canvas.Handle,TA_CENTER);
                       //Windows.ExtTextOut(Canvas.Handle, Rect.Left + (Rect.Right-Rect.Left) div 2, Rect.Top + 2,
                       //                   ETO_CLIPPED or ETO_OPAQUE, @Rect, PChar(S), Length(S), nil);
                       end
                    else begin
                       Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_LEFT or DT_SINGLELINE,nil);
                       //SetTextAlign(Canvas.Handle,TA_LEFT);
                       //Windows.ExtTextOut(Canvas.Handle, Rect.Left + 2, Rect.Top + 2,
                       //                   ETO_CLIPPED or ETO_OPAQUE, @Rect, PChar(S), Length(S), nil);
                       end;
               end
       else begin
          if ACol<>Ord(oscNo) then begin
             Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_LEFT or DT_SINGLELINE,nil);
             //SetTextAlign(Canvas.Handle,TA_LEFT);
             //Windows.ExtTextOut(Canvas.Handle, Rect.Left + 2, Rect.Top + 2,
             //                   ETO_CLIPPED or ETO_OPAQUE, @Rect, PChar(S), Length(S), nil);
             end
          else begin
             Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_RIGHT or DT_SINGLELINE,nil);
             //SetTextAlign(Canvas.Handle,TA_RIGHT);
             //Windows.ExtTextOut(Canvas.Handle, Rect.Right-2, Rect.Top + 2,
             //                   ETO_CLIPPED or ETO_OPAQUE, @Rect, PChar(S), Length(S), nil);
             end;
          end;
       Inc(Rect.Right); Inc(Rect.Bottom);
       end;

    if (ARow=Row) and (ACol=Col) and (ActiveControl=OptimizeSolutionsStringGrid) then begin
       Canvas.Brush.Color:=clHighlightText;
       Canvas.FrameRect(Rect);
       with Rect do Canvas.FrameRect(Classes.Rect(Succ(Left),Succ(Top),Pred(Right),Pred(Bottom)));
       end;

    Canvas.Font .Color:=Font.Color; //clWindowText;
    Canvas.Brush.Color:=Color; //clWindow;
    end;
end;

procedure TToolsForm.GenerateLevelsStringGridDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var S:String;
begin
  with GenerateLevelsStringGrid do begin
    if ARow>=FixedRows then begin
       if ARow<>GeneratorTaskQueue.HighlightedRowNumber then begin
          if ARow<>Row then begin
             if (ActiveControl<>GenerateLevelsStringGrid) then begin
                Canvas.Font .Color:=Font.Color; //clWindowText;
                Canvas.Brush.Color:=Color; //clWindow;
                end
             else begin
                if (ARow<>Row) or (ACol<>Col) then begin
                   Canvas.Font .Color:=Font.Color; //clWindowText;
                   Canvas.Brush.Color:=Color; //clWindow;
                   end
                else begin
                   Canvas.Font .Color:=clHighlightText;
                   Canvas.Brush.Color:=clHighlight;
                   end;
                end;
             end
          else begin
             Canvas.Font .Color:=clHighlightText;
             Canvas.Brush.Color:=clHighlight;
             end;
          end
       else begin
          if //not ((ActiveControl=GenerateLevelsStringGrid) and (ACol=Col) and (ARow=Row)) then begin
             (ACol<>Col) or (ARow<>Row) then begin
             Canvas.Font .Color:=clInactiveCaption;
             Canvas.Brush.Color:=clInactiveCaptionText;
             end
          else begin
             Canvas.Font .Color:=clHighlightText;
             Canvas.Brush.Color:=clHighlight;
             end;
          end;
       end
    else begin
       Canvas.Font .Color:=Font.Color; //clWindowText;
       Canvas.Brush.Color:=Color; //clWindow;
       end;

    Canvas.FillRect(Rect);

    S:=Cells[ACol,ARow];
    if S<>'' then begin
       Dec(Rect.Right); Dec(Rect.Bottom);
       Windows.SetBkMode(Canvas.Handle, Windows.OPAQUE);

       if   ARow<FixedRows then
            if        ACol=Ord(glcNo) then
                      Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_RIGHT or DT_SINGLELINE,nil)
            else if   (ACol>=Ord(glcPushes)) then //(ACol=Ord(glcPushes)) or (ACol=Ord(glcScore)) then
                      Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_CENTER or DT_SINGLELINE,nil)
                 else //Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_CENTER or DT_SINGLELINE,nil)
                      Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_LEFT  or DT_SINGLELINE,nil)
       else if   (ACol=Ord(glcNo)) or (ACol>=Ord(glcPushes)) then
                 Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_RIGHT or DT_SINGLELINE,nil)
            else Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_LEFT  or DT_SINGLELINE,nil);
       Inc(Rect.Right); Inc(Rect.Bottom);
       end;

    if (ARow=Row) and (ACol=Col) and (ActiveControl=GenerateLevelsStringGrid) then begin
       Canvas.Brush.Color:=clHighlightText;
       Canvas.FrameRect(Rect);
       with Rect do Canvas.FrameRect(Classes.Rect(Succ(Left),Succ(Top),Pred(Right),Pred(Bottom)));
       end;

    Canvas.Font .Color:=Font.Color; //clWindowText;
    Canvas.Brush.Color:=Color; //clWindow;
    end;
end;

function  TToolsForm.ClickToFocusHighlightedRow(DoIt__:Boolean):Boolean;
var b:Boolean; CurrentTaskQueue:TTaskQueue;
begin
  Result:=False;
  with PluginLevelFileNamePanel do
    if (Caption<>'') and Assigned(Game) and (not Game.IsBusy) and (not Game.IsReplaying) and
       Assigned(OpenForm) then begin
       if        ((PluginForCurrentTabSheet is TSolverPlugin   ) and  SolveLevelsGroupBox      .Visible) then
                 CurrentTaskQueue:=SolverTaskQueue
       else if   ((PluginForCurrentTabSheet is TOptimizerPlugin) and  OptimizeSolutionsGroupBox.Visible) then
                 CurrentTaskQueue:=OptimizerTaskQueue
       else if   ((PluginForCurrentTabSheet is TGenerator      ) and  GenerateLevelsGroupBox   .Visible) then
                 CurrentTaskQueue:=GeneratorTaskQueue
            else CurrentTaskQueue:=nil;
       if   Assigned(CurrentTaskQueue) then with CurrentTaskQueue do
            if Assigned(Plugin) then with StringGrid do begin
               Plugin.Enter;
               try     if (HighlightedRowNumber>=FixedRows) and
                          ((HighlightedRowNumber< TopRow) or (HighlightedRowNumber >=TopRow+VisibleRowCount)) then begin
                          Result:=True;
                          if DoIt__ then begin
                             ScrollInView(HighlightedRowNumber);
                             Row:=HighlightedRowNumber;
                             if        Plugin is TOptimizerPlugin then
                                       ToolsForm.OptimizeSolutionsStringGridSelectCell(nil,StringGrid.Col,StringGrid.Row,b)
                             else if   Plugin is TSolverPlugin then
                                       ToolsForm.SolveLevelsStringGridSelectCell      (nil,StringGrid.Col,StringGrid.Row,b)
                             else if   Plugin is TGenerator then
                                       ToolsForm.GenerateLevelsStringGridSelectCell   (nil,StringGrid.Col,StringGrid.Row,b);
                             end
                          end;
               finally Plugin.Leave;
               end;
               end;
       end;
end;

procedure TToolsForm.SolveLevelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var Col,Row,PanelWidth:Integer; ClearStatusText:Boolean;
begin
  if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;

  ClearStatusText:=not ((Sender=PluginLevelFileNamePanel) and ClickToFocusHighlightedRow(False));
  with PluginLevelFileNamePanel do
    if ClearStatusText then begin
       if Hint<>'' then begin
          Hint:=''; StatusText:='';
          end;
       end
    else begin
       Hint:=HintFocusTaskText; StatusText:=GetLongHint(Hint);
       end;

  if ClearStatusText and (StatusText<>'') then StatusText:='';

  if Assigned(OpenForm) and OpenForm.ToolTips.Enabled then with PanelToolTips do begin
     if      Sender is TPanel then begin

             if   Caption<>TPanel(Sender).Caption then begin
                  PanelWidth:=Self.Canvas.TextWidth(TPanel(Sender).Caption)+8;
                  Caption:=''; Width:=1;
                  end
             else PanelWidth:=Width;

             if Sender<>LevelNamePanel then
                Left:=Min(TPanel(Sender).Left+X+OpenForm.ToolTips.OffsetX+TPanel(Sender).Parent.Left+PageControl1.ActivePage.Left,
                          PageControl1.ActivePage.Left+PageControl1.ActivePage.ClientWidth-PanelWidth-2)
             else with TPanel(Sender) do
                PanelToolTips.Left:=Min(Min(Left+X+OpenForm.ToolTips.OffsetX+Parent.Left+PageControl1.ActivePage.Left-PanelWidth,
                                            Left+ClientWidth+Parent.Left+PageControl1.ActivePage.Left-PanelWidth), // the inner 'Min()' may be obsolete after adding the outer 'Min()', but better safe than sorry
                                        PageControl1.ActivePage.Left+PageControl1.ActivePage.ClientWidth-PanelWidth-2);
             Top :=TPanel(Sender).Top +Y+OpenForm.ToolTips.OffsetY+TPanel(Sender).Parent.Top +PageControl1.ActivePage.Top;
             if   Caption<>TPanel(Sender).Caption then begin
                  Caption:=TPanel(Sender).Caption;
                  Width:=PanelWidth;
                  end;
             end
     else if Sender is TStringGrid then begin
             TStringGrid(Sender).MouseToCell(X,Y,Col,Row);
             if (Col>=0) and (Col<TStringGrid(Sender).ColCount) and
                (Row>=0) and (Row<TStringGrid(Sender).RowCount) and
                (((Sender=PluginLevelStringGrid)
                  and
                  (Row=Ord(pcbiiStatus))
                  and
                  (Col=1)
                 )
                 or
                 ((Sender=OptimizeSolutionsStringGrid)
                  and
                  (Col>=Ord(oscLevelName))
                  and
                  (Row>=0)
                 )
                 or
                 ((Sender=GenerateLevelsStringGrid)
                  and
                  (Col>=Ord(glcLevelName))
                  and
                  (Row>=0)
                 )
                ) then begin
                if Caption<>TStringGrid(Sender).Cells[Col,Row] then begin
                   Caption:=TStringGrid(Sender).Cells[Col,Row];
                   Width:=Canvas.TextWidth(Caption)+8;
                   end;
                Left:=Min(TStringGrid(Sender).Left+X+OpenForm.ToolTips.OffsetX+TStringGrid(Sender).Parent.Left+PageControl1.ActivePage.Left+TStringGrid(Sender).Parent.Parent.Left,
                          PageControl1.ActivePage.Left+PageControl1.ActivePage.ClientWidth-Width-2
                         );
                Top :=TStringGrid(Sender).Top +Y+OpenForm.ToolTips.OffsetY+TStringGrid(Sender).Parent.Top +PageControl1.ActivePage.Top +TStringGrid(Sender).Parent.Parent.Top;
                end
             else Caption:='';
             end
          else Caption:='';
     if   (Caption<>'') and (not Game.IsBusy) then begin
          if      (Sender=LevelNamePanel) and (PageControl1.ActivePage=TabSheetOptimizer) and PluginToolButtonReplay.Enabled then // try to enter browse mode instead of showing the level name
                  if Visible then Hide
                  else
          else if not Visible then Show
               else
          end
     else if Visible then Hide;
     end
  else if PanelToolTips.Visible then PanelToolTips.Hide;
end;

function TToolsForm.EnterBrowseMode:Boolean;
begin
  Result:=Assigned(Game)
          and
          (Game.IsBrowsing
           or
           ((PageControl1.ActivePage=TabSheetOptimizer)
            and
            PluginToolButtonReplay.Enabled
            and
            (not Game.IsBusy)
            and
            (PluginLevelInfo.ReplayInfo.IsLoaded
             or
             (Replay([roPosition],0)
              and
              PluginLevelInfo.ReplayInfo.IsLoaded))
            and
            OpenForm.Game.EnterBrowseMode(False)));
  if Result then begin
     Game.IsBrowsing:=True; Game.IsBusy:=True; ImageBoard.Tag:=0;
     PluginLevelInfo.ReplayInfo.SelectedRange[2]:=OpenForm.Game.MaxBrowsePosition;
     if PluginLevelInfo.SelectedSquaresAsText<>'' then with OpenForm.Game do begin
        SelectedSquaresAsTextToBoard('',BoardWidth,BoardHeight,Board);
        LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,LevelSetForm.GameViewer.BoardImage,False);
        end;
     ShowOptimizationRange(-1);
     end
  else if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
end;

procedure TToolsForm.LeaveBrowseMode(RestorePosition:Boolean);
var Level:TLevel;
begin
  if Game.IsBrowsing then begin
     Game.IsBrowsing:=False; Game.IsReplaying:=False; Game.IsBusy:=False;
     OpenForm.Game.LeaveBrowseMode(RestorePosition);
     with OpenForm.Game do begin
       if (PluginLevelInfo.SelectedSquaresAsText<>'') then
          SelectedSquaresAsTextToBoard(PluginLevelInfo.SelectedSquaresAsText,BoardWidth,BoardHeight,Board);
       LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,LevelSetForm.GameViewer.BoardImage,False);
       end;
     PluginLevelInfo.ReplayInfo.MoveCount:=OpenForm.Game.History.Count;
     if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;

     if (ImageReplaySpeed.Tag and 1) <> 0 then with PluginLevelInfo.ReplayInfo do begin // 'True': mouse down; update the selected range
        if      (ImageReplaySpeed.Tag and 2) = 2 then // 'True': setting start of range
                if   SelectedRange[0] >= SelectedRange[1] then begin
                     SelectedRange[1] := Succ(OpenForm.Game.MaxBrowsePosition);
                     SelectedRange[6] := 0; // clear repeat interval size
                     end
                else
        else if (ImageReplaySpeed.Tag and 4) = 4 then // 'True': setting end of range
                if SelectedRange[0] >= SelectedRange[1] then SelectedRange[0] := 0;
        if SelectedRange[0]         >= Min(SelectedRange[1],OpenForm.Game.MaxBrowsePosition) then begin
           SelectedRange[0]         := 0;
           SelectedRange[1]         := 0;
           SelectedRange[6]         := 0; // clear repeat interval size
           end;
        SelectedRange[2]            := Max(1,OpenForm.Game.MaxBrowsePosition); // remember the range, so it's unnecessary to load the solution just to display the [start, end] slice
        if SelectedRange[1]         >= SelectedRange[2] then begin // 'True': selected end of range >= end-of-game
           if   SelectedRange[0]    =  0 then // 'True': all moves selected; clear selection
                SelectedRange[1]    := 0
           else SelectedRange[1]    := Succ(OpenForm.Game.MaxBrowsePosition); // ensure that the last push is included in the range of pushes to be optimized by overshooting the range
           SelectedRange[6]         := 0; // clear repeat interval size
           end;
        if SelectedRange[6]         >  0 then // 'True': interval optimization is enabled
           SelectedRange[6]         := SelectedRange[1]-SelectedRange[0]; // update interval size

        if IsLoaded then begin
           if Assigned(OptimizerTaskQueue.Plugin) then
              OptimizerTaskQueue.Plugin.Enter;
           try
             Level:=OptimizerTaskQueue.Levels[OptimizerTaskQueue.StringGrid.Row];
                  if Assigned(Level) and (not Level.SnapshotsAsText.IsEmpty) and
                     (Level.SnapshotsAsText.Last is TExtendedSnapshotAsText) then
                     with TExtendedSnapshotAsText(Level.SnapshotsAsText.Last) do begin
                       if OptimizerTaskQueue.StringGrid.Row <> OptimizerTaskQueue.HighlightedRowNumber then begin
                          FillChar( SelectedRange, SizeOf( SelectedRange ), 0 );
                          SelectedRange[0] := PluginLevelInfo.ReplayInfo.SelectedRange[0];
                          SelectedRange[1] := PluginLevelInfo.ReplayInfo.SelectedRange[1];
                          SelectedRange[2] := PluginLevelInfo.ReplayInfo.SelectedRange[2];
                          SelectedRange[6] := PluginLevelInfo.ReplayInfo.SelectedRange[6];
                          end
                       else begin // optimization is in progress; restore the range on the screen to the actually used range of moves, if any
                          PluginLevelInfo.ReplayInfo.SelectedRange[0] := SelectedRange[0];
                          PluginLevelInfo.ReplayInfo.SelectedRange[1] := SelectedRange[1];
                          PluginLevelInfo.ReplayInfo.SelectedRange[2] := SelectedRange[2];
                          PluginLevelInfo.ReplayInfo.SelectedRange[6] := SelectedRange[6];
                          end;
                       end;
           finally if Assigned(OptimizerTaskQueue.Plugin) then
                      OptimizerTaskQueue.Plugin.Leave;
           end;
           end;
        end;

     with PluginLevelInfo.ReplayInfo do
       if (SelectedRange[1]-SelectedRange[0] >= OpenForm.Game.MaxBrowsePosition) and
          ((SelectedRange[0]<>1) or (SelectedRange[1]<>Succ(OpenForm.Game.MaxBrowsePosition)) or (OpenForm.Game.MaxBrowsePosition=0)) then
          FillChar( SelectedRange, SizeOf( SelectedRange ), 0 ); // no subinterval specified

     ImageBoard.Tag:=0;
     ImageReplaySpeed.Tag:=0;
     ShowOptimizationRange(-1);
     ImageReplaySpeed.Hint:=HintToolsWindowReplaySpeedText;
     if OptimizerTaskQueue.StringGrid.Visible then
        OptimizerTaskQueue.StringGrid.SetFocus;
     ShowStatus;
     end;
end;

procedure TToolsForm.BrowseMouseMove(X,Y:Integer);
var i,j,Position:Integer;
begin
  if Game.IsBrowsing then begin
     Y:=Max(1,ImageReplaySpeed.ClientWidth-2*OPTIMIZATION_RANGE_MARGIN); // use 'Y' as width
     Position:=Max(0,Min(X-OPTIMIZATION_RANGE_MARGIN,ImageReplaySpeed.ClientWidth-OPTIMIZATION_RANGE_MARGIN));
     Position:=Max(0,Min(OpenForm.Game.MaxBrowsePosition,
                              (OpenForm.Game.MaxBrowsePosition*
                               ((Position)
                                +
                                (Y                                              // try to smooth things out a bit
                                 div                                            // when the number of positions are
                                 (2*Max(1,OpenForm.Game.MaxBrowsePosition))     // low by adding half the size of
                                )                                               // each position
                               )
                              ) div Y
                             ));
     if Position<>OpenForm.Game.BrowsePosition then begin
        OpenForm.Game.BrowsePosition:=Position;
        with OpenForm.Game do LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,LevelSetForm.GameViewer.BoardImage,False);
        end;
     StatusBar1.Panels[0].Text:=IntToStr(OpenForm.Game.History.Count)+SLASH+IntToStr(OpenForm.Game.History.PushCount);   
     if PluginLevelInfo.ReplayInfo.SelectedRange[1] =  0 then
        PluginLevelInfo.ReplayInfo.SelectedRange[1] := Succ(OpenForm.Game.MaxBrowsePosition);
     X:=OPTIMIZATION_RANGE_MARGIN+((OpenForm.Game.BrowsePosition*Max(1,(ImageReplaySpeed.ClientWidth-2*OPTIMIZATION_RANGE_MARGIN))) div Max(1,OpenForm.Game.MaxBrowsePosition));

     if (ImageReplaySpeed.Tag and 1)=0 then begin // 'True': not mouse down
        i:=OPTIMIZATION_RANGE_MARGIN+((PluginLevelInfo.ReplayInfo.SelectedRange[0]*Max(1,(ImageReplaySpeed.ClientWidth-2*OPTIMIZATION_RANGE_MARGIN))) div Max(1,OpenForm.Game.MaxBrowsePosition));
        j:=OPTIMIZATION_RANGE_MARGIN+((PluginLevelInfo.ReplayInfo.SelectedRange[1]*Max(1,(ImageReplaySpeed.ClientWidth-2*OPTIMIZATION_RANGE_MARGIN))) div Max(1,OpenForm.Game.MaxBrowsePosition));
        if   X<=i+((j-i) div 2) then
             ImageReplaySpeed.Tag:=2  // the start of range is closer
        else ImageReplaySpeed.Tag:=4; // the end   of range is closer
        if   OptimizerTaskQueue.StringGrid.Row <> OptimizerTaskQueue.HighlightedRowNumber then
             ImageReplaySpeed.Hint:=HintSelectOptimizationRangeText[ImageReplaySpeed.Tag=4]
        else ImageReplaySpeed.Hint:='';
        end
     else begin
        if Assigned( OptimizerTaskQueue.Plugin ) then OptimizerTaskQueue.Plugin.Enter;
        try
          if   OptimizerTaskQueue.StringGrid.Row <> OptimizerTaskQueue.HighlightedRowNumber then begin
               if      ImageReplaySpeed.Tag = 3 then PluginLevelInfo.ReplayInfo.SelectedRange[0]:=OpenForm.Game.BrowsePosition  // start of range, and mouse down
               else if ImageReplaySpeed.Tag = 5 then PluginLevelInfo.ReplayInfo.SelectedRange[1]:=OpenForm.Game.BrowsePosition; // end   of range, and mouse down
               end
          else ImageReplaySpeed.Hint:='';
        finally if Assigned( OptimizerTaskQueue.Plugin ) then OptimizerTaskQueue.Plugin.Leave;
        end;
        end;
     StatusText:=ImageReplaySpeed.Hint;
     ShowOptimizationRange(X);
     end;
end;

procedure TToolsForm.ShowOptimizationRange(CursorPosition:Integer);
var i,j,k:Integer; // RightJustifiedCaption: Boolean;
    s:String; R:TRect;

  procedure DrawLeftMargin( Position__ : Integer; DrawArrow__ : Boolean );
  var i,n,HalfHeight:Integer; R:TRect;
  begin
    with ImageReplaySpeed do with Picture.BitMap.Canvas do begin
      if   DrawArrow__ then
           HalfHeight:=ClientHeight div 2
      else HalfHeight:=0;
      Brush.Color:=clBtnShadow;
      Pen.Color:=Brush.Color;
      R:=Rect(0,0,Max(0,Position__-HalfHeight),ClientHeight);
      FillRect(R); // draw solid rectangle to the left of the arrow
      n:=0; // draw right arrow
      for i:=0 to Pred(ClientHeight) do begin
          if   i<= HalfHeight then
               Inc(n)
          else Dec(n);
          MoveTo(R.Right,i);
          LineTo(Min(R.Right+n,Succ(Position__)),i);
          end;
      end;
  end;

  procedure DrawRightMargin( Position__ : Integer; DrawArrow__ : Boolean );
  var i,n,HalfHeight:Integer; R:TRect;
  begin
    with ImageReplaySpeed do with Picture.BitMap.Canvas do begin
      if   DrawArrow__ then
           HalfHeight:=ClientHeight div 2
      else HalfHeight:=0;
      Brush.Color:=clBtnShadow;
      Pen.Color:=Brush.Color;
      R:=Rect(Min(Position__+HalfHeight,ClientWidth),0,ClientWidth,ClientHeight);
      FillRect(R); // draw solid rectangle to the right of the arrow
      n:=0; // draw left arrow
      for i:=0 to Pred(ClientHeight) do begin
          if   i<= HalfHeight then
               Inc(n)
          else Dec(n);
          MoveTo(R.Left,i);
          LineTo(Max(R.Left-n,Pred(Position__)),i);
          end;
      end;
  end;

begin // ShowOptimizationRange
  if   PageControl1.ActivePage=TabSheetOptimizer then with ImageReplaySpeed do with Picture.BitMap.Canvas do begin
       if Top=0 then begin // 'True': the image hasn't been properly aligned to the parent size yet; do it now;
          with ImageReplaySpeed do begin Left:=2; Top:=2; Width:=Parent.ClientWidth-4; Height:=Parent.ClientHeight-4; end;
          ResizeImage(ImageReplaySpeed);
          end;

       with PluginLevelInfo.ReplayInfo do
         if SelectedRange[2] > 0 then begin // 'True': has range
            i:=OPTIMIZATION_RANGE_MARGIN+((SelectedRange[0]*Max(1,(ClientWidth-2*OPTIMIZATION_RANGE_MARGIN))) div SelectedRange[2]);
            if   SelectedRange[1]>0 then
                 j:=OPTIMIZATION_RANGE_MARGIN+((Min(SelectedRange[1],SelectedRange[2])*Max(1,(ClientWidth-2*OPTIMIZATION_RANGE_MARGIN))) div SelectedRange[2])
            else j:=ClientWidth;
            end
         else begin
            i:=0; j:=ClientWidth;
            end;

       Brush.Style:=bsSolid; Pen.Style:=psSolid;
       Brush.Color:=LevelNamePanel.Color;
       R:=Rect(0,0,ClientWidth,ClientHeight);
       FillRect(R);

       if (CursorPosition>0) and (CursorPosition<ClientWidth) then begin
          if      (Tag and 2)<>0 then // start of range
                  DrawLeftMargin(CursorPosition, True)
          else if (Tag and 4)<>0 then // end of range
                  DrawRightMargin(CursorPosition, True);
          end
       else if (CursorPosition<0) and
               //(j-i<ClientWidth-(2*OPTIMIZATION_RANGE_MARGIN)) and // this isn't accurate enough; instead, check the selected range;
               (PluginLevelInfo.ReplayInfo.SelectedRange[2]>0) and (PluginLevelInfo.ReplayInfo.SelectedRange[0]<PluginLevelInfo.ReplayInfo.SelectedRange[1]) and
               (not Game.IsBrowsing) then begin
               DrawLeftMargin(i,False);
               R:=Rect(i,0,j,ClientHeight);
               Brush.Color:=clHighlightText;
               FillRect(R);
               DrawRightMargin(j,False);
               if PluginLevelInfo.ReplayInfo.SelectedRange[6]>0 then begin
                  k:=RectHeight(R) div 2;
                  DrawArrow( Picture.BitMap.Canvas, R.Right+k, R.Top+k, k, SokUtil_.Right, clHighlightText, $ffffff );
                  end;
               end;

       R:=Rect(0,0,ClientWidth,ClientHeight);
       if   Assigned(Game) and Game.IsBrowsing and (CursorPosition>=0) then begin
            s:=StatusBar1.Panels[0].Text;
            if   (Tag and 4)=0 then
                 R.Left:=Max(0, Min(CursorPosition, R.Right-TextWidth(s)))
            else R.Left:=Max(0, CursorPosition-TextWidth(s));
            end
       else s:=LevelNamePanel.Caption;

       Brush.Color:=LevelNamePanel.Color;
       Font.Color:=LevelNamePanel.Font.Color;
       Windows.SetBkMode (Canvas.Handle,Windows.TRANSPARENT);
       DrawText(Handle,PChar(s),Length(s),R,DT_LEFT+DT_VCENTER+DT_SINGLELINE);
       Windows.SetBkMode (Canvas.Handle,Windows.OPAQUE);

       if   ((Tag and 2)=0) or (OptimizerTaskQueue.StringGrid.Row=OptimizerTaskQueue.HighlightedRowNumber) then
            DrawArrow( Picture.BitMap.Canvas, Max(i,            OPTIMIZATION_RANGE_MARGIN), 3, 4, Down, clBtnText  , $ffffff )  //clBtnShadow )
       else DrawArrow( Picture.BitMap.Canvas, Max(i,            OPTIMIZATION_RANGE_MARGIN), 3, 4, Down, clHighlight, $ffffff ); //clBtnShadow )
       if   ((Tag and 4)=0) or (OptimizerTaskQueue.StringGrid.Row=OptimizerTaskQueue.HighlightedRowNumber) then
            DrawArrow( Picture.BitMap.Canvas, Min(j,ClientWidth-OPTIMIZATION_RANGE_MARGIN), 3, 4, Down, clBtnText  , $ffffff )  //clBtnShadow )
       else DrawArrow( Picture.BitMap.Canvas, Min(j,ClientWidth-OPTIMIZATION_RANGE_MARGIN), 3, 4, Down, clHighlight, $ffffff ); //clBtnShadow )

       if   PluginLevelInfo.ReplayInfo.MovesAsText <> '' then begin
            if (not ImageReplaySpeed.Visible) and
               PluginLevelInfo.ReplayInfo.IsLoaded then
               ImageReplaySpeed.Show;
            end
       else if ImageReplaySpeed.Visible then ImageReplaySpeed.Hide;
       end;
end;

function  TToolsForm.CreateLevelFromSolutionSlice( var Level__ : TLevel ) : Boolean;
var Index, FromMove, MoveCount, PushCount, BoxNo, Col, Row : Integer;
    PlayerFromPos,PlayerToPos, BoxFromPos, BoxToPos : TColRow;
    ABoard : TBoard;
    SnapshotAsText : TSnapshotAsText;
begin
  Result := False;
  Level__ := nil;

  if (not PluginLevelInfo.ReplayInfo.IsLoaded ) and // 'True': the solution hasn't been loaded for replay in the thumbnail viewer; do it now
     EnterBrowseMode then
     LeaveBrowseMode( True );

  if PluginLevelInfo.ReplayInfo.IsLoaded and
     ( PluginLevelInfo.ReplayInfo.SelectedRange[0] < PluginLevelInfo.ReplayInfo.SelectedRange[1]) and
     ( PluginLevelInfo.ReplayInfo.SelectedRange[2] > 0 ) and
     ( Length( PluginLevelInfo.ReplayInfo.MovesAsText ) = OpenForm.Game.History.Top ) and
     Assigned(MainForm.Optimizer) and
     CreateObject( otLevel, TNode( Level__ ) ) then
     try     OpenForm.Game.BrowsePosition := PluginLevelInfo.ReplayInfo.SelectedRange[0];
             ABoard := OpenForm.Game.Board;
             for Index := 1 to OpenForm.Game.GoalCount do with OpenForm.Game.GoalPos[ Index ] do
                 ABoard[ X, Y ] := ABoard[ X, Y ] and ( not GOAL );
             FromMove  := OpenForm.Game.History.Count;
             PushCount := OpenForm.Game.History.PushCount;

             if MainForm.CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles then
                Result := Level__.SetName( Trim( OptimizerTaskQueue.StringGrid.Cells[ Ord( oscLevelName    ), OptimizerTaskQueue.StringGrid.Row] ) +
                                           COMMA + SPACE +
                                           Trim( OptimizerTaskQueue.StringGrid.Cells[ Ord( oscSnapshotName ), OptimizerTaskQueue.StringGrid.Row] ) +
                                           COMMA + SPACE + LEFT_BRACE + IntToStr( FromMove ) + SLASH + IntToStr( PushCount ) )
             else Result := Level__.SetName( SPACE );

             if Result then
                if   MainForm.Optimizer.WallifyBoxesOutsideRangeOfPushesToBeOptimized then with OpenForm.Game do begin
                     for Col := 1 to BoardWidth do
                         for Row := 1 to BoardHeight do
                            ABoard[ Col, Row ] := ABoard[ Col, Row ] and ( not SQUARE_SET ); // clear 'SQUARE_SET' flags, if any

{                    // using 'SetBrowsePosition' suffers from exponential growth, so long solutions makes the program stop responding;
                     // instead, 'Redo0' is used directly; see further down;
                     while Result and
                           ( BrowsePosition < Min( PluginLevelInfo.ReplayInfo.SelectedRange[ 1 ], PluginLevelInfo.ReplayInfo.SelectedRange[ 2 ] ) ) do begin // replay all pushes in the selected interval
                       Index := BrowsePosition;
                       BrowsePosition := Succ( BrowsePosition );
                       if BrowsePosition = Succ( Index ) then begin
                          UnpackLastMove( PlayerFromPos, PlayerToPos, BoxFromPos, BoxToPos, BoxNo );
                          if BoxNo > 0 then begin
                             ABoard[ BoxFromPos.X, BoxFromPos.Y ] := ABoard[ BoxFromPos.X, BoxFromPos.Y ] or SQUARE_SET;
                             ABoard[ BoxToPos  .X, BoxToPos  .Y ] := ABoard[ BoxToPos  .X, BoxToPos  .Y ] or SQUARE_SET;
                             end;
                          end
                       else Result := False; // something went wrong with the browsing position adjustment
                       end;
}
                     Index := Min( PluginLevelInfo.ReplayInfo.SelectedRange[ 1 ], PluginLevelInfo.ReplayInfo.SelectedRange[ 2 ] );
                     while Result and ( History.PushCount < Index ) do begin // replay all pushes in the selected interval
                       if Redo0( False ) then begin
                          UnpackLastMove( PlayerFromPos, PlayerToPos, BoxFromPos, BoxToPos, BoxNo );
                          if BoxNo > 0 then begin
                             ABoard[ BoxFromPos.X, BoxFromPos.Y ] := ABoard[ BoxFromPos.X, BoxFromPos.Y ] or SQUARE_SET;
                             ABoard[ BoxToPos  .X, BoxToPos  .Y ] := ABoard[ BoxToPos  .X, BoxToPos  .Y ] or SQUARE_SET;
                             end;
                          end
                       else Result := False;
                       end;

                     for Col := 1 to BoardWidth do
                         for Row := 1 to BoardHeight do
                             if ( Board[ Col, Row ] and BOX ) <> 0 then                // 'True': a box position at the end of the slice
                                if   ( ABoard[ Col, Row ] and SQUARE_SET ) = 0 then    // 'True': the box hasn't been pushed in the selected range of pushes
                                     ABoard[ Col, Row ] := WALL                        // turn the box into a wall
                                else ABoard[ Col, Row ] := ABoard[ Col, Row ] or GOAL; // turn the box position into a target square
                     end
                else begin OpenForm.Game.BrowsePosition := PluginLevelInfo.ReplayInfo.SelectedRange[1];
                           for Index := 1 to OpenForm.Game.BoxCount do with OpenForm.Game.BoxPos[ Index ] do
                               ABoard[ X, Y ] := ABoard[ X, Y ] or GOAL;
                     end;

             MoveCount := OpenForm.Game.History.Count     - FromMove;
             PushCount := OpenForm.Game.History.PushCount - PushCount;

             Result    := Result and
                          Level__.BoardToTextLines( OpenForm.Game.BoardWidth, OpenForm.Game.BoardHeight, ABoard );
             if MainForm.CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles then
                Result := Result and
                          Level__.SetName( Level__.Name + SPACE + HYPHEN + SPACE + IntToStr( OpenForm.Game.History.Count ) + SLASH + IntToStr( OpenForm.Game.History.PushCount ) + RIGHT_BRACE );

             if Result and ( MoveCount > 0 ) then begin
                Result := CreateObject( otSnapshotAsText, TNode( SnapshotAsText ) );
                if Result then Level__.SnapshotsAsText.Add( TNode( SnapshotAsText ) );
                Result := Result and
                          SnapshotAsText.MovesToTextLines( PHistoryMoves( Addr( OpenForm.Game.History.Moves[ FromMove ] ) ), MoveCount, MoveCount, False, True, False, False );
                if MainForm.CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles then
                   Result := Result and
                             Assigned( SnapshotsForm ) and
                             SnapshotAsText.SetName( SnapshotsForm.SolutionName + SPACE + IntToStr( MoveCount ) + SLASH + IntToStr( PushCount ) )
                else Result := Result and
                               SnapshotAsText.SetName( SPACE );
                end;

     finally if not Result then begin
                Level__.Free; Level__ := nil;
                end;
     end;
end;

procedure TToolsForm.PluginEditMenuItemCopyLevelCreatedFromSolutionSliceClick(
  Sender: TObject);
var Result : Boolean; Text : String; Level : TLevel;
begin
  Result := CreateLevelFromSolutionSlice( Level );
  if Result then
     try     Result := Level.ToText( Text );
             if Result then Clipboard.AsText := Text;
     finally Level.Free;
     end;
  if not Result then
     Msg( TEXT_TASK_FAILED, Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+Trim(ToolsForm.PageControl1.ActivePage.Caption), MB_ICONINFORMATION + MB_OK );
end;

procedure TToolsForm.PluginEditMenuItemRepeatSolutionSlicingClick(
  Sender: TObject);
begin
  if (not PluginLevelInfo.ReplayInfo.IsLoaded ) and // 'True': the solution hasn't been loaded for replay in the thumbnail viewer; do it now
     EnterBrowseMode then
     LeaveBrowseMode( True );

  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then begin
     if Assigned( OptimizerTaskQueue.Plugin ) then OptimizerTaskQueue.Plugin.Enter;
     try
       if OptimizerTaskQueue.StringGrid.Row <> OptimizerTaskQueue.HighlightedRowNumber then begin
          if PluginLevelInfo.ReplayInfo.IsLoaded and
             ( PluginLevelInfo.ReplayInfo.SelectedRange[0] < PluginLevelInfo.ReplayInfo.SelectedRange[1]) and
             ( PluginLevelInfo.ReplayInfo.SelectedRange[2] > 0 ) and
             ( Length( PluginLevelInfo.ReplayInfo.MovesAsText ) = OpenForm.Game.History.Top ) and
             Assigned(MainForm.Optimizer) then begin
             if   PluginLevelInfo.ReplayInfo.SelectedRange[6]<>0 then
                  PluginLevelInfo.ReplayInfo.SelectedRange[6]:=0
             else PluginLevelInfo.ReplayInfo.SelectedRange[6]:=PluginLevelInfo.ReplayInfo.SelectedRange[1]-PluginLevelInfo.ReplayInfo.SelectedRange[0];
             if   EnterBrowseMode then begin
                  ImageReplaySpeed.Tag:=1; // fake that the mouse-button is pressed, so the solution is updated with the new 'SelectRange[6]' value
                  LeaveBrowseMode( True );
                  end;
             ShowOptimizationRange(-1);
             end
          else PluginLevelInfo.ReplayInfo.SelectedRange[6]:=0;
          ShowStatus;
          end;
     finally if Assigned( OptimizerTaskQueue.Plugin ) then OptimizerTaskQueue.Plugin.Leave;
     end;
     end;
end;

function  TToolsForm.LoadLevelFromEditor(ClearReplayGame__,Refresh__:Boolean):Boolean;
var P:TColRow; B:TBoard;

  procedure Clear(ClearReplayGame__:Boolean);
  begin
    with PluginLevelInfo do with BoardAsText do begin
      Board:=''; Height:=0; Width:=0; LevelName:=''; ErrorText:=''; IsALegalLevel:=False; SelectedSquaresAsText:='';
      //NewGamesCount:=0;
      Game.IsBusy:=False; Game.IsBrowsing:=False; Game.IsReplaying:=False;
      with OpenForm.Game do begin IsBusy:=False; IsReplaying:=False; IsBrowsing:=False; end;
      if ClearReplayGame__ then ClearPluginReplayInfo;
      end;
  end;

begin // LoadLevelFromEditor
  Result:=False;
  try
    Clear(ClearReplayGame__);
    if Assigned(Game) and Assigned(LevelSetForm) then with Game do begin
       if PageControl1.ActivePage<>TabSheetGenerator then begin
          B:=Board;
          PluginLevelInfo.BoardAsText.Width:=MAX_BOARD_WIDTH;
          PluginLevelInfo.BoardAsText.Height:=MAX_BOARD_HEIGHT;
          TrimBoard(0,0,B,PluginLevelInfo.BoardAsText.Width,PluginLevelInfo.BoardAsText.Height,P);
          PluginLevelInfo.BoardAsText.Board:=SokFile_.BoardToText(PluginLevelInfo.BoardAsText.Width,PluginLevelInfo.BoardAsText.Height,B,'');
          PluginLevelInfo.IsALegalLevel:=IsALegalLevel(False,True,PluginLevelInfo.ErrorText);
          if PluginLevelInfo.ErrorText<>'' then
             PluginLevelInfo.ErrorText:=StrWithout(Trim(PluginLevelInfo.ErrorText),SPACE,PERIOD);
          PluginLevelInfo.LevelName:=Editor.FileName;
          end;

       if Refresh__ and
          ((PageControl1.ActivePage=TabSheetSolver) or (PageControl1.ActivePage=TabSheetOptimizer) or (PageControl1.ActivePage=TabSheetGenerator)) then
          with PluginLevelInfo.BoardAsText do
            LevelSetForm.ShowBoard0(Width,Height,B,ImageBoard ,False);
       LevelNamePanel.Hint:=StrWithQuotedAmpersands(VisualFileName(PluginLevelInfo.LevelName));
       if   LevelNamePanel.Hint<>'' then
            LevelNamePanel.Caption:=SPACE+LevelNamePanel.Hint+SPACE // the first space is for pretty-printing in the panel; the second space is for a better balance in the "mouse-over" tool-tip
       else LevelNamePanel.Caption:='';

       with PluginLevelInfo do
         if (not ClearReplayGame__) and (ReplayInfo.MovesAsText<>'') then // 'True': there is a previously found game (typically a solution) on the screen
            if IsSameBoardAsText(BoardAsText,ReplayInfo.BoardAsText) and
               (StrEqual(ReplayInfo.PluginFileName,MainForm.Solver.PluginFileName)
                or
                StrEqual(ReplayInfo.PluginFileName,MainForm.Optimizer.PluginFileName)
               )
               then begin
               // the old moves matches the current level
               // (i.e., the level hasn't been modified in the meantime),
               // and the moves were found by one of the currently loaded
               // plugins, hence, leave the moves on the screen
               ReplayInfo.MoveCount:=0;
               end
            else ClearPluginReplayInfo;

       if (PageControl1.ActivePage=TabSheetSolver) and
          (not SolveLevelsGroupBox.Visible) then
          OpenForm.EnableDisablePluginControls(Self,MainForm.Solver,False);

       Result:=True;
       end;
  except on E:Exception do begin
       Clear(True);
       Result:=Error(E.Message,Application.Title);
       end;
  end;
end;

function  TToolsForm.CopyLevelToClipboard(OppositeFillFloorsSetting__,RunLengthEncoding__:Boolean):Boolean;
var FloorFillChar:Char;
begin
  Result:=False;
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors and
     StrEqual(Editor.FileName,MainForm.Game.FileName) and
     (not (Modified {$IFDEF YASC} or MainForm.Modified {$ENDIF})) and
     PluginEditMenuItemCopy.Enabled then with MainForm.Game do begin
     if        RunLengthEncoding__ then
               FloorFillChar:=FLOOR_CH // don't fill the floors before run-len encoding; it uses 'RunLengthEncodingFloor' itself
     else if   (     MainForm.CopyLevelToClipboardFillFloors  and (not OppositeFillFloorsSetting__))
               or
               ((not MainForm.CopyLevelToClipboardFillFloors) and      OppositeFillFloorsSetting__ ) then
               FloorFillChar:=MainForm.CopyLevelToClipboardFloorFillCharacter
          else FloorFillChar:=FLOOR_CH;
     Result:=CopyToClipboard(FloorFillChar,MainForm.CopyLevelToClipboardPreserveCombinedMoves,RunLengthEncoding__,False,False);
     ShowStatus;
     end;
end;

procedure TToolsForm.PluginEditMenuItemCopyClick(Sender: TObject);
begin
  CopyLevelToClipboard(Sender=nil,False);
end;

procedure TToolsForm.PluginEditMenuItemPasteClick(Sender: TObject);
var OldRowCount:Integer; HasLoadedSolutions:Boolean;

  function LoadSolutionsAndSnapshotsForCurrentGame:Boolean;
  var GameReady:Boolean;
  begin
    Result:=Assigned(MainForm.Game) and
            (not MainForm.Game.IsBusy) and
            (MainForm.Modified or Self.Modified) and
            MainForm.LoadSolutionsAndSnapshotsForCurrentGame('',{True}False,nil,GameReady);
    if Result then
       if   IsANewFileName(MainForm.Game.FileName) then
            MainForm.Modified:=True                // the clipboard contained a single level, not a collection
       else MainForm.Game.Notes.Modified:=True;    // 'Notes.Modified' later triggers a silent 'SaveToFile', in contrast to 'MainForm.Modified' which asks the user before saving
  end;

begin // 'PluginEditMenuItemPasteClick'
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors and
     Clipboard.HasFormat(CF_TEXT) and
     PluginEditMenuItemPaste.Enabled then begin
     if   PageControl1.ActivePage=TabSheetSolver then
          OldRowCount:=SolverTaskQueue.RowCount
     else OldRowCount:=OptimizerTaskQueue.RowCount;

     HasLoadedSolutions:=LoadSolutionsAndSnapshotsForCurrentGame;
     if HasLoadedSolutions or MainForm.CloseLevel(Self) then
        try
          PluginToolButtonStopReplayClick(PluginToolButtonReplay);
          PluginLevelInfo.ReplayInfo.IsLoaded:=False;
          if not HasLoadedSolutions then MainForm.BtnOpenClipBoardClick(Sender);
          InitializeTask(MainForm,PageControl1.ActivePage,[]);
        finally
          SetMessageHandlers;
          if      PageControl1.ActivePage=TabSheetOptimizer then
                  OptimizerTaskQueue.FocusLevelName(Editor.FileName,OptimizerTaskQueue.RowCount=OldRowCount) // if new solutions have been imported, then focus the last one
          else if PageControl1.ActivePage=TabSheetSolver then
                  SolverTaskQueue.FocusLevelName(Editor.FileName,SolverTaskQueue.RowCount=OldRowCount); // if new solutions have been imported, then focus the last one
        end;
     end;
end;

procedure TToolsForm.PluginToolButtonCutClick(Sender: TObject);
begin
  if PageControl1.ActivePage=TabSheetGenerator then GeneratorEditMenuItemCutClick(Sender);
end;

procedure TToolsForm.PluginToolButtonCopyClick(Sender: TObject);
begin
  if   PageControl1.ActivePage<>TabSheetGenerator then PluginEditMenuItemCopyClick(Sender)
  else GeneratorEditMenuItemCopyClick(Sender)
end;

procedure TToolsForm.PluginToolButtonPasteClick(Sender: TObject);
begin
  if   PageControl1.ActivePage<>TabSheetGenerator then PluginEditMenuItemPasteClick(Sender)
  else GeneratorEditMenuItemPasteClick(Sender);
end;

procedure TToolsForm.BtnPluginBrowseClick(Sender: TObject);
var b:Boolean; TaskQueue:TTaskQueue;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors and
     (PluginForCurrentTabSheet<>nil) then with PluginForCurrentTabSheet do begin
     StatusText:='';
     MainForm.BtnStatusMenuPluginClick(Self);
     Add(PluginFileName);
     OpenPluginFromList(PluginForCurrentTabSheet,IndexOf(PluginFileName));
     if        PageControl1.ActivePage=TabSheetOptimizer then TaskQueue:=OptimizerTaskQueue
     else if   PageControl1.ActivePage=TabSheetSolver    then TaskQueue:=SolverTaskQueue
     else if   PageControl1.ActivePage=TabSheetGenerator then TaskQueue:=GeneratorTaskQueue
          else TaskQueue:=nil;
     if Assigned(TaskQueue) then with TaskQueue do begin
        Plugin.Enter;
        try     OpenForm.EnableDisablePluginControls(Self,Plugin,True);
                if   not Plugin.IsActive then
                     HighlightedRowNumber:=-1
                else RefreshRow(HighlightedRowNumber);
                if        PageControl1.ActivePage=TabSheetOptimizer then begin
                          if not BtnOptimizerBrowse.Enabled then
                             ActiveControl:=TaskQueue.StringGrid;
                          OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,b);
                          end
                else if   PageControl1.ActivePage=TabSheetSolver    then begin
                          if (not BtnSolverBrowse.Enabled)  and
                             SolveLevelsGroupBox.Visible then
                             ActiveControl:=TaskQueue.StringGrid;
                          end;
                Refresh(False);
                ShowStatus;
        finally Plugin.Leave;
        end;
        end;
     end;
end;

procedure TToolsForm.PluginSettingsWindowOrAboutFactBox(Sender: TObject);
var HasLockedSolver,HasLockedOptimizer,HasLockedGenerator:Boolean;
    HintText:String; Handle:THandle;
begin
  HintText:='';
  if        Sender=OpenForm.BtnPluginSettings then begin
            Handle:=OpenForm.Handle;
            if      OpenForm.Task=otSolver    then Sender:=BtnSolverSettings
            else if OpenForm.Task=otOptimizer then Sender:=BtnOptimizerSettings;
            end
  else if   Sender=OpenForm.BtnPluginAbout then begin
            Handle:=OpenForm.Handle;
            if      OpenForm.Task=otSolver    then Sender:=BtnSolverAbout
            else if OpenForm.Task=otOptimizer then Sender:=BtnOptimizerAbout;
            end
       else Handle:=Self.Handle;

  if        Sender=PluginMenuItemSettings then
            if        PluginForCurrentTabSheet is TSolverPlugin    then Sender:=BtnSolverSettings
            else if   PluginForCurrentTabSheet is TOptimizerPlugin then Sender:=BtnOptimizerSettings
                 else
  else if   Sender=PluginMenuItemAbout    then
            if        PluginForCurrentTabSheet is TSolverPlugin    then Sender:=BtnSolverAbout
            else if   PluginForCurrentTabSheet is TOptimizerPlugin then Sender:=BtnOptimizerAbout;

  try
    HasLockedSolver:=Assigned(MainForm.Solver) and MainForm.Solver.HasThread;
    if      HasLockedSolver then begin MainForm.Solver.Enter; MainForm.Solver.Suspend; end;
    try     HasLockedOptimizer:=Assigned(MainForm.Optimizer) and MainForm.Optimizer.HasThread;
            if      HasLockedOptimizer then begin MainForm.Optimizer.Enter; MainForm.Optimizer.Suspend; end;
            try     HasLockedGenerator:=Assigned(MainForm.Generator) and MainForm.Generator.HasThread;
                    if      HasLockedGenerator then begin MainForm.Generator.Enter; MainForm.Generator.Suspend; end;
                    try
                            if      OpenForm.PluginTimer.Enabled then
                                    if   (Sender=BtnSolverAbout) or  (Sender=BtnOptimizerAbout) then
                                         HintText:=HintCloseAboutWindowWhenYouAreDoneSoSuspendedToolsCanResumeTheirWorkText
                                    else HintText:=HintCloseSettingsWindowWhenYouAreDoneSoSuspendedToolsCanResumeTheirWorkText;
                            if      HintText<>'' then
                                    if   Handle=Self.Handle then
                                         StatusText:=HintText
                                    else OpenForm.StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=HintText;

                            if      (Sender=BtnSolverSettings   ) and Assigned(MainForm.Solver   ) then MainForm.Solver   .Settings (Handle)
                            else if (Sender=BtnOptimizerSettings) and Assigned(MainForm.Optimizer) then MainForm.Optimizer.Settings (Handle)
                            else if (Sender=BtnGeneratorSettings) and Assigned(MainForm.Generator) then GeneratorForm     .ShowModal
                            else if (Sender=BtnSolverAbout      ) and Assigned(MainForm.Solver   ) then MainForm.Solver   .ShowAbout(Handle)
                            else if (Sender=BtnOptimizerAbout   ) and Assigned(MainForm.Optimizer) then MainForm.Optimizer.ShowAbout(Handle)

                    finally if HasLockedGenerator then begin MainForm.Generator.Resume; MainForm.Generator.Leave; end;
                    end;
            finally if HasLockedOptimizer then begin MainForm.Optimizer.Resume; MainForm.Optimizer.Leave; end;
            end;
    finally if HasLockedSolver then begin MainForm.Solver.Resume; MainForm.Solver.Leave; end;
    end;
  finally   if HintText<>'' then
               if   Handle=Self.Handle then
                    StatusText:=''
               else OpenForm.StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:='';
  end;
end;

procedure TToolsForm.PluginMenuItemSettingsClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and (not Game.IsReplaying) and
     CloseEditors and
     (PluginForCurrentTabSheet<>nil) then begin
     StatusText:='';
     //with PluginForCurrentTabSheet do Settings(Handle);
     PluginSettingsWindowOrAboutFactBox(Sender);
     end;
end;

procedure TToolsForm.PluginMenuItemAboutClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and (not Game.IsReplaying) and
     CloseEditors and
     (PluginForCurrentTabSheet<>nil) then begin
     StatusText:='';
     //PluginForCurrentTabSheet.ShowAbout(Handle);
     PluginSettingsWindowOrAboutFactBox(Sender);
     end;
end;

procedure TToolsForm.BtnPluginClick(Sender: TObject);
begin
  if CloseEditors
     and
     OpenForm.Game.IsIdleAndStopReplayingAndBrowsing
     and
     (not Game.IsBusy)
     and
     Assigned(OpenForm) then begin
     PluginToolButtonStopReplayClick(Sender);
     OpenForm.BtnPluginClick(PluginForCurrentTabsheet);
     if MainForm.ShutDownApplication then Close;
     end;
end;

function TToolsForm.OpenPluginFromList(Plugin__:TPlugin; Index__:Integer):Boolean;
var i:Integer; s:String; GroupBox:TGroupBox;
begin
  if      PlugIn__ is TSolverPlugin then
          GroupBox:=CurrentSolverGroupBox
  else if Plugin__ is TOptimizerPlugin then
          GroupBox:=CurrentOptimizerGroupBox
  else    GroupBox:=nil;
  OptimizationComboBox.Clear;

  with Plugin__ do with ComboBox do with Items do begin
    if   (Index__>=0) and (Index__<Count) then begin
         ItemIndex:=Index__;
         if    Objects[ItemIndex]<>nil then
               s:=StringsDB.Strings[Cardinal(Objects[ItemIndex])]
         else  s:=Items[ItemIndex];
         end
    else begin ItemIndex:=-1; s:='';
         end;

    if (ItemIndex>=0)
       and
       ((not IsLoaded)
        or
        ((PluginFileName<>s)
         and
         (not ((PluginFileName=DEFAULT_VALUE)
               and
               (s=DefaultPluginFileName)
              )
         )
        )
       ) then begin
       if   StrEqual(s,DefaultPluginFileName) then
            Open(DEFAULT_VALUE)
       else Open(s);
       end;

    Result:=(ItemIndex>=0) and IsLoaded;

    if Result then begin
       if   PluginName<>'' then
            SetGroupBoxCaption(Self.Canvas,GroupBox,PluginName)
       else SetGroupBoxCaption(Self.Canvas,GroupBox,ExtractFileNameWithoutPathAndExtension(s));
       end
    else begin
       GroupBox.Caption:='';
       if ItemIndex>=0 then Plugin__.Remove(ItemIndex);
       if (ItemIndex<0) and (Count>0) then ItemIndex:=0;
       if ItemIndex>=0 then OpenPluginFromList(Plugin__,ItemIndex); // try another plugin, if any
       end;

    with PluginLevelInfo.ReplayInfo do
      if (s<>'') and
         (not StrEqual(s,PluginFileName)) and
         (not (StrEqual(PluginFileName,DEFAULT_VALUE) and StrEqual(s,DefaultPluginFileName)))
         then begin
         PluginToolButtonStopReplayClick(PluginToolButtonReplay); // this rewinds the game in case the program isn't in replay mode
         ClearPluginReplayInfo;
         ReplayOptions:=ReplayOptions+[roStep,roHome]; // this rewinds the game if necessary (not stepwise, even though the arguments suggest that) in case the program currently is in replay mode
         for i:=0 to Pred(PluginLevelStringGrid.RowCount) do PluginLevelStringGrid.Cells[1,i]:='';
         end;

    if   Assigned(OpenForm) then OpenForm.EnableDisablePluginControls(Self,Plugin__,True);
    if   Plugin__ is TSolverPlugin then // there is only 1 instance of 'TSolverPlugin' and 'TOptimizerPlugin' respectively, hence, the settings may be updated with the folowing lines
         OptionsForm.UpdateSettings(-Ord(stToolsSolverFileName)   ,MainForm.Solver   .PluginFileName,Self)
    else OptionsForm.UpdateSettings(-Ord(stToolsOptimizerFileName),MainForm.Optimizer.PluginFileName,Self);
    ShowStatus;
    end;
end;

procedure TToolsForm.PluginMenuItemRemoveClick(Sender: TObject);
begin
  if PluginForCurrentTabSheet<>nil then
     with PluginForCurrentTabSheet do
       if ComboBox<>nil then with ComboBox do with Items do
          if ItemIndex>=0 then begin
             Remove(ItemIndex);
             if (ItemIndex< 0) and (Count>0) then ItemIndex:=0;
             if ItemIndex >=0 then OpenPluginFromList(PluginForCurrentTabSheet,ItemIndex);
             if Count     = 0 then OpenPluginFromList(PluginForCurrentTabSheet,-1);
             end;
end;

procedure TToolsForm.SolverComboBoxChange(Sender: TObject);
begin
  with SolverComboBox do OpenPluginFromList(MainForm.Solver,ItemIndex);
end;

procedure TToolsForm.OptimizerComboBoxChange(Sender: TObject);
var b:Boolean;
begin
  with OptimizerComboBox do OpenPluginFromList(MainForm.Optimizer,ItemIndex);
  if Assigned(OptimizerTaskQueue) then
     OptimizeSolutionsStringGridSelectCell(nil,0,OptimizerTaskQueue.StringGrid.Row,b);
end;

function TToolsForm.IsToolsFormPluginLevel(BoardWidth__,BoardHeight__:Integer; const BoardAsText__,LevelName__:String):Boolean;
begin
  with PluginLevelInfo do with BoardAsText do
    Result:=(BoardWidth__ =Width) and
            (BoardHeight__=Height) and
            (BoardAsText__=Board) and
            StrEqual(LevelName__,LevelName);
end;

procedure TToolsForm.PluginToolButtonReplayClick(Sender: TObject);
begin
  with PluginToolButtonReplay do
    if Enabled then begin
       StatusText:='';
       if Game.IsReplaying then
          OpenForm.Game.IsIdleAndStopReplayingAndBrowsing
       else begin
         Game.UserBreakWhileReplaying:=True;
         Replay([roReplay,roForwards],0); // always start replaying forwards; this is the least confusing for the user, even though it means that the direction isn't retained from one replay session to the next
         end;
       ShowStatus;
       end;
end;

procedure TToolsForm.PluginToolButtonStopReplayClick(Sender: TObject);
begin
  LeaveBrowseMode(True);
  ImageBoard.Tag:=0;
  ImageReplaySpeed.Tag:=0;
  with PluginToolButtonStopReplay do
    if Enabled or (Sender=PluginToolButtonReplay) then begin
       StatusText:='';
       if      Enabled then begin
               Enabled:=False; Refresh;
               end;
       if      Game.IsReplaying then begin
               Include(PluginLevelInfo.ReplayInfo.ReplayOptions,roHome); // 'roHome': rewind to the start position when the replay loop exits
               OpenForm.Game.IsIdleAndStopReplayingAndBrowsing;
               end
       else if PluginLevelInfo.ReplayInfo.IsLoaded then with OpenForm.Game do begin
               Reset(True);
               if (PluginLevelInfo.SelectedSquaresAsText<>'') then
                  SelectedSquaresAsTextToBoard(PluginLevelInfo.SelectedSquaresAsText,BoardWidth,BoardHeight,Board);
               LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,ImageBoard,False);
               PluginLevelInfo.ReplayInfo.MoveCount:=History.Count;
               ShowStatus;
               end;
       end;
end;

function  TToolsForm.Replay(ReplayOptions__:TReplayOptions; Position__:Integer):Boolean;
var IsActive:Boolean; ReplayStartTimeMS:TTimeMS;

  function InitializeGame:Boolean;
  begin
    with PluginLevelInfo.ReplayInfo do with BoardAsText do begin
      if not IsLoaded then begin
         MoveCount:=0;
         if SelectedRange[2]=0 then begin // 'True': no optimization slice loaded from the solution
            SelectedRange[0]:=0; SelectedRange[1]:=0; SelectedRange[6]:=0;
            end;
         end;

      Result:=IsLoaded
              or
              (OpenForm.Game.LoadFromBoardAsText(Width,Height,PageControl1.ActivePage=TabSheetGenerator,False,PageControl1.ActivePage=TabSheetGenerator,False,Board)
               and
               OpenForm.Game.LoadSnapshotAsTextString(MovesAsText,True)
               and
               (OpenForm.Game.History.Top>OpenForm.Game.ForcedInitialJumps)
               and
               ((PluginLevelInfo.SelectedSquaresAsText='')
                or
                (SelectedSquaresAsTextToBoard(PluginLevelInfo.SelectedSquaresAsText,OpenForm.Game.BoardWidth,OpenForm.Game.BoardHeight,OpenForm.Game.Board)>=0))
              );

      if Result then with OpenForm.Game do begin // set up the board so it matches the number of moves in 'MoveCount'
         while (PluginLevelInfo.ReplayInfo.MoveCount>History.Count) and
               InheritedRedo(False) do begin end;
         while (PluginLevelInfo.ReplayInfo.MoveCount<History.Count) and
               InheritedUndo(False) do begin end;
         Result:=PluginLevelInfo.ReplayInfo.MoveCount=History.Count;
         end;

      IsLoaded:=Result;

      if Result and (PageControl1.ActivePage=TabSheetOptimizer) then
         ShowOptimizationRange(-1);
      end;
  end;

  function InitializeSprites:Boolean;

    function CreateSprite(var Sprite__:TToolsFormSprite):Boolean;
    begin
      Sprite__:=nil;
      try    Sprite__:=TToolsFormSprite.Create(ImageBoard.Picture.BitMap.Canvas,LevelSetForm.GameViewer.SkinPict,nil,nil);
             Result:=True;
      except on E:Exception do begin
                Sprite__.Free; Sprite__:=nil;
                Result:=Error(E.Message,Application.Title);
                end;
      end;
    end;

  begin // InitializeSprites
    with LevelSetForm.GameViewer do begin
      if not Assigned(BoxSprite   ) then CreateSprite(BoxSprite   );
      if not Assigned(PlayerSprite) then CreateSprite(PlayerSprite);
      Result:=Assigned(BoxSprite) and Assigned(PlayerSprite) and BackgroundInitialized and SkinInitialized;
      if Result then
         try    BoxSprite.Resize(ColWidth,RowHeight);
                PlayerSprite.Resize(ColWidth,RowHeight);
                BoxSprite.FrameCount:=SkinPict.FrameCount;
                PlayerSprite.FrameCount:=SkinPict.FrameCount;
                BoxSprite.ItemIndex:=-1; PlayerSprite.ItemIndex:=-1; // clear the background for the sprites
         except on E:Exception do Result:=Error(E.Message,Application.Title);
         end;
      end;
  end;

begin // Replay
  begin
    if        roForwards  in ReplayOptions__ then Exclude(ReplayOptions__,roBackwards)
    else if   roBackwards in ReplayOptions__ then Exclude(ReplayOptions__,roForwards)
         else Include(ReplayOptions__,roForwards);
    if        roReplay    in ReplayOptions__ then Exclude(ReplayOptions__,roStep)
    else if   roStep      in ReplayOptions__ then Exclude(ReplayOptions__,roReplay)
         else Include(ReplayOptions__,roReplay);
    Result:=(not Game.IsBusy) and
            CloseEditors and
            (not OpenForm.Game.IsBusy) and
            (not OpenForm.Game.IsBrowsing) and
            (not OpenForm.Game.IsReplaying) and
            InitializeGame and
            (not ((roStep   in ReplayOptions__) and (roBackwards in ReplayOptions__) and (OpenForm.Game.History.Count<=OpenForm.Game.ForcedInitialJumps))) and
            (not ((roStep   in ReplayOptions__) and (roForwards  in ReplayOptions__) and (OpenForm.Game.History.Count =OpenForm.Game.History.Top))) and
            InitializeSprites;
    if Result then with PluginLevelInfo.ReplayInfo do begin
       IsActive:=False; ReplayStartTimeMS:=0;
       try     Game.IsBusy     :=True;
               Game.IsBrowsing :=ReplayOptions__=[roPosition];
               Game.IsReplaying:=not Game.IsBrowsing;
               if PanelToolTips.Visible then PanelToolTips.Hide;
               if      PageControl1.ActivePage=TabSheetOptimizer then with OptimizerTaskQueue do begin
                       ScrollInView(StringGrid.Row);
                       Plugin.Enter;
                       try     RefreshRow(StringGrid.Row);
                               // suspending the plugin would make the timing more inaccurate, hence, the following lines are commented out
                               //IsActive:=Plugin.IsActive;
                               //if IsActive then Plugin.Suspend;
                       finally Plugin.Leave;
                       end;
                       ActiveControl:=LevelNamePanel;
                       end
               else if (PageControl1.ActivePage=TabSheetSolver) and SolveLevelsGroupBox.Visible then with SolverTaskQueue do begin
                       ScrollInView(StringGrid.Row);
                       Plugin.Enter;
                       try     RefreshRow(StringGrid.Row);
                               // suspending the plugin would make the timing more inaccurate, hence, the following lines are commented out
                               //IsActive:=Plugin.IsActive;
                               //if IsActive then Plugin.Suspend;
                       finally Plugin.Leave;
                       end;
                       ActiveControl:=LevelNamePanel;
                       end
               else if PageControl1.ActivePage=TabSheetGenerator then with GeneratorTaskQueue do begin
                       ScrollInView(StringGrid.Row);
                       Plugin.Enter;
                       try     RefreshRow(StringGrid.Row);
                               IsActive:=Plugin.IsActive;
                               if IsActive then begin
                                  Plugin.Suspend;
                                  MainForm.Generator.StopTimer;
                                  ReplayStartTimeMS:=GetTimeMS;
                                  end;
                       finally Plugin.Leave;
                       end;
                       ActiveControl:=LevelNamePanel;
                       end;

               if (ActiveControl=PageControl1) or
                  (ActiveControl=PluginLevelStringGrid) or
                  (ActiveControl=OptimizeSolutionsStringGrid) or
                  (ActiveControl=SolveLevelsStringGrid) or
                  (ActiveControl=GenerateLevelsStringGrid)
                  then
                  ActiveControl:=LevelNamePanel; // so '[Home]' and '[End]' etc. only affect the replay state and don't change the selected control/cell
               StatusText:='';

               if ReplayOptions__*[roStep,roHome,roEnd,roPosition]=[] then begin
                  if ReplaySpeedMilliSecondsPerMove<=0 then
                     ReplaySpeedMilliSecondsPerMove:=DEFAULT_ANIMATE_REPLAY_MOVES_MS;
                  ImageReplaySpeed.Hint:=HintToolsWindowReplaySpeedText;
                  ImageReplaySpeed.Show;
                  ShowReplaySpeed;
                  end;

               with OpenForm.Game do with History do
                 if ((Count=Top) and (roForwards in ReplayOptions__)) and (not (roEnd in ReplayOptions__)) then begin // rewind to the start of the game
                    Reset(True);
                    LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,LevelSetForm.GameViewer.BoardImage,False);
                    ShowStatus;
                    Application.ProcessMessages;
                    SleepEx(MainForm.PauseBeforeReplayMS,False);
                    end
                 else begin
                    if (Count<=ForcedInitialJumps) and (roBackwards in ReplayOptions__) then
                       ReplayOptions__:=ReplayOptions-[roBackwards]+[roForwards];
                    end;

               ReplayOptions:=ReplayOptions__;
               OpenForm.Game.AnimateReplayMovesMS:=Self.ReplaySpeedMilliSecondsPerMove;

               if (ReplayOptions*[roHome,roEnd,roPosition])=[] then begin
                  if PluginLevelInfo.SelectedSquaresAsText<>'' then with OpenForm.Game do begin
                     SelectedSquaresAsTextToBoard('',BoardWidth,BoardHeight,Board); // remove selected squares from the board before replaying moves
                     LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,LevelSetForm.GameViewer.BoardImage,False);
                     end;
                  repeat  ShowStatus;
                          if (ReplayOptions__<>ReplayOptions) and // 'True': the direction has changed
                             (ImageReplaySpeed.Tag=0) then
                             StatusText:=GetLongHint(ImageBoard.Hint);
                          ReplayOptions__:=ReplayOptions;
                          OpenForm.Game.ReplayInToolsWindow(roForwards in ReplayOptions,roStep in ReplayOptions__);
                          if not (roStep in ReplayOptions__) then
                             Self.ReplaySpeedMilliSecondsPerMove:=OpenForm.Game.AnimateReplayMovesMS;
                  until   (roStep in ReplayOptions)
                          or
                          ((roForwards in ReplayOptions__)=(RoForwards in ReplayOptions)); // until the user didn't toggle the direction during replay
                  end;

               with OpenForm.Game do with History do begin
                 if      (roHome in ReplayOptions) and (Count>ForcedInitialJumps) then begin // rewind the game
                         Reset(True);
                         LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,LevelSetForm.GameViewer.BoardImage,False);
                         end
                 else if (roEnd in ReplayOptions) and (Count<Top) then begin
                         while (Count<Top) and Redo(False) do begin end;
                         LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,LevelSetForm.GameViewer.BoardImage,False);
                         end
                 else if (roPosition in ReplayOptions) and (Count<>Position__) then begin
                         BrowsePosition:=Position__;
                         LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,LevelSetForm.GameViewer.BoardImage,False);
                         end;
                 PluginLevelInfo.ReplayInfo.MoveCount:=Count;
                 end;

       finally Game.IsBusy:=False; Game.IsBrowsing:=False; Game.IsReplaying:=False;
               StatusText:='';
               ImageReplaySpeed.Hide;
               if   PluginLevelInfo.ReplayInfo.IsLoaded then
                    ImageBoard.Hint:=HintReplayInToolsWindowText
               else ImageBoard.Hint:='';
               PluginLevelInfo.ReplayInfo.MoveCount:=OpenForm.Game.History.Count;
               if      PageControl1.ActivePage=TabSheetOptimizer then with OptimizerTaskQueue do begin
                       if (PluginLevelInfo.SelectedSquaresAsText<>'') and
                          ((ReplayOptions*[roHome,roEnd,roPosition])=[]) then with OpenForm.Game do begin
                          SelectedSquaresAsTextToBoard(PluginLevelInfo.SelectedSquaresAsText,BoardWidth,BoardHeight,Board); // mark selected squares on the board again after replaying moves
                          LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,LevelSetForm.GameViewer.BoardImage,False);
                          end;
                       Plugin.Enter;
                       try     RefreshRow(StringGrid.Row);
                               if IsActive then Plugin.Resume;
                       finally Plugin.Leave;
                       end;
                       end
               else if (PageControl1.ActivePage=TabSheetSolver) and SolveLevelsGroupBox.Visible then with SolverTaskQueue do begin
                       Plugin.Enter;
                       try     RefreshRow(StringGrid.Row);
                               if IsActive then Plugin.Resume;
                       finally Plugin.Leave;
                       end;
                       end
               else if PageControl1.ActivePage=TabSheetGenerator then with GeneratorTaskQueue do begin
                       Plugin.Enter;
                       try     RefreshRow(StringGrid.Row);
                               if IsActive then begin
                                  MainForm.Generator.PauseTimeMS:=AddTimeMS(MainForm.Generator.PauseTimeMS,CalculateElapsedTimeMS(ReplayStartTimeMS,GetTimeMS));
                                  MainForm.Generator.StartTimer;
                                  Plugin.Resume;
                                  end;
                       finally Plugin.Leave;
                       end;
                       end;
               ShowStatus;
       end;
       end
    else ShowStatus;
  end;
end;

function  TToolsForm.UndoRedoMove(Redo__:Boolean):Boolean;
begin
  Result:=False;
  if Assigned(Game) and PluginToolButtonReplay.Enabled then
     with OpenForm.Game do with PluginLevelInfo.ReplayInfo do
       if IsIdleAndStopReplayingAndBrowsing then
          if   Redo__ then
               Replay([roStep,roForwards],0)
          else Replay([roStep,roBackwards],0);
end;

procedure TToolsForm.ClearPluginReplayInfo;

begin
  with PluginLevelInfo.ReplayInfo do with BoardAsText do begin
    if Game.IsBrowsing then LeaveBrowseMode(True);
    Board:=''; Height:=0; Width:=0; IsLoaded:=False; MovesAsText:=''; ReplayOptions:=[roReplay,roForwards]; PluginFileName:='';
    FillChar(SelectedRange,SizeOf(SelectedRange),0);
    ImageBoard.Hint:='';
    if ImageReplaySpeed.Visible then ImageReplaySpeed.Hide;
    end;
end;

procedure TToolsForm.ShowReplaySpeed;
var i:Integer; s:String; R:TRect;
begin
  with ImageReplaySpeed do with Picture.BitMap do with Canvas do begin
    i:=(ReplaySpeedMovesPerSecond*ClientWidth) div MAX_REPLAY_SPEED_MOVES_PER_SEC;

    Brush.Style:=bsSolid;
    Brush.Color:=ReplaySpeedTrackBarSliderColor;
    R:=Rect(0,0,i,ClientHeight);
    FillRect(R);

    Brush.Color:=ReplaySpeedTrackBarBackgroundColor;
    R.Left:=i; R.Right:=ClientWidth;
    FillRect(R);

    R.Left:=0;
    if   ReplaySpeedMovesPerSecond<>1 then
         s:=ReplayText+SUB_TITLE_SEPARATOR+IntToStr(ReplaySpeedMovesPerSecond)+MovesPerSecondShortText
    else s:=ReplayText+SUB_TITLE_SEPARATOR+IntToStr(ReplaySpeedMovesPerSecond)+MovePerSecondShortText;
    Font.Color:=ReplaySpeedTrackBarFontColor;
    Windows.SetBkMode (Canvas.Handle,Windows.TRANSPARENT);
    DrawText(Handle,PChar(s),Length(s),R,DT_CENTER+DT_VCENTER+DT_SINGLELINE);
    Windows.SetBkMode (Canvas.Handle,Windows.OPAQUE);
    DrawArrow( Canvas, i, 4, 5, SokUtil_.Down, ReplaySpeedTrackBarFontColor, ReplaySpeedTrackBarShadowColor );
    end;
end;

procedure TToolsForm.ImageReplaySpeedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var StartOfInterval:Integer;
begin
  ImageBoard.Tag:=0;
  with   PluginLevelInfo.ReplayInfo do with ImageReplaySpeed do with Picture.BitMap do
    if   Button=mbLeft then begin
         if Game.IsReplaying then begin
            Tag:=1;
            if Cursor<>crDrag then Cursor:=crDrag;
            if Screen.Cursor<>Cursor then Screen.Cursor:=Cursor;
            ReplaySpeedMovesPerSecond:=MAX_REPLAY_SPEED_MOVES_PER_SEC
                                       *
                                       Max(0,
                                           Min(X+(ClientWidth div (2*Max(1,MAX_REPLAY_SPEED_MOVES_PER_SEC))), // '+' try to smooth things out a bit by adding half the size of each tick
                                               ClientWidth))
                                       div Max(1,ClientWidth);
            OpenForm.Game.AnimateReplayMovesMS:=ReplaySpeedMilliSecondsPerMove;
            Hint:=HintToolsWindowReplaySpeedText;
            StatusText:=GetLongHint(Hint);
            ShowReplaySpeed;
            end
         else if Game.IsBrowsing then begin
                 if Assigned( OptimizerTaskQueue.Plugin ) then OptimizerTaskQueue.Plugin.Enter;
                 try
                   if OptimizerTaskQueue.StringGrid.Row <> OptimizerTaskQueue.HighlightedRowNumber then begin
                      Tag:=Tag or 1; // 1: mouse down
                      if Cursor<>crDrag then Cursor:=crDrag;
                      if Screen.Cursor<>Cursor then Screen.Cursor:=Cursor;
                      BrowseMouseMove(X,Y);
                      end;
                 finally if Assigned( OptimizerTaskQueue.Plugin ) then OptimizerTaskQueue.Plugin.Leave;
                 end;
                 end;
         end
    else begin if Game.IsBrowsing and ((Tag and 1)=0) then begin
                  // allow all mouse buttons to select start/end optimization
                  // range; selecting an area uses right-click, hence, it's less
                  // confusing when selecting start/end range allows that too
                  if Button=mbMiddle then begin
                     // For convenience: the middle mouse button always moves
                     // the end-of-interval marker, as opposed to the normal
                     // behavior for the other mouse buttons, which is to move
                     // the nearest marker. Additionally, the middle mouse
                     // button toggles the "repeat interval" mode for the task.
                     StartOfInterval:=SelectedRange[0];
                     ImageReplaySpeedMouseDown(Sender,mbLeft,Shift,X,Y);
                     if Assigned( OptimizerTaskQueue.Plugin ) then OptimizerTaskQueue.Plugin.Enter;
                     try
                       if (OptimizerTaskQueue.StringGrid.Row <> OptimizerTaskQueue.HighlightedRowNumber) and
                          Game.IsBrowsing and ((Tag and 1)<>0) then begin
                          if (Tag and 2)          <>0 then begin // 'True': setting the start-of-interval marker; change it to the end-of-interval marker;
                             SelectedRange[1]     :=SelectedRange[0];
                             if   StartOfInterval < SelectedRange[0] then
                                  SelectedRange[0]:=StartOfInterval
                             else SelectedRange[0]:=0; // set the interval starting point to the beginning of the game
                             Tag:=Tag+2; // change selected marker from start (2) to end (4)
                             end;
                          BrowseMouseMove(X,Y);
                          if   (SelectedRange[0]  < SelectedRange[1]) and (SelectedRange[6]=0) then
                               SelectedRange[6]   :=SelectedRange[1]-SelectedRange[0]
                          else SelectedRange[6]   :=0;
                          end;
                     finally if Assigned( OptimizerTaskQueue.Plugin ) then OptimizerTaskQueue.Plugin.Leave;
                     end;
                     end
                  else ImageReplaySpeedMouseDown(Sender,mbLeft,Shift,X,Y);
                  end
               else begin
                  LeaveBrowseMode(True);
                  ImageReplaySpeed.Tag:=0; ImageReplaySpeed.Cursor:=crDefault;
                  if Screen.Cursor<>Cursor then Screen.Cursor:=Cursor;
                  end;
         end;
end;

procedure TToolsForm.ImageReplaySpeedMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  with ImageReplaySpeed do begin
    if Game.IsReplaying then begin
       if Tag=1 then // 'True': mouse down
          ImageReplaySpeedMouseDown(Sender,mbLeft,Shift,X,Y)
       else begin
         Tag:=0; Cursor:=crDefault;
         if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
         end;
       end
    else if (PageControl1.ActivePage=TabSheetOptimizer) and
            PluginToolButtonReplay.Enabled and
            Assigned(Game) and
            (Game.IsBrowsing or ((not Game.IsBusy) and EnterBrowseMode)) then begin
            BrowseMouseMove(X,Y);
            end;
    StatusText:=GetLongHint(Hint);
    end;
end;

procedure TToolsForm.ImageReplaySpeedMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(Game) and Game.IsBrowsing then
     LeaveBrowseMode(Button<>mbLeft);
  ImageBoard.Tag:=0; ImageReplaySpeed.Tag:=0; ImageReplaySpeed.Cursor:=crDefault; IgnoreKeyUp:=False; IgnoreMouseUp:=False;
  if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
  if (Button=mbRight) and Assigned(Game) and Game.IsReplaying then begin
     ControlMouseUp(Sender,Button,Shift,X,Y);
     ImageReplaySpeed.Tag:=2; // '2': so  the replay loop keeps the replay trackbar hint on the statusbar
     end;
  if Button<>mbLeft then
     Mouse.CursorPos:=Mouse.CursorPos; //ensure that the mouse cursor type is updated now, and not first when them mouse moves
end;

procedure TToolsForm.PluginLevelFileNamePanelClick(Sender: TObject);
begin
  FormMouseDown(Sender,mbLeft,[],0,0);
  ClickToFocusHighlightedRow(True);
end;

function  TToolsForm.PluginForCurrentTabSheet:TPlugin;
begin
  if        PageControl1.ActivePage=TabSheetSolver    then Result:=MainForm.Solver
  else if   PageControl1.ActivePage=TabSheetOptimizer then Result:=MainForm.Optimizer
  else if   PageControl1.ActivePage=TabSheetGenerator then Result:=MainForm.Generator
       else Result:=nil;
end;

procedure TToolsForm.TabSheetPluginMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var Processed: Boolean; APoint:TPoint;
begin
  Processed:=False;
  if ((Sender=TabSheetSolver) or (Sender=TabSheetOptimizer) or (Sender=TabSheetGenerator)) then with Sender as TTabSheet do
     if (Y< PluginLevelGroupBox.Top+PluginLevelGroupBox.Height) and
        (Y>=PluginLevelGroupBox.Top+BtnSolverBrowse.Top) and
        (Screen.Cursor<>crVSplit) then begin
        if      X<=SolverGroupBox.Left+(SolverGroupBox.Width div 2) then begin
                APoint:=PluginLevelGroupBox.ScreenToClient(ClientToScreen(Point(X,Y)));
                ControlMouseMove(PluginLevelGroupBox,Shift,APoint.X,APoint.Y);
                Hint:=PluginLevelGroupBox.Hint;
                StatusText:=GetLongHint(Hint); // update the screen; setting the hint doesn't always trigger an update when the mouse comes from the "ImageBoard" control; reason unknown
                Processed:=True;
                end
        else if X<LevelGroupBox.Left+LevelGroupBox.Width then begin
                APoint:=SolverGroupBox.ScreenToClient(ClientToScreen(Point(X,Y)));
                ControlMouseMove(SolverGroupBox,Shift,APoint.X,APoint.Y);
                Hint:=SolverGroupBox.Hint;
                StatusText:=GetLongHint(Hint); // update the screen; setting the hint doesn't always trigger an update when the mouse comes from the "ImageBoard" control; reason unknown
                Processed:=True;
                end
             else begin
                if Hint<>'' then Hint:='';
                if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
                StatusText:=GetLongHint(Hint); // update the screen; setting the hint doesn't always trigger an update when the mouse comes from the "ImageBoard" control; reason unknown
                Processed:=True;
                end;
        end
     else if ((Y>=PluginLevelGroupBox.Top+PluginLevelGroupBox.Height) and
              (Y< SolveLevelsGroupBox.Top)
             )
             or (Screen.Cursor=crVSplit) then begin
             APoint:=LevelGroupBox.ScreenToClient(ClientToScreen(Point(X,Y)));
             ControlMouseMove(LevelGroupBox,Shift,APoint.X,APoint.Y);
             Hint:=LevelGroupBox.Hint;
             StatusText:=GetLongHint(Hint); // update the screen; setting the hint doesn't always trigger an update when the mouse comes from the "ImageBoard" control; reason unknown
             Processed:=True;
             end;
  if not Processed then begin
     if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
     if Sender is TTabSheet then with Sender as TTabSheet do
        if Hint<>'' then Hint:='';
     if StatusText<>'' then StatusText:='';
     end;
  if PanelToolTips.Visible then PanelToolTips.Hide;
end;

function  TToolsForm.CloseEditors:Boolean;
begin
  Result:=True;
  if Assigned(OptimizerTaskQueue.Plugin) then OptimizerTaskQueue.Plugin.Enter;
  try     if OptimizationComboBox.Visible then with OptimizerTaskQueue do with StringGrid do begin
             OptimizationFlags[Row]:=Integer(OptimizationComboBox.Items.Objects[OptimizationComboBox.ItemIndex]);
             Cells[Ord(oscOptimization),Row]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+OptimizationComboBox.Items.Strings[OptimizationComboBox.ItemIndex];
             end;
  finally if Assigned(OptimizerTaskQueue.Plugin) then OptimizerTaskQueue.Plugin.Leave;
  end;
  if Result then HideEditors;
  IgnoreMouseUp:=True; // this hasn't much to do with closing the editors, but it happens to be a convenient spot to set the switch because 'CloseEditors' is called before all major events like opening a new level or opening the settings window
end;

function  TToolsForm.HideEditors:Boolean;
begin
  Result:=OptimizationComboBox.Visible;
  if Result                then OptimizationComboBox.Hide;
  if SelectBtn.Visible     then SelectBtn.Hide;
  if PanelToolTips.Visible then PanelToolTips.Hide;
end;

function  TTaskQueue.LookupSnapshot(Snapshot__:TExtendedSnapshotAsText; var OldLevel__:TLevel; var OldSnapshot__:TExtendedSnapshotAsText):Boolean;
var Level:TLevel;
begin // returns 'True' if a matching snapshot is on the queue;
  Level:=Plugin.SokoFile.GetLevelByExtendedSnapshotAsTextSerialNo(Snapshot__.SerialNo);
  Result:=Assigned(Level) and
          (not Level.BoardAsTextLines.IsEmpty) and
          (not Snapshot__.MovesAsTextLines.IsEmpty) and
          Plugin.Lookup(Level.Tag.BoardWidth,Level.Tag.BoardHeight,Level.BoardAsTextLines.First.Text,
                        Snapshot__.MovesAsTextLines.First.Text,
                        OldLevel__); // 'True': an identical snapshot is already on the optimizer queue
  if Result then OldSnapshot__:=TExtendedSnapshotAsText(OldLevel__.SnapshotsAsText.Last);
end;

procedure TTaskQueue.OnStringGridDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var i,ACol,ARow:Integer; HighlightedLevel:TLevel;
begin
  with StringGrid do
    if   (Sender=StringGrid) and (Source=StringGrid) then begin
         Plugin.Enter;
         try
           MouseToCell(X,Y,ACol,ARow);
           if (ARow>=FixedRows) and
              (ARow<RowCount) and
              (ARow<>Row) and
              Assigned(Levels[Row])
              then begin
              if (Self<>ToolsForm.GeneratorTaskQueue)
                 or
                 ((Cells[Ord(glcPushes),ARow]='') and (Cells[Ord(glcPushes),Row]='')) then begin // generator task queue: only non-calculated candidates can be moved; all calculated candidates must stay sorted in descending order on fitness
                 EndDrag(True);

                 HighlightedLevel:=Levels[HighlightedRowNumber];

                 for  ACol:=0 to Pred(ColCount) do if ACol<>Ord(oscNo) then Objects[ACol,Pred(FixedRows)]:=Objects[ACol,Row];
                 if   ARow<Row then
                      for i:=Row downto Succ(ARow) do begin
                          for ACol:=0 to Pred(ColCount) do
                              if ACol<>Ord(oscNo) then Objects[ACol,i]:=Objects[ACol,Pred(i)];
                          end
                 else for i:=Row to     Pred(ARow) do begin
                          for ACol:=0 to Pred(ColCount) do
                              if ACol<>Ord(oscNo) then Objects[ACol,i]:=Objects[ACol,Succ(i)];
                          end;
                 for  ACol:=0 to Pred(ColCount) do if ACol<>Ord(oscNo) then Objects[ACol,ARow]:=Objects[ACol,Pred(FixedRows)];

                 if   Assigned(HighlightedLevel) then
                      for i:=Min(ARow,Row) to Max(ARow,Row) do
                          if Levels[i]=HighlightedLevel then HighlightedRowNumber:=i;

                 if   ARow=FixedRows then
                      Plugin.SokoFile.Levels.MoveAfter(Levels[ARow],nil)
                 else Plugin.SokoFile.Levels.MoveAfter(Levels[ARow],Levels[Pred(ARow)]);

                 for i:=Min(ARow,Row) to Max(ARow,Row) do RefreshRow(i);

                 Row:=ARow;
                 Plugin.SokoFile.Modified:=True;
                 Self.Refresh(False);
                 ScrollInView(Row);
                 if Self=ToolsForm.GeneratorTaskQueue then
                    MainForm.Generator.SortCandidates(GA.Control.FitnessFunction);
                 end
              else EndDrag(False);
              end
           else EndDrag(False);
         finally Plugin.Leave;
         end;
         end
    else if Sender is TControl then
            TControl(Sender).EndDrag(False); // unknown sender/source
end;

procedure TTaskQueue.OnStringGridDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var ACol,ARow:Integer;
begin
  if   (Sender=StringGrid) and (Source=StringGrid) then with StringGrid do begin
       MouseToCell(X,Y,ACol,ARow);
       if   (ACol>=FixedCols) and (ARow>=FixedRows) and
            ((Self<>ToolsForm.GeneratorTaskQueue) or (Cells[Ord(glcPushes),ARow]='')) then
            Accept:=True
       else Accept:=False;
       if (ARow=TopRow) and (TopRow>FixedRows) then TopRow:=TopRow-1
       else if (ARow>=Pred(TopRow+VisibleRowCount)) and
               (TopRow+VisibleRowCount<RowCount) then
               TopRow:=TopRow+1;
       end;
end;

procedure TTaskQueue.OnStringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ACol,ARow:Integer;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not ToolsForm.Game.IsBusy) and
     ToolsForm.CloseEditors then with StringGrid do begin
     if Assigned(Plugin) then Plugin.Enter;
     try
       MouseToCell(X, Y, ACol, ARow);
       ToolsForm.IgnoreKeyUp:=False; ToolsForm.IgnoreMouseUp:=False;
       if (ACol<ColCount) and (ARow<RowCount ) then
          if (ACol>=FixedCols) and (ARow>=FixedRows) then begin
             if      (Button=mbLeft) and (not Dragging) and
                     //Assigned(Levels[ARow]) and
                     (Cells[Ord(oscNo),ARow]<>'')
                     then begin
                     if   (Self=ToolsForm.OptimizerTaskQueue) and
                          (ACol=Ord(oscOptimization)) then
                          ToolsForm.ShowOptimizationComboBox(ARow)
                     else if (Self<>ToolsForm.GeneratorTaskQueue) or (Cells[Ord(glcPushes),ARow]='') then
                             BeginDrag(False);
                     end
             else if (Button<>mbLeft) and (ARow<>Row) then begin
                     Row:=ARow;
                     end;
             end
          else if (Button=mbLeft) and (ARow=0) and (Screen.Cursor=crHSplit) then
                  ToolsForm.Editor.MouseButtonDown:=True;
       finally if Assigned(Plugin) then Plugin.Leave;
       end;
     end;
end;

procedure TTaskQueue.OnStringGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const SPLIT_DELTA=8;
var   i,j,ACol,ARow,PanelWidth:Integer; s:String; CheckBox:TCheckBox;
begin
  with StringGrid do begin
    s:='';
    MouseToCell(X,Y,ACol,ARow);
    if (ACol>=0) and (ACol<ColCount) and
       (ARow>=0) and (ARow<RowCount) and (ARow<=TopRow+VisibleRowCount) then begin
       s:=Cells[ACol,ARow];
       if ARow=0 then begin
          if ToolsForm.Editor.MouseButtonDown and (Screen.Cursor=crHSplit) then begin
             j:=0;
             for i:=0 to Pred(ToolsForm.Editor.DragPoint.X) do Inc(j,ColWidths[i]+GridLineWidth);
             if j>=X then begin
                if ColWidths[Pred(ToolsForm.Editor.DragPoint.X)]-(j-X)> 2*VerticalScrollBarWidth then begin
                   ColWidths[Pred(ToolsForm.Editor.DragPoint.X)]        :=ColWidths[Pred(ToolsForm.Editor.DragPoint.X)]-(j-X);
                   ColWidths[     ToolsForm.Editor.DragPoint.X ]        :=ColWidths[     ToolsForm.Editor.DragPoint.X ]+(j-X);
                   end;
                end
             else
                if ColWidths[     ToolsForm.Editor.DragPoint.X ]      > 2*VerticalScrollBarWidth then begin
                   ColWidths[     ToolsForm.Editor.DragPoint.X ]        :=ColWidths[     ToolsForm.Editor.DragPoint.X ]+(j-X);
                   ColWidths[Pred(ToolsForm.Editor.DragPoint.X)]        :=ColWidths[Pred(ToolsForm.Editor.DragPoint.X)]-(j-X);
                   end;
             s:=HintChangeColumnWidthText;
             ToolsForm.MakeAllColumnsFullyVisible;
             end
          else begin
             j:=0; ToolsForm.Editor.DragPoint.X:=-1;
             for i:=0 to Pred(ACol) do Inc(j,ColWidths[i]+GridLineWidth);
             if (X-j<=SPLIT_DELTA) and (ACol>Ord(oscNo)) then begin
                ToolsForm.Editor.DragPoint.X:=ACol;
                Screen.Cursor:=crHSplit;
                s:=HintChangeColumnWidthText;
                end
             else if (ACol>=Ord(oscNo)) and (ACol<Pred(ColCount)) then begin
                     Inc(j,ColWidths[ACol]+GridLineWidth);
                     if j-X<=SPLIT_DELTA then begin
                        ToolsForm.Editor.DragPoint.X:=Succ(ACol);
                        Screen.Cursor:=crHSplit;
                        s:=HintChangeColumnWidthText;
                        end;
                     end;
             if (Screen.Cursor<>crDefault) and (ToolsForm.Editor.DragPoint.X<0) then begin
                Screen.Cursor:=crDefault; s:='';
                end;
             end;
          end
       else
          if (Screen.Cursor<>crDefault) and (not ToolsForm.Editor.MouseButtonDown) then
             Screen.Cursor:=crDefault;
       end
    else
       if (Screen.Cursor<>crDefault) and (not ToolsForm.Editor.MouseButtonDown) then
          Screen.Cursor:=crDefault;

    if (s='')
       or
       ((ARow<FixedRows)
        and
        ((ACol<>Ord(oscNo))
         or
         (Screen.Cursor<>crDefault)
        )
       )
       or
       ((ARow>=FixedRows)
        and
        (ACol<=Ord(oscNo))
       )
       or
       (
       (Self=ToolsForm.OptimizerTaskQueue)
        and
        ToolsForm.OptimizationComboBox.Visible
       ) then begin
       if ToolsForm.PanelToolTips.Visible then ToolsForm.PanelToolTips.Hide;
       if Screen.Cursor<>crHSplit then begin
          s:='';
          if   ACol=Ord(oscSelect) then begin
               if Assigned(Plugin) then Plugin.Enter;
               try     if (ARow>=FixedRows) and (ARow<TopRow+VisibleRowCount) and
                          GetCheckBoxForRow(ARow,CheckBox) and
                          CheckBox.Visible and CheckBox.Enabled then
                          s:=HintTaskQueueSelectItemText;
               finally if Assigned(Plugin) then Plugin.Leave;
               end;
               end;
          end;
       end
    else if (X>=Tag) then begin
       if ToolsForm.PanelToolTips.Visible then ToolsForm.PanelToolTips.Hide;
       end
    else with ToolsForm.PanelToolTips do begin
       if   Caption<>s then begin
            PanelWidth:=ToolsForm.Canvas.TextWidth(s)+8; Caption:=''; Width:=1;
            end
       else PanelWidth:=Width;
       if   True or (ACol<Ord(oscMetrics)) then
            // show tooltips to the right
            if   X+OpenForm.ToolTips.OffsetX+PanelWidth<StringGrid.ClientWidth-2 then
                 Left:=StringGrid.Left+X+OpenForm.ToolTips.OffsetX+StringGrid.Parent.Left+ToolsForm.PageControl1.ActivePage.Left+StringGrid.Parent.Parent.Left
            else Left:=StringGrid.Left+StringGrid.ClientWidth+StringGrid.Parent.Left+ToolsForm.PageControl1.ActivePage.Left+StringGrid.Parent.Parent.Left-PanelWidth-2
       else // show tooltips to the left
            Left:=Min(StringGrid.Left+X+OpenForm.ToolTips.OffsetX+StringGrid.Parent.Left+ToolsForm.PageControl1.ActivePage.Left+StringGrid.Parent.Parent.Left-PanelWidth,
                      ToolsForm.OptimizeSolutionsGridPanel.Left+ToolsForm.OptimizeSolutionsGridPanel.ClientWidth-PanelWidth-VerticalScrollBarWidth);
       Top :=Min(StringGrid.Top +Y+OpenForm.ToolTips.OffsetY+StringGrid.Parent.Top +ToolsForm.PageControl1.ActivePage.Top +StringGrid.Parent.Parent.Top,
                 ToolsForm.StatusBar1.Top-Height-4);
       if   Caption<>s then begin
            Width:=PanelWidth; Caption:=s;
            end;

       if   (Self<>ToolsForm.GeneratorTaskQueue) or (not Assigned(Plugin)) or (not Plugin.IsActive) then begin
            if not ToolsForm.PanelToolTips.Visible then ToolsForm.PanelToolTips.Show;
            end
       else if ToolsForm.PanelToolTips.Visible then ToolsForm.PanelToolTips.Hide; // don't show the tool tip panel while the generator is running; otherwise, there is a lot of flicker if candidates frequently change position in the population as the result of new candidates replacing old ones

       if   (ARow=HighlightedRowNumber)
            or
            (ACol<>Ord(oscOptimization))
            or
            (Self<>ToolsForm.OptimizerTaskQueue) then
            s:=''
       else s:=HintClickToSelectOptimizationText;
       end;

    if s='' then
       if   (ARow<FixedRows) and (ARow>=0) then begin
            if   Self<>ToolsForm.GeneratorTaskQueue then begin
                 if   RowCount>Succ(FixedRows) then
                      if   ACol<>Ord(oscNo) then
                           s:=HintSortOnColumnText
                      else s:=HintSortOnChronologicalOrderText
                 else s:=HintRightClickToShowMenuText;
                 end
            else if   ACol<>Ord(glcFitness) then
                      s:=HintRightClickToShowMenuText
                 else s:=HintFitnessText+HintRightClickToShowMenuText;
            end
       else if ARow<RowCount then
               if      Self=ToolsForm.OptimizerTaskQueue then begin
                       if   not ToolsForm.OptimizationComboBox.Visible then
                            s:=HintClickToSelectSolutionText;
                       end
               else if Self=ToolsForm.SolverTaskQueue then begin
                       s:=HintClickToSelectLevelText;
                       end;

    with ToolsForm.StatusBar1.Panels[1] do
      if Text<>s then Text:=s;
    end;
end;

procedure TTaskQueue.OnStringGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ACol,ARow:Integer; APoint:TPoint; CheckBox:TCheckBox; //OldSortMetric:TGameMetricsExtended;
begin
  with StringGrid do begin
    if Dragging then EndDrag(False);
    if      Button=mbLeft then begin
            MouseToCell(X,Y,ACol,ARow);
            if Screen.Cursor<>crHSplit then begin
               if      (ARow<FixedRows) and (ARow>=0) then begin
                       if Self<>ToolsForm.GeneratorTaskQueue then begin
                          if      ACol=Ord(oscSelect      ) then ToolsForm.MenuItemSortClick(ToolsForm.PopupMenuItemSortOnSelected)
                          else if ACol=Ord(oscNo          ) then ToolsForm.MenuItemSortClick(ToolsForm.PopupMenuItemSortOnChronologicalOrder)
                          else if ACol=Ord(oscLevelName   ) then ToolsForm.MenuItemSortClick(ToolsForm.PopupMenuItemSortOnLevelNames)
                          else if Self=ToolsForm.OptimizerTaskQueue then begin
                                  if      ACol=Ord(oscSnapShotName) then ToolsForm.MenuItemSortClick(ToolsForm.PopupMenuItemSortOnSolutionNames)
                                  else if ACol=Ord(oscMetrics     ) then ToolsForm.MenuItemSortClick(nil) // sort on metrics
                                  else if ACol=Ord(oscOptimization) then ToolsForm.MenuItemSortClick(ToolsForm.PopupMenuItemSortOnOptimization);
                                  end
                          else ToolsForm.MenuItemSortClick(nil); // sort on metrics
                          end
                       else begin
                          //OldSortMetric:=ToolsForm.SortMetric;
                          //if      ACol=Ord(glcPushes) then ToolsForm.MenuItemSortClick(ToolsForm.PopupMenuItemSortOnPushes)
                          //else if ACol=Ord(glcScore ) then ToolsForm.MenuItemSortClick(GeneratorForm.RadioButtonFitnessBoxLinesAndBoxChanges);
                          //ToolsForm.SortMetric:=OldSortMetric;
                          end;
                  end
               else if ACol=Ord(oscSelect) then with StringGrid do begin
                       if Assigned(Plugin) then Plugin.Enter;
                       try     if (ARow>=FixedRows) and (ARow<TopRow+VisibleRowCount) and
                                  GetCheckBoxForRow(ARow,CheckBox) and
                                  CheckBox.Visible and CheckBox.Enabled then
                                  OnCheckBoxMouseUp(CheckBox,Button,Shift,0,0);
                       finally if Assigned(Plugin) then Plugin.Leave;
                       end;
                       end;
               end;
            end
    else if Button=mbRight then begin
            if ToolsForm.PanelToolTips.Visible then ToolsForm.PanelToolTips.Hide;
            APoint:=ClientToScreen(Point(Left+X-16,Top+Y-16));
            Self.PopupMenu.Popup(APoint.X,APoint.Y);
            end;
    if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
    ToolsForm.Editor.MouseButtonDown:=False;
    end;
end;

procedure TTaskQueue.OnStringGridTopLeftChanged(Sender: TObject);
begin
  if Assigned(Plugin) then with StringGrid do begin
     Plugin.Enter;
     try
       if LeftCol<>FixedCols then begin
          LeftCol:=FixedCols; // ensure that the user cannot scroll the grid horizontally; it would ruin things like checkbox positions and the optimizer task-queue optimization-combobox position
          end;
       ScrollInView(StringGrid.TopRow);
     finally Plugin.Leave;
     end;
     end;
end;

procedure TTaskQueue.OnCheckBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ARow:Integer;
begin
  if Sender is TCheckBox then with Sender as TCheckBox do begin
     if (X>=0) and (X<Width) and
        (Y>=0) and (Y<Height) and
        ((Button=mbLeft)
         or
         ((Sender=ItemsCheckBox)
           and
          (Button=mbRight)
         )
        ) and
        (not ToolsForm.IgnoreMouseUp) then begin
        Plugin.Enter;
        try     if Sender=ItemsCheckBox then with StringGrid do begin
                   if      Button=mbLeft then  // select/unselect all items
                           if   Checked then
                                for ARow:=FixedRows to Pred(RowCount) do Selected[ARow]:=False
                           else for ARow:=FixedRows to Pred(RowCount) do Selected[ARow]:=True
                   else if Button=mbRight then // toggle selection for all items
                           for ARow:=FixedRows to Pred(RowCount) do Selected[ARow]:=not Selected[ARow];
                   end
                else with StringGrid do begin
                   ARow:=Row;
                   Row:=TCheckBox(Sender).Tag; Col:=Ord(oscSelect);
                   Selected[Row]:=not Selected[Row];
                   if   Plugin is TSolverPlugin then
                        if   ToolsForm.SolveLevelsGroupBox.Visible then
                             StringGrid.SetFocus
                        else begin end
                   else StringGrid.SetFocus;
                   if ARow=Row then RefreshRow(Row);
                   end;
                if (Plugin is TSolverPlugin) and
                   (not ToolsForm.SolveLevelsGroupBox.Visible) and
                   (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver) then
                   ToolsForm.LoadLevelFromEditor(True,True);
                OpenForm.EnableDisablePluginControls(Self,Plugin,False);
                ToolsForm.ShowStatus;
        finally Plugin.Leave;

        end;
        end;
     if Assigned(Plugin) and (not MainForm.ShutDownApplication) and (Screen.ActiveForm=ToolsForm) then // all these guards are probably not necessary, but they don't hurt
        if   Plugin is TSolverPlugin then
             if   ToolsForm.SolveLevelsGroupBox.Visible then
                  StringGrid.SetFocus
             else begin end
        else StringGrid.SetFocus;
     end;
  ToolsForm.IgnoreKeyUp:=False;
  ToolsForm.IgnoreMouseUp:=False;
end;

procedure TTaskQueue.OnStringGridKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Assigned(ToolsForm) and (Key<>SIMULATED_KEYBOARD_EVENT_KEY) then begin
     if   ToolsForm.IgnoreKeyUp or ToolsForm.IgnoreMouseUp then begin
          ToolsForm.IgnoreKeyUp:=False;
          ToolsForm.IgnoreMouseUp:=False;
          end
     else if Key=VK_RETURN then with StringGrid do
             if      //{Col=Ord(oscSelect)
                     (Self<>ToolsForm.OptimizerTaskQueue) or
                     (Col<Ord(oscOptimization)) then begin
                     Selected[Row]:=not Selected[Row];
                     OpenForm.EnableDisablePluginControls(Self,Plugin,False);
                     ToolsForm.ShowStatus;
                     end
             else if (Self=ToolsForm.OptimizerTaskQueue) and
                     (Col=Ord(oscOptimization)) and
                     Assigned(Levels[Row]) then
                     ToolsForm.ShowOptimizationComboBox(Row);
     end;
end;

procedure TToolsForm.FillOptimizationComboBox;
var i,j:Integer; s:String; PrimaryMetric,SecondaryMetric:TGameMetrics;
begin
  with OptimizationComboBox do begin
    Clear;
    Items.AddObject(SettingsText,nil);
    if OptimizerTaskQueue.Plugin.HasIsSupportedOptimizationFunction then begin
       for PrimaryMetric:=Low(PrimaryMetric) to High(PrimaryMetric) do
           for SecondaryMetric:=Low(SecondaryMetric) to High(SecondaryMetric) do
               if OptimizerTaskQueue.Plugin.IsSupportedOptimization(PRIMARY_METRICS_FLAGS[PrimaryMetric]+SECONDARY_METRICS_FLAGS[SecondaryMetric]) then begin
                  s:=GameMetricsText[PrimaryMetric]+', '+AnsiLowerCase(GameMetricsText[SecondaryMetric]);
                  Items.AddObject(s,TObject(PRIMARY_METRICS_FLAGS[PrimaryMetric]+SECONDARY_METRICS_FLAGS[SecondaryMetric]));
                  end;
       for PrimaryMetric:=Low(PrimaryMetric) to High(PrimaryMetric) do
           if OptimizerTaskQueue.Plugin.IsSupportedOptimization(PRIMARY_METRICS_FLAGS[PrimaryMetric]) then
              Items.AddObject(GameMetricsText[PrimaryMetric],TObject(PRIMARY_METRICS_FLAGS[PrimaryMetric]));
       end
    else begin // use the deprecated 'GetSupportedOptimizations()' function instead of 'IsSupportedOptimization()'
       j:=OptimizerTaskQueue.Plugin.SupportedOptimizations;
       // add primary+secondary optimizations
       for PrimaryMetric:=Low(PrimaryMetric) to High(PrimaryMetric) do
           if (j and PRIMARY_METRICS_FLAGS[PrimaryMetric])<>0 then begin
              Tag:=0;
              for SecondaryMetric:=Low(SecondaryMetric) to High(SecondaryMetric) do
                  if (PrimaryMetric<>SecondaryMetric) and
                     ((j and SECONDARY_METRICS_FLAGS[SecondaryMetric])<>0) then begin
                     Tag:=j;
                     s:=GameMetricsText[PrimaryMetric]+', '+AnsiLowerCase(GameMetricsText[SecondaryMetric]);
                     Items.AddObject(s,TObject(j and (PRIMARY_METRICS_FLAGS[PrimaryMetric]+SECONDARY_METRICS_FLAGS[SecondaryMetric])));
                     end;
              if Tag=0 then // 'True': no secondary metrics listed, hence, add an item with the primary metric only
                 // Items.AddObject(GameMetricsText[PrimaryMetric],TObject(PRIMARY_METRICS_FLAGS[PrimaryMetric]));
                 j:=j or (65536*PRIMARY_METRICS_FLAGS[PrimaryMetric]); // add the primary-only optimation later, after all primary+secondary optimizations, if any
              end;
       // add primary optimizations without secondary optimizations
       for PrimaryMetric:=Low(PrimaryMetric) to High(PrimaryMetric) do
           if (j and (65536*PRIMARY_METRICS_FLAGS[PrimaryMetric]))<>0 then
              Items.AddObject(GameMetricsText[PrimaryMetric],TObject(PRIMARY_METRICS_FLAGS[PrimaryMetric]));
       end;

    Tag:=0;
    for i:=0 to Pred(Items.Count) do SetComboBoxDropDownWidth(OptimizationComboBox,i,False);
    end;
end;

procedure TToolsForm.ShowOptimizationComboBox(Row__:Integer);
var i,j:Integer; Level:TLevel;
begin
  SelectBtn.Hide;
  if Assigned(OptimizerTaskQueue.Plugin) then OptimizerTaskQueue.Plugin.Enter;
  try
    if Row__<>OptimizerTaskQueue.HighlightedRowNumber then with OptimizationComboBox do begin
       if Items.Count=0 then FillOptimizationComboBox;
       with OptimizerTaskQueue do with StringGrid do begin
         ScrollInView(Row);
         i:=(DefaultRowHeight+GridLineWidth)*(FixedRows+Row__-TopRow)+Max(0,((DefaultRowHeight-OptimizationComboBox.Height) div 2));
         end;
       Top:=i;
       ItemIndex:=0;
       j:=OptimizerTaskQueue.OptimizationFlags[Row__];
       for i:=0 to Pred(Items.Count) do
           if j=Integer(Items.Objects[i]) then begin
              ItemIndex:=i; break;
              end;
       if PanelToolTips.Visible then PanelToolTips.Hide;

       Level:=OptimizerTaskQueue.Levels[Row__];
       if Assigned(Level) and (TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.First.Text<>'') then begin
          Show;
          SetFocus;
          StatusText:=Items[ItemIndex];
          end;
       end
    else
       OptimizerTaskQueue.StringGrid.SetFocus;
  finally if Assigned(OptimizerTaskQueue.Plugin) then OptimizerTaskQueue.Plugin.Leave;
  end;
end;

procedure TToolsForm.OptimizationComboBoxKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if      Key=VK_RETURN then with OptimizerTaskQueue.StringGrid do
          begin CloseEditors;
                SetFocus;
                Key:=0;
          end
  else if Key in [VK_HOME,VK_END] then with OptimizerTaskQueue.StringGrid do
          begin HideEditors;
                SetFocus;
                PostMessage(Handle, WM_KEYDOWN, Key, 0);
                Key:=0;
          end
  else if Key=VK_ESCAPE then with OptimizerTaskQueue.StringGrid do begin
          ShowOptimizationComboBox(Row); Key:=0; CanEscape:=False;
          end;
end;

procedure TToolsForm.OptimizationComboBoxExit(Sender: TObject);
begin
  CloseEditors;
end;

procedure TToolsForm.OptimizationComboBoxChange(Sender: TObject);
begin
  if ActiveControl=OptimizationComboBox then with OptimizationComboBox do begin
     StatusText:=Text;
     //with OptimizerTaskQueue do OptimizationFlags[StringGrid.Row]:=Integer(OptimizationComboBox.Items.Objects[OptimizationComboBox.ItemIndex]);
     end;
end;

procedure TToolsForm.OptimizeSolutionsStringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var OldSelectedSquaresAsText:String; Level:TLevel;
begin
  if Assigned(OptimizerTaskQueue) then with OptimizeSolutionsStringGrid do begin
    CanSelect:=ARow>=FixedRows;
    if CanSelect then begin
       if (ACol<>Ord(oscOptimization))
          or
          (Cells[ACol,ARow]='') then begin
          if ACol<ColCount then StatusText:=Cells[ACol,ARow];
          CloseEditors;
          end
       else begin
          if   ARow<>OptimizerTaskQueue.HighlightedRowNumber then
               StatusText:=Cells[ACol,ARow]+SPACE+SPACE+LEFT_PAREN+HintClickOrPressEnterToSelectOptimizationText+RIGHT_PAREN
          else StatusText:=Cells[ACol,ARow];
          if (ARow>=TopRow) and (ARow<TopRow+VisibleRowCount) then begin
             SelectBtn.Top:=Top+(FixedRows+ARow-TopRow)*(DefaultRowHeight+GridLineWidth);
             //SelectBtn.Visible:=True;
             end;
          end;
       if (ARow<>Row) or (Sender=nil) then with PluginLevelInfo.ReplayInfo do with BoardAsText do begin
          Level:=nil;
          if Assigned(OptimizerTaskQueue.Plugin) then OptimizerTaskQueue.Plugin.Enter;
          try
            if ARow<>OptimizerTaskQueue.OldStringGridRow then with OptimizerTaskQueue do begin
               RefreshRow(OptimizerTaskQueue.OldStringGridRow);
               OptimizerTaskQueue.OldStringGridRow:=ARow;
               RefreshRow(ARow);
               end;
            if (Sender=nil)
               and
               (Game.IsReplaying
                or
                (ACol=ColCount) // kludge: 'ColCount' signals not to reload the current level; see 'Open1_.TOpenForm.SynchronizedPluginCallback()'
               )
               then begin
               end
            else begin
               PluginToolButtonStopReplayClick(nil);
               Level:=OptimizerTaskQueue.Levels[ARow];
               if Assigned(Level) then begin
                  LevelNamePanel.Hint   :=StrWithQuotedAmpersands(VisualFileName(Level.Name));
                  LevelNamePanel.Caption:=SPACE+LevelNamePanel.Hint+SPACE; // the first space is for pretty-printing in the panel; the second space is for a better balance in the "mouse-over" tool-tip

                  IsLoaded              :=False;
                  MoveCount             :=0;
                  MovesAsText           :='';
                  PluginFileName        :='';
                  FillChar(SelectedRange,SizeOf(SelectedRange),0);
                  OldSelectedSquaresAsText:=PluginLevelInfo.SelectedSquaresAsText;
                  PluginLevelInfo.SelectedSquaresAsText:='';

                  if not Level.SnapshotsAsText.IsEmpty then begin
                     MovesAsText        :=TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.First.Text;
                     if Level.SnapshotsAsText.Last is TExtendedSnapshotAsText then
                        with TExtendedSnapshotAsText(Level.SnapshotsAsText.Last) do begin
                          PluginLevelInfo.ReplayInfo.SelectedRange[0] := SelectedRange[0];
                          PluginLevelInfo.ReplayInfo.SelectedRange[1] := SelectedRange[1];
                          PluginLevelInfo.ReplayInfo.SelectedRange[2] := SelectedRange[2];
                          PluginLevelInfo.ReplayInfo.SelectedRange[6] := SelectedRange[6];
                          end;
                     TSnapshotAsText(Level.SnapshotsAsText.Last).Notes.Lines.ReadString(KEY_SELECTED_SQUARES,PluginLevelInfo.SelectedSquaresAsText);
                     end;

                  if (Sender                  = nil) or
                     (Width                   <>Level.Tag.BoardWidth) or
                     (Height                  <>Level.Tag.BoardHeight) or
                     (Board                   <>Level.BoardAsTextLines.First.Text) or
                     (OldSelectedSquaresAsText<>PluginLevelInfo.SelectedSquaresAsText) then begin
                     Width                    :=Level.Tag.BoardWidth;
                     Height                   :=Level.Tag.BoardHeight;
                     Board                    :=Level.BoardAsTextLines.First.Text;
                     if   OpenForm.Game.LoadFromBoardAsText(Width,Height,False,False,False,False,Board)
                          and
                          OpenForm.Game.LoadSnapshotAsTextString(MovesAsText,True)
                          and
                          ((OpenForm.Game.History.Top>OpenForm.Game.ForcedInitialJumps)
                           or
                           (MovesAsText=''))
                          and
                          ((PluginLevelInfo.SelectedSquaresAsText='')
                           or
                           (SelectedSquaresAsTextToBoard(PluginLevelInfo.SelectedSquaresAsText,OpenForm.Game.BoardWidth,OpenForm.Game.BoardHeight,OpenForm.Game.Board)>=0))
                          then begin
                          LevelSetForm.ShowBoard0(OpenForm.Game.BoardWidth,OpenForm.Game.BoardHeight,OpenForm.Game.Board,LevelSetForm.GameViewer.BoardImage,False);
                          end
                     else LevelSetForm.ShowBoard0(0                       ,                        0,OpenForm.Game.Board,LevelSetForm.GameViewer.BoardImage,False);
                     if   Game.IsReplaying then
                          Inc(ToolsForm.FormResizeCount); // force an update so the new game shows up on the screen when the replay-loop exits
                     end;

                  ShowOptimizationRange(-1);
                  end
               else begin
                  ClearPluginReplayInfo;
                  LevelNamePanel.Hint:='';
                  LevelNamePanel.Caption:='';
                  LevelSetForm.ShowBoard0(0,0,OpenForm.Game.Board,LevelSetForm.GameViewer.BoardImage,False);
                  LoadLevelFromEditor(True,True);
                  end;
               end;
          finally if   Assigned(OptimizerTaskQueue.Plugin) then OptimizerTaskQueue.Plugin.Leave;
                  if   (Sender=nil) and Self.Visible and (PageControl1.ActivePage=TabSheetOptimizer) and (Screen.ActiveForm=Self) then
                       if Assigned(Level) then OptimizeSolutionsStringGrid.SetFocus;
                  if   PageControl1.ActivePage<>TabSheetOptimizer then
                       ShowStatus
                  else try    ShowStatusPlugin(MainForm.Optimizer,ARow); // ensure that the status reflects the contents in row 'ARow'; it may differ from 'Row' which hasn't been updated yet
                       except on E:Exception do begin end; // e.g., to catch 'access denied' exceptions when the application is deactivated by a password-protected screen saver
                       end;
          end;
          end;
       end;
    end;
end;

procedure TToolsForm.GenerateLevelsStringGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var NewMovesAsText:String; Level:TLevel;
begin
  if Assigned(GeneratorTaskQueue) then with GeneratorTaskQueue do with StringGrid do begin
    CanSelect:=(ARow>=FixedRows); //and (ACol=Ord(slcLevelName));
    if CanSelect then begin
       if (ACol<ColCount) and (PageControl1.ActivePage=TabSheetGenerator) then
          StatusText:=Cells[ACol,ARow];
       CloseEditors;
       if (ARow<>Row) or (Sender=nil) then with PluginLevelInfo.ReplayInfo do with BoardAsText do begin
          Level:=nil;
          if Assigned(Plugin) then Plugin.Enter;
          try
            if ARow<>OldStringGridRow then begin
               RefreshRow(OldStringGridRow);
               OldStringGridRow:=ARow;
               RefreshRow(ARow);
               end;

            if (PageControl1.ActivePage<>TabSheetGenerator)
               or
               ((Sender=nil)
                and
                (Game.IsReplaying
                 or
                 (ACol=ColCount) // kludge: 'ColCount' signals not to reload the current level; see 'Open1_.TOpenForm.SynchronizedPluginCallback()'
                )
               )
               then begin
               CanSelect:=False;
               end
            else begin
               Level:=Levels[ARow];
               if Assigned(Level) then begin
                  LevelNamePanel.Hint      :=StrWithQuotedAmpersands(VisualFileName(Level.Name));
                  LevelNamePanel.Caption   :=SPACE+LevelNamePanel.Hint+SPACE; // the first space is for pretty-printing in the panel; the second space is for a better balance in the "mouse-over" tool-tip

                  if   Level.SnapshotsAsText.IsEmpty or TSnapshotAsText(Level.SnapshotsAsText.First).MovesAsTextLines.IsEmpty then
                       NewMovesAsText      :=''
                  else NewMovesAsText      :=TSnapshotAsText(Level.SnapshotsAsText.First).MovesAsTextLines.First.Text;

                  if   IsLoaded and (MoveCount<>0) and
                       (Width              = Level.Tag.BoardWidth) and
                       (Height             = Level.Tag.BoardHeight) and
                       (Board              = Level.BoardAsTextLines.First.Text) and
                       (MovesAsText        = NewMovesAsText) then begin
                       end
                  else begin PluginToolButtonStopReplayClick(nil);
                             IsLoaded      :=False;
                             MovesAsText   :=NewMovesAsText;
                             MoveCount     :=0;
                             PluginFileName:='';
                             FillChar(SelectedRange,SizeOf(SelectedRange),0);
                       end;

                  if (Sender            = nil) or
                     (Width             <>Level.Tag.BoardWidth) or
                     (Height            <>Level.Tag.BoardHeight) or
                     (Board             <>Level.BoardAsTextLines.First.Text) then begin
                     Width              :=Level.Tag.BoardWidth;
                     Height             :=Level.Tag.BoardHeight;
                     Board              :=Level.BoardAsTextLines.First.Text;
                     if   OpenForm.Game.LoadFromBoardAsText(Width,Height,PageControl1.ActivePage=TabSheetGenerator,False,MovesAsText<>'',False,Board)
                          and
                          OpenForm.Game.LoadSnapshotAsTextString(MovesAsText,True)
                          and
                          ((OpenForm.Game.History.Top>OpenForm.Game.ForcedInitialJumps)
                           or
                           (MovesAsText='')
                          ) then begin
                          LevelSetForm      .ShowBoard0(OpenForm.Game.BoardWidth,OpenForm.Game.BoardHeight,OpenForm.Game.Board,LevelSetForm.GameViewer.BoardImage,False);
                          end
                     else begin LevelSetForm.ShowBoard0(0                       ,                        0,OpenForm.Game.Board,LevelSetForm.GameViewer.BoardImage,False);
                                IsLoaded:=False;
                          end;
                     if   Game.IsReplaying and (not IsLoaded) then
                          Inc(ToolsForm.FormResizeCount); // force an update so the new game shows up on the screen when the replay-loop exits
                     end;
                  end
               else begin
                  ClearPluginReplayInfo;
                  LevelNamePanel.Hint:='';
                  LevelNamePanel.Caption:='';
                  LevelSetForm.ShowBoard0(0,0,OpenForm.Game.Board,LevelSetForm.GameViewer.BoardImage,False);
                  //LoadLevelFromEditor(True,True);
                  end;
               end;
          finally if   Assigned(Plugin) then Plugin.Leave;
                  if   (Sender=nil) and Self.Visible and (PageControl1.ActivePage=TabSheetGenerator) and (Screen.ActiveForm=Self) then
                       if Assigned(Level) then GenerateLevelsStringGrid.SetFocus;
                  if   PageControl1.ActivePage<>TabSheetGenerator then
                       ShowStatus
                  else try    ShowStatusPlugin(Plugin,ARow); // ensure that the status reflects the contents in row 'ARow'; it may differ from 'Row' which may not have been updated yet
                       except on E:Exception do begin end; // e.g., to catch 'access denied' exceptions when the application is deactivated by a password-protected screen saver
                       end;
          end;
          end;
       end;
    end;
end;

procedure TToolsForm.OnOptimizerRefresh(var Msg: TMessage); // Message Misc_.MSG_OPTIMIZER_REFRESH;
begin
  if Self.Visible and (PageControl1.ActivePage=TabsheetOptimizer) then
     with OptimizerTaskQueue do
       case Msg.WParam of
         MSG_PLUGIN_REFRESH_CHECKBOXES : ScrollInView(StringGrid.TopRow);
         MSG_PLUGIN_REFRESH_COMPACT    : Refresh(True);
         MSG_PLUGIN_REFRESH_IMPORT     : ImportFromOtherPlugins(True); // e.g., transfer solutions found by a solver to the optimizer task queue
         else                            Refresh(Msg.WParam<>0); // '<>0': compact the lines if 'Msg.WParam' <> 0
         end;
end;

procedure TToolsForm.OnSolverRefresh(var Msg: TMessage); // Message Misc_.MSG_SOLVER_REFRESH;
begin
  if Self.Visible and (PageControl1.ActivePage=TabsheetSolver) then
     with SolverTaskQueue do
       case Msg.WParam of
         MSG_PLUGIN_REFRESH_CHECKBOXES   : ScrollInView(StringGrid.TopRow);
         MSG_PLUGIN_REFRESH_COMPACT      : Refresh(True);
         else                              Refresh(Msg.WParam<>0); // '<>0': compact the lines if 'Msg.WParam' <> 0
     end;
end;

procedure TToolsForm.OnGeneratorRefresh(var Msg: TMessage); // Message Misc_.MSG_GENERATOR_REFRESH;
var ARow:Integer; b:Boolean;
begin
  if Self.Visible and (PageControl1.ActivePage=TabsheetGenerator) then
     with GeneratorTaskQueue do
       case Msg.WParam of
         MSG_PLUGIN_REFRESH_CHECKBOXES   : ScrollInView(StringGrid.TopRow);
         MSG_PLUGIN_REFRESH_COMPACT      : Refresh(True);
         MSG_PLUGIN_REFRESH_ROW          : with StringGrid do begin
                                             if Assigned(Plugin) then Plugin.Enter;
                                             try     for ARow:=FixedRows to Pred(RowCount) do // refresh all empty rows, if any
                                                         if not Assigned(Levels[ARow]) then RefreshRow(ARow);
                                                     RefreshRow(Row); // refresh current row
                                                     GenerateLevelsStringGridSelectCell(nil,Col,Row,b);
                                             finally if Assigned(Plugin) then Plugin.Leave;
                                             end;
                                             end;
         else                              Refresh(Msg.WParam<>0); // '<>0': compact the lines if 'Msg.WParam' <> 0
     end;
end;

procedure TToolsForm.OnStatus(var Msg: TMessage); // Message Misc_.MSG_STATUS;
begin
  ShowStatus;
end;

procedure TTaskQueue.OnStringGridEnter(Sender: TObject);
begin
  RefreshRow(StringGrid.Row);
end;

procedure TTaskQueue.OnStringGridExit(Sender: TObject);
begin
  RefreshRow(StringGrid.Row);
  ToolsForm.StatusText:='';
end;

procedure TToolsForm.OnTest(var Msg: TMessage); // Message Misc_.MSG_TEST;
begin
// MenuItemWindowSizeClick(SettingsMenuItemWindowSizeMaximized);
end;

procedure TToolsForm.PluginEditMenuItemDeleteLevelsClick(Sender: TObject);
var ARow,Count,Index:Integer; b:Boolean; Level:TLevel; CurrentTaskQueue:TTaskQueue;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors then begin
     if      PluginForCurrentTabSheet is TSolverPlugin then
             CurrentTaskQueue:=SolverTaskQueue
     else if PluginForCurrentTabSheet is TOptimizerPlugin then
             CurrentTaskQueue:=OptimizerTaskQueue
     else if PluginForCurrentTabSheet is TGenerator then
             CurrentTaskQueue:=GeneratorTaskQueue
     else    raise Exception.Create(InternalErrorText+' (TToolsForm.PluginEditMenuItemDeleteLevelsClick)');
     with CurrentTaskQueue do begin
        if Assigned(Plugin) then Plugin.Enter;
        try     Count:=0;
                for ARow:=Pred(StringGrid.RowCount) downto 0 do begin
                    Level:=Levels[ARow];
                    if Selected[ARow] and Assigned(Level) then begin
                       Selected[ARow]:=False;
                       Levels[ARow]:=nil;
                       Inc(Count);

                       if CurrentTaskQueue=GeneratorTaskQueue then {update the generator's population}
                          if  GALookupLevel(Level,Index) then begin
                              if GA.Individuals[Index].Fitness < 0 then
                                 {It's a seed level which hasn't been calculated yet. Decrement the 'candidates' count.
                                  Note that it's not strictly correct to decrement the counter here because the counter is also
                                  used for numbering the calculated candidates, but it looks more correct for the user when the
                                  counter is decremented. In the unlikely situation that the task list contains several seed levels,
                                  and the user starts the generator, processes some seed levels, stops the generator, and then deletes
                                  one or more remaining uncalculated seed levels, then when the generator is started again, it may
                                  produce duplicate candidate numbers. This has no practical consequences, and unless a duplicate
                                  number happens to be alive as one of the  best candidates for a long time, the user won't even
                                  notice it.
                                 }
                                 Dec(GA.Statistics.CandidatesCount);
                              GADeleteIndividual(Index);
                              Plugin.SokoFile.Modified:=True;
                              end;

                       if   (ltfNew in Level.Tag.Flags) or (ltfLocked in Level.Tag.Flags) then begin
                            // this is a new solution, or it's currently being processed; keep it on the internal queue but hide it from the user
                            Level.Flags:=-1;
                            if ((Level.Tag.Flags*[ltfNew,ltfProcessed])=[ltfNew]) and
                               Assigned(Plugin) and (Plugin is TSolverPlugin) then
                               Include(Level.Tag.Flags,ltfProcessed); // the optimizer requires '[lftNew,ltfProcessed]' for importing the level
                            end
                       else Plugin.SokoFile.Levels.Remove(Level,True); // destroy the item, note that except for the generator, this is not the level or the solution itself; it's only the item on the task queue
                       end;
                    end;
                if Count<>0 then begin
                   Refresh(True);
                   OpenForm.EnableDisablePluginControls(ToolsForm,Plugin,False);
                   {
                   if CurrentTaskQueue=GeneratorTaskQueue then
                      if (IsEmpty or (GA.Control.PopulationSize=0)) and
                         IsANewFileName(MainForm.Generator.FileName) then begin
                         MainForm.Generator.SokoFile.Modified:=False;
                         ShowStatus;
                         end;
                   }
                   if IsEmpty then with ToolsForm do begin
                      if (CurrentTaskQueue=GeneratorTaskQueue) and
                         Assigned(MainForm.Generator) and
                         (not MainForm.Generator.IsActive) and
                         ((not MainForm.Generator.HasSokoFile) or (MainForm.Generator.SokoFile.Levels.IsEmpty)) and
                         PluginToolButtonNewCandidateSet.Enabled then
                         MainForm.Generator.Clear;
                      //ClearPluginReplayInfo;
                      //LevelNamePanel.Hint:='';
                      //LevelNamePanel.Caption:='';
                      //LevelSetForm.ShowBoard0(0,0,OpenForm.Game.Board,LevelSetForm.GameViewer.BoardImage,False);
                      LoadLevelFromEditor(True,True);
                      ShowStatus;
                      end
                   else
                      if        PluginForCurrentTabSheet is TOptimizerPlugin then
                                ToolsForm.OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,b)
                      else if   PluginForCurrentTabSheet is TSolverPlugin then
                                ToolsForm.SolveLevelsStringGridSelectCell      (nil,0,StringGrid.Row,b)
                      else if   PluginForCurrentTabSheet is TGenerator then
                                ToolsForm.GenerateLevelsStringGridSelectCell   (nil,0,StringGrid.Row,b)
                           else ShowStatus;
                   end;
        finally if Assigned(Plugin) then Plugin.Leave;
        end;
        end;
     end;
end;

procedure TToolsForm.PluginEditMenuItemResetOptimizationMethodClick(
  Sender: TObject);
var ARow:Integer; Level:TLevel; CurrentTaskQueue:TTaskQueue;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then begin
     if   PageControl1.ActivePage=TabSheetOptimizer then
          CurrentTaskQueue:=OptimizerTaskQueue
     else CurrentTaskQueue:=nil;

     if   Assigned(CurrentTaskQueue) then with CurrentTaskQueue do
          if Assigned(Plugin) then with StringGrid do begin
             Plugin.Enter;
             try     for ARow:=FixedRows to Pred(RowCount) do begin
                          Level:=Levels[ARow];
                          if (ARow<>HighlightedRowNumber) and
                             Selected[ARow] and Assigned(Level) and
                             (OptimizationFlags[ARow] <>0) then begin
                             OptimizationFlags[ARow]:=0;
                             RefreshRow( ARow );
                             end;
                          end;
                     OpenForm.EnableDisablePluginControls(Self,Plugin,False);
                     ToolsForm.ShowStatus;
             finally Plugin.Leave;
             end;
             end;
     end;
end;

procedure TToolsForm.PluginEditMenuItemSetOptimizationIntervalClick(
  Sender: TObject);
var ARow:Integer; Level:TLevel; CurrentTaskQueue:TTaskQueue;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then begin
     if   PageControl1.ActivePage=TabSheetOptimizer then
          CurrentTaskQueue:=OptimizerTaskQueue
     else CurrentTaskQueue:=nil;

     if   Assigned(CurrentTaskQueue) then with CurrentTaskQueue do
          if Assigned(Plugin) then with StringGrid do begin
             Plugin.Enter;
             try
                   if (not PluginLevelInfo.ReplayInfo.IsLoaded ) and // 'True': the solution hasn't been loaded for replay in the thumbnail viewer; do it now
                      EnterBrowseMode then
                      LeaveBrowseMode( True );

                 if  PluginLevelInfo.ReplayInfo.IsLoaded and
                     (PluginLevelInfo.ReplayInfo.SelectedRange[2]>0) then
                     for ARow:=FixedRows to Pred(RowCount) do begin
                         Level:=Levels[ARow];
                         if (ARow<>HighlightedRowNumber) and
                            Selected[ARow] and
                            Assigned(Level) and
                            (not Level.SnapshotsAsText.IsEmpty) and
                            (Level.SnapshotsAsText.Last is TExtendedSnapshotAsText) then
                            with TExtendedSnapshotAsText(Level.SnapshotsAsText.Last) do begin
                              SelectedRange[2]:=Max(1,Metrics.PushCount);
                              SelectedRange[0]:=Min(PluginLevelInfo.ReplayInfo.SelectedRange[0],SelectedRange[2]);
                              SelectedRange[1]:=Min(PluginLevelInfo.ReplayInfo.SelectedRange[1],SelectedRange[2]);
                              SelectedRange[6]:= 0; // clear repeat interval size
                              if   SelectedRange[0]         >= SelectedRange[1] then // 'True': clear selection
                                   FillChar( SelectedRange, SizeOf( SelectedRange), 0 );
                              if   SelectedRange[1]         >= SelectedRange[2] then begin // 'True': selected end of range >= end-of-game
                                   if   SelectedRange[0]    =  0 then // 'True': all moves selected; clear selection
                                        SelectedRange[1]    := 0
                                   else SelectedRange[1]    := Succ(SelectedRange[2]); // ensure that the last push is included in the range of pushes to be optimized by overshooting the range
                                   end;
                              if   SelectedRange[0]         <  SelectedRange[1] then
                                   if   PluginLevelInfo.ReplayInfo.SelectedRange[6]>0 then // 'True': repeat interval
                                        SelectedRange[6]    := SelectedRange[1]-SelectedRange[0]
                                   else SelectedRange[6]    := 0
                              else FillChar( SelectedRange, SizeOf( SelectedRange), 0 );
                              RefreshRow( ARow );
                              end;
                         end;
                     OpenForm.EnableDisablePluginControls(Self,Plugin,False);
                     ToolsForm.ShowStatus;
             finally Plugin.Leave;
             end;
             end;
     end;
end;

procedure TToolsForm.PluginEditMenuItemClearBoardAreaSelectedForOptimizationClick(
  Sender: TObject);
var ACol,Arow:Integer; CurrentTaskQueue:TTaskQueue;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then begin
     if   PageControl1.ActivePage=TabSheetOptimizer then
          CurrentTaskQueue:=OptimizerTaskQueue
     else CurrentTaskQueue:=nil;

     if   Assigned(CurrentTaskQueue) then with CurrentTaskQueue do
          if Assigned(Plugin) then with StringGrid do begin
             Plugin.Enter;
             try
                   if (not PluginLevelInfo.ReplayInfo.IsLoaded ) and // 'True': the solution hasn't been loaded for replay in the thumbnail viewer; do it now
                      EnterBrowseMode then
                      LeaveBrowseMode( True );

                 if  PluginLevelInfo.ReplayInfo.IsLoaded and
                     (PluginLevelInfo.SelectedSquaresAsText<>'') then with OpenForm.Game do begin
                     for ACol:=1 to BoardWidth do
                         for ARow:=1 to BoardHeight do
                             Board[ACol,ARow]:=Board[ACol,ARow] and (not SQUARE_SET);
                     ImageBoard.Tag:=1;
                     ControlMouseUp(ImageBoard,mbRight,[],0,0);
                     LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,LevelSetForm.GameViewer.BoardImage,False);
                     end;

             finally Plugin.Leave;
             end;
             end;
     end;
end;

procedure TToolsForm.SetSortMetric(Metric__:TGameMetricsExtended);
begin
  fSortMetric:=Metric__;

  PluginMenuItemSortOnMoves.Checked:=SortMetric=gmMoves;
  PluginMenuItemSortOnPushes.Checked:=SortMetric=gmPushes;
  PluginMenuItemSortOnBoxLines.Checked:=SortMetric=gmBoxLines;
  PluginMenuItemSortOnBoxChanges.Checked:=SortMetric=gmBoxChanges;
  PluginMenuItemSortOnPushingSessions.Checked:=SortMetric=gmPushingSessions;
  PluginMenuItemSortOnPlayerLines.Checked:=SortMetric=gmPlayerLines;

  PopupMenuItemSortOnMoves.Checked:=PluginMenuItemSortOnMoves.Checked;
  PopupMenuItemSortOnPushes.Checked:=PluginMenuItemSortOnPushes.Checked;
  PopupMenuItemSortOnBoxLines.Checked:=PluginMenuItemSortOnBoxLines.Checked;
  PopupMenuItemSortOnBoxChanges.Checked:=PluginMenuItemSortOnBoxChanges.Checked;
  PopupMenuItemSortOnPushingSessions.Checked:=PluginMenuItemSortOnPushingSessions.Checked;
  PopupMenuItemSortOnPlayerLines.Checked:=PluginMenuItemSortOnPlayerLines.Checked;
end;

procedure TToolsForm.MakeAllColumnsFullyVisible;
var ACol,X:Integer;
begin
  if      PageControl1.ActivePage=TabSheetOptimizer then with OptimizeSolutionsStringGrid do begin
          Misc_.MakeAllColumnsFullyVisible(OptimizeSolutionsStringGrid,PluginLevelStringGrid.ColWidths[0],-1);
          X:=0;
          for ACol:=0 to Pred(Ord(oscOptimization)) do Inc(X,ColWidths[ACol]+GridLineWidth);
          OptimizationComboBox.Left :=X;
          OptimizationComboBox.Width:=ColWidths[Ord(oscOptimization)];
          end
  else if PageControl1.ActivePage=TabSheetSolver then
          Misc_.MakeAllColumnsFullyVisible(SolveLevelsStringGrid,PluginLevelStringGrid.ColWidths[0],-1)
  else if PageControl1.ActivePage=TabSheetGenerator then
          Misc_.MakeAllColumnsFullyVisible(GenerateLevelsStringGrid,PluginLevelStringGrid.ColWidths[0],-1);
end;

procedure TToolsForm.SetDefaultStringGridColumnWidths;
begin
  with OptimizeSolutionsStringGrid do begin
    ColWidths[Ord(oscSelect)]                           :=Max(PluginLevelStringGrid.ColWidths[0],DefaultRowHeight);
    ColWidths[Ord(oscNo)]                               :=Self.Canvas.TextWidth(IntToStr(Max(99999,Max(RowCount,Max(SolveLevelsStringGrid.RowCount,GenerateLevelsStringGrid.RowCount)))))+8;
//  ColWidths[Ord(oscSnapshotName)]                     :=Max(Self.Canvas.TextWidth(OptimizeSolutionsColumnHeadersText[oscSnapshotName]),Self.Canvas.TextWidth('99999/99999/999/999/999'))+8;
    ColWidths[Ord(oscMetrics)]                          :=Max(Self.Canvas.TextWidth(OptimizeSolutionsColumnHeadersText[oscMetrics     ]),Self.Canvas.TextWidth('99999/99999/999/999/999'))+8;
    ColWidths[Ord(oscOptimization)]                     :=Self.Canvas.TextWidth(OptimizeSolutionsColumnHeadersText[oscOptimization])+8;
    ColWidths[Ord(oscSnapshotName)]                     :=Max(Self.Canvas.TextWidth(OptimizeSolutionsColumnHeadersText[oscSnapshotName])+8,ColWidths[Ord(oscMetrics)]);

    SolveLevelsStringGrid.ColWidths[Ord(slcSelect)]     :=ColWidths[Ord(oscSelect)];
    SolveLevelsStringGrid.ColWidths[Ord(slcNo    )]     :=ColWidths[Ord(oscNo    )];
    SolveLevelsStringGrid.ColWidths[Ord(slcMetrics)]    :=Max(Self.Canvas.TextWidth(SolveLevelsColumnHeadersText[slcMetrics]),Self.Canvas.TextWidth('9999/9999'))+8;

    GenerateLevelsStringGrid.ColWidths[Ord(glcSelect  )]:=ColWidths[Ord(oscSelect)];
    GenerateLevelsStringGrid.ColWidths[Ord(glcNo      )]:=ColWidths[Ord(oscNo    )];
    GenerateLevelsStringGrid.ColWidths[Ord(glcPushes  )]:=Max(Self.Canvas.TextWidth(GenerateLevelsColumnHeadersText[glcPushes  ]),Self.Canvas.TextWidth('9999'))+8;
    GenerateLevelsStringGrid.ColWidths[Ord(glcFitness )]:=Max(Self.Canvas.TextWidth(GenerateLevelsColumnHeadersText[glcFitness ]),Self.Canvas.TextWidth('999999999'))+8;
    GenerateLevelsStringGrid.ColWidths[Ord(glcBirth   )]:=Max(Self.Canvas.TextWidth(GenerateLevelsColumnHeadersText[glcBirth   ]),Self.Canvas.TextWidth('99:99:99'))+8;
    GenerateLevelsStringGrid.ColWidths[Ord(glcChildren)]:=Max(Self.Canvas.TextWidth(GenerateLevelsColumnHeadersText[glcChildren]),Self.Canvas.TextWidth('99999'))+8;
    GenerateLevelsStringGrid.ColWidths[Ord(glcTime    )]:=Max(Self.Canvas.TextWidth(GenerateLevelsColumnHeadersText[glcTime    ]),Self.Canvas.TextWidth('99:99:99'))+8;
    end;
end;

procedure TToolsForm.PluginMenuItemOpenPriorOrNextClick(Sender: TObject);
begin
  CloseEditorSelection(True,False);

  if (Screen.ActiveForm=Self) and
     OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors and
     (not Modified) and
     (not MainForm.Modified) and
     (not IsANewFileName(Editor.FileName)) and
     CloseLevel(Sender)
     then begin
     if        (Sender=PluginMenuItemOpenPrior) or (Sender=PluginToolButtonOpenPrior) then
               EditMenuItemOpenClick(PluginMenuItemOpenPrior)
     else      EditMenuItemOpenClick(PluginMenuItemOpenNext );

     if        PageControl1.ActivePage=TabSheetOptimizer then
               OptimizerTaskQueue.FocusLevelName(Editor.FileName,True)
     else if   PageControl1.ActivePage=TabSheetSolver then
               SolverTaskQueue.FocusLevelName(Editor.FileName,True)
          else ShowStatus;
     end;
end;

function  SortOnMetrics(a,b:Pointer):Integer;
var s,t:TExtendedSnapshotAsText;
begin
  s:=TExtendedSnapshotAsText(TLevel(a).SnapshotsAsText.Last);
  t:=TExtendedSnapshotAsText(TLevel(b).SnapshotsAsText.Last);
  if Assigned(s) then begin
     if Assigned(t) then with s.Metrics do begin // first sort on the selected metric
        case ToolsForm.SortMetric of
          gmMoves                     : Result:=MoveCount                       -t.Metrics.MoveCount;
          gmPushes                    : Result:=PushCount                       -t.Metrics.PushCount;
          gmBoxLines                  : Result:=SecondaryMetrics.BoxLines       -t.Metrics.SecondaryMetrics.BoxLines;
          gmBoxChanges                : Result:=SecondaryMetrics.BoxChanges     -t.Metrics.SecondaryMetrics.BoxChanges;
          gmPushingSessions           : Result:=SecondaryMetrics.PushingSessions-t.Metrics.SecondaryMetrics.PushingSessions;
          gmPlayerLines               : Result:=SecondaryMetrics.PlayerLines    -t.Metrics.SecondaryMetrics.PlayerLines;
          else {gmBoxLinesAndChanges}   Result:=SecondaryMetrics.BoxLines       -t.Metrics.SecondaryMetrics.BoxLines+SecondaryMetrics.BoxChanges-t.Metrics.SecondaryMetrics.BoxChanges;
        end; // case
        if Result=0 then begin // for tiebreaking, sort on the other metrics, in order
           if ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator then begin
              if GeneratorForm.RadioButtonFitnessBoxLinesAndBoxChanges.Checked then
                                        Result:=SecondaryMetrics.BoxLines       -t.Metrics.SecondaryMetrics.BoxLines+SecondaryMetrics.BoxChanges-t.Metrics.SecondaryMetrics.BoxChanges;
              if Result=0 then          Result:=PushCount                       -t.Metrics.PushCount;
              if Result=0 then          Result:=SecondaryMetrics.BoxLines       -t.Metrics.SecondaryMetrics.BoxLines+SecondaryMetrics.BoxChanges-t.Metrics.SecondaryMetrics.BoxChanges;
              end
           else
              if Result=0 then begin
                 Result:=MoveCount-t.Metrics.MoveCount;
                 if  Result=0 then begin
                     Result:=PushCount-t.Metrics.PushCount;
                     if Result=0 then begin
                        Result:=SecondaryMetrics.BoxLines-t.Metrics.SecondaryMetrics.BoxLines;
                        if Result=0 then begin
                           Result:=SecondaryMetrics.BoxChanges-t.Metrics.SecondaryMetrics.BoxChanges;
                           if Result=0 then begin
                              Result:=SecondaryMetrics.PushingSessions-t.Metrics.SecondaryMetrics.PushingSessions;
                              if Result=0 then begin
                                 Result:=SecondaryMetrics.PlayerLines-t.Metrics.SecondaryMetrics.PlayerLines;
                                 end;
                              end;
                           end;
                        end;
                     end;
                  end;
           end;
        Result:=Result*ToolsForm.AscendingOrDescendingSortingOrderSign;
        end
     else Result:=1;
     end
  else
     if   Assigned(t) then Result:=-1
     else Result:=0;
end;

function  SortOnSelected(a,b:Pointer):Integer;
begin
  Result:=(Ord(ltfSelected in TLevel(b).Tag.Flags)-Ord(ltfSelected in TLevel(a).Tag.Flags))*ToolsForm.AscendingOrDescendingSortingOrderSign; // relies on Ord(False,True) = 0,1
  // if Result=0 then Result:=SortOnMetrics(a,b);
end;

function  SortOnChronologicalOrder(a,b:Pointer):Integer;
var i,j:Integer; StringGrid:TStringGrid;
begin
  with ToolsForm do begin
    if   PageControl1.ActivePage=TabSheetOptimizer then
         StringGrid:=OptimizerTaskQueue.StringGrid
    else StringGrid:=SolverTaskQueue.StringGrid;
    with StringGrid do begin
      i:=TLevel(a).Flags; j:=TLevel(b).Flags;
      if (i>=StringGrid.FixedRows) and (i<RowCount) then begin
         if (j>=StringGrid.FixedRows) and (j<RowCount) then begin
            if         Cardinal(Objects[Ord(oscSnapshotName),i])<Cardinal(Objects[Ord(oscSnapshotName),j]) then
                       Result:=-1
            else if    Cardinal(Objects[Ord(oscSnapshotName),i])>Cardinal(Objects[Ord(oscSnapshotName),j]) then
                       Result:= 1
                 else Result:=  0;
            end
         else Result:=-1;
         end
      else Result:=1;
      //if Result=0 then Result:=SortOnMetrics(a,b);
      Result:=Result*AscendingOrDescendingSortingOrderSign;
      end;
    end;
end;

function  SortOnLevelNames(a,b:Pointer):Integer;
begin
  Result:=AnsiCompareText(TLevel(a).Text,TLevel(b).Text)*ToolsForm.AscendingOrDescendingSortingOrderSign;
  // if Result=0 then Result:=SortOnMetrics(a,b);
end;

function  SortOnSolutionNames(a,b:Pointer):Integer;
begin
  Result:=AnsiCompareText(TLevel(a).SnapshotsAsText.Last.Text,TLevel(b).SnapshotsAsText.Last.Text)*ToolsForm.AscendingOrDescendingSortingOrderSign;
  // if Result=0 then Result:=SortOnMetrics(a,b);
end;

function  SortOnOptimization(a,b:Pointer):Integer;
begin
  with ToolsForm.OptimizeSolutionsStringGrid do with TLevel(a) do
    if   (Flags>=FixedRows) and (Flags<RowCount) then
         if   (TLevel(b).Flags>=FixedRows) and (TLevel(b).Flags<RowCount) then
              Result:=AnsiCompareText(Cells[Ord(oscOptimization),Flags],Cells[Ord(oscOptimization),TLevel(b).Flags])*ToolsForm.AscendingOrDescendingSortingOrderSign
         else Result:=-1
    else Result:=1;
end;

procedure TToolsForm.MenuItemSortClick(Sender: TObject);
var ARow:Integer; b:Boolean; Level,CurrentLevel,HighlightedLevel:TLevel;
    SortFun:TCompareFunction; CurrentTaskQueue:TTaskQueue;

  function IsSortedInAscendingOrder(TaskQueue__:TTaskQueue; SortFun__:TCompareFunction):Boolean;
  var Level1,Level2:TLevel;
  begin
    with TaskQueue__ do with Plugin do begin
      AscendingOrDescendingSortingOrderSign:=1;
      Result:=True;
      Level1:=nil;
      Level2:=TLevel(Plugin.SokoFile.Levels.First);
      while Result and Assigned(Level2) do with Level2 do begin
        if Flags>=StringGrid.FixedRows then begin                               // 'True': the level is visible in the grid
           if Level1<>nil then                                                  // '<>': this is not the first visible level
              Result:=SortFun__(Level1,Level2)<=0;
           Level1:=Level2;
           end;
        Level2:=TLevel(Next);
        end;
      end;
  end;

begin // MenuItemSortClick
  if         (Sender=PluginMenuItemSortOnSelected          ) or (Sender=PopupMenuItemSortOnSelected          ) then SortFun   :=SortOnSelected
  else if    (Sender=PluginMenuItemSortOnChronologicalOrder) or (Sender=PopupMenuItemSortOnChronologicalOrder) then SortFun   :=SortOnChronologicalOrder
  else if    (Sender=PluginMenuItemSortOnLevelNames        ) or (Sender=PopupMenuItemSortOnLevelNames        ) then SortFun   :=SortOnLevelNames
  else if    (Sender=PluginMenuItemSortOnSolutionNames     ) or (Sender=PopupMenuItemSortOnSolutionNames     ) then SortFun   :=SortOnSolutionNames
  else if    (Sender=PluginMenuItemSortOnOptimization      ) or (Sender=PopupMenuItemSortOnOptimization      ) then SortFun   :=SortOnOptimization
  else begin
     if      (Sender=PluginMenuItemSortOnMoves             ) or (Sender=PopupMenuItemSortOnMoves             ) then SortMetric:=gmMoves
     else if (Sender=PluginMenuItemSortOnPushes            ) or (Sender=PopupMenuItemSortOnPushes            ) then SortMetric:=gmPushes
     else if (Sender=PluginMenuItemSortOnBoxLines          ) or (Sender=PopupMenuItemSortOnBoxLines          ) then SortMetric:=gmBoxLines
     else if (Sender=PluginMenuItemSortOnBoxChanges        ) or (Sender=PopupMenuItemSortOnBoxChanges        ) then SortMetric:=gmBoxChanges
     else if (Sender=PluginMenuItemSortOnPushingSessions   ) or (Sender=PopupMenuItemSortOnPushingSessions   ) then SortMetric:=gmPushingSessions
     else if (Sender=PluginMenuItemSortOnPlayerLines       ) or (Sender=PopupMenuItemSortOnPlayerLines       ) then SortMetric:=gmPlayerLines
     else if (Sender=GeneratorForm.RadioButtonFitnessPushes) or
             (Sender=GeneratorForm.RadioButtonFitnessBoxLinesAndBoxChanges)                                    then SortMetric:=gmPlayerLines; // the fitness is stored in the 'PlayerLines' field
     SortFun:=SortOnMetrics;
     end;

  if         PageControl1.ActivePage=TabSheetOptimizer then CurrentTaskQueue:=OptimizerTaskQueue
  else if    PageControl1.ActivePage=TabSheetSolver    then CurrentTaskQueue:=SolverTaskQueue
  else if    PageControl1.ActivePage=TabSheetGenerator then CurrentTaskQueue:=GeneratorTaskQueue
  else if    PageControl1.ActivePage=TabSheetEditor    then CurrentTaskQueue:=GeneratorTaskQueue
  else raise Exception.Create(InternalErrorText+': Tools_.TToolsForm.MenuItemSortClick');

  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors then with CurrentTaskQueue do begin
     Plugin.Enter;

     if      (CurrentTaskQueue=GeneratorTaskQueue)
             or
             IsSortedInAscendingOrder(CurrentTaskQueue,SortFun) then            // 'True': already sorted in ascending order: sort in descending order
             AscendingOrDescendingSortingOrderSign:=-1
     else    AscendingOrDescendingSortingOrderSign:= 1;                         // sort in ascending order

     try     CurrentLevel:=Levels[StringGrid.Row];
             HighlightedLevel:=Levels[HighlightedRowNumber];
             HighlightedRowNumber:=-1;

             Plugin.SokoFile.Levels.MergeSort(SortFun);

//        if Plugin.SokoFile.Levels.SaveToVector(Count,Vector) then begin
//           try     QuickSort(Vector,Count,SizeOf(Vector^[Low(Vector^)]),SortFun);
//           finally Plugin.SokoFile.Levels.LoadFromVector(Count,Vector);
//           end;

             Level:=TLevel(Plugin.SokoFile.Levels.First);
             while Assigned(Level) do with Level do begin
               if (Flags>=StringGrid.FixedRows) and
                  (Flags<StringGrid.RowCount) and
                  (Cardinal(StringGrid.Objects[Ord(oscSnapshotName),Flags])>=Cardinal(StringGrid.FixedRows)) then
                  // temporary save the chronological run number in the 'Flags'
                  // field; there isn't really any other place to store it
                  // without putting in some more work, such as adding a real
                  // timestamp field to the 'TLevel' datastructure;
                  // saving the timestamp in 'Flags' is dirty and tricky because
                  // 'Flags' then momentarily serves 2 purposes: whether the
                  // level is visible in the grid, and for storage of the
                  // timestamp; it happens to work correctly because
                  // 'StringGrid.FixedRows' = 1 = first timestamp
                  Flags:=Cardinal(StringGrid.Objects[Ord(oscSnapshotName),Flags]);
               Level:=TLevel(Next);
               end;

             ARow:=StringGrid.FixedRows;
             Level:=TLevel(Plugin.SokoFile.Levels.First);
             while Assigned(Level) do with Level do begin
               if Flags>=StringGrid.FixedRows then
                  if   ARow<StringGrid.RowCount then begin
                       StringGrid.Objects[Ord(oscSnapshotName),ARow]:=TObject(Flags); // the chronological sequence number was temporarily saved in 'Flags'; see the note above
                       Flags:=ARow;
                       Levels[ARow]:=Level;
                       RefreshRow(ARow);
                       Inc(ARow);
                       end
                  else Flags:=-1;
               Level:=TLevel(Next);
               end;
             while ARow<StringGrid.RowCount do begin // '<': this shouldn't happen, but better safe than sorry
               Level:=Levels[ARow];
               if Assigned(Level) then Level.Flags:=-1;
               Levels[ARow]:=nil; // clean any remaining rows in the string-grid
               Inc(ARow);
               end;

             if Assigned(HighlightedLevel) then
                HighlightedRowNumber:=HighlightedLevel.Flags;

             Refresh(True);

             if   Assigned(CurrentLevel) and (CurrentLevel=Levels[CurrentLevel.Flags]) then begin
                  StringGrid.Row:=CurrentLevel.Flags;
                  ScrollInView(StringGrid.Row);
                  if      Plugin is TOptimizerPlugin then
                          OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,b) // ends with a call to 'ShowStatus', hence, there is no reason to do it again here
                  else if Plugin is TSolverPlugin then
                          SolveLevelsStringGridSelectCell(nil,0,StringGrid.Row,b) // ends with a call to 'ShowStatus', hence, there is not reason to do it again here
                  else if (Plugin is TGenerator) and (PageControl1.ActivePage=TabSheetGenerator) then
                          GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,b); // ends with a call to 'ShowStatus', hence, there is not reason to do it again here
                  end
             else ShowStatus;
//           end;
     finally Plugin.Leave;
     end;
     end;
end;

procedure TToolsForm.PluginMenuItemAddClick(Sender: TObject);
var   LevelCount,NewRowNo:Integer; AddedSolutions,AllLevels,AllSolutions,b:Boolean;
      PackFileName,s:String;
      FirstNewLevel,Level,NewLevel,RootLevel,L:TLevel;
      Snapshot : TSnapshot;
      oCursor:TCursor;
      CurrentPlugin:TPlugin; CurrentTaskQueue:TTaskQueue;

  function  ImportOptimizerTaskQueueInformation(
              LevelOnTaskQueue__ : TLevel;
              OriginalLevel__    : TLevel;
              OriginalSnapshot__ : TSnapshot ) : Boolean;
  var i : Integer; s : String;
  begin
    Result := ( Sender = PluginMenuItemImportTaskQueue ) and
              Assigned( LevelOnTaskqueue__ ) and
              Assigned( OriginalLevel__ ) and
              Assigned( OriginalSnapshot__ ) and
              ( OptimizerTaskQueue.Levels[ LevelOnTaskQueue__.Flags ] = LevelOnTaskQueue__ ) and
              ( not LevelOnTaskQueue__.SnapshotsAsText.IsEmpty ) and
              ( LevelOnTaskQueue__.SnapshotsAsText.Last is TExtendedSnapshotAsText );
    if Result then begin
       if OriginalLevel__.Notes.Lines.ReadString( KEY_TITLE, s ) then
          LevelOnTaskQueue__.SetName( ExpandedFilePath( s, MainForm.MyDocumentsFolder ) );
       with TExtendedSnapshotAsText( LevelOnTaskQueue__.SnapshotsAsText.Last ) do SetName( SnapshotsForm.DethronedSolutionName( Name ) ); // avoid that the imported task is called a best solution; there may be several tasks related to a given level;
       if OriginalSnapshot__.Notes.Lines.ReadString( KEY_SELECTED, s ) and // get 'Selected' from the original snapshot notes
          StrEqual( s, TEXT_NO ) then
          OptimizerTaskQueue.Selected[ LevelOnTaskQueue__.Flags ] := False;
       if OriginalSnapshot__.Notes.Lines.ReadString( KEY_SETTINGS, s ) then // get 'Settings', i.e., the optimization type, from the original snapshot notes
          OptimizerTaskQueue.OptimizationFlags[ LevelOnTaskQueue__.Flags ] := TextToOptimizationFlags( s );
       if OriginalSnapshot__.Notes.Lines.ReadString( KEY_BEGIN   , s ) and SafeStrToInt( s, False, i ) then // get 'Begin',    i.e., the start of the range of moves to be optimized
          with TExtendedSnapshotAsText( LevelOnTaskQueue__.SnapshotsAsText.Last ) do SelectedRange[ 0 ] := Min( Max( i, 0                  ),       Metrics.PushCount   );
       if OriginalSnapshot__.Notes.Lines.ReadString( KEY_END     , s ) and SafeStrToInt( s, False, i ) then // get 'Begin',    i.e., the start of the range of moves to be optimized
          with TExtendedSnapshotAsText( LevelOnTaskQueue__.SnapshotsAsText.Last ) do SelectedRange[ 1 ] := Min( Max( i, SelectedRange[ 0 ] ), Succ( Metrics.PushCount ) );
       if OriginalSnapshot__.Notes.Lines.ReadString( KEY_INTERVAL, s ) and SafeStrToInt( s, False, i ) then // get 'Interval', i.e., the interval size for the optimizer feature 'partition solution into subintervals'
          with TExtendedSnapshotAsText( LevelOnTaskQueue__.SnapshotsAsText.Last ) do SelectedRange[ 6 ] := Min( Max( i, 0                  ),       Metrics.PushCount   );

       with TExtendedSnapshotAsText( LevelOnTaskQueue__.SnapshotsAsText.Last ) do
          if   ( SelectedRange[ 0 ]  <  SelectedRange[ 1 ] ) then begin // 'True': a range of moves has been selected for optimization
               SelectedRange[ 2 ]    := Metrics.PushCount;              // set the maximum range, which is the number of pushes
               if SelectedRange[ 6 ] >= SelectedRange[ 2 ] then         // 'True': the interval size for "partition solution into subintervals" is out of bounds; reset it;
                  SelectedRange[ 6 ] := 0;
               end
          else FillChar( SelectedRange, SizeOf( SelectedRange ), 0 );
       OptimizerTaskQueue.RefreshRow( LevelOnTaskQueue__.Flags );
       end;
  end;

begin //
  CurrentPlugin:=PluginForCurrentTabSheet;
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors and
     (CurrentPlugin<>nil) and
     Assigned(CurrentPlugin.PluginThread) then begin
     if      CurrentPlugin is TOptimizerPlugin then
             CurrentTaskQueue:=OptimizerTaskQueue
     else if CurrentPlugin is TSolverPlugin then
             CurrentTaskQueue:=SolverTaskQueue
     else if CurrentPlugin is TGenerator then
             CurrentTaskQueue:=GeneratorTaskQueue
     else    CurrentTaskQueue:=nil;
     if Assigned( CurrentTaskQueue ) then with CurrentTaskQueue do with StringGrid do begin
       AddedSolutions:=False; FirstNewLevel:=nil;
       AllLevels := ( not ( CurrentPlugin is TSolverPlugin    ) ) or  ( not  IsKeyPressed( VK_CONTROL ) ) or ( Sender = PluginMenuItemImportTaskQueue ); // if [Ctrl] is pressed then only add unsolved levels to the solver's list
       AllSolutions :=    ( CurrentPlugin is TOptimizerPlugin )   and      ( IsKeyPressed( VK_CONTROL   ) or ( Sender = PluginMenuItemImportTaskQueue ) );
       if PanelToolTips.Visible then begin
          PanelToolTips.Hide;
          Self.Update;
          end;
       oCursor:=Screen.Cursor;
       try
         Screen.Cursor:=crHourGlass;
         if      CurrentPlugin is TOptimizerPlugin then OptimizeSolutionsGroupBox.Hide
         else if CurrentPlugin is TGenerator       then GenerateLevelsGroupBox.Hide;
         try
           CurrentPlugin.Enter;
           try
                   if Sender <> PluginMenuItemImportTaskQueue then begin
                      if   IsAnIniFileSectionFileName(Editor.FileName) then
                           PackFileName:=ExtractIniFileName(Editor.FileName)
                      else PackFileName:=Editor.FileName;

                      if   (PackFileName<>'') and
                           (not StrEqual(PackFileName,MainForm.SokoFile.Name)) and
                           FileExists(PackFileName) then
                           MainForm.SokoFile.LoadFromFile(PackFileName);
                      end;

                   //Level:=TLevel(MainForm.SokoFile.Levels.GetItemByName(MainForm.Game.Name));
                   Level:=TLevel(MainForm.SokoFile.Levels.First);
                   RootLevel:=Level; LevelCount:=0;

                   OpenForm.Game.SaveBestSolutionsAutomatically:=False;
                   OpenForm.Game.SaveSnapshotsAutomatically:=False;
                   OpenForm.Game.IsReplaying:=True;
                   OpenForm.Game.UserBreakWhileReplaying:=False;
                   Game.UserBreakWhileReplaying:=False;

                   while Assigned(Level)
                         and
                         (
                          ((CurrentPlugin is TSolverPlugin)
                           and
                           (CurrentPlugin.SelectedLevelsCount<MAX_ITEMS_ON_PLUGIN_TASK_QUEUE)
                           and
                           (PageControl1.ActivePage=TabSheetSolver)
                          )
                          or
                          ((CurrentPlugin is TOptimizerPlugin)
                           and
                           (RowCount-FixedRows<MAX_ITEMS_ON_PLUGIN_TASK_QUEUE)
                           and
                           (PageControl1.ActivePage=TabSheetOptimizer)
                          )
                         )
                         do begin
                     Inc(LevelCount);
                     StatusBar1.Panels[0].Text:=IntToStr(LevelCount);
                     StatusText:=Level.Name;
                     StatusBar1.Repaint;
                     //SleepEx(50,False);
                     if (LevelCount mod 100)=0 then Application.ProcessMessages; // so the user can click the window in order to stop the search

                     if CurrentPlugin is TSolverPlugin then with OpenForm.Game do begin // add items to the solver task queue
                        if LoadFromFileOrClipboard(MakeIniFileSectionFileName(MainForm.SokoFile.Name,Level.Name),nil,nil,b)
                           and
                           (GameState<>gsNull)
                           and
                           (CurrentPlugin.SelectedLevelsCount<MAX_ITEMS_ON_PLUGIN_TASK_QUEUE)
                           and
                           (GetStartBoardAsText<>'') then begin // ensure that 'StartBoardAsText' has been calculated
                           // add the level, unless it already is on the queue
                           NewLevel:=nil;
                           if      CurrentPlugin.Lookup(BoardWidth,BoardHeight,StartBoardAsText,'',NewLevel)
                                   and
                                   (NewLevel=SolverTaskQueue.Levels[NewLevel.Flags]) then begin
                                   // the level is already on the queue
                                   end
                           else if (AllLevels
                                    or
                                    ((not Assigned(BestSolutionMoves))
                                     and
                                     (not (Assigned(NewLevel)
                                           and
                                           (not NewLevel.SnapshotsAsText.IsEmpty)
                                           {
                                           and
                                           (TExtendedSnapshotAsText(NewLevel.SnapshotsAsText.First).IsASolution) // 'don't check 'IsASolution'; maybe it hasn't been calculated, i.e., 'False' may mean 'undefined'
                                           }
                                           // the task queue contains an unlisted
                                           // solution for this level, hence,
                                           // don't add it again

                                           // note that it's not strictly correct to
                                           // omit testing 'IsASolution'; a solver
                                           // could return a non-solution, and if
                                           // that happens while the 'Tools' window
                                           // isn't visible, the host doesn't check
                                           // it immidiately; maybe the non-solution
                                           // is filtered out upon entry to
                                           // the 'Tools' form, but it's not
                                           // 100% bullet-proof
                                          )
                                     )
                                     and
                                     (not (Assigned(OptimizerTaskQueue.Plugin)
                                           and
                                           OptimizerTaskQueue.Plugin.Lookup(BoardWidth,BoardHeight,StartBoardAsText,'',L)
                                          )
                                     )
                                    )
                                   )
                                   and
                                   CurrentPlugin.LoadLevel(BoardWidth,BoardHeight,-1,0,FileName,StartBoardAsText,nil,True,False,NewLevel) then begin
                                   if FirstNewLevel=nil then FirstNewLevel:=NewLevel;
                                   if   Sender = PluginMenuItemImportTaskQueue then begin
                                        if Level.Notes.Lines.ReadString( KEY_TITLE, s ) then
                                           NewLevel.SetName( ExpandedFilePath( s, MainForm.MyDocumentsFolder ) );
                                        if Level.Notes.Lines.ReadString( KEY_SELECTED, s ) and
                                           StrEqual( s, TEXT_NO ) then
                                           NewLevel.Flags := Low( NewLevel.Flags ); // 'Low': not selected; the checkbox is set accordingly further down when the new level has been assigned to a row in the grid;
                                        end;
                                   end;
                           end;
                        end
                     else
                        if CurrentPlugin is TOptimizerPlugin then with OpenForm.Game do begin // add items to the optimizer task queue
                           if (not Level.SnapshotsAsText.IsEmpty) and
                              LoadFromFileOrClipboard(MakeIniFileSectionFileName(MainForm.SokoFile.Name,Level.Name),nil,nil,b)
                              and
                              (GameState<>gsNull)
                              and
                              (RowCount-FixedRows<MAX_ITEMS_ON_PLUGIN_TASK_QUEUE)
                              and
                              (GetStartBoardAsText<>'') then begin // ensure that 'StartBoardAsText' has been calculated
                              // add best solutions, if any,  for the current game; only add solutions that aren't already on the queue
                              AddedSolutions:=False;
                              NewLevel := nil;
                              if MainForm.ShowSolutionMoves and
                                 Assigned(BestSolutionMoves) then
                                 if   Assigned(BestSolutionPushes) and CurrentPlugin.SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly then
                                      AddedSolutions:=Add(BoardWidth,BoardHeight,[ltfBest],BestMatchOptimization(SOKOBAN_PLUGIN_FLAG_MOVES)   ,FileName,StartBoardAsText,BestSolutionMoves ,True ,NewLevel)
                                 else AddedSolutions:=Add(BoardWidth,BoardHeight,[ltfBest],SOKOBAN_PLUGIN_FLAG_NONE                           ,FileName,StartBoardAsText,BestSolutionMoves ,True ,NewLevel);
                              ImportOptimizerTaskQueueInformation( NewLevel, Level, BestSolutionMoves );
                              NewLevel := nil;
                              if Assigned(BestSolutionPushes) then
                                 if   Assigned(BestSolutionMoves) and CurrentPlugin.SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly then
                                      if   Add(BoardWidth,BoardHeight,[ltfBest],BestMatchOptimization(SOKOBAN_PLUGIN_FLAG_PUSHES)             ,FileName,StartBoardAsText,BestSolutionPushes,True ,NewLevel) then
                                           AddedSolutions:=True
                                      else
                                 else if   Add(BoardWidth,BoardHeight,[ltfBest],SOKOBAN_PLUGIN_FLAG_NONE                                      ,FileName,StartBoardAsText,BestSolutionPushes,True ,NewLevel) then
                                           AddedSolutions:=True;
                              ImportOptimizerTaskQueueInformation( NewLevel, Level, BestSolutionPushes );
                              NewLevel := nil;
                              if (not MainForm.ShowSolutionMoves) and
                                 Assigned(BestSolutionMoves) then
                                 if   Assigned(BestSolutionPushes) and CurrentPlugin.SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly then
                                      if   Add(BoardWidth,BoardHeight,[ltfBest],BestMatchOptimization(SOKOBAN_PLUGIN_FLAG_MOVES)              ,FileName,StartBoardAsText,BestSolutionMoves ,True ,NewLevel) then
                                           AddedSolutions:=True
                                      else
                                 else if   Add(BoardWidth,BoardHeight,[ltfBest],SOKOBAN_PLUGIN_FLAG_NONE                                      ,FileName,StartBoardAsText,BestSolutionMoves ,True ,NewLevel) then
                                           AddedSolutions:=True;
                              ImportOptimizerTaskQueueInformation( NewLevel, Level, BestSolutionMoves );
                              if AllSolutions then begin
                                 Snapshot := TSnapshot( Snapshots.First );
                                 while Assigned( Snapshot ) do begin
                                   NewLevel := nil;
                                   if      ( Snapshot.Gamestate = gsSolved  ) and
                                           ( Snapshot <> BestSolutionMoves  ) and
                                           ( Snapshot <> BestSolutionPushes ) and
                                           Add(BoardWidth,BoardHeight,[ltfBest],SOKOBAN_PLUGIN_FLAG_NONE                                      ,FileName,StartBoardAsText,Snapshot          ,True ,NewLevel) then
                                           AddedSolutions:=True;
                                   ImportOptimizerTaskQueueInformation( NewLevel, Level, Snapshot );
                                   Snapshot := TSnapshot( Snapshot.Next );
                                   end;
                                 end;
                              end;
                           end;

                     Level:=TLevel(Level.Next);
                     if not Assigned(Level) then Level:=TLevel(MainForm.SokoFile.Levels.First); // 'True': wrap-around
                     if Level=RootLevel then Level:=nil; // 'True': all levels have been visited
                     if Game.UserBreakWhileReplaying or
                        OpenForm.Game.UserBreakWhileReplaying then
                        Level:=nil; // the user clicked inside the window; take it as an indication to stop the search
                     end;

                   if Assigned(FirstNewLevel) then begin // 'True': new levels have been added to the solver task queue
                      Level:=FirstNewLevel; FirstNewLevel:=nil; LevelCount:=0;
                      while Level<>nil do begin
                        if Level.Flags<StringGrid.FixedRows then begin
                           Inc(LevelCount);
                           if FirstNewLevel=nil then FirstNewLevel:=Level;
                           end;
                        Level:=TLevel(Level.Next);
                        end;
                      if (LevelCount=1) and
                         (not SolveLevelsGroupBox.Visible) and
                         ( Sender <> PluginMenuItemImportTaskQueue ) and
                         (FirstNewLevel.BoardAsTextLines.First.Text=MainForm.Game.GetStartBoardAsText)
                         then begin // don't show the task queue if it only contains the currently loaded level
                         LevelCount:=0;
                         Plugin.DeleteLevel(FirstNewLevel);
                         end;
                      if LevelCount<>0 then
                         try    with StringGrid do begin
                                  if Assigned(Levels[FixedRows]) then begin
                                     NewRowNo:=RowCount;
                                     RowCount:=RowCount+LevelCount;
                                     end
                                  else begin
                                     NewRowNo:=FixedRows;
                                     RowCount:=FixedRows+LevelCount;
                                     end;
                                  Level:=FirstNewLevel;
                                  while Assigned(Level) and (NewRowNo<RowCount) do begin
                                    if Level.Flags<StringGrid.FixedRows then begin
                                       Cells[Ord(slcNo),NewRowNo]       :=IntToStr(Succ(NewRowNo-FixedRows))+SPACE;
                                       if   IsAnIniFileSectionFileName(Level.Name) then s:=ExtractSectionName(Level.Name)
                                       else s:=ExtractFileNameWithoutPathAndExtension(Level.Name);
                                       Cells[Ord(slcLevelName),NewRowNo]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+s;
                                       Cells[Ord(slcMetrics),NewRowNo]:='';
                                       Levels[NewRowNo]:=Level;
                                       SolverTaskQueue.Selected[NewRowNo]:=Level.Flags<>Low(Level.Flags); // 'Low': importing from a task queue, and the level was marked as unselected
                                       Level.Flags:=NewRowNo;
                                       Inc(NewRowNo);
                                       end;
                                    Level:=TLevel(Level.Next);
                                    end;
                                  SolverTaskQueue.Refresh(True);
                                  SolveLevelsStringGridSelectCell(nil,0,StringGrid.Row,b);
                                  if Editor.FileName<>'' then SolverTaskQueue.FocusLevelName(Editor.FileName,True);
                                  end;
                         except on E:Exception do begin
                                   Error(E.Message,Application.Title);
                                   CurrentPlugin.ClearQueue(True);
                                   end;
                         end;
                      end;

           finally CurrentPlugin.Leave;
           end;
         finally if      (CurrentPlugin is TOptimizerPlugin) and
                         (not OptimizeSolutionsGroupBox.Visible) then
                         OptimizeSolutionsGroupBox.Show
                 else if (CurrentPlugin is TGenerator) and
                         (not GenerateLevelsGroupBox.Visible) then
                         GenerateLevelsGroupBox.Show;
         end;
       finally Screen.Cursor:=oCursor;
               OpenForm.Game.IsReplaying:=False;
               MakeAllColumnsFullyVisible;
               OpenForm.EnableDisablePluginControls(ToolsForm,CurrentPlugin,False);
               StatusBar1.Panels[0].Text:='';
               StatusText:='';
               if        CurrentPlugin is TOptimizerPlugin then begin
                         if AddedSolutions then StringGrid.Update;
                         OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,b);
                         end
               else if   CurrentPlugin is TSolverPlugin then begin
                         SolveLevelsStringGridSelectCell(nil,0,StringGrid.Row,b);
                         end
               else if   CurrentPlugin is TGenerator then begin
                         GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,b);
                         end
                    else ShowStatus;
       end;
       end;
     end;
end;

procedure TToolsForm.PluginGroupBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FormMouseDown(Sender,Button,Shift,X,Y);
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and (not Game.IsBusy) and CloseEditors and
     (Button=mbLeft) and
     ((Sender is TGroupBox) or (Sender is TTabSheet)) then begin
     if   (Screen.Cursor=crHSplit) or (Screen.Cursor=crVSplit) then
          Editor.MouseButtonDown:=True;
     if   Sender is TTabSheet then
          with Sender as TTabSheet do Editor.DragPoint:=ClientToScreen(Point(X,Y))
     else with Sender as TGroupBox do Editor.DragPoint:=ClientToScreen(Point(X,Y));
     end
end;

function  TToolsForm.ResizePluginGroupBoxes(PluginLevelGroupBoxWidth__,PluginGroupBoxWidth__,LevelGroupBoxWidth__,PluginGroupBoxesHeight__:Integer; ShowBoard__:Boolean):Boolean;
var A,B,C,D:Integer; o:Boolean;
begin // precondition: the sum of new widths for the 3 groupboxes must match the old sum
  Result:=True;
  A:=PluginLevelGroupBoxWidth__-PluginLevelGroupBox.Width;
  B:=PluginGroupBoxWidth__     -SolverGroupBox     .Width;
  C:=LevelGroupBoxWidth__      -LevelGroupBox      .Width;
  D:=PluginGroupBoxesHeight__  -LevelGroupBox      .Height;

  if Result then Result:=PluginLevelGroupBoxWidth__   >=PluginLevelGroupBoxMinimumWidth;
  if Result then Result:=PluginGroupBoxWidth__        >=PluginGroupBoxMinimumWidth;
  if Result then Result:=LevelGroupBoxWidth__         >=LevelGroupBoxMinimumWidth;
  if Result then Result:=(PluginGroupBoxesHeight__    >=PluginGroupBoxesMinimumHeight)
                         and
                         ((SolveLevelsGroupBox.Height-D>=SolveLevelsGroupBoxMinimumHeight) or
                          (not Visible));
                          // 'not Visible': this is initialization;
                          // the panels and group boxes may not yet have been
                          // properly initialized; accept the height even if it
                          // doesn't match the window size;

  if Result then begin
     if A<>0 then with PluginLevelGroupBox do begin
        Width:=PluginLevelGroupBoxWidth__;
        with PluginLevelFileNamePanel do Width:=PluginLevelGroupBox.ClientWidth-2*Left;
        with BtnSolveLevel do Left:=PluginLevelGroupBox.Width-PluginLevelFileNamePanel.Left-Width;
        BtnOptimizeGames.Left:=BtnSolveLevel.Left;
        BtnGenerateLevels.Left:=BtnSolveLevel.Left;
        PanelPluginLevelInfo.Width:=PluginLevelFileNamePanel.Width;
        with PluginLevelStringGrid do begin
          LeftCol:=FixedCols;
          ColWidths[1]:=Min(ColWidths[1]+A,ClientWidth-ColWidths[0]);
          while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]); // for safety; this shouldn't happen
          end;
        with PluginLevelGroupBox do SolverGroupBox.Left:=2*Left+Width;
        OptimizerGroupBox.Left:=SolverGroupBox.Left;
        GeneratorGroupBox.Left:=SolverGroupBox.Left;
        if B=0 then LevelGroupBox.Left:=SolverGroupBox.Left+SolverGroupBox.Width+PluginLevelGroupBox.Left;
        end;

     if B<>0 then with SolverGroupBox do begin
        Width:=PluginGroupBoxWidth__;
        with SolverComboBox do Width:=SolverGroupBox.ClientWidth-2*Left;
        with BtnSolverBrowse do Left:=SolverGroupBox.Width-SolverComboBox.Left-Width;
        with CurrentSolverGroupBox do Width:=SolverComboBox.Width;
        with BtnSolverSettings do Left:=(CurrentSolverGroupBox.ClientWidth-Width) div 2;
        BtnSolverAbout.Left:=BtnSolverSettings.Left;
        LevelGroupBox.Left:=Left+Width+PluginLevelGroupBox.Left;
        BtnGeneralSolverSettings.Width:=CurrentSolverGroupBox.Width;

        OptimizerGroupBox.Width:=Width;
        OptimizerComboBox.Width:=SolverComboBox.Width;
        BtnOptimizerBrowse.Left:=BtnSolverBrowse.Left;
        CurrentOptimizerGroupBox.Width:=CurrentSolverGroupBox.Width;
        BtnOptimizerSettings.Left:=BtnSolverSettings.Left;
        BtnOptimizerAbout.Left:=BtnSolverAbout.Left;
        with BtnGeneralSolverSettings do BtnGeneralOptimizerSettings.SetBounds(Left,Top,Width,Height);

        GeneratorGroupBox.Width:=Width;
        with GeneratorNamePanel do Width:=GeneratorGroupBox.ClientWidth-2*Left;
        //with BtnGeneralSolverSettings do BtnGeneratorSettings.SetBounds(Left,{Top}BtnSolverBrowse.Top,Width,Height);
        with BtnSolverBrowse do BtnGeneratorSettings.SetBounds(Left,Top,Width,Height);
        PanelGeneratorStatus.Width:=GeneratorNamePanel.Width;
        with GeneratorStatusStringGrid do begin
          LeftCol:=FixedCols;
          ColWidths[1]:=ClientWidth-Pred(ColCount)*GridLineWidth-ColWidths[0];
          while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]); // for safety; this shouldn't happen
          end;
        end;

     if (C<>0) or (D<>0) then with LevelGroupBox do begin
        Width:=LevelGroupBoxWidth__;
        with LevelNamePanel do Width:=LevelGroupBox.ClientWidth-2*Left;
        PanelBoard.Width:=LevelNamePanel.Width;
        if D<>0 then begin
           Height:=PluginGroupBoxesHeight__;
           with PanelBoard do Height:=LevelGroupBox.ClientHeight-Top-Left;
           end;
        with ImageBoard do begin Width:=Parent.ClientWidth; Height:=Parent.ClientHeight; end;
        ResizeImage(ImageBoard);
        with ImageReplaySpeed do begin Width:=Parent.ClientWidth; Height:=Parent.ClientHeight; end;
        ResizeImage(ImageReplaySpeed); ShowReplaySpeed;
         if ShowBoard__ and Assigned(LevelSetForm) then with LevelSetForm.GameViewer do
            if BoardImage=Self.ImageBoard then begin
               BackgroundInitialized:=False;
               SkinInitialized:=False;
               if PageControl1.ActivePage=TabsheetSolver then begin
                  if Game.IsReplaying or PluginToolButtonStopReplay.Enabled then with OpenForm.Game do
                     LevelSetForm.ShowBoard0(BoardWidth,BoardHeight,Board,BoardImage,False)
                  else if Assigned(SolverTaskQueue)  then with SolverTaskQueue do with StringGrid do begin
                          ScrollInView(StringGrid.Row);
                          if   (StringGrid.Row>Succ(FixedRows)) or Assigned(Levels[StringGrid.Row]) then begin
                               SolveLevelsStringGridSelectCell(nil,0,StringGrid.Row,o);
                               end
                          else begin LoadLevelFromEditor(False,True);
                                     ShowStatus;
                               end;
                          end;
                  end;
               if PageControl1.ActivePage=TabsheetOptimizer then
                  if   Assigned(OptimizerTaskQueue)  then with OptimizerTaskQueue do with StringGrid do begin
                       ScrollInView(StringGrid.Row);
                       if   (StringGrid.Row>Succ(FixedRows)) or Assigned(Levels[StringGrid.Row]) then
                            OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,o)
                       else OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,o);
                       end;
               if PageControl1.ActivePage=TabsheetGenerator then
                  if   Assigned(GeneratorTaskQueue)  then with GeneratorTaskQueue do with StringGrid do begin
                       ScrollInView(StringGrid.Row);
                       if   (StringGrid.Row>Succ(FixedRows)) or Assigned(Levels[StringGrid.Row]) then
                            GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,o)
                       else GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,o);
                       end;
               end;
        end;

     if D<>0 then with LevelGroupBox do begin
        PluginLevelGroupBox .Height:=PluginGroupBoxesHeight__;
        SolverGroupBox      .Height:=PluginGroupBoxesHeight__;
        LevelGroupBox       .Height:=PluginGroupBoxesHeight__;
        OptimizerGroupBox   .Height:=PluginGroupBoxesHeight__;
        GeneratorGroupBox   .Height:=PluginGroupBoxesHeight__;

        SolveLevelsGroupBox.Top:=PluginLevelGroupBox.Top+PluginLevelGroupBox.Height+PluginLevelGroupBox.Left;
        OptimizeSolutionsGroupBox.Top:=SolveLevelsGroupBox.Top;
        GenerateLevelsGroupBox.Top:=SolveLevelsGroupBox.Top;

        SolveLevelsGroupBox.Height:=TabSheetSolver.ClientHeight-SolveLevelsGroupBox.Top-PluginLevelGroupBox.Left;
        OptimizeSolutionsGroupBox.Height:=SolveLevelsGroupBox.Height;
        GenerateLevelsGroupBox.Height:=SolveLevelsGroupBox.Height;

        with OptimizeSolutionsGridPanel do begin
          Width :=OptimizeSolutionsGroupBox.ClientWidth-2*Left;
          Height:=OptimizeSolutionsGroupBox.ClientHeight-Top-Left;
          SolveLevelsGridPanel.SetBounds(Left,Top,Width,Height);
          GenerateLevelsGridPanel.SetBounds(Left,Top,Width,Height);
          end;
        end;
     end;
end;

function  TToolsForm.GetReplaySpeedMovesPerSecond:Integer;
begin
  if   ReplaySpeedMilliSecondsPerMove>0 then
       Result:=Round(1000/ReplaySpeedMilliSecondsPerMove)
  else Result:=0;
end;

procedure TToolsForm.SetReplaySpeedMovesPerSecond(ReplaySpeedMovesPerSecond__:Integer);
begin
  if   ReplaySpeedMovesPerSecond__>0 then
       ReplaySpeedMilliSecondsPerMove:=SokUtil_.Min(Round(1000/     ReplaySpeedMovesPerSecond__),MAX_ANIMATION_TIME_MS)
  else ReplaySpeedMilliSecondsPerMove:=0;
  if   ReplaySpeedMilliSecondsPerMove>0 then // ensure that 'milli-seconds-per-move' and 'moves-per-second' denotes the exact same speed; rounding may otherwise cause differences
       ReplaySpeedMilliSecondsPerMove:=SokUtil_.Min(Round(1000/Self.ReplaySpeedMovesPerSecond  ),MAX_ANIMATION_TIME_MS);
end;

procedure TToolsForm.PluginEditMenuItemCopyStatisticsToClipboardClick(
  Sender: TObject);
begin
  if      (PageControl1.ActivePage=TabSheetSolver   ) and Assigned(SolverTaskQueue   ) then SolverTaskQueue   .CopyStatisticsToClipboard
  else if (PageControl1.ActivePage=TabSheetOptimizer) and Assigned(OptimizerTaskQueue) then OptimizerTaskQueue.CopyStatisticsToClipboard;
end;

procedure TToolsForm.PluginEditMenuItemClearStatisticsClick(
  Sender: TObject);
begin
  if      (PageControl1.ActivePage=TabSheetSolver   ) and Assigned(SolverTaskQueue   ) then SolverTaskQueue   .ClearStatistics
  else if (PageControl1.ActivePage=TabSheetOptimizer) and Assigned(OptimizerTaskQueue) then OptimizerTaskQueue.ClearStatistics;
  ShowStatus;
end;

function  TToolsForm.PluginLevelGroupBoxMinimumWidth:Integer;
begin
  Result:=Succ(2*PluginLevelFileNamePanel.Left+BtnSolveLevel.Width+PluginLevelStringGrid.ColWidths[0]) and (not 1);
end;

function  TToolsForm.LevelGroupBoxMinimumWidth:Integer;
begin
  Result:=Succ(2*LevelNamePanel.Left+
               LevelNamePanel.Width-LevelNamePanel.ClientWidth+
               Max(MAX_BOARD_WIDTH,Max(Canvas.TextWidth(TEXT_LEVEL),Canvas.TextWidth(CandidateText)))+
               8)
          and (not 1); // 'not 1': even number
end;

function  TToolsForm.PluginGroupBoxMinimumWidth:Integer;
begin
  //Result:=Succ(4*SolverComboBox.Left+VerticalScrollBarWidth+BtnSolverBrowse.Width) and (not 1);
  Result:=Succ(4*SolverComboBox.Left+((3*BtnSolverBrowse.Width) div 2)) and (not 1);
end;

function  TToolsForm.SolveLevelsGroupBoxMinimumHeight:Integer;
begin
  Result:=SolveLevelsGroupBox.Tag;
end;

procedure TToolsForm.PluginLevelStringGridTopLeftChanged(Sender: TObject);
begin
  with PluginLevelStringGrid do begin
    if LeftCol<>FixedCols then LeftCol:=FixedCols; // for safety
    if TopRow <>FixedRows then TopRow :=FixedRows;
    end;
end;

procedure TToolsForm.PluginEditMenuItemCopySolutionClick(Sender: TObject);
begin
     if PanelToolTips.Visible then PanelToolTips.Hide;
     if Game.IsBusy and (not Game.IsReplaying) then
        Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)   // the game is busy doing something else than replaying a game; (it's OK to replay a game while it is being copied to the clipboard)
     else
          if (Screen.ActiveForm=Self) and
             CloseEditors and
             Assigned(MainForm.Optimizer) and
             Assigned(OptimizerTaskQueue) then
             if      PageControl1.ActivePage=TabSheetSolver then with PluginLevelInfo.ReplayInfo do begin
                     if   MovesAsText<>'' then begin
                          if not IsLoaded then Replay([roHome],0); // 'Replay()' loads the solution into 'OpenForm.Game'; '[roHome]' ensures that the solution doesn't really starts replaying
                          if IsLoaded then
                             try    Clipboard.AsText:=MovesAsText;
                                    StatusText:=Format(SolutionCopiedToClipboardText__,[OpenForm.Game.History.Top,HistoryPushCountAtMoveNumber(OpenForm.Game.History,OpenForm.Game.History.Top)]);
                                    Self.Refresh; SleepEx(STATUS_BAR_MESSAGE_PAUSE_MS,False);
                                    //StatusText:='';
                             except on E:Exception do Error(E.Message,Application.Title);
                             end
                          else Error(TEXT_TASK_FAILED,Application.Title);
                          end
                     else Msg(HintNoSolutionAvailableText+PERIOD,Self.Caption,MB_OK);
                     end
             else if PageControl1.ActivePage=TabSheetOptimizer then
                     with MainForm.Optimizer do with OptimizerTaskQueue do with StringGrid do begin
                       Enter;
                       try     if Assigned(Levels[Row]) and
                                  (not Levels[Row].SnapshotsAsText.IsEmpty) and
                                  (TSnapshotAsText(Levels[Row].SnapshotsAsText.Last).MovesAsTextLines.First.Text<>'') then
                                  try    Clipboard.AsText:=TSnapshotAsText(Levels[Row].SnapshotsAsText.Last).MovesAsTextLines.First.Text;
                                         with TExtendedSnapshotAsText(Levels[Row].SnapshotsAsText.Last).Metrics do
                                           StatusText:=Format(SolutionCopiedToClipboardText__,[MoveCount,PushCount]);
                                         Self.Refresh; SleepEx(STATUS_BAR_MESSAGE_PAUSE_MS,False);
                                         //StatusText:='';
                                  except on E:Exception do Error(E.Message,Application.Title);
                                  end
                               else Msg(HintNoSolutionAvailableText+PERIOD,Application.Title,MB_OK); // 'Application.Title' and not 'Self.Caption' because the selected row can contain another level than the current primary level for the 'Tools' window
                       finally Leave;
                       end;
                       end;
end;

procedure TToolsForm.SettingsMenuAlternatingOptimizationsEnabledClick(
  Sender: TObject);
begin
  with SettingsMenuAlternatingOptimizationsEnabled do begin
     if Assigned(Sender) then Checked:=not Checked;
     Caption:=DisabledEnabledText[Checked];
     SettingsMenuAlternatingOptimizationsRepeat.Enabled:=Checked;
     SettingsMenuAlternatingOptimizationsType.Enabled:=Checked;
     SettingsMenuAlternatingOptimizationsRepetitionSettings.Enabled:=Checked;
     end;
end;

procedure TToolsForm.SettingsMenuAlternatingOptimizationsTriggerTypeClick(
  Sender: TObject);
begin
  if Assigned(Sender) and (Sender is TMenuItem) then with Sender as TMenuItem do begin
     Checked:=True;
     end;
end;

procedure TToolsForm.SettingsMenuAlternatingOptimizationsRepeatClick(
  Sender: TObject);
var T:TAlternateOptimizationTaskType;
begin
  with SettingsMenuAlternatingOptimizationsRepeat do begin
     if Assigned(Sender) then begin
        Checked:=not Checked;
        if (not Checked) and Assigned(OptimizerTaskQueue) then with OptimizerTaskQueue.AlternateOptimization do
           for T:=Low(Tasks) to High(Tasks) do
               Tasks[T].Transitions:=[]; // resetting the transitions has the consequeuence that the new value 'once' refers to future flip-flops, and not those that may have been made already
        end;
     Caption:=OnceRepeatText[Checked];
     SettingsMenuAlternatingOptimizationsRepetitionSettingsSeparator.Visible:= Checked;
     SettingsMenuAlternatingOptimizationsRepetitionSettings.Visible:=Checked;
     end;
end;

procedure TToolsForm.SettingsMenuAlternatingOptimizationsRepetitionSettingsClick(
  Sender: TObject);
begin
  if Assigned(Sender) and (Sender is TMenuItem) then with Sender as TMenuItem do begin
     Checked:=True;
     end;
end;

procedure TToolsForm.SolveLevelsStringGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var S:String;
begin
  with SolveLevelsStringGrid do begin
    if ARow>=FixedRows then begin
       if ARow<>SolverTaskQueue.HighlightedRowNumber then begin
          if ARow<>Row then begin
             if (ActiveControl<>SolveLevelsStringGrid) then begin
                Canvas.Font .Color:=Font.Color; //clWindowText;
                Canvas.Brush.Color:=Color; //clWindow;
                end
             else begin
                if (ARow<>Row) or (ACol<>Col) then begin
                   Canvas.Font .Color:=Font.Color; //clWindowText;
                   Canvas.Brush.Color:=Color; //clWindow;
                   end
                else begin
                   Canvas.Font .Color:=clHighlightText;
                   Canvas.Brush.Color:=clHighlight;
                   end;
                end;
             end
          else begin
             Canvas.Font .Color:=clHighlightText;
             Canvas.Brush.Color:=clHighlight;
             end;
          end
       else begin
          if //not ((ActiveControl=SolveLevelsStringGrid) and (ACol=Col) and (ARow=Row)) then begin
             (ACol<>Col) or (ARow<>Row) then begin
             Canvas.Font .Color:=clInactiveCaption;
             Canvas.Brush.Color:=clInactiveCaptionText;
             end
          else begin
             Canvas.Font .Color:=clHighlightText;
             Canvas.Brush.Color:=clHighlight;
             end;
          end;
       end
    else begin
       Canvas.Font .Color:=Font.Color; //clWindowText;
       Canvas.Brush.Color:=Color; //clWindow;
       end;

    Canvas.FillRect(Rect);

    S:=Cells[ACol,ARow];
    if S<>'' then begin
       Dec(Rect.Right); Dec(Rect.Bottom);
       Windows.SetBkMode(Canvas.Handle, Windows.OPAQUE);

       if   ARow<FixedRows then
            if        ACol=Ord(slcNo) then
                      Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_RIGHT or DT_SINGLELINE,nil)
            else if   ACol=Ord(slcMetrics) then
                      Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_CENTER or DT_SINGLELINE,nil)
                 else //Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_CENTER or DT_SINGLELINE,nil)
                      Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_LEFT  or DT_SINGLELINE,nil)
       else if   ACol=Ord(slcNo) then
                 Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_RIGHT or DT_SINGLELINE,nil)
       else if   (ACol=Ord(slcMetrics))
                 and
                 ((S='') or IsADigitChar(S[1])) then
                 Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_RIGHT or DT_SINGLELINE,nil)
            else Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_LEFT  or DT_SINGLELINE,nil);
       Inc(Rect.Right); Inc(Rect.Bottom);
       end;

    if (ARow=Row) and (ACol=Col) and (ActiveControl=SolveLevelsStringGrid) then begin
       Canvas.Brush.Color:=clHighlightText;
       Canvas.FrameRect(Rect);
       with Rect do Canvas.FrameRect(Classes.Rect(Succ(Left),Succ(Top),Pred(Right),Pred(Bottom)));
       end;

    Canvas.Font .Color:=Font.Color; //clWindowText;
    Canvas.Brush.Color:=Color; //clWindow;
    end;
end;

procedure TToolsForm.SolveLevelsStringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var Level:TLevel;
begin
  if Assigned(SolverTaskQueue) then with SolverTaskQueue do with StringGrid do begin
    CanSelect:=(ARow>=FixedRows); //and (ACol=Ord(slcLevelName));
    if CanSelect then begin
       if ACol<ColCount then StatusText:=Cells[ACol,ARow];
       CloseEditors;
       if (ARow<>Row) or (Sender=nil) then with PluginLevelInfo.ReplayInfo do with BoardAsText do begin
          Level:=nil;
          if Assigned(Plugin) then Plugin.Enter;
          try
            if ARow<>OldStringGridRow then begin
               RefreshRow(OldStringGridRow);
               OldStringGridRow:=ARow;
               RefreshRow(ARow);
               end;

            if (Sender=nil)
               and
               (Game.IsReplaying
                or
                (ACol=ColCount) // kludge: 'ColCount' signals not to reload the current level; see 'Open1_.TOpenForm.SynchronizedPluginCallback()'
               )
               then begin
               CanSelect:=False;
               end
            else begin
               PluginToolButtonStopReplayClick(nil);
               Level:=Levels[ARow];
               if Assigned(Level) then begin
                  LevelNamePanel.Hint   :=StrWithQuotedAmpersands(VisualFileName(Level.Name));
                  LevelNamePanel.Caption:=SPACE+LevelNamePanel.Hint+SPACE; // the first space is for pretty-printing in the panel; the second space is for a better balance in the "mouse-over" tool-tip

                  IsLoaded              :=False;
                  if   Level.SnapshotsAsText.IsEmpty then
                       MovesAsText      :=''
                  else MovesAsText      :=TSnapshotAsText(Level.SnapshotsAsText.First).MovesAsTextLines.First.Text;
                  MoveCount             :=0;
                  PluginFileName        :='';
                  if (Sender=nil) or
                     (Width             <>Level.Tag.BoardWidth) or
                     (Height            <>Level.Tag.BoardHeight) or
                     (Board             <>Level.BoardAsTextLines.First.Text) then begin
                     Width              :=Level.Tag.BoardWidth;
                     Height             :=Level.Tag.BoardHeight;
                     Board              :=Level.BoardAsTextLines.First.Text;
                     if   OpenForm.Game.LoadFromBoardAsText(Width,Height,False,False,False,False,Board)
                          and
                          OpenForm.Game.LoadSnapshotAsTextString(MovesAsText,True)
                          and
                          ((OpenForm.Game.History.Top>OpenForm.Game.ForcedInitialJumps)
                           or
                           (MovesAsText='')
                          ) then
                          LevelSetForm.ShowBoard0(OpenForm.Game.BoardWidth,OpenForm.Game.BoardHeight,OpenForm.Game.Board,LevelSetForm.GameViewer.BoardImage,False)
                     else LevelSetForm.ShowBoard0(0                       ,                        0,OpenForm.Game.Board,LevelSetForm.GameViewer.BoardImage,False);
                     if   Game.IsReplaying then
                          Inc(ToolsForm.FormResizeCount); // force an update so the new game shows up on the screen when the replay-loop exits
                     end;
                  end
               else begin
                  ClearPluginReplayInfo;
                  LevelNamePanel.Hint:='';
                  LevelNamePanel.Caption:='';
                  LevelSetForm.ShowBoard0(0,0,OpenForm.Game.Board,LevelSetForm.GameViewer.BoardImage,False);
                  LoadLevelFromEditor(True,True);
                  end;
               end;
          finally if   Assigned(Plugin) then Plugin.Leave;
                  if   (Sender=nil) and Self.Visible and (PageControl1.ActivePage=TabSheetSolver) and SolveLevelsGroupBox.Visible and (Screen.ActiveForm=Self) then
                       if Assigned(Level) then SolveLevelsStringGrid.SetFocus;
                  if   PageControl1.ActivePage<>TabSheetSolver then
                       ShowStatus
                  else try    ShowStatusPlugin(Plugin,ARow); // ensure that the status reflects the contents in row 'ARow'; it may differ from 'Row' which hasn't been updated yet
                       except on E:Exception do begin end; // e.g., to catch 'access denied' exceptions when the application is deactivated by a password-protected screen saver
                       end;
          end;
          end;
       end;
    end;
end;

procedure TToolsForm.CleanSolverTaskQueue;
begin
  // the task queue is invisible to the user if the only level on the queue is
  // the current level;
  // if this single level isn't processed by the solver, then the level must
  // be removed from the queue when the user loads another level;
  if not SolveLevelsGroupBox.Visible then
     with SolverTaskQueue do with StringGrid do begin
       if Assigned(Plugin) then Plugin.Enter;
       try     if (RowCount=Succ(FixedRows)) and IsPluginLevel(FixedRows) then begin
                  DeleteRow(FixedRows);
                  end;
       finally if Assigned(Plugin) then Plugin.Leave;
       end;
       end;
end;

function  TTaskQueue.Add(BoardWidth__,BoardHeight__:Integer; LevelTagFlags__:TLevelTagFlagsSet; SnapshotFlags__:Integer; const LevelName__,BoardAsText__:String; Snapshot__:TSnapshot; Selected__:Boolean; var NewLevel__:TLevel):Boolean;
var ARow:Integer; RowRunNo:Cardinal; L:TLevel;
begin // postcondition: if adding a new item, or updating a matching existing item fails, then 'NewLevel__' is nil
  NewLevel__:=nil;
  try
    Plugin.Enter;
    try     if        Lookup(BoardWidth__,BoardHeight__,BoardAsText__,Snapshot__,L) then begin
                      if   (L.Flags  < StringGrid.FixedRows) or
                           (L.Flags  >=StringGrid.RowCount) or
                           (L        <>Levels[L.Flags]) then begin // 'True': the task is on the queue but it's not visible in the string-grid
                           NewLevel__:=L;
                           NewLevel__.SetName(LevelName__);
                           RowRunNo  :=GetNextRunNo;
                           end
                      else RowRunNo:=Cardinal(StringGrid.Objects[Ord(oscSnapshotName),L.Flags]);
                      end
            else if   Plugin.LoadLevel(BoardWidth__,BoardHeight__,-1,SnapshotFlags__,
                                       LevelName__,BoardAsText__,
                                       Snapshot__,True,False,NewLevel__) then begin
                      RowRunNo:=GetNextRunNo;
                      end
                 else begin NewLevel__:=nil; RowRunNo:=0;
                      end;

            if   Assigned(NewLevel__) then with StringGrid do begin
                 NewLevel__.Tag.Flags:=NewLevel__.Tag.Flags+LevelTagFlags__;
                 // the task is now on the queue but not yet in the string-grid; add it to the string-grid, i.e., update the screen
                 if MakeRow(ARow) then begin
                    NewLevel__.Flags:=ARow; // 'Level.Flags' links back from the level to its row-number in the string-grid
                    Levels[ARow]:=NewLevel__;
                    Selected[ARow]:=Selected__;
                    RefreshRow(ARow);
                    ToolsForm.MakeAllColumnsFullyVisible;
                    Objects[Ord(oscSnapshotName),ARow]:=TObject(RowRunNo);
                    end;
                 end;
    finally Plugin.Leave
    end;
  except on E:Exception do begin
            if Assigned(NewLevel__) then begin
               L:=NewLevel__; NewLevel__:=nil; Plugin.DeleteLevel(L);
               end;
            Error(E.Message,Application.Title);
            end;
  end;
  Result:=Assigned(NewLevel__);
end;

function  TTaskQueue.AddStatistics(const PluginName__,PluginTypeText__,LevelName__:String; TimeMS__:TTimeMS; TaskResult__:TPluginResult; MoveCount__,PushCount__:Integer; const TaskResultText__:String):Boolean;
var sResult,sTime:String;
begin {AddStatistics}
  Result:=False;
  try    if not Assigned(fStatisticsText) then begin
            fStatisticsText:=TMemo.Create(nil);
            //fStatisticsText:=TRichEdit.Create(nil);
            //fStatisticsText.PlainText:=True;
            fStatisticsText.Visible:=False;
            fStatisticsText.WordWrap:=False;
            fStatisticsText.Parent:=ToolsForm;
            end;
         if Assigned(fStatisticsText) then with StatisticsText.Lines do begin
            if Count<TASK_QUEUE_STATISTICS_HEADER_LINES_COUNT then begin
               Clear;
               Add(StrLine(TASK_QUEUE_STATISTICS_LINE_LENGTH));
               Add('Sokoban YASC - Tools - '+PluginTypeText__+' - Statistics');
               Add('');
               Add(StrLine(TASK_QUEUE_STATISTICS_LINE_LENGTH));
               Add('   Task       Time Plugin               Result                     Moves  Pushes Level');
               Add(StrLine(TASK_QUEUE_STATISTICS_LINE_LENGTH));
               if Count<>TASK_QUEUE_STATISTICS_HEADER_LINES_COUNT then
                  raise Exception.Create(InternalErrorText+' (TTaskQueue.AddStatistics)');
               end;

            Inc(fStatisticsMetrics[gmMoves ],MoveCount__);
            Inc(fStatisticsMetrics[gmPushes],PushCount__);

            //sResult:=TaskResultText__;
            if   TaskResult__=prOk then begin
                 Inc(fStatisticsSucceededCount);
                 sResult:=OKText;
                 end
            else if   (TaskResult__=prUnsolved) and Assigned(Plugin) and (Plugin is TOptimizerPlugin) then
                      sResult:=NoOptimizationsFoundText
                 else sResult:=PluginResultText[TaskResult__];
            if   Length(sResult)>24 then sResult:=Copy(sResult,1,24);
            if   TimeMS__>=500 {milli seconds} then sTime:=FormatTimeMS(TimeMS__)
            else sTime:='';
            Inc(fStatisticsTaskCount); Inc(fStatisticsTimeMS,TimeMS__);
            Add(Format('%7d %10s %-20s %-24s %7s %7s %s',[fStatisticsTaskCount,sTime,Copy(PluginName__,1,20),sResult,IntToStrOrBlank(MoveCount__),IntToStrOrBlank(PushCount__),LevelName__]));
            if (StatisticsTaskCount=1) and Assigned(ToolsForm) and (not ToolsForm.PluginEditMenuItemStatistics.Enabled) then
               ToolsForm.ShowStatus;
            end;
  except on E:Exception do begin
            if fStatisticsTaskCount=0 then begin
               fStatisticsText.Free; fStatisticsText:=nil;
               end;
            Result:=Error(E.Message,Application.Title);
            end;
  end;
end;

function  TTaskQueue.BestMatchOptimization(Optimization__:Integer):Integer;
var i:Integer;
begin
  Result:=0;
  with ToolsForm.OptimizationComboBox do with Items do begin
    if Count=0 then ToolsForm.FillOptimizationComboBox;
    for i:=0 to Pred(Count) do
        if (Integer(Objects[i]) and Optimization__)<>0 then begin
           Result:=Integer(Objects[i]); exit;
           end;
    end;
end;

procedure TTaskQueue.Clear;
var i,j:Integer; ACheckBox:TCheckBox;
begin
  if Assigned(Plugin) then begin
     Plugin.ClearQueue(False);
     end;
  RowCount:=Succ(StringGrid.FixedRows);
  Selected[Pred(RowCount)]:=False;
  with ItemsCheckBox do begin
   Checked:=False; Enabled:=False;
   end;
  fSelectedCount:=0;
  with StringGrid do
    for j:=0 to Pred(RowCount) do
        for i:=0 to Pred(ColCount) do begin
            if (j>=FixedRows) or (i=Ord(oscNo)) then Cells[i,j]:='';
            if i<>Ord(oscNo) then Objects[i,j]:=nil // checkboxes are stored in 'Objects[Ord(oscNo),Row]'
            else if Assigned(Objects[i,j]) and
                    GetCheckBoxForRow(StringGrid.FixedRows,ACheckBox) and (TCheckBox(Objects[i,j])<>ACheckBox) then
                    with TCheckBox(Objects[i,j]) do Visible:=False; // there shouldn't be any other visible checkboxes than the one attached to the single unfixed row; if changing the row count to 'StringGrid.FixedRows+1' failed, then this logic is flawed, but it will have to do
            end;
  if Self=ToolsForm.GeneratorTaskQueue then
     ToolsForm.LoadLevelFromEditor(True,True); // actually, this doesn't load the level from the editor; it clears the loaded level on the screen
end;

procedure TTaskQueue.ClearStatistics;
begin
  fStatisticsSucceededCount:=0; fStatisticsTaskCount:=0; fStatisticsTimeMS:=0;
  FillChar(fStatisticsMetrics,SizeOf(fStatisticsMetrics),0);
  try    if Assigned(StatisticsText) then StatisticsText.Clear;
  except on E:Exception do begin
         fStatisticsText.Free; fStatisticsText:=nil;
         Error(E.Message,Application.Title);
         end;
  end;
end;

function  TTaskQueue.CopyStatisticsToClipboard:Boolean;
begin
  Result:=Assigned(StatisticsText);
  if Result then
     try    try     StatisticsText.Lines[2]:=DateText+COLON+SPACE+FormatDateTime(FORMAT_DATE_TIME,Now);
                    StatisticsText.Lines.Add(StrLine(TASK_QUEUE_STATISTICS_LINE_LENGTH));
                    StatisticsText.Lines.Add(Format(':%6d %10s %-20s %-24s %7s %7s     :',[StatisticsTaskCount,FormatTimeMS(StatisticsTimeMS),'Total','OK: '+IntToStr(StatisticsSucceededCount)+' Failed: '+IntToStr(StatisticsTaskCount-StatisticsSucceededCount),IntToStrOrBlank(fStatisticsMetrics[gmMoves]),IntToStrOrBlank(fStatisticsMetrics[gmPushes])]));
                    StatisticsText.Lines.Add(StrLine(TASK_QUEUE_STATISTICS_LINE_LENGTH));
                    if Self<>ToolsForm.GeneratorTaskQueue then begin
                       StatisticsText.Lines.Add('Moves and pushes statistics are based on the results reported by the plugin(s).');
                       StatisticsText.Lines.Add('The results have not been verified by Sokoban YASC.');
                       StatisticsText.Lines.Add(StrLine(TASK_QUEUE_STATISTICS_LINE_LENGTH));
                       end;
                    Clipboard.AsText:=StatisticsText.Lines.Text;
                    ToolsForm.StatusText:=CopiedStatisticsToClipboardText;
            finally with StatisticsText.Lines do
                      while Count>TASK_QUEUE_STATISTICS_HEADER_LINES_COUNT+StatisticsTaskCount do
                        Delete(Pred(Count));
            end;
     except on E:Exception do begin
            Result:=Error(E.Message,Application.Title);
            end;
     end;
end;

constructor TTaskQueue.Create(Plugin__:TPlugin; StringGrid__:TStringGrid; ItemsCheckBox__:TCheckBox; PopupMenu__:TPopupMenu);
begin
  fStatisticsText:=nil; ClearStatistics;
  fPlugin:=Plugin__; fStringGrid:=StringGrid__;
  fItemsCheckBox:=ItemsCheckBox__; fPopupMenu:=PopupMenu__;
  fSelectedCount:=0; fHighlightedRowNumber:=-1; fRunNo:=0;
  RowCount:=Succ(StringGrid.FixedRows);
  StringGrid   .OnDragDrop      :=OnStringGridDragDrop;
  StringGrid   .OnDragOver      :=OnStringGridDragOver;
  StringGrid   .OnEnter         :=OnStringGridEnter;
  StringGrid   .OnExit          :=OnStringGridExit;
  StringGrid   .OnKeyUp         :=OnStringGridKeyUp;
  StringGrid   .OnMouseDown     :=OnStringGridMouseDown;
  StringGrid   .OnMouseMove     :=OnStringGridMouseMove;
  StringGrid   .OnMouseUp       :=OnStringGridMouseUp;
  StringGrid   .OnTopLeftChanged:=OnStringGridTopLeftChanged;
  ItemsCheckBox.OnMouseDown     :=ToolsForm.FormMouseDown;
  ItemsCheckBox.OnMouseUp       :=OnCheckBoxMouseUp;
end;

function  TTaskQueue.DeleteRow(Row__:Integer):Boolean;
var ACol,ARow,Index:Integer; Level:TLevel;
begin
  Result:=False;
  Plugin.Enter;
  with StringGrid do
    try     if (Row__>=FixedRows) and (Row__<RowCount) then begin
               Level:=Levels[Row__];
               if not (Assigned(Level) and (ltfLocked in Level.Tag.Flags)) then begin
                  Result:=True;

                  if Assigned(Level) then begin
                     Selected[Row__]:=False;
                     Levels[Row__]:=nil;
                     if (ltfNew in Level.Tag.Flags) and (not Level.SnapshotsAsText.IsEmpty) and (Self<>ToolsForm.GeneratorTaskQueue) then begin
                        // this is a new solution; keep it on the internal queue for later import, but hide it from the user
                        Level.Flags:=-1;
                        if ((Level.Tag.Flags*[ltfNew,ltfProcessed])=[ltfNew]) then
                           Include(Level.Tag.Flags,ltfProcessed); // the optimizer requires '[lftNew,ltfProcessed]' for importing the level
                        end
                     else begin
                        if (Self=ToolsForm.GeneratorTaskQueue) and GALookupLevel(Level,Index) then
                           GADeleteIndividual(Index);
                        Plugin.SokoFile.Levels.Remove(Level,True); // destroy the item; note that this is not the level itself; it's only an item on the task queue
                        end;
                     end;

                  for ARow:=Row__ to Pred(Pred(RowCount)) do begin
                      for ACol:=0 to Pred(ColCount) do
                          if ACol<>Ord(oscNo) then Objects[ACol,ARow]:=Objects[ACol,Succ(ARow)];
                      if Assigned(Levels[ARow]) then Levels[ARow].Flags:=ARow;
                      end;

                  Self.RowCount:=Pred(RowCount);
                  RefreshRow(Row);
                  end;
               end;
    finally Plugin.Leave;
    end;
end;

destructor TTaskQueue.Destroy;
begin
  ClearStatistics;
  fStatisticsText.Free; fStatisticsText:=nil;
end;

procedure TTaskQueue.DestroyCheckBox(Row__:Integer);
var CheckBox:TCheckBox;
begin
  with StringGrid do
    if (Row__>=0) and (Row__<RowCount) then begin
       CheckBox:=TCheckBox(Objects[Ord(oscNo),Row__]);
       Objects[Ord(oscNo),Row__]:=nil;
       if Assigned(CheckBox) then begin
          CheckBox.Hide;
          CheckBox.Free;
          end;
       end;
end;

procedure TTaskQueue.FocusLevelName(const LevelName__:String; First__:Boolean);
var ARow,FocusRow:Integer; b:Boolean; Level:TLevel;
begin
  with StringGrid do begin
    if Assigned(Plugin) then Plugin.Enter;
    try    FocusRow:=-1;
           for ARow:=FixedRows to Pred(RowCount) do begin
                Level:=Levels[ARow];
                if   Assigned(Level) and
                     StrEqual(Level.Name,LevelName__) and
                     (not ( (FocusRow>=0) and First__ ) ) then begin
                     FocusRow:=ARow; b:=True;
                     if First__ then break; // quick-and-dirty exit loop
                     end;
                end;
            if FocusRow>=0 then Row:=FocusRow;
            ScrollInView(Row);
            if Assigned(Plugin) then
               if      Plugin is TOptimizerPlugin then
                       ToolsForm.OptimizeSolutionsStringGridSelectCell(nil,0,Row,b)
               else if Plugin is TSolverPlugin then
                       ToolsForm.SolveLevelsStringGridSelectCell(nil,0,Row,b)
               else if Plugin is TGenerator then
                       ToolsForm.GenerateLevelsStringGridSelectCell(nil,0,Row,b);
    finally if Assigned(Plugin) then Plugin.Leave;
    end;
    end;
end;

function  TTaskQueue.GetCheckBoxForRow(Row__:Integer; var CheckBox__:TCheckBox):Boolean;
var b:Boolean; Level:TLevel;
begin // precondition: the plugin-thread is blocked
  // all rows share a limited number of checkboxes; physically, checkboxes are only allocated for the number of visible non-fixed rows in the string-grid ('VisibleRowCount'+1)
  Result:=False;
  with StringGrid do begin
    Dec(Row__,TopRow);
    if (Row__>=0) and (Row__<=VisibleRowCount) and (Row__<RowCount) then begin
       CheckBox__:=TCheckBox(Objects[Ord(oscNo),Row__]);
       if not Assigned(CheckBox__) then begin
          try    CheckBox__:=TCheckBox.Create(ToolsForm);
                 with CheckBox__ do begin
                   Parent:=StringGrid;
                   Caption:='';
                   Height:=ItemsCheckBox.Height; // otherwise, the checkbox size doesn't match the checkbox in the column header when Windows uses "small fonts" settings
                   Width:=Height;
                   Enabled:=True;
                   AllowGrayed:=True; Checked:=False; State:=cbUnchecked;
                   OnMouseDown:=ToolsForm.FormMouseDown;
                   OnMouseMove:=ToolsForm.ControlMouseMove;
                   OnMouseUp  :=OnCheckBoxMouseUp;
                   Hint:=HintTaskQueueSelectItemText;
                   end;
          except on E:Exception do begin
                    CheckBox__.Free; CheckBox__:=nil; Objects[Ord(oscNo),Row__]:=nil;
                    Error(E.Message,Application.Title);
                    end;
          end;
          Objects[Ord(oscNo),Row__]:=CheckBox__;
          end;
       if Assigned(CheckBox__) then  with CheckBox__ do begin
          Tag    :=TopRow+Row__; // 'Tag' = row number currently assigned to the checkbox
          Top    :=(FixedRows+Row__)*(StringGrid.DefaultRowHeight+StringGrid.GridLineWidth) + ((StringGrid.DefaultRowHeight-Height) div 2);
          Left   :=(StringGrid.ColWidths[Ord(oscSelect)]-Width) div 2;
          Level  :=Levels[Tag];
          b      :=(Tag<RowCount)
                   and
                   ((not Assigned(Level))
                    or
                    ((not (ltfLocked in Level.Tag.Flags))
                     and
                     (Level.SnapshotsAsText.IsEmpty
                      or
                      (TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.First.Text<>'')
                      or
                      (Self=ToolsForm.GeneratorTaskQueue)
                     )
                    )
                   );
          if b<>Visible then Visible:=b;
          b:=Assigned(Level);
          if Enabled<>b then Enabled:=b;
          Result :=True;
          end;
       end;
    end;
end;

function  TTaskQueue.GetLevels(Row__:Integer):TLevel;
begin
  if Assigned(Plugin) then Plugin.Enter;
  try
    with StringGrid do
      if   (Row__>=FixedRows) and (Row__<RowCount) then
           Result:=TLevel(Objects[Ord(oscLevelName),Row__])
      else Result:=nil;
  finally if Assigned(Plugin) then Plugin.Leave;
  end;
end;

function  TTaskQueue.GetNextRunNo:Cardinal;
var ARow:Integer; MinRunNo,RowRunNo:Cardinal;
begin
  with StringGrid do begin
    if Assigned(Plugin) then Plugin.Enter;
    try     if fRunNo<High(fRunNo) then
               Inc(fRunNo)
            else begin
               MinRunNo:=High(MinRunNo); fRunNo:=0;
               for ARow:=FixedRows to Pred(RowCount) do begin                   // find minimum run number in use
                   RowRunNo:=Cardinal(Objects[Ord(oscSnapshotName),ARow]);
                   if (RowRunNo<>0) and (RowRunNo<MinRunNo) then MinRunNo:=RowRunNo;
                   end;
               for ARow:=FixedRows to Pred(RowCount) do begin                   // decrease existing run numbers and find new maximum value in use
                   RowRunNo:=Cardinal(Objects[Ord(oscSnapshotName),ARow]);
                   if RowRunNo<>0 then begin
                      RowRunNo:=Succ(RowRunNo-MinRunNo);
                      if RowRunNo>fRunNo then fRunNo:=RowRunNo;
                      Objects[Ord(oscSnapshotName),ARow]:=TObject(RowRunNo);
                      end;
                   end;
               if fRunNo<High(fRunNo) then Inc(fRunNo)
               else begin
                  // theoretically, it could happen that this simple calculation
                  // fails and doesn't produce a vacant run number; however,
                  // this issue isn't important enough to justify more complex machinery;
                  // the only small consequence of an overflowing next number is
                  // that some tasks are grouped together by sharing the same
                  // high-value run number;
                  end;
               end;
            Result:=fRunNo;
    finally if Assigned(Plugin) then Plugin.Leave;
    end;
    end;
end;

function  TTaskQueue.GetOptimizationFlags(Row__:Integer):Integer;
begin
  Plugin.Enter;
  try     if   Assigned(Levels[Row__]) then with Levels[Row__] do
               Result:=TSnapshotAsText(SnapshotsAsText.Last).Tag
          else Result:=0;
  finally Plugin.Leave;
  end;
end;

function  TTaskQueue.GetRowCount:Integer;
begin
  Plugin.Enter;
  try     Result:=StringGrid.RowCount;
  finally Plugin.Leave;
  end;
end;

function  TTaskQueue.GetSelected(Row__:Integer):Boolean;
var Level:TLevel;
begin
  if Assigned(Plugin) then Plugin.Enter;
  try     Level:=Levels[Row__];
          Result:=Assigned(Level) and (ltfSelected in Level.Tag.Flags);
  finally if Assigned(Plugin) then Plugin.Leave;
  end;
end;

function  TTaskQueue.HasCheckBoxInRow(Row__:Integer; var CheckBox__:TCheckBox):Boolean;
begin
  CheckBox__:=nil;
  Plugin.Enter;
  try     with StringGrid do
            if (Row__>=0) and (Row__<RowCount) then
               CheckBox__:=TCheckBox(Objects[Ord(oscNo),Row__]);
          Result:=Assigned(CheckBox__);
  finally Plugin.Leave;
  end;
end;

function  TTaskQueue.ImportSolutionsForCurrentLevel(DeleteImportedLevels__:Boolean):Integer;
var CurrentLevel,NextLevel:TLevel;
begin
  with ToolsForm do begin
    Result:=0;
    if PluginLevelInfo.IsALegalLevel and
       (PluginLevelInfo.NewGamesCount>0) and
       (not Modified) and
       (not MainForm.Modified) and
       (not Editor.Selection.Enabled) and
       (not IsANewFileName(Editor.FileName)) and
       Plugin.HasSokoFile
       then begin
       // update the currently selected level
       // with new solutions, so the optimizer
       // works with the best found solutions
       Plugin.Enter;
       try     CurrentLevel:=TLevel(Plugin.SokoFile.Levels.First);
               while CurrentLevel<>nil do begin
                 NextLevel:=TLevel(CurrentLevel.Next);
                 if ((CurrentLevel.Tag.Flags*[ltfSelected,ltfSelectedForFurtherProcessing,ltfLocked,ltfNew,ltfProcessed])=[ltfNew,ltfProcessed]) and
                    (not CurrentLevel.BoardAsTextLines.IsEmpty) and
                    IsToolsFormPluginLevel(CurrentLevel.Tag.BoardWidth,CurrentLevel.Tag.BoardHeight,CurrentLevel.BoardAsTextLines.First.Text,CurrentLevel.Name) then begin
                    if Plugin.ImportGame(CurrentLevel) then begin
                       Inc(Result);
                       if DeleteImportedLevels__ then begin
                          Plugin.DeleteLevel(CurrentLevel);
                          //PluginLevelInfo.NewGamesCount:=Max(0,Pred(PluginLevelInfo.NewGamesCount)); // 'Max(0,...)': just to be sure
                          end;
                       end;
                    end;
                 CurrentLevel:=NextLevel;
                 end;
       finally Plugin.Leave;
       end;
       end;
    end;
end;

procedure TTaskQueue.ImportFromOtherPlugins(ImportSolverSolutions__:Boolean);
var ARow,i:Integer; b:Boolean;
    L,Level,NextLevel,NoSolutionsLevel,OldLevel:TLevel;
    SnapshotAsText:TExtendedSnapshotAsText;
begin // kludge: implemented for 'MainForm.Optimizer' only, and only importing items from 'MainForm.Solver'
  Plugin.Enter;
  try     if Assigned(MainForm.Solver) and (Plugin=MainForm.Optimizer) then begin
             MainForm.Solver.Enter;
             try     if (MainForm.Solver.PendingResultsCount<>0) and
                        MainForm.Solver.HasSokoFile then begin // '.HasSokoFile': just to be sure
                        // locate the dummy-item, if any, with 'no solutions' for the currently active level
                        NoSolutionsLevel:=nil;
                        for ARow:=Pred(StringGrid.ColCount) downto StringGrid.FixedRows do
                            if (StringGrid.Cells[Ord(oscMetrics),ARow]='') and  // '': the dummy-item is the only row with empty metrics
                               Assigned(Levels[ARow]) and
                               (Levels[ARow].Flags=ARow) then begin
                               NoSolutionsLevel:=Levels[ARow]; break;
                               end;

                        Level:=TLevel(MainForm.Solver.SokoFile.Levels.First);
                        while Assigned(Level) do begin
                          NextLevel:=TLevel(Level.Next);
                          if ((Level.Tag.Flags*[ltfSelected,ltfSelectedForFurtherProcessing,ltfLocked,ltfNew,ltfProcessed])=[ltfNew,ltfProcessed])
                             and
                             (ImportSolverSolutions__
                              or
                              (not (ltfSolver in Level.Tag.Flags))
                             )
                             then begin
                             if Level=ToolsForm.SolverTaskQueue.Levels[Level.Flags] then begin
                                ToolsForm.SolverTaskQueue.Levels[Level.Flags]:=nil;
                                end;
                             MainForm.Solver.SokoFile.Levels.Remove(Level,False);
                             MainForm.Solver.PendingResultsCount:=MainForm.Solver.PendingResultsCount-Level.SnapshotsAsText.Count;

                             L:=nil;
                             if Plugin.Lookup(Level.Tag.BoardWidth,Level.Tag.BoardHeight,Level.BoardAsTextLines.First.Text,'',L) then begin
                                // this level is represented on the optimizer queue;
                                // check the individual solutions
                                for i:=Pred(Level.SnapshotsAsText.Count) downto 0 do begin
                                    SnapshotAsText:=TExtendedSnapshotAsText(Level.SnapshotsAsText.GetItemByIndex(i));
                                    if SnapshotAsText.MovesAsTextLines.IsEmpty
                                       or
                                       Plugin.Lookup(Level.Tag.BoardWidth,Level.Tag.BoardHeight,Level.BoardAsTextLines.First.Text,
                                                     SnapshotAsText.MovesAsTextLines.First.Text,
                                                     OldLevel) then begin // 'True': a copy of this solution is already on the optimizer queue
                                       Level.SnapshotsAsText.Remove(SnapshotAsText,True);
                                       ToolsForm.OptimizerTaskQueue.ResurrectLevel(OldLevel); // ensure that the old solution is visible in the string-grid
                                       if (not (ltfNew in OldLevel.Tag.Flags))
                                          //and
                                          //(ToolsForm.Modified or MainForm.Modified)
                                          and
                                          (not OldLevel.BoardAsTextLines.IsEmpty)
                                          and
                                          ToolsForm.IsToolsFormPluginLevel(OldLevel.Tag.BoardWidth,OldLevel.Tag.BoardHeight,OldLevel.BoardAsTextLines.First.Text,OldLevel.Name) then begin
                                          // the existing copy of the new
                                          // solution has already been
                                          // processed and imported by the main
                                          // form;
                                          // if the user modifies the level
                                          // back and forth, the solution may
                                          // get lost unless it's imported again
                                          // by the main form, hence, raise its
                                          // 'ltfNew' flag;
                                          Include(OldLevel.Tag.Flags,ltfNew);
                                          Plugin.PendingResultsCount:=Succ(Plugin.PendingResultsCount);
                                          end;
                                       end;
                                    end;
                                end;

                             if Level.SnapshotsAsText.IsEmpty  then // 'True': a copy of the solution(s) for this level is/are already on the optimizer queue
                                Level.Free
                             else begin
                                if ltfSolver in Level.Tag.Flags then // 'True': the solution was produced by the solver and not by the optimizer
                                   while Assigned(L) do begin // 'True': the level is already represented on the queue
                                      if (ltfBest in L.Tag.Flags) and
                                         (not L.SnapshotsAsText.IsEmpty) and
                                         Assigned(SnapshotsForm) then
                                         with TExtendedSnapshotAsText(L.SnapshotsAsText.Last) do
                                           try    // change 'Best Solution' to 'Solution';
                                                  // it may not be necessary, but it's an easy
                                                  // way to reduce the potential for confusion
                                                  SetName(SnapshotsForm.DethronedSolutionName(Name));
                                                  Exclude(L.Tag.Flags,ltfBest);
                                           except on E:Exception do begin end;
                                           end;
                                      repeat L:=TLevel(L.Next);
                                      until (not Assigned(L)) // until all levels have been tested, or until the next matching level has been found (note that the level name isn't taken into account)
                                            or
                                            ((L.Tag.BoardWidth=Level.Tag.BoardWidth)
                                             and
                                             (L.Tag.BoardHeight=Level.Tag.BoardHeight)
                                             and
                                             (not L.BoardAsTextLines.IsEmpty)
                                             and
                                             (not Level.BoardAsTextLines.IsEmpty)
                                             and
                                             (L.BoardAsTextLines.First.Text=Level.BoardAsTextLines.First.Text)
                                            );
                                      end;

                                Level.Flags:=-1; // '-1': the level hasn't been added to the string-grid yet
                                Level.Tag.Flags:=Level.Tag.Flags*[ltfNew,ltfSolver];

                                Plugin.SokoFile.Levels.Add(Level);
                                Plugin.PendingResultsCount:=Succ(Plugin.PendingResultsCount);
                                if Assigned(Levels[Pred(RowCount)]) or (RowCount<=StringGrid.FixedRows) then RowCount:=Succ(RowCount);
                                if (RowCount>StringGrid.FixedRows)
                                   and
                                   (not Assigned(Levels[Pred(RowCount)])) then begin // 'True': the last row in the string-grid is ready for use
                                   Level.Flags:=Pred(RowCount);
                                   Levels[Level.Flags]:=Level;
                                   RefreshRow(Level.Flags);
                                   if ltfSolver in Level.Tag.Flags then // 'True': the solution was produced by the solver and not the optimizer, hence, select it for optimization now
                                      Selected[Level.Flags]:=True;
                                   if StringGrid.Row=Level.Flags then
                                      ToolsForm.OptimizeSolutionsStringGridSelectCell(nil,0,StringGrid.Row,b);
                                   if (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer) and Assigned(OpenForm) then
                                      OpenForm.EnableDisablePluginControls(Self,Plugin,False);
                                   ToolsForm.ShowStatus;

                                   if Assigned(NoSolutionsLevel) and
                                      StrEqual(Level.Name,NoSolutionsLevel.Name) and
                                      (Level.Tag.BoardWidth=NoSolutionsLevel.Tag.BoardWidth) and
                                      (Level.Tag.BoardHeight=NoSolutionsLevel.Tag.BoardHeight) and
                                      (Level.BoardAsTextLines.First.Text=NoSolutionsLevel.BoardAsTextLines.First.Text) then begin
                                      Plugin.DeleteLevel(NoSolutionsLevel);
                                      NoSolutionsLevel:=nil;
                                      end;

                                   end;
                                end;
                             end;
                          Level:=NextLevel;
                          end;
                        end;
             finally MainForm.Solver.Leave;
                     ToolsForm.MakeAllColumnsFullyVisible;
             end;
             end;
  finally Plugin.Leave;
          if Assigned(ToolsForm) and
             ToolsForm.Visible and
             (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver) then with ToolsForm do begin
             SolverTaskQueue.Refresh(True);
             SolveLevelsStringGridSelectCell(nil,0,SolverTaskQueue.StringGrid.Row,b);
             ShowStatus;
             end;
  end;
end;

procedure TTaskQueue.InitializeItemsCheckBox;
begin
  with ItemsCheckBox do begin
    Parent:=StringGrid;
    Width:=Height;
    Left:=(StringGrid.ColWidths[Ord(oscSelect)]-Width) div 2;
    Top :=(StringGrid.DefaultRowHeight-Height) div 2;
    Visible:=True; Enabled:=True;
    AllowGrayed:=True; Checked:=False; State:=cbUnchecked;
    Tag:=-1;
    end;
end;

function  TTaskQueue.IsEmpty:Boolean;
begin
  if Assigned(Plugin) then Plugin.Enter;
  try     with StringGrid do Result:=(RowCount<=Succ(FixedRows)) and (not Assigned(Levels[FixedRows]));
  finally if Assigned(Plugin) then Plugin.Leave;
  end;
end;

function  TTaskQueue.IsPluginLevel(Row__:Integer):Boolean;
var Level:TLevel;
begin
  with ToolsForm.PluginLevelInfo do begin
    if Assigned(Plugin) then Plugin.Enter;
    try     Level:=Levels[Row__];
            Result:=Assigned(Level) and
                    StrEqual(ToolsForm.Editor.FileName,Level.Name) and
                    (Level.Tag.BoardHeight=BoardAsText.Height) and
                    (Level.Tag.BoardWidth =BoardAsText.Width) and
                    (not Level.BoardAsTextLines.IsEmpty) and
                    (Level.BoardAsTextLines.First.Text=BoardAsText.Board) and
                    StrEqual(LevelName,ToolsForm.Editor.FileName)
                    and
                    IsALegalLevel
                    and
                    (not ToolsForm.Editor.Selection.Enabled);
    finally if Assigned(Plugin) then Plugin.Leave;
    end;
    end;
end;

function  TTaskQueue.Lookup(BoardWidth__,BoardHeight__:Integer; const BoardAsText__:String; Snapshot__:TSnapshot; var Level__:TLevel):Boolean;
begin
  Level__:=nil;
  Plugin.Enter;
  try     if Assigned(Plugin.SokoFile) then Level__:=TLevel(Plugin.SokoFile.Levels.First);
          while Assigned(Level__)
                and
                ((Snapshot__.MoveCount     <>TExtendedSnapshotAsText(Level__.SnapshotsAsText.Last).Metrics.MoveCount) // 'Last': output from the optimizer is pushed on the snapshot list, hence, the input is the last item
                 or
                 (BoardWidth__             <>Level__.Tag.BoardWidth)
                 or
                 (BoardHeight__            <>Level__.Tag.BoardHeight)
                 or
                 (BoardAsText__            <>Level__.BoardAsTextLines.First.Text)
                 or
                 (Snapshot__.GetMovesAsText<>TExtendedSnapshotAsText(Level__.SnapshotsAsText.Last).MovesAsTextLines.First.Text) // 'Last': output from the optimizer is pushed on the snapshot list, hence, the input is the last item
                ) do
                Level__:=TLevel(Level__.Next);
  finally Plugin.Leave;
  end;
  Result:=Assigned(Level__);
end;

function  TTaskQueue.MakeRow(var ARow__:Integer):Boolean;
begin // finds an empty row in the string grid; if there isn't one available, then an attempt is made to extend the grid with one more row
  with StringGrid do begin
    if Assigned(Plugin) then Plugin.Enter;
    try     ARow__:=Pred(RowCount); // search backwards through the rows to find the last occupied row
            while (ARow__>=FixedRows) and (not Assigned(Levels[ARow__])) do Dec(ARow__);
            Inc(ARow__);
            if (ARow__=RowCount) and (RowCount<High(RowCount)) then RowCount:=Succ(RowCount);
            Result:=ARow__<RowCount; // 'True': finding or making an empty row succeeded
    finally if Assigned(Plugin) then Plugin.Leave;
    end;
    end;
end;

procedure TTaskQueue.Reload(Clear__:Boolean);
var ARow,Count,SnapshotFlags:Integer; AddToolsFormLevelVersion,OldModified:Boolean;
    SecondaryScoreMetrics:TSecondaryScoreMetrics;
    Level,LevelBestMoves,LevelBestPushes,NextLevel,NoSolutionsLevel:TLevel;
    SnapShot:TSnapshot; NewSnapshot:TExtendedSnapshotAsText;
begin
  if Assigned(Plugin) and
     Assigned(Plugin.PluginThread) then begin
     Plugin.Enter;
     try
       Plugin.Suspend; // so reload is as quick as possible; the user is waiting for the updated window
       try
               Count:=0;

               if Clear__ then begin
                  if (Self=ToolsForm.SolverTaskQueue) or (Self=ToolsForm.OptimizerTaskQueue) then begin
                     // solver task queue and optimizer task queue:
                     // clean up the task queue by dropping items unless they are:
                     // * related to the current game, or
                     // * currently being processed by the plugin (locked), or
                     // * actively selected for processing by the user, or
                     // * selected for further processing by the application, or
                     // * pending solutions that the user hasn't seen yet
                     for ARow:=StringGrid.FixedRows to Pred(StringGrid.RowCount) do begin
                         Level:=Levels[ARow];
                         if Assigned(Level) then begin
                            if (Level.Tag.Flags*[ltfSelected,ltfSelectedForFurtherProcessing,ltfLocked]=[]) and
                               (not StrEqual(Level.Name,MainForm.Game.FileName)) and
                               (MainForm.Game.GameState<>gsNull) // if the current level isn't a legal level then keep all items on the queue
                               then begin
                               Levels[ARow]:=nil;
                               if   ltfNew in Level.Tag.Flags then
                                    Level.Flags:=-1 // '-1': the level isn't in the string-grid anymore, even though it still is on the internal queue
                               else Plugin.DeleteLevel(Level); // if it isn't a new solution then there is no reason to keep the item on the queue
                               if ARow<Pred(StringGrid.RowCount) then // otherwise keep the last row for the time being; it may be filled in later with solutions for the currently active level, in which case this might save a resizing of the string-grid
                                  Inc(Count);
                               end;
                            end
                         else
                            if ARow<Pred(StringGrid.RowCount) then Inc(Count); // 'True': there is an interior empty row in the string-grid
                         end;

                     // there may be more items on the queue than the ones listed
                     // in the string-grid, hence, make a second pass through the items
                     // to delete any inactive items, and add any items related to the
                     // current game that aren't visible in the string-grid right now
                     Level:=TLevel(Plugin.SokoFile.Levels.First);
                     while Assigned(Level) do with Level do begin
                       NextLevel:=TLevel(Level.Next);
                       if      (Flags<StringGrid.FixedRows)
                               and
                               (StrEqual(Name,MainForm.Game.FileName)
                                or
                                (Level.BoardAsTextLines.First.Text=MainForm.Game.GetStartBoardAsText)
                               )
                               then with StringGrid do begin
                               ARow:=Pred(RowCount);
                               while (ARow>=FixedRows) and Assigned(Levels[ARow]) do Dec(ARow); // find a free row, if any
                               if ARow<FixedRows then begin // 'True': no free row was found; add a new row
                                  ARow:=RowCount; RowCount:=RowCount+1;
                                  end;
                               if (ARow>=FixedRows) and (ARow<RowCount) then begin
                                  Levels[ARow]:=Level; Level.Flags:=ARow; Inc(Count);
                                  end;
                               end
                       else if (Tag.Flags*[ltfSelected,ltfSelectedForFurtherProcessing,ltfLocked,ltfNew,ltfProcessed]=[])
                               and
                               (not StrEqual(Name,MainForm.Game.FileName)) then begin
                               if Level=Levels[Flags] then begin
                                  Selected[Flags]:=False;
                                  Levels[Flags]:=nil;
                                  end
                               else Plugin.SetSelected(Level,False);
                               Plugin.SokoFile.Levels.Remove(Level,True);
                               Inc(Count);
                               end;
                       Level:=NextLevel;
                       end;
                     end;
                  end
               else begin // 'Clear__' = 'False; check for empty rows in the string-grid
                  for ARow:=StringGrid.FixedRows to Pred(StringGrid.RowCount) do
                      if Count=0 then begin
                         if (not Assigned(Levels[ARow])) and
                            (ARow<Pred(StringGrid.RowCount)) then Inc(Count); // 'True': there is an empty row in the string-grid
                         end
                      else break;
                  end;

               if Count<>0 then // 'True': compact and refresh the string-grid
                  Refresh(True);

               if ((MainForm.Game.GameState<>gsNull) and
                   (MainForm.Game.FileName=ToolsForm.Editor.FileName) and
                   (MainForm.Game.GetStartBoardAsText<>'') // ensure that 'StartBoardAsText' has been calculated
                  )
                  or
                  (Self=ToolsForm.GeneratorTaskQueue)
                  then with MainForm.Game do begin

                  if Self=ToolsForm.OptimizerTaskQueue then begin
                     // add any solutions for the current level that aren't already on the list

                     LevelBestMoves:=nil; LevelBestPushes:=nil;

                     // add best solutions, if any, that aren't already on the queue
                     if MainForm.ShowSolutionMoves and
                        Assigned(BestSolutionMoves) then begin
                        if   Assigned(BestSolutionPushes) and Plugin.SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly then
                             Add(BoardWidth,BoardHeight,[ltfBest],BestMatchOptimization(SOKOBAN_PLUGIN_FLAG_MOVES) ,FileName,StartBoardAsText,BestSolutionMoves ,True ,LevelBestMoves )
                        else Add(BoardWidth,BoardHeight,[ltfBest],SOKOBAN_PLUGIN_FLAG_NONE                         ,FileName,StartBoardAsText,BestSolutionMoves ,True ,LevelBestMoves );
                        end;
                     if Assigned(BestSolutionPushes) then begin
                        if   Assigned(BestSolutionMoves) and Plugin.SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly then
                             Add(BoardWidth,BoardHeight,[ltfBest],BestMatchOptimization(SOKOBAN_PLUGIN_FLAG_PUSHES),FileName,StartBoardAsText,BestSolutionPushes,True ,LevelBestPushes)
                        else Add(BoardWidth,BoardHeight,[ltfBest],SOKOBAN_PLUGIN_FLAG_NONE                         ,FileName,StartBoardAsText,BestSolutionPushes,True ,LevelBestPushes);
                        end;
                     if (not MainForm.ShowSolutionMoves) and
                        Assigned(BestSolutionMoves) then begin
                        if   Assigned(BestSolutionPushes) and Plugin.SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly then
                             Add(BoardWidth,BoardHeight,[ltfBest],BestMatchOptimization(SOKOBAN_PLUGIN_FLAG_MOVES) ,FileName,StartBoardAsText,BestSolutionMoves ,True ,LevelBestMoves )
                        else Add(BoardWidth,BoardHeight,[ltfBest],SOKOBAN_PLUGIN_FLAG_NONE                         ,FileName,StartBoardAsText,BestSolutionMoves ,True ,LevelBestMoves );
                        end;

                     // add any non-optimal solutions for the current game that aren't already on the queue
                     Snapshot:=TSnapshot(Snapshots.First);
                     while Assigned(Snapshot) do with Snapshot do begin
                        if   (GameState=gsSolved) and (MoveCount=MoveTop) and (not ReverseMode) then begin

                             SnapshotFlags:=SOKOBAN_PLUGIN_FLAG_NONE;
                             if Assigned(BestSolutionMoves) and
                                Assigned(BestSolutionPushes) and                                     // 'True': the level has both a 'best-moves' solution and a 'best-pushes' solution; use them to select a best-fit optimization for snapshots with the same number of moves or pushes
                                Plugin.SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly then begin // 'True': don't initialize optimization to "Settings" for all solutions
                                if MainForm.ShowSolutionMoves and                                    // 'True': the user prefers 'best-move' solutions to 'best-push' solutions
                                   (Snapshot.MoveCount=BestSolutionMoves.MoveCount) then             // 'True': same number of moves  as the 'best moves'  solution
                                   SnapshotFlags:=BestMatchOptimization(SOKOBAN_PLUGIN_FLAG_MOVES);
                                if (Snapshot.PushCount=BestSolutionPushes.PushCount) and             // 'True': same number of pushes as the 'best pushes' solution
                                   (SnapshotFlags=SOKOBAN_PLUGIN_FLAG_NONE) then
                                   SnapshotFlags:=BestMatchOptimization(SOKOBAN_PLUGIN_FLAG_PUSHES);
                                if (Snapshot.MoveCount=BestSolutionMoves.MoveCount) and              // 'True': same number of moves  as the 'best moves'  solution
                                   (SnapshotFlags=SOKOBAN_PLUGIN_FLAG_NONE) then
                                   SnapshotFlags:=BestMatchOptimization(SOKOBAN_PLUGIN_FLAG_MOVES);
                                end;

                             Add(BoardWidth,BoardHeight,[]       ,SnapshotFlags                                    ,FileName,StartBoardAsText,Snapshot          ,False,Level);
                             end;
                        Snapshot:=TSnapshot(Snapshot.Next);
                        end;

                     // if the current position is a solution then add it to the task queue as well;
                     // that way, the user doesn't need explicitly to save a snapshot of a newly
                     // discovered solution before it can be optimized
                     if (GameState=gsSolved) and
                        (not ReverseMode) and
                        (History.Count>ForcedInitialJumps) and
                        (not IsEqualToCurrentGame(BestSolutionMoves )) and
                        (not IsEqualToCurrentGame(BestSolutionPushes)) then begin
                        CalculateSecondaryScoreMetrics(SecondaryScoreMetrics);
                        try    Snapshot:=MakeSnapshot(SnapshotsForm.SolutionName+SPACE+
                                                      Format(FORMAT_MOVES_AND_PUSHES,
                                                      [History.Count,History.PushCount])+
                                                      SecondaryMetricsFormattedAsATitleSuffix(
                                                        SecondaryMetricsInTitles,
                                                        SecondaryScoreMetrics));
                               try     Add(BoardWidth,BoardHeight,[],0,FileName,StartBoardAsText,Snapshot,False,Level);
                               finally Snapshot.Free;
                               end;
                        except on E:Exception do Error(E.Message,Application.Title);
                        end;
                        end;

                     // check if the current level is represented in the string-grid
                     Count:=0; NoSolutionsLevel:=nil;
                     for ARow:=Pred(StringGrid.RowCount) downto StringGrid.FixedRows do begin
                         Level:=Levels[ARow];
                         if Assigned(Level) and
                            (not Level.BoardAsTextLines.IsEmpty) and
                            (not Level.SnapshotsAsText.IsEmpty) and
                            (not TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.IsEmpty) then begin
                            if TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.First.Text='' then
                               NoSolutionsLevel:=Level;
                            if (BoardWidth=Level.Tag.BoardWidth) and
                               (BoardHeight=Level.Tag.BoardHeight) and
                               (StartBoardAsText=Level.BoardAsTextLines.First.Text) then begin
                               Inc(Count);

                               // check if this solution is a dethroned best solution, in which case its name is changed from 'Best Solution' to 'Solution'
                               if (ltfBest in Level.Tag.Flags)
                                  and
                                  (Assigned(LevelBestMoves) or Assigned(LevelBestPushes)) // '...or...' at least one new best solution was added to the queue
                                  and
                                  (Level<>LevelBestMoves)
                                  and
                                  (Level<>LevelBestPushes)
                                  and
                                  (
                                   (not Assigned(BestSolutionMoves))
                                   or
                                   (TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.First.Text<>BestSolutionMoves .GetMovesAsText)
                                  )
                                  and
                                  ((not Assigned(BestSolutionPushes))
                                   or
                                   (TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.First.Text<>BestSolutionPushes.GetMovesAsText)
                                  )
                                  and
                                  Assigned(SnapshotsForm) then
                                  with TExtendedSnapshotAsText(Level.SnapshotsAsText.Last) do
                                    try    // change 'Best Solution' to 'Solution';
                                           // it may not be necessary, but it's an easy
                                           // way to reduce the potential for confusion
                                           SetName(SnapshotsForm.DethronedSolutionName(Name));
                                           Exclude(Level.Tag.Flags,ltfBest);
                                           RefreshRow(Level.Flags);
                                    except on E:Exception do begin end;
                                    end;

                               if not StrEqual(Level.Name,MainForm.Game.FileName) then begin
                                  Level.SetName(MainForm.Game.FileName); // attach this task (i.e., row) to the current level instead of its original level
                                  RefreshRow(Level.Flags);
                                  end;
                               end;
                            end;
                         end;
                     if Count<>0 then begin
                        if Assigned(NoSolutionsLevel) then
                           if   (Count=1) and (StartBoardAsText=NoSolutionsLevel.BoardAsTextLines.First.Text) then begin
                                // the level is already represented by a 'no solutions' item in the string-grid
                                end
                           else Plugin.DeleteLevel(NoSolutionsLevel); // the level is represented by solutions on the list, hence, the 'no solutions' entry is obsolete now
                        end
                     else begin
                        // there are no solutions in the string-grid for this level;
                        // add a dummy item to the task queue representing this level;
                        // otherwise it's confusing for the user to browse through
                        // the levels because the current level wouldn't show up on the list

                        if Assigned(NoSolutionsLevel) then Plugin.DeleteLevel(NoSolutionsLevel); // in weird situations (e.g., when 'MainGame.Game.GameState=gsNull') there may already be a 'no solutions' item in the string-grid; delete it now

                        if Plugin.LoadLevel(BoardWidth,BoardHeight,-1,0, FileName,StartBoardAsText,nil,True,False,Level) then with Level do
                           try
                                  if   CreateObject(otExtendedSnapshotAsText,TNode(NewSnapshot)) then begin
                                       Level.SnapshotsAsText.Add(NewSnapshot);
                                       if NewSnapshot.SetText(StrWithParenthesis(NoSolutionsText))
                                          and
                                          NewSnapshot.MovesAsTextLines.AddBlankLine then with StringGrid do begin
                                          // the dummy task is now on the queue but not in the string-grid; add it to the string-grid, i.e., update the screen
                                          ARow:=Pred(RowCount); // search backwards through the rows to find the last occupied row
                                          while (ARow>=FixedRows) and (not Assigned(Levels[ARow])) do Dec(ARow);
                                          Inc(ARow);
                                          if (ARow=RowCount) and (RowCount<High(RowCount)) then RowCount:=Succ(RowCount);
                                          if ARow<RowCount then begin
                                             Level.Flags:=ARow; // 'Level.Flags' links back from the level to its row-number in the string-grid
                                             Levels[ARow]:=Level;
                                             Selected[ARow]:=False;
                                             RefreshRow(ARow);
                                             Count:=1;
                                             end;
                                          end;
                                       end;
                                  if Count=0 then raise Exception.Create(TEXT_TASK_FAILED);
                           except on E:Exception do begin
                                  Plugin.DeleteLevel(Level);
                                  end;
                           end;
                        end;

                     with StringGrid do // delete a 'no solutions' dummy-item left over from earlier in case it hasn't been reused
                       if (RowCount>FixedRows) and (not Assigned(Levels[Pred(RowCount)])) then
                          try     Self.RowCount:=RowCount-1; // this works because 'SetRowCount' ensures that there always are at least ('StringGrid.FixedRows' + 1) rows
                          finally RefreshRow(Pred(RowCount));
                          end;

                     end
                  else if Self=ToolsForm.SolverTaskQueue then begin
                          // if the level isn't a member of the list, then add it now

                          // check if the current level is represented in the string-grid
                          Count:=0; AddToolsFormLevelVersion:=False;

                          if ToolsForm.Modified or MainForm.Modified then with ToolsForm.PluginLevelInfo do
                             if IsALegalLevel  and
                                StrEqual(LevelName,ToolsForm.Editor.FileName) then begin
                                for ARow:=Pred(StringGrid.RowCount) downto StringGrid.FixedRows do
                                    if Count=0 then begin
                                       Level:=Levels[ARow];
                                       if Assigned(Level) and
                                          (BoardAsText.Width=Level.Tag.BoardWidth) and
                                          (BoardAsText.Height=Level.Tag.BoardHeight) and
                                          (BoardAsText.Board=Level.BoardAsTextLines.First.Text) then begin
                                          Inc(Count);
                                          if not StrEqual(Level.Name,LevelName) then begin
                                             Level.SetName(LevelName); // attach this task (i.e., row) to the current level instead of its original level
                                             RefreshRow(Level.Flags);
                                             end;
                                          end
                                       end
                                    else break;
                                if Count=0 then begin // '0': the 'Tools' window version of the level isn't represented on the task queue
                                   AddToolsFormLevelVersion:=True;
                                   Count:=-1; // '-1': don't search through the task queue for the level version matching the 'MainForm.Game' version
                                   end;
                                end;

                          for ARow:=Pred(StringGrid.RowCount) downto StringGrid.FixedRows do
                              if Count=0 then begin
                                 Level:=Levels[ARow];
                                 if Assigned(Level) then begin
                                    if (BoardWidth=Level.Tag.BoardWidth) and
                                       (BoardHeight=Level.Tag.BoardHeight) and
                                       (StartBoardAsText=Level.BoardAsTextLines.First.Text) then begin
                                       Inc(Count);
                                       if not StrEqual(Level.Name,MainForm.Game.FileName) then begin
                                          Level.SetName(MainForm.Game.FileName); // attach this task (i.e., row) to the current level instead of its original level
                                          RefreshRow(Level.Flags);
                                          end;
                                       end;
                                    end;
                                 end
                              else break;

                          if Count<=0 then begin // '<=0': the level isn't a member of the list; add it now

                             if  ((AddToolsFormLevelVersion
                                  and
                                  Plugin.LoadLevel(ToolsForm.PluginLevelInfo.BoardAsText.Width,
                                                   ToolsForm.PluginLevelInfo.BoardAsText.Height,
                                                   -1,0,
                                                   ToolsForm.PluginLevelInfo.LevelName,
                                                   ToolsForm.PluginLevelInfo.BoardAsText.Board,
                                                   nil,True,False,Level)
                                 )
                                 or
                                 ((not AddToolsFormLevelVersion)
                                  and
                                  Plugin.LoadLevel(MainForm.Game.BoardWidth,
                                                   MainForm.Game.BoardHeight,
                                                   -1,0,
                                                   MainForm.Game.FileName,
                                                   MainForm.Game.StartBoardAsText,
                                                   nil,True,False,Level)
                                 )
                                )
                                and
                                (Level.Flags<Self.StringGrid.FixedRows) then
                                with Self do with StringGrid do begin
                                  ARow:=Pred(RowCount);
                                  while (ARow>=FixedRows) and Assigned(Levels[ARow]) do Dec(ARow); // find a free row, if any
                                  if   ARow<FixedRows then begin
                                       ARow:=RowCount; Self.RowCount:=Succ(RowCount);
                                       end;
                                  if   (ARow>=FixedRows) and (ARow<RowCount) then begin
                                       Levels[ARow]:=Level; Selected[ARow]:=True;
                                       RefreshRow(ARow);
                                       end;
                                  end;
                             end;
                          end
                  else if Self=ToolsForm.GeneratorTaskQueue then begin
                          try

                                 // ensure that the grid has the exact number of rows to hold the visible candidates
                                 Count:=0;
                                 if Plugin.HasSokoFile then begin
                                    Level:=TLevel(Plugin.SokoFile.Levels.First);
                                    while Assigned(Level) do begin
                                      if (Level.Flags>=StringGrid.FixedRows) and (Level.Flags<StringGrid.RowCount) then
                                         Inc(Count);
                                      Level:=TLevel(Level.Next);
                                      end;
                                    end;
                                 Count:=Max(1,Count); // '1': the grid always contains at least one row for items + the fixed rows
                                 if  RowCount<>StringGrid.FixedRows+Count then
                                     RowCount:=StringGrid.FixedRows+Count;

                                 // refresh all cells in the grid, i.e., reload all the candidates
                                 Refresh(True);

                          except on E:Exception do begin
                                    Error(E.Message,Application.Title);
                                    Refresh(True);
                                    end;
                          end;

                          end;
                  end;

               with StringGrid do
                 if (RowCount>Succ(FixedRows)) and (not Assigned(Levels[Pred(RowCount)])) then
                    Self.Refresh(True); // compact the grid in case it contains one or  more trailing blank rows

               ToolsForm.MakeAllColumnsFullyVisible;

               ToolsForm.ShowStatus;
       finally Plugin.Resume;
       end;
     finally Plugin.Leave;
     end;
     end;
end;

procedure TTaskQueue.Refresh(Compact__:Boolean);
var ARow,OldRowCount:Integer;

  procedure Compact;
  var ARow,NextRow,Count:Integer; Level:TLevel; CheckBox:TCheckBox;
  begin // compact the grid rows so there are no interior empty rows (there may be one trailing empty row)
    Count:=0; NextRow:=0;
    for ARow:=StringGrid.FixedRows to Pred(StringGrid.RowCount) do begin
        Level:=Levels[ARow];
        if (not Assigned(Level))
           or
           ((Self=ToolsForm.OptimizerTaskQueue)
            and
            Level.SnapshotsAsText.IsEmpty
           ) then begin
           if Assigned(Level) then begin // 'True': the level hasn't any snapshots; delete the malformed level
              Selected[ARow]:=False;
              Levels[ARow]:=nil;
              Plugin.SokoFile.Levels.Remove(Level,True);
              end;
           NextRow:=Max(Succ(ARow),NextRow);
           while (NextRow<RowCount) and (not Assigned(Levels[NextRow])) do Inc(NextRow);
           if NextRow<RowCount then SwapRows(ARow,NextRow,False);
           end;
        Level:=Levels[ARow];
        if      Assigned(Level) then begin
                Inc(Count); Level.Flags:=ARow;
                end
        else if False and (ARow=Pred(StringGrid.RowCount)) and (Self=ToolsForm.GeneratorTaskQueue) and Plugin.IsActive then
                Inc(Count); // allow one empty row in the string grid for the generator task queue
        end;
    if Count+StringGrid.FixedRows<RowCount then
       try    for ARow:=0 to Pred(RowCount) do
                  if HasCheckBoxInRow(ARow,CheckBox) then CheckBox.Hide; // order important: if the string-grid shrinks then all the checkboxes must be hidden first
              RowCount:=Max(1,Count)+StringGrid.FixedRows;
       except on E:Exception do Error(E.Message,Application.Title);
       end;
  end;

begin // Refresh
  if Assigned(Plugin) then begin
     Plugin.Enter;
     try     ToolsForm.CloseEditors;
             OldRowCount:=StringGrid.RowCount;
             if Compact__ then Compact;
             with StringGrid do
               if   RowCount=OldRowCount then
                    for ARow:=TopRow    to TopRow+VisibleRowCount do RefreshRow(ARow)
               else for ARow:=FixedRows to Pred(RowCount)         do RefreshRow(ARow);
     finally Plugin.Leave;
     end;
     end;
end;

procedure TTaskQueue.RefreshRow(Row__:Integer);
var ACol,i:Integer; b:Boolean; s:String; Level:TLevel; CheckBox:TCheckBox;
begin
  if Assigned(Plugin) then Plugin.Enter;
  try     ToolsForm.PanelToolTips.Hide;
          Level:=Levels[Row__];
          if Assigned(Level) then with StringGrid do with Level do begin
             Flags:=Row__; // 'Level.Flags' links back from the level to its row-number in the string-grid
             Cells[Ord(oscSelect),Row__]:='';
             Cells[Ord(oscNo),Row__]:=IntToStr(Succ(Row__)-FixedRows)+SPACE;
             if        IsAnIniFileSectionFileName(Name) then s:=ExtractSectionName(Name)
             else if   Self<>ToolsForm.GeneratorTaskQueue then
                       s:=ExtractFileNameWithoutPathAndExtension(Name)
                  else s:=Name;
             Cells[Ord(oscLevelName),Row__]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+StrWithQuotedAmpersands(s);

             if Assigned(Plugin) then begin
                if      Plugin is TOptimizerPlugin then begin
                        if (not SnapshotsAsText.IsEmpty) then begin
                           Cells[Ord(oscSnapShotName),Row__]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+SnapshotsAsText.Last.Name;
                           with TExtendedSnapshotAsText(SnapshotsAsText.Last) do with Metrics do begin
                                if MovesAsTextLines.First.Text<>'' then begin
                                   Cells[Ord(oscMetrics)     ,Row__]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+MetricsAsText(MoveCount,PushCount,SecondaryMetrics)+ImprovementAsText;
                                   if    (ltfLocked in Level.Tag.Flags) and // locked: the level currently being optimized
                                         (not (ltfSelected in Level.Tag.Flags)) and // selected: mostly just to be sure, but maybe there are some scenarios where the level is selected for further processing
                                         (OptimizationFlags<>0) then // 'OptimizationFlags': the actually used optimization mode by the optimizer, as opposed to 'Tag' which contains the optimization mode requested by the user
                                        i:= OptimizationFlags // actually used optimization mode for the current task
                                   else i:=Tag; // optimization mode requested by the user, possibly the generic value "Settings", which leaves the decision to the optimizer plugin
                                   Cells[Ord(oscOptimization),Row__]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+OptimizationFlagsAsText(i);
                                   end
                                else begin
                                   Cells[Ord(oscMetrics)     ,Row__]:='';
                                   Cells[Ord(oscOptimization),Row__]:='';
                                   end;
                                end;
                           end
                        else begin
                           Cells[Ord(oscSnapShotName),Row__]:='';
                           Cells[Ord(oscMetrics),Row__]:='';
                           Cells[Ord(oscOptimization),Row__]:=SettingsText;
                          end;
                        ToolsForm.ShowOptimizationRange(-1);
                        end
                else if Plugin is TSolverPlugin then begin
                        if SnapshotsAsText.IsEmpty then begin
                           if   ltfUnsolvable in Level.Tag.Flags then
                                Cells[Ord(slcMetrics),Row__]:=UnsolvableOrInvalidLevelAccordingToPluginText
                           else Cells[Ord(slcMetrics),Row__]:='';
                           end
                        else with TExtendedSnapshotAsText(SnapshotsAsText.First).Metrics do
                           //Cells[Ord(slcSnapshotName),Row__]:=OPTIMIZER_TASK_QUEUE_LEFT_MARGIN_CHARACTERS+Name;
                           Cells[Ord(slcMetrics),Row__]:={TASK_QUEUE_LEFT_MARGIN_CHARACTERS+}Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+TASK_QUEUE_RIGHT_MARGIN_CHARACTERS
                        end
                else if Plugin is TGenerator then begin
                        if not SnapshotsAsText.IsEmpty then with TExtendedSnapshotAsText(SnapshotsAsText.First) do with Metrics do begin
                           if   PushCount>0 then
                                Cells[Ord(glcPushes  ),Row__]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+IntToStr(PushCount)+SPACE
                           else Cells[Ord(glcPushes  ),Row__]:='';
//                         if   SecondaryMetrics.BoxChanges+SecondaryMetrics.BoxLines>0 then
//                              Cells[Ord(glcFitness ),Row__]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+IntToStr(SecondaryMetrics.BoxChanges+SecondaryMetrics.BoxLines)+SPACE
//                         else Cells[Ord(glcFitness ),Row__]:='';
                           if   SecondaryMetrics.PlayerLines>0 then // the fitness is stored in 'PlayerLines'
                                Cells[Ord(glcFitness ),Row__]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+IntToStr(SecondaryMetrics.PlayerLines)+SPACE
                           else Cells[Ord(glcFitness ),Row__]:='';
                           if   True {TimeOfBirthMS<>0} then
                                Cells[Ord(glcBirth   ),Row__]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+FormatTimeMS(TimeOfBirthMS)+SPACE
                           else Cells[Ord(glcBirth   ),Row__]:='';
                           if   (Tag<>0) or (PushCount>0) then
                                Cells[Ord(glcChildren),Row__]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+IntToStr(Tag)+SPACE
                           else Cells[Ord(glcChildren),Row__]:='';
                           if   (TimeMS>=500) or (PushCount>0) then
                                Cells[Ord(glcTime    ),Row__]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+FormatTimeMS(TimeMS)+SPACE
                           else Cells[Ord(glcTime    ),Row__]:='';
                           end
                        else begin
                           Cells[Ord(glcPushes  ),Row__]:='';
                           Cells[Ord(glcFitness ),Row__]:='';
                           Cells[Ord(glcBirth   ),Row__]:='';
                           Cells[Ord(glcChildren),Row__]:='';
                           end;
                        end;
                end
             else begin
                end;
             end
          else with StringGrid do // no level assigned to this row
             if (Row__>=FixedRows) and (Row__<RowCount) then begin
                for ACol:=0 to Pred(ColCount) do begin
                    Cells[ACol,Row__]:='';
                    if ACol<>Ord(oscNo) then Objects[ACol,Row__]:=nil; // checkboxes are stored in 'Objects[Ord(oscNo),Row]'
                    end;
                if Row__=Row then
                   if        (Plugin is TOptimizerPlugin) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer) then
                             ToolsForm.OptimizeSolutionsStringGridSelectCell(nil,StringGrid.Col,StringGrid.Row,b)
                   else if   (Plugin is TSolverPlugin) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver) then
                             ToolsForm.SolveLevelsStringGridSelectCell      (nil,StringGrid.Col,StringGrid.Row,b)
                   else if   (Plugin is TGenerator) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator) then
                             ToolsForm.GenerateLevelsStringGridSelectCell   (nil,StringGrid.Col,StringGrid.Row,b);

                end;
          if GetCheckBoxForRow(Row__,CheckBox) then begin
             CheckBox.Checked:=Selected[Row__];
             end;
  finally if Assigned(Plugin) then Plugin.Leave;
  end;
end;

function  TTaskQueue.ResurrectLevel(Level__:TLevel):Boolean;
var ARow:Integer;
begin // resurrects the level (the task) if it isn't visible in the string-grid
  Result:=False;
  if Assigned(Plugin) then Plugin.Enter;
  try     if Assigned(Level__) then
             if      (Level__.Flags>=StringGrid.FixedRows) and (Level__.Flags<StringGrid.RowCount) then // 'True': the level is already visible in the string-grid
                     Result:=True
             else if MakeRow(ARow) then begin
                     Level__.Flags:=ARow; // 'Level.Flags' links back from the level to its row-number in the string-grid
                     Levels[ARow]:=Level__;
                     RefreshRow(ARow);
                     ToolsForm.MakeAllColumnsFullyVisible;
                     StringGrid.Objects[Ord(oscSnapshotName),ARow]:=Pointer(GetNextRunNo);
                     Result:=True;
                     end;
  finally if Assigned(Plugin) then Plugin.Leave;
  end;
end;

function  TTaskQueue.SaveToFile( const FileName__ : String ) : Boolean;
var ARow:Integer;
    SelectedSquares:String;
    Level, LevelCopy:TLevel;
    Snapshot, SnapshotCopy : TExtendedSnapshotAsText;
    SokoFile : TSokoFile;
    Board : TBoard;
begin
  Result := Assigned( Plugin );
  if Result then begin
     Result := CreateObject( otSokoFile, TNode( SokoFile ) );
     if Result then
        try
           Plugin.Enter;
           try     for ARow:=0 to Pred(StringGrid.RowCount) do
                       if Result then begin
                          Level:=Levels[ARow];
                          if Assigned(Level) and ( not Level.BoardAsTextLines.IsEmpty ) then begin
                             Result := CreateObject( otLevel, TNode( LevelCopy ) );
                             if Result then begin
                                SokoFile.Levels.Add( TNode( LevelCopy ) );
                                Result := LevelCopy.SetName( AbbreviatedFilePath( Level.Name, MainForm.MyDocumentsFolder ) ) and
                                          TextToBoard( Level.Tag.BoardWidth, Level.Tag.BoardHeight, Level.BoardAsTextLines.First.Text, Board ) and
                                          LevelCopy.BoardToTextLines( Level.Tag.BoardWidth, Level.Tag.BoardHeight, Board ) and
                                          LevelCopy.Notes.Lines.WriteString( KEY_TITLE, LevelCopy.Name );

                                if Result then
                                   if      Self = ToolsForm.SolverTaskQueue then begin
                                           Result := LevelCopy.Notes.Lines.WriteString( KEY_SELECTED, BooleanText[ Selected[ ARow ] ] );
                                           end
                                   else if Self = ToolsForm.OptimizerTaskQueue then begin
                                           if Level.SnapshotsAsText.IsEmpty or
                                              ( not ( Level.SnapshotsAsText.Last is TExtendedSnapshotAsText ) ) then
                                              SokoFile.Levels.Remove( LevelCopy, True )
                                           else begin
                                              Snapshot := TExtendedSnapshotAsText( Level.SnapshotsAsText.Last );
                                              if not Snapshot.Notes.Lines.ReadString(KEY_SELECTED_SQUARES,SelectedSquares) then
                                                 SelectedSquares:='';
                                              Result := CreateObject( otExtendedSnapshotAsText, TNode( SnapshotCopy ) );
                                              if Result then begin
                                                 LevelCopy.SnapshotsAsText.Add( SnapshotCopy );
                                                 Result := SnapshotCopy.SetName( SnapshotsForm.DethronedSolutionName(Snapshot.Name ) ) and
                                                           Snapshot.MovesAsTextLines.CopyTo( SnapshotCopy.MovesAsTextLines ) and
                                                           SnapshotCopy.Notes.Lines.WriteString( KEY_SELECTED, BooleanText[ Selected[ ARow ] ] ) and
                                                           SnapshotCopy.Notes.Lines.WriteString( KEY_SETTINGS, OptimizationFlagsAsText(Snapshot.Tag ) ) and
                                                           ((Snapshot.SelectedRange[0]>=Snapshot.SelectedRange[1]) // 'True': no selected range of moves
                                                            or
                                                            (SnapshotCopy.Notes.Lines.WriteString( KEY_BEGIN   ,IntToStr(Snapshot.SelectedRange[0]))
                                                             and
                                                             SnapshotCopy.Notes.Lines.WriteString( KEY_END     ,IntToStr(Snapshot.SelectedRange[1])))) and
                                                           ((Snapshot.SelectedRange[6]<=0 ) or
                                                            SnapshotCopy.Notes.Lines.WriteString ( KEY_INTERVAL,IntToStr(Snapshot.SelectedRange[6])))  and
                                                           (( SelectedSquares = '' ) or
                                                            SnapshotCopy.Notes.Lines.WriteString ( KEY_SELECTED_SQUARES,SelectedSquares ) );
                                                 end;
                                              end;
                                           end;
                                end;
                             end;
                          end;
                   if  Result then begin
                       Result := SokoFile.SaveToFile( FileName__ );
                       end;
           finally Plugin.Leave;
           end;
        finally try     if Result and FileExists( SokoFile.Name ) and Assigned( OpenForm ) then begin
                           AddItemOrMoveItemToTopOfComboBox( OpenForm.LevelsFileHistoryComboBox  ,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(SokoFile.Name,MainForm.MyDocumentsFolder),True);
                           AddItemOrMoveItemToTopOfComboBox( OpenForm.LevelsFolderHistoryComboBox,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(ExtractFilePath(SokoFile.Name)),MainForm.MyDocumentsFolder),True);
                           end;
                finally SokoFile.Free;
                end;
        end;
     end;
   if not Result then
      Error( TEXT_TASK_FAILED, TEXT_SAVE_FILE );
end;

procedure TTaskQueue.ScrollInView(Row__:Integer);
var Level:TLevel; CheckBox:TCheckBox;
begin
  if Assigned(ToolsForm)
     and
     Assigned(Plugin)
     and
     (((Self=ToolsForm.OptimizerTaskQueue) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer))
      or
      ((Self=ToolsForm.SolverTaskQueue   ) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver   ))
      or
      ((Self=ToolsForm.GeneratorTaskQueue) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator))
     ) then with StringGrid do begin
     Plugin.Enter;
     try     ToolsForm.PanelToolTips.Hide;
             GridScrollInView(StringGrid,Row__);
             for Row__:=0 to Pred(RowCount) do
                 if   HasCheckBoxInRow(Row__,CheckBox) then CheckBox.Tag:=-1
                 else break;
             for Row__:=TopRow to TopRow+VisibleRowCount do
                 if GetCheckBoxForRow(Row__,CheckBox) then begin
                    CheckBox.Checked:=Selected[Row__];
                    Level:=Levels[Row__];
                    if (Row__<RowCount)
                       and
                       (not (Assigned(Level)
                             and
                             ((ltfLocked in Level.Tag.Flags)
                              or
                              ((not Level.SnapshotsAsText.IsEmpty)
                               and
                               ((Self<>ToolsForm.GeneratorTaskQueue)
                                and
                                (TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.First.Text='')
                               )
                              )
                             )
                            )
                       ) then begin
                       CheckBox.Show;
                       end;
                    end;
             for Row__:=0 to Pred(RowCount) do
                 if   HasCheckBoxInRow(Row__,CheckBox) then
                      if CheckBox.Tag=-1 then CheckBox.Hide
                      else
                 else break;
     finally Plugin.Leave;
     end;
     end;
end;

procedure TTaskQueue.SetHighlightedRowNumber(Row__:Integer);
var oHighlightedRowNumber:Integer;
begin
  with StringGrid do begin
    if (Row__<>HighlightedRowNumber) then begin
       oHighlightedRowNumber:=HighlightedRowNumber;
       fHighlightedRowNumber:=Row__;
       RefreshRow(oHighlightedRowNumber);
       end;
    RefreshRow(HighlightedRowNumber); // always refresh the highlighted row; the checkbox may need an update even if the rest of the row hasn't changed
    end;
end;

procedure TTaskQueue.SetLevels(Row__:Integer; Level__:TLevel);
begin
  with StringGrid do
    if   (Row__>=FixedRows) and (Row__<RowCount) then begin
         if Assigned(Plugin) then Plugin.Enter;
         try     if (not Assigned(Level__)) and Selected[Row__] then
                    Selected[Row__]:=False;
                 Objects[Ord(oscLevelName),Row__]:=Level__;
         finally if Assigned(Plugin) then Plugin.Leave;
         end;
         end
    else raise Exception.Create(TEXT_RANGE_ERROR+': Tools_.TTaskQueue.SetLevels');
end;

procedure  TTaskQueue.SetOptimizationFlags(Row__,Value__:Integer);
begin
  with StringGrid do
    if   (Row__>=FixedRows) and (Row__<RowCount) then begin
         Plugin.Enter;
         try     if Assigned(Levels[Row__]) and
                    (not Levels[Row__].SnapshotsAsText.IsEmpty) and
                    (Levels[Row__].SnapshotsAsText.Last is TExtendedSnapshotAsText) then
                    with TExtendedSnapshotAsText(Levels[Row__].SnapshotsAsText.Last) do begin
                      Tag:=Value__; // the selected flags
                      OptimizationFlags:=0; // '0': the field is first filled in when the optimization starts
                      end;
         finally Plugin.Leave;
         end;
         end
    else raise Exception.Create(TEXT_RANGE_ERROR+': Tools_.TTaskQueue.SetOptimizationFlags');
end;

procedure TTaskQueue.SetRowCount(RowCount__:Integer);
var ACol,ARow:Integer; Level:TLevel;
begin
  if Assigned(Plugin) then Plugin.Enter;
  try
    with StringGrid do
      if RowCount__<High(RowCount)-Screen.DeskTopHeight then // 'VisibleRowCount' is probably always less than 'Screen.DeskTopHeight'; at least, this will have to do as guard
         try    for ARow:=Max(RowCount__,FixedRows) to Pred(RowCount) do begin
                    Level:=Levels[ARow]; Levels[ARow]:=nil; // 'Levels[ARow]' must be cleared before setting 'Selected[ARow]:=False'
                    Selected[ARow]:=False;
                    DestroyCheckBox(ARow);
                    if Assigned(Level) then
                       if   ((Level.Tag.Flags*[ltfLocked,ltfNew,ltfProcessed])=[]) and
                            Assigned(Plugin) then begin
                            Plugin.SetSelected(Level,False);
                            Plugin.SokoFile.Levels.Remove(Level,True)
                            end
                       else Level.Flags:=-1;
                    end;
                if RowCount__<Succ(FixedRows) then begin
                   RowCount__:=Succ(FixedRows); // there must always be at least one detail line in a string-grid
                   for ACol:=0 to Pred(ColCount) do begin
                       Cells  [ACol,Pred(RowCount__)]:='';
                       Objects[ACol,Pred(RowCount)]:=nil;
                       end;
                   end;
                if RowCount<>RowCount__ then begin
                   if Dragging then EndDrag(False);
                   RowCount:=RowCount__;
                   end;
         except on E:Exception do begin
                   Error(E.Message,Application.Title);
                   end;
         end;
  finally if Assigned(Plugin) then Plugin.Leave;
  end;
end;

procedure TTaskQueue.SetSelected(Row__:Integer; Value__:Boolean);
var CheckBox:TCheckBox; Level:TLevel;
begin
  with StringGrid do
    if (Row__>=FixedRows) and (Row__<RowCount) then begin
       if Assigned(Plugin) then Plugin.Enter;
       try     Level:=Levels[Row__];
               Value__:=Value__ and Assigned(Level) and
                        (not (ltfLocked in Level.Tag.Flags)) and
                        (Level.SnapshotsAsText.IsEmpty
                         or
                         (TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.First.Text<>'')
                         or
                         (Self=ToolsForm.GeneratorTaskQueue)
                        ) ;
               if Selected[Row__]<>Value__ then with ToolsForm do begin
                  if   Value__ then begin
                       Inc(fSelectedCount);
                       if (Self=ToolsForm.OptimizerTaskQueue) and
                          (not Level.SnapshotsAsText.IsEmpty) and
                          (Level.SnapshotsAsText.Last is TExtendedSnapshotAsText) then
                          with TExtendedSnapshotAsText(Level.SnapshotsAsText.Last) do begin
                            OptimizationFlags:=0; // the field is first filled in when the optimization starts
                            end;
                       end
                  else Dec(fSelectedCount);
                  if SelectedCount<>0 then begin
                     if not ItemsCheckBox.Checked then
                        ItemsCheckBox.Checked:=True;
                     Cells[Ord(oscNo),0]:=IntToStr(SelectedCount)+SPACE;
                     if not ItemsCheckBox.Enabled then
                            ItemsCheckBox.Enabled:=True;
                     end
                  else begin
                     if ItemsCheckBox.Checked then
                        ItemsCheckBox.Checked:=False;
                     Cells[Ord(oscNo),0]:='';
                     ItemsCheckBox.Enabled:=(RowCount>Succ(FixedRows))
                                            or
                                            ((RowCount=Succ(FixedRows))
                                             and
                                             Assigned(Levels[FixedRows])
                                             and
                                             (not (ltfLocked in Levels[FixedRows].Tag.Flags))
                                            );
                     end;
                  if Assigned(Plugin) then
                     if      (Plugin is TSolverPlugin) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver) then begin
                             ToolsForm.PluginEditMenuItemDeleteLevels.Enabled:=SelectedCount<>0;
                             ToolsForm.PluginToolButtonDelete.Enabled:=ToolsForm.PluginEditMenuItemDeleteLevels.Enabled;
                             end
                     else if (Plugin is TOptimizerPlugin) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer) then begin
                             ToolsForm.PluginEditMenuItemDeleteSolutions.Enabled:=SelectedCount<>0;
                             ToolsForm.PluginToolButtonDelete.Enabled:=ToolsForm.PluginEditMenuItemDeleteSolutions.Enabled;
                             PluginEditMenuItemResetOptimizationMethod.Enabled:=PluginEditMenuItemDeleteSolutions.Enabled;
                             TaskQueuePopupMenuItemResetOptimizationMethod.Enabled:=PluginEditMenuItemResetOptimizationMethod.Enabled;
                             end
                     else if (Plugin is TGenerator) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator) then begin
                             ToolsForm.PluginEditMenuItemDeleteLevels.Enabled:=SelectedCount<>0;
                             ToolsForm.PluginToolButtonDelete.Enabled:=ToolsForm.PluginEditMenuItemDeleteLevels.Enabled;
                             end;
                  end;
               Objects[Ord(oscSelect),Row__]:=TObject(Ord(Value__));
               if Assigned(Level) then begin
                  if   Value__ then begin
                       if   Assigned(Plugin) then
                            Plugin.SetSelected(Level,True)
                       else Include(Level.Tag.Flags,ltfSelected);
                       Exclude(Level.Tag.Flags,ltfProcessed);
                       end
                  else if   Assigned(Plugin) then
                            Plugin.SetSelected(Level,False)
                       else Exclude(Level.Tag.Flags,ltfSelected);
                  if   GetCheckBoxForRow(Row__,CheckBox) then CheckBox.Checked:=Value__;
                  end;
       finally if Assigned(Plugin) then Plugin.Leave;
       end;
       end;
end;

procedure TTaskQueue.SwapRows(Row1__,Row2__:Integer; Show__:Boolean);
var ACol:Integer; Tmp:TObject; Level:TLevel;
begin
  with StringGrid do begin
    Plugin.Enter;
    try
      if   (Row1__>=FixedRows) and (Row1__<RowCount) and
           (Row2__>=FixedRows) and (Row2__<RowCount) then begin
           for ACol:=0 to Pred(ColCount) do
               if ACol<>Ord(oscNo) then begin
                  Tmp:=Objects[ACol,Row1__];
                  Objects[ACol,Row1__]:=Objects[ACol,Row2__];
                  Objects[ACol,Row2__]:=Tmp;
               end;
           Level:=Levels[Row1__]; if Assigned(Level) then Level.Flags:=Row1__;  // update links back from the levels to the row numbers in the string-grid
           Level:=Levels[Row2__]; if Assigned(Level) then Level.Flags:=Row2__;
           if      HighlightedRowNumber=Row1__ then HighlightedRowNumber:=Row2__
           else if HighlightedRowNumber=Row2__ then HighlightedRowNumber:=Row1__;
           if      Show__ then begin
                   RefreshRow(Row1__); RefreshRow(Row2__);
                   end;
           end
      else raise Exception.Create('Tools_.TTaskQueue.SwapRows: '+InternalErrorText);
    finally Plugin.Leave;
    end;
    end;
end;

procedure TToolsForm.TaskQueuePopupMenuItemToggleSelectionAboveOrBelowItemClick(
  Sender: TObject);
var ARow:Integer; CurrentTaskQueue:TTaskQueue;
begin
  if      PageControl1.ActivePage=TabSheetSolver    then CurrentTaskQueue:=SolverTaskQueue
  else if PageControl1.ActivePage=TabSheetOptimizer then CurrentTaskQueue:=OptimizerTaskQueue
  else if PageControl1.ActivePage=TabSheetGenerator then CurrentTaskQueue:=GeneratorTaskQueue
  else CurrentTaskQueue:=nil;

  if   Assigned(CurrentTaskQueue) then with CurrentTaskQueue do
       if Assigned(Plugin) then with StringGrid do begin
          Plugin.Enter;
          try     if   (Sender=TaskQueuePopupMenuItemToggleSelectionAboveItem)
                       or
                       (Sender=GeneratorPopupMenuItemToggleSelectionAboveItem)
                       then begin
                       for ARow:=FixedRows to Pred(Row) do
                           if ARow<>HighlightedRowNumber then Selected[ARow]:=not Selected[ARow]
                       end
                  else for ARow:=Succ(Row) to Pred(RowCount) do
                           if ARow<>HighlightedRowNumber then Selected[ARow]:=not Selected[ARow];
                  OpenForm.EnableDisablePluginControls(Self,Plugin,False);
                  ToolsForm.ShowStatus;
          finally Plugin.Leave;
          end;
          end;
end;


procedure TToolsForm.BtnGeneratorSettingsClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and (not Game.IsReplaying) and
     CloseEditors and
     (PluginForCurrentTabSheet<>nil) then begin
     StatusText:='';
     try    try     //GeneratorForm.ShowModal;
                    PluginSettingsWindowOrAboutFactBox(Sender);
            finally SetMessageHandlers;
                    ShowStatus;
            end;
     except on E:Exception do begin
            Error(E.Message,Application.Title);
            ShowStatus;
            end;
     end;
     end;
end;

procedure TToolsForm.GeneratorMenuItemDeleteFilesClick(Sender: TObject);
var Result:Boolean;
begin //
  if (Screen.ActiveForm=Self) and
     OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors then with MPlayerForm.OpenDialog1 do begin
     Filter:=OpenForm.FilterComboBox1.Filter;
     FilterIndex:=DeleteFilesFilterIndex;
     Title:=Application.Title+SUB_TITLE_SEPARATOR+ToolsText+//SUB_TITLE_SEPARATOR+Trim(PageControl1.ActivePage.Caption)+
            SUB_TITLE_SEPARATOR+DeleteFilesText;
     InitialDir:=ExtractFilePath(MainForm.Game.FileName);
     if InitialDir='' then InitialDir:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath)+DEFAULT_LEVEL_DIRECTORY);

     Result:=DeleteFilesDialog(MPlayerForm.OpenDialog1,Self,StatusBar1.Panels[1]);

     if Result then begin
        DeleteFilesFilterIndex:=FilterIndex;
        end;
     end;
end;

procedure TToolsForm.GeneratorMenuItemNewClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing and
     (not Game.IsBusy) and
     CloseEditors and
     Assigned(MainForm.Generator) and
     (not MainForm.Generator.IsActive) then with MainForm.Generator do
     if Shutdown then begin // when the generator isn't active, 'Shutdown' merely saves a changed candidate set; it doesn't really trigger a generator shutdown
        Clear;
        end;
end;

procedure TToolsForm.GeneratorMenuItemSaveClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
       if CloseEditors and
          (Screen.ActiveForm=Self) and
          GeneratorMenuItemSave.Enabled and
          MainForm.Generator.HasSokoFile and
          MainForm.Generator.SokoFile.Modified then
          if   IsANewFileName(MainForm.Generator.FileName) then
               Save(GeneratorMenuItemSaveAs)
          else Save(GeneratorMenuItemSave); // 'Self': don't ask before overwriting an existing file
end;

procedure TToolsForm.GeneratorMenuItemSaveAsClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
       if CloseEditors and
          (Screen.ActiveForm=Self) and
          MainForm.Generator.HasSokoFile then
          Save(GeneratorMenuItemSaveAs);
end;

procedure TToolsForm.GeneratorEditMenuItemCutClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
       if CloseEditors and (Screen.ActiveForm=Self) then
          MainForm.Generator.CopyOrCutToClipboard(True);

end;

procedure TToolsForm.GeneratorEditMenuItemCopyClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
       if CloseEditors and (Screen.ActiveForm=Self) then begin
          MainForm.Generator.CopyOrCutToClipboard(False);
          end;
end;

procedure TToolsForm.GeneratorEditMenuItemPasteClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy or (Assigned(MainForm.Generator) and MainForm.Generator.IsActive) then Msg(ApplicationIsBusyText,Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+GeneratorText+SUB_TITLE_SEPARATOR+ImportFromClipboardText,MB_OK+MB_ICONINFORMATION)
     else
       if CloseEditors and (Screen.ActiveForm=Self) then
          MainForm.Generator.ImportFromClipboard;
end;

procedure TToolsForm.GeneratorEditMenuItemEditClick(Sender: TObject);
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
       if CloseEditors and (Screen.ActiveForm=Self) then with GeneratorTaskQueue do begin
          if Assigned(Plugin) then Plugin.Enter;
          try     if Assigned(Levels[StringGrid.Row]) then begin
                     if      MainForm.Modified or Self.Modified then begin // if the currently loaded level has been modified, then show the editor tabsheet so the user can see the level that the "Save - yes/no?" question refers to
                             PageControl1.ActivePage:=TabSheetEditor;
                             PageControl1Change(PageControl1);
                             if MakeNewLevel(GeneratorEditMenuItemEdit) then begin
                                if Sender=GeneratorEditMenuItemPlay then begin
                                   PageControl1.ActivePage:=TabSheetGenerator;
                                   PageControl1Change(PageControl1);
                                   ModalResult:=mrOk; Close; ModalResult:=mrOk;
                                   end;
                                end
                             else begin
                                PageControl1.ActivePage:=TabSheetGenerator;
                                PageControl1Change(PageControl1);
                                end;
                             end
                     else if MakeNewLevel(GeneratorEditMenuItemEdit) then begin
                             if Sender<>GeneratorEditMenuItemPlay then begin
                                PageControl1.ActivePage:=TabSheetEditor;
                                PageControl1Change(PageControl1);
                                end
                             else begin
                                ModalResult:=mrOk; Close; ModalResult:=mrOk;
                                end;
                             end;
                     end;
          finally if Assigned(Plugin) then Plugin.Leave;
          end;
          end;
end;

procedure TToolsForm.GeneratorEditMenuItemPlayClick(Sender: TObject);
begin
  GeneratorEditMenuItemEditClick(GeneratorEditMenuItemPlay);
end;

procedure TToolsForm.SetAlternatingOptimizations(AlternatingOptimizations__:TAlternatingOptimizations);
begin
  fAlternatingOptimizations:=AlternatingOptimizations__;
  case fAlternatingOptimizations of
    aoPrimarySecondary         : SettingsMenuAlternatingOptimizationsPrimarySecondary.Checked:=True;
    aoBoxLinesPrimary          : SettingsMenuAlternatingOptimizationsBoxLinesPrimary.Checked:=True;
    aoPrimarySecondaryBoxLines : SettingsMenuAlternatingOptimizationsPrimarySecondaryBoxLines.Checked:=True;
    aoBoxLinesPrimarySecondary : SettingsMenuAlternatingOptimizationsBoxLinesPrimarySecondary.Checked:=True;
  end; // case
end;

procedure TToolsForm.SettingsMenuAlternatingOptimizationsTypeClick(
  Sender: TObject);
begin
  if      Sender=SettingsMenuAlternatingOptimizationsPrimarySecondary then
          AlternatingOptimizations:=aoPrimarySecondary
  else if Sender=SettingsMenuAlternatingOptimizationsBoxLinesPrimary then
          AlternatingOptimizations:=aoBoxLinesPrimary
  else if Sender=SettingsMenuAlternatingOptimizationsPrimarySecondaryBoxLines then
          AlternatingOptimizations:=aoPrimarySecondaryBoxLines
  else if Sender=SettingsMenuAlternatingOptimizationsBoxLinesPrimarySecondary then
          AlternatingOptimizations:=aoBoxLinesPrimarySecondary;
end;

constructor TOptimizerTaskQueue.Create(Plugin__:TPlugin; StringGrid__:TStringGrid; ItemsCheckBox__:TCheckBox; PopupMenu__:TPopupMenu);
begin
  ClearAlternateOptimizationTasks;
  Inherited;
end;

procedure TOptimizerTaskQueue.ClearAlternateOptimizationTasks;
begin
  FillChar(AlternateOptimization,SizeOf(AlternateOptimization),0);
end;

function  TOptimizerTaskQueue.RegisterResultForAlternatingOptimizationTasks(SourceSnapshot__,NewSnapshot__:TExtendedSnapshotAsText; RecursionDepth__:Integer):Boolean;
const // returns 'True' if an alternate task has been put on the task queue;
      // precondition: must be called synchronized from the optimizer plugin, typically via 'TOpenForm.SynchronizedPluginCallback'
  ALTERNATE_TASK_TYPE:array[TAlternateOptimizationTaskType] of TAlternateOptimizationTaskType
    = (SecondaryPrimary,PrimarySecondary,PrimarySecondary,SecondaryPrimary);
  ALTERNATE_BOX_LINES_TASK_TYPE:array[TAlternateOptimizationTaskType] of TAlternateOptimizationTaskType
    = (BoxLinesPrimary,BoxLinesSecondary,BoxLinesSecondary,BoxLinesPrimary);
var
  AlternateTaskType,TaskType,T:TAlternateOptimizationTaskType;
  SourceSnapshotPrimaryMetric,SourceSnapshotSecondaryMetric:TGameMetrics;
  OldLevel:TLevel; OldSnapshot,ResultSnapshot:TExtendedSnapshotAsText;

  function  HasSameMetricTypes(Task1__,Task2__:TAlternateOptimizationTaskType):Boolean;
  begin
    with AlternateOptimization do with Tasks[Task1__] do
      Result:=(PrimaryMetric=Tasks[Task2__].PrimaryMetric) and (SecondaryMetric=Tasks[Task2__].SecondaryMetric);
  end;

  function  IsLegalAlternateTaskType(TaskType__,AlternateTaskType__:TAlternateOptimizationTaskType; TryAlternateTaskType__:Boolean):Boolean;
  var AlternateEnabled,RepeatEnabled:Boolean;

    function IsAlternatingBoxLinesEnabled:Boolean;
    begin
      Result:=ToolsForm.AlternatingOptimizations<>aoPrimarySecondary;
    end;

    function IsAlternatingPrimarySecondaryEnabled:Boolean;
    begin
      Result:=ToolsForm.AlternatingOptimizations<>aoBoxLinesPrimary;
    end;

  begin // 'IsLegalAlternateTaskType'
    with AlternateOptimization do with Tasks[TaskType__] do begin // note that it's 'TaskType__' and not 'AlternateTaskType__'; most checks in the function relate to the former
      AlternateEnabled:=ToolsForm.SettingsMenuAlternatingOptimizationsEnabled.Checked
                        and
                        TryAlternateTaskType__; // 'TryAlternateTaskType__' refers either to (alternating primary and secondary metrics) or (alternating boxlines and user metrics)

      if AlternateEnabled then begin

         // transitions may go up/down in the left column in the following
         // figure, and left/right in each row
         //
         // primary/secondary  <-> boxlines/primary
         // ^
         // |
         // v
         // secondary/primary  <-> boxlines/secondary

         case TaskType__ of
           PrimarySecondary  : if       AlternateTaskType__ =SecondaryPrimary  then
                                        AlternateEnabled   :=IsAlternatingPrimarySecondaryEnabled
                               else if  AlternateTaskType__ =BoxLinesPrimary   then
                                        AlternateEnabled   :=IsAlternatingBoxLinesEnabled;
           SecondaryPrimary  : if       AlternateTaskType__ =PrimarySecondary  then
                                        AlternateEnabled   :=IsAlternatingPrimarySecondaryEnabled
                               else if  AlternateTaskType__ =BoxLinesSecondary then
                                        AlternateEnabled   :=IsAlternatingBoxLinesEnabled and
                                                             IsAlternatingPrimarySecondaryEnabled;
           BoxLinesPrimary   :          AlternateEnabled   :=(AlternateTaskType__=PrimarySecondary) and
                                                             IsAlternatingBoxLinesEnabled;
           BoxLinesSecondary :          AlternateEnabled   :=(AlternateTaskType__=SecondaryPrimary) and
                                                             IsAlternatingBoxLinesEnabled and
                                                             IsAlternatingPrimarySecondaryEnabled;
         end; // case

         AlternateEnabled:=AlternateEnabled
                           and
                           (not ((AlternateTaskType__ in [BoxLinesPrimary,BoxLinesSecondary])
                                 and
                                 (Tasks[AlternateTaskType__].SecondaryMetric=gmBoxLines)
                                )
                           )
                           and
                           // don't let boxlines/secondary repeat primary/secondary
                           // in case they are identical, e.g., if
                           // primary/secondary are boxlines/moves
                           (not ((AlternateTaskType__=BoxLinesSecondary)
                                 and
                                 HasSameMetricTypes(PrimarySecondary,BoxLinesPrimary)
                                )
                           )
                           and
                           // boxlines/boxlines is never optimized in the right
                           // column, hence, a transition back from
                           // boxlines/boxlines to the left column must be
                           // discarded as well
                           (not ((TaskType__ in [BoxLinesPrimary,BoxLinesSecondary])
                                 and
                                 (Tasks[TaskType__].PrimaryMetric=Tasks[TaskType__].SecondaryMetric)
                                )
                           )
                           and
                           // primary and secondary metric may be identical, in
                           // which case there is only one optimization criteria
                           ((Tasks[PrimarySecondary].PrimaryMetric<>Tasks[PrimarySecondary].SecondaryMetric)
                            or
                            (AlternateTaskType__ in [PrimarySecondary,BoxLinesPrimary])
                           )
                           ;
         end;

      RepeatEnabled   :=ToolsForm.SettingsMenuAlternatingOptimizationsRepeat.Checked;

      Result:=AlternateEnabled
              and
              (RepeatEnabled
               or
               ((not (AlternateTaskType__ in Transitions))
                and
                (not // when 'repeat' is disabled, then only perform
                     // primary/secondary and secondary/primary once; this is
                     // in contrast to boxlines optimization, where 'once'
                     // must perform the base task (i.e., primary/secondary or
                     // secondary/primary) a second time; otherwise it
                     // wouldn't make sense to try the boxlines optimization as
                     // an attempt to improve the primary and secondary metrics
                     ((TaskType__         =SecondaryPrimary)
                      and
                      (AlternateTaskType__=PrimarySecondary)
                      and
                      (TaskType__         in Tasks[AlternateTaskType__].Transitions)
                     )
                )
               )
              )
              and
              Plugin.IsSupportedOptimization(
                MetricTypesToOptimizationFlags(Tasks[AlternateTaskType__].PrimaryMetric,
                                               Tasks[AlternateTaskType__].SecondaryMetric))
              and
              Assigned(Plugin.SokoFile.GetLevelByExtendedSnapshotAsTextSerialNo(SerialNo)); // 'True': the snapshot is still there on the queue, i.e., the user hasn't deleted it
      end;
  end;

  function  IsBestResult(TaskType__:TAlternateOptimizationTaskType; const NewMetrics__:TMetrics):Boolean;
  begin
    with AlternateOptimization.Tasks[TaskType__] do
      Result:=(SerialNo=0) or // '0': this is the first result for this optimization task type
              (CompareMetrics(PrimaryMetric,SecondaryMetric,MainForm.Game.PathFindingOptimizeMoves,NewMetrics__,Metrics)<0);
  end;

  function  LookupTaskType(PrimaryMetric__,SecondaryMetric__:TGameMetrics; var TaskType__:TAlternateOptimizationTaskType):Boolean;
  var TaskType:TAlternateOptimizationTaskType;
  begin // postcondition: the function returns the first matching set of metrics, if any
    Result:=False;
    with AlternateOptimization do
      for TaskType:=Low(Tasks) to High(Tasks) do with Tasks[TaskType] do
          if (PrimaryMetric=PrimaryMetric__) and (SecondaryMetric=SecondaryMetric__) then begin
             TaskType__:=TaskType; Result:=True; exit; {'exit': quick and dirty exit when found}
             end;
  end;

  function  MarkSnapshotForDeletion(var Snapshot__:TExtendedSnapshotAsText):Boolean;
  var i:Integer; SnapshotSerialNo:TSerialNo; Level:TLevel;
  begin
    Result:=False;
    SnapshotSerialNo:=Snapshot__.SerialNo;
    Plugin.MoveOptimizedSolutionToSolverQueue(False,Level); // first move the snapshot from the optimizer to the solver queue, thereby turning the snapshot into a task
    ImportFromOtherPlugins(False); // transfer the level with the snapshot (the new task) back from the solver queue to the optimizer queue

    Level:=Plugin.SokoFile.GetLevelByExtendedSnapshotAsTextSerialNo(SnapshotSerialNo);
    if Assigned(Level) then with StringGrid do begin // 'True': sanity check: the round trip to the solver queue and back to the optimizer queue succeeded
       for i:=Pred(RowCount) downto FixedRows do
           if Levels[i]=Level then begin // 'True': the new solution was imported by the optimizer task queue; the new solution wasn't dropped because it's a duplicate of an existing solution, and adding the solution to the grid succeeded
              Exclude(Level.Tag.Flags,ltfNew); // exclude 'ltfNew': don't save the solution upon exit from the optimizer
              Result:=True;
              break;
              end;
       end
    else Snapshot__:=nil; // the snapshot disappeared, e.g., it may have been a duplicate
  end;

  procedure SaveBestResult(TaskType__:TAlternateOptimizationTaskType;
                           SerialNo__:TSerialNo;
                           const Metrics__:TMetrics;
                           const SelectedRange__:TSelectedRange; // selected range of pushes
                           SourceSnapshotPrimaryMetric__,SourceSnapshotSecondaryMetric__:TGameMetrics); // 'SourceSnapshotPrimaryMetric__' and 'SourceSnapshotSecondaryMetric__' are the metric types used by the optimization which produced this best result
  var BetterMovesOrPushesOrBoxLines : Boolean;
  begin
    with AlternateOptimization do with Tasks[TaskType__] do begin
      BetterMovesOrPushesOrBoxLines := ( Metrics.MoveCount =                   0 ) or ( Metrics.PushCount =                   0 ) or ( Metrics.SecondaryMetrics.BoxLines =                                   0 ) or
                                       ( Metrics.MoveCount > Metrics__.MoveCount ) or ( Metrics.PushCount > Metrics__.PushCount ) or ( Metrics.SecondaryMetrics.BoxLines > Metrics__.SecondaryMetrics.BoxLines );
      SerialNo      :=SerialNo__; // remember the snapshot with the best metrics
      Metrics       :=Metrics__;  // save the new metrics
      SelectedRange :=SelectedRange__; // save the selected range of pushes

      TryAlternate  :=not ((Tasks[ALTERNATE_TASK_TYPE[TaskType__]].PrimaryMetric  =SourceSnapshotPrimaryMetric__)
                           and
                           (Tasks[ALTERNATE_TASK_TYPE[TaskType__]].SecondaryMetric=SourceSnapshotSecondaryMetric__)
                          ); // 'True': the alternate optimization task hasn't been tried yet
      TryBoxLines :=(TaskType__ in [PrimarySecondary,SecondaryPrimary]) and //  alternate boxlines optimization is only applicable for primary/secondary and secondary/primary tasks
                    not ((PrimaryMetric=SourceSnapshotSecondaryMetric__) and (SourceSnapshotPrimaryMetric__=gmBoxLines)); // True': the corresponding boxlines optimization task type hasn't been tried yet
      if ToolsForm.SettingsMenuAlternatingOptimizationsRepeat.Checked and
         ToolsForm.SettingsMenuAlternatingOptimizationsRepeatOnMovesPushesBoxLines.Checked then begin // 'True': only perform alternate optimizations if moves, pushes, or box lines improved
         TryAlternate := TryAlternate and BetterMovesOrPushesOrBoxLines;
         TryBoxLines  := TryBoxlines  and BetterMovesOrPushesOrBoxLines;
         end;
      end;
  end;

  function  StartAlternateTask(TaskType__,AlternateTaskType__:TAlternateOptimizationTaskType):Boolean;
  var AlternateTaskOptimizationFlags:Integer; IsLocked:Boolean;
      Level:TLevel; Snapshot:TExtendedSnapshotAsText;
  begin
    Result:=False;
    with AlternateOptimization do with Tasks[TaskType__] do begin
      AlternateTaskOptimizationFlags:=MetricTypesToOptimizationFlags(Tasks[AlternateTaskType__].PrimaryMetric,
                                                                     Tasks[AlternateTaskType__].SecondaryMetric);
      Level:=Plugin.SokoFile.GetLevelByExtendedSnapshotAsTextSerialNo(SerialNo);
      if Assigned(Level) then begin
         Snapshot:=Level.GetExtendedSnapshotAsTextBySerialNo(SerialNo);
         if Assigned(Snapshot) then begin
            if Snapshot=NewSnapshot__ then begin // 'True': the snapshot selected for further optimization is the newly created one; create a new task (a level) for it on the task queue, unless there already is an identical snapshot on the queue
               // send the newly created snapshot on a round trip, where it
               // first is put on the solver task queue, and then imported back
               // on the optimizer queue; that's the easiest safe way (given the
               // existing service functions) to turn the new result snapshot
               // into a task on the optimizer queue;
               // if the newly created snapshot is a duplicate, then
               // 'ImportFromOtherPlugins()' destroys it
               Plugin.MoveOptimizedSolutionToSolverQueue(False,Level);
               ImportFromOtherPlugins(False);
               end;

            Level:=Plugin.SokoFile.GetLevelByExtendedSnapshotAsTextSerialNo(SerialNo);
            if Assigned(Level) then begin
               Snapshot:=Level.GetExtendedSnapshotAsTextBySerialNo(SerialNo);
               if Assigned(Snapshot) then begin
                  if ((Selected[Level.Flags]
                       and
                       (Level<>Plugin.Level) // '<>': just to be sure; the current task (level) with the source snapshot should have 'Selected=False' at this point, but it's better to be safe than sorry
                      )
                      or
                      ((Level=Plugin.Level) // 'True': this is the currently selected task
                       and
                       (SourceSnapshot__=NewSnapshot__) // 'True': the caller is making arrangements to perform boxlines optimizations before optimization of the primary/secondary metrics
                      )
                      or
                      (Snapshot.SelectedRange[0]<>SelectedRange[0])
                      or
                      (Snapshot.SelectedRange[1]<>SelectedRange[1])
                     )
                     and
                     ((Snapshot.Tag<>AlternateTaskOptimizationFlags)
                      or
                      (Snapshot.SelectedRange[0]<>SelectedRange[0])
                      or
                      (Snapshot.SelectedRange[1]<>SelectedRange[1])
                      or
                      (Snapshot.SelectedRange[6]<>SelectedRange[6]) // interval size
                     ) then begin // 'True': the user has selected this snapshot for optimization with different metrics or a different range of moves; save the user's selection so it can be restored later
                     RestoreHijackedTask;
                     HijackedTaskSerialNo:=Snapshot.SerialNo;
                     HijackedTaskOptimizationFlags:=Snapshot.Tag;
                     HijackedSelectedRange:=Snapshot.SelectedRange;
                     end;

                  Snapshot.Tag:=AlternateTaskOptimizationFlags;
                  Snapshot.OptimizationFlags:=0; // '0': optimization hasn't been performed yet
                  FillChar(Snapshot.SelectedRange,SizeOf(Snapshot.SelectedRange),0);
                  Snapshot.SelectedRange[0]:=SelectedRange[0]; // set selected range, in pushes
                  Snapshot.SelectedRange[1]:=SelectedRange[1];
                  Snapshot.SelectedRange[2]:=SelectedRange[2];
                  Snapshot.SelectedRange[6]:=SelectedRange[6]; // interval size
                  IsLocked:=ltfLocked in Level.Tag.Flags;
                  try     Exclude(Level.Tag.Flags,ltfLocked); // otherwise Tools_.TTaskQueue.SetSelected' sets 'Selected=False', i.e., if it's the one currently being optimized
                          ResurrectLevel(Level); // ensure that the level is visible in the string-grid
                          if SourceSnapshot__<>NewSnapshot__ then // 'True': the caller is not making arrangements to perform boxlines optimizations before optimization of the primary/secondary metrics
                             Selected[Level.Flags]:=True;
                  finally if IsLocked then Include(Level.Tag.Flags,ltfLocked); // restore 'Locked' if necessary
                  end
                  end
               else Level:=nil; // this shouldn't happen
               end;

            CurrentTaskSerialNo:=SerialNo;
            Include(Transitions,AlternateTaskType__); // remember the transitions in order to support 'alternate once' and 'alternate repeatedly'
            if   (TaskType__          in [PrimarySecondary,SecondaryPrimary]) and
                 (AlternateTaskType__ in [BoxLinesPrimary,BoxLinesSecondary]) then
                 TryBoxLines:=False   // ensure that the transition to boxlines optimization doesn't occur again before a new better result has been found
            else TryAlternate:=False; // ensure that the transition to primary or secondary metrics optimization doesn't occur again before a new better result has been found

            if Assigned(Level) then begin // 'True': the snapshot (or rather the task) is on the task queue, ready for optimization
               with Plugin.SokoFile.Levels do Push(Remove(Level,False)); // move the new task to the top of the queue
               Result:=True;
               end;
            end;
         end;
      end;
  end;

  procedure UpdateMetricsTypesForAlternatingTasks;
  var FirstMetric,SecondMetric,Metric:TGameMetrics;

    procedure SetMetricsTypesForTask(PrimaryMetric__,SecondaryMetric__:TGameMetrics; TaskType__:TAlternateOptimizationTaskType);
    begin
      with AlternateOptimization do with Tasks[TaskType__] do
        if (PrimaryMetric<>PrimaryMetric__) or (SecondaryMetric<>SecondaryMetric__) then begin // 'True': the user has changed task type during the process
           FillChar(Tasks[TaskType__],SizeOf(Tasks[TaskType__]),0); // clear the old values, if any
           PrimaryMetric:=PrimaryMetric__;
           SecondaryMetric:=SecondaryMetric__;
           end;
    end;

  begin // 'UpdateMetricsTypesForAlternatingTasks'
    // the user may change alternating tasks type during the process
    with AlternateOptimization do begin
      if ToolsForm.AlternatingOptimizations<>aoBoxLinesPrimary then begin
         // set up alternating tasks based on the actual metrics used during the first optimization; this is disabled in favor of just performing "moves/pushes" alternating tasks; see below
         //FirstMetric:=OriginalPrimaryMetric; SecondMetric:=OriginalSecondaryMetric;

         // set up alternating tasks with "moves/pushes" only
         if      OriginalPrimaryMetric=gmMoves then begin
                 FirstMetric:=gmMoves;  SecondMetric:=gmPushes; // since the task selection flip-flops, the first selection will be 'pushes/moves'
                 end
         else if OriginalPrimaryMetric=gmPushes then begin
                 FirstMetric:=gmPushes; SecondMetric:=gmMoves;  // since the task selection flip-flops, the first selection will be 'moves/pushes'
                 end
         else if OriginalSecondaryMetric=gmMoves then begin
                 FirstMetric:=gmPushes; SecondMetric:=gmMoves;  // since the task selection flip-flops, the first selection will be 'moves/pushes'
                 end
         else begin
                 FirstMetric:=gmMoves;  SecondMetric:=gmPushes; // since the task selection flip-flops, the first selection will be 'pushes/moves'
              end;
         end
      else begin
         // with boxlines-only alternating tasks, set up alternating tasks based
         // on the metrics actually used during the first optimization
         FirstMetric:=OriginalPrimaryMetric; SecondMetric:=OriginalSecondaryMetric;
         if OriginalPrimaryMetric=gmBoxLines then
            for Metric:=Low(Metric) to High(Metric) do
                if (Metric<>SecondMetric) and
                   Plugin.IsSupportedOptimization(MetricTypesToOptimizationFlags(SecondMetric,Metric)) then begin
                   FirstMetric:=SecondMetric; SecondMetric:=Metric; // en example: the original metrics was "boxlines/pushes"; if "pushes/moves" ("second original metric/moves") is supported then pick this one for alternation
                   break; {'break': quick and dirty exit the 'for' loop as soon as a supported optimization type has been found}
                   end;
         end;

      SetMetricsTypesForTask(FirstMetric ,SecondMetric,PrimarySecondary);
      SetMetricsTypesForTask(SecondMetric,FirstMetric ,SecondaryPrimary);
      SetMetricsTypesForTask(gmBoxLines  ,FirstMetric ,BoxLinesPrimary);
      SetMetricsTypesForTask(gmBoxLines  ,SecondMetric,BoxLinesSecondary);
      end;
  end;

begin // 'TOptimizerTaskQueue.RegisterResultForAlternatingOptimizationTasks';
  Result:=False;
  if Assigned(Plugin) and
     (Plugin.Button.Tag=Ord(pbsCancel)) and // 'pbsCancel': the plugin is still running
     Plugin.HasSokoFile and
     Assigned(SourceSnapshot__) and
     (SourceSnapshot__.Metrics.PushCount>0) and // '>0': the original snapshot isn't empty
     HasOptimizationFlags(SourceSnapshot__.OptimizationFlags,SourceSnapshotPrimaryMetric,SourceSnapshotSecondaryMetric) and
     Assigned(ToolsForm) then
     with AlternateOptimization do begin
       if (CurrentTaskSerialNo=SourceSnapshot__.SerialNo) and
          LookupTaskType(SourceSnapshotPrimaryMetric,SourceSnapshotSecondaryMetric,TaskType) then begin // 'True': the current alternate optimization task returned a result
          RestoreHijackedTask; // this can first be done after the currently selected metrics have been picked up from the source snapshot; see the preceding statement;
          // update the metrics types for the alternating optimization tasks; the user may change task types while alternating optimizations are in progress;
          UpdateMetricsTypesForAlternatingTasks;
          end
       else begin // start a new alternating optimization task
          // initialize the different alternation tasks for this combination of primary and secondary metric types
          RestoreHijackedTask;
          ClearAlternateOptimizationTasks;
          OriginalPrimaryMetric  :=SourceSnapshotPrimaryMetric;
          OriginalSecondaryMetric:=SourceSnapshotSecondaryMetric;
          // update the metrics types for the alternating optimization tasks; the user may change task types while alternating optimizations are in progress;
          UpdateMetricsTypesForAlternatingTasks;
          // if alternating optimizations should be performed at least once, then initialize the tasks accordingly
          if (RecursionDepth__ =0) and
             ToolsForm.SettingsMenuAlternatingOptimizationsTriggerAtLeastOnce.Checked then // 'True': perform alternating optimizations at least once
             for TaskType:=Low(Tasks) to High(Tasks) do
                 SaveBestResult(TaskType,SourceSnapshot__.SerialNo,SourceSnapshot__.Metrics,SourceSnapshot__.SelectedRange,SourceSnapshotPrimaryMetric,SourceSnapshotSecondaryMetric);
          end;

       // update best results for the alternating optimization tasks
       ResultSnapshot:=NewSnapshot__; OldLevel:=nil; OldSnapshot:=nil;
       if Assigned(NewSnapshot__) and (NewSnapshot__.Metrics.PushCount>0) then begin
          if   LookupSnapshot(NewSnapshot__,OldLevel,OldSnapshot) then begin
               ResultSnapshot:=OldSnapshot;
               ResurrectLevel(OldLevel); // ensure that the old level is visible in the string-grid
               end;

          for TaskType:=Low(Tasks) to High(Tasks) do
              if IsBestResult(TaskType,ResultSnapshot.Metrics) then begin
                 Result:=True;
                 SaveBestResult(TaskType,ResultSnapshot.SerialNo,ResultSnapshot.Metrics,NewSnapshot__.SelectedRange,SourceSnapshotPrimaryMetric,SourceSnapshotSecondaryMetric);
                 end;
          if not Result then MarkSnapshotForDeletion(NewSnapshot__); // the new snapshot is not better than the existing ones; don't save it when the optimizer exits
          end;

       // find next task to perform, if any
       Result:=False; TaskType:=Low(TaskType); AlternateTaskType:=Low(AlternateTaskType);
       if  ToolsForm.AlternatingOptimizations in [aoBoxLinesPrimarySecondary] then // 'True': try boxlines tasks first
           for T:=PrimarySecondary to SecondaryPrimary do
               if not Result then begin               // 'True': search for boxlines tasks
                  TaskType:=T; AlternateTaskType:=ALTERNATE_BOX_LINES_TASK_TYPE[TaskType];
                  Result:=IsLegalAlternateTaskType(TaskType,AlternateTaskType,Tasks[TaskType].TryBoxLines);
                  if not Result then begin            // 'True': search for alternates to boxlines tasks
                     TaskType:=ALTERNATE_BOX_LINES_TASK_TYPE[T]; AlternateTaskType:=ALTERNATE_TASK_TYPE[TaskType];
                     Result:=IsLegalAlternateTaskType(TaskType,AlternateTaskType,Tasks[TaskType].TryAlternate);
                     end;
                  end;
       for T:=PrimarySecondary to SecondaryPrimary do // search for alternating primary and secondary metrics tasks
           if not Result then begin
              TaskType:=T; AlternateTaskType:=ALTERNATE_TASK_TYPE[TaskType];
              Result:=IsLegalAlternateTaskType(TaskType,AlternateTaskType,Tasks[TaskType].TryAlternate);
              end;
       for T:=PrimarySecondary to SecondaryPrimary do // search for boxlines tasks
           if not Result then begin
              TaskType:=T; AlternateTaskType:=ALTERNATE_BOX_LINES_TASK_TYPE[TaskType];
              Result:=IsLegalAlternateTaskType(TaskType,AlternateTaskType,Tasks[TaskType].TryBoxLines);
              end;
       for T:=BoxLinesPrimary to BoxLinesSecondary do // search for alternates to boxlines tasks
           if not Result then begin
              TaskType:=T; AlternateTaskType:=ALTERNATE_TASK_TYPE[TaskType];
              Result:=IsLegalAlternateTaskType(TaskType,AlternateTaskType,Tasks[TaskType].TryAlternate);
              end;

       if Result then
          Result:=StartAlternateTask(TaskType,AlternateTaskType) or
                  RegisterResultForAlternatingOptimizationTasks(SourceSnapshot__,nil,Succ(RecursionDepth__)); // the recursive calls are guaranteed to stop because each invocation of the function peels off one of the alternate task types

       if not Result then CurrentTaskSerialNo:=0;     // '0': no alternate optimization tasks in progress
       end;
  AlternateOptimization.PartitionSolutionIntoSubIntervals:=False; // "alternating optimizations" and "partition solution into subintervals" are implemented in a way which makes them mutually exclusive
  if not Result then begin
     AlternateOptimization.CurrentTaskSerialNo:=0;    // '0': no alternate optimization tasks in progress
     RestoreHijackedTask;
     end;
end;

function  TOptimizerTaskQueue.RegisterResultForPartitioningSolutionIntoSubIntervals(SourceSnapshot__,NewSnapshot__:TExtendedSnapshotAsText):Boolean;
var
  OptimizationFlags:Integer;
  b,IsHijackedTask,IsLocked:Boolean;
  SourceSnapshotPrimaryMetric,SourceSnapshotSecondaryMetric:TGameMetrics;
  SerialNo:TSerialNo;
  SelectedRange:TSelectedRange;
  ALevel:TLevel; OldSnapshot,ResultSnapshot:TExtendedSnapshotAsText;
begin
  Result:=False;
  if Assigned(Plugin) and
     (Plugin.Button.Tag=Ord(pbsCancel)) and // 'pbsCancel': the plugin is still running
     Assigned(Plugin.Level) and
     Plugin.HasSokoFile and
     Assigned(SourceSnapshot__) and
     (SourceSnapshot__.Metrics.PushCount>0) and // '>0': the original snapshot isn't empty
     HasOptimizationFlags(SourceSnapshot__.OptimizationFlags,SourceSnapshotPrimaryMetric,SourceSnapshotSecondaryMetric) and
     Assigned(ToolsForm) then
     with AlternateOptimization do begin
       // save the optimization settings and the current interval range and size
       OptimizationFlags:=SourceSnapshot__.Tag; // optimization type, e.g., 'moves/pushes' or 'pushes/moves'
       SerialNo         :=SourceSnapshot__.SerialNo;  // if there is no new snapshot, then optimize the next subinterval of the source snapshot
       SelectedRange    :=SourceSnapshot__.SelectedRange;
       IsHijackedTask   :=HijackedTaskSerialNo<>0; // remember whether the current task is hijacked
       SourceSnapshot__.SelectedRange[6]:=0; // reset "partion solution into subintervals" for the source snapshot to help the user to see that that the source snapshot has been processed

       if CurrentTaskSerialNo=SourceSnapshot__.SerialNo then begin // 'True': this is the continuation of a subinterval optimization in progress
          RestoreHijackedTask;
          end
       else begin // start a new subinterval optimization task
          // restore hijacked task, if any, and save current optimization type
          RestoreHijackedTask;
          ClearAlternateOptimizationTasks;
          OriginalPrimaryMetric                :=SourceSnapshotPrimaryMetric;
          OriginalSecondaryMetric              :=SourceSnapshotSecondaryMetric;
          Tasks[PrimarySecondary].SerialNo     :=SerialNo;      // original task
          Tasks[PrimarySecondary].SelectedRange:=SelectedRange; // original task
          end;

       if SerialNo<>Tasks[SecondaryPrimary].SerialNo then begin
          Tasks[SecondaryPrimary].SerialNo:=SerialNo;
          if   not IsHijackedTask then
               // save the first processed subinterval for this snapshot; if no
               // improvements are found, then the snapshot will be processed
               // again after advancing to the next subinterval; when the
               // subinterval optimization has finished with this snapshot, then
               // the its first subinterval is restored, so the user has the
               // option to try to optimize it again with different parameters;
               Tasks[SecondaryPrimary].SelectedRange:=SelectedRange
          else // for a hijacked task, it's impossible to restore its first
               // optimized subinterval
               FillChar(        Tasks[SecondaryPrimary].SelectedRange,
                         SizeOf(Tasks[SecondaryPrimary].SelectedRange),0);
          end;

       // check if the new snaphot, if any, is a duplicate or inferior to the source snapshot
       if Assigned(NewSnapshot__) and (NewSnapshot__.Metrics.PushCount>0) then begin
          if CompareMetrics(SourceSnapshotPrimaryMetric,SourceSnapshotSecondaryMetric,MainForm.Game.PathFindingOptimizeMoves,NewSnapshot__.Metrics,SourceSnapshot__.Metrics)<0 then begin
             // the new snapshot is better than the source snapshot
             SelectedRange :=NewSnapshot__.SelectedRange; // use the new interval
             IsHijackedTask:=LookupSnapshot(NewSnapshot__,ALevel,OldSnapshot);
             if   IsHijackedTask then begin // 'True': the newly created snapshot is a duplicate of a snapshot which already is on the task queue
                  ResurrectLevel(ALevel); // ensure that the old level is visible in the string-grid
                  SerialNo:=OldSnapshot.SerialNo; // optimize the existing snapshot; the new duplicate will be destroyed further down;
                  end
             else SerialNo:=NewSnapshot__.SerialNo; // the new snapshot isn't a duplicate of an existing task on the task queue
             end
          else begin
             // the new snapshot is not better than the source snapshot; delete
             // the new snapshot;
             Plugin.Level.SnapshotsAsText.Remove(NewSnapshot__,True);
             end;
          // send newly created snapshots on a round trip, where they first are
          // put on the solver task queue, and then imported back on the
          // optimizer queue; that's the easiest safe way (given the existing
          // service functions) to turn new snapshots into tasks on the
          // optimizer queue; if newly created snapshots are duplicates, then
          // 'ImportFromOtherPlugins()' destroys them;
          Plugin.MoveOptimizedSolutionToSolverQueue(False,ALevel);
          ImportFromOtherPlugins(False);
          end;

       // locate the selected snapshot on the task queue
       ALevel:=Plugin.SokoFile.GetLevelByExtendedSnapshotAsTextSerialNo(SerialNo);
       if   Assigned(ALevel) then
            ResultSnapshot:=ALevel.GetExtendedSnapshotAsTextBySerialNo(SerialNo)
       else ResultSnapshot:=nil;
       Result := Assigned( ResultSnapshot ) and
                 ( SelectedRange[ 6 ] > 0 ) and               // '>': 'True': the snapshot has an interval size
                 ( SelectedRange[ 1 ] < SelectedRange[ 2 ] ); // '<': 'True': not end of snapshot

       if ( ( not Result ) or ( ResultSnapshot <> SourceSnapshot__ ) ) and
          ( Tasks[ SecondaryPrimary ].SelectedRange[ 6 ] > 0 ) and
          ( Tasks[ SecondaryPrimary ].SerialNo = SourceSnapshot__.SerialNo ) then begin
          // restore the first processed subinterval for the source snapshot
          SourceSnapshot__.SelectedRange := Tasks[ SecondaryPrimary ].SelectedRange;
          end;

       if Result then begin // 'True': optimize the next subinterval of the selected snapshot
          if IsHijackedTask then begin
             // save the settings for this snapshot before they are overwritten with settings for the selected subinterval
             HijackedTaskSerialNo:=SerialNo;
             HijackedTaskOptimizationFlags:=ResultSnapshot.Tag;
             HijackedSelectedRange:=ResultSnapshot.SelectedRange;
             end;
          // select the next subinterval for optimization
          ResultSnapshot.SelectedRange         := SelectedRange;
          ResultSnapshot.SelectedRange[ 0 ]    := Max( Succ( ResultSnapshot.SelectedRange[ 0 ] ), // 'Succ': ensure progress
                                                       ResultSnapshot.SelectedRange[ 0 ] + ( ( ( 100 - MainForm.Optimizer.SubIntervalOverlapPct ) * ( ResultSnapshot.SelectedRange[ 1 ] - ResultSnapshot.SelectedRange[ 0 ] ) ) div 100  ) );
          ResultSnapshot.SelectedRange[ 1 ]    := Min( ResultSnapshot.SelectedRange[ 0 ] + ResultSnapshot.SelectedRange[ 6 ], ResultSnapshot.SelectedRange[ 2 ] );

          // keep the interval size even for the last interval, so the user can
          // see that subinterval optimization still is in progress, which
          // entails that the currently processed interval doesn't trigger the
          // "alternate optimizations" feature;
          //if ResultSnapshot.SelectedRange[ 1 ] =  ResultSnapshot.SelectedRange[ 2 ] then // 'True': end of snapshot
          //   ResultSnapshot.SelectedRange[ 6 ] := 0;

          // set the correct optimization type
          ResultSnapshot.Tag                   := OptimizationFlags; //MetricTypesToOptimizationFlags(OriginalPrimaryMetric,OriginalSecondaryMetric);
          ResultSnapshot.OptimizationFlags     := 0; // '0': optimization hasn't been performed yet
          // select the snapshot for optimization
          IsLocked:=ltfLocked in ALevel.Tag.Flags;
          try     Exclude(ALevel.Tag.Flags,ltfLocked); // otherwise Tools_.TTaskQueue.SetSelected' sets 'Selected=False', i.e., if it's the one currently being optimized
                  ResurrectLevel(ALevel); // ensure that the level is visible in the string-grid
                  Selected[ALevel.Flags]:=True;
          finally if IsLocked then Include(ALevel.Tag.Flags,ltfLocked); // restore 'Locked' if necessary
          end;
          // move the new task to the front of the optimizer task queue
          with Plugin.SokoFile.Levels do Push(Remove(ALevel,False));
          CurrentTaskSerialNo := ResultSnapshot.SerialNo;
          end;

       if Assigned(Plugin.Level) and
          (Plugin.Level.Flags=StringGrid.Row) and
          (Screen.ActiveForm=ToolsForm) and
          (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer) then begin
          // update the screen to show that subinterval optimization now has been disabled for the source snapshot
          ToolsForm.PluginLevelInfo.ReplayInfo.SelectedRange:=SourceSnapshot__.SelectedRange;
          ToolsForm.ShowOptimizationRange(-1);
          if Result and (ResultSnapshot<>SourceSnapshot__) and (ALevel.Flags>=StringGrid.FixedRows) then begin
             // change focus to the snapshot which now has been selected for optimization
             Plugin.Enter;
             try     StringGrid.Row:=ALevel.Flags;
                     ScrollInView(StringGrid.Row);
                     ToolsForm.OptimizeSolutionsStringGridSelectCell(nil,StringGrid.Col,StringGrid.Row,b); // ends with a call to 'ShowStatus', hence, there is no reason to do it again here
             finally Plugin.Leave;
             end;
             end;
          end;
       end;
  AlternateOptimization.PartitionSolutionIntoSubIntervals:=Result;
  if not Result then begin
     AlternateOptimization.CurrentTaskSerialNo:=0;    // '0': no alternate optimization tasks in progress
     RestoreHijackedTask;
     end;
end;

function  TOptimizerTaskQueue.RestoreHijackedTask:Boolean;
var IsLocked:Boolean; Level:TLevel; Snapshot:TExtendedSnapshotAsText;
begin
  Result:=False;
  with AlternateOptimization do
    if HijackedTaskSerialNo<>0 then begin
       if Assigned(Plugin) and Plugin.HasSokoFile then begin
          Plugin.Enter;
          try
            Level:=Plugin.SokoFile.GetLevelByExtendedSnapshotAsTextSerialNo(HijackedTaskSerialNo);
            if Assigned(Level) then begin
               Snapshot:=Level.GetExtendedSnapshotAsTextBySerialNo(HijackedTaskSerialNo);
               if Assigned(Snapshot) then begin
                  IsLocked:=ltfLocked in Level.Tag.Flags;
                  try     Exclude(Level.Tag.Flags,ltfLocked); // otherwise Tools_.TTaskQueue.SetSelected' sets 'Selected=False' if the level is locked, i.e., if it's the one currently being optimized
                          Snapshot.Tag:=HijackedTaskOptimizationFlags;
                          Snapshot.SelectedRange:=HijackedSelectedRange;
                          if   (Level<>Plugin.Level) and (not IsLocked) then begin
                               Selected[Level.Flags]:=True;
                               Result:=Selected[Level.Flags];
                               end
                          else Result:=True; // don't re-select the currently running optimization task
                  finally if IsLocked then Include(Level.Tag.Flags,ltfLocked); // restore 'Locked' if necessary
                  end;
                  if Assigned(ToolsForm) and ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetOptimizer) then
                     PostMessage(ToolsForm.Handle,MSG_OPTIMIZER_REFRESH,0,Cardinal(Plugin));
                  end;
               end;
          finally Plugin.Leave;
          end;
          end;
       HijackedTaskSerialNo:=0;
       end;
end;

function  TOptimizerTaskQueue.IsLevelSelectedForAlternatingOptimizations(Level__:TLevel):Boolean;
var T:TAlternateOptimizationTaskType;
begin
  Result:=False;
  if Assigned(Level__) then with AlternateOptimization do
     for T:=Low(Tasks) to High(Tasks) do with Tasks[T] do
         if (TryAlternate or TryBoxLines)
            and
            Assigned(Level__.GetExtendedSnapshotAsTextBySerialNo(SerialNo)) then begin
            Result:=True; exit; {'exit': quick and dirty exit when found}
            end;

  Result:=Result
          or
          ((AlternateOptimization.HijackedTaskSerialNo<>0)
           and
           Assigned(Level__.GetExtendedSnapshotAsTextBySerialNo(AlternateOptimization.HijackedTaskSerialNo))
          );
end;

procedure TToolsForm.CaptureFileMenuItemNewClick(Sender: TObject);
begin
  CaptureForm.FileNew( Sender );
end;

procedure TToolsForm.CaptureFileMenuItemOpenClick(Sender: TObject);
begin
  CaptureForm.FileOpen( Sender );
end;

procedure TToolsForm.CaptureFileMenuItemSaveClick(Sender: TObject);
begin
  CaptureForm.FileSave( Sender );
end;

procedure TToolsForm.CaptureFileMenuItemSaveAsClick(Sender: TObject);
begin
  CaptureForm.FileSaveAs( Sender );
end;

procedure TToolsForm.CaptureViewZoomItemClick(Sender: TObject);
begin
  CaptureForm.ZoomItemClick( Sender );
end;

procedure TToolsForm.CaptureViewMenuItemLoadImageFromUpscaledViewOfImageClick(
  Sender: TObject);
begin
  CaptureForm.LoadImageFromUpscaledViewOfImage( Sender );
end;

procedure TToolsForm.CaptureEditMenuItemUndoClick(Sender: TObject);
begin
  CaptureForm.EditUndo( Sender );
end;

procedure TToolsForm.CaptureEditMenuItemRedoClick(Sender: TObject);
begin
  CaptureForm.EditRedoItemClick( Sender );
end;

procedure TToolsForm.CaptureEditMenuItemCutClick(Sender: TObject);
begin
  CaptureForm.EditCut( Sender );
end;

procedure TToolsForm.CaptureEditMenuItemCopyClick(Sender: TObject);
begin
  CaptureForm.EditCopy( Sender );
end;

procedure TToolsForm.CaptureEditMenuItemPasteClick(Sender: TObject);
begin
  CaptureForm.EditPaste( Sender );
end;

procedure TToolsForm.CaptureEditCaptureItemClick(Sender: TObject);
//var OriginalWindowState : TWindowState;
begin
  CaptureForm.EditCapture( Sender );
end;

procedure TToolsForm.CaptureEditMenuItemSelectAllClick(Sender: TObject);
begin
  CaptureForm.CaptureEditSelectAllItemClick( Sender );
end;

procedure TToolsForm.CaptureViewMenuItemShowColorQuantizationClick(
  Sender: TObject);
begin
  CaptureForm.CaptureViewColorQuantizationItemClick( Sender );
end;

procedure TToolsForm.CaptureViewMenuItemShowImageAnalysisResultsClick(
  Sender: TObject);
begin
  CaptureForm.CaptureViewImageAnalysisResultsItemClick( Sender );
end;

procedure TToolsForm.CaptureSettingsMenuItemSnapToNearbyEdgesClick(
  Sender: TObject);
begin
  CaptureForm.CaptureSettingsSnapToNearbyEdgesItemClick( Sender );
end;

procedure TToolsForm.CaptureSettingsMenuItemUseAutomaticCompletionClick(
  Sender: TObject);
begin
  CaptureForm.CaptureSettingsUseAutomaticCompletionItemClick( Sender );
end;

procedure TToolsForm.CaptureSettingsMenuItemKeepAuthorAndDesignerNamesClick(
  Sender: TObject);
begin
  CaptureForm.CaptureSettingsKeepAuthorAndDesignerNamesItemClick( Sender );
end;

procedure TToolsForm.CaptureSettingsSkinExportFormatItemClick(
  Sender: TObject);
begin
  CaptureForm.SettingsSkinExportFormatTypeItemClick( Sender );
end;

procedure TToolsForm.CaptureSettingsSkinExportFormatObjectBackgroundItemClick(
  Sender: TObject);
begin
  CaptureForm.SettingsSkinExportFormatObjectBackgroundClick( Sender );
end;

procedure TToolsForm.CaptureSettingsMenuItemGridColorClick(Sender: TObject);
begin
  CaptureForm.CaptureSettingsGridColorItemClick( Sender );
end;

procedure TToolsForm.CaptureSettingsColorQuantizationColorsItemClick(
  Sender: TObject);
begin
  CaptureForm.SettingsColorLevelItemClick( Sender );
end;

procedure TToolsForm.CaptureSettingsColorQuantizationIgnoreSmallColorDifferencesClick(
  Sender: TObject);
begin
  CaptureForm.SettingsIgnoreSmallColorDifferencesClick( Sender );
end;


procedure TToolsForm.CaptureSettingsMenuItemImageAnalysisClick(Sender: TObject);
begin
  CaptureForm.SettingsImageAnalysisItemsClick( Sender );
end;

procedure TToolsForm.PreviousStepButtonClick(Sender: TObject);
begin
  CaptureForm.PreviousStepButtonClick( Sender );
end;

procedure TToolsForm.NextStepButtonClick(Sender: TObject);
begin
  CaptureForm.NextStepButtonClick( Sender );
end;

procedure TToolsForm.SpinEditChange(Sender: TObject);
begin
  if ( PageControl1.ActivePage = TabSheetCapture ) and Assigned( CaptureForm ) then CaptureForm.SpinEditChange( Sender );
end;

procedure TToolsForm.SpinEditExit(Sender: TObject);
begin
  if ( PageControl1.ActivePage = TabSheetCapture ) and Assigned( CaptureForm ) then CaptureForm.SpinEditExit( Sender );
end;

procedure TToolsForm.CaptureImage1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CaptureForm.CaptureImage1MouseDown( Sender, Button, Shift, X, Y );
end;

procedure TToolsForm.CaptureImage1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  CaptureForm.CaptureImage1MouseMove( Sender, Shift, X, Y );
end;

procedure TToolsForm.CaptureImage1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CaptureForm.CaptureImage1MouseUp( Sender, Button, Shift, X, Y );
end;

procedure TToolsForm.CaptureEditMenuItemDrawingToolClick(Sender: TObject);
begin
  CaptureForm.EditMenuItemDrawingToolClick( Sender );
end;

procedure TToolsForm.WallCapCheckBoxClick(Sender: TObject);
begin
  CaptureForm.WallCapCheckBoxClick( Sender );
end;

procedure TToolsForm.CaptureToolButtonPlayClick(Sender: TObject);
var W, H : Integer; CapturedBoardAsText : String;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
        if CloseEditors and (Screen.ActiveForm=Self) and
           Assigned( CaptureForm) and CaptureToolButtonPlay.Enabled and
           ( ( CaptureForm.Editor.PuzzleFileName = '' ) or CaptureForm.Save( PlayPuzzleBitBtn ) ) then begin
           if CaptureForm.Editor.Step = csSavePuzzle then begin // play the captured level in the main window
              if CaptureForm.BoardAsTextWithBorder( CapturedBoardAsText, W, H ) then begin
                 if           ( W = Game.BoardWidth) and
                              ( H = Game.BoardHeight ) and
                              ( CapturedBoardAsText = SokFile_.BoardToText( Game.BoardWidth, Game.BoardHeight, Game.StartBoard, '' ) ) then
                              BtnOKClick(Self)
                 else
                      if      MainForm.Modified or Self.Modified then begin // if the currently loaded level has been modified, then show the editor tabsheet so the user can see the level that the "Save - yes/no?" question refers to
                              PageControl1.ActivePage:=TabSheetEditor;
                              PageControl1Change(PageControl1);

                              if ( ( CaptureForm.Editor.PuzzleFileName =  '' ) and MakeNewLevel(PlayPuzzleBitBtn) )
                                 or
                                 ( ( CaptureForm.Editor.PuzzleFileName <> '' ) and MainForm.CloseLevel( ToolsForm ) and MainForm.LoadGame( CaptureForm.Editor.PuzzleFileName, False ) ) then begin
                                 PageControl1.ActivePage:=TabSheetCapture;
                                 PageControl1Change(PageControl1);
                                 ModalResult:=mrOk; Close; ModalResult:=mrOk;
                                 end
                              else begin
                                 PageControl1.ActivePage:=TabSheetCapture;
                                 PageControl1Change(PageControl1);
                                 end;
                              end
                      else if ( ( CaptureForm.Editor.PuzzleFileName =  '' ) and MakeNewLevel(PlayPuzzleBitBtn) )
                              or
                              ( ( CaptureForm.Editor.PuzzleFileName <> '' ) and MainForm.CloseLevel( ToolsForm ) and MainForm.LoadGame( CaptureForm.Editor.PuzzleFileName, False ) ) then begin
                              ModalResult:=mrOk; Close; ModalResult:=mrOk;
                              end;
                 end
              else Msg( TEXT_TASK_FAILED, ToolsForm.Caption, MB_ICONINFORMATION + MB_OK );
              end
           else begin // use the captured skin for playing in the main window
              if ( CaptureForm.Editor.SkinFileName = '' ) or
                 ( not FileExists( CaptureForm.Editor.SkinFileName ) ) then
                 CaptureForm.Editor.SkinFileName := '';
              if CaptureForm.Save( PlaySkinBitBtn ) and
                 OpenForm.LoadImage( CaptureForm.Editor.SkinFileName ) then begin
                 OptionsForm.LoadData; // load current settings
                 if   MainForm.Skins.LoadFromFile( CaptureForm.Editor.SkinFileName, MainForm.Skins.CommonSkinsScriptFileName ) then begin
                      OptionsForm.SaveData( False );
                      ModalResult:=mrOk; Close; ModalResult:=mrOk
                      end
                 else OptionsForm.LoadData; // on fail, reload the settings; the import may have left some garbage
                 SettingsModified:=True;
                 end;
              end;
           end;
end;

procedure TToolsForm.CaptureMenuEditClick(Sender: TObject);
begin
  CaptureForm.SetSkinHints;
end;

procedure TToolsForm.CaptureEditMenuItemEditLevelClick(Sender: TObject);
var W, H : Integer; CapturedBoardAsText : String;
begin
  if OpenForm.Game.IsIdleAndStopReplayingAndBrowsing then
     if Game.IsBusy then Msg(ApplicationIsBusyText,Application.Title,MB_OK+MB_ICONINFORMATION)
     else
        if CloseEditors
           and
           (Screen.ActiveForm=Self)
           and
           Assigned( CaptureForm)
           and
           CaptureEditMenuItemEditLevel.Enabled
           then begin
              if CaptureForm.BoardAsTextWithBorder( CapturedBoardAsText, W, H ) then begin
                 if           ( W = Game.BoardWidth  ) and
                              ( H = Game.BoardHeight ) and
                              ( CapturedBoardAsText = SokFile_.BoardToText( Game.BoardWidth, Game.BoardHeight, Game.Board, '' ) )
                              then begin
                              PageControl1.ActivePage:=TabSheetEditor;
                              PageControl1Change(PageControl1);
                              end
                 else
                      if      MainForm.Modified or Self.Modified then begin // if the currently loaded level has been modified, then show the editor tabsheet so the user can see the level that the "Save - yes/no?" question refers to
                              PageControl1.ActivePage:=TabSheetEditor;
                              PageControl1Change(PageControl1);

                              if MakeNewLevel(CaptureEditLevelBitBtn) then begin
                                 end
                              else begin
                                 PageControl1.ActivePage:=TabSheetCapture;
                                 PageControl1Change(PageControl1);
                                 end;
                              end
                      else if MakeNewLevel(CaptureEditLevelBitBtn) then begin
                              PageControl1.ActivePage:=TabSheetEditor;
                              PageControl1Change(PageControl1);
                              end;
                 end
              else Error( TEXT_TASK_FAILED, '' );
           end;
end;

procedure TToolsForm.CaptureEditMenuItemMatchingSkinClick(Sender: TObject);
begin
  CapturePopupMenuPopup( Sender );
end;

procedure TToolsForm.SkinBitBtnClick(Sender: TObject);
begin
  if Assigned( CaptureForm ) then
     if      CaptureForm.Editor.Step = csBoard then
             with ClientToScreen( Point( TabsheetCapture.Left + CaptureBoardPanel  .Left + SkinBitBtn .Left, TabsheetCapture.Top + CaptureBoardPanel  .Top + SkinBitBtn .Top ) ) do
               CapturePopupMenu.Popup( X, Y )
     else if CaptureForm.Editor.Step = csColumnsRows then
             with ClientToScreen( Point( TabsheetCapture.Left + CaptureGridPanel.Left + SkinBitBtn2.Left, TabsheetCapture.Top + CaptureGridPanel.Top + SkinBitBtn2.Top ) ) do
               CapturePopupMenu.Popup( X, Y )
     else if CaptureForm.Editor.Step = csBoardSquares then
             with ClientToScreen( Point( TabsheetCapture.Left + CaptureSquaresPanel.Left + SkinBitBtn3.Left, TabsheetCapture.Top + CaptureSquaresPanel.Top + SkinBitBtn3.Top ) ) do
               CapturePopupMenu.Popup( X, Y )
end;

procedure TToolsForm.CapturePopupMenuPopup(Sender: TObject);
var Index : Integer; FileName : String; MenuItem, MenuItem2 : TMenuItem;
begin
    try

      CaptureEditMenuItemMatchingSkinRecentSkinsClearRecentSkinsClick( nil );

      try

        CaptureForm.SetSkinHints;

        for Index := Pred( CaptureForm.SkinsComboBox.Items.Count ) downto 0 do begin
            FileName := ExpandedFilePath( CaptureForm.SkinsComboBox.Items[Index], MainForm.MyDocumentsFolder );
            if not FileExists( FileName ) then
               CaptureForm.SkinsComboBox.Items.Delete( Index );
            end;

        for Index := Pred( CaptureForm.SkinsComboBox.Items.Count ) downto 0 do begin
            CaptureForm.SkinsComboBox.Items.Objects[ Index ] := TObject( Index );
            MenuItem:=TMenuItem.Create(ToolsForm);
            MenuItem.Tag:=Index; // 'Tag' = reference back to the skins
            CaptureEditMenuItemMatchingSkinRecentSkins.Insert(0, MenuItem );
            MenuItem.Caption:=ExtractFileNameWithoutPathAndExtension( CaptureForm.SkinsComboBox.Items[ Index ] );
            MenuItem.OnClick:=CaptureMenuItemLoadSkinFromHistoryClick;

            MenuItem2:=TMenuItem.Create(ToolsForm);
            MenuItem2.Tag:=MenuItem.Tag;
            CaptureMenuItemRecentSkins.Insert(0, MenuItem2 );
            MenuItem2.Caption:=MenuItem.Caption;
            MenuItem2.OnClick:=MenuItem.OnClick;
            end;

      except on E:Exception do begin
             CaptureEditMenuItemMatchingSkinRecentSkinsClearRecentSkinsClick( nil );
             Error( E.Message, '' );
             end;
      end;

    finally  //CaptureMenuItemLoadRecentSkin.Visible := CaptureMenuItemLoadRecentSkin.Count > 2;
            //CaptureMenuItemLoadRecentSkinSeparator.Visible:=Visible;
            CaptureEditMenuItemMatchingSkinRecentSkins.Enabled := CaptureEditMenuItemMatchingSkinRecentSkins.Count > 2;
            CaptureMenuItemRecentSkins                .Enabled := CaptureMenuItemRecentSkins                .Count > 2;
    end;

end;

procedure TToolsForm.CaptureEditMenuItemMatchingSkinLoadSkinClick(Sender: TObject);
begin
  CaptureForm.SkinBitBtnClick( Sender );
end;

procedure TToolsForm.CaptureMenuItemLoadSkinFromHistoryClick(Sender: TObject);
var SkinFileName : String;
begin
  if Assigned( CaptureForm ) and ( Sender is TMenuItem ) then with Sender as TMenuItem do
     if ( Tag >= 0 ) and ( Tag < CaptureForm.SkinsComboBox.Items.Count ) then begin
        SkinFileName := ExpandedFilePath(CaptureForm.SkinsComboBox.Items[Tag], MainForm.MyDocumentsFolder);
        if not StrEqual( SkinFileName, CaptureForm.DefaultMatchingSkinFileName ) then
           AddItemOrMoveItemToTopOfComboBox( CaptureForm.SkinsComboBox, SKINS_CAPACITY, AbbreviatedFilePath( SkinFileName, MainForm.MyDocumentsFolder ), False );
        CaptureForm.FindBoardUsingMatchingSkin( SkinFileName, CaptureForm.Settings.MatchThresholdPct, True );
        end;
end;

procedure TToolsForm.CaptureEditMenuItemMatchingSkinRecentSkinsClearRecentSkinsClick(Sender: TObject);
begin
  with CaptureEditMenuItemMatchingSkinRecentSkins do begin
    while Count>2 do Items[0].Free; // clear the list, leaving static items only
    Enabled := Count > 2;
    end;
  with CaptureMenuItemRecentSkins do begin
    while Count>2 do Items[0].Free; // clear the list, leaving static items only
    Enabled := Count > 2;
    end;
  if Assigned( Sender ) then
     CaptureForm.SkinsComboBox.Clear;
end;

procedure TToolsForm.CaptureSettingsMenuItemDefaultSkinClick(
  Sender: TObject);
begin
  if Assigned( CaptureForm ) then CaptureForm.SettingsMenuItemDefaultSkinClick( Sender );
end;

procedure TToolsForm.CaptureSettingsMenuItemSkinClick(Sender: TObject);
begin
  if Assigned( CaptureForm ) then CaptureForm.SettingsMenuItemSkinClick( Sender );
end;

procedure TToolsForm.OnFontChange;
begin
  StatusBar1.Font.Assign(Self.Font);
  CurrentSolverGroupBox.Font.Assign(Self.Font);
  CurrentOptimizerGroupBox.Font.Assign(Self.Font);
  PluginLevelFileNamePanel.Font.Assign(Self.Font);
  LevelNamePanel.Font.Assign(Self.Font);
  LevelNamePanel.Font.Size:=8; // tries to ensure that text never exceeds the height of the panel. Text is drawn manually inside the panel, using the "ImageReplaySpeed" bitmap.
  ImageReplaySpeed.Canvas.Font.Assign(LevelNamePanel.Font);
  PluginLevelFileNamePanel.Font.Color:=ApplicationHiglightedTextColor;
  LevelNamePanel.Font.Color:=PluginLevelFileNamePanel.Font.Color;
  CurrentSolverGroupBox.Font.Color:=PluginLevelFileNamePanel.Font.Color;
  CurrentOptimizerGroupBox.Font.Color:=PluginLevelFileNamePanel.Font.Color;

  StatusLabel1.Height:=Self.Canvas.TextHeight(FONT_HEIGHT_TEST_STRING)+2;
  StatusLabel2.Height:=StatusLabel1.Height;
  StatusLabel3.Height:=StatusLabel1.Height;
  StatusLabel1.Left:=PanelBtn.ClientWidth-StatusLabel1.Width-4;
  StatusLabel2.Left:=PanelBtn.ClientWidth-StatusLabel2.Width-4;
  StatusLabel2.Top:=PanelBtn.ClientHeight-2-StatusLabel2.Height;
  StatusLabel3.Left:=PanelBtnBevel1.Left-StatusLabel3.Width-4;
  StatusLabel3.Top:=PanelBtn.ClientHeight-2-StatusLabel3.Height;
  with StatusLabel1 do Visible:=(Left>=PanelBtnBevel1.Left+PanelBtnBevel1.Width) and
                                (Top+Height<=StatusLabel2.Top);
  with StatusLabel2 do Visible:=Left>=PanelBtnBevel1.Left+PanelBtnBevel1.Width;
  with StatusLabel3 do Visible:=Left>=BtnHelp.Left+BtnHelp.Width+4;
end;

end.

