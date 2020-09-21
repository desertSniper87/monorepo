unit Open1_;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  {$WARNINGS OFF}
  FileCtrl, // Warning: Unit 'FileCtrl' is specific to a platform
  {$WARNINGS ON}
  Grids,
  ExtCtrls, ComCtrls, Menus, jpeg,
  Buttons, MPlayer, Spin,
  YASGen_,Misc_,Game_,GView_,IniFile_,Pack_,Pict_,Music_,SokUtil_, SokFile_,Plugin_;

const
  OPEN_FORM_PANEL_INDEX_HINT     = 1;
  OPEN_FORM_PANEL_INDEX_STATE    = 0;

  MAX_FILE_HISTORY_ITEMS         = 25;
  MAX_TOOL_TIPS_OFFSET_PIXELS    = 50;
  TOOL_TIP_FREE_ZONE_PIXELS      = 16; // 'OnMouseMove' isn't triggered when the mouse is over a control's scrollbar, hence, this extra tooltip free zone

type
  TSizeHandle = (shNull,
                 shBottomLeft, shLeft,  shTopLeft,     shTop,
                 shTopRight,   shRight, shBottomRight, shBottom);
  TState      = (stNull,stReady,stDrag,stSize);
  TText       = record FileName:String; Memo:TMemo; end;
  TTextType   = (tGameNotes,tSkinScript,tTextFile);
  TToolTips   = record
    Enabled   : Boolean;
    OffsetX   : Integer;
    OffsetY   : Integer;
  end;
  TOpenForm   = class(TForm)
    StatusBar1: TStatusBar;
    TopPanel: TPanel;
    DiskGroupBox: TGroupBox;
    DriveComboBox1: TDriveComboBox;
    FilterComboBox1: TFilterComboBox;
    FolderLabel: TLabel;
    DriveLabel: TLabel;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    FileTypeLabel: TLabel;
    FileLabel: TLabel;
    GameFileGroupBox: TGroupBox;
    CollectionGroupBox: TGroupBox;
    BtnOpen: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    GameNameLabel: TLabel;
    CollectionFileNameLabel: TLabel;
    GameBoardPanel: TPanel;
    GameBoardImage: TImage;
    GameLoadErrorMemo: TMemo;
    GameNamePanel: TPanel;
    CollectionFileNamePanel: TPanel;
    CollectionStringGrid: TStringGrid;
    GameDataGroupBox: TGroupBox;
    GameInfoMemo: TMemo;
    GameSolutionTextLabel: TLabel;
    GameSolutionLabel: TLabel;
    GameSolutionPushesLabel: TLabel;
    EditPopupMenu: TPopupMenu;
    MenuItemEdit: TMenuItem;
    N1: TMenuItem;
    MenuItemUndo: TMenuItem;
    N2: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemDelete: TMenuItem;
    N3: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    N4: TMenuItem;
    MenuItemTextColor: TMenuItem;
    MenuItemBackgroundColor: TMenuItem;
    ColorDialog1: TColorDialog;
    MenuItemFont: TMenuItem;
    FontDialog1: TFontDialog;
    BtnEditMenu: TButton;
    FilePopupMenu: TPopupMenu;
    MenuItemFileCopy: TMenuItem;
    MenuItemFileMove: TMenuItem;
    MenuItemFileDelete: TMenuItem;
    MenuItemFileRename: TMenuItem;
    N5: TMenuItem;
    MenuItemNewFolder: TMenuItem;
    BtnFileMenu: TButton;
    MenuItemSubMenuNew: TMenuItem;
    MenuItemNewLevelCollection: TMenuItem;
    GameSolutionMovesRadioButton: TRadioButton;
    GameSolutionPushesRadioButton: TRadioButton;
    ImageFileGroupBox: TGroupBox;
    BtnOK: TButton;
    BtnCancel1: TButton;
    BtnHelp1: TButton;
    ImagePanel: TPanel;
    MusicImage: TImage;
    SoundImage: TImage;
    ImageMemo: TMemo;
    ImageScrollBox: TScrollBox;
    Image1: TImage;
    ImageNamePanel: TPanel;
    ImageGroupBox: TGroupBox;
    RectGroupBox: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SpinEditLeft: TSpinEdit;
    SpinEditTop: TSpinEdit;
    SpinEditWidth: TSpinEdit;
    SpinEditHeight: TSpinEdit;
    PanelBackgroundColor: TPanel;
    GroupBox1: TGroupBox;
    SoundGroupBox: TGroupBox;
    PlayingSoundFileLabel: TLabel;
    BtnPlaySound: TButton;
    BtnStopSound: TButton;
    MenuItemNewPlaylist: TMenuItem;
    PlayListPopupMenu: TPopupMenu;
    MenuItemPlayListNew: TMenuItem;
    MenuItemPlayListOpen: TMenuItem;
    MenuItemPlayListDelete: TMenuItem;
    N8: TMenuItem;
    MenuItemPlayListRemoveItem: TMenuItem;
    MenuItemPlayListSubMenuAdd: TMenuItem;
    MenuItemPlayListAddFile: TMenuItem;
    MenuItemPlayListAddFolder: TMenuItem;
    N9: TMenuItem;
    MenuItemSubMenuPlayListAdd: TMenuItem;
    MenuItemPlayListAddFolder1: TMenuItem;
    MenuItemPlayListAddFile1: TMenuItem;
    N6: TMenuItem;
    OpenDialog1: TOpenDialog;
    MenuItemPlayListClear: TMenuItem;
    BtnImagePrior: TSpeedButton;
    BtnImageNext: TSpeedButton;
    BitBtnOpen: TBitBtn;
    OpenPopupMenu: TPopupMenu;
    MenuItemOpen: TMenuItem;
    MenuItemOpenCopy: TMenuItem;
    BitBtnGameBuiltinSolutions__: TBitBtn;
    LevelsFolderHistoryComboBox: TComboBox;
    LevelsFileHistoryComboBox: TComboBox;
    MenuItemCollectionSeparator: TMenuItem;
    MenuItemSubMenuSolutions: TMenuItem;
    MenuItemSolutionMoves: TMenuItem;
    MenuItemSolutionPushes: TMenuItem;
    MenuItemIncludeBuiltinSolutions: TMenuItem;
    N7: TMenuItem;
    MenuItemSolutionMovesAndPushes: TMenuItem;
    MenuItemSolutionPushesAndMoves: TMenuItem;
    MenuItemDragLevels: TMenuItem;
    MenuItemClipboardImportSeparator: TMenuItem;
    MenuItemImportFromClipboard: TMenuItem;
    MenuItemCopyLevelToClipboard: TMenuItem;
    MenuItemCopyCollectionLevelsToClipboard: TMenuItem;
    SkinScriptGroupBox: TGroupBox;
    SkinScriptsComboBox: TComboBox;
    BtnSkinScriptBrowse: TButton;
    SkinsComboBox: TComboBox;
    MenuItemNewTextFile: TMenuItem;
    SkinScriptPanel: TPanel;
    SkinScriptMemo: TMemo;
    BtnOpen2: TButton;
    BtnGameBuiltinSolutions: TButton;
    MenuItemAutoScroll: TMenuItem;
    MenuItemFileReorganize: TMenuItem;
    MenuItemFileReorganizeSeparator: TMenuItem;
    MenuItemBuiltinSolutions: TMenuItem;
    MenuItemBuiltinSolutionsSeparator: TMenuItem;
    MenuItemCopyCollectionSolutionsToClipboard: TMenuItem;
    MenuItemSubMenuFileConversion: TMenuItem;
    MenuItemFileConversionSokoMind: TMenuItem;
    MenuItemFileConversionMergeLevelFiles: TMenuItem;
    MenuItemFileConversionSokofan: TMenuItem;
    MenuItemFileConversionSokobanForWindows: TMenuItem;
    MenuItemSubMenuCopyToClipboard: TMenuItem;
    MenuItemSubMenuClipboardLevel: TMenuItem;
    MenuItemSubMenuClipboardCollection: TMenuItem;
    MenuItemCopySolutionToClipboard: TMenuItem;
    MenuItemCopySolutionMovesToClipboard: TMenuItem;
    MenuItemCopySolutionPushesToClipboard: TMenuItem;
    MenuItemImageText: TMenuItem;
    MenuItemImageSeparator: TMenuItem;
    TextFilePanel: TPanel;
    TextFileMemo: TMemo;
    MenuItemImageText2: TMenuItem;
    MenuItemImageTextSeparator2: TMenuItem;
    PanelToolTips: TPanel;
    SolverImage: TImage;
    PluginGroupBox: TGroupBox;
    BtnPluginSettings: TButton;
    BtnPluginAbout: TButton;
    PluginTimer: TTimer;
    PluginLevelGroupBox: TGroupBox;
    BtnSolveLevel: TButton;
    PanelPluginLevelInfo: TPanel;
    PluginLevelMemo: TMemo;
    PluginLevelStringGrid: TStringGrid;
    PluginLevelFileNamePanel: TPanel;
    MenuItemSubMenuImportFromClipboard: TMenuItem;
    MenuItemCreateLevelsUsingSolutionsFromClipboard: TMenuItem;
    N10: TMenuItem;
    MenuItemSubMenuCopyToClipboardFormat: TMenuItem;
    MenuItemCopyToClipboardFormatNormal: TMenuItem;
    MenuItemCopyToClipboardFormatRLE: TMenuItem;
    BtnOptimizeGames: TButton;
    MenuItemSubMenuFindDuplicateLevels: TMenuItem;
    MenuItemFindDuplicateLevelsCurrentLevel: TMenuItem;
    MenuItemFindDuplicateLevelsAllLevels: TMenuItem;
    MenuItemSubMenuFindDuplicateLevelsFolders: TMenuItem;
    N11: TMenuItem;
    MenuItemDuplicateLevelsFolderSeparator: TMenuItem;
    MenuItemDuplicateLevelsDefaultLevelFolder: TMenuItem;
    N12: TMenuItem;
    MenuItemDuplicateLevelsAddFolder: TMenuItem;
    MenuItemDuplicateLevelsClearFolderList: TMenuItem;
    MenuItemFindDuplicateLevelsCurrentCollection: TMenuItem;
    N13: TMenuItem;
    MenuItemSubMenuFindDuplicateLevelsMatchOptions: TMenuItem;
    MenuItemFindDuplicateLevelsMatchOptionsBoxes: TMenuItem;
    MenuItemFindDuplicateLevelsMatchOptionsGoals: TMenuItem;
    MenuItemFindDuplicateLevelsMatchOptionsInteriorWalls: TMenuItem;
    MenuItemFindDuplicateLevelsMatchOptionsExteriorWalls: TMenuItem;
    N14: TMenuItem;
    MenuItemFindDuplicateLevelsMatchOptionsReset: TMenuItem;
    MenuItemFindDuplicateLevelsMoreSettings: TMenuItem;
    N15: TMenuItem;
    MenuItemDuplicateLevelsCurrentLevelFolder: TMenuItem;
    AnythingButLevelsFolderHistoryComboBox: TComboBox;
    AnythingButLevelsFileHistoryComboBox: TComboBox;
    MenuItemInitializeSkinForCaptureTool: TMenuItem;
    N16: TMenuItem;
    MenuItemCopyCollectionToClipboardWriteTitleAndAuthor: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    MenuItemFileConversionNormalizeBoard: TMenuItem;
    MenuItemFileConversionNormalizeBoardMakeRectangularBoard: TMenuItem;
    MenuItemInitializeSkinWithColumnsAndRows: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnOpen2Click(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FileListBox1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DirectoryListBox1MouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FileListBox1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CollectionStringGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; AState: TGridDrawState);
    procedure CollectionStringGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure CollectionStringGridEnter(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditPopupMenuPopup(Sender: TObject);
    procedure MenuItemEditClick(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
    procedure MenuItemCutClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure MenuItemTextColorClick(Sender: TObject);
    procedure MenuItemBackgroundColorClick(Sender: TObject);
    procedure MenuItemFontClick(Sender: TObject);
    procedure MemoEnter(Sender: TObject);
    procedure MemoExit(Sender: TObject);
    procedure MemoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnEditMenuClick(Sender: TObject);
    procedure BtnEditMenuMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure BtnFileMenuClick(Sender: TObject);
    procedure FileListBox1Enter(Sender: TObject);
    procedure FileListBox1Exit(Sender: TObject);
    procedure BtnFileMenuMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FilePopupMenuPopup(Sender: TObject);
    procedure DirectoryListBox1Enter(Sender: TObject);
    procedure MenuItemFileCopyClick(Sender: TObject);
    procedure MenuItemFileMoveClick(Sender: TObject);
    procedure MenuItemFileDeleteClick(Sender: TObject);
    procedure MenuItemFileRenameClick(Sender: TObject);
    procedure MenuItemNewFolderClick(Sender: TObject);
    procedure MenuItemNewLevelCollectionClick(Sender: TObject);
    procedure DirectoryListBox1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FileListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DirectoryListBox1DragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DirectoryListBox1DragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure FileListBox1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FileListBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CollectionStringGridMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CollectionStringGridMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CollectionStringGridDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure CollectionStringGridDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure GameSolutionRadioButtonClick(Sender: TObject);
    procedure DriveComboBox1Change(Sender: TObject);
    procedure BtnPlaySoundClick(Sender: TObject);
    procedure BtnStopSoundClick(Sender: TObject);
    procedure SpinEditRectChange(Sender: TObject);
    procedure SpinEditRectEnter(Sender: TObject);
    procedure SpinEditRectExit(Sender: TObject);
    procedure SpinEditLeftKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PanelBackgroundColorEndDrag(Sender, Target: TObject; X,
      Y: Integer);
    procedure PanelBackgroundColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelBackgroundColorMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Image1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemPlayListAddFileClick(Sender: TObject);
    procedure MenuItemPlayListAddFolderClick(Sender: TObject);
    procedure MenuItemPlayListRemoveItemClick(Sender: TObject);
    procedure MenuItemPlayListNewClick(Sender: TObject);
    procedure MenuItemPlayListOpenClick(Sender: TObject);
    procedure MenuItemPlayListDeleteClick(Sender: TObject);
    procedure PlayListPopupMenuPopup(Sender: TObject);
    procedure CollectionStringGridKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuItemPlayListClearClick(Sender: TObject);
    procedure DriveComboBox1DropDown(Sender: TObject);
    procedure BtnImagePriorClick(Sender: TObject);
    procedure BtnImageNextClick(Sender: TObject);
    procedure CollectionStringGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FileListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BitBtnOpenClick(Sender: TObject);
    procedure BtnOpenCopyClick(Sender: TObject);
    procedure BtnGameBuiltinSolutionsClick(Sender: TObject);
    procedure FolderHistoryComboBoxChange(Sender: TObject);
    procedure FolderHistoryComboBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FileHistoryComboBoxChange(Sender: TObject);
    procedure FileHistoryComboBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuItemSolutionMovesAndOrPushesClick(Sender: TObject);
    procedure MenuItemIncludeBuiltinSolutionsClick(Sender: TObject);
    procedure CollectionStringGridTopLeftChanged(Sender: TObject);
    procedure MenuItemDragLevelsClick(Sender: TObject);
    procedure MenuItemCopyCollectionToClipboardClick(Sender: TObject);
    procedure MenuItemCopyLevelToClipboardClick(Sender: TObject);
    procedure MenuItemImportFromClipboardClick(Sender: TObject);
    procedure MenuItemCreateLevelsUsingSolutionsFromClipboardClick(
      Sender: TObject);
    procedure BtnSkinScriptBrowseClick(Sender: TObject);
    procedure SkinScriptsComboBoxChange(Sender: TObject);
    procedure SkinsComboBoxEnter(Sender: TObject);
    procedure SkinScriptsComboBoxEnter(Sender: TObject);
    procedure MenuItemNewTextFileClick(Sender: TObject);
    procedure FileListBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DirectoryListBox1MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemAutoScrollClick(Sender: TObject);
    procedure CollectionStringGridMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FileListBox1Click(Sender: TObject);
    procedure MenuItemFileReorganizeClick(Sender: TObject);
    procedure MenuItemFileConversionClick(Sender: TObject);
    procedure FilterComboBox1Change(Sender: TObject);
    procedure MenuItemCopySolutionToClipboardClick(Sender: TObject);
    procedure MenuItemImageTextClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TextFileMemoChange(Sender: TObject);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton;
       Shift: TShiftState; X, Y: Integer);      
    procedure FolderHistoryComboBoxDropDown(Sender: TObject);
    procedure HistoryComboBoxDropDown(Sender: TObject);
    procedure BtnPluginSettingsClick(Sender: TObject);
    procedure BtnPluginAboutClick(Sender: TObject);
    procedure BtnPluginClick(Sender: TObject);
    procedure PluginTimerTimer(Sender: TObject);
    procedure PluginLevelStringGridDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure PluginLevelFileNamePanelMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure PluginLevelStringGridMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemCopyToClipboardFormatClick(Sender: TObject);
    procedure MenuItemFindDuplicateLevelsClick(
      Sender: TObject);
    procedure MenuItemDuplicateLevelsAddFolderClick(Sender: TObject);
    procedure MenuItemDuplicateLevelsClearFolderListClick(Sender: TObject);
    procedure MenuItemDuplicateLevelsFolderClick(
      Sender: TObject);
    procedure MenuItemFindDuplicateLevelsMatchOptionsClick(
      Sender: TObject);
    procedure MenuItemFindDuplicateLevelsMoreSettingsClick(
      Sender: TObject);
    procedure HistoryComboBoxDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure MenuItemInitializeSkinForCaptureToolClick(Sender: TObject);
    procedure MenuItemCopyCollectionToClipboardWriteTitleAndAuthorClick(
      Sender: TObject);
//  procedure DataStringGridDrawCell(Sender: TObject; ACol, ARow: Integer;
//    ARect: TRect; AState: TGridDrawState);
  protected
    FloppyBMP, FixedBMP, NetworkBMP, CDROMBMP, RAMBMP: TBitmap;
    procedure ApplicationOnDeactivate(Sender: TObject);
    procedure ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
    function  GetSolutionColumns:Integer;
    procedure Junk(Sender: TObject);
    procedure OnCloseWindow(var Msg: TMessage); Message Misc_.MSG_CLOSE;
    procedure OnMaximize(var Msg:TMessage); Message MSG_MAXIMIZE;
    procedure SetSolutionColumns(SolutionColumns__:Integer);
  private
    { Private declarations }
    ImageAntialiasingEnabled:Boolean;
    CollectionGroupBoxMinimumHeight:Integer;
    CurrentCollectionCheckedLevelsCountDown:Integer;
    CurrentCollectionSolvedLevelsCount:Integer;
    CurrentMemo:TMemo;
    CurrentFileStringGridRow:Integer;
    CurrentSectionName:String;
    DriveHistory:TStringList;
    EnterKeyDown:Boolean;
    FilterIndexGame,
    FilterIndexImage,
    FilterIndexPalette,
    FilterIndexSkin,
    FilterIndexSkinType,
    FilterIndexPlugin,
    FilterIndexSound:Integer;
    GameFileGroupBoxMinimumWidth:Integer;
    FileManagerTasksHaveBeenPerformed:Boolean;
    IsLoading:Boolean;
    IsModified:Boolean;
    LastOpenedPackFileName:String;
    OnDirectoryChangeLoadFirstFileOnFileList:Boolean;
    LoadPackFileIndexCount:Integer;
    MouseButtonDown:Boolean;
    Music:TMusicmanager;
    oApplicationOnDeactivate:TNotifyEvent;
    oMusicOnNotify:TNotifyEvent;
    SelectionEnabled:Boolean;
    ShortCutsEnabled:Boolean; // 'False' after changing file in 'FileListBox1' until the user presses [Ctrl] again
    State:TState;
    SubTask:TOpenSubTask;

    BaseRect:TRect;
    DirectoryListBox1VisibleRowCount: Integer;
    DefaultImageFileName:String;
    DefaultPaletteFileName:String;
    DefaultRect:TRect;
    DragOriginPoint : TPoint;
    DragPoint : TPoint;
    DragRect : TRect;
    DragRectVisible : Boolean;
    FileListBox1VisibleRowCount: Integer;
    FixedDefaultImageSize:Boolean;
    IsDragging,
    IsZooming : Boolean;
    oSpinEditValue:Integer;
    SizeHandle : TSizeHandle;
    SwSizing : Boolean;
    SwZoomIn : Boolean;
    WindowsDrive:Char;

    function  CanUnpackFile(const FileName__:String):Boolean;
    procedure ClearSolutionStatistics;
    procedure ClipDragRectToImage;
    function  CopyCollectionToClipboard(WriteTitleAndAuthor__,CopyAllSnapshots__,CopySolutionsOnly__,RunLengthEncoding__:Boolean):Boolean;
    function  CopyLevelToClipboard(OppositeFillFloorsSetting__,RunLengthEncoding__,BoardOnly__,CurrentBoardOnly__:Boolean):Boolean;
    function  CopySolutionToClipboard(SolutionPushes__,RunLengthEncoding__:Boolean):Boolean;
    procedure DeleteCurrentCollectionStringGridItem;
    function  DiskCurrentItem:String;
    function  DiskCurrentItemType:TFileTaskItemType;
    procedure DisplayBuiltinImage;
    procedure DoNotSplitImageFileGroupBox;
    procedure DrawRect(const Rect: TRect; PenMode: TPenMode);
    procedure DriveComboBoxBuildList(var DriveComboBox: TDriveComboBox);
    procedure DriveComboBoxReadBitmaps;
    procedure EnableDisableSoundControls;
    function  GetBitMapText:Boolean;
    function  HasSolutionsForTheOptimizer(Sender: TObject):Boolean;
    procedure HideDragRect;
    procedure ImageResize;
    function  IsMediaPresent(Drive: Char):Boolean;
    procedure LoadGameData(LoadNotes:Boolean);
    procedure LoadOrMakeNewPlayListFile;
    function  LoadPalette(const FileName__:String):Boolean;
    function  LoadPlugin(const FileName__:String; Plugin__:TPlugin):Boolean;
    procedure MediaPlayerNotify(Sender: TObject);
    function  PlayListAddItem(Index: Integer; const Item: String): Boolean;
    function  PlayListLoadFromFile(const FileName:String):Boolean;
    function  PlaySound: Boolean;
    procedure ShowGame;
    function  SkinTypeDetection(const Directory__:String):Boolean;
    function  TryToLoadGame(const FileName:String):Boolean;
    procedure ShowHint(Sender: TObject);
    procedure UpdateAfterFileManagerTask(const FileName__:String);
    function  ReloadCurrentGameIfNecessary:Boolean;
    procedure ResetDragRect;
    function  ResizeDiskGroupBox(DiskGroupBoxWidth__,DiskGroupBoxHeight__:Integer):Boolean;
    function  ResizeFolderListBox(FolderListBoxWidth__:Integer):Boolean;    
    function  ResizeGameBoardPanel(GameBoardPanelHeight__:Integer):Boolean;
    procedure ResizePluginLevelGroupBox;
    procedure ResizeSkinScriptGroupBox;
    function  SaveCurrentImageInBMPFormat( var ErrorMessage__ : String ) : Boolean;
    procedure SetFileFilter(const Filter__:String);
    procedure SetState(State__: TState);
    procedure ScrollDragRectInView;
    procedure ShowDragRect(const Rect: TRect);
    procedure ShowPicture(Pict:TPicture; View:TImageView; FillColor:TColor; IsABuiltinImage:Boolean);
    procedure ShowPluginCaption(const PluginTypeName__,VisualFileName__:String);
    procedure ShowSolutionsForVisibleLevels;
    procedure SetBackgroundColor(Color: TColor);
    procedure SetCollectionStringGridColumnWidths;
    function  ToggleMenuItemText:Boolean;
    function  TryToLoadImage(const FileName__: String): Boolean;
    procedure UpdateCollectionHintText;
  public
    { Public declarations }
    ActiveList:TWinControl;
    BaseFileName:String;
    CollectionHighlightBackgroundColor,CollectionHighlightTextColor:TColor;
    CurrentCollectionFileName:String;
    CurrentFileName:String;
    CurrentImageFileName:String;
    DiskGroupBoxMinimumHeight:Integer;
    DiskGroupBoxMinimumWidth:Integer;
    DoMaximize:Boolean; // kludge: if 'True' then don't resize the form components in 'FormResize'
    EscapeEnabled:Boolean;
    FolderListBoxMinimumWidth:Integer;
    Game:TGame;
    GameDataGroupBoxMinimumHeight:Integer;
    GameViewer:TGameViewer;
    PlayListOk:Boolean;
    Task:TOpenTask;
    Texts:array[TTextType] of TText;
    ToolTips:TToolTips;
    procedure BoardResize(GameBoardImage:TImage);
    function  CalculateMinHeight:Integer;
    function  CalculateMinWidth:Integer;
    procedure ClearText(TextType__:TTextType);
    function  CloseFile:Boolean;
    function  CloseText(TextType__:TTextType):Boolean;
    function  CopyCollectionToClipboard0(SokoFile__:TSokoFile; const FileName__,Text__:String; WriteTitleAndAuthor__,CopyAllSnapshots__,CopySolutionsOnly__,RunLengthEncoding__,CopyLevelNotes__,ShowResult__:Boolean; var Count__:Integer):Boolean;
    procedure EnableDisablePluginControls(Sender: TObject; Plugin__:TPlugin; ClearStatus__:Boolean);
    procedure ExchangeGames(var Game__:TGame);
    function  FileListBox1FileName:String;
    function  FlushText(TextType__:TTextType):Boolean;
    function  InitTask(Task__: TOpenTask; SubTask__:TOpenSubTask;
                       const Caption__, TopPanelCaption__, FileName__,Filter__,DefaultImageFileName__: String;
                       const Rect__: TRect; BackgroundColor__: TColor;
                       FixedDefaultImageSize__,SelectionEnabled__: Boolean; const DefaultRect__: TRect):Boolean;
    function  LoadImage(const FileName__: String): Boolean;
    function  LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    function  LoadTextFromFile(const FileName__:String; TextType__:TTextType):Boolean;
    function  FindPriorNextFileName(const FileName:String; Prior:Boolean; var ItemIndex:Integer):String;
    function  GetRectItem(Index:Integer):Integer;
    function  GetSubDirectories(const Path__,AbbreviatedPath__:String; SubDirectoryList__:SokUtil_.TList):Boolean;
    procedure OnActivate;
    procedure OnFontChange;
    function  PluginForTask(Task__:TOpenTask):TPlugin;
    function  SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
    procedure SetFileFilterIndex(const FileName:String);
    procedure SetPluginButtonState(Button__:TButton; PluginButtonState__:TPluginButtonState);
    procedure ShowSolverMovesAndPushes(PluginLevelStringGrid__:TStringGrid; MoveCount__,PushCount__:Integer);
    property  SolutionColumns:Integer read GetSolutionColumns write SetSolutionColumns;
    function  StrSubstituteInFiles(const Path__,Mask__,IniFileSection__:String; const OldStrings__,NewStrings__:array of String):Integer;
    procedure SynchronizedPluginCallback(Plugin__:TPlugin; PluginSucceeded__,IsIdle__:Boolean);
    function  TryToLoadFile(const FileName:String):Boolean;
    function  TryToSetDirectory(const Directory__:String):Boolean;
    function  UnpackFile(const FileName__:String):Boolean;
  end;

var
  OpenForm: TOpenForm = nil;

implementation

{$R *.DFM}

uses Clipbrd,
     Res_,SokGame_,Text_,Main_,Help_,
     File_,BitMap_,Fractal_,Snapshots_,MPlayer1_,Options_,Skins_,Tools_,
     MPlayer2_,Generator_, Duplicates_,PNG_, Capture_;

const
  DEFAULT_COLLECTION_BACKGROUND_COLOR
                                 = COLOR_LEMONCHIFFON;
//DEFAULT_COLLECTION_HIGHLIGHT_BACKGROUND_COLOR // use 'clHighlight' and 'clHighlightText' instead of customized highlight colors, so selected items in the file list box, the directory list box, and the collection string grid look the same
//                               = TColor($C56A31); // BGR, not RGB
//DEFAULT_COLLECTION_HIGHLIGHT_TEXT_COLOR
//                               = clWhite;
  DEFAULT_COLLECTION_TEXT_COLOR  = clBlack;
  DEFAULT_DRAGRECT_WIDTH         = 50;
  DEFAULT_TOOL_TIPS_OFFSET_PIXELS= 16; // pixels
  FORMAT_XY_INT                  = '[%d:%d]';
  GameInfoSection                = 'GameInfo';         // don't localize
  MAX_IMAGE_ANTIALIASING_TIME_MS = 100; // milliseconds
  PANEL_X_GAP                    = 12;
  PANEL_Y_GAP                    = 6;
  PLUGIN_OPTIMIZER_FLAG          = 1; // PLUGIN_XXX_FLAG values must be 2^n integers so they can be OR'ed together
  PLUGIN_SOLVER_FLAG             = 2;
  SIZERECT_DELTA                 = 4;
  SkinScriptSection              = 'SkinScriptEditor'; // don't localize
  SOKOBAN_FOR_WINDOWS_LEVEL_FILE_NAME_EXT
                                 = '.slc';
  SOLUTION_FONT_SIZE             = 8;
  TextFileSection                = 'TextFileEditor';   // don't localize

function ExportBestSolutionsToFile(SokFile__:TSokoFile; Game__:TGame; const FileName__:String; RunLengthEncoding__:Boolean; var Count__:Integer):Boolean;
var IOResult:Integer; s,Author,AuthorFromFileNotes:String; Level:TLevel; F:TextFile;

  function WriteSnapshotToFile(Snapshot__:TSnapshot):Boolean;
  var SnapshotAsText:TSnapshotAsText;
  begin // returns 'False' if writing the snapshot fails; otherwise 'True' is returned; note that this includes a 'nil' snapshot
    Result:=True;
    if Snapshot__<>nil then
       try SnapshotAsText:=Snapshot__.MakeSnapshotAsText(True,RunLengthEncoding__,False,True);
           if SnapshotAsText<>nil then
              try
                if Count__<>0 then Writeln(F,'');
                Inc(Count__);

                SnapshotAsText.WriteToFile(Addr(F));
                Writeln(F,KEY_TITLE ,COLON,SPACE,Level.Name);
                Writeln(F,KEY_AUTHOR,COLON,SPACE,Author);

              finally SnapshotAsText.Free;
              end
           else raise Exception.Create(TEXT_MEMORY_FULL);
       except on E:Exception do Result:=Error(E.Message,TEXT_APPLICATION_TITLE);
       end;
  end;

begin // ExportBestSolutionsToFile
  Result:=False; Count__:=0; IOResult:=0; AuthorFromFileNotes:='';
  if (SokFile__<>nil) and (Game__<>nil) then with SokFile__ do
     try     FileHeader.Lines.ReadString(KEY_AUTHOR,AuthorFromFileNotes);

             AssignFile(F,FileName__);
             Rewrite(F);
             try     Level:=TLevel(Levels.First); Result:=True;
                     while (Level<>nil) and Result do begin
                       if not Level.SnapshotsAsText.IsEmpty then begin
                          Game__.Clear;
                          Result:=Game__.LoadFromFile(Level.Name,SokFile__,True,s);
                          if Result then with Game__ do begin
                             if not Game__.Notes.Lines.ReadString(KEY_AUTHOR,Author) then
                                Author:=AuthorFromFileNotes;

                             Result:=WriteSnapshotToFile(BestSolutionMoves) and
                                     WriteSnapshotToFile(BestSolutionPushes) and
                                     WriteSnapshotToFile(BuiltinBestSolutionMoves) and
                                     WriteSnapshotToFile(BuiltinBestSolutionPushes);
                             end;
                          end;
                       Level:=TLevel(Level.Next);
                       end;
             finally CloseFile(F);
                     Inc(IOResult,System.IOResult);
                     Result:=(IOResult=0) and (Level=nil) and Result; // 'nil': all levels has been processed
                     if not Result then SysUtils.DeleteFile(FileName__);

             end;
  except on E:Exception do
            Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE);
  end;
end; {ExportBestSolutionsToFile}

procedure TOpenForm.FormCreate(Sender: TObject);
var i,W:Integer; s:String; Buf:array[0..MAX_PATH+1] of Char;
begin
  OnFontChange;
  if biMinimize in BorderIcons then BorderIcons:=BorderIcons-[biMinimize]; // minimize: it may be impossible for the user to bring the application back on the screen again (a Microsoft Windows operating system bug when the general Windows text size settings is > 100%)
  Caption:=Application.Title+SUB_TITLE_SEPARATOR+Caption;
  if   Length(MainForm.ApplicationDataPath)>1 then
       DriveComboBox1.Drive:=MainForm.ApplicationDataPath[1]
  else DriveComboBox1.Drive:=Application.ExeName[1];
  if   DirectoryExists(StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath)) then
       DirectoryListBox1.Directory:=MainForm.ApplicationDataPath
  else DirectoryListBox1.Directory:=ExtractFilePath(Application.ExeName);
  GameLoadErrorMemo.Align:=alClient; GameLoadErrorMemo.WordWrap:=True;
  ImageScrollBox.BorderStyle:=bsNone;
  ImageScrollBox.Align:=alClient;
  ImageMemo.ScrollBars:=ssNone; ImageMemo.Align:=alClient;
  SkinScriptMemo.Align:=alClient; TextFileMemo.Align:=alClient;
  PanelToolTips.Visible:=False; PanelToolTips.Caption:=''; PanelToolTips.BringToFront;
  ToolTips.Enabled:=True;
  ToolTips.OffsetX:=DEFAULT_TOOL_TIPS_OFFSET_PIXELS;
  ToolTips.OffsetY:=DEFAULT_TOOL_TIPS_OFFSET_PIXELS;
  ImageGroupBox.BringToFront;
  OnDirectoryChangeLoadFirstFileOnFileList:=True;

  GameFileGroupBox.Left:=DiskGroupBox.Left+DiskGroupBox.Width+PANEL_X_GAP;
  ImageFileGroupBox.Left:=GameFileGroupBox.Left;
  CollectionGroupBox.Top:=DiskGroupBox.Top+DiskGroupBox.Height+PANEL_X_GAP;
  with CollectionGroupBox do Height:=StatusBar1.Top-Left-Top;
  ResizePluginLevelGroupBox;

  s:=Misc_.BoardDimensionsAndBoxesAndFloorsAsText(MAX_BOARD_WIDTH,MAX_BOARD_HEIGHT,MAX_BOX_COUNT,MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT,bdColRow,True);
  W:=Canvas.TextWidth(s+'MQZ');
  with StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE] do
    if W>Width then Width:=W; // ensure there is room enough in the status field for the "[Width x Height - Boxes - Floors]" text

  Task:=otNone; IsLoading:=False;

  LevelsFolderHistoryComboBox.Tag                       :=LevelsFolderHistoryComboBox.Width; // 'Tag' is used to store dropdown width
  LevelsFolderHistoryComboBox.DropDownCount             :=Min(25,MAX_FILE_HISTORY_ITEMS);

  DirectoryListBox1          .Top                       :=LevelsFolderHistoryComboBox.Top+LevelsFolderHistoryComboBox.Height+1;
  DirectoryListBox1          .Height                    :=DriveLabel.Top-8-DirectoryListBox1.Top;
  DirectoryListBox1VisibleRowCount                      :=DirectoryListBox1.ClientHeight div DirectoryListBox1.ItemHeight;

  LevelsFileHistoryComboBox  .Tag                       :=LevelsFileHistoryComboBox.Width;  // 'Tag' is used to store dropdown width
  LevelsFileHistoryComboBox  .DropDownCount             :=LevelsFolderHistoryComboBox.DropDownCount;

  FileListBox1               .Top                       :=DirectoryListBox1.Top;
  FileListBox1               .Height                    :=DirectoryListBox1.Height;
  FileListBox1VisibleRowCount                           :=FileListBox1.ClientHeight div FileListBox1.ItemHeight;

  AnythingButLevelsFolderHistoryComboBox.Tag            :=AnythingButLevelsFolderHistoryComboBox.Width; // 'Tag' is used to store dropdown width
  AnythingButLevelsFolderHistoryComboBox.DropDownCount  :=LevelsFolderHistoryComboBox.DropDownCount;
  AnythingButLevelsFolderHistoryComboBox.Top            :=LevelsFolderHistoryComboBox.Top;

  AnythingButLevelsFileHistoryComboBox  .Tag            :=AnythingButLevelsFileHistoryComboBox.Width;  // 'Tag' is used to store dropdown width
  AnythingButLevelsFileHistoryComboBox  .DropDownCount  :=LevelsFileHistoryComboBox.DropDownCount;
  AnythingButLevelsFileHistoryComboBox  .Top            :=LevelsFileHistoryComboBox.Top;

  with ImageGroupBox do begin
    Left:=CollectionGroupBox.Left;
    Top :=CollectionGroupBox.Top;
    end;

  with SkinScriptGroupBox do begin
    Left:=CollectionGroupBox.Left;
    Top :=CollectionGroupBox.Top;
    Tag :=Width; // 'Tag' is used for storing dropdown width
    end;

  with ImageFileGroupBox do begin
    Left:=GameFileGroupBox.Left;
    Top :=GameFileGroupBox.Top;
    end;

  SoundGroupBox.Left:=ImageNamePanel.Left;

  with CollectionStringGrid do Col:=1;

  Game:=nil; GameViewer:=nil; DriveHistory:=nil;
  try    Game        :=TGame.Create; Game.ShowGame:=ShowGame;
         GameViewer  :=TGameViewer.Create; GameViewer.LegalMovesInfo.Enabled:=False;
         DriveHistory:=TStringList.Create;
  except on E:Exception do begin
         Game.Free; GameViewer.Free; DriveHistory.Free;
         Game:=nil; GameViewer:=nil; DriveHistory:=nil;
         Error(E.Message,'');
         end;
  end;

  if (Game=nil) or (GameViewer=nil) or (DriveHistory=nil) then Halt(1);

  DriveComboBoxReadBitmaps;

  i:=GetWindowsDirectory(Buf,SizeOf(Buf));
  if   (i=0) or (i>SizeOf(Buf)-SizeOf(Char)) then
       WindowsDrive:=#0
  else WindowsDrive:=LoCase(Buf[0]);

  Game.HourGlassCursor:=False; Game.Verbose:=False;
  CurrentFileName:=''; LastOpenedPackFileName:=''; LoadPackFileIndexCount:=0;
  CurrentCollectionFileName:=''; CurrentSectionName:=''; CurrentImageFileName:='';

  with CollectionStringGrid do begin
    SetCollectionStringGridColumnWidths;
    DefaultRowHeight:=Canvas.TextHeight(FONT_HEIGHT_TEST_STRING)+4;
    ClientHeight:=4*(DefaultRowHeight+GridLineWidth);
    Font.Color:=DEFAULT_COLLECTION_TEXT_COLOR;
    Color:=DEFAULT_COLLECTION_BACKGROUND_COLOR;
    CollectionHighlightBackgroundColor:=clHighlight; //DEFAULT_COLLECTION_HIGHLIGHT_BACKGROUND_COLOR;
    CollectionHighlightTextColor:=clHighlightText; //DEFAULT_COLLECTION_HIGHLIGHT_TEXT_COLOR;
    end;
  GameSolutionMovesRadioButton.Left:=GameSolutionTextLabel.Left;
  GameSolutionMovesRadioButton.Top :=GameSolutionTextLabel.Top;

  BtnFileMenu.Left:=BtnEditMenu.Left;
  BtnFileMenu.Top :=BtnEditMenu.Top;

  BtnCancel.Left := BtnOpen2 .Left + BtnOpen2 .Width + BtnOpen.Left;
  BtnHelp  .Left := BtnCancel.Left + BtnCancel.Width + BtnOpen.Left;

  LoadGameData(True);

  CurrentMemo:=GameInfoMemo;
  GameInfoMemo.ReadOnly:=True; SkinScriptMemo.ReadOnly:=True; TextFileMemo.ReadOnly:=True;
  MenuItemEdit.Checked:=GameInfoMemo.ReadOnly; EditPopupMenuPopup(nil);

  FilterIndexGame:=0; FilterIndexImage:=0; FilterIndexPalette:=0;
  FilterIndexSkin:=1; FilterIndexSkinType:=0; FilterIndexPlugin:=0; FilterIndexSound:=0;
  CurrentFileStringGridRow:=-1; SolutionColumns:=1;
  MenuItemDragLevels.Checked:=False; MenuItemAutoScroll.Checked:=False;
  MenuItemCopyCollectionToClipboardWriteTitleAndAuthor.Checked := True;
  IsModified:=False; FileManagerTasksHaveBeenPerformed:=False; EscapeEnabled:=True;
  DragRectVisible:=False; ResetDragRect; SelectionEnabled:=False;
  DefaultImageFileName:=MainForm.ApplicationDataPath+DEFAULT_VALUE+BMP_FILE_EXT;
  DefaultPaletteFileName:=Fractals.DefaultPaletteFileName;
  SetPluginButtonState(BtnSolveLevel,pbsRun);
  SetPluginButtonState(BtnOptimizeGames,pbsRun);
  PluginTimer.Enabled:=False; PluginTimer.Interval:=PLUGIN_TIMER_INTERVAL_MS;
  MenuItemCopyToClipboardFormatClick(MenuItemCopyToClipboardFormatNormal);
  DiskGroupBoxMinimumHeight:=DiskGroupBox.Height;
  DiskGroupBoxMinimumWidth:=DiskGroupBox.Width;
  FolderListBoxMinimumWidth:=DirectoryListBox1.Width;  
  GameFileGroupBoxMinimumWidth:=BtnHelp.Left+BtnHelp.Width+BtnOpen.Left;
  GameFileGroupBox.Width:=GameFileGroupBoxMinimumWidth;
  CollectionGroupBoxMinimumHeight:=140;
  CollectionGroupBox.Height:=CollectionGroupBoxMinimumHeight;
  GameDataGroupBoxMinimumHeight:=CollectionGroupBoxMinimumHeight-BtnOpen.Height-(2*8);
  MouseButtonDown:=False; oApplicationOnDeactivate:=nil; DoMaximize:=False;
  MenuItemFileConversionNormalizeBoard.Hint := HintNormalizeBoardText;
  MenuItemFileConversionNormalizeBoardMakeRectangularBoard.Hint := HintNormalizeBoardMakeRectangularBoardText;

  MenuItemDuplicateLevelsDefaultLevelFolder.Caption:=ApplicationName+SPACE+LevelFolderText;
  MenuItemDuplicateLevelsDefaultLevelFolder.Checked:=True;
  MenuItemDuplicateLevelsCurrentLevelFolder.Hint   :=MenuItemDuplicateLevelsDefaultLevelFolder.Hint;
  MenuItemDuplicateLevelsCurrentLevelFolder.Checked:=True;

  MenuItemFindDuplicateLevelsMatchOptionsClick(MenuItemFindDuplicateLevelsMatchOptionsReset);
  MenuItemFindDuplicateLevelsMatchOptionsGoals.Hint:=MenuItemFindDuplicateLevelsMatchOptionsBoxes.Hint;
  MenuItemFindDuplicateLevelsMatchOptionsInteriorWalls.Hint:=MenuItemFindDuplicateLevelsMatchOptionsBoxes.Hint;
  MenuItemFindDuplicateLevelsMatchOptionsExteriorWalls.Hint:=MenuItemFindDuplicateLevelsMatchOptionsBoxes.Hint;

  with Texts[tGameNotes ] do begin FileName:=''; Memo:=GameInfoMemo;   end;
  with Texts[tSkinScript] do begin FileName:=''; Memo:=SkinScriptMemo; end;
  with Texts[tTextFile  ] do begin FileName:=''; Memo:=TextFileMemo;   end;

  TextFilePanel.BringToFront; // otherwise, foreign skin scripts may not show up in the window

  for i:=0 to Pred(ComponentCount) do
      if      Components[i] is TPanel       then with Components[i] as TPanel       do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TLabel       then with Components[i] as TLabel       do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TButton      then with Components[i] as TButton      do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TToolButton  then with Components[i] as TToolButton  do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TRadioButton then with Components[i] as TRadioButton do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TComboBox    then with Components[i] as TComboBox    do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TGroupBox    then with Components[i] as TGroupBox    do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TListBox     then with Components[i] as TListBox     do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TMemo        then with Components[i] as TMemo        do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TEdit        then with Components[i] as TEdit        do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TImage       then with Components[i] as TImage       do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TScrollBox   then with Components[i] as TScrollBox   do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TStringGrid  then with Components[i] as TStringGrid  do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TStatusBar   then with Components[i] as TStatusBar   do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TToolBar     then with Components[i] as TToolBar     do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TPageControl then with Components[i] as TPageControl do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
      else if Components[i] is TTabSheet    then with Components[i] as TTabSheet    do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end
//    else if Components[i] is TTrackBar    then with Components[i] as TTrackBar    do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end // 'TTrackBar' doesn't have 'OnMouseMove' and 'OnMouseUp', unfortunately
      else if Components[i] is TProgressBar then with Components[i] as TProgressBar do begin if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=ControlMouseUp; end;
  PanelToolTips.OnMouseMove:=nil; // otherwise, the tool tip panel may turn up blank when the mouse hovers over it

  i:=CalculateMinWidth;
  if Width <i then begin DoMaximize := True; Width :=i; DoMaximize:=False; end;
  i:=CalculateMinHeight;
  if Height<i then begin DoMaximize := True; Height:=i; DoMaximize:=False; end;
  Left:=Max(0,(Screen.Width -Width ) div 2);
  Top :=Max(0,(Screen.Height-Height) div 2);
end;

procedure TOpenForm.FormDestroy(Sender: TObject);
begin
  if Game<>nil then begin Game.SokoFile:=nil; Game.Clear; end; // avoid that 'Game'  tries to save current game
  DriveHistory.Free;
  Game.Free; Game:=nil;
  GameViewer.Free; GameViewer:=nil;
  if Assigned(MainForm) and (MainForm.Skins<>nil) then with MainForm.Skins do begin
     ClearSkins; ClearScripts; // uses comboboxes here on 'OpenForm', therefore they must be cleared here
     end;
  PluginTimer.Enabled:=False;

  FloppyBMP.Free;
  FixedBMP.Free;
  NetworkBMP.Free;
  CDROMBMP.Free;
  RAMBMP.Free;

  OpenForm:=nil;
end;

procedure TOpenForm.FormActivate(Sender: TObject);
var s:String;
begin
  if Assigned(DuplicatesForm) and (DuplicatesForm.Visible or (DuplicatesForm.ScanState=ssCloseWindow)) then begin // 'True': the focus changed back from the duplicates form to this form
     if DuplicatesForm.IsBusy then DuplicatesForm.SetFocus
     else begin
        if not DuplicatesForm.DuplicateLevelsStringGrid.Visible then DuplicatesForm.Hide; // 'True': no levels listed on the duplicates form; close it
        TryToLoadFile(CurrentFileName);
        end;
     end
  else begin
     MainForm.FormDeactivate(Sender);
     ModalResult:=mrNone;
     EnterKeyDown:=False; ShortCutsEnabled:=False;
     OnActivate;
     Game.HourGlassCursor:=True; Game.Verbose:=False;
     FileListBox1.Mask := FilterComboBox1.Mask;
     FilterComboBox1.Perform(CB_SETDROPPEDWIDTH,
       Max(FilterComboBox1.Width,
           Canvas.TextWidth(System.Copy(FilterComboBox1.Filter,1, // 'Canvas': 'FilterComboBox1.Canvas' doesn't allow '.TextWidth' do draw, hence, use another canvas with same font
                                        AnsiPos(BAR,FilterComboBox1.Filter)))+8),0);
     GameNameLabel.Caption:='';
     DragPoint:=Point(0,0);
     CollectionFileNameLabel.Visible:=False;
     BtnOpen.Enabled:=False; BtnOpen2.Enabled:=False; BitBtnOpen.Enabled:=False; MenuItemOpenCopy.Enabled:=False;
     BtnOK.Enabled:=False;
     BtnEditMenu.Visible:=False;
     BtnFileMenu.Visible:=False;
     GameFileGroupBox.Visible:=Task in [otGame];
     ImageFileGroupBox.Visible:=Task<>otGame;
     ImageGroupBox.Visible:=(Task=otImage) and (SubTask<>osSkin);
     SoundGroupBox.Visible:=Task in [otSound,otMusic,otMusicPath,otMusicPlayer];
     PluginGroupBox.Visible:=(Task=otSolver) or (Task=otOptimizer);
     PluginLevelGroupBox.Visible:=((Task=otSolver)
                                   //and
                                   //((MainForm.Solver.ThreadState=ptsLoadLevel)
                                   // or
                                   // (MainForm.Solver.ThreadState=ptsProcessLevel)
                                   //)
                                  )
                                  or
                                  ((Task=otOptimizer)
                                   and
                                   ((MainForm.Optimizer.ThreadState=ptsLoadLevel)
                                    or
                                    (MainForm.Optimizer.ThreadState=ptsProcessLevel)));
     if PluginLevelGroupBox.Visible then
        if      Task=otSolver    then PluginLevelGroupBox.Caption:=SolveLevelText
        else if Task=otOptimizer then PluginLevelGroupBox.Caption:=OptimizeSolutionText;
     MenuItemImageText.Checked:=False;

     ActiveList:=nil; Music:=nil; oMusicOnNotify:=nil;
     FileManagerTasksHaveBeenPerformed:=False;
     OnFontChange;

     if (Task=otSolver) or (Task=otOptimizer) then begin
        //EditSolveLevelName.Color:=PluginLevelGroupBox.Color;
        //EditSolveLevelName.Font.Color:=PluginLevelGroupBox.Font.Color;
        PluginLevelStringGrid.Color:=PluginLevelGroupBox.Color;
        PluginLevelStringGrid.Font.Color:=PluginLevelGroupBox.Font.Color;
        with PluginLevelStringGrid do
          if VisibleRowCount<RowCount then ScrollBars:=ssVertical;
        //with PluginForTask(Task) do PluginTimer.Enabled:=IsActive;
        PluginTimer.Enabled:=(MainForm.Solver.StartTimeMS<>0) or (MainForm.Optimizer.StartTimeMS<>0) or MainForm.Generator.IsActive;
        end;

     try
       IsLoading:=True;
       DriveComboBox1.Update;
       DirectoryListBox1.Update;
       FileListBox1.Update;
       DriveComboBox1Change(nil);
     finally
       IsLoading:=False;
     end;

     s:=BaseFileName;

     if Task=otGame then begin
        if   (GameViewer<>nil) and (GameViewer.Canvas=GameBoardImage.Picture.BitMap.Canvas) then
             // no need to call 'BoardResize' which makes a complete rebuild of the game-viewer
             // background images, a time-consuming task for some antialiasing settings
             GameViewer.Show
        else BoardResize(GameBoardImage);

        if IsAnIniFileSectionFileName(s) then s:=ExtractIniFileName(s);
        if (s<>'') and (FileExists(s)) then begin
           if AnsiCompareText(s,FileListBox1FileName)=0 then
              TryToLoadFile(BaseFileName)
           else begin
              SetFileFilterIndex(s);
              try     // loading the file by the assignment
                      // 'FileListBox1.FileName:=s' triggers a call to
                      // 'DirectoryListBox1Change()' which by default loads the
                      // first file on the file list;
                      //
                      // this is not wanted here where the intention is to load
                      // the file named 's'; if the first file on the list is
                      // large, loading it can cause a noticeable delay;
                      //
                      // the 'OnDirectoryChangeLoadFirstFileOnFileList' switch
                      // is used here to circumvent the default behavior;
                      OnDirectoryChangeLoadFirstFileOnFileList:=False;
                      FileListBox1.FileName:=s;
              finally OnDirectoryChangeLoadFirstFileOnFileList:=True;
              end;
              end;
           end
        else if   FileExists(FileListBox1FileName) and
                  MainForm.SokoFile.IsASokobanFile(FileListBox1FileName) then
                  TryToLoadFile(FileListBox1FileName)
             else TryToLoadFile('');
        end
     else begin
        if   (Task<>otNone) and (Task<>otImage) and (Task<>otPalette) and (Task<>otSolver) and (Task<>otOptimizer) and (MainForm.Music<>nil) then begin
             if MainForm.Music.Player<>nil then oMusicOnNotify:=MainForm.Music.Player.OnNotify;
             if MainForm.Music.CreatePlayer(MediaPlayerNotify) then begin
                Music:=MainForm.Music;
                if Music.Player.Mode=mpPlaying then
                   PlayingSoundFileLabel.Caption:=Format(PlayingFileText__,[ExtractFileNameWithoutExt(Music.Player.FileName)]);
                end
             else Music:=nil;
             end
        else Music:=nil;

        if Task=otMusicPath then begin
           TryToLoadFile('');

           try    repeat s:=StrWithoutTrailingPathDelimiter(s);
                         if   DirectoryExists(s) then begin
                              DirectoryListBox1.Directory:=StrWithTrailingPathDelimiter(s); s:='';
                              end
                         else s:=ExtractFilePath(s);
                  until  AnsiPos(FILE_NAME_PATH_DELIMITER,s)=0;
           except on E:Exception do;
           end;

           ActiveControl:=DirectoryListBox1;

           if (FileListBox1.Items.Count>0) and (ImageNamePanel.Caption='') then
              TryToLoadFile(StrWithTrailingPathDelimiter(DirectoryListBox1.Directory)+FileListBox1.Items[0]);
           end
        else begin
           if   (s<>'') and (FileExists(s)) then begin
                if   AnsiCompareText(s,FileListBox1FileName)=0 then
                     TryToLoadFile(s)
                else //try    FileListBox1.ApplyFilePath(s);
                     //except on E:Exception do;
                     FileListBox1.FileName:=s;
                end
           else if   FileExists   (FileListBox1FileName) then
                     TryToLoadFile(FileListBox1FileName)
                else TryToLoadFile('');
           end;
        end;

     FileListBox1.Mask:=FilterComboBox1.Mask;
     if      (FileListBox1.Items.Count<>0) and (Task=otImage) and DiskGroupBox.Visible then
             ActiveControl:=FileListBox1;
     if      ActiveControl=DirectoryListBox1    then DirectoryListBox1Enter(DirectoryListBox1)
     else if ActiveControl=FileListBox1         then FileListBox1Enter(FileListBox1)
     else if ActiveControl=CollectionStringGrid then CollectionStringGridEnter(CollectionStringGrid);

     BtnOpen2.Enabled:=MenuItemOpenCopy.Enabled; BitBtnOpen.Enabled:=True; BtnCancel.Enabled:=True;

     if Task=otGame then begin
        if   Assigned(DuplicatesForm) and DuplicatesForm.ShowOnStartUp and (SubTask<>osGeneratorCandidateSet) and (SubTask<>osPluginTaskQueue) then begin
             DuplicatesForm.Show;
             if Screen.ActiveForm<>Self then Self.SetFocus;
             end;
        if   CollectionGroupBox.Visible and
             (CollectionStringGrid.RowCount<>0) then
             ActiveControl:=CollectionStringGrid
        else ActiveControl:=FileListBox1;
        end;

     if SubTask=osSkin then begin
        SkinScriptsComboBoxEnter(nil); SkinsComboBoxEnter(nil);
        SkinScriptsComboBoxChange(Self);
        end;

     with LevelsFolderHistoryComboBox do
       if Tag<>0 then Perform(CB_SETDROPPEDWIDTH,Tag,0);
     with LevelsFileHistoryComboBox do
       if Tag<>0 then Perform(CB_SETDROPPEDWIDTH,Tag,0);
     with AnythingButLevelsFolderHistoryComboBox do
       if Tag<>0 then Perform(CB_SETDROPPEDWIDTH,Tag,0);
     with AnythingButLevelsFileHistoryComboBox do
       if Tag<>0 then Perform(CB_SETDROPPEDWIDTH,Tag,0);

     if (Task=otSolver) or (Task=otOptimizer) then with PluginForTask(Task) do begin
        Enter;
        try if IsActive then begin
               DiskGroupBox.Visible:=False;
               BtnCancel1  .Enabled:=False;
               end
            else begin
               //PanelPluginLevelMemo.Visible:=not Terminate;
               end;
        finally Leave;
        end;
        end;

     //MenuItemFindDuplicateLevelsClick(MenuItemFindDuplicateLevelsCurrentLevel);
     //MenuItemFindDuplicateLevelsClick(MenuItemFindDuplicateLevelsAllLevels);
     //PostMessage(Self.Handle,MSG_CLOSE,0,0);
     end;
end;

procedure TOpenForm.ShowHint(Sender: TObject);
begin
  StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=GetLongHint(Application.Hint);
end;

procedure TOpenForm.BtnOpenClick(Sender: TObject);
var Len:Integer; s:String; oCursor:TCursor;
begin
  if Task=otGame then begin
     if not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy) then begin
        if BtnOpen.Enabled
           and
           ((Sender=MenuItemOpenCopy) // 'CloseFile has already been called, so don't let 'ReloadCurrentGameIfNecessary' mess with the new filename in 'Game.FileName'
            or
            CloseFile
           )
           and
           (Game.BoardWidth >=MIN_BOARD_WIDTH) and
           (Game.BoardHeight>=MIN_BOARD_HEIGHT) and
           ((Game.GameState<>gsNull) or (SubTask=osGeneratorCandidateSet) or (SubTask=osPluginTaskQueue) or (SubTask=osLevelEditor))
           then begin
           oCursor:=Screen.Cursor;
           try
             Screen.Cursor:=crHourGlass;
             Len:=Length(Game.FileName);
             if AnsiPos(FILE_NAME_PATH_DELIMITER+LEFT_BRACKET+RIGHT_BRACKET,Game.FileName)=Len-2 then
                Game.FileName:=Copy(Game.FileName,1,Len-3); // drop '\[]' suffix, if any
             if IsBlank(Game.FileName) and
                Game.MakeNewFileName(StrWithoutBrackets(MainForm.ApplicationDataPath),TEXT_LEVEL,s) then
                Game.FileName:=s;

             if      SubTask=osGeneratorCandidateSet then begin
                     if   IsALevelGeneratorCandidateSet(MainForm.SokoFile) then  // 'True': the currently loaded level is a member of a collection
                          MainForm.Generator.LoadFromSokoFile(MainForm.SokoFile)
                     else MainForm.Generator.LoadFromGame(Self.Game);
                     BtnOpen2.Enabled:=MenuItemOpenCopy.Enabled; BitBtnOpen.Enabled:=True; BtnCancel.Enabled:=True;
                     ModalResult:=mrOk; Self.Close; ModalResult:=mrOk;
                     end
             else if SubTask=osPluginTaskQueue then begin
                     ToolsForm.PluginMenuItemAddClick( ToolsForm.PluginMenuItemImportTaskQueue );
                     BtnOpen2.Enabled:=MenuItemOpenCopy.Enabled; BitBtnOpen.Enabled:=True; BtnCancel.Enabled:=True;
                     ModalResult:=mrOk; Self.Close; ModalResult:=mrOk;
                     end
                  else begin
                     MainForm.MultiView.Clear;
                     ExchangeGames(MainForm.Game);
                     SnapshotsForm.LoadSnapshots(nil);
                     MainForm.Deadlocks.Clear;
                     MainForm.Game.ResetAndLoadSaveGame(MainForm.Game.RestoreSaveGame, // a saved game can first be loaded now because the start position is required for display
                                                        MainForm.Game.ResetSaveGameAndLoadItIfItIsANormalModeGame);
                     MainForm.InitGame(False,True,True,False,True,True,True,0);
                     if Assigned(MainForm.MultiView) then begin
                        if MainForm.Game.RestoreSaveGame then MainForm.MultiView.LoadSnapshots;
                        MainForm.MultiView.DeleteScreenRegionInformationForAllSnapshots // screen region information is one-shot information; if the multiple views aren't restored upon entry, the screen region information is considered garbage which should be removed
                        end;

                     BtnOpen2.Enabled:=MenuItemOpenCopy.Enabled; BitBtnOpen.Enabled:=True; BtnCancel.Enabled:=True;
                     ModalResult:=mrOk; Self.Close; ModalResult:=mrOk;
                     end;
           finally
             Screen.Cursor:=oCursor;
           end;
           end
        else begin
           if BtnOpen.Enabled and CanUnpackFile(FileListBox1FileName) then UnpackFile(FileListBox1FileName);
           end;
        end;
     end
  else begin
    if MenuItemImageText.Checked then
       MenuItemImageTextClick(Sender)
    else begin
       if BtnOK.Enabled and
          (not ((Sender=ImageFileGroupBox) and (Task=otImage) and (SubTask=osSkin))) and
          CloseFile then begin
          ModalResult:=mrOk;

          if Task=otMusicPlayer then begin
             PlayListOk:=True;
             if (Music<>nil) and (Music.Player<>nil) and
                FileExists(CurrentFileName) and
                ((Music.Player.Mode<>mpPlaying) or (CurrentFileName<>Music.Player.FileName)) and
                (AnsiPos(AnsiLowerCase(ExtractFileExt(CurrentFileName)),AnsiLowerCase(FilterComboBox1.Items[0]))<>0) and
                (not PlaySound) then ModalResult:=mrNone;
             end;

          if ModalResult=mrOk then begin Close; ModalResult:=mrOk; end;
          end;
       end;
    end;

  if (ModalResult=mrOk) and
     FileExists(FileListBox1FileName) and
     (not StrEqual(ExtractFileNameWithoutPathAndExtension(FileListBox1FileName),DEFAULT_VALUE)) then
     try // don't add temporary files to history
        if   LevelsFolderHistoryComboBox.Visible then
             AddItemOrMoveItemToTopOfComboBox(           LevelsFolderHistoryComboBox,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(DirectoryListBox1.Directory),MainForm.MyDocumentsFolder),True)
        else AddItemOrMoveItemToTopOfComboBox(AnythingButLevelsFolderHistoryComboBox,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(DirectoryListBox1.Directory),MainForm.MyDocumentsFolder),True);
        if   LevelsFileHistoryComboBox.Visible then
             AddItemOrMoveItemToTopOfComboBox(           LevelsFileHistoryComboBox  ,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(CurrentFileName,MainForm.MyDocumentsFolder),True)
        else AddItemOrMoveItemToTopOfComboBox(AnythingButLevelsFileHistoryComboBox  ,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(CurrentFileName,MainForm.MyDocumentsFolder),True);
     except on E:Exception do; //Error(E.Message,'');
     end;
end;

procedure TOpenForm.BtnOpen2Click(Sender: TObject);
var p:TPoint;
begin
  if not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy) then begin
     p:=ClientToScreen(Point(0,0));
     OpenPopupMenu.Popup(p.x+GameFileGroupBox.Left+BtnOpen.Left,
                         p.y+GameFileGroupBox.Top +BtnOpen.Top +BtnOpen.Height);
     end;
end;

procedure TOpenForm.BtnCancelClick(Sender: TObject);
var SnapshotType:TSnapshotType; v,NextSnapshot:TSnapshot;
begin
  if not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy) then begin
     if Task=otGame then begin
        if Game.Snapshots<>nil then begin
            v:=TSnapshot(Game.Snapshots.First); // remove newly imported snapshots, if any
            while v<>nil do begin
              NextSnapshot:=TSnapshot(v.Next);
              if v.Modified and (not IsASpecialSnapshotName__(v.Name,SnapshotType)) then
                 Game.Snapshots.Remove(v,True);
              v:=NextSnapshot;
              end;
           end;

        if FileManagerTasksHaveBeenPerformed and
           (BaseFileName<>'') and
           (not TryToLoadFile(BaseFileName)) then begin
           //RestoreImageSettings(False);
           end;
        GameInfoMemo.Modified:=False;
        end;
     if MenuItemImageText.Checked then begin
        ClearText(tTextFile);
        MenuItemImageTextClick(Sender);
        end
     else begin
        ClearText(tSkinScript); ClearText(tTextFile);
        ModalResult:=mrCancel; Close; ModalResult:=mrCancel;
        end;
     end;
end;

procedure TOpenForm.BtnHelpClick(Sender: TObject);
var oOnMessage:TMessageEvent; oOnIdle:TIdleEvent;
begin
  if not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy) then begin
     oOnMessage:=Application.OnMessage;
     oOnIdle   :=Application.OnIdle;
     try     MainForm.BtnHelpClick(Sender);
     finally Application.OnMessage:=oOnMessage;
             Application.OnIdle:=oOnIdle;
             if DiskGroupBox.Visible then
                ActiveControl:=FileListBox1;
     end;
     end;
end;

procedure TOpenForm.BtnOpenCopyClick(Sender: TObject);
var Len:Integer; s:String;
begin
  if Task=otGame then
     if BtnOpen.Enabled and
        CloseFile and
        (Game.BoardWidth >=MIN_BOARD_WIDTH) and
        (Game.BoardHeight>=MIN_BOARD_HEIGHT) and
        ((Game.GameState<>gsNull) or (SubTask=osGeneratorCandidateSet) or (SubTask=osPluginTaskQueue) or (SubTask=osLevelEditor)) then begin

        Len:=Length(Game.FileName);
        if AnsiPos(FILE_NAME_PATH_DELIMITER+LEFT_BRACKET+RIGHT_BRACKET,Game.FileName)=Len-2 then
           Game.FileName:=Copy(Game.FileName,1,Len-3); // drop '\[]' suffix, if any
        if IsBlank(Game.FileName) then
           if  Game.MakeNewFileName(StrWithoutBrackets(MainForm.ApplicationDataPath),TEXT_LEVEL,s) then begin
               Game.FileName:=s; Game.SokoFileName:=s;
               Game.DisplayName:=ExtractFileNameWithoutPathAndExtension(s);
               BtnOpenClick(MenuItemOpenCopy);
               end
           else
        else if IsAnIniFileSectionFileName(Game.FileName) then
                if Game.MakeNewFileName(ExtractFilePath(ExtractIniFileName(Game.FileName)),
                                        ExtractSectionName(Game.FileName),s) then begin
                   Game.FileName:=s; Game.SokoFileName:=s;
                   Game.DisplayName:=ExtractFileNameWithoutPathAndExtension(s);
                   BtnOpenClick(MenuItemOpenCopy);
                   end
                else
             else if Game.MakeNewFileName(ExtractFilePath(Game.FileName),
                                          ExtractFileNameWithoutPathAndExtension(Game.FileName),s) then begin
                     Game.FileName:=s; Game.SokoFileName:=s;
                     Game.DisplayName:=ExtractFileNameWithoutPathAndExtension(s);
                     BtnOpenClick(MenuItemOpenCopy);
                     end;
        end;
end;

procedure TOpenForm.FormResize(Sender: TObject);
var i,j,k,W,H:Integer; oVisible:Boolean;
begin
  if not DoMaximize then begin // if 'DoMaximize=True' then wait until the window is maximized
     StatusBar1                   .Width       :=ClientWidth;
     StatusBar1                   .Top         :=ClientHeight-StatusBar1.Height;

     //i:=StatusBar1.Top-DiskGroupBox.Left-CollectionGroupBox.Height-PANEL_Y_GAP-DiskGroupBox.Top;
     //if i>0 then DiskGroupBox.Height:=DiskGroupBox.Height+i;
     //j:=ClientWidth-GameFileGroupBox.Left-GameFileGroupBox.Width-DiskGroupBox.Left;
     //if j>0 then DiskGroupBox.Width:=DiskGroupBox.Width+j;

     W:=DiskGroupBox.Width; H:=DiskGroupBox.Height;
     i:=ClientHeight-StatusBar1.Height-DiskGroupBox.Top-DiskGroupBox.Height-PANEL_Y_GAP-DiskGroupBox.Left;
     if i<CollectionGroupBoxMinimumHeight then
        H:=Max(DiskGroupBoxMinimumHeight,DiskGroupBox.Height-(CollectionGroupBoxMinimumHeight-i));
     j:=ClientWidth-2*DiskGroupBox.Left-DiskGroupBox.Width-PANEL_X_GAP;
     if j<GameFileGroupBoxMinimumWidth then
        W:=Max(DiskGroupBoxMinimumWidth,DiskGroupBox.Width-(GameFileGroupBoxMinimumWidth-j));
     DiskGroupBox.SetBounds(DiskGroupBox.Left,DiskGroupBox.Top,W,H);

     i:=Min(FileListBox1.Left,DiskGroupBox.ClientWidth-FolderListBoxMinimumWidth-DirectoryListBox1.Left); // 'FolderListBoxMinimumWidth': the folder list box and the file list box have the same minimum width
     if i<FileListBox1.Left then begin
        j:=FileListBox1.Left-(DirectoryListBox1.Left+DirectoryListBox1.Width); // gap
        DirectoryListBox1.Width:=Max(FolderListBoxMinimumWidth,DiskGroupBox.ClientWidth-i-j-DirectoryListBox1.Left);
        FileListBox1.Left:=DirectoryListBox1.Left+DirectoryListBox1.Width+j;
        end;
     FileListBox1                          .Width  :=DiskGroupBox.ClientWidth-FileListBox1.Left-DirectoryListBox1.Left;
     with FileListBox1 do                  Tag     :=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // '-TOOL_TIP_FREE_ZONE_PIXELS': 'OnMouseMove' isn't triggered when the mouse is over the scrollbar, hence, this extra tooltip free zone
     with DirectoryListBox1 do             Tag     :=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // '-TOOL_TIP_FREE_ZONE_PIXELS': 'OnMouseMove' isn't triggered when the mouse is over the scrollbar, hence, this extra tooltip free zone

     FilterComboBox1                       .Width  :=FileListBox1.Width;
     FilterComboBox1.Perform(CB_SETDROPPEDWIDTH,
       Max(FilterComboBox1.Width,
           Canvas.TextWidth(System.Copy(FilterComboBox1.Filter,1, // 'Canvas': 'FilterComboBox1.Canvas' doesn't allow '.TextWidth' do draw, hence, use another canvas with the same font
                                        AnsiPos(BAR,FilterComboBox1.Filter)))+8),0);

     LevelsFolderHistoryComboBox           .Left   :=DirectoryListBox1.Left+DirectoryListBox1.Width-LevelsFolderHistoryComboBox.Width;
     AnythingButLevelsFolderHistoryComboBox.Left   :=LevelsFolderHistoryComboBox.Left;
     LevelsFileHistoryComboBox             .Left   :=FileListBox1.Left+FileListBox1.Width-LevelsFileHistoryComboBox.Width;
     AnythingButLevelsFileHistoryComboBox  .Left   :=LevelsFileHistoryComboBox.Left;

     FileLabel                             .Left   :=FileListBox1.Left;
     DriveComboBox1                        .Top    :=DiskGroupBox.Height-DriveComboBox1.Left-DriveComboBox1.Height;
     DriveComboBox1                        .Width  :=DirectoryListBox1.Width;     
     DriveLabel                            .Top    :=DriveComboBox1.Top-DriveLabel.Left-DriveLabel.Height;
     DirectoryListBox1                     .Height :=DriveLabel.Top-DirectoryListBox1.Left-DirectoryListBox1.Top;
     FilterComboBox1                       .Top    :=DiskGroupBox.Height-DriveComboBox1.Left-FilterComboBox1.Height;
     FilterComboBox1                       .Left   :=FileListBox1.Left;
     FileTypeLabel                         .Top    :=DriveLabel.Top;
     FileTypeLabel                         .Left   :=FileListBox1.Left;
     FileListBox1                          .Height :=DirectoryListBox1.Height;

     i:=DiskGroupBox.Left+DiskGroupBox.Width+PANEL_X_GAP;
     W:=ClientWidth-i-CollectionGroupBox.Left;
     H:=StatusBar1.Top-TopPanel.Top-TopPanel.Height-16;
     GameFileGroupBox.SetBounds(i,GameFileGroupBox.Top,W,H);

     j:=DiskGroupBox.Top+DiskGroupBox.Height+PANEL_Y_GAP;
     CollectionGroupBox.SetBounds(CollectionGroupBox.Left,j,DiskGroupBox.Width,GameFileGroupBox.Top+GameFileGroupBox.Height-j);
     ImageAntialiasingEnabled                  :=True;

     SkinScriptGroupBox                 .Height:=CollectionGroupBox.Height;
     SkinScriptPanel                    .Height:=SkinScriptGroupBox.Height-SkinScriptPanel.Top-SkinScriptPanel.Left;

     with CollectionGroupBox do GroupBox1.SetBounds(Left,Top,Width,Height);

     if Task=otGame then begin
        BtnOpen     .Hide;
        BtnOpen2    .Hide;
        BitBtnOpen  .Hide;
        BtnCancel   .Hide;
        BtnHelp     .Hide;

        BtnOpen                         .Top   :=GameFileGroupBox.Height-BtnOpen.Height-8;
        BtnCancel                       .Top   :=BtnOpen.Top;
        BtnHelp                         .Top   :=BtnOpen.Top;
        BtnOpen2                        .Top   :=BtnOpen.Top;
        BitBtnOpen                      .Top   :=BtnOpen.Top;

        BtnOpen     .Show;
        BtnOpen2    .Show;
//      BitBtnOpen  .Show;
        BtnCancel   .Show;
        BtnHelp     .Show;


        CollectionFileNamePanel         .Width :=CollectionGroupBox.Width-2*CollectionFileNamePanel.Left;
        CollectionStringGrid            .Width :=CollectionFileNamePanel.Width;
        with CollectionStringGrid do     Tag   :=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // '-TOOL_TIP_FREE_ZONE_PIXELS': 'OnMouseMove' isn't triggered when the mouse is over the scrollbar, hence, this extra tooltip free zone
        SetCollectionStringGridColumnWidths;

        GameBoardPanel                  .Width :=GameFileGroupBox.Width     -2*GameBoardPanel.Left;

        i:=Min(GameDataGroupBox.Top,BtnOpen.Top-GameDataGroupBoxMinimumHeight-(GameNamePanel.Top div 2));
        if i<GameDataGroupBox.Top then begin
           j:=GameDataGroupBox.Top-(GameBoardPanel.Top+GameBoardPanel.Height); // gap
           GameBoardPanel.Height:=Max(GameBoardPanel.Height-(GameDataGroupBox.Top-i),DiskGroupBoxMinimumHeight-GameBoardPanel.Top);
           GameDataGroupBox.Top:=GameBoardPanel.Top+GameBoardPanel.Height+j;
           end;

        GameBoardImage.SetBounds(GameBoardImage.Left,GameBoardImage.Top,
                                 GameBoardPanel.ClientWidth -2*GameBoardImage.Left,
                                 GameBoardPanel.ClientHeight-2*GameBoardImage.Top);

        GameNameLabel                   .Width :=GameFileGroupBox.Width  -2*GameNameLabel.Left;
        GameNamePanel                   .Width :=GameFileGroupBox.Width  -2*GameNamePanel.Left;

        GameDataGroupBox.SetBounds(GameDataGroupBox.Left,GameDataGroupBox.Top,
                                   GameNamePanel.Width,
                                   BtnOpen.Top-GameDataGroupBox.Top-(GameNamePanel.Top div 2));

        BoardResize(GameBoardImage);

        if GameViewer.Initialized then GameViewer.LoadGame(Game);

        //GameLoadErrorMemo.SetBounds(GameLoadErrorMemo.Left,GameLoadErrorMemo.Top,
        //                            GameBoardPanel.Width  -2*GameLoadErrorMemo.Left,
        //                            GameBoardPanel.Height -2*GameLoadErrorMemo.Top);

        i:=CollectionStringGrid.Top;
        j:=CollectionStringGrid.DefaultRowHeight+CollectionStringGrid.GridLineWidth;
        k:=CollectionGroupBox.Height-CollectionGroupBox.Left;
        repeat Inc(i,j);
        until  i>=k;
        CollectionStringGrid.ClientHeight      :=Max(i-j-CollectionStringGrid.Top,CollectionStringGrid.DefaultRowHeight+2*CollectionStringGrid.GridLineWidth);

        CurrentMemo                            :=GameInfoMemo;
        GameInfoMemo.SetBounds(GameInfoMemo.Left,GameInfoMemo.Top,
                               GameDataGroupBox.Width-2*GameInfoMemo.Left, //GameBoardImage.Width-12;
                               Max(24,GameDataGroupBox.Height-GameInfoMemo.Top-(GameSolutionTextLabel.Top div 2)));

        BtnGameBuiltinSolutions         .Left  :=GameInfoMemo.Left+GameInfoMemo.Width-BtnGameBuiltinSolutions.Width;

        ShowSolutionsForVisibleLevels;

        if CollectionStringGrid.Visible then
           GridScrollInView(CollectionStringGrid,CollectionStringGrid.Row);
        with CollectionStringGrid do
          while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]);
        end
     else begin
        BtnOK     .Hide;
        BtnCancel1.Hide;
        BtnHelp1  .Hide;

        i:=DiskGroupBox.Left+DiskGroupBox.Width+PANEL_X_GAP;
        ImageFileGroupBox.SetBounds(i,ImageFileGroupBox.Top,
                                    ClientWidth-i-DiskGroupBox.Left,
                                    StatusBar1.Top-TopPanel.Top-TopPanel.Height-16);

        GroupBox1.SetBounds(GroupBox1.Left,GroupBox1.Top,
                            ImageFileGroupBox.Width,
                            ImageFileGroupBox.Top+ImageFileGroupBox.Height-GroupBox1.Top);

        with CollectionGroupBox do ImageGroupBox.SetBounds(Left,Top,Width,Height);

        BtnOK                           .Top   :=ImageFileGroupBox.Height-BtnOK.Height-8;
        BtnCancel1                      .Top   :=BtnOK.Top;
        BtnHelp1                        .Top   :=BtnOK.Top;

        BtnOK     .Show;
        BtnCancel1.Show;
        BtnHelp1  .Show;

        SoundGroupBox.SetBounds(SoundGroupBox.Left,
                                BtnOK.Top-8-SoundGroupBox.Height,
                                ImageFileGroupBox.Width - 2*SoundGroupBox.Left,
                                SoundGroupBox.Height);

        with CollectionGroupBox do PluginLevelGroupBox.SetBounds(Left,Top,Width,Height);
        PanelPluginLevelInfo       .Height:=PluginLevelGroupBox.ClientHeight-PanelPluginLevelInfo.Top-8;
        PluginGroupBox             .Top   :=PluginLevelGroupBox.Top-ImageFileGroupBox.Top;
        if (Task=otSolver) or (Task=otOptimizer) then ResizePluginLevelGroupBox;
        if SkinScriptGroupBox.Visible then ResizeSkinScriptGroupBox;

        with SoundGroupBox do PluginGroupBox.SetBounds(Left,PluginGroupBox.Top,Width,BtnOK.Top-8-PluginGroupBox.Top);
        //with SoundGroupBox do PluginGroupBox.SetBounds(Left,Top,Width,Height);

        if Task=otPalette then begin
          W:=Image1.ClientWidth+4;
          i:=(ImageFileGroupBox.Width-W) div 2;
          end
        else begin
          i:=ImageNamePanel.Left;
          W:=ImageFileGroupBox.Width - 2*i;
          end;
        if   Task=otImage then
             if   MenuItemImageText.Checked then
                  H:=DiskGroupBox.Height-ImagePanel.Top
             else H:=BtnOK.Top-8-ImagePanel.Top
        else if Task=otPalette then
             H:=Min(BtnOK.Top-8-ImagePanel.Top,Image1.ClientHeight+4)
        else if (Task=otSolver) or (Task=otOptimizer) then
                H:=DiskGroupBox.Height-ImagePanel.Top
        else    H:=SoundGroupBox .Top-ImagePanel.Top-8;

        oVisible:=ImageScrollBox.Visible;
        try
          if not ImageScrollBox.Visible then with ImageScrollBox do begin
             // use the tag to save the visibility of the scrollbars;
             // it's unclear if the visibility is used later when the scrollbox
             // becomes visible again, so better safe than sorry
             Tag:=0;
             if HorzScrollBar.Visible then begin Tag:=2    ; HorzScrollBar.Visible:=False; end; // hide the scrollbars; otherwise they are momentarily visible when the scrollbox resizes
             if VertScrollBar.Visible then begin Tag:=Tag+1; VertScrollBar.Visible:=False; end;
             Visible:=True; // if the scrollbox is hidden, it doesn't auto-align to client size
             end;

          ImagePanel.SetBounds(i,ImagePanel.Top,W,H);

//        Image1                        .Width :=ImageScrollBox.ClientWidth -2*Image1.Left;
//        Image1                        .Height:=ImageScrollBox.ClientHeight-2*Image1.Top;

          ImageNamePanel                .Width :=ImageFileGroupBox.Width  -2*ImageNamePanel.Left;
        finally
          if ImageScrollBox.Visible<>oVisible then with ImageScrollBox do begin
             Visible:=oVisible;
             if (Tag and 1)<>0 then VertScrollBar.Visible:=True;
             if (Tag and 2)<>0 then HorzScrollBar.Visible:=True;
             end;
        end;

        if MenuItemImageText.Checked then begin
           j:=ImagePanel.Top+ImagePanel.Height+8;
           H:=BtnOK.Top-8-j;
           end
        else begin
           j:=ImagePanel.Top;
           H:=ImagePanel.Height;
           end;
        TextFilePanel.SetBounds(ImagePanel.Left,j,ImagePanel.Width,H);

        ImageResize;

        //ImageLoadErrorMemo            .Width :=ImagePanel.Height -2*ImageLoadErrorMemo.Left;
        //ImageLoadErrorMemo            .Height:=ImagePanel.Height -2*ImageLoadErrorMemo.Top;

        DisplayBuiltinImage;

        ImageGroupBox                   .Height:=GroupBox1.Height;

        end;

     DirectoryListBoxScrollInView(DirectoryListBox1,DirectoryListBox1.ItemIndex);
     FileListBoxScrollInView     (FileListBox1     ,FileListBox1     .ItemIndex);

     if Left>Screen.DeskTopLeft+Screen.DeskTopWidth -30 then Left   :=Screen.DeskTopLeft+Screen.DeskTopWidth -30;
     if Top >Screen.DeskTopTop +Screen.DeskTopHeight-30 then Top    :=Screen.DeskTopTop +Screen.DeskTopHeight-30;
     end;
end;

function  TOpenForm.ResizeDiskGroupBox(DiskGroupBoxWidth__,DiskGroupBoxHeight__:Integer):Boolean;
begin
  Result:=(DiskGroupBoxWidth__>=DiskGroupBoxMinimumWidth) and
          (GameFileGroupBox.Width-(DiskGroupBoxWidth__-DiskGroupBox.Width)>=GameFileGroupBoxMinimumWidth) and
          (DiskGroupBoxHeight__>=DiskGroupBoxMinimumHeight) and
          (CollectionGroupBox.Height-(DiskGroupBoxHeight__-DiskGroupBox.Height)>=CollectionGroupBoxMinimumHeight);
  if Result then begin
     DiskGroupBox.Width :=DiskGroupBoxWidth__;
     DiskGroupBox.Height:=DiskGroupBoxHeight__;
     FormResize(Self);
     end;
end;

function  TOpenForm.ResizeFolderListBox(FolderListBoxWidth__:Integer):Boolean;
var Delta:Integer;
begin
  Delta:=FolderListBoxWidth__-DirectoryListBox1.Width;
  Result:=(FolderListBoxWidth__>=FolderListBoxMinimumWidth) and
          (FileListBox1.Width-Delta>=FolderListBoxMinimumWidth); // 'FolderListBoxMinimumWidth': the folder list box and the file list box have the same minimum width
  if Result then begin
     DirectoryListBox1.Width:=FolderListBoxWidth__;
     FileListBox1.Left:=FileListBox1.Left+Delta;
     FormResize(Self);
     end;
end;

function  TOpenForm.ResizeGameBoardPanel(GameBoardPanelHeight__:Integer):Boolean;
var Delta:Integer;
begin
  Delta:=GameBoardPanelHeight__-GameBoardPanel.Height;
  Result:=(GameBoardPanel.Top+GameBoardPanelHeight__>=DiskGroupBoxMinimumHeight) and
          (GameDataGroupBox.Height-Delta>=GameDataGroupBoxMinimumHeight);
  if Result then begin
     GameBoardPanel.Height:=GameBoardPanelHeight__;
     GameDataGroupBox.Top:=GameDataGroupBox.Top+Delta;
     FormResize(Self);
     end;
end;

procedure TOpenForm.ResizePluginLevelGroupBox;
var i,W:Integer; pcbii:TPluginCallBackInfoItem;
begin
  with CollectionGroupBox do PluginLevelGroupBox.SetBounds(Left,Top,Width,Height);
  PluginLevelMemo.Align:=alClient;
  PluginLevelStringGrid.Align:=alClient;
  PluginLevelFileNamePanel.Height:=BtnSolveLevel.Height;
  PanelPluginLevelInfo.Left:=PluginLevelFileNamePanel.Left;
  PanelPluginLevelInfo.Top:=BtnSolveLevel.Top+BtnSolveLevel.Height+8;
  PanelPluginLevelInfo.Width:=PluginLevelGroupBox.Width-2*PanelPluginLevelInfo.Left;
  BtnSolveLevel.Left:=PanelPluginLevelInfo.Left+PanelPluginLevelInfo.Width-BtnSolveLevel.Width;
  if   BtnSolveLevel.Visible or BtnOptimizeGames.Visible then
       PluginLevelFileNamePanel.Width:=BtnSolveLevel.Left-2*PluginLevelFileNamePanel.Left
  else PluginLevelFileNamePanel.Width:=PanelPluginLevelInfo.Width;
  W:=0;
  for pcbii:=Low(PluginCallBackInfoText) to High(PluginCallBackInfoText) do with PluginLevelStringGrid do begin
      Cells[0,Ord(pcbii)]:=PluginCallBackInfoText[pcbii];
      i:=Canvas.TextWidth (PluginCallBackInfoText[pcbii]);
      if i>W then W:=i;
      end;
  PluginLevelStringGrid.ColWidths[0]:=W+8;
  with PluginLevelStringGrid do ColWidths[1]:=ClientWidth-ColCount*GridLineWidth-ColWidths[0];
  with BtnSolveLevel do BtnOptimizeGames.SetBounds(Left,Top,Width,Height);
end;

procedure TOpenForm.ResizeSkinScriptGroupBox;
begin
  with CollectionGroupBox do SkinScriptGroupBox.SetBounds(Left,Top,Width,Height);
  with BtnSkinScriptBrowse do Left:=SkinScriptGroupBox.Width-SkinScriptsComboBox.Left-Width;
  with SkinScriptsComboBox do Width:=BtnSkinScriptBrowse.Left-2*Left;
  with SkinScriptPanel do begin
    Width :=SkinScriptGroupBox.Width -2*Left;
    Height:=SkinScriptGroupBox.Height-Top-Left;
    end;
end;

procedure TOpenForm.BoardResize(GameBoardImage:TImage);
begin
  if GameViewer<>nil then with GameBoardImage.Picture.BitMap do
     try    if GameBoardImage.ClientWidth <=0 then
               GameBoardImage.ClientWidth:=1;
            if GameBoardImage.ClientHeight<=0 then
               GameBoardImage.ClientHeight:=1;
            if Width <>GameBoardImage.ClientWidth  then Width :=GameBoardImage.ClientWidth;
            if Height<>GameBoardImage.ClientHeight then Height:=GameBoardImage.ClientHeight;

            GameViewer.SetWindow(Canvas,Rect(0,0,Width,Height));
            GameViewer.Show;
     except on E:Exception do Error(E.Message,Application.Title);
     end;
end;

procedure TOpenForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize:=(NewWidth >=CalculateMinWidth ) and
          (NewHeight>=CalculateMinHeight) and
          (not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy));
end;

procedure TOpenForm.FileListBox1Change(Sender: TObject);
var i:Integer; s:String;
begin
  if (Screen.ActiveForm=Self) and DiskGroupBox.Visible then begin
     ShortCutsEnabled:=False;
     if not BtnFileMenu.Visible then BtnFileMenu.Show;
     if MenuItemImageText.Checked then MenuItemImageTextClick(Self);
     with FileListBox1 do if IsMediaPresent(Drive) then;

     if not IsLoading then
        try
          IsLoading:=True;
          if Task=otGame then begin
             s:=BaseFileName;
             if   IsAnIniFileSectionFileName(s) then begin
                  s:=ExtractIniFileName(s);
                  if   ((FileListBox1FileName='')
                        or
                        (AnsiCompareText(s,FileListBox1FileName)=0)
                       )
                       and
                       StrEqual(StrWithoutTrailingPathDelimiter(ExtractFilePath(s)),StrWithoutTrailingPathDelimiter(FileListBox1.Directory))
                       and
                       MainForm.SokoFile.Open(s)
                       and
                       MainForm.SokoFile.Levels.ItemExists(ExtractSectionName(BaseFileName)) then
                       TryToLoadFile(BaseFileName)
                  else TryToLoadFile(FileListBox1FileName);
                  end
             else TryToLoadFile(FileListBox1FileName);
             end
          else if Task=otMusicPlayer then begin
                  s:=BaseFileName;
                  if   IsAnIniFileSectionFileName(s) then begin
                       s:=ExtractIniFileName(s);
                       if   (AnsiCompareText(s,FileListBox1FileName)=0)
                            and
                            IsAPlayListFile(s) //and
                            //PlayListLoadFromFile(s) //and
                            //MainForm.Pack.IniFile.SectionExists(ExtractSectionName(BaseFileName))
                            then
                            TryToLoadFile(BaseFileName)
                       else TryToLoadFile(FileListBox1FileName);
                       end
                  else TryToLoadFile(FileListBox1FileName);
                  end
               else with FileListBox1 do begin
                  TryToLoadFile(FileListBox1FileName);

                  if (Task=otImage) and (SubTask=osSkin) and
                     (Items.Count<>0) and (FileListBox1FileName='') then begin
                     TryToLoadFile(StrWithTrailingPathDelimiter(Directory)+Items[0]);
                     end;
                  end;
        finally
          IsLoading:=False;
        end;

     with DriveComboBox1 do
       if (ItemIndex>=0) and (DriveHistory<>nil) then begin
          for i:=DriveHistory.Count to ItemIndex do DriveHistory.Add('');
          if (ItemIndex<DriveHistory.Count) and
             (FileListBox1FileName<>'') then
             DriveHistory.Strings[ItemIndex]:=FileListBox1FileName;
          end;

     with FileListBox1 do begin
       if (Task<>otGame) and (Task<>otSolver) and (Task<>otOptimizer) then
          if   ItemIndex>=0 then
               StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=IntToStr(Succ(ItemIndex))+SLASH+IntToStr(Items.Count)
          else StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
       BtnImagePrior.Enabled:=Items.Count>1;
       BtnImageNext .Enabled:=Items.Count>1;
       end;

     with FileListBox1 do Tag:=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // '-TOOL_TIP_FREE_ZONE_PIXELS': 'OnMouseMove' isn't triggered when the mouse is over the scrollbar, hence, this extra tooltip free zone
     end;
end;

function  TOpenForm.TryToLoadFile(const FileName:String):Boolean;
begin
  try
    IsLoading:=True;
    if   Task=otGame then
         Result:=TryToLoadGame (FileName)
    else Result:=TryToLoadImage(FileName);
  finally
    IsLoading:=False;
  end;
end;

function  TOpenForm.TryToLoadGame(const FileName:String):Boolean;
var i,VisibleRowCount:Integer; b,CanUnpackFile,IsAnEmptyLevelCandidateSet:Boolean; s,ThisFileName,ThisPackFileName:String;
    IsAPackFile:Boolean; R:TRect;

  procedure LoadPackFileIndex;
  var ACol,ARow,Count:Integer; oCurrentCollectionFileName, s:String; Node:TNode;
  begin
    if MainForm.SokoFile.Name<>CurrentCollectionFileName then
       with CollectionStringGrid do
         try    if   MainForm.SokoFile.Open(ThisPackFileName) then
                     CurrentCollectionFileName:=MainForm.SokoFile.Name
                else CurrentCollectionFileName:='';

                oCurrentCollectionFileName:=CurrentCollectionFileName;
                try
                  CurrentCollectionFileName:='';
                  Count:=MainForm.SokoFile.Levels.Count;
                  Node:=MainForm.SokoFile.Levels.Items;
                  // caution: changing 'RowCount' may trigger several updating actions,
                  // hence, all actions must be sure they don't destroy anything if they are called prematurely
                  for ACol:=0 to Pred(ColCount) do CollectionStringGrid.Cells[ACol,0]:=''; // clear first row
                  // kludge: an empty first row tells 'ShowSolutionsForVisibleLevels' not to
                  // do anything when it's activated by changing 'RowCount'
                  Row:=0;
                  RowCount:=Max(1,Count);
                  for ARow:=0 to Pred(Count) do begin
                      s:=Node.Name; Node:=Node.Next;
                      Cells[1,ARow]:=s;
                      Cells[0,ARow]:=IntToStr(Succ(ARow));
                      for ACol:=2 to Pred(ColCount) do Cells[ACol,ARow]:='';
                      end;
                finally CurrentCollectionFileName:=oCurrentCollectionFileName;
                        SetCollectionStringGridColumnWidths;
                        LoadPackFileIndexCount:=1;
                        ClearSolutionStatistics;
                end;
         except on E:Exception do begin
                   CollectionStringGrid.RowCount:=1; LoadPackFileIndexCount:=0;
                   for ACol:=0 to Pred(ColCount) do CollectionStringGrid.Cells[ACol,0]:='';
                   ClearSolutionStatistics;
                   end;
         end
    else
       if LoadPackFileIndexCount<>High(LoadPackFileIndexCount) then
          Inc(LoadPackFileIndexCount);
  end;

begin // TryToLoadGame
  Result:=False;
  StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
  //if PanelToolTips.Visible then begin
  //   PanelToolTips.Hide; Update;
  //   end;

  if CloseFile then begin
     IsAPackFile:=False; CurrentFileName:=''; CurrentSectionName:=''; CurrentFileStringGridRow:=-1;
     Game.Clear; CanUnpackFile:=False; IsAnEmptyLevelCandidateSet:=False;

     if (Length(FileName)>3) and (FileName[3]=FILE_NAME_PATH_DELIMITER) then begin
//      GameNameLabel.Caption:=Copy(FileName,1,3)+'...\'+ExtractFileName(FileName);
        if   (Length(FileName)>3) and (FileName[3]=FILE_NAME_PATH_DELIMITER) then
             s:=SPACE+StrWithTrailingPathDelimiter(Copy(FileName,1,3)+'...')
        else s:='';
        GameNamePanel         .Caption :=s+StrWithoutBrackets(ExtractFileName(FileName));
        GameLoadErrorMemo     .Text    :='';
        Game.LastErrorStr              :='';

        if SubTask=osGeneratorCandidateSet then begin
           if   IsAnIniFileSectionFileName(FileName) then
                ThisFileName:=ExtractIniFileName(FileName)
           else ThisFileName:=FileName;
           if   Game.SokoFile.Open(ThisFileName) and
                IsALevelGeneratorCandidateSet(Game.SokoFile) and
                Game.SokoFile.Levels.IsEmpty then
                IsAnEmptyLevelCandidateSet:=True;
           end;

        if (not IsAnIniFileSectionFileName(FileName)) and (SubTask<>osGeneratorCandidateSet) and (SubTask<>osPluginTaskQueue) then with LevelsFileHistoryComboBox do begin
           for i:=0 to Pred(Items.Count) do begin
               s:=ExpandedFilePath(Items[i],MainForm.MyDocumentsFolder);
               if (StrBeginsWith(s,FileName)) and
                  ((Length(s)<=Length(FileName))
                   or
                   (s[Succ(Length(FileName))]=FILE_NAME_PATH_DELIMITER)
                  ) then begin
                  Result:=Game.LoadFromFileOrClipBoard(s,nil,nil,b); // try to load the most recently used level from the collection
                  if not Result then LevelsFileHistoryComboBox.Items.Delete(i);
                  break;
                  end;
               end;
           end;
        if not (Result or IsAnEmptyLevelCandidateSet) then Result:=Game.LoadFromFileOrClipBoard(FileName,nil,nil,b);
        if Result     then ThisFileName:=Game.FileName
        else               ThisFileName:=FileName;
        if   IsAnIniFileSectionFileName(ThisFileName) then begin
             IsAPackFile:=(not Result) or
                          ((Game.SokoFile<>nil) and
                           (Game.SokoFile.Levels.Count>1));
             if IsAPackFile then begin
                ThisPackFileName       :=ExtractIniFileName(ThisFileName);
                CurrentSectionName     :=ExtractSectionName(ThisFileName);
                CollectionFileNamePanel
                             .Caption  :=StrWithQuotedAmpersands(SPACE+StrWithTrailingPathDelimiter(Copy(ThisPackFileName,1,3)+'...')+ExtractFileName(ThisPackFileName));
                GameNamePanel.Caption  :=SPACE+StrWithQuotedAmpersands(CurrentSectionName);
                end
             else begin
                if (Length(ThisFileName)>3) and (ThisFileName[3]=FILE_NAME_PATH_DELIMITER) then
                   s:=SPACE+strWithTrailingPathDelimiter(Copy(ThisFileName,1,3)+'...')
                else s:='';
                ThisFileName           :=ExtractIniFileName(ThisFileName);
                GameNamePanel.Caption  :=s+StrWithoutBrackets(ExtractFileName(ThisFileName));
                end;
             end
        else begin IsAPackFile:=False;
                   if IsAnIniFileSectionFileName(FileName) then
                      GameNamePanel.Caption:=SPACE+StrWithTrailingPathDelimiter(Copy(ThisFileName,1,3)+'...')+ExtractFileName(ThisFileName);
             end;

        GameLoadErrorMemo      .Visible:=(not Result) or IsAnEmptyLevelCandidateSet;
        GameBoardImage         .Visible:=Result;
//      GameNameLabel          .Visible:=Result;
        GameNamePanel          .Visible:=True; //Result or IsAPackFile;
        BtnOpen                .Enabled:=(Result and ((Game.GameState<>gsNull) or (SubTask=osGeneratorCandidateSet) or (SubTask=osPluginTaskQueue) or (SubTask=osLevelEditor))) or IsAnEmptyLevelCandidateSet;
        MenuItemOpenCopy       .Enabled:=BtnOpen.Enabled and (SubTask<>osGeneratorCandidateSet) and (SubTask<>osPluginTaskQueue);
        BtnOpen2               .Enabled:=BtnOpen.Enabled and BtnCancel.Enabled and MenuItemOpenCopy.Enabled;
        BitBtnOpen             .Enabled:=BtnOpen.Enabled and BtnCancel.Enabled;

        CollectionGroupBox     .Visible:=IsAPackFile;
        CollectionFileNameLabel.Visible:=False;
        CollectionFileNamePanel.Visible:=IsAPackFile;

        if   Result then begin
             if GameViewer=nil then begin
                GameLoadErrorMemo.Text:=OKText;
                GameLoadErrorMemo.Show;
                end
             else begin
                GameViewer.Modified:=True;
                if Game.Reversemode then begin
                   Game.SetReverseMode(False); // otherwise the game will show up in reverse mode if last loaded snapshot is a reverse mode snapshot
                   Game.Reset(False);
                   end;
                GameViewer.LoadGame(Game);
                end;
             CurrentFileName:=Game.FileName;
             StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=Misc_.BoardDimensionsAndBoxesAndFloorsAsText(Game.BoardWidth,Game.BoardHeight,Game.BoxCount,Game.FloorCount,MainForm.BoardDimensionsAsText,MainForm.BoardDimensionsWithFloorCount); //SPACE+IntToStr(Game.FloorCount);
             end
        else begin
             Game.Clear;
             if IsAnEmptyLevelCandidateSet then begin
                Result:=Game.SokoFile.FileHeader.Lines.ToText(NL,s);
                if   Result then GameLoadErrorMemo.Text:=s
                else GameLoadErrorMemo.Text:=TEXT_TASK_FAILED;
                end
             else begin
                if   IsAPackFile then
                     s:=Format(OpenPackFileMemberFailedText__,[CurrentSectionName,ThisPackFileName])
                else s:=Format(FileNotASokobanGameText__  ,[ThisFileName]);
                if (Game.LastErrorStr<>'') and (Game.LastErrorStr<>NoLevelsFoundText) then
                   s:=s+NL+NL+TEXT_FAILURE_DESCRIPTION+NL+NL+Game.LastErrorStr;
                if Self.CanUnpackFile(ThisFileName) then begin
                   CanUnpackFile:=True;
                   s:=Format(UnpackFileHintText__,[ThisFileName])+UnpackFileHint2Text+UnpackFileHint3Text;
                   BtnOpen.Enabled:=True;
                   end;
                GameLoadErrorMemo.Text:=s;
                end;
             end;

        LoadGameData(True);

        with FileListBox1 do begin
          if   IsAPackFile then
               s:=ExtractFileName(ThisPackFileName)
          else s:=ExtractFileName(ThisFileName);
          i:=Items.IndexOf(s);
          if i>=0 then begin
             ItemIndex:=i;
             end;

          R:=ItemRect(0);
          VisibleRowCount:=Max(1,ClientHeight div (R.Bottom-R.Top));
          if (ItemIndex<TopIndex) or (ItemIndex>=TopIndex+VisibleRowCount) then begin
             i:=0;
             repeat Inc(i,VisibleRowCount);
             until i>ItemIndex;
             TopIndex:=i-VisibleRowCount;
             end;

          FileListBox1Change(Self);
          end;

        if IsAPackFile then with CollectionStringGrid do begin
           LoadPackFileIndex;
           for i:=0 to Pred(RowCount) do
               if CurrentSectionName=Cells[1,i] then begin
                  CurrentFileStringGridRow:=i; Row:=i;
                  if LoadPackFileIndexCount=1 then
                     // first time after the collection was opened, or first time after this form was opened
                     TopRow:=Max(FixedRows,
                                 Min(RowCount-VisibleRowCount,Row-(Max(0,Pred(VisibleRowCount)) div 2)));
                  break;
                  end;
           GridScrollInView(CollectionStringGrid,Row);
           ShowSolutionsForVisibleLevels;
           end;
        end
     else begin
        GameLoadErrorMemo .Visible:=False;
        GameBoardImage    .Visible:=False;
        GameNamePanel     .Visible:=False;
        BtnOpen           .Enabled:=False;
        BtnOpen2          .Enabled:=False; MenuItemOpenCopy.Enabled:=False;
        BitBtnOpen        .Enabled:=False;
        CollectionGroupBox.Visible:=False;
        LoadGameData(True);
        if Screen.ActiveForm=Self then
           ActiveControl          :=FileListBox1;
        end;

     if Screen.ActiveForm=Self then
        if   IsAPackFile then
             ActiveControl:=CollectionStringGrid
        else ActiveControl:=FileListBox1;

     if (SubTask<>osGeneratorCandidateSet) or (not IsALevelGeneratorCandidateSet(Game.SokoFile)) then begin
        GameBoardImage.Hint:=HintLeftClickToOpenLevelText+HintRightClickToReturnText;
        if   CanUnpackFile then
             BtnOpen.Hint:=HintUnpackFileText
        else BtnOpen.Hint:=HintOpenLevelText;
        end
     else begin
        GameBoardImage.Hint:=HintLeftClickToOpenCandidateSetText+HintRightClickToReturnText;
        BtnOpen.Hint:=HintOpenCandidateSetText;
        end;

     end;
end;

procedure TOpenForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if      Sender=Self then
          Action:=caHide
  else    Action:=caFree;

  if      Task=otGame          then FilterIndexGame   :=FilterComboBox1.ItemIndex
  else if Task=otImage         then
          if   SubTask<>osSkin then FilterIndexImage  :=FilterComboBox1.ItemIndex
          else                      FilterIndexSkin   :=FilterComboBox1.ItemIndex
  else if Task=otPalette       then FilterIndexPalette:=FilterComboBox1.ItemIndex
  else if Task=otSolver        then FilterIndexPlugin :=FilterComboBox1.ItemIndex
  else if Task=otOptimizer     then FilterIndexPlugin :=FilterComboBox1.ItemIndex
  else                              FilterIndexSound  :=FilterComboBox1.ItemIndex;

  if      Task=otGame then begin
          LastOpenedPackFileName:=CurrentCollectionFileName;
          if (Sender=Self) and (not BtnCancel.Enabled) then
             BtnOpenClick(Sender); // the user probably pressed the 'close-window button' try to update the game in the main-window;
          //RestoreImageSettings(True);
          end
  else if (Task=otImage) and (AnsiCompareText(CurrentFileName,DefaultImageFileName)=0) then
          CurrentFileName:=DEFAULT_VALUE
  else if (Task=otPalette) and (AnsiCompareText(CurrentFileName,DefaultPaletteFileName)=0) then
          //CurrentFileName:=DEFAULT_VALUE
  else if (Task=otSound) and
          (AnsiCompareText(CurrentFileName,MainForm.ApplicationDataPath+DEFAULT_VALUE+WAV_FILE_EXT)=0) then
          CurrentFileName:=DEFAULT_VALUE
  else if (Task in [otMusic,otMusicPath,otMusicPlayer]) and (Music<>nil) then begin
          Music.FileList.Directory:=FileListBox1.Directory;
          Music.FileList.Mask     :=FileListBox1.Mask;
          end;

  if      (Music<>nil) and (Music.Player<>nil) then begin
          if Assigned(oMusicOnNotify) then Music.Player.OnNotify:=oMusicOnNotify
          else Music.Player.OnNotify:=Music.PlayerNotify;

          if Music.Player.Mode<>mpPlaying then begin
             if Task<>otMusicPlayer then Music.Close;
             MainForm.Status.Hint:='';
             end;
          end;

  if MenuItemImageText.Checked then MenuItemImageTextClick(Self);

  PluginGroupBox.Hide;

  FlushText(tTextFile); FlushText(tSkinScript); // order important

  if Assigned(OptionsForm) and OptionsForm.Visible then with OptionsForm do begin
     if Self.WindowState=wsNormal then begin
        SettingsString[stWindowsOpenLeft]                 :=IntToStr(Self.Left);
        SettingsString[stWindowsOpenTop]                  :=IntToStr(Self.Top);
        SettingsString[stWindowsOpenWidth]                :=IntToStr(Self.Width);
        SettingsString[stWindowsOpenHeight]               :=IntToStr(Self.Height);
        end;
     SettingsString[stWindowsOpenDiskGroupBoxWidth]       :=IntToStr(Self.DiskGroupBox.Width);
     SettingsString[stWindowsOpenDiskGroupBoxHeight]      :=IntToStr(Self.DiskGroupBox.Height);
     SettingsString[stWindowsOpenLevelPreviewHeight]      :=IntToStr(Self.GameBoardPanel.Height);
     end;

  if (Task=otGame) and (SubTask<>osGeneratorCandidateSet) and (SubTask<>osPluginTaskQueue) and Assigned(DuplicatesForm) then with DuplicatesForm do begin
     ScanState:=ssIdle;
     ShowOnStartUp:=Visible;
     if Visible then Hide;
     end;

  if (Task=otImage) and (SubTask=osSkin) then DoNotSplitImageFileGroupBox;

  Application.OnDeactivate:=oApplicationOnDeactivate; oApplicationOnDeactivate:=nil;
end;

procedure TOpenForm.DirectoryListBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with DirectoryListBox1 do begin
    if ItemIndex>=0 then OpenCurrent;
    end;
end;

procedure TOpenForm.ShowGame;
begin
  if GameViewer<>nil then GameViewer.LoadGame(Game);
end;

procedure TOpenForm.FileListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then EnterKeyDown:=True;
end;

procedure TOpenForm.FileListBox1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then begin
     if EnterKeyDown then BtnOpenClick(Sender); // 'EnterKeyDown': [Enter] was pressed while the filelist had focus
     EnterKeyDown:=False;
     end;
end;

procedure TOpenForm.CollectionStringGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; ARect: TRect; AState: TGridDrawState);
var s:String;
begin
  with CollectionStringGrid do
    begin
          if    (gdFocused in AState)
                or
                ((ARow=CurrentFileStringGridRow)
                 and
                 (Screen.ActiveForm=DuplicatesForm)
                 and
                 (ACol=1)
                )
                then
                begin Canvas.Font .Color:=CollectionHighlightTextColor; //clHighlightText;
                      Canvas.Brush.Color:=CollectionHighlightBackgroundColor; //clHighlight;
                end
//         else if ACol=0 then
//              begin Canvas.Font .Color:=clBtnText;
//                    Canvas.Brush.Color:=clBtnFace;
//              end
           else begin Canvas.Font .Color:=Font.Color; //clWindowText;
                      Canvas.Brush.Color:=Color; //clWindow;
                end;
           Canvas.FillRect(ARect);

           s:=Cells[ACol,ARow];

           if Task=otMusicPlayer then
              if ACol=1 then begin
                 if gdFocused in AState then
                    if   s='' then
                         StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=''
                    else StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=s+RightClickForMenuText;
                 s:=ExtractFileNameWithoutExt(s);
                 end;

           if s<>'' then
              begin
                Dec(ARect.Right); Dec(ARect.Bottom);
                if   ACol<=1 then
                     Windows.ExtTextOut(Canvas.Handle, ARect.Left + 2, ARect.Top + 2,
                                        ETO_CLIPPED or ETO_OPAQUE, @ARect, PChar(S), Length(S), nil)
                else Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),ARect,
                                        DT_RIGHT or DT_VCENTER or DT_SINGLELINE,nil);


              end;
{          if gdSelected in AState then
              begin Inc(ARect.Left); Inc(ARect.Top);
                    Canvas.DrawFocusRect(ARect);
              end;
}
    end;
end;

procedure TOpenForm.CollectionStringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect:=(ACol=1) or (Task=otGame);
  if CanSelect and (ActiveControl=CollectionStringGrid) and
     (CurrentCollectionFileName<>'') then with CollectionStringGrid do
     if      Task=otGame then
             if ((ARow<>Row) or GameLoadErrorMemo.Visible) and // 'Visible': no level is loaded
                (Cells[1,ARow]<>'') and
                (Cells[1,ARow]<>CurrentSectionName) then
                TryToLoadFile(MakeIniFileSectionFileName(CurrentCollectionFileName,Cells[1,ARow]))
             else
     else if Task=otMusicPlayer then
             if (ACol<>Col) or (ARow<>Row) and
                (Cells[1,ARow]<>CurrentSectionName) then
                TryToLoadFile(Cells[1,ARow]);
end;

procedure TOpenForm.CollectionStringGridEnter(Sender: TObject);
begin
  if CollectionStringGrid.Col<>1 then CollectionStringGrid.Col:=1;
  ActiveList:=CollectionStringGrid;
  BtnFileMenu.Visible:=True;
//FileListBox1.Font.Color:=clBtnText; //clWindow;
//FileListBox1.Color:=clBtnFace; //clWindowText;
end;

procedure TOpenForm.LoadGameData(LoadNotes:Boolean);
var BetterBuiltInSolutionMoves,BetterBuiltInSolutionPushes:TSnapshot;
begin
  with Game do begin
    GameSolutionPushesRadioButton .Visible:=BestSolutionPushes<>nil;
    GameSolutionPushesLabel       .Visible:=BestSolutionPushes<>nil;
    if BestSolutionPushes<>nil then with BestSolutionPushes do
       GameSolutionPushesLabel    .Caption:=Format(MovesPushesText__,[MoveTop,PushCount]);

    GameSolutionTextLabel         .Visible:=(BestSolutionMoves<>nil) and (not GameSolutionPushesLabel.Visible);
    GameSolutionMovesRadioButton  .Visible:=(BestSolutionMoves<>nil) and      GameSolutionPushesLabel.Visible;
    GameSolutionLabel             .Visible:= BestSolutionMoves<>nil;
    if BestSolutionMoves <>nil then with BestSolutionMoves do
       GameSolutionLabel          .Caption:=Format(MovesPushesText__,[MoveTop,PushCount]);

    BtnGameBuiltinSolutions       .Visible:=IsABetterBuiltInSolutionAvailable(BetterBuiltInSolutionMoves,BetterBuiltInSolutionPushes);
    if BtnGameBuiltinSolutions    .Visible then begin
       if      BetterBuiltInSolutionMoves =nil then with BetterBuiltInSolutionPushes do
               BtnGameBuiltinSolutions.Hint:=Format(HintImportBuiltInSolutionText__ ,[MoveCount,PushCount])
       else if BetterBuiltInSolutionPushes=nil then with BetterBuiltInSolutionMoves  do
               BtnGameBuiltinSolutions.Hint:=Format(HintImportBuiltInSolutionText__ ,[MoveCount,PushCount])
       else with BetterBuiltInSolutionMoves do
               BtnGameBuiltinSolutions.Hint:=Format(HintImportBuiltInSolutionsText__,[MoveCount,PushCount,BetterBuiltInSolutionPushes.MoveCount,BetterBuiltInSolutionPushes.PushCount])
       end
    else       BtnGameBuiltinSolutions.Hint:=HintNoBuiltinSolutionsAvailableText;
    MenuItemBuiltinSolutions.Hint:=BtnGameBuiltinSolutions.Hint;

    GameSolutionMovesRadioButton  .Checked:=(MainForm<>nil) and MainForm.ShowSolutionMoves;
    GameSolutionPushesRadiobutton .Checked:=not GameSolutionMovesRadioButton.Checked;

    if LoadNotes then begin
       ListToStrings(Game.Notes.Lines,GameInfoMemo.Lines);
       GameInfoMemo.Modified:=False;
       GameInfoMemo.SelStart:=0; GameInfoMemo.SelLength:=0;
       end;
    end;
end;

function  TOpenForm.CloseFile:Boolean;
var ImportBuiltInSolutions,ImportBuiltInSolutionsForMainFormGame:Boolean;
    TimeMS:TTimeMS; s:String;
begin
  if Task=otGame then begin
     ReloadCurrentGameIfNecessary;

     ImportBuiltInSolutions:=
       (CurrentFileName<>'')
       and
       (Game.SokoFileName<>'')
       and
       ((Game.BestSolutionMoves <>nil) and (Game.BestSolutionMoves .Modified)
        or // best solutions are only updated from 'OpenForm' by importing built-in best solutions
        (Game.BestSolutionPushes<>nil) and (Game.BestSolutionPushes.Modified)
       );

     Result:=(CurrentFileName='')
             or
             ((not GameInfoMemo.Modified)  and
              (not ImportBuiltInSolutions) and
              (not Game.HasModifiedSnapshots)
             );
     if not Result then begin
        Game.Notes.Modified:=GameInfoMemo.Modified;

        ImportBuiltInSolutionsForMainFormGame:=
          (CurrentFileName=BaseFileName)
          and
          (BaseFileName<>'')
          and
          ImportBuiltInSolutions;

        Result:=(not Game.Notes.Modified);
        if not Result then begin
           Result:=StringsToList(GameInfoMemo.Lines,Game.Notes.Lines);
           if Game.Notes.Lines.ReadString(KEY_TIME,s) and
              SokUtil_.StrToTime(s,TimeMS) then begin
              Game.OriginalTimeMS:=TimeMS; Game.ClearTimer;
              end;
           end;

        Result:=Result and Game.SaveToFile(CurrentFileName,True);

        if Result then begin
           GameInfoMemo.Modified:=False;
           if ImportBuiltInSolutionsForMainFormGame then
              MainForm.Game.ImportBuiltinSolutions;

           if CurrentFileName=BaseFileName then
              MainForm.Game.OriginalTimeMS:=Game.OriginalTimeMS;
           end;
        end;
     end
  else begin
     Result:=((CurrentFileName='') and (Task<>otMusicPlayer)) or
             (not IsModified);
     if not Result then begin
        if Task=otMusicPlayer then
           Result:=(CurrentCollectionFileName='')
                   or
                   PlayListSaveToFile(CurrentCollectionFileName,{CollectionStringGrid,1}'',Music.PlayList);
        if Result then IsModified:=False;
        end;
     end;
end;

procedure TOpenForm.EditPopupMenuPopup(Sender: TObject);
var i:Integer; b:Boolean;
begin
  if CurrentMemo<>nil then with CurrentMemo do begin
     if (ActiveControl<>CurrentMemo) and (Sender<>nil) then SetFocus;

     if MenuItemEdit.Checked=CurrentMemo.ReadOnly then begin
        MenuItemEdit.Checked :=not CurrentMemo.ReadOnly;
        for i:=1 to 9 do // kludge: these hard-wired numbers cover the edit-features
            EditPopupMenu.Items[i].Visible:=MenuItemEdit.Checked;
        end;

     MenuItemUndo              .Enabled:=SendMessage(Handle,EM_CANUNDO,0,0)<>0;
     b                                 :=SelLength>0;
     MenuItemCut               .Enabled:=b and (not ReadOnly);
     MenuItemCopy              .Enabled:=b;
     MenuItemPaste             .Enabled:=(Clipboard.Hasformat(CF_TEXT)) and
                                         (not ReadOnly);
     MenuItemDelete            .Enabled:=MenuItemCut.Enabled;
     MenuItemSelectAll         .Enabled:=Lines.Count<>0;
     end;

  MenuItemImageText2           .Visible:=(Task=otImage) and (SubTask=osSkin) and
                                         ImageScrollBox.Visible;
  MenuItemImageText2           .Enabled:=MenuItemImageText2.Visible and
                                         (CurrentFileName<>'') and
                                         (Image1.Picture.BitMap<>nil) and
                                         (PIXEL_BYTE_SIZE[Image1.Picture.BitMap.PixelFormat]<>0);
  MenuItemImageTextSeparator2  .Visible:=MenuItemImageText2.Visible;
  MenuItemImageText2           .Checked:=MenuItemImageText.Checked;
end;

procedure TOpenForm.MenuItemEditClick(Sender: TObject);
begin
  if (Sender<>nil) and (CurrentMemo<>nil) then begin
     CurrentMemo.ReadOnly:=not CurrentMemo.ReadOnly;
     CurrentMemo.SetFocus;
     end;
end;

procedure TOpenForm.MenuItemUndoClick(Sender: TObject);
begin
  if CurrentMemo<>nil then SendMessage(CurrentMemo.Handle,EM_UNDO,0,0);
end;

procedure TOpenForm.MenuItemCutClick(Sender: TObject);
begin
  if CurrentMemo<>nil then CurrentMemo.CutToClipBoard;
end;

procedure TOpenForm.MenuItemCopyClick(Sender: TObject);
begin
  if CurrentMemo<>nil then CurrentMemo.CopyToClipboard;
end;

procedure TOpenForm.MenuItemPasteClick(Sender: TObject);
begin
  if CurrentMemo<>nil then CurrentMemo.PasteFromClipboard;
end;

procedure TOpenForm.MenuItemDeleteClick(Sender: TObject);
begin
  if CurrentMemo<>nil then CurrentMemo.ClearSelection;
end;

procedure TOpenForm.MenuItemSelectAllClick(Sender: TObject);
begin
  if CurrentMemo<>nil then CurrentMemo.SelectAll;
end;

procedure TOpenForm.MenuItemTextColorClick(Sender: TObject);
begin
  if CurrentMemo<>nil then begin
     ColorDialog1.Color:=CurrentMemo.Font.Color;
     if ColorDialog1.Execute then CurrentMemo.Font.Color:=ColorDialog1.Color;
     end;
end;

procedure TOpenForm.MenuItemBackgroundColorClick(Sender: TObject);
begin
  if CurrentMemo<>nil then begin
     ColorDialog1.Color:=CurrentMemo.Color;
     if ColorDialog1.Execute then CurrentMemo.Color:=ColorDialog1.Color;
     end;
end;

procedure TOpenForm.MenuItemFontClick(Sender: TObject);
begin
  if CurrentMemo<>nil then begin
     CurrentMemo.HideSelection:=False;
     FontDialog1.Font.Assign(CurrentMemo.Font);
     if FontDialog1.Execute then CurrentMemo.Font.Assign(FontDialog1.Font);
     CurrentMemo.HideSelection:=True;
     end;
end;

function TOpenForm.LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
var i,j,H,W,MinHeight,MinWidth:Integer;
begin {LoadSettingsFromIniFile}
  with IniFile do begin
    MinWidth           :=CalculateMinWidth;
    MinHeight          :=CalculateMinHeight;
    if   ReadBool('OpenWindow','Maximized',WindowState=wsMaximized) then
         if   WindowState<>wsMaximized then begin DoMaximize:=True; {WindowState:=wsMaximized;} end
         else
    else if WindowState=wsMaximized then WindowState:=wsNormal;
    Left               :=Max(Screen.DeskTopLeft,Min(Screen.DeskTopLeft+Screen.DeskTopWidth -MinWidth,IniFile.ReadInteger('OpenWindow','Left',Left)));
    Top                :=Max(Screen.DeskTopTop ,Min(Screen.DeskTopTop +Screen.DeskTopHeight-MinHeight-40,IniFile.ReadInteger('OpenWindow','Top',Top)));
    Width              :=Max(MinWidth ,Min(Screen.DeskTopWidth -Left,IniFile.ReadInteger('OpenWindow','Width',Width)));
    Height             :=Max(MinHeight,Min(Screen.DeskTopHeight-Top-40,IniFile.ReadInteger('OpenWindow','Height',Height))); // -40: normally sufficient to avoid collision with start-menu
    if DoMaximize      or (WindowState=wsMaximized) then begin
       i               :=0;
       j               :=0;
       end
    else begin
       i               :=Left;
       j               :=Top;
      end;
    DiskGroupBox.Width :=Max(DiskGroupBoxMinimumWidth ,Min(Screen.DeskTopWidth -i,IniFile.ReadInteger('OpenWindow','DiskGroupBox.Width',DiskGroupBox.Width)));
    DiskGroupBox.Height:=Max(DiskGroupBoxMinimumHeight,Min(Screen.DeskTopHeight-j-40,IniFile.ReadInteger('OpenWindow','DiskGroupBox.Height',DiskGroupBox.Height)));

    W:=Max(FolderListBoxMinimumWidth,Min(Screen.DeskTopWidth-i,IniFile.ReadInteger('OpenWindow','FolderListBox.Width',DirectoryListBox1.Width)));
    FileListBox1.Left:=DirectoryListBox1.Left+W+(FileListBox1.Left-(DirectoryListBox1.Left+DirectoryListBox1.Width));
    DirectoryListBox1.Width:=W;

    H:=Max(DiskGroupBoxMinimumHeight-GameBoardPanel.Top,Min(Screen.DeskTopHeight-j,IniFile.ReadInteger('OpenWindow','GameBoardPanel.Height',GameBoardPanel.Height)));
    GameDataGroupBox.Top:=GameBoardPanel.Top+H+(GameDataGroupBox.Top-(GameBoardPanel.Top+GameBoardPanel.Height));
    GameBoardPanel.Height:=H;

    LoadComboBoxFromIniFile(IniFile,Self.Name+SUB_TITLE_SEPARATOR+           LevelsFolderHistoryComboBox.Name,MAX_FILE_HISTORY_ITEMS,True ,False,True,True,           LevelsFolderHistoryComboBox);
    LoadComboBoxFromIniFile(IniFile,Self.Name+SUB_TITLE_SEPARATOR+           LevelsFileHistoryComboBox  .Name,MAX_FILE_HISTORY_ITEMS,False,True ,True,True,           LevelsFileHistoryComboBox  );
    LoadComboBoxFromIniFile(IniFile,Self.Name+SUB_TITLE_SEPARATOR+AnythingButLevelsFolderHistoryComboBox.Name,MAX_FILE_HISTORY_ITEMS,True ,False,True,True,AnythingButLevelsFolderHistoryComboBox);
    LoadComboBoxFromIniFile(IniFile,Self.Name+SUB_TITLE_SEPARATOR+AnythingButLevelsFileHistoryComboBox  .Name,MAX_FILE_HISTORY_ITEMS,False,True ,True,True,AnyThingButLevelsFileHistoryComboBox  );

    SolutionColumns:=ReadInteger(Self.Name,'SolutionColumns',SolutionColumns);
    MenuItemIncludeBuiltinSolutions.Checked:=ReadBool(Self.Name,'IncludeBuiltinSolutions',MenuItemIncludeBuiltinSolutions.Checked);
    MenuItemDragLevels.Checked:=ReadBool(Self.Name,'DragLevels',MenuItemDragLevels.Checked);
//  MenuItemAutoScroll.Checked:=ReadBool(Self.Name,'AutoScroll',MenuItemAutoScroll.Checked);
    MenuItemCopyCollectionToClipboardWriteTitleAndAuthor.Checked := ReadBool(Self.Name,'CopyCollectionToClipboardWriteTitleAndAuthor', MenuItemCopyCollectionToClipboardWriteTitleAndAuthor.Checked );
    GameInfoMemo.ReadOnly:=ReadBool(Self.Name,'NotesReadOnly',GameInfoMemo.ReadOnly);
    MenuItemEdit.Checked:=GameInfoMemo.ReadOnly; EditPopupMenuPopup(nil);

    ToolTips.Enabled:=ReadBool   (Self.Name,'ToolTips.Enabled',ToolTips.Enabled);
    ToolTips.OffsetX:=ReadInteger(Self.Name,'ToolTips.OffsetX',ToolTips.OffsetX);
    if (ToolTips.OffsetX<-MAX_TOOL_TIPS_OFFSET_PIXELS) or
       (ToolTips.OffsetX> MAX_TOOL_TIPS_OFFSET_PIXELS) then
       ToolTips.OffsetX:=DEFAULT_TOOL_TIPS_OFFSET_PIXELS;
    ToolTips.OffsetY:=ReadInteger(Self.Name,'ToolTips.OffsetY',ToolTips.OffsetY);
    if (ToolTips.OffsetY<-MAX_TOOL_TIPS_OFFSET_PIXELS) or
       (ToolTips.OffsetY> MAX_TOOL_TIPS_OFFSET_PIXELS) then
       ToolTips.OffsetY:=DEFAULT_TOOL_TIPS_OFFSET_PIXELS;

    GameInfoMemo  .Color:=TColor(ReadInteger(GameInfoSection  ,'Color',Integer(GameInfoMemo  .Color)));
    SkinScriptMemo.Color:=TColor(ReadInteger(SkinScriptSection,'Color',Integer(SkinScriptMemo.Color)));
    TextFileMemo  .Color:=TColor(ReadInteger(TextFileSection  ,'Color',Integer(TextFileMemo  .Color)));
    Result:=LoadFontFromIniFile(IniFile,GameInfoSection  ,'',GameInfoMemo  .Font) and
            LoadFontFromIniFile(IniFile,SkinScriptSection,'',SkinScriptMemo.Font) and
            LoadFontFromIniFile(IniFile,TextFileSection  ,'',TextFileMemo  .Font) and
            LoadFontFromIniFile(IniFile,Self.Name,'Window',Self.Font);
    OnFontChange;
    end;
end;

function TOpenForm.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
begin
  with IniFile do begin
   if  WindowState=wsNormal then begin
       WriteInteger('OpenWindow','Left',Left);
       WriteInteger('OpenWindow','Top',Top);
       WriteInteger('OpenWindow','Width',Width);
       WriteInteger('OpenWindow','Height',Height);
       end;
    WriteBool('OpenWindow','Maximized',(WindowState=wsMaximized) or DoMaximize);
    IniFile.WriteInteger('OpenWindow','DiskGroupBox.Width',DiskGroupBox.Width);
    IniFile.WriteInteger('OpenWindow','DiskGroupBox.Height',DiskGroupBox.Height);
    IniFile.WriteInteger('OpenWindow','FolderListBox.Width',DirectoryListBox1.Width);
    IniFile.WriteInteger('OpenWindow','GameBoardPanel.Height',GameBoardPanel.Height);

    SaveComboBoxToIniFile(IniFile,Self.Name+SUB_TITLE_SEPARATOR+           LevelsFolderHistoryComboBox.Name,MAX_FILE_HISTORY_ITEMS,           LevelsFolderHistoryComboBox);
    SaveComboBoxToIniFile(IniFile,Self.Name+SUB_TITLE_SEPARATOR+           LevelsFileHistoryComboBox  .Name,MAX_FILE_HISTORY_ITEMS,           LevelsFileHistoryComboBox);
    SaveComboBoxToIniFile(IniFile,Self.Name+SUB_TITLE_SEPARATOR+AnythingButLevelsFolderHistoryComboBox.Name,MAX_FILE_HISTORY_ITEMS,AnythingButLevelsFolderHistoryComboBox);
    SaveComboBoxToIniFile(IniFile,Self.Name+SUB_TITLE_SEPARATOR+AnythingButLevelsFileHistoryComboBox  .Name,MAX_FILE_HISTORY_ITEMS,AnythingButLevelsFileHistoryComboBox);

    WriteInteger   (Self.Name,'SolutionColumns',SolutionColumns);
    WriteBool      (Self.Name,'IncludeBuiltinSolutions',MenuItemIncludeBuiltinSolutions.Checked);
    WriteBool      (Self.Name,'DragLevels',MenuItemDragLevels.Checked);
//  WriteBool      (Self.Name,'AutoScroll',MenuItemAutoScroll.Checked);
    WriteBool      (Self.Name,'CopyCollectionToClipboardWriteTitleAndAuthor', MenuItemCopyCollectionToClipboardWriteTitleAndAuthor.Checked );
    WriteBool      (Self.Name,'NotesReadOnly',GameInfoMemo.ReadOnly);

    WriteBool      (Self.Name,'ToolTips.Enabled',ToolTips.Enabled);
    WriteInteger   (Self.Name,'ToolTips.OffsetX',ToolTips.OffsetX);
    WriteInteger   (Self.Name,'ToolTips.OffsetY',ToolTips.OffsetY);

    WriteInteger   (GameInfoSection  ,'Color',Integer(GameInfoMemo  .Color));
    WriteInteger   (SkinScriptSection,'Color',Integer(SkinScriptMemo.Color));
    WriteInteger   (TextFileSection  ,'Color',Integer(TextFileMemo  .Color));
    Result:=SaveFontToIniFile(IniFile,GameInfoSection  ,'',GameInfoMemo  .Font) and
            SaveFontToIniFile(IniFile,SkinScriptSection,'',SkinScriptMemo.Font) and
            SaveFontToIniFile(IniFile,TextFileSection  ,'',TextFileMemo  .Font);
            SaveFontToIniFile(IniFile,Self.Name,'Window',Self.Font);
    end;
end;

procedure TOpenForm.MemoEnter(Sender: TObject);
begin
  if Sender is TMemo then CurrentMemo:=TMemo(Sender);
  BtnEditMenu.Visible:=True;
end;

procedure TOpenForm.MemoExit(Sender: TObject);
begin
  if ActiveControl<>BtnEditMenu then
     BtnEditMenu.Visible:=False;
end;

procedure TOpenForm.MemoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TMemo then CurrentMemo:=TMemo(Sender);
end;

procedure TOpenForm.BtnEditMenuClick(Sender: TObject);
begin
  EditPopupMenu.Popup(Left+BtnEditMenu.Left+20,Top+BtnEditMenu.Top+BtnEditMenu.Height+Height-ClientHeight);
  if CurrentMemo<>nil then ActiveControl:=CurrentMemo;
end;

procedure TOpenForm.BtnEditMenuMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then BtnEditMenuClick(Sender);
end;

procedure TOpenForm.ApplicationOnDeactivate(Sender: TObject);
begin
  ControlMouseUp(Sender,mbLeft,[],0,0);
  if MainForm.Music<>nil then MainForm.Music.OnDeactivateApplication;
end;

procedure TOpenForm.ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if   (Msg.Message = WM_MOUSEWHEEL) then begin
       if Msg.WParam>=0 then begin
          if ((Msg.hwnd=FileListBox1.Handle)
              or
              ((Task=otImage) and PtInRect(ImageFileGroupBox.ClientRect,ImageFileGroupBox.ScreenToClient(Mouse.CursorPos)))
             )
             and
             (FileListBox1.ItemIndex>0) then begin
             FileListBox1.ItemIndex:=Pred(FileListBox1.ItemIndex);
             FileListBox1Change(FileListBox1);
             Handled:=True;
             end
          else if (Msg.hwnd=DirectoryListBox1.Handle) and
             (DirectoryListBox1.ItemIndex>0) then begin
             //DirectoryListBox1.ItemIndex:=Pred(DirectoryListBox1.ItemIndex);
             //DirectoryListBox1.OpenCurrent;
             //Handled:=True;
             end
          else if (Msg.hwnd=CollectionStringGrid.Handle) and
             (CollectionStringGrid.Row=0) and
             (FileListBox1.ItemIndex>0) then begin
             FileListBox1.ItemIndex:=Pred(FileListBox1.ItemIndex);
             FileListBox1Change(FileListBox1);
             Handled:=True;
             end
          else if Msg.hwnd=GameInfoMemo.Handle then begin
             GameInfoMemo.Perform(EM_LINESCROLL,0,-MOUSE_WHEEL_UP_DOWN_SCROLL_LINES);
             Handled:=True;
             end
          else if (Msg.hwnd=LevelsFolderHistoryComboBox.Handle) and
                  (LevelsFolderHistoryComboBox.ItemIndex>0) then begin
             LevelsFolderHistoryComboBox.ItemIndex:=Pred(LevelsFolderHistoryComboBox.ItemIndex);
             Handled:=True;
             end
          else if (Msg.hwnd=LevelsFileHistoryComboBox.Handle) and
                  (LevelsFileHistoryComboBox.ItemIndex>0) then begin
             LevelsFileHistoryComboBox.ItemIndex:=Pred(LevelsFileHistoryComboBox.ItemIndex);
             Handled:=True;
             end
          else if (Msg.hwnd=AnythingButLevelsFolderHistoryComboBox.Handle) and
                  (AnythingButLevelsFolderHistoryComboBox.ItemIndex>0) then begin
             AnythingButLevelsFolderHistoryComboBox.ItemIndex:=Pred(AnythingButLevelsFolderHistoryComboBox.ItemIndex);
             Handled:=True;
             end
          else if (Msg.hwnd=AnythingButLevelsFileHistoryComboBox.Handle) and
                  (AnythingButLevelsFileHistoryComboBox.ItemIndex>0) then begin
             AnythingButLevelsFileHistoryComboBox.ItemIndex:=Pred(AnythingButLevelsFileHistoryComboBox.ItemIndex);
             Handled:=True;
             end
          else if (ActiveControl=CollectionStringGrid) and
                  (CollectionStringGrid.Row>CollectionStringGrid.FixedRows) and
                  (Task=otGame) and (PtInRect(GameFileGroupBox.ClientRect,GameFileGroupBox.ScreenToClient(Mouse.CursorPos))) then begin
             CollectionStringGrid.Row:=Pred(CollectionStringGrid.Row);
             Handled:=True;
             end
          else Handled:=False;
          end
       else begin
          if ((Msg.hwnd=FileListBox1.Handle)
              or
              ((Task=otImage) and PtInRect(ImageFileGroupBox.ClientRect,ImageFileGroupBox.ScreenToClient(Mouse.CursorPos)))
             )
             and
             (FileListBox1.ItemIndex>=0) and
             (FileListBox1.ItemIndex<Pred(FileListBox1.Items.Count)) then begin
             FileListBox1.ItemIndex:=Succ(FileListBox1.ItemIndex);
             FileListBox1Change(FileListBox1);
             Handled:=True;
             end
          else if (Msg.hwnd=DirectoryListBox1.Handle) and
             (DirectoryListBox1.ItemIndex>=0) and
             (DirectoryListBox1.ItemIndex<Pred(DirectoryListBox1.Items.Count)) then begin
             //DirectoryListBox1.ItemIndex:=Succ(DirectoryListBox1.ItemIndex);
             //DirectoryListBox1.OpenCurrent;
             //Handled:=True;
             end
          else if (Msg.hwnd=CollectionStringGrid.Handle) and
             (CollectionStringGrid.Row=Pred(CollectionStringGrid.RowCount)) and
             (FileListBox1.ItemIndex>=0) and
             (FileListBox1.ItemIndex<Pred(FileListBox1.Items.Count)) then begin
             FileListBox1.ItemIndex:=Succ(FileListBox1.ItemIndex);
             FileListBox1Change(FileListBox1);
             Handled:=True;
             end
          else if Msg.hwnd=GameInfoMemo.Handle then begin
             GameInfoMemo.Perform(EM_LINESCROLL,0,MOUSE_WHEEL_UP_DOWN_SCROLL_LINES);
             Handled:=True;
             end
          else if (Msg.hwnd=LevelsFolderHistoryComboBox.Handle) and
                  (LevelsFolderHistoryComboBox.ItemIndex<Pred(LevelsFolderHistoryComboBox.Items.Count)) then begin
             LevelsFolderHistoryComboBox.ItemIndex:=Succ(LevelsFolderHistoryComboBox.ItemIndex);
             Handled:=True;
             end
          else if (Msg.hwnd=LevelsFileHistoryComboBox.Handle) and
                  (LevelsFileHistoryComboBox.ItemIndex<Pred(LevelsFileHistoryComboBox.Items.Count)) then begin
             LevelsFileHistoryComboBox.ItemIndex:=Succ(LevelsFileHistoryComboBox.ItemIndex);
             Handled:=True;
             end
          else if (Msg.hwnd=AnythingButLevelsFolderHistoryComboBox.Handle) and
                  (AnythingButLevelsFolderHistoryComboBox.ItemIndex<Pred(AnythingButLevelsFolderHistoryComboBox.Items.Count)) then begin
             AnythingButLevelsFolderHistoryComboBox.ItemIndex:=Succ(AnythingButLevelsFolderHistoryComboBox.ItemIndex);
             Handled:=True;
             end
          else if (Msg.hwnd=AnythingButLevelsFileHistoryComboBox.Handle) and
                  (AnythingButLevelsFileHistoryComboBox.ItemIndex<Pred(AnythingButLevelsFileHistoryComboBox.Items.Count)) then begin
             AnythingButLevelsFileHistoryComboBox.ItemIndex:=Succ(AnythingButLevelsFileHistoryComboBox.ItemIndex);
             Handled:=True;
             end
          else if (ActiveControl=CollectionStringGrid) and
                  (CollectionStringGrid.Row<Pred(CollectionStringGrid.RowCount)) and
                  (Task=otGame) and (PtInRect(GameFileGroupBox.ClientRect,GameFileGroupBox.ScreenToClient(Mouse.CursorPos))) then begin
                  CollectionStringGrid.Row:=Succ(CollectionStringGrid.Row);
                  Handled:=True;
                  end
          else Handled:=False;
          end;
       end
  else Handled:=False;
end;

function  TOpenForm.SkinTypeDetection(const Directory__:String):Boolean;
var i,j,k,n:Integer;

  function LoadScript(Index__:Integer):Boolean;
  begin
   MainForm.Skins.ScriptFileName:=MainForm.Skins.Scripts[Index__,SKIN_SCRIPT_INDEX];
   Result:=MainForm.Skins.ScriptFileName=MainForm.Skins.Scripts[Index__,SKIN_SCRIPT_INDEX];
   if Result then begin
      SkinScriptsComboBox.ItemIndex:=Index__;
      SkinScriptsComboBoxChange(Self);
      end;
  end;

begin // SkinTypeDetection
  Result:=False;
  if (Screen.ActiveForm=Self) and (Task=otImage) and (SubTask=osSkin) then begin
     k:=0;
     for i:=0 to Pred(SkinScriptsComboBox.Items.Count) do begin
         j:=StrAnsiPosCI(MainForm.Skins.Scripts[i,SKIN_NAME_INDEX],Directory__);
         if j>k then begin
            if StrEqual(JSOKO_RES_NAME,MainForm.Skins.Scripts[i,SKIN_NAME_INDEX]) and
               StrBeginsWith(ExtractFileName(Directory__),Copy(SOKOBAN_FOR_WINDOWS_SKIN_FILE_EXT,2,MaxInt)) then with MainForm.Skins do begin
               // JSoko has a '\skins\skn' folder for 'Sokoban for Windows' skins
               n:=IndexOfScript(StrWithTrailingPathDelimiter(ScriptsPath)+StrSubstitute(SOKOBAN_FOR_WINDOWS_RES_NAME,UNDERSCORE,SPACE,j)+SKIN_SCRIPT_FILE_EXT);
               if n>=0 then
                  if (n=SkinScriptsComboBox.ItemIndex) or LoadScript(n) then begin
                     k:=j; Result:=True;
                     end;
               end;
            if (j>k) and // 'True': the skin type hasn't been detected by the preceding lines, i.e., the folder isn't the JSoko folder for 'Sokoban for Windows' skins
               LoadScript(i) then begin
               k:=j; Result:=True;
               end;
            end;
         end;
     if not Result then begin
        if StrAnsiPosCI(YSOKOBAN_PROGRAM_TITLE,Directory__)<>0 then begin // Ysokoban uses the common skin format
           i:=MainForm.Skins.IndexOfScript(MainForm.Skins.CommonSkinsScriptFileName);
           if i>=0 then Result:=(i=SkinScriptsComboBox.ItemIndex) or LoadScript(i);
           end
        else
           if (StrAnsiPosCI(SOKOBAN_FOR_WINDOWS_DEFAULT_DIRECTORY_NAME,Directory__)<>0)
              or
              StrEqual(ExtractFileExt(FileListBox1FileName),SOKOBAN_FOR_WINDOWS_SKIN_FILE_EXT)
              or
              (FileExists(StrWithTrailingPathDelimiter(Directory__)+SOKOBAN_FOR_WINDOWS_FILE_NAME_GUESS1)
               and
               FileExists(StrWithTrailingPathDelimiter(Directory__)+SOKOBAN_FOR_WINDOWS_FILE_NAME_GUESS2)
              ) then
              with MainForm.Skins do begin
                i:=IndexOfScript(StrWithTrailingPathDelimiter(ScriptsPath)+StrSubstitute(SOKOBAN_FOR_WINDOWS_RES_NAME,UNDERSCORE,SPACE,j)+SKIN_SCRIPT_FILE_EXT);
                if i>=0 then Result:=(i=SkinScriptsComboBox.ItemIndex) or LoadScript(i);
                end;
        end;
     end;
end;

function  TOpenForm.TryToSetDirectory(const Directory__:String):Boolean;
var Directory:String;
begin
  Directory:=StrWithoutTrailingPathDelimiter(Directory__);
  Result:=StrEqual(Directory,StrWithoutTrailingPathDelimiter(DirectoryListBox1.Directory));
  if (not Result) and (Directory<>'') and DirectoryExists(Directory) then begin
     DirectoryListBox1.Directory:=StrWithTrailingPathDelimiter(Directory);
     if DirectoryListBox1.Drive<>DriveComboBox1.Drive then
        DriveComboBox1.Drive:=DirectoryListBox1.Drive;
     DirectoryListBoxScrollInView(DirectoryListBox1,DirectoryListBox1.ItemIndex);
     Result:=StrEqual(Directory,StrWithoutTrailingPathDelimiter(DirectoryListBox1.Directory));
     end;
end;

procedure TOpenForm.DirectoryListBox1Change(Sender: TObject);
begin // DirectoryListBox1Change
  if (Screen.ActiveForm=Self) and DiskGroupBox.Visible and DirectoryListBox1.Visible then begin
     if MenuItemImageText.Checked then MenuItemImageTextClick(Self);
     FileListBox1.Mask:=FilterComboBox1.Mask; // necessary; otherwise 'Mask' contains the old filename from 'FileListBox1'
     if FileListBox1.Items.Count=0 then DirectoryListBox1.SetFocus
     else begin
       if (Task=otGame) and
          (FileListBox1FileName='') and
          {(FilterComboBox1.ItemIndex<=2) and}
          OnDirectoryChangeLoadFirstFileOnFileList then
          TryToLoadFile(StrWithTrailingPathDelimiter(DirectoryListBox1.Directory)+FileListBox1.Items[0]);
       if ActiveControl<>CollectionStringGrid then
          DirectoryListBox1.SetFocus; //FileListBox1.SetFocus;
       end;
     with DirectoryListBox1 do if IsMediaPresent(Drive) then;
     if DirectoryListBox1.Drive<>DriveComboBox1.Drive then DriveComboBox1.Drive:=DirectoryListBox1.Drive;

     if (Task=otImage) and (SubTask=osSkin) then SkinTypeDetection(DirectoryListBox1.Directory);

     with DirectoryListBox1 do Tag:=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // '-TOOL_TIP_FREE_ZONE_PIXELS': 'OnMouseMove' isn't triggered when the mouse is over the scrollbar, hence, this extra tooltip free zone
     end;
end;

procedure TOpenForm.BtnFileMenuClick(Sender: TObject);
begin
  if not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy) then begin
     if Task=otGame then
        FilePopupMenu.Popup(Left+BtnFileMenu.Left+20,Top+BtnFileMenu.Top+BtnFileMenu.Height+Height-ClientHeight)
     else if (Task=otMusicPlayer) and (ActiveList=CollectionStringGrid)  then
        PlayListPopupMenu.Popup(Left+BtnFileMenu.Left+20,Top+BtnFileMenu.Top+BtnFileMenu.Height+Height-ClientHeight)
     else
        FilePopupMenu.Popup(Left+BtnFileMenu.Left+20,Top+BtnFileMenu.Top+BtnFileMenu.Height+Height-ClientHeight);
     if DiskGroupBox.Visible then ActiveControl:=ActiveList;
     end;
end;

procedure TOpenForm.FileListBox1Enter(Sender: TObject);
begin
  ActiveList:=FileListBox1;
  BtnFileMenu.Visible:=True;
  with FileListBox1 do if IsMediaPresent(Drive) then;
  if FileListBox1.Drive<>DriveComboBox1.Drive then
     DriveComboBox1.Drive:=FileListBox1.Drive;
//  FileListBox1.Font.Color:=clWindowText;
//  FileListBox1.Color:=clWindow;
  with FileListBox1 do 
    if (Task<>otGame) and (Task<>otSolver) and (Task<>otOptimizer) then
       if   ItemIndex>=0 then
            StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=IntToStr(Succ(ItemIndex))+SLASH+IntToStr(Items.Count)
       else StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
end;

procedure TOpenForm.FileListBox1Exit(Sender: TObject);
begin
  if (ActiveControl<>BtnFileMenu) and
     (ActiveControl<>DirectoryListBox1) and
     (ActiveControl<>FileListBox1) and
     (ActiveControl<>CollectionStringGrid) then
     BtnFileMenu.Visible:=False;
  if (Task<>otGame) and (Task<>otSolver) and (Task<>otOptimizer) then StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
end;

procedure TOpenForm.BtnFileMenuMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then BtnFileMenuClick(Sender);
end;

procedure TOpenForm.FilePopupMenuPopup(Sender: TObject);
var i:Integer; DriveIsProbablyWriteable,HasGame:Boolean; CurrentFolder,DefaultLevelFolder:String;
begin
  PlayListPopupMenuPopup(Sender);

  i:=GetDriveType(PChar(DriveComboBox1.Drive+':\'));
  DriveIsProbablyWriteable:=(i>1) and (i<>DRIVE_CDROM);
  HasGame:=(Task=otGame) and (CurrentFileName<>'') and (Game<>nil) and
           (Game.BoardWidth>=MIN_BOARD_WIDTH) and (Game.BoardHeight>=MIN_BOARD_HEIGHT)
           //and
           //((Game.GameState<>gsNull) or (SubTask=osGeneratorCandidateSet) or (SubTask=osPluginTaskQueue)) // the game doesn't need to be playable here, hence, this test is commented out
           ;

  if      ActiveList=DirectoryListBox1 then begin
          MenuItemFileCopy                    .Enabled        :=False; //DirectoryListBox1.ItemIndex<>-1
          MenuItemFileMove                    .Enabled        :=DriveIsProbablyWriteable and (DirectoryListBox1.ItemIndex<>-1);
          MenuItemFileDelete                  .Enabled        :=MenuItemFileMove.Enabled;
          MenuItemFileRename                  .Enabled        :=MenuItemFileDelete.Enabled;
          end
  else if ActiveList=FileListBox1 then begin
          MenuItemFileCopy                    .Enabled        :=(FileListBox1.ItemIndex<>-1) and (FileListBox1FileName<>'');
          MenuItemFileMove                    .Enabled        :=DriveIsProbablyWriteable and MenuItemFileCopy.Enabled;
          MenuItemFileDelete                  .Enabled        :=MenuItemFileMove.Enabled;
          MenuItemFileRename                  .Enabled        :=MenuItemFileDelete.Enabled;
          end
  else if ActiveList=CollectionStringGrid then begin
          MenuItemFileCopy                    .Enabled        :=CollectionStringGrid.Row<>-1;
          MenuItemFileMove                    .Enabled        :=DriveIsProbablyWriteable and MenuItemFileCopy.Enabled;
          MenuItemFileDelete                  .Enabled        :=DriveIsProbablyWriteable and (CollectionStringGrid.RowCount>1);
          MenuItemFileRename                  .Enabled        :=DriveIsProbablyWriteable;
          MenuItemSolutionMoves               .Visible        :=MainForm.ShowSolutionMoves;
          MenuItemSolutionPushes              .Visible        :=not MenuItemSolutionMoves.Visible;
          MenuItemSolutionMovesAndPushes      .Visible        :=MenuItemSolutionMoves.Visible;
          MenuItemSolutionPushesAndMoves      .Visible        :=not MenuItemSolutionMovesAndPushes.Visible;
          MenuItemSolutionMoves               .Checked        :=SolutionColumns=1;
          MenuItemSolutionPushes              .Checked        :=SolutionColumns=1;
          MenuItemSolutionMovesAndPushes      .Checked        :=SolutionColumns=2;
          MenuItemSolutionPushesAndMoves      .Checked        :=SolutionColumns=2;
          end
  else    begin
          MenuItemFileCopy                    .Enabled        :=False;
          MenuItemFileMove                    .Enabled        :=False;
          MenuItemFileDelete                  .Enabled        :=False;
          MenuItemFileRename                  .Enabled        :=False;
          end;

  MenuItemClipboardImportSeparator            .Visible        :=Task=otGame;
  MenuItemSubMenuCopyToClipboard              .Visible        :=MenuItemClipboardImportSeparator.Visible;
  MenuItemCopyLevelToClipboard                .Visible        :=MenuItemClipboardImportSeparator.Visible;
  MenuItemSubMenuClipboardCollection          .Visible        :=MenuItemClipboardImportSeparator.Visible and CollectionGroupBox.Visible;
  MenuItemCopySolutionToClipboard             .Visible        :=HasGame and
                                                                (Game.BestSolutionPushes=nil);
  MenuItemCopySolutionMovesToClipboard        .Visible        :=HasGame and (not MenuItemCopySolutionToClipboard.Visible);
  MenuItemCopySolutionPushesToClipboard       .Visible        :=HasGame and MenuItemCopySolutionMovesToClipboard.Visible;
  MenuItemCopyCollectionLevelsToClipboard     .Visible        :=MenuItemSubMenuClipboardCollection.Visible;
  MenuItemCopyCollectionSolutionsToClipboard  .Visible        :=MenuItemCopyCollectionLevelsToClipboard.Visible;
  MEnuItemSubMenuImportFromClipboard          .Visible        :=MenuItemClipboardImportSeparator.Visible;
  MenuItemImportFromClipboard                 .Visible        :=MenuItemClipboardImportSeparator.Visible;

  MenuItemCopyLevelToClipboard                .Enabled        :=HasGame;
  MenuItemCopySolutionToClipboard             .Enabled        :=MenuItemCopySolutionToClipboard.Visible and
                                                               ((Game.BestSolutionMoves <>nil) or
                                                                (Game.BestSolutionPushes<>nil));
  MenuItemSubMenuClipboardCollection          .Enabled        :=MenuItemClipboardImportSeparator.Visible and CollectionGroupBox.Visible;
  MenuItemCopyCollectionLevelsToClipboard     .Enabled        :=MenuItemFileCopy.Enabled and CollectionGroupBox.Visible;
  MenuItemCopyCollectionSolutionsToClipboard  .Enabled        :=MenuItemCopyCollectionLevelsToClipboard.Enabled;
  MenuItemImportFromClipboard                 .Enabled        :=Clipboard.Hasformat(CF_TEXT);
  MenuItemCreateLevelsUsingSolutionsFromClipboard
                                              .Enabled        :=MenuItemImportFromClipboard.Enabled;

  MenuItemSubMenuFindDuplicateLevels          .Visible        :=(Task=otGame);
  MenuItemSubMenuFindDuplicateLevels          .Enabled        :=(Task=otGame) and (SubTask=osNone);
  MenuItemFindDuplicateLevelsCurrentLevel     .Enabled        :=HasGame;
  CurrentFolder                                               :=StrWithTrailingPathDelimiter(FileListBox1.Directory);
  DefaultLevelFolder                                          :=StrWithTrailingPathDelimiter(MainForm.DefaultLevelFolder);
{
  if   StrEqual(DefaultLevelFolder,CurrentFolder) or           // 'True': the current folder is the default level folder
       StrBeginsWith(CurrentFolder,DefaultLevelFolder) then    // 'True': the current folder is a subfolder of the default level folder
       MenuItemFindDuplicateLevelsCurrentLevel .Hint           :=HintDuplicateFinderSearchPathText[2]
  else if   StrBeginsWith(DefaultLevelFolder,CurrentFolder) then // 'True': the default level folder is a subfolder of the current folder
            MenuItemFindDuplicateLevelsCurrentLevel.Hint     :=HintDuplicateFinderSearchPathText[0]
       else MenuItemFindDuplicateLevelsCurrentLevel.Hint     :=HintDuplicateFinderSearchPathText[1]; // the current folder and the default folder are mutually exclusive
}
  MenuItemFindDuplicateLevelsCurrentCollection.Enabled        :=//CollectionGroupBox.Visible and (CollectionStringGrid.RowCount>1);
                                                                MenuItemFindDuplicateLevelsCurrentLevel.Enabled;
  MenuItemFindDuplicateLevelsCurrentCollection.Hint           :=MenuItemFindDuplicateLevelsCurrentLevel.Hint;
  MenuItemFindDuplicateLevelsAllLevels        .Enabled        :=True;
  MenuItemFindDuplicateLevelsAllLevels        .Hint           :=MenuItemFindDuplicateLevelsCurrentLevel.Hint;
  MenuItemSubMenuFileConversion               .Visible        :=(Task=otGame);
  MenuItemFileReorganize                      .Visible        :=HasGame and
                                                                (Game.SokoFile<>nil) and
                                                                (Game.SokoFile.Levels.Count<>0);

  MenuItemFileReorganizeSeparator             .Visible        :=MenuItemFileReorganize.Visible
                                                                or
                                                                MenuItemSubMenuFileConversion.Visible;

  MenuItemBuiltinSolutions                    .Visible        :=Task=otGame;
  MenuItemBuiltinSolutionsSeparator           .Visible        :=MenuItemBuiltinSolutions.Visible;
  MenuItemBuiltinSolutions                    .Enabled        :=BtnGameBuiltinSolutions.Visible;

  MenuItemSubMenuSolutions                    .Visible        :=ActiveList=CollectionStringGrid;
  MenuItemDragLevels                          .Visible        :=MenuItemSubMenuSolutions.Visible;

  MenuItemAutoScroll                          .Visible        :=False;

  MenuItemCollectionSeparator                 .Visible        :=MenuItemSubMenuSolutions.Visible or
                                                                MenuItemAutoScroll.Visible;

  MenuItemImageText                           .Visible        :=(Task=otImage) and (SubTask=osSkin) and
                                                                ImageScrollBox.Visible and
                                                                SkinScriptGroupBox.Visible;
  MenuItemImageText                           .Enabled        :=MenuItemImageText.Visible and
                                                                (CurrentFileName<>'') and
                                                                (Image1.Picture.BitMap<>nil) and
                                                                (PIXEL_BYTE_SIZE[Image1.Picture.BitMap.PixelFormat]<>0);
  MenuItemInitializeSkinForCaptureTool        .Visible        :=MenuItemImageText.Visible;
  MenuItemInitializeSkinForCaptureTool        .Enabled        :=MenuItemImageText.Enabled;
  MenuItemInitializeSkinWithColumnsAndRows    .Visible        :=MenuItemInitializeSkinForCaptureTool.Visible;
  MenuItemInitializeSkinWithColumnsAndRows    .Enabled        :=MenuItemInitializeSkinForCaptureTool.Enabled;
  MenuItemImageSeparator                      .Visible        :=MenuItemImageText.Visible;

  with MenuItemDuplicateLevelsFolderSeparator do begin
                                               Visible        :=MenuIndex<>0;
    MenuItemDuplicateLevelsClearFolderList    .Enabled        :=Visible;
    end;

end;

procedure TOpenForm.DirectoryListBox1Enter(Sender: TObject);
begin
  ActiveList:=DirectoryListBox1;
  BtnFileMenu.Visible:=True;
  with DirectoryListBox1 do if IsMediaPresent(Drive) then;
  if DirectoryListBox1.Drive<>DriveComboBox1.Drive then
     DriveComboBox1.Drive:=DirectoryListBox1.Drive;
end;

function TOpenForm.DiskCurrentItem:String;
begin
  if      ActiveList=DirectoryListBox1 then
          Result:=DirectoryListBox1.Directory
  else if ActiveList=FileListBox1 then
          Result:=FileListBox1FileName
  else if ActiveList=CollectionStringGrid then
          Result:=CurrentSectionName
  else    Result:='';
end;

function TOpenForm.DiskCurrentItemType:TFileTaskItemType;
begin
  if      ActiveList=DirectoryListBox1 then
          Result:=ftitFolder
  else if ActiveList=FileListBox1 then
          Result:=ftitFile
  else if ActiveList=CollectionStringGrid then
          Result:=ftitLevel
  else    Result:=ftitUndefined;
end;

procedure TOpenForm.DeleteCurrentCollectionStringGridItem;
var i,j:Integer; s:String;
begin
  with CollectionStringGrid do begin
    if Screen.ActiveForm=DuplicatesForm then CurrentCollectionFileName:=''; // this is a delete operation launched from the duplicates form; don't load another level
    CurrentSectionName:=''; CurrentFileName:=''; Game.Clear;
    if RowCount=1 then begin
       for j:=1 to Pred(ColCount) do Cells[j,0]:='';
       end
    else begin
       for i:=Row to Pred(RowCount) do
           for j:=1 to Pred(ColCount) do Cells[j,i]:=Cells[j,Succ(i)];
       RowCount:=Pred(RowCount);
       if CurrentCollectionFileName<>'' then begin
          CurrentSectionName:=Cells[1,Row];
          CurrentFileName:=MakeIniFileSectionFileName(CurrentCollectionFileName,CurrentSectionName);
          end;
       end;
    s:=CurrentFileName;
    FileListBox1.Update;
    FileManagerTasksHaveBeenPerformed:=True;
    TryToLoadFile(s);
    end
end;

procedure TOpenForm.MenuItemFileCopyClick(Sender: TObject);
var s,s1,s2:String;
begin
  s:=DiskCurrentItem;
  if   (ActiveList=CollectionStringGrid) and (Task=otGame) then
       s1:=ExtractFilePath(CurrentCollectionFileName)+s+SOKOBAN_FILE_NAME_EXT
  else s1:=s;
  if ((Task<>otGame) or CloseFile) and
     FileForm.Perform(ftCopy,DiskCurrentItemType,s,s1,s2,
                     (Task=otGame) and (ActiveList=CollectionStringGrid),
                     (Task=otGame) and (ActiveList=FileListBox1        ) and (CurrentFileName<>'') and (Game.SokoFile<>nil) and (Game.SokoFile.Levels.Count=1),
                     CurrentCollectionFileName,CurrentFileName) then begin
     UpdateAfterFileManagerTask(s2);
     end;
end;

procedure TOpenForm.MenuItemFileMoveClick(Sender: TObject);
var s,s1,s2:String;
begin
  s:=DiskCurrentItem;
  if   ActiveList=CollectionStringGrid then // disabled in menu
       s1:=ExtractFilePath(CurrentCollectionFileName)+s+SOKOBAN_FILE_NAME_EXT
  else s1:=s;
  if ((Task<>otGame) or CloseFile) and
     FileForm.Perform(ftMove,DiskCurrentItemType,s,s1,s2,
                     (Task=otGame) and (ActiveList=CollectionStringGrid),
                     (Task=otGame) and (ActiveList=FileListBox1        ) and (CurrentFileName<>'') and (Game.SokoFile<>nil) and (Game.SokoFile.Levels.Count=1),
                      CurrentCollectionFileName,CurrentFileName) then begin
     if ActiveList=CollectionStringGrid then begin
        DeleteCurrentCollectionStringGridItem;
        CurrentFileName:=s2;
        end
     else begin
        if (ActiveList=FileListBox1) and (Task=otGame) then begin
           if   CurrentSectionName<>'' then begin
                CurrentCollectionFileName:=s2;
                CurrentFileName:=MakeIniFileSectionFileName(CurrentCollectionFileName,CurrentSectionName);
                end
           else CurrentFileName:=s2;
           end;
        end;
     UpdateAfterFileManagerTask(s2);
     end;
end;

procedure TOpenForm.MenuItemFileDeleteClick(Sender: TObject);
var s:String;
begin
  if ((Task<>otGame) or CloseFile) and
     ((ActiveList<>CollectionStringGrid) or
      (CollectionStringGrid.RowCount>1)) and
      (DiskCurrentItem<>'') and
     FileForm.Perform(ftDelete,DiskCurrentItemType,DiskCurrentItem,'',s,
                     (Task=otGame) and (ActiveList=CollectionStringGrid),
                     (Task=otGame) and (ActiveList=FileListBox1        ) and (CurrentFileName<>'') and (Game.SokoFile<>nil) and (Game.SokoFile.Levels.Count=1),
                     CurrentCollectionFileName,CurrentFileName) then begin
     if ActiveList=CollectionStringGrid then
        DeleteCurrentCollectionStringGridItem
     else begin
        if (Task=otGame) and (AnsiCompareText(CurrentFileName,BaseFileName)=0) then
           BaseFileName:='';
        UpdateAfterFileManagerTask('');
        end;
     end;
end;

procedure TOpenForm.MenuItemFileRenameClick(Sender: TObject);
var s:String;
begin
  if ((Task<>otGame) or CloseFile) and
     FileForm.Perform(ftRename,DiskCurrentItemType,DiskCurrentItem,'',s,
                     (Task=otGame) and (ActiveList=CollectionStringGrid),
                     (Task=otGame) and (ActiveList=FileListBox1        ) and (CurrentFileName<>'') and (Game.SokoFile<>nil) and (Game.SokoFile.Levels.Count=1),
                     CurrentCollectionFileName,CurrentFileName) then begin
     if   ActiveList=CollectionStringGrid then begin
          if   (Task=otGame) and (not IsAnIniFilesectionFileName(s)) then
               CurrentFileName:=MakeIniFileSectionFileName(CurrentCollectionFileName,s)
          else CurrentFileName:=s;
          CurrentCollectionFileName:=''; CurrentSectionName:='';
          end
     else if (ActiveList=FileListBox1) and (Task=otGame) then begin
             if   CurrentSectionName<>'' then begin
                  CurrentCollectionFileName:=s;
                  CurrentFileName:=MakeIniFileSectionFileName(CurrentCollectionFileName,CurrentSectionName);
                  end
             else CurrentFileName:=s;
             end;
     UpdateAfterFileManagerTask(s);
     end;
end;

procedure TOpenForm.MenuItemNewFolderClick(Sender: TObject);
var s:String;
begin
  if ((Task<>otGame) or CloseFile) and
     FileForm.Perform(ftNewFolder,DiskCurrentItemType,StrWithTrailingPathDelimiter(FileListBox1.Directory)+NewFolderText,'',s,False,False,'','') then begin
     DirectoryListBox1.ItemIndex:=DirectoryListBox1.ItemIndex+1;
     UpdateAfterFileManagerTask('');
     end;
end;

procedure TOpenForm.MenuItemNewLevelCollectionClick(Sender: TObject);
var s,s1:String;
begin
  if CloseFile then
     if Game.MakeNewFileName(FileListBox1.Directory,TEXT_LEVELS,s) then
        if FileForm.Perform(ftNewPackFile,DiskCurrentItemType,s,'',s1,False,False,'','') then begin
           UpdateAfterFileManagerTask(s1);
           if TryToLoadFile(s1) then FileListBox1.FileName:=s1;
           end
        else
     else Msg(DiskFullText,'',MB_OK+MB_ICONERROR);
end;

procedure TOpenForm.UpdateAfterFileManagerTask(const FileName__:String);
var i,j:Integer; s,s1:String; oActiveControl:TWinControl;
begin
  oActiveControl:=ActiveControl;
  s:=CurrentFileName; CurrentFileName:='';

  with DirectoryListBox1 do begin
    if (not DirectoryExists(Directory)) and DirectoryExists(FileName__) then
       Directory:=FileName__;
    end;

  if ActiveList=DirectoryListBox1 then with DirectoryListBox1 do begin
     Update; j:=-1;
     for i:=0 to Pred(ItemIndex) do
         if DirectoryExists(StrWithoutTrailingPathDelimiter(GetItemPath(i))) then j:=i;
     if j<>0 then begin
        ItemIndex:=j; if ItemIndex>=0 then OpenCurrent;
        end;
     end
  else begin
     if IsAnIniFileSectionFileName(s) then s1:=ExtractIniFileName(s)
     else s1:=s;
     s1:=StrWithoutTrailingPathDelimiter(ExtractFilePath(s1));
     if (s1<>'') and
        (AnsiCompareText(s1,StrWithoutTrailingPathDelimiter(DirectoryListBox1.Directory))<>0) and
        (DirectoryExists(s1)) then
        DirectoryListBox1.Directory:=StrWithTrailingPathDelimiter(s1);
     DirectoryListBox1.Update;
     end;

  FileListBox1.Update;

  CurrentFileName:=s;
  FileManagerTasksHaveBeenPerformed:=True;

  if Task=otGame then begin
     CurrentCollectionFileName:='';
     if (Screen.ActiveForm<>DuplicatesForm)
        and
        (FileExists(CurrentFileName)
         or
         FileExists(ExtractIniFileName(CurrentFileName))
        ) then begin
        if FileExists(ExtractIniFileName(CurrentFileName)) and
           TryToLoadFile(MakeIniFileSectionFileName(
             ExtractIniFileName(CurrentFileName),
             ExtractSectionName(CurrentFileName))) then //
        else TryToLoadFile(CurrentFileName);
        end
     else begin
        CurrentFileName:='';
        TryToLoadFile('');
        end;
     MainForm.SokoFile.Flush;
     end
  else begin
     if FileExists(CurrentFileName) then begin
        TryToLoadFile(CurrentFileName);
        end
     else begin
        TryToLoadFile('');
        end;
     end;

  UpdateCollectionHintText;

  if SubTask=osSkin then begin
     SkinScriptsComboBoxEnter(nil); SkinsComboBoxEnter(nil);
     SkinScriptsComboBoxChange(Self);
     end;

  if DirectoryListBox1.Drive<>DriveComboBox1.Drive then DriveComboBox1.Drive:=DirectoryListBox1.Drive;

  if oActiveControl.Visible then ActiveControl:=oActiveControl;
end;

procedure TOpenForm.DirectoryListBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//var i:Integer;
begin
  if Button=mbRight then with DirectoryListBox1 do begin
{    i:=ItemAtPos(Point(X,Y),False);
     if Succ(i)>Items.Count then Dec(i);
     ItemIndex:=i; if ItemIndex>=0 then OpenCurrent;}
     ActiveList:=DirectoryListBox1;
     BtnFileMenu.Visible:=True;
     BtnFileMenuClick(Sender);
     end;
end;

procedure TOpenForm.FileListBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ModalResult=mrNone then // a double-click may have closed the form, before the program gets here
     if      Button=mbLeft then with FileListBox1 do begin
             if (Task=otMusicPlayer) and (ItemIndex>=0) and
                (AnsiCompareText(ExtractFileExt(Items[ItemIndex]),PLAYLIST_FILE_EXT)<>0) then
                BeginDrag(False);
             end
     else if Button=mbRight then with FileListBox1 do begin
             ActiveList:=FileListBox1;
             BtnFileMenu.Visible:=True;
             BtnFileMenuClick(Sender);
             end;
end;

procedure TOpenForm.DirectoryListBox1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var i:Integer;
begin
   i:=DirectoryListBox1.ItemAtPos(Point(X, Y),True);
   Accept := (Source = FileListBox1) and
             (i >= 0) and
             (DirectoryListBox1.ItemIndex <> i);
end;

procedure TOpenForm.DirectoryListBox1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var i:Integer; s:String;
begin
  i:=DirectoryListBox1.ItemAtPos(Point(X,Y),True);
  if (Source=FileListBox1) and (i>=0) and ((Task<>otGame) or CloseFile) and
     FileForm.Perform(ftMove,DiskCurrentItemType,FileListBox1FileName,StrWithTrailingPathDelimiter(DirectoryListBox1.GetItemPath(i))+ExtractFileName(FileListBox1FileName),s,False,False,'','') then
     UpdateAfterFileManagerTask(s);
end;

procedure TOpenForm.FileListBox1EndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
   if (Target <> nil) and (Task<>otMusicPlayer) then FileListBox1.Update;
end;

procedure TOpenForm.FileListBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FileListBox1.Dragging then FileListBox1.EndDrag(False);

  if      (Task=otGame) and
          IsAnIniFileSectionFileName(CurrentFileName) then begin
          if AnsiCompareText(ExtractIniFileName(CurrentFileName),FileListBox1FileName)<>0 then
             TryToLoadFile(FileListBox1FileName); // kludge: sometimes, 'FileListBox1Change' isn't triggered, in particular when the file selected at open-window-time is re-selected; reason unknown
          end
  else if AnsiCompareText(CurrentFileName,FileListBox1FileName)<>0 then
          TryToLoadFile(FileListBox1FileName);    // kludge: sometimes, 'FileListBox1Change' isn't triggered, in particular when the file selected at open-window-time is re-selected; reason unknown
end;

procedure TOpenForm.CollectionStringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ACol,ARow:Integer;
begin
  if Button=mbLeft then with CollectionStringGrid do begin
     if RowCount>1 then
        if Task=otGame then
           if Cells[1,Row]=CurrentSectionName then BeginDrag(False)
           else
        else if Task=otMusicPlayer then begin
                MouseToCell(X,Y,DragPoint.X,DragPoint.Y);
                if (DragPoint.X=1) and (DragPoint.Y<RowCount) then
                   BeginDrag(False);
                end;
     end
  else if Button=mbRight then with CollectionStringGrid do begin
     MouseToCell(X,Y,ACol,ARow); if ARow<>Row then Row:=ARow;
     ActiveList:=CollectionStringGrid;
     BtnFileMenu.Visible:=True;
     BtnFileMenuClick(Sender);
     end;
end;

procedure TOpenForm.CollectionStringGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with CollectionStringGrid do if Dragging then EndDrag(False);
end;

procedure TOpenForm.CollectionStringGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var Acol,ARow:Integer;
begin
  with CollectionStringGrid do begin
    MouseToCell(X,Y,ACol,ARow);
    //Accept:=False and (Source=CollectionStringGrid) and (ARow<>Row); // disabled
    Accept:=(ARow>=FixedRows) and (ARow<RowCount) and
            (
             ((Task=otMusicPlayer) and
              ((Source=FileListBox1) or (Source=CollectionStringGrid))
             )
             or
             ((Task=otGame) and (Source=CollectionStringGrid) and MenuItemDragLevels.Checked)
            );
    if   (ARow=TopRow) and (TopRow>0) then
         TopRow:=TopRow-1
    else if (ARow>=Pred((TopRow+VisibleRowCount))) and
            (TopRow+VisibleRowCount<RowCount) then
            TopRow:=TopRow+1;
    end;
end;

procedure TOpenForm.CollectionStringGridDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var i,ACol,ARow:Integer; s:String; Level,AfterLevel:TNode;
begin
  if  (Task=otMusicPlayer) then with CollectionStringGrid do begin
      MouseToCell(X,Y,ACol,ARow);
      if Source=FileListBox1 then begin
         if (ARow=Pred(RowCount)) and
           (Y>=(GridLineWidth+DefaultRowHeight)*(ARow-TopRow)+(DefaultRowHeight*8 div 10)) then
           ARow:=RowCount;
         if ARow=-1 then ARow:=RowCount;

         with FileListBox1 do
           if (ItemIndex>=0) and
              PlayListAddItem(ARow,
                              StrWithTrailingPathDelimiter(Directory)+Items[ItemIndex]) then begin
              CollectionStringGrid.Row:=Min(ARow,Pred(CollectionStringGrid.RowCount));
              TryToLoadFile(CollectionStringGrid.Cells[1,CollectionStringGrid.Row]);
              end
           else TryToLoadFile('');
         end
      else if (Source=CollectionStringGrid) and (ARow<>DragPoint.Y) then begin
              s:=Cells[1,DragPoint.Y];

              if      ARow<DragPoint.Y then
                      for  i:=DragPoint.Y downto Succ(ARow) do Cells[1,i]:=Cells[1,Pred(i)]
              else if ARow>DragPoint.Y then
                      for  i:=DragPoint.Y     to Pred(ARow) do Cells[1,i]:=Cells[1,Succ(i)];

              Cells[1,ARow]:=s;
              Row:=ARow;
              IsModified:=True;
              end;
      end
  else if (Task=otGame) and (Source=CollectionStringGrid) and
          MenuItemDragLevels.Checked and
          MainForm.SokoFile.Open(CurrentCollectionFileName) then
     with CollectionStringGrid do begin
       MouseToCell(X,Y,ACol,ARow);
       if (Row>ARow) then Dec(ARow);
       if        ARow<FixedRows then
                 AfterLevel:=nil
       else if   ARow<RowCount then
                 AfterLevel:=MainForm.SokoFile.Levels.GetItemByName(Cells[1,ARow])
            else AfterLevel:=MainForm.SokoFile.Levels.Last;

       Level:=MainForm.SokoFile.Levels.GetItemByName(Cells[1,Row]);
       if (Level<>nil) and (Row<>ARow) then with MainForm.SokoFile do begin
          Levels.MoveAfter(Level,AfterLevel); Modified:=True; Flush;
          s:=MakeIniFileSectionFileName(CurrentCollectionFileName,Level.Name);
          CurrentCollectionFileName:='';
          ActiveControl:=BtnHelp; // kludge: this avoids that 'CollectionStringGridSelectCell' loads the game at the old position in the grid
          if not TryToLoadFile(s) then
             TryToLoadFile('');
          end;
       end;
  if CollectionStringGrid.Enabled and
     CollectionStringGrid.Visible then ActiveControl:=CollectionStringGrid;
end;

procedure TOpenForm.GameSolutionRadioButtonClick(Sender: TObject);
var oShowSolutionMoves:Boolean;
begin
  if MainForm<>nil then begin
     oShowSolutionMoves:=MainForm.ShowSolutionMoves;

     MainForm.ShowSolutionMoves:=Sender=GameSolutionMovesRadioButton;

     if oShowSolutionMoves<>MainForm.ShowSolutionMoves then begin
        ClearSolutionStatistics;
        ShowSolutionsForVisibleLevels;
        if SnapshotsForm<>nil then SnapshotsForm.LoadBestSolutions;
        end;
     end;
end;

function  TOpenForm.FindPriorNextFileName(const FileName:String; Prior:Boolean; var ItemIndex:Integer):String;
var i:Integer; FileNameWithoutPath,Path:String;
begin // please note that the function is not fully implemented for disappearing medias;
      // the function is only used for file browsing, and correctness is not a vital issue here, so this will have to do
  if FileName='' then Result:=FileName
  else begin
     Path:=ExtractFilePath(FileName);

     if ItemIndex<0 then begin
        FileNameWithoutPath:=Copy(FileName,Succ(Length(Path)),Length(FileName));
        if Path='' then Path:=StrWithTrailingPathDelimiter(GetCurrentDir);
        if (Length(Path)>1) and (Path[2]=COLON) and IsMediaPresent(Path[1]) then begin
           FileListBox1.Drive:=Path[1];
           if   Length(Path)>3 then
                FileListBox1.Directory:=StrWithoutTrailingPathDelimiter(Path)
           else FileListBox1.Directory:=Path; // a root directory like "C:\" requires the trailing path delimiter
{
           s:=ExtractFileExt(FileName);
           if   s='' then
                FileListBox1.Mask:=ALL_FILES_FILTER
           else if StrEqual(s,SOKOBAN_FILE_NAME_EXT) or StrEqual(s,XSB_FILE_NAME_EXT) then // Sokoban file-types
                FileListBox1.Mask:=Star+SOKOBAN_FILE_NAME_EXT+SEMICOLON+Star+XSB_FILE_NAME_EXT
           else FileListBox1.Mask:=STAR+s;
}
           FileListBox1.Mask:=ALL_FILES_FILTER;
           FileListBox1.Update;
           for i:=0 to Pred(FileListBox1.Items.Count) do
               if AnsiCompareText(FileNameWithoutPath,FileListBox1.Items[i])=0 then begin
                  ItemIndex:=i; break;
                  end;
           end;
        end;

     if   Prior then
          if   ItemIndex=0 then ItemIndex:=Pred(FileListBox1.Items.Count)
          else ItemIndex:=Pred(ItemIndex)
     else if   ItemIndex>=Pred(FileListBox1.Items.Count) then ItemIndex:=0
          else ItemIndex:=Succ(ItemIndex);

     if   (ItemIndex>=0) and (ItemIndex<FileListBox1.Items.Count) then
          Result:=Path+FileListBox1.Items[ItemIndex]
     else Result:='';
     end;
end;

procedure TOpenForm.DriveComboBox1Change(Sender: TObject);
var i:Integer;
begin
  if not IsMediaPresent(DriveComboBox1.Drive) then; //

  with DriveComboBox1 do begin

    if (ItemIndex>=0) and (DriveHistory<>nil) then begin
       for i:=DriveHistory.Count to ItemIndex do DriveHistory.Add('');

       if (ItemIndex<DriveHistory.Count) and
          (DriveHistory.Strings[ItemIndex]='') then begin
          if (Task=otGame) or (Task=otImage) or (Task=otPalette) then
               if (MainForm.ApplicationDataPath<>'') and
                  (UpCase(Drive)=UpCase(MainForm.ApplicationDataPath[1])) then
                  DriveHistory.Strings[ItemIndex]:=StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath)
               else
          else if (MainForm.Music.MusicFilePath<>'') and
                  (UpCase(Drive)=UpCase(MainForm.Music.MusicFilePath[1])) then
                  DriveHistory.Strings[ItemIndex]:=StrWithTrailingPathDelimiter(MainForm.Music.MusicFilePath);
          end;

       if (Sender<>nil) and
          (ItemIndex<DriveHistory.Count) and
          (DriveHistory.Strings[ItemIndex]<>'') and
          (DriveHistory.Strings[ItemIndex]<>FileListBox1FileName) then begin
          if   DriveHistory.Strings[ItemIndex][Length(DriveHistory.Strings[ItemIndex])]=FILE_NAME_PATH_DELIMITER then
               FileListBox1.Directory:=StrWithoutTrailingPathDelimiter(DriveHistory.Strings[ItemIndex])
          else if FileExists(DriveHistory.Strings[ItemIndex]) and (not IsLoading) then
                  FileListBox1.FileName :=DriveHistory.Strings[ItemIndex]
               else begin
                  end;
          FileListBox1.Mask:=FilterComboBox1.Mask;
          end;
       end;
    end;

end;

function  TOpenForm.InitTask(Task__:TOpenTask; SubTask__:TOpenSubTask;
                             const Caption__,TopPanelCaption__,
                                   FileName__,Filter__,DefaultImageFileName__:String;
                             const Rect__:TRect;
                             BackgroundColor__:TColor;
                             FixedDefaultImageSize__,
                             SelectionEnabled__:Boolean;
                             const DefaultRect__:TRect):Boolean;
var i:Integer; s:String; oTask:TOpenTask;
begin
  Result:=True; oTask:=Task; Task:=Task__; SubTask:=SubTask__;
  Caption:=Application.Title+SUB_TITLE_SEPARATOR+Caption__;
  TopPanel.Caption:=TopPanelCaption__+SPACE;
  PlayListOk:=False; LoadPackFileIndexCount:=0;

  FilterComboBox1.Filter:=Filter__;
  if      Task=otGame        then FilterComboBox1.ItemIndex:=FilterIndexGame
  else if Task=otImage       then
          if SubTask<>osSkin then FilterComboBox1.ItemIndex:=FilterIndexImage
          else                    FilterComboBox1.ItemIndex:=FilterIndexSkin
  else if Task=otPalette     then FilterComboBox1.ItemIndex:=FilterIndexPalette
  else if Task=otSolver      then FilterComboBox1.ItemIndex:=FilterIndexPlugin
  else if Task=otOptimizer   then FilterComboBox1.ItemIndex:=FilterIndexPlugin
  else                            FilterComboBox1.ItemIndex:=FilterIndexSound;

  BaseFileName:=FileName__;
  DefaultImageFileName:=DefaultImageFileName__;

  if not DiskGroupBox.Visible then DiskGroupBox.Visible:=True;
  GameFileGroupBox      .Visible:=Task=otGame;
  CollectionGroupBox    .Visible:=Task in [otGame,otMusicPlayer];
  SoundGroupBox         .Visible:=Task in [otSound,otMusic,otMusicPath,otMusicPlayer];
  PluginGroupBox        .Visible:=(Task=otSolver) or (Task=otOptimizer);
  ImageGroupBox         .Visible:=(Task in [otImage,otPalette]) and (SubTask<>osSkin);
  SkinScriptGroupBox    .Visible:=(Task=otImage) and (SubTask=osSkin);

  MenuItemNewLevelCollection.Visible:=Task=otGame;
  MenuItemNewPlaylist.Visible:=Task=otMusicPlayer;
  MenuItemNewTextFile.Visible:=(Task=otImage) and (SubTask=osSkin);

  LevelsFolderHistoryComboBox           .Visible:=Task=otGame;
  LevelsFileHistoryComboBox             .Visible:=LevelsFolderHistoryComboBox.Visible;
  AnythingButLevelsFolderHistoryComboBox.Visible:=not LevelsFolderHistoryComboBox.Visible;
  AnythingButLevelsFileHistoryComboBox  .Visible:=not LevelsFileHistoryComboBox.Visible;

  MenuItemImageText.Checked:=False;

  SelectionEnabled:=SelectionEnabled__;

  CurrentFileName:=''; CurrentSectionName:=''; CurrentFileStringGridRow:=-1;

  if Task=otGame then begin
     //if   (ToolsForm<>nil) and ToolsForm.Visible then
     //     if biMinimize in BorderIcons then BorderIcons:=BorderIcons-[biMinimize]
     //else if not (biMinimize in BorderIcons) then BorderIcons:=BorderIcons+[biMinimize];
     CurrentCollectionFileName:='';
     CurrentCollectionCheckedLevelsCountDown:=High(CurrentCollectionCheckedLevelsCountDown);
     CurrentCollectionSolvedLevelsCount:=0;
     CollectionGroupBox.Caption:=CollectionText;
     CollectionStringGrid.Hint:=PackFileStringGridHintText;
     FileListBox1.Hint:=OpenGameFileListBoxHintText;
     MenuItemOpenCopy.Enabled:=(SubTask<>osGeneratorCandidateSet) and (SubTask<>osPluginTaskQueue);
     BtnOpen2.Enabled:=MenuItemOpenCopy.Enabled;
     GameBoardImage.Hint:=HintLeftClickToOpenLevelText+HintRightClickToReturnText;
     BtnOpen.Hint:=HintOpenLevelText;
     RectGroupBox.Visible:=False;
     if GameViewer<>nil then begin
        if not GameViewer.Initialized then
           GameViewer.LoadPictures;
        GameViewer.Modified:=True;
        if GameViewer.Game<>Game then begin // 'MainForm' and 'OpenForm'  exchange 'Game' frequently; ensure that the viewer sees the right one
           Game.Clear; GameViewer.LoadGame(Game);
           end;
        end;
     Game.ResetSaveGameAndLoadItIfItIsANormalModeGame     :=False;
     Game.RestoreSaveGame                                 :=False; // avoid showing savegame, if any
     Game.PrettyPrintGamesEnabled                         :=MainForm.Game.PrettyPrintGamesEnabled;
     Game.RleEnabled                                      :=MainForm.Game.RleEnabled;
     Game.SaveOldSolutionsAfterFindingBetterOnes          :=MainForm.Game.SaveOldSolutionsAfterFindingBetterOnes;
     Game.SaveOldSolutionsAfterClipboardImport            :=MainForm.Game.SaveOldSolutionsAfterClipboardImport;
     Game.SecondaryMetricsInTitles                        :=MainForm.Game.SecondaryMetricsInTitles;
     Game.SolutionsRequireAtLeastOnePush                  :=True; // *not* 'MainForm.Game.SolutionsRequireAtLeastOnePush' in order to load any snapshots and solutions that requires moves

     ToolsForm.Game.PrettyPrintGamesEnabled               :=Game.PrettyPrintGamesEnabled;
     ToolsForm.Game.RleEnabled                            :=Game.RleEnabled;
     ToolsForm.Game.SaveOldSolutionsAfterFindingBetterOnes:=MainForm.Game.SaveOldSolutionsAfterFindingBetterOnes;
     ToolsForm.Game.SaveOldSolutionsAfterClipboardImport  :=Game.SaveOldSolutionsAfterClipboardImport;
     ToolsForm.Game.SecondaryMetricsInTitles              :=Game.SecondaryMetricsInTitles;
     ToolsForm.Game.SolutionsRequireAtLeastOnePush        :=Game.SolutionsRequireAtLeastOnePush;
     // flush and close the level file, so it's loaded anew the next time the
     // user accesses it; the file on disk may have been edited using another
     // program while a copy has been loaded by this program;
     Result                                               := MainForm.SokoFile.Close;
     end
  else begin
     //if not (biMinimize in BorderIcons) then BorderIcons:=BorderIcons+[biMinimize];
     case Task of
       otImage  : if        SubTask=osNone then
                            ImageFileGroupBox.Caption:=ImageText
                  else if   SubTask=osSkin then
                            ImageFileGroupBox.Caption:=SkinText
                       else ImageFileGroupBox.Caption:='';
       otSound     : ImageFileGroupBox.Caption:=SoundText;
       otPalette   : ImageFileGroupBox.Caption:=PaletteText;
       otSolver    : ImageFileGroupBox.Caption:=SolverText;
       otOptimizer : ImageFileGroupBox.Caption:=OptimizerText;
       else          ImageFileGroupBox.Caption:='';
     end;
     CurrentCollectionFileName:='';
     CollectionGroupBox.Caption:=PlayListText;
     CollectionStringGrid.Hint:=PlayListStringGridHintText;
     FileListBox1.Hint:=FileListBoxHintText;
     BaseRect:=Rect__; DragRect:=Rect__; DefaultRect:=DefaultRect__;
     FixedDefaultImageSize:=FixedDefaultImageSize__;
     SetBackgroundColor(BackgroundColor__);
     BtnCancel1.Enabled:=True;

     s:=DEFAULT_VALUE;
     if (Task=otImage) and (AnsiCompareText(s,FileName__)=0) then begin
        BaseFileName:=DefaultImageFileName;
        SpinEditWidth .Enabled:=False;
        SpinEditHeight.Enabled:=False;
        if not FileExists(BaseFileName) then
           try    BaseFileName:=ChangeFileExt(BaseFileName,JPG_FILE_EXT);
                  MusicImage.Picture.SaveToFile(BaseFileName);
           except on E:Exception do Error(E.Message,Caption);
           end;
        end
     else if (Task=otPalette) and (AnsiCompareText(s,FileName__)=0) then begin
        BaseFileName:=DefaultPaletteFileName;
        SpinEditWidth .Enabled:=False;
        SpinEditHeight.Enabled:=False;
        end
     else begin
        BaseFileName:=FileName__;
        SpinEditWidth .Enabled:=True;
        SpinEditHeight.Enabled:=True;
        end;

     if Task=otMusicPlayer then begin
        LoadOrMakeNewPlayListFile;

        with PlayListPopupMenu do begin
          for i:=7 to Pred(Items.Count) do Items[i].Visible:=False;
          end;

        OpenDialog1.Title:=Caption+SUB_TITLE_SEPARATOR+OpenPlayListText;
        OpenDialog1.Filter:=PlayListsText+' (*.'+Copy(PLAYLIST_FILE_EXT,2,MaxInt)+')|*'+Copy(PLAYLIST_FILE_EXT,2,MaxInt)+BAR+AllFilesFilterText;
        OpenDialog1.FilterIndex:=1;
        end;

     if   PluginGroupBox.Visible then
          i:=DiskGroupBox.Height-ImagePanel.Top
     else i:=SoundGroupBox.Top-ImagePanel.Top-8;
     if (SoundGroupBox.Visible or PluginGroupBox.Visible)
        and
        ((ImagePanel.Height<>i) or (ImageScrollBox.Height<>ImagePanel.ClientHeight)) then begin
        ImagePanel.Height:=i;
        ImageScrollBox.Align:=alNone;
        ImageScrollBox.Height:=ImagePanel.ClientHeight;
        ImageScrollBox.Width :=ImagePanel.ClientWidth;
        ImageScrollBox.Align:=alClient;
        end;

     DisplayBuiltInImage;

     SetFileFilter(Filter__);
     if      (Task=otImage) or (Task=otSolver) or (Task=otOptimizer) then
             FilterComboBox1.ItemIndex:=0
     else if (Task<>otMusicPath) and (Task<>otMusicPlayer) and
             (AnsiPos(ExtractFileExt(AnsiLowerCase(FileName__)),AnsiLowerCase(Filter__))=0) then
             FilterComboBox1.ItemIndex:=1;

     SoundGroupBox .Caption:=Caption__;
     if   Task=otSolver then
          PluginGroupBox.Caption:=SolverText
     else PluginGroupBox.Caption:=OptimizerText;
     RectGroupBox  .Visible:=ImageGroupBox.Visible and (Rect__.Bottom>=0);

     if RectGroupBox.Visible then BtnImagePrior.Top:=PanelBackgroundColor.Top
     else BtnImagePrior.Top:=RectGroupBox.Top;
     BtnImageNext.Top:=BtnImagePrior.Top;

     ImageNamePanel.Caption:='';
     EnableDisableSoundControls;
     if (Task=otSolver) or (Task=otOptimizer) then
        EnableDisablePluginControls(Self,PluginForTask(Task),True);

     if Task=otPalette then begin
        ImageScrollBox.HorzScrollBar.Position:=0;
        ImageScrollBox.VertScrollBar.Position:=0;

        if Fractals.BitMap=nil then with MainForm.MPlayer do begin // no fractals shown yet, try to make one
           if not Initialized then Initialized:=Initialize;
           if (Display.BitMaps[0]=nil) and Initialized then
              Display.Initialize(Buttons[Ord(btDisplay)].MPlayerRect);
           if Fractals.ZoomList=nil then
              Fractals.ColorCycling:=False; // this calls "Fractals.Show", i.e., it makes an image
           end;

        if Fractals.BitMap<>nil then
           try    Image1.ClientWidth          :=Fractals.BitMap.Width;
                  Image1.ClientHeight         :=Fractals.BitMap.Height;
                  Image1.Picture.BitMap.Width :=Image1.ClientWidth;
                  Image1.Picture.BitMap.Height:=Image1.ClientHeight;
                  Image1.Picture.BitMap.Canvas.CopyRect(
                    Rect(0,0,Fractals.BitMap.Width,Fractals.BitMap.Height),
                    Fractals.BitMap.Canvas,
                    Rect(0,0,Fractals.BitMap.Width,Fractals.BitMap.Height));
                  ImageScrollBox.HorzScrollBar.Range:=Image1.ClientWidth;
                  ImageScrollBox.VertScrollBar.Range:=Image1.ClientHeight;
           except on E:Exception do;
           end;
        end;
     end;

  if   IsAnIniFileSectionFileName(BaseFileName) then
       s:=ExtractIniFileName(BaseFileName)
  else s:=BaseFileName;
  if   (not StrEqual(ExtractFileNameWithoutPathAndExtension(s),DEFAULT_VALUE)) // don't add temporary files to history
       then begin
       if   LevelsFolderHistoryComboBox.Visible then
            AddItemOrMoveItemToTopOfComboBox     (           LevelsFolderHistoryComboBox,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(ExtractFilePath(s)),MainForm.MyDocumentsFolder),True)
       else AddItemOrMoveItemToTopOfComboBox     (AnythingButLevelsFolderHistoryComboBox,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(ExtractFilePath(s)),MainForm.MyDocumentsFolder),True);
       if   FileExists(s) then
            if   LevelsFileHistoryComboBox.Visible then
                 AddItemOrMoveItemToTopOfComboBox(           LevelsFileHistoryComboBox  ,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(BaseFileName,MainForm.MyDocumentsFolder),True)
            else AddItemOrMoveItemToTopOfComboBox(AnythingButLevelsFileHistoryComboBox  ,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(BaseFileName,MainForm.MyDocumentsFolder),True);

       //with FolderHistoryComboBox do Perform(CB_SETDROPPEDWIDTH,Tag,0);
       //with FileHistoryComboBox   do Perform(CB_SETDROPPEDWIDTH,Tag,0);
       end;

  with FilePopupMenu do
    for i:=0 to 1 do Items[i].Visible:=Task=otMusicPlayer;

  if (SkinScriptsComboBox.Items.Count<>0) and
     (SkinScriptsComboBox.ItemIndex=-1) then
     SkinScriptsComboBox.ItemIndex:=0;

  if ((Task=otGame) and (oTask<>otGame)) or
     (Task<>otGame) then
     FormResize(Self);

  for i:=0 to Pred(StatusBar1.Panels.Count) do StatusBar1.Panels[i].Text:='';
end;

procedure TOpenForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and
     ((Screen.Cursor=crHSplit) or (Screen.Cursor=crVSplit)) then begin
     MouseButtonDown:=True; DragPoint.X:=X; DragPoint.Y:=Y;
     end;
end;

procedure TOpenForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if   MouseButtonDown then begin
       if      Screen.Cursor=crHSplit then begin
               Y:=X-DragPoint.X;
               if (Y<>0) and ((Y and 1)=0) then begin // 'and 1': even changes only, that is 2,4,6...
                  if   DiskGroupBox.Tag=1 then
                       ResizeDiskGroupBox(DiskGroupBox.Width+Y,DiskGroupBox.Height)
                  else ResizeFolderListBox(DirectoryListBox1.Width+Y);
                  DragPoint.X:=X;
                  end;
               end
       else if Screen.Cursor=crVSplit then begin
               X:=Y-DragPoint.Y;
               if (X<>0) and ((X and 1)=0) then begin // 'and 1': even changes only, that is 2,4,6...
                  if   GameFileGroupBox.Tag=0 then
                       ResizeDiskGroupBox(DiskGroupBox.Width,DiskGroupBox.Height+X)
                  else ResizeGameBoardPanel(GameBoardPanel.Height+X);
                  DragPoint.Y:=Y;
                  end;
               end;
       end
  else if (Sender=Self)
          and
          (X<GameFileGroupBox.Left)
          and
          (X>=DiskGroupBox.Left+DiskGroupBox.Width)
          and
          (Y>=DiskGroupBox.Top)
          and
          (Y<CollectionGroupBox.Top+CollectionGroupBox.Height)
          and
          (Width>Succ(CalculateMinWidth))
          and
          (not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy))
          then with StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT] do begin
          DiskGroupBox.Tag:=1;
          Hint:=HintChangePanelWidthText;
          ShowHint(Sender);
          Screen.Cursor:=crHSplit;
          end
  else if (Sender=Self)
          and
          (X<DiskGroupBox.Left+DiskGroupBox.Width)
          and
          (X>=DiskGroupBox.Left)
          and
          (Y<CollectionGroupBox.Top) and
          (Y>=DiskGroupBox.Top+DiskGroupBox.Height) and
          (Height>Succ(CalculateMinHeight))
          and
          (not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy))
          then with StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT] do begin
          GameFileGroupBox.Tag:=0;
          Hint:=HintChangePanelHeightText;
          ShowHint(Sender);
          Screen.Cursor:=crVSplit;
          end
  else if (Sender=DiskGroupBox)
          and
          (X<FileListBox1.Left)
          and
          (X>DirectoryListBox1.Left+DirectoryListBox1.Width)
          and
          (Y>=DirectoryListBox1.Top)
          and
          (Y<=DirectoryListBox1.Top+DirectoryListBox1.Height)
          and
          (Width>Succ(CalculateMinWidth))
          and
          (DiskGroupBox.Width>DiskgroupBoxMinimumWidth)
          and
          (not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy))
          then with StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT] do begin
          DiskGroupBox.Tag:=0;
          Hint:=HintChangePanelWidthText;
          ShowHint(Sender);
          Screen.Cursor:=crHSplit;
          end
  else if (Sender=GameFileGroupBox)
          and
          (X<GameBoardPanel.Left+GameBoardPanel.Width)
          and
          (X>=GameBoardPanel.Left)
          and
          (Y<GameDataGroupBox.Top) and
          (Y>=GameBoardPanel.Top+GameBoardPanel.Height) and
          (Height>Succ(CalculateMinHeight))
          and
          (not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy))
          then with StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT] do begin
          GameFileGroupBox.Tag:=1;
          Hint:=HintChangePanelHeightText;
          ShowHint(Sender);
          Screen.Cursor:=crVSplit;
          end
  else with StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT] do begin
       if Hint<>'' then begin
          Hint:=''; ShowHint(Sender);
          end;
       if (Screen.Cursor<>crDefault) and
          (not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy and (Screen.Cursor=crHourGlass)))
          then
          Screen.Cursor:=crDefault;
       end;
  if (Task<>otGame) and (Task<>otSolver) and (Task<>otOptimizer) and
     (StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text<>'') then
     StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
  if PanelToolTips.Visible then PanelToolTips.Hide;
  if Assigned(DuplicatesForm) and DuplicatesForm.Visible then with DuplicatesForm.StatusBar1 do begin
     if DuplicatesForm.PanelToolTips.Visible then DuplicatesForm.PanelToolTips.Hide;
     if (Screen.ActiveForm<>DuplicatesForm) and
        (Panels[OPEN_FORM_PANEL_INDEX_HINT].Text<>'') then
        Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:='';
     end;
end;

procedure TOpenForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ControlMouseUp(Sender,Button,Shift,X,Y);
  if (Button=mbRight) and (not IsModified) then
     BtnCancelClick(Sender);
end;

procedure TOpenForm.ControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Screen.Cursor=crHSplit) or (Screen.Cursor=crVSplit) then begin
     Screen.Cursor:=crDefault;
     Hint:=''; ShowHint(Sender);
     end;
  if PanelToolTips.Visible and (Sender<>nil) then PanelToolTips.Hide;
  if Assigned(DuplicatesForm) and DuplicatesForm.Visible then with DuplicatesForm.StatusBar1 do begin
     if DuplicatesForm.PanelToolTips.Visible then DuplicatesForm.PanelToolTips.Hide;
     if (Screen.ActiveForm<>DuplicatesForm) and
        (Panels[OPEN_FORM_PANEL_INDEX_HINT].Text<>'') then
        Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:='';
     end;  
end;

procedure TOpenForm.ControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseButtonDown:=False;
  ControlMouseMove(nil,Shift,X,Y);
end;

procedure TOpenForm.BtnPlaySoundClick(Sender: TObject);
begin
  PlaySound;
end;

function  TOpenForm.PlaySound:Boolean;
var o:Boolean;
begin
  Result:=False;
  if   ActiveList<>nil then ActiveControl:=ActiveList
  else ActiveControl:=FileListBox1;
  if Music<>nil then
     if   FileExists(CurrentFileName) then
          if Music.LoadFromFile(CurrentFileName) then begin
             BtnPlaySound.Enabled:=False;
             BtnStopSound.Enabled:=True;
             PlayingSoundFileLabel.Caption:=Format(PlayingFileText__,[ExtractFileNameWithoutExt(CurrentFileName)]);
             PlayingSoundFileLabel.Visible:=True;

             o:=Music.Enabled;
             try     Music.Enabled:=True; Result:=Music.Play;
             finally Music.Enabled:=o;
             end;

             if   (Music.Player<>nil) and (Music.Player.Mode=mpPlaying) then
                  StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=MediaPlayerStateText[Music.Player.Mode]
             else EnableDisableSoundControls;
             end
          else begin
             EnableDisableSoundControls;
             Error(Format(OpenFileFailedShortText__,[CurrentFileName]){+NL+NL+E.Message},
                   Caption+SUB_TITLE_SEPARATOR+TopPanel.Caption);
             end
     else Error(Format(FileNotFoundText__,[CurrentFileName]),Caption);
end;

procedure TOpenForm.EnableDisableSoundControls;
var CurrentFileIsPlaying:Boolean; Mode:TMPModes;
begin
  if   (Music<>nil) and (Music.Player<>nil) and Music.ModeOk(Music.Player.Mode) then
       Mode:=Music.Player.Mode
  else Mode:=mpNotReady;
  CurrentFileIsPlaying:=(Mode=mpPlaying) and (Music.Player.FileName=CurrentFileName);
  BtnPlaySound.Enabled:=(Music<>nil) and
                        (AnsiPos(AnsiLowerCase(ExtractFileExt(CurrentFileName)),AnsiLowerCase(FilterComboBox1.Items[0]))<>0) and
                        FileExists(CurrentFileName) and
                        (not CurrentFileIsPlaying);
  BtnStopSound.Enabled:=Mode=mpPlaying;
  PlayingSoundFileLabel.Visible:=BtnStopSound.Enabled;
  if   (Task<>otImage) and (Task<>otPalette) and (Task<>otSolver) and (Task<>otOptimizer) then
       Image1.Visible:=BtnPlaySound.Enabled or CurrentFileIsPlaying;
  if   ImageNamePanel.Caption<>'' then begin //FileListBox1.ItemIndex>=0 then begin
       SetGroupBoxCaption(Self.Canvas,SoundGroupBox,ImageNamePanel.Caption);
       SetGroupBoxCaption(Self.Canvas,ImageGroupBox,ImageNamePanel.Caption);
       end
  else if Task=otMusicPath then
       begin
       SoundGroupBox .Caption:='';
       SetGroupBoxCaption(Self.Canvas,ImageGroupBox,TopPanel.Caption);
       end
  else begin
       SetGroupBoxCaption(Self.Canvas,SoundGroupBox,TopPanel.Caption);
       SetGroupBoxCaption(Self.Canvas,ImageGroupBox,TopPanel.Caption);
       end;
//PanelBackgroundColor.Visible:=Integer(PanelBackgroundColor.Color)<>-1;
  PanelBackgroundColor.Visible:=(SubTask=osTile) or (SubTask=osWall);
end;

procedure TOpenForm.BtnStopSoundClick(Sender: TObject);
begin
  ActiveControl:=FileListBox1;
  if Music<>nil then Music.Stop(True);
end;

procedure TOpenForm.SetState(State__:TState);
begin
  State:=State__;
  with StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT] do
    case State of
      stNull            : Text := '';
      stReady           : if        SpinEditWidth.Enabled then
                                    Text := HintSelectZoomRectText
                          else if   SpinEditLeft.Enabled then
                                    Text := HintSelectRectText
                               else Text:='';
      stDrag            : if        SpinEditWidth.Enabled then
                                    Text := HintDragRectText+'.  '+HintDeleteRectText
                          else      Text := HintDragRectText;
      stSize            : Text := HintResizeRectText;
      else                Text := '';
    end; // case
end;

procedure TOpenForm.DrawRect(const Rect:TRect; PenMode:TPenMode);
const PEN_WIDTH=1;
begin
  with Image1.Picture.BitMap.Canvas do begin
     Pen.Color   := Color;
     Pen.Mode    := PenMode;
     Pen.Style   := psDot; // only available with 'Pen.Width = 1'
     Pen.Width   := PEN_WIDTH;
     Brush.Style := bsClear;
     end;
  DragRect := Rect;
  with Rect do
    Image1.Picture.BitMap.Canvas.Rectangle(Left,Top,Right,Bottom);
end;

procedure TOpenForm.ClipDragRectToImage;
begin
  with DragRect do begin
    // dont't clip the rectangle; it's easier for the user to move the rectangle around without clipping
    {
    if Left  <0 then Left:=0;
    if Top   <0 then Top :=0;
    if Right >Image1.Picture.BitMap.Width  then Right :=Image1.Picture.BitMap.Width;
    if Bottom>Image1.Picture.BitMap.Height then Bottom:=Image1.Picture.BitMap.Height;
    }
    if   RangeCheck(Left,SpinEditLeft.MinValue,SpinEditLeft.MaxValue) then
         SpinEditLeft  .Value:=Left
    else Left:=SpinEditLeft.Value;
    if   RangeCheck(Top,SpinEditTop.MinValue,SpinEditTop.MaxValue) then
         SpinEditTop   .Value:=Top
    else Top:=SpinEditTop.Value;
    if   RangeCheck(Right-Left,SpinEditWidth.MinValue,SpinEditWidth.MaxValue) then
         SpinEditWidth .Value:=Right -Left
    else Right:=Left+SpinEditWidth.Value;
    if   RangeCheck(Bottom-Top,SpinEditHeight.MinValue,SpinEditHeight.MaxValue) then
         SpinEditHeight.Value:=Bottom-Top
    else Bottom:=Top+SpinEditHeight.Value;

    ShowDragRect(DragRect);
    end;
end;

procedure TOpenForm.ResetDragRect;
begin
  HideDragRect; FillChar(DragRect,SizeOf(DragRect),0);
  IsDragging:=False; IsZooming:=False; SwSizing:=False; SizeHandle:=shNull;
end;

procedure TOpenForm.HideDragRect;
begin
  with DragRect do
    if DragRectVisible and (Left<Right) and (Top<Bottom) then
       DrawRect(DragRect,pmXor);
 DragRectVisible:=False;
end;

procedure TOpenForm.ShowDragRect(const Rect:TRect);
begin
 HideDragRect;
 DragRect:=Rect;
 with DragRect do
   if (Left<Right) and (Top<Bottom) then begin
      DrawRect(DragRect,pmXor); DragRectVisible:=True;
      end
end;

procedure TOpenForm.ScrollDragRectInView;
var R:TRect;
begin
  if DragRectVisible then with DragRect do begin
    R.Left  :=ImageScrollBox.HorzScrollBar.Position;
    R.Top   :=ImageScrollBox.VertScrollBar.Position;
    R.Right :=R.Left+ImageScrollBox.ClientWidth;
    R.Bottom:=R.Top +ImageScrollBox.ClientHeight;

    if (Left<R.Left) or (Right>R.Right  ) then
       ImageScrollBox.HorzScrollBar.Position:=Max(0,Left-10);
    if (Top <R.Top ) or (Bottom>R.Bottom) then
       ImageScrollBox.VertScrollBar.Position:=Max(0,Top-10);
    end;
end;

procedure TOpenForm.SpinEditRectChange(Sender: TObject);
begin
  if Sender is TSpinEdit then with Sender as TSpinEdit do begin
     if      Value<MinValue then Value:=MinValue
     else if Value>MaxValue then Value:=MaxValue;
     if      Sender=SpinEditLeft   then
             if Value<>DragRect.Left then with DragRect do begin
                HideDragRect; Left:=Value; Right :=Left+SpinEditWidth .Value; ClipDragRectToImage;
                end
             else
     else if Sender=SpinEditTop    then
             if Value<>DragRect.Top then with DragRect do begin
                HideDragRect; Top :=Value; Bottom:=Top +SpinEditHeight.Value; ClipDragRectToImage;
                end
             else
     else if Sender=SpinEditWidth  then
             if Value<>DragRect.Right-DragRect.Left then with DragRect do begin
                HideDragRect; Right:=Left+Value; ClipDragRectToImage;
                end
             else
     else if Sender=SpinEditHeight then
             if Value<>DragRect.Bottom-DragRect.Top then with DragRect do begin
                HideDragRect; Bottom:=Top+Value; ClipDragRectToImage;
                end;

     if   (not MenuItemImageText.Checked) or
          IsDragging or IsZooming or (SizeHandle<>shNull) then
          // do nothing;
     else GetBitMapText;
     end;
end;

procedure TOpenForm.SpinEditRectEnter(Sender: TObject);
begin
  if Sender is TSpinEdit then with Sender as TSpinEdit do begin
     oSpinEditValue:=Value;
     if      Sender=SpinEditLeft   then begin
             end
     else if Sender=SpinEditTop    then begin
             end
     else if Sender=SpinEditWidth  then begin
             MaxValue:=Image1.Picture.BitMap.Width-SpinEditLeft.Value;
             end
     else if Sender=SpinEditHeight then begin
             MaxValue:=Image1.Picture.BitMap.Height-SpinEditTop.Value;
             end;
     end;
end;

procedure TOpenForm.SpinEditRectExit(Sender: TObject);
begin
  if Sender is TSpinEdit then with Sender as TSpinEdit do
     Text:=IntToStr(Value); // change blank to '0'
end;

procedure TOpenForm.SpinEditLeftKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Sender is TSpinEdit then with Sender as TSpinEdit do begin
     if Key=VK_ESCAPE then
        begin Value:=oSpinEditValue;
              SelLength:=Length(Text);
              EscapeEnabled:=False;
        end;
     end;
end;

procedure TOpenForm.PanelBackgroundColorEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  PanelBackgroundColor.BevelOuter:=bvRaised;
end;

procedure TOpenForm.PanelBackgroundColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then begin
     PanelBackgroundColor.BeginDrag(False);
     PanelBackgroundColor.BevelOuter:=bvLowered;
     end;
end;

procedure TOpenForm.PanelBackgroundColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PanelBackgroundColor.BevelOuter:=bvRaised;
end;

procedure TOpenForm.SetBackgroundColor(Color:TColor);
begin
  PanelBackgroundColor.Color:=Color;
  PanelBackgroundColor.Font.Color:=ContrastColor(Color);
end;

procedure TOpenForm.Image1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source=PanelBackgroundColor then SetBackgroundColor(Image1.Picture.BitMap.Canvas.Pixels[X,Y]);
end;

procedure TOpenForm.Image1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept:=Source=PanelBackgroundColor;
end;

function  SizeHandleToCursor(SizeHandle:TSizeHandle):TCursor;
begin
  case SizeHandle of
    shTopLeft, shBottomRight: Result := crSizeNWSE;
    shTop, shBottom         : Result := crSizeNS;
    shTopRight, shBottomLeft: Result := crSizeNESW;
    shRight, shLeft         : Result := crSizeWE;
    else                      Result := crDefault;
  end; // case
end;

procedure SizeHandleResize(var SizeHandle:TSizeHandle; X,Y:Integer; var ControlPoint:TPoint; var R:TRect);
const HorisontallyOppositeSizeHandle : array[TSizeHandle] of TSizeHandle =
        (shNull,shTopLeft    ,shLeft ,shBottomLeft,shBottom,
                shBottomRight,shRight,shTopRight  ,shTop);
      VerticallyOppositeSizeHandle  : array[TSizeHandle] of TSizeHandle =
        (shNull,shBottomRight,shRight,shTopRight ,shTop,
                shTopLeft    ,shLeft,shBottomLeft,shBottom);

var DX, DY : Integer;

  procedure Swap(var A, B: Integer);
  var Tmp: Integer;
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

begin // SizeHandleResize
  DX :=  X - ControlPoint.X;
  DY :=  Y - ControlPoint.Y;
  ControlPoint.X := X;
  ControlPoint.Y := Y;

  case SizeHandle of
    shTopLeft:     R := Rect(R.Left+DX, R.Top+DY, R.Right, R.Bottom);
    shTop:         Inc(R.Top,DY);
    shTopRight:    R := Rect(R.Left, R.Top+DY, R.Right+DX, R.Bottom);
    shRight:       Inc(R.Right,DX);
    shBottomRight: R := Rect(R.Left, R.Top, R.Right+DX, R.Bottom+DY);
    shBottom:      Inc(R.Bottom,DY);
    shBottomLeft:  R := Rect(R.Left+DX, R.Top, R.Right, R.Bottom+DY);
    shLeft:        Inc(R.Left,DX);
  end; // case
  with R do
    begin if Top  > Bottom then begin Swap(Top, Bottom);
                                      SizeHandle:=HorisontallyOppositeSizeHandle[SizeHandle];
                                end;
          if Left > Right  then begin Swap(Left, Right);
                                      SizeHandle:=VerticallyOppositeSizeHandle[SizeHandle];
                                end;
    end;
end;

function  PointToSizeHandle(const P:TPoint; const Rect:TRect; Delta:Integer):TSizeHandle;
begin
  with Rect do with P do
    if   (X>Left-Delta) and (X<Right+Delta)  and
         (Y>Top -Delta) and (Y<Bottom+Delta) and
         (Left<Right) and (Top<Bottom) then
         if        Y<Top+Delta then
                   if        X<Left +Delta then Result:=shTopLeft
                   else if   X>Right-Delta then Result:=shTopRight
                        else Result:=shTop
         else if   Y>Bottom-Delta then
                   if        X<Left +Delta then Result:=shBottomLeft
                   else if   X>Right-Delta then Result:=shBottomRight
                        else Result:=shBottom
              else if        X<Left +Delta then Result:=shLeft
                   else if   X>Right-Delta then Result:=shRight
                        else Result:=shNull
    else Result:=shNull;
end;

procedure TOpenForm.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var DragRectWidth,DragRectHeight:Integer; Point:TPoint;
begin
  if      (Button=mbLeft) and (Task=otImage) then
          if SelectionEnabled then begin
             if BtnFileMenu.Visible then BtnFileMenu.Hide;
             Point.X:=X; Point.Y:=Y;
             IsDragging:=PtInRect(DragRect,Point);
             if   SwSizing and SpinEditWidth.Enabled then
                  begin SizeHandle:=PointToSizeHandle(Point,DragRect,SIZERECT_DELTA);
                        if SizeHandle<>shNull then IsDragging:=False;
                  end;
             if Screen.Cursor<>SizeHandleToCursor(SizeHandle) then
                Screen.Cursor:=SizeHandleToCursor(SizeHandle);
             IsZooming :=(not IsDragging) and (SizeHandle=shNull) and SpinEditWidth.Enabled;
             SwZoomIn  :=IsDragging;
             SwSizing  :=False;
             DragPoint.X:=X; DragPoint.Y:=Y;
             DragOriginPoint.X:=X; DragOriginPoint.Y:=Y;
             if   IsZooming then
                  begin HideDragRect;

                        if   SpinEditWidth.Value=0 then DragRectWidth:=DEFAULT_DRAGRECT_WIDTH
                        else DragRectWidth:=SpinEditWidth.Value;
                        if   SpinEditHeight.Value=0 then DragRectHeight:=DEFAULT_DRAGRECT_WIDTH
                        else DragRectHeight:=SpinEditHeight.Value;

                        DragRect.Left:=X-(DragRectWidth  div 2);
                        DragRect.Top :=Y-(DragRectHeight div 2);
                        DragRect.Right:=DragRect.Left+DragRectWidth;
                        DragRect.Bottom:=DragRect.Top+DragRectHeight;
                        ClipDragRectToImage;
                        DragOriginPoint.X:=DragRect.Left;
                        DragOriginPoint.Y:=DragRect.Top;
                        ////Screen.Cursor:=crSize;
                        SetState(stReady);
                        if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
                  end
             else if (not IsDragging) and (not SpinEditWidth.Enabled) then
                  begin HideDragRect;
                        with DragRect do begin
                          PointToCell(X,Y,Right-Left,Bottom-Top,1,X,Y);
                          DragRect:=GridCellToRect(X,Y,Right-Left,Bottom-Top,1);
                          ClipDragRectToImage;
                          end;
                  end
             else begin
                  end;
             end
          else BtnOpenClick(Sender)
  else if (Button=mbLeft) and (Task=otPalette) then BtnOpenClick(Sender);
end;

procedure TOpenForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var DX,DY,W,H:Integer; C:TCursor; SH:TSizeHandle;
begin
  ControlMouseMove(Sender,Shift,X,Y);
  if (Task=otImage) and SelectionEnabled then with Image1 do
     begin
          if StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text='' then SetState(State);

          if      IsZooming then with DragRect do
                  begin HideDragRect;

                        W:=2*Abs(X-DragOriginPoint.X);
                        H:=2*Abs(Y-DragOriginPoint.Y);

                        Left  :=DragPoint.X-(W div 2);
                        Right :=Left+W;
                        Top   :=DragPoint.Y-(H div 2);
                        Bottom:=Top +H;

                        ClipDragRectToImage;

                        if Screen.Cursor<>crDefault then
                           Screen.Cursor:=crDefault;
                  end
          else if IsDragging then with DragRect do
                  begin HideDragRect;

                        DX:=X-DragPoint.X; DY:=Y-DragPoint.Y;
                        DragPoint.X:=X; DragPoint.Y:=Y;

                        Inc(Left  ,DX);
                        Inc(Top   ,DY);
                        Inc(Right ,DX);
                        Inc(Bottom,DY);

                        ClipDragRectToImage;

                        if Screen.Cursor<>crDrag then
                           Screen.Cursor:=crDrag;

                        //X:=DragRect.Left; // display top-left position
                        //Y:=DragRect.Top;
                  end
          else if SizeHandle<>shNull then with DragRect do
                  begin HideDragRect;
                        SizeHandleResize(SizeHandle,X,Y,DragPoint,DragRect);
                        W:=Right-Left; H:=Bottom-Top;

                        case SizeHandle of
                          shTopLeft     : begin Left :=Right-W; Top   :=Bottom-H; end;
                          shTop,
                          shTopRight    : begin Right:=Left +W; Top   :=Bottom-H; end;
                          shLeft,
                          shBottomLeft  : begin Left :=Right-W; Bottom:=Top   +H; end;
                          else            begin Right:=Left +W; Bottom:=Top   +H; end;
                        end; // case

                        ClipDragRectToImage;
                  end
          else begin
                     SH:=PointToSizeHandle(Point(X,Y),DragRect,SIZERECT_DELTA);
                     if SH=shNull then
                        begin SwSizing:=True;
                              if   Screen.Cursor<>crDefault then
                                   Screen.Cursor:=crDefault;
                              if   PtInRect(DragRect,Point(X,Y)) then
                                   SetState(stDrag)
                              else SetState(stReady);
                        end
                     else if SwSizing and SpinEditWidth.Enabled then
                             begin C:=SizeHandleToCursor(SH);
                                   if   C<>Screen.Cursor then
                                        Screen.Cursor:=C;
                                   if   C=crDefault then
                                        if State<>stReady then SetState(stReady)
                                        else
                                   else if State<>stSize  then SetState(stSize);
                             end;
               end;
          StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=Format(FORMAT_XY_INT,[X,Y]);
     end;
  if PanelToolTips.Visible then PanelToolTips.Hide;
end;

procedure TOpenForm.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if IsDragging or IsZooming or (SizeHandle<>shNull) then
     with Image1.Picture.BitMap do with Canvas do
       begin //DrawRect(DragRect,pmXor);
             Screen.Cursor:=crDefault;
             IsDragging:=False; IsZooming:=False; SizeHandle:=shNull;
             SwSizing:=False;
             SetState(stReady);
             if MenuItemImageText.Checked then GetBitMapText;
       end
  else if Button=mbRight then begin
          if Task=otImage then SetState(stReady);
          if DragRectVisible then begin
             HideDragRect;
             if SpinEditWidth.Enabled then FillChar(DragRect,SizeOf(DragRect),0);
             ClipDragRectToImage;
             end
          else if not IsModified then
                  BtnCancelClick(Sender);
          end;
end;

procedure TOpenForm.ShowPicture(Pict:TPicture; View:TImageView; FillColor:TColor; IsABuiltinImage:Boolean);
var H1,H2,W1,W2:Integer; ARect:TRect; B:TBitMap;

  procedure DoAntialiasing;
  var T1,T2:TTimeMS;
  begin
    if ImageAntialiasingEnabled then with Image1 do begin
       T1:=GetTickCount;
       BitMapBilinearAntiAliasing(Picture.BitMap,False,clBlack,0);
       T2:=GetTickCount;
       if   T2<T1 then T2:=High(T2)-T1+T2 // clock wrap-around
       else Dec(T2,T1);
       ImageAntialiasingEnabled:=T2<=MAX_IMAGE_ANTIALIASING_TIME_MS;
       end;
  end;

  function DoTile(DestBitMap__:TBitMap; var Pict__:TPicture):Boolean;
  var {b:Boolean;} P:TPict;
  begin
    try    P:=TPict.Create;
           try      if not (Pict__.Graphic is TBitMap) then
                       try     P.Pict:=Pict__;
                               if P.MakeOrgBitMapFromPict then
                                  if (P.OrgBitMap.Height<>DestBitMap__.Height) and
                                     (P.OrgBitMap.Height<>0) then begin
                                     {
                                     // the image on the screen always has a fixed height;
                                     // scale the image now, so it isn't performed more than once
                                     P.Scale(MulDiv(P.OrgBitMap.Width,DestBitMap__.Height,P.OrgBitMap.Height),
                                             DestBitMap__.Height,
                                             ivPicture,0,0,b);
                                     Pict__.Assign(P.BitMap);    // copy the bitmap to the picture so the conversion isn't performed more than once
                                     }

                                     // the "fixed height" condition mentioned in
                                     // the lines above doesn't seem to
                                     // hold anymore, hence, use the original
                                     // bitmap instead
                                     Pict__.Assign(P.OrgBitMap);
                                     end
                                  else
                                     Pict__.Assign(P.OrgBitMap); // copy the bitmap to the picture so the conversion isn't performed more than once
                       finally P.Pict:=nil; // // the picture 'P' doesn't own the picture 'Pict__', hence, clear the reference before 'P' is cleared/destroyed
                       end;

                    Result:=Pict__.Graphic is TBitMap;
                    if Result then begin
                       P.Clear;
                       try     P.BitMap:=DestBitMap__; // destination bitmap, i.e., the screen
                               P.OrgBitMap:=Pict__.BitMap; // source bitmap
                               Result:=P.SetView(ivTile,P.BitMap.Width,P.BitMap.Height,clBlack);
                       finally P.BitMap:=nil; P.OrgBitMap:=nil; // the picture 'P' doesn't own the graphics, hence, clear the references before 'P' is destroyed
                       end;
                       end;
           finally  P.Free;
           end;
    except on E:Exception do Result:=False;
    end;

  end;

begin // ShowPicture
  if Pict<>nil then with Image1 do begin
     Visible:=True;
     ImageScrollBox.HorzScrollBar.Position:=0;
     ImageScrollBox.VertScrollBar.Position:=0;
     ImageScrollBox.HorzScrollBar.Visible:=(View<>ivStretch) and (View<>ivTile);
     ImageScrollBox.VertScrollBar.Visible:=ImageScrollBox.HorzScrollBar.Visible;

     if (View=ivStretch) or (View=ivTile) then begin // note that 'tiled' is implemented for built-in jpg-images only
        ClientWidth          :=ImageScrollBox.ClientWidth;
        ClientHeight         :=ImageScrollBox.ClientHeight;

        Picture.BitMap.Width :=ClientWidth;
        Picture.BitMap.Height:=ClientHeight;
        Picture.BitMap.Pixelformat:=pf24Bit;
        Picture.BitMap.Canvas.Brush.Color:=FillColor;
        Picture.BitMap.Canvas.FillRect(Rect(0,0,ClientWidth,ClientHeight));

        W1:=Pict.Width;
        H1:=Pict.Height;
        W2:=Image1.ClientWidth-2;
        H2:=Image1.ClientHeight-2;

        if W1>W2 then begin H1:=MulDiv(W2,H1,W1); W1:=W2; end;
        if H1>H2 then begin W1:=MulDiv(H2,W1,H1); H1:=H2; end;

        ARect.Left  :=(W2-W1+2) div 2;
        ARect.Top   :=(H2-H1+2) div 2;
        ARect.Right :=ARect.Left+W1;
        ARect.Bottom:=ARect.Top+H1;

        if ((Pict.Graphic is TBitMap) and (View=ivStretch)) or
           (Pict.Graphic is TMetaFile) or
           (Pict.Graphic is TIcon) or
           (not IsABuiltinImage) then begin
           // 'StretchDraw' only handles the mentioned graphic-types
           Picture.BitMap.Canvas.StretchDraw(ARect,Pict.Graphic);
           end
        else begin
           // kludge: built-in images like 'Music', 'Sound', and 'Solver'
           // illustrations are jpg-images;
           // tile or stretch-draw them so they fill the client window

           if (View=ivTile) and // kludge: show the solver graphic tiled instead of stretch-drawing it
              DoTile(Picture.BitMap,Pict) then begin
              end
           else
              if BitMapCreate(B,Pict.Graphic.Width,Pict.Graphic.Height) then
                 try     B.Canvas.Draw(0,0,Pict.Graphic);
                         Picture.BitMap.Canvas.StretchDraw(Classes.Rect(0,0,Image1.ClientWidth,Image1.ClientHeight),B);
                         DoAntialiasing;
                 finally B.Free;
                 end;
           end;

        if (Task=otImage)
           and
           (SubTask<>osSkin)
           and
           ImageAntialiasingEnabled
           and
           ((Pict.Width>ClientWidth) or (Pict.Height>ClientHeight)) then
           DoAntialiasing;
        end
     else begin
        Image1.ClientWidth            :=Pict.Width;
        Image1.ClientHeight           :=Pict.Height;
        ImageScrollBox.HorzScrollBar.Range:=Image1.ClientWidth;
        ImageScrollBox.VertScrollBar.Range:=Image1.ClientHeight;
        Image1.Picture.Assign(Pict);
        end;
     end;
end;

procedure TOpenForm.DisplayBuiltinImage;
begin
  case Task of
    otSound      : begin CurrentImageFileName:='';
                         ShowPicture(SoundImage .Picture,ivTile{ivStretch},clWhite,True);
                   end;
    otMusic,
    otMusicPath,
    otMusicPlayer: begin CurrentImageFileName:='';
                         ShowPicture(MusicImage .Picture,ivTile{ivStretch},clWhite,True);
                   end;
    otSolver,
    otOptimizer  : begin CurrentImageFileName:='';
                         ShowPicture(SolverImage.Picture,ivTile{ivStretch},clWhite,True);
                   end;
    else;
  end; // case
end;

procedure TOpenForm.ImageResize;
begin
  if      Task in [otSound,otMusic,otMusicPath,otMusicPlayer,otSolver,otOptimizer] then
          DisplayBuiltinImage
  else if (Task in [otImage,otPalette]) and Image1.Visible and
          (FileExists(CurrentFileName)) and (not MenuItemImageText.Checked) then
          FileListBox1Change(Self);
end;

function  TOpenForm.TryToLoadImage(const FileName__:String):Boolean;
var i,VisibleRowCount:Integer; b:Boolean; ThisFileName:String; R:TRect;

  procedure ShowBothAnImageAndAScript;
  var i:Integer;
  begin
    i                       :=DiskGroupBox.Height-ImagePanel.Top;
    if ImagePanel   .Height <>i then begin
       ImagePanel   .Height :=i;
       TextFilePanel.Top    :=GroupBox1.Top-ImageFileGroupBox.Top;
       TextFilePanel.Height :=BtnOK.Top-8-TextFilePanel.Top;
       end;
  end;

begin // 'TryToLoadImage
  Result:=False;
  StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text :='';
  //if PanelToolTips.Visible then begin
  //   PanelToolTips.Hide; Update;
  //   end;

  if MenuItemImageText.Checked then MenuItemImageTextClick(Self);
  if Task<>otImage then ResetDragRect;
  if AnsiCompareText(CurrentFileName,BaseFileName)=0 then
     BaseRect:=DragRect; // save image section for later
  if AnsiCompareText(CurrentFileName,DefaultImageFileName)=0 then begin
     if RectWidth (DragRect)<>RectWidth (DefaultRect) then with DragRect do Right :=Left+RectWidth (DefaultRect);
     if RectHeight(DragRect)<>RectHeight(DefaultRect) then with DragRect do Bottom:=Top +RectHeight(DefaultRect);
     DefaultRect:=DragRect; // save image section for later
     end;
  CurrentFileName:=''; CurrentSectionName:='';
  ResetDragRect;
  SpinEditLeft  .Enabled:=True;
  SpinEditTop   .Enabled:=True;
  SpinEditWidth .Enabled:=True;
  SpinEditHeight.Enabled:=True;
  ThisFileName:=FileName__;
  if CloseText(tTextFile) and
     (Length(ThisFileName)>3) and (ThisFileName[3]=FILE_NAME_PATH_DELIMITER) then begin

     if Task=otMusicPlayer then begin

        if   IsAPlayListFile(ThisFileName) and
             PlayListLoadFromFile(ThisFileName) then begin
             CurrentCollectionFileName:=ThisFileName;
             ThisFileName:=CollectionStringGrid.Cells[1,0];
             end;

        if   CurrentCollectionFileName<>'' then begin
             CurrentSectionName       :=ThisFileName;
             CollectionFileNamePanel
                             .Caption :=StrWithQuotedAmpersands(SPACE+Copy(CurrentCollectionFileName,1,3)+'...\'+ExtractFileName(CurrentCollectionFileName));
             Result:=FileExists(CurrentCollectionFileName) and
                     ((ThisFileName='') or FileExists(ThisFileName));
             end
        else Result                   :=FileExists(ThisFileName);
        end
     else if Task = otPalette then
        Result:=LoadPalette(ThisFileName)
     else if Task = otSolver then
        Result:=LoadPlugin(ThisFileName,MainForm.Solver)
     else if Task = otOptimizer then
        Result:=LoadPlugin(ThisFileName,MainForm.Optimizer)
     else begin
        if SubTask=osSkin then
           for i:=0 to Pred(SkinsComboBox.Items.Count) do with MainForm.Skins do
               if (AnsiCompareText(ThisFileName,Skins[i,SKIN_NAME_INDEX])=0) and
                  (Skins[i,SKIN_SCRIPT_INDEX]<>'') then begin
                  SkinScriptsComboBox.ItemIndex:=IndexOfScript(Skins[i,SKIN_SCRIPT_INDEX]);
                  SkinScriptsComboBoxChange(Self);
                  break;
                  end;

        Result:=(Task<>otImage)
                or
                ((StrAnsiPosCI(ExtractFileExt(ThisFileName),IMAGE_FILES_FILTER)<>0)
                 and
                 LoadImage(ThisFileName));

        if SubTask=osSkin then begin
           if not Result then with MainForm.Skins do begin
              Result                     :=Self.LoadTextFromFile(ThisFileName,tTextFile);
              if Result and IsASettingsFile(Texts[tTextFile].Memo.Lines) and
                 FileExists(SettingsScriptFileName) and
                 (AddScript(SettingsScriptFileName)>=0) then begin
                 ScriptFileName:=SettingsScriptFileName;
                 SkinScriptsComboBox.ItemIndex:=IndexOfScript(SettingsScriptFileName);
                 SkinScriptsComboBoxChange(Self);
                 Result                  :=Self.LoadTextFromFile(ThisFileName,tTextFile);
                 end;

              CurrentImageFileName:='';
              if Result then begin
                 CurrentImageFileName:=ScriptImageFileName;
                 if CurrentImageFileName<>'' then
                    try    if   FileExists(CurrentImageFileName) and LoadImage(CurrentImageFileName) then begin
                                end
                           else CurrentImageFileName:='';
                    except on E:Exception do CurrentImageFileName:='';
                    end;
                 end;
              end;

           if   (CurrentImageFileName<>'') and (Texts[tTextFile].FileName<>'') then // 'True': show both an image and a script
                ShowBothAnImageAndAScript
           else DoNotSplitImageFileGroupBox;
           end;
        end;

     if   ThisFileName='' then
          ImageNamePanel   .Caption:=''
     else ImageNamePanel   .Caption:=SPACE+Copy(ThisFileName,1,3)+'...\'+ExtractFileName(ThisFileName);

     if (Task=otPalette) and (Fractals.PaletteAuthor<>'') then
        if   ImageNamePanel.Caption='' then
             ImageNamePanel.Caption:=Fractals.PaletteAuthor
        else ImageNamePanel.Caption:=ImageNamePanel.Caption+' : '+Fractals.PaletteAuthor;

     b:=not Result;
     if b and (not ImageMemo.WordWrap) then ImageMemo.WordWrap:=True;
     if b<>ImageMemo.Visible then ImageMemo.Visible:=b;

     b:=Result and (Texts[tTextFile].FileName<>'');
     if b<>TextFilePanel.Visible then TextFilePanel.Visible:=b;
     if TextFileMemo.Visible<>TextFilePanel.Visible then TextFileMemo.Visible:=TextFilePanel.Visible;
     if TextFilePanel.Caption<>'' then TextFilePanel.Caption:='';

     b:=not (ImageMemo.Visible or (TextFilePanel.Visible and (CurrentImageFileName='')));
     if b<>ImageScrollBox.Visible then ImageScrollBox.Visible:=b;

     b:=True;
     if b<>ImageNamePanel.Visible then ImageNamePanel.Visible:=b;

     if   Result then begin
          CurrentFileName:=ThisFileName;
          SpinEditLeft  .MaxValue:=Max(0,Pred(Image1.Picture.BitMap.Width));
          SpinEditTop   .MaxValue:=Max(0,Pred(Image1.Picture.BitMap.Height));
          SpinEditLeft  .MinValue:=-SpinEditLeft.MaxValue;
          SpinEditTop   .MinValue:=-SpinEditTop .MaxValue;
          SpinEditWidth .MaxValue:=Image1.Picture.BitMap.Width;
          SpinEditHeight.MaxValue:=Image1.Picture.BitMap.Height;

          if AnsiCompareText(CurrentFileName,BaseFileName)=0 then begin
             DragRect:=BaseRect;
             end
          else if SubTask=osWall then with Image1.Picture.BitMap do
                  if (Width>16) and (Width<=1024) and (Width*2=Height) and
                     (Height mod 8=0) then begin
                     DragRect:=Classes.Rect(0,Height div 4,3*(Width div 4),Height div 2);
                     end;

          if (AnsiCompareText(CurrentFileName,DefaultImageFileName)=0) and
             FixedDefaultImageSize then begin
             DragRect:=DefaultRect;
             DragRect      .Right  :=DragRect.Left+RectWidth (DefaultRect);
             DragRect      .Bottom :=DragRect.Top +RectHeight(DefaultRect);
             SpinEditLeft  .Enabled:=RectWidth (DragRect)<>0;
             SpinEditTop   .Enabled:=RectHeight(DragRect)<>0;
             SpinEditWidth .Enabled:=False;
             SpinEditHeight.Enabled:=False;
             end;

          ClipDragRectToImage;
          ScrollDragRectInView;
          if (Task=otImage) and (SubTask=osSkin) then HideDragRect;
          end
     else if   Task=otMusicPlayer then
               ImageMemo.Text:=Format(FileNotFoundText__               ,[ThisFileName])
          else if Task=otPalette then
               ImageMemo.Text:=Format(FileNotALegalPaletteFileText__   ,[ThisFileName])
          else if Task=otSolver then
               ImageMemo.Text:=Format(FileNotALegalSolverFileText__    ,[ThisFileName])
          else if Task=otOptimizer then
               ImageMemo.Text:=Format(FileNotALegalOptimizerFileText__ ,[ThisFileName])
          else ImageMemo.Text:=Format(FileNotALegalImageFileText__     ,[ThisFileName]);

     with FileListBox1 do begin

       i:=Items.IndexOf(ExtractFileName(CurrentFileName));
       if i>=0 then ItemIndex:=i;

       R:=ItemRect(0);
       VisibleRowCount:=Max(1,ClientHeight div (R.Bottom-R.Top));
       if (ItemIndex<TopIndex) or (ItemIndex>=TopIndex+VisibleRowCount) then begin
          i:=0;
          repeat Inc(i,VisibleRowCount);
          until i>ItemIndex;
          TopIndex:=i-VisibleRowCount;
          end;
       end;

     BtnOK.Enabled:=(Result
                     and
                     (ThisFileName<>'')
                     and
                     ((SubTask<>osSkin)
                      or
                      (SkinScriptsComboBox.ItemIndex>=0)
                     )
                    )
                    or
                    ((Task=otMusicPlayer) and IsModified);
     end
  else if Task=otMusicPath then begin
     ImageMemo         .Visible:=False;
     TextFilePanel     .Visible:=False;
     ImageScrollBox    .Visible:=True;
     ImageNamePanel    .Visible:=False;
     BtnOK             .Enabled:=True;
     CollectionGroupBox.Visible:=False;
     DoNotSplitImageFileGroupBox;
     end
  else begin
     ImageMemo         .Visible:=False;
     TextFilePanel     .Visible:=False;
     ImageScrollBox    .Visible:=False;
     ImageNamePanel    .Visible:=False;
     BtnOK             .Enabled:=False;
     CollectionGroupBox.Visible:=Task=otMusicPlayer;
     DoNotSplitImageFileGroupBox;

     if (SubTask=osSkin) and (FileName__='') and
        StrEqual(StrWithoutTrailingPathDelimiter(DirectoryListBox1.Directory),StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath)) and
        (FilterComboBox1.ItemIndex=0) then begin
        if      DirectoryExists(MainForm.Skins.DefaultSkinPath) and
                FileExists(MainForm.Skins.CommonSkinsScriptFileName) then begin
                DirectoryListBox1.Directory:=StrWithTrailingPathDelimiter(MainForm.Skins.DefaultSkinPath);
                SkinScriptsComboBox.ItemIndex:=MainForm.Skins.IndexOfScript(MainForm.Skins.CommonSkinsScriptFileName);
                end
        else if DirectoryExists(SkinsPath) and
                FileExists(MainForm.Skins.SettingsScriptFileName) then begin
                DirectoryListBox1.Directory:=StrWithTrailingPathDelimiter(SkinsPath);
                SkinScriptsComboBox.ItemIndex:=MainForm.Skins.IndexOfScript(MainForm.Skins.SettingsScriptFileName);
                end;
        if FileListBox1.Items.Count<>0 then begin
           FileListBox1.ItemIndex:=0;
           TryToLoadImage(StrWithTrailingPathDelimiter(DirectoryListBox1.Directory)+FileListBox1.Items[0]);
           end;
        end;
     end;

  CollectionGroupBox.Visible:=CurrentCollectionFileName<>'';

  if (ActiveList<>nil) and (CollectionGroupBox.Visible) and (SubTask<>osSkin) then begin
     if DiskGroupBox.Visible then ActiveControl:=ActiveList;
     end
  else begin
    if DiskGroupBox.Visible then ActiveControl:=FileListBox1;
    ActiveList:=FileListBox1;
    end;

  EnableDisableSoundControls;
  EnableDisablePluginControls(Self,PluginForTask(Task),True);
end;

function  TOpenForm.LoadImage(const FileName__:String):Boolean;
var BitMap:TBitMap; Pict:TPicture; //T1,T2:TTimeMS;
begin
//T1:=GetTickCount;
  Pict:=nil; CurrentImageFileName:='';
  try     try    Pict:=TPicture.Create;
          except on E:Exception do begin Pict.Free; Pict:=nil; end;
          end;
          Result:=Pict<>nil;
          if Result then
             try    try    Pict.LoadFromFile(FileName__);
                    except on E:Exception do
                           Result:=IsPNGFile(FileName__) and
                                   MainForm.PNGImageLoader.LoadFromFile(FileName__,Pict);
                    end;

                    if Result and (not (Pict.Graphic is TBitMap)) then begin
                       if BitMapCreate(BitMap,Pict.Width,Pict.Height) then
                          try     BitMap.Canvas.Brush.Color:=clWhite;
                                  BitMap.Canvas.FillRect(Rect(0,0,BitMap.Width,BitMap.Height));
                                  if   (BitMap.Width=Pict.Width) and (BitMap.Height=Pict.Height) then
                                       BitMap.Canvas.Draw(0,0,Pict.Graphic)
                                  else BitMap.Canvas.StretchDraw(Rect(0,0,BitMap.Width,BitMap.Height),Pict.Graphic);
                                  Pict.BitMap.Assign(BitMap);
                          finally BitMap.Free;
                          end
                       else Result:=False;
                       end;

             except on E:Exception do Result:=False;
             end;

          if Result then begin
             if Screen.ActiveForm=Self then
                if   SelectionEnabled or (SubTask=osSkin) then
                     ShowPicture(Pict,ivNone   ,ImagePanel.Color,False)
                else ShowPicture(Pict,ivStretch,ImagePanel.Color,False)
             else Image1.Picture.BitMap.Assign(Pict.BitMap);
             end;
  finally Pict.Free;
  end;

  if   Result then CurrentImageFileName:=FileName__
  else CurrentImageFileName:='';

//if Task=otImage then begin
//   T2:=GetTickCount;
//   if   T2<T1 then T2:=High(T2)-T1+T2 // clock wrap-around
//   else Dec(T2,T1);
//   StatusBar1.Panels[PANEL_INDEX_HINT].Text:=Format(SecondsFormatText__,[T2/1000]);
//   end;
end;

function  TOpenForm.LoadPalette(const FileName__:String):Boolean;
var oCursor:TCursor; R:TRect;
begin
  Result:=Fractals.LoadPalette(FileName__,True) and
          (Fractals.ZoomList<>nil) and
          (MainForm.MPlayer.Display.BitMaps[0]<>nil);
  oCursor:=Screen.Cursor;
  if Result then with MainForm.MPlayer.Display.BitMaps[0] do with Canvas do
     try
       Screen.Cursor:=crHourGlass;
       ImageScrollBox.HorzScrollBar.Range:=Image1.ClientWidth;
       ImageScrollBox.VertScrollBar.Range:=Image1.ClientHeight;
       ImageScrollBox.HorzScrollBar.Visible:=True;
       ImageScrollBox.VertScrollBar.Visible:=True;
       Fractals.MakeBitMap(Fractals.ZoomList,MainForm.MPlayer.Display.BitMaps[0]);
       R:=Rect(0,0,Width,Height);
       Image1.Picture.BitMap.Canvas.CopyRect(R,MainForm.MPlayer.Display.BitMaps[0].Canvas,R);
     finally
       Screen.Cursor:=oCursor;
     end;
  if FileName__<>'' then SetFileFilterIndex(FileName__);
end;

function  TOpenForm.LoadPlugin(const FileName__:String; Plugin__:TPlugin):Boolean;
var i:Integer;
begin
  Result:=Assigned(Plugin__)
          and
          (Plugin__.IsLoaded
           and
           (StrEqual(FileName__,Plugin__.PluginFileName)
            or
            (StrEqual(FileName__,Plugin__.DefaultPluginFileName)
             and
             StrEqual(Plugin__.PluginFileName,DEFAULT_VALUE)
            )
            or
            (StrEqual(FileName__,DEFAULT_VALUE)
             and
             StrEqual(Plugin__.PluginFileName,Plugin__.DefaultPluginFileName)
            )
           )
          )
          or
          Plugin__.Open(FileName__);
  if FileName__<>'' then SetFileFilterIndex(FileName__);
  for i:=0 to Pred(PluginLevelStringGrid.RowCount) do PluginLevelStringGrid.Cells[1,i]:='';
end;

procedure TOpenForm.MediaPlayerNotify(Sender: TObject);
begin
  if Sender is TMediaPlayer then with Sender as TMediaPlayer do begin
    SleepEx(50,False); // necessary to update 'Mode'
    StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=MediaPlayerStateText[Mode];
    if Mode<>mpPlaying then EnableDisableSoundControls;
    Notify:=True;
    end;
end;

function  TOpenForm.GetRectItem(Index:Integer):Integer;
begin
  if (SpinEditWidth.Value=0) or (SpinEditHeight.Value=0) then
     Result:=0
  else
     case Index of
       0  : Result:=SpinEditLeft  .Value;
       1  : Result:=SpinEditTop   .Value;
       2  : Result:=SpinEditWidth .Value;
       else Result:=SpinEditHeight.Value;
     end; // case
end;

function  TOpenForm.PlayListLoadFromFile(const FileName:String):Boolean;
var i:Integer; ThisFileName:String;
begin
  ThisFileName:=FileName;
  Result:=(Music<>nil) and Music_.PlayListLoadFromFile(ThisFileName,{CollectionStringGrid,1}Music.PlayList);

  if (not Result) and (FileName<>CurrentCollectionFileName) and
     (CurrentCollectionFileName<>'') and
     FileExists(CurrentCollectionFileName) and
     IsAPlayListFile(CurrentCollectionFileName) and
     Music_.PlayListLoadFromFile(CurrentCollectionFileName,{CollectionStringGrid,1}Music.PlayList) then begin
     Result:=True; ThisFileName:=CurrentCollectionFileName;
     end;

  for i:=1 to CollectionStringGrid.RowCount do
      CollectionStringGrid.Cells[0,Pred(i)]:=IntToStr(i);

  if Result then begin
     CurrentCollectionFileName:=ThisFileName;
     CollectionFileNamePanel.Caption :=StrWithQuotedAmpersands(SPACE+Copy(CurrentCollectionFileName,1,3)+'...\'+ExtractFileName(CurrentCollectionFileName));
     end
  else begin
     CurrentCollectionFileName:='';
     CollectionFileNamePanel.Caption :='';
     end;
end;

function  TOpenForm.PlayListAddItem(Index:Integer; const Item:String):Boolean;
var i:Integer;
begin
  with CollectionStringGrid do
    try    IsModified:=True;
           if (RowCount=1) and (Cells[1,0]='') then Index:=0
           else RowCount:=RowCount+1;
           Cells[0,Pred(RowCount)]:=IntToStr(RowCount);
           for i:=Pred(RowCount) downto(Succ(Index)) do
               Cells[1,i]:=Cells[1,Pred(i)];
           Cells[1,Index]:=Item; Row:=Index;
           Result:=True;
    except on E:Exception do Result:=Error(E.Message,'');
    end;
end;

procedure TOpenForm.MenuItemPlayListAddFileClick(Sender: TObject);
begin
  with FileListBox1 do
    if (ItemIndex>=0) and
       PlayListAddItem(CollectionStringGrid.Row,
                       StrWithTrailingPathDelimiter(Directory)+Items[ItemIndex]) then begin
       TryToLoadFile(CollectionStringGrid.Cells[1,Pred(CollectionStringGrid.RowCount)]);
       ActiveControl:=CollectionStringGrid;
       end;
end;

procedure TOpenForm.MenuItemPlayListAddFolderClick(Sender: TObject);
var i,j:Integer; s,s1:String;
begin
  with FileListBox1 do begin
    j:=0; s1:=AnsiLowerCase(FilterComboBox1.Items[0]);
    for i:=0 to Pred(Items.Count) do begin
        s:=StrWithTrailingPathDelimiter(Directory)+Items[i];
        if (AnsiPos(AnsiLowerCase(ExtractFileExt(s)),s1)<>0) and
           (Misc_.GridColIndexOf(CollectionStringGrid,1,s)=-1) then
           if   PlayListAddItem(CollectionStringGrid.RowCount,s) then
                Inc(j)
           else break;
        end;
    if j<>0 then with CollectionStringGrid do Row:=Pred(RowCount);
    end;
end;

procedure TOpenForm.MenuItemPlayListRemoveItemClick(Sender: TObject);
var i:Integer;
begin
  with CollectionStringGrid do begin
    for i:=Row to RowCount-2 do Cells[1,i]:=Cells[1,Succ(i)];
    RowCount:=RowCount-1; IsModified:=True;
    end;
end;

procedure TOpenForm.MenuItemPlayListClearClick(Sender: TObject);
begin
  if Msg(PlayListClearText,PChar(Caption),MB_YESNO+MB_DEFBUTTON2)=ID_YES then
     with CollectionStringGrid do begin
       RowCount:=1; Cells[1,0]:=''; IsModified:=True;
       TryToLoadFile('');
       end;
end;

procedure TOpenForm.MenuItemPlayListNewClick(Sender: TObject);
var s,s1,oDirectory:String;
begin
  s:=StrWithoutTrailingPathDelimiter(ExtractFilePath(CurrentCollectionFileName));
  if not DirectoryExists(s) then
     s:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
  s:=PlayListNewFileName(s);
  oDirectory:=FileListBox1.Directory;
  if s<>'' then
     if FileForm.Perform(ftNewPlayList,DiskCurrentItemType,s,'',s1,False,False,'','') then begin
        UpdateAfterFileManagerTask(s1);
        if   TryToLoadFile(s1) then FileListBox1.FileName:=s1
        else TryToLoadFile('');
        end
     else
  else Msg(DiskFullText,'',MB_OK+MB_ICONERROR);
  FileListBox1.Directory:=oDirectory;
  FileListBox1.Mask:=FilterComboBox1.Mask;
end;

procedure TOpenForm.MenuItemPlayListOpenClick(Sender: TObject);
var s,oDirectory:String;
begin
  with OpenDialog1 do begin
    s:=StrWithoutTrailingPathDelimiter(ExtractFilePath(CurrentCollectionFileName));
    if not DirectoryExists(s) then s:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
    InitialDir:=s; FileName:='';
    if Execute and (AnsiCompareText(FileName,CurrentCollectionFileName)<>0) then begin
       oDirectory:=FileListBox1.Directory;
       if TryToLoadFile(FileName) then //
       else TryToLoadFile('');
       FileListBox1.Directory:=oDirectory;
       FileListBox1.Mask:=FilterComboBox1.Mask;
       end;
    end;
end;

procedure TOpenForm.MenuItemPlayListDeleteClick(Sender: TObject);
begin
  if CurrentCollectionFileName<>'' then with CollectionStringGrid do
     if Msg(PlayListDeleteText+NL+NL+NoUndoText,Caption,MB_YESNO+MB_DEFBUTTON2)=ID_YES then begin
        RowCount:=1; Cells[1,0]:=''; IsModified:=False;
        DeleteFile(CurrentCollectionFileName);
        CurrentCollectionFileName:='';
        LoadOrMakeNewPlayListFile;
        if   (CurrentCollectionFileName<>'') and
             TryToLoadFile(CollectionStringGrid.Cells[1,0]) then //
        else TryToLoadFile('');
        end;
end;

procedure TOpenForm.PlayListPopupMenuPopup(Sender: TObject);
begin
  MenuItemPlayListAddFile   .Enabled:=CollectionGroupBox.Visible and (FileListBox1.ItemIndex>=0);
  MenuItemPlayListAddFolder .Enabled:=CollectionGroupBox.Visible and (FileListBox1.Items.Count>=0);
  MenuItemPlayListClear     .Enabled:=CollectionGroupBox.Visible and (CollectionStringGrid.Cells[1,0]<>'');
  MenuItemPlayListDelete    .Enabled:=CollectionGroupBox.Visible and (CurrentCollectionFileName<>'');

  MenuItemPlayListAddFile1  .Enabled:=MenuItemPlayListAddFile  .Enabled;
  MenuItemPlayListAddFolder1.Enabled:=MenuItemPlayListAddFolder.Enabled;
  
  if PanelToolTips.Visible then PanelToolTips.Hide;
end;

procedure TOpenForm.CollectionStringGridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key=VK_RETURN then EnterKeyDown:=True;
end;

procedure TOpenForm.CollectionStringGridKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if      Key=VK_RETURN then begin
          if EnterKeyDown then BtnOpenClick(Sender); // EnterKeyDown': [Enter] was pressed while the grid has focus
          EnterKeyDown:=False;
          end
  else if Key=VK_HOME   then CollectionStringGrid.Row:=0
  else if Key=VK_END    then CollectionStringGrid.Row:=Pred(CollectionStringGrid.RowCount)
  else if Key=VK_DELETE then
          if Task=otMusicPlayer then MenuItemPlayListRemoveItemClick(Sender)
          else;
end;

procedure TOpenForm.LoadOrMakeNewPlayListFile;
var i:Integer; s,oDirectory:String;
begin
  if   MainForm.Music<>nil then
       CurrentCollectionFileName:=MainForm.Music.PlayListFileName;

  if   IsAPlayListFile(CurrentCollectionFileName) and
       PlayListLoadFromFile(CurrentCollectionFileName) then //
  else CurrentCollectionFileName:='';

  if   CurrentCollectionFileName='' then begin
       oDirectory:=FileListBox1.Directory;
       FileListBox1.Directory:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
       FileListBox1.Mask     :=STAR+PLAYLIST_FILE_EXT;
       for i:=0 to Pred(FileListBox1.Items.Count) do begin
           s:=StrWithTrailingPathDelimiter(FileListBox1.Directory)+FileListBox1.Items[i];
              if IsAPlayListFile(s) and
                 PlayListLoadFromFile(s) then begin
                 CurrentCollectionFileName:=s; break;
                 end;
           end;
       FileListBox1.Directory:=oDirectory;
       FileListBox1.Mask     :=FilterComboBox1.Mask;
       end;

  if   CurrentCollectionFileName='' then begin
       s:=PlayListNewFileName(StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath));
       if s<>'' then with CollectionStringGrid do begin
          RowCount:=1; Cells[1,0]:='';
          if (Music<>nil) and
             PlayListSaveToFile(s,{CollectionStringGrid,1}'',Music.PlayList) and
             IsAPlayListFile(s) and
             PlayListLoadFromFile(s) then
             CurrentCollectionFileName:=s;
          end;
       end;
end;

procedure TOpenForm.DriveComboBox1DropDown(Sender: TObject);
var i:Integer; oCursor:TCursor;
begin  // update the combobox - Delphi does not automatically remove an empty CD-drive from the list

       // please note that the program does not handle all exceptions gracefully
       // when the user removes disks and CD's from a drive.
       // The reason is, that the Delphi-components 'TDriveComboBox',
       // 'TDirectoryListBox' and 'TFileListBox' use a simple unprotected 'ChDir' command,
       // raising an 'IO Error 21', which cannot be hidden from the user
  with DriveComboBox1 do begin
    oCursor:=Screen.Cursor;
    try     Screen.Cursor:=crHourGlass;
            DriveComboBoxBuildList(DriveComboBox1);
    finally Screen.Cursor:=oCursor;
    end;
    for i:=0 to Pred(Items.Count) do
        if (Items[i]<>'') and
           (LoCase(Drive)=LoCase(Items[i][1])) then begin
           ItemIndex:=i; break;
           end;
    end;
end;

procedure TOpenForm.DriveComboBoxBuildList(var DriveComboBox:TDriveComboBox);
var // copy from 'FileCtrl';
  DriveNum: Integer;
  DriveChar: Char;
  DriveType: TDriveType;
  DriveBits: set of 0..25;
  ErrorMode: Cardinal;

  procedure AddDrive(const VolName: string; Obj: TObject);
  begin
    if DirectoryExists(DriveChar+':\') then // caution: this line is NOT a part of the original procedure from 'FileCtrl'
       DriveComboBox.Items.AddObject(Format('%s: %s',[DriveChar, VolName]), Obj);
  end;

begin
  ErrorMode := SetErrorMode(SEM_FailCriticalErrors); // NOT a part of the original procedure from 'FileCtrl'
  try
    { fill list }
    DriveComboBox.Items.Clear;
    Integer(DriveBits) := GetLogicalDrives;
    for DriveNum := 0 to 25 do
    begin
      if not (DriveNum in DriveBits) then Continue;
      DriveChar := Char(DriveNum + Ord('a'));
      DriveType := TDriveType(GetDriveType(PChar(DriveChar + ':\')));
      if DriveComboBox.TextCase = FileCtrl.tcUpperCase then
        DriveChar := Upcase(DriveChar);

      case DriveType of
        dtFloppy:   // testing a floppydisk takes too long time; thus it's preferred to accept the ugly error 'IO Error 21', which is the result of an attempt to access an empty drive
                    //if DirectoryExists(DriveChar+':\') then // caution: this is NOT a part of the original procedure from 'FileCtrl'
                    DriveComboBox.Items.AddObject(DriveChar + ':', FloppyBMP);
        dtFixed:    AddDrive(VolumeID(DriveChar), FixedBMP);
        dtNetwork:  AddDrive(NetworkVolume(DriveChar), NetworkBMP);
        dtCDROM:    AddDrive(VolumeID(DriveChar), CDROMBMP);
        dtRAM:      AddDrive(VolumeID(DriveChar), RAMBMP);
      end;
    end;
    DriveComboBox.Update;
  finally // restore old error mode
    SetErrorMode(ErrorMode); // NOT a part of the original procedure from 'FileCtrl'
  end;
end;

procedure TOpenForm.DriveComboBoxReadBitmaps;
begin //copy from 'FileCtrl';
  { assign bitmap glyphs }
  FloppyBMP := TBitmap.Create;
  FloppyBMP.Handle := LoadBitmap(HInstance, 'FLOPPY'); // don't localize
  FixedBMP := TBitmap.Create;
  FixedBMP.Handle := LoadBitmap(HInstance, 'HARD');
  NetworkBMP := TBitmap.Create;
  NetworkBMP.Handle := LoadBitmap(HInstance, 'NETWORK');
  CDROMBMP := TBitmap.Create;
  CDROMBMP.Handle := LoadBitmap(HInstance, 'CDROM');
  RAMBMP := TBitmap.Create;
  RAMBMP.Handle := LoadBitmap(HInstance, 'RAM');
end;

function TOpenForm.IsMediaPresent(Drive:Char):Boolean;
var Ch:Char;
begin
  Drive:=LoCase(Drive);
  Result:=DirectoryExists(Drive+COLON+FILE_NAME_PATH_DELIMITER);
  if not Result then begin
     Msg(Format(MediaDisappearedText__,[UpCase(Drive)]),'',MB_OK);
     if   MainForm.ApplicationDataPath<>'' then
          Ch:=LoCase(MainForm.ApplicationDataPath[1])
     else Ch:=LoCase(Application.ExeName[1]);
     if        (Ch<>Drive) and DirectoryExists(Ch+COLON+FILE_NAME_PATH_DELIMITER) then
               DirectoryListBox1.Directory:=Ch+COLON
     else if   (WindowsDrive<>#0)and DirectoryExists(WindowsDrive+COLON+FILE_NAME_PATH_DELIMITER) then
               DirectoryListBox1.Directory:=WindowsDrive+COLON
          else BtnCancelClick(Self);
     end;
end;

procedure TOpenForm.BtnImagePriorClick(Sender: TObject);
var i:Integer;
begin
  i:=Pred(FileListBox1.ItemIndex);
  if i<0 then i:=Pred(FileListBox1.Items.Count);
  if i<>FileListBox1.ItemIndex then
     FileListBox1.FileName:=StrWithTrailingPathDelimiter(FileListBox1.Directory)+FileListBox1.Items[i];
end;

procedure TOpenForm.BtnImageNextClick(Sender: TObject);
var i:Integer;
begin
  if FileListBox1.Items.Count>0 then begin
     i:=Succ(FileListBox1.ItemIndex);
     if i>=FileListBox1.Items.Count then i:=0;
     if i<>FileListBox1.ItemIndex then
        FileListBox1.FileName:=StrWithTrailingPathDelimiter(FileListBox1.Directory)+FileListBox1.Items[i];
     end;
end;

procedure TOpenForm.OnActivate;
begin
  Application.OnHint                 :=ShowHint;
  Application.OnMessage              :=ApplicationOnMessage;
  oApplicationOnDeactivate           :=Application.OnDeactivate;
  Application.OnDeactivate           :=ApplicationOnDeactivate;

  if DoMaximize then begin
     DoMaximize:=False;
     PostMessage(Self.Handle,MSG_MAXIMIZE,0,0);
     end;

  StatusBar1.SizeGrip                :=IsWindowsDefaultColorBtnFace(StatusBar1.Color) or
                                       (Graphics.ColorToRGB(StatusBar1.Color)=Graphics.ColorToRGB(clBlack ));

  LevelsFolderHistoryComboBox.Width  :=VerticalScrollBarWidth+4;
//FolderHistoryComboBox.Height       :=FilterComboBox1.Height;
  LevelsFolderHistoryComboBox.Height :=DriveComboBox1.Height;
//FolderHistoryComboBox.Top          :=DirectoryListBox1.Top-FolderHistoryComboBox.Height;
  LevelsFolderHistoryComboBox.Left   :=DirectoryListBox1.Left+DirectoryListBox1.Width-LevelsFolderHistoryComboBox.Width;

  LevelsFileHistoryComboBox  .Width  :=LevelsFolderHistoryComboBox.Width;
  LevelsFileHistoryComboBox  .Height :=LevelsFolderHistoryComboBox.Height;
//FileHistoryComboBox  .Top          :=FileListBox1.Top-FileHistoryComboBox.Height;
  LevelsFileHistoryComboBox  .Left   :=FileListBox1.Left+FileListBox1.Width-LevelsFileHistoryComboBox.Width;

  with LevelsFolderHistoryComboBox   do AnythingButLevelsFolderHistoryComboBox.SetBounds(Left,Top,Width,Height);
  with LevelsFileHistoryComboBox     do AnythingButLevelsFileHistoryComboBox  .SetBounds(Left,Top,Width,Height);

  DirectoryListBox1VisibleRowCount   :=DirectoryListBox1.ClientHeight div DirectoryListBox1.ItemHeight;
  FileListBox1VisibleRowCount        :=FileListBox1.ClientHeight      div FileListBox1.ItemHeight;

  with DirectoryListBox1     do Tag  :=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // the tags in in 'DirectoryListBox1' and 'FileListBox1' are used for storing the tooltip width threshold;
  with FileListBox1          do Tag  :=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // '-TOOL_TIP_FREE_ZONE_PIXELS': 'OnMouseMove' isn't triggered when the mouse is over the scrollbar, hence, this extra tooltip free zone
  with PluginLevelStringGrid do Tag  :=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // '-TOOL_TIP_FREE_ZONE_PIXELS': 'OnMouseMove' isn't triggered when the mouse is over the scrollbar, hence, this extra tooltip free zone

  CollectionHighlightBackgroundColor :=clHighlight;
  CollectionHighlightTextColor       :=clHighlightText;

  PanelToolTips.Visible              :=False;
  ControlMouseUp(nil,mbLeft,[],0,0);
end;

procedure TOpenForm.ExchangeGames(var Game__:TGame);
begin
  Game.MoveAnimationEnabled                        :=Game__.MoveAnimationEnabled;
  Game.SmoothMoveAnimationEnabled                  :=Game__.SmoothMoveAnimationEnabled;
  Game.SessionSmoothMoveAnimationEnabled           :=Game__.SessionSmoothMoveAnimationEnabled;
  Game.SmoothMoveAnimationThresholdEnabled         :=Game__.SmoothMoveAnimationThresholdEnabled;
  Game.SmoothMoveAnimationThresholdMaxPixelsPerMove:=Game__.SmoothMoveAnimationThresholdMaxPixelsPerMove;
  Game.PlayerDirectionAnimationEnabled             :=Game__.PlayerDirectionAnimationEnabled;
  Game.AnimateDoMoveMS                             :=Game__.AnimateDoMoveMS;
  Game.AnimateUndoMoveMS                           :=Game__.AnimateUndoMoveMS;
  Game.AnimateReplayMovesMS                        :=Game__.AnimateReplayMovesMS;
  Game.AnimateMovesOnMouseWheelUpDown              :=Game__.AnimateMovesOnMouseWheelUpDown;
  Game.Color                                       :=Game__.Color;
  Game.DeadlockDetection                           :=Game__.DeadlockDetection;
  Game.ForkliftDrivingEnabled                      :=Game__.ForkliftDrivingEnabled;
  Game.ForkliftDrivingDriveInReverseSquares        :=Game__.ForkliftDrivingDriveInReverseSquares;
  Game.JumpsAllowedAfterFirstBoxMoveInReverseMode  :=Game__.JumpsAllowedAfterFirstBoxMoveInReverseMode;
  Game.PathFindingMaxTimeMS                        :=Game__.PathFindingMaxTimeMS;
  Game.PathFindingOptimizeMoves                    :=Game__.PathFindingOptimizeMoves;
  Game.PrettyPrintGamesEnabled                     :=Game__.PrettyPrintGamesEnabled;
  Game.ResetSaveGameAndLoadItIfItIsANormalModeGame :=Game__.ResetSaveGameAndLoadItIfItIsANormalModeGame;
  Game.RestoreSaveGame                             :=Game__.RestoreSaveGame;
  Game.RleEnabled                                  :=Game__.RleEnabled;
  Game.SaveBestSolutionsAutomatically              :=Game__.SaveBestSolutionsAutomatically;
  Game.SaveOldSolutionsAfterFindingBetterOnes      :=Game__.SaveOldSolutionsAfterFindingBetterOnes;
  Game.SaveOldSolutionsAfterClipboardImport        :=Game__.SaveOldSolutionsAfterClipboardImport;
  Game.SaveSnapshotsAutomatically                  :=Game__.SaveSnapshotsAutomatically;
  Game.ShowBoxStartPositionsAsGoalsInReverseMode   :=Game__.ShowBoxStartPositionsAsGoalsInReverseMode;
  Game.ShowGame                                    :=Game__.ShowGame;
  Game.SimpleIllegalMovesMask                      :=Game__.SimpleIllegalMovesMask;
  Game.SecondaryMetricsInTitles                    :=Game__.SecondaryMetricsInTitles;
  Game.SolutionsRequireAtLeastOnePush              :=Game__.SolutionsRequireAtLeastOnePush;
  Game.TimingEnabled                               :=Game__.TimingEnabled;
  Game.TimingIdleTimeThresholdEnabled              :=Game__.TimingIdleTimeThresholdEnabled;
  Game.TimingIdleTimeThresholdMS                   :=Game__.TimingIdleTimeThresholdMS;
  SwapNodes(TNode(Game),TNode(Game__));
  Game.SokoFileName                                :='';  // ensure that 'Game.CloseLevel' doesn't try to save the game
  Game.DeadlockDetection.Deadlocks                 :=nil; // 'Deadlocks' is assigned to 'Game__', not to 'Game'
end;

procedure TOpenForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then ShortCutsEnabled:=True; // Shortcuts are disabled when Ctrl+<letter> changes the selected item in the file listbox ('FileListBox1')
end;

procedure TOpenForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if      (Key=VK_ESCAPE)
          or
          (Key=VK_BROWSER_BACK)
          or
          ((Key=VK_SPACE)
           and
           ((CurrentMemo=nil) or CurrentMemo.ReadOnly)
          )
          or
          ((Shift=[ssAlt]) and (Key=Ord(ACCEL_CHAR_EXIT))) then
          if   EscapeEnabled then
               if (not IsModified)              and
                  (not GameInfoMemo  .Modified) and
                  (not SkinScriptMemo.Modified) and
                  (not TextFileMemo  .Modified) then Close
               else
          else EscapeEnabled:=True
  else if (Key=Ord(ACCEL_CHAR_COPY_TO_CLIPBOARD                )) and
          ((Shift=[ssCtrl]) or (Shift=[ssShift,ssCtrl]) or (Shift=[ssAlt]) or (Shift=[ssAlt,ssCtrl]) or (Shift=[ssAlt,ssShift,ssCtrl])) and (Task=otGame ) and ShortCutsEnabled and
          (ActiveControl<>CurrentMemo) then
          CopyLevelToClipBoard(ssAlt in Shift,(ssShift in Shift) or MenuItemCopyToClipboardFormatRLE.Checked,False,False)
  else if (Key=Ord(ACCEL_CHAR_COPY_SOLUTION_MOVES_TO_CLIPBOARD )) and ((Shift=[ssCtrl]) or (Shift=[ssShift,ssCtrl])) and (Task=otGame ) and ShortCutsEnabled then
          CopySolutionToClipboard(False,(ssShift in Shift) or MenuItemCopyToClipboardFormatRLE.Checked)
  else if (Key=Ord(ACCEL_CHAR_COPY_SOLUTION_PUSHES_TO_CLIPBOARD)) and ((Shift=[ssCtrl]) or (Shift=[ssShift,ssCtrl])) and (Task=otGame ) and ShortCutsEnabled then
          CopySolutionToClipboard(True,(ssShift in Shift) or MenuItemCopyToClipboardFormatRLE.Checked)
  else if (Key=Ord(ACCEL_CHAR_COPY_COLLECTION_TO_CLIPBOARD     )) and ((Shift=[ssCtrl]) or (Shift=[ssShift,ssCtrl])) and (Task=otGame ) and ShortCutsEnabled then
          CopyCollectionToClipboard(MenuItemCopyCollectionToClipboardWriteTitleAndAuthor.Checked,False,False,(ssShift in Shift) or MenuItemCopyToClipboardFormatRLE.Checked)
  else if (Key=Ord(ACCEL_CHAR_COPY_COLLECTION_SOLUTIONS_TO_CLIPBOARD)) and ((Shift=[ssCtrl]) or (Shift=[ssShift,ssCtrl])) and (Task=otGame ) and ShortCutsEnabled then
          CopyCollectionToClipboard(MenuItemCopyCollectionToClipboardWriteTitleAndAuthor.Checked,False,True,(ssShift in Shift) or MenuItemCopyToClipboardFormatRLE.Checked)
  else if (Key=Ord(ACCEL_CHAR_COPY_TO_CLIPBOARD_FORMAT)) and ((Shift=[ssCtrl]) or (Shift=[ssShift,ssCtrl]) or (Shift=[ssAlt]) or (Shift=[ssAlt,ssCtrl]) or (Shift=[ssAlt,ssShift,ssCtrl])) and (Task=otGame ) and ShortCutsEnabled then
          if  (ssShift in Shift) or (ssAlt in Shift) then
               MenuItemCopyToClipboardFormatClick(MenuItemCopyToClipboardFormatRLE)
          else MenuItemCopyToClipboardFormatClick(MenuItemCopyToClipboardFormatNormal)
  else if (Key=Ord(ACCEL_CHAR_PASTE_FROM_CLIPBOARD               )) and (Shift=[ssCtrl]) and (Task=otGame ) and ShortCutsEnabled and (ActiveControl<>CurrentMemo) then
          MenuItemImportFromClipboardClick(Sender)
  else if (Key=Ord(ACCEL_CHAR_DUPLICATE_LEVELS_CURRENT_LEVEL     )) and (Shift=[ssCtrl]) and (Task=otGame) and MenuItemSubMenuFindDuplicateLevels.Visible then
          MenuItemFindDuplicateLevelsClick(MenuItemFindDuplicateLevelsCurrentLevel)
  else if (Key=Ord(ACCEL_CHAR_DUPLICATE_LEVELS_CURRENT_COLLECTION)) and (Shift=[ssCtrl]) and (Task=otGame) and MenuItemSubMenuFindDuplicateLevels.Visible then
          MenuItemFindDuplicateLevelsClick(MenuItemFindDuplicateLevelsCurrentCollection)
  else if (Key=Ord(ACCEL_CHAR_DUPLICATE_LEVELS_ALL_LEVELS        )) and (Shift=[ssCtrl]) and (Task=otGame) and MenuItemSubMenuFindDuplicateLevels.Visible then
          MenuItemFindDuplicateLevelsClick(MenuItemFindDuplicateLevelsAllLevels)
  else if (Key=Ord(ACCEL_CHAR_REORGANIZE_FILE                    )) and (Shift=[ssCtrl]) and (Task=otGame ) and ShortCutsEnabled then
          MenuItemFileReorganizeClick(Sender)
  else if (Key=Ord(ACCEL_CHAR_IMAGE_TEXT                         )) and (Shift=[ssCtrl]) and (Task=otImage) and ShortCutsEnabled and
          (SubTask=osSkin) then
          MenuItemImageTextClick(Sender)
  else if Key=VK_F1  then BtnHelpClick(Sender)
  else if Key=VK_F12 then
          if   WindowState=wsNormal then
               WindowState:=wsMaximized
          else WindowState:=wsNormal
  else;
end;

procedure TOpenForm.BitBtnOpenClick(Sender: TObject);
var p:TPoint;
begin
  p:=ClientToScreen(Point(0,0));
  OpenPopupMenu.Popup(p.x+GameFileGroupBox.Left+BtnOpen.Left,
                      p.y+GameFileGroupBox.Top +BtnOpen.Top +BtnOpen.Height);
end;

procedure TOpenForm.BtnGameBuiltinSolutionsClick(Sender: TObject);
var ACol:Integer;
begin
  if ReloadCurrentGameIfNecessary and
     Game.ImportBuiltInSolutions then begin
     LoadGameData(False);
     if CurrentFileStringGridRow>=0 then with CollectionStringGrid do
        for ACol:=2 to Pred(ColCount) do Cells[ACol,CurrentFileStringGridRow]:='';
     ShowSolutionsForVisibleLevels;
     end;
end;

procedure TOpenForm.SetFileFilter(const Filter__:String);
var s:String;
begin
  if   System.Pos(BAR,Filter__)=0 then begin
       case Task of
         otSound      : s:=SoundFilesText;
         otMusic,
         otMusicPath,
         otMusicPlayer: s:=MusicFilesText;
         otImage      : s:=ImageFilesText;
         otPalette    : s:=ColorPaletteFilesText;
         otSolver     : s:=PluginsText;
         otOptimizer  : s:=PluginsText;
         else           s:='';
       end; // case
       s:=s+'  ('+Filter__+')'+BAR+Filter__;
       end
  else s:=Filter__; // // filters with a "|" are assumed to be pre-formatted
  if s<>'' then s:=s+BAR;
  FilterComboBox1.Filter:=s+AllFilesFilterText;
end;

procedure TOpenForm.SetFileFilterIndex(const FileName:String);
var i,j:Integer; s:String;
begin
  if FileName<>'' then with FilterComboBox1 do begin
     s:=AnsiLowerCase(ExtractFileExt(FileName));
     if (s='') or (ItemIndex=Pred(Items.Count)) then j:=Pred(Items.Count) // last item is always 'all files'
     else begin
        s:=STAR+s; j:=-1;
        if   (ItemIndex>=0) and (AnsiPos(s,Items[ItemIndex])<>0) then j:=ItemIndex
        else for i:=0 to Pred(Items.Count) do
                 if (AnsiPos(s,Items[i])<>0) and (j=-1) then j:=i;
        if j=-1 then j:=Pred(Items.Count); // '-1': not found, use 'all files'
        end;
     if j<>ItemIndex then ItemIndex:=j;
     FileListBox1.Mask:=Mask;
     end;
end;

procedure TOpenForm.FolderHistoryComboBoxChange(Sender: TObject);
var FolderName:String;
begin
  if Sender is TComboBox then with Sender as TComboBox do
     if ItemIndex>=0 then begin
        FolderName:=ExpandedFilePath(Items[ItemIndex],MainForm.MyDocumentsFolder);
        if DirectoryExists(FolderName) then begin
           if (Task=otImage) and (SubTask=osSkin) then SkinTypeDetection(FolderName);
           DirectoryListBox1.Directory:=StrWithTrailingPathDelimiter(FolderName);
           if (Screen.ActiveForm=Self) and
              DiskGroupBox.Visible and DirectoryListBox1.Visible and
              (FileListBox1.Items.Count<>0) and
              (ActiveControl=DirectoryListBox1) and
              (Task=otImage) then
              FileListBox1.SetFocus;
           end;
        end;
end;

procedure TOpenForm.FolderHistoryComboBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then EscapeEnabled:=False;
end;

procedure TOpenForm.FileHistoryComboBoxChange(Sender: TObject);
var s:String;
begin
  if Sender is TComboBox then with Sender as TComboBox do
     if ItemIndex>=0 then begin
        s:=ExpandedFilePath(Items[ItemIndex],MainForm.MyDocumentsFolder);
        if IsAnIniFileSectionFileName(s) then s:=ExtractIniFileName(s);
        if FileExists(s) then begin
           SetFileFilterIndex(s);
           DirectoryListBox1.Directory:=StrWithTrailingPathDelimiter(ExtractFilePath(s)); // without trailing path delimiter, the assignment fails if the new value is the root folder on the currently selected drive
           TryToLoadFile(ExpandedFilePath(Items[ItemIndex],MainForm.MyDocumentsFolder));
           end;
        end;
end;

procedure TOpenForm.FileHistoryComboBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then EscapeEnabled:=False;
end;

procedure TOpenForm.SetCollectionStringGridColumnWidths;
var ACol,W:Integer; s:String;
begin
  with CollectionStringGrid do begin
    s:=SPACE+SPACE+SPACE;
    if RowCount>0 then s:=s+Cells[0,Pred(RowCount)];
    ColWidths[0]:=Canvas.TextWidth(s);
    if (Task=otGame) and (ColCount>2) then begin
       ColWidths[2]:=Canvas.TextWidth(' *99999/99999');
       for ACol:=3 to Pred(ColCount) do ColWidths[ACol]:=ColWidths[2];
       end;
    W:=ClientWidth-ColCount*GridLineWidth;
    for ACol:=0 to Pred(ColCount) do
        if ACol<>1 then Dec(W,ColWidths[ACol]);
    ColWidths[1]:=Max(0,W);

    if LeftCol<>FixedCols then
       LeftCol:=FixedCols; // ensure that the user cannot scroll the grid horizontally;
    while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]);
    end;
end;

function  TOpenForm.GetSolutionColumns:Integer;
begin
  Result:=Max(0,CollectionStringGrid.ColCount-2);
end;

procedure TOpenForm.SetSolutionColumns(SolutionColumns__:Integer);
begin
  SolutionColumns__:=Max(0,Min(2,SolutionColumns__));
  if SolutionColumns__<>SolutionColumns then with CollectionStringGrid do
     try
       ColCount:=2+SolutionColumns__;
       SetCollectionStringGridColumnWidths;
       ClearSolutionStatistics;
       ShowSolutionsForVisibleLevels;
     except on E:Exception do Error(E.Message,'');
     end;
end;

procedure TOpenForm.MenuItemSolutionMovesAndOrPushesClick(Sender: TObject);
begin
  if Sender is TMenuItem then with Sender as TMenuItem do
     if        (Sender=MenuItemSolutionMoves) or
               (Sender=MenuItemSolutionPushes) then
               SetSolutionColumns(Abs(Pred(SolutionColumns))) // 0 -> Abs(0-1)=1; 1 -> Abs(1-1)=0
          else if SolutionColumns<2 then SetSolutionColumns(2)
               else SetSolutionColumns(0);
end;

procedure TOpenForm.MenuItemIncludeBuiltinSolutionsClick(Sender: TObject);
begin
  MenuItemIncludeBuiltinSolutions.Checked:=not MenuItemIncludeBuiltinSolutions.Checked;
  ClearSolutionStatistics;
  ShowSolutionsForVisibleLevels;
end;

procedure TOpenForm.ClearSolutionStatistics;
var ACol,ARow:Integer;
begin
  for ACol:=2 to Pred(CollectionStringGrid.ColCount) do with CollectionStringGrid do
      for ARow:=0 to Pred(CollectionStringGrid.RowCount) do Cells[ACol,ARow]:='';
  CurrentCollectionCheckedLevelsCountDown:=CollectionStringGrid.RowCount;
  CurrentCollectionSolvedLevelsCount:=0;
  UpdateCollectionHintText;
end;

procedure TOpenForm.ShowSolutionsForVisibleLevels;
const BUILT_IN_SOLUTION_MOVES=1; BUILT_IN_SOLUTION_PUSHES=2;
var ACol,ARow:Integer; b,oVerbose,HasSolution:Boolean;
    Stats:array[0..4] of Integer; // 'Stats[1..4]': best solution moves/pushes, Stats[0]=flags for built-in solutions
    Ok:Boolean; ActiveGame:TGame;

  procedure SetCell(ACol,ARow,Moves,Pushes:Integer; IsABuiltinSolution:Boolean);
  var s:String;
  begin
    if Moves<>0 then with CollectionStringGrid do begin
       s:=Format('%d/%d',[Moves,Pushes]);
       if IsABuiltinSolution then s:=STAR+s;
       Cells[ACol,ARow]:=s;
       end;
  end;

begin // ShowSolutionsForVisibleLevels;
  if (SolutionColumns<>0) and
     CollectionGroupBox.Visible and
     (CurrentCollectionFileName<>'') and
     (CollectionStringGrid.Cells[1,0]<>'') then with CollectionStringGrid do begin // kludge: Cells[1,0]='' means that this procedure was called while loading a collection
     oVerbose:=ToolsForm.Game.Verbose;
     try
       for ARow:=TopRow to Pred(Min(RowCount,Succ(TopRow+VisibleRowCount))) do // 'Succ(TopRow...)': so a partially visible row at the bottom of the string-grid is updated too
           if Cells[1,ARow]<>'' then begin
              Ok:=True;
              for ACol:=2 to Pred(ColCount) do
                  if Cells[ACol,ARow]='' then Ok:=False;
              if not Ok then begin
                 for ACol:=2 to Pred(ColCount) do Cells[ACol,ARow]:=SPACE; // 'SPACE' marks 'visited'
                 ToolsForm.Game.Clear; // ensure that loading the new level isn't messed up somewhere in the pipeline by an attempt to reload the old level
                 ToolsForm.Game.Verbose:=False;
                 if  ToolsForm.Game.LoadFromFileOrClipBoard(MakeIniFileSectionFileName(CurrentCollectionFileName,Cells[1,ARow]),nil,nil,b) then begin
                     FillChar(Stats,SizeOf(Stats),0);
                     HasSolution:=False;
                     if   ARow=CurrentFileStringGridRow then ActiveGame:=Game
                     else ActiveGame:=ToolsForm.Game;
                     if ActiveGame.BestSolutionMoves <>nil then with ActiveGame.BestSolutionMoves  do begin
                        Stats[1]:=MoveCount; Stats[2]:=PushCount; // best solution/moves  so far
                        Stats[3]:=MoveCount; Stats[4]:=PushCount; // best solution/pushes so far
                        HasSolution:=True;
                        end;
                     if ActiveGame.BestSolutionPushes<>nil then with ActiveGame.BestSolutionPushes do begin
                        Stats[3]:=MoveCount; Stats[4]:=PushCount;
                        HasSolution:=True;
                        end;
                     if MenuItemIncludeBuiltinSolutions.Checked then begin
                        if ActiveGame.BuiltinBestSolutionMoves <>nil then with ActiveGame.BuiltinBestSolutionMoves  do begin
                           if (Stats[1]=0) or
                              (((MoveCount<=Stats[1]) and ((MoveCount<Stats[1]) or (PushCount<Stats[2])))) then begin
                              Stats[1]:=MoveCount; Stats[2]:=PushCount; Stats[0]:=Stats[0] or BUILT_IN_SOLUTION_MOVES;
                              end;
                           if (Stats[4]=0) or
                              ((PushCount<=Stats[4]) and ((PushCount<Stats[4]) or (MoveCount<Stats[3]))) then begin
                              Stats[3]:=MoveCount; Stats[4]:=PushCount; Stats[0]:=Stats[0] or BUILT_IN_SOLUTION_PUSHES;
                              end;
                           HasSolution:=True;
                           end;

                        if ActiveGame.BuiltinBestSolutionPushes<>nil then with ActiveGame.BuiltinBestSolutionPushes do begin
                           if (Stats[1]=0) or
                              ((MoveCount<=Stats[1]) and ((MoveCount<Stats[1]) or (PushCount<Stats[2]))) then begin
                              Stats[1]:=MoveCount; Stats[2]:=PushCount; Stats[0]:=Stats[0] or BUILT_IN_SOLUTION_MOVES;
                              end;
                           if (Stats[4]=0) or
                              ((PushCount<=Stats[4]) and ((PushCount<Stats[4]) or (MoveCount<Stats[3]))) then begin
                              Stats[3]:=MoveCount; Stats[4]:=PushCount; Stats[0]:=Stats[0] or BUILT_IN_SOLUTION_PUSHES;
                              end;
                           HasSolution:=True;
                           end;
                        end;

                     if (Stats[1]=Stats[3]) and (Stats[2]=Stats[4]) then begin // only 1 solution
                        if   Stats[0]<>BUILT_IN_SOLUTION_MOVES+BUILT_IN_SOLUTION_PUSHES then Stats[0]:=0; // solution/moves and solution/pushes are not both built-in solutions
                        if   MainForm.ShowSolutionMoves then
                             begin Stats[3]:=0; Stats[4]:=0; end
                        else begin Stats[1]:=0; Stats[2]:=0; end;
                        end;

                     if MainForm.ShowSolutionMoves then begin
                        SetCell(2,ARow,Stats[1],Stats[2],(Stats[0] and BUILT_IN_SOLUTION_MOVES)<>0);
                        if SolutionColumns=2 then SetCell(3,ARow,Stats[3],Stats[4],(Stats[0] and BUILT_IN_SOLUTION_PUSHES)<>0);
                        end
                      else begin
                        SetCell(2,ARow,Stats[3],Stats[4],(Stats[0] and BUILT_IN_SOLUTION_PUSHES)<>0);
                        if SolutionColumns=2 then SetCell(3,ARow,Stats[1],Stats[2],(Stats[0] and BUILT_IN_SOLUTION_MOVES)<>0);
                        end;

                     if HasSolution then Inc(CurrentCollectionSolvedLevelsCount);
                     Dec(CurrentCollectionCheckedLevelsCountDown);
                     if CurrentCollectionCheckedLevelsCountDown=0 then
                        UpdateCollectionHintText;
                     end;
                 end;
              end;
     finally ToolsForm.Game.Verbose:=oVerbose;
     end;
  end;
end;

procedure TOpenForm.CollectionStringGridTopLeftChanged(Sender: TObject);
begin
  with CollectionStringGrid do
    if LeftCol<>FixedCols then
       LeftCol:=FixedCols; // ensure that the user cannot scroll the grid horizontally;
  if SolutionColumns<>0 then ShowSolutionsForVisibleLevels;
end;

procedure TOpenForm.MenuItemDragLevelsClick(Sender: TObject);
begin
  MenuItemDragLevels.Checked:=not MenuItemDragLevels.Checked;
end;

procedure TOpenForm.MenuItemAutoScrollClick(Sender: TObject);
begin
  MenuItemAutoScroll.Checked:=not MenuItemAutoScroll.Checked;
end;

function TOpenForm.CalculateMinHeight:Integer;
begin
  Result:=Min(525,DiskGroupBox.Top+DiskGroupBoxMinimumHeight+PANEL_Y_GAP+CollectionGroupBoxMinimumHeight+CollectionGroupBox.Left+StatusBar1.Height+(Height-ClientHeight));
end;

function TOpenForm.CalculateMinWidth:Integer;
begin
  Result:=DiskGroupBox.Left+DiskGroupBoxMinimumWidth+PANEL_X_GAP+GameFileGroupBoxMinimumWidth+DiskGroupBox.Left+(Width-ClientWidth);
end;

function  TOpenForm.ReloadCurrentGameIfNecessary:Boolean;
// this logic is obsolete because string-grid statistics -  for safety -
// now uses 'ToolsForm.Game' for loading levels, not 'Game'
//
// ensure that 'Game' contains currently shown level, if any;
// ('Game' may have been used for showing statistics for levels
//  visible in the collection string-grid, thus, it may hold another
// level at the time this function is called.)
begin
  Result:=True;
{
  Result:=(Game=nil) or (CurrentFileName='') or StrEqual(Game.FileName,CurrentFileName);
  if not Result then
     if Game.LoadFromFileOrClipBoard(CurrentFileName) then Result:=True
     else begin // reload failed: either the level has been deleted or something went terribly wrong;
                // the best thing to do is to clear everything
        if   (CurrentCollectionFileName<>'') and (CurrentSectionName<>'') then
             Error(Format(OpenPackFileMemberFailedText__,[CurrentSectionName,CurrentCollectionFileName]),'')
        else Error(Format(OpenFileFailedShortText__     ,[CurrentFileName]),'');
        CurrentFileName:='';
        TryToLoadFile('');
        end;
}
end;

function TOpenForm.CopyCollectionToClipboard0(SokoFile__:TSokoFile; const FileName__,Text__:String; WriteTitleAndAuthor__,CopyAllSnapshots__,CopySolutionsOnly__,RunLengthEncoding__,CopyLevelNotes__,ShowResult__:Boolean; var Count__:Integer):Boolean;
var TempFileName,Text,Caption:String; FileTime:TFileTime; oCursor:TCursor;
begin //
  Result:=False;
  if SokoFile__<>nil then begin
     Count__:=0; oCursor:=Screen.Cursor;
     TempFileName:=MainForm.ApplicationDataPath+ExtractFileName(ChangeFileExt(Application.ExeName,WORK_FILE_NAME_EXT));
     try     Screen.Cursor:=crHourGlass;
             if (CopySolutionsOnly__
                 and
                 ExportBestSolutionsToFile(SokoFile__,ToolsForm.Game,TempFileName,RunLengthEncoding__,Count__)
                )
                or
                ((not CopySolutionsOnly__)
                 and
                 SokoFile__.ExportToFile(TempFileName,WriteTitleAndAuthor__,CopyLevelNotes__,CopyAllSnapshots__,False,MainForm.CopyLevelToClipboardFillFloors,RunLengthEncoding__,MainForm.CopyLevelToClipboardFloorFillCharacter,MainForm.RunLengthEncodingFloor,Count__)
                ) then
                try     if   SokUtil_.LoadTextFromFile(Text,FileTime,TempFileName) then begin
                             Clipboard.AsText:=Text; Result:=True;
                             end;
                finally SysUtils.DeleteFile(TempFileName);
                end;
     finally Screen.Cursor:=oCursor;
     end;
     if   Result then begin
          if ShowResult__ then begin
             if    Text__<>'' then begin
                   if   System.Pos('%d',Text__)<>0 then
                        Text:=Format(Text__,[Count__])
                   else Text:=Text__;
                   end
             else  if   CopySolutionsOnly__ then
                        Text:=Format(SolutionsCopiedToClipboardText__,[Count__])
                   else Text:=CollectionCopiedToClipboardText;
             Caption:=TEXT_APPLICATION_TITLE;
             if FileName__<>'' then Caption:=Caption+SUB_TITLE_SEPARATOR+ExtractFileName(FileName__);
             Msg(Text,Caption,MB_OK);
             end;
          end
     else Msg(TEXT_TASK_FAILED,TEXT_APPLICATION_TITLE,MB_OK);
     end;
end;

function TOpenForm.CopyCollectionToClipboard(WriteTitleAndAuthor__,CopyAllSnapshots__,CopySolutionsOnly__,RunLengthEncoding__:Boolean):Boolean;
var Count:Integer;
begin
  Result:=(Game<>nil) and
          (Game.SokoFile<>nil) and
          ((CurrentCollectionFileName<>'')
            or
            CopySolutionsOnly__
          ) and
          CopyCollectionToClipboard0(Game.SokoFile,CurrentCollectionFileName,'',WriteTitleAndAuthor__,CopyAllSnapshots__,CopySolutionsOnly__,RunLengthEncoding__,MainForm.CopyLevelCollectionToClipboardIncludeLevelComments,True,Count);
end;

procedure TOpenForm.MenuItemCopyCollectionToClipboardClick(Sender: TObject);
begin
  CopyCollectionToClipboard(MenuItemCopyCollectionToClipboardWriteTitleAndAuthor.Checked,False,Sender=MenuItemCopyCollectionSolutionsToClipboard,MenuItemCopyToClipboardFormatRLE.Checked);
end;

procedure TOpenForm.MenuItemCopyCollectionToClipboardWriteTitleAndAuthorClick( Sender: TObject);
begin
  MenuItemCopyCollectionToClipboardWriteTitleAndAuthor.Checked := not MenuItemCopyCollectionToClipboardWriteTitleAndAuthor.Checked;
end;


function TOpenForm.CopyLevelToClipboard(OppositeFillFloorsSetting__,RunLengthEncoding__,BoardOnly__,CurrentBoardOnly__:Boolean):Boolean;
var FloorFillChar:Char;
begin
  if        RunLengthEncoding__ then
            FloorFillChar:=FLOOR_CH // don't fill the floors before run-len encoding; it uses 'RunLengthEncodingFloor' itself
  else if   (     MainForm.CopyLevelToClipboardFillFloors  and (not OppositeFillFloorsSetting__))
            or
            ((not MainForm.CopyLevelToClipboardFillFloors) and      OppositeFillFloorsSetting__ ) then
            FloorFillChar:=MainForm.CopyLevelToClipboardFloorFillCharacter
       else FloorFillChar:=FLOOR_CH;

  if   (Game<>nil) and (CurrentFileName<>'') then
       Result:=Game.CopyToClipboard(FloorFillChar,MainForm.CopyLevelToClipboardPreserveCombinedMoves,RunLengthEncoding__,BoardOnly__,CurrentBoardOnly__)
  else Result:=False;
end;

procedure TOpenForm.MenuItemCopyLevelToClipboardClick(Sender: TObject);
begin
  CopyLevelToClipboard(Sender=nil,MenuItemCopyToClipboardFormatRLE.Checked,False,False);
end;

function TOpenForm.CopySolutionToClipboard(SolutionPushes__,RunLengthEncoding__:Boolean):Boolean;
var Solution:TSnapshot;
begin
  Result:=False;
  if (Game<>nil) and (CurrentFileName<>'') then with Game do begin
     if   SolutionPushes__ and (BestSolutionPushes<>nil) then
          Solution:=BestSolutionPushes
     else Solution:=BestSolutionMoves;
     if Solution<>nil then begin
        Result:=CopySnapshotToClipboard(Solution,RunLengthEncoding__);
        if Result then with Solution do
           //if Screen.ActiveForm=OpenForm then
           //   this doesn't work because the message immidately is overwritten by a normal hint
           //   OpenForm.StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=Format(SolutionCopiedToClipboardText__,[MoveCount,PushCount])
           //else
                Msg(Format(SolutionCopiedToClipboardText__,[MoveCount,PushCount]),PChar(TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+DisplayName),MB_OK)
        else Msg(TEXT_TASK_FAILED,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+DisplayName,MB_OK);
        end;
     end;
end;

procedure TOpenForm.MenuItemCopySolutionToClipboardClick(Sender: TObject);
begin
  CopySolutionToClipboard(Sender=MenuItemCopySolutionPushesToClipboard,
                          MenuItemCopyToClipboardFormatRLE.Checked);
end;

procedure TOpenForm.MenuItemImportFromClipboardClick(Sender: TObject);
var Modified:Boolean; s1,s2:String;
begin
  if (ToolsForm.Game<>nil) and (not IsLoading) then
     if ClipBoard.HasFormat(CF_TEXT) then begin
        ToolsForm.Game.Clear;
        if ToolsForm.Game.LoadFromFileOrClipboard('',Self.Game,nil,Modified) and
           (ToolsForm.Game.FileName<>'') then begin
           if IsANewFileName(ToolsForm.Game.FileName) then begin // 'True': a single level was imported; multi-level import would have saved all levels to a file
              s1:=ToolsForm.Game.FileName;
              if IsAnIniFileSectionFileName(s1) then s1:=ExtractIniFileName(s1);
              s1:=ExtractFilePath(s1);
              s2:=StrSubstituteCharacters(ToolsForm.Game.Name,FILE_NAME_AND_PATH_NAME_ILLEGAL_CHARACTERS,SPACE);
              if IsBlank(s2) then begin
                 ToolsForm.Game.SetName(TEXT_LEVEL);
                 s2:=TEXT_LEVEL;
                 end
              else if not StrEqual(s2,ToolsForm.Game.Name) then
                      ToolsForm.Game.Notes.Lines.WriteString(KEY_TITLE,ToolsForm.Game.Name); // keep the unfiltered title

              ToolsForm.Game.FileName:=StrWithTrailingPathDelimiter(s1)+s2+SOKOBAN_FILE_NAME_EXT;
              ToolsForm.Game.FileName:=Misc_.MakeNewFileName(ToolsForm.Game.FileName,SOKOBAN_FILE_NAME_EXT,True);
              end;
           if ToolsForm.Game.SaveToFile(ToolsForm.Game.FileName,True) then begin
              IsLoading:=True;
              try     DriveComboBox1.Update; DirectoryListBox1.Update; FileListBox1.Update;
              finally IsLoading:=False;
              end;
              TryToLoadFile(ToolsForm.Game.FileName);
              end;
           end
        else if Modified then begin
                TryToLoadFile(Game.FileName); // new solutions and/or snapshots may have been imported
                MenuItemOpenCopy.Enabled:=False;
                BtnOpen2.Enabled:=False; BitBtnOpen.Enabled:=False; BtnCancel.Enabled:=False;
                end;
        end
     else Msg(TEXT_CLIPBOARD_NO_LEVEL_TEXT,
              Application.Title+SUB_TITLE_SEPARATOR+TEXT_OPEN_LEVEL_FROM_CLIPBOARD,
              MB_OK+MB_ICONINFORMATION);
end;

procedure TOpenForm.MenuItemCreateLevelsUsingSolutionsFromClipboardClick(
  Sender: TObject);
var oMakeBoardFromMovesEnabled:Boolean;
begin
  if (ToolsForm.Game<>nil) and Assigned(ToolsForm.Game.SokoFile) then with ToolsForm.Game.SokoFile do begin
     oMakeBoardFromMovesEnabled:=MakeBoardFromMovesEnabled;
     try     MakeBoardFromMovesEnabled:=True;
             MenuItemImportFromClipboardClick(Sender);
     finally MakeBoardFromMovesEnabled:=oMakeBoardFromMovesEnabled;
     end;
     end;
end;

function  TOpenForm.LoadTextFromFile(const FileName__:String; TextType__:TTextType):Boolean;
var oShowErrorMessages:TShowErrorMessages; oCurrentFileName,Text:String;
    FileTime:TFileTime; t:TTextType;
begin
  Result:=False; oShowErrorMessages:=ShowErrorMessages;
  if FileName__<>'' then with Texts[TextType__] do
     if AnsiCompareText(FileName__,FileName)=0 then Result:=True
     else
       if CloseText(TextType__) then
          try
            ShowErrorMessages:=semNone; ClearText(TextType__);
            try
                   for t:=Low(t) to High(t) do
                       if (t<>TextType__) and (not Result) and
                          (AnsiCompareText(FileName__,Texts[t].FileName)=0) then begin
                          Memo.Text    :=Texts[t].Memo.Text;
                          Memo.Modified:=Texts[t].Memo.Modified;
                          Texts[t].Memo.Modified:=False;
                          FileName:=FileName__;
                          Result:=True;

                          if (TextType__=tSkinScript) or (t=tSkinScript) then begin
                             SkinScriptPanel.Visible:=False;
                             Texts[tTextFile].Memo.Modified:=Texts[tTextFile].Memo.Modified or Texts[tSkinScript].Memo.Modified;
                             Texts[tSkinScript].Memo.Modified:=False;
                             end;
                          end;

                   if  (not Result) and
                       FileExists(FileName__) and (FileSize(FileName__)<=MAX_SCRIPT_FILE_SIZE) and
                       SokUtil_.LoadTextFromFile(Text,FileTime,FileName__) and IsText(Text) then begin
                       if AnsiPos(NL,Text)=0 then
                          // there are no CR+LF line breaks;
                          // the 'Memo.Text := Text' assignment is supposed to
                          // adjust line breaks automatically (e.g., changing
                          // single carriage returns and line feeds to CR+NL),
                          // but it seems to fail sometimes, hence do it before
                          // the text is assigned to the memo;
                          Text:=SysUtils.AdjustLineBreaks(Text);
                       Memo.Text:=Text;
                       Memo.Modified:=False;
                       FileName:=FileName__;
                       Result:=True;
                       if TextType__=tSkinScript then SkinScriptPanel.Visible:=True;
                       end;

                   if  Result and (TextType__=tSkinScript) then begin
                       oCurrentFileName:=CurrentFileName;

                       SetFileFilter(MainForm.Skins.FileFilter);
                       //if   (CurrentFileName<>'') and FileExists(CurrentFileName) then
                       //     SetFileFilterIndex(CurrentFileName)
                       //else
                              FilterComboBox1.ItemIndex:=0;

                       if (oCurrentFileName<>CurrentFileName) and
                          FileExists(oCurrentFileName) and
                          (FileListBox1.Items.IndexOf(ExtractFileName(oCurrentFileName))>=0) then
                          TryToLoadFile(oCurrentFileName);
                       end;
            except on E:Exception do Result:=Error(E.Message,'');
            end
          finally
            ShowErrorMessages:=oShowErrorMessages;
            if not Result then ClearText(TextType__);
          end;
end;

procedure TOpenForm.BtnSkinScriptBrowseClick(Sender: TObject);
var oItemIndex:Integer; Directory,SkinTypeFileName:String;
begin
  with OpenDialog1 do begin
    Title:=Self.Caption+SUB_TITLE_SEPARATOR+SkinTypesText;
    if   SkinScriptsComboBox.ItemIndex>=0 then
         SkinTypeFileName:=MainForm.Skins.Scripts[SkinScriptsComboBox.ItemIndex,SKIN_SCRIPT_INDEX]
    else SkinTypeFileName:='';
    Directory:=StrWithoutTrailingPathDelimiter(ExtractFilePath(SkinTypeFileName));
    if (Directory='') or (not DirectoryExists(Directory)) then
       Directory:=StrWithoutTrailingPathDelimiter(MainForm.Skins.ScriptsPath);
    if (Directory='') or (not DirectoryExists(Directory)) then
       Directory:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
    InitialDir:=Directory; FileName:='';
    Filter:=SkinScriptsText+'  (*'+SKIN_SCRIPT_FILE_EXT+')|*'+SKIN_SCRIPT_FILE_EXT
                           +BAR+AllFilesFilterText;
    FilterIndex:=FilterIndexSkinType;

    if Execute then begin
       oItemIndex:=SkinScriptsComboBox.ItemIndex;
       FilterIndexSkinType:=FilterIndex;
       MainForm.Skins.AddScript(FileName); // adds the script to the list without validating it
       SkinScriptsComboBoxChange(Self);    // loads and validates the script, and deletes it from the list again in case there is a problem
       if MainForm.Skins.IndexOfScript(FileName)<0 then begin // loading the new file as a skin file script failed; try to restore the previous item
          SkinScriptsComboBox.ItemIndex:=oItemIndex;
          SkinScriptsComboBoxChange(Self);
          end;
       if not (BtnOK.Enabled and (CurrentFileName<>'')) then
          TryToLoadFile(CurrentFileName);
       end;
    end;
end;

procedure TOpenForm.ClearText(TextType__:TTextType);
begin
  with Texts[TextType__] do begin
    FileName:=''; Memo.Clear; Memo.Modified:=False;
    end;
end;

function  TOpenForm.CloseText(TextType__:TTextType):Boolean;
begin
  Result:=FlushText(TextType__);
  if Result then ClearText(TextType__);
end;

function  TOpenForm.FlushText(TextType__:TTextType):Boolean;
begin
  with Texts[TextType__] do
    if   FileName='' then Result:=True
    else try    if   (TextType__=tTextFile) and
                     (AnsiCompareText(FileName,Texts[tSkinScript].FileName)=0) and
                     (not SkinScriptPanel.Visible) then begin
                     Texts[tSkinScript].Memo.Text:=Memo.Text;
                     Texts[tSkinScript].Memo.Modified:=Memo.Modified;
                     SkinScriptPanel.Visible:=True;
                     ClearText(TextType__);
                     end
                else if Memo.Modified then begin
                        Memo.Lines.SaveToFile(FileName); // save the file to disk
                        Memo.Modified:=False;
                        end;
                Result:=Memo.Modified=False;
         except on E:Exception do
                   Result:=Error(Format(SaveFileFailedText__,[FileName])+NL+NL+LEFT_PAREN+E.Message+RIGHT_PAREN,Application.Title);
         end;
end;

procedure TOpenForm.SkinsComboBoxEnter(Sender: TObject);
var i:Integer;
begin
  for i:=Pred(SkinsComboBox.Items.Count) downto 0 do with MainForm.Skins do
      if FileExists(Skins[i,SKIN_NAME_INDEX]) and
         FileExists(Skins[i,SKIN_SCRIPT_INDEX]) then //
      else with SkinsComboBox do begin
         if Items.Objects[i]<>nil then SokUtil_.TNode(Items.Objects[i]).Free;
         Items.Delete(i);
         if i<OptionsForm.MenuItemRecentSkins.Count then
            OptionsForm.MenuItemRecentSkins.Items[i].Free;
         end;
end;

procedure TOpenForm.SkinScriptsComboBoxChange(Sender: TObject);
var ErrorMessage:String;
begin
  if   SkinScriptsComboBox.ItemIndex>=0 then with MainForm.Skins do
       if LoadTextFromFile(Scripts[SkinScriptsComboBox.ItemIndex,SKIN_SCRIPT_INDEX],tSkinScript,ErrorMessage) then begin
          //StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=Scripts[SkinScriptsComboBox.ItemIndex,SKIN_SCRIPT_INDEX];
          if (not (BtnOK.Enabled and (CurrentFileName<>''))) and (not IsLoading) then
             TryToLoadFile(CurrentFileName);
          end
       else with SkinScriptsComboBox do begin
          DeleteScript(ItemIndex);
          if   Items.Count=0 then Clear
          else ItemIndex:=-1;
          ClearText(tSkinScript);
          Error(ErrorMessage,Application.Title+SUB_TITLE_SEPARATOR+SkinsText);
          end
  else CloseText(tSkinScript);
end;

procedure TOpenForm.SkinScriptsComboBoxEnter(Sender: TObject);
var i:Integer;
begin
  for i:=Pred(SkinScriptsComboBox.Items.Count) downto 0 do with MainForm.Skins do begin
      if        FileExists(Scripts[i,SKIN_SCRIPT_INDEX]) then // ok
      else if   AnsiCompareText(Scripts[i,SKIN_SCRIPT_INDEX],Texts[tSkinScript].FileName)=0 then
                Texts[tSkinScript].Memo.Modified:=True // file disappeared: make sure that 'FlushText' saves it
           else DeleteScript(i);
      end;
  if  SkinScriptsComboBox.Items.Count=0 then SkinScriptsComboBox.Clear; // necessary to clear text
  if  SkinScriptsComboBox.ItemIndex  <0 then CloseText(tSkinScript);
end;

procedure TOpenForm.MenuItemNewTextFileClick(Sender: TObject);
var s:String;
begin
  if ((Task<>otGame) or CloseFile) and
     FileForm.Perform(ftNewTextFile,DiskCurrentItemType,MakeNewFileName(StrWithTrailingPathDelimiter(FileListBox1.Directory)+TextText,TEXT_FILE_EXT,True),'',s,False,False,'','') then begin
     UpdateAfterFileManagerTask('');
     if (Task=otImage) and (SubTask=osSkin) and CloseText(tTextFile) and
        LoadTextFromFile(s,tTextFile) then with Texts[tTextFile] do begin
        SetFileFilterIndex(FileName);
        TryToLoadFile(s);
        if StrEqual(FileName,s) then begin
           Memo.ReadOnly:=False;
           Memo.SetFocus;
           end;
        end;
     end;
end;

procedure TOpenForm.FileListBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var i:Integer; s:String;
begin
  ControlMouseMove(nil,Shift,X,Y);
  with FileListBox1 do begin
    if (Items.Count>1) and MenuItemAutoScroll.Checked then begin
       i:=ItemAtPos(Point(X,Y),False);
       if      (i=TopIndex) and (TopIndex<>0) and (Y<ItemHeight div 2) then begin
               TopIndex:=TopIndex-1;
               end
       else if (i>=Pred(TopIndex+FileListBox1VisibleRowCount)) and
               (TopIndex+FileListBox1VisibleRowCount<Items.Count) and
//             (Y>ItemHeight*FileListBox1VisibleRowCount-(ItemHeight div 2)) then begin
               (Y>ClientHeight-(ItemHeight div 2)) then begin
               TopIndex:=TopIndex+1;
               end;
       end;

    if ToolTips.Enabled then
       if Items.Count>=1 then begin
          i:=ItemAtPos(Point(X,Y),True);
          if (i>=0) and (X<Tag) then with PanelToolTips do begin
             s:=StrWithQuotedAmpersands(Items[i]);
             if Caption<>s then begin
                if Visible then Hide;
                Caption:=s;
                Width:=Canvas.TextWidth(Caption)+8;
                end;
             Left:=Min(DiskGroupBox.Left+FileListBox1.Left+X+ToolTips.OffsetX,Self.ClientWidth-Width-DiskGroupBox.Left);
             Top :=DiskGroupBox.Top +FileListBox1.Top +Y+ToolTips.OffsetY;
             if not PanelToolTips.Visible then PanelToolTips.Show;
             end
          else if PanelToolTips.Visible then PanelToolTips.Hide;
          end
       else if PanelToolTips.Visible then PanelToolTips.Hide;
    end;
end;

procedure TOpenForm.DirectoryListBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var i:Integer; s:String;
begin
  ControlMouseMove(nil,Shift,X,Y);
  with DirectoryListBox1 do begin
    if (Items.Count>1) and MenuItemAutoScroll.Checked then begin
       i:=ItemAtPos(Point(X,Y),False);
       if      (i=TopIndex) and (TopIndex<>0) and (Y<ItemHeight div 2) then begin
               TopIndex:=TopIndex-1;
               end
       else if (i=Pred(TopIndex+DirectoryListBox1VisibleRowCount)) and
               (TopIndex+DirectoryListBox1VisibleRowCount<Items.Count) and
               (Y>ItemHeight*DirectoryListBox1VisibleRowCount-(ItemHeight div 2)) then begin
               TopIndex:=TopIndex+1;
               end;
       end;

    if ToolTips.Enabled then
       if (Items.Count>=1) then begin
          i:=ItemAtPos(Point(X,Y),True);
          if (i>=0) and (X<Tag) then with PanelToolTips do begin
             s:=StrWithQuotedAmpersands(Items[i]);
             if Caption<>s then begin
                if Visible then Hide;
                Caption:=s;
                Width:=Canvas.TextWidth(Caption)+8;
                end;
             Left:=Min(DiskGroupBox.Left+DirectoryListBox1.Left+X+ToolTips.OffsetX,Self.ClientWidth-Width-DiskGroupBox.Left);
             Top :=DiskGroupBox.Top +DirectoryListBox1.Top +Y+ToolTips.OffsetY;
             if not PanelToolTips.Visible then PanelToolTips.Show;
             end
          else if PanelToolTips.Visible then PanelToolTips.Hide;
          end
       else if PanelToolTips.Visible then PanelToolTips.Hide;
    end;
end;

procedure TOpenForm.CollectionStringGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var ACol,ARow:Integer; s:String;
begin
  if   ToolTips.Enabled and (CollectionStringGrid.RowCount>=1) then
       ControlMouseMove(nil   ,Shift,X,Y)
  else ControlMouseMove(Sender,Shift,X,Y);
  with CollectionStringGrid do begin
    if (RowCount>1) and MenuItemAutoScroll.Checked then begin
       MouseToCell(X,Y,ACol,ARow);
       if      (ARow=TopRow) and (TopRow<>0) and (Y<DefaultRowHeight div 2) then begin
               TopRow:=TopRow-1;
               end
       else if (ARow=Pred(TopRow+VisibleRowCount)) and
               (TopRow+VisibleRowCount<RowCount) and
               (Y>DefaultRowHeight*VisibleRowCount-(DefaultRowHeight div 2)) then begin
               TopRow:=TopRow+1;
               end;
       end;

    if ToolTips.Enabled then
       if (RowCount>=1) then begin
          MouseToCell(X,Y,ACol,ARow);
          if (ACol=1) and (ARow<RowCount) and (X<Tag) then with PanelToolTips do begin
             s:=StrWithQuotedAmpersands(Cells[ACol,ARow]);
             if Caption<>s then begin
                if Visible then Hide;
                Caption:=s;
                Width:=Self.Canvas.TextWidth(Caption)+8;
                end;
             Left:=Min(CollectionGroupBox.Left+CollectionStringGrid.Left+X+ToolTips.OffsetX,Self.ClientWidth-Width-CollectionGroupBox.Left);
             Top :=CollectionGroupBox.Top +CollectionStringGrid.Top +Y+ToolTips.OffsetY;
             if not PanelToolTips.Visible then PanelToolTips.Show;
             end
          else if PanelToolTips.Visible then PanelToolTips.Hide;
          end
       else if PanelToolTips.Visible then PanelToolTips.Hide
    else if PanelToolTips.Visible then PanelToolTips.Hide;
    end;
end;

procedure TOpenForm.FileListBox1Click(Sender: TObject);
begin
  with FileListBox1 do
    if (ItemIndex=0) then begin
       // Window XP work-around: setting ItemIndex := 0 does not
       // necessarily means a change of state because a
       // previously attempt to set ItemIndex := -1 may have been discarded
       //Msg('Clicked '+StrWithTrailingPathDelimiter(Directory)+Items[0],'',MB_OK);
       TryToLoadFile(StrWithTrailingPathDelimiter(Directory)+Items[0]);
       end;
end;

function TOpenForm.FileListBox1FileName:String;
begin
// Windows XP work-around: setting ItemIndex := -1 doesn't work,
// hence, the ItemIndex is 0 for a non-empty list when it should be -1;
// this work-around assumes that item[0] is selected if itemindex is 0 and
// filename=''
  with FileListBox1 do
    if        FileName<>'' then Result:=FileName
    else if   (Items.Count<>0) and
              (ItemIndex=0) then Result:=StrWithTrailingPathDelimiter(Directory)+Items[0]
         else Result:='';
end;

procedure TOpenForm.MenuItemFileReorganizeClick(Sender: TObject);
var i,FirstLineNo,LastLineNo,MajorVersionNo,MinorVersionNo:Integer;
    s,DateCreated,DateOfLastChange:String; oCursor:TCursor; Node:TNode; Level:TLevel;
begin
  ToolsForm.Game.Clear;
  oCursor:=Screen.Cursor;
  if (Task=otGame) and (CurrentFileName<>'') and
     CloseFile and (Game<>nil) and
     (Game.SokoFile<>nil) and
     (Game.SokoFile.Levels.Count<>0) then
     try     Screen.Cursor:=crHourGlass;
             StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=ReorganizingFileText+SPACE+PleaseWaitText;

             if   MainForm.AddFileFormatDescriptionToFiles then
                  Game.SokoFile.UpdateFileFormatDescription(True)
             else if   Game.SokoFile.HasFileFormatDescription(FirstLineNo,LastLineNo,MajorVersionNo,MinorVersionNo) then
                       with Game.SokoFile.FileHeader.Lines do  begin
                         DateCreated     :=''; ReadString(KEY_DATE_CREATED    ,DateCreated);
                         DateOfLastChange:=''; ReadString(KEY_DATE_LAST_CHANGE,DateOfLastChange);

                         for i:=FirstLineNo to LastLineNo do
                             Remove(GetItemByIndex(Pred(FirstLineNo)),True); {'Pred': 'FirstLineNo'  is 1-based, change it to the 0-based number}

                         if (DateCreated     <>'') and FindKey(KEY_DATE_CREATED    ,Node) then Remove(Node,True);
                         if (DateOfLastChange<>'') and FindKey(KEY_DATE_LAST_CHANGE,Node) then Remove(Node,True);

                         if (DateCreated<>'') or (DateOfLastChange<>'') then begin
                            AddBlankLine;
                            if DateCreated     <>'' then WriteString(KEY_DATE_CREATED    ,DateCreated);
                            if DateOfLastChange<>'' then WriteString(KEY_DATE_LAST_CHANGE,DateOfLastChange);
                            end;

                         Game.SokoFile.FileHeader.Modified:=True;
                         Game.SokoFile.Modified:=True;
                         end;

             Level:=TLevel(Game.SokoFile.Levels.First);
             while Level<>nil do with Level do begin
               Node:=Next;
               StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=Level.Name;
               StatusBar1.Repaint;
               if   ToolsForm.Game.LoadFromFile(Level.Name,Game.SokoFile,True,s) then
                    TSokoGame(ToolsForm.Game).SaveToFile(Game.SokoFile,True,True,True,True,False)
               else Error(s,'');
               Level:=TLevel(Node);
               end;
     finally Screen.Cursor:=oCursor;
             ToolsForm.Game.Clear;
             StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:='';
     end;
end;

function TOpenForm.StrSubstituteInFiles(const Path__,Mask__,IniFileSection__:String; const OldStrings__,NewStrings__:array of String):Integer;
var i,j,k:Integer; s:String; L:SokUtil_.TList; oCursor:TCursor;
begin
  Result:=0; oCursor:=Screen.Cursor;
  with OpenForm.FileListBox1 do begin
    Directory:=Path__;
    Mask:=Mask__;
    Update;
    try L:=SokUtil_.TList.Create;
        try
          Screen.Cursor:=crHourGlass;
          for i:=0 to Pred(Items.Count) do begin
              s:=StrWithTrailingPathDelimiter(Directory)+Items[i];
              if FileExists(s) and
                 L.LoadFromFile(s) then begin
                 k:=0;
                 for j:=Max(Low (OldStrings__),Low (NewStrings__)) to
                        Min(High(OldStrings__),High(NewStrings__)) do
                     Inc(k,L.StrSubstitute(OldStrings__[j],NewStrings__[j],IniFileSection__,True));
                 if  k<>0 then
                     if   L.SaveToFile(s) then Inc(Result,k)
                     else raise Exception.Create(TEXT_WRITE_FILE_ERROR);
                 end;
              end;
         finally Screen.Cursor:=oCursor;
                 L.Free;
         end;
     except on E:Exception do begin
            Error(E.Message,Application.Title);
            Result:=-1;
            end;
     end;
     end;
end;

procedure TOpenForm.UpdateCollectionHintText;
var s:String;
begin
  if   (Task=otGame) and (CollectionStringGrid.RowCount>1) then begin
       s:=Format(LevelCountText__,[Game.SokoFile.Levels.Count]);
       if CurrentCollectionCheckedLevelsCountDown=0 then
          s:=s+COMMA+SPACE+Format(SolvedCountText__,[CurrentCollectionSolvedLevelsCount]);
       CollectionFileNamePanel.Hint:=LevelCollectionStatsText+s;
       end
  else CollectionFileNamePanel.Hint:='';
end;

function TOpenForm.GetSubDirectories(const Path__,AbbreviatedPath__:String; SubDirectoryList__:SokUtil_.TList):Boolean;
var i,PathLength:Integer; oDrive:Char; oVisible:Boolean; oDirectory,s,s1:String;
    Node:SokUtil_.TNode;
begin // note that the function modifies the directory box and the file box
  s:=StrWithoutTrailingPathDelimiter(Path__);
  PathLength:=Length(s);
  Result:=PathLength<>0;

  if Result then with DirectoryListBox1 do
     try
       oDirectory:=Directory; oDrive:=Drive; oVisible:=Visible;
       try     Visible:=False;
               Drive:=UpCase(s[1]);
               Directory:=s;

               for i:=0 to Pred(Items.Count) do
                   if Result then begin
                      s:=GetItemPath(i);
                      if   AbbreviatedPath__='' then
                           s1:=s
                      else s1:=AbbreviatedFilePath(s,AbbreviatedPath__);
                      if (Length(s)>PathLength) and
                         (s[Succ(PathLength)]=FILE_NAME_PATH_DELIMITER) and
                         StrEqual(Directory,Copy(s,1,PathLength)) and
                         (SubDirectoryList__.GetItemByName(s1)=nil) then begin
                         if   CreateObject(otNode,Node) then begin
                              SubDirectoryList__.Add(Node);
                              Result:=Node.SetText(s1);
                              end
                         else Result:=False;
                         end
                      end
                   else break;

               if not Result then Error(TEXT_TASK_FAILED,Application.Title+' OpenForm.GetSubDirectories');

       finally Drive:=oDrive; Directory:=oDirectory; Visible:=oVisible;
               DirectoryListBox1.Update; FileListBox1.Update;
       end;
     except on E:Exception do Result:=Error(E.Message,Application.Title);
     end;
end;

procedure TOpenForm.FilterComboBox1Change(Sender: TObject);
begin
  if (Screen.ActiveForm=Self) and (Task=otImage) and (SubTask=osSkin) then
     SkinTypeDetection(DirectoryListBox1.Directory);
end;

procedure TOpenForm.MenuItemFileConversionClick(Sender: TObject);
const
    MAX_SOLUTION_FILE_SIZE            = 4*1024*1024; // arbitrary limit, but big files are probably not solution files, so don't try to load them
//  SOKOBAN_FOR_WINDOWS_LEVEL_FILE_NAME_EXT
//                                    = '.slc';
    SOKOBAN_FOR_WINDOWS_SOLUTION_FILE_NAME_EXT
                                      = '.ssc';
    SOKOFAN_SOLUTION_FILE_NAME_EXT    = '.sol';
    SOKOMIND_COMMENT_BEGIN            = 'Comment:';
    SOKOMIND_COMMENT_END              = 'Comment-end:';
    SOKOMIND_SOLUTION_FILE_NAME_EXT   = '.xht';
    SOKOMIND_SOLUTION_DIRECTORY_PREFIX= 'sol';
var i:Integer; Result:Boolean;
    s,s1,CurrentFileName:String; // note that 'CurrentFileName' deliberately shadows 'Self.CurrentFileName'
    oCursor:TCursor;
    SortedFileNames:TStringList;
    Node:TNode; Level:TLevel;
    TextLines,SubDirectoryList,DeleteFileList:SokUtil_.TList;
    CombinedSokoFile,SokoFile:SokFile_.TSokoFile;

  function LoadSokobanForWindowsLevels(const FileName__:String):Boolean;
  const
       TAG_COPYRIGHT='Copyright="';
       TAG_BOARD_LINE='<L>';
       TAG_LEVEL_NAME='<Level Id="';
       TAG_LEVELS='<LevelCollection';
       TAG_COLLECTION='<SokobanLevels>';
  type TState=(sFileHeader,sCollection,sLevel,sBoard);
  var  i:Integer; s,LevelCopyright,LevelName,LevelSetCopyright:String;
       State:TState; Node,BoardLineNode:TNode; Level:TLevel;

    procedure LevelPostProcessing(var Level__:TLevel);
    begin
      if Level__<>nil then
         try     Level__.BoardAsTextLines.Reverse;
         finally Level__:=nil; // clear the variable; this doesn't destroy the level
         end;
    end;

  begin // LoadSokobanForWindowsLevels
    SokoFile.Clear;
    Result:=TextLines.LoadFromFile(FileName__);
    if Result then begin
       State:=sFileHeader; Level:=nil; LevelSetCopyright:='';
       while Result and (not TextLines.IsEmpty) do begin
         Node:=TextLines.Pop; s:=Node.Text;

         case State of
           sFileHeader : begin
                         if      AnsiPos(TAG_COLLECTION,s)<>0 then State:=sCollection
                         else if AnsiPos(TAG_LEVELS    ,s)<>0 then State:=sLevel;
                         end;
           sCollection : begin
                         if   AnsiPos(TAG_LEVELS,s)<>0 then begin
                              State:=sLevel;
                              i:=AnsiPos(TAG_COPYRIGHT,s);
                              if i<>0 then begin
                                 Delete(s,1,i+Pred(Length(TAG_COPYRIGHT)));
                                 i:=AnsiPos(DOUBLE_QUOTE,s);
                                 if i<>0 then LevelSetCopyRight:=Copy(s,1,Pred(i));

                                 if (LevelSetCopyright<>'') and
                                    Node.SetText(KEY_AUTHOR+': '+LevelSetCopyright) then begin // 'Author' is not the same thing as 'Copyright' but 'Author' gets special treatment by the common file-format, so it will have to do
                                    SokoFile.FileHeader.Lines.Add(Node); Node:=nil;
                                    end;

                                 end;
                              end
                         else begin
                           SokoFile.FileHeader.Lines.Add(Node); Node:=nil;
                           end;
                         end;
           sLevel      : begin
                           i:=AnsiPos(TAG_LEVEL_NAME,s);
                           if i<>0 then begin
                              LevelName:=''; LevelCopyright:='';

                              Delete(s,1,i+Pred(Length(TAG_LEVEL_NAME)));
                              i:=AnsiPos(DOUBLE_QUOTE,s);
                              if i<>0 then LevelName:=Copy(s,1,Pred(i));

                              i:=AnsiPos(TAG_COPYRIGHT,s);
                              if i<>0 then begin
                                 Delete(s,1,i+Pred(Length(TAG_COPYRIGHT)));
                                 i:=AnsiPos(DOUBLE_QUOTE,s);
                                 if i<>0 then LevelCopyRight:=Copy(s,1,Pred(i));
                                 end;
                              //if LevelCopyRight='' then LevelCopyRight:=LevelSetCopyright;

                              if LevelName='' then LevelName:=TEXT_LEVEL;
                              Result:=CreateObject(otLevel,TNode(Level));
                              if Result then begin
                                 Result:=Level.SetName(LevelName);
                                 if Result then begin
                                    SokoFile.Levels.Push(TNode(Level));
                                    State:=sBoard;

                                    if (LevelCopyright<>'') and
                                       Node.SetText(KEY_AUTHOR+': '+LevelCopyright) then begin // 'Author' is not the same thing as 'Copyright' but 'Author' gets special treatment by the common file-format, so it will have to do
                                       Level.Notes.Lines.Push(Node); Node:=nil;
                                       end;
                                    end
                                 else begin
                                    Level.Free; Level:=nil;
                                    end;
                                 end;
                              end;
                         end;
           sBoard      : begin
                           i:=AnsiPos(TAG_BOARD_LINE,s);
                           if i<>0 then begin
                              s:=Copy(s,i+Length(TAG_BOARD_LINE),MaxInt);
                              i:=AnsiPos('<',s);
                              if i<>0 then Delete(s,i,MaxInt);
                              if s<>'' then begin
                                 Result:=CreateObject(otNode,BoardLineNode);
                                 if Result then begin
                                    Level.BoardAsTextLines.Push(BoardLineNode);
                                    Result:=BoardLineNode.SetText(s);
                                    end;
                                 end;
                              end
                           else
                              if s<>'' then begin
                                 LevelPostProcessing(Level);
                                 State:=sLevel;
                                 end;
                         end;
         end; // case

         if Node<>nil then Node.Free;
         end;

       LevelPostProcessing(Level);
       SokoFile.Levels.Reverse;
       end;
  end; // LoadSokobanForWindowsLevels

  function LoadSokobanForWindowsSolutions(Levels__:TList; const Name__:String):Boolean;
  const
      TAG_LEVEL='<ScoreLevel Id="';
      TAG_SCORE='<Score';
      TAG_SOKOBAN_SCORES='<SokobanScores';
      TAG_VERSION='Version';
  var i,Len,MoveLengthBytes,Version:Integer;
      s,SolutionAsText,SolutionFileName:String;
      Level:TLevel;

    procedure SolutionPostProcessing(Level__:TLevel; var SolutionAsText__:String);
    const
        DIRECTION_TO_CHAR:array[0..3] of Char=('u','r','d','l');
    var i,BufferSize,Byte,MoveCount:Integer; p,Buffer:PByte;
        Node:TNode; SnapshotAsText:TSnapshotAsText;
    begin
      try
        if (Level__<>nil) and
           (SolutionAsText__<>'') and
           Base64Decode(SolutionAsText__,Buffer,BufferSize,'') then
           try
                   if BufferSize>MoveLengthBytes then begin
                      p:=Buffer; Byte:=0;
                      // the first 2 or 4 bytes contain the number of moves
                      MoveCount:=0;
                      for i:=Pred(MoveLengthBytes) downto 0 do begin
                          MoveCount:=(MoveCount shl 8) + p^;
                          if i<>0 then Inc(p); // 'True': advance to the next byte
                          end;
                      if (MoveCount>0) and
                         (BufferSize-MoveLengthBytes>=MoveCount div 4) then begin // 'div 4': one move = 2 bits, i.e., there are 4 moves in each byte
                         if Length(SolutionAsText)<MoveCount then
                            SetLength(SolutionAsText,MoveCount);
                         for i:=1 to Length(SolutionAsText) do SolutionAsText[i]:=SPACE;

                         for i:=0 to Pred(MoveCount) do begin
                             if (i mod 4)=0 then begin // 'True': advance to the next byte in the input buffer
                                Inc(p); Byte:=p^;
                                end;
                             SolutionAsText[Succ(i)]:=DIRECTION_TO_CHAR[(Byte and $c0) shr 6];
                             Byte:=Byte shl 2;
                             end;

                         if CreateObject(otSnapshotAsText,TNode(SnapshotAsText)) then begin
                            Level__.SnapshotsAsText.Add(SnapshotAsText);
                            SnapshotAsText.SetText(SnapshotsForm.SolutionName);
                            if CreateObject(otNode,Node) then begin
                               SnapshotAsText.MovesAsTextLines.Add(Node);
                               Node.SetText(SolutionAsText);
                               end;
                            end;
                         end;

                      end;
           finally FreeMem(Buffer);
           end;
      finally SolutionAsText__:=''; // clear the accumulating solution-string
      end;
    end; // SolutionPostProcessing

  begin // LoadSokobanForWindowsSolutions
    Result:=True; Level:=nil; SolutionAsText:=''; MoveLengthBytes:=4;

    SolutionFileName:=ExpandFileName(
                       StrWithTrailingPathDelimiter(ExtractFilePath(Name__))+
                       '..\Scores\'+
                       ExtractFileNameWithoutPathAndExtension(Name__)+
                       SOKOBAN_FOR_WINDOWS_SOLUTION_FILE_NAME_EXT);
    if not FileExists(SolutionFileName) then
       SolutionFileName:=ExpandFileName(
                       StrWithTrailingPathDelimiter(ExtractFilePath(Name__))+
                       ExtractFileNameWithoutPathAndExtension(Name__)+
                       SOKOBAN_FOR_WINDOWS_SOLUTION_FILE_NAME_EXT);

    if FileExists(SolutionFileName) and
       (FileSize(SolutionFileName)<=MAX_SOLUTION_FILE_SIZE) then begin
       Result:=TextLines.LoadFromFile(SolutionFileName);
       if Result then begin
          while Result and (not TextLines.IsEmpty) do begin
            s:=TextLines.First.Text; // get a copy of the next line before it's destroyed
            TextLines.Pop.Free;

            i:=AnsiPos(TAG_SOKOBAN_SCORES,s);
            if i<>0 then begin
               // find the 'scores' version number;
               // version number <= 1 : move count is a 2 byte number
               // version number >= 2 : move count is a 4 byte number
               MoveLengthBytes:=2; // move length defaults to old value, i.e., 2 bytes
               i:=AnsiPos(TAG_VERSION,s);
               if i<>0 then begin // 'True': got 'Version'
                  Len:=Length(s);
                  if Len<High(Len) then begin // 'True': no risk of overflowing the string character index
                     Inc(i,Length(TAG_VERSION));
                     while (i<=Len) and (s[i]<>EQUAL) do Inc(i);
                     if i<=Len then // 'True': got '='
                        repeat Inc(i);
                        until  (i>Len) or (s[i]=DOUBLE_QUOTE) or IsADigitChar(s[i]);
                     if (i<=Len) and (s[i]=DOUBLE_QUOTE) then Inc(i);
                     Version:=0;
                     while i<=Len do
                       if   IsADigitChar(s[i]) then begin
                            Version:=(Version*10)+Ord(s[i])-Ord('0');
                            Inc(i);
                            end
                       else i:=Succ(Len);
                     if Version>=2 then MoveLengthBytes:=4;
                     end;
                  end;
               end;

            i:=AnsiPos(TAG_LEVEL,s);
            if   i<>0 then begin // new level
                 SolutionPostProcessing(Level,SolutionAsText);

                 Delete(s,1,i+Pred(Length(TAG_LEVEL)));
                 i:=AnsiPos(DOUBLE_QUOTE,s);
                 if i<>0 then Delete(s,i,MaxInt);

                 Level:=SokoFile.GetLevelByName(s);
                 end
            else if Level<>nil then begin
                    i:=AnsiPos(TAG_SCORE,s);
                    if           i<>0 then
                                 SolutionPostProcessing(Level,SolutionAsText)
                    else if      IsABase64EncodedText(s,'') then
                                 SolutionAsText:=SolutionAsText+s // collect solution lines
                         else if SolutionAsText<>'' then SolutionPostProcessing(Level,SolutionAsText);
                    end;
            end;

          SolutionPostProcessing(Level,SolutionAsText);
          if Result and (Sender=nil) then DeleteFileList.AddTextLine(SolutionFileName,True);
          end;
       end;

  end; // LoadSokobanForWindowsSolutions

  function LoadSokofanSolutions(Levels__:TList; const Name__:String):Boolean;
  var i,LevelCount,SolutionCount:Integer; s1,s2,SolutionFileName:String;
      Node:TNode;
      Level:TLevel; SnapshotAsText:TSnapshotAsText; SolutionsSokoFile:TSokoFile;
  begin
    Result:=True; LevelCount:=Levels__.Count; SolutionCount:=0;
    SolutionFileName:=StrWithTrailingPathDelimiter(ExtractFilePath(Name__))+
                      ExtractFileNameWithoutPathAndExtension(Name__)+
                      UNDERSCORE+
                      Copy(ExtractFileExt(Name__),2,MaxInt)+
                      Sokofan_SOLUTION_FILE_NAME_EXT;

    if FileExists(SolutionFileName) and
       (FileSize(SolutionFileName)<=MAX_SOLUTION_FILE_SIZE) then begin
       Result:=TextLines.LoadFromFile(SolutionFileName);
       if Result then begin
          Node:=TextLines.First;
          while Node<>nil do begin
            // search for 'Level : <number>'
            if StrBeginsWith(Node.Text,TEXT_LEVEL) then begin
               i:=AnsiPos(COLON,Node.Text);
               if (i<>0) and
                  SafeStrToInt(Copy(Node.Text,Succ(i),MaxInt),False,i) and
                  (i>0) and (i<=LevelCount) then begin
                  Node.SetText(Levels__.GetItemByIndex(Pred(i)).Text);
                  Inc(SolutionCount);
                  end;
               end
            else
               if StrBeginsWith(Node.Text,'----') then Node.SetText('');
            Node:=Node.Next;
            end;

          if SolutionCount<>0 then begin
             if CreateObject(otSokoFile,TNode(SolutionsSokoFile)) then
                try     //TextLines.SaveToFile('c:\Temp\tx.txt');
                        SolutionsSokoFile.ParseSnapshotsWithoutBoardsAsIndividualLevels:=True;
                        Result:=SolutionsSokoFile.LoadFromTextLines(TextLines);
                        if Result then begin

                           //SolutionsSokoFile.SaveToFile('c:\Temp\tx.sok');
                           //SolutionsSokoFile.SetName('');

                           Result:=SolutionsSokoFile.MergeSokoFile(SokoFile,True,False,False);
                           if Result then begin
                              SwapNodes(TNode(SolutionsSokoFile),TNode(SokoFile)); // before: data is in 'SolutionsSokoFile'; after: data is back in 'SokoFile'

                              Level:=TLevel(SokoFile.Levels.First);
                              while Level<>nil do begin
                                SnapshotAsText:=TSnapshotAsText(Level.SnapshotsAsText.First);
                                while SnapshotAsText<>nil do begin
                                  s1:=''; s2:='';
                                  SnapshotAsText.Notes.Lines.ReadString(MovesText,s1);
                                  SnapshotAsText.Notes.Lines.ReadString(PushesText,s2);
                                  if (s1<>'') and (s2<>'') then
                                     SnapshotAsText.SetName(SnapshotsForm.SolutionName+SPACE+s1+SLASH+s2);
                                  SnapshotAsText:=TSnapshotAsText(SnapshotAsText.Next);
                                  end;
                                Level:=TLevel(Level.Next);
                                end;

                              end;
                           end;
                finally SolutionsSokoFile.Free;
                end;
             end;

          end;
       end;

  end; // LoadSokofanSolutions

  function LoadSokoMindSolutions(Level__:TLevel; const Name__:String):Boolean;
  var SolutionFileName:String; DirectoryNode:TNode;

    function LoadSolutionFile(const FileName__:String):Boolean;
    var MoveCount,PushCount:Integer; SolverName:String; Node:TNode; SnapshotAsText:TSnapshotAsText;

      procedure CountMovesAndPushes(const Text__:String; var MoveCount__,PushCount__:Integer);
      var i:Integer; Ch:Char;
      begin
        MoveCount__:=0; PushCount__:=0;
        for i:=1 to Length(Text__) do begin
            Ch:=Text__[i];
            if      (Ch=UP_CH1) or (Ch=DOWN_CH1) or (Ch=LEFT_CH1) or (Ch=RIGHT_CH1) then       // lowercase u,d,l,r
                    Inc(MoveCount__)
            else if (Ch=UP_CH ) or (Ch=DOWN_CH ) or (Ch=LEFT_CH ) or (Ch=RIGHT_CH ) then begin // uppercase U,L,D,R
                    Inc(MoveCount__); Inc(PushCount__);
                    end;
            end;
      end;

    begin // the return value 'True' doesn't mean that any solutions are found in the file, only that no errors occured
      Result:=True;
      if FileExists(FileName__) and
         (FileSize(FileName__)<=MAX_SOLUTION_FILE_SIZE) then begin
         if TextLines.LoadFromFile(FileName__) then begin
            while Result and (not TextLines.IsEmpty) do begin
              Node:=TextLines.Pop;
              if IsAMoveTextLine(Node.Text) then
                 if   Level__.SnapshotsAsText.IsEmpty
                      or
                      TSnapshotAsText(Level__.SnapshotsAsText.First).MovesAsTextLines.IsEmpty
                      or
                      (Node.Text<>TSnapshotAsText(Level__.SnapshotsAsText.First).MovesAsTextLines.First.Text) then
                      if   CreateObject(otSnapshotAsText,TNode(SnapshotAsText)) then begin
                           CountMovesAndPushes(Node.Text,MoveCount,PushCount);
                           SnapshotAsText.MovesAsTextLines.Add(Node); Node:=nil;
                           Level__.SnapshotsAsText.Add(SnapshotAsText);
                           SolverName:=ExtractFileName(StrWithoutTrailingPathDelimiter(ExtractFilePath(FileName__)));
                           if SolverName<>'' then
                              if   StrEqual(SolverName,SOKOMIND_SOLUTION_DIRECTORY_PREFIX) or (SolverName='') then
                                   SolverName:=''
                              else SolverName:=SPACE+StrWithParenthesis(SolverName);
                           Result:=SnapshotAsText.SetText(SnapshotsForm.SolutionName+SPACE+IntToStr(MoveCount)+SLASH+IntToStr(PushCount)+SolverName);
                           end
                      else Result:=False;
              if Node<>nil then Node.Free;
              end;
            if Result and (Sender=nil) and (not Level__.SnapshotsAsText.IsEmpty) then DeleteFileList.AddTextLine(FileName__,True);
            end
         else Result:=False;
         end;
    end;

  begin // LoadSokoMindSolutions
    SolutionFileName:=ExtractFileNameWithoutPathAndExtension(Name__)+SOKOMIND_SOLUTION_FILE_NAME_EXT;
    Result:=(Level__<>nil) and (Level__.SnapshotsAsText<>nil);
    Result:=Result and LoadSolutionFile(StrWithTrailingPathDelimiter(ExtractFilePath(Name__))+SolutionFileName);
    DirectoryNode:=SubDirectoryList.First;
    while Result and (DirectoryNode<>nil) do begin
      if (Sender=nil) or StrBeginsWith(ExtractFileName(DirectoryNode.Text),SOKOMIND_SOLUTION_DIRECTORY_PREFIX) then // 'ExtractFileName()' will in this case extract the last directory name
         Result:=LoadSolutionFile(StrWithTrailingPathDelimiter(DirectoryNode.Text)+SolutionFileName);
      DirectoryNode:=DirectoryNode.Next;
      end;
    if Result then Level__.SnapshotsAsText.Reverse;
  end; // LoadSokoMindSolutions

  procedure RemoveSokoMindCommentLineTags(Level__:TLevel);
  var Node,NextNode:TNode;
  begin
    Node:=Level__.Notes.Lines.First;
    while Node<>nil do begin
      NextNode:=Node.Next;
      if StrEqual(Node.Text,SOKOMIND_COMMENT_BEGIN) or
         StrEqual(Node.Text,SOKOMIND_COMMENT_END) then
         Level__.Notes.Lines.Remove(Node,True);
      Node:=NextNode;
      end;
  end;

  function NormalizeBoard( SokoFile__ : TSokoFile; Level__ : TLevel; MakeRectangularBoard__ : Boolean ) : Boolean;
  var BoardWidth, BoardHeight, BoxCount, GoalCount : Integer;
      PlayerPos : TColRow;
      ErrorText : String;
      Board : TBoard;
      History : THistory;
  begin
    Result := False;
    with Level__ do begin
      if TextLinesToBoard( Board, BoardWidth, BoardHeight, BoxCount, GoalCount, PlayerPos, ErrorText ) then begin
         Result := SokFile_.NormalizeBoard( SnapshotsAsText.IsEmpty, // 'IsEmpty': moving player and boxes could ruin existing snapshots and solutions
                                            True, True, True, True, BoardWidth, BoardHeight, Board, PlayerPos, History )<>0;
         if MakeRectangularBoard__ and ( MakeBoardRectangular( BoardWidth, BoardHeight, Board ) <> 0 ) then
            Result:=True;
         if Result and ( not BoardToTextLines( BoardWidth, BoardHeight, Board ) ) then begin
            SokoFile__.Clear; // drop all transactions
            Result := Error( TEXT_TASK_FAILED, Application.Title + SUB_TITLE_SEPARATOR + FileConversionText );
            end;
         end;
      end;
  end;

begin // FileConversion
  if CloseFile and (MPlayerForm<>nil) and (SnapshotsForm<>nil) then with MPlayerForm.OpenDialog1 do begin
     CurrentFileName:=Self.CurrentFileName; // note that 'CurrentFileName' deliberately shadows 'Self.CurrentFileName'

     Title:=FileConversionText;
     if        Sender=MenuItemFileConversionMergeLevelFiles then
               Title:=Title+SUB_TITLE_SEPARATOR+HintMergeFilesText
     else if   Sender=MenuItemFileConversionSokobanForWindows then
               Title:=Title+SUB_TITLE_SEPARATOR+HintSokobanForWindowsSelectFilesText
     else if   Sender=MenuItemFileConversionSokofan then
               Title:=Title+SUB_TITLE_SEPARATOR+HintSokofanSelectFilesText
     else if   (Sender=MenuItemFileConversionSokoMind) or (Sender=nil) then
               Title:=Title+SUB_TITLE_SEPARATOR+HintSokoMindSelectFilesText
     else if   (Sender=MenuItemFileConversionNormalizeBoard) or
               (Sender=MenuItemFileConversionNormalizeBoardMakeRectangularBoard) then
               Title:=Title+SUB_TITLE_SEPARATOR+HintNormalizeBoardSelectFilesText;
     if   FileListBox1.Directory<>'' then
          InitialDir:=StrWithoutTrailingPathDelimiter(FileListBox1.Directory)
     else InitialDir:= StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);

     Filter:=FilterComboBox1.Filter;
     if        Sender=MenuItemFileConversionSokobanForWindows then
               FilterIndex:=FilterComboBox1.Items.Count
     else if   Sender<>MenuItemFileConversionMergeLevelFiles then
               FilterIndex:=1
          else FilterIndex:=Succ(FilterComboBox1.ItemIndex);
     FileName:='';
     Options:=Options+[ofAllowMultiSelect,ofFileMustExist];

     if Sender<>nil then begin
        StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=GetLongHint(SelectMultipleFilesHint);
        Result:=Execute;
        StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:='';

        Result:=Result
                and
                ((Files.Count>=2)
                 or
                 ((Files.Count=1)
                  and
                  (Sender<>MenuItemFileConversionMergeLevelFiles)
                 )
                );
        end
     else with FileListBox1 do begin
        Result:=False; // merge 'SokoMind' files and 'Sokoban for Windows' in the 'Self.FileListBox1' directory
        for i:=0 to Pred(Items.Count) do
            if StrEqual(ExtractFileExt(Items[i]),XSB_FILE_NAME_EXT) or
               StrEqual(ExtractFileExt(Items[i]),SOKOBAN_FOR_WINDOWS_LEVEL_FILE_NAME_EXT) then begin
               Result:=True; break;
               end;
        end;

     if Result then begin
        SortedFileNames:=nil;
        CombinedSokoFile:=nil; SokoFile:=nil;
        TextLines:=nil; SubDirectoryList:=nil; DeleteFileList:=nil;
        oCursor:=Screen.Cursor;
        try
                Screen.Cursor:=crHourGlass;

                // sort selected files
                SortedFileNames:=TStringList.Create;
                SortedFileNames.Sorted:=True;
                if   Sender<>nil then
                     for i:=0 to Pred(Files.Count)              do SortedFileNames.Add(Files[i])
                else for i:=0 to Pred(FileListBox1.Items.Count) do
                         if StrEqual(ExtractFileExt(FileListBox1.Items[i]),XSB_FILE_NAME_EXT) or
                            StrEqual(ExtractFileExt(FileListBox1.Items[i]),SOKOBAN_FOR_WINDOWS_LEVEL_FILE_NAME_EXT) then
                            SortedFileNames.Add(StrWithTrailingPathDelimiter(FileListBox1.Directory)+FileListBox1.Items[i]);

                // create working storage objects
                Result:=CreateObject(otList,TNode(SubDirectoryList)) and
                        CreateObject(otSokoFile,TNode(CombinedSokoFile)) and
                        CreateObject(otSokoFile,TNode(SokoFile)) and
                        CreateObject(otList,TNode(TextLines)) and
                        CreateObject(otList,TNode(DeleteFileList)) and
                        GetSubDirectories(StrWithoutTrailingPathDelimiter(ExtractFilePath(SortedFileNames.Strings[0])),'',SubDirectoryList);

                // process level-files
                for i:=0 to Pred(SortedFileNames.Count) do
                    if   Result then begin
                         s:=SortedFileNames.Strings[i];
                         if   FileExists(s) then begin
                              if   (Sender=MenuItemFileConversionSokobanForWindows) or
                                   ((Sender=nil) and StrEqual(ExtractFileExt(s),SOKOBAN_FOR_WINDOWS_LEVEL_FILE_NAME_EXT)) then
                                   Result:=LoadSokobanForWindowsLevels(s)
                              else Result:=SokoFile.LoadFromFile(s);

                              if Result then
                                 if      SokoFile.Levels.IsEmpty then begin
                                         if Sender<>nil then
                                            Result:=Error(Format(FileNotASokobanGameText__,[s]),Application.Title+SUB_TITLE_SEPARATOR+FileConversionText)
                                         end
                                 else if SokoFile.Levels.Count=1 then begin
                                         if         ( Sender = MenuItemFileConversionNormalizeBoard ) or
                                                    ( Sender = MenuItemFileConversionNormalizeBoardMakeRectangularBoard ) then begin
                                                    SokoFile.Modified := NormalizeBoard( SokoFile, TLevel( SokoFile.Levels.First ), Sender = MenuItemFileConversionNormalizeBoardMakeRectangularBoard );
                                                    end
                                         else begin
                                            if      (Sender=MenuItemFileConversionSokobanForWindows) or
                                                    ((Sender=nil) and StrEqual(ExtractFileExt(s),SOKOBAN_FOR_WINDOWS_LEVEL_FILE_NAME_EXT)) then begin
                                                    Result:=LoadSokobanForWindowsSolutions(SokoFile.Levels,s);
                                                    if Result and (Sender=nil) then DeleteFileList.AddTextLine(s,True);
                                                    end
                                            else if Sender=MenuItemFileConversionSokofan then
                                                    Result:=LoadSokofanSolutions(SokoFile.Levels,s)
                                            else if (Sender=MenuItemFileConversionSokoMind) or (Sender=nil) then begin
                                                    RemoveSokoMindCommentLineTags(TLevel(SokoFile.Levels.First));
                                                    Result:=LoadSokoMindSolutions(TLevel(SokoFile.Levels.First),s);
                                                    if Result and (Sender=nil) then DeleteFileList.AddTextLine(s,True);
                                                    end;

                                            CombinedSokoFile.Levels.Push(SokoFile.Levels.Pop);
                                            if Sender=MenuItemFileConversionMergeLevelFiles then begin
                                               with TLevel(CombinedSokoFile.Levels.First) do
                                                 Notes.MacroExpand(SokoFile.FileHeader.Lines,C_STYLE_NEWLINE);
                                               end
                                            else if Sender=nil then with TLevel(CombinedSokoFile.Levels.First) do begin
                                                    if not Notes.Lines.ReadString(KEY_TITLE,s) then
                                                       Notes.Lines.WriteString(KEY_TITLE,ExtractFileNameWithoutExt(SortedFileNames.Strings[i]));
                                                    end;
                                            end;
                                         end

                                      else begin // more than one level in file
                                         if ( Sender = MenuItemFileConversionNormalizeBoard ) or
                                            ( Sender = MenuItemFileConversionNormalizeBoardMakeRectangularBoard ) then begin
                                            Level:=TLevel(SokoFile.Levels.First);
                                            while Result and (Level<>nil) do begin
                                              if   NormalizeBoard( SokoFile, Level, Sender = MenuItemFileConversionNormalizeBoardMakeRectangularBoard ) then
                                                   SokoFile.Modified := True;
                                              if   SokoFile.Levels.IsEmpty then // 'True': task failed
                                                   Result := False
                                              else Level:=TLevel(Level.Next)
                                              end;
                                            if SokoFile.Modified then
                                               Result := ( MainForm.SokoFile.Close
                                                           and
                                                           SokoFile.SaveToFile( SokoFile.Name ) )
                                                         or
                                                         Error( TEXT_TASK_FAILED, Application.Title + SUB_TITLE_SEPARATOR + FileConversionText );
                                            end
                                         else begin
                                            if      (Sender=MenuItemFileConversionSokobanForWindows) or
                                                    ((Sender=nil) and StrEqual(ExtractFileExt(s),SOKOBAN_FOR_WINDOWS_LEVEL_FILE_NAME_EXT))  then begin
                                                    Result:=LoadSokobanForWindowsSolutions(SokoFile.Levels,s);
                                                    if Result and (Sender=nil) then DeleteFileList.AddTextLine(s,True);
                                                    end
                                            else if Sender=MenuItemFileConversionSokofan then
                                                    Result:=LoadSokofanSolutions(SokoFile.Levels,s)
                                            else if (Sender=MenuItemFileConversionSokoMind) or (Sender=nil) then begin
                                                    Level:=TLevel(SokoFile.Levels.First);
                                                    while Result and (Level<>nil) do begin
                                                      RemoveSokoMindCommentLineTags(Level);
                                                      Result:=LoadSokoMindSolutions(Level,StrWithTrailingPathDelimiter(ExtractFilePath(s))+Level.Name);
                                                      Level:=TLevel(Level.Next);
                                                      end;
                                                    end;

                                            if not SokoFile.FileHeader.Lines.IsEmpty then
                                               with CombinedSokoFile.FileHeader.Lines do begin
                                                 if not IsEmpty then begin
                                                    AddBlankLine;
                                                    with SokoFile.FileHeader.Lines do begin {ensure there isn't more than one file-format-description}
                                                       if FindKey(KEY_DATE_CREATED    ,Node) then Remove(Node,True);
                                                       if FindKey(KEY_DATE_LAST_CHANGE,Node) then Remove(Node,True);
                                                       SokoFile.RemoveFileFormatDescription;
                                                       end;
                                                    end;

                                                 while not SokoFile.FileHeader.Lines.IsEmpty do
                                                   Add(SokoFile.FileHeader.Lines.Pop);
                                                 end;

                                            while not SokoFile.Levels.IsEmpty do begin
                                              CombinedSokoFile.Levels.Push(SokoFile.Levels.Pop);
                                              if Sender=MenuItemFileConversionMergeLevelFiles then
                                                 with TLevel(CombinedSokoFile.Levels.First) do
                                                   Notes.MacroExpand(SokoFile.FileHeader.Lines,C_STYLE_NEWLINE);
                                              end;
                                            end;
                                         end;
                              end
                         else Result:=Error(Format(TEXT_FILE_NOT_FOUND_FORMAT,[s]),Application.Title+SUB_TITLE_SEPARATOR+FileConversionText)
                         end
                    else break;

                if CombinedSokoFile.Levels.IsEmpty then Result:=False;

                if Result then begin
                   CombinedSokoFile.Levels.Reverse;
                   if CombinedSokoFile.Levels.Count=1 then begin
                      s:=CombinedSokoFile.Levels.First.Name;
                      if s='' then s:=TEXT_LEVEL;
                      end
                   else begin
                      Level:=TLevel(CombinedSokoFile.Levels.First); i:=0;
                      while Result and (Level<>nil) do begin
                        Inc(i);
                        if (Level.Name='') or
                           StrEqual(Level.Name,TEXT_LEVEL) then
                           Result:=Level.SetName(TEXT_LEVEL+SPACE+IntToStr(i));
                        Level:=TLevel(Level.Next);
                        end;

                      if   SortedFileNames.Count=1 then
                           s:=ExtractFileNameWithoutPathAndExtension(SortedFileNames.Strings[0])
                      else s:='';
                      if s='' then s:=TEXT_LEVELS;
                      end;
                   s:=StrWithTrailingPathDelimiter(ExtractFilePath(SortedFileNames.Strings[0]))+s;

                   if Sender=nil then begin
                      s1:=ExtractFileName(FileListBox1.Directory);
                      if   StrEqual(s1,Copy(XSB_FILE_NAME_EXT,2,MaxInt)) then begin
                           s1:=ExtractFileName(StrWithoutTrailingPathDelimiter(ExtractFilePath(FileListBox1.Directory)));
                           if s1<>'' then s1:=StrWithTrailingPathDelimiter(ExtractFilePath(FileListBox1.Directory))+s1;
                           end
                      else s1:=StrWithTrailingPathDelimiter(FileListBox1.Directory)+s1;
                      if s1<>'' then s:=s1;
                      end;

                   s:=MakeNewFileName(s,SOKOBAN_FILE_NAME_EXT,True);
                   Result:=s<>'';

                   Screen.Cursor:=oCursor;

                   if   Result then
                        if (Sender=nil) or FileForm.Perform(ftSaveAs,DiskCurrentItemType,'',s,s,False,False,'','') then begin
                           Screen.Cursor:=crHourGlass;
                           SysUtils.DeleteFile(s);
                           SysUtils.RemoveDir(s);
                           Result:=MainForm.SokoFile.Close and
                                   CombinedSokoFile.SaveToFile(s);
                           if Result then begin
                              CurrentFileName:=s;

                              DeleteFileList.MergeSort(CompareNodes);
                              try
                                Node:=DeleteFileList.First;
                                while Node<>nil do begin
                                  SysUtils.DeleteFile(Node.Text);
                                  Node:=Node.Next;
                                  end;
                                Node:=DeleteFileList.First;
                                while Node<>nil do begin
                                  if (Node.Next=nil) or
                                     (not StrEqual(ExtractFilePath(Node.Text),ExtractFilePath(Node.Next.Text))) then
                                     SysUtils.RemoveDir(StrWithoutTrailingPathDelimiter(ExtractFilePath(Node.Text)));
                                  Node:=Node.Next;
                                  end;
                              except on E:Exception do begin end;
                              end;
                              end;
                           end
                        else begin
                           // the user cancelled the job
                           end;
                   if not Result then Error(TEXT_TASK_FAILED,Application.Title+SUB_TITLE_SEPARATOR+FileConversionText);
                   end;

        finally Screen.Cursor:=oCursor;
                TextLines.Free;
                SokoFile.Free;
                CombinedSokoFile.Free;
                SortedFileNames.Free; SubDirectoryList.Free; DeleteFileList.Free;

                DirectoryListBox1.Update; FileListBox1.Update;
                if Sender<>nil then begin
                   CurrentCollectionFileName:=''; // otherwise 'TryToLoadGame.LoadPackFileIndex' may falsely think that it already has loaded the correct index
                   s:=CurrentFileName;
                   if   IsAnIniFileSectionFileName(s) then s:=ExtractIniFileName(s);
                   if   FileExists(s) then
                        TryToLoadFile(CurrentFileName)
                   else TryToLoadFile('');

                   s:=CurrentFileName;
                   if IsAnIniFileSectionFileName(s) then s:=ExtractIniFileName(s);
                   if (Length(s)>3) and (s[2]=COLON) then
                      try     IsLoading:=True;
                              DirectoryListBox1.Drive:=UpCase(s[1]);
                              DirectoryListBox1.Directory:=StrWithTrailingPathDelimiter(ExtractFilePath(s));
                              FileListBox1.FileName:=s;
                      finally IsLoading:=False;
                              AddItemOrMoveItemToTopOfComboBox(LevelsFolderHistoryComboBox,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(DirectoryListBox1.Directory),MainForm.MyDocumentsFolder),True);
                              AddItemOrMoveItemToTopOfComboBox(LevelsFileHistoryComboBox  ,MAX_FILE_HISTORY_ITEMS,AbbreviatedFilePath(CurrentFileName,MainForm.MyDocumentsFolder),True);
                      end;
                   end;
        end;
        end;

     StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:='';
     end;
end;

function TOpenForm.GetBitMapText:Boolean;
var ErrorText:String;
begin
  Result:=False;
  if MenuItemImageText.Checked then with Image1.Picture.BitMap do with DragRect do
     if (Left>=0) and (Top>=0) and (Right<=Width) and (Bottom<=Height) then
        with Texts[tTextFile].Memo do begin
          ShowDragRect(DragRect);
          Lines.BeginUpdate;
          try
            if WordWrap then WordWrap:=False;
            if BitMap_.GetBitMapText(Image1.Picture.BitMap,DragRect,True,Lines,ErrorText) then begin
               Modified:=False;
               TextFileMemoChange(Self);
               Result:=True;
               end
            else Result:=Error(ErrorText,Application.Title+SUB_TITLE_SEPARATOR+TopPanel.Caption);
          finally
            Lines.EndUpdate;
          end;
          end
     else with Texts[tTextFile] do begin
        if Memo.Modified or (Memo.Lines.Count<>0) then ClearText(tTextFile);
        TextFileMemoChange(Self);
        end;
end;

function TOpenForm.SaveCurrentImageInBMPFormat( var ErrorMessage__ : String ) : Boolean;
var s : String;
begin
  Result := False; ErrorMessage__ := '';
  if StrEqual(BMP_FILE_EXT,ExtractFileExt(CurrentFileName)) then
     s:=CurrentFileName
  else begin
     s:=ChangeFileExt(CurrentFileName,BMP_FILE_EXT);
     if   FileExists(s) then begin
          if Application.MessageBox(PChar(Format(OnlySavingImagesInBMPFormatIsSupportedText+NL+NL+OverwriteExistingItemText__,[s])),PChar(Caption),MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)<>ID_YES then
             s:='';
          end
     else if Application.MessageBox(PChar(OnlySavingImagesInBMPFormatIsSupportedText+NL+NL+ChangeImageToBMPFormatText),PChar(Caption),MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON1)<>ID_YES then
             s:='';
     end;
  if s<>'' then
     try    Image1.Picture.BitMap.SaveToFile(s);
            Result:=True;
            if not StrEqual(s,CurrentFileName) then
               try     DeleteFile(CurrentFileName);
               finally CurrentFileName:=s;
                       UpdateAfterFileManagerTask( CurrentFileName );
               end;
     except on E:Exception do begin
               Result:=False; ErrorMessage__:=E.Message;
               end;
     end;
end;

function TOpenForm.ToggleMenuItemText:Boolean;
var s:String;

  function GuessImageTextPosition:Boolean;
  const MIN_RECT_SIZE=8;
  var ColCount,RowCount:Integer;
      VariableColCount,VariableRowCount:Boolean;
      s:String; R:TRect;
  begin
    Result:=True; //StrEqual(ExtractFileName(Texts[tSkinScript].FileName),ExtractFileName(MainForm.Skins.CommonSkinsScriptFileName));
    if Result then begin
       Result:=MainForm.Skins.ReadColumnsAndRows(Texts[tSkinScript].Memo.Lines,ColCount,RowCount,VariableColCount,VariableRowCount,s);
       if (s='') and (Image1.Picture.BitMap<>nil) then  begin // 'True': no parsing error, but not necessarily any data
          if ColCount<=0 then ColCount:=1;
          Result:=MainForm.Skins.GuessColumnsAndRows(Image1.Picture.BitMap,Image1.Picture.BitMap.Width,Image1.Picture.BitMap.Height,
                                                     ColCount,RowCount,VariableColCount,VariableRowCount);
          if Result then with Image1.Picture.BitMap do begin
             R:=CellToRect(3,3,Width div Max(1,ColCount),Height div Max(1,RowCount));
             with R do
               if (Left<=Right-MIN_RECT_SIZE) and (Top<=Bottom-MIN_RECT_SIZE) and
                  (Left>=0) and (Top>=0) and (Right<=Width) and (Bottom<=Height) then begin
                  IsDragging:=True;
                  try     SpinEditLeft.Value:=Left; SpinEditTop.Value:=Top;
                          SpinEditWidth.Value:=RectWidth(R); SpinEditHeight.Value:=RectHeight(R);
                  finally IsDragging:=False;
                          DragRect:=Rect(SpinEditLeft.Value,SpinEditTop.Value,SpinEditLeft.Value+SpinEditWidth.Value,SpinEditTop.Value+SpinEditHeight.Value);
                  end;
                  end;
             end;
          end;
       end;
  end;

begin // ToggleMenuItemText
  Result:= (Task=otImage) and (SubTask=osSkin) and (CurrentFileName<>'') and
           ImageScrollBox.Visible and
           (SkinScriptGroupBox.Visible
            or
            ImageGroupBox.Visible
           ) and
           (Image1.Picture.BitMap<>nil) and
           (PIXEL_BYTE_SIZE[Image1.Picture.BitMap.PixelFormat]<>0);

  if Result then begin
     MenuItemImageText.Checked:=not MenuItemImageText.Checked;

     if MenuItemImageText.Checked then begin
        ImagePanel.Height    :=DiskGroupBox.Height-ImagePanel.Top;
        TextFilePanel.Top    :=GroupBox1.Top-ImageFileGroupBox.Top;
        TextFilePanel.Height :=BtnOK.Top-8-TextFilePanel.Top;

        TopPanel.Caption     :=SkinText+SUB_TITLE_SEPARATOR+ImageTextCaptionText+SPACE;

        SelectionEnabled     :=True;
        Texts[tTextFile].Memo.Modified:=False;

        DiskGroupBox.Hide;

        if (RectWidth(DragRect)=0) or (RectHeight(DragRect)=0) then
           GuessImageTextPosition;

        GetBitMapText;
        end
     else begin
        HideDragRect;
        if Texts[tTextFile].Memo.Modified and
           (RectWidth(DragRect)>0) and (RectHeight(DragRect)>0) then begin
           Image1.Repaint;
           s := '';
           Result:=SetBitMapText(Image1.Picture.BitMap,DragRect,Trim(Texts[tTextFile].Memo.Text),s);
           if   Result then begin
                Result := SaveCurrentImageInBMPFormat( s );
                if   Result then
                     Texts[tTextFile].Memo.Modified:=False;
                end;
           if not Result then begin
              if s = '' then s := TEXT_TASK_FAILED;
              Msg(s,Application.Title+SUB_TITLE_SEPARATOR+TopPanel.Caption,MB_OK+MB_ICONERROR);
              end;
           end
        else
           Texts[tTextFile].Memo.Modified:=False;

        if Result then begin
           DoNotSplitImageFileGroupBox;

           TopPanel.Caption     :=SkinText+SPACE;
           SelectionEnabled     :=False;
           StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';

           s:=CurrentFileName;

           DiskGroupBox.Show;

           FileListBox1.Update; // kludge: otherwise, the following problem occurs for some unknown reason:
           TryToLoadFile(s);    // if base-file is the previous file, and the user clicks that one with the mouse then nothing happens, i.e., it isn't loaded

           if DiskGroupBox.Visible then ActiveControl:=FileListBox1;
           HideDragRect;
           end
        else begin // something went wrong; go back to editing the image text
           MenuItemImageText.Checked:=True;
           ShowDragRect(DragRect);
           end;
        end;

     RectGroupBox   .Visible:=MenuItemImageText.Checked;
     ImageGroupBox  .Visible:=MenuItemImageText.Checked;
     TextFilePanel  .Visible:=MenuItemImageText.Checked;
     TextFileMemo   .Visible:=TextFilePanel.Visible;
     SkinScriptPanel.Visible:=not ImageGroupBox.Visible;

     TextFileMemoChange(Self);
     end;
end;

procedure TOpenForm.MenuItemImageTextClick(Sender: TObject);
begin
  ToggleMenuItemText;
end;

procedure TOpenForm.MenuItemInitializeSkinForCaptureToolClick( Sender: TObject);
var ColCount,RowCount:Integer;
    b,VariableColCount,VariableRowCount,Result : Boolean;
    s,MenuItemCaption:String;
    SingleRowSkinBitmap : TBitmap;
begin
  Result:= (Task=otImage) and (SubTask=osSkin) and (CurrentFileName<>'') and
           ImageScrollBox.Visible and
           (SkinScriptGroupBox.Visible
            or
            ImageGroupBox.Visible
           ) and
           (Image1.Picture.BitMap<>nil) and
           (PIXEL_BYTE_SIZE[Image1.Picture.BitMap.PixelFormat]<>0);
  if Result then begin
     ColCount := 0; RowCount := 0;
     //Result:=
         MainForm.Skins.ReadColumnsAndRows(Texts[tSkinScript].Memo.Lines,ColCount,RowCount,VariableColCount,VariableRowCount,s);
     if (s='') and (Image1.Picture.BitMap<>nil) then begin // 'True': no parsing error, but not necessarily any data
        if ColCount<=0 then ColCount:=1;
        //Result:=
          MainForm.Skins.GuessColumnsAndRows(Image1.Picture.BitMap,Image1.Picture.BitMap.Width,Image1.Picture.BitMap.Height,
                                                   ColCount,RowCount,VariableColCount,VariableRowCount);
        end;
     if ColCount = 0 then ColCount := 4;
     if RowCount = 0 then RowCount := 4;

     s :=IntToStr( ColCount );
     if   Sender=MenuItemInitializeSkinWithColumnsAndRows then
          MenuItemCaption:=MenuItemInitializeSkinWithColumnsAndRows.Caption
     else MenuItemCaption:=MenuItemInitializeSkinForCaptureTool.Caption;
     if InputQuery( Application.Title + SUB_TITLE_SEPARATOR + MenuItemCaption,
                    ColumnsText + COLON,
                    s ) and
        ( Trim( s ) <> '' ) then begin
        if   SafeStrToInt( s, False, ColCount ) then
             if   ( ColCount >= 4 ) and ( ColCount <= Image1.Picture.BitMap.Width ) and ( ( Image1.Picture.BitMap.Width mod ColCount ) = 0 ) then begin
                  s :=IntToStr( RowCount );
                  if InputQuery( Application.Title + SUB_TITLE_SEPARATOR + MenuItemCaption,
                                 RowsText + COLON,
                                 s ) and
                     ( Trim( s ) <> '' ) then begin
                     if   SafeStrToInt( s, False, RowCount ) then
                          if   ( RowCount >= 4 ) and ( RowCount <= Image1.Picture.BitMap.Height ) and ( ( Image1.Picture.BitMap.Height mod RowCount ) = 0 ) then begin
                               SingleRowSkinBitmap := nil;
                               try     Result := CaptureForm.CreateSingleRowSkinFromSkin( Image1.Picture.BitMap,
                                                                                          Image1.Picture.BitMap.Width div ColCount,
                                                                                          Image1.Picture.BitMap.Height div RowCount,
                                                                                          SingleRowSkinBitmap )
                                                 and
                                                 CaptureForm.CreateSkinCaptureInformation( SingleRowSkinBitmap, Image1.Picture.BitMap.Width div ColCount, Image1.Picture.BitMap.Height div RowCount, False, b )
                                                 and
                                                 CaptureForm.WriteSkinCaptureInformation ( Image1.Picture.BitMap, CaptureForm.SkinCaptureInformation );
                                       if Result then
                                          try    Result := SaveCurrentImageInBMPFormat( s );
                                                 if s <> '' then raise Exception.Create( s );
                                          except on E:Exception do Result := Error( E.Message, '' );
                                          end;

                               finally SingleRowSkinBitmap.Free;
                                       CaptureForm.CreateSkinCaptureInformationUpdatingMatchingSkin( b ); // recalculate the skin capture information for the current skin in the capture tool window, if any
                                       if not Result then
                                          Error( TEXT_TASK_FAILED, '' );
                               end;
                               end
                          else Error( Format( Minimum2DSizeText__, [ 4, 4 ] ), '' )
                     else Error( Format( NotANumberText__, [ s ] ), '' );
                     end
                  end
             else Error( Format( Minimum2DSizeText__, [ 4, 4 ] ), '' )
        else Error( Format( NotANumberText__, [ s ] ), '' );
        end;
     end;
end;

procedure TOpenForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=not (Assigned(DuplicatesForm) and DuplicatesForm.IsBusy);
  if CanClose and MenuItemImageText.Checked and Texts[tTextFile].Memo.Modified then begin
     if (RectWidth(DragRect)>0) and (RectHeight(DragRect)>0) then
        case Msg(TextHasChangedText+NL+NL+DoYouWantToSaveItText,
                 Application.Title+SUB_TITLE_SEPARATOR+TopPanel.Caption,
                 MB_YESNOCANCEL+MB_ICONQUESTION) of
          IDYES    : CanClose:=ToggleMenuItemText;
          IDNO     : begin ClearText(tTextFile); CanClose:=ToggleMenuItemText;
                     end;
          IDCANCEL : CanClose:=False;
        end // case
     else begin
        ClearText(tTextFile); CanClose:=ToggleMenuItemText;
        end;
     end;
end;

procedure TOpenForm.TextFileMemoChange(Sender: TObject);
begin
  if MenuItemImageText.Checked then
     StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=OKChangedText[Texts[tTextFile].Memo.Modified];
end;

procedure TOpenForm.FolderHistoryComboBoxDropDown(Sender: TObject);
begin
  if PanelToolTips.Visible then PanelToolTips.Hide;
  if Sender is TComboBox then with Sender as TComboBox do ItemIndex:=-1; // so all items show up with the same text color and background color
end;

procedure TOpenForm.HistoryComboBoxDropDown(Sender: TObject);
begin
  if PanelToolTips.Visible then PanelToolTips.Hide;
  if Sender is TComboBox then with Sender as TComboBox do ItemIndex:=-1; // so all items show up with the same text color and background color
end;

procedure TOpenForm.MenuItemCopyToClipboardFormatClick(Sender: TObject);
begin
  MenuItemCopyToClipboardFormatNormal.Checked:=Sender=MenuItemCopyToClipboardFormatNormal;
  MenuItemCopyToClipboardFormatRLE   .Checked:=not MenuItemCopyToClipboardFormatNormal.Checked;
  StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=CopyToClipboardText+COLON+SPACE+CopyToClipboardFormatText[MenuItemCopyToClipboardFormatNormal.Checked];
end;

procedure TOpenForm.EnableDisablePluginControls(Sender: TObject; Plugin__:TPlugin; ClearStatus__:Boolean);
var i:Integer; IsPluginRunning:Boolean; s:String;
begin // precondition: 'Plugin__' is either 'MainForm.Solve', 'MainForm.Optimizer', or 'MainForm.Generator' and non-nil
  if Visible and (PluginForTask(Task)<>Plugin__) then Plugin__:=nil;

  if Assigned(Plugin__) then begin
     BtnSolveLevel.Visible:=Plugin__=MainForm.Solver;
     //BtnOptimizeGames.Visible:=not BtnSolveLevel.Visible;
     //BtnSolveLevel   .Visible:=False; // starting and stopping the solver    from 'OpenForm' isn't fully implemented, hence, disable the button
     BtnOptimizeGames.Visible:=False; // starting and stopping the optimizer from 'OpenForm' isn't fully implemented, hence, disable the button
     if   BtnSolveLevel.Visible or BtnOptimizeGames.Visible then with PluginLevelFileNamePanel do
          Width:=BtnSolveLevel.Left-2*Left
     else PluginLevelFileNamePanel.Width:=PanelPluginLevelInfo.Width;

     BtnPluginSettings.Enabled:=Assigned(Plugin__.PluginFunctions.Settings);
     BtnPluginAbout.Enabled   :=Assigned(Plugin__.PluginFunctions.ShowAbout);
     Image1.Visible:=Plugin__.IsLoaded;
     s:=Plugin__.PluginName;
     if (s='') and PluginGroupBox.Visible then s:=ImageNamePanel.Caption;
     if s='' then s:=Plugin__.PluginTypeText;
     SetGroupBoxCaption(Self.Canvas,PluginGroupBox,s);

     if   PluginLevelMemo.Lines.Count=0 then PluginLevelMemo.Text:=HintPluginHasNoCancellationText;

     if        Plugin__.Button.Tag<>Ord(pbsRun) then
               Plugin__.Button.Enabled:=True
     else if   Plugin__.IsLoaded and (not Plugin__.IsRunningInAnotherThread) then
               if   (Assigned(ToolsForm) and ToolsForm.Visible) or (Sender=ToolsForm) or (Sender=nil) then
                    if        Plugin__ is TOptimizerPlugin then
                              Plugin__.Button.Enabled:={ToolsForm.PluginLevelInfo.IsALegalLevel and} HasSolutionsForTheOptimizer(ToolsForm)
                    else if   Plugin__ is TSolverPlugin then
                              Plugin__.Button.Enabled:=((not ToolsForm.SolveLevelsGroupBox.Visible) and ToolsForm.PluginLevelInfo.IsALegalLevel)
                                                       or
                                                       (     ToolsForm.SolveLevelsGroupBox.Visible  and (ToolsForm.SolverTaskQueue.SelectedCount<>0))
                         else Plugin__.Button.Enabled:=ToolsForm.PluginLevelInfo.IsALegalLevel or (Plugin__.SelectedLevelsCount<>0)
               else if        Plugin__ is TOptimizerPlugin then
                              Plugin__.Button.Enabled:=(MainForm.Game.GameState<>gsNull) and HasSolutionsForTheOptimizer(Self)
                    else if   Plugin__ is TSolverPlugin then
                              Plugin__.Button.Enabled:=(MainForm.Game.GameState<>gsNull) or (Plugin__.SelectedLevelsCount<>0)
                         else Plugin__.Button.Enabled:=(MainForm.Game.GameState<>gsNull) or (Plugin__.SelectedLevelsCount<>0)
          else Plugin__.Button.Enabled:=False;

     if   Plugin__.IsLoaded and (Plugin__.PluginLimitsAsText<>'') then
          BtnPluginAbout.Hint:=HintShowPluginInformationText+'  ('+PluginLimitsText+COLON+SPACE+Plugin__.PluginLimitsAsText+RIGHT_PAREN
     else BtnPluginAbout.Hint:=HintShowPluginInformationText;

     s:='';
     Plugin__.Enter;
     try     IsPluginRunning:=Plugin__.IsActive;

             if IsPluginRunning and (Sender<>nil) then begin
                //DiskGroupBox.Visible:=False;
                //BtnCancel1  .Enabled:=False;
                if   PluginLevelMemo.Visible then begin
                     PluginLevelMemo.Visible:=False;
                     PluginLevelStringGrid.Visible:=True;
                     end;
                if   (Plugin__ is TOptimizerPlugin) then
                     if   Assigned(Plugin__.Level) then
                          s:=Plugin__.Level.SnapshotsAsText.Last.Name
                     else s:=''
                else s:=VisualFileName(Plugin__.LevelFileName);
                s:=StrWithQuotedAmpersands(s);
                if   s<>'' then PluginLevelFileNamePanel.Caption:=SPACE+s+SPACE // the first space is for pretty-printing in the panel; the second space is for a better balance in the "mouse-over" tool-tip
                else PluginLevelFileNamePanel.Caption:='';
                BtnPluginSettings.Hint:=HintPluginSettings2Text;
                s:=PluginLevelFileNamePanel.Caption; // s<>'': the plugin is processing a level or a game at the moment
                end
             else begin
                if   Assigned(ToolsForm) and ToolsForm.Visible then
                     if        Plugin__ is TOptimizerPlugin then
                               if   Assigned(Plugin__.Level) and
                                    IsPluginRunning then
                                    s:=Plugin__.Level.SnapshotsAsText.Last.Name
                               else s:=''
                     else if   Plugin__ is TSolverPlugin then
                               if   Assigned(Plugin__.Level) and
                                    IsPluginRunning then
                                    s:=VisualFileName(Plugin__.Level.Name)
                               else if   ToolsForm.SolveLevelsGroupBox.Visible then
                                         s:=''
                                    else s:=VisualFileName(ToolsForm.PluginLevelInfo.LevelName)
                          else s:=VisualFileName(ToolsForm.PluginLevelInfo.LevelName)
                else s:=VisualFileName(MainForm.Game.FileName);
                s:=StrWithQuotedAmpersands(s);
                if   s<>'' then PluginLevelFileNamePanel.Caption:=SPACE+s+SPACE
                else PluginLevelFileNamePanel.Caption:='';
                PluginLevelMemo.Visible:=Plugin__.IsLoaded and (not IsPluginRunning) and (not Plugin__.IsRunningInAnotherThread) and (not Plugin__.Terminate);
                PluginLevelStringGrid.Visible:=not PluginLevelMemo.Visible;
                BtnPluginSettings.Hint:=HintPluginSettings1Text;
                if   (not ClearStatus__)
                     or
                     (Visible
                      and
                      (PluginForTask(Task)=Plugin__)
                     )
                     or
                     ((Sender=ToolsForm)
                      and
                       Assigned(ToolsForm)
                      and
                      (ToolsForm.PluginLevelInfo.ReplayInfo.MovesAsText<>'')
                     ) then begin
                     // keep the current game information on the screen
                     end
                else for i:=0 to Pred(PluginLevelStringGrid.RowCount) do PluginLevelStringGrid.Cells[1,i]:='';
                if not IsPluginRunning then s:=''; // s='': the plugin isn't active at the moment
                end;

             if Assigned(ToolsForm) then begin
                if        Plugin__ is TSolverPlugin then begin
                          if (ToolsForm.PageControl1.ActivePage<>ToolsForm.TabSheetOptimizer) and
                             (ToolsForm.PageControl1.ActivePage<>ToolsForm.TabSheetGenerator) then
                             if   (s='') and // '': the plugin is not currently processing a level or a game
                                  ToolsForm.Visible and
                                  (ToolsForm.PluginLevelInfo.LevelName<>'') and
                                  (not ToolsForm.PluginLevelInfo.IsALegalLevel) and
                                  (ToolsForm.PluginLevelInfo.ErrorText<>'') then
                                  ToolsForm.PluginLevelFileNamePanel.Caption:=SPACE+ToolsForm.PluginLevelInfo.ErrorText+SPACE
                             else ToolsForm.PluginLevelFileNamePanel.Caption:=PluginLevelFileNamePanel.Caption;
                          end
                else if   Plugin__ is TOptimizerPlugin then begin
                          if (ToolsForm.PageControl1.ActivePage<>ToolsForm.TabSheetSolver) and
                             (ToolsForm.PageControl1.ActivePage<>ToolsForm.TabSheetGenerator) then begin
                             if (s<>'') and (s[1]<>SPACE) then s:=SPACE+s+SPACE;
                             ToolsForm.PluginLevelFileNamePanel.Caption:=s;
                             end;
                          end
                else if   Plugin__ is TGenerator then begin
                          if (ToolsForm.PageControl1.ActivePage<>ToolsForm.TabSheetSolver) and
                             (ToolsForm.PageControl1.ActivePage<>ToolsForm.TabSheetOptimizer) then begin
                             if (s<>'') and (s[1]<>SPACE) then s:=SPACE+s+SPACE; 
                             ToolsForm.PluginLevelFileNamePanel.Caption:=s;
                             end;
                          end;
                ToolsForm.BtnSolveLevel          .Caption:=BtnSolveLevel.Caption;
                ToolsForm.BtnSolveLevel          .Hint   :=BtnSolveLevel.Hint;
                ToolsForm.BtnOptimizeGames       .Caption:=BtnOptimizeGames.Caption;
                ToolsForm.BtnOptimizeGames       .Hint   :=BtnOptimizeGames.Hint;

                if (ToolsForm.PluginLevelInfo.ReplayInfo.MovesAsText='') and ClearStatus__ then
                   for i:=0 to Pred(ToolsForm.PluginLevelStringGrid.RowCount) do ToolsForm.PluginLevelStringGrid.Cells[1,i]:='';

                if      Plugin__ is TSolverPlugin    then begin
                        ToolsForm.BtnSolverSettings   .Hint   :=BtnPluginSettings.Hint;
                        ToolsForm.BtnSolverAbout      .Hint   :=BtnPluginAbout.Hint;
                        ToolsForm.BtnSolverSettings   .Enabled:=BtnPluginSettings.Enabled and (not ToolsForm.Game.IsReplaying);
                        ToolsForm.BtnSolverAbout      .Enabled:=BtnPluginAbout.Enabled and (not ToolsForm.Game.IsReplaying);
                        //ToolsForm.SolveLevelsGroupBox .Visible:=Plugin__.SelectedLevelsCount<>0;

                        if (Plugin__.Button.Tag=Ord(pbsRun)) and
                           Plugin__.Button.Enabled and
                           ToolsForm.Visible and
                           (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer) and
                           Assigned(MainForm.Optimizer) and
                           StrEqual(Plugin__.PluginFileName,MainForm.Optimizer.PluginFileName) and
                           MainForm.Optimizer.IsLoaded then
                           BtnOptimizeGames.Enabled:=True;
                        end
                else if Plugin__ is TOptimizerPlugin then begin
                        ToolsForm.BtnOptimizerSettings.Hint   :=BtnPluginSettings.Hint;
                        ToolsForm.BtnOptimizerAbout   .Hint   :=BtnPluginAbout.Hint;
                        ToolsForm.BtnOptimizerSettings.Enabled:=BtnPluginSettings.Enabled and (not ToolsForm.Game.IsReplaying);
                        ToolsForm.BtnOptimizerAbout   .Enabled:=BtnPluginAbout.Enabled and (not ToolsForm.Game.IsReplaying);

                        if (Plugin__.Button.Tag=Ord(pbsRun)) and
                           Plugin__.Button.Enabled and
                           ToolsForm.Visible and
                           (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver) and
                           Assigned(MainForm.Solver) and
                           StrEqual(Plugin__.PluginFileName,MainForm.Solver.PluginFileName) and
                           MainForm.Solver.IsLoaded then
                           BtnSolveLevel.Enabled:=True;
                        end;

                ToolsForm.BtnSolveLevel           .Enabled:=BtnSolveLevel.Enabled and (not ToolsForm.Game.IsReplaying);
                ToolsForm.BtnOptimizeGames        .Enabled:=BtnOptimizeGames.Enabled and (not ToolsForm.Game.IsReplaying);

                if ToolsForm.PluginLevelMemo.Lines.Count=0 then ToolsForm.PluginLevelMemo.Text:=HintPluginHasNoCancellationText;
                ToolsForm.PluginLevelMemo         .Visible:=PluginLevelMemo.Visible;
                ToolsForm.PluginLevelStringGrid   .Visible:=PluginLevelStringGrid.Visible;
                end;

     finally Plugin__.Leave;
             if (Sender<>ToolsForm) and (Plugin__ is TSolverPlugin) then
                ShowPluginCaption(Plugin__.PluginTypeText,s);
     end;
     end;
end;

procedure TOpenForm.BtnPluginSettingsClick(Sender: TObject);
begin
  if Assigned(PluginForTask(Task)) then
     // PluginForTask(Task).Settings(Handle);
     ToolsForm.PluginSettingsWindowOrAboutFactBox(BtnPluginSettings);
end;

procedure TOpenForm.BtnPluginAboutClick(Sender: TObject);
begin
  if Assigned(PluginForTask(Task)) then
     // PluginForTask(Task).ShowAbout(Handle);
     ToolsForm.PluginSettingsWindowOrAboutFactBox(BtnPluginAbout);
end;

procedure TOpenForm.PluginLevelFileNamePanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ControlMouseMove(nil,Shift,X,Y);
  if ToolTips.Enabled then with PanelToolTips do begin
     if Caption<>PluginLevelFileNamePanel.Caption then begin
        if Visible then Hide;
        Caption:=PluginLevelFileNamePanel.Caption;
        Width:=Self.Canvas.TextWidth(Caption)+8;
        end;
     Left:=Min(PluginLevelGroupBox.Left+PluginLevelFileNamePanel.Left+X+ToolTips.OffsetX,Self.ClientWidth-Width-DiskGroupBox.Left);
     Top :=PluginLevelGroupBox.Top +PluginLevelFileNamePanel.Top +Y+ToolTips.OffsetY;
     if (not PanelToolTips.Visible) and (Caption<>'') then PanelToolTips.Show;
     end;
end;

procedure TOpenForm.BtnPluginClick(Sender: TObject);
var i:Integer; NewLevel:TLevel; Plugin:TPlugin;
begin
  if   Sender is TPlugin then Plugin:=TPlugin(Sender)
  else Plugin:=PluginForTask(Task);
  if Assigned(Plugin) and Assigned(Plugin.PluginThread) then with Plugin do
     case TPluginButtonState(Button.Tag) of
       pbsCancel      : begin
                          Plugin.Enter;
                          try if      Terminate then begin
                                      SetPluginButtonState(Button,pbsTerminating);
                                      Plugin.ThreadState:=ptsIdle;
                                      MainForm.BusyMode:=False;
                                      if      Plugin is TOptimizerPlugin then begin
                                              if (Screen.ActiveForm=ToolsForm) and
                                                 (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer) and
                                                 (ToolsForm.ActiveControl=ToolsForm.BtnOptimizeGames) and
                                                 ToolsForm.OptimizeSolutionsGroupBox.Visible then
                                                 ToolsForm.OptimizerTaskQueue.StringGrid.SetFocus; // focus the task queue; after stopping the optimizer, the user often wants to use the mouse wheel to scroll through the items on the task queue, e.g., to locate a newly found optimized solution
                                              end
                                      else if Plugin is TSolverPlugin then begin
                                              //Plugin.ClearQueue(False);
                                              if (Screen.ActiveForm=ToolsForm) and
                                                 (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver) and
                                                 (ToolsForm.ActiveControl=ToolsForm.BtnSolveLevel) and
                                                 ToolsForm.SolveLevelsGroupBox.Visible then
                                                 ToolsForm.SolverTaskQueue.StringGrid.SetFocus; // focus the task queue; after stopping the optimizer, the user often wants to use the mouse wheel to scroll through the items on the task queue
                                              end
                                      else if Plugin is TGenerator then begin
                                              if (Screen.ActiveForm=ToolsForm) and
                                                 (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator) and
                                                 (ToolsForm.ActiveControl=ToolsForm.BtnGenerateLevels) and
                                                 ToolsForm.GenerateLevelsGroupBox.Visible then
                                                 ToolsForm.GeneratorTaskQueue.StringGrid.SetFocus; // focus the task queue; after stopping the generator}
                                              end;
                                      end
                              else if Msg(TerminatePluginNowEvenThoughItMeansShuttingDownTheApplicationText,TitleWithOptionalSubTitle(TitleWithOptionalSubTitle(Application.Title,Plugin.PluginTypeText),Plugin.PluginName),MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)=mrYes then begin
                                      SetPluginButtonState(Button,pbsRun);
                                      MainForm.ShutDownApplication:=True;
                                      if Sender<>ToolsForm then BtnOpenClick(Sender);
                                      end;
                          finally Plugin.Leave;
                          end;
                        end;
       pbsTerminating : if   Msg(TerminatePluginNowEvenThoughItMeansShuttingDownTheApplicationText,TitleWithOptionalSubTitle(TitleWithOptionalSubTitle(Application.Title,Plugin.PluginTypeText),Plugin.PluginName),MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)=mrYes then begin
                             SetPluginButtonState(Button,pbsRun);
                             MainForm.ShutDownApplication:=True;
                             MainForm.BusyMode:=False;
                             if Sender<>ToolsForm then BtnOpenClick(Sender);
                             end;
       pbsRun         : if Button.Enabled then begin
                           StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
                           PluginLevelMemo.Visible:=False;
                           PluginLevelStringGrid.Visible:=True;
                           for i:=0 to Pred(PluginLevelStringGrid.RowCount) do PluginLevelStringGrid.Cells[1,i]:='';
                           with ToolsForm do begin
                             ImageBoard.Hint:='';
                             StatusBar1.Panels[0].Text:='';
                             StatusBar1.Panels[1].Text:='';
                             PluginLevelMemo.Visible:=False;
                             PluginLevelStringGrid.Visible:=True;
                             for i:=0 to Pred(PluginLevelStringGrid.RowCount) do PluginLevelStringGrid.Cells[1,i]:='';
                             if PluginToolButtonReplay.Enabled and (not (Plugin is TOptimizerPlugin)) and (not (Plugin is TGenerator)) then with PluginLevelInfo.ReplayInfo do begin
                                ToolsForm.ClearPluginReplayInfo;
                                if   PluginToolButtonStopReplay.Enabled then
                                     PluginToolButtonStopReplayClick(PluginToolButtonReplay); // display the start position
                                end;
                             end;
                           SetPluginButtonState(Button,pbsCancel);

                           try

                             if not (Plugin is TGenerator) then with ToolsForm.PluginLevelInfo do with BoardAsText do begin
                                if ToolsForm.Visible then begin // process level from the tools form
                                   end
                                else begin // process level from the main window
                                   Width        :=MainForm.Game.BoardWidth;
                                   Height       :=MainForm.Game.BoardHeight;
                                   Board        :=MainForm.Game.GetStartBoardAsText;
                                   IsALegalLevel:=MainForm.Game.GameState<>gsNull;
                                   LevelName    :=MainForm.Game.FileName;
                                   end;
                                if IsALegalLevel
                                   or
                                   (Plugin is TOptimizerPlugin)
                                   or
                                   ((Plugin is TSolverPlugin) and (Plugin.SelectedLevelsCount<>0)) then begin
                                   if      Plugin is TSolverPlugin then begin
                                           Plugin.Enter;
                                           try     if   Plugin.SelectedLevelsCount=0 then begin
                                                        if not Plugin.LoadLevel(Width,Height,-1,0,LevelName,Board,nil,True,True,NewLevel) then
                                                           SetPluginButtonState(Button,pbsRun);
                                                        end
                                                   else if Plugin.ThreadState=ptsIdle then begin
                                                           Plugin.ThreadState:=ptsLoadLevel;
                                                           Plugin.Resume;
                                                           end;
                                           finally Plugin.Leave;
                                           end;
                                           end
                                   else if Plugin is TOptimizerPlugin then begin
                                           Plugin.Enter;
                                           try
                                                   ToolsForm.OptimizerTaskQueue.ClearAlternateOptimizationTasks; // reset alternating optimization tasks

                                                   if ToolsForm.OptimizerTaskQueue.SelectedCount<>0 then begin
                                                      Plugin.PluginThread.State:=Ord(ptsLoadLevel);
                                                      Plugin.Resume;
                                                      end
                                                   else
                                                      if not ToolsForm.Visible then begin // optimize best solutions for the game in the main window; this is obsolete and not in production
                                                         if Assigned(MainForm.Game.BestSolutionMoves) then begin
                                                            if   Assigned(MainForm.Game.BestSolutionPushes) then
                                                                 i:=SOKOBAN_PLUGIN_FLAG_MOVES
                                                            else i:=SOKOBAN_PLUGIN_FLAG_NONE; // use default optimization
                                                            Plugin.LoadLevel(Width,Height,-1,i,LevelName,Board,MainForm.Game.BestSolutionMoves,True,True,NewLevel);
                                                            end;
                                                         if Assigned(MainForm.Game.BestSolutionPushes) then begin
                                                            if   Assigned(MainForm.Game.BestSolutionMoves) then
                                                                 i:=SOKOBAN_PLUGIN_FLAG_PUSHES
                                                            else i:=SOKOBAN_PLUGIN_FLAG_NONE; // use default optimization
                                                            Plugin.LoadLevel(Width,Height,-1,i,LevelName,Board,MainForm.Game.BestSolutionPushes,True,True,NewLevel);
                                                            end;
                                                         if not Plugin.IsActive then
                                                            SetPluginButtonState(Button,pbsRun); // 'LoadLevel' didn't succeed; reset the button-state
                                                         end;
                                           finally Plugin.Leave;
                                           end;
                                           end;
                                   end;
                                end
                             else begin // start the level generator
                                if not MainForm.Generator.Run then
                                   SetPluginButtonState(Button,pbsRun); // launching the generator failed; reset the button-state
                                end;
                           except on E:Exception do begin
                                     if Plugin is TSolverPlugin then ToolsForm.PluginLevelInfo.IsALegalLevel:=False;
                                     Error(E.Message,Caption);
                                     end;
                           end;

                           Plugin.Enter;
                           try     if      Plugin.IsActive then begin
                                           PluginTimerTimer(Plugin);
                                           PluginTimer.Enabled:=True;
                                           MainForm.BusyMode:=True;
                                           end
                                   else if TPluginButtonState(Button.Tag)=pbsCancel then
                                           SetPluginButtonState(Button,pbsRun);
                           finally Plugin.Leave;
                                   if ToolsForm.Visible then ToolsForm.ShowStatus;
                           end;

                           if Screen.ActiveForm=ToolsForm then
                              if      (Plugin is TOptimizerPlugin) and
                                      (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer) and
                                      (ToolsForm.ActiveControl=ToolsForm.BtnOptimizeGames) and
                                      ToolsForm.OptimizeSolutionsGroupBox.Visible then
                                      ToolsForm.OptimizerTaskQueue.StringGrid.SetFocus  // focus the task queue
                              else if (Plugin is TSolverPlugin) and
                                      (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver) and
                                      (ToolsForm.ActiveControl=ToolsForm.BtnSolveLevel) and
                                      ToolsForm.SolveLevelsGroupBox.Visible then
                                      ToolsForm.SolverTaskQueue.StringGrid.SetFocus     // focus the task queue
                              else if (Plugin is TGenerator) and
                                      (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator) and
                                      (ToolsForm.ActiveControl=ToolsForm.BtnGenerateLevels) and
                                      ToolsForm.GenerateLevelsGroupBox.Visible then
                                      ToolsForm.GeneratorTaskQueue.StringGrid.SetFocus; // focus the task queue
                           end;
     end; // case
end;

procedure TOpenForm.SetPluginButtonState(Button__:TButton; PluginButtonState__:TPluginButtonState);
var s:String; Plugin:TPlugin;
begin
  with Button__ do begin
    if        Button__=BtnSolveLevel               then Plugin:=MainForm.Solver
    else if   Button__=BtnOptimizeGames            then Plugin:=MainForm.Optimizer
    else if   Button__=ToolsForm.BtnGenerateLevels then Plugin:=MainForm.Generator
         else Plugin:=nil;
    if Assigned(Plugin) then begin
       Plugin.Enter;
       try
         if (PluginButtonState__=pbsTerminating) and (Tag=Ord(pbsRun)) then begin
            // it's only the main-thread that sets the state to 'Terminating'
            // (when the user clicks 'Cancel');
            // if the state now is 'Run', then the plugin returned
            // normally before the main-thread was allowed to change the state
            // to 'Terminating', so there is nothing to terminate now
            end
         else begin
            Tag:=Ord(PluginButtonState__);
            if      Button__=BtnSolveLevel then begin // solver
                    Caption:=SolverButtonText[PluginButtonState__];
                    Hint:=HintSolverButton[PluginButtonState__];
                    end
            else if Button__=BtnOptimizeGames then begin // optimizer
                    Caption:=OptimizerButtonText[PluginButtonState__];
                    Hint:=HintOptimizerButton[PluginButtonState__];
                    end
            else if Button__=ToolsForm.BtnGenerateLevels then begin // generator
                    Caption:=GeneratorButtonText[PluginButtonState__];
                    Hint:=HintGeneratorButton[PluginButtonState__];
                    end
            else raise Exception.Create('Open1_.TOpenForm.SetPluginButtonState: '+InternalErrorText); // currently, the only implemented plugin types are solvers, optimizers, and generators
            if Plugin<>MainForm.Generator then begin
               DiskGroupBox.Visible:=PluginButtonState__=pbsRun;
               BtnCancel1.Enabled:=DiskGroupBox.Visible;
               if   PluginButtonState__=pbsRun then begin
                    BtnPluginSettings.Hint:=HintPluginSettings1Text;
                    if Assigned(ToolsForm) and ToolsForm.Visible then begin
                       Button__.Enabled:=Assigned(Plugin) and Plugin.IsLoaded and ToolsForm.PluginLevelInfo.IsALegalLevel;
                       s:=ToolsForm.PluginLevelInfo.LevelName;
                       end
                    else begin
                       Button__.Enabled:=Assigned(Plugin) and Plugin.IsLoaded and (MainForm.Game.FileName<>'') and  (MainForm.Game.GameState<>gsNull);
                       s:=MainForm.Game.FileName;
                       end;
                    s:=StrWithQuotedAmpersands(VisualFileName(s));
                    PluginLevelFileNamePanel.Caption:=SPACE+s+SPACE;
                    ShowPluginCaption(Plugin.PluginTypeText,s);
                    end
               else BtnPluginSettings.Hint:=HintPluginSettings2Text;
               end
            else
               if Assigned(ToolsForm) and ToolsForm.Visible then begin
                  if   PluginButtonState__=pbsRun then begin
                       ToolsForm.BtnGeneratorSettings.Hint:=HintGeneratorSettings1Text;
                       Button__.Enabled:=True;
                       end
                  else ToolsForm.BtnGeneratorSettings.Hint:=HintGeneratorSettings2Text;
                  end;
            end;

         if Assigned(ToolsForm) then begin
            if      Plugin is TSolverPlugin then begin
                    ToolsForm.BtnSolveLevel       .Caption :=BtnSolveLevel.Caption;
                    ToolsForm.BtnSolveLevel       .Hint    :=BtnSolveLevel.Hint;
                    ToolsForm.BtnSolverSettings   .Hint    :=BtnPluginSettings.Hint;
                    ToolsForm.PluginLevelFileNamePanel.Caption :=PluginLevelFileNamePanel.Caption;
                    end
            else if Plugin is TOptimizerPlugin then begin
                    ToolsForm.BtnOptimizeGames    .Caption :=BtnOptimizeGames.Caption;
                    ToolsForm.BtnOptimizeGames    .Hint    :=BtnOptimizeGames.Hint;
                    ToolsForm.BtnOptimizerSettings.Hint    :=BtnPluginSettings.Hint;
                    ToolsForm.PluginLevelFileNamePanel.Caption :=PluginLevelFileNamePanel.Caption;
                    end
            else if Plugin is TGenerator then begin
                    end;
            end;

         if (Plugin is TOptimizerPlugin) then
            if   Tag=Ord(pbsCancel) then begin // 'True': the plugin is about to start
                 end
            else if Assigned(Plugin.Level) and Assigned(Plugin.Level.SnapshotsAsText) then
                    with Plugin.Level.SnapshotsAsText do
                      if (not IsEmpty) and
                         (Last is TExtendedSnapshotAsText) then
                         with TExtendedSnapshotAsText(Last) do begin
                           end;

       finally Plugin.Leave;
       end;
       end;
    end;
end;

procedure TOpenForm.ShowSolverMovesAndPushes(PluginLevelStringGrid__:TStringGrid; MoveCount__,PushCount__:Integer);
begin
  with PluginLevelStringGrid__ do begin
     if   MoveCount__<>0 then Cells[1,Ord(pcbiiMoves )]:=IntToStr(MoveCount__)
     else Cells[1,Ord(pcbiiMoves)]:='';
     if   PushCount__<>0 then Cells[1,Ord(pcbiiPushes)]:=IntToStr(PushCount__)
     else Cells[1,Ord(pcbiiPushes)]:='';
     end;
end;

procedure TOpenForm.SynchronizedPluginCallback(Plugin__:TPlugin; PluginSucceeded__,IsIdle__:Boolean);
var i,j,MoveCount,PushCount:Integer;
    b,Edit,IsToolsFormPluginLevel,IsSnapshotOK:Boolean;
    Ch:Char; p, q : PChar;
    RestoreHijackedTaskSerialNo,SnapshotSerialNo:TSerialNo;
    PluginTimeAsText,s:String; Direction:TDirection;
    PrimaryMetric,SecondaryMetric:TGameMetrics;
    SavedUpperBoundMetrics:TUpperBoundMetrics;
    Node:TNode; ALevel{,OldPreviousLevel},OptimizeLevel:TLevel;
    Snapshot2:TSnapshotAsText;
    OptimizeSnapshot,OriginalSnapshot,Snapshot:TExtendedSnapshotAsText;
    StringGrid:TStringGrid;
    OldActiveControl:TWinControl;
begin {precondition: must only be called synchronized from the plugin-thread}
  if IsIdle__ then begin
     if Assigned(MainForm.Solver) and Assigned(MainForm.Optimizer) and Assigned(MainForm.Generator) then begin
        Plugin__.StartTimeMS:=0; // signals that the plugin is inactive
        PluginTimer.Enabled:=(MainForm.Solver.StartTimeMS<>0) or (MainForm.Optimizer.StartTimeMS<>0) or (MainForm.Generator.IsActive);
        SetPluginButtonState(Plugin__.Button,pbsRun);
        if      Plugin__ is TOptimizerPlugin then begin
                ToolsForm.OptimizerTaskQueue.HighlightedRowNumber:=-1;
                ToolsForm.OptimizerTaskQueue.RestoreHijackedTask;
                ToolsForm.OptimizerTaskQueue.ClearAlternateOptimizationTasks; // reset alternating optimization tasks
                end
        else if Plugin__ is TSolverPlugin then
                ToolsForm.SolverTaskQueue.HighlightedRowNumber:=-1
        else if Plugin__ is TGenerator then begin
                ToolsForm.GeneratorTaskQueue.HighlightedRowNumber:=-1;
                if Assigned(GeneratorForm) and (not GeneratorForm.Visible) then
                   GeneratorForm.SaveData; // if the user changed fitness function during processing then sort the candidates according to the new function now
                MainForm.Generator.InitializePositions(0);
                if Assigned(ToolsForm) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator) then begin
                   for i:=0 to Pred(ToolsForm.PluginLevelStringGrid.RowCount) do ToolsForm.PluginLevelStringGrid.Cells[1,i]:='';
                   end;
                end;
        {
        if   (Plugin__=MainForm.Optimizer) and
             Assigned(ToolsForm) and
             ToolsForm.Visible and
             (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer) and
             ToolsForm.PluginLevelInfo.IsALegalLevel and
             (ToolsForm.PluginLevelInfo.NewGamesCount>0) and
             (not ToolsForm.Modified) and
             (not MainForm.Modified) and
             (not ToolsForm.Editor.Selection.Enabled) and
             (not IsANewFileName(ToolsForm.Editor.FileName))
             then begin
             // update the level with new solutions so the optimizer
             // works with the best found solutions
             MainForm.Solver.ImportGames;
             MainForm.Optimizer.ImportGames;
             ToolsForm.PluginLevelInfo.NewGamesCount:=0;
             end;
        }
        if   Visible and (PluginForTask(Task)=Plugin__) then
             EnableDisablePluginControls(Self,Plugin__,False)
        else EnableDisablePluginControls(ToolsForm,Plugin__,False);
        if   Assigned(ToolsForm) and ToolsForm.Visible then ToolsForm.ShowStatus;
        MainForm.BusyMode:=False;
        end;
     end
  else begin
     PluginTimerTimer(Plugin__);
     //if PluginSucceeded__ then
     //   Msg('Ok',TitleWithOptionalSubTitle(Application.Title,Plugin__.PluginName),0);

//   if   Assigned(Plugin__) then begin
//        OldPreviousLevel:=Plugin__.PreviousLevel; Plugin__.PreviousLevel:=nil; // reset the previous level, if any, but keep a local copy for this procedure
//        end
//   else OldPreviousLevel:=nil;

     if Assigned(OpenForm) and
        Assigned(ToolsForm) and
        Assigned(Plugin__) and
        Assigned(Plugin__.Level) then begin

        MoveCount:=0; PushCount:=0; SnapShot:=nil; OriginalSnapshot:=nil; SnapshotSerialNo:=0;

        if   not ((Plugin__ is TOptimizerPlugin) or (Plugin__ is TSolverPlugin)) then begin
             ToolsForm.PluginToolButtonStopReplayClick(nil);
             ToolsForm.ClearPluginReplayInfo;
             end;

        if   Visible then
             if   ((Task=otSolver   ) and (Plugin__ is TSolverPlugin   ))
                  or
                  ((Task=otOptimizer) and (Plugin__ is TOptimizerPlugin)) then
                  StringGrid:=PluginLevelStringGrid
             else StringGrid:=ToolsForm.PluginLevelStringGrid
        else if   ((Plugin__ is TSolverPlugin   ) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver   ))
                  or
                  ((Plugin__ is TOptimizerPlugin) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer)) then
                  StringGrid:=ToolsForm.PluginLevelStringGrid
             else StringGrid:=PluginLevelStringGrid;

        if (Plugin__ is TOptimizerPlugin) and
           (not Plugin__.Level.SnapshotsAsText.IsEmpty) and
           (Plugin__.Level.SnapshotsAsText.Last is TExtendedSnapshotAsText) then begin
           OriginalSnapshot:=TExtendedSnapshotAsText(Plugin__.Level.SnapshotsAsText.Last);
           end;

        with StringGrid do begin
          if   Plugin__.PluginResult=prOK then begin

               if (not Plugin__.Level.SnapshotsAsText.IsEmpty) and
                  (Plugin__.Level.SnapshotsAsText.First is TExtendedSnapshotAsText) then begin // 'True': there is at least one snapshot, and the first one is a 'TExtendedSnapshotAsText'
                  if   Plugin__ is TOptimizerPlugin then begin
                       if Assigned(Plugin__.Level.SnapshotsAsText.First.Next) then // 'True': there is more than one snapshot on the list, i.e., the original snapshot is not the only one
                          Snapshot:=TExtendedSnapshotAsText(Plugin__.Level.SnapshotsAsText.First);
                       end
                  else Snapshot:=TExtendedSnapshotAsText(Plugin__.Level.SnapshotsAsText.First);
                  end;

               s:=Plugin__.MovesAsText;
               if s<>'' then
                  for i:=1 to Length(s) do begin
                      Ch:=s[i];
                      if CharToDirection(Ch,Direction) then begin
                         Inc(MoveCount);
                         if UpCase(Ch)=Ch then Inc(PushCount); {note that a solver is not required to use capital letters for pushes, so this count may be inaccurate}
                         end;
                      end;
               end
          else if (Plugin__.PluginResult=prUnsolvable) or (Plugin__.PluginResult=prInvalidLevel) then
                  Include(Plugin__.Level.Tag.Flags,ltfUnsolvable);

          ShowSolverMovesAndPushes(StringGrid,MoveCount,PushCount);

          if (MoveCount<>0) {and (PushCount<>0)} then begin
             if   (Plugin__.PluginResult=prOK)
                  and
                  ((Plugin__ is TSolverPlugin)
                   or
                   ((Plugin__ is TOptimizerPlugin)
                    and
                    Assigned(Plugin__.PluginStatusInfoPointer)
                    and
                    ((Plugin__.PluginStatusInfoPointer^.Flags and SOKOBAN_PLUGIN_FLAG_SOLUTION)<>0)
                   )
                  ) then
                  s:=SnapshotsForm.SolutionName
             else s:=SnapshotsForm.NormalModeSnapshotName;
             if Assigned(Snapshot) then begin
                if (PushCount<>0) and
                   (Snapshot.SelectedRange[5]=0) then
                   // 'Snapshot.SelectedRange[5]=0':
                   // the snapshot is not an optimization of a solution-slice;
                   // optimizing a slice may require "glue" moves to combine the
                   // slice with the original moves before and after the slice;
                   // these glue moves have not been calculated and inserted in
                   // the solution yet, hence the number of moves cannot be
                   // trusted and should not be exposed as part of the solution
                   // name;
                   s:=s+SPACE+Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount]);

                if Plugin__.AddPluginNameToSolutionTitles then begin
                   s:=s+SPACE+LEFT_PAREN+Plugin__.PluginName;
                   if not Plugin__.HasPluginTypeInName then
                      if   Plugin__ is TSolverPlugin then
                           s:=s+SPACE+SolverText
                      else s:=s+SPACE+OptimizerText;
                   s:=s+RIGHT_PAREN;
                   end;

                Snapshot.SetText(s);
                //Snapshot.Notes.Lines.WriteString(Plugin__.PluginTypeText,Plugin__.PluginName);

                if Assigned(OriginalSnapshot) then begin
                   Node:=Snapshot;
                   while Assigned(Node) and (Node<>OriginalSnapshot) do begin
                     if Node is TExtendedSnapshotAsText then
                        OriginalSnapshot.Notes.CopyTo(TExtendedSnapshotAsText(Node).Notes);
                     Node:=Node.Next;
                     end;
                   end;

                Snapshot.Notes.Lines.WriteString(MovesText,IntToStr(MoveCount));
                if   PushCount<>0 then
                     Snapshot.Notes.Lines.WriteString(PushesText,IntToStr(PushCount))
                else Snapshot.Notes.Lines.DeleteKey(PushesText);
                end;
             end;

          PluginTimeAsText:=FormatTimeMS(Plugin__.PluginTimeMS);
          Cells[1,Ord(pcbiiTime)]:=PluginTimeAsText;

          s:='';
          if Assigned(Plugin__.PluginStatusInfoPointer) then
             with Plugin__.PluginStatusInfoPointer^ do begin
               if (Plugin__.PluginResult=prOK) or (Plugin__.PluginResult=prGameTooLong) then begin
                  {optimality info to text}
                  i:=Plugin__.PluginStatusInfoPointer^.Flags;
                  if (i and SOKOBAN_PLUGIN_FLAG_MOVES)<>0 then begin {move-optimal solution}
                     s:=AnsiLowerCase(MoveOptimalSolutionText);
                     if (i and (SOKOBAN_PLUGIN_FLAG_PUSHES+SOKOBAN_PLUGIN_FLAG_SECONDARY_PUSHES))=SOKOBAN_PLUGIN_FLAG_SECONDARY_PUSHES then
                        s:=s+', '+AnsiLowerCase(MinimumPushesText);
                     end;
                  if (i and SOKOBAN_PLUGIN_FLAG_PUSHES)<>0 then begin {push-optimal solution}
                     if   s<>'' then
                          s:=s+', '+AnsiLowerCase(PushOptimalSolutionText)
                     else s:=PushOptimalSolutionText;
                     if (i and (SOKOBAN_PLUGIN_FLAG_MOVES+SOKOBAN_PLUGIN_FLAG_SECONDARY_MOVES))=SOKOBAN_PLUGIN_FLAG_SECONDARY_MOVES then
                        s:=s+', '+AnsiLowerCase(MinimumMovesText);
                     end;

                  {import plugin settings from the text, if any; this is an
                   undocumented and unofficial feature; the format is:
                   if Ord( next to last char in text ) is a null-terminator
                      and
                      Ord( last char in text ) is an index into the text
                      and
                      the text at that index is 'Settings:', case sensitive
                      then
                      the text at that index is assumed to be plugin settings;
                  }
                  p := Addr( Plugin__.PluginStatusInfoPointer^.StatusText[ High( Plugin__.PluginStatusInfoPointer^.StatusText ) ] );
                  i := Ord( p^ );
                  if ( i > 0 ) and ( i < High( Plugin__.PluginStatusInfoPointer^.StatusText ) ) then begin
                     Dec( p );
                     if p^ = NULL_CHAR then begin
                        p := Addr( Plugin__.PluginStatusInfoPointer^.StatusText[ Low( Plugin__.PluginStatusInfoPointer^.StatusText ) ] );
                        Inc( p, i );
                        j := Length( KEY_SETTINGS );
                        if StrLComp( p, KEY_SETTINGS, j ) = 0 then begin // 'True': found the 'Settings' key text
                           Inc( p, j ); // past the settings key text; look for ':';
                           if p^ = COLON then begin // 'True': found 'Settings:'; assume that the plugin settings follows;
                              q := StrEnd( p );
                              repeat Dec( q );
                              until ( p = q ) or ( q^ > SPACE ); // right-trim text
                              Inc( q );
                              q^:= NULL_CHAR; // add null-terminator after text
                              repeat Inc( p );
                              until ( p = q ) or ( p^ > SPACE ); // left-trim text
                              if p <> q then
                                 Snapshot.Notes.Lines.WriteString( Plugin__.PluginTypeText + SPACE + Lowercase( KEY_SETTINGS ), StrNew( p ) );
                              end;
                           end;
                        end;
                     end;
                  end;

               {statistics}
               //if   Moves    <>0 then Cells[1,Ord(scbiiMoves )]:=IntToStr(Moves)  // solution length is shown instead of statistics
               //else Cells[1,Ord(scbiiMoves)]:='';
               //if   Pushes   <>0 then Cells[1,Ord(scbiiPushes)]:=IntToStr(Pushes) // solution length is shown instead of statistics
               //else Cells[1,Ord(scbiiPushes)]:='';
               if   StatesGenerated<>0 then
                    if   (StatesGenerated>=ONE_MILLION) and (High(StatesGenerated)-StatesGenerated>=HALF_MILLION) then
                         Cells[1,Ord(pcbiiStates)]:=IntToStr((StatesGenerated+HALF_MILLION) div ONE_MILLION)+MillionSuffixText
                    else Cells[1,Ord(pcbiiStates)]:=IntToStr(StatesGenerated)
               else Cells[1,Ord(pcbiiPushes)]:='';
               end;

          if   s<>'' then begin
               if   Assigned(Snapshot) then Snapshot.Notes.Lines.WriteString(TEXT_OPTIMALITY,s);
               if   Plugin__.PluginResult=prOK then
                    s:=PluginResultText[Plugin__.PluginResult]+', '+AnsiLowerCase(s)
               else s:=PluginResultText[Plugin__.PluginResult];
               if   (Plugin__.PluginResult<>prTimeOut) and
                    (Plugin__.PluginThread.PluginCallBackFunctionResult=prTimeOut) then
                    s:=s+SPACE+StrWithParenthesis(PluginResultText[prTimeOut]);
               Cells[1,Ord(pcbiiStatus)]:=s;
               end
          else if      Plugin__ is TSolverPlugin then begin
                       s:=PluginResultText[Plugin__.PluginResult];
                       if   (Plugin__.PluginResult<>prTimeOut) and
                            (Plugin__.PluginThread.PluginCallBackFunctionResult=prTimeOut) then
                            s:=s+SPACE+StrWithParenthesis(PluginResultText[prTimeOut]);
                       Cells[1,Ord(pcbiiStatus)]:=s;
                       end
               else if Plugin__ is TOptimizerPlugin then
                       if   Plugin__.PluginThread.PluginCallBackFunctionResult<>prTimeOut then
                            if        Plugin__.PluginResult=prOK then
                                      Cells[1,Ord(pcbiiStatus)]:=OKChangedText[False]
                            else if   Plugin__.PluginResult     =prUnsolved then
                                      Cells[1,Ord(pcbiiStatus)]:=NoOptimizationsFoundText
                                 else Cells[1,Ord(pcbiiStatus)]:=PluginResultText[Plugin__.PluginResult]
                       else           Cells[1,Ord(pcbiiStatus)]:=PluginResultText[prTimeOut]
               else Cells[1,Ord(pcbiiStatus)]:=PluginResultText[Plugin__.PluginResult];

          IsToolsFormPluginLevel:=False;
          with Plugin__ do
            if Assigned(Level) and Assigned(Level.BoardAsTextLines) and (not Level.BoardAsTextLines.IsEmpty) then
               with Level do with Tag do begin
                 IsToolsFormPluginLevel:=ToolsForm.Visible and ToolsForm.IsToolsFormPluginLevel(BoardWidth,BoardHeight,BoardAsTextLines.First.Text,Name);
                 if not IsToolsFormPluginLevel then begin
                    // clear the statistics if the window contains another level
                    // than the one that has been processed by the plugin
                    // for i:=0 to Pred(RowCount) do Cells[1,i]:='';
                    end;
                 if StatisticsEnabled
                    and
                    (True
                     or
                     (Plugin__.PluginResult<>prTerminatedByUser)
                    ) then
                    if      (Plugin__ is TOptimizerPlugin) and Assigned(ToolsForm.OptimizerTaskQueue) then begin
                            if (Plugin__.PluginResult=prOK) or Plugin__.Level.SnapshotsAsText.IsEmpty then begin
                               i:=MoveCount; j:=PushCount;
                               end
                            else with TExtendedSnapshotAsText(Plugin__.Level.SnapshotsAsText.First) do begin
                               i:=MoveCount; j:=PushCount; // use the original moves/pushes for the statistics
                               end;
                            ToolsForm.OptimizerTaskQueue.AddStatistics(Plugin__.PluginName,Plugin__.PluginTypeText,VisualFileName(Level.Name),CalculateElapsedTimeMS(Plugin__.StartTimeMS,GetTimeMS),Plugin__.PluginResult,i,j,Cells[1,Ord(pcbiiStatus)])
                            end
                    else if (Plugin__ is TSolverPlugin   ) and Assigned(ToolsForm.SolverTaskQueue   ) then
                            ToolsForm.SolverTaskQueue   .AddStatistics(Plugin__.PluginName,Plugin__.PluginTypeText,VisualFileName(Level.Name),CalculateElapsedTimeMS(Plugin__.StartTimeMS,GetTimeMS),Plugin__.PluginResult,MoveCount,PushCount,Cells[1,Ord(pcbiiStatus)]);
                 end;

          PanelToolTips.Caption:=Cells[1,Ord(pcbiiStatus)];
          with PanelToolTips do Width:=Canvas.TextWidth(Caption)+8;
          with ToolsForm.PanelToolTips do begin
            Caption:=Cells[1,Ord(pcbiiStatus)];
            Width:=ToolsForm.Canvas.TextWidth(Caption)+8;
            end;

          RestoreHijackedTaskSerialNo:=ToolsForm.OptimizerTaskQueue.AlternateOptimization.HijackedTaskSerialNo;

          if Assigned(Snapshot) then begin
             Snapshot2:=Snapshot;
             repeat  with Snapshot2.Notes.Lines do begin
                       s:=Plugin__.PluginTypeText;
                       WriteString(s,Plugin__.PluginName);
                       WriteString(s+SPACE+AnsiLowercase(TimeText),PluginTimeAsText);
                       if Plugin__.AddDateToSolutionNotes then
                          WriteString(s+SPACE+AnsiLowercase(DateText),FormatDateTime(FORMAT_DATE_TIME,Now));
                       if Plugin__ is TOptimizerPlugin then begin
                          if FindKey(TEXT_OPTIMALITY,Node) then Remove(Node,True); // generally, it's impossible or too complex to handle optimality information correctly
                          if OriginalSnapshot.OptimizationFlags<>0 then
                             WriteString(OptimizerText+SPACE+AnsiLowercase(MetricsText), OptimizationFlagsAsText(OriginalSnapshot.OptimizationFlags));
                          end;
                       end;
                     Snapshot2:=TSnapshotAsText(Snapshot2.Next);
             until   (not (Plugin__ is TOptimizerPlugin)) or // currently it's only the optimizer that may return more than one solution
                     (not Assigned(Snapshot2)) or
                     (Snapshot2=OriginalSnapshot); // 'OriginalSnapshot': the original solution is the last one on the list
             Plugin__.SokoFile.Modified:=True;
             //Plugin__.SokoFile.SaveToFile(MainForm.ApplicationDataPath+'t1.sok');

             // check if the optimization should be repeated with alternating metrics
             IsSnapshotOK:=False; // 'False' means 'undefined' here; it doesn't mean that the snapshot is invalid

             if (Plugin__ is TOptimizerPlugin) and
                Assigned(OriginalSnapshot) and
                (OriginalSnapshot.Metrics.PushCount>0) then begin  // '>0': the original snapshot isn't empty
                IsSnapshotOK:=Plugin__.ReplayGame(Plugin__.Level,nil,nil,False); // check that the snapshot is valid and calculate its metrics
                if IsSnapshotOK then begin
                   Snapshot.SelectedRange[5]:=0; // '0': any player moves after an optimized slice (before the first push after the slice) have been recalculated at this point by 'ReplayGame'
                   with Snapshot do with Metrics do with SecondaryMetrics do // the level contains an optimized game; make a textual representation of the improvement
                     ImprovementAsText:=Format('  (%d/%d/%d/%d/%d/%d)',
                                               [MoveCount      -OriginalSnapshot.Metrics.MoveCount,
                                                PushCount      -OriginalSnapshot.Metrics.PushCount,
                                                BoxLines       -OriginalSnapshot.Metrics.SecondaryMetrics.BoxLines,
                                                BoxChanges     -OriginalSnapshot.Metrics.SecondaryMetrics.BoxChanges,
                                                PushingSessions-OriginalSnapshot.Metrics.SecondaryMetrics.PushingSessions,
                                                PlayerLines    -OriginalSnapshot.Metrics.SecondaryMetrics.PlayerLines
                                               ]);

                   if ToolsForm.SettingsMenuAlternatingOptimizationsEnabled.Checked or
                      ToolsForm.OptimizerTaskQueue.AlternateOptimization.PartitionSolutionIntoSubIntervals then begin
                      ShowSolverMovesAndPushes(StringGrid,Snapshot.Metrics.MoveCount,Snapshot.Metrics.PushCount); // show the correct number of moves and pushes; solvers and optimizers are not required to return correct upper/lowercase characters for the moves
                      if Snapshot.Metrics.PushCount>0 then begin // '>0': the snapshot isn't empty
                         SnapshotSerialNo:=Snapshot.SerialNo;

                         if   ToolsForm.OptimizerTaskQueue.AlternateOptimization.PartitionSolutionIntoSubIntervals then
                              ToolsForm.OptimizerTaskQueue.RegisterResultForPartitioningSolutionIntoSubIntervals(OriginalSnapshot,Snapshot)
                         else ToolsForm.OptimizerTaskQueue.RegisterResultForAlternatingOptimizationTasks(OriginalSnapshot,Snapshot,0);

                         // find the new snapshot if it still is on the task queue; it may have disappeared, e.g., if it was a duplicate
                         ALevel:=Plugin__.SokoFile.GetLevelByExtendedSnapshotAsTextSerialNo(SnapshotSerialNo);
                         if   Assigned(ALevel) then
                              Snapshot:=ALevel.GetExtendedSnapshotAsTextBySerialNo(SnapshotSerialNo)
                         else Snapshot:=nil;
                         end;
                      end;
                   end
                else if ToolsForm.OptimizerTaskQueue.AlternateOptimization.PartitionSolutionIntoSubIntervals then
                        ToolsForm.OptimizerTaskQueue.RegisterResultForPartitioningSolutionIntoSubIntervals(OriginalSnapshot,nil);
                end;

             if      ToolsForm.Visible and Assigned(Snapshot) then with Plugin__ do begin
                     if IsSnapshotOK or Snapshot.IsASolution or ReplayGame(Level,nil,nil,False) then begin
                        {
                        if (Plugin__ is TSolverPlugin) and
                           (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver) and
                           (Plugin__.Level=ToolsForm.SolverTaskQueue.Levels[Plugin__.Level.Flags]) then with ToolsForm.SolverTaskQueue do begin
                           Levels[Plugin__.Level.Flags]:=nil;
                           Refresh(True);
                           end
                        else
                        }  begin
                           if IsToolsFormPluginLevel then begin
                              ShowSolverMovesAndPushes(StringGrid,Snapshot.Metrics.MoveCount,Snapshot.Metrics.PushCount); // show the correct number of moves and pushes; solvers and optimizers are not required to return correct upper/lowercase characters for the moves
                              if      Snapshot.ImprovementAsText<>'' then with Snapshot do with Metrics do with SecondaryMetrics do begin // 'True': this level contains an optimized game
                                      s:=Format('%d/%d/%d/%d/%d/%d',
                                                [MoveCount,PushCount,BoxLines,BoxChanges,PushingSessions,PlayerLines])+ImprovementAsText;
                                      if   Plugin__.PluginThread.PluginCallBackFunctionResult=prTimeOut then
                                           //s:=s+SPACE+StrWithParenthesis(PluginResultText[prTimeOut]);
                                           s:=PluginResultText[prTimeOut]+COLON+SPACE+s;
                                      Cells[1,Ord(pcbiiStatus)]:=s;
                                      end;

                              if      Snapshot.IsASolution then begin
                                      if Assigned(Plugin__.PluginStatusInfoPointer) and
                                         ((Plugin__.PluginStatusInfoPointer^.Flags and SOKOBAN_PLUGIN_FLAG_SOLUTION)=0) and
                                         (Plugin__ is TOptimizerPlugin) then // change the name from 'Snapshot' to 'Solution'
                                         Snapshot.SetText(StrSubstitute1(Snapshot.Name,SnapshotsForm.NormalModeSnapshotName,SnapshotsForm.SolutionName,i));
                                      if Snapshot.Metrics.MoveCount<>0 then
                                         Inc(ToolsForm.PluginLevelInfo.NewGamesCount) // new solution for the current level: enable the 'Save level" button in the "Tools' window
                                      else begin
                                         Cells[1,Ord(pcbiiMoves )]:=IntToStr(0);
                                         Cells[1,Ord(pcbiiPushes)]:=IntToStr(0);
                                         end;
                                      end
                              else if Plugin__ is TSolverPlugin then
                                      Cells[1,Ord(pcbiiStatus)]:=PluginResultText[prFailed];

                              if   Snapshot.Metrics.MoveCount<>0 then begin
                                   if (Plugin__ is TSolverPlugin)
                                      and
                                      (ToolsForm.PageControl1.ActivePage<>ToolsForm.TabSheetOptimizer)
                                      and
                                      (ToolsForm.PageControl1.ActivePage<>ToolsForm.TabSheetGenerator)
                                      and
                                      (
                                       ((not ToolsForm.SolveLevelsGroupBox.Visible)
                                        and
                                        StrEqual(ToolsForm.PluginLevelInfo.LevelName,ToolsForm.Editor.FileName)
                                       )
                                       or
                                       (ToolsForm.SolveLevelsGroupBox.Visible
                                        and
                                        (ToolsForm.SolverTaskQueue.StringGrid.Row=Plugin__.Level.Flags)
                                        and
                                        (Plugin__.Level=ToolsForm.SolverTaskQueue.Levels[Plugin__.Level.Flags])
                                       )
                                      )
                                      then with ToolsForm.PluginLevelInfo do begin
                                      ReplayInfo.BoardAsText.Board :=BoardAsText.Board; // set up the replay game board state
                                      ReplayInfo.BoardAsText.Height:=BoardAsText.Height;
                                      ReplayInfo.BoardAsText.Width :=BoardAsText.Width;
                                      ReplayInfo.MovesAsText       :=Snapshot.MovesAsTextLines.First.Text; // save the moves
                                      ReplayInfo.PluginFileName    :=Plugin__.PluginFileName;
                                      end;
                                   end
                              else Include(Plugin__.Level.Tag.Flags,ltfProcessed); // signals that the plugin-thread should destroy the level
                              end
                           else
                              if (Plugin__ is TSolverPlugin) and
                                 (ToolsForm.SolverTaskQueue.StringGrid.Row=Plugin__.Level.Flags) and
                                 (Plugin__.Level=ToolsForm.SolverTaskQueue.Levels[Plugin__.Level.Flags]) and
                                 (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver)
                                 then with ToolsForm do begin
                                 SolveLevelsStringGridSelectCell(nil,0,SolverTaskQueue.StringGrid.Row,b);
                                 if b then ToolsForm.PluginLevelInfo.ReplayInfo.PluginFileName:=Plugin__.PluginFileName;
                                 end;
                           end;
                        end
                     else begin // invalid or useless data from the plugin; for instance, after optimizing an interval of pushes, it may not be possible to "glue" the returned slice together with the moves before and after the optimized slice;
                       if (Plugin__ is TOptimizerPlugin) then begin
                          Plugin__.Level.SnapshotsAsText.Remove(Snapshot,True); // destroy the invalid snapshot
                          end
                       else Include(Plugin__.Level.Tag.Flags,ltfProcessed); // signals that the plugin-thread should destroy the level
                       end;
                     end
             else if Self.Visible and (Task<>otGame) then
                     if   Assigned(Snapshot) then begin
                          if (not (ltfSelected in Plugin__.Level.Tag.Flags)) and
                             (Snapshot.OptimizationFlags=0) then with Plugin__ do begin
                             Include(Level.Tag.Flags,ltfProcessed);
                             if ImportGame(Level) and HasThread then
                                PluginThread.SynchronizedSavePluginResultToLogFile;
                             end
                          end
                     else if not (ltfSelected in Plugin__.Level.Tag.Flags) then
                             Include(Plugin__.Level.Tag.Flags,ltfProcessed);
             end
          else if (Plugin__ is TOptimizerPlugin) and
                  Assigned(OriginalSnapshot) and
                  ToolsForm.OptimizerTaskQueue.AlternateOptimization.PartitionSolutionIntoSubIntervals then begin
                  ToolsForm.OptimizerTaskQueue.RegisterResultForPartitioningSolutionIntoSubIntervals(OriginalSnapshot,nil);
                  SnapshotSerialNo:=OriginalSnapshot.SerialNo; // avoid launching "alternate optimizations" after the last subinterval has been optimized
                  end;

          if Plugin__ is TOptimizerPlugin then begin
             if not ToolsForm.OptimizerTaskQueue.AlternateOptimization.PartitionSolutionIntoSubIntervals then begin
                if RestoreHijackedTaskSerialNo=ToolsForm.OptimizerTaskQueue.AlternateOptimization.HijackedTaskSerialNo then // 'True': the alternate optimization module didn't restore the hijacked task, if any; do it now
                   ToolsForm.OptimizerTaskQueue.RestoreHijackedTask;
                if Assigned(OriginalSnapshot) and (SnapshotSerialNo=0) then // 'True': the optimizer's alternating tasks haven't been updated; one of the reasons can be that the plugin didn't return a new improved snapshot
                   ToolsForm.OptimizerTaskQueue.RegisterResultForAlternatingOptimizationTasks(OriginalSnapshot,nil,0);
                end;
             end;

          if ToolsForm.Visible then begin

             if   Self.Visible
                  and
                  (StringGrid=ToolsForm.PluginLevelStringGrid)
                  and
                  (((Plugin__ is TSolverPlugin   ) and (ToolsForm.PageControl1.ActivePage<>ToolsForm.TabSheetSolver))
                   or
                   ((Plugin__ is TOptimizerPlugin) and (ToolsForm.PageControl1.ActivePage<>ToolsForm.TabSheetOptimizer))
                  )
                  then begin
                  // the status information was sent to 'ToolsForm.PluginLevelStringGrid'
                  // even though there is a mismatch between the plugin type and
                  // the tabsheet which currently has focus;
                  // "repair" it by clearing the status;
                  for i:=0 to Pred(StringGrid.RowCount) do StringGrid.Cells[1,i]:='';
                  with ToolsForm.PanelToolTips do begin
                    Caption:='';
                    Width:=ToolsForm.Canvas.TextWidth(Caption)+8;
                    if Visible then Hide;
                    end;
                  end;

             if not IsToolsFormPluginLevel then begin
                EnableDisablePluginControls(nil,Plugin__,False);
                end;
             ToolsForm.ShowStatus;
             end;
          end;
        end;
     end;

  with ToolsForm do
    if Visible then begin
       if (PageControl1.ActivePage=TabSheetOptimizer) then with OptimizerTaskQueue do begin
          OldActiveControl  :=ToolsForm.ActiveControl;
          Edit              :=ToolsForm.OptimizationComboBox.Visible;
          try     OptimizeSolutionsStringGridSelectCell(nil,StringGrid.ColCount,StringGrid.Row,b); // kludge: 'ColCount' signals not to reload the current level
                  if Plugin__<>Plugin then begin
                     EnableDisablePluginControls(nil,Plugin,True);
                     ToolsForm.ShowStatus;
                     end;
          finally if Edit then ShowOptimizationComboBox(StringGrid.Row);
                  ToolsForm.ActiveControl:=OldActiveControl;
          end;
          end;
       end;
end;

procedure TOpenForm.PluginTimerTimer(Sender: TObject);
var PluginTimeLimitsEnabled:Boolean; OptimizerTimeMS,SolverTimeMS,GeneratorTimeMS:TTimeMS;
begin
  PluginTimeLimitsEnabled:=False;

  if   Assigned(MainForm.Solver) then with MainForm.Solver do begin
       if   StartTimeMS=0 then SolverTimeMS:=0
       else if not TimeLimitEnabled then
               SolverTimeMS:=CalculateElapsedTimeMS(MainForm.Solver.StartTimeMS,GetTimeMS)
            else begin
               PluginTimeLimitsEnabled:=True;
               SolverTimeMS:=CalculateElapsedTimeMS(MainForm.Solver.StartTimeMS,GetTimeMS);
               if SolverTimeMS>=TimeLimitMS then begin
                  Enter;
                  try     SolverTimeMS:=CalculateElapsedTimeMS(MainForm.Solver.StartTimeMS,GetTimeMS); // a recalculation is necessary; the plugin may have changed to another level
                          if (SolverTimeMS>=TimeLimitMS) and
                             Assigned(PluginThread) and
                             (not TerminatedByUser) and
                             (PluginThread.PluginCallBackFunctionResult=prOK) then
                             PluginThread.PluginCallBackFunctionResult:=prTimeOut;
                  finally Leave;
                  end;
                  end;
               end;
       end
  else SolverTimeMS:=0;

  if   Assigned(MainForm.Optimizer) then with MainForm.Optimizer do begin
       if   StartTimeMS=0 then OptimizerTimeMS:=0
       else if not TimeLimitEnabled then
               OptimizerTimeMS:=CalculateElapsedTimeMS(MainForm.Optimizer.StartTimeMS,GetTimeMS)
            else begin
               PluginTimeLimitsEnabled:=True;
               OptimizerTimeMS:=CalculateElapsedTimeMS(MainForm.Optimizer.StartTimeMS,GetTimeMS);
               if OptimizerTimeMS>=TimeLimitMS then begin
                  Enter;
                   try     OptimizerTimeMS:=CalculateElapsedTimeMS(MainForm.Optimizer.StartTimeMS,GetTimeMS); // a recalculation is necessary; the plugin may have changed to another level
                          if (OptimizerTimeMS>=TimeLimitMS) and
                             Assigned(PluginThread) and
                             (not TerminatedByUser) and
                             (PluginThread.PluginCallBackFunctionResult=prOK) then
                             PluginThread.PluginCallBackFunctionResult:=prTimeOut;
                  finally Leave;
                  end;
                  end;
               end;
       end
  else OptimizerTimeMS:=0;

  if   Assigned(MainForm.Generator) then with MainForm.Generator do begin
       //if   StartTimeMS=0 then GeneratorTimeMS:=0
       if   (not IsActive) then GeneratorTimeMS:=0
       else GeneratorTimeMS:=GACalculateElapsedTimeMS;
       end
  else GeneratorTimeMS:=0;

  if      Visible then begin
          if      (Task=otSolver) and Assigned(MainForm.Solver) then begin
                  if MainForm.Solver.StartTimeMS   <>0 then StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=FormatTimeMS(SolverTimeMS);
                  exit; // quick-and-dirty exit
                  end
          else if (Task=otOptimizer) and Assigned(MainForm.Optimizer) then begin
                  if MainForm.Optimizer.StartTimeMS<>0 then StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=FormatTimeMS(OptimizerTimeMS);
                  exit; // quick-and-dirty exit
                  end;
//        else if (Task=otGenerator) and Assigned(MainForm.Generator) then begin
//                if MainForm.Generator.StartTimeMS<>0 then StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=FormatTimeMS(GeneratorTimeMS);
//                exit; // quick-and-dirty exit
//                end;
          end
  else if Assigned(ToolsForm) and ToolsForm.Visible then
          if      ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver then with ToolsForm do begin
                  if MainForm.Solver.StartTimeMS   <>0 then StatusBar1.Panels[0].Text:=FormatTimeMS(SolverTimeMS);
                  exit; // quick-and-dirty exit
                  end
          else if ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer then with ToolsForm do begin
                  if MainForm.Optimizer.StartTimeMS<>0 then StatusBar1.Panels[0].Text:=FormatTimeMS(OptimizerTimeMS);
                  exit; // quick-and-dirty exit
                  end
          else if ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator  then with ToolsForm do begin
               // if MainForm.Generator.StartTimeMS<>0 then StatusBar1.Panels[0].Text:=FormatTimeMS(GeneratorTimeMS);
                  if MainForm.Generator.IsActive       then StatusBar1.Panels[0].Text:=FormatTimeMS(GeneratorTimeMS);
                  exit; // quick-and-dirty exit
                  end;
  // fall through; disable the timer if none of the plugin timelimits are enabled
  if not PluginTimeLimitsEnabled then begin
     PluginTimer.Enabled:=False;
     StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
     if Assigned(ToolsForm) then ToolsForm.ShowStatus;
     end;
end;

procedure TOpenForm.PluginLevelStringGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var S:String;
begin
  with PluginLevelStringGrid do begin
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

procedure TOpenForm.ShowPluginCaption(const PluginTypeName__,VisualFileName__:String);
var s:String;
begin
  if (Task=otSolver) {or (Task=otOptimizer)} then begin // not fully implemented for the optimizer plugin
     s:=TitleWithOptionalSubTitle(TEXT_SOKOBAN+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+PluginTypeName__,VisualFileName__);
     if s<>Caption then Caption:=s;
     end;
end;

procedure TOpenForm.PluginLevelStringGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var ACol,ARow:Integer;
begin
  ControlMouseMove(nil,Shift,X,Y);
  with PluginLevelStringGrid do begin
    MouseToCell(X,Y,ACol,ARow);
    if   (ACol=1) and (ARow=Ord(pcbiiStatus)) and ToolTips.Enabled and (X<Tag) then
         if Cells[1,Ord(pcbiiStatus)]<>'' then with PanelToolTips do begin
            if Caption<>Cells[1,Ord(pcbiiStatus)] then begin
               if Visible then Hide;
               Caption:=Cells[1,Ord(pcbiiStatus)];
               Width:=Self.Canvas.TextWidth(Caption)+8;
               end;
            Left:=Min(PluginLevelGroupBox.Left+PanelPluginLevelInfo.Left+PluginLevelStringGrid.Left+X+ToolTips.OffsetX,Self.ClientWidth-Width-DiskGroupBox.Left);
            Top :=PluginLevelGroupBox.Top +PanelPluginLevelInfo.Top +PluginLevelStringGrid.Top +Y+ToolTips.OffsetY;
            if not PanelToolTips.Visible then PanelToolTips.Show;
            end
         else if PanelToolTips.Visible then PanelToolTips.Hide
              else
    else if PanelToolTips.Visible then PanelToolTips.Hide;
    end;
end;

function  TOpenForm.PluginForTask(Task__:TOpenTask):TPlugin;
begin
  if        Task__=otSolver    then Result:=MainForm.Solver
  else if   Task__=otOptimizer then Result:=MainForm.Optimizer
//else if   Task__=otGenerator then Result:=MainForm.Generator
       else Result:=nil;
end;

function  TOpenForm.HasSolutionsForTheOptimizer(Sender: TObject):Boolean;
begin
  if  (Sender=ToolsForm) and Assigned(ToolsForm) then
      Result:=ToolsForm.OptimizerTaskQueue.SelectedCount<>0
  else with ToolsForm.OptimizerTaskQueue do begin
      if SelectedCount=0 then Reload(True);
      Result:=SelectedCount<>0;
      end;
end;

procedure TOpenForm.OnCloseWindow(var Msg: TMessage); // Message Misc_.MSG_CLOSE;
begin
  Close;
end;

procedure TOpenForm.OnMaximize(var Msg:TMessage); //Message MSG_MAXIMIZE;
begin
  DoMaximize:=False;
  if   WindowState =wsMaximized then FormResize(Self)
  else WindowState:=wsMaximized;
end;

(*
procedure TOpenForm.Button1Click(Sender: TObject);
var i,j:Integer; s:String; Level:TLevel; F:TextFile;

  function ExtractName(const Name__:String):String;
  begin
    Result:=StrWithoutDoubleQuotes(Copy(Name__,1,Pred(System.Pos(COMMA,Name__))));
  end;

begin {$I-}
  if (Game<>nil) and (Game.SokoFile<>nil) then with Game.SokoFile do begin
     Level:=TLevel(Levels.First); j:=0;
     if IOResult=0 then;
     while Level<>nil do with Level do begin
       Inc(j); s:=IntToStr(j); while Length(s)<3 do s:='0'+s;
       //s:='c:\temp\'+s+'-'+ExtractName(Name)+'.scr';
       s:='c:\temp\screen.'+IntToStr(j);

       System.AssignFile(F,s);
       System.Rewrite(F);
       i:=IOResult;
       if i=0 then begin
          BoardAsTextLines.WriteToFile(Addr(F));
          System.CloseFile(F);
          end
       else begin
          Error('IO-Error '+IntToStr(i),'');
          break;
          end;

       Level:=TLevel(Next);
       end;
     end;

end; {$I+}
*)

procedure TOpenForm.Junk(Sender: TObject);
var i:Integer; s:String;
begin
  with FileListBox1 do
    for i:=0 to Pred(Items.Count) do begin
        s:=''+IntToStr(1+i)+'.jpg';
        BtnOpen.Caption:=s;
        if not RenameFile(Items[i],s) then break;
        end;
end;

function  TOpenForm.CanUnpackFile(const FileName__:String):Boolean;
begin
  Result:=(StrEqual(ExtractFileExt(FileName__),ZIP_FILE_EXT) and FileExists(StrWithTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+UNZIP_PROGRAM_NAME))
          or
          (StrEqual(ExtractFileExt(FileName__),RAR_FILE_EXT) and FileExists(StrWithTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+UNRAR_PROGRAM_NAME))
end;

function  TOpenForm.UnpackFile(const FileName__:String):Boolean;
var
  CountOK,CountFailed:Integer; oShowErrorMessages:TShowErrorMessages;
  FirstFailedFileName,oCurrentDir,UnpackDirectory:String; oCursor:TCursor;
  Node:TNode; DirectoryList:SokUtil_.TList;

  function  UnpackFile(const FileName__:String; var NewDirectoryName__:String):Boolean;
  var NewFileName:String;
  begin
    NewDirectoryName__:='';
    if   FileExists(FileName__) then begin
         NewDirectoryName__:=MakeNewFileName(FileName__,SPACE,True); // 'SPACE' produce a filename without an extension
         if   NewDirectoryName__<>'' then begin
              try    ForceDirectories(NewDirectoryName__);
              except on E:Exception do;
              end;
              if   DirectoryExists(NewDirectoryName__) then begin
                   NewFileName:=StrWithTrailingPathDelimiter(NewDirectoryName__)+ExtractFileName(FileName__);
                   if   Windows.CopyFile(PChar(FileName__),PChar(NewFileName),True) then begin
                        SetCurrentDir(NewDirectoryName__);
                        if      StrEqual(ExtractFileExt(NewFileName),ZIP_FILE_EXT) then begin
                                Result:=ExecuteAndWait(StrWithTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+UNZIP_PROGRAM_NAME+SPACE+DOUBLE_QUOTE+NewFileName+DOUBLE_QUOTE,Windows.SW_NORMAL,Windows.INFINITE)=0;
                                end
                        else if StrEqual(ExtractFileExt(NewFileName),RAR_FILE_EXT) then begin
                                Result:=ExecuteAndWait(StrWithTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+UNRAR_PROGRAM_NAME+' x '+DOUBLE_QUOTE+NewFileName+DOUBLE_QUOTE,Windows.SW_NORMAL,Windows.INFINITE)=0;
                                end
                        else raise Exception.Create(Format(FileNotCompressedWithRARorZIPText__,[FileName__]));
                        DeleteFile(NewFileName);
                        GetSubDirectories(NewDirectoryName__,'',DirectoryList);
                        if Result then Inc(CountOK)
                        else begin
                           Inc(CountFailed);
                           if FirstFailedFileName='' then FirstFailedFileName:=FileName__;
                           end;
                        end
                   else raise Exception.Create(Format(CopyFileFailedText__,[NewFileName]));
                  end
              else raise Exception.Create(Format(CreateDirectoryFailedText__,[NewDirectoryName__]));
              end
         else raise Exception.Create(Format(CreateDirectoryFailedText__,[ChangeFileExt(FileName__,'')]));
         end
    else raise Exception.Create(Format(TEXT_FILE_NOT_FOUND_FORMAT,[FileName__]));
  end;

  function UnpackFilesInDirectory(const Directory__:String; DirectoryList__:SokUtil_.TList):Boolean;
  var i:Integer; s,s1:String;
  begin
    Result:=True;
    if DirectoryExists(Directory__) then with FileListBox1 do begin
       Mask:=ALL_FILES_FILTER;
       Directory:=Directory__;
       for i:=Pred(Items.Count) downto 0 do begin
           s:=StrWithTrailingPathDelimiter(Directory)+Items[i];
           if (StrEqual(ExtractFileExt(s),ZIP_FILE_EXT)
               or
               StrEqual(ExtractFileExt(s),RAR_FILE_EXT)
              ) then
              if UnpackFile(s,s1) then DeleteFile(s);
           end;
       GetSubDirectories(Directory__,'',DirectoryList__);
       end;
  end;

  function FileConversion(const Directory__:String):Boolean;
  var i:Integer; s,s1:String;
  begin
    Result:=True;
    if DirectoryExists(Directory__) then with FileListBox1 do begin
       StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:=Directory__;
       StatusBar1.Repaint;

       Mask:=ALL_FILES_FILTER;
       Directory:=Directory__;
       i:=Items.Count;
       while (i>0) do begin
         Dec(i);
         s:=StrWithTrailingPathDelimiter(Directory)+Items[i];
         if      Result and StrEqual(ExtractFileExt(s),XSB_FILE_NAME_EXT) and (FileExists(s)) then begin
                 MenuItemFileConversionClick(nil); // nil': treat ".xsb" files as SokoMind files
                 Update; i:=Items.Count;
                 Result:=not FileExists(s); // 'True': 'MenuItemFileConversionClick' converted the file successfully and deleted it
                 end
         else if Result and StrEqual(ExtractFileExt(s),SOKOBAN_FOR_WINDOWS_LEVEL_FILE_NAME_EXT) and (FileExists(s)) and (not StrBeginsWith(Items[i],'Hyper')) then begin // 'Hyper': at the time of writing, 2008, there was a large 10,000 "garbage" levels collection floating around; it was random autogenerated levels without any game-play qualities
                 MenuItemFileConversionClick(nil); // nil': treat ".slc" files as "Sokoban for Windows" files
                 Update; i:=Items.Count;
                 Result:=not FileExists(s); // 'True': 'MenuItemFileConversionClick' converted the file successfully and deleted it
                 end
         else if StrEqual(ExtractFileExt(s),TEXT_FILE_EXT) and (FileExists(s)) and (FileSize(s)<MAX_FILE_SIZE) then begin
                 if MainForm.SokoFile.IsASokobanFile(s) then begin
                    MainForm.SokoFile.Clear;
                    s1:=MakeNewFileName(s,SOKOBAN_FILE_NAME_EXT,True);
                    if MainForm.SokoFile.SetName('') and (s1<>'') then Misc_.MoveFileOrFolder(s,s1);
                    end;
                 end
              else
                 if StrEqual(ExtractFileName(s),'all.compleet') then DeleteFile(s); // delete Grigoriev level collection log file
         end;
       end;
  end;

  function  RemoveEmptyDirectory(const Directory__:String; DirectoryList__:SokUtil_.TList):Boolean;
  var s{,s1}:String; //Node:TNode;
  begin
    Result:=False;
    if DirectoryExists(Directory__) then with FileListBox1 do begin
         StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:=Directory__;
         StatusBar1.Repaint;

         s:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Directory__));
         if s<>'' then begin
            Directory:=s;
            {
            if Items.Count=0 then begin // 'empty parent directory; try to move elements in 'Directory__' up to the parent level
               Directory:=Directory__;
               for i:=Pred(FileListBox1.Items.Count) downto 0 do
                   MoveFileOrFolder(StrWithTrailingPathDelimiter(Directory__)+FileListBox1.Items[i],StrWithTrailingPathDelimiter(s)+FileListBox1.Items[i]);

               Node:=DirectoryList__.First;
               while Assigned(Node) do begin
                 if DirectoryExists(Node.Text) and StrBeginsWith(Node.Text,Directory) then begin
                    i:=1;
                    s1:=StrSubstitute(Node.Text,Directory__,s,i);
                    if (s1<>'') and (i=1) then
                       if MoveFileOrFolder(Node.Text,s1) then DirectoryList__.AddTextLine(s1,False);
                    end;

                 Node:=Node.Next;
                 end;
               end;
            }
            Directory:=Directory__;
            if Items.Count=0 then begin
               SetCurrentDir(s);
               Directory:=s;
               try    Result:=SysUtils.RemoveDir(Directory__);
                      //if not Result then Msg(Format(DeleteDirectoryFailedText__,[Directory__]),Application.ExeName,MB_OK+MB_ICONINFORMATION);
               except on E:Exception do begin end;
               end;
               end;
            end;
         end;
  end;

begin // UnpackFile
  Result:=False;
  if CloseFile then
     try    DirectoryList:=nil;
            oCursor:=Screen.Cursor;
            oCurrentDir:=GetCurrentDir;
            oShowErrorMessages:=SokUtil_.ShowErrorMessages;
            CountOK:=0; CountFailed:=0; FirstFailedFileName:=''; UnpackDirectory:='';

            try     DiskGroupBox.Hide; GameFileGroupBox.Hide;
                    StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
                    StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
                    if PanelToolTips.Visible then PanelToolTips.Hide;
                    Self.Update;
                    Screen.Cursor:=crHourGlass;

                    SokUtil_.ShowErrorMessages:=semAll;
                    Result:=CreateObject(otList,TNode(DirectoryList));
                    SokUtil_.ShowErrorMessages:=semNone; IsLoading:=True;

                    Result:=Result and UnpackFile(FileName__,UnpackDirectory);

                    if Result then begin
                       Node:=DirectoryList.First;
                       while Assigned(Node) do begin
                         UnpackFilesInDirectory(Node.Text,DirectoryList);
                         Node:=Node.Next;
                         end;

                       Self.Update; Self.Repaint;

                       DirectoryList.MergeSort(CompareNodes);
                       Node:=DirectoryList.First;
                       while Assigned(Node) do begin
                         FileConversion(Node.Text);
                         Node:=Node.Next;
                         end;

                       Node:=DirectoryList.First;
                       while Assigned(Node) do begin
                         RemoveEmptyDirectory(Node.Text,DirectoryList);
                         Node:=Node.Next;
                         end;
                       end;

                    StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
                    StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';

                    if   CountOK=0 then
                         raise Exception.Create(Format(UnpackFileFailedText__,[FileName__]))
                    else if CountOK=1 then
                         else if   CountFailed=0 then
                                   Msg(Format(UnpackedFilesText__,[CountOK]),Application.Title+SUB_TITLE_SEPARATOR+UnpackFilesText,MB_OK+MB_ICONINFORMATION)
                              else Msg(Format(UnpackedFilesWithFailuresText__,[CountOK,CountFailed,FirstFailedFileName]),Application.Title+SUB_TITLE_SEPARATOR+UnpackFilesText,MB_OK+MB_ICONINFORMATION);
            finally SokUtil_.ShowErrorMessages:=oShowErrorMessages; IsLoading:=False;
                    DiskGroupBox.Show; GameFileGroupBox.Show;
                    Screen.Cursor:=oCursor;
                    DirectoryList.Free;
                    StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
                    StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
                    if DirectoryExists(oCurrentDir) then SetCurrentDir(oCurrentDir);
                    FileListBox1.Directory:=StrWithoutTrailingPathDelimiter(ExtractFilePath(FileName__));
                    FileListBox1.Update;
                    TryToLoadFile(FileName__);
                    if Result and DirectoryExists(UnpackDirectory) then DirectoryListBox1.Directory:=StrWithTrailingPathDelimiter(UnpackDirectory);
                    if DiskGroupBox.Visible then ActiveControl:=FileListBox1;
            end;
     except on E:Exception do Result:=Error(E.Message,Application.Title+SUB_TITLE_SEPARATOR+UnpackFilesText);
     end;
end;

procedure TOpenForm.MenuItemFindDuplicateLevelsClick(
  Sender: TObject);
begin
  if (Task=otGame)
     and
     (SubTask=osNone)
     and
     MenuItemSubMenuFindDuplicateLevels.Enabled
     and
     ((Sender =MenuItemFindDuplicateLevelsAllLevels)
      or
      ((Sender=MenuItemFindDuplicateLevelsCurrentCollection)
       and
       (CurrentFileName<>'')
       and
       Assigned(Game)
       and
       ((CollectionGroupBox.Visible
         and
         (CollectionStringGrid.RowCount>1)
        )
        or
        ((Game.BoardWidth >=MIN_BOARD_WIDTH )
         and
         (Game.BoardHeight>=MIN_BOARD_HEIGHT)
         and
         (Game.GameState  <>gsNull)
        )
       )
      )
      or
      ((Sender=MenuItemFindDuplicateLevelsCurrentLevel)
       and
       (CurrentFileName<>'')
       and
       Assigned(Game)
       and
       (Game.BoardWidth >=MIN_BOARD_WIDTH )
       and
       (Game.BoardHeight>=MIN_BOARD_HEIGHT)
       and
       (Game.GameState<>gsNull)
      )
     )
     and
     CloseFile then begin
     ControlMouseUp(Sender,mbLeft,[],0,0);
     StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:='';

     if WindowState<>wsNormal then {avoid that the "Duplicates" window is hidden behind a maximixed "Open..." window in some scenarios where the Windows operating system intervenes because the program doesn't respond to user input during a lengthy calculation}
        WindowState:=wsNormal;

     DuplicatesForm.HighlightedRowNumber:=-1; // so 'TDuplicatesForm.FormActivate' doesn't reload the last focused level, if any
     if   not DuplicatesForm.Visible then DuplicatesForm.Show
     else DuplicatesForm.SetFocus;
     if        Sender=MenuItemFindDuplicateLevelsAllLevels            then
               DuplicatesForm.FindDuplicateLevels(fdlAllLevels        ,DuplicatesForm.ViewMenuItemAllLevelsDuplicates        ,'','','')
     else if   Sender=MenuItemFindDuplicateLevelsCurrentCollection    then
               DuplicatesForm.FindDuplicateLevels(fdlCurrentCollection,DuplicatesForm.ViewMenuItemCurrentCollectionDuplicates,'','','')
          else DuplicatesForm.FindDuplicateLevels(fdlCurrentLevel     ,nil,'','','');
     end;

end;

procedure TOpenForm.MenuItemFindDuplicateLevelsMatchOptionsClick(Sender: TObject);
begin
  if      Sender=MenuItemFindDuplicateLevelsMatchOptionsReset then begin
          MenuItemFindDuplicateLevelsMatchOptionsBoxes.Checked:=True;
          MenuItemFindDuplicateLevelsMatchOptionsGoals.Checked:=True;
          MenuItemFindDuplicateLevelsMatchOptionsInteriorWalls.Checked:=True;
          MenuItemFindDuplicateLevelsMatchOptionsExteriorWalls.Checked:=True;
          end
  else if Sender is TMenuItem then with Sender as TMenuItem do Checked:=not Checked;

  MenuItemFindDuplicateLevelsMatchOptionsReset.Enabled:=
    (not MenuItemFindDuplicateLevelsMatchOptionsBoxes.Checked) or
    (not MenuItemFindDuplicateLevelsMatchOptionsGoals.Checked) or
    (not MenuItemFindDuplicateLevelsMatchOptionsInteriorWalls.Checked) or
    (not MenuItemFindDuplicateLevelsMatchOptionsExteriorWalls.Checked);
end;

procedure TOpenForm.MenuItemFindDuplicateLevelsMoreSettingsClick(
  Sender: TObject);
begin
  if Assigned(DuplicatesForm) and (not DuplicatesForm.IsBusy) then
     DuplicatesForm.SettingsMenuItemMoreSettingsClick(Sender);
end;

procedure TOpenForm.MenuItemDuplicateLevelsAddFolderClick(Sender: TObject);
var Directory:String;
begin
  Directory:='';
  if SelectDirectory(Application.Title+SUB_TITLE_SEPARATOR+DuplicateLevelsCaptionText+SUB_TITLE_SEPARATOR+AddFolderCaptionText,'',Directory) then
     DuplicatesForm.AddDirectory(Directory);
end;

procedure TOpenForm.MenuItemDuplicateLevelsClearFolderListClick(
  Sender: TObject);
begin
  with MenuItemDuplicateLevelsFolderSeparator do
    if (MenuIndex<>0) and
       (SokUtil_.Msg(ClearListQuestionText,Application.Title+SUB_TITLE_SEPARATOR+DuplicateLevelsCaptionText+SUB_TITLE_SEPARATOR+FoldersText,MB_YESNOCANCEL+MB_ICONQUESTION+MB_DEFBUTTON2)=IDYES) then begin
       while MenuIndex<>0 do
         MenuItemSubMenuFindDuplicateLevelsFolders.Delete(0);
       MenuItemDuplicateLevelsDefaultLevelFolder.Checked:=True; // enable scanning of the default level folder; some users may not want that, but to help the casual user it's better to do it this way   
       end;
end;

procedure TOpenForm.MenuItemDuplicateLevelsFolderClick(
  Sender: TObject);
begin
  if Sender is TMenuItem then with Sender as TMenuItem do Checked:=not Checked;
end;

procedure TOpenForm.DoNotSplitImageFileGroupBox;
var i:Integer;
begin
  i                       :=BtnOK.Top-8-ImagePanel.Top;
  if ImagePanel   .Height <>i then begin
     ImagePanel   .Height :=i;
     TextFilePanel.Top    :=ImagePanel.Top;
     TextFilePanel.Height :=ImagePanel.Height;
     end;
end;

procedure TOpenForm.HistoryComboBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var s,ItemName,ItemPath:String; P:TPoint; R:TRect;
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    if   odFocused in State then
         Canvas.Font.Color:=clHighlightText
    else Canvas.Font.Color:=clWindowText;
    s:=FormatItemAsNameAndPath(Items[Index],ItemName,ItemPath);
    if   ItemPath<>'' then begin
         R:=Rect; R.Right:=Min(R.Right,R.Left+OpenForm.Canvas.TextWidth(ItemName)+2);
         ExtTextOut(Canvas.Handle, R.Left+2, R.Top, ETO_CLIPPED or ETO_OPAQUE, @R, PChar(ItemName), Length(ItemName), nil);

         if   odFocused in State then
              Canvas.Font.Color:=clHighlightText
         else Canvas.Font.Color:=clGrayText;
         Rect.Right:=Min(Rect.Right,Rect.Left+Tag-VerticalScrollBarWidth-8); // 'Tag' is dropdown width
         P:=ClientToScreen(Point(Rect.Right,0));
         P.X:=Min(P.X,Screen.DeskTopWidth-8);
         P:=ScreenToClient(P);
         Rect.Right:=P.X;
         Rect.Left :=Max(R.Right+8,Rect.Right-OpenForm.Canvas.TextWidth(ItemPath));

         ItemPath:=PathCompactPath(OpenForm.Canvas,ItemPath,RectWidth(Rect));

         Rect.Left :=Max(R.Right+8,Rect.Right-OpenForm.Canvas.TextWidth(ItemPath)); // the text appears right-justified on the screen, but calculating the left side and left-justifying the text in its rectangle is something different than right-justifying the text; if the text needs clipping, then left-justification displays the start of the text, while right-justification displays the last part of the text

         ExtTextOut(Canvas.Handle, Rect.Left, Rect.Top, ETO_CLIPPED or ETO_OPAQUE, @Rect, PChar(ItemPath), Length(ItemPath), nil);
         end
    else ExtTextOut(Canvas.Handle, Rect.Left + 2, Rect.Top, ETO_CLIPPED or ETO_OPAQUE, @Rect, PChar(s), Length(s), nil);
    end;
end;

procedure TOpenForm.OnFontChange;
begin
  StatusBar1.Font.Assign(Self.Font);
  TopPanel.Font.Name:=Self.Font.Name;
  TopPanel.Font.Style:=Self.Font.Style;
  GameNamePanel.Font.Assign(Self.Font);
  CollectionFileNamePanel.Font.Assign(Self.Font);
  BtnFileMenu.Font.Assign(Self.Font);
  BtnEditMenu.Font.Assign(Self.Font);

  GameSolutionTextLabel.Font.Name:=Self.Font.Name;
  GameSolutionTextLabel.Font.Style:=Self.Font.Style;
  GameSolutionTextLabel.Font.Size:=SOLUTION_FONT_SIZE;
  GameSolutionMovesRadioButton.Font.Name:=Self.Font.Name;
  GameSolutionMovesRadioButton.Font.Style:=Self.Font.Style;
  GameSolutionMovesRadioButton.Font.Size:=SOLUTION_FONT_SIZE;
  GameSolutionPushesRadioButton.Font.Name:=Self.Font.Name;
  GameSolutionPushesRadioButton.Font.Style:=Self.Font.Style;
  GameSolutionPushesRadioButton.Font.Size:=SOLUTION_FONT_SIZE;
  GameSolutionLabel.Font.Name:=Self.Font.Name;
  GameSolutionLabel.Font.Style:=Self.Font.Style;
  GameSolutionLabel.Font.Size:=SOLUTION_FONT_SIZE;
  GameSolutionPushesLabel.Font.Name:=Self.Font.Name;
  GameSolutionPushesLabel.Font.Style:=Self.Font.Style;
  GameSolutionPushesLabel.Font.Size:=SOLUTION_FONT_SIZE;

  GameSolutionLabel.Left:=(2*GameSolutionMovesRadioButton.Left)+Max(GameSolutionMovesRadioButton.Width,GameSolutionPushesRadioButton.Width);
  GameSolutionPushesLabel.Left:=GameSolutionLabel.Left;

  TopPanel.Font.Color:=ApplicationHiglightedTextColor;
  GameNameLabel.Font.Color:=TopPanel.Font.Color;
  GameNamePanel.Font.Color:=TopPanel.Font.Color;
  CollectionFileNameLabel.Font.Color:=TopPanel.Font.Color;
  CollectionFileNamePanel.Font.Color:=TopPanel.Font.Color;
  ImageNamePanel.Font.Color:=TopPanel.Font.Color;
  PluginLevelFileNamePanel.Font.Color:=TopPanel.Font.Color;
  PlayingSoundFileLabel.Font.Color:=TopPanel.Font.Color;
end;

end.

