unit Duplicates_;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, Grids, StdCtrls, Menus,
  SokUtil_,SokFile_,SokGame_,Game_,Misc_,IniFile_,Hash_;

type
  TFindDuplicateLevelsTask=(fdlAllLevels,fdlCurrentCollection,fdlCurrentLevel);
  TScanState=(ssIdle,ssScan,ssCancel,ssCloseWindow);

////////////////////////////////////////////////////////////////////////////////
// TLevelIndexItem
////////////////////////////////////////////////////////////////////////////////

  TLevelIndexItem=class(THashTableItem)
  private
    fBoardTransformation2D:TBoardTransformation2D; // the transformation from the actual level board to the normalized board
    fDirectoryItem        :TNode;
    fDuplicatesCount      :Integer;
    fFileItem             :TNode;
    fHasBeenUpdated       :Boolean;
    fLevelIndex           :Integer;
    fName                 :String;
    fNormalizedBoard      :String;          // rows are separated by CR; a row separator is necessary so the normalized board can be used for comparisons
    fSnapshots            :TList;           // snapshots on the list are either of type 'TSnapshotsAsText' or 'TSnapshot'
  protected
    NextDuplicate         :TLevelIndexItem; // next duplicate; a circular list
    NextInFile            :TLevelIndexItem; // next item in the same file; a circular list
    NextItem              :TLevelIndexItem; // next item in chronological order
  public
    BoardAsText           :TBoardAsText;    // BoardAsText.Board has no row separators

    constructor            Create(const Name__,NormalizedBoard__,Board__:String; BoardWidth__,BoardHeight__:Integer; BoardTransformation2D__:TBoardTransformation2D; HashKey__:THashTableHashKey; DirectoryItem__,FileItem__:TNode; LevelIndex__:Integer);
    destructor             Destroy; override;

    property               BoardTransformation2D:TBoardTransformation2D read fBoardTransformation2D write fBoardTransformation2D;
    property               DirectoryItem:TNode read fDirectoryItem;
    property               DuplicatesCount:Integer read fDuplicatesCount write fDuplicatesCount;
    property               FileItem:TNode read fFileItem;
    property               HasBeenUpdated:Boolean read fHasBeenUpdated write fHasBeenUpdated;
    property               LevelIndex:Integer read fLevelIndex;
    property               Key:String read fNormalizedBoard;
    property               Name :String read fName;
    property               NormalizedBoard:String read fNormalizedBoard write fNormalizedBoard;
    property               Snapshots:TList read fSnapshots write fSnapshots;
  end;

////////////////////////////////////////////////////////////////////////////////
// TLevelIndexHashTable - caller calculates hash keys before lookup
////////////////////////////////////////////////////////////////////////////////

  TLevelIndex=class(THashTable)
  protected
    First           :TLevelIndexItem;
    Last            :TLevelIndexItem;
  public
    constructor      Create(CapacityHint__,LoadFactor__:Integer); override;

    procedure        Add(Item__:THashTableItem); override;
    procedure        Clear; override;
    procedure        Delete(var Item__:THashTableItem); override;
    function         Lookup(HashKey__:THashTableHashKey; const Key__:String; var Item__:TLevelIndexItem):Boolean;
  end;

////////////////////////////////////////////////////////////////////////////////
// TDuplicatesForm - Find Duplicates Form
////////////////////////////////////////////////////////////////////////////////

  TDuplicatesForm = class(TForm)
    TopPanel: TPanel;
    StatusBar1: TStatusBar;
    DuplicateLevelsGroupBox: TGroupBox;
    PanelDuplicateLevelsStatus: TPanel;
    DuplicateLevelsStatusStringGrid1: TStringGrid;
    BottomPanel: TPanel;
    CloseBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    MenuItemExit: TMenuItem;
    HelpMenu: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit2: TMenuItem;
    DuplicateLevelsStatusStringGrid2: TStringGrid;
    SettingsMenu: TMenuItem;
    SettingsMenuWindowSize: TMenuItem;
    MenuItemWindowSizeDefault: TMenuItem;
    MenuItemWindowSizeDefaultCentered: TMenuItem;
    MenuItemWindowSizeMaximized: TMenuItem;
    PanelDuplicateLevelsList: TPanel;
    RichEdit1: TRichEdit;
    N1: TMenuItem;
    SettingsMenuItemMoreSettings: TMenuItem;
    EditMenu: TMenuItem;
    EditMenuItemCopyDuplicatesListToClipboard: TMenuItem;
    DuplicateLevelsStringGrid: TStringGrid;
    PanelToolTips: TPanel;
    ViewMenuAllLevels: TMenuItem;
    ViewMenuItemAllLevelsAllLevels: TMenuItem;
    ViewMenuItemAllLevelsNoDuplicates: TMenuItem;
    ViewMenuItemAllLevelsWithDuplicates: TMenuItem;
    ViewMenuItemAllLevelsDuplicates: TMenuItem;
    ViewMenuItemAllLevelsUniqueLevels: TMenuItem;
    ViewMenuItemRefreshSeparator: TMenuItem;
    ViewMenuItemAllLevelsRefresh: TMenuItem;
    ViewMenuCurrentCollection: TMenuItem;
    ViewMenuItemCurrentCollectionRefresh: TMenuItem;
    N2: TMenuItem;
    ViewMenuItemCurrentCollectionDuplicates: TMenuItem;
    ViewMenuItemCurrentCollectionWithDuplicates: TMenuItem;
    ViewMenuItemCurrentCollectionNoDuplicates: TMenuItem;
    ViewMenuItemCurrentCollectionAllLevels: TMenuItem;
    ViewMenuCurrentLevel: TMenuItem;
    ViewMenuItemCurrentLevelRefresh: TMenuItem;
    ViewMenuItemCurrentCollectionUniqueLevels: TMenuItem;
    ToolsMenu: TMenuItem;
    ToolsMenuItemUpdateSolutions: TMenuItem;
    ToolsMenuItemNewCollectionUniqueLevels: TMenuItem;
    ViewMenuItemAllLevelsWithSolutionsAndSnapshots: TMenuItem;
    ViewMenuItemAllLevelsNoSolutionsAndSnapshots: TMenuItem;
    ViewMenuItemCurrentCollectionNoSolutionsAndSnapshots: TMenuItem;
    ViewMenuItemCurrentCollectionWithSolutionsAndSnapshots: TMenuItem;
    PopupMenu: TPopupMenu;
    N3: TMenuItem;
    EditMenuItemDeleteLevel: TMenuItem;
    PopupMenuItemDeleteLevel: TMenuItem;
    PopupMenuItemCopyDuplicatesListToClipboard: TMenuItem;
    N4: TMenuItem;
    ViewMenuItemCurrentCollectionUpdatedLevels: TMenuItem;
    ViewMenuItemAllLevelsUpdatedLevels: TMenuItem;
    SettingsMenuItemToolsUpdateSolutions: TMenuItem;
    SettingsMenuItemToolsUpdateSolutionsAllSolutions: TMenuItem;
    SettingsMenuItemToolsUpdateSolutionsBestSolutionsOnly: TMenuItem;
    SettingsMenuItemToolsNewCollection: TMenuItem;
    SettingsMenuItemToolsNewCollectionBestSolutionsOnly: TMenuItem;
    SettingsMenuItemToolsNewCollectionAllSolutions: TMenuItem;
    N5: TMenuItem;
    SettingsMenuItemToolsSettings: TMenuItem;
    ToolsMenuItemNewCollectionUnsolvedLevels: TMenuItem;
    BottomPanelBevel1: TBevel;
    ToolsMenuItemNewCollectionSolvedLevels: TMenuItem;
    ToolsMenuItemNewCollectionCurrentlyDisplayedLevels: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuItemHelpClick(Sender: TObject);
    procedure MenuItemDeleteLevelClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure DuplicateLevelsStringGridDrawCell(Sender: TObject;
      ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemWindowSizeClick(Sender: TObject);
    procedure RichEdit1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RichEdit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SettingsMenuItemMoreSettingsClick(Sender: TObject);
    procedure RichEdit1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditMenuItemCopyDuplicatesListToClipboardClick(
      Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DuplicateLevelsStringGridMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DuplicateLevelsStringGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure DuplicateLevelsStringGridMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure DuplicateLevelsStringGridMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewMenuItemClick(Sender: TObject);
    procedure ViewMenuItemRefreshClick(Sender: TObject);
    procedure ToolsMenuItemUpdateSolutionsClick(Sender: TObject);
    procedure ToolsMenuItemNewCollectionClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure DuplicateLevelsStringGridKeyUp(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure DuplicateLevelsStringGridDblClick(Sender: TObject);
    procedure ToolsMenuItemSettingsClick(
      Sender: TObject);
  private
    { Private declarations }
    CurrentCollectionDuplicateLevelsCount:Integer; // finding duplicates for current collection: duplicates inside the collection itself
    CurrentCollectionFileName:String;
    CurrentCollectionLevelsCount:Integer;
    CurrentCollectionLevelsWithSolutionsAndSnapshotsCount:Integer;
    CurrentDirectoryName:String;
    CurrentFileName:String;
    DragPoint:TPoint;
    DirectoryList:SokUtil_.TList;
    DuplicatedLevelsCount:Integer; // note the difference between 'DuplicatedLevelsCount' and 'DuplicateLevelsCount'
    DuplicateLevelsCount:Integer;  // note the difference between 'DuplicatedLevelsCount' and 'DuplicateLevelsCount'
    DuplicateLevelsTask:TFindDuplicateLevelsTask;
    EscapeEnabled:Boolean;
    fHighlightedRowNumber:Integer;
    fScanState:TScanState;
    FileList:SokUtil_.TList;
    fIsBusy:Boolean;
    fMatchThresholdPercent:Integer;
    LevelIndex:TLevelIndex;
    LevelsWithSolutionsAndSnapshotsCount:Integer;
    MatchOptionsMask:Integer;
    MouseButtonDown:Boolean;
    OldOnHint:TNotifyEvent;
    OldOnIdle:TIdleEvent;
    OldOnMessage:TMessageEvent;
    ScannedDirectoryNamesText:String;
    ScannedLevelsCount:Integer;
    UpdatedLevelsCount:Integer; // number of levels updated with solutions from duplicates
    function  AddLevelToStringGrid(LevelIndexItem__:TLevelIndexItem; var LastRow__:Integer):Boolean;
    procedure ClearRow(Row__:Integer);
    function  ItemCount(Sender__:TObject):Integer;
    function  MinimumHeight:Integer;
    function  MinimumWidth:Integer;
    function  Tools(Sender:TObject; const NewSokoFileName__:String):Boolean;
    function  SelectedViewMenuItem:TMenuItem;
    procedure SetGridColumnWidths(RowCount__,LevelNumbers__:Integer; SetDefaultColumnWidths__:Boolean);
    procedure SetMessageHandlers;
    function  SetRowCount(RowCount__:Integer):Boolean;
    procedure ShowHint(Sender: TObject);
    function  ShowSearchResults(ViewMenuItem__:TMenuItem; const FocusLevelName__,FocusFileName__,FocusDirectoryName__:String):Boolean;
    function  ShowStatus:Boolean;
    function  TryToLoadGame(const FileName__:String):Boolean;
  protected
    function  GetCellText(Col__,Row__:Integer):String;
    procedure OnCloseWindow(var Msg: TMessage); Message Misc_.MSG_CLOSE;
    procedure OnRefresh(var Msg: TMessage); Message Misc_.MSG_REFRESH;
    procedure RichEdit1MouseWheelDown(Sender: TObject;Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure RichEdit1MouseWheelUp  (Sender: TObject;Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure SetHighlightedRowNumber(Row__:Integer);
    procedure SetMatchThresholdPercent(Percent__:Integer);
  public
    { Public declarations }
    AdvancedIdentityCheck:Boolean; {disadvantage: misses some symmetric duplicates}
    BackgroundColor1,BackgroundColor2:TColor;
    HighlightBackgroundColor,HighlightTextColor:TColor;
    ShowOnStartup:Boolean;
    TextColor1,TextColor2:TColor;

    function  AddDirectory(const Directory__:String):TMenuItem;
    function  FindDuplicateLevels(DuplicateLevelsTask__:TFindDuplicateLevelsTask; ViewMenuItem__:TMenuItem; const FocusLevelName__,FocusFileName__,FocusDirectoryName__:String):Boolean;
    function  IsASubDirectoryName(const SubDirectory__,Directory__:String):Boolean;
    function  LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
    procedure NormalizeRotationsAndReflectionsAndPlayerPosition(Game__:TGame; NormalizePlayerPosition__:Boolean; var HashValue__:THashTableHashKey);
    procedure OnFontChange;
    function  SaveSettingsToIniFile  (const IniFile: TIniFile): Boolean;

    property  CellText[Col__,Row__:Integer]:String read GetCellText;
    property  HighlightedRowNumber:Integer read fHighlightedRowNumber write SetHighlightedRowNumber;
    property  IsBusy:Boolean read fIsBusy;
    property  MatchThresholdPercent:Integer read fMatchThresholdPercent write SetMatchThresholdPercent;
    property  ScanState:TScanState read fScanState write fScanState;
  end;

var
  DuplicatesForm: TDuplicatesForm = nil;

implementation

{$R *.DFM}

uses RichEdit,Clipbrd,
     {$WARNINGS OFF}
       FileCtrl, // Warning: Unit 'FileCtrl' is specific to a platform
     {$WARNINGS ON}
     Text_,Help_,File_,Pack_,Open1_,Main_,
     Tools_, Options_;

const
  ALL_MATCH_OPTIONS               = BOARD_PIECES+EXTERIOR_WALL;
  DUPLICATES_INIFILE_SECTION      = 'DuplicatesForm';   // don't localize
  DEFAULT_ADVANCED_IDENTITY_CHECK = False; {disabled by default because of the disadvantages; it misses some symmetric duplicates}
  DEFAULT_MATCH_THRESHOLD_PERCENT = 85;
  MULTIPLE_ROOTS_TEXT_SUFFIX      = '; ...'; // indicates that two or more root folders have been scanned

constructor TLevelIndexItem.Create(const Name__,NormalizedBoard__,Board__:String; BoardWidth__,BoardHeight__:Integer; BoardTransformation2D__:TBoardTransformation2D; HashKey__:THashTableHashKey; DirectoryItem__,FileItem__:TNode; LevelIndex__:Integer);
begin
  Inherited Create(HashKey__);
  BoardAsText.Board:=Board__; BoardAsText.Width:=BoardWidth__; BoardAsText.Height:=BoardHeight__;
  fBoardTransformation2D:=BoardTransformation2D__;
  fDirectoryItem:=DirectoryItem__; fFileItem:=FileItem__; fLevelIndex:=LevelIndex__; fName:=Name__; fNormalizedBoard:=NormalizedBoard__;
  fDuplicatesCount:=0; fHasBeenUpdated:=False;
  NextItem:=nil; NextDuplicate:=nil; NextInFile:=nil; fSnapshots:=nil;
end;

destructor TLevelIndexItem.Destroy;
begin
  BoardAsText.Board:=''; fName:=''; fNormalizedBoard:='';
  fSnapshots.Free; fSnapshots:=nil;
  Inherited;
end;

constructor TLevelIndex.Create(CapacityHint__,LoadFactor__:Integer);
begin
  First:=nil; Last:=nil;
  Inherited Create(CapacityHint__,LoadFactor__);
end;

procedure   TLevelIndex.Add(Item__:THashTableItem);
begin
  if   (Item__ is TLevelIndexItem) then with Item__ as TLevelIndexItem  do begin
       Inherited Add(Item__);
       if   Last<>nil then Last.NextItem:=TLevelIndexItem(Item__)
       else First:=TLevelIndexItem(Item__);
       Last:=TLevelIndexItem(Item__);
       end
  else raise Exception.Create(InternalErrorText+': TLevelIndex.Add');
end;

procedure   TLevelIndex.Clear;
begin
  First:=nil; Last:=nil; Inherited;
end;

procedure   TLevelIndex.Delete(var Item__:THashTableItem);
var Node:TLevelIndexItem;
begin
  if   (Item__ is TLevelIndexItem) then with Item__ as TLevelIndexItem  do begin
       if   TLevelIndexItem(Item__)=Last then begin
            Last:=First;
            while Assigned(Last.NextItem) do Last:=Last.NextItem;
            end
       else if TLevelIndexItem(Item__)=First then First:=NextItem
            else begin
               Node:=First;
               while Assigned(Node) and (Node.NextItem<>TLevelIndexItem(Item__)) do Node:=Node.NextItem;
               if Assigned(Node) then Node.NextItem:=NextItem;
               end;

       if   Assigned(NextDuplicate) then begin
            Node:=TLevelIndexItem(Item__);
            while Assigned(Node) and (Node.NextDuplicate<>TLevelIndexItem(Item__)) do begin // find the previous item on the circular list
                  if Node.DuplicatesCount>0 then Node.DuplicatesCount:=Pred(Node.DuplicatesCount); // update the root node for this set of duplicates
                  Node:=Node.NextDuplicate;
                  end;
            if Assigned(Node) then begin // 'True': the circular list is intact
               if    Node<>NextDuplicate then // 'True': the circular list has more than 2 members
                     Node.NextDuplicate:=NextDuplicate // remove the item from the circular list
               else  Node.NextDuplicate:=nil; // 'Node' has no duplicates anymore after removal of 'Item__'
               Node.DuplicatesCount:=DuplicatesCount; // 'Node' is the new root item for this set of duplicates
               end;
            end;

       if   Assigned(NextInFile) then begin
            Node:=TLevelIndexItem(Item__);
            while Assigned(Node) and (Node.NextInFile<>TLevelIndexItem(Item__)) do Node:=Node.NextInFile; // find the previous item on the circular list
            if Assigned(Node) then begin // 'True': the circular list is intact
               if    Node<>NextInFile then // 'True': the circular list has more than 2 members
                     Node.NextInFile:=NextInFile // remove the item from the circular list
               else  Node.NextInFile:=Node; // make a circular list with the remaining node only
               end;
            end;

       Inherited;
       end
  else raise Exception.Create(InternalErrorText+': TLevelIndex.Delete');
end;

function    TLevelIndex.Lookup(HashKey__:THashTableHashKey; const Key__:String; var Item__:TLevelIndexItem):Boolean;
var Last:THashTableItem;
begin
  Result:=False;
  if inherited Lookup(HashKey__,THashTableItem(Item__)) then
     if Key__=Item__.Key then
        Result:=True
     else begin
        Last:=TLevelIndexItem(Item__.Prev);
        if Last<>Item__ then  // 'True': this bucket contains more than 1 item
           repeat Item__:=TLevelIndexItem(Item__.Next);
                  if Item__.Key=Key__ then begin
                     Result:=True; exit; // found: quick and dirty exit
                     end
           until  (Item__.HashKey<>HashKey__) or (Item__=Last); // until all items with the same hash-key, or all items in the bucket, have been visited
        end;
end;

////////////////////////////////////////////////////////////////////////////////
// Rich edit - scroll caret into view
////////////////////////////////////////////////////////////////////////////////

procedure ScrollCaretIntoView(RichEdit__:TRichEdit);
var CurrentLineNo,TopRow,BottomRow:Integer;

  function LastVisibleLine:Integer;
  var LastLineCharIndex:Integer; R:TRect;
  begin
    with RichEdit__ do begin
      Perform(EM_GETRECT,0,Integer(Addr(R)));
      R.Bottom:=Max(0,R.Bottom-HorizontalScrollBarHeight);
      R.Right :=Max(0,R.Right -VerticalScrollBarWidth   );
      Inc(R.Left); R.Top:=R.Bottom-2;
      LastLineCharIndex:=Perform(EM_CHARFROMPOS, 0, Integer(Addr(R.TopLeft)));
      Result:=Perform(EM_EXLINEFROMCHAR,0,LastLineCharIndex);
      end;
  end;

begin // ScrollCaretIntoView
  with RichEdit__ do begin
    //Perform(EM_SCROLLCARET,0,0); // according to the Windows documentation this should do the job, but it doesn't seem to work; reason unknown

    TopRow:=Perform(EM_GETFIRSTVISIBLELINE,0,0);
    BottomRow:=LastVisibleLine;
    CurrentLineNo:=Perform(EM_EXLINEFROMCHAR,0,SelStart);
    if      CurrentLineNo<TopRow    then
            Perform(EM_LINESCROLL,0,CurrentLineNo-TopRow)
    else if CurrentLineNo>BottomRow then
            Perform(EM_LINESCROLL,0,CurrentLineNo-BottomRow);
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// TDuplicatesForm
////////////////////////////////////////////////////////////////////////////////

procedure TDuplicatesForm.FormCreate(Sender: TObject);
var i,W:Integer; dlsii:TDuplicateLevelsStatusInfoItem; dlc:TDuplicateLevelsStringGridColumn;
begin
  OnFontChange;
  if biMinimize in BorderIcons then BorderIcons:=BorderIcons-[biMinimize]; // minimize: it may be impossible for the user to bring the application back on the screen again (a Microsoft Windows operating system bug when the general Windows text size settings is > 100%)
  RichEdit1.Visible:=False; RichEdit1.Align:=alClient;
  DuplicateLevelsStringGrid.Align:=alClient;
  DuplicateLevelsStringGrid.ColCount:=Ord(High(TDuplicateLevelsStringGridColumn))-Ord(Low(TDuplicateLevelsStringGridColumn))+1;
  TopPanel.Caption:=Trim(TopPanel.Caption)+SPACE;
  SetBounds(0,0,MinimumWidth,DEFAULT_FORM_HEIGHT);
  EscapeEnabled:=True; fHighlightedRowNumber:=-1; fMatchThresholdPercent:=DEFAULT_MATCH_THRESHOLD_PERCENT;
  MouseButtonDown:=False; ShowOnStartUp:=False; ScanState:=ssIdle; fIsBusy:=False;
  DirectoryList:=nil; FileList:=nil; LevelIndex:=nil;
  SettingsMenuItemToolsUpdateSolutionsAllSolutions.Checked:=True;
  SettingsMenuItemToolsUpdateSolutionsBestSolutionsOnly.Checked:=not SettingsMenuItemToolsUpdateSolutionsAllSolutions.Checked;
  SettingsMenuItemToolsNewCollectionAllSolutions.Checked:=True;
  SettingsMenuItemToolsNewCollectionBestSolutionsOnly.Checked:=not SettingsMenuItemToolsNewCollectionAllSolutions.Checked;
  ViewMenuItemCurrentCollectionRefresh.Hint:=ViewMenuItemCurrentLevelRefresh.Hint;
  ViewMenuItemAllLevelsRefresh.Hint:=ViewMenuItemCurrentLevelRefresh.Hint;

  with DuplicateLevelsStatusStringGrid1 do begin
    Top:=2; Left:=2;
    Font.Color:=clBtnText; Color:=clBtnFace;
    RowCount:=Ord(dlScanningFile)-Ord(dlScanning)+1;
    W:=0;
    for dlsii:=dlScanning to dlScanningFile do
      if Ord(dlsii)<RowCount then begin
         Cells[0,Ord(dlsii)]:=DuplicateLevelsStatusInfoText[dlsii];
         W:=Max(W,Self.Canvas.TextWidth(DuplicateLevelsStatusInfoText[dlsii]));
         end;
    W:=Max(W,Self.Canvas.TextWidth(DuplicateLevelsStatusInfoFoldersText));
    ColWidths[0]:=W+8;
    LeftCol:=FixedCols;
    ColWidths[1]:=ClientWidth-ColCount*GridLineWidth-ColWidths[0];
    while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]); // for safety; this shouldn't happen
    Height:=RowCount*(DefaultRowHeight+GridLineWidth);
    while VisibleRowCount<RowCount do Height:=Height+1;

    with DuplicateLevelsStatusStringGrid2 do begin
      Top :=DuplicateLevelsStatusStringGrid1.Top+DuplicateLevelsStatusStringGrid1.Height;
      Left:=DuplicateLevelsStatusStringGrid1.Left;
      Color:=DuplicateLevelsStatusStringGrid1.Color;
      ColCount:=Ord(dlScannedDirectories)-Ord(dlScanned)+1;
      for dlsii:=dlScanned to dlScannedDirectories do
          if Ord(dlsii)-Ord(dlScanned)<ColCount then
             Cells[Ord(dlsii)-Ord(dlScanned),0]:=DuplicateLevelsStatusInfoText[dlsii];
      DefaultRowHeight:=DuplicateLevelsStatusStringGrid1.DefaultRowHeight;
      GridLineWidth:=DuplicateLevelsStatusStringGrid1.GridLineWidth;
      ColWidths[0]:=DuplicateLevelsStatusStringGrid1.ColWidths[0];
      LeftCol:=FixedCols;
      Height:=RowCount*(DefaultRowHeight+GridLineWidth);
      while VisibleRowCount<RowCount do Height:=Height+1;
      end;

    with DuplicateLevelsStringGrid do begin
      for dlc:=Low(dlc) to High(dlc) do Cells[Ord(dlc),0]:=DuplicateLevelsColumnHeadersText[dlc];
      SetGridColumnWidths(Max(10000,RowCount),10000,True);
      end;

    PanelDuplicateLevelsStatus.ClientHeight:=DuplicateLevelsStatusStringGrid1.Height+
                                             DuplicateLevelsStatusStringGrid2.Height+
                                             2*Top;
    with PanelDuplicateLevelsStatus do PanelDuplicateLevelsList.Top:=Top+Height+PanelDuplicateLevelsList.Left;
    end;

  with BottomPanelBevel1 do Height:=BottomPanel.ClientHeight-2*Top;  

  PanelToolTips.Visible:=False; PanelToolTips.Caption:=''; PanelToolTips.BringToFront;

  AdvancedIdentityCheck   :=DEFAULT_ADVANCED_IDENTITY_CHECK;
  BackgroundColor1        :=DEFAULT_DUPLICATE_LEVELS_BACKGROUND_COLOR_1;
  BackgroundColor2        :=DEFAULT_DUPLICATE_LEVELS_BACKGROUND_COLOR_2;
  TextColor1              :=clBlack;
  TextColor2              :=clBlack;
  HighlightBackgroundColor:=$71B33C; // mediumseagreen  //$C56A31; //$92B7B1;
  HighlightTextColor      :=clWhite;

  for i:=0 to Pred(ComponentCount) do
      if      Components[i] is TPanel       then with Components[i] as TPanel       do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TLabel       then with Components[i] as TLabel       do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TButton      then with Components[i] as TButton      do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TToolButton  then with Components[i] as TToolButton  do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TRadioButton then with Components[i] as TRadioButton do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TComboBox    then with Components[i] as TComboBox    do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TGroupBox    then with Components[i] as TGroupBox    do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TListBox     then with Components[i] as TListBox     do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TMemo        then with Components[i] as TMemo        do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TEdit        then with Components[i] as TEdit        do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TImage       then with Components[i] as TImage       do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TScrollBox   then with Components[i] as TScrollBox   do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TStringGrid  then with Components[i] as TStringGrid  do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TStatusBar   then with Components[i] as TStatusBar   do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TToolBar     then with Components[i] as TToolBar     do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TPageControl then with Components[i] as TPageControl do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
      else if Components[i] is TTabSheet    then with Components[i] as TTabSheet    do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end
//    else if Components[i] is TTrackBar    then with Components[i] as TTrackBar    do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end // 'TTrackBar' doesn't have 'OnMouseMove' and 'OnMouseUp', unfortunately
      else if Components[i] is TProgressBar then with Components[i] as TProgressBar do begin if not Assigned(OnMouseMove) then OnMouseMove:=FormMouseMove; if not Assigned(OnMouseUp) then OnMouseUp:=FormMouseUp; end;
  PanelToolTips.OnMouseMove:=nil; // otherwise the tool tip panel may turn up blank when the mouse hovers over it

  MenuItemWindowSizeClick(nil);
  FormResize(Self);
end;

procedure TDuplicatesForm.FormDestroy(Sender: TObject);
begin
  DirectoryList.Free; FileList.Free; LevelIndex.Free;
  DirectoryList:=nil; FileList:=nil; LevelIndex:=nil;
end;

procedure TDuplicatesForm.FormActivate(Sender: TObject);
var b:Boolean;
begin
  OldOnMessage       :=Application.OnMessage;
  OldOnIdle          :=Application.OnIdle;
  OldOnHint          :=Application.OnHint;

  TopPanel.Font.Color:=ApplicationHiglightedTextColor;
  StatusBar1.SizeGrip:=IsWindowsDefaultColorBtnFace(StatusBar1.Color) or
                       (Graphics.ColorToRGB(StatusBar1.Color)=Graphics.ColorToRGB(clBlack ));
  MouseButtonDown    :=False; EscapeEnabled:=True;
  if PanelToolTips.Visible then PanelToolTips.Hide;
  if OpenForm.PanelToolTips.Visible then OpenForm.PanelToolTips.Hide;
  OpenForm.StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
  SetMessageHandlers;

  if ShowOnStartup then // 'True': the form is merely put on the screen at the time the 'OpenForm' is opened; don't load the level selected on this form, if any
     ShowOnStartup:=False
  else with DuplicateLevelsStringGrid do begin
     OpenForm.Repaint; // it may take a while before the screen is updated if focusing the current level forces the 'Open' form to load a large collection
     if   HighlightedRowNumber>=FixedRows then begin // 'True': load the level selected on this form
          HighlightedRowNumber:=-1;
          Self.Repaint;
          DuplicateLevelsStringGridSelectCell(nil,1,Row,b);
          //TopRow:=Max(FixedRows,
          //            Min(RowCount-VisibleRowCount,
          //                Row-(Max(0,Pred(VisibleRowCount)) div 2))); // center the row in the string grid // no; the consequence would be that a user click on a specific row in the grid would be ignored in favor of focusing the old row
          end
     else Self.Repaint;
     end;
  ShowStatus;
end;

procedure TDuplicatesForm.FormDeactivate(Sender: TObject);
begin
  Application.OnMessage:=OldOnMessage;
  Application.OnIdle   :=OldOnIdle;
  Application.OnHint   :=OldOnHint;

  if PanelToolTips.Visible then PanelToolTips.Hide;
  StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
  StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';

  with DuplicateLevelsStringGrid do
    if HighlightedRowNumber>=FixedRows then begin
       //HighlightedRowNumber:=-1;
       Repaint;
       end;

  //with OpenForm.CollectionStringGrid do
  //  if Visible then Repaint;

  if Assigned( OpenForm ) then begin
     if Screen.ActiveForm <> OpenForm then
        OpenForm.SetFocus;
     OpenForm.BringToFront;
     end;
end;

procedure TDuplicatesForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=(not IsBusy) and CloseBtn.Visible;
  ScanState:=ssCloseWindow; // ''ssCloseWindow': closing the window has been requested
end;

procedure TDuplicatesForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if      Sender=Self then
          Action:=caHide
  else    Action:=caFree;

  RichEdit1.Clear;
  SetRowCount(0); // clear the string grid
  if Assigned(DirectoryList) then DirectoryList.Clear;
  if Assigned(FileList     ) then FileList     .Clear;
  if Assigned(LevelIndex   ) then LevelIndex   .Clear;

  OpenForm.EscapeEnabled:=False;
end;

procedure TDuplicatesForm.OnCloseWindow(var Msg: TMessage); // Message Misc_.MSG_CLOSE;
begin
  Close;
end;

procedure TDuplicatesForm.OnRefresh(var Msg: TMessage); // Message Misc_.Refresh;
begin
  ViewMenuItemRefreshClick(nil);
end;

procedure TDuplicatesForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize:=(NewWidth>=MinimumWidth) and (NewHeight>=MinimumHeight);
end;

procedure TDuplicatesForm.FormResize(Sender: TObject);
var ACol,W,MinWidth,MinHeight:Integer;
begin
  MinWidth :=Self.MinimumWidth;
  MinHeight:=Self.MinimumHeight;
  if      ClientWidth < MinWidth  then ClientWidth :=MinWidth
  else if ClientHeight< MinHeight then ClientHeight:=MinHeight
       else begin
         HelpBtn  .Left:=BottomPanel.ClientWidth   -HelpBtn  .Width-DuplicateLevelsGroupBox.Left;
         CancelBtn.Left:=HelpBtn  .Left-CancelBtn.Width-DuplicateLevelsGroupBox.Left;
//       CloseBtn    .Left:=CancelBtn.Left-CloseBtn    .Width-DuplicateLevelsGroupBox.Left;
         CloseBtn    .Left:=CancelBtn.Left;

         BottomPanelBevel1.Left:=CancelBtn.Left-DuplicateLevelsGroupBox.Left;
         with BottomPanelBevel1 do Visible:=False and (Left>=2*DuplicateLevelsGroupBox.Left);

         with DuplicateLevelsGroupBox do begin
           Width:=Self.ClientWidth-2*Left;
           //Height:=PanelDuplicateLevelsStatus.Top+PanelDuplicateLevelsStatus.Left+PanelDuplicateLevelsStatus.Height;
           Height:=Self.ClientHeight-Left-Top-BottomPanel.Height-StatusBar1.Height;
           end;
         with PanelDuplicateLevelsStatus do begin
           Width:=DuplicateLevelsGroupBox.Width-2*Left;
           end;
         with DuplicateLevelsStatusStringGrid1 do begin
           Width:=PanelDuplicateLevelsStatus.Width-2*Left;
           ColWidths[1]:=ClientWidth-ColCount*GridLineWidth-ColWidths[0];
           while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]); // for safety; this shouldn't happen
           end;
         with DuplicateLevelsStatusStringGrid2 do begin
           Width:=DuplicateLevelsStatusStringGrid1.Width;
           W:=ClientWidth-ColWidths[0]-ColCount*GridLineWidth;
           for ACol:=1 to Pred(Pred(ColCount)) do ColWidths[ACol]:=W div (Pred(ColCount));
           for ACol:=1 to Pred(Pred(ColCount)) do Dec(W,ColWidths[ACol]);
           ColWidths[Pred(ColCount)]:=W;
           while (VisibleColCount<ColCount) and (ColWidths[Pred(ColCount)]>0) do
             ColWidths[Pred(ColCount)]:=Pred(ColWidths[Pred(ColCount)]);
           end;
         with PanelDuplicateLevelsList do begin
           Width:=PanelDuplicateLevelsStatus.Width;
           Height:=DuplicateLevelsGroupBox.ClientHeight-Top-Left;
           end;

         with RichEdit1 do
           if Visible then begin
              Align:=alNone;
              Height:=PanelDuplicateLevelsList.ClientHeight-2*Top;
              Width:=PanelDuplicateLevelsList.ClientWidth div 2;
              end;

         with DuplicateLevelsStringGrid do begin
           if RichEdit1.Visible then begin
              Align:=alNone;
              Height:=PanelDuplicateLevelsList.ClientHeight-2*Top;
              Left:=RichEdit1.Left+RichEdit1.Width;
              Width:=PanelDuplicateLevelsList.ClientWidth-Left-RichEdit1.Left;
              end;
           Tag:=Width-VerticalScrollBarWidth-TOOL_TIP_FREE_ZONE_PIXELS; // '-TOOL_TIP_FREE_ZONE_PIXELS': 'OnMouseMove' isn't triggered when the mouse is over the scrollbar, hence, this extra tooltip free zone

           MakeAllColumnsFullyVisible(DuplicateLevelsStringGrid,2*VerticalScrollBarWidth,Ord(dlcLevelName));
           GridScrollInView(DuplicateLevelsStringGrid,DuplicateLevelsStringGrid.Row);
           end;

         if Left>Screen.DeskTopLeft+Screen.DeskTopWidth -30 then Left:=Screen.DeskTopLeft+Screen.DeskTopWidth -30;
         if Top >Screen.DeskTopTop +Screen.DeskTopHeight-30 then Top :=Screen.DeskTopTop +Screen.DeskTopHeight-30;
         end;
end;

procedure TDuplicatesForm.ShowHint(Sender: TObject);
begin
  StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=GetLongHint(Application.Hint);
end;

procedure TDuplicatesForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if        Key=VK_F1 then
            MenuItemHelpClick(Sender)
  else if   (Key=VK_ESCAPE)
            or
            ((Key=VK_BROWSER_BACK) and EscapeEnabled and (ScanState=ssIdle)) then
            if   EscapeEnabled then begin
                 if   ScanState=ssScan then
                      CancelBtnClick(Sender)
                 else MenuItemExitClick(Self);
                 end
            else begin EscapeEnabled:=True;
                       OpenForm.EscapeEnabled:=True;
                 end
//else if   Key=VK_F12 then
//          if   WindowState=wsMaximized then WindowState:=wsNormal
//          else WindowState:=wsMaximized
  else;
end;

procedure TDuplicatesForm.MenuItemHelpClick(Sender: TObject);
begin
  try     OpenForm.BtnHelpClick(Sender);
  finally SetMessageHandlers;
  end;
end;

procedure TDuplicatesForm.MenuItemDeleteLevelClick(Sender: TObject);
begin
  if (not IsBusy) and EditMenuItemDeleteLevel.Enabled then with OpenForm do begin
     if CollectionGroupBox.Visible then begin
        ActiveControl:=CollectionStringGrid; ActiveList:=CollectionStringGrid;
        end
     else begin
       ActiveControl:=FileListBox1; ActiveList:=FileListBox1;
       end;
     EscapeEnabled:=False;
     OpenForm.MenuItemFileDeleteClick(OpenForm.MenuItemFileDelete);
     end;
end;

procedure TDuplicatesForm.MenuItemExitClick(Sender: TObject);
begin
  if   ScanState=ssIdle then
       Close
  else ScanState:=ssCloseWindow;
end;

procedure TDuplicatesForm.SetMessageHandlers;
begin
  Application.OnHint          :=ShowHint;
  RichEdit1  .OnMouseWheelDown:=RichEdit1MouseWheelDown;
  RichEdit1  .OnMouseWheelUp  :=RichEdit1MouseWheelUp;
end;

procedure TDuplicatesForm.CancelBtnClick(Sender: TObject);
begin
  if ScanState<>ssCloseWindow then ScanState:=ssCancel;
end;

procedure TDuplicatesForm.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Screen.Cursor<>crDefault) and (Screen.Cursor<>crHourGlass) then Screen.Cursor:=crDefault;
  if PanelToolTips.Visible then PanelToolTips.Hide;
  if OpenForm.PanelToolTips.Visible then OpenForm.PanelToolTips.Hide;
  if OpenForm.StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text<>'' then
     OpenForm.StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:='';
end;

procedure TDuplicatesForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then
     if ScanState=ssIdle then MenuItemExitClick(Sender);
end;

procedure TDuplicatesForm.EditMenuItemCopyDuplicatesListToClipboardClick(
  Sender: TObject);
const INDENTATION=SPACE+SPACE+SPACE+SPACE;
      LINE_LENGTH=80;
var   ARow,Count,DigitCount:Integer; oCursor:TCursor; s,s1,s2:String; SL:TStringList;
begin
{
  if RichEdit1.Lines.Count<>0 then begin
     oCursor:=Screen.Cursor;
     try     Clipboard.AsText:=RichEdit1.Text;
             StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=DuplicatesListCopiedToClipboardText;
     finally Screen.Cursor:=oCursor;
     end;
     end;
}
  if EditMenuItemCopyDuplicatesListToClipboard.Enabled and (not IsBusy) then with DuplicateLevelsStringGrid do begin
     Count:=0;
     for ARow:=FixedRows to Pred(RowCount) do
         if Assigned(Objects[Ord(dlcRowNo),ARow]) then Inc(Count);
     if  Count<>0 then begin
         try
           SL:=nil;
           oCursor:=Screen.Cursor;
           try     SL:=TStringList.Create;
                   SL.Capacity:=2*Count+20;
                   DigitCount:=0;
                   while Count<>0 do begin
                     Inc(DigitCount); Count:=Count div 10;
                     end;
                   s1:=SPACE;
                   while Length(s1)<DigitCount+3 do s1:=s1+SPACE;

                   SL.Add(StrLine(LINE_LENGTH));
                   SL.Add(Application.Title+SUB_TITLE_SEPARATOR+DuplicateLevelsCaptionText);
                   SL.Add(StrLine(LINE_LENGTH));
                   if DuplicateLevelsTask=fdlCurrentLevel then begin
                      if   IsAnIniFileSectionFileName(CurrentFileName) then begin
                           s2:=ExtractSectionName(CurrentFileName);
                           if StrEqual(s2,TEXT_LEVEL) then
                              s2:=ExtractFileNameWithoutPathAndExtension(ExtractIniFileName(CurrentFileName));
                           end
                      else s2:=ExtractFileName   (CurrentFileName);
                      SL.Add(TEXT_LEVEL+COLON+SPACE+s2);
                      SL.Add(Trim(DuplicateLevelsStatusStringGrid2.Cells[0,Ord(dlScanned)-Ord(dlScanned)])+SPACE+AnsiLowerCase(Trim(DuplicateLevelsStatusStringGrid1.Cells[0,Ord(dlScanningFolder)]))+COLON+SPACE+DuplicateLevelsStatusStringGrid1.Cells[1,Ord(dlScanningFolder)-Ord(dlScanning)]);
                      end
                   else begin
                      if DuplicateLevelsTask=fdlCurrentCollection then
                         SL.Add(CollectionText+COLON+SPACE+AbbreviatedFilePath(CurrentCollectionFileName,MainForm.MyDocumentsFolder));
                      SL.Add(Trim(DuplicateLevelsStatusStringGrid2.Cells[0,Ord(dlScanned)-Ord(dlScanned)])+SPACE+AnsiLowerCase(Trim(DuplicateLevelsStatusStringGrid1.Cells[0,Ord(dlScanningFolder)]))+COLON+SPACE+DuplicateLevelsStatusStringGrid1.Cells[1,Ord(dlScanningFolder)-Ord(dlScanning)]);
                      SL.Add(Copy(DuplicateLevelsGroupBox.Caption,System.Pos(ViewText+COLON,DuplicateLevelsGroupBox.Caption),MaxInt));
                      end;
                   if MatchOptionsMask<>ALL_MATCH_OPTIONS then
                      SL.Add(DuplicateLevelsStatusStringGrid1.Cells[1,Ord(dlScanning)-Ord(dlScanning)]);
                   SL.Add(DateText+COLON+SPACE+FormatDateTime(FORMAT_DATE_TIME,Now));
                   SL.Add(StrLine(LINE_LENGTH));

                   for ARow:=FixedRows to Pred(RowCount) do
                       if Assigned(Objects[Ord(dlcRowNo),ARow]) then with TLevelIndexItem(Objects[Ord(dlcRowNo),ARow]) do
                          if Assigned(DirectoryItem) and Assigned(FileItem) then begin

                             s:=MakeIniFileSectionFileName(//StrWithTrailingPathDelimiter({ExpandedFilePath(Trom(DirectoryItem.Text),MainForm.MyDocumentsFolder)}Trim(DirectoryItem.Text))+
                                                           FileItem.Text,
                                                           Name)+
                                SPACE+StrWithParenthesis(IntToStr(LevelIndex));
                             if ViewMenuItemAllLevelsDuplicates.Checked and (DuplicatesCount=0) then
                                s:=INDENTATION+s;

                             s2:=IntToStr(ARow-Pred(FixedRows));
                             while Length(s2)<DigitCount do s2:=SPACE+s2;
                             s2:=BAR+s2+BAR+SPACE;
                             if ViewMenuItemAllLevelsDuplicates.Checked and (DuplicatesCount=0) then
                                s2:=s2+INDENTATION;
                             s2:=s2+StrWithTrailingPathDelimiter({ExpandedFilePath(Trim(DirectoryItem.Text),MainForm.MyDocumentsFolder)}Trim(DirectoryItem.Text));

                             SL.Add(s2);   // add line with path
                             SL.Add(s1+s); // add line with filename and level name

                             Inc(Count);
                             if (Count mod 1000)=0 then with StatusBar1 do begin
                                Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=IntToStr(Count);
                                Repaint;
                                end;
                             end;
                   SL.Add(StrLine(LINE_LENGTH));
                   SL.Add(DuplicateLevelsReportFooterText);
                   SL.Add(StrLine(LINE_LENGTH));
                   Clipboard.AsText:=SL.Text;

                   StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=DuplicatesListCopiedToClipboardText;
           finally Screen.Cursor:=oCursor;
                   StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
                   SL.Free;
           end;
         except on E:Exception do Error(E.Message,Caption);
         end;
         end;
     end;

end;

procedure TDuplicatesForm.ViewMenuItemClick(Sender: TObject);
begin
  if not  IsBusy then
     if   Assigned(Sender) then begin
          if Sender is TMenuItem then begin
             if DuplicateLevelsTask=fdlAllLevels then // 'True': translate keyboard shortcuts; the keyboard shortcuts for 'ViewMenuCurrentCollection' items shadow the 'ViewMenuAllLevels' items
                if      Sender=ViewMenuItemCurrentCollectionAllLevels                 then Sender:=ViewMenuItemAllLevelsAllLevels
                else if Sender=ViewMenuItemCurrentCollectionUniqueLevels              then Sender:=ViewMenuItemAllLevelsUniqueLevels
                else if Sender=ViewMenuItemCurrentCollectionWithDuplicates            then Sender:=ViewMenuItemAllLevelsWithDuplicates
                else if Sender=ViewMenuItemCurrentCollectionNoDuplicates              then Sender:=ViewMenuItemAllLevelsNoDuplicates
                else if Sender=ViewMenuItemCurrentCollectionWithSolutionsAndSnapshots then Sender:=ViewMenuItemAllLevelsWithSolutionsAndSnapshots
                else if Sender=ViewMenuItemCurrentCollectionNoSolutionsAndSnapshots   then Sender:=ViewMenuItemAllLevelsNoSolutionsAndSnapshots
                else if Sender=ViewMenuItemCurrentCollectionUpdatedLevels             then Sender:=ViewMenuItemAllLevelsUpdatedLevels
                else if Sender=ViewMenuItemCurrentCollectionDuplicates                then Sender:=ViewMenuItemAllLevelsDuplicates;
             with Sender as TMenuItem do
               if Visible and Enabled and Parent.Visible then begin
                  Checked:=True;
                  with DuplicateLevelsStringGrid do
                    ShowSearchResults(TMenuItem(Sender),
                                      CellText[Ord(dlcLevelName    ),Row],
                                      CellText[Ord(dlcFileName     ),Row],
                                      CellText[Ord(dlcDirectoryName),Row]);
                  end;
             end;
          end
     else begin ViewMenuItemAllLevelsDuplicates.Checked:=True; // '.Checked' controls the string grid colors
                with DuplicateLevelsStringGrid do
                  ShowSearchResults  (nil,
                                      CellText[Ord(dlcLevelName    ),Row],
                                      CellText[Ord(dlcFileName     ),Row],
                                      CellText[Ord(dlcDirectoryName),Row]);
          end;
end;

procedure TDuplicatesForm.ToolsMenuItemUpdateSolutionsClick(
  Sender: TObject);
begin
  if (not IsBusy) and ToolsMenuItemUpdateSolutions.Enabled then begin
     Tools(ToolsMenuItemUpdateSolutions,'');
     end;
end;

procedure TDuplicatesForm.ToolsMenuItemNewCollectionClick(
  Sender: TObject);
//var
//  OldHighlightedRowNumber:Integer;
begin
  if (not IsBusy) and (Sender is TMenuItem) then with Sender as TMenuItem do
     if Visible and Enabled then with FileForm.SaveDialog1 do begin
        Title:=TitleWithOptionalSubTitle(Self.Caption,StrRemoveChar(StrRemoveChar(Caption,AMPERSAND),PERIOD));
        if   DirectoryExists(CurrentDirectoryName) then
             InitialDir:=CurrentDirectoryName
        else InitialDir:=StrWithoutTrailingPathDelimiter(GetCurrentDir);
        FilterIndex:=1;
        if        DuplicateLevelsTask=fdlCurrentLevel then
                  FileName:=Misc_.MakeNewFileName(StrWithTrailingPathDelimiter(InitialDir)+TEXT_LEVEL,SOKOBAN_FILE_NAME_EXT,True)
        else if   DuplicateLevelsTask=fdlCurrentCollection then
                  FileName:=Misc_.MakeNewFileName(StrWithTrailingPathDelimiter(InitialDir)+ExtractFileNameWithoutPathAndExtension(CurrentCollectionFileName),SOKOBAN_FILE_NAME_EXT,True)
             else FileName:=Misc_.MakeNewFileName(StrWithTrailingPathDelimiter(InitialDir)+TEXT_LEVELS,SOKOBAN_FILE_NAME_EXT,True);
        if   FileName<>'' then begin
             EscapeEnabled:=False; // so pressing [Escape] on the save dialog form doesn't close this window too
             if Execute then
                try     fIsBusy:=True; // to ensure that this form keeps the focus after the call to 'Application.ProcessMessages'
                        Self.Repaint; Application.ProcessMessages; // remove artifacts of the save dialog from the screen; it may take awhile before the screen is updated again
                        if ExtractFilePath(FileName)='' then
                           FileName:=StrWithTrailingPathDelimiter(InitialDir)+FileName;
                        if ExtractFileExt(FileName)='' then FileName:=ChangeFileExt(FileName,SOKOBAN_FILE_NAME_EXT);
                        if DirectoryExists(FileName) then
                           Msg(Format(DirectoryExistsText__,[AbbreviatedFilePath(FileName,MainForm.MyDocumentsFolder)]),Title,MB_OK+MB_ICONINFORMATION)
                        else
                           if FileExists(FileName)
                              and
                              (Msg(Format(FileExistsText__,[AbbreviatedFilePath(FileName,MainForm.MyDocumentsFolder)])+NL+NL+OverwriteItText,
                                   Title,
                                   MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)
                               <> IDYES) then begin
                               {cancel save operation}
                               end
                           else begin
                              Tools(Sender,FileName);
                              end;
                finally fIsBusy:=False;
                end;
             end
        else Error(TEXT_TASK_FAILED,Title)
        end;
end;

procedure TDuplicatesForm.ViewMenuItemRefreshClick(Sender: TObject);
var Result:Boolean; OldLevelName,OldFileName,OldDirectoryName:String;
begin
  if (not IsBusy) and (ScanState<>ssScan) and (ScanState<>ssCloseWindow) then with DuplicateLevelsStringGrid do begin
     OldLevelName    :=CellText[Ord(dlcLevelName    ),Row];
     OldFileName     :=CellText[Ord(dlcFileName     ),Row];
     OldDirectoryName:=CellText[Ord(dlcDirectoryName),Row];

     Result:=OpenForm.TryToSetDirectory(CurrentDirectoryName);
     if   Result and (CurrentFileName<>'') and (DuplicateLevelsTask<>fdlAllLevels) then
          Result:=TryToLoadGame(CurrentFileName);

     if   Result then Result:=FindDuplicateLevels(DuplicateLevelsTask,SelectedViewMenuItem,OldLevelName,OldFileName,OldDirectoryName);
     if   not Result then Close; // simply close the window if something went wrong, e.g., if the task was finding duplicate levels for the - at that time - current level, and the level disappeared in the meantime
     end;
end;

procedure TDuplicatesForm.SettingsMenuItemMoreSettingsClick(Sender: TObject);
var
  b,OldMainFormVisible:Boolean;
  OldForm:TForm; OldOnMessage:TMessageEvent; OldOnIdle:TIdleEvent; OldApplicationOnActivate,OldApplicationOnDeactivate,OldOnHint:TNotifyEvent;
begin
  if not IsBusy then begin
     OldMainFormVisible                  :=MainForm.Visible;
     OldForm                             :=Screen.ActiveForm;
     OldApplicationOnActivate            :=Application.OnActivate;
     OldApplicationOnDeactivate          :=Application.OnDeactivate;
     OldOnHint                           :=Application.OnHint;
     OldOnIdle                           :=Application.OnIdle;
     OldOnMessage                        :=Application.OnMessage;

     try
               with OptionsForm.TreeView1 do
                if (Selected=nil)
                   or
                   (((TSettings(Selected.AbsoluteIndex)<stToolsDuplicateFinder)
                     or
                     (TSettings(Selected.AbsoluteIndex)>stToolsDuplicateFinderAllLevelsNoOptions)
                    )
                    and
                    ((TSettings(Selected.AbsoluteIndex)<stGraphicsToolsDuplicateLevels)
                     or
                     (TSettings(Selected.AbsoluteIndex)>stGraphicsToolsDuplicateLevelsColorsHighlightTextColor)
                    )
                   )
                   or
                   (not Items[Ord(stToolsDuplicateFinder)].Expanded)
                   then begin
                   Items[Ord(stToolsDuplicateFinder)].Expand(True);
                   Selected:=Items[Ord(stToolsDuplicateFinderCurrentCollectionNoOptions)];
                   Selected:=Items[Ord(stToolsDuplicateFinderAllLevelsNoOptions)];
                   Selected:=Items[Ord(stToolsDuplicateFinderCurrentLevelMatchThresholdPercent)];
                   end;

               if   MainForm.Visible and (MainForm.WindowState=wsMaximized) then
                    MainForm.Hide
               else OldMainFormVisible:=False; // it's unnecessary to restore the main window
               MainForm.BtnOptionsClick(Self);
               if Self.Visible then Self.Repaint;

     finally   if OldMainFormVisible then begin
                  MainForm.Show;
                  OpenForm.BringToFront;
                  if Self.Visible then Self.BringToFront;
                  end;
               if Screen.ActiveForm<>OldForm then OldForm.SetFocus;
               
               Application.OnActivate  :=OldApplicationOnActivate;
               Application.OnDeactivate:=OldApplicationOnDeactivate;
               Application.OnHint      :=OldOnHint;
               Application.OnIdle      :=OldOnIdle;
               Application.OnMessage   :=OldOnMessage;
               if Screen.ActiveForm=Self then SetMessageHandlers;

               OpenForm.BoardResize(OpenForm.GameBoardImage);
               with OpenForm.CollectionStringGrid do
                 if Visible then Repaint; // repaint the string grid; the colors may have changed
               if   ViewMenuItemAllLevelsDuplicates.Checked then
                    DuplicateLevelsStringGrid.Color:=BackgroundColor1
               else DuplicateLevelsStringGrid.Color:=OpenForm.CollectionStringGrid.Color;
               if DuplicateLevelsStringGrid.Visible then DuplicateLevelsStringGrid.Repaint; // repaint the string grid; the colors may have changed
               if (HighlightedRowNumber<>-1) and (Screen.ActiveForm=Self) then begin
                  HighlightedRowNumber:=-1; // so the next line forces an update of the thumbnail view of the selected level, if any
                  DuplicateLevelsStringGridSelectCell(nil,1,DuplicateLevelsStringGrid.Row,b); // reload the currently selected level
                  end
               else with OpenForm do
                      if CurrentFileName<>'' then TryToLoadFile(CurrentFileName);
     end;
     end;
end;

procedure TDuplicatesForm.ToolsMenuItemSettingsClick(
  Sender: TObject);
begin
  if (Sender is TMenuItem) and (not IsBusy) then with Sender as TMenuItem do
     Checked:=not Checked;
end;

procedure TDuplicatesForm.MenuItemWindowSizeClick(Sender: TObject);
var oCursor:TCursor;
begin
  if      Sender=MenuItemWindowSizeDefault then begin
          if WindowState<>wsNormal then WindowState:=wsNormal;
          oCursor:=Screen.Cursor;
          try     Screen.Cursor:=crHourGlass;
                  Width          :=DEFAULT_FORM_WIDTH;
                  Height         :=DEFAULT_FORM_HEIGHT;
                  SetGridColumnWidths(Max(10000,DuplicateLevelsStringGrid.RowCount),10000,True);
                  FormResize(Self);
          finally Screen.Cursor  :=oCursor;
          end;
          end
  else if Sender=MenuItemWindowSizeDefaultCentered then begin
          MenuItemWindowSizeClick(MenuItemWindowSizeDefault);
          Left:=Max(0,(Screen.DeskTopWidth -Width ) div 2);
          Top :=Max(0,(Screen.DeskTopHeight-Height) div 2);
          end
  else if Sender=MenuItemWindowSizeMaximized then begin
          if   WindowState=wsNormal then
               WindowState:=wsMaximized
          else WindowState:=wsNormal;
          end;
  MenuItemWindowSizeMaximized.Checked:=Self.WindowState=wsMaximized;
end;

procedure TDuplicatesForm.RichEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  with RichEdit1 do begin
    SelLength:=0;
    SetHighlightedRowNumber(Perform(EM_EXLINEFROMCHAR,0,SelStart));
    end;
  FormKeyUp(Sender,Key,Shift);
end;

procedure TDuplicatesForm.RichEdit1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then with RichEdit1 do begin
     SetHighlightedRowNumber(Perform(EM_EXLINEFROMCHAR,0,SelStart));
     end;
//FormMouseUp(Sender,Button,Shift,X,Y); // don't perform 'FormMouseUp' here; a right-click on the form closes the window, but the user probably expects that right-clicking the rich edit list shows a popup menu
end;

procedure TDuplicatesForm.RichEdit1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  with RichEdit1 do begin
    end;
  if PanelToolTips.Visible then PanelToolTips.Hide;
end;

procedure TDuplicatesForm.RichEdit1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  //RichEdit1.Perform(EM_LINESCROLL,0,1);
  if HighlightedRowNumber<Pred(RichEdit1.Lines.Count) then HighlightedRowNumber:=Succ(HighlightedRowNumber);
  Handled:=True;
end;

procedure TDuplicatesForm.RichEdit1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  //RichEdit1.Perform(EM_LINESCROLL,0,-1);
  if HighlightedRowNumber>0 then HighlightedRowNumber:=Pred(HighlightedRowNumber);
  Handled:=True;
end;


function  TDuplicatesForm.ShowStatus:Boolean; // the return value has no particular meaning
var i:Integer;
begin
  with DuplicateLevelsStringGrid do
     Result:=(HighlightedRowNumber>=FixedRows) and
             (HighlightedRowNumber<RowCount) and
             Assigned(Objects[Ord(dlcRowNo),HighlightedRowNumber]) and
             (OpenForm.FileListBox1.ItemIndex<>-1) and
             (OpenForm.CurrentFileName<>'');
  if Result then begin
     i:=GetDriveType(PChar(OpenForm.DriveComboBox1.Drive+':\'));
     Result:=(i>1) and (i<>DRIVE_CDROM); // 'True': the drive is probably writeable
     end;
  if Result then
     if   OpenForm.CollectionGroupBox.Visible then
          Result:=OpenForm.CollectionStringGrid.RowCount>1 // '1': deletion of the last member of a collection is not implemented
     else Result:=OpenForm.FileListBox1FileName<>'';

  EditMenuItemDeleteLevel.Enabled:=Result;
  PopupMenuItemDeleteLevel.Enabled:=EditMenuItemDeleteLevel.Enabled;

  ViewMenuItemCurrentCollectionUpdatedLevels.Visible:=(DuplicateLevelsTask<>fdlCurrentCollection) or (UpdatedLevelsCount<>0);
  ViewMenuItemCurrentCollectionUpdatedLevels.Enabled:=ViewMenuItemCurrentCollectionUpdatedLevels.Visible;
  ViewMenuItemAllLevelsUpdatedLevels.Visible:=UpdatedLevelsCount<>0;
  ViewMenuItemAllLevelsUpdatedLevels.Enabled:=ViewMenuItemAllLevelsUpdatedLevels.Visible;

  ToolsMenuItemNewCollectionCurrentlyDisplayedLevels.Visible:=DuplicateLevelsTask=fdlCurrentCollection;
  ToolsMenuItemNewCollectionCurrentlyDisplayedLevels.Enabled:=ToolsMenuItemNewCollectionCurrentlyDisplayedLevels.Visible and DuplicateLevelsStringGrid.Visible and (not ViewMenuItemCurrentCollectionDuplicates.Checked);
end;

procedure TDuplicatesForm.SetHighlightedRowNumber(Row__:Integer);
var oHighlightedRowNumber:Integer; FileName:String;
begin //SetHighlightedRowNumber
  if DuplicateLevelsStringGrid.Visible then with DuplicateLevelsStringGrid do begin
     if (Row__>=FixedRows) and Assigned(Objects[Ord(dlcRowNo),Row__]) then begin
        //oHighlightedRowNumber:=HighlightedRowNumber;
        if (Row__<>HighlightedRowNumber) then begin
           fHighlightedRowNumber:=Row__;
           if Assigned(Objects[Ord(dlcRowNo),Row__]) then with TLevelIndexItem(Objects[Ord(dlcRowNo),Row__]) do
              if Assigned(DirectoryItem) and Assigned(FileItem) then begin
                 FileName:=MakeIniFileSectionFileName(StrWithTrailingPathDelimiter(ExpandedFilePath(Trim(DirectoryItem.Text),MainForm.MyDocumentsFolder))+FileItem.Text,Name);
                 TryToLoadGame(FileName);
                 end;
           StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
           StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
           Self.ShowHint(DuplicateLevelsStringGrid);
           end;
        end
     else fHighlightedRowNumber:=Row__;
     end
  else
     if RichEdit1.Visible then with RichEdit1 do begin
        oHighlightedRowNumber:=HighlightedRowNumber;
        if (Row__<>HighlightedRowNumber) then begin
           fHighlightedRowNumber:=Row__;
           end;

        if (HighlightedRowNumber>=0) and (HighlightedRowNumber<Lines.Count) then begin
           SelStart :=Perform(EM_LINEINDEX,Row__,0);
           SelLength:=Perform(EM_LINELENGTH,SelStart,0){+Length(NL)};
           end
        else begin
           SelLength:=0;
           end;

        ScrollCaretIntoView(RichEdit1);

        if (oHighlightedRowNumber<>HighlightedRowNumber) and (SelLength<>0) and (SelText<>IncompleteListText) then begin
           TryToLoadGame(ExpandedFilePath({Trim(Copy(SelText,1,Pred(StrLastPos(SPACE,SelText))))}Trim(SelText),MainForm.MyDocumentsFolder));
           StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
           StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
           Self.ShowHint(DuplicateLevelsStringGrid);
           end;
        end
     else
        fHighlightedRowNumber:=-1;
end;

procedure TDuplicatesForm.DuplicateLevelsStringGridDrawCell(
  Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var S:String;
begin
  if Sender is TStringGrid then with Sender as TStringGrid do begin
     if Sender=DuplicateLevelsStringGrid then begin
        if (ACol>=FixedCols) and (ARow>=FixedRows) then begin
           if (ARow<RowCount) and Assigned(Objects[Ord(dlcRowNo),ARow]) then with TLevelIndexItem(Objects[Ord(dlcRowNo),ARow]) do begin
              if (ARow=Row) and (ARow=HighlightedRowNumber) and ((not ViewMenuItemAllLevelsDuplicates.Checked) or (ACol=Ord(dlcLevelName))) and (Screen.ActiveForm=Self) then begin
                 if ViewMenuItemAllLevelsDuplicates.Checked then begin
                    Canvas.Font .Color:=HighlightTextColor;
                    Canvas.Brush.Color:=HighlightBackgroundColor;
                    end
                 else begin
                    Canvas.Font .Color:=OpenForm.CollectionHighlightTextColor; //clHighlightText;
                    Canvas.Brush.Color:=OpenForm.CollectionHighlightBackgroundColor; //clHighlight;
                    end;
                 end
              else if ViewMenuItemAllLevelsDuplicates.Checked then begin
                      if DuplicatesCount<>0 then begin
                         Canvas.Font .Color:=TextColor1;
                         Canvas.Brush.Color:=BackgroundColor1;
                         end
                      else begin
                         Canvas.Font .Color:=TextColor2;
                         Canvas.Brush.Color:=BackgroundColor2;
                         end;
                      end
                   else begin
                      //Canvas.Font .Color:=Font.Color; //clWindowText;
                      //Canvas.Brush.Color:=Color; //Color; //clWindow;
                      Canvas.Font .Color:=OpenForm.CollectionStringGrid.Font.Color;
                      Canvas.Brush.Color:=OpenForm.CollectionStringGrid.Color;
                      end;
              end
           else
              if ARow=Row then begin
                 if ViewMenuItemAllLevelsDuplicates.Checked then begin
                    Canvas.Font .Color:=HighlightTextColor;
                    Canvas.Brush.Color:=HighlightBackgroundColor;
                    end
                 else begin
                    Canvas.Font .Color:=OpenForm.CollectionHighlightTextColor; //clHighlightText;
                    Canvas.Brush.Color:=OpenForm.CollectionHighlightBackgroundColor; //clHighlight;
                    end;
                 end
              else begin
                 Canvas.Font .Color:=clBtnText; //clWindowText;
                 Canvas.Brush.Color:=clBtnFace; //Color; //clWindow;
                 end;
           end
        else begin // fixed cols/rows
           Canvas.Font .Color:=clBtnText; //clWindowText;
           Canvas.Brush.Color:=clBtnFace; //Color; //clWindow;
           end;
        S:=CellText[ACol,ARow];
        end
     else begin // not 'DuplicateLevelsStringGrid'
        if (ACol>=FixedCols) and (ARow>=FixedRows) then begin
           Canvas.Font .Color:=Font.Color; //clWindowText;
           Canvas.Brush.Color:=Color; //clWindow;
           end
        else begin // fixed cols/rows
           Canvas.Font .Color:=Font.Color; //clWindowText;
           Canvas.Brush.Color:=Color; //clWindow;
           end;
        S:=Cells[ACol,ARow];
        end;

     Canvas.FillRect(Rect);

     if S<>'' then begin
        S:=StrWithQuotedAmpersands(S);
        Inc(Rect.Left,2); Dec(Rect.Right,2); Dec(Rect.Bottom);
        if   Sender=DuplicateLevelsStringGrid then begin
             if   ARow>=FixedRows then
                  if   ACol<>Ord(dlcRowNo) then
                       Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_LEFT   or DT_SINGLELINE,nil)
                  else Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_RIGHT  or DT_SINGLELINE,nil)
             else if   ACol<>0 then
                       Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_CENTER or DT_SINGLELINE,nil)
                  else Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_RIGHT  or DT_SINGLELINE,nil);
             end
        else if   (Sender=DuplicateLevelsStatusStringGrid2) and (ACol>0) then
                  Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_CENTER or DT_SINGLELINE,nil)
             else if   (Sender<>DuplicateLevelsStatusStringGrid1) or {(ACol<>1) or (ARow<>(Ord(dlScanning)-Ord(dlScanning)))} True then
                       Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_LEFT   or DT_SINGLELINE,nil)
                  else Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),Rect,DT_VCENTER or DT_RIGHT  or DT_SINGLELINE,nil);
        //Windows.ExtTextOut(Canvas.Handle, Rect.Left + 2, Rect.Top + 2,
        //                   ETO_CLIPPED or ETO_OPAQUE, @Rect, PChar(S), Length(S), nil);
        Inc(Rect.Bottom); Inc(Rect.Right,2); Dec(Rect.Left,2);
        end;
    end;
end;

procedure TDuplicatesForm.DuplicateLevelsStringGridKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_RETURN) and (Shift=[ssCtrl]) and (not IsBusy) and // '[ssCtrl]': a clean [Return] without 'Alt', 'Ctrl', or 'Shift' keys doesn't work because menu selections with [Enter] falls through to this function when the string grid has focus
     (HighlightedRowNumber>=DuplicateLevelsStringGrid.FixedRows) then begin
     OpenForm.SetFocus;
     OpenForm.BtnOpenClick(Sender);
     end;
end;

procedure TDuplicatesForm.DuplicateLevelsStringGridDblClick(Sender: TObject);
begin
  if (not IsBusy) and // '[ssCtrl]': a clean [Return] without 'Alt', 'Ctrl', or 'Shift' keys doesn't work because menu selections with [Enter] falls through to this function when the string grid has focus
     (HighlightedRowNumber>=DuplicateLevelsStringGrid.FixedRows) then begin
     OpenForm.SetFocus;
     OpenForm.BtnOpenClick(Sender);
     end;
end;

procedure TDuplicatesForm.DuplicateLevelsStringGridMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var ACol,ARow:Integer;
begin
  with DuplicateLevelsStringGrid do begin
    MouseButtonDown:=False;
    MouseToCell(X, Y, ACol, ARow);
    if (ACol<ColCount) and (ARow<RowCount ) then
       if (Button=mbLeft) and (ARow=0) and (not Dragging) and (Screen.Cursor=crHSplit) then
          MouseButtonDown:=True;
    if (Button<>mbLeft) and (ARow>=FixedRows) and (ARow<RowCount) and (ARow<>Row) and Assigned(Objects[Ord(dlcRowNo),ARow]) then
       Row:=ARow;
    end;
end;

procedure TDuplicatesForm.DuplicateLevelsStringGridMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
const SPLIT_DELTA=8;
var   i,j,ACol,ARow:Integer; CellText:String;
begin
  with DuplicateLevelsStringGrid do begin
    MouseToCell(X,Y,ACol,ARow);

    if OpenForm.ToolTips.Enabled then
       if (RowCount>FixedRows) then begin

          if (ARow>=FixedRows) and (ARow<RowCount) and (ACol<>Ord(dlcRowNo)) and Assigned(Objects[Ord(dlcRowNo),ARow]) and (X<Tag) then with PanelToolTips do begin
             CellText:=StrWithQuotedAmpersands(Self.CellText[ACol,ARow]);
             if Caption<>CellText then begin
                if Visible then Hide;
                Caption:=CellText;
                Width:=Self.Canvas.TextWidth(Caption)+8;
                end;
             Left:=Min(DuplicateLevelsGroupBox.Left+PanelDuplicateLevelsList.Left+DuplicateLevelsStringGrid.Left+X+OpenForm.ToolTips.OffsetX,
                       DuplicateLevelsGroupBox.Left+PanelDuplicateLevelsList.Left+DuplicateLevelsStringGrid.Left+DuplicateLevelsStringGrid.ClientWidth-Width);
             Top :=Min(DuplicateLevelsGroupBox.Top +PanelDuplicateLevelsList.Top +DuplicateLevelsStringGrid.Top +Y+OpenForm.ToolTips.OffsetY,
                       DuplicateLevelsGroupBox.Top +PanelDuplicateLevelsList.Top +DuplicateLevelsStringGrid.Top +DuplicateLevelsStringGrid.ClientHeight-Height);
             if not PanelToolTips.Visible then PanelToolTips.Show;
             end
          else if PanelToolTips.Visible then PanelToolTips.Hide;
          end
       else if PanelToolTips.Visible then PanelToolTips.Hide
            else
    else if PanelToolTips.Visible then PanelToolTips.Hide;

    if ARow=0 then begin
       if MouseButtonDown and (Screen.Cursor=crhSplit) then begin
          j:=0;
          for i:=0 to Pred(DragPoint.X) do Inc(j,ColWidths[i]+GridLineWidth);
          if j>=X then begin
             if ColWidths[Pred(DragPoint.X)]-(j-X)> 2*VerticalScrollBarWidth then begin
                ColWidths[Pred(DragPoint.X)]      :=ColWidths[Pred(DragPoint.X)]-(j-X);
                ColWidths[     DragPoint.X ]      :=ColWidths[     DragPoint.X ]+(j-X);
                end;
             end
          else
             if ColWidths[     DragPoint.X ]       > 2*VerticalScrollBarWidth then begin
                ColWidths[     DragPoint.X ]       :=ColWidths[     DragPoint.X ]+(j-X);
                ColWidths[Pred(DragPoint.X)]       :=ColWidths[Pred(DragPoint.X)]-(j-X);
                end;
          MakeAllColumnsFullyVisible(DuplicateLevelsStringGrid,2*VerticalScrollBarWidth,Ord(dlcLevelName));
          end
       else begin
          j:=0; DragPoint.X:=-1;
          for i:=0 to Pred(ACol) do Inc(j,ColWidths[i]+GridLineWidth);
          if (X-j<=SPLIT_DELTA) then begin
             DragPoint.X:=ACol;
             Screen.Cursor:=crHSplit;
             end
          else if ACol<Pred(ColCount) then begin
                  Inc(j,ColWidths[ACol]+GridLineWidth);
                  if j-X<=SPLIT_DELTA then begin
                     DragPoint.X:=Succ(ACol);
                     Screen.Cursor:=crHSplit;
                     end;
                  end;
          if (Screen.Cursor<>crDefault) and (Screen.Cursor<>crHourGlass) and (DragPoint.X<0) then begin
             Screen.Cursor:=crDefault;
             end;
          end;
       end
    else begin
       if (Screen.Cursor<>crDefault) and (Screen.Cursor<>crHourGlass) then Screen.Cursor:=crDefault;
       end;

    if        Screen.Cursor=crHSplit then
              Hint:=HintChangeColumnWidthText
    else if   (ACol>=0) and (ACol<ColCount) and (ARow>=0) and (ARow<RowCount) and
              Assigned(Objects[Ord(dlcRowNo),ARow]) then
              Hint:=HintDuplicateLevelsSelectLevelToSeeAThumbnailView
         else Hint:='';

    if StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text<>Hint then Self.ShowHint(Sender);
    if OpenForm.PanelToolTips.Visible then OpenForm.PanelToolTips.Hide;
    if OpenForm.StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text<>'' then
       OpenForm.StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:='';
    end;
end;

procedure TDuplicatesForm.DuplicateLevelsStringGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with DuplicateLevelsStringGrid do begin
    if Dragging then EndDrag(False);
    if (Screen.Cursor<>crDefault) and (Screen.Cursor<>crHourGlass) then Screen.Cursor:=crDefault;
    MouseButtonDown:=False;
    if Button=mbLeft then begin
       //MouseToCell(X,Y,ACol,ARow);
       //SetHighlightedRowNumber(Row);
       end;
    end;
//FormMouseUp(Sender,Button,Shift,X,Y); // don't perform 'FormMouseUp' here; a right-click on the form closes the window, but the user probably expects that right-clicking the string grid shows a popup menu
end;

procedure TDuplicatesForm.DuplicateLevelsStringGridSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  with DuplicateLevelsStringGrid do begin
    CanSelect:=(ARow>=FixedRows) and (ARow<RowCount) and
               (Assigned(Objects[Ord(dlcRowNo),ARow])
                or
                ((RowCount=Succ(FixedRows)) and (ARow=FixedRows)));
    if CanSelect then HighlightedRowNumber:=ARow;
    end;
end;

procedure TDuplicatesForm.PopupMenuPopup(Sender: TObject);
begin
  if PanelToolTips.Visible then PanelToolTips.Hide;
  PopupMenuItemDeleteLevel.Enabled:=EditMenuItemDeleteLevel.Enabled;
  PopupMenuItemCopyDuplicatesListToClipboard.Enabled:=EditMenuItemCopyDuplicatesListToClipboard.Enabled;
end;

procedure TDuplicatesForm.SetMatchThresholdPercent(Percent__:Integer);
begin
  if (Percent__>=0) and (Percent__<=100) then fMatchThresholdPercent:=Percent__;
end;

function TDuplicatesForm.LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
var i,MinHeight,MinWidth:Integer; s,Section:String; MenuItem:TMenuItem;
begin
  Result:=True;
  with IniFile do begin
    MinWidth           :=Self.MinimumWidth;
    MinHeight          :=Self.MinimumHeight;
    Left               :=Max(Screen.DeskTopLeft,Min(Screen.DeskTopLeft+Screen.DeskTopWidth -MinWidth,IniFile.ReadInteger(DUPLICATES_INIFILE_SECTION,'Left',Left)));
    Top                :=Max(Screen.DeskTopTop ,Min(Screen.DeskTopTop +Screen.DeskTopHeight-MinHeight-40,IniFile.ReadInteger(DUPLICATES_INIFILE_SECTION,'Top',Top)));
    Width              :=Max(MinWidth ,Min(Screen.DeskTopWidth -Left,IniFile.ReadInteger(DUPLICATES_INIFILE_SECTION,'Width',Width)));
    Height             :=Max(MinHeight,Min(Screen.DeskTopHeight-Top-40,IniFile.ReadInteger(DUPLICATES_INIFILE_SECTION,'Height',Height))); // -40: normally sufficient to avoid collision with start-menu
    if   ReadBool(DUPLICATES_INIFILE_SECTION,'Maximized',WindowState=wsMaximized) then
         if   WindowState<>wsMaximized then begin {DoMaximize:=True;} WindowState:=wsMaximized; end
         else
    else if WindowState=wsMaximized then WindowState:=wsNormal;

    AdvancedIdentityCheck:=ReadBool(DUPLICATES_INIFILE_SECTION,'AdvancedIdentityCheck',AdvancedIdentityCheck);
    MatchThresholdPercent:=ReadInteger(DUPLICATES_INIFILE_SECTION,'MatchThresholdPercent',MatchThresholdPercent);
    BackgroundColor1:=TColor(IniFile.ReadInteger(DUPLICATES_INIFILE_SECTION,'BackgroundColor1',BackgroundColor1));
    TextColor1:=TColor(IniFile.ReadInteger(DUPLICATES_INIFILE_SECTION,'TextColor1',TextColor1));
    BackgroundColor2:=TColor(IniFile.ReadInteger(DUPLICATES_INIFILE_SECTION,'BackgroundColor2',BackgroundColor2));
    TextColor2:=TColor(IniFile.ReadInteger(DUPLICATES_INIFILE_SECTION,'TextColor',TextColor2));
    HighlightBackgroundColor:=TColor(IniFile.ReadInteger(DUPLICATES_INIFILE_SECTION,'HighlightBackgroundColor',HighlightBackgroundColor));
    HighlightTextColor:=TColor(IniFile.ReadInteger(DUPLICATES_INIFILE_SECTION,'HighlightTextColor',HighlightTextColor));
    SettingsMenuItemToolsUpdateSolutionsAllSolutions.Checked:=IniFile.ReadBool(DUPLICATES_INIFILE_SECTION,'Tools.UpdateSolutions.KeepAllSolutions',SettingsMenuItemToolsUpdateSolutionsAllSolutions.Checked);
    SettingsMenuItemToolsUpdateSolutionsBestSolutionsOnly.Checked:=not SettingsMenuItemToolsUpdateSolutionsAllSolutions.Checked;
    SettingsMenuItemToolsNewCollectionAllSolutions.Checked:=IniFile.ReadBool(DUPLICATES_INIFILE_SECTION,'Tools.NewCollection.KeepAllSolutions',SettingsMenuItemToolsNewCollectionAllSolutions.Checked);
    SettingsMenuItemToolsNewCollectionBestSolutionsOnly.Checked:=not SettingsMenuItemToolsNewCollectionAllSolutions.Checked;

    LoadStringGridColumnWidthsFromIniFile(IniFile,DUPLICATES_INIFILE_SECTION,DuplicateLevelsStringGrid);

    i:=0; Section:=DuplicateLevelsCaptionText+SUB_TITLE_SEPARATOR+FoldersText;
    repeat Inc(i);
           s:=KeepDataPathUpToDate(ReadString(Section,IntToStr(i),''));
           MenuItem:=AddDirectory(s);
           if Assigned(MenuItem) then MenuItem.Checked:=ReadBool(Section,s,MenuItem.Checked);
    until  s='';
    with OpenForm.MenuItemDuplicateLevelsDefaultLevelFolder do
      Checked:=ReadBool(Section,TITLE_ILLEGAL_FIRST_CHARACTER+Caption+TITLE_ILLEGAL_FIRST_CHARACTER,Checked);
    with OpenForm.MenuItemDuplicateLevelsCurrentLevelFolder do
      Checked:=ReadBool(Section,TITLE_ILLEGAL_FIRST_CHARACTER+Caption+TITLE_ILLEGAL_FIRST_CHARACTER,Checked);

    Result:=LoadFontFromIniFile(IniFile,DUPLICATES_INIFILE_SECTION,'Window',Self.Font) and Result;
    OnFontChange;
    end;
end;

function TDuplicatesForm.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
var i:Integer; s,Section:String;
begin
  Result:=True;
  with IniFile do begin
   if  WindowState=wsNormal then begin
       WriteInteger(DUPLICATES_INIFILE_SECTION,'Left',Left);
       WriteInteger(DUPLICATES_INIFILE_SECTION,'Top',Top);
       WriteInteger(DUPLICATES_INIFILE_SECTION,'Width',Width);
       WriteInteger(DUPLICATES_INIFILE_SECTION,'Height',Height);
       end;
   WriteBool(DUPLICATES_INIFILE_SECTION,'Maximized',WindowState=wsMaximized);

   WriteBool   (DUPLICATES_INIFILE_SECTION,'AdvancedIdentityCheck',AdvancedIdentityCheck);
   WriteInteger(DUPLICATES_INIFILE_SECTION,'MatchThresholdPercent',MatchThresholdPercent);
   WriteInteger(DUPLICATES_INIFILE_SECTION,'BackgroundColor1',BackgroundColor1);
   WriteInteger(DUPLICATES_INIFILE_SECTION,'TextColor1',TextColor1);
   WriteInteger(DUPLICATES_INIFILE_SECTION,'BackgroundColor2',BackgroundColor2);
   WriteInteger(DUPLICATES_INIFILE_SECTION,'TextColor',TextColor2);
   WriteInteger(DUPLICATES_INIFILE_SECTION,'HighlightBackgroundColor',HighlightBackgroundColor);
   WriteInteger(DUPLICATES_INIFILE_SECTION,'HighlightTextColor',HighlightTextColor);
   WriteBool   (DUPLICATES_INIFILE_SECTION,'Tools.UpdateSolutions.KeepAllSolutions',SettingsMenuItemToolsUpdateSolutionsAllSolutions.Checked);
   WriteBool   (DUPLICATES_INIFILE_SECTION,'Tools.NewCollection.KeepAllSolutions',SettingsMenuItemToolsNewCollectionAllSolutions.Checked);

   SaveStringGridColumnWidthsToIniFile(IniFile,DUPLICATES_INIFILE_SECTION,DuplicateLevelsStringGrid);

   Section:=DuplicateLevelsCaptionText+SUB_TITLE_SEPARATOR+FoldersText;
   IniFile.EraseSection(Section);
   for i:=0 to Pred(OpenForm.MenuItemDuplicateLevelsFolderSeparator.MenuIndex) do
       with OpenForm.MenuItemSubMenuFindDuplicateLevelsFolders.Items[i] do begin
         s:=ExpandedFilePath(StrRemoveChar(Caption,AMPERSAND),MainForm.MyDocumentsFolder);
         WriteString(Section,IntToStr(Succ(i)),s);
         WriteBool(Section,s,Checked);
         end;
   with OpenForm.MenuItemDuplicateLevelsDefaultLevelFolder do
     WriteBool(Section,TITLE_ILLEGAL_FIRST_CHARACTER+StrRemoveChar(Caption,AMPERSAND)+TITLE_ILLEGAL_FIRST_CHARACTER,Checked);
   with OpenForm.MenuItemDuplicateLevelsCurrentLevelFolder do
     WriteBool(Section,TITLE_ILLEGAL_FIRST_CHARACTER+StrRemoveChar(Caption,AMPERSAND)+TITLE_ILLEGAL_FIRST_CHARACTER,Checked);

   Result:=SaveFontToIniFile(IniFile,DUPLICATES_INIFILE_SECTION,'Window',Self.Font) and Result;
   end;
end;

function TDuplicatesForm.MinimumHeight:Integer;
begin
  Result:=DuplicateLevelsGroupBox.Top+PanelDuplicateLevelsStatus.Top+PanelDuplicateLevelsStatus.Left+PanelDuplicateLevelsStatus.Height+DuplicateLevelsGroupBox.Left+BottomPanel.Height+StatusBar1.Height+80+HorizontalScrollBarHeight;
end;

function TDuplicatesForm.MinimumWidth:Integer;
begin
  Result:=4*CloseBtn.Width+4*DuplicateLevelsGroupBox.Left+2*BottomPanel.BevelWidth;
end;

procedure TDuplicatesForm.SetGridColumnWidths(RowCount__,LevelNumbers__:Integer; SetDefaultColumnWidths__:Boolean);
var ACol,W:Integer; s:String;
begin
  with DuplicateLevelsStringGrid do begin
    s:=IntToStr(Max(RowCount,RowCount__))+SPACE+SPACE;
    ColWidths[Ord(dlcRowNo)]:=8+Max(Canvas.TextWidth(s),Canvas.TextWidth(Cells[Ord(dlcRowNo),0]));
    //s:=IntToStr(LevelNumbers__)+SPACE+SPACE;
    //ColWidths[Ord(dlcLevelNo)]:=8+Max(Canvas.TextWidth(s),Canvas.TextWidth(Cells[Ord(dlcLevelNo),0]));
    if SetDefaultColumnWidths__ then begin
       W:=ClientWidth-ColCount*GridLineWidth;
       for ACol:=0 to Pred(ColCount) do
           if (ACol<>Ord(dlcLevelName)) and (ACol<>Ord(dlcFileName)) and (ACol<>Ord(dlcDirectoryName)) then Dec(W,ColWidths[ACol]);
       ColWidths[Ord(dlcLevelName)]:=W div 3; Dec(W,ColWidths[Ord(dlcLevelName)]);
       ColWidths[Ord(dlcFileName)]:=W div 2; Dec(W,ColWidths[Ord(dlcFileName)]);
       ColWidths[Ord(dlcDirectoryName)]:=W;
       end;
    LeftCol:=FixedCols;
    MakeAllColumnsFullyVisible(DuplicateLevelsStringGrid,2*VerticalScrollBarWidth,Ord(dlcLevelName));
    end;
end;

function TDuplicatesForm.AddDirectory(const Directory__:String):TMenuItem;
var Index:Integer; NewDirectory,s:String;
begin
  Result:=nil;
  if (Directory__<>'') and DirectoryExists(Directory__) then
     if StrEqual(StrWithoutTrailingPathDelimiter(Directory__),StrWithoutTrailingPathDelimiter(MainForm.DefaultLevelFolder)) then
        Result:=OpenForm.MenuItemDuplicateLevelsDefaultLevelFolder
     else begin
        NewDirectory:=StrWithTrailingPathDelimiter(AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(Directory__),MainForm.MyDocumentsFolder));
        for Index:=Pred(OpenForm.MenuItemDuplicateLevelsFolderSeparator.MenuIndex) downto 0 do begin
            s:=StrWithTrailingPathDelimiter(StrRemoveChar(OpenForm.MenuItemSubMenuFindDuplicateLevelsFolders.Items[Index].Caption,AMPERSAND));
            if StrEqual(NewDirectory,s) then
               Result:=OpenForm.MenuItemSubMenuFindDuplicateLevelsFolders.Items[Index];
            end;

        if Assigned(Result) then
           Result.Checked:=True
        else // add the new directory
           try    Result:=TMenuItem.Create(OpenForm);
                  Result.Caption:=StrWithoutTrailingPathDelimiter(NewDirectory);
                  Result.Checked:=True;
                  Result.Hint   :=OpenForm.MenuItemDuplicateLevelsDefaultLevelFolder.Hint;
                  Result.OnClick:=OpenForm.MenuItemDuplicateLevelsDefaultLevelFolder.OnClick;

                  Index:=Pred(OpenForm.MenuItemDuplicateLevelsFolderSeparator.MenuIndex);
                  while (Index>=0) and
                        (AnsiCompareText(Result.Caption,StrRemoveChar(OpenForm.MenuItemSubMenuFindDuplicateLevelsFolders.Items[Index].Caption,AMPERSAND))<0) do
                        Dec(Index); // keep the items sorted
                  OpenForm.MenuItemSubMenuFindDuplicateLevelsFolders.Insert(Succ(Index),Result);
           except on E:Exception do begin
                      Error(E.Message,'');
                      Result:=nil;
                      end;
           end;
        end;
end;

procedure TDuplicatesForm.ClearRow(Row__:Integer);
var ACol:Integer;
begin
  with DuplicateLevelsStringGrid do
    if (Row__>=FixedRows) and (Row__<RowCount) then
       for ACol:=FixedCols to Pred(ColCount) do begin
           Cells[ACol,Row__]:=''; Objects[ACol,Row__]:=nil;
           end;
end;

function  TDuplicatesForm.GetCellText(Col__,Row__:Integer):String;
begin
  with DuplicateLevelsStringGrid do
    if   (Col__>=0) and (Col__<ColCount) and (Row__>=0) and (Row__<RowCount) then
         if             ((Col__=Ord(dlcLevelName    )) and Assigned(Objects[Ord(dlcRowNo),Row__])) then
                        with TLevelIndexItem(Objects[Ord(dlcRowNo),Row__]) do begin
                          Result:=TLevelIndexItem(Objects[Ord(dlcRowNo),Row__]).Name;
                          if (Result=TEXT_LEVEL) and Assigned(FileItem) then
                             Result:=ChangeFileExt(FileItem.Text,'');
                          end
         else if        ((Col__=Ord(dlcFileName     )) and Assigned(Objects[Ord(dlcRowNo),Row__])) then
                        with TLevelIndexItem(Objects[Ord(dlcRowNo),Row__]) do
                          if   Assigned(FileItem) then Result:=FileItem.Text
                          else Result:=Cells[Col__,Row__]
              else if   ((Col__=Ord(dlcDirectoryName)) and Assigned(Objects[Ord(dlcRowNo),Row__])) then
                        with TLevelIndexItem(Objects[Ord(dlcRowNo),Row__]) do
                          if   Assigned(DirectoryItem) then
                               if   (DirectoryItem.Text='') or (DirectoryItem.Text[1]>SPACE) then
                                    Result:=DirectoryItem.Text
                               else Result:=Trim(DirectoryItem.Text) // only call 'Trim()' when necessary
                          else Result:=Cells[Col__,Row__]
                   else Result:=Cells[Col__,Row__]
    else Result:='';
end;

function  TDuplicatesForm.SetRowCount(RowCount__:Integer):Boolean;
var OldRowCount:Integer;
begin
  with DuplicateLevelsStringGrid do
    try    if RowCount__<=FixedRows then begin
              RowCount__:=Succ(FixedRows); ClearRow(FixedRows); // the minimum number of rows is 'FixedRows' + 1
              Row:=FixedRows;
              end;
           if RowCount<>RowCount__ then begin
              OldRowCount:=RowCount;
              RowCount:=RowCount__;
              while OldRowCount<RowCount do begin // clear the objects for the new rows; this is not strictly necessary since Delphi creates new rows with empty data, but better safe than sorry
                Objects[Ord(dlcRowNo),OldRowCount]:=nil;
                Inc(OldRowCount);
                end;
              SetGridColumnWidths(Max(10000,RowCount),10000,False);
              end;
           Result:=True;
    except on E:Exception do
              Result:=Error(E.Message,Caption);
    end;
end;

function  TDuplicatesForm.TryToLoadGame(const FileName__:String):Boolean;
var Directory,FileNameWithoutLevelName:String;
begin
  Result:=False;

  if   IsAnIniFileSectionFileName(FileName__) then
       FileNameWithoutLevelName:=ExtractIniFileName(FileName__)
  else FileNameWithoutLevelName:=FileName__;
  Directory:=StrWithoutTrailingPathDelimiter(ExtractFilePath(FileNameWithoutLevelName));

  if   (Directory<>'') and DirectoryExists(Directory) then with OpenForm do begin
       DirectoryListBox1.Directory:=StrWithTrailingPathDelimiter(Directory);
       if DirectoryListBox1.Drive<>DriveComboBox1.Drive then
          DriveComboBox1.Drive:=DirectoryListBox1.Drive;
       DirectoryListBoxScrollInView(DirectoryListBox1,DirectoryListBox1.ItemIndex);
       if FileExists(FileNameWithoutLevelName) then with OpenForm.FilterComboBox1 do begin
          if ItemIndex=Pred(Items.Count) then ItemIndex:=0; // 'Pred(Items.Count): this is the 'all files' filter index; force an update to the best best matching filter
          OpenForm.SetFileFilterIndex(FileNameWithoutLevelName);
          end;
       Result:=OpenForm.TryToLoadFile(FileName__);
       end;

  ShowStatus;
end;

procedure TDuplicatesForm.NormalizeRotationsAndReflectionsAndPlayerPosition(Game__:TGame; NormalizePlayerPosition__:Boolean; var HashValue__:THashTableHashKey);
var NewBoardWidth,NewBoardHeight,Col,Row,Tiebreaker,TopLeftCol,TopLeftRow:Integer;
    b:TBoardTransformation2D; H:THashTableHashKey;
begin // select the rotation and reflection that produces the minimum board hash value;
      // output fields in 'Game__': BoardWidth, BoardHeight, StartBoard, PlayerStartPos, and OriginalBoardTransformation2D
  with Game__ do begin
    BoardTransformation2D:=t2DRotate0DegreesClockwise; OriginalBoardTransformation2D:=BoardTransformation2D;
    NewBoardWidth:=BoardWidth; NewBoardHeight:=BoardHeight;
    Board:=StartBoard; PlayerPos:=PlayerStartPos;
    HashValue__:=BoardHashValue(BoardWidth,BoardHeight,StartBoard);

    for b:=Low(TBoardTransformation2D) to Pred(High(TBoardTransformation2D)) do begin // enumerate the transformations; 'Pred': the last transformation is unnecessary because it would just bring the board back to the initial state
        DoBoardTransformation2D(t2DRotate90DegreesClockwise,True); // board transformations are relative to the current transformation
        if BoardTransformation2D=t2DRotate0DegreesClockwise then // 'True': the board has been rotated back to the initial state; flip the board horizontally and continue rotating the board
           DoBoardTransformation2D(t2DFlipHorizontally,True);
        H:=BoardHashValue(BoardWidth,BoardHeight,StartBoard);
        Tiebreaker:=0;
        if H=HashValue__ then begin
           for Col:=1 to Min(BoardWidth,NewBoardWidth) do // equal hash keys; try harder even though it doesn't guarantee finding a unique normalized transformation either; but it will have to do
               for Row:=1 to Min(BoardHeight,NewBoardHeight) do
                   if   TieBreaker=0 then
                        TieBreaker:=(Board[Col,Row] and (BOARD_PIECES-PLAYER))-(StartBoard[Col,Row] and (BOARD_PIECES-PLAYER)) // if 'Tiebreaker' becomes < 0 then the current transformation is selected
                   else break; // quick and dirty exit the inner 'for' loop when a tiebreaker result has been found
           if (TieBreaker=0) and  (PlayerPos.X<>0) then begin // True': the board may be symmetrical along an axis and there is a player on the board; use the normalized (top-left) player position as tiebreaker
              CalculatePlayersReachableSquares(PlayerPos     .X,PlayerPos     .Y,NewBoardWidth,NewBoardHeight,Board     ,False,TopLeftCol,TopLeftRow);
              CalculatePlayersReachableSquares(PlayerStartPos.X,PlayerStartPos.Y,BoardWidth   ,BoardHeight   ,StartBoard,False,Col       ,Row);
              TieBreaker:=((MAX_BOARD_WIDTH+1)*Row+Col)-((MAX_BOARD_WIDTH+1)*TopLeftRow+TopLeftCol);
              end;
           if TieBreaker=0 then TieBreaker:=BoardWidth -NewBoardWidth;
           if TieBreaker=0 then TieBreaker:=BoardHeight-NewBoardHeight;
           end;
        if (H<HashValue__) or (Tiebreaker<0) then begin // 'True': select this transformation as the representative for the board
           NewBoardWidth:=BoardWidth; NewBoardHeight:=BoardHeight; Board:=StartBoard;
           PlayerPos:=PlayerStartPos; OriginalBoardTransformation2D:=BoardTransformation2D;
           HashValue__:=H;
           end;
        end;

    BoardWidth:=NewBoardWidth; BoardHeight:=NewBoardHeight; StartBoard:=Board; BoardTransformation2D:=t2DRotate0DegreesClockwise;
    
    if   PlayerPos.X<>0 then begin {normalize the player starting position}
         CalculatePlayersReachableSquares(PlayerPos.X,PlayerPos.Y,BoardWidth,BoardHeight,Board,False,TopLeftCol,TopLeftRow);
         Dec(StartBoard[PlayerPos.X,PlayerPos.Y],PLAYER);
         Inc(StartBoard[TopLeftCol ,TopLeftRow ],PLAYER);
         PlayerStartPos.X:=TopLeftCol; PlayerStartPos.Y:=TopLeftRow; // 'PlayerStartPos' now contains the top-left normalized position whereas 'PlayerPos' contains the normalized position after 'SokFile_.NormalizeBoard' have filled tubes and thereby possibly have moved the player
         if NormalizePlayerPosition__ then
            if   ((Board[PlayerPos.X,PlayerPos.Y] and PLAYER)<>0) then begin // '<>0': this should always be true
                 Dec(Board[PlayerPos.X,PlayerPos.Y],PLAYER);
                 PlayerPos:=PlayerStartPos;
                 Inc(Board[PlayerPos.X,PlayerPos.Y],PLAYER);
                 end
            else raise Exception.Create(InternalErrorText+': NormalizeRotationsAndReflectionsAndPlayerPosition');
         end
    else PlayerStartPos:=PlayerPos;

    StartBoardAsText:=''; // invalidate an existing textual representation of the board, if any
    PlayersReachableSquaresTimestampAfterLastPush:=0; // invalidate any calculated distances to the player's reachable squares
    end;
end; // NormalizeRotationsAndReflectionsAndPlayerPosition

function  TDuplicatesForm.IsASubDirectoryName(const SubDirectory__,Directory__:String):Boolean;
begin
  Result:=StrBeginsWith(StrWithTrailingPathDelimiter(SubDirectory__),StrWithTrailingPathDelimiter(Directory__))
          and
          (Length(StrWithTrailingPathDelimiter(SubDirectory__))>Length(StrWithTrailingPathDelimiter(Directory__))); // '>': the directories aren't identical
end;

function  TDuplicatesForm.AddLevelToStringGrid(LevelIndexItem__:TLevelIndexItem; var LastRow__:Integer):Boolean;
begin
  Result:=True;
  with DuplicateLevelsStringGrid do
    try
           if   (LastRow__=Pred(RowCount)) and (RowCount<High(RowCount)) then begin
                if Dragging then EndDrag(False);
                SetRowCount(Max(Succ(RowCount),(RowCount div 4)*5)); // increase the number of rows with 25%
                end;

           if   LastRow__<Pred(RowCount) then begin
                Inc(LastRow__);
                Cells  [Ord(dlcRowNo),LastRow__]:=IntToStr(LastRow__+Pred(FixedRows))+SPACE;
                Objects[Ord(dlcRowNo),LastRow__]:=LevelIndexItem__;
                end
           else raise Exception.Create(TEXT_TASK_FAILED);

    except on E:Exception do Result:=Error(E.Message,Caption);
    end;
end;

function  TDuplicatesForm.SelectedViewMenuItem:TMenuItem;
var i:Integer; MenuItem:TMenuItem;
begin
  Result:=nil;
  if   DuplicateLevelsTask=fdlCurrentCollection then
       MenuItem:=ViewMenuCurrentCollection
  else MenuItem:=ViewMenuAllLevels;
  for i:=0 to Pred(MenuItem.Count) do with MenuItem do
      if Items[i].RadioItem and Items[i].Checked then Result:=Items[i];
end;

function  TDuplicatesForm.ItemCount(Sender__:TObject):Integer;
begin
  if      Sender__=ViewMenuItemCurrentCollectionAllLevels                 then Result:=CurrentCollectionLevelsCount
  else if Sender__=ViewMenuItemCurrentCollectionUniqueLevels              then Result:=CurrentCollectionLevelsCount-CurrentCollectionDuplicateLevelsCount
  else if Sender__=ViewMenuItemCurrentCollectionWithDuplicates            then Result:=DuplicatedLevelsCount
  else if Sender__=ViewMenuItemCurrentCollectionNoDuplicates              then Result:=CurrentCollectionLevelsCount-CurrentCollectionDuplicateLevelsCount-DuplicatedLevelsCount
  else if Sender__=ViewMenuItemCurrentCollectionWithSolutionsAndSnapshots then Result:=CurrentCollectionLevelsWithSolutionsAndSnapshotsCount
  else if Sender__=ViewMenuItemCurrentCollectionNoSolutionsAndSnapshots   then Result:=CurrentCollectionLevelsCount-CurrentCollectionLevelsWithSolutionsAndSnapshotsCount
  else if Sender__=ViewMenuItemCurrentCollectionUpdatedLevels             then Result:=UpdatedLevelsCount
  else if Sender__=ViewMenuItemCurrentCollectionDuplicates                then Result:=DuplicatedLevelsCount+DuplicateLevelsCount
  else if Sender__=ViewMenuItemAllLevelsAllLevels                         then Result:=ScannedLevelsCount
  else if Sender__=ViewMenuItemAllLevelsUniqueLevels                      then Result:=ScannedLevelsCount-DuplicateLevelsCount
  else if Sender__=ViewMenuItemAllLevelsWithDuplicates                    then Result:=DuplicatedLevelsCount
  else if Sender__=ViewMenuItemAllLevelsNoDuplicates                      then Result:=ScannedLevelsCount-DuplicatedLevelsCount-DuplicateLevelsCount
  else if Sender__=ViewMenuItemAllLevelsWithSolutionsAndSnapshots         then Result:=LevelsWithSolutionsAndSnapshotsCount
  else if Sender__=ViewMenuItemAllLevelsNoSolutionsAndSnapshots           then Result:=ScannedLevelsCount-LevelsWithSolutionsAndSnapshotsCount
  else if Sender__=ViewMenuItemAllLevelsUpdatedLevels                     then Result:=UpdatedLevelsCount
  else if Sender__=ViewMenuItemAllLevelsDuplicates                        then Result:=DuplicatedLevelsCount+DuplicateLevelsCount
  else Result:=DuplicatedLevelsCount+DuplicateLevelsCount; // the function can be called with a 'nil' parameter in which case it returns the number of duplicated levels + number of duplicates
end;

function  TDuplicatesForm.ShowSearchResults(ViewMenuItem__:TMenuItem; const FocusLevelName__,FocusFileName__,FocusDirectoryName__:String):Boolean;
var FocusRow,Index,ItemCount,LastRow,LineCount:Integer; OldIsBusy:Boolean; oCursor:TCursor; //FileName: String;
    Item,Node,PreviousNode,N:TLevelIndexItem;

  function IsADuplicate(LevelIndexItem__:TLevelIndexItem):Boolean;
  begin
    Result:=Assigned(LevelIndexItem__.NextDuplicate) and (LevelIndexItem__.DuplicatesCount=0); // '0': only the duplicated level (i.e., the first encountered instance of the level) has a non-zero count
  end;

begin // ShowSearchResults
  Result:=True;
  try
    try
      Self.Update; //Self.Repaint; // to remove any menu artefacts from the screen; it may take some time before the window is updated with the new data set

      if   Assigned(ViewMenuItem__) then
           DuplicateLevelsGroupBox.Caption:=TitleWithOptionalSubTitle(DuplicateLevelsText,ViewText+COLON+SPACE+StrRemoveChar(StrRemoveChar(ViewMenuItem__.Caption,AMPERSAND),PERIOD));
      ViewMenuItemAllLevelsDuplicates.Checked:=(ViewMenuItem__=ViewMenuItemCurrentCollectionDuplicates) or (ViewMenuItem__=ViewMenuItemAllLevelsDuplicates) or (ViewMenuItem__=nil); // '.Checked' controls the string grid colors
      if   ViewMenuItemAllLevelsDuplicates.Checked then
           DuplicateLevelsStringGrid.Color:=BackgroundColor1
      else DuplicateLevelsStringGrid.Color:=OpenForm.CollectionStringGrid.Color; //clBtnFace;
      DuplicateLevelsStringGrid.Cells[0,0]:='';

      ItemCount:=Self.ItemCount(ViewMenuItem__);

      with DuplicateLevelsStringGrid do begin
           if Assigned(ViewMenuItem__) then ClearRow(FixedRows);
           Row:=FixedRows;
           GridScrollInView(DuplicateLevelsStringGrid,Row);
           end;

      if   SetRowCount(DuplicateLevelsStringGrid.FixedRows+ItemCount) then begin
           LineCount:=0; LastRow:=Pred(DuplicateLevelsStringGrid.FixedRows);
           oCursor:=Screen.Cursor; OldIsBusy:=IsBusy;
           try
             fIsBusy:=True; Screen.Cursor:=crHourGlass;
             DuplicateLevelsStringGrid.Visible:=ItemCount<>0;
             if PanelToolTips.Visible then PanelToolTips.Hide;

             if   ItemCount<>0 then begin
                  Item:=LevelIndex.First;
                  while Assigned(Item) and Assigned(ViewMenuItem__) do begin // 'Assigned(MenuItem)': when the function is called with a 'nil' parameter it doesn't fill the string grid but only checks that it has been filled correctly
                    if Item.DuplicatesCount>0 then begin
                       Node:=Item.NextDuplicate; PreviousNode:=Item; // the list members were pushed on the list; reverse the list so it's in chronological order
                       while Node<>Item do begin
                          N:=Node.NextDuplicate;
                          Node.NextDuplicate:=PreviousNode; PreviousNode:=Node;
                          Node:=N;
                          end;
                       Item.NextDuplicate:=PreviousNode;
                       Item.DuplicatesCount:=-Item.DuplicatesCount; // quick and dirty: use negated numbers to indicate that the duplicate levels have been reversed to chronological order
                       end;

                    Node:=Item;

                    if      (ViewMenuItem__=ViewMenuItemCurrentCollectionAllLevels)                 then begin
                            if   LineCount<ItemCount                                                then // 'True': this is a level from the current collection
                                 if   AddLevelToStringGrid(Node,LastRow)                            then Inc(LineCount)
                                 else raise Exception.Create(TEXT_TASK_FAILED)
                            else Item:=nil;
                            end
                    else if (ViewMenuItem__=ViewMenuItemAllLevelsAllLevels)                         then begin
                            if   AddLevelToStringGrid(Node,LastRow)                                 then Inc(LineCount)
                            else raise Exception.Create(TEXT_TASK_FAILED);
                            end
                    else if (ViewMenuItem__=ViewMenuItemCurrentCollectionUniqueLevels)              or
                            (ViewMenuItem__=ViewMenuItemAllLevelsUniqueLevels)                      then begin
                            if not  IsADuplicate(Node)                                              then
                               if   AddLevelToStringGrid(Node,LastRow)                              then Inc(LineCount)
                               else raise Exception.Create(TEXT_TASK_FAILED);
                            end
                    else if (ViewMenuItem__=ViewMenuItemCurrentCollectionWithDuplicates)            or
                            (ViewMenuItem__=ViewMenuItemAllLevelsWithDuplicates)                    then begin
                            if Node.DuplicatesCount<>0 then
                               if   AddLevelToStringGrid(Node,LastRow)                              then Inc(LineCount)
                               else raise Exception.Create(TEXT_TASK_FAILED);
                            end
                    else if (ViewMenuItem__=ViewMenuItemCurrentCollectionNoDuplicates)              or
                            (ViewMenuItem__=ViewMenuItemAllLevelsNoDuplicates)                      then begin
                            if not  Assigned(Node.NextDuplicate)                                    then
                               if   AddLevelToStringGrid(Node,LastRow)                              then Inc(LineCount)
                               else raise Exception.Create(TEXT_TASK_FAILED);
                            end
                    else if (ViewMenuItem__=ViewMenuItemCurrentCollectionWithSolutionsAndSnapshots) then begin
                            if   LineCount<ItemCount                                                then begin // 'True': this is a level from the current collection
                                 if not  Node.Snapshots.IsEmpty then
                                    if   AddLevelToStringGrid(Node,LastRow)                         then Inc(LineCount)
                                    else raise Exception.Create(TEXT_TASK_FAILED);
                                 end
                            else Item:=nil;
                            end
                    else if (ViewMenuItem__=ViewMenuItemAllLevelsWithSolutionsAndSnapshots)         then begin
                            if not  Node.Snapshots.IsEmpty then
                               if   AddLevelToStringGrid(Node,LastRow)                              then Inc(LineCount)
                               else raise Exception.Create(TEXT_TASK_FAILED);
                            end
                    else if (ViewMenuItem__=ViewMenuItemCurrentCollectionNoSolutionsAndSnapshots)   then begin
                            if   LineCount<ItemCount                                                then begin // 'True': this is a level from the current collection
                                 if Node.Snapshots.IsEmpty then
                                    if   AddLevelToStringGrid(Node,LastRow)                         then Inc(LineCount)
                                    else raise Exception.Create(TEXT_TASK_FAILED);
                                 end
                            else Item:=nil;
                            end
                    else if (ViewMenuItem__=ViewMenuItemAllLevelsNoSolutionsAndSnapshots)           then begin
                            if Node.Snapshots.IsEmpty then
                               if   AddLevelToStringGrid(Node,LastRow)                              then Inc(LineCount)
                               else raise Exception.Create(TEXT_TASK_FAILED);
                            end
                    else if (ViewMenuItem__=ViewMenuItemCurrentCollectionUpdatedLevels)             or
                            (ViewMenuItem__=ViewMenuItemAllLevelsUpdatedLevels)                     then begin
                            if Node.HasBeenUpdated then
                               if   AddLevelToStringGrid(Node,LastRow)                              then Inc(LineCount)
                               else raise Exception.Create(TEXT_TASK_FAILED);
                            end
                    else if (ViewMenuItem__=ViewMenuItemCurrentCollectionDuplicates)                or
                            (ViewMenuItem__=ViewMenuItemAllLevelsDuplicates)                        then begin
                            if Node.DuplicatesCount<>0 then
                               repeat
                                 //with Node do FileName:=AbbreviatedFilePath(MakeIniFileSectionFileName(StrWithTrailingPathDelimiter(Trim(DirectoryItem.Text))+FileItem.Text,Name),MainForm.MyDocumentsFolder);
                                 //if Node.DuplicatesCount=0 then FileName:=MARGIN+FileName;
                                 //RichEdit1.Lines.Add(FileName+SPACE+LEFT_PAREN+IntToStr(Node.LevelIndex)+RIGHT_PAREN);
                                 if   AddLevelToStringGrid(Node,LastRow) then Inc(LineCount)
                                 else raise Exception.Create(TEXT_TASK_FAILED);
                                 if   LineCount mod 1000=0 then with StatusBar1 do begin
                                      StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=IntToStr(LineCount);
                                      StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:=PreparingListText;
                                      Repaint;
                                      //Application.ProcessMessages;
                                      end;
                                 Node:=Node.NextDuplicate;
                               until Node=Item;
                            end;

                    if Assigned(Item) then Item:=Item.NextItem;
                    end;
                  end;
           finally
             Screen.Cursor:=oCursor; fIsBusy:=OldIsBusy;
             StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
             StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';

             if RichEdit1.Visible then with RichEdit1 do with Lines do // // the names of the the duplicates may overflow the rich-edit text size; in that case, try to delete a few lines to make room for an 'incomplete' information line
                if Count<ItemCount then begin
                   if Count<>0 then Delete(Pred(Count)); // delete the last 2 lines
                   if Count<>0 then Delete(Pred(Count));
                   Lines.Add(IncompleteListText); // add an information line showing that the list is incomplete
                   end;

             with DuplicateLevelsStringGrid do
               if Visible then begin
                  if   not Assigned(ViewMenuItem__) then begin
                       LastRow:=ItemCount+Pred(FixedRows);
                       while (LastRow>=FixedRows)
                             and
                             ((LastRow>=RowCount) or (not Assigned(Objects[Ord(dlcRowNo),LastRow]))) do
                             Dec(LastRow);
                       LineCount:=LastRow-Pred(FixedRows);
                       end;

                  if   LineCount<>0 then Cells[0,0]:=IntToStr(LineCount)+SPACE;

                  if   LastRow<ItemCount+Pred(FixedRows) then begin
                       Item:=TLevelIndexItem.Create(IncompleteListText,'','',0,0,t2DRotate0DegreesClockwise,0,nil,nil,0);
                       if   Assigned(Item) then AddLevelToStringGrid(Item,LastRow)
                       else raise Exception.Create(TEXT_TASK_FAILED);
                       end;

                  SetRowCount(Succ(LastRow));
                  MakeAllColumnsFullyVisible(DuplicateLevelsStringGrid,2*VerticalScrollBarWidth,Ord(dlcLevelName));
                  HighLightedRowNumber:=-1;

                  if   ScanState<>ssCloseWindow then begin
                       ScanState:=ssIdle;
                       if LastRow>=FixedRows then begin
                          FocusRow:=FixedRows;

                          if   FocusLevelName__<>'' then
                               for Index:=FixedRows to Pred(RowCount) do
                                   if StrEqual(FocusLevelName__    ,CellText[Ord(dlcLevelName    ),Index]) and
                                      StrEqual(FocusFileName__     ,CellText[Ord(dlcFileName     ),Index]) and
                                      StrEqual(FocusDirectoryName__,CellText[Ord(dlcDirectoryName),Index]) then begin
                                      FocusRow:=Index;
                                      TopRow:=Max(FixedRows,
                                                  Min(RowCount-VisibleRowCount,
                                                      FocusRow-(Max(0,Pred(VisibleRowCount)) div 2))); // center the row in the string grid
                                      break;
                                      end;

                          if   Row<>FocusRow then Row:=FocusRow
                          else HighlightedRowNumber:=FocusRow;
                          GridScrollInView(DuplicateLevelsStringGrid,Row);
                          SetFocus;
                          end;
                       end
                  else PostMessage(Self.Handle,MSG_CLOSE,0,0);
                  Repaint;
                  end;
           end;
           end
      else raise Exception.Create(TEXT_TASK_FAILED);
    except on E:Exception do Result:=Error(E.Message,Caption);
    end;
  finally
    EditMenuItemCopyDuplicatesListToClipboard.Enabled:=Result and (LastRow>=DuplicateLevelsStringGrid.FixedRows) and DuplicateLevelsStringGrid.Visible;
    if not Result then begin
       SetRowCount(0); PostMessage(Self.Handle,MSG_CLOSE,0,0);
       end;
  end;
end;

function  TDuplicatesForm.FindDuplicateLevels(DuplicateLevelsTask__:TFindDuplicateLevelsTask; ViewMenuItem__:TMenuItem; const FocusLevelName__,FocusFileName__,FocusDirectoryName__:String):Boolean;
//const
//  MARGIN='     ';
var
  LastRow,ScannedFilesCount,ScannedDirectoriesCount,SearchCountDown:Integer; //TimeMS:TTimeMS;
  SearchHashValue:THashTableHashKey;
  oShowErrorMessages:TShowErrorMessages;
  oCursor:TCursor;
  oCurrentDir,CurrentRootDirectory,
  s,s1,SearchLevelName,YASGenCandidateSetFileNamePrefix:String;
  SearchBoardTransformation2D:TBoardTransformation2D;
  SearchBoardAsText,SearchNormalizedBoardAsText:TBoardAsText;
  SearchNormalizedBoard:TBoard;
  SearchLevelSnapshotsAsText:SokUtil_.TList;
  SokoFile:TSokoFile; Game:TGame; SearchLevelIndexItem:TLevelIndexItem;
  FileListBox:TFileListBox;

  procedure FilterSquares(MatchOptionsMask__:Integer; var BoardWidth__,BoardHeight__:Integer; var Board__:TBoard; var PlayerPos__:TColRow);
  var Col,Row,SquareValue:Integer;
  begin
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do begin  // for each square on the board
            SquareValue:=Board__[Col,Row];
            if        (SquareValue and WALL)=0 then // 'True': it's a floor square
                      Board__[Col,Row]:=SquareValue and MatchOptionsMask__
            else if   (SquareValue and EXTERIOR_WALL)<>0 then // 'True': the square contains an exterior wall
                      if   (MatchOptionsMask__ and EXTERIOR_WALL)=0 then // 'True': exterior walls are filtered out
                           Board__[Col,Row]:=FLOOR // change the exterior wall to a floor
                      else begin end
                 else if   (MatchOptionsMask__ and WALL)=0 then // 'True': interior walls are filtered out
                           Board__[Col,Row]:=FLOOR; // change the interior wall to a floor
            end;

    if PlayerPos__.X<>0 then with PlayerPos__ do // 'True': there is a player on the board
       Board__[X,Y]:=Board__[X,Y] and (not PLAYER); // remove the player from the board when only some objects types are taken into account, e.g., boxes; in that case, the player is just in the way and can cause havoc to the comparison
    PlayerPos__.X:=0; PlayerPos__.Y:=0;

    TrimBoard(1,1,Board__,BoardWidth__,BoardHeight__,PlayerPos__);
    //Clipboard.AsText:=SokFile_.BoardToText(BoardWidth__,BoardHeight__,Board__,NL);
  end;

  procedure ShowStatus(IsScanning__:Boolean);
  var Index:Integer; s:String;
  begin
    //PanelDuplicateLevelsList.Visible:=not IsScanning__;
    //EditMenuItemCopyDuplicatesListToClipboard.Enabled:=RichEdit1.Lines.Count<>0;
    Self.ShowStatus;

    with ViewMenuCurrentCollection do
      for Index:=IndexOf(ViewMenuItemCurrentCollectionAllLevels) to IndexOf(ViewMenuItemCurrentCollectionDuplicates) do
          Items[Index].Enabled:=(DuplicateLevelsTask<>fdlCurrentCollection) or (ItemCount(Items[Index])>0);

    with ViewMenuAllLevels do
      for Index:=IndexOf(ViewMenuItemAllLevelsAllLevels) to IndexOf(ViewMenuItemAllLevelsDuplicates) do
          Items[Index].Enabled:=ItemCount(Items[Index])>0;

    ToolsMenuItemUpdateSolutions.Enabled:=(LevelsWithSolutionsAndSnapshotsCount<>0) and (MatchOptionsMask=ALL_MATCH_OPTIONS);
    ToolsMenuItemNewCollectionUniqueLevels.Enabled:=ToolsMenuItemNewCollectionUniqueLevels.Visible and (ScannedLevelsCount<>0) and (MatchOptionsMask=ALL_MATCH_OPTIONS);
    ToolsMenuItemNewCollectionSolvedLevels.Enabled:=MatchOptionsMask=ALL_MATCH_OPTIONS;
    if   DuplicateLevelsTask=fdlCurrentCollection then
         ToolsMenuItemNewCollectionUnsolvedLevels.Enabled:=ToolsMenuItemNewCollectionUnsolvedLevels.Visible and (ItemCount({ViewMenuItemCurrentCollectionNoSolutionsAndSnapshots}ViewMenuItemCurrentCollectionAllLevels)<>0) {the duplicate finder hasn't information about solved levels at this time, only information about levels with snapshots and/or solutions} and (MatchOptionsMask=ALL_MATCH_OPTIONS)
    else ToolsMenuItemNewCollectionUnsolvedLevels.Enabled:=ToolsMenuItemNewCollectionUnsolvedLevels.Visible and (ItemCount({ViewMenuItemAllLevelsNoSolutionsAndSnapshots}ViewMenuItemAllLevelsAllLevels)<>0) {the duplicate finder hasn't information about solved levels at this time, only information about levels with snapshots and/or solutions} and (MatchOptionsMask=ALL_MATCH_OPTIONS);

    with DuplicateLevelsStatusStringGrid1 do begin
      Cells[1,Ord(dlScanningFile)-Ord(dlScanning)]:='';
      if IsScanning__ then begin
         Cells[0,Ord(dlScanning)-Ord(dlScanning)]:=DuplicateLevelsStatusInfoText[dlScanning];
         Cells[0,Ord(dlScanningFolder)-Ord(dlScanning)]:=DuplicateLevelsStatusInfoText[dlScanningFolder];
         Cells[1,Ord(dlScanningFolder)-Ord(dlScanning)]:='';
         RichEdit1.Hint:='';
         DuplicateLevelsStringGrid.Hint:='';
         //RichEdit1.Color:=clLtGray;
         //RichEdit1.Font.Color:=clWhite;
         CancelBtn.SetFocus;
         //if DuplicateLevelsTask=fdlAllLevels then PanelDuplicateLevelsList.Hide;
         EditMenuItemCopyDuplicatesListToClipboard.Enabled:=False;

         s:='';
         if  MatchOptionsMask<>ALL_MATCH_OPTIONS then begin
             ToolsMenuItemUpdateSolutions.Enabled:=False;
             ToolsMenuItemNewCollectionUniqueLevels.Enabled:=False;
             with OpenForm.MenuItemFindDuplicateLevelsMatchOptionsBoxes         do if Checked then AddItemToCommaSeparatedText(StrRemoveChar(Caption,AMPERSAND),s);
             with OpenForm.MenuItemFindDuplicateLevelsMatchOptionsGoals         do if Checked then AddItemToCommaSeparatedText(StrRemoveChar(Caption,AMPERSAND),s);
             with OpenForm.MenuItemFindDuplicateLevelsMatchOptionsInteriorWalls do if Checked then AddItemToCommaSeparatedText(StrRemoveChar(Caption,AMPERSAND),s);
             with OpenForm.MenuItemFindDuplicateLevelsMatchOptionsExteriorWalls do if Checked then AddItemToCommaSeparatedText(StrRemoveChar(Caption,AMPERSAND),s);
             s:=StrRemoveChar(StrRemoveChar(OpenForm.MenuItemSubMenuFindDuplicateLevelsMatchOptions.Caption,AMPERSAND),PERIOD)+COLON+SPACE+s;
             end;
         Cells[1,Ord(dlScanning)-Ord(dlScanning)]:=s;
         end
      else begin
         Cells[0,Ord(dlScanning)-Ord(dlScanning)]:=ScanText;
         if AnsiPos(MULTIPLE_ROOTS_TEXT_SUFFIX,ScannedDirectoryNamesText)=Succ(Length(ScannedDirectoryNamesText)-Length(MULTIPLE_ROOTS_TEXT_SUFFIX)) then
            Cells[0,Ord(dlScanningFolder)-Ord(dlScanning)]:=DuplicateLevelsStatusInfoFoldersText; // more than one root folder has been scanned
         Cells[1,Ord(dlScanningFolder)-Ord(dlScanning)]:=ScannedDirectoryNamesText;

         if RichEdit1.Lines.Count<>0 then with RichEdit1 do with Lines do begin
            if not PanelDuplicateLevelsList.Visible then PanelDuplicateLevelsList.Show;
            Hint:=HintDuplicateLevelsSelectLevelToSeeAThumbnailView;
            //SetFocus;
            Perform(EM_LINESCROLL,Length(Lines[Pred(Count)]),-Lines.Count);
            end;
         RichEdit1.Color:=clBtnFace;
         RichEdit1.Font.Color:=clBtnText;

         if LastRow>1 then with DuplicateLevelsStringGrid do begin
            if not PanelDuplicateLevelsList.Visible then PanelDuplicateLevelsList.Show;
            Hint:=HintDuplicateLevelsSelectLevelToSeeAThumbnailView;
            end;
         end;
      RichEdit1.SelStart:=0; RichEdit1.SelLength:=0;

      //Repaint;
      Update;
      end;

    with DuplicateLevelsStatusStringGrid2 do begin
      Cells[Ord(dlScannedDuplicates)-Ord(dlScanned),0]:=DuplicateLevelsStatusInfoText[dlScannedDuplicates];
      if   not IsScanning__ then begin // 'True': the scan is over; use 'Objects[,]' to store the statistics for the duplicate finder
           Objects[Ord(dlScannedLevels     )-Ord(dlScanned),1]:=Pointer(ScannedLevelsCount);
           Objects[Ord(dlScannedDuplicates )-Ord(dlScanned),1]:=Pointer(DuplicateLevelsCount);
           Objects[Ord(dlScannedFiles      )-Ord(dlScanned),1]:=Pointer(ScannedFilesCount);
           Objects[Ord(dlScannedDirectories)-Ord(dlScanned),1]:=Pointer(ScannedDirectoriesCount);
           end;

      if   ScannedLevelsCount<>0 then
           Cells[Ord(dlScannedLevels)-Ord(dlScanned),1]:=IntToStr(ScannedLevelsCount)
      else Cells[Ord(dlScannedLevels)-Ord(dlScanned),1]:='';
      if   (DuplicateLevelsCount<>0) or ((not IsScanning__) and (ScannedLevelsCount<>0)) then
           Cells[Ord(dlScannedDuplicates)-Ord(dlScanned),1]:=IntToStr(DuplicateLevelsCount)
      else Cells[Ord(dlScannedDuplicates)-Ord(dlScanned),1]:='';
      if   ScannedFilesCount<>0 then
           Cells[Ord(dlScannedFiles)-Ord(dlScanned),1]:=IntToStr(ScannedFilesCount)
      else Cells[Ord(dlScannedFiles)-Ord(dlScanned),1]:='';
      if   ScannedDirectoriesCount<>0 then
           Cells[Ord(dlScannedDirectories)-Ord(dlScanned),1]:=IntToStr(ScannedDirectoriesCount)
      else Cells[Ord(dlScannedDirectories)-Ord(dlScanned),1]:='';
      //Repaint;
      Update;
      end;

    StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
    StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
    StatusBar1.Repaint;
  end;

  function  ScanFile(const PathName__,FileNameWithoutPath__:String; DirectoryItem__:TNode):Boolean;
  var FileLevelCount:Integer; Found:Boolean; HashValue:THashTableHashKey; ScannedFileName,s:String;
      BoardAsText:TBoardAsText; Level:TLevel;
      LevelIndexItem,ExistingLevelIndexItem,FirstItemInFile,LastItemInFile:TLevelIndexItem;

    procedure DeleteBuiltinSolutions(Snapshots__:TList);
    var Node,PreviousNode,NextNode:TNode;
    begin
      Node:=Snapshots__.First; PreviousNode:=nil;
      while Assigned(Node) do begin
        NextNode:=Node.Next;
        if   StrBeginsWith(Node.Name,SNAPSHOT_TYPE_NAME[stBuiltinBestSolutionMoves ]) or
             StrBeginsWith(Node.Name,SNAPSHOT_TYPE_NAME[stBuiltinBestSolutionPushes]) then begin
             if   PreviousNode<>nil then PreviousNode.Next:=NextNode
             else Snapshots__.Items:=NextNode;
             Node.Free;
             end
        else PreviousNode:=Node;
        Node:=NextNode;
        end;
    end;

    function  FilteredOut(const FileName__:String):Boolean;
    begin // returns 'True' is the file should not be scanned
      Result:=StrBeginsWith(FileName__,YASGenCandidateSetFileNamePrefix);
    end; // FilteredOut

    function  IsAMatchingBoard : Boolean;
    var b : TBoardTransformation2D;

      function  IsAMatchingBoard:Boolean;
      const SQUARE_MASK=WALL+BOX+GOAL+PLAYER;
      var   Col,Row,CountDown:Integer;
      begin
        Result:=(Game.BoardWidth=SearchNormalizedBoardAsText.Width) and (Game.BoardHeight=SearchNormalizedBoardAsText.Height);
        if Result then with Game do begin
           CountDown:=SearchCountDown; // number of squares that are allowed to differ
           for Col:=1 to BoardWidth do
               for Row:=1 to BoardHeight do
                   if (StartBoard[Col,Row] and SQUARE_MASK)<>(SearchNormalizedBoard[Col,Row] and SQUARE_MASK) then
                      if CountDown>0 then Dec(CountDown)
                      else begin
                          Result:=False; exit; // 'exit': quick-and-dirty exit when the levels don't match according to the match threshold percent
                          end;
           end;
      end; // IsAMatchingBoard

    begin // IsAMatchingBoard
      // first check is the board width and height matches the searched level, either directly or after a rotation; otherwise, there is no chance of a match;
      Result := ( (Game.BoardWidth=SearchNormalizedBoardAsText.Width ) and (Game.BoardHeight=SearchNormalizedBoardAsText.Height) )
                or
                ( (Game.BoardWidth=SearchNormalizedBoardAsText.Height) and (Game.BoardHeight=SearchNormalizedBoardAsText.Width ) );
      if Result then begin // 'True': width and height matches the searched level, either directly or after a rotation; check for a match;
         Result := IsAMatchingBoard;
         if not Result then with Game do begin
            for b:=Low(TBoardTransformation2D) to High(TBoardTransformation2D) do begin // enumerate the transformations; the last transformation brings the board back to the initial state;
                DoBoardTransformation2D(t2DRotate90DegreesClockwise,True); // board transformations are relative to the current transformation
                if ( BoardTransformation2D=t2DRotate0DegreesClockwise ) or                   // 'True': the board has been rotated back to the initial state; flip the board horizontally and continue rotating the board
                   ( BoardTransformation2D=t2DRotate0DegreesClockwiseFlipHorizontally ) then // 'True': the board has been rotated back to the flipped initial state; flip it back to the initial state;
                   DoBoardTransformation2D(t2DFlipHorizontally,True);
                if ( not Result ) and ( b < High( TBoardTransformation2D ) ) and IsAMatchingBoard then begin // b < High( TBoardTransformation2D ): the last transformation brings the board back to the initial state, which was checked for a match at the beginning
                   Result := True;
                   end;
                end;
            end;
         end;
    end; // IsAMatchingBoard

    procedure UpdateDuplicateLevel(ExistingLevelIndexItem__,NewLevelIndexItem__:TLevelIndexItem);
    begin
      Inc(DuplicateLevelsCount);
      DuplicateLevelsStatusStringGrid2.Cells[Ord(dlScannedDuplicates)-Ord(dlScanned),1]:=IntToStr(DuplicateLevelsCount);
      //DuplicateLevelsStatusStringGrid.Repaint;
      DuplicateLevelsStatusStringGrid2.Update;
      with ExistingLevelIndexItem__ do begin
        if DuplicatesCount=0 then Inc(DuplicatedLevelsCount);
        DuplicatesCount:=Succ(DuplicatesCount);
        if   Assigned(NextDuplicate) then
             NewLevelIndexItem__.NextDuplicate:=NextDuplicate // push the new item on the circular list
        else NewLevelIndexItem__.NextDuplicate:=ExistingLevelIndexItem__; // make circular list with 2 items, i.e., the existing level and the first found duplicate
        NextDuplicate:=NewLevelIndexItem__;
        end;

      NewLevelIndexItem__.NormalizedBoard:=ExistingLevelIndexItem__.NormalizedBoard; // reuse the existing board and free the new one; the boards are stored as strings, and Delphi uses reference counts for strings; a string assignment does not copy bytes but merely updates reference counts, freeing the old string in case it's not used anymore
      with NewLevelIndexItem__.BoardAsText do
        if (Width =ExistingLevelIndexItem__.BoardAsText.Width ) and
           (Height=ExistingLevelIndexItem__.BoardAsText.Height) and
           (Board =ExistingLevelIndexItem__.BoardAsText.Board ) then
           Board :=ExistingLevelIndexItem__.BoardAsText.Board; // reuse the existing board and free the new one; the boards are stored as strings, and Delphi uses reference counts for strings; a string assignment does not copy bytes but merely updates reference counts, freeing the old string in case it's not used anymore
    end;

  begin // ScanFile
    Result:=True;
    BoardAsText.Board:='';
    try
      ScannedFileName:=StrWithTrailingPathDelimiter(PathName__)+FileNameWithoutPath__;
      if Result and
         FileExists(ScannedFileName) and
         (not FilteredOut(FileNameWithoutPath__)) and
         (not ((DuplicateLevelsTask=fdlCurrentCollection)
               and
               (CurrentCollectionLevelsCount<>0) // '<>0': this is not the initial scan of the current collection
               and
               StrEqual(ScannedFileName,CurrentCollectionFileName)
              )
         ) and
         (not ((DuplicateLevelsTask=fdlCurrentLevel)
               and
               (CurrentCollectionLevelsCount<>0) // '<>0': this is not the initial scan of the current collection
               and
               StrEqual(ScannedFileName,CurrentCollectionFileName)
              )
         )
         then begin
         with DuplicateLevelsStatusStringGrid1 do begin
           Cells[1,Ord(dlScanningFile)-Ord(dlScanning)]:=FileNameWithoutPath__;
           //DuplicateLevelsStatusStringGrid.Repaint;
           DuplicateLevelsStatusStringGrid1.Update;
           end;

         if SokoFile.IsASokobanFile(ScannedFileName) and SokoFile.Open(ScannedFileName) then begin
            with StatusBar1 do
              if (Panels[OPEN_FORM_PANEL_INDEX_STATE].Text<>'')
                 or
                 (Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text<>'') then begin
                 Panels [OPEN_FORM_PANEL_INDEX_STATE].Text:='';
                 Panels [OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
                 Repaint;
                 end;

            if (DuplicateLevelsTask=fdlCurrentLevel) or
               Assigned(FileList.AddTextLine(FileNameWithoutPath__,True)) then begin
               FileLevelCount:=0; FirstItemInFile:=nil; LastItemInFile:=nil;
               Level:=TLevel(SokoFile.Levels.First);

               while Assigned(Level) and (ScanState=ssScan) do with Game do begin
                 if Level.TextLinesToBoard(StartBoard,BoardWidth,BoardHeight,BoxCount,GoalCount,PlayerPos,s) then begin
                    Inc(FileLevelCount);

                    BoardAsText.Width:=BoardWidth; BoardAsText.Height:=BoardHeight; BoardAsText.Board:=SokFile_.BoardToText(BoardWidth,BoardHeight,StartBoard,''); // keep a copy of the original board before it's normalized
                    NormalizeBoard(True,False,True,AdvancedIdentityCheck,True,BoardWidth,BoardHeight,StartBoard,PlayerStartPos,History);
                    if MatchOptionsMask<>ALL_MATCH_OPTIONS then FilterSquares(MatchOptionsMask,BoardWidth,BoardHeight,StartBoard,PlayerStartPos);
                    //MainForm.TestLogFile.Writeln( IntToStr( RunNo ) + SPACE + ScannedFileName + LEFT_BRACKET + Level.Name + RIGHT_BRACKET);
                    //MainForm.TestLogFile.Writeln( BoardToText( Game.BoardWidth, Game.BoardHeight, Game.StartBoard, NL ) );
                    //MainForm.TestLogFile.Writeln( '' );
                    NormalizeRotationsAndReflectionsAndPlayerPosition(Game,False,HashValue);

                    if DuplicateLevelsTask=fdlAllLevels then begin // 'True': find all duplicate levels
                       LevelIndexItem:=TLevelIndexItem.Create(Level.Name,SokFile_.BoardToText(Game.BoardWidth,Game.BoardHeight,Game.StartBoard,CR),BoardAsText.Board,BoardAsText.Width,BoardAsText.Height,Game.OriginalBoardTransformation2D,HashValue,DirectoryItem__,FileList.First,FileLevelCount);
                       if   LevelIndexItem.Key<>'' then begin
                            if   LevelIndex.Lookup(HashValue,LevelIndexItem.Key,ExistingLevelIndexItem) then
                                 UpdateDuplicateLevel(ExistingLevelIndexItem,LevelIndexItem);
                            if   Assigned(LastItemInFile) then // 'True': this isn't the first level in the file
                                 LastItemInFile.NextInFile:=LevelIndexItem // make the previous level item point to the new last item on the list
                            else FirstItemInFile:=LevelIndexItem; // this is the first level in the file; make the new item the head of the list
                            LevelIndexItem.NextInFile:=FirstItemInFile; // make the new last item point back to the first item, i.e., a circular list
                            LastItemInFile:=LevelIndexItem; // new last item on the circular list

                            SwapNodes(TNode(LevelIndexItem.fSnapshots),TNode(Level.SnapshotsAsText)); // load the snapshots and solutions for this level
                            if   Assigned(LevelIndexItem.Snapshots) then begin
                                 DeleteBuiltinSolutions(LevelIndexItem.Snapshots);
                                 if not LevelIndexItem.Snapshots.IsEmpty then Inc(LevelsWithSolutionsAndSnapshotsCount);

                                 LevelIndex.Add(LevelIndexItem);
                                 end
                            else raise Exception.Create(TEXT_TASK_FAILED);
                            end
                       else raise Exception.Create(TEXT_TASK_FAILED);
                       end
                    else
                       if DuplicateLevelsTask=fdlCurrentLevel then begin // 'True': find levels similar to the selected level
                          if IsAMatchingBoard then begin
                             if   (((not FileList.IsEmpty) and StrEqual(FileNameWithoutPath__,FileList.First.Text))
                                   or
                                   Assigned(FileList.AddTextLine(FileNameWithoutPath__,True))
                                  ) then begin
                                  LevelIndexItem:=TLevelIndexItem.Create(Level.Name,SokFile_.BoardToText(Game.BoardWidth,Game.BoardHeight,Game.StartBoard,CR),BoardAsText.Board,BoardAsText.Width,BoardAsText.Height,Game.OriginalBoardTransformation2D,HashValue,DirectoryItem__,FileList.First,FileLevelCount);
                                  if   Assigned(LevelIndex.First) then // 'True': this is not the first matching level
                                       UpdateDuplicateLevel(LevelIndex.First,LevelIndexItem);
                                  if   Assigned(LastItemInFile) then // 'True': this isn't the first level in the file
                                       LastItemInFile.NextInFile:=LevelIndexItem // make the previous level item point to the new last item on the list
                                  else FirstItemInFile:=LevelIndexItem; // this is the first level in the file; make the new item the head of the list
                                  LevelIndexItem.NextInFile:=FirstItemInFile; // make the new last item point back to the first item, i.e., a circular list
                                  LastItemInFile:=LevelIndexItem; // new last item on the circular list

                                  SwapNodes(TNode(LevelIndexItem.fSnapshots),TNode(Level.SnapshotsAsText)); // load the snapshots and solutions for this level
                                  if   Assigned(LevelIndexItem.Snapshots) then begin
                                       DeleteBuiltinSolutions(LevelIndexItem.Snapshots);
                                       if   not LevelIndexItem.Snapshots.IsEmpty then Inc(LevelsWithSolutionsAndSnapshotsCount);

                                       LevelIndex.Add(LevelIndexItem);

                                       if   DuplicateLevelsCount<>0 then begin // 'True': there is one or more duplicates; add the items to the string grid
                                            if   DuplicateLevelsCount=1 then // 'True': this is the first encountered duplicate
                                                 if   AddLevelToStringGrid(LevelIndex.First,LastRow) then {add the first level to the string grid now that it is known there are one or more duplicates}
                                                 else ScanState:=ssCancel;
                                            if   AddLevelToStringGrid(LevelIndexItem,LastRow) then {add the matching level to the string grid}
                                            else ScanState:=ssCancel;
                                            //if   not DuplicateLevelsStringGrid.Visible then begin // disabled because the number of duplicates is out of sync until the searched level has been added to the list; this may happen very late in the search, hence, it's better to postpone showing the string grid until the search has completed
                                            //     DuplicateLevelsStringGrid.Show;
                                            //     DuplicateLevelsStringGrid.Repaint;
                                            //     end;
                                            end;

                                       if   StrEqual(MakeIniFileSectionFileName(ScannedFileName,Level.Name),SearchLevelName) then
                                            SearchLevelIndexItem:=LevelIndexItem; // remember the level the user searched for
                                       end
                                  else raise Exception.Create(TEXT_TASK_FAILED);
                                  end
                             else raise Exception.Create(TEXT_TASK_FAILED);
                             end;
                          end
                       else begin // find duplicates of levels in the current collection
                          s:=SokFile_.BoardToText(Game.BoardWidth,Game.BoardHeight,Game.StartBoard,CR); // the board in text format; this is also the key for the items stored in the level index
                          Found:=LevelIndex.Lookup(HashValue,s,ExistingLevelIndexItem);
                          if Found or
                             (CurrentCollectionLevelsCount=0) then begin // '0': this is the initial scan of the current collection; all its levels are added to the level index
                             LevelIndexItem:=TLevelIndexItem.Create(Level.Name,s,BoardAsText.Board,BoardAsText.Width,BoardAsText.Height,Game.OriginalBoardTransformation2D,HashValue,DirectoryItem__,FileList.First,FileLevelCount);
                             if   LevelIndexItem.Key<>'' then begin // cannot remember if this check is necessary, but better safe than sorry
                                  if   Found then UpdateDuplicateLevel(ExistingLevelIndexItem,LevelIndexItem);
                                  if   Assigned(LastItemInFile) then // 'True': this isn't the first level in the file
                                       LastItemInFile.NextInFile:=LevelIndexItem // make the previous level item point to the new last item on the list
                                  else FirstItemInFile:=LevelIndexItem; // this is the first level in the file; make the new item the head of the list
                                  LevelIndexItem.NextInFile:=FirstItemInFile; // make the new last item point back to the first item, i.e., a circular list
                                  LastItemInFile:=LevelIndexItem; // new last item on the circular list

                                  SwapNodes(TNode(LevelIndexItem.fSnapshots),TNode(Level.SnapshotsAsText)); // load the snapshots and solutions for this level
                                  if   Assigned(LevelIndexItem.Snapshots) then begin
                                       DeleteBuiltinSolutions(LevelIndexItem.Snapshots);
                                       if not LevelIndexItem.Snapshots.IsEmpty then Inc(LevelsWithSolutionsAndSnapshotsCount);

                                       LevelIndex.Add(LevelIndexItem);
                                       end
                                  else raise Exception.Create(TEXT_TASK_FAILED);
                                  end
                             else raise Exception.Create(TEXT_TASK_FAILED);
                             end;
                          end;

                    Inc(ScannedLevelsCount);

                    DuplicateLevelsStatusStringGrid2.Cells[Ord(dlScannedLevels)-Ord(dlScanned),1]:=IntToStr(ScannedLevelsCount);
                    if (ScannedLevelsCount mod 100=0) or (FileLevelCount mod 100=0) then
                       DuplicateLevelsStatusStringGrid2.Update;
                    end;
                 Level:=TLevel(Level.Next);
                 end;
               end
            else raise Exception.Create(TEXT_TASK_FAILED);
            end;

         Inc(ScannedFilesCount);
         with DuplicateLevelsStatusStringGrid2 do begin
           Cells[Ord(dlScannedFiles)-Ord(dlScanned),1]:=IntToStr(ScannedFilesCount);
           //DuplicateLevelsStatusStringGrid.Repaint;
           DuplicateLevelsStatusStringGrid2.Update;
           end;
         end;
    except on E:Exception do Result:=Error(E.Message,Caption);
    end;
  end; // ScanFile

  procedure ScanRootDirectories;
  var Index:Integer; ScanCurrentDirectory:Boolean;

    function  IsASubDirectoryOfASelectedDirectory(const Directory__:String; IsASubDirectoryIfIdenticalToCurrentFolderOrDefaultFolder__:Boolean):Boolean;
    var Index:Integer; Directory:String;
    begin

      Result:=(OpenForm.MenuItemDuplicateLevelsDefaultLevelFolder.Checked // check the default level folder
               and
               (IsASubdirectoryName(Directory__,MainForm.DefaultLevelFolder)
                or
                (IsASubDirectoryIfIdenticalToCurrentFolderOrDefaultFolder__
                 and
                 StrEqual(StrWithoutTrailingPathDelimiter(Directory__),StrWithoutTrailingPathDelimiter(MainForm.DefaultLevelFolder))
                )
               )
              )
              or
              (ScanCurrentDirectory // check the current folder
               and
               (IsASubdirectoryName(Directory__,CurrentDirectoryName)
                or
                (IsASubDirectoryIfIdenticalToCurrentFolderOrDefaultFolder__
                 and
                 StrEqual(StrWithoutTrailingPathDelimiter(Directory__),StrWithoutTrailingPathDelimiter(CurrentDirectoryName))
                )
               )
              );

      if not Result then begin // check the user's folder list
         Directory:=StrWithTrailingPathDelimiter(AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(Directory__),MainForm.MyDocumentsFolder));
         for Index:=0 to Pred(OpenForm.MenuItemDuplicateLevelsFolderSeparator.MenuIndex) do
             with OpenForm.MenuItemSubMenuFindDuplicateLevelsFolders.Items[Index] do
               if Checked and
                  (not Result) and
                  IsASubDirectoryName(Directory,StrRemoveChar(Caption,AMPERSAND)) then
                  Result:=True;
         end;
    end; // IsASubDirectoryOfASelectedDirectory

    procedure ScanDirectories(DirectoryItem__:TNode);

      function ScanDirectory(DirectoryItem__:TNode):Boolean;
      var i:Integer; DirectoryName,s:String;
      begin
        Result:=True;
        s:=Trim(DirectoryItem__.Text);
        DirectoryName:=ExpandedFilePath(s,MainForm.MyDocumentsFolder);
        if DirectoryExists(DirectoryName) then with FileListBox do begin
           with DuplicateLevelsStatusStringGrid1 do begin
             Cells[1,Ord(dlScanningFolder)-Ord(dlScanning)]:=s;
             //DuplicateLevelsStatusStringGrid.Repaint;
             DuplicateLevelsStatusStringGrid1.Update;
             end;

           Mask:=ALL_FILES_FILTER;
           Directory:=DirectoryName;
           DirectoryName:=StrWithTrailingPathDelimiter(DirectoryName); // 'StrWith...': so 'ScanFile()' doesn't need to create a new string with a trailing path delimiter for each file
           i:=0;
           while (i<Items.Count) and (ScanState=ssScan) and Result do begin
             Result:=ScanFile(DirectoryName,Items[i],DirectoryItem__);
             Inc(i);
             Application.ProcessMessages;
             end;

           Inc(ScannedDirectoriesCount);
           with DuplicateLevelsStatusStringGrid2 do begin
             Cells[Ord(dlScannedDirectories)-Ord(dlScanned),1]:=IntToStr(ScannedDirectoriesCount);
             //DuplicateLevelsStatusStringGrid.Repaint;
             DuplicateLevelsStatusStringGrid2.Update;
             end;

           OpenForm.GetSubDirectories(DirectoryName,MainForm.MyDocumentsFolder,DirectoryList);
           end;
      end; // ScanDirectory

    begin // ScanDirectories; scans the directories on the list starting from the item 'Node__'; note that subfolders are added at the end of the list during the scan
      while Assigned(DirectoryItem__) and (ScanState=ssScan) do begin
        ScanDirectory(DirectoryItem__);
        DirectoryItem__:=DirectoryItem__.Next;
        end;

      if      ScannedDirectoryNamesText='' then // 'ScannedDirectoryNamesText' is used when the search is over to show which roots have been scanned
              ScannedDirectoryNamesText:=AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(CurrentRootDirectory),MainForm.MyDocumentsFolder)
      else if AnsiPos(MULTIPLE_ROOTS_TEXT_SUFFIX,ScannedDirectoryNamesText)<>Succ(Length(ScannedDirectoryNamesText)-Length(MULTIPLE_ROOTS_TEXT_SUFFIX)) then
              ScannedDirectoryNamesText:=ScannedDirectoryNamesText+MULTIPLE_ROOTS_TEXT_SUFFIX;
    end; // ScanDirectories

  begin // ScanRootDirectories

    // simplify the tests for 'is-this-directory-a-sub-directory-of-another-checked-directory?'
    // by taking one of the 'current folder' and 'default folder' directories out of the equation
    // if they are identical
    ScanCurrentDirectory:=OpenForm.MenuItemDuplicateLevelsCurrentLevelFolder.Checked
                          and
                          (not (OpenForm.MenuItemDuplicateLevelsDefaultLevelFolder.Checked
                                and
                                StrEqual(StrWithoutTrailingPathDelimiter(CurrentDirectoryName),
                                         StrWithoutTrailingPathDelimiter(MainForm.DefaultLevelFolder))));

    // scan selected root directories
    CurrentRootDirectory:=CurrentDirectoryName; // check if the current folder should be scanned
    if ScanCurrentDirectory and
       (not IsASubDirectoryOfASelectedDirectory(CurrentRootDirectory,False)) then // the current folder is not covered by any of the listed root folders, hence, scan the current folder and its subfolders
       ScanDirectories(DirectoryList.AddTextLine(AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(CurrentRootDirectory),MainForm.MyDocumentsFolder),False));

    if (ScanState=ssScan) and
       OpenForm.MenuItemDuplicateLevelsDefaultLevelFolder.Checked and
       (not IsASubDirectoryOfASelectedDirectory(MainForm.DefaultLevelFolder,False)) then begin // the default level folder is not covered by any of the listed root folders, hence, scan the default level folder and its subfolders
       CurrentRootDirectory:=MainForm.DefaultLevelFolder;
       ScanDirectories(DirectoryList.AddTextLine(AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(CurrentRootDirectory),MainForm.MyDocumentsFolder),False));
       end;

    for Index:=0 to Pred(OpenForm.MenuItemDuplicateLevelsFolderSeparator.MenuIndex) do // scan selected folders on the list
        with OpenForm.MenuItemSubMenuFindDuplicateLevelsFolders.Items[Index] do
               if (ScanState=ssScan) and Checked then begin
                  CurrentRootDirectory:=ExpandedFilePath(StrRemoveChar(Caption,AMPERSAND),MainForm.MyDocumentsFolder);
                  if not IsASubDirectoryOfASelectedDirectory(CurrentRootDirectory,True) then
                     ScanDirectories(DirectoryList.AddTextLine(AbbreviatedFilePath(StrWithoutTrailingPathDelimiter(CurrentRootDirectory),MainForm.MyDocumentsFolder),False));
                  end;
  end; // ScanRootDirectories

  function  InitializeCurrentCollectionSearch:Boolean;
  var DirectoryName:String; DirectoryItem:TNode;
  begin
    if   IsAnIniFileSectionFileName(CurrentFileName) then
         CurrentCollectionFileName:=ExtractIniFileName(CurrentFileName)
    else CurrentCollectionFileName:=CurrentFileName;
    DirectoryName:=StrWithoutTrailingPathDelimiter(ExtractFilePath(CurrentCollectionFileName));
    DirectoryItem:=DirectoryList.AddTextLine(SPACE+SPACE+AbbreviatedFilePath(DirectoryName,MainForm.MyDocumentsFolder),False); // kludge: the leading SPACE+SPACE ensures that the directory later can be added to the list again by calling 'OpenForm.GetSubDirectories()', if the directory is to be scanned
    Result:=Assigned(DirectoryItem) and
            ScanFile(DirectoryName,ExtractFileName(CurrentCollectionFileName),DirectoryItem) and
            (LevelIndex.Count<>0);
    if Result then begin // 'True': the current collection is not an empty one, and its levels have been added to the level index
       CurrentCollectionLevelsCount:=LevelIndex.Count;
       CurrentCollectionDuplicateLevelsCount:=DuplicateLevelsCount;
       CurrentCollectionLevelsWithSolutionsAndSnapshotsCount:=LevelsWithSolutionsAndSnapshotsCount;
       end;
  end; // InitializeCurrentCollectionSearch

  function  InitializeCurrentLevelSearch:Boolean;
  var DirectoryName:String; DirectoryItem:TNode;
  begin
    with Game do begin
      BoardWidth                        :=OpenForm.Game.BoardWidth;
      BoardHeight                       :=OpenForm.Game.BoardHeight;
      StartBoard                        :=OpenForm.Game.StartBoard;

      SearchBoardAsText.Width           :=BoardWidth;
      SearchBoardAsText.Height          :=BoardHeight;
      SearchBoardAsText.Board           :=SokFile_.BoardToText(BoardWidth,BoardHeight,StartBoard,'');

      NormalizeBoard(True,False,True,AdvancedIdentityCheck,True,BoardWidth,BoardHeight,StartBoard,PlayerStartPos,History);
      //Clipboard.AsText:=SokFile_.BoardToText(BoardWidth,BoardHeight,StartBoard,NL);
      if MatchOptionsMask<>ALL_MATCH_OPTIONS then FilterSquares(MatchOptionsMask,BoardWidth,BoardHeight,StartBoard,PlayerStartPos);
      NormalizeRotationsAndReflectionsAndPlayerPosition(Game,False,SearchHashValue);
      //Clipboard.AsText:=SokFile_.BoardToText(BoardWidth,BoardHeight,StartBoard,NL);
      SearchNormalizedBoardAsText.Width :=BoardWidth;
      SearchNormalizedBoardAsText.Height:=BoardHeight;
      SearchNormalizedBoardAsText.Board :=SokFile_.BoardToText(BoardWidth,BoardHeight,StartBoard,CR); // the normalized board is used for comparisons, hence, a row separator is required
      SearchNormalizedBoard             :=StartBoard;

      SearchLevelName                   :=MakeIniFileSectionFileName(OpenForm.Game.SokoFileName,OpenForm.Game.Name);
      SearchCountDown                   :=(SearchNormalizedBoardAsText.Width*SearchNormalizedBoardAsText.Height*(100-MatchThresholdPercent)) div 100; // number of squares that are allowed to differ
      SearchBoardTransformation2D       :=Game.OriginalBoardTransformation2D;
      SearchLevelIndexItem              :=nil;

      // scan the current file before other files so any duplicates inside
      // the current file are grouped together with the searched level in the
      // level index; otherwise, the tools (e.g., "Update solutions for
      // duplicate levels") may load the file twice and count is as 2 files in
      // the statistics
      if   IsAnIniFileSectionFileName(CurrentFileName) then
           CurrentCollectionFileName:=ExtractIniFileName(CurrentFileName)
      else CurrentCollectionFileName:=CurrentFileName;
      DirectoryName:=StrWithoutTrailingPathDelimiter(ExtractFilePath(CurrentCollectionFileName));
      DirectoryItem:=DirectoryList.AddTextLine(SPACE+SPACE+AbbreviatedFilePath(DirectoryName,MainForm.MyDocumentsFolder),False); // kludge: the leading SPACE+SPACE ensures that the directory later can be added to the list again by calling 'OpenForm.GetSubDirectories()', if the directory is to be scanned
      Result:=Assigned(DirectoryItem) and
              ScanFile(DirectoryName,ExtractFileName(CurrentCollectionFileName),DirectoryItem);
      CurrentCollectionLevelsCount:=Max(1,LevelIndex.Count); // a non-zero value informs 'ScanFile' not to process the file again
      end;

    if not Result then Error(TEXT_TASK_FAILED,Caption);
  end; // InitializeCurrentLevelSearch

begin // FindDuplicateLevels
  Result:=(OpenForm.Task=otGame)
          and
          Assigned(OpenForm.Game)
          and
          ((DuplicateLevelsTask__ =fdlAllLevels)
           or
           ((DuplicateLevelsTask__=fdlCurrentCollection)
            and
            (OpenForm.CurrentFileName<>'')
            and
            Assigned(OpenForm.Game)
            and
            ((OpenForm.CollectionGroupBox.Visible
              and
              (OpenForm.CollectionStringGrid.RowCount>1)
             )
             or
             ((OpenForm.Game.BoardWidth >=MIN_BOARD_WIDTH )
              and
              (OpenForm.Game.BoardHeight>=MIN_BOARD_HEIGHT)
              and
              (OpenForm.Game.GameState  <>gsNull)
             )
            )
           )
           or
           ((DuplicateLevelsTask__=fdlCurrentLevel)
            and
            (OpenForm.CurrentFileName<>'')
            and
            Assigned(OpenForm.Game)
            and
            (OpenForm.Game.BoardWidth >=MIN_BOARD_WIDTH )
            and
            (OpenForm.Game.BoardHeight>=MIN_BOARD_HEIGHT)
            and
            (OpenForm.Game.GameState  <>gsNull)
           )
          )
          and
          OpenForm.CloseFile;

  CurrentFileName:=OpenForm.CurrentFileName;
  CurrentDirectoryName:=StrWithoutTrailingPathDelimiter(OpenForm.DirectoryListBox1.Directory);
  DuplicateLevelsTask:=DuplicateLevelsTask__;

  s:=Application.Title+SUB_TITLE_SEPARATOR+DuplicateLevelsCaptionText;
  s1:='';
  if CurrentFileName<>'' then
     if      DuplicateLevelsTask=fdlCurrentLevel then begin
             if   IsAnIniFileSectionFileName(CurrentFileName) then
                  s1:=ExtractSectionName    (CurrentFileName)
             else s1:=ExtractFileName       (CurrentFileName);
             end
     else if DuplicateLevelsTask=fdlCurrentCollection then begin
             if   IsAnIniFileSectionFileName(CurrentFileName) then
                  s1:=ExtractFileName(ExtractIniFileName    (CurrentFileName))
             else s1:=ExtractFileName                       (CurrentFileName);
             end;
  Caption                                           :=TitleWithOptionalSubTitle(s,s1);
  if   DuplicateLevelsTask=fdlCurrentLevel then
       DuplicateLevelsGroupBox             .Caption :=TitleWithOptionalSubTitle(SimilarLevelsText  ,StrWithQuotedAmpersands(s1))
  else DuplicateLevelsGroupBox             .Caption :=TitleWithOptionalSubTitle(DuplicateLevelsText,StrWithQuotedAmpersands(s1));
  ViewMenuCurrentLevel                     .Visible :=DuplicateLevelsTask=fdlCurrentLevel;
  ViewMenuCurrentCollection                .Visible :=DuplicateLevelsTask=fdlCurrentCollection;
  ViewMenuAllLevels                        .Visible :=DuplicateLevelsTask=fdlAllLevels;
  ToolsMenuItemNewCollectionUniqueLevels   .Visible :=DuplicateLevelsTask<>fdlCurrentLevel;
  ToolsMenuItemNewCollectionUnsolvedLevels .Visible :=ToolsMenuItemNewCollectionUniqueLevels.Visible;
  SettingsMenuItemToolsNewCollection       .Visible :=ToolsMenuItemNewCollectionUniqueLevels.Visible;

  ToolsMenuItemUpdateSolutions          .Hint:=HintDuplicateFinderToolsMenuItemUpdateSolutionsText          [DuplicateLevelsTask<>fdlCurrentLevel];
  ToolsMenuItemNewCollectionSolvedLevels.Hint:=HintDuplicateFinderToolsMenuItemNewCollectionSolvedLevelsText[DuplicateLevelsTask<>fdlCurrentLevel];

  MatchOptionsMask:=ALL_MATCH_OPTIONS;
  with OpenForm.MenuItemFindDuplicateLevelsMatchOptionsBoxes         do if not Checked then Dec(MatchOptionsMask,BOX);
  with OpenForm.MenuItemFindDuplicateLevelsMatchOptionsGoals         do if not Checked then Dec(MatchOptionsMask,GOAL);
  with OpenForm.MenuItemFindDuplicateLevelsMatchOptionsInteriorWalls do if not Checked then Dec(MatchOptionsMask,WALL);
  with OpenForm.MenuItemFindDuplicateLevelsMatchOptionsExteriorWalls do if not Checked then Dec(MatchOptionsMask,EXTERIOR_WALL);

  SearchBoardAsText.Board:=''; SearchBoardTransformation2D:=t2DRotate0DegreesClockwise;
  ScannedLevelsCount:=0; ScannedFilesCount:=0; ScannedDirectoriesCount:=0; LevelsWithSolutionsAndSnapshotsCount:=0;
  CurrentCollectionDuplicateLevelsCount:=0; CurrentCollectionLevelsCount:=0; CurrentCollectionLevelsWithSolutionsAndSnapshotsCount:=0;
  DuplicatedLevelsCount:=0; DuplicateLevelsCount:=0; // note the difference between 'Duplicated...' and 'Duplicate...'; the former is the number of levels having duplicates; the former is the "set representatives"; the latter is the number of duplicates
  UpdatedLevelsCount:=0; HighlightedRowNumber:=-1;

  ScanState:=ssIdle;
  oCurrentDir:=GetCurrentDir;
  oCursor:=Screen.Cursor;
  oShowErrorMessages:=SokUtil_.ShowErrorMessages;
  SearchLevelSnapshotsAsText:=nil;
  SokoFile:=nil; Game:=nil; FileListBox:=nil;
  CurrentCollectionFileName:=''; CurrentRootDirectory:=''; ScannedDirectoryNamesText:='';
  if Result then
     try
       RichEdit1.Clear;
       DuplicateLevelsStringGrid.Hide;
       if PanelToolTips.Visible then PanelToolTips.Hide;
       SetRowCount(0); DuplicateLevelsStringGrid.Cells[0,0]:=''; // clear the string grid
       ViewMenuItemAllLevelsDuplicates.Checked:=True;
       DuplicateLevelsStringGrid.Color:=BackgroundColor1;
       RichEdit1.Lines.BeginUpdate;

       //TimeMS:=GetTimeMS;

       try      fIsBusy:=True;
                ScanState:=ssScan;
                CancelBtn.Show; CloseBtn.Hide; CancelBtn.SetFocus;

                OpenForm.DiskGroupBox.Hide;
                OpenForm.CollectionGroupBox.Hide;
                OpenForm.Repaint;

                YASGenCandidateSetFileNamePrefix:=MainForm.Generator.GeneratorName+SPACE+CandidateSetCaptionText;
                LastRow:=Pred(DuplicateLevelsStringGrid.FixedRows); // the column headers occupy the first row

                ShowStatus(True);

                StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
                StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
                Self.Update;

                if   Assigned(DirectoryList) then DirectoryList.Clear
                else DirectoryList:=SokUtil_.TList.Create;
                if   Assigned(FileList     ) then FileList.Clear
                else FileList     :=SokUtil_.TList.Create;
                if   Assigned(LevelIndex   ) then LevelIndex.Clear
                else LevelIndex   :=TLevelIndex.Create(0,5);

                SokoFile:=TSokoFile.Create;
                Game:=Game_.TGame.Create; Game.SokoFile:=SokoFile;
                FileListBox:=TFileListBox.Create(nil); FileListBox.Visible:=False; FileListBox.Parent:=Self;

                SokUtil_.ShowErrorMessages:=semNone;

                Screen.Cursor:=crHourGlass;

                if      DuplicateLevelsTask=fdlCurrentLevel then
                        Result:=InitializeCurrentLevelSearch
                else if DuplicateLevelsTask=fdlCurrentCollection then
                        Result:=InitializeCurrentCollectionSearch;

                if   Result then ScanRootDirectories;

                if   (ScanState=ssScan) and (ScannedDirectoriesCount=0) then
                     Msg(DuplicateFinderDidNotScanAnyFoldersText,Self.Caption,MB_OK+MB_ICONINFORMATION);

                if   ScanState<>ssCloseWindow then begin
                     ScanState:=ssIdle;
                     OpenForm.BringToFront;
                     Self.BringToFront;
                     if Assigned(ViewMenuItem__) then ViewMenuItem__.Checked:=True;
                     if   DuplicateLevelsTask=fdlCurrentLevel then
                          if   (FocusLevelName__='') and Assigned(SearchLevelIndexItem) then with SearchLevelIndexItem do
                               ShowSearchResults(nil           ,Name            ,FileItem.Name  ,Trim(DirectoryItem.Name) )
                          else ShowSearchResults(nil           ,FocusLevelName__,FocusFileName__,     FocusDirectoryName__)
                     else ShowSearchResults     (ViewMenuItem__,FocusLevelName__,FocusFileName__,     FocusDirectoryName__);
                     end;
                //raise Exception.Create('Test.');
       finally  //Msg('Time: '+IntToStr(CalculateElapsedTimeMS(TimeMS,GetTimeMS)),Caption,MB_OK); // for testing
                RichEdit1.Lines.EndUpdate;
                SokUtil_.ShowErrorMessages:=oShowErrorMessages;
                Screen.Cursor:=oCursor; fIsBusy:=False;
                OpenForm.DiskGroupBox.Show;
                if Assigned(SokoFile) then SokoFile.Modified:=False;
                if Assigned(Game) then Game.SokoFile:=nil;
                SearchLevelSnapshotsAsText.Free;
                SokoFile.Free; Game.Free; FileListBox.Free;
                StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
                StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
                if DirectoryExists(oCurrentDir) then SetCurrentDir(oCurrentDir);
                if   ScanState<>ssCloseWindow then ScanState:=ssIdle
                else PostMessage(Self.Handle,MSG_CLOSE,0,0);
                CloseBtn.Show; CancelBtn.Hide;
                if not (ActiveControl=DuplicateLevelsStringGrid) then CloseBtn.SetFocus;
                ShowStatus(False);
                if DuplicateLevelsCount=0 then TryToLoadGame(CurrentFileName);
       end;
     except on E:Exception do Result:=Error(E.Message,Caption);
     end;

  if not Result then Msg(TEXT_TASK_FAILED,Caption,MB_OK+MB_ICONINFORMATION);
end; // FindDuplicateLevels

function  TDuplicatesForm.Tools(Sender:TObject; const NewSokoFileName__:String):Boolean;
var  ScannedLevelsCount,DuplicateLevelsCount,ScannedFilesCount,ScannedDirectoriesCount,ScannedSolutionsCount,UpdatedSolutionsCount:Integer;
     oCollectionGroupBoxVisible,oDuplicateLevelsStringGridVisible:Boolean;
     oShowErrorMessages:TShowErrorMessages;
     oCursor:TCursor;
     oCurrentDir,oCurrentFileName,oGroupBoxCaption:String;
     NewSokoFile,SokoFile:TSokoFile; Game1,Game2:TGame;

  function Error(const Text__,Caption__:String):Boolean;
  begin
    SokUtil_.ShowErrorMessages:=semAll;
    Result:=SokUtil_.Error(Text__,Caption__);
  end; // Error

  procedure ShowStatus(IsScanning__:Boolean);
  begin
    Self.ShowStatus;

    with DuplicateLevelsStatusStringGrid1 do begin
      Cells[1,Ord(dlScanningFile)-Ord(dlScanning)]:='';
      if IsScanning__ then begin
         if Sender is TMenuItem then with Sender as TMenuItem do
            DuplicateLevelsGroupBox.Caption:=TitleWithOptionalSubTitle(DuplicateLevelsText,StrRemoveChar(StrRemoveChar(Caption,AMPERSAND),PERIOD));
         Cells[0,Ord(dlScanning)-Ord(dlScanning)]:=DuplicateLevelsStatusInfoText[dlScanning];
         //Cells[1,Ord(dlScanning)-Ord(dlScanning)]:=Format(PassMofNText__,[Succ(Ord(ScanTask)),2])+SPACE;
         Cells[0,Ord(dlScanningFolder)-Ord(dlScanning)]:=DuplicateLevelsStatusInfoText[dlScanningFolder];
         Cells[1,Ord(dlScanningFolder)-Ord(dlScanning)]:='';
         CancelBtn.SetFocus;
         end
      else begin
         Cells[0,Ord(dlScanning)-Ord(dlScanning)]:=ScanText;
         Cells[1,Ord(dlScanning)-Ord(dlScanning)]:='';
         if AnsiPos(MULTIPLE_ROOTS_TEXT_SUFFIX,ScannedDirectoryNamesText)=Succ(Length(ScannedDirectoryNamesText)-Length(MULTIPLE_ROOTS_TEXT_SUFFIX)) then
            Cells[0,Ord(dlScanningFolder)-Ord(dlScanning)]:=DuplicateLevelsStatusInfoFoldersText; // more than one root folder has been scanned
         Cells[1,Ord(dlScanningFolder)-Ord(dlScanning)]:=ScannedDirectoryNamesText;
         end;
      Update; //Repaint;
      end;

    with DuplicateLevelsStatusStringGrid2 do begin
      if   IsScanning__ then begin
           Cells[Ord(dlScannedDuplicates)-Ord(dlScanned),0]:=DuplicateLevelsStatusInfoText[dlScannedSolutions];
           DuplicateLevelsCount:=ScannedSolutionsCount;
           end
      else begin // 'True': the scan is over; restore the statistics for the duplicate finder
           ScannedLevelsCount     :=Integer(Objects[Ord(dlScannedLevels     )-Ord(dlScanned),1]);
           DuplicateLevelsCount   :=Integer(Objects[Ord(dlScannedDuplicates )-Ord(dlScanned),1]);
           ScannedFilesCount      :=Integer(Objects[Ord(dlScannedFiles      )-Ord(dlScanned),1]);
           ScannedDirectoriesCount:=Integer(Objects[Ord(dlScannedDirectories)-Ord(dlScanned),1]);
           Cells[Ord(dlScannedDuplicates)-Ord(dlScanned),0]:=DuplicateLevelsStatusInfoText[dlScannedDuplicates];
           end;
      if   ScannedLevelsCount<>0 then
           Cells[Ord(dlScannedLevels)-Ord(dlScanned),1]:=IntToStr(ScannedLevelsCount)
      else Cells[Ord(dlScannedLevels)-Ord(dlScanned),1]:='';
      if   (DuplicateLevelsCount<>0) or ((not IsScanning__) and (ScannedLevelsCount<>0)) then
           Cells[Ord(dlScannedDuplicates)-Ord(dlScanned),1]:=IntToStr(DuplicateLevelsCount)
      else Cells[Ord(dlScannedDuplicates)-Ord(dlScanned),1]:='';
      if   ScannedFilesCount<>0 then
           Cells[Ord(dlScannedFiles)-Ord(dlScanned),1]:=IntToStr(ScannedFilesCount)
      else Cells[Ord(dlScannedFiles)-Ord(dlScanned),1]:='';
      if   ScannedDirectoriesCount<>0 then
           Cells[Ord(dlScannedDirectories)-Ord(dlScanned),1]:=IntToStr(ScannedDirectoriesCount)
      else Cells[Ord(dlScannedDirectories)-Ord(dlScanned),1]:='';
      Update; //Repaint;
      end;

    StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
    StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
    StatusBar1.Repaint;

    if not IsScanning__ then
       if UpdatedLevelsCount<>0 then begin
          ToolsMenuItemUpdateSolutions.Enabled:=False; // running the update twice would duplicate the distributed solutions because the update doesn't check the solutions present in the level file; the update only considers the solutions loaded during the initial scan
          OpenForm.MenuItemOpenCopy.Enabled:=False; // the currently loaded level in the main window may have been updated with new solutions; don't allow the user to escape back to the main window without explicitly loading a level; (this is easier and safer than trying to track whether the level actually has received new solutions)
          OpenForm.BtnOpen2.Enabled:=False; OpenForm.BitBtnOpen.Enabled:=False;
          OpenForm.BtnCancel.Enabled:=False;
          if UpdatedSolutionsCount>0 then begin
             if Msg(Format(UpdatedLevelsAndSolutionsText__+NL+NL+DoYouWantToRefreshTheDuplicateLevelsWindowText,[UpdatedLevelsCount,UpdatedSolutionsCount]),Self.Caption,MB_YESNO+MB_ICONQUESTION)=IDYES then
                PostMessage(Self.Handle,MSG_REFRESH,0,0);
             end
          else if UpdatedSolutionsCount=0 then // otherwise, don't show message
                  Msg(Format(UpdatedLevelsAndSolutionsText__,[UpdatedLevelsCount,UpdatedSolutionsCount]),Self.Caption,MB_OK+MB_ICONINFORMATION);
          end
       else if UpdatedSolutionsCount>=0 then // otherwise, don't show message
               Msg(Format(UpdatedLevelsAndSolutionsText__,[UpdatedLevelsCount,UpdatedSolutionsCount]),Self.Caption,MB_OK+MB_ICONINFORMATION);
  end; // ShowStatus

  function  ScanDirectories(Item__:TLevelIndexItem):Boolean;

    function  ScanDirectory(var Item__:TLevelIndexItem):Boolean;
    var DirectoryName:String; FirstDirectoryItem:TNode; FirstItem,N:TLevelIndexItem;

      function  ScanFile(var Item__:TLevelIndexItem):Boolean;
      var FileLevelCount,NewSolutionsCount:Integer; ErrorText,ScannedFileName:String;
          FirstFileItem:TNode; Level,LevelRover:TLevel;

        function LoadLevel(const SearchedNormalizedBoard__:String; SokoFile__:TSokoFile; Game__:TGame; var LevelRover__,Level__:TLevel):Boolean;

          function LoadAndNormalizeBoard(Game__:TGame; Level__:TLevel):Boolean;
          var HashValue:THashTableHashKey; s:String;
          begin
            with Game__ do begin
              Result:=Level__.TextLinesToBoard(StartBoard,BoardWidth,BoardHeight,BoxCount,GoalCount,PlayerPos,s);
              if Result then begin
                 NormalizeBoard(True,False,True,AdvancedIdentityCheck,True,BoardWidth,BoardHeight,StartBoard,PlayerStartPos,History);
                 NormalizeRotationsAndReflectionsAndPlayerPosition(Game__,False,HashValue);
                 end;
              end;
          end;

        begin // LoadLevel
          Result  :=False;
          Level__ :=LevelRover__; // start searching from the level right after the previously found level (the levels in a file are processed in sequential order)

          while   (not Result) and Assigned(Level__) do with Game__ do
            if    (Level.Tag.Value=0) and // '0': the level hasn't been visited yet
                  LoadAndNormalizeBoard(Game__,Level__) and
                  (SearchedNormalizedBoard__=SokFile_.BoardToText(BoardWidth,BoardHeight,StartBoard,CR)) then
                  Result:=True
            else  Level__:=TLevel(Level__.Next);

          if      Assigned(Level__) then begin
                  Level__.Tag.Value:=1; // mark that the level has been visited
                  LevelRover__:=TLevel(Level__.Next); // start the next search for a matching level from the following level
                  end
          else if LevelRover__<>TLevel(SokoFile__.Levels.First) then begin // 'True': the search didn't find the level, but the search started somewhere in the middle of the level list; retry from the beginning of the level list
                  LevelRover__:=TLevel(SokoFile__.Levels.First);
                  Result:=LoadLevel(SearchedNormalizedBoard__,SokoFile__,Game__,LevelRover__,Level__);
                  end;
        end; // LoadLevel

        procedure AddLevelToNewCollection(Level__:TLevel; FileItem__:TNode);
        var Count:Integer; //Key,Value:String;

          function IsANumericName(const Name__:String):Boolean;
          var Index:Integer; Ch:Char;
          begin // returns 'True' is the name is numeric, or only contains digits, spaces, and parenthesis, e.g., '12 (2)' which sometimes occurs after accumulating anonymous levels in a collection
            Index:=Length(Name__);
            while Index>0 do begin
              Ch:=Name__[Index];
              if ((Ch<'0') or (Ch>'9')) and (Ch>SPACE) and (Ch<>LEFT_PAREN) and (Ch<>RIGHT_PAREN) then
                 Index:=0; // '-1': the name is not numeric; exit the 'while' loop
              Dec(Index);
              end;
            Result:=Index=0;
          end; // IsANumericName

        begin // AddLevelToNewCollection
          with Level__ do begin
            NewSokoFile.Levels.Push(Level__); // first make the level a member of the new collection so it's memory doesn't leak in case of exceptions
            NewSokoFile.Modified:=True;

            if DuplicateLevelsTask<>fdlCurrentCollection then
               Notes.MacroExpand(SokoFile.FileHeader.Lines,C_STYLE_NEWLINE);

            if Assigned(FileItem__) then begin

               if IsANumericName(Name)            then SetName(ChangeFileExt(FileItem__.Text,'')+SPACE+Name); // change number name to '<filename> <name>'
               if StrBeginsWith (Name,TEXT_LEVEL) then
                  if   StrEqual (Name,TEXT_LEVEL) then SetName(ChangeFileExt(FileItem__.Text,''))             // change 'Level'    to '<filename>'
                  else SetName  (StrSubstitute1(Name,TEXT_LEVEL,                                              // change 'Level...' to '<filename> level...'
                                                ChangeFileExt(FileItem__.Text,'')+SPACE+Copy(Name,1,Length(TEXT_LEVEL)),
                                                Count));
               end;
            end;
        end; // AddLevelToNewCollection

        function  AddImportedSolutionsToLevel(Level__:TLevel; Snapshots__:TList):Integer;
        var Node,NextNode:TNode; SnapshotAsText:TSnapshotAsText;
        begin // returns the number of imported solutions
          Result:=0; Node:=Snapshots__.First;
          while Assigned(Node) do begin
            NextNode:=Node.Next;
            if Node is TSnapshot then with Node as TSnapshot do
               if (not ReverseMode) and (GameState=gsSolved) and (SnapshotTag<>0) then begin // 'SnapshotTag<>0': 'True': this is one of the solutions imported from another level; it's not one of the original solutions for this level
                  SnapshotAsText:=MakeSnapshotAsText(True,False,False,True);
                  if   Assigned(SnapshotAsText) then begin
                       Level__.SnapshotsAsText.Add(SnapshotAsText);
                       Snapshots__.Remove(Node,True); // destroy the internal format snapshot; it would be a waste of memory to keep it on the list after it has been transformed to text format and exported to the level; the 'True' parameter: destroy the item after it has been removed from the list
                       Inc(Result);
                       end
                  else raise Exception.Create(TEXT_TASK_FAILED);
                  end;
            Node:=NextNode;
            end;
        end; // AddImportedSolutionsToLevel

        function  CollectSolutions(Item__:TLevelIndexItem):Boolean;
        type TColRow=record X,Y:Integer; end; {this 'TColRow' type declaration deliberately shadows the global declaration of 'TColRow'; the local declaration allows negative values so relative distances can be represented by this type}
        var  Game1NormalizationMoveCount,Game2NormalizationMoveCount,Game1NormalizationPushCount,Game2NormalizationPushCount:Integer;
             Game1NormalizationMovesDistance,Game1TopLeftPlayerNormalizationDistance,
             Game2NormalizationMovesDistance,Game2TopLeftPlayerNormalizationDistance:TColRow;
             Game1NormalizationDirectionMap,Game2NormalizationDirectionMap,InverseGame1NormalizationDirectionMap:TDirectionMap;
             ThisItem:TLevelIndexItem; Node:TNode;

          function  LoadLevel(Item__:TLevelIndexItem; Game__:TGame; var NormalizationMoveCount__,NormalizationPushCount__:Integer; var NormalizationMovesDistance__,TopLeftPlayerNormalizationDistance__:TColRow; var NormalizationDirectionMap__:TDirectionMap):Boolean;

            function  NormalizeBoard(Game__:TGame; BoardTransformation2D__:TBoardTransformation2D; var NormalizationMoveCount__,NormalizationPushCount__:Integer; var NormalizationMovesDistance__,TopLeftPlayerNormalizationDistance__:TColRow; var NormalizationDirectionMap__:TDirectionMap):Boolean;
            var Index,TopLeftCol,TopLeftRow:Integer; Direction:TDirection;
            begin
              Result:=True;
              with Game__ do begin
                // normalization depends on the orientation of the board, hence,
                // do the normalization before performing rotations and
                // reflections because that's what the duplicate finder did when
                // it classified levels as unique or identical;

                SokFile_.NormalizeBoard(True,False,True,AdvancedIdentityCheck,True,BoardWidth,BoardHeight,Board,PlayerPos,History);
                // 'NormalizeBoard' doesn't update 'Top' but only 'Count';
                // update it now so 'DoBoardTransformation' transforms any
                // initial forced moves which 'NormalizeBoard' has returned in
                // the history
                History.Top:=History.Count;

                DoBoardTransformation2D(BoardTransformation2D__,False); // transform the board to the version with normalized rotations and reflections
                CalculateTransformation2DDirections(True,BoardTransformation2D__,NormalizationDirectionMap__); // calculate the moves transformation

                NormalizationMoveCount__:=Game__.History.Count;     // forced initial normalization moves
                NormalizationPushCount__:=0;                        // forced initial normalization pushes are calculated in the following loop
                with NormalizationMovesDistance__ do begin // transform the initial normalization moves, if any
                  X:=0; Y:=0;
                  for Index:=1 to NormalizationMoveCount__ do with Game__.History do begin // calculate the player's relative movement during the forced initial normalization moves
                      Direction:=TDirection(Moves[Index] and H_MASK_DIRECTION); // the forced initial moves are made on the normalized board, hence, the moves don't need a transformation here
                      Inc(X,DIRECTION_XY[Direction,ColAxis]);
                      Inc(Y,DIRECTION_XY[Direction,RowAxis]);
                      if (Moves[Index] and H_FLAG_BOX)<>0 then
                         Inc(NormalizationPushCount__);
                      end;
                  end;

                CalculateInternalData; // this makes the board (in particular 'StartBoard' and 'PlayerStartPos') ready for replaying and testing the solutions and snapshots for this level; replay must, however, take into account that the board has been transformed to the version with normalized rotations and reflections

                if   (PlayerPos.X<>0) and (PlayerPos.Y<>0) then begin {normalize the player starting position; note that 'PlayerPos' and 'PlayerStartPos' are out of sync after this operation}
                     PlayerLegalMoves(0,0,TopLeftCol,TopLeftRow);
                     Dec(Board[PlayerPos.X,PlayerPos.Y],PLAYER);
                     Inc(Board[TopLeftCol ,TopLeftRow ],PLAYER);
                     PlayerPos.X:=TopLeftCol; PlayerPos.Y:=TopLeftRow;
                     TopLeftPlayerNormalizationDistance__.X:=PlayerPos.X-PlayerStartPos.X;
                     TopLeftPlayerNormalizationDistance__.Y:=PlayerPos.Y-PlayerStartPos.Y;
                     end
                else Result:=False; // no player: it's a malformed level
                end;
            end; // NormalizeBoard

            function  ConvertTextSnapshotsToInternalFormat(Item__:TLevelIndexItem; Game__:TGame):Boolean;
            var Node,NextNode:TNode; Snapshot:TSnapshot;
            begin
              Result:=True;
              try     Node:=Item__.Snapshots.First;
                      while Assigned(Node) and Result do begin
                        NextNode:=Node.Next; Snapshot:=nil;

                        if   Node is TSnapshotAsText then begin // 'True': the node is a text format snapshot; try to convert it to internal format
                             if   Game__.LoadSnapshotAsText(TSnapshotAsText(Node),nil,Snapshot) then with Game__ do begin // 'True': the text snapshot contains legal moves, and the snapshot has successfully been converted to internal format
                                  // update the game so it doesn't own the new snapshot
                                  if BestSolutionMoves        =Snapshot then BestSolutionMoves        :=nil;
                                  if BestSolutionPushes       =Snapshot then BestSolutionPushes       :=nil;
                                  if BuiltinBestSolutionMoves =Snapshot then BuiltinBestSolutionMoves :=nil;
                                  if BuiltinBestSolutionPushes=Snapshot then BuiltinBestSolutionPushes:=nil;
                                  if SaveGame                 =Snapshot then SaveGame                 :=nil;
                                  Snapshots.Remove(Snapshot,False);   // 'False': don't destroy the item, just remove it from the list in case the item is a member

                                  Snapshot.SetName(Node.Text);        // keep the original name

                                  Item__.Snapshots.Add   (Snapshot);  // put the newly created snapshot back on the list of snapshots for this level
                                  Item__.Snapshots.Remove(Node,True); // remove the textual representation of the snapshot from the list; 'True': destroy the node after removal from the list

                                  if   Snapshot.GameState=gsSolved then with DuplicateLevelsStatusStringGrid2 do begin
                                       Inc(ScannedSolutionsCount);
                                       Cells[Ord(dlScannedDuplicates)-Ord(dlScanned),1]:=IntToStr(ScannedSolutionsCount);
                                       if (ScannedSolutionsCount mod 100)=0 then begin
                                          DuplicateLevelsStatusStringGrid2.Update;
                                          Application.ProcessMessages;
                                          end;
                                       end;
                                  end
                             else begin
                                  Snapshot.Free; Snapshot:=nil;
                                  // a text format snapshot may contain illegal moves,
                                  // so it's not a reason to stop just because conversion
                                  // to an internal format snapshot failed;
                                  end;
                             end;

                        Node:=NextNode;
                        end;
              finally Game__.SetReverseMode(False);
                      Game__.Reset(False);
              end;
            end; // ConvertTextSnapshotsToInternalFormat

          begin // LoadLevel
            with Item__.BoardAsText do
              Result:=Game__.LoadFromBoardAsText(Width,Height,False,True,True,True,Board) and
                      ConvertTextSnapshotsToInternalFormat(Item__,Game__) and // convert text snapshots to internal format before the board is normalized; otherwise, the snapshots are invalid if the normalization moves the player away from its starting position
                      NormalizeBoard(Game__,Item__.BoardTransformation2D,NormalizationMoveCount__,NormalizationPushCount__,NormalizationMovesDistance__,TopLeftPlayerNormalizationDistance__,NormalizationDirectionMap__);
          end; // LoadLevel

          function  TestSnapshot(Snapshot__:TSnapshot):Boolean;
          var  Index,dx,dy,PushCountdown:Integer;
               __IsAFreezingMove:Boolean;
               Direction,DirectionA,DirectionB:TDirection;
               MoveA,MoveB:THistoryMove; Distance:TColRow; Node:TNode; //s1,s2:String;

            function MovePlayer(Col__,Row__:Integer; EndDirection__:TDirection):Boolean;
            var Index,dx,dy,PlayerMovesCount,PlayerLinesCount:Integer;
                IsPreferredDirectionOK:Boolean; ToPos:SokFile_.TColRow;
            begin
              with Game1 do begin
                Result:=(PlayerPos.X=Col__) and (PlayerPos.Y=Row__); // 'True': the player is already at the target square
                if not Result then begin // 'True': find a path to the target square
                   ToPos.X:=Col__; ToPos.Y:=Row__;
                   Result:=PlayerPath(PlayerPos,ToPos,True,True,EndDirection__,
                                      PlayerMovesCount,PlayerLinesCount,IsPreferredDirectionOK,
                                      PPlayerMoves(Addr(MovesOrHistory.Moves))); // 'True': there is a path to player square for the first push in the foreign snpshot
                   if Result then
                      for Index:=1 to PlayerMovesCount do // perform the moves to get the player to the target square
                          if GameState=gsPlay then
                             if    Result then begin
                                   dx:=MovesOrHistory.Moves[Index].X-PlayerPos.X;
                                   dy:=MovesOrHistory.Moves[Index].Y-PlayerPos.Y;
                                   if  IsALegalMove(dx,dy,0,__IsAFreezingMove) and (Game1.History.Count<MAX_MOVES) then
                                       DoMoveUpdateGame(dx,dy,History.Count,0)
                                  else Result:=False;
                                  end
                             else break;
                   end;
                end;
            end; // MovePlayer

          begin // 'TestSnapshot';
                // preconditions:
                //   the snapshot is a 'Game2' (the source level) unnormalized snapshot;
                //   'Game1' and 'Game2' have been normalized;
                //   'Game1' (the target level) has been reset;
            Result:=False;
            if (not Snapshot__.ReverseMode) and (Snapshot__.GameState=gsSolved) and (Snapshot__.SnapshotTag=0) then // 'SnapshotTag=0': 'True': this is an original snapshot and not an automatically generated snapshot
               with Snapshot__ do with Game1 do begin
                 // if initial moves (non-pushing player moves as well as box
                 // pushes) brings the player to its starting point on the
                 // normalized version of the board, then skip these moves;
                 // the snapshot may contain non-optimized moves, so it may take
                 // more than 'Game2NormalizationMoveCount' moves before the
                 // player gets there;
                 // if normalization has moved the player, but not any boxes,
                 // then it can happen that the snapshot moves don't bring the
                 // pusher to the normalized starting point; this is all right
                 // and is handled correctly by the following calculations;
                 PushCountDown := Game2NormalizationPushCount;
                 Index:=1;
                 Distance:=Game2NormalizationMovesDistance;
                 while   (Index<=MoveTop)
                         and
                         ((Distance.X<>0) or (Distance.Y<>0))
                         and
                         (((Moves^[Index] and H_FLAG_BOX)=0) or (PushCountdown>0))
                         do begin
                         if (Moves^[Index] and H_FLAG_BOX)<>0 then
                            Dec( PushCountdown );
                         Direction:=Game2NormalizationDirectionMap[TDirection(Moves^[Index] and H_MASK_DIRECTION)];
                         Dec(Distance.X,DIRECTION_XY[Direction,ColAxis]);
                         Dec(Distance.Y,DIRECTION_XY[Direction,RowAxis]);
                         Inc(Index);
                         end;

                 // find the relative movement caused by non-pushing player
                 // moves before the first push, if any;
                 // the calculation returns the relative distance to the
                 // top-left normalized player position
                 Inc( Distance.X,Game2TopLeftPlayerNormalizationDistance.X);
                 Inc( Distance.Y,Game2TopLeftPlayerNormalizationDistance.Y);
                 while   (Index<=MoveTop) and ((Moves^[Index] and H_FLAG_BOX)=0) do begin
                         Direction:=Game2NormalizationDirectionMap[TDirection(Moves^[Index] and H_MASK_DIRECTION)];
                         Dec(Distance.X,DIRECTION_XY[Direction,ColAxis]);
                         Dec(Distance.Y,DIRECTION_XY[Direction,RowAxis]);
                         Inc(Index);
                         end;

                 try     SetReverseMode(False);
                         Reset(False);
                         BestSolutionMoves .Free; BestSolutionMoves :=nil; // free existing best solutions, if any
                         BestSolutionPushes.Free; BestSolutionPushes:=nil;

                         History.Count:=Game1NormalizationMoveCount; // the board position matches the position after the forced initial moves, if any
                         History.Top  :=0;

                         Distance.X:=Game1TopLeftPlayerNormalizationDistance.X-Distance.X; // translate the snapshot's leading non-pushing player moves to the target game co-ordinate system
                         Distance.Y:=Game1TopLeftPlayerNormalizationDistance.Y-Distance.Y;

                         Result:=True;
                         while (Index<=MoveTop) and (History.Count<MAX_MOVES) and (GameState=gsPlay) and Result do begin
                           Direction:=Game2NormalizationDirectionMap[TDirection(Moves^[Index] and H_MASK_DIRECTION)];
                           dx:=DIRECTION_XY[Direction,ColAxis];
                           dy:=DIRECTION_XY[Direction,RowAxis];
                           if        (Moves^[Index] and H_FLAG_BOX)=0 then begin // 'True': this is a non-pushing player move; accumulate the relative player movement until the next box push
                                     Inc(Distance.X,DIRECTION_XY[Direction,ColAxis]);
                                     Inc(Distance.Y,DIRECTION_XY[Direction,RowAxis]);
                                     end
                           else if   MovePlayer(PlayerPos.X+Distance.X,PlayerPos.Y+Distance.Y,Direction) and // 'Distance' contains the relative player movement since the last box push (or since the normalized starting position)
                                     IsALegalMove(dx,dy,Ord(Direction),__IsAFreezingMove) and
                                     (History.Count<MAX_MOVES) then begin
                                     DoMoveUpdateGame(dx,dy,History.Count,0);
                                     Distance.X:=0; Distance.Y:=0; // clear the relative movement since the last box push
                                     end
                                else Result:=False;
                           Inc(Index);
                           end;

                         if (GameState=gsNull) and (History.Count=Game1NormalizationMoveCount) and (not Assigned(BestSolutionMoves)) and Result then begin
                            // the normalization moves has solved the level
                            GameState:=gsSolved;
                            History.Top:=History.Count;
                            Game1.TestForNewBestSolution;
                            end;

                         Result:=(GameState=gsSolved) and Assigned(BestSolutionMoves); // 'gsSolved': only solutions are considered; because 'Game1' has no previous solutions, the new solution is available in 'BestSolutionMoves' unless creation of the solution failed

                         if Result then with BestSolutionMoves do begin // 'True': the snapshot from the source level is also a solution for the target level
                            // transform the game to unnormalized moves
                            for Index:=1 to MoveTop do
                                Moves^[Index]:=(Moves^[Index] and (not H_MASK_DIRECTION)) or Ord(InverseGame1NormalizationDirectionMap[TDirection(Moves^[Index] and H_MASK_DIRECTION)]);

                            // check that it's not a duplicate solution
                            Node:=ThisItem.Snapshots.First;
                            while Assigned(Node) and Result do begin
                              if Node is TSnapshot then with Node as TSnapshot do
                                 if (GameState=gsSolved) and (MoveCount=BestSolutionMoves.MoveCount) then begin
                                    // non-pushing player-moves are allowed to
                                    // differ as long as the player reaches the
                                    // correct square before each push;
                                    // without this identity relaxation,
                                    // unwanted solutions could be created with only
                                    // minor differences in the non-pushing player moves;
                                    Index:=0; Distance.X:=0; Distance.Y:=0;
                                    while Index<MoveCount do begin
                                      Inc(Index);
                                      MoveA     :=Moves^                  [Index];
                                      MoveB     :=BestSolutionMoves.Moves^[Index];
                                      DirectionA:=TDirection(MoveA and H_MASK_DIRECTION);
                                      DirectionB:=TDirection(MoveB and H_MASK_DIRECTION);

                                      if        ((MoveA and H_FLAG_BOX)=0) and ((MoveB and H_FLAG_BOX)=0) then begin
                                                // both moves are non-pushing player moves;
                                                // accumulate the distance travelled by the player during
                                                // the current sequence of non-pushing player moves;
                                                Inc(Distance.X,DIRECTION_XY[DirectionA,ColAxis]-DIRECTION_XY[DirectionB,ColAxis]);
                                                Inc(Distance.Y,DIRECTION_XY[DirectionA,RowAxis]-DIRECTION_XY[DirectionB,RowAxis]);
                                                end
                                      else if   (DirectionA=DirectionB) and // 'True': the moves have the same direction
                                                (Distance.X=0) and (Distance.Y=0) then begin // 'True': no predecing non-pushing player moves differ
                                                end
                                           else Index:=Succ(MoveCount) // the moves differ; exit the loop; 'Succ(...)':  indicates that the moves differ;
                                      end;

                                    Result:=(Index>MoveCount)
                                            or
                                            (Distance.X<>0) or (Distance.Y<>0)
                                            or
                                            (SecondaryScoreMetrics.PlayerLines>BestSolutionMoves.SecondaryScoreMetrics.PlayerLines);
                                            //'True': the tested target level solution ('Node') differ from the
                                            // new solution ('BestSolutionMoves') imported from the source level
                                    end;

                              Node:=Node.Next;
                              end;

                            if Result then begin // 'True': the new solution is not a duplicate of an existing solution for the target level
                               BestSolutionMoves.SetName(StrSubstitute(BestSolutionMoves.Name,SnapshotTypeName(stBestSolution),SnapshotTypeName(stSolution),Index)); // rename the new solution from 'Best Solution' to 'Solution'
                               BestSolutionMoves.SnapshotTag:=1; // '1': non-zero marks that it's an automatically created solution, and not one of the original ones for this level
                               Snapshot__.Notes.CopyTo(TNode(BestSolutionMoves.Notes)); // copy notes
                               Snapshot__:=BestSolutionMoves; BestSolutionMoves:=nil; // 'steal' the best solution from the target game
                               ThisItem.Snapshots.Add(Snapshot__); // add the new solution to the target level snapshots
                               end;
                            end;
                 finally SetReverseMode(False);
                         Reset(False);
                         with PlayerPos do begin // move the player to the top-left normalized position; the sanity check in the enclosing function requires the player to be there
                           Board[X,Y]:=Board[X,Y] and (not PLAYER);
                           Inc(X,Game1TopLeftPlayerNormalizationDistance.X);
                           Inc(Y,Game1TopLeftPlayerNormalizationDistance.Y);
                           Board[X,Y]:=Board[X,Y] or PLAYER;
                           end;

                         BestSolutionMoves .Free; BestSolutionMoves :=nil; // free existing best solutions, if any
                         BestSolutionPushes.Free; BestSolutionPushes:=nil;
                 end;
               end;
          end; // TestSnapshot

        begin // CollectSolutions
          Result:=True; ThisItem:=Item__;
          if Assigned(ThisItem.NextDuplicate) and (ThisItem<>ThisItem.NextDuplicate) then // 'True': the level has duplicates
             if   LoadLevel(ThisItem,Game1,Game1NormalizationMoveCount,Game1NormalizationPushCount,Game1NormalizationMovesDistance,Game1TopLeftPlayerNormalizationDistance,Game1NormalizationDirectionMap) then begin
                  CalculateTransformation2DDirections(True,BOARD_TRANSFORMATION_INVERSE[ThisItem.BoardTransformation2D],InverseGame1NormalizationDirectionMap);
                  repeat Item__:=Item__.NextDuplicate;

                         if Assigned(Item__.Snapshots) and
                            (not Item__.Snapshots.IsEmpty) then
                            if   LoadLevel(Item__,Game2,Game2NormalizationMoveCount,Game2NormalizationPushCount,Game2NormalizationMovesDistance,Game2TopLeftPlayerNormalizationDistance,Game2NormalizationDirectionMap) then begin
                                 if   (Game1.BoardWidth =Game2.BoardWidth ) and
                                      (Game1.BoardHeight=Game2.BoardHeight) and
                                      Game1.IsAnIdenticalBoard(Game2.Board,True,True) then begin // 'True': this sanity check succeeded; the boards are identical after the transformations, including the normalized top-left player position after any forced initial normalization moves
                                      //Clipboard.AsText:=SokFile_.BoardToText(Game1.BoardWidth,Game1.BoardHeight,Game1.Board,NL);
                                      Node:=Item__.Snapshots.First;
                                      while Assigned(Node) do begin
                                        if Node is TSnapshot then with Node as TSnapshot do // 'True': the node is a snapshot in internal format
                                           TestSnapshot(TSnapshot(Node));
                                        Node:=Node.Next;
                                        end;
                                      end
                                 else begin
                                        // there can be several reasons why the
                                        // levels aren't identical, e.g.,:
                                        // 1. the duplicate finder searched for
                                        //    levels similar to the selected
                                        //    level using a
                                        //    match threshold < 100%;
                                        // 2. the user has chosen to filter out
                                        //    some of the square types, e.g.,
                                        //    goal squares;

                                        //debug only:
                                        //Clipboard.AsText:=ThisItem.Name+NL+SokFile_.BoardToText(Game1.BoardWidth,Game1.BoardHeight,Game1.Board,NL)+NL+NL+
                                        //                  Item__.Name  +NL+SokFile_.BoardToText(Game2.BoardWidth,Game2.BoardHeight,Game2.Board,NL);

                                        // Result:=Error(InternalErrorText,Caption);
                                      end;
                                 end
                            else begin // levels may be illegal; this isn't a reason to terminate
                                       //Result:=Error(TEXT_TASK_FAILED,Caption);
                                 end;

                  until  (Item__.NextDuplicate=ThisItem) or (not Result);
                  end
             else begin // levels may be illegal; this isn't a reason to terminate
                        // Result:=Error(TEXT_TASK_FAILED,Caption);
                  end;
        end; // CollectSolutions

        function  KeepBestSolutionsOnly(Level__:TLevel; SokoFile__:TSokoFile; Game__:TGame):Boolean;
        var ErrorText:String; Snapshot,NextSnapshot:TSnapshot;
        begin // returns 'True' if solutions have been deleted by the function
          Result:=False;
          with Game__ do begin
            Clear;
            if LoadFromFile(Level__.Name,SokoFile__,True,ErrorText) and (not Snapshots.IsEmpty) then begin
               Snapshot:=TSnapshot(Snapshots.First);
               repeat NextSnapshot:=TSnapshot(Snapshot.Next);
                      if Snapshot.GameState=gsSolved then begin
                         Result:=True;
                         Snapshots.Remove(Snapshot,True);
                         end;
                      Snapshot:=NextSnapshot;
               until  not Assigned(Snapshot);

               if Result then
                  if   SaveToFile0(SokoFile__,True,True,False,False,False) then
                       SokoFile__.Modified:=True
                  else raise Exception.Create(TEXT_TASK_FAILED);
               end;
            end;
        end; // KeepBestSolutionsOnly

      begin // ScanFile
        Result:=True;
        FirstFileItem:=Item__.FileItem;

        DuplicateLevelsStatusStringGrid1.Cells[1,Ord(dlScanningFile)-Ord(dlScanning)]:=Item__.FileItem.Text;
        DuplicateLevelsStatusStringGrid1.Repaint;

        ScannedFileName:=StrWithTrailingPathDelimiter(ExpandedFilePath(Trim(Item__.DirectoryItem.Text),MainForm.MyDocumentsFolder))+Item__.FileItem.Text;
        SokoFile.Clear;

        if   FileExists(ScannedFileName) and
             SokoFile.IsASokobanFile(ScannedFileName) and
             SokoFile.Open(ScannedFileName) then begin

             with StatusBar1 do
               if (Panels[OPEN_FORM_PANEL_INDEX_STATE].Text<>'')
                  or
                  (Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text<>'') then begin
                  Panels [OPEN_FORM_PANEL_INDEX_STATE].Text:='';
                  Panels [OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
                  Repaint;
                  end;

             if (DuplicateLevelsTask=fdlCurrentCollection) and
                (Sender<>ToolsMenuItemUpdateSolutions) and
                StrEqual(ScannedFileName,CurrentCollectionFileName) then begin
                SokoFile.FileHeader.CopyTo(NewSokoFile.FileHeader); // borrow the original fileheader; that way, macros don't need to be expanded for the individual levels
                end;

             FileLevelCount:=0; LevelRover:=TLevel(SokoFile.Levels.First);

             while Assigned(Item__) and (Item__.FileItem=FirstFileItem) and (ScanState=ssScan) and Result do begin

               if LoadLevel(Item__.Key,SokoFile,Game1,LevelRover,Level) then begin
                  if      Sender=ToolsMenuItemUpdateSolutions then begin // 'True': update all duplicates with the common solutions
                          Result:=CollectSolutions(Item__); // import solutions from duplicates, if any
                          if   Result then
                               NewSolutionsCount:=AddImportedSolutionsToLevel(Level,Item__.Snapshots)
                          else NewSolutionsCount:=0;
                          if   Result and (NewSolutionsCount<>0) then begin // '<>0': new solutions have been imported from one of more duplicates
                               SokoFile.Modified:=True; // signal that the file must be updated
                               Item__.HasBeenUpdated:=True;
                               end;
                          if   SettingsMenuItemToolsUpdateSolutionsBestSolutionsOnly.Checked and
                               KeepBestSolutionsOnly(Level,SokoFile,Game2) then
                               Item__.HasBeenUpdated:=True;
                          if   Item__.HasBeenUpdated then begin
                               Inc(UpdatedLevelsCount);
                               Inc(UpdatedSolutionsCount,NewSolutionsCount);
                               end;
                          end
                  else if Sender=ToolsMenuItemNewCollectionUniqueLevels then begin // 'True': create new collection with all unique levels, collecting all solutions from duplicates, if any
                          SokoFile.Levels.Remove(Level,False); // remove the level from the input file; 'False': don't destroy the level, only remove it from the list
                          if        not Assigned(Item__.NextDuplicate) then // 'True': the level is unique, i.e., no duplicates were found in this scan
                                    AddLevelToNewCollection(Level,Item__.FileItem) // transfer the unique level to the new file
                          else if   Item__.DuplicatesCount<>0 then begin // 'True': the level has duplicates
                                    AddLevelToNewCollection(Level,Item__.FileItem); // transfer the duplicated level to the new file
                                    Result:=CollectSolutions(Item__); // import solutions from duplicates, if any
                                    AddImportedSolutionsToLevel(Level,Item__.Snapshots);
                                    end
                               else begin Level.Free; Level:=nil; // delete the duplicate level; it has already been removed from the 'SokoFile' collection, hence, it can be destroyed here
                                    end;
                          if   Assigned(Level) and SettingsMenuItemToolsNewCollectionBestSolutionsOnly.Checked then
                               KeepBestSolutionsOnly(Level,NewSokoFile,Game2);
                          end
                  else if Sender=ToolsMenuItemNewCollectionSolvedLevels then begin // 'True': create new collection with all solved (unique) levels
                          Level.Tag.Value:=0; // '0': delete the level from the input file unless the following code lines finds solutions for the level
                          if        not Assigned(Item__.NextDuplicate) then begin // 'True': the level is unique, i.e., no duplicates were found in this scan; the level must be checked for having solutions
                                    Level.Tag.Value:=1; // '1': check whether the level has solutions
                                    end
                          else if   Item__.DuplicatesCount<>0 then begin // 'True': the level has duplicates
                                    Result:=CollectSolutions(Item__); // import solutions from duplicates, if any
                                    if Result then AddImportedSolutionsToLevel(Level,Item__.Snapshots);
                                    Level.Tag.Value:=1; // '1': check whether the level has solutions
                                    end;
                          if   Level.Tag.Value<>0 then begin // 'True': the level may have solutions; load the level in order to check it
                               Game2.Clear;
                               if   Game2.LoadFromFile(Level.Name,SokoFile,True,ErrorText)
                                    and
                                    (Assigned(Game2.BestSolutionMoves)
                                     or
                                     Assigned(Game2.BestSolutionPushes) // 'True': the level has one of more solutions
                                    ) then begin
                                    SokoFile.Levels.Remove (Level,False); // remove the level from the input file; 'False': only remove the level from the list, don't destroy it
                                    AddLevelToNewCollection(Level,Item__.FileItem); // transfer the solved level to the new file
                                    end
                               else SokoFile.Levels.Remove(Level,True); // delete the level from the input file; 'True': destroy the level after having removed it from the list
                               end
                          else SokoFile.Levels.Remove(Level,True); // delete the level from the input file; 'True': destroy the level after having removed it from the list
                          end
                  else if Sender=ToolsMenuItemNewCollectionUnsolvedLevels then begin // 'True': create new collection with all unsolved (unique) levels
                          Level.Tag.Value:=0; // '0': delete the level from the input file unless the following code lines finds solutions for the level
                          if        not Assigned(Item__.NextDuplicate) then begin // 'True': the level is unique, i.e., no duplicates were found in this scan; the level must be checked for having solutions
                                    Level.Tag.Value:=1; // '1': check whether the level has solutions
                                    end
                          else if   Item__.DuplicatesCount<>0 then begin // 'True': the level has duplicates
                                    Result:=CollectSolutions(Item__); // import solutions from duplicates, if any
                                    if   AddImportedSolutionsToLevel(Level,Item__.Snapshots)=0 then // 'True': no solutions were imported from duplicated levels
                                         Level.Tag.Value:=1 // '1': check whether the level has solutions
                                    else Level.Tag.Value:=0 // '0': the level has solutions; delete it from the input file
                                    end;
                          if   Level.Tag.Value<>0 then begin // 'True': the level may have solutions; load the level in order to check it
                               Game2.Clear;
                               if   Game2.LoadFromFile(Level.Name,SokoFile,True,ErrorText) and
                                    (not Assigned(Game2.BestSolutionMoves)) and
                                    (not Assigned(Game2.BestSolutionPushes)) then begin // 'True': no solutions were found for the level in this scan
                                    SokoFile.Levels.Remove(Level,False); // remove the level from the input file; 'False': only remove the level from the list, don't destroy it
                                    AddLevelToNewCollection(Level,Item__.FileItem); // transfer the unsolved level to the new file
                                    end
                               else SokoFile.Levels.Remove(Level,True); // delete the level from the input file; 'True': destroy the level after having removed it from the list
                               end
                          else SokoFile.Levels.Remove(Level,True); // delete the level from the input file; 'True': destroy the level after having removed it from the list
                          end
                  else if Sender=ToolsMenuItemNewCollectionCurrentlyDisplayedLevels then begin // 'True': create new collection with currently displayed levels
                          if        not Assigned(Item__.NextDuplicate) then begin // 'True': the level is unique, i.e., no duplicates were found in this scan
                                    end
                          else if   Item__.DuplicatesCount<>0 then begin // 'True': the level has duplicates
                                    CollectSolutions(Item__); // import solutions from duplicates, if any
                                    AddImportedSolutionsToLevel(Level,Item__.Snapshots);
                                    end;
                          Game2.Clear;
                          if   Game2.LoadFromFile(Level.Name,SokoFile,True,ErrorText) then begin
                               SokoFile.Levels.Remove(Level,False); // remove the level from the input file; 'False': only remove the level from the list, don't destroy it
                               AddLevelToNewCollection(Level,Item__.FileItem); // transfer the unsolved level to the new file
                               end
                          else SokoFile.Levels.Remove(Level,True); // delete the level from the input file; 'True': destroy the level after having removed it from the list
                          end;
                  end;

               if Result then with DuplicateLevelsStatusStringGrid2 do begin
                  Inc(ScannedLevelsCount); Inc(FileLevelCount);
                  Cells[Ord(dlScannedLevels)-Ord(dlScanned),1]:=IntToStr(ScannedLevelsCount);
                  if ((ScannedLevelsCount mod 100)=0) or ((FileLevelCount mod 100)=0) then begin
                     DuplicateLevelsStatusStringGrid2.Update;
                     Application.ProcessMessages;
                     end;
                  end;

               if Assigned(Item__) then
                  if Sender<>ToolsMenuItemNewCollectionCurrentlyDisplayedLevels then
                     Item__:=Item__.NextItem
                  else with DuplicateLevelsStringGrid do
                         if   FileLevelCount<RowCount-FixedRows then
                              Item__:=TLevelIndexItem(Objects[Ord(dlcRowNo),FixedRows+FileLevelCount])
                         else Item__:=nil;
               end;

             if Result and (ScanState=ssScan) then with DuplicateLevelsStatusStringGrid2 do begin
                Inc(ScannedFilesCount);
                Cells[Ord(dlScannedFiles)-Ord(dlScanned),1]:=IntToStr(ScannedFilesCount);
                Update;
                end;

             if (Sender=ToolsMenuItemUpdateSolutions) and
                SokoFile.Modified and Result and (ScanState=ssScan) then begin // 'True': the levels in the file have been updated with new solutions, no errors has been detected, and the user hasn't terminated the task; save the updated file to disk
                if   StrEqual(SokoFile.Name,OpenForm.Game.SokoFile.Name) then
                     try     SokoFile.Close;
                     finally OpenForm.Game.SokoFile.Clear;
                             OpenForm.Game.SokoFile.SetName('');
                             OpenForm.CurrentCollectionFileName:=''; // force the open form to reload the collection string grid if the current level belongs to a collection
                             OpenForm.TryToLoadFile(OpenForm.CurrentFileName);
                     end
                else SokoFile.Close;
                end;
             end
        else // file not found, or it doesn't contain Sokoban levels anymore; skip the levels previously found in this file
             while Assigned(Item__) and (Item__.FileItem=FirstFileItem) do Item__:=Item__.NextItem;
      end; // ScanFile

    begin // ScanDirectory
      Result:=True;
      FirstItem:=Item__;
      FirstDirectoryItem:=Item__.DirectoryItem;
      DirectoryName:=Trim(FirstDirectoryItem.Text);
      with DuplicateLevelsStatusStringGrid1 do begin
           Cells[1,Ord(dlScanningFolder)-Ord(dlScanning)]:=DirectoryName;
           Repaint;
         end;

      while Assigned(Item__) and (Item__.DirectoryItem=FirstDirectoryItem) and (ScanState=ssScan) and Result do begin
        Result:=ScanFile(Item__);
        Application.ProcessMessages;
        if (ScanState<>ssScan) and Result and Assigned(Item__) then
           if Msg(TerminateTaskText,TitleWithOptionalSubTitle(Application.Title,DuplicateLevelsGroupBox.Caption),MB_YESNOCANCEL+MB_ICONQUESTION+MB_DEFBUTTON2)<>ID_YES then
              ScanState:=ssScan;
        end;

      if Result and (ScanState=ssScan) and (Sender<>ToolsMenuItemNewCollectionCurrentlyDisplayedLevels) then with DuplicateLevelsStatusStringGrid2 do begin
         if   (FirstItem=LevelIndex.First) and (DuplicateLevelsTask<>fdlAllLevels) then begin // 'True': the first level(s) is/are special in that the current file has been parsed separately, before the remaining files were visited in directory order
              N:=Item__;
              while  Assigned(N) and (not StrEqual(DirectoryName,N.DirectoryItem.Name)) do N:=N.NextItem; // search for a pending file in the same directory
              if not Assigned(N) then Inc(ScannedDirectoriesCount); // 'True': the directory will not be visited again later, hence, update the count
              end
         else Inc(ScannedDirectoriesCount);
         Cells[Ord(dlScannedDirectories)-Ord(dlScanned),1]:=IntToStr(ScannedDirectoriesCount);
         Update;
         end;
    end; // ScanDirectory

  begin // ScanDirectories
    Result:=True;
    while Assigned(Item__) and (ScanState=ssScan) and Result do
      Result:=ScanDirectory(Item__);
  end; // ScanDirectories

begin // Tools
  Result:=False;
  if (Sender is TMenuItem) and
     TMenuItem(Sender).Visible and
     TMenuItem(Sender).Enabled then begin
     try

       DuplicateLevelsCount:=0; UpdatedLevelsCount:=0; UpdatedSolutionsCount:=0;
       ScannedLevelsCount:=0; ScannedFilesCount:=0; ScannedDirectoriesCount:=0; ScannedSolutionsCount:=0;

       NewSokoFile:=nil; SokoFile:=nil; Game1:=nil; Game2:=nil;

       oCursor:=Screen.Cursor;
       oShowErrorMessages:=SokUtil_.ShowErrorMessages;
       oGroupBoxCaption:=DuplicateLevelsGroupBox.Caption;
       oDuplicateLevelsStringGridVisible:=DuplicateLevelsStringGrid.Visible;
       oCollectionGroupBoxVisible:=OpenForm.CollectionGroupBox.Visible;
       oCurrentDir:=GetCurrentDir;
       oCurrentFileName:=OpenForm.CurrentFileName;

       try      fIsBusy:=True;
                ScanState:=ssScan;
                CancelBtn.Show; CloseBtn.Hide; CancelBtn.SetFocus;

                DuplicateLevelsStringGrid.Hide;
                if PanelToolTips.Visible then PanelToolTips.Hide;

                OpenForm.DiskGroupBox.Hide;
                OpenForm.CollectionGroupBox.Hide;
                OpenForm.Repaint;

                ShowStatus(True);

                StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
                StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
                Self.Update;

                NewSokoFile:=TSokoFile.Create;
                NewSokoFile.AddFileFormatDescriptionToFiles:=MainForm.SokoFile.AddFileFormatDescriptionToFiles;
                Result:=NewSokoFile.SetName(NewSokoFileName__);

                SokoFile:=TSokoFile.Create; SokoFile.AddFileFormatDescriptionToFiles:=MainForm.AddFileFormatDescriptionToFiles;
                Game1:=Game_.TGame.Create; Game1.SokoFile:=SokoFile; Game1.SecondaryMetricsInTitles:=MainForm.Game.SecondaryMetricsInTitles; Game1.DeadlockDetection.DeadlockDetectionType:=lmhLow;
                Game2:=Game_.TGame.Create; Game2.SokoFile:=SokoFile; Game2.SecondaryMetricsInTitles:=MainForm.Game.SecondaryMetricsInTitles; Game2.DeadlockDetection.DeadlockDetectionType:=lmhLow;

                SokUtil_.ShowErrorMessages:=semNone;

                Screen.Cursor:=crHourGlass;

                if Result then
                   if   Sender<>ToolsMenuItemNewCollectionCurrentlyDisplayedLevels then
                        Result:=ScanDirectories(LevelIndex.First)
                   else with DuplicateLevelsStringGrid do
                          Result:=(RowCount>FixedRows) and Assigned(Objects[Ord(dlcRowNo),FixedRows]) and ScanDirectories(TLevelIndexItem(Objects[Ord(dlcRowNo),FixedRows]));

                if Result
                   and
                   ((Sender=ToolsMenuItemNewCollectionUniqueLevels)
                    or
                    (Sender=ToolsMenuItemNewCollectionSolvedLevels)
                    or
                    (Sender=ToolsMenuItemNewCollectionUnsolvedLevels)
                    or
                    (Sender=ToolsMenuItemNewCollectionCurrentlyDisplayedLevels)
                   ) then with NewSokoFile do
                   if Modified and (not Levels.IsEmpty) then
                      if   ScanState=ssScan then begin
                           Levels.Reverse;
                           Modified:=True;
                           Close;
                           Modified:=False;
                           OpenForm.FileListBox1.Update;
                           TryToLoadGame(oCurrentFileName); // so the file listbox on the 'Open' form again focuses the selected item after the update
                           end
                      else Msg(Format(FileNotSavedText__,[NewSokoFileName__]),TitleWithOptionalSubTitle(Application.Title,DuplicateLevelsGroupBox.Caption),MB_OK+MB_ICONINFORMATION);

                //raise Exception.Create('Test.');
       finally  SokUtil_.ShowErrorMessages:=oShowErrorMessages;
                Screen.Cursor:=oCursor; fIsBusy:=False;
                OpenForm.DiskGroupBox.Show;
                if oCollectionGroupBoxVisible then OpenForm.CollectionGroupBox.Show;

                DuplicateLevelsGroupBox.Caption:=oGroupBoxCaption;
                if oDuplicateLevelsStringGridVisible then DuplicateLevelsStringGrid.Show;

                StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
                StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
                if (not Result) or (ScanState=ssCancel) then UpdatedSolutionsCount:=-1; // '-1': don't show result message

                if   DirectoryExists(oCurrentDir) then SetCurrentDir(oCurrentDir);
                if   ScanState<>ssCloseWindow then ScanState:=ssIdle
                else PostMessage(Self.Handle,MSG_CLOSE,0,0);
                CloseBtn.Show; CancelBtn.Hide;
                if (ScanState=ssIdle) and DuplicateLevelsStringGrid.Visible then DuplicateLevelsStringGrid.SetFocus;
                ShowStatus(False);

                if SettingsMenuItemToolsUpdateSolutionsBestSolutionsOnly.Checked then begin
                   UpdatedLevelsCount:=0; // '0': make the "Show updated levels" menu item on the "View" menu invisible; the number of updated levels isn't really meaningful when "update solutions" not only distribute solutions to duplicated levels but also deletes all solutions except the best ones
                   Self.ShowStatus;
                   end;

                if Assigned(NewSokoFile) then NewSokoFile.Modified:=False;
                if Assigned(SokoFile)    then SokoFile.Modified   :=False;
                if Assigned(Game1)       then Game1.SokoFile      :=nil;
                if Assigned(Game2)       then Game2.SokoFile      :=nil;
                NewSokoFile.Free; SokoFile.Free; Game1.Free; Game2.Free;
       end;
     except on E:Exception do Result:=Error(E.Message,Caption);
     end;
     if not Result then Msg(TEXT_TASK_FAILED,Caption,MB_OK+MB_ICONINFORMATION);
     end;
end; // Tools

procedure TDuplicatesForm.OnFontChange;
begin
  StatusBar1.Font.Assign(Self.Font);
  TopPanel.Font.Name:=Self.Font.Name;
  DuplicateLevelsStringGrid.Font.Name:=Self.Font.Name;
end;

end.

