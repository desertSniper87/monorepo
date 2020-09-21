unit LSView_;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Grids, Menus,
  Misc_,IniFile_,SokFile_,SokGame_,Tools_;

const
  INTERNAL_CLIPBOARD_INIFILE_SECTION='Internal Clipboard'; // don't localize

type
  TLevelSetForm = class(TForm)
    StatusBar1: TStatusBar;
    PanelPaste: TPanel;
    PanelDelete: TPanel;
    PanelNew: TPanel;
    PanelRename: TPanel;
    PanelGrid: TPanel;
    Grid: TStringGrid;
    PanelBoard: TPanel;
    ImageBoard: TImage;
    PanelName: TPanel;
    PanelInfo: TPanel;
    InfoMemo: TMemo;
    InfoLabel: TLabel;
    EditPopupMenu: TPopupMenu;
    MenuItemMemoEdit: TMenuItem;
    N1: TMenuItem;
    MenuItemMemoUndo: TMenuItem;
    N2: TMenuItem;
    MenuItemMemoCut: TMenuItem;
    MenuItemMemoCopy: TMenuItem;
    MenuItemMemoPaste: TMenuItem;
    MenuItemMemoDelete: TMenuItem;
    N3: TMenuItem;
    MenuItemMemoSelectAll: TMenuItem;
    N4: TMenuItem;
    MenuItemMemoPenColor: TMenuItem;
    MenuItemMemoBackgroundColor: TMenuItem;
    MenuItemMemoFont: TMenuItem;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    Edit1: TEdit;
    GridPopupMenu: TPopupMenu;
    MenuItemGridPaste: TMenuItem;
    MenuItemGridDelete: TMenuItem;
    MenuItemGridRename: TMenuItem;
    MenuItemGridDeleteAll: TMenuItem;
    N6: TMenuItem;
    MenuItemGridSortOnNames: TMenuItem;
    N5: TMenuItem;
    MenuItemGridCopyToWindowsClipboard: TMenuItem;
    MenuItemGridPasteFromWindowsClipboard: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; AState: TGridDrawState);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MenuItemMemoEditClick(Sender: TObject);
    procedure MenuItemMemoUndoClick(Sender: TObject);
    procedure MenuItemMemoCutClick(Sender: TObject);
    procedure MenuItemMemoCopyClick(Sender: TObject);
    procedure MenuItemMemoPasteClick(Sender: TObject);
    procedure MenuItemMemoDeleteClick(Sender: TObject);
    procedure MenuItemMemoSelectAllClick(Sender: TObject);
    procedure MenuItemMemoPenColorClick(Sender: TObject);
    procedure MenuItemMemoBackgroundColorClick(Sender: TObject);
    procedure MenuItemMemoFontClick(Sender: TObject);
    procedure EditPopupMenuPopup(Sender: TObject);
    procedure PanelPasteClick(Sender: TObject);
    procedure PanelDeleteClick(Sender: TObject);
    procedure PanelNewClick(Sender: TObject);
    procedure PanelRenameClick(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure InfoMemoEnter(Sender: TObject);
    procedure InfoMemoExit(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Edit1Enter(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure GridTopLeftChanged(Sender: TObject);
    procedure GridKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GridEnter(Sender: TObject);
    procedure GridPopupMenuPopup(Sender: TObject);
    procedure MenuItemGridDeleteAllClick(Sender: TObject);
    procedure GridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure GridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemGridSortClick(Sender: TObject);
    procedure ImageBoardMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormHide(Sender: TObject);
    procedure MenuItemGridCopyToWindowsClipboardClick(Sender: TObject);
    procedure MenuItemGridPasteFromWindowsClipboardClick(Sender: TObject);
  protected
    procedure ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
  private
    { Private declarations }
    BoxTargetMaskForDisplay:Integer;
    fInitialized:Boolean;
    DefaultPanelInfoHeight:Integer;
    fIsClosing:Boolean;
    LastClipBoardItemNo:Integer;
    MouseOverSoundEnabled:Boolean;

    oGridCellStr:String;
    oGridRow:Integer;

    SokoFile:TSokoFile;

    procedure ApplicationActivate(Sender: TObject);
    procedure ApplicationDeActivate(Sender: TObject);
    function  CloseEditor(HideFirst:Boolean):Boolean;
    procedure DisplayHint(Sender: TObject);
    procedure EnableDisableButtons(Initialize:Boolean);
    procedure Finalize;
    function  GetItem(GridIndex:Integer):TLevel;
    function  HideEditor:Boolean;
    function  InitializeGameViewer(BoardWidth__,BoardHeight__:Integer; BoardImage__:TImage):Boolean;
    function  RenameItem(GridIndex:Integer; const NewName:String):Boolean;
    procedure SetControlColor(Control: TWinControl; TextColor__,
                BackgroundColor__: TColor);
    procedure SetDefaultValues;
    procedure SetFormColors;
    procedure SetGridColumnsWidth;
    procedure SetItem(GridIndex:Integer; Item:TLevel);
    procedure ShowBoard(GridIndex:Integer);
    procedure ShowEditor;
    procedure ShowStatus;

  public
    { Public declarations }
    GameViewer:Tools_.TToolsGameViewer;
    FormColors:TFormColors;
    LastFocusedControl:TWinControl;
    ShowOnStartup:Boolean;
    StandardClipboardItemName:String;

    procedure Clear;
    function  EnableOrDisableNewItem:Boolean;
    procedure GameViewerCellToPos(Col__,Row__:Integer; var X__,Y__:Integer); // 0-based columns, rows
    procedure GameViewerMouseToCell(X__,Y__:Integer; var Col__,Row__:Integer); // 0-based columns, rows
    function  LoadItems:Boolean;
    function  LoadSettingsFromIniFile(const IniFile: TIniFile; const Section__:String): Boolean;
    procedure OnFontChange;
    function  SaveSettingsToIniFile(const IniFile: TIniFile; const Section__:String): Boolean;
    function  ShowItem(GridIndex:Integer): Boolean;
    procedure ShowBoard0(BoardWidth__,BoardHeight__:Integer; const Board__:TBoard; BoardImage__:TImage; Disabled__:Boolean);
    procedure ShowBoard1(BoardWidth__,BoardHeight__:Integer; const BoardAsText__,SelectedSquaresAsText__:String; BoardImage__:TImage);
    procedure ShowMove(const FromPos,ToPos,BoxStartPos:TPoint; BoxNo,TimeMS,MoveCount,PushCount:Integer; Undo,Jump,LastMove:Boolean);
    
    property  Initialized:Boolean read fInitialized;
    property  IsClosing:Boolean read fIsClosing;
    property  Items[GridIndex:Integer]:TLevel read GetItem write SetItem;

  end;

var
  LevelSetForm: TLevelSetForm = nil;

implementation

{$R *.DFM}

uses Clipbrd, SokUtil_,Text_,BitMap_,Pict_,Open1_,Main_, Snapshots_{,Sound_};

const
  DISABLED_ALPHA_BLEND_COLOR = clBlack;
  DISABLED_ALPHA_BLEND_PCT   = 25;
  MAX_SQUARE_SIZE            = 30;
  MIN_COL_WIDTH              = 1;
  MIN_ROW_HEIGHT             = 1;

  TAG_BUTTON                 = 1; // panels used as buttons are tagged with this number

procedure TLevelSetForm.FormCreate(Sender: TObject);
const SMALL_THUMBNAIL_HEIGHT_THRESHOLD=600; SMALL_THUMBNAIL_HEIGHT=150;
var i:Integer;
begin //
  OnFontChange;
  if biMinimize in BorderIcons then BorderIcons:=BorderIcons-[biMinimize]; // minimize: it may be impossible for the user to bring the application back on the screen again (a Microsoft Windows operating system bug when the general Windows text size settings is > 100%)  
  SokoFile:=nil; FillChar(GameViewer,SizeOf(GameViewer),0); fIsClosing:=False;
  LastClipBoardItemNo:=-1;

  if Screen.DeskTopHeight<SMALL_THUMBNAIL_HEIGHT_THRESHOLD then begin
     PanelBoard.Height:=SMALL_THUMBNAIL_HEIGHT;
     ImageBoard.Height:=PanelBoard.Height;
     end;

  Caption:=Application.Title+' - '+Caption;
  for i:=0 to Pred(ComponentCount) do
      if      Components[i] is TPanel       then with Components[i] as TPanel       do begin OnMouseDown:=FormMouseDown; OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TButton      then with Components[i] as TButton      do begin OnMouseDown:=FormMouseDown; OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TRadioButton then with Components[i] as TRadioButton do begin OnMouseDown:=FormMouseDown; OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TComboBox    then with Components[i] as TComboBox    do begin OnMouseDown:=FormMouseDown; OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TGroupBox    then with Components[i] as TGroupBox    do begin OnMouseDown:=FormMouseDown; OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TListBox     then with Components[i] as TListBox     do begin OnMouseDown:=FormMouseDown; OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
//    else if Components[i] is TStringGrid  then with Components[i] as TStringGrid  do begin OnMouseDown:=FormMouseDown; OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TImage       then with Components[i] as TImage       do begin OnMouseDown:=FormMouseDown; OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
//    else if Components[i] is TMemo        then with Components[i] as TMemo        do begin OnMouseDown:=FormMouseDown; OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TStatusBar   then with Components[i] as TStatusBar   do begin OnMouseDown:=FormMouseDown; OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end;

  ImageBoard.OnMouseDown:=nil; ImageBoard.OnMouseUp:=ImageBoardMouseUp;
  PanelName .OnMouseDown:=nil; PanelName .OnMouseUp:=ImageBoardMouseUp;

  Grid.Align:=alClient;
  Grid.Cells[1,0]:='';
  ImageBoard.Align:=alClient;
  InFoMemo.Align:=alClient;

  DefaultPanelInfoHeight:=PanelInfo.Height;

  InfoLabel.Top:=PanelBoard.Top+PanelBoard.Height+PanelName.Top;
  PanelGrid.Top:=InfoLabel.Top+InfoLabel.Height+PanelName.Top;
  PanelInfo.Top:=PanelGrid.Top+PanelGrid.Height+PanelName.Height;

  oGridRow         :=-1; oGridCellStr:='';
  PanelName.Caption:=''; fInitialized:=False; BoxTargetMaskForDisplay:=0;
  Items[Grid.Row]:=nil; MouseOverSoundEnabled:=True;

  SetDefaultValues;

  try    SokoFile:=TSokoFile.Create;
         GameViewer.BackgroundPict:=Pict_.TPict.Create;
         GameViewer.SkinPict:=TPict.Create;
  except on E:Exception do begin
            Finalize;
            raise;
            end;
  end;
end;

procedure TLevelSetForm.FormDestroy(Sender: TObject);
begin
  Finalize;
end;

procedure TLevelSetForm.Finalize;
begin
  with GameViewer do begin
    if BackgroundPict<>nil then BackgroundPict.OrgBitMap:=nil;
    if SkinPict      <>nil then SkinPict      .OrgBitMap:=nil;
    BackgroundPict.Free;
    SkinPict.Free;
    FillChar(GameViewer,SizeOf(GameViewer),0);
    end;

  InfoMemoExit(Self);
  SokoFile.Free; SokoFile:=nil;
  LevelSetForm:=nil;
end;

procedure TLevelSetForm.ApplicationActivate(Sender: TObject);
begin
  FormActivate(nil); // to set the right colors;
  if MainForm.Music<>nil then MainForm.Music.OnActivateApplication;
end;

procedure TLevelSetForm.ApplicationDeActivate(Sender: TObject);
begin
  FormDeactivate(nil);  // reset color for active control
  if MainForm.Music<>nil then MainForm.Music.OnDeactivateApplication;
end;

procedure TLevelSetForm.FormActivate(Sender: TObject);
var s:String;
begin
  //MainForm.MPlayer.Hide;

  Application.OnActivate   :=ApplicationActivate;
  Application.OnDeactivate :=ApplicationDeactivate;
  Application.OnHint       :=DisplayHint;
  Application.OnMessage    :=ApplicationOnMessage;

  ToolsForm.EditMenuItemDelete.ShortCut:=ShortCut(Ord(NULL_CHAR),[]);

  ToolsForm.StatusBar1.Panels[1].Text:='';
  ToolsForm.ShowStatus;

  fIsClosing               :=False;
  MouseOverSoundEnabled    :=False; // skip sound the first time; otherwise the menu-over sound somehow is played when the form opens

  if ToolsForm.Initialized  then begin

     if Initialized then
        ShowBoard(Grid.Row)
     else begin
        fInitialized:=True;
        FormResize(Sender);
        Clear;

        SokoFile.HasBoardTags:=True;
        SokoFile.AddFileFormatDescriptionToFiles:=False;

        s:=MainForm.ApplicationDataPath+InternalClipBoardText+TEXT_FILE_EXT;
        if   FileExists(s) and SokoFile.IsASokobanFile(s) then begin
             LoadItems;
             ShowItem(Grid.FixedRows);
             end
        else SokoFile.New(s);
        end;

     SetFormColors;
     StatusBar1.SizeGrip:=IsWindowsDefaultColorBtnFace(StatusBar1.Color) or
                          (Graphics.ColorToRGB(StatusBar1.Color)=Graphics.ColorToRGB(clBlack ));

     LastFocusedControl:=nil; FormMouseMove(ActiveControl,[],0,0);
     if Grid.Visible then Grid.Invalidate;
     EnableDisableButtons(True);
     if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
     end
  else begin
     ToolsForm.SetFocus;
     end;
end;

procedure TLevelSetForm.FormDeactivate(Sender: TObject);
begin
//ToolsForm.SetMessageHandlers;

  MouseOverSoundEnabled:=True; // so a 'save new' event triggered by a keyboard command triggers the menu-select sound

  CloseEditor(True);
  InfoMemoExit(Sender);
  if Grid.Dragging then Grid.EndDrag(False);
  if Grid.Visible then Grid.Invalidate;
  FormMouseMove(nil,[],0,0);
end;

procedure TLevelSetForm.FormClose(Sender: TObject; var Action: TCloseAction);
var CanSelect:Boolean;
begin
  GridSelectCell(nil,Grid.Col,Grid.Row,CanSelect);
  FormMouseMove(nil,[],0,0);
  fIsClosing:=True;
end;

procedure TLevelSetForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=CloseEditor(False);
end;

procedure TLevelSetForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin //
  if (Key=Ord(ACCEL_CHAR_NEW)) and
     ((ssAlt in Shift) or (ssCtrl in Shift)) then PanelNewClick(Sender);
end;

procedure TLevelSetForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var Editing:Boolean;
begin
  if (Screen.ActiveForm=Self) then begin
     end
  else begin
     end;
{
  Editing:=( ( (ActiveControl=InfoMemo) and (not InfoMemo.ReadOnly) )
             or
             Edit1.Visible
           )
           and
           (not ((ssCtrl in Shift) or (ssAlt in Shift)));
}

  Editing:=( Edit1.Visible
             or
             ( (ActiveControl=InfoMemo)
               and
               (not InfoMemo.ReadOnly)
               and
               (Shift<>[ssAlt])
             )
           );

  if              Key=VK_ESCAPE                      then
                  if   Edit1.Visible                 then //
                  else Close
  else if         Key=SIMULATED_KEYBOARD_EVENT_KEY   then //
  else if not Editing then begin
          if      Key=VK_ESCAPE                      then Close
          else if Key=VK_F2                          then PanelRenameClick(Sender)
          else if Key=VK_DELETE                      then PanelDeleteClick(Sender)
          else if Key=VK_INSERT                      then PanelNewClick(Sender)
          else if Key=Ord(ACCEL_CHAR_OPEN)           then PanelPasteClick(Sender)
          else if Key=Ord(ACCEL_CHAR_PASTE_FROM_CLIPBOARD)
                                                     then PanelPasteClick(Sender)
          else if Key=Ord(ACCEL_CHAR_DELETE)         then PanelDeleteClick(Sender)
          else if Key=Ord(ACCEL_CHAR_NEW)            then PanelNewClick(Sender)
          else if Key=Ord(ACCEL_CHAR_RENAME)         then PanelRenameClick(Sender)
          else if Key=Ord(ACCEL_CHAR_EXIT)           then ToolsForm.SetFocus
          else if Key=VK_TAB                         then
                  if Shift=[]                        then ToolsForm.SetFocus
                  else
          else if Key=VK_SHIFT                       then ToolsForm.SetFocus
          else if Key=VK_RETURN                      then
                  if PanelName.Caption=''            then PanelNewClick(Sender)
                  else
          else if Key=VK_LEFT                        then
                  if Shift=[]                        then ToolsForm.SetFocus
                  else
          else if Key=VK_RIGHT                       then
                  if Shift=[]                        then begin
                     ToolsForm.SetFocus; Close;
                     end
                  else
          else if Key=VK_F12                         then
                  if   WindowState=wsNormal          then
                       WindowState:=wsMaximized
                  else WindowState:=wsNormal
          else;
          if (ActiveControl<>Grid) and
             (Screen.ActiveForm=Self) and
             Grid.Visible and
             (ActiveControl<>Edit1) and
             (ActiveControl<>InfoMemo)               then Grid.SetFocus;
          end;
end;

procedure TLevelSetForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin //
  if Screen.ActiveForm=Self then begin
     end
  else begin
     end;

  MouseOverSoundEnabled:=False;
  if      Button=mbLeft then
          if   Sender=PanelGrid then PanelNewClick(Sender)
          else begin if Sender is TPanel then with Sender as TPanel do
                        if Enabled then begin
                           BevelOuter:=bvLowered; Repaint;
                           end;
               end
  else if Button=mbRight then
          if (Sender<>Grid) and
             (Sender<>ImageBoard) and
             (Sender<>PanelName) then
             Close;
end;

procedure TLevelSetForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin //
  if TWinControl(Sender)<>LastFocusedControl then with FormColors do begin
     if LastFocusedControl<>nil then begin
        SetControlColor(LastFocusedControl,ButtonTextColor,ButtonColor);
        if LastFocusedControl is TPanel then with LastFocusedControl as TPanel do
           if BevelOuter=bvLowered then BevelOuter:=bvRaised;
        end;
     LastFocusedControl:=TWinControl(Sender);
     if LastFocusedControl<>nil then SetControlColor(LastFocusedControl,FocusedButtonTextColor,FocusedButtonColor);
     if Sender=Grid then GridMouseMove(Sender,Shift,X,Y)
     else if (Sender is TPanel) and
             (TPanel(Sender).Tag=TAG_BUTTON)
             //and
             //MainForm.Sound.Enabled
             then
             if   MouseOverSoundEnabled then
                  //MainForm.Sound.Play(stMenuOver)
             else MouseOverSoundEnabled:=True;

     end;

  if Screen.ActiveForm=Self then begin
     if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
     if not ToolsForm.Editor.Selection.Enabled then ToolsForm.HideCellCursor;
     end;
end;

procedure TLevelSetForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then Close;
end;

procedure TLevelSetForm.FormResize(Sender: TObject);
var i:Integer; Edit:Boolean;
begin
  i:=2*PanelPaste.Width+16;
  if ClientWidth<i then ClientWidth:=i;

  PanelInfo.Height:=DefaultPanelInfoHeight;
  i:=PanelGrid.Top+4*Grid.DefaultRowHeight+PanelInfo.Height+2*PanelPaste.Height+6*PanelName.Top+StatusBar1.Height;
  if ClientHeight<i then ClientHeight:=i;
  if Initialized then begin
     Edit:=HideEditor;

     with PanelName            do begin Width :=Self.ClientWidth-2*Left;
                                  end;
     with PanelBoard           do begin Width :=PanelName.Width;
                                  end;
     with PanelPaste            do begin Left  :=Max(PanelName.Left,(Self.ClientWidth-(2*(PanelPaste.Width+PanelName.Left))) div 2);
                                        Top   :=Self.ClientHeight-StatusBar1.Height-PanelName.Top-2*PanelPaste.Height;
                                  end;
     with PanelDelete          do begin Left  :=PanelPaste.Left+PanelPaste.Width;
                                        Top   :=PanelPaste.Top;
                                  end;
     with PanelNew             do begin Left  :=PanelPaste.Left;
                                        Top   :=PanelPaste.Top+PanelPaste.Height;
                                  end;
     with PanelRename          do begin Left  :=PanelDelete.Left;
                                        Top   :=PanelNew.Top;
                                  end;
     with InfoLabel            do begin Left  :=PanelName.Left;
                                        Width :=PanelName.Width;
                                  end;
     with PanelInfo            do begin Width :=PanelName.Width;
                                        Top   :=PanelPaste.Top-Height-PanelName.Top;
                                  end;
     with PanelGrid            do begin Top   :=InfoLabel.Top+InfoLabel.Height+PanelName.Top;
                                        Width :=PanelName.Width;
                                        Height:=PanelInfo.Top-Top-PanelName.Top;
                                        {
                                        if (Grid.RowCount>1) and
                                           (Grid.Cells[1,Pred(Grid.RowCount)]<>'') then // grid isn't empty
                                           Height:=Height-Grid.Height+(Grid.VisibleRowCount+Grid.FixedRows)*(Grid.DefaultRowHeight+Grid.GridLineWidth); // drop unused space below the grid
                                        }
                                  end;
     with PanelInfo            do begin Top   :=PanelGrid.Top+PanelGrid.Height+PanelName.Top;
                                        Height:=PanelPaste.Top-Top-PanelName.Top;
                                  end;
     with Grid                 do begin SetGridColumnsWidth;
                                  end;
     with StatusBar1           do begin Width :=Self.ClientWidth;
                                        Top   :=Self.ClientHeight-Height;
                                  end;
     with Edit1                do begin Left  :=Grid.Left+Grid.ColWidths[0]+Grid.GridLineWidth*2+0;
                                        Width :=Grid.ColWidths[1]+Grid.GridLineWidth*2+0;
                                  end;
     FormMouseMove(nil,[],0,0);
     //if not OpenForm.GameViewer.Initialized then
     //   OpenForm.GameViewer.LoadPictures;
     //OpenForm.BoardResize(ImageBoard);
     GameViewer.BackgroundInitialized:=False;
     GameViewer.SkinInitialized:=False;
     ShowBoard(Grid.Row);
     if Edit then ShowEditor;

     if Left>Screen.DeskTopLeft+Screen.DeskTopWidth -30 then Left:=Screen.DeskTopLeft+Screen.DeskTopWidth -30;
     if Top >Screen.DeskTopTop +Screen.DeskTopHeight-30 then Top :=Screen.DeskTopTop +Screen.DeskTopHeight-30;
     end;
end;

procedure TLevelSetForm.DisplayHint(Sender: TObject);
begin
  StatusBar1.{Panels[1].Text}SimpleText := GetLongHint(Application.Hint);
end;

procedure TLevelSetForm.ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if   (Msg.Message = WM_MOUSEWHEEL) then
       if Msg.WParam>=0 then begin
          end
       else begin
          end
  else Handled:=False;
end;

procedure TLevelSetForm.SetDefaultValues;
begin // color numbers: BGR, not RGB
  Left                             :=Screen.Width-Width;
  Top                              :=0;
  Height                           :=Min(Screen.Height,800)-100;

  InfoMemo.Color                   :=clTeal;
  InfoMemo.Font.Color              :=clLtGray;

  StandardClipboardItemName        :=ClipboardItemText;

  InfoMemo.ReadOnly                :=False; MenuItemMemoEditClick(nil);
  ShowOnStartup                    :=False;

  SetDefaultFormColors(FormColors);
  FormColors.ButtonColor           :=clGreen;
end;

procedure TLevelSetForm.SetFormColors;
var i:Integer;
begin
  with FormColors do begin
    MakeUniqueGrayedButtonTextColor(FormColors);

    FormMouseMove(nil,[],0,0);
      for i:=0 to Pred(ComponentCount) do
        if Components[i] is TPanel then with Components[i] as TPanel do
           if Tag=TAG_BUTTON then begin
              Font.Color:=ButtonTextColor; Color:=ButtonColor;
              end;

    with Self                do begin Font.Color:=BackgroundTextColor;    Color:=BackgroundColor;    end;
    with InfoLabel           do begin Font.Color:=BackgroundTextColor;    Color:=BackgroundColor;    end;
    with PanelGrid           do begin Font.Color:=BackgroundTextColor;    Color:=BackgroundColor;    end;
    with Grid                do begin Font.Color:=WindowTextColor;        Color:=WindowColor;        end;
    with Edit1               do begin Font.Color:=FocusedButtonTextColor; Color:=FocusedButtonColor; end;
    end;
end;

procedure TLevelSetForm.SetControlColor(Control:TWinControl; TextColor__,BackgroundColor__:TColor);
begin
  with FormColors do
    if      Control is TPanel                                then with Control as TPanel       do
            if Tag=TAG_BUTTON                                then
               if Font.Color=GrayedButtonTextColor           then  Color:=GrayedButtonColor
               else                                          begin Font.Color:=TextColor__; Color:=BackgroundColor__; end
            else
    else if Control is TStringGrid                           then with Control as TStringGrid  do
            if Tag=TAG_BUTTON                                then
                 begin Font.Color:=TextColor__;              Color:=BackgroundColor__; end
            else
    else if Control is TRadioButton                          then with Control as TRadioButton do
            if   Checked and (BackgroundColor__=ButtonColor) then
                 begin Font.Color:=HighlightedTextColor;     Color:=BackgroundColor; end
            else if BackgroundColor__=FocusedButtonColor     then
                 begin Font.Color:=TextColor__;              Color:=BackgroundColor__; end
            else begin Font.Color:=BackgroundTextColor;      Color:=BackgroundColor; end
    else if Control is TComboBox                             then with Control as TComboBox    do
                     begin Font.Color:=TextColor__;          Color:=BackgroundColor__; end;
//  else if Control is TEdit                                 then with Control as TEdit        do
//                   begin Font.Color:=TextColor__;          Color:=BackgroundColor__; end;
end;

function TLevelSetForm.LoadSettingsFromIniFile(const IniFile: TIniFile; const Section__:String): Boolean;
begin // LoadSettingsFromIniFile;
  Left                     :=Max(Screen.DeskTopLeft               ,Min(Screen.DeskTopLeft  +Screen.DeskTopWidth -SnapshotsForm.MinimumClientWidth ,IniFile.ReadInteger(Section__,'Left',Left)));
  Top                      :=Max(Screen.DeskTopTop                ,Min(Screen.DeskTopTop   +Screen.DeskTopHeight-SnapshotsForm.MinimumClientHeight,IniFile.ReadInteger(Section__,'Top',Top)));
  Width                    :=Max(SnapshotsForm.MinimumClientWidth ,Min(Screen.DeskTopWidth -Left,IniFile.ReadInteger(Section__,'Width',Width)));
  Height                   :=Max(SnapshotsForm.MinimumClientHeight,Min(Screen.DeskTopHeight-Top,IniFile.ReadInteger(Section__,'Height',Height)));

  ShowOnStartup            :=IniFile.ReadBool(Section__,'ShowOnStartup',ShowOnStartup);
  StandardClipboardItemName:=IniFile.ReadString(Section__,'StandardClipboardItemName',StandardClipboardItemName);
  InfoMemo.ReadOnly        :=IniFile.ReadBool(Section__,'MemoReadOnly',InfoMemo.ReadOnly);
  MenuItemMemoEditClick(nil);
  InfoMemo.Color           :=TColor(IniFile.ReadInteger(Section__,'MemoColor',Integer(InfoMemo.Color)));
  Result                   :=LoadFormColorsFromIniFile(IniFile,Section__,FormColors) and
                             LoadFontFromIniFile(IniFile,Section__,'Memo',InfoMemo.Font) and
                             LoadFontFromIniFile(IniFile,Section__,'Window',Self.Font);
  OnFontChange;
end;

function TLevelSetForm.SaveSettingsToIniFile(const IniFile: TIniFile; const Section__:String): Boolean;
begin // SaveSettingsToIniFile;
  try
         if  WindowState=wsNormal then begin
             IniFile.WriteInteger(Section__,'Left',Left);
             IniFile.WriteInteger(Section__,'Top',Top);
             IniFile.WriteInteger(Section__,'Width',Width);
             IniFile.WriteInteger(Section__,'Height',Height);
             end;

         IniFile.WriteBool   (Section__,'ShowOnStartup',ShowOnStartup);
         IniFile.WriteString (Section__,'StandardClipboardItemName',StandardClipboardItemName);
         IniFile.WriteBool   (Section__,'MemoReadOnly',InfoMemo.ReadOnly);
         IniFile.WriteInteger(Section__,'MemoColor',Integer(InfoMemo.Color));
         Result:=SaveFormColorsToIniFile(IniFile,Section__,FormColors) and
                 SaveFontToIniFile(IniFile,Section__,'Memo',InfoMemo.Font) and
                 SaveFontToIniFile(IniFile,Section__,'Window',Self.Font);
  except on E:Exception do Result:=Error(E.Message,Application.Title);
  end;
end;

procedure TLevelSetForm.EnableDisableButtons(Initialize:Boolean);
var s:String;

  procedure SetColor(Panel:TPanel);
  begin
    with FormColors do begin
      if   Panel.Enabled then
           Panel.Font.Color:=ButtonTextColor
      else Panel.Font.Color:=GrayedButtonTextColor;
      if   Panel=LastFocusedControl then
           SetControlColor(Panel,FocusedButtonTextColor,FocusedButtonColor)
      else SetControlColor(Panel,ButtonTextColor,ButtonColor);
      end;
  end;

begin // EnableDisableButtons;
  if (PanelName .Enabled<>(PanelName.Caption<>'')) or Initialize then begin
     PanelName  .Enabled:=PanelName.Caption<>'';
     ImageBoard .Enabled:=PanelName.Enabled;
     PanelPaste .Enabled:=PanelName.Enabled;
     PanelDelete.Enabled:=PanelPaste.Enabled;

     EnableOrDisableNewItem;

     PanelRename.Enabled:=PanelPaste.Enabled;

     SetColor(PanelName);
     SetColor(PanelPaste);
     SetColor(PanelDelete);
     SetColor(PanelNew);
     SetColor(PanelRename);

     if   PanelName.Enabled then
          s:=HintPasteItemText
     else s:='';
     PanelName .Hint:=s;
     ImageBoard.Hint:=s;

     if PanelName.Caption='' then begin
        Grid.Hide; oGridRow:=-1;
        if PanelNew.Enabled then begin
           PanelGrid.Show; ActiveControl:=PanelNew;
           end
        else begin
           PanelGrid.Hide; ActiveControl:=InfoMemo;
           end;
        InfoLabel.Caption:='';
        end
     else if (not PanelGrid.Visible) or (not Grid.Visible) then begin
             PanelGrid.Show;
             Grid.Show;
             SetGridColumnsWidth;
             end;
     end;
  FormMouseMove(nil,[],0,0); // to update colors on the screen
  //MainForm.MakeDynamicHints;
  ShowStatus;
end;

procedure TLevelSetForm.ShowStatus;
begin //

end;

procedure TLevelSetForm.Clear;
var i:Integer;
begin
  if (Grid.RowCount>Succ(Grid.FixedRows)) or
     (Grid.Cells[1,Grid.FixedRows]<>'') then begin
     for i:=0 to Pred(Grid.RowCount) do Items[i]:=nil;
     Grid.RowCount:=Succ(Grid.FixedRows);
     Grid.Cells[1,Grid.FixedRows]:='';
     end;
  Grid.Cells[1,0]:=InternalClipboardText;
  ShowItem(0);
  LastClipBoardItemNo:=-1;
end;

function  TLevelSetForm.LoadItems:Boolean;
var i:Integer; s:String; Node:TNode;
begin
  Result:=False;
  if (SokoFile<>nil) and (SokoFile.Levels<>nil) then with Grid do
     try
       if SokoFile.Levels.IsEmpty
          and
          ((RowCount>Succ(FixedRows))
           or
           (Cells[1,FixedRows]<>'')) then Clear
       else begin
          for i:=0 to Pred(RowCount) do Items[i]:=nil;
          for i:=0 to Pred(ColCount) do Cells[i,FixedRows]:='';
          RowCount:=FixedRows+Max(1,SokoFile.Levels.Count);
          Node:=SokoFile.Levels.First; LastClipBoardItemNo:=0;
          while Node<>nil do begin
            Cells[0,LastClipBoardItemNo+FixedRows]:=IntToStr(Succ(LastClipBoardItemNo));
            Cells[1,LastClipBoardItemNo+FixedRows]:=Node.Name;
            Items[LastClipBoardItemNo+FixedRows]:=TLevel(Node);
            Node:=Node.Next; Inc(LastClipBoardItemNo);
            end;
          LastClipBoardItemNo:=-1; // '-1': start searching from the beginning for next free number when the next item is created
          end;

       s:=IntToStr(Max(999,RowCount))+SPACE+SPACE+SPACE;
       ColWidths[0]:=Canvas.TextWidth(s);
       ColWidths[1]:=ClientWidth-ColWidths[0]-2*GridLineWidth;
       Grid.Cells[1,0]:=VisualFileName(SokoFile.Name);
       if IsBlank(Grid.Cells[1,0]) then Grid.Cells[1,0]:='';
       Result:=True;
     except on E:Exception do Result:=Error(E.Message,Application.Title);
     end;
  if not Result then Clear;
end;

procedure TLevelSetForm.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
var s:String;
begin
  with Grid do with FormColors do
    begin s:=Cells[ACol,ARow];

          if   ARow<FixedRows then
               begin Canvas.Font .Color:=BackgroundTextColor;
                     Canvas.Brush.Color:=BackgroundColor;
               end
          else if (ARow=Row) and
                  (Screen.ActiveForm=Self)
                  //gdFocused in AState then // don't use 'Focused', because then the current row is only highlighted when the grid has focus
                  then
                  if   s='' then
                       begin Canvas.Font .Color:=BackgroundTextColor;
                             Canvas.Brush.Color:=BackgroundColor;
                       end
                  else if ACol<FixedCols then
                       begin Canvas.Font .Color:=WindowTextColor;
                             Canvas.Brush.Color:=WindowColor;
                       end
                  else begin Canvas.Font .Color:=FocusedWindowTextColor;
                             Canvas.Brush.Color:=FocusedWindowColor;
                       end
          else begin Canvas.Font .Color:=WindowTextColor;
                     Canvas.Brush.Color:=WindowColor;
               end;

          Canvas.FillRect(ARect);

          Dec(ARect.Right); Dec(ARect.Bottom);

          if s<>'' then
             ExtTextOut(Canvas.Handle, ARect.Left + 2, ARect.Top + 2, ETO_CLIPPED or
                        ETO_OPAQUE, @ARect, PChar(S), Length(S), nil);

          if ACol=0 then with Canvas do begin
             Pen.Color:=Font.Color;
             MoveTo(ARect.Right,ARect.Top);
             LineTo(ARect.Right,ARect.Bottom+1);
             end;

          if (ARow=Pred(FixedRows)) or (ARow=Pred(Rowcount)) then with Canvas do begin
             Pen.Color:=Font.Color;
             if ARow<>Pred(FixedRows) then Inc(ARect.Bottom);
             MoveTo(ARect.Left ,ARect.Bottom-1);
             LineTo(ARect.Right,ARect.Bottom-1);
             end;
{
          if gdSelected in AState then
             begin if s<>'' then begin Inc(ARect.Left); Inc(ARect.Top); end;
                   Canvas.DrawFocusRect(ARect);
             end;
}
    end;
end;

procedure TLevelSetForm.GridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect:=(ACol=1) and (ARow>=Grid.FixedRows) and (ARow<Grid.RowCount);
  if CanSelect then with Grid do begin
     InfoMemoExit(Sender);
     if (Sender<>nil) and (ARow<>oGridRow) then ShowItem(ARow);
     oGridRow:=ARow; oGridCellStr:=Cells[1,oGridRow];
     Options:=Options-[goEditing,goAlwaysShowEditor];
     end;
end;

function  TLevelSetForm.ShowItem(GridIndex:Integer):Boolean;
var s:String; Item:TLevel; n:TNode;
begin
  InfoMemoExit(Self);

  if   (GridIndex>=Grid.FixedRows) and
       (GridIndex<Grid.RowCount) and
       (Items[GridIndex]<>nil) then begin
       s:=Trim(Grid.Cells[1,GridIndex]);
       if   s='' then Item:=nil
       else Item:=Items[GridIndex];
       end
  else begin s:=''; Item:=nil;
       end;
  Result:=(s<>'') and (Item<>nil);

  if Result then PanelName.Caption:=SPACE+s
  else PanelName.Caption:='';

  InfoMemo.Lines.BeginUpdate;
  try     try    InfoMemo.Clear;
                 if Result then begin
                    n:=Item.Notes.Lines.Items;
                    while n<>nil do begin
                      InfoMemo.Lines.Add(n.Text); n:=n.Next;
                      end;
                    end;
          except on E:Exception do Result:=Error(E.Message,Application.Title);
          end;
  finally InfoMemo.Modified:=False;
          InfoMemo.SelStart:=0; InfoMemo.SelLength:=0;
          InfoMemo.Lines.EndUpdate;

          InfoLabel.Caption:='';

          EnableDisableButtons(False);
          ShowBoard(GridIndex);
  end;
end;

procedure TLevelSetForm.EditPopupMenuPopup(Sender: TObject);
var b:Boolean;
begin
  if ActiveControl<>InfoMemo then InfoMemo.SetFocus;
  MenuItemMemoUndo          .Enabled:=SendMessage(InfoMemo.Handle,EM_CANUNDO,0,0)<>0;
  b                                 :=InfoMemo.SelLength>0;
  MenuItemMemoCut           .Enabled:=b and (not InfoMemo.ReadOnly);
  MenuItemMemoCopy          .Enabled:=b;
  MenuItemMemoPaste         .Enabled:=(Clipboard.Hasformat(CF_TEXT)) and
                                      (not InfoMemo.ReadOnly);
  MenuItemMemoDelete        .Enabled:=MenuItemMemoCut.Enabled;
  MenuItemMemoSelectAll     .Enabled:=InfoMemo.Lines.Count<>0;
end;

procedure TLevelSetForm.MenuItemMemoEditClick(Sender: TObject);
var i:Integer;
begin
  if Sender<>nil then begin
     InfoMemo.ReadOnly:=not InfoMemo.ReadOnly;
     InfoMemo.SetFocus;
     end;
  MenuItemMemoEdit.Checked :=not InfoMemo.ReadOnly;
  for i:=1 to 9 do // kludge: these hard-wired numbers cover the edit-features
      EditPopupMenu.Items[i].Visible:=MenuItemMemoEdit.Checked;
end;

procedure TLevelSetForm.MenuItemMemoUndoClick(Sender: TObject);
begin
  SendMessage(InfoMemo.Handle,EM_UNDO,0,0);
end;

procedure TLevelSetForm.MenuItemMemoCutClick(Sender: TObject);
begin
  InfoMemo.CutToClipBoard;
end;

procedure TLevelSetForm.MenuItemMemoCopyClick(Sender: TObject);
begin
  InfoMemo.CopyToClipboard;
end;

procedure TLevelSetForm.MenuItemMemoPasteClick(Sender: TObject);
begin
  InfoMemo.PasteFromClipboard;
end;

procedure TLevelSetForm.MenuItemMemoDeleteClick(Sender: TObject);
begin
  InfoMemo.ClearSelection;
end;

procedure TLevelSetForm.MenuItemMemoSelectAllClick(Sender: TObject);
begin
  InfoMemo.SelectAll;
end;

procedure TLevelSetForm.MenuItemMemoPenColorClick(Sender: TObject);
begin
  ColorDialog1.Color:=InfoMemo.Font.Color;
  if ColorDialog1.Execute then
     begin InfoMemo.Font.Color:=ColorDialog1.Color;
     end;
end;

procedure TLevelSetForm.MenuItemMemoBackgroundColorClick(Sender: TObject);
begin
  ColorDialog1.Color:=InfoMemo.Color;
  if ColorDialog1.Execute then
     begin InfoMemo.Color:=ColorDialog1.Color;
     end;
end;

procedure TLevelSetForm.MenuItemMemoFontClick(Sender: TObject);
begin
  InfoMemo.HideSelection:=False;
  FontDialog1.Font.Assign(InfoMemo.Font);
  if FontDialog1.Execute then with InfoMemo do
     begin Font.Assign(FontDialog1.Font);
     end;
  InfoMemo.HideSelection:=True;
end;

procedure TLevelSetForm.PanelPasteClick(Sender: TObject);
begin
  if CloseEditor(False) and
     (Items[Grid.Row]<>nil) then begin
     MouseOverSoundEnabled:=False;
     ToolsForm.SetFocus;
     ToolsForm.EditMenuItemPasteClick(Self);
     end;
end;

procedure TLevelSetForm.PanelDeleteClick(Sender: TObject);
var i:Integer; Item:TLevel;
begin
  //MainForm.Sound.Play(Sound_.stMenuSelect);
  MouseOverSoundEnabled:=False;
  if Edit1.Visible then Edit1.Text:=oGridCellStr;
  if CloseEditor(False) then with Grid do
     if (PanelName.Caption<>'') and (Trim(PanelName.Caption)=Cells[1,Row]) and
        (SokoFile<>nil) and (SokoFile.Levels<>nil) then begin
        Item:=Items[Row]; SokoFile.Modified:=True;
        SokoFile.Levels.Remove(Item,True);
        Cells[1,Row]:=''; Items[Row]:=nil;
        for i:=Row to RowCount-2 do begin
            Cells     [1,i]:=Cells[1,Succ(i)];
            Items[i]       :=Items[Succ(i)];
            end;
        Cells[1,Pred(RowCount)]:=''; Items[Pred(RowCount)]:=nil;
        if   RowCount>Succ(FixedRows) then
             RowCount:=RowCount-1;
        if   RowCount>FixedRows then
             ShowItem(Grid.Row)
        else ShowItem(0);
        LastClipboardItemNo:=-1;
        if   Cells[1,Grid.Row]='' then ToolsForm.SetFocus; // empty grid: keyboard commands should go to 'ToolsForm'
        end;
end;

procedure TLevelSetForm.PanelNewClick(Sender: TObject);
var i,NewRowCount,oRowCount:Integer; Result:Boolean; s:String; Item:TLevel;
begin
  if PanelNew.Enabled then begin
//   if Sender<>MainForm.BtnSetItems then // if 'New was triggered by the button, the 'select menu' sound is playing
//      MainForm.Sound.Play(Sound_.stMenuSelect);
     MouseOverSoundEnabled:=False;
     if CloseEditor(False) and (SokoFile<>nil) then
        with Grid do begin

          oRowCount:=RowCount; Item:=nil;
          try    Item:=TLevel.Create;
                 if   (RowCount=Succ(FixedRows)) and
                      (Cells[1,Pred(RowCount)]='') then // last item (= first item) is empty
                      NewRowCount:=RowCount
                 else NewRowCount:=Succ(RowCount);

                 s:=SokoFile.Levels.MakeUniqueName1('',StandardClipboardItemName,False,LastClipBoardItemNo);
                 Result:=(s<>'') and Item.SetName(s);
                 if Result then begin
                    if   ToolsForm.Editor.Selection.HasBoard then
                         Result:=Item.BoardAsTextLines.LoadFromText(ToolsForm.Editor.Selection.BoardAsText)
                    else with ToolsForm do with Editor do with CursorRect do
                           if (Cursor=ctSelection) and
                              Editor.Selection.Enabled and
                              ((Left<Pred(Right)) or (Top<Pred(Bottom))) then
                              if   BoardToText(Game.BoardWidth,Game.BoardHeight,Game.Board,CellRectToGameBoardRect(CursorRect),s,i) then
                                   Result:=Item.BoardAsTextLines.LoadFromText(s)
                              else Result:=False
                           else with ToolsForm.Game do Result:=Item.BoardToTextLines(BoardWidth,BoardHeight,Board);
                    if Result then begin
                       if NewRowCount<>RowCount then RowCount:=NewRowCount;
                       SetGridColumnsWidth;
                       Cells     [0,Pred(RowCount)]:=IntToStr(RowCount-FixedRows);
                       Cells     [1,Pred(RowCount)]:=Item.Name;
                       Items[Pred(RowCount)  ]:=Item;
                       SokoFile.Levels.Add(Item); SokoFile.Modified:=True;
                       if   Row =Pred(RowCount) then ShowItem(Row)
                       else Row:=Pred(RowCount); // 'GridSelectCell' calls 'ShowSetItem'
                       end;
                    end
                 else raise Exception.Create(TEXT_TASK_FAILED);
          except on E:Exception do begin
                    SokoFile.Levels.Remove(Item,True); RowCount:=oRowCount;
                    Error(E.Message,Application.Title);
                    end;
          end;
          end;
     end;
end;

procedure TLevelSetForm.PanelRenameClick(Sender: TObject);
begin
//MainForm.Sound.Play(Sound_.stMenuSelect);
  MouseOverSoundEnabled:=False;
  if Edit1.Visible then CloseEditor(False)
  else if PanelName.Caption<>'' then with Grid do begin
          SetFocus; ShowEditor;
          end;
end;

procedure TLevelSetForm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if      Key=VK_RETURN then
          if Shift=[] then PanelPasteClick(Sender)
          else
  else if Key=VK_HOME   then
          Grid.Row:=Grid.FixedRows
  else if Key=VK_END    then
          Grid.Row:=Pred(Grid.RowCount)
  else if Key=Ord(ACCEL_CHAR_EXIT) then
          Close
  else if Key=VK_F1 then// MainForm.BtnHelpClick(Sender)
  else;
end;

procedure TLevelSetForm.GridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if      Key=VK_RETURN then
          if ssCtrl in Shift then PanelRenameClick(Sender)
          else
  else if ((Key=VK_LEFT) or (Key=VK_TAB)) and
          (Shift=[]) then //MainForm.SetFocus
  else if (Key=VK_RIGHT) and (Shift=[]) then Close
  else;
end;

procedure TLevelSetForm.InfoMemoEnter(Sender: TObject);
begin //
end;

procedure TLevelSetForm.InfoMemoExit(Sender: TObject);
begin
  if InfoMemo.Modified and // if text is modified then save it now
     (Items[Grid.Row]<>nil) then with Items[Grid.Row] do
     if Notes<>nil then begin
        InfoMemo.Modified:=False; Notes.Modified:=True;
        StringsToList(InfoMemo.Lines,Notes.Lines);
        if SokoFile<>nil then SokoFile.Modified:=True;
        end;
end;

procedure TLevelSetForm.ShowEditor;
begin
  GridScrollInView(Grid,Grid.Row);
  HideEditor;
  with Edit1 do begin
    Text :=Grid.Cells[1,Grid.Row];
    Top  :=Grid.Top+
           (Grid.Row-Grid.TopRow+Grid.FixedRows)*(Grid.DefaultRowHeight+Grid.GridLineWidth);
    Width:=Grid.ColWidths[1];
    Show; Edit1.SetFocus;
    end;
end;

function  TLevelSetForm.HideEditor:Boolean;
begin
  Result:=Edit1.Visible;
  Edit1.Hide;
end;

function  TLevelSetForm.CloseEditor(HideFirst:Boolean):Boolean;
begin
  Result:=True;
  if Edit1.Visible then begin
     if HideFirst then Edit1.Hide;
     Result:=RenameItem(Grid.Row,StrRemoveCharacters(Trim(Edit1.Text),TITLE_ILLEGAL_CHARACTERS));
     end;
  if Result then HideEditor;
end;

function  TLevelSetForm.RenameItem(GridIndex:Integer; const NewName:String):Boolean;
var CanSelect:Boolean; s,oName:String; Item:TLevel;
begin
  Result:=False; Item:=Items[GridIndex];
  if Item<>nil then begin
     oName:=Item.Name;
     s:=TextThatCannotBeInterpretedAsMoves(StrRemoveCharacters(Trim(NewName),TITLE_ILLEGAL_CHARACTERS));
     if      s=oName then Result:=True
     else if s='' then s:=TEXT_NAME_CANNOT_BE_BLANK_2
     else if (StrEqual(s,Item.Name)
              or
              (SokoFile.Levels.GetItemByName(s)=nil)
             )
             and
             SokoFile.Levels.RenameItem(oName,s) then
             Result:=True
     else    s:=Format(TEXT_NAME_ALREADY_EXISTS_FORMAT,[s]);

     if Result then with Grid do begin
        if oName<>Item.Name then SokoFile.Modified:=True;
        Cells[1,GridIndex]:=Item.Name;
        PanelName.Caption:=Cells[1,GridIndex];
        GridSelectCell(Self,Col,GridIndex,CanSelect);
        end
     else with Grid do begin
        Application.MessageBox(PChar(s),PChar(Caption),MB_OK+MB_ICONERROR);
        Edit1.Text:=Cells[1,GridIndex]; Edit1.SelLength:=Length(Edit1.Text);
        Edit1.SetFocus;
        end;
     end
  else Result:=False;
end;

procedure TLevelSetForm.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key in [{VK_RETURN,}VK_UP,VK_DOWN{,VK_HOME,VK_END}]) and
     RenameItem(Grid.Row,Edit1.Text) then begin
     Grid.SetFocus;
     if Key<>VK_RETURN then PostMessage(Grid.Handle, WM_KEYDOWN, Key, 0);
     Key:=0;
     end;
end;

procedure TLevelSetForm.Edit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key in [VK_RETURN{,VK_UP,VK_DOWN{,VK_HOME,VK_END}]) and
     RenameItem(Grid.Row,Edit1.Text) then begin
     Grid.SetFocus;
     if Key<>VK_RETURN then PostMessage(Grid.Handle, WM_KEYDOWN, Key, 0);
     Key:=0;
     end
  else if Key=VK_ESCAPE then
          if Trim(Edit1.Text)=Trim(Grid.Cells[1,Grid.Row]) then begin
             Key:=VK_RETURN; Edit1KeyUp(Sender,Key,Shift);
             end
          else begin
            Edit1.Text:=Grid.Cells[1,Grid.Row];
            Edit1.SelLength:=Length(Edit1.Text);
            end;
end;

procedure TLevelSetForm.Edit1Enter(Sender: TObject);
begin //
  FormMouseMove(ActiveControl,[],0,0);
end;

procedure TLevelSetForm.Edit1Exit(Sender: TObject);
begin
  CloseEditor(False);
  FormMouseMove(ActiveControl,[],0,0);
end;

procedure TLevelSetForm.GameViewerCellToPos(Col__,Row__:Integer; var X__,Y__:Integer); // 0-based columns, rows
begin
  with GameViewer do begin
    X__:=CellsPixelOrigin.X+Col__*ColWidth;
    Y__:=CellsPixelOrigin.Y+Row__*RowHeight;
    end;
end;

procedure TLevelSetForm.GameViewerMouseToCell(X__,Y__:Integer; var Col__,Row__:Integer); // 0-based columns, rows
begin
  with GameViewer do begin
    Col__:=(X__-CellsPixelOrigin.X) div Max(1,ColWidth );
    Row__:=(Y__-CellsPixelOrigin.Y) div Max(1,RowHeight);
    end;
end;

procedure TLevelSetForm.GridTopLeftChanged(Sender: TObject);
begin
  CloseEditor(True);
end;

procedure TLevelSetForm.GridEnter(Sender: TObject);
begin
  GridScrollInView(Grid,Grid.Row);
end;

procedure TLevelSetForm.GridPopupMenuPopup(Sender: TObject);
begin
  MenuItemGridPaste       .Enabled:=PanelName.Caption<>'';
  MenuItemGridRename      .Enabled:=MenuItemGridPaste.Enabled;
  MenuItemGridDelete      .Enabled:=MenuItemGridPaste.Enabled;
  MenuItemGridDeleteAll   .Enabled:=MenuItemGridPaste.Enabled;
  MenuItemGridSortOnNames .Enabled:=Grid.RowCount>Succ(Grid.FixedRows);
  MenuItemGridCopyToWindowsClipboard.Enabled:=MenuItemGridPaste.Enabled and (Grid.RowCount>Grid.FixedRows) and Assigned(Items[Grid.Row]);
  MenuItemGridPasteFromWindowsClipboard.Enabled:=Clipboard.HasFormat(CF_TEXT) and Assigned(MainForm.GamePictures) and MainForm.GamePictures.Initialized;
end;

procedure TLevelSetForm.MenuItemGridDeleteAllClick(Sender: TObject);
begin
  if not SokoFile.Levels.IsEmpty then begin
     SokoFile.Levels.Clear;
     SokoFile.Modified:=True;
     end;
  Clear;
  ToolsForm.SetFocus; // empty grid: keyboard commands should go to 'ToolsForm'
end;

procedure TLevelSetForm.GridDragDrop(Sender, Source: TObject; X, Y: Integer);
var i,ACol,ARow:Integer; s:String; Item:TLevel;
begin
//if  Source=ToolsForm.EditImage1 then begin
//    EndDrag(True);
//    PanelNewClick(Self);
//    end
//else
      if (Sender=Grid) and (Source=Grid) then with Grid do begin
         MouseToCell(X,Y,ACol,ARow);
         if (ACol=1) and (ARow>=FixedRows) and
            (ARow<RowCount) and (ARow<>Row) and
            (SokoFile<>nil) and (SokoFile.Levels<>nil) then begin
            EndDrag(True);
            s:=Cells[1,Row]; Item:=Items[Row];
            if   ARow<Row then
                 for i:=Row downto Succ(ARow) do begin
                     Cells[1,i]   :=Cells[1,Pred(i)];
                     Items[i]     :=Items[Pred(i)];
                     end
            else for i:=Row to     Pred(ARow) do begin
                     Cells[1,i]   :=Cells[1,Succ(i)];
                     Items[i]     :=Items[Succ(i)];
                     end;
            Cells[1,ARow]:=s; Items[ARow]:=Item;
            GridScrollInView(Grid,ARow);

            if   ARow=FixedRows then
                 SokoFile.Levels.MoveAfter(Item,nil)
            else SokoFile.Levels.MoveAfter(Item,Items[Pred(ARow)]);

            Row:=ARow;

            SokoFile.Modified:=True;
            end
         else EndDrag(False);
         end
      else if Sender is TControl then
              TControl(Sender).EndDrag(False); // unknown sender/source
end;

procedure TLevelSetForm.GridDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var ACol,ARow:Integer;
begin
  if   (Sender=Grid) and (Source=Grid) then with Grid do begin
       MouseToCell(X,Y,ACol,ARow);
       if (ACol>=FixedCols) and (ARow>=FixedRows) then Accept:=True
       else Accept:=False;
       if (ARow=TopRow) and (TopRow>FixedRows) then TopRow:=TopRow-1
       else if (ARow=(TopRow+VisibleRowCount-FixedRows)) and
               (TopRow+VisibleRowCount-FixedRows<RowCount-FixedRows) then
               TopRow:=TopRow+1;
       end
  else;// with MainForm do Accept:=(Source=DrawGrid1) and (TrackBox.X=0) and (TrackBox.Y=0);
end;

procedure TLevelSetForm.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var ACol,ARow:Integer;
begin
  with Grid do begin
    MouseToCell(X, Y, ACol, ARow);
    if (ACol>=FixedCols) and (ARow>=FixedRows) and
       (ACol<ColCount) and (ARow<RowCount) then
       if      (Button=mbLeft) and (not Dragging) and
               (Cells[1,ARow]<>'') then
               BeginDrag(False)
       else if (Button=mbRight) and (ARow<>Row) then
               Row:=ARow;
    end;
end;

procedure TLevelSetForm.GridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  FormMouseMove(Sender,Shift,X,Y);
end;

procedure TLevelSetForm.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with Grid               do if Dragging then EndDrag(False);
//with MainForm.DrawGrid1 do if Dragging then EndDrag(False);
end;

function  TLevelSetForm.GetItem(GridIndex:Integer):TLevel;
begin
  Result:=TLevel(Grid.Objects[1,GridIndex]);
end;

procedure TLevelSetForm.SetItem(GridIndex:Integer; Item:TLevel);
begin
  Grid.Objects[1,GridIndex]:=TObject(Item);
end;

procedure TLevelSetForm.SetGridColumnsWidth;
var i:Integer; s:String;
begin
  with Grid do begin
    s:=IntToStr(Max(999,RowCount))+SPACE+SPACE+SPACE;
    i:=Canvas.TextWidth(s);
    if i<>ColWidths[0] then ColWidths[0]:=i;
    i:=ClientWidth-ColWidths[0]-2*GridLineWidth;
    if i<>ColWidths[1] then ColWidths[1]:=i;
    end;
end;

function  SortOnNames(a,b:Pointer):Integer;
begin
  Result:=AnsiCompareText(TLevel(PPointer(a)^).Text,TLevel(PPointer(b)^).Text);
end;

procedure TLevelSetForm.MenuItemGridSortClick(Sender: TObject);
var i,Count:Integer; Node:TNode; Item:TLevel; SortFun:TCompareFunction;
    Vector:PPNodeVector;
begin
  SortFun:=SortOnNames;
  if (SokoFile<>nil) and
     (SokoFile.Levels<>nil) and (SokoFile.Levels.Count>1) then begin
     Item:=Items[Grid.Row];
     if SokoFile.Levels.SaveToVector(Count,Vector) then
        try     QuickSort(Vector,Count,SizeOf(Vector^[Low(Vector^)]),SortFun);
        finally SokoFile.Levels.LoadFromVector(Count,Vector);
        end;

     i:=Grid.FixedRows;
     Node:=SokoFile.Levels.First;
     while (Node<>nil) and (i<Grid.RowCount) and
           StrEqual(Node.Name,Items[i].Name) do begin
           Inc(i); Node:=Node.Next;
           end;
     if i<Grid.RowCount then begin // new sorting order
        SokoFile.Modified:=True;
        LoadItems; // load the items in the new sorting order
        end;

     Grid.Row:=Grid.FixedRows+SokoFile.Levels.IndexOf(Item);
     ShowItem(Grid.Row);
     end;
end;

procedure TLevelSetForm.ImageBoardMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PanelPasteClick(Sender);
end;

procedure TLevelSetForm.ShowBoard(GridIndex:Integer);
var i,BoardWidth,BoardHeight:Integer; s:String;
    PlayerPos:TColRow; Board:TBoard; Item:TLevel;
begin
  Item:=Items[GridIndex]; BoardWidth:=0; BoardHeight:=0;
  if      Item<>nil then begin
          Item.TextLinesToBoard(Board,BoardWidth,BoardHeight,i,i,PlayerPos,s);
          TrimBoard(0,0,Board,BoardWidth,BoardHeight,PlayerPos);
          end;
  if      (Item=nil) or (SokoFile=nil) or (not Initialized) then
          ShowBoard0(0,0,Board,ImageBoard,False)
  else if Visible then
          ShowBoard0(BoardWidth,BoardHeight,Board,ImageBoard,False);
  if GridIndex>=Grid.FixedRows then GridScrollInView(Grid,GridIndex);
end;

procedure TLevelSetForm.ShowBoard0(BoardWidth__,BoardHeight__:Integer; const Board__:TBoard; BoardImage__:TImage; Disabled__:Boolean);
var i,j,Col,Row,X,Y:Integer; MarkedSquare,MarkedSquares:Boolean; R0,R1,R2:TRect; B:TBitMap;
begin // ShowBoard0
  if      Assigned(BoardImage__) then
          InitializeGameViewer(BoardWidth__,BoardHeight__,BoardImage__);

  if      (not ToolsForm.Initialized) or
          (BoardWidth__=0) or
          (BoardHeight__=0) then with GameViewer do begin
          R1:=Rect(0,0,BackgroundPict.BitMap.Width,BackgroundPict.BitMap.Height);
          if      BackgroundInitialized and
                  BackgroundPict.Visible then with BoardImage.Picture.BitMap.Canvas do begin
                  CopyMode:=cmSrcCopy;
                  CopyRect(R1,BackgroundPict.BitMap.Canvas,R1);
                  if not BoardPanel.Visible then BoardPanel.Show;
                  end
          else if Assigned(BoardImage) then with BoardImage.Picture.BitMap do with Canvas do begin
                  Brush.Style:=bsSolid; Brush.Color:=BackgroundPict.Color;
                  FillRect(Rect(0,0,Width,Height));
                  BoardPanel.Hide;
                  end;
          end
  else    if Assigned(GameViewer.BoardImage) and
             MakeStaticWorkBitMap(GameViewer.ColWidth,GameViewer.RowHeight,True) then with GameViewer do with BoardImage.Picture.BitMap.Canvas do begin
             if not BoardPanel.Visible then BoardPanel.Show;
             if BackgroundInitialized and
                BackgroundPict.Visible then with BoardImage.Picture.BitMap.Canvas do begin
                R1:=Rect(0,0,BackgroundPict.BitMap.Width,BackgroundPict.BitMap.Height);
                CopyMode:=cmSrcCopy;
                CopyRect(R1,BackgroundPict.BitMap.Canvas,R1);
                end
             else with BoardImage.Picture.BitMap do with Canvas do begin
                Brush.Style:=bsSolid; Brush.Color:=BackgroundPict.Color;
                FillRect(Rect(0,0,Width,Height));
                end;

             R0:=Classes.Rect(0,0,ColWidth,RowHeight);
             R2:=Classes.Rect(0,0,BoardImage.Picture.BitMap.Width,BoardImage.Picture.BitMap.Height);
             MarkedSquares:=False;
             for Col:=0 to ColCount do
                 for Row:=0 to RowCount do
                     if (Board__[Col,Row] and SQUARE_SET)<>0 then begin
                        MarkedSquares:=True;
                        break; // quick-and-dirty exit loop when a marked square has been found
                        end;

             for Col:=0 to Pred(ColCount) do
                 for Row:=0 to Pred(RowCount) do begin
                     GameViewerCellToPos(Col,Row,X,Y); R1:=Classes.Rect(X,Y,X+ColWidth,Y+RowHeight);
                     if Misc_.ClipRect(R1,R2) then begin
                        {
                        if BackgroundInitialized then begin
                           CopyMode:=cmSrcCopy;
                           CopyRect(R,BackgroundPict.BitMap.Canvas,R);
                           end
                        else begin
                           Brush.Style:=bsSolid; Brush.Color:=BackgroundPict.Color;
                           FillRect(R);
                           end;
                        }
                        if FloorTilesVisible then
                           SkinPict.DrawRect(R1.Left,R1.Top,SkinPict.FrameRect(TILE_INDEX_FLOOR),BoardImage.Picture.BitMap.Canvas);

                        i:=Board__[Succ(Col),Succ(Row)];
                        j:=-1;
                        if        (i and WALL)<>0 then j:=TILE_INDEX_WALL
                        else if   (i and GOAL)<>0 then
                                  if        (i and BOX   )<>0 then j:=TILE_INDEX_BOX_ON_GOAL
                                  else if   (i and PLAYER)<>0 then j:=TILE_INDEX_PLAYER_ON_GOAL
                                       else j:=TILE_INDEX_GOAL
                        else if   (i and BOX)<>0 then j:=TILE_INDEX_BOX
                             else if (i and PLAYER)<>0 then j:=TILE_INDEX_PLAYER;
                        MarkedSquare:=(i and (FLOOR+SQUARE_SET))=(FLOOR+SQUARE_SET);

                        if (j>=0) or MarkedSquare or (MarkedSquares and ((i and FLOOR)<>0)) then begin
                           if ((i and FLOOR)=0) or ((not MarkedSquare) and (not MarkedSquares)) then begin
                              if (i and GOAL)<>0 then
                                 SkinPict.DrawRect(R1.Left,R1.Top,SkinPict.FrameRect(TILE_INDEX_GOAL),BoardImage.Picture.BitMap.Canvas);
                              SkinPict.DrawRect(R1.Left,R1.Top,SkinPict.FrameRect(j),BoardImage.Picture.BitMap.Canvas);
                              end
                           else begin
                              StaticWorkBitMap.Canvas.CopyMode:=cmSrcCopy;
                              StaticWorkBitMap.Canvas.CopyRect(R0,BoardImage.Picture.BitMap.Canvas,R1);
                              if FloorTilesVisible then
                                 SkinPict.DrawRect(0,0,SkinPict.FrameRect(TILE_INDEX_FLOOR),StaticWorkBitMap.Canvas);
                              if (i and GOAL)<>0 then
                                 SkinPict.DrawRect(0,0,SkinPict.FrameRect(TILE_INDEX_GOAL),StaticWorkBitMap.Canvas);
                              if j>=0 then
                                 SkinPict.DrawRect(0,0,SkinPict.FrameRect(j),StaticWorkBitMap.Canvas);
                              if   MarkedSquare then
                                   BitMapAlphaBlendColor(StaticWorkBitMap,ToolsForm.GameViewer.SquareSetSelectedSquaresColor   ,ToolsForm.GameViewer.SquareSetTransparencyPct,R0)
                              else BitMapAlphaBlendColor(StaticWorkBitMap,ToolsForm.GameViewer.SquareSetNotSelectedSquaresColor,ToolsForm.GameViewer.SquareSetTransparencyPct,R0);
                              BoardImage.Picture.BitMap.Canvas.CopyMode:=cmSrcCopy;
                              BoardImage.Picture.BitMap.Canvas.CopyRect(R1,StaticWorkBitMap.Canvas,R0);
                              end;
                           end;
                        end;
                     end;

             if Disabled__ then with BoardImage.Picture do with BitMap do with Canvas do
                if  BitMapCreate(B,Width,Height) then
                    try     R1:=Rect(0,0,Width,Height);
                            B.Canvas.CopyMode:=cmSrcCopy;
                            B.Canvas.CopyRect(R1,Canvas,R1);
                            BitMapAlphaBlendColor(B,DISABLED_ALPHA_BLEND_COLOR,DISABLED_ALPHA_BLEND_PCT,R1);
                            CopyMode:=cmSrcCopy;
                            CopyRect(R1,B.Canvas,R1);
                    finally B.Free;
                    end;
             end;
end;

procedure TLevelSetForm.ShowBoard1(BoardWidth__,BoardHeight__:Integer; const BoardAsText__,SelectedSquaresAsText__:String; BoardImage__:TImage);
var Col,Row,Index:Integer; Board:TBoard;
begin
  if BoardWidth__*BoardHeight__=Length(BoardAsText__) then begin
     Index:=0;
     for Row:=1 to BoardHeight__ do
         for Col:=1 to BoardWidth__ do begin
             Inc(Index);
             Board[Col,Row]:=CharToBoardSquareValue(BoardAsText__[Index]);
             end;
     SelectedSquaresAsTextToBoard(SelectedSquaresAsText__,BoardWidth__,BoardHeight__,Board);
     ShowBoard0(BoardWidth__,BoardHeight__,Board,BoardImage__,False);
     end;
end;

function TLevelSetForm.InitializeGameViewer(BoardWidth__,BoardHeight__:Integer; BoardImage__:TImage):Boolean;
var H,W:Integer;
begin
  with GameViewer do begin
    Result:=True;

    if BoardImage__<>BoardImage then begin
       BoardImage:=BoardImage__;
       BackgroundInitialized:=False;
       ColCount:=-1; RowCount:=-1; SkinInitialized:=False;
       if BoardImage.Parent is TPanel then BoardPanel:=TPanel(BoardImage.Parent)
       else begin
          BoardPanel:=nil;
          raise Exception.Create(InternalErrorText+': The board image must be embedded in a panel');
          end;
       end;

    if (BackgroundPict<>nil) and (BackgroundPict.OrgBitMap=nil) and
       (ToolsForm.GameViewer.BackgroundPict<>nil) then with BackgroundPict do begin
       OrgBitMap:=ToolsForm.GameViewer.BackgroundPict.OrgBitMap;
       AntiAliasing:=ToolsForm.GameViewer.BackgroundPict.AntiAliasing;
       Color:=ToolsForm.GameViewer.BackgroundPict.Color;
       View:=ToolsForm.GameViewer.BackgroundPict.View;
       Visible:=ToolsForm.GameViewer.BackgroundPict.Visible;
       BackgroundInitialized:=False;
       end;

    if (SkinPict<>nil) and (SkinPict.OrgBitMap=nil) and
       (ToolsForm.GameViewer.SkinPict<>nil) and
       (ToolsForm.GameViewer.SkinPict.FrameCount>0) then with SkinPict do begin
       OrgBitMap:=ToolsForm.GameViewer.SkinPict.OrgBitMap;
       Antialiasing:=ToolsForm.GameViewer.SkinPict.AntiAliasing;
       Masked:=ToolsForm.GameViewer.SkinPict.Masked;
       MaskBitMapColor:=ToolsForm.GameViewer.SkinPict.MaskBitMapColor;
       MaskBitMapPct:=ToolsForm.GameViewer.SkinPict.MaskBitMapPct;
       FrameCount:=ToolsForm.GameViewer.SkinPict.FrameCount;
       FloorTilesVisible:=ToolsForm.GameViewer.FloorTilesVisible;
       ColCount:=-1; RowCount:=-1; SkinInitialized:=False;
       end;

    W:=BoardImage.ClientWidth; H:=BoardImage.ClientHeight;
     if (W<>BoardImage.Picture.BitMap.Width) or
        (H<>BoardImage.Picture.BitMap.Height) then with BoardImage.Picture.BitMap do
        try    Width:=W; Height:=H;
        except on E:Exception do Result:=Error(E.Message,Application.Title);
        end;

    Result:=Result and
            (W=BoardImage.Picture.BitMap.Width) and
            (H=BoardImage.Picture.BitMap.Height) and
            (BackgroundPict<>nil) and (BackgroundPict.OrgBitMap<>nil);

    if Result then with BackgroundPict do begin
       BackgroundInitialized:=(BackgroundInitialized
                               and
                               (BitMap<>nil)
                               and
                               (BoardImage.Picture.BitMap.Width =BitMap.Width)
                               and
                               (BoardImage.Picture.BitMap.Height=BitMap.Height)
                              )
                              or
                              SetView(View,BoardImage.Picture.BitMap.Width,BoardImage.Picture.BitMap.Height,BackgroundPict.Color);
       Result:=BackgroundInitialized;
       end
    else
       BackgroundInitialized:=False;

    if (SkinPict<>nil) and
       (SkinPict.OrgBitMap<>nil) and
       (SkinPict.FrameCount>0) then
       SkinInitialized:=SkinInitialized and
                        (BoardWidth__ =ColCount) and
                        (BoardHeight__=RowCount)
    else
       Result:=False;

    if Result then begin
       if (not SkinInitialized) then with SkinPict do begin
          ColCount:=BoardWidth__; RowCount:=BoardHeight__; // game parameters

          ColWidth:=Min(MAX_SQUARE_SIZE,Min(W div Max(1,ColCount),H div Max(1,RowCount)));
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

          CellsPixelOrigin.X:=(W - (ColCount*ColWidth )) div 2;
          CellsPixelOrigin.Y:=(H - (RowCount*RowHeight)) div 2;

          SkinInitialized:=SkinPict.ResizeFrames(ColWidth,RowHeight,SkinPict.FrameCount,nil);
          Result:=SkinInitialized;
          end;
       end
    else
       SkinInitialized:=False;
    end;
end;

function TLevelSetForm.EnableOrDisableNewItem:Boolean;
begin
  with ToolsForm.Editor do with CursorRect do
//  if             MouseButtonDown then
//                 Result:=False
//  else
         if        Selection.HasBoard then
                   Result:=Selection.NonFloorCellCount<>0
         else if   (Cursor=ctSelection) and Selection.Enabled then
                   Result:=((Left<Pred(Right))
                            or
                            (Top <Pred(Bottom))
                           )
                           and
                           (not ToolsForm.IsAnEmptyRect(ToolsForm.CellRectToGameBoardRect(CursorRect)))
              else Result:=(ToolsForm.Game.BoardWidth<>0) and (ToolsForm.Game.BoardHeight<>0);

  if Result<>PanelNew.Enabled then begin
     PanelNew.Enabled:=Result;
     EnableDisableButtons(True);
     end;
end;

procedure TLevelSetForm.FormHide(Sender: TObject);
begin
  if SokoFile<>nil then SokoFile.Flush;
end;

procedure TLevelSetForm.ShowMove(const FromPos,ToPos,BoxStartPos:TPoint; BoxNo,TimeMS,MoveCount,PushCount:Integer; Undo,Jump,LastMove:Boolean);
var i,j,k,x,y,
    HalfColWidth,HalfRowHeight,
    MoveCountSign,oWindowResizeCount,oAnimateReplayMovesMS:Integer;
    oIsBusy,DoMove:Boolean; Direction:TDirection;
    Time,StartTime,StopTime,PrevTime:TTimeMS;
    CurrentCell,NewCell,CurrentPlayerPos:TPoint;
    t:TToolsFormSprite;
    Sprites:array[0..1] of TToolsFormSprite; // box-sprite and player-sprite sorted according to the task
    //SoundType:TSoundType; //q:Double;
begin // The board must be updated BEFORE calling this procedure,
      // but all parameters ('MoveCount' etc.) must reflect the state before performing the move
  // note that the replay feature uses 'OpenForm.Game', not 'ToolsForm.Game'
  oIsBusy:=OpenForm.Game.IsBusy;
//try
  with GameViewer do begin
    OpenForm.Game.IsBusy:=True;
    oWindowResizeCount:=ToolsForm.FormResizeCount; oAnimateReplayMovesMS:=MainForm.Game.AnimateReplayMovesMS;
    HalfColWidth:=ColWidth div 2; HalfRowHeight:=RowHeight div 2;
    //ClearFPS;

    if   not Undo then CurrentPlayerPos:=FromPos
    else CurrentPlayerPos:=ToPos;

    if TimeMS<1 then TimeMS:=1;

    x:=ToPos.x-FromPos.x;
    y:=ToPos.y-FromPos.y;

    if (BoxNo<=0) or (not Assigned(BoxSprite)) then begin
       BoxNo:=-1; //SoundType:=stMove;
       end
    else with BoxSprite do begin
      if ItemIndex<>BoxNo then with StartPosition do begin // ensure that the box sprite has the correct position and background
         ItemIndex:=BoxNo;
         LevelSetForm.GameViewerCellToPos(Pred(BoxStartPos.X),Pred(BoxStartPos.Y),X,Y);
         R:=Rect(X,Y,X+PictureSizeRect.Right,Y+PictureSizeRect.Bottom);
         SaveBackground(R,(OpenForm.Game.Board[BoxStartPos.X,BoxStartPos.Y] and GOAL)<>0);
         SetVisibleStateWithoutUpdatingTheDisplay(True); // physically, the box    is visible even though the sprite hasn't been shown yet, hence set 'Visible' accordingly
         end;
      // calculate [x,y] = box position 1 square away from the destination
      if not OpenForm.Game.ReverseMode then begin // normal game mode
         if Undo then begin
            if x=0 then x:=FromPos.x else x:=FromPos.x+(x div Abs(x));
            if y=0 then y:=FromPos.y else y:=FromPos.y+(y div Abs(y));
            end
         else begin
            if x=0 then x:=FromPos.x else x:=ToPos  .x+(x div Abs(x));
            if y=0 then y:=FromPos.y else y:=ToPos  .y+(y div Abs(y));
            end;
         end
      else begin // reverse mode
         if Undo then begin
            if x=0 then x:=FromPos.x else x:=FromPos.x-(x div Abs(x));
            if y=0 then y:=FromPos.y else y:=FromPos.y-(y div Abs(y));
            end
         else begin
            if x=0 then x:=FromPos.x else x:=ToPos  .x-(x div Abs(x));
            if y=0 then y:=FromPos.y else y:=ToPos  .y-(y div Abs(y));
            end;
         end;

      // caution: whether the box moves to a goal-square or a non-goal square
      // is calculated only once per call and not for each move,
      // hence, mixing is not allowed
      if   (OpenForm.Game.Board[x,y] and OpenForm.Game.BoxTargetMaskForDisplay)=0 then
           begin FrameIndex:=TILE_INDEX_BOX; {SoundType:=stPush;} end
      else begin FrameIndex:=TILE_INDEX_BOX_ON_GOAL; {SoundType:=stGoal;} end;

      StartPosition.x:=R.Left; StartPosition.y:=R.Top;
      LevelSetForm.GameViewerCellToPos(Pred(x),Pred(y),StopPosition.x,StopPosition.y);
      dx:=(StopPosition.x-StartPosition.x)  / TimeMS; // calculate velocities for x-axis and y-axis
      dy:=(StopPosition.y-StartPosition.y ) / TimeMS;
      end;

    if Assigned(PlayerSprite) then with PlayerSprite do begin
       if ItemIndex<>0 then with StartPosition do begin // player
          ItemIndex:=0;
          LevelSetForm.GameViewerCellToPos(Pred(CurrentPlayerPos.X),Pred(CurrentPlayerPos.Y),X,Y);
          R:=Rect(X,Y,X+PictureSizeRect.Right,Y+PictureSizeRect.Bottom);
          SaveBackground(R,(OpenForm.Game.Board[CurrentPlayerPos.X,CurrentPlayerPos.Y] and GOAL)<>0);
          SetVisibleStateWithoutUpdatingTheDisplay(True); // physically, the player is visible even though the sprite hasn't been shown yet, hence set 'Visible' accordingly
          end;
       with CurrentPlayerPos do
         if   (OpenForm.Game.Board[x,y] and OpenForm.Game.BoxTargetMaskForDisplay)=0 then
              FrameIndex:=TILE_INDEX_PLAYER
         else FrameIndex:=TILE_INDEX_PLAYER_ON_GOAL;

       StartPosition.x:=R.Left; StartPosition.y:=R.Top;

       if   Undo then begin // show player before box
            LevelSetForm.GameViewerCellToPos(Pred(FromPos.x),Pred(FromPos.y),StopPosition.x,StopPosition.y);
            Sprites[0]:=PlayerSprite;
            if   BoxNo<=0 then
                 Sprites[1]:=nil
            else Sprites[1]:=BoxSprite;
            //SoundType:=stUndo;
            MoveCountSign:=-1;
            end
       else begin // show box before player
            LevelSetForm.GameViewerCellToPos(Pred(ToPos  .x),Pred(ToPos  .y),StopPosition.x,StopPosition.y);
            if   BoxNo<=0 then
                 Sprites[0]:=nil
            else Sprites[0]:=BoxSprite;
            Sprites[1]:=PlayerSprite;
            MoveCountSign:=1;
            end;
       dx:=(StopPosition.x-StartPosition.x) / TimeMS;
       dy:=(StopPosition.y-StartPosition.y) / TimeMS;

       CurrentCell.x:=(StartPosition.x+HalfColWidth ) div ColWidth;
       CurrentCell.y:=(StartPosition.y+HalfRowHeight) div RowHeight;

       if OpenForm.Game.ReverseMode then begin // reverse mode: the sprites must be shown in opposite order
          t:=Sprites[0]; Sprites[0]:=Sprites[1]; Sprites[1]:=t;
          if (not Undo) and Jump then; // SoundType:=stJump;
          end;

       repeat StartTime:=GetTickCount;
              StopTime :=StartTime+TTimeMS(TimeMS);
       until  StopTime >=StartTime; // primitive clock wrap-around control

       Time:=StartTime; PrevTime:=High(PrevTime);
       while (Time>=StartTime) and (Time<=StopTime) and (oWindowResizeCount=ToolsForm.FormResizeCount) do begin
         Dec(Time,StartTime);
         if Time<>PrevTime then with PlayerSprite do begin

            x:=StartPosition.x+Trunc(dx*Time)-R.Left;
            y:=StartPosition.y+Trunc(dy*Time)-R.Top;

            //q:=Time/TimeMS;
            //q:=q*q*(3-2*q);
            //x:=StartPosition.x+Trunc((StopPosition.x-StartPosition.x)*q)-R.Left;
            //y:=StartPosition.y+Trunc((StopPosition.y-StartPosition.y)*q)-R.Top;

            if OpenForm.Game.SessionSmoothMoveAnimationEnabled then begin
               DoMove:=(x<>0) or (y<>0);
               if DoMove and
                  OpenForm.Game.SmoothMoveAnimationThresholdEnabled and
                  (x+y>OpenForm.Game.SmoothMoveAnimationThresholdMaxPixelsPerMove) then
                  OpenForm.Game.SessionSmoothMoveAnimationEnabled:=False;
               end
            else begin
               DoMove:=(Abs(x)>=ColWidth) or (Abs(y)>=RowHeight);
               if DoMove then begin
                  x:=(x div ColWidth )*ColWidth;
                  y:=(y div RowHeight)*RowHeight;
                  end;
               end;

            if DoMove then begin
               NewCell.x:=(R.Left+x+HalfColWidth ) div ColWidth;
               NewCell.y:=(R.Top +y+HalfRowHeight) div RowHeight;
               if (NewCell.x<>CurrentCell.x) or (NewCell.y<>CurrentCell.y) then begin
                  Inc(CurrentPlayerPos.x,NewCell.x-CurrentCell.x);
                  Inc(CurrentPlayerPos.y,NewCell.y-CurrentCell.y);
                  with CurrentPlayerPos do
                    if   (OpenForm.Game.Board[x,y] and OpenForm.Game.BoxTargetMaskForDisplay)=0 then
                         FrameIndex:=TILE_INDEX_PLAYER
                    else FrameIndex:=TILE_INDEX_PLAYER_ON_GOAL;

                  k:=(Abs(CurrentCell.x-NewCell.x)+Abs(CurrentCell.y-NewCell.y))*MoveCountSign;
                  Inc(MoveCount,k); if BoxNo>0 then Inc(PushCount,k);
                  CurrentCell.x:=NewCell.x; CurrentCell.y:=NewCell.y;
                  //MainForm.Status.MoveCount:=IntToStr(MoveCount);
                  //MainForm.Status.PushCount:=IntToStr(PushCount);
                  ToolsForm.StatusBar1.Panels[0].Text:=IntToStr(MoveCount)+SLASH+IntToStr(PushCount);
                  end;

               if (BoxNo>0) and (PlayerSprite=Sprites[1]) then with BoxSprite do
                  MoveTo(R.Left+x,R.Top+y);

               MoveTo(R.Left+x,R.Top+y);

               if (BoxNo>0) and (PlayerSprite=Sprites[0]) then with BoxSprite do
                  MoveTo(R.Left+x,R.Top+y);

               //Inc(FrameCount);
               Application.ProcessMessages;
               end;
            PrevTime:=Time;
            end;

         if (oAnimateReplayMovesMS<>MainForm.Game.AnimateReplayMovesMS) and
            (MainForm.Game.AnimateReplayMovesMS>0) then begin  //
            TimeMS:=Max(Max(1,Min(100,StopTime-StartTime-Time)), // '100': give the animation a chance to finish smoothly
                        (Integer(TimeMS)*MainForm.Game.AnimateReplayMovesMS div Max(1,oAnimateReplayMovesMS))-Integer(Time));
            oAnimateReplayMovesMS:=MainForm.Game.AnimateReplayMovesMS;

            //ClearFPS;
            OpenForm.Game.SessionSmoothMoveAnimationEnabled:=OpenForm.Game.SmoothMoveAnimationEnabled;

            repeat StartTime:=GetTickCount;
                   StopTime :=StartTime+TTimeMS(TimeMS);
            until  StopTime >=StartTime; // primitive clock wrap-around control

            for i:=0 to 1 do
                if Assigned(Sprites[i]) then with Sprites[i] do begin
                   StartPosition.x:=R.Left; StartPosition.y:=R.Top;
                   dx:=(StopPosition.x-StartPosition.x) / TimeMS;
                   dy:=(StopPosition.y-StartPosition.y) / TimeMS;
                   end;
            end;

         Time:=GetTickCount;
         end;

       if Time>=StartTime then begin
          //Time:=Time-StartTime; Inc(FrameTime,Time);
          end
       else begin
          // ClearFPS; // clock wrap-around
          end;

       // ensure that the sprites reached their destination

       if (BoxNo>0) and (PlayerSprite=Sprites[1]) then with BoxSprite do
          if (R.Left<>StopPosition.x) or (R.Top<>StopPosition.y) then
             MoveTo(StopPosition.x,StopPosition.y);

       with PlayerSprite do begin
         if (R.Left<>StopPosition.x) or (R.Top<>StopPosition.y) then begin
            NewCell.x:=(StopPosition.x+HalfColWidth ) div ColWidth;
            NewCell.y:=(StopPosition.y+HalfRowHeight) div RowHeight;
            k:=Abs(CurrentCell.x-NewCell.x)+Abs(CurrentCell.y-NewCell.y);

            if k<>0 then begin
               Inc(CurrentPlayerPos.x,NewCell.x-CurrentCell.x);
               Inc(CurrentPlayerPos.y,NewCell.y-CurrentCell.y);
               k:=k*MoveCountSign;
               Inc(MoveCount,k); if BoxNo>0 then Inc(PushCount,k);
               ToolsForm.StatusBar1.Panels[0].Text:=IntToStr(MoveCount)+SLASH+IntToStr(PushCount);
               //MainForm.Status.MoveCount:=IntToStr(MoveCount);
               //MainForm.Status.PushCount:=IntToStr(PushCount);
               end;

            with CurrentPlayerPos do
              if   (OpenForm.Game.Board[x,y] and OpenForm.Game.BoxTargetMaskForDisplay)=0 then
                   FrameIndex:=TILE_INDEX_PLAYER
              else FrameIndex:=TILE_INDEX_PLAYER_ON_GOAL;
            MoveTo(StopPosition.x,StopPosition.y);
            end;
         end;

       if (BoxNo>0) and (PlayerSprite=Sprites[0]) then with BoxSprite do
          if (R.Left<>StopPosition.x) or (R.Top<>StopPosition.y) then
             MoveTo(StopPosition.x,StopPosition.y);

       if LastMove and MainForm.Sound.Enabled then begin
          //MainForm.Sound.Play(SoundType);
          end;

       Application.ProcessMessages;

       if oWindowResizeCount<>ToolsForm.FormResizeCount then with OpenForm.Game do begin
          // the window was resized, and the move animation used the old settings: refresh the board
          ShowBoard0(BoardWidth,BoardHeight,Board,GameViewer.BoardImage,False);
          end;
       end;
    end;
//finally
    OpenForm.Game.IsBusy:=oIsBusy;
//end;
end;

procedure TLevelSetForm.MenuItemGridCopyToWindowsClipboardClick(
  Sender: TObject);
var BoardAsText:String; Level:TLevel;
begin
  if Grid.Row>=Grid.FixedRows then begin
     Level:=Items[Grid.Row];
     if Level<>nil then with Level do
        if   BoardAsTextLines.ToText(NL,BoardAsText) then begin
             Clipboard.AsText:=BoardAsText;
             StatusBar1.SimpleText:=CopiedClipboardItemToClipboardText+COLON+SPACE+Name;
             Self.Refresh; SleepEx(STATUS_BAR_MESSAGE_PAUSE_MS,False);
             end
        else Msg(CopyToClipboardText+COLON+SPACE+TEXT_TASK_FAILED,Self.Caption,MB_OK+MB_ICONERROR);
     end;
end;

procedure TLevelSetForm.MenuItemGridPasteFromWindowsClipboardClick(
  Sender: TObject);
var Count:Integer; NewName:String; Level:TLevel; ClipboardSokoFile:TSokoFile;
begin
  Count:=0;
  if   Clipboard.HasFormat(CF_TEXT) then begin
       try    if CreateObject(otSokoFile,TNode(ClipboardSokoFile)) then
                 try     if   ClipboardSokoFile.IsASokobanFile('') then begin
                              while not ClipboardSokoFile.Levels.IsEmpty do begin
                                Level:=TLevel(ClipboardSokoFile.Levels.Pop);
                                NewName:=SokoFile.Levels.MakeUniqueName1('',StandardClipboardItemName,False,LastClipBoardItemNo);
                                if   (NewName<>'') and Level.SetName(NewName) then begin
                                     Inc(Count);
                                     SokoFile.Levels.Add(TNode(Level));
                                     end
                                else raise Exception.Create(TEXT_TASK_FAILED);
                                end;
                              end
                         else Msg(ClipboardNotASokobanGameText,
                                  //TEXT_CLIPBOARD_NO_LEVEL_TEXT,
                                  Self.Caption,
                                  //Application.Title+SUB_TITLE_SEPARATOR+TEXT_OPEN_LEVEL_FROM_CLIPBOARD,
                                  MB_OK+MB_ICONINFORMATION);
                 finally ClipboardSokoFile.Free;
                         if Count<>0 then begin
                            SokoFile.Modified:=True;
                            LoadItems;
                            ShowItem(Grid.Row);
                            end;
                  end;
       except on E:Exception do Error(E.Message,TEXT_APPLICATION_TITLE);
       end;
       end
  else Msg(TEXT_CLIPBOARD_NO_LEVEL_TEXT,
           Self.Caption,
           //Application.Title+SUB_TITLE_SEPARATOR+TEXT_OPEN_LEVEL_FROM_CLIPBOARD,
           MB_OK+MB_ICONINFORMATION);
end;

procedure TLevelSetForm.OnFontChange;
begin
  StatusBar1.Font.Assign(Self.Font);
  PanelName.Font.Assign(Self.Font);
  PanelPaste.Font.Assign(Self.Font);
  PanelDelete.Font.Assign(Self.Font);
  PanelNew.Font.Assign(Self.Font);
  PanelRename.Font.Assign(Self.Font);
  InfoLabel.Font.Assign(Self.Font);
end;

end.

