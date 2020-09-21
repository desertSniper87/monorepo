unit Snapshots_;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Grids, Menus,
  Misc_, IniFile_, Pack_, SokFile_, SokGame_, Game_, GView_, Buttons;

type
  TSnapshotsForm = class(TForm)
    StatusBar1: TStatusBar;
    PanelOpen: TPanel;
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
    MenuItemGridOpen: TMenuItem;
    MenuItemGridDelete: TMenuItem;
    MenuItemGridRename: TMenuItem;
    MenuItemGridDeleteAll: TMenuItem;
    N5: TMenuItem;
    MenuItemGridSort: TMenuItem;
    MenuItemGridSortMoves: TMenuItem;
    MenuItemGridSortPushes: TMenuItem;
    MenuItemGridSortNames: TMenuItem;
    N6: TMenuItem;
    MenuItemGridConcatenate: TMenuItem;
    N7: TMenuItem;
    MenuItemGridCopy: TMenuItem;
    MenuItemGridMakeForwardSolution: TMenuItem;
    PanelOpenView: TPanel;
    MenuItemGridOpenView: TMenuItem;
    ImageOpenView: TImage;
    MenuItemGridPaste: TMenuItem;
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
    procedure PanelOpenClick(Sender: TObject);
    procedure PanelDeleteClick(Sender: TObject);
    procedure PanelNewClick(Sender: TObject);
    procedure PanelRenameClick(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
    procedure MenuItemGridConcatenateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure MenuItemGridCopyClick(Sender: TObject);
    procedure MenuItemGridMakeForwardSolutionClick(Sender: TObject);
    procedure PanelOpenViewClick(Sender: TObject);
    procedure ImageOpenViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemGridPasteClick(Sender: TObject);
  protected
    procedure ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
    function  GetItemCount:Integer;
  private
    { Private declarations }
    BestSolutionsCount:Integer;
    BoxTargetMaskForDisplay:Integer;
    DefaultPanelInfoHeight:Integer;
    DeleteBestSolutionWarningEnabled:Boolean;
    fMinimumClientHeight:Integer;
    fMinimumClientWidth:Integer;
    Game:TGame;
    MouseOverSoundEnabled:Boolean;

    oGridCellStr:String;
    oGridRow:Integer;

    procedure ApplicationActivate(Sender: TObject);
    procedure ApplicationDeactivate(Sender: TObject);
    function  CloseEditor(HideFirst:Boolean):Boolean;
    procedure DisplayHint(Sender: TObject);
    function  GetSnapshots(GridIndex:Integer):TSnapshot;
    function  HideEditor:Boolean;
    function  RenameSnapshot(GridIndex__:Integer; const NewName__:String):Boolean;
    procedure SetControlColor(Control: TControl; TextColor__,
                BackgroundColor__: TColor);
    procedure SetDefaultValues;
    procedure SetFormColors;
    procedure SetGridColumnsWidth;
    procedure SetPanelColor(Control:TControl);
    procedure SetSnapshots(GridIndex:Integer; Snapshot:TSnapshot);
//  procedure ShowBoard;
    procedure ShowEditor;
    procedure ShowGame(GridIndex:Integer);
    procedure ShowStatus;

  public
    { Public declarations }
    AllowDeletionOfBestSolutions:Boolean;
    BestSolutionMovesName:String;
    BestSolutionName:String;
    BestSolutionPushesName:String;
    FormColors:TFormColors;
    Initialized:Boolean;
    LastFocusedControl:TControl;
    NormalModeSnapshotName:String;
    ReverseModeSnapshotName:String;
    ShowOnStartup:Boolean;
    SolutionName:String;

    procedure Clear;
    function  DethronedSolutionName(const SolutionName__:String):String; {throws EOutOfMemory}
    procedure EnableDisableButtons(Initialize:Boolean; Snapshot__:TSnapshot);
    function  IndexOf(Snapshot__:TSnapshot):Integer;
    function  IsASolution(Snapshot__:TSnapshot):Boolean;
    function  LoadBestSolutions:Boolean;
    function  LoadNotes(Notes__:TNotes):Boolean;
    function  LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
    function  LoadSnapshots(SnapShot__:TSnapshot):Boolean;
    procedure OnFontChange;
//  function  RefreshSnapshots:Boolean;
    function  SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
    function  ShowSnapshot(GridIndex:Integer): Boolean;

    property  ItemCount:Integer read GetItemCount;
    property  MinimumClientHeight:Integer read fMinimumClientHeight;
    property  MinimumClientWidth:Integer read fMinimumClientWidth;
    property  Snapshots[GridIndex:Integer]:TSnapshot read GetSnapshots write SetSnapshots;

  end;

var
  SnapshotsForm: TSnapshotsForm = nil;

implementation

{$R *.DFM}

uses Clipbrd, SokUtil_,Text_,BitMap_,Popup_,Main_,Open1_,Sound_, Options_;

const
  SNAPSHOTS_FORM_INIFILE_SECTION='SnapshotsForm'; // don't localize

  TAG_BUTTON = 1; // all panels used as buttons are tagged with this number

procedure TSnapshotsForm.FormCreate(Sender: TObject);
const SMALL_THUMBNAIL_HEIGHT_THRESHOLD=600; SMALL_THUMBNAIL_HEIGHT=150;
var i:Integer;
begin //
  OnFontChange;
  if biMinimize in BorderIcons then BorderIcons:=BorderIcons-[biMinimize]; // minimize: it may be impossible for the user to bring the application back on the screen again (a Microsoft Windows operating system bug when the general Windows text size settings is > 100%)  

  if Screen.DeskTopHeight<SMALL_THUMBNAIL_HEIGHT_THRESHOLD then begin
     PanelBoard.Height:=SMALL_THUMBNAIL_HEIGHT;
     ImageBoard.Height:=PanelBoard.Height;
     end;

  Caption:=Application.Title+' - '+Caption;
  for i:=0 to Pred(ComponentCount) do
      if      Components[i] is TPanel       then with Components[i] as TPanel       do begin OnMouseDown:=FormMouseDown;   OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp;   end
      else if Components[i] is TButton      then with Components[i] as TButton      do begin OnMouseDown:=FormMouseDown;   OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp;   end
      else if Components[i] is TRadioButton then with Components[i] as TRadioButton do begin OnMouseDown:=FormMouseDown;   OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp;   end
      else if Components[i] is TComboBox    then with Components[i] as TComboBox    do begin OnMouseDown:=FormMouseDown;   OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp;   end
      else if Components[i] is TGroupBox    then with Components[i] as TGroupBox    do begin OnMouseDown:=FormMouseDown;   OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp;   end
      else if Components[i] is TListBox     then with Components[i] as TListBox     do begin OnMouseDown:=FormMouseDown;   OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp;   end
//    else if Components[i] is TStringGrid  then with Components[i] as TStringGrid  do begin OnMouseDown:=FormMouseDown;   OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp;   end
      else if Components[i] is TImage       then with Components[i] as TImage       do begin OnMouseDown:=FormMouseDown;   OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp;   end
      else if Components[i] is TMemo        then with Components[i] as TMemo        do begin {OnMouseDown:=FormMouseDown;} OnMouseMove:=FormMouseMove; {OnMouseUp:=FormMouseUp;} end
      else if Components[i] is TStatusBar   then with Components[i] as TStatusBar   do begin OnMouseDown:=FormMouseDown;   OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp;   end;

  ImageBoard   .OnMouseDown:=nil; ImageBoard.OnMouseUp:=ImageBoardMouseUp;
  PanelName    .OnMouseDown:=nil; PanelName .OnMouseUp:=ImageBoardMouseUp;
  ImageOpenView.OnMouseUp  :=ImageOpenViewMouseUp;

  Grid.Align:=alClient;
  Grid.Cells[1,0]:=SnapshotText;
  ImageBoard.Align:=alClient;
  InFoMemo.Align:=alClient;

  DefaultPanelInfoHeight:=PanelInfo.Height;
  fMinimumClientHeight  :=PanelGrid.Top+4*Grid.DefaultRowHeight+DefaultPanelInfoHeight+2*PanelOpen.Height+6*PanelName.Top+StatusBar1.Height;
  fMinimumClientWidth   :=Max(2*(PanelNew.Width+PanelName.Left),2*PanelGrid.Left+Canvas.TextWidth(PanelGrid.Caption))+16;

  InfoLabel.Top:=PanelBoard.Top+PanelBoard.Height+PanelName.Top;
  PanelGrid.Top:=InfoLabel.Top+InfoLabel.Height+PanelName.Top;
  PanelInfo.Top:=PanelGrid.Top+PanelGrid.Height+PanelName.Height;

  oGridRow         :=-1; oGridCellStr:='';
  PanelName.Caption:=''; Initialized:=False; BoxTargetMaskForDisplay:=0;
  Snapshots[Grid.Row]:=nil; MouseOverSoundEnabled:=True;
  DeleteBestSolutionWarningEnabled:=True;

  with ImageOpenView.Picture.BitMap.Canvas do begin
   Brush.Style:=bsSolid; Pen.Mode:=pmCopy; Pen.Style:=psSolid; Pen.Width:=1;
   end;
  PanelOpenView.Visible:=False; // the 'Open view' button isn't used; instead, when the user clicks 'Open' then the snapshot or solution is opened as a new view if there already are multiple views on the screen 

  SetDefaultValues;
end;

procedure TSnapshotsForm.FormDestroy(Sender: TObject);
begin //
  SnapshotsForm:=nil; // kludge: 'MainForm' needs this during shutdown
end;

procedure TSnapshotsForm.ApplicationActivate(Sender: TObject);
begin
  if MainForm.Music<>nil then MainForm.Music.OnActivateApplication;
  FormActivate(nil); // to set the right colors;
end;

procedure TSnapshotsForm.ApplicationDeactivate(Sender: TObject);
begin
  FormDeactivate(nil);  // reset color for active control
  if MainForm.Music<>nil then MainForm.Music.OnDeactivateApplication;
end;

procedure TSnapshotsForm.FormActivate(Sender: TObject);
begin
  MainForm.ClearTrackBox(Sender<>nil);
  MainForm.FormDeactivate(Sender);
  MainForm.MPlayer.Hide;

  Game                     :=MainForm.Game;

  Application.OnActivate   :=ApplicationActivate;
  Application.OnDeactivate :=ApplicationDeactivate;
  Application.OnHint       :=DisplayHint;
  Application.OnMessage    :=ApplicationOnMessage;

  MainForm.Status.Hint     :='';
  MouseOverSoundEnabled    :=False; // skip sound the first time; otherwise the menu-over sound somehow is played when the form opens

  if MainForm.Initialized and
     MainForm.Game.IsIdleAndStopReplayingAndBrowsing then begin

     if not Initialized then begin
        Initialized:=True;
        FormResize(Sender);
        end
     else begin
        if (OpenForm.GameViewer.Canvas      <>ImageBoard.Picture.BitMap.Canvas)
           or
           (ImageBoard.Picture.BitMap.Width <>ImageBoard.ClientWidth)
           or
           (ImageBoard.Picture.BitMap.Height<>ImageBoard.ClientHeight) then
           OpenForm.BoardResize(ImageBoard);
        ShowGame(Grid.Row);
        end;

     SetFormColors;
     StatusBar1.SizeGrip:=IsWindowsDefaultColorBtnFace(StatusBar1.Color) or
                          (Graphics.ColorToRGB(StatusBar1.Color)=Graphics.ColorToRGB(clBlack ));

     LastFocusedControl:=nil; FormMouseMove(ActiveControl,[],0,0);
     if Grid.Visible then Grid.Invalidate;
     EnableDisableButtons(True,Snapshots[Grid.Row]);
     if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
     end
  else begin
     MainForm.SetFocus;
     end;
end;

procedure TSnapshotsForm.FormDeactivate(Sender: TObject);
begin

  MainForm.SetMessageHandlers;

  MouseOverSoundEnabled:=True; // so a 'save new' event triggered by a keyboard command triggers the menu-select sound

  if Game<>nil then begin
     CloseEditor(True);
     InfoMemoExit(Sender);
     if Grid.Dragging then Grid.EndDrag(False);
     if Grid.Visible then Grid.Invalidate;
     FormMouseMove(nil,[],0,0);
     Game:=nil;
     end;
end;

procedure TSnapshotsForm.FormClose(Sender: TObject; var Action: TCloseAction);
var CanSelect:Boolean;
begin
  GridSelectCell(nil,Grid.Col,Grid.Row,CanSelect);
  FormMouseMove(nil,[],0,0);
  MainForm.MakeDynamicHints(nil);
end;

procedure TSnapshotsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=CloseEditor(False);
end;

procedure TSnapshotsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//if (Key=Ord(ACCEL_CHAR_NEW)) and
//   ((ssAlt in Shift) or (ssCtrl in Shift)) then PanelNewClick(Sender);
end;

procedure TSnapshotsForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var Editing:Boolean;
begin
  if (Screen.ActiveForm=Self) and
     (MainForm.RotateAndFlipPopupMenu<>nil) and
     MainForm.RotateAndFlipPopupMenu.Visible and
     (Key<>SIMULATED_KEYBOARD_EVENT_KEY) then
     MainForm.RotateAndFlipPopupMenu.Hide;
  if (Screen.ActiveForm=Self) and
     (MainForm.MultiViewPopupMenu<>nil) and
     MainForm.MultiViewPopupMenu.Visible and
     (Key<>SIMULATED_KEYBOARD_EVENT_KEY) then
     MainForm.MultiViewPopupMenu.Hide;

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

  if              Key=VK_ESCAPE                     then
                  if   Edit1.Visible                then //
                  else Close
  else if         Key=SIMULATED_KEYBOARD_EVENT_KEY  then //
  else if not Editing then begin
          if      Key=VK_ESCAPE                     then Close
          else if Key=VK_F2                         then PanelRenameClick(Sender)
          else if Key=VK_DELETE                     then PanelDeleteClick(Sender)
          else if Key=VK_INSERT                     then PanelNewClick(Sender)
          else if Key=Ord(ACCEL_CHAR_OPEN)          then PanelOpenClick(Sender)
          else if Key=Ord(ACCEL_CHAR_DELETE)        then PanelDeleteClick(Sender)
          else if Key=Ord(ACCEL_CHAR_NEW)           then PanelNewClick(Sender)
          else if Key=Ord(ACCEL_CHAR_RENAME)        then PanelRenameClick(Sender)
          else if Key=Ord(ACCEL_CHAR_SPLIT_VIEW)    then PanelOpenViewClick(Sender)
          else if Key=Ord(ACCEL_CHAR_EXIT)          then MainForm.SetFocus
          else if Key=VK_TAB                        then
                  if Shift=[]                       then MainForm.SetFocus
                  else
          else if Key=VK_SHIFT                      then MainForm.SetFocus
          else if Key=VK_RETURN                     then
                  if PanelName.Caption=''           then PanelNewClick(Sender)
                  else
          else if Key=VK_LEFT                       then
                  if Shift=[]                       then MainForm.SetFocus
                  else
          else if Key=VK_RIGHT                      then
                  if Shift=[]                       then begin
                     MainForm.SetFocus; Close;
                     end
                  else
          else if Key=VK_F12                        then
                  if   WindowState=wsNormal         then
                       WindowState:=wsMaximized
                  else WindowState:=wsNormal
          else;
          if (ActiveControl<>Grid) and
             (Screen.ActiveForm=Self) and
             Grid.Visible and
             (ActiveControl<>Edit1) and
             (ActiveControl<>InfoMemo)              then Grid.SetFocus;
          end;
end;

procedure TSnapshotsForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin //
  if Sender=ImageOpenView then Sender:=PanelOpenView;

  if (Screen.ActiveForm=Self) and
     (MainForm.RotateAndFlipPopupMenu<>nil) and
     MainForm.RotateAndFlipPopupMenu.Visible then
     MainForm.RotateAndFlipPopupMenu.Hide;
  if (Screen.ActiveForm=Self) and
     (MainForm.MultiViewPopupMenu<>nil) and
     MainForm.MultiViewPopupMenu.Visible then
     MainForm.MultiViewPopupMenu.Hide;

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

procedure TSnapshotsForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin //
  if Sender=ImageOpenView then Sender:=PanelOpenView;

  if TControl(Sender)<>LastFocusedControl then with FormColors do begin
     if LastFocusedControl<>nil then begin
        SetControlColor(LastFocusedControl,ButtonTextColor,ButtonColor);
        if LastFocusedControl is TPanel then with LastFocusedControl as TPanel do
           if BevelOuter=bvLowered then BevelOuter:=bvRaised;
        end;
     LastFocusedControl:=TControl(Sender);
     if LastFocusedControl<>nil then SetControlColor(LastFocusedControl,FocusedButtonTextColor,FocusedButtonColor);
     if Sender=Grid then GridMouseMove(Sender,Shift,X,Y)
     else if (((Sender is TPanel) and (TPanel(Sender).Tag=TAG_BUTTON))
              or
              ((Sender is TImage) and (TImage(Sender).Tag=TAG_BUTTON))
             )
             and
             MainForm.Sound.Enabled then
             if   MouseOverSoundEnabled then
                  MainForm.Sound.Play(stMenuOver)
             else MouseOverSoundEnabled:=True;
     end;
  if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
  if MainForm.Menu.ItemIndex>=0 then MainForm.Menu.ItemIndex:=-1;
  if (Screen.ActiveForm=Self) and
     (MainForm.RotateAndFlipPopupMenu<>nil) and
     MainForm.RotateAndFlipPopupMenu.Visible then
     MainForm.RotateAndFlipPopupMenu.Hide;
  if (Screen.ActiveForm=Self) and
     (MainForm.MultiViewPopupMenu<>nil) and
     MainForm.MultiViewPopupMenu.Visible then
     MainForm.MultiViewPopupMenu.Hide;     
end;

procedure TSnapshotsForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then Close;
end;

procedure TSnapshotsForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
//Resize:=(NewWidth -(Width -ClientWidth )>=MinimumClientWidth ) and
//        (NewHeight-(Height-ClientHeight)>=MinimumClientHeight);
  Resize:=(NewWidth >=MinimumClientWidth) and
          (NewHeight>=MinimumClientHeight);
end;

procedure TSnapshotsForm.FormResize(Sender: TObject);
var Edit:Boolean;
begin
  if ClientWidth<MinimumClientWidth then ClientWidth:=MinimumClientWidth;
  PanelInfo.Height:=DefaultPanelInfoHeight;
  if ClientHeight<MinimumClientHeight then ClientHeight:=MinimumClientHeight;
  if Initialized then begin
     Edit:=HideEditor;

     with PanelName            do begin Width   :=Self.ClientWidth-2*Left;
                                  end;
     with PanelBoard           do begin Width   :=PanelName.Width;
                                  end;
     with PanelOpen            do begin Left    :=Max(PanelName.Left,(Self.ClientWidth-(2*(PanelNew.Width+PanelName.Left))) div 2);
                                        Top     :=Self.ClientHeight-StatusBar1.Height-PanelName.Top-2*PanelOpen.Height;
                                  end;
     with PanelOpenView        do begin Left    :=PanelOpen.Left+PanelOpen.Width;
                                        Top     :=PanelOpen.Top;
                                  end;
     with PanelDelete          do begin if PanelOpenView.Visible then
                                           Left :=PanelOpenView.Left+PanelOpenView.Width
                                        else
                                           Left :=PanelOpen.Left+PanelOpen.Width;
                                        Top     :=PanelOpen.Top;
                                  end;
     with PanelNew             do begin Left    :=PanelOpen.Left;
                                        Top     :=PanelOpen.Top+PanelOpen.Height;
                                  end;
     with PanelRename          do begin Left    :=PanelDelete.Left;
                                        Top     :=PanelNew.Top;
                                  end;
     with InfoLabel            do begin Left    :=PanelName.Left;
                                        Width   :=PanelName.Width;
                                  end;
     with PanelInfo            do begin Width   :=PanelName.Width;
                                        Top     :=PanelOpen.Top-Height-PanelName.Top;
                                  end;
     with PanelGrid            do begin Top     :=InfoLabel.Top+InfoLabel.Height+PanelName.Top;
                                        Width   :=PanelName.Width;
                                        Height  :=PanelInfo.Top-Top-PanelName.Top;
                                        {
                                        if (Grid.RowCount>1) and
                                           (Grid.Cells[1,Pred(Grid.RowCount)]<>'') then // grid isn't empty
                                           Height:=Height-Grid.Height+(Grid.VisibleRowCount+Grid.FixedRows)*(Grid.DefaultRowHeight+Grid.GridLineWidth); // drop unused space below the grid
                                        }
                                  end;
     with PanelInfo            do begin Top     :=PanelGrid.Top+PanelGrid.Height+PanelName.Top;
                                        Height  :=PanelOpen.Top-Top-PanelName.Top;
                                  end;
     with Grid                 do begin SetGridColumnsWidth;
                                  end;
     with StatusBar1           do begin Width   :=Self.ClientWidth;
                                        Top     :=Self.ClientHeight-Height;
                                  end;
     with Edit1                do begin Left    :=Grid.Left+Grid.ColWidths[0]+Grid.GridLineWidth*2+0;
                                        Width   :=Grid.ColWidths[1]+Grid.GridLineWidth*2+0;
                                  end;
     with ImageOpenView        do begin Left    :=2*PanelOpenView.BevelWidth+PanelOpenView.BorderWidth;
                                        Top     :=Left;
                                        Width   :=PanelOpenView.Width -2*Left;
                                        Height  :=PanelOpenView.Height-2*Top;
                                        Visible :=True;
                                        try     Picture.BitMap.Width :=Width;
                                                Picture.BitMap.Height:=Height;
                                        except  on E:Exception do Visible:=False;
                                        end;
                                  end;
     FormMouseMove(nil,[],0,0);
     if not OpenForm.GameViewer.Initialized then
        OpenForm.GameViewer.LoadPictures;
     OpenForm.BoardResize(ImageBoard);
     ShowGame(Grid.Row);
     if Edit then ShowEditor;

     if Left>Screen.DeskTopWidth-30 then Left:=Screen.DeskTopWidth-30;
     end;
end;

procedure TSnapshotsForm.DisplayHint(Sender: TObject);
begin
  StatusBar1.{Panels[1].Text}SimpleText := GetLongHint(Application.Hint);
end;

procedure TSnapshotsForm.ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if   (Msg.Message = WM_MOUSEWHEEL) then
       if Msg.WParam>=0 then begin
          end
       else begin
          end
  else Handled:=False;
end;

procedure TSnapshotsForm.SetDefaultValues;
begin // color numbers: BGR, not RGB
  Width                        :=MinimumClientWidth;
  Left                         :=Screen.DeskTopWidth-Width;
  Top                          :=0;
  Height                       :=Min(Screen.DeskTopHeight,800)-100;

  InfoMemo.Color               :=clTeal;
  InfoMemo.Font.Color          :=clLtGray;

  BestSolutionMovesName        :=SNAPSHOT_TYPE_NAME[stBestSolutionMoves];
  BestSolutionName             :=SNAPSHOT_TYPE_NAME[stBestSolution];
  BestSolutionPushesName       :=SNAPSHOT_TYPE_NAME[stBestSolutionPushes];
  NormalModeSnapshotName       :=SNAPSHOT_TYPE_NAME[stSnapshot];
  ReverseModeSnapshotName      :=SNAPSHOT_TYPE_NAME[stReverseSnapshot];
  SolutionName                 :=SNAPSHOT_TYPE_NAME[SokFile_.stSolution];

  AllowDeletionOfBestSolutions :=False;
  InfoMemo.ReadOnly            :=False; MenuItemMemoEditClick(nil);
  ShowOnStartup                :=False;

  SetDefaultFormColors(FormColors);
  FormColors.ButtonColor       :=clGreen;
end;

procedure TSnapshotsForm.SetFormColors;
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
    with ImageOpenView       do begin Font.Color:=BackgroundTextColor;    Color:=BackgroundColor;    end;
    end;
end;

procedure TSnapshotsForm.SetControlColor(Control:TControl; TextColor__,BackgroundColor__:TColor);
var R:TRect;
begin
  with FormColors do
    if      Control is TPanel                                then with Control as TPanel       do
            if Tag=TAG_BUTTON                                then begin
               if Font.Color=GrayedButtonTextColor           then  Color:=GrayedButtonColor
               else                                          begin Font.Color:=TextColor__; Color:=BackgroundColor__; end;
               if Control=PanelOpenView then begin
                  ImageOpenView.Picture.BitMap.Canvas.Pen  .Color:=Font.Color;
                  ImageOpenView.Picture.BitMap.Canvas.Brush.Color:=Color;
                  with ImageOpenView do with ImageOpenView.Picture.BitMap.Canvas do begin
                    R:=Classes.Rect(0,0,Width,Height);
                    FillRect(R);

                    // draw 'split view' icon
                    Brush.Color:=Pen.Color;
                    R.Right :=Min(R.Right ,R.Left+14); // 14 x 11 matches the 'split view' menu button icon in the main window
                    R.Bottom:=Min(R.Bottom,R.Top +11);
                    if Odd(RectWidth(R)) then Dec(R.Right);
                    R:=RectPlusOffset(R,(Width -RectWidth (R)) div 2,Succ(Height-RectHeight(R)) div 2); // 'Succ': it the image cannot be centered vertically then it looks better to push it 1 pixel further down
                    FrameRect(R);
                    Inc(R.Left); Inc(R.Top); Dec(R.Right); Dec(R.Bottom);
                    FrameRect(R);
                    Inc(R.Top);                     MoveTo(R.Left,R.Top); LineTo(R.Right,R.Top);
                    Inc(R.Top);                     MoveTo(R.Left,R.Top); LineTo(R.Right,R.Top);
                    Inc(R.Left,Pred(RectWidth(R) div 2)); MoveTo(R.Left,R.Top); LineTo(R.Left,R.Bottom);
                    Inc(R.Left);                    MoveTo(R.Left,R.Top); LineTo(R.Left,R.Bottom);
                    end;
                  end;
               end
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

function TSnapshotsForm.LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
var Section:String;
begin // LoadSettingsFromIniFile;
  Section:=SNAPSHOTS_FORM_INIFILE_SECTION;

  Left                        :=Max(Screen.DeskTopLeft ,Min(Screen.DeskTopLeft  +Screen.DeskTopWidth -MinimumClientWidth -(Width -ClientWidth ),IniFile.ReadInteger(Section,'Left',Left)));
  Top                         :=Max(Screen.DeskTopTop  ,Min(Screen.DeskTopTop   +Screen.DeskTopHeight-MinimumClientHeight-(Height-ClientHeight),IniFile.ReadInteger(Section,'Top',Top)));
  Width                       :=Max(MinimumClientWidth ,Min(Screen.DeskTopWidth -Left,IniFile.ReadInteger(Section,'Width',Width)));
  Height                      :=Max(MinimumClientHeight,Min(Screen.DeskTopHeight-Top,IniFile.ReadInteger(Section,'Height',Height)));

  AllowDeletionOfBestSolutions:=IniFile.ReadBool  (Section,'AllowDeletionOfBestSolutions',AllowDeletionOfBestSolutions);
  ShowOnStartup               :=IniFile.ReadBool  (Section,'ShowOnStartup',ShowOnStartup);

  BestSolutionName            :=IniFile.ReadString(Section,'BestSolutionName',BestSolutionName);
  BestSolutionMovesName       :=IniFile.ReadString(Section,'BestSolutionMovesName',BestSolutionMovesName);
  BestSolutionPushesName      :=IniFile.ReadString(Section,'BestSolutionPushesName',BestSolutionPushesName);
  NormalModeSnapshotName      :=IniFile.ReadString(Section,'NormalModeSnapshotName',NormalModeSnapshotName);
  ReverseModeSnapshotName     :=IniFile.ReadString(Section,'ReverseModeSnapshotName',ReverseModeSnapshotName);
  SolutionName                :=IniFile.ReadString(Section,'SolutionName',SolutionName);

  InfoMemo.ReadOnly           :=IniFile.ReadBool(Section,'MemoReadOnly',InfoMemo.ReadOnly);
  MenuItemMemoEditClick(nil);
  InfoMemo.Color              :=TColor(IniFile.ReadInteger(Section,'MemoColor',Integer(InfoMemo.Color)));

  Result                      :=LoadFormColorsFromIniFile(IniFile,Section,FormColors) and
                                LoadFontFromIniFile(IniFile,Section,'Memo',InfoMemo.Font) and
                                LoadFontFromIniFile(IniFile,Section,'Window',Self.Font);
  OnFontChange;
end;

function TSnapshotsForm.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
var Section:String;
begin // SaveSettingsToIniFile;
  try    Section:=SNAPSHOTS_FORM_INIFILE_SECTION;

         if  WindowState=wsNormal then begin
             IniFile.WriteInteger(Section,'Left',Left);
             IniFile.WriteInteger(Section,'Top',Top);
             IniFile.WriteInteger(Section,'Width',Width);
             IniFile.WriteInteger(Section,'Height',Height);
             end;

         IniFile.WriteBool   (Section,'AllowDeletionOfBestSolutions',AllowDeletionOfBestSolutions);
         IniFile.WriteBool   (Section,'ShowOnStartup',ShowOnStartup);

         IniFile.WriteString (Section,'BestSolutionName',BestSolutionName);
         IniFile.WriteString (Section,'BestSolutionMovesName',BestSolutionMovesName);
         IniFile.WriteString (Section,'BestSolutionPushesName',BestSolutionPushesName);
         IniFile.WriteString (Section,'NormalModeSnapshotName',NormalModeSnapshotName);
         IniFile.WriteString (Section,'ReverseModeSnapshotName',ReverseModeSnapshotName);
         IniFile.WriteString (Section,'SolutionName',SolutionName);

         IniFile.WriteBool   (Section,'MemoReadOnly',InfoMemo.ReadOnly);
         IniFile.WriteInteger(Section,'MemoColor',Integer(InfoMemo.Color));

         Result:=SaveFormColorsToIniFile(IniFile,Section,FormColors) and
                 SaveFontToIniFile(IniFile,Section,'Memo',InfoMemo.Font) and
                 SaveFontToIniFile(IniFile,Section,'Window',Self.Font);

  except on E:Exception do Result:=Error(E.Message,Application.Title);
  end;
end;

procedure TSnapshotsForm.SetPanelColor(Control:TControl);
begin // 'SetPanelColor' is a bit of a misnomer because the control may be either a panel or an image
  with FormColors do begin
    if      Control is TPanel then with Control as TPanel do begin
            if   Enabled then
                 Font.Color:=ButtonTextColor
            else Font.Color:=GrayedButtonTextColor;
            end
    else if Control is TImage then with Control as TImage do with Picture.BitMap.Canvas do begin
            if   Enabled then
                 Font.Color:=ButtonTextColor
            else Font.Color:=GrayedButtonTextColor;
            end;
    if   Control=LastFocusedControl then
         SetControlColor(Control,FocusedButtonTextColor,FocusedButtonColor)
    else SetControlColor(Control,ButtonTextColor       ,ButtonColor);
    end;
end;

procedure TSnapshotsForm.EnableDisableButtons(Initialize:Boolean; Snapshot__:TSnapshot);
var s1,s2,s3:String;
begin
  Game:=MainForm.Game;
  
  if (PanelName   .Enabled<>(PanelName.Caption<>'')) or Initialize then begin
     PanelName    .Enabled:=PanelName.Caption<>'';
     ImageBoard   .Enabled:=PanelName.Enabled;
     PanelOpen    .Enabled:=PanelName.Enabled;
     PanelOpenView.Enabled:=PanelOpen.Enabled
                            //and
                            //PanelOpenView.Visible
                            and
                            (MainForm.BtnSplitView.Enabled or (not MainForm.MultiView.IsEmpty));
     ImageOpenView.Enabled:=PanelOpenView.Enabled;
     PanelDelete  .Enabled:=PanelOpen.Enabled
                            and
                            ((Grid.Row>=Grid.FixedRows+BestSolutionsCount)
                             or
                             (AllowDeletionOfBestSolutions and (Grid.Row>=Grid.FixedRows))
                            );
     PanelNew     .Enabled:=(Game<>nil) and (Game.FileName<>'');// and (Game.History.Count>Game.ForcedInitialJumps);
     PanelRename  .Enabled:=PanelDelete.Enabled and (Grid.Row>=Grid.FixedRows+BestSolutionsCount);

     SetPanelColor(PanelName);
     SetPanelColor(PanelOpen);
     SetPanelColor(PanelOpenView);
     SetPanelColor(PanelDelete);
     SetPanelColor(PanelNew);
     SetPanelColor(PanelRename);

     if PanelName.Caption='' then begin
        Grid.Hide; oGridRow:=-1; oGridCellStr:='';
        if PanelNew.Enabled then begin
           PanelGrid.Show; ActiveControl:=PanelNew;
           end
        else begin
           PanelGrid.Hide; ActiveControl:=InfoMemo;
           end;
        InfoLabel.Caption:='';
        end
     else if not Grid.Visible then begin
             if not PanelGrid.Visible then PanelGrid.Show;
             Grid.Show;
             ActiveControl:=Grid;
             SetGridColumnsWidth;
             end;
     end;

  s1:='';
  if   PanelName.Enabled and
       Assigned(Snapshot__) then begin
       if IsASolution(Snapshot__) then begin
          s1:=HintOpenSnapshot1Text[True];
          s2:=HintOpenSnapshotButtonText[True];
          s3:=HintOpenSnapshotAsNewViewButtonText[True];
          end
       else begin
          s1:=HintOpenSnapshot1Text[False];
          s2:=HintOpenSnapshotButtonText[False];
          s3:=HintOpenSnapshotAsNewViewButtonText[False];
          end;
       end
  else begin
    s2:=HintOpenSnapshotButtonText[False];
    s3:=HintOpenSnapshotAsNewViewButtonText[False];
    end;

  PanelName .Hint:=s1;
  ImageBoard.Hint:=s1;
  PanelOpen .Hint:=s2;
  PanelOpenView.Hint:=s3;
  ImageOpenView.Hint:=s3;

  FormMouseMove(nil,[],0,0); // to update colors on the screen
  MainForm.MakeDynamicHints(nil);
  ShowStatus;
end;

procedure TSnapshotsForm.ShowStatus;
begin //
end;

procedure TSnapshotsForm.Clear;
var i:Integer;
begin
  Game:=MainForm.Game;
  if ItemCount<>0 then begin
     for i:=0 to Pred(Grid.RowCount) do Snapshots[i]:=nil;
     Grid.RowCount:=Succ(Grid.FixedRows);
     Grid.Cells[0,Grid.FixedRows]:='';
     Grid.Cells[1,Grid.FixedRows]:='';
     end;
  Grid.Cells[1,0]:=SnapshotText;
  if Assigned(OpenForm) and Assigned(OpenForm.GameViewer) and
     Assigned(OpenForm.GameViewer.Pictures[ptScreenBackground]) and
     (OpenForm.GameViewer.Pictures[ptScreenBackground].View=ivFloorTile) then
     OpenForm.GameViewer.BoardRect:=Rect(0,0,0,0);
  ShowSnapshot(0);
  BestSolutionsCount:=0;
  oGridRow:=-1; oGridCellStr:='';
end;

function  TSnapshotsForm.LoadNotes(Notes__:TNotes):Boolean;
var Node:TNode;
begin
  Result:=True;
  InfoMemo.Lines.BeginUpdate;
  try     try    InfoMemo.Clear;
                 if Assigned(Notes__) then begin
                    Node:=Notes__.Lines.Items;
                    while Assigned(Node) do begin
                      InfoMemo.Lines.Add(Node.Text); Node:=Node.Next;
                      end;
                    end;
          except on E:Exception do Result:=Error(E.Message,Application.Title);
          end;
  finally InfoMemo.Modified:=False;
          InfoMemo.SelStart:=0; InfoMemo.SelLength:=0;
          InfoMemo.Lines.EndUpdate;
  end;
end;

function  TSnapshotsForm.LoadSnapshots(SnapShot__:TSnapshot):Boolean;
var i:Integer; s:String; v:TSnapshot;
begin
  Result:=False; Game:=MainForm.Game;
  if (Game<>nil) and (Game.Snapshots<>nil) then with Grid do
     try
       BestSolutionsCount:=Game.BestSolutionsCount;
       if (ItemCount<>0) and
          Game.Snapshots.IsEmpty and
          (BestSolutionsCount=0) then
          Clear
       else begin
          for i:=0 to Pred(RowCount) do Snapshots[i]:=nil;
          for i:=0 to Pred(ColCount) do Cells[i,FixedRows]:='';
          RowCount:=FixedRows+Max(1,Game.Snapshots.Count+BestSolutionsCount);
          Result:=LoadBestSolutions;
          if Result then begin
             v:=TSnapshot(Game.Snapshots.First); i:=0;
             while v<>nil do begin
               Cells    [0,i+FixedRows+BestSolutionsCount]:=IntToStr(Succ(i)){+SPACE};
               Cells    [1,i+FixedRows+BestSolutionsCount]:=v.Name;
               Snapshots[  i+FixedRows+BestSolutionsCount]:=v;
               v:=TSnapshot(v.Next); Inc(i);
               end;
             end;
          end;

       s:=IntToStr(Max(999,RowCount))+SPACE+SPACE+SPACE;
       ColWidths[0]:=Canvas.TextWidth(s);
       ColWidths[1]:=ClientWidth-ColWidths[0]-2*GridLineWidth;
       //while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]);
       Grid.Cells[1,0]:=VisualFileName(Game.FileName);
       if IsBlank(Grid.Cells[1,0]) then Grid.Cells[1,0]:=SnapshotText;

       if   Snapshot__=nil then i:=Row
       else i:=IndexOf(Snapshot__);
       if (i<FixedRows)or (i>=RowCount) then i:=FixedRows;

       if Assigned(Snapshots[i]) and
          (MainForm.GameViewer.IsFloorTileVisible) then
          OpenForm.GameViewer.Modified:=True; // ensure that the board background is shown

       ShowSnapshot(i);
       oGridRow:=i;
       if   (oGridRow>=FixedRows) and (oGridRow<RowCount) then
            oGridCellStr:=Cells[1,oGridRow]
       else oGridCellStr:='';
       Row:=i;

       Result:=True;
     except on E:Exception do Result:=Error(E.Message,Application.Title);
     end;
  if not Result then Clear;
end;

procedure TSnapshotsForm.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
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
                  //gdFocused in AState then // don't use 'Focused', because current row then only is highlighted when the grid has focus
                  then begin
                  if        s='' then
                            begin Canvas.Font .Color:=BackgroundTextColor;
                                  Canvas.Brush.Color:=BackgroundColor;
                            end
                  else if   ACol<FixedCols then
                            if   ARow<FixedRows+BestSolutionsCount then
                                 begin Canvas.Font .Color:=BackgroundTextColor;
                                       Canvas.Brush.Color:=BackgroundColor;
                                 end
                            else begin Canvas.Font .Color:=WindowTextColor;
                                       Canvas.Brush.Color:=WindowColor;
                                 end
                       else begin Canvas.Font .Color:=FocusedWindowTextColor;
                                  Canvas.Brush.Color:=FocusedWindowColor;
                            end;
                  end
          else if ARow<FixedRows+BestSolutionsCount then
                  begin Canvas.Font .Color:=BackgroundTextColor;
                        Canvas.Brush.Color:=BackgroundColor;
                  end
          else begin Canvas.Font .Color:=WindowTextColor;
                     Canvas.Brush.Color:=WindowColor;
               end;

          Canvas.FillRect(ARect);

          Dec(ARect.Right); Dec(ARect.Bottom);

          if s<>'' then begin
             ExtTextOut(Canvas.Handle, ARect.Left + 2, ARect.Top + 2, ETO_CLIPPED or
                        ETO_OPAQUE, @ARect, PChar(S), Length(S), nil);
             {
             Inc(ARect.Left,2);
             if   ACol<>0 then
                  Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),ARect,DT_VCENTER or DT_LEFT   or DT_SINGLELINE,nil)
             else Windows.DrawTextEx(Canvas.Handle,PChar(S),Length(S),ARect,DT_VCENTER or DT_RIGHT  or DT_SINGLELINE,nil);
             Dec(ARect.Left,2);
             }
             end;

          if ACol=0 then with Canvas do begin
             Pen.Color:=Font.Color;
             Pen.Style:=psSolid;
             MoveTo(ARect.Right,ARect.Top);
             LineTo(ARect.Right,ARect.Bottom+1);
             end;

          if (ARow=Pred(FixedRows))
             or
             (ARow=Pred(RowCount))
             or
             ((BestSolutionsCount<>0)
              and
              (ARow=Pred(FixedRows+BestSolutionsCount))
             )
             then with Canvas do begin
             Pen.Color:=Font.Color;
             Pen.Style:=psSolid;
             if (ARow=Pred(RowCount)) and (FixedRows<>RowCount) then Inc(ARect.Bottom);
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

procedure TSnapshotsForm.GridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect:=(ACol=1) and (ARow>=Grid.FixedRows) and (ARow<Grid.RowCount);
  if CanSelect then with Grid do begin
     InfoMemoExit(Sender);
     if (Sender<>nil) and (ARow<>oGridRow) then begin
        ShowSnapshot(ARow);
        if IsASolution(Snapshots[ARow])<>IsASolution(Snapshots[oGridRow]) then begin
           MainForm.MakeDynamicHints(Snapshots[ARow]);
           MainForm.ShowStatus;
           end;
        if Sender=MainForm.MultiView then Row:=ARow;   
        end;
     oGridRow:=ARow; oGridCellStr:=Cells[1,oGridRow];
     Options:=Options-[goEditing,goAlwaysShowEditor];
     if PanelDelete.Enabled<>(PanelOpen.Enabled
                              and
                              ((ARow>=Grid.FixedRows+BestSolutionsCount)
                               or
                               (AllowDeletionOfBestSolutions and (Grid.Row>=Grid.FixedRows))
                              )
                             ) then begin
        PanelDelete.Enabled:=not PanelDelete.Enabled;
        SetPanelColor(PanelDelete);
        end;
     if PanelRename.Enabled<>(PanelDelete.Enabled and (ARow>=Grid.FixedRows+BestSolutionsCount)) then begin
        PanelRename.Enabled:= PanelDelete.Enabled and (ARow>=Grid.FixedRows+BestSolutionsCount);
        SetPanelColor(PanelRename);
        end;
     end;
end;

function  TSnapshotsForm.ShowSnapshot(GridIndex:Integer):Boolean;
var s:String; Snapshot:TSnapshot; n:TNode;
begin
  Game:=MainForm.Game;

  if   (GridIndex>=Grid.FixedRows) and
       (GridIndex<Grid.RowCount) and
       (Snapshots[GridIndex]<>nil) then begin
       s:=Trim(Grid.Cells[1,GridIndex]);
       if   s='' then Snapshot:=nil
       else Snapshot:=Snapshots[GridIndex];
       end
  else begin s:=''; Snapshot:=nil;
       end;
  Result:=(s<>'') and (Snapshot<>nil);

  if   Result then PanelName.Caption:=SPACE+s
  else PanelName.Caption:='';

  InfoMemo.Lines.BeginUpdate;
  try     try    InfoMemo.Clear;
                 if Result then begin
                    n:=Snapshot.Notes.Lines.Items;
                    while n<>nil do begin
                      InfoMemo.Lines.Add(n.Text); n:=n.Next;
                      end;
                    end;
          except on E:Exception do Result:=Error(E.Message,Application.Title);
          end;
  finally InfoMemo.Modified:=False;
          InfoMemo.SelStart:=0; InfoMemo.SelLength:=0;
          InfoMemo.Lines.EndUpdate;

          if   Result {and (Snapshot.MoveCount>0)} then with Snapshot do begin
               s:=Format(SnapshotInfoText__[ReverseMode],[MoveCount,PushCount]);
               if   ReverseMode then
                    InfoLabel.Caption:=TEXT_REVERSE_MODE+' - '+s
               else InfoLabel.Caption:=s;
               end
          else InfoLabel.Caption:='';

          EnableDisableButtons(False,Snapshot);
          if Visible then ShowGame(GridIndex);
  end;
end;

procedure TSnapshotsForm.EditPopupMenuPopup(Sender: TObject);
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

procedure TSnapshotsForm.MenuItemMemoEditClick(Sender: TObject);
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

procedure TSnapshotsForm.MenuItemMemoUndoClick(Sender: TObject);
begin
  SendMessage(InfoMemo.Handle,EM_UNDO,0,0);
end;

procedure TSnapshotsForm.MenuItemMemoCutClick(Sender: TObject);
begin
  InfoMemo.CutToClipBoard;
end;

procedure TSnapshotsForm.MenuItemMemoCopyClick(Sender: TObject);
begin
  InfoMemo.CopyToClipboard;
end;

procedure TSnapshotsForm.MenuItemMemoPasteClick(Sender: TObject);
begin
  InfoMemo.PasteFromClipboard;
end;

procedure TSnapshotsForm.MenuItemMemoDeleteClick(Sender: TObject);
begin
  InfoMemo.ClearSelection;
end;

procedure TSnapshotsForm.MenuItemMemoSelectAllClick(Sender: TObject);
begin
  InfoMemo.SelectAll;
end;

procedure TSnapshotsForm.MenuItemMemoPenColorClick(Sender: TObject);
begin
  ColorDialog1.Color:=InfoMemo.Font.Color;
  if ColorDialog1.Execute then
     begin InfoMemo.Font.Color:=ColorDialog1.Color;
     end;
end;

procedure TSnapshotsForm.MenuItemMemoBackgroundColorClick(Sender: TObject);
begin
  ColorDialog1.Color:=InfoMemo.Color;
  if ColorDialog1.Execute then
     begin InfoMemo.Color:=ColorDialog1.Color;
     end;
end;

procedure TSnapshotsForm.MenuItemMemoFontClick(Sender: TObject);
begin
  InfoMemo.HideSelection:=False;
  FontDialog1.Font.Assign(InfoMemo.Font);
  if FontDialog1.Execute then with InfoMemo do
     begin Font.Assign(FontDialog1.Font);
     end;
  InfoMemo.HideSelection:=True;
end;

procedure TSnapshotsForm.PanelOpenClick(Sender: TObject);
var NewSaveGame:TSnapshot;
begin
  NewSaveGame:=nil;
  if CloseEditor(False) and
     (Snapshots[Grid.Row]<>nil) then
     try
       MainForm.MPlayer.Hide;

       with MainForm.Game do
         if (ReverseMode<>Self.Snapshots[Grid.Row].ReverseMode) and
            (History.Top>ForcedinitialJumps) then
            NewSaveGame:=MakeSnapshot(SnapshotTypeName(stSaveGame)+SPACE+Format(FORMAT_MOVES_AND_PUSHES,[History.Count,History.PushCount]));

       MainForm.SetFocus;
       MainForm.ClearTrackBox(True);

       if      (Sender<>PanelOpenView) and MainForm.MultiView.IsEmpty then begin
               MainForm.Game.LoadSnapshot(Snapshots[Grid.Row]);
               with Snapshots[Grid.Row] do MainForm.InitGame((GameState=gsSolved) and (MoveCount=MoveTop),False,False,False,False,True,True,0);
               MainForm.Sound.Play(Sound_.stRestartGame); MouseOverSoundEnabled:=False;
               if MainForm.Game.StartTimeMS=0 then MainForm.Game.StartTimer;
               end
       else if MainForm.MultiView.LoadSnapshot(Snapshots[Grid.Row]) then begin
               //MainForm.Sound.Play(Sound_.stRestartGame);
               MouseOverSoundEnabled:=False;
               if MainForm.Game.StartTimeMS=0 then MainForm.Game.StartTimer;
               end;

     finally
       if NewSaveGame<>nil then with MainForm.Game do begin
          DeleteSnapshot(SaveGame);  // the new savegame can first be activated here
          SaveGame:=NewSaveGame;     // because 'TGame.LoadSnapshot' needs the old one
          end;
     end;
end;

procedure TSnapshotsForm.PanelOpenViewClick(Sender: TObject);
begin
  if PanelOpenView.Enabled then begin
     FormMouseMove(nil,[],0,0);
     //MainForm.Sound.Play(Sound_.stMenuSelect); MouseOverSoundEnabled:=False;
     PanelOpenClick(PanelOpenView);
     end;
end;

procedure TSnapshotsForm.ImageOpenViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FormMouseUp(Sender,Button,Shift,X,Y);
  with ImageOpenView do
    if (Button=mbLeft) and
       PtInRect(Classes.Rect(-Left,-Top,Width+2*Left,Height+2*Top),Point(X,Y)) then
       PanelOpenViewClick(PanelOpenView);
end;

procedure TSnapshotsForm.PanelDeleteClick(Sender: TObject);
var i:Integer; oCursor:TCursor;
    OldBestSolutionMoves,OldBestSolutionPushes,NextSnapshot,Snapshot:TSnapshot;
begin
  MainForm.Sound.Play(Sound_.stMenuSelect); MouseOverSoundEnabled:=False;
  if Edit1.Visible then Edit1.Text:=oGridCellStr;
  if CloseEditor(False) then with Grid do begin
     if (Game=nil) and (MainForm.Game<>nil) then
        Game:=MainForm.Game;
     if (PanelName.Caption<>'') and (Trim(PanelName.Caption)=Cells[1,Row]) and
        (Game<>nil) and (Game.Snapshots<>nil) and
        ((Row>=FixedRows+Game.BestSolutionsCount)
         or
         (AllowDeletionOfBestSolutions and (Row>=FixedRows))
        )
        then begin
        Snapshot:=Snapshots[Row];

        if (Snapshot=Game.BestSolutionMoves) or
           (Snapshot=Game.BestSolutionPushes) then begin {delete one of the best solutions}

           if DeleteBestSolutionWarningEnabled then begin
              DeleteBestSolutionWarningEnabled:=False;
              if Msg(DeleteBestSolutionWarningText,TEXT_APPLICATION_TITLE+' - '+DeleteSolutionText,MB_YESNO+MB_ICONWARNING+MB_DEFBUTTON2)<>ID_YES then
                 Snapshot:=nil;
              end;

           if Snapshot<>nil then begin
              oCursor:=Screen.Cursor;
              try
                Screen.Cursor:=crHourGlass;
                if   Snapshot=Game.BestSolutionMoves then begin
                     Game.BestSolutionMoves:=Game.BestSolutionPushes; {promote best-solution-pushes, if any, to best-solution-moves}
                     Game.BestSolutionPushes:=nil;
                     end
                else Game.BestSolutionPushes:=nil;
                Game.DeleteSnapshot(Snapshot);

                OpenForm.Game.Clear;
                OpenForm.Game.BoardWidth:=Game.BoardWidth;
                OpenForm.Game.BoardHeight:=Game.BoardHeight;
                OpenForm.Game.Board:=Game.StartBoard;
                OpenForm.Game.CalculateInternalData;
                SwapNodes(TNode(OpenForm.Game.BestSolutionMoves ),TNode(Game.BestSolutionMoves )); {transfer the best solutions to 'OpenForm.Game'}
                SwapNodes(TNode(OpenForm.Game.BestSolutionPushes),TNode(Game.BestSolutionPushes));

                SnapShot:=TSnapshot(Game.Snapshots.First);

                while Snapshot<>nil do begin
                   NextSnapshot:=TSnapshot(Snapshot.Next);

                   if (Snapshot.GameState=gsSolved) and (not Snapshot.ReverseMode) then begin
                      OldBestSolutionMoves :=OpenForm.Game.BestSolutionMoves;
                      OldBestSolutionPushes:=OpenForm.Game.BestSolutionPushes;

                      if OpenForm.Game.LoadSnapshot(Snapshot) then begin
                        if OpenForm.Game.History.Count=Snapshot.MoveTop then
                            OpenForm.Game.InheritedUndo(True);                     {undo a move so 'Redo' can be used for updating the best solutions}
                         while (OpenForm.Game.History.Count<Snapshot.MoveTop) and
                               OpenForm.Game.InheritedRedo(True) do begin end;

                        if OpenForm.Game.GameState=gsSolved then begin
                           if      OldBestSolutionMoves<>OpenForm.Game.BestSolutionMoves then begin {'True': the snapshot is a new best solution/moves}
                                   OpenForm.Game.BestSolutionMoves.SetName(Snapshot.Name); {keep the original name for a while; it may carry a solver identication; the name is updated below by 'Game.UpdateBestSolutionNames'}
                                   Snapshot.Notes.CopyTo(OpenForm.Game.BestSolutionMoves.Notes);
                                   OpenForm.Game.BestSolutionMoves.SnapshotTag:=Cardinal(Snapshot);
                                   end
                           else if OldBestSolutionPushes<>OpenForm.Game.BestSolutionPushes then begin {'True': the snapshot is a new best solution/pushes}
                                   OpenForm.Game.BestSolutionPushes.SetName(Snapshot.Name); {keep the original name for a while; it may carry a solver identication; the name is updated below by 'Game.UpdateBestSolutionNames'}
                                   Snapshot.Notes.CopyTo(OpenForm.Game.BestSolutionPushes.Notes);
                                   OpenForm.Game.BestSolutionPushes.SnapshotTag:=Cardinal(Snapshot);
                                   end;
                           end;
                        end;
                     end;

                  Snapshot:=NextSnapshot;
                  end;

               SwapNodes(TNode(OpenForm.Game.BestSolutionMoves ),TNode(Game.BestSolutionMoves )); {get the best solutions back from 'OpenForm.Game'}
               SwapNodes(TNode(OpenForm.Game.BestSolutionPushes),TNode(Game.BestSolutionPushes));

               if Assigned(Game.BestSolutionMoves) then with Game.BestSolutionMoves do
                  if SnapshotTag<>0 then begin
                     Game.Snapshots.Remove(TSnapshot(SnapshotTag),True);          {destroy the snapshot that has been promoted to best solution/moves (the promotion created a new copy of the snapshot)}
                     SnapshotTag:=0;
                     end;
               if Assigned(Game.BestSolutionPushes) then with Game.BestSolutionPushes do
                  if SnapshotTag<>0 then begin
                     Game.Snapshots.Remove(TSnapshot(SnapshotTag),True);          {destroy the snapshot that has been promoted to best solution/moves (the promotion created a new copy of the snapshot)}
                     SnapshotTag:=0;
                     end;

               Game.UpdateBestSolutionNames;

               if   (BestSolutionsCount<2) or
                    (Game.BestSolutionPushes=nil) or
                    ((Row=FixedRows) and MainForm.ShowSolutionMoves) or
                    ((Row=Succ(FixedRows)) and (not MainForm.ShowSolutionMoves) and (BestSolutionsCount=2)) then
                    Snapshot:=Game.BestSolutionMoves
               else Snapshot:=Game.BestSolutionPushes;
               LoadSnapshots(Snapshot);

               OpenForm.Game.Clear;
               Game.OriginalBestSolutionMovesName :=''; {ensure that the solutions aren't resurrected when the level is closed}
               Game.OriginalBestSolutionPushesName:='';
               if Game.BestSolutionMoves <>nil then Game.BestSolutionMoves .Modified:=True; {ensure that the best solutions, if any, are updated when the level is closed}
               if Game.BestSolutionPushes<>nil then Game.BestSolutionPushes.Modified:=True;
               Game.Notes.Modified:=True; {ensure that the file is updated when the level is closed}
             finally
               Screen.Cursor:=oCursor;
             end;
             end;
           end
        else begin {delete a normal snapshot, i.e., not one of the best solutions}
           Cells[1,Row]:=''; Snapshots[Row]:=nil;
           Game.DeleteSnapshot(Snapshot);
           for i:=Row to RowCount-2 do begin
               Cells    [1,i]:=Cells[1,Succ(i)];
               Snapshots[i]  :=Snapshots[Succ(i)];
               end;
           Cells[1,Pred(RowCount)]:=''; Snapshots[Pred(RowCount)]:=nil;
           if   RowCount>Succ(FixedRows) then
                RowCount:=RowCount-1;
           end;

        if   RowCount>FixedRows then
             ShowSnapshot(Grid.Row)
        else ShowSnapshot(0);
        if   Cells[1,Grid.Row]=''       then MainForm.SetFocus // empty grid: keyboard commands should go to 'MainForm'
        else if ActiveControl =InfoMemo then Grid.SetFocus;

        MainForm.MakeDynamicHints(nil);
        MainForm.ShowStatus;
        end;
     end;
end;

procedure TSnapshotsForm.PanelNewClick(Sender: TObject);
var oRowCount:Integer; Snapshot:TSnapshot;
begin
  Game:=MainForm.Game;
  if PanelNew.Enabled or (Sender=MainForm.Solver) or (Sender=MainForm.Optimizer) or (Sender=MainForm.MultiView) then begin
     if (Sender<>MainForm.BtnSnapshots) and // if 'New was triggered by the button, then the 'select menu' sound is playing
        (Sender<>MainForm.Solver) and
        (Sender<>MainForm.Optimizer) and
        (Sender<>MainForm.MultiView) then
        MainForm.Sound.Play(Sound_.stMenuSelect);
     MouseOverSoundEnabled:=False;
     if CloseEditor(False)
        and
        (Game<>nil)
        and
        ((Sender=MainForm.Solver) // the solver, the optimizer, and the multiple views manager add the new snapshot as the last member of the 'snapshots' list before calling this procedure
         or
         (Sender=MainForm.Optimizer)
         or
         (Sender=MainForm.MultiView)
         or
         Game.SaveSnapshot // create a snapshot of the current game state
        ) then
        with Grid do begin
          Snapshot:=TSnapshot(Game.Snapshots.Last);
          oRowCount:=RowCount;
          try    if (RowCount=Succ(FixedRows)) and
                    (Cells[1,Pred(RowCount)]='') then begin // last item (= first item) is empty
                    if (MainForm.GameViewer.IsFloorTileVisible) then
                       OpenForm.GameViewer.Modified:=True; // ensure that the board background is shown
                    end
                 else begin
                   RowCount:=RowCount+1;
                   if RowCount<>oRowCount+1 then raise EoutOfMemory.Create(TEXT_MEMORY_FULL);
                   end;
                 SetGridColumnsWidth;
                 Cells     [0,Pred(RowCount)]:=IntToStr(RowCount-FixedRows-BestSolutionsCount);
                 Cells     [1,Pred(RowCount)]:=Snapshot.Name;
                 Snapshots [Pred(RowCount)  ]:=Snapshot;
                 if   Row =Pred(RowCount) then
                      ShowSnapshot(Row)
                 else Row:=Pred(RowCount); // 'GridSelectCell' calls 'ShowSnapshot'
                 if (ActiveControl=InfoMemo) and (Screen.ActiveForm=Self) then
                    Grid.SetFocus;
                 if (Sender=MainForm.Solver) or (Sender=MainForm.Optimizer) then ShowOnStartUp:=True;
                 MainForm.MakeDynamicHints(nil);
                 MainForm.ShowStatus;

                 if (Snapshot.GameState=gsSolved) and (not Game.IsLoading) then
                    MainForm.SaveSnapshotToLogFile(MakeIniFileSectionFileName(Game.SokoFileName,Game.Name),'',Snapshot.GetMovesAsText);

          except on E:Exception do begin
                    Game.DeleteSnapshot(Snapshot); RowCount:=oRowCount;
                    Error(E.Message,Application.Title);
                    end;
          end;
          end;
     end;
end;

procedure TSnapshotsForm.PanelRenameClick(Sender: TObject);
begin
  MainForm.Sound.Play(Sound_.stMenuSelect); MouseOverSoundEnabled:=False;
  if (Game=nil) and (MainForm.Game<>nil) then
     Game:=MainForm.Game;
  if Edit1.Visible then CloseEditor(False)
  else if (PanelName.Caption<>'') and PanelRename.Enabled then with Grid do begin
          SetFocus; ShowEditor;
          end;
end;

procedure TSnapshotsForm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if      Key=VK_RETURN then
          if Shift=[] then PanelOpenClick(Sender)
          else
  else if Key=VK_HOME   then
          Grid.Row:=Grid.FixedRows
  else if Key=VK_END    then
          Grid.Row:=Pred(Grid.RowCount)
  else if Key=Ord(ACCEL_CHAR_EXIT) then
          Close
  else if Key=VK_F1 then MainForm.BtnHelpClick(Sender)
  else;
end;

procedure TSnapshotsForm.GridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if      Key=VK_RETURN then
          if ssCtrl in Shift then PanelRenameClick(Sender)
          else
  else if ((Key=VK_LEFT) or (Key=VK_TAB)) and
          (Shift=[]) then MainForm.SetFocus
  else if (Key=VK_RIGHT) and (Shift=[]) then Close
  else;
end;

procedure TSnapshotsForm.InfoMemoExit(Sender: TObject);
begin
  if InfoMemo.Modified and // if the text has been modified then save it now
     (Snapshots[Grid.Row]<>nil) then with Snapshots[Grid.Row] do
     if Notes<>nil then begin
        InfoMemo.Modified:=False; Notes.Modified:=True;
        StringsToList(InfoMemo.Lines,Notes.Lines);
        end;
end;

procedure TSnapshotsForm.ShowEditor;
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

function  TSnapshotsForm.HideEditor:Boolean;
begin
  Result:=Edit1.Visible;
  Edit1.Hide;
end;

function  TSnapshotsForm.CloseEditor(HideFirst:Boolean):Boolean;
begin
  Result:=True;
  if Edit1.Visible then begin
     if HideFirst then Edit1.Hide;
     Result:=RenameSnapshot(Grid.Row,StrRemoveCharacters(Trim(Edit1.Text),TITLE_ILLEGAL_CHARACTERS));
     end;
  if Result then HideEditor;
end;

function  TSnapshotsForm.RenameSnapshot(GridIndex__:Integer; const NewName__:String):Boolean;
var i:Integer; CanSelect:Boolean; s,OldName,NewName:String; Snapshot:TSnapshot;
begin
  Snapshot:=Snapshots[GridIndex__];
  if (Snapshot<>nil) and
     PanelRename.Enabled and
     (Game<>nil) and
     (GridIndex__>=Grid.FixedRows+Game.BestSolutionsCount) then begin
     OldName:=Snapshot.Name;
     NewName:=TextThatCannotBeInterpretedAsMoves(StrRemoveCharacters(Trim(NewName__),TITLE_ILLEGAL_CHARACTERS));
     for i:=1 to Length(NewName) do
         if NewName[i]<SPACE then NewName[i]:=UNDERSCORE; {change control characters like TAB and LINEFEED to underscores}

     Result:=StrEqual(OldName,NewName)
             or
             (Game.Snapshots.GetItemByName(NewName)=nil);
     if   Result then
          Result:=Snapshot.Rename(NewName,s)
     else s:=Format(TEXT_NAME_ALREADY_EXISTS_FORMAT,[NewName]);
     if Result then with Grid do begin
        if OldName<>Snapshot.Name then Snapshot.Modified:=True;
        Cells[1,GridIndex__]:=Snapshot.Name;
        PanelName.Caption:=Cells[1,GridIndex__];
        GridSelectCell(Self,Col,GridIndex__,CanSelect);
        end
     else with Grid do begin
        Application.MessageBox(PChar(s),PChar(Caption),MB_OK+MB_ICONERROR);
        Edit1.Text:=Cells[1,GridIndex__]; Edit1.SelLength:=Length(Edit1.Text);
        Edit1.SetFocus;
        end;
     end
  else Result:=False;
end;

procedure TSnapshotsForm.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key in [{VK_RETURN,}VK_UP,VK_DOWN{,VK_HOME,VK_END}]) and
     RenameSnapshot(Grid.Row,Edit1.Text) then begin
     Grid.SetFocus;
     if Key<>VK_RETURN then PostMessage(Grid.Handle, WM_KEYDOWN, Key, 0);
     Key:=0;
     end;
end;

procedure TSnapshotsForm.Edit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key in [VK_RETURN{,VK_UP,VK_DOWN{,VK_HOME,VK_END}]) and
     RenameSnapshot(Grid.Row,Edit1.Text) then begin
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

procedure TSnapshotsForm.Edit1Enter(Sender: TObject);
begin //
  FormMouseMove(ActiveControl,[],0,0);
end;

procedure TSnapshotsForm.Edit1Exit(Sender: TObject);
begin
  CloseEditor(False);
  FormMouseMove(ActiveControl,[],0,0);
end;

procedure TSnapshotsForm.GridTopLeftChanged(Sender: TObject);
begin
  CloseEditor(True);
end;

procedure TSnapshotsForm.GridEnter(Sender: TObject);
begin
  GridScrollInView(Grid,Grid.Row);
end;

procedure TSnapshotsForm.GridPopupMenuPopup(Sender: TObject);
begin
  Game                                   :=MainForm.Game;
  MenuItemGridOpen               .Enabled:=PanelName.Caption<>'';
  MenuItemGridOpenView           .Enabled:=MenuItemGridOpen.Enabled
                                           and
                                           (MainForm.BtnSplitView.Enabled or (not MainForm.MultiView.IsEmpty));
                                           //and
                                           //(RectWidth (MainForm.MultiView.ClippingRect)>6*MAX_BOARD_WIDTH)  // just a ballpark limit based on the assumption that each square on the board should be at least 2x2 pixels + some slack for borders and headers; a calculation of an exact limit is too complicated and too much work
                                           //and
                                           //(RectHeight(MainForm.MultiView.ClippingRect)>6*MAX_BOARD_HEIGHT); // just a ballpark limit based on the assumption that each square on the board should be at least 2x2 pixels + some slack for borders and headers; a calculation of an exact limit is too complicated and too much work
  MenuItemGridRename             .Enabled:=MenuItemGridOpen.Enabled and ( Grid.Row>=Grid.FixedRows+BestSolutionsCount);
  MenuItemGridDelete             .Enabled:=MenuItemGridOpen.Enabled and ((Grid.Row>=Grid.FixedRows+BestSolutionsCount) or (AllowDeletionOfBestSolutions and (Grid.Row>=Grid.FixedRows)));
  MenuItemGridDeleteAll          .Enabled:=MenuItemGridOpen.Enabled and (ItemCount-BestSolutionsCount>0);
  MenuItemGridCopy               .Enabled:=MenuItemGridOpen.Enabled and (Grid.RowCount>Grid.FixedRows) and Assigned(Snapshots[Grid.Row]) and (Snapshots[Grid.Row].MoveCount>0);
  MenuItemGridPaste              .Enabled:=Clipboard.HasFormat(CF_TEXT) and Assigned(MainForm.GamePictures) and MainForm.GamePictures.Initialized;
  MenuItemGridConcatenate        .Enabled:=(Game<>nil) and Game.CanCombineCurrentPositionAndSnapshotToFormASolution(Snapshots[Grid.Row]);
  MenuItemGridMakeForwardSolution.Enabled:=(Game<>nil) and
                                           Assigned(Snapshots[Grid.Row]) and
                                           Snapshots[Grid.Row].ReverseMode and
                                           (Snapshots[Grid.Row].GameState=gsSolved) and
                                           (Snapshots[Grid.Row].MoveCount=Snapshots[Grid.Row].MoveTop) and
                                           (Snapshots[Grid.Row].MoveCount>0);
  MenuItemGridSort               .Enabled:=ItemCount-BestSolutionsCount>1;

  MenuItemGridOpen               .Hint   :=PanelOpen.Hint;
  MenuItemGridOpenView           .Hint   :=PanelOpenView.Hint;
end;

procedure TSnapshotsForm.MenuItemGridDeleteAllClick(Sender: TObject);
begin
  if Game<>nil then with Grid do begin
     Game.DeleteAllSnapshots;

     if        MainForm.GameViewer.IsFloorTileVisible then
               OpenForm.GameViewer.Modified:=True; // ensure that the board background is shown

     if        Game.BestSolutionsCount=0 then begin
               Clear;
               MainForm.SetFocus; // empty grid: keyboard commands should go to 'MainForm'
               end
     else if   Row>=FixedRows+Game.BestSolutionsCount then
               LoadSnapshots(Snapshots[FixedRows])
          else LoadSnapshots(Snapshots[Row]);
     end;
end;

procedure TSnapshotsForm.GridDragDrop(Sender, Source: TObject; X, Y: Integer);
var i,ACol,ARow:Integer; s:String; Snapshot:TSnapshot;
begin
 if      Source=MainForm.DrawGrid1 then begin
         EndDrag(True);
         PanelNewClick(Self);
         end
 else if (Sender=Grid) and (Source=Grid) then with Grid do begin
         MouseToCell(X,Y,ACol,ARow);
         if (ACol=1) and (ARow>=FixedRows) and
            (ARow<RowCount) and (ARow<>Row) and
            (Game<>nil) and (Game.Snapshots<>nil) then begin
            EndDrag(True);
            s:=Cells[1,Row]; Snapshot:=Snapshots[Row];
            if   ARow<Row then
                 for i:=Row downto Succ(ARow) do begin
                     Cells[1,i]   :=Cells[1,Pred(i)];
                     Snapshots[i]:=Snapshots[Pred(i)];
                     end
            else for i:=Row to     Pred(ARow) do begin
                     Cells[1,i]   :=Cells[1,Succ(i)];
                     Snapshots[i]:=Snapshots[Succ(i)];
                     end;
            Cells[1,ARow]:=s; Snapshots[ARow]:=Snapshot;
            GridScrollInView(Grid,ARow);

            if   ARow=FixedRows+BestSolutionsCount then
                 Game.Snapshots.MoveAfter(Snapshot,nil)
            else Game.Snapshots.MoveAfter(Snapshot,Snapshots[Pred(ARow)]);

            Row:=ARow;
            MainForm.MakeDynamicHints(nil);
            MainForm.ShowStatus;

            Snapshot:=TSnapshot(Game.Snapshots.Items);
            while Snapshot<>nil do begin {mark all snapshots as modified to ensure that the level is updated when it's closed}
              Snapshot.Modified:=True; Snapshot:=TSnapshot(Snapshot.Next);
              end;
            end
         else EndDrag(False);
         end
      else if Sender is TControl then
              TControl(Sender).EndDrag(False); // unknown sender/source
end;

procedure TSnapshotsForm.GridDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var ACol,ARow:Integer;
begin
  if   (Sender=Grid) and (Source=Grid) then with Grid do begin
       MouseToCell(X,Y,ACol,ARow);
       if   (ACol>=FixedCols) and (ARow>=FixedRows+BestSolutionsCount) then
            Accept:=True
       else Accept:=False;
       if   (ARow=TopRow) and (TopRow>FixedRows) then TopRow:=TopRow-1
       else if (ARow=(TopRow+VisibleRowCount-FixedRows)) and
               (TopRow+VisibleRowCount-FixedRows<RowCount-FixedRows) then
               TopRow:=TopRow+1;
       end
  else with MainForm do Accept:=(Source=DrawGrid1) and (TrackBox.X=0) and (TrackBox.Y=0);
end;

procedure TSnapshotsForm.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var ACol,ARow:Integer;
begin
  with Grid do begin
    MouseToCell(X, Y, ACol, ARow);
    if (ACol>=FixedCols) and (ARow>=FixedRows) and
       (ACol<ColCount) and (ARow<RowCount) then
       if      (Button=mbLeft) and (not Dragging) and
               (Cells[1,ARow]<>'') and
               (ARow>=FixedRows+BestSolutionsCount) then
               BeginDrag(False)
       else if (Button=mbRight) and (ARow<>Row) then
               Row:=ARow;
    end;
end;

procedure TSnapshotsForm.GridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  FormMouseMove(Sender,Shift,X,Y);
  if Screen.ActiveForm=Self then
     StatusBar1.{Panels[1].Text}SimpleText := GetLongHint(Application.Hint);
end;

procedure TSnapshotsForm.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with Grid               do if Dragging then EndDrag(False);
  with MainForm.DrawGrid1 do if Dragging then EndDrag(False);
end;

procedure TSnapshotsForm.ShowGame(GridIndex:Integer);
var oPlayerPos:TColRow; oBoxPos:TBoxPositions; oReverseMode:Boolean;
    Snapshot:TSnapshot;
begin
  Snapshot:=Snapshots[GridIndex];
  if      (Snapshot=nil) or (not MainForm.Initialized) then
          if   OpenForm.GameViewer.Initialized then begin
               if not PanelBoard.Visible then PanelBoard.Show;
               OpenForm.GameViewer.ShowBackground;
               end
          else PanelBoard.Hide
  else if Visible and (Game<>nil) then begin
          if not PanelBoard.Visible then PanelBoard.Show;
          oPlayerPos:=Game.PlayerPos; oBoxPos:=Game.BoxPos;
          oReverseMode:=Game.ReverseMode;
          try     Game.LoadBoxes(Snapshot.PlayerPos,PBoxPositions(Addr(Snapshot.BoxPos)),Snapshot.ReverseMode);

                  OpenForm.GameViewer.LoadGame(Game);

          finally Game.LoadBoxes(oPlayerPos,PBoxPositions(Addr(oBoxPos)),oReverseMode);
          end;
          end;
  if GridIndex>=Grid.FixedRows then GridScrollInView(Grid,GridIndex);
end;

function  TSnapshotsForm.GetSnapshots(GridIndex:Integer):TSnapshot;
begin
  with Grid do
    if   (GridIndex>=FixedRows) and (GridIndex<RowCount) then
         Result:=TSnapshot(Objects[1,GridIndex])
    else Result:=nil;
end;

procedure TSnapshotsForm.SetSnapshots(GridIndex:Integer; Snapshot:TSnapshot);
begin
  Grid.Objects[1,GridIndex]:=TObject(Snapshot);
end;

procedure TSnapshotsForm.SetGridColumnsWidth;
var i:Integer; s:String;
begin
  with Grid do begin
    s:=IntToStr(Max(999,RowCount))+SPACE+SPACE+SPACE;
    i:=Canvas.TextWidth(s);
    if i<>ColWidths[0] then ColWidths[0]:=i;
    i:=ClientWidth-ColWidths[0]-2*GridLineWidth;
    if i<>ColWidths[1] then ColWidths[1]:=i;
    //while (VisibleColCount<ColCount) and (ColWidths[1]>0) do ColWidths[1]:=Pred(ColWidths[1]);
    end;
end;

function  SortOnNames(a,b:Pointer):Integer;
begin
  Result:=AnsiCompareText(TSnapshot(PPointer(a)^).Text,TSnapshot(PPointer(b)^).Text);
end;

function  SortOnMoves(a,b:Pointer):Integer;
begin
  Result:=TSnapshot(PPointer(a)^).MoveCount-TSnapshot(PPointer(b)^).MoveCount;
  if Result=0 then Result:=SortOnNames(a,b);
end;

function  SortOnPushes(a,b:Pointer):Integer;
begin
  Result:=TSnapshot(PPointer(a)^).PushCount-TSnapshot(PPointer(b)^).PushCount;
  if Result=0 then Result:=SortOnNames(a,b);
end;

procedure TSnapshotsForm.MenuItemGridSortClick(Sender: TObject);
var i,Count:Integer; Node:TNode; Snapshot:TSnapshot; SortFun:TCompareFunction;
    Vector:PPNodeVector;
begin
  if      Sender=MenuItemGridSortMoves  then SortFun:=SortOnMoves
  else if Sender=MenuItemGridSortPushes then SortFun:=SortOnPushes
  else SortFun:=SortOnNames;
  if (Game<>nil) and
     (Game.Snapshots<>nil) and (Game.Snapshots.Count>1) then begin
     Snapshot:=Snapshots[Grid.Row];
     if Game.Snapshots.SaveToVector(Count,Vector) then
        try     QuickSort(Vector,Count,SizeOf(Vector^[Low(Vector^)]),SortFun);
        finally Game.Snapshots.LoadFromVector(Count,Vector);
        end;

     i:=Grid.FixedRows;
     Node:=Game.Snapshots.Items;
     while (Node<>nil) and (i<Grid.RowCount) and
           StrEqual(Node.Name,Snapshots[i].Name) do Inc(i);
     if i<Grid.RowCount then begin // new sorting order: mark all snapshots as modified, so they are saved later
        Node:=Game.Snapshots.Items;
        while Node<>nil do begin
              TSnapshot(Node).Modified:=True; Node:=Node.Next;
              end;
        LoadSnapshots(nil); // load the snapshots in the new sorting order
        end;

     Grid.Row:=IndexOf(Snapshot);
     ShowSnapshot(Grid.Row);
     end;
end;

procedure TSnapshotsForm.MenuItemGridConcatenateClick(Sender: TObject);
begin // Requires that selected snapshot match up with current position
  if (Game<>nil) and (Snapshots[Grid.Row]<>nil) then
     Game.CombineCurrentPositionAndSnapshotToFormASolution(Snapshots[Grid.Row]);
end;

procedure TSnapshotsForm.ImageBoardMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PanelOpenClick(Sender);
end;

procedure TSnapshotsForm.FormShow(Sender: TObject);
begin
  ShowOnStartUp:=False;
end;

procedure TSnapshotsForm.MenuItemGridCopyClick(Sender: TObject);
var Snapshot:TSnapshot;
begin
  if Grid.Row>=Grid.FixedRows then begin
     Snapshot:=Snapshots[Grid.Row];
     if Snapshot<>nil then with Snapshot do
        if   MoveCount>0 then begin
             if GetMovesAsText<>'' then begin
                Clipboard.AsText:=MovesAsText;
                StatusBar1.SimpleText:=SnapshotCopiedToClipboardText+COLON+SPACE+Name;
                Self.Refresh; SleepEx(STATUS_BAR_MESSAGE_PAUSE_MS,False);
                end;
             end
        else Msg(NoMovesFoundText,Self.Caption,MB_OK);
     end;
end;

procedure TSnapshotsForm.MenuItemGridPasteClick(Sender: TObject);
begin
  MainForm.BtnOpenClipBoardClick(Sender);
  if Screen.ActiveForm=Self then FormActivate(Sender);
end;

function TSnapshotsForm.GetItemCount:Integer;
begin
  with Grid do begin
    if        RowCount >Succ(FixedRows) then Result:=RowCount-FixedRows
    else if   (RowCount>FixedRows) and
              //Assigned(Snapshots[Grid.FixedRows]) and // it's probably not necessary to test both the snapshot and the name in the grid
              (Cells[1,Grid.FixedRows]<>'') then Result:=1
         else Result:=0;
    end;
end;

function TSnapshotsForm.IndexOf(Snapshot__:TSnapshot):Integer;
begin
  with Grid do
    for Result:=FixedRows to Pred(RowCount) do
        if Snapshots[Result]=Snapshot__ then exit; // quick-and-dirty exit: 'Result' contains the correct index
  Result:=-1; // not found
end;

function  TSnapshotsForm.LoadBestSolutions:Boolean;
var i:Integer; oSelectedSnapshot,v:TSnapshot;
begin
  Result:=True; Game:=MainForm.Game;
  with Grid do begin
    BestSolutionsCount:=Game.BestSolutionsCount;
    oSelectedSnapshot:=Snapshots[Row];
    if (RowCount<FixedRows+BestSolutionsCount) and (BestSolutionsCount<>0) then
       try    RowCount:=FixedRows+BestSolutionsCount;
       except on E:Exception do Result:=Error(E.Message,Application.Title);
       end;
    if Result then begin
       i:=0;
       if   MainForm.ShowSolutionMoves then v:=Game.BestSolutionMoves
       else v:=Game.BestSolutionPushes;
       if v<>nil then begin
          Cells    [0,i+FixedRows                  ]:=SnapshotTypeAbbreviationText[Game.SnapshotType(v)]{+SPACE};
          Cells    [1,i+FixedRows                  ]:=v.Name;
          Snapshots[  i+FixedRows                  ]:=v;
          if (v=oSelectedSnapshot) and (Row<>i+FixedRows) then
             Row:=i+FixedRows;
          Inc(i);
          end;
       if   not MainForm.ShowSolutionMoves then v:=Game.BestSolutionMoves
       else v:=Game.BestSolutionPushes;
       if v<>nil then begin
          Cells    [0,i+FixedRows                  ]:=SnapshotTypeAbbreviationText[Game.SnapshotType(v)]{+SPACE};
          Cells    [1,i+FixedRows                  ]:=v.Name;
          Snapshots[  i+FixedRows                  ]:=v;
          if (v=oSelectedSnapshot) and (Row<>i+FixedRows) then
             Row:=i+FixedRows;
          end;

       if   (oGridRow>=FixedRows) and (oGridRow<RowCount) then
            oGridCellStr:=Cells[1,oGridRow]
       else oGridCellStr:='';
       end
    else Clear;
    end;
end;

function TSnapshotsForm.DethronedSolutionName(const SolutionName__:String):String; {throws EOutOfMemory}
var i:Integer;
begin
  Result:=SolutionName__; i:=0;
  if      StrBeginsWith(Result,BestSolutionName                        ) then i:=Length(BestSolutionName                        )
  else if StrBeginsWith(Result,BestSolutionMovesName                   ) then i:=Length(BestSolutionMovesName                   )
  else if StrBeginsWith(Result,BestSolutionPushesName                  ) then i:=Length(BestSolutionPushesName                  )
  else if StrBeginsWith(Result,SNAPSHOT_TYPE_NAME[stBestSolution      ]) then i:=Length(SNAPSHOT_TYPE_NAME[stBestSolution      ])
  else if StrBeginsWith(Result,SNAPSHOT_TYPE_NAME[stBestSolutionMoves ]) then i:=Length(SNAPSHOT_TYPE_NAME[stBestSolutionMoves ])
  else if StrBeginsWith(Result,SNAPSHOT_TYPE_NAME[stBestSolutionPushes]) then i:=Length(SNAPSHOT_TYPE_NAME[stBestSolutionPushes]);
  if i<>0 then Result:=SnapshotsForm.SolutionName+Copy(Result,Succ(i),MaxInt);
end;

function  TSnapshotsForm.IsASolution(Snapshot__:TSnapshot):Boolean;
begin
  if Snapshot__<>nil then with Snapshot__ do
     Result:=(GameState=gsSolved)
             and
             (MoveCount=MoveTop)
             and
             ((MoveCount>0)
              or
              (MainForm.Game=nil)
              or
              (not MainForm.Game.SolutionsRequireAtLeastOnePush)
             )
  else Result:=False;
end;

{
function  TSnapshotsForm.RefreshSnapshots:Boolean;
var i,Delta,oBestSolutionsCount,oRow:Integer; B:Boolean;
begin
  Result:=True; Game:=MainForm.Game;
  with Grid do begin
    oRow:=Row; oBestSolutionsCount:=BestSolutionsCount;
    BestSolutionsCount:=Game.BestSolutionsCount;
    Delta:=BestSolutionsCount-oBestSolutionsCount;
    if Delta<>0 then
       try    if Delta<0 then begin // reduce the number of rows
                 for i:=FixedRows to Pred(RowCount) do begin
                     Cells[0,i]   :=IntToStr(Succ(i-FixedRows-BestSolutionsCount));
                     Cells[1,i]   :=Cells[1,Succ(i)];
                     Snapshots[i]:=Snapshots[Succ(i)];
                     end;
                 RowCount:=Max(Succ(FixedRows),RowCount+Delta);
                 end
              else begin // increase the number of rows
                 RowCount:=Max(Succ(FixedRows),RowCount+Delta);
                 for i:=Pred(RowCount) downto FixedRows+oBestSolutionsCount+Delta do begin
                     Cells[0,i]   :=IntToStr(Succ(i-FixedRows-BestSolutionsCount));
                     Cells[1,i]   :=Cells[1,i-Delta];
                     Snapshots[i]:=Snapshots[i-Delta];
                     end;
                 end;
              Result:=LoadBestSolutions;
       except on E:Exception do Result:=Error(E.Message,Application.Title);
       end;

    oGridRow:=-1;
    if Result then GridSelectCell(Self,1,Max(FixedRows,oRow+Delta),B);
    end;
  if not Result then Clear;
end;
}
procedure TSnapshotsForm.MenuItemGridMakeForwardSolutionClick(
  Sender: TObject);
var oCursor:TCursor; NewSnapshot,Snapshot:TSnapshot;
begin
  MainForm.Sound.Play(Sound_.stMenuSelect); MouseOverSoundEnabled:=False;
  if CloseEditor(False) then with Grid do
     if (PanelName.Caption<>'') and (Trim(PanelName.Caption)=Cells[1,Row]) and
        (Game<>nil) and (Game.Snapshots<>nil) and
        (Row>=FixedRows+Game.BestSolutionsCount)
        then begin
        Snapshot:=Snapshots[Row];

        if    (Snapshot<>nil) and Snapshot.ReverseMode and (Snapshot.GameState=gsSolved) then begin
              oCursor:=Screen.Cursor;
              try     Screen.Cursor:=crHourGlass;
                      if Game.MakeForwardSolutionFromBackwardSolution(OpenForm.Game,Snapshot,NewSnapshot) then
                         LoadSnapshots(NewSnapshot);
              finally Screen.Cursor:=oCursor;
                      MainForm.MakeDynamicHints(nil);
                      MainForm.ShowStatus;
              end;
              end;
        end;
end;

procedure TSnapshotsForm.OnFontChange;
begin
  StatusBar1.Font.Assign(Self.Font);
  PanelName.Font.Assign(Self.Font);
  PanelOpen.Font.Assign(Self.Font);
  PanelDelete.Font.Assign(Self.Font);
  PanelNew.Font.Assign(Self.Font);
  PanelRename.Font.Assign(Self.Font);
  InfoLabel.Font.Assign(Self.Font);
end;

end.

