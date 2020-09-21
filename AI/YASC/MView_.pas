unit MView_;

interface

uses Windows,Classes,Graphics,Controls,ExtCtrls,
     IniFile_,SokUtil_,SokFile_,SokGame_;

const // global constants
  MULTI_VIEW_INI_FILE_SECTION_NAME     = 'MultiView';
  MULTI_VIEW_ITEM_BOARD_BORDER_SIZE    = 6; {must be >=4 (for "TGameViewer.LoadGame"). the value must reflect the actual behavior of the 'GameViewer' component in 'GView_.pas', i.e., how it centers boards in the window and how it reserves a few pixels for a border; furthermore, there are the additional pixels reserved by the 'MultiView' component for drawing a frame around the selected view}

type
  TMultiViewItemPanel         = record
    Alignment                 : TAlignment;
    Caption                   : String;
    Enabled                   : Boolean;
    Rect                      : TRect;
  end;
  TMouseButtonSet             = set of TMouseButton;
  TMultiView                  = class; // forward declaration
  TMultiViewItem              = class; // forward declaration
  TMultiViewItemPanelType     = (mvipNone,mvipMenu,mvipCaption,mvipSplit,mvipMinimize,mvipMaximize,mvipClose);
  TMultiViewItemsSortCriteria = (scTop,scLeft);
  TSizeHandle                 = (shNull,
                                 shBottomLeft, shLeft,  shTopLeft,     shTop,
                                 shTopRight,   shRight, shBottomRight, shBottom);
  TSplitAxis                  = (saBest, saBiggest, saHorizontal, saVertical);
  TTreeHeights                = array[TDirection] of Integer;

  TMultiViewItem              = class(TNode)
  private
    PanelHeight:Integer;
    fPanelsRect:TRect;
    fVisible:Boolean;
    Owner:TMultiView;

    procedure   DrawShadowedFrame(const Rect__:TRect);
    function    GetMinHeight:Integer;
    function    GetMinWidth:Integer;

  protected
    BoardRect:TRect;
    SerialNo:TSerialNo;
    TreeHeights:TTreeHeights;

    procedure   SetVisible(Visible__:Boolean);

  public
    HasSecondaryMetricsCaption:Boolean;
    MaxRectLeavingOtherItemsUnchanged:TRect; // maximized rectangle without colliding with other multi view items
    MaxRectAfterSqueezingOtherItems:TRect; // maximized rectangle after squeezing any other multi view items
    Panels:array[TMultiViewItemPanelType] of TMultiViewItemPanel;
    Rect:TRect;
    Snapshot:TSnapshot;
    Tag:Integer;

    constructor Create(Owner__:TMultiView);
    destructor  Destroy; override;

    function    CalculatePanelHeight:Integer;
    function    CalculateMaxRect(SqueezeOtherItems__:Boolean):TRect;
    function    CanResize(const Rect__:TRect):Boolean;
    function    CanSplit(SplitAxis__:TSplitAxis):Boolean; virtual;
    procedure   DrawFrame;
    procedure   Hide;
    function    IntersectsAnotherItem(var Item__:TMultiViewItem):Boolean;
    function    IsNeighbor(Item__:TMultiViewItem):Boolean;
    function    MakeSnapshot:Boolean;
    function    Maximize(Open__:Boolean):Boolean;
    function    MouseToPanel(X__,Y__:Integer):TMultiViewItemPanelType;
    procedure   Open;
    function    Resize(const Rect__:TRect; Open__:Boolean):Boolean;
    function    SaveSnapshot(var Count__:Cardinal):Boolean;
    procedure   SetVisibleWithoutUpdatingScreen(Visible__:Boolean);
    function    ScreenToClient(const Point:TPoint):TPoint;
    procedure   ShowCaptionWithMetrics(CalculateSecondaryScoreMetricsTimeLimitEnabled__:Boolean);
    procedure   ShowCaptionWithSecondaryMetrics;
    procedure   ShowPanel(PanelType__:TMultiViewItemPanelType; const Caption__:String; Enabled__:Boolean; const Rect__:TRect; Alignment__:TAlignment);
    procedure   ShowPanels(const Caption__:String);
    function    SplitRect(SplitAxis__:TSplitAxis):TRect;

    property    MinHeight:Integer read GetMinHeight;
    property    MinWidth:Integer read GetMinWidth;
    property    PanelsRect:TRect read fPanelsRect;
    property    Visible:Boolean read fVisible write SetVisible;
  end;

  TDragRect=record
    DragOriginPoint:TPoint;
    DragPoint:TPoint;
    IsDragging:Boolean;
    IsSizing:Boolean;
    IsZooming:Boolean;
    DragRect:TRect;
    SizeHandle:TSizeHandle;
    SizeHandleCursor:TCursor;
  end;

  TMultiView=class
  private
    CaptionFontSize:Integer;
    DragRect:TDragRect;
    fDisappearedItemsCount:Cardinal;
    fIgnoreMouseUp:Boolean;
    fItems:TList;
    fSerialNo:TSerialNo;
    fHint:String;
    Image:TImage;
    MouseDownSet:TMouseButtonSet;
    MouseOverSoundEnabled:Boolean;

    procedure   CalculateMaximizedRectangles(Show__:Boolean);
    function    LookupSnapshot(ReverseMode__:Boolean; MoveCount__,MoveTop__:Integer; Moves__:PHistoryMoves; var Item__:TMultiViewItem):Boolean;
    function    MakeItem(var Item__:TMultiViewItem):Boolean;
    function    PointToItem(X__,Y__:Integer; var Item__:TMultiViewItem):Boolean;
    procedure   SetDefaultValues;
    procedure   SetHint(const Hint__:String);
    procedure   ShowDragRect;

  protected
    CalculateSecondaryScoreMetricsTimeMS:TTimeMS;
    fOpening:TMultiViewItem;
    fSelected:TMultiViewItem;
    FocusedItem:TMultiViewItem;
    FocusedItemPanelType:TMultiViewItemPanelType;

    function    FindDuplicateItem(ExceptItem__:TMultiViewItem; MoveCountMustMatch__,MoveTopMustMatch__,VisibleItemsOnly__:Boolean; var DuplicateItem__:TMultiViewItem):Boolean;
    function    GetNextSerialNo:TSerialNo;
    function    GetSpaceFromAnotherItem(Item__:TMultiViewItem):Boolean;
    function    IsDuplicateItem(Item__:TMultiViewItem; MoveCountMustMatch__,MoveTopMustMatch__,VisibleItemsOnly__:Boolean; var DuplicateItem__:TMultiViewItem):Boolean;
    function    ResizeItem(Item__:TMultiViewItem; const NewRect__:TRect):Boolean;
    procedure   SetFocusedPanel(Item__:TMultiViewItem; PanelType__:TMultiViewItemPanelType);
    procedure   SetSelected(Item__:TMultiViewItem);

    property    Hint:String             read fHint     write SetHint;

  public
    BackgroundColor:TColor;
    BackgroundTransparencyPct:Integer;
    ClippingRect:TRect;
    DoReportDisappearedItems:Boolean;
    Font:TFont;
    FocusedBackgroundColor:TColor;
    FocusedTextColor:TColor;
    LineColor:TColor;
    ShadowColor:TColor;
    TextColor:TColor;
    SplitAxis:TSplitAxis;

    constructor Create(Image__:TImage);
    destructor  Destroy; override;

    function    BiggestItem(ExceptItem__:TMultiViewItem):TMultiViewItem;
    procedure   Clear;
    function    CloseItem(Item__:TMultiViewItem; ItemDisappeared__:Boolean):Boolean;
    function    CombineSnapshotsToFormSolutions(DoIt__:Boolean):Integer;
    function    DeleteScreenRegionInformation(Snapshot__:TSnapshot):Boolean;
    function    DeleteScreenRegionInformationForAllSnapshots:Integer;
    function    GetItemBySerialNo(SerialNo__:TSerialNo):TMultiViewItem;
    procedure   HideDragRect;
    function    IsEmpty:Boolean;
    function    IsDragging:Boolean;
    function    IsSizing:Boolean;
    function    LoadSettingsFromIniFile(const IniFile: TIniFile; const Section: String): Boolean;
    function    LoadSnapshot(Snapshot__:TSnapshot):Boolean;
    function    LoadSnapshots:Boolean;
    function    MarkDuplicates:Boolean;
    function    OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var PreviousGameViewerMask__:Integer):Boolean;
    function    OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer):Boolean;
    function    OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer):Boolean;
    procedure   OnResize(MaximizeThisItemLast__:TMultiViewItem); // postcondition: 'CalculateTreeHeights()' has been called
    function    Refresh:Boolean;
    function    RefreshItem(Item__:TMultiViewItem):Boolean;
    function    ReportDisappearedItems(ReportDisappearedItems__:Boolean; const AdditionalText__:String):Boolean;

    function    SaveSettingsToIniFile(const IniFile: TIniFile; const Section: String): Boolean;
    function    SaveSnapshots:Boolean;
    procedure   SetCursor(X__,Y__:Integer);
    procedure   ShowBackground(const Rect__:TRect);
    procedure   Sort(SortCriteria__:TMultiViewItemsSortCriteria);
    function    SplitItem(Item__:TMultiViewItem; SplitAxis__:TSplitAxis; KeepSelectedItem__:Boolean):TMultiViewItem;

    property    DisappearedItemsCount:Cardinal read fDisappearedItemsCount write fDisappearedItemsCount; // items falling off the screen because there isn't room for them anymore
    property    IgnoreMouseUp__:Boolean read fIgnoreMouseUp write fIgnoreMouseUp; //"__" suffix: to ensure that "with ..." statements don't cause name clashes with "MainForm.IgnoreMouseUp"
    property    Items:TList             read fItems;
    property    NextSerialNo:TSerialNo  read GetNextSerialNo;
    property    Opening:TMultiViewItem  read fOpening;//  write fOpening;
    property    Selected:TMultiViewItem read fSelected write SetSelected;
  end;

implementation

uses SysUtils,Clipbrd,Forms,Sound_,Main_,Misc_,Text_,BitMap_,Status_,Snapshots_,GView_;

const
  DEFAULT_BACKGROUND_COLOR    : TColor = clBlack;
  DEFAULT_BACKGROUND_TRANSPARENCY_PCT  = 66;
  DEFAULT_CAPTION_FONT_SIZE            = 8;
  DEFAULT_FOCUSED_BACKGROUND_COLOR
                              : TColor = //clBlack;
                                         //clDkGray;
                                         //TColor((64)+(64 shl 8)+(64 shl 16));
                                         TColor((96)+(96 shl 8)+(96 shl 16));
  DEFAULT_FOCUSED_TEXT_COLOR  : TColor = //clLtGray;
                                         TColor((224)+(224 shl 8)+(224 shl 16));
  DEFAULT_LINE_COLOR          : TColor = TColor((160)+(160 shl 8)+(160 shl 16));
  DEFAULT_SHADOW_COLOR        : TColor = TColor((64)+(64 shl 8)+(64 shl 16));
  DEFAULT_TEXT_COLOR          : TColor = //clLtGray;
                                         //TColor((160)+(160 shl 8)+(160 shl 16));
                                         TColor((176)+(176 shl 8)+(176 shl 16));
  MAX_CALCULATE_SECONDARY_SCORE_METRICS_TIME_MS {if it takes too long time to calculate the secondary score metrics, then don't do it for the selected view where the user is playing the game}
                                       = 3; {milliseconds}
  MAX_MULTI_VIEW_ITEMS                 = 50; // a reasonably small limit is required because the multi-view machinery uses simple O(n^2) and maybe even O(n^3) algorithms, and there are resursive calls too, which must not overflow the stack
//MULTI_VIEW_ITEM_BOARD_BORDER_SIZE    = 6;
  MIN_BOARD_SQUARE_SIZE                = 2; // minimum width and height in pixels for each square on the board
  MIN_MULTI_VIEW_ITEM_CAPTION_HEIGHT   = 26;
  SIZE_HANDLE_RECT_DELTA               = 8;

  ViewItemPanelCaptionClose            ='X';
  ViewItemPanelCaptionMaximize         ='+';
  ViewItemPanelCaptionMinimize         ='-';
  ViewItemPanelCaptionSplit            ='2';
  ViewItemPanelCaptionMenu             ='V';

var
  MultiViewItemsSortCriteria           : TMultiViewItemsSortCriteria = scTop;

function  IsSizeHandleCursor(Cursor__:TCursor):Boolean;
begin
  Result:=(Cursor__=crSizeNWSE) or (Cursor__=crSizeNS) or (Cursor__=crSizeNESW) or (Cursor__=crSizeWE);
end;

function  SizeHandleToCursor(SizeHandle__:TSizeHandle):TCursor;
begin
  case SizeHandle__ of
    shTopLeft, shBottomRight: Result := crSizeNWSE;
    shTop, shBottom         : Result := crSizeNS;
    shTopRight, shBottomLeft: Result := crSizeNESW;
    shRight, shLeft         : Result := crSizeWE;
    else                      Result := MainForm.Image1.Cursor;
  end; // case
end;

function SizeHandleResize(IsSwapEnabled__:Boolean; var SizeHandle:TSizeHandle; X,Y:Integer; var ControlPoint:TPoint; var R:TRect):Integer;
const HorizontallyOppositeSizeHandle : array[TSizeHandle] of TSizeHandle =
        (shNull,shTopLeft    ,shLeft ,shBottomLeft,shBottom,
                shBottomRight,shRight,shTopRight  ,shTop);
      VerticallyOppositeSizeHandle   : array[TSizeHandle] of TSizeHandle =
        (shNull,shBottomRight,shRight,shTopRight ,shTop,
                shTopLeft    ,shLeft,shBottomLeft,shBottom);
var DX, DY : Integer;

  procedure SwapInteger(var A, B: Integer);
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

  if IsSwapEnabled__ then with R do
     begin if Top  > Bottom then begin SwapInteger(Top, Bottom);
                                       SizeHandle:=HorizontallyOppositeSizeHandle[SizeHandle];
                                 end;
           if Left > Right  then begin SwapInteger(Left, Right);
                                       SizeHandle:=VerticallyOppositeSizeHandle[SizeHandle];
                                 end;
     end;

  Result := Abs(DX) + Abs(DY);
end;

function  PointToSizeHandle(const P:TPoint; const Rect:TRect; Delta:Integer):TSizeHandle;
begin
  with Rect do with P do
    if //(X>Left-Delta) and (X<Right+Delta)
       //and
       //(Y>Top -Delta) and (Y<Bottom+Delta)
       (X>Left) and (X<Right ) and
       (Y>Top ) and (Y<Bottom) // only accept points inside the rectangle
       then
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

function  MultiViewItemsCompareFunction(Item1,Item2:Pointer):Integer;
begin
  with TMultiViewItem(Item1).Rect do begin
     if        MultiViewItemsSortCriteria=scTop then
               Result:=Top-TMultiViewItem(Item2).Rect.Top
     else if   MultiViewItemsSortCriteria=scLeft  then
               Result:=Left-TMultiViewItem(Item2).Rect.Left
          else Result:=0;
     if   Result=0 then begin
          Result:=Top-TMultiViewItem(Item2).Rect.Top;
          if Result=0 then begin
             Result:=Left-TMultiViewItem(Item2).Rect.Left;
             if Result=0 then begin
                Result:=Right-TMultiViewItem(Item2).Rect.Right;
                if Result=0 then begin
                   Result:=Bottom-TMultiViewItem(Item2).Rect.Bottom;
                   end;
                end;
             end;
          end;
     end;
end;

function  IntervalIntersection(const Start1__,End1__,Start2__,End2__:Integer; var IntersectionStart__,IntersectionEnd__:Integer):Boolean;
begin
//Result:=((Start1__<End2__) and (End1__>Start2__));
  IntersectionStart__:=Max(Start1__,Start2__);
  IntersectionEnd__  :=Min(End1__  ,End2__  );
  Result:=IntersectionStart__<IntersectionEnd__;
end;

constructor TMultiViewItem.Create(Owner__:TMultiView);
begin
  Inherited Create;
  HasSecondaryMetricsCaption:=False; PanelHeight:=0; Owner:=Owner__; Snapshot:=nil; Tag:=0;
  FillChar(BoardRect,SizeOf(BoardRect),0);
  FillChar(MaxRectLeavingOtherItemsUnchanged,SizeOf(MaxRectLeavingOtherItemsUnchanged),0);
  FillChar(MaxRectAfterSqueezingOtherItems,SizeOf(MaxRectAfterSqueezingOtherItems),0);
  FillChar(Panels,SizeOf(Panels),0);
  FillChar(fPanelsRect,SizeOf(fPanelsRect),0);
  FillChar(Rect,SizeOf(Rect),0);
  FillChar(TreeHeights,SizeOf(TreeHeights),0);
  SerialNo:=Owner.NextSerialNo;
  Text:='View '+IntToStr(SerialNo);
end;

destructor  TMultiViewItem.Destroy;
begin
  if Self=Owner.Selected    then Owner.Selected:=nil;
  if Self=Owner.FocusedItem then Owner.SetFocusedPanel(nil,mvipNone);
  if Visible then Hide;
  Snapshot.Free;
  Inherited;
end;

function  TMultiViewItem.CalculatePanelHeight:Integer;
var Size1,Size2,Size3:TSize;
begin
  with Owner.Image.Picture.BitMap.Canvas do begin
    if Owner.Image.Tag<>LongInt(Owner) then begin
       Owner.Image.Tag:=LongInt(Owner);
       Owner.Image.Picture.BitMap.Canvas.Font.Assign(Owner.Font);
       end;
    Size1:=Owner.Image.Picture.BitMap.Canvas.TextExtent(Name);
    Size2:=Owner.Image.Picture.BitMap.Canvas.TextExtent(ViewItemPanelCaptionClose);
    Size3:=Owner.Image.Picture.BitMap.Canvas.TextExtent(ViewItemPanelCaptionSplit);
    Result:=Max(0,Min(RectHeight(Rect)-4,Max(MIN_MULTI_VIEW_ITEM_CAPTION_HEIGHT,Max(Max(Size1.cy,Max(Size2.cy,Size3.cy)),Max(Size2.cx,Size3.cx))+8))); {'+8': shadowed frame + top and bottom text margin}
    end;
end;

function  TMultiViewItem.CalculateMaxRect(SqueezeOtherItems__:Boolean):TRect;
var {i,Count,}IntersectionStart,IntersectionEnd:Integer; Item:TMultiViewItem;
begin // calculates the largest rectangle on the screen for the item, either with or without squeezing the other items
      // precondition: if 'SqueezeOtherItems__' is 'True' then 'Owner.CalculateMaximizedRectangles()' must have calculated the 'tree height' for all view items for all directions
  if SqueezeOtherItems__ then begin
     Result:=Owner.ClippingRect;
     Inc(Result.Left  ,MinWidth *Pred(TreeHeights[Left ])); // 'Pred': don't count the item itself; each of the other items in this direction require at least the minimum width
     Inc(Result.Top   ,MinHeight*Pred(TreeHeights[Up   ]));
     Dec(Result.Right ,MinWidth *Pred(TreeHeights[Right]));
     Dec(Result.Bottom,MinHeight*Pred(TreeHeights[Down ]));
     end
  else begin
     if   (RectHeight(Rect)>0) and (RectWidth(Rect)>0) then
          Result:=Owner.ClippingRect
     else FillChar(Result,SizeOf(Result),0); // special: if the view item has a degenerated rectangle size, then return a zero-filled rectangle
     Item:=TMultiViewItem(Owner.Items.First);
     while Assigned(Item) do begin
       if Item<>Self then begin
          if IntervalIntersection(Item.Rect.Left,Item.Rect.Right,Self.Rect.Left,Self.Rect.Right,IntersectionStart,IntersectionEnd) then begin // 'True': horizontal overlap
             if Item.Rect.Bottom    <=Self.Rect.Top    then // '<=' item above self
                Result.Top          :=Max(Result.Top   ,Item.Rect.Bottom);
             if Item.Rect.Top       >=Self.Rect.Bottom then // '>=' item below self
                Result.Bottom       :=Min(Result.Bottom,Item.Rect.Top);
             end;
          if IntervalIntersection(Item.Rect.Top,Item.Rect.Bottom,Self.Rect.Top,Self.Rect.Bottom,IntersectionStart,IntersectionEnd) then begin // 'True': vertical overlap
             if Item.Rect.Right     <=Self.Rect.Left   then // '<=' item to the left  of self
                Result.Left         :=Max(Result.Left  ,Item.Rect.Right);
             if Item.Rect.Left      >=Self.Rect.Right  then // '>=' item to the right of self
                Result.Right        :=Min(Result.Right ,Item.Rect.Left);
             end;
          end;
       Item:=TMultiViewItem(Item.Next);
       end;
     end;
end;

function  TMultiViewItem.CanResize(const Rect__:TRect):Boolean;
begin
    Result:=(RectWidth (Rect__) >=MinWidth) and
            (RectHeight(Rect__) >=MinHeight) and
            (Rect__.Left        >=Owner.ClippingRect.Left  ) and
            (Rect__.Top         >=Owner.ClippingRect.Top   ) and
            (Rect__.Right       <=Owner.ClippingRect.Right ) and
            (Rect__.Bottom      <=Owner.ClippingRect.Bottom);
end;

function  TMultiViewItem.CanSplit(SplitAxis__:TSplitAxis):Boolean;
begin
  Result:=CanResize(SplitRect(SplitAxis__))
          and
          (Owner.Items.Count<MAX_MULTI_VIEW_ITEMS);
end;

procedure TMultiViewItem.DrawFrame;
begin
  with Rect do
    DrawShadowedFrame(Classes.Rect(Left+2,Top+1+PanelHeight,Right-2,Bottom-2));
end;

procedure TMultiViewItem.DrawShadowedFrame(const Rect__:TRect);
var R:TRect;
begin
  with Owner.Image.Picture.BitMap.Canvas do begin
    Brush.Style:=bsSolid;
    Brush.Color:=Owner.ShadowColor;
    with Rect__ do R:=Classes.Rect(Succ(Left),Succ(Top),Succ(Right),Succ(Bottom));
    Misc_.ClipRect(R,Rect);
    FrameRect(R); // shadow frame

    //if Self=Owner.Selected then Brush.Color:=Owner.FocusedTextColor
    //else
    Brush.Color:=Owner.LineColor;
    R:=Rect__;
    Misc_.ClipRect(R,Rect);
    FrameRect(R); // frame

    Pen.Mode:=pmCopy; Pen.Style:=psSolid; Pen.Color:=Brush.Color; // side-effect: set brush color as well as pen color to the line color in use for this item
    end;
end;

function  TMultiViewItem.GetMinHeight:Integer;
begin {precondition: 'PanelHeight' has been calculated}
  Result:=Max(MIN_BOARD_SQUARE_SIZE * MainForm.Game.BoardHeight, 2 * MAX_BOARD_HEIGHT)
          + 2 * MULTI_VIEW_ITEM_BOARD_BORDER_SIZE + PanelHeight + 6 + 8;
end;

function  TMultiViewItem.GetMinWidth:Integer;
begin {precondition: 'PanelHeight' has been calculated}
  Result:=5*PanelHeight + 0; // 5 buttons on the title bar (menu, split, minimize, maximize, and close) + the exact number of extra pixels for border and separators, if any
end;

procedure TMultiViewItem.Hide;
begin
  if Visible then begin
     Misc_.ClipRect(Rect,Owner.ClippingRect);
     Owner.ShowBackground(Rect);
     fVisible:=False;
     if Self=Owner.FocusedItem then Owner.SetFocusedPanel(nil,mvipNone);
     end;
end;

function  TMultiViewItem.IntersectsAnotherItem(var Item__:TMultiViewItem):Boolean;
var IntersectionRect:TRect;
begin // returns 'True' if the item intersects one or more items; the first found, if any, is returned in 'Item__'
  Result:=False;
  Item__:=TMultiViewItem(Owner.Items.First);
  while Assigned(Item__) and (not Result) do begin
    if   (Item__<>Self) and IntersectRect(InterSectionRect,Item__.Rect,Self.Rect) then
         Result:=True
    else Item__:=TMultiViewItem(Item__.Next);
    end;
end;

function  TMultiViewItem.IsNeighbor(Item__:TMultiViewItem):Boolean;
var IntersectionStart,IntersectionEnd:Integer;
begin // returns 'True' if 'Item__' touches 'Self'
  Result:=False;
  if IntervalIntersection(Item__.Rect.Left,Item__.Rect.Right ,Self.Rect.Left,Self.Rect.Right,IntersectionStart,IntersectionEnd) then begin // 'True': horizontal overlap
     Result:=(Item__.Rect.Bottom    =Self.Rect.Top)  or  // '<=' item above self
             (Item__.Rect.Top       =Self.Rect.Bottom ); // '>=' item below self
     end;
  if IntervalIntersection(Item__.Rect.Top ,Item__.Rect.Bottom,Self.Rect.Top,Self.Rect.Bottom,IntersectionStart,IntersectionEnd) then begin // 'True': vertical overlap
     Result:=(Item__.Rect.Right     =Self.Rect.Left) or  // '<=' item to the left  of self
             (Item__.Rect.Left      =Self.Rect.Right);   // '>=' item to the right of self
     end;
end;

function  TMultiViewItem.MakeSnapshot:Boolean;
var NewSnapshot:TSnapshot;
begin // makes a snapshot of the current game state ('Mainform.Game') and stores it in this view item
  Result:=Assigned(MainForm.Game) and MainForm.Game.IsIdleAndStopReplayingAndBrowsing;
  if Result then begin
     NewSnapshot:=MainForm.Game.MakeSnapshot('');
     Result:=Assigned(NewSnapshot);
     if Result then begin
        Snapshot.Free;
        Snapshot:=NewSnapshot;
        end
     end;
end;

function  TMultiViewItem.Maximize(Open__:Boolean):Boolean; // note that calling 'Maximize' may destroy the item; that happens if there isn't room for the item on the screen anymore
begin // returns 'False' if the item is destroyed instead of being shown maximized on the screen; the item is destroyed if there isn't room for it on the screen anymore
  ClipRect(Rect,Owner.ClippingRect);
  MaxRectLeavingOtherItemsUnchanged:=CalculateMaxRect(False);
  with Rect do
    Result:=(Left  =MaxRectLeavingOtherItemsUnchanged.Left) and
            (Top   =MaxRectLeavingOtherItemsUnchanged.Top) and
            (Right =MaxRectLeavingOtherItemsUnchanged.Right) and
            (Bottom=MaxRectLeavingOtherItemsUnchanged.Bottom) and
            CanResize(Rect); // 'CanResize': the current rectangle may be too small; this can happen when the window is resized and the item has been clipped to the new clipping rectangle

  if Result then begin
     if Open__ then Open;
     end
  else begin // the item changes size or it must change size (because 'CanResize()' returned 'False')
     Result:=Resize(MaxRectLeavingOtherItemsUnchanged,Open__);
     if not Result then Owner.CloseItem(Self,True); // the item doesn't fit on the screen anymore; destroy it
     end;
end;

function  TMultiViewItem.MouseToPanel(X__,Y__:Integer):TMultiViewItemPanelType;
var Point:TPoint;
begin
  Point.X:=X__; Point.Y:=Y__;
  for Result:=Low(Result) to High(Result) do
      if PtInRect(Panels[Result].Rect,Point) then exit; // 'exit': quick and dirty exit when a match has been found
  Result:=mvipNone;
end;

procedure TMultiViewItem.Open;
var OldOpening,OldSelected:TMultiViewItem;
begin
  if not MainForm.ShutDownApplication then with Owner.Image.Picture.BitMap.Canvas do begin
     OldOpening:=Owner.fOpening; OldSelected:=Owner.Selected;
     try
       Hide;
       PanelHeight:=CalculatePanelHeight;
       Misc_.ClipRect(Rect,Owner.ClippingRect);
       if   Self<>Owner.Selected then
            Owner.fSelected:=nil // 'nil': so 'MainForm.ShowStatus' doesn't update the caption for the currently selected view before the positions of its panels have been updated
       else Panels[mvipSplit].Enabled:=CanSplit(Owner.SplitAxis); // so 'MainForm.SetStatus' can set the correct status for the 'split view' button in the main window
       Owner.fOpening:=Self;

       MainForm.GameViewer.SetWindow(Owner.Image.Picture.BitMap.Canvas,
                                     Classes.Rect(Rect.Left +MULTI_VIEW_ITEM_BOARD_BORDER_SIZE,Rect.Top   +MULTI_VIEW_ITEM_BOARD_BORDER_SIZE+PanelHeight,
                                                  Rect.Right-MULTI_VIEW_ITEM_BOARD_BORDER_SIZE,Rect.Bottom-MULTI_VIEW_ITEM_BOARD_BORDER_SIZE));

       if   (MainForm.Game.FileName<>'') and
            MainForm.Game.LoadSnapshot(Snapshot) and
            MainForm.InitGame(False,False,False,False,False,True,True,0) then begin
            BoardRect:=MainForm.GameViewer.BoardRect;
            end
       else FillChar(BoardRect,SizeOf(BoardRect),0);

       fVisible:=True;
       if   Assigned(Snapshot) then begin
            ShowPanels(Format(FORMAT_MOVES_AND_PUSHES,[Snapshot.MoveCount,Snapshot.PushCount])+
                       SecondaryMetricsFormattedAsATitleSuffix(MainForm.Game.SecondaryMetricsInTitles,Snapshot.SecondaryScoreMetrics));
            HasSecondaryMetricsCaption:=MainForm.Game.SecondaryMetricsInTitles;
            end
       else ShowPanels('');

       if Self=Owner.Selected then
          DrawFrame;
     finally
       Owner.fOpening:=OldOpening; Owner.fSelected:=OldSelected;
     end;
     end;
end;

function  TMultiViewItem.Resize(const Rect__:TRect; Open__:Boolean):Boolean;
var OldRect:TRect;
begin
    Result:=CanResize(Rect__);
    if Result then begin
       Hide;
       OldRect:=Rect;
       Rect:=Rect__;
       ClipRect(Rect,Owner.ClippingRect);
       if not IsEqualRects(OldRect,Rect) and
          (RectWidth(OldRect)*RectHeight(OldRect)<>0) and // '0': the old region is empty the first time the item is processed
          Assigned(Snapshot) and Assigned(Snapshot.Notes) then
          Snapshot.Notes.Modified:=True; // mark notes as modified instead of the snaphot itself; it signals that the snapshot itself is intact, in case that information comes in handy at some time
       if Open__ then Open;
       end;
end;

function  TMultiViewItem.SaveSnapshot(var Count__:Cardinal):Boolean;
var Index:Integer; CanSelect:Boolean; s:String; ASnapshot:TSnapshot;
begin // gives the snapshot away to the game, i.e., transfers the snapshot to the 'MainForm.Game.Snapshots' list
  Result:=Assigned(Self.Snapshot) and Assigned(MainForm.Game);
  if Result
     and
     (Self.Snapshot.MoveCount>Self.Snapshot.ForcedInitialJumps) then begin // '>': only really save the snapshot if it has enough moves to be worth it
     Index:=-1;
     with Self.Snapshot do begin
       if Assigned(SnapshotsForm) and
          MainForm.Game.LookupSnapshot(ReverseMode,MoveCount,MoveTop,Moves,ASnapshot) then begin // 'True': an identical snapshot already exists
          Index:=SnapshotsForm.IndexOf(ASnapshot); // look up the existing snapshot in the grid in the "Snapshots" window
          if Index>=0 then Owner.DeleteScreenRegionInformation(ASnapshot);
          end;

       if Index<0 then begin // 'True': it's a new snapshot, i.e., it's not a duplicate of an existing snapshot
          if        GameState=gsSolved then
                    if   ReverseMode then
                         s:=TEXT_REVERSE_MODE+SPACE+TEXT_SOLUTION // there is no user-defined text for this one
                    else s:=MainForm.Game.SnapshotTypeName(SokFile_.stSolution)
          else if   ReverseMode then
                    s:=MainForm.Game.SnapshotTypeName(stReverseSnapshot)
               else s:=MainForm.Game.SnapshotTypeName(stSnapshot);
          s:=MainForm.Game.Snapshots.MakeUniqueName(s+SPACE+
                                                    Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
                                                    SecondaryMetricsFormattedAsATitleSuffix(MainForm.Game.SecondaryMetricsInTitles,SecondaryScoreMetrics),'',True);
          Self.Snapshot.SetName(s);
          Self.Snapshot.Notes.Lines.DeleteKey(TEXT_SCREEN_REGION);
          Self.Snapshot.SnapshotTag:=0;
          end;
       end;

     ASnapshot:=Self.Snapshot; Self.Snapshot:=nil; // steal the snapshot from the view item
     if   Index<0 then begin                       // 'True': it's a new snapshot, i.e., it's not a duplicate of an existing snapshot
          MainForm.Game.Snapshots.Add(ASnapshot);  // add the snapshot to the game
          ASnapshot.Modified:=True;                // mark the snapshot as 'modified' so the level is saved when it's closed
          if Count__<High(Count__) then Inc(Count__);
          end
     else begin ASnapshot.Free;                    // destroy the duplicate
          end;

     if Assigned(SnapshotsForm) then begin
        if      Index<0 then                       // 'True': it's a new snapshot, i.e., it's not a duplicate of an existing snapshot
                SnapshotsForm.PanelNewClick (MainForm.MultiView)
        else    SnapshotsForm.GridSelectCell(MainForm.MultiView,1,Index,CanSelect);

        if      SnapshotsForm.Visible then with SnapshotsForm do begin
                if WindowState=wsMinimized then WindowState:=wsNormal;
                end
        else if Screen.ActiveForm=MainForm then begin
                MainForm.ShowSnapshots(True,False,SnapshotsForm.Grid.Row);
                end;

        MainForm.BringToFront;
        MainForm.MakeDynamicHints(nil);
        MainForm.ShowStatus;
        end;
     end;
end;

function  TMultiViewItem.ScreenToClient(const Point:TPoint):TPoint;
begin
  Result:=Classes.Point(Point.X-Rect.Left,Point.Y-Rect.Top);
end;

procedure TMultiViewItem.SetVisible(Visible__:Boolean);
begin
  if Visible__<>Visible then
     if   Visible__ then Open
     else Hide;
end;

procedure TMultiViewItem.SetVisibleWithoutUpdatingScreen(Visible__:Boolean);
begin
  fVisible:=Visible__;
end;

procedure TMultiViewItem.ShowCaptionWithMetrics(CalculateSecondaryScoreMetricsTimeLimitEnabled__:Boolean);
var TimeMS:TTimeMS; SecondaryScoreMetrics:TSecondaryScoreMetrics;
begin
  with Panels[mvipCaption] do begin
    if        Self=Owner.Selected then with MainForm.Game.History do begin
              Caption:=Format(FORMAT_MOVES_AND_PUSHES,[Count,PushCount]);
              HasSecondaryMetricsCaption:=MainForm.Game.SecondaryMetricsInTitles
                                          and
                                          (not (MainForm.Game.IsBusy or MainForm.Game.IsReplaying or MainForm.Game.IsBrowsing))
                                          and
                                          ((not CalculateSecondaryScoreMetricsTimeLimitEnabled__)
                                           or
                                           (Owner.CalculateSecondaryScoreMetricsTimeMS<=MAX_CALCULATE_SECONDARY_SCORE_METRICS_TIME_MS) // if it earlier has taken too long time to calculate the secondary score metrics, then don't do it anymore
                                          );
              if HasSecondaryMetricsCaption then begin
                 TimeMS:=GetTimeMS;
                 MainForm.Game.CalculateSecondaryScoreMetrics(SecondaryScoreMetrics);
                 Caption:=Caption+SecondaryMetricsFormattedAsATitleSuffix(True,SecondaryScoreMetrics);
                 Owner.CalculateSecondaryScoreMetricsTimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS);
                 end;
              end
    else if   Assigned(Snapshot) then with Snapshot do begin
              Caption:=Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
                       SecondaryMetricsFormattedAsATitleSuffix(MainForm.Game.SecondaryMetricsInTitles,SecondaryScoreMetrics);
              HasSecondaryMetricsCaption:=MainForm.Game.SecondaryMetricsInTitles;
              end
         else begin Caption:='';
                    HasSecondaryMetricsCaption:=False;
              end;
    ShowPanel(mvipCaption,Caption,Enabled,Rect,Alignment);
    end;
end;

procedure TMultiViewItem.ShowCaptionWithSecondaryMetrics;
begin // the procedure only updates the caption if it doesn't contain secondary score metrics
  if (not HasSecondaryMetricsCaption) and
     MainForm.Game.SecondaryMetricsInTitles then
     ShowCaptionWithMetrics(False);
end;

procedure TMultiViewItem.ShowPanel(PanelType__:TMultiViewItemPanelType; const Caption__:String; Enabled__:Boolean; const Rect__:TRect; Alignment__:TAlignment);
var H,W:Integer; FormatOptions:Cardinal; R,R1,R2:TRect;

  procedure DrawIcon(PanelType__:TMultiViewItemPanelType; const Rect__:TRect);
  var a,b,Width,Height:Integer; P:TPoint; R:TRect;

    procedure DrawBox(X__,Y__,Width__,Height__:Integer);
    var R:TRect;
    begin // draws a box with a a 4-pixel top line and 2-pixel side lines and bottom line
      with Owner.Image.Picture.BitMap.Canvas do begin
        R.Left:=X__; R.Top:=Y__; R.Right:=X__+Width__; R.Bottom:=Y__+Height;
        FrameRect(R);
        Inc(R.Left); Inc(R.Top); Dec(R.Right); Dec(R.Bottom);
        FrameRect(R);
        Inc(R.Top); MoveTo(R.Left,R.Top); LineTo(R.Right,R.Top);
        Inc(R.Top); MoveTo(R.Left,R.Top); LineTo(R.Right,R.Top);
        end;
    end;

    procedure DrawCross(X__,Y__,Left__,Right__:Integer);
    begin
      with Owner.Image.Picture.BitMap.Canvas do begin
        MoveTo(X__-Left__,Y__-Left__);   LineTo(X__+Right__,Y__+Right__);
        MoveTo(X__-Left__,Y__-Left__+1); LineTo(X__+Right__-1,Y__+Right__);
        MoveTo(X__-Left__+1,Y__-Left__); LineTo(X__+Right__,Y__+Right__-1);

        MoveTo(X__-Left__,Y__+Left__);   LineTo(X__+Right__,Y__-Right__);
        MoveTo(X__-Left__,Y__+Left__-1); LineTo(X__+Right__-1,Y__-Right__);
        MoveTo(X__-Left__+1,Y__+Left__); LineTo(X__+Right__,Y__-Right__+1);
        end;
    end;

    procedure DrawDownArrow(X__,Y__,Height__:Integer);
    var i:Integer;
    begin
      with Owner.Image.Picture.BitMap.Canvas do
        for i:=1 to Height__ do begin
            MoveTo(X__,Y__); LineTo(X__+Pred(2*i),Y__);
            Dec(X__); Dec(Y__);
            end;
    end;

  begin // 'DrawIcon'
    with Owner.Image.Picture.BitMap.Canvas do with R do with P do begin
      R:=Rect__;
      Width:=Min(RectWidth(R),RectHeight(R)); Height:=Width; // ensure that the rectangle is a square
      Right:=Left+Width; Bottom:=Top+Height;

      if Panels[PanelType__].Enabled and (Width>=16) then begin

         Brush.Color:=Owner.ShadowColor;
         Pen.Color:=Brush.Color;

         Width:=Min(RectWidth(R)-6,18); Height:=Width;
         X:=Left+((RectWidth(R)-Width) div 2); // 'Point.X' = left
         Y:=Top +(X-Left)+Height; // 'Point.Y' = bottom; most icons are aligned at the bottom

         case PanelType__ of
           mvipMenu    : begin
                           Height:=Min(Min(Width-4,Height-4),8);
                           X:=Left+(RectWidth (R) div 2); //
                           Y:=Succ(Top) +(RectHeight(R) div 2)+(Succ(Height) div 2);
                           DrawDownArrow(Succ(X),Y,Height);

                           if   (Self=Owner.FocusedItem) and (PanelType__=Owner.FocusedItemPanelType) then
                                Brush.Color:=Owner.FocusedTextColor
                           else Brush.Color:=Owner.TextColor;
                           Pen.Color:=Brush.Color;

                           DrawDownArrow(X,Pred(Y),Height);
                         end;
           mvipSplit,
           mvipMinimize,
           mvipMaximize: begin
                           if      PanelType__=mvipSplit    then begin
                                   Width:=Min(Width,18);
                                   if Odd(Width) then Inc(Width);
                                   Height:=Min(Height,14);
                                   end
                           else if PanelType__=mvipMinimize then begin
                                   Width:=Min(Width, 10); Height:=Width;
                                   end;

                           X:=Left+((RectWidth(R)-Width) div 2); // center horizontally
                           Dec(Y,Height); // align at the bottom

                           DrawBox(Succ(X),Succ(Y),Width,Height);

                           if PanelType__=mvipSplit then begin
                              MoveTo(X+Succ(Width div 2),Y); LineTo(X+Succ(Width div 2),Y+Height);
                              end;

                           if   (Self=Owner.FocusedItem) and (PanelType__=Owner.FocusedItemPanelType) then
                                Brush.Color:=Owner.FocusedTextColor
                           else Brush.Color:=Owner.TextColor;
                           Pen.Color:=Brush.Color;

                           DrawBox(X,Y,Width,Height);

                           if PanelType__=mvipSplit then begin
                              Inc(X,Pred(Width div 2));
                              MoveTo(X,Y); LineTo(X,Y+Height);
                              Inc(X);
                              MoveTo(X,Y); LineTo(X,Y+Height);
                              end;
                         end;
           mvipClose   : begin
                           Width:=Min(Height,13);
                           if not Odd(Width) then Inc(Width); // so 'Width = 1 + (2 * (Width div 2))', i.e., so there is the same distance from the center to both ends of a line

                           X:=Left+(Succ(RectWidth (R)) div 2);
                           ////Dec(Y,2+(Width div 2)); // 'Point' = center position
                           Y:=Top +(Succ(RectHeight(R)) div 2);

                           a:=Width div 2; b:=Width-a; // 'a' and 'b' are the lengths of the left and right side components of the lines; 'LineTo' excludes the end-point

                           //MoveTo(X-a,Y-a+2); LineTo(X+b-2,Y+b);
                           //MoveTo(X-a+2,Y+a); LineTo(X+b,Y-b+2);
                           DrawCross(Succ(X),Succ(Y),a,b);

                           if   (Self=Owner.FocusedItem) and (PanelType__=Owner.FocusedItemPanelType) then
                                Brush.Color:=Owner.FocusedTextColor
                           else Brush.Color:=Owner.TextColor;
                           Pen.Color:=Brush.Color;

                           DrawCross(X,Y,a,b);
                         end;
         end; // case
      end;
    end;
  end;

begin // 'ShowPanel'
  if Visible then with Owner.Image.Picture.BitMap.Canvas do with R do begin
     Brush.Style:=bsSolid; Pen.Style:=psSolid; Pen.Width:=1;

     Panels[PanelType__].Alignment:=Alignment__;
     if   Enabled__ or (PanelType__=mvipCaption) then
          Panels[PanelType__].Caption:=Caption__
     else Panels[PanelType__].Caption:='';
     Panels[PanelType__].Enabled:=Enabled__;
     R:=Rect__;
     Misc_.ClipRect(R,PanelsRect);
     Panels[PanelType__].Rect:=R;
     if PanelType__=mvipCaption then HasSecondaryMetricsCaption:=False; // for conveniency, updating the caption always resets the flag

     R1:=Classes.Rect(Succ(Left),Succ(Top),Pred(Right),Pred(Bottom));
     if PanelType__<>High(PanelType__) then Inc(R1.Right);
     W:=RectWidth(R1); H:=RectHeight(R1);

     if (W>0) and (H>0) then begin // 'FillRect' doesn't seem to have the '>' guards itself, hence, these explicit guards
        Brush.Color:=clNavy; Brush.Style:=bsSolid;
        Owner.ShowBackGround(R1); //FillRect(R1);

        if MakeStaticWorkBitMap(W,H,True) then begin
           R2:=Classes.Rect(0,0,W,H);
           StaticWorkBitMap.Canvas.CopyMode:=cmSrcCopy;
           StaticWorkBitMap.Canvas.CopyRect(R2,Owner.Image.Picture.BitMap.Canvas,R1);
           if   Self=Owner.Selected then
                BitMapAlphaBlendColor(StaticWorkBitMap,Owner.FocusedBackgroundColor,100-Owner.BackgroundTransparencyPct,R2)
           else BitMapAlphaBlendColor(StaticWorkBitMap,Owner.BackgroundColor       ,100-Owner.BackgroundTransparencyPct,R2);
           Owner.Image.Picture.BitMap.Canvas.CopyRect(R1,StaticWorkBitMap.Canvas,R2);
           end;
        end;

     Pen.Color:=Owner.LineColor;
     MoveTo(Left,Top); LineTo(Right,Top); // top line

     Pen.Color:=Owner.ShadowColor;
     MoveTo(Succ(Left),Succ(Top)); LineTo(Succ(Left+W),Succ(Top)); // top shadow line

     Pen.Color:=Owner.LineColor;
     MoveTo(Left,Succ(Top)); LineTo(Left,Pred(Bottom)); // left-side line

     if Succ(Left)<Right then begin
        Pen.Color:=Owner.ShadowColor;
        MoveTo(Succ(Left),Succ(Top)); LineTo(Succ(Left),Pred(Bottom)); // left-side shadow line
        end;

     if (Left<Right) and (Top<Bottom) then
        if PanelType__<>mvipCaption then begin
           DrawIcon(PanelType__,R1);
           end
        else begin
           Inc(Left,4); Inc(Top,2); Dec(Right,2); Dec(Bottom,2);

           if RectWidth(R)>=Abs(Owner.Font.Height) then begin // 'True': the rectangle is at least wide enough to hold (the most of) the first character
              SetBkMode(Handle, TRANSPARENT);

              if   Owner.Image.Tag<>LongInt(Owner) then begin
                   Owner.Image.Tag:=LongInt(Owner);
                   Owner.Image.Picture.BitMap.Canvas.Font.Assign(Owner.Font);
                   end;
              if   PanelType__=mvipCaption then
                   Font.Style:=[]
              else Font.Style:=[fsBold];

              FormatOptions:=DT_SINGLELINE+DT_VCENTER;
              case Alignment__ of
                taLeftJustify : Inc(FormatOptions,DT_LEFT);
                taCenter      : Inc(FormatOptions,DT_CENTER);
                taRightJustify: Inc(FormatOptions,DT_RIGHT);
                end;
              if Alignment__=taCenter then begin
                 W:=TextWidth(Caption__);
                 Dec(FormatOptions,DT_CENTER);
                 Inc(FormatOptions,DT_LEFT);
                 Dec(Left,2);
                 Inc(Left,Max(0,(RectWidth(R)-W)) div 2);
                 end;

              R1:=Classes.Rect(Succ(Left),Succ(Top),Succ(Right),Succ(Bottom));

              Font.Color:=Owner.ShadowColor;
              DrawTextEx(Handle,PChar(Panels[PanelType__].Caption),Length(Panels[PanelType__].Caption),R1,FormatOptions,nil); // caption shadow

              if   (Self=Owner.Selected) or
                   ((PanelType__=mvipCaption) and
                    (Self=Owner.FocusedItem) and
                    (Owner.FocusedItemPanelType=mvipCaption) and
                    Panels[mvipCaption].Enabled
                   ) then
                   Font.Color:=Owner.FocusedTextColor
              else Font.Color:=Owner.TextColor;
              DrawTextEx(Handle,PChar(Panels[PanelType__].Caption),Length(Panels[PanelType__].Caption),R ,FormatOptions,nil); // caption

              SetBkMode(Handle, OPAQUE);

              //if Font.Color=Owner.FocusedTextColor then if PanelType__=mvipCaption then if StrStartsWith(Panels[mvipCaption].Caption,'191') then
              //   DoNothing;
              end;
           end;
     end;
end;

procedure TMultiViewItem.ShowPanels(const Caption__:String);
begin // 'ShowPanels'; side-effects: calculates 'fPanelsRect', 'PanelHeight', and Panels[].Enabled
  with Owner.Image.Picture.BitMap.Canvas do begin
    Owner.Image.Tag:=0; // force 'load font' for menu and status
    Pen.Mode:=pmCopy; Pen.Style:=psSolid;
    PanelHeight:=CalculatePanelHeight;
    with Rect do fPanelsRect:=Classes.Rect(Left+2,Top+2,Right-2,Top+2+PanelHeight);
    Misc_.ClipRect(fPanelsRect,Rect);
    DrawShadowedFrame(PanelsRect);

    with PanelsRect                do ShowPanel(mvipClose   ,ViewItemPanelCaptionClose   ,True,
                                                Classes.Rect(Right-PanelHeight,Top,Right,Bottom),taCenter);
    with Panels[mvipClose   ].Rect do ShowPanel(mvipMaximize,ViewItemPanelCaptionMaximize,(RectWidth(Rect)<RectWidth(MaxRectAfterSqueezingOtherItems)) or (RectHeight(Rect)<RectHeight(MaxRectAfterSqueezingOtherItems)),
                                                Classes.Rect(Left-PanelHeight+1,Top,Left,Bottom),taCenter);
    with Panels[mvipMaximize].Rect do ShowPanel(mvipMinimize,ViewItemPanelCaptionMinimize,((RectWidth(Rect)>MinWidth) or (RectHeight(Rect)>MinHeight)) and (Owner.Items.Count>1),
                                                Classes.Rect(Left-PanelHeight+1,Top,Left,Bottom),taCenter);
    with Panels[mvipMinimize].Rect do ShowPanel(mvipSplit   ,ViewItemPanelCaptionSplit   ,CanSplit(Owner.SplitAxis),
                                                Classes.Rect(Left-PanelHeight+1,Top,Left,Bottom),taCenter);
    with Panels[mvipSplit   ].Rect do ShowPanel(mvipMenu    ,ViewItemPanelCaptionMenu    ,True,
                                                Classes.Rect(PanelsRect.Left,Top,PanelsRect.Left+Pred(PanelHeight),Bottom),taCenter);
    with Panels[mvipSplit   ].Rect do ShowPanel(mvipCaption ,{Self.Name} Caption__       ,Self<>Owner.Selected,
                                                Classes.Rect(Panels[mvipMenu].Rect.Right,Top,Left,Bottom),taLeftJustify);
    end;
end;

function  TMultiViewItem.SplitRect(SplitAxis__:TSplitAxis):TRect;
var H1,H2,W1,W2:Integer;
begin
  Result:=Rect;
  with Result do begin
    if        SplitAxis__=saBest then with MainForm.Game do begin
              ScaleKeepingAspectRatio(BoardWidth,BoardHeight,RectWidth(Rect)      ,RectHeight(Rect) div 2,W1,H1);
              ScaleKeepingAspectRatio(BoardWidth,BoardHeight,RectWidth(Rect) div 2,RectHeight(Rect)      ,W2,H2);
              if ((W1*H1>W2*H2)                         // 'True': a horizontal split gives the best filling ratio for the two regions
                  and
                  ((RectHeight(Rect) div 2)>=MinHeight) // 'True': a horizontal split produces a rectangle with a height above or equal to the minimum
                 )
                 or
                 ((W1*H1<W2*H2)                         // 'True': a vertical split gives the best filling ratio for the two regions
                  and
                  ((RectWidth(Rect) div 2)<  MinWidth)  // 'True': a vertical   split produces a rectangle with a width  below the minimum
                 ) then
                 SplitAxis__:=saHorizontal
              else begin
                 // if width = height, then the split axis defaults to 'vertical'
                 // because it's better from an ergonomical point of view;
                 // horizontal eye movements are easier than vertical movements,
                 // so it's easier to compare two items (boards) arranged side
                 // by side than two items stacked vertically;
                 end;
              end
    else if   SplitAxis__=saBiggest then
              if RectHeight(Rect)>RectWidth(Rect) then SplitAxis__:=saHorizontal;

    if   SplitAxis__=saHorizontal then
         Bottom:=Top +(RectHeight(Rect) div 2)
    else Right :=Left+(RectWidth (Rect) div 2);
    end;
end;

constructor TMultiView.Create(Image__:TImage);
begin
  Image:=Image__;
  MouseDownSet:=[];
  CalculateSecondaryScoreMetricsTimeMS:=0;
  FillChar(ClippingRect,SizeOf(ClippingRect),0);
  FillChar(DragRect,SizeOf(DragRect),0);
  fHint:=''; fSelected:=nil; fSerialNo:=0;
  fDisappearedItemsCount:=0; fOpening:=nil; DoReportDisappearedItems:=True;
  FocusedItem:=nil; FocusedItemPanelType:=mvipNone;
  Font:=nil; fIgnoreMouseUp:=False; fItems:=nil; MouseOverSoundEnabled:=True;
  try    Font:=TFont.Create;
         fItems:=TList.Create;
  except on E:Exception do begin fItems.Free; fItems:=nil;
                                 Font  .Free; Font  :=nil;
                           end;
  end;
  SetDefaultValues;
end;

destructor  TMultiView.Destroy;
begin
  Items.Free; // order important; 'Font' may first be destroyed after the items; items hide themselves when they are destroyed
  Font.Free;
end;

procedure TMultiView.CalculateMaximizedRectangles(Show__:Boolean);

  procedure CalculateMaximizedRectangles(Show__:Boolean);
  var Item:TMultiViewItem;
  begin
    Item:=TMultiViewItem(Items.First);
    while Assigned(Item) do begin
      Item.MaxRectAfterSqueezingOtherItems:=Item.CalculateMaxRect(True);
      if Show__ then
         Item.ShowPanel(mvipMaximize,ViewItemPanelCaptionMaximize,
                        (RectWidth(Item.Rect) <RectWidth (Item.MaxRectAfterSqueezingOtherItems))
                        or
                        (RectHeight(Item.Rect)<RectHeight(Item.MaxRectAfterSqueezingOtherItems)),
                        Item.Panels[mvipMaximize].Rect,
                        Item.Panels[mvipMaximize].Alignment);
      Item:=TMultiViewItem(Item.Next);
      end;
  end;

  procedure CalculateTreeHeights(Direction__:TDirection);
  var IntersectionStart,IntersectionEnd:Integer;
      Item,PrevItem,NextItem,P:TMultiViewItem;
  begin // side-effect: reverses the multi view items on the 'Items' list
    PrevItem:=nil;
    Item:=TMultiViewItem(Items.First);
    while Assigned(Item) do begin
      NextItem:=TMultiViewItem(Item.Next); Item.Next:=PrevItem; PrevItem:=Item; // reverse the list items

      Item.TreeHeights[Direction__]:=0;
      P:=TMultiViewItem(Item.Next);
      while Assigned(P) do begin
        if ((DIRECTION_TO_AXIS[Direction__]=colAxis)                 // 'True': 'Direction__' in [Up,Down]
            and
            IntervalIntersection(Item.Rect.Left,Item.Rect.Right,
                                 P   .Rect.Left,P   .Rect.Right,
                                 IntersectionStart,IntersectionEnd)  // 'True': horizontal overlap
            and
            ((P.BoardRect.Right=0)
             or
             (P.BoardRect.Right+MULTI_VIEW_ITEM_BOARD_BORDER_SIZE>IntersectionStart) // 'True': the overlap collides with the board display for the 'P' item
            )
           )
           or
           ((DIRECTION_TO_AXIS[Direction__]=rowAxis)                 // 'True': 'Direction__' in [Left,Right]
            and
            IntervalIntersection(Item.Rect.Top ,Item.Rect.Bottom,
                                 P   .Rect.Top ,P   .Rect.Bottom,
                                 IntersectionStart,IntersectionEnd)  // 'True': vertical overlap
            and
            ((P.BoardRect.Bottom=0)
             or
             (P.BoardRect.Bottom+MULTI_VIEW_ITEM_BOARD_BORDER_SIZE>IntersectionStart) // 'True': the overlap collides with the board display for the 'P' item
            )
           ) then begin
           Item.TreeHeights[Direction__]:=Max(Item.TreeHeights[Direction__],P.TreeHeights[Direction__]);
           end;
        P:=TMultiViewItem(P.Next);
        end;
      Inc(Item.TreeHeights[Direction__]); // count the item itself

      Item:=NextItem; // advance to the next item on the list
      end;
    Items.Items:=PrevItem; // update the root of the list after the list items have been reversed
  end;

begin // 'CalculateTreeHeights'; calculates for each view item the maximum number of other items blocking the view to the edge of the screen in each direction
  Sort(scTop);                            // sort the items in ascending order on top co-ordinates
  CalculateTreeHeights(Up);               // calculate tree heights from the top

  // after the list reversal, the items are now sorted in descending order on top co-ordinates
  CalculateTreeHeights(Down);             // calculate tree heights from the bottom

  Sort(scLeft);                           // sort the items in ascending order on left co-ordinates
  CalculateTreeHeights(Left);             // calculate tree heights from the left

  // after the list reversal, the items are now sorted in descending order on left co-ordinates
  CalculateTreeHeights(Right);            // calculate tree heights from the right

  CalculateMaximizedRectangles(Show__);   // calculate the maximum extent for all the multi view items
end;

function  TMultiView.BiggestItem(ExceptItem__:TMultiViewItem):TMultiViewItem;
var Item:TMultiViewItem;
begin
  Result:=nil;
  Item:=TMultiViewItem(Items.First);
  while Assigned(Item) do begin
        if (Item<>ExceptItem__)
           and
           Item.Visible // 'hidden' means 'locked' in 'ResizeItem'
           and
           ((not Assigned(Result))
            or
            (RectWidth(Item.Rect)*RectHeight(Item.Rect)>RectWidth(Result.Rect)*RectHeight(Result.Rect))
           )then
           Result:=Item;
        Item:=TMultiViewItem(Item.Next);
        end;
end;

procedure TMultiView.Clear;
begin
  CalculateSecondaryScoreMetricsTimeMS:=0; fSerialNo:=0; fOpening:=nil;
  HideDragRect;
  while not Items.IsEmpty do Items.Pop.Free;
  if Assigned(MainForm.GameViewer) and (not IsEqualRects(ClippingRect,MainForm.GameViewer.WindowRect)) then
     MainForm.GameViewer.SetWindow(MainForm.Image1.Picture.BitMap.Canvas,ClippingRect); // restore the game viewer region to the entire board area of the screen
end;

function  TMultiView.CloseItem(Item__:TMultiViewItem; ItemDisappeared__:Boolean):Boolean;
begin // closes the item under all circumstances, but returns 'False' is something went wrong during the process
  Result:=True;
  if Assigned(Item__) then begin
     if ItemDisappeared__ then begin
        if Item__=Selected then Result:=Item__.MakeSnapshot; // 'True': the existing snapshot for the selected item may be out of sync with the current game state, hence, create a new updated snapshot
        Result:=Item__.SaveSnapshot(fDisappearedItemsCount) and Result; // transfer the snapshot to the game, i.e., the 'MainForm.Game.Snapshots' list
       end;

     if Items.Count>2 then
        Items.Remove(Item__,True)
     else begin
        if (Item__<>Selected) and Assigned(Selected) then Result:=Selected.MakeSnapshot and Result; // 'True': the existing snapshot for the selected item may be out of sync with the current game state, hence, create a new updated snapshot
        fSelected:=nil;
        Items.MoveToFront(Item__); // if there are 2 views, then ensure that it's the other item which ends up on the screen after both views have been closed
        Item__:=TMultiViewItem(Items.Remove(Items.Last,False)); // select the item to appear on the screen after the items have been closed
        try     Clear;
                Item__.Hide;
                Result:=MainForm.Game.LoadSnapshot(Item__.Snapshot) and
                        MainForm.InitGame(False,False,False,False,False,True,True,0) and
                        Result;
        finally Item__.Free; Clear; // 'Clear': ensure the list is cleansed even if 'try..finally' terminates abnormally
                MainForm.Game.SessionSmoothMoveAnimationEnabled:=MainForm.Game.SmoothMoveAnimationEnabled;
                MainForm.ShowTitle(MainForm.Game.FileName)
        end;
        end;
     end;
end;

function  TMultiView.CombineSnapshotsToFormSolutions(DoIt__:Boolean):Integer;
// if 'DoIt__' is 'False': returns the number of snapshots which can be combined with the current game state (in 'MainForm.Game') to form solutions; duplicates are not filtered out
// if 'DoIt__' is 'True' : makes the combinations and returns the number of new solutions; duplicates are dropped
var
  OldSelectedSerialNo:TSerialNo;
  CurrentGameSnapshot,Snapshot:TSnapshot; Item:TMultiViewItem;

  function CombineSnapshot(Snapshot__:TSnapshot):Boolean;
  var OldSoundEnabled:Boolean; OldSelected:TMultiViewItem;
  begin
    Result:=False;
    OldSoundEnabled:=MainForm.Sound.Enabled;
    try     MainForm.Sound.Enabled:=False;

            if Assigned(CurrentGameSnapshot) and
               MainForm.Game.LoadSnapshot(CurrentGameSnapshot) and
               MainForm.Game.CombineCurrentPositionAndSnapshotToFormASolution(Snapshot__) then begin
               Snapshot__:=MainForm.Game.MakeSnapshot('');
               try     if Assigned(Snapshot__) then begin
                          if Assigned(Selected) then begin // ensure that 'LoadSnapshot()' doesn't overwrite the snapshot for the selected item with the current game state, i.e., the new solution
                             OldSelected:=Selected;
                             fSelected:=nil;
                             OldSelected.Open; // show the item as un-selected
                             end;
                          Snapshot__.GameState:=gsPlay; // trick 'LoadSnapshot()' to show the final position instead of the starting position, as it normally does when solutions are loaded
                          Result:=LoadSnapshot(Snapshot__);
                          end;
               finally Snapshot__.Free;
               end;
               end;

    finally MainForm.Sound.Enabled:=OldSoundEnabled;
    end;
  end;

begin // 'CombineSnapshotsToFormSolutions'
  Result:=0;
  if (not DoIt__) or (Assigned(Selected) and Selected.MakeSnapshot) then begin // the snapshot for the current item may be out of sync with the current game state, hence create a new updated snapshot

     if DoIt__ then begin
        CurrentGameSnapshot:=Selected.Snapshot.Clone; // make a local copy of the current game state; the selected snapshot may not live throughout the combinations so it cannt be used; its may be pushed off the screen in which case its snapshot of a copy of its snaphot, is stored on the "snapshots" list
        OldSelectedSerialNo:=Selected.SerialNo;  // remember the selected item, so it can be focused again if no new solutions are created
        end
     else begin
        CurrentGameSnapshot:=nil; OldSelectedSerialNo:=0;
        end;

     try
       Item:=TMultiViewItem(Items.First);
       while Assigned(Item) do begin
         if  MainForm.Game.CanCombineCurrentPositionAndSnapshotToFormASolution(Item.Snapshot) and
             ((not DoIt__) or CombineSnapshot(Item.Snapshot)) then
             Inc(Result);
         Item:=TMultiViewItem(Item.Next);
         end;

       Snapshot:=TSnapshot(MainForm.Game.Snapshots.First);
       while Assigned(Snapshot) do begin
         if  MainForm.Game.CanCombineCurrentPositionAndSnapshotToFormASolution(Snapshot) and
             ((not DoIt__) or CombineSnapshot(Snapshot)) then
             Inc(Result);
         Snapshot:=TSnapshot(Snapshot.Next);
         end;
     finally
       if DoIt__ then begin
          if     Assigned(Selected) then Selected.Open; // ensure that the current game state matches the selected item snapshot; it may be out of sync if 'CombineSnapshot' 1) could not create a solution because of too many moves, or 2) could not find screen space for the solution
          if not Assigned(Selected) then Selected:=GetItemBySerialNo(OldSelectedSerialNo); // 'True': no new solutions appeared on the screen; try to focus the old one
          if not Assigned(Selected) then Selected:=TMultiViewItem(Items.First); // no new solutions appeared on the screen and the original one disappeared; try to focus another view, if any
          if not Assigned(Selected) then MainForm.ShowTitle(MainForm.Game.FileName); // ensure that the main window caption has been updated
          CurrentGameSnapshot.Free;
          end;
     end;
     end;
end;

function  TMultiView.DeleteScreenRegionInformation(Snapshot__:TSnapshot):Boolean;
begin
  Result:=False;
  if Assigned(Snapshot__) and Assigned(Snapshot__.Notes) then begin
     if   Snapshot__=SnapshotsForm.Snapshots[SnapshotsForm.Grid.Row] then begin // 'True': the snapshot is the one currently selected in the 'Snapshots' window; the user may have modified the notes (the memo) on the screen
          SnapshotsForm.InfoMemoExit(nil);                                      // memo -> list
          Result:=Snapshot__.Notes.Lines.DeleteKey(TEXT_SCREEN_REGION)<>'';     // 'True': the snapshot had screen region information, which now has been deleted
          if Result then SnapshotsForm.LoadNotes(Snapshot__.Notes);             // list -> memo
          end
     else Result:=Snapshot__.Notes.Lines.DeleteKey(TEXT_SCREEN_REGION)<>'';
     if Result then Snapshot__.Notes.Modified:=True;
     end;
end;

function  TMultiView.DeleteScreenRegionInformationForAllSnapshots:Integer;
var Index:Integer;
begin  // precondition: the snapshots have been loaded by the snapshots window
  Result:=0;
  if Assigned(SnapshotsForm) then with SnapshotsForm do
     for Index:=SnapshotsForm.Grid.FixedRows to Pred(SnapshotsForm.Grid.RowCount) do
         if DeleteScreenRegionInformation(SnapshotsForm.Snapshots[Index]) then Inc(Result);
end;

function  TMultiView.FindDuplicateItem(ExceptItem__:TMultiViewItem; MoveCountMustMatch__,MoveTopMustMatch__,VisibleItemsOnly__:Boolean; var DuplicateItem__:TMultiViewItem):Boolean;
var Item,OldSelected:TMultiViewItem;
begin
  Result:=False;
  if Assigned(Selected) then Selected.MakeSnapshot; // make an updated snapshot for the selected item; the old snapshot saved for the selected item may be out of sync with the current game state

  OldSelected:=Selected;
  try     fSelected:=nil; // so 'IsDuplicateItem' doesn't repeat making new updated snapshots for the current item

          Item:=TMultiViewItem(Items.First);
          while  Assigned(Item) and (not Result) do
            if   IsDuplicateItem(Item,MoveCountMustMatch__,MoveTopMustMatch__,VisibleItemsOnly__,DuplicateItem__) and
                 (DuplicateItem__<>ExceptItem__) then
                 Result:=True
            else Item:=TMultiViewItem(Item.Next);

  finally fSelected:=OldSelected;
  end;
end;

function  TMultiView.GetItemBySerialNo(SerialNo__:TSerialNo):TMultiViewItem;
begin
  Result:=TMultiViewItem(Items.First);
  while Assigned(Result) and (Result.SerialNo<>SerialNo__) do
    Result:=TMultiViewItem(Result.Next);
end;

function  TMultiView.GetNextSerialNo:TSerialNo;
begin
  Inc(fSerialNo); Result:=fSerialNo;
end;

function  TMultiView.GetSpaceFromAnotherItem(Item__:TMultiViewItem):Boolean;
var AnotherItem,NewItem,SplitCandidate,DuplicateItem:TMultiViewItem;
begin
  Result:=False;
  SplitCandidate:=BiggestItem(Item__);
  if           Assigned(SplitCandidate) and SplitCandidate.CanSplit(SplitAxis) then begin
               NewItem:=nil;
               try     NewItem:=SplitItem(SplitCandidate,SplitAxis,True);
                       if Assigned(NewItem) and Item__.CanResize(NewItem.Rect) then begin
                          Item__.Hide;
                          Item__.Rect:=NewItem.Rect;  // take the area from the new item
                          Result:=True;
                          end;
               finally if Assigned(NewItem) then begin // destroy the temporary new item, it any
                          Items.Remove(NewItem,True);
                          Dec(fSerialNo); // adjust the serial number so the temporary new item doesn't influence future serial numbers
                          end;
               end;
               end
  else if      FindDuplicateItem(Item__,True ,True,True,DuplicateItem) then begin // note that the implementation is a simple one, only checking the remaining visible view items; it doesn't try to do anything about duplicates which happen to have been processed earlier and already have allocated a region on the screen
               DuplicateItem.Hide;
               Item__.Rect:=DuplicateItem.Rect;  // take the area from the duplicate
               Items.Remove(DuplicateItem,True); // destroy the duplicate
               Result:=True;
               end
       else if (Item__=Selected) and              // try to protect the currently selected item against deletion by stealing a screen region from another view item
               (Items.Count>2) then begin         // '>2': don't risk that closing a stolen view makes 'Item__' the last remaining view; it would be closed automatically too during the call to 'CloseItem'
               AnotherItem:=TMultiViewItem(Items.First);
               while Assigned(AnotherItem) and (not Result) do
                 if   AnotherItem.Visible and (AnotherItem<>Item__) then Result:=True
                 else AnotherItem:=TMultiViewItem(AnotherItem.Next);
               if Result then begin               // 'True': found a visible view; steal its screen region
                  Item__.Rect:=AnotherItem.Rect;  // take the area from the other view item
                  CloseItem(AnotherItem,True);    // destroy the other item; note that it would destroy 'Item__' too if 'Item__' and 'AnotherItem' were the only 2 views left on the screen
                  end;
               end;
end;

procedure TMultiView.HideDragRect;
begin
  DragRect.SizeHandle:=shNull;
  if DragRect.IsDragging or DragRect.IsSizing or DragRect.IsZooming then begin
     ShowDragRect; // 'ShowDragRect' is an XOR image operation, hence, calling it here in effect hides it
     FillChar(DragRect,SizeOf(DragRect),0); // quick-and-dirty: zero-ing the entire record resets all boolean flags like 'IsDragging'
     if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;

     with Image.Picture.BitMap.Canvas do begin
       Pen.Mode    := pmCopy;
       Pen.Style   := psSolid;
       Brush.Style := bsSolid;
       end;
     end;
end;

function  TMultiView.IsEmpty:Boolean;
begin
  Result:=Items.IsEmpty;
end;

function  TMultiView.IsDuplicateItem(Item__:TMultiViewItem; MoveCountMustMatch__,MoveTopMustMatch__,VisibleItemsOnly__:Boolean; var DuplicateItem__:TMultiViewItem):Boolean;
var MoveCount,MoveTop:Integer;
begin
  Result:=False;
  if Assigned(Selected) then Selected.MakeSnapshot; // make an updated snapshot for the selected item; the old snapshot saved for the selected item may be out of sync with the current game state
  if Assigned(Item__.Snapshot) then begin
     if   MoveCountMustMatch__ then MoveCount:=Item__.Snapshot.MoveCount
     else MoveCount:=-1;
     if   MoveTopMustMatch__   then MoveTop  :=Item__.Snapshot.MoveTop
     else MoveTop  :=-1;
     DuplicateItem__:=TMultiViewItem(Items.First);
     while  Assigned(DuplicateItem__) and (not Result) do
       if   Assigned(DuplicateItem__.Snapshot) and
            (DuplicateItem__<>Item__) and
            (DuplicateItem__.Visible or (not VisibleItemsOnly__)) and
            DuplicateItem__.Snapshot.HasIdenticalMoves(Item__.Snapshot.ReverseMode,MoveCount,MoveTop,Item__.Snapshot.Moves) then
            Result:=True
       else DuplicateItem__:=TMultiViewItem(DuplicateItem__.Next);
     end;
end;

function  TMultiView.IsDragging:Boolean;
begin
  Result:=DragRect.IsDragging;
end;

function  TMultiView.IsSizing:Boolean;
begin
  Result:=DragRect.IsSizing;
end;

function  TMultiView.LoadSettingsFromIniFile(const IniFile:TIniFile; const Section:String):Boolean;
begin
  BackgroundColor            :=TColor       (IniFile.ReadInteger(Section,'BackgroundColor',Integer(BackgroundColor)));
  BackgroundTransparencyPct  :=Max(0,Min(100,IniFile.ReadInteger(Section,'BackgroundTransparencyPct',BackgroundTransparencyPct)));
  FocusedBackgroundColor     :=TColor       (IniFile.ReadInteger(Section,'FocusedBackgroundColor',Integer(FocusedBackgroundColor)));
  FocusedTextColor           :=TColor       (IniFile.ReadInteger(Section,'FocusedTextColor',Integer(FocusedTextColor)));
  LineColor                  :=TColor       (IniFile.ReadInteger(Section,'LineColor',Integer(LineColor)));
  ShadowColor                :=TColor       (IniFile.ReadInteger(Section,'ShadowColor',Integer(ShadowColor)));
  TextColor                  :=TColor       (IniFile.ReadInteger(Section,'TextColor',Integer(TextColor)));
  Result:=LoadFontFromIniFile(IniFile,Section,'',Font);
  Image.Tag:=0; // 'Tag' used as flag for current font assigned to the canvas
end;

function  TMultiView.LoadSnapshot(Snapshot__:TSnapshot):Boolean;
var MoveCount:Integer; Count:Cardinal; FirstItem,Item,OldSelected:TMultiViewItem;
begin
  Result:=False;
  if Assigned(Snapshot__) and MainForm.Game.IsIdleAndStopReplayingAndBrowsing then begin
     MainForm.TrackState:=tsWait; MainForm.ClearTrackBox(True);
     OldSelected:=Selected;
     Selected:=nil; // update the selected item, if any, so it contains a snapshot of the current game state

     if   (Snapshot__.GameState=gsSolved) and
          (Snapshot__.MoveCount=Snapshot__.MoveTop) and
          (Snapshot__<>MainForm.Game.SaveGame) then // 'True': solutions are loaded with the starting position, hence, compare against the existing views with move count = 0
          MoveCount:=0
     else MoveCount:=Snapshot__.MoveCount;

     if LookupSnapshot(Snapshot__.ReverseMode,MoveCount,Snapshot__.MoveTop,Snapshot__.Moves,Item) then begin
        Selected:=Item;
        Result:=True;
        end
     else begin
        Item:=nil;
        try
                 // make a view item and get a region on the screen for it
                 if        IsEmpty then with Snapshot__ do begin
                           Item:=SplitItem(nil,SplitAxis,True);
                           if Assigned(Item) and
                              MainForm.Game.LoadSnapshot(Snapshot__) and
                              MainForm.InitGame((GameState=gsSolved) and (MoveCount=MoveTop),False,False,False,False,True,False,0) and
                              Item.MakeSnapshot then begin
                              OldSelected:=Selected;
                              fSelected:=nil;
                              if Assigned(OldSelected) then OldSelected.Open; // display the old selected view as a not selected view
                              Result:=True;
                              end
                           else begin
                              CloseItem(Item,False); Item:=nil; // call 'CloseItem' before 'Clear' because in contrast to 'Clear', 'CloseItem' restores the old game state on the screen
                              Clear;
                              end;
                           end
                 else if   MakeItem(Item) then
                           try
                             Result:=GetSpaceFromAnotherItem(Item); // 'True': got a screen region either by splitting an existing item or by throwing a duplicate item away
                             if not Result then begin // 'True': steal one of the existing views and use it for this snapshot
                                Sort(scTop);
                                Items.MoveToBack(OldSelected); // if there is more than one view item, then don't steal the most recently selected view
                                FirstItem:=TMultiViewItem(Items.First);
                                if FirstItem.SaveSnapshot(Count) then begin
                                   Item.Free; // drop the newly created item
                                   Item:=FirstItem; // use the existing view to display the requested snapshot
                                   Item.HasSecondaryMetricsCaption:=False;
                                   Result:=True;
                                   end;
                                end;

                             Result:=Result and
                                     MainForm.Game.LoadSnapshot(Snapshot__) and
                                     MainForm.InitGame((Snapshot__.GameState=gsSolved) and (Snapshot__.MoveCount=Snapshot__.MoveTop),False,False,False,False,True,False,0) and
                                     Item.MakeSnapshot;

                             if Result then begin
                                if not Items.IsMember(Item) then begin
                                   Items.Add(Item); // 'True': it's a new item; add it to the list

                                   fSelected:=Item; // otherwise, 'OnResize' selects the item under the mouse cursor position, and that's not ok because the enclosing 'try...finally' statement manipulates 'fSelected' directly instead of calling 'SetSelected()' by a 'Selected:=...' assignment to the 'Selected' property

                                   fDisappearedItemsCount:=0;
                                   OnResize(nil); // recalculate information, in particular for showing the correct 'maximize' and 'minimize' status for each view
                                   if ReportDisappearedItems(DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
                                      DoReportDisappearedItems:=False;
                                   end;
                                end;
                           finally if not Result then Items.Remove(Item,True); // if 'try...finally' terminated abnormally then destroy the item again; 'TList.Remove(Item,True)' destroys the item even when the item isn't a member of the list
                           end
                      else begin Item.Free; Item:=nil; // destroy the item, if it has been created
                           end;

        finally  if Result and Assigned(Item) then begin
                    fSelected:=Item;
                    Selected.Open;
                    MainForm.ShowTitle(MainForm.Game.FileName);
                    end
                 else begin
                    Result:=False;
                    Selected:=OldSelected;
                    Msg(TEXT_TASK_FAILED,MainForm.Caption,MB_OK+MB_ICONERROR);

                    fDisappearedItemsCount:=0;
                    OnResize(nil); // recalculate information and maximize all view items so they fill out the screen
                    if ReportDisappearedItems(DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
                       DoReportDisappearedItems:=False;
                    end;
        end;
        end;
     if Result then DeleteScreenRegionInformation(Snapshot__);
     end;
end;

function  TMultiView.LoadSnapshots:Boolean;
var Count,Row:Integer; Text:String; Rect:TRect; Snapshot:TSnapshot; Item:TMultiViewItem;
begin // loads snapshots from 'MainForm.Game' having 'screen region' information in the notes;
      // precondition: the snapshots and solutions for 'MainForm.Game' have been loaded by 'SnapshotsForm'
  Count:=0;

  if Assigned(SnapshotsForm) and (RectWidth(ClippingRect)*RectHeight(ClippingRect)>0) then begin
     Selected:=nil;

     for Row:=SnapshotsForm.Grid.FixedRows to Pred(SnapshotsForm.Grid.RowCount) do begin
         Snapshot:=SnapshotsForm.Snapshots[Row];
         if Assigned(Snapshot) and Assigned(Snapshot.Notes) and
            Assigned(Snapshot.Notes.Lines) and
            Snapshot.Notes.Lines.ReadString(TEXT_SCREEN_REGION,Text) and
            StrToRect(Text,Rect) and
            (RectWidth(Rect)>0) and (RectHeight(Rect)>0) then begin

            if   MainForm.Game.Snapshots.IsMember(Snapshot) then begin // 'True': the snapshot is a member of the snapshots list; remove it from the list
                 MainForm.Game.Snapshots.Remove(Snapshot,False); // steal the snapshot from the list
                 Snapshot.SnapshotTag:=0; // '0': the snapshot has been stolen from the snapshots list
                 end
            else begin Snapshot:=Snapshot.Clone; // the snapshot is a best solution; clone it
                       Snapshot.SnapshotTag:=Integer(SnapshotsForm.Snapshots[Row]);  // 'SnapshotTag<>0': the snapshot hasn't been stolen from the snapshots list; instead it's a clone of a best solution and 'SnapshotTag' contains the address of the original snapshot
                 end;

            if   Assigned(Snapshot) then // 'Assigned()': cloning the snapshot may have failed, hence, this check is necessary
                 if      MakeItem(Item) then begin
                         Item.Snapshot.Free; Item.Snapshot:=Snapshot; // attach the snapshot to the view item
                         Snapshot.Notes.Modified:=True; // mark the snaphot as modified so it wil be saved later
                         Item.Rect:=RectPlusOffset(Rect,ClippingRect.Left,ClippingRect.Top); // tentatively allocate the screen region specified in the notes; 'OnResize' will rearrange the items if necessary
                         Items.Push(Item);
                         Inc(Count);
                         end
                 else if Snapshot.SnapshotTag<>0 then Snapshot.Free; // 'True': the snapshot is a clone of a best solution; destroy the clone
            end;
         end;
    end;

  Result:=Items.Count>=2; // 'True': there are 2 or more view items; otherwise the screen should not display multiple views

  if Result then begin // 'True': there are 2 or more view items on the screen
     if Count<>0 then begin
        ShowBackground(ClippingRect); // clear the entire background area before the new views pop up on the screen
        fDisappearedItemsCount:=0;
        OnResize(nil); // recalculate information and maximize all view items so they fill out the screen
        if Assigned(MainForm.Game.SaveGame) then
           LoadSnapshot(MainForm.Game.SaveGame); // focus or load the last active snapshot
        ReportDisappearedItems(False,'');
        end;
     end
  else begin
     while not Items.IsEmpty do begin // put stolen snaphots back on the snapshots list
       Item:=TMultiViewItem(Items.Pop);
       Snapshot:=Item.Snapshot; Item.Snapshot:=nil;
       Item.Free;

       if Assigned(Snapshot) then
          if Snapshot.SnapshotTag=0 then begin // 'True': the snapshot was stolen from the snapshots list; put it back on the list
             Snapshot.Notes.Lines.DeleteKey(TEXT_SCREEN_REGION);
             Snapshot.Notes.Modified:=False; // reset the 'modified' flag
             MainForm.Game.Snapshots.Add(Snapshot);

             if (MainForm.Game.FileName<>'') and
                MainForm.Game.RestoreSaveGame and
                (not Assigned(MainForm.Game.SaveGame)) and // if there isn't any savegame, then load this multiple view item, i.e., treat is as the most recently played variation
                Items.IsEmpty and
                MainForm.Game.LoadSnapshot(Snapshot) and
                MainForm.InitGame(False,False,False,False,False,True,True,0) then begin
                end;
             end
          else begin // the snapshot is a clone of a best solution
             with TSnapshot(Pointer(Snapshot.SnapshotTag)) do begin // remove the screen region information from the original solution
               Notes.Lines.DeleteKey(TEXT_SCREEN_REGION);
               Notes.Modified:=False; // set the 'modified' flag so the screen region information is removed from the file when the level is closed later
               end;
             Snapshot.Free; // destroy the clone
             end;
       end;
     end;

  if Count<>0 then with SnapshotsForm do begin
     LoadSnapshots(Snapshots[Grid.Row]);
     if ItemCount=0 then Hide;
     MainForm.ClearTrackBox(True);
     end;
end;

function  TMultiView.LookupSnapshot(ReverseMode__:Boolean; MoveCount__,MoveTop__:Integer; Moves__:PHistoryMoves; var Item__:TMultiViewItem):Boolean;
begin
  Result:=False;
  if Assigned(Selected) then Selected.MakeSnapshot; // ensure the selected item contains an updated snaphot; the existing one may be out of sync with the current game state
  Item__:=TMultiViewItem(Items.First);
  while Assigned(Item__) and (not Result) do
    if   Assigned(Item__.Snapshot) and
         Item__.Snapshot.HasIdenticalMoves(ReverseMode__,MoveCount__,MoveTop__,Moves__) then
         Result:=True
    else Item__:=TMultiViewItem(Item__.Next);
end;

function  TMultiView.MakeItem(var Item__:TMultiViewItem):Boolean;
begin
  Item__:=nil;
  try    Item__         :=TMultiViewItem.Create(Self);
         Item__.Snapshot:=MainForm.Game.MakeSnapshot('');
         if not Assigned(Item__.Snapshot) then raise Exception.Create(TEXT_MEMORY_FULL);
  except on E:Exception do begin
            Item__.Free; Item__:=nil;
            Msg(E.Message,MainForm.Caption,MB_OK+MB_ICONERROR);
            end;
  end;
  Result:=Assigned(Item__);
end;

function  TMultiView.MarkDuplicates:Boolean;
var DuplicateItem,Item,NewList,OldSelected:TMultiViewItem; DuplicateSnapshot:TSnapshot;
begin // uses the 'TMultiViewItem.Tag' field for marking view items having duplicate snapshots;
      // 'Tag' = 0: unique snapshot, or a representative for a set of duplicates
      // 'Tag' = Integer(Self) : a duplicate view
      // otherwise, 'Tag' = Integer(duplicate snapshot on the 'MainForm.Game.Snapshots' list)
  Result:=(not Assigned(Selected))
          or
          Selected.MakeSnapshot;                 // the existing snapshot for the selected item may be out of sync with the current game state, hence, create a new updated snapshot
  OldSelected:=Selected;
  try
    fSelected:=nil; NewList:=nil;
    Sort(scTop); Items.Reverse;                  // 'reverse': so views near the top end up as the representatives for duplicate snapshots

    while not Items.IsEmpty do begin
      Item     :=TMultiViewItem(Items.Pop);      // temporarily remove the item from the the list and store it on a new list
      Item.Next:=NewList;
      NewList  :=Item;

      if        IsDuplicateItem(Item,True,True,False,DuplicateItem) then
                Item.Tag:=Integer(Item)          // duplicate view item
      else if   Assigned(Item.Snapshot) then with Item.Snapshot do
                if   MainForm.Game.LookupSnapshot(ReverseMode,MoveCount,MoveTop,Moves,DuplicateSnapshot) then // 'True': an identical snapshot already exists
                     Item.Tag:=Integer(DuplicateSnapshot) // duplicate snapshot on the 'Snapshots' list
                else Item.Tag:=0                 // unique snapshot, or a representative for a set of duplicates
           else Item.Tag:=0;                     // no snapshot
      end;
    Items.Items:=NewList;                        // restore the list
  finally fSelected:=OldSelected;
  end;
end;

function  TMultiView.PointToItem(X__,Y__:Integer; var Item__:TMultiViewItem):Boolean;
var P:TPoint;
begin
  P:=Point(X__,Y__);
  if Assigned(Selected) and PtInRect(Selected.Rect,P) and Selected.Visible then
     Item__:=Selected
  else begin
    Item__:=TMultiViewItem(Items.First);
    while Assigned(Item__) and (not (PtInRect(Item__.Rect,P) and Item__.Visible)) do Item__:=TMultiViewItem(Item__.Next);
    end;
  Result:=Assigned(Item__);
end;

function  TMultiView.OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var PreviousGameViewerMask__:Integer):Boolean;
var Item:TMultiViewItem;
begin
  Result:=False;
  HideDragRect;
  Include(MouseDownSet,Button);
  MouseOverSoundEnabled:=False;
  fIgnoreMouseUp:=False;

  if Button=mbLeft then begin
     if PointToItem(X,Y,Item) then with DragRect do begin
        //Image.Picture.BitMap.Canvas.Pen.Mode :=pmCopy;
        //Image.Picture.BitMap.Canvas.Pen.Style:=psSolid;
        //Image.Picture.BitMap.Canvas.Pen.Color:=clYellow;
        //Image.Picture.BitMap.Canvas.MoveTo(X,Y);
        //if Screen.Cursor<>crDrag then Screen.Cursor:=crDrag;
        HideDragRect;
        DragPoint:=Point(X,Y);
        DragOriginPoint:=DragPoint;
        if Assigned(Selected) then DragRect:=Selected.Rect; // note that it's 'Selected.Rect' and not 'Item.Rect', i.e., resizing is only enabled for the currently selected item
        SizeHandle:=PointToSizeHandle(DragPoint,DragRect,SIZE_HANDLE_RECT_DELTA);
        IsSizing:=SizeHandle<>shNull;
        IsDragging:=(not IsSizing) and (Item=Selected) and PtInRect(Item.Panels[mvipCaption].Rect,DragPoint);
        ShowDragRect;
        SetFocusedPanel(nil,mvipNone);
        MouseOverSoundEnabled:=False;
        if        IsDragging then begin
                  if Screen.Cursor<>crDrag then Screen.Cursor:=crDrag;
                  end
        else if   IsSizing then begin
                  if Screen.Cursor<>SizeHandleToCursor(SizeHandle) then
                     Screen.Cursor:=SizeHandleToCursor(SizeHandle);
                  end;
        if        IsDragging
                  or
                  IsSizing
                  or
                  (PtInRect(Item.Panels[mvipClose].Rect,DragPoint) and (not IsSizing)) // 'True': // no actions here; allow closing the item in 'OnMouseUp' without selecting the item
                  then
                  Result:=True // 'True': the 'mouse down' event has been handled
        else if   (Item<>Selected) and Assigned(MainForm.Game) and MainForm.Game.IsIdleAndStopReplayingAndBrowsing then
                  try     MainForm.TrackState:=tsWait; MainForm.ClearTrackBox(True);
                          //Screen.Cursor:=crHourGlass; // don't change to hour glass cursor; it looks disturbing if the user has started an operation like 'move box' in this new view, and the cursor then shortly changes to 'hour glass' and then changes again to 'drag item'
                          Selected:=Item;
                  finally if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
                          PreviousGameViewerMask__:=0; // clear the previous game viewer mask (e.g., 'show movable boxes') when focus changes from one view item to another
                  end;
        end;
     //Image.Picture.BitMap.Canvas.MoveTo(X,Y);
     end;
end;

function  TMultiView.OnMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer):Boolean;
var SH:TSizeHandle; C:TCursor; P:TPoint; ItemPanelType:TMultiViewItemPanelType;
    NewDragRect:TRect; Item:TMultiViewItem;
begin
//if mbLeft in MouseDownSet then begin
     //if Screen.Cursor<>crDrag then Screen.Cursor:=crDrag;
     //DragPoint:=Point(X,Y);
     //Image.Picture.BitMap.Canvas.LineTo(X,Y);
     //Image.Picture.BitMap.Canvas.MoveTo(X,Y);
//   end;

//if (not Assigned(Selected)) and PointToItem(X,Y,Item) then Selected:=Item;

  if Sender=nil then begin // 'True': special: detect the mouse position and update the view state accordingly, if necessary
     P:=Image.ScreenToClient(Mouse.CursorPos);
     X:=P.X; Y:=P.Y;
     if not Assigned(Selected) then begin // 'True': no selected item; select the item under the mouse cursor position, if any, otherwise select the top-left item
        if PointToItem(X,Y,Item) then
           Selected:=Item
        else begin
           Sort(scTop);
           Selected:=TMultiViewItem(Items.First);
           end;
        end;
     end;

  Item:=nil; ItemPanelType:=mvipNone;

  if Assigned(Selected) then begin
     if        DragRect.IsDragging then with DragRect do with DragRect do begin
               ShowDragRect; // 'ShowDragRect' performs an XOR image operation, hence, calling it here in effect hides it

               //Inc(Left,X-DragPoint.X);
               //Inc(Top ,Y-DragPoint.Y);
               //Right :=Left+RectWidth (Selected.Rect);
               //Bottom:=Top +RectHeight(Selected.Rect);
               //Misc_.ClipRect(DragRect,ClippingRect);

               if PointToItem(X,Y,Item) then DragRect:=Item.Rect;
               Item:=nil;

               DragPoint.X:=X; DragPoint.Y:=Y;
               if Screen.Cursor<>crDrag then Screen.Cursor:=crDrag;

               ShowDragRect;
               MouseOverSoundEnabled:=True;
               end
     else if   DragRect.IsSizing then with DragRect do with DragRect do begin
               ShowDragRect; // 'ShowDragRect' performs an XOR image operation, hence, calling it here in effect hides it

               NewDragRect:=DragRect;
               SizeHandleResize(True,SizeHandle,X,Y,DragPoint,NewDragRect);
               Misc_.ClipRect(NewDragRect,Selected.MaxRectAfterSqueezingOtherItems);

               if Selected.CanResize(NewDragRect) then DragRect:=NewDragRect;

               ShowDragRect;
               MouseOverSoundEnabled:=True;
               end
          else begin
               HideDragRect;
               if PointToItem(X,Y,Item) then begin
                  P:=Point(X,Y);
                  if      Item=Selected then
                          SH:=PointToSizeHandle(P,Item.Rect,SIZE_HANDLE_RECT_DELTA) // only allow resizing of the selected item
                  else    SH:=shNull;
                  C:=SizeHandleToCursor(SH);
                  if      (C<>Screen.Cursor) and
                          ((SH<>shNull) or IsSizeHandleCursor(Screen.Cursor)) and
                          (not (Assigned(Opening) and (Screen.Cursor=crHourGlass))) then
                          Screen.Cursor:=C;

                  if      (SH<>shNull) and (MouseDownSet=[]) then begin
                          Hint:=HintResizeViewText; MouseOverSoundEnabled:=True;
                          end
                  else if MouseDownSet=[] then begin
                          if        Item<>Items.First then Items.MoveToFront(Item); // speed up 'PointToItem' by making the item-list a so-called 'self-organizing list' with the most recently visited items at the front of the list
                          if        PtInRect(Item.Panels[mvipClose   ].Rect,P)                                        then begin
                                    Hint:=HintCloseViewText[((Item= Selected) and                             (MainForm.Game.History.Count>MainForm.Game.ForcedInitialJumps))
                                                            or
                                                            ((Item<>Selected) and Assigned(Item.Snapshot) and (Item.Snapshot.MoveCount    >Item.Snapshot.ForcedInitialJumps))
                                                           ];
                                    ItemPanelType:=mvipClose;
                                    end

                          else if   PtInRect(Item.Panels[mvipMaximize].Rect,P) and  Item.Panels[mvipMaximize].Enabled then begin
                                    Hint:=HintMaximizeViewText;    ItemPanelType:=mvipMaximize;
                                    end
                          else if   PtInRect(Item.Panels[mvipMinimize].Rect,P) and  Item.Panels[mvipMinimize].Enabled then begin
                                    Hint:=HintMinimizeViewText;    ItemPanelType:=mvipMinimize;
                                   end
                          else if   PtInRect(Item.Panels[mvipSplit   ].Rect,P) and  Item.Panels[mvipSplit   ].Enabled then begin
                                    Hint:=HintSplitViewText;       ItemPanelType:=mvipSplit;
                                    end
                          else if   PtInRect(Item.Panels[mvipCaption ].Rect,P) then begin
                                    Item.ShowCaptionWithSecondaryMetrics;
                                    if   Item.Panels[mvipCaption ].Enabled then begin
                                         Hint:=HintSelectViewText; ItemPanelType:=mvipCaption;
                                         end
                                    else if   Item=Selected then
                                              Hint:=MetricsText+COLON+SPACE+Item.Panels[mvipCaption].Caption+HintClickAndDragMouseToMoveSnapshotViewText
                                         else Hint:=MetricsText+COLON+SPACE+Item.Panels[mvipCaption].Caption;
                                    end
                          else if   PtInRect(Item.Panels[mvipMenu    ].Rect,P) and  Item.Panels[mvipMenu    ].Enabled then begin
                                    Hint:=HintShowMenuText;        ItemPanelType:=mvipMenu;
                                   end
                          else if   PtInRect(Item.PanelsRect,P) then begin
                                    Item.ShowCaptionWithSecondaryMetrics;
                                    Hint:=MetricsText+COLON+SPACE+Item.Panels[mvipCaption].Caption;
                                    end
                               else begin Hint:='';
                                    if Assigned(Sender) then MouseOverSoundEnabled:=True;
                                    end;
                          end
                       else begin
                          if (mbLeft in MouseDownSet) and PtInRect(Selected.Rect,P) then begin
                             //if Screen.Cursor<>crDrag then Screen.Cursor:=crDrag;
                             //Image.Picture.BitMap.Canvas.Pen.Mode :=pmCopy;
                             //Image.Picture.BitMap.Canvas.Pen.Style:=psSolid;
                             //Image.Picture.BitMap.Canvas.Pen.Color:=clYellow;
                             //Image.Picture.BitMap.Canvas.LineTo(X,Y);
                             //Image.Picture.BitMap.Canvas.MoveTo(X,Y);
                             end;
                          MouseOverSoundEnabled:=True;
                          end;
                  end
               else MouseOverSoundEnabled:=True;
               end;
     end
  else begin
     HideDragRect;
     MouseOverSoundEnabled:=True;
     end;

  SetFocusedPanel(Item,ItemPanelType);

  Result:=DragRect.IsDragging or DragRect.IsSizing or Assigned(Item);
end;

function  TMultiView.OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer):Boolean;
var SerialNo:TSerialNo; P:TPoint; R:TRect; Item:TMultiViewItem;

  function  CloseItem(Item__:TMultiViewItem; SaveSnapshot__:Boolean):Boolean;
  var Snapshot:TSnapshot;
  begin
    Result:=False;
    if Assigned(MainForm.Sound) and MainForm.Sound.Enabled then
       MainForm.Sound.Play(stMenuSelect);
    if MainForm.MPlayer.Visible then MainForm.MPlayer.Hide;
    if MainForm.Game.IsIdleAndStopReplayingAndBrowsing then begin
       MainForm.TrackState:=tsWait; MainForm.ClearTrackBox(True);

       Result:=(not SaveSnapshot__) or (Item__<>Selected) or Item__.MakeSnapshot; // ensure that the selected item has an updated snapshot that is in sync with the current game state
       if Result then begin
          if   (not SaveSnapshot__)
               or
               (not Assigned(Item__.Snapshot))
               or
               (Item__.Snapshot.MoveCount<=Item__.Snapshot.ForcedInitialJumps) then begin // 'True': the view contains too few moves to be worth saving
               Snapshot:=Item__.Snapshot; Item__.Snapshot:=nil; // steal the snapshot from the view item
               Snapshot.Free; // destroy the snapshot;
               end
          else Result:=Item__.SaveSnapshot(fDisappearedItemsCount);

          if   Result then begin
               Self.CloseItem(Item__,False);

               fDisappearedItemsCount:=0;
               OnResize(nil); // recalculate information and maximize all view items so they fill out the screen
               if ReportDisappearedItems(DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
                  DoReportDisappearedItems:=False;
               end;
          end;
       end;
  end;

begin // 'OnMouseUp'
  Result:=False;
  Exclude(MouseDownSet,Button);
  if      (not (mbLeft in  MouseDownSet)) and
          (Screen.Cursor<>DEFAULT_CURSOR) and
          (IsSizeHandleCursor(Screen.Cursor)
           or
           ((Screen.Cursor=crDrag) and (not fIgnoreMouseUp))
          ) then
          Screen.Cursor:=DEFAULT_CURSOR;
  if      Button=mbLeft   then begin
          if        DragRect.IsDragging then begin
                    R:=DragRect.DragRect;
                    HideDragRect;
                    if Assigned(Selected) and
                       PointToItem(R.Left,R.Top,Item) and
                       (Item<>Selected) and
                       Item.Visible and
                       Selected.MakeSnapshot // the existing snapshot for the selected item may be out of sync with the current game state, hence, create a new updated snapshot
                       then begin
                       R:=Selected.Rect; Selected.Rect:=Item.Rect; Item.Rect:=R;
                       Item.Open;
                       Selected.Open;
                       CalculateMaximizedRectangles(True);
                       SetFocusedPanel(nil,mvipNone);
                       MouseOverSoundEnabled:=False;
                       OnMouseMove(nil,[],0,0);
                       end;
                    end
          else if   DragRect.IsSizing then begin
                    R:=DragRect.DragRect; HideDragRect;
                    if Assigned(Selected) and
                       (not IsEqualRects(R,Selected.Rect)) and
                       Selected.MakeSnapshot then
                       try     Screen.Cursor:=crHourGlass; fOpening:=Selected;
                               fDisappearedItemsCount:=0;
                               ResizeItem(Selected,R);
                               OnResize(Selected); // update the screen so all items are maximized
                               if ReportDisappearedItems(DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
                                  DoReportDisappearedItems:=False;
                       finally if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
                               fOpening:=nil;
                      end;
                    Result:=True;
                    end
               else if PointToItem(X,Y,Item) then begin
                       P:=Point(X,Y);
                       if        PtInRect(Item.Panels[mvipClose].Rect,P) then begin
                                 if Assigned(MainForm.Sound) and MainForm.Sound.Enabled then
                                    MainForm.Sound.Play(stMenuSelect);
                                 if MainForm.MPlayer.Visible then MainForm.MPlayer.Hide;
                                 if MainForm.Game.IsIdleAndStopReplayingAndBrowsing then begin
                                    CloseItem(Item,True);
                                    Result:=True; // 'True': the 'mouse up' event has been handled
                                    end;
                                 end
                       else if   PtInRect(Item.Panels[mvipMaximize].Rect,P) and Item.Panels[mvipMaximize].Enabled then begin
                                 if Assigned(MainForm.Sound) and MainForm.Sound.Enabled then
                                    MainForm.Sound.Play(stMenuSelect);
                                 if MainForm.MPlayer.Visible then MainForm.MPlayer.Hide;
                                 if MainForm.Game.IsIdleAndStopReplayingAndBrowsing then begin
                                    MainForm.TrackState:=tsWait; MainForm.ClearTrackBox(True);
                                    if (Item<>Selected) or (not Assigned(Selected)) or Selected.MakeSnapshot then begin // if there is a selected item, then its existing snapshot may be out of sync with the current game state, hence, create a new updated snapshot for it if the following Selected:=...' statement doesn't do it automatically in connection with a change of focus from one item to another
                                       Selected:=Item;
                                       fDisappearedItemsCount:=0;
                                       ResizeItem(Item,Item.MaxRectAfterSqueezingOtherItems);
                                       OnResize(nil); // update the screen so all items are maximized
                                       if ReportDisappearedItems(DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
                                          DoReportDisappearedItems:=False;
                                       end;
                                    Result:=True;
                                    end;
                                 end
                       else if   PtInRect(Item.Panels[mvipMinimize].Rect,P) and Item.Panels[mvipMinimize].Enabled then begin
                                 if Assigned(MainForm.Sound) and MainForm.Sound.Enabled then
                                    MainForm.Sound.Play(stMenuSelect);
                                 if MainForm.MPlayer.Visible then MainForm.MPlayer.Hide;
                                 if MainForm.Game.IsIdleAndStopReplayingAndBrowsing then begin
                                    MainForm.TrackState:=tsWait; MainForm.ClearTrackBox(True);
                                    if (Item<>Selected) or (not Assigned(Selected)) or Selected.MakeSnapshot then begin // if there is a selected item, then its existing snapshot may be out of sync with the current game state, hence, create a new updated snapshot for it if the following Selected:=...' statement doesn't do it automatically in connection with a change of focus from one item to another
                                       Selected:=Item;
                                       R:=Item.Rect; SerialNo:=Item.SerialNo;
                                       // try to move the minimized item to the top-left corner
                                       fDisappearedItemsCount:=0;
                                       ResizeItem(Item,Classes.Rect(R.Left,R.Top,R.Left+Item.MinWidth,R.Top+Item.MinHeight));
                                       OnResize(GetItemBySerialNo(SerialNo)); // update the screen so all items are maximized
                                       Item:=GetItemBySerialNo(SerialNo); // the item may have disappeared, hence, this lookup is necessary
                                       if Assigned(Item) and EqualRect(R,Item.Rect) then with R do begin // 'True': the item didn't change position or size; try again, this time moving the item to the top-right corner
                                          ResizeItem(Item,Classes.Rect(Right-Item.MinWidth,Top,Right,Top+Item.MinHeight));
                                          OnResize(GetItemBySerialNo(SerialNo)); // update the screen so all items are maximized
                                          Item:=GetItemBySerialNo(SerialNo);
                                          if Assigned(Item) and EqualRect(R,Item.Rect) then with R do begin // 'True': the item didn't change position or size; try again, this time moving the item to the bottom-left corner
                                             ResizeItem(Item,Classes.Rect(Left,Bottom-Item.MinHeight,Left+Item.MinWidth,Bottom));
                                             OnResize(GetItemBySerialNo(SerialNo)); // update the screen so all items are maximized
                                             Item:=GetItemBySerialNo(SerialNo); // the item may have disappeared, hence, this lookup is necessary
                                             if Assigned(Item) and EqualRect(R,Item.Rect) then with R do begin // 'True': the item didn't change position or size; try again, this time moving the item to the bottom-right corner
                                                ResizeItem(Item,Classes.Rect(Right-Item.MinWidth,Bottom-Item.MinHeight,Right,Bottom));
                                                OnResize(GetItemBySerialNo(SerialNo)); // update the screen so all items are maximized
                                                Item:=GetItemBySerialNo(SerialNo); // the item may have disappeared, hence, this lookup is necessary
                                                end;
                                             end;
                                          end;
                                       if Assigned(Item) then
                                          Item.ShowPanel(mvipMinimize,ViewItemPanelCaptionMinimize,False,Item.Panels[mvipMinimize].Rect,Item.Panels[mvipMinimize].Alignment);
                                       ReportDisappearedItems(False,'');
                                       end;
                                    Result:=True;
                                    end;
                                 end
                       else if   PtInRect(Item.Panels[mvipSplit].Rect,P) and Item.Panels[mvipSplit].Enabled then begin
                                 if Assigned(MainForm.Sound) and MainForm.Sound.Enabled then
                                    MainForm.Sound.Play(stMenuSelect);
                                 MouseOverSoundEnabled:=False;
                                 if MainForm.MPlayer.Visible then MainForm.MPlayer.Hide;
                                 if MainForm.Game.IsIdleAndStopReplayingAndBrowsing then begin
                                    MainForm.TrackState:=tsWait; MainForm.ClearTrackBox(True);
                                    Selected:=Item;
                                    SplitItem(Item,SplitAxis,False);
                                    Result:=True;
                                    end;
                                 end
                       else if   PtInRect(Item.Panels[mvipMenu].Rect,P) and Item.Panels[mvipMenu].Enabled and
                                 Assigned(MainForm.MultiViewPopupMenu) then with MainForm.PanelMultiViewMenu do begin
                                 if Assigned(MainForm.Sound) and MainForm.Sound.Enabled then
                                    MainForm.Sound.Play(stMenuSelect);
                                 MouseOverSoundEnabled:=False;
                                 if MainForm.MPlayer.Visible then MainForm.MPlayer.Hide;
                                 if MainForm.Game.IsIdleAndStopReplayingAndBrowsing then begin
                                    MainForm.TrackState:=tsWait; MainForm.ClearTrackBox(True);
                                    Selected:=Item;
                                    P:=Image.ClientToScreen(P);

                                    Width:=Min(Tag,Image.ClientWidth); // the panel stores its ideal width in 'Tag'
                                    Left:=Max(0,Min(Item.Panels[mvipMenu].Rect.Left,ClippingRect.Right -Width ));
                                    Top :=Max(0,Min(Item.Panels[mvipMenu].Rect.Top ,ClippingRect.Bottom-Height));

                                    MainForm.PanelMultiViewCombineSnapshotsToFormSolutions.Caption:=CombineWithOppositeDirectionSnapshotsToFormSolutionsText[MainForm.Game.ReverseMode];
                                    MainForm.PanelMultiViewCombineSnapshotsToFormSolutions.Hint:=HintCombineWithOppositeDirectionSnapshotsToFormSolutionsText[MainForm.Game.ReverseMode];

                                    MainForm.PanelMultiViewCopyMovesToClipboard.Enabled:=MainForm.Game.History.Count>0;
                                    MainForm.PanelMultiViewCopyContinuationMovesToClipboard.Enabled:=MainForm.Game.History.Count<MainForm.Game.History.Top;
                                    MainForm.PanelMultiViewPasteMovesFromClipboard.Enabled:=Clipboard.HasFormat(CF_TEXT);
                                    MainForm.PanelMultiViewCombineSnapshotsToFormSolutions.Enabled:=CombineSnapshotsToFormSolutions(False)<>0;
                                    MainForm.PanelMultiViewMoveToSnapshots.Enabled:=MainForm.Game.History.Count>MainForm.Game.ForcedInitialJumps;

                                    MainForm.MultiViewPopupMenu.Show;

                                    Result:=True;
                                    end;
                                 end;
                       end
                    else begin
                       //if Items.IsEmpty then SplitItem(nil,SplitAxis,False);
                       //if PointToItem(X,Y,Item) then Selected:=Item;
                      end;
          end
  else if Button=mbRight  then begin
          P:=Point(X,Y);
          if (not DragRect.IsSizing) and
             PointToItem(X,Y,Item) and
             PtInRect(Item.Panels[mvipClose].Rect,P) then begin
             CloseItem(Item,False);
             Result:=True; // 'True': the 'mouse up' event has been handled
             end
          else begin
             //MainForm.Close;
             end;
          end
  else if Button=mbMiddle then begin
          end;
  HideDragRect;
  Hint:='';

  fIgnoreMouseUp:=False;
  MouseOverSoundEnabled:=False;
  OnMouseMove(Sender,Shift,X,Y);
  MouseOverSoundEnabled:=True;
end;

procedure TMultiView.OnResize(MaximizeThisItemLast__:TMultiViewItem);
var OldCursor:TCursor; AnotherItem,Item:TMultiViewItem;

  function  TryToMoveItem(Item__:TMultiViewItem):Boolean;
  var NewRect:TRect;
  begin // the item is too small after clipping; try to move it left and/or up to keep it on the screen, thereby squeezing some of the other items
    NewRect       :=Item__.Rect;
    NewRect.Left  :=Max(ClippingRect.Left  ,NewRect.Left-Max(0,Item__.MinWidth -Max(0,RectWidth (NewRect))));
    NewRect.Top   :=Max(ClippingRect.Top   ,NewRect.Top -Max(0,Item__.MinHeight-Max(0,RectHeight(NewRect))));
    NewRect.Right :=Min(ClippingRect.Right ,NewRect.Right );
    NewRect.Bottom:=Min(ClippingRect.Bottom,NewRect.Bottom);
    Result        :=ResizeItem(Item__,NewRect);
  end; // OnResize.TryToMoveItem

begin // 'OnResize'
  OldCursor:=Screen.Cursor;
  try
           Screen.Cursor:=crHourGlass;
           Sort(scTop); // so filling gaps after any newly closed items is done in a well-defined order

           if Assigned(Selected) then begin
              if Assigned(MainForm.Game) and
                 MainForm.Game.IsIdleAndStopReplayingAndBrowsing and
                 (not Selected.MakeSnapshot) then
                 Msg(TEXT_MEMORY_FULL,MainForm.Caption,MB_OK+MB_ICONERROR); // the views must be resized even if a new snapshot couldn't be created; continue with the existing one;
              Items.MoveToBack(Selected); // if there is a selected item, then protect it against deletion caused by overlaps (instead, delete the other ones)
              end;

           Item:=TMultiViewItem(Items.First);
           while Assigned(Item) do begin
             // recalculate panel height for each item; font settings may have changed
             Item.PanelHeight:=Item.CalculatePanelHeight;

             // guard against overlapping views;
             // it cannot be ruled out completely that 'ResizeItem' and
             // 'TViewItem.Maximize' produce overlapping views, so for safety,
             // there is a guard against overlaps here;
             if   Item.IntersectsAnotherItem(AnotherItem) then begin // True': an intersecting item has been found;
                  CloseItem(Item,True);
                  Item:=TMultiViewItem(Items.First); // the item disappeared; start over again from the top of the list
                  end
             else Item:=TMultiViewItem(Item.Next)    // advance to the next item on the list
             end;

           Items.MoveToFront(Selected); // so the selected item, if any, gets the first chance to occupy any vacant screen space
           Items.MoveToBack (MaximizeThisItemLast__); // so a selected and resized item gives the other items the first chance to occupy any newly released space

           // clip the views to the clipping rectangle
           Item:=TMultiViewItem(Items.First);
           while Assigned(Item) do begin
             ClipRect(Item.Rect,ClippingRect);
             Item:=TMultiViewItem(Item.Next)         // advance to the next item on the list
             end;

           // resize the items, if necessary
           Item:=TMultiViewItem(Items.First);
           while Assigned(Item) do begin
             if   Item.CanResize(Item.Rect)          // 'True': the size is OK
                  or
                  TryToMoveItem(Item) then           // 'True': keeping the item on the screen succeeded after moving or resizing it
                  Item:=TMultiViewItem(Item.Next)    // advance to the next item on the list
             else Item:=TMultiViewItem(Items.First); // the item disappeared; start over again from the top of the list
             end;

           // maximize the views on the screen; reserve a region on the screen for each view, but don't show the views yet
           Item:=TMultiViewItem(Items.First);
           while  Assigned(Item) do begin
             if   Item.Maximize(False) then          // 'Maximize' returns 'True' if the item still is on the screen; the 'False' parameter means 'do not show the the item, just reserve a region on the screen'
                  Item:=TMultiViewItem(Item.Next)
             else Item:=TMultiViewItem(Items.First); // if 'Item.Maximize' failed then the item has been destroyed; start over again from the top of the list
             end;

  finally  CalculateMaximizedRectangles(False);

           // show the view items on the screen with the selected item, if any, as the last one so it isn't loaded repeatedly
           Items.MoveToBack(Selected);
           Item:=TMultiViewItem(Items.First);
           while Assigned(Item) do begin
             Item.Open;
             Item:=TMultiViewItem(Item.Next);
             end;

           if Screen.Cursor<>OldCursor then Screen.Cursor:=OldCursor;
           SetFocusedPanel(nil,mvipNone);
           MouseOverSoundEnabled:=False;
           OnMouseMove(nil,[],0,0);
           if Assigned(MainForm.Game) then MainForm.ShowTitle(MainForm.Game.FileName);
  end;
end;

function  TMultiView.Refresh:Boolean;
var OldCursor:TCursor; Item:TMultiViewItem;
begin
  Result:=(not Assigned(Selected)) or Selected.MakeSnapshot; // the snapshot for the current item may be out of sync with the current game state, hence create a new updated snapshot

  fDisappearedItemsCount:=0;
  OldCursor:=Screen.Cursor;

  if Result then
     try
       Screen.Cursor:=crHourGlass;
        // hide all the views
       Item:=TMultiViewItem(Items.First);
       while Assigned(Item) do begin
         Item.Hide;
         Item:=TMultiViewItem(Item.Next);
         end;

       // show all the view items on the screen with the selected item, if any, as the last one so it isn't loaded repeatedly
       Items.MoveToBack(Selected);
       Item:=TMultiViewItem(Items.First);
       while Assigned(Item) do begin
             Item.PanelHeight:=Item.CalculatePanelHeight;
             if Item.CanResize(Item.Rect) then begin
                Item.Open;
                Item:=TMultiViewItem(Item.Next);
                end
             else begin
                CloseItem(Item,True);
                Item:=TMultiViewItem(Items.First); // the item disappeared; start over again from the top of the list
                end;
             end;
     finally
       if Screen.Cursor<>OldCursor then Screen.Cursor:=OldCursor;
       SetFocusedPanel(nil,mvipNone);
       OnMouseMove(nil,[],0,0);
       if ReportDisappearedItems(DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
          DoReportDisappearedItems:=False;
     end;
end;

function  TMultiView.RefreshItem(Item__:TMultiViewItem):Boolean;
begin
  Result:=Assigned(Item__)
          and
          ((Item__=Selected)
           or
           (not Assigned(Selected))
           or
           Selected.MakeSnapshot // the snapshot for the current item may be out of sync with the current game state, hence create a new updated snapshot
          );
  if Result then begin
     Item__.Open;
     if (Item__<>Selected) and Assigned(Selected) then Selected.Open;
     end;
end;

function  TMultiView.ReportDisappearedItems(ReportDisappearedItems__:Boolean; const AdditionalText__:String):Boolean;
var s:String;
begin
  Result:=(DisappearedItemsCount<>0) and ReportDisappearedItems__;
  if Result then begin
     if   DisappearedItemsCount=1 then
          s:=ViewDisappearedText
     else s:=Format(ViewsDisappearedText__,[DisappearedItemsCount]);
     if AdditionalText__<>'' then s:=s+NL+NL+AdditionalText__;
     Msg(s,MainForm.Caption,MB_OK+MB_ICONINFORMATION);
     end;
  fDisappearedItemsCount:=0;
end;

function  TMultiView.ResizeItem(Item__:TMultiViewItem; const NewRect__:TRect):Boolean;
var OldCursor:TCursor;

  function  CalculateMaxNewRect(Item__:TMultiViewItem; const NewRect__:TRect):TRect;
  // calculates a maximum new rectangle, making it unlikely that items are pushed off the screen (there is no guarantee though)
  var Direction:TDirection; ClippingRect,IntersectionRect:TRect; TreeHeights:TTreeHeights; Item:TMultiViewItem;
  begin
    for Direction:=Low(TreeHeights) to High(TreeHeights) do
        TreeHeights[Direction]:=Max(0,Pred(Item__.TreeHeights[Direction])); // 'Pred': don't count the item itself

    Item:=TMultiViewItem(Items.First);
    while Assigned(Item) do begin
      if (Item<>Item__) and
         Item.Visible and
         IntersectRect(IntersectionRect,Item.Rect,NewRect__) then // 'True': the item 'Item' overlaps the item 'Item__' when the latter occupies the region 'NewRect__'
         for Direction:=Low(TreeHeights) to High(TreeHeights) do
             TreeHeights[Direction]:=Max(TreeHeights[Direction],Pred(Item.TreeHeights[Direction])); // 'Pred': don't count the item itself; this makes the function a heuristic and not a guaranteed maximum rectangle; it would be correct to count the item itself, but that would be so restrictive that it almost prohibit enlarging a view
      Item:=TMultiViewItem(Item.Next);
      end;

    ClippingRect:=Self.ClippingRect;
    if NewRect__.Left   <Item__.Rect.Left   then Inc(ClippingRect.Left  ,Item__.MinWidth *TreeHeights[Left ]);
    if NewRect__.Top    <Item__.Rect.Top    then Inc(ClippingRect.Top   ,Item__.MinHeight*TreeHeights[Up   ]);
    if NewRect__.Right  >Item__.Rect.Right  then Dec(ClippingRect.Right ,Item__.MinWidth *TreeHeights[Right]);
    if NewRect__.Bottom >Item__.Rect.Bottom then Dec(ClippingRect.Bottom,Item__.MinHeight*TreeHeights[Down ]);

    Result:=NewRect__;

    ClipRect(Result,ClippingRect);
  end; // ResizeItem.CalculateMaxNewRect

  function  Resize(Item__:TMultiViewItem; const NewRect__:TRect; RecursionDepth__:Integer):Boolean;
  const MAX_RECURSION_DEPTH=1000; // each recursive invocation of 'Resize' requires ~150 bytes on the stack
  var   OldRect,NewRect:TRect; DuplicateItem:TMultiViewItem;

    function  ResizeNeighbors(const ResizedItem__:TMultiViewItem; const OldRect__:TRect):Boolean;
    var LoopCount,PendingCount:Integer; DeltaRect,IntersectionRect,NewRect:TRect; Item,NextItem:TMultiViewItem;
    begin // 'ResizeItem.Resize.ResizeNeighbors';
          // resizes, moves, or closes neighbor items if necessary, so
          // 'ResizedItem__' can occupy the new area on the screen stored in
          // 'ResizedItem__.Rect';
          //
          // the old area is given by 'OldRect__' and is used for calculating
          // in which directions the neighbor items should move;
          //
          // the function returns 'False' if 'ResizedItem__' collides with
          // another item which already has reserved a part of the same region
          // on the screen;

       Result:=True;

       // calculate the movement, and use the directions to decide which way to move overlapping neighbors, if any
       DeltaRect.Top   :=ResizedItem__.Rect.Top   -OldRect__.Top;
       DeltaRect.Left  :=ResizedItem__.Rect.Left  -OldRect__.Left;
       DeltaRect.Right :=ResizedItem__.Rect.Right -OldRect__.Right;
       DeltaRect.Bottom:=ResizedItem__.Rect.Bottom-OldRect__.Bottom;

       LoopCount:=0;
       repeat
         Inc(LoopCount); PendingCount:=0;

         Item:=TMultiViewItem(Items.First);
         while Result and Assigned(Item) do begin
           NextItem:=TMultiViewItem(Item.Next);
           if (Item<>ResizedItem__) and //'True': this is not the item being moved by this call to 'ResizeItem.Resize'
              IntersectRect(InterSectionRect,Item.Rect,ResizedItem__.Rect) then // 'True': the item overlaps the item being moved, i.e., 'ResizedItem__'
              if Item.Visible then begin // 'True': the item isn't locked; the only hidden items are the ones currently are being processed, i.e., the ones for which new co-ordinates have been assigned by one of the recursive calls to 'ResizeItem.Resize'
                 if (LoopCount>1) or // 'True': resize all overlapping items in the second pass through the loop
                    Item.IsNeighbor(ResizedItem__) then begin // 'True': 'Item' touches the resized item 'ResizedItem__', in which case 'Item' should be resized during the first pass;
                    NewRect:=Item.Rect;

                    if (DeltaRect.Top<0) and    // 'True': 'ResizedItem__' moves up
                       (NewRect.Bottom<=OldRect__.Top) then begin
                       if (Item.BoardRect.Right<>0) and
                          (Item.BoardRect.Right+MULTI_VIEW_ITEM_BOARD_BORDER_SIZE<=IntersectionRect.Left) then // 'True': the overlap doesn't collide with the board
                          NewRect.Right:=IntersectionRect.Left
                       else begin
                          NewRect.Bottom:=Min(NewRect.Bottom,ResizedItem__.Rect.Top);
                          Dec(NewRect.Top   ,Max(0,Item.MinHeight-RectHeight(NewRect)));
                          end;
                       end;
                    if (DeltaRect.Left<0) and   // 'True': 'ResizedItem__' moves left
                       (NewRect.Right<=OldRect__.Left) and
                       IntersectRect(InterSectionRect,NewRect,ResizedItem__.Rect) then begin
                       if (Item.BoardRect.Bottom<>0) and
                          (Item.BoardRect.Bottom+MULTI_VIEW_ITEM_BOARD_BORDER_SIZE<=IntersectionRect.Top) then // 'True': the overlap doesn't collide with the board
                          NewRect.Bottom:=IntersectionRect.Top
                       else begin
                          NewRect.Right:=Min(NewRect.Right,ResizedItem__.Rect.Left);
                          Dec(NewRect.Left  ,Max(0,Item.MinWidth -RectWidth (NewRect)));
                          end;
                       end;
                    if (DeltaRect.Right>0) and  // 'True': 'ResizedItem__' moves right
                       (NewRect.Left>=OldRect__.Right) and
                       IntersectRect(InterSectionRect,NewRect,ResizedItem__.Rect) then begin
                       if (Item.BoardRect.Bottom<>0) and
                          (Item.BoardRect.Bottom+MULTI_VIEW_ITEM_BOARD_BORDER_SIZE<=IntersectionRect.Top) then // 'True': the overlap doesn't collide with the board
                          NewRect.Bottom:=IntersectionRect.Top
                       else begin
                          NewRect.Left:=Max(NewRect.Left,ResizedItem__.Rect.Right);
                          Inc(NewRect.Right ,Max(0,Item.MinWidth -RectWidth (NewRect)));
                          end;
                       end;
                    if (DeltaRect.Bottom>0) and // 'True': 'ResizedItem__' moves down
                       (NewRect.Top>=OldRect__.Bottom) and
                       IntersectRect(InterSectionRect,NewRect,ResizedItem__.Rect) then begin
                       if (Item.BoardRect.Right<>0) and
                          (Item.BoardRect.Right+MULTI_VIEW_ITEM_BOARD_BORDER_SIZE<=IntersectionRect.Left) then // 'True': the overlap doesn't collide with the board
                          NewRect.Right:=IntersectionRect.Left
                       else begin
                          NewRect.Top:=Max(NewRect.Top,ResizedItem__.Rect.Bottom);
                          Inc(NewRect.Bottom,Max(0,Item.MinHeight-RectHeight(NewRect)));
                          end;
                       end;

                    if not Resize(Item,NewRect,Succ(RecursionDepth__)) then begin // 'True': moving the item out of the way failed
                       // the item has been closed by 'Resize' so it doesn't block the area on the screen reserved for 'ResizedItem__'
                       end;
                    NextItem:=TMultiViewItem(Items.First); // start over again from the top of the list; after the recursive call to 'ResizeItem.Resize', items may have disappeared from the list and new ones may have been added
                    end
                 else
                    // postpone resizing this item;
                    // 'Item.IsNeighbor()' only returns 'True' if the items are
                    // touching each other; there may be a gap between them, and
                    // if there is, and if the item isn't visited and resized
                    // (enough) by a recursive call, then the item is resized in
                    // a second pass through the enclosing 'repeat..until' loop
                    Inc(PendingCount);
                 end
              else
                 // the overlapping item is hidden, which also means it's locked
                 // because the only hidden items are the ones for which new
                 // co-ordinates have been assigned by one of the recursive
                 // calls to 'ResizeItem.Resize'
                 Result:=False;
           Item:=NextItem; // advance to the next item on the list
           end;
       until PendingCount=0; // until all overlapping neighbors have been resized or closed, or until the function failed freeing the requested region of the screen for the item 'ResizedItem__'
    end; // ResizeItem.Resize.ResizeNeighbors

  begin // 'ResizeItem.Resize'
    OldRect:=Item__.Rect; // remember the original area; the function 'ResizeNeighbors' uses it for calculating in which direction neighboring items should move; furthermore, it's required for updating the 'Modified' flag in case the item moves or changes size
    NewRect:=NewRect__;
    ClipRect(NewRect,ClippingRect);

    Item__.Hide; // hidden items are also locked; they reserve the area 'NewRect' on the screen, and any overlapping neighbors are moved away or closed

    Result:=(RecursionDepth__<MAX_RECURSION_DEPTH)
            // in practice, it's extremely unlikely that the user has so
            // many views on the screen that the recursive 'ResizeItem.Resize()'
            // function can cause a stack overflow;
            // for safety, however, the application guards against a stack
            // overflow here by closing some of the views if the recursion goes
            // too deep;
            and
            Item__.CanResize(NewRect);
    if Result then begin
       Item__.Rect:=NewRect; // update the item with the new region
       Result:=ResizeNeighbors(Item__,OldRect); // 'True': making room for the resized item succeeded
       end;

    if not Items.IsEmpty then begin // 'True': some views are still on the screen; the recursive calls to 'Neighbors' may have closed some or all of the views
       Result:=(Result or GetSpaceFromAnotherItem(Item__)) and Item__.Resize(Item__.Rect,False);
       if Result then begin
          Item__.SetVisibleWithoutUpdatingScreen(True); // hidden items are locked; release the lock by setting 'Visible' := 'True' even though the item really are hidden; the caller must launch 'OnResize()' anyway to refresh the screen after 'ResizeItem()' returns, so all view items will appear on the screen again at that time
          if not IsEqualRects(OldRect,Item__.Rect) and
             (RectWidth(OldRect)*RectHeight(OldRect)<>0) and // '0': the old region is empty the first time the item is processed
             Assigned(Item__.Snapshot) and Assigned(Item__.Snapshot.Notes) then
             Item__.Snapshot.Notes.Modified:=True; // mark notes as modified instead of the snaphot itself; it signals that the snapshot itself is intact, in case that information comes in handy at some time
          end
       else begin                   // 'True': the item cannot be resized/moved to the given size/position; close the item
          if (Items.Count=2) and    // 'True': closing the view automatically closes the other remaining view too
             (Item__=Selected) then // ensure that it's the selected view which is restored for game play when the screen returns to 'normal', i.e., without multiple views
             if   Item__=Items.First then
                  Item__:=TMultiViewItem(Item__.Next)
             else Item__:=TMultiViewItem(Items.First);
          CloseItem(Item__,not FindDuplicateItem(Item__,True ,True,False,DuplicateItem)); // 'not FindDuplicateItem(...)' if the item is a duplicate, then drop it silently instead of saving it to the 'Snapshots' window
          end;
       end
    else Result:=False;

  end; // ResizeItem.Resize

begin // 'ResizeItem'
  OldCursor:=Screen.Cursor;
  try     Screen.Cursor:=crHourGlass;
          Result:=Resize(Item__,CalculateMaxNewRect(Item__,NewRect__),0);
  finally if Screen.Cursor<>OldCursor then Screen.Cursor:=OldCursor;
          SetFocusedPanel(nil,mvipNone);
          OnMouseMove(nil,[],0,0);
          if Items.Count=1 then begin // 'True': there is only one view left on the screen; close it and return to normal game play without multiple views
             CloseItem(TMultiViewItem(Items.First),False);
             Result:=False;
             end;
  end;
end;

function  TMultiView.SaveSettingsToIniFile(const IniFile:TIniFile; const Section:String):Boolean;
begin
  IniFile.WriteInteger(Section,'BackgroundColor',Integer(BackgroundColor));
  IniFile.WriteInteger(Section,'BackgroundTransparencyPct',BackgroundTransparencyPct);
  IniFile.WriteInteger(Section,'FocusedBackgroundColor',Integer(FocusedBackgroundColor));
  IniFile.WriteInteger(Section,'FocusedTextColor',Integer(FocusedTextColor));
  IniFile.WriteInteger(Section,'LineColor',Integer(LineColor));
  IniFile.WriteInteger(Section,'ShadowColor',Integer(ShadowColor));
  IniFile.WriteInteger(Section,'TextColor',Integer(TextColor));
  Result:=SaveFontToIniFile(IniFile,Section,'',Font);
end;

function  TMultiView.SaveSnapshots:Boolean;
var Count:Cardinal; Item:TMultiViewItem;
begin // transfers all the snapshots to the game, i.e., adds the snapshots to the 'MainForm.Game.Snapshots' list
  Result:=Assigned(MainForm.Game)
          and
          MainForm.Game.IsIdleAndStopReplayingAndBrowsing
          and
          ((not Assigned(Selected))
           or
           Selected.MakeSnapshot // the existing snapshot for the selected item may be out of sync with the current game state, hence, create a new updated snapshot
          );
  if Result then begin
     Item:=TMultiViewItem(Items.First); Count:=0;
     while Assigned(Item) do begin
       Item.SaveSnapshot(Count);
       Item:=TMultiViewItem(Item.Next)
       end;
     end;
end;

procedure TMultiView.SetDefaultValues;
begin
  BackgroundColor:=DEFAULT_BACKGROUND_COLOR;
  BackgroundTransparencyPct:=DEFAULT_BACKGROUND_TRANSPARENCY_PCT;
  CaptionFontSize:=DEFAULT_CAPTION_FONT_SIZE;
  FocusedBackgroundColor:=DEFAULT_FOCUSED_BACKGROUND_COLOR;
  FocusedTextColor:=DEFAULT_FOCUSED_TEXT_COLOR;
  Font.Name:='Arial';
  Font.Size:=DEFAULT_STATUS_PANEL_FONT_SIZE[IsAHighResolutionScreen];
  Font.Color:=clWhite;
  Font.Style:=[];
  LineColor:=DEFAULT_LINE_COLOR;
  ShadowColor:=DEFAULT_SHADOW_COLOR;
  TextColor:=DEFAULT_TEXT_COLOR;
  SplitAxis:=saBest;
end;

procedure TMultiView.SetHint(const Hint__:String);
begin
  if Hint__<>Hint then begin
     fHint:=Hint__;
     MainForm.Status.Hint:=Hint;
     end;
end;

procedure TMultiView.SetFocusedPanel(Item__:TMultiViewItem; PanelType__:TMultiViewItemPanelType);
var OldFocusedItem:TMultiViewItem; OldFocusedItemPanelType:TMultiViewItemPanelType;
begin
 if (Item__<>FocusedItem) or (PanelType__<>FocusedItemPanelType) then begin
    OldFocusedItem:=FocusedItem; OldFocusedItemPanelType:=FocusedItemPanelType;
    FocusedItem:=Item__; FocusedItemPanelType:=PanelType__;
    if   Assigned(OldFocusedItem) and (OldFocusedItemPanelType<>mvipNone) then with OldFocusedItem do with Panels[OldFocusedItemPanelType] do
         ShowPanel(OldFocusedItemPanelType,Caption,Enabled,Rect,Alignment);
    if   Assigned(FocusedItem) and (FocusedItemPanelType<>mvipNone) then with FocusedItem do with Panels[FocusedItemPanelType] do
         ShowPanel(FocusedItemPanelType,Caption,Enabled,Rect,Alignment);

    if   Assigned(FocusedItem) and (FocusedItemPanelType<>mvipNone) and
         Assigned(MainForm.Sound) and MainForm.Sound.Enabled then
         if   MouseOverSoundEnabled then
              MainForm.Sound.Play(stMenuOver)
         else MouseOverSoundEnabled:=True;
    end;
end;

procedure TMultiView.SetSelected(Item__:TMultiViewItem);
var OldSelected:TMultiViewItem;
begin
  if Item__<>Selected then begin
     OldSelected:=Selected;
     fSelected:=Item__;
     if   Assigned(OldSelected) and OldSelected.Visible then begin
          MainForm.TrackState:=tsWait; MainForm.ClearTrackBox(True);
          if OldSelected.MakeSnapshot then // ok
          else begin
            // creating a new snapshot failed;
            // there is nothing to do about it since the caller must be able to
            // trust that the newly selected item 'Item__' really has been
            // selected as the new item when 'SetSelected' returns;

            //fSelected:=OldSelected; // don't change focus when saving the current game state failed for the currently selected item;
            end;
          OldSelected.Open; // set the correct 'selected/unselected' appearance on the screen for the old item
          end;
     if   (Selected=Item__) and // 'True': the statement above didn't prevent the selection of the new item
          Assigned(Selected) then begin
          if Selected.Visible then begin
             Selected.Open;
             MainForm.ShowTitle(MainForm.Game.FileName);
             end;
          end;
     end;
end;

procedure TMultiView.ShowBackground(const Rect__:TRect);
var R:TRect;
begin
  R:=Rect__; ClipRect(R,ClippingRect);
  if (RectWidth(R)>0) and (RectHeight(R)>0) then with Image.Picture.BitMap.Canvas do begin
     if MainForm.GameViewer.Initialized then with MainForm.GameViewer.Pictures[ptScreenBackground] do begin
        if Visible and (BitMap<>nil) then begin
           CopyMode:=cmSrcCopy; CopyRect(R,BitMap.Canvas,R);
           end
        else begin // no screen background picture
           Brush.Color:=Color;
           FillRect(R); // clear rectangle
           end;
        end
     else begin
        Brush.Style:=bsSolid; Brush.Color:=clNavy;
        FillRect(R);
       end;
    end;
end;

procedure TMultiView.ShowDragRect;
begin
  with DragRect do
    if IsDragging or IsSizing or IsZooming then
       with Image.Picture.BitMap.Canvas do begin
         Pen.Color   := FocusedTextColor;
         Pen.Mode    := pmXor;
         Pen.Style   := psDot; // only available with 'Pen.Width = 1'
         Pen.Width   := 1;
         Brush.Style := bsClear;
         with DragRect do Rectangle(Left,Top,Right,Bottom);
        end;
end;

procedure TMultiView.SetCursor(X__,Y__:Integer);
var Cursor:TCursor;
begin
  if   Assigned(Selected) then
       if        DragRect.IsDragging then
                 Cursor:=crDrag
       else if   DragRect.IsSizing then
                 Cursor:=SizeHandleToCursor(DragRect.SizeHandle)
            else Cursor:=SizeHandleToCursor(PointToSizeHandle(Point(X__,Y__),Selected.Rect,SIZE_HANDLE_RECT_DELTA))
  else Cursor:=DEFAULT_CURSOR;

  if   (Screen.Cursor<>Cursor)
       and
       (not (Assigned(Opening) and (Screen.Cursor=crHourGlass))) then
       Screen.Cursor:=Cursor; // update the screen cursor; it differs from the one selected by the 'multiple views' manager itself
end;

procedure TMultiView.Sort(SortCriteria__:TMultiViewItemsSortCriteria);
var OldSortCriteria:TMultiViewItemsSortCriteria;
begin
  OldSortCriteria:=MultiViewItemsSortCriteria;
  try     MultiViewItemsSortCriteria:=SortCriteria__;
          Items.MergeSort(MultiViewItemsCompareFunction);
  finally MultiViewItemsSortCriteria:=OldSortCriteria;
  end;
end;

function  TMultiView.SplitItem(Item__:TMultiViewItem; SplitAxis__:TSplitAxis; KeepSelectedItem__:Boolean):TMultiViewItem;
var OldCursor:TCursor; OldIsVisible:Boolean; OldSelected,OldItem:TMultiViewItem; NewRect:TRect;
begin // special: if 'Item__' is 'nil' then any existing views are destroyed and a new set of items is created
  Result:=nil;
  OldCursor:=Screen.Cursor;
  try
    Screen.Cursor:=crHourGlass;
    OldItem:=Item__;

    if not Assigned(Item__) and MakeItem(Item__) then begin
       Items.Clear;
       Item__.Rect:=ClippingRect;
       Items.Push(Item__);
       CalculateMaximizedRectangles(True);
       MouseOverSoundEnabled:=True;
       fSelected:=Item__;
       end;

    if Assigned(Item__) and
       Item__.CanSplit(SplitAxis__) and
       MakeItem(Result)  and
       ((not Assigned(Selected)) or Selected.MakeSnapshot) then begin // if there is a selected item, then its existing snapshot may be out of sync with the current game state, hence, create a new updated snapshot for it
       OldSelected:=Selected; fSelected:=nil;
       OldIsVisible:=Item__.Visible or (not Assigned(OldItem)); // 'OldItem': if this is a new set of views, then both of the new views should be visible upon exit from this function
       Item__.Hide;

       NewRect:=Item__.SplitRect(SplitAxis__);
       if   NewRect.Right=Item__.Rect.Right then // 'True': split horizontally
            Result.Rect:=Classes.Rect(Item__.Rect.Left                   ,Item__.Rect.Top+RectHeight(NewRect),Item__.Rect.Right,Item__.Rect.Bottom)
       else Result.Rect:=Classes.Rect(Item__.Rect.Left+RectWidth(NewRect),Item__.Rect.Top                    ,Item__.Rect.Right,Item__.Rect.Bottom);

       if not Assigned(OldItem) then ShowBackground(ClippingRect); // clear the entire background area before the new views pop up on the screen

       Items.Add(Result);
       if OldIsVisible then Result.Open; // only show the new item if the old one is visible; 'ResizeItem' uses 'hidden' to mean 'locked'

       Item__.Rect:=NewRect; // update region for the old item which has been split
       if   Assigned(Item__.Snapshot) and Assigned(Item__.Snapshot.Notes) then
            Item__.Snapshot.Notes.Modified:=True; // mark notes as modified instead of the snaphot itself; it signals that the snapshot itself is intact, in case that information comes in handy at some time

       if   KeepSelectedItem__ and (Item__=OldSelected) and OldIsVisible then begin
            fSelected:=Item__; // so 'Item__.Open' highlights the selected item
            Item__.Open;
            end
       else if OldIsVisible then Item__.Open;

       if   KeepSelectedItem__ then
            Selected:=OldSelected
       else OnMouseMove(nil,[],0,0);
       end
    else begin
       Result.Free; Result:=nil;
       if (not Assigned(OldItem)) and Assigned(Item__) then CloseItem(Item__,False); // 'True': the first view item in the set has been created, but splitting failed
       end;
  finally
    CalculateMaximizedRectangles(True);
    if Screen.Cursor<>OldCursor then Screen.Cursor:=OldCursor;
  end;
end;

end.

