unit Menu_;

interface

uses Windows,Classes,Graphics,Buttons,Controls,ExtCtrls,IniFile_,Misc_,Pict_;

{///$DEFINE MUSIC_PLAYER}

type
  TButtonState                 = (bsDisabled,bsEnabled,bsFocusedEnabled,bsFocusedDisabled); // order must not change
  TButtonStateColors           = array[TButtonState] of TColor;

const
  BUTTON_OFFS_X                = 2; // button bitmaps include a border to get the best results from the edge-smoothing function
  BUTTON_OFFS_Y                = 1;

  DEFAULT_BUTTON_FONT_COLOR    = clWhite;
  DEFAULT_BUTTON_TILE_HEIGHT   = 35;
  DEFAULT_BUTTON_TILE_NO       : array[TButtonState,0..1] of TInt8 =
    ((0,0),(0,1),(0,2),(0,0));
  DEFAULT_BUTTON_TILE_WIDTH    = 130;
  DEFAULT_GAME_MENU_FONT_SIZE  : array[Boolean] of Byte = (8,9); // for low-resolution and high-resolution screen respectively
  DEFAULT_TEXT_COLOR           : TButtonStateColors =            // must match the glyph images, i.e., 'Main.ArrowRightImage' etc.
                                 (clWhite,clWhite,clGreen,clWhite);
  DEFAULT_TEXT_SHADOW_COLOR    : TButtonStateColors =            // not necessarily identical to colors used by the built-in glyphs
                                 (clDkGray,DEFAULT_SHADOW_COLOR_ENABLED,DEFAULT_SHADOW_COLOR_FOCUSED,clDkGray);
  GLYPH_TEXT_SHADOW_COLOR      :  TButtonStateColors =           // must match the glyph images, i.e., 'Main.ArrowRightImage' etc.
                                 (clDkGray,clNavy,clWhite,clDkGray);
  HOTSPOT_COLLECTION_NAME_INDEX= -2; // the main window has some hot-spot areas that isn't real buttons
  HOTSPOT_STATUSBAR_HINT_INDEX = -3;
  HOTSPOT_STATUSBAR_REPLAY_SPEED_INDEX
                               = -4;
  MAX_MENU_ITEM_COUNT          = 20;

  PICTURE_TYPE_NAME               :array[TButtonState] of String =
    ('Disabled','Enabled','FocusedEnabled','FocusedDisabled'); // don't localize

type
  // this menu is specialized for 'Sokoban', it is not a general menu-class:
  TButtonType                  = (btOpen,btOpenPrior,btOpenClipboard,btOpenNext,
                                  btSave,btSaveAs,btRotate,btSolution,btUndo,btRedo,btSnapshot,
                                  btReset,btReplay,
                                  btReverseMode,btTools,btMenu,btSettings,btHelp,btExit,
                                  btMPlayer,btSize,btSlideShow,btStop,btPause,btTimer,btSplitView
                                  );

  TButtonBitMaps               = array[TButtonType] of Graphics.TBitMap;

  TGameMenuItem                = record
    Caption                    : String;
    Glyph                      : TBitMap;
    Pict                       : array[TButtonState] of TPict;
    Rect                       : TRect;
    SpeedButton                : TSpeedButton; // use info from this button
    State                      : TButtonState;
    TextOnly                   : Boolean; // i.e., no picture
    Visible                    : Boolean;
  end;

  TMenu0                       = class // this is not a general menu class, it's a menu specialized for Sokoban YASC
    Count                      : Integer;
    MenuItems                  : array[0..MAX_MENU_ITEM_COUNT-1] of TGameMenuItem;
  private
    ButtonGlyphFileName        : array[TButtonType]  of String;
    ButtonRightSlice           : Integer;
    fItemIndex                 : Integer;
    fVisible                   : Boolean;
    procedure ClearButtonImages;
    function  IsABuiltinGlyph(const BitMap:TBitMap):Boolean;
    procedure SetItemIndex(ItemIndex__:Integer);
    procedure SetTransparency(Transparency__,EdgeSmoothing__:Boolean; DisabledPct,EnabledPct,FocusedPct,FocusedDisabledPct,EdgePct:Integer);

  protected
    ButtonMirrorSlice          : Boolean;
    ButtonWidth                : Integer;
    Canvas                     : TCanvas;
    Image                      : TImage;
    procedure DrawCaption(const Canvas:TCanvas; Index:Integer; ButtonState:TButtonState; var R:TRect);
    procedure DrawGlyph(Glyph:TBitMap; State:TButtonState; X,Y:Integer; BitMap:TBitMap);
    procedure SetVisible(Visible__:Boolean); virtual;
  public
    BuiltinGlyph               : TButtonBitMaps;
    ButtonDX                   : Integer;
    ButtonDY                   : Integer;
    ButtonFontColor            : TButtonStateColors;
    ButtonGlyph                : TButtonBitMaps;
    ButtonHeight               : Integer;
    ButtonPicture              : array[TButtonState] of TPict;
    DefaultTileRect            : array[TButtonState] of TRect;
    EdgeSmoothing              : Boolean;
    EdgeTransparencyPct        : Integer;
    MenuPanel                  : TPanel;
    MenuWidth                  : Integer;
    SkipMouseOverSoundForItemIndex
                               : Integer;
    TextShadow                 : Boolean;
    TextShadowColor            : TButtonStateColors;
    TileRect                   : array[TButtonState] of TRect;
    Transparency               : Boolean;
    TransparencyPct            : array[TButtonState] of Integer;

    property ItemIndex:Integer read fItemIndex write SetItemIndex;
    property Visible:Boolean read fVisible write SetVisible;

    procedure Clear;
    procedure ClearButton(Index:Integer);
    function  Click: Integer; virtual;
    constructor Create(Destination__:TObject; MenuPanel__:TPanel; FontSize__:Integer);
    destructor Destroy; override;
    procedure Hide; virtual;
    function  Insert(const Caption__:String; Rect__:TRect;
                     SpeedButton__:TSpeedButton; Glyph__:TbitMap;
                     TextOnly__:Boolean):Integer;
    procedure Invalidate; virtual;
    function  ItemAtPos(X, Y: Integer): Integer;
    function  KeyDown(Key: Word; Shift: TShiftState): Integer;
    procedure HideButton(Index:Integer); virtual;
    function  LoadButtonPicture(Index:TButtonState):Boolean;
    function  LoadGlyph(Index:TButtonType):Boolean;
    function  LoadSettingsFromIniFile(const IniFile: TIniFile;
      const Section: String): Boolean; virtual;
    function  MakeButtonPicture(Index__:Integer; State__:TButtonState):Boolean; virtual;
    procedure MakeButtons; virtual;
    function  MouseMove(X, Y: Integer): Integer; virtual;
    function  Next: Integer;
    procedure Show; virtual;
    function  Prior: Integer;
    function  SaveSettingsToIniFile(const IniFile: TIniFile;
      const Section: String): Boolean; virtual;
    procedure ShowButton(Index:Integer; ButtonState:TButtonState; ShowHint:Boolean);
    procedure SetDefaultValues(BackgroundColor:TColor; FontSize:Integer); virtual;
  end;

  TGameMenu                                  = class(TMenu0)
  private
    fReverseMode:Boolean;
    ImageScaleFactor:Integer;
  protected
    procedure SetReverseMode(ReverseMode__:Boolean);
  public
    BtnBookmarksIndex          : Integer;
    BtnMenuIndex               : Integer;
    BtnMPlayerIndex            : Integer;
    BtnNextLevelIndex          : Integer;
    BtnPreviousLevelIndex      : Integer;
    BtnReverseModeIndex        : Integer;
    BtnRedoIndex               : Integer;
    BtnResetIndex              : Integer;
    BtnRedoAllIndex            : Integer;
    BtnRotateIndex             : Integer;
    BtnSaveIndex               : Integer;
    BtnSettingsIndex           : Integer;
    BtnSnapshotsIndex          : Integer;
    BtnSolutionIndex           : Integer;
    BtnSplitViewIndex          : Integer;
    BtnToolsIndex              : Integer;
    BtnUndoIndex               : Integer;
    MaxMenuHeight              : Integer;
    MinMenuHeight              : Integer;

    procedure Invalidate; override;
    function  MakeButtonPicture(Index__:Integer; State__:TButtonState):Boolean; override;
    procedure MakeButtons; override;

    property  ReverseMode:Boolean read fReverseMode write SetReverseMode;
  end;

  TStatusBarMenu                             = class(TMenu0)
  private
    PriorButtonLeftPos         : Integer;
  protected
  public
    BtnBrowseIndex             : Integer;
    BtnDeadlocksIndex          : Integer;
    BtnGenerateIndex           : Integer;
    BtnOptimizeIndex           : Integer;
    BtnReplayIndex             : Integer;
    BtnSolveIndex              : Integer;
    BtnStopIndex               : Integer;
    BtnTimerIndex              : Integer;

    procedure HideButton(Index:Integer); override;
    function  MakeButtonPicture(Index__:Integer; State__:TButtonState):Boolean; override;
    procedure MakeButtons; override;
  end;

  TMPlayerDisplayMenu                        = class(TMenu0)
  private
    MenuRect:TRect;
    TextSize:TSize;
  protected
    procedure EnableDisableButtons; virtual;
    function  GetMenuTexts(Index:Integer):String; virtual;
    function  GetMenuTextsCount:Integer; virtual;
    procedure SetVisible(Visible__:Boolean); override;
  public
    BitMap:TBitMap;
    DestRect:TRect;
    Pict:TPict;
    TransparencyPct:Integer;
    constructor Create(Destination__:TObject; MenuPanel__:TPanel; FontSize__:Integer);
    destructor Destroy; override;
    function  LoadSettingsFromIniFile(const IniFile: TIniFile;
      const Section: String): Boolean; override;
    procedure MakeButtons; override;
    function  MouseDown(Sender: TObject; Button: TMouseButton;
                Shift: TShiftState; X, Y: Integer):Boolean;
    function  MouseMove(X,Y: Integer):Integer; override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
                Shift: TShiftState; X, Y: Integer);
    function  SaveSettingsToIniFile(const IniFile: TIniFile;
      const Section: String): Boolean; override;
    procedure SetDefaultValues(BackgroundColor:TColor; FontSize:Integer); override;
    procedure Show; override;
    procedure Hide; override;
    procedure Write(const s:String; const R:TRect; Flags:Cardinal; FontColor:TColor);

    property  MenuTexts[Index:Integer]:String read GetMenuTexts;
    property  MenuTextsCount:Integer read GetMenuTextsCount;
  end;

  TActivitiesMenu                              = class(TMPlayerDisplayMenu)
  private
  protected
    function  GetMenuTexts(Index:Integer):String; override;
    function  GetMenuTextsCount:Integer; override;
  public
    procedure MakeButtons; override;
  end;

  TMandalaMenu                                = class(TMPlayerDisplayMenu)
  private
  protected
    function  GetMenuTexts(Index:Integer):String; override;
    function  GetMenuTextsCount:Integer; override;
  public
    procedure EnableDisableButtons; override;
    procedure MakeButtons; override;
  end;

  TViewer1Menu                                 = class(TMPlayerDisplayMenu)
  private
  protected
    procedure EnableDisableButtons; override;
    function  GetMenuTexts(Index:Integer):String; override;
    function  GetMenuTextsCount:Integer; override;
  public
    procedure MakeButtons; override;
  end;

  TFractalsMenu                                = class(TMPlayerDisplayMenu)
  private
  protected
    function  GetMenuTexts(Index:Integer):String; override;
    function  GetMenuTextsCount:Integer; override;
  public
    procedure EnableDisableButtons; override;
    procedure MakeButtons; override;
  end;

  TFireworksMenu                              = class(TMPlayerDisplayMenu)
  private
  protected
    function  GetMenuTexts(Index:Integer):String; override;
    function  GetMenuTextsCount:Integer; override;
  public
    procedure MakeButtons; override;
  end;


implementation

uses SysUtils,Forms,
     Pack_,SokUtil_,SokFile_,SokGame_,Text_,Main_,Game_,Sound_,Music_,BitMap_,GView_,
     MPlayer1_,Display_,Mandal1_,IView1_,Fractal_,Status_;

const
  BUTTON_TYPE_NAME                :array[TButtonType    ] of String =
    ('Open','OpenPrior','OpenClipboard','OpenNext','Save','SaveAs',
     'Rotate','Solution','Undo','Redo','Snapshot','New','Replay','Tools','Menu',
     'ReverseMode','Settings','Help',
     'Exit','MusicPlayer','Size','SlideShow','Stop','Pause','Timer','SplitView');

constructor TMenu0.Create(Destination__:TObject; MenuPanel__:TPanel; FontSize__:Integer);
var b:TButtonState;
begin
  Image:=nil; Canvas:=nil;
  if      Destination__ is TImage then begin
          Image         := TImage(Destination__);
          Canvas        := Image.Picture.BitMap.Canvas;
          end
  else if Destination__ is TBitMap then
          Canvas        := TBitMap(Destination__).Canvas
  else if Destination__ is TCanvas then
          Canvas        := TCanvas(Destination__)
  else if Destination__ =  nil then begin // accept 'nil' for the time being
          end
  else raise exception.Create('TMenu0.Create: Image or BitMap expected.');
  MenuPanel             :=MenuPanel__;

  FillChar(ButtonGlyph  ,SizeOf(ButtonGlyph  ),0);
  FillChar(ButtonGlyphFileName,SizeOf(ButtonGlyphFileName),0);
  FillChar(ButtonPicture,SizeOf(ButtonPicture),0); fVisible:=True;
  FillChar(TileRect,SizeOf(TileRect),0);
  for b:=Low(ButtonPicture) to High(ButtonPicture) do
      ButtonPicture[b]:=TPict.Create;
  fItemIndex:=-1; Count:=0; FillChar(MenuItems,SizeOf(MenuItems),0);
  SkipMouseOverSoundForItemIndex:=-1;

  FillChar(BuiltinGlyph,SizeOf(BuiltinGlyph),0);
  if MainForm<>nil then begin
     BuiltinGlyph[btOpenPrior    ]:=MainForm.OpenPriorImage    .Picture.BitMap;
     BuiltinGlyph[btOpenClipboard]:=MainForm.OpenClipboardImage.Picture.BitMap;
     BuiltinGlyph[btOpenNext     ]:=MainForm.OpenNextImage     .Picture.BitMap;
     BuiltinGlyph[btRotate       ]:=MainForm.RotateImage       .Picture.BitMap;
     BuiltinGlyph[btUndo         ]:=MainForm.ArrowLeftImage    .Picture.BitMap;
     BuiltinGlyph[btRedo         ]:=MainForm.ArrowRightImage   .Picture.BitMap;
     BuiltinGlyph[btSnapshot     ]:=MainForm.SnapshotImage     .Picture.BitMap;
     BuiltinGlyph[btReset        ]:=MainForm.ArrowStartImage   .Picture.BitMap;
     BuiltinGlyph[btReplay       ]:=MainForm.ArrowEndImage     .Picture.BitMap;
     BuiltinGlyph[btReverseMode  ]:=MainForm.ReverseModeImage  .Picture.BitMap;
     BuiltinGlyph[btMPlayer      ]:=MainForm.OpenMPlayerImage  .Picture.BitMap;
     BuiltinGlyph[btPause        ]:=MainForm.PauseImage        .Picture.BitMap;
     BuiltinGlyph[btTimer        ]:=MainForm.TimerImage        .Picture.BitMap;
     BuiltinGlyph[btSplitView    ]:=MainForm.SplitViewImage    .Picture.BitMap;
     end;

  SetDefaultValues(clBlack,FontSize__);
end;

destructor TMenu0.Destroy;
var b:TButtonState; g:TButtonType;
begin
  Clear;
  for b:=Low(ButtonPicture) to High(ButtonPicture) do ButtonPicture[b].Free;
  FillChar(ButtonPicture,SizeOf(ButtonPicture),0);

  for g:=Low(TButtonType) to High(TButtonType) do begin
      if (ButtonGlyph[g]<>nil) and (not IsABuiltinGlyph(ButtonGlyph[g])) then
         ButtonGlyph[g].Free;
      ButtonGlyph[g]:=nil;
      ButtonGlyphFileName[g]:='';
      end;
end;

procedure TMenu0.Clear;
begin
  ItemIndex:=-1; ClearButtonImages;
  Count:=0; FillChar(MenuItems,SizeOf(MenuItems),0);
end;

procedure TMenu0.ClearButtonImages;
var i,j:Integer; b:TButtonState;

  function  IsABasePicture(const Pict:TPict):Boolean;
  var b:TButtonState;
  begin
    for b:=Low(ButtonPicture) to High(ButtonPicture) do
        if Pict=ButtonPicture[b] then
           begin Result:=True; exit; end;
    Result:=False;
  end;

begin // ClearButtonImages
  for i:=0 to Pred(Count) do with MenuItems[i] do begin
      for b:=Low(Pict) to High(Pict) do
          if Pict[b]<>nil then begin
             if not IsABasePicture(Pict[b]) then begin
                for j:=Succ(i) to Pred(Count) do
                    if MenuItems[j].Pict[b]=Pict[b] then
                       MenuItems[j].Pict[b]:=nil;
                if not TextOnly then Pict[b].Free; // kludge: text-only buttons use 'Pict' as a flag for created/not created
                end;
             Pict[b]:=nil;
             end;
      end;
end;

procedure TMenu0.ClearButton(Index:Integer);
var b,b1,oState:TButtonState; BackBitMap:TBitMap;
begin
  with MenuItems[Index] do begin
    oState:=State; BackBitMap:=nil; b1:=State;
    for b:=Low(Pict) to High(Pict) do                  // find the picture that owns the background
        if Pict[b]<>nil then
           if   Pict[b].OrgBitMap<>nil then
                begin BackBitMap:=Pict[b].OrgBitMap; b1:=b; end
           else begin Pict[b].Free; Pict[b]:=nil; end; // delete all other pictures
    if BackBitMap<>nil then begin
       for b:=Low(Pict) to High(Pict) do
           if b<>b1 then begin                         // try to create one of the other pictures
              ShowButton(Index,b,False);
              if Pict[b]<>nil then begin
                 Pict[b].OrgBitMap.Free;
                 Pict[b].OrgBitMap:=BackBitMap;        // make the new picture the owner of the background
                 Pict[b1].OrgBitMap:=nil;
                 Pict[b1].Free; Pict[b1]:=nil;         // destroy the old owner
                 break;
                 end;
              end;
       ShowButton(Index,oState,False);                 // finally, show the button with the correct state
       end;
    end;
end;

function  TMenu0.IsABuiltinGlyph(const BitMap:TBitMap):Boolean;
var g:TButtonType;
begin
  for g:=Low(BuiltInGlyph) to High(BuiltinGlyph) do
      if BitMap=BuiltinGlyph[g] then
         begin Result:=True; exit; end;
  Result:=(BitMap=MainForm.MandelbrotImage .Picture.BitMap) or
          (BitMap=MainForm.NormalModeImage .Picture.BitMap) or
          (BitMap=MainForm.ReverseModeImage.Picture.BitMap);
end;

procedure TMenu0.SetItemIndex(ItemIndex__:Integer);
begin
  if ItemIndex__<>ItemIndex then begin
     ShowButton(ItemIndex,bsEnabled,False);
     fItemIndex:=ItemIndex__;
     ShowButton(ItemIndex,bsFocusedEnabled,True);
     if (ItemIndex>=0) and (MainForm.Sound.Enabled) and
        (MenuItems[ItemIndex].State=bsFocusedEnabled) and
        (not MainForm.RotateAndFlipPopupMenu.Visible) and // kludge: popup menus don't block normal menu updating
        (not MainForm.MultiViewPopupMenu.Visible) then begin
        if ItemIndex<>SkipMouseOverSoundForItemIndex then
           MainForm.Sound.Play(stMenuOver);
        end;
     SkipMouseOverSoundForItemIndex:=-1;
     end;
end;

procedure TMenu0.SetTransparency(Transparency__,EdgeSmoothing__:Boolean; DisabledPct,EnabledPct,FocusedPct,FocusedDisabledPct,EdgePct:Integer);
begin
  Transparency                       :=Transparency__;
  EdgeSmoothing                      :=EdgeSmoothing__;
  TransparencyPct[bsDisabled]        :=DisabledPct;
  TransparencyPct[bsEnabled]         :=EnabledPct;
  TransparencyPct[bsFocusedEnabled]  :=FocusedPct;
  TransparencyPct[bsFocusedDisabled] :=FocusedDisabledPct;
  EdgeTransparencyPct                :=EdgePct;
  ClearButtonImages;
end;

procedure TMenu0.SetVisible(Visible__:Boolean);
begin
  if Visible__<>Visible then begin
     fVisible:=Visible__;
     end;
end;

function  TMenu0.Insert(const Caption__:String; Rect__:TRect;
                        SpeedButton__:TSpeedButton; Glyph__:TBitMap;
                        TextOnly__:Boolean):Integer;
var i:Integer;
begin
  Result:=-1;
  if Count<MAX_MENU_ITEM_COUNT then with Rect__ do begin
     Result:=0;
     if (Right*Bottom=0) and (Count>0) then begin
        Left  :=Left+MenuItems[Pred(Count)].Rect.Left;
        Top   :=Top +MenuItems[Pred(Count)].Rect.Top+RectHeight(MenuItems[Pred(Count)].Rect);
        Right :=Left+RectWidth (MenuItems[Pred(Count)].Rect);
        Bottom:=Top +RectHeight(MenuItems[Pred(Count)].Rect);
        end;
     for i:=Pred(Count) downto 0 do
         if (Rect__.Top>MenuItems[i].Rect.Top)
            or
            ((Rect__.Top=MenuItems[i].Rect.Top) and
             (Rect__.Left>MenuItems[i].Rect.Left)) then begin
            Result:=Succ(i); break;
            end;
     Inc(Count);
     for i:=Pred(Count) downto Succ(Result) do MenuItems[i]:=MenuItems[Pred(i)];
     with MenuItems[Result] do begin
       FillChar(Pict,SizeOf(Pict),0); Visible:=True; State:=bsDisabled;
       Caption:=Caption__; Rect:=Rect__;
       SpeedButton:=SpeedButton__; Glyph:=Glyph__;
       TextOnly:=TextOnly__;
       end;
     end;
end;

function  TMenu0.ItemAtPos(X,Y:Integer):Integer;
var a,b,c:Integer; BinarySearch:Boolean; p:TPoint;
begin
  if (ItemIndex>=0) and
     PtInRect(MenuItems[ItemIndex].Rect,Point(X,Y)) and
     MenuItems[ItemIndex].Visible then
     Result:=ItemIndex
  else begin
     Result:=-1; a:=0; b:=Pred(Count); BinarySearch:=True;
     while a<=b do begin
       if BinarySearch then c:=(a+b) div 2
       else c:=a;
       with MenuItems[c] do
         if      Y< Rect.Top    then b:=Pred(c)
         else if Y>=Rect.Bottom then a:=Succ(c)
//       else if X< Rect.Left   then b:=Pred(c)
//       else if X>=Rect.Right  then a:=Succ(c)
//       else begin Result:=c; exit; end;
         else if (X>=Rect.Left) and (X<Rect.Right) and Visible then begin
                 Result:=c; exit;
                 end
         else if BinarySearch then BinarySearch:=False
              else Inc(a);
       end;
     if (Self=MainForm.Menu) and
        ((a=0) or (a>=Pred(Count))) and
        (MainForm.Status<>nil) then begin // the statusbar panels 'Moves', 'Pushes', and 'Replay Speed' are recognized as specials "buttons"
        if a=0 then with MenuItems[0].Rect do begin
           if (X<Right) and (Y<Top) then Result:=HOTSPOT_COLLECTION_NAME_INDEX
           end
        else begin
           p:=Point(X,Y);
           if PtInRect(MainForm.Status.RectForm,p) then
              if      PtInRect(MainForm.Status.Panels[spMoves      ].RectForm,p) or
                      PtInRect(MainForm.Status.Panels[spPushes     ].RectForm,p) then
                      Result:=HOTSPOT_STATUSBAR_HINT_INDEX
              else if PtInRect(MainForm.Status.Panels[spReplaySpeed].RectForm,p) then
                      Result:=HOTSPOT_STATUSBAR_REPLAY_SPEED_INDEX;
           end;
        end;
     end;
end;

function  TMenu0.Click:Integer;
var oSkipMouseOverSoundForItemIndex:Integer;
begin
  Result:=-1;
  if ItemIndex>=0 then with MenuItems[ItemIndex].SpeedButton do
        if Enabled and (@OnClick<>nil) then begin
           Result:=ItemIndex;
           if SkipMouseOverSoundForItemIndex=-1 then
              SkipMouseOverSoundForItemIndex:=Result;
           oSkipMouseOverSoundForItemIndex:=SkipMouseOverSoundForItemIndex;
           ItemIndex:=-1;
           SkipMouseOverSoundForItemIndex:=oSkipMouseOverSoundForItemIndex; // kludge: don't let the mouse-down event destroy the skip-number
           if MainForm.Sound.Enabled then with MainForm.Sound do begin
              StopAll;
              Play(stMenuSelect);
              end;
           MainForm.MPlayer.ResetFps;
           Click;
           end;
end;

function  TMenu0.KeyDown(Key: Word; Shift: TShiftState):Integer;
begin
  Result:=-1;
  if      Key=VK_RETURN then Result:=Click
  else if (Key=VK_LEFT) or (Key=VK_UP) or
          ((Key=VK_TAB) and (ssShift in Shift)) then
          Result:=Prior
  else if (Key=VK_NEXT ) or (Key=VK_DOWN) or
          ((Key=VK_TAB) and (not (ssShift in Shift))) then
          Result:=Next;
end;

function  TMenu0.Prior:Integer;
begin
  Result:=ItemIndex;
  if Count<>0 then
     repeat if   Result>0 then Dec(Result)
            else Result:=Pred(Count);
     until  MenuItems[Result].SpeedButton.Enabled or
            (Result=ItemIndex);
end;

function  TMenu0.Next:Integer;
begin
  Result:=ItemIndex;
  if Count<>0 then
     repeat if   (Result<0) or
                 (Result>Pred(Count)) then Result:=0
            else Inc(Result);
     until  MenuItems[Result].SpeedButton.Enabled or
            (Result=ItemIndex);
end;

function  TMenu0.MouseMove(X,Y:Integer):Integer;
begin
  Result:=ItemAtPos(X,Y);
  ItemIndex:=Result;
end;

procedure TMenu0.HideButton(Index:Integer);
var b:TButtonState;
begin
  if (Index>=0) and (Index<Count) then with MenuItems[Index] do
     if Visible then begin
        Visible:=False;
        for b:=Low(Pict) to High(Pict) do
            if (Pict[b]<>nil) and (Pict[b].OrgBitMap<>nil) then begin
               Self.Canvas.CopyRect(Rect,Pict[b].OrgBitMap.Canvas,Classes.Rect(0,0,Pict[b].OrgBitMap.Width,Pict[b].OrgBitMap.Height));
               break;
               end;
        end;
end;

procedure TMenu0.DrawCaption(const Canvas:TCanvas; Index:Integer; ButtonState:TButtonState; var R:TRect);
var H:Integer;
begin
  if Canvas<>nil then with MenuItems[Index] do begin
     H:=RectHeight(R);
     if Odd(H) then Inc(R.Bottom);  // show text below the middle

     Windows.SetBkMode (Canvas.Handle,Windows.TRANSPARENT);

     if TextShadow then begin
        if      ButtonState=bsDisabled then Canvas.Font.Color:=TextShadowColor[bsDisabled]
        else if ButtonState=bsEnabled  then Canvas.Font.Color:=TextShadowColor[bsEnabled ]
        else if SpeedButton.Enabled    then Canvas.Font.Color:=TextShadowColor[bsFocusedEnabled]
        else                                Canvas.Font.Color:=TextShadowColor[bsFocusedDisabled];
        Inc(R.Left); Inc(R.Top); Inc(R.Right); Inc(R.Bottom);
        Windows.DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R,DT_CENTER or DT_VCENTER or DT_SINGLELINE,nil);
        Dec(R.Left); Dec(R.Top); Dec(R.Right); Dec(R.Bottom);
        end;

     if      ButtonState=bsDisabled then Canvas.Font.Color:=ButtonFontColor[bsDisabled]
     else if ButtonState=bsEnabled  then Canvas.Font.Color:=ButtonFontColor[bsEnabled ]
     else if SpeedButton.Enabled    then Canvas.Font.Color:=ButtonFontColor[bsFocusedEnabled]
     else                                Canvas.Font.Color:=ButtonFontColor[bsFocusedDisabled];

     Windows.DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R,DT_CENTER or DT_VCENTER or DT_SINGLELINE,nil);
     Windows.SetBkMode (Canvas.Handle,Windows.OPAQUE);

     if Odd(H) then Dec(R.Bottom);
     end;
end;

procedure TMenu0.ShowButton(Index:Integer; ButtonState:TButtonState; ShowHint:Boolean);
var b:TButtonState; SecondaryScoreMetrics:TSecondaryScoreMetrics;
begin
  if Visible then
     if (Index>=0) and (Index<Count) then with MenuItems[Index] do begin

        if Visible then begin
           if SpeedButton=nil then
              if ShowHint then MainForm.Status.Hint:=Caption
              else
           else begin

              if (ButtonState=bsEnabled) and
                 (not SpeedButton.Enabled)         then ButtonState:=bsDisabled;

              if ButtonState=bsEnabled then
                 if   SpeedButton.Enabled          then
                      if   (SpeedButton=MainForm.BtnStatusMenuTimer) and
                           (MainForm.Game<>nil) and (MainForm.Game.StartTimeMS<>0) then
                           ButtonState:=bsFocusedDisabled // kludge: the state 'focused disabled' is (mis-)used to represent 'timer active'
                      else
                 else ButtonState:=bsDisabled;

              if      SpeedButton.Enabled          then b:=ButtonState
              else if ButtonState=bsFocusedEnabled then b:=bsFocusedDisabled
                   else                                 b:=bsDisabled;

              if (Pict[b]<>nil) and ((State=b) and (not (Self is TStatusBarMenu))) then
                 if ShowHint then MainForm.Status.Hint:=GetLongHint(SpeedButton.Hint)
                 else //MainForm.Status.Hint:=''
              else begin
                 if (Pict[b]=nil) and (not TextOnly) then // images are first created on demand
                    MakeButtonPicture(Index,b);

                 if TextOnly
                    or
                    ((Pict[b]<>nil)
                     and
                     ((Index=ItemIndex) or (b<bsFocusedEnabled) or (Self is TStatusBarMenu))) then begin

                    State:=b;

                    if   TextOnly then //
                    else Pict[b].Draw(Rect.Left,Rect.Top,Self.Canvas);

                    if ((Glyph=nil) or (Glyph.PixelFormat<>pf24Bit)) and
                       ((not Transparency) or TextOnly) then begin // otherwise the picture already contains glyph or caption
                       if (Image<>nil) and
                          (Image.Tag<>LongInt(Addr(MenuPanel))) then begin
                          Image.Tag :=LongInt(Addr(MenuPanel));
                          Self.Canvas.Font.Assign(MenuPanel.Font);
                          end;
                       DrawCaption(Self.Canvas,Index,b,Rect);
                       end;

                    if   ShowHint then MainForm.Status.Hint:=GetLongHint(SpeedButton.Hint)
                    else if Self is TStatusBarMenu then MainForm.Status.Hint:='';
                    end;
                 end;
              end;
           end;
        end
     else if   ShowHint then
               if   (Self=MainForm.Menu) and
                    (MainForm.Game<>nil) and
                    (not MainForm.Game.IsReplaying) then begin
                    if   Index=HOTSPOT_COLLECTION_NAME_INDEX then with MainForm do begin
                         if Assigned(Game) and
                            (Game.GameState<>gsNull) and
                            (Game.FileName<>'') and
                            (Game.DisplayName<>'') and
                            IsAnIniFileSectionFileName(MainForm.Game.FileName) and
                            (Status<>nil) then
                            Status.Hint:=Format(CurrentLevelCollectionText__,[ExtractFileNameWithoutPathAndExtension(ExtractIniFileName(Game.FileName))]);
                         end
                    else if (Index=HOTSPOT_STATUSBAR_HINT_INDEX) and (MainForm.Game.GameState<>gsNull) then begin
                            //MainForm.Status.Hint:=StatusBarHintText[MainForm.Game.ReverseMode];
                            MainForm.Game.CalculateSecondaryScoreMetrics(SecondaryScoreMetrics);
                            with SecondaryScoreMetrics do
                              MainForm.Status.Hint:=Format(StatusBarHintText__,[BoxLines,BoxChanges,PushingSessions,PlayerLines]); // + ' ('+IntToStr(MainForm.Game.History.PlayerLinesCount)+')';
                            end
                    else MainForm.Status.Hint:='';
                    end
               else MainForm.Status.Hint:=''
          else      MainForm.Status.Hint:='';
end;

procedure TMenu0.Show;
var i:Integer;
begin
  for i:=0 to Pred(Count) do
      if i=ItemIndex then ShowButton(i,bsFocusedEnabled,False)
      else ShowButton(i,bsEnabled,False);
end;

procedure TMenu0.Hide;
begin
  Error('TMenu0.Hide: Not implemented',Application.Title);
end;

function  TMenu0.LoadSettingsFromIniFile(const IniFile:TIniFile; const Section:String):Boolean;
var b:TButtonState; g:TButtonType;
begin
//Result:=True; exit;
//fVisible              :=IniFile.ReadBool   (Section,'Visible',Visible);
  Transparency          :=IniFile.ReadBool(Section,'Transparency',Transparency);
  EdgeSmoothing         :=IniFile.ReadBool(Section,'EdgeSmoothing',EdgeSmoothing);
  EdgeTransparencyPct   :=Max(0,Min(100,IniFile.ReadInteger(Section,'EdgeTransparencyPct',EdgeTransparencyPct)));
  TextShadow            :=IniFile.ReadBool(Section,'TextShadow',TextShadow);
  for b:=Low(ButtonPicture) to High(ButtonPicture) do with ButtonPicture[b] do begin
      FileName          :=              KeepDataPathUpToDate(IniFile.ReadString (Section,PICTURE_TYPE_NAME[b]+'.FileName',FileName));
      TileRect[b].Left  :=              IniFile.ReadInteger(Section,PICTURE_TYPE_NAME[b]+'.Left'  ,TileRect[b].Left);
      TileRect[b].Top   :=              IniFile.ReadInteger(Section,PICTURE_TYPE_NAME[b]+'.Top'   ,TileRect[b].Top);
      TileRect[b].Right :=              Max(
                                        IniFile.ReadInteger(Section,PICTURE_TYPE_NAME[b]+'.Right' ,TileRect[b].Right),
                                        TileRect[b].Left);
      TileRect[b].Bottom:=              Max(
                                        IniFile.ReadInteger(Section,PICTURE_TYPE_NAME[b]+'.Bottom',TileRect[b].Bottom),
                                        TileRect[b].Top);
      MaskBitMapColor   :=ColorToRGB   (IniFile.ReadInteger(Section,PICTURE_TYPE_NAME[b]+'.BackgroundColor',Integer(RGBToColor(MaskBitMapColor))));
      MaskBitMapPct     :=              IniFile.ReadInteger(Section,PICTURE_TYPE_NAME[b]+'.BackgroundColorTolerance',MaskBitMapPct);
      Masked            :=              IniFile.ReadBool   (Section,PICTURE_TYPE_NAME[b]+'.Masked',Masked);
      ButtonFontColor[b]:=TColor(       IniFile.ReadInteger(Section,PICTURE_TYPE_NAME[b]+'.FontColor',Integer(ButtonFontColor[b])));
      TextShadowColor[b]:=TColor(       IniFile.ReadInteger(Section,PICTURE_TYPE_NAME[b]+'.TextShadowColor',Integer(TextShadowColor[b])));
      TransparencyPct[b]:=Max(0,Min(100,IniFile.ReadInteger(Section,PICTURE_TYPE_NAME[b]+'.TransparencyPct',TransparencyPct[b])));
      end;

  ButtonDX              :=              IniFile.ReadInteger(Section,'ButtonDX',ButtonDX);
  ButtonDY              :=              IniFile.ReadInteger(Section,'ButtonDY',ButtonDY);
  ButtonMirrorSlice     :=              IniFile.ReadBool   (Section,'ButtonMirrorSlice',ButtonMirrorSlice);
  ButtonRightSlice      :=              IniFile.ReadInteger(Section,'ButtonRightSlice',ButtonRightSlice);

  for g:=Low(TButtonType) to High(TButtonType) do
      ButtonGlyphFileName[g]:=IniFile.ReadString(Section,'Button.'+BUTTON_TYPE_NAME[g]+'.Glyph.FileName',ButtonGlyphFileName[g]);

  Result:=LoadFontFromIniFile(IniFile,Section,'',MenuPanel.Font);
//  SetTransparency(True,True,30,30,20,20,25,False);
//SetTransparency(False,False,30,30,20,20,25);
end;

function  TMenu0.SaveSettingsToIniFile(const IniFile:TIniFile; const Section:String):Boolean;
var b:TButtonState; g:TButtonType;
begin
//Result:=True; exit;
//IniFile.WriteBool   (Section,'Visible',Visible);
  IniFile.WriteBool   (Section,'Transparency',Transparency);
  IniFile.WriteBool   (Section,'EdgeSmoothing',EdgeSmoothing);
  IniFile.WriteInteger(Section,'EdgeTransparencyPct',EdgeTransparencyPct);
  IniFile.WriteBool   (Section,'TextShadow',TextShadow);
  for b:=Low(ButtonPicture) to High(ButtonPicture) do with ButtonPicture[b] do begin
      IniFile.WriteString (Section,PICTURE_TYPE_NAME[b]+'.FileName',FileName);
      IniFile.WriteInteger(Section,PICTURE_TYPE_NAME[b]+'.Left'  ,TileRect[b].Left);
      IniFile.WriteInteger(Section,PICTURE_TYPE_NAME[b]+'.Top'   ,TileRect[b].Top);
      IniFile.WriteInteger(Section,PICTURE_TYPE_NAME[b]+'.Right' ,TileRect[b].Right);
      IniFile.WriteInteger(Section,PICTURE_TYPE_NAME[b]+'.Bottom',TileRect[b].Bottom);
      IniFile.WriteInteger(Section,PICTURE_TYPE_NAME[b]+'.BackgroundColor',Integer(RGBToColor(MaskBitMapColor)));
      IniFile.WriteInteger(Section,PICTURE_TYPE_NAME[b]+'.BackgroundColorTolerance',MaskBitMapPct);
      IniFile.WriteBool   (Section,PICTURE_TYPE_NAME[b]+'.Masked',Masked);
      IniFile.WriteInteger(Section,PICTURE_TYPE_NAME[b]+'.FontColor',ButtonFontColor[b]);
      IniFile.WriteInteger(Section,PICTURE_TYPE_NAME[b]+'.TextShadowColor',Integer(TextShadowColor[b]));
      IniFile.WriteInteger(Section,PICTURE_TYPE_NAME[b]+'.TransparencyPct',TransparencyPct[b]);
      end;

  IniFile.WriteInteger(Section,'ButtonDX',ButtonDX);
  IniFile.WriteInteger(Section,'ButtonDY',ButtonDY);
  IniFile.WriteBool   (Section,'ButtonMirrorSlice',ButtonMirrorSlice);
  IniFile.WriteInteger(Section,'ButtonRightSlice',ButtonRightSlice);

  for g:=Low(TButtonType) to High(TButtonType) do
      IniFile.WriteString(Section,'Button.'+BUTTON_TYPE_NAME[g]+'.Glyph.FileName',ButtonGlyphFileName[g]);

  Result:=SaveFontToIniFile(IniFile,Section,'',MenuPanel.Font);
end;

procedure TMenu0.SetDefaultValues(BackgroundColor:TColor; FontSize:Integer);
var b:TButtonState; g:TButtonType;
begin
  Clear;

  ButtonFontColor                      :=DEFAULT_TEXT_COLOR;

  if MenuPanel<>nil then with MenuPanel.Font do begin
     Name :='Arial';
     Size :=FontSize;
     Color:=DEFAULT_TEXT_COLOR[bsEnabled];
     Style:=[fsBold];
     ButtonFontColor[bsEnabled]         :=MenuPanel.Font.Color;
     MenuPanel.Color                    :=BackgroundColor;
     end;

  TextShadow                            :=True;
  TextShadowColor                       :=DEFAULT_TEXT_SHADOW_COLOR;

  for b:=Low(ButtonPicture) to High(ButtonPicture) do with ButtonPicture[b] do begin
      FileName:=DEFAULT_VALUE;
      TileRect[b]:=GridCellToRect(
                      DEFAULT_BUTTON_TILE_NO[b,0],
                      DEFAULT_BUTTON_TILE_NO[b,1],
                      DEFAULT_BUTTON_TILE_WIDTH,
                      DEFAULT_BUTTON_TILE_HEIGHT,
                      DEFAULT_TILE_GRIDLINE_WIDTH);
      DefaultTileRect[b]:=TileRect[b];
      Masked:=True; MaskBitMapColor:=RGB_BLACK; MaskBitMapPct:=0;
      LoadButtonPicture(b);
      end;

  ButtonDX:=2; ButtonDY:=8;
  ButtonMirrorSlice:=True;
  ButtonRightSlice:=7;

  ButtonGlyph:=BuiltinGlyph;
  for g:=Low(TButtonType) to High(TButtonType) do
      if ButtonGlyph[g]<>nil then ButtonGlyphFileName[g]:=DEFAULT_VALUE;

  SetTransparency(True,True,30,30,20,20,25);
end;

function  TMenu0.LoadButtonPicture(Index:TButtonState):Boolean;
var s:String; SourceBitMap:TBitMap;
begin
  Result:=False; SourceBitMap:=nil;
  if Self=MainForm.Menu then with ButtonPicture[Index] do begin
    if FileName=DEFAULT_VALUE then begin
       if MainForm<>nil then SourceBitMap:=MainForm.DefaultButtonSetImage.Picture.BitMap;
       end
    else begin
       if   ExtractFilePath(FileName)<>'' then
            s:='' // the filename contains a path
       else s:=StrWithTrailingPathDelimiter(ExtractFilePath(Application.ExeName)); // use the application directory as default directory
       if (FileName<>'') and
          LoadFromFile(s+FileName) and
          ((Pict=nil) or               // simple bitmap
           MakeOrgBitMapFromPict) then // image is able to draw itself
          SourceBitMap:=OrgBitMap;
       end;
    Result:=(SourceBitMap<>nil) and LoadFromBitMapRect(SourceBitMap,TileRect[Index],Point(BUTTON_OFFS_X,BUTTON_OFFS_Y),RGBToColor(MaskBitMapColor));
    if not Result then MakeBlank(RectWidth(TileRect[Index]),RectHeight(TileRect[Index]),clBlue);
    OrgBitMap.Free; OrgBitMap:=nil; // 'OrgBitMap' isn't used, hence, clear it now
    if Masked then MakeMaskBitMap(MaskBitMapColor,MaskBitMapPct,MaskExteriorOnly);
    end;
end;

function  TMenu0.LoadGlyph(Index:TButtonType):Boolean;
var s:String; Glyph:TBitMap; Pict:TPict;
begin
  Result:=False; Glyph:=ButtonGlyph[Index];
  if     (Glyph<>nil) and (not IsABuiltInGlyph(Glyph)) then begin
         Glyph.Free; ButtonGlyph[Index]:=nil;
         end;
  if     ButtonGlyphFileName[Index]<>'' then
    if   ButtonGlyphFileName[Index]=DEFAULT_VALUE then begin
         ButtonGlyph[Index]:=BuiltinGlyph[Index]; Result:=True;
         end
    else try    Pict:=TPict.Create;
                try     if   ExtractFilePath(ButtonGlyphFileName[Index])<>'' then
                             s:='' // the glyph filename contains a path
                        else s:=StrWithTrailingPathDelimiter(ExtractFilePath(Application.ExeName)); // use the application directory as default directory
                        if   (ButtonGlyphFileName[Index]<>'') and
                             Pict.LoadFromFile(s+ButtonGlyphFileName[Index]) then begin
                             ButtonGlyph[Index]:=Pict.GiveBitMapAway; Result:=True;
                             end;
                finally Pict.Free;
                end;
         except on E:Exception do begin
                Error(E.Message,Application.Title); Result:=False;
                end;
         end;
  if not Result then begin
     ButtonGlyph[Index]:=BuiltinGlyph[Index];
     if   ButtonGlyph[Index]=nil then ButtonGlyphFileName[Index]:=''
     else ButtonGlyphFileName[Index]:=DEFAULT_VALUE;
     end;
end;

procedure TMenu0.DrawGlyph(Glyph:TBitMap; State:TButtonState; X,Y:Integer; BitMap:TBitMap);
var GlyphIndex,GlyphHeight,GlyphWidth:Integer;
    GlyphTransparentColor,OldColor1,NewColor1,OldColor2,NewColor2:TColor;
begin
  if (Glyph<>nil) and (BitMap<>nil) and
     (Glyph.PixelFormat=pf24bit) then begin
     GlyphIndex:=Ord(State);
     GlyphWidth :=Glyph.Width div 4;
     GlyphHeight:=Glyph.Height;
     GlyphTransparentColor:=RGBToColor(PRGBVector(Glyph.ScanLine[Pred(GlyphHeight)])[0]);

     if IsABuiltInGlyph(Glyph) then begin
        OldColor1:=DEFAULT_TEXT_COLOR[State];
        NewColor1:=ButtonFontColor[State];
        OldColor2:=GLYPH_TEXT_SHADOW_COLOR[State];
        if   TextShadow then
             NewColor2:=TextShadowColor[State]
        else NewColor2:=GlyphTransparentColor;
        end
     else begin
        OldColor1:=DEFAULT_TEXT_COLOR[State];
        NewColor1:=ButtonFontColor[State];
        OldColor2:=GLYPH_TEXT_SHADOW_COLOR[State];
        if   TextShadow then
             NewColor2:=TextShadowColor[State]
        else NewColor2:=GlyphTransparentColor;

        //OldColor1:=clBlack; NewColor1:=clBlack;
        //OldColor2:=clBlack; NewColor2:=clBlack;
        end;

     if X<0 then X:=(BitMap.Width-GlyphWidth) div 2;
     if Y<0 then Y:=(BitMap.Height+Ord(Odd(BitMap.Height))-(GlyphHeight-2)) div 2; // '-2': each glyph has 2 extra rows for a shadow, but this should not affect the position

     BitBltTransparent(BitMap,
                       X,
                       Y,
                       Glyph,
                       Classes.Rect(GlyphIndex*GlyphWidth,0,Succ(GlyphIndex)*GlyphWidth,GlyphHeight),
                       GlyphTransparentColor,0,
                       OldColor1,NewColor1,
                       OldColor2,NewColor2);
     end;
end;

function  TMenu0.MakeButtonPicture(Index__:Integer; State__:TButtonState):Boolean;
begin
  Result:=False;
end;

procedure TMenu0.MakeButtons;
var b:TButtonState; g:TButtonType;
begin
  Clear;
  for b:=Low(ButtonPicture) to High(ButtonPicture) do LoadButtonPicture(b);
  for g:=Low(TButtonType)   to High(TButtonType)   do LoadGlyph(g);
end;

procedure TMenu0.Invalidate;
begin
  ClearButtonImages;
  if (Image<>nil) and (Canvas<>Image.Picture.BitMap.Canvas) then
     Canvas:=Image.Picture.BitMap.Canvas;
  Show;
end;

procedure TGameMenu.Invalidate;
begin
  Inherited;
  if MainForm.Game<>nil then begin
     fReverseMode:=not MainForm.Game.ReverseMode; // so following assignment triggers a new state
     ReverseMode:=MainForm.Game.ReverseMode;
     end;
end;

function  TGameMenu.MakeButtonPicture(Index__:Integer; State__:TButtonState):Boolean;
var i,j,k,H,W,W1,GlyphIndex,GlyphHeight,GlyphWidth:Integer;
    GlyphTransparentColor,OldColor1,NewColor1,OldColor2,NewColor2:TColor;
    b1:TButtonState; p:PRGBVector; R:TRect; BackBitMap:TBitMap;
begin
  Result:=False;
  if (Index__>=0) and (Index__<Count) then with MenuItems[Index__] do begin
     Pict[State__].Free; Pict[State__]:=nil;

     W:=RectWidth(Rect); H:=RectHeight(Rect);
     if (not Transparency) and (BUTTON_OFFS_X+BUTTON_OFFS_Y=0) then
        if   (W=ButtonPicture[State__].BitMap.Width) and
             (H=ButtonPicture[State__].BitMap.Height) then
             Pict[State__]:=ButtonPicture[State__]
        else if Glyph=nil then
                for i:=0 to Pred(Count) do with MenuItems[i] do
                    if (Pict[State__]<>nil) and (Glyph=nil) and
                       (W=RectWidth(Rect)) and (H=RectHeight(Rect)) then begin
                       MenuItems[Index__].Pict[State__]:=Pict[State__]; break;
                       end;

     if Pict[State__]=nil then
        try    Pict[State__]:=TPict.Create;
               BackBitMap:=nil;
               with Pict[State__] do begin
                 if Transparency or EdgeSmoothing then with MenuItems[Index__] do begin
                    for b1:=Low(Pict) to High(Pict) do
                        if (b1<>State__) and (Pict[b1]<>nil) and (Pict[b1].OrgBitMap<>nil) then
                           BackBitMap:=Pict[b1].OrgBitMap;
                    if BackBitMap<>nil then begin      // borrow saved background from another button-picture
                       OrgBitMap.Free; OrgBitMap:=nil; // 'OrgBitMap' isn't used, hence, free it now
                       end
                    else
                       if Pict[State__].MakeOrgBitMapFromPict then begin
                          if Image<>nil then with Image.Picture.BitMap do begin
                             if Width <>Image.ClientWidth then
                                Width :=Image.ClientWidth;
                             if Height<>Image.ClientHeight then
                                Height:=Image.ClientHeight;
                             end;

                          OrgBitMap.Width :=W; // 'OrgBitMap' is used to save the background
                          OrgBitMap.Height:=H;
                          OrgBitMap.Canvas.CopyRect(Classes.Rect(0,0,W,H),Self.Canvas,Rect);
                          BackBitMap:=OrgBitMap;
                          end;
                    end
                 else begin
                    OrgBitMap.Free; OrgBitMap:=nil; // 'OrgBitMap' isn't used, hence, free it now
                    end;

                 BitMap.Width :=W;
                 BitMap.Height:=H;

                 if      W=ButtonPicture[State__].BitMap.Width then W1:=W
                 else if ButtonMirrorSlice                     then W1:=W div 2
                 else if ButtonRightSlice<0                    then W1:=W div Abs(ButtonRightSlice)
                 else                                               W1:=W-ButtonRightSlice-BUTTON_OFFS_X;

                 if   ImageScaleFactor<=1 then
                      begin i:=0; j:=0; end
                 else begin i:=2; j:=1; end;                                    // kludge: buttons are enlarged with a border at load-time to get the best results from the simple edge-smoothing function, hence, compensate here for the extra border created by scaling
                 BitMap.Canvas.CopyRect(Classes.Rect(0,0,W1,H),
                                        ButtonPicture[State__].BitMap.Canvas,
                                        Classes.Rect(i,j,W1,H));

                 if W1<W then
                    if ButtonMirrorSlice then with BitMap do begin
                       for i:=0 to Pred(Height) do begin
                           p:=ScanLine[i];
                           if Odd(W) then begin
                              p[W1]:=p[Pred(W1)]; k:=1;
                              end
                           else k:=0;
                           for j:=0 to Pred(W1) do p[W1+j+k]:=p[W1-j-1];
                           end;
                       end
                    else BitMap.Canvas.CopyRect(Classes.Rect(W1,0,W,H),
                                                ButtonPicture[State__].BitMap.Canvas,
                                                Classes.Rect(ButtonPicture[State__].BitMap.Width-(W-W1),0,ButtonPicture[State__].BitMap.Width,H));

                 Masked:=ButtonPicture[State__].Masked or (BUTTON_OFFS_X+BUTTON_OFFS_Y<>0);

                 if Masked then
                    if RGBToColor(ButtonPicture[State__].MaskBitMapColor)=RGBToColor(RGB_BLACK) then
                       MakeMaskBitMap(ButtonPicture[State__].MaskBitMapColor,ButtonPicture[State__].MaskBitMapPct,ButtonPicture[State__].MaskExteriorOnly)
                    else // non-black mask-color: the bitmap's mask-color was changed to black on load-time
                       MakeMaskBitMap(RGB_BLACK,0,ButtonPicture[State__].MaskExteriorOnly);

                 R:=Classes.Rect(0,0,W,H);

                 if   Glyph<>nil then
                      DrawGlyph(Glyph,State__,-1,-1,BitMap)
                 else if Transparency then begin
                         BitMap.Canvas.Font.Assign(MenuPanel.Font);
                         DrawCaption(BitMap.Canvas,Index__,State__,R);
                         end;

                 if (BackBitMap<>nil) then begin
                    if   Transparency then
                         AlphaBlend(BitMap,BackBitMap,MaskBitMap,R,R,TransparencyPct[State__],EdgeTransparencyPct,False)
                    else AlphaBlend(BitMap,BackBitMap,MaskBitMap,R,R,0,0,False);
                    if Self.EdgeSmoothing then
                       BitMap_.EdgeSmoothing(BitMap,BackBitMap,MaskBitMap,Classes.Rect(0,0,W,H),True,False);
                       Masked:=False; // quick and dirty: since 'AlphaBlend' must examine each pixel anyway, it fills in masked pixels
                       MaskBitMap.Free; MaskBitMap:=nil;
                    end;
                 end;
        except on E:Exception do begin
                  Pict[State__].Free; Pict[State__]:=nil;
                  Result:=Error(E.Message,Application.Title);
                  end;
        end;
     end;
end;

procedure TGameMenu.MakeButtons;
const MIN_ROW_COUNT=5; MAX_ROW_COUNT=10;
var i,j,DX,DY,ExtraWidth,MenuHeight,MenuRowCount:Integer;
    {$IFDEF MUSIC_PLAYER}
      R,R1:TRect;
    {$ENDIF}

  function  LastLeft:Integer;
  begin
    if Count=0 then Result:=0
    else Result:=MenuItems[Pred(Count)].Rect.Left;
  end;

  function  LastTop:Integer;
  begin
    if Count=0 then Result:=0
    else Result:=MenuItems[Pred(Count)].Rect.Top;
  end;

  function ScaleButtonPictures:Integer;
  var b:TButtonState; oAntiAliasing:TAntiAliasing;
  begin // not in production; scaling the button images isn't fully implemented, and it doesn't look good with the currently implemented scaling functions
    try
      if Assigned(MainForm) and
         (MainForm.ClientWidth >=ENLARGE_BUTTONS_AND_STATUS_BAR_IMAGES_WINDOW_WIDTH_THRESHOLD) and
         (MainForm.ClientHeight>=ENLARGE_BUTTONS_AND_STATUS_BAR_IMAGES_WINDOW_HEIGHT_THRESHOLD) then begin
         Result:=2; // actually, the scale factor isn't 2 but 3/2
         for b:=Low(ButtonPicture) to High(ButtonPicture) do with ButtonPicture[b] do begin
             if not Assigned(OrgBitMap) then begin
                OrgBitMap:=BitMap; BitMap:=nil; // save the original image as 'OrgBitMap'
                end;
             if not Assigned(BitMap) then begin
                oAntiAliasing:=Antialiasing; AntiAliasing:=aaBilinear;
                //AntiAliasing:=aaFilter; // high-quality scaling using a sine-filter doesn't look good because of artifacts around the edges; the 'BitMap_.BitMapScale()' function needs a different algorithm if this problem should be rectified
                if   Assigned(OrgBitMap) and Resize((3*OrgBitMap.Width) div 2,(3*OrgBitMap.Height) div 2) then begin
                     end
                else Result:=0;
                AntiAliasing:=oAntiAliasing;
                end;
             end;
         end
      else begin
         Result:=1;
         for b:=Low(ButtonPicture) to High(ButtonPicture) do with ButtonPicture[b] do begin
             if Assigned(OrgBitMap) then begin
                BitMap.Free; BitMap:=OrgBitMap; OrgBitMap:=nil; // take the orginal image from 'OrgBitMap' and store is as 'BitMap'
                end;
             if not Assigned(BitMap) then Result:=0;
             end;
         end;
    except
      on E:Exception do begin
         Error(E.Message,Application.Title); Result:=0;
         end;
    end;
  end;

begin // TGameMenu.MakeButtons
  Inherited;

  fReverseMode:=False;
  DX:=ButtonDX; DY:=ButtonDY-2*BUTTON_OFFS_Y;
  ImageScaleFactor:=1;
//ImageScaleFactor:=ScaleButtonPictures;                                        // scaling the button images isn't in production; it's not fully implemented, and it doesn't look good with the currently implemented scaling functions

  if ImageScaleFactor>0 then begin
     ButtonWidth      :=ButtonPicture[bsEnabled].BitMap.Width;
     ButtonHeight     :=ButtonPicture[bsEnabled].BitMap.Height;
     MenuWidth        :=ButtonWidth+2*DX;
     MinMenuHeight    :=DY+((ButtonHeight+DY)*MIN_ROW_COUNT); // the minimum number of buttons rows
     MaxMenuHeight    :=DY+((ButtonHeight+DY)*MAX_ROW_COUNT); // all buttons

     MenuHeight       :=MaxMenuHeight;
     MenuRowCount     :=MAX_ROW_COUNT;
     while (MenuRowCount>MIN_ROW_COUNT) and
           (MainForm.ClientHeight<MenuHeight) do begin
           Dec(MenuRowCount); Dec(MenuHeight,ButtonHeight+DY);
           end;

     if   ButtonMirrorSlice then                                                // kludge: buttons are enlarged with a border at load-time to get the best results from the simple edge-smoothing function;
          ExtraWidth:=(2*(BUTTON_OFFS_X-BUTTON_OFFS_Y))*ImageScaleFactor        // thus, creating square buttons or circular buttons requires special attention.
     else ExtraWidth:=0;

     Insert(BTN_OPEN_CAPTION         ,Rect(DX,DY,DX+ButtonWidth,DY+ButtonHeight),MainForm.BtnOpen,ButtonGlyph[btOpen],False);

     if MenuRowCount>=MIN_ROW_COUNT+2 then begin

        Insert('<'                   ,Rect(DX,LastTop+ButtonHeight+DY,DX+ButtonHeight+ExtraWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnOpenPrior,ButtonGlyph[btOpenPrior],False);
        Insert('>'                   ,Rect(MenuItems[0].Rect.Right-ButtonHeight-ExtraWidth,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnOpenNext,ButtonGlyph[btOpenNext],False);

        i:=DX+ButtonHeight+DX; j:=LastLeft-i-DX;
        if   j>=ButtonHeight+ExtraWidth then begin
//        Insert(''                  ,Rect(i,LastTop,LastLeft-DX,LastTop+ButtonHeight),MainForm.BtnOpenClipboard,ButtonGlyph[btOpenClipboard]);
          Insert(''                  ,Rect(i+((j-ButtonHeight) div 2),LastTop,i+((j-ButtonHeight) div 2)+ButtonHeight+ExtraWidth,LastTop+ButtonHeight),MainForm.BtnOpenClipboard,ButtonGlyph[btOpenClipboard],False);
          end
        else begin
          i:=(ButtonWidth div 2)-(DX div 2); j:=LastTop+ButtonHeight+DY;
          Insert(''                  ,Rect(DX+(ButtonWidth div 4),j,DX+(ButtonWidth div 4)+i+ExtraWidth,j+ButtonHeight),MainForm.BtnOpenClipboard,ButtonGlyph[btOpenClipboard],False);
          end;
        end;
//
     if MenuRowCount>=MIN_ROW_COUNT+5 then begin
        i:=(ButtonWidth div 2)-(DX div 2);
        Insert(BTN_SAVE_CAPTION      ,Rect(DX,LastTop+ButtonHeight+DY,DX+i,LastTop+2*ButtonHeight+DY),MainForm.BtnSave,ButtonGlyph[btSave],False);
        Insert(BTN_SAVE_AS_CAPTION   ,Rect(MenuItems[0].Rect.Right-i,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnSaveAs,ButtonGlyph[btSaveAs],False);
        end;
//
     if MenuRowCount>=MIN_ROW_COUNT+1 then begin
        //Insert('',Rect(DX,LastTop+ButtonHeight+DY,DX+ButtonHeight+ExtraWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnRotate,ButtonGlyph[btRotate],False);
        //Insert(BTN_SOLUTION_CAPTION  ,Rect(DX+(ButtonWidth-ButtonHeight-ExtraWidth) div 2,LastTop,DX+ButtonWidth,LastTop+ButtonHeight),MainForm.BtnSolution,ButtonGlyph[btSolution],False);
        Insert(BTN_SOLUTION_CAPTION  ,Rect(DX,LastTop+ButtonHeight+DY,DX+ButtonWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnSolution,ButtonGlyph[btSolution],False);
        end;

{
     i:=(ButtonWidth-2*(ButtonHeight+ExtraWidth)) div 3;
     Insert('<'                      ,Rect(DX+i,LastTop+ButtonHeight+DY,DX+i+ButtonHeight+ExtraWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnUndo,ButtonGlyph[btUndo]);
     Insert('>'                      ,Rect(MenuItem[0].Rect.Right-i-ButtonHeight-ExtraWidth,LastTop,MenuItem[0].Rect.Right-i,LastTop+ButtonHeight),MainForm.BtnRedo,ButtonGlyph[btRedo]);
     Insert('|<'                     ,Rect(DX+i,LastTop+ButtonHeight+DY,DX+i+ButtonHeight+ExtraWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnNew,ButtonGlyph[btNew]);
     Insert('>|'                     ,Rect(MenuItem[0].Rect.Right-i-ButtonHeight-ExtraWidth,LastTop,MenuItem[0].Rect.Right-i,LastTop+ButtonHeight),MainForm.BtnRedoAll,ButtonGlyph[btRedoAll]);
}
     Insert('<'                      ,Rect(DX,LastTop+ButtonHeight+DY,DX+ButtonHeight+ExtraWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnUndo,ButtonGlyph[btUndo],False);
     Insert('>'                      ,Rect(MenuItems[0].Rect.Right-ButtonHeight-ExtraWidth,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnRedo,ButtonGlyph[btRedo],False);
     Insert('|<'                     ,Rect(DX,LastTop+ButtonHeight+DY,DX+ButtonHeight+ExtraWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnReset,ButtonGlyph[btReset],False);
     Insert('>|'                     ,Rect(MenuItems[0].Rect.Right-ButtonHeight-ExtraWidth,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnRedoAll,ButtonGlyph[btReplay],False);
     i:=DX+ButtonHeight+DX; j:=LastLeft-i-DX;
     if   j>=ButtonHeight+ExtraWidth then
          Insert(''            ,Rect(i+((j-ButtonHeight) div 2),LastTop,i+((j-ButtonHeight) div 2)+ButtonHeight+ExtraWidth,LastTop+ButtonHeight),MainForm.BtnSplitView,ButtonGlyph[btSplitView],False);


     if MenuRowCount>=MIN_ROW_COUNT+3 then begin
        {$IFDEF MUSIC_PLAYER}
           Insert(''                 ,Rect(DX,LastTop+ButtonHeight+DY,DX+ButtonHeight+ExtraWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnMPlayer,ButtonGlyph[btMPlayer],False);
           Insert(''                 ,Rect(MenuItems[0].Rect.Right-ButtonHeight-ExtraWidth,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnReverseMode,ButtonGlyph[btReverseMode],False);
           i:=DX+ButtonHeight+DX; j:=LastLeft-i-DX;
           if   j>=ButtonHeight+ExtraWidth then
                Insert(''            ,Rect(i+((j-ButtonHeight) div 2),LastTop,i+((j-ButtonHeight) div 2)+ButtonHeight+ExtraWidth,LastTop+ButtonHeight),MainForm.BtnRotate,ButtonGlyph[btRotate],False);
        {$ELSE}
{
           Insert(''                 ,Rect(DX,LastTop+ButtonHeight+DY,DX+ButtonHeight+ExtraWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnRotate,ButtonGlyph[btRotate],False);
           Insert(''                 ,Rect(MenuItems[0].Rect.Right-ButtonHeight-ExtraWidth,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnReverseMode,ButtonGlyph[btReverseMode],False);
           i:=DX+ButtonHeight+DX; j:=LastLeft-i-DX;
           if   j>=ButtonHeight+ExtraWidth then
                Insert(''            ,Rect(i+((j-ButtonHeight) div 2),LastTop,i+((j-ButtonHeight) div 2)+ButtonHeight+ExtraWidth,LastTop+ButtonHeight),MainForm.BtnSnapshots,ButtonGlyph[btSnapshot],False);
}
           Insert(''                 ,Rect(DX,LastTop+ButtonHeight+DY,DX+ButtonHeight+ExtraWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnRotate,ButtonGlyph[btRotate],False);
           Insert(''                 ,Rect(MenuItems[0].Rect.Right-ButtonHeight-ExtraWidth,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnSnapshots,ButtonGlyph[btSnapshot],False);
           i:=DX+ButtonHeight+DX; j:=LastLeft-i-DX;
           if   j>=ButtonHeight+ExtraWidth then
                Insert(''            ,Rect(i+((j-ButtonHeight) div 2),LastTop,i+((j-ButtonHeight) div 2)+ButtonHeight+ExtraWidth,LastTop+ButtonHeight),MainForm.BtnReverseMode,ButtonGlyph[btReverseMode],False);


//         i:=(ButtonWidth div 2)-(DX div 2);
//         Insert(''                 ,Rect(DX,LastTop+ButtonHeight+DY,DX+i,LastTop+2*ButtonHeight+DY),MainForm.BtnRotate,ButtonGlyph[btRotate],False);
//         Insert(''                 ,Rect(MenuItems[0].Rect.Right-i,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnReverseMode,ButtonGlyph[btReverseMode],False);
        {$ENDIF}
        end;
{
     if MenuRowCount>=MAX_ROW_COUNT then
        Insert(BTN_EDIT_CAPTION      ,Rect(DX+(ButtonWidth-ButtonHeight-ExtraWidth) div 2,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnTools,ButtonGlyph[btTools],False)
     else
        Insert(MainForm.BtnMenu.Caption
                                  ,Rect(DX+(ButtonWidth-ButtonHeight-ExtraWidth) div 2,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnMenu,ButtonGlyph[btMenu],False);
}
//   Insert(BTN_MODE_CAPTION[True],Rect(DX,LastTop+ButtonHeight+DY,DX+i,LastTop+2*ButtonHeight+DY),MainForm.BtnReverseMode,ButtonGlyph[btReverseMode],False);
//   Insert(BTN_EDIT_CAPTION      ,Rect(MenuItems[0].Rect.Right-i,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnTools,ButtonGlyph[btTools],False);

//   Insert(BTN_EDIT_CAPTION      ,Rect(MenuItems[0].Rect.Right-i,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnTools,ButtonGlyph[btTools],False,False,clBlack);

     if MenuRowCount>=MIN_ROW_COUNT+4 then begin
        //Insert('',Rect(DX,LastTop+ButtonHeight+DY,DX+ButtonHeight+ExtraWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnRotate,ButtonGlyph[btRotate],False);
        //Insert(BTN_OPTIONS_CAPTION   ,Rect(DX+(ButtonWidth-ButtonHeight-ExtraWidth) div 2,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnOptions,ButtonGlyph[btSettings],False);
        Insert(BTN_OPTIONS_CAPTION   ,Rect(DX,LastTop+ButtonHeight+DY,DX+ButtonWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnOptions,ButtonGlyph[btSettings],False);
        end;

//   Insert(BTN_HELP_CAPTION         ,Rect(0,DY,0,0),MainForm.BtnHelp,ButtonGlyph[btHelp],False);
//   Insert(BTN_EXIT_CAPTION         ,Rect(0,DY,0,0),MainForm.BtnExit,ButtonGlyph[btExit],False);

     i:=(ButtonWidth div 2)-(DX div 2);
     if MenuRowCount>=MAX_ROW_COUNT then
        Insert(BTN_TOOLS_CAPTION     ,Rect(DX,LastTop+ButtonHeight+DY,DX+i,LastTop+2*ButtonHeight+DY),MainForm.BtnTools,ButtonGlyph[btTools],False)
     else
        Insert(MainForm.BtnMenu.Caption
                                     ,Rect(DX,LastTop+ButtonHeight+DY,DX+i,LastTop+2*ButtonHeight+DY),MainForm.BtnMenu,ButtonGlyph[btMenu],False);
     Insert(BTN_HELP_CAPTION         ,Rect(MenuItems[0].Rect.Right-i,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnHelp,ButtonGlyph[btHelp],False);

//   Insert(''                       ,Rect(DX,LastTop+ButtonHeight+DY,DX+i,LastTop+2*ButtonHeight+DY),MainForm.BtnMPlayer,ButtonGlyph[btMPlayer],False);
//   Insert(BTN_HELP_CAPTION         ,Rect(MenuItems[0].Rect.Right-i,LastTop,MenuItems[0].Rect.Right,LastTop+ButtonHeight),MainForm.BtnHelp,ButtonGlyph[btHelp],False);

     Insert(BTN_EXIT_CAPTION         ,Rect(DX,LastTop+ButtonHeight+DY,DX+ButtonWidth,LastTop+2*ButtonHeight+DY),MainForm.BtnExit,ButtonGlyph[btExit],False);

     {$IFDEF MUSIC_PLAYER}
       j:=-1;
       for i:=0 to Pred(Count) do
           if MenuItems[i].SpeedButton=MainForm.BtnRedo  then j:=i;
       if j<>-1 then begin
          R:=Bounds(DX+(ButtonWidth-ButtonHeight-ExtraWidth) div 2,
                    MenuItems[j].Rect.Top+(ButtonHeight+DY)  div 2,
                    ButtonHeight+ExtraWidth,
                    ButtonHeight);
          for i:=0 to Pred(Count) do
              if InterSectRect(R1,R,MenuItems[i].Rect) then j:=-1;
          if j<>-1 then Insert('' ,R,MainForm.BtnSnapshots,ButtonGlyph[btSnapshot],False);
//        if j<>-1 then Insert('' ,R,MainForm.BtnBookmark,ButtonGlyph[btSnapshot],False,False,clBlack);
          end;
     {$ENDIF}
{
     i:=Image.ClientWidth;
     Insert(''                  ,Rect(i-ButtonHeight-ExtraWidth-DX,LastTop+ButtonHeight+DY,i-DX,LastTop+2*ButtonHeight+DY),
                                 MainForm.BtnMPlayer,ButtonGlyph[btMPlayer],False);

}
     end;

  BtnResetIndex        :=-1;    BtnBookmarksIndex :=-1; BtnSnapshotsIndex :=-1;
  BtnRedoIndex         :=-1;    BtnRedoAllIndex   :=-1; BtnSolutionIndex  :=-1;
  BtnReverseModeIndex  :=-1;    BtnUndoIndex      :=-1; BtnMPlayerIndex   :=-1;
  BtnMenuIndex         :=-1;    BtnSaveIndex      :=-1; BtnSettingsIndex  :=-1;
  BtnPreviousLevelIndex:=-1;    BtnNextLevelIndex :=-1; BtnRotateIndex    :=-1;
  BtnToolsIndex        :=-1;    BtnSplitViewIndex :=-1;

  for i:=0 to Pred(Count) do with MenuItems[i]    do
      if      SpeedButton=MainForm.BtnMenu        then BtnMenuIndex         :=i
      else if SpeedButton=MainForm.BtnMPlayer     then BtnMPlayerIndex      :=i
      else if SpeedButton=MainForm.BtnOpenPrior   then BtnPreviousLevelIndex:=i
      else if SpeedButton=MainForm.BtnOpenNext    then BtnNextLevelIndex    :=i
      else if SpeedButton=MainForm.BtnRedo        then BtnRedoIndex         :=i
      else if SpeedButton=MainForm.BtnRedoAll     then BtnRedoAllIndex      :=i
      else if SpeedButton=MainForm.BtnReset       then BtnResetIndex        :=i
      else if SpeedButton=MainForm.BtnReverseMode then BtnReverseModeIndex  :=i
      else if SpeedButton=MainForm.BtnRotate      then BtnRotateIndex       :=i
      else if SpeedButton=MainForm.BtnSave        then BtnSaveIndex         :=i
      else if SpeedButton=MainForm.BtnOptions     then BtnSettingsIndex     :=i
      else if SpeedButton=MainForm.BtnBookmarks   then BtnBookmarksIndex    :=i
      else if SpeedButton=MainForm.BtnSolution    then BtnSolutionIndex     :=i
      else if SpeedButton=MainForm.BtnUndo        then BtnUndoIndex         :=i
      else if SpeedButton=MainForm.BtnSnapshots   then BtnSnapshotsIndex    :=i
      else if SpeedButton=MainForm.BtnTools       then BtnToolsIndex        :=i
      else if SpeedButton=MainForm.BtnSplitView   then BtnSplitViewIndex    :=i;

//if BtnMPlayerIndex>=0 then MenuItems[BtnMPlayerIndex].Visible:=(MainForm.Music<>nil) and MainForm.Music.Enabled;
end;

procedure TGameMenu.SetReverseMode(ReverseMode__:Boolean);
begin
  if fReverseMode<>ReverseMode__ then
     if BtnReverseModeIndex>=0 then with MenuItems[BtnReverseModeIndex] do begin
        fReverseMode                    :=ReverseMode__;
        Caption                         :=BTN_MODE_CAPTION[not ReverseMode];
        SpeedButton.Hint                :=GameModeHintText[    ReverseMode];
        if   ReverseMode then
             BuiltinGlyph[btReverseMode]:=MainForm.NormalModeImage .Picture.BitMap
        else BuiltinGlyph[btReverseMode]:=MainForm.ReverseModeImage.Picture.BitMap;
        ButtonGlyph[btReverseMode]      :=BuiltinGlyph[btReverseMode];
        Glyph                           :=ButtonGlyph [btReverseMode];
        ClearButton(BtnReverseModeIndex);
        end;
end;

constructor TMPlayerDisplayMenu.Create(Destination__:TObject; MenuPanel__:TPanel; FontSize__:Integer);
begin
  Inherited Create(Destination__,nil,FontSize__);
  if Destination__ is TBitMap then BitMap:=TBitMap(Destination__)
  else BitMap:=nil;
  MenuPanel:=MenuPanel__; // kludge: inititalize here, and not in 'Inherited.Crate', to avoid initialization of 'MenuPanel.Font' when 'TMenu0.Create' calls 'SetDefaultValues'
  Pict:=nil;
  if MenuPanel<>nil then MenuPanel.Font.Size:=FontSize__;
end;

destructor TMPlayerDisplayMenu.Destroy;
begin
  Hide;
  Pict.Free;
  Inherited;
end;

const
  MPLAYER_DISPLAY_MENU_SETTINGS_SUFFIX = 'Menu'; // don't localize

function  TMPlayerDisplayMenu.LoadSettingsFromIniFile(const IniFile: TIniFile;
      const Section: String): Boolean;
var s:String; b:Menu_.TButtonState;
begin
  s:=Section+' - '+MPLAYER_DISPLAY_MENU_SETTINGS_SUFFIX;
  MenuPanel.Color               :=TColor(         IniFile.ReadInteger(s,'BackgroundColor',Integer(MenuPanel.Color)));
  TransparencyPct               :=Max(0,Min(  100,IniFile.ReadInteger(s,'TransparencyPct',TransparencyPct)));
  TextShadow                    :=                IniFile.ReadBool   (s,'TextShadow',TextShadow);
  TextShadowColor[bsEnabled]    :=                IniFile.ReadInteger(s,'TextShadowColor',Integer(TextShadowColor[bsEnabled]));
  for b:=Low(b) to High(b) do begin
      ButtonFontColor[b]        :=TColor(         IniFile.ReadInteger(s,PICTURE_TYPE_NAME[b]+'.FontColor',Integer(ButtonFontColor[b])));
      TextShadowColor[b]        :=TextShadowColor[bsEnabled];
      end;
  ButtonFontColor[bsFocusedDisabled]
                                :=ButtonFontColor[bsDisabled];
  Result:=LoadFontFromIniFile(IniFile,s,'',MenuPanel.Font);
end;

function  TMPlayerDisplayMenu.SaveSettingsToIniFile(const IniFile: TIniFile;
      const Section: String): Boolean;
var s:String; b:Menu_.TButtonState;
begin
  s:=Section+' - '+MPLAYER_DISPLAY_MENU_SETTINGS_SUFFIX;
  IniFile.WriteInteger(s,'BackgroundColor',Integer(MenuPanel.Color));
  IniFile.WriteInteger(s,'TransparencyPct',TransparencyPct);
  IniFile.WriteBool   (s,'TextShadow',TextShadow);
  IniFile.WriteInteger(s,'TextShadowColor',Integer(TextShadowColor[bsEnabled]));
  for b:=Low(b) to High(b) do
      IniFile.WriteInteger(s,PICTURE_TYPE_NAME[b]+'.FontColor',Integer(ButtonFontColor[b]));
  Result:=SaveFontToIniFile(IniFile,s,'',MenuPanel.Font);
end;

procedure TMPlayerDisplayMenu.SetDefaultValues(BackgroundColor:TColor; FontSize:Integer);
var b:TButtonState;
begin
  Inherited;

  if   BitMap<>nil then
       MenuRect                 :=Classes.Rect(0,0,BitMap.Width,BitMap.Height)
  else MenuRect                 :=Classes.Rect(0,0,1,1); // adjusted later
  TransparencyPct               :=70;
  TextShadow                    :=True;
  for b:=Low(TextShadowColor) to High(TextShadowColor) do
      TextShadowColor[b]        :=clBlack;
  fVisible                      :=False;

  if MenuPanel<>nil then with MenuPanel do begin
    Color                       :=BackgroundColor; //$00D7FF; // gold; //clYellow; //$d6ec1a;
    Font.Name:='Arial'; Font.Size:=10; Font.Color:=clWhite; Font.Style:=[fsBold];
    end;

  ButtonFontColor[Menu_.bsDisabled       ] :=clLtGray;
  ButtonFontColor[Menu_.bsEnabled        ] :=clWhite;
  ButtonFontColor[Menu_.bsFocusedEnabled ] :=clYellow;
  ButtonFontColor[Menu_.bsFocusedDisabled] :=ButtonFontColor[bsDisabled];
end;

procedure TMPlayerDisplayMenu.SetVisible(Visible__:Boolean);
begin
  if Visible__<>Visible then
     if Visible__ then Show
     else Hide;
end;

procedure TMPlayerDisplayMenu.EnableDisableButtons;
begin
end;

procedure TMPlayerDisplayMenu.Show;

  procedure Paint;
  var s:String;

    procedure ShowButtons;
    var i:Integer;
    begin
      for i:=0 to Pred(Count) do MenuItems[i].State:=Menu_.bsEnabled;

      EnableDisableButtons;

      Inherited Show;
    end; // ShowButtons

  begin // Paint
    if   MainForm.MPlayer<>nil then
         BitMap:=MainForm.MPlayer.Display.BitMaps[Ord(dlFocus)]
    else BitMap:=nil;

    if BitMap<>nil then
       try
         Canvas:=BitMap.Canvas;

         MainForm.MPlayer.Display.ClearLayer(dlFocus);

         Pict.BitMap.Canvas.CopyRect(Pict.BitMapRect,BitMap.Canvas,DestRect);

         Pict.BitMap.Canvas.Brush.Color:=MenuPanel.Color;
         Pict.BitMap.Canvas.Pen  .Color:=MenuPanel.Color;
         Pict.BitMap.Canvas.FillRect(Rect(2,2,Pict.BitMap.Width-2,Pict.BitMap.Height-2));

         AlphaBlend(BitMap,Pict.BitMap,Pict.MaskBitMap,DestRect,Pict.BitMapRect,100-TransparencyPct,0,False);

         Write(MenuTexts[0],Rect(2+DestRect.Left,2+DestRect.Top,DestRect.Right-2,DestRect.Top+TextSize.cy+2),0,MenuPanel.Font.Color);
         BitMap.Canvas.Pen .Color:=MenuPanel.Font.Color;
         with BitMap.Canvas do begin
              MoveTo(DestRect.Left +8,DestRect.Top+TextSize.cy+2);
              LineTo(DestRect.Right-8,DestRect.Top+TextSize.cy+2);
              if TextShadow then begin
                 BitMap.Canvas.Pen.Color:=TextShadowColor[bsEnabled];
                 MoveTo(DestRect.Left +9,DestRect.Top+TextSize.cy+3);
                 LineTo(DestRect.Right-7,DestRect.Top+TextSize.cy+3);
                 end;
              end;

         MainForm.MPlayer.Display.ClearLayer(Pred(dlFocus));
         if   MainForm.MPlayer.Display.Text=ClickOpenText then
              MainForm.MPlayer.Display.Text:='';

         ShowButtons;
       except on E:Exception do begin
              s:='Error in "TMPlayerDisplayMenu.Paint": ';
              if Pict=nil then s:=s+' Pict=NIL'
              else if Pict.BitMap=nil then s:=s+' Pict.BitMap=NIL';
              Error(s,Application.Title);
              end;
       end;
  end; // Paint

begin // Show
  if BitMap=nil then
     MakeButtons;

  if BitMap<>nil then begin
     fVisible:=True;

     Paint;

     MainForm.MPlayer.Display.ClearLayer(Pred(dlFocus));
     if not MainForm.MPlayer.Animation then
        MainForm.MPlayer.Display.ShowText;
     end;
end;

procedure TMPlayerDisplayMenu.Hide;
begin
  if Visible then begin
     fVisible:=False;
     if MainForm.MPlayer<>nil then begin
        MainForm.MPlayer.Display.ClearLayer(dlFocus);
        if   (MainForm.MPlayer.Display.Text='') and
             (MainForm.MPlayer.Buttons[Ord(MPlayer1_.btOpen)].Visible) then
             MainForm.MPlayer.Display.Text:=ClickOpenText;
        if not MainForm.MPlayer.Animation then
           MainForm.MPlayer.Display.ShowText;
        end;
     end;
end;

procedure TMPlayerDisplayMenu.Write(const s:String; const R:TRect; Flags:Cardinal; FontColor:TColor);
var R1,R2:TRect;
begin
  BitMap.Canvas.CopyMode:=cmSrcCopy; R1:=R;
  Windows.SetBkMode (BitMap.Canvas.Handle, Windows.TRANSPARENT);

  if TextShadow then begin
     BitMap.Canvas.Font.Color:=TextShadowColor[bsEnabled];
     R2:=Classes.Rect(Succ(R1.Left),Succ(R1.Top),Succ(R1.Right),Succ(R1.Bottom));
     Windows.DrawTextEx(BitMap.Canvas.Handle,PChar(s),Length(s),R2,DT_CENTER or DT_VCENTER or DT_WORDBREAK,nil);
     end;

  BitMap.Canvas.Font.Color:=FontColor;
  Windows.DrawTextEx(BitMap.Canvas.Handle,PChar(s),Length(s),R1,Flags or DT_CENTER or DT_VCENTER or DT_WORDBREAK {or DT_SINGLELINE},nil);
  Windows.SetBkMode (BitMap.Canvas.Handle, Windows.OPAQUE);
end;

function  TMPlayerDisplayMenu.GetMenuTexts(Index:Integer):String;
begin
  Result:='';
end;

function  TMPlayerDisplayMenu.GetMenuTextsCount:Integer;
begin
  Result:=0;
end;

function  TMPlayerDisplayMenu.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer):Boolean;
begin
  if Button=mbLeft then begin
     Result:=True;
     end
  else if Button=mbRight then begin Hide; Result:=True; end
       else Result:=False;
end;

function  TMPlayerDisplayMenu.MouseMove(X,Y: Integer):Integer;
begin
  Inherited MouseMove(X,Y);
  if not MainForm.MPlayer.Animation then
     MainForm.MPlayer.Display.ShowText;
  Result:=-1;
end;

procedure TMPlayerDisplayMenu.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MainForm.Status.Hint:='';
end;

procedure TMPlayerDisplayMenu.MakeButtons;
var i,W,H:Integer; Size:TSize;
begin
  Inherited;

  FillChar(DestRect,SizeOf(DestRect),0);
  if MainForm<>nil then BitMap:=MainForm.MPlayer.Display.BitMaps[Ord(dlFocus)];

  if Pict=nil then
     try    Pict      :=TPict.Create;
     except on E:Exception do begin Pict.Free; Pict:=nil; end;
     end;

  if (BitMap<>nil) and
     (Pict<>nil) and
     (Pict.BitMap<>nil) and
     (MenuPanel<>nil) then begin

     BitMap.Canvas.Font.Assign(MenuPanel.Font);

     FillChar(TextSize,SizeOf(TextSize),0);
     for i:=0 to Pred(MenuTextsCount) do begin
         Size:=BitMap.Canvas.TextExtent(MenuTexts[i]);
         TextSize.cx:=Max(TextSize.cx,Size.cx);
         TextSize.cy:=Max(TextSize.cy,Size.cy);
         end;
     MenuWidth:=Min(TextSize.cx+20,BitMap.Width);

     DestRect:=Classes.Rect(BitMap.Width-MenuWidth,
                            0,
                            BitMap.Width,
                            BitMap.Height);
     W:=RectWidth(DestRect); H:=RectHeight(DestRect);
     Pict.Resize(W,H);
     if Pict.BitMap<>nil then begin
        Pict.BitMap.Canvas.Font.Assign(MenuPanel.Font);
        DestRect:=Classes.Rect(BitMap.Width-Pict.BitMap.Width,
                               0,
                               BitMap.Width,
                               Pict.BitMap.Height);

        if   Graphics.ColorToRGB(MenuPanel.Color)=Graphics.ColorToRGB(clBlack) then
             Pict.MaskBitMapColor:=RGB_WHITE
        else Pict.MaskBitMapColor:=RGB_BLACK;
        Pict.BitMap.Canvas.Brush.Color:=RGBToColor(Pict.MaskBitMapColor);
        Pict.BitMap.Canvas.FillRect(Pict.BitMapRect);

        Pict.BitMap.Canvas.Brush.Color:=MenuPanel.Color;
        Pict.BitMap.Canvas.Pen  .Color:=MenuPanel.Color;
        //Pict.BitMap.Canvas.FillRect(Pict.BitMapRect);
        //Pict.BitMap.Canvas.RoundRect(5,5,Pict.BitMap.Width-5,Pict.BitMap.Height-5,20,20);
        Pict.BitMap.Canvas.FillRect(Rect(2,2,Pict.BitMap.Width-2,Pict.BitMap.Height-2));

        Pict.Masked:=True;
        Pict.MakeMaskBitMap(Pict.MaskBitMapColor,0,Pict.MaskExteriorOnly);
        end;
     end;
  if (BitMap=nil) or (Pict=nil) or (Pict.BitMap=nil) then
     BitMap:=nil; // 'BitMap' used as a flag for proper initialization
end;

procedure TActivitiesMenu.MakeButtons;
var i,X,Y,W:Integer; Size:TSize; R:TRect;

  function SpeedButton(Index:TActivitiesButtonType):TSpeedButton;
  begin
    with MainForm do
      case Index of
         Display_.abMandala   : Result := BtnActivitiesMandala;
         Display_.abImages    : Result := BtnActivitiesImages;
{        Display_.abFireworks : Result := BtnActivitiesFireworks;}
         Display_.abFractals  : Result := BtnActivitiesFractals;
         Display_.abSettings  : Result := BtnActivitiesSettings;
         else                   Result :=nil;
      end; // case
  end;

begin // TActivitiesMenu.MakeButtons
  Inherited;

  if BitMap<>nil then begin
     X:=DestRect.Left; Y:=DestRect.Top+TextSize.cy+6;
     W:=RectWidth(DestRect);

     for i:=1 to Pred(MenuTextsCount) do begin // 1: items[0]=title;
         Size:=BitMap.Canvas.TextExtent(MenuTexts[i]);
         R:=Classes.Rect(X+((W-Size.cx) div 2),Y,X+((W-Size.cx) div 2)+Size.cx,Y+TextSize.cy);
         Insert(MenuTexts[i],R,SpeedButton(TActivitiesButtonType(Pred(i))),nil,True);
         Inc(Y,TextSize.cy);
         end;
     end;

end;

function  TActivitiesMenu.GetMenuTexts(Index:Integer):String;
begin
  Result:=ActivitiesMenuText[Index];
end;

function  TActivitiesMenu.GetMenuTextsCount:Integer;
begin
  Result:=Succ(High(ActivitiesMenuText)-Low(ActivitiesMenuText));
end;

procedure TMandalaMenu.MakeButtons;
var i,X,Y,W:Integer; Size:TSize; R:TRect;

  function SpeedButton(Index:TMandalaButtonType):TSpeedButton;
  begin
    with MainForm do
      case Index of
         Mandal1_.mbStartStop : Result := BtnMandalaStartStop;
         Mandal1_.mbSaveAs    : Result := BtnMandalaSaveAs;
         Mandal1_.mbSettings  : Result := BtnMandalaSettings;
         else                   Result:=nil;
      end; // case
  end;

begin  // TMandalaMenu.MakeButtons
  Inherited;

  if BitMap<>nil then begin
     X:=DestRect.Left; Y:=DestRect.Top+TextSize.cy+6;
     W:=RectWidth(DestRect);

     for i:=1 to MenuTextsCount-2 do begin // 1: items[0]=title; -2: last is skipped
         Size:=BitMap.Canvas.TextExtent(MenuTexts[i]);
         R:=Classes.Rect(X+((W-Size.cx) div 2),Y,X+((W-Size.cx) div 2)+Size.cx,Y+TextSize.cy);
         Insert(MenuTexts[i],R,SpeedButton(TMandalaButtonType(Pred(i))),nil,True);
         Inc(Y,TextSize.cy);
         end;
     end;
end;

function  TMandalaMenu.GetMenuTexts(Index:Integer):String;
begin
  Result:=MandalaMenuText[Index];
end;

function  TMandalaMenu.GetMenuTextsCount:Integer;
begin
  Result:=Succ(High(MandalaMenuText)-Low(MandalaMenuText));
end;

procedure TMandalaMenu.EnableDisableButtons;
begin
  Inherited;
  if Mandala<>nil then with Mandala do begin
     if   Suspended then
          Menu.MenuItems[Ord(Mandal1_.mbStartStop)].Caption  :=MenuTexts[Succ(Ord(Mandal1_.mbStartStop))]
     else Menu.MenuItems[Ord(Mandal1_.mbStartStop)].Caption  :=MenuTexts[Pred(MenuTextsCount)];
     Menu.MenuItems[Ord(Mandal1_.mbStartStop)].SpeedButton.Hint:=MandalaStartStopHintText[not Suspended];
     end;
end;

procedure TViewer1Menu.MakeButtons;
var i,X,Y,W:Integer; Size:TSize; R:TRect;

  function SpeedButton(Index:TViewer1ButtonType):TSpeedButton;
  begin
    with MainForm do
      case Index of
         IView1_.vbOpen       : Result := BtnViewer1Open;
         IView1_.vbPrior      : Result := BtnViewer1Prior;
         IView1_.vbNext       : Result := BtnViewer1Next;
         IView1_.vbSize       : Result := BtnViewer1Size;
         IView1_.vbSettings   : Result := BtnViewer1Settings;
         IView1_.vbSlideShow  : Result := BtnViewer1SlideShow;
         else                   Result:=nil;
      end; // case
  end;

begin //TViewer1Menu.MakeButtons
  Inherited;

  if BitMap<>nil then begin
     X:=DestRect.Left; Y:=DestRect.Top+TextSize.cy+6;
     W:=RectWidth(DestRect);
     for i:=1 to MenuTextsCount-2 do begin // 1: items[0]=title; -2: last is skipped
         Size:=BitMap.Canvas.TextExtent(MenuTexts[i]);
         R:=Classes.Rect(X+((W-Size.cx) div 2),Y,X+((W-Size.cx) div 2)+Size.cx,Y+TextSize.cy);
         if i=2 then with R do begin
            Left:=X+((W-3*Size.cx) div 2); Right:=Left+Size.cx;
            end
         else if i=3 then with R do begin
                 Left:=X+W-((W-3*Size.cx) div 2)-Size.cx; Right:=Left+Size.cx;
                 end;
         Insert(MenuTexts[i],R,SpeedButton(TViewer1ButtonType(Pred(i))),nil,True);
         if i<>2 then Inc(Y,TextSize.cy);
         end;
     end;
end;

procedure TViewer1Menu.EnableDisableButtons;
var b:Boolean;
begin
  Inherited;
  if Viewer1<>nil then with Viewer1 do begin
     IView1_.Data.Enter;
     try
       b:=(PictThread<>nil) and (PictThread.FileListItemsCount>1);
       if not b then Data.SlideShow:=False;

       MenuItems[Ord(IView1_.vbPrior    )].SpeedButton.Enabled:=b;
       MenuItems[Ord(IView1_.vbNext     )].SpeedButton.Enabled:=b;
       MenuItems[Ord(IView1_.vbSlideShow)].SpeedButton.Enabled:=b;

       MenuItems[Ord(IView1_.vbSize     )].SpeedButton.Enabled:=FileName<>DEFAULT_VALUE;
       MenuItems[Ord(IView1_.vbSize     )].Caption            :=Viewer1ResizeToWindowWidthText    [not Data.ResizeToWindowWidth];
       MenuItems[Ord(IView1_.vbSize     )].SpeedButton.Hint   :=Viewer1ResizeToWindowWidthHintText[Data.ResizeToWindowWidth];

       MenuItems[Ord(IView1_.vbSlideShow)].SpeedButton.Hint   :=SlideShowHintText[Data.SlideShow];
       if   Data.SlideShow then
            Menu.MenuItems[Ord(IView1_.vbSlideShow)].Caption  :=Viewer1MenuText[High(Viewer1MenuText)]
       else Menu.MenuItems[Ord(IView1_.vbSlideShow)].Caption  :=Viewer1MenuText[Pred(High(Viewer1MenuText))];
     finally
       IView1_.Data.Leave;
     end;
     end;
end;

function  TViewer1Menu.GetMenuTexts(Index:Integer):String;
begin
  Result:=Viewer1MenuText[Index];
end;

function  TViewer1Menu.GetMenuTextsCount:Integer;
begin
  Result:=Succ(High(Viewer1MenuText)-Low(Viewer1MenuText));
end;

procedure TFractalsMenu.MakeButtons;
var i,X,Y,W,W2:Integer; Size:TSize; R:TRect;

  function SpeedButton(Index:TFractalsButtonType):TSpeedButton;
  begin
    with MainForm do
      case Index of
         Fractal_.fbPrior     : Result := BtnFractalsPrior;
         Fractal_.fbReset     : Result := BtnFractalsReset;
         Fractal_.fbNext      : Result := BtnFractalsNext;
         Fractal_.fbColors    : Result := BtnFractalsColors;
         Fractal_.fbSaveAs    : Result := BtnFractalsSaveAs;
         Fractal_.fbSettings  : Result := BtnFractalsSettings;
         else                   Result:=nil;
      end; // case
  end;

begin // TFractalsMenu.MakeButtons
  Inherited;

  if BitMap<>nil then begin
     X:=DestRect.Left; Y:=DestRect.Top+TextSize.cy+6;
     W:=RectWidth(DestRect);

     Size:=BitMap.Canvas.TextExtent(MenuTexts[2]);
     W2:=(W-Size.cx) div 2;

     for i:=1 to Pred(MenuTextsCount) do begin // 1: items[0]=title
         Size:=BitMap.Canvas.TextExtent(MenuTexts[i]);
         if   i=1 then
              R:=Classes.Rect(X+((W2-Size.cx) div 2),Y,X+((W2-Size.cx) div 2)+Size.cx,Y+TextSize.cy)
{        else if i=2 then begin
              Glyph:=MainForm.MandelbrotImage.Picture.BitMap;
              Size.cx:=MainForm.MandelbrotImage.Width div 4;
              R:=Classes.Rect(X+((W-Size.cx) div 2),Y,X+((W-Size.cx) div 2)+Size.cx,Y+MainForm.MandelbrotImage.Height);
              end}
         else if i=3 then
              R:=Classes.Rect(DestRect.Right-((W2-Size.cx) div 2)-Size.cx,Y,DestRect.Right-((W2-Size.cx) div 2),Y+TextSize.cy)
         else R:=Classes.Rect(X+((W-Size.cx) div 2),Y,X+((W-Size.cx) div 2)+Size.cx,Y+TextSize.cy);
         Insert(MenuTexts[i],R,SpeedButton(TFractalsButtonType(Pred(i))),nil,True);
         if i>=3 then Inc(Y,TextSize.cy);
         end;
     end;
end;

procedure TFractalsMenu.EnableDisableButtons;
begin
  Inherited;
  if (BitMap<>nil) and (Fractals<>nil) then begin
     MenuItems[Ord(fbPrior)].SpeedButton.Enabled:=(Fractals.ZoomList<>nil) and (Fractals.ZoomList.Prev<>nil);
     MenuItems[Ord(fbReset)].SpeedButton.Enabled:=MenuItems[Ord(fbPrior)].SpeedButton.Enabled or Fractals.PaletteCycled;
     MenuItems[Ord(fbNext)] .SpeedButton.Enabled:=(Fractals.ZoomList<>nil) and (Fractals.ZoomList.Next<>nil);
     end;
end;

function  TFractalsMenu.GetMenuTexts(Index:Integer):String;
begin
  Result:=FractalsMenuText[Index];
end;

function  TFractalsMenu.GetMenuTextsCount:Integer;
begin
  Result:=Succ(High(FractalsMenuText)-Low(FractalsMenuText));
end;

procedure TFireworksMenu.MakeButtons;
var X,Y,W:Integer; Size:TSize; R:TRect;
begin
  Inherited;

  if BitMap<>nil then begin
     X:=DestRect.Left; Y:=DestRect.Top+TextSize.cy+6;
     W:=RectWidth(DestRect);

     Size:=BitMap.Canvas.TextExtent(MenuTexts[1]);
     R:=Classes.Rect(X+((W-Size.cx) div 2),Y,X+((W-Size.cx) div 2)+Size.cx,Y+TextSize.cy);
     Insert(MenuTexts[1],R,MainForm.BtnFworksSettings,nil,True);
     end;
end;

function  TFireworksMenu.GetMenuTexts(Index:Integer):String;
begin
  Result:=FireworksMenuText[Index];
end;

function  TFireworksMenu.GetMenuTextsCount:Integer;
begin
  Result:=Succ(High(FireworksMenuText)-Low(FireworksMenuText));
end;

function  TStatusBarMenu.MakeButtonPicture(Index__:Integer; State__:TButtonState):Boolean;
var i,j,H,W:Integer; TextSize:TSize; R,R1:TRect;
begin // precondition: all buttons are made before any of them are shown
  Result:=False;
  if (Index__>=0) and (Index__<Count) then
     with MenuItems[Index__] do begin
       Pict[State__].Free; Pict[State__]:=nil;

       try    Pict[State__]:=TPict.Create;
              with Pict[State__] do begin
                W:=RectWidth(Rect); H:=RectHeight(Rect);

                BitMap.Width:=W; BitMap.Height:=H; BitMap.Canvas.CopyMode:=cmSrcCopy;

                with BitMap.Canvas do begin
                   Font.Name:=MainForm.Status.StatusPanel.Font.Name;
                   Font.Size:=MainForm.Status.SubTextsFontSize;
                   end;

                if W<=1 then with MainForm.Status do begin
                   if Glyph<>nil then with Panels[spHint].Rect do begin
                      Dec(PriorButtonLeftPos,(Glyph.Width div 4)+STATUS_BUTTON_SEPARATOR_PIXELS);
                      Rect:=Classes.Rect(PriorButtonLeftPos,
                                         Max(Top,Bottom-Glyph.Height-2), //Max(Top,(Bottom-Top-(Glyph.Height-2)) div 2),
                                         PriorButtonLeftPos+(Glyph.Width div 4),
                                         Bottom-1);
                      end
                   else begin
                      TextSize:=BitMap.Canvas.TextExtent(SpeedButton.Caption+'m');
                      j:=PriorButtonLeftPos-TextSize.cx-STATUS_BUTTON_SEPARATOR_PIXELS;
                      if SpeedButton=MainForm.BtnStatusMenuStop then
                         for i:=0 to Pred(Count) do with MenuItems[i] do
                             if SpeedButton=MainForm.BtnStatusMenuReplay then
                                j:=Rect.Right-TextSize.cx;
//                    if SpeedButton=MainForm.BtnStatusMenuDeadlocks then begin
//                       end;

                      with Panels[spHint].Rect do begin
                        Rect:=Classes.Rect(j,Max(Succ(Top), Bottom-TextSize.cy-9),j+TextSize.cx,Bottom); // '-9': increase the mouse sensitive area a little above the text
                        end;
                      end;
                   PriorButtonLeftPos:=Min(Rect.Left,PriorButtonLeftPos);
                   end;

                W:=RectWidth(Rect); H:=RectHeight(Rect);
                R:=Classes.Rect(0,0,W,H);
                BitMap.Width:=W; BitMap.Height:=H;

                BitMap.Canvas.CopyRect(R,MainForm.Status.BitMap.Canvas,Rect);

                OrgBitMap.Free; OrgBitMap:=nil; // quick and dirty: 'OrgBitMap' is not used

                if MakeStaticWorkBitMap(W,H,False) then with StaticWorkBitMap.Canvas do begin

                   CopyMode:=cmSrcCopy; CopyRect(R,BitMap.Canvas,R);

                   if      Glyph<>nil then begin
                           if   State__=bsFocusedEnabled then
                                ButtonFontColor[State__]:=MainForm.Status.StatusPanel.Font.Color
                           else ButtonFontColor[State__]:=MainForm.Status.PanelColor;
                           TextShadowColor     [State__]:=MainForm.Status.TextShadowColor;
                           DrawGlyph(Glyph,State__,0,Max(0,(H+Ord(Odd(H))-(Glyph.Height-2)) div 2),StaticWorkBitMap);
                           end
                   else if ((State__=bsEnabled) or (State__=bsFocusedEnabled)) then begin

                           R1:=Classes.Rect(0,0,W-1,H-1);
                           Font.Name:=MainForm.Status.StatusPanel.Font.Name;
                           Font.Size:=MainForm.Status.SubTextsFontSize;

                           if   MainForm.Status.TextShadow then begin
                                R1:=RectPlusOffset(R1,1,1);
                                Font.Color:=MainForm.Status.TextShadowColor;
                                Windows.SetBkMode (Handle, Windows.TRANSPARENT);
                                Windows.DrawTextEx(Handle,PChar(SpeedButton.Caption),Length(SpeedButton.Caption),R1,DT_CENTER or DT_BOTTOM or DT_SINGLELINE,nil);
                                R1:=RectPlusOffset(R1,-1,-1);
                                end;

                           if   State__=bsFocusedEnabled then
                                Font.Color:=MainForm.Status.StatusPanel.Font.Color
                           else Font.Color:=MainForm.Status.PanelColor;
                           Windows.SetBkMode (Handle, Windows.TRANSPARENT);
                           Windows.DrawTextEx(Handle,PChar(SpeedButton.Caption),Length(SpeedButton.Caption),R1,DT_CENTER or DT_BOTTOM or DT_SINGLELINE,nil);
                           end;

                   if   (State__=bsFocusedEnabled) then
                        BitMap.Canvas.CopyRect(R,StaticWorkBitMap.Canvas,R)
                   else BitMapAlphaBlendRect(BitMap,BitMap,StaticWorkBitMap,R,R,R,Min(80,100-TransparencyPct[State__]),False,clBlack,0);
                   end;
                end;

       except on E:Exception do begin
                 Pict[State__].Free; Pict[State__]:=nil;
                 Result:=Error(E.Message,Application.Title);
               end;
       end;
     end;
end;

procedure TStatusBarMenu.HideButton(Index:Integer);
begin // precondition: 'Disabled' picture for the button is 'blank', i.e., it's identical to the background
  if (Index>=0) and (Index<Count) then with MenuItems[Index] do
     if Visible then begin
        ShowButton(Index,bsDisabled,False); Visible:=False;
        if ItemIndex=Index then ItemIndex:=-1;
        end;
end;

procedure TStatusBarMenu.MakeButtons;
var i:Integer; R:TRect; b:TButtonState;
begin
  Inherited;

  PriorButtonLeftPos:=MainForm.Status.Panels[spHint].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS-
                      MainForm.Status.RectForm.Left;

  with MainForm.Status do begin
    SetTransparency(True,False,ButtonTransparencyPct,ButtonTransparencyPct,0,ButtonTransparencyPct,PanelTransparencyPct);
    end;

  if MainForm.Status.BitMap<>nil then with MainForm.Status do begin
    Menu.Canvas:=BitMap.Canvas; // caution: status menu uses 'Status.BitMap.Canvas', not 'MainForm.Canvas' or 'MainForm.Image1.Canvas'
    R:=Panels[spHint].Rect;

    if MainForm.Game.TimingEnabled then
       Insert('',Rect(R.Right,R.Top-1,R.Right,R.Bottom),MainForm.BtnStatusMenuTimer    ,MainForm.TimerImage.Picture.BitMap,False);
    Insert(   '',Rect(R.Right,R.Top-2,R.Right,R.Bottom),MainForm.BtnStatusMenuReplay   ,nil,False); // Rect: only a correctly sorted placeholder for the time being; 'MakeButtonPicture' makes the real 'Rect'
    Insert(   '',Rect(R.Right,R.Top-3,R.Right,R.Bottom),MainForm.BtnStatusMenuStop     ,nil,False); //
    Insert(   '',Rect(R.Right,R.Top-4,R.Right,R.Bottom),MainForm.BtnStatusMenuBrowse   ,nil,False);
    Insert(   '',Rect(R.Right,R.Top-5,R.Right,R.Bottom),MainForm.BtnStatusMenuSolver   ,nil,False);
    Insert(   '',Rect(R.Right,R.Top-6,R.Right,R.Bottom),MainForm.BtnStatusMenuOptimizer,nil,False);
    Insert(   '',Rect(R.Right,R.Top-7,R.Right,R.Bottom),MainForm.BtnStatusMenuGenerator,nil,False);
    if MainForm.Game.DeadlockDetection.Enabled then
       Insert('',//MainForm.BtnStatusMenuDeadlocks.Caption,
                 Rect(R.Right,R.Top-7,R.Right,R.Bottom),MainForm.BtnStatusMenuDeadlocks,nil,False);
    BtnBrowseIndex   := 0; // '0' instead of '-1', because the rest of the program doesn't perform 'paranoia-checks' for these variables
    BtnGenerateIndex := 0;
    BtnOptimizeIndex := 0;
    BtnReplayIndex   := 0;
    BtnSolveIndex    := 0;
    BtnStopIndex     := 0;
    BtnTimerIndex    :=-1;
    BtnDeadlocksIndex:=-1;
    for i:=0 to Pred(Count) do with MenuItems[i] do begin
        if      SpeedButton=MainForm.BtnStatusMenuBrowse    then BtnBrowseIndex    :=i
        else if SpeedButton=MainForm.BtnStatusMenuGenerator then BtnGenerateIndex  :=i
        else if SpeedButton=MainForm.BtnStatusMenuOptimizer then BtnOptimizeIndex  :=i
        else if SpeedButton=MainForm.BtnStatusMenuReplay    then BtnReplayIndex    :=i
        else if SpeedButton=MainForm.BtnStatusMenuSolver    then BtnSolveIndex     :=i
        else if SpeedButton=MainForm.BtnStatusMenuStop      then BtnStopIndex      :=i
        else if SpeedButton=MainForm.BtnStatusMenuTimer     then BtnTimerIndex     :=i
        else if SpeedButton=MainForm.BtnStatusMenuDeadlocks then BtnDeadlocksIndex :=i;
        Visible:=False;
        end;

    // all button pictures must be created before displaying any of them
    // because the buttons updates 'Status.BitMap' directly
    for i:=Pred(Count) downto 0 do // backwards because buttons are right-justified
        for b:=Low(b) to High(b) do MakeButtonPicture(i,b);

    end;
end;

end.

