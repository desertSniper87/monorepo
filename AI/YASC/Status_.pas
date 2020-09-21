unit Status_;

interface

uses Windows,Graphics,ExtCtrls,IniFile_,BitMap_,Pict_,Menu_;

const
  DEFAULT_NON_ZERO_STATUS_TOP_MARGIN             = 16;
  DEFAULT_STATUS_PANEL_FONT_SIZE                 : array[Boolean] of Byte = (8,9); // for low-resolution and high-resolution screen respectively
  DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_BLUE   = TColor($EA7A63); // TColor($C08080); // BGR, not RGB;
  DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_GREEN  = TColor($028F49); // BGR, not RGB;
  DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_ORANGE = TColor($8080C0); // TColor($045ADC); // BGR, not RGB;
  DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_RED    = TColor($0006A5); // BGR, not RGB;
  DEFAULT_STATUS_PANEL_SUBTEXT_FONT_SIZE         : array[Boolean] of Byte = (8,9); // for low-resolution and high-resolution screen respectively
  DEFAULT_STATUS_TRANSPARENCY_PCT                = 30;
  MAX_STATUS_PANEL_SUBTEXT_FONT_SIZE             = 10;
  MIN_STATUS_PANEL_SUBTEXT_FONT_SIZE             = 6;
  STATUS_BUTTON_SEPARATOR_PIXELS                 = 8;

type
  TStatusPanelTypes = (spMoves,spPushes,spModified,spHint,spReplaySpeed,spBrowse);

  TStatusPanel = record
    BitMap:TBitMap;
    ProgressBarFocused:Boolean;
    ProgressBarLocked:Boolean;
    ProgressBarLockOnMouseUp:Boolean;
    Rect:TRect;          // rectangle on screen, local status bar coordinates
    RectForm:TRect;      // rectangle on screen, screen coordinates
    RectFormProgressBar:TRect;
    RectProgressBar:TRect;
  end;

type
  TStatus = class(TPict)
  private
    Canvas:TCanvas;
    fBrowsePosition:Integer;
    fHintCount:Integer;
//  fHintText:String;
    fReplayMovesPerSec:Integer;
    fReverseMode:Boolean;
    LeftMargin:Integer;
    procedure ClearPanels(DestroyBitMaps:Boolean);
    function  ClipRect(const R:TRect):TRect;
    function  LoadPicture: Boolean;
    function  MakePicture: Boolean;
    procedure MakePanel(Index:TStatusPanelTypes; Left,Right:Integer; const Text:String; RectProgressBar__:TRect; PanelBitMap:TBitMap);
    function  MakeProgressBar(Index:TStatusPanelTypes; const Caption:String):Boolean;

    procedure SetHint     (const s:String);
    procedure SetModified (const s:String);
    procedure SetMoveCount(const s:String);
    procedure SetPushCount(const s:String);

    procedure ClearHint;
    procedure Write(const s:String; const R:TRect; Flags:Cardinal);
  protected
    procedure SetBrowsePosition(BrowsePosition__:Integer);
    procedure SetHintCount(HintCount__:Integer);
    procedure SetReplayMovesPerSec(MovesPerSec:Integer);
    procedure SetReverseMode(ReverseMode__:Boolean);
  public
    BodySlice:Integer;
    BottomMargin:Integer;
    ButtonTransparencyPct:Integer;
    DefaultTileRect:TRect;
    EdgeSmoothing:Boolean;
    EdgeTransparencyPct:Integer;
    LastMenuItemIndex:Integer;
    LastMousePos:TPoint;
    LeftSlice:Integer;
    Menu:TStatusBarMenu;
    Panels:array[TStatusPanelTypes] of TStatusPanel;
    PushOrPullBitMap:array[Boolean] of TBitMap;
    RightSlice:Integer;
    PanelBoundsBottom:Integer;
    PanelBoundsTop:Integer;
    PanelColor:TColor;
    PanelRoundRect:Integer;
    PanelTransparencyPct:Integer;
    RectForm:TRect;
    StatusPanel:TPanel;
    SubTextsAreTransparent:Boolean;
    SubTextsFontSize:Integer;
    SubTextsFontColor:TColor;
    TopMargin:Integer;
    TextShadow:Boolean;
    TextShadowColor:TColor;
    TileRect:TRect;
    Transparency:Boolean;
    TransparencyPct:Integer;
    Visible:Boolean;

    property  Hint     :String write SetHint;
    property  Modified :String write SetModified;
    property  PushCount:String write SetPushCount;
    property  MoveCount:String write SetMoveCount;

    constructor Create(Canvas__:TCanvas; StatusPanel__:TPanel);
    destructor  Destroy; override;

    procedure BrowseMouseMove(X,Y:Integer);
    procedure EnableDisableButtons;
    function  EnterBrowseMode:Boolean;
    procedure EnterReplayMode;
    function  Click:Integer;
    procedure LeaveReplayMode;
    procedure LeaveBrowseMode;
    procedure Invalidate;
    procedure Paint;
    function  Resize(Width__,Height__:Integer):Boolean; override;

    function  LoadSettingsFromIniFile(const IniFile: TIniFile;
      const Section: String): Boolean;
    function  MakeStatusBar: Boolean;
    function  MouseMove(X,Y:Integer):Integer;
    procedure ReplaySpeedMouseMove(X,Y:Integer);
    function  SaveSettingsToIniFile(const IniFile: TIniFile;
      const Section: String): Boolean;
    procedure SetDefaultValues;
    procedure ShowHintForReplayMovesPerSec;
    procedure StartTimer;
    function  StatusRectToImageRect(const R:TRect):TRect;
    procedure StopTimer;

    property  BrowsePosition:Integer read fBrowsePosition write SetBrowsePosition;
    property  HintCount:Integer read fHintCount write SetHintCount;
//  property  HintText:String read fHintText write SetHint;
    property  ReplayMovesPerSec:Integer read fReplayMovesPerSec write SetReplayMovesPerSec;
    property  ReverseMode:Boolean read fReverseMode write SetReverseMode;
  end;

implementation

uses SysUtils,Classes,Forms,Controls,
     Text_,Misc_,Pack_,SokUtil_,SokFile_,Sound_,Snapshots_,Main_,Game_,MPlayer2_,Dead_,GView_;

constructor TStatus.Create(Canvas__:TCanvas; StatusPanel__:TPanel);
begin
  Menu             :=TStatusBarMenu.Create(Canvas__,MainForm.StatusMenuPanel,8); // caution: Menu is later changed to use 'Status.BitMap.Canvas' instead of 'Canvas__'
  Canvas           :=Canvas__;
  StatusPanel      :=StatusPanel__;
  Visible          :=False;
  fReverseMode     :=False;
  FillChar(Panels,SizeOf(Panels),0); ClearPanels(True);
  FillChar(PushOrPullBitMap,SizeOf(PushOrPullBitMap),0);
  BitMapCreate(PushOrPullBitMap[False],1,1);
  BitMapCreate(PushOrPullBitMap[True ],1,1);
  RectForm         :=Rect(0,0,0,0);
  Clear;
  MakeBitMap;
  SetDefaultValues;
  fBrowsePosition  :=-1; fHintCount:=0; //fHintText:=NL;
  LastMenuItemIndex:=-1; LastMousePos.X:=-1; LastMousePos.Y:=-1;
end;

destructor TStatus.Destroy;
begin
  Menu.Free;
  ClearPanels(True);
  PushOrPullBitMap[False].Free; PushOrPullBitMap[True].Free;
  Inherited Destroy;
end;

function  TStatus.LoadSettingsFromIniFile(const IniFile:TIniFile; const Section:String):Boolean;
begin
//  Result:=True; exit;
  FileName              :=              KeepDataPathUpToDate(IniFile.ReadString (Section,'FileName',FileName));
  TileRect.Left         :=              IniFile.ReadInteger(Section,'Tile.Left',TileRect.Left);
  TileRect.Top          :=              IniFile.ReadInteger(Section,'Tile.Top',TileRect.Top);
  TileRect.Right        :=              Max(
                                        IniFile.ReadInteger(Section,'Tile.Right',TileRect.Right),
                                        TileRect.Left);
  TileRect.Bottom       :=              Max(
                                        IniFile.ReadInteger(Section,'Tile.Bottom',TileRect.Bottom),
                                        TileRect.Top);
  MaskBitMapColor       :=ColorToRGB   (IniFile.ReadInteger(Section,'BackgroundColor',Integer(RGBToColor(MaskBitMapColor))));
  MaskBitMapPct         :=              IniFile.ReadInteger(Section,'BackgroundColorTolerance',MaskBitMapPct);
  Masked                :=              IniFile.ReadBool   (Section,'Masked',Masked);
  Transparency          :=              IniFile.ReadBool   (Section,'Transparency',Transparency);
  EdgeSmoothing         :=              IniFile.ReadBool   (Section,'EdgeSmoothing',EdgeSmoothing);
  TransparencyPct       :=Max(0,Min(100,IniFile.ReadInteger(Section,'TransparencyPct',TransparencyPct)));
  EdgeTransparencyPct   :=Max(0,Min(100,IniFile.ReadInteger(Section,'EdgeTransparencyPct',EdgeTransparencyPct)));
  LeftSlice             :=Max(0,        IniFile.ReadInteger(Section,'LeftSlice',LeftSlice));
  BodySlice             :=Max(0,        IniFile.ReadInteger(Section,'BodySlice',BodySlice));
  RightSlice            :=Max(0,        IniFile.ReadInteger(Section,'RightSlice',RightSlice));
  ButtonTransparencyPct :=Max(0,Min(100,IniFile.ReadInteger(Section,'ButtonTransparencyPct',ButtonTransparencyPct)));
  PanelBoundsTop        :=Max(0,        IniFile.ReadInteger(Section,'PanelBoundsTop',PanelBoundsTop));
  PanelBoundsBottom     :=              IniFile.ReadInteger(Section,'PanelBoundsBottom',PanelBoundsBottom);
  if PanelBoundsBottom<0 then
     PanelBoundsBottom:=Max(PanelBoundsTop+2,RectHeight(TileRect)+PanelBoundsBottom);
  PanelColor            :=NotBlackColor(TColor(IniFile.ReadInteger(Section,'PanelColor',Integer(PanelColor))));
  PanelRoundRect        :=Max(0,Min(100,IniFile.ReadInteger(Section,'PanelRoundRect',PanelRoundRect)));
  SubTextsAreTransparent:=              IniFile.ReadBool   (Section,'SubTextsAreTransparent',SubTextsAreTransparent);
  SubTextsFontColor     :=TColor       (IniFile.ReadInteger(Section,'SubTextsFontColor',Integer(SubTextsFontColor)));
  SubTextsFontSize      :=Max(MIN_STATUS_PANEL_SUBTEXT_FONT_SIZE,
                              Min(MAX_STATUS_PANEL_SUBTEXT_FONT_SIZE,
                                        IniFile.ReadInteger(Section,'SubTextsFontSize',SubTextsFontSize)));
  PanelTransparencyPct  :=Max(0,Min(100,IniFile.ReadInteger(Section,'PanelTransparencyPct',PanelTransparencyPct)));
  TextShadow            :=              IniFile.ReadBool   (Section,'TextShadow',TextShadow);
  TextShadowColor       :=TColor       (IniFile.ReadInteger(Section,'TextShadowColor',Integer(TextShadowColor)));
  Result:=LoadFontFromIniFile(IniFile,Section,'',StatusPanel.Font);
  MainForm.Image1.Tag:=0; // 'Tag' used as flag for current font assigned to the canvas
end;

function  TStatus.ClipRect(const R:TRect):TRect;
begin
  Result:=R;
  with Result do begin
    Left   :=Max(Left,10);
    Top    :=Max(Top,2);
    Right  :=Min(Right,RectWidth(RectForm)-Max(10,Abs(RightSlice)));
    Bottom :=Min(Bottom,RectHeight(RectForm));
    Left   :=Min(Left,Right);
    Top    :=Min(Top,Bottom);
    end;
end;

function  TStatus.Resize(Width__,Height__:Integer):Boolean;
begin
  Result:=MakePicture;
  Paint;
end;

function  TStatus.SaveSettingsToIniFile(const IniFile:TIniFile; const Section:String):Boolean;
begin
//Result:=True; exit;
  IniFile.WriteString (Section,'FileName',FileName);
  IniFile.WriteInteger(Section,'Tile.Left',TileRect.Left);
  IniFile.WriteInteger(Section,'Tile.Top',TileRect.Top);
  IniFile.WriteInteger(Section,'Tile.Right',TileRect.Right);
  IniFile.WriteInteger(Section,'Tile.Bottom',TileRect.Bottom);
  IniFile.WriteInteger(Section,'BackgroundColor',Integer(RGBToColor(MaskBitMapColor)));
  IniFile.WriteInteger(Section,'BackgroundColorTolerance',MaskBitMapPct);
  IniFile.WriteBool   (Section,'Masked',Masked);
  IniFile.WriteBool   (Section,'Transparency',Transparency);
  IniFile.WriteBool   (Section,'EdgeSmoothing',EdgeSmoothing);
  IniFile.WriteInteger(Section,'TransparencyPct',TransparencyPct);
  IniFile.WriteInteger(Section,'EdgeTransparencyPct',EdgeTransparencyPct);
  IniFile.WriteInteger(Section,'LeftSlice',LeftSlice);
  IniFile.WriteInteger(Section,'BodySlice',BodySlice);
  IniFile.WriteInteger(Section,'RightSlice',RightSlice);
  IniFile.WriteInteger(Section,'ButtonTransparencyPct',ButtonTransparencyPct);
  IniFile.WriteInteger(Section,'PanelBoundsTop',PanelBoundsTop);
  IniFile.WriteInteger(Section,'PanelBoundsBottom',PanelBoundsBottom);
  IniFile.WriteInteger(Section,'PanelColor',Integer(PanelColor));
  IniFile.WriteInteger(Section,'PanelRoundRect',PanelRoundRect);
  IniFile.WriteBool   (Section,'SubTextsAreTransparent',SubTextsAreTransparent);
  IniFile.WriteInteger(Section,'SubTextsFontColor',Integer(SubTextsFontColor));
  IniFile.WriteInteger(Section,'SubTextsFontSize',SubTextsFontSize);
  IniFile.WriteInteger(Section,'PanelTransparencyPct',PanelTransparencyPct);
  IniFile.WriteBool   (Section,'TextShadow',TextShadow);
  IniFile.WriteInteger(Section,'TextShadowColor',Integer(TextShadowColor));
  Result:=SaveFontToIniFile(IniFile,Section,'',StatusPanel.Font);
end;

procedure TStatus.SetDefaultValues;
begin
  Visible:=True;
  with StatusPanel do begin
    Font.Name:='Arial'; Font.Size:=DEFAULT_STATUS_PANEL_FONT_SIZE[IsAHighResolutionScreen];
    Font.Color:=clWhite; Font.Style:=[fsBold];
    end;

  if (MainForm<>nil) and (MainForm.Menu<>nil) then begin
     LeftMargin  :=MainForm.Menu.ButtonDX;
     BottomMargin:=4; //MainForm.Menu.ButtonDY;
     end
  else begin
     LeftMargin  :=4;
     BottomMargin:=4;
     end;
  FileName                     :=DEFAULT_VALUE;
  TileRect                     :=GridCellToRect(
                                   DEFAULT_BUTTON_TILE_NO[bsEnabled,0],
                                   DEFAULT_BUTTON_TILE_NO[bsEnabled,1],
                                   DEFAULT_BUTTON_TILE_WIDTH,
                                   DEFAULT_BUTTON_TILE_HEIGHT,
                                   DEFAULT_TILE_GRIDLINE_WIDTH);
  DefaultTileRect              :=TileRect;
  Masked                       :=True;
  MaskBitMapColor              :=RGB_BLACK;
  MaskBitMapPct                :=0;
  Transparency                 :=True;
  TransparencyPct              :=DEFAULT_STATUS_TRANSPARENCY_PCT;
  EdgeTransparencyPct          :=25;
  EdgeSmoothing                :=True;
  ButtonTransparencyPct        :=40;
  LeftSlice                    :=15;
  BodySlice                    :=116-LeftSlice;
  RightSlice                   :=0;
  PanelBoundsTop               :=2;
  PanelBoundsBottom            :=0;
  PanelColor                   :=clWhite;
  PanelTransparencyPct         :=60;
  PanelRoundRect               :=25;
  SubTextsAreTransparent       :=False;
  SubTextsFontColor            :=DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_BLUE;
  SubTextsFontSize             :=DEFAULT_STATUS_PANEL_SUBTEXT_FONT_SIZE[IsAHighResolutionScreen];
  TextShadow                   :=True;
  TextShadowColor              :=DEFAULT_SHADOW_COLOR_ENABLED; //clDkGray;
  TopMargin                    :=0;
end;

procedure TStatus.Paint;
begin
  if Visible then with RectForm do Draw(Left,Top,Canvas);
end;

procedure TStatus.SetHint(const s:String);

  procedure Write(const s:String);
  begin
    if True {(s<>fHintText)} then begin // 'True': always update the screen; there is an complex mixture of updating the hint text and the statusbar buttons, e.g., 'Browse' and 'Replay'
       //fHintText:=s;
       Self.Write(s,Panels[spHint].Rect,DT_LEFT);
       if (s<>'') and (HintCount<High(HintCount)) then Inc(fHintCount);
       end;
  end;

begin // SetHint
//MainForm.StatusBar1.Panels[3].Text:=Hint__;
  if   (s='') and
       (MainForm.Game<>nil) then with MainForm do
       if   Game.IsReplaying and
            (RectHeight(Panels[spReplaySpeed].RectForm)>0) then
            Write(HintToggleReplayDirectionText)
       else if   (RotateAndFlipPopupMenu<>nil) and
                 RotateAndFlipPopupMenu.Visible and
                 (RotateAndflipPopupMenu.FocusedControl<>nil) then
                 Write(GetLongHint(RotateAndflipPopupMenu.FocusedControl.Hint))
            else if   (GameViewer=nil) or (GameViewer.LegalMovesInfo.Mask=0) then
                      Write(s)
                 else if                  GameViewer.LegalMovesInfo.Mask=SQUARE_SET then
                                          if   (not (Game.GameState=gsSolved)) then
                                               Write(HiglightedBoxesCanMoveText)
                                          else Write(s)
                      else if             GameViewer.LegalMovesInfo.Mask=BOX_SET_TO_SQUARE then
                                          Write(HiglightedBoxesCanMoveToTheSelectedSquareText)
                           else if        GameViewer.LegalMovesInfo.Mask=BOX_SET_DEADLOCK then
                                     if   Game.History.Count>0 then
                                          Write(DeadlockedPositionText+SPACE+PleaseBacktrackText)
                                     else Write(DeadlockedPositionText)
                                else      Write(s)
  else if   (s='') or (not Assigned(MainForm.MultiView)) or (not MainForm.MultiView.IsSizing) then
            Write(s)
end;

procedure TStatus.SetHintCount(HintCount__:Integer);
begin
  fHintCount:=HintCount__;
end;

procedure TStatus.SetModified(const s:String);
begin
  //MainForm.StatusBar1.Panels[2].Text:=OkChangedText[True]
  if   (s<>'') and
       (Panels[spModified].Rect.Left=Panels[spHint].Rect.Left) and
       (Panels[spHint].Rect.Left<>0) then with Panels[spHint].Rect do begin
       Hint:='';
       Panels[spHint].Rect:=ClipRect(Rect(STATUS_BUTTON_SEPARATOR_PIXELS+Panels[spModified].Rect.Right,Top,Right,Bottom));
       if Left=Panels[spModified].Rect.Left then begin
          Left:=0; Right:=0;
          end;
       Panels[spHint].RectForm:=StatusRectToImageRect(Panels[spHint].Rect);
       MakePanel(spModified,Panels[spModified].Rect.Left,Panels[spModified].Rect.Right,'',Rect(0,0,0,0),nil);
       Paint;
       end;
  if   (RectWidth(Panels[spReplaySpeed].RectForm)=0) and
       (RectWidth(Panels[spBrowse     ].RectForm)=0) then
       Write(s,Panels[spModified].Rect,DT_CENTER);
end;

procedure TStatus.SetPushCount(const s:String);
begin
//MainForm.StatusBar1.Panels[1].Text:=IntToStr(PushCount__);
  Write(s,Panels[spPushes].Rect,DT_CENTER);
end;

procedure TStatus.SetMoveCount(const s:String);
begin
//MainForm.StatusBar1.Panels[0].Text:=IntToStr(MoveCount__);
  Write(s,Panels[spMoves].Rect,DT_CENTER);
end;

procedure TStatus.ClearHint;
var oMask:Integer; oIsReplaying,oVisible:Boolean;
begin // ensure that the status bar really gets cleared, no matter how the game state is
  with MainForm do
    if (GameViewer<>nil) and (RotateAndFlipPopupMenu<>nil) and (Game<>nil) then
       with GameViewer do begin
         oMask:=LegalMovesInfo.Mask;
         oVisible:=RotateAndFlipPopupMenu.Visible;
         oIsReplaying:=Game.IsReplaying;
         try     Game.IsReplaying:=False;
                 if oVisible then RotateAndFlipPopupMenu.Hide;
                 LegalMovesInfo.Mask:=0;
                 //fHintText:=NL; // ensure that setting the hint to '' changes the hint text and triggers a screen update
                 Self.Hint:='';
         finally Game.IsReplaying:=oIsReplaying;
                 if oVisible then RotateAndFlipPopupMenu.Show;
                 LegalMovesInfo.Mask:=oMask;
         end;
         end
    else Self.Hint:='';
end;

procedure TStatus.Write(const s:String; const R:TRect; Flags:Cardinal);
var R1:TRect;
begin
  if Visible then with RectForm do begin
     R1:=R; Inc(R1.Left,Left); Inc(R1.Top,Top); Inc(R1.Right,Left); Inc(R1.Bottom,Top);
     Canvas.CopyMode:=cmSrcCopy;
     Canvas.CopyRect(R1,BitMap.Canvas,R);

     if MainForm.Image1.Tag<>LongInt(Addr(StatusPanel)) then begin
        MainForm.Image1.Tag:=LongInt(Addr(StatusPanel));
        Canvas.Font.Assign(StatusPanel.Font);
        end;
     Windows.SetBkMode (Canvas.Handle, Windows.TRANSPARENT);

     if R.Top>=1 then begin          // it looks better with the text slightly above the center
        Dec(R1.Top); Dec(R1.Bottom); // and it gives a little more space to panel captions
        end;

     if TextShadow then begin
        Canvas.Font.Color:=TextShadowColor;
        Inc(R1.Left); Inc(R1.Top);
        Windows.DrawTextEx(Canvas.Handle,PChar(s),Length(s),R1,Flags or DT_VCENTER or DT_SINGLELINE,nil);
        Dec(R1.Left); Dec(R1.Top);
        Dec(R1.Right); Dec(R1.Bottom); // decrease bounds for the main text
        Canvas.Font.Color:=StatusPanel.Font.Color;
        end;

     Windows.DrawTextEx(Canvas.Handle,PChar(s),Length(s),R1,Flags or DT_VCENTER or DT_SINGLELINE,nil);
     Windows.SetBkMode (Canvas.Handle, Windows.OPAQUE);

     // left-justified replay speed slider:
     // do nothing;
     // right-justified replay speed slider:
     if Panels[spReplaySpeed].RectForm.Left<Panels[spReplaySpeed].RectForm.Right then begin
        R1:=Classes.Rect(Panels[spReplaySpeed].Rect.Right,0,RectWidth(Self.RectForm),RectHeight(Self.RectForm));
        Canvas.CopyMode:=cmSrcCopy;
        Canvas.CopyRect(StatusRectToImageRect(R1),BitMap.Canvas,R1);
        end;
     end;
end;

procedure TStatus.Invalidate;
begin
  Resize(0,0);
end;

function  TStatus.LoadPicture:Boolean;
var SourceBitMap:TBitMap;
begin
  if (FileName<>DEFAULT_VALUE) and
     FileExists(FileName) and
     LoadFromFile(FileName) and
     ((Pict=nil) or              // simple bitmap
     MakeOrgBitMapFromPict) then // image is able to draw itself
     SourceBitMap:=OrgBitMap
  else begin
     SourceBitMap:=MainForm.DefaultButtonSetImage.Picture.BitMap;
     if FileName<>DEFAULT_VALUE then TileRect:=DefaultTileRect;
     end;
  Result:=(SourceBitMap<>nil) and LoadFromBitMapRect(SourceBitMap,TileRect,Point(BUTTON_OFFS_X,BUTTON_OFFS_Y),RGBToColor(MaskBitMapColor));
  if Result and MakeOrgBitMapFromPict then
     try OrgBitMap.Width :=BitMap.Width;
         OrgBitMap.Height:=BitMap.Height;
         OrgBitMap.Canvas.CopyRect(BitMapRect,BitMap.Canvas,BitMapRect);
     except
       on E:Exception do begin
          Error(E.Message,Application.Title); Clear; ClearPanels(False); Result:=False;
          end;
     end
  else Result:=False;
  if not Result then MakeBlank(2*BUTTON_OFFS_X+RectWidth(TileRect),2*BUTTON_OFFS_Y+RectHeight(TileRect),clNavy);
  //fHintText:=NL;
end;

function  TStatus.MakeStatusBar:Boolean;
begin
  OrgBitMap.Free; OrgBitMap:=nil; MakeBitMap;
  Result:=LoadPicture; Visible:=Result;
end;

function TStatus.MakePicture:Boolean;
var i,j,k,m,n,H,W,LS,RS,Count:Integer; bs:TButtonState;
    R1,R2:TRect; p:PRGBVector; B:TBitMap;
begin
  ClearPanels(Assigned(MainForm.GameViewer)
              and
              Assigned(MainForm.GameViewer.Pictures[ptScreenBackground])
              and
              (MainForm.GameViewer.Pictures[ptScreenBackground].View=ivFloorTile));
  Menu.Clear;
  TopMargin:=0;
  Result:=(BitMap<>nil) and (OrgBitMap<>nil);
  if Result then with RectForm do begin
     W:=MainForm.Image1.ClientWidth-2*LeftMargin;
     H:=OrgBitMap.Height;

     Left:=LeftMargin;
     Top :=MainForm.Image1.ClientHeight-H-BottomMargin;
     if (MainForm.Image1.ClientHeight >= 2 * MIN_MAIN_WINDOW_HEIGHT)
        and
        (MainForm.Image1.ClientWidth  >= HIGH_RESOLUTION_SCREEN_WIDTH_THRESHOLD)
        then
        // when the window is large, then enlarge the height of the mouse
        // sensitive area a little for the history slider and the replay speed
        // slider
        TopMargin:=DEFAULT_NON_ZERO_STATUS_TOP_MARGIN;

     if (MainForm.Menu<>nil) and (MainForm.Menu.Count<>0) then
        with MainForm.Menu do begin
          if   BtnMPlayerIndex=Pred(Count) then i:=Pred(Pred(Count))
          else i:=Pred(Count); // 'i'=number of the last button in the menu-panel to the left
          j:=MenuItems[i].Rect.Bottom+(ButtonDY div 2);
          if j>=RectForm.Top-TopMargin then TopMargin:=0; // '0': drop the enlarged mouse senstive area, if any, is the window height is small
          if j>=RectForm.Top-TopMargin then begin
             Inc(RectForm.Left ,MainForm.Menu.MenuWidth);
             RectForm.Top:=MenuItems[i].Rect.Top;
             Dec(W,MainForm.Menu.MenuWidth);

             if BtnMPlayerIndex=Pred(Count) then with MenuItems[Pred(Count)] do begin
                Dec(W,RectWidth(Rect)+LeftMargin);
                HideButton(Pred(Count));
                for bs:=Low(Pict) to High(Pict) do begin
                    Pict[bs].Free; Pict[bs]:=nil;
                    end;
                Rect:=Classes.Rect(RectForm.Left+W+LeftMargin,RectForm.Top,RectForm.Left+W+LeftMargin+RectWidth(Rect),RectForm.Top+RectHeight(Rect));
                Visible:=True; ShowButton(Pred(Count),State,False);
                end;
             end;
          end;

     if Odd(W) then Dec(W);
     Right:=Left+W; Bottom:=Top+H;

     i:=Min(PanelBoundsTop,H-2);
     if PanelBoundsBottom<=i then j:=H-2
     else j:=PanelBoundsBottom;

     if MainForm.Image1.Tag<>LongInt(Addr(StatusPanel)) then begin
        MainForm.Image1.Tag:=LongInt(Addr(StatusPanel));
        Canvas.Font.Assign(StatusPanel.Font);
        end;
     k:=Max(50,Canvas.TextWidth('99999MX'));
     m:=Canvas.TextWidth(OKChangedText[True]+'MX');
     n:=Max(10,LeftSlice+2);

     Panels[spMoves         ].Rect:=ClipRect(Rect(n,i,n+k,j));
     Panels[spPushes        ].Rect:=ClipRect(Rect(STATUS_BUTTON_SEPARATOR_PIXELS+Panels[spMoves].Rect.Right,i,STATUS_BUTTON_SEPARATOR_PIXELS+Panels[spMoves].Rect.Right+k,j));
     Panels[spModified      ].Rect:=ClipRect(Rect(STATUS_BUTTON_SEPARATOR_PIXELS+Panels[spPushes].Rect.Right,i,STATUS_BUTTON_SEPARATOR_PIXELS+Panels[spPushes].Rect.Right+m,j));
     Panels[spHint          ].Rect:=ClipRect(Rect(Panels[spModified].Rect.Left,i,W-Max(STATUS_BUTTON_SEPARATOR_PIXELS,RightSlice+2),j));
     Panels[spHint          ].RectForm:=StatusRectToImageRect(Panels[spHint].Rect);

     LS              :=LeftSlice +BUTTON_OFFS_X;
     if RightSlice=0 then RS:=0
     else RS         :=RightSlice+BUTTON_OFFS_X;

     if (W>=LS+BodySlice+RS) and
        (W>=OrgBitMap.Width) and
        (W>=RS) then
        try BitMap.Width :=W;
            BitMap.Height:=H;
            BitMap.Canvas.CopyMode:=cmSrcCopy;
            BitMap.Canvas.Brush.Color:=clGreen;
            BitMap.Canvas.FillRect(Rect(0,0,W,H));

            R1:=Rect(0,0,LS+BodySlice,H);
            BitMap.Canvas.CopyRect(R1,OrgBitMap.Canvas,R1); // copy left part of image to bitmap

            if RS>0 then begin // use right part of the image
               BitMap.Canvas.CopyRect(Rect(W-RS,0,W,H),OrgBitMap.Canvas,Rect(OrgBitMap.Width-RS,0,OrgBitMap.Width,OrgBitMap.Height));
               Dec(W,RS); // if W is odd now, the slices will not fit perfectly together at the end
               m:=Max(LS,W-BodySlice); k:=LS;
               end
            else begin
               m:=Max(LS,W-LS-BodySlice); k:=0; // use mirrored left slice as right slice
               end;

            for i:=0 to Pred(H) do begin // mirror left part to right part
                p:=BitMap.ScanLine[i];
                for j:=Pred(W) downto m do p[j]:=p[Pred(W)-j+k];
               end;

            Count:=0;
            R1.Left:=LS; R1.Top:=0; R1.Right:=R1.Left+BodySlice; R1.Bottom:=H;
            while R1.Right<=m do begin // fill from left side
              if   Odd(Count) then R2:=Rect(m ,0,m +BodySlice,H)  // mirrored part
              else                 R2:=Rect(LS,0,LS+BodySlice,H); // original part
              //if Odd(Count) then
              BitMap.Canvas.CopyRect(R1,BitMap.Canvas,R2);
              R1.Left:=R1.Right; Inc(R1.Right,BodySlice); Inc(Count);
              end;

            if m>R1.Left then // fill the gap
               if Odd(Count) then begin // continue with the mirrored image
                 j:=(m-R1.Left) div 2;
                 BitMap.Canvas.CopyRect(Rect(R1.Left,0,R1.Left+j,H),BitMap.Canvas,Rect(m,0,m+j,H));
                 BitMap.Canvas.CopyRect(Rect(R1.Left+j,0,m,H),BitMap.Canvas,Rect(LS+BodySlice-j,0,LS+BodySlice,H));
                 end
               else begin // continue with the original image
                 j:=(m-R1.Left+BodySlice) div 2; // ... +BodySlice: overwrite part of mirrored image, so length of original part and mirrored part are identical
                 BitMap.Canvas.CopyRect(Rect(R1.Left,0,R1.Left+j,H),BitMap.Canvas,Rect(LS,0,LS+j,H));
                 end;

//          BitMap.SaveToFile('t1.bmp');

            if Masked or Transparency or EdgeSmoothing or (BUTTON_OFFS_X+BUTTON_OFFS_Y<>0) then
               MakeMaskBitMap(MaskBitMapColor,MaskBitMapPct,MaskExteriorOnly);

//          if AndBitMap<>nil then AndBitMap.SaveToFile('t2.bmp');

            W:=BitMap.Width; // 'W' was changed, if 'RS'<>0 ('RS': right slice)
            R1:=Rect(0,0,W,H);
            R2:=Rect(RectForm.Left,RectForm.Top,RectForm.Left+W,RectForm.Top+H);
            if Transparency or EdgeSmoothing then begin
               B:=TBitMap.Create; // 'AlphaBlend' requires 24-bit pixelformat; 'Image.Picture.BitMap.PixelFormat' = 'pfDevice'
               try     B.Width:=W; B.Height:=H; B.PixelFormat:=pf24Bit;
                       B.Canvas.CopyRect(R1,Self.Canvas,R2);
                       B.PixelFormat:=pf24Bit;
                       if   Transparency then
                            AlphaBlend(BitMap,B,MaskBitMap,R1,R1,TransparencyPct,EdgeTransparencyPct,False)
                       else AlphaBlend(BitMap,B,MaskBitMap,R1,R1,0,0,False);
                       if EdgeSmoothing then
                          BitMap_.EdgeSmoothing(BitMap,B,MaskBitMap,R1,True,False);
                       MaskBitMap.Free; MaskBitMap:=nil; // quick and dirty: since 'AlphaBlend' must examine each pixel anyway, it fills in masked pixels
               finally B.Free;
               end;
               end;

            with Panels[spMoves        ].Rect do MakePanel (spMoves ,Left,Right,MovesText ,Rect(0,0,0,0),nil);
            with Panels[spPushes       ].Rect do MakePanel (spPushes,Left,Right,PullsText ,Rect(0,0,0,0),PushOrPullBitMap[True]);
            with Panels[spPushes       ].Rect do MakePanel (spPushes,Left,Right,PushesText,Rect(0,0,0,0),PushOrPullBitMap[False]);
            fReverseMode:=not MainForm.Game.ReverseMode;
            ReverseMode:=MainForm.Game.ReverseMode; // set current status, i.e., show 'Pulls' or 'Pushes'

            if Menu.Visible then Menu.MakeButtons;
        except
          on E:Exception do begin
             Visible:=False; Clear; ClearPanels(False); Menu.Clear;
             Result:=Error(E.Message,Application.Title);
             end;
        end;
     end;
end;

procedure TStatus.MakePanel(Index:TStatusPanelTypes; Left,Right:Integer; const Text:String; RectProgressBar__:TRect; PanelBitMap:TBitMap);
const PROGRESS_BAR_AND_CAPTION_SEPARATOR_PIXELS=1;
var {Assigns the panel to 'PanelBitMap' if specified; otherwise the panel draws itself directly on 'Self.BitMap'}
  i,H,H1,W,Top,Bottom:Integer; 
  B:TBitMap; P:TPict; R,R2:TRect;
begin
  P:=nil; B:=nil;
  with Panels[Index] do begin
    Rect:=Classes.Rect(0,0,0,0); RectForm:=Rect;
    RectProgressBar:=Rect; RectFormProgressBar:=Rect;
    end;
  if Right>Left then with Panels[Index] do
     try
       try P:=TPict.Create; B:=TBitMap.Create;
           H:=RectHeight(Self.RectForm);
           Top:=Max(2,PanelBoundsTop);
           if PanelBoundsBottom<=Top then Bottom:=Max(Top+2,H-2)
           else Bottom:=Max(PanelBoundsTop+2,Min(PanelBoundsBottom,H-2));
           W:=Max(1,Right-Left); H:=Max(1,Bottom-Top);
           R:=Classes.Rect(0,0,W,H);

           Rect:=Classes.Rect(Left,Top,Left+W,Top+H);
           RectForm:=StatusRectToImageRect(Rect);

           B.Width:=W; B.Height:=H; B.PixelFormat:=pf24Bit; B.Canvas.CopyMode:=cmSrcCopy;
           B.Canvas.CopyRect(R,Self.BitMap.Canvas,Classes.Rect(Left,Top,Right,Bottom));

           RectProgressBar:=RectProgressBar__;
           i:=RectHeight(RectProgressBar);
           if   (Text<>'') and (i>0) then
                with B.Canvas do with RectProgressBar do begin
                  Font.Name:=StatusPanel.Font.Name;
                  Font.Size:=SubTextsFontSize;
                  H1:=TextHeight(Text);
                  if Bottom>=H-1-H1-PROGRESS_BAR_AND_CAPTION_SEPARATOR_PIXELS then begin
                     Bottom:=H-1-H1-PROGRESS_BAR_AND_CAPTION_SEPARATOR_PIXELS;
                     Top   :=Max(4,Bottom-i);
                     if Top>=Bottom then Bottom:=Min(Top+2,H);
                     end;
                  end;
           with RectProgressBar do
             RectFormProgressBar:=Classes.Rect(Left  +RectForm.Left,
                                               Top   +RectForm.Top,
                                               Right +RectForm.Left,
                                               Bottom+RectForm.Top);

           P.MakeBlank(W,H,clBlack);
           with P.BitMap.Canvas do begin
             Brush.Color:=PanelColor;
             Pen  .Color:=PanelColor;
             RoundRect(2,2,W-2,H-2,PanelRoundRect,PanelRoundRect);

             if RectHeight(RectProgressBar)>0 then begin
                Brush.Color:=clBlack; Brush.Style:=bsSolid;
                FillRect(RectProgressBar);
                Brush.Color:=PanelColor;
                end;
             end; // P.BitMap = rounded rectangle (in user's color) with black background and black progressbar, if any

           P.MakeMaskBitMap(RGB_BLACK,0,False);

           //BitMapDump(P.BitMap); BitMapDump(P.MaskBitMap);

           P.BitMap.Canvas.CopyRect(R,B.Canvas,R); // P.BitMap = Statusbar image
           P.BitMap.Canvas.RoundRect(2,2,W-2,H-2,PanelRoundRect,PanelRoundRect);

           if RectHeight(RectProgressBar)>0 then with P.BitMap.Canvas do with RectProgressBar do begin
              Brush.Color:=RGB(248,248,248);  FrameRect(Classes.Rect(Left -2,Top-2,Right+2,Bottom+2));
              Brush.Color:=clDkGray;          FrameRect(Classes.Rect(Left -2,Top-2,Right+1,Bottom+1));
              Brush.Color:=clLtGray;          FrameRect(Classes.Rect(Left -1,Top-1,Right+1,Bottom+1));
                                            //FrameRect(Classes.Rect(Right+1,Top-2,Right+2,Top     ));
              Brush.Color:=RGB(48,48,64);     FrameRect(Classes.Rect(Left -1,Top-1,Right  ,Bottom  ));
              Brush.Color:=PanelColor;
              Pen.Color  :=PanelColor;
              CopyRect(RectProgressBar,B.Canvas,Classes.Rect(R.Left+Left,R.Top+Top,R.Left+Left+RectWidth(RectProgressBar),R.Top+Top+RecTHeight(RectProgressBar)));
              end;

           BitMap_.AlphaBlend(B,P.BitMap,P.MaskBitMap,R,R,100-PanelTransparencyPct,0,False); // put rounded rectangle on B.BitMap
           BitMap_.EdgeSmoothing(B,P.BitMap,P.MaskBitMap,R,True,False);

           if (Text<>'') and (P.MaskBitMap<>nil) then
              if SubTextsAreTransparent then with P.MaskBitMap.Canvas do begin
                 Brush.Color:=clBlack;
                 FillRect(R);
                 R2:=Classes.Rect(4,1,W-4,H-1);
                 Font.Name:=StatusPanel.Font.Name;
                 Font.Size:=SubTextsFontSize; Font.Color:=clWhite;

                 Windows.SetBkMode (Handle, Windows.TRANSPARENT);
                 Windows.DrawTextEx(Handle,PChar(Text),Length(Text),R2,DT_CENTER or DT_BOTTOM or DT_SINGLELINE,nil);
                 BitMap_.AlphaBlend(B,Self.BitMap,P.MaskBitMap,R,Classes.Rect(Left,Top,Right,Bottom),0,0,True);
                 end
              else with B.Canvas do begin
                 P.BitMap.Canvas.CopyRect(R,B.Canvas,R);
                 R2:=Classes.Rect(4,1,W-4,H-1);
                 Font.Name:=StatusPanel.Font.Name;
                 Font.Size:=SubTextsFontSize; Font.Color:=SubTextsFontColor;
                 Windows.SetBkMode (Handle, Windows.TRANSPARENT);
                 Windows.DrawTextEx(Handle,PChar(Text),Length(Text),R2,DT_CENTER or DT_BOTTOM or DT_SINGLELINE,nil);
                 BitMap_.AlphaBlend(B,P.BitMap,P.MaskBitMap,R,R,0,0,False);
                 end;

           if RectHeight(RectProgressBar)>0 then with P.BitMap.Canvas do with RectProgressBar do begin
              CopyRect(RectProgressBar,B.Canvas,Classes.Rect(R.Left+Left,R.Top+Top,R.Left+Left+RectWidth(RectProgressBar),R.Top+Top+RectHeight(RectProgressBar)));
              if P.MaskBitMap<>nil then with P.MaskBitMap.Canvas do begin
                 CopyMode:=cmSrcCopy;
                 CopyRect(R,B.Canvas,R);
                 Brush.Color:=StatusPanel.Font.Color; Brush.Style:=bsSolid;
                 FillRect(RectProgressBar);
                 BitMap_.AlphaBlend(B,P.MaskBitMap,nil,RectProgressBar,RectProgressBar,Min(100,Max(80,PanelTransparencyPct+10)),0,False);
                 end;
              end;

           if PanelBitMap<>nil then begin                                        // save the panel for later
              PanelBitMap.Assign(B);
              end
           else begin                                                            // draw the panel directly on status bitmap
              Self.BitMap.Canvas.CopyMode:=cmSrcCopy;
              Self.BitMap.Canvas.CopyRect(Classes.Rect(Left,Top,Right,Bottom),B.Canvas,R);
              end;
       except
          on  E:Exception do with Panels[Index] do begin
              Rect:=Classes.Rect(0,0,0,0); RectForm:=Rect;
              RectProgressBar:=Rect; RectFormProgressBar:=Rect;
              end;
       end;
     finally  P.Free; B.Free;
     end;
end;

function TStatus.MakeProgressBar(Index:TStatusPanelTypes; const Caption:String):Boolean;
var i,L,R,H,W:Integer; R1:TRect;
begin
  with Panels[Index] do begin
    for  i:=0 to Pred(Menu.Count) do
         if i<>Menu.BtnStopIndex then Menu.ShowButton(i,bsDisabled,False);
    if Index=spReplaySpeed then begin
       W:=PanelRoundRect+3*MAX_REPLAY_SPEED_MOVES_PER_SEC;
       // left-justified:
       //L:=Panels[spHint].Rect.Left;
       //right-justified:
       L:=Max(Panels[spHint].Rect.Left,Menu.MenuItems[Menu.btnReplayIndex].Rect.Left-STATUS_BUTTON_SEPARATOR_PIXELS-W);
       R:=Min(Menu.MenuItems[Menu.BtnReplayIndex].Rect.Left,Menu.MenuItems[Menu.BtnStopIndex].Rect.Left)-STATUS_BUTTON_SEPARATOR_PIXELS;
       W:=Max(0,R-L);
       end
    else begin
       W:=RectWidth(Panels[spHint].Rect);
       L:=Panels[spHint].Rect.Left;
       end;
    Rect:=ClipRect(Classes.Rect(L,Panels[spMoves].Rect.Top,L+W,Panels[spMoves].Rect.Bottom));
    RectForm:=StatusRectToImageRect(Rect);
    W:=RectWidth(Rect); H:=RectHeight(Rect);
    R1:=Classes.Rect(0,0,W,H);
    i:=Min(H-16,((H+1) div 2));

    RectProgressBar:=Classes.Rect(PanelRoundRect div 2,
                                  Max(Rect.Top+2,i-5),W-(PanelRoundRect div 2),
                                  Min(Rect.Bottom,Max(Rect.Top+2,
                                  i)));
    Result:=(W*H<>0) and
            (RectProgressBar.Left<RectProgressBar.Right) and
            BitMapResize(BitMap,W,H);
    if Result then begin
       with Rect do MakePanel(Index,Left,Right,Caption,RectProgressBar,BitMap);
       end
    else begin
      Rect:=Classes.Rect(0,0,0,0); RectForm:=Rect;
      RectProgressBar:=Rect; RectFormProgressBar:=Rect;
      end;
    end;
end;

procedure TStatus.SetReverseMode(ReverseMode__:Boolean);
begin
  if fReverseMode<>ReverseMode__ then begin
     fReverseMode:=ReverseMode__;
     BitMap.Canvas.CopyRect(Panels[spPushes].Rect,PushOrPullBitMap[fReverseMode].Canvas,Rect(0,0,RectWidth(Panels[spPushes].Rect),RectHeight(Panels[spPushes].Rect)));
     Paint;
     end;
end;

function  TStatus.StatusRectToImageRect(const R:TRect):TRect;
begin
  with Result do begin
    if   R.Left<R.Right  then begin Left:=R.Left+RectForm.Left; Right :=R.Right +RectForm.Left; end
    else                      begin Left:=0; Right :=0; end;
    if   R.Top <R.Bottom then begin Top :=R.Top+RectForm.Top  ; Bottom:=R.Bottom+RectForm.Top;  end
    else                      begin Top :=0; Bottom:=0; end;
    end;
end;

procedure TStatus.StartTimer;
begin
  if (Menu.BtnTimerIndex>=0) and (MainForm.Game<>nil) then with Menu do begin
     if MenuItems[BtnTimerIndex].State<>bsFocusedDisabled then
        ShowButton(BtnTimerIndex,bsFocusedDisabled,False);
     with MPlayerForm.Timer1 do
       if MainForm.Game.TimingIdleTimeThresholdEnabled and
          (MainForm.Game.TimingIdleTimeThresholdMS>=500) and
          (not Enabled) then begin
          Interval:=MAINFORM_TIMER_INTERVAL_MILLI_SECONDS;
          OnTimer:=MainForm.Timer1Timer;
          Enabled:=True;
          end;
     end;
end;

procedure TStatus.StopTimer;
begin
  if MPlayerForm<>nil then with MPlayerForm.Timer1 do
     if Enabled then Enabled:=False;
  with Menu do
    if (BtnTimerIndex>=0) and
       (MenuItems[BtnTimerIndex].State=bsFocusedDisabled) then
       ShowButton(BtnTimerIndex,bsEnabled,False);
end;

procedure TStatus.ClearPanels(DestroyBitMaps:Boolean);
var p:TStatusPanelTypes; b:TButtonState;
begin
  for p:=Low(p) to High(p) do with Panels[p] do begin
      if DestroyBitMaps then
         for b:=Low(b) to High(b) do begin
             BitMap.Free; BitMap:=nil;
             end;
      Rect:=Classes.Rect(0,0,0,0); RectForm:=Rect;
      RectProgressBar:=Rect; RectFormProgressBar:=Rect;
      ProgressBarFocused:=False; ProgressBarLocked:=p=spReplaySpeed;
      ProgressBarLockOnMouseUp:=False;
      end;
end;

function TStatus.EnterBrowseMode:Boolean;
begin
  Result:=False;

  ClearHint;

  with Panels[spBrowse] do begin
     if RectWidth(RectForm)=0 then begin
        if   RectWidth(Rect)=0 then MakeProgressBar(spBrowse,HintBrowseHistoryText)
        else RectForm:=StatusRectToImageRect(Rect);

        if   (RectWidth(RectForm)<>0) and (RectWidth(RectProgressBar)<>0) then begin
             Menu.HideButton(Menu.BtnReplayIndex); Menu.HideButton(Menu.BtnBrowseIndex);
             Menu.HideButton(Menu.BtnTimerIndex);  Menu.HideButton(Menu.BtnDeadlocksIndex);
             ClearHint;

             Canvas.CopyMode:=cmSrcCopy;
             Canvas.CopyRect(RectForm,BitMap.Canvas,Classes.Rect(0,0,RectWidth(Rect),RectHeight(Rect)));

             with Panels[spHint] do with Rect do Rect:=Classes.Rect(0,Top,0,Bottom);
             Panels[spHint].RectForm:=StatusRectToImageRect(Panels[spHint].Rect);

             if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;

             EnableDisableButtons;

             Result:=True;

             //Application.ProcessMessages; SleepEx(1000,True);
             end;
        end;
     end;
end;

procedure TStatus.LeaveBrowseMode;
begin
  if Panels[spBrowse].RectForm.Top<Panels[spBrowse].RectForm.Bottom then begin
     Panels[spHint  ].Rect    :=ClipRect(Rect(Panels[spBrowse].Rect.Left,
                                              Panels[spHint].Rect.Top,
                                              RectWidth(RectForm)-Max(STATUS_BUTTON_SEPARATOR_PIXELS,RightSlice+2),
                                              Panels[spHint].Rect.Bottom));
     Panels[spHint  ].RectForm:=StatusRectToImageRect(Panels[spHint].Rect);
     Panels[spBrowse].RectForm:=Rect(0,0,0,0);
     if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
     end;

  EnableDisableButtons;
end;

procedure TStatus.BrowseMouseMove(X,Y:Integer);
var Position:Integer;
begin
  with Panels[SpBrowse] do
    if RectForm.Top<RectForm.Bottom then with RectProgressBar do begin
       Dec(X,Rect.Left);
       if (X>=Left-8) and (X<=Right+8) and
          (Y>=Rect.Top-Self.TopMargin) and (Y<Rect.Bottom) then begin
          Position:=Max(0,Min(MainForm.Game.MaxBrowsePosition,
                              (MainForm.Game.MaxBrowsePosition*
                               ((X-Left)+
                                (RectWidth(RectProgressBar)                     // try to smooth things out a bit
                                 div                                            // when the number of positions are
                                 (2*Max(1,MainForm.Game.MaxBrowsePosition))     // low by adding half the size of
                                )                                               // each position
                               )
                              ) div Max(1,RectWidth(RectProgressBar))
                             ));
          if Position<>BrowsePosition then
             MainForm.Game.BrowsePosition:=Position;
          if (Screen.Cursor<>MainForm.Image1.DragCursor) and
             (not ProgressBarLocked) then
             Screen.Cursor:=MainForm.Image1.DragCursor;
          end
       else if Screen.Cursor<>DEFAULT_CURSOR then
               Screen.Cursor:=DEFAULT_CURSOR;
       end;
end;

procedure TStatus.SetBrowsePosition(BrowsePosition__:Integer);
begin
  with Panels[spBrowse] do
    if RectForm.Top<RectForm.Bottom then with Canvas do begin
       fBrowsePosition:=Max(0,Min(BrowsePosition__,MainForm.Game.MaxBrowsePosition));
       CopyMode:=cmSrcCopy;
//     CopyRect(RectFormProgressBar,BitMap.Canvas,RectProgressBar); // restore background
       CopyRect(RectFormProgressBar,Self.BitMap.Canvas,RectPlusOffset(RectProgressBar,Rect.Left,Rect.Top)); // restore background
       Brush.Color:=StatusPanel.Font.Color; Brush.Style:=bsSolid;
       with RectFormProgressBar do
         FillRect(Classes.Rect(Left,Top,Left+(BrowsePosition*RectWidth(RectFormProgressBar)) div Max(1,MainForm.Game.MaxBrowsePosition),Bottom));
     end;
end;

procedure TStatus.EnterReplayMode;
begin
  if MainForm.Game.AnimateReplayMovesMS=0 then
     MainForm.Game.AnimateReplayMovesMS:=DEFAULT_ANIMATE_REPLAY_MOVES_MS;

  ClearHint;

  if RectWidth(Panels[spReplaySpeed].RectForm)=0 then with Panels[spReplaySpeed] do begin
     if   RectWidth(Rect)=0 then MakeProgressBar(spReplaySpeed,ReplaySpeedText)
     else RectForm:=StatusRectToImageRect(Rect);

     if RectWidth(RectForm)<>0 then begin
        Menu.HideButton(Menu.BtnReplayIndex); Menu.HideButton(Menu.BtnBrowseIndex  );
        Menu.HideButton(Menu.BtnSolveIndex ); Menu.HideButton(Menu.BtnOptimizeIndex); Menu.HideButton(Menu.BtnGenerateIndex);
        ClearHint;

        Canvas.CopyMode:=cmSrcCopy;
        Canvas.CopyRect(RectForm,BitMap.Canvas,Classes.Rect(0,0,RectWidth(Rect),RectHeight(Rect)));
        SetReplayMovesPerSec(1000 div Max(1,MainForm.Game.AnimateReplayMovesMS));

        // left-justified:
        //Panels[spHint].Rect:=ClipRect(Classes.Rect(STATUS_BUTTON_SEPARATOR_PIXELS+Rect.Right,Panels[spHint].Rect.Top,Panels[spHint].Rect.Right,Panels[spHint].Rect.Bottom));
        //if Rect.Left=Panels[spHint].Rect.Left then with Panels[spHint].Rect do begin
        //   Left:=0; Right:=0;
        //   end;
        // right-justified:
        Panels[spHint].Rect:=ClipRect(Classes.Rect(Panels[spHint].Rect.Left,Panels[spHint].Rect.Top,Rect.Left-STATUS_BUTTON_SEPARATOR_PIXELS,Panels[spHint].Rect.Bottom));
        if Rect.Left<=Panels[spHint].Rect.Right then with Panels[spHint].Rect do begin
           Left:=0; Right:=0;
           end;

        Panels[spHint].RectForm:=StatusRectToImageRect(Panels[spHint].Rect);
        ProgressBarLocked:=True; ProgressBarLockOnMouseUp:=False;
        end;
     end
  else
     SetReplayMovesPerSec(1000 div Max(1,MainForm.Game.AnimateReplayMovesMS)); // update screen with current value

  EnableDisableButtons;
end;

procedure TStatus.LeaveReplayMode;
begin
  if Panels[spReplaySpeed].RectForm.Top<Panels[spReplaySpeed].RectForm.Bottom then begin
     // left-justified:
     //Panels[spHint]     .Rect    :=ClipRect(Rect(Panels[spReplaySpeed].Rect.Left,Panels[spHint].Rect.Top,RectWidth(RectForm)-Max(STATUS_BUTTON_SEPARATOR_PIXELS,RightSlice+2),Panels[spHint].Rect.Bottom));
     // right-justified:
     Panels[spHint]       .Rect    :=ClipRect(Rect(Panels[spHint].Rect.Left,Panels[spHint].Rect.Top,RectWidth(RectForm)-Max(STATUS_BUTTON_SEPARATOR_PIXELS,RightSlice+2),Panels[spHint].Rect.Bottom));

     Panels[spHint]       .RectForm:=StatusRectToImageRect(Panels[spHint].Rect);
     Panels[spReplaySpeed].RectForm:=Rect(0,0,0,0);
     Panels[spReplaySpeed].ProgressBarLockOnMouseUp:=False;
     if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
     end;

  EnableDisableButtons;
end;

procedure TStatus.ReplaySpeedMouseMove(X,Y:Integer);
var MovesPerSec:Integer;
begin
  with Panels[SpReplaySpeed] do
    if RectForm.Top<RectForm.Bottom then with RectProgressBar do begin
       Dec(X,Rect.Left);
       if (X>=Left-8) and (X<=Right+8) and
          (Y>=Rect.Top-Self.TopMargin) and (Y<Rect.Bottom) then begin
          if   ProgressBarLocked then begin
               MovesPerSec:=ReplayMovesPerSec;
               if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
               end
          else begin
                 MovesPerSec:=Max(0,
                                  Min(MAX_REPLAY_SPEED_MOVES_PER_SEC,
                                      (MAX_REPLAY_SPEED_MOVES_PER_SEC*
                                       ((X-Left)+
                                        (RectWidth(RectProgressBar)             // try to smooth things out a bit
                                         div                                    // when the number of positions are
                                         Max(1,MAX_REPLAY_SPEED_MOVES_PER_SEC)  // low by adding the size of each
                                        )                                       // position
                                       )
                                      )
                                      div
                                      Max(1,RectWidth(RectProgressBar))));
               end;
          if (MovesPerSec<>ReplayMovesPerSec) or (not ProgressBarFocused) then begin
             if MovesPerSec<>ReplayMovesPerSec then
                MainForm.Game.SessionSmoothMoveAnimationEnabled:=MainForm.Game.SmoothMoveAnimationEnabled;
             if (not ProgressBarFocused) and MainForm.Sound.Enabled then MainForm.Sound.Play(stMenuOver);
             if (Screen.Cursor<>MainForm.Image1.DragCursor) and
                (not ProgressBarLocked) then
                Screen.Cursor:=MainForm.Image1.DragCursor;
             ProgressBarFocused:=True; ProgressBarLockOnMouseUp:=True;
             ReplayMovesPerSec :=MovesPerSec;
             ShowHintForReplayMovesPerSec;
             end;
          end
       else begin
         ProgressBarLocked:=True;
         if ProgressBarFocused then begin
            ProgressBarFocused:=False; ReplayMovesPerSec:=ReplayMovesPerSec;     // refresh display
            if Screen.Cursor<>DEFAULT_CURSOR then
               Screen.Cursor:=DEFAULT_CURSOR;
            end;
         end;
       end;
end;

procedure TStatus.SetReplayMovesPerSec(MovesPerSec:Integer);
var i:Integer;
begin
  with Panels[spReplaySpeed] do
    if RectForm.Top<RectForm.Bottom then with Canvas do begin
       fReplayMovesPerSec:=Max(0,Min(MovesPerSec,MAX_REPLAY_SPEED_MOVES_PER_SEC));
       MovesPerSec:=ReplayMovesPerSec;

       CopyMode:=cmSrcCopy;
//     CopyRect(RectFormProgressBar,BitMap.Canvas,RectProgressBar); // restore background
       CopyRect(RectFormProgressBar,Self.BitMap.Canvas,RectPlusOffset(RectProgressBar,Rect.Left,Rect.Top)); // restore background
       with RectFormProgressBar do begin
         i:=(MovesPerSec*RectWidth(RectFormProgressBar)) div MAX_REPLAY_SPEED_MOVES_PER_SEC;
         if   ProgressBarFocused then begin
              Brush.Color:=StatusPanel.Font.Color;
              Brush.Style:=bsSolid;
              FillRect(Classes.Rect(Left,Top,Left+i,Bottom));
              end
         else CopyRect(Classes.Rect(Left,Top,Left+i,Bottom),BitMap.Canvas,Classes.Rect(RectProgressBar.Left,RectProgressBar.Top,RectProgressBar.Left+i,RectProgressBar.Bottom));
         end;
       if   ReplayMovesPerSec=0 then
            MainForm.Game.AnimateReplayMovesMS:=0
       else MainForm.Game.AnimateReplayMovesMS:=Min(1000 div ReplayMovesPerSec,MAX_ANIMATION_TIME_MS);
     end;
end;

procedure TStatus.ShowHintForReplayMovesPerSec;
begin
  Hint:=Misc_.MovesPerSecondText(ReplayMovesPerSec);
end;

procedure TStatus.EnableDisableButtons;
var i:Integer;
begin // precondition: 'MainForm.ShowStatus' has updated the states for the buttons on 'MainForm'
  if (Screen.ActiveForm=MainForm) or (Screen.ActiveForm=SnapshotsForm) then begin // 'if...': because 'MainForm.Deadlocks.Thread' calls this method synchronized, hence, it may call this procedure at a time when another window has control, e.g., the 'Settings' window
     MainForm.BtnStatusMenuReplay   .Enabled:=(MainForm.Game.History.Top>MainForm.Game.ForcedInitialJumps)
                                              and
                                              ((MainForm.Game.History.Count<>0)
                                               or
                                               (MainForm.Game.GameState<>gsSolved) // don't show the buttons 'Replay' and 'Browse' if the initial position is a solution
                                              )
                                              and
                                              (not MainForm.MPlayer.Visible);
     MainForm.BtnStatusMenuBrowse   .Enabled:=MainForm.BtnStatusMenuReplay.Enabled;
     MainForm.BtnStatusMenuTimer    .Enabled:=(MainForm.Game<>nil) and MainForm.Game.TimingEnabled  and (not MainForm.MPlayer.Visible);
     MainForm.BtnStatusMenuDeadlocks.Enabled:=(MainForm.Deadlocks<>nil) and (MainForm.Deadlocks.Thread<>nil) and
                                              ((MainForm.Deadlocks.Thread.State=Ord(dlLoadGame))
                                               or
                                               (MainForm.Deadlocks.Thread.State=Ord(dlIsCalculating))
                                              ) and
                                              (not MainForm.MPlayer.Visible);
     MainForm.BtnStatusMenuSolver   .Enabled:=(MainForm.Solver   <>nil) and True;
     MainForm.BtnStatusMenuOptimizer.Enabled:=(MainForm.Optimizer<>nil) and True;

     if (not MainForm.MPlayer.Visible) and
        (MainForm.Game<>nil) and
        MainForm.Game.IsReplaying and
        (Panels[spReplaySpeed].RectForm.Top<Panels[spReplaySpeed].RectForm.Bottom) then with Menu do begin // replay mode active
        i:=Menu.ItemIndex;
        HideButton(BtnReplayIndex); HideButton(BtnBrowseIndex);
        HideButton(BtnTimerIndex);  HideButton(BtnDeadlocksIndex);
        HideButton(BtnSolveIndex);  HideButton(BtnOptimizeIndex); HideButton(BtnGenerateIndex);
        with MenuItems[BtnStopIndex] do begin
          Visible:=(Rect.Left+Self.RectForm.Left>=Panels[spReplaySpeed].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                   (Rect.Left+Self.RectForm.Left>=Panels[spModified   ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                   (Rect.Left+Self.RectForm.Left>=Panels[spPushes     ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS);
          if Visible and (i=BtnReplayIndex) then begin
             Menu.SkipMouseOverSoundForItemIndex:=Menu.BtnStopIndex;
             Menu.ItemIndex:=Menu.BtnStopIndex;
             end;
          end;
        end
     else if Panels[spBrowse].RectForm.Top<Panels[spBrowse].RectForm.Bottom then begin // browse history mode
             for i:=0 to Pred(Menu.Count) do Menu.HideButton(i);
             end
          else with Menu do begin // normal game mode
             i:=ItemIndex;
             HideButton(BtnStopIndex);
             with MenuItems[BtnReplayIndex] do
               if    (Rect.Left+Self.RectForm.Left>=Panels[spReplaySpeed].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (Rect.Left+Self.RectForm.Left>=Panels[spModified   ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (Rect.Left+Self.RectForm.Left>=Panels[spPushes     ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) then begin
                     Visible:=True;
                     if i=BtnStopIndex then begin
                        Menu.SkipMouseOverSoundForItemIndex:=Menu.BtnReplayIndex;
                        Menu.ItemIndex:=Menu.BtnReplayIndex;
                        end;
                     end
               else  HideButton(BtnReplayIndex);
             with MenuItems[BtnBrowseIndex] do
               if    (Rect.Left+Self.RectForm.Left>=Panels[spReplaySpeed].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (Rect.Left+Self.RectForm.Left>=Panels[spModified   ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (Rect.Left+Self.RectForm.Left>=Panels[spPushes     ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (RectWidth(Panels[spReplaySpeed].RectForm)=0)
                     then
                     Visible:=True
               else  HideButton(BtnBrowseIndex);
             with MenuItems[BtnSolveIndex] do
               if    (Rect.Left+Self.RectForm.Left>=Panels[spReplaySpeed].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (Rect.Left+Self.RectForm.Left>=Panels[spModified   ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (Rect.Left+Self.RectForm.Left>=Panels[spPushes     ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (RectWidth(Panels[spReplaySpeed].RectForm)=0) and
                     Assigned(MainForm.Solver) and MainForm.Solver.IsActive
                     then
                     Visible:=True
               else  HideButton(BtnSolveIndex);
             with MenuItems[BtnOptimizeIndex] do
               if    (Rect.Left+Self.RectForm.Left>=Panels[spReplaySpeed].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (Rect.Left+Self.RectForm.Left>=Panels[spModified   ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (Rect.Left+Self.RectForm.Left>=Panels[spPushes     ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (RectWidth(Panels[spReplaySpeed].RectForm)=0) and
                     Assigned(MainForm.Optimizer) and MainForm.Optimizer.IsActive
                     then
                     Visible:=True
               else  HideButton(BtnOptimizeIndex);
             with MenuItems[BtnGenerateIndex] do
               if    (Rect.Left+Self.RectForm.Left>=Panels[spReplaySpeed].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (Rect.Left+Self.RectForm.Left>=Panels[spModified   ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (Rect.Left+Self.RectForm.Left>=Panels[spPushes     ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                     (RectWidth(Panels[spReplaySpeed].RectForm)=0) and
                     Assigned(MainForm.Generator) and MainForm.Generator.IsActive
                     then
                     Visible:=True
               else  HideButton(BtnGenerateIndex);
             if BtnTimerIndex>=0 then with MenuItems[BtnTimerIndex] do
               if   (Rect.Left+Self.RectForm.Left>=Panels[spReplaySpeed].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                    (Rect.Left+Self.RectForm.Left>=Panels[spModified   ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                    (Rect.Left+Self.RectForm.Left>=Panels[spPushes     ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                    (RectWidth(Panels[spReplaySpeed].RectForm)=0)
                    then
                    Visible:=True
               else HideButton(BtnTimerIndex);
             if  BtnDeadlocksIndex>=0 then with MenuItems[BtnDeadlocksIndex] do
                 if (Rect.Left+Self.RectForm.Left>=Panels[spReplaySpeed].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                    (Rect.Left+Self.RectForm.Left>=Panels[spModified   ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                    (Rect.Left+Self.RectForm.Left>=Panels[spPushes     ].RectForm.Right+STATUS_BUTTON_SEPARATOR_PIXELS) and
                    (RectWidth(Panels[spReplaySpeed].RectForm)=0) and
                    MainForm.BtnStatusMenuDeadlocks.Enabled
                    then
                    Visible:=True
                 else HideButton(BtnDeadlocksIndex);
             end;

     Menu.Show;
     if   HintCount>1 then Hint:=''
     else Hint:=WelcomeText;
     end;
end;

function TStatus.MouseMove(X,Y:Integer):Integer;
begin
  Dec(X,RectForm.Left); Dec(Y,RectForm.Top);
  LastMousePos.X:=X; LastMousePos.Y:=Y; LastMenuItemIndex:=Menu.ItemIndex;
  Result:=Menu.MouseMove(X,Y);
  with Panels[spReplaySpeed].RectForm do
    if Top<Bottom then begin // replay mode is activated

       if      Y>=0-Self.TopMargin then // the mouse is inside the statusbar area, or at least inside the extended mouse area for the statusbar
       else if MainForm.ReplayStarterButton=MainForm.BtnStatusMenuReplay then begin
               // stop replay mode when the mouse leaves the statusbar area
               //MainForm.Game.IsIdleAndStopReplayingAndBrowsing;
               end;

       if (Y<0-Self.TopMargin) and
          (not MainForm.Game.IsReplaying) and
          MainForm.Game.IsIdleAndStopReplayingAndBrowsing then begin
          LeaveReplayMode;
          end
       else begin

          ReplaySpeedMouseMove(X,Y);

          if Result>=0 then
             if Menu.MenuItems[Result].SpeedButton.Enabled then begin
                if      Result=Menu.BtnStopIndex      then begin
                        end
                else if Result=Menu.BtnReplayIndex    then begin
                        end;
                end;
          end;
       end
    else // maybe enter replay mode ...
       if        (Result=Menu.BtnReplayIndex) and (Menu.MenuItems[Menu.BtnReplayIndex].State=bsFocusedEnabled) and (not MainForm.MPlayer.Visible) then begin
                 EnterReplayMode;
                 Menu.SkipMouseOverSoundForItemIndex:=Menu.BtnReplayIndex;
                 end
       else if   (Result=Menu.BtnBrowseIndex) and (Menu.MenuItems[Menu.BtnBrowseIndex].State=bsFocusedEnabled) and (not MainForm.MPlayer.Visible) then
                 MainForm.Game.EnterBrowseMode(True) // ... or browse mode, browsing choice points
            else if MainForm.Game.IsBrowsing then // browse mode is activated
                    if   Y<0-Self.TopMargin then MainForm.Game.LeaveBrowseMode(True)
                    else BrowseMouseMove(X,Y)
                 else if Result>=0 then
                         if      Result=Menu.BtnTimerIndex then begin // ... or show timer
                                 MainForm.Game.StopTimer;
                                 Hint:=TimeText+COLON+SPACE+SokUtil_.TimeToStr(MainForm.Game.ElapsedTimeMS);
                                 end
                         else if (Result=Menu.BtnDeadlocksIndex) and
                                 MainForm.BtnStatusMenuDeadlocks.Enabled then begin // ... or show deadlock calculation statistics
                                 Hint:=Format(DeadlockCalculationStatisticsText__,[MainForm.Deadlocks.Count,(MainForm.Deadlocks.TimeMS+500) div 1000])+
                                       HintStopDeadlockCalculationText;
                                 end;
end;

function  TStatus.Click:Integer;
begin
  Result:=-1;
  if   Menu.ItemIndex>=0 then begin
       Result:=Menu.Click;
       end
  else with Panels[spReplaySpeed].RectForm do
         if Top<Bottom then // replay mode is activated
            if   Mouse.CursorPos.Y<RectForm.Top then
                 LeaveReplayMode
            else MainForm.Game.IsIdleAndStopReplayingAndBrowsing
         else with Panels[spBrowse].RectForm do
                if (Top<Bottom) and MainForm.Game.IsBrowsing and
                   PtInRect(Classes.Rect(Left,Top-Self.TopMargin,Right,Bottom),
                            Point(LastMousePos.X+RectForm.Left,LastMousePos.Y+RectForm.Top)) then begin
                   MainForm.Game.LeaveBrowseMode(False); // exit browse mode, loading current position
                   if MainForm.Sound.Enabled then MainForm.Sound.Play(stMenuSelect);
                   end;
end;

end.

