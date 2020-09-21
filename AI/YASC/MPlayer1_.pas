unit MPlayer1_;

interface

uses Windows,Classes,Controls,ExtCtrls,Graphics,Forms,Messages,
     Misc_,IniFile_,Menu_,BitMap_,Pict_,Display_;
{
const
  ACTIVITY_RECT_LEFT           = 510; // must match 'MainForm.
  ACTIVITY_RECT_TOP            =   0; //
  ACTIVITY_RECT_WIDTH          =  45; //
  ACTIVITY_RECT_HEIGHT         = 150; //
}
type
  TWordVec                     = array[0..(MaxInt div SizeOf(Word))-1] of Word;
  PWordVec                     = ^TWordVec;

  TButtonState                 = (bsEnabled,bsFocused,bsDisabled);
  TMPlayerButtonType           = (btNone, // order must not change
                                  btDisplay,
                                  btPositionBar,btPosition,
                                  btPlay,btStop,btPrev,btNext,
                                  btVolumeBar,btVolume,
                                  btOpen,btExit,btHelp,
                                  btPlayPause,
                                  btActivityMenu,
                                  btActivityPrev,
                                  btActivityNext);

  TMPlayerButton               = record
    BitMap                     : array[TButtonState] of TBitMap;
    BitMapSet                  : set of TButtonState; // set of valid bitmaps
    BoundsRect                 : TRect;
    Hint                       : String;
    LinkedButton               : Integer;
    MaskBitMap                 : array[TButtonState] of TBitMap;
    MaskColor                  : TRGB;
    Masked                     : Boolean;
    MaskPct                    : Integer;
    MPlayerRect                : TRect;
    SourceRect                 : array[TButtonState] of TRect;
    State                      : TButtonState;
    Visible                    : Boolean;
  end;

  TMPlayerButtonsVec           = array[0..Ord(High(TMPlayerButtonType))-Ord(Low(TMPlayerButtonType))] of TMPlayerButton;

  TMPlayer = class(TPict)
  private
    ActiveButton:TMPlayerButtonType;
    BackBitMap:TBitMap;
    BackRect:TRect;
    Canvas:TCanvas;
    Dragging:Boolean;
    DragPoint:TPoint;
    DragRect:TRect;
    fItemIndex: Integer;
    FrameStartTime:Cardinal;
    IsBusy:Boolean;
    LastItemIndex:Integer;
    oMusicEnabled:Boolean;
    oOnIdle:TIdleEvent;
    oOnMessage:TMessageEvent;
    PositionBarWidth:Integer;
    PositionFrameStartTime:Cardinal;
    PositionScreenLeft:Integer;
    SortedButtons:array[0..Ord(High(TMPlayerButtonType))-Ord(Low(TMPlayerButtonType))] of TMPlayerButtonType;
    SourceBitMap:TBitMap;
    VolumeBarWidth:Integer;
    VolumeScreenLeft:Integer;
    procedure ButtonMoveTo(Button:TMPlayerButtonType; X,Y:Integer);
    procedure DestroyMenuButtons(DestroyMasks:Boolean);
    procedure DrawRect(const Rect:TRect; PenMode:TPenMode);
    procedure EnsureVisiblePosition(var Left,Top,Right,Bottom:Integer);
    procedure OnIdle;
    function  ItemAtPos(X, Y: Integer): Integer;
    procedure MPlayerNotify(Sender: TObject);
    function  MPlayerRectToBackRect(const R:TRect):TRect;
    function  MPlayerRectToScreenRect(const R:TRect):TRect;
    procedure MPlayerXYtoFormXY(var X,Y:Integer);
    procedure SetDragPointToButtonCenterOffset(Button:TMPlayerButtonType);
    procedure SetItemIndex(ItemIndex__:Integer);
    procedure FormXYtoMPlayerXY(var X,Y:Integer);
  public
    ActivityRectLeft,
    ActivityRectTop,
    ActivityRectWidth,
    ActivityRectHeight:Integer;
    Animation:Boolean;
    Buttons:TMPlayerButtonsVec;
    DefaultTileRect:TRect;
    Display:TDisplay;
    DisplayRect:TRect;
    EdgeSmoothing:Boolean;
    EdgeTransparencyPct:Integer;
    Fps:Cardinal;
    FpsOk:Boolean; // frame rate is ok, i.e., above a required minimum
    Initialized:Boolean;
    IsIdle:Boolean;
    MaskBitMapColor:TRGB;
    MaskBitMapPct:Integer;
    MPlayerDisplayPanel:TPanel;
    RGBShift:TRGBShift;
    TickCount:Cardinal;
    TimeCount:Cardinal;
    TileRect:TRect;
    Transparency:Boolean;
    TransparencyPct:Integer;
    Visible:Boolean;

    Left:Integer;
    Top:Integer;
    Width:Integer;
    Height:Integer;

    function    Click(MouseButton:TMouseButton): Integer;
    constructor Create(Canvas__:TCanvas; MPlayerDisplayPanel__:TPanel);
    destructor  Destroy; override;
    procedure   FormXYtoMPlayerDisplayXY(var X,Y:Integer);
    procedure   Hide;
    function    Initialize: Boolean;
    function    LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
    function    MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer):Boolean; // returns true if mouse-click is over MPlayer, considering masked image
    function    MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer):Boolean; // returns true if mouse is over MPlayer, considering the masked image
    procedure   MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure   MoveTo(X,Y:Integer);
    procedure   MPlayerOnIdle;
    procedure   ResetFps;
    function    SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
    procedure   SetDefaultValues;
    procedure   Show;
    procedure   ShowButton(Index,Link,NextItemIndex:Integer; ButtonState: TButtonState; ShowHint,DisplayMenuJustClosed: Boolean);
    procedure   ShowButtons;
    procedure   ShowPosition;
    procedure   ShowStatus;
    procedure   ShowVolume;
    procedure   Stop;

    procedure   Write(const s:String; const R:TRect; Flags:Cardinal);

    property    ItemIndex:Integer read fItemIndex write SetItemIndex;
  end;

implementation

uses SysUtils,MPlayer,SokUtil_,Text_,Main_,Options_,Sound_,Open1_,Music_,MPlayer2_,
     IView1_,Fractal_,Fworks_,Mandal1_;

const
  LEFT__=0; TOP__=1; RIGHT__=2; BOTTOM__=3; // rectangle slots

  clCyan :TColor = TColor((255 shl 16)+(255 shl 8));

  BUTTON_FONT_COLOR :array[TButtonState] of TColor = (clAqua,clYellow,clLtGray);

  DEFAULT_MASK_COLOR :TRGB = (r:5; g:130; b:252); // actually bgr, not rgb; (~orange) must match mask color for 'MainForm.MPlayerImage.Picture.BitMap'
  BUTTON_MASK_COLOR  :TRGB = (r:9; g:  2; b:  4); // actually bgr, not rgb; must match buttons in 'MainForm.MPlayerImage.Picture.BitMap'

  MUSIC_PLAYER_INIFILE_SECTION='MusicPlayer'; // don't localize

constructor TMPlayer.Create(Canvas__:TCanvas; MPlayerDisplayPanel__:TPanel);
begin
  Inherited Create;
  Canvas               :=Canvas__;
  MPlayerDisplayPanel  :=MPlayerDisplayPanel__;
  Visible              :=False; Dragging:=False;
  IsBusy:=False; IsIdle:=False; FpsOk:=True;
  BackBitMap           :=nil;
  FillChar(Buttons,SizeOf(Buttons),0);
  SetDefaultValues;
  BitMap_.BitMapCreate(BackBitMap,1,1);
  FillChar(BackRect,SizeOf(BackRect),0);
  BitMap_.BitMapCreate(SourceBitMap,1,1);
  fItemIndex:=-1; oMusicEnabled:=True; Display:=nil;
  try    Display:=TDisplay.Create(Canvas,MPlayerDisplayPanel);
  except on E:Exception do begin Display.Free; Display:=nil; end;
  end;
end;

destructor TMPlayer.Destroy;
begin
  DestroyMenuButtons(True);
  BackBitMap.Free;
  SourceBitMap.Free;
  Display.Free;
  Inherited Destroy;
end;

procedure TMPlayer.DestroyMenuButtons(DestroyMasks:Boolean);
var i:Integer; b:TButtonState;
begin
  for i:=Low(Buttons) to High(Buttons) do with Buttons[i] do
      for b:=Low(BitMap) to High(BitMap) do begin
          if BitMap[b]<>nil then begin
             BitMap[b].Free;
             BitMap[b]:=nil;
             end;

          if DestroyMasks then begin
             if MaskBitMap[b]<>nil then MaskBitMap[b].Free;
             MaskBitMap[b]:=nil;
             end;
          end;
end;

function  TMPlayer.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
var i:Integer;
begin
  Transparency          :=              IniFile.ReadBool   (MUSIC_PLAYER_INIFILE_SECTION,'Transparency',Transparency);
  TransparencyPct       :=Max(0,Min(100,IniFile.ReadInteger(MUSIC_PLAYER_INIFILE_SECTION,'TransparencyPct',TransparencyPct)));
  i                     :=              IniFile.ReadInteger(MUSIC_PLAYER_INIFILE_SECTION,'ColorShift',Ord(RGBShift));
  if (i>=Ord(Low(RGBShift))) and (i<=Ord(High(RGBShift))) then RGBShift:=TRGBShift(i);
  Result:=True;
end;

function  TMPlayer.SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
begin
  IniFile.WriteBool   (MUSIC_PLAYER_INIFILE_SECTION,'Transparency',Transparency);
  IniFile.WriteInteger(MUSIC_PLAYER_INIFILE_SECTION,'TransparencyPct',TransparencyPct);
  IniFile.WriteInteger(MUSIC_PLAYER_INIFILE_SECTION,'ColorShift',Ord(RGBShift));
  Result:=True;
end;

procedure TMPlayer.SetDefaultValues;
const //BACKGROUND_COLOR:TRGB=(r:161;g:7;b:7); // kludge: actually BGR; not RGB // must match 'MainForm.MPlayerImage'
  W:array[0..12*Succ((Ord(High(TMPlayerButtonType))-Ord(Low(TMPlayerButtonType))))-1] of Word= ( // must match 'MainForm.MplayerImage'
{
         002,004,338,256, 002,004,338,256, 002,004,338,256, // background
         074,038,266,146, 074,038,266,146, 074,038,266,146, // display
         074,153,266,166, 350,188,542,201, 350,201,542,214, // positionbar
         450,235,468,253, 480,235,498,253, 510,235,528,253, // position
         066,172,152,218, 344,003,430,049, 494,003,580,049, // play
         158,180,187,209, 351,055,380,084, 501,055,530,084, // stop
         201,178,230,210, 352,105,381,137, 502,105,531,137, // previous
         243,178,272,210, 352,155,381,187, 502,155,531,187, // next
         103,226,237,238, 343,216,477,228, 493,216,627,228, // volumebar
         450,235,468,253, 480,235,498,253, 510,235,528,253, // volume
         038,077,067,100, 410,230,439,253, 038,077,067,100, // open
         270,046,290,067, 350,233,370,254, 270,046,290,067, // exit
         288,069,309,093, 379,230,400,254, 288,069,309,093, // help
         394,053,480,099, 394,103,480,149, 066,172,152,218, // pause (play)
         544,053,630,099, 544,103,630,149, 066,172,152,218, // open (play)
         270,120,271,121, 544,155,545,156, 544,155,545,156  // menu
}
         002,004,506,381, 002,004,506,381, 002,004,506,381, // background
         094,075,414,255, 094,075,414,255, 094,075,414,255, // display
         094,277,414,285, 094,075,414,083, 094,247,414,255, // positionbar
         096,215,116,234, 120,215,140,234, 144,215,164,234, // position
         028,311,150,332, 095,148,217,169, 095,170,217,191, // play
         163,313,206,334, 095,104,138,125, 095,192,138,213, // stop
         223,313,266,334, 139,104,182,125, 139,192,182,213, // previous
         283,313,326,334, 183,104,226,125, 183,192,226,213, // next
         229,345,320,363, 227,104,317,123, 227,192,317,211, // volumebar
         096,215,116,234, 120,215,140,234, 144,215,164,234, // volume
         338,311,474,332, 095,126,231,147, 095,126,231,147, // open
         466,015,490,039, 290,214,314,238, 466,015,490,039, // exit
         433,015,457,039, 265,214,289,238, 433,015,457,039, // help
         218,170,340,191, 218,148,340,169, 218,170,340,191, // pause (play)
         414,075,459,225, 414,075,459,225, 414,075,459,225, // menu
         427,237,446,256, 319,104,338,123, 427,237,445,256, // previous activity
         453,237,472,256, 339,104,358,123, 452,237,472,256  // next activity
  );

  ACTIVITY_RECT_LEFT           = 510; // must match 'MainForm.MPlayerImage'
  ACTIVITY_RECT_TOP            =   0; // -
  ACTIVITY_RECT_WIDTH          =  45; // -
  ACTIVITY_RECT_HEIGHT         = 150; // -

var i:Integer; bs:TButtonState;
begin
  Visible                      :=False;
  Initialized                  :=False;
  RGBShift                     :=rgbShiftNone;
  FileName:=DEFAULT_VALUE;
  TileRect:=Rect(W[LEFT__],W[TOP__],W[RIGHT__],W[BOTTOM__]);
  DefaultTileRect:=TileRect;
  Masked                       :=True;
  MaskBitMapColor              :=DEFAULT_MASK_COLOR;
  MaskBitMapPct                :=5;
  Transparency                 :=True;
  TransparencyPct              :=10;
  EdgeTransparencyPct          :=25;
  EdgeSmoothing                :=True;
  Left                         :=0;
  Top                          :=0;
  Width                        :=W[RIGHT__ ]-W[LEFT__];
  Height                       :=W[BOTTOM__]-W[TOP__ ];

  for i:=Low(Buttons) to High(Buttons) do with Buttons[i] do begin
      Hint:=MusicPlayerText[Succ(2*i)];
      for bs:=Low(TButtonState) to High(TButtonState) do with SourceRect[bs] do begin
          Left  :=W[12*i+Ord(bs)*4+LEFT__  ];
          Top   :=W[12*i+Ord(bs)*4+TOP__   ];
          if bs=Low(TButtonState) then begin
             Right :=W[12*i+Ord(bs)*4+RIGHT__ ];
             Bottom:=W[12*i+Ord(bs)*4+BOTTOM__];
             end
          else begin
             Right :=Left+RectWidth (SourceRect[Low(TButtonState)]);
             Bottom:=Top +RectHeight(SourceRect[Low(TButtonState)]);
             end;
          end;
      MPlayerRect:=SourceRect[bsEnabled];

      Visible:=not (TMPlayerButtonType(i) in [btDisplay,btPlayPause]);
      if   TMPlayerButtonType(i) in [btDisplay,btPlay,btStop,btPrev,btNext] then
           State:=bsDisabled
      else State:=bsEnabled;
      if i=Ord(btDisplay) then begin
        Masked:=True;  MaskColor:=RGB_WHITE; MaskPct:=25;
        end
      else begin
        Masked:=TMPlayerButtonType(i) in [btPosition,btVolume];
        MaskColor:=BUTTON_MASK_COLOR; MaskPct:= 0;
        end;
      end;

  ActivityRectLeft  :=ACTIVITY_RECT_LEFT;
  ActivityRectTop   :=ACTIVITY_RECT_TOP;
  ActivityRectWidth :=ACTIVITY_RECT_WIDTH;
  ActivityRectHeight:=ACTIVITY_RECT_HEIGHT;
end;

function  TMPlayer.Initialize:Boolean;

  function  LoadPicture:Boolean;
  var {i:Integer; s:String; Size:TSize; State:TButtonState;} TempBitMap:TBitMap;
{
    procedure ExtractButtons;
    var i,Y:Integer; R1:TRect; BM:TBitMap;
    begin
      if BitMapCreate(400,1000,BM) then
         try     BM.Canvas.Brush.Color:=clGreen;
                 BM.Canvas.FillRect(Classes.Rect(0,0,BM.Width,BM.Height));
                 Y:=1; FillChar(R1,SizeOf(R1),0);
                 for i:=Ord(btPositionBar) to Ord(btActivityNext) do
                     with Buttons[i] do begin
                       R1:=SourceRect[bsFocused];
                       BM.Canvas.CopyRect(Classes.Rect(1,Y,1+RectWidth(R1),Y+RectHeight(R1)),SourceBitMap.Canvas,R1);
                       Inc(Y,Succ(RectHeight(R1)));
                       end;
                 BitMapDump(BM);
         finally BM.Free;
         end;
    end;
}
  begin  //LoadPicture
    try
         if (FileName<>DEFAULT_VALUE) and
            FileExists(FileName) and
            LoadFromFile(FileName) and
            ((Pict=nil) or              // simple bitmap
            MakeOrgBitMapFromPict) then begin// the image is able to draw itself
            SourceBitMap.Assign(OrgBitMap);
            MaskBitMapColor:=RGB_BLACK;
            end
         else begin
            FileName:=DEFAULT_VALUE; TileRect:=DefaultTileRect;
            with MainForm.MPlayerImage.Picture.BitMap do begin
                 SourceBitMap.Width :=Width;
                 SourceBitMap.Height:=Height;
                 SourceBitMap.Canvas.Draw(0,0,MainForm.MPlayerImage.Picture.BitMap);
                 end;
            MaskBitMapColor:=DEFAULT_MASK_COLOR;
            end;

         if RGBShift<>rgbShiftNone then begin
            BitMapRGBShift(SourceBitMap,RGBShift);
            RGBColorShift(RGBShift,MaskBitMapColor);
            end;

         TempBitMap:=nil;
         try
           with Buttons[Ord(btDisplay)] do
             if BitMapCreate(TempBitMap,
                             RectWidth (SourceRect[bsEnabled]),
                             RectHeight(SourceRect[bsEnabled])) then
                TempBitMap.Canvas.CopyRect(Rect(0,0,RectWidth (SourceRect[bsEnabled]),RectHeight(SourceRect[bsEnabled])),
                                           SourceBitMap.Canvas,
                                           SourceRect[bsEnabled]);

           with SourceBitMap.Canvas do begin // clear display rectangle
             Brush.Color:=clBlack; //RGBToColor(AndBitMapMaskColor);
             FillRect(Buttons[Ord(btDisplay)].SourceRect[bsEnabled]);
             end;

           Result:=LoadFromBitMapRect(SourceBitMap,TileRect,Point(0,0),RGBToColor(MaskBitMapColor));

           if Result then begin
              if MakeOrgBitMapFromPict then begin // make 'OrgBitMap' an independent bitmap, not a clone of 'BitMap'
                 OrgBitMap.Width :=BitMap.Width;
                 OrgBitMap.Height:=BitMap.Height;
                 OrgBitMap.Canvas.CopyRect(BitMapRect,BitMap.Canvas,BitMapRect);
                 end;

              if Masked or Transparency or EdgeSmoothing then begin
                 BitMap.Canvas.Brush.Color:=ContrastColor(RGBToColor(MaskBitMapColor));
                 BitMap.Canvas.FillRect(RectPlusOffset(Buttons[Ord(btDisplay)].SourceRect[bsEnabled],-TileRect.Left,-TileRect.Top));
                 MakeMaskBitMap(MaskBitMapColor,MaskBitMapPct,MaskExteriorOnly);
                 BitMap.Canvas.Brush.Color:=RGBToColor(MaskBitMapColor);
                 BitMap.Canvas.FillRect(RectPlusOffset(Buttons[Ord(btDisplay)].SourceRect[bsEnabled],-TileRect.Left,-TileRect.Top));
                 end;
              end;

           with Buttons[Ord(btDisplay)] do
             if (TempBitMap<>nil) and (SourceBitMap<>nil) then
                SourceBitMap.Canvas.CopyRect(SourceRect[bsEnabled],
                                             TempBitMap.Canvas,
                                             Rect(0,0,RectWidth (SourceRect[bsEnabled]),RectHeight(SourceRect[bsEnabled])));

         finally
           TempBitMap.Free;
         end;

{
         if BitMap   <>nil then BitMap.SaveToFile('1.bmp');
         if OrgBitMap<>nil then OrgBitMap.SaveToFile('2.bmp');
         if AndBitMap<>nil then AndBitMap.SaveToFile('3.bmp');
         if Pict     <>nil then Pict.SaveToFile('4.bmp');
}
    except
      on E:Exception do begin
         Error(E.Message,Application.Title); Clear; Result:=False;
         end;
    end;
    if not Result then MakeBlank(RectWidth(TileRect),RectHeight(TileRect),clNavy);
  end;

  procedure SortButtons;
  var i,j,t1,t2:Integer; Temp:TMPlayerButtonType;
  begin
    for i:=Low(SortedButtons) to High(SortedButtons) do SortedButtons[i]:=TMPlayerButtonType(i);

    for i:=Succ(Low(SortedButtons)) to High(SortedButtons) do
        for j:=i downto Low(SortedButtons) do begin
            t1:=Buttons[Ord(SortedButtons[Pred(j)])].MPlayerRect.Top;
            t2:=Buttons[Ord(SortedButtons[j      ])].MPlayerRect.Top;
            if (t1>t2)
               or
               ((t1=t2)
                and
                (Buttons[Ord(SortedButtons[Pred(j)])].MPlayerRect.Left>
                 Buttons[Ord(SortedButtons[j      ])].MPlayerRect.Left)
               ) then begin
               Temp:=SortedButtons[j];
               SortedButtons[j]:=SortedButtons[Pred(j)];
               SortedButtons[Pred(j)]:=Temp;
               end
            else break;
            end;
  end;


begin // Initialize
  OrgBitMap.Free; OrgBitMap:=nil; MakeBitMap; Hide;
  DestroyMenuButtons(True);
  Result:=LoadPicture;

  Buttons[Ord(btPosition   )].LinkedButton:=-Ord(btPositionBar); // '-': behind this button
  Buttons[Ord(btPositionBar)].LinkedButton:=+Ord(btPosition);    // '+': in front of this button
  Buttons[Ord(btVolume     )].LinkedButton:=-Ord(btVolumeBar);
  Buttons[Ord(btVolumeBar  )].LinkedButton:=+Ord(btVolume);

  with Buttons[Ord(btPosition)] do with MPlayerRect do begin
    Masked:=True; MaskColor:=BUTTON_MASK_COLOR;
    if RGBShift<>rgbShiftNone then RGBColorShift(RGBShift,MaskColor);

    Left  :=Buttons[Ord(btPositionBar)].MPlayerRect.Left-(RectWidth (MPlayerRect) div 2);
    Top   :=Buttons[Ord(btPositionBar)].MPlayerRect.Top +
            ((RectHeight(Buttons[Ord(btPositionBar)].MPlayerRect)-
              RectHeight(MPlayerRect))
            div 2) -
            3; // -3: this just happens to give the best result with the builtin "skin"
    Right :=Left+RectWidth (SourceRect[bsEnabled]);
    Bottom:=Top +RectHeight(SourceRect[bsEnabled]);
    BoundsRect:=Rect(Left,Top,
                     Left+RectWidth (Buttons[Ord(btPositionBar)].SourceRect[bsEnabled]),
                     Top +RectHeight(Buttons[Ord(btPositionBar)].SourceRect[bsEnabled]));
    end;
  with Buttons[Ord(btVolume)] do with MPlayerRect do begin
    Masked:=True; MaskColor:=BUTTON_MASK_COLOR;
    if RGBShift<>rgbShiftNone then RGBColorShift(RGBShift,MaskColor);

    Left:=Buttons[Ord(btVolumeBar)].MPlayerRect.Left {- (RectWidth (MPlayerRect) div 2)};
    Top   :=Buttons[Ord(btVolumeBar)].MPlayerRect.Top +
            ((RectHeight(Buttons[Ord(btVolumeBar)].MPlayerRect)-
             RectHeight(MPlayerRect))
             div 2) -
             1;
    Right :=Left+RectWidth (SourceRect[bsEnabled]);
    Bottom:=Top +RectHeight(SourceRect[bsEnabled]);
    BoundsRect:=Rect(Left,Top,
                     Left+RectWidth (Buttons[Ord(btVolumeBar)].SourceRect[bsEnabled])-RectWidth(SourceRect[bsEnabled]),
                     Top +RectHeight(Buttons[Ord(btVolumeBar)].SourceRect[bsEnabled]));
    end;
  with Buttons[Ord(btPlayPause)] do MPlayerRect:=Buttons[Ord(btPlay)].MPlayerRect;

  SortButtons;

  if BackBitMap<>nil then
     try BackBitMap.Width :=Width;
         BackBitMap.Height:=Height;
     except on E:Exception do begin BackBitMap.Free; BackBitMap:=nil; end;
     end;

  Left:=(MainForm.Image1.ClientWidth-Width  ) div 2;
  Top :=(MainForm.Image1.ClientHeight-Height) div 2;

  PositionBarWidth:=RectWidth(Buttons[Ord(btPositionBar)].MPlayerRect);
  VolumeBarWidth  :=RectWidth(Buttons[Ord(btVolumeBar  )].MPlayerRect)-
                    RectWidth(Buttons[Ord(btVolume     )].MPlayerRect);
end;

procedure TMPlayer.MoveTo(X,Y:Integer);
begin
  if not IsBusy then
     try     IsBusy:=True;
             Hide;
             if Transparency and ((X<>Left) or (Y<>Top)) then
                DestroyMenuButtons(False);
             Left:=X; Top:=Y;

             Show;
     finally IsBusy:=False;
     end;
end;

procedure TMPlayer.Show;
var R1,R2:TRect;
begin
  if MainForm.Game<>nil then MainForm.Game.StopTimer;
  if MainForm.Deadlocks<>nil then MainForm.Deadlocks.Suspend;
  if not Initialized then Initialized:=Initialize;

  if (not Visible) and (BackBitMap<>nil) then begin
     Visible:=True; LastItemIndex:=-1;
     oOnMessage:=Application.OnMessage;
     oOnIdle   :=Application.OnIdle; Application.OnIdle:=nil;

     with MainForm.Music do begin
       oMusicEnabled:=Enabled;
       Enabled:=True;
       FirstTime:=False;
       UpdateFileList(MusicSource);
       if Player=nil then
          CreatePlayer(PlayerNotify); // the mainform suffers from flicker when this statement is executed; reason unknown
       if Player<>nil then Player.OnNotify:=MPlayerNotify;
       end;

     BackRect:=Rect(Left,Top,Left+BitMap.Width,Top+BitMap.Height);
     with BackRect do EnsureVisiblePosition(Left,Top,Right,Bottom);
     Left:=BackRect.Left; Top:=BackRect.Top;

     PositionScreenLeft:=Buttons[Ord(btPosition)].BoundsRect.Left; R1.Left:=0;
     MPlayerXYtoFormXY(PositionScreenLeft,R1.Left);

     VolumeScreenLeft:=Buttons[Ord(btVolume    )].BoundsRect.Left; R1.Left:=0;
     MPlayerXYtoFormXY(VolumeScreenLeft,R1.Left);

     if BackBitMap<>nil then
        BackBitMap.Canvas.CopyRect(BitMapRect,Self.Canvas,BackRect);

     if OrgBitMap<>nil then
        BitMap.Canvas.CopyRect(BitMapRect,OrgBitMap.Canvas,BitMapRect);

     if (Masked or Transparency or EdgeSmoothing) and (BackBitMap<>nil) then begin

        if MaskBitMap=nil then
           MakeMaskBitMap(MaskBitMapColor,MaskBitMapPct,MaskExteriorOnly);
        if MaskBitMap=nil then Masked:=False;

        if Dragging then
           Draw(Left,Top,Self.Canvas)
        else begin
           if   Transparency then
                AlphaBlend(BitMap,BackBitMap,MaskBitMap,BitMapRect,BitMapRect,TransparencyPct,EdgeTransparencyPct,False)
           else AlphaBlend(BitMap,BackBitMap,MaskBitMap,BitMapRect,BitMapRect,0,10,False);
           if   EdgeSmoothing then
                BitMap_.EdgeSmoothing(BitMap,BackBitMap,MaskBitMap,Classes.Rect(0,0,Width,Height),True,False);
//         BitMapTrilinearAntiAliasing(BitMap,Masked,RGBToColor(AndBitMapMaskColor),AndBitMapMaskPct);
           end;
        end;

     if {Transparency and} (Display.TransparencyPct<>0) then with Buttons[Ord(btDisplay)] do begin
        BitMap[bsDisabled].Free; BitMap[bsDisabled]:=nil;
        if BitMapCreate(BitMap[bsDisabled],RectWidth(MPlayerRect),RectHeight(MPlayerRect)) then
           with BitMap[bsDisabled] do begin
             R1:=Rect(0,0,Width,Height);
             R2:=MPlayerRectToBackRect(MPlayerRect);
             Canvas.CopyMode:=cmSrcCopy;
             Canvas.CopyRect(R1,OrgBitMap.Canvas,R2);
             AlphaBlend(BitMap[bsDisabled],Self.BackBitMap,nil,R1,R2,Display.TransparencyPct,0,False);
             Self.BitMap.Canvas.CopyRect(R2,BitMap[bsDisabled].Canvas,R1);
             BitMap[bsDisabled].Free; BitMap[bsDisabled]:=nil;
             end;
        end;

     Self.Canvas.CopyMode:=cmSrcCopy;
     Self.Canvas.Draw(Left,Top,BitMap);

     DisplayRect:=MPlayerRectToScreenRect(Buttons[Ord(btDisplay)].MPlayerRect);
     if Display<>nil then Display.Initialize(DisplayRect);
{
     with Buttons[Ord(btDisplay)] do begin
       Visible:=(Display<>nil) and (Display.Activity=ecImage);
       if   Visible then Hint:=MPlayerImageViewerButtonHintText
       else Hint:='';
       end;
}
     Self.ShowStatus;
     MainForm.ShowStatus;
     end;
end;

procedure TMPlayer.Hide;
begin
  if Visible and
     (Screen.ActiveForm<>MPlayerForm) then begin
     Visible:=False; IsIdle:=False;
     Application.OnMessage:=oOnMessage;
     Application.OnIdle   :=oOnIdle;

     Viewer1.Suspend;
     if Fractals<>nil then Fractals.ResetDragRect;
     if Mandala <>nil then Mandala.Suspend;
     if Display <>nil then Display.HideMenu(True);

     Self.Canvas.CopyRect(BackRect,BackBitMap.Canvas,BitMapRect);
     MainForm.Status.Hint:='';

     DestroyMenuButtons(False);

     with MainForm.Music do begin
       if Player<>nil then Player.OnNotify:=PlayerNotify;
       UpdateStatus(oMusicEnabled);
       end;

     if MainForm.Deadlocks<>nil then MainForm.Deadlocks.Resume;

     MainForm.ShowStatus;
     MainForm.UpdateCursor;
     end;
end;

procedure TMPlayer.DrawRect(const Rect:TRect; PenMode:TPenMode);
const PEN_WIDTH=2;
begin
  with Self.Canvas do begin
     Brush.Style := bsClear; // note: this must precede the other settings; otherwise there is screen-update problems with the first call (reason deeply buried in VCL)
     Pen.Color   := Brush.Color;
     Pen.Mode    := PenMode;
     //Pen.Style   := psDot;
     Pen.Width   := PEN_WIDTH;

     DragRect := Rect;
     with DragRect do RoundRect(Left+2,Top+2,Right-2,Bottom-2,Width div 4,Height div 4);
     end;
end;

procedure TMPlayer.EnsureVisiblePosition(var Left,Top,Right,Bottom:Integer);
begin
  Left:=Max(MainForm.GraphicMenuWidth,Min(Left,MainForm.Image1.ClientWidth-40));
  Top :=Max(40-Height,Min(Top,MainForm.Status.RectForm.Top-2-Height));
  Right:=Left+Self.Width; Bottom:=Top+Self.Height;
end;

function  TMPlayer.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer):Boolean;
var ClickedButton:Integer;
begin
  if Dragging then MouseUp(Sender,Button,Shift,X,Y);
  DragPoint.X:=X; DragPoint.Y:=Y;

  if Button=mbLeft then begin
     Result:=MouseMove(Self,Shift,X,Y);
     ItemIndex:=ItemAtPos(X,Y);
     if Result and
        (ItemIndex>=0) and
        (Buttons[ItemIndex].State<>bsDisabled) then
        if   (ItemIndex=Ord(btDisplay)) and (Display<>nil) then begin
             if (Display.Menu<>nil) and Display.Menu.Visible then
                ClickedButton:=Click(mbLeft)
             else ClickedButton:=-1;
             if Display.Activity=acFractals then begin
                if ClickedButton<0 then begin
                   if (Display.Menu<>nil) and
                      Display.Menu.Visible then Display.Menu.Hide
                   else Fractals.MouseDown(Sender,Button,Shift,X,Y)
                   end;
                end
             else if (Display.Activity=acMandala) and (ClickedButton<0) then
                     Mandala.ToggleSuspended;
             end
        else Click(mbLeft);
     end
  else if Button=mbRight then
          if   (ItemAtPos(X,Y)=Ord(btDisplay)) and
               (Buttons[Ord(btDisplay)].State in [bsEnabled,bsFocused]) and
               (Display<>nil) then begin
               if      (Display.Menu<>nil) and Display.Menu.Visible then
                       if      Display.Activity=acFractals then
                               Display.HideMenu(False)
                       else if Display.Activity=acMandala then
                               Mandala.ToggleSuspended
                       else Click(mbRight)
               else if Display.Activity=acFractals then
                       if (Fractals.ZoomList<>nil) and (Fractals.ZoomList.Prev=nil) and
                          (Display.Menu<>nil) and (not Display.Menu.Visible) and
                          (Fractals.DragRect.Left=Fractals.DragRect.Right) then
                          Display.ShowMenu
                       else Fractals.MouseDown(Sender,Button,Shift,X,Y)
               else if (Display.Menu<>nil) and (not Display.Menu.Visible) then
                       Display.ShowMenu;
               Result:=True;
               end
          else begin Hide; Result:=True; end
       else if (Button=mbMiddle) and (Display<>nil) and
               (Display.Activity=acFractals) and
               (Fractals<>nil) and Fractals.ColorCyclingEnabled then begin
               Fractals.ColorCycling:=not Fractals.ColorCycling;
               Result:=True;
               end
            else Result:=False;
end;

function  TMPlayer.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer):Boolean;
begin
  if Dragging then begin
     Result:=True;

     if ActiveButton=btNone then with DragRect do begin
        Viewer1.Suspend; Mandala.Suspend;
        if Screen.Cursor<>crDrag then Screen.Cursor:=crDrag;
        if Left<Right then DrawRect(DragRect,pmXor);
        Left:=Self.Left+X-DragPoint.X;
        Top :=Self.Top +Y-DragPoint.Y;
        EnsureVisiblePosition(Left,Top,Right,Bottom);
        if Left<Right then DrawRect(DragRect,pmXor);
        end
     else if ActiveButton=btPosition then with Buttons[Ord(btPosition)] do begin
        if Screen.Cursor<>crDrag then Screen.Cursor:=crDrag;
        ButtonMoveTo(btPosition,X-DragPoint.X,Y-DragPoint.Y);
        MainForm.Status.Hint:=PositionText+SPACE+StrWithBrackets(MainForm.Music.PositionToString(
          (Min(Max(X,PositionScreenLeft),PositionScreenLeft+PositionBarWidth) - PositionScreenLeft) * Abs(MainForm.Music.LengthInMilliSeconds) div PositionBarWidth));
        end
     else if ActiveButton=btVolume   then with Buttons[Ord(btVolume)] do begin
        if Screen.Cursor<>crDrag then Screen.Cursor:=crDrag;
        ButtonMoveTo(btVolume,X-DragPoint.X,Y-DragPoint.Y);
        X:=MPlayerRect.Left; MPlayerXYtoFormXY(X,Y);
        MainForm.Music.Volume64Kibi:=(Succ(X)-Ord(X=VolumeScreenLeft)-VolumeScreenLeft)*65535 div VolumeBarWidth;; // Kludge: 'Ord(X=...)' allows [0%..100%], and not [1%..100%]
        MainForm.Status.Hint:=SoundVolumeText+SPACE+MainForm.Music.Volume64KibiToString(MainForm.Music.Volume64Kibi);
        end;
     end
  else begin
     ItemIndex:=ItemAtPos(X,Y);

     if ItemIndex=Ord(btPositionBar) then
        MainForm.Status.Hint:=Buttons[Ord(btPositionBar)].Hint+'  '+
          StrWithBrackets(MainForm.Music.PositionToString(
            (Min(Max(X-(RectWidth(Buttons[Ord(btPosition)].MPlayerRect) div 2),
                     PositionScreenLeft),
                 Pred(PositionScreenLeft+PositionBarWidth)) - PositionScreenLeft)
            * Abs(MainForm.Music.LengthInMilliSeconds)
            div PositionBarWidth))
     else if (ItemIndex=Ord(btDisplay)) then
             if Display<>nil then
                if (Display.Menu<>nil) and Display.Menu.Visible then
                   Display.Menu.MouseMove(X-DisplayRect.Left,Y-DisplayRect.Top)
                else if (Display.Activity=acFractals) then
                        Fractals.MouseMove(Sender,Shift,X,Y);

     FormXYtoMPlayerXY(X,Y);
     Result:=(X>=0) and (X<Width) and (Y>=0) and (Y<Height) and
             ((MaskBitMap=nil) or
              (MaskBitMap.Canvas.Pixels[X,Y]=COLOR_BLACK));
     end;
end;

procedure TMPlayer.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Dragging then begin
     Dragging:=False;
     ResetFps;
     if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;

     if ActiveButton=btNone then with DragRect do begin
        if Left<Right then begin
           DrawRect(DragRect,pmXor);
           MoveTo(Left,Top);
           FillChar(DragRect,SizeOf(DragRect),0);
           end;
        end
     else if (ActiveButton=btPosition) then with Buttons[Ord(btPosition)] do begin
                X:=MPlayerRect.Left; MPlayerXYtoFormXY(X,Y);
                MainForm.Music.PositionMilliSeconds:=
                  (X - PositionScreenLeft) * Abs(MainForm.Music.LengthInMilliSeconds) div PositionBarWidth;
                end
     else if ActiveButton=btVolume then with Buttons[Ord(btVolume)] do begin
             X:=MPlayerRect.Left; MPlayerXYtoFormXY(X,Y);
             MainForm.Music.Volume64Kibi:=(Succ(X)-Ord(X=VolumeScreenLeft)-VolumeScreenLeft)*65535 div VolumeBarWidth;
             ShowVolume;
             end;
     end;

  ActiveButton:=btNone;
  MainForm.Status.Hint:='';
  if ItemIndex>=0 then
     ShowButton(ItemIndex,Buttons[ItemIndex].LinkedButton,-1,Buttons[ItemIndex].State,True,False);

  if Display<>nil then
     if (Display.Menu<>nil) and Display.Menu.Visible then with Display.Menu do
        if ItemIndex>=0 then ShowButton(ItemIndex,MenuItems[ItemIndex].State,True)
        else
     else if (ItemIndex=Ord(btDisplay)) and (Display.Activity=acFractals) then
             Fractals.MouseUp(Sender,Button,Shift,X,Y);
end;

function  TMPlayer.ItemAtPos(X,Y:Integer):Integer;
var a,b,c,d:Integer; SwPartition:Boolean; p:TPoint;
begin
  FormXYtoMPlayerXY(X,Y); p:=Point(X,Y);

  if (ItemIndex>Ord(btNone)) and
     PtInRect(Buttons[ItemIndex].MPlayerRect,p) and
     (Buttons[ItemIndex].LinkedButton=0) and
     Buttons[ItemIndex].Visible then
     Result:=ItemIndex
  else
     if PtInRect(Buttons[Ord(btNone)].MPlayerRect,p) then begin
        a:=Low(SortedButtons)+1; // +1: 'btNone'=background fills the first slot
        b:=High(SortedButtons); SwPartition:=True;

        // this algorithm is broken when buttons overlap in the y-axis direction;
        // but with the extra check for the display-area,
        // it happens to do the job for the currently used layout
        while a<=b do begin
          if SwPartition then c:=(a+b) div 2
          else c:=a;
          d:=Ord(SortedButtons[c]);
          with Buttons[d].MPlayerRect do
            if        Y< Top    then b:=Pred(c)
            else if   Y>=Bottom then a:=Succ(c)
            else if   (X>=Left) and (X<Right) then begin
                      if      (d=Ord(btPositionBar)) and
                              PtInRect(Buttons[Ord(btPosition)].MPlayerRect,p) then
                              d:=Ord(btPosition)
                      else if (d=Ord(btVolumeBar)) and
                              PtInRect(Buttons[Ord(btVolume)].MPlayerRect,p) then
                              d:=Ord(btVolume);

                      if (not Buttons[d].Visible) and (d<>Ord(btDisplay)) then
                         if      Buttons[Ord(btPlay     )].Visible then d:=Ord(btPlay)
                         else if Buttons[Ord(btPlayPause)].Visible then d:=Ord(btPlayPause);
                      Result:=d; exit;
                      end
            else if   SwPartition then
                      SwPartition:=False
                 else Inc(a);
          end;
        if   PtInRect(Buttons[Ord(btDisplay)].MPlayerRect,p) then
             Result:=Ord(btDisplay)
        else Result:=Ord(btNone);
        end
     else Result:=-1;
end;

procedure TMPlayer.SetItemIndex(ItemIndex__:Integer);
var DisplayMenuVisible:Boolean;
begin
  if ItemIndex__<>ItemIndex then begin
     DisplayMenuVisible:=(Display<>nil) and (Display.Menu<>nil) and Display.Menu.Visible;
     if ItemIndex>=0 then
        ShowButton(ItemIndex,Buttons[ItemIndex].LinkedButton,ItemIndex__,bsEnabled,False,False);
     LastItemIndex:=ItemIndex;
     fItemIndex:=ItemIndex__;
     if ItemIndex>=0 then
        ShowButton(ItemIndex,Buttons[ItemIndex].LinkedButton,-1,bsFocused,True,DisplayMenuVisible and (not Display.Menu.Visible));
     if (ItemIndex>Ord(btDisplay)) and
        (MainForm.Sound.Enabled) and
        Buttons[ItemIndex].Visible and
        (Buttons[ItemIndex].State=bsFocused) and
        Visible
        then
        MainForm.Sound.Play(stMenuOver);
     end;
end;

procedure TMPlayer.ShowButtons;
var i:Integer;

  procedure EnableDisableButtons;
  var AMode:TMPModes;
  begin
    SleepEx(2,False); // to update mode ?
    if   (MainForm.Music<>nil) and
         (MainForm.Music.Player<>nil) and
         (MainForm.Music.Player.FileName<>'') and
         FileExists(MainForm.Music.Player.FileName) and
         MainForm.Music.ModeOk(MainForm.Music.Player.Mode) then
         AMode:=MainForm.Music.Player.Mode
    else AMode:=mpNotReady;
    //MainForm.Status.Hint:=MediaPlayerStateText[Mode];

    Buttons[Ord(btDisplay  )].Visible:= (Display<>nil) and (Display.Menu<>nil);
{
                                        (((Display.Activity=acNone )     and (Display.Menu<>nil))
                                         or
                                         ((Display.Activity=acImage)     and (Viewer1<>nil))
                                         or
                                         ((Display.Activity=acFractals)  and (Fractals<>nil))
                                         or
                                         ((Display.Activity=acFireworks) and (Fireworks<>nil))
                                        );
}
    Buttons[Ord(btPlayPause)].Visible:= AMode=mpPlaying;
    Buttons[Ord(btPlay     )].Visible:=not Buttons[Ord(btPlayPause)].Visible;

    with Buttons[Ord(btDisplay    )] do if    Visible          then State:=bsEnabled  else State:=bsDisabled;
    with Buttons[Ord(btPlay       )] do if    AMode in [mpPlaying,mpNotReady,mpOpen]  // 'mpOpen' means 'Tray is open', not 'File is open and ready'
                                                               then State:=bsDisabled else State:=bsEnabled;
    with Buttons[Ord(btPlayPause  )] do if    Visible          then State:=bsEnabled  else State:=bsDisabled;

    with Buttons[Ord(btStop       )] do if    AMode=mpPlaying  then State:=bsEnabled  else State:=bsDisabled;
    with Buttons[Ord(btPositionBar)] do if    AMode=mpNotReady then State:=bsDisabled else State:=bsEnabled;
    with Buttons[Ord(btPosition   )] do if    AMode=mpNotReady then State:=bsDisabled else State:=bsEnabled;

    with Buttons[Ord(btPrev       )] do
      if (MainForm.Music.PlayList=nil) or
         (MainForm.Music.PlayList.Count<=1)                    then State:=bsDisabled else State:=bsEnabled;
    with Buttons[Ord(btNext       )] do
      if (MainForm.Music.PlayList=nil) or
         (MainForm.Music.PlayList.Count<=1)                    then State:=bsDisabled else State:=bsEnabled;
  end;

begin // ShowButtons
  EnableDisableButtons;

  ActiveButton:=btNone; ShowPosition;
  with Buttons[Ord(btPosition)] do
    if State=bsDisabled then ButtonMoveTo(btPosition,PositionScreenLeft,0);

  ShowVolume;

  if (ItemIndex>=0) and (not Buttons[ItemIndex].Visible) then
     if      Buttons[Ord(btPlay     )].Visible then fItemIndex:=Ord(btPlay)
     else if Buttons[Ord(btPlayPause)].Visible then fItemIndex:=Ord(btPlayPause);

  for i:=Low(Buttons) to High(Buttons) do
      if   i=ItemIndex then
           ShowButton(i,Buttons[i].LinkedButton,-1,bsFocused,False,False)
      else ShowButton(i,Buttons[i].LinkedButton,-1,Buttons[i].State,False,False);

//if ItemIndex<=Ord(btDisplay) then MainForm.Status.Hint:='';
end;

procedure TMPlayer.ShowButton(Index,Link,NextItemIndex:Integer; ButtonState:TButtonState; ShowHint,DisplayMenuJustClosed:Boolean);
var b:TButtonState; BackRect,DestRect,R:TRect;
begin
  if (Index>=Low(Buttons)) and (Index<=High(Buttons)) then with Buttons[Index] do begin
     if Visible then begin
        if   State=bsDisabled then b:=bsDisabled
        else b:=ButtonState;

        if Index<=Ord(btDisplay) then begin
           if Index=Ord(btDisplay) then begin
              State:=b;
              if (Display<>nil) and (Display.Menu<>nil) then
                 if   State=bsFocused then
                      if Display.Activity<>acFractals then Display.ShowMenu
                      else
                 else Display.HideMenu(False);
              end;
           if ShowHint then MainForm.Status.Hint:=Hint
           else ;//MainForm.Status.Hint:=''
           end
        else begin

           BackRect:=MPlayerRectToBackRect(MPlayerRect);

           if (BitMap[b]=nil) and  // images are created on demand
              BitMap_.BitMapCreate(BitMap[b],RectWidth(MPlayerRect),RectHeight(MPlayerRect)) then
              Exclude(BitMapSet,b);

           if BitMap[b]<>nil then with BitMap[b] do begin
              State:=b;
              R:=Rect(0,0,Width,Height);

              if Transparency and Masked then Exclude(BitMapSet,b);

              if not (b in BitMapSet) then begin

                 Canvas.CopyMode:=cmSrcCopy;
                 Canvas.CopyRect(R,SourceBitMap.Canvas,SourceRect[b]);

                 if Masked and (MaskBitMap[b]=nil) then
                    BitMapCreateMask(BitMap[b],MaskBitMap[b],MaskColor,MaskPct);

                 if MaskBitMap[b]=nil then Masked:=False;

                 if   Transparency then
                      AlphaBlend(BitMap[b],BackBitMap,nil,R,BackRect,TransparencyPct,0,False)
                 else AlphaBlend(BitMap[b],BackBitMap,nil,R,BackRect,0,0,False);

                 Include(BitMapSet,b);
                 end;

              if (BackRect.Right<=Self.Width) and
                 (BackRect.Bottom<=Self.Height) and
                 Visible and Self.Visible then begin
                 DestRect:=MPlayerRectToScreenRect(MPlayerRect);
                 if Masked then begin
                    MaskBitMap[b].Canvas.CopyMode:=cmDstInvert;
                    MaskBitMap[b].Canvas.CopyRect(R,BackBitMap.Canvas,R);

                    Canvas.CopyMode:=cmSrcAnd;
                    Canvas.CopyRect(R,MaskBitMap[b].Canvas,R);

                    MaskBitMap[b].Canvas.CopyMode:=cmDstInvert;
                    MaskBitMap[b].Canvas.CopyRect(R,BackBitMap.Canvas,R);

                    Self.Canvas.CopyMode:=cmSrcAnd;   // and-ing:
                    Self.Canvas.CopyRect(DestRect,MaskBitMap[State].Canvas,R);
                    Self.Canvas.CopyMode:=cmSrcPaint; // or-ing:
                    end
                 else Self.Canvas.CopyMode:=cmSrcCopy;// copying:

                 Self.Canvas.CopyRect(DestRect,BitMap[State].Canvas,R);
                 Self.Canvas.CopyMode:=cmSrcCopy;
                 end;

              if (Link>0) and (Link<MaxInt-1) then
                 ShowButton(Link,MaxInt,-1,Buttons[Abs(Link)].State,False,False);

              if ShowHint and (not Dragging) and Visible and Self.Visible then
                 MainForm.Status.Hint:=Hint;
              end;

           if (Index=Ord(btActivityMenu)) and
              (Display<>nil) and (Display.Menu<>nil) then
              if   (State=bsFocused) and
                   ((LastItemIndex<>Ord(btDisplay))
                    or
                    ((Display.Activity=acFractals)
                     and
                     (not DisplayMenuJustClosed)
                    )
                   ) then
                   Display.Menu.Show
              else if NextItemIndex<>Ord(btDisplay) then
                      Display.Menu.Hide;
           end;
        end;
     end
  else if ShowHint then MainForm.Status.Hint:='';
end;

function  TMPlayer.Click(MouseButton:TMouseButton):Integer;
var X,X0,DX:Integer; s,s1,s2:String;
    oOnActivate,oOnDeactivate,oOnHint: TNotifyEvent; oOnMessage:TMessageEvent;
begin
  Result:=-1; Dragging:=False;
  if ItemIndex>=0 then with Buttons[ItemIndex] do
     if (State<>bsDisabled) then begin
        Result:=ItemIndex;
        if ItemIndex<>Ord(btDisplay) then with MainForm.Sound do begin
          if Enabled then begin StopAll; Play(stMenuSelect); end;
          Self.ItemIndex:=-1;
          end;

        ActiveButton:=TMPlayerButtonType(Result);

        case ActiveButton of
          btNone         : begin Dragging:=True; // 'btNone': actually, it's the background
                                 FillChar(DragRect,SizeOf(DragRect),0);
                           end;
          btDisplay      : if (Display<>nil) and (Display.Menu<>nil) and Display.Menu.Visible then begin
                              if   MouseButton=mbLeft then Result:=Display.Menu.Click
                              else Result:=Ord(btDisplay);
                              end;
          btPositionBar  : begin with Buttons[Ord(btPosition)] do
                                   if State=bsEnabled then begin
                                      ButtonMoveTo(btPosition,
                                                   DragPoint.X-(RectWidth (MPlayerRect) div 2),
                                                   DragPoint.Y-(RectHeight(MPlayerRect) div 2));
                                      MainForm.Music.PositionMilliSeconds:=
                                        (DragPoint.X - (RectWidth(MPlayerRect) div 2) - PositionScreenLeft) * Abs(MainForm.Music.LengthInMilliSeconds) div PositionBarWidth;
                                      end;
                           end;
          btPosition     : begin with Buttons[Ord(btPosition)] do
                                   if State=bsEnabled then begin
                                      Dragging:=True; SetDragPointToButtonCenterOffset(btPosition);
                                      ItemIndex:=Result;
                                      end;
                           end;

          btPlay         : if MainForm.Music<>nil then with MainForm.Music do
                              if (Player<>nil) and
                                 ModeOk(Player.Mode) and (Player.Mode<>mpNotReady) then begin
                                 Play; Self.ShowStatus;
                                 end;

          btStop         : Stop;
          btPrev         : if MainForm.Music<>nil then with MainForm.Music do begin
                              PrevTrack; Self.ShowStatus;
                              end;
          btNext         : if MainForm.Music<>nil then with MainForm.Music do begin
                              NextTrack; Self.ShowStatus;
                              end;
          btVolumeBar    : begin FormXYtoMPlayerXY(DragPoint.X,DragPoint.Y);
                                 DX:=VolumeBarWidth div 10;
                                 with Buttons[Ord(btVolume)] do begin
                                   X0:=Buttons[Ord(btVolumeBar)].MPlayerRect.Left{-(RectWidth(MPlayerRect) div 2)};
                                   X :=((MPlayerRect.Left-X0) div DX)*DX+
                                       Misc_.Sign(DragPoint.X-MPlayerRect.Left)*DX+
                                       X0;
                                   MPlayerXYtoFormXY(X,DragPoint.Y);
                                   MainForm.Music.Volume64Kibi:=(Max(0,Min(Succ(X),VolumeScreenLeft+VolumeBarWidth)-Ord(X=VolumeScreenLeft)-VolumeScreenLeft))*65535 div VolumeBarWidth;
                                   ShowVolume;
                                   end;
                           end;
          btVolume       : begin Dragging:=True; SetDragPointToButtonCenterOffset(btVolume);
                                 ItemIndex:=Result;
                           end;

          btPlayPause    : if MainForm.Music<>nil then begin
                              IsIdle:=False;
                              MainForm.Music.Stop(True);
                              ShowStatus;
                              end;
          btOpen         : begin
                             if MainForm.Music<>nil then with MainForm.Music do begin
                                IsIdle:=False; Viewer1.Suspend; Mandala.Suspend;
                                if MusicSource<>msPlayList then UpdateFileList(MusicSource);
                                if   (Player<>nil) and (FileExists(Player.FileName)) then s:=Player.FileName
                                else if (TrackFileName<>'') and (FileExists(TrackFileName)) then s:=TrackFileName
                                else if PlayList<>nil then with PlayList do begin
                                        if   (Count=0) and (MusicSource<>msFolder) then
                                             UpdateFileList(msFolder);
                                        if   Count=0 then s:=''
                                        else s:=FullFileName(Max(0,Pred(TrackNo)));
                                        end;

                                Write('',Buttons[Ord(btDisplay)].MPlayerRect,DT_CENTER);
                                ButtonMoveTo(btPosition,PositionScreenLeft,0);

                                s1:=STAR+PLAYLIST_FILE_EXT; s2:=MainForm.Music.MusicFileTypeFilter;
                                if AnsiPos(s1,s2)=0 then
                                   if   s2='' then s2:=s1
                                   else s2:=s2+SEMICOLON+s1;

                                //OpenForm.InitTask(otMusicPlayer,MusicPlayerTitleText,MusicPlayerTitleText,s,s2,Rect(0,0,0,0),clBlack,False,Rect(0,0,0,0));

                                oOnActivate  :=Application.OnActivate;
                                oOnDeactivate:=Application.OnDeactivate;
                                oOnHint      :=Application.OnHint;
                                oOnMessage   :=Application.OnMessage;
                                try     Application.OnActivate  :=nil;
                                        Application.OnDeactivate:=nil;
                                        if MPlayerForm.ShowModal=mrOk then begin
                                           end;
                                finally Application.OnMessage   :=oOnMessage;
                                        Application.OnActivate  :=oOnActivate;
                                        Application.OnDeactivate:=oOnDeactivate;
                                        Application.OnHint      :=oOnHint;
                                        if (MainForm.Music<>nil) then with MainForm.Music do
                                           if Player<>nil then Player.OnNotify:=MPlayerNotify;
                                        Screen.Cursor:=DEFAULT_CURSOR;
                                end;
                                if Visible then begin // otherwise, 'MainForm' closed it behind the musicplayer's back during 'MPlayerForm.ShowModal'
                                   if Display<>nil then begin
                                      Display.Text:='';
                                      if Display.Activity=acImage then begin
                                         Display.RestoreImageIfFading;
                                         Viewer1.ShowImage(Viewer1.FileName)
                                         end
                                      else if Display.Activity=acMandala then
                                         Mandala.Resume;
                                      end;
                                   Self.ShowStatus;
                                   end;
                                end;

                           end;
          btActivityMenu : begin with Buttons[Ord(btActivityMenu)].SourceRect[bsEnabled] do
                                   //MainForm.PopupMenuMPlayer.Popup(Left,Top);
                           end;
          btActivityPrev : begin if Display<>nil then with Display do begin
                                    IsIdle:=False;
                                    FpsOk:=True;
                                    Viewer1.PictThread.FadeEnabled:=True;
                                    //MainForm.Music.UpdateStatus(False);
                                    if   Activity<=Low(Activity) then //Succ(acNone) then
                                         Activity:=High(Activity)
                                    else Activity:=Pred(Activity);
                                    Self.ShowStatus;
                                    end;
                           end;
          btActivityNext : begin if Display<>nil then with Display do begin
                                    IsIdle:=False;
                                    FpsOk:=True;
                                    Viewer1.PictThread.FadeEnabled:=True;
                                    //MainForm.Music.UpdateStatus(False);
                                    if   Activity>=High(Activity) then
                                         Activity:=Low(Activity) //Succ(acNone)
                                    else Activity:=Succ(Activity);
                                    Self.ShowStatus;
                                    end;
                           end;
          btHelp         : begin MainForm.BtnHelpClick(MainForm);
                           end;
          btExit         : begin Hide;
                           end;
        end; // case
        end;
end;

procedure TMPlayer.ButtonMoveTo(Button:TMPlayerButtonType; X,Y:Integer);
var R:TRect;
begin
  if not IsBusy then with Buttons[Ord(Button)] do
    try
      IsBusy:=True;

      FormXYtoMPlayerXY(X,Y);

      if (Button=btPosition) or (Button=btVolume) then Y:=MPlayerRect.Top;

      if PtInRect(BoundsRect,Point(X,Y)) then begin

         R:=MPlayerRect;
         MPlayerRect:=Rect(X,Y,X+RectWidth(R),Y+RectHeight(R));

         if {MPlayer:} Self.Visible and {Button:} Visible then begin
            Self.Canvas.CopyRect(
              MPlayerRectToScreenRect(R),
              Self.BitMap.Canvas,
              MPlayerRectToBackRect(R));

            if Transparency then BitMapSet:=[];

            if   (LinkedButton<0) and (Buttons[-LinkedButton].State=bsFocused) then
                 ShowButton(-LinkedButton,Ord(Button) ,-1,bsFocused,False,False)
            else ShowButton(Ord(Button)  ,LinkedButton,-1,State    ,False,False);
            end;
         end;
    finally IsBusy:=False;
    end;
end;

procedure TMPlayer.FormXYtoMPlayerDisplayXY(var X,Y:Integer);
begin
  FormXYtoMPlayerXY(X,Y);
  Dec(X,Buttons[Ord(btDisplay)].MPlayerRect.Left);
  Dec(Y,Buttons[Ord(btDisplay)].MPlayerRect.Top);
end;

procedure TMPlayer.FormXYtoMPlayerXY(var X,Y:Integer);
begin
  Inc(X,TileRect.Left-Self.Left); Inc(Y,TileRect.Top-Self.Top);
end;

procedure TMPlayer.MPlayerXYtoFormXY(var X,Y:Integer);
begin
  Dec(X,TileRect.Left-Self.Left); Dec(Y,TileRect.Top-Self.Top);
end;

function  TMPlayer.MPlayerRectToScreenRect(const R:TRect):TRect;
begin
  Result:=R;
  MPlayerXYtoFormXY(Result.Left ,Result.Top);
  MPlayerXYtoFormXY(Result.Right,Result.Bottom);
end;

function  TMPlayer.MPlayerRectToBackRect(const R:TRect):TRect;
begin
  with Result do begin
    Left :=R.Left -TileRect.Left; Top   :=R.Top-TileRect.Top;
    Right:=R.Right-TileRect.Left; Bottom:=R.Bottom-TileRect.Top
    end;
end;

procedure TMPlayer.SetDragPointToButtonCenterOffset(Button:TMPlayerButtonType);
begin
  FormXYtoMPlayerXY(DragPoint.X,DragPoint.Y);
  with Buttons[Ord(Button)].MPlayerRect do begin
    Dec(DragPoint.X,Left);
    Dec(DragPoint.Y,Top);
  end;
end;

procedure TMPlayer.MPlayerNotify(Sender: TObject);
begin
  if MainForm.Music.Player<>nil then begin
     if not Visible then MainForm.Music.Player.OnNotify:=MainForm.Music.PlayerNotify;
     MainForm.Music.PlayerNotify(MainForm.Music.Player);
     if not MainForm.Music.Playing then IsIdle:=False;
     end;
end;

procedure TMPlayer.Write(const s:String; const R:TRect; Flags:Cardinal);
var R1:TRect;
begin
  if Visible then begin
     R1:=MPlayerRectToScreenRect(R);
     Canvas.CopyMode:=cmSrcCopy;
     Canvas.CopyRect(R1,BitMap.Canvas,MPlayerRectToBackRect(R));
     if MainForm.Image1.Tag<>LongInt(Addr(MPlayerDisplayPanel)) then begin
        MainForm.Image1.Tag:=LongInt(Addr(MPlayerDisplayPanel));
        Canvas.Font.Assign(MPlayerDisplayPanel.Font);
        end;
     Windows.SetBkMode (Canvas.Handle, Windows.TRANSPARENT);
     Windows.DrawTextEx(Canvas.Handle,PChar(s),Length(s),R1,Flags or DT_CENTER or DT_VCENTER or DT_WORDBREAK {or DT_SINGLELINE},nil);
     Windows.SetBkMode (Canvas.Handle, Windows.OPAQUE);
     end;
end;

procedure TMPlayer.ShowStatus;
var s:String; oDisplayMenuVisible:Boolean;
begin
  oDisplayMenuVisible:=(Display<>nil) and (Display.Menu<>nil) and (Display.Menu.Visible);

  Viewer1.PictThread.FadeEnabled:=FpsOk;

  if Dragging then // cancelling any drag operation in progress seems the only reasonable thing to do here, since the track may have changed; otherwise at least dragging the musicplayer itself neeeds repair
     MouseUp(Self,mbLeft,[],0,0);

  ShowButtons; ResetFps;
  with MainForm.Music do begin
    if (Player<>nil) and ModeOk(Player.Mode) and
       ((Player.Mode=mpPlaying) or (Player.Mode=mpStopped) or (Player.Mode=mpSeeking)) and
       FileExists(Player.FileName) then begin
       //MainForm.Caption:=Format(PlayingFileText__,[ExtractFileNameWithoutExt(Player.FileName)]);
       s:=PrettyTrackName(Player.FileName);
       Animation:=FpsOk;
       if Display<>nil then Display.TitleAnimation:=(Player.Mode=mpPlaying) and Animation;
       end
    else
       if   (Buttons[Ord(btPlay)].Visible) and
            (Buttons[Ord(btPlay)].State=bsDisabled) then begin
            s:=ClickOpenText;
            Animation:=FpsOk;
            if Display<>nil then Display.TitleAnimation:=Animation;
            end
       else begin s:='';
                  Animation:=False;
                  if Display<>nil then Display.TitleAnimation:=False;
            end;

    if Display<>nil then begin
       Animation := Animation
                    or
                    (not (Display.Activity in [acNone,acImage]))
                    or
                    ((Display.Activity=acImage)
                     and
                     (Viewer1.PictThread<>nil)
                     and
                     Viewer1.PictThread.SlideShow
                    );
       if not Animation then Display.TitleAnimation:=False;

       Display.Text:=s;
       if not Animation then begin
          IsIdle:=False; Display.ShowText;
          end;
       if oDisplayMenuVisible then Display.Menu.Show;
       PostMessage(MainForm.Handle,MSG_MPLAYER_ON_IDLE,0,0);
       end;
    end;
end;

procedure TMPlayer.ShowPosition;
var Pos,Len:Integer; AMode:TMPModes;
begin
  if (ActiveButton<>btPosition) and (not Dragging) then with MainForm.Music do begin
     if   Player<>nil then AMode:=Player.Mode
     else AMode:=mpNotReady;
     if not ModeOk(AMode) then AMode:=mpNotReady;

     if   (Playing) or
          ((AMode<>mpNotReady) and (AMode<>mpOpen)) then begin
          Pos:=PositionMilliSeconds; Len:=Abs(LengthInMilliSeconds);
          ButtonMoveTo(btPosition,PositionScreenLeft+Pos*PositionBarWidth div Len,0);
          end
     else if (Player=nil) or (AMode=mpNotReady) or (AMode<>mpStopped) then begin
          Pos:=0; Len:=1;
          //Buttons[Ord(btPosition)].State:=bsDisabled;
          ButtonMoveTo(btPosition,PositionScreenLeft+Pos*PositionBarWidth div Len,0);
          end;
     end;
end;

procedure TMPlayer.ShowVolume;
begin
  if not Dragging then begin
     ButtonMoveTo(btVolume,VolumeScreenLeft+(VolumeBarWidth*MainForm.Music.Volume64Kibi div 65536),0); // '65536' and not '65535': because 'PtInRect' excludes 'Rect.Right'
     end;
end;

procedure TMPlayer.ResetFps;
begin
  Fps:=0; TickCount:=High(TickCount); // recalculates 'fps' from scratch
end;

procedure TMPlayer.OnIdle;
//const MPLAYER_FRAME_TIME_FAST_MS=20; // local for quick test - remove in production version
//const MPLAYER_FRAME_TIME_SLOW_MS=40; // local for quick test - remove in production version
//const MIN_FPS=26; // local for quick test - remove in production version
var Vol1,Vol2:Integer; Time,Time1,Time2,FrameTimeMS,FpsResetCount:Cardinal;
begin
  if   Display.FastSpeed then
       FrameTimeMS:=MPLAYER_FRAME_TIME_FAST_MS
  else FrameTimeMS:=MPLAYER_FRAME_TIME_SLOW_MS;
  TickCount:=0; TimeCount:=0; Fps:=0; FrameStartTime:=0; PositionFrameStartTime:=0;
  IsIdle:=True; Time1:=GetTickCount; Vol2:=MainForm.Music.Volume64Kibi; FpsResetCount:=0;
  repeat Time:=GetTickCount;
         if Time<Time1 then begin // clock wrap around
            FrameStartTime:=0; PositionFrameStartTime:=0; // reset triggers
            end;

         if Time>=FrameStartTime then begin
            FrameStartTime:=Time+FrameTimeMS; // start time for next frame

            if Time>=PositionFrameStartTime then begin
               PositionFrameStartTime:=Time+MPLAYER_POSITION_FRAME_TIME_MS;

               if MainForm.Music.Playing then ShowPosition;

               Vol1:=MainForm.Music.MasterVolume64Kibi;
               if Vol1<>Vol2 then begin
                  Vol2:=Vol1; MainForm.Music.Volume64Kibi:=Vol2;
                  ShowVolume;
                  end;

               with MainForm do
                 if (not Music.FileListOk)
                    and
                    Music.CheckIfFileAppeared then begin
                    Self.ShowStatus; IsIdle:=False;
                    end;

               end;

            if //(Display<>nil) and
               (not (Dragging and (ActiveButton=btNone))) and
               Animation and Visible then Display.Execute;

            Inc(TickCount);
            if TickCount=0 then begin TimeCount:=0; Time1:=GetTickCount; end;

            Time2:=GetTickCount;
            if Time2>Time1 then Inc(TimeCount,Time2-Time1);
            Time1:=Time2;

            if TimeCount>5000 then begin // Refresh 'frames per second' every 5 seconds
               Fps:=TickCount*1000 div TimeCount;
               TickCount:=0; TimeCount:=0; Inc(FpsResetCount);
               if (Fps<MIN_FPS) and
                  FpsOk and
                  Animation and
                  (not Dragging) and
                  (FpsResetCount>6) // > 6: give the program some time to try to improve the performance ('Mandala' needs this, because it fills the pattern-buffer on startup)
                  then begin
                  if (Display.Activity<>acNone) and
                     (Display.Activity<>acImage) then begin
                     Display.Activity:=acImage; // drop the current 'eye candy', and continue with 'Image Viewer'
                     if   Display.FastSpeed then
                          FrameTimeMS:=MPLAYER_FRAME_TIME_FAST_MS
                     else FrameTimeMS:=MPLAYER_FRAME_TIME_SLOW_MS;
                     ShowStatus;
                     if PtInRect(MainForm.FormRectToScreenRect(MainForm.MPlayer.DisplayRect),Mouse.CursorPos) then
                        if Viewer1.Menu<>nil then Viewer1.Menu.Show;
                     end
{
                  else if (Display.Activity=acImage) and
                          (Viewer1.PictThread<>nil) and
                          Viewer1.PictThread.SlideShow then begin
                          Viewer1.PictThread.SlideShow:=False;
                          if (Viewer1.Menu<>nil) and Viewer1.Menu.Visible then
                             Viewer1.Menu.Show;
                          end
}
                  else begin
                     // even 'Image Viewer' can not run with the minimum frame rate:
                     // drop title-animation and fading
                     FpsOk:=False;
                     //Animation:=False; // don't touch: 'Image Viewer' may have a fade in progress
                     Display.TitleAnimation:=False;
                     Viewer1.PictThread.FadeEnabled:=False;
                     {if Display<>nil then} Display.ShowText;
                     end;
                  end;
               end;
            end
         else
            if FrameStartTime-Time>1 then
               SleepEx(FrameStartTime-Time-1,False); // give rest of the time slice to the other threads, if any

         if IsIdle then Application.ProcessMessages;

  until  not (IsIdle {and MainForm.Music.Playing});
  IsIdle:=False;
end;

procedure TMPlayer.MPlayerOnIdle;
begin
  if (not IsIdle) and
     (Display<>nil) and // make some nil-checks now, so they aren't required for each idle-step
     (Display.BitMaps[Low(Display.BitMaps)]<>nil) and
     (Mandala<>nil) and (Viewer1<>nil) and (Viewer1.PictThread<>nil) and
     (Fireworks<>nil) and (Fractals<>nil) and
     (Screen.ActiveForm=MainForm) then OnIdle;
end;

procedure TMPlayer.Stop;
begin
  if MainForm.Music<>nil then with MainForm.Music do begin
     Stop(True);
     PositionMilliseconds:=0; // if position is set by a 'Rewind', the user cannot set the position manually before the musicplayer is started again; reason unknown
     IsIdle:=False;
     ButtonMoveTo(btPosition,PositionScreenLeft,0);
     if Self.Visible then Self.ShowStatus;
     oMusicEnabled:=False; // don't start 'MediaPlayer' automatically
     end;
end;

end.
