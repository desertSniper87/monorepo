unit Fractal_;

interface

uses Windows,Classes,Controls,ExtCtrls,Graphics,IniFile_,Menu_,Misc_;

type
  TFractalsButtonType = (fbPrior,fbReset,fbNext,fbColors,fbSaveAs,fbSettings);

  TPalette      = array[0..255] of TColor;
  TFloat        = Double; //Single;
  TZoomRect     = record Left,Top,Width,Height: TFloat; // [width,height]; not [right,bottom]
                  end;

  TGrabHandle   = (ghNull,
                   ghBottomLeft, ghLeft,  ghTopLeft,     ghTop,
                   ghTopRight,   ghRight, ghBottomRight, ghBottom);

  TFractalState = (stNull,stReady,stZoom,stReSize,stCalculate);

  PZoomListItem = ^TZoomListItem;
  TZoomListItem = record Rect:TZoomRect;
                         ImageWidth,ImageHeight:Integer;
                         Data:PByte;
                         Prev,Next:PZoomListItem;
                  end;


  TFractals = class
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
    ColorCount : Integer;
    DragOriginPoint : TPoint;
    DragPoint : TPoint;
    DragRectHeight,
    DragRectWidth : Integer;
    FColorCycling:Boolean;
    FHint : String;
    IsDragging,
    IsZooming : Boolean;
    Iterations : Integer;
    SizeHandle : TGrabHandle;
    ColorCyclingIntervalMS : Integer;
    FState : TFractalState;
    IsResizing : Boolean;
    DoZoomIn : Boolean;
    Zoom:TZoomRect;

    procedure   DisplayDimensions;
    procedure   FixedAspectRatio16x9(var Width,Height:Integer);
    procedure   Mandelbrot(const Zoom:TZoomRect; ImageWidth,ImageHeight:Integer);
    function    SaveImage(const FileName:String):Boolean;
    procedure   SetDefaultValues;
    procedure   ZoomIn;
    procedure   ZoomListDeleteList(Item:PZoomListItem);
    procedure   ZoomListInsert(const Zoom:TZoomRect; ImageWidth,ImageHeight:Integer);
    procedure   ZoomOut;
  protected
    procedure   SetColorCycling(ColorCycling__:Boolean);
    procedure   SetHint(const Hint__:String);
    procedure   SetState(State__:TFractalState);
  public
    AntiAliasing:Boolean;
    BitMap    : TBitMap; // kludge: should have been private; but 'OpenForm' uses it;
    ColorCyclingEnabled
              : Boolean;
    DragRect  : TRect;   // kludge: should have been private; but 'MPlayer.MouseDown' and 'MPlayer.Display.Execute' use it;
    Menu:TFractalsMenu;
    Palette   : TPalette;
    PaletteAuthor:String;
    PaletteCycled
              : Boolean;
    PaletteFileName:String;
    ZoomList  : PZoomListItem;
    constructor Create(MenuPanel__:TPanel);
    function    DefaultPaletteFileName:String;
    destructor  Destroy; override;
    procedure   DrawRect(const Rect:TRect; PenMode:TPenMode);
    procedure   Finalize;
    procedure   Initialize;
    function    LoadPalette(const FileName:String; SilentError:Boolean):Boolean; overload;
    function    LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
    procedure   MakeBitMap(ZoomList:PZoomListItem; BitMap:TBitMap);
    procedure   Next;
    procedure   Prior;
    procedure   Reset;
    procedure   ResetDragRect;
    procedure   SaveAs;
    function    SavePalette(const FileName:String; PascalSyntax:Boolean):Boolean;
    function    SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
    procedure   Show;
    function    UpdatePattern:Boolean;

    property    ColorCycling:Boolean read FColorCycling write SetColorCycling;
    property    Hint:String read FHint write SetHint;
    property    State:TFractalState read FState write SetState;
  end;

var
  Fractals : TFractals = nil;

implementation

uses Math,SysUtils,Forms,SokUtil_,Text_,Main_,BitMap_,Display_,Mandal1_;

const
  DEFAULT_COLOR_COUNT = 256;
  DEFAULT_PALETTE     : array[0..255,0..2] of byte = (

(  0,   0,   0),
(  0,   0, 188),
(  0, 188,   0),
(188, 188,   0),
(188,   0,   0),
(188,   0, 188),
(  0, 188, 188),
(192, 192, 192),
(192, 220, 192),
(164, 200, 240),
(128,   0,   0),
(  0, 128,   0),
(128, 128,   0),
(  0,   0, 128),
(128,   0, 128),
(  0, 128, 128),
( 44,   0,  90),
(135,   0, 141),
(190,   0, 113),
(238,   0,  37),
(255,  39,   0),
(255,  99,   0),
(255, 157,   0),
(255, 213,   0),
(121, 135, 255),
( 95, 178, 255),
( 69, 242, 255),
( 43, 255, 186),
( 18, 255,  86),
( 14,   0,  73),
(  0,   4, 108),
(  0,  38, 143),
(  0,  85, 176),
(  0, 146, 209),
(  0, 220, 241),
(  0, 231, 255),
(  0, 190, 255),
(  0, 149, 255),
(  0, 110, 255),
(  0,  71, 255),
(  0,  33, 255),
(  4,   0, 255),
( 41,   0, 255),
( 77,   0, 255),
(113,   0, 255),
(147,   0, 255),
(182,   0, 255),
(215,   0, 255),
(248,   0, 255),
( 35,   0,  83),
( 69,   0, 107),
(113,   0, 131),
(154,   0, 143),
(177,   0, 127),
(199,   0, 102),
(222,   0,  68),
(243,   0,  26),
(255,  13,   0),
(255,  42,   0),
(255,  69,   0),
(255,  97,   0),
(255, 124,   0),
(255, 151,   0),
(255, 178,   0),
(255, 204,   0),
(255, 229,   0),
(255, 255,   0),
(115, 143, 255),
(103, 162, 255),
( 91, 186, 255),
( 79, 215, 255),
( 67, 248, 255),
( 55, 255, 226),
( 43, 255, 185),
( 31, 255, 141),
( 20, 255,  93),
(  9, 255,  42),
( 15,   0,  68),
( 10,   0,  84),
(  1,   0, 101),
(  0,  11, 117),
(  0,  27, 133),
(  0,  45, 149),
(  0,  67, 164),
(  0,  92, 180),
(  0, 119, 195),
(  0, 149, 210),
(  0, 182, 226),
(  0, 217, 240),
(  0, 255, 255),
(  0, 235, 255),
(  0, 216, 255),
(  0, 197, 255),
(  0, 178, 255),
(  0, 159, 255),
(  0, 141, 255),
(  0, 122, 255),
(  0, 104, 255),
(  0,  86, 255),
(  0,  68, 255),
(  0,  50, 255),
(  0,  33, 255),
(  0,  15, 255),
(  2,   0, 255),
( 19,   0, 255),
( 36,   0, 255),
( 53,   0, 255),
( 70,   0, 255),
( 86,   0, 255),
(103,   0, 255),
(119,   0, 255),
(135,   0, 255),
(151,   0, 255),
(167,   0, 255),
(183,   0, 255),
(198,   0, 255),
(214,   0, 255),
(229,   0, 255),
(245,   0, 255),
( 19,   0,  68),
( 31,   0,  79),
( 44,   0,  90),
( 60,   0, 101),
( 78,   0, 112),
( 98,   0, 123),
(120,   0, 134),
(144,   0, 145),
(156,   0, 142),
(166,   0, 135),
(177,   0, 127),
(187,   0, 116),
(198,   0, 104),
(208,   0,  89),
(218,   0,  73),
(229,   0,  55),
(239,   0,  36),
(249,   0,  14),
(255,   5,   0),
(255,  18,   0),
(255,  31,   0),
(255,  45,   0),
(255,  58,   0),
(255,  70,   0),
(255,  83,   0),
(255,  96,   0),
(255, 109,   0),
(255, 121,   0),
(255, 134,   0),
(255, 146,   0),
(255, 159,   0),
(255, 171,   0),
(255, 183,   0),
(255, 195,   0),
(255, 207,   0),
(255, 219,   0),
(255, 231,   0),
(255, 243,   0),
(255, 255,   0),
(122, 134, 255),
(116, 142, 255),
(111, 150, 255),
(105, 159, 255),
( 99, 170, 255),
( 93, 181, 255),
( 88, 193, 255),
( 82, 206, 255),
( 76, 220, 255),
( 71, 235, 255),
( 65, 251, 255),
( 60, 255, 242),
( 54, 255, 225),
( 49, 255, 206),
( 43, 255, 187),
( 38, 255, 167),
( 33, 255, 146),
( 27, 255, 125),
( 22, 255, 102),
( 17, 255,  79),
( 11, 255,  55),
(  6, 255,  30),
(  1, 255,   5),
( 15,   0,  70),
( 12,   0,  78),
(  9,   0,  86),
(  6,   0,  93),
(  1,   0, 101),
(  0,   4, 108),
(  0,  10, 116),
(  0,  17, 123),
(  0,  24, 131),
(  0,  32, 138),
(  0,  41, 145),
(  0,  50, 153),
(  0,  60, 160),
(  0,  71, 167),
(  0,  82, 174),
(  0,  94, 181),
(  0, 107, 189),
(  0, 120, 196),
(  0, 133, 203),
(  0, 148, 210),
(  0, 163, 217),
(  0, 178, 224),
(  0, 194, 231),
(  0, 211, 238),
(  0, 228, 244),
(  0, 245, 251),
(  0, 251, 255),
(  0, 242, 255),
(  0, 233, 255),
(  0, 224, 255),
(  0, 215, 255),
(  0, 206, 255),
(  0, 197, 255),
(  0, 188, 255),
(  0, 180, 255),
(  0, 171, 255),
(  0, 162, 255),
(  0, 154, 255),
(  0, 145, 255),
(  0, 136, 255),
(  0, 128, 255),
(  0, 119, 255),
(  0, 111, 255),
(  0, 102, 255),
(  0,  94, 255),
(  0,  86, 255),
(  0,  77, 255),
(  0,  69, 255),
(  0,  61, 255),
(  0,  52, 255),
(  0,  44, 255),
(  0,  36, 255),
(  0,  28, 255),
(  0,  20, 255),
(  0,  12, 255),
(  0,   4, 255),
(  4,   0, 255),
( 12,   0, 255),
( 20,   0, 255),
( 28,   0, 255),
( 36,   0, 255),
( 44,   0, 255),
( 51,   0, 255),
( 59,   0, 255),
( 67,   0, 255),
( 75,   0, 255),
( 82,   0, 255),
( 90,   0, 255),
( 98,   0, 255),
(105,   0, 255),
(113,   0, 255),
(120,   0, 255),
(128,   0, 255),
(135,   0, 255),
(143,   0, 255)
{
(150, 000, 255),
(158, 000, 255),
(165, 000, 255),
(172, 000, 255),
(180, 000, 255),
(187, 000, 255),
(194, 000, 255),
(202, 000, 255),
(209, 000, 255),
(216, 000, 255),
(223, 000, 255),
(230, 000, 255),
(237, 000, 255),
(244, 000, 255),
(251, 000, 255),
(192, 192, 192)
}
  );
  DEFAULT_DRAGRECT_WIDTH = 16;
  DEFAULT_ITERATIONS : Integer = 256;
  FRACTALS_INIFILE_SECTION='Fractals'; // don't localize
  FORMAT_C = '%.16f'; //'%.10f';
  FORMAT_TIME = '%.2f sec.';
  FORMAT_X_TIMES_Y = ' %d x %d';
  FORMAT_XY_FLOAT = '[ %.10f : %.10f ]';
  FORMAT_XY_INT = '[ %d : %d ]';
  LIMIT : TFloat = 4.0;
  SIZERECT_DELTA = 4;

constructor  TFractals.Create(MenuPanel__:TPanel);
begin
  Menu:=nil; ZoomList:=nil; FHint:=''; BitMap:=nil;
  FColorCycling:=False; PaletteCycled:=False;
  try    Menu:=TFractalsMenu.Create(nil,MenuPanel__,MenuPanel__.Font.Size);
  except begin Menu.Free; Menu:=nil; end;
  end;
  SetDefaultValues;
end;

destructor  TFractals.Destroy;
begin
  if ZoomList<>nil then
     while ZoomList.Prev<>nil do ZoomList:=ZoomList.Prev;
  ZoomListDeleteList(ZoomList);

  Finalize;
  Menu.Free;
  Inherited Destroy;
end;

function  TFractals.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
begin
  AntiAliasing          :=IniFile.ReadBool   (FRACTALS_INIFILE_SECTION,'AntiAliasing',AntiAliasing);
  PaletteFileName       :=KeepDataPathUpToDate(IniFile.ReadString (FRACTALS_INIFILE_SECTION,'PaletteFileName',PaletteFileName));
  ColorCyclingIntervalMS:=Max(0,IniFile.ReadInteger(FRACTALS_INIFILE_SECTION,'ColorCyclingIntervalMS',ColorCyclingIntervalMS));
  Result:=(Menu<>nil) and Menu.LoadSettingsFromIniFile(IniFile,FRACTALS_INIFILE_SECTION);
end;

function  TFractals.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
begin
  IniFile.WriteBool   (FRACTALS_INIFILE_SECTION,'AntiAliasing',AntiAliasing);
  IniFile.WriteString (FRACTALS_INIFILE_SECTION,'PaletteFileName',PaletteFileName);
  IniFile.WriteInteger(FRACTALS_INIFILE_SECTION,'ColorCyclingIntervalMS',ColorCyclingIntervalMS);
  Result:=(Menu<>nil) and Menu.SaveSettingsToIniFile(IniFile,FRACTALS_INIFILE_SECTION);
end;

procedure TFractals.SetDefaultValues;
var i:Integer;
begin
  if Menu<>nil then
     if    Menu.MenuPanel<>nil then
           Menu.SetDefaultValues(clBlack,Menu.MenuPanel.Font.Size)
     else  Menu.SetDefaultValues(clBlack,10);

  with Zoom do begin Left:=-3.68; Top:=2.07; Width:=7.36; Height:=4.14; end;
  DisplayDimensions;

  Iterations:=DEFAULT_ITERATIONS;
  ColorCount:=DEFAULT_COLOR_COUNT;

  State:=stReady; ColorCycling:=False; ColorCyclingIntervalMS:=500;

  IsDragging:=False; IsZooming:=False; IsResizing:=False; SizeHandle:=ghNull;
  DragRectWidth:=DEFAULT_DRAGRECT_WIDTH;
  DragRectHeight:=DragRectWidth*9 div 16;
  FillChar(DragRect,SizeOf(DragRect),0);

  PaletteFileName:=DEFAULT_VALUE; PaletteAuthor:='';
  for i:=0 to 255 do
      Palette[i]:=RGB(DEFAULT_PALETTE[i,0],DEFAULT_PALETTE[i,1],DEFAULT_PALETTE[i,2]);

  AntiAliasing:=True;

  ColorCyclingEnabled:=False;
  // Color cycling is 99% implemented, although it isn't put into the menu.
  // Set the switch to 'True' to play with it.
  // When enabled, use [Blank] or [Enter] to toggle colorcycling.
  // The missing part is to find out, why it sometimes causes the program to
  // hang during shutdown.
  // Obviously, one of the threads blocks the finalization procedure.
end;

procedure TFractals.Initialize;
begin
  ResetDragRect;
  if MainForm<>nil then BitMap:=MainForm.MPlayer.Display.BitMaps[Ord(dlImage)];
  if BitMap<>nil then ColorCycling:=False;
  LoadPalette(PaletteFileName,False);
end;

procedure TFractals.Finalize;
begin
  if Mandala<>nil then Mandala.Enabled:=False;
  BitMap:=nil; // flag for 'not initialized'
end;

function  GrabHandleToCursor(GrabHandle:TGrabHandle):TCursor;
begin
  case GrabHandle of
    ghTopLeft, ghBottomRight: Result := crSizeNWSE;
    ghTop, ghBottom         : Result := crSizeNS;
    ghTopRight, ghBottomLeft: Result := crSizeNESW;
    ghRight, ghLeft         : Result := crSizeWE;
    else                      Result := DEFAULT_CURSOR;
  end; // case
end;

procedure GrabHandleResize(var GrabHandle:TGrabHandle; X,Y:Integer; var ControlPoint:TPoint; var R:TRect);
const HorisontallyOppositeGrabHandle : array[TGrabHandle] of TGrabHandle =
        (ghNull,ghTopLeft    ,ghLeft ,ghBottomLeft,ghBottom,
                ghBottomRight,ghRight,ghTopRight  ,ghTop);
      VerticallyOppositeGrabHandle  : array[TGrabHandle] of TGrabHandle =
        (ghNull,ghBottomRight,ghRight,ghTopRight ,ghTop,
                ghTopLeft    ,ghLeft,ghBottomLeft,ghBottom);

var DX, DY : Integer;

  procedure Swap(var A, B: Integer);
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
    begin if Top  > Bottom then begin Swap(Top, Bottom);
                                      GrabHandle:=HorisontallyOppositeGrabHandle[GrabHandle];
                                end;
          if Left > Right  then begin Swap(Left, Right);
                                      GrabHandle:=VerticallyOppositeGrabHandle[GrabHandle];
                                end;
    end;
end;

function  PointToGrabHandle(const P:TPoint; const Rect:TRect; Delta:Integer):TGrabHandle;
begin
  with Rect do with P do
    if (X>Left-Delta) and (X<Right+Delta) and
       (Y>Top -Delta) and (Y<Bottom+Delta) then
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

procedure TFractals.SetHint(const Hint__:String);
begin
  FHint:=Hint__;
  if (MainForm<>nil) and MainForm.MPlayer.Visible then MainForm.Status.Hint:=Hint;
end;

procedure TFractals.Show;
begin
  if BitMap=nil then Initialize;
  ResetDragRect;
  if MainForm<>nil then BitMap:=MainForm.MPlayer.Display.BitMaps[Ord(dlImage)];
  if BitMap<>nil then begin
     if   (ZoomList=nil) or (ZoomList.Data=nil) or
          (ZoomList.ImageWidth <>BitMap.Width ) or
          (ZoomList.ImageHeight<>BitMap.Height) then
          MandelBrot(Zoom,BitMap.Width,BitMap.Height);
     if   ColorCycling then begin
          State:=stReady;
          ColorCycling:=True; // to start cycling;
          end
     else if ZoomList<>nil then begin
             MakeBitMap(ZoomList,BitMap);
             State:=stReady;
             MainForm.MPlayer.Display.ShowImage;
             if ColorCyclingEnabled and
                (Mandala<>nil) then Mandala.Enabled:=True;
             end;
     end;
end;

procedure TFractals.Mandelbrot(const Zoom:TZoomRect; ImageWidth,ImageHeight:Integer);
var i,j,Count:Integer;
    A,B,C,x,y,dx,dy,SqrA,SqrB:TFloat;
    p:PByte;
    Time1,Time2:Cardinal; oCursor:TCursor;
begin
  if (ImageWidth>0) and (ImageHeight>0) and (MainForm<>nil) then begin
     ZoomListInsert(Zoom,ImageWidth,ImageHeight);

     if (ZoomList<>nil) and (ZoomList.Data<>nil) and
        (State <> stCalculate) then begin
        oCursor:=Screen.Cursor;
        try
          State:=stCalculate;
          Screen.Cursor:=crHourGlass;
          FillChar(DragRect,SizeOf(DragRect),0);

          Time1:=GetTickCount;
          dx:=Zoom.Width/ImageWidth;
          dy:=Zoom.Height/ImageHeight;
          y :=Zoom.Top;
          p:=ZoomList.Data;
          for j:=0 to Pred(ImageHeight) do
              begin x:=Zoom.Left;
                    for i:=0 to Pred(ImageWidth) do
                         begin
                               A:=0.0; B:=0.0; Count:=0;
                               SqrA:=0.0; SqrB:=0.0;
                               repeat C:=SqrA - SqrB + x;
                                      B:=2*A*B + y;
                                      A:=C;
                                      SqrA:=Sqr(A); SqrB:=Sqr(B);
                                      Inc(Count);
                               until  (Count=Iterations) or
                                      (SqrA+SqrB>LIMIT);

                               {
                               // algorithm for a smooth fractional iteration count
                               var
                                 FloatingPointColorIndex;

                               // 2 extra iterations helps decrease the size of the error term
                               C:=SqrA - SqrB + x;
                               B:=2*A*B + y;
                               A:=C;
                               SqrA:=Sqr(A); SqrB:=Sqr(B);
                               Inc(Count);

                               C:=SqrA - SqrB + x;
                               B:=2*A*B + y;
                               A:=C;
                               SqrA:=Sqr(A); SqrB:=Sqr(B);
                               Inc(Count);

                               //float mu = iter_count - (log (log (modulus)))/ log (2.0)

                               A:=Log10(Sqrt(SqrA+SqrB));
                               if A>0.0 then A:=Log10(A);

                               FloatingPointColorIndex:=Trunc(Count-(A/Log10(2.0)));
                               }

                               if   Count<Iterations then
                                    p^:=Count mod ColorCount
                               else p^:=1; // index 1 used as default color, not index 0
                               Inc(p);

                               x:=x+dx;
                         end;
                     y:=y-dy;
               end;
          Time2:=GetTickCount;
          if Time2>=Time1 then Dec(Time2,Time1)
          else Inc(Time2,High(Time2)-Time1);
          Hint:=Format(FORMAT_TIME,[Time2 / 1000]);
        finally
          Screen.Cursor:=oCursor;
          State:=stReady;
          MainForm.MPlayer.ResetFps;
        end;
        end;
     end;
end;

procedure TFractals.MakeBitMap(ZoomList:PZoomListItem; BitMap:TBitMap);
var X,Y:Integer; C:TColor; p:PByte; q:PRGBVector;
begin
  if (ZoomList<>nil) and (BitMap<>nil) and
     (ZoomList.Data<>nil) and
     (ZoomList.ImageWidth =BitMap.Width ) and
     (ZoomList.ImageHeight=BitMap.Height) then begin
     p:=ZoomList.Data;
     for Y:=0 to Pred(BitMap.Height) do begin
         q:=BitMap.ScanLine[Y];
         for X:=0 to Pred(BitMap.Width) do begin
             C:=Palette[p^];
             q[X].r:= C shr 16;
             q[X].g:=(C shr 8) and 255;
             q[X].b:= C and 255;
             Inc(p);
             end;
         end;
         
     if AntiAliasing then
        BitMapBilinearAntiAliasing(BitMap,False,clBlack,0);
     end;
end;

procedure TFractals.DisplayDimensions;
begin
{ Panel3.Caption:='';
  LabelWindowLeft  .Caption:=Format(FORMAT_C,[Zoom.Left]);
  LabelWindowBottom.Caption:=Format(FORMAT_C,[Zoom.Top-Zoom.Height]);
  LabelWindowRight .Caption:=Format(FORMAT_C,[Zoom.Left+Zoom.Width]);
  LabelWindowTop   .Caption:=Format(FORMAT_C,[Zoom.Top]);
  LabelWindowXSize .Caption:=Format(FORMAT_C,[Zoom.Width]);
  LabelWindowYSize .Caption:=Format(FORMAT_C,[Zoom.Height]);
  LabelMouseX      .Caption:='';
  LabelMouseY      .Caption:='';
  Application.ProcessMessages;}
end;

procedure TFractals.DrawRect(const Rect:TRect; PenMode:TPenMode);
const PEN_WIDTH=1;
begin
  if BitMap<>nil then with BitMap.Canvas do begin
     Pen.Color   := clWhite;
     Pen.Mode    := PenMode;
//   Pen.Style   := psDot;
     Pen.Width   := PEN_WIDTH;
     Brush.Style := bsClear;

     DragRect := Rect;
     with Rect do Rectangle(Left +PEN_WIDTH, Top   +PEN_WIDTH,
                            Right+PEN_WIDTH, Bottom+PEN_WIDTH);

     MainForm.MPlayer.Display.ShowImage;
     end;
end;

procedure TFractals.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Point:TPoint;
begin
  if      Button=mbLeft then
          begin MainForm.MPlayer.FormXYtoMPlayerDisplayXY(X,Y);
                Point.X:=X; Point.Y:=Y;
                IsDragging:=PtInRect(DragRect,Point);
                if   IsResizing then
                     begin SizeHandle:=PointToGrabHandle(Point,DragRect,SIZERECT_DELTA);
                           if SizeHandle<>ghNull then IsDragging:=False;
                     end;
                if Screen.Cursor<>GrabHandleToCursor(SizeHandle) then
                   Screen.Cursor:=GrabHandleToCursor(SizeHandle);
                IsZooming :=(not IsDragging) and (SizeHandle=ghNull);
                DoZoomIn  :=IsDragging;
                IsResizing  :=False;
                DragPoint.X:=X; DragPoint.Y:=Y;
                DragOriginPoint.X:=X; DragOriginPoint.Y:=Y;
                if   IsZooming then
                     begin if DragRect.Left<DragRect.Right then
                              DrawRect(DragRect,pmXor);
                           DragRect.Left:=X-(DragRectWidth  div 2);
                           DragRect.Top :=Y-(DragRectHeight div 2);
                           DragRect.Right:=DragRect.Left+DragRectWidth;
                           DragRect.Bottom:=DragRect.Top+DragRectHeight;
                           DrawRect(DragRect,pmXor);
                           ////Screen.Cursor:=crSize;
                           State:=stReady;
                           if Screen.Cursor<>DEFAULT_CURSOR then
                              Screen.Cursor:=DEFAULT_CURSOR;
                     end
                else begin
                     end;
          end
  else if Button=mbRight then
          begin if   DragRect.Left<DragRect.Right then
                     ResetDragRect
                else ZoomOut;
                State:=stReady;
          end;
end;

procedure TFractals.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (BitMap<>nil) and
     (IsDragging or IsZooming or (SizeHandle<>ghNull)) then with BitMap.Canvas do
     begin //DrawRect(DragRect,pmXor);
           Screen.Cursor:=DEFAULT_CURSOR;
           IsDragging:=False; IsZooming:=False; SizeHandle:=ghNull;
           IsResizing:=False;
           State:=stZoom;

           if DoZoomIn then ZoomIn;
     end;
end;

procedure TFractals.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var //X1,Y1:TFloat;
    DX,DY,W,H:Integer; C:TCursor; GrabHandle:TGrabHandle; P:TPoint; R:TRect;
begin
  MainForm.MPlayer.FormXYtoMPlayerDisplayXY(X,Y);
  if (BitMap<>nil) then with BitMap do
     begin
          //X1:=Zoom.Left+Zoom.Width *X/Width;
          //Y1:=Zoom.Top +Zoom.Height*(Height-Y)/Height-Zoom.Height;
          //StatusBar1.Panels[0].Text:=Format(FORMAT_XY,[X1,Y1]);
          //LabelMouseX.Caption:=Format(FORMAT_C,[X1]);
          //LabelMouseY.Caption:=Format(FORMAT_C,[Y1]);

          if (Hint='') or (State=stReady) then State:=State;

          if      IsZooming then with DragRect do
                  begin DrawRect(DragRect,pmXor);

                        W:=2*Abs(X-DragOriginPoint.X);
                        H:=2*Abs(Y-DragOriginPoint.Y);
                        if   W <  H*16 div 9 then
                             begin W := H*16 div 9;
                             end
                        else begin H := W*9  div 16;
                             end;
                        FixedAspectRatio16x9(W,H);

                        Left  :=DragOriginPoint.X-(W div 2);
                        Right :=Left+W;
                        Top   :=DragOriginPoint.Y-(H div 2);
                        Bottom:=Top +H;

                        DrawRect(DragRect,pmXor);

                        if Screen.Cursor<>DEFAULT_CURSOR then
                           Screen.Cursor:=DEFAULT_CURSOR;
                  end
          else if IsDragging then with DragRect do
                  begin DrawRect(DragRect,pmXor);

                        DX:=X-DragPoint.X; DY:=Y-DragPoint.Y;
                        DragPoint.X:=X; DragPoint.Y:=Y;

                        Inc(Left  ,DX);
                        Inc(Top   ,DY);
                        Inc(Right ,DX);
                        Inc(Bottom,DY);

                        DrawRect(DragRect,pmXor);

                        DoZoomIn:=False;

                        if Screen.Cursor<>DEFAULT_CURSOR then
                           Screen.Cursor:=DEFAULT_CURSOR;
                  end
          else if SizeHandle<>ghNull then with DragRect do
                  begin
                        W:=Right-Left; H:=Bottom-Top;
                        P:=DragPoint; R:=DragRect;
                        GrabHandleResize(SizeHandle,X,Y,P,R);
                        DX:=(R.Right-R.Left)-W;
                        DY:=(R.Bottom-R.Top)-H;

                        if (Abs(DX)>=16) or (Abs(DY)>=9) then begin
                           DrawRect(DragRect,pmXor);

                           DragPoint:=P; DragRect:=R;

                           W:=Right-Left; H:=Bottom-Top;
                           if Abs(DX)<Abs(DY) then begin
                              W:=H*16 div 9;  H:=W* 9 div 16;
                              end
                           else begin
                              H:=W* 9 div 16; W:=H*16 div 9;
                              end;

                           case SizeHandle of
                             ghTopLeft     : begin Left :=Right-W; Top   :=Bottom-H; end;
                             ghTop,
                             ghTopRight    : begin Right:=Left +W; Top   :=Bottom-H; end;
                             ghLeft,
                             ghBottomLeft  : begin Left :=Right-W; Bottom:=Top   +H; end;
                             else            begin Right:=Left +W; Bottom:=Top   +H; end;
                           end; // case

                           DrawRect(DragRect,pmXor);
                           end;
                  end
          else begin
                     GrabHandle:=PointToGrabHandle(Point(X,Y),DragRect,SIZERECT_DELTA);
                     if GrabHandle=ghNull then
                        begin if Screen.Cursor<>DEFAULT_CURSOR then
                                   begin Screen.Cursor:=DEFAULT_CURSOR;
                                         State:=stReady;
                                   end;
                              IsResizing:=True;
                        end
                     else if IsResizing then
                             begin C:=GrabHandleToCursor(GrabHandle);
                                   if   C<>Screen.Cursor then
                                        Screen.Cursor:=C;
                                   if   C=DEFAULT_CURSOR then
                                        if State<>stReady then State:=stReady
                                        else
                                   else if State<>stReSize then State:=stReSize;
                             end;
               end;
     end;
end;

procedure TFractals.ZoomIn;
var X1,Y1,X2,Y2:TFloat;
begin
  if (BitMap<>nil) and
     (DragRect.Left<DragRect.Right) then with BitMap do
     begin
       X1:=Zoom.Left+Zoom.Width *DragRect.Left/Width;
       Y1:=Zoom.Top +(Zoom.Height*(Height-DragRect.Top)/Height)-Zoom.Height;
       X2:=Zoom.Left+Zoom.Width *DragRect.Right/Width;
       Y2:=Zoom.Top +(Zoom.Height*(Height-DragRect.Bottom)/Height)-Zoom.Height;

       if (X1<X2) and (Y2<Y1) then
          begin Zoom.Left:=X1; Zoom.Top:=Y1;
                Zoom.Width:=X2-X1; Zoom.Height:=Y1-Y2;
                if Menu<>nil then Menu.EnableDisableButtons;
                MandelBrot(Zoom,BitMap.Width,BitMap.Height);
                DisplayDimensions;
                if ColorCycling and UpdatePattern then //
                else begin
                   MakeBitMap(ZoomList,BitMap);
                   MainForm.MPlayer.Display.ShowImage;
                   end;
          end;
     end;
end;

procedure TFractals.ZoomOut;
begin
  if (ZoomList<>nil) and (ZoomList.Prev<>nil) then
     begin
       ZoomList:=ZoomList.Prev;
       if Menu<>nil then Menu.EnableDisableButtons;
       Zoom:=ZoomList.Rect; DisplayDimensions;
       Show;
     end;
end;

procedure TFractals.FixedAspectRatio16x9(var Width,Height:Integer);
begin
  if Width <16 then Width :=16;
  if Height< 9 then Height:= 9;
  while ((Height mod  9)<>0)
        or
        ((Width  mod 16)<>0)
        or
        ((Height div 9) <> (Width div 16)) do
        begin Inc(Height); Width:=(Height*16) div 9;
        end;
end;

procedure TFractals.SetState(State__:TFractalState);
begin
  FState:=State__;
  case State of
    stNull        : Hint := '';
    stReady       : if   (ZoomList=nil) or (ZoomList.Prev=nil) then
                         Hint := FractalsHelp1Text
                    else Hint := FractalsHelp1Text+FractalsHelp4Text;
    stZoom        : Hint := FractalsHelp2Text;
    stResize      : Hint := FractalsHelp3Text;
    stCalculate   : Hint := CalculatingImageText+PleaseWaitText;
    else            Hint := '';
    end; // case
  if Menu<>nil then Menu.EnableDisableButtons;
end;

procedure TFractals.Reset;
begin
  if (ZoomList<>nil) then
     begin while ZoomList.Prev<>nil do ZoomList:=ZoomList.Prev;
           if Menu<>nil then Menu.EnableDisableButtons;
           Zoom:=ZoomList.Rect;
           Initialize;
           Show;
     end;
end;

procedure TFractals.Prior;
begin
  ZoomOut;
end;

procedure TFractals.Next;
begin
  if (ZoomList<>nil) and (ZoomList.Next<>nil) then
     begin ZoomList:=ZoomList.Next;
           if Menu<>nil then Menu.EnableDisableButtons;
           Zoom:=ZoomList.Rect; DisplayDimensions;
           Show;
     end;
end;

procedure TFractals.ZoomListDeleteList(Item:PZoomListItem);
var PrevItem,p:PZoomListItem;
begin
  if Item<>nil then
     begin PrevItem:=Item.Prev;
           if PrevItem<>nil then PrevItem.Next:=nil;
           while Item<>nil do
             begin if Item.Data<>nil then FreeMem(Item.Data);
                   p:=Item.Next;
                   if Item=ZoomList then ZoomList:=PrevItem;
                   Dispose(Item);
                   Item:=p;
             end;
     end;
end;

procedure TFractals.ZoomListInsert(const Zoom:TZoomRect; ImageWidth,ImageHeight:Integer);
var p:PZoomListItem;
begin
  try    New(p);
  except on E:Exception do p:=nil;
  end;

  if ZoomList<>nil then // note that the rest of the images are destroyed
     ZoomListDeleteList(ZoomList.Next);

  if (p<>nil) and
     (not SafeGetMem(Pointer(p.Data),ImageWidth*ImageHeight,False,True)) then begin
     Dispose(p); p:=nil;
     end;

  if p<>nil then begin
     p.Rect:=Zoom;
     p.ImageWidth:=ImageWidth;
     p.ImageHeight:=ImageHeight;
     p.Prev:=ZoomList;
     if ZoomList=nil then
        p.Next:=nil
     else begin
       p.Next:=ZoomList.Next;
       if p.Next<>nil then p.Next.Prev:=p;
       ZoomList.Next:=p;
       end;
     ZoomList:=p;
    end;
end;

function  TFractals.LoadPalette(const FileName:String; SilentError:Boolean):Boolean;
var R,G,B,No:Integer; s,Author:String;
    TextFile:System.TextFile;  ByteFile:file;
    Map: record case Boolean of
                  False      : (C:TPalette);
                  True       : (B: array[0..255,0..3] of Byte)
         end;
begin {$I+}
  Result:=False;
  try
//  raise Exception.Create('Test Error #123445');
    if FileName=DEFAULT_VALUE then s:=DefaultPaletteFileName
    else s:=FileName;
    if FileExists(s) then begin
       AssignFile(ByteFile,s);
       System.Reset(ByteFile,1);
       try     if System.FileSize(ByteFile)=SizeOf(Map.C) then
                  begin FillChar(Map,SizeOf(Map),0);
                        BlockRead(ByteFile,Map.C,SizeOf(Map.C),No);
                        Result:=No=SizeOf(Map.C);
                  end;
       finally CloseFile(ByteFile);
       end;

       if not Result then
          begin
            AssignFile(TextFile,s);
            System.Reset(TextFile);
            try
              No:=0;
              FillChar(Map,SizeOf(Map),0);
              while (not Eof(TextFile)) and (No<=255) do
                begin Read(TextFile,R,G,B);
                      if   No=0 then Readln(TextFile,Author)
                      else Readln(TextFile);
                      Map.C[No]:=RGB(R,G,B);
                      Inc(No);
                end;
              Result:=True;
            finally
              CloseFile(TextFile);
            end;
          end;

       if Result then begin
          Palette:=Map.C;
          PaletteFileName:=s;
          PaletteAuthor:=Trim(Author);
          PaletteCycled:=False;
          end
       else begin
          PaletteFileName:=DEFAULT_VALUE;
          PaletteAuthor:='';
          end;
       end;
  except
    on E:Exception do begin
       if not SilentError then
          Error(Format(OpenFileFailedShortText__+NL+NL+TEXT_FAILURE_DESCRIPTION+NL+'"%s".',[s,E.Message]),
                Application.Title+' - '+FractalsMenuText[0]+' - '+OpenPaletteText);
       Result:=False;
       end;
  end;
end;

function  TFractals.SavePalette(const FileName:String; PascalSyntax:Boolean):Boolean;
var i,RGB:Integer; s1,s2,s3:String; OutFile:TextFile;
begin {$R+}

  if   ExtractFileExt(FileName)='' then
       s1:=FileName+PALETTE_FILE_EXT
  else s1:=FileName;
  try
//  raise Exception.Create('Test Error #123445');
    AssignFile(OutFile,s1);
    Rewrite(OutFile); //PascalSyntax:=True;
    if   PascalSyntax then
         begin s1:='('; s2:=', '; s3:=')'; end
    else begin s1:='' ; s2:=' ' ; s3:='' ; end;
    try
      for i:=0 to 255 do
          begin RGB:=Palette[i];
                Write(OutFile,Format('%s%3d%s%3d%s%3d%s',
                                     [s1,
                                      (RGB         and 255),
                                      s2,
                                      ((RGB shr  8) and 255),
                                      s2,
                                      ((RGB shr 16) and 255),
                                      s3])
                       );
                if PascalSyntax and (i<>255) then Write(OutFile,s2);
                Writeln(OutFile);
          end;
    finally
      CloseFile(OutFile);
    end;
    Result:=True;
  except
    on E:Exception do Result:=Error(E.Message,'');
  end;
end;

procedure TFractals.SaveAs;
begin
  if Mandala<>nil then Mandala.Suspend;
  if BitMap<>nil then
     with MainForm.FractalsSaveDialog do
       begin
             if (InitialDir='') or
                (not DirectoryExists(StrWithoutTrailingPathDelimiter(InitialDir))) then
                InitialDir:=MainForm.ApplicationDataPath+FRACTALS_DIRECTORY;
             FileName:=MakeNewFileName(StrWithTrailingPathDelimiter(InitialDir)+MandelbrotText,BMP_FILE_EXT,False);
             if Execute then
                begin if (not FileExists(FileName)) or
                         (Application.MessageBox(PChar(Format(FileExistsText__+NL+NL+OverwriteItText,[FileName])),
                                                 PChar(Title),
                                                 MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)=IDYES)
                         then
                         begin InitialDir:=ExtractFilePath(FileName);
                               SaveImage(FileName);
                         end;
                end
     end;
  if ColorCycling then ColorCycling:=True;
end;

function  TFractals.SaveImage(const FileName:String):Boolean;
var s:String;
begin
  try
    if ExtractFileExt(FileName)='' then
       s:=FileName+BMP_FILE_EXT
    else s:=FileName;
    if BitMap<>nil then
       begin BitMap.SaveToFile(s);
             Result:=True;
       end
    else Result:=False;
  except
  on E:Exception do
     begin Application.MessageBox(PChar(E.Message),PChar(MainForm.FractalsSaveDialog.Title),MB_OK);
           Result:=False;
     end;
  end;
end;

procedure TFractals.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin // not in production
  case Key Of
    VK_LEFT,
    VK_UP                            : MainForm.BtnFractalsPriorClick(Sender);
    Ord(ACCEL_CHAR_RESET           ) : MainForm.BtnFractalsResetClick(Sender);
    VK_RIGHT,
    VK_DOWN                          : MainForm.BtnFractalsPriorClick(Sender);
    VK_F1,
    Ord(ACCEL_CHAR_HELP            ) : MainForm.BtnHelpClick(Sender);
    Ord(ACCEL_CHAR_SAVE_AS         ) : MainForm.BtnFractalsSaveAsClick(Sender);
    Ord(ACCEL_CHAR_OPTIONS         ) : MainForm.BtnFractalsSettingsClick(Sender);
  end; // case
end;

procedure TFractals.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  Prior;
  Handled:=True;
end;

procedure TFractals.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  Next;
  Handled:=True;
end;

procedure TFractals.ResetDragRect;
begin
  if DragRect.Left<DragRect.Right then
     begin DrawRect(DragRect,pmXor);
           FillChar(DragRect,SizeOf(DragRect),0);
     end;
end;

function  TFractals.DefaultPaletteFileName:String;
begin
  Result:=StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath+FRACTALS_DIRECTORY)+StrWithoutBrackets(DEFAULT_VALUE)+PALETTE_FILE_EXT;
end;

procedure TFractals.SetColorCycling(ColorCycling__:Boolean);
var i:Integer; RGBPalette:array[Low(Palette)..High(Palette)] of TRGB;
begin
  FColorCycling:=ColorCycling__
                 and
                 (Mandala<>nil)
                 and
                 (Mandala.PaletteAndPatternSaved
                  or
                  (Mandala.SavePaletteAndPattern or True) // '... or True': don't care whether 'SavePaletteAndPattern' succeeded or not
                 )
                 and
                 UpdatePattern;
  if ColorCycling then begin
     Mandala.PaletteChangeEnabled:=False;
     Mandala.PatternChangeEnabled:=False;
     Mandala.Resume;
     PaletteCycled:=True;
     if Menu.Visible then Menu.Show;
     end
  else begin
    if Mandala<>nil then begin
       if (not Mandala.Suspended) and // grap current palette from 'Mandala'
          Mandala.SavePaletteToMemory(SizeOf(RGBPalette),Addr(RGBPalette)) then
          for i:=Low(Palette) to High(Palette) do
              Palette[i]:=RGBToColor(RGBPalette[i]);
       Mandala.Enabled:=False;
       end;
    Show;
    end;
end;

function  TFractals.UpdatePattern:Boolean;
var i:Integer; RGBPalette:array[Low(Palette)..High(Palette)] of TRGB;
begin
  for i:=Low(RGBPalette) to High(RGBPalette) do
      RGBPalette[i]:=ColorToRGB(Palette[i]);

  Result:=(ZoomList<>nil) and (ZoomList.Data<>nil) and
          (Mandala<>nil) and (Mandala.Engine<>nil) and
          Mandala.LoadPatternFromMemory(
            ZoomList.ImageWidth*ZoomList.ImageHeight,
            ZoomList.Data) and
          Mandala.LoadPaletteFromMemory(SizeOf(RGBPalette),Addr(RGBPalette));
  if Result then Mandala.SetFixedSpeed(ColorCyclingIntervalMS);
end;

end.

