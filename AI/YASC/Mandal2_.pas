unit Mandal2_;
// this unit is a stripped down version of:
// 'Mandala' Copyright (c) by Anthony Steele

{
Differences to the original version:

1. A fixed image-size, based on 'BITMAP_WIDTH' and 'BITMAP_HEIGHT'.
2. Software rendering to a bitmap, no DirectX support.
3. Simple objects, and no 'COM' machinery.
4. Fewer object-types, i.e., a 'functional' approach to programming
   rather than the cleaner 'object-oriented' approach.
5. The thread handling has been rewritten to match my personal taste.
}

interface

uses Windows,Classes,Graphics,StdCtrls,SyncObjs,
     IniFile_;

// note that with one modification, this unit is self-contained:
// 1. Substitute 'IniFile_' with Delphi's own 'IniFiles'.

// To get the unit up and running, 3 steps are required:
// 1. call Mandala.Initialize(BitMap), where 'BitMap' has dimensions BITMAP_WIDTH * BITMAP_HEIGHT
// 2. call Mandala.Resume, to start the animation.
// 3. Use 'Mandala.Paint' to refresh the screen. A simple way to do it
//    is to put in an 'OnTimer'-event, or an 'OnIdle'-event.
//
//    An Example
//    ----------
//    A. Make a new project, or at least a new form.
//    B. Put an 'Image' on the form.
//    C. Put a 'Timer' on the form.
//    D. Make an 'OnCreate'-event for the form like this:
//
//       procedure TForm1.FormCreate(Sender: TObject);
//       begin
//         Timer1.Interval:=20;                         // 20 millisec. = 50 frames/second
//         Image1.Picture.BitMap:=TBitMap.Create;       // create the bitmap
//         Image1.Picture.BitMap.Width:=BITMAP_WIDTH;   // set width
//         Image1.Picture.BitMap.Height:=BITMAP_HEIGHT; // set height
//         Mandala.Initialize(Image1.Picture.BitMap);   // tell 'Mandala' about the bitmap
//         Mandala.Resume;                              // start 'Mandala'
//       end;
//
//    E. Make an 'OnTimer'-event for the timer like this:
//
//       procedure TForm1.Timer1Timer(Sender: TObject);
//       begin
//         Mandala.Paint;                               // refresh the screen
//       end;

type
  TColorSelection= (csHigh, csMidHigh, csMid, csMidLow, csLow, csRandom);
  TFloat         = Extended;

  TPaletteEffectType
                 = (peSnowBurst,peRegeneration,
                    pePastel,peMuted,peShadows,pePrimary,peBlue,
                    peTriColor,peCaterpillar,peWinterMountain,peRust,
                    peStain
                   );
  TPaletteThreadTask
                 = (ptSpin,ptChange);
  TPatternType   = (ptAmoeba,ptConcentric,ptConstellations,ptCutGlass,
                    ptDiamond,ptDoubleDiamond,ptFloralMorph,ptFlower,
                    ptLiquidCrystal,ptPlasmaCloud,ptPrimalOrbit,
                    ptSnakes,ptTwistedPyramids,ptFan);

  TMandalaThreadStateType
                 = (tsUndefined,tsWaiting,tsCalculating,tsReady,tsError,tsTerminate,tsTerminated);

const
  BITMAP_HEIGHT  = 180;
  BITMAP_WIDTH   = 320;
  BITMAP_RECT    : TRect = (Left:0; Top:0; Right:BITMAP_WIDTH; Bottom:BITMAP_HEIGHT);
  BITMAP_XCENTER = BITMAP_WIDTH  div 2;
  BITMAP_YCENTER = BITMAP_HEIGHT div 2;

  MANDALA_ENGINE_SETTINGS_SUFFIX
                 = 'Engine'; // don't localize

  MAX_PALETTES   = 5;
  MAX_PATTERNS   = 10;

  CR             = #13;
  LF             = #10;
  NL             = CR+LF;

  PaletteEffectTypeText:array[tPaletteEffectType] of String =
    ('Snowburst','Regeneration',
     'Pastel','Muted','Shadows',
     'Primary','Blue',
     'TriColor','Caterpillar','Winter Mountain','Rust',
     'Stain'
    );

  PatternTypeText:array[TPatternType] of String =
    ('Amoeba','Concentric','Constellations','CutGlass',
     'Diamond','DoubleDiamond','FloralMorph','Flower',
     'LiquidCrystal','PlasmaCloud','PrimalOrbit',
     'Snakes','TwistedPyramids','Fan'
  );


  RGB_RED_INDEX  = 2;
  RGB_GREEN_INDEX= 1;
  RGB_BLUE_INDEX = 0;

  WAIT_FOR_THREAD_TO_TERMINATE_MS
                 = 3000; // {milli seconds}

type
  TRGB           = packed record
    case Boolean of
      False      : (b,g,r:Byte); // note: 'rgb' is really 'bgr'!
      True       : (c: array[0..2] of Byte);
  end;
  PRGB           = ^TRGB;
  TRGBVector     = array[0..(MaxInt div SizeOf(TRGB))-1] of TRGB;
  PRGBVector     = ^TRGBVector;

  TPaletteColors = array[0..255] of TRGB;

  TPalette = class
  private
    FColorCount:Integer;
    FPhase:Integer;
  protected
    procedure   SetPhase(NewPhase:Integer);
  public
    PaletteColors:TPaletteColors;
    Next:TPalette;
    SpinCount  :Integer;
    procedure   AddColor(const RGB:TRGB);
    procedure   Assign(Source:TPalette);
    function    Chop(Value:Integer):Integer;
    procedure   Clear;
    constructor Create;
    destructor  Destroy; override;
    procedure   IncBlueNoWrap(Amount, StartIndex, EndIndex: Integer);
    procedure   IncBlueWrap(Amount, StartIndex, EndIndex: Integer);
    procedure   IncGreenNoWrap(Amount, StartIndex, EndIndex: Integer);
    procedure   IncGreenWrap(Amount, StartIndex, EndIndex: Integer);
    procedure   IncNoWrap(Amount, StartIndex, EndIndex: Integer; Red, Green,
      Blue: Boolean);
    procedure   IncNoWrapColorIndex(Amount, StartIndex, EndIndex,
      ColorIndex: Integer);
    procedure   IncRedNoWrap(Amount, StartIndex, EndIndex: Integer);
    procedure   IncRedWrap(Amount, StartIndex, EndIndex: Integer);
    procedure   IncWrap(Amount, StartIndex, EndIndex: Integer; Red, Green,
      Blue: Boolean);
    procedure   IncWrapColorIndex(Amount, StartIndex, EndIndex,
      ColorIndex: Integer);
    function    LoadFromMemory(Size: Integer; Data: Pointer):Boolean;
    procedure   MakeGreyScalePalette;
    procedure   MakeRandomPalette(Count:Integer);
    function    SaveToMemory(Size:Integer; Data:Pointer):Boolean;
    procedure   SetRange(StartIndex, EndIndex: Integer; const StartColor,
      EndColor: TRGB);
    procedure   Spin(Spin:Integer);
    function    Wrap(Value:Integer):Integer;

    property    Phase:Integer read FPhase write SetPhase;
    property    ColorCount:Integer read FColorCount;
  end;

  TPatternBytes = array[0..BITMAP_HEIGHT-1,0..BITMAP_WIDTH-1] of Byte;

  TPattern = class
  public
    RangeLow:Integer;
    RangeHigh:Integer;
    PatternBytes:TPatternBytes;
    PatternType:TPatternType;
    Next:TPattern;
    procedure   AlphaBlend(const StartPatternBytes,EndPatternBytes:TPatternBytes; Pct:Integer);
    procedure   Assign(Source:TPattern);
    function    Chop(Value:Integer):Byte;
    constructor Create;
    destructor  Destroy; override;
    function    LoadFromMemory(Size: Integer; Data: Pointer):Boolean;
    procedure   MakeAmoeba;
    procedure   MakeConcentric;
    procedure   MakeConstellations;
    procedure   MakeCutGlass;
    procedure   MakeDiamond;
    procedure   MakeDoubleDiamond;
    procedure   MakeFan;
    procedure   MakeFloralMorph;
    procedure   MakeFlower;
    procedure   MakeTitle;
    procedure   MakeLiquidCrystal;
    procedure   MakeNextPatternType;
    procedure   MakePatternType(NewPatternType:TPatternType);
    procedure   MakePlasmaCloud;
    procedure   MakePrimalOrbit;
    procedure   MakePriorPatternType;
    procedure   MakeRandomPattern;
    procedure   MakeSnakes;
    procedure   MakeText(const Text:String; XOffset,YOffSet:Integer; TextFont:TFont);
    procedure   MakeTwistedPyramids;
    function    PixelMorph(TargetPattern: TPattern): Boolean;
    function    ReversePixelMorph(TargetPattern: TPattern; CountDown:Integer): Boolean;
    function    RangeSize:Integer;
    procedure   SaveToFile(const FileName:String);
    procedure   SaveToMemo(Memo:TMemo);
    function    Scale(Value,Low,High: TFloat):Byte;
    function    Wrap(Value:Integer):Byte;
  end;

  TPaletteEffect = class
  protected
    CurrentColors: TPaletteColors;
    FColorCount  : Integer;
    FDone        : Boolean;
    FIsFade      : Boolean;
    FEffectType  : TPaletteEffectType;
    Steps        : Integer;

  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure   Clear; virtual;
    procedure   Step; virtual;

    property    ColorCount:Integer read FColorCount;
    property    Done: Boolean read FDone;
    property    IsFade: Boolean read FIsFade;
    property    EffectType: TPaletteEffectType read FEffectType;
  end;

  TPaletteEffectReversion = class (TPaletteEffect)
  private
  protected
    OriginalColors: TPaletteColors;

    function NewColorByAverage (Index: Integer): TRGB;
    function NewColorByRevert  (Index: Integer): TRGB;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure Step; override;
  end;

  TPaletteEffectSnowBurst = class (TPaletteEffectReversion)
  private
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure Step; override;
  end;

  TPaletteEffectRegeneration = class (TPaletteEffectReversion)
  private
    DestinationColors: TPaletteColors;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure Step; override;
  end;

  TPaletteEffectFourColors = class (TPaletteEffect)
  private
    FourColor: array [0..3] of TRGB;
    FourColorLength: array[0..3] of Integer;
    FourColorPosition: array[0..3] of Integer; // four color points are located in the palette
    FourColorTarget: array [0..3] of TRGB;
  protected
    MaxLength: Integer;
    LifeTime: Cardinal;
    StartTime: Cardinal;

    function  NewColorType (Pos, Col: Integer): TColorSelection; virtual;
    function  NewColor (Pos: Integer): TRGB; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure Step; override;
  end;

  TPaletteEffectPastel = class (TPaletteEffectFourColors)
  protected
    function  NewColorType(Pos, Col: Integer): TColorSelection; override;
    function  SecondaryColorType(Pos, Col: Integer): TColorSelection;
    function  NewColor (Pos: Integer): TRGB; override;
  public
    constructor Create; override;
  end;

  TPaletteEffectMuted = class (TPaletteEffectFourColors)
  protected
    function  NewColorType(Pos, Col: Integer): TColorSelection; override;
  public
    constructor Create; override;
  end;

  TPaletteEffectShadows = class (TPaletteEffectFourColors)
  private
    ShadowPos, LightPos: Integer;
  protected
    function  NewColorType(Pos, Col: Integer): TColorSelection; override;
    function  NewColor (Pos: Integer): TRGB; override;
  public
    constructor Create; override;
    procedure Clear; override;
  end;

  TPaletteEffectPrimary = class (TPaletteEffectFourColors)
  protected
    function  NewColorType(Pos, Col: Integer): TColorSelection; override;
    function  SecondaryColorType(Pos, Col: Integer): TColorSelection;
    function  NewColor (Pos: Integer): TRGB; override;
  public
    constructor Create; override;
  end;

  TPaletteEffectBlue = class (TPaletteEffectFourColors)
  protected
    function  NewColorType(Pos, Col: Integer): TColorSelection; override;
    function  SecondaryColorType(Pos, Col: Integer): TColorSelection;
    function  NewColor (Pos: Integer): TRGB; override;
  public
    constructor Create; override;
  end;

  TPaletteEffectSequential =  class (TPaletteEffectFourColors)
  private
    List     : TPaletteColors;
    ListCount: Integer;
    ListIndex: Integer;
  protected
    function  NewColor (Pos: Integer): TRGB; override;
    procedure AddColor (Color: TRGB);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TPaletteEffectTriColor = class (TPaletteEffectSequential)
  public
    constructor Create; override;
  end;

  TPaletteEffectCaterpillar = class (TPaletteEffectSequential)
  public
    constructor Create; override;
  end;

  TPaletteEffectWinterMountain = class (TPaletteEffectSequential)
  public
    constructor Create; override;
  end;

  TPaletteEffectRust = class (TPaletteEffectSequential)
  public
    constructor Create; override;
  end;

  TPaletteEffectStain = class (TPaletteEffect)
  private
    Ending         : Boolean;
    LifeTime       : Cardinal;
    OriginalColors : TPaletteColors;
    StartTime      : Cardinal;
    TargetColors   : TPaletteColors;
    procedure StainPalette (Start, FullWidth, Depth, Color: Integer);
    procedure DoStain;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure Step; override;
  end;

  TThread1 = class(TThread) // a basic thread type, with a critical section and a state
  private
    CriticalSection:TCriticalSection;
  protected
    fLockCount: Integer;
    fState:     Integer;

    function    GetState:Integer;
    procedure   SetState(State__:Integer);
  public
    {$WARNINGS OFF} // Symbols 'Resume' and 'Suspend' are deprecated
      constructor Create(CriticalSection__:TCriticalSection; Priority__:TThreadPriority);
      destructor  Destroy; override;

      procedure Enter;
      procedure Leave;
      procedure Resume;  // note that 'Resume' and 'Suspend' are NOT stack-oriented as normal; they set 'Suspended' to 'True' or 'False'
      procedure Suspend; // note that 'Resume' and 'Suspend' are NOT stack-oriented as normal; they set 'Suspended' to 'True' or 'False'

      property  LockCount:Integer read fLockCount;
      property  State:Integer read GetState write SetState;
    {$WARNINGS ON}
  end;

  TMandalaThread = class(TThread1)
  protected
    TimeForNextUpdate :Cardinal;
    TimeOfLastUpdate  :Cardinal;
  public
    procedure   Initialize; virtual;
  end;

const
  MAX_SPEED_INDEX = 5;
type
  TSleepVector = array [0..MAX_SPEED_INDEX] of Cardinal;

  TSpeedData = record
    ChangeIntervalMS   : Cardinal; // milliseconds
    ChangeDeviationPct : Cardinal; // percent
    StepsPerSecondMin  : Integer;  // steps per second
    StepsPerSecondMax  : Integer;  // steps per second
    Direction          : Integer;  // -1: backwards 1: forwards
  end;

  TMandala             = class;

  TPaletteThread       = class(TMandalaThread)
  private
    Sleep             :TSleepVector;
    Speed             :TSpeedData;
    SpeedChangeCount  :Integer;
    SpeedIndex        :Integer;
    Task              :TPaletteThreadTask;
    TextLabel         :TLabel;
    TimeForNextSpeedChange,
    TimeForNextStep   :Cardinal;
    procedure   Step; virtual; abstract;
  public
    FixedIntervalMS   :Integer;
    constructor Create(Mandala__:TMandala; Task__:TPaletteThreadTask);
    destructor  Destroy; override;

    procedure   Execute; override;
    procedure   Initialize(Task__:TPaletteThreadTask); reintroduce;
    procedure   SetFixedSpeed(IntervalMS:Integer);
  end;

  TPaletteSpinThread = class(TPaletteThread)
  private
    procedure   Step; override;
  public
    constructor Create(Mandala__:TMandala);
  end;

  TPaletteChangeThread = class(TPaletteThread)
  private
    PaletteEffects : array[TPaletteEffectType] of TPaletteEffect;
    procedure   Finalize;
    function    RandomEffectNoFades: TPaletteEffect;
    procedure   Step; override;
  public
    PaletteEffect  : TPaletteEffect;

    constructor Create(Mandala__:TMandala);
    destructor  Destroy; override;

    function    RandomEffect: TPaletteEffect;
  end;

  TPatternThread   = class(TMandalaThread)
  private
    CurrentPattern   : TPattern;
    FadePattern      : TPattern;
    PatternList      : TPattern;
    PatternListCount : Integer;
    PrevPattern      : TPattern;
    procedure   EnqueuePattern(APattern: TPattern);
    function    RandomNotRecentlyUsedPatternType:TPatternType;

  public
    constructor Create(Mandala__:TMandala);
    destructor  Destroy; override;

    function    DequeuePattern: TPattern;
    procedure   Execute; override;
  end;

  TMandala = class(TCriticalSection)
  private
    BitMap             : TBitMap; // output

    FPaletteChangeEnabled,
    FPatternChangeEnabled
                       : Boolean;
    NextFreePattern    : TPattern;
    NextFreePalette    : TPalette;
    Palettes           : array[0..MAX_PALETTES-1] of TPalette;
    Patterns           : array[0..MAX_PATTERNS-1] of TPattern;

    PaletteSpinThread  : TPaletteSpinThread;
    PaletteChangeThread: TPaletteChangeThread;
    PatternThread      : TPatternThread;
    SavedPalette       : TPalette;
    SavedPattern       : TPattern;
    WorkBitMap         : TBitMap;
    WorkBitMapPitch    : Integer;

    procedure   Finalize;
  protected
    procedure   SetPaletteChangeEnabled(PaletteChangeEnabled__:Boolean);
    procedure   SetPatternChangeEnabled(PatternChangeEnabled__:Boolean);
  public
    Palette            : TPalette; // not implemented as a 'property'; lock before use
    PaletteGreyScale   : TPalette; // not implemented as a 'property'; lock before use
    Pattern            : TPattern; // not implemented as a 'property'; lock before use

    PaletteChangeSpeedLabel,
    PaletteEffectNameLabel,
    PaletteSpinSpeedLabel,
    PatternNameLabel   : TLabel;

    // settings
    PaletteSpinSpeed:TSpeedData;
    PaletteChangeSpeed:TSpeedData;
    PaletteEffectsEnabled:Boolean;
    PaletteEffectsFadesEnabled:Boolean;
    PaletteEffectsIntervalMS:Cardinal; // milliseconds;
    PaletteEffectsDeviationPct:Cardinal; // pct

    PatternChangeIntervalMS:Cardinal; // milliseconds;
    PatternChangeDeviationPct:Cardinal; // pct
    PatternChangeFadeMS:Cardinal; // milliseconds
    PatternChangeFadeSteps:Cardinal;
    PatternChangeFadeStepIntervalMS:Cardinal;
    PatternChangeFadeColorCycling:Boolean;
    PatternChangePixelMorphIntervalMS:Cardinal;

    ShowTitleOnStartup:Boolean;

    Title,SubTitle:String;

    constructor Create(BitMap__:TBitMap;
                       PaletteChangeSpeedLabel__,
                       PaletteEffectNameLabel__,
                       PaletteSpinSpeedLabel__,
                       PatternNameLabel__:TLabel);
    destructor  Destroy; override;

    function    AllSuspended: Boolean;
    procedure   FreePalette(Palette:TPalette);
    procedure   FreePattern(Pattern:TPattern);
    procedure   GetScreenPaletteColors(var ColorCount:Integer; var PaletteColors:TPaletteColors);
    function    LoadPaletteFromMemory(Size:Integer; Data:Pointer):Boolean;
    function    LoadPatternFromMemory(Size:Integer; Data:Pointer):Boolean;
    function    LoadSettingsFromIniFile(const IniFile: TIniFile; const Section:String): Boolean;
    procedure   Initialize(BitMap__:TBitMap);
    function    NewPalette:TPalette;
    function    NewPattern:TPattern;
    function    NextPattern:Boolean;
    procedure   Paint;
    function    PaletteAndPatternSaved:Boolean;
    procedure   Resume;
    function    RestorePaletteAndPattern:Boolean;
    function    SavePaletteAndPattern:Boolean;
    function    SavePaletteToMemory(Size:Integer; Data:Pointer):Boolean;
    function    SaveSettingsToIniFile(const IniFile: TIniFile; const Section:String): Boolean;
    procedure   SetFixedSpeed(IntervalMS:Integer);
    procedure   Suspend;

    property    PaletteChangeEnabled:Boolean read FPaletteChangeEnabled write SetPaletteChangeEnabled;
    property    PatternChangeEnabled:Boolean read FPatternChangeEnabled write SetPatternChangeEnabled;
  end;

type
  TAlphaBlendTable     = array[-255..256] of SmallInt; // ..'256'instead of '255': 512 entries in case it helps alignment
  PAlphaBlendTable     = ^TAlphaBlendTable;
var
  AlphaBlendTables     : array[0..100] of TAlphaBlendTable;

  Mandala              : TMandala = nil;

implementation

uses SysUtils,Math,Forms,Controls;

const
  RADIANS_QUARTER_CIRCLE = PI / 2;
  RADIANS_HALF_CIRCLE = PI;
  RADIANS_THREEQUARTER_CIRCLE = PI * 3 / 2;
  RADIANS_FULL_CIRCLE = PI * 2;

  RGB_BLACK : TRGB = (b:  0; g:  0; r:  0);
  RGB_WHITE : TRGB = (b:255; g:255; r:255);

  DEFAULT_TITLE_TEXT    ='Mandala';
  DEFAULT_SUBTITLE_TEXT ='Images for contemplation...';

type
  TTimeMS = DWORD; // milli seconds

procedure MakeAlphaBlendTable(NewPct:Integer; var Pct:Integer; var Table:TAlphaBlendTable);
var i,Alpha:Integer;
begin
  Pct:=NewPct; Alpha:=Pct*128 div 100;
  for i:=Low(Table) to High(Table) do Table[i]:=(i*Alpha) div 128;
end;

function  Max(a,b:Integer):Integer;
begin
  if a>=b then Result:=a
  else Result:=b;
end;

function  Min(a,b:Integer):Integer;
begin
  if a<=b then Result:=a
  else Result:=b;
end;

function ByteWrap (Value: Integer): Byte;
begin
  while Value < 0 do Inc(Value,256);
  Result :=Value and 255;
end;

function ByteChop (Value: Integer): Byte;
begin
  Result:=Min(255,Max(0,Value));
end;

function ScaleToByte (Value: TFloat; Low, High: Integer): Byte;
begin
  Result := Round (((Value - Low) / (High - Low)) * 255);
end;

function ByteAverage (b1, b2,b3: Integer): Byte;
begin
  Result := ByteChop (Round ((b1 + b2 + b3) / 3));
end;

function XYToDistanceRounded(x,y: Integer): Integer;
begin
  if (x=0) or (y=0) then Result:=0
  else Result:=Round(Hypot(Abs(x),Abs(y)));
end;

function SafeHypot (x,y: TFloat): TFloat;
begin
  if      x = 0.0 then Result := Abs(y)
  else if y = 0.0 then Result := Abs(x)
  else                 Result := Hypot(Abs(x),Abs(y));
end;

function XYDistanceToAngle (x,y: Integer; Distance: TFloat): TFloat;
const QuadrantOffset:array[0..3] of TFloat =
        (0.0,RADIANS_QUARTER_CIRCLE,RADIANS_HALF_CIRCLE,RADIANS_THREEQUARTER_CIRCLE);
var Quadrant: Integer;
begin

{ quadrants are numbered
  1|0
  -+-
  2|3

  angles are calculated up from the horizontal line
  so for quadrants 1 and 3 use  90o - angle  }

  // check for zero distance from the center
  Distance := Abs(Distance);
  if Distance <= 0.000001 then Result:=0.0
  else
    if      x = 0     then
            if y >=0  then Result := RADIANS_QUARTER_CIRCLE
            else           Result := RADIANS_THREEQUARTER_CIRCLE
    else if y = 0     then
            if x >=0  then Result := 0.0
            else           Result := RADIANS_HALF_CIRCLE
    else begin // normal case - neither x or y = 0
      // determine the quadrant
      if   x >= 0    then
           if y >= 0 then Quadrant := 0
           else           Quadrant := 3
      else if y >= 0 then Quadrant := 1
           else           Quadrant := 2;

      Result := ArcTan (Abs(y)/Abs(x));

      // compensate  for downward angle
      if Odd(Quadrant) then Result := RADIANS_QUARTER_CIRCLE - Result;

      Result:=Result+QuadrantOffset[Quadrant];

    end;
end;

procedure AngleDistanceToXY (Angle, Distance: TFloat; var x,y: Integer);
begin
  while Angle < 0.0 do Angle := Angle + RADIANS_FULL_CIRCLE;

// cos Angle = adjacent  / hypotenuse = x / Distance => x = cos Angle * Distance
  x := Round (Cos (Angle) * Distance);

//  sin Angle = opposite / hypotenuse = y / Distance => y = sin Angle * Distance
  y := Round (Sin (Angle) * Distance);
end;

procedure RotateCounterClockwise (var x,y: Integer; Angle: TFloat);
var Distance: TFloat;
begin
  Distance := SafeHypot (x,y); // distance from origin
  AngleDistanceToXY(
    Angle+XYDistanceToAngle(x,y, Distance), // rotation angle + angle from origin
    Distance,
    x,y);
end;

function RandomFloat(Max: TFloat): TFloat;
begin
  Result:=Random*Max;
end;

function RandomBoolean: Boolean;
begin
  Result:=Random(2)=1;
end;

function RandomByte: Byte;
begin
  Result := Random(256);
end;

function RandomNonZeroByte: Byte;
begin
  Result := Succ(Random(255));
end;

function SawTooth (x, Period: Integer): Integer;
begin
  while x < 0 do x := x + (Period * 2);
  Result := Abs (Period - (x mod (2 * Period)));
end;

function XYToSquareDistance (x, y: Integer): Integer;
begin
  Result:=Max(Abs (x), Abs (y));
end;

function XYToCrossDistance (x, y: Integer): Integer;
begin
  x := Abs (x);
  y := Abs (y);
  if x >= y then Result := x + y*2
  else           Result := y + x*2;
end;

function  ByteHighOrLow: Byte;
begin
  Result:=Random (2) * 255;
end;

function  RGB(r,g,b:Byte):TRGB;
begin
  Result.r:=r; Result.g:=g; Result.b:=b;
end;

function  RGBRandom:TRGB;
begin
  with Result do begin
    r:=Random(256); g:=Random(256); b:=Random(256);
    end;
end;

function  RGBRandomPrimary: TRGB;
begin
 with Result do begin
   r:=ByteHighOrLow; g:=ByteHighOrLow; b:=ByteHighOrLow;
   end;
end;

function  RGBIsBlack(const C:TRGB):Boolean;
begin
  Result:=(C.r=0) and (C.g=0) and (C.b=0);
end;

function  RGBIsEqual(const A,B:TRGB):Boolean;
begin
  Result:=(A.r=B.r) and (A.g=B.g) and (A.b=B.b);
end;

function  RGBIsAlike(const A,B:TRGB; Pct:Integer):Boolean;
begin
  Result := (Abs(A.r-B.r) + Abs(A.g-B.g) + Abs(A.b-B.b)) * 100 div (3*256) < Pct ;
end;

function  RGBIsWhite(const C:TRGB):Boolean;
begin
  Result:=(C.r=255) and (C.g=255) and (C.b=255);
end;

function  RGBMoveTowards (Amount: Integer;
	var Source: TRGB; const Dest: TRGB): Boolean;
var i:Integer;
begin
  Result := False;

  for i:=0 to 2 do with Source do
      if      c[i] < Dest.c[i] then begin
              c[i] := ByteChop(c[i] + Amount);
              if c[i] > Dest.c[i] then c[i] := Dest.c[i];
              Result:=True;
              end
      else if c[i] > Dest.c[i] then begin
              c[i] := ByteChop(c[i] - Amount);
              if c[i] < Dest.c[i] then c[i] := Dest.c[i];
              Result:=True;
              end;
end;

// set the values to a smooth first -> last
procedure RGBSetRange (StartIndex, EndIndex, PaletteStart, PaletteEnd: Integer;
	  const StartColor, EndColor: TRGB; var PaletteColors: TPaletteColors);
var
  i, j, Index, TempEnd, Length, PaletteLength: Integer;
  Dif : array[0..2] of Integer;
begin
  PaletteLength := Succ(PaletteEnd - PaletteStart); // palette size

  // find range to do - cater for cases such as entries 193 -> 0
  TempEnd := EndIndex;
  while TempEnd < StartIndex do Inc(TempEnd, PaletteLength);
  Length := TempEnd - StartIndex;

  if (Length >= 1) and (Length < PaletteLength) then begin
     for i :=0 to 2 do Dif[i] := EndColor.c[i] - StartColor.c[i];

     for i := 0 to Pred(Length) do begin
  	 Index := StartIndex + i;
         //wrap around
         while Index > PaletteEnd do Dec(Index, PaletteLength);

         for j := 0 to 2 do
             PaletteColors[Index].c[j] := StartColor.c[j] + (Dif[j] * i div Length);
         end;
     end;
end;

function ColorSelectionValue (ColorType: TColorSelection): Byte;
begin
  case ColorType of
    csHigh    :	Result := 255;
    csMidHigh : Result := 192;
    csMid     :	Result := 128;
    csMidLow  :	Result := 64;
    csLow     : Result := 0;
    else        Result := Random (256);
  end;
end;

function  RandomInterval(Interval,DeviationPct:Cardinal):Cardinal;
begin
  Result := Interval * DeviationPct div 100; // result:=deviation
  Result := Interval + Result - Cardinal(Random(2 * Result));
end;

function  RandomPaletteEffectType:TPaletteEffectType;
begin
  Result:=TPaletteEffectType(Random(Succ(Ord(High(TPaletteEffectType))-Ord(Low(TPaletteEffectType)))));
end;

function  RandomPatternType:TPatternType;
begin
  Result:=TPatternType(Random(Succ(Ord(High(TPatternType))-Ord(Low(TPatternType)))));
end;

procedure MakeRandomPalette(Count:Integer; var PaletteColors:TPaletteColors);
var i:Integer; Value:Byte;
begin
  for i:=0 to Pred(Count) do begin
      Value:=Random(256);
      //with PaletteColors[i] do begin r:=Value; g:=Value; b:=Value; end;
      with PaletteColors[i] do begin r:=Value; g:=Random(256); b:=Random(256); end;
      end;
  for i:=1 to Count-2 do
      if Odd(i) then with PaletteColors[i] do begin
         r:=ByteAverage(PaletteColors[Pred(i)].r,PaletteColors[Pred(i)].r,PaletteColors[Succ(i)].r);
         g:=ByteAverage(PaletteColors[Pred(i)].g,PaletteColors[Pred(i)].g,PaletteColors[Succ(i)].g);
         b:=ByteAverage(PaletteColors[Pred(i)].b,PaletteColors[Pred(i)].b,PaletteColors[Succ(i)].b);
         end;
end;

constructor TPalette.Create;
begin
  Inherited;
  Clear;
end;

destructor  TPalette.Destroy;
begin
  Inherited;
end;

procedure   TPalette.Clear;
begin
  FColorCount:=0;
end;

procedure  TPalette.SetPhase(NewPhase:Integer);
begin
  Spin(NewPhase-Phase);
end;

procedure TPalette.Spin(Spin:Integer);
begin
  FPhase:=(FPhase+Spin) mod FColorCount;
end;

procedure TPalette.Assign(Source:TPalette);
begin
  if Source<>nil then begin
     PaletteColors:=Source.PaletteColors;
     FColorCount  :=Source.ColorCount;
     SpinCount    :=Source.SpinCount;
     FPhase       :=Source.Phase;
     end;
end;

procedure TPalette.MakeGreyScalePalette;
var i:Integer;
begin
  FPhase:=0; FColorCount:=256;
  for i:=0 to 255 do PaletteColors[i]:=RGB(i,i,i);
end;

procedure TPalette.MakeRandomPalette(Count:Integer);
begin
  FPhase:=0; FColorCount:=Count;
  Mandal2_.MakeRandomPalette(Count,PaletteColors);
end;

function  TPalette.Wrap(Value:Integer):Integer;
begin
  while Value<0 do Inc(Value,ColorCount);
  Result :=Value mod ColorCount;
end;

function  TPalette.Chop(Value:Integer):Integer;
begin
  Result:=Min(Pred(ColorCount),Max(0,Value));
end;

procedure TPalette.IncWrapColorIndex (Amount,StartIndex,EndIndex,ColorIndex: Integer);
var
  Loop, Index : Integer;
begin
  for Loop := StartIndex to EndIndex do begin
      Index := Wrap(Loop);
      PaletteColors[Index].c[ColorIndex] :=
        ByteWrap(PaletteColors[Index].c[ColorIndex] + Amount);
      end;
end;

procedure TPalette.IncNoWrapColorIndex (Amount,StartIndex,EndIndex,ColorIndex: Integer);
var
  Loop, Index : Integer;
begin
  for Loop := StartIndex to EndIndex do begin
      Index := Wrap(Loop);
      PaletteColors[Index].c[ColorIndex] :=
        ByteChop(PaletteColors[Index].c[ColorIndex] + Amount);
      end;
end;

procedure TPalette.IncRedWrap (Amount,StartIndex,EndIndex: Integer);
begin
  IncWrapColorIndex(Amount,StartIndex,EndIndex,RGB_RED_INDEX);
end;

procedure TPalette.IncRedNoWrap (Amount,StartIndex,EndIndex: Integer);
begin
  IncNoWrapColorIndex(Amount,StartIndex,EndIndex,RGB_RED_INDEX);
end;

procedure TPalette.IncGreenWrap (Amount,StartIndex,EndIndex: Integer);
begin
  IncWrapColorIndex(Amount,StartIndex,EndIndex,RGB_GREEN_INDEX);
end;

procedure TPalette.IncGreenNoWrap (Amount,StartIndex,EndIndex: Integer);
begin
  IncNoWrapColorIndex(Amount,StartIndex,EndIndex,RGB_GREEN_INDEX);
end;

procedure TPalette.IncBlueWrap (Amount,StartIndex,EndIndex: Integer);
begin
  IncWrapColorIndex(Amount,StartIndex,EndIndex,RGB_BLUE_INDEX);
end;

procedure TPalette.IncBlueNoWrap (Amount,StartIndex,EndIndex: Integer);
begin
  IncNoWrapColorIndex(Amount,StartIndex,EndIndex,RGB_BLUE_INDEX);
end;

procedure TPalette.IncWrap (Amount, StartIndex, EndIndex: Integer;
                            Red, Green, Blue: Boolean);
var Loop : Integer;
begin
  for Loop := StartIndex to EndIndex do with PaletteColors[Wrap(Loop)] do begin
      if Red   then r := ByteWrap(r + Amount);
      if Green then g := ByteWrap(g + Amount);
      if Blue  then b := ByteWrap(b + Amount);
      end;
end;

procedure TPalette.IncNoWrap (Amount, StartIndex, EndIndex: Integer;
                              Red, Green, Blue: Boolean);
var Loop : Integer;
begin
  for Loop := StartIndex to EndIndex do with PaletteColors[Wrap(Loop)] do begin
      if Red   then r := ByteChop(r + Amount);
      if Green then g := ByteChop(g + Amount);
      if Blue  then b := ByteChop(b + Amount);
      end;
end;

// set the values to a smooth first -> last
procedure TPalette.SetRange (StartIndex, EndIndex: Integer; const StartColor, EndColor: TRGB);
begin
  RGBSetRange(StartIndex,EndIndex,0,Pred(ColorCount),StartColor,EndColor,PaletteColors);
end;

procedure TPalette.AddColor(const RGB:TRGB);
begin
  if ColorCount<High(PaletteColors) then begin
     PaletteColors[ColorCount]:=RGB;
     Inc(FColorCount);
     end;
end;

function  TPalette.LoadFromMemory(Size:Integer; Data:Pointer):Boolean;
begin
  Result:=Data<>nil;
  if Result then begin
     Move(Data^,PaletteColors,Min(Size,SizeOf(PaletteColors)));
     FPhase:=0;
     end;
end;

function  TPalette.SaveToMemory(Size:Integer; Data:Pointer):Boolean;
var i,j:Integer; P:TPaletteColors;
begin
  Result:=Data<>nil;
  if Result then begin
     j:=0;
     for i:=0 to Pred(ColorCount) do begin
         P[j]:=PaletteColors[(i+Phase) mod ColorCount]; Inc(j);
         end;
     Move(P,Data^,Min(Size,SizeOf(P)));
     end;
end;

constructor TPattern.Create;
begin
  Inherited;
  RangeLow:=Low(TPaletteColors); RangeHigh:=High(TPaletteColors);
//RangeLow:=128;  RangeHigh:=196;
end;

destructor  TPattern.Destroy;
begin
  Inherited;
end;

procedure TPattern.Assign(Source:TPattern);
begin
  if Source<>nil then begin
     RangeLow     :=Source.RangeLow;
     RangeHigh    :=Source.RangeHigh;
     PatternBytes :=Source.PatternBytes;
     PatternType  :=Source.PatternType;
     end;
end;

function  TPattern.Chop(Value:Integer):Byte;
begin
  Result:=Min(RangeHigh,Max(RangeLow,Value));
end;

procedure  TPattern.SaveToFile(const FileName:String);
var X,Y:Integer; Text:TextFile;
begin
  AssignFile(Text,FileName);
  Rewrite(Text);
  for Y:=0 to BITMAP_HEIGHT -1 do begin
      for X:=0 to BITMAP_WIDTH-1 do Write(Text,PatternBytes[Y,X]:4);
      Writeln(Text);
      end;
  CloseFile(Text);
end;

procedure  TPattern.SaveToMemo(Memo:TMemo);
var X,Y:Integer; s:String;
begin
  Memo.Lines.Clear;
  for Y:=0 to BITMAP_HEIGHT -1 do begin
      s:='';
      for X:=0 to BITMAP_WIDTH-1 do s:=s+Format('%4d',[PatternBytes[Y,X]]);
      Memo.Lines.Add(s+NL);
      end;
end;

procedure TPattern.MakeAmoeba;
const OK_BITMAP_HEIGHT = 40; MAX_BITMAP_WIDTH = 70;
var BITMAP_HEIGHT1, BITMAP_HEIGHT2, BITMAP_HEIGHT3, BITMAP_HEIGHT4: Integer;
    BITMAP_WIDTH1, BITMAP_WIDTH2, BITMAP_WIDTH3, BITMAP_WIDTH4: Integer;
    X,Y,XLoop,YLoop:Integer;
    Distance,Value:TFloat;

begin
  PatternType:=ptAmoeba;
  BITMAP_HEIGHT1 := 0; BITMAP_HEIGHT2 := 0; BITMAP_HEIGHT3 := 0; BITMAP_HEIGHT4 := 0;
  while (BITMAP_HEIGHT1 < OK_BITMAP_HEIGHT) and (BITMAP_HEIGHT2 < OK_BITMAP_HEIGHT) and
  	(BITMAP_HEIGHT3 < OK_BITMAP_HEIGHT) and (BITMAP_HEIGHT4 < OK_BITMAP_HEIGHT) do begin
    BITMAP_HEIGHT1 := 10 + Random (50) + Random (50);
    BITMAP_HEIGHT2 := 10 + Random (50) + Random (50);
    BITMAP_HEIGHT3 := 10 + Random (50) + Random (50);
    BITMAP_HEIGHT4 := 10 + Random (50) + Random (50);
    end;

  BITMAP_WIDTH1 := MAX_BITMAP_WIDTH + 1; BITMAP_WIDTH2 := MAX_BITMAP_WIDTH + 1;
  BITMAP_WIDTH3 := MAX_BITMAP_WIDTH + 1; BITMAP_WIDTH4 := MAX_BITMAP_WIDTH + 1;

  while (BITMAP_WIDTH1 > MAX_BITMAP_WIDTH) and (BITMAP_WIDTH2 > MAX_BITMAP_WIDTH) and
  	(BITMAP_WIDTH3 > MAX_BITMAP_WIDTH) and (BITMAP_WIDTH4 > MAX_BITMAP_WIDTH) do begin
    BITMAP_WIDTH1 := 15 + Random (120);
    BITMAP_WIDTH2 := 15 + Random (120);
    BITMAP_WIDTH3 := 15 + Random (120);
    BITMAP_WIDTH4 := 15 + Random (120);
    end;

  for YLoop := 0 to BITMAP_HEIGHT -1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      Y := YLoop - BITMAP_YCENTER;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
          X := XLoop - BITMAP_XCENTER;

          Distance :=SafeHypot(X,Y);

          Value := 0.0;
          Value := Value + BITMAP_HEIGHT1 * Cos(Distance / BITMAP_WIDTH1) * Sin(Y / BITMAP_WIDTH1);
          Value := Value + BITMAP_HEIGHT2 * Cos(Distance / BITMAP_WIDTH2) * Sin(Y / BITMAP_WIDTH2);
          Value := Value + BITMAP_HEIGHT3 * Cos(Distance / BITMAP_WIDTH3) * Sin((X + Y) / BITMAP_WIDTH3);
          Value := Value + BITMAP_HEIGHT4 * Cos(Distance / BITMAP_WIDTH4) * Sin((X - Y) / BITMAP_WIDTH4);

          PatternBytes[YLoop,XLoop] := Wrap(Floor(Value));
          end;
      end;
end;

procedure TPattern.MakeConcentric;
var X,Y,XLoop,YLoop,ConcentricScale,ConcentricStrength,
    CircularRays,ColumnRays,ColumnRaysStrength:Integer;
    Angle,Distance,Value:TFloat;
begin
  PatternType:=ptConcentric;
  ConcentricScale := 5 + Random (36);
  ConcentricStrength := 50 + Random (350);

    { NUM RAYS }
    { both 0 -> concentric circles }
    { circular rays only -> ice-cream swirl/ wobly spiral }
    { column rays only -> spider / eyes }

    { don't let both # rays be the same }
  CircularRays := 0;
  ColumnRays := 0;
  while (CircularRays = ColumnRays) do begin
    CircularRays := Random (5);
    ColumnRays   := Random (5) + 2;
    end;
  ColumnRaysStrength := Random (300) + 6;

//CircularRays := 0;
//ColumnRays := 0;

  for YLoop := 0 to BITMAP_HEIGHT -1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      Y := YLoop - BITMAP_YCENTER;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
          X := XLoop - BITMAP_XCENTER;

          Distance :=SafeHypot(X,Y);

          Angle := XYDistanceToAngle (X,Y, Distance);

          Value := 0.0;

          { concentric circles }
          Value := Value + Sin (Distance/1000 * ConcentricScale) * ConcentricStrength;


          { circular rays }
          Value := Value + Scale(Angle, 0, RADIANS_FULL_CIRCLE) * CircularRays;

          { columnar rays }
          Value := Value + Sin(Angle * ColumnRays) * ColumnRaysStrength;

          PatternBytes[YLoop,XLoop] := Wrap(Round(Value));
          end;
      end;
end;

procedure TPattern.MakeConstellations;
const
  MAX_POINTS = 25;
type
  TStarType = (stCircle, stDiamond, stSquare, stCross);
  TStarSelection = (ssAllTheSame, ssSequence, ssRandom);
  TStarCurve = (scSpiral, scLobed);
var
  Points: array [0..MAX_POINTS] of TPoint;
  StarTypes: array [0..MAX_POINTS] of TStarType;

  Concentric, Spiral: Boolean;
  Petals, SpiralFactor, PaletteScale: Integer;
  ConcentricScale, ConcentricMagnitude: Integer;

  Loop, X, Y, XLoop, YLoop, Height,
  IDistance, XDistance, YDistance, MinDistance,
  Uncoil,
  Count, IValue: Integer;
  Angle, Distance, Step, Value: TFloat;
  TempStarTypes: array [0..5] of TStarType;

  function RandomStarType:TStarType;
  begin
    Result:=TStarType(Random(Ord(High(TStarType))-Ord(Low(TStarType))+1));
  end;

begin // MakeConstellations;
  PatternType:=ptConstellations;
  case TStarSelection(Random(3)) of //type of star sequences
    ssAllTheSame: begin TempStarTypes[0] := RandomStarType;
                        for Loop := 1 to MAX_POINTS do
      	                    StarTypes[Loop] := TempStarTypes[0];
                  end;
    ssSequence  : begin Count := 2 + Random (4); { 2 to 5 types }
                        for Loop := 0 to Count - 1 do
      	                    TempStarTypes[Loop] := RandomStarType;
			for Loop := 1 to MAX_POINTS do
                            StarTypes[Loop] := TempStarTypes[Loop mod Count];
                  end;
    ssRandom    : begin for Loop := 1 to MAX_POINTS do
      	                    StarTypes[Loop] := RandomStarType;
                  end;
  end; // case

  Height := 2 + Random (4); // 2..5

  Angle := RandomFloat(RADIANS_FULL_CIRCLE); //starting angle
  Step := RandomFloat(RADIANS_QUARTER_CIRCLE / 3 * 2) + (RADIANS_QUARTER_CIRCLE) / 5; // rotation
  if not RandomBoolean then Step:=-Step; // clockwise ?
  IDistance := Random (40); // expansion
  Uncoil := 10 + Random (11); // 10..20

//  IDistance := 4; // expansion
//  Uncoil := 20;

  for Loop := 1 to MAX_POINTS do begin
      Angle := Angle + Step;
      AngleDistanceToXY (Angle, IDistance + (Loop * Uncoil), X, Y);
      Points[Loop].x := X;
      Points[Loop].y := Y;
      end;

  //overlays
  Concentric := RandomBoolean;
//Concentric := False;
  Spiral := RandomBoolean;
//Spiral := True;

  ConcentricScale := 10 + Random (81);
  ConcentricMagnitude := 5 + Random (50);

  Petals := Succ(Random (6)); // petals := 1-6
  case Random (3) of // low is loosely coiled
    0   : SpiralFactor := 10 + Random (50);  // loosely coiled
    1   : SpiralFactor := 10 + Random (200); // medium coiled
    else  SpiralFactor := 10 + Random (500); // tight coiled
  end; // case

  PaletteScale := Succ(Random (4)); // low scale means v. raised  spiral
  // tight spirals should not be to raised
  PaletteScale := PaletteScale + (SpiralFactor div 120);

  for YLoop := 0 to BITMAP_HEIGHT - 1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      Y := YLoop - BITMAP_YCENTER;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
    	  X := XLoop - BITMAP_XCENTER;

          MinDistance := High (Integer);
          IDistance := 0;

          for Loop := 1 to MAX_POINTS do begin // find the nearest point
              XDistance := X - Points[Loop].x;
      	      YDistance := Y - Points[Loop].y;

      	      case StarTypes[Loop] of
                stCircle:   IDistance := Round (SafeHypot (XDistance, YDistance));
                stDiamond:  IDistance := Abs (XDistance) + Abs (YDistance);
                stSquare:   IDistance := XYToSquareDistance (XDistance, YDistance);
                stCross:    IDistance := XYtoCrossDistance (XDistance, YDistance);
              end; // case

      	      // IDistance := IDistance + (MAX_POINTS - iNearest);}
              if IDistance < MinDistance then MinDistance := IDistance;
              end;

          IValue := MinDistance * Height; // distance to nearest point

          Distance := SafeHypot (X,Y);

          if Concentric then begin
             Value := Sin (Distance/1000 * ConcentricScale) * ConcentricMagnitude;
             IValue := IValue + Round (Value);
             end;

          if Spiral then begin
             Angle := XYDistanceToAngle(X,Y,Distance);
             Value :=  Sin (Angle * Petals + Distance/1000 * SpiralFactor);
             IValue := IValue + Scale(Value, -PaletteScale, PaletteScale);
             end;

          PatternBytes[YLoop,XLoop]:=Wrap(IValue);
          end;
      end;
end;

procedure TPattern.MakeCutGlass;
const
  MAX_POINTS = 25;
  NULL_POINT = -999999;
  RANDOM_RANGE = 400;
var
  Points: array [0..MAX_POINTS] of TPoint;
  Height,Chop: Integer;
  Concentric: Boolean;
  ConcentricScale, ConcentricMagnitude: Integer;

  Loop, PointsCount,
  IDistance,XDistance,YDistance, MinDistance,
  X, Y, XLoop, YLoop, InterLopers, IValue: Integer;
  InitialAngle, Angle, Distance, Value: TFloat;

begin
  PatternType:=ptCutGlass;
  Chop := (1 shl 8);
  if RandomBoolean then
    Chop := Chop +(1 shl 7)
  else begin
    if RandomBoolean then Chop := Chop + (1 shl 6);
    if RandomBoolean then Chop := Chop + (1 shl 5);
    end;

  Height := Succ(Random(4)); // 1..4

  for Loop := 1 to MAX_POINTS do begin
      Points[Loop].x := NULL_POINT;
      Points[Loop].y := NULL_POINT;
      end;

  // first point is at the center
  Points[1].x := 0;
  Points[1].y := 0;

  // points are arranged regularly 3 to 10 points per circle
  // but usually 4 to 8 points
  if   RandomBoolean then
       PointsCount := 3 + Random (8)
  else PointsCount := 4 + Random (5);

  // distance 100 to 1000 but mostly 100 to 200 or so
  case Random (6) of
    0   : IDistance :=  90 + Random (60);
    1..2: IDistance := 100 + Random (50);
    3   : IDistance := 100 + Random (100);
    4   : IDistance := 100 + Random (300);
    else  IDistance := 100 + Random (1000);
  end; // case

  InitialAngle := RandomFloat(RADIANS_FULL_CIRCLE / PointsCount);
  Angle := InitialAngle;

  for Loop := 2 to PointsCount + 1 do begin
    AngleDistanceToXY (Angle, IDistance, X, Y);
    Points[Loop].x := X;
    Points[Loop].y := Y;
    Angle := Angle + (RADIANS_FULL_CIRCLE / PointsCount);
    end;

  // second row
  Angle := InitialAngle + (RADIANS_FULL_CIRCLE / (PointsCount * 2));
  IDistance := IDistance * 2 + (Random (20) - 10);

  for Loop := PointsCount + 2 to (2 * PointsCount) + 1 do begin
    AngleDistanceToXY (Angle, IDistance, X, Y);
    Points[Loop].x := X;
    Points[Loop].y := Y;
    Angle := Angle + (RADIANS_FULL_CIRCLE / PointsCount);
    end;

  // and maybe an interloper or two
  case Random (10) of
  	0..4: Interlopers := 0; // 5/10 times there are none
  	5..6: Interlopers := 1;
  	7..8: Interlopers := 2;
  	else  Interlopers := 3;
  end; // case

  for Loop := 20 downto (20 - Interlopers + 1) do begin
      Points[Loop].x := Random (RANDOM_RANGE * 2) - RANDOM_RANGE;
      Points[Loop].y := Random (RANDOM_RANGE * 2) - RANDOM_RANGE;
      end;

  // Concentric := RandomBoolean;
  // always concentric !
  Concentric := True;

  // concentric can be small, weak waves (foreground ) or
  // strong, large waves (background )}

  //ConcentricScale := 10 + Random (81);
  //ConcentricMagnitude := 5 + Random (50);

  // 1 .. 5 : the smaller the scale, the larger the circles
  ConcentricScale := 1 + Random (5);
  ConcentricMagnitude := 300 + Random (300); // 300 - 600

  for YLoop := 0 to BITMAP_HEIGHT - 1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      Y := YLoop - BITMAP_YCENTER;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
    	  X := XLoop - BITMAP_XCENTER;

          MinDistance := High (Integer);

          for Loop := 1 to MAX_POINTS do begin // find the nearest point
              XDistance := X - Points[Loop].x;
      	      YDistance := Y - Points[Loop].y;

              IDistance := Round (SafeHypot (XDistance, YDistance));
              IDistance := IDistance mod Chop;

              if IDistance < MinDistance then MinDistance := IDistance;
              end;

          IValue := MinDistance * Height; // distance to nearest point

          if Concentric then begin
             Distance := SafeHypot (X,Y);
             Value := Sin (Distance/1000 * ConcentricScale) * ConcentricMagnitude;
             IValue := IValue + Round (Value);
             end;

          PatternBytes[YLoop,XLoop]:=Wrap(IValue);
          end;
      end;
end;

procedure TPattern.MakeDiamond;
type
  TDiamondCurveType    = (dcNone, dcInwards, dcOutwards, dcSawtooth);
  TDiamondDistanceType = (ddDiamond, ddSquare, ddCross);
var
  DistanceType: TDiamondDistanceType;
  XScale, YScale: Integer;
  CurveType: TDiamondCurveType;
  CurveFactor: Integer;
  SawToothXLength, SawToothXScale: Integer;
  SawToothYLength, SawToothYScale: Integer;

  Concentric, Spiral: Boolean;
  Petals, SpiralFactor, PaletteScale: Integer;
  ConcentricScale, ConcentricMagnitude: Integer;
  Reflect: Boolean;

  X, Y, XLoop, YLoop, IValue: Integer;
  Angle, Distance, Value: TFloat;
begin 
  PatternType:=ptDiamond;
  case Random (3) of
    0:   DistanceType := ddDiamond;
    1:   DistanceType := ddSquare;
    else DistanceType := ddCross;
  end; // case

  // x:y ratios vary from 1:1 to 2:5

  XScale := 2 + Random (3);
  YScale := 2 + Random (3);

  // both divisible by 2 ?
  if ((XScale mod 2) = 0) and (((YScale mod 2) = 0)) then begin
     XScale := XScale div 2;
     YScale := YScale div 2;
     end;

  // curved only sometimes
  case Random (14) of
    1,2,3 : CurveType := dcInwards;
    4,5,6 : CurveType := dcOutwards;
    7,8,9 : CurveType := dcSawTooth;
    else  CurveType   := dcNone;
  end; // case

  // curviness 150 -> 1000
  CurveFactor := 150 + Random (850);

  SawToothXLength := 20 + Random (60);
  SawToothYLength := 20 + Random (60);
  SawToothXScale  :=  1 + Random (8);
  SawToothYScale  :=  1 + Random (8);

  case Random (6) of
    0   : begin Concentric := False; Spiral := False; end; // neither
    1   : begin Concentric := True;  Spiral := False; end; // concentric
    2   : begin Concentric := False; Spiral := True;  end; // spiral
    else  begin Concentric := True;  Spiral := True;  end; // both
  end; // case

  ConcentricScale := 10 + Random (81);
  ConcentricMagnitude := 10 + Random (91);

  Petals := Succ(Random (6));
  SpiralFactor := Succ(Random (599));
  PaletteScale := 5 + Random (16);
  Reflect := Random(4)=0; // reflect 1 time in 4

  for YLoop := 0 to BITMAP_HEIGHT - 1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      Y := Abs(YLoop - BITMAP_YCENTER);
      if Reflect then Y:=-Y;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
    	  X := Abs(XLoop - BITMAP_XCENTER);
          if Reflect then X:=-X;

          case DistanceType of
            ddDiamond  : IValue := (X * XScale + Y * YScale);
            ddSquare   : IValue := XYToSquareDistance (X * XScale, Y * YScale);
            else         IValue := XYToCrossDistance  (X * XScale, Y * YScale); // ddCross
          end; // case

          case CurveType of
            dcInwards  : IValue := IValue + ((X * Y) div CurveFactor);
            dcOutwards : IValue := IValue - ((X * Y) div CurveFactor);
            dcSawTooth : IValue := iValue
                           + ((SawTooth (XLoop, SawToothXLength) * SawToothXScale) div 10)
                           + ((SawTooth (YLoop, SawToothYLength) * SawToothYScale) div 10);
          end; // case

          Distance := SafeHypot (X,Y);

          if Concentric then begin
             Value := Sin (Distance/1000 * ConcentricScale) * ConcentricMagnitude;
             IValue := IValue + Round (Value);
             end;

          if Spiral then begin
             Angle := XYDistanceToAngle(X,Y,Distance);
             Value :=  Sin (Angle * Petals + Distance/1000 * SpiralFactor);
             IValue := IValue + Scale(Value, -PaletteScale, PaletteScale);
             end;

          PatternBytes[YLoop,XLoop]:=Wrap(IValue);
          end;
      end;
end;

procedure TPattern.MakeDoubleDiamond;
type
  TDiamondCurveType    = (dcNone, dcInwards, dcOutwards, dcSpiral, dcConcentric);
  TDiamondDistanceType = (ddDiamond, ddSquare, ddCross);
var
  DistanceType1,DistanceType2: TDiamondDistanceType;
  XScale1, YScale1, Mod1: Integer;
  XScale2, YScale2, Mod2: Integer;
  CurveType: TDiamondCurveType;
  CurveFactor: Integer;

  Petals, SpiralFactor, PaletteScale: Integer;
  ConcentricScale, ConcentricMagnitude: Integer;
  Reflect: Boolean;

  X, Y, XLoop, YLoop, IValue1,IValue2: Integer;
  Angle, Distance, Value: TFloat;
begin
  PatternType:=ptDoubleDiamond;
  case Random (3) of
    0:   DistanceType1 := ddDiamond;
    1:   DistanceType1 := ddSquare;
    else DistanceType1 := ddCross;
  end; // case

  case Random (3) of
    0:   DistanceType2 := ddDiamond;
    1:   DistanceType2 := ddSquare;
    else DistanceType2 := ddCross;
  end; // case

  // x:y ratios vary from 2:2 to 4

  XScale1 := 2 + Random (3); // first fill is the primary & is larger
  YScale1 := 2 + Random (3);

  // both divisible by 2 ?
  if ((XScale1 mod 2) = 0) and (((YScale1 mod 2) = 0)) then begin
     XScale1 := XScale1 div 2;
     YScale1 := YScale1 div 2;
     end;

  Mod1 := 192 + Random (63);

  // second fill is on overlay, ratio 1 : 1 to 3

  XScale2 := 1 + Random (3);
  YScale2 := 1 + Random (3);

  // both divisible by 2 ?
  if ((XScale2 mod 2) = 0) and (((YScale2 mod 2) = 0)) then begin
     XScale2 := XScale2 div 2;
     YScale2 := YScale2 div 2;
     end;

  Mod2 := 16 + Random (128);

  // make sure both scales not the same
  while XScale1 = XScale2 do XScale2 := XScale2 + Random (2);
  while YScale1 = YScale2 do YScale2 := YScale2 + Random (2);

  // curve the primary diamond sometimes
  case Random (13) of
    1,2,3 : CurveType := dcInwards;
    4,5,6 : CurveType := dcOutwards;
    7,8,9 : CurveType := dcConcentric;
    10..12: CurveType := dcSpiral;
    else    CurveType := dcNone;
  end; // case

  // curviness 150 -> 1000
  CurveFactor := 150 + Random (850);

  ConcentricScale := 10 + Random (81);
  ConcentricMagnitude := 10 + Random (91);

  Petals := Succ(Random (6));
  SpiralFactor := Succ(Random (599));
  PaletteScale := 5 + Random (16);
  Reflect := Random(4)=0; // reflect 1 time in 4

  for YLoop := 0 to BITMAP_HEIGHT - 1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      Y := Abs(YLoop - BITMAP_YCENTER);
      if Reflect then Y:=-Y;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
    	  X := Abs(XLoop - BITMAP_XCENTER);
          if Reflect then X:=-X;

          case DistanceType1 of
            ddDiamond  : IValue1 := (X * XScale1 + Y * YScale1);
            ddSquare   : IValue1 := XYToSquareDistance (X * XScale1, Y * YScale1);
            else         IValue1 := XYtoCrossDistance (X * XScale1, Y * YScale1); // ddCross
          end; // case
          IValue1 := IValue1 mod Mod1;


          case DistanceType2 of
            ddDiamond    : IValue2 := (X * XScale2 + Y * YScale2);
            ddSquare     : IValue2 := XYToSquareDistance (X * XScale2, Y * YScale2);
            else           IValue2 := XYtoCrossDistance     (X * XScale2, Y * YScale2); // ddCross
          end; // case

          Distance := SafeHypot (X,Y);

          case CurveType of
            dcInwards    : IValue2 := IValue2 + ((X * Y) div CurveFactor);
            dcOutwards   : IValue2 := IValue2 - ((X * Y) div CurveFactor);
            dcConcentric : IValue2 := IValue2 + Round (Sin (Distance/1000 * ConcentricScale) * ConcentricMagnitude);
            dcSpiral     : begin Angle   := XYDistanceToAngle (X, Y, Distance);
                                 Value   := Sin (Angle * Petals + Distance/1000 * SpiralFactor);
                                 IValue2 := IValue2 + Scale(Value, -PaletteScale, PaletteScale);
                           end;
          end; // case

          IValue2 := IValue2 mod Mod2;
          PatternBytes[YLoop,XLoop]:=Wrap(IValue1+IValue2);
          end;
      end;
end;

procedure TPattern.MakeFloralMorph;
var
  ConcentricScale, ConcentricStrength: Integer;
  ConcentricScale2, ConcentricStrength2: Integer; // second lot

  CircularRays: Integer;
  ColumnRays, ColumnRayStrength: Integer;

  SpiralFactor, PetalFactor, Petals: Integer; // flowers
  SpiralFactor2, PetalFactor2, Petals2: Integer;
  FlowerStrength, Flower2Strength: Integer;

  HasConcentric1, HasConcentric2: Boolean;
  HasCircular, HasColumnar: Boolean;
  HasFlower1, HasFlower2: Boolean;

  XLoop, YLoop, X, Y: Integer;
  Angle, Distance, Value, PetalValue, PetalValue2: TFloat;
begin
  PatternType:=ptFloralMorph;
  ConcentricScale    := 5  + Random (36); // 5 to 40
  ConcentricStrength := 50 + Random (351); // 50 - 400

  // second set must have different scale
  ConcentricScale2 := ConcentricScale;
  while Abs (ConcentricScale - ConcentricScale2) < 5 do
    ConcentricScale2 := 5 + Random (36); // 5 to 40

  // and different strength
  ConcentricStrength2 := ConcentricStrength;
  while Abs (ConcentricStrength - ConcentricStrength2) < 5 do
    ConcentricStrength2 := 50 + Random (351); // 50 - 400

  // rays
  // both 0 -> concentric circles
  // circular rays only -> ice-cream swirl/ wobly spiral
  // column rays only -> spider / eyes

  // don't let both # rays be the same
  CircularRays := 0;
  ColumnRays := 0;
  while CircularRays = ColumnRays do begin
    CircularRays := Succ(Random (4));
    ColumnRays := 2 + Random(5); // 2 - 6 lobes
    end;
  ColumnRayStrength := 6 + Random (300); // 6 - 306

  Petals := Succ(Random (7));
  SpiralFactor := Random (100) - 50;
  case Random (3) of // petal factor from 0 to 50, but usually low
  	0, 1: PetalFactor := Random (10);
  	else  PetalFactor := Random (51);
  end; // case

  // different number of petals
  Petals2 := Petals;
  while Petals2 = Petals do Petals2 := Succ(Random (7));

  // different spiral factor
  SpiralFactor2 := SpiralFactor;
  while Abs (SpiralFactor - SpiralFactor2) < 10 do
    SpiralFactor2 := Random (60) - 30;


  case Random (3) of // petal factor from 0 to 70, but usually low
       0:   PetalFactor2 := Random (10);
       1:   PetalFactor2 := Random (50);
       else PetalFactor2 := Random (71);
  end; // case

  if PetalFactor + Petalfactor2 < 4 then
     PetalFactor:=2; // try to avoid 'blank' images

  FlowerStrength  := 600 + Random (400);
  Flower2Strength := 600 + Random (400);

  repeat
  	HasFlower1     := RandomBoolean;
  	HasFlower2     := RandomBoolean;
  	HasConcentric1 := RandomBoolean;
  	HasConcentric2 := RandomBoolean;
  	HasCircular    := RandomBoolean;
  	HasColumnar    := RandomBoolean;
  until (HasConcentric1 or HasConcentric2 or HasCircular or HasColumnar)
        and
        (HasCircular or HasColumnar or (HasFlower1 and HasFlower2))
        and
  	(HasFlower1 or HasFlower2);

  for YLoop := 0 to BITMAP_HEIGHT -1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      Y := YLoop - BITMAP_YCENTER;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
          X := XLoop - BITMAP_XCENTER;

          Distance :=SafeHypot(X,Y);

          Angle := XYDistanceToAngle (X,Y,Distance);

          Value := 0.0;
          PetalValue := 0.0; PetalValue2 := 0.0;

          // concentric circles
          if HasConcentric1 then
             Value := Value + Sin (Distance/1000 * ConcentricScale) * ConcentricStrength;

          if HasConcentric2 then
             Value := Value + Sin (Distance/1000 * ConcentricScale2) * ConcentricStrength2;

          // circular rays
          if HasCircular then
             Value := Value + Scale(Angle, 0, RADIANS_FULL_CIRCLE) * CircularRays;

          // columnar rays
          if HasColumnar then
             Value := Value + Sin(Angle * ColumnRays) * ColumnRayStrength;

          // flowers
          if HasFlower1 then begin
             PetalValue := Sin ((Angle*Petals) + (Distance/1000 * SpiralFactor)) * (FlowerStrength / 1000);
             PetalValue := Round(PetalValue + Scale (PetalValue, -1.0, 1.0) + Round (Distance/10 * PetalFactor));
             end;

          if HasFlower2 then begin
             PetalValue2:= Sin ((Angle*Petals2) + (Distance/1000 * SpiralFactor2)) * (Flower2Strength / 1000);
             PetalValue2:= Round(PetalValue2 + Scale (PetalValue2, -1.0, 1.0) + Round (Distance/10 * PetalFactor2));
             end;

          PatternBytes[YLoop,XLoop] :=Wrap(Round(Round(Value) + PetalValue + PetalValue2));
          end;
      end;
end;

procedure TPattern.MakeFlower;
var X,Y,XLoop,YLoop,Petals,PetalFactor,SpiralFactor:Integer;
    Angle,Distance,Value:TFloat;
begin
  PatternType:=ptFlower;
  Petals := Succ(Random(7));
  SpiralFactor := Random(100) - 50;
  case Random(3) of
    0  : PetalFactor := 2 + Random(10);  // + 2: try to avoid 'blank' images
    1  : PetalFactor := 2 + Random(50);
    else PetalFactor := 2 + Random(100);
  end; // case

  for YLoop := 0 to BITMAP_HEIGHT -1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      Y := YLoop - BITMAP_YCENTER;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
          X := XLoop - BITMAP_XCENTER;

          Distance :=SafeHypot(X,Y);

          Angle := XYDistanceToAngle (X,Y,Distance);

          Value := Sin ((Angle*Petals) + (Distance/1000 * SpiralFactor));

          Value := Scale(Value, -1.0, 1.0) + Round(Distance / 10 * PetalFactor);

          PatternBytes[YLoop,XLoop] :=Wrap(Round(Value));
          end;
      end;
end;

procedure TPattern.MakeLiquidCrystal;
type
  TDistortType = (dtNone, dtCircle, dtSquare, dtDiamond, dtCross);
var
  // first pattern params
  Height1,
  xSinDiv1, xCosDiv1, ySinDiv1, yCosDiv1, xySinDiv1, xyCosDiv1,
  xOffset1, yOffset1,
  Mod1: Integer;
  DistortType1: TDistortType;

// second pattern params
  Height2,
  xSinDiv2, xCosDiv2, ySinDiv2, yCosDiv2, xySinDiv2, xyCosDiv2,
  xOffset2, yOffset2,
  Mod2: Integer;
  DistortType2: TDistortType;

// fade params
  RippleSize: Integer;
  FadeType: TDistortType;


  XLoop, YLoop, XOff1, YOff1, XOff2, YOff2,
  X, Y, IX, IY: Integer;
  Value1, Value2,
  Distance, FadeDistance, Factor1, Factor2: TFloat;

begin
  PatternType:=ptLiquidCrystal;
  // set the divisors etc. for the first background
  Height1   := 10 + Random (30) + Random (30);

  xSinDiv1  := 15 + Random (50);
  yCosDiv1  := 15 + Random (50);

  xCosDiv1  := 15 + Random (20);
  ySinDiv1  := 15 + Random (20);

  xySinDiv1 := 15 + Random (50);
  xyCosDiv1 := 15 + Random (50);

  xOffset1  := 50 - Random (100);
  yOffset1  := 50 - Random (100);

  Mod1      := 20 + Random (40);

  if   RandomBoolean then
       DistortType1:=  TDistortType (Random (Ord(High(TDistortType)) + 1))
  else DistortType1 := dtNone;


// set the divisors etc. for 2nd pattern

// height must be dissimilar to first one
  Height2   := 25 + Random (30) + Random (30);
  while (Abs (Height2 - Height1) < 20) do Height2 := Height2 + 10;

  xSinDiv2  := 15 + Random (50);
  yCosDiv2  := 15 + Random (50);

  xCosDiv2  := 15 + Random (20);
  ySinDiv2  := 15 + Random (20);

  xySinDiv2 := 15 + Random (50);
  xyCosDiv2 := 15 + Random (50);

  xOffset2  := 50 - Random (100);
  yOffset2  := 50 - Random (100);

  Mod2      := 20 + Random (30) + Random (30);

  if RandomBoolean then DistortType2 := dtNone
  else DistortType2 :=  TDistortType (Random (Ord(High(TDistortType)) + 1));

  RippleSize := 10 + Random (40) + Random (40);

// fade as circle mostly, never as none
  if RandomBoolean then FadeType := dtCircle
  else repeat FadeType := TDistortType (Random (Ord(High(TDistortType)) + 1));
       until  FadeType<>dtNone;

  for YLoop := 0 to BITMAP_HEIGHT -1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      IY    := YLoop - BITMAP_YCENTER;
      Y     := Abs(IY);
      YOff1 := IY + YOffset1;
      YOff2 := IY + YOffset2;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
          IX    := XLoop - BITMAP_XCENTER;
          X     := Abs(IX);
          XOff1 := IX + XOffset1;
          XOff2 := IX + XOffset2;

          Distance :=SafeHypot(X,Y);

          case FadeType of
            dtCircle    : FadeDistance := Distance / RippleSize;
            dtSquare    : FadeDistance := XYToSquareDistance(X,Y) / RippleSize;
            dtDiamond   : FadeDistance := (X + Y) / RippleSize;
            else          FadeDistance := XYToCrossDistance(X,Y) / RippleSize; // dtCross
          end; { case FadeType }


          // fade it in and out
          Factor1 := 0.5 + Cos (FadeDistance);
          if Factor1 < 0.0 then Factor1 := 0.0;

          // don't bother calculating if it is faded out

          if Factor1 <= 0.0 then Value1 := 0.0
          else begin
            Value1 := Height1 * (
                      Sin(IX / xSinDiv1) + Cos(IY / yCosDiv1) +
                      Cos(XOff1 / xCosDiv1) + Sin(YOff1 / ySinDiv1) +
                      Sin((IX + YOff1) / xySinDiv1));

            // distort
            case DistortType1 of
              dtNone       :;
              dtCircle     : Value1 := Value1 + (Floor (Distance) mod Mod1);
              dtSquare     : Value1 := Value1 + (XYToSquareDistance(X,Y) mod Mod1);
              dtDiamond    : Value1 := Value1 + ((X + Y) mod Mod1);
              dtCross      : Value1 := Value1 + (XYToCrossDistance(X,Y) mod Mod1);
            end; // case

            Value1 := Value1 * Factor1;
            end;

     	  Factor2 := 0.5 + Cos ((FadeDistance)+ RADIANS_HALF_CIRCLE);
          if Factor2 < 0.0 then Factor2 := 0.0;

          if Factor2 <= 0.0 then Value2 := 0.0
          else begin
            // trig function 2 to make a lumpy background
            Value2 := Height2 * (
                      Sin(IX / xSinDiv2) + cos(IY / yCosDiv2) +
                      Cos(XOff2 / xCosDiv2) + Sin(YOff2 / ySinDiv2) +
                      Cos(Distance / xyCosDiv2) );

            // distort
            case DistortType2 of
              dtNone       :;
              dtCircle     : Value2 := Value2 + (Floor (Distance) mod Mod2);
              dtSquare     : Value2 := Value2 + (XYToSquareDistance(X,Y) mod Mod2);
              dtDiamond    : Value2 := Value2 + ((X + Y) mod Mod2);
              dtCross      : Value2 := Value2 + (XYToCrossDistance(X,Y) mod Mod2);
            end; // case

            Value2 := Value2 * Factor2;
            end;

          PatternBytes[YLoop,XLoop] :=Wrap(Floor(Value1+Value2));
          end;
      end;
end;

procedure TPattern.MakePlasmaCloud;
var PixelCount:Integer;

  function RandomBetweenH(const Point1,Point2:TPoint):Byte;
  begin
    Result:=Wrap( ((PatternBytes[Point1.y,Point1.x] + PatternBytes[Point2.y,Point2.x]) div 2) +
                  Random (2 * (Abs (Point2.x - Point1.x))) +
                  Point1.x -Point2.x );
  end;

  function RandomBetweenV(const Point1,Point2:TPoint):Byte;
  begin
    Result:=Wrap( ((PatternBytes[Point1.y,Point1.x] + PatternBytes[Point2.y,Point2.x]) div 2) +
                  Random (2 * (Abs (Point2.y - Point1.y))) +
                  Point1.y -Point2.y );
  end;

  function RandomBetweenD (const TopLeft, BottomRight, TopRight, BottomLeft: TPoint): Byte;
  begin
    Result:=Wrap( ((PatternBytes[TopLeft.y,TopLeft.x] +
                    PatternBytes[BottomRight.y,BottomRight.x] +
                    PatternBytes[TopRight.y,TopRight.x] +
                    PatternBytes[BottomLeft.y,BottomLeft.x]) div 4) +
                  Random(2 * Abs(BottomRight.x - TopLeft.x + BottomRight.y - TopLeft.y)) -
                  BottomRight.x + TopLeft.x - BottomRight.y + TopLeft.y );
  end;

  procedure PlasmaRectangle(Bounds:TRect);
  var TopRight, BottomLeft, Mid: TPoint;
  begin
    if (Bounds.Right > Bounds.Left) or (Bounds.Bottom > Bounds.Top) then begin

       if PixelCount>=BITMAP_WIDTH*8 then begin
          PixelCount:=0;
          SleepEx(0,False);
          end;

       // find the middlepoints of the edges
       Mid        := Point ((Bounds.Left + Bounds.Right) div 2,
                            (Bounds.Top  + Bounds.Bottom) div 2);

       TopRight   := Point (Bounds.Right, Bounds.Top);

       BottomLeft := Point (Bounds.Left, Bounds.Bottom);

       // populate the midpoints of the edges

       // midpoint of top edge
       if PatternBytes [Bounds.Top,Mid.x] = 0 then begin
          PatternBytes [Bounds.Top,Mid.x] := RandomBetweenH (Bounds.TopLeft, TopRight);
          Inc(PixelCount);
          end;

       // midpoint of bottom edge
       if PatternBytes [Bounds.Bottom,Mid.x] = 0 then begin
          PatternBytes [Bounds.Bottom,Mid.x] := RandomBetweenH (BottomLeft,Bounds.BottomRight);
          Inc(PixelCount);
          end;

       // midpoint of left edge
       if PatternBytes [Mid.y,Bounds.Left] = 0 then begin
          PatternBytes [Mid.y,Bounds.Left] := RandomBetweenV(Bounds.TopLeft,BottomLeft);
          Inc(PixelCount);
          end;

       // midpoint of right edge
       if PatternBytes [Mid.y,Bounds.Right] = 0 then begin
          PatternBytes [Mid.y,Bounds.Right] := RandomBetweenV(TopRight,Bounds.BottomRight);
          Inc(PixelCount);
          end;

      // recurse
      if (Mid.x > Bounds.Left) or (Mid.y > Bounds.Top) then begin
         if PatternBytes [Mid.y,Mid.x] = 0 then begin
            PatternBytes [Mid.y,Mid.x] := RandomBetweenD (Bounds.TopLeft, Bounds.BottomRight, TopRight, BottomLeft);
            Inc(PixelCount);
            end;

         PlasmaRectangle (Rect (Bounds.Left, Bounds.Top, Mid.x, Mid.y));
         PlasmaRectangle (Rect (Bounds.Left, Mid.y, Mid.x, Bounds.Bottom));
         PlasmaRectangle (Rect (Mid.x, Bounds.Top, Bounds.Right, Mid.y));
         PlasmaRectangle (Rect (Mid.x, Mid.y, Bounds.Right, Bounds.Bottom));
         end;
    end;
  end;

begin // MakePlasmaCloud
  PatternType:=ptPlasmaCloud;
  FillChar(PatternBytes,SizeOf(PatternBytes),0);
  PatternBytes [0              ,             0] := Wrap(RandomNonZeroByte);
  PatternBytes [0              ,BITMAP_WIDTH-1] := Wrap(RandomNonZeroByte);
  PatternBytes [BITMAP_HEIGHT-1,             0] := Wrap(RandomNonZeroByte);
  PatternBytes [BITMAP_HEIGHT-1,BITMAP_WIDTH-1] := Wrap(RandomNonZeroByte);

  PixelCount := 0;
  PlasmaRectangle (Classes.Rect(0,0,BITMAP_WIDTH-1,BITMAP_HEIGHT-1));
end;

procedure TPattern.MakePrimalOrbit;
const
  MAX_PLANETS = 8;
type
  TPlanet = record
    SpiralFactor, PetalFactor, Petals,
    XCenter, YCenter,
    XCurrent, YCurrent,
    Stretch,
    ConcentricStrength,
    XOffset, YOffset : Integer;
    ConcentricPos: Boolean;
  end;

var
  Planets: Integer;
  Planet : array[0..MAX_PLANETS] of TPlanet;
  //ConcentricScale: Integer;
  OrbitNumber: Integer;

  Loop, Distance, XDisp, YDisp, XLoop, YLoop, IValue: Integer;
  Angle: TFloat;

  function PlanetDistanceDecay (const Planet:TPlanet; Distance: TFloat): TFloat;
  const	FULL_THRESHHOLD: TFloat = 20.0;
  var StretchDistance: TFloat;
  begin
    if Distance < FULL_THRESHHOLD then Result := 1.0
    else begin
       StretchDistance := (Distance - FULL_THRESHHOLD) / Planet.Stretch;
       if StretchDistance > RADIANS_QUARTER_CIRCLE then Result := 0.0
       else Result := Cos (StretchDistance);
       end;
  end;

  procedure PlanetInitialize(var Planet:TPlanet);
  begin
    with Planet do begin
      FillChar(Planet,SizeOf(Planet),0);

      if   OrbitNumber = 3 then
           Petals := Succ(Random (4))
      else Petals := Succ(Random (7));

      //SpiralFactor1 := Random (100) - 50; }
      SpiralFactor := Random (10) - 5;

      case Random (3) of // petal factor from 0 to 100, but usually low
  	0: PetalFactor := Random (10);
  	1: PetalFactor := Random (50);
  	2: PetalFactor := Random (100);
      end; // case

      Stretch := 200 + Random (400);

      ConcentricStrength := 5 + Random (20);
      ConcentricPos := RandomBoolean;

      XCenter := BITMAP_XCENTER;
      YCenter := BITMAP_YCENTER;
      end;
  end;

  procedure PlanetMakeTwin (const Planet1:TPlanet; var Planet2: TPlanet);
  begin
    Planet2.Petals       := Planet1.Petals;
    Planet2.SpiralFactor := Planet1.SpiralFactor;
    Planet2.PetalFactor  := Planet1.PetalFactor;
    Planet2.Stretch      := Planet1.Stretch;
  end;

  procedure PlanetDisplace(var Planet:TPlanet; Offset: TPoint);
  begin
    Inc(Planet.XCenter,Offset.x);
    Inc(Planet.YCenter,Offset.y);
  end;

  function PlanetOrbit1(const Planet:TPlanet): Integer;
  var
  Distance, Angle, Value: TFloat;
  begin
    with Planet do begin
      Distance := SafeHypot (XOffset, YOffset);
      Angle := XYDistanceToAngle(XOffset, YOffset, Distance);

      Value := Sin ((Angle* Petals) + (Distance/1000 * SpiralFactor));
      Result := Scale(Value, -1.0, 1.0) +
  	        Round ((Distance / 10 * PetalFactor) * PlanetDistanceDecay (Planet,Distance));
      end;
  end;

  function PlanetOrbit2(const Planet:TPlanet): Integer;
  var Distance: TFLoat;
  begin
    with Planet do begin
      Distance := SafeHypot (XOffset, YOffset);
      Result :=  Round((Distance / 10 * PetalFactor) * PlanetDistanceDecay (Planet,Distance / 10));
      end;
  end;

  function PlanetOrbit3(const Planet:TPlanet): Integer;
  var Angle, Distance: TFloat;
      Conc: Integer;
  begin
    with Planet do begin
      Distance := SafeHypot (XOffset, YOffset);
      Angle := XYDistanceToAngle(XOffset, YOffset, Distance);

      // circular rays
      Angle := Angle * Petals;
      while Angle >= RADIANS_FULL_CIRCLE do Angle := Angle - RADIANS_FULL_CIRCLE;

      Result := Scale (Angle, 0.0, RADIANS_FULL_CIRCLE); // rays

      if   (Distance > 0.0) and (ConcentricStrength > 0) then
           Conc := Round (Distance / ConcentricStrength)
      else Conc := 0;

      if   ConcentricPos then
           Result := Result + Conc
      else Result := Result - Conc;
      end;
  end;

  procedure PlanetInitPair (var Planet1, Planet2: TPlanet; XDisplacement, YDisplacement: Integer);
  begin
    //PlanetInitialize(Planet1);
    PlanetMakeTwin  (Planet1,Planet2);
    PlanetDisplace  (Planet1,Point ( XDisplacement,  YDisplacement));
    PlanetDisplace  (Planet2,Point (-XDisplacement, -YDisplacement));
  end;

begin // MakePrimalOrbit
  PatternType:=ptPrimalOrbit;
  OrbitNumber := Succ(Random(2)); // 1..2: 'Orbit 3' does not produce interesting patterns

  //ConcentricScale := 5 + Random (36); {5 to 40 }

  // 4, 6 or 8 planets
  Planets := (2 + Random (3)) * 2;
  //Planets := 8;

  for Loop := 1 to Planets do PlanetInitialize(Planet[Loop]);

  // calculate the centers - opposite each other, centered on the screen
  //Distance := 50 + Random (140);
  // if there are more planets, they are more closely spaced
  Distance := 20 + Random (70) + (80 - (Planets * 10));
  Angle    := RandomFloat(RADIANS_FULL_CIRCLE);
  XDisp    := Round(Sin (Angle) * Distance);
  YDisp    := Round(Cos (Angle) * Distance);
  PlanetInitPair (Planet[1], Planet[2], XDisp, YDisp);

  // next 2 are 90 degrees off and 3/2 as far out (far out... mmm)
  Distance := Distance * 3 div 2;
  Angle    := Angle + RADIANS_QUARTER_CIRCLE;
  XDisp    := Round(Sin (Angle) * Distance);
  YDisp    := Round(Cos (Angle) * Distance);
  PlanetInitPair (Planet[3], Planet[4], XDisp, YDisp);

  // next 2 are 90 degrees off and 3/2 as far out (far out... mmm)
  Distance := Distance * 3 div 2;
  Angle    := Angle + RADIANS_QUARTER_CIRCLE;
  XDisp    := Round(Sin (Angle) * Distance);
  YDisp    := Round(Cos (Angle) * Distance);
  PlanetInitPair (Planet[5], Planet[6], XDisp, YDisp);

  // next 2 are 90 degrees off and 3/2 as far out (far out... mmm)
  Distance := Distance * 3 div 2;
  Angle    := Angle + RADIANS_QUARTER_CIRCLE;
  XDisp    := Round(Sin (Angle) * Distance);
  YDisp    := Round(Cos (Angle) * Distance);
  PlanetInitPair (Planet[7], Planet[8], XDisp, YDisp);

  for YLoop := 0 to BITMAP_HEIGHT -1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      for Loop := 1 to Planets do begin
          Planet[Loop].YCurrent := YLoop;
          Planet[Loop].YOffset  := Planet[Loop].YCurrent-Planet[Loop].YCenter;
          end;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin

          IValue := 0;

          for Loop := 1 to Planets do begin
              Planet[Loop].XCurrent := XLoop;
              Planet[Loop].XOffset  := Planet[Loop].XCurrent-Planet[Loop].XCenter;
              case OrbitNumber of
                1: IValue := IValue + PlanetOrbit1(Planet[Loop]);
                2: IValue := IValue + PlanetOrbit2(Planet[Loop]);
                3: IValue := IValue + PlanetOrbit3(Planet[Loop]);
              end; // case
              end;

          PatternBytes[YLoop,XLoop] :=Wrap(IValue);
          end;
      end;
end;

procedure TPattern.MakeSnakes;
type
  TSnakesSign = (ssNegative, ssZero, ssPositive);
const
  Sign        : array[TSnakesSign] of Integer = (-1,1,1);
var
  MainWavePeriod: Integer;
  MainWaveStrength: TFloat;

  xWavePeriod, yWavePeriod: Integer;
  xWaveStrength, yWaveStrength: Integer;

  AddX: Boolean;
  AddStrength: Integer;

  xSign, ySign, DistSign: TSnakesSign;
  DistStrength, xAngle, yAngle: Integer;

  Adds: Integer;

  XLoop, YLoop, X, Y, CenterDist, DistanceFn: Integer;
  Value: TFLoat;

begin
  PatternType:=ptSnakes;

  MainWavePeriod := 50 + Random (401); // 50 - 450
  MainWaveStrength := 1.0 + Random (201) / 100.0; // 1 - 3
  // no low strength for low periods
  if (MainWavePeriod < 200) and (MainWaveStrength < 2.0) then
     MainWaveStrength := MainWaveStrength + ((200 - MainWavePeriod) / 200);

  xWavePeriod := 40 + Random (80); // 40 - 120
  yWavePeriod := 40 + Random (80); // 40 - 120

  xWaveStrength := 2 + Random (1); // 2 - 3
  yWaveStrength := 2 + Random (1); // 2 - 3

  AddX         := RandomBoolean;

  case Random (4) of // 1 - 5, strong bias to 1 and 2
    0:   AddStrength := Succ(Random (1));
    1:   AddStrength := Succ(Random (2));
    2:   AddStrength := Succ(Random (3));
    else AddStrength := Succ(Random (4));
  end; // case

  repeat
    Adds := 0;

    case Random (4) of
      0:	DistSign := ssNegative;
      1:	DistSign := ssPositive;
      else	DistSign := ssZero;
    end; // case

    xSign := TSnakesSign(Random(Ord(High(TSnakesSign))));
    ySign := TSnakesSign(Random(Ord(High(TSnakesSign))));

    // make sure we have at least 2 of these!
    if xSign <> ssZero then Inc (Adds);
    if ySign <> ssZero then Inc (Adds);
    if DistSign <> ssZero then Inc (Adds);

  // must have at least 2 factors, or concentric on it's own
  until (Adds > 1) or (DistSign <> ssZero);

  DistStrength := 10 + Random (21); // 10 - 30

  xAngle := 7 + Random (7); // (7 - 13) to vary the angle slightly from 45 deg
  yAngle := 7 + Random (7);

  for YLoop := 0 to BITMAP_HEIGHT -1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      Y := YLoop - BITMAP_YCENTER;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
          X := XLoop - BITMAP_XCENTER;

          // variations:
          // (+- )x (+-) y
          // (+-) Dist (+-) x
          // (+-) Dist (+-) y
          // (+-) Dist (+-) x (+-) y
          //
          // each of x,y,Dist has sign, magnitude, and inclusion

          DistanceFn := (Sign[xSign]*((XLoop * xAngle) div 10)) +
                        (Sign[ySign]*((YLoop * yAngle) div 10));


          if DistSign <> ssZero then begin
             CenterDist := Round (SafeHypot (X,Y));
             DistanceFn := DistanceFn +
                           Sign[DistSign]*((CenterDist * DistStrength) div 10);
             end;

          // minor waves
          DistanceFn := DistanceFn +
                        (SawTooth (xLoop, xWavePeriod) div xWaveStrength) +
                        (SawTooth (yLoop, yWavePeriod) div yWaveStrength);

          // main wave
          Value := SawTooth (DistanceFn, MainWavePeriod) * MainWaveStrength;

          if   AddX then
               Value := Value + (XLoop div AddStrength)
          else Value := Value + (YLoop div AddStrength);

          PatternBytes[YLoop,XLoop] :=Wrap(Round(Value));
          end;
      end;
end;

procedure TPattern.MakeTwistedPyramids;
type
  TBaseType = (btPyramids, btDiamond, btCross);

var
  PyramidWidth, PyramidHeight: Integer;
  BaseType: TBaseType;
  XScale, YScale,
  ConcentricScale, ConcentricStrength,
  SinWidth, SinStrength: Integer;
  InitialAngle, Twist: TFloat;
  Clockwise: Boolean;
  Rays: Integer;

  XLoop, YLoop, X, Y, RX, RY: Integer;
  Angle, Distance, Value: TFloat;

begin
  PatternType:=ptTwistedPyramids;

  BaseType := TBaseType (Random (Ord (High (TBaseType))));

  PyramidWidth := 50 + Random (40) + Random (40);
  PyramidHeight := Succ (Random (2));

  XScale := Succ (Random (2));
  YScale := Succ (Random (2));
  // both divisible by 2 ?
  if ((XScale mod 2) = 0) and (((YScale mod 2) = 0)) then begin
     XScale := XScale div 2;
     YScale := YScale div 2;
     end;

  SinWidth := 10 + Random (10);
  SinStrength := 30 + Random (30);

  ConcentricScale := Succ (Random (20));
  ConcentricStrength := 40 + Random (480);

  InitialAngle := RandomFloat (RADIANS_HALF_CIRCLE);
  Twist := 500 + Random (500);

  Clockwise := RandomBoolean;

  // number of circular rays - usually none, sometimes 1 or more
  case Random (7) of
    0..3: Rays := 0;
    4..5: Rays := 1;
    else  Rays := Random (5);
  end; // case

  for YLoop := 0 to BITMAP_HEIGHT -1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      Y := YLoop - BITMAP_YCENTER;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
          X := XLoop - BITMAP_XCENTER;

          Distance := SafeHypot (X,Y);

          // rotate !
          RX := X;
          RY := Y;
          Angle := InitialAngle;
          if   Clockwise then
               Angle := Angle - (Distance / Twist)
          else Angle := Angle + (Distance / Twist);
          Angle := Angle + Sin (Distance / SinWidth) / SinStrength;
          RotateCounterClockwise (RX, RY, Angle);

          case BaseType of
            btPyramids: Value := SawTooth (RX, PyramidWidth) * PyramidHeight +
                                 SawTooth (RY, PyramidWidth) * PyramidHeight;
            btDiamond : Value := (Abs(RX) * XScale) + (Abs(RY) * YScale);
            else        Value := XYToCrossDistance (RX * XScale, RY * YScale); // btCross
          end; // case

          // concentric circles
          Value := Value + Sin (Distance/1000 * ConcentricScale) * ConcentricStrength;

          // circular rays
          if Rays > 0 then begin
             Angle := XYDistanceToAngle (X, Y, Distance);
             Value := Value + Scale(Angle, 0, RADIANS_FULL_CIRCLE) * Rays;
             end;

          PatternBytes[YLoop,XLoop] :=Wrap(Round(Value));
          end;
      end;
end;

procedure TPattern.MakeFan;
var
  XLoop, YLoop, X, Y, XCenter, YCenter, Petals, Sign: Integer;
  Angle, Distance, Value: TFloat;
begin
  PatternType:=ptFan;
  Petals := 2 + Random(4); // 2..5
  if RandomBoolean then Sign:=1
  else Sign:=-1;

  if Random(20)<>0 then begin
     XCenter := BITMAP_XCENTER; YCenter := BITMAP_YCENTER;
     end
  else begin
     XCenter := 10 + Random(BITMAP_WIDTH -20);
     YCenter := 10 + Random(BITMAP_HEIGHT-20);
     end;

  for YLoop := 0 to BITMAP_HEIGHT -1 do begin
      if YLoop mod 8 = 0 then SleepEx(0,False);

      Y := YLoop - YCenter; //BITMAP_YCENTER;

      for XLoop := 0 to BITMAP_WIDTH - 1  do begin
          X := XLoop - XCenter; //BITMAP_XCENTER;

          Distance := SafeHypot (X,Y);
          Angle    := XYDistanceToAngle(X,Y,Distance);

          Value := Round( RangeSize * Angle / RADIANS_FULL_CIRCLE); // radiant
          Value := (Petals * Value) + (Sign * XYToSquareDistance(X,Y));

          PatternBytes[YLoop,XLoop] :=Wrap(Round(Value));
          end;
      end;
end;

procedure TPattern.MakeTitle;
var Font:TFont;
begin
  try Font:=TFont.Create;
      try     Font.Name:='Arial';
              Font.Size:=26;
              MakeText(Mandala.Title,-1,-1,Font);
              Font.Size:=12;
              MakeText(Mandala.SubTitle,-1,BITMAP_HEIGHT-10-Abs(Font.Height),Font);
      finally Font.Free;
      end;
  except on E:Exception do;
  end;
end;

procedure TPattern.MakePriorPatternType;
begin
  if PatternType=Low(PatternType) then MakePatternType(High(PatternType))
  else MakePatternType(Pred(PatternType));
end;

procedure TPattern.MakeNextPatternType;
begin
  if PatternType=High(PatternType) then MakePatternType(Low(PatternType))
  else MakePatternType(Succ(PatternType));
end;

procedure TPattern.MakePatternType(NewPatternType:TPatternType);
begin
  PatternType:=NewPatternType;

  case PatternType of
    ptAmoeba           : MakeAmoeba;
    ptConcentric       : MakeConcentric;
    ptConstellations   : MakeConstellations;
    ptCutGlass         : MakeCutGlass;
    ptDiamond          : MakeDiamond;
    ptDoubleDiamond    : MakeDoubleDiamond;
    ptFloralMorph      : MakeFloralMorph;
    ptFlower           : MakeFlower;
    ptLiquidCrystal    : MakeLiquidCrystal;
    ptPlasmaCloud      : MakePlasmaCloud;
    ptPrimalOrbit      : MakePrimalOrbit;
    ptSnakes           : MakeSnakes;
    ptTwistedPyramids  : MakeTwistedPyramids;
    ptFan              : MakeFan;
  end; // case
end;

procedure TPattern.MakeText(const Text:String; XOffset,YOffSet:Integer; TextFont:TFont);
var X,Y:Integer; TextSize:TSize; p:PRGBVector; BitMap:TBitMap;
begin
  try BitMap:=TBitMap.Create;
      try
        with BitMap do with Canvas do begin
          Width:=BITMAP_WIDTH;
          Height:=BITMAP_HEIGHT;
          PixelFormat:=pf24bit;

          Font.Assign(TextFont);
          Brush.Color := clWhite;
          FillRect (BITMAP_RECT);
          Font.Color := clBlack;
          TextOut (0,0,Text);

          TextSize := TextExtent (Text);
          if XOffset<0 then XOffset:= (BITMAP_WIDTH -TextSize.cx) div 2;
          if YOffset<0 then YOffset:= (BITMAP_HEIGHT-TextSize.cy) div 2;

          for Y := 0 to Pred(Min(BITMAP_HEIGHT,TextSize.cy)) do
              if Y+YOffset<BITMAP_HEIGHT then begin
                 p := ScanLine[Y];
                 for X := 0 to Pred(Min(BITMAP_WIDTH,TextSize.cx)) do
                     if (p[X].r = 0) and
                        (X+XOffset<BITMAP_WIDTH) then
                        PatternBytes[Y+YOffset,X+XOffset]:=
                        Wrap(PatternBytes[Y+YOffset,X+XOffset]+ 25 + Random(5));
                 end;
          end;
      finally
        BitMap.Free;
      end;
  except on E:Exception do;
  end;
end;

procedure TPattern.MakeRandomPattern;
begin
  MakePatternType(RandomPatternType);
end;

function  TPattern.RangeSize:Integer;
begin
  Result:=Succ(RangeHigh-RangeLow);
end;

function TPattern.Scale(Value,Low,High: TFloat):Byte;
var Range: TFloat;
begin
  if Value<Low  then Value:=Low;
  if Value>High then Value:=High;
  Range:=High-Low;
  Result:=RangeLow+(Round((Value - Low) / Range) * Pred(RangeSize));
end;

function  TPattern.Wrap(Value:Integer):Byte;
begin
  while Value<RangeLow do Inc(Value,RangeSize);
  Result:=RangeLow+(Value mod RangeSize);
end;

procedure TPattern.AlphaBlend(const StartPatternBytes,EndPatternBytes:TPatternBytes; Pct:Integer);
var i:Integer; pa,pb:PByte; AlphaBlendTable:PAlphaBlendTable;
begin
  AlphaBlendTable:=Addr(AlphaBlendTables[Pct]);
  PatternBytes   :=StartPatternBytes;
  pa             :=Addr(PatternBytes   [0,0]);
  pb             :=Addr(EndPatternBytes[0,0]);
  for i:=0 to SizeOf(PatternBytes)-1 do begin
      Inc(pa^,AlphaBlendTable[pb^-pa^]);
      Inc(pa); Inc(pb);
      end;
end;

function  TPattern.PixelMorph(TargetPattern:TPattern):Boolean;
var i,a,b,Value:Integer; pa,pb:PByte;
begin
  Result:=False;
  pa:=Addr(PatternBytes  [0,0]);
  pb:=Addr(TargetPattern.PatternBytes[0,0]);
  for i:=0 to SizeOf(PatternBytes)-1 do begin
      a:=pa^; b:=pb^; Value:=a;
      if      a<b then
              if   b-a<128 then Inc(Value)
              else Dec(Value)
      else if a>b then
              if   a-b>128 then Inc(Value)
              else Dec(Value);
      if Value<RangeLow  then Value:=RangeHigh;
      if Value>RangeHigh then Value:=RangeLow;
      if Value<>a then begin
         Result:=True;
         pa^:=Lo(Value);
         end;
      Inc(pa); Inc(pb);
      end;
end;

function  TPattern.ReversePixelMorph(TargetPattern:TPattern; CountDown:Integer):Boolean;
var i,a,b,Distance,Value:Integer; pa,pb:PByte;
begin
  Result:=False;
  pa:=Addr(PatternBytes  [0,0]);
  pb:=Addr(TargetPattern.PatternBytes[0,0]);
  for i:=0 to SizeOf(PatternBytes)-1 do begin
      a:=pa^; b:=pb^; Value:=a;
      if      a<b then begin
              Distance := b-a;
              if      Distance < 128 then
        	      if Distance    >= CountDown then
          	         Inc(Value)
                      else
              else if 256 - Distance >= CountDown then
          	      Dec(Value);
              end
      else if a>b then begin
              Distance := a-b;
              if      a-b > 128 then
        	      if Distance    >= CountDown then
          	         Inc(Value)
                      else
              else if 256 - Distance >= CountDown then
          	      Dec(Value);
              end;
      if Value<RangeLow  then Value:=RangeHigh;
      if Value>RangeHigh then Value:=RangeLow;
      if Value<>a then begin
         Result:=True;
         pa^:=Lo(Value);
         end;
      Inc(pa); Inc(pb);
      end;
end;

function  TPattern.LoadFromMemory(Size:Integer; Data:Pointer):Boolean;
begin
  Result:=Data<>nil;
  if Result then Move(Data^,PatternBytes,Min(Size,SizeOf(PatternBytes)));
end;

constructor TThread1.Create(CriticalSection__: TCriticalSection; Priority__:TThreadPriority);
begin
  inherited Create(True); // 'True' parameter: create suspended
  CriticalSection:=CriticalSection__;
  FreeOnTerminate:=False;
  Priority:=Priority__;
  fState:=0; fLockCount:=0;
end;

destructor TThread1.Destroy;
begin
  Inherited;
end;

procedure TThread1.Enter;
begin
  CriticalSection.Enter;
  Inc(fLockCount);
end;

procedure TThread1.Leave;
begin
  Dec(fLockCount);
  if fLockCount<0 then
     Application.MessageBox('Internal error: Enter/Leave stack corrupted.',PChar(Application.Title),MB_OK+MB_ICONERROR);
  CriticalSection.Leave;
end;

function  TThread1.GetState:Integer;
begin
  Enter;
  try     Result:=fState;
  finally Leave;
  end;
end;

procedure TThread1.Resume; // caution: 'Resume' and 'Suspend' are NOT stack-oriented as normal; they set 'Suspended' to 'False' or 'True'
begin
  Self.Enter;
  try     Suspended:=False;
  finally Self.Leave;
  end;
end;

procedure TThread1.SetState(State__:Integer);
begin
  Enter;
  try     fState:=State__;
  finally Leave;
  end;
end;

procedure TThread1.Suspend; // caution: 'Resume' and 'Suspend' are NOT stack-oriented as normal; they set 'Suspended' to 'False' or 'True'
begin
  Self.Enter;
  try     Suspended:=True;
  finally Self.Leave;
  end;
end;

procedure   TMandalaThread.Initialize;
begin
  Self.Enter;
  try     TimeOfLastUpdate:=0; TimeForNextUpdate:=0;
  finally Self.Leave;
  end;
end;

constructor TPaletteThread.Create(Mandala__:TMandala; Task__:TPaletteThreadTask);
begin
  Inherited Create(Mandala__,tpNormal);
  Task:=Task__; FixedIntervalMS:=-1;
end;

destructor  TPaletteThread.Destroy;
begin
  Inherited;
end;

procedure   TPaletteThread.Initialize(Task__:TPaletteThreadTask);
var i,j,Size:Integer;
begin
  Inherited Initialize;

  Task:=Task__;
  SpeedChangeCount := 0; TextLabel := nil;

  Mandala.Enter;
  try     if      Task=ptSpin   then begin
                  Speed:=Mandala.PaletteSpinSpeed;
                  TextLabel:=Mandala.PaletteSpinSpeedLabel;
                  end
          else if Task=ptChange then begin
                  Speed:=Mandala.PaletteChangeSpeed;
                  TextLabel:=Mandala.PaletteChangeSpeedLabel;
                  end;
  finally Mandala.Leave;
  end;

  if FixedIntervalMS>=0 then begin // kludge: a late introduced feature
     for i:=Low(Sleep) to High(Sleep) do Sleep[i]:=FixedIntervalMS;
     Speed.Direction:=1;
     end
  else begin
     Size := Max(0,Speed.StepsPerSecondMax-Speed.StepsPerSecondMin);
     j := Succ(High(Sleep)-Low(Sleep));
     for i:=Low(Sleep) to High(Sleep) do
         Sleep[i]:=1000 div (Max(1,Speed.StepsPerSecondMin+(Size*i div j)));
     end;

  SpeedIndex := MAX_SPEED_INDEX div 2; // start in the middle gear

  TimeForNextSpeedChange:=0; TimeForNextStep:=0;
end;

procedure   TPaletteThread.Execute;
var TimeNow:TTimeMS;

  procedure SpeedChange;
  begin
    if        Random(2)=0 then
         if   SpeedIndex<MAX_SPEED_INDEX then Inc(SpeedIndex)
         else SpeedIndex:=0
    else if   SpeedIndex>0 then Dec(SpeedIndex)
         else SpeedIndex:=MAX_SPEED_INDEX;

    if TextLabel<>nil then TextLabel.Caption:=Format('%d (%d ms)',[SpeedIndex,Sleep[SpeedIndex]]);

    Inc (SpeedChangeCount);
    if (SpeedChangeCount > 10) and (Random (4) = 0) and
       (FixedIntervalMS<0) then begin // kludge: 'FixedInterval': a late introduced feature, abused to control direction too
  	Speed.Direction := -Speed.Direction;
        SpeedChangeCount := 0;
        end;

    TimeForNextSpeedChange:=TimeNow +
      RandomInterval(Speed.ChangeIntervalMS,Speed.ChangeDeviationPct);
  end;

begin // Execute
  Initialize(Task);

  while not Terminated do begin
    TimeNow:=GetTickCount;

    if TimeNow<TimeOfLastUpdate then begin // clock wrap around
       TimeForNextUpdate:=TimeNow;
       TimeForNextSpeedChange:=0; TimeForNextStep:=0;
       end;

    if TimeNow>=TimeForNextUpdate then begin
       if      TimeNow>TimeForNextStep then begin // '>', not '>=', so '$ffffffff' can be used as 'never'
               Step;
               TimeForNextStep:=TimeNow+Sleep[SpeedIndex];
               end
       else if TimeNow>TimeForNextSpeedChange then
               SpeedChange;

       if   TimeForNextStep<TimeForNextSpeedChange then
            TimeForNextUpdate:=TimeForNextStep
       else TimeForNextUpdate:=TimeForNextSpeedChange;

       TimeOfLastUpdate:=TimeNow;
       end
    else if (not Terminated) and
            (TimeForNextUpdate-TimeNow>1) then
            SleepEx(TimeForNextUpdate-TimeNow-1,False);
    end;
  State:=Ord(tsTerminated);
end;

procedure TPaletteThread.SetFixedSpeed(IntervalMS:Integer);
var i:Integer;
begin
  IntervalMS:=Max(0,IntervalMS);
  for i:=Low(Sleep) to High(Sleep) do Sleep[i]:=IntervalMS;
  TimeForNextSpeedChange:=0; TimeForNextStep:=0;
end;

constructor TPaletteSpinThread.Create(Mandala__:TMandala);
begin
  Inherited Create(Mandala__,ptSpin);
end;

procedure   TPaletteSpinThread.Step;
begin
  Mandala.Enter;
  try     if (Mandala.Palette<>nil) and (Mandala.Pattern<>nil) then begin
             Mandala.Palette.Spin(Speed.Direction);
             end;
  finally Mandala.Leave;
  end;
end;

constructor TPaletteChangeThread.Create(Mandala__:TMandala);
begin
  Inherited Create(Mandala__,ptChange);

  FillChar(PaletteEffects,SizeOf(PaletteEffects),0);
  try    PaletteEffects[peSnowBurst            ]:=TPaletteEffectSnowBurst.Create;
         PaletteEffects[peRegeneration         ]:=TPaletteEffectRegeneration.Create;
         PaletteEffects[pePastel               ]:=TPaletteEffectPastel.Create;
         PaletteEffects[peMuted                ]:=TPaletteEffectMuted.Create;
         PaletteEffects[peShadows              ]:=TPaletteEffectShadows.Create;
         PaletteEffects[pePrimary              ]:=TPaletteEffectPrimary.Create;
         PaletteEffects[peBlue                 ]:=TPaletteEffectBlue.Create;
         PaletteEffects[peTriColor             ]:=TPaletteEffectTriColor.Create;
         PaletteEffects[peCaterpillar          ]:=TPaletteEffectCaterpillar.Create;
         PaletteEffects[peWinterMountain       ]:=TPaletteEffectWinterMountain.Create;
         PaletteEffects[peRust                 ]:=TPaletteEffectRust.Create;
         PaletteEffects[peStain                ]:=TPaletteEffectStain.Create;
         if   PaletteEffects[pePastel]<>nil then
              PaletteEffect:=PaletteEffects[pePastel]
         else PaletteEffect:=RandomEffect;
  except on E:Exception do Finalize;
  end;
end;

destructor  TPaletteChangeThread.Destroy;
begin
  Finalize;
  Inherited;
end;

procedure  TPaletteChangeThread.Finalize;
var i:TPaletteEffectType;
begin
  for i:=Low(PaletteEffects) to High(PaletteEffects) do begin
      PaletteEffects[i].Free; PaletteEffects[i]:=nil;
      end;
end;

procedure   TPaletteChangeThread.Step;
begin
  if PaletteEffect <> nil then begin

     with Mandala do
       if PaletteSpinThread.Suspended then PaletteEffect.Step
       else begin
          PaletteSpinThread.Suspend;
          try     PaletteEffect.Step;
          finally PaletteSpinThread.Resume;
          end;
          end;

     if PaletteEffect.Done then begin
  	Mandala.Enter;
        try     if   Mandala.PaletteEffectsFadesEnabled or (not PaletteEffect.IsFade) then
                     PaletteEffect := RandomEffect
                else PaletteEffect := RandomEffectNoFades;
        finally Mandala.Leave;
        end;
        if PaletteEffect<>nil then PaletteEffect.Steps:=0;
        end;
    end;
end;

function   TPaletteChangeThread.RandomEffect:TPaletteEffect;
begin
//Result:=PaletteEffects[pePastel]; exit;
  repeat Result:=PaletteEffects[RandomPaletteEffectType];
  until  (Result=nil) or
         (PaletteEffect=nil) or
         ((Result<>PaletteEffect)
          and
          (not (PaletteEffect.IsFade and Result.IsFade))
          and
          ((Result<>PaletteEffects[peRegeneration]) or (Random(4)=0)) // 'Regeneration' is good, but not too often
         );
end;

function   TPaletteChangeThread.RandomEffectNoFades:TPaletteEffect;
begin
  repeat Result:=RandomEffect;
  until  (Result=nil) or (not Result.IsFade);
end;

constructor TPatternThread.Create(Mandala__:TMandala);
begin
  Inherited Create(Mandala__,tpNormal);
  PatternList:=nil; PatternListCount:=0;
  PrevPattern:=nil; CurrentPattern:=nil;
  FadePattern:=nil;
end;

destructor  TPatternThread.Destroy;
begin
  while PatternList<>nil do DequeuePattern;
  Mandala.FreePattern(FadePattern);
  Inherited;
end;

procedure   TPatternThread.Execute;
var t,TimeNow:TTimeMS; APattern:TPattern;

  procedure DoChangePattern;
  begin
    Mandala.FreePattern(PrevPattern);
    PrevPattern:=CurrentPattern;
    CurrentPattern:=DequeuePattern;

    Mandala.Enter;
    try     Mandala.Pattern:=CurrentPattern;
    finally Mandala.Leave;
    end;
    if Mandala.PatternNameLabel<>nil then
       Mandala.PatternNameLabel.Caption:=PatternTypeText[CurrentPattern.PatternType];
  end;

  procedure DoFade;
  var i,Steps,SleepMS:Integer; ColorCycling:Boolean;
      Pct,PctStep:TFloat; CurrentPatternBytes:TPatternBytes;
  begin
    Mandala.Enter;
    try     Steps:=Max(1,Min(1000,Mandala.PatternChangeFadeSteps));
            SleepMS:=Integer(Mandala.PatternChangeFadeStepIntervalMS);
            ColorCycling:=Mandala.PatternChangeFadeColorCycling;
            CurrentPatternBytes:=Mandala.Pattern.PatternBytes;
    finally Mandala.Leave;
    end;

    if FadePattern<>nil then
       try
         if Mandala.PatternNameLabel<>nil then
            Mandala.PatternNameLabel.Caption:='*** Fading...';
         Pct:=0.0; PctStep:=100 / Steps;
         if not ColorCycling then begin
            Mandala.PaletteSpinThread.Suspend;
            Mandala.PalettechangeThread.Suspend;
            end;

         for i:=0 to Steps-2 do // -2: so last alphablending not is 100% of the new pattern
             if not Terminated then begin
                Pct:=Pct+PctStep;
                FadePattern.AlphaBlend(CurrentPatternBytes,PatternList.PatternBytes,Trunc(Pct));
                Mandala.Enter;
                try     Mandala.Pattern.PatternBytes:=FadePattern.PatternBytes;
                finally Mandala.Leave;
                end;
                SleepEx(SleepMS,False);
                end;

         DoChangePattern;
       finally
         if not ColorCycling then begin
            Mandala.PaletteSpinThread.Resume;
            Mandala.PaletteChangeThread.Resume;
            end;
         SleepEx(0,False);
       end
    else DoChangePattern;
  end;

  procedure DoPixelMorph;
  var SleepMS:Integer; ColorCycling:Boolean;
  begin
    Mandala.Enter;
    try     SleepMS:=Integer(Mandala.PatternChangePixelMorphIntervalMS);
            ColorCycling:=Mandala.PatternChangeFadeColorCycling;
    finally Mandala.Leave;
    end;

    if (FadePattern<>nil) and (CurrentPattern<>nil) then
       try
         if Mandala.PatternNameLabel<>nil then
            Mandala.PatternNameLabel.Caption:='*** Pixel Morphing...';

         FadePattern.PatternBytes:=CurrentPattern.PatternBytes;
         if not ColorCycling then begin
            Mandala.PaletteSpinThread.Suspend;
            Mandala.PalettechangeThread.Suspend;
            end;

         while (not Terminated) and
               FadePattern.PixelMorph(PatternList) do begin
               Mandala.Enter;
               try     Mandala.Pattern.PatternBytes:=FadePattern.PatternBytes;
               finally Mandala.Leave;
               end;
               SleepEx(SleepMS,False);
               end;

         DoChangePattern;
       finally
         if not ColorCycling then begin
            Mandala.PaletteSpinThread.Resume;
            Mandala.PaletteChangeThread.Resume;
            end;
         SleepEx(0,False);
       end
    else DoChangePattern;
  end;

  procedure DoReversePixelMorph;
  var CountDown,SleepMS:Integer; ColorCycling:Boolean;
  begin
    Mandala.Enter;
    try     SleepMS:=Integer(Mandala.PatternChangeFadeStepIntervalMS);
            ColorCycling:=Mandala.PatternChangeFadeColorCycling;
    finally Mandala.Leave;
    end;

    if (FadePattern<>nil) and (CurrentPattern<>nil) then
       try
         if Mandala.PatternNameLabel<>nil then
            Mandala.PatternNameLabel.Caption:='*** Reverse Pixel Morphing...';

         FadePattern.PatternBytes:=CurrentPattern.PatternBytes;
         CountDown:=128;
         if not ColorCycling then begin
            Mandala.PaletteSpinThread.Suspend;
            Mandala.PaletteChangeThread.Suspend;
            end;

         while (not Terminated) and
               FadePattern.ReversePixelMorph(PatternList,CountDown) do begin
               Mandala.Enter;
               try     Mandala.Pattern.PatternBytes:=FadePattern.PatternBytes;
               finally Mandala.Leave;
               end;
               SleepEx(SleepMS,False);
               if CountDown>1 then Dec(CountDown);
               end;

         DoChangePattern;
       finally
         if not ColorCycling then begin
            Mandala.PaletteSpinThread.Resume;
            Mandala.PaletteChangeThread.Resume;
            end;
         SleepEx(0,False);
       end
    else DoChangePattern;
  end;

begin // Execute
  Initialize;

  if FadePattern=nil then FadePattern:=Mandala.NewPattern;

  while not Terminated do begin
    TimeNow:=GetTickCount;

    if TimeNow<TimeOfLastUpdate then begin // clock wrap around
       TimeForNextUpdate:=TimeNow;
       {
       t:=$ffffffff-TimeOfLastUpdate;
       if   t<TimerInterval then
            TimeForNextUpdate:=TimerInterval-t
       else TimeForNextUpdate:=TimeNow;
       }
       end;

    if (TimeNow>=TimeForNextUpdate) and
       (PatternList<>nil)  then begin

       if   CurrentPattern=nil then DoChangePattern
       else begin
              //PaletteSpinThread  .Suspend;
              //PaletteChangeThread.Suspend;
              try
                case Random(3) of
                  0: DoFade;
                  1: DoPixelMorph;
                  2: DoReversePixelMorph;
                end;
              finally //PaletteSpinThread  .Resume;
                      //PaletteChangeThread.Resume;
              end;
            end;

       Mandala.Enter;
       try     t:=RandomInterval(Mandala.PatternChangeIntervalMS,
                                 Mandala.PatternChangeDeviationPct);
       finally Mandala.Leave;
       end;

       repeat TimeNow := GetTickCount;
              TimeForNextUpdate:=TimeNow+t;
       until  Terminated or (TimeForNextUpdate>=TimeNow);

       TimeOfLastUpdate:=TimeNow;
       end
    else if not Terminated then begin
            APattern:=Mandala.NewPattern;
            if APattern<>nil then begin // if buffer not full, then make new pattern
               APattern.MakePatternType(RandomNotRecentlyUsedPatternType);
               if (PatternList=nil) and
                  (PrevPattern=nil) and
                  Mandala.ShowTitleOnStartup then APattern.MakeTitle;
               EnqueuePattern(APattern);
               end
            else if (TimeForNextUpdate>TimeNow) and
                    (TimeForNextUpdate-TimeNow>1) then
                    SleepEx(TimeForNextUpdate-TimeNow-1,False); // change this to 'SleepEx(0,False), if you want to enable 'NextPattern'
            end;
    end;
  State:=Ord(tsTerminated);
end;

procedure   TPatternThread.EnqueuePattern(APattern:TPattern);
var p:TPattern;
begin
  APattern.Next:=nil;
  if PatternList=nil then
     PatternList:=APattern
  else begin
     p:=PatternList;
     while p.Next<>nil do p:=p.Next;
     p.Next:=APattern;
     end;
  Inc(PatternListCount);
//  if MainForm.MPlayer.Visible then MainForm.Status.Hint:=IntToStr(PatternListCount);
end;

function    TPatternThread.DequeuePattern:TPattern;
begin
  Result:=PatternList;
  if PatternList<>nil then begin
     PatternList:=PatternList.Next;
     Dec(PatternListCount);
     end;
//  if MainForm.MPlayer.Visible then MainForm.Status.Hint:=IntToStr(PatternListCount);
end;

function    TPatternThread.RandomNotRecentlyUsedPatternType:TPatternType;
var i,CheckLength:Integer; p,List:TPattern;
begin
//Result:=ptCutGlass; exit;
  Result:=RandomPatternType;
  if (PatternList<>nil) and (Low(Result)<>High(Result)) then begin
     CheckLength:=Min(PatternListCount,(1+Ord(High(TPatternType))-Ord(Low(TPatternType))) div 2); // ... div 2: a way to ensure that it's possible to select an unused type
     List := PatternList;
     for i:=Succ(CheckLength) to PatternListCount do List:=List.Next; // skip head of the list

     repeat if Result=High(Result) then Result:=Low(Result)
            else Result:=Succ(Result); // try next/first patterntype
            if (Result=ptFan) and (Random(8)<>0) then
               i:=MaxInt // 'Fan' doesn't vary much; thus, don't use it is as frequently as the other patterns
            else begin
               p:=List; i:=CheckLength;
               while (i<>0) and (p<>nil) and (p.PatternType<>Result) do begin
                     p:=p.Next; Dec(i);
                     end;
               end;
     until  i=0; // until not found
     end;
end;

constructor TPaletteEffect.Create;
begin
  Inherited Create;
  FDone := False; FIsFade := False; Steps := 0 ; FColorCount:=256;
end;

destructor  TPaletteEffect.Destroy;
begin
  Inherited Destroy;
end;

procedure TPaletteEffect.Clear;
begin
  FDone := False; Steps := 0;
  Mandala.GetScreenPaletteColors(FColorCount,CurrentColors);
  if Mandala.PaletteEffectNameLabel<>nil then
     Mandala.PaletteEffectNameLabel.Caption:=PaletteEffectTypeText[EffectType];
end;

procedure TPaletteEffect.Step;
begin
  Mandala.Enter;
  try     if Mandala.Palette<>nil then Mandala.Palette.PaletteColors:=CurrentColors;
  finally Mandala.Leave;
  end;
end;

constructor TPaletteEffectReversion.Create;
begin
  Inherited;
  FIsFade := True;
end;

destructor  TPaletteEffectReversion.Destroy;
begin
  Inherited;
end;

procedure TPaletteEffectReversion.Clear;
begin
  Inherited;
  OriginalColors:=CurrentColors;
end;

procedure TPaletteEffectReversion.Step;
begin
  Inherited;
end;

function TPaletteEffectReversion.NewColorByAverage (Index: Integer): TRGB;
var Below, Above, Average: TRGB;
begin
  if (Index >= Low (CurrentColors)) and
     (Index <= High(CurrentColors)) then begin
     if   Index = High (CurrentColors) then
          Above := CurrentColors [Low(CurrentColors)]
     else Above := CurrentColors[Succ(Index)];

     if   Index = Low (CurrentColors) then
  	  Below := CurrentColors [High(CurrentColors)]
     else Below := CurrentColors[Pred(Index)];

     Average.r := ByteAverage (CurrentColors[Index].r, Below.r, Above.r);
     Average.g := ByteAverage (CurrentColors[Index].g, Below.g, Above.g);
     Average.b := ByteAverage (CurrentColors[Index].b, Below.b, Above.b);
     // Result is slightly averaged out
     Result := CurrentColors[Index];
     RGBMoveTowards (1, Result, Average);
     end
  else Result:=CurrentColors[Low(CurrentColors)];
end;

function TPaletteEffectReversion.NewColorByRevert (Index: Integer): TRGB;
begin
  if (Index < Low (CurrentColors)) or
     (Index > High(CurrentColors)) then Index:=Low(CurrentColors);
  // Result is slightly averaged out
  Result := CurrentColors[Index];
  RGBMoveTowards (1, Result, OriginalColors[Index]);
end;

constructor TPaletteEffectSnowBurst.Create;
begin
  Inherited;
  FEffectType := peSnowBurst;
  FIsFade := True;
end;

destructor TPaletteEffectSnowBurst.Destroy;
begin
  Inherited;
end;

procedure TPaletteEffectSnowBurst.Clear;
begin
  Inherited;
end;

procedure TPaletteEffectSnowBurst.Step;
var Loop: Integer;
begin
  FDone := Steps > 400;

  if Steps=0 then Clear;

  for Loop := 0 to Pred(ColorCount) do begin
      // start of letting it show, then smoothing, then go into fading back
      case Steps of
        0..100:   begin
                    case Steps mod 6 of
                      0,1,2: CurrentColors[Loop] := NewColorByAverage (Loop);
                      3    : CurrentColors[Loop] := NewColorByRevert  (Loop);
                    end; // case
                  end;
        101..200: begin
                    case Steps mod 4 of
                      0    : CurrentColors[Loop] := NewColorByAverage (Loop);
                      1    : CurrentColors[Loop] := NewColorByRevert  (Loop);
                    end; // case
                  end;
        201..400: begin
                    case Steps mod 4 of
                      0,1  : CurrentColors[Loop] := NewColorByRevert  (Loop);
                      2,3  : CurrentColors[Loop] := NewColorByAverage (Loop);
                    end; // case
                  end;
        else      begin      CurrentColors[Loop] := NewColorByRevert  (Loop);
                             if not RGBIsEqual (CurrentColors[Loop], OriginalColors[Loop]) then
                                FDone := False;
                  end;
      end; // case
      end;

  Inc(Steps);
  Inherited;
end;

constructor TPaletteEffectRegeneration.Create;
begin
  Inherited;
  FEffectType := peRegeneration;
  FIsFade := True;
end;

destructor TPaletteEffectRegeneration.Destroy;
begin
  Inherited;
end;

procedure TPaletteEffectRegeneration.Clear;
begin
  Inherited;
  MakeRandomPalette(ColorCount,DestinationColors);
end;

procedure TPaletteEffectRegeneration.Step;
const DEGENERATION_STEPS = 200;
var Loop: Integer;
begin
  FDone := True;

  if Steps=0 then Clear;

  if Steps < DEGENERATION_STEPS then begin // degenerate
     // move towards the random palette
     for Loop := 0 to Pred(ColorCount) do begin
      	  RGBMoveTowards (1, CurrentColors[Loop], DestinationColors[Loop]);
          if not RGBIsEqual (CurrentColors[Loop], DestinationColors[Loop]) then
             FDone := False;
          end;

     if Done then begin // oops! got there ahead of schedule. Go back now
        Steps := DEGENERATION_STEPS;
        FDone := False; // don't end yet
        end;
     end
  else begin // regenerate
     // move towards the original palette
     for Loop := 0 to Pred(ColorCount) do begin
         RGBMoveTowards (1, CurrentColors[Loop], OriginalColors[Loop]);
         if not RGBIsEqual (CurrentColors[Loop], OriginalColors[Loop]) then
            FDone := False;
         end;
     end;

  Inc(Steps);
  Inherited;
end;

constructor TPaletteEffectFourColors.Create;
begin
  Inherited;
  MaxLength := 100; // default max length
end;

destructor TPaletteEffectFourColors.Destroy;
begin
  Inherited;
end;

procedure TPaletteEffectFourColors.Clear;
var Loop: Integer;
begin
  Inherited;

  for Loop := 0 to 3 do begin
      // decide where to put colors
      FourColorPosition[Loop] := ColorCount div 4 * Loop;
      // make new target colors
      FourColorTarget[Loop] := NewColor (Loop);
      // find out what the current colors are
      FourColor[Loop] := CurrentColors[FourColorPosition[Loop]];
      FourColorLength[Loop] := 0;
      end;

  Mandala.Enter;
  try     if   Mandala.PaletteEffectsEnabled then begin
               LifeTime:=RandomInterval(Mandala.PaletteEffectsIntervalMS,Mandala.PaletteEffectsDeviationPct);
               end
          else LifeTime := High(LifeTime);
  finally Mandala.Leave;
  end;

  StartTime := 0;
end;

procedure TPaletteEffectFourColors.Step;
var Loop: Integer;
begin
  if Steps = 0 then Clear;
  Inc(Steps);

  // migrate current colors towards target colors and
  // make new targets if they have got there
  for Loop := 0 to 3 do begin
      if not RGBMoveTowards (1, FourColor[Loop], FourColorTarget[Loop]) then begin
    	 // count up towards end of palette stasis
    	 Inc (FourColorLength[Loop]);
         // if it's got there
         if FourColorLength[Loop] > MaxLength then begin
            FourColorTarget[Loop] := NewColor (Loop); // new color
            // take a random bit off the next stasis length
            FourColorLength[Loop] := Random (MaxLength div 10);
            end;
         end;

      RGBSetRange(FourColorPosition[Loop], FourColorPosition[Succ(Loop) mod 4],
                  0,Pred(ColorCount),
                  FourColor[Loop], FourColor[Succ(Loop) mod 4],
                  CurrentColors);
      end;

  if StartTime = 0 then StartTime := GetTickCount;  // first time ?

  if   (LifeTime <> High(LifeTime)) and
       (GetTickCount > StartTime + LifeTime) then
       FDone:=True // time expired ?
  else Inherited; // no: update screen palette
end;

function  TPaletteEffectFourColors.NewColor (Pos: Integer): TRGB;
begin
  Result.r := ColorSelectionValue (NewColorType (Pos, 0));
  Result.g := ColorSelectionValue (NewColorType (Pos, 1));
  Result.b := ColorSelectionValue (NewColorType (Pos, 2));
end;

function  TPaletteEffectFourColors.NewColorType(Pos, Col: Integer): TColorSelection;
begin
  raise Exception.Create ('TPaletteEffectFourColors.NewColorType: Override before use!');
end;

constructor TPaletteEffectPastel.Create;
begin
  Inherited;
  FEffectType := pePastel;
  MaxLength := 150; // pastel is mellow
end;

function  TPaletteEffectPastel.NewColorType(Pos, Col: Integer): TColorSelection;
begin
  case Random(5) of
    0..1: 	Result := csHigh;
    2..3: 	Result := csMidHigh;
    else 	Result := csMid;
  end; // case random
end;

function  TPaletteEffectPastel.SecondaryColorType(Pos, Col: Integer): TColorSelection;
begin
  case Random(8) of
    0..2: Result := csLow;
    3,4 : Result := csMidLow;
    5   : Result := csMid;
    6   : Result := csMidHigh;
    else  Result := csRandom;
  end; // case
end;

function  TPaletteEffectPastel.NewColor (Pos: Integer): TRGB;
var Shade1: Integer;
begin
  // there is a primary shade and 2 secondary shades
  Shade1   := ColorSelectionValue (NewColorType (Pos, 0));
  Result.r := ColorSelectionValue (SecondaryColorType (Pos, 0));
  Result.g := ColorSelectionValue (SecondaryColorType (Pos, 0));
  Result.b := ColorSelectionValue (SecondaryColorType (Pos, 0));

  case Random (3) of
    0   : Result.r   := Shade1;
    1   : Result.g   := Shade1;
    else  Result.b   := Shade1;
  end; // case
end;

constructor TPaletteEffectMuted.Create;
begin
  Inherited;
  FEffectType := peMuted;
  MaxLength := 0; // flux means never staying still
end;

function  TPaletteEffectMuted.NewColorType(Pos, Col: Integer): TColorSelection;
begin
  // quite a dark palette - soft tones  }
  case Random(13) of
    0..6: Result := csLow;
    7,8 : Result := csMidHigh;
    9,10: Result := csHigh;
    11  : Result := csMid;
    else  Result := csRandom;
  end; // case
end;

constructor TPaletteEffectShadows.Create;
begin
  Inherited;
  FEffectType := peShadows;
  MaxLength := 200; // shadows are slow
end;

procedure TPaletteEffectShadows.Clear;
begin
  Inherited;

  ShadowPos := Random (4); // there's one slot that is always dark

  LightPos := Succ(ShadowPos) mod 4; // one slot is never dark - the light is next on from the shadow }
end;

function TPaletteEffectShadows.NewColorType(Pos, Col: Integer): TColorSelection;
begin
  // almost a blackout
  case Random(23) of
    0..18: Result := csLow;
    19   : Result := csMidLow;
    20   : Result := csMid;
    21   : Result := csHigh;
    else   Result := csRandom;
  end; // case
end;

function TPaletteEffectShadows.NewColor (Pos: Integer): TRGB;
begin
  if Pos = ShadowPos then
     Result:=RGB_BLACK
  else
     repeat
       Result.r := ColorSelectionValue (NewColorType (Pos, 0));
       Result.g := ColorSelectionValue (NewColorType (Pos, 1));
       Result.b := ColorSelectionValue (NewColorType (Pos, 2));
     until not RGBIsAlike(Result,RGB_BLACK,10); // until not almost black
end;

constructor TPaletteEffectPrimary.Create;
begin
  Inherited;
  FEffectType := pePrimary;
  MaxLength := 30; // changes quite often - energy
end;

function  TPaletteEffectPrimary.NewColorType(Pos, Col: Integer): TColorSelection;
begin
  // primary Color is usually bright
  if Random(10) <= 7 then Result := csHigh
  else Result := csLow;
end;

function  TPaletteEffectPrimary.SecondaryColorType(Pos, Col: Integer): TColorSelection;
begin
  // secondary color is muted or not at all
  case Random (8) of
    0:   Result := csMid;
    1:   Result := csMidLow;
    2:   Result := csRandom;
    else Result := csLow;
  end; // case
end;

function  TPaletteEffectPrimary.NewColor (Pos: Integer): TRGB;
var i,j: Integer;
begin
  // there is a primary shade and a secondary shade.
  i:=Random(3); j:=(Succ(i)+Random(2)) mod 3;
  Result.c[i]:=ColorSelectionValue (NewColorType       (Pos, 0));
  Result.c[j]:=ColorSelectionValue (SecondaryColorType (Pos, 0));
  Result.c[3-i-j]:=0;
end;

constructor TPaletteEffectBlue.Create;
begin
  Inherited;
  FEffectType := peBlue;
end;

function  TPaletteEffectBlue.NewColorType (Pos, Col: Integer): TColorSelection;
begin
  case Random(10) of
    0..4: Result := csHigh;
    5   : Result := csMidHigh;
    6   : Result := csMid;
    7,8 : Result := csLow;
    else  Result := csRandom;
  end; // case
end;

function TPaletteEffectBlue.SecondaryColorType (Pos, Col: Integer): TColorSelection;
begin
  // quite a dark palette - soft tones
  case Random(10) of
    0    : Result := csHigh;
    1    : Result := csMidHigh;
    2,3  : Result := csMid;
    4,5  : Result := csMidLow;
    6,7,8: Result := csLow;
    else   Result := csRandom;
  end; // case
end;

function  TPaletteEffectBlue.NewColor (Pos: Integer): TRGB;
begin
  // there is a primary shade and 2 secondary shades
  Result.b := ColorSelectionValue (NewColorType (Pos, 0));

  // if there is no blue, no nothing else either
  if Result.b > 0 then begin
     Result.r := ColorSelectionValue (SecondaryColorType (Pos, 0));
     Result.g := ColorSelectionValue (SecondaryColorType (Pos, 0));
     end
  else begin
     Result.r := 0;
     Result.g := 0;
     end;
end;

constructor TPaletteEffectSequential.Create;
begin
  Inherited;
  ListCount:=Low(List);
  ListIndex:=-1;
end;

destructor TPaletteEffectSequential.Destroy;
begin
  Inherited;
end;

procedure TPaletteEffectSequential.AddColor(Color: TRGB);
begin
  if ListCount<Succ(High(List)) then begin
     List[ListCount]:=Color;
     Inc(ListCount);
     end;
end;

function TPaletteEffectSequential.NewColor(Pos: Integer): TRGB;
begin
  if ListCount<>0 then begin
     Inc(ListIndex);
     if ListIndex>=ListCount then ListIndex:=Low(List);
     Result:=List[ListIndex];
     end
  else Result:=RGB_BLACK
end;

constructor TPaletteEffectTriColor.Create;
const
  C_RANDOM        : TRGB = (b: 69; g: 13; r: 42);
  C_RANDOM_PRIMARY: TRGB = (b: 1 ; g: 42; r: 1);
begin
  Inherited;
  FEffectType := peTriColor;
  MaxLength := 250;  // slow

  AddColor (RGB (255, 0, 0));  // red
  AddColor (RGB_WHITE);        // white
  AddColor (RGB (0, 0, 255));  // blue
  AddColor (RGB_BLACK);        // black
  AddColor (C_RANDOM_PRIMARY); // random

  // and again in a  different order

  AddColor (RGB (0, 0, 255));  // blue
  AddColor (RGB (255, 0, 0));  // red
  AddColor (RGB_WHITE);        // white
  AddColor (C_RANDOM_PRIMARY); // random
  AddColor (RGB_BLACK);        // black
end;

constructor TPaletteEffectCaterpillar.Create;
begin
  Inherited;
  FEffectType := peCaterpillar;
  MaxLength := 250;  // slow

  AddColor (RGB (255, 128, 128)); // pink
  AddColor (RGB_BLACK);           // black
  AddColor (RGB (255, 255, 0));   // yellow
  AddColor (RGB (255, 0, 255));   // purple
  AddColor (RGB_BLACK);           // black
  AddColor (RGB (0, 255, 0));     // green
  AddColor (RGBRandomPrimary);    // random
  AddColor (RGB_WHITE);           // white
end;

constructor TPaletteEffectWinterMountain.Create;
begin
  Inherited;
  FEffectType := peWinterMountain;
  MaxLength := 250;  // slow

  // inspired by party @ Cape Hanglip, morning of Wed 16 June 1999
  AddColor (RGB (40, 0, 128));    // blue
  AddColor (RGB (100, 100, 100)); // grey
  AddColor (RGB (130, 120, 0));   // brown
  AddColor (RGB (0, 128, 0));     // green
  AddColor (RGB_BLACK);           // black
  AddColor (RGBRandomPrimary);    // random
  AddColor (RGB_WHITE);           // white
  AddColor (RGB (0, 64, 0));      // dark green
  AddColor (RGB (128, 128, 128)); // grey
  AddColor (RGB (64, 32, 0));     // other brown
end;

constructor TPaletteEffectRust.Create;
begin
  Inherited;
  FEffectType:= peRust;
  MaxLength := 600; // very slow

  // Heavy metal for depression. Metallica and ministry
  AddColor (RGB_BLACK);           // black
  AddColor (RGB_BLACK);           // black
  AddColor (RGB (120, 45, 20));   // rust red
  AddColor (RGB (120, 20, 45));   // rust red
  AddColor (RGBRandom);           // random
  AddColor (RGB (255, 30, 0));    // red
  AddColor (RGB (20, 0, 0));      // black, almost
  AddColor (RGB (0, 0, 20));      // black, almost
  AddColor (RGB (100, 100, 120)); // battleship grey
  AddColor (RGB (255, 237, 237)); // coma white
  AddColor (RGB (237, 255, 237)); // coma white
  AddColor (RGB (128, 50, 50));   // puss-red
  AddColor (RGB (0, 64, 0));      // dark green
  AddColor (RGB (0, 64, 0));      // dark green
  AddColor (RGBRandom);           // random
end;

constructor TPaletteEffectStain.Create;
begin
  Inherited Create;
  FEffectType := peStain;
  FIsFade := True;
end;

destructor TPaletteEffectStain.Destroy;
begin
  Inherited;
end;

procedure TPaletteEffectStain.Clear;
begin
  Inherited;
  OriginalColors := CurrentColors;
  TargetColors   := CurrentColors;

  Mandala.Enter;
  try     if   Mandala.PaletteEffectsEnabled then begin
               LifeTime:=RandomInterval(Mandala.PaletteEffectsIntervalMS,Mandala.PaletteEffectsDeviationPct);
               end
          else LifeTime := High(LifeTime);
  finally Mandala.Leave;
  end;

  StartTime := 0;

  Ending := False;
end;

procedure TPaletteEffectStain.Step;
var Loop: Integer; Dif: Boolean;
begin
  if Steps = 0 then Clear;

  if ((Steps mod 10) = 0) and (not Ending) then
     DoStain; // do a stain at regular intervals

  Inc(Steps); Dif := False;

  for Loop := 0 to Pred(ColorCount) do begin
      if   not Ending then
           if   RGBMoveTowards (1, CurrentColors[Loop], TargetColors[Loop]) then
                Dif := True
           else
      else if   RGBMoveTowards (1, CurrentColors[Loop], OriginalColors[Loop]) then
                Dif := True;
      end;

  if StartTime = 0 then StartTime := GetTickCount;  // first time ?

  if   (LifeTime <> High(LifeTime)) and
       (GetTickCount > StartTime + LifeTime) then
       Ending:=True; // time expired ?

  FDone := Ending and (not Dif);

  if not Done then Inherited; // Done? if no, update screen palette
end;

procedure TPaletteEffectStain.StainPalette (Start, FullWidth, Depth, Color: Integer);
var HalfWidth, Midpoint, Disp, Change, Loop, ActualEntry: Integer;
begin
  if (Depth <> 0) and
     (FullWidth > 0) and
     (Color >= 0) and (Color <= 2) then begin
     MidPoint := Start + (FullWidth div 2);
     HalfWidth := FullWidth div 2;

     for Loop := Start to (Start + FullWidth) do begin
         Disp := Abs (Loop - Midpoint); // how far are we from the midpoint?
         Disp := HalfWidth - Disp;      // how far from an end
         if   Disp = 0 then             // how much to change at this point?
    	      Change := 0
         else Change := Round (Depth * Disp / HalfWidth);

         ActualEntry := Loop; // where are we actually in the palette?
         if ActualEntry < Low(CurrentColors) then
    	    ActualEntry := ActualEntry + ColorCount;
         if ActualEntry >= ColorCount then
    	    ActualEntry := ActualEntry - ColorCount;

         case Color of
    	   0: TargetColors[ActualEntry].r :=
      		ByteChop (TargetColors[ActualEntry].r + Change);
           1: TargetColors[ActualEntry].g :=
      		ByteChop (TargetColors[ActualEntry].g + Change);
           2: TargetColors[ActualEntry].b :=
      		ByteChop (TargetColors[ActualEntry].b + Change);
         end; // case
         end;
     end;
end;

procedure TPaletteEffectStain.DoStain;
begin
  StainPalette (Random (ColorCount),
                Random (180) + 30,
                Random (500) - 300, // bias towards removing color
                Random (3));
end;


constructor TMandala.Create(BitMap__:TBitMap;
                            PaletteChangeSpeedLabel__,
                            PaletteEffectNameLabel__,
                            PaletteSpinSpeedLabel__,
                            PatternNameLabel__:TLabel);
var i:Integer;
begin
  Inherited Create;

  BitMap                 :=BitMap__; // output
  PaletteChangeSpeedLabel:=PaletteChangeSpeedLabel__;
  PaletteEffectNameLabel :=PaletteEffectNameLabel__;
  PaletteSpinSpeedLabel  :=PaletteSpinSpeedLabel__;
  PatternNameLabel       :=PatternNameLabel__;

  FPaletteChangeEnabled  :=True;
  FPatternChangeEnabled  :=True;
  PaletteSpinThread:=nil; PaletteChangeThread:=nil; PatternThread:=nil;
  FillChar(Palettes,SizeOf(Palettes),0); NextFreePalette:=nil;
  FillChar(Patterns,SizeOf(Patterns),0); NextFreePattern:=nil;
  SavedPalette:=nil; SavedPattern:=nil; WorkBitMap:=nil; WorkBitMapPitch:=0;

  try    WorkBitMap                                 :=TBitMap.Create;
         WorkBitMap.Width                           :=BITMAP_WIDTH;
         WorkBitMap.Height                          :=BITMAP_HEIGHT;
         WorkBitMap.PixelFormat                     :=pf24Bit;

         with WorkBitMap do
           if Height>1 then
              WorkBitMapPitch:=Integer(ScanLine[1])-Integer(ScanLine[0]);

         for i:=High(Palettes) downto Low(Palettes) do begin
             Palettes[i]:=TPalette.Create;
             Palettes[i].Next:=NextFreePalette; NextFreePalette:=Palettes[i];
             end;

         for i:=High(Patterns) downto Low(Patterns) do begin
             Patterns[i]:=TPattern.Create;
             Patterns[i].Next:=NextFreePattern; NextFreePattern:=Patterns[i];
             end;

         PaletteGreyScale                           :=NewPalette;
         if   PaletteGreyScale<>nil then
              PaletteGreyScale.MakeGreyScalePalette
         else raise Exception.Create('TMandala: Palette table full.');

         PaletteSpinSpeed.ChangeIntervalMS          :=5000;
         PaletteSpinSpeed.ChangeDeviationPct        :=20;
         PaletteSpinSpeed.StepsPerSecondMin         :=20;  // steps per second
         PaletteSpinSpeed.StepsPerSecondMax         :=50;  // steps per second
         PaletteSpinSpeed.Direction                 :=1;

         PaletteChangeSpeed.ChangeIntervalMS        :=5000;
         PaletteChangeSpeed.ChangeDeviationPct      :=20;
         PaletteChangeSpeed.StepsPerSecondMin       :=10;  // steps per second
         PaletteChangeSpeed.StepsPerSecondMax       :=50;  // steps per second
         PaletteChangeSpeed.Direction               :=1;

         PaletteEffectsEnabled                      :=True;
         PaletteEffectsFadesEnabled                 :=True;
         PaletteEffectsIntervalMS                   :=25000; // milliseconds
         PaletteEffectsDeviationPct                 :=20;    // pct

         PatternChangeIntervalMS                    :=10000; // milliseconds
         PatternChangeDeviationPct                  :=20;    // pct
         PatternChangeFadeSteps                     :=100;
         PatternChangeFadeStepIntervalMS            :=50;
         PatternChangeFadeColorCycling              :=True;
         PatternChangePixelMorphIntervalMS          :=50;

         ShowTitleOnStartup                         :=False;
         Title                                      :=DEFAULT_TITLE_TEXT;
         SubTitle                                   :=DEFAULT_SUBTITLE_TEXT;

         Palette:=NewPalette;
         // if Palette<>nil then Palette.Assign(PaletteGreyScale);
         Randomize;
         if Palette<>nil then Palette.MakeRandomPalette(256);

         //Pattern:=NewPattern;
         //if Pattern<>nil then Pattern.MakeRandomPattern;

         PaletteSpinThread                          :=TPaletteSpinThread  .Create(Self);
         PaletteChangeThread                        :=TPaletteChangeThread.Create(Self);
         PatternThread                              :=TPatternThread      .Create(Self);

  except on E:Exception do begin
            Finalize;
            Application.MessageBox(PChar(E.Message),'',MB_OK);
            raise; // re-raise the exception to inform the caller, that 'Create' failed
            end;
  end;
end;

destructor  TMandala.Destroy;
begin
  Finalize;
  Inherited;
end;

procedure   TMandala.Initialize(BitMap__:TBitMap);
begin
  BitMap:=BitMap__;
  PaletteSpinThread  .Initialize(PaletteSpinThread  .Task); // calculates speed
  PaletteChangeThread.Initialize(PaletteChangeThread.Task); // calculates speed
end;

procedure   TMandala.Finalize;

var i:Integer; StartTime,TimeOut:TTimeMS; oCursor:TCursor;

  procedure TerminateThread(Thread:TMandalaThread);
  begin
    if Thread<>nil then begin
       Thread.Suspend;
       Thread.State:=Ord(tsTerminate);
       Thread.Resume;    // activate the thread, so it can terminate normally
       Thread.Terminate; // unless the thread was created with 'FreeOnTerminate'=True
       end;
  end;

  procedure FreeThread(Thread:TMandalaThread);
  begin
    if Thread<>nil then begin
       Self.Enter;
       try     if not Thread.State=Ord(tsTerminated) then
                  Thread.Suspend; // otherwise 'TThread.Destroy' enters an infinite 'WaitFor' loop
       finally Self.Leave;
       end;
       end;
    Thread.Free; // free here, unless the thread was created with 'FreeOnTerminate'=True
  end;

begin // TMandala.Finalize
  TerminateThread(PaletteSpinThread);
  TerminateThread(PaletteChangeThread);
  TerminateThread(PatternThread);

  repeat StartTime:=GetTickCount;
         TimeOut  :=StartTime+WAIT_FOR_THREAD_TO_TERMINATE_MS;
  until  TimeOut>=StartTime; // simple clock wrap-around control

  oCursor:=Screen.Cursor;
  try     //Screen.Cursor:=crHourGlass;
          repeat SleepEx(0,False);
          until  (((PaletteSpinThread  =nil) or (PaletteSpinThread  .State=Ord(tsTerminated)))
                  and
                  ((PaletteChangeThread=nil) or (PaletteChangeThread.State=Ord(tsTerminated)))
                  and
                  ((PatternThread      =nil) or (PatternThread      .State=Ord(tsTerminated)))
                 )
                 or
                 (GetTickCount>=TimeOut)
                 or
                 (GetTickCount<StartTime);
  finally Screen.Cursor:=oCursor;
  end;

  FreeThread(PaletteSpinThread);   PaletteSpinThread  :=nil;
  FreeThread(PaletteChangeThread); PaletteChangeThread:=nil;
  FreeThread(PatternThread);       PatternThread      :=nil;

  WorkBitMap.Free;                 WorkBitMap         :=nil; WorkBitMapPitch:=0;
  for i:=Low(Palettes) to High(Palettes) do
      begin Palettes[i].Free;      Palettes[i]        :=nil; end;
  for i:=Low(Patterns) to High(Patterns) do
      begin Patterns[i].Free;      Patterns[i]        :=nil; end;
  NextFreePalette:=nil; NextFreePattern:=nil;
end;

procedure TMandala.GetScreenPaletteColors(var ColorCount:Integer; var PaletteColors:TPaletteColors);
begin
  Self.Enter;
  try     if Palette<>nil then begin
             ColorCount   :=Palette.ColorCount;
             PaletteColors:=Palette.PaletteColors;
             end
          else begin
             ColorCount:=256;
             MakeRandomPalette(ColorCount,PaletteColors);
             end;
  finally Self.Leave;
  end;
end;

function  TMandala.NewPalette:TPalette;
begin
  Self.Enter;
  try     Result:=NextFreePalette;
          if Result<>nil then NextFreePalette:=Result.Next;
  finally Self.Leave;
  end;
end;

procedure TMandala.FreePalette(Palette:TPalette);
begin
  if Palette<>nil then begin
     Self.Enter;
     try  if Palette<>PaletteGreyScale then begin // a fixed palette
             Palette.Next:=NextFreePalette;
             NextFreePalette:=Palette;
             end;
     finally Self.Leave;
     end;
     end;
end;

function  TMandala.NewPattern:TPattern;
begin
  Self.Enter;
  try     Result:=NextFreePattern;
          if Result<>nil then NextFreePattern:=Result.Next;
  finally Self.Leave;
  end;
end;

procedure TMandala.FreePattern(Pattern:TPattern);
begin
  if Pattern<>nil then begin
     Self.Enter;
     try     Pattern.Next:=NextFreePattern;
             NextFreePattern:=Pattern;
     finally Self.Leave;
     end;
     end;
end;
{
procedure TMandala.Paint; // basic, not optimized, 'Paint' algorithm
var X,Y,Phase:Integer; p:PRGB;
    PaletteColors:^TPaletteColors;
    PatternBytes :^TPatternBytes;
begin
  Self.Enter;
  try
    if (Palette<>nil) and (Pattern<>nil) then begin
       Phase        :=Palette.Phase;
       PaletteColors:=Addr(Palette.PaletteColors[0]);
       PatternBytes :=Addr(Pattern.PatternBytes [0,0]);
       for Y:=0 to BITMAP_HEIGHT-1 do begin
           p:=WorkBitMap.ScanLine[Y];
           for X:=0 to BITMAP_WIDTH-1 do begin
               p^:=PaletteColors^[Pattern.Wrap(Phase+PatternBytes^[Y,X])];
               Inc(p);
               end;
           end;
       end;
  finally
    Self.Leave;
  end;
  BitMap.Canvas.CopyRect(BITMAP_RECT,WorkBitMap.Canvas,BITMAP_RECT);
  //BitMap.Assign(WorkBitMap); // a little faster than 'CopyRect'?
end;
}
procedure TMandala.Paint; // optimized 'Paint' algorithm
var X,Y,Phase:Integer; p,pRow:PRGB;
    PaletteColors:^TPaletteColors;
    PatternBytes :PByte;
begin
  Self.Enter;
  try
    if (Palette<>nil) and (Pattern<>nil) then begin
       Phase        :=Palette.Phase;
       PaletteColors:=Addr(Palette.PaletteColors[0]);
       PatternBytes :=Addr(Pattern.PatternBytes [0,0]);
       pRow         :=WorkBitMap.ScanLine[0];
       for Y:=0 to BITMAP_HEIGHT-1 do begin
           p:=pRow;
           for X:=0 to BITMAP_WIDTH-1 do begin
               // optimizing step 1: avoid 'PatternBytes' array reference
               // optimizing step 2: (quick and dirty:) inline 'Pattern.Wrap' for fixed palette-size=256 (0..255)
               p^:=PaletteColors^[(Phase+PatternBytes^) and 255];
               Inc(p); Inc(PatternBytes);
               end;
           // optimizing step 3: avoid calling 'GetScanLine' for each row
           Inc(Integer(pRow),WorkBitMapPitch);
           end;
       end;
  finally
    Self.Leave;
  end;
  BitMap.Canvas.CopyRect(BITMAP_RECT,WorkBitMap.Canvas,BITMAP_RECT);
end;

procedure TMandala.Resume;
begin
  PaletteSpinThread.Resume;
  if PaletteChangeEnabled then PaletteChangeThread.Resume;
  if PatternChangeEnabled then PatternThread.Resume;
end;

procedure TMandala.Suspend;
begin
  PaletteSpinThread.Suspend;
  PaletteChangeThread.Suspend;
  PatternThread.Suspend;
end;

function  TMandala.NextPattern:Boolean;
begin  // note: this has no effect, because the patternthread sleeps until next scheduled pattern-change
  if PatternThread<>nil then begin
     PatternThread.Initialize; Result:=True;
     end
  else Result:=False;
end;

procedure TMandala.SetPaletteChangeEnabled(PaletteChangeEnabled__:Boolean);
begin
  Self.Enter;
  try     FPaletteChangeEnabled:=PaletteChangeEnabled__;
  finally Self.Leave;
  end;
end;

procedure TMandala.SetPatternChangeEnabled(PatternChangeEnabled__:Boolean);
begin
  Self.Enter;
  try     FPatternChangeEnabled:=PatternChangeEnabled__;
  finally Self.Leave;
  end;
end;

function  TMandala.LoadSettingsFromIniFile(const IniFile: TIniFile;
      const Section: String): Boolean;
var s:String;
begin
  Result:=True; s:=Section+' - '+MANDALA_ENGINE_SETTINGS_SUFFIX;
  PaletteSpinSpeed.StepsPerSecondMin:=Max(0,Min(100,IniFile.ReadInteger(s,'PaletteSpinSpeed.StepsPerSecondMin',PaletteSpinSpeed.StepsPerSecondMin)));
  PaletteSpinSpeed.StepsPerSecondMax:=Max(PaletteSpinSpeed.StepsPerSecondMin,Min(100,IniFile.ReadInteger(s,'PaletteSpinSpeed.StepsPerSecondMax',PaletteSpinSpeed.StepsPerSecondMax)));
  PaletteSpinSpeed.ChangeIntervalMS:=Max(0,Min(600000,IniFile.ReadInteger(s,'PaletteSpinSpeed.ChangeIntervalMS',PaletteSpinSpeed.ChangeIntervalMS)));
  PaletteSpinSpeed.ChangeDeviationPct:=Max(0,Min(100,IniFile.ReadInteger(s,'PaletteSpinSpeed.ChangeDeviationPct',PaletteSpinSpeed.ChangeDeviationPct)));
  PaletteChangeSpeed.StepsPerSecondMin:=Max(0,Min(100,IniFile.ReadInteger(s,'PaletteChangeSpeed.StepsPerSecondMin',PaletteChangeSpeed.StepsPerSecondMin)));
  PaletteChangeSpeed.StepsPerSecondMax:=Max(PaletteChangeSpeed.StepsPerSecondMin,Min(100,IniFile.ReadInteger(s,'PaletteChangeSpeed.StepsPerSecondMax',PaletteChangeSpeed.StepsPerSecondMax)));
  PaletteChangeSpeed.ChangeIntervalMS:=Max(0,Min(600000,IniFile.ReadInteger(s,'PaletteChangeSpeed.ChangeIntervalMS',PaletteChangeSpeed.ChangeIntervalMS)));
  PaletteChangeSpeed.ChangeDeviationPct:=Max(0,Min(100,IniFile.ReadInteger(s,'PaletteChangeSpeed.ChangeDeviationPct',PaletteChangeSpeed.ChangeDeviationPct)));
  PaletteEffectsIntervalMS:=Max(0,Min(600000,IniFile.ReadInteger(s,'PaletteEffectsIntervalMS',PaletteEffectsIntervalMS)));
  PaletteEffectsDeviationPct:=Max(0,Min(100,IniFile.ReadInteger(s,'PaletteEffectsDeviationPct',PaletteEffectsDeviationPct)));
  PatternChangeIntervalMS:=Max(0,Min(600000,IniFile.ReadInteger(s,'PatternChangeIntervalMS',PatternChangeIntervalMS)));
  PatternChangeDeviationPct:=Max(0,Min(100,IniFile.ReadInteger(s,'PatternChangeDeviationPct',PatternChangeDeviationPct)));
  PatternChangeFadeSteps:=Max(0,Min(100,IniFile.ReadInteger(s,'PatternChangeFadeSteps',PatternChangeFadeSteps)));
  PatternChangeFadeStepIntervalMS:=Max(0,Min(100,IniFile.ReadInteger(s,'PatternChangeFadeStepIntervalMS',PatternChangeFadeStepIntervalMS)));
  PatternChangePixelMorphIntervalMS:=Max(0,Min(100,IniFile.ReadInteger(s,'PatternChangePixelMorphIntervalMS',PatternChangePixelMorphIntervalMS)));
  ShowTitleOnStartup:=IniFile.ReadBool(s,'ShowTitleOnStartup',ShowTitleOnStartup);
  Title:=IniFile.ReadString (s,'Title',Title);
  SubTitle:=IniFile.ReadString (s,'SubTitle',SubTitle);
end;

function  TMandala.SaveSettingsToIniFile(const IniFile: TIniFile;
      const Section: String): Boolean;
var s:String;
begin
  Result:=True; s:=Section+' - '+MANDALA_ENGINE_SETTINGS_SUFFIX;
  IniFile.WriteInteger(s,'PaletteSpinSpeed.StepsPerSecondMin',PaletteSpinSpeed.StepsPerSecondMin);
  IniFile.WriteInteger(s,'PaletteSpinSpeed.StepsPerSecondMax',PaletteSpinSpeed.StepsPerSecondMax);
  IniFile.WriteInteger(s,'PaletteSpinSpeed.ChangeIntervalMS',PaletteSpinSpeed.ChangeIntervalMS);
  IniFile.WriteInteger(s,'PaletteSpinSpeed.ChangeDeviationPct',PaletteSpinSpeed.ChangeDeviationPct);
  IniFile.WriteInteger(s,'PaletteChangeSpeed.StepsPerSecondMin',PaletteChangeSpeed.StepsPerSecondMin);
  IniFile.WriteInteger(s,'PaletteChangeSpeed.StepsPerSecondMax',PaletteChangeSpeed.StepsPerSecondMax);
  IniFile.WriteInteger(s,'PaletteChangeSpeed.ChangeIntervalMS',PaletteChangeSpeed.ChangeIntervalMS);
  IniFile.WriteInteger(s,'PaletteChangeSpeed.ChangeDeviationPct',PaletteChangeSpeed.ChangeDeviationPct);
  IniFile.WriteInteger(s,'PaletteEffectsIntervalMS',PaletteEffectsIntervalMS);
  IniFile.WriteInteger(s,'PaletteEffectsDeviationPct',PaletteEffectsDeviationPct);
  IniFile.WriteInteger(s,'PatternChangeIntervalMS',PatternChangeIntervalMS);
  IniFile.WriteInteger(s,'PatternChangeDeviationPct',PatternChangeDeviationPct);
  IniFile.WriteInteger(s,'PatternChangeFadeSteps',PatternChangeFadeSteps);
  IniFile.WriteInteger(s,'PatternChangeFadeStepIntervalMS',PatternChangeFadeStepIntervalMS);
  IniFile.WriteInteger(s,'PatternChangePixelMorphIntervalMS',PatternChangePixelMorphIntervalMS);
  IniFile.WriteBool   (s,'ShowTitleOnStartup',ShowTitleOnStartup);
  IniFile.WriteString (s,'Title',Title);
  IniFile.WriteString (s,'SubTitle',SubTitle);
end;

function  TMandala.LoadPaletteFromMemory(Size:Integer; Data:Pointer):Boolean;
begin
  Self.Enter;
  try     if Palette=nil then Palette:=NewPalette;
          Result:=(Palette<>nil) and Palette.LoadFromMemory(Size,Data);
  finally Self.Leave;
  end;
end;

function  TMandala.LoadPatternFromMemory(Size:Integer; Data:Pointer):Boolean;
begin
  Self.Enter;
  try     if Pattern=nil then Pattern:=NewPattern;
          Result:=(Pattern<>nil) and Pattern.LoadFromMemory(Size,Data);
  finally Self.Leave;
  end;
end;

function  TMandala.SavePaletteToMemory(Size:Integer; Data:Pointer):Boolean;
begin
  Self.Enter;
  try     Result:=(Palette<>nil) and Palette.SaveToMemory(Size,Data);
  finally Self.Leave;
  end;
end;

procedure TMandala.SetFixedSpeed(IntervalMS:Integer);
begin
  Self.Enter;
  try     if PaletteSpinThread<>nil then begin
             PaletteSpinThread.FixedIntervalMS:=IntervalMS;
             PaletteSpinThread.Initialize(PaletteSpinThread.Task);
             end;
  finally Self.Leave;
  end;
end;

function  TMandala.SavePaletteAndPattern:Boolean;
begin
  Self.Enter;
  try     if Palette<>nil then begin
             if SavedPalette =nil then SavedPalette:=NewPalette;
             if SavedPalette<>nil then SavedPalette.Assign(Palette);
             end;
          if Pattern<>nil then begin
             if SavedPattern =nil then SavedPattern:=NewPattern;
             if (SavedPattern=nil) and (PatternThread<>nil) then
                SavedPattern:=PatternThread.DequeuePattern; // steal a pattern from 'PatternThread'
             if SavedPattern<>nil then SavedPattern.Assign(Pattern);
             end;

          Result:=PaletteAndPatternSaved;
  finally Self.Leave;
  end;
end;

function  TMandala.RestorePaletteAndPattern:Boolean;
begin
  Self.Enter;
  try     Result:=PaletteAndPatternSaved;
          if SavedPalette<>nil then begin
             FreePalette(Palette); Palette:=SavedPalette;
             SavedPalette:=nil;
             end;
          if SavedPattern<>nil then begin
             FreePattern(Pattern); Pattern:=SavedPattern;
             SavedPattern:=nil;
             end
  finally Self.Leave;
  end;
end;

function  TMandala.PaletteAndPatternSaved:Boolean;
begin
  Result:=(SavedPalette<>nil) and (SavedPattern<>nil);
end;

function  TMandala.AllSuspended:Boolean;
begin
  Result:=True;
  if Result then Result:=(PaletteSpinThread  =nil) or PaletteSpinThread  .Suspended;
  if Result then Result:=(PaletteChangeThread=nil) or PaletteChangeThread.Suspended;
  if Result then Result:=(PatternThread      =nil) or PatternThread      .Suspended;
end;

procedure Initialize;
var i,j:Integer;
begin
  Mandala:=nil;
  try    Mandala:=TMandala.Create(nil,nil,nil,nil,nil);
  except begin Mandala.Free; Mandala:=nil; Halt(1); end;
  end;

  for i:=Low(AlphaBlendTables) to High(AlphaBlendTables) do
      MakeAlphaBlendTable(i,j,AlphaBlendTables[i]);
end;

initialization
  Initialize;

finalization
  Mandala.Free;

end.
