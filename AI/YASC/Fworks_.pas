unit Fworks_; // simple particle system;

// The eyecandy "Fireworks" is loosely based upon:
// "PyroTechnics" Copyright (c) 1998 by Mike Oliphant and Rob Clark.
// "Boom" Copyright (c) 1999 Nate Miller.

// This eyecandy isn't in production, because it's too primitive.
// Add these options to "Tree.txt" if it ever is used again:

{
				Fireworks
					1-Color rockets
						Interval (milliseconds)
						Deviation (%)
					2-Color rockets
						Interval (milliseconds)
						Deviation (%)
					Menu
						Transparency (%)
						Text shadow
						Colors
							Background
							Button text
							Focused button text
							Grayed button text
							Text shadow
						Font
							Name
							Size
							Color
							Bold
							Italic
							Underlined
}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,ExtCtrls,
  Forms, Dialogs,
  IniFile_,BitMap_,Pict_,Menu_;

type
  TFloat               = Single;
{
  TRGBColor            = LongInt;
  TRGB                 = packed record r,g,b :Byte; end;
  PRGB                 = ^TRGB;
  TRGBVec              = array[0..2000000000 div SizeOf(TRGB)] of TRGB;
  PRGBVec              = ^TRGBVec;

  TRGBAb               = packed record r,g,b,a:Byte; end;
  TRGBAi               = packed record r,b,g,a:Integer; end;
}
  TRGBf                = record r,b,g:TFloat; end;
  TVRGBf               = array[0..2000000000 div SizeOf(TRGBf)] of TRGBf;
  PVRGBf               = ^TVRGBf;

  TRGBAf               = record r,b,g,a:TFloat; end;
  TVRGBAf              = array[0..2000000000 div SizeOf(TRGBAf)] of TRGBAf;
  PVRGBAf             = ^TVRGBAf;
{
const
  COLOR_BLACK :TColor  = TColor(0);
  RGB_BLACK   : TRGB   = (r:  0; g:  0; b:  0);
  RGB_WHITE   : TRGB   = (r:255; g:255; b:255);
}
const
{
  RainBowColorsB : array[0..11,0..2] of Byte = (
    (255,128,128),(255,192,128),(255,255,128),(192,255,128),
    (128,255,128),(128,255,192),(128,255,255),(128,192,255),
    (128,128,255),(192,128,255),(255,128,255),(255,128,192));
}
  RainBowColorsF : array [0..11, 0..2] of TFloat = (
	(1.0,0.5,0.5),(1.0,0.75,0.5),(1.0,1.0,0.5),(0.75,1.0,0.5),
	(0.5,1.0,0.5),(0.5,1.0,0.75),(0.5,1.0,1.0),(0.5,0.75,1.0),
	(0.5,0.5,1.0),(0.75,0.5,1.0),(1.0,0.5,1.0),(1.0,0.5,0.75));

  FIREWORKS_TEMPLATE_COUNT = 5;

type
  TTime      = Cardinal;
  TVertex3Df = record x,y,z:TFloat; end;

  TFireworkFlag     = (ffNull,ffGroundPos,ffLifeDependsOnVerticalSpeed,
                       ffTiltTowardsCenter,ffReincarnation,
                       ffRainBowColor,ffRandomColor);
  TFireworkFlagSet  = set of TFireworkFlag;

  TFWorkButtonType  = (fbSettings);

  TBuiltinParticleTemplateType = (ptFountain,ptFountainParticle,ptRocket,ptRocketTrail,
                                  ptWhite,ptRed,
                                  ptRainbowColor,
                                  ptRandomColor);

  PExplosion        = ^TExplosion;
  PFireworkTemplate = ^TFireworkTemplate;
  PParticle         = ^TParticle;
  PParticleTemplate = ^TParticleTemplate;
  PTrail            = ^TTrail;

  TParticleFunction = function(Particle:PParticle):Integer of object;

  TTemplateType     = (ttParticle,ttFirework);
  TTemplateMode     = (tmStickToOneTemplate,tmSelectATemplateBasedOnFrequency);

  TTrail = record
    Particle:PParticleTemplate;
    ParticlesPerSecond:TFloat; // zero if new particle for each frame; otherwise as the name says
  end;

  TExplosion = record
    Count:Integer;
    CountDeviation:Integer;
    Frequency:array[0..FIREWORKS_TEMPLATE_COUNT-1] of Integer;
    FrequencySum:Integer; // sum of 'Frequency[]'
    Mode:TTemplateMode;
    TemplateCount:Integer;
    TemplateType: TTemplateType; // 'particle' or 'fireworks'
    Template:array[0..FIREWORKS_TEMPLATE_COUNT-1] of Pointer; // a template is either a 'particle' or a 'firework'
    Trail   :array[0..FIREWORKS_TEMPLATE_COUNT-1] of TTrail;  // trail particles for each template
  end;

  TTimerControl = record
    Enabled:Boolean;
    Interval:TTime;
    IntervalDeviation:TTime;
    IntervalDeviationPct:Integer;
    NextTime:TTime;
  end;

  TParticleTemplate = record
    ColorDeviation:TFloat;
    ColorFunction:TParticleFunction;
    Direction,            DirectionDeviation:TVertex3Df;
    Flags:TFireworkFlagSet;
    Life,                 LifeDeviation:TFloat; // seconds
    MoveFunction:TParticleFunction;
    Name:String;
    RGBAf:TRGBAf; // color
    Size:Integer; // display size
    Velocity,             VelocityDeviation:TFloat;
  end;

  TBuiltinFireworkTemplateType = (ftFountain,
                                   ftRocket1Color,ftRocket2Colors);

  TFireworkTemplate = record
    Name:String;
    Explosion:TExplosion;
    Flags:TFireworkFlagSet;
    Particle:PParticleTemplate; // self particle-type
    Timer:TTimerControl;
    Trail:TTrail;          // emit this particle as trail, if any
  end;

  TParticle = record
    Alpha:TFloat;          // current transparency
    AlphaFade:TFloat;      // change in transparency per second
    ColorIndex:Integer;    // color index when created
    Enabled:Boolean;       // alive ?
    Explosion:PExplosion;  // explosion when time expires, if any
    Flags:TFireworkFlagSet;
    Life:TFloat;           // remaining lifetime, seconds
    Next:PParticle;        // single linked list of particles
    Parent:PParticle;      // parent particle, if any
    Position:TVertex3Df;   // current position
    RGBAf:TRGBAf;          // color, float
    Size:Integer;          // size (square)
    StartPosition:TVertex3Df;   // position when created, for reincarnation
    Template:PParticleTemplate; // particle type
    Trail:PTrail;          // trail particles, if any
    TrailParticleCount:TFloat; // trail particles due for emission;
    Velocity:TVertex3Df;   // speed per second
  end;

  TVParticle = array[0..2000000000 div SizeOf(TParticle)] of PParticle;
  PVParticle = ^TVParticle;

  TFireworks = class
  private
    fParticleCapacity:Integer; // size of particle vector
    fRect:TRect; // canvas window
    FireworksTemplates:TList;
    GroundLevel:TFloat; // float version of 'Rect.Bottom';
    Height:Integer; // height of 'Rect';
    ParticleFreeList:PParticle; // linked list of free particles
    ParticleTemplates:TList;
    ParticleTop:Integer; // last used particle in vector
    TimeForNextUpdate:TTime;
    Width:Integer; // width of 'Rect';
    ZeroVertex3Df:TVertex3Df;
    procedure   Clear(ParticlesOnly:Boolean);
    procedure   ClearList(List:Classes.TList);
    function InterpolateColors(Factor: TFloat; ColorIndex1,
      ColorIndex2: Integer; Alpha:TFloat): TRGBAf;
    procedure   MakeBuiltinParticleTemplate(
      TemplateType:TBuiltinParticleTemplateType;
      Template:PParticleTemplate);
    procedure MakeBuiltinFireworkTemplate(
      TemplateType: TBuiltinFireworkTemplateType;
      Template: PFireworkTemplate);
    procedure   NewRandomColor;
    procedure   SetParticleCapacity(ParticleCapacity__:Integer);
    procedure   SetRect(Rect__:TRect);
  public
    Viscosity:TFloat; //
    AlphaStart:TFloat;  // transparency start value
    AlphaEnd:TFloat;    // transparency end value
    BuiltinFireworkTemplate:Array[TBuiltinFireworkTemplateType] of PFireworkTemplate;
    BuiltinParticleTemplate:Array[TBuiltinParticleTemplateType] of PParticleTemplate;
    ColorIndex:Integer; // current color-table index, for color circulation control
    Canvas:TCanvas;
    Gravity:TVertex3Df;
    Menu:TFireworksMenu;
    NextColorIndex:Integer; // for color circulation control
    Particles: PVParticle; // public for easy programming
    RandomColor:TRGBAf;  // current random-color,  interpolated between to succeeding colors in the rainbow-palette
    RainbowColor:TRGBAf; // current rainbow-color, interpolated between to succeeding colors in the rainbow-palette
    TimeElapsed:TFloat;  // time elapsed since last update, seconds
    TimeForNextColorIndexChange:TTime;
    TimeNow:TTime;
    TimeOfLastColorIndexChange:TTime; // for color circulation control
    TimeOfLastUpdate:TTime;
    TimerInterval:TTime;
    TimerIntervalForColorIndexChange:TTime;
    Visible:Boolean;

    constructor Create(ParticleCapacity__:Integer; Canvas__:TCanvas; Rect__:TRect; TimerInterval__:TTime; MenuPanel__:TPanel);
    destructor  Destroy; override;
    function    LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
    function    NewFirework(const Position__,Velocity__:TVertex3Df;
                            FireworkTemplate: PFireworkTemplate):PParticle;
    function    NewFireworkTemplate: PFireworkTemplate;
    function    NewParticle(const Position__,Velocity__:TVertex3Df;
                            ParticleTemplate:PParticleTemplate;
                            ExplosionTemplate:PExplosion;
                            TrailTemplate:PTrail): PParticle;
    function    NewParticleTemplate:PParticleTemplate;
    procedure   Reset;
    function    SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
    procedure   Show;
    procedure   SimulateRun(Seconds:Cardinal);
    procedure   Step;

    property    ParticleCapacity:Integer read fParticleCapacity write SetParticleCapacity;
    property    Rect:TRect read fRect write SetRect; // canvas window
  end;

var
  Fireworks : TFireworks = nil;

implementation

uses Buttons,Misc_,SokUtil_,Text_,Main_;

const
  FIREWORKS_INIFILE_SECTION='Fireworks'; // don't localize

var
  DSIN:array[0..360] of TFloat;
  DCOS:array[0..360] of TFloat;
{
function  ColorToRGB(Color:TColor):TRGB;
begin
  Result.b:= Color         and 255;
  Result.g:=(Color shr  8) and 255;
  Result.r:=(Color shr 16) and 255;
end;
}
function  LerpFactor(Value,Left,Right:Cardinal):TFloat;
begin // linear interpolation factor;
  if   Value<=Left then Result:=0.0
  else if Value>=Right then Result:=1.0
       else Result:=(Value-Left)/(Right-Left);
end;

function  RGBToColor(const RGB:TRGB):TColor;
begin
  Result:=RGB.b+(RGB.g shl 8)+(RGB.r shl 16);
end;

function Vector3DfLength(x,y,z:TFloat):TFloat;
begin
  Result:=Sqrt(x*x+y*y+z*z);
end;

constructor TFireworks.Create(ParticleCapacity__:Integer; Canvas__:TCanvas; Rect__:TRect; TimerInterval__:TTime; MenuPanel__:TPanel);
var f:TBuiltinFireworkTemplateType; p:TBuiltinParticleTemplateType;
begin
  Inherited Create;

  Menu:=nil;
  try Menu:=TFireworksMenu.Create(nil,MenuPanel__,MenuPanel__.Font.Size);
  except on E:Exception do begin Menu.Free; Menu:=nil; end;
  end;

  Clear(False); Randomize;

  try    ParticleTemplates:=Classes.TList.Create;
  except on E:Exception do ParticleTemplates:=nil;
  end;

  FireworksTemplates:=nil;
  try    FireworksTemplates:=Classes.TList.Create;
  except on E:Exception do begin FireworksTemplates.Free; FireworksTemplates:=nil; end;
  end;

  Canvas:=Canvas__; Rect:=Rect__;
  ParticleCapacity:=ParticleCapacity__;
  TimerInterval:=TimerInterval__; if TimerInterval<1 then TimerInterval:=1;
  FillChar(BuiltinParticleTemplate,SizeOf(BuiltinParticleTemplate),0);
  FillChar(BuiltinFireworkTemplate,SizeOf(BuiltinFireworkTemplate),0);
  FillChar(ZeroVertex3Df,SizeOf(ZeroVertex3Df),0);

  Visible:=True;
  Gravity.y:=9.81; // default values; can be changed later
  TimeForNextUpdate:=0;
  TimerIntervalForColorIndexChange:=513; // ... modulo 'TimerInterval' should be <> 0;
  Viscosity:=2.0;
  AlphaStart:=1.0;
  AlphaEnd:=0.45;

  NewRandomColor;

  for p:=Low (BuiltinParticleTemplate) to
         High(BuiltinParticleTemplate) do begin
         BuiltinParticleTemplate[p]:=NewParticleTemplate;
         MakeBuiltinParticleTemplate(p,BuiltinParticleTemplate[p]);
         end;

  for f:=Low (BuiltinFireworkTemplate) to
         High(BuiltinFireworkTemplate) do begin
         BuiltinFireworkTemplate[f]:=NewFireworkTemplate;
         MakeBuiltinFireworkTemplate(f,BuiltinFireworkTemplate[f]);
         end;

  if Menu<>nil then begin
     if    Menu.MenuPanel<>nil then
           Menu.SetDefaultValues(TColor((128 shl 16)+255),Menu.MenuPanel.Font.Size)
     else  Menu.SetDefaultValues(TColor((128 shl 16)+255),10);
     end;
end;

destructor  TFireworks.Destroy;
begin
  Clear(False);
  Menu.Free;
  Inherited Destroy;
end;

procedure TFireworks.Clear(ParticlesOnly:Boolean);
var i:Integer;
begin
  if Particles<>nil then begin
     for i:=0 to ParticleTop do
         if Particles[i]<>nil then FreeMem(Particles[i]);
     FreeMem(Particles);
     end;
  Particles:=nil; fParticleCapacity:=0; ParticleTop:=-1; ParticleFreeList:=nil;

  if not ParticlesOnly then begin
     ClearList(FireworksTemplates); FireworksTemplates.Free; FireworksTemplates:=nil;
     ClearList(ParticleTemplates); ParticleTemplates  .Free; ParticleTemplates :=nil;
     end;
end;

procedure TFireworks.ClearList(List:Classes.TList);
var i:Integer;
begin
  if List<>nil then with List do
    for i:=0 to Pred(Count) do
        if Items[i]<>nil then FreeMem(Items[i]);
end;

procedure TFireworks.SetParticleCapacity(ParticleCapacity__:Integer);
begin // simple implementation: deletes old particles
  if ParticleCapacity__<>ParticleCapacity then begin
     Clear(True);
     if ParticleCapacity__<>0 then
        try    GetMem(Particles,ParticleCapacity__*SizeOf(TParticle));
               FillChar(Particles^,ParticleCapacity__*SizeOf(TParticle),0);
               fParticleCapacity:=ParticleCapacity__;
        except on E:Exception do begin
               Application.MessageBox(PChar(E.Message),PChar(Application.Title),MB_OK+MB_ICONERROR);
               Clear(True);
               end;
        end;
     end;
end;

procedure TFireworks.SetRect(Rect__:TRect);
begin
  fRect:=Rect__;
  Width:=Rect.Right-Rect.Left;
  Height:=Rect.Bottom-Rect.Top;
  GroundLevel:=Rect.Bottom;
end;

procedure TFireworks.MakeBuiltinParticleTemplate(TemplateType:TBuiltinParticleTemplateType; Template:PParticleTemplate);
begin
  if Template<>nil then with Template^ do begin
     FillChar(Template^,SizeOf(Template^),0);
     with RGBAf do begin r:=-1.0; g:=-1.0; b:=-1.0; a:=-1.0; end;
     ColorDeviation:=0.1;
     with DirectionDeviation do begin x:=1.0; y:=1.0; z:=1.0; end;
     Velocity:=50.0; VelocityDeviation:=10.0;
     Life:=1.0; LifeDeviation:=0.2;
     Size:=1;

     case TemplateType of
       ptFountain         : begin FillChar(Template^,SizeOf(Template^),0);
                                  Name:='Fountain';
                                  Life:=0.0;
                            end;
       ptFountainParticle : begin Name:='FountainParticle';
                                  with Direction do begin x:=0.0; y:=-50.0; z:=0.0; end;
                                  with DirectionDeviation do begin x:= 10.0; y:=10.0; z:=10.0; end;
                                  Flags:=[ffRainbowColor,ffReincarnation];
                                  Life:=5.0; LifeDeviation:=3.0;
                                  Size:=-2;
                                  Velocity:=25.0; VelocityDeviation:=10.0;
                            end;
       ptRocket           : begin Name:='Rocket';
                                  with RGBAf do begin r:=0.9; g:=0.9; b:=0.9; a:=0.9; end;
                                  Direction.y:=-1.2;
                                  DirectionDeviation.y:=0.1;
                                  Flags:=[ffGroundPos,ffLifeDependsOnVerticalSpeed,
                                          ffTiltTowardsCenter];
                                  Size:=-2;
                                  Velocity:=60.0; VelocityDeviation:=25.0;
                            end;
       ptRocketTrail      : begin Name:='RocketTrail';
                                  with RGBAf do begin r:=0.8; g:=0.8; b:=0.8; a:=0.8; end;
                                  ColorDeviation:=1.0;
                                  Life:=0.3; LifeDeviation:=0.2;
                                  Velocity:=0.0; VelocityDeviation:=0.5;
                            end;
       ptWhite            : begin Name:='White';
                                  with RGBAf do begin r:=1.0; g:=1.0; b:=1.0; a:=1.0; end;
                                  ColorDeviation:=0.0;
                                  Life:=1.75; LifeDeviation:=0.5;
                                  Velocity:=50.0;
                            end;
       ptRed              : begin Name:='Red';
                                  with RGBAf do begin r:=0.9; g:=0.1; b:=0.1; a:=1.0; end;
                                  ColorDeviation:=0.1;
                                  Life:=1.75; LifeDeviation:=0.5;
                                  Velocity:=25.0;
                            end;
       ptRainbowColor     : begin Name:='RainbowColor';
                                  ColorDeviation:=0.5;
                                  Flags:=[ffRainBowColor];
                                  Life:=1.75; LifeDeviation:=0.5;
                                  Velocity:=50.0; VelocityDeviation:=2.0;
                            end;

       ptRandomColor      : begin Name:='RandomColor';
                                  ColorDeviation:=0.5;
                                  Flags:=[ffRandomColor];
                                  Life:=1.75; LifeDeviation:=0.5;
                                  Velocity:=30.0; VelocityDeviation:=2.0;
                            end;


     end; // case
     end;
end;

procedure TFireworks.MakeBuiltinFireworkTemplate(TemplateType:TBuiltinFireworkTemplateType; Template:PFireworkTemplate);
var i:Integer;
begin
  if Template<>nil then with Template^ do begin
     FillChar(Template^,SizeOf(Template^),0);
     Explosion.Count:=100; Explosion.CountDeviation:=30;
     Explosion.Mode:=tmSelectATemplateBasedOnFrequency;
     Explosion.TemplateType:=ttParticle;

     case TemplateType of
       ftFountain  : begin Name:='Fountain';
                           Particle:=BuiltinParticleTemplate[ptFountain];
                           Explosion.Count:=200;
                           Explosion.TemplateCount:=1;
                           Explosion.Template[0]:=BuiltinParticleTemplate[ptFountainParticle];
                     end;
       ftRocket1Color
                   : begin Name:='Rocket1';
                           Particle:=BuiltinParticleTemplate[ptRocket];
                           Explosion.Count:=50;
                           Explosion.CountDeviation:=25;
                           Explosion.Mode:=tmStickToOneTemplate;
                           Explosion.TemplateCount:=3;
                           Explosion.Frequency[0]:=85;
                           Explosion.Frequency[1]:=10;
                           Explosion.Frequency[2]:=5;
                           Explosion.Template[0]:=BuiltinParticleTemplate[ptRandomColor];
                           Explosion.Template[1]:=BuiltinParticleTemplate[ptWhite];
                           Explosion.Template[2]:=BuiltinParticleTemplate[ptRainbowColor];
                           for i:=0 to Pred(Explosion.TemplateCount) do
                               with Explosion.Trail[i] do begin
                                 Particle:=BuiltinParticleTemplate[ptRocketTrail];
                                 ParticlesPerSecond:=0.0;
                                 end;
                           Timer.Enabled:=True;
                           Timer.Interval:=1000;
                           Timer.IntervalDeviation:=250;
                           Timer.IntervalDeviationPct:=25;
                           Trail.Particle:=BuiltinParticleTemplate[ptRocketTrail];
                           Trail.ParticlesPerSecond:=0.0;
                     end;
       ftRocket2Colors
                   : begin Name:='Rocket2';
                           Particle:=BuiltinParticleTemplate[ptRocket];
                           Explosion.Count:=75;
                           Explosion.CountDeviation:=25;
                           Explosion.TemplateCount:=2;
                           Explosion.Frequency[0]:=50;
                           Explosion.Frequency[1]:=50;
                           Explosion.Template[0]:=BuiltinParticleTemplate[ptRandomColor];
                           Explosion.Template[1]:=BuiltinParticleTemplate[ptRandomColor];
                           for i:=0 to Pred(Explosion.TemplateCount) do
                               with Explosion.Trail[i] do begin
                                 Particle:=BuiltinParticleTemplate[ptRocketTrail];
                                 ParticlesPerSecond:=0.0;
                                 end;
                           Timer.Enabled:=True;
                           Timer.Interval:=1500;
                           Timer.IntervalDeviation:=500;
                           Timer.IntervalDeviationPct:=33;
                           Trail.Particle:=BuiltinParticleTemplate[ptRocketTrail];
                           Trail.ParticlesPerSecond:=0.0;
                     end;
     end; // case
     with Explosion do
       for i:=0 to Pred(TemplateCount) do Inc(FrequencySum,Frequency[i]);
     end;
end;

function  TFireworks.NewParticleTemplate:PParticleTemplate;
begin
  if ParticleTemplates<>nil then
     try    GetMem(Result,SizeOf(Result^));
            FillChar(Result^,SizeOf(Result^),0);
            ParticleTemplates.Add(Result);
     except on E:Exception do Result:=nil;
     end
  else Result:=nil;
end;

function  TFireworks.NewFireworkTemplate:PFireworkTemplate;
begin
  if FireworksTemplates<>nil then
     try    GetMem(Result,SizeOf(Result^));
            FillChar(Result^,SizeOf(Result^),0);
            FireworksTemplates.Add(Result);
     except on E:Exception do Result:=nil;
     end
  else Result:=nil;
end;

function  TFireworks.NewFirework(const Position__,Velocity__:TVertex3Df; FireworkTemplate:PFireworkTemplate):PParticle;

begin
  if   (FireworkTemplate<>nil) and (FireworkTemplate.Particle<>nil) then
       Result:=NewParticle(Position__,Velocity__,FireworkTemplate.Particle,
                           Addr(FireworkTemplate.Explosion),
                           Addr(FireworkTemplate.Trail))
  else Result:=nil;
end;

function  TFireworks.NewParticle(
            const Position__,Velocity__:TVertex3Df;
            ParticleTemplate:PParticleTemplate;
            ExplosionTemplate:PExplosion;
            TrailTemplate:PTrail): PParticle;
var t,u,v,Scale:TFloat;
begin
  if   ParticleFreeList<>nil then begin
       Result:=ParticleFreeList;
       ParticleFreeList:=Result.Next;
       end
  else if ParticleTop<Pred(ParticleCapacity) then
          try    GetMem(Result,SizeOf(Result^));
                 Inc(ParticleTop); Particles[ParticleTop]:=Result;
          except on E:Exception do Result:=nil;
          end
       else Result:=nil;
  if Result<>nil then with Result^ do begin
     Template       :=ParticleTemplate;
     Explosion      :=ExplosionTemplate;
     Trail          :=TrailTemplate;

     Alpha          :=AlphaStart;
     ColorIndex     :=Self.ColorIndex;
     Flags          :=Template.Flags;

     if ffGroundPos in Flags then begin
        Position.x  :=Random(Width);
        Position.y  :=Pred(Height);
        Position.z  :=0.0;
        end
     else Position  :=Position__;
     StartPosition  :=Position;

     Velocity.x     :=Template.Direction.x+Template.DirectionDeviation.x-2.0*Random*Template.DirectionDeviation.x;
     Velocity.y     :=Template.Direction.y+Template.DirectionDeviation.y-2.0*Random*Template.DirectionDeviation.y;
     Velocity.z     :=Template.Direction.z+Template.DirectionDeviation.z-2.0*Random*Template.DirectionDeviation.z;
     u              :=Vector3DfLength(Velocity.x,Velocity.y,Velocity.z);
     if Abs(u)<1e-5 then u:=1.0;
     v              :=Template.Velocity+Template.VelocityDeviation-2.0*Random*Template.VelocityDeviation;
     Scale          :=v/u;
     Velocity.x     :=Velocity__.x+Velocity.x*Scale;
     Velocity.y     :=Velocity__.y+Velocity.y*Scale;
     Velocity.z     :=Velocity__.z+Velocity.z*Scale;
     if ffTiltTowardsCenter in Flags then
        if   Position.x<Width div 2 then
             Velocity.x:= Abs(Velocity.x)
        else Velocity.x:=-Abs(Velocity.x);

     if   (ffLifeDependsOnVerticalSpeed in Flags) and
          (Abs(Velocity.y)>1.0) then // calculate life based on window height
          Life      :=Height/(2*Abs(Velocity.y))+ // at least give the particle time to go half way up
                        Random*(Height/Abs(Velocity.y))
     else Life      :=Template.Life+Template.LifeDeviation-2.0*Random*Template.LifeDeviation;

     Next           :=nil;

     if      ffRainbowColor in Flags then
             RGBAf  :=RainbowColor
     else if ffRandomColor in Flags then
             RGBAf  :=RandomColor
     else    RGBAf  :=Template.RGBAf;

     if Template.ColorDeviation<>0.0  then with RGBAf do begin
        //t     :=Template.ColorDeviation-2.0*Random*Template.ColorDeviation;
        //if t<0.0 then t:=-t; // note that only positive values are used
        t       :=Random*Template.ColorDeviation; // simplified version
        r       :=r+t*(1.0-r);
        g       :=g+t*(1.0-g);
        b       :=b+t*(1.0-b);
        end;

     Alpha          :=AlphaStart;
     if   Life>1e-5 then
          AlphaFade :=(AlphaStart-AlphaEnd) / Life
     else AlphaFade := 0.0;

     Size           :=Template.Size;
     TrailParticleCount
                    :=0.0;
     Enabled        :=True;
     end;
end;

procedure TFireworks.Show;
var i,xi,yi:Integer;
begin
  for i:=0 to ParticleTop do with Particles[i]^ do
      if Enabled then begin
         xi:=Round(Position.x);
         yi:=Round(Position.y);

         if PtInRect(Rect,Point(xi,yi)) then begin
            with RGBAf do
              Canvas.Brush.Color:=TColor(
                (Trunc(Alpha*r*255))+        // reversed color-components: bgr
                (Trunc(Alpha*g*255) shl 8)+
                (Trunc(Alpha*b*255) shl 16));
            if   Size=1 then
                 Canvas.Pixels[xi,yi]:=Canvas.Brush.Color
            else if Size>1 then
                 Canvas.FillRect(Classes.Rect(xi,yi,xi+Size,yi+Size))
            else if   Random(-Size)=1 then
                 Canvas.FillRect(Classes.Rect(xi,yi,xi+1+Random(-Size),yi+1+Random(-Size)))
            else Canvas.Pixels[xi,yi]:=Canvas.Brush.Color;
            end;
         end;
end;

procedure TFireworks.Step;
const TRAIL_COLOR_FACTOR:TFloat=0.6;//0.60;
var c,i,j,k,m,n:Integer; Scale,Speed:TFloat;
    p:PParticle; TrailColorFactor:TFloat;
    TemplateUsed       :array[0..FIREWORKS_TEMPLATE_COUNT-1] of Boolean;
    TemplateRandomColor:array[0..FIREWORKS_TEMPLATE_COUNT-1] of TRGBAf;

begin
  TimeNow:=GetTickCount;
//if TimeNow>=TimeForNextUpdate then begin
     if TimeOfLastUpdate=0 then TimeOfLastUpdate:=TimeNow;

     if TimeNow>=TimeOfLastUpdate then
        TimeElapsed:=(TimeNow-TimeOfLastUpdate)*0.001
     else begin // clock wrap-around
        TimeElapsed:=(TimeNow+($ffffffff-TimeOfLastUpdate))*0.001;

        if FireworksTemplates<>nil then with FireworksTemplates do
           for i:=0 to Pred(Count) do // reset fireworks timers
               if Items[i]<>nil then with PFireworkTemplate(Items[i])^.Timer do
                  NextTime:=TimeNow+TTime(Random(Interval));; // of course, this is not 100% correct, but it keeps things going
        end;

     if TimeNow>=TimeForNextColorIndexChange then begin
        ColorIndex    :=Pred(ColorIndex) mod (Succ(High(RainBowColorsf)));
        if ColorIndex<0 then ColorIndex:=High(RainBowColorsf);
        NextColorIndex:=Pred(ColorIndex) mod (Succ(High(RainBowColorsf)));
        if NextColorIndex<0 then NextColorIndex:=High(RainBowColorsf);
        TimeOfLastColorIndexChange:=TimeNow;
        TimeForNextColorIndexChange:=TimeNow+TimerIntervalForColorIndexChange;
        RainbowColor:=InterpolateColors(
                        LerpFactor(TimeNow,TimeOfLastColorIndexChange,TimeForNextColorIndexChange),
                        ColorIndex,NextColorIndex,AlphaStart);
        end;

     if FireworksTemplates<>nil then with FireworksTemplates do
        for i:=0 to Pred(Count) do
            if Items[i]<>nil then with PFireworkTemplate(Items[i])^.Timer do
               if Enabled and (TimeNow>=NextTime) then begin
                  NewFireWork(ZeroVertex3Df,ZeroVertex3Df,PFireworkTemplate(Items[i]));
                  NextTime:=TimeNow+Interval+IntervalDeviation-TTime(2*Random(Integer(IntervalDeviation)));
                  end;

     for i:=0 to ParticleTop do with Particles[i]^ do
         if Enabled then begin
            Life:=Life-TimeElapsed;
            if Life<=0.0 then begin
               Enabled:=False;
               Next:=ParticleFreeList;
               ParticleFreeList:=Particles[i];
               if ffReincarnation in Flags then begin
                  if ffRandomColor in Template.Flags then NewRandomColor;
                  NewParticle(StartPosition,ZeroVertex3Df,Template,Explosion,Trail);
                  end;
               if Explosion<>nil then with Explosion^ do begin // make explosion
                  k:=0;
                  FillChar(TemplateUsed,SizeOf(TemplateUsed),0);
                  for j:=0 to Pred(Count)+CountDeviation-2*Random(CountDeviation) do begin
                      if (TemplateCount>1) and
                         ((j=0) or (Mode=tmSelectATemplateBasedOnFrequency)) then begin // pick a template  based on frequency
                         m:=Random(FrequencySum); c:=0;
                         for n:=0 to Pred(TemplateCount) do begin
                             Inc(c,Frequency[n]);
                             if m<c then begin k:=n; break; end;
                             end;

                         end;
                      if Template[k]<>nil then
                         if   TemplateType=ttParticle then begin
                              if (ffRandomColor in PParticleTemplate(Template[k])^.Flags) then
                                 if TemplateUsed[k] then
                                    RandomColor:=TemplateRandomColor[k]
                                 else begin
                                    NewRandomColor;
                                    TemplateRandomColor[k]:=RandomColor;
                                    TemplateUsed[k]:=True;
                                    end;
                              NewParticle(Position,ZeroVertex3Df,Template[k],nil,Addr(Trail[k]));
                              end
                         else NewFirework(Position,ZeroVertex3Df,Template[k]);
                      end;
                  end;
               end
            else begin // particle still alive
               Alpha     :=Alpha     -AlphaFade *TimeElapsed;

               RGBAf.a   :=Alpha;

               Velocity.x:=Velocity.x+Gravity.x*TimeElapsed;
               Velocity.y:=Velocity.y+Gravity.y*TimeElapsed;
               Velocity.z:=Velocity.z+Gravity.z*TimeElapsed;

               if Viscosity<>0.0 then begin
                  Speed     :=Vector3DfLength(Velocity.x,Velocity.y,Velocity.z);
                  Scale     :=(Speed-((Viscosity+Random*0.005)*TimeElapsed))/Speed;
                  Velocity.x:=Velocity.x*Scale;
                  Velocity.y:=Velocity.y*Scale;
                  Velocity.z:=Velocity.z*Scale;
                  end;

               Position.x:=Position.x+Velocity.x*TimeElapsed;
               Position.y:=Position.y+Velocity.y*TimeElapsed;
               Position.z:=Position.z+Velocity.z*TimeElapsed;

               if   Position.y>=GroundLevel then begin
                    Life:=0.0; Explosion:=nil; // process the particle in next run
                    end
               else if (Trail<>nil) and
                       (Trail.Particle<>nil) then with Trail^ do begin
                       TrailParticleCount:=TrailParticleCount+ParticlesPerSecond*TimeElapsed;
                       if (ParticlesPerSecond=0.0) or
                          (TrailParticleCount>=1.0) then
                          repeat if ffRandomColor in Particle.Flags then NewRandomColor;
                                 p:=NewParticle(Position,ZeroVertex3Df,Particle,nil,nil);
                                 if p<>nil then begin // darken the trail
                                    p^.RGBAf.r := TRAIL_COLOR_FACTOR*RGBAf.r;
                                    p^.RGBAf.g := TRAIL_COLOR_FACTOR*RGBAf.g;
                                    p^.RGBAf.b := TRAIL_COLOR_FACTOR*RGBAf.b;
                                    end;
                                 TrailParticleCount:=TrailParticleCount-1.0;
                          until  TrailParticleCount<1.0;
                       end;
               end;
            end;

     TimeOfLastUpdate:=TimeNow;
     TimeForNextUpdate:=TimeNow+TimerInterval;
//   end;
end;

procedure TFireworks.SimulateRun(Seconds:Cardinal);
var i:Integer;
begin
  for i:=0 to Seconds*(1000 div TimerInterval) do begin // similate an n-seconds run
      TimeOfLastUpdate:=GetTickCount-TimerInterval;
      Step;
      end;
end;

procedure TFireworks.Reset;
var i:Integer; TimeNow:TTime;
begin
  TimeOfLastUpdate:=0;
  for i:=0 to ParticleTop do with Particles[i]^ do
      if Enabled then begin
         Enabled:=False;
         Next:=ParticleFreeList;
         ParticleFreeList:=Particles[i];
         end;

  TimeNow:=GetTickCount;
  if FireworksTemplates<>nil then with FireworksTemplates do
     for i:=0 to Pred(Count) do
         if Items[i]<>nil then with PFireworkTemplate(Items[i])^.Timer do
            NextTime:=TimeNow+TTime(Random(Interval));

  NewRandomColor;
end;

function  TFireworks.InterpolateColors(Factor:TFloat; ColorIndex1,ColorIndex2:Integer; Alpha:TFloat):TRGBAf;
begin
  with Result do begin
    r        :=RainBowColorsF[ColorIndex1,0]+Factor*(RainBowColorsF[ColorIndex2,0]-RainBowColorsF[ColorIndex1,0]);
    g        :=RainBowColorsF[ColorIndex1,1]+Factor*(RainBowColorsF[ColorIndex2,1]-RainBowColorsF[ColorIndex1,1]);
    b        :=RainBowColorsF[ColorIndex1,2]+Factor*(RainBowColorsF[ColorIndex2,2]-RainBowColorsF[ColorIndex1,2]);
    a        :=Alpha;
    end;
end;

procedure TFireworks.NewRandomColor;
var i,j:Integer;
begin
  i        :=Random(Succ(High(RainbowColorsf)));
  if i=0 then j:=High(RainBowColorsf)
  else j   :=Pred(i);
  RandomColor:=InterpolateColors(Random,i,j,AlphaStart);
end;

function  TFireworks.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
begin
//Result:=True; exit;
  if BuiltinfireworkTemplate[ftRocket1Color]<>nil then
     with BuiltinfireworkTemplate[ftRocket1Color].Timer do begin
       Interval:=Max(0,Min(10000,IniFile.ReadInteger(FIREWORKS_INIFILE_SECTION,'1ColorRocketInterval',Interval)));
       IntervalDeviationPct:=Max(0,Min(100,IniFile.ReadInteger(FIREWORKS_INIFILE_SECTION,'1ColorRocketDeviationPct',IntervalDeviationPct)));
       IntervalDeviation:=(Cardinal(IntervalDeviationPct)*Interval) div 100;
       end;
  if BuiltinfireworkTemplate[ftRocket2Colors]<>nil then
     with BuiltinfireworkTemplate[ftRocket2Colors].Timer do begin
       Interval:=Max(0,Min(10000,IniFile.ReadInteger(FIREWORKS_INIFILE_SECTION,'2ColorRocketInterval',Interval)));
       IntervalDeviationPct:=Max(0,Min(100,IniFile.ReadInteger(FIREWORKS_INIFILE_SECTION,'2ColorRocketDeviationPct',IntervalDeviationPct)));
       IntervalDeviation:=(Cardinal(IntervalDeviationPct)*Interval) div 100;
       end;
  Result:=(Menu<>nil) and Menu.LoadSettingsFromIniFile(IniFile,FIREWORKS_INIFILE_SECTION);
end;

function  TFireworks.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
begin
  if BuiltinfireworkTemplate[ftRocket1Color]<>nil then
     with BuiltinfireworkTemplate[ftRocket1Color].Timer do begin
       IniFile.WriteInteger(FIREWORKS_INIFILE_SECTION,'1ColorRocketInterval',Interval);
       IniFile.WriteInteger(FIREWORKS_INIFILE_SECTION,'1ColorRocketDeviationPct',IntervalDeviationPct);
       end;
  if BuiltinfireworkTemplate[ftRocket2Colors]<>nil then
     with BuiltinfireworkTemplate[ftRocket2Colors].Timer do begin
       IniFile.WriteInteger(FIREWORKS_INIFILE_SECTION,'2ColorRocketInterval',Interval);
       IniFile.WriteInteger(FIREWORKS_INIFILE_SECTION,'2ColorRocketDeviationPct',IntervalDeviationPct);
       end;
  Result:=(Menu<>nil) and Menu.SaveSettingsToIniFile(IniFile,FIREWORKS_INIFILE_SECTION);
end;

procedure Initialize;
var i:Integer;
begin
  for i:=0 to 360 do begin
      DSIN[i]:=System.Sin(i*PI/180);
      DCOS[i]:=System.Cos(i*PI/180);
      end;
end;

initialization
  Initialize;

end.
