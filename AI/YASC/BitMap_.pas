unit BitMap_;

interface

uses Windows,Classes,Graphics,Misc_,SokUtil_;

type
//TImageView           = (ivNone,ivFill,ivStretch,ivCenter,ivTile);
  TBoundsRect          = record Left,Top,Width,Height:Integer;
                         end;
  THSVColor            = packed record
                         H, S, V : Single;
                         end;
  THSVVector           = array[0..(MaxInt div SizeOf(THSVColor))-1] of THSVColor;
  PHSVVector           = ^THSVVector;
  TRGBColor            = LongInt;
  TRGB                 = packed record
                           case Boolean of
                             False : (r,g,b:Byte;);
                             True  : (rgbs :array[0..2] of Byte);
                         end;
  // kludge: 'rgb' is really 'bgr';
  // note this declaration overrides 'TRGB' in 'Mandal2_', where 'TRGB' is declared as bgr
  PRGB                 = ^TRGB;
  TRGBVector           = array[0..(MaxInt div SizeOf(TRGB))-1] of TRGB;
  PRGBVector           = ^TRGBVector;

  TRGBA                = packed record r,g,b,a:Byte; end;
  PRGBA                = ^TRGBA;
  TRGBAVector          = array[0..(MaxInt div SizeOf(TRGBA))-1] of TRGBA;
  PRGBAVector          = ^TRGBAVector;

  TRGBAi               = packed record r,b,g,a:Integer; end;
  TRGBAw               = packed record r,b,g,a:Word; end;

  TRGBs                = record r,b,g:Single; end;
  TRGBsVector          = array[0..(MaxInt div SizeOf(TRGBs))-1] of TRGBs;
  PRGBsVector          = ^TRGBsVector;
  TRGBShift            = (rgbShiftNone,rgbShiftRG,rgbShiftRB,rgbShiftGB);

const
  COLOR_BLACK          : TColor = TColor(0);
  IMAGE_HEIGHT_FULL_HD = 1080;
  IMAGE_HEIGHT_4K      = 2 * IMAGE_HEIGHT_FULL_HD;
  IMAGE_WIDTH_FULL_HD  = 1920;
  IMAGE_WIDTH_4K       = 2 * IMAGE_WIDTH_FULL_HD;
  MAX_IMAGE_HEIGHT     = 2 * IMAGE_HEIGHT_4K;
  MAX_IMAGE_WIDTH      = 2 * IMAGE_WIDTH_4K;
  PIXEL_BYTE_SIZE      : array[TPixelFormat] of Byte =
                          //(pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pfCustom);
                         (0,0,0,1,0,2,3,4,0); // only non-zero values for pixelformats supported by 'GetBitMapText' and 'SetBitMapText'
  RGB_BLACK            : TRGB   = (r:  0; g:  0; b:  0);
//RGB_LIGHT_GRAY       : TRGB   = (r:192; g:192; b:192);
  RGB_MASK             = $00ffffff;
  RGB_WHITE            : TRGB   = (r:255; g:255; b:255);

  function  ColorToRGB(Color:TColor):TRGB;
  function  RGBToColor(const RGB:TRGB):TColor;
  function  __RGB(r__,g__,b__:Byte):TRGB;

  procedure AlphaBlend(Dest,Source,Mask:TBitMap; const DestRect,SourceRect:TRect; AlphaBlendPct__,EdgeAlphaBlendPct__:Integer; MaskNonBlackPixels__:Boolean);
  procedure BitBltTransparent(Dest:TBitMap; X,Y:Integer; Source:TBitMap; Rect:TRect;
                              TransparentColor:TColor; TransparentColorEqualityPct:Integer;
                              OldColor1,NewColor1,OldColor2,NewColor2:TColor);
  procedure BitMapAlphaBlend(DestBitMap,StartBitMap,EndBitMap:TBitMap;
                             Pct:Integer; var Cancel:Boolean);
  procedure BitMapAlphaBlendColor(BitMap:TBitMap; Color:TColor; Pct:Integer; const Rect:TRect);
  procedure BitMapAlphaBlendRect(DestBitMap,StartBitMap,EndBitMap:TBitMap;
                                 const DestRect,StartRect,EndRect:TRect;
                                 Pct:Integer;
                                 Masked:Boolean; MaskColor:TColor; MaskPct:Integer);
  function  BitMapApplyMask(BitMap,MaskBitMap:TBitMap):Boolean;
  procedure BitMapBilinearAntiAliasing(BitMap:TBitMap; Masked:Boolean; MaskColor:TColor; MaskPct:Integer);
  function  BitMapClampSize(Width,Height:Integer; var W,H:Integer):Boolean; // without clamping, large bitmaps crashes the application during assignment to 'Width' and 'Height'
  procedure BitMapColorChange( BitMap__: TBitMap; Hue__,Saturation__, Value__ : Single );
  function  BitMapCompare(BitMap1__,BitMap2__:TBitMap; HighlightDifferences__:Boolean; MarkColor__:TColor):Integer;
  function  BitMap32Create(var BitMap:TBitMap; Width,Height:Integer):Boolean;
  function  BitMapCreate(var BitMap:TBitMap; Width,Height:Integer):Boolean;
  function  BitMapCreateMask(var {i/o: }BitMap,{out: }MaskBitMap:TBitMap; MaskColor:TRGB; MaskPct:Integer):Boolean;
  function  BitMapCreateMaskMaskingExteriorOnly(var {i/o: }BitMap,{out: }MaskBitMap:TBitMap; MaskColor:TRGB; MaskPct:Integer):Boolean;
  function  BitMapDump(BitMap:TBitMap):String;
  function  BitMapEqual( BitMap1__, BitMap2__ : TBitMap; const Rect1__, Rect2__ : TRect; EqualityThresholdPct__ : Integer ) : Boolean;
  function  BitMapExtractRect(var BitMap:TBitMap; const Rect:TRect):Boolean;
  function  BitMapFillWithTile(DestBitMap,SourceBitMap:TBitMap; SourceRect:TRect; XOffset,YOffset:Integer):Boolean;
  function  BitMapFloodFillMaskedPixels(DestBitMap,SourceBitMap:TBitMap; MaskColor:TRGB; MaskPct:Integer):Boolean;
  procedure BitMapGrayImage(BitMap:TBitMap);
  function  BitMapIsColor(BitMap:TBitMap; const Rect:TRect; Color:TRGB; MaskBitMapPct:Integer):Boolean;
  function  BitMapMakeTransparent(BitMap:TBitMap; MaskColor:TRGB; MaskPct:Integer):Boolean;
  function  BitMapMaskBackground(var {i:} BitMap, {io:} MaskBitMap, {i:} BackgroundBitMap:TBitMap; MaskPct:Integer; Masked:Boolean):Integer;
  function  BitMapMaskBackgroundForAllPixels(var {io:} BitMap, {io:} MaskBitMap, {i:} BackgroundBitMap:TBitMap; MaskPct:Integer; Masked:Boolean):Integer;
  function  BitMapMaskBackgroundForAllPixels_HueAndSaturationOnly(var {io:} BitMap, {io:} MaskBitMap, {i:} BackgroundBitMap:TBitMap; MaskPct:Integer; Masked:Boolean):Integer;
  function  BitMapPitch(BitMap:TBitMap):Integer;
  function  BitMapResize(var BitMap:TBitMap; Width,Height:Integer):Boolean;
  procedure BitMapRGBShift(var BitMap:TBitMap; RGBShift:TRGBShift);
  function  BitMapRotate(Dest__,Source__:TBitMap; CounterClockwiseAngleDegrees__:Integer; FillColor__:TColor; BicubicInterpolation__:Boolean):Boolean;
  procedure BitMapRotatedImageSize(Width,Height,AngleDegrees:Integer; var RotatedWidth,RotatedHeight:Integer);
  function  BitMapScale(Dest,Source:TBitMap; FilterDistance:Integer;
                        LowPriority,Masked:Boolean; MaskColor:TColor; MaskPct:Integer;
                        View:TImageView;
                        HorizontalBorder,VerticalBorder:Integer;
                        var ScaledRect:TRect;
                        var Cancel:Boolean):Boolean;
  procedure BitMapSwap(var BitMap1,BitMap2:TBitMap);
  function  BitMap24BitPixelFormat(OriginalBitMap:TBitMap; var BitMap24Bit:TBitMap):Boolean;
  procedure DrawArrow( Canvas__                    : TCanvas;
                       X__, Y__, ArrowPixelSize__  : Integer;
                       ArrowDirection__            : TDirection;
                       ArrowColor__, ShadowColor__ : TColor );
  procedure EdgeSmoothing(const Dest,Source,Mask:TBitMap; const Rect:TRect; Horizontal,Vertical:Boolean);
  function  GetAlphaChannel( Color__ : TColor ) : Integer;
  function  GetBitMapText(BitMap__:TBitMap; const Rect__:TRect; Clear__:Boolean; Strings__:TStrings; var ErrorText__:String):Boolean;
  function  GridCellToRect(ACol,ARow,ColWidth,RowHeight,GridLineWidth:Integer):TRect;
  function  HSVToRGB( Hue__, Saturation__, Value__ : Single ) : TRGB;
  function  IdenticalRects(const R1,R2:TRect):Boolean;
  function  InvertedColor( Color : TColor ) : TColor;
  function  MakeStaticWorkBitMap(Width,Height:Integer; HandleException:Boolean):Boolean;
  function  MaxBitMapTextLength( BitMap__:TBitMap; const Rect__:TRect ) : Integer;
  function  NotBlackColor(Color__:TColor):TColor;
  procedure PointToCell(X,Y,ColWidth,RowHeight,GridLineWidth:Integer; var ACol,ARow:Integer);
  procedure RGBColorShift(RGBShift:TRGBShift; var RGB:TRGB);
  function  RGBInterpolate(Color1,Color2:TRGB; Percent:Integer):TRGB;
  function  RGBToBGR(const RGB:TRGB):TRGB;
  procedure RGBToHSV( RGB__ : TRGB; var HSV__ : THSVColor );
  function  RGBComponentsToColor( r__, g__, b__ : Byte ) : TColor;
  function  RGBComponentsToRGB( r__, g__, b__ : Byte ) : TRGB;
//function  SaveBMPAsWMFFile(BitMap__:TBitMap; const FileName__:String):Boolean;
  function  SetAlphaChannel( Alpha__ : Integer; Color__ : TColor ) : TColor;
  function  SetBitMapText(BitMap__:TBitMap; const Rect__:TRect; const Text__:String; var ErrorText__:String):Boolean;

{
type
  TAlphaBlendTable             = array[-255..256] of Integer; // ..256: Size = 512 bytes
  PAlphaBlendTable             = ^TAlphaBlendTable;
}
var
{
  AlphaBlendTables             : array[0..100] of TAlphaBlendTable;
}
  // static workspace, to improve speed and to avoid memory trashing
  StaticWorkBitMap             : TBitMap = nil;

implementation

uses SysUtils,Forms,Controls,Math,Text_,Mandal2_,Main_;

var
  // static workspace, to improve speed and to avoid memory trashing
//StaticWorkBitMap             : TBitMap = nil; // global, see above
  StaticScaleColFilter         : Pointer = nil;
  StaticScaleColFilterCapacity : Integer = 0;
  StaticScaleColFilterHeight   : Integer = 0;   // the cached calculated filter
  StaticScaleColFilterScale    : Single  = 0.0; // the cached calculated filter
  StaticScaleFilterDistance    : Integer = 0;   // the cached calculated filter
  StaticScaleRowFilter         : Pointer = nil;
  StaticScaleRowFilterCapacity : Integer = 0;
  StaticScaleRowFilterScale    : Single  = 0.0; // the cached calculated filter
  StaticScaleRowFilterWidth    : Integer = 0;   // the cached calculated filter
  StaticWorkSpace              : Pointer = nil;
{
procedure MakeAlphaBlendTable(NewPct:Integer; var Pct:Integer; var Table:TAlphaBlendTable);
var i,Alpha:Integer;
begin
  Pct:=NewPct; Alpha:=Pct*128 div 100;
  for i:=Low(Table) to High(Table) do Table[i]:=i*Alpha div 128;
end;
}
function  ColorToRGB(Color:TColor):TRGB;
begin
  Result.b:= Color         and 255;
  Result.g:=(Color shr  8) and 255;
  Result.r:=(Color shr 16) and 255;
end;

function  RGBInterpolate(Color1,Color2:TRGB; Percent:Integer):TRGB;
begin // simple and naive interpolation in RGB space between the two colors
  Result.r:=Color1.r+(((Color2.r-Color1.r)*Percent) div 100);
  Result.g:=Color1.g+(((Color2.g-Color1.g)*Percent) div 100);
  Result.b:=Color1.b+(((Color2.b-Color1.b)*Percent) div 100);
end;

function  RGBToBGR(const RGB:TRGB):TRGB;
begin
  with Result do begin r:=RGB.b; g:=RGB.g; b:=RGB.r; end;
end;

procedure RGBToHSV( RGB__ : TRGB; var HSV__ : THSVColor );
var Min, Max, Delta : Integer;
begin
  with RGB__ do with HSV__ do begin
    Min   := r;
    if g  <  Min then Min := g;
    if b  <  Min then Min := b;
    Max   := r;
    if g  >  Max then Max := g;
    if b  >  Max then Max := b;
    Delta := Max - Min;
    if   Max     =  0 then
         S := 0.0
    else S := Delta / Max;
    if   S =  0.0 then
         H := 0.0
    else begin
         if        r = Max then begin H       := ( ( g - b ) / Delta );
                                      while H >= 6.0 do H := H - 6.0;
                                end
         else if   g = Max then H := ( ( b - r ) / Delta ) + 2.0
              else              H := ( ( r - g ) / Delta ) + 4.0;
         H := H * 60.0;
         end;
    V := Max / 255;
    end;
end;

function  RGBComponentsToColor( r__, g__, b__ : Byte ) : TColor;
begin
  Result := b__ +( g__ shl 8 ) + ( r__ shl 16 );
end;

function  RGBComponentsToRGB( r__, g__, b__ : Byte ) : TRGB;
begin
  with Result do begin r := r__; g := g__; b := b__; end;
end;

function  RGBToColor(const RGB:TRGB):TColor;
begin
  Result:=RGB.b+(RGB.g shl 8)+(RGB.r shl 16);
end;

function  __RGB(r__,g__,b__:Byte):TRGB;
begin
  with Result do begin r:=r__; g:=g__; b:=b__; end;
end;

function  SafeBitMapLoadFromFile(const FileName:String):TBitMap;
begin
  Result:=nil;
  if FileExists(FileName) then
     try    Result:=TBitMap.Create;
            Result.LoadFromFile(FileName);
     except
     on E:Exception do
        begin Result.Free; Result:=nil;
        end;
     end;
end;

procedure AlphaBlend(Dest,Source,Mask:TBitMap; const DestRect,SourceRect:TRect; AlphaBlendPct__,EdgeAlphaBlendPct__:Integer; MaskNonBlackPixels__:Boolean);
var i,j,H,H1,W,W1,Edge:Integer; c:TRGB; m,p,q:PRGBVector; Ok:Boolean; //B:TBitMap;
    AlphaBlendTable,EdgeAlphaBlendTable:PAlphaBlendTable;
begin
  //exit;
  m:=nil;
  W :=Min(RectWidth (  DestRect),Dest  .Width -DestRect  .Left);
  H :=Min(RectHeight(  DestRect),Dest  .Height-DestRect  .Top );
  W1:=Min(RectWidth (SourceRect),Source.Width -SourceRect.Left);
  H1:=Min(RectHeight(SourceRect),Source.Height-SourceRect.Top );
  if (W=W1) and (H=H1) and
     ((Mask=nil) or
      ((Mask.Width=W) and (Mask.Height=H))) then begin

     //EdgeAlphaBlendPct__:=0;
 {
     //if Dest=MainForm.Status.BitMap then begin
     //Dest.SaveToFile('t1.bmp');
     B:=TBitMap.Create;
     B.Width:=W; B.Height:=H;
     B.Canvas.CopyRect(Classes.Rect(0,0,W,H),Source.Canvas,Rect);
     //B.SaveToFile('t2.bmp');
     //if Mask<>nil then Mask.SaveToFile('t3.bmp');
     B.Free;
     //end;
}

     AlphaBlendTable    :=Addr(AlphaBlendTables[AlphaBlendPct__    ]);
     EdgeAlphaBlendTable:=Addr(AlphaBlendTables[EdgeAlphaBlendPct__]);

     W :=Pred(W); // caution:  'W' and 'H' are not 'Width' and 'Height' anymore
     H :=Pred(H);
     H1:=Pred(H);

//     Dest.Canvas.CopyMode:=cmSrcCopy;
//     Dest.Canvas.CopyRect(Classes.Rect(0,0,W,H),Source.Canvas,Classes.Rect(Rect.Left,Rect.Top,Rect.Left+W,Rect.Top+H));
//     exit;

     for i:=0 to H do begin
         p:=Dest  .ScanLine[  DestRect.Top+i];
         q:=Source.ScanLine[SourceRect.Top+i];
         if Mask<>nil then m:=Mask.ScanLine[i];
         Edge:=0;

         for j:=0 to W do begin
             Ok:=True;
             if  Mask<>nil then begin
                 c:=m[j];
                 if not MaskNonBlackPixels__ then begin
                    if (c.r=RGB_WHITE.r) and
                       (c.g=RGB_WHITE.g) and
                       (c.b=RGB_WHITE.b) then begin
                       if (Edge=Pred(j)) and
                          (not ((i<=1) or (i>=H1))) then with p[DestRect.Left+Edge] do begin
                          c:=q[SourceRect.Left+Edge];
                          Inc(r,EdgeAlphaBlendTable[c.r-r]);
                          Inc(g,EdgeAlphaBlendTable[c.g-g]);
                          Inc(b,EdgeAlphaBlendTable[c.b-b]);
                          //r:=0; g:=0; b:=255;
                          end;
                       Ok:=False;
                       Edge:=-Succ(j);
                       end;
                    end
                 else begin
                    if (c.r<>RGB_BLACK.r) or
                       (c.g<>RGB_BLACK.g) or
                       (c.b<>RGB_BLACK.b) then begin
                       if (Edge=Pred(j)) and
                          (not ((i<=1) or (i>=H1))) then with p[DestRect.Left+Edge] do begin
                          c:=q[SourceRect.Left+Edge];
                          Inc(r,EdgeAlphaBlendTable[c.r-r]);
                          Inc(g,EdgeAlphaBlendTable[c.g-g]);
                          Inc(b,EdgeAlphaBlendTable[c.b-b]);
                          //r:=0; g:=0; b:=255;
                          end;
                       Ok:=False;
                       Edge:=-Succ(j);
                       end;
                    end;
                 end;

             if Ok then with p[DestRect.Left+j] do begin
                c:=q[SourceRect.Left+j];
                Inc(r,AlphaBlendTable[c.r-r]);
                Inc(g,AlphaBlendTable[c.g-g]);
                Inc(b,AlphaBlendTable[c.b-b]);
                if (i<=1) or (i>=H1) or (j=-Edge) then begin // caution: <=1 or >=H1 : this is not a general function, but specialized for buttons with a blank top-row and a blank bottom-row
                   Inc(r,EdgeAlphaBlendTable[c.r-r]);
                   Inc(g,EdgeAlphaBlendTable[c.g-g]);
                   Inc(b,EdgeAlphaBlendTable[c.b-b]);
                   //r:=0; g:=0; b:=255;
                   end;
                Edge:=j;
                //p[j]:=c;
                end
             else p[DestRect.Left+j]:=q[SourceRect.Left+j]; // quick and dirty: coorporate with caller by filling the image, so the mask is not required anymore
             end;

         if Edge=W then with p[DestRect.Left+Edge] do begin
            c:=q[SourceRect.Left+Edge];
            Inc(r,EdgeAlphaBlendTable[c.r-r]);
            Inc(g,EdgeAlphaBlendTable[c.g-g]);
            Inc(b,EdgeAlphaBlendTable[c.b-b]);
            //r:=0; g:=255; b:=255;
            end;
         end;
{
     //if Dest=MainForm.Status.BitMap then begin
     B:=TBitMap.Create;
     B.Width:=Dest.Width; B.Height:=Dest.Height;
     B.Canvas.CopyRect(Classes.Rect(0,0,B.Width,B.Height),Dest.Canvas,Classes.Rect(0,0,B.Width,B.Height));
     //B.SaveToFile('t4.bmp');
     B.Free;
     //end;
}
     end;
end;

procedure BitBltTransparent(Dest:TBitMap; X,Y:Integer; Source:TBitMap; Rect:TRect;
                            TransparentColor: TColor;
                            TransparentColorEqualityPct : Integer;
                            OldColor1,NewColor1,OldColor2,NewColor2:TColor);
var i,j,H,W,TransparentColorLimit:Integer; p,q:PRGBVector;
    RGBColor,RGBTransparentColor,
    RGBOldColor1,RGBNewColor1,RGBOldColor2,RGBNewColor2:TRGB;
begin
 RGBTransparentColor  :=ColorToRGB(TransparentColor);
 TransparentColorLimit:=((3*255)*TransparentColorEqualityPct) div 100;
 RGBOldColor1         :=ColorToRGB(OldColor1);
 RGBNewColor1         :=ColorToRGB(NewColor1);
 RGBOldColor2         :=ColorToRGB(OldColor2);
 RGBNewColor2         :=ColorToRGB(NewColor2);
 W                    :=Min(RectWidth(Rect) ,Dest.Width -X);
 H                    :=Min(RectHeight(Rect),Dest.Height-Y);
 for i:=0 to Pred(H) do
     if Y+i>=0 then begin
        p:=Dest  .ScanLine[Y+i];
        q:=Source.ScanLine[Rect.Top+i];
        for j:=0 to Pred(W) do
            if X+j>=0 then begin
               RGBColor:=q[Rect.Left+j];
               if      ( (TransparentColorEqualityPct=0)    and
                         (RGBColor.r=RGBTransparentColor.r) and
                         (RGBColor.g=RGBTransparentColor.g) and
                         (RGBColor.b=RGBTransparentColor.b) )
                       or
                       ( (TransparentColorEqualityPct>0)    and
                         ( ( Abs( RGBColor.r - RGBTransparentColor.r ) +
                             Abs( RGBColor.g - RGBTransparentColor.g ) +
                             Abs( RGBColor.b - RGBTransparentColor.b )
                           )
                           <= TransparentColorLimit ) ) then // do nothing
               else if (RGBColor.r=RGBOldColor1.r) and
                       (RGBColor.g=RGBOldColor1.g) and
                       (RGBColor.b=RGBOldColor1.b) then
                       if   TransparentColor<>NewColor1 then
                            p[X+j]:=RGBNewColor1
                       else
               else if (RGBColor.r=RGBOldColor2.r) and
                       (RGBColor.g=RGBOldColor2.g) and
                       (RGBColor.b=RGBOldColor2.b) then
                       if   TransparentColor<>NewColor2 then
                            p[X+j]:=RGBNewColor2
                       else
               else p[X+j]:=RGBColor;
               end;
        end;
end;
(*
procedure BitMapBilinearAntiAliasing(BitMap:TBitMap);
var i,j,k,W,H,LineWidth:Integer; p:PByte;
begin
  W:=Bitmap.Width*3; H:=BitMap.Height;
  if (W>6) and (H>3) and (BitMap.PixelFormat=pf24Bit) then with BitMap do begin
     LineWidth:=Integer(ScanLine[1])- Integer(ScanLine[0]);
{
     p:=PByte(Integer(ScanLine[0])+3);
     for j:=3 to W-4 do begin // top row
             k:=(p^ + ((PByte(Integer(p)-3)^           +
                        PByte(Integer(p)+3)^           +
                        PByte(Integer(p)+LineWidth)^   +
                        PByte(Integer(p)+LineWidth-3)^ +
                        PByte(Integer(p)+LineWidth+3)^
                       ) div 5
                      )

                ) shr 1;
             if k>=255 then p^:=255
             else p^:=k;
             Inc(Integer(p));
             end;

    for i:=1 to H-2 do begin // left column
         p:=PByte(Integer(ScanLine[i]));
         for j:=1 to 3 do begin
             k:=(p^ + ((PByte(Integer(p)+3)^           +
                        PByte(Integer(p)-LineWidth)^   +
                        PByte(Integer(p)+LineWidth)^   +
                        PByte(Integer(p)-LineWidth+3)^ +
                        PByte(Integer(p)+LineWidth+3)^
                       ) div 5
                      )

                ) shr 1;
             if k>=255 then p^:=255
             else p^:=k;
             Inc(Integer(p));
             end;
         end;
}
     for i:=1 to H-2 do begin // internal pixels
         p:=PByte(Integer(ScanLine[i])+3);
         for j:=3 to W-4 do begin
             k:=(p^ + ((PByte(Integer(p)-3)^           +
                        PByte(Integer(p)+3)^           +
                        PByte(Integer(p)-LineWidth)^   +
                        PByte(Integer(p)+LineWidth)^   +
                        PByte(Integer(p)-LineWidth-3)^ +
                        PByte(Integer(p)-LineWidth+3)^ +
                        PByte(Integer(p)+LineWidth-3)^ +
                        PByte(Integer(p)+LineWidth+3)^
                       ) shr 3
                      )

                ) shr 1;
             if k>=255 then p^:=255
             else p^:=k;
             Inc(Integer(p));
             end;
         end;
{
     for i:=1 to H-2 do begin // right column
         p:=PByte(Integer(ScanLine[i])+W-3);
         for j:=1 to 3 do begin
             k:=(p^ + ((PByte(Integer(p)-3)^           +
                        PByte(Integer(p)-LineWidth)^   +
                        PByte(Integer(p)+LineWidth)^   +
                        PByte(Integer(p)-LineWidth-3)^ +
                        PByte(Integer(p)+LineWidth-3)^
                       ) div 5
                      )

                ) shr 1;
             if k>=255 then p^:=255
             else p^:=k;
             Inc(Integer(p));
             end;
         end;

     p:=PByte(Integer(ScanLine[Pred(H)])+3);
     for j:=3 to W-4 do begin // bottom row
             k:=(p^ + ((PByte(Integer(p)-3)^           +
                        PByte(Integer(p)+3)^           +
                        PByte(Integer(p)-LineWidth)^   +
                        PByte(Integer(p)-LineWidth-3)^ +
                        PByte(Integer(p)-LineWidth+3)^
                       ) div 5
                      )

                ) shr 1;
             if k>=255 then p^:=255
             else p^:=k;
             Inc(Integer(p));
             end;
}
    end;
end;
*)

procedure BitMapAlphaBlend(DestBitMap,StartBitMap,EndBitMap:TBitMap; Pct:Integer; var Cancel:Boolean);
var X,Y:Integer; AByte:Byte; AlphaBlendTable:PAlphaBlendTable; p,q,r:PRGB;
begin // preconditions: bitmaps have the same dimensions, and pixelformat 24-bit rgb
  AlphaBlendTable:=Addr(AlphaBlendTables[Pct]); Cancel:=False;
  with DestBitMap do
    for Y:=0 to Pred(Height) do begin
        if Y mod 16 = 0 then SleepEx(0,False); // give the other threads a chance
        if not Cancel then begin // 'Cancel' may have been set by another thread
           p:=ScanLine[Y];
           q:=StartBitMap.ScanLine[Y];
           r:=EndBitMap.ScanLine[Y];
           for X:=0 to Pred(Width) do begin
               AByte:=q.r; p.r:=AByte+AlphaBlendTable[r.r-AByte];
               AByte:=q.g; p.g:=AByte+AlphaBlendTable[r.g-AByte];
               AByte:=q.b; p.b:=AByte+AlphaBlendTable[r.b-AByte];
               Inc(p); Inc(q); Inc(r);
               end;
           end;
        end;
end;

procedure BitMapAlphaBlendColor(BitMap:TBitMap; Color:TColor; Pct:Integer; const Rect:TRect);
var X,Y:Integer; AByte:Byte; RGBColor:TRGB; AlphaBlendTable:PAlphaBlendTable; p:PRGB;
begin // precondition: 'BitMap' has pixelformat 24-bit rgb
  RGBColor:=ColorToRGB(Color);
  AlphaBlendTable:=Addr(AlphaBlendTables[Pct]);
  with BitMap do
    for Y:=Rect.Top to Pred(Rect.Bottom) do begin
        p:=ScanLine[Y]; Inc(p,Rect.Left);
        for X:=Rect.Left to Pred(Rect.Right) do begin
            AByte:=p.r; p.r:=AByte+AlphaBlendTable[RGBColor.r-AByte];
            AByte:=p.g; p.g:=AByte+AlphaBlendTable[RGBColor.g-AByte];
            AByte:=p.b; p.b:=AByte+AlphaBlendTable[RGBColor.b-AByte];
            Inc(p);
            end;
        end;
end;

procedure BitMapAlphaBlendRect(DestBitMap,StartBitMap,EndBitMap:TBitMap;
                               const DestRect,StartRect,EndRect:TRect;
                               Pct:Integer;
                               Masked:Boolean; MaskColor:TColor; MaskPct:Integer);
var X,Y,W,H,MaskLimit:Integer; AByte:Byte; RGBMask:TRGB;
    AlphaBlendTable:PAlphaBlendTable; p,q,r:PRGB;
begin // bitmaps must have pixelformat 24-bit rgb and the same size, and the rectangles must be valid sections of the bitmaps
  AlphaBlendTable:=Addr(AlphaBlendTables[Pct]);
  RGBMask:=ColorToRGB(MaskColor);
  MaskLimit:=((3*255)*MaskPct) div 100;
  W:=Pred(RectWidth(DestRect)); H:=Pred(RectHeight(DestRect)); // 'Pred': 'W' and 'H' are *not* width and height
  with DestBitMap do
    for Y:=0 to H do begin
        p:=PRGB(Cardinal(            ScanLine[DestRect .Top+Y])+Cardinal(DestRect .Left)*SizeOf(p^));
        q:=PRGB(Cardinal(StartBitMap.ScanLine[StartRect.Top+Y])+Cardinal(StartRect.Left)*SizeOf(p^));
        r:=PRGB(Cardinal(EndBitMap  .ScanLine[EndRect  .Top+Y])+Cardinal(EndRect  .Left)*SizeOf(p^));
        for X:=0 to W do begin

            if           not Masked then begin
                         AByte:=q.r; p.r:=AByte+AlphaBlendTable[r.r-AByte];
                         AByte:=q.g; p.g:=AByte+AlphaBlendTable[r.g-AByte];
                         AByte:=q.b; p.b:=AByte+AlphaBlendTable[r.b-AByte];
                         end
            else if      (Abs(q.r-RGBMask.r)+
                          Abs(q.g-RGBMask.g)+
                          Abs(q.b-RGBMask.b)) <= MaskLimit
                         then begin
                         if (Abs(r.r-RGBMask.r)+
                             Abs(r.g-RGBMask.g)+
                             Abs(r.b-RGBMask.b)) <= MaskLimit
                            then begin
                            p^:=RGBMask;
                            end
                         else begin
                            p^:=r^;
                            end;
                         end
                 else if (Abs(r.r-RGBMask.r)+
                          Abs(r.g-RGBMask.g)+
                          Abs(r.b-RGBMask.b)) <= MaskLimit
                         then begin
                         p^:=q^;
                         end
                      else begin
                         AByte:=q.r; p.r:=AByte+AlphaBlendTable[r.r-AByte];
                         AByte:=q.g; p.g:=AByte+AlphaBlendTable[r.g-AByte];
                         AByte:=q.b; p.b:=AByte+AlphaBlendTable[r.b-AByte];
                         end;

            Inc(p); Inc(q); Inc(r);
            end;
        end;
end;

function BitMapApplyMask(BitMap,MaskBitMap:TBitMap):Boolean;
var R:TRect;
begin
  Result:=(BitMap<>nil) and (MaskBitMap<>nil);
  if Result then begin
     R:=Rect(0,0,BitMap.Width,BitMap.Height);
     MaskBitMap.Canvas.CopyMode:=cmDstInvert;
     MaskBitMap.Canvas.CopyRect(R,MaskBitMap.Canvas,R); // invert the mask

     BitMap.Canvas.CopyMode:=cmSrcAnd;
     BitMap.Canvas.CopyRect(R,MaskBitMap.Canvas,R);     // apply it
     BitMap.Canvas.CopyMode:=cmSrcCopy;

     MaskBitMap.Canvas.CopyMode:=cmDstInvert;
     MaskBitMap.Canvas.CopyRect(R,MaskBitMap.Canvas,R); // invert the mask again, i.e., back to normal state
     MaskBitMap.Canvas.CopyMode:=cmSrcCopy;
     end;
end;

procedure BitMapBilinearAntiAliasing(BitMap:TBitMap; Masked:Boolean; MaskColor:TColor; MaskPct:Integer);
const MAX_WIDTH=320; MAX_HEIGHT=256;
type  TRGBAwVec=array[-1..MAX_HEIGHT+1,-1..MAX_WIDTH+1] of TRGBAw; // there is no runtime-penalty for non-zero-based array access; hence, this is the easiest way to avoid bounds-checks in the code
var   X0,X1,Y0,Y1,W,H,X,Y,MaskLimit:Integer; p:PRGB; RGBMask,Color:TRGB; V:^TRGBAwVec;
begin
  W:=Bitmap.Width; H:=BitMap.Height;
  if (W<=MaxInt-MAX_WIDTH) and (H<=MaxInt-MAX_HEIGHT) and
     (BitMap.PixelFormat=pf24Bit) then
     try
       if   StaticWorkSpace<>nil then V:=StaticWorkSpace
       else begin GetMem(V,SizeOf(V^)); StaticWorkSpace:=V; end;
       try
         RGBMask:=ColorToRGB(MaskColor);
         MaskLimit:=((3*255)*MaskPct) div 100;
         X0:=0; Y0:=0;
         repeat
           Y1:=Pred(Min(Y0+MAX_HEIGHT,H));
           repeat
             X1:=Pred(Min(X0+MAX_WIDTH,W)); // work on rectangle(X0,Y0,X1+1,Y1+1)

             FillChar(V^,SizeOf(V^),0);

             for Y:=0 to Y1-Y0 do begin
                 p:=PRGB(Cardinal(BitMap.ScanLine[Y0+Y])+Cardinal(X0*SizeOf(TRGB)));

                 for X:=0 to X1-X0 do begin
                     Color:=p^;

                     if Masked and
                        ((Abs(Color.r-RGBMask.r)+
                          Abs(Color.g-RGBMask.g)+
                          Abs(Color.b-RGBMask.b)) <= MaskLimit) then
                        with V[Y,X] do a:=Low(a) // respect masked pixels
                     else begin
                                          with V[Y  ,X  ] do begin Inc(r,12*Color.r); Inc(g,12*Color.g); Inc(b,12*Color.b); Inc(a,12); end;
                        {if X>0  then}    with V[Y  ,X-1] do begin Inc(r, 2*Color.r); Inc(g, 2*Color.g); Inc(b, 2*Color.b); Inc(a, 2); end;
                        {if X<X1 then}    with V[Y  ,X+1] do begin Inc(r, 2*Color.r); Inc(g, 2*Color.g); Inc(b, 2*Color.b); Inc(a, 2); end;
                        {if Y>0  then}    begin
                                          with V[Y-1,X  ] do begin Inc(r, 2*Color.r); Inc(g, 2*Color.g); Inc(b, 2*Color.b); Inc(a, 2); end;
                           {if X>0 then}  with V[Y-1,X-1] do begin Inc(r, 1*Color.r); Inc(g, 1*Color.g); Inc(b, 1*Color.b); Inc(a, 1); end;
                           {if X<X1 then} with V[Y-1,X+1] do begin Inc(r, 1*Color.r); Inc(g, 1*Color.g); Inc(b, 1*Color.b); Inc(a, 1); end;
                           end;
                        {if Y<Y1    then} begin
                                          with V[Y+1,X  ] do begin Inc(r, 2*Color.r); Inc(g, 2*Color.g); Inc(b, 2*Color.b); Inc(a, 2); end;
                           {if X>0  then} with V[Y+1,X-1] do begin Inc(r, 1*Color.r); Inc(g, 1*Color.g); Inc(b, 1*Color.b); Inc(a, 1); end;
                           {if X<X1 then} with V[Y+1,X+1] do begin Inc(r, 1*Color.r); Inc(g, 1*Color.g); Inc(b, 1*Color.b); Inc(a, 1); end;
                           end;
                        end;
                     Inc(Cardinal(p),Cardinal(SizeOf(TRGB)));
                     end;
                 end;

             for Y:=0 to Y1-Y0 do begin
                 p:=PRGB(Cardinal(BitMap.ScanLine[Y0+Y])+Cardinal(X0*SizeOf(TRGB)));
                 for X:=0 to X1-X0 do with V[Y,X] do begin
                     if a<=12 then // masked pixel
                     else begin r:=r div a; g:=g div a; b:=b div a;
                                if r>=255 then p.r:=255 else p.r:=r;
                                if g>=255 then p.g:=255 else p.g:=g;
                                if b>=255 then p.b:=255 else p.b:=b;
                          end;
                     Inc(Cardinal(p),Cardinal(SizeOf(p^)));
                     end;
                 end;

             Inc(X0,MAX_WIDTH);
           until X0>=W;
           X0:=0; Inc(Y0,MAX_HEIGHT);
         until Y0>=H;

       finally //FreeMem(V); //StaticWorkSpace:=nil;
       end;
     except on E:Exception do Error(E.Message,Application.Title+' - BitMap_.BitmapBilinearAntialiasing');
     end;
end;

function  BitMapClampSize(Width,Height:Integer; var W,H:Integer):Boolean;
const MAX_PIXELS=(MAX_IMAGE_WIDTH+2)*(MAX_IMAGE_HEIGHT+2); // "+2": the skin capture tool uses 2 extra columns and rows
begin // kludge: large bitmaps crashes the program during assignment to 'Width' and 'Height'
  Result:=False; W:=Width; H:=Height;
  while (W>MAX_PIXELS) or (H>MAX_PIXELS) or (W*H>MAX_PIXELS) do begin
    W:=W div 2; H:=H div 2;
    Result:=True; // returns 'True' if the size is reduced
    end;
  if W<1 then W:=1;
  if H<1 then H:=1;
end;

procedure BitMapColorChange( BitMap__: TBitMap; Hue__,Saturation__, Value__ : Single );
var X, Y, SaturationSign, ValueSign : Integer; HSV : THSVColor; p:PRGB;
begin
  with BitMap__ do with HSV do begin
    if        Saturation__   =  0.0 then
              SaturationSign := 0
    else if   Saturation__   >  0.0 then
              SaturationSign := 1
         else SaturationSign := -1;
    if        Value__        =  0.0 then
              ValueSign      := 0
    else if   Value__        >  0.0 then
              ValueSign      := 1
         else ValueSign      := -1;

    for Y:=0 to Pred(Height) do begin
        p:=ScanLine[Y];
        for X:=0 to Pred(Width) do begin
            RGBToHSV( p^, HSV );
            H := H + Hue__;
            if SaturationSign <> 0 then begin
               S := S + Saturation__;
               if      SaturationSign > 0 then
                       if S > 1.0 then S := 1.0
                       else
               else if S < 0.0 then S := 0.0;
               end;
            if ValueSign <> 0 then begin
               V := V + Value__;
               if      ValueSign > 0 then
                       if V > 1.0 then V := 1.0
                       else
               else if V < 0.0 then V := 0.0;
               end;
            p^ := HSVToRGB( H, S, V );
            Inc(p);
            end;
        end;
    end;
end;

function  BitMapCompare(BitMap1__,BitMap2__:TBitMap; HighlightDifferences__:Boolean; MarkColor__:TColor):Integer;
var Col,Row:Integer;
begin
  if BitMap1__=BitMap2__ then Result:=0
  else if (BitMap1__=nil) or (BitMap2__=nil) or
          (BitMap1__.Width<>BitMap2__.Width) or
          (BitMap1__.Height<>BitMap2__.Height) then Result:=-1
       else with BitMap1__ do with Canvas do begin
              Result:=0;
              for Row:=0 to Pred(Height) do
                  for Col:=0 to Pred(Width) do
                      if Pixels[Col,Row]<>BitMap2__.Canvas.Pixels[Col,Row] then begin
                         Inc(Result);
                         if HighlightDifferences__ then Pixels[Col,Row]:=MarkColor__;
                         end;
              end;
end;

function  CreateBitmap( Width, Height : Integer; PixelFormat : TPixelFormat; var Bitmap : TBitmap) : Boolean;
begin
  BitMap                       :=nil;
  try BitMap                   :=TBitMap.Create;
      BitMapClampSize(Width,Height,Width,Height);
      BitMap.Width             :=Width;
      BitMap.Height            :=Height;
      BitMap.PixelFormat       :=PixelFormat;
      BitMap.Canvas.Brush.Style:=bsSolid;
      BitMap.Canvas.Pen  .Style:=psSolid;
      BitMap.Canvas.Pen  .Width:=1;
      BitMap.Canvas.CopyMode   :=cmSrcCopy;
      if        BitMap.Pixelformat<>PixelFormat then
                raise Exception.Create('Bitmap_.CreateBitmap:'+NL+NL+PixelformatErrorText)
      else if   (BitMap.Width<>Width) or (BitMap.Height<>Height) then
                raise Exception.Create('Bitmap_.CreateBitmap:'+NL+NL+TEXT_MEMORY_FULL)
           else Result:=True;
  except
    on E:Exception do
       begin Result:=False; Error(E.Message,Application.Title+' - ''Create bitmap''');
             BitMap.Free; BitMap:=nil;
       end;
  end;
end;

function  BitMap32Create(var BitMap:TBitMap; Width,Height:Integer):Boolean;
begin
  Result := CreateBitmap( Width, Height, pf32Bit, Bitmap );
end;

function  BitMapCreate(var BitMap:TBitMap; Width,Height:Integer):Boolean;
begin
  Result := CreateBitmap( Width, Height, pf24Bit, Bitmap );
end;

function  BitMapCreateMask(var {i/o: }BitMap,{out: }MaskBitMap:TBitMap; MaskColor:TRGB; MaskPct:Integer):Boolean;
var i,j,k,MaskLimit:Integer; p,q:PRGBVector; rgb:TRGB; BitMapRect:TRect;
begin // Returns 'False' on error or if no pixels are masked
  MaskBitMap:=nil;
  if (BitMap<>nil) and BitMapCreate(MaskBitMap,BitMap.Width,BitMap.Height) then
     begin
       MaskLimit:=((3*255)*MaskPct) div 100;
       k:=0; BitMapRect:=Rect(0,0,BitMap.Width,BitMap.Height);
       for i:=0 to Pred(BitMap.Height) do
           begin p:=BitMap    .ScanLine[i];
                 q:=MaskBitMap.ScanLine[i];
                 for j:=0 to Pred(BitMap.Width) do
                     begin rgb:=p[j];
                           if   (Abs(rgb.r-MaskColor.r)+
                                 Abs(rgb.g-MaskColor.g)+
                                 Abs(rgb.b-MaskColor.b)) <= MaskLimit then
                                begin q[j]:=RGB_WHITE; Inc(k); end
                           else q[j]:=RGB_BLACK;
                     end;
           end;
       if   k=0 then begin
            MaskBitMap.Free; MaskBitMap:=nil;
            end
       else if RGBToColor(MaskColor)<>RGBToColor(RGB_BLACK) then
               // change the bitmap so it has black as mask-color
               BitMapApplyMask(BitMap,MaskBitMap);
     end;
  Result:=MaskBitMap<>nil;
end;

function  BitMapCreateMaskMaskingExteriorOnly(var {i/o: }BitMap,{out: }MaskBitMap:TBitMap; MaskColor:TRGB; MaskPct:Integer):Boolean;
var i,j,k,MaskLimit:Integer; p,q:PRGBVector; rgb:TRGB;
begin // Returns 'False' on error or if no pixels are masked
  MaskBitMap:=nil;
  if (BitMap<>nil) and
     BitMapCreate(MaskBitMap,BitMap.Width,BitMap.Height) then begin
     MaskBitMap.Canvas.Brush.Color:=clBlack;
     MaskBitMap.Canvas.FillRect(Rect(0,0,MaskBitMap.Width,MaskBitMap.Height));
     MaskLimit:=((3*255)*MaskPct) div 100;
     k:=0;
     for i:=0 to Pred(BitMap.Height) do
         begin p:=BitMap    .ScanLine[i];
               q:=MaskBitMap.ScanLine[i];
               j:=0;
               while j<BitMap.Width do begin
                 rgb:=p[j];
                 if   (Abs(rgb.r-MaskColor.r)+
                       Abs(rgb.g-MaskColor.g)+
                       Abs(rgb.b-MaskColor.b)) <= MaskLimit then
                      begin q[j]:=RGB_WHITE; Inc(k); Inc(j); end
                 else j:=BitMap.Width;
                 end;
               while j>0 do begin
                 Dec(j);
                 rgb:=p[j];
                 if   (Abs(rgb.r-MaskColor.r)+
                       Abs(rgb.g-MaskColor.g)+
                       Abs(rgb.b-MaskColor.b)) <= MaskLimit then
                      begin q[j]:=RGB_WHITE; Inc(k); end
                 else j:=0;
                 end;
           end;
     for j:=0 to Pred(BitMap.Width) do
         begin i:=0;
               while i<BitMap.Height do begin
                 p:=BitMap    .ScanLine[i];
                 q:=MaskBitMap.ScanLine[i];
                 rgb:=p[j];
                 if   (Abs(rgb.r-MaskColor.r)+
                       Abs(rgb.g-MaskColor.g)+
                       Abs(rgb.b-MaskColor.b)) <= MaskLimit then
                      begin q[j]:=RGB_WHITE; Inc(k); Inc(i); end
                 else i:=BitMap.Height;
                 end;
               while i>0 do begin
                 Dec(i);
                 p:=BitMap    .ScanLine[i];
                 q:=MaskBitMap.ScanLine[i];
                 rgb:=p[j];
                 if   (Abs(rgb.r-MaskColor.r)+
                       Abs(rgb.g-MaskColor.g)+
                       Abs(rgb.b-MaskColor.b)) <= MaskLimit then
                      begin q[j]:=RGB_WHITE; Inc(k); end
                 else i:=0;
                 end;
           end;

     if   k=0 then begin
          MaskBitMap.Free; MaskBitMap:=nil;
          end
     else if RGBToColor(MaskColor)<>RGBToColor(RGB_BLACK) then
            // change the bitmap so it has black as mask-color
            BitMapApplyMask(BitMap,MaskBitMap);
     end;
  Result:=MaskBitMap<>nil;
end;


var BitMapDumpCount:Integer=0;

function  BitMapDump(BitMap:TBitMap):String;
var s:String;
begin
  Inc(BitMapDumpCount);
  s:=Format('t%4.4d.bmp',[BitMapDumpCount]);
  Result:=MainForm.ApplicationDataPath+s;
  if BitMap<>nil then BitMap.SaveToFile(Result);
end;

function  BitMapEqual( BitMap1__, BitMap2__ : TBitMap; const Rect1__, Rect2__ : TRect; EqualityThresholdPct__ : Integer ) : Boolean;
var Col, Row, Count : Integer;
begin
  Result := ( RectWidth ( Rect1__ ) = RectWidth ( Rect2__ ) ) and
            ( RectHeight( Rect1__ ) = RectHeight( Rect2__ ) ) ;
  if Result
     and
     ( ( Rect1__.Left <> Rect2__.Left ) or ( Rect1__. Top <> Rect2__.Top ) ) then begin
     Count := 0;
     for Row := 0 to Pred( RectHeight( Rect1__ ) ) do
         for Col := 0 to Pred( RectWidth ( Rect1__ ) ) do
             if BitMap1__.Canvas.Pixels[ Rect1__.Left + Col, Rect1__.Top + Row ] <>
                BitMap2__.Canvas.Pixels[ Rect2__.Left + Col, Rect2__.Top + Row ] then
                Inc( Count );
     Result := Count < ( ( ( 100 - EqualityThresholdPct__ ) * RectWidth( Rect1__ ) * RectHeight( Rect1__ ) ) div 100 );
     end;
end;

function  BitMapExtractRect(var BitMap:TBitMap; const Rect:TRect):Boolean;
var R:TRect; Temp:TBitMap;
begin
  Result:=BitMap<>nil;
  if Result then begin
     R:=Classes.Rect(Max(0            ,Rect.Left),
                     Max(0            ,Rect.Top),
                     Min(BitMap.Width ,Rect.Right),
                     Min(BitMap.Height,Rect.Bottom));

     Result:=BitMapCreate(Temp,Max(1,RectWidth(R)),Max(1,RectHeight(R)));
     if Result then begin
        Temp.Canvas.CopyRect(Classes.Rect(0,0,Temp.Width,Temp.Height),BitMap.Canvas,R);
        BitMap.Free;
        BitMap:=Temp;
        end;
     end;
end;

function  BitMapFillWithTile(DestBitMap,SourceBitMap:TBitMap; SourceRect:TRect; XOffset,YOffset:Integer):Boolean;
var H,W,X,Y:Integer;
begin
  W:=RectWidth(SourceRect); H:=RectHeight(SourceRect);
  Result:=Assigned(DestBitMap) and Assigned(SourceBitMap) and
          (DestBitMap.PixelFormat=SourceBitMap.PixelFormat) and
          (W>0) and (H>0);
  if Result then with DestBitMap do with Canvas do begin
     CopyMode:=cmSrcCopy; Y:=YOffset;
     repeat X:=XOffset;
            repeat CopyRect(Rect(X,Y,X+W,Y+H),SourceBitMap.Canvas,SourceRect);
                   Inc(X,W);
            until  X>=Width;
            inc(Y,H);
     until  Y>=Height;
     end;
end;

function  BitMapFloodFillMaskedPixels(DestBitMap,SourceBitMap:TBitMap; MaskColor:TRGB; MaskPct:Integer):Boolean;
var i,X,Y,DestPitch,SourcePitch,SpanLength,Start,MaskLimit:Integer;
    RGB,RGBStart:TRGB; p,q,r:PRGBVector;
begin
  Result:=False;
  if Assigned(SourceBitMap) and (SourceBitMap.PixelFormat=pf24bit) and
     Assigned(DestBitMap  ) and (DestBitMap  .PixelFormat=pf24bit) and
     (SourceBitMap.Width>1) and (SourceBitMap.Height>1) and
     BitMapResize(DestBitMap,SourceBitMap.Width,SourceBitMap.Height) then with SourceBitMap do begin
     MaskLimit:=((3*255)*MaskPct) div 100;
     // interpolate masked pixels horizontally
     for Y:=0 to Pred(Height) do begin
         p:=ScanLine[Y];
         q:=DestBitMap.ScanLine[Y];
         Start:=-1; RGBStart:=p[0];//MaskColor;
         for X:=0 to Pred(Width) do begin
             RGB:=p[X];
             q[X]:=RGB;
             if   (Abs(RGB.r-MaskColor.r)+
                   Abs(RGB.g-MaskColor.g)+
                   Abs(RGB.b-MaskColor.b)) > MaskLimit then begin // 'True': it's not a masked pixel
                  if Start<Pred(X) then begin // 'True': there are masked pixels to the left of the current one
                     SpanLength:=X-Start;
                     if  Start<0 then RGBStart:=RGB;
                     for i:=Succ(Start) to Pred(X) do begin
                         q[i].r:=(RGBStart.r*(X-i) div SpanLength)+(RGB.r*(i-Start) div SpanLength);
                         q[i].g:=(RGBStart.g*(X-i) div SpanLength)+(RGB.g*(i-Start) div SpanLength);
                         q[i].b:=(RGBStart.b*(X-i) div SpanLength)+(RGB.b*(i-Start) div SpanLength);
                         end;
                     end;
                  Start:=X; RGBStart:=RGB;
                  end
             else begin // a masked pixel
                  end;
             end;
         if (Start>=0) and (Start<Pred(Width)) then begin // fill right side masked pixels, if any
            //SpanLength:=Pred(Width)-Start;
            for X:=Succ(Start) to Pred(Width) do begin
                q[X]:=RGBStart;
                //q[X].r:=(RGBStart.r*(Pred(Width)-X) div SpanLength)+(MaskColor.r*(X-Start) div SpanLength);
                //q[X].g:=(RGBStart.g*(Pred(Width)-X) div SpanLength)+(MaskColor.g*(X-Start) div SpanLength);
                //q[X].b:=(RGBStart.b*(Pred(Width)-X) div SpanLength)+(MaskColor.b*(X-Start) div SpanLength);
                end;
            end;
         end;

     // interpolate masked pixels vertically
     p          :=SourceBitMap.ScanLine[0];
     q          :=DestBitMap  .ScanLine[0];
     SourcePitch:=BitMapPitch(SourceBitMap);
     DestPitch  :=BitMapPitch(DestBitMap);

     for X:=0 to Pred(Width) do begin
         Start:=-1; RGBStart:=q[X];//MaskColor;
         for Y:=0 to Pred(Height) do begin
             RGB:=PRGB(Int64(Cardinal(p))+Int64(X*SizeOf(TRGB))+Int64(Y*SourcePitch))^;
             if   (Abs(RGB.r-MaskColor.r)+
                   Abs(RGB.g-MaskColor.g)+
                   Abs(RGB.b-MaskColor.b)) > MaskLimit then begin // 'True': it's not a masked pixel
                  RGB:=PRGB(Int64(Cardinal(q))+Int64(X*SizeOf(TRGB))+Int64((Y*DestPitch)))^; // get the color from the destination bitmap which already has been interpolated horizontally
                  if Start<Pred(Y) then begin // 'True': there are masked pixels above the current one
                     SpanLength:=Y-Start;
                     if  Start<0 then RGBStart:=RGB;
                     for i:=Succ(Start) to Pred(Y) do begin
                         r:=DestBitMap.ScanLine[i];
                         r[X].r:=(RGBStart.r*(Y-i) div SpanLength)+(RGB.r*(i-Start) div SpanLength);
                         r[X].g:=(RGBStart.g*(Y-i) div SpanLength)+(RGB.g*(i-Start) div SpanLength);
                         r[X].b:=(RGBStart.b*(Y-i) div SpanLength)+(RGB.b*(i-Start) div SpanLength);
                         end;
                     end;
                  Start:=Y; RGBStart:=RGB;
                  end
             else begin // a masked pixel
                  end;
             end;
         if (Start>=0) and (Start<Pred(Height)) then begin
            //SpanLength:=Pred(Height)-Start;
            for Y:=Succ(Start) to Pred(Height) do begin // fill masked pixels at the bottom, if any
                r:=DestBitMap.ScanLine[Y];
                r[X]:=RGBStart;
                //r[X].r:=(RGBStart.r*(Pred(Height)-Y) div SpanLength)+(MaskColor.r*(Y-Start) div SpanLength);
                //r[X].g:=(RGBStart.g*(Pred(Height)-Y) div SpanLength)+(MaskColor.g*(Y-Start) div SpanLength);
                //r[X].b:=(RGBStart.b*(Pred(Height)-Y) div SpanLength)+(MaskColor.b*(Y-Start) div SpanLength);
                end;
            end;
         end;

     Result:=True;
     end;
end;

procedure BitMapGrayImage(BitMap:TBitMap);
var X,Y,RGB:Integer; p:PRGB;
begin // precondition: 'BitMap' has pixelformat 24-bit rgb
  with BitMap do
    for Y:=0 to Pred(Height) do begin
        p:=ScanLine[Y];
        for X:=0 to Pred(Width) do begin
            RGB:=p.r+p.g+p.b;
            if RGB<>0 then begin
               // simple gray algorithm: sum(RGB) div 3
               //RGB:=RGB div 3;

               // luminance Rec. 709 RGB coefficients: 0.2126, 0.7152, and 0.0722. in this program, RGB is really BGR, hence, the discrepancy between the formula and the code.
               RGB := Round( ( 0.0722 * p.r ) + ( 0.7152 * p.g ) + ( 0.2126 * p.b ) );

               p.r:=RGB; p.g:=RGB; p.b:=RGB;
               end;
            Inc(p);
            end;
        end;
end;

function  BitMapIsColor(BitMap:TBitMap; const Rect:TRect; Color:TRGB; MaskBitMapPct:Integer):Boolean;
var Col,Row,MaskLimit:Integer; p:PRGBVector; rgb:TRGB;
begin // precondition: 'BitMap' has pixelformat 24-bit rgb
  Result:=False;
  if BitMap<>nil then with Rect do begin
     MaskLimit:=((3*255)*MaskBitMapPct) div 100;
     for Row:=Max(0,Top) to Pred(Min(Bottom,BitMap.Height)) do begin
         p:=BitMap.ScanLine[Row];
         for Col:=Max(0,Left) to Pred(Min(Right,BitMap.Width)) do begin
             rgb:=p[Col];
             if   MaskBitMapPct=0 then begin
                  if (rgb.r<>Color.r) or
                     (rgb.g<>Color.g) or
                     (rgb.b<>Color.b) then exit;
                  end
             else if (Abs(rgb.r-Color.r)+
                      Abs(rgb.g-Color.g)+
                      Abs(rgb.b-Color.b)) > MaskLimit then exit;
             end;
         end;
     end;
  Result:=True;
end;

function  BitMapMakeTransparent(BitMap:TBitMap; MaskColor:TRGB; MaskPct:Integer):Boolean;
var i,j,MaskLimit:Integer; p:PRGBVector; rgb:TRGB;
begin // Returns 'False' if no pixels are masked
  Result:=False;
  if BitMap<>nil then
     begin
       MaskLimit:=((3*255)*MaskPct) div 100;
       for i:=0 to Pred(BitMap.Height) do
           begin p:=BitMap.ScanLine[i];
                 for j:=0 to Pred(BitMap.Width) do
                     begin rgb:=p[j];
                           if   (Abs(rgb.r-MaskColor.r)+
                                 Abs(rgb.g-MaskColor.g)+
                                 Abs(rgb.b-MaskColor.b)) <= MaskLimit then
                                begin p[j]:=MaskColor; Result:=True;
                                end;
                     end;
           end;
       BitMap.Transparent:=Result;
       BitMap.TransparentMode:=tmFixed;
       BitMap.TransparentColor:=RGBToColor(MaskColor);
     end;
end;


(*
function  BitMapMaskBackground(var {io:} BitMap, {io:} MaskBitMap, {i:} BackgroundBitMap:TBitMap; MaskPct:Integer; Masked:Boolean):Integer;
var i,j,k,BackgroundWidth,Left,MaskLimit:Integer; // masks left-side and right-side pixels only, no interior pixels
    p,q,r:PRGBVec; rgb1,rgb2:TRGB;
begin // Returns number of masked pixels
  Result:=0;
  if (BitMap<>nil) and (MaskBitMap<>nil) and (BackgroundBitMap<>nil) and
     (BitMap.Width =MaskBitMap      .Width) and
     (BitMap.Height=MaskBitMap      .Height) and
     {(BitMap.Width=BackgroundBitMap.Width) and}
     (BitMap.Height=BackgroundBitMap.Height) then begin
     MaskLimit:=((3*255)*MaskPct) div 100;
     BackgroundWidth:=BackgroundBitMap.Width;
     for i:=0 to Pred(BitMap.Height) do
         begin p:=BitMap          .ScanLine[i];
               q:=BackgroundBitMap.ScanLine[i];
               r:=MaskBitMap      .ScanLine[i];
               Left:=-1;
               for j:=0 to Pred(BitMap.Width) do
                   begin k:=j mod BackgroundWidth;
                         if k=0 then begin // start a new frame
                            if Left>=0 then // mask right side of last frame
                               for k:=Left to Pred(j) do begin
                                   p[k]:=RGB_BLACK;
                                   r[k]:=RGB_WHITE; Inc(Result);
                                   end;
                            Left:=-1; k:=0;
                            end;

                         rgb1:=p[j];
                         rgb2:=q[k];

                         if   (Abs(rgb1.r-rgb2.r)+
                               Abs(rgb1.g-rgb2.g)+
                               Abs(rgb1.b-rgb2.b)) <= MaskLimit then
                              begin if      Left=-1 then begin // left side
                                            p[j]:=RGB_BLACK;
                                            r[j]:=RGB_WHITE; Inc(Result);
                                            end
                                    else if Left=-2 then Left:=j; // new candidate for right-side start
                              end
                         else if not Masked then begin
                                 r[j]:=RGB_BLACK; // build mask now
                                 if Left>=0 then
                                    for k:=Left to Pred(j) do r[k]:=RGB_BLACK;
                                 Left:=-2; // no right-side starting-point candidates at the moment
                                 end;
                   end;
               if Left>=0 then // mask right side
                  for k:=Left to Pred(BitMap.Width) do begin
                      p[k]:=RGB_BLACK;
                      r[k]:=RGB_WHITE; Inc(Result);
                      end;
         end;
     end;
end;
*)

function  BitMapMaskBackground(var {io:} BitMap, {io:} MaskBitMap, {i:} BackgroundBitMap:TBitMap; MaskPct:Integer; Masked:Boolean):Integer;
var i,BackgroundWidth,MaskLimit:Integer; // masks most of the matching pixels reachable from one of the edges
    s:PRGBVector;

  procedure MaskRow(Row__:Integer);
  var i,j,Left:Integer; // masks most of the matching pixels reachable from one of the edges
      HasRoute:Boolean; p,q,r:PRGBVector; rgb1,rgb2:TRGB;
  begin
    p:=BitMap          .ScanLine[Row__];
    q:=BackgroundBitMap.ScanLine[Row__];
    r:=MaskBitMap      .ScanLine[Row__];
    Left:=-1; HasRoute:=True;
    for i:=0 to Pred(BitMap.Width) do
        begin j:=i mod BackgroundWidth;
              if j=0 then begin // start a new frame
                 if Left>=0 then // mask right side of last frame
                    for j:=Left to Pred(i) do begin
                        p[j]:=RGB_BLACK;
                        r[j]:=RGB_WHITE; Inc(Result);
                        s[j].r:=Ord(True);
                        end;
                    Left:=-1; j:=0; HasRoute:=True;
                    end;

              rgb1:=p[i];
              rgb2:=q[j];

              if   (Abs(rgb1.r-rgb2.r)+
                    Abs(rgb1.g-rgb2.g)+
                    Abs(rgb1.b-rgb2.b)) <= MaskLimit then
                   begin if      Left=-1 then begin // left side
                                 p[i]:=RGB_BLACK;
                                 r[i]:=RGB_WHITE; Inc(Result);
                                 s[j].r:=Ord(True);
                                 end
                         else if Left=-2 then Left:=i; // new candidate for right-side start
                         if Boolean(s[i].r) then HasRoute:=True;
                   end
              else begin if not Masked then begin
                            r[i]:=RGB_BLACK; // build mask now

                            if (rgb1.r<16) and (rgb1.g<16) and (rgb1.b<16) then begin
                               // ensure that non-masked pixels aren't almost black;
                               // otherwise they risk being treated as masked pixels,
                               // e.g., by some of the alpha-blending functions
                               Inc(rgb1.r,16); Inc(rgb1.g,16); Inc(rgb1.b,16);   // lightening up the pixel a little won't be noticeable
                               p[i]:=rgb1;
                               end;

                            if Left>=0 then
                               if   HasRoute then
                                    for j:=Left to Pred(i) do begin
                                        p[j]:=RGB_BLACK;
                                        r[j]:=RGB_WHITE; Inc(Result);
                                        s[j].r:=Ord(True);
                                        end
                               else for j:=Left to Pred(i) do begin
                                        r[j]:=RGB_BLACK;
                                        s[j].r:=Ord(False);
                                        end;
                            end;

                         Left:=-2; // no right-side starting-point candidates at the moment
                         s[i].r:=Ord(False); HasRoute:=False;
                   end;
        end;

    if  Left>=0 then // mask right side
        for j:=Left to Pred(BitMap.Width) do begin
            p[j]:=RGB_BLACK;
            r[j]:=RGB_WHITE; Inc(Result);
            s[j].r:=Ord(True);
            end;
  end;

begin // BitMapmaskBackground; Returns number of masked pixels
  Result:=0;
  if (BitMap<>nil) and (MaskBitMap<>nil) and (BackgroundBitMap<>nil) and
     (BitMap.Width =MaskBitMap      .Width) and
     (BitMap.Height=MaskBitMap      .Height) and
     {(BitMap.Width=BackgroundBitMap.Width) and}
     (BitMap.Height=BackgroundBitMap.Height) and
     MakeStaticWorkBitMap(BitMap.Width,1,True) then begin
     BackgroundWidth:=BackgroundBitMap.Width;
     MaskLimit:=((3*255)*MaskPct) div 100;
     s:=StaticWorkBitMap.ScanLine[0];

     for i:=0 to Pred(BitMap.Width) do s[i].r:=Ord(True); // 's' signals whether there is a route to the top/bottom row using masked pixels only
     for i:=0 to BitMap.Height div 2 do MaskRow(i);

     for i:=0 to Pred(BitMap.Width) do s[i].r:=Ord(True);
     for i:=Pred(BitMap.Height) downto Succ(BitMap.Height div 2) do MaskRow(i);
     end;
end;

function  BitMapMaskBackgroundForAllPixels(var {io:} BitMap, {io:} MaskBitMap, {i:} BackgroundBitMap:TBitMap; MaskPct:Integer; Masked:Boolean):Integer;
var i,j,BackgroundWidth,MaskLimit:Integer; p,q,r:PRGBVector; rgb1,rgb2:TRGB; // masks all matching pixels, including interior pixels
begin // Returns number of masked pixels
  Result:=0;
  if (BitMap<>nil) and (MaskBitMap<>nil) and (BackgroundBitMap<>nil) and
     (BitMap.Width=MaskBitMap.Width) and (BitMap.Height=MaskBitMap.Height) and
     {(BitMap.Width=BackgroundBitMap.Width) and}
     (BitMap.Height=BackgroundBitMap.Height) then begin
     MaskLimit:=((3*255)*MaskPct) div 100;
     BackgroundWidth:=BackgroundBitMap.Width;
     for i:=0 to Pred(BitMap.Height) do
         begin p:=BitMap          .ScanLine[i];
               q:=BackgroundBitMap.ScanLine[i];
               r:=MaskBitMap      .ScanLine[i];
               for j:=0 to Pred(BitMap.Width) do
                   begin
                         rgb1:=p[j];
                         rgb2:=q[j mod BackgroundWidth];

                         if   (Abs(rgb1.r-rgb2.r)+
                               Abs(rgb1.g-rgb2.g)+
                               Abs(rgb1.b-rgb2.b)) <= MaskLimit then
                              begin p[j]:=RGB_BLACK;
                                    r[j]:=RGB_WHITE; Inc(Result);
                              end
                         else if not Masked then begin
                                 r[j]:=RGB_BLACK; // build mask now
                                 end;
                   end;
         end;
     end;
end;

function  BitMapMaskBackgroundForAllPixels_HueAndSaturationOnly(var {io:} BitMap, {io:} MaskBitMap, {i:} BackgroundBitMap:TBitMap; MaskPct:Integer; Masked:Boolean):Integer;
var i,j,BackgroundWidth:Integer; p,q,r:PRGBVector; rgb1,rgb2:TRGB; hsv1, hsv2:THSVColor; MaskLimit: Single; // masks all matching pixels, including interior pixels
begin // Returns number of masked pixels
  Result:=0;
  if (BitMap<>nil) and (MaskBitMap<>nil) and (BackgroundBitMap<>nil) and
     (BitMap.Width=MaskBitMap.Width) and (BitMap.Height=MaskBitMap.Height) and
     {(BitMap.Width=BackgroundBitMap.Width) and}
     (BitMap.Height=BackgroundBitMap.Height) then begin
     MaskLimit:=(2*MaskPct) / 100;
     BackgroundWidth:=BackgroundBitMap.Width;
     for i:=0 to Pred(BitMap.Height) do
         begin p:=BitMap          .ScanLine[i];
               q:=BackgroundBitMap.ScanLine[i];
               r:=MaskBitMap      .ScanLine[i];
               for j:=0 to Pred(BitMap.Width) do
                   begin
                         rgb1:=p[j];
                         rgb2:=q[j mod BackgroundWidth];

                         RGBToHSV( rgb1, hsv1 );
                         RGBToHSV( rgb2, hsv2 );

                         if   ( (Abs(hsv1.h-hsv2.h) / 360.00)
                                +
                                 Abs(hsv1.s-hsv2.s)
                              )
                              <= MaskLimit then
                              begin p[j]:=RGB_BLACK;
                                    r[j]:=RGB_WHITE; Inc(Result);
                              end
                         else if not Masked then begin
                                 r[j]:=RGB_BLACK; // build mask now
                                 end;
                   end;
         end;
     end;
end;

function  BitMapPitch(BitMap:TBitMap):Integer;
var a,b:Cardinal;
begin
  with BitMap do
    if  Height>1 then begin
        a:=Cardinal(ScanLine[1]);
        b:=Cardinal(ScanLine[0]);
        if   a>=b then
             Result:= Integer(Cardinal(a-b))
        else Result:=-Integer(Cardinal(b-a));
        end
    else Result:=0;
end;

function  BitMapResize(var BitMap:TBitMap; Width,Height:Integer):Boolean;
begin
  if   BitMap=nil then
       Result:=BitMapCreate(BitMap,Width,Height)
  else try    Width:=Max(1,Width); Height:=Max(1,Height);
              if BitMap.Width <>Width  then BitMap.Width :=Width;
              if BitMap.Height<>Height then BitMap.Height:=Height;
              BitMap.Canvas.CopyMode:=cmSrcCopy;
              if   (BitMap.Width<>Width) or (BitMap.Height<>Height) then
                   raise Exception.Create('Pict_.BitMapResize:'+NL+NL+TEXT_MEMORY_FULL)
              else Result:=True;
       except on E:Exception do Result:=Error(E.Message,Application.Title+' - ''Resize BitMap''');
       end;
end;

procedure BitMapRGBShift(var BitMap:TBitMap; RGBShift:TRGBShift);
var i,j:Integer; p:PRGBVector;
begin
  with BitMap do
    for i:=0 to Pred(Height) do begin
        p:=ScanLine[i];
        for j:=0 to Pred(Width) do RGBColorShift(RGBShift,p[j]);
        end;
end;

function  BitMapRotate(Dest__,Source__:TBitMap;
                       CounterClockwiseAngleDegrees__:Integer;
                       FillColor__:TColor; BicubicInterpolation__:Boolean):Boolean;
var SourceWidth,SourceHeight,SourceMaxX,SourceMaxY,
    i,j,x,y,x1,y1,x2,y2,x3,y3:Integer;
//  TimeMS:TTimeMS;
    FillColor,Color1,Color2,Color3,Color4:TRGB;
    SourceHalfWidth,SourceHalfHeight,DestHalfWidth,DestHalfHeight:Double;
    AngleRadians,CosAngle,SinAngle,R,G,B,yr,xr,sx,sy,dx1,dy1,dx2,dy2,c1,c2:Double;
    p1,p2:PRGBVector;
    q:PRGB;

  function BicubicInterpolation(x:Double):Double;
  var a,b,c,d,xMinus1,xPlus1,xPlus2:Double;
  begin
    if   x<2.0 then begin
         xMinus1:=x-1.0;
         xPlus1 :=x+1.0;
         xPlus2 :=x+2.0;
         if xPlus2 >0.0 then a:=xPlus2*xPlus2*xPlus2    else a:=0.0;
         if xPlus1 >0.0 then b:=xPlus1*xPlus1*xPlus1    else b:=0.0;
         if x      >0.0 then c:=x*x*x                   else c:=0.0;
         if xMinus1>0.0 then d:=xMinus1*xMinus1*xMinus1 else d:=0.0;
         Result:=(1.0 / 6.0) * (a - (4.0 * b) + (6.0 * c) - (4.0 * d));
         end
    else Result:=0.0;
  end;

begin // rotates a bitmap counterclockwise ; precondition: bitmaps are 24-bit rgb bitmaps
  Result:=False;
//TimeMS:=GetTimeMS;
  try    SourceWidth    :=Source__.Width;   SourceHeight    :=Source__.Height;
         SourceHalfWidth:=SourceWidth  / 2; SourceHalfHeight:=SourceHeight  / 2;
         DestHalfWidth  :=Dest__.Width / 2; DestHalfHeight  :=Dest__.Height / 2;
         SourceMaxX     :=Pred(SourceWidth);
         SourceMaxY     :=Pred(SourceHeight);
         AngleRadians   :=-CounterClockwiseAngleDegrees__*PI/180.0;
         CosAngle       :=Cos(AngleRadians);
         SinAngle       :=Sin(AngleRadians);
         FillColor      :=ColorToRGB(FillColor__);

         yr:=-DestHalfHeight; // start row relative to center
         for y:=0 to Pred(Dest__.Height) do begin // for each row
             q:=Dest__.ScanLine[y]; // get scan line
             xr:=-DestHalfWidth; //start column relative to center
             for x:=0 to Pred(Dest__.Width) do begin
                 sx:= CosAngle*xr+SinAngle*yr+SourceHalfWidth; // calculate source pixel co-ordinates
                 sy:=-SinAngle*xr+CosAngle*yr+SourceHalfHeight;

                 x1:=Trunc(sx); y1:=Trunc(sy); // source pixel co-ordinates truncated to integers,i.e., to grid co-ordinates

                 if (x1>=0) and (x1<SourceWidth) and (y1>=0) and (y1<SourceHeight) then begin // 'True': a source pixel exists
                    R:=0.0; G:=0.0; B:=0.0; // initialize rgb color components
                    dx1:=sx-x1; dy1:=sy-y1; // calculate relative position in grid cell

                    if BicubicInterpolation__ then begin
                       // bicubic interpolation, visiting 4x4 neighboring pixels
                       for y2:=-1 to 2 do begin
                           c1:=BicubicInterpolation(dy1-y2); // 'dy1-y2': 'y' grows downwards in an image, opposite the Cartesian coordinate system; count moving upwards as the positive direction
                           y3:=y1+y2;
                           if y3<0 then y3:=0; // clamp y co-ordinate to source bitmap height
                           if y3>SourceMaxY then y3:=SourceMaxY;
                           p1:=Source__.ScanLine[y3];

                           for x2:=-1 to 2 do begin
                               c2:=c1*BicubicInterpolation(x2-dx1); // 'x2-dx1': 'x' grows to the right, as the Cartesian coordinate system; count moving right as the positive direction
                               x3:=x1+x2;
                               if x3<0 then x3:=0; // clamp x co-ordinate to source bitmap width
                               if x3>SourceMaxX then x3:=SourceMaxX;
                               Color1:=p1^[x3];
                               R:=R+c2*Color1.r;
                               G:=G+c2*Color1.g;
                               B:=B+c2*Color1.b;
                               end;
                           end;
                       end
                    else begin
                       // bilinear interpolation
                       if   x1<SourceMaxX then x2:=Succ(x1) // [sx2,sy2] = bottom-right pixel in current grid cell, with [sx1,sy1] as top-left pixel
                       else x2:=SourceMaxX;
                       if   y1<SourceMaxY then y2:=Succ(y1)
                       else y2:=SourceMaxY;

                       // get the colors from the 4 corners of the current grid cell
                       p1:=Source__.ScanLine[y1];
                       p2:=Source__.ScanLine[y2];
                       Color1:=p1^[x1]; Color2:=p1^[x2];
                       Color3:=p2^[x1]; Color4:=p2^[x2];

                       // interpolate the colors
                       if dx1<0.0 then dx1:=0.0; // guard against numeric instability
                       if dy1<0.0 then dy1:=0.0;
                       dx2:=1.0-dx1;
                       dy2:=1.0-dy1;
                       R:=(dy2*(dx2*Color1.r+dx1*Color2.r))+(dy1*(dx2*Color3.r+dx1*Color4.r));
                       G:=(dy2*(dx2*Color1.g+dx1*Color2.g))+(dy1*(dx2*Color3.g+dx1*Color4.g));
                       B:=(dy2*(dx2*Color1.b+dx1*Color2.b))+(dy1*(dx2*Color3.b+dx1*Color4.b));
                       end;

                    q^.r:=Trunc(R);
                    q^.g:=Trunc(G);
                    q^.b:=Trunc(B);
                    end
                 else begin // no source pixel; use fill color
                    q^:=FillColor;
                    end;

                 Inc(q); // advance to next destination pixel
                 xr:=xr+1.0; // advance to next column relative to center
                 end;
             yr:=yr+1.0; // advance to next row relative to center
             end;
         Result:=True;
  except on E:Exception do begin
            Result:=Error(E.Message,Application.Title);
            end;
  end;
  //TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS);
  //Msg('Time: '+IntToStr(TimeMS),'Rotate bitmap',MB_OK+MB_ICONINFORMATION);
end;

procedure BitMapRotatedImageSize(Width,Height,AngleDegrees:Integer; var RotatedWidth,RotatedHeight:Integer);
var Axis,Index:Integer;
    HalfWidth,HalfHeight,AngleRadians,CosAngle,SinAngle,Min,Max:Double;
    P:array[0..4,0..1] of Double;

  procedure Rotate2D(X,Y:Double; var RotatedX,RotatedY:Double);
  begin
    RotatedX:=X*CosAngle-Y*SinAngle;
    RotatedY:=X*SinAngle+Y*CosAngle;
  end;

begin {BitMapRotatedImageSize}
  HalfWidth    :=Width   / 2;
  HalfHeight   :=Height  / 2;
  AngleRadians :=-AngleDegrees*PI/180.0;
  CosAngle     :=Cos(AngleRadians);
  SinAngle     :=Sin(AngleRadians);

  Rotate2D( HalfWidth, HalfHeight,P[1,0],P[1,1]); // rotate top-right corner
  Rotate2D(-HalfWidth, HalfHeight,P[2,0],P[2,1]); // rotate top-left corner
  Rotate2D(-HalfWidth,-HalfHeight,P[3,0],P[3,1]); // rotate bottom-left corner
  Rotate2D( HalfWidth,-HalfHeight,P[4,0],P[4,1]); // rotate bottom-right corner

  for Axis:=0 to 1 do begin // for x co-ordinates and y co-ordinates respectively
      Min      :=P[1,Axis];
      Max      :=P[1,Axis];
      for Index:=2 to 4 do begin
          if P[Index,Axis]<Min then Min:=P[Index,Axis];
          if P[Index,Axis]>Max then Max:=P[Index,Axis];
          end;
      Max:=Abs(Max-Min) + 0.5;
      if   Axis=0 then
           RotatedWidth :=Trunc(Max)
      else RotatedHeight:=Trunc(Max);
      end;
end;

function  BitMapScale(Dest,Source:TBitMap; FilterDistance:Integer; // 'FilterDistance' = width/2 of the window around a pixel; neighboring pixels within that distance influence the final pixel value
                      LowPriority,Masked:Boolean; MaskColor:TColor; MaskPct:Integer;
                      View:TImageView;
                      HorizontalBorder,VerticalBorder:Integer;
                      var ScaledRect:TRect;
                      var Cancel:Boolean):Boolean;
const // resizes a bitmap with a better image quality than 'StretchDraw'; preconditions: bitmaps are 24-bit rgb bitmaps and 'FilterDistance' > 0
  MAX_PIXEL_WINDOW   = 16;
type
  TPixelWeight       = record
    Pixel            : Integer;		// source pixel
    Weight           : Single;		// pixel weight
  end;

  TPixelWeightVector = array[0..MAX_PIXEL_WINDOW] of TPixelWeight;

  TFilterArray       = array[0..(MaxInt div SizeOf(TPixelWeightVector))-1] of TPixelWeightVector;
  PFilterArray       = ^TFilterArray;

var i,j,X,Y,SourceLeft,SourceTop,SourceWidth,SourceHeight,DestWidth,DestHeight,
    Left,Right,Count,Col,Row,SourcePitch,DestPitch,
    ColFilterDistanceTimes2,RowFilterDistanceTimes2,MaskLimit,
    H1,H2,W1,W2:Integer;

//  TimeMS:TTimeMS;

    XScale,YScale,RowFilterDistance,ColFilterDistance,
    Center,Weight,WeightSum:Single;

    p1,p2,p3:PRGBVector;

    RowFilter,ColFilter:PFilterArray;

    oCursor:TCursor;

    rgb:TRGBs;
    Color,RGBMask:TRGB;

//  Temp:TBitMap;

{
  function  Filter(Value:Single):Single;
  var tt:Single;
  begin // Spline Filter
    if (Value < 0.0) then Value := -Value;
    if (Value < 1.0) then begin
       tt := Sqr(Value);
       Result := 0.5*tt*Value - tt + 2.0 / 3.0;
       end
    else if (Value < 2.0) then begin
       Value := 2.0 - Value;
       Result := 1.0/6.0 * Sqr(Value) * Value;
       end
    else Result := 0.0;
  end;
}
  function  Filter(Value:Single):Single;

    function SinC(Value: Single): Single;
    begin
      if   Value <> 0.0 then begin
           Value := Value * Pi;
           Result := Sin(Value) / Value
           end
      else Result := 1.0;
    end;

  begin // Filter (Lanczos 3)
    if   Value  <  0.0 then Value  := -Value;
    if   Value  <  3.0 then Result := SinC(Value) * SinC(Value / 3.0)
    else Result := 0.0;
  end;

  procedure Finalize;
  begin
//  if RowFilter<>nil then begin FreeMem(RowFilter); RowFilter:=nil; end;
//  if ColFilter<>nil then begin FreeMem(ColFilter); ColFilter:=nil; end;
//  Temp.Free; Temp:=nil;
  end;

begin // BitMapScale
//TimeMS:=GetTickCount;
  Result:=True; oCursor:=Screen.Cursor; Cancel:=False;
//RowFilter:=nil; ColFilter:=nil; Temp:=nil;

  W1:=Dest  .Width -2*HorizontalBorder;
  H1:=Dest  .Height-2*VerticalBorder;
  W2:=Source.Width;
  H2:=Source.Height;
  ScaledRect:=Rect(HorizontalBorder,VerticalBorder,HorizontalBorder+W1,VerticalBorder+H1);
  SourceTop:=0; SourceLeft:=0;
  MaskLimit:=((3*255)*MaskPct) div 100;

  if (W1>0) and (H1>0) and (W2>0) and (H2>0) then
     try
       try
//       Screen.Cursor:=crHourGlass;
//       Dest.Canvas.Brush.Color:=MaskColor;
//       Dest.Canvas.FillRect(Rect(0,0,Dest.Width,Dest.Height));

         if (View<>ivStretch) and
            ((View=ivFill)
             or
             (Abs((Abs(W1/H1)-Abs(W2/H2)))>10e-3)
            ) then begin

            H2:=MulDiv(H2,W1,W2); W2:=W1;
            if H2>H1 then begin W2:=MulDiv(W2,H1,H2); H2:=H1; end;
            if View=ivFill then begin
               if W2<W1 then begin
                  H2:=MulDiv(H2,W1,W2); W2:=W1; // use full window-width and cut the image top and bottom
                  SourceTop:=Max(0,MulDiv(Source.Height,H2-H1,H2) div 2);
                  H2:=Min(H2,H1);
                  end;
               if H2<H1 then begin
                  W2:=MulDiv(W2,H1,H2); H2:=H1; // use full window-width and cut the image left and right
                  SourceLeft:=Max(0,MulDiv(Source.Width,W2-W1,W2) div 2);
                  W2:=Min(W2,W1);
                  end;
               end;

            ScaledRect.Left  :=(W1-W2) div 2;
            ScaledRect.Top   :=(H1-H2) div 2;
            ScaledRect.Right :=W1-ScaledRect.Left;
            ScaledRect.Bottom:=H1-ScaledRect.Top;

            //Dest.Canvas.Brush.Color:=MaskColor;
            //Dest.Canvas.FillRect(DestRect);
            end;

         SourceWidth:=Source.Width-2*SourceLeft; SourceHeight:=Source.Height-2*SourceTop; //SourceTop:=0;
         DestWidth  :=RectWidth (ScaledRect);
         DestHeight :=RectHeight(ScaledRect);

         if StaticScaleRowFilterCapacity<DestWidth then begin
            if StaticScaleRowFilter<>nil then FreeMem(StaticScaleRowFilter);
            StaticScaleRowFilterCapacity:=0; StaticScaleRowFilterScale:=0.0; StaticScaleFilterDistance:=0;
            GetMem(StaticScaleRowFilter,DestWidth *SizeOf(RowFilter[0]));
            StaticScaleRowFilterCapacity:=DestWidth;
            end;
         RowFilter:=StaticScaleRowFilter;

         if StaticScaleColFilterCapacity<DestHeight then begin
            if StaticScaleColFilter<>nil then FreeMem(StaticScaleColFilter);
            StaticScaleColFilterCapacity:=0; StaticScaleColFilterScale:=0.0; StaticScaleFilterDistance:=0;
            GetMem(StaticScaleColFilter,DestHeight*SizeOf(ColFilter[0]));
            StaticScaleColFilterCapacity:=DestHeight;
            end;
         ColFilter:=StaticScaleColFilter;

         MakeStaticWorkBitMap(DestWidth,SourceHeight,False); // 'False': raise an exception on error

//       GetMem(RowFilter,DestWidth *SizeOf(RowFilter[0]));
//       GetMem(ColFilter,DestHeight*SizeOf(ColFilter[0]));
//       if not BitMapCreate(DestWidth,SourceHeight,Temp) then
//          raise Exception.Create('Pict_.BitMapScale: '+MemoryFullText);

          // scale factors, new/old
         if   (SourceWidth=1) or (DestWidth=1) then XScale:=DestWidth
         else XScale:=Pred(DestWidth)/Pred(SourceWidth);
         if   (SourceHeight=1) or (DestHeight=1) then YScale:=DestHeight
         else YScale:=Pred(DestHeight)/Pred(SourceHeight);

         if   XScale<1.0 then
              RowFilterDistance:=FilterDistance/XScale
         else RowFilterDistance:=FilterDistance;
         if   RowFilterDistance*2>MAX_PIXEL_WINDOW-2 then
              RowFilterDistance:=(MAX_PIXEL_WINDOW-2) div 2;
         RowFilterDistanceTimes2:=Trunc(2*RowFilterDistance);
         if   YScale<1.0 then
              ColFilterDistance:=FilterDistance/YScale
         else ColFilterDistance:=FilterDistance;
         if   ColFilterDistance*2>MAX_PIXEL_WINDOW-2 then
              ColFilterDistance:=(MAX_PIXEL_WINDOW-2) div 2;
         ColFilterDistanceTimes2:=Trunc(2*ColFilterDistance);

         RGBMask:=ColorToRGB(MaskColor);

         if (XScale<>StaticScaleRowFilterScale) or
            (DestWidth<StaticScaleRowFilterWidth) or
            (FilterDistance<>StaticScaleFilterDistance) then begin
            for X:=0 to Pred(DestWidth) do begin // build row filter
                if (X mod 8 = 0) and LowPriority then SleepEx(0,False); // give the other threads a chance
                if not Cancel then begin // when 'BitMapScale' is called by the image-viewer thread, 'Cancel' may be set to 'True' by the main thread;
                   Center:=X/XScale;
                   Left :=Math.Floor(Center-RowFilterDistance);
                   Right:=Math.Ceil (Center+RowFilterDistance);
                   WeightSum:=0.0;
                   for i:=Left to Right do begin
                       if   i>=0 then
                            if   i<SourceWidth then Col:=i
                            else begin Col:=Pred(SourceWidth)+SourceWidth-i;
                                       if Col<0 then Col:=0;
                                 end
                       else if   -i<SourceWidth then Col:=-i
                            else Col:=Pred(SourceWidth);

                       if   XScale>=1.0 then
                            Weight:=Filter (Center-i)
                       else Weight:=Filter((Center-i)*XScale)*XScale;

                       RowFilter[X,i-Left].Pixel :=Col+SourceLeft;
                       RowFilter[X,i-Left].Weight:=Weight;
                       WeightSum:=WeightSum+Weight;
                       end;

                   for i:=Succ(Right-Left) to High(TPixelWeightVector) do
                       RowFilter[X,i].Weight:=0.0;

                   WeightSum:=Abs(WeightSum);
                   if (WeightSum>10e-3) and (Abs(1.0-WeightSum)>10e-3) then begin
                      // normalize weights so they sum to 1.0;
                      // this avoids that the image gets brighter or darker
                      WeightSum:=1.0/WeightSum; // invert weightsum; multiplication may be faster than division
                      for i:=Left to Right do RowFilter[X,i-Left].Weight:=RowFilter[X,i-Left].Weight*WeightSum;
                      end;
                   end;
                end;
            StaticScaleRowFilterScale:=XScale; StaticScaleRowFilterWidth:=DestWidth; // remember the cached row filter
            end;

         if (YScale<>StaticScaleColFilterScale) or
            (DestHeight<StaticScaleColFilterHeight) or
            (FilterDistance<>StaticScaleFilterDistance) then begin
            for Y:=0 to Pred(DestHeight) do begin // build column filter
                if (Y mod 8 = 0) and LowPriority then SleepEx(0,False); // give the other threads a chance
                if not Cancel then begin
                   Center:=Y/YScale;
                   Left :=Math.Floor(Center-ColFilterDistance);
                   Right:=Math.Ceil (Center+ColFilterDistance);
                   WeightSum:=0.0;
                   for i:=Left to Right do begin
                       if   i>=0 then
                            if   i<SourceHeight then Row:=i
                            else begin Row:=Pred(SourceHeight)+SourceHeight-i;
                                       if Row<0 then Row:=0;
                                 end
                       else if   -i<SourceHeight then Row:=-i
                            else Row:=Pred(SourceHeight);

                       if   YScale>=1.0 then
                            Weight:=Filter (Center-i)
                       else Weight:=Filter((Center-i)*YScale)*YScale;

                       ColFilter[Y,i-Left].Pixel :=Row; // not 'Row+SourceTop' because the input to the column filter is the temporary 'WorkBitMap', which is the output after the row filter has been applied
                       ColFilter[Y,i-Left].Weight:=Weight;
                       WeightSum:=WeightSum+Weight;
                       end;

                   for i:=Succ(Right-Left) to High(TPixelWeightVector) do
                       ColFilter[Y,i].Weight:=0.0;

                   WeightSum:=Abs(WeightSum);
                   if (WeightSum>10e-3) and (Abs(1.0-WeightSum)>10e-3) then begin
                      // normalize weights so they sum to 1.0;
                      // this avoids that the image gets brighter or darker
                      WeightSum:=1.0/WeightSum; // invert weightsum; multiplication may be faster than division
                      for i:=Left to Right do ColFilter[Y,i-Left].Weight:=ColFilter[Y,i-Left].Weight*WeightSum;
                      end;
                   end;
                end;
            StaticScaleColFilterScale:=YScale; StaticScaleColFilterHeight:=DestHeight; // remember the cached column filter
            end;

         if not Cancel then begin
            StaticScaleFilterDistance:=FilterDistance; // remember the cached filter distance
            end
         else begin // clear the cached filter
            StaticScaleRowFilterScale:=0.0;
            StaticScaleColFilterScale:=0.0;
            StaticScaleFilterDistance:=0;
            end;

         for i:=0 to Pred(SourceHeight) do begin // apply row filter
             if (i mod 8 = 0) and LowPriority then SleepEx(0,False); // give the other threads a chance

             if not Cancel then begin
                p1:=Source          .ScanLine[i+SourceTop];
                p2:=StaticWorkBitMap.ScanLine[i];

                for X:=0 to Pred(DestWidth) do
                    if not Cancel then begin
                       rgb.r:=0; rgb.g:=0; rgb.b:=0;
                       for j:=0 to RowFilterDistanceTimes2 do begin
                           Weight:=RowFilter[X,j].Weight;
                           if Weight<>0.0 then begin
                              Color:=p1[RowFilter[X,j].Pixel];
                              rgb.r := rgb.r + Color.r * Weight;
                              rgb.g := rgb.g + Color.g * Weight;
                              rgb.b := rgb.b + Color.b * Weight;
                              end;
                          end;

                       if      (rgb.r >= 255.0) then Color.r := 255
                       else if (rgb.r <= 0.0)   then Color.r := 0
                       else Color.r := Round(rgb.r);
                       if      (rgb.g >= 255.0) then Color.g := 255
                       else if (rgb.g <= 0.0)   then Color.g := 0
                       else Color.g := Round(rgb.g);
                       if      (rgb.b >= 255.0) then Color.b := 255
                       else if (rgb.b <= 0.0)   then Color.b := 0
                       else Color.b := Round(rgb.b);

                       if   (not Masked) or
                            ((Abs(Color.r-RGBMask.r)+
                              Abs(Color.g-RGBMask.g)+
                              Abs(Color.b-RGBMask.b)) > MaskLimit) then
                            p2[X]:=Color
                       else p2[X]:=RGBMask;
                       end;
                end;
             end;

         if   SourceHeight>1 then
              SourcePitch:=BitMapPitch(StaticWorkBitMap)
         else SourcePitch:=0;
         if   DestWidth>1 then
              DestPitch  :=BitMapPitch(Dest)
         else DestPitch  :=0;

         p1:=StaticWorkBitMap.ScanLine[0];
         for X:=0 to Pred(DestWidth) do begin // apply column filter
             if (X mod 8 = 0) and LowPriority then SleepEx(0,False); // give the other threads a chance

             if not Cancel then begin
                p2:=Dest.ScanLine[0+ScaledRect.Top];

                for Y:=0 to Pred(DestHeight) do
                    if not Cancel then begin
                       rgb.r:=0; rgb.g:=0; rgb.b:=0;
                       for j:=0 to ColFilterDistanceTimes2 do begin
                           Weight:=ColFilter[Y,j].Weight;
                           if Weight<>0.0 then begin
                              Color:=PRGB(Integer(p1)+(ColFilter[Y,j].Pixel*SourcePitch))^;
                              rgb.r := rgb.r + Color.r * Weight;
                              rgb.g := rgb.g + Color.g * Weight;
                              rgb.b := rgb.b + Color.b * Weight;
                              end;
                           end;

                       if      (rgb.r >= 255.0) then Color.r := 255
                       else if (rgb.r <= 0.0)   then Color.r := 0
                       else Color.r := Round(rgb.r);
                       if      (rgb.g >= 255.0) then Color.g := 255
                       else if (rgb.g <= 0.0)   then Color.g := 0
                       else Color.g := Round(rgb.g);
                       if      (rgb.b >= 255.0) then Color.b := 255
                       else if (rgb.b <= 0.0)   then Color.b := 0
                       else Color.b := Round(rgb.b);

                       if   (not Masked) or
                            ((Abs(Color.r-RGBMask.r)+
                              Abs(Color.g-RGBMask.g)+
                              Abs(Color.b-RGBMask.b)) > MaskLimit) then
                            p2[X+ScaledRect.Left]:=Color
                       else p2[X+ScaledRect.Left]:=RGBMask;

                       Inc(Cardinal(p2),DestPitch);
                       end;
                Inc(Cardinal(p1),SizeOf(TRGB));
                end;
             end;

       finally //Screen.Cursor:=oCursor;
               Finalize;
       end;
     except
       on E:Exception do begin
          Result:=False; //Error(E.Message,Application.Title); //Finalize;
          //Result:=Error(E.Message,Application.Title);
          ScaledRect:=Rect(0,0,Dest.Width,Dest.Height);
          StaticScaleColFilterScale:=0.0; StaticScaleRowFilterScale:=0.0; StaticScaleFilterDistance:=0;
          end;
     end;
//Msg(IntToStr(CalculateElapsedTimeMS(TimeMS,GetTickCount)),'',MB_OK);
end;

procedure BitMapSwap(var BitMap1,BitMap2:TBitMap);
var Temp:TBitMap;
begin
  Temp:=BitMap1; BitMap1:=BitMap2; BitMap2:=Temp;
end;

function BitMap24BitPixelFormat(OriginalBitMap:TBitMap; var BitMap24Bit:TBitMap):Boolean;
var B:TBitMap;
begin // returns 'True' if the original bitmap is in 24 bit pixelformat, or if the function succeeds in creating a 24 bit pixelformat version; the 24 bit pixelformat bitmap is returned in 'BitMap24Bit' in both cases
  BitMap24Bit:=nil;
  if Assigned(OriginalBitMap) then with OriginalBitMap do
     if   PixelFormat=pf24bit then
          BitMap24Bit:=OriginalBitMap // return the original bitmap as a valid 24-bit pixelformat bitmap
     else if BitMapCreate(B,Width,Height) then // 'True': creating a new 24-bit bitmap succeeded
             try     if (B.Width=Width) and (B.Height=Height) then begin // 'True': the bitmap size wasn't clamped to a smaller size than the original
                        B.Canvas.Draw(0,0,OriginalBitMap);
                        BitMapSwap(B,BitMap24Bit); // return the new 24-bit bitmap
                        end;
             finally B.Free; // free the temporary bitmap unless it has been returned as 'BitMap24Bit'
            end;
  Result:=Assigned(BitMap24Bit);
end;

procedure DrawArrow( Canvas__                    : TCanvas;
                     X__, Y__, ArrowPixelSize__  : Integer;
                     ArrowDirection__            : TDirection;
                     ArrowColor__, ShadowColor__ : TColor );

  procedure DrawArrow(X__,Y__ :Integer; Color__ : TColor );

    procedure DrawArrow( X__, Y__, DX__, DY__ : Integer );
    var Index : Integer;
    begin // 'DrawArrow.DrawArrow.DrawArrow'
      for Index := 1 to ArrowPixelSize__ do with Canvas__ do begin
          MoveTo( X__, Y__ );
          LineTo( X__ + ( DY__ * Pred( 2 * Index ) ), Y__ + ( DX__ * Pred( 2 * Index ) ) );
          Inc( X__, DX__ - DY__ ); Inc(Y__, DY__ - DX__ );
          end;
    end;

  begin // 'DrawArrow.DrawArrow'
    Canvas__.Pen.Color := Color__;
    case ArrowDirection__ of
      Up              : DrawArrow( X__, Y__,  0, +1 );
      Left            : DrawArrow( X__, Y__, +1,  0 );
      Down            : DrawArrow( X__, Y__,  0, -1 );
      Right           : DrawArrow( X__, Y__, -1,  0 );
    end;
  end;

begin // 'DrawArrow'
  Canvas__.Pen.Style := psSolid;
  if ShadowColor__ <> $ffffff then
     DrawArrow( Succ( X__ ), Succ( Y__ ), ShadowColor__ ); // arrow shadow
  DrawArrow(       X__  ,       Y__  , ArrowColor__  ); // arrow
end;

procedure EdgeSmoothing(const Dest,Source,Mask:TBitMap; const Rect:TRect; Horizontal,Vertical:Boolean);
var i,j,H,W,Edge:Integer; c:TRGB; m0,m,p0,p1,p2,p3,px,pt:PRGBVector; // not a general procedure: 'Source' pixels are not used, they are supposed to be part of the 'Dest' image at this time

  procedure Interpolate(x:Integer);
  var R1,G1,B1,Count,HalfOfCount:Integer;
  begin
   //exit;
   Count:=0; R1:=0; G1:=0; B1:=0;

     if x>0 then with p1[Pred(x)] do begin Inc(R1, 2*r); Inc(G1, 2*g); Inc(B1, 2*b); Inc(Count, 2); end;
     if x<W then with p1[Succ(x)] do begin Inc(R1, 2*r); Inc(G1, 2*g); Inc(B1, 2*b); Inc(Count, 2); end;

   if p0<>nil then begin
     with p0[x]                   do begin Inc(R1, 2*r); Inc(G1, 2*g); Inc(B1, 2*b); Inc(Count, 2); end;
     if x>0 then with p0[Pred(x)] do begin Inc(R1, 1*r); Inc(G1, 1*g); Inc(B1, 1*b); Inc(Count, 1); end;
     if x<W then with p0[Succ(x)] do begin Inc(R1, 1*r); Inc(G1, 1*g); Inc(B1, 1*b); Inc(Count, 1); end;
   end;

   if p2<>nil then begin
     with p2[x]                   do begin Inc(R1, 2*r); Inc(G1, 2*g); Inc(B1, 2*b); Inc(Count, 2); end;
     if x>0 then with p2[Pred(x)] do begin Inc(R1, 1*r); Inc(G1, 1*g); Inc(B1, 1*b); Inc(Count, 1); end;
     if x<W then with p2[Succ(x)] do begin Inc(R1, 1*r); Inc(G1, 1*g); Inc(B1, 1*b); Inc(Count, 1); end;
   end;

   with p1[x] do begin
     Inc(R1,8*r); Inc(G1,8*g); Inc(B1,8*b); Inc(Count,8); HalfOfCount:=Count div 2;
     r:=(R1+HalfOfCount) div Count;
     g:=(G1+HalfOfCount) div Count;
     b:=(B1+HalfOfCount) div Count;

     //r:=0; g:=255; b:=255;
     //p1[x]:=q1[x]
     end;
   //Inc(MainForm.DebugCount[0]);
  end;

begin // EdgeSmoothing;
  //exit;
  W:=Dest.Width; H:=Dest.Height;
  if (W=RectWidth(Rect)) and (H=RectHeight(Rect)) and
     (Mask<>nil) and(Mask.Width=W) and (Mask.Height=H) and
     (Rect.Top<=Source.Height) then begin

     W :=Pred(Min(W,Source.Width -Rect.Left)); // caution:  'W' and 'H' are not 'Width' and 'Height' anymore
     H :=Pred(Min(H,Source.Height-Rect.Top));
     //p1:=Dest  .ScanLine[0];

     if H>=0 then begin
        if Horizontal then begin
           p1:=nil;
           p2:=Dest.ScanLine[0];
           for i:=0 to H do begin

               m :=Mask.ScanLine[i];

               p0:=p1; p1:=p2;
               if   i<H then p2:=Dest.ScanLine[Succ(i)]
               else p2:=nil;

               Edge:=0;

               for j:=0 to W do begin
                   c:=m[j];
                   if (c.r=RGB_WHITE.r) and
                      (c.g=RGB_WHITE.g) and
                      (c.b=RGB_WHITE.b) then begin

                      if (Edge=Pred(j)) then with p1[Edge] do begin // 'True': previous mask pixel is black
                         Interpolate(j);
                         Interpolate(Edge);
                         //r:=0; g:=255; b:=0;
                         if j<W then Interpolate(Succ(j));

                         if Edge>0 then Interpolate(Pred(Edge));
                         Interpolate(Edge); // again: seems to give the best results
                         end;

                      Edge:=-Succ(j);
                      end
                   else with p1[j] do begin // mask pixel is black
                      if //(i<=1) or (i>=H) or
                         (j=-Edge) then begin // 'True': previous mask pixel is white
                         if j>0 then Interpolate(Pred(j));
                         Interpolate(j);
                         //r:=255; g:=255; b:=0;
                         if j>1 then Interpolate(Pred(Pred(j)));

                         if j<W then Interpolate(Succ(j));
                         Interpolate(j); // again: seems to give the best results
                         end;
                      Edge:=j;
                      end;
                   end;

               if Edge=W then with p1[Edge] do begin
                  //r:=0; g:=255; b:=0;
                  Interpolate(Edge);
                  end;
               end;
           end;

        if Vertical then begin
           // for ease of implementation, the vertical edge smoothing is not
           // fully symmetrical with the horizontal edge smoothing. the former
           // processes 1 pixel on each side of an edge, whereas the latter
           // processes 2 pixels on each side of the edge.
           p0:=nil;
           p1:=nil;
           p2:=Dest.ScanLine[0];
           m:=nil;
           for i:=0 to H do begin
               m0:=m; // previous mask scan line, if any
               m :=Mask.ScanLine[i];

               px:=p0; p0:=p1; p1:=p2; // 'px': previous(previous(current mask scan line)). vertical edge smoothing was added so late to the program that it was considered too much work to rename all the scan line pointers
               if i<H then begin
                  p2:=Dest.ScanLine[Succ(i)];
                  if   (i+2)<=H then
                       p3:=Dest.ScanLine[i+2]
                  else p3:=nil;
                  end
               else begin
                  p2:=nil;
                  p3:=nil;
                  end;

               for j:=0 to W do begin
                   c:=m[j];
                   if (c.r=RGB_WHITE.r) and
                      (c.g=RGB_WHITE.g) and
                      (c.b=RGB_WHITE.b) then begin
                      if (i>0) and (m0[j].r=RGB_BLACK.r) then begin // 'True': previous mask pixel is black
                         Interpolate(j); // this pixel
                         pt:=p2; // temporary
                         p2:=p1;
                         p1:=p0;
                         p0:=px;
                         Interpolate(j); // pixel above
                         p0:=p1;
                         p1:=p2;
                         p2:=pt;
                         if i<H then begin
                            pt:=p0; // temporary
                            p0:=p1;
                            p1:=p2;
                            p2:=p3;
                            Interpolate(j); // pixel below
                            p2:=p1;
                            p1:=p0;
                            p0:=pt;
                            end;
                         //Interpolate(j); // again: seems to give the best results
                         end;
                      end
                   else with p1[j] do begin // mask pixel is black
                      if (i>0) and (m0[j].r=RGB_WHITE.r) then begin // 'True': previous mask pixel is white
                         pt:=p2; // temporary
                         p2:=p1;
                         p1:=p0;
                         p0:=px;
                         Interpolate(j); // pixel above
                         p0:=p1;
                         p1:=p2;
                         p2:=pt;
                         Interpolate(j); // this pixel
                         if i<H then begin
                            pt:=p0; // temporary
                            p0:=p1;
                            p1:=p2;
                            p2:=p3;
                            Interpolate(j); // pixel below
                            p2:=p1;
                            p1:=p0;
                            p0:=pt;
                            end;
                         //Interpolate(j); // again: seems to give the best results
                         end;
                      end;
                   end;

               //if Edge=W then with p1[Edge] do begin
               //   //r:=0; g:=255; b:=0;
               //   Interpolate(Edge);
               //   end;
               end;
           end;
        end;
     end;
end;

function GetAlphaChannel( Color__ : TColor ) : Integer;
begin
  Result := ( Color__ shr 24 ) and $ff;
end;

function  GridCellToRect(ACol,ARow,ColWidth,RowHeight,GridLineWidth:Integer):TRect;
begin
  Result.Left  :=GridLineWidth+ACol*(GridLineWidth+ColWidth );
  Result.Top   :=GridLineWidth+ARow*(GridLineWidth+RowHeight);
  Result.Right :=Result.Left+ColWidth ;
  Result.Bottom:=Result.Top +RowHeight;
end;

function  HSVToRGB( Hue__, Saturation__, Value__ : Single ) : TRGB;
// preconditions: Hue: 0..360 degrees; Saturation: 0..1; Value: 0..1;
var H, V, X, Y, Z : Integer; Fraction, H1 : Double;
begin
  while Hue__        <  0.0 do
    Hue__            := Hue__ + 360.0;
  while Hue__        >= 360.0 do
    Hue__            := Hue__ - 360.0;
  H1                 := Hue__ / 60.0;
  H                  := Trunc( H1 );
  Fraction           := H1 - H; // fraction of H1
  V                  := Trunc( 255.0 * Value__ );
  X                  := Trunc( 255.0 * Value__ * ( 1.0 -   Saturation__ ) );
  Y                  := Trunc( 255.0 * Value__ * ( 1.0 - ( Saturation__ * Fraction ) ) );
  Z                  := Trunc( 255.0 * Value__ * ( 1.0 - ( Saturation__ * ( 1.0 - Fraction ) ) ) );
  with Result do
    case H of
      0     : begin r := V; g := Z; b := X; end;
      1     : begin r := Y; g := V; b := X; end;
      2     : begin r := X; g := V; b := Z; end;
      3     : begin r := X; g := Y; b := V; end;
      4     : begin r := Z; g := X; b := V; end;
      else    begin r := V; g := X; b := Y; end;
    end;
end;

function  IdenticalRects(const R1,R2:TRect):Boolean;
begin
  with R1 do Result:=(Left=R2.Left) and (Top=R2.Top) and (Right=R2.Right) and (Bottom=R2.Bottom);
end;

function  InvertedColor( Color : TColor ) : TColor;
begin
  with ColorToRGB( Color ) do
    Result := RGBComponentsToColor( 255 - r, 255 - g, 255 - b );
end;

function  MakeStaticWorkBitMap(Width,Height:Integer; HandleException:Boolean):Boolean;
begin
  try
    if  StaticWorkBitMap=nil then
        if   BitMapCreate(StaticWorkBitMap,Width,Height) then //
        else raise Exception.Create(TEXT_MEMORY_FULL);
    if  Width >StaticWorkBitMap.Width   then StaticWorkBitMap.Width:=Width;
    if  Height>StaticWorkBitMap.Height  then StaticWorkBitMap.Height:=Height;
    if (Width >StaticWorkBitMap.Width ) or
       (Height>StaticWorkBitMap.Height) then
       raise Exception.Create(TEXT_MEMORY_FULL)
    else Result:=True;
  except
    on E:Exception do
       begin Result:=False; StaticWorkBitMap.Free; StaticWorkBitMap:=nil;
             if   HandleException then
                  Error(E.Message,Application.Title+' - ''Create Static Working Storage BitMap''')
             else raise;
       end;
  end;
end;

function  MaxBitMapTextLength( BitMap__:TBitMap; const Rect__:TRect ) : Integer;
var W, H, PixelByteSize : Integer;
begin // returns the maximum length of a text stored in the bitmap in the given
      // rectangle
  if (BitMap__<>nil) and
     (Rect__.Left<=Rect__.Right) and (Rect__.Top<=Rect__.Bottom) and
     (Rect__.Left>=0) and (Rect__.Top>=0) and
     (Rect__.Right<=BitMap__.Width) and (Rect__.Bottom<=BitMap__.Height) then begin
     W:=RectWidth(Rect__)-2; H:=RectHeight(Rect__)-3; // '-2' and '-3': a 1-pixel border around the text-area minus one more row at the bottom
     PixelByteSize:=PIXEL_BYTE_SIZE[BitMap__.PixelFormat];
     Result := ( ( PixelByteSize * W * H ) - PixelByteSize - 1 ) div SizeOf( Char );
     end
  else Result := 0;
end;

function  NotBlackColor(Color__:TColor):TColor;
const BLACK_RGB_THRESHOLD=9; NOT_BLACK_COLOR:TRGB =(r:3; g:3; b:3);
var RGB:TRGB;
begin
  RGB:=ColorToRGB(Graphics.ColorToRGB(Color__));
  if RGB.r+RGB.b+RGB.g<BLACK_RGB_THRESHOLD then RGB:=NOT_BLACK_COLOR;
  Result:=RGBToColor(RGB);
end;

procedure PointToCell(X,Y,ColWidth,RowHeight,GridLineWidth:Integer; var ACol,ARow:Integer);
begin
  ACol:=X div (ColWidth +GridLineWidth);
  ARow:=Y div (RowHeight+GridLineWidth);
end;

procedure RGBColorShift(RGBShift:TRGBShift; var RGB:TRGB);
var i:Integer;
begin
  with RGB do
    case RGBShift of
      rgbShiftNone :;
      rgbShiftRG   : begin i:=r; r:=g; g:=Lo(i); end; // red-green shift
      rgbShiftRB   : begin i:=r; r:=b; b:=Lo(i); end; // red-blue shift
      rgbShiftGB   : begin i:=g; g:=b; b:=Lo(i); end; // green-blue shift
    end; // case
end;
{
function SaveBMPAsWMFFile(BitMap__:TBitMap; const FileName__:String):Boolean;
var Wmf: TMetafile; WmfCanvas: TMetafileCanvas;
begin
  try Wmf := TMetafile.Create;
      try
        Wmf.Width := Bitmap__.Width;
        Wmf.Height := Bitmap__.Height;
        WmfCanvas := TMetafileCanvas.Create(Wmf, 0);
        try
          WmfCanvas.Draw(0,0,Bitmap__);
        finally
          WmfCanvas.Free;
        end;
        Wmf.SaveToFile(FileName__);
        Result:=True;
      finally
       Wmf.Free;
      end;
  except on E:Exception do Result:=Error(E.Message,Application.Title);
  end;
end;
}

function SetAlphaChannel( Alpha__ : Integer; Color__ : TColor ) : TColor;
begin
  Result := ( Color__ and $00ffffff ) + ( Alpha__ shl 24 );
end;

function  GetBitMapText(BitMap__:TBitMap; const Rect__:TRect; Clear__:Boolean; Strings__:TStrings; var ErrorText__:String):Boolean;
var i,W,H,Count,Index,Length,LineEndIndex,Row,PixelByteSize:Integer; Ch:Char; SkipCRLF:Boolean;
    Text:String; p:PByte;
begin
  Result:=False; ErrorText__:='';
  if Clear__ then Strings__.Clear;
  if (BitMap__<>nil) then with Rect__ do
     try
       PixelByteSize:=PIXEL_BYTE_SIZE[BitMap__.PixelFormat];
       if PixelByteSize=0 then
          ErrorText__:=UnsupportedImageFormatText
       else begin
          Result:=(Left<=Right) and (Top<=Bottom) and
                  (Left>=0) and (Top>=0) and (Right<=BitMap__.Width) and (Bottom<=BitMap__.Height);
          if Result then begin
             W:=RectWidth(Rect__); H:=RectHeight(Rect__);

             if (W>=0) and (H>=0) then begin
                Dec(W,2); Dec(H,3); // '-2' and '-3': a 1-pixel border around the text-area minus one more row at the bottom
                if (W>0) and (H>0) then begin
                   Length:=(W*H*PixelByteSize)-PixelByteSize-1; // one pixel with padding color ; additionally a null-character as string-terminator
                   SetLength(Text,Length);
                   if Length<>System.Length(Text) then
                      raise Exception.Create(TEXT_TASK_FAILED)
                   else begin
                      Count:=0; Index:=0; SkipCRLF:=False; LineEndIndex:=0;
                      for Row:=Succ(Top) to Bottom-3 do
                          if Index<Length then begin
                             p:=PByte(Cardinal(BitMap__.ScanLine[Row])+Cardinal(Succ(Left)*PixelByteSize));
                             for i:=0 to Pred(W*PixelByteSize) do begin
                                 if Index<Length then begin
                                    Ch:=Chr(Ord(p^)); Inc(Index);
                                    if        (Ch>=SPACE) or (Ch=Tab) then begin
                                              Inc(Count); Text[Count]:=Ch;
                                              SkipCRLF:=False;
                                              if Ch>SPACE then LineEndIndex:=Count;
                                              end
                                    else if   (Ch=CR) or (Ch=LF) then begin
                                              if SkipCRLF then SkipCRLF:=False
                                              else begin
                                                 Strings__.Add(Copy(Text,1,LineEndIndex));
                                                 Count:=0; SkipCRLF:=True; LineEndIndex:=0;
                                                 end;
                                              end
                                         else begin Length:=Index;
                                                    break; // quick-and-dirty exit from 'for' loop
                                              end;
                                    end;
                                 Inc(p);
                                 end;
                             end
                          else break; // quick-and-dirty exit from 'for' loop
                      if LineEndIndex<>0 then Strings__.Add(Copy(Text,1,LineEndIndex));
                      end;
                   end;
                end;
             end
          else ErrorText__:=RectangleIsOutOfBoundsText;
          end;
     except
       on E:Exception do begin Result:=False; ErrorText__:=E.Message;
                         end;
     end;

  if ErrorText__<>'' then Result:=False;
  if not Result and (ErrorText__='') then ErrorText__:=TEXT_TASK_FAILED;
end;

function  SetBitMapText(BitMap__:TBitMap; const Rect__:TRect; const Text__:String; var ErrorText__:String):Boolean;
var i,Index,Length,MaxLength,Row,Column,PixelByteSize:Integer; p:PByte;
begin
  Result:=False; ErrorText__:='';
  if (BitMap__<>nil) then with Rect__ do begin
     PixelByteSize:=PIXEL_BYTE_SIZE[BitMap__.PixelFormat];
     if PixelByteSize=0 then
        ErrorText__:=UnsupportedImageFormatText
     else begin
        Result:=(Left<=Right) and (Top<=Bottom) and
                (Left>=0) and (Top>=0) and (Right<=BitMap__.Width) and (Bottom<=BitMap__.Height);
        if Result then begin
           Length:=System.Length(Text__);
           MaxLength := MaxBitMapTextLength( BitMap__, Rect__ );
           Result := Length <= MaxLength;
           if not Result then begin
              ErrorText__ := TEXT_TEXT_TOO_LARGE;
              Length      := MaxLength; // write as much as possible
              end;

           if Length<=MaxLength then begin // one pixel with padding color ; additionally a null-character as string-terminator
              //BitMapDump(BitMap__);
              BitMap__.Canvas.Brush.Style:=bsSolid;
              BitMap__.Canvas.Brush.Color:=BitMap__.Canvas.Pixels[Right-2,Top];
              BitMap__.Canvas.FillRect(Rect(Succ(Left),Succ(Top),Pred(Right),Bottom-2));
              Index:=0;
              if Length<>0 then
                 for Row:=Succ(Top)  to Bottom-3 do begin
                     p:=PByte(Cardinal(BitMap__.ScanLine[Row])+Cardinal(Succ(Left)*PixelByteSize));
                     for Column:=Succ(Left) to Right-2 do
                         for i:=0 to Pred(PixelByteSize) do
                             if        Index<Length then begin
                                       Inc(Index); p^:=Ord(Text__[Index]); Inc(p);
                                       end
                             else if   Index=Length then begin
                                       if i=Pred(PixelByteSize) then Inc(Index); // 'True': finishing filling the last pixel with '0' components (e.g., rgb components in a 24-bit rgb image)
                                       p^:=0; Inc(p);
                                       end
                                  else break; // done

                     end;
              //BitMapDump(BitMap__);
              end
           else
              ErrorText__:=TEXT_TEXT_TOO_LARGE;
           end
        else ErrorText__:=RectangleIsOutOfBoundsText;
        end;
     end;

  if ErrorText__<>'' then Result:=False;
  if not Result and (ErrorText__='') then ErrorText__:=TEXT_TASK_FAILED;
end;

procedure Initialize;
//var i,j:Integer;
begin
//  for i:=Low(AlphaBlendTables) to High(AlphaBlendTables) do
//      MakeAlphaBlendTable(i,j,AlphaBlendTables[i]);
{$WARNINGS OFF}
  if not((PIXEL_BYTE_SIZE[pfDevice]=0) and
         (PIXEL_BYTE_SIZE[  pf8bit]=1) and
         (PIXEL_BYTE_SIZE[ pf16bit]=2) and
         (PIXEL_BYTE_SIZE[ pf24bit]=3) and
         (PIXEL_BYTE_SIZE[ pf32bit]=4)) then
         Halt;
{$WARNINGS ON}

end;

initialization
  Initialize;

finalization
  if StaticWorkBitMap     <> nil then StaticWorkBitMap.Free;
  if StaticScaleColFilter <> nil then FreeMem(StaticScaleColFilter);
  if StaticScaleRowFilter <> nil then FreeMem(StaticScaleRowFilter);
  if StaticWorkSpace      <> nil then FreeMem(StaticWorkSpace);

end.

