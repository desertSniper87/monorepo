unit Pict_;

interface

uses Windows,ExtCtrls,Graphics,Controls,jpeg,Misc_,BitMap_;

type
  TPict = class
  private
    fFileName:String;
    MaskingEnabled:Boolean;
  public
    AntiAliasing:TAntiAliasing;
    BitMap:TBitMap;
    BitMapRect:TRect;
    Color:TColor;
    FrameCount:Integer;
    KeepAspectRatio:Boolean;
    OrgBitMap:TBitMap;
    OrgBitMapWithFloodFilledMaskedPixels:TBitMap; // the original bitmap, but where all masked pixels have been filled with one of the nearest non-masked pixel values; this version is used for high-quality scaling because it avoids polluting the border pixels of an object with the background color
    MaskBitMap:TBitMap;
    MaskBitMapColor:TRGB;
    MaskBitMapPct:Integer;
    Masked:Boolean;
    MaskExteriorOnly:Boolean;
    Pict:TPicture;       // the picture loaded from disk
    ScaledRect:TRect;    // the real image after a 'Scale', as opposed to 'BitMapRect'
    SourceRect:TRect;    // selected part of the original image
    Transparent:Boolean; // if 'Masked' then treat the image as transparent instead of using a mask bitmap
    View:TImageView;     // tiled, centered, etc.
    Visible:Boolean;

    constructor Create;
    destructor  Destroy; override;

    procedure   Clear;
    procedure   Draw(X,Y:Integer; Canvas:TCanvas);
    procedure   DrawRect(X,Y:Integer; const SourceRect:TRect; Canvas:TCanvas);
    function    FrameRect(FrameIndex__:Integer):TRect;
    function    GiveBitMapAway:TBitMap;
    function    GiveMaskBitMapAway:TBitMap;
    function    LoadFromBitMap(Source:TBitMap):Boolean;
    function    LoadFromBitMapRect(BitMap__:TBitMap; R:TRect; const Offset:TPoint; Color:TColor):Boolean;
    function    LoadFromCompressedImage(Data__:Pointer; ByteSize__:Integer; const SourceBitMap__:TBitMap; const SourceRect__:TRect):Boolean;
    function    LoadFromFile(const FileName__:String):Boolean; virtual;
    function    LoadFromFileOrResource(const FileName__,ResourceType__:String):Boolean;
    function    LoadFromResource(const ResourceName,ResourceType:String):Boolean; virtual;
    procedure   MakeBitMap;
    function    MakeBlank(Width__,Height__:Integer; Color__:TRGBColor):Boolean;
    function    MakeFrameBitMap(FrameNo__,FrameCount__:Integer):Boolean;
    function    MakeMaskBitMap(MaskColor__:TRGB; MaskPct__:Integer; MaskExteriorOnly__:Boolean):Boolean;
    function    MakeTransparentBitMap(MaskColor__:TRGB; MaskPct__:Integer):Boolean;
    function    MakeOrgBitMapFromPict:Boolean;
    function    MaskBackgroundBitMap(BackgroundBitMap__:TBitMap):Integer;
    function    Resize(Width__,Height__:Integer):Boolean; virtual;
    function    ResizeFrames(Width__,Height__,FrameCount__:Integer; BackgroundBitMap__:TBitMap):Boolean;
    function    ResizeOriginal(Width__,Height__:Integer; AntiAliasing__:TAntiAliasing):Boolean;
    procedure   Scale(Width__,Height__:Integer;
                      View__:TImageView;
                      HorisontalBorder__,VerticalBorder__:Integer;
                      LowPriority__:Boolean;
                      var Cancel__:Boolean);
    function    SetView(View:TImageView; Width,Height:Integer; Color:TColor):Boolean;

    property    FileName:String      read fFileName write fFileName;
  end;

implementation

uses SysUtils,Forms,Classes,Dialogs,Math,
     SokUtil_,Text_,Res_,BitMap2_,GView_,Open1_,Main_,PNG_;
const
  MAX_SCALE_X=256;
  MAX_SCALE_Y=256;

constructor TPict.Create;
begin
  BitMap:=nil; OrgBitMap:=nil; OrgBitMapWithFloodFilledMaskedPixels:=nil; MaskBitMap:=nil; Pict:=nil;
  Clear;
  AntiAliasing:=aaNone; Color:=clWhite;
  Masked:=False; MaskExteriorOnly:=False; MaskingEnabled:=True;
  Transparent:=False; View:=ivFill; Visible:=True; KeepAspectRatio:=True;
  MakeBitMap;
end;

destructor TPict.Destroy;
begin
  Clear;
  Inherited Destroy;
end;

function TPict.FrameRect(FrameIndex__:Integer):TRect;
var W:Integer;
begin
  if   (FrameIndex__>=0) and (FrameIndex__<FrameCount) and (BitMap<>nil) then begin
       W:=BitMap.Width div FrameCount;
       Result:=Rect(FrameIndex__*W,0,Succ(FrameIndex__)*W,BitMap.Height);
       end
  else Result:=Rect(0,0,0,0);
end;

function TPict.LoadFromBitMap(Source:TBitMap):Boolean;
begin
  Result:=True;
  if (Pict=nil) or (Pict.BitMap<>Source) then begin
     Pict.Free; Pict:=nil; // loaded picture not used anymore
     end;
  OrgBitMap .Free; OrgBitMap         :=nil;
  OrgBitMapWithFloodFilledMaskedPixels.Free; OrgBitMapWithFloodFilledMaskedPixels:=nil;
  MaskBitMap.Free; MaskBitMap        :=nil;
  try if BitMap=nil then BitMap      :=TBitMap.Create;
      if Source.PixelFormat          = pf24Bit then
         BitMap.Assign(Source)
      else begin
         Result                      :=BitMapResize(BitMap,Source.Width,Source.Height);
         if Result then with BitMap.Canvas do begin
            CopyMode                 :=cmSrcCopy;
            Draw(0,0,Source);
            end;
         end;
      if BitMap .Width               = 0 then BitMap.Width :=1;
      if BitMap .Height              = 0 then BitMap.Height:=1;
      BitMapRect                     :=Rect(0,0,BitMap.Width,BitMap.Height);
      BitMap    .Canvas.Brush.Style  :=bsSolid;
      BitMap    .Canvas.CopyMode     :=cmSrcCopy;
      BitMap    .PixelFormat         :=pf24bit;
      if OrgBitMap=nil then OrgBitMap:=TBitMap.Create;
      OrgBitMap .Assign(BitMap);
      if BitMap.PixelFormat<>pf24bit then
         raise Exception.Create(FileName+':'+NL+NL+PixelFormatErrorText);
  except
    on E:Exception do
       begin Result:=Error(E.Message,Application.Title);
             Clear; MakeBitMap;
       end;
  end;
end;

function  TPict.LoadFromBitMapRect(BitMap__:TBitMap; R:TRect; const Offset:TPoint; Color:TColor):Boolean;
var W,H:Integer; Temp:TBitMap;
begin
  Result:=False;
  if BitMap__<>nil then begin
     if RectWidth (R)<=0 then R.Right :=BitMap__.Width -R.Left-Offset.X;
     if RectHeight(R)<=0 then R.Bottom:=BitMap__.Height-R.Top -Offset.Y;
     R.Right :=Min(BitMap__.Width ,R.Right);
     R.Bottom:=Min(BitMap__.Height,R.Bottom);
     R.Left  :=Min(R.Right ,Max(0,R.Left));
     R.Top   :=Min(R.Bottom,Max(0,R.Top));
     W       :=RectWidth (R)+2*Offset.X; // 'Offset' is the destination position
     H       :=RectHeight(R)+2*Offset.Y;

     if BitMapCreate(Temp,W,H) then
        try     if Offset.X+Offset.Y<>0 then begin
                   Temp.Canvas.Brush.Color:=Color;
                   Temp.Canvas.FillRect(Rect(0,0,Temp.Width,Temp.Height));
                   end;
                Temp.Canvas.CopyRect(Rect(Offset.X,Offset.Y,Offset.X+RectWidth(R),Offset.Y+RectHeight(R)),BitMap__.Canvas,R);
                Result:=LoadFromBitMap(Temp);
        finally Temp.Free;
        end;
     end;
end;

function TPict.LoadFromCompressedImage(Data__:Pointer; ByteSize__:Integer; const SourceBitMap__:TBitMap; const SourceRect__:TRect):Boolean;
begin
  Result:=True;
  Pict.Free; Pict:=nil; // loaded picture not used anymore
  OrgBitMap  .Free; OrgBitMap :=nil;
  OrgBitMapWithFloodFilledMaskedPixels.Free; OrgBitMapWithFloodFilledMaskedPixels:=nil;
  MaskBitMap .Free; MaskBitMap:=nil;
  try if BitMap=nil then BitMap:=TBitMap.Create;
      if   LoadBitMapFromCompressedImage(Data__,ByteSize__,SourceBitMap__,SourceRect__,BitMap) then begin
           if OrgBitMap=nil then OrgBitMap:=TBitMap.Create;
           OrgBitMap.Assign(BitMap);
           if BitMap.PixelFormat<>pf24bit then
              raise Exception.Create(FileName+':'+NL+NL+PixelFormatErrorText);
           end
      else raise Exception.Create(TEXT_TASK_FAILED);
  except
    on E:Exception do
       begin Result:=Error(E.Message,Application.Title);
             Clear; MakeBitMap;
       end;
  end;
end;

function TPict.LoadFromFileOrResource(const FileName__,ResourceType__:String):Boolean;
var ExpandedFileName:String;

  function LoadFromFile(const FileName:String; Pict:TPicture):Boolean;
  begin
    try Pict.LoadFromFile(FileName);
        Result:=True;
    except on E:Exception do
           if   IsPNGFile(FileName) and
                MainForm.PNGImageLoader.LoadFromFile(FileName,Pict) then
                Result:=True
           else raise;
    end;
  end;

  function LoadFromResource(const ResourceName,ResourceType:String; Pict:TPicture):Boolean;
  var M:TMemoryStream;
  begin
    M:=MemoryStreamLoadFromResource(ResourceName,ResourceType);
    if M<>nil then
       try     if ResourceType=RC_JPG then Pict.Graphic:=TJPEGImage.Create; // caution: only 'jpg' images are handled
               Pict.Graphic.LoadFromStream(M); // no exception handling here; the caller takes care of that
               Result:=True;
       finally M.Free;
       end
    else raise Exception.Create(Format(ResourceNotFoundText__,[ResourceName,ResourceType]));
  end;

begin // LoadFromFileOrResource
  Result:=True;
  Pict      .Free; Pict      :=nil;
  OrgBitMap .Free; OrgBitMap :=nil;
  OrgBitMapWithFloodFilledMaskedPixels.Free; OrgBitMapWithFloodFilledMaskedPixels:=nil;
  MaskBitMap.Free; MaskBitMap:=nil;

  if   ResourceType__='' then
       ExpandedFileName:=ExpandFileName(FileName__)
  else ExpandedFileName:=FileName__;

  try     Pict:=TPicture.Create;
          if ((ResourceType__ ='') and (FileExists(ExpandedFileName) or FileHasMoved(ExpandedFileName)) and LoadFromFile(ExpandedFileName,Pict))
             or
             ((ResourceType__<>'') and LoadFromResource(FileName__,ResourceType__,Pict)) then begin
             FileName:=ExpandedFileName;
             if Pict.Graphic is TBitMap then begin
                Result:=LoadFromBitMap(Pict.BitMap);
                Pict.Free; Pict:=nil;

                if Result and
                   (BitMap<>nil) and
                   (RectWidth (SourceRect)<>0) and
                   (RectHeight(SourceRect)<>0) then begin
                   if OrgBitMap=nil then OrgBitMap:=TBitMap.Create;
                   Result:=BitMapExtractRect(BitMap,SourceRect) and
                           (OrgBitMap<>nil);
                   if Result then OrgBitMap.Assign(BitMap);
                   end;
                end
             else ; // not a simple bitmap: keep the 'Pict'
             end
          else raise Exception.Create(Format(FileNotFoundText__,[ExpandedFileName]));
  except
    on E:Exception do
       begin Error(E.Message,Application.Title);
             Clear; MakeBitMap; Result:=False;
       end;
  end;
end;

function TPict.LoadFromFile(const FileName__:String):Boolean;
begin
  Result:=LoadFromFileOrResource(FileName__,'');
end;

function  TPict.LoadFromResource(const ResourceName,ResourceType:String):Boolean;
begin
  Result:=LoadFromFileOrResource(ResourceName,ResourceType);
end;

procedure TPict.Clear;
begin
  BitMap    .Free; BitMap    :=nil;
  OrgBitMap .Free; OrgBitMap :=nil;
  OrgBitMapWithFloodFilledMaskedPixels.Free; OrgBitMapWithFloodFilledMaskedPixels:=nil;
  MaskBitMap.Free; MaskBitMap:=nil;
  Pict      .Free; Pict      :=nil;
  FillChar(BitMapRect,SizeOf(BitMapRect),0);
  FillChar(SourceRect,SizeOf(SourceRect),0);
  FileName:=''; FrameCount:=0;
end;

function    TPict.MakeFrameBitMap(FrameNo__,FrameCount__:Integer):Boolean;
begin
  if   ((OrgBitMap<>nil) or MakeOrgBitMapFromPict) and
       (OrgBitMap.Width>=FrameCount__) and
       (FrameNo__>=0) and (FrameNo__<FrameCount__) and
       BitMapResize(BitMap,OrgBitMap.Width div FrameCount__,OrgBitMap.Height) then
       with BitMap do with Canvas do begin
         CopyMode:=cmSrcCopy;
         CopyRect(Rect(0,0,Width,Height),OrgBitMap.Canvas,CellToRect(FrameNo__,0,Width,Height));
         Result:=True;
         end
  else Result:=False;
end;

function    TPict.MakeMaskBitMap(MaskColor__:TRGB; MaskPct__:Integer; MaskExteriorOnly__:Boolean):Boolean;
begin
  MaskBitMap.Free; MaskBitMap:=nil;
  MaskBitMapColor :=MaskColor__;
  MaskBitMapPct   :=MaskPct__;
  MaskExteriorOnly:=MaskExteriorOnly__;
  if   MaskExteriorOnly then
       Result:=BitMapCreateMaskMaskingExteriorOnly(BitMap,MaskBitMap,MaskColor__,MaskPct__)
  else Result:=BitMapCreateMask(BitMap,MaskBitMap,MaskColor__,MaskPct__);
end;

function    TPict.MakeTransparentBitMap(MaskColor__:TRGB; MaskPct__:Integer):Boolean;
begin
  MaskBitMap.Free; MaskBitMap:=nil;
  Result:=BitMapMakeTransparent(BitMap,MaskColor__,MaskPct__);
end;

function    TPict.MaskBackgroundBitMap(BackgroundBitMap__:TBitMap):Integer;
var oMaskBitMap:TBitMap;
begin
  Result:=0; oMaskBitMap:=MaskBitMap;
  if (BitMap<>nil) and (BackgroundBitMap__<>nil) then
     try if BitMapResize(MaskBitMap,BitMap.Width,BitMap.Height) then
            Result:=BitMapMaskBackground(BitMap,MaskBitMap,BackgroundBitMap__,MaskBitMapPct,Masked);
     finally
       if (Result=0) and (oMaskBitMap=nil) then begin
          // 'MaskBitMap' was created by calling 'BitMapResize' above;
          // drop it now that it turned out not to be used
          MaskBitMap.Free;
          MaskBitMap:=nil;
          end;
     end;
end;

function    TPict.Resize(Width__,Height__:Integer):Boolean;
var W1,H1,W2,H2,X,Y:Integer; Cancel:Boolean; MaskColor:TColor;
    p,q:PRGBVector; Temp1:TBitMap;
begin
  Result:=BitMapResize(BitMap,Width__,Height__);
  if Result then begin
     BitMapRect:=Rect(0,0,BitMap.Width,BitMap.Height);
     W1:=BitMap.Width;
     H1:=BitMap.Height;
     if   OrgBitMap<>nil then
          begin W2:=OrgBitMap.Width; H2:=OrgBitMap.Height; end
     else begin W2:=W1; H2:=H1; end;
     if (W1<>0) and (H1<>0) and (W2<>0) and (H2<>0) then begin
        BitMap.Canvas.CopyMode:=cmSrcCopy;
        if (W1=W2) and (H1=H2) then
           ScaledRect:=BitMapRect
        else with BitMap.Canvas do begin
           H2:=MulDiv(H2,W1,W2); W2:=W1;
           if H2>H1 then begin W2:=MulDiv(W2,H1,H2); H2:=H1; end;

           if   Masked or (MaskBitMap<>nil) then
                Brush.Color:=RGBToColor(MaskBitMapColor)
           else Brush.Color:=Self.Color;
           FillRect(BitMapRect);

           if not KeepAspectRatio then begin W2:=W1; H2:=H1; end;

           ScaledRect.Left  :=(W1-W2) div 2;
           ScaledRect.Top   :=(H1-H2) div 2;
           ScaledRect.Right :=W1-ScaledRect.Left;
           ScaledRect.Bottom:=H1-ScaledRect.Top;
           end;

        if   (not Masked) and (MaskBitMap=nil) and (OrgBitMap<>nil) and (OrgBitMap.Width=RectWidth(ScaledRect)) and (OrgBitMap.Height=RectHeight(ScaledRect)) then
             BitMap.Canvas.Draw((W1-W2) div 2,(H1-H2) div 2,OrgBitMap)
        else if   (AntiAliasing=aaFilter) and
                  (W1*H1<MAX_SCALE_X*MAX_SCALE_Y) and
                  BitMapCreate(Temp1,RectWidth(ScaledRect),RectHeight(ScaledRect)) then
                  try
                    if   OrgBitMap=nil then MakeOrgBitMapFromPict;
                    if   Masked {or (MaskBitMap<>nil)} then MaskColor:=RGBToColor(MaskBitMapColor)
                    else MaskColor:=clBlack;
                    if   OrgBitMap<>nil then begin
                         BitMapScale(Temp1,OrgBitMap,5,False,Masked or (MaskBitMap<>nil),MaskColor,MaskBitMapPct,ivStretch,0,0,ScaledRect,Cancel);
                         BitMap.Canvas.Draw((W1-W2) div 2,(H1-H2) div 2,Temp1);
                         end;
                  finally
                    Temp1.Free;
                  end
        else if Pict<>nil then
                BitMap.Canvas.StretchDraw(ScaledRect,Pict.Graphic)
        else if OrgBitMap<>nil then
                BitMap.Canvas.StretchDraw(ScaledRect,OrgBitMap);

        if AntiAliasing=aaBilinear then begin
           if   Masked or (MaskBitMap<>nil) then MaskColor:=RGBToColor(MaskBitMapColor)
           else MaskColor:=clBlack;
           BitMapBilinearAntiAliasing(BitMap,Masked or (MaskBitMap<>nil),MaskColor,MaskBitMapPct);
           end;

        if (Masked or (MaskBitMap<>nil)) and MaskingEnabled then
           if   Transparent then
                MakeTransparentBitMap(MaskBitMapColor,MaskBitMapPct)
           else MakeMaskBitMap       (MaskBitMapColor,MaskBitMapPct,MaskExteriorOnly);

        if //False and
           Masked and
           (AntiAliasing=aaFilter) and
           (W1*H1<MAX_SCALE_X*MAX_SCALE_Y) and
           Assigned(OrgBitMap) then begin
           // high-quality scaling of a masked image requires having a
           // version of the image where the masked pixels first have been
           // filled with colors from the nearest neighbor unmasked pixels;
           // otherwise, scaling pollutes the pixels on the border of the
           // object with background color artifacts, in effect giving the
           // object an outline;
           // note that the mask bitmap has been created based on the
           // not-floodfilled original bitmap, and that the mask bitmap
           // is left as it is;
           if not Assigned(OrgBitMapWithFloodFilledMaskedPixels) then begin
              Result:=Result and
                      BitMapCreate(OrgBitMapWithFloodFilledMaskedPixels,OrgBitMap.Width,OrgBitMap.Height) and
                      BitMapFloodFillMaskedPixels(OrgBitMapWithFloodFilledMaskedPixels,OrgBitMap,MaskBitMapColor,MaskBitMapPct);
              end;
           Result:=Result and BitMapCreate(Temp1,RectWidth(ScaledRect),RectHeight(ScaledRect));
           if Result then
              try     BitMapScale(Temp1,OrgBitMapWithFloodFilledMaskedPixels,5,False,False,clBlack,0,ivStretch,0,0,ScaledRect,Cancel);
                      if not  MaskingEnabled then // 'True': a mask bitmap hasn't been created yet; do it now; note that the mask is based on the scaled image in 'BitMap', not the flood-filled version in 'Temp1'
                         if   Transparent then
                              MakeTransparentBitMap(MaskBitMapColor,MaskBitMapPct)
                         else MakeMaskBitMap       (MaskBitMapColor,MaskBitMapPct,MaskExteriorOnly);
                      BitMap.Canvas.Draw((W1-W2) div 2,(H1-H2) div 2,Temp1);
                      if (RGBToColor(MaskBitMapColor)=clBlack) or MaskingEnabled then
                         BitMapApplyMask(BitMap,MaskBitMap)
                      else // the mask color is non-black; do the masking manually to ensure that masked pixels get the mask color
                         if Assigned(MaskBitMap) and
                            (BitMap.PixelFormat=pf24bit) and (MaskBitMap.PixelFormat=pf24bit) then begin
                            for Y:=0 to Pred(BitMap.Height) do begin
                                p:=BitMap    .ScanLine[Y];
                                q:=MaskBitMap.ScanLine[Y];
                                for X:=0 to Pred(BitMap.Width) do with q^[X] do
                                    if (r=255) and (g=255) and (b=255) then
                                       p^[X]:=MaskBitMapColor;
                                end;
                            end;
                      if not MaskingEnabled then begin // 'True: the caller didn't request a mask bitmap; destroy the one which was created a few lines further up
                         MaskBitMap.Free; MaskBitMap:=nil;
                         // destroy the mask bitmap; it was only created
                         // to help the high-quality scaling; it must not
                         // exist after the function returns because
                         // there are several places in the program
                         // where a non-nil mask bitmap signals masking,
                         // just as if the 'Masked' flag has been set
                         end;
              finally Temp1.Free;
              end;
           end;
        end;

     if not Result then begin
        BitMap.Canvas.Brush.Color:=Color;
        BitMap.Canvas.FillRect(BitMapRect);
        OrgBitMapWithFloodFilledMaskedPixels.Free; OrgBitMapWithFloodFilledMaskedPixels:=nil;
        end;
     end;
end;

function    TPict.ResizeFrames(Width__,Height__,FrameCount__:Integer; BackgroundBitMap__:TBitMap):Boolean;
var FrameNo:Integer; oMaskingEnabled:Boolean; R0:TRect;
    B1,B2,B3,B4,oOrgBitMap,oOrgBitMapWithFloodFilledMaskedPixels:TBitMap;
begin
  Result:=False;
  FrameCount:=0;
  if (FrameCount__<=1) then begin
     Result:=Resize(Width__,Height__);
     if Result and (FrameCount__>0) then FrameCount:=FrameCount__;
     end
  else if OrgBitMap<>nil then begin
          oOrgBitMap:=OrgBitMap;
          oOrgBitMapWithFloodFilledMaskedPixels:=OrgBitMapWithFloodFilledMaskedPixels;
          oMaskingEnabled:=MaskingEnabled;
          B1:=nil; B2:=nil; B3:=nil; B4:=nil;
          try
            // antialiasing requires each frame to be resized
            // individually, otherwise they contaminate each other
            if BitMapCreate(B1,FrameCount__*Width__,Height__) and
               BitMapCreate(B2,OrgBitMap.Width div FrameCount__,OrgBitMap.Height) and
               BitMapCreate(B3,B2.Width,B2.Height) then
               with B1 do with Canvas do begin
                 OrgBitMap:=B2;
                 OrgBitMapWithFloodFilledMaskedPixels:=nil;
                 R0:=Rect(0,0,OrgBitMap.Width,OrgBitMap.Height);
                 MaskingEnabled:=False;

                 for FrameNo:=0 to Pred(FrameCount__) do begin
                     // copy frame to original image
                     OrgBitMap.Canvas.CopyRect(R0,oOrgBitMap.Canvas,CellToRect(FrameNo,0,R0.Right,R0.Bottom));
                     if   oOrgBitMapWithFloodFilledMaskedPixels<>nil then begin
                          OrgBitMapWithFloodFilledMaskedPixels:=B3;
                          OrgBitMapWithFloodFilledMaskedPixels.Canvas.CopyRect(R0,oOrgBitMapWithFloodFilledMaskedPixels.Canvas,CellToRect(FrameNo,0,R0.Right,R0.Bottom));
                          end;
                     // resize the image as if there was one frame only
                     if Resize(Width__,Height__) and
                        (BitMap<>nil) then begin // and finally, save the resized image as this frame
                        if Assigned(BackgroundBitMap__) and (not Masked) and oMaskingEnabled then // 'not Masked': the frame hasn't been masked with a background color; use the background bitmap 'BackgroundBitMap__' for masking
                           MaskBackgroundBitMap(BackgroundBitMap__);
                        B1.Canvas.CopyRect(CellToRect(FrameNo,0,Width__,Height__),BitMap.Canvas,Rect(0,0,Width__,Height__));
                        Inc(FrameCount);
                        if (oOrgBitMapWithFloodFilledMaskedPixels= nil) and
                           ( OrgBitMapWithFloodFilledMaskedPixels<>nil) then begin // 'True': a new flood-filled version of the original bitmap has been created
                           if (B4=nil) and (FrameNo=0) then BitMapCreate(B4,oOrgBitMap.Width,oOrgBitMap.Height);
                           if B4<>nil then B4.Canvas.CopyRect(CellToRect(FrameNo,0,R0.Right,R0.Bottom),OrgBitMapWithFloodFilledMaskedPixels.Canvas,Rect(0,0,R0.Right,R0.Bottom));
                           OrgBitMapWithFloodFilledMaskedPixels.Free; OrgBitMapWithFloodFilledMaskedPixels:=nil;
                           end;
                        end;
                     end;
                end;
          finally
            OrgBitMap:=oOrgBitMap; OrgBitMapWithFloodFilledMaskedPixels:=oOrgBitMapWithFloodFilledMaskedPixels;
            MaskingEnabled:=oMaskingEnabled;
            if FrameCount=FrameCount__ then begin
               Result:=True;
               BitMap.Free; BitMap:=B1; B1:=nil;
               if B4<>nil then begin // 'True': a new flood-filled version of the original bitmap has been created
                  OrgBitMapWithFloodFilledMaskedPixels.Free;
                  OrgBitMapWithFloodFilledMaskedPixels:=B4;
                  B4:=nil;
                  end;
               if Masked or (MaskBitMap<>nil) then
                  if   Transparent then
                       MakeTransparentBitMap(MaskBitMapColor,MaskBitMapPct)
                  else MakeMaskBitMap       (MaskBitMapColor,MaskBitMapPct,MaskExteriorOnly);
                  //BitMapDump(BitMap);
                  //BitMapDump(MaskBitMap);
               end
            else begin
               FrameCount:=0;
               OrgBitMapWithFloodFilledMaskedPixels.Free; OrgBitMapWithFloodFilledMaskedPixels:=nil;
               end;
            B1.Free; B2.Free; B3.Free; B4.Free;
          end;
          end;
end;

function  TPict.ResizeOriginal(Width__,Height__:Integer; AntiAliasing__:TAntiAliasing):Boolean;
var oMaskingEnabled:Boolean; oAntiAliasing:TAntiAliasing; oKeepAspectRatio:Boolean;
begin
  oMaskingEnabled:=MaskingEnabled;
  oKeepAspectRatio:=KeepAspectRatio; oAntiAliasing:=AntiAliasing;

  try
    MaskingEnabled:=False; AntiAliasing:=AntiAliasing__; KeepAspectRatio:=False;

    if    (OrgBitMap<>nil) or MakeOrgBitMapFromPict then with OrgBitMap do
          if (Width=Width__) and (Height=Height__) then
             Result:=True
          else begin
             if   Resize(Width__,Height__) then begin
                  BitMapSwap(BitMap,OrgBitMap); Result:=True;
                  end
             else Result:=False;
             end
    else  Result:=False;
  finally MaskingEnabled:=oMaskingEnabled;
          AntiAliasing:=oAntiAliasing; KeepAspectRatio:=oKeepAspectRatio;
          OrgBitMapWithFloodFilledMaskedPixels.Free; OrgBitMapWithFloodFilledMaskedPixels:=nil;
  end;
end;

procedure TPict.Scale(Width__,Height__:Integer;
                      View__:TImageView;
                      HorisontalBorder__,VerticalBorder__:Integer;
                      LowPriority__:Boolean;
                      var Cancel__:Boolean);
var Color:TColor;
begin // resizes the bitmap with a better image quality than 'StretchDraw'
  if OrgBitMap=nil then MakeOrgBitMapFromPict;

  if (BitMap<>nil) and (OrgBitMap<>nil) then
     try
       BitMap.Width :=Max(1,Width__);
       BitMap.Height:=Max(1,Height__);
       BitMapRect:=Rect(0,0,BitMap.Width,BitMap.Height);
       if   Masked or (MaskBitMap<>nil) then Color:=RGBToColor(MaskBitMapColor)
       else Color:=clBlack;
       BitMapScale(BitMap,OrgBitMap,5,LowPriority__,
         Masked or (MaskBitMap<>nil),Color,MaskBitMapPct,
         View__,HorisontalBorder__,VerticalBorder__,ScaledRect,Cancel__);

       //if Self=MainForm.Board.Picture[ptPlayer] then
       //   BitMap.SaveToFile('c:\compiler\delphi4\pg\sokoban\'+IntToStr(RunNo)+'-Scale.bmp');

       if Masked or (MaskBitMap<>nil) then
          MakeMaskBitMap(MaskBitMapColor,MaskBitMapPct,MaskExteriorOnly);
     except on E:Exception do begin
               Error(E.Message,Application.Title); Clear; MakeBitMap;
               end;
     end;
end;

function TPict.SetView(View:TImageView; Width,Height:Integer; Color:TColor):Boolean;
var W,H,X,Y,X1:Integer; b:Boolean; SourceRect,DestRect,R,R1:TRect; C:TCanvas;
    p:PRGBVector; RGBColor:TRGB;
    Temp:TBitMap;
    GameViewer:TGameViewer;

  procedure ClipRect(var SourceRect,DestRect:TRect);
  begin
    if DestRect.Left<R.Left then begin
       Inc(SourceRect.Left,(R.Left-DestRect.Left));
       DestRect.Left:=R.Left;
       end;
    if DestRect.Top <R.Top  then begin
       Inc(SourceRect.Top ,(R.Top-DestRect.Top));
       DestRect.Top:=R.Top;
       end;
    if DestRect.Right>R.Right then begin
       Dec(SourceRect.Right,(DestRect.Right-R.Right));
       DestRect.Right:=R.Right;
       end;
    if DestRect.Bottom>R.Bottom then begin
       Dec(SourceRect.Bottom,(DestRect.Bottom-R.Bottom));
       DestRect.Bottom:=R.Bottom;
       end;
  end;

  procedure CenterRect(var SourceRect,DestRect:TRect);
  var x,y:Integer;
  begin
    DestRect:=R;
    x:=RectWidth (DestRect)-RectWidth (SourceRect);
    y:=RectHeight(DestRect)-RectHeight(SourceRect);
    if   x>=0 then with DestRect do begin
         Inc(Left,x div 2); Dec(Right,x-(x div 2));
         end
    else with SourceRect do begin
         Inc(Left,(-x) div 2); Dec(Right,(-x)-((-x) div 2));
         end;
    if   y>=0 then with DestRect do begin
         Inc(Top,y div 2); Dec(Bottom,y-(y div 2));
         end
    else with SourceRect do begin
         Inc(Top,(-y) div 2); Dec(Bottom,(-y)-((-y) div 2));
         end;
  end;

  procedure DoAntialiasing;
  var Color:TColor;
  begin
    if   Masked or (MaskBitMap<>nil) then Color:=RGBToColor(MaskBitMapColor)
    else Color:=clBlack;
    if (AntiAliasing=aaBilinear) or (Antialiasing=aaFilter) then // 'aaFilter' not supported here because it takes too long time; use 'aaBilinear' instead
       BitMapBilinearAntiAliasing(BitMap,Masked or (MaskBitMap<>nil),Color,MaskBitMapPct);
  end;

begin // SetView
  Self.View:=View; Self.Color:=Color;

  if   BitMap<>nil then
       Result:=BitMapResize(BitMap,Width,Height)
  else Result:=BitMapCreate(BitMap,Width,Height);
  BitMapRect:=Rect(0,0,BitMap.Width,BitMap.Height);

  if OrgBitMap=nil then MakeOrgBitMapFromPict;

  Result:=Result and (OrgBitMap<>nil);
  if Result and
     ((OrgBitMap.Width<=1) or (OrgBitMap.Height<=1)) then
     View:=ivNone; // if original bitmap is empty then create a blank bitmap

  if Result then begin
     R:=BitMapRect; C:=BitMap.Canvas; C.Copymode:=cmSrcCopy;

     case View of
       ivNone        : begin if      (OrgBitMap<>nil) and
                                     BitMapResize(OrgBitMap,Self.BitMap.Width,Self.BitMap.Height) then begin
                                     OrgBitMap.Canvas.Brush.Color:=Color;
                                     OrgBitMap.Canvas.FillRect(R); // this is ok, because the image is always loaded again if the view is modified
                                     C.CopyRect(R,OrgBitMap.Canvas,R);
                                     end
                             else    Result:=False;
                       end;
       ivFill        : begin if      Self.Pict<>nil then begin
                                     W:=Self.Pict.Width;
                                     H:=Self.Pict.Height;
                                     end
                             else if OrgBitMap<>nil then begin
                                     W:=Self.OrgBitMap.Width;
                                     H:=Self.OrgBitMap.Height;
                                     end
                             else    begin W:=1; H:=1; end;

                             X       :=Max(1,Width);
                             Y       :=Max(1,Height);

                             H       :=MulDiv(H,X,W); W:=X;
                             if      H<Y then begin
                                     W:=MulDiv(W,Y,H); H:=Y;
                                     end;
                             DestRect.Left  :=R.Left-((W-Width ) div 2);
                             DestRect.Top   :=R.Top -((H-Height) div 2);
                             DestRect.Right :=DestRect.Left+W;
                             DestRect.Bottom:=DestRect.Top +H;

                             if      Self.Pict<>nil then begin
                                     C.StretchDraw(DestRect,Self.Pict.Graphic);
                                     DoAntialiasing;
                                     end
                             else if Self.OrgBitMap<>nil then
                                     if (Antialiasing=aaFilter) //and
                                        //(not OpenForm.GameViewer.IsBoardBackgroundOrReverseModeBackground(Self)) then // kludge: scaling is slow; don't run it while the user is browsing levels
                                        then
                                        Scale(Width,Height,View,0,0,False,b)
                                     else begin
                                        C.StretchDraw(DestRect,Self.OrgBitMap);
                                        DoAntialiasing;
                                        end;
                       end;

       ivStretch     : begin if      Self.Pict<>nil then begin
                                     C.StretchDraw(R,Self.Pict.Graphic);
                                     DoAntialiasing;
                                     end

                             else if OrgBitMap<>nil then
                                     if (Antialiasing=aaFilter) then //and
                                        //(not OpenForm.GameViewer.IsBoardBackgroundOrReverseModeBackground(Self)) then // kludge: scaling is slow; don't run it while the user is browsing levels
                                        Scale(Width,Height,View,0,0,False,b)
                                     else begin
                                        C.StretchDraw(R,Self.OrgBitMap);
                                        DoAntialiasing;
                                        end;
                       end;
       ivCenter      : begin C.Brush.Color:=Color;
                             C.FillRect(R);

                             if (FileName<>'') and (OrgBitMap<>nil) then begin
                                SourceRect:=Rect(0,0,OrgBitMap.Width,OrgBitMap.Height);
                                DestRect  :=R;
                                CenterRect(SourceRect,DestRect);
                                C.CopyRect(DestRect,OrgBitMap.Canvas,SourceRect);
                                end
                             else FillChar(DestRect,SizeOf(DestRect),0);

                             // fill the part not covered by the image
                             RGBColor:=ColorToRGB(Color);
                             for Y:=Max(0,R.Top) to
                                    Min(R.Bottom,Pred(BitMap.Height)) do begin
                                 p:=BitMap.ScanLine[Y];
                                 if (Y<DestRect.Top) or (Y>=DestRect.Bottom) then begin
                                    for X:=R.Left to Pred(R.Right) do
                                        if (MaskBitMap=nil) or (p[X].r=255) then
                                           p[X]:=RGBColor;
                                    end
                                 else begin
                                    for X:=R.Left to Pred(DestRect.Left) do
                                        if (MaskBitMap=nil) or
                                           (p[X].r=255) then p[X]:=RGBColor;
                                    for X:=DestRect.Right to Pred(R.Right) do
                                        if (MaskBitMap=nil) or (p[X].r=255) then
                                           p[X]:=RGBColor;
                                    end;
                                 end;
                       end;
       ivTile        : begin if      OrgBitMap<>nil then begin
                                     W:=OrgBitMap.Width;
                                     H:=OrgBitMap.Height;
                                     X:=R.Left+((Width -W) div 2);
                                     Y:=R.Top +((Height-H) div 2);

                                     if (W>0) and (H>0) then begin
                                        while X>R.Left do Dec(X,W);
                                        while Y>R.Top  do Dec(Y,H);
                                        repeat
                                          X1:=X;
                                          repeat
                                            SourceRect:=Rect(0,0,OrgBitMap.Width,OrgBitMap.Height);
                                            DestRect  :=Rect(X1,Y,X1+OrgBitMap.Width,Y+OrgBitMap.Height);
                                            ClipRect(SourceRect,DestRect);
                                            if (SourceRect.Right>SourceRect.Left) and
                                               (SourceRect.Bottom>SourceRect.Top) then
                                               C.CopyRect(DestRect,OrgBitMap.Canvas,SourceRect);
                                            Inc(X1,W);
                                          until  X1>=R.Right;
                                          Inc(Y,H);
                                        until Y>=R.Bottom;
                                        end;
                                     end;
                       end;
       ivFloorTile   : begin // draw a background with a tiled outer floor image
                             // properly scaled and aligned with the game board
                             // squares
                             GameViewer:=nil;
                             if      Assigned(MainForm) and
                                     Assigned(MainForm.GameViewer) and
                                     (Self=MainForm.GameViewer.Pictures[ptScreenBackground]) then
                                     GameViewer:=MainForm.GameViewer
                             else if Assigned(OpenForm) and
                                     Assigned(OpenForm.GameViewer) and
                                     (Self=OpenForm.GameViewer.Pictures[ptScreenBackground]) then begin
                                     GameViewer:=OpenForm.GameViewer;
                                     if   (OrgBitMap<>nil) and
                                          GameViewer.Initialized and
                                          (RectWidth( GameViewer.BoardRect)>0) and
                                          (RectHeight( GameViewer.BoardRect)>0) and
                                          (GameViewer.ColWidth>0) and
                                          (GameViewer.RowHeight>0) then // OK
                                     else GameViewer:=MainForm.GameViewer; // empty game viewer: display an empty board with the floor square size taken the main window
                                     end;

                             if GameViewer<>nil then begin // 'True': "Self", i.e., the image, is one of the screen background images
                                if      (OrgBitMap<>nil) and
                                        GameViewer.Initialized and
                                        (RectWidth( GameViewer.BoardRect)>0) and
                                        (RectHeight( GameViewer.BoardRect)>0) and
                                        (GameViewer.ColWidth>0) and
                                        (GameViewer.RowHeight>0) and
                                        BitMapCreate( Temp, GameViewer.ColWidth, GameViewer.RowHeight ) then
                                        try
                                          if BitMapScale( Temp, OrgBitMap, 5, False, False, clBlack, 0, ivNone, 0, 0, R1, b ) then begin
                                             W:=Temp.Width;
                                             H:=Temp.Height;
                                             X:=GameViewer.BoardRect.Left;
                                             Y:=GameViewer.BoardRect.Top;

                                             if (W>0) and (H>0) then begin
                                                while X>R.Left do Dec(X,W);
                                                while Y>R.Top  do Dec(Y,H);
                                                repeat
                                                  X1:=X;
                                                  repeat
                                                    SourceRect:=Rect(0,0,Temp.Width,Temp.Height);
                                                    DestRect  :=Rect(X1,Y,X1+Temp.Width,Y+Temp.Height);
                                                    ClipRect(SourceRect,DestRect);
                                                    if (SourceRect.Right>SourceRect.Left) and
                                                       (SourceRect.Bottom>SourceRect.Top) then
                                                       C.CopyRect(DestRect,Temp.Canvas,SourceRect);
                                                    Inc(X1,W);
                                                  until  X1>=R.Right;
                                                  Inc(Y,H);
                                                until Y>=R.Bottom;
                                                end;
                                             end
                                          else begin
                                             BitMap.Canvas.Brush.Color:=Self.Color;
                                             BitMap.Canvas.FillRect(BitMapRect);
                                             end;
                                        finally Temp.Free;
                                        end;
                                end
                             else // "Self" isn't a screen background image. just tile the image without scaling it to the game board floor size
                                try     SetView(ivTile,Width,Height,Color);
                                finally Self.View:=ivFloorTile;
                                end;
                       end;
          end; //case
     end;
end;

function  TPict.MakeBlank(Width__,Height__:Integer; Color__:TRGBColor):Boolean;
begin
  Result:=True; Self.Color:=Color__;
  Pict      .Free; Pict      :=nil;
  OrgBitMap .Free; OrgBitMap :=nil;
  MaskBitMap.Free; MaskBitMap:=nil;
  try FillChar(SourceRect,SizeOf(SourceRect),0);
      if BitMap=nil then BitMap      :=TBitMap.Create;
      BitMap    .Width               :=Width__;
      BitMap    .Height              :=Height__;
      BitMap.PixelFormat             :=pf24bit;
      BitMapRect                     :=Rect(0,0,BitMap.Width,BitMap.Height);

      BitMap.Canvas.Brush.Color      :=Color__;
      BitMap.Canvas.FillRect(BitMapRect);

      if OrgBitMap=nil then OrgBitMap:=TBitMap.Create;
      OrgBitMap.Assign(BitMap);
      if BitMap.PixelFormat<>pf24bit then
         raise Exception.Create(FileName+':'+NL+NL+PixelFormatErrorText);
  except
    on E:Exception do
       begin Result:=Error(E.Message,Application.Title); Clear; MakeBitMap;
       end;
  end;
end;

function  TPict.MakeOrgBitMapFromPict:Boolean;
var W,H:Integer;
begin
  Result:=True;
  OrgBitMap.Free; OrgBitMap:=nil;
  OrgBitMapWithFloodFilledMaskedPixels.Free; OrgBitMapWithFloodFilledMaskedPixels:=nil;
  try
    OrgBitMap:=TBitMap.Create;
    if Pict<>nil then begin
       BitMapClampSize(Pict.Width,Pict.Height,W,H);
       OrgBitMap .Width      :=W;
       OrgBitMap .Height     :=H;
       if   (OrgBitMap.Width=Pict.Width) and (OrgBitMap.Height=Pict.Height) then
            OrgBitMap .Canvas.Draw(0,0,Pict.Graphic)
       else OrgBitMap .Canvas.StretchDraw(Rect(0,0,OrgBitMap.Width,OrgBitMap.Height),Pict.Graphic);
       end;
    if OrgBitMap .Width       =0 then OrgBitMap.Width :=1;
    if OrgBitMap .Height      =0 then OrgBitMap.Height:=1;
    OrgBitMap    .PixelFormat:=pf24bit;
    if OrgBitMap.PixelFormat<>pf24bit then
       raise Exception.Create(FileName+':'+NL+NL+PixelFormatErrorText);
    if (RectWidth(SourceRect)<>0) and (RectHeight(SourceRect)<>0) then
       BitMapExtractRect(OrgBitMap,SourceRect);
  except
    on E:Exception do
       begin Result:=Error(E.Message,Application.Title); Clear; MakeBitMap;
       end;
  end;
end;

procedure TPict.MakeBitMap;
begin
  BitMap.Free; BitMap:=nil; BitMap_.BitMapCreate(BitMap,1,1);
end;

function  TPict.GiveBitMapAway:TBitMap;
begin
  if (Pict<>nil) and MakeOrgBitMapFromPict then
     BitMap.Assign(OrgBitMap);
  if BitMap .Width       =0 then BitMap.Width :=1;
  if BitMap .Height      =0 then BitMap.Height:=1;
  Result:=BitMap; BitMap:=nil;
end;

function  TPict.GiveMaskBitMapAway:TBitMap;
begin
  Result:=MaskBitMap; MaskBitMap:=nil;
end;

procedure TPict.Draw(X,Y:Integer; Canvas:TCanvas);
begin
  if MaskBitMap=nil then
     Canvas.CopyMode:=cmSrcCopy
  else begin
     Canvas.CopyMode:=cmSrcAnd;   // and-ing
     Canvas.Draw(X,Y,MaskBitMap);
     Canvas.CopyMode:=cmSrcPaint; // or-ing
     end;
  Canvas.Draw(X,Y,BitMap);
  Canvas.CopyMode:=cmSrcCopy;
end;

procedure TPict.DrawRect(X,Y:Integer; const SourceRect:TRect; Canvas:TCanvas);
var DestRect:TRect;
begin
  DestRect:=Rect(X,Y,X+RectWidth(SourceRect),Y+RectHeight(SourceRect));
  if MaskBitMap=nil then
     Canvas.CopyMode:=cmSrcCopy
  else begin
     Canvas.CopyMode:=cmSrcAnd;   // and-ing
     Canvas.CopyRect(DestRect,MaskBitMap.Canvas,SourceRect);
     Canvas.CopyMode:=cmSrcPaint; // or-ing
     end;
  Canvas.CopyRect(DestRect,BitMap.Canvas,SourceRect);
  Canvas.CopyMode:=cmSrcCopy;
end;

end.

