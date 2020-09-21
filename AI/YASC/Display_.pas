unit Display_; // Music Player Display

interface

uses Windows,Graphics,ExtCtrls,Misc_,IniFile_,Menu_,BitMap_,Pict_;

type
  TActivitiesButtonType = (abMandala,abImages,{abFireworks,}abFractals,abSettings);

  TDisplay=class
  private
    BaseAddr:Integer;
    Canvas:TCanvas;
    DestRect:TRect;
    FActivity:TActivity;
    FPSDisplayRect:TRect;
    FText:String;
    Height:Integer;
    MPlayerDisplayPanel:TPanel;
    MusicEnabled:Boolean;
    Pitch:Integer;
    TextBitMap:TBitMap;
    TextBitMapRect:TRect;
    TextDisplayRect:TRect;
    Width:Integer;
  protected
    function    GetFont:TFont;
//  function    GetImageFileName:String;
    procedure   SetActivity(Activity__:TActivity);
    procedure   SetFont(Font__:TFont);
    procedure   SetText(const Text__:String);
  public
    ActivitiesMenu
               :TActivitiesMenu;
    BitMaps    :TDisplayBitMapTower;
    BitMapRect :TRect;
    FastSpeed  :Boolean;
    Menu       :TMPlayerDisplayMenu;
    ShowFps    :Boolean;
    TitleAnimation
               :Boolean;
    TitleMode  :TMPlayerTitleMode;
    TransparencyPct:Integer;

    property    Activity:TActivity read FActivity write SetActivity;
    property    Font:TFont read GetFont write SetFont;
    property    Text:String read FText write SetText;

    procedure   ClearLayer(Layer:TDisplayLayer);
    constructor Create(Canvas__:TCanvas; MPlayerDisplayPanel__: TPanel);
    destructor  Destroy; override;
    procedure   Execute;
    procedure   Finalize;
    procedure   HideMenu(RemoveMenu:Boolean);
    function    Initialize(DestRect__: TRect):Boolean;
    procedure   InitializeActivity(MusicEnabled__:Boolean);
    function    LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    function    RestoreImageIfFading:Boolean;
    function    SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
    procedure   SetDefaultValues;
    procedure   Show;
    procedure   ShowImage;
    function    ShowImageSynchronized(const FileName:String; BitMap:TBitMap):Boolean;
    procedure   ShowMenu;
    procedure   ShowText;
    procedure   Write(const Rect:TRect; const s:String; Flags:Integer);
  end;

implementation

uses SysUtils,Classes,Controls,Forms,
     SokUtil_,Text_,Main_,MPlayer1_,Mandal1_,IView1_,Fractal_,FWorks_;

const
  MUSIC_PLAYER_DISPLAY_INIFILE_SECTION='MusicPlayerDisplay'; // don't localize

{ TDisplay }

constructor TDisplay.Create(Canvas__:TCanvas; MPlayerDisplayPanel__: TPanel);
begin
  Self.Canvas:=Canvas__;
  MPlayerDisplayPanel:=MPlayerDisplayPanel__;
  FillChar(BitMaps,SizeOf(BitMaps),0); Width:=0; Height:=0; TitleAnimation:=True;
  TextBitMap:=nil; FText:=''; Menu:=nil; MusicEnabled:=False; FastSpeed:=False;
  if Self.Canvas<>nil then Font:=Self.Canvas.Font;

  ActivitiesMenu:=nil;
  try    ActivitiesMenu:=TActivitiesMenu.Create(nil,MainForm.ActivitiesPanel,MainForm.ActivitiesPanel.Font.Size);
  except on E:Exception do begin ActivitiesMenu.Free; ActivitiesMenu:=nil; end;
  end;

  SetDefaultValues;
end;

destructor TDisplay.Destroy;
begin
  Finalize;
  ActivitiesMenu.Free;
  Inherited;
end;

function  TDisplay.Initialize(DestRect__: TRect):Boolean;
var i,W,H:Integer;
begin
  DestRect:=DestRect__;
//MakeAlphaBlendTable(TransparencyPct,TransparencyPct,AlphaBlendTable);

  W:=RectWidth(DestRect); H:=RectHeight(DestRect);
  Result:=(W=Width) and (H=Height);
  if not Result then begin
     Finalize; Result:=True;
     Width:=W; Height:=H; BitMapRect:=Rect(0,0,Width,Height);
     for i:=Low(BitMaps) to High(BitMaps) do
         if not BitMapCreate(BitMaps[i],Width,Height) then
            Result:=False;
     if  Result then begin
         BaseAddr:=Integer(BitMaps[Ord(Low(BitMaps))].ScanLine[0]);
         Pitch:=Integer(BitMaps[Ord(Low(BitMaps))].ScanLine[1])-BaseAddr;
         BitMaps[Ord(Low(BitMaps))].Canvas.Font.Assign(Font);
         if Mandala  <>nil then Mandala.Initialize;
         if Fireworks<>nil then with Fireworks do begin
            Canvas           :=BitMaps[Ord(Low(BitMaps))].Canvas;
            Rect             :=BitMapRect;
            TimerInterval    :=MPLAYER_FRAME_TIME_SLOW_MS;
            Reset;
            end;
         end;
     TextDisplayRect:=BitMapRect; // 'TextDisplayRect' is updated, when text is specified
     FPSDisplayRect :=Rect(1,H-14,Pred(W),H);

     Viewer1.Suspend;
     Viewer1.PictThread.InitializeBitMapInfo;
     IView1_.Data.Scale      :=True;  // kludge: ThreadData-slots should really have been properties
     IView1_.Data.ScaleWidth :=Width;
     IView1_.Data.ScaleHeight:=Height;
     end;

  if Screen.ActiveForm=MainForm then // kludge: 'OpenForm' calls 'Initialize' if fractals hasn't been shown yet
     if Result then begin
        Activity:=Activity; // to set 'Menu'
        if Self.Canvas<>nil then begin
           BitMaps[Ord(High(BitMaps))].Canvas.CopyRect(BitMapRect,Self.Canvas,DestRect);
           end;
        ClearLayer(TDisplayLayer(High(BitMaps)));
        if Activity=acImage then begin
           Viewer1.ShowImage(Viewer1.FileName);
           if PtInRect(MainForm.FormRectToScreenRect(MainForm.MPlayer.DisplayRect),Mouse.CursorPos) then
              ShowMenu;
           end
        else HideMenu(False);

        case Activity of
          acNone      :;
          acMandala   : begin Mandala.RestorePaletteAndPattern; // will not hurt, even if there is nothing to restore
                              Mandala.Resume;
                        end;
          acImage     : Viewer1 .ShowImage(Viewer1.FileName);
          acFractals  : Fractals.Show;
        end; // case

        Text:=Text; // update text-display region
        end
     else Finalize;
end;

procedure TDisplay.Finalize;
var i,j:Integer;
begin
  HideMenu(True);
  for i:=Low(BitMaps) to High(BitMaps) do
      if BitMaps[i]<>nil then begin
         for j:=Succ(i) to High(BitMaps) do // remove duplicates, if any
             if BitMaps[j]=BitMaps[i] then BitMaps[j]:=nil;
         BitMaps[i].Free; BitMaps[i]:=nil;
         end;

  TextBitMap.Free; TextBitMap:=nil;
end;

procedure TDisplay.SetDefaultValues;
begin
  ShowFps:=False;
  TitleMode:=tmAnimateBottom;
  TransparencyPct:=33;
  FActivity:=acMandala;

  with MPlayerDisplayPanel do begin
    Font.Name:='Arial'; Font.Size:=8; Font.Color:=clWhite;
    end;
  Font:=Font;

  if ActivitiesMenu<>nil then
     ActivitiesMenu.SetDefaultValues(clWhite,ActivitiesMenu.MenuPanel.Font.Size);
end;

function  TDisplay.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
var i:Integer;
begin
  TransparencyPct       :=Max(0,Min(100, IniFile.ReadInteger(MUSIC_PLAYER_DISPLAY_INIFILE_SECTION,'TransparencyPct',TransparencyPct)));
  i                     :=               IniFile.ReadInteger(MUSIC_PLAYER_DISPLAY_INIFILE_SECTION,'Activity',Ord(Activity));
  if (i>=Ord(Low(Activity))) and (i<=Ord(High(Activity))) then fActivity:=TActivity(i);
  ShowFps               :=               IniFile.ReadBool(MUSIC_PLAYER_DISPLAY_INIFILE_SECTION,'ShowFps',ShowFps);
  i                     :=               IniFile.ReadInteger(MUSIC_PLAYER_DISPLAY_INIFILE_SECTION,'TitleMode',Ord(TitleMode));
  if (i>=Ord(Low(TitleMode))) and (i<=Ord(High(TitleMode))) then TitleMode:=TMPlayerTitleMode(i);
  Result:=LoadFontFromIniFile(IniFile,MUSIC_PLAYER_DISPLAY_INIFILE_SECTION,'',MPlayerDisplayPanel.Font)
          and
          ((ActivitiesMenu=nil)
           or
           ActivitiesMenu.LoadSettingsFromIniFile(IniFile,ActivitiesMenuText[0])
          );
  Font:=Font;
end;

function  TDisplay.SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
begin
  IniFile.WriteInteger(MUSIC_PLAYER_DISPLAY_INIFILE_SECTION,'TransparencyPct',TransparencyPct);
  IniFile.WriteInteger(MUSIC_PLAYER_DISPLAY_INIFILE_SECTION,'Activity',Ord(Activity));
  IniFile.WriteBool(MUSIC_PLAYER_DISPLAY_INIFILE_SECTION,'ShowFps',ShowFps);
  IniFile.WriteInteger(MUSIC_PLAYER_DISPLAY_INIFILE_SECTION,'TitleMode',Ord(TitleMode));
  Result:=SaveFontToIniFile(IniFile,MUSIC_PLAYER_DISPLAY_INIFILE_SECTION,'',MPlayerDisplayPanel.Font)
          and
          ((ActivitiesMenu=nil)
           or
           ActivitiesMenu.SaveSettingsToIniFile(IniFile,ActivitiesMenuText[0])
          );
end;

procedure   TDisplay.ClearLayer(Layer:TDisplaylayer);
var i:Integer;
begin
  if Layer>=High(Layer) then Layer:=Pred(Layer);
  for i:=Ord(Layer) downto Ord(Low(BitMaps)) do
      if (BitMaps[i]<>nil) and (BitMaps[Succ(i)]<>nil) then
         BitMaps[i].Canvas.CopyRect(BitMapRect,BitMaps[Succ(i)].Canvas,BitMapRect);
end;

procedure TDisplay.Show;
begin
  if (BitMaps[Ord(Low(BitMaps))]<>nil) and
     (Self.Canvas<>nil) and
     (MainForm.MPlayer.Visible) then
     Self.Canvas.CopyRect(DestRect,BitMaps[Ord(Low(BitMaps))].Canvas,BitMapRect);
end;

procedure TDisplay.ShowImage;
begin
  if (Menu<>nil) and Menu.Visible then ShowMenu
  else begin
    ClearLayer(Pred(dlImage));
    ShowText;
    end;
end;

procedure TDisplay.ShowMenu;
//var Point:TPoint;
begin
  if Menu<>nil then begin //with Point do begin
     //Point:=Mouse.CursorPos;
     //Menu.ItemIndex:=Menu.ItemAtPos(X-MainForm.Left-MainForm.MPlayer.DisplayRect.Left,Y-MainForm.Top-MainForm.MPlayer.DisplayRect.Top);
     //previous statement doesn't find the correct itemindex (reason unknown); thus, reset the index:
     //Menu.ItemIndex:=-1;
     Menu.Show;
     end;
end;

procedure TDisplay.HideMenu(RemoveMenu:Boolean);
begin
  if Menu<>nil then Menu.Hide;
  if RemoveMenu then Menu:=nil;
end;

procedure TDisplay.ShowText;
begin
  ClearLayer(dlText);
  if TitleMode<>tmNone then
     if (TextBitMap<>nil) and TitleAnimation then with TextBitMapRect do begin
        BitMaps[Low(BitMaps)].Canvas.CopyMode:=cmSrcPaint;
        BitMaps[Low(BitMaps)].Canvas.CopyRect(TextDisplayRect,TextBitMap.Canvas,TextBitMapRect);
        BitMaps[Low(BitMaps)].Canvas.CopyMode:=cmSrcCopy;
        end
     else
        Write(TextDisplayRect,Text,DT_CENTER or DT_VCENTER or DT_WORDBREAK);
  Show;
end;

procedure TDisplay.Write(const Rect:TRect; const s:String; Flags:Integer);
var R:TRect;
begin
  if IntersectRect(R,BitMapRect,Rect) and (BitMaps[Ord(Low(BitMaps))]<>nil) then
     with BitMaps[Ord(Low(BitMaps))].Canvas do begin
       CopyMode:=cmSrcCopy;
       Windows.SetBkMode (Handle,Windows.TRANSPARENT);
       Windows.DrawTextEx(Handle,PChar(s),Length(s),R,Flags or {DT_CENTER or} DT_VCENTER {or DT_WORDBREAK} {or DT_SINGLELINE},nil);
       Windows.SetBkMode (Handle,Windows.OPAQUE);
       end;
end;

procedure TDisplay.InitializeActivity(MusicEnabled__:Boolean);
var oActivity:TActivity;
begin
  oActivity:=Activity;
  Activity:=acNone;
  try     MusicEnabled:=MusicEnabled__;
          Activity:=oActivity;
  finally MusicEnabled:=False;
  end;
end;

procedure TDisplay.SetActivity(Activity__:TActivity);
const ACTIVITY_MAP:array[Ord(Low(TActivity))..Ord(High(TActivity))] of Byte = (0,2,1,{4,}3);
var oFastSpeed:Boolean; //oPositionMilliSeconds:Integer; oPlaying:Boolean;
begin
  oFastSpeed:=FastSpeed;
  FastSpeed:=Activity__=acMandala;

  if Activity__<>Activity then begin
     FActivity:=Activity__;

     Viewer1.Suspend;

     Mandala.Enabled:=False;    // 'Mandala.Suspend' is not sufficient; 'Enabled:=False' triggers initialization next time 'Mandala' is activated
     Mandala.SetFixedSpeed(-1); // kludge: 'FixedSpeed' is a late introduced feature to help 'Fractals'

     ClearLayer(dlImage);

     case Activity of
       acNone      : Show;
       acMandala   : begin  Mandala.PaletteChangeEnabled:=True;
                            Mandala.PatternChangeEnabled:=True;
                            Mandala.RestorePaletteAndPattern; // will not hurt, even if there is nothing to restore
                            Mandala.Resume;
                     end;
       acImage     : begin {
                           oPlaying:=MainForm.Music.Playing;
                           oPositionMilliSeconds:=MainForm.Music.PositionMilliSeconds;
                           MainForm.Music.Initialize; // kludge: otherwise something eats the processor-time when 'Image Viewer' is launched after another activity; reason unknown
                           if (not oPlaying) and MainForm.Music.Playing and (not MusicEnabled) then begin
                              MainForm.Music.Stop(True);
                              MainForm.Music.PositionMilliSeconds:=oPositionMilliSeconds;
                              end;
                           }
                           IView1_.Data.FadeOverride:=True;
                           Viewer1.ShowImage(Viewer1.FileName);
                     end;
//     acFireworks : begin Show;
//                   end;
       acFractals  : begin Fractals.ResetDragRect;
                           Mandala.PaletteChangeEnabled:=False;
                           Mandala.PatternChangeEnabled:=False;
                           Fractals.Show;
                     end;

     end; // case
     end;

  Menu:=nil;
  case Activity of
    acNone     : Menu:=ActivitiesMenu;
    acMandala  : if Mandala  <>nil then Menu:=Mandala.Menu;
    acImage    : if Viewer1  <>nil then Menu:=Viewer1.Menu;
    acFractals : if Fractals <>nil then Menu:=Fractals.Menu;
//  acFireworks: if Fireworks<>nil then Menu:=Fireworks.Menu;
  end; //case
  if Menu<>nil then Menu.BitMap:=nil; // flag for 'not initialized'

  with MainForm.MPlayer do with Buttons[Ord(btActivityMenu)] do begin
    BitMapSet:=[]; // rebuild images for all button-states
    with SourceRect[bsEnabled] do begin
      Left  :=ActivityRectLeft+ACTIVITY_MAP[Ord(Activity)]*ActivityRectWidth;
      Top   :=ActivityRectTop;
      Right :=Left+ActivityRectWidth;
      Bottom:=Top +ActivityRectHeight;
      end;
    with SourceRect[bsFocused] do begin
      Left  :=SourceRect[bsEnabled].Left;
      Top   :=SourceRect[bsEnabled].Top+ActivityRectHeight;
      Right :=Left+ActivityRectWidth;
      Bottom:=Top +ActivityRectHeight;
      end;
    end;

  with MainForm.MPlayer do
    if (oFastSpeed<>FastSpeed) and IsIdle then begin
       IsIdle:=False;
       FpsOk:=True;
       ShowStatus;
       end;
end;

function  TDisplay.GetFont:TFont;
begin
  Result:=MPlayerDisplayPanel.Font;
end;

procedure TDisplay.SetFont(Font__:TFont);
var s:String;
begin
  MPlayerDisplayPanel.Font.Assign(Font__);
  if BitMaps[Ord(Low(BitMaps))]<>nil then BitMaps[Ord(Low(BitMaps))].Canvas.Font.Assign(Font);
  s:=Text; Text:=''; Text:=s; // to update bitmap
end;

procedure TDisplay.SetText(const Text__:String);
var W:Integer; TextSize:TSize;
begin
//  if Text<>Text__ then begin // no! update always; 'TitleMode' might have changed
     FText:=Text__;
     if BitMaps[Ord(Low(BitMaps))]<>nil then with TextSize do begin
        W:=BitMaps[Ord(Low(BitMaps))].Width;
        TextSize:=BitMaps[Ord(Low(BitMaps))].Canvas.TextExtent(Text);
        TextBitMap.Free; TextBitMap:=nil;
        if cx*cy<>0 then begin
           if (TitleMode in [tmAnimateTop,tmAnimateBottom]) and
              BitMapCreate(TextBitMap,cx+W,cy) then with TextBitMap.Canvas do begin
              Brush.Color:=clBlack;
              FillRect(Rect(0,0,TextBitMap.Width,TextBitMap.Height));
              Font.Assign(Self.Font);
              TextBitMap.Canvas.TextOut(W,0,Text);
              end;
           TextBitMapRect:=Rect(1,0,W-1,cy);
           with BitMapRect do TextDisplayRect:=Rect(Succ(Left),Top,Pred(Right),Bottom);
           with TextDisplayRect do begin
             if   TitleMode in [tmFixedTop,tmAnimateTop] then
                  Bottom:=Top+cy
             else Top:=Bottom-cy;
             end;
           with FPSDisplayRect do Top:=Bottom-cy;
           end;
        end;
//   end;
end;

procedure TDisplay.Execute;
begin
//if BitMaps[Low(BitMaps)]<>nil then begin
     if //(Mandala<>nil) and
        (not Mandala.Suspended) then begin
        Mandala.Show;
        if Menu.Visible then Menu.Show
        else ClearLayer(Pred(dlImage));
        if (Activity=acFractals) {and (Fractals<>nil)} then with Fractals do
           if DragRect.Left<DragRect.Right then DrawRect(DragRect,pmXor);
        end
     else if (Activity=acImage) and IView1_.Data.NewBitMap then begin
             with IView1_.Data do begin
               Enter;
               try     NewBitMap:=False;
               finally Leave;
               end;
               end;

             if Menu.Visible then Menu.Show  // quick and dirty: the layer 'dlFocus' (menu) is just above the layer 'dlImage' in the image-tower
             else ClearLayer(Pred(dlImage));
             end
{   else if (Activity=acFireworks) (*and (Fireworks<>nil)*) then begin
        ClearLayer(dlText);
        with Fireworks do if Visible then begin Step; Show; end;
        end
}
     else ClearLayer(dlText);

     if TitleMode<>tmNone then // inline 'ShowText' to avoid superflous 'CopyRect' calls in the bitmap-tower
        if (TextBitMap<>nil) and TitleAnimation then with TextBitMapRect do begin
           if (MainForm.MPlayer.Fps<=MPLAYER_DISPLAY_TEXT_MAX_SPEED_PPS) // speed limit, pixels per second
              or
              Odd(MainForm.MPlayer.TickCount) then begin
              Inc(Left); Inc(Right);
              if Left>TextBitMap.Width then begin
                 Left:=0; Right:=BitMaps[Low(BitMaps)].Width-2;
                 end;
              end;
           BitMaps[Low(BitMaps)].Canvas.CopyMode:=cmSrcPaint;
           BitMaps[Low(BitMaps)].Canvas.CopyRect(TextDisplayRect,TextBitMap.Canvas,TextBitMapRect);
           BitMaps[Low(BitMaps)].Canvas.CopyMode:=cmSrcCopy;
           end
        else
           Write(TextDisplayRect,Text,DT_CENTER or DT_VCENTER or DT_WORDBREAK);

     with MainForm.MPlayer do begin
       if ShowFps and (Fps<>0) then
          Self.Write(FPSDisplayRect,IntToStr(Fps)+FpsText,{DT_CENTER} DT_RIGHT or DT_VCENTER);
       end;

     Show;
//   end;
end;

function  TDisplay.RestoreImageIfFading:Boolean;
begin
  Result:=(Viewer1<>nil) and
          (Viewer1.PictThread<>nil) and
          (Viewer1.PictThread.State=Ord(tsFading));
  if Result then begin // display original image again
     BitMaps[Ord(dlImage)].Canvas.CopyRect(BitMapRect,BitMaps[Ord(dlFadeFrom)].Canvas,BitMapRect);
     ShowImage;
     end;
end;

function  TDisplay.ShowImageSynchronized(const FileName:String; BitMap:TBitMap):Boolean;
begin
  if (Activity=acImage) and  // activity may have changed after the calling thread delivered this picture
     (FileName<>'') and
     (BitMap<>nil) then begin
     BitMaps[Ord(dlImage)].Canvas.CopyRect(BitMapRect,BitMap.Canvas,BitMapRect);
     Viewer1.FileName:=FileName;
     if MainForm.MPlayer.Visible then ShowImage;
     Result:=True;
     end
  else
     Result:=False;
end;

end.
