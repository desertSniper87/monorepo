unit IView1_;

interface

uses Windows,Classes,Controls,StdCtrls, ExtCtrls,
     {$WARNINGS OFF}
     FileCtrl, // Warning: Unit 'FileCtrl' is specific to a platform
     {$WARNINGS ON}
     Graphics,Forms,Messages,SyncObjs,
     Misc_,IniFile_,Mandal2_,BitMap_,Pict_,Menu_;

type
  TPictThreadState = (tsIdle,tsLoad,tsLoading,tsWait,tsFade,tsFading,tsShow,tsError,tsTerminate,tsTerminated);

const
  PictThreadStateText : array[TPictThreadState] of String =
    ('Undefined','Load','Loading','Wait','Fade','Fading','Show','Error','Terminate','Terminated');

type
  TImageViewerData = class(TCriticalSection) // Data shared among main-program and load-thread
  public
    CancelAction:Boolean;
    FadeEnabled:Boolean;
    FadeOverride:Boolean;
    FileName:String;
    ResizeToWindowWidth:Boolean;
    RandomOrder:Boolean;
    Scale:Boolean;
    ScaleHeight:Integer;
    ScaleWidth:Integer;
    SlideShow:Boolean;
    SlideShowOverride:Boolean;
    TimeDelayMS:Integer;
    TimeFadeMS:Integer;
    NewBitMap:Boolean;           // new bitmap ready for display; set by load-thread, used by main-program
  end;

type
  TViewer1ButtonType = (vbOpen,vbPrior,vbNext,vbSize,vbSettings,vbSlideShow);

  TPictThread = class;

  TViewer1 = class
  private
    fFileName  :String;
    procedure   Finalize;
  protected
    function    GetFileName:String;
    procedure   SetFileName(const FileName__:String);
  public
    Menu       :TMPlayerDisplayMenu;
    PictThread :TPictThread;

    property    FileName           :String  read GetFileName            write SetFileName;

    constructor Create(MenuPanel__:TPanel);
    destructor  Destroy; override;

    procedure   Initialize;
    function    LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
    procedure   PriorOrNext(Prior,StopSlideShow:Boolean);
    procedure   Resume;
    function    SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
    procedure   SetDefaultValues;
    procedure   ShowImage(const FileName__:String);
    procedure   Suspend;
  end;

  TPictThread = class(TThread1)
  private
    BitMapHeight
               :Integer;
    BitMapRect :TRect;
    BitMaps    :PDisplayBitMapTower;
    BitMapWidth:Integer;
    FileListBox:TFileListBox; // caution: 'FileListBox' methods are not thread-safe; thus all access must be syncronized with main-thread
    fFileListItemsCount:Integer;
    Image      :TImage;
    Pict       :TPict;
    NewIndex   :Integer;
    NewFileName:String;
    procedure   FileListBoxDeleteCurrentItemSynchronized;
    procedure   LoadItem(Index__:Integer);
    procedure   LoadItemSynchronized;
    procedure   SetFileNameSynchronized;
    procedure   ShowSynchronized;
  protected
    procedure   Execute; override;
    function    GetFadeEnabled:Boolean;
    function    GetTimeFadeMS:Cardinal;
    function    GetFileName:String;
    function    GetResizeToWindowWidth:Boolean;
    function    GetRandomOrder:Boolean;
    function    GetSlideShow:Boolean;
    procedure   SetFadeEnabled(FadeEnabled__:Boolean);
    procedure   SetFileName(const FileName__:String);
    procedure   SetResizeToWindowWidth(ResizeToWindowWidth__:Boolean);
    procedure   SetRandomOrder(RandomOrder__:Boolean);
    procedure   SetSlideShow(SlideShow__:Boolean);
  public
    procedure   AssignTo(Destination:TImage);
    procedure   AssignToImageSynchronized;
    function    CancelLoadInProgress:Boolean;
    constructor Create(CriticalSection__: TCriticalSection; Memo__:TMemo; Image__:TImage);
    destructor  Destroy; override;
    procedure   InitializeBitMapInfo;
    procedure   NextSynchronized;
    procedure   NextRandomSynchronized;
    procedure   PriorSynchronized;

    property    TimeFadeMS         :Cardinal        read GetTimeFadeMS;
    property    FadeEnabled        :Boolean         read GetFadeEnabled         write SetFadeEnabled;
    property    FileListItemsCount :Integer         read fFileListItemsCount;
    property    FileName           :String          read GetFileName            write SetFileName;
    property    ResizeToWindowWidth:Boolean         read GetResizeToWindowWidth write SetResizeToWindowWidth;
    property    RandomOrder        :Boolean         read GetRandomOrder         write SetRandomOrder;
    property    SlideShow          :Boolean         read GetSlideShow           write SetSlideShow;
  end;

var
  Data    : TImageViewerData = nil;

  Viewer1 : TViewer1 = nil;

implementation

uses SysUtils,Buttons,
     SokUtil_,Text_,Main_,Options_,Sound_,Open1_,Music_,MPlayer1_,Display_,Res_;

const
  IMAGE_VIEWER1_INIFILE_SECTION='ImageViewer'; // don't localize

constructor TViewer1.Create(MenuPanel__:TPanel);
begin
  Inherited Create;
  Menu                 :=nil;
  PictThread           :=nil;
  try    Menu          :=TViewer1Menu.Create(nil,MenuPanel__,MenuPanel__.Font.Size);
  except on E:Exception do begin
            Menu.Free; Menu:=nil;
            end;
  end;
  SetDefaultValues;
  Data.FadeOverride    :=True; // first time: show the image without fading
  Initialize;
  if PictThread<>nil then begin
     // kludge: for some unknown reason, it's best to launch the picture-thread
     // before the 'Mandala'-threads;
     // otherwise the picture-thread sometimes 'crawls'
     PictThread.State:=Ord(tsIdle); // 'tsIdle': the thread will suspend itself again
     Self.Resume;
     end;
end;

destructor TViewer1.Destroy;
begin
  Finalize;
  Menu.Free; Menu:=nil;
  Inherited Destroy;
end;

procedure TViewer1.Initialize;
begin
  if PictThread=nil then
     try    PictThread:=TPictThread.Create(Data,nil,nil);
     except on E:Exception do begin
               PictThread.Free; PictThread:=nil;
               Error(E.Message,Application.Title);
               end;
     end;

  if Menu<>nil then
     if   PictThread=nil then
          Menu.BitMap:=nil  // 'Menu.BitMap' used as a flag for proper initialization
     else Menu.MakeButtons;
end;

procedure TViewer1.Finalize;
const WAIT_MS=5000;
var TimeOut:Cardinal; oCursor:TCursor;
begin
  if Menu<>nil then begin
     Menu.Hide;
     Menu.BitMap:=nil; // flag for 'not initialized'
     end;

  if PictThread<>nil then begin
     Self.Suspend;
     PictThread.CancelLoadInProgress;
     PictThread.State:=Ord(tsTerminate);
     Self.Resume; // activate the thread, so it can terminate normally
     PictThread.Terminate; // unless the thread was created with 'FreeOnTerminate'=True

     repeat TimeOut:=GetTickCount+WAIT_MS;
     until  TimeOut>GetTickCount; // simple clock wrap around check

     oCursor:=Screen.Cursor;
     try     //Screen.Cursor:=crHourGlass;
             repeat SleepEx(0,False);
             until  (PictThread.State=Ord(tsTerminated)) or (GetTickCount>=TimeOut);
     finally Screen.Cursor:=oCursor;
     end;

     Data.Enter;
     try     if not PictThread.State=Ord(tsTerminated) then
                Self.Suspend; // otherwise 'TThread.Destroy' enters an infinite 'WaitFor' loop
     finally Data.Leave;
     end;
     PictThread.Free; // free here, unless the thread was created with 'FreeOnTerminate'=True
     end;
  PictThread:=nil;
end;

function  TViewer1.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
var Section:String;
begin
//Result:=True; exit;
  Data.Enter;
  try
    Section:=IMAGE_VIEWER1_INIFILE_SECTION;
    fFileName               :=                KeepDataPathUpToDate(IniFile.ReadString(Section,'PictFileName',fFileName));
    Data.FileName           :=fFileName;
    Data.ResizeToWindowWidth:=                IniFile.ReadBool   (Section,'ResizeToWindowWidth',Data.ResizeToWindowWidth);
    Data.SlideShow          :=                IniFile.ReadBool   (Section,'SlideShow.Enabled',Data.SlideShow);
    Data.TimeDelayMS        :=Max(0,Min(      VIEWER_MAX_TIME_DELAY_MS,
                                                  IniFile.ReadInteger(Section,'SlideShow.TimeDelayMS',Data.TimeDelayMS)));
    Data.TimeFadeMS         :=Max(0,Min(      VIEWER_MAX_FADE_TIME_MS,
                                                  IniFile.ReadInteger(Section,'SlideShow.TimeFadeMS',Data.TimeFadeMS)));
    Data.RandomOrder        :=                IniFile.ReadBool   (Section,'SlideShow.RandomOrder',Data.RandomOrder);
    Result:=(Menu<>nil) and Menu.LoadSettingsFromIniFile(IniFile,Section);
  finally
    Data.Leave;
  end;
end;

function  TViewer1.SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
var Section:String;
begin
  Data.Enter;
  try
    Section:=IMAGE_VIEWER1_INIFILE_SECTION;
    IniFile.WriteString (Section,'PictFileName',fFileName); // note: not necessarily identical to 'PictThread.ThreadData.FileName'
    IniFile.WriteBool   (Section,'ResizeToWindowWidth',Data.ResizeToWindowWidth);
    IniFile.WriteBool   (Section,'SlideShow.Enabled',Data.SlideShow);
    IniFile.WriteInteger(Section,'SlideShow.TimeDelayMS',Data.TimeDelayMS);
    IniFile.WriteInteger(Section,'SlideShow.TimeFadeMS',Data.TimeFadeMS);
    IniFile.WriteBool   (Section,'SlideShow.RandomOrder',Data.RandomOrder);
    Result:=(Menu<>nil) and Menu.SaveSettingsToIniFile(IniFile,Section);
  finally
    Data.Leave;
  end;
end;

procedure TViewer1.SetDefaultValues;
begin
  Data.Enter;
  try
    fFileName               :=DEFAULT_VALUE;
    Data.FadeEnabled        :=True;
    Data.FadeOverride       :=False;
    Data.FileName           :=fFileName;
    Data.ResizeToWindowWidth:=False;
    Data.RandomOrder        :=False;
    Data.SlideShow          :=False;
    Data.SlideShowOverride  :=False;
    Data.TimeDelayMS        :=3000;
    Data.TimeFadeMS         :=1500;
    Data.NewBitMap          :=False;
  finally
    Data.Leave;
  end;

  if Menu<>nil then
     if    Menu.MenuPanel<>nil then
           Menu.SetDefaultValues(clBlack,Menu.MenuPanel.Font.Size)
     else  Menu.SetDefaultValues(clBlack,10);
end;

function  TViewer1.GetFileName:String;
begin
  Data.Enter;
  try     Result:=fFileName; // current file, not 'Data.FileName' which contains file in progress, if slideshow is enabled
  finally Data.Leave;
  end;
end;

procedure TViewer1.SetFileName(const FileName__:String);
begin
  fFileName:=FileName__;

  Data.Enter;
  try
    Data.FileName:=FileName__;
    if Data.FileName=DEFAULT_VALUE then begin
       Data.ResizeToWindowWidth:=True;
       Data.SlideShow:=False;
       end;
  finally
    Data.Leave;
  end;

end;

procedure TViewer1.ShowImage(const FileName__:String);
begin
  if PictThread=nil then Initialize;

  if PictThread<>nil then begin
     Self.Suspend;
     fFileName:=FileName__; // self filename
     PictThread.FileName:=FileName__; // thread's filename
     PictThread.CancelLoadInProgress;
     PictThread.State:=Ord(tsLoad);
     Data.SlideShowOverride:=True;
     Data.NewBitMap:=False;
     if MainForm.MPlayer.Visible then Self.Resume;
     end
  else FileName:=FileName__;
end;

procedure TViewer1.PriorOrNext(Prior,StopSlideShow:Boolean);
begin
  if PictThread<>nil then begin
    Self.Suspend;
    Data.SlideShowOverride:=True;
    if StopSlideShow then PictThread.SlideShow:=False;
    if (FileName=DEFAULT_VALUE) or FileExists(FileName) then
       PictThread.FileName:=FileName;
    if Prior then PictThread.PriorSynchronized
    else PictThread.NextSynchronized;
    if (Menu<>nil) and Menu.Visible then Menu.Show;
    Self.Resume;
    end;
end;

procedure TViewer1.Resume;
begin
  if (PictThread<>nil) and PictThread.Suspended then begin
     Data.Enter;
     try     PictThread.Suspended:=False;
     finally Data.Leave;
     end;
     end;
end;

procedure TViewer1.Suspend;
begin
  Data.Enter; // the thread must not lock the data while it's suspended
  try     if (PictThread<>nil) and (not PictThread.Suspended) then begin
             PictThread.Suspended:=True;
             end;
  finally Data.Leave;
  end;
end;

constructor TPictThread.Create(CriticalSection__:TCriticalSection; Memo__:TMemo; Image__:TImage);
begin
  inherited Create(CriticalSection__,tpNormal);

  Image:=Image__;
  FileListBox:=nil; Pict:=nil; fFileListItemsCount:=0; BitMaps:=nil;
  try    FileListBox            :=TFileListBox.Create(nil);
         FileListBox.Name       :='TPictThreadFileListBox';
         FileListBox.Visible    :=False;
         if   MainForm<>nil then
              FileListBox.Parent:=MainForm
         else FileListBox.Parent:=OpenForm;
         FileListBox.Mask       :=IMAGE_FILES_FILTER;
         FileListBox.MultiSelect:=True;
         FileListBox.ItemIndex  :=-1;
         Pict                   :=TPict.Create;
  except on E:Exception do
         begin FileListBox.Free; FileListBox:=nil;
               Pict       .Free; Pict       :=nil;
               Error(E.Message,Application.Title);
         end;
  end;
end;

destructor TPictThread.Destroy;
begin
  FileListBox.Free; Pict.Free;
  Inherited;
end;

procedure TPictThread.Execute; // load and display pictures
var TimeOfLastUpdate,TimeForNextUpdate:Cardinal;

  function  DoLoad:Boolean;
  var ScaleWidth,ScaleHeight:Integer;
      Scale,ResizeToWindowWidth:Boolean; s:String;

    function  LoadPicture(const FileName:String):Boolean;
    //var TimeStart:Cardinal;
    begin
      Result:=False;

      if Pict=nil then
         try    Pict:=TPict.Create;
         except begin Pict.Free; Pict:=nil; end
         end;

      if Pict<>nil then begin
         Pict.FileName:=''; //TimeStart:=GetTickCount;
         if FileName<>'' then
            if ((FileName=DEFAULT_VALUE) // kludge: specialized for the image viewer
                and
                //Pict.LoadFromBitMap(MainForm.DefaultBoardBackgroundImage.Picture.BitMap)
                Pict.LoadFromResource(BOARD_BACKGROUND_RES_NAME,RC_JPG)
               )
               or
               ((FileName<>DEFAULT_VALUE) and
                FileExists(FileName) and
                Pict.LoadFromFile(FileName)) then begin
                Pict.FileName:=FileName;
                Result:=True;
                //Writeln(LogFile,'Loaded: ',Pict.FileName,SPACE,IntToStr(GetTickCount-TimeStart));
               end;
         end;
    end;

    function  ScalePicture(ScaleWidth,ScaleHeight:Integer; ResizeToWindowWidth:Boolean):Boolean;
    var Border:Integer; View:TImageView; //TimeStart:Cardinal;
    begin
      Result:=False;
      if (Pict<>nil) and (ScaleWidth<>0) and (ScaleHeight<>0) then begin
         //TimeStart:=GetTickCount;
         if   (Pict.FileName=DEFAULT_VALUE) or ResizeToWindowWidth then
              View:=ivFill
         else View:=ivNone;
         if   View=ivFill then Border:=0
         else Border:=0;//1;
         //Pict.Resize(ScaleWidth,ScaleHeight); // quick test
         Pict.Scale(ScaleWidth,ScaleHeight,View,Border,Border,True,Data.CancelAction);
         Result:=not Data.CancelAction; //True;
         //Writeln(LogFile,'Scaled: ',Pict.FileName,SPACE,IntToStr(GetTickCount-TimeStart));
         end;
    end;

    function  FramePicture:Boolean;
    var x,y,W,H:Integer; R:TRect;
    begin
      Result:=(Pict<>nil) and
              (Pict.BitMap<>nil) and
              (BitMaps<>nil) and
              (BitMaps[0]<>nil); //  [0]: if this one exists, the other ones are ok too
      if Result then begin
         W:=RectWidth (Pict.ScaledRect);  // caution: 'ScaledRect' is supposed to be valid
         H:=RectHeight(Pict.ScaledRect);
         x:=(BitMapWidth -W) div 2;
         y:=(BitMapHeight-H) div 2;
         R:=Rect(x,y,x+W,y+H);
         BitMaps[Ord(dlFadeTo)].Canvas.CopyRect(BitMapRect,BitMaps[Ord(dlbackground)].Canvas,BitMapRect);
         BitMaps[Ord(dlFadeTo)].Canvas.CopyRect(R,Pict.BitMap.Canvas,Pict.ScaledRect);
         end;
    end;

  begin // DoLoad
    Data.Enter;
    try     s:=Data.FileName;
            Scale:=Data.Scale;
            ScaleWidth:=Data.ScaleWidth; ScaleHeight:=Data.ScaleHeight;
            ResizeToWindowWidth:=Data.ResizeToWindowWidth;
            if fState=Ord(tsLoad) then fState:=Ord(tsLoading);
            Result:=(fState=Ord(tsLoading)) and (Pict<>nil) and (s<>'');
    finally Data.Leave;
    end;

    if Result then Result:=LoadPicture(s);
    if Result and (State=Ord(tsLoading)) and Scale then // check that caller hasn't abandoned the task
       Result:=ScalePicture(ScaleWidth,ScaleHeight,ResizeToWindowWidth);
    if Result and (State=Ord(tsLoading)) then // check again that caller hasn't abandoned the task
       Result:=FramePicture;

    if Result then begin
       Data.Enter;
       try     if   fState=Ord(tsLoading) then // check that caller hasn't abandoned the task
                    if        Data.SlideShowOverride then begin // show the picture immidiately, regardless of the slideshow status
                              Data.SlideShowOverride:=False;    // reset the switch

                              if   Data.SlideShow and
                                   (Data.TimeFadeMS>0) and
                                   Data.FadeEnabled and
                                   (not Data.FadeOverride) then
                                   fState:=Ord(tsFade)
                              else
                                   fState:=Ord(tsShow);

                              Data.SlideShowOverride:=False;    // reset the switch
                              Data.FadeOverride     :=False;    // reset the switch

                              end
                    else if   Data.SlideShow then
                              fState:=Ord(tsWait) // slide show: wait until it is time to show the next picture
                         else fState:=Ord(tsShow) // load one picture only
               else Result:=False;
       finally Data.Leave;
       end;
       end
    else begin
       Data.Enter;
       try     if fState=Ord(tsLoading) then
                  fState:=Ord(tsError);
       finally Data.Leave;
       end;
       end;
  end;

  procedure DoWait(TimeOfLastUpdate:Cardinal; var TimeForNextUpdate:Cardinal);
  var t,TimeNow:Cardinal;
  begin
    TimeNow:=GetTickCount;
    if TimeNow<TimeOfLastUpdate then begin // clock wrap around
       t:=High(TimeOfLastUpdate)-TimeOfLastUpdate;
       Data.Enter;
       try     if   t<Cardinal(Data.TimeDelayMS) then
                    TimeForNextUpdate:=Cardinal(Data.TimeDelayMS)-t
               else TimeForNextUpdate:=TimeNow;
       finally Data.Leave;
       end;
       end;

    if TimeNow>=TimeForNextUpdate then begin
       Data.Enter;
       try     if fState=Ord(tsWait) then
                  if   (Data.TimeFadeMS>0) and
                       Data.FadeEnabled then
                       fState:=Ord(tsFade)
                  else fState:=Ord(tsShow);
       finally Data.Leave;
       end;
       end
    else //if not Terminated then SleepEx(TimeForNextUpdate-(Succ(TimeNow)),False); // 'Succ...' : wake up 1 millisecond before timeout
         SleepEx(0,False);
  end;

  function  DoFade:Boolean;
  var Pct:Integer; TimeNow,TimeStart,TimeEnd,TimeFade:Cardinal;
  begin
    Data.Enter;
    try     Result:=fState=Ord(tsFade);
            if Result then begin // it is a dilemma when to accept the fading image as current image; here it is accepted as current image as soon as fading begins
               fState:=Ord(tsFading);
               Viewer1.FileName:=FileName;
               end;
            BitMaps[Ord(dlFadeFrom)].Canvas.CopyRect(BitMapRect,BitMaps[Ord(dlImage)].Canvas,BitMapRect);
            TimeFade        :=Cardinal(Data.TimeFadeMS);
            Data.NewBitMap  :=False;
    finally Data.Leave;
    end;

    repeat TimeStart:=GetTickCount;
           TimeEnd  :=TimeStart+TimeFade;
    until  TimeEnd  >=TimeStart; // simple clock wrap around check

    SleepEx(0,False); // do something else, before fading begins

    Pct:=0;
    while (State=Ord(tsFading)) and (Pct<100) do begin
      TimeNow:=GetTickCount;

      if (TimeNow<TimeEnd) and (TimeNow>=TimeStart) then
         Pct:=Integer((TimeNow-TimeStart)*100 div TimeFade)
      else begin
        Inc(Pct,4); Result:=False; // complete the fade smoothly
        end;

      if Pct<100 then begin
         BitMapAlphaBlend(BitMaps[Ord(dlImage)],BitMaps[Ord(dlFadeFrom)],BitMaps[Ord(dlFadeTo)],Pct,Data.CancelAction);

         Data.Enter;
         try     if fState=Ord(tsFading) then Data.NewBitMap:=True;
         finally Data.Leave;
         end;

         if   Result then
              SleepEx(0,False) // give the main-thread a chance to show the image
         else while Data.NewBitMap and (State=Ord(tsFading)) do
                SleepEx(0,False); // wait for main-thread to show the image, so the transition completes smoothly
         end;

      end;

    Data.Enter;
    try     Result:=fState=Ord(tsFading);
            if Result then begin
               fState:=Ord(tsShow); // this does not trigger 'DoShow', but 'State' must be set to this value, before 'DoSlideShow' is called after this procedure
               BitMaps[Ord(dlImage)].Canvas.CopyRect(BitMapRect,BitMaps[Ord(dlFadeTo)].Canvas,BitMapRect);
               Data.NewBitMap:=True;
               end;
    finally Data.Leave;
    end;

  end;

  function  DoShow:Boolean;
  begin
    Synchronize(ShowSynchronized);
    Result:=State=Ord(tsShow);
  end;

  function  DoError:Boolean;
  var SlideShow:Boolean;
  begin
    Result:=False;

    Data.Enter;
    try     SlideShow  :=Data.SlideShow;
            fState     :=Ord(tsIdle);
    finally Data.Leave;
    end;

    if SlideShow and (not Terminated) then begin
       Synchronize(FileListBoxDeleteCurrentItemSynchronized);
       if FileListItemsCount>0 then
          if   RandomOrder then
               Synchronize(NextRandomSynchronized)
          else Synchronize(NextSynchronized)
       else begin
          Data.Enter;
          try     Data.SlideShow:=False;
                  Result:=fState=Ord(tsIdle); // check that caller hasn't started another task
          finally Data.Leave;
          end;
          if Result and (not Terminated) then
             Suspended:=True; // nothing to do right now
          end;
       end;
  end;

  procedure DoTerminate;
  begin
    if not Terminated then Terminate;
  end;

  function  DoSlideShow(var TimeOfLastUpdate,TimeForNextUpdate:Cardinal):Boolean;
  var TimeNow,TimerInterval:Cardinal; SlideShow:Boolean;
  begin
    TimeNow:=GetTickCount;
    TimeOfLastUpdate :=TimeNow;

    Data.Enter;
    try     Result:=fState=Ord(tsShow);
            if Result then // check that caller hasn't abandoned the task
               fState    :=Ord(tsIdle); // prepare to load next image
            TimerInterval:=Data.TimeDelayMS;
            SlideShow    :=Data.SlideShow;
    finally Data.Leave;
    end;

    if Result and (not Terminated) then
       if SlideShow then
          if   RandomOrder then
               Synchronize(NextRandomSynchronized)
          else Synchronize(NextSynchronized)
       else begin //nothing to do right now
          Suspended:=True;
          TimeNow:=GetTickCount;
          end;

    TimeForNextUpdate:=TimeNow+TimerInterval;
    if TimeForNextUpdate=0 then Inc(TimeForNextUpdate);
  end;

begin // TPictThread.Execute
  TimeOfLastUpdate:=0; TimeForNextUpdate:=0;

  InitializeBitMapInfo;

  while not Terminated do begin
    case TPictThreadState(State) of
      tsIdle        : Suspended:=True;
      tsLoad        : DoLoad;
      tsLoading     :;
      tsWait        : DoWait(TimeOfLastUpdate,TimeForNextUpdate);
      tsFade        : if DoFade then DoSlideShow(TimeOfLastUpdate,TimeForNextUpdate);
      tsFading      :;
      tsShow        : if DoShow then DoSlideShow(TimeOfLastUpdate,TimeForNextUpdate);
      tsError       : DoError;
      tsTerminate   : DoTerminate;
      tsTerminated  :;
      else            Suspended:=True;
    end; // case
    end;
//Windows.MessageBox(0,'Execute done','Load Thread',MB_OK);
  State:=Ord(tsTerminated);
end;

procedure TPictThread.InitializeBitMapInfo;
begin
  BitMapRect  :=MainForm.MPlayer.Display.BitMapRect;
  BitMaps     :=Addr(MainForm.MPlayer.Display.BitMaps);
  BitMapHeight:=RectHeight(BitMapRect);
  BitMapWidth :=RectWidth (BitMapRect);
end;

procedure TPictThread.FileListBoxDeleteCurrentItemSynchronized;
begin // Synchronized, because 'FileListBox' is not thread-safe
  Data.Enter;
  try
  if FileListBox<>nil then with FileListBox do begin
     if ItemIndex>=0 then begin
        Items.Delete(ItemIndex);
        ItemIndex:=ItemIndex-1; // adjust before call to 'Next'
        end;
     fFileListItemsCount:=Items.Count;
     end
  else fFileListItemsCount:=0;
  finally
    Data.Leave;
  end;
end;

function  TPictThread.GetFadeEnabled:Boolean;
begin
  Data.Enter;
  try     Result:=Data.FadeEnabled;
  finally Data.Leave;
  end;
end;

procedure TPictThread.SetFadeEnabled(FadeEnabled__:Boolean);
begin
  Data.Enter;
  try     Data.FadeEnabled:=FadeEnabled__;
  finally Data.Leave;
  end;
end;

function  TPictThread.GetTimeFadeMS:Cardinal;
begin
  Data.Enter;
  try     Result:=Data.TimeFadeMS;
  finally Data.Leave;
  end;
end;

function  TPictThread.GetFileName:String;
begin
  Data.Enter;
  try     Result:=Data.FileName;
  finally Data.Leave;
  end;
end;

procedure TPictThread.SetFileNameSynchronized;
var i:Integer; Directory__:String;
begin // Synchronized, because 'FileListBox' is not thread-safe
  Data.Enter;
  try
    Data.FileName:=NewFileName;

    Directory__:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Data.FileName));
    if FileListBox<>nil then begin
       if DirectoryExists(Directory__) then with FileListBox do begin
          Directory:=Directory__;
          Update;

          for i:=0 to Pred(Items.Count) do
              FileListBox.Selected[i]:=False;

          ItemIndex:=Items.IndexOf(ExtractFileName(Data.FileName));

          if (ItemIndex>=0) and (ItemIndex<Items.Count) then
             Selected[ItemIndex]:=True;
          end
       else FileListBox.Clear;
       end;

    if (FileListBox=nil) or
       (FileListBox.Items.Count=0) or
       (FileListBox.ItemIndex=-1) then begin
       if FileListBox<>nil then FileListBox.Clear;
       Data.FileName:=DEFAULT_VALUE;
       Data.SlideShow:=False;
       end;

  finally
    if   FileListBox=nil then
         fFileListItemsCount:=0
    else fFileListItemsCount:=FileListBox.Items.Count;
    Data.Leave;
  end;
end;

procedure TPictThread.SetFileName(const FileName__:String);
begin
  Data.Enter;
  try     NewFileName:=FileName__;
          if   Suspended then SetFileNameSynchronized
          else Synchronize(SetFileNameSynchronized);
  finally Data.Leave;
  end;
end;

function  TPictThread.GetResizeToWindowWidth:Boolean;
begin
  Data.Enter;
  try     Result:=Data.ResizeToWindowWidth;
  finally Data.Leave;
  end;
end;

procedure TPictThread.SetResizeToWindowWidth(ResizeToWindowWidth__:Boolean);
begin
  Data.Enter;
  try     if Data.ResizeToWindowWidth<>ResizeToWindowWidth__ then begin
             Data.ResizeToWindowWidth:=ResizeToWindowWidth__;
             Data.CancelAction:=True;
             end;
  finally Data.Leave;
  end;
end;

function  TPictThread.GetRandomOrder:Boolean;
begin
  Data.Enter;
  try     Result:=Data.RandomOrder;
  finally Data.Leave;
  end;
end;

procedure TPictThread.SetRandomOrder(Randomorder__:Boolean);
begin
  Data.Enter;
  try     Data.RandomOrder:=RandomOrder__;
  finally Data.Leave;
  end;
end;

function  TPictThread.GetSlideShow:Boolean;
begin
  Data.Enter;
  try     Result:=Data.SlideShow;
  finally Data.Leave;
  end;
end;

procedure TPictThread.SetSlideShow(SlideShow__:Boolean);
begin
  Data.Enter;
  try     Data.SlideShow:=SlideShow__;
          if (not Data.SlideShow) and
             (fState<>Ord(tsFading)) then CancelLoadInProgress;
  finally Data.Leave;
  end;
end;

procedure TPictThread.ShowSynchronized;
begin
  if not MainForm.MPlayer.Display.ShowImageSynchronized(
           Pict.FileName,BitMaps[Ord(dlFadeTo)]) then
     State:=Ord(tsIdle);
end;

procedure TPictThread.AssignToImageSynchronized;
begin
  AssignTo(Image);
end;

procedure TPictThread.AssignTo(Destination:TImage);
begin
  if (Destination<>nil) and (Pict<>nil) and (State=Ord(tsShow)) then begin
     Data.Enter;
     try     fState:=Ord(tsIdle);
             try    Destination.Picture.Assign(Pict.BitMap);
             except on E:Exception do
                    //Application.MessageBox(PChar(E.Message),PChar(Application.Title),MB_OK)
                    ;
             end;
     finally Data.Leave;
     end;
     end;
end;

procedure TPictThread.LoadItemSynchronized;
begin // Synchronized, because 'FileListBox' is not thread-safe
  Data.Enter;
  try
    if (FileListBox<>nil) and
       (fState=Ord(tsIdle)) and
       (not Terminated) then with FileListBox do
       if (NewIndex>=0) and (NewIndex<Items.Count) then begin
          ItemIndex:=NewIndex;
          Data.FileName:=StrWithTrailingPathDelimiter(Directory)+Items[ItemIndex];
          Selected[ItemIndex]:=True;
          fState:=Ord(tsLoad);
          if Suspended then
             Suspended:=False;
          end
       else ItemIndex:=-1;
  finally
    Data.Leave;
  end;
end;

procedure TPictThread.LoadItem(Index__:Integer);
begin
  NewIndex:=Index__;
  LoadItemSynchronized;
end;

procedure TPictThread.PriorSynchronized;
begin // Synchronized, because 'FileListBox' is not thread-safe
  if FileListBox<>nil then with FileListBox do begin
     State:=Ord(tsIdle);
     if   ItemIndex>0 then LoadItem(Pred(ItemIndex))
     else if Items.Count>0 then LoadItem(Pred(Items.Count));
     end;
end;

procedure TPictThread.NextSynchronized;
begin // Synchronized, because 'FileListBox' is not thread-safe
  if FileListBox<>nil then with FileListBox do begin
     State:=Ord(tsIdle);
     if   ItemIndex<Pred(Items.Count) then LoadItem(Succ(ItemIndex))
     else if Items.Count>0 then LoadItem(0);
     end;
end;

procedure  TPictThread.NextRandomSynchronized;
var i,j:Integer;
begin // Synchronized, because 'FileListBox' is not thread-safe
  if FileListBox<>nil then with FileListBox do
     if Items.Count>0 then begin
        i:=Random(Items.Count); j:=i;
        repeat Inc(i);
               if i>=Items.Count then i:=0;
        until  (not Selected[i]) or (i=j);
        if Selected[i] then // all visited: reset
           for j:=0 to Pred(Items.Count) do Selected[j]:=False;
        State:=Ord(tsIdle);
        LoadItem(i);
        end;
end;

function  TPictThread.CancelLoadInProgress:Boolean;
begin
  Data.Enter;
  try     Result:=(fState=Ord(tsLoad)) or (fState=Ord(tsLoading)) or
                  (fState=Ord(tsWait)) or
                  (fState=Ord(tsFade)) or (fState=Ord(tsFading));
          if Result then begin
             Data.CancelAction:=True;  // kludge : if thread is scaling or alpha blending, abort this time-consuming operation
             fState:=Ord(tsIdle);      // caution: the thread will suspend itself after this
             end;
  finally Data.Leave;
  end;
end;

initialization
  try    Data:=TImageViewerData.Create;
  except on E:Exception do begin Data.Free; Data:=nil; Halt(1); end;
  end;

finalization
  Data.Free;

end.

