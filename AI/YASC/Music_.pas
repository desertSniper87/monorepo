unit Music_;

interface

uses Windows,MPlayer,Classes,Controls,Grids,
     {$WARNINGS OFF}
     FileCtrl, // Warning: Unit 'FileCtrl' is specific to a platform
     {$WARNINGS ON}
     MMSystem,
     Misc_,IniFile_,BitMap_,MPlayer1_;

{///$DEFINE MUSIC_PLAYER}

const
  PLAYLIST_INIFILE_SECTION     = '*Sokoban Music PlayList*'; // don't localize

type
  TVolType      = (vtLeft, vtRight);
  TVol          = array[vtLeft..vtRight] of Word;
  TOutputDevice = (odAux,odWaveOut,odMidiOut);
  TMusicManager = class
  private
    fExitMasterVolume   :Integer;
    fVolume64Kibi       :Integer;
    ExitDeviceVolume    :array[TOutputDevice] of DWORD;
    OldMasterVolume     :Integer; // master volume at the time the application is deactivated
    OldTrackLength      :Integer;
    OldTrackPosition    :Integer;
    Parent              :TWinControl;
{
    hMx: HMIXER;
    MxId: UINT;
    mlMixerLine: TMIXERLINE;
    mlcMixerLineControls: TMIXERLINECONTROLS;
    pmcMixerControl: PMIXERCONTROL;
    mcdMixerData: TMIXERCONTROLDETAILS;
    pmcdsMixerDataSigned: PMIXERCONTROLDETAILSSIGNED;
}
    function    GetMasterVolume64Kibi:Integer;
//  function    GetStatus(StatusItem: DWord): DWord;
    function    GetPositionMilliSeconds:Integer;
    procedure   RestoreVolume;
    function    SafePlay:Boolean;
    procedure   SaveVolume;
    procedure   SetPositionMilliSeconds(PositionMilliSeconds__:Integer);
    procedure   SetVolume64Kibi(Volume64Kibi__:Integer);
    procedure   SetMasterVolume64Kibi(Volume64Kibi__:Integer);

  public
    CDDrive             :Char;
    Enabled             :Boolean;
    FileList            :TFileListBox;
    FileListOk          :Boolean;
    FilterIndex         :Integer;
    FirstTime           :Boolean;
    LengthInMilliSeconds:Integer;
    MusicSource         :TMusicSource;
    MusicFilePath       :String;
    PlayList            :TStringList;
    PlayListFileName    :String;
    MusicFileNo         :Integer;
    MusicFileTypeFilter :String;
    Player              :TMediaPlayer;
    Playing             :Boolean;
    TrackEnd            :Integer;
    TrackFileName       :String;
    TrackNo             :Integer;
    TrackStart          :Integer;
    TrackLength         :Integer;
    WindowsMediaPath    :String;

    property    MasterVolume64Kibi:Integer read GetMasterVolume64Kibi write SetMasterVolume64Kibi;
    property    PositionMilliSeconds:Integer read GetPositionMilliSeconds write SetPositionMilliSeconds;
    property    Volume64Kibi:Integer read fVolume64Kibi write SetVolume64Kibi;

    function    CheckIfFileAppeared:Boolean;
    procedure   Close;
    constructor Create(Parent:TWinControl);
    function    CreatePlayer(OnNotify__:TNotifyEvent):Boolean;
    destructor  Destroy; override;
    procedure   FileListToPlayList;
    procedure   Finalize;
    function    FullFileName(Index:Integer):String;
    function    GetVolume(var VolLeft, VolRight : Integer):Boolean;
    procedure   Initialize;
    function    LengthToMilliSeconds(Length:Integer; TimeFormat:TMPTimeFormats):Integer;
    function    LoadFromFile(const FileName__:String):Boolean;
    function    LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    procedure   NextFrame;
    procedure   NextTrack;
    function    ModeOK(Mode:TMPModes):Boolean;
    function    OpenPrevNextTrack(Direction:Integer):Boolean;
    procedure   OnActivateApplication;
    procedure   OnDeactivateApplication;
    procedure   Pause;
    function    PrettyTrackName(const FileName:String):String;
    function    Play:Boolean;
    function    PlayerIsPlaying(Player:TMediaPlayer):Boolean;
    procedure   PlayerNotify(Sender:TObject);
    function    PositionToString(PositionMilliSeconds__:Integer):String;
    procedure   PrevFrame;
    procedure   PrevTrack;
    procedure   Rewind;
    function    SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
    procedure   SetDefaultValues;
    function    SetVolume(VolLeft, VolRight : Integer):Boolean;
    procedure   ShowStatus;
    procedure   Stop(Wait__:Boolean);
    function    UpdateFileList(MusicSource:TMusicSource):Boolean;
    procedure   UpdateStatus(EnabledEvenIfNotPlaying:Boolean);
    function    Volume64KibiToString(Volume64Kibi__: Integer): String;
//  function    VUMeterStart:Boolean;
//  function    VUMeterStop:Boolean;
  end;

function  IsAPlayListFile(const FileName:String):Boolean;
//function  PlayListLoadFromFile(const FileName:String; var Grid:TStringGrid; ColNo:Integer):Boolean;
function  PlayListLoadFromFile(const FileName:String; var PlayList:TStringList):Boolean;
function  PlayListNewFileName(const Path:String):String;
//function  PlayListSaveToFile(const FileName:String; const Grid:TStringGrid; ColNo:Integer):Boolean;
function  PlayListSaveToFile(const FileName,Path:String; const PlayList:TStringList):Boolean;

implementation

uses SysUtils,Forms,SokUtil_,Text_,Sound_,Main_,Game_,Status_,Open1_,Options_,Tools_;

const
  DEFAULT_MUSIC_ENABLED        = False;
  DEFAULT_MUSIC_FILE_NAME      = 'Start.wav';
  DEFAULT_MUSIC_SOURCE         = msFolder; //msCD;
  MUSIC_INIFILE_SECTION        = 'Music'; // don't localize


(*
function  TMusicManager.VUMeterStart:Boolean;
begin
  Result:=False;
  if not VUMeterStop then exit;

  if waveOutGetNumDevs<1 then begin
     Error('No Wave Devices to open',Application.Title);
     exit;
     end;

  if mixerGetID(0, MxId, MIXER_OBJECTF_WAVEOUT)<>MMSYSERR_NOERROR then begin
     Error('Unable to get mixer ID',Application.Title);
     exit;
     end;

  if mixerOpen(@hMx, MxId, 0, 0,MIXER_OBJECTF_WAVEOUT)<>MMSYSERR_NOERROR then begin
     Error('Undefined mixer error',Application.Title);
     exit;
     end;

  with mlMixerLine do begin
     cbStruct:=SizeOf(TMIXERLINE);
     dwComponentType:=MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT;
     end;
  if mixerGetLineInfo(hMx, @mlMixerLine,MIXER_GETLINEINFOF_COMPONENTTYPE)<>MMSYSERR_NOERROR then begin
     Error('Cannot get mixer line info',Application.Title);
     VUMeterStop; exit;
     end;

  GetMem(pmcMixerControl, SizeOf(TMIXERCONTROL));
  with mlcMixerLineControls do begin
    cbStruct:=SizeOf(TMIXERLINECONTROLS);
    dwLineID:=mlMixerLine.dwLineID;
    dwControlType:=MIXERCONTROL_CONTROLTYPE_PEAKMETER;
    cControls:=1;
    cbmxctrl:=SizeOf(TMIXERCONTROL);
    pamxctrl:=pmcMixerControl;
    end;

  if mixerGetLineControls(hMx, @mlcMixerLineControls,MIXER_GETLINECONTROLSF_ONEBYTYPE)<>MMSYSERR_NOERROR then begin
     Error('Unable to get line controls.',Application.Title);
     VUMeterStop; exit;
     end;

  GetMem(pmcdsMixerDataSigned, 2*SizeOf(TMIXERCONTROLDETAILSSIGNED));
  with mcdMixerData do begin
    cbStruct:=SizeOf(TMIXERCONTROLDETAILS);
    dwControlID:=pmcMixerControl^.dwControlID;
    cChannels:=mlMixerLine.cChannels;
    cMultipleItems:=0;
    cbDetails:=SizeOf(TMIXERCONTROLDETAILSSIGNED);
    paDetails:=pmcdsMixerDataSigned;
    end;

  Result:=True;
end;

function TMusicManager.VUMeterStop:Boolean;
begin
  if pmcMixerControl     <>nil then FreeMem(pmcMixerControl);
  if pmcdsMixerDataSigned<>nil then FreeMem(pmcdsMixerDataSigned);

  Result:=(hMx=0) or (mixerClose(hMx)<>MMSYSERR_NOERROR);

  hMx:=0; pmcMixerControl:=nil; pmcdsMixerDataSigned:=nil;

  if not Result then begin
     Error('Warning !!! Unable to close mixer.Wave device may be unoperative until next reboot.',Application.Title);
     exit;
     end;
end;
*)

var
  MasterVolume                 : Integer = -1;    // -1: master volume not available
  MasterVolumeChanged          : Boolean = False;
  // if 'True' then the master volume has been changed by this application,
  // in which case the original value is restored upon exit;
  // an important scenario where the application shouldn't restore the value is
  // this one: sound effects have been disabled throughout the session and the
  // sound volume has never been touched by this application, but the user has
  // changed the sound volume via a parallel running music player;

  MixerControl                 : TMixerControl;
  MixerControlDetails          : TMixerControlDetails;
  MixerControlDetailsUnsigned  : TMixerControlDetailsUnsigned;
  MMResultCode                 : MMResult;

  MixerLine                    : TMixerLine;
  MixerLineControls            : TMixerLineControls;

function  GetMasterVolume:Integer;
begin
  if   (MasterVolume<>-1) and
       (mixerGetControlDetails(0,@MixerControlDetails,MIXER_GETCONTROLDETAILSF_VALUE)=MMSYSERR_NOERROR) then
       MasterVolume:=MixerControlDetailsUnsigned.dwValue
  else MasterVolume:=-1;
  Result:=MasterVolume;
end;

function  SetMasterVolume(Value: Integer):Integer;
begin
  Result:=MasterVolume;
  MixerControlDetailsUnsigned.dwValue:=Value;
  if   (MasterVolume<>-1) and
       (mixerSetControlDetails(0,@MixerControlDetails,MIXER_SETCONTROLDETAILSF_VALUE)=MMSYSERR_NOERROR) then
       MasterVolume:=Value
  else MasterVolume:=-1;
  MasterVolumeChanged:=MasterVolumeChanged or (Result<>MasterVolume);
  Result:=MasterVolume;
end;

procedure InitializeMasterVolume;
begin
  MasterVolume:=0;
  ZeroMemory(@MixerLine, SizeOf(MixerLine));
  MixerLine.cbStruct := SizeOf(MixerLine);
  MixerLine.dwComponentType := MIXERLINE_COMPONENTTYPE_DST_SPEAKERS;
  MMResultCode := mixerGetLineInfo(0,@MixerLine,MIXER_GETLINEINFOF_COMPONENTTYPE);
  if   MMResultCode = MMSYSERR_NOERROR then begin
       ZeroMemory(@MixerLineControls, SizeOf(MixerLineControls));
       MixerLineControls.cbStruct := SizeOf(MixerLineControls);
       MixerLineControls.dwLineID := MixerLine.dwLineID;
       MixerLineControls.cControls := 1;
       MixerLineControls.dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
       MixerLineControls.cbmxctrl := SizeOf(MixerControl);
       MixerLineControls.pamxctrl := @MixerControl;
       MMResultCode := mixerGetLineControls(0,@MixerLineControls,MIXER_GETLINECONTROLSF_ONEBYTYPE);
       end;
  if   MMResultCode = MMSYSERR_NOERROR then with MixerControlDetails do begin
       cbStruct := SizeOf(MixerControlDetails);
       dwControlID := MixerControl.dwControlID;
       cChannels := 1;  // set all channels
       cMultipleItems := 0;
       cbDetails := SizeOf(MixerControlDetailsUnsigned);
       paDetails := @MixerControlDetailsUnsigned;
       end;
  if   MMResultCode = MMSYSERR_NOERROR then
       MasterVolume := GetMasterVolume
  else MasterVolume := -1;
end;

constructor TMusicManager.Create(Parent:TWinControl);
begin
  Self.Parent:=Parent;
  Player:=nil; FileList:=nil; PlayList:=nil; FirstTime:=True; fVolume64Kibi:=0;
  Enabled:=False; TrackFileName:=''; OldTrackLength:=-1; OldTrackPosition:=-1;
  FileListOk:=False;
  TrackStart:=-1; TrackEnd:=-1; LengthInMilliSeconds:=1; Playing:=False;
  if   Assigned(MainForm.Sound) and (MainForm.Sound.WindowsMediaPath<>'') then
       WindowsMediaPath:=MainForm.Sound.WindowsMediaPath
  else WindowsMediaPath:=GetMediaPath(CSIDL_MYMUSIC);
  CreatePlayer(PlayerNotify);
  SetDefaultValues;

  //hMx:=0; pmcMixerControl:=nil; pmcdsMixerDataSigned:=nil;

  Enabled:=False; // kludge: don't start music on create
  Initialize;
  Enabled:=DEFAULT_MUSIC_ENABLED;
  SaveVolume;
  fVolume64Kibi:=Max(0,fExitMasterVolume div 2);
end;

destructor  TMusicManager.Destroy;
begin
  RestoreVolume;
  Finalize;
  if Player  <>nil then begin Player  .Free; Player  :=nil; end;
  if FileList<>nil then begin FileList.Free; FileList:=nil; end;
  if PlayList<>nil then begin PlayList.Free; PlayList:=nil; end;
end;

procedure   TMusicManager.Finalize;
begin
//VUMeterStop;
  FileListOk:=False;
  if Player<>nil then with Player do begin
     OnNotify:=nil;

     if ModeOK(Mode) and
        ((Mode=mpPlaying) or (Mode=mpStopped)) and
        (Player.FileName<>'') and
        FileExists(Player.FileName) then begin
        Self.Stop(True);
        TrackFileName:=Player.FileName;
        OldTrackLength:=Self.TrackLength;
        OldTrackPosition:=Player.Position;
        end;

     Self.Close;
     end;
end;

procedure   TMusicManager.Initialize; // initialize (and continue) play
begin
  Finalize;
//if EditForm<>nil then Parent:=EditForm; // kludge: use 'EditForm' as parent, because it isn't visible when 'MPlayer' is visible; hence, 'Music' can be destroyed without flicker
  if ToolsForm<>nil then Parent:=ToolsForm; // kludge: use 'ToolsForm' as parent, because it isn't visible when 'MPlayer' is visible; hence, 'Music' can be destroyed without flicker

  if FileList=nil then
     try    FileList:=TFileListBox.Create(nil);
            FileList.Parent:=Parent;
            FileList.Visible:=False;
     except on E:Exception do begin FileList.Free; FileList:=nil; end;
     end;

  if PlayList=nil then
     try    PlayList:=TStringList.Create;
            PlayList.Sorted:=False;
     except on E:Exception do begin PlayList.Free; PlayList:=nil; end;
     end;


  if Enabled and (FileList<>nil) and (PlayList<>nil) then
     if CreatePlayer(PlayerNotify) then begin
        Player.FileName:=TrackFileName;
        UpdateFileList(MusicSource);
        Volume64Kibi:=fVolume64Kibi;

        if (PlayList.Count>0) and
           OpenPrevNextTrack(0) and
           (Player<>nil) then begin
           if (TrackFileName<>'') and (TrackFileName=Player.FileName) and
              (OldTrackPosition<>-1) and
              (OldTrackLength=Self.TrackLength) and
              (OldTrackPosition>=Player.StartPos) and
              (OldTrackPosition<=Player.EndPos) then
              Player.StartPos:=OldTrackPosition;
           TrackFileName:='';
           OldTrackLength:=-1;
           OldTrackPosition:=-1;
           Self.Play;
           if FirstTime then begin // do first time initialization here
              FirstTime:=False;
              end;
           end;
        end;
end;

procedure TMusicManager.FileListToPlayList;
var i:Integer; ErrorMode:Cardinal;
begin
  FileListOk:=DirectoryExists(FileList.Directory);
  if   FileListOk then  begin // otherwise 'Update' raises an I/O Error
       ErrorMode := SetErrorMode(SEM_FailCriticalErrors+SEM_NOOPENFILEERRORBOX); //
       try     FileList.Update;
       finally SetErrorMode(ErrorMode); // restore old error mode
       end;
       end
  else FileList.Items.Clear;
  PlayList.Clear;
  for i:=0 to Pred(FileList.Items.Count) do
      PlayList.Add(FileList.Items[i]);
end;

function TMusicManager.FullFileName(Index:Integer):String;
begin
  if   (PlayList<>nil) and (Index<PlayList.Count) then with PlayList do
       if   MusicSource=msPlayList then Result:=Strings[Index]
       else Result:=StrWithTrailingPathDelimiter(FileList.Directory)+Strings[Index]
  else Result:='';
end;

function  TMusicManager.UpdateFileList(MusicSource:TMusicSource):Boolean;
var s:String;
begin
  Result:=False; FileListOk:=False;
  if (FileList<>nil) and (PlayList<>nil) then
     try    PlayList.Clear;
            case MusicSource of
              msCD          : begin
                                 s:=CDDrive+COLON;
                                 if   DirectoryExists(s) then begin // otherwise 'set directory' displays the message 'I/O Error' on failure
                                      FileList.Directory:=s;

                                      s:=MusicFileTypeFilter;
                                      if System.Pos(CD_AUDIO_FILE_EXT,s)=0 then begin
                                         if s<>'' then s:=s+SEMICOLON;
                                         s:=s+STAR+CD_AUDIO_FILE_EXT;
                                         end;
                                      FileList.Mask     :=s;

                                      FileListToPlayList;

                                      Result:=True;
                                      end
                                 else begin FileList.Items.Clear;
                                            FileList.Directory:='';
                                      end;
                           end;
              msFolder   : begin //if not DirectoryExists(WithoutTrailingBackslash(MusicFilePath)) then
                                 //   MusicFilePath:=MainForm.ApplicationDataPath;
                                 s:=StrWithoutTrailingPathDelimiter(MusicFilePath);

                                 if   DirectoryExists(s) then begin
                                      FileList.Directory:=s;
                                      FileList.Mask     :=MusicFileTypeFilter;

                                      FileListToPlayList;

                                      Result:=True;
                                      end
                                 else begin FileList.Items.Clear;
                                            FileList.Directory:='';
                                            if (MusicFilePath<>'') and
                                               (GetDriveType(PChar(UpCase(MusicFilePath[1])+':\'))=DRIVE_FIXED) then
                                               MusicFilePath:='';
                                      end;
                           end;
              msPlayList : begin
                             if   (PlayListFileName<>'') and
                                  FileExists(PlayListFileName) and
                                  PlayListLoadFromFile(PlayListFileName,PlayList) then begin
                                  FileList.Directory:=StrWithoutTrailingPathDelimiter(ExtractFilePath(PlayListFileName));
                                  //FileList.Directory:=StrWithoutTrailingPathDelimiter(PlayListFileName);
                                  FileListOk:=True; // for speed, so 'MPlayerForm.Timer1Timer' can handle all kinds of music-source the same way
                                  Result:=True;
                                  end
                             else begin PlayListFileName:='';
                                  end;
                           end;
            end; //case;
  except on E:Exception do begin
            Result:=Error(E.Message,Application.Title);
            PlayList.Clear; FileListOk:=False;
            end;
  end;
end;

procedure TMusicManager.UpdateStatus(EnabledEvenIfNotPlaying:Boolean);
begin
  Enabled:=EnabledEvenIfNotPlaying or Playing;
  if (Player<>nil) and
     (Player.FileName<>'') and FileExists(Player.FileName) then
     if (MusicSource=msPlayList)    and (PlayList.IndexOf(Player.FileName)>=0) then
        MusicSource:=msPlayList
     else if (MusicSource=msFolder) and (FileList.Items.IndexOf(ExtractFileName(Player.FileName))>=0) then
        MusicSource:=msFolder
     else if GetDriveType(PChar(AnsiUpperCase(System.Copy(Player.FileName,1,3))))=DRIVE_CDROM then
        MusicSource:=msCD
     else begin
        MusicSource:=msFolder;
        MusicFilePath:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Player.FileName));
        end;
end;

function  TMusicManager.CreatePlayer(OnNotify__:TNotifyEvent):Boolean;
begin
  try    if Player=nil then begin
            Player:=TMediaPlayer.Create(nil);
            Player.Parent:=Parent;
            Player.Visible:=False;
            //Player.Shareable:=True;
            Player.FileName:='';
            end;
         Player.OnNotify:=OnNotify__;
  except on E:Exception do begin
           Player.Free; Player:=nil;
           Error(E.Message,'');
           end;
  end;
  Result:=Player<>nil;
end;

function  TMusicManager.OpenPrevNextTrack(Direction:Integer):Boolean;
var StopNo:Integer; s:String;
begin
  Result:=False;
  if Enabled and (Player=nil) then CreatePlayer(PlayerNotify);

  if Enabled and (Player<>nil) and (FileList<>nil) and (PlayList<>nil) then begin
     Self.Stop(True);

     if   MusicSource=msPlayList then FileListOk:=True
     else FileListToPlayList;

     if PlayList.Count>0 then begin
        if Player.FileName='' then
           if   (TrackFileName<>'') and (FileExists(TrackFileName)) then
                Player.FileName:=TrackFileName
           else begin Player.FileName:=FullFileName(0); Direction:=0;
                      TrackFileName:='';
                end;

        if   MusicSource=msPlayList then
             TrackNo:=Succ(PlayList.IndexOf(Player.FileName))
        else TrackNo:=Succ(PlayList.IndexOf(ExtractFileName(Player.FileName)));

        if   (TrackNo<1) or (TrackNo>PlayList.Count) then TrackNo:=1;
        //else if SwFirst and (TrackFileName<>'') then Inc(TrackNo) // first time: skip the last track the user listened to

        if   Direction=0 then
             if   TrackNo=1 then
                  StopNo:=PlayList.Count
             else StopNo:=Pred(TrackNo)
        else      StopNo:=TrackNo;

        repeat Inc(TrackNo,Direction); if Direction=0 then Direction:=1;
               if      TrackNo<1 then
                       TrackNo:=PlayList.Count
               else if TrackNo>PlayList.Count then
                       TrackNo:=1;

               s:=FullFileName(Pred(TrackNo));
               if FileExists(s) and LoadFromFile(s) then begin
                  Result:=True;
                  end;
        until  Result or (TrackNo=StopNo);

        if not Result then Player.FileName:='';
       end;
     end;
end;

function    TMusicManager.LoadFromFile(const FileName__:String):Boolean;
var TrackNo:Integer;

  function ExtractTrackNo(const FileName:String; var TrackNo:Integer):Boolean;
  var i,j:Integer; s:String;
  begin
    s:=ExtractFileNameWithoutExt(FileName); i:=Length(s); TrackNo:=0; j:=1;
    while (i>0) and (s[i]>='0') and (s[i]<='9') do begin
      Inc(TrackNo,j*(Ord(s[i])-Ord('0'))); j:=j*10; Dec(i);
      end;
    Result:=TrackNo>0;
  end;

begin // LoadFromFile
  Result:=False;
  if   (Player=nil) or (not Assigned(Player.OnNotify)) then
       CreatePlayer(PlayerNotify);

  if Player<>nil then with Player do
     try    Self.Stop(True);

            if   FileName__=DEFAULT_VALUE then
                 FileName:=WindowsMediaPath+DEFAULT_MUSIC_FILE_NAME
            else FileName:=FileName__;

            //MainForm.Status.Hint:=Format(OpenFileText__,[ExtractFileNameWithoutExt(FileName)]);

            DeviceType:=dtAutoSelect; TimeFormat:=tfMilliseconds;
            Wait:=True; Notify:=False; Open;

            if (Error=0) and (Mode<>mpNotReady) then
               if not (mpCanPlay in Capabilities) then begin
                  Wait:=True; Notify:=False; Close;
                  end
               else begin
                  TimeFormat:=tfTMSF; // try to set timeformat to track-minute-seconds; on failure, the mediaplayer ignores the setting

                  if (TimeFormat=tfTMSF) and
                     ExtractTrackNo(FileName,TrackNo) and
                     (TrackNo<=Player.Tracks) then begin
                     TimeFormat:=tfMilliseconds;
                     Self.TrackLength:=TrackLength[TrackNo];
                     StartPos:=TrackPosition[TrackNo];
                     end
                  else begin
                     TimeFormat:=tfMilliseconds;
                     Self.TrackLength:=Player.Length;
                     StartPos:=0;
                     end;
                  EndPos:=StartPos+Self.TrackLength;
                  LengthInMilliSeconds:=Max(1,LengthToMilliSeconds(Self.TrackLength,TimeFormat));
                  TrackStart:=StartPos; TrackEnd:=EndPos;
                  Result:=TimeFormat=tfMilliseconds;
                  end;
     except on E:Exception do begin
                 //Self.Close; // wait until 'Finalize'; it might be an error to close a mediaplayer more than once
                 //Misc_.Error(Format(OpenFileFailedShortText__,[FileName__])+NL+NL+ReasonText+NL+E.Message,Application.Title);
                 Result:=False;
                 end;
     end;
end;

procedure TMusicManager.PlayerNotify(Sender:TObject);
begin
  if Sender=Player then with Player do begin
     SleepEx(50,False); // seems necessary to ensure that 'Mode' is updated
     ShowStatus;
     if   Playing and ModeOK(Mode) then
          if   (Mode=mpStopped) // seems not to be 100% reliable, hence, try to look at position too
               or
               ((PositionMilliSeconds>=LengthInMilliSeconds-10)
                and
                (LengthInMilliSeconds>=10)
               ) then
               if   OpenPrevNextTrack(+1) then
                    if   Self.Play then //
                    else Finalize
               else
          else if Mode in [mpNotReady,mpOpen] then begin
                  if   MusicSource=msPlayList then FileListOk:=False
                  else FileListToPlayList;
                  if Player<>nil then Player.FileName:='';
                  PositionMilliSeconds:=0;
                  end;

     Playing:=PlayerIsPlaying(Player);
     if Playing then Player.Notify:=True;
     if (MainForm<>nil) and MainForm.MPlayer.Visible then MainForm.MPlayer.ShowStatus;
     end;
end;

function    TMusicManager.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
{$IFDEF MUSIC_PLAYER}
  var i:Integer; s:String;
{$ENDIF}
begin
  Result:=True;
  {$IFDEF MUSIC_PLAYER}
    Enabled:=IniFile.ReadBool(MUSIC_INIFILE_SECTION,'Enabled',Enabled); //Enabled:=True;
    i:=IniFile.ReadInteger(MUSIC_INIFILE_SECTION,'MusicSource',Ord(MusicSource));
    if (i>=Ord(Low(MusicSource))) and (i<=Ord(High(MusicSource))) then
       MusicSource:=TMusicSource(i);
    s:=IniFile.ReadString(MUSIC_INIFILE_SECTION,'CD Drive',CDDrive);
    if Length(s)=1 then CDDrive:=AnsiUpperCase(s)[1];
    MusicFilePath:=KeepDataPathUpToDate(IniFile.ReadString(MUSIC_INIFILE_SECTION,'MusicFilePath',MusicFilePath));
    PlayListFileName:=KeepDataPathUpToDate(IniFile.ReadString(MUSIC_INIFILE_SECTION,'PlayListFileName',PlayListFileName));
    MusicFileTypeFilter:=IniFile.ReadString(MUSIC_INIFILE_SECTION,'MusicFileTypeFilter',MusicFileTypeFilter);
    TrackFileName:=KeepDataPathUpToDate(IniFile.ReadString(MUSIC_INIFILE_SECTION,'TrackFileName',''));
    OldTrackLength:=IniFile.ReadInteger(MUSIC_INIFILE_SECTION,'TrackLength',OldTrackLength);
    OldTrackPosition:=IniFile.ReadInteger(MUSIC_INIFILE_SECTION,'TrackPosition',OldTrackPosition);
    Volume64Kibi:=Max(0,Min(65535,IniFile.ReadInteger(MUSIC_INIFILE_SECTION,'Volume64Kibi',Volume64Kibi)));
  {$ELSE}
    Volume64Kibi:=Max(0,Min(65535,IniFile.ReadInteger(SOUND_INIFILE_SECTION,'Volume64Kibi',Volume64Kibi)));
  {$ENDIF}
end;

function    TMusicManager.SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
begin
  Result:=True;
  {$IFDEF MUSIC_PLAYER}
    IniFile.WriteBool   (MUSIC_INIFILE_SECTION,'Enabled',Enabled);
    IniFile.WriteInteger(MUSIC_INIFILE_SECTION,'MusicSource',Ord(MusicSource));
    IniFile.WriteString (MUSIC_INIFILE_SECTION,'CD Drive',CDDrive);
    IniFile.WriteString (MUSIC_INIFILE_SECTION,'MusicFilePath',MusicFilePath);
    IniFile.WriteString (MUSIC_INIFILE_SECTION,'PlayListFileName',PlayListFileName);
    IniFile.WriteString (MUSIC_INIFILE_SECTION,'MusicFileTypeFilter',MusicFileTypeFilter);
    IniFile.WriteString (MUSIC_INIFILE_SECTION,'TrackFileName',TrackFileName);
    IniFile.WriteInteger(MUSIC_INIFILE_SECTION,'TrackLength',OldTrackLength);     // caution: only valid if 'Finalize' is called before 'SaveSettingsToIniFile'
    IniFile.WriteInteger(MUSIC_INIFILE_SECTION,'TrackPosition',OldTrackPosition); // caution: only valid if 'Finalize' is called before 'SaveSettingsToIniFile'
    IniFile.WriteInteger(MUSIC_INIFILE_SECTION,'Volume64Kibi',Volume64Kibi);
  {$ELSE}
    IniFile.WriteInteger(SOUND_INIFILE_SECTION,'Volume64Kibi',Volume64Kibi);
  {$ENDIF}
end;

procedure   TMusicManager.SetDefaultValues;
var i:Integer; Drive:Char;
begin
  Enabled:=DEFAULT_MUSIC_Enabled;
  MusicSource:=DEFAULT_MUSIC_SOURCE;
  CDDrive:='D';
  for Drive:='A' to 'Z' do begin
      i:=GetDriveType(PChar(Drive+':\'));
      if i=DRIVE_CDROM then begin CDDrive:=Drive; break; end
      else if i=DRIVE_REMOVABLE then CDDrive:=Drive;
      end;
  MusicFilePath:=WindowsMediaPath;
  PlayListFileName:='';
  MusicFileTypeFilter:=DEFAULT_MUSIC_FILES_FILTER;
end;

procedure TMusicManager.NextTrack;
begin
  if OpenPrevNextTrack(+1) then Play;
end;

procedure TMusicManager.PrevTrack;
begin
  if OpenPrevNextTrack(-1) then Play;
end;

procedure TMusicManager.NextFrame;
begin
  if (Player<>nil) and ModeOK(Player.Mode) and (Player.Mode=mpPlaying) then with Player do begin
     Step;
     end;
end;

procedure TMusicManager.PrevFrame;
begin
  if (Player<>nil) and ModeOK(Player.Mode) and (Player.Mode=mpPlaying) then with Player do begin
     Back;
     end;
end;

function  TMusicManager.PlayerIsPlaying(Player:TMediaPlayer):Boolean;
begin
  Result:=(Player<>nil) and ModeOK(Player.Mode) and (Player.Mode=mpPlaying);
end;

function  TMusicManager.LengthToMilliSeconds(Length:Integer; TimeFormat:TMPTimeFormats):Integer;
begin
  case TimeFormat of
       tfMilliseconds: Result:=Length;
       tfMSF         : Result:=mci_MSF_Minute (Length)*  60000+mci_MSF_Second (Length)*1000;
       tfHMS         : Result:=mci_HMS_Hour   (Length)*3600000+mci_HMS_Minute (Length)*60000+mci_HMS_Second(Length)*1000;
       tfTMSF        : Result:=mci_TMSF_Minute(Length)*  60000+mci_TMSF_Second(Length)*1000;
       else            Result:=-1;
  end;
end;

procedure TMusicManager.ShowStatus;
var s,s1:String;
begin
  if (MainForm<>nil) and (MainForm.MPlayer<>nil) then
     if Player=nil then MainForm.Status.Hint:=''
     else if not MainForm.MPlayer.Visible then with Player do
             if   ModeOK(Mode) then
                  if   Mode=mpPlaying then begin
                       s:=PrettyTrackName(Player.FileName);
                       s1:=PositionToString(LengthInMilliseconds);
                       MainForm.Status.Hint:=Format(PlayingFileText__,[s])+'  '+s1;
                       end
                  else MainForm.Status.Hint:=MediaPlayerStateText[Mode]
             else      MainForm.Status.Hint:=MediaPlayerStateText[mpNotReady];
end;

procedure TMusicManager.Close;
begin
  Stop(True);
  if Player<>nil then with Player do begin Wait:=True; Notify:=False; Close; end;
  try      try    Player.Free;
                  // kludge: if Windows is shutting down then 'Player.Free'
                  // causes an exception like:
                  // "Access violation at address 00000073. Read of address 00000073"
                  //
                  // it's unlikely that this problem has been in the program from
                  // the start (it would have been found during debugging),
                  // so maybe it's a Windows XP compatibility issue;
           except on E:Exception do begin
                     // ignore exception
                     end;
           end;
  finally  Player:=nil;
  end;
end;

function  TMusicManager.SafePlay:Boolean;

  procedure DoPlay;
  begin // kludge: 'TMediaPlayer.Play' seems to cause several problems on error: 1) eats a stackframe, and 2) destroys the 'Mode' property
    Player.Play;
  end;

begin // SafePlay
  Playing:=False;
  if Player=nil then CreatePlayer(PlayerNotify);
  if (Player<>nil) and
     ModeOK(Player.Mode) and
     (Player.Mode<>mpNotReady) then with Player do
       try    if TrackEnd<>-1 then EndPos:=TrackEnd;
              if Volume64Kibi<>MasterVolume64Kibi then MasterVolume64Kibi:=Volume64Kibi;
              Wait:=False; Notify:=True; DoPlay; SleepEx(50,False);
              while ModeOK(Player.Mode) and (Player.Mode=mpSeeking) do;
              TrackFileName:=FileName;
       except on E:Exception do FileName:='';
       end;
  Playing:=PlayerIsPlaying(Player);
  Result:=Playing;
end;

function  TMusicManager.Play:Boolean;
begin
  Result:=Enabled and SafePlay;
  ShowStatus;
end;

procedure TMusicManager.Pause;
begin
  Playing:=False;
  if (Player<>nil) then with Player do
     if Mode=mpPlaying then begin Wait:=False; Notify:=False; PauseOnly; end;
end;

procedure TMusicManager.Stop(Wait__:Boolean);
begin
  Playing:=False;
  if Player<>nil then with Player do
     if Mode=mpPlaying then begin Wait:=Wait__; Notify:=False; Stop; end;
end;

procedure TMusicManager.Rewind;
begin
  Self.Stop(True);
  if Player<>nil then with Player do begin
     if TrackStart<>-1 then StartPos:=TrackStart;
     if TrackEnd  <>-1 then EndPos  :=TrackEnd;
     end;
end;
{
function TMusicManager.GetStatus(StatusItem : DWord) : DWord;
const ErrorMsg:String='MediaPlayer.GetStatus failed.';
var
  MyError, dwFlags: Longint;
  MyStatusParams: TMCI_Status_Parms;
begin
  if (Player<>nil) and (Player.DeviceId > 0) then with MyStatusParms do begin
    dwFlags := MCI_WAIT or MCI_STATUS_ITEM;
    MyStatusParms.dwCallback := Player.Handle; // TForm1.Handle
    MyStatusParms.dwItem := StatusItem;
    MyError := mciSendCommand(Player.DeviceId, MCI_STATUS, dwFlags,
                 Longint(@MyStatusParams));
    if MyError = 0 then Result := MyStatusParams.dwReturn
    else raise Exception.Create(ErrorMsg+NL+NL+IntToStr(MyError));
    end
  else raise Exception.Create(ErrorMsg+NL+NL+'No open file.');
end;
}

function  TMusicManager.GetVolume(var VolLeft, VolRight : Integer):Boolean;
{volume is returned as a pointer to a DWord with the least
 significant word for the left channel. The channels are
 extracted by treating the DWord as a two element array and
 accessing the two array elements for the Lt and Rt volumes}
var ResultCode : Integer; TempVol : TVol;
begin
  Result:=False;
  if (Player<>nil) and (Player.DeviceID<>0) then with Player do begin
     //ResultCode := AuxGetVolume(DeviceID, @TempVol);
     ResultCode := waveOutGetVolume({DeviceID} 0, @TempVol);
     if ResultCode = MMSYSERR_NOERROR then begin
        VolLeft := TempVol[vtLeft];
        VolRight := TempVol[vtRight];
        Result:=True;
        end
     else if ResultCode=MMSYSERR_BADDEVICEID then
             ;//Misc_.Msg(IntToStr(ResultCode),MB_OK);
     end;
end;

function  SetAuxVolume(Volume:DWORD):Boolean;
var Caps:TAUXCAPS;
begin
  Result:=(auxGetDevCaps    (0,@Caps,     SizeOf(Caps    ))=MMSYSERR_NOERROR) and
          ((Caps.dwSupport  and AUXCAPS_VOLUME)<>0) and
          (auxSetVolume     (0, Volume)=MMSYSERR_NOERROR);
end;

function  SetWaveOutVolume(Volume:DWORD):Boolean;
var Caps:TWAVEOUTCAPS;
begin
  Result:=(waveOutGetDevCaps (0,@Caps,     SizeOf(Caps    ))=MMSYSERR_NOERROR) and
          ((Caps.dwSupport   and WAVECAPS_VOLUME)<>0) and
          (waveOutSetVolume  (0, Volume)=MMSYSERR_NOERROR);
end;

function  SetMidiOutVolume(Volume:DWORD):Boolean;
var Caps:TMIDIOUTCAPS;
begin
  Result:=(midiOutGetDevCaps (0,@Caps,     SizeOf(Caps    ))=MMSYSERR_NOERROR) and
          ((Caps.dwSupport   and MIDICAPS_VOLUME)<>0) and
          (midiOutSetVolume  (0, Volume)=MMSYSERR_NOERROR);
end;

function  TMusicManager.SetVolume(VolLeft, VolRight : Integer):Boolean;
{volume is set by passing a DWord value with the most
 significant word set for the left channel, and the least
 significant word set for the right channel. The channels are
 set by treating the DWord as a two element  array and
 setting the two array elements for the Lt and Rt volumes}
var TempVol : TVol;
begin
  Result:=False;
  if (Player<>nil) {and (Player.DeviceId<>0)} then with Player do begin
     TempVol[vtLeft] := LoWord(VolLeft);
     TempVol[vtRight]:= LoWord(VolRight);

     SetAuxVolume    (LongInt(TempVol));
     SetWaveOutVolume(LongInt(TempVol));
     SetMidiOutVolume(LongInt(TempVol));

     Result:=True;
     end;
end;

function  TMusicManager.ModeOK(Mode:TMPModes):Boolean;
begin
  Result:=RangeCheck(Ord(Mode),Ord(Low(Mode)),Ord(High(Mode)));
end;

function TMusicManager.PrettyTrackName(const FileName:String):String;
var i,Len:Integer;
begin
  Result:=ExtractFileNameWithoutExt(FileName);

  Len:=System.Length(Result); i:=Len; // change 'TrackNN' to 'Track NN';
  while (i>1) and (Result[i]>='0') and (Result[i]<='9') do Dec(i);
  if (i>1) and (i<Len) and (Result[i]<>SPACE) then
     Insert(SPACE,Result,Succ(i));

  Len:=System.Length(Result); i:=1;
  while (i<Len) and // skip leading digits, i.e., manually entered track numbers
        (Result[i]>='0') and (Result[i]<='9') do Inc(i);
  if (i<Len) and (Result[i]=SPACE) then begin
     while (i<Len) and ((Result[i]=SPACE) or (Result[i]=HYPHEN)) do Inc(i);
     System.Delete(Result,1,Pred(i));
     end;
end;

function  TMusicManager.GetPositionMilliSeconds:Integer;
begin
  if {Playing and}
     (Player<>nil) and
     ModeOK(Player.Mode) and
     (Player.Mode<>mpNotReady) then with Player do begin
     if   LengthInMilliSeconds<=0 then
          Result:=Position-TrackStart // actually not seconds, but some kind of count
     else Result:=LengthToMilliSeconds(Position,TimeFormat)-TrackStart;
     end
  else Result:=0;
end;

procedure TMusicManager.SetPositionMilliSeconds(PositionMilliSeconds__:Integer);
//var H,M,S:Integer;

  procedure SetPosition(Position__:Integer; TimeFormat__:TMPTimeFormats);
  var oPlaying:Boolean; oTimeformat:TMPTimeFormats;
  begin
    if Player<>nil then with Player do begin
       oPlaying:=Playing; Self.Stop(True);
       oTimeFormat:=TimeFormat;
       if TimeFormat<>TimeFormat__ then TimeFormat:=TimeFormat__;
       try    Wait:=True; Notify:=False; Position:=Position__;
       except on E:Exception do;
       end;
       if TimeFormat<>oTimeFormat then TimeFormat:=oTimeFormat;
       Notify:=True;
       if oPlaying then SafePlay;
       end;
  end;

begin // SetPositionMilliSeconds
  if (MainForm<>nil) and (Screen.ActiveForm=MainForm) and Playing then
     MainForm.Status.Hint:=StrWithBrackets(PositionToString(PositionMilliSeconds__));
  if Player<>nil then with Player do
     if   LengthInMilliSeconds<=0 then
          if RangeCheck(PositionMilliSeconds__,0,Abs(LengthInMilliSeconds)) then
             SetPosition(PositionMilliSeconds__,TimeFormat)
          else
     else SetPosition(TrackStart+PositionMilliSeconds__,tfMilliSeconds);
end;

function  TMusicManager.PositionToString(PositionMilliSeconds__:Integer):String;
begin
  if   LengthInMilliSeconds<=0 then // should never happen. if it does, it's supposed to be a percentage measure
       Result:=Format('%d%%',[PositionMilliSeconds__ * 100 div Abs(LengthInMilliSeconds)])
  else Result:=Format('%d:%.2d',[PositionMilliSeconds__ div 60000,((PositionMilliSeconds__) div 1000) mod 60]);
end;

function  TMusicManager.GetMasterVolume64Kibi:Integer;
begin
  Result:=GetMasterVolume;
end;

procedure TMusicManager.SetMasterVolume64Kibi(Volume64Kibi__:Integer);
//var VolLeft,VolRight:Integer;
begin
{ fVolumePct:=VolumePct__;
  VolLeft:=65535; VolRight:=65535;
  VolLeft :=VolLeft *VolumePct div 100;
  VolRight:=VolRight*VolumePct div 100;
  SetVolume(VolLeft,VolRight);
}
  if fExitMasterVolume<>-1 then SetMasterVolume(Max(0,Min(65535,Volume64Kibi__)));
end;

procedure TMusicManager.SetVolume64Kibi(Volume64Kibi__:Integer);
//var VolLeft,VolRight:Integer;
begin
{ fVolumePct:=VolumePct__;
  VolLeft:=65535; VolRight:=65535;
  VolLeft :=VolLeft *VolumePct div 100;
  VolRight:=VolRight*VolumePct div 100;
  SetVolume(VolLeft,VolRight);
}
  if   (not Assigned(MainForm.Sound)) or MainForm.Sound.Enabled or Self.Playing then begin
       MasterVolume64Kibi:=Volume64Kibi__;
       fVolume64Kibi:=Max(0,MasterVolume64Kibi);
       end
  else fVolume64Kibi:=Max(0,Min(65535,Volume64Kibi__));
end;

function TMusicManager.Volume64KibiToString(Volume64Kibi__:Integer):String;
begin
  Result:=Format('[%d%%]',[Volume64Kibi__*100 div 65535]);
end;

function TMusicManager.CheckIfFileAppeared:Boolean;
begin
  if Enabled
     and
     (not FileListOk)
     and
     (not Playing)
     and
     (((MusicSource=msCD) and
        DirectoryExists(CDDrive+COLON)
      )
      or
      ((MusicSource=msFolder) and
        DirectoryExists(StrWithoutTrailingPathDelimiter(MusicFilePath))
      )
     )
     and
     UpdateFileList(MusicSource)
     and
     (PlayList.Count>0)
     and
     (Player<>nil) then begin
     Player.FileName:=''; TrackFileName:='';
     if OpenPrevNextTrack(0) then begin
        Self.Play;
        end;
     Result:=Playing;
     end
  else Result:=False;
end;

function  IsAPlayListFile(const FileName:String):Boolean;
begin
  Result:=(AnsiCompareText(ExtractFileExt(FileName),PLAYLIST_FILE_EXT)=0) and
          FileStartsWith(FileName,'['+PLAYLIST_INIFILE_SECTION+']');
end;
{
function PlayListLoadFromFile(const FileName:String; var Grid:TStringGrid; ColNo:Integer):Boolean;
var i:Integer; SL:TStringList; oCursor:TCursor;
begin
  Result:=False;
  try       SL:=TStringList.Create; oCursor:=Screen.Cursor;
    try     Screen.Cursor:=crHourGlass;
            SL.LoadFromFile(FileName);
            for i:=Pred(SL.Count) downto 0 do
                if SL.Strings[i]='' then SL.Delete(i);
            if (SL.Count>0) and
               (AnsiCompareText(SL.Strings[0],LEFT_BRACKET+PLAYLIST_FILE_SECTION+RIGHT_BRACKET)=0) then begin
               Grid.RowCount:=Max(1,Pred(SL.Count));
               Grid.Cells[ColNo,0]:='';
               for i:=1 to Pred(SL.Count) do
                   Grid.Cells[ColNo,Pred(i)]:=SL.Strings[i];
               Result:=True;
               end
            else begin
               Grid.RowCount:=1; Grid.Cells[ColNo,0]:='';
               end;
    finally SL.Free;
            Screen.Cursor:=oCursor;
    end;
  except on E:Exception do begin
            Grid.RowCount:=1; Grid.Cells[ColNo,0]:='';
            Result:=Error(E.Message,Application.Title);
            end;
  end;
end;
}

function PlayListLoadFromFile(const FileName:String; var PlayList:TStringList):Boolean;
var i:Integer; oCursor:TCursor;
begin
  Result:=False;
  if PlayList<>nil then with PlayList do
     try       oCursor:=Screen.Cursor;
       try     Screen.Cursor:=crHourGlass;
               LoadFromFile(FileName);

               if Count>0 then Delete(0); // first line is a playlist identification
               for i:=Pred(Count) downto 0 do
                   if (Strings[i]<>'')
                      and
                      (FileExists(Strings[i])
                       or
                       (GetDriveType(PChar(UpCase(Strings[i][1])+':\'))<>DRIVE_FIXED)) then //
                   else Delete(i);
               Result:=True;
       finally Screen.Cursor:=oCursor;
       end;
     except on E:Exception do begin
               PlayList.Clear;
               Result:=Error(E.Message,Application.Title);
               end;
     end;
end;

function PlayListNewFileName(const Path:String):String;
begin
  Result:=MakeNewFileName(StrWithTrailingPathDelimiter(Path)+PlayListFileNameText,PLAYLIST_FILE_EXT,True);
end;
{
function PlayListSaveToFile(const FileName:String; const Grid:TStringGrid; ColNo:Integer):Boolean;
var i:Integer; SL:TStringList;
begin
  try       SL:=TStringList.Create;
    try     SL.Add(LEFT_BRACKET+PLAYLIST_FILE_SECTION+RIGHT_BRACKET);
            for i:=0 to Pred(Grid.RowCount) do SL.Add(Grid.Cells[ColNo,i]);
            SL.SaveToFile(FileName);
            Result:=True;
    finally SL.Free;
    end;
  except on E:Exception do
            Result:=Error(E.Message,Application.Title);
  end;
end;
}
function PlayListSaveToFile(const FileName,Path:String; const PlayList:TStringList):Boolean;
var i:Integer; PathIfAny:String; SL:TStringList;
begin
  if   PlayList=nil then Result:=False
  else try       SL:=TStringList.Create;
         try     SL.Add(LEFT_BRACKET+PLAYLIST_INIFILE_SECTION+RIGHT_BRACKET);
                 PathIfAny:=Path;
                 if PathIfAny<>'' then PathIfAny:=StrWithTrailingPathDelimiter(PathIfAny);
                 for i:=0 to Pred(PlayList.Count) do
                     SL.Add(PathIfAny+PlayList.Strings[i]);
                 SL.SaveToFile(FileName);
                 Result:=True;
         finally SL.Free;
        end;
       except on E:Exception do begin
                 Result:=Error(E.Message,Application.Title);
                 DeleteFile(FileName);
                 end;
       end;
end;

procedure TMusicManager.SaveVolume;
begin
  fExitMasterVolume:=GetMasterVolume;
  OldMasterVolume  :=fExitMasterVolume;
  if auxGetVolume          (0,@ExitDeviceVolume[odAux       ])<>MMSYSERR_NOERROR then
     ExitDeviceVolume[odAux    ]:=$ffffffff;
  if waveOutGetVolume      (0,@ExitDeviceVolume[odWaveOut   ])<>MMSYSERR_NOERROR then
     ExitDeviceVolume[odWaveOut]:=$ffffffff;
  if midiOutGetVolume      (0,@ExitDeviceVolume[odMidiOut   ])<>MMSYSERR_NOERROR then
     ExitDeviceVolume[odMidiOut]:=$ffffffff;
end;

procedure TMusicManager.RestoreVolume;
begin
  if (fExitMasterVolume<>-1) and MasterVolumeChanged then
     SetMasterVolume(fExitMasterVolume);
  SetAuxVolume    (ExitDeviceVolume[odAux]);
  SetWaveOutVolume(ExitDeviceVolume[odWaveOut]);
  SetMidiOutVolume(ExitDeviceVolume[odMidiOut]);
end;

procedure TMusicManager.OnActivateApplication;
begin
  if OldMasterVolume<>MasterVolume64Kibi then begin
     //the user changed the sound master volume from another application
     OldMasterVolume:=MasterVolume64Kibi;

     // save the  value so this application can restore it upon exit
     // (it's commented out because it doesn't work; the function is invoked
     // at a point where the volume is set according to the settings in this
     // application, hence, it destroys the value set from another application;
     //
     // if (fExitMasterVolume<>-1) and (OldMasterVolume<>-1) then
     //    fExitMasterVolume:=OldMasterVolume;

     // update the volume in this application with the new value;
     // it's a dilemma whether to update the volume when the user sets
     // the volume from another application;
     // the rule imposed here is that the new value is imported if:
     // 1) sound effects are enabled, or
     // 2) the music player is currently playing;
     // in both cases, the user most probably expects that the volume set from
     // the outside also should apply to this application
     if (not Assigned(MainForm.Sound)) or MainForm.Sound.Enabled or Self.Playing then
        fVolume64Kibi:=Max(0,Min(65535,OldMasterVolume));
     end;
end;

procedure TMusicManager.OnDeactivateApplication;
begin
  OldMasterVolume:=MasterVolume64Kibi; // save the current sound master volume; when the application is activated again, the sound master volume may have changed
end;

initialization
  InitializeMasterVolume;

end.


