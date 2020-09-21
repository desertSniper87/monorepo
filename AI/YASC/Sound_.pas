unit Sound_;

interface

uses MPlayer,Classes,Controls,IniFile_;

const
  SOUND_INIFILE_SECTION        = 'Sound'; // don't localize

type

  TSoundType                   = (stMove,stPush,stGoal,stBlockedStep,stSolution,stUndo,stJump,
                                  stRestartGame,stMenuOver,stMenuSelect);
                                 // 'Move' and 'Push' must come first; otherwise 'Options_' needs modifications

  TSoundManager = class
  private
    Parent:TWinControl;
    function    PlayerUserCount(p:TMemoryStream):Integer;
  public
    Enabled         :Boolean;
    Player          :array[TSoundType] of TMemoryStream;
    ResetSoundEffectsOnLoadingASkin
                    :Boolean;
    SoundEnabled    :array[TSoundType] of Boolean;
    SoundFileName   :array[TSoundType] of String;
    SoundFileTypeFilter : String;
    WindowsMediaPath:String;
    constructor Create(Parent:TWinControl);
    destructor  Destroy; override;
    procedure   Finalize;
    procedure   Initialize;
    function    LoadFromFile(const FileName__:String; Sound:TSoundType):Boolean;
    function    LoadFromResource(const ResourceName:String; Sound:TSoundType):Boolean;
    function    LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    procedure   Play(Sound:TSoundType);
    function    SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
    procedure   SetDefaultValues;
    procedure   Stop(Sound: TSoundType; Wait__: Boolean);
    procedure   StopAll;
    procedure   Test;
  end;

implementation

uses SysUtils,Windows,MMSystem,Forms,Misc_,SokUtil_,Text_,Main_,Game_,Res_;

const
  DEFAULT_RESET_SOUND_EFFECTS_ON_LOADING_A_SKIN
                               = False;
  DEFAULT_SOUND_ENABLED        = True;
  DEFAULT_SOUND_FILE_NAME      : array[TSoundType] of String =
    ('Start.wav','Start.wav','Start.wav','Ding.wav',
     'Notify.wav','Start.wav','Start.wav','Start.wav','Start.wav','Start.wav');
  SOUND_TYPE_NAME              : array[TSoundType] of String =
    ('MoveSound','PushSound','GoalSound','IllegalMoveSound',
     'SolutionSound','UndoSound','JumpSound','RestartSound','MenuOverSound',
     'MenuSelectSound'); // don't localize

constructor TSoundManager.Create(Parent:TWinControl);
begin
  Self.Parent:=Parent;
  Enabled:=False;
  ResetSoundEffectsOnLoadingASkin:=False;
  FillChar(Player,SizeOf(Player),0);
  if   Assigned(MainForm.Music) and (MainForm.Music.WindowsMediaPath<>'') then
       WindowsMediaPath:=MainForm.Music.WindowsMediaPath
  else WindowsMediaPath:=GetMediaPath(CSIDL_MYMUSIC);
  SetDefaultValues;
end;

destructor  TSoundManager.Destroy;
begin
  Finalize;
end;

procedure   TSoundManager.Finalize;
var s,s1:TSoundType; p:TMemoryStream;
begin
  StopAll;
  for s:=Low(s) to High(s) do begin
      if Player[s]<>nil then begin
         p:=Player[s]; Player[s].Free;
         for s1:=Low(s1) to High(s1) do
             if Player[s1]=p then Player[s1]:=nil;
         end;
      end;
end;

procedure   TSoundManager.Initialize;
var s:TSoundType;
begin
  Finalize;
  if Enabled then
     for s:=Low(s) to High(s) do
         if SoundEnabled[s] then
            if   (AnsiCompareText(SoundFileName[s],DEFAULT_VALUE)=0) and
                 LoadFromResource(SOUND_RES_NAME[s],s) then //
            else LoadFromFile(SoundFileName[s],s);
end;

function    TSoundManager.LoadFromFile(const FileName__:String; Sound:TSoundType):Boolean;
var ErrorStr,Name:String; s:TSoundType;
begin
  Result:=False; Stop(Sound,True);
  try
    if   AnsiCompareText(FileName__,DEFAULT_VALUE)=0 then
         Name:=WindowsMediaPath+DEFAULT_SOUND_FILE_NAME[Sound]
    else Name:=FileName__;

    if   (Player[Sound]<>nil) and
         (Player[Sound].Memory<>nil) and
         (AnsiCompareText(SoundFileName[Sound],Name)=0) then
         Result:=True
    else begin
       for s:=Low(s) to High(s) do
           if (Player[s]<>nil) and
              (Player[s].Memory<>nil) and
              (AnsiCompareText(SoundFileName[s],Name)=0) then begin
              Stop(s,True); Result:=True;
              if PlayerUserCount(Player[Sound])=1 then Player[Sound].Free;
              Player[Sound]:=Player[s]; break;
              end;

       if (not Result) and FileExists(Name) then begin
          if Player[Sound]=nil then
             try    Player[Sound]:=TMemoryStream.Create;
             except on E:Exception do begin
                       Player[Sound].Free; Player[Sound]:=nil;
                    end;
             end;

          if Player[Sound]<>nil then
             try    Player[Sound].LoadFromFile(Name);
                    Result:=True;
             except on E:Exception do Result:=False;
             end;
          end;
       end;
  except
    on E:Exception do begin
       Result:=False;
       if FileName__='' then ErrorStr:=E.Message
       else ErrorStr:=Format(OpenFileFailedShortText__,[FileName__])+NL+NL+E.Message;
       Error(ErrorStr,Application.Title+' - '+SettingsText+' - '+SoundText+' - '+SOUND_TYPE_NAME[Sound]);
       end;
  end;

  if not Result then begin
     if PlayerUserCount(Player[Sound])<=1 then Player[Sound].Free;
     Player[Sound]:=nil;
     end;
end;

function    TSoundManager.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
var s:TSoundType;
begin
  Result:=True;

  Enabled:=IniFile.ReadBool(SOUND_INIFILE_SECTION,'Enabled',Enabled); //Enabled:=True;
  ResetSoundEffectsOnLoadingASkin:=IniFile.ReadBool(SOUND_INIFILE_SECTION,'ResetSoundEffectsOnLoadingASkin',ResetSoundEffectsOnLoadingASkin);
//SoundFileTypeFilter:=IniFile.ReadString(SOUND_SECTION_NAME,'SoundFileTypeFilter',SoundFileTypeFilter);
  for s:=Low(s) to High(s) do begin // continue;
      SoundFileName[s]:=KeepDataPathUpToDate(IniFile.ReadString (SOUND_TYPE_NAME[s],'FileName',SoundFileName[s]));
      SoundEnabled [s]:=IniFile.ReadBool   (SOUND_TYPE_NAME[s],'Enabled' ,SoundEnabled [s]);
      end;
end;

procedure TSoundManager.Play(Sound:TSoundType);
begin
  if (Player[Sound]<>nil) and SoundEnabled[Sound] then with Player[Sound] do
     if not MMSystem.PlaySound(Memory,0,SND_MEMORY+SND_ASYNC+{SND_PURGE+}SND_NOWAIT) then begin
        SoundEnabled[Sound]:=False;
        if PlayerUserCount(Player[Sound])<=1 then Player[Sound].Free;
        Player[Sound]:=nil;
        end;
end;

function  TSoundManager.PlayerUserCount(p:TMemoryStream):Integer;
var s:TSoundType;
begin
  Result:=0;
  if p<>nil then
     for s:=Low(s) to High(s) do
         if Player[s]=p then Inc(Result);
end;

function    TSoundManager.SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
var s:TSoundType;
begin
  Result:=True;
  IniFile.WriteBool(SOUND_INIFILE_SECTION,'Enabled',Enabled);
  IniFile.WriteBool(SOUND_INIFILE_SECTION,'ResetSoundEffectsOnLoadingASkin',ResetSoundEffectsOnLoadingASkin);  
  IniFile.WriteString(SOUND_INIFILE_SECTION,'SoundFileTypeFilter',SoundFileTypeFilter);
  for s:=Low(s) to High(s) do begin
      IniFile.WriteString (SOUND_TYPE_NAME[s],'FileName',SoundFileName[s]);
      IniFile.WriteBool   (SOUND_TYPE_NAME[s],'Enabled' ,SoundEnabled [s]);
      end;
end;

procedure   TSoundManager.SetDefaultValues;
var s:TSoundType;
begin
  Enabled:=DEFAULT_SOUND_ENABLED;
  ResetSoundEffectsOnLoadingASkin:=DEFAULT_RESET_SOUND_EFFECTS_ON_LOADING_A_SKIN;
  SoundFileTypeFilter:=DEFAULT_SOUND_FILES_FILTER;
  for s:=Low(s) to High(s) do begin
      SoundFileName[s]:=DEFAULT_VALUE;
      //SoundFileName[s]:=WindowsMediaPath+DEFAULT_SOUND_FILE_NAME[s]; //DEFAULT_VALUE;
      SoundEnabled [s]:=s in [stBlockedStep,stSolution];
      end;
end;

procedure TSoundManager.Stop(Sound:TSoundType; Wait__:Boolean);
begin
  if Player[Sound]<>nil then with Player[Sound] do
     MMSystem.PlaySound(nil,0,SND_PURGE); // kludge: this stops all sounds, not this one only
end;

procedure TSoundManager.StopAll;
begin
  MMSystem.PlaySound(nil,0,SND_PURGE);
end;

function  TSoundManager.LoadFromResource(const ResourceName:String; Sound:TSoundType):Boolean;
begin
  Player[Sound].Free;
  Player[Sound]:=MemoryStreamLoadFromResource(ResourceName,RC_WAV);
  Result:=Player[Sound]<>nil;
end;

procedure TSoundManager.Test;
var s:TSoundType;
begin
  for s:=Low(s) to High(s) do
      if LoadFromFile(SoundFileName[s],s) then begin
         MainForm.Status.Hint:=SOUND_TYPE_NAME[s];
         Application.ProcessMessages;
         Play(s);
         end;
  MainForm.Status.Hint:='';
end;

end.

