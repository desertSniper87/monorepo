program Sokoban;

uses
  Windows,
  SysUtils,
  Forms,
  Messages,
  Main_ in 'Main_.pas' {MainForm},
  Game_ in 'Game_.pas',
  Help_ in 'Help_.pas' {HelpForm},
  Misc_ in 'Misc_.pas',
  Sprite_ in 'Sprite_.pas',
  Pict_ in 'Pict_.pas',
  Sound_ in 'Sound_.pas',
  Pack_ in 'Pack_.pas',
  Open1_ in 'Open1_.pas' {OpenForm},
  File_ in 'File_.pas' {FileForm},
  Menu_ in 'Menu_.pas',
  Status_ in 'Status_.pas',
  Text_ in 'Text_.pas',
  Options_ in 'Options_.pas' {OptionsForm},
  Music_ in 'Music_.pas',
  IniFile_ in 'IniFile_.pas',
  BitMap_ in 'BitMap_.pas',
  MPlayer1_ in 'MPlayer1_.pas',
  Display_ in 'Display_.pas',
  MPlayer2_ in 'MPlayer2_.pas' {MPlayerForm},
  Res_ in 'Res_.pas',
  Fworks_ in 'Fworks_.pas',
  IView1_ in 'IView1_.pas',
  Fractal_ in 'Fractal_.pas',
  Mandal1_ in 'Mandal1_.pas',
  Mandal2_ in 'Mandal2_.pas',
  Snapshots_ in 'Snapshots_.pas' {SnapshotsForm},
  SokFile_ in 'SokFile_.pas',
  SokGame_ in 'SokGame_.pas',
  SokUtil_ in 'SokUtil_.pas',
  GView_ in 'GView_.pas',
  Yantra_ in 'Yantra_.pas',
  Dead_ in 'Dead_.pas',
  Popup_ in 'Popup_.pas',
  Skins_ in 'Skins_.pas',
  LSView_ in 'LSView_.pas' {LevelSetForm},
  Tools_ in 'Tools_.pas' {ToolsForm},
  Log_ in 'Log_.pas',
  FileAssoc_ in 'FileAssoc_.pas',
  Plugin_ in 'Plugin_.pas',
  YASGen_ in 'YASGen_.pas',
  Generator_ in 'Generator_.pas' {GeneratorForm},
  Duplicates_ in 'Duplicates_.pas' {DuplicatesForm},
  Hash_ in 'Hash_.pas',
  MView_ in 'MView_.pas',
  PNG_ in 'PNG_.pas',
  Capture_ in 'Capture_.pas' {CaptureForm},
  Generator2_ in 'Generator2_.pas';

{$R *.RES}

function  CreateMutexes(const MutexName__: String; var Mutex__: THandle; var IsFirstInstance__:Boolean):Boolean;
{ Creates the two mutexes checked for by the installer/uninstaller to see if
  the program still is running.
  One of the mutexes is created in the global name space (which makes it
  possible to access the mutex across user sessions); the other is created in
  the session name space (because versions of Windows NT prior to 4.0 TSE don't
  have a global name space and don't support the 'Global\' prefix). }
const
  SECURITY_DESCRIPTOR_REVISION = 1;  { Win32 constant not defined in Delphi 3 }
var
  GlobalMutex : THandle;
  SecurityDesc: TSecurityDescriptor;
  SecurityAttr: TSecurityAttributes;
begin
  Result:=False; Mutex__:=0; IsFirstInstance__:=True;
  { By default on Windows NT, created mutexes are accessible only by the user
    running the process. We need our mutexes to be accessible to all users, so
    that the mutex detection can work across user sessions. To do this we use a
    security descriptor with a null DACL.
  }
  if InitializeSecurityDescriptor(@SecurityDesc, SECURITY_DESCRIPTOR_REVISION) and
     SetSecurityDescriptorDacl(@SecurityDesc, True, nil, False) then begin
     SecurityAttr.nLength := SizeOf(SecurityAttr);
     SecurityAttr.lpSecurityDescriptor := @SecurityDesc;
     SecurityAttr.bInheritHandle := False;
     Mutex__:=CreateMutex(@SecurityAttr, False, PChar(MutexName__));
     Result:=Mutex__<>0;
     if   Result and (GetLastError=ERROR_ALREADY_EXISTS) then
          IsFirstInstance__:=False;
     GlobalMutex:=CreateMutex(@SecurityAttr, False, PChar('Global\' + MutexName__));
     if   GlobalMutex=0 then
          Result:=False
     else Mutex__:=GlobalMutex; // use the global mutex instead of the user mutex
     end;
end;

function IsHandledByAnotherInstance:Boolean;
var AnotherInstanceWindowHandle:HWND; s:String; CopyData:TCopyDataStruct;
begin // IsHandledByAnotherInstance
  Result:=False;
  if (not Main_.IsFirstInstance) and
     (ParamCount>=1) and
     GetAnotherInstanceWindowHandle(AnotherInstanceWindowHandle) then begin
     s:=ExpandFileName(ParamStr(1));
     if IsAnIniFileSectionFileName(s) then s:=ExtractIniFileName(s);
     if (s<>'') and FileExists(s) then begin
        //SetForegroundWindow(AnotherInstanceWindowHandle);
        CopyData.dwData := 0;
        CopyData.cbData := Length(s);
        CopyData.lpData := Addr(s[1]);
        SendMessage(AnotherInstanceWindowHandle,WM_COPYDATA,HInstance,Integer(Addr(CopyData)));
        Result:=True;
        end;
     end;
end; // IsHandledByAnotherInstance

begin
  if GetDeviceCaps(GetDeskTopWindow,NUMCOLORS)>0 then
     Windows.MessageBox(0,TooFewColorsText,PChar(ExtractFileNameWithOutExt(ParamStr(0))),MB_OK+MB_ICONINFORMATION)
  else begin
     {
     if   (CreateMutex(NIL,False,APPLICATION_MUTEX_NAME)<>0) and
          (GetLastError=ERROR_ALREADY_EXISTS) then
          Main_.IsFirstInstance:=False
     else Main_.IsFirstInstance:=True;
     }
     CreateMutexes(APPLICATION_MUTEX_NAME,SokFile_.ApplicationMutex,Main_.IsFirstInstance);

     if not IsHandledByAnotherInstance then begin
        Application.Initialize;
        Application.Title := 'Sokoban';
        Application.CreateForm(TMainForm, MainForm);
  if not ((ParamCount>=1) and StrEqual(ParamStr(1),COMMAND_LINE_PARAMETER_INSTALL)) then begin
           Application.CreateForm(TFileForm, FileForm);
           Application.Run;
           end
        else begin // installation only, i.e., don't run the program
           //Msg(ApplicationHasBeenInstalledText,Application.Title,MB_ICONINFORMATION+MB_OK);
           if Assigned(MainForm) then begin
              if Assigned(MainForm.Game) then with MainForm.Game do LastValidFileName:=FileName;
              MainForm.CloseForm;
              MainForm.Free; MainForm:=nil;
              end;
           end;
        end;
     end;
end.
