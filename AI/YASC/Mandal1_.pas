unit Mandal1_;
// this unit is not much more than an interface to
// the real 'Mandala-engine' in 'Mandal2_'

interface

uses Windows,Classes,Controls,ExtCtrls,Graphics,IniFile_,Menu_,Mandal2_;

type
  TMandalaButtonType = (mbStartStop,mbSaveAs,mbSettings);

  TMandalaState = (stNull,stReady,stZoom,stReSize,stCalculate);

  TMandala = class
  private
    { Private declarations }
    FEnabled   :Boolean;
    FSuspended :Boolean;

    function    SaveImage(const FileName: String): Boolean;
    procedure   SetDefaultValues;
  protected
    function    GetPaletteChangeEnabled:Boolean;
    function    GetPatternChangeEnabled:Boolean;
    procedure   SetEnabled(Enabled__:Boolean);
    procedure   SetPaletteChangeEnabled(PaletteChangeEnabled__:Boolean);
    procedure   SetPatternChangeEnabled(PatternChangeEnabled__:Boolean);
    procedure   SetSuspended(Suspended__:Boolean);
  public
    BitMap     :TBitMap; // kludge: should have been private; but 'OpenForm' uses it;
    Engine     :Mandal2_.TMandala; // the 'Mandala' image engine
    Menu       :TMandalaMenu;

    constructor Create(MenuPanel__:TPanel);
    destructor  Destroy; override;

    procedure   Finalize;
    procedure   Initialize;
    function    LoadPaletteFromMemory(Size:Integer; Data:Pointer):Boolean;
    function    LoadPatternFromMemory(Size:Integer; Data:Pointer):Boolean;
    function    LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
    function    NextPattern:Boolean;
    function    PaletteAndPatternSaved:Boolean;
    function    RestorePaletteAndPattern:Boolean;
    procedure   Resume;
    procedure   SaveImageAs;
    function    SavePaletteAndPattern:Boolean;
    function    SavePaletteToMemory(Size:Integer; Data:Pointer):Boolean;
    function    SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
    procedure   SetFixedSpeed(IntervalMS:Integer);
    procedure   Show;
    procedure   Suspend;
    procedure   ToggleSuspended;

    property    Enabled:Boolean read FEnabled write SetEnabled;
    property    PaletteChangeEnabled:Boolean read GetPaletteChangeEnabled write SetPaletteChangeEnabled;
    property    PatternChangeEnabled:Boolean read GetPatternChangeEnabled write SetPatternChangeEnabled;
    property    Suspended:Boolean read FSuspended write SetSuspended;
  end;

var
  Mandala : TMandala = nil;

implementation

uses SysUtils,Forms,Misc_,SokUtil_,Text_,Main_,BitMap_,Display_;

const
  MANDALA_INIFILE_SECTION='Mandala'; // don't localize

constructor  TMandala.Create(MenuPanel__:TPanel);
begin
  Menu:=nil; BitMap:=nil; FEnabled:=False; FSuspended:=True;
  try    Menu:=TMandalaMenu.Create(nil,MenuPanel__,MenuPanel__.Font.Size);
         if Engine<>nil then begin
            Engine.ShowTitleOnStartUp:=False;
            Engine.Title   :=ActivityText[acMandala];
            Engine.SubTitle:=MandalaSubTitleText;
            end;
  except begin Menu.Free; Menu:=nil;
         end;
  end;
  Engine:=Mandal2_.Mandala;  // the unit 'Mandal2_' is almost self-contained and is not visible to the rest of the program
  SetDefaultValues;
end;

destructor  TMandala.Destroy;
begin
  Finalize;
  Menu.Free;
  Engine:=nil;
  Inherited Destroy;
end;

function  TMandala.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
begin
  Result:=(Engine<>nil) and Engine.LoadSettingsFromIniFile(IniFile,MANDALA_INIFILE_SECTION)
          and
          (Menu  <>nil) and Menu  .LoadSettingsFromIniFile(IniFile,MANDALA_INIFILE_SECTION);
end;

function  TMandala.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
begin
  Result:=(Engine<>nil) and Engine.SaveSettingsToIniFile(IniFile,MANDALA_INIFILE_SECTION)
          and
          (Menu  <>nil) and Menu  .SaveSettingsToIniFile(IniFile,MANDALA_INIFILE_SECTION);
end;

procedure TMandala.SetDefaultValues;
begin
  if Menu<>nil then
     if    Menu.MenuPanel<>nil then
           Menu.SetDefaultValues(clBlack,Menu.MenuPanel.Font.Size)
     else  Menu.SetDefaultValues(clBlack,10);
end;

procedure TMandala.Initialize;
begin
  if MainForm<>nil then BitMap:=MainForm.MPlayer.Display.BitMaps[Ord(dlImage)];
  Engine.Initialize(BitMap);
end;

procedure TMandala.Finalize;
begin
  BitMap:=nil; // flag for 'not inititalized'
  Suspend;
end;

procedure TMandala.Show;
begin
  Engine.Paint;
end;

procedure TMandala.ToggleSuspended;
begin
  if   Suspended then Resume
  else Suspend;
end;

function  TMandala.NextPattern:Boolean;
begin
  Result:=(Engine<>nil) and Engine.NextPattern;
end;

procedure TMandala.SaveImageAs;
var oSuspended:Boolean;
begin
  if BitMap<>nil then with MainForm.MandalaSaveDialog do begin
     oSuspended:=Self.Suspended;
     try if not oSuspended then Suspend;
         if (InitialDir='') or
            (not DirectoryExists(StrWithoutTrailingPathDelimiter(InitialDir))) then
            InitialDir:=MainForm.ApplicationDataPath;
            FileName:=MakeNewFileName(StrWithTrailingPathDelimiter(InitialDir)+ActivityText[acMandala],BMP_FILE_EXT, False);
            if Execute then begin
               if (not FileExists(FileName)) or
                  (Application.MessageBox(PChar(Format(FileExistsText__+NL+NL+OverwriteItText,[FileName])),
                                          PChar(Title),
                                          MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)=IDYES)
                  then begin InitialDir:=ExtractFilePath(FileName);
                             SaveImage(FileName);
                       end;
               end;
     finally
       if not oSuspended then Resume;
     end;
     end;
end;

function  TMandala.SaveImage(const FileName:String):Boolean;
var s:String;
begin
  try
    if ExtractFileExt(FileName)='' then s:=FileName+BMP_FILE_EXT
    else s:=FileName;
    if BitMap<>nil then
       begin BitMap.SaveToFile(s);
             Result:=True;
       end
    else Result:=False;
  except
  on E:Exception do
     begin Application.MessageBox(PChar(E.Message),PChar(MainForm.MandalaSaveDialog.Title),MB_OK);
           Result:=False;
     end;
  end;
end;

function  TMandala.LoadPaletteFromMemory(Size:Integer; Data:Pointer):Boolean;
begin
  Result:=(Engine<>nil) and Engine.LoadPaletteFromMemory(Size,Data);
end;

function  TMandala.LoadPatternFromMemory(Size:Integer; Data:Pointer):Boolean;
begin
  Result:=(Engine<>nil) and Engine.LoadPatternFromMemory(Size,Data);
end;

function  TMandala.SavePaletteToMemory(Size:Integer; Data:Pointer):Boolean;
begin
  Result:=(Engine<>nil) and Engine.SavePaletteToMemory(Size,Data);
end;

procedure TMandala.SetEnabled(Enabled__:Boolean);
begin
  if Enabled__<>Enabled then begin
     FEnabled:=Enabled__;
     if not Enabled then Suspended:=True;
     end;
end;

function  TMandala.GetPaletteChangeEnabled:Boolean;
begin
  Result:=(Engine<>nil) and Engine.PaletteChangeEnabled;
end;

function  TMandala.GetPatternChangeEnabled:Boolean;
begin
  Result:=(Engine<>nil) and Engine.PatternChangeEnabled;
end;

procedure TMandala.SetPaletteChangeEnabled(PaletteChangeEnabled__:Boolean);
begin
  if Engine<>nil then Engine.PaletteChangeEnabled:=PaletteChangeEnabled__;
end;

procedure TMandala.SetPatternChangeEnabled(PatternChangeEnabled__:Boolean);
begin
  if Engine<>nil then Engine.PatternChangeEnabled:=PatternChangeEnabled__;
end;

procedure TMandala.SetSuspended(Suspended__:Boolean);
begin
  if   Suspended__ then Suspend
  else Resume;
end;

procedure TMandala.Resume;
begin
  if not Enabled then Initialize;
  FEnabled:=Engine<>nil;
  FSuspended:=not Enabled;
  if Enabled then Engine.Resume;
  if Menu.Visible then Menu.Show;
end;

procedure TMandala.Suspend;
begin
  FSuspended:=True;
  if Engine<>nil then Engine.Suspend;
  if Menu.Visible then Menu.Show;
end;

procedure TMandala.SetFixedSpeed(IntervalMS:Integer);
begin
  if Engine<>nil then Engine.SetFixedSpeed(IntervalMS);
end;

function  TMandala.SavePaletteAndPattern:Boolean;
begin
  Result:=(Engine<>nil) and Engine.SavePaletteAndPattern;
end;

function  TMandala.RestorePaletteAndPattern:Boolean;
begin
  Result:=(Engine<>nil) and Engine.RestorePaletteAndPattern;
end;

function  TMandala.PaletteAndPatternSaved:Boolean;
begin
  Result:=(Engine<>nil) and Engine.PaletteAndPatternSaved;
end;

end.

