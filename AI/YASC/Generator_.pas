unit Generator_;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Spin, Menus,
  YASGen_,IniFile_,Plugin_,SokUtil_,SokFile_,SokGame_;

type
  TGeneratorThread = class(TPluginThread)
  private
  protected
    procedure                SynchronizedPluginCallBackFunction; override;
    procedure                SynchronizedStartGeneratingLevel;
    procedure                SynchronizedStopGeneratingLevel;

  public
    constructor              Create(Plugin__:TPlugin);
    destructor               Destroy; override;

    procedure                Execute; override;
    procedure                SynchronizeStartGeneratingLevel;
    procedure                SynchronizeStopGeneratingLevel;
  end;

  TGenerator = class(TPlugin) // the only supported instance of this class is 'MainForm.Generator';
  private
    fPauseTimeMS           : TTimeMS;
  protected
    function                 AppendFromSokoFileOrGame(SokoFile__:TSokoFile; Game__:TSokoGame):Boolean;
    function                 GetCaption:String;
    function                 GetFileName:String;
    function                 GetIsFullCandidateSet:Boolean;
    function                 GetGeneratorLongName:String;
    function                 GetGeneratorName:String;
    function                 GetGeneratorNameAndVersion:String;
    function                 GetSessionStartTimeMS:TTimeMS;
    procedure                SetFileName(const FileName__:String);
  public
    Initialized             :Boolean;

    constructor              Create(const PluginTypeText__,DefaultPluginFileNameStub__:String; Button__:TButton; ComboBox__:TComboBox);
    destructor               Destroy; override;

    procedure                Clear;
    function                 CopyOrCutToClipboard(CutToClipBoard__:Boolean):Boolean;
    function                 ImportFromClipboard:Boolean;
    function                 InitializePositions(MemoryByteSize__:Integer):Boolean;
    function                 LoadFromFile(const FileName__:String):Boolean;
    function                 LoadFromGame(Game__:TSokoGame):Boolean;
    function                 LoadFromSokoFile(SokoFile__:TSokoFile):Boolean;
    function                 LoadSettingsFromIniFile(const SectionName__:String; const IniFile:TIniFile):Boolean; override;
    function                 ReleaseMemory:Boolean;
    function                 RemoveLevel(Level__:TLevel):Boolean;
    function                 Run:Boolean;
    function                 SaveCandidateToSokoFile(Level__:TLevel; SokoFile__:TSokoFile):Boolean;
    function                 SaveSettingsToIniFile(const SectionName__:String; const IniFile:TIniFile):Boolean; override;
    function                 SaveToFile(const FileName__:String):Boolean;
    procedure                ShowStatus;
    function                 Shutdown:Boolean; // don't confuse 'Shutdown with 'Terminate'; the latter merely stops a generator task; if the generator is running then 'Shutdown' terminates it in a way so it cannot run anymore
    procedure                SortCandidates(FitnessFunction__:TGAFitnessFunction);
    procedure                StartTimer; override;
    function                 StopTimer:TTimeMS; override;

    property                 Caption:String read GetCaption;
    property                 FileName:String read GetFileName write SetFileName;
    property                 GeneratorLongName:String read GetGeneratorLongName;
    property                 GeneratorName:String read GetGeneratorName;
    property                 GeneratorNameAndVersion:String read GetGeneratorNameAndVersion;
    property                 IsFullCandidateSet:Boolean read GetIsFullCandidateSet;
    property                 PauseTimeMS:TTimeMS read fPauseTimeMS write fPauseTimeMS;
    property                 SessionStartTimeMS:TTimeMS read GetSessionStartTimeMS;
  end;

type
  TGeneratorForm = class(TForm)
    PanelTop: TPanel;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    MenuItemOpenFile: TMenuItem;
    MenuItemSaveAsFile: TMenuItem;
    N3: TMenuItem;
    MenuItemDeleteFiles: TMenuItem;
    N1: TMenuItem;
    MenuItemExit: TMenuItem;
    HelpMenu: TMenuItem;
    MenuItemExit2: TMenuItem;
    GroupBox1: TGroupBox;
    Panel3: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    DefaultBtn: TButton;
    ApplyBtn: TButton;
    MenuItemHelp: TMenuItem;
    N2: TMenuItem;
    MenuItemAbout: TMenuItem;
    PageControl1: TPageControl;
    TabSheetEvolution: TTabSheet;
    TabSheetFitness: TTabSheet;
    TabSheetReproduction: TTabSheet;
    TabSheetRandomBoards: TTabSheet;
    TabSheetGenerator: TTabSheet;
    GroupBox3: TGroupBox;
    Label7: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    SpinEditPopulation: TSpinEdit;
    SpinEditBoxesGoals: TSpinEdit;
    SpinEditOpenPositions: TSpinEdit;
    SpinEditInactivityThreshold: TSpinEdit;
    GroupBox6: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    SpinEditBoardWidth: TSpinEdit;
    SpinEditBoardHeight: TSpinEdit;
    SpinEditInteriorWallsPct: TSpinEdit;
    GroupBox7: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    SpinEditGeneratorTranspositionTableSize: TSpinEdit;
    ComboBoxThreadPriority: TComboBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    RadioButtonLevelTypeRandom: TRadioButton;
    RadioButtonLevelTypeConnectedGoals: TRadioButton;
    CheckBoxOpenPositionsLimitEnabled: TCheckBox;
    Label3: TLabel;
    Label12: TLabel;
    RadioButtonFitnessPushes: TRadioButton;
    RadioButtonFitnessBoxLinesAndBoxChanges: TRadioButton;
    CheckBoxDiscountTailPushesEnabled: TCheckBox;
    SpinEditTailPushes: TSpinEdit;
    Label13: TLabel;
    Panel2: TPanel;
    RadioButtonForwardSearch: TRadioButton;
    Label14: TLabel;
    RadioButtonBackwardSearch: TRadioButton;
    Label15: TLabel;
    Label16: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    SpinEditCrossOverProbability: TSpinEdit;
    SpinEditMutationProbability: TSpinEdit;
    Label23: TLabel;
    Label24: TLabel;
    LabelInactivityThresholdHint1: TLabel;
    LabelInactivityThresholdHint2: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Panel1: TPanel;
    Memo1: TMemo;
    SpinEditMutationCount: TSpinEdit;
    Label25: TLabel;
    Label26: TLabel;
    Panel4: TPanel;
    Memo2: TMemo;
    Panel6: TPanel;
    Memo4: TMemo;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    CheckBoxInactivityThresholdEnabled: TCheckBox;
    CheckBoxBoxesGoalsLimitEnabled: TCheckBox;
    RadioButtonRandomSeed: TRadioButton;
    RadioButtonFixedRandomSeed: TRadioButton;
    SpinEditFixedRandomSeedNumber: TSpinEdit;
    CheckBoxAutoSavePeriodically: TCheckBox;
    Label8: TLabel;
    CheckBoxFixedTemplate: TCheckBox;
    LabelFixedTemplateExpiresAfter24Hours: TLabel;
    CheckBoxDiscardTrivialOpeningPushes: TCheckBox;
    Label10: TLabel;
    Label17: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemOpenFileClick(Sender: TObject);
    procedure MenuItemSaveAsFileClick(Sender: TObject);
    procedure MenuItemDeleteFilesClick(Sender: TObject);
    procedure MenuItemExit2Click(Sender: TObject);
    procedure MenuItemHelpClick(Sender: TObject);
    procedure DefaultBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure SpinEditExit(Sender: TObject);
  private
    { Private declarations }
    EscapeEnabled:Boolean;
    fSettingsDirectory:String;
    HintControl:TObject;
    Modified:Boolean;
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function  GetSettingsDirectory:String;
    procedure LoadData;
    procedure SetDefaultValues;
    procedure ShowStatus;
  protected
    procedure ApplicationOnHint(Sender: TObject);
    procedure ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
  public
    { Public declarations }
    FilterIndex:Integer; // for load/save settings dialog

    function  CommitOrDropChanges:Boolean;
    function  LoadSettingsFromFile(const FileName__:String):Boolean;
    procedure LoadSettingsFromGenerator;
    function  LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    procedure SaveData;
    function  SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
    function  SaveSettingsToFile(const FileName__:String):Boolean;
    procedure SaveSettingsToGenerator;

    property  SettingsDirectory:String read GetSettingsDirectory write fSettingsDirectory; // for load/save settings dialog
  end;

function  IsALevelGeneratorCandidateSet(SokoFile__:TSokoFile):Boolean;

var
  GeneratorForm: TGeneratorForm = nil;

implementation

{$R *.DFM}

uses
  Clipbrd,Text_,Misc_,Options_,Main_,Tools_, Open1_;

const
  MAX_SPIN_EDIT_TEXT_LENGTH     = 11; // length(min(integer))


function  IsALevelGeneratorCandidateSet(SokoFile__:TSokoFile):Boolean;
var Version:String;
begin // this is not fool-proof, but it will have to do
  Result:=Assigned(SokoFile__) and Assigned(SokoFile__.FileHeader) and
          Assigned(SokoFile__.FileHeader.Lines) and
          Assigned(SokoFile__.FileHeader.Lines.GetItemByName(StrWithBrackets(GA_TEXT_APPLICATION_TITLE_LONG))) and
          SokoFile__.FileHeader.Lines.ReadString(VersionText,Version) and (Version<>'');
end;

procedure TGeneratorForm.FormCreate(Sender: TObject);
var i:Integer; PhysicalMemoryByteSize:Cardinal; // TimeMS:TTimeMS;
begin
  //TimeMS:=GetTimeMS;
  StatusBar1.Font.Assign(Self.Font);
  Modified:=False; HintControl:=nil; EscapeEnabled:=True;
  PanelTop.Caption:=Trim(PanelTop.Caption)+SPACE;
  StatusBar1.Panels[1].Text:='';
  LoadComboBoxFromStrings(Ord(Low(TPluginThreadPriority)),Ord(High(TPluginThreadPriority)),ThreadPriorityText,Self.Canvas,ComboBoxThreadPriority);
  PageControl1.ActivePage:=TabSheetEvolution;
  FilterIndex:=1; SettingsDirectory:='';

  for i:=0 to Pred(ComponentCount) do
      if      Components[i] is TPanel       then with Components[i] as TPanel       do begin OnMouseUp:=FormMouseUp;    if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; end
      else if Components[i] is TLabel       then with Components[i] as TLabel       do begin OnMouseUp:=FormMouseUp;    if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; if (Hint='') and Assigned(FocusControl) then Hint:=FocusControl.Hint; end
      else if Components[i] is TButton      then with Components[i] as TButton      do begin OnMouseUp:=FormMouseUp;    if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; end
      else if Components[i] is TRadioButton then with Components[i] as TRadioButton do begin OnMouseUp:=FormMouseUp;    if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; OnClick:=RadioButtonClick; end
      else if Components[i] is TCheckBox    then with Components[i] as TCheckBox    do begin OnMouseUp:=FormMouseUp;    if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; OnClick:=CheckBoxClick; end
      else if Components[i] is TComboBox    then with Components[i] as TComboBox    do begin OnMouseUp:=FormMouseUp;    if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; end
      else if Components[i] is TGroupBox    then with Components[i] as TGroupBox    do begin OnMouseUp:=FormMouseUp;    if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; end
      else if Components[i] is TListBox     then with Components[i] as TListBox     do begin OnMouseUp:=FormMouseUp;    if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; end
      else if Components[i] is TMemo        then with Components[i] as TMemo        do begin OnMouseUp:=FormMouseUp;    if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; ScrollBars:=ssVertical; end
      else if Components[i] is TImage       then with Components[i] as TImage       do begin OnMouseUp:=FormMouseUp;    if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; end
      else if Components[i] is TSpinEdit    then with Components[i] as TSpinEdit    do begin OnMouseUp:=FormMouseUp;    if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove;
                                                                                             OnChange :=SpinEditChange; if not Assigned(OnExit     ) then OnExit     :=SpinEditExit;
                                                                                             if MaxLength=0 then MaxLength:=MAX_SPIN_EDIT_TEXT_LENGTH;
                                                                                             end
      else if Components[i] is TStatusBar   then with Components[i] as TStatusBar   do begin OnMouseUp:=FormMouseUp; if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; end
      else if Components[i] is TTabSheet    then with Components[i] as TTabSheet    do begin OnMouseUp:=FormMouseUp; if not Assigned(OnMouseMove) then OnMouseMove:=ControlMouseMove; end;

  SpinEditPopulation                     .MinValue:=1;
  SpinEditPopulation                     .MaxValue:=GA_MAX_INDIVIDUALS;
  SpinEditBoxesGoals                     .MinValue:=1;
  SpinEditBoxesGoals                     .MaxValue:=YASGen_.MAX_BOX_COUNT;
  SpinEditOpenPositions                  .MaxValue:=99999; // arbitrary limit, but the spinedit must be wide enough to contain the number
  SpinEditInactivityThreshold            .MaxValue:=ONE_THOUSAND; // arbitrary limit;
  SpinEditTailPushes                     .MinValue:=1;
  SpinEditTailPushes                     .MaxValue:=MAX_TAIL_POSITION_THRESHOLD;
  SpinEditMutationCount                  .MaxValue:=GA_MAX_MAX_MUTATION_COUNT;
  SpinEditBoardWidth                     .MinValue:=MIN_BOARD_WIDTH;
  SpinEditBoardWidth                     .MaxValue:=MAX_BOARD_WIDTH-2;
  SpinEditBoardHeight                    .MinValue:=SpinEditBoardWidth.MinValue;
  SpinEditBoardHeight                    .MaxValue:=MAX_BOARD_HEIGHT-2;
  SpinEditInteriorWallsPct               .MaxValue:=50; // (MAX_BOARD_WIDTH-2)*(MAX_BOARD_HEIGHT-2);
  SpinEditFixedRandomSeedNumber          .MaxValue:=High(SpinEditFixedRandomSeedNumber.MaxValue);
  SpinEditGeneratorTranspositionTableSize.MinValue:=1;
  PhysicalMemoryByteSize:=GetPhysicalMemoryByteSize;
  if PhysicalMemoryByteSize>Cardinal(MaxInt) then PhysicalMemoryByteSize:=Cardinal(MaxInt); // limit the transposition table byte size to a signed integer
  SpinEditGeneratorTranspositionTableSize.MaxValue:=(((PhysicalMemoryByteSize div 4)*3)+(ONE_MEBI div 2)) div ONE_MEBI;

//CheckBoxBoxesGoalsLimitEnabled         .Hint    :=SpinEditBoxesGoals.Hint;
  LabelInactivityThresholdHint2          .Hint    :=LabelInactivityThresholdHint1.Hint;

  SetDefaultValues; Modified:=False; SaveData;

  //Msg(IntToStr(CalculateElapsedTimeMS(TimeMS,GetTimeMS)),'',MB_OK);
end;

procedure TGeneratorForm.FormDestroy(Sender: TObject);
begin
//
end;

procedure TGeneratorForm.ApplicationOnHint(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := GetLongHint(Application.Hint);
end;

procedure TGeneratorForm.ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if   (Msg.Message = WM_MOUSEWHEEL) then begin
       if   ActiveControl is TComboBox then with ActiveControl as TComboBox do begin
            if Msg.WParam>=0 then begin
               if ItemIndex>0 then ItemIndex:=Pred(ItemIndex);
               end
            else begin
               if ItemIndex<Pred(Items.Count) then ItemIndex:=Succ(ItemIndex);
               end;
            Handled:=True;
            end
       else Handled:=False;
       end
  else Handled:=False;
end;

procedure TGeneratorForm.FormActivate(Sender: TObject);
begin
  Application.OnHint   :=ApplicationOnHint;
  Application.OnMessage:=ApplicationOnMessage;
  PanelTop.Font.Color  :=ApplicationHiglightedTextColor;
  ActiveControl        :=OKBtn;
  ApplyBtn.Tag         :=0; // 0: the data hasn't been updated by means of the 'Apply' button
  Modified             :=False;
  SaveData;
end;

procedure TGeneratorForm.FormDeactivate(Sender: TObject);
begin
//
end;

procedure TGeneratorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  CommitOrDropChanges;

  if      Sender=Self then
          Action:=caHide
  else    Action:=caFree;

  if   ModalResult=mrOk then
       if   Modified then
            SaveData;
end;

procedure TGeneratorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//
end;

procedure TGeneratorForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if        Key=VK_F1 then
            MenuItemHelpClick(Sender)
  else if   Key=VK_ESCAPE then
            if   EscapeEnabled then
                 if   Modified then begin
                      end
                 else CancelBtnClick(Sender)
            else EscapeEnabled:=True
  else if   Key=VK_F12 then
            if WindowState=wsMaximized then WindowState:=wsNormal
            else WindowState:=wsMaximized
  else;
end;

procedure TGeneratorForm.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TGeneratorForm.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
//
end;

procedure TGeneratorForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbRight) and (not Modified) then
     CancelBtnClick(Sender);
end;

procedure TGeneratorForm.ControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TGeneratorForm.ShowStatus;
begin
  if StatusBar1.Panels[0].Text<>OkChangedText[Modified] then
     StatusBar1.Panels[0].Text:=OkChangedText[Modified];
  if ApplyBtn.Enabled<>Modified then ApplyBtn.Enabled:=Modified;
end;

procedure TGeneratorForm.MenuItemOpenFileClick(Sender: TObject);
begin
  if SettingsDirectory='' then
     SettingsDirectory:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath)+DEFAULT_PLUGINS_DIRECTORY);
  OptionsForm.MenuItemOpenFileClick(Self);
end;

procedure TGeneratorForm.MenuItemSaveAsFileClick(Sender: TObject);
begin
  OptionsForm.MenuItemSaveAsFileClick(Self);
end;

procedure TGeneratorForm.MenuItemDeleteFilesClick(Sender: TObject);
var Result:Boolean;
begin
  if FilterIndex=2 then FilterIndex:=3; // the delete dialog on the settings-form has one more filter (*.sok) than the load/save settings dialogs
  with OptionsForm.OpenDialog2 do begin
     FilterIndex:=Self.FilterIndex;
     InitialDir:=Self.SettingsDirectory;

     Result:=DeleteFilesDialog(OptionsForm.OpenDialog2,Self,StatusBar1.Panels[1]);

     if Result then begin
        Self.FilterIndex:=FilterIndex;
        Self.SettingsDirectory:=ExtractFilePath(Files[0]);
        end
     end;
  if FilterIndex=3 then FilterIndex:=2; // back from 3 filters (*.set;*.sok;*.*) to 2 filters (*.set;*.*)
end;

procedure TGeneratorForm.MenuItemExit2Click(Sender: TObject);
begin
  Close;
end;

procedure TGeneratorForm.MenuItemHelpClick(Sender: TObject);
begin
  MainForm.BtnHelpClick(Self);
end;


procedure TGeneratorForm.DefaultBtnClick(Sender: TObject);
begin
  SetDefaultValues;
end;

procedure TGeneratorForm.ApplyBtnClick(Sender: TObject);
var oCursor:TCursor;
begin
  if {(not IsBusy) and} Modified and ApplyBtn.Enabled {and CloseEditors(False)} then begin
     oCursor:=Screen.Cursor;
     try     //IsBusy:=True;
             Screen.Cursor:=crHourGlass;
             ApplyBtn.Tag:=1; // 1: the data has been updated using the 'Apply' button
             SaveData;
     finally Screen.Cursor:=oCursor;
             //IsBusy:=False;
             ShowStatus;
     end;
     end;
end;

procedure TGeneratorForm.OKBtnClick(Sender: TObject);
begin
 SaveData;
 if not Modified then begin
    ActiveControl:=OKBtn; ModalResult:=mrOk; Close; ModalResult:=mrOk;
    end;
end;

procedure TGeneratorForm.CancelBtnClick(Sender: TObject);
var CloseAction:TCloseAction;
begin
  LoadData;
  if   ApplyBtn.Tag=0 then ModalResult:=mrCancel
  else ModalResult:=mrOk;
  if   Screen.ActiveForm=Self then Close
  else FormClose(Self,CloseAction);
  if   ApplyBtn.Tag=0 then ModalResult:=mrCancel
  else ModalResult:=mrOk;
end;

procedure TGeneratorForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=True; //CloseEditors(False);
  if CanClose then
     if Modified and (ActiveControl<>OKBtn) then
        case Msg(SettingsChangedText,
                 Caption,
                 MB_ICONQUESTION+MB_YESNOCANCEL) of
          IDYES   : begin SaveData; ActiveControl:=OKBtn; ModalResult:=mrOk;
                    end;
          IDNO    : LoadData;
          IDCANCEL: CanClose:=False;
        end; // case
  CanClose:=CanClose and not Modified;
end;

procedure TGeneratorForm.LoadData;
var i:Integer;
begin // the data is loaded from the 'Tag' field for each of the components
  for i:=0 to Pred(ComponentCount) do
      if      Components[i] is TRadioButton then with Components[i] as TRadioButton do begin Self.Modified:=Self.Modified or (Checked  <>(Tag=Ord(True))); Checked:=Tag=Ord(True); end
      else if Components[i] is TCheckBox    then with Components[i] as TCheckBox    do begin Self.Modified:=Self.Modified or (Checked  <>(Tag=Ord(True))); Checked:=Tag=Ord(True); end
      else if Components[i] is TComboBox    then with Components[i] as TComboBox    do begin Self.Modified:=Self.Modified or (ItemIndex<>Tag            ); ItemIndex:=Tag; end
      else if Components[i] is TSpinEdit    then with Components[i] as TSpinEdit    do begin Self.Modified:=Self.Modified or (Value    <>Tag            ); Value:=Tag; end
      else begin
           end;

  if (ComboBoxThreadPriority.ItemIndex>=Ord(Low (MainForm.Generator.Priority))) and
     (ComboBoxThreadPriority.ItemIndex<=Ord(High(MainForm.Generator.Priority))) and
     Assigned(MainForm.Generator) then
     MainForm.Generator.Priority:=TThreadPriority(ComboBoxThreadPriority.ItemIndex);

  LabelFixedTemplateExpiresAfter24Hours.Visible:=CheckBoxFixedTemplate.Checked;
  Modified:=False;
  ShowStatus;
end;

procedure TGeneratorForm.SaveData;
var Index,FitnessPushCount,FitnessScore:Integer; OldFitnessFunction:TGAFitnessFunction;
begin // stores the data in the 'Tag' field for each of the components
  if Modified and MainForm.Generator.HasSokoFile and (not MainForm.Generator.SokoFile.Levels.IsEmpty) then
     MainForm.Generator.SokoFile.Modified:=True;

  for Index:=0 to Pred(ComponentCount) do
      if      Components[Index] is TRadioButton then with Components[Index] as TRadioButton do Tag:=Ord(Checked)
      else if Components[Index] is TCheckBox    then with Components[Index] as TCheckBox    do Tag:=Ord(Checked)
      else if Components[Index] is TComboBox    then with Components[Index] as TComboBox    do Tag:=ItemIndex
      else if Components[Index] is TSpinEdit    then with Components[Index] as TSpinEdit    do Tag:=Value
      else;
  LoadData; // load the data again because a few things may have changed by saving the settings

  with MainForm.Generator do begin
    Enter;
    try     OldFitnessFunction:=GA.Control.FitnessFunction;

            if not IsActive then
               SaveSettingsToGenerator;

            if OldFitnessFunction<>GA.Control.FitnessFunction then begin
               for Index:=1 to GA.IndividualCount do with GA.Individuals[Index] do
                   if Fitness>0 then begin
                      GAFitnessValueToPushesAndScore(OldFitnessFunction        ,Fitness,FitnessPushCount,FitnessScore); // unpack the fitness using the old fitness function
                      Fitness:=GAFitnessValue       (GA.Control.FitnessFunction        ,FitnessPushCount,FitnessScore); // pack the fitness value using the new fitness function
                      if Assigned( Level ) and ( not Level.SnapshotsAsText.IsEmpty ) and
                         ( Level.SnapshotsAsText.First is TExtendedSnapshotAsText ) then
                         TExtendedSnapshotAsText( Level.SnapshotsAsText.First ).Metrics.SecondaryMetrics.PlayerLines := Fitness; // the fitness is stored in 'PlayerLines'
                      end;
               MainForm.Generator.SortCandidates(GA.Control.FitnessFunction);
               end;

    finally Leave;
    end;
    end;
end;

procedure TGeneratorForm.MenuItemAboutClick(Sender: TObject);
var Text:String;
begin
  Text :=  Caption+NL+
           VersionText+SPACE+YASGen_.TEXT_APPLICATION_VERSION+NL+
           YASGen_.TEXT_APPLICATION_COPYRIGHT+
           NL+NL+
           Format(GeneratorLimitsText__,[MAX_BOARD_WIDTH,
                                         MAX_BOARD_HEIGHT,
                                         YASGEN_.MAX_BOX_COUNT,
                                         YASGEN_.MAX_HISTORY_MOVES]);
  Msg(Text,Caption,MB_OK+MB_ICONINFORMATION);
end;

function  TGeneratorForm.CommitOrDropChanges:Boolean;
begin
  Result:=True; //CloseEditors(False);
  if Result then
     if Modified then
        case Msg(SettingsChangedText,
                 Caption,
                 MB_ICONQUESTION+MB_YESNOCANCEL) of
          IDYES   : SaveData;
          IDNO    : LoadData;
          IDCANCEL: Result:=False;
        end; // case
end;

procedure TGeneratorForm.SetDefaultValues;
begin
  SpinEditPopulation                     .Value        :=GA_DEFAULT_POPULATION_SIZE;
  CheckBoxBoxesGoalsLimitEnabled         .Checked      :=True;
  SpinEditBoxesGoals                     .Value        :=GA_DEFAULT_BOXES_GOALS_COUNT;
  CheckBoxOpenPositionsLimitEnabled      .Checked      :=False;
  SpinEditOpenPositions                  .Value        :=GA_DEFAULT_OPEN_POSITIONS div ONE_THOUSAND;
  CheckBoxInactivityThresholdEnabled     .Checked      :=True;
  SpinEditInactivityThreshold            .Value        :=GA_DEFAULT_INACTIVITY_THRESHOLD;
  RadioButtonLevelTypeRandom             .Checked      :=True;
  RadioButtonLevelTypeConnectedGoals     .Checked      :=not RadioButtonLevelTypeRandom.Checked;

  RadioButtonFitnessPushes               .Checked      :=GA_DEFAULT_FITNESS_FUNCTION=ffPushes;
  RadioButtonFitnessBoxLinesAndBoxChanges.Checked      :=not RadioButtonFitnessPushes.Checked;
  CheckBoxDiscardTrivialOpeningPushes    .Checked      :=DEFAULT_TRIVIAL_OPENING_PUSHES_IGNORED;
  CheckBoxDiscountTailPushesEnabled      .Checked      :=DEFAULT_TAIL_POSITION_THRESHOLD>0;
  SpinEditTailPushes                     .Value        :=Max(1,DEFAULT_TAIL_POSITION_THRESHOLD);
  RadioButtonForwardSearch               .Checked      :=False;
  RadioButtonBackwardSearch              .Checked      :=not RadioButtonForwardSearch.Checked;

  SpinEditCrossOverProbability           .Value        :=GA_DEFAULT_CROSSOVER_PROBABILITY_PCT;
  SpinEditMutationProbability            .Value        :=GA_DEFAULT_MUTATION_PROBABILITY_PCT;
  SpinEditMutationCount                  .Value        :=GA_DEFAULT_MAX_MUTATION_COUNT;
  CheckBoxFixedTemplate                  .Checked      :=False;

  SpinEditBoardWidth                     .Value        :=DEFAULT_RANDOM_BOARD_WIDTH;
  SpinEditBoardHeight                    .Value        :=DEFAULT_RANDOM_BOARD_HEIGHT;
  SpinEditInteriorWallsPct               .Value        :=DEFAULT_RANDOM_BOARD_INTERIOR_WALLS_PERCENT;

  RadioButtonRandomSeed                  .Checked      :=True;
  RadioButtonFixedRandomSeed             .Checked      :=not RadioButtonRandomSeed.Checked;
  SpinEditFixedRandomSeedNumber          .Value        :=0;

  SpinEditGeneratorTranspositionTableSize.Value        :=Min(YASGen_.DEFAULT_MEMORY_BYTE_SIZE div ONE_MEBI,SpinEditGeneratorTranspositionTableSize.MaxValue);
  ComboBoxThreadPriority                 .ItemIndex    :=Ord(tpLower);
  MainForm.Generator.Priority                          :=TThreadPriority(Max(0,ComboBoxThreadPriority.ItemIndex));
  CheckBoxAutoSavePeriodically           .Checked      :=True;

  ShowStatus;
end;

procedure TGeneratorForm.SpinEditChange(Sender: TObject);
begin
  with Sender as TSpinEdit do
    if Value<>Tag then begin
       Self.Modified:=True;
       ShowStatus;
       end;
end;

procedure TGeneratorForm.CheckBoxClick(Sender: TObject);
begin
  with Sender as TCheckBox do
    if Ord(Checked)<>Tag then begin
       Self.Modified:=True;
       ShowStatus;
       end;
  if Sender=CheckBoxFixedTemplate then
     LabelFixedTemplateExpiresAfter24Hours.Visible:=CheckBoxFixedTemplate.Checked;
end;

procedure TGeneratorForm.RadioButtonClick(Sender: TObject);
begin
  with Sender as TRadioButton do
    if Ord(Checked)<>Tag then begin
       Self.Modified:=True;
       ShowStatus;
       end;
end;

procedure TGeneratorForm.ComboBoxChange(Sender: TObject);
begin
  with Sender as TComboBox do
    if ItemIndex<>Tag then begin
       Self.Modified:=True;
       ShowStatus;
       end;
end;

function  TGeneratorForm.LoadSettingsFromFile(const FileName__:String):Boolean;
var oCursor:TCursor; IniFile:TIniFile;
begin
  oCursor:=Screen.Cursor;
  try    IniFile:=TIniFile.Create(FileName__);
         try     Screen.Cursor:=crHourGlass;

                 Result:=LoadSettingsFromIniFile(IniFile);
                 //raise Exception.Create('Test');

         finally Screen.Cursor:=oCursor;
                 IniFile.Free;
         end;
  except on E:Exception do
            Result:=Error(Format(OpenFileFailedShortText__+NL+NL+
                          TEXT_FAILURE_DESCRIPTION+NL+E.Message,[FileName__]),
                          MainForm.Generator.GeneratorName+SUB_TITLE_SEPARATOR+SettingsText+SUB_TITLE_SEPARATOR+OpenSetCaptionText);
  end;
end;

function  TGeneratorForm.SaveSettingsToFile(const FileName__:String):Boolean;
var oCursor:TCursor; IniFile:TIniFile;
begin
  oCursor:=Screen.Cursor;
  try    IniFile:=TIniFile.Create(FileName__);
         try     Screen.Cursor:=crHourGlass;

                 IniFile.WriteString(OptionsForm.SettingsFileID,MainForm.Generator.GeneratorName+SPACE+VersionText,YASGen_.TEXT_APPLICATION_VERSION);
                 Result:=SaveSettingsToIniFile(IniFile);
                 //raise Exception.Create('Test');

         finally Screen.Cursor:=oCursor;
                 IniFile.Free;
         end;
  except on E:Exception do
            Result:=Error(Format(SaveFileFailedText__+NL+NL+
                          TEXT_FAILURE_DESCRIPTION+NL+E.Message,[FileName__]),
                          MainForm.Generator.GeneratorName+SUB_TITLE_SEPARATOR+SettingsText+SUB_TITLE_SEPARATOR+SaveSettingsCaptionText);
  end;

end;

//function  TGeneratorForm.IsAGeneratorSettingsFile(const FileName__:String):Boolean;
//begin
//  Result:=FileStartsWith(FileName__,LEFT_BRACKET+MainForm.Generator.GeneratorName+SUB_TITLE_SEPARATOR+SettingsText+RIGHT_BRACKET);
//end;

function TGeneratorForm.LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
var i:Integer; DateTime1,DateTime2:TDateTime; s:String;

  procedure LoadSpinEditValue(const Section__,Key__:String; SpinEdit__:TSpinEdit);
  var Value:Integer;
  begin
    Value:=IniFile.ReadInteger(Section__,Key__,SpinEdit__.Value);
    if   (Value>=SpinEdit__.MinValue) and (Value<=SpinEdit__.MaxValue) then
         SpinEdit__.Value:=Value;
  end;

begin // LoadSettingsFromIniFile
  Result:=True;
  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Population',SpinEditPopulation);
  CheckBoxBoxesGoalsLimitEnabled         .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Boxes/Goals Limit Enabled',CheckBoxBoxesGoalsLimitEnabled.Checked);
  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Boxes/Goals',SpinEditBoxesGoals);
  CheckBoxOpenPositionsLimitEnabled      .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Open Positions Limit Enabled',CheckBoxOpenPositionsLimitEnabled.Checked);
  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Open Positions Limit (x 1000)',SpinEditOpenPositions);
  CheckBoxInactivityThresholdEnabled     .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Inactivity Threshold Enabled',CheckBoxInactivityThresholdEnabled.Checked);
  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Inactivity Threshold',SpinEditInactivityThreshold);
  RadioButtonLevelTypeRandom             .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Goals Scattered At Random',RadioButtonLevelTypeRandom.Checked);
  RadioButtonLevelTypeConnectedGoals     .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Connected Goals',RadioButtonLevelTypeConnectedGoals.Checked);

  RadioButtonFitnessPushes               .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Fitness Pushes',RadioButtonFitnessPushes.Checked);
  RadioButtonFitnessBoxLinesAndBoxChanges.Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Fitness Boxlines + Boxchanges',RadioButtonFitnessBoxLinesAndBoxChanges.Checked);
  CheckBoxDiscardTrivialOpeningPushes    .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Discard Trivial Opening Pushes',CheckBoxDiscardTrivialOpeningPushes.Checked);
  CheckBoxDiscountTailPushesEnabled      .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Discount Tail Pushes Enabled',CheckBoxDiscountTailPushesEnabled.Checked);
  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Tail Pushes',SpinEditTailPushes);
  RadioButtonForwardSearch               .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Forward Search',RadioButtonForwardSearch.Checked);
  RadioButtonBackwardSearch              .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Backward Search',RadioButtonBackwardSearch.Checked);

  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Crossover Probability',SpinEditCrossOverProbability);
  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Mutation Probability',SpinEditMutationProbability);
  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Mutations',SpinEditMutationCount);
  CheckBoxFixedTemplate                  .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Fixed Template',CheckBoxFixedTemplate.Checked);

  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Board Width',SpinEditBoardWidth);
  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Board Height',SpinEditBoardHeight);
  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Interior Walls (Percent)',SpinEditInteriorWallsPct);

  RadioButtonRandomSeed                  .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Random Seed',RadioButtonRandomSeed.Checked);
  RadioButtonFixedRandomSeed             .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Fixed Random Seed Enabled',RadioButtonFixedRandomSeed.Checked);
  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Fixed Random Seed Number',SpinEditFixedRandomSeedNumber);

  LoadSpinEditValue                                                         (INIFILE_GENERATOR_SECTION,'Transposition Table Size (MiB)',SpinEditGeneratorTranspositionTableSize);
  i:=                                              IniFile.ReadInteger      (INIFILE_GENERATOR_SECTION,'Task Priority',ComboBoxThreadPriority.ItemIndex);
  if (i>=0) and (i<ComboBoxThreadPriority.Items.Count) then ComboBoxThreadPriority.ItemIndex:=i;
  if (ComboBoxThreadPriority.ItemIndex>=Ord(Low (MainForm.Generator.Priority))) and
     (ComboBoxThreadPriority.ItemIndex<=Ord(High(MainForm.Generator.Priority))) then
     MainForm.Generator.Priority:=TThreadPriority(ComboBoxThreadPriority.ItemIndex);
  CheckBoxAutoSavePeriodically           .Checked:=IniFile.ReadBoolText     (INIFILE_GENERATOR_SECTION,'Autosave Periodically',CheckBoxAutoSavePeriodically.Checked);


  // 'fixed template' expires after 24 hours;
  // the rationale is that typically, it only applies to a particular candidate;
  try
         DateTime1:=Now;
         s:=IniFile.ReadString(INIFILE_GENERATOR_SECTION,KEY_DATE,SysUtils.DateToStr(DateTime1));
         DateTime2:=SysUtils.StrToDate(s);
         s:=IniFile.ReadString(INIFILE_GENERATOR_SECTION,KEY_TIME,SysUtils.TimeToStr(DateTime1));
         DateTime2:=DateTime2+SysUtils.StrToTime(s);
         if DateTime1-DateTime2>=1.0 then CheckBoxFixedTemplate.Checked:=False; // '>=1.0': 1 day
  except on E:EConvertError do // ignore any date/time conversion errors from 'StrToDate' and 'StrToTime'
            CheckBoxFixedTemplate.Checked:=False; // reset to default settings in case a conversion error occured; this is safer than letting an unwary user continue with settings that were designed for one particular level only
  end;
  LabelFixedTemplateExpiresAfter24Hours.Visible:=CheckBoxFixedTemplate.Checked;
  Modified := False;
end;

function TGeneratorForm.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
var DateTime:TDateTime;
begin
  Result:=True;
  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Population',SpinEditPopulation.Value);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Boxes/Goals Limit Enabled',CheckBoxBoxesGoalsLimitEnabled.Checked);
  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Boxes/Goals',SpinEditBoxesGoals.Value);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Open Positions Limit Enabled',CheckBoxOpenPositionsLimitEnabled.Checked);
  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Open Positions Limit (x 1000)',SpinEditOpenPositions.Value);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Inactivity Threshold Enabled',CheckBoxInactivityThresholdEnabled.Checked);
  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Inactivity Threshold',SpinEditInactivityThreshold.Value);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Goals Scattered At Random',RadioButtonLevelTypeRandom.Checked);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Connected Goals',RadioButtonLevelTypeConnectedGoals.Checked);

  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Fitness Pushes',RadioButtonFitnessPushes.Checked);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Fitness Boxlines + Boxchanges',RadioButtonFitnessBoxLinesAndBoxChanges.Checked);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Discard Trivial Opening Pushes',CheckBoxDiscardTrivialOpeningPushes.Checked);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Discount Tail Pushes Enabled',CheckBoxDiscountTailPushesEnabled.Checked);
  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Tail Pushes',SpinEditTailPushes.Value);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Forward Search',RadioButtonForwardSearch.Checked);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Backward Search',RadioButtonBackwardSearch.Checked);

  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Crossover Probability',SpinEditCrossOverProbability.Value);
  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Mutation Probability',SpinEditMutationProbability.Value);
  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Mutations',SpinEditMutationCount.Value);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Fixed Template',CheckBoxFixedTemplate.Checked);

  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Board Width',SpinEditBoardWidth.Value);
  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Board Height',SpinEditBoardHeight.Value);
  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Interior Walls (Percent)',SpinEditInteriorWallsPct.Value);

  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Random Seed',RadioButtonRandomSeed.Checked);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Fixed Random Seed Enabled',RadioButtonFixedRandomSeed.Checked);
  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Fixed Random Seed Number',SpinEditFixedRandomSeedNumber.Value);

  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Transposition Table Size (MiB)',SpinEditGeneratorTranspositionTableSize.Value);
  IniFile.WriteInteger    (INIFILE_GENERATOR_SECTION,'Task Priority',ComboBoxThreadPriority.ItemIndex);
  IniFile.WriteBoolText   (INIFILE_GENERATOR_SECTION,'Autosave Periodically',CheckBoxAutoSavePeriodically.Checked);

  DateTime:=Now;
  IniFile.WriteString     (INIFILE_GENERATOR_SECTION,KEY_DATE,SysUtils.DateToStr(DateTime));
  IniFile.WriteString     (INIFILE_GENERATOR_SECTION,KEY_TIME,SysUtils.TimeToStr(DateTime));
end;

procedure TGeneratorForm.LoadSettingsFromGenerator;
begin
  if Assigned(MainForm.Generator) then MainForm.Generator.Enter;
  try
    SpinEditPopulation                         .Value    :=GA.Control.PopulationSize;
    CheckBoxBoxesGoalsLimitEnabled             .Checked  :=Generator.MaxBoxCountEnabled;
    SpinEditBoxesGoals                         .Value    :=Generator.MaxBoxCount;
    CheckBoxOpenPositionsLimitEnabled          .Checked  :=Generator.MaxOpenPositionsEnabled;
    SpinEditOpenPositions                      .Value    :=Min(Generator.MaxOpenPositions div ONE_THOUSAND,SpinEditOpenPositions.MaxValue);
    CheckBoxInactivityThresholdEnabled         .Checked  :=GA.Control.InactivityThresholdEnabled;
    SpinEditInactivityThreshold                .Value    :=GA.Control.InactivityThreshold;
    RadioButtonLevelTypeRandom                 .Checked  :=not GA.Control.GenerateConnectedGoals;
    RadioButtonLevelTypeConnectedGoals         .Checked  :=not RadioButtonLevelTypeRandom.Checked;

    RadioButtonFitnessPushes                   .Checked  :=GA.Control.FitnessFunction=ffPushes;
    RadioButtonFitnessBoxLinesAndBoxChanges    .Checked  :=not RadioButtonFitnessPushes.Checked;

    CheckBoxDiscardTrivialOpeningPushes        .Checked  :=Generator.TrivialOpeningPushesIgnored;
    CheckBoxDiscountTailPushesEnabled          .Checked  :=Generator.TailPushesThresholdEnabled;
    SpinEditTailPushes                         .Value    :=Max(1,Generator.TailPushesThreshold);
    RadioButtonForwardSearch                   .Checked  :=GA.Control.ForwardSearch;
    RadioButtonBackwardSearch                  .Checked  :=not RadioButtonForwardSearch.Checked;

    SpinEditCrossOverProbability               .Value    :=GA.Control.CrossOverProbabilityPct;
    SpinEditMutationProbability                .Value    :=GA.Control.MutationProbabilityPct;
    SpinEditMutationCount                      .Value    :=GA.Control.MaxMutationCount;
    CheckBoxFixedTemplate                      .Checked  :=Generator.FixedTemplate;

//  SpinEditBoardWidth                         .Value    :=YASGen_.Game.BoardWidth;
//  SpinEditBoardHeight                        .Value    :=YASGen_.Game.BoardHeight;
//  SpinEditInteriorWalls                      .Value    :=Generator.WallCount;

    RadioButtonRandomSeed                      .Checked  :=Generator.RandomSeedEnabled;
    RadioButtonFixedRandomSeed                 .Checked  :=not RadioButtonRandomSeed.Checked;
    if RadioButtonFixedRandomSeed.Checked then
       SpinEditFixedRandomSeedNumber           .Value    :=Generator.RandomSeed;

    SpinEditGeneratorTranspositionTableSize    .Value    :=Min((YASGen_.Positions.MemoryByteSize + (ONE_MEBI div 2)) div ONE_MEBI,SpinEditGeneratorTranspositionTableSize.MaxValue);
    ComboBoxThreadPriority                     .ItemIndex:=Ord(MainForm.Generator.Priority);
    CheckBoxAutoSavePeriodically               .Checked  :=GA.Control.AutoSaveIntervalTimeMS<>0;

    SaveData;

  finally if Assigned(MainForm.Generator) then MainForm.Generator.Leave;
  end;
end;

procedure TGeneratorForm.SaveSettingsToGenerator;
begin
  if Assigned(MainForm.Generator) then MainForm.Generator.Enter;
  try
    Generator.Method                                     :=gmGA;
    GA.Control.PopulationSize                            :=SpinEditPopulation                         .Value;
    Generator .MaxBoxCountEnabled                        :=CheckBoxBoxesGoalsLimitEnabled             .Checked;
    Generator .MaxBoxCount                               :=SpinEditBoxesGoals                         .Value;
    Generator .MaxOpenPositionsEnabled                   :=CheckBoxOpenPositionsLimitEnabled          .Checked;
    Generator .MaxOpenPositions                          :=SpinEditOpenPositions                      .Value*ONE_THOUSAND;
    GA.Control.InactivityThresholdEnabled                :=CheckBoxInactivityThresholdEnabled         .Checked;
    GA.Control.InactivityThreshold                       :=SpinEditInactivityThreshold                .Value;
    GA.Control.GenerateConnectedGoals                    :=RadioButtonLevelTypeConnectedGoals         .Checked;

    if   RadioButtonFitnessPushes.Checked  then
         GA.Control.FitnessFunction                      :=ffPushes
    else GA.Control.FitnessFunction                      :=ffScore;
    Generator .TrivialOpeningPushesIgnored               :=CheckBoxDiscardTrivialOpeningPushes        .Checked;
    Generator .TailPushesThresholdEnabled                :=CheckBoxDiscountTailPushesEnabled          .Checked;
    Generator .TailPushesThreshold                       :=SpinEditTailPushes                         .Value;
    GA.Control.ForwardSearch                             :=RadioButtonForwardSearch                   .Checked;

    GA.Control.CrossOverProbabilityPct                   :=SpinEditCrossOverProbability               .Value;
    GA.Control.MutationProbabilityPct                    :=SpinEditMutationProbability                .Value;
    GA.Control.MaxMutationCount                          :=SpinEditMutationCount                      .Value;
    Generator .FixedTemplate                             :=CheckBoxFixedTemplate                      .Checked;

//  SpinEditBoardWidth                         .Value    :=YASGen_.Game.BoardWidth;
//  SpinEditBoardHeight                        .Value    :=YASGen_.Game.BoardHeight;
//  Generator.WallCount                                  :=SpinEditInteriorWalls                      .Value;

    Generator .RandomSeedEnabled                         :=RadioButtonRandomSeed                      .Checked;
    Generator .RandomSeed                                :=SpinEditFixedRandomSeedNumber              .Value;
    YASGen_.Positions.MemoryByteSize                     :=SpinEditGeneratorTranspositionTableSize    .Value*ONE_MEBI;
    if   (ComboBoxThreadPriority.ItemIndex>=Ord(Low (MainForm.Generator.Priority))) and
         (ComboBoxThreadPriority.ItemIndex<=Ord(High(MainForm.Generator.Priority))) then
         MainForm.Generator.Priority                     :=TThreadPriority(ComboBoxThreadPriority.ItemIndex);
    if   CheckBoxAutoSavePeriodically.Checked then
         GA.Control.AutoSaveIntervalTimeMS               :=GA_DEFAULT_AUTO_SAVE_INTERVAL_TIME_MS
    else GA.Control.AutoSaveIntervalTimeMS               :=0;

  finally if Assigned(MainForm.Generator) then MainForm.Generator.Leave;
  end;
end;

function  TGeneratorForm.GetSettingsDirectory:String;
begin
  if fSettingsDirectory='' then
     fSettingsDirectory:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath)+DEFAULT_PLUGINS_DIRECTORY);
  Result:=fSettingsDirectory;
end;

constructor TGeneratorThread.Create(Plugin__:TPlugin);
begin
  inherited Create(Plugin__);
end;

destructor TGeneratorThread.Destroy;
begin
  Inherited;
end;

procedure TGeneratorThread.SynchronizedPluginCallBackFunction;
var TimeMS:TTimeMS;
begin
  if Assigned(ToolsForm)
     and
     ToolsForm.Visible
     and
     (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator)
     and
     Plugin.HasThread
     and
     ((fState=Ord(ptsProcessLevel))
      or
      (fState=Ord(ptsLoadLevel))
     )
     then with ToolsForm.PluginLevelStringGrid do begin
     if   ToolsForm.BtnGenerateLevels.Tag=Ord(pbsTerminating) then begin
          Cells[1,Ord(gcbiiStatus)]:=TerminatedByUserText;
          if fPluginCallBackFunctionResult<>prTimeOut then
             fPluginCallBackFunctionResult:=prUnsolved;
          end
     else Cells[1,Ord(gcbiiStatus)]:=StatusText;
     if   Positions.Count<>0 then
          if   Positions.Count>=ONE_MILLION then
               Cells[1,Ord(gcbiiStates)]:=IntToStr((Positions.Count+HALF_MILLION) div ONE_MILLION)+MillionSuffixText
          else Cells[1,Ord(gcbiiStates)]:=IntToStr(Positions.Count)
     else Cells[1,Ord(gcbiiStates)]:='';
     if   Positions.OpenPositions.Count<>0 then
          if   Positions.OpenPositions.Count>=ONE_MILLION then
               Cells[1,Ord(gcbiiOpen)]:=IntToStr((Positions.OpenPositions.Count+HALF_MILLION) div ONE_MILLION)+MillionSuffixText
          else Cells[1,Ord(gcbiiOpen)]:=IntToStr(Positions.OpenPositions.Count)
     else Cells[1,Ord(gcbiiOpen)]:='';
     if   Plugin.StartTimeMS<>0 then
          TimeMS:=CalculateElapsedTimeMS(Plugin.StartTimeMS,GetTimeMS)-TGenerator(Plugin).PauseTimeMS
     else TimeMS:=0;
     if   TimeMS>=500 then
          Cells[1,Ord(gcbiiTime)]:=FormatTimeMS(TimeMS)
     else Cells[1,Ord(gcbiiTime)]:='';
     if   OpenForm.PanelToolTips.Visible then OpenForm.PanelToolTips.Hide;
     MainForm.Generator.ShowStatus;
     end;
end;

procedure TGeneratorThread.Execute;
//var //Index,Len:Integer; MovesAsText:String;
    //PluginStatusInfoPointer:PPluginStatusInfo; {shadows the property of the same name deliberately}
    //Level:TLevel; //Snapshot:TExtendedSnapshotAsText;
begin
  try
    while not Terminated do begin
      Enter;
      case TPluginThreadStateType(fState) of
        ptsIdle         : try     try     if (Plugin=MainForm.Generator) and Assigned(Plugin) and (Plugin.PluginThread=Self) then
                                             MainForm.Generator.StopTimer;
                                          PostMessage(MainForm.Handle,MSG_PLUGIN,0,Cardinal(Plugin));
                                  finally Leave;
                                          if (Plugin=MainForm.Generator) and Assigned(Plugin) and (Plugin.PluginThread=Self) then
                                             SynchronizePluginOnIdle;           {this should work even though the state isn't protected here; 'SynchronizedPluginOnIdle()' checks that the thread is idle before any actions are taken}
                                  end;
                          finally if fState=Ord(ptsIdle) then Suspended:=True;  {there is a race condition here; hopefully it works most of the time}
                          end;
        ptsLoadLevel    : try     fState:=Ord(ptsProcessLevel);
                          finally Leave;
                          end;
        ptsProcessLevel : begin   {the calling thread must not set this state directly;}
                                  {it must send levels to the plugin using 'ptsLoadLevel'}
                                  try
                                  finally Leave;
                                  end;

                                  // the main procedure for the generator thread
                                  GARun;

                                  Enter;
                                  try     if fState=Ord(ptsProcessLevel) then
                                             fState:=Ord(ptsIdle);
                                  finally Leave;
                                  end;
                          end;
        ptsTerminate    : try     if not Terminated then Terminate;             {terminates the thread; don't confuse this with the solver command 'Terminate'}
                          finally Leave;
                          end;
        ptsTerminated   : try
                          finally Leave;
                          end;
        else              try     fState:=Ord(ptsIdle);                         {unknown state; reset the state to 'Idle'}
                          finally Leave;
                          end;
      end; // case
      end;
  except on E:Exception do begin
         SynchronizePluginError(E.Message);
         SynchronizePluginOnIdle;                                               {reset the locked (highlighted) level, if any, on the 'Tools' form}
         end;
  end;

  try    State:=Ord(ptsTerminated);
  except on E:Exception do begin end;
  end;

end;

procedure TGeneratorThread.SynchronizeStartGeneratingLevel;
begin
  Synchronize(SynchronizedStartGeneratingLevel);
end;

procedure TGeneratorThread.SynchronizedStartGeneratingLevel;
var i{,ARow}:Integer; b:Boolean; s:String;
begin // 'SynchronizedStartGeneratingLevel'
  if Assigned(OpenForm) and
     Assigned(ToolsForm) and
     (Plugin=MainForm.Generator) and
     Assigned(Plugin) then with ToolsForm.GeneratorTaskQueue do begin
     if Assigned(Plugin.Level) then with Plugin.Level do
        if ltfLocked in Tag.Flags then begin
           {
           if ltfNew in Tag.Flags then begin
              Exclude(Tag.Flags,ltfNew);
              if (Plugin.Level<>Levels[Flags]) and
                 MakeRow(ARow) then begin
                 Plugin.Level.Flags:=ARow; // 'Level.Flags' links back from the level to its row-number in the string-grid
                 Levels[ARow]:=Plugin.Level;
                 Selected[ARow]:=False;
                 ToolsForm.MakeAllColumnsFullyVisible;
                 StringGrid.Objects[Ord(oscSnapshotName),ARow]:=TObject(GetNextRunNo);
                 end;
              end;
           }
           if   ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetGenerator) then begin
                s:=VisualFileName(Name);
                if   s<>'' then s:=SPACE+s+SPACE; // the first space is for pretty-printing in the panel; the second space is for a better balance in the "mouse-over" tool-tip
                ToolsForm.PluginLevelFileNamePanel.Caption:=s;
                //OpenForm.BtnPluginSettings.Hint:=HintPluginSettings2Text;
                //ToolsForm.BtnSolverSettings.Hint:=OpenForm.BtnPluginSettings.Hint;
                for i:=0 to Pred(ToolsForm.PluginLevelStringGrid.RowCount) do ToolsForm.PluginLevelStringGrid.Cells[1,i]:='';
                end;

           if Plugin.Level=Levels[Flags] then begin
              Selected[Flags]:=False;

              if (StringGrid.Row=Flags) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetGenerator) then begin
                ScrollInView(StringGrid.Row);
                if   (StringGrid.Row>Succ(StringGrid.FixedRows)) or Assigned(Levels[StringGrid.Row]) then
                     ToolsForm.GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,b)
                else ToolsForm.GenerateLevelsStringGridSelectCell(nil,0,StringGrid.Row,b);
                end;

              HighlightedRowNumber:=Flags;
              end
           else HighlightedRowNumber:=-1;
           end;

     if ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator then with ToolsForm do begin
        PluginEditMenuItemDeleteLevels.Enabled:=SelectedCount<>0;
        PluginToolButtonDelete.Enabled:=PluginEditMenuItemDeleteLevels.Enabled;
        PopupMenuItemDeleteLevels.Enabled:=PluginEditMenuItemDeleteLevels.Enabled;
        GeneratorPopupMenuItemDeleteCandidates.Enabled:=PopupMenuItemDeleteLevels.Enabled;
        if ToolsForm.Visible then ToolsForm.ShowStatus;
        end;
     end;
end;

procedure TGeneratorThread.SynchronizeStopGeneratingLevel;
begin
  Synchronize(SynchronizedStopGeneratingLevel);
end;

procedure TGeneratorThread.SynchronizedStopGeneratingLevel;
var IndividualNo:Integer; b:Boolean; ALevel:TLevel;

  function  AddIndividual(IndividualNo__:Integer):Integer;
  var Index,ARow,FirstNewRow,NewRowCount:Integer; CurrentLevel,HighlightedLevel:TLevel;

    function SetRowCount(RowCount__:Integer):Boolean;
    var ARow:Integer;
    begin
      with ToolsForm.GeneratorTaskQueue do with StringGrid do begin
        if RowCount<>RowCount__ then begin
           for ARow:=FixedRows to Pred(RowCount) do Levels[ARow]:=nil;
           RowCount:=RowCount__;
           ToolsForm.MakeAllColumnsFullyVisible;
           end;
        Result:=RowCount=RowCount__;
        end;
    end;

  begin // 'AddIndividual'; makes the candidate stored as member 'IndividualNo__' a member of the population, and keeps the population sorted in descending order on fitness
    with ToolsForm.GeneratorTaskQueue do with StringGrid do begin
      if Dragging then EndDrag(False);

      CurrentLevel:=Levels[Row];
      HighlightedLevel:=Levels[HighlightedRowNumber];

      //NewRowCount:=FixedRows+GA.IndividualCount;
      NewRowCount:=FixedRows+Max(GA.IndividualCount,GA.Control.PopulationSize);
      SetRowCount(NewRowCount);

      if RowCount=NewRowCount then with GA do with Control do begin
         Individuals[Low(Individuals)]:=Individuals[IndividualNo__]; {temporarily save the candidate as element 0}
         Result:=IndividualNo__;
         while ( Individuals[Low(Individuals)].Fitness  >GA.Individuals[Pred(Result)].Fitness) {'>': higher is better}
               or
               ((Individuals[Low(Individuals)].Fitness  =GA.Individuals[Pred(Result)].Fitness)
                and
                (Individuals[Low(Individuals)].PushCount>GA.Individuals[Pred(Result)].PushCount)) do begin
               Individuals[Result]:=Individuals[Pred(Result)]; Dec(Result); {insertion sort, descending order}
               end;
         Individuals[Result]:=Individuals[Low(Individuals)]; {insert the new candidate at its sorted position in the vector}
         BestIndividualNo:=1; WorstIndividualNo:=Min(IndividualCount,PopulationSize);

         with GA.Statistics do LastNewCandidateNo:=CandidatesCount;

         FirstNewRow:=FixedRows;
         for Index:=GA.IndividualCount downto 1 do with Individuals[Index] do begin
             ARow:=FixedRows+Pred(Index);
             if Levels[ARow]<>Level then begin
                Levels[ARow]:=Level;
                FirstNewRow:=ARow;
                end;
             end;
         for ARow:=FirstNewRow to Pred(RowCount) do RefreshRow(ARow);

         ARow:=FixedRows+Pred(Result);
         if   ARow=FixedRows then
              Plugin.SokoFile.Levels.MoveAfter(Levels[ARow],nil)
         else Plugin.SokoFile.Levels.MoveAfter(Levels[ARow],Levels[Pred(ARow)]);

         Plugin.SokoFile.Modified:=True;
         if   Plugin.SokoFile.Name='' then begin
              MainForm.Generator.FileName:=TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER;
              if ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator then ToolsForm.ShowTitle('',MainForm.Generator.FileName);
              end;

         if   Assigned(HighlightedLevel) then
              for Index:=FixedRows to Pred(RowCount) do
                  if Levels[Index]=HighlightedLevel then HighlightedRowNumber:=Index;

         if   ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator) then begin
              if   Assigned(CurrentLevel) then begin
                   for Index:=FixedRows to Pred(RowCount) do
                       if Levels[Index]=CurrentLevel then Row:=Index;
                   if CurrentLevel<>Levels[Row] then Row:=FixedRows; // the current level disappeared; focus the best one instead
                   end
              else Row:=FixedRows;

              ScrollInView(Row);

              if (CurrentLevel<>Levels[Row]) or (Assigned(Plugin) and (CurrentLevel=Plugin.Level)) then // 'Plugin.Level': this is the currently processed level; update the bord on the screen
                 ToolsForm.GenerateLevelsStringGridSelectCell(nil,0,Row,b);
              end;
         end
      else Result:=-1;
      end;
  end;

  procedure ShowParentChildrenCount(ParentLevel__:TLevel);
  begin
    if Assigned(ParentLevel__) then with ToolsForm.GeneratorTaskQueue do with StringGrid do
       if Levels[ParentLevel__.Flags]=ParentLevel__ then with TExtendedSnapshotAsText(ParentLevel__.SnapshotsAsText.First) do
          Cells[Ord(glcChildren),ParentLevel__.Flags]:=TASK_QUEUE_LEFT_MARGIN_CHARACTERS+IntToStr(Tag)+SPACE; // 'Tag' contains the children count
  end;

begin // 'SynchronizedStopGeneratingLevel'
  if Assigned(OpenForm) and
     Assigned(ToolsForm) and
     (Plugin=MainForm.Generator) and
     Assigned(Plugin) then with ToolsForm.GeneratorTaskQueue do begin
     if Assigned(Plugin.Level) then with Plugin.Level do
        if ltfLocked in Tag.Flags then begin
           Exclude(Tag.Flags,ltfLocked);

           if HighlightedRowNumber>=StringGrid.FixedRows then HighlightedRowNumber:=-1;

           if ltfProcessed in Tag.Flags then begin
              if   ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetGenerator) then begin
                   ShowParentChildrenCount(GA.ParentLevel1);
                   ShowParentChildrenCount(GA.ParentLevel2);
                   end;
              end;

           if ltfNew in Tag.Flags then begin
              Exclude(Tag.Flags,ltfNew);
              if   GALookupLevel(Plugin.Level,IndividualNo) then with GA.Individuals[IndividualNo] do begin
                   if             IndividualNo<=GA.Control.PopulationSize then begin
                                  if   AddIndividual(IndividualNo)>0 then begin
                                       end
                                  else begin Plugin.DeleteLevel(Plugin.Level); // something went wrong; delete the level
                                             Refresh(True);
                                       end;
                                  end
                   else if        Fitness>GA.Individuals[GA.WorstIndividualNo].Fitness then begin
                                  ALevel:=GA.Individuals[GA.WorstIndividualNo].Level; // this level is worse than the new candidate
                                  if   Assigned(ALevel) then begin
                                       if ALevel=GA.ParentLevel1 then GA.ParentLevel1:=nil;
                                       if ALevel=GA.ParentLevel2 then GA.ParentLevel2:=nil;
                                       Include(ALevel.Tag.Flags,ltfLocked); // 'ltfLocked': here it means: don't compact the string grid
                                       Plugin.DeleteLevel(ALevel); // remove the dethroned candidate from the the population, i.e., compact the candidate vector
                                       end;
                                  if   GALookupLevel(Plugin.Level,IndividualNo) and // perform a new lookup; the level changes position after 'GADeleteIndividual' has deleted the worst member of the population
                                       (AddIndividual(IndividualNo)>0) then begin
                                       end
                                  else begin Plugin.DeleteLevel(Plugin.Level); // something went wrong; delete the level
                                             Refresh(True);
                                       end;
                                  end
                        else if   (GA.IndividualCount>1) and
                                  GA.Control.InactivityThresholdEnabled and
                                  (GA.Statistics.LastNewCandidateNo+GA.Control.InactivityThreshold<GA.Statistics.CandidatesCount) then begin
                                  {the population hasn't changed for some time;}
                                  {try to increase the diversity by deleting the individual}
                                  {which had the most chances to create offspring}
                                  Inc(GA.Statistics.InactivityCount);

                                  IndividualNo:=GAIndividualWithHighestChildCountExceptTheBest;
                                  ALevel:=GA.Individuals[IndividualNo].Level; // this level is the one that will be deleted
                                  if   Assigned(ALevel) then begin
                                       if ALevel=GA.ParentLevel1 then GA.ParentLevel1:=nil;
                                       if ALevel=GA.ParentLevel2 then GA.ParentLevel2:=nil;
                                       Include(ALevel.Tag.Flags,ltfLocked); // 'ltfLocked': here it means: don't compact the string grid
                                       Plugin.DeleteLevel(ALevel); // remove the deleted candidate candidate from the population, i.e., compact the candidate vector
                                       end;
                                  if   GALookupLevel(Plugin.Level,IndividualNo) and // perform a new lookup; the level changes position after 'GADeleteIndividual' has deleted the worst member of the population
                                       (AddIndividual(IndividualNo)>0) then begin
                                       end
                                  else begin Plugin.DeleteLevel(Plugin.Level); // something went wrong; delete the level
                                             Refresh(True);
                                       end;
                                  end
                             else // discard the candidate; it doesn't qualify to be added to the population
                                  Plugin.DeleteLevel(Plugin.Level);
                   end
              else Plugin.DeleteLevel(Plugin.Level); // something is wrong; the level isn't a member of the candidate list; delete the level
              end
           else begin // don't add the candidate to the population
              if        ltfProcessed in Tag.Flags then // 'ltfprocessed': the fitness has been calculated, and the caller decided not to add the candidate to the population
                        Plugin.DeleteLevel(Plugin.Level)
              else if   Flags<StringGrid.FixedRows then begin // '<': the candidate is not a member of the string grid, i.e., it's a candidate generated by the generator (for members of the string grid the following identity holds: 'Level.Flags = ToolsForm.GeneratorTaskQueue.Levels[Level.Flags]')
                        if GALookupLevel(Plugin.Level,IndividualNo) then with GA.Individuals[IndividualNo] do
                           if CandidateNo=GA.Statistics.CandidatesCount then begin
                              Dec(GA.Statistics.CandidatesCount); // the number of candidates can be adjusted if the dropped candidate is the last one, i.e., the user hasn't added candidates manually since the calculation of the currently processed candidate started
                              end;
                        Plugin.DeleteLevel(Plugin.Level)
                        end
                   else begin // the candidate is a member of the string grid, but its fitness
                              // hasn't been calculated, meaning the user has terminated
                              // the calculation manually; leave the candidate on the screen
                              // for further processing
                        end;
              end;
           end;

     if (GA.Control.AutoSaveIntervalTimeMS<>0) and
        (CalculateElapsedTimeMS(GA.Control.LastAutoSaveTimeMS,GetTimeMS)>=GA.Control.AutoSaveIntervalTimeMS) then begin
        if MainForm.Generator.HasSokoFile then
           if   IsANewFileName(MainForm.Generator.SokoFile.Name) then begin
                if Assigned(ToolsForm) and ToolsForm.SaveDialog(ToolsForm.GeneratorMenuItemSaveAs) then begin
                   MainForm.Generator.SaveToFile(MainForm.Generator.SokoFile.Name);
                   if ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator then
                      ToolsForm.ShowTitle('',MainForm.Generator.SokoFile.Name);
                   end;
                end
           else MainForm.Generator.SaveToFile(MainForm.Generator.SokoFile.Name);

        GA.Control.LastAutoSaveTimeMS:=GetTimeMS;
        end;

     if ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator) then begin
        ToolsForm.ShowStatus;
        end;
     end;
end;

constructor TGenerator.Create(const PluginTypeText__,DefaultPluginFileNameStub__:String; Button__:TButton; ComboBox__:TComboBox);
begin
  Initialized:=False;
  Inherited;
end;

destructor TGenerator.Destroy;
begin // precondition: the plugin must only be destroyed during application shutdown
  try     Shutdown;
  finally if HasSokoFile then SokoFile.Modified:=False; {avoid that the parent class tries to save the contents of 'SokoFile'}
          Inherited;
  end;
end;

function  TGenerator.AppendFromSokoFileOrGame(SokoFile__:TSokoFile; Game__:TSokoGame):Boolean;
begin // preconditions: if 'SokoFile__' is specified then is must contain a candidate set for the generator; if 'Game__' is specified then it must contain a valid board
  Result:=False;
  if Assigned(PluginThread) and Assigned(Self.SokoFile) then begin
     Enter;
     try
          if IsActive and Assigned(SokoFile__) then begin
             Msg(GeneratorIsCurrentlyActivePleaseStopItBeforeYouAddNewCandidatesText,Caption,MB_OK+MB_ICONINFORMATION);
             end
          else begin
             GeneratorForm.SaveData;
             try    if        Assigned(SokoFile__) then begin
                              try     Result:=GAAppendFromSokoFile(SokoFile__);
                                      if   Result and (GA.IndividualCount<>0) and (FileName='') then
                                           FileName:=TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER;
                                      //if Result and (not IsANewFileName(FileName)) then GASaveToFile(ChangeFileExt(FileName,TEMP_FILE_NAME_EXT));
                              finally if   Result then
                                           GeneratorForm.LoadSettingsFromGenerator
                                      else GeneratorForm.LoadData;
                              end;
                              end
                    else if   Assigned(Game__) then begin
                              if IsFullCandidateSet then begin
                                 Result:=Error(CandidateSetIsFullText,Caption);
                                 if Assigned(ToolsForm) and ToolsForm.Visible then ToolsForm.Update; // to repaint the form after the message window has been closed
                                 end
                              else begin
                                 Result:=GAAppendFromGame(Game__,False,False);
                                 //if Result then SortCandidates(GA.Control.FitnessFunction);
                                 if Result and (GA.IndividualCount<>0) and (FileName='') then
                                    FileName:=TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER;
                                 end;
                              end
                         else raise Exception.Create(InternalErrorText);
             except on E:Exception do begin
                       Result:=Error(E.Message,Caption);
                       Clear; GeneratorForm.LoadData;
                       end;
             end;

             if Assigned(ToolsForm) and ToolsForm.Visible then begin
                if ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator then ToolsForm.ShowTitle('',Self.SokoFile.Name);
                ToolsForm.ShowStatus;
                end;
             end;

     finally Leave;
     end;
     end;
end;

function  TGenerator.CopyOrCutToClipboard(CutToClipBoard__:Boolean):Boolean;
var ARow,Index,Count:Integer; Text:String; Level:TLevel; ASokoFile:TSokoFile;
begin // copies selected candidates to clipboard
  Result:=False;
  Enter;
  try     if CreateObject(otSokoFile,TNode(ASokoFile)) then
             try
                   Count:=0;
                   with ToolsForm.GeneratorTaskQueue do with StringGrid do begin
                     for ARow:=FixedRows to Pred(RowCount) do begin
                         Level:=Levels[ARow];
                         if Selected[ARow] and (Count>=0) and GALookupLevel(Level,Index) then begin
                            if   SaveCandidateToSokoFile(Level,ASokoFile) then Inc(Count)
                            else Count:=-1;
                            end;
                         end;
                     if Count>0 then begin
                        Result:=OpenForm.CopyCollectionToClipboard0(ASokoFile,'','',True,True,False,False,True,False,Count);
                        if Result then begin
                           if   CutToClipBoard__ then begin
                                ToolsForm.PluginEditMenuItemDeleteLevelsClick(nil);
                                Text:=CandidatesCutToClipboardText__;
                                end
                           else Text:=CandidatesCopiedToClipboardText__;
                           // Msg(Format(Text,[Count]),ToolsForm.Caption,MB_OK); // it becomes tiresome to have to click 'Ok' each time the message is shown, hence, it's commented out
                           ToolsForm.StatusText:=Format(Text,[Count]);
                           end;
                        end;
                     end;
             finally ASokoFile.Modified:=False;
                     ASokoFile.Free;
             end;
  finally Leave;
  end;
end;

function  TGenerator.GetCaption:String;
var s:String;
begin
  Result:=Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+GeneratorText;
  s:=VisualFileName(SokoFile.Name);
  if s<>'' then Result:=Result+SUB_TITLE_SEPARATOR+s;
end;

procedure TGenerator.Clear;
var Level:TLevel;
begin
  if HasSokoFile then begin
     Level:=TLevel(SokoFile.Levels.First);
     while Assigned(Level) do with Level do begin
       Tag.Flags:=[]; // so 'ToolsForm.GeneratorTaskQueue.Clear' can delete the levels normally
       Level:=TLevel(Next);
       end;
     end;
  if Assigned(ToolsForm) then ToolsForm.GeneratorTaskQueue.Clear;
  GAClear;
  YASGen_.Generator.FixedTemplate:=False;
  Ga.Control.MutateWallsOnly:=False;
  YASGen_.Game.Title:='';
  InitializePositions(0);
  if HasSokoFile then begin
     SokoFile.Clear; SokoFile.SetName('');
     end;
  if Assigned(ToolsForm) and ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator) then begin
     ToolsForm.ShowTitle('',FileName);
     ToolsForm.ShowStatus;
     end;
end;

function  TGenerator.GetFileName:String;
begin
  if   HasSokoFile then Result:=SokoFile.Name
  else Result:='';
end;

function  TGenerator.GetGeneratorLongName:String;
begin
 Result:=YASGen_.GA_TEXT_APPLICATION_TITLE_LONG;
end;

function  TGenerator.GetGeneratorName:String;
begin
 Result:=YASGen_.TEXT_APPLICATION_TITLE;
end;

function  TGenerator.GetGeneratorNameAndVersion:String;
begin
 Result:=GeneratorName+SPACE+YASGen_.TEXT_APPLICATION_VERSION;
end;

function  TGenerator.GetIsFullCandidateSet:Boolean;
begin
  with GA do Result:=IndividualCount>=High(Individuals);
end;

function  TGenerator.GetSessionStartTimeMS:TTimeMS;
begin // 'Self.StartTimeMS' refers to the fitness calculation for the individual candidates; 'GA.StartTimeMS' refers to the session running time
  Result:=GA.StartTimeMS;
end;

function  TGenerator.ImportFromClipboard:Boolean;
var OldIndividualCount:Integer; Text:String; ASokoFile:TSokoFile;
begin // pastes candidates and settings from the clipboard
  Result:=False;
  Enter;
  try     if CreateObject(otSokoFile,TNode(ASokoFile)) then
             try     OldIndividualCount:=GA.IndividualCount; Text:='';

                     if Clipboard.HasFormat(CF_TEXT)
                        and
                        ASokoFile.Open('')
                        and
                        (IsALevelGeneratorCandidateSet(ASokoFile)
                         or
                         ((ASokoFile.Levels.Count>1)
                          or
                          ((ASokoFile.Levels.Count=1) and (not TLevel(ASokoFile.Levels.First).BoardAsTextLines.IsEmpty))
                         )
                        ) then begin

                        Result:=ASokoFile.SetName(TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER) and // without a name, a 'TSokoFile' loads the clipboard contents repeatedly instead of using the currently loaded levels
                                AppendFromSokoFileOrGame(ASokoFile,nil);
                        if Result and (GA.IndividualCount>OldIndividualCount) and (GA.IndividualCount>1) then // '1': don't show a 'duplicates will be removed later' message if there only is a single candidate on the task queue
                           Text:=Format(CandidatesImportedFromClipboardText__,[GA.IndividualCount-OldIndividualCount]);
                        end
                     else begin
                        Text:=NoCandidatesFoundOnClipboardText;
                        end;
                             //if   CutToClipBoard__ then begin
                             //     ToolsForm.PluginEditMenuItemDeleteLevelsClick(nil);
                             //     Text:=CandidatesCutToClipboardText__;
                             //     end
                             //else Text:=CandidatesCopiedToClipboardText__;
                             //
                     if Text<>'' then Msg(Text,Application.Title+SUB_TITLE_SEPARATOR+ToolsText+SUB_TITLE_SEPARATOR+GeneratorText+SUB_TITLE_SEPARATOR+ImportFromClipboardText,MB_OK);
             finally ASokoFile.Modified:=False;
                     ASokoFile.Free;
             end;
  finally Leave;
  end;
end;

function  TGenerator.InitializePositions(MemoryByteSize__:Integer):Boolean;
begin
  Result:=YASGen_.InitializePositions(MemoryByteSize__);
end;

function  TGenerator.LoadFromFile(const FileName__:String):Boolean;
var ASokobanFile:TSokoFile; // note: 'ASokobanFile', not the generator's own 'SokoFile'
begin // precondition: the generator isn't running; otherwise, 'Shutdown' will terminate the generator so it cannot run anymore
  Result:=Shutdown;
  if Result then
     try    ASokobanFile:=TSokoFile.Create;
            try     if FileExists(FileName__) then begin
                       if   ASokobanFile.Open(FileName__) and // the test is not 'ASokoban.IsASokobanFile()' because an empty candidate set is a valid set, but not a valid Sokoban file according to 'TSokoFile.IsASokobanFile()'
                            IsALevelGeneratorCandidateSet(ASokobanFile) then
                            Result:=LoadFromSokoFile(ASokobanFile)
                       else raise Exception.Create(Format(FileNotAGeneratorCandidateText__,[FileName__]));
                       end
                    else raise Exception.Create(Format(FileNotFoundText__,[FileName__]));
            finally ASokobanFile.Free;
            end;
     except on E:Exception do begin
               Clear;
               Result:=Error(E.Message,Caption);
               end;
     end;
end;

function  TGenerator.LoadFromGame(Game__:TSokoGame):Boolean;
begin // precondition: 'Game__' contains a valid board
  Result:=AppendFromSokoFileOrGame(nil,Game__);
end;

function  TGenerator.LoadFromSokoFile(SokoFile__:TSokoFile):Boolean;
begin // preconditions: 1. 'SokoFile__' contains a candidate set for the generator; 2. the generator isn't running; otherwise, 'Shutdown' will terminate the generator so it cannot run anymore
  Result:=Shutdown;
  if Result and Assigned(PluginThread) then begin
     Enter;
     try     Clear;
             try     GA.FileName:=SokoFile__.Name;
                     Result:=Assigned(Self.SokoFile) and SokoFile.SetName(SokoFile__.Name) and AppendFromSokoFileOrGame(SokoFile__,nil);
             finally if not Result then Clear;
                     if HasSokoFile then SokoFile.Modified:=False;
             end;
     finally Leave;
     end;
     end;
end;

function  TGenerator.LoadSettingsFromIniFile(const SectionName__:String; const IniFile:TIniFile):Boolean;
var s:String;
begin
  Result:=True;
  s:=IniFile.ReadString(SectionName__,'FileName','');
  if (s<>'') and Assigned(SokoFile) then SokoFile.SetName(s);
end;

function  TGenerator.ReleaseMemory:Boolean;
begin
  Enter;
  try     Result:=(not IsActive) and
                  // the 'is terminating' state should really have been a part
                  // of 'IsActive', but changing its meaning now would require
                  // a thorough scrutiny of its use, so the explicit test here
                  // will have to do;
                  (not ( Assigned(ToolsForm) and (Button.Tag=Ord(pbsTerminating))));
          if Result and (YASGen_.Positions.Capacity<>0) then begin
             YASGen_.FinalizePositions;
             end;
  finally Leave;
  end;
end;

function  TGenerator.RemoveLevel(Level__:TLevel):Boolean;
var IndividualNo:Integer;
begin // removes the level from the population; this does not delete the level from the level file ('SokoFile')
  Enter;
  try     Result:=GALookupLevel(Level__,IndividualNo);
          if Result then GADeleteIndividual(IndividualNo);
  finally Leave;
  end;
end;

function  TGenerator.Run:Boolean;
var i:Integer;
begin
  Result:=Assigned(PluginThread) and Assigned(ToolsForm) and Assigned(GeneratorForm);
  if Result then
     try
       Enter;
       try     Result:=ThreadState=ptsIdle;
               if Result then begin
                  GeneratorForm.SaveData; // updates generator settings

                  if   InitializePositions(GeneratorForm.SpinEditGeneratorTranspositionTableSize.Value*ONE_MEBI) then begin
                       if Generator.RandomSeedEnabled then begin
                          Generator.RandomSeed:=GetTimeMS;  {randomize the random seed}
                          InitializeRandomState(Generator.RandomSeed);
                          for i:=0 to Generator.RandomSeed mod (10*ONE_KIBI) do Generator.RandomSeed:=YASGen_.Random(High(Generator.RandomSeed)); // warm up the random number generator
                          end;
                       //Generator.RandomSeed:=0;
                       InitializeRandomState(Generator.RandomSeed);

                       if GA.IndividualCount=0 then YASGen_.Game.BoardWidth:=0; // ensure that the random board generator uses the current board dimensions settings

                       PluginThread.RecycledLevels.Clear; // clear the recycling pool
                       TerminatedByUser:=False;
                       GA.Control.LastShowStatusTimeMS:=0;
                       Self.StartTimeMS:=0; // 'Self.StartTimeMS' refers to the fitness calculation for the individual candidates; 'GA.StartTimeMS' refers to the session running time
                       Self.PauseTimeMS:=0; // pauses (e.g., when the user replays a solution on the screen) are deducted from the candidate calculation time
                       StartTimer;
                       GA.Control.LastAutoSaveTimeMS:=GA.StartTimeMS;
                       end
                  else Result:=False;
                  end;
       finally Leave;
       end;

       if Result then begin
          ThreadState:=ptsLoadLevel;
          Resume; // the 'Execute' method in the plugin-thread runs the generator
          end;

     except on E:Exception do begin
            Clear; ThreadState:=ptsIdle;
            Result:=Error(E.Message,'');
            end;
     end;
end;

function  TGenerator.SaveSettingsToIniFile(const SectionName__:String; const IniFile:TIniFile):Boolean;
begin
  Result:=True;
  IniFile.WriteString(SectionName__,'FileName',FileName);
end;

function  TGenerator.SaveCandidateToSokoFile(Level__:TLevel; SokoFile__:TSokoFile):Boolean;
var i,Index:Integer; OldAddFileFormatDescriptionToFiles:Boolean; OldSokoFile:TSokoFile;
begin
  Enter;
  try     Result:=GALookupLevel(Level__,Index);
          if Result then with GA.Individuals[Index] do
             try
               ToolsForm.PluginLevelInfo.ReplayInfo.IsLoaded:=False;
               Result:=OpenForm.Game.LoadFromBoardAsText(Level__.Tag.BoardWidth,Level__.Tag.BoardHeight,True,False,Index<=GA.Control.PopulationSize,False,Level__.BoardAsTextLines.First.Text)
                       and
                       OpenForm.Game.LoadSnapshotAsTextString(TSnapshotAsText(Level__.SnapshotsAsText.First).MovesAsTextLines.First.Text,True);

               if Result then begin
                  OpenForm.Game.Notes.Lines.WriteString(KEY_TITLE,Level__.Name);
                  if Fitness>0 then begin
                     OpenForm.Game.Notes.Lines.WriteString(GA_TEXT_FITNESS_FUNCTION_TYPES[ffPushes],IntToStr(PushCount));
                     OpenForm.Game.Notes.Lines.WriteString(GA_TEXT_FITNESS_FUNCTION_TYPES[ffScore ],IntToStr(Score));
                     OpenForm.Game.Notes.Lines.WriteString(GA_TEXT_FITNESS                         ,IntToStr(Fitness));
                     end;
                  OpenForm.Game.Notes.Lines.WriteString(GA_TEXT_CANDIDATE_NUMBER,IntToStr(CandidateNo));
                  i:=Integer(TimeOfBirthMS);
                  OpenForm.Game.Notes.Lines.WriteString(GA_TEXT_TIME_OF_BIRTH_MS,IntToStr(i));
                  i:=Integer(TimeMS);
                  OpenForm.Game.Notes.Lines.WriteString(GA_TEXT_FITNESS_CALCULATION_TIME_MS,IntToStr(i));
                  OpenForm.Game.Notes.Lines.WriteString(GA_TEXT_CHILDREN,IntToStr(ChildrenCount));

                  OldAddFileFormatDescriptionToFiles:=MainForm.AddFileFormatDescriptionToFiles;
                  OldSokoFile:=OpenForm.Game.SokoFile;
                  try     MainForm.AddFileFormatDescriptionToFiles:=False;
                          OpenForm.Game.SokoFile:=SokoFile__;
                          Result:=OpenForm.Game.SetName(Level__.Name) and
                                  TSokoGame(OpenForm.Game).SaveToFile(SokoFile__,True,True,True,True,False);
                  finally OpenForm.Game.SokoFile:=OldSokoFile;
                          MainForm.AddFileFormatDescriptionToFiles:=OldAddFileFormatDescriptionToFiles;
                  end;
                  end;
             except on E:Exception do Result:=Error(E.Message,Application.Title);
             end;
  finally Leave;
  end;
end;

function  TGenerator.SaveToFile(const FileName__:String):Boolean;
begin
  Enter;
  try     try    Result:=Assigned(SokoFile) and
                         DirectoryExists(StrWithoutTrailingPathDelimiter(ExtractFilePath(FileName__))) and // 'DirectoryExists': try to catch situations where a media disappeared
                         SokoFile.SetName(FileName__) and
                         GASaveToFile(FileName__);
                 if Result then SokoFile.Modified:=False;
          except on E:Exception do Result:=Error(E.Message,Application.Title);
          end;
  finally Leave;
  end;
end;

procedure TGenerator.SetFileName(const FileName__:String);
begin
//if Assigned(SokoFile) then
     SokoFile.SetName(FileName__);
end;

procedure TGenerator.ShowStatus;
var ARow:Integer; TimeMS:TTimeMS;
begin
   TimeMS:=GetTimeMS;
   if True then begin // CalculateElapsedTimeMS(GA.Control.LastShowStatusTimeMS,TimeMS)>=GA_DEFAULT_SHOW_STATUS_INTERVAL_TIME_MS then begin
      GA.Control.LastShowStatusTimeMS:=TimeMS;

      Enter;
      try    if Assigned(ToolsForm) then with ToolsForm do with GeneratorStatusStringGrid do begin
                if GA.IndividualCount>0 then begin
                   if   GA.Individuals[1].PushCount>0 then
                        Cells[1,Ord(gsiiBestCandidate)]:=IntToStr(GA.Individuals[1].PushCount)+SPACE+PushOrPushesSuffixText[GA.Individuals[1].PushCount<>1]
                   else Cells[1,Ord(gsiiBestCandidate)]:='';
                   //if   GA.Statistics.InactivityCount<>0 then
                        Cells[1,Ord(gsiiInactivityCount)]:=IntToStr(GA.Statistics.InactivityCount)+SPACE+TimeOrTimesSuffixText[GA.Statistics.InactivityCount<>1];
                   //else Cells[1,Ord(gsiiInactivityCount)]:='';
                   TimeMS:=GACalculateElapsedTimeMS;
                   //if   TimeMS>=500 then
                        Cells[1,Ord(gsiiTime)]:=FormatTimeMS(TimeMS);
                   //else Cells[1,Ord(gsiiTime)]:='';
                   //if   (GA.Statistics.CandidatesCount<>0) and
                   //     ((PushCount>0) or (TimeMS<>0) or (GA.Statistics.InactivityCount<>0)) then
                        Cells[1,Ord(gsiiCandidateCount)]:=IntToStr(GA.Statistics.CandidatesCount);
                   //else Cells[1,Ord(gsiiCandidateCount)]:='';
                   if GA.Statistics.MemoryFullCandidatesCount=0 then begin
                      Cells[0,Ord(gsiiMemoryFull)]:='';
                      Cells[1,Ord(gsiiMemoryFull)]:='';
                      end
                   else begin
                      if Cells[0,Ord(gsiiMemoryFull)]='' then Cells[0,Ord(gsiiMemoryFull)]:=GeneratorStatusInfoText[gsiiMemoryFull];
                      Cells[1,Ord(gsiiMemoryFull)]:=IntToStr(GA.Statistics.MemoryFullCandidatesCount)+SPACE+CandidateOrCandidatesSuffixText[GA.Statistics.MemoryFullCandidatesCount<>1]
                      end;
                   end
                else begin
                   Cells[0,Ord(gsiiMemoryFull)]:='';
                   for ARow:=0 to Pred(RowCount) do Cells[1,ARow]:='';
                   end;
                end;
      finally Leave;
      end;
      end;
end;

function  TGenerator.Shutdown:Boolean;
var i:Integer; s:String;

  procedure Shutdown;
  begin
    // the generator was originally coded as a simple console mode program;
    // one of the design decisions that later became a bottleneck is the
    // usage of global variables like 'YASGen_.Game' which is used both
    // during the search and when data is saved to a file by 'GASaveToFile';
    // therefore, the candidate set cannot be closed and saved to disk while the
    // generator is running;

    if Screen.ActiveForm=MainForm then MainForm.Refresh;

    Enter;
    try
            if IsActive then begin
               Terminate;
               Suspend;
               if Assigned(Level) and (ltfLocked in Level.Tag.Flags) then begin
                  Exclude(Level.Tag.Flags,ltfProcessed);
                  Exclude(Level.Tag.Flags,ltfNew);
                  if HasThread then
                     TGeneratorThread(PluginThread).SynchronizedStopGeneratingLevel; // note that the procedure is called from the main thread here, so it's not really a 'synchronized' invocation; the function adjusts statistics, e.g., number of tested candidates
                  end;
               end;
    finally Leave;
    end;
  end;

begin
  Result:=True;
  Enter;
  try
    if   IsActive and HasSokoFile and (not IsANewFileName(SokoFile.Name)) then begin // autosave the candidate set without bothering the user with a "Save yes/no?" question
         Shutdown; // the generator cannot run while the candidates are being saved to a disk file
         try     try    SaveToFile(SokoFile.Name);
                 except on E:Exception do Error(E.Message,Application.Title);
                 end;
         finally SokoFile.Modified:=False; Result:=True; // if saving the file failed there is nothing to do about that now; the generator thread can never be activated again because 'SaveToFile' overwrites some of the generator's work areas, e.g., 'YASGen_.Game.Board'
         end;
         end
    else if HasSokoFile and (SokoFile.Modified or IsActive) then begin
            if   IsANewFileName(SokoFile.Name) then begin
                 if SokoFile.Name='' then SokoFile.SetName(TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER);
                 s:=ChangedCandidateSetNewText+NL+NL+DoYouWantToSaveItText;
                 end
            else s:=ChangedCandidateSetText+NL+NL+DoYouWantToSaveItText;
            if   MainForm.ShutDownApplication then i:=MB_YESNO
            else i:=MB_YESNOCANCEL;

            repeat
              case Msg(s,Caption,i+MB_ICONQUESTION) of
                IDYES    : begin Shutdown;
                                 if   IsANewFileName(SokoFile.Name) then begin
                                      if Assigned(ToolsForm) then begin
                                         if ToolsForm.SaveDialog(ToolsForm.GeneratorMenuItemSaveAs) then begin
                                            Result:=SaveToFile(SokoFile.Name);
                                            end
                                         else begin
                                            // repeat the question
                                            end;
                                         end
                                      else begin
                                         // no 'Tools' window; this should not happen
                                         SokoFile.SetName('');
                                         SokoFile.Modified:=False;
                                         end;
                                      end
                                 else Result:=SaveToFile(SokoFile.Name);

                                 if   Result then begin
                                      // saving the file succeeded
                                      end
                                 else if   i=MB_YESNOCANCEL then
                                           SokoFile.Modified:=True
                                      else SokoFile.Modified:=False; // saving the file failed, but at this point (application shutdown) there is nothing to do about it
                           end;
                IDNO     : begin Shutdown;
                                 SokoFile.SetName('');
                                 SokoFile.Modified:=False;
                           end;
                IDCANCEL : begin if Screen.ActiveForm=MainForm then MainForm.Refresh;
                                 Result:=False;
                           end;
              end; // case

            until (not Result) or (not SokoFile.Modified); // loop as long as the user doesn't give a new file a name in the save dialog

            if Assigned(ToolsForm) and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator) and (not MainForm.ShutDownApplication) then begin
               ToolsForm.ShowTitle('',FileName);
               ToolsForm.ShowStatus;
               end;
            end;
  finally Leave;
  end;
end;

procedure TGenerator.SortCandidates(FitnessFunction__:TGAFitnessFunction);
var ARow,Index:Integer; OldSortMetric:TGameMetricsExtended; OldLevel:TLevel; NewIndividuals:TGAIndividuals;
begin
  OldSortMetric:=ToolsForm.SortMetric;
  Enter;
  try     try     if GA.IndividualCount=SokoFile.Levels.Count then begin
                     // the following lines are commented out because level
                     // generation keeps the levels sorted in correct order;
                     // if the correct order isn't maintained, then
                     // drag-and-drop items in the string grid needs
                     // modifications (see
                     // "Tools_.TTaskQueue.OnStringGridDragDrop")

                     //SokoFile.Levels.Items:=nil; // first sort the candidates according to their positions in the population; for efficiency, this order isn't maintained during the level generation
                     //for Index:=GA.IndividualCount downto 1 do with GA.Individuals[Index] do
                     //    if Assigned(Level) then SokoFile.Levels.Push(Level); // 'Assigned(Level)' should always be true, and all levels should be members of the 'SokoFile' collection, so unless something has gone really wrong, no levels are lost by rebuilding the list this way
                     end;

                  if      FitnessFunction__=ffPushes then ToolsForm.MenuItemSortClick(GeneratorForm.RadioButtonFitnessPushes)
                  else if FitnessFunction__=ffScore  then ToolsForm.MenuItemSortClick(GeneratorForm.RadioButtonFitnessBoxLinesAndBoxChanges);

                  with  ToolsForm.GeneratorTaskQueue do with StringGrid do begin
                        if   RowCount-FixedRows=Max(FixedRows,GA.IndividualCount) then begin
                             OldLevel:=Levels[Row];
                             FillChar(NewIndividuals,SizeOf(NewIndividuals),0);
                             for ARow:=FixedRows to Pred(RowCount) do
                                 if      Assigned(Levels[ARow]) and (Levels[ARow].Flags=ARow) and GALookupLevel(Levels[ARow],Index) then begin
                                         NewIndividuals[Succ(ARow-FixedRows)]:=GA.Individuals[Index];
                                         end
                                 else if GA.IndividualCount<>0 then raise Exception.Create(TEXT_TASK_FAILED);
                             GA.Individuals:=NewIndividuals;
                             for ARow:=FixedRows to Pred(RowCount) do
                                 if OldLevel=Levels[ARow] then Row:=ARow;
                             ScrollInView(Row);
                             end
                        else raise Exception.Create(TEXT_TASK_FAILED);
                        end;

          except  on E:Exception do begin
                     Clear;
                     Error(E.Message,Application.Title);
                     end;
          end;
  finally Leave;
          ToolsForm.SortMetric:=OldSortMetric;
  end;
end;

procedure TGenerator.StartTimer;
begin // 'Self.StartTimeMS' refers to the fitness calculation for the individual candidates; 'GA.StartTimeMS' refers to the session running time
  repeat GA.StartTimeMS:=GetTimeMS;
  until  GA.StartTimeMS<>0; // '0' is a reserved value meaning 'inactive'
end;

function  TGenerator.StopTimer:TTimeMS;
begin
//Inherited; // don't call 'Inherited'; the "replay solution" feature suspends the
             // generator while the solution replays on the screen; the total
             // generator running time is correctly calculated using 'StopTimer'
             // and 'StartTimer' before and after the suspension; however, the
             // calculation time for the currently processed level is not adjusted
             // accordingly; it would require more work than it was considered
             // worthwhile to invest in this matter.
  if GA.StartTimeMS<>0 then begin {'True': the generator is running; stop the timer}
     GA.PriorSessionsTimeMS:=AddTimeMS(GA.PriorSessionsTimeMS,CalculateElapsedTimeMS(GA.StartTimeMS,GetTimeMS));
     GA.StartTimeMS:=0;
     end;
  Result:=GA.PriorSessionsTimeMS; // return the total running time
end;

procedure TGeneratorForm.SpinEditExit(Sender: TObject);
var s:String;
begin
  if Sender is TSpinEdit then with Sender as TSpinEdit do
     try    s:=IntToStr(Value);      // trim unnecessary leading zero characters, if any, and interpret a blank text as the minimum value; 'IntToStr' raises an 'EConvertError' if the characters in the spinedit text field isn't a valid integer
            if s<>Text then Text:=s; // update the text, if necessary
     except on E:EConvertError do Text:=IntToStr(MinValue);
     end;
end;

end.

