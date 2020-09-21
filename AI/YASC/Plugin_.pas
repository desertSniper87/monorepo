unit Plugin_;

interface

uses SyncObjs,Windows,Classes,StdCtrls,IniFile_,Mandal2_,SokUtil_,SokFile_,SokGame_,Game_,Misc_;//,Clipbrd;

const
  DEFAULT_OPTIMIZER_FILE_NAME_STUB            // the default optimizer changed from 'YASS' to 'YASO' in Sokoban YASC version 1.556
                           = 'YASO';          // the path and the file-extension '.dll' is added automatically
  DEFAULT_PLUGIN_TIME_LIMIT_MS
                           : TTimeMS = 10 {minutes} * 60 {seconds} * ONE_THOUSAND {milli seconds};
  DEFAULT_PLUGINS_DIRECTORY= 'Plugins';       // full path: "{Application}\Plugins"
  DEFAULT_SOLVER_FILE_NAME_STUB
                           = 'YASS';          // the path and the file-extension '.dll' is added automatically
  INIFILE_GENERATOR_SECTION= 'Generator';     // don't localize
  INIFILE_OPTIMIZER_SECTION= 'Optimizer';     // don't localize
  INIFILE_SOLVER_SECTION   = 'Solver';        // don't localize
  MAX_PLUGIN_TIME_LIMIT_MS = High(TTimeMS)-ONE_THOUSAND-1; {'-1001': high-values are reserved for meaning 'unlimited'}
  PLUGIN_TIMER_INTERVAL_MS = 5000;

  {Sokoban flags must match the Sokoban plugin specification}
  SOKOBAN_PLUGIN_FLAG_NONE = 0;
  SOKOBAN_PLUGIN_FLAG_UNSOLVED
                           = 0;
  SOKOBAN_PLUGIN_FLAG_SOLUTION
                           = 1;
  SOKOBAN_PLUGIN_FLAG_MOVES
                           = 2;
  SOKOBAN_PLUGIN_FLAG_PUSHES
                           = 4;
  SOKOBAN_PLUGIN_FLAG_BOX_LINES
                           = 8;
  SOKOBAN_PLUGIN_FLAG_BOX_CHANGES
                           = 16;
  SOKOBAN_PLUGIN_FLAG_PUSHING_SESSIONS
                           = 32;
  SOKOBAN_PLUGIN_FLAG_PLAYER_LINES
                           = 64;
  SOKOBAN_PLUGIN_FLAG_SECONDARY_MOVES
                           = 128;
  SOKOBAN_PLUGIN_FLAG_SECONDARY_PUSHES
                           = 256;
  SOKOBAN_PLUGIN_FLAG_SECONDARY_BOX_LINES
                           = 512;
  SOKOBAN_PLUGIN_FLAG_SECONDARY_BOX_CHANGES
                           = 1024;
  SOKOBAN_PLUGIN_FLAG_SECONDARY_PUSHING_SESSIONS
                           = 2048;
  SOKOBAN_PLUGIN_FLAG_SECONDARY_PLAYER_LINES
                           = 4096;

  SOKOBAN_PLUGIN_STATUS_TEXT_SIZE       {must match the Sokoban plugin specification}
                           = 256;

  PRIMARY_METRICS_FLAGS    : array[TGameMetrics] of Integer
                           = (SOKOBAN_PLUGIN_FLAG_MOVES,SOKOBAN_PLUGIN_FLAG_PUSHES,SOKOBAN_PLUGIN_FLAG_BOX_LINES,SOKOBAN_PLUGIN_FLAG_BOX_CHANGES,SOKOBAN_PLUGIN_FLAG_PUSHING_SESSIONS,SOKOBAN_PLUGIN_FLAG_PLAYER_LINES);
  PRIMARY_METRICS_FLAGS_MASK
                           =  SOKOBAN_PLUGIN_FLAG_MOVES+SOKOBAN_PLUGIN_FLAG_PUSHES+SOKOBAN_PLUGIN_FLAG_BOX_LINES+SOKOBAN_PLUGIN_FLAG_BOX_CHANGES+SOKOBAN_PLUGIN_FLAG_PUSHING_SESSIONS+SOKOBAN_PLUGIN_FLAG_PLAYER_LINES;
  SECONDARY_METRICS_FLAGS  : array[TGameMetrics] of Integer
                           = (SOKOBAN_PLUGIN_FLAG_SECONDARY_MOVES,SOKOBAN_PLUGIN_FLAG_SECONDARY_PUSHES,SOKOBAN_PLUGIN_FLAG_SECONDARY_BOX_LINES,SOKOBAN_PLUGIN_FLAG_SECONDARY_BOX_CHANGES,SOKOBAN_PLUGIN_FLAG_SECONDARY_PUSHING_SESSIONS,SOKOBAN_PLUGIN_FLAG_SECONDARY_PLAYER_LINES);
  SECONDARY_METRICS_FLAGS_MASK
                           =  SOKOBAN_PLUGIN_FLAG_SECONDARY_MOVES+SOKOBAN_PLUGIN_FLAG_SECONDARY_PUSHES+SOKOBAN_PLUGIN_FLAG_SECONDARY_BOX_LINES+SOKOBAN_PLUGIN_FLAG_SECONDARY_BOX_CHANGES+SOKOBAN_PLUGIN_FLAG_SECONDARY_PUSHING_SESSIONS+SOKOBAN_PLUGIN_FLAG_SECONDARY_PLAYER_LINES;

  SOKOBAN_CHAR_FLAG        = 128;       {must match the Sokoban plugin specification; all board characters and move characters are 7-bit ASCII characters, hence, the 8th bit is free to use as a flag value}

type
  TBoardAsTextBuffer       = array[0..MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT] of Char; // including 1 null-terminator
  PBoardAsTextBuffer       = ^TBoardAsTextBuffer;
  TMovesAsTextBuffer       = array[0..MAX_MOVES+2] of Char; // including 1 or 2 null-terminators
  PMovesAsTextBuffer       = ^TMovesAsTextBuffer;

  TPluginStatusInfo        = packed record
    Size                   : Cardinal;  {filled in by caller; record size = SizeOf(record)}
    Flags                  : Cardinal;
    MovesGenerated         : Int64;
    PushesGenerated        : Int64;
    StatesGenerated        : Int64;     {positions}
    StatusText             : array[0..SOKOBAN_PLUGIN_STATUS_TEXT_SIZE-1] of Char; {null-terminated string}
    TimeMS                 : Cardinal;  {milli seconds}
                             end;
  PPluginStatusInfo        = ^TPluginStatusInfo;
  TPluginCallBackFunction  = function: Integer; stdcall;

  TPluginFunctionType
                           = (pfAbout,pfConstraints,pfPluginName,pfSettings,pfSolve,pfSolveEx,pfTerminate,pfOptimize,pfIsSupportedOptimization,pfSupportedOptimizations);

  TPluginThreadStateType   = (ptsIdle,ptsLoadLevel,ptsProcessLevel,ptsTerminate,ptsTerminated);

  TPlugin                  = class; {forward declaration}

  TPluginThread            = class(TThread1)
  private
    BoardBuffer            : PBoardAsTextBuffer; // pointer to the stack allocated board for the 'Execute' function; the pointer provides access to the buffer for 'SynchronizedStartOptimizingLevel' so it can prepare for optimizing the specified range of moves
    MovesBuffer            : PMovesAsTextBuffer; // pointer to the stack allocated moves for the 'Execute' function; the pointer provides access to the buffer for 'SynchronizedStartOptimizingLevel' so it can prepare for optimizing the specified range of moves
  protected
    fCriticalSection       : TCriticalSection;
    fMessageText           : String;
    fPluginCallBackFunctionResult
                           : TPluginResult;      // 'SynchronizedPluginCallBackFunction' result
    fPluginStatusInfoPointer
                           : PPluginStatusInfo;  // points to 'PluginStatusInfo' if the info is valid; otherwise it's a null-pointer
    fPluginResult          : TPluginResult;      // only valid in 'SynchronizedPluginSucceeded' and 'SynchronizedPluginFailed'
    fPluginTimeMS          : TTimeMS;            // only valid in 'SynchronizedPluginSucceeded' and 'SynchronizedPluginFailed'
    fRecycledLevels        : SokUtil_.TList;     // level pool
    OldStatusInfoFlags     : Cardinal;
    Plugin                 : TPlugin;
    PluginStatusInfo       : TPluginStatusInfo;  // 'SolveEx' and 'Optimize' parameter; access it through the property 'PluginStatusInfoPointer'; it's only valid in 'SynchronizedPluginSucceeded' and 'SynchronizedPluginFailed'

    function                 GetStatusText:String;
    function                 MakeLevel(var Level__:TLevel):Boolean;
    procedure                SetStatusText(const StatusText__:String);
    procedure                SynchronizedPluginCallBackFunction; virtual;
    procedure                SynchronizedPluginError;
//  procedure                SynchronizedPluginHint;
    procedure                SynchronizedPluginFailed;
    procedure                SynchronizedPluginOnIdle;
    procedure                SynchronizedStartOptimizingLevel;
    procedure                SynchronizedStartSolvingLevel;
    procedure                SynchronizedPluginSucceeded;
  public
    constructor              Create(Plugin__:TPlugin);
    destructor               Destroy; override;

    procedure                Execute; override;
    function                 PluginCallBackFunction:TPluginResult;
    procedure                SynchronizedSavePluginResultToLogFile;
    procedure                SynchronizePluginError(const MessageText__:String);
    procedure                SynchronizePluginOnIdle;

    property                 MessageText:String read fMessageText write fMessageText;
    property                 PluginCallBackFunctionResult:TPluginResult read fPluginCallBackFunctionResult write fPluginCallBackFunctionResult;
    property                 PluginStatusInfoPointer:PPluginStatusInfo read fPluginStatusInfoPointer;
    property                 PluginResult:TPluginResult read fPluginResult;
    property                 PluginTimeMS:TTimeMS read fPluginTimeMS;
    property                 RecycledLevels:Sokutil_.TList read fRecycledLevels;
    property                 StatusText:String read GetStatusText write SetStatusText;
  end;

  TPCharVector             = array[0..(MaxInt div SizeOf(PChar))-1] of PChar; // vector or PChar pointers
  PTPCharVector            = ^TPCharVector; // a pointer to a vector of PChar pointers; in the function 'Solve(...)', the C-declaration of the board is "Char**"
  TPluginFunctions         = record
    case Boolean of
      False                : (PluginFunction:array[TPluginFunctionType] of Pointer);
      True                 : (ShowAbout          :procedure(WindowHandle__: HWND) stdcall;
                              GetConstraints     :procedure(var MaxWidth__,MaxHeight__,MaxBoxCount__: Cardinal) stdcall;
                              GetPluginName      :procedure(PluginName__: PChar; MaxPluginNameLength__: Cardinal) stdcall;
                              Settings           :procedure(WindowHandle__: HWND) stdcall;
                              Solve              :function (Width__, Height__        : Cardinal;
                                                            BoardAsText2D__          : PTPCharVector;
                                                            SolutionAsText__         : PChar;
                                                            SolutionAsTextSize__     : Cardinal): Integer; stdcall;
                              SolveEx            :function (Width__, Height__        : Cardinal;
                                                            BoardAsText__            : PChar;
                                                            SolutionAsText__         : PChar;
                                                            SolutionAsTextSize__     : Cardinal;
                                                            PluginStatusInfoPointer__: PPluginStatusInfo;
                                                            PluginCallBackFunction__ : TPluginCallBackFunction
                                                            ): Integer; stdcall;
                              Terminate          :function: Integer; stdcall;
                              Optimize           :function (Width__, Height__        : Cardinal;
                                                            BoardAsText__            : PChar;
                                                            MovesAsText__            : PChar;
                                                            MovesAsTextSize__        : Cardinal;
                                                            PluginStatusInfoPointer__: PPluginStatusInfo;
                                                            PluginCallBackFunction__ : TPluginCallBackFunction
                                                            ): Integer; stdcall;
                              IsSupportedOptimization
                                                 :function (Optimization__           : Cardinal
                                                           ): Integer; stdcall; // zero = False, nonzero = True

                              GetSupportedOptimizations // deprecated; in fact, it was never publically documented so only a few early versions of the YASS plugin used it
                                                 :function: Integer; stdcall;
                             );
  end;

  TPlugin                  = class(TObject)
  private
    DLLHandle              : THandle;
    fButton                : TButton;
    fComboBox              : TComboBox;
    fDefaultPluginFileNameStub
                           : String;
    fHasPluginTypeInName   : Boolean;// e.g., the word 'solver' or 'optimizer' is a part of the plugin-name
    fIsDefaultPlugin       : Boolean;
    fLevel                 : TLevel; // currently processed level, if any
    fPendingResultsCount   : Integer;
    fPluginFileName        : String;
    fPluginFunctions       : TPluginFunctions;
    fPluginLimitsAsText    : String;
    fPluginThread          : TPluginThread;
    fPluginTypeText        : String;
    fPriority              : TThreadPriority;
    fSelectedLevelsCount   : Integer;
    fSokoFile              : TSokoFile; // task queue and pending results waiting for postprocessing
    fStartTimeMS           : TTimeMS;
    fTerminatedByUser      : Boolean;
  protected
    function                 GetHasIsSupportedOptimizationFunction:Boolean;
    function                 GetHasOptimizer:Boolean;
    function                 GetHasSokoFile:Boolean;
    function                 GetHasSolver:Boolean;
    function                 GetHasThread:Boolean;
    function                 GetIsActive:Boolean;
    function                 GetIsLoaded:Boolean;
    function                 GetIsRunningInAnotherThread:Boolean;
    function                 GetLevel:TLevel;
    function                 GetLevelFileName:String;
    function                 GetPluginName:String;
    function                 GetPluginResult:TPluginResult;
    function                 GetPluginStatusInfoPointer:PPluginStatusInfo;
    function                 GetPluginThread:TPluginThread;
    function                 GetPluginTimeMS:TTimeMS;
    function                 GetSokoFile:TSokoFile;
    function                 GetMovesAsText:String;
    function                 GetSupportedOptimizations:Integer;
    function                 GetThreadState:TPluginThreadStateType;
    procedure                SetLevel(Level__:TLevel);
    procedure                SetPriority(ThreadPriority__:TThreadPriority);
    procedure                SetPluginFileName(const PluginFileName__:String);
    procedure                SetThreadState(ThreadState__:TPluginThreadStateType);
  public
    AddDateToSolutionNotes,
    AddPluginNameToSolutionTitles,
    DiscardDethronedSolutions, // optimizer only
    StatisticsEnabled,
    TimeLimitEnabled,
    WallifyBoxesOutsideRangeOfPushesToBeOptimized // optimizer only
                           : Boolean;
    SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly // optimizer only
                           : Boolean;
    SubIntervalOverlapPct  : Integer; // optimizer only
    TimeLimitMS            : TTimeMS;

    constructor              Create(const PluginTypeText__,DefaultPluginFileNameStub__:String; Button__:TButton; ComboBox__:TComboBox);
    destructor               Destroy; override;

    function                 Add(const FileName__:String):Boolean; // add new plugin to the list
    procedure                ClearQueue(RemoveSelectedItems__:Boolean);
    function                 Close:Boolean;
    function                 DefaultPluginFileName:String;
    procedure                DeleteLevel(Level__:TLevel);
    procedure                Enter;
    function                 ImportGame(Level__:TLevel):Boolean;
    function                 ImportGames(DeleteImportedLevels__:Boolean):Integer;
    function                 IndexOf(const FileName__:String):Integer; // lookup a plugin in the list
    function                 IsSupportedOptimization(Optimization__:Cardinal):Boolean;
    procedure                Leave;
    function                 LoadLevel(BoardWidth__,BoardHeight__,LevelFlags__,SnapshotFlags__:Integer; const LevelName__,BoardAsText__:String; Snapshot__:TSnapshot; CopySnapshotNotes__,Execute__:Boolean; var NewLevel__:TLevel):Boolean;
    function                 LoadSettingsFromIniFile(const SectionName__:String; const IniFile:TIniFile):Boolean; virtual;
    function                 Lookup(BoardWidth__,BoardHeight__:Integer; const BoardAsText__,MovesAsText__:String; var Level__:TLevel):Boolean;
    function                 MoveOptimizedSolutionToSolverQueue(DiscardDethronedSolutions__:Boolean; var NewLevel__:TLevel):Integer; // the optimizer puts new solutions on the solver task queue; later the optimizer task queue imports them again using 'Tools_.TTaskQueue.ImportFromOtherPlugins'
    function                 Open(const PluginFileName__:String):Boolean;
    function                 Optimize(BoardWidth__,BoardHeight__:Integer;
                                      BoardAsText__:PChar;
                                      var MovesAsTextBuffer__:TMovesAsTextBuffer;
                                      var TimeMS__:TTimeMS;
                                      var PluginStatusInfoPointer__:PPluginStatusInfo):TPluginResult;

    function                 Remove(Index__:Integer):Boolean;
    function                 RenameLevels(const OldFileName__,NewFileName__:String):Integer;
    function                 RenamePlugins(const OldFileName__,NewFileName__:String):Integer;
    function                 ReplayGame( {input:} Level__:TLevel; {output:} BoardAsText__ : PBoardAsTextBuffer; {output:} MovesAsText__ : PMovesAsTextBuffer; {input:} WallifyBoxesOutsideRangeOfPushesToBeOptimized__ : Boolean ):Boolean;
    procedure                Resume;
    function                 SaveSettingsToIniFile(const SectionName__:String; const IniFile:TIniFile):Boolean; virtual;
    procedure                SetSelected(Level__:TLevel; Selected__:Boolean);
    function                 Settings(WindowHandle__:HWND):Boolean;
    function                 ShowAbout(WindowHandle__:HWND):Boolean;
    function                 Solve(BoardWidth__,BoardHeight__:Integer;
                                   BoardAsText__:PChar;
                                   var SolutionAsTextBuffer__:TMovesAsTextBuffer;
                                   var TimeMS__:TTimeMS;
                                   var PluginStatusInfoPointer__:PPluginStatusInfo):TPluginResult;
    procedure                StartTimer; virtual;
    function                 StopTimer:TTimeMS; virtual;
    procedure                Suspend;
    function                 Terminate:Boolean;

    property                 Button:TButton read fButton;
    property                 ComboBox:TComboBox read fComboBox;
    property                 DefaultPluginFileNameStub:String read fDefaultPluginFileNameStub write fDefaultPluginFileNameStub;
    property                 HasIsSupportedOptimizationFunction:Boolean read GetHasIsSupportedOptimizationFunction;
    property                 HasPluginTypeInName:Boolean read fHasPluginTypeInName;
    property                 HasOptimizer:Boolean read GetHasOptimizer;
    property                 HasSokoFile:Boolean read GetHasSokoFile;
    property                 HasSolver:Boolean read GetHasSolver;
    property                 HasThread:Boolean read GetHasThread;
    property                 IsActive:Boolean read GetIsActive;
    property                 IsDefaultPlugin:Boolean read fIsDefaultPlugin;
    property                 IsRunningInAnotherThread:Boolean read GetIsRunningInAnotherThread;
    property                 Level:TLevel read fLevel write SetLevel; {currently processed level}
    property                 LevelFileName:String read GetLevelFileName;
    property                 IsLoaded:Boolean read GetIsLoaded;
    property                 MovesAsText:String read GetMovesAsText;
    property                 PendingResultsCount:Integer read fPendingResultsCount write fPendingResultsCount;
    property                 PluginFileName:String read fPluginFileName write SetPluginFileName; // don't confuse 'PluginName' and 'PluginFileName'
    property                 PluginFunctions:TPluginFunctions read fPluginFunctions;
    property                 PluginName:String read GetPluginName; // don't confuse 'PluginName' and 'PluginFileName'
    property                 PluginResult:TPluginResult read GetPluginResult;
    property                 PluginStatusInfoPointer:PPluginStatusInfo read GetPluginStatusInfoPointer;
    property                 PluginThread:TPluginThread read GetPluginThread;
    property                 PluginTimeMS:TTimeMS read GetPluginTimeMS;
    property                 PluginTypeText:String read fPluginTypeText;
    property                 Priority:TThreadPriority read fPriority write SetPriority;
    property                 SelectedLevelsCount:Integer read fSelectedLevelsCount;
    property                 SokoFile:TSokoFile read GetSokoFile; {level queue; created on demand only}
    property                 StartTimeMS:TTimeMS read fStartTimeMS write fStartTimeMS;
    property                 SupportedOptimizations:Integer read GetSupportedOptimizations;
    property                 PluginLimitsAsText:String read fPluginLimitsAsText;
    property                 TerminatedByUser:Boolean read fTerminatedByUser write fTerminatedByUser;
    property                 ThreadState:TPluginThreadStateType read GetThreadState write SetThreadState;
  end;

  TOptimizerPlugin = class(TPlugin); // the only supported instance of this class is 'MainForm.Optimizer'; see the function 'OptimizerPluginCallBackFunction()' below
  TSolverPlugin    = class(TPlugin); // the only supported instance of this class is 'MainForm.Solver'; see the function 'SolverPluginCallBackFunction()' below

function  HasOptimizationFlags         (OptimizationFlags__:Integer; var PrimaryMetric__,SecondaryMetric__:TGameMetrics):Boolean;
function  HasPrimaryOptimizationFlags  (OptimizationFlags__:Integer; var Metric__:TGameMetrics):Boolean;
function  HasSecondaryOptimizationFlags(OptimizationFlags__:Integer; var Metric__:TGameMetrics):Boolean;
function  OptimizationFlagsAsText(Flags__:Integer):String; {throws EOutOfMemory}
function  TextToOptimizationFlags( const Text__ : String ) : Integer;

implementation

{$WARNINGS OFF}
uses SysUtils,FileCtrl,Forms,Controls,Grids,
     Text_,Pack_,Dead_,Tools_,Snapshots_,Main_,Open1_,Generator_;
{$WARNINGS ON}
const
  DEFAULT_SUB_INTERVAL_OVERLAP_PCT
                           = 10;
  PLUGIN_FUNCTION_NAMES    : array[TPluginFunctionType] of String // don't localize; the names must match the names used in the dll-files
                           = ('ShowAbout','GetConstraints','GetPluginName','Configure','Solve','SolveEx','Terminate','Optimize','IsSupportedOptimization','GetSupportedOptimizations'); // don't localize
  GET_SOLVER_NAME_FUNCTION_NAME               // backwards compatibility; early versions of the plugin interface only supported solvers and used the function name 'GetSolvernName' instead of 'GetPluginName'
                           = 'GetSolverName'; // don't localize;

var
  PluginShutdown           : Boolean = False; // there may be a pending synchronized callback from a thread after the thread has been destroyed; in that case the callback must be informed not to do anything, hence, this switch

function HasOptimizationFlags(OptimizationFlags__:Integer; var PrimaryMetric__,SecondaryMetric__:TGameMetrics):Boolean;
begin // returns 'True' if 'Flags__' contain a primary metric; its value is returned in 'PrimaryMetric__'; if no secondary metric is present, then 'SecondaryMetric__' is set to the same value as 'PrimaryMetric__'
  Result:=HasPrimaryOptimizationFlags(OptimizationFlags__,PrimaryMetric__);
  if Result then
     if   HasSecondaryOptimizationFlags(OptimizationFlags__,SecondaryMetric__) then
     else SecondaryMetric__:=PrimaryMetric__;
end;

function  HasPrimaryOptimizationFlags(OptimizationFlags__:Integer; var Metric__:TGameMetrics):Boolean;
var Metric:TGameMetrics;
begin
  Result:=False;
  for Metric:=Low(Metric) to High(Metric) do
      if (OptimizationFlags__ and PRIMARY_METRICS_FLAGS[Metric])<>0 then begin
         Metric__:=Metric; Result:=True; exit; {quick-and-dirty exit}
         end;
end;

function  HasSecondaryOptimizationFlags(OptimizationFlags__:Integer; var Metric__:TGameMetrics):Boolean;
var Metric:TGameMetrics;
begin
  Result:=False;
  for Metric:=Low(Metric) to High(Metric) do
      if (OptimizationFlags__ and SECONDARY_METRICS_FLAGS[Metric])<>0 then begin
         Metric__:=Metric; Result:=True; exit; {quick-and-dirty exit}
         end;
end;

function  OptimizationFlagsAsText(Flags__:Integer):String; {throws EOutOfMemory}
var PrimaryMetric,SecondaryMetric:TGameMetrics;
begin
  for PrimaryMetric:=Low(PrimaryMetric) to High(PrimaryMetric) do
      if (Flags__ and PRIMARY_METRICS_FLAGS[PrimaryMetric])<>0 then begin
         for SecondaryMetric:=Low(SecondaryMetric) to High(SecondaryMetric) do
             if (PrimaryMetric<>SecondaryMetric) and
                ((Flags__ and SECONDARY_METRICS_FLAGS[SecondaryMetric])<>0) then begin
                Result:=GameMetricsText[PrimaryMetric]+', '+AnsiLowerCase(GameMetricsText[SecondaryMetric]);
                exit; {quick-and-dirty exit}
                end;

         // no secondary metrics listed; return primary metric only
         Result:=GameMetricsText[PrimaryMetric];
         exit; {quick-and-dirty exit}
         end;
  Result:=SettingsText; // unspecified flags: use current optimizer settings
end;

function  TextToOptimizationFlags( const Text__ : String ) : Integer;
var PrimaryMetric,SecondaryMetric:TGameMetrics;
begin
  Result := SOKOBAN_PLUGIN_FLAG_NONE;
  for PrimaryMetric:=Low(PrimaryMetric) to High(PrimaryMetric) do
      if StrBeginsWith( Text__, GameMetricsText[PrimaryMetric] ) then begin
         Result := PRIMARY_METRICS_FLAGS[PrimaryMetric];
         if System.Pos( COMMA, Text__ ) <> 0 then
            for SecondaryMetric:=Low(SecondaryMetric) to High(SecondaryMetric) do
                if (PrimaryMetric<>SecondaryMetric) and
                   StrEndsWith( Text__, GameMetricsText[SecondaryMetric] ) then begin
                   Inc( Result, SECONDARY_METRICS_FLAGS[SecondaryMetric] );
                   exit; {quick-and-dirty exit}
                end;
         exit; {quick-and-dirty exit}
         end;
end;

{Plugin callback functions; the interface requires that they aren't methods but plain functions, hence, they are defined here as trampolines to the plugins}

function OptimizerPluginCallBackFunction: Integer; stdcall;
begin {trampoline from the dll to the optimizer plugin-thread}
  Result:=Ord(MainForm.Optimizer.PluginThread.PluginCallBackFunction);
end;

function SolverPluginCallBackFunction: Integer; stdcall;
begin {trampoline from the dll to the solver plugin-thread}
  Result:=Ord(MainForm.Solver.PluginThread.PluginCallBackFunction);
end;

function GeneratorPluginCallBackFunction: Integer; stdcall;
begin {trampoline from the dll to the generator plugin-thread}
  Result:=Ord(MainForm.Generator.PluginThread.PluginCallBackFunction);
end;

{Plugin thread}

constructor TPluginThread.Create(Plugin__:TPlugin);
begin
  Plugin:=Plugin__; fCriticalSection:=nil; fRecycledLevels:=nil; StatusText:='';
  try       fCriticalSection:=TCriticalSection.Create;
            fRecycledLevels:=SokUtil_.TList.Create;
  except    fCriticalSection.Free; fCriticalSection:=nil;
            fRecycledLevels .Free; fRecycledLevels :=nil;
            raise;
  end;
//inherited Create(fCriticalSection,tpLower);
  inherited Create(fCriticalSection,tpNormal);
end;

destructor TPluginThread.Destroy;
begin
  fCriticalSection.Free; fCriticalSection:=nil;
  fRecycledLevels .Free; fRecycledLevels :=nil;
  Plugin:=nil;
  Inherited;
end;

procedure TPluginThread.Execute;
var Index,Len,SliceLength,FromMove,ToMove,NextPushPlayerPositionAndDirection,RepeatInterval:Integer;
    Text:String;
    PluginStatusInfoPointer:PPluginStatusInfo; {shadows the property of the same name deliberately}
    BoardAsTextBuffer:TBoardAsTextBuffer;
    MovesAsTextBuffer:TMovesAsTextBuffer;
    Level:TLevel; Snapshot:TExtendedSnapshotAsText;
begin
  Self.BoardBuffer:=Addr(BoardAsTextBuffer); {provide access to the stack allocated buffers from the outside}
  Self.MovesBuffer:=Addr(MovesAsTextBuffer); {provide access to the stack allocated buffers from the outside}
  try
    while not Terminated do begin
      Enter;
      case TPluginThreadStateType(fState) of
        ptsIdle         : begin try     PostMessage(MainForm.Handle,MSG_PLUGIN,0,Cardinal(Plugin));
                                finally Leave;
                                end;
                                if fState=Ord(ptsIdle) then Suspended:=True; {there is a race condition here; hopefully it works most of the time}
                          end;
        ptsLoadLevel    : try
                            Level:=TLevel(Plugin.SokoFile.Levels.First);
                            repeat // search through the queue for the next level to process
                                   while Assigned(Level)
                                         and
                                         ((Level.Tag.Flags*[ltfSelected,ltfLocked,ltfProcessed])<>[ltfSelected])
                                         do
                                         Level:=TLevel(Level.Next);
                                   if Assigned(Level) then
                                      if   Level.BoardAsTextLines.IsEmpty
                                           or
                                           (Length(Level.BoardAsTextLines.First.Text)<>Level.Tag.BoardWidth*Level.Tag.BoardHeight)
                                           or
                                           ((Plugin is TOptimizerPlugin)
                                            and
                                            (Level.SnapshotsAsText.IsEmpty
                                             or
                                             (not (Level.SnapshotsAsText.First is TExtendedSnapshotAsText))
                                             or
                                             TSnapshotAsText (Level.SnapshotsAsText.First).MovesAsTextLines.IsEmpty
                                             or
                                             (TSnapshotAsText(Level.SnapshotsAsText.First).MovesAsTextLines.First.Text='')
                                            )
                                           )
                                           then begin
                                           // malformed level; delete it
                                           Plugin.DeleteLevel(Level);
                                           Level:=TLevel(Plugin.SokoFile.Levels.First); {restart the search for a level from the beginning}
                                           end
                                      else // try to process this level
                                           fState:=Ord(ptsProcessLevel);
                            until  (fState=Ord(ptsProcessLevel)) or (not Assigned(Level));

                            if   Assigned(Level) and (fState=Ord(ptsProcessLevel)) and (not PluginShutDown) then begin
                                 Include(Level.Tag.Flags,ltfLocked);
                                 Exclude(Level.Tag.Flags,ltfProcessed);
                                 Exclude(Level.Tag.Flags,ltfUnsolvable);
                                 BoardAsTextBuffer[High(BoardAsTextBuffer)  ]:=NULL_CHAR; {ensure that 'BoardAsTextBuffer' is a null-terminated string}
                                 MovesAsTextBuffer[High(MovesAsTextBuffer)-1]:=NULL_CHAR; {ensure that 'MovesAsTextBuffer' is a double-null-terminated string}
                                 MovesAsTextBuffer[High(MovesAsTextBuffer)  ]:=NULL_CHAR; {ensure that 'MovesAsTextBuffer' is a double-null-terminated string}
                                 fPluginTimeMS:=0; fPluginResult:=prUnsolved;   {clear result}
                                 Plugin.Level:=Level;                           {update currently processed level}
                                 if        Plugin is TOptimizerPlugin then begin

                                           Text:=Level.BoardAsTextLines.First.Text; {copy board to buffer}
                                           Len:=Min(Length(Text),(SizeOf(BoardAsTextBuffer) div SizeOf(Char))-SizeOf(Char));
                                           for Index:=0 to Pred(Len) do BoardAsTextBuffer[Index]:=Text[Succ(Index)];
                                           BoardAsTextBuffer[Len]:=NULL_CHAR;   {add a null-terminator after the board}

                                           Text:=TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.First.Text; {copy moves to buffer}
                                           Len:=Min(Length(Text),(SizeOf(MovesAsTextBuffer) div SizeOf(Char))-(2*SizeOf(Char)));
                                           for Index:=0 to Pred(Len) do MovesAsTextBuffer[Index]:=Text[Succ(Index)];
                                           MovesAsTextBuffer[     Len ]:=NULL_CHAR;    {add a null-terminator after the moves}
                                           MovesAsTextBuffer[Succ(Len)]:=NULL_CHAR;    {add an extra null-terminator after the last game (YASC only sends a single game to the optimizer)}
                                           Text:='';                                   {clear reference to the string, so the plugin thread doesn't prevent garbage collection of the string}

                                           if Level.Flags<ToolsForm.OptimizerTaskQueue.StringGrid.FixedRows then {'True': the level isn't listed on the user's task queue}
                                              Plugin.SetSelected(Level,False);
                                           {the optimizer resets 'Selected' later in 'SynchronizedStartOptimizingLevel'}
                                           end
                                 else if   Plugin is TSolverPlugin then begin
                                           if Level.Flags<ToolsForm.SolverTaskQueue.StringGrid.FixedRows then {'True': the level isn't listed on the user's task queue}
                                              Plugin.SetSelected(Level,False);
                                           {the solver resets 'Selected' later in 'SynchronizedStartSolvingLevel'}
                                           end
                                      else Plugin.SetSelected(Level,False);
                                 end
                            else begin Plugin.Level:=nil;                       {update currently processed level}
                                       fState:=Ord(ptsIdle);                    {nothing to do right now}
                                 end;
                          finally
                            Leave;
                            if (fState=Ord(ptsIdle)) and
                               Assigned(Plugin) and
                               (Plugin.PluginThread=Self) and
                               ((Plugin=MainForm.Solver) or (Plugin=MainForm.Optimizer))
                               then
                               SynchronizePluginOnIdle;                         {this should work even though the state isn't protected here; 'SynchronizedPluginOnIdle()' checks that the thread is idle before any actions are taken}
                          end;
        ptsProcessLevel : begin {the calling thread must not set this state directly;}
                                {it must send levels to the plugin using 'ptsLoadLevel'}
                            fPluginResult:=prFailed;
                            fPluginCallBackFunctionResult:=prOK;                {default call-back result; the host may change the value during the search, e.g., if the host-side time limit is enabled and exceeded}

                            try     Level:=Plugin.Level;
                                    if Assigned(Level) and (not (ltfLocked in Level.Tag.Flags)) then Level:=nil;
                            finally Leave;
                            end;

                            if Assigned(Level) and
                               (ltfLocked in Level.Tag.Flags) and
                               (not Level.BoardAsTextLines.IsEmpty) then
                               try    FillChar(PluginStatusInfo,SizeOf(PluginStatusInfo),0);
                                      PluginStatusInfo.Size   :=SizeOf(PluginStatusInfo);
                                      PluginStatusInfoPointer :=Addr(PluginStatusInfo);

                                      // the central statement: execute the plugin task
                                      if        Plugin is TSolverPlugin then begin
                                                Synchronize(SynchronizedStartSolvingLevel);
                                                fPluginResult :=Plugin.Solve   (Level.Tag.BoardWidth,Level.Tag.BoardHeight,PChar(Level.BoardAsTextLines.First.Text),MovesAsTextBuffer,fPluginTimeMS,PluginStatusInfoPointer);
                                                end
                                      else if   Plugin is TOptimizerPlugin then begin
                                                Synchronize(SynchronizedStartOptimizingLevel);
                                                PluginStatusInfoPointer^.Flags:=TSnapshotAsText(Level.SnapshotsAsText.Last).Tag;
                                                OldStatusInfoFlags:=PluginStatusInfoPointer^.Flags;
                                                TExtendedSnapshotAsText(Level.SnapshotsAsText.Last).OptimizationFlags:=OldStatusInfoFlags; // 'OptimizationFlags' contains the actually used flags by the optimizer, as opposed to 'Tag' which contains the user-settings from the YASC host program
                                                fPluginResult :=Plugin.Optimize(Level.Tag.BoardWidth,Level.Tag.BoardHeight,BoardAsTextBuffer,MovesAsTextBuffer,fPluginTimeMS,PluginStatusInfoPointer);
                                                end
                                           else fPluginResult :=prUnsolved;

                                      fPluginStatusInfoPointer:=PluginStatusInfoPointer; {access the plugin result status, if any, via the property 'PluginStatusInfoPointer'; it is, however, only valid during the synchronized post-processing of the level}

                                      case PluginResult of
                                        prOK  : begin
                                                      //PostMessage(ToolsForm.Handle,MSG_TEST,0,0);
                                                      if (not Level.SnapshotsAsText.IsEmpty) and
                                                         (Level.SnapshotsAsText.Last is TExtendedSnapshotAsText) and
                                                         (not TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.IsEmpty) and
                                                         (TSnapshotAsText(Level.SnapshotsAsText.Last).MovesAsTextLines.First.Text<>'')
                                                         then with TExtendedSnapshotAsText(Level.SnapshotsAsText.Last) do begin
                                                         // if the original snapshot/solution specified a move interval,
                                                         // then the moves produced by the plugin must be inserted in
                                                         // the existing snapshot/solution, replacing the specified interval;
                                                         FromMove:=SelectedRange[3]; ToMove:=SelectedRange[4]; // substitute the moves interval ]start, end], start exclusive, end inclusive; 'move' is a misnomer here; it's really character indices;
                                                         NextPushPlayerPositionAndDirection:=SelectedRange[5]; // the player position before the next push after the slice, and the push direction
                                                         RepeatInterval                    :=SelectedRange[6]; // partition solution into subintervals of this size
                                                         end
                                                      else begin
                                                         FromMove:=0; ToMove:=0; NextPushPlayerPositionAndDirection:=0; RepeatInterval:=0;
                                                         end;

                                                      Index:=Low(MovesAsTextBuffer);
                                                      repeat {retrieve one or more snapshots/solutions from the plugin}
                                                        Len:=Index;
                                                        while (Len<High(MovesAsTextBuffer)) and
                                                              (MovesAsTextBuffer[Len]<>NULL_CHAR) and
                                                              (MovesAsTextBuffer[Len]<>SPACE) do
                                                              Inc(Len);
                                                        SliceLength:=Len-Index;
                                                        SetString(Text,PChar(Addr(MovesAsTextBuffer[Index])),SliceLength);

                                                        if FromMove<ToMove then begin
                                                           Snapshot:=TExtendedSnapshotAsText(Level.SnapshotsAsText.Last);
                                                           Text:=Copy(Snapshot.MovesAsTextLines.First.Text,1,FromMove)+
                                                                 Text+
                                                                 Copy(Snapshot.MovesAsTextLines.First.Text,Succ(ToMove),MaxInt);
                                                           end;

                                                        Enter;
                                                        try     if   Assigned(Plugin.Level) and
                                                                     (ltfLocked in Plugin.Level.Tag.Flags) and
                                                                     CreateObject(otExtendedSnapshotAsText,TNode(Snapshot)) then begin
                                                                     Level.SnapshotsAsText.Push(Snapshot);
                                                                     if not Assigned(Snapshot.MovesAsTextLines.AddTextLine(Text,False)) then
                                                                        fPluginResult:=prFailed; {failed to add the textline with the moves}
                                                                     if FromMove<ToMove then begin {inherit interval information from the original snapshot/solution}
                                                                        Snapshot.SelectedRange[3]:=FromMove;
                                                                        Snapshot.SelectedRange[4]:=FromMove+SliceLength;
                                                                        Snapshot.SelectedRange[5]:=NextPushPlayerPositionAndDirection;
                                                                        Snapshot.SelectedRange[6]:=RepeatInterval;
                                                                        end;
                                                                     end
                                                                else fPluginResult:=prFailed; {the level disappeared, or creation of the snapshot failed}
                                                        finally Leave;
                                                        end;
                                                        Index:=Len;
                                                        if Index<High(MovesAsTextBuffer) then Inc(Index); {skip the current character, i.e., the null-terminator or the SPACE terminator}
                                                      until  (not (Plugin is TOptimizerPlugin)) {currently, only optimizer plugins may return more than one solution}
                                                             or
                                                             (Index>High(MovesAsTextBuffer))
                                                             or
                                                             (MovesAsTextBuffer[Index]=NULL_CHAR); {until the extra null-terminator (after the last null-terminated solution) has been found}

                                                      if   PluginResult=prOK then
                                                           Synchronize(SynchronizedPluginSucceeded)
                                                      else Synchronize(SynchronizedPluginFailed);
                                                end;
                                        else    begin Synchronize(SynchronizedPluginFailed);
                                                end;
                                      end; {case}
                               except on E:Exception do SynchronizePluginError(E.Message);
                               end;

                            Enter;
                            try     Level:=Plugin.Level;
                                    if Assigned(Level) and (ltfLocked in Level.Tag.Flags) then begin
                                       if      Plugin is TSolverPlugin then begin
                                               if ((fPluginResult<>prOK)         {the plugin didn't succeed; remove the level from the queue unless this is an optimizer plugin}
                                                   and
                                                   ((Level.Flags< ToolsForm.SolveLevelsStringGrid.FixedRows)
                                                    or
                                                    (Level.Flags>=ToolsForm.SolveLevelsStringGrid.RowCount)
                                                    or
                                                    (Level<>ToolsForm.SolverTaskQueue.Levels[Level.Flags]) // 'True': // the level is on the solver queue but not appearing in the string grid
                                                   )
                                                  )
                                                  or
                                                  (ltfProcessed in Level.Tag.Flags) then begin {the synchronized procedure updated the original level; remove the solution from the queue}
                                                  Plugin.DeleteLevel(Level);
                                                  end
                                               else begin                       {keep the solution on the queue until the main-thread can process it}
                                                  Plugin.SetSelected(Level,False);

                                                  if fPluginResult=prOK then begin
                                                     Level.Tag.Flags:=Level.Tag.Flags+[ltfNew,ltfProcessed,ltfSolver];
                                                     Leave;  // temporarily release the lock in order to call the synchronized 'SynchronizedSavePluginResultToLogFile'
                                                     try     Synchronize(SynchronizedSavePluginResultToLogFile);
                                                     finally Enter; // acquire the lock again so it's in balance with the enclosing "enter/leave"
                                                     end;
                                                     with ToolsForm.SolveLevelsStringGrid do
                                                       if Assigned(Plugin.Level) and
                                                          (Plugin.Level.Flags>=FixedRows) and (Plugin.Level.Flags<RowCount) and
                                                          (Plugin.Level=TLevel(Objects[Ord(slcLevelName),Plugin.Level.Flags])) then begin
                                                          //Objects[Ord(slcLevelName),Plugin.Level.Flags]:=nil; // the level is in the row numbered 'Flags' in the string-grid; remove it now
                                                          //if ToolsForm.Visible {and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetSolver)} then
                                                          //   PostMessage(ToolsForm.Handle,MSG_SOLVER_REFRESH,MSG_PLUGIN_REFRESH_COMPACT,Cardinal(Plugin));
                                                          end;
                                                     Plugin.PendingResultsCount:=Succ(Plugin.PendingResultsCount);
                                                     if ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetOptimizer) then
                                                        PostMessage(ToolsForm.Handle,MSG_OPTIMIZER_REFRESH,MSG_PLUGIN_REFRESH_IMPORT,Cardinal(Plugin));
                                                     end;

                                                  if Assigned(Plugin.Level) then
                                                     Exclude(Plugin.Level.Tag.Flags,ltfLocked);
                                                  end;

                                               if ToolsForm.Visible
                                                  and
                                                  ((not Assigned(Plugin.Level))
                                                   or
                                                   (Plugin.Level.Flags<>ToolsForm.SolverTaskQueue.HighlightedRowNumber)
                                                  ) then
                                                  PostMessage(ToolsForm.Handle,MSG_SOLVER_REFRESH,MSG_PLUGIN_REFRESH_CHECKBOXES,Cardinal(Plugin)); // refresh checkboxes
                                               end
                                       else if Plugin is TOptimizerPlugin then begin
                                               if (ltfProcessed in Level.Tag.Flags) {the synchronized procedure updated the original level and/or marked the solution for deletion; remove the solution from the queue}
                                                  and
                                                  (not (ltfSelected in Plugin.Level.Tag.Flags)) {'ltfSelected': just to be sure; there can be situations where the synchronized procedure reports that it both has processed the solution and re-selected it for further processing}
                                                  then begin
                                                  Plugin.DeleteLevel(Level);
                                                  end
                                               else begin                       {keep the solution on the queue; the user may want to try optimizing it again with different settings}
                                                  if fPluginResult=prOK then begin {'True': the optimizer found improvements}
                                                     if (not (ltfSelected in Plugin.Level.Tag.Flags)) and Assigned(Plugin.Level.SnapshotsAsText) and (Plugin.Level.SnapshotsAsText.Count>1) then {>1: the synchronized procedure didn't discard all the snapshots/solutions produced by the plugin}
                                                        Include(Level.Tag.Flags,ltfProcessed);
                                                     Leave;  // temporarily release the lock so 'MoveOptimizedSolutionToSolverQueue' can call the synchronized 'SynchronizedSavePluginResultToLogFile'
                                                     try     Plugin.MoveOptimizedSolutionToSolverQueue(Plugin.DiscardDethronedSolutions,Level);
                                                     finally Enter; // acquire the lock again so it's in balance with the enclosing "enter/leave"
                                                     end;
                                                     end;

                                                  if Assigned(Plugin.Level) then begin
                                                     if (not (ltfSelected in Plugin.Level.Tag.Flags)) and
                                                        Assigned(Plugin.Level.SnapshotsAsText) then
                                                        with Plugin.Level.SnapshotsAsText do
                                                          if (not IsEmpty) and
                                                             (Last is TExtendedSnapshotAsText) then
                                                             with TExtendedSnapshotAsText(Last) do begin
                                                               OptimizationFlags:=0; // the field is first filled in when the optimization starts
                                                               end;
                                                     Exclude(Plugin.Level.Tag.Flags,ltfLocked);
                                                     end;
                                                  end;

                                               if ToolsForm.Visible
                                                  and
                                                  ((not Assigned(Plugin.Level))
                                                   or
                                                   (Plugin.Level.Flags<>ToolsForm.OptimizerTaskQueue.HighlightedRowNumber)
                                                  ) then
                                                  PostMessage(ToolsForm.Handle,MSG_OPTIMIZER_REFRESH,MSG_PLUGIN_REFRESH_CHECKBOXES,Cardinal(Plugin)); // refresh checkboxes
                                               end
                                       else begin                               {unspecified plugin}
                                               if ltfProcessed in Level.Tag.Flags then begin {the synchronized procedure updated the original level; remove the solution from the queue}
                                                  Plugin.DeleteLevel(Level);
                                                  end
                                               else begin                       {keep the solution on the queue}
                                                  if fPluginResult=prOK then begin {'True': the plugin succeeded}
                                                     Include(Level.Tag.Flags,ltfNew);
                                                     Include(Level.Tag.Flags,ltfProcessed);
                                                     Leave;  // temporarily release the lock in order to call the synchronized 'SynchronizedSavePluginResultToLogFile'
                                                     try     Synchronize(SynchronizedSavePluginResultToLogFile);
                                                     finally Enter; // acquire the lock again so it's in balance with the enclosing "enter/leave"
                                                     end;
                                                     Plugin.PendingResultsCount:=Succ(Plugin.PendingResultsCount);
                                                     end;
                                                  if Assigned(Plugin.Level) then
                                                     Exclude(Plugin.Level.Tag.Flags,ltfLocked);
                                                  end;
                                            end;
                                       end;

                                    if fState=Ord(ptsProcessLevel) then         {'True': the main-thread hasn't sent a new command}
                                       fState:=Ord(ptsLoadLevel);               {check the plugin-queue for more levels to process}

                                    PostMessage(MainForm.Handle,MSG_PLUGIN,1,Cardinal(Plugin));
                            finally Plugin.Level:=nil;                          {clear currently processed level}
                                    Leave;
                                    if (fState=Ord(ptsIdle)) and
                                       Assigned(Plugin) and
                                       (Plugin.PluginThread=Self) and
                                       ((Plugin=MainForm.Solver) or (Plugin=MainForm.Optimizer))
                                       then
                                       SynchronizePluginOnIdle;                 {this should work even though the state isn't protected here; 'SynchronizedPluginOnIdle()' checks that the thread is idle before any actions are taken}
                            end;

                          end;
        ptsTerminate    : try     if not Terminated then Terminate;             {terminates the thread; don't confuse this with the solver command 'Terminate'}
                          finally Leave;
                          end;
        ptsTerminated   : try     if not Terminated then Terminate;             {terminates the thread; don't confuse this with the solver command 'Terminate'}
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

  try    fState:=Ord(ptsTerminated);
  except on E:Exception do begin end;
  end;
end;

function  TPluginThread.GetStatusText:String;
begin
  with PluginStatusInfo do begin
    StatusText[High(StatusText)]:=NULL_CHAR;                                    {ensure that the status text is a null-terminated string}
    Result:=StatusText;
    end;
end;

function  TPluginThread.MakeLevel(var Level__:TLevel):Boolean;
begin // caution: when levels are recycled, the levels may contain board line(s) and snapshot(s), so the caller must be careful to overwrite data instead of adding data to a recycled level
  if RecycledLevels.IsEmpty then
     Result:=CreateObject(otLevel,TNode(Level__))
  else begin
     Level__          :=TLevel(RecycledLevels.Pop);
     Level__.Tag.Value:=0;
     Level__.Flags    :=0;
     Result           :=Level__.SetText('');
     end;
end;

function  TPluginThread.PluginCallBackFunction:TPluginResult;
begin // trampoline to the synchronized call-back function
  // fPluginCallBackFunctionResult:=prOK; {default call-back result (don't reset it here; the host may have changed the value to 'prTimeOut'}
  fPluginStatusInfoPointer:=Addr(PluginStatusInfo);
  with PluginStatusInfo do StatusText[High(StatusText)]:=NULL_CHAR;             {ensure that the status text is a null-terminated string}
  Synchronize(SynchronizedPluginCallBackFunction);                              {only procedures can be synchronized, hence its "parameters" are globally defined, i.e., 'fPluginStatusInfo' and 'fPluginCallBackFunctionResult'}
  Result:=fPluginCallBackFunctionResult;
end;

procedure TPluginThread.SetStatusText(const StatusText__:String);
var CharCount:Integer;
begin
  with PluginStatusInfo do begin
    CharCount:=Min(Length(StatusText__),(SizeOf(StatusText)-SizeOf(Char)) div SizeOf(Char));
    if CharCount<>0 then System.Move(PChar(Addr(StatusText__[1]))^,PChar(Addr(StatusText))^,CharCount*SizeOf(Char));
    StatusText[Low(StatusText)+CharCount]:=NULL_CHAR; // add null-terminator
    end;
end;

procedure TPluginThread.SynchronizedPluginCallBackFunction;
var i{,ACol,ARow}:Integer; {APoint:TPoint;} PluginLevelStringGrid:TStringGrid;
begin
  if (not PluginShutdown) and
     Assigned(Plugin) and Plugin.HasThread then begin
     if Assigned(PluginStatusInfoPointer) and
        ((fState=Ord(ptsProcessLevel))
         or
         (fState=Ord(ptsLoadLevel))
        ) then with PluginStatusInfoPointer^ do
        if Size>=SizeOf(PluginStatusInfoPointer^) then begin
           if        OpenForm.Visible
                     and
                     (((OpenForm.Task=otSolver   ) and (Plugin is TSolverPlugin   ))
                      or
                      ((OpenForm.Task=otOptimizer) and (Plugin is TOptimizerPlugin))
                     )
                     then begin
                     PluginLevelStringGrid:=OpenForm .PluginLevelStringGrid;
                     if ToolsForm.Visible then with ToolsForm.PluginLevelStringGrid do
                        if Cells[1,Ord(pcbiiStatus)]<>'' then
                           for i:=0 to Pred(RowCount) do Cells[1,i]:='';
                     end
           else if   ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver   ) and (Plugin is TSolverPlugin) then
                     PluginLevelStringGrid:=ToolsForm.PluginLevelStringGrid
           else if   ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer) and (Plugin is TOptimizerPlugin) then
                     PluginLevelStringGrid:=ToolsForm.PluginLevelStringGrid
           else if   ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetGenerator) and (Plugin is TGenerator) then
                     PluginLevelStringGrid:=ToolsForm.PluginLevelStringGrid
                else PluginLevelStringGrid:=nil;
           if Assigned(PluginLevelStringGrid) then with PluginLevelStringGrid do begin
              if   MovesGenerated<>0 then begin
                   if   (MovesGenerated>=ONE_MILLION) {and (MovesGenerated<=High(MovesGenerated)-HALF_MILLION)} then // with 64-bit signed integers, an overflow check seems unnecessary
                        Cells[1,Ord(pcbiiMoves )]:=IntToStr((MovesGenerated+HALF_MILLION) div ONE_MILLION)+MillionSuffixText
                   else Cells[1,Ord(pcbiiMoves )]:=IntToStr( MovesGenerated);
                   end
              else Cells[1,Ord(pcbiiMoves)]:='';
              if   PushesGenerated<>0 then begin
                   if   (PushesGenerated>=ONE_MILLION) {and (PushesGenerated<=High(PushesGenerated)-HALF_MILLION)} then
                        Cells[1,Ord(pcbiiPushes)]:=IntToStr((PushesGenerated+HALF_MILLION) div ONE_MILLION)+MillionSuffixText
                   else Cells[1,Ord(pcbiiPushes)]:=IntToStr( PushesGenerated);
                   end
              else Cells[1,Ord(pcbiiPushes)]:='';
              if   StatesGenerated<>0 then begin
                   if   (StatesGenerated>=ONE_MILLION) {and (StatesGenerated<=High(StatesGenerated)-HALF_MILLION)} then
                        Cells[1,Ord(pcbiiStates)]:=IntToStr((StatesGenerated+HALF_MILLION) div ONE_MILLION)+MillionSuffixText
                   else Cells[1,Ord(pcbiiStates)]:=IntToStr(StatesGenerated);
                   end
              else Cells[1,Ord(pcbiiStates)]:='';
              if   ((Plugin is TSolverPlugin)
                    and
                    ((ToolsForm.BtnSolveLevel.Tag=Ord(pbsTerminating))
                     or
                     (OpenForm .BtnSolveLevel.Tag=Ord(pbsTerminating))
                    )
                   )
                   or
                   ((Plugin is TOptimizerPlugin)
                    and
                    ((ToolsForm.BtnOptimizeGames.Tag=Ord(pbsTerminating))
                     or
                     (OpenForm .BtnOptimizeGames.Tag=Ord(pbsTerminating))
                    )
                   )
                   or
                   ((Plugin is TGenerator)
                    and
                    ((ToolsForm.BtnGenerateLevels.Tag=Ord(pbsTerminating))
                    )
                   )
                   then begin
                   Cells[1,Ord(pcbiiStatus     )]:=TerminatedByUserText;
                   if fPluginCallBackFunctionResult<>prTimeOut then
                      fPluginCallBackFunctionResult:=prUnsolved;
                   end
              else begin {
                         // if the text contains more than one line, then
                         // change the line breaks to spaces
                         i:=System.Pos(LF,StatusText);
                         if i<>0 then begin
                            repeat
                              Inc(i,-1+Low(StatusText)); // '-1': 'System.Pos returns a 1-based offset
                              StatusText[i]:=SPACE;
                              if (i>Low(StatusText)) and (StatusText[Pred(i)]=CR) then
                                 StatusText[Pred(i)]:=SPACE; // the text contained CR+LF, and not only a LF
                              i:=System.Pos(LF,StatusText);
                            until i=0;
                            end;
                         }
                         StrSubstituteLineSeparatorsBySpaces(Addr(PluginStatusInfoPointer^.StatusText));
                         Cells[1,Ord(pcbiiStatus)]:=StatusText;
                   end;
              if   TimeMS>=500 then Cells[1,Ord(pcbiiTime)]:=FormatTimeMS(TimeMS)
              else Cells[1,Ord(pcbiiTime)]:='';
              {
              if (Plugin is TOptimizerPlugin) and
                 (PluginLevelStringGrid=ToolsForm.PluginLevelStringGrid) then
                 ToolsForm.StatusText:=Cells[1,Ord(pcbiiStatus)];
              }
              if ToolsForm.PanelToolTips.Visible then ToolsForm.PanelToolTips.Hide;
              if OpenForm .PanelToolTips.Visible then OpenForm .PanelToolTips.Hide;
              {
              if (Screen.ActiveForm=ToolsForm) and (PluginLevelStringGrid=ToolsForm.PluginLevelStringGrid) then
                 with ToolsForm do with PluginLevelStringGrid do begin
                   APoint:=ScreenToClient(Mouse.CursorPos);
                   MouseToCell(APoint.X,APoint.Y,ACol,ARow);
                   if (ARow=Ord(pcbiiStatus)) and (ACol=1) and (PanelToolTips.Caption<>'') then begin
                      // try to show the tool tips when the mouse is over the status test;
                      SolveLevelMouseMove(PluginLevelStringGrid,[],APoint.X,APoint.Y);
                     end;
                   end;
              }
              end;
           {
           // test changing optimization mode from the plugin,
           // i.e., where the plugin overwrites the requested mode at the time
           // the Optimize() function call was issued
           if   (Flags and SOKOBAN_PLUGIN_FLAG_MOVES)<>0 then
                Flags:=SOKOBAN_PLUGIN_FLAG_PUSHES+SOKOBAN_PLUGIN_FLAG_SECONDARY_MOVES
           else Flags:=SOKOBAN_PLUGIN_FLAG_MOVES +SOKOBAN_PLUGIN_FLAG_SECONDARY_PUSHES;
           }
           if Flags<>OldStatusInfoFlags then begin // 'True': the status flags changed
              OldStatusInfoFlags:=Flags;
              if Plugin=MainForm.Optimizer then begin // 'True': the plugin may have changed optimization mode; in that case, update the level and the screen
                 i:=Flags and (PRIMARY_METRICS_FLAGS_MASK+SECONDARY_METRICS_FLAGS_MASK);
                 if Assigned(Plugin.Level) and
                    (ltfLocked in Plugin.Level.Tag.Flags)  and
                    (not Plugin.Level.SnapshotsAsText.IsEmpty) then with TExtendedSnapshotAsText(Plugin.Level.SnapshotsAsText.Last) do begin
                    OptimizationFlags:=i; // remember the actual optimization flags

                    if i<>Tag then begin // 'True': the actually used optimization mode by the optimizer differs from the optimization mode requested by the user
                       if (Tag and (PRIMARY_METRICS_FLAGS_MASK+SECONDARY_METRICS_FLAGS_MASK))<>0 then begin
                        // '<>0': the old optimization mode wasn't 'settings';
                        // if it was 'settings', then keep this as optimization mode,
                        // the rationale being that whatever the plugin uses as
                        // actual optimization mode, then this is an internal matter
                        // for that particular plugin, and it should not influence
                        // the choice for the item on the task queue;
                        // if the items on the task queue were updated with the actual
                        // optimization mode, then the user couldn't change settings
                        // later for a whole group of items;
                       Tag:=i;
                       end;
                       if  (Plugin.Level=ToolsForm.OptimizerTaskQueue.Levels[Plugin.Level.Flags]) then begin
                           ToolsForm.OptimizerTaskQueue.RefreshRow(Plugin.Level.Flags);
                           end;
                       end;
                    end;
                 end;
              end;
           end;
     end
  else // shutdown
    if fPluginCallBackFunctionResult=prOK then
       fPluginCallBackFunctionResult:=prTerminatedByUser;
end;

procedure TPluginThread.SynchronizePluginError(const MessageText__:String);
begin
  MessageText:=MessageText__;
  Synchronize(SynchronizedPluginError);
end;

procedure TPluginThread.SynchronizedPluginError;
begin
  if (not PluginShutdown) and Assigned(Plugin) and (Plugin.HasThread) and (Plugin.PluginThread=Self) then
     Error(MessageText,TitleWithOptionalSubTitle(Application.Title,Plugin.PluginName));
end;

procedure TPluginThread.SynchronizedPluginFailed;
begin
  if (not PluginShutdown) and Assigned(OpenForm) and
     Assigned(Plugin) and (Plugin.HasThread) and (Plugin.PluginThread=Self) then
     OpenForm.SynchronizedPluginCallback(Plugin,False,False);
end;

//procedure TPluginThread.SynchronizedHint;
//begin
//  if (not PluginShutdown) then MainForm.Status.Hint:=MessageText;
//end;

procedure TPluginThread.SynchronizePluginOnIdle;
begin
  if Assigned(Plugin) then begin
     Enter;
     try     fState:=Ord(ptsIdle);
     finally Leave;
     end;
     if (not PluginShutdown) then Synchronize(SynchronizedPluginOnIdle);
     end;
end;

procedure TPluginThread.SynchronizedPluginOnIdle;
begin
  if (not PluginShutdown) and (fState=Ord(ptsIdle)) and
     Assigned(OpenForm) and Assigned(Plugin) and (Plugin.HasThread) and (Plugin.PluginThread=Self) then
     OpenForm.SynchronizedPluginCallback(Plugin,False,True);
end;

procedure TPluginThread.SynchronizedPluginSucceeded;
begin
  if (not PluginShutdown) and Assigned(OpenForm) and
     Assigned(Plugin) and (Plugin.HasThread) and (Plugin.PluginThread=Self) then
     OpenForm.SynchronizedPluginCallback(Plugin,True,False);
end;

procedure TPluginThread.SynchronizedSavePluginResultToLogFile;
begin
  if (not PluginShutDown) and
     Assigned(Plugin) and (Self=Plugin.PluginThread) and
     Assigned(Plugin.Level) and
     (ltfLocked in Plugin.Level.Tag.Flags) and
     Assigned(Plugin.Level.SnapshotsAsText) and
     (not Plugin.Level.SnapshotsAsText.IsEmpty) and
     (Plugin.Level.SnapshotsAsText.First is TSnapshotAsText) and
     (not  TSnapshotAsText(Plugin.Level.SnapshotsAsText.First).MovesAsTextLines.IsEmpty) and
     (not (TSnapshotAsText(Plugin.Level.SnapshotsAsText.First).MovesAsTextLines.First.Text='')) and
     (not ((Plugin is TOptimizerPlugin) and (Plugin.Level.SnapshotsAsText.First.Next=nil))) then // optimizer: the original solution is the last one on the list;
     MainForm.SaveSnapshotToLogFile(Plugin.Level.Name,'',TSnapshotAsText(Plugin.Level.SnapshotsAsText.First).MovesAsTextLines.First.Text);
end;

procedure TPluginThread.SynchronizedStartOptimizingLevel;
var Col,Row,Index,Length,RepeatInterval,SelectedSquaresCount:Integer; //PrimaryMetric,SecondaryMetric:TGameMetrics;
    s,s1:String;
    Snapshot:TExtendedSnapshotAsText;
begin
  if (not PluginShutdown) and
     Assigned(OpenForm) and
     Assigned(ToolsForm) and
     Assigned(ToolsForm.Game) and
     Assigned(ToolsForm.OptimizerTaskQueue) and
     (Plugin=MainForm.Optimizer) and
     Assigned(Plugin) then with ToolsForm.OptimizerTaskQueue do begin
     AlternateOptimization.PartitionSolutionIntoSubIntervals := False;
     if Assigned(Plugin) and Assigned(Plugin.Level) and
        Assigned(Plugin.Level.SnapshotsAsText) then with Plugin.Level do
        if (ltfLocked in Tag.Flags) and (Plugin.Level=Levels[Flags]) then begin

           if   ToolsForm.Game.IsBrowsing and
                ((ToolsForm.ImageReplaySpeed.Tag and 1)<>0) and // '<>0': mouse down
                (StringGrid.Row=Flags) and
                (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetOptimizer) then // 'True': the user is selecting a range of moves to be optimized for the current level; stop it and use the current selection
                ToolsForm.LeaveBrowseMode(True);

           Selected[Flags]:=False;
           HighlightedRowNumber:=Flags;

           if   ToolsForm.OptimizationComboBox.Visible and (StringGrid.Row=HighlightedRowNumber) then begin
                ToolsForm.HideEditors;
                ToolsForm.StatusText:='';
                with ToolsForm.OptimizerTaskQueue do StringGrid.SetFocus;
                end;

           if   (not SnapshotsAsText.IsEmpty) then begin
                Snapshot := TExtendedSnapshotAsText( SnapshotsAsText.Last );
                if not ( Snapshot is TExtendedSnapshotAsText ) then
                   Snapshot := nil;
                end
           else Snapshot:=nil;
           if   Assigned( Snapshot ) then begin
                s:=StrWithQuotedAmpersands(Snapshot.Name);
                if Snapshot.SelectedRange[ 0 ] < Snapshot.SelectedRange[ 1 ] then
                   s := s + Format( ' {%s: %d..%d}', [ GameMetricsText[ gmPushes ], Snapshot.SelectedRange[ 0 ], Min( Snapshot.SelectedRange[ 1 ], Snapshot.SelectedRange[ 2 ] ) ] );
                end
           else s:='';
           if   s<>'' then s:=SPACE+s+SPACE; // the first space is for pretty-printing in the panel; the second space is for a better balance in the "mouse-over" tool-tip
           if   OpenForm.Visible and (OpenForm.Task=otOptimizer) then begin
                OpenForm.PluginLevelFileNamePanel.Caption:=s;
                end;
           if   ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetOptimizer) then with ToolsForm do begin
                PluginLevelFileNamePanel.Caption:=s;
                OpenForm.BtnPluginSettings.Hint:=HintPluginSettings2Text;
                ToolsForm.BtnOptimizerSettings.Hint:=OpenForm.BtnPluginSettings.Hint;
                for Index:=0 to Pred(ToolsForm.PluginLevelStringGrid.RowCount) do ToolsForm.PluginLevelStringGrid.Cells[1,Index]:='';
                end;

           if   Assigned(Snapshot) then begin
                if Snapshot.SelectedRange[ 0 ] < Snapshot.SelectedRange[ 1 ] then begin// 'True': only optimize the selected range of pushes
                   RepeatInterval := Snapshot.SelectedRange[ 6 ]; // save repeating interval size, if any
                   for  Index := 3 to High( Snapshot.SelectedRange ) do
                        Snapshot.SelectedRange[ Index ] := 0; // clear the selected range measured in characters in the text-version of the solution
                   Snapshot.SelectedRange[ 6 ] := RepeatInterval; // restore repeating interval size, if any
                   if   Plugin.ReplayGame(Plugin.Level, BoardBuffer, MovesBuffer, Plugin.WallifyBoxesOutsideRangeOfPushesToBeOptimized ) then // OK; the optimization task changed from the entire solution to the selected range of moves
                        AlternateOptimization.PartitionSolutionIntoSubIntervals := ( RepeatInterval > 0 ) // 'True': optimize the rest of the snapshot using subintervals of this size
                   else raise Exception.Create(TEXT_TASK_FAILED);
                   end;
                if Plugin.IsDefaultPlugin and // only the default plugin(s) are guaranteed to support optimizing selected squares; at the time of writing, the optimizer interface doesn't provide support for that feature;
                   Snapshot.Notes.Lines.ReadString(KEY_SELECTED_SQUARES,s1) then begin // 'True': only optimize pushes passing the selected squares
                   Length:=System.Length(s1);
                   Index:=0;
                   SelectedSquaresCount:=0;
                   for Row:=1 to Plugin.Level.Tag.BoardHeight do
                       for Col:=1 to Plugin.Level.Tag.BoardWidth do
                           if   Index<Length then begin
                                if s1[Index+STRING_BASE]='1' then begin // mark the square in the buffer as selected for optimization; all normal board squares are 7-bit ASCII characters, hence, the 8th bit is free to use as a flag value
                                   BoardBuffer^[Index]:=Char(Ord(BoardBuffer^[Index]) or SOKOBAN_CHAR_FLAG);
                                   Inc( SelectedSquaresCount );
                                   end;
                                Inc(Index);
                                end
                           else break; // quick-and-dirty exit loop at the end of the string
                   if SelectedSquaresCount<>0 then begin
                      // add the "Selected squares: 999" text; the reason the calculation isn't performed earlier,
                      // so this information is available when the screen status text was updated 10-20 lines ago,
                      // is that it's preferable to get the correct level title on the screen before the call
                      // to 'Plugin.ReplayGame', in the unlikely case that it fails and raises an error;
                      s:=SPACE + Trim( s ) + Format( ' {%s: %d}', [ SelectedSquaresText, SelectedSquaresCount ] ) + SPACE;
                      if   OpenForm.Visible and (OpenForm.Task=otOptimizer) then
                           OpenForm.PluginLevelFileNamePanel.Caption:=s;
                      if   ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetOptimizer) then
                           ToolsForm.PluginLevelFileNamePanel.Caption:=s;
                      end;
                   end;
                end;
           end;
     end;
end;

procedure TPluginThread.SynchronizedStartSolvingLevel;
var i:Integer; s:String; NewLevel:TLevel;
begin
  if (not PluginShutdown) and
     Assigned(OpenForm) and
     Assigned(ToolsForm) and
     (Plugin=MainForm.Solver) and
     Assigned(Plugin) then with ToolsForm.SolverTaskQueue do begin
     if Assigned(Plugin) and Assigned(Plugin.Level) then with Plugin.Level do
        if (ltfLocked in Tag.Flags) and (Plugin.Level=Levels[Flags]) then begin
           Selected[Flags]:=False;

           if (not SnapshotsAsText.IsEmpty) then begin // 'True': the level has already been solved; clone the level and transfer the existing solution to the clone, leaving the existing level as a fresh, unsolved level
              if Plugin.LoadLevel(Tag.BoardWidth,Tag.BoardHeight,-1,0,Name,BoardAsTextLines.First.Text,nil,True,False,NewLevel) then begin
                 NewLevel.Tag.Flags:=Tag.Flags-[ltfLocked,ltfSelected,ltfSelectedForFurtherProcessing]+[ltfNew,ltfProcessed];
                 SnapshotsAsText.Swap(NewLevel.SnapshotsAsText);
                 end
              else begin
                 // if a new level couldn't be created then leave the existing
                 // solution untouched; it may, however, be lost later because
                 // postprocessing only imports the first solution on the list
                 end;
              end;

           HighlightedRowNumber:=Flags;
           s:=StrWithQuotedAmpersands(VisualFileName(Name));
           if   s<>'' then s:=SPACE+s+SPACE; // the first space is for pretty-printing in the panel; the second space is for a better balance in the "mouse-over" tool-tip
           if   OpenForm.Visible and (OpenForm.Task=otSolver) then begin
                OpenForm.PluginLevelFileNamePanel.Caption:=s;
                OpenForm.BtnPluginSettings.Hint:=HintPluginSettings2Text;
                for i:=0 to Pred(OpenForm.PluginLevelStringGrid.RowCount) do OpenForm.PluginLevelStringGrid.Cells[1,i]:='';
                end;
           if   ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetSolver) then begin
                ToolsForm.PluginLevelFileNamePanel.Caption:=s;
                OpenForm.BtnPluginSettings.Hint:=HintPluginSettings2Text;
                ToolsForm.BtnSolverSettings.Hint:=OpenForm.BtnPluginSettings.Hint;
                for i:=0 to Pred(ToolsForm.PluginLevelStringGrid.RowCount) do ToolsForm.PluginLevelStringGrid.Cells[1,i]:='';
                end;
           end;
     if ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver then with ToolsForm do begin
        PluginEditMenuItemDeleteLevels.Enabled:=SelectedCount<>0;
        PluginToolButtonDelete.Enabled:=PluginEditMenuItemDeleteLevels.Enabled;
        PopupMenuItemDeleteLevels.Enabled:=PluginEditMenuItemDeleteLevels.Enabled;
        end;
     end;
end;

{Plugin, e.g., solvers and optimizers}

constructor TPlugin.Create(const PluginTypeText__,DefaultPluginFileNameStub__:String; Button__:TButton; ComboBox__:TComboBox);
begin
  fPluginFileName:=DEFAULT_VALUE; DLLHandle:=0; FillChar(fPluginFunctions,SizeOf(fPluginFunctions),0);
  fPluginThread:=nil; fPriority:=tpNormal; fSokoFile:=nil; fLevel:=nil; fPluginLimitsAsText:='';
  fStartTimeMS:=0; fIsDefaultPlugin:=False; fTerminatedByUser:=False; fHasPluginTypeInName:=False;
  fPendingResultsCount:=0; fSelectedLevelsCount:=0;
  AddDateToSolutionNotes:=True; AddPluginNameToSolutionTitles:=True;
  DiscardDethronedSolutions:=True; // optimizer only
  SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly:=True; // optimizer only
  SubIntervalOverlapPct:=DEFAULT_SUB_INTERVAL_OVERLAP_PCT; // optimizer only
  WallifyBoxesOutsideRangeOfPushesToBeOptimized:=True; // optimizer only
  StatisticsEnabled:=False;
  fPluginTypeText:=PluginTypeText__; DefaultPluginFileNameStub:=DefaultPluginFileNameStub__;
  fButton:=Button__; fComboBox:=ComboBox__;
  TimeLimitEnabled:=False; TimeLimitMS:=DEFAULT_PLUGIN_TIME_LIMIT_MS;
end;

destructor TPlugin.Destroy;
var TimeMS:TTimeMS;
begin // precondition: the plugin can only be destroyed during application shutdown
  Close;

  if Assigned(fPluginThread) then with fPluginThread do begin
     try
       if Suspended and ( not Terminated ) then begin
          Enter;
          try     State:=Ord(ptsTerminate); // try to terminate the thread by setting the plugin state to 'ptsTerminate'
                  Terminate;                // try to terminate the thread by calling 'TThread.Terminate'
                  Resume;
          finally Leave;
                  TimeMS:=GetTimeMS;
                  repeat SleepEx(50,False); // give the thread some time to finish normally
                  until  (State=Ord(ptsTerminated))
                         or
                         (CalculateElapsedTimeMS(TimeMS,GetTimeMS)>WAIT_FOR_THREAD_TO_TERMINATE_MS);
          end;
          end;

       if (not Suspended) and (State<>Ord(ptsTerminated)) then
          // if the thread hasn't terminated itself by now, then the only thing
          // that can be done here is to suspend it;
          //
          // if the thread has finished, i.e., exited its 'Execute' procedure,
          // then an exception is raised when the application tries to suspend
          // it, hence, the 'try..except' statement;
          try    Suspend;
          except on E:Exception do begin end;
          end;
     except on E : Exception do begin
               // Application.MessageBox( PChar( E.Message ) , PChar( Application.Title ), MB_OK+MB_ICONERROR );
               end;
     end;
     fPluginThread.Free; fPluginThread:=nil;
     end;

  PluginShutdown:=True; // there may be a pending synchronized callback from a thread after the thread has been destroyed; in that case the callback must be informed to do nothing, hence, this switch

  if (Self is TSolverPlugin) or (Self is TOptimizerPlugin) then ImportGames(True);

  fSokoFile.Free; fSokoFile:=nil;
end;

function TPlugin.Add(const FileName__:String):Boolean;
var i,j:Integer; s,s1:String;
begin
  Result:=False;
  if Assigned(ComboBox) then with ComboBox do with Items do begin
    if   StrEqual(FileName__,DEFAULT_VALUE) then
         s:=DefaultPluginFileName
    else s:=ExpandFileName(FileName__);

    if (s<>'') and (Self.IndexOf(s)<0) and FileExists(s) then
       if Sorted then begin // 'True': the combobox contains stripped filenames (without path and extension)
          if (not IsActive) and Open(s) then begin
             i:=ToolsForm.StringsDB.AddCI(s);
             if i>0 then begin
                s1:=PluginName;
                if s1='' then s1:=ExtractFileNameWithoutPathAndExtension(s);
                //else s1:=s1+'  ['+ExtractFileName(s)+']';
                Add(s1);
                for j:=0 to Pred(Count) do // find the position of the new item; a simple 'IndexOf(s1)' won't suffice because there may be several items with identical names
                    if (Objects[j]=nil) and StrEqual(Items[j],s1) then begin // 'Objects[j]=nil': this identifies the new item
                       ItemIndex:=j;
                       Objects[ItemIndex]:=TObject(i);
                       SetComboBoxDropDownWidth(ComboBox,ItemIndex,False);
                       Result:=True;
                       break;
                       end;
                end;
             end;
          end
       else
          Add(s); // add the raw filename to the unsorted list
    end;
end;

procedure TPlugin.ClearQueue(RemoveSelectedItems__:Boolean);
var Level,NextLevel:TLevel;
begin
  Enter;
  try     if Assigned(fSokoFile) and (not SokoFile.Levels.IsEmpty) then begin
             Level:=TLevel(SokoFile.Levels.First);
             repeat NextLevel:=TLevel(Level.Next);
                    if RemoveSelectedItems__ and (ltfSelected in Level.Tag.Flags) then
                       SetSelected(Level,False);
                    if (Level.Tag.Flags*[ltfSelected,ltfSelectedForFurtherProcessing,ltfLocked,ltfNew,ltfProcessed])=[] then
                       DeleteLevel(Level);
                    Level:=NextLevel;
             until  Level=nil;
             if SokoFile.Levels.IsEmpty then begin
                // the counters may be out of sync, hence, at least ensure that they are 0 if the queue is empty
                fPendingResultsCount:=0;
                fSelectedLevelsCount:=0;
                end;
             end;

  finally Leave;
  end;
end;

function  TPlugin.Close:Boolean;
var TimeMS:TTimeMS;
begin
  // precondition: 'Close' may only be called when the plugin thread isn't
  // running, or during application shutdown, in which case it's assumed that
  // the game in the main window ('MainForm.Game') is inactive;

  Result:=True;

  if not IsRunningInAnotherThread then Terminate;

  if (ThreadState<>ptsIdle) and (ThreadState<>ptsTerminated)  then begin
     Resume;
     TimeMS:=GetTimeMS;
     while  (ThreadState<>ptsIdle) and (ThreadState<>ptsTerminated) and
            (CalculateElapsedTimeMS(TimeMS,GetTimeMS)<=WAIT_FOR_THREAD_TO_TERMINATE_MS)
            do begin
            SleepEx(50,False);

            // raising the 'MainForm.Game.IsReplaying' flag prevents the user
            // from performing any actions in the main window; this makes it
            // safe to process pending messages here;
            //
            // the 'IsReplaying' flag is raised here instead of 'IsBusy' because
            // the latter is used more frequently to block validating and
            // performing moves, and maybe the 'ImportGame' function for the
            // plugin depends on this functionality in subtle ways;
            try     MainForm.Game.IsReplaying:=True;
                    Application.ProcessMessages; // process messages: the plugin thread may trigger calls to synchronized procedures like 'TPluginThread.SynchronizedPluginCallBackFunction'
            finally MainForm.Game.IsReplaying:=False;
            end;
            end;
     try    Suspend;
     except on E:Exception do begin end;
     end;
     end;

  if (ThreadState<>ptsIdle) and (ThreadState<>ptsTerminated) then begin
     // the plugin-thread didn't co-operate, hence, there is nothing else to do
     // but to destroy it, even though its resources will leak;

     // there may be a pending synchronized callback from a thread after the thread has been destroyed;
     // in that case the callback must be informed not to do anything, hence, the 'PluginShutdown' switch;
     // however, setting 'PluginShutdown' to 'True' here causes any other plugins
     // to stop responding too, in effect making them loose their results;
     // that's why this function should only be called at a time when the
     // plugin isn't running, unless it's during program shutdown where any
     // pending results are discarded anyway.
     PluginShutdown:=True;

     if Assigned(fPluginThread) and (not fPluginThread.Suspended) then
        try    fPluginThread.Suspend;
        except on E:Exception do begin end;
        end;

     fPluginThread.Free; fPluginThread:=nil;

     if Assigned(OpenForm) then
        OpenForm.SynchronizedPluginCallback(Self,False,True);
     end;

  Result:=((DLLHandle=0) or FreeLibrary(DLLHandle)) and Result;
  DLLHandle:=0; FillChar(fPluginFunctions,SizeOf(fPluginFunctions),0); // clear info even if 'FreeLibrary' failed, in which case the state of the info is undefined
  fIsDefaultPlugin:=False;

  Self.StartTimeMS:=0;
  if Assigned(OpenForm) then
     OpenForm.PluginTimer.Enabled:=(Assigned(MainForm.Solver   ) and (MainForm.Solver   .StartTimeMS<>0))
                                   or
                                   (Assigned(MainForm.Optimizer) and (MainForm.Optimizer.StartTimeMS<>0))
                                   or
                                   //(Assigned(MainForm.Generator) and (MainForm.Generator.StartTimeMS<>0));
                                   (Assigned(MainForm.Generator) and (MainForm.Generator.IsActive));

  if fLevel<>nil then with fLevel.Tag do begin
     Exclude(Flags,ltfLocked); Exclude(Flags,ltfProcessed);
     fLevel:=nil; {no currently processed level}
     end;
end;

function  TPlugin.DefaultPluginFileName:String;
var Path,FileName:String;
begin
//Path    :=MainForm.ApplicationDataPath;
  Path    :=ExtractFilePath(Application.ExeName);
  FileName:=ChangeFileExt(DefaultPluginFileNameStub,DLL_FILE_EXT);
  Result:=Path+StrWithTrailingPathDelimiter(DEFAULT_PLUGINS_DIRECTORY)+FileName;
  if not FileExists(Result) then
     if FileExists(Path+FileName) then Result:=Path+FileName; // 'True': found a plugin in the application's home-directory
end;

procedure TPlugin.DeleteLevel(Level__:TLevel);
begin
  if Assigned(Level__) then with Level__ do begin
     Enter;
     try
             if      Self is TOptimizerPlugin then begin
                     if   Level__=ToolsForm.OptimizerTaskQueue.Levels[Flags] then begin
                          ToolsForm.OptimizerTaskQueue.Selected[Flags]:=False;
                          ToolsForm.OptimizerTaskQueue.Levels[Flags]:=nil; // the level is in the row numbered 'Flags' in the string-grid
                          if ToolsForm.Visible {and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetOptimizer)} then
                             PostMessage(ToolsForm.Handle,MSG_OPTIMIZER_REFRESH,MSG_PLUGIN_REFRESH_COMPACT,Cardinal(Self));
                          end;
                     SetSelected(Level__,False);
                     Self.SokoFile.Levels.Remove(Level__,True);
                     end
             else if Self is TSolverPlugin then with ToolsForm.SolveLevelsStringGrid do begin
                     if   Level__=ToolsForm.SolverTaskQueue.Levels[Flags] then begin
                          ToolsForm.SolverTaskQueue.Selected[Flags]:=False;
                          ToolsForm.SolverTaskQueue.Levels[Flags]:=nil; // the level is in the row numbered 'Flags' in the string-grid
                          if ToolsForm.Visible {and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetSolver)} then
                             PostMessage(ToolsForm.Handle,MSG_SOLVER_REFRESH,MSG_PLUGIN_REFRESH_COMPACT,Cardinal(Self));
                          end;
                     SetSelected(Level__,False);
                     Self.SokoFile.Levels.Remove(Level__,True);
                     end
             else if Self is TGenerator then with ToolsForm.GenerateLevelsStringGrid do begin
                     if   Level__=Self.Level then Self.Level:=nil;
                     TGenerator(Self).RemoveLevel(Level__); // remove the level from the population (it doesn't remove or delete the level from the level file)
                     if   Level__=ToolsForm.GeneratorTaskQueue.Levels[Flags] then begin
                          ToolsForm.GeneratorTaskQueue.Selected[Flags]:=False;
                          ToolsForm.GeneratorTaskQueue.Levels[Flags]:=nil; // the level is in the row numbered 'Flags' in the string-grid
                          if ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetGenerator) then
                             if not (ltfLocked in Level__.Tag.Flags) then
                                //ToolsForm.GeneratorTaskQueue.Refresh(True);
                                PostMessage(ToolsForm.Handle,MSG_GENERATOR_REFRESH,MSG_PLUGIN_REFRESH_COMPACT,Cardinal(Self));
                          end;
                     SetSelected(Level__,False);
                     if   Assigned(PluginThread) and (PluginThread.RecycledLevels.Count<2) then begin // 'True': save the level for recycling
                          Self.SokoFile.Levels.Remove(Level__,False);
                          PluginThread.RecycledLevels.Push(Level__);
                          end
                     else Self.SokoFile.Levels.Remove(Level__,True); // destroy the level
                     end;
     finally Leave;
     end;
     end;
end;

procedure TPlugin.Enter;
begin
  if fPluginThread<>nil then fPluginThread.Enter;
end;

function  TPlugin.GetHasIsSupportedOptimizationFunction:Boolean;
begin
  Result:=Assigned(PluginFunctions.IsSupportedOptimization);
end;

function  TPlugin.GetHasOptimizer:Boolean;
var MajorVersionNumber,MinorVersionNumber,Position:Integer; OptimizerPluginName:String;
begin
  Result:=Assigned(PluginFunctions.Optimize)
          and
          (Assigned(PluginFunctions.IsSupportedOptimization)
           or
           Assigned(PluginFunctions.GetSupportedOptimizations)
          );
  if Result then begin
     OptimizerPluginName:=PluginName;
     if StrBeginsWith(OptimizerPluginName,DEFAULT_OPTIMIZER_FILE_NAME_STUB) then begin
        MajorVersionNumber:=0; MinorVersionNumber:=0; Position:=0;
        ReadUnsignedInteger(OptimizerPluginName,Position,MajorVersionNumber);
        ReadUnsignedInteger(OptimizerPluginName,Position,MinorVersionNumber);
        if (MajorVersionNumber<=2) and (MinorVersionNumber<=89) then
           // Sokoban YASC 1.424 / YASS 2.90 introduced player lines as a new
           // secondary Sokoban game metric, just like box lines, box changes,
           // and pushing sessions;
           // some of the 'SOKOBAN_PLUGIN_FLAG_XXXX' identifiers changed values,
           // thereby breaking backwards compatibility
           Result:=False;
        end;
     end;
end;

function  TPlugin.GetHasSokoFile:Boolean;
begin
  Result:=Assigned(fSokoFile);
end;

function  TPlugin.GetHasSolver:Boolean;
begin
  Result:=Assigned(PluginFunctions.Solve) or Assigned(PluginFunctions.SolveEx);
end;

function  TPlugin.GetHasThread:Boolean;
begin
  Result:=Assigned(fPluginThread);
end;

function  TPlugin.GetIsActive:Boolean;
var CurrentThreadState:TPluginThreadStateType;
begin
  CurrentThreadState:=GetThreadState;
  Result:=(CurrentThreadState=ptsLoadLevel) or (CurrentThreadState=ptsProcessLevel);
end;

function  TPlugin.GetIsLoaded:Boolean;
begin
  Result:=DLLHandle<>0;
end;

function  TPlugin.GetIsRunningInAnotherThread:Boolean;
var s1,s2:String;
begin // only implemented for solver and optimizer plugins
  if      (Self=MainForm.Solver) or (Self=MainForm.Optimizer) then begin
          if Assigned(MainForm.Solver) then with MainForm.Solver do
             if   StrEqual(PluginFileName,DEFAULT_VALUE) then s1:=DefaultPluginFileName
             else s1:=PluginFileName
          else s1:='';
          if Assigned(MainForm.Optimizer) then with MainForm.Optimizer do
             if   StrEqual(PluginFileName,DEFAULT_VALUE) then s2:=DefaultPluginFileName
             else s2:=PluginFileName
          else s2:='';
          if   Self=MainForm.Solver then
               Result:=Assigned(MainForm.Optimizer) and (MainForm.Optimizer.Button.Tag<>Ord(pbsRun)) and StrEqual(s1,s2) and (s1<>'')
          else // Self=MainForm.Optimizer
               Result:=Assigned(MainForm.Solver   ) and (MainForm.Solver   .Button.Tag<>Ord(pbsRun)) and StrEqual(s1,s2) and (s1<>'');
          end
  else if Self=MainForm.Generator then
          Result:=False
  else Result:=Error('Plugin_.TPlugin.GetIsRunningInAnotherThread',InternalErrorText); // the function needs a major rewrite if more plugins are added later
end;

function TPlugin.GetLevel:TLevel;
begin
  Enter;
  try     Result:=fLevel;
  finally Leave;
  end;
end;

function  TPlugin.GetLevelFileName:String;
begin
  Enter;
  try     if   Assigned(fLevel) then Result:=fLevel.Name
          else Result:='';
  finally Leave;
  end;
end;

function  TPlugin.GetSokoFile:TSokoFile;
begin
  if not Assigned(fSokoFile) then // create the SokoFile (the level queue) on demand only
     try    fSokoFile:=TSokoFile.Create;
            fSokoFile.AddFileFormatDescriptionToFiles:=False;
     except on E:Exception do begin
               fSokoFile.Free; fSokoFile:=nil;
               Error(E.Message,Application.Title);
               end;
     end;
  Result:=fSokoFile;
end;

function  TPlugin.GetMovesAsText:String;
begin
  Enter;
  try     if   Assigned(Level) and
               (not    Level.SnapshotsAsText.IsEmpty) and
               (not    TSnapshotAsText(Level.SnapshotsAsText.First).MovesAsTextLines.IsEmpty) then
               Result:=TSnapshotAsText(Level.SnapshotsAsText.First).MovesAsTextLines.First.Text
          else Result:='';
  finally Leave;
  end;
end;

function  TPlugin.GetPluginName:String; // don't confuse 'PluginName' and 'PluginFileName'
const MAX_SOLVER_NAME_LENGTH=8*1024;
var Pos:Integer; CharBuffer:array[0..MAX_SOLVER_NAME_LENGTH] of Char;
begin
  Result:='';
  if   Assigned(PluginFunctions.GetPluginName) then begin
       PluginFunctions.GetPluginName(CharBuffer,MAX_SOLVER_NAME_LENGTH);
       Pos:=Low(CharBuffer);
       while (Pos<High(CharBuffer)) and (CharBuffer[Pos]<>NULL_CHAR) do Inc(Pos);
       try    SetString(Result,CharBuffer,Pos-Low(CharBuffer));
       except on E:Exception do begin
              Error(E.Message,Application.Title);
              Result:='';
              end;
       end;
       end;
end;

function  TPlugin.GetPluginResult:TPluginResult;
begin
  if   Assigned(fPluginThread) then
       Result:=fPluginThread.PluginResult
  else Result:=prUnsolved;
end;

function  TPlugin.GetPluginStatusInfoPointer:PPluginStatusInfo;
begin
  if   Assigned(fPluginThread) then
       Result:=fPluginThread.PluginStatusInfoPointer
  else Result:=nil;
end;

function  TPlugin.GetPluginThread:TPluginThread;
begin
  if not Assigned(fPluginThread) then // create the plugin-thread on demand only
     try    if   not (Self is TGenerator) then
                 fPluginThread:=TPluginThread   .Create(Self)
            else fPluginThread:=TGeneratorThread.Create(Self);
            fPluginThread.Priority:=Priority;
     except on E:Exception do begin
               fPluginThread.Free; fPluginThread:=nil;
               Error(E.Message,Application.Title);
              end;
     end;
  Result:=fPluginThread;
end;

function  TPlugin.GetPluginTimeMS:TTimeMS;
begin
  if   Assigned(fPluginThread) then
       Result:=fPluginThread.PluginTimeMS
  else Result:=0;
end;

function  TPlugin.GetSupportedOptimizations:Integer;
begin
  if   Assigned(PluginFunctions.GetSupportedOptimizations) then
       Result:=PluginFunctions.GetSupportedOptimizations
  else Result:=SOKOBAN_PLUGIN_FLAG_NONE;
end;

function  TPlugin.GetThreadState:TPluginThreadStateType;
begin
  if   Assigned(fPluginThread) then
       Result:=TPluginThreadStateType(fPluginThread.State)
  else Result:=ptsIdle;
end;

function TPlugin.IndexOf(const FileName__:String):Integer;
var s:String;
begin
  if   StrEqual(FileName__,DEFAULT_VALUE) then
       s:=DefaultPluginFileName
  else s:=FileName__;
  if Assigned(ComboBox) then with ComboBox do with Items do
     for Result:=Pred(Count) downto 0 do
         if   Objects[Result]<>nil then // 'Objects[]' contain references to the strings stored in 'StringsDB'
              if StrEqual(s,ToolsForm.StringsDB.Strings[Integer(Objects[Result])]) then exit // found: quick-and-dirty exit
              else
         else if StrEqual(s,Items[Result]) then exit; // found: quick-and-dirty exit
  Result:=-1;
end;

function TPlugin.IsSupportedOptimization(Optimization__:Cardinal):Boolean;
begin // note that this function returns 'False' if the plugin doesn't have an 'IsSupportedOptimization()' function, even though it may have the deprecated 'GetSupportedOptimizations()' function
  if   Assigned(PluginFunctions.IsSupportedOptimization) then
       Result:=PluginFunctions.IsSupportedOptimization(Optimization__)<>0
  else Result:=False;
end;

procedure TPlugin.Leave;
begin
  if fPluginThread<>nil then fPluginThread.Leave;
end;

function TPlugin.LoadLevel(BoardWidth__,BoardHeight__,LevelFlags__,SnapshotFlags__:Integer; const LevelName__,BoardAsText__:String; Snapshot__:TSnapshot; CopySnapshotNotes__,Execute__:Boolean; var NewLevel__:TLevel):Boolean;
var OldModified:Boolean; L:TLevel; NewSnapshot:TExtendedSnapshotAsText;
begin // put the level on the queue and optionally (if 'Execute__' = True) launch the plugin-thread
  Result:=False;
  if //IsLoaded and // 'IsLoaded' should not be one of the conditions for loading a level; the user may select a plugin later
     Assigned(PluginThread) and
     Assigned(SokoFile) and
     PluginThread.MakeLevel(NewLevel__) then begin
     OldModified:=fSokoFile.Modified;
     try
       Result:=NewLevel__.SetName(LevelName__) and
               (NewLevel__.BoardAsTextLines.SetSingleTextLine(BoardAsText__)<>nil);

       if   Result then begin
            // use 'Tag' to store the board dimensions
            with NewLevel__.Tag do begin
              BoardWidth :=BoardWidth__;
              BoardHeight:=BoardHeight__;
              Flags      :=[];
              end;
            NewLevel__.Flags:=LevelFlags__; // note that 'TLevel.Flags' is something else than 'TLevel.Tag.Flags'

            if Assigned(Snapshot__) then begin
               case NewLevel__.SnapshotsAsText.Count of
                 0: begin Result:=CreateObject(otExtendedSnapshotAsText,TNode(NewSnapshot));
                          if Result then NewLevel__.SnapshotsAsText.Add(NewSnapshot);
                    end;
                 1: begin Result:=NewLevel__.SnapshotsAsText.First is TExtendedSnapshotAsText;
                          NewSnapshot:=TExtendedSnapshotAsText(NewLevel__.SnapshotsAsText.First); // this is a recycled level with an existing snapshot; recycle the snapshot too (it's always a 'TExtendedSnapshotAsText', hence, the type cast is valid here)
                    end;
                 else begin
                    NewLevel__.SnapshotsAsText.Clear;
                    Result:=CreateObject(otExtendedSnapshotAsText,TNode(NewSnapshot));
                    if Result then NewLevel__.SnapshotsAsText.Add(NewSnapshot);
                    end;
               end; // case

               if   Result then begin
                    NewSnapshot.IsASolution             :=(Snapshot__.GameState=gsSolved) and (Snapshot__.MoveCount=Snapshot__.MoveTop);
                    NewSnapshot.Metrics.MoveCount       := Snapshot__.MoveCount;
                    NewSnapshot.Metrics.PushCount       := Snapshot__.PushCount;
                    NewSnapshot.Metrics.SecondaryMetrics:= Snapshot__.SecondaryScoreMetrics;
                    FillChar(NewSnapshot.SelectedRange, SizeOf(NewSnapshot.SelectedRange),0);
                    NewSnapshot.Tag                     := SnapshotFlags__;
                    if   CopySnapshotNotes__ then
                         Snapshot__.Notes.CopyTo(NewSnapshot.Notes)
                    else NewSnapshot.Notes.Clear; // if it's  a recycled level then it may contain notes
                    with Snapshot__ do
                      if (MovesAsText='') // 'MovesAsText' is calculated on demand
                         and
                         (MoveCount>0)
                         and
                         (not MovesToText(Moves,MoveCount,MoveCount,False,False,False,MovesAsText)) then MovesAsText:='';
                    Result:=((Snapshot__.MovesAsText<>'') or (Snapshot__.MoveCount=0))
                            and
                            NewSnapshot.SetText(Snapshot__.Name)
                            and
                            (NewSnapshot.MovesAsTextLines.SetSingleTextLine(Snapshot__.MovesAsText)<>nil);
                    end;
               end
            else if   NewLevel__.SnapshotsAsText.Count=1 then begin // this is a recycled level; special: keep a single snapshot for the generator, but ensure that it's empty
                      NewSnapshot:=TExtendedSnapshotAsText(NewLevel__.SnapshotsAsText.First);
                      Result:=NewSnapshot.MovesAsTextLines.SetSingleTextLine('')<>nil;
                      NewSnapshot.Notes.Clear;
                      NewSnapshot.Metrics.MoveCount       :=0;
                      NewSnapshot.Metrics.PushCount       :=0;
                      FillChar(NewSnapshot.Metrics.SecondaryMetrics,SizeOf(NewSnapshot.Metrics.SecondaryMetrics),0);
                      FillChar(NewSnapshot.SelectedRange,SizeOf(NewSnapshot.SelectedRange),0);
                      NewSnapshot.Tag                     :=0;
                      end
                 else NewLevel__.SnapshotsAsText.Clear;
            end;

       if   Result then begin
            Enter;
            try     fSokoFile.Levels.Add(NewLevel__);
                    fSokoFile.Modified:=True;
                    if Execute__ then begin
                       SetSelected(NewLevel__,True);
                       fPluginThread.State:=Ord(ptsLoadLevel);
                       Resume; // the 'Execute' method in the plugin-thread processes the levels on the queue
                       end;
            finally Leave;
            end;
            end
       else raise Exception.Create(TEXT_TASK_FAILED);
     except
       on E:Exception do begin
         L:=NewLevel__; NewLevel__:=nil; SetSelected(L,False); L.Free;
         fSokoFile.Modified:=OldModified;
         Result:=Error(E.Message,Application.Title);
         end;
     end;
     end;
end;

function  TPlugin.LoadSettingsFromIniFile(const SectionName__:String; const IniFile:TIniFile):Boolean;
var i:Integer; s:String; a:TAlternatingOptimizations; p:TPluginThreadPriority;
begin
  PluginFileName:=KeepDataPathUpToDate(KeepPluginsPathUpToDate(IniFile.ReadString(SectionName__,'FileName',PluginFileName)));
  if StrEqual(PluginFileName,DEFAULT_VALUE) or (not FileExists(PluginFileName)) then
     PluginFileName:=DefaultPluginFileName;
  s:=IniFile.ReadString(SectionName__,'Priority',ThreadPriorityText[Priority]);
  for p:=Low(p) to High(p) do
      if StrEqual(s,ThreadPriorityText[p]) then begin
         Priority:=p; break;
         end;
  if (Self is TOptimizerPlugin) or (Self is TSolverPlugin) then begin
     TimeLimitEnabled             :=IniFile.ReadBool(SectionName__,'TimeLimitEnabled',TimeLimitEnabled);
     i                            :=IniFile.ReadInteger(SectionName__,'TimeLimitSeconds',TimeLimitMS div ONE_THOUSAND);
     if (i>=0) and (i<=MAX_PLUGIN_TIME_LIMIT_MS div ONE_THOUSAND) then TimeLimitMS:=i*ONE_THOUSAND;
     AddPluginNameToSolutionTitles:=IniFile.ReadBool(SectionName__,'AddPluginNameToSolutionTitles',AddPluginNameToSolutionTitles);
     AddDateToSolutionNotes       :=IniFile.ReadBool(SectionName__,'AddDateToSolutionNotes',AddDateToSolutionNotes);
     DiscardDethronedSolutions    :=IniFile.ReadBool(SectionName__,'DiscardDethronedSolutions',DiscardDethronedSolutions);
     StatisticsEnabled            :=IniFile.ReadBool(SectionName__,'StatisticsEnabled',StatisticsEnabled);
     if (Self is TOptimizerPlugin) then begin
        SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly:=IniFile.ReadBool(SectionName__,'SeparateBestMovesAndBestSolutions:InitializeOptimizationAccordingly',SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly);
        with ToolsForm.SettingsMenuAlternatingOptimizationsEnabled  do Checked:=IniFile.ReadBool(SectionName__,'AlternatingOptimizations.Enabled',Checked);
        ToolsForm.SettingsMenuAlternatingOptimizationsEnabledClick (nil);
        s:=IniFile.ReadString(SectionName__,'AlternatingOptimizations.Type',AlternatingOptimizationsText[ToolsForm.AlternatingOptimizations]);
        for a:=Low(a) to High(a) do
            if StrEqual(s,AlternatingOptimizationsText[a]) then begin
               Toolsform.AlternatingOptimizations:=a; break;
               end;

        with ToolsForm.SettingsMenuAlternatingOptimizationsTriggerAtLeastOnce do Checked:=IniFile.ReadBool(SectionName__,'AlternatingOptimizations.TriggerAtLeastOnce' ,Checked);
        with ToolsForm.SettingsMenuAlternatingOptimizationsTriggerOnImprovements do Checked:=not ToolsForm.SettingsMenuAlternatingOptimizationsTriggerAtLeastOnce.Checked;
        with ToolsForm.SettingsMenuAlternatingOptimizationsRepeat   do Checked:=IniFile.ReadBool(SectionName__,'AlternatingOptimizations.Repeat' ,Checked);
        with ToolsForm.SettingsMenuAlternatingOptimizationsRepeatOnAnyMetrics do Checked:=IniFile.ReadBool(SectionName__,'AlternatingOptimizations.RepeatOnAnyMetrics' ,Checked);
        with ToolsForm.SettingsMenuAlternatingOptimizationsRepeatOnMovesPushesBoxLines do Checked:=not ToolsForm.SettingsMenuAlternatingOptimizationsRepeatOnAnyMetrics.Checked;
        WallifyBoxesOutsideRangeOfPushesToBeOptimized:=IniFile.ReadBool(SectionName__,'WallifyBoxesOutsideRangeOfPushesToBeOptimized',WallifyBoxesOutsideRangeOfPushesToBeOptimized);
        SubIntervalOverlapPct:=Max(0,Min(100,IniFile.ReadInteger(SectionName__,'SubIntervalOverlapPct',SubIntervalOverlapPct)));
        ToolsForm.SettingsMenuAlternatingOptimizationsRepeatClick(nil);
        end;
     end;
  Result:=(not Assigned(ComboBox)) or LoadComboBoxFromIniFile(IniFile,SectionName__,MaxInt,False,True,False,False,ComboBox);
  Add(PluginFileName);
  Add(DEFAULT_VALUE);
end;

function  TPlugin.Lookup(BoardWidth__,BoardHeight__:Integer; const BoardAsText__,MovesAsText__:String; var Level__:TLevel):Boolean;
begin // if 'MovesAsText__' is empty, then moves are not considered
  Level__:=nil;
  Enter;
  try     if HasSokoFile then Level__:=TLevel(SokoFile.Levels.First);
          while Assigned(Level__)
                and
                ((BoardWidth__             <>Level__.Tag.BoardWidth)
                 or
                 (BoardHeight__            <>Level__.Tag.BoardHeight)
                 or
                 (BoardAsText__            <>Level__.BoardAsTextLines.First.Text)
                 or
                 ((MovesAsText__           <>'')
                  and
                  (Level__.SnapshotsAsText.IsEmpty
                   or
                   (MovesAsText__          <>TExtendedSnapshotAsText(Level__.SnapshotsAsText.Last).MovesAsTextLines.First.Text)
                  )
                 )
                ) do
                Level__:=TLevel(Level__.Next);
  finally Leave;
  end;
  Result:=Assigned(Level__);
end;

function TPlugin.MoveOptimizedSolutionToSolverQueue(DiscardDethronedSolutions__:Boolean; var NewLevel__:TLevel):Integer; // the optimizer puts new solutions on the solver task queue; later the optimizer task queue imports them again using 'Tools_.TTaskQueue.ImportFromOtherPlugins'
var NextSnapshot,Snapshot:TExtendedSnapshotAsText;
begin // preconditions:
      // * the level currently being optimized by the plugin ('Plugin.Level') is locked
      // * new solutions are the first members of 'Level.SnapshotsAsText'; the last member is the original solution
  Result:=0;
  if Assigned(Level) and (ltfLocked in Level.Tag.Flags) and (Level.SnapshotsAsText.Count>=2) then begin // '>=2': the last snapshot is the original one, i.e., the source snapshot for the the optimization
     Exclude(Level.Tag.Flags,ltfSolver); // if this solution came from a solver plugin then drop it; use the new improved solution instead
     if ltfBest in Level.Tag.Flags then begin // 'True': the solution on the queue was/is a best solution
        Exclude(Level.Tag.Flags,ltfBest);
        // it would be taking it too far to calculate here whether
        // the improved snapshot is a new best solution;
        // instead the program attempts to limit the amount of confusion
        // by renaming existing best solutions to plain solutions
        // when an improvement has been found,
        // e.g., "Best solution 999/999" => "Solution 999/999"
        if Assigned(SnapshotsForm) then
           with TExtendedSnapshotAsText(Level.SnapshotsAsText.Last) do
             try    SetName(SnapshotsForm.DethronedSolutionName(Name));
             except on E:Exception do begin end;
             end;
        end;

     Snapshot:=TExtendedSnapshotAsText(Level.SnapshotsAsText.First);
     repeat
       NextSnapshot:=TExtendedSnapshotAsText(Snapshot.Next);
       if Snapshot.OptimizationFlags=0 then begin // '0': the optimization flags hasn't been filled in already; inherit optimization settings
          Snapshot.Tag:=TExtendedSnapshotAsText(Snapshot.Last).Tag; // inherit optimization settings
          Snapshot.OptimizationFlags:=TExtendedSnapshotAsText(Snapshot.Last).OptimizationFlags; // inherit optimization settings
          end;
       if Snapshot.IsASolution  then begin // 'True': 'ReplayGame' has confirmed that it's a solution; 'False': the solution status is undefined
          NewLevel__:=nil;
          if CreateObject(otLevel,TNode(NewLevel__)) then begin
             {make a clone of the level and transfer the newly found optimization to the clone}
             if   NewLevel__.SetName(Level.Name) and
                  Assigned(NewLevel__.BoardAsTextLines.AddTextLine(Level.BoardAsTextLines.First.Text,True)) and
                  Assigned(MainForm.Solver) and
                  Assigned(MainForm.Solver.SokoFile) and
                  Assigned(MainForm.Solver.PluginThread) then begin
                  NewLevel__.Tag:=Level.Tag;  {get board dimensions and flags from the original level}
                  Include(NewLevel__.Tag.Flags,ltfNew); {mark it as a new item}
                  Include(NewLevel__.Tag.Flags,ltfProcessed); {mark the new item as 'processed', i.e., that the solution is ready for import by the optimizer task queue}
                  Exclude(NewLevel__.Tag.Flags,ltfLocked); {remove the locked flag from the new item}
                  Exclude(NewLevel__.Tag.Flags,ltfSelected); {so the new task isn't selected for processing on the solver queue, in which case 'ImportFromOtherPlugins()' doesn't import it}
                  if DiscardDethronedSolutions__ then
                     with TExtendedSnapshotAsText(Level.SnapshotsAsText.Last) do
                       {if the new solution is better/equal, counting moves as well as pushes,}
                       {then discard the old solution if it's the result of a previous optimization in this session}
                       if //(Snapshot.Metrics.MoveCount<=Metrics.MoveCount) and
                          //(Snapshot.Metrics.PushCount<=Metrics.PushCount) and
                          (CompareMetrics(gmMoves,gmPushes,MainForm.Game.PathFindingOptimizeMoves,Snapshot.Metrics,Metrics)<=0)
                          and
                          (CompareMetrics(gmPushes,gmMoves,MainForm.Game.PathFindingOptimizeMoves,Snapshot.Metrics,Metrics)<=0)
                          then
                          Exclude(Level.Tag.Flags,ltfNew); {update the flags for the original level}
                  NewLevel__.Flags:=-1;

                  with PluginThread do Synchronize(SynchronizedSavePluginResultToLogFile); {update the log file before the solution is transferred to the new level}

                  Level.SnapshotsAsText.Remove(Snapshot,False);
                  NewLevel__.SnapshotsAsText.Push(Snapshot); {transfer the new solution to the cloned level}

                  if   Assigned(MainForm.Solver) then begin
                       {put the new solution on the solver queue}
                       {later, the optimizer window (in the main thread) imports solutions and adds them to the task queue}
                       MainForm.Solver.Enter;
                       try     MainForm.Solver.SokoFile.Levels.Add(NewLevel__);
                               MainForm.Solver.PendingResultsCount:=Succ(MainForm.Solver.PendingResultsCount);
                               Inc(Result);
                       finally MainForm.Solver.Leave;
                       end;
                       end
                  else begin NewLevel__.Free; NewLevel__:=nil; {drop the level; the program is probably exiting, and the easy way out is to drop the current optimization}
                       end;

                  if   ToolsForm.Visible and (ToolsForm.PageControl1.ActivePage=ToolsForm.TabsheetOptimizer) and (Result=1) then
                       PostMessage(ToolsForm.Handle,MSG_OPTIMIZER_REFRESH,MSG_PLUGIN_REFRESH_IMPORT,Cardinal(Self));
                  end
             else begin NewLevel__.Free; NewLevel__:=nil;
                        Include(Level.Tag.Flags,ltfNew); // ensure that the snapshot is imported by 'ImportGames';
                        PendingResultsCount:=Succ(PendingResultsCount);
                  end;
             end
          else begin
             Include(Level.Tag.Flags,ltfNew); // ensure that the snapshot is imported by 'ImportGames';
             PendingResultsCount:=Succ(PendingResultsCount);
             end;
          end
       else begin
          // i.e., 'Snapshot.IsASolution=False';
          // this doesn't necessarily mean that the snapshot isn't a solution,
          // only that 'ReplayGame' hasn't calculated the solution status
          Include(Level.Tag.Flags,ltfNew); // ensure that the snapshot is imported by 'ImportGames';
          PendingResultsCount:=Succ(PendingResultsCount);
          end;

       Snapshot:=NextSnapshot;
     until (not Assigned(Snapshot)) or (not Assigned(Snapshot.Next)); // 'Snapshot.Next': the original solution is the last one on the list
     end;
end;

function  TPlugin.Open(const PluginFileName__:String):Boolean;
var MaxWidth,MaxHeight,MaxBoxCount:Integer;
    f:TPluginFunctionType; s:String; Level:TLevel; Node:TNode;
begin
  Close;
  s:=DefaultPluginFileName;
  if   StrEqual(PluginFileName__,s) then
       fPluginFileName:=DEFAULT_VALUE
  else fPluginFileName:=PluginFileName__;
  fPluginLimitsAsText:=''; fHasPluginTypeInName:=False; fIsDefaultPlugin:=False;

  try    if not StrEqual(PluginFileName,DEFAULT_VALUE) then s:=PluginFileName;
         if s<>'' then DLLHandle:=LoadLibrary(PChar(s));
  except on E:Exception do DLLHandle:=0;
  end;

  Result:=DLLHandle<>0;
  if Result then begin
     for f:=Low(f) to High(f) do with fPluginFunctions do begin
         PluginFunction[f]:=GetProcAddress(DLLHandle,PChar(PLUGIN_FUNCTION_NAMES[f]));
         if   not Assigned(PluginFunction[f]) then begin
              if f=pfPluginName then
                 PluginFunction[f]:=GetProcAddress(DLLHandle,PChar(GET_SOLVER_NAME_FUNCTION_NAME)); {backwards compatibility; early versions of the plugin interface only supported solvers and used the function name 'GetSolverName'}
              if (not Assigned(PluginFunction[f])) and
                 (f<pfSolve) then // '<pfSolve': all basic functions like 'GetPluginName' must be present in the DLL file}
                 Result:=False;
              end;
         end;

     s:=AnsiLowerCase(PluginName);
     if        Self is TSolverPlugin then begin
               Result:=Result and HasSolver;
               fHasPluginTypeInName:=AnsiPos(AnsiLowerCase(SolverText),s)<>0;
               if Result then
                  fIsDefaultPlugin:=StrBeginsWith(ExtractFileName(PluginName),DEFAULT_SOLVER_FILE_NAME_STUB);
               end
     else if   Self is TOptimizerPlugin then begin
               Result:=Result and HasOptimizer;
               fHasPluginTypeInName:=AnsiPos(AnsiLowerCase(OptimizerText),s)<>0;
               if Result then
                  fIsDefaultPlugin:=StrBeginsWith(ExtractFileName(PluginName),DEFAULT_SOLVER_FILE_NAME_STUB) or // the default YASS solver is a combined solver and optimizer very similar to the default YASO optimizer
                                    StrBeginsWith(ExtractFileName(PluginName),DEFAULT_OPTIMIZER_FILE_NAME_STUB);
               if Result and HasSokoFile then begin
                  Enter;
                  try
                    Level:=TLevel(SokoFile.Levels.First);
                    while Assigned(Level) do with Level do begin
                      if Assigned(SnapshotsAsText) and (not SnapshotsAsText.IsEmpty) then begin
                         Node:=SnapshotsAsText.First;
                         while Assigned(Node) do begin
                           if (Node is TExtendedSnapshotAsText) and (not (ltfLocked in Level.Tag.Flags)) then
                              with Node as TExtendedSnapshotAsText do begin
                                OptimizationFlags:=0; // '0': the field is first filled in when the optimization starts
                                end;
                           Node:=Node.Next;
                           end;
                         end;
                      Level:=TLevel(Next);
                      end;
                  finally Leave;
                  end;
                  end;
               end
          else Result:=False; // undefined plugin type

     if Result and Assigned(PluginFunctions.GetConstraints) then begin
        PluginFunctions.GetConstraints(Cardinal(MaxWidth),Cardinal(MaxHeight),Cardinal(MaxBoxCount));
        if (MaxWidth>0) and (MaxHeight>0)  then
           fPluginLimitsAsText:=BoardText+COLON+SPACE+Misc_.BoardDimensionsAsText(MaxWidth,MaxHeight,MainForm.BoardDimensionsAsText);
        if MaxBoxCount>0 then begin
           if fPluginLimitsAsText<>'' then fPluginLimitsAsText:=fPluginLimitsAsText+PERIOD+SPACE;
           fPluginLimitsAsText:=fPluginLimitsAsText+BoxesText+COLON+SPACE+IntToStr(MaxBoxCount);
           end;
        end;
     if not Result then Close;
     end;
end;

function  TPlugin.Optimize(BoardWidth__,BoardHeight__:Integer;
                           BoardAsText__:PChar;
                           var MovesAsTextBuffer__:TMovesAsTextBuffer;
                           var TimeMS__:TTimeMS;
                           var PluginStatusInfoPointer__:PPluginStatusInfo):TPluginResult;
var i:Integer;
begin // this is the function which the optimizer plugin-thread calls
  Result:=prFailed; fTerminatedByUser:=False; TimeMS__:=0;
  if   (BoardWidth__>=MIN_BOARD_WIDTH) and (BoardHeight__>=MIN_BOARD_HEIGHT) and
       (BoardWidth__<=MAX_BOARD_WIDTH) and (BoardHeight__<=MAX_BOARD_HEIGHT) and
       (StrLen(BoardAsText__)=Cardinal(BoardWidth__*BoardHeight__)) then begin
       if   Assigned(PluginThread) then begin // note: 'PluginThread' and not 'fPluginThread'; this creates the thread on demand only
            //FillChar(MovesAsTextBuffer__,SizeOf(MovesAsTextBuffer__),0); // this shouldn't be necessary, the plugin is required to add a null-terminator after the moves
            if      Assigned(PluginFunctions.Optimize) then begin
                    StartTimer;
                    i:=PluginFunctions.Optimize(BoardWidth__,BoardHeight__,
                                                BoardAsText__,
                                                MovesAsTextBuffer__,
                                                SizeOf(MovesAsTextBuffer__),
                                                PluginStatusInfoPointer__,
                                                Plugin_.OptimizerPluginCallBackFunction);
                    TimeMS__:=CalculateElapsedTimeMS(fStartTimeMS,GetTimeMS);
                    if (i>=Ord(Low (TPluginResult))) and
                       (i<=Ord(High(TPluginResult))) then Result:=TPluginResult(i);
                    end;
            if (Result=prUnsolved) or (Result=prFailed) or (Result=prTerminatedByUser) then
               if      TerminatedByUser then
                       Result:=prTerminatedByUser
               else if Assigned(PluginThread) and (PluginThread.PluginCallBackFunctionResult=prTimeOut) then
                       Result:=prTimeOut;
            end;
       end
  else Result:=prInvalidLevel;
end;

function TPlugin.Remove(Index__:Integer):Boolean;
begin
  with ComboBox do with Items do begin
    Result:=(Index__>=0) and (Index__<Count);
    if Result then begin
       if Objects[Index__]<>nil then ToolsForm.StringsDB.Delete(Integer(Objects[Index__])); // remove the plugin filename from the 'StringsDB' string collection
       if Index__=ItemIndex then begin
          Delete(Index__);
          if   Count=0 then ItemIndex:=-1
          else ItemIndex:=Min(Index__,Pred(Count));
          end
       else
          Delete(Index__);
       end;
    end;
end;

function  TPlugin.RenameLevels(const OldFileName__,NewFileName__:String):Integer;
var IsAnIniFileSectionFileName:Boolean; Node:TNode;
begin // note that items on the level-queue don't necessarily have unique names
  Result:=0;
  if HasSokoFile then begin
     IsAnIniFileSectionFileName:=Pack_.IsAnIniFileSectionFileName(OldFileName__) or
                                 Pack_.IsAnIniFileSectionFileName(NewFileName__);
     Enter;
     try     Node:=SokoFile.Levels.First;
             while Node<>nil do begin
               if      StrEqual(OldFileName__,Node.Text) and
                       Node.SetText(NewFileName__) then
                       Inc(Result)  // rename OLDFILE => NEWFILE
               else if (not IsAnIniFileSectionFileName) and
                       StrEqual(OldFileName__,ExtractIniFileName(Node.Text)) and
                       Node.SetText(MakeIniFileSectionFileName(NewFileName__,ExtractSectionName(Node.Text))) then
                       Inc(Result); // rename OLDFILE\LevelName => NEWFILE\LevelName
               Node:=Node.Next;
               end;
             if Self is TOptimizerPlugin then ToolsForm.OptimizerTaskQueue.Refresh(False);
     finally Leave;
     end;
     end;
end;

function TPlugin.RenamePlugins(const OldFileName__,NewFileName__:String):Integer;
var Index,OldIndex,NewIndex:Integer;
begin
  Result:=0;
  OldIndex:=ToolsForm.StringsDB.IndexOf(OldFileName__);
  if (OldIndex>0) and Assigned(ComboBox) then with ComboBox do with Items do begin // '>0': the old filename is a member of the string collection, hence, there is a chance that a plugin has this name
     NewIndex:=-1;
     for Index:=Pred(Count) downto 0 do
         if Integer(Objects[Index])=OldIndex then begin
            ToolsForm.StringsDB.Delete(OldIndex);
            if   NewIndex< 0 then
                 NewIndex:=ToolsForm.StringsDB.AddCI(NewFileName__);
            if   NewIndex>0 then begin
                 Objects[Index]:=TObject(NewIndex);
                 Inc(Result);
                 end
            else Delete(Index);
            end;
     end;
end;

procedure TPlugin.Resume;
begin
  if Assigned(fPluginThread) then fPluginThread.Resume;
end;

function  TPlugin.SaveSettingsToIniFile(const SectionName__:String; const IniFile:TIniFile):Boolean;
var i:Integer;
begin
  Result:=True;
  // note that the items in the combobox are destroyed, hence, only call this procedure during shutdown
  // also note that the combobox items must be saved first because 'SaveComboBoxToIniFile' erases the whole section in the inifile
  if Assigned(ComboBox) then with ComboBox do with Items do begin
     i:=IndexOf(DefaultPluginFileName);
     if i>=0 then Delete(i);
     Sorted:=False;
     for i:=0 to Pred(Count) do
         if Objects[i]<>nil then Items[i]:=ToolsForm.StringsDB.Strings[Integer(Objects[i])]; // change the items from short names to filenames
     Result:=SaveComboBoxToIniFile(IniFile,SectionName__,MaxInt,ComboBox);
     end;

  if   StrEqual(PluginFileName,DefaultPluginFileName) then
       IniFile.WriteString(SectionName__,'FileName',DEFAULT_VALUE)
  else IniFile.WriteString(SectionName__,'FileName',PluginFileName);
  IniFile.WriteString(SectionName__,'Priority',ThreadPriorityText[Priority]);
  if (Self is TOptimizerPlugin) or (Self is TSolverPlugin) then begin
     IniFile.WriteBool   (SectionName__,'TimeLimitEnabled',TimeLimitEnabled);
     IniFile.WriteInteger(SectionName__,'TimeLimitSeconds',TimeLimitMS div ONE_THOUSAND);
     IniFile.WriteBool   (SectionName__,'AddPluginNameToSolutionTitles',AddPluginNameToSolutionTitles);
     IniFile.WriteBool   (SectionName__,'AddDateToSolutionNotes',AddDateToSolutionNotes);
     IniFile.WriteBool   (SectionName__,'DiscardDethronedSolutions',DiscardDethronedSolutions);
     IniFile.WriteBool   (SectionName__,'StatisticsEnabled',StatisticsEnabled);
     if (Self is TOptimizerPlugin) then begin
        IniFile.WriteBool(SectionName__,'SeparateBestMovesAndBestSolutions:InitializeOptimizationAccordingly',SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly);
        with ToolsForm.SettingsMenuAlternatingOptimizationsEnabled  do IniFile.WriteBool(SectionName__,'AlternatingOptimizations.Enabled',Checked);
        IniFile.WriteString(SectionName__,'AlternatingOptimizations.Type',AlternatingOptimizationsText[ToolsForm.AlternatingOptimizations]);
        with ToolsForm.SettingsMenuAlternatingOptimizationsTriggerAtLeastOnce do IniFile.WriteBool(SectionName__,'AlternatingOptimizations.TriggerAtLeastOnce',Checked);
        with ToolsForm.SettingsMenuAlternatingOptimizationsRepeat   do IniFile.WriteBool(SectionName__,'AlternatingOptimizations.Repeat',Checked);
        with ToolsForm.SettingsMenuAlternatingOptimizationsRepeatOnAnyMetrics do IniFile.WriteBool(SectionName__,'AlternatingOptimizations.RepeatOnAnyMetrics',Checked);
        IniFile.WriteBool(SectionName__,'WallifyBoxesOutsideRangeOfPushesToBeOptimized',WallifyBoxesOutsideRangeOfPushesToBeOptimized);
        IniFile.WriteInteger(SectionName__,'SubIntervalOverlapPct',SubIntervalOverlapPct);
        end;
     end;
end;

procedure TPlugin.SetLevel(Level__:TLevel);
begin
  Enter;
  try     fLevel:=Level__;
  finally Leave;
  end;
end;

procedure TPlugin.SetPriority(ThreadPriority__:TThreadPriority);
begin
  fPriority:=ThreadPriority__;
  if fPluginThread<>nil then fPluginThread.Priority:=fPriority;
end;

procedure TPlugin.SetSelected(Level__:TLevel; Selected__:Boolean);
begin
  Enter;
  try     if Assigned(Level__) then with Level__.Tag do
             if   Selected__ then
                  if not (ltfSelected in Flags) then begin
                     Include(Flags,ltfSelected);
                     Inc(fSelectedLevelsCount);
                     end
                  else
             else if ltfSelected in Flags then begin
                     Exclude(Flags,ltfSelected);
                     Dec(fSelectedLevelsCount);
                     end;
  finally Leave;
  end;
end;

procedure TPlugin.SetThreadState(ThreadState__:TPluginThreadStateType);
begin
  if Assigned(PluginThread) then PluginThread.State:=Integer(ThreadState__);
end;

function  TPlugin.Settings(WindowHandle__:HWND):Boolean;
const DEFAULT_PLUGIN_SETTINGS_TABSHEET_CAPTIONS:array[Boolean] of String=('Solver','Optimizer'); // must match declarations in 'YASSdllSettings.pas'
var s:String; IniFile:TIniFile;
begin
  Result:=Assigned(PluginFunctions.Settings);
  if Result then begin
     if   PluginFileName=DEFAULT_VALUE then s:=DefaultPluginFileName
     else s:=PluginFileName;
     if StrEqual(ExtractFileNameWithoutPathAndExtension(s),DefaultPluginFileNameStub) then begin
        // the YASS optimizer shares its settings window with the YASS solver,
        // hence, inform the settings window which set of settings to show first (they each have their own tabsheet)
        //s:=ChangeFileExt(s,INI_FILE_EXT);
        s:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath)+DEFAULT_PLUGINS_DIRECTORY)+
           ExtractFileName(ChangeFileExt(s,INI_FILE_EXT));
        try    ForceDirectories(StrWithoutTrailingPathDelimiter(ExtractFilePath(s)));
               IniFile:=TIniFile.Create(s);
               try      IniFile.WriteString(ExtractFileNameWithoutPathAndExtension(s),'TabSheet',DEFAULT_PLUGIN_SETTINGS_TABSHEET_CAPTIONS[Self is TOptimizerPlugin]);
               finally  IniFile.Free;
               end;
        except on E:Exception do;
        end;
        end;

     Enter;
     try     try    PluginFunctions.Settings(WindowHandle__);
             except on E:Exception do begin end;
             end;
     finally Leave;
     end;
     end;
end;

procedure TPlugin.SetPluginFileName(const PluginFileName__:String);
begin
  if (PluginFileName__<>PluginFileName) and
     (not ((PluginFileName__=DEFAULT_VALUE) and (PluginFileName=DefaultPluginFileName))) then begin
     Close;
     fPluginFileName:=PluginFileName__;
     end;
end;

function TPlugin.ShowAbout(WindowHandle__:HWND):Boolean;
begin
  Enter;
  try     Result:=Assigned(PluginFunctions.ShowAbout);
          if Result then
             try    PluginFunctions.ShowAbout(WindowHandle__);
             except on E:Exception do begin end;
             end;
  finally Leave;
  end;
end;

function  TPlugin.Solve(BoardWidth__,BoardHeight__:Integer;
                        BoardAsText__:PChar;
                        var SolutionAsTextBuffer__:TMovesAsTextBuffer;
                        var TimeMS__:TTimeMS;
                        var PluginStatusInfoPointer__:PPluginStatusInfo):TPluginResult; {'var': it's cleared if the dll doesn't have 'SolveEx' but only a 'Solve' command}
var i,Index,Row:Integer; FirstCharInRow:array[0..MAX_BOARD_HEIGHT] of PChar;
begin // this is the function which the solver plugin-thread calls
  Result:=prFailed; fTerminatedByUser:=False; TimeMS__:=0;
  if   (BoardWidth__>=MIN_BOARD_WIDTH) and (BoardHeight__>=MIN_BOARD_HEIGHT) and
       (BoardWidth__<=MAX_BOARD_WIDTH) and (BoardHeight__<=MAX_BOARD_HEIGHT) and
       (StrLen(BoardAsText__)=Cardinal(BoardWidth__*BoardHeight__)) then begin
       if   Assigned(PluginThread) then begin // note: 'PluginThread' and not 'fPluginThread'; this creates the thread on demand only
            //FillChar(SolutionAsTextBuffer__,SizeOf(SolutionAsTextBuffer__),0); // this shouldn't be necessary, the plugin is required to add a null-terminator to the solution
            if      Assigned(PluginFunctions.SolveEx) then begin
                    StartTimer;
                    i:=PluginFunctions.SolveEx(BoardWidth__,BoardHeight__,
                                               BoardAsText__,
                                               SolutionAsTextBuffer__,
                                               SizeOf(SolutionAsTextBuffer__),
                                               PluginStatusInfoPointer__,
                                               Plugin_.SolverPluginCallBackFunction);
                    TimeMS__:=CalculateElapsedTimeMS(fStartTimeMS,GetTimeMS);
                    if (i>=Ord(Low (TPluginResult))) and
                       (i<=Ord(High(TPluginResult))) then Result:=TPluginResult(i);
                    end
            else if Assigned(PluginFunctions.Solve) then begin
                    PluginStatusInfoPointer__:=nil;
                    Index:=0;
                    for Row:=1 to BoardHeight__ do begin
                        FirstCharInRow[Row]:=Addr(BoardAsText__[Index]);
                        Inc(Index,BoardWidth__);
                        end;
                    StartTimer;
                    i:=PluginFunctions.Solve(BoardWidth__,BoardHeight__,
                                              Addr(FirstCharInRow[1]),
                                              SolutionAsTextBuffer__,
                                              SizeOf(SolutionAsTextBuffer__));
                    TimeMS__:=CalculateElapsedTimeMS(fStartTimeMS,GetTimeMS);
                    if (i>=Ord(Low (TPluginResult))) and
                       (i<=Ord(High(TPluginResult))) then Result:=TPluginResult(i);
                    end;
            if (Result=prUnsolved) or (Result=prFailed) or (Result=prTerminatedByUser) then
               if      TerminatedByUser then
                       Result:=prTerminatedByUser
               else if Assigned(PluginThread) and (PluginThread.PluginCallBackFunctionResult=prTimeOut) then
                       Result:=prTimeOut;
            end;
       end
  else Result:=prInvalidLevel;
end;

procedure TPlugin.StartTimer;
begin
  repeat fStartTimeMS:=GetTimeMS;
  until  fStartTimeMS<>0; // '0' is a reserved value meaning 'inactive'
end;

function  TPlugin.StopTimer:TTimeMS;
begin // resets the start time to '0' which is a reserved value meaning 'inactive'
  if   StartTimeMS<>0 then begin
       Result:=CalculateElapsedTimeMS(StartTimeMS,GetTimeMS);
       fStartTimeMS:=0;
       end
  else Result:=0;
end;

procedure TPlugin.Suspend;
begin
  if Assigned(fPluginThread) then fPluginThread.Suspend;
end;

function  TPlugin.Terminate:Boolean;
begin
  if not (Self is TGenerator) then
     Result:=IsLoaded and
             Assigned(fPluginFunctions.Terminate) and
             (PluginFunctions.Terminate=Ord(prOK))
  else begin // quick and dirty special-case code for the generator instead of writing a separate "Terminate()" function for the generator class
     Result:=True;
     if HasThread then PluginThread.State:=Ord(ptsIdle);
     end;
  if Result then fTerminatedByUser:=True;
end;

function TPlugin.ImportGame(Level__:TLevel):Boolean;
var i,Count,OldSnapshotsCount:Integer;
    s:String;
    OldVerbose:Boolean; OldOpenFormGameFileName:String; OldDeadlocks:TDeadlocks;
    DestGame,TempGame:TGame;
    OldBestSolutionMoves,OldBestSolutionPushes:TSnapshot;
    Snapshot:TSnapshot; SnapshotAsText:TSnapshotAsText; Node:TNode;

  function LoadLevel(Level__:TLevel; TempGame__:TGame; var DestGame__:TGame):Boolean;
  var b:Boolean;
  begin
    TempGame__.Clear;
    if        StrEqual(Level__.Name,MainForm.Game.FileName) and (MainForm.Game.GameState<>gsNull) then begin
              DestGame__:=MainForm.Game;
              Result:=True;
              end
    else if   not IsANewFileName(Level__.Name) then begin
              DestGame__:=TempGame__;
              Result:=DestGame__.LoadFromFileOrClipboard(Level__.Name,nil,nil,b);
              end
         else Result:=False; // there is no receiver for a solution to a new level
  end; // LoadLevel

begin {ImportGame;
       preconditions: 1. 'MainForm.Game' is in a stable and inactive state;
                      2. 'MainForm.SokoFile', 'OpenForm.Game', and 'ToolsForm.Game' are free to use;
                      3. the game to be imported is the first snapshot on the list of snapshots;
                      4. the game to be imported is a solution;
      }
  Result:=False;
  if Assigned(Level__) and
     (Level__.Name<>'') and
     (not Level__.SnapshotsAsText.IsEmpty) and
     Assigned(MainForm.Game) and
     (not MainForm.Game.IsLoading) and
     Assigned(OpenForm) and
     Assigned(OpenForm.Game) and
     Assigned(ToolsForm) and
     Assigned(ToolsForm.Game) and
     Assigned(SnapshotsForm) then begin
     MainForm.Game.StopTimer;
     OldOpenFormGameFileName:=OpenForm.Game.FileName;
     OldDeadlocks           :=MainForm.Game.DeadlockDetection.Deadlocks;
     OldVerbose             :=MainForm.Game.Verbose;
     Enter;
     try
             if Assigned(MainForm.Deadlocks) then MainForm.Deadlocks.Suspend;

             OpenForm.Game.FileName:=''; // don't give solutions to 'OpenForm.Game'
             MainForm.Game.Verbose:=False;
             MainForm.Game.DeadlockDetection.Deadlocks:=nil;

             TempGame:=OpenForm.Game;
             TempGame.Verbose:=False;
             TempGame.DeadlockDetection.Deadlocks:=nil;
             TempGame.SecondaryMetricsInTitles:=MainForm.Game.SecondaryMetricsInTitles;

             SnapshotAsText:=TSnapshotAsText(Level__.SnapshotsAsText.First);

             try    if LoadLevel(Level__,TempGame,DestGame) and
                       SnapshotAsText.TextLinesToMoves(TempGame.History,TempGame.ReverseMode) then begin
                       DestGame.IsLoading   :=True; // so dethroned solutions are saved as snapshots
                       OldBestSolutionMoves :=DestGame.BestSolutionMoves;
                       OldBestSolutionPushes:=DestGame.BestSolutionPushes;
                       if DestGame=TempGame then with DestGame do
                          DoBoardTransformation2D(BOARD_TRANSFORMATION_INVERSE[BoardTransformation2D],False);
                       OldSnapshotsCount:=DestGame.Snapshots.Count;

                       if ToolsForm.CheckSnapshots(DestGame,TempGame,True,Count) then begin
                          if Count=0 then with SnapshotAsText do begin // the plugin didn't produce a solution
                             SetName(StrSubstitute(StrSubstitute(Name,
                                                                 SnapshotsForm.SolutionName,
                                                                 SnapshotsForm.NormalModeSnapshotName,
                                                                 i),
                                                   TEXT_SOLUTION,
                                                   SnapshotsForm.NormalModeSnapshotName,
                                                   i));
                             end;
                          if Count=1 then begin // the plugin produced a solution
                             if      OldBestSolutionMoves <>DestGame.BestSolutionMoves then begin
                                     Snapshot:=DestGame.BestSolutionMoves;
                                     end
                             else if OldBestSolutionPushes<>DestGame.BestSolutionPushes then begin
                                     Snapshot:=DestGame.BestSolutionPushes;
                                     end
                                  else begin
                                     TempGame.RenumberCombinedMoves; {optimize the combined move grouping}
                                     TempGame.IsLoading:=False;
                                     TempGame.GameState:=gsSolved;
                                     {caution:}
                                     {when 'DestGame' <> 'TempGame' then the game state}
                                     {of 'TempGame' isn't fully synchronized with its}
                                     {history at this time; therefore, if the snapshot-record}
                                     {is modified at some time, it may be necessary to}
                                     {do more initialization here}
                                     Snapshot:=TempGame.MakeSnapshot(SnapshotsForm.SolutionName);
                                     if Assigned(Snapshot) then begin
                                        DestGame.Snapshots.Add(Snapshot);
                                        Count:=0; {signals that the new solution wasn't a new best solution}
                                        end;
                                     end;

                             if Assigned(Snapshot) then begin
                                if Assigned(OpenForm) then
                                   if      OpenForm.Visible
                                           and
                                           (((OpenForm.Task=otSolver   ) and (Self is TSolverPlugin))
                                            or
                                            ((OpenForm.Task=otOptimizer) and (Self is TOptimizerPlugin))
                                           ) then begin
                                            OpenForm.ShowSolverMovesAndPushes(OpenForm.PluginLevelStringGrid,Snapshot.MoveCount,Snapshot.PushCount);
                                           end
                                   else if Assigned(ToolsForm)
                                           and
                                           (((ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetSolver   ) and (Self is TSolverPlugin))
                                            or
                                            ((ToolsForm.PageControl1.ActivePage=ToolsForm.TabSheetOptimizer) and (Self is TOptimizerPlugin))
                                           ) then begin
                                           OpenForm.ShowSolverMovesAndPushes(ToolsForm.PluginLevelStringGrid,Snapshot.MoveCount,Snapshot.PushCount);
                                           end;
                                if DestGame=TempGame then with DestGame do
                                   DoBoardTransformation2D(OriginalBoardTransformation2D,False);

                                if Snapshot.SetName('') then begin
                                   s:=ExtractTextInParenthesis(SnapshotAsText.Name);
                                   if s<>'' then s:=SPACE+StrWithParenthesis(s);
                                   s:=SnapshotsForm.SolutionName+SPACE+
                                      Format(FORMAT_MOVES_AND_PUSHES,[Snapshot.MoveCount,Snapshot.PushCount])+
                                      SecondaryMetricsFormattedAsATitleSuffix(
                                        DestGame.SecondaryMetricsInTitles,
                                        Snapshot.SecondaryScoreMetrics)+
                                      s;
                                   s:=DestGame.Snapshots.MakeUniqueName(s,SnapshotsForm.SolutionName,True);
                                   if s='' then s:=SnapshotAsText.Name;
                                   Snapshot.Notes.Lines.Swap(SnapshotAsText.Notes.Lines);
                                   if Snapshot.Notes.Lines.FindKey(MovesText,Node) then
                                      Snapshot.Notes.Lines.Remove(Node,True);
                                   if Snapshot.Notes.Lines.FindKey(PushesText,Node) then
                                      Snapshot.Notes.Lines.Remove(Node,True);
                                   if Snapshot.Notes.Lines.FindKey(KEY_SELECTED_SQUARES,Node) then
                                      Snapshot.Notes.Lines.Remove(Node,True);

                                   Snapshot.Modified:=True;
                                   Result:=Snapshot.SetName(s);

                                   if Result then begin
                                      if DestGame=MainForm.Game then begin
                                         if Count=0 then begin {'True': the new solution wasn't a new best solution; instead it was saved as a normal snapshot}
                                            SnapshotsForm.PanelNewClick(Self);
                                            if      SnapshotsForm.Visible then with SnapshotsForm do begin
                                                    if WindowState=wsMinimized then WindowState:=wsNormal;
                                                    end
                                            else if Screen.ActiveForm=MainForm then
                                                    MainForm.ShowSnapshots(True,False,SnapshotsForm.Grid.Row);
                                            MainForm.MakeDynamicHints(nil);
                                            end
                                         else begin {the new solution was a new best solution}
                                            MainForm.Game.UpdateBestSolutionNames;
                                            with SnapshotsForm do begin
                                              LoadSnapshots(Snapshots[Grid.Row]);
                                              if OldSnapshotsCount<>MainForm.Game.Snapshots.Count then
                                                 ShowOnStartUp:=True;
                                              end;
                                            MainForm.MakeDynamicHints(nil);
                                            MainForm.ShowStatus;
                                            MainForm.Status.Hint:=Snapshot.Name;
                                            end;
                                         end
                                      else begin // this is not the level currently loaded in the main window; save the level to disk
                                         with TempGame.History do begin
                                           Count:=0; Top:=0;
                                           end;
                                         Result:=DestGame.SaveToFile(DestGame.FileName,False); // 'False': don't flush the file to disk; there may be a burst of small fast tasks on the queue, and if it's a big file then flushing will steal too much running time
                                         if Result then
                                            MainForm.Status.Hint:=VisualFileName(Level__.Name)+': '+Snapshot.Name;
                                         end;
                                      end;
                                   end;
                                end;
                             end;
                          end;
                       end;
             except on E:Exception do Result:=Error(E.Message,Application.Title);
             end;

     finally Leave;
             MainForm.Game.DeadlockDetection.Deadlocks:=OldDeadlocks;
             MainForm.Game.Verbose:=OldVerbose;
             MainForm.Game.IsLoading:=False;
             if Assigned(MainForm.Deadlocks) then begin
                MainForm.Deadlocks.LoadBoard;
                MainForm.Deadlocks.Resume;
                end;
             OpenForm .Game.FileName :=OldOpenFormGameFileName;
             OpenForm .Game.IsLoading:=False;
             ToolsForm.Game.IsLoading:=False;
     end;
     end;
end;

function TPlugin.ImportGames(DeleteImportedLevels__:Boolean):Integer;
var oCursor:TCursor; CurrentLevel,NextLevel:TLevel;

  function  IsLevelRelatedToAlternatingOptimizationsInProgress(Level__:TLevel):Boolean;
  var AlternatingOptimizationLevel:TLevel;
  begin  // returns 'True' if the level board matches the board for the alternating optimizations in progress, if any;
         // preconditions:
         // * the plugin is an optimizer plugin;
         // * Level__' is a member of the task queue for the plugin;
    Result:=False;
    if (Self is TOptimizerPlugin) and
       Assigned(ToolsForm) and
       Assigned(ToolsForm.OptimizerTaskQueue) and
       (ToolsForm.OptimizerTaskQueue.AlternateOptimization.CurrentTaskSerialNo<>0) and
       Assigned(Level__) and
       Assigned(Level__.BoardAsTextLines) and
       (Level__.BoardAsTextLines.Count=1) then begin
       AlternatingOptimizationLevel:=fSokoFile.GetLevelByExtendedSnapshotAsTextSerialNo(ToolsForm.OptimizerTaskQueue.AlternateOptimization.CurrentTaskSerialNo);
       if Assigned(AlternatingOptimizationLevel) then with AlternatingOptimizationLevel do
          Result:=(Tag.BoardWidth=Level__.Tag.BoardWidth)
                  and
                  (Tag.BoardHeight=Level__.Tag.BoardHeight)
                  and
                  Assigned(BoardAsTextLines)
                  and
                  (BoardAsTextLines.Count=1)
                  and
                  (BoardAsTextLines.First.Text=Level__.BoardAsTextLines.First.Text); // 'True': matching boards
       end;
  end;

begin // 'ImportGames' preconditions: see 'TPlugin.ImportGame';
  Result:=0;
  Enter;
  try     PendingResultsCount:=0;
          if Assigned(fSokoFile) and (not fSokoFile.Levels.IsEmpty) then begin
             oCursor:=Screen.Cursor;
             try     if Assigned(fPluginThread) then fPluginThread.Suspend;
                     try     CurrentLevel:=TLevel(fSokoFile.Levels.First);
                             while Assigned(CurrentLevel) do begin
                               NextLevel:=TLevel(CurrentLevel.Next);
                               if (not (ltfLocked in CurrentLevel.Tag.Flags))
                                  and
                                  (     ltfNew    in CurrentLevel.Tag.Flags)
                                  and
                                  (PluginShutdown
                                   or
                                   (Assigned(MainForm)
                                    and
                                    MainForm.ShutDownApplication
                                   )
                                   or
                                   (not IsLevelRelatedToAlternatingOptimizationsInProgress(CurrentLevel))
                                  ) then
                                  try     if ImportGame(CurrentLevel) then Inc(Result);
                                  finally if DeleteImportedLevels__ then
                                             if   (ltfSelected in CurrentLevel.Tag.Flags)
                                                  or
                                                  (ltfSelectedForFurtherProcessing in CurrentLevel.Tag.Flags) then begin
                                                  // the level is selected for further processing;
                                                  // keep it on the queue, but avoid that it's
                                                  // imported again later
                                                  Exclude(CurrentLevel.Tag.Flags,ltfNew);
                                                  end
                                             else DeleteLevel(CurrentLevel);
                                  end;
                               CurrentLevel:=NextLevel;
                               end;
                     finally if Assigned(fPluginThread) then fPluginThread.Resume;
                     end;
             finally Screen.Cursor:=oCursor;
             end;
             end;
  finally Leave;
  end;
end;

function TPlugin.ReplayGame( {input:} Level__:TLevel; {output:} BoardAsText__ : PBoardAsTextBuffer; {output:} MovesAsText__ : PMovesAsTextBuffer; {input:} WallifyBoxesOutsideRangeOfPushesToBeOptimized__ : Boolean ):Boolean;
const SLICE_END_BOX_POSITION      = BOX_LEGAL_MOVE; // use existing board square flag values as local flags with different meanings
      SLICE_VISITED_SQUARE        = PLAYER_TRY_MOVE;
type  TBoard=array[0..(MAX_BOARD_WIDTH+2)*(MAX_BOARD_HEIGHT+2)+1] of Cardinal; {must be unsigned}
var   i,j,PlayerPos,ToPos,BoxToPos,LastPushMoveNo,LastPushedBoxPos,LastPushedBoxPlayerPos,LastPushedBoxCharIndex,LastSquareNo,SquareValue,SimpleLowerBound,BoxCount:Integer;
      Ch:Char;
      Direction,LastPlayerDirection,LastPushedBoxDirection:TDirection;
      s,s1:String;
      Board,StartBoard:TBoard;
      Offset:array[TDirection] of Integer;
      Snapshot:TNode;
      LastPushedBoxSecondaryMetrics:TSecondaryScoreMetrics;

  function  BoardAsTextToBoard(BoardWidth__,BoardHeight__:Integer; const BoardAsText__:String;
                               var LastSquareNo__,PlayerPos__, SimpleLowerBound__, BoxCount__:Integer;
                               var Board__:TBoard):Boolean;
  var SourceIndex,Col,Row,SquareValue,GoalCount:Integer;
  begin // precondition: 'BoardWidth__' and 'BoardHeight__' are valid board dimensions
    Result:=BoardWidth__*BoardHeight__=Length(BoardAsText__);
    if Result then begin
       SourceIndex:=0; LastSquareNo__:=BoardWidth__; PlayerPos__:=0; SimpleLowerBound__:=0; BoxCount__:=0; GoalCount:=0;
       for Col:=0 to LastSquareNo__ do Board__[Col]:=WALL; // top wall
       for Row:=1 to BoardHeight__ do begin
           Inc(LastSquareNo__); Board__[LastSquareNo__]:=WALL; // add a left side wall to guard against a move that wraps around the left/right edge
           for Col:=1 to BoardWidth__ do begin
               Inc(SourceIndex); Inc(LastSquareNo__);
               SquareValue:=CharToBoardSquareValue(BoardAsText__[SourceIndex]);
               if (SquareValue and PLAYER)<>0 then
                  if   PlayerPos__=0 then
                       PlayerPos__:=LastSquareNo__
                  else Result:=False; // this shouldn't happen
               if (SquareValue and BOX)<>0 then begin
                  if BoxCount__ < MAX_BOXES  then begin
                     Inc( BoxCount__ );
                     if (SquareValue and GOAL)=0 then Inc(SimpleLowerBound__);
                     end
                  else Result:=False;  // this shouldn't happen
                  end;
               if (SquareValue and GOAL)<>0 then begin
                  Inc(GoalCount);
                  Inc(SquareValue,SLICE_END_BOX_POSITION); // default initialization: the snapshot is a solution, meaning that all boxes are located at goal positions in the final position
                  end;
               Board__[LastSquareNo__]:=SquareValue;
               end;
           end;
       for Col:=Succ(LastSquareNo__) to Succ(LastSquareNo__)+BoardWidth__ do Board__[Col]:=WALL; // bottom wall
       Result:=Result and (BoxCount__=GoalCount) and ((Board__[PlayerPos__] and (WALL+FLOOR+PLAYER))=(FLOOR+PLAYER));
       end;
  end;

  function  BoardToBoardAsText( BoardWidth__, BoardHeight__ : Integer; const Board__ : TBoard; var BoardAsText__ : TBoardAsTextBuffer ) :Boolean;
  var Col, Row, Length, SourceIndex : Integer; //s : String;
  begin
     Length := 0;
     SourceIndex := BoardWidth__;
     for Row:=1 to BoardHeight__ do begin
         Inc( SourceIndex );
         for Col:=1 to BoardWidth__ do begin
             Inc( SourceIndex );
             BoardAsText__[ Length ] := BoardSquareToChar( Board__[ SourceIndex ] );
             Inc( Length );
             end;
         //BoardAsText__[ Length ] := CR; // test
         //Inc( Length );
         //BoardAsText__[ Length ] := LF; // test
         //Inc( Length );
         end;
    //BoardAsText__[ Length ] := CR; // test
    //Inc( Length );
    //BoardAsText__[ Length ] := LF; // test
    //Inc( Length );
    BoardAsText__[ Length ] := NULL_CHAR; // add a null-terminator after the board
    Result := True;
    //SetString( s, PChar( Addr( BoardAsText__ ) ), Pred( Length ) );
    //ClipBoard.AsText := s;
  end;

  function  MovesToMovesAsText( FromMove__, ToMove__ : Integer; const Moves__ : String; var MovesAsText__ : TMovesAsTextBuffer ) : Boolean;
  var Index, Length : Integer; //s : String;
  begin // extract the range of moves ]start, end], start exclusive, end inclusive
    Length                          := ToMove__ - FromMove__;
    for Index                       := 0 to Pred( Length ) do
        MovesAsText__[ Index ]      := Moves__[ Index + Succ( FromMove__ ) ];
    MovesAsText__[ Length         ] := NULL_CHAR; // add a null-terminator after the moves
    MovesAsText__[ Succ( Length ) ] := NULL_CHAR; // add an extra null-terminator after the last game (YASC only sends a single game to the optimizer)
    Result                          := True;
    //SetString( s, PChar( Addr( MovesAsText__ ) ), Length );
    //ClipBoard.AsText := s;
  end;

  {module for calculating a moves/lines optimal player path;
   it's unfortunate to duplicate such a large and complicated piece of code from
   'TSokGame', but it's the easiest way to equip the 'ReplayGame' function with
   the ability to optimize player moves;
   'ReplayGame' uses a 1-dimensional board in contrast to the 2D board in
   'TSokGame'; rewriting 'ReplayGame' to use a 'TSokGame' would entail
   allocating the rather large 'TSokGame' object statically for speed, or
   spending time on allocating and initializing a fresh 'TSokGame' object
   dynamically each time 'ReplayGame' is called;
  }
  const
    MAX_SINGLE_STEP_MOVES    = MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT;
  type
    TBoardOfDirectionSets    = array[0..(MAX_BOARD_WIDTH+2)*(MAX_BOARD_HEIGHT+2)+1] of TDirectionSet;
    TMove                    = packed record {'packed': sequence of unaligned slots, without gaps}
      Direction              : TDirection;
                               end;
    TSingleStepMoves         = array[0..MAX_SINGLE_STEP_MOVES] of TMove;
    TTimeStamp               = Cardinal;
  var
    PlayerMoveCount          : Integer;
    PlayerLineCount          : Integer;
    FinalDirection           : TDirection;
    PlayerMoves              : TSingleStepMoves;

  function  CalculatePlayerPath(ToSquare__:Integer;
                                PreferredFinalDirection__:TDirection;
                                MakeMoves__:Boolean;
                                var MoveCount__,LineCount__:Integer;
                                var FinalDirection__:TDirection;
                                var Moves__:TSingleStepMoves):Boolean;
  const BITS_PER_PLAYER_LINE_LENGTH=8; {each player line has this number of bits to represent its length; precondition: remainder(BITS_PER_UNSIGNED_INTEGER div BITS_PER_PLAYER_LINE_LENGTH) = 0}
  var   Distance,StopIndex,Square,MinPlayerPos,NeighborSquare:Integer;
        LineCount,LineLength,NeighborLineCount,NeighborLineLength:Cardinal;
        NeighborTimeStamp,NextTimeStamp,TimeStamp:TTimeStamp;
        Direction,LineDirection,NextLineDirection:TDirection;
        Directions:TDirectionSet;
        QueueBottom,QueueTop:^Integer;
        Queue,LineCounts,LineLengths,TimeStamps:TBoard;
        ParentDirections:TBoardOfDirectionSets;

  begin {Calculates a moves/lines optimal path from the current player position to the square 'ToSquare__', using the most recent player line lengths for tiebreaking}
    FillChar(TimeStamps,SizeOf(TimeStamps),0); {initialize timestamps}
    TimeStamp:=1;
    Result:=False; MinPlayerPos:=PlayerPos;
    {calculated square values: reachable empty floors = timestamp + distance + 1; reachable boxes = timestamp}

    if PlayerPos<>0 then begin
       QueueBottom:=Addr(Queue[Low(Queue)]);
       QueueTop:=QueueBottom; Inc(QueueTop); QueueTop^:=PlayerPos;

       TimeStamps       [PlayerPos]:=Succ(TimeStamp);     {'Succ': the distance to a square S is 'Squares[S] - TimeStamp - 1'; this convention is practical because it makes room for tagging squares with reachable boxes with the value 'Timestamp'}
       LineCounts       [PlayerPos]:=0;                   {the number of lines (almost the same as turns) it takes to get to the square via a moves/lines optimal path}
       LineLengths      [PlayerPos]:=0;
       ParentDirections [PlayerPos]:=[];

       while QueueBottom<>QueueTop do begin
         Inc(QueueBottom);                                {advance to the next item on the queue}
         Square          :=QueueBottom^;                  {get next square from the queue}

         if Square<>ToSquare__ then begin                 {'True': this isn't the target square; the breadth-first search ensures that all paths to the target square have been explored at the time the target square is selected for expansion}
            NextTimeStamp   :=Succ(TimeStamps  [Square]); {the next higher timestamp value, i.e., if the best path to a neighbor square goes through the current square, then this is the timestamp value for the neighbor square}
            Directions      :=ParentDirections [Square];  {directions from parent squares to the square currently being expanded}
            LineCount       :=LineCounts       [Square];  {the number of lines to get to this square via a moves/lines optimal path}
            LineLength      :=LineLengths      [Square];  {length of the most recent player lines on a moves/lines optimal path to this square}

            for Direction:=Low(Direction) to High(Direction) do begin {examine neighbors to this square}
                NeighborSquare:=Square+Offset[Direction];
                if (Board[NeighborSquare] and (WALL+BOX))=0 then begin {'True': the neighbor square in this direction doesn't contain a wall or a box}
                   NeighborTimeStamp:=TimeStamps[NeighborSquare]; {the timestamp for the neighbor square}

                   if  NeighborTimestamp                                 <   Timestamp then begin {'True': this is the first time this neighbor square is visited}
                       Inc(QueueTop); QueueTop^                          := NeighborSquare; {put the neighbor square on the queue}
                       if NeighborSquare<MinPlayerPos then MinPlayerPos  := NeighborSquare; {update the normalized (top-left) player position}
                       TimeStamps                       [NeighborSquare] := NextTimeStamp; {update the number of moves it takes to get to this neighbor square}
                       ParentDirections                 [NeighborSquare] := [Direction]; {the current direction is at the moment the only member of the set of directions leading to this neighbor square via a moves/lines optimal path}
                       if Direction                                      in Directions then begin {'True': the move is a continuation of an existing player player line}
                          LineCounts                    [NeighborSquare] := LineCount;
                          LineLengths                   [NeighborSquare] := Succ(LineLength);
                          end
                       else begin
                          LineCounts                    [NeighborSquare] := Succ(LineCount); {'Succ': it takes a turn to get to this neighbor square from the current parent square}
                          LineLengths                   [NeighborSquare] := Succ(LineLength shl BITS_PER_PLAYER_LINE_LENGTH); {'shl': each player line has 'BITS_PER_PLAYER_LINE_LENGTH' bits to represent its length}
                          end;
                       end
                    else
                       if   NeighborTimestamp                            =  NextTimestamp then begin {'True': this is a visit from another direction than the first one, but with the same number of moves}
                            if   Direction                               in Directions then begin {'True': the move is a continuation of an existing player player line}
                                 NeighborLineCount                       := LineCount;
                                 NeighborLineLength                      := Succ(LineLength);
                                 end
                            else begin
                                   NeighborLineCount                     := Succ(LineCount); {'Succ': it takes a turn to get to this neighbor square from the current parent square}
                                   NeighborLineLength                    := Succ(LineLength shl BITS_PER_PLAYER_LINE_LENGTH); {'shl': each player line has 'BITS_PER_PLAYER_LINE_LENGTH' bits to represent its length}
                                 end;
                            if   NeighborLineCount                       <  LineCounts[NeighborSquare] then begin {'True': this is a new best moves/lines path to this neighbor square}
                                 LineCounts             [NeighborSquare] := NeighborLineCount; {update the number of lines it takes to get to this neighbor square via a moves/lines optimal path}
                                 ParentDirections       [NeighborSquare] := [Direction]; {the current direction is at the moment the only member of the set of directions leading to this neighbor square via a moves/lines optimal path}
                                 LineLengths            [NeighborSquare] := NeighborLineLength;
                                 end
                            else if   NeighborLineCount                  =  LineCounts[NeighborSquare] then begin {'True': this is another moves/lines optimal path to this neighbor square}
                                      Include(ParentDirections[NeighborSquare],Direction); {update the set of directions from parent squares to this neighbor square via a moves optimal path}
                                      if LineLengths    [NeighborSquare] <  NeighborLineLength then {'True': the new path to the neighbor square has a better line length score}
                                         LineLengths    [NeighborSquare] := NeighborLineLength; {save the best line length score (for the most recent lines); it's used as a tiebreaker between equal moves/lines paths to a square}
                                      end;
                            end;
                   end;
                end;
            end
         else begin {this is the target square; update the return values and construct the path if it has been requested}
           QueueBottom:=QueueTop; {exit the 'while' loop, i.e., stop the search; the breadth-first search ensures that all paths to the target square have been explored at the time the target square is selected for expansion}

           Result          :=True; {'True': there is a path from the current player position to the target square}
           MoveCount__     :=Pred(Integer(TimeStamps[Square]-TimeStamp)); {return the number of pushes it takes to get to the target square}
           LineCount__     :=LineCounts[Square]; {return the number of lines it takes to get to the target square}

           FinalDirection__:=PreferredFinalDirection__; {initialize the final direction}
           if   MakeMoves__ then {'True': create the path from the current player position to the target square}
                StopIndex  :=1 {backtrack all the way back to the first move}
           else StopIndex  :=MoveCount__; {only backtrack a single move in order to find the final player direction}

           {construct the path backwards from the target square to the starting position, using the parent square directions}
           LineDirection:=PreferredFinalDirection__;                                    {first check if there is a moves/lines optimal path ending with the specified final direction}
           for  Distance:=MoveCount__ downto StopIndex do begin
                NextLineDirection:=LineDirection;                                       {continue backtracking in the current direction if there is a moves/lines optimal path going through the neighbor square in this direction}

                if not (LineDirection in ParentDirections[ToSquare__]) then begin       {'True': the player changes direction}
                   LineLength:=0;                                                       {initialize the search for the parent square with the highest line length, i.e., the one with the longest preceding lines}
                   for Direction:=Low(Direction) to High(Direction) do                  {find the direction to a parent square on a best path to the current square}
                       if Direction in ParentDirections[ToSquare__] then begin          {'True': there is a parent square in this direction which belongs to an optimal moves/lines path to the current square}
                          Square:=ToSquare__-Offset[Direction];
                          NeighborLineLength:=LineLengths[Square];
                          if not (Direction in ParentDirections[Square]) then           {'True': it requires a turn (on a moves/lines optimal path) to get from this neighbor square to the currently investigated square on the path}
                             NeighborLineLength:=NeighborLineLength shl BITS_PER_PLAYER_LINE_LENGTH; {'shl': for comparing line lengths, the extra turn to get from the neighbor square to the current square must be taken into account}
                          if LineLength<=NeighborLineLength then begin                  {'<=': not '<' because the starting position has line length = '0', and 'LineLength' is an unsigned integer which also was initialized to '0'}
                             LineLength:=NeighborLineLength;
                             NextLineDirection:=Direction;
                             end;
                          end;
                   if  (not (LineDirection in ParentDirections[ToSquare__])) and (Distance<>MoveCount__) then begin {'True': the current direction isn't a member of the optimal parent directions for the current square}
                       {check if it possible to continue in the current direction instead of making a turn now}
                       Square:=ToSquare__-Offset[LineDirection];
                       NeighborTimestamp:=Pred(TimeStamps[ToSquare__]);                 {timestamp for a neighboring square if there is a move-optimal to the current square via the neighbor square}
                       NeighborLineLength:=0;                                           {calculate the line length to the square next to the current square, moving on the line in the current direction, i.e., 'LineDirection'}
                       while (TimeStamps[Square]=NeighborTimeStamp)                     {while the next square in this direction is on a move-optimal path to the current position ...}
                             and
                             (not (LineDirection in ParentDirections[Square])) do begin {... and while it requires a turn to reach the next square in this direction}
                             Dec(Square,Offset[LineDirection]);                         {backtrack to the next square in this direction}
                             Dec(NeighborTimeStamp);                                    {timestamp for a neighboring square if there is a move-optimal path to the current square via the neighbor square}
                             Inc(NeighborLineLength);                                   {calculate the line length to the square which is the neighbor of the current square on the path being produced}
                             end;
                       if    (TimeStamps[Square]=NeighborTimeStamp)                     {'True': there is a move-optimal path via 'Square' which doesn't require a turn now, but if may not be moves/lines optimal}
                             and
                             (Pred(LineCounts[Square])=LineCounts[ToSquare__]) then begin {'True': there is a moves/lines optimal path which doesn't require a turn now; ('Pred': it saves one turn not making a turn now)}
                             {$IFDEF FPC}
                               NeighborLineLength:=NeighborLineLength+LineLengths[Square]; {in some versions of the FPC compiler, the built-in function 'Inc()' extends unsigned integers to long integers}
                             {$ELSE}
                               Inc(NeighborLineLength,LineLengths[Square]);             {calculate the line length to the square which is the neighbor of the current square on the path}
                             {$ENDIF}
                             if LineLength shl BITS_PER_PLAYER_LINE_LENGTH              {'shl': the best line length from the other directions stems from an 'n' lines path while this line length refers to an 'n+1' lines path on the forward path}
                                <=NeighborLineLength then begin                         {'<=': not '<' because the starting position has line length = '0', and 'LineLength' is an unsigned integer which also has been initialized to '0'}
                                NextLineDirection:=LineDirection;
                                end;
                             end;
                       end;
                   if  Distance=MoveCount__ then FinalDirection__:=NextLineDirection;   {update the final direction}
                   end;

                LineDirection:=NextLineDirection;                                       {update the current direction}
                Moves__[Distance].Direction:=LineDirection;                             {store the move direction before backtracking}
                Dec(ToSquare__,Offset[LineDirection]);                                  {backtrack to the parent position given by the current direction; 'ToSquare__' now contains the previous square on the path}
                end;
           end;
         end;
       end;
  end; {CalculatePlayerPath}

begin // replays a game either for verification or for creating a board-and-pushes excerpt taken from a given range of pushes in the game
  Result:=False;
  if Assigned(Level__) and
     (not Level__.BoardAsTextLines.IsEmpty) and
     (not Level__.SnapshotsAsText.IsEmpty)  then begin
     Snapshot:=Level__.SnapshotsAsText.First;
     repeat
       if Snapshot is TExtendedSnapshotAsText then with TExtendedSnapshotAsText(Snapshot) do with Metrics do with SecondaryMetrics do begin
          if not MovesAsTextLines.IsEmpty then with Level__ do with Tag do begin
             if  BoardAsTextToBoard(BoardWidth,BoardHeight,BoardAsTextLines.First.Text,LastSquareNo,PlayerPos,SimpleLowerBound,BoxCount,Board) then begin
                 if Assigned( BoardAsText__ ) then StartBoard:=Board; // only copy when necessary
                 s:=MovesAsTextLines.First.Text;
                 IsASolution:=False; MoveCount:=0; PushCount:=0; FillChar(SecondaryMetrics,SizeOf(SecondaryMetrics),0);
                 LastPushMoveNo:=-1; LastPushedBoxPos:=-1; LastPlayerDirection:=UP; LastPushedBoxDirection:=UP;
                 LastPushedBoxPlayerPos:=PlayerPos; LastPushedBoxCharIndex:=0;
                 LastPushedBoxSecondaryMetrics:=SecondaryMetrics;
                 Offset[LEFT]:=-1; Offset[RIGHT]:=1; Offset[DOWN]:=Succ(BoardWidth); Offset[UP]:=-Offset[DOWN];
                 WallifyBoxesOutsideRangeOfPushesToBeOptimized__:=WallifyBoxesOutsideRangeOfPushesToBeOptimized__ and Assigned( BoardAsText__ ) and (SelectedRange[0]<SelectedRange[1]);
                 Result:=True;
                 i:=0;
                 while ( i<Length(s) ) and Result do begin
                   Inc(i); Ch:=s[i];
                   Result:=CharToDirection(Ch,Direction);
                   if   Result then begin
                        ToPos:=PlayerPos+Offset[Direction];
                        if (Board[ToPos] and (WALL+FLOOR))=FLOOR then begin
                           if (Board[ToPos] and BOX)=0 then begin // a non-pushing player move
                              Ch:=LoCase(Ch);
                              if Ch<>s[i] then s[i]:=Ch; // Delphi uses copy-on-write for shared strings, so only update the string if necessary

                              Inc(MoveCount);

                              Dec(Board[PlayerPos],PLAYER);
                              PlayerPos:=ToPos;
                              Inc(Board[PlayerPos],PLAYER);

                              if   MoveCount>1 then begin
                                   if Direction<>LastPlayerDirection then
                                      Inc(PlayerLines);
                                   end
                              else PlayerLines:=1;
                              LastPlayerDirection:=Direction;
                              end
                           else begin // a box push
                              BoxToPos:=ToPos+Offset[Direction];
                              if (Board[BoxToPos] and (WALL+FLOOR+BOX))=FLOOR then begin
                                 Ch:=UpCase(Ch);
                                 if Ch<>s[i] then s[i]:=Ch; // Delphi uses copy-on-write for shared strings, so only update the string if necessary

                                 if ( PushCount = SelectedRange[ 1 ] ) and ( SelectedRange[ 1 ] <> 0 ) then begin
                                    // this is the first push after a slice which will be sent to an optimizer plugin for optimization;
                                    // store the current player position and the push direction;
                                    // after optimization, it's necessary to substitute/construct/optimize the non-pushing player
                                    // moves starting from the last pushed box in the optimized slice and ending with the player
                                    // position before the first push after the slice;
                                    SelectedRange[ 5 ] := ( PlayerPos shl BITS_PER_BYTE ) + Ord( Direction );
                                    end;

                                 if      LastPushMoveNo<>MoveCount then Inc(PushingSessions);
                                 if      LastPushedBoxPos<>ToPos then begin
                                         Inc(BoxChanges); Inc(BoxLines);
                                         end
                                 else if LastPushedBoxDirection<>Direction then
                                         Inc(BoxLines);

                                 Inc(MoveCount); Inc(PushCount);
                                 LastPushMoveNo:=MoveCount;
                                 LastPushedBoxPos:=BoxToPos;
                                 LastPushedBoxDirection:=Direction;
                                 LastPushedBoxPlayerPos:=ToPos;
                                 LastPushedBoxCharIndex:=i;

                                 Dec(Board[PlayerPos],PLAYER);     // remove player
                                 PlayerPos:=ToPos;                 // new player position
                                 Inc(Board[PlayerPos],PLAYER-BOX); // add player and remove box
                                 Inc(Board[BoxToPos],BOX);         // add box

                                 if   MoveCount>1 then begin
                                      if Direction<>LastPlayerDirection then
                                         Inc(PlayerLines);
                                      end
                                 else PlayerLines:=1;
                                 LastPlayerDirection:=Direction;

                                 if (Board[ToPos] and GOAL)<>0 then
                                    Inc(SimpleLowerBound); // box leaving  a goal square
                                 if (Board[BoxToPos] and GOAL)<>0 then
                                    Dec(SimpleLowerBound); // box entering a goal square

                                 if ( PushCount >  SelectedRange[ 0 ] ) and        // '>' : start of slice exclusive
                                    ( PushCount <= SelectedRange[ 1 ] ) then begin // '<=': end   of slice inclusive
                                    Board[ PlayerPos ] := Board[ PlayerPos ] or SLICE_VISITED_SQUARE;
                                    Board[ BoxToPos  ] := Board[ BoxToPos  ] or SLICE_VISITED_SQUARE;
                                    end;

                                 if PushCount  =  SelectedRange[ 0 ] then begin // 'True': create board matching the current position
                                    SelectedRange[ 3 ] := i;
                                    if Assigned( BoardAsText__ ) then StartBoard := Board; // only copy when necessary
                                    end;
                                 if PushCount  =  SelectedRange[ 1 ] then begin // 'True': extract the given range of moves
                                    SelectedRange[ 4 ] := i;
                                    SelectedRange[ 5 ] := 0; // '0': the next push after the slice hasn't been located yet
                                    if Assigned( MovesAsText__ ) and
                                       ( not MovesToMovesAsText( SelectedRange[ 3 ], SelectedRange[ 4 ], s, MovesAsText__^ ) ) then
                                       Result  := False;
                                    if Assigned( BoardAsText__ ) then
                                       for j := BoardWidth to LastSquareNo do begin
                                           SquareValue := Board[ j ] and ( not SLICE_END_BOX_POSITION );
                                           if ( SquareValue and BOX ) <> 0 then Inc( SquareValue, SLICE_END_BOX_POSITION );
                                           Board[ j ] := SquareValue;
                                           end;
                                    end;

                                 if SelectedRange[ 4 ] <> 0 then // 'True': remember secondary metrics; it's only necessary to do that when player moves after an optimized slice need optimization
                                    LastPushedBoxSecondaryMetrics := SecondaryMetrics;
                                 end
                              else Result:=False; // pushing the box into a wall or another box
                              end;
                           end
                        else Result:=False;
                        end
                   else Result:=Ch<=SPACE; {skip whitespace characters}

                   if i = SelectedRange[ 3 ] then       // start of slice, in characters; update start of slice measured in pushes
                      SelectedRange[ 0 ] := PushCount;
                   if i = SelectedRange[ 4 ] then begin // end   of slice, in characters; update end   of slice measured in pushes
                      SelectedRange[ 1 ] := PushCount;
                      // insert optimal player moves after the optimized slice leading up to the first push after the slice, if any
                      if SelectedRange[ 5 ] <> 0 then begin // [5] contains next push direction and the player position before the next push
                         // move player back to the position after the last push
                         Dec(Board[PlayerPos],PLAYER);
                         PlayerPos:=LastPushedBoxPlayerPos;
                         Inc(Board[PlayerPos],PLAYER);
                         // calculate moves/lines optimal player path to the next push
                         Result := CalculatePlayerPath( SelectedRange[ 5 ] shr BITS_PER_BYTE, TDirection( SelectedRange[ 5 ] and High( Byte ) ), True, PlayerMoveCount, PlayerLineCount, FinalDirection, PlayerMoves );
                         if Result then begin // 'True': there is a path to the next push; otherwise, the optimized slice ends with the player in a different access area, in which case the optimization must be discarded;
                            // update the moves text string with the new player path between the previous push and the next push
                            SetLength( s1, PlayerMoveCount );
                            for j := 1 to PlayerMoveCount do // make player moves between the last push and the next
                                s1[ j ] := MoveToChar(PlayerMoves[ j ].Direction, False, True );

                            j := Length( s );
                            repeat Inc( i );
                            until  ( i > j ) or ( ( s[ i ] >= 'A' ) and ( s[ i ] <= 'Z' ) ); // skip lowercase move characters, i.e., non-pushing player moves
                            s := Copy( s, 1, LastPushedBoxCharIndex ) + s1 + Copy( s, i, MaxInt ); // next text string with moves

                            i := LastPushedBoxCharIndex; // go back to the first non-pushing player move after the last push
                            MoveCount := LastPushMoveNo; // restore game metrics to the state at the time of the last push
                            LastPlayerDirection := LastPushedBoxDirection;
                            SecondaryMetrics := LastPushedBoxSecondaryMetrics;
                            //Clipboard.AsText := s;
                            end;
                         SelectedRange[ 5 ] := 0; // clear the stored player position and direction, so the player path isn't calculated and inserted again
                         end;
                      end;

                   if (SimpleLowerBound=0) and (PushCount>0) and (i<Length(s)) then begin // 'True': a non-empty solution has been found, with moves (or at least characters) after the solution
                      Delete(s, Succ(i),MaxInt);
                      if SelectedRange[ 0 ] >  PushCount then SelectedRange[ 0 ]:= PushCount;
                      if SelectedRange[ 1 ] >  PushCount then SelectedRange[ 1 ]:= PushCount;
                      if SelectedRange[ 0 ] >= SelectedRange[ 1 ] then begin
                         SelectedRange[ 0 ] := 0;
                         SelectedRange[ 1 ] := 0;
                         end;
                      if SelectedRange[ 3 ] >  i         then SelectedRange[ 3 ]:= i;
                      if SelectedRange[ 4 ] >  i         then SelectedRange[ 4 ]:= i;
                      if SelectedRange[ 3 ] >= SelectedRange[ 4 ] then begin
                         SelectedRange[ 3 ] := 0;
                         SelectedRange[ 4 ] := 0;
                         end;
                      SelectedRange   [ 5 ] := 0; // '0': there is no push after a slice, if any
                      end;
                   end; // end of moves

                 if Result and ( SelectedRange[ 1 ] > PushCount ) then begin // 'True': extract the given range of pushes when the end-of-range overshoots the length of the solution
                    SelectedRange[ 4 ] := Length(s);
                    SelectedRange[ 5 ] := 0; // '0': there is no push after the slice
                    if Assigned( MovesAsText__ ) and
                       ( not MovesToMovesAsText( SelectedRange[ 3 ], SelectedRange[ 4 ], s, MovesAsText__^ ) ) then
                       Result  := False;
                    if Assigned( BoardAsText__ ) then
                       for j := BoardWidth to LastSquareNo do begin
                           SquareValue := Board[ j ] and ( not SLICE_END_BOX_POSITION );
                           if ( SquareValue and BOX ) <> 0 then Inc( SquareValue, SLICE_END_BOX_POSITION );
                           Board[ j ] := SquareValue;
                           end;
                    end;

                 if Result then begin
                    SelectedRange[2]:=PushCount; // update maximum range of a slice, measured in pushes
                    Result:=MovesAsTextLines.First.SetText(s);
                    IsASolution:=True;
                    for i:=BoardWidth to LastSquareNo do begin
                        SquareValue:=Board[i];
                        if      (   SquareValue and GOAL)<>0 then begin
                                if (SquareValue and BOX )= 0 then IsASolution:=False;
                                end
                        else if (   SquareValue and BOX )<>0 then IsASolution:=False;
                        end;
                    end;

                 if Result and Assigned( BoardAsText__ ) then begin
                    if WallifyBoxesOutsideRangeOfPushesToBeOptimized__ then begin
                       for j := BoardWidth to LastSquareNo do begin
                           SquareValue     := StartBoard[ j ] and ( not GOAL );     // remove existing goals
                           if ( Board[ j ] and SLICE_END_BOX_POSITION ) <> 0 then   // 'True': a box position at the end of the slice
                              if   ( Board[ j ] and SLICE_VISITED_SQUARE ) = 0 then // 'True': the box hasn't been pushed in the selected range of pushes
                                   SquareValue := WALL                              // turn the box into a wall
                              else Inc( SquareValue, GOAL   );                      // turn the box end-position into a target square for the search
                           StartBoard[ j ] := SquareValue;
                           end;
                       end;

                    Result := Result and BoardToBoardAsText( BoardWidth, BoardHeight, StartBoard, BoardAsText__^ );
                    end;
                 end;
             end
          else Result:=False;
          end
       else Result:=False;
       Snapshot:=Snapshot.Next; // advance to next snapshot, if any
     until (not Result) or (not Assigned(Snapshot)) or (Snapshot=Snapshot.Last); // 'Last': currently, only optimizer plugins may return more than one snapshot (solution), and optimizer levels have the original solution as the last member of the snapshot list
     end;
end;

end.

