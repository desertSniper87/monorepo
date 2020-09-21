unit SokGame_;
{$DEFINE SokobanYASC} {only when this module is used as part of 'Sokoban YASC'}
{///$DEFINE SokHtml}
{
 File           : SokGame_.pas
 Function       : A Sokoban game kernel
 Copyright      : (c) 2001-2017 by Brian Damgaard, Denmark
 WARRANTY       : ABSOLUTELY NONE - USE THIS CODE AT YOUR OWN RISK.

 Original Author: Brian Damgaard, Denmark.
 E-mail         : BrianDamgaard@jubii.dk
 Date           : 2001-2016

 Modified by    : ...
 E-mail         : ...
 Date           : ...

 --------------------------------------------------------------------
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
 --------------------------------------------------------------------
}

interface

uses Windows, {for 'GetTickCount'}
     SokUtil_,SokFile_;

{
 --------------------------------------------------------------------
 Sokoban engine
 --------------------------------------------------------------------
 The engine handles one level at a time:

 SokoGame
   Board
   History (current game moves)
   Best Solution/Moves
   Best Solution/Pushes
   Notes
   Snapshots and solutions

 A game uses methods like 'LoadLevelFromFile' and 'SaveToFile' to
 communicate with a low-level file-manager. A Sokoban-program can
 retrieve and update game data, either by direct access to the slots,
 or by using the built-in methods.

 --------------------------------------------------------------------
}

type
  TMove               = TColRow;
  TMoves              = array[0..MAX_MOVES+1] of TMove; {'+1': so a pointer beyond last move is legal, and so movelists may include a sentinel}
  PMoves              = ^TMoves;
  TPlayerMoves        = array[0..MAX_BOARD_SIZE+1] of TMove; {'+1': so a pointer beyond last move is legal, and so movelists may include a sentinel}
  PPlayerMoves        = ^TPlayerMoves;

  TSnapshot = class(TNode)              {internal format, as opposed to 'SokFile_.TSnapshotAsText'}
    BoxPos            :TBoxPositions;   {box positions}
    ForcedInitialJumps:Integer;         {for a reverse mode game, the number of jumps it takes to get the player away from a box square (not implemented)}
    GameState         :TGameState;      {game state at the end of the snapshot ('MoveTop'), not in the current position ('MoveCount')}
    LastBoxNo         :Integer;         {last moved box in current position}
    LastPushIndex     :Integer;         {last moved box: the corresponding move number in the history}
    Modified          :Boolean;         {new or modified snapshot during current session with this level}
    Moves             :PHistoryMoves;   {game history in internal form; *caution*: only the actually used byte-size is allocated}
    MoveCapacity      :Integer;         {max position, i.e., the number of 'Moves' (*not* byte-size)}
    MoveCount         :Integer;         {current position}
    MovesAsText       :String;          {note: use the function 'GetMovesAsText' to get the value which only is created on demand, e.g., by the optimizer}
    MoveTop           :Integer;         {total number of moves ( > 'MoveCount' if any moves were taken back}
    PlayerPos         :TColRow;         {current player position}
    PushCount         :Integer;         {total number of pushes (or pulls) for current position}
    Notes             :TNotes;          {copy of 'SnapshotAsText.Notes'}
    ReverseMode       :Boolean;         {reverse/normal mode game}
    SecondaryScoreMetrics
                      :TSecondaryScoreMetrics;
    SnapshotTag       :Integer;         {the tag field was added rather late in the life-cycle of the application, and therefore it was considered too dangerous to name it 'Tag' like the other tag-fields; a 'WITH' Pascal-statement somewhere in the application could accidentally start referring to this tag instead of, say, 'TLevel.Tag'}

    constructor Create; {throws EOutOfMemory}
    destructor  Destroy; override;

    function    ByteSize(Capacity__:Integer):Integer;
    procedure   Clear; override;
    function    Clone:TSnapshot;
    function    CopyTo(Destination__:TNode):Boolean; override; {not implemented}
    function    ExpandMoves(MoveTop__:Integer; Moves__:PHistoryMoves):Boolean;
    function    GetMovesAsText:String;
    function    HasIdenticalMoves(ReverseMode__:Boolean; MoveCount__,MoveTop__:Integer; Moves__:PHistoryMoves):Boolean;
    function    MakeName(SecondaryMetricsInTitles__:Boolean):String;
    function    MakeSnapshotAsText(PrettyPrintGamesEnabled__,RleEnabled__,CombinedMoveFormattingEnabled__,CopyNotes__:Boolean):TSnapshotAsText;
    function    ObjectType:TObjectType; override;
    function    Rename(const NewName__:String; var ErrorStr__:String):Boolean;
    function    WriteToFile(TextFile__:PTextFile):Integer; override; {not implemented}
  end;

  TSokoGame = class(TNode)
    BestSolutionMoves:TSnapshot;            {best solution/moves  found so far}
    BestSolutionPushes:TSnapshot;           {best solution/pushes found so far}
    Board:TBoard;                           {current position}
    BoardHeight:Integer;                    {board dimensions}
    BoardTransformation2D:TBoardTransformation2D; {board transformation}
    BoardWidth:Integer;
    BoxCount:Integer;                       {number of boxes = number of goals}
    BoxOnTargetCount:Integer;               {normal game:  boxes on goal positions; reverse mode game: boxes on box-start positions}
    BoxPos:TBoxPositions;                   {box positions}
    BoxTargetMaskForCalculations:Integer;   {normal game: 'GOAL'; reverse mode game: 'BOX_START'}
    BoxTargetMaskForDisplay:Integer;        {if reverse mode game and 'ShowBoxStartPositionsAsGoalsInReverseMode' then 'BOX_START' else 'GOAL'}
    BuiltInBestSolutionMoves:TSnapshot;     {built-in best solution/moves}
    BuiltInBestSolutionPushes:TSnapshot;    {built-in best solution/pushes}
    DisplayName:String;                     {current level name formatted for display}
    ForcedInitialJumps:Integer;             {reverse mode game, start position: number of jump-moves to get the player away from a box position}
    FloorCount:Integer;                     {number of non-wall squares reachable by the player if boxes are removed from the board}
    GameState:TGameState;                   {game state, e.g., 'play', 'solved', etc.}
    GoalCount:Integer;                      {number of goals = number of boxes}
    GoalPos:TBoxPositions;                  {goal positions}
    History:THistory;                       {current game history}
    IsLoading:Boolean;                      {'True' while loading a level}
    JumpsAllowedAfterFirstBoxMoveInReverseMode:Boolean; {allowing jumps after first box-move in reverse mode is optional}
    KeepOriginalSnapshotNames:Boolean;      {a switch added late in the life cycle of the program to avoid that 'TestForNewBestSolution' changes original snapshot names while a level is being loaded; the 'IsLoading' switch serves so many purposes that it's wasn't safe to let it control the name-update too}
    LastSolutionWasTheFirstOne:Boolean;     {'True': last found solution (in this session) was the first solution for this level}
    CombinedMoveFormattingEnabled:Boolean;  {'True': save combined moves like '(' ... ')', e.g., '(rrrDDD)(llUU)'}
    Notes:TNotes;                           {level notes}
    OriginalBestSolutionMovesName:String;   {name of best solution/moves  when the level was loaded}
    OriginalBestSolutionPushesName:String;  {name of best solution/pushes when the level was loaded}
    OriginalBoardTransformation2D:          {rotations and mirroring when the level was loaded}
      TBoardTransformation2D;
    {$IFDEF SokobanYASC}
      OriginalMultiViewCount:Integer;       {number of multiple views when the level was loaded}
    {$ENDIF}
    OriginalTimeMS:TTimeMS;                 {time spent on the level when it was loaded}
    OriginalSnapshotCount:Integer;          {number of snapshots when the level was loaded}
    PathFindingMaxTimeMS:TTimeMS;           {timelimit for finding a box path}
    PathFindingOptimizeMoves:Boolean;       {optimize box paths for moves or pushes}
    PathFindingTimeInterval:TTimeIntervalMS;{time interval for finding box paths}
    PlayerPos:TColRow;                      {player's current position}
    PlayersReachableSquaresTimestampAfterLastPush:SokUtil_.TTimestamp; {used for optimizing moves between pushes}
    PlayerStartPos:TColRow;                 {player's initial position}
    PrettyPrintGamesEnabled:Boolean;        {use 'pretty-print' when saving snapshots}
    {$IFDEF SokHtml}
      PushesLowerBound:Integer;             {the lower bound of the number of pushes for a push-optimal solution}
    {$ENDIF}
    ResetSaveGameAndLoadItIfItIsANormalModeGame:Boolean; {what to do with a saved game when a level is loaded}
    RestoreSaveGame:Boolean;                {what to do with a saved game when a level is loaded}
    ReverseMode:Boolean;                    {game mode: normal/reverse}
    RleEnabled:Boolean;                     {use run-length encoding when saving snapshots}
    SaveBestSolutionsAutomatically:Boolean; {on false, the user can avoid overwriting the level-file}
    SaveGame:TSnapshot;                     {saved game from last session with current level}
    SaveOldSolutionsAfterFindingBetterOnes:Boolean; {on true, dethroned best solutions are saved on the snapshot list}
    SaveSnapshotsAutomatically:Boolean;     {on false, the user can avoid overwriting the level-file}
    SecondaryMetricsInTitles:Boolean;       {on true, create titles with "moves/pushes/lines/changes/sessions"}
    SessionTimeMS:TTimeMS;                  {time spent on this level during current session}
    ShowBoxStartPositionsAsGoalsInReverseMode:Boolean;   {in reversemode, show box start positions as goal positions}
    SimpleIllegalMovesMask:Integer;         {mask value for rejecting/allowing simple illegal moves}
    Snapshots:TList;                        {snapshots of current level; internal format as opposed to 'TLevel.SnapshotsAsText'}
    SokoFile:TSokoFile;                     {note: the game does not own this file-reader/writer, but it must stay alive while a level is loaded}
    SokoFileName:String;                    {name of the file to which current level belongs}
    SolutionsRequireAtLeastOnePush:Boolean; {decides whether a starting position with all boxes on goals is a solution}
    StartBoard:TBoard;                      {initial position}
    StartBoardAsText:String;                {note: use 'GetStartBoardAsText' to get the value which only is created on demand, e.g., by the optimizer}
    StartBoxOnTargetCount:Integer;          {number of boxes on targets in the initial position}
    StartTimeMS:TTimeMS;                    {timer for the time spent on this level}
    TimeStatistics:array[0..3] of TTimeMS;  {for testing}
    TimingEnabled:Boolean;                  {enables/disable timing for current level}
    TimingIdleTimeThresholdEnabled:Boolean; {stop timer if idle}
    TimingIdleTimeThresholdMS:TTimeMS;      {idle time threshold}
    Verbose:Boolean;                        {show errors if loading a level fails}

    constructor Create; {throws EOutOfMemory}
    destructor  Destroy; override;

    function    AppendReverseModeSolution(Count__:Integer; Moves__:PHistoryMoves;
                                          LastMoveIndex__:Integer;
                                          var History__:THistory):Boolean;
    function    BestSolutionsCount:Integer; {0, 1, or 2, i.e., no solution, 1 single best solution, or separate best solutions for moves and pushes respectively}
    function    BoxLegalMoves(Col,Row:Integer; var TimeOut:Boolean):Integer; virtual;
    function    BoxPath(StartPos__,EndPos__,PlayerPos__:TColRow;
                        var MoveCount__:Integer; var Moves__:TMoves):Boolean; virtual;
    function    CalculateGameState: TGameState;
    procedure   CalculateInternalData; virtual;
    function    CalculateMovableBoxes:Integer;
    procedure   CalculatePathFindingTimeInterval;
    function    CalculatePlayersDistanceToAllReachableSquares(PlayerPos__:TColRow):SokUtil_.TTimestamp;
    function    CalculateSecondaryScoreMetrics (var   SecondaryScoreMetrics__:TSecondaryScoreMetrics):Boolean;
    procedure   CalculateScoreAndState;
//  function    CalculateWeightedSecondaryScore(const SecondaryScoreMetrics__:TSecondaryScoreMetrics):Integer;
    procedure   Clear; override;
    procedure   ClearTimer;
    function    CloseLevel(Flush__:Boolean):Boolean; virtual; {related: LoadLevelFromFile/CloseLevel}
    function    CopyTo(Destination__:TNode):Boolean; override; {not implemented}
    procedure   DeleteAllSnapshots;
    procedure   DeleteSnapshot(var Snapshot__:TSnapshot); virtual;
    procedure   DethroneBestSolution(var Snapshot__:TSnapshot);
    procedure   DoBoardTransformation2D(Transformation2D__:TTransformation2D; TransformStartBoardOnly__:Boolean); virtual; {the transformation operation is relative to current transformation}
    function    DoMoveUpdateBoard0(dx__,dy__,Flags__:Integer):Integer;{(dx,dy) must be a legal move}
    function    DoMoveUpdateBoard(dx__,dy__,Flags__:Integer):Integer; virtual;
    procedure   DoMoveUpdateGame(dx__,dy__,LastMoveIndex__,Flags__:Integer);
    function    ElapsedTimeMS:TTimeMS;
    function    GetStartBoardAsText:String;
    function    IsABetterBuiltInSolutionAvailable(var BetterBuiltInSolutionMoves__,BetterBuiltInSolutionPushes__:TSnapshot):Boolean;
    function    IsABetterSolutionMoves(Count__, PushCount__:Integer; const SecondaryScoreMetrics__:TSecondaryScoreMetrics):Boolean;
    function    IsABetterSolutionPushes(Count__,PushCount__:Integer; const SecondaryScoreMetrics__:TSecondaryScoreMetrics):Boolean;
    function    IsAFreezingMove(FromCol__,FromRow__,ToCol__,ToRow__,PlayerCol__,PlayerRow__:Integer):Boolean;
    function    IsAFrozenSquare(Col__,Row__:Integer; var BoxCount__:Integer; var BoxSquares__:TBoxPositions):Boolean;
    function    IsALegalMove(dx__,dy__:Integer; Flags__:Integer; var IsAFreezingMove__:Boolean):Boolean; virtual;
    function    IsAnEmptyCol(Col__:Integer):Boolean; {1-based 'Col__'}
    function    IsAnEmptyRow(Row__:Integer):Boolean; {1-based 'Row__'}
    function    IsAnIdenticalBoard(const Board__:TBoard; BoxNumbersMustMatch__,PlayerPositionMustMatch__:Boolean):Boolean;
    function    IsEqualToCurrentGame(Snapshot__:TSnapshot):Boolean;
    function    IsJumpFeatureUsedAfterFirstBoxMove(ReverseMode__:Boolean; Count__:Integer; Moves__:PHistoryMoves; var InitialJumps__:Integer):Boolean;
    function    ImportBuiltinSolutions:Boolean; {returns 'True' if anything is imported}
    procedure   LoadBoard(const Board__:TBoard); virtual;
    procedure   LoadBoard0(const Board__:TBoard); {load board without any inherited additional functioniality}
    procedure   LoadBoxes(PlayerPos__:TColRow; BoxPos__:PBoxPositions; ReverseMode__:Boolean); virtual;
    function    LoadFromBoardAsText(BoardWidth__,BoardHeight__:Integer; NormalizeBoard__,MovePlayerAndBoxes__,ChangeImmovableBoxesToWalls__,FillUnnecessaryIllegalBoxSquares__:Boolean; const BoardAsText__:String):Boolean; virtual;
    function    LoadFromFile(const LevelName__:String; SokoFile__:TSokoFile; Flush__:Boolean; var ErrorStr__:String):Boolean; reintroduce;
    function    LoadSnapshot(Snapshot__:TSnapshot):Boolean; virtual;
    function    LoadSnapshot0(Snapshot__:TSnapshot):Boolean; {load snapshot without any inherited additional functioniality like checking various game states before loading}
    function    LoadSnapshotAsTextString(const SnapshotAsTextString__:String; IgnoreIllegalMoves__:Boolean):Boolean; virtual;
    function    LoadSnapshotAsText(SnapshotAsText__:TSnapshotAsText; Macros__:TList; var Snapshot__  :TSnapshot):Boolean; {throws EOutOfMemory}
    function    LookupSnapshot(ReverseMode__:Boolean; MoveCount__,MoveTop__:Integer; Moves__:PHistoryMoves; var Snapshot__:TSnapshot):Boolean;
    procedure   MakeCombinedMove(FromIndex__,ToIndex__:Integer);
    function    MakeSnapshot(const Name__:String):TSnapshot; virtual;
    function    MakeSnapshot0(const Name__:String):TSnapshot; {load snapshot without any inherited additional functioniality}
    function    NonOptimalText:String;
    function    ObjectType:TObjectType; override;
    function    PlayerDirection(MoveIndex__:Integer):TDirection; {the player is "looking" in this direction}
    function    PlayerJump(const FromPos__,ToPos__:TColRow; MakeMoves__:Boolean;
                           var MoveCount__,PlayerLinesCount__:Integer; var Moves__:TMoves):Boolean;
    function    PlayerPath(FromPos__,ToPos__:TColRow; IsAFinalPreferredDirection__,IsPreferredDirectionSupplied__:Boolean; PreferredDirection__:TDirection; var MoveCount__,PlayerLinesCount__:Integer; var IsPreferredDirectionOK__:Boolean; Moves__:PPlayerMoves):Boolean;
    function    PlayerPathLength(const FromPos__,ToPos__:TColRow; var MoveCount__:Integer):Boolean;
    function    PlayerLegalJumps:Integer;
    function    PlayerLegalMoves(ContinueFromSquareCol__,ContinueFromSquareRow__: Integer; var TopLeftCol__,TopLeftRow__:Integer):Integer;
    function    Redo(RedoCombinedMoves__:Boolean):Boolean; virtual;
    function    Redo0(RedoCombinedMoves__:Boolean):Boolean; {redo next move without any inherited additional functioniality like displaying the move}
    function    RemoveNonOptimalText(const Text__:String):String;
    function    RemoveRedundantWalls:Integer;
    function    RenameSnapshot(const OldName__,NewName__:String):Boolean;
    procedure   RenumberCombinedMoves;
    procedure   Reset(SeparateUndoneMoves__:Boolean); virtual;
    function    ResetAndLoadSaveGame(RestoreSaveGame__,
                                     ResetSaveGameAndLoadItIfItIsANormalModeGame__:Boolean):Boolean;
    function    SaveToFile (SokoFile__:TSokoFile; UpdateBestSolutions__,UpdateSnapshots__,UpdateTime__,UpdateBoardTransformation2D__,Flush__:Boolean):Boolean;
    function    SaveToFile0(SokoFile__:TSokoFile; UpdateBestSolutions__,UpdateSnapshots__,UpdateTime__,UpdateBoardTransformation2D__,Flush__:Boolean):Boolean;
    function    SaveSnapshot:Boolean;
    function    SecondaryMetricsAsText:String; {note: returns an empty string if 'SecondaryMetricsInTitles' is disabled}
    procedure   SeparateMoves(Index__:Integer; var History__:THistory);
    procedure   SeparateUndoneMoves;
    procedure   SetReverseMode(ReverseMode__:Boolean);
    function    SnapshotTypeName(SnapshotType__:TSnapshotType):String; virtual;
    function    SolverNameText(const Text__:String):String;
    procedure   StartTimer; virtual;
    procedure   StopTimer; virtual;
    function    TestForNewBestSolution:Boolean; virtual;
    procedure   ToggleReverseMode;
    function    Undo(UndoCombinedMoves__:Boolean):Boolean; virtual;
    procedure   UnpackLastMove(var PlayerFromPos__,PlayerToPos__,
                                   BoxFromPos__,BoxToPos__:TColRow;
                               var BoxNo__:Integer);
    procedure   UpdateBestSolutionNames;
  end;

  TBoardDirectionArrayOfBoolean   =array[0..MAX_BOARD_WIDTH+1,0..MAX_BOARD_HEIGHT+1,TDirection] of Boolean;
  TBoardDirectionArrayOfTimeStamps=array[0..MAX_BOARD_WIDTH+1,0..MAX_BOARD_HEIGHT+1,TDirection] of SokUtil_.TTimeStamp;

{
 --------------------------------------------------------------------
 Exported functions
 --------------------------------------------------------------------
}

function  SecondaryMetricsFormattedAsATitleSuffix(SecondaryMetricsInTitles__:Boolean; const SecondaryScoreMetrics__:TSecondaryScoreMetrics):String;
{
procedure SetDefaultScoreMetricsWeights;
}
{
 --------------------------------------------------------------------
 Global constants
 --------------------------------------------------------------------
}
var
  AMove                    : TMove; {only used for definition of 'MOVE_LIST_...' constants}
const
  DEFAULT_PATH_FINDING_MAX_TIME_MS
                           = 2000;  {default timelimit for (box) pathfinding (2 seconds)}
  DEFAULT_PLAYER_DIRECTION = Down;  {player direction before first move (many skins show the back of the pusher when direction is 'up'}
  MAX_SCORE_METRICS_WEIGHT = 100;
  MOVE_LIST_FLAG_BOX       = ( High( AMove.X ) +1 ) shr 1; {e.g., 128 when move co-ordinates are byte-sized}  {don't confuse move-list flags with history move flags like 'H_FLAG_BOX'}
  MOVE_LIST_FLAG_JUMP      = MOVE_LIST_FLAG_BOX;       {don't confuse move-list flags with history move flags like 'H_FLAG_JUMP'}
  MOVE_FLAG_KEYBOARD       = MOVE_LIST_FLAG_BOX * 2;   {don't confuse move-flags with history flags like 'H_FLAG_BOX'}
  MOVE_FLAG_FORKLIFT_DRIVING                           {'True': drive in reverse instead of making a u-turn}
                           = MOVE_FLAG_KEYBOARD * 2;
  {$IFDEF SokHtml}
    // error codes and special return values for calculating the pushes lower bound must be negative integers
    PUSHES_LOWER_BOUND_PARITY_ODD
                           = -1;
    PUSHES_LOWER_BOUND_PARITY_EVEN
                           = -2;
    PUSHES_LOWER_BOUND_UNDEFINED
                           = -3;
  {$ENDIF}
  TIMESTAMP_FLAG_VISITED   = 1;     {flags and masks available for algorithms using board timestamps; the low bits of the timestamps are reserved for flags}
  TIMESTAMP_FLAG_TARGET    = 2;
  TIMESTAMP_FLAGS          = TIMESTAMP_FLAG_VISITED+TIMESTAMP_FLAG_TARGET; {sum of the flags stored together with timestamps; it must be a 2^n-1 number}
  TIMESTAMP_INCREMENT      = TIMESTAMP_FLAGS+1; {must be a 2^n number, n>=0}
  TIMESTAMP_MASK           = not (TIMESTAMP_INCREMENT-1); {removes timestamp flag bits}
{
 --------------------------------------------------------------------
 Global variables
 --------------------------------------------------------------------
}
var
  BoardTimestamps          : TBoardTimestamps; {global timestamp area for tasks like 'freeze test' and 'try moves'}
  PlayerPathBoardTimestamps: TBoardTimestamps; {global static calculation area for faster calculation of player path (with timestamping, it's unnecessary to initialize a board-sized area on each call)}
{ ScoreMetricsWeights      : TSecondaryScoreMetrics;}

implementation

uses
  SysUtils {for 'Format'}
  {$IFDEF SokobanYASC}
    ,Pack_      {for 'MakeIniFileSectionFileName'}
    ,Misc_      {for 'RectToStr'}
    ,Main_      {for logging solutions}
    ,Snapshots_ {for snapshot type names}
    ,MView_     {for multiple view items}
  {$ENDIF}
  ;

{
 --------------------------------------------------------------------
 Private variables
 --------------------------------------------------------------------
}

{
 --------------------------------------------------------------------
 Miscellaneous
 --------------------------------------------------------------------
}
const
  DEFAULT_COMBINED_MOVE_FORMATTING_ENABLED      = False; {'False': combined moves are optimized at load time by 'RenumberCombinedMoves'; hence, there is no reason to store the users own combined moves because this information is lost anyway next time the level is loaded}
  DEFAULT_PRETTY_PRINT_GAMES_ENABLED            = True;  {'True': exports games in fixed length lines; 'False': one single line only}
  DEFAULT_RESTORE_SAVE_GAME                     = False; {what to do with a saved game when a level is loaded}
  DEFAULT_RLE_ENABLED                           = False;
  DEFAULT_SCORE_METRICS_WEIGHT_BOX_CHANGES      = 10;
  DEFAULT_SCORE_METRICS_WEIGHT_BOX_LINES        = 100;
  DEFAULT_SCORE_METRICS_WEIGHT_PUSHING_SESSIONS = 1;
  DEFAULT_SCORE_METRICS_WEIGHT_PLAYER_LINES     = 0;
  DEFAULT_TIMING_ENABLED                        = False; {'True': measure time spent on each level}
  DEFAULT_TIMING_IDLE_TIME_THRESHOLD_MS         = 15000; {idle time threshold, milli-seconds}
  PUSH_OR_PULL_SIGN                             : array[Boolean] of Integer = (1,-1); {False = push =1; True = pull =-1}


function SecondaryMetricsFormattedAsATitleSuffix(SecondaryMetricsInTitles__:Boolean; const SecondaryScoreMetrics__:TSecondaryScoreMetrics):String;
begin
  if   SecondaryMetricsInTitles__ then with SecondaryScoreMetrics__ do
       Result:=Format('/%d/%d/%d/%d',[BoxLines,BoxChanges,PushingSessions,PlayerLines])
  else Result:='';
end;
{
procedure SetDefaultScoreMetricsWeights;
begin
  with ScoreMetricsWeights do begin
    BoxChanges     :=DEFAULT_SCORE_METRICS_WEIGHT_BOX_CHANGES;
    BoxLines       :=DEFAULT_SCORE_METRICS_WEIGHT_BOX_LINES;
    PushingSessions:=DEFAULT_SCORE_METRICS_WEIGHT_PUSHING_SESSIONS;
    PlayerLines    :=DEFAULT_SCORE_METRICS_WEIGHT_PLAYER_LINES;
    end;
end;
}
{
 --------------------------------------------------------------------
 Sokoban engine
 --------------------------------------------------------------------
}

constructor TSokoGame.Create; {throws EOutOfMemory}
begin
  Inherited;
  Notes:=nil; Snapshots:=nil;
  SokoFile:=nil; {note that 'SokoFile' is not owned by 'TSokoGame'}
  BestSolutionMoves:=nil; BestSolutionPushes:=nil; SaveGame:=nil;
  BuiltInBestSolutionMoves:=nil; BuiltInBestSolutionPushes:=nil;
  Notes:=TNotes.Create; Snapshots:=TList.Create;
  PrettyPrintGamesEnabled:=DEFAULT_PRETTY_PRINT_GAMES_ENABLED;
  RleEnabled:=DEFAULT_RLE_ENABLED;
  CombinedMoveFormattingEnabled:=DEFAULT_COMBINED_MOVE_FORMATTING_ENABLED;
  TimingEnabled:=DEFAULT_TIMING_ENABLED;
  TimingIdleTimeThresholdEnabled:=True;
  TimingIdleTimeThresholdMS:=DEFAULT_TIMING_IDLE_TIME_Threshold_MS;
  JumpsAllowedAfterFirstBoxMoveInReverseMode:=False;
  IsLoading:=False; Verbose:=True;
  SaveBestSolutionsAutomatically:=True;
  SaveOldSolutionsAfterFindingBetterOnes:=True;
  SaveSnapshotsAutomatically:=True;
  SecondaryMetricsInTitles:=False;
  ShowBoxStartPositionsAsGoalsInReverseMode:=True;
  SolutionsRequireAtLeastOnePush:=True;
  SimpleIllegalMovesMask:=SIMPLE_ILLEGAL_MOVES_MASK;
  PathFindingMaxTimeMS:=DEFAULT_PATH_FINDING_MAX_TIME_MS; PathFindingOptimizeMoves:=False;
  RestoreSaveGame:=DEFAULT_RESTORE_SAVE_GAME; {if 'True' this switch has preceedence over 'ResetSaveGameAndLoadIf...'}
  ResetSaveGameAndLoadItIfItIsANormalModeGame:=True;
  Clear;
end; {TSokoGame.Create}

destructor TSokoGame.Destroy;
begin
  CloseLevel(True); Clear; Notes.Free; Snapshots.Free; {note that 'SokoFile' is not owned by 'TSokoGame'}
  Inherited;
end; {TSokoGame.Destroy}

function TSokoGame.AppendReverseModeSolution(Count__:Integer; Moves__:PHistoryMoves;
                                             LastMoveIndex__:Integer;
                                             var History__:THistory):Boolean;
var i,j:Integer; Move:THistoryMove; Snapshot:TSnapshot;
begin {Returns the concatenation of moves in 'History__' and 'Moves__';}
      {upon entry, 'History__' contains leading normal mode moves, if any;}
      {'Moves__' contains reverse mode moves;}
      {the function fails if there are too many moves}
  Result:=True;
  j:=Succ(Count__);
  for i:=Count__ downto 1 do {initial non-pulling moves in reverse mode are not used in the normal version}
      if (Moves__[i] and H_FLAG_BOX)<>0 then j:=i;
  for i:=Count__ downto j do begin {append the moves in reverse order}
      Move:=Moves__^[i];
      if (i<Count__) and
         ((Move and H_FLAG_ODD)<>(Moves__^[Succ(i)] and H_FLAG_ODD)) then
         LastMoveIndex__:=History__.Count; {new combined move group}
      if History__.Count<MAX_MOVES then begin
         Inc(History__.Count);
         History__.Moves[History__.Count]:=
           Ord(OPPOSITE_DIRECTION[TDirection(Move and H_MASK_DIRECTION)])+
           (Move and H_FLAG_BOX)+
           (H_FLAG_ODD and (not History__.Moves[LastMoveIndex__])); {opposite odd-flag than previous (combined) move}
         end
      else begin
         Result:=False; break; {too many moves}
         end;
      end;

  if Result then with History__ do begin
     Top:=Count; PushCount:=0; LastPushIndex:=0; LastBoxNo:=0; PlayerLinesCount:=0;
     Snapshot:=MakeSnapshot0(SPACE);            {save the current game state}
     if   Snapshot<>nil then                    {replay the new moves in order to calculate the proper history information}
          try     Count        :=0;
                  History      :=History__;
                  SetReverseMode(False);
                  LoadBoard(StartBoard);

                  while Result and (History.Count<History.Top) do
                    Result:=Redo0(True);

                  {the exact inverse of a reverse mode game has combined moves with the box-move first;}
                  {this looks very confusing when replayed, hence, the combined moves are renumbered}
                  {so non-box-moves are attached to the following box-move}
                  RenumberCombinedMoves;

                  History__    :=History;       {get the calculated history information}

          finally LoadSnapshot0(Snapshot);      {restore the current game state}
                  Snapshot.Free;
          end
     else Result:=False;
     end;
end; {TSokoGame.AppendReverseModeSolution}

function TSokoGame.BestSolutionsCount:Integer;
begin
  Result:=0;
  if BestSolutionMoves <>nil then Inc(Result);
  if BestSolutionPushes<>nil then Inc(Result);
end; {TSokoGame.BestSolutionsCount}

function TSokoGame.BoxLegalMoves(Col,Row:Integer; var TimeOut:Boolean):Integer;
  {Returns number of reachable squares, including current box position;}
  {On timeout, the true set of reachable squares may include more elements than calculated}
  {The board is updated with the 'BOX_LEGAL_MOVE' flag}

  {Contrary to 'BoxPath', finding reachable squares doesn't need}
  {to calculate exact distances,}
  {hence, the calculation can use a simpler and faster search}
const
  INFINITY=(MaxInt-2*(MAX_MOVES+1)) div 2; {avoid overflow when adding 2 path-lengths}
type
  TStackItem=packed record BoxPos,PlayerPos:TColRow; end;
var
  i,j,X,Y,dx,dy,BoxNo,BoardSize,Index,MoveCount,PushOrPullSign:Integer;
  Direction:TDirection; {StartTimeMS:TTimeMS;}
  OriginalPlayerPos,PlayerFromSquare,PlayerToSquare,BoxFromSquare,BoxToSquare:TColRow;
  StackBottom,StackTop:^TStackItem;
  StackItems:array[0..MAX_BOARD_SIZE*NUMBER_OF_DIRECTIONS+1+NUMBER_OF_DIRECTIONS] of TStackItem;
  Visited   :array[0..MAX_BOARD_SIZE*NUMBER_OF_DIRECTIONS] of Boolean; {marks visited [col,row,direction] combinations, where [col,row] is the boxsquare, and [direction] is the side of the box where the player is positioned}

begin {BoxLegalMoves}
  Result:=0; {StartTimeMS:=GetTickCount;}
  BoxNo:=Board[Col,Row] shr BOARD_FLAG_COUNT; TimeOut:=False;
  if (BoxNo>0) and (BoxNo<=BoxCount) and
     ((Board[PlayerPos.x,PlayerPos.y] and (WALL+BOX))=0) then begin
     CalculatePathFindingTimeInterval;

{    PathFindingTimeInterval.StopTimeMS:=High(PathFindingTimeInterval.StopTimeMS);   {for testing}

     PlayerLegalMoves(0,0,i,i);                                {calculate the set of squares reachable for the player in current position}

     BoardSize:=BoardWidth*BoardHeight;
     FillChar(Visited,BoardSize*NUMBER_OF_DIRECTIONS,0);       {clear visited positions}
     OriginalPlayerPos:=PlayerPos;
     X:=BoxPos[BoxNo].x; Y:=BoxPos[BoxNo].y;
     Dec(Board[X,Y],BOX);                                      {remove the box from the board}
     PushOrPullSign:=PUSH_OR_PULL_SIGN[ReverseMode];           {move direction}
     StackBottom:=Addr(StackItems[Low(StackItems)]); StackTop:=StackBottom; {'StackTop' points to next unused item}

     {put legal start positions on the stack}
     for Direction:=Low(Direction) to High(Direction) do begin {for each direction...}
         StackTop^.PlayerPos.X:=X+DIRECTION_XY[Direction,ColAxis];
         StackTop^.PlayerPos.Y:=Y+DIRECTION_XY[Direction,RowAxis];
         with StackTop^ do
           if   (Board[PlayerPos.X,PlayerPos.Y] and PLAYER_LEGAL_MOVE)<>0 then begin {the player can reach the neighbor square}
                BoxPos.X:=X; BoxPos.Y:=Y; {each stack item contains the box position and the neighboring player position}
                {don't update 'Visited' for the root moves; it doesn't hurt performance visiting them twice, so there is no need to figure out from which direction the square was visited}
                Inc(StackTop); {increment the stackpointer so it points to next unused item}
                end;
         end;

     while (StackBottom<>StackTop) and (GetTickCount<PathFindingTimeInterval.StopTimeMS) do begin
       Dec(StackTop);
       {generate all legal moves for the box starting with the player and the box positioned according to this stack-item}
       BoxFromSquare:=StackTop^.BoxPos; PlayerPos:=StackTop^.PlayerPos;
       for Direction:=Low(Direction) to High(Direction) do begin                {try each direction}
           dx                :=PushOrPullSign    *DIRECTION_XY[Direction,ColAxis];
           dy                :=PushOrPullSign    *DIRECTION_XY[Direction,RowAxis];
           PlayerFromSquare.X:=BoxFromSquare   .X-DIRECTION_XY[Direction,ColAxis];    {the player starts here}
           PlayerFromSquare.Y:=BoxFromSquare   .Y-DIRECTION_XY[Direction,RowAxis];
           PlayerToSquare  .X:=PlayerFromSquare.X+dx;                           {the player ends here}
           PlayerToSquare  .Y:=PlayerFromSquare.Y+dy;
           BoxToSquare     .X:=BoxFromSquare   .X+dx;                           {the box ends here}
           BoxToSquare     .Y:=BoxFromSquare   .Y+dy;

           Inc(Board[BoxFromSquare.X,BoxFromSquare.Y],BOX);                     {put the box on the board}

           Index:=Pred(BoxToSquare.X)+Pred(BoxToSquare.Y)*BoardWidth+Ord(Direction)*BoardSize; {columns and rows (X,Y) are 1-based, hence 'Pred' is used for making them 0-based}
           if (not Visited[Index]) and                                          {is it an unvisited square, from this direction?}
              ((Board[PlayerFromSquare.X,PlayerFromSquare.Y] and (BOX+WALL))=0) and {is the square to push from free?}
              ((Board[BoxToSquare     .X,BoxToSquare     .Y] and (BOX+WALL+SimpleIllegalMovesMask))=0) and {is 'Box-To-Square' free and legal?}
              ((PushOrPullSign=1)                                               {pushing}
               or                                                               {pulling: is 'Player-To-Square' free?}
               ((Board[PlayerToSquare .X,PlayerToSquare  .Y] and (BOX+WALL))=0)
              ) and
              PlayerPathLength(PlayerPos,PlayerFromSquare,MoveCount) then begin {can the player reach 'Push from Square' from its current position next to the box?}
              Visited[Index]:=True;                                             {mark that the square has been visited from this direction}
              with StackTop^ do begin                                           {put the new boxposition/playerposition on the stack}
                BoxPos:=BoxToSquare; PlayerPos:=PlayerToSquare;
                Inc(StackTop);                                                  {increment the stackpointer so it points to next unused item}
                end;
              end;

             Dec(Board[BoxFromSquare.X,BoxFromSquare.Y],BOX);                   {remove the box from the board again}
             end;
       end;

     for i:=1 to BoardWidth do
         for j:=1 to BoardHeight do begin
             Board[i,j]:=Board[i,j] and (not BOX_LEGAL_MOVE);
             for Direction:=Low(Direction) to High(Direction) do
                 if Visited[Pred(i)+Pred(j)*BoardWidth+Ord(Direction)*BoardSize] then begin
                    if (SimpleIllegalMovesMask=0) or
                       (not IsAFreezingMove(X,Y,i,j,PlayerPos.X,PlayerPos.Y)) then begin
                       Inc(Board[i,j],BOX_LEGAL_MOVE);
                       Inc(Result);
                       end;
                    break;                                                      {quick and dirty: leave the 'for Direction...' loop and continue to next row/column}
                    end;
             end;

     if (Board[X,Y] and BOX_LEGAL_MOVE)=0 then begin                            {ensure that the current position is included in the set}
        Inc(Board[X,Y],BOX_LEGAL_MOVE);
        Inc(Result);
        end;

     Inc(Board[X,Y],BOX);                                                       {put the box on the board again}
     Self.PlayerPos:=OriginalPlayerPos;
     TimeOut:=StackTop<>StackBottom;

     {Msg(IntToStr(CalculateElapsedTimeMS(StartTimeMS,GetTickCount))+SPACE+IntToStr(Col)+'-'+IntToStr(Row),
         IntToStr(Result),MB_OK);} {for testing}

     end;
end; {TSokoGame.BoxLegalMoves}

function TSokoGame.BoxPath(StartPos__,EndPos__,PlayerPos__:TColRow;
                           var MoveCount__:Integer; var Moves__:TMoves):Boolean;

{the function uses a large per-square-per-direction area, and there is a risk of
 stack overflow if the function is called from other functions which also
 allocate large temporary arrays on the stack;
 defining 'SIXTY_FIVE_KIBI_MOVES_LIMIT' enforces a 65535 moves limit, so
 moves and pushes can be stored in 2-byte unsigned integers, thereby reducing
 the number of stack allocated bytes;
}
{///$DEFINE SIXTY_FIVE_KIBI_MOVES_LIMIT}
type
  TScore=Int64; {the score function result values may overflow a 32-bit integer if the maximum board size is, say, 100x100 or more}
  TQueueItem =packed record BoxPos:TColRow; Dir:TDirection end;
{$IFDEF SIXTY_FIVE_KIBI_MOVES_LIMIT}
  type
    TSquareItem=packed record Moves,ParentQueueIndex,Pushes,QueueIndex:UInt16; end; {'QueueIndex': if a new better path is found to this square then all later discovered squares must be re-examined}
{$ELSE}
  const
    INFINITY=(MaxInt-2*(MAX_MOVES+1)) div 2;                                    {avoid overflow when adding 2 path-lengths}
  type
    TSquareItem=packed record Moves:Integer; ParentQueueIndex:UInt16; Pushes:Integer; QueueIndex:UInt16; end; {'QueueIndex': if a new better path is found to this square then all later discovered squares must be re-examined}
{$ENDIF}

{///$DEFINE FULLY_EXPAND_NON_PUSHING_PLAYER_MOVES_BETWEEN_BOX_PUSHES}           {when enabled, all non-pushing in-between player moves are generated and inserted between the pushes}
var
  BestLastMoveQueueIndex,BoardSize,BoxNo, dx,dy,i,
  Index,NextIndex,__ParentQueueIndex,ParentSquareIndex,PlayerMoveCount,
  PushOrPullSign,QueueBottom,QueueTop:Integer;
  BoxPushesWeight,PlayerMovesWeight,BestResult,Score:TScore;
  TimeMS:TTimeMS;
  Direction,PlayerDirection:TDirection;
  OriginalPlayerPos,PlayerFromSquare,PlayerToSquare,BoxFromSquare,BoxToSquare:TColRow;
  QueueItems :array[0..MAX_BOARD_SIZE*NUMBER_OF_DIRECTIONS+1] of TQueueItem;    {each queue item represents a box-push, saved as a [col,row,direction] tuple}
  SquareItems:array[0..MAX_BOARD_SIZE*NUMBER_OF_DIRECTIONS+1] of TSquareItem;
{$IFDEF FULLY_EXPAND_NON_PUSHING_PLAYER_MOVES_BETWEEN_BOX_PUSHES}
  PlayerLinesCount:Integer; IsPreferredDirectionOK:Boolean;
{$ENDIF}
begin {BoxPath}
  TimeMS:=GetTimeMS;
  BoxNo:=Board[StartPos__.X,StartPos__.Y] shr BOARD_FLAG_COUNT;                 {find box on the square, if any}
  Result:=(BoxNo<>0) and                                                        {for safety: 'StartPos' must contain a box}
          ((StartPos__.X<>EndPos__.X) or (StartPos__.Y<>EndPos__.Y)) and        {start=destination is regarded as 'False'; otherwise the algorithm needs modifications}
          (BoxPos[BoxNo].X=StartPos__.X) and (BoxPos[BoxNo].Y=StartPos__.Y);    {the box position must match, otherwise the data is out of sync}
  if Result then begin
     BestResult:=High(BestResult); BestLastMoveQueueIndex:=0;                   {'High(BestResult)': best result found so far: none}
     if PathFindingOptimizeMoves then begin
        BoxPushesWeight  :=1;
        PlayerMovesWeight:=MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT+1;                  {optimize moves : non-pushing player-moves weight > maximum box-pushes}
        end
     else begin
        BoxPushesWeight  :=MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT+1;                  {optimize pushes: box-pushing moves weight > maximum intermediate non-box-moves}
        PlayerMovesWeight:=1;
        end;

     BoardSize:=BoardWidth*BoardHeight;
     for Index:=0 to BoardSize*NUMBER_OF_DIRECTIONS do with SquareItems[Index] do begin {mark all squares/directions as unvisited}
         {$IFDEF SIXTY_FIVE_KIBI_MOVES_LIMIT}
           Moves:=High(Moves); Pushes:=High(Pushes);
         {$ELSE}
           Moves:=INFINITY; Pushes:=INFINITY;
         {$ENDIF}
         ParentQueueIndex:=0; QueueIndex:=0;
         end;

     OriginalPlayerPos:=PlayerPos;
     PushOrPullSign:=PUSH_OR_PULL_SIGN[ReverseMode];                            {'1' or '-1' depending on normal/reverse mode play}
     QueueBottom:=Low(QueueItems); QueueTop:=QueueBottom;                       {'QueueTop' points to last used item}

     CalculatePathFindingTimeInterval;                                          {calculate time limit}
{    PathFindingTimeInterval.StopTimeMS:=High(PathFindingTimeInterval.StopTimeMS);} {for testing}

     CalculatePlayersDistanceToAllReachableSquares(PlayerPos);                  {calculate player's distance to all reachable squares; the results can be found in 'PlayerPathBoardTimestamps' as '(squarevalue - timestamp) div TIMESTAMP_INCREMENT'}

     {put legal starting positions on the queue}
     for Direction:=Low(Direction) to High(Direction) do with PlayerFromSquare do begin {for each direction...}
         X:=StartPos__.X-DIRECTION_XY[Direction,ColAxis];                       {note the negative sign; this way, the position with the box at 'StartPos__' can be treated as the result of pushing the box in direction 'Direction'}
         Y:=StartPos__.Y-DIRECTION_XY[Direction,RowAxis];
         if PlayerPathBoardTimestamps.Squares[X,Y]>=PlayerPathBoardTimestamps.Timestamp then begin {'True': the player can reach the neighbor square}
            Inc(QueueTop);
            with QueueItems[QueueTop] do begin BoxPos:=StartPos__; Dir:=Direction; end;
            with SquareItems[Pred(StartPos__.X)+Pred(StartPos__.Y)*BoardWidth+Ord(Direction)*BoardSize] do begin {'Pred(..) + Pred(...) * BoardWidth + ...': maps 3-dimensional [width, height, direction] to a 1-dimensional index}
              Moves     :=(PlayerPathBoardTimestamps.Squares[X,Y]-PlayerPathBoardTimestamps.Timestamp) div TIMESTAMP_INCREMENT; {the player's distance is stored in the timestamp array as '(squarevalue - timestamp) div TIMESTAMP_INCREMENT'}
              Pushes    :=0;
              QueueIndex:=QueueTop;
              end;
            end;
         end;

     Dec(Board[StartPos__.X,StartPos__.Y],BOX);                                 {remove the box from the board}

     {
     expand the nodes on the queue, mostly as a breadth-first search (BFS);

     expanding the nodes on the queue in strict FIFO order (first-in-first-out)
     amounts to a breadth-first search;

     the nodes are [box square x direction] tuples, and expanding a node visits
     all its 1-push neighbor squares for a forward path search, or all its
     1-pull neighbor squares for a reverse mode path search;

     the first found path to a node is not necessarily the best one for two
     reasons:

     * finding a push-optimal path uses the number of moves as tiebreaker;

     * finding a move-optimal path not only uses the number of pushes as
       tiebreaker, but the first found path may not be the best move-optimal
       path because the nodes are still expanded one push/pull at a time here;

     if a position has been expanded, and a new better path to the position is
     found later, then all its successors on the queue have too high scores,
     hence, they must be reexamined; this is accomplished by adjusting the
     'QueueBottom' pointer so the position again is the next node to be
     expanded, in effect dropping all nodes from the queue added later than the
     position with the new better path;

     this means the search isn't a pure breadth-first search, but in practice
     reexamination isn't required so often that the search becomes too slow;

     however, a time limit is imposed to ensure that the search doesn't take too
     long time; the box path finder is only used for supporting drag-and-drop a
     box with the mouse, and if the search fails because of a time limit, the
     only consequence is that the user must find another way to perform the
     pushes, for instance by breaking the sequence up in 2 or more sequences;
     }

     while (QueueBottom<>QueueTop) and (GetTickCount<PathFindingTimeInterval.StopTimeMS) do begin  {while there are more un-expanded squares/directions, and while the time limit hasn't been exceeded...}
       Inc(QueueBottom);                                                        {points to currently processed square}
       {generate all legal moves for the box starting with the player and the box positioned according to this queue-item}
       __ParentQueueIndex       :=QueueBottom;                                  {remember the parent queue index; 'QueueBottom' may be adjusted before all successors have been generated so it cannot be trusted to hold the parent queue index}
       BoxFromSquare            :=QueueItems[QueueBottom].BoxPos;
       PlayerDirection          :=QueueItems[QueueBottom].Dir;
       PlayerPos.X              :=BoxFromSquare.X-DIRECTION_XY[PlayerDirection,ColAxis]; {the player is located next to the box, in the opposite direction of 'PlayerDirection'}
       PlayerPos.Y              :=BoxFromSquare.Y-DIRECTION_XY[PlayerDirection,RowAxis];
       ParentSquareIndex        :=Pred(BoxFromSquare.X)+Pred(BoxFromSquare.Y)*BoardWidth+Ord(PlayerDirection)*BoardSize;
       Score                    :=Succ(SquareItems[ParentSquareIndex].Moves)*PlayerMovesWeight+Succ(SquareItems[ParentSquareIndex].Pushes)*BoxPushesWeight; {'Succ': the score after pushing the box one square away from its current position}

       Direction                :=PlayerDirection;                              {first try to continue in the current direction as an attempt to minimize the number of player lines}
       repeat                                                                   {try each direction}
              dx                :=PushOrPullSign    *DIRECTION_XY[Direction,ColAxis];
              dy                :=PushOrPullSign    *DIRECTION_XY[Direction,RowAxis];
              PlayerFromSquare.X:=BoxFromSquare   .X-DIRECTION_XY[Direction,ColAxis]; {the player starts here}
              PlayerFromSquare.Y:=BoxFromSquare   .Y-DIRECTION_XY[Direction,RowAxis];
              PlayerToSquare  .X:=PlayerFromSquare.X+dx;                        {the player ends here}
              PlayerToSquare  .Y:=PlayerFromSquare.Y+dy;
              BoxToSquare     .X:=BoxFromSquare   .X+dx;                        {the box ends here}
              BoxToSquare     .Y:=BoxFromSquare   .Y+dy;
              Index             :=Pred(BoxToSquare.X)+Pred(BoxToSquare.Y)*BoardWidth+Ord(Direction)*BoardSize; {maps 3-dimensional [width, height, direction] to a 1-dimensional index}

              Inc(Board[BoxFromSquare.X,BoxFromSquare.Y],BOX);                  {put the box on the board}

              with SquareItems[Index] do
                if ((Board[PlayerFromSquare.x,PlayerFromSquare.y] and (BOX+WALL))=0) and         {is the square to push from free?}
                   ((Board[BoxToSquare     .x,BoxToSquare     .y] and (BOX+WALL+SimpleIllegalMovesMask))=0) and {is 'Box-To-Square' free and legal?}
                   ((PushOrPullSign=1)                                          {pushing}
                    or                                                          {pulling: is 'Player-To-Square' free?}
                    ((Board[PlayerToSquare .x,PlayerToSquare  .y] and (BOX+WALL))=0)
                   ) and
                   (Score<BestResult) and                                                        {can this path be shorter than best solution found so far? (for efficiency, filter out as many moves as possible before calling 'Self.PlayerPathLength')}
                   (Score<Moves*PlayerMovesWeight+Pushes*BoxPushesWeight) and                    {can this path be shorter than best path to the square from this direction found so far? (for efficiency, filter out as many moves as possible before calling 'Self.PlayerPathLength')}
                   Self.PlayerPathLength(PlayerPos,PlayerFromSquare,PlayerMoveCount) and         {can the player reach the square to push from?}
                   (High(Moves)-PlayerMoveCount>SquareItems[ParentSquareIndex].Moves) and        {can the player move to 'Player-From-Square' without overflowing the counter?}
                   (Score+PlayerMovesWeight*PlayerMoveCount<BestResult) and                      {is this path shorter than best solution found so far?}
                   (Score+PlayerMovesWeight*PlayerMoveCount<Moves*PlayerMovesWeight+Pushes*BoxPushesWeight) and {is this the best path to the square from this direction found so far?}
                   (High(Pushes)>SquareItems[ParentSquareIndex].Pushes)                          {can the box be pushed to its neighbor square without overflowing the counter?}
                   then begin                                                                    {save new best path to the square from this direction}
                   Moves           :=Succ(SquareItems[ParentSquareIndex].Moves+PlayerMoveCount); {'Succ': the box moves to its neighbor square}
                   Pushes          :=Succ(SquareItems[ParentSquareIndex].Pushes);                {'Succ': the box moves to its neighbor square}
                   ParentQueueIndex:=__ParentQueueIndex;                                         {remember the parent so it's possible to create a path if this move later turns out to be on the best path}
                   if        QueueIndex=0 then begin                                             {'True': this is the first time the square is visited from this direction}
                             Inc(QueueTop);
                             QueueIndex:=QueueTop;
                             with QueueItems[QueueTop] do begin BoxPos:=BoxToSquare; Dir:=Direction; end;
                             end
                   else if   QueueIndex<=QueueBottom then                       {'True': this position has already been expanded, hence, all positions discovered later than this position must be re-examined}
                             QueueBottom:=Pred(QueueIndex)                      {'Pred': upon entry, the 'while' loop advances to the next item}
                        else begin                                              {the position is on the queue, but it hasn't been expanded yet, hence, no special action is required}
                             end;

                   if   (BoxToSquare.X=EndPos__.X) and (BoxToSquare.Y=EndPos__.Y)  then begin  {'True': this is a new best path to the target position}
                        BestResult:=Moves*PlayerMovesWeight+Pushes*BoxPushesWeight;
                        BestLastMoveQueueIndex:=QueueIndex;
                        end;
                   end;

              Dec(Board[BoxFromSquare.X,BoxFromSquare.Y],BOX);                  {remove the box from the board again}

              Direction:=NEXT_DIRECTION[Direction];                             {get ready to try the next direction}
       until  Direction=PlayerDirection;                                        {until all directions have been explored}
       end;

     Result:=BestLastMoveQueueIndex<>0;                                         {'True': the search found a path to the target position; note that it's not necessarily optimal because the search may have terminated on a timeout}
     if Result then begin
        {the path to the target position is a backward path; reverse it so it's a forward path}
        Index:=BestLastMoveQueueIndex; NextIndex:=0;                            {'Squares[].ParentQueueIndex' is a backward path; reverse the best path to the target position}
        repeat with QueueItems[Index] do with BoxPos do                         {'Index' is a 'QueueItems' index; use it to calculate the corresponding index into the 'Squares' array}
                 with SquareItems[Pred(X)+Pred(Y)*BoardWidth+Ord(Dir)*BoardSize] do begin
                   i:=ParentQueueIndex;                                         {temporarily save the parent in 'i'}
                   ParentQueueIndex:=NextIndex;                                 {reverse the path}
                   end;
               NextIndex:=Index;
               Index:=i;                                                        {get ready to process the parent move}
        until  Index=0;

        {put the moves on the best found path on the 'Moves__' list}
        MoveCount__:=0; PlayerPos:=OriginalPlayerPos;                           {create the moves leading to the target position}

        with QueueItems[NextIndex] do with BoxPos do                            {skip the initial queue item; 'Index' is a 'QueueItems' index; use it to calculate the corresponding index into the 'Squares' array}
          Index:=SquareItems[Pred(X)+Pred(Y)*BoardWidth+Ord(Dir)*BoardSize].ParentQueueIndex; {despite the name, 'ParentQueueIndex' is really a forward path at this time}
        repeat Direction           :=QueueItems[Index].Dir;
               dx                  :=PushOrPullSign*DIRECTION_XY[Direction,ColAxis];
               dy                  :=PushOrPullSign*DIRECTION_XY[Direction,RowAxis];
               BoxToSquare         :=QueueItems[Index].BoxPos;
               BoxFromSquare   .X  :=BoxToSquare     .X-dx;                     {the box position before it was moved to its 'BoxToSquare'}
               BoxFromSquare   .Y  :=BoxToSquare     .Y-dy;
               PlayerFromSquare.X  :=BoxFromSquare   .X-DIRECTION_XY[Direction,ColAxis]; {the player position before the box was moved to 'BoxToSquare'}
               PlayerFromSquare.Y  :=BoxFromSquare   .Y-DIRECTION_XY[Direction,RowAxis];
               PlayerToSquare  .X  :=PlayerFromSquare.X+dx;
               PlayerToSquare  .Y  :=PlayerFromSquare.Y+dy;
               if (PlayerPos.X<>PlayerFromSquare.X) or (PlayerPos.Y<>PlayerFromSquare.Y) then begin
                  {insert non-pushing player-moves when the player isn't already}
                  {located at the correct neighbor square next to the box;}

                  {$IFDEF FULLY_EXPAND_NON_PUSHING_PLAYER_MOVES_BETWEEN_BOX_PUSHES}
                    Inc(Board[BoxFromSquare.X,BoxFromSquare.Y],BOX);            {put the box on the board before the player path is calculated}
                    Result:=PlayerPath(PlayerPos,PlayerFromSquare,True,True,Direction,PlayerMoveCount,PlayerLinesCount,IsPreferredDirectionOK,PPlayerMoves(Addr(Moves__[MoveCount__])))
                            and
                            (MoveCount__+PlayerMoveCount<=High(Moves__));
                    Dec(Board[BoxFromSquare.X,BoxFromSquare.Y],BOX);            {remove the box from the board again}
                    if   Result then
                         Inc(MoveCount__,PlayerMoveCount)
                    else raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,['TSokoGame.BoxPath']));
                  {$ELSE}
                    {note that the exact path for the player isn't calculated here;}
                    {it's only the position next to the box that is put on the list}
                    Inc(MoveCount__);
                    if MoveCount__<=High(Moves__) then Moves__[MoveCount__]:=PlayerFromSquare;
                  {$ENDIF}
                  end;
               Inc(MoveCount__);                                                {move the box from 'BoxFromSquare' to 'BoxToSquare'}
               if MoveCount__<=High(Moves__) then begin
                  Moves__[MoveCount__]:=PlayerToSquare;
                  Inc(Moves__[MoveCount__].X,MOVE_LIST_FLAG_BOX);
                  end;
               PlayerPos:=PlayerToSquare;                                       {update the player position after the move}

               with QueueItems[Index] do with BoxPos do                         {'Index' is a 'QueueItems' index; use it to calculate the corresponding index into the 'Squares' array}
                 Index:=SquareItems[Pred(X)+Pred(Y)*BoardWidth+Ord(Dir)*BoardSize].ParentQueueIndex; {despite the name, 'ParentQueueIndex' is really a forward path at this time}
        until  Index=0;                                                         {until all moves on the path have been processed}

        Result:=MoveCount__<=High(Moves__);                                     {'True': the moves don't overflow the move vector; due to the interleaved non-pushing player-moves, the path may overflow the 'Moves__' vector}
        end;

     Inc(Board[StartPos__.X,StartPos__.Y],BOX);                                 {put the box on the board again}
     Self.PlayerPos:=OriginalPlayerPos;                                         {restore player's position}

     Inc(TimeStatistics[1],CalculateElapsedTimeMS(TimeMS,GetTimeMS));           {update time statistics}
     {Msg('Time: '+IntToStr(CalculateElapsedTimeMS(TimeMS,GetTimeMS)),'Items: '+IntToStr(QueueTop),MB_OK);} {for testing}
     end;
end;

function TSokoGame.CalculateGameState:TGameState;
var i:Integer;
begin {CalculateGameState}
  if        BoxOnTargetCount=BoxCount           then
            if        BoxCount=0                then Result:=gsNull
            else if   (History.PushCount<>0) or
                      (not SolutionsRequireAtLeastOnePush) then
                      if   not ReverseMode      then Result:=gsSolved {normal mode game}
                      else if   ((Board[PlayerPos.x,PlayerPos.y] and PLAYER_LEGAL_MOVE_IN_START_POSITION)<>0) and
                                (not IsJumpFeatureUsedAfterFirstBoxMove(ReverseMode,History.Count,Addr(History.Moves),i)) then
                                                     Result:=gsSolved {the position is only solved if the jump-feature hasn't been used, except for an initial jump}
                           else                      Result:=gsPlay
                 else                                Result:=gsPlay {start position with all boxes on goals}
  else if   History.Count=MAX_MOVES             then Result:=gsStop
       else                                          Result:=gsPlay;
end; {TSokoGame.CalculateGameState}

procedure TSokoGame.CalculateInternalData;
var a,b,c,d,e,i,j,k,dx,dy,SquareUp,SquareDown,SquareLeft,SquareRight,IllegalCornerCount:Integer;
    IllegalCorners:array[0..MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT] of TColRow; TimeMS:TTimeMS;
begin
  IllegalCornerCount:=0; PlayerPos.x:=0; PlayerPos.y:=0;
  BoxCount:=0; GoalCount:=0; BoxOnTargetCount:=0;
  BoxPos[0].x:=0; BoxPos[0].y:=0;

  for j:=0 to MAX_BOARD_HEIGHT+1 do begin {fill unused squares with walls}
      Board[0,j]:=WALL; {left column}
      if   (j=0) or (j>BoardHeight) then k:=1
      else k:=Succ(BoardWidth);
      for i:=k to MAX_BOARD_WIDTH+1 do
          Board[i,j]:=WALL;
      end;

  for i:=1 to BoardWidth do
      for j:=1 to BoardHeight do begin
          k:=Board[i,j] and (WALL+BOX+GOAL+PLAYER);
          if (k and WALL)=0 then k:=k or FLOOR; {tentatively assume that a blank square is a floor, i.e., reachable for the player if all boxes are removed}
          Board[i,j]:=k; {reset flags and box number, if any}

          if (k and PLAYER)<>0 then
             if   PlayerPos.x=0 then with PlayerPos do begin x:=i; y:=j; end
             else Dec(Board[i,j],PLAYER); {ignore extra players}

          if (k and BOX)<>0 then {count boxes}
             if   BoxCount<MAX_BOXES then begin
                  Inc(BoxCount);
                  Inc(Board[i,j],(BoxCount shl BOARD_FLAG_COUNT)+BOX_START_POSITION);
                  BoxPos[BoxCount].x:=i;
                  BoxPos[BoxCount].y:=j;
                  end
             else Dec(Board[i,j],BOX); {ignore extra boxes}

          if (k and GOAL)<>0 then {count goals}
             if   GoalCount<MAX_BOXES then begin
                  Inc(GoalCount);
                  GoalPos[GoalCount].x:=i;
                  GoalPos[GoalCount].y:=j;
                  end
             else Dec(Board[i,j],GOAL); {ignore extra goals}

          if (Board[i,j] and (BOX+BoxTargetMaskForCalculations))=BOX+BoxTargetMaskForCalculations then
             Inc(BoxOnTargetCount);

          {check if 'Board[i,j]' is an illegal corner}
          SquareUp    :=Board[i+DIRECTION_XY[Up   ,ColAxis],j+DIRECTION_XY[Up   ,RowAxis]];
          SquareDown  :=Board[i+DIRECTION_XY[Down ,ColAxis],j+DIRECTION_XY[Down ,RowAxis]];
          SquareLeft  :=Board[i+DIRECTION_XY[Left ,ColAxis],j+DIRECTION_XY[Left ,RowAxis]];
          SquareRight :=Board[i+DIRECTION_XY[Right,ColAxis],j+DIRECTION_XY[Right,RowAxis]];
          if ((k and GOAL)=0)                                            {not a goal position}
             and
             ((k and WALL)=0)                                            {not a wall}
             and
             (((SquareUp    and WALL)<>0) or ((SquareDown and WALL)<>0)) {vertical   wall-neigbour}
             and
             (((SquareRight and WALL)<>0) or ((SquareLeft and WALL)<>0)) {horizontal wall-neighbor}
             then begin
             Inc(Board[i,j],ILLEGAL_SQUARE); {'Board[i,j]' is an illegal corner}
             Inc(IllegalCornerCount);
             with IllegalCorners[IllegalCornerCount] do begin x:=Lo(i); y:=Lo(j); end;
             end;
          end;

  if IllegalCornerCount<>0 then {'IllegalCorners' contains ... all illegal corners}
     for i:=1 to Pred(IllegalCornerCount) do with IllegalCorners[i] do
         for j:=Succ(i) to IllegalCornerCount do {for each pair of illegal corners...}
             if ((x=IllegalCorners[j].x) or (y =IllegalCorners[j].y)) then begin {corners on same column or same row}
                dx:=IllegalCorners[j].x-x;  dy:=IllegalCorners[j].y-y;
                k:=Pred(Max(Abs(dx),Abs(dy))); {k = number of squares BETWEEN the endpoints i and j}
                if k>0 then begin
                   if dx<>0 then dx:=dx div Abs(dx); {make dx,dy to directions, i.e., -1,0, or 1}
                   if dy<>0 then dy:=dy div Abs(dy);
                   a:=x; b:=y; c:=0; d:=0;
                   for e:=1 to k do begin {for each square between the endpoints i and j}
                       Inc(a,dx); Inc(b,dy);
                       if (Board[a,b] and (WALL+GOAL+ILLEGAL_SQUARE))=0 then begin
                          if (Board[a+dy,b+dx] and FLOOR)=0 then Inc(c); {the square on adjacent side 1 is blocked}
                          if (Board[a-dy,b-dx] and FLOOR)=0 then Inc(d); {the square on adjacent side 2 is blocked}
                          end
                       else break; {non-floor square or a goal: the direct line between the endpoints i and j is broken, or there is a goal square on the line}
                       end;
                   if (c=k) or (d=k) then {the line has no goals, and one of the adjacent sides are blocked, i.e., a box can never escape from the line}
                      for e:=1 to k do
                          Board[x+dx*e,y+dy*e]:=Board[x+dx*e,y+dy*e] or ILLEGAL_SQUARE;
                   end;
                end;

  PlayerLegalMoves(0,0,i,j); {find the set of squares the player can reach from the start-position}
  for i:=1 to BoardWidth do
      for j:=1 to BoardHeight do
          if   (Board[i,j] and PLAYER_LEGAL_MOVE)<>0 then
               Board[i,j]:=Board[i,j] or PLAYER_LEGAL_MOVE_IN_START_POSITION; {this flag is used for detecting a solution in a reverse-mode game}

  {find the floor squares;}
  {this is normally the set of squares the player can reach if all boxes are removed}
  {from the board, but for better level rendering of skins with floor images,}
  {all squares reachable by a simulated player on each box-square and on each}
  {goal-square are also considered floor squares - except exterior squares; they}
  {are treated as blank squares}
  for i:=1 to BoxCount do with BoxPos[i] do       {remove all boxes from the board}
      Board[x,y]:=Board[x,y] and (not BOX);

  FloorCount:=PlayerLegalMoves(0,0,i,j);          {find legal player moves on the empty board}

  for i:=1 to BoardWidth do
      for j:=1 to BoardHeight do                  {mark the reachable squares as proper floor-squares}
          if   (Board[i,j] and PLAYER_LEGAL_MOVE)<>0 then
               Board[i,j]:=(Board[i,j] or FLOOR)  {the player can reach the square, i.e., it's a proper floor-square}
          else Board[i,j]:=Board[i,j] and (not FLOOR);

  for i:=1 to BoxCount do with BoxPos[i] do
      Inc(FloorCount,PlayerLegalMoves(x,y,j,j));  {simulate a player on each box-square}

  for i:=1 to GoalCount do with GoalPos[i] do
      Inc(FloorCount,PlayerLegalMoves(x,y,j,j));  {simulate a player on each goal-square}

  for i:=1 to BoxCount do with BoxPos[i] do       {put all boxes on the board again}
      Board[x,y]:=Board[x,y] or BOX;

  k:=0;
  for i:=1 to BoardWidth do
      for j:=1 to BoardHeight do                  {mark "dead floors", i.e., squares rendered as floors but unreachable for the player}
          if   (Board[i,j] and (PLAYER_LEGAL_MOVE+FLOOR))=PLAYER_LEGAL_MOVE then begin {'True': the square is only reachable from a box or a goal, but not reachable for the player}
               Board [i,j]:=(Board[i,j] or (FLOOR+PLAYER_UNREACHABLE_FLOOR)) and (not PLAYER_LEGAL_MOVE);
               Inc(k);
               end
          else Board[i,j]:=Board [i,j] and (not PLAYER_LEGAL_MOVE);

  if k<>0 then begin
     {the board contains "dead floors" marked for being rendered as floors even}
     {though the player cannot reach them; find exterior unreachable squares, if any}
     k:=0;

     for i:=1 to BoardWidth do begin {check squares on top row and bottom row}
         if (Board[i,1] and (FLOOR+PLAYER_UNREACHABLE_FLOOR+PLAYER_LEGAL_MOVE))=FLOOR+PLAYER_UNREACHABLE_FLOOR then {check squares in the top row}
            Inc(k,PlayerLegalMoves(i,1,j,j)); {floodfill connected unreachable floor-squares, marking them with the 'PLAYER_LEGAL_MOVE' flag, starting from the selected square}
         if (Board[i,BoardHeight] and (FLOOR+PLAYER_UNREACHABLE_FLOOR+PLAYER_LEGAL_MOVE))=FLOOR+PLAYER_UNREACHABLE_FLOOR then {check squares in the bottom row}
            Inc(k,PlayerLegalMoves(i,BoardHeight,j,j)); {floodfill connected unreachable floor-squares, marking them with the 'PLAYER_LEGAL_MOVE' flag, starting from the selected square}
         end;
     for i:=1 to BoardHeight do begin {check squares on leftmost column and rightmost column}
         if (Board[1,i] and (FLOOR+PLAYER_UNREACHABLE_FLOOR+PLAYER_LEGAL_MOVE))=FLOOR+PLAYER_UNREACHABLE_FLOOR then {check squares in the leftmost column}
            Inc(k,PlayerLegalMoves(1,i,j,j)); {floodfill connected unreachable floor-squares, marking them with the 'PLAYER_LEGAL_MOVE' flag, starting from the selected square}
         if (Board[BoardWidth,i] and (FLOOR+PLAYER_UNREACHABLE_FLOOR+PLAYER_LEGAL_MOVE))=FLOOR+PLAYER_UNREACHABLE_FLOOR then {check squares in the rightmost column}
            Inc(k,PlayerLegalMoves(BoardWidth,i,j,j)); {floodfill connected unreachable floor-squares, marking them with the 'PLAYER_LEGAL_MOVE' flag, starting from the selected square}
         end;

     if k<>0 then begin {'True': the may be some exterior unreachable squares which should not be rendered as floors after all}
        for i:=1 to BoardWidth do
            for j:=1 to BoardHeight do
                if (Board[i,j] and (FLOOR+PLAYER_UNREACHABLE_FLOOR+PLAYER_LEGAL_MOVE))=(FLOOR+PLAYER_UNREACHABLE_FLOOR+PLAYER_LEGAL_MOVE) then begin
                   Dec(Board[i,j],  FLOOR+PLAYER_UNREACHABLE_FLOOR+PLAYER_LEGAL_MOVE);
                   Dec(FloorCount);
                   end;
        for i:=1 to BoardWidth do
            for j:=1 to BoardHeight do
                Board[i,j]:=Board[i,j] and (not PLAYER_LEGAL_MOVE); {clear board flags}
        end;
     end;

  StartBoard:=Board; StartBoardAsText:=''; {save initial position}
  StartBoxOnTargetCount:=BoxOnTargetCount;
  PlayerStartPos:=PlayerPos;
  PlayersReachableSquaresTimestampAfterLastPush:=0; {invalidate any calculated distances to the player's reachable squares}
end; {TSokoGame.CalculateInternalData}

function  TSokoGame.CalculateMovableBoxes:Integer;
var BoxNo,Col,Row:Integer; IsAFreezingMove:Boolean; Direction:TDirection; OriginalPlayerPos:TColRow;
begin {CalculateMovableBoxes}
  Result:=0; PlayerLegalMoves(0,0,Col,Row); OriginalPlayerPos:=PlayerPos;
  for Col:=1 to BoardWidth do
      for Row:=1 to BoardHeight do
          Board[Col,Row]:=Board[Col,Row] and (not (SQUARE_SET or BOX_SET_DEADLOCK));
  try
    for BoxNo:=1 to BoxCount do with BoxPos[BoxNo] do
        for Direction:=Low(Direction) to High(Direction) do begin
            PlayerPos.x:=x-DIRECTION_XY[Direction,ColAxis]; {move player to neighbor square}
            PlayerPos.y:=y-DIRECTION_XY[Direction,RowAxis];
            if ((Board[PlayerPos.x,PlayerPos.y] and PLAYER_LEGAL_MOVE)<>0)
               and
               (((not ReverseMode)
                 and
                 IsALegalMove(DIRECTION_XY[Direction,ColAxis],  DIRECTION_XY[Direction,RowAxis],0,IsAFreezingMove)
                )
                or
                (ReverseMode
                 and
                 IsALegalMove(-DIRECTION_XY[Direction,ColAxis],-DIRECTION_XY[Direction,RowAxis],0,IsAFreezingMove)
                )
               ) then begin
               Board[x,y]:=Board[x,y] or SQUARE_SET;
               Inc(Result);
               break;
               end;
            end;
  finally
    PlayerPos:=OriginalPlayerPos;
  end;
end; {TSokoGame.CalculateMovableBoxes}

procedure TSokoGame.CalculatePathFindingTimeInterval;
begin
  PathFindingTimeInterval:=CalculateTimeIntervalMS(PathFindingMaxTimeMS);
end; {CalculatePathFindingTimeInterval}

function  TSokoGame.CalculatePlayersDistanceToAllReachableSquares(PlayerPos__:TColRow):SokUtil_.TTimestamp;
var MoveCount,PlayerLinesCount:Integer; b:Boolean; UnreachableSquare:TColRow;
begin {Calculates the player's distance to all reachable squares and saves the distances in 'PlayerPathBoardTimestamps' as 'squarevalue - timestamp'; the function returns the timestamp used for this calculation}
  UnreachableSquare.X:=0; UnreachableSquare.Y:=0; {'PlayerPath' will never find a path to 'UnreachableSquare', hence, it doesn't terminate before it has calculated the distance to all the reachable squares}
  PlayerPath(PlayerPos__,UnreachableSquare,False,False,Low(TDirection),MoveCount,PlayerLinesCount,b,nil);
  Result:=PlayerPathBoardTimestamps.Timestamp;
end; {CalculatePlayersDistanceToAllReachableSquares}

function TSokoGame.CalculateSecondaryScoreMetrics(var SecondaryScoreMetrics__:TSecondaryScoreMetrics):Boolean;
var i,Move:Integer;
begin {Calculates the secondary metrics and returns them in 'SecondaryMetrics__'; the function return value is always 'True'}
  Result:=True;
  with SecondaryScoreMetrics__ do begin
    BoxChanges:=0; BoxLines:=0; PlayerLines:=0; PushingSessions:=0;
    History.LastPushIndex:=0; History.Moves[0]:=0;
    for i:=1 to History.Count do with History do begin
        Move:=Moves[i];

        if   (Move and H_FLAG_BOX)<>0 then begin
             if      (Moves[Pred(i)] and H_FLAG_BOX)=0 then Inc(PushingSessions);
             if      (Move and H_FLAG_BOX_CHANGE)<>0 then begin
                     Inc(BoxChanges); Inc(BoxLines);
                     end
             else if TDirection(Move and H_MASK_DIRECTION)<>
                     TDirection(Moves[LastPushIndex] and H_MASK_DIRECTION) then
                     Inc(BoxLines);
             LastPushIndex:=i;
             end;

        if   i>1 then begin
             if TDirection(Move and H_MASK_DIRECTION)<>
                TDirection(Moves[Pred(i)] and H_MASK_DIRECTION) then
                Inc(PlayerLines);
             end
        else PlayerLines:=1;
        end;
    //Result:=CalculateWeightedSecondaryScore(SecondaryScoreMetrics__);
    end
end; {TSokoGame.CalculateSecondaryScoreMetrics}

procedure TSokoGame.CalculateScoreAndState;
var i:Integer;
begin {Calculates the score from scratch, i.e., from current board position}
  BoxOnTargetCount:=0;
  for i:=1 to BoxCount do
      if (Board[BoxPos[i].x,BoxPos[i].y] and BoxTargetMaskForCalculations) <> 0 then
         Inc(BoxOnTargetCount);
  GameState:=CalculateGameState;
end; {TSokoGame.CalculateScoreAndState}
(*
function TSokoGame.CalculateWeightedSecondaryScore(const SecondaryScoreMetrics__:TSecondaryScoreMetrics):Integer;
begin
  with SecondaryScoreMetrics__ do
    Result:=BoxChanges     *ScoreMetricsWeights.BoxChanges +
            BoxLines       *ScoreMetricsWeights.BoxLines +
            PushingSessions*ScoreMetricsWeights.PushingSessions +
            PlayerLines    *ScoreMetricsWeights.PlayerLines;
end; {TSokoGame.CalculateWeightedSecondaryScore}
*)
procedure TSokoGame.Clear;
begin
  SetName(''); DisplayName:=''; SokoFileName:=''; Notes.Clear;
  ClearTimer; OriginalTimeMS:=0; LastSolutionWasTheFirstOne:=False;
  BoardTransformation2D:=t2DRotate0DegreesClockwise;
  IsLoading:=False; KeepOriginalSnapshotNames:=False;
  GameState:=gsNull; SetReverseMode(False);
  ClearBoard(Board); StartBoardAsText:='';
  BoardWidth:=3; BoardHeight:=3;
  FillChar(TimeStatistics,SizeOf(TimeStatistics),0);
  CalculateInternalData;
  FillChar(History,SizeOf(History),0);
  PlayersReachableSquaresTimestampAfterLastPush:=0;
  OriginalBestSolutionMovesName:='';
  OriginalBestSolutionPushesName:='';
  OriginalBoardTransformation2D:=BoardTransformation2D;
  {$IFDEF SokobanYASC}
    OriginalMultiViewCount:=0;
  {$ENDIF}
  {$IFDEF SokHtml}
    PushesLowerBound:=PUSHES_LOWER_BOUND_UNDEFINED;
  {$ENDIF}
  OriginalSnapshotCount:=0;
  DeleteSnapshot(BestSolutionMoves);
  DeleteSnapshot(BestSolutionPushes);
  DeleteSnapshot(SaveGame);
  DeleteSnapshot(BuiltInBestSolutionMoves);
  DeleteSnapshot(BuiltInBestSolutionPushes);
  DeleteAllSnapshots;
end; {TSokoGame.Clear}

procedure TSokoGame.ClearTimer;
begin
  StartTimeMS:=0; SessionTimeMS:=0;
end;

function TSokoGame.CloseLevel(Flush__:Boolean):Boolean;
var BestSolutionsModified,BoardTransformation2DModified,TimeModified,SnapshotsModified:Boolean; n:TNode;
    {$IFDEF SokobanYASC} View:TMultiViewItem; {$ENDIF}
begin {Closes the level after checking for pending updates}
  StopTimer;
  PlayersReachableSquaresTimestampAfterLastPush:=0; {invalidate any calculated distances to the player's reachable squares}

  if      (SokoFile=nil) or (SokoFileName='') then
          Result:=True
  else if (SokoFile<>nil) and
          SokoFile.Open(SokoFileName) then begin                    {note: the level is saved to the file 'SokoFileName', not necessarily identical to the currently opened file}
          BestSolutionsModified        :=False;                     {only save the level in case of changes}
          SnapshotsModified            :=False;
          TimeModified                 :=False;
          BoardTransformation2DModified:=BoardTransformation2D<>OriginalBoardTransformation2D;

          if SaveBestSolutionsAutomatically then
             BestSolutionsModified:=
               ((BestSolutionMoves <>nil)
                and
                (BestSolutionMoves .Modified
                 or
                 BestSolutionMoves.Notes.Modified
                 or
                 ((BuiltInBestSolutionMoves<>nil)
                  and
                  (not IsABetterSolutionMoves(BuiltInBestSolutionMoves.MoveCount,BuiltInBestSolutionMoves.PushCount,BuiltinBestSolutionMoves.SecondaryScoreMetrics))
                 )
                )
               )
               or
               ((BestSolutionPushes<>nil)
                and
                (BestSolutionPushes.Modified
                 or
                 BestSolutionPushes.Notes.Modified
                 or
                 ((BuiltInBestSolutionPushes<>nil)
                  and
                  (not IsABetterSolutionPushes(BuiltInBestSolutionPushes.MoveCount,BuiltInBestSolutionPushes.PushCount,BuiltinBestSolutionPushes.SecondaryScoreMetrics))
                 )
                )
               );

          if SaveSnapshotsAutomatically then begin
             if ((History.Count>ForcedInitialJumps)                 {check for new current game}
                 or
                 (SaveGame<>nil)
                )
                and
                ((SaveGame=nil)
                 or
                 ((History.Count<>SaveGame.MoveCount)
                  or
                  (SaveGame.MoveCount=0)                            {'0': saved game from last session: delete it}
                  or
                  (not IsEqualToCurrentGame(SaveGame)))) then
                 SnapshotsModified:=True;                           {a new current game}

             if (OriginalSnapshotCount<>Snapshots.Count) or         {old and new number of snapshots differ}
                {$IFDEF SokobanYASC}
                  ((Self=MainForm.Game) and Assigned(MainForm.MultiView) and (OriginalMultiViewCount<>MainForm.MultiView.Items.Count)) or {old and new number of multiple views differ}
                {$ENDIF}
                BoardTransformation2DModified then                  {if the game is updated then ensure that savegame is updated as well}
                SnapshotsModified:=True;

             if Notes.Modified then SnapshotsModified:=True;        {if 'Notes.Modified' is 'True' then it may be a new level imported from the clipboard, in which case the text versions of the snapshots must be recreated from the snapshot versions in internal format}

             n:=Snapshots.First;                                    {check for changed/new snapshots}
             while (n<>nil) and (not SnapshotsModified) do begin
               if (TSnapshot(n).MoveCount>TSnapshot(n).ForcedInitialJumps) and   {drop 0-move snapshots}
                  (TSnapshot(n).Modified or TSnapshot(n).Notes.Modified) then
                  SnapshotsModified:=True;
               n:=n.Next;
               end;

             {$IFDEF SokobanYASC}
               if (Self=MainForm.Game) and Assigned(MainForm.MultiView) then begin
                  View:=TMultiViewItem(MainForm.MultiView.Items.First);
                  while Assigned(View) and (not SnapshotsModified) do begin
                    if Assigned(View.Snapshot) and
                       (View.Snapshot.MoveCount>View.Snapshot.ForcedInitialJumps) and   {drop 0-move snapshots}
                       (View.Snapshot.Modified or View.Snapshot.Notes.Modified) then
                       SnapshotsModified:=True;
                    View:=TMultiViewItem(View.Next);
                    end;
                  end;
             {$ENDIF}
             end;

          if TimingEnabled and (SessionTimeMS<>0) then TimeModified:=True;

          Result:=(not (Notes.Modified or BestSolutionsModified or SnapshotsModified or TimeModified or BoardTransformation2DModified))
                  and
                  SokoFile.Levels.ItemExists(Name);

          if not Result then
             if SokoFile.Open(SokoFileName) then begin
                Result:=SaveToFile(SokoFile,BestSolutionsModified,SnapshotsModified,TimeModified,BoardTransformation2DModified,Flush__);
                if SokoFile.Name<>'' then SokoFileName:=SokoFile.Name; {the name may have changed}
                end
          end
       else
          Result:=False;
end; {TSokoGame.CloseLevel}

function TSokoGame.CopyTo(Destination__:TNode):Boolean;
begin
  Result:=SokUtil_.Error(Format(TEXT_INTERNAL_ERROR_FORMAT,[TEXT_NOT_IMPLEMENTED]),'TSokoGame.CopyTo');
end; {TSokoGame.CopyTo}

procedure TSokoGame.DeleteAllSnapshots;
var V:TSnapshot;
begin
  while not Snapshots.IsEmpty do begin
    V:=TSnapshot(Snapshots.Pop); DeleteSnapshot(V);
    end;
end; {TSokoGame.DeleteAllSnapshots}

procedure TSokoGame.DeleteSnapshot(var Snapshot__:TSnapshot);
{var Node:TNode;}
begin
  if Snapshot__<>nil then
     try     if   IsLoading then
                  {dethroned best solutions are the only snapshots deleted while loading a level; degrade them to normal snapshots}
                  DethroneBestSolution(Snapshot__)
             else {normal mode, i.e., not loading the level}
                  Snapshots.Remove(Snapshot__,True) {note: the snapshot doesn't need to be a list member to be destroyed}
     finally Snapshot__:=nil;
     end;
end; {TSokoGame.DeleteSnapshot}

procedure TSokoGame.DethroneBestSolution(var Snapshot__:TSnapshot);
begin
  if Snapshot__<>nil then begin
     if SaveOldSolutionsAfterFindingBetterOnes then begin
        {the dethroned best solution changes name and is added to the snapshot list}
        try    Snapshot__.SetName(Snapshots.MakeUniqueName(SnapshotTypeName(stSolution)+SPACE+
                                                           Format(FORMAT_MOVES_AND_PUSHES,
                                                                  [Snapshot__.MoveCount,Snapshot__.PushCount])+
                                                           SecondaryMetricsFormattedAsATitleSuffix(
                                                             SecondaryMetricsInTitles,
                                                             SnapShot__.SecondaryScoreMetrics)+
                                                           SolverNameText(SnapShot__.Name)
                                                           {+NonOptimalText}
                                                           ,
                                                           SnapshotTypeName(stSolution),
                                                           True));
        except on E:Exception do Error(E.Message,'TSokoGame.DeleteSnapshot');
        end;
        Snapshots.Push(TNode(Snapshot__));
        end
     else {delete dethroned best solution}
        Snapshots.Remove(Snapshot__,True); {note: the snapshot doesn't need to be a list member to be destroyed}

     Snapshot__:=nil; {for convenience and safety, clear the caller's reference to the dethroned best solution}
     end;
end; {TSokoGame.DethroneBestSolution}

procedure TSokoGame.DoBoardTransformation2D(Transformation2D__:TTransformation2D; TransformStartBoardOnly__:Boolean); {the transformation operation is relative to current transformation}
type
  TNewBoxNumbers=array[0..MAX_BOXES] of Integer;
  TNewColRows   =array[0..MAX_BOARD_WIDTH+1,0..MAX_BOARD_HEIGHT+1] of TColRow;
var
  i,OldBoardWidth,OldBoardHeight:Integer;
  OldPlayerPos:TColRow; OldBoard:TBoard; v:TSnapshot;
  NewBoxNumbers:TNewBoxNumbers; NewColRows:TNewColRows; NewDirections:TDirectionMap;
  {$IFDEF SokobanYASC} View:TMultiViewItem; {$ENDIF}

  procedure CalculateBoxPositions;
  var BoxNo,Col,Row:Integer;
  begin
    for Col:=1 to BoardWidth do
        for Row:=1 to BoardHeight do begin
            BoxNo:=Board[Col,Row] shr BOARD_FLAG_COUNT;
            if BoxNo<>0 then with BoxPos[BoxNo] do begin X:=Col; Y:=Row; end;
            end;
  end;

  procedure CalculateNewBoardDimensions(OldBoardWidth__,OldBoardHeight__:Integer; var NewBoardWidth__,NewBoardHeight__:Integer);
  begin
    NewBoardWidth__:=OldBoardWidth__; NewBoardHeight__:=OldBoardHeight__;
    if (BOARD_TRANSFORMATION_ANGLE[Transformation2D__]=90) or
       (BOARD_TRANSFORMATION_ANGLE[Transformation2D__]=270) then
       SwapIntegers(NewBoardWidth__,NewBoardHeight__);
  end;

  function  CalculateNewBoardTransformation2D(Transformation2D__:TTransformation2D):TBoardTransformation2D;
  begin {the transformation operation is relative to current transformation}
    Result:=Self.BoardTransformation2D;
    case Transformation2D__ of
      t2DRotate0DegreesClockwise                   :;
      t2DRotate90DegreesClockwise                  :Result:=BOARD_TRANSFORMATION_ROTATE_CLOCKWISE        [Result];
      t2DRotate180DegreesClockwise                 :Result:=BOARD_TRANSFORMATION_ROTATE_CLOCKWISE        [BOARD_TRANSFORMATION_ROTATE_CLOCKWISE        [Result]];
      t2DRotate270DegreesClockwise                 :Result:=BOARD_TRANSFORMATION_ROTATE_COUNTER_CLOCKWISE[Result];
      t2DRotate0DegreesClockwiseFlipHorizontally   :Result:=BOARD_TRANSFORMATION_FLIP_HORIZONTALLY       [Result];
      t2DRotate90DegreesClockwiseFlipHorizontally  :Result:=BOARD_TRANSFORMATION_FLIP_HORIZONTALLY       [BOARD_TRANSFORMATION_ROTATE_CLOCKWISE        [Result]];
      t2DRotate180DegreesClockwiseFlipHorizontally :Result:=BOARD_TRANSFORMATION_FLIP_HORIZONTALLY       [BOARD_TRANSFORMATION_ROTATE_CLOCKWISE        [BOARD_TRANSFORMATION_ROTATE_CLOCKWISE        [Result]]];
      t2DRotate270DegreesClockwiseFlipHorizontally :Result:=BOARD_TRANSFORMATION_FLIP_HORIZONTALLY       [BOARD_TRANSFORMATION_ROTATE_COUNTER_CLOCKWISE[Result]];
      t2DFlipVertically                            :Result:=BOARD_TRANSFORMATION_FLIP_VERTICALLY         [Result];
      t2DFlipHorizontally                          :Result:=BOARD_TRANSFORMATION_FLIP_HORIZONTALLY       [Result];
      else Error(Format(TEXT_INTERNAL_ERROR_FORMAT,['Unhandled board transformation']),'SokGame__.DoBoardTransformation2D.CalculateNewBoardTransformation2D');
    end; {case}
  end;

  procedure CalculateNewBoxNumbers(BoardWidth__,BoardHeight__:Integer; const Board__:TBoard; var NewBoxNumbers__:TNewBoxNumbers);
  var Col,Row,BoxNo,NewBoxNo:Integer;
  begin {boxes are numbered 1..BoxCount during a col-row walk through the squares (see 'CalculateInternalData')}
    NewBoxNumbers__[0]:=0; NewBoxNo:=0;
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do begin
            BoxNo:=Board__[Col,Row] shr BOARD_FLAG_COUNT;
            if BoxNo<>0 then begin
               Inc(NewBoxNo); NewBoxNumbers__[BoxNo]:=NewBoxNo;
               end;
            end;
  end;

  procedure CalculateNewColRows(Transformation2D__:TTransformation2D; BoardWidth__,BoardHeight__:Integer; var NewColRows__:TNewColRows);
  var Col,Row,NewCol,NewRow:Integer;
  begin
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do begin
            CalculateTransformation2D(Transformation2D__,Col,Row,BoardWidth__,BoardHeight__,NewCol,NewRow);
            NewColRows__[Col,Row].X:=NewCol;
            NewColRows__[Col,Row].Y:=NewRow;
            end;
  end;

  procedure TransformBoard(var Board__:TBoard);
  var i,Col,Row,BoxNo:Integer; OldBoard:TBoard;
  begin
    OldBoard:=Board__;
    for Col:=1 to OldBoardWidth do
        for Row:=1 to OldBoardHeight do with NewColRows[Col,Row] do begin
            i           :=OldBoard[Col,Row];
            BoxNo       :=i shr BOARD_FLAG_COUNT;
            Board__[X,Y]:={new box number, if any}
                          (NewBoxNumbers[BoxNo] shl BOARD_FLAG_COUNT)
                          +
                          {flags describing the game position from the board}
                          (i and BOARD_GAME_STATE_FLAGS_MASK)
                          +
                          {extra flags from the already transformed starting position}
                          (StartBoard[X,Y] and (BOARD_FLAGS_MASK-BOARD_GAME_STATE_FLAGS_MASK));
            end;
  end;

  procedure TransformBoxPositions(var BoxPos__:TBoxPositions);
  var BoxNo:Integer; OldBoxPos:TBoxPositions;
  begin
    OldBoxPos:=BoxPos__;
    for BoxNo:=1 to BoxCount do with NewColRows[OldBoxPos[BoxNo].X,OldBoxPos[BoxNo].Y] do begin
        BoxPos__[NewBoxNumbers[BoxNo]].X:=X;
        BoxPos__[NewBoxNumbers[BoxNo]].Y:=Y;
        end;
  end;

  procedure TransformMoves(MoveTop__:Integer; Moves__:PHistoryMoves);
  var i:Integer; Move:THistoryMove;
  begin
    if Moves__<>nil then
       for i:=1 to MoveTop__ do begin
           Move       :=Moves__^[i];
           Moves__^[i]:=(Move and (not H_MASK_DIRECTION)) or
                        Ord(NewDirections[TDirection(Move and H_MASK_DIRECTION)]);
           end;
  end;

  procedure TransformSnapshot(var Snapshot__:TSnapshot);
  begin
    if Snapshot__<>nil then with Snapshot__ do begin
       TransformBoxPositions(BoxPos);
       LastBoxNo:=NewBoxNumbers[LastBoxNo];
       TransformMoves(MoveTop,Moves);
       with NewColRows[PlayerPos.X,PlayerPos.Y] do begin
         PlayerPos.X:=X; PlayerPos.Y:=Y;
         end;
       MovesAsText:=''; // ensure that the textual representation of the snapshot is re-created the next time it is used
       end;
  end;

begin {DoBoardTransformation2D}
  if Transformation2D__<>t2DRotate0DegreesClockwise then begin    {the transformation operation is relative to the current transformation}
     OldBoardWidth:=BoardWidth; OldBoardHeight:=BoardHeight;      {save current state for later}
     OldBoard:=Board; OldPlayerPos:=PlayerPos;

     BoardTransformation2D:=CalculateNewBoardTransformation2D(Transformation2D__);
     CalculateNewBoardDimensions(OldBoardWidth,OldBoardHeight,BoardWidth,BoardHeight);
     CalculateNewColRows(Transformation2D__,OldBoardWidth,OldBoardHeight,NewColRows);
     CalculateTransformation2DDirections(True,Transformation2D__,NewDirections);

     for i:=0 to BoxCount do NewBoxNumbers[i]:=i;                 {map new box numbers to identical numbers before transforming the start position}
     TransformBoard(StartBoard);

     if TransformStartBoardOnly__ then begin
        StartBoardAsText:=''; {invalidate an existing textual representation of the board, if any}
        PlayersReachableSquaresTimestampAfterLastPush:=0; {invalidate any calculated distances to the player's reachable squares}
        FindPlayerPosition(BoardWidth,BoardHeight,StartBoard,PlayerStartPos); {find the starting position for the player after the transformation}
        end
     else begin
        {it's first possible to calculate new box numbers now that the starting position has been transformed}
        CalculateNewBoxNumbers(BoardWidth,BoardHeight,StartBoard,NewBoxNumbers);

        Board:=StartBoard; CalculateInternalData;                 {find boxes, calculate flags, etc.}

        Board:=OldBoard; TransformBoard(Board);                   {transform current board position}
        CalculateBoxPositions;                                    {update box positions so they match current position}
        if (OldPlayerPos.X<>0) and (OldPlayerPos.Y<>0) then
           with NewColRows[OldPlayerPos.X,OldPlayerPos.Y] do begin{transform current player position}
             PlayerPos.X:=X; PlayerPos.Y:=Y;
             end;
        TransformMoves(History.Top,Addr(History.Moves));          {transform game history}
        History.LastBoxNo:=NewBoxNumbers[History.LastBoxNo];

        TransformSnapshot(BuiltinBestSolutionMoves);              {transform solutions}
        TransformSnapshot(BuiltinBestSolutionPushes);
        TransformSnapshot(BestSolutionMoves);
        TransformSnapshot(BestSolutionPushes);
        TransformSnapshot(SaveGame);                              {transform savegame}

        v:=TSnapshot(Snapshots.First);                            {transform snapshots}
        while v<>nil do begin
          TransformSnapshot(v);
          v:=TSnapshot(v.Next);
          end;
        end;

        {$IFDEF SokobanYASC}
          if (Self=MainForm.Game) and Assigned(MainForm.MultiView) then begin
             View:=TMultiViewItem(MainForm.MultiView.Items.First);
             while Assigned(View) do begin
               if Assigned(View.Snapshot) then TransformSnapshot(View.Snapshot);
               View:=TMultiViewItem(View.Next);
               end;
             end;
        {$ENDIF}
     end;
end; {TSokoGame.DoBoardTransformation2D}

function TSokoGame.DoMoveUpdateBoard0(dx__,dy__,Flags__:Integer):Integer;{Precondition: (dx,dy) must be a legal move}
var i,x,y,BoxNo:Integer;                                                 {note: compare with 'DoMoveUpdateGame' to see the difference}
begin {Updates board, player position, box positions, and score ('BoxOnTargetCount')}
  Result:=0;                                                            {return value: moved box-number, if any}
  Dec(Board[PlayerPos.x,PlayerPos.y],PLAYER);                           {player moves away from current position}
  Inc(PlayerPos.x,dx__); Inc(PlayerPos.y,dy__);
  Inc(Board[PlayerPos.x,PlayerPos.y],PLAYER);                           {player moves to new position}
  if        not ReverseMode then                                        {normal mode game}
            i:=Board[PlayerPos.x,PlayerPos.y]
  else if   ((Flags__ and (H_FLAG_BOX+H_FLAG_JUMP))=H_FLAG_BOX) and     {in reverse mode, it is necessary to distinguish among jumps, pulls, and simple moves}
            ((Board[PlayerPos.x-dx__,PlayerPos.y-dy__] and SimpleIllegalMovesMask)=0) then
            i:=Board[PlayerPos.x-2*dx__,PlayerPos.y-2*dy__]
       else i:=0;
  if  (i and BOX)<>0 then begin
      if (i and BoxTargetMaskForCalculations)<>0 then
         Dec(BoxOnTargetCount);                                         {update if the box leaves a target-square}
      BoxNo:=i shr BOARD_FLAG_COUNT; x:=BoxPos[BoxNo].x; y:=BoxPos[BoxNo].y;
      i:=(BoxNo shl BOARD_FLAG_COUNT)+BOX;
      Dec(Board[x,y],i);                                                {box moves away from (x,y)}
      Inc(x,dx__); BoxPos[BoxNo].x:=x;
      Inc(y,dy__); BoxPos[BoxNo].y:=y;
      Inc(Board[x,y],i);                                                {box moves to new position}

      if (Board[x,y] and BoxTargetMaskForCalculations)<>0 then
         Inc(BoxOnTargetCount);                                         {the box is now on a target-square}

      Result:=BoxNo;
      end;
end; {TSokoGame.DoMoveUpdateBoard0}

function TSokoGame.DoMoveUpdateBoard(dx__,dy__,Flags__:Integer):Integer;{Precondition: (dx,dy) must be a legal move}
begin {Updates board, player position, box positions, and score ('BoxOnTargetCount')}
  {note: compare with 'DoMoveUpdateGame' to see the difference}
  Result:=DoMoveUpdateBoard0(dx__,dy__,Flags__);
end; {TSokoGame.DoMoveUpdateBoard}

procedure TSokoGame.DoMoveUpdateGame(dx__,dy__,LastMoveIndex__,Flags__:Integer); {Precondition: (dx,dy) must be a legal move}
var i,BoxNo:Integer; Direction:TDirection; Move:THistoryMove;           {note: compare with 'DoMoveUpdateBoard' to see the difference}
begin {Updates history, state, board, and best solutions}
  if DxDyToDirection(dx__,dy__,Direction) and
     (History.Count<MAX_MOVES) then with History do begin               {update history}

     if   not ReverseMode then                                          {normal mode game}
          BoxNo:=Board[PlayerPos.x+dx__,PlayerPos.y+dy__] shr BOARD_FLAG_COUNT {box, if any, at player's target-position}
     else if   ((Flags__ and (H_FLAG_JUMP+H_FLAG_BOX))=H_FLAG_BOX) and  {in reverse mode, it is necessary to distinguish between jumps, pulls, and simple moves}
               ((Board[PlayerPos.x,PlayerPos.y] and SimpleIllegalMovesMask)=0) then
               BoxNo:=Board[PlayerPos.x-dx__,PlayerPos.y-dy__] shr BOARD_FLAG_COUNT {box, if any, at pull-position}
          else begin BoxNo:=0; Flags__:=Flags__ and (not H_FLAG_BOX); end;

     Move:=Ord(Direction);

     if        BoxNo<>0 then begin
               Inc(PushCount); Inc(Move,H_FLAG_BOX);
               if ReverseMode and ((Moves[Count] and H_FLAG_JUMP)<>0) then begin
                  if LastMoveIndex__>=0 then LastMoveIndex__:=Count;    {ensure that jumps and normal moves don't belong to same combined move group}
                  end;

               if BoxNo<>LastBoxNo then begin
                  Inc(Move,H_FLAG_BOX_CHANGE);
                  LastBoxNo:=BoxNo;
                  end;

               LastPushIndex:=Succ(Count);
               end
     else if   ReverseMode and ((Flags__ and H_FLAG_JUMP)<>0) then begin
               Inc(Move,H_FLAG_JUMP);
               if LastMoveIndex__>=0 then begin                         {in reverse mode, successive jumps are joined in the same combined move group}
                  LastMoveIndex__:=Count;
                  while (LastMoveIndex__>ForcedInitialJumps) and
                        ((Moves[LastMoveIndex__] and H_FLAG_JUMP)<>0) do
                        Dec(LastMoveIndex__);
                  for i:=Succ(LastMoveIndex__) to Count do
                      Moves[i]:=(Moves[i] and (not H_MASK_MOVE_SEPARATOR)) or
                                (H_FLAG_ODD and (not Moves[LastMoveIndex__]));
                  end;
               end;

     if        Count>0 then
               if   Direction<>TDirection(Moves[Count] and H_MASK_DIRECTION) then
                    Inc(PlayerLinesCount)                               {update the number of player lines}
               else
     else      PlayerLinesCount:=1;                                     {the first move counts as a player line}

     Inc(Count);                                                        {update number of moves}

     if        (Top<Count) or
               (Direction<>TDirection(Moves[Count] and H_MASK_DIRECTION)) or
               ((Move         and (H_FLAG_JUMP+H_FLAG_BOX))<>
                (Moves[Count] and (H_FLAG_JUMP+H_FLAG_BOX))) then
               Top:=Count;                                              {it's a new move: clear moves taken back, if any}

     if        LastMoveIndex__>=0 then
               Moves[Count]:=Move or (H_FLAG_ODD and (not Moves[LastMoveIndex__])) {flip the odd-flag: this is all that it takes to separate combined moves}
     else      Moves[Count]:=Move or (Moves[Count] and H_MASK_MOVE_SEPARATOR);     {keep old combined move separator}

     DoMoveUpdateBoard(dx__,dy__,Flags__);                              {update board, player position, box positions, and score}
     end;

  GameState    :=CalculateGameState;

  if   GameState=gsSolved then begin
       StopTimer;
       TestForNewBestSolution;
       end
  else begin ElapsedTimeMS; {accumulates time}
             if StartTimeMS=0 then StartTimer;
       end;
end; {TSokoGame.DoMoveUpdateGame}

function TSokoGame.ElapsedTimeMS:TTimeMS;
var TimeMS,TimeNowMS:TTimeMS;
begin
  if StartTimeMS<>0 then begin
     TimeNowMS:=GetTickCount;
     if TimeNowMS=0 then TimeNowMS:=1; {'StartTimeMS' <>0 signals 'activated', thus, '0' is an invalid time}
     TimeMS:=CalculateElapsedTimeMS(StartTimeMS,TimeNowMS);
     if (TimeMS>TimingIdleTimeThresholdMS) and TimingIdleTimeThresholdEnabled then begin
        Inc(SessionTimeMS,TimingIdleTimeThresholdMS); StartTimeMS:=0; {'0': don't count the time twice by calling 'StopTimer' now}
        StopTimer;
        end
     else begin
        Inc(SessionTimeMS,TimeMS);
        StartTimeMS:=TimeNowMS;
        end;
     end;
  Result:=OriginalTimeMS+SessionTimeMS;
end;

function TSokoGame.GetStartBoardAsText:String;
begin
  if (StartBoardAsText='') {'StartBoardAsText' is calculated on demand only}
     and
     (BoardWidth>0)
     and
     (BoardHeight>0)
     then
     StartBoardAsText:=SokFile_.BoardToText(BoardWidth,BoardHeight,StartBoard,'');
  Result:=StartBoardAsText;
end;

function TSokoGame.IsABetterBuiltInSolutionAvailable(var BetterBuiltInSolutionMoves__,BetterBuiltInSolutionPushes__:TSnapshot):Boolean;
begin
  BetterBuiltInSolutionMoves__:=nil; BetterBuiltInSolutionPushes__:=nil;

  if BuiltInBestSolutionMoves <>nil then with BuiltInBestSolutionMoves  do
     if      IsABetterSolutionMoves (MoveCount,PushCount,SecondaryScoreMetrics) then
             BetterBuiltInSolutionMoves__:=BuiltInBestSolutionMoves
     else if IsABetterSolutionPushes(MoveCount,PushCount,SecondaryScoreMetrics) then
             BetterBuiltInSolutionPushes__:=BuiltInBestSolutionMoves;

  if BuiltInBestSolutionPushes<>nil then with BuiltInBestSolutionPushes do
     if      (BestSolutionMoves=nil) or
             IsABetterSolutionPushes(MoveCount,PushCount,SecondaryScoreMetrics) then
             BetterBuiltInSolutionPushes__:=BuiltInBestSolutionPushes;

  Result:=(BetterBuiltInSolutionMoves__ <>nil) or
          (BetterBuiltInSolutionPushes__<>nil);
end;

function TSokoGame.IsABetterSolutionMoves(Count__,PushCount__:Integer; const SecondaryScoreMetrics__:TSecondaryScoreMetrics):Boolean;
begin
  Result:=(BestSolutionMoves=nil)                          {'nil': no solution found yet, i.e., this is the first}
          or
          (Count__        <BestSolutionMoves.MoveCount)    {new solution with fewer moves}
          or
          ((Count__       =BestSolutionMoves.MoveCount)    {same number of moves}
           and
           ((PushCount__  <BestSolutionMoves.PushCount)    {fewer pushes}
            or
            ((PushCount__ =BestSolutionMoves.PushCount)
             and                                           {better secondary score}
             (CompareSecondaryScoreMetrics(SecondaryScoreMetrics__,BestSolutionMoves.SecondaryScoreMetrics)<0)
            )
           )
          );
end;

function TSokoGame.IsABetterSolutionPushes(Count__,PushCount__:Integer; const SecondaryScoreMetrics__:TSecondaryScoreMetrics):Boolean;
begin
  Result:=(BestSolutionMoves<>nil)
          and
          (((BestSolutionPushes=nil) and                   {'nil': no solution/pushes registered yet}
            ((PushCount__ <BestSolutionMoves.PushCount)    {new solution with fewer pushes than used by the best found solution/moves}
             or
             ((PushCount__=BestSolutionMoves.PushCount)
              and
              (Count__    <BestSolutionMoves.MoveCount)
              and                                          {new solution with the same number of pushes but a better weighted secondary score}
              (CompareSecondaryScoreMetrics(SecondaryScoreMetrics__,BestSolutionMoves.SecondaryScoreMetrics)<0)
             )
            )
           )
           or
           ((BestSolutionPushes<>nil) and
            (PushCount__  <BestSolutionPushes.PushCount))  {new solution with fewer pushes}
           or
           ((BestSolutionPushes<>nil) and
            (PushCount__  =BestSolutionPushes.PushCount) and
            ((Count__     <BestSolutionPushes.MoveCount)   {fewer moves}
             or
             ((Count__    =BestSolutionPushes.MoveCount)
              and                                          {better secondary score}
              (CompareSecondaryScoreMetrics(SecondaryScoreMetrics__,BestSolutionPushes.SecondaryScoreMetrics)<0)
             )
            )
           )
          );
end;

function  TSokoGame.IsAFreezingMove ( FromCol__ , FromRow__ , ToCol__ , ToRow__ , PlayerCol__ , PlayerRow__ : Integer ) : Boolean;
{returns 'True' if putting a box on [ 'ToCol__' , 'ToRow__' ] creates a
 deadlocked frozen position (optionally moving the box from
 [ 'FromCol__' , 'FromRow__' ]);
 if the function is used for testing if the box at [ 'ToCol__' , 'ToRow__' ] is
 a frozen box in the starting position, then there is no preceding push; in that
 case, pass a wall square as player position in 'PlayerCol__' and 'PlayerRow__';
}
var OriginalFromSquareValue : Integer; ABoxIsBlockedOnANonGoalSquare : Boolean;

  function  BoxIsBlockedAlongOneAxis( Col__, Row__ : Integer; Direction__ : TDirection;
                                      var ABoxIsBlockedOnANonGoalSquare__ : Boolean):Boolean;
  var Neighbor1, Neighbor2 : Integer;
      Neighbor1Position , Neighbor2Position : TColRow;
  begin
    if   Direction__              = Low  ( Direction__ ) then                   {flip horizontal/vertical direction}
         Direction__             := Succ ( Low ( Direction__ ) )                {caution: 'Succ(Low...'): assumes 4 directions only}
    else Direction__             := Low  ( Direction__ );

    if   ( Direction__            = Low ( Direction__ ) )
         and
         (BoardTimestamps.Squares [ Col__ , Row__ ] >= BoardTimestamps.Timestamp) then {'True': use the already calculated value}

         Result                  := BoardTimestamps.Squares [ Col__ , Row__ ] > BoardTimestamps.Timestamp {relies on Ord ( False , True ) = (0 , 1)}

    else begin
           Neighbor1Position.x  := Col__ - DIRECTION_XY [ Direction__ , ColAxis ];
           Neighbor1Position.y  := Row__ - DIRECTION_XY [ Direction__ , RowAxis ];
           Neighbor1            := Board [ Neighbor1Position.x , Neighbor1Position.y ];

           Neighbor2Position.x  := Col__ + DIRECTION_XY [ Direction__ , ColAxis ];
           Neighbor2Position.y  := Row__ + DIRECTION_XY [ Direction__ , RowAxis ];
           Neighbor2            := Board [ Neighbor2Position.x , Neighbor2Position.y ];

           Inc ( Board [ Col__ , Row__ ] , WALL);                               {temporarily change this square to a wall}

           Result := ((  Neighbor1 and (WALL + INVISIBLE_WALL)) <> 0 )
                     or                                                         {is there a wall on any of the neighbor squares?}
                     ((  Neighbor2 and (WALL + INVISIBLE_WALL)) <> 0 )
                     or                                                         {are both neighbors illegal squares?}
                     ((( Neighbor1 and ILLEGAL_SQUARE         ) <> 0 )
                      and
                      (( Neighbor2 and ILLEGAL_SQUARE         ) <> 0 )
                     );

           if (not ( Result and ABoxIsBlockedOnANonGoalSquare__ ))              {it's not enough to know whether a box is blocked; it's also necessary to know whether a box is blocked on a non-goal square}
              and
              (( Neighbor1 and ( WALL + BOX ) ) = BOX )                         {test if neighbor1 is a blocked box}
              and
              BoxIsBlockedAlongOneAxis( Neighbor1Position.x , Neighbor1Position.y , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
              then Result := True;

           if (not ( Result and ABoxIsBlockedOnANonGoalSquare__ ))              {it's not enough to know whether a box is blocked; it's also necessary to know whether a box is blocked on a non-goal square}
              and
              (( Neighbor2 and ( WALL + BOX ) ) = BOX )                         {test if neighbor2 is a blocked box}
              and
              BoxIsBlockedAlongOneAxis( Neighbor2Position.x , Neighbor2Position.y , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
              then Result:=True;

           Dec ( Board [ Col__ , Row__ ] , WALL );                              {remove the wall again}
         end;

    if   Result and                                                             {if this box is blocked}
         ( ( Board [ Col__ , Row__ ] and GOAL ) = 0 ) then                      {and it's not a goal-square}
         ABoxIsBlockedOnANonGoalSquare__ := True;                               {then set the flag}

    if   Direction__ = Low ( Direction__ ) then with BoardTimestamps do         {reduce the exponential growth by storing the results for one axis}
         Squares [ Col__ , Row__ ] := Timestamp + Cardinal ( Ord ( Result ) );  {relies on Ord ( False , True ) = (0 , 1)}

  end; {IsAFreezingMove.BoxIsBlockedAlongOneAxis}

  function  BoxIsBlockedInReverseMode( Col__, Row__ : Integer ) : Boolean;
  {the box queue can be shared by all recursive calls of the                   }
  {"BoxIsBlockedInReverseMode()" function because there is always only one     }
  {instance of a box on the queue at a time.                                   }
  var Queue : TBoxPositions;

    function  BoxIsBlockedInReverseMode(
                Col__, Row__, QueueBase__ : Integer ) : Boolean;
    var QueueBottom, QueueTop, SquareValue : Integer;
        ABoxIsBlockedOnANonGoalSquare      : Boolean;
        Direction                          : TDirection;
        Square                             : TColRow;
    begin
      if BoardTimestamps.Squares[ Col__, Row__ ]
         < BoardTimestamps.Timestamp then begin
         {the box square hasn't been investigated yet; do it now}
         Result                            := True;
         ABoxIsBlockedOnANonGoalSquare     := False;
         {put the box square on the queue for examination}
         QueueBottom                       := QueueBase__;
         QueueTop                          := Succ( QueueBottom );
         Queue[ QueueTop ].X               := Col__;
         Queue[ QueueTop ].Y               := Row__;
         {turn the box square into a wall so it isn't visited more than once}
         Inc( Board[ Col__, Row__ ], WALL );
         while QueueBottom < QueueTop do begin
           {get the next box position from the queue}
           Inc( QueueBottom );
           Col__                              := Queue[ QueueBottom ].X;
           Row__                              := Queue[ QueueBottom ].Y;

           if  ( Board[ Col__, Row__ ] and BOX_START_POSITION ) = 0 then
               {the current box square isn't a target square; raise the flag}
               {even though it's not known yet whether the box is blocked}
               ABoxIsBlockedOnANonGoalSquare  := True;

           for Direction := Low( Direction ) to High( Direction ) do
               if Result then begin {'True': not failed yet; continue search}
                  {check neighbor square in this direction}
                  Square.X        := Col__ +
                                     DIRECTION_XY[ Direction , ColAxis ];
                  Square.Y        := Row__ +
                                     DIRECTION_XY[ Direction , RowAxis ];
                  SquareValue     := Board [ Square.X, Square.Y ];
                  if (   ( SquareValue and WALL ) = 0 )  {'True': not a wall}
                     and
                     ( ( ( SquareValue and BOX  ) = 0 )  {'True': not a box}
                       or
                       ( not BoxIsBlockedInReverseMode( Square.X, Square.Y,
                                                        QueueTop ) )
                     ) then begin
                     {the neighbor it neither a wall nor a frozen box; advance
                     { to the next square in this direction}
                     Inc( Square.X,   DIRECTION_XY[ Direction , ColAxis ] );
                     Inc( Square.Y,   DIRECTION_XY[ Direction , RowAxis ] );
                     SquareValue    := Board [ Square.X, Square.Y ];
                     if ( SquareValue and WALL ) = 0 then
                        {next square is neither a wall nor a visited box square}
                        if ( SquareValue and BOX ) = 0 then begin
                           {the next square is an empty floor square, hence,}
                           {the boxes aren't frozen}
                           Result      := False;    {exit 'for'   loop}
                           QueueBottom := QueueTop; {exit 'while' loop}
                           end
                        else begin
                           {the next square contains an unvisited box; put it}
                           {on the queue for later examination}
                           Inc( QueueTop );
                           Queue[ QueueTop ].X := Square.X;
                           Queue[ QueueTop ].Y := Square.Y;
                           {turn the box square into a wall so it isn't visited}
                           {more than once}
                           Inc( Board[ Square.X, Square.Y ], WALL );
                           end;
                     end;
                  end;
           end;

         Result := Result and ABoxIsBlockedOnANonGoalSquare; {return value}

         if QueueTop > High( Queue ) then
            // sanity check failed. if the program ever gets here, then it has
            // already currupted the memory by overwriting memory outside the
            // queue vector.
            raise Exception.Create(
                    Format( TEXT_INTERNAL_ERROR_FORMAT,
                            [ 'Queue overflow in "TSokoGame.IsAFreezingMove.' +
                              'BoxIsBlockedInReverseMode"' ] ) );

         {remove temporary walls from all the visited box squares}
         while QueueTop <> QueueBase__ do with Queue[ QueueTop ] do begin
           {avoid exponential growth by saving the result for visited squares;}
           {the stored result relies on Ord( False ) = 0, and Ord( True ) = 1}
           with BoardTimestamps do
             Squares [ X, Y ] := Timestamp + Cardinal ( Ord ( Result ) );

           Dec( Board [ X, Y ], WALL ); {remove the temporary wall}
           Dec( QueueTop );
           end;
         end
      else
         {the box square has already been investigated; return the stored}
         {result; the stored result relies on Ord( False ) = 0, and}
         {Ord( True ) = 1}
         with BoardTimestamps do
           Result := Squares [ Col__ , Row__ ] > Timestamp;
    end; {IsAFreezingMove.BoxIsBlockedInReverseMode.BoxIsBlockedInReverseMode}

  begin {IsAFreezingMove.BoxIsBlockedInReverseMode}
    Result := BoxIsBlockedInReverseMode( Col__, Row__, 0 );
  end; {IsAFreezingMove.BoxIsBlockedInReverseMode}

  function BoxIsBlockedOnClosedDiagonal( BoxCol__ , BoxRow__ , PlayerCol__ , PlayerRow__ : Integer ):Boolean; {caution: assumes 4 directions only}
  var OriginalBoxSquareValue , OriginalPlayerSquareValue : Integer;

    function Check( DiagonalSquareCol__ , DiagonalSquareRow__ , BoxSquareCol__ : Integer) : Boolean;
    {
     preconditions: 'diagonal square' is neighbor to the pushed box and on the
     same row as the pushed box;
    }
    const DONE = BOX + PLAYER; {must be a non-zero impossible board square value with the 'PLAYER' bit}
          TRY_NEXT_DIAGONAL = WALL + PLAYER; {must be a non-zero impossible board square value with the 'PLAYER' bit}
          TRY_SQUARES_ON_DIAGONAL_IN_OPPOSITE_DIRECTION_FROM_START_POINT = WALL + BOX + PLAYER; {must be a non-zero impossible board square value with the 'PLAYER' bit}
    var   Col , Row , DiagonalSquareValue , DX , DY ,
          GoalOrWallSquareOnDiagonalCol , GoalOrWallSquareOnDiagonalRow ,
          NextSquareOnSameRowAsDiagonalSquareValue ,                      {'next'    : according to the vertical direction of the diagonal, i.e., left or right}
          PreviousSquareOnSameRowAsDiagonalSquareValue : Integer;         {'previous': according to the vertical direction of the diagonal, i.e., left or right}
          HasGoalsAndWallsSequence , StartingRowIsOneEndOfTheDiagonal : Boolean;
          ColDirection , RowDirection : TDirection;
    begin {IsAFreezingMove.BoxIsBlockedOnClosedDiagonal.Check}
      {
       there are two diagonals with directions 'right-up' and 'left-up'
       respectively; each '%' square around the closed diagonal contains a box
       or a wall;

       ---%%  left-down/right-up diagonal
       --%-%
       -%-%-
       %-%--
       %%---

       %%---  left-up/right-down diagonal
       %-%--
       -%-%-
       --%-%
       ---%%

       the search visits squares along the diagonals as long as there are
       boxes or walls to the left and to the right of the diagonal square in the
       middle, and until a blocking square (either a box or a wall) has been
       found in both ends of the diagonal;

       if the pushed box belongs to one of the rows in the middle, then the
       search only investigates two diagonals as depicted above, but the search
       must look for a blocking square in each end of these diagonals;

       if the pushed box is located at the first or the last row in the
       figure, then there is already a blocked end point for the diagonal, and
       the search must investigate 4 separate diagonals, starting from the 2
       boxes in that row;

       if a diagonal is blocked in both ends by a box or by a wall, and pushing
       boxes inwards always produces new smaller closed diagonals with at least
       one box at a non-goal square, then the closed diagonal is a deadlock;
      }

      Result                                     := False;

      StartingRowIsOneEndOfTheDiagonal           := ( Board [ DiagonalSquareCol__ , DiagonalSquareRow__ ] and ( BOX + WALL ) ) <> 0; {'True': the starting diagonal square is itself an end point of the diagonal}
      ColDirection                               := Left; {start investigating the left-up diagonal}
      RowDirection                               := Up;

      repeat {until all diagonals have been investigated, or until a closed diagonal deadlock has been found}
        if   ColDirection                         = Left then {'True': investigate the first (part of the) diagonal going left}
             DX                                  := DIRECTION_XY [ Left  , ColAxis ]  {investigate the (part of the) diagonal going left}
        else DX                                  := DIRECTION_XY [ Right , ColAxis ]; {investigate the (part of the) diagonal going right}
        if   RowDirection                         = Up then {'True': investigate the first (part of the) diagonal going up}
             DY                                  := DIRECTION_XY [ Up    , RowAxis ]  {investigate the (part of the) diagonal going up}
        else DY                                  := DIRECTION_XY [ Down  , RowAxis ]; {investigate the (part of the) diagonal going down}

        GoalOrWallSquareOnDiagonalCol            := 0; {'0': no goals or walls found on the diagonal yet}
        GoalOrWallSquareOnDiagonalRow            := 0;

        repeat {until the current diagonal has been investigated both upwards and downwards, if that's necessary (diagonals where the start square itself is blocked by a box or a wall only needs investigation in one direction)}
          if   StartingRowIsOneEndOfTheDiagonal then begin {'True': the start point is itself a blocked diagonal square; skip to the next diagonal square in the row above/below the start square}
               {find the correct column of the diagonal square in the row
                above/below the start square; the correct column is above/below
                one of the two boxes or walls on the starting row;
               }
               if   DX > 0 then {'True': investigating 'right-up' or 'right-down' diagonal; the diagonal starts at the leftmost square}
                    if   DiagonalSquareCol__      < BoxSquareCol__ then
                         Col                     := DiagonalSquareCol__
                    else Col                     := BoxSquareCol__
               else {investigating 'left-up' or 'left-down' diagonal; the diagonal starts at the rightmost square}
                    if   DiagonalSquareCol__      > BoxSquareCol__ then
                         Col                     := DiagonalSquareCol__
                    else Col                     := BoxSquareCol__;

               Row                               := DiagonalSquareRow__;

               HasGoalsAndWallsSequence          := ( ( Board [ Col      , Row ] and ( GOAL + WALL ) ) <> 0 )  {'True': the starting diagonal square is a goal square or a wall square}
                                                    and
                                                    ( ( Board [ Col + DX , Row ] and ( GOAL + WALL ) ) <> 0 ); {'True': the next square in the row (according to the horizontal direction of the diagonal) is a goal square or a wall square}

               Inc ( Col , DX ); {move to the row above/below the diagonal starting point}
               Inc ( Row , DY );
               end
          else
            if GoalOrWallSquareOnDiagonalCol      = 0 then begin {'True': no goals or walls found on the diagonal during the first left-going search along the diagonal}
               Col                               := DiagonalSquareCol__;
               Row                               := DiagonalSquareRow__;
               HasGoalsAndWallsSequence          := False;
               end
            else begin
               {the first search along the diagonal found a goal square or a
                wall square;
                to test correctly for goal square sequences, this second search
                along the diagonal in the opposite direction must start from
                this square;
               }
               Col                               := GoalOrWallSquareOnDiagonalCol;
               Row                               := GoalOrWallSquareOnDiagonalRow;

               {if the next square in this row (according to the horizontal
                direction of the current search direction along the diagonal)
                also is a goal or a wall, then this second search along the
                diagonal begins with an 'all goals and walls' sequence;
               }
               HasGoalsAndWallsSequence          := ( Board [ Col + DX , Row ] and ( GOAL + WALL ) ) <> 0;

               Inc ( Col , DX ); {move down to the row below the goal or wall}
               Inc ( Row , DY );
               end;

          DiagonalSquareValue                    := Board [ Col , Row ]; {get the board square value for the diagonal square}

          while ( DiagonalSquareValue and PLAYER  ) = 0 do begin {'True': the investigation of the diagonal in the current direction hasn't finished yet, and the current diagonal square doesn't contain a player}
            {PLAYER: testing if the player is inside the closed diagonal is not
             strictly necessary; the player cannot be inside a closed diagonal
             after a push;

             making the test anyway keeps the entire 'IsAFreezingMove' function
             independent of the player position; that way, the function can also
             be used for checking if the starting position contains a
             freeze-deadlock;

             state-control values like 'TRY_NEXT_DIAGONAL' have the 'PLAYER' bit
             set so they also terminate this 'while' loop;
            }

            PreviousSquareOnSameRowAsDiagonalSquareValue := Board [ Col - DX , Row ]; {'previous/next': according to the vertical direction of the diagonal, i.e., left or right}

            if HasGoalsAndWallsSequence then {'True': there is an 'all goals or walls' sequence of most recently visited squares around the diagonal}
               {there is an 'all goals and walls' sequence of most recently
                visited squares on, and around, the diagonal; check if the
                sequence can be extended;

                an example with the diagonal going 'right-up':

                ?!    <---- '!' is the current  diagonal square
                .#    <---- '.' is the previous diagonal square

                the previous diagonal square happened to be a goal square,
                thereby starting a new 'all goals and walls' sequence;

                the next square (in the diagonal horizontal direction) after the previous
                diagonal square was a goal or a wall (a wall in this example),
                so it extended the 'all goals and walls' sequence;

                if the '?' square is a goal or a wall, and the current diagonal
                square '!' also is a goal or a wall, then some of the boxes
                around the diagonal can be pushed inwards to goal squares and
                open up the diagonal without creating a deadlock;
               }
               HasGoalsAndWallsSequence          := ( PreviousSquareOnSameRowAsDiagonalSquareValue and ( GOAL + WALL ) ) <> 0;

            if  ( ( DiagonalSquareValue and ( BOX + WALL ) ) = 0 )  then begin {'True': the diagonal square is an empty floor square}
                NextSquareOnSameRowAsDiagonalSquareValue := Board [ Col + DX , Row ];             {get the value of the next square (according to the horizontal direction of the diagonal) in the same row as the current diagonal square}
                if   ( ( PreviousSquareOnSameRowAsDiagonalSquareValue and ( BOX + WALL ) ) <> 0 ) {'True': there is a box or a wall to the left or right of the current diagonal square}
                     and
                     ( ( NextSquareOnSameRowAsDiagonalSquareValue     and ( BOX + WALL ) ) <> 0 ) {'True': there is a box or a wall to the left or right of the current diagonal square}
                     and
                     ( not ( HasGoalsAndWallsSequence {'True': there is an 'all goals and walls ' sequence of squares on, and around, the most recently visited part of the diagonal}
                             and
                             ( ( DiagonalSquareValue and GOAL ) <> 0 ) {'True': the current diagonal square is a goal square which closes the 'all goals' sequence of squares, so the diagonal isn't a deadlock}
                           )
                     ) then begin
                     if ( DiagonalSquareValue and GOAL ) <> 0 then begin {'True': the current diagonal square is a goal square}
                        HasGoalsAndWallsSequence         := True; {the goal on the diagonal square starts a new sequence of 'all goals and walls' squares, or extends an existing sequence}
                        if GoalOrWallSquareOnDiagonalCol  = 0 then begin {'True': this is the first found goal on the diagonal during the search in this direction}
                           GoalOrWallSquareOnDiagonalCol := Col; {remember the first found goal during the search in this direction}
                           GoalOrWallSquareOnDiagonalRow := Row;
                           end;
                        end;

                     if HasGoalsAndWallsSequence then {'True': there is an 'all goals and walls ' sequence of squares on, and around, the most recently visited part of the diagonal}
                        HasGoalsAndWallsSequence := ( ( NextSquareOnSameRowAsDiagonalSquareValue and ( GOAL + WALL ) ) <> 0 ); {'True': the next square is a goal or a wall, hence it extends the 'all goals and walls' sequence of squares}

                     if ( ( NextSquareOnSameRowAsDiagonalSquareValue and WALL ) = 0 )
                        or {'True': the current diagonal square doesn't have 2 neigboring walls which block the access to the next diagonal square in the current direction}
                        ( ( Board [ Col  , Row + DY ]                and WALL ) = 0 )
                        or
                        HasGoalsAndWallsSequence {'True': there is an 'all goals and walls' sequence of squares, so it's necessary to advance to the next diagonal square and continue the investigation}
                        then begin
                        Inc ( Col , DX ); {advance to the next diagonal square}
                        Inc ( Row , DY );
                        DiagonalSquareValue      := Board [ Col , Row ];
                        end
                     else begin {the diagonal ends with an empty floor square having 2 neigboring walls which block the access to the next diagonal square in the current direction}
                        {fake that the diagonal is blocked by a box in this end;
                         the enclosing 'while' loop then re-evaluates this end
                         of the diagonal;
                        }
                        DiagonalSquareValue      := BOX or ( DiagonalSquareValue and GOAL ); {'GOAL': keep the correct 'is-this-a-goal-square?' flag for the diagonal square value}

                        {if this is the first found end of the diagonal, then
                         remember the starting point for the search along the
                         diagonal in the opposite direction;

                         the starting point is the next diagonal square in the
                         current direction, so the current diagonal square also
                         is examined by the downward search; that is necessary
                         for a correct 'HasGoalsAndWallsSequence' analysis;
                        }
                        GoalOrWallSquareOnDiagonalCol := Col + DX;
                        GoalOrWallSquareOnDiagonalRow := Row + DY;
                        end;
                     end
                else DiagonalSquareValue         := TRY_NEXT_DIAGONAL;  {try the next diagonal, if any; there is no closed diagonal, or it's not a deadlock because there is a sequence of goal squares making it possible to push boxes inwards without creating a deadlock}
                end
            else {the diagonal is blocked by a box or a wall at [Col,Row]}
                if ( ( PreviousSquareOnSameRowAsDiagonalSquareValue and ( BOX + WALL ) ) = 0 ) {'True': there isn't a box or a wall at the square 'before' the diagonal square on this row, so the diagonal isn't closed by 3 boxes and walls forming an 'L' in this end}
                   or
                   ( HasGoalsAndWallsSequence and ( ( DiagonalSquareValue and ( WALL + GOAL ) ) <> 0 ) ) then {'True': the current diagonal square closes a sequence of 'all goals and walls' sequence, hence, the diagonal isn't a deadlock}
                   DiagonalSquareValue           := TRY_NEXT_DIAGONAL   {try next diagonal, if any}
                else {there are 2 neighboring boxes or walls in the row, closing the diagonal in this end, and pushing boxes inwards along the examined part of the diagonal creates a deadlock}
                   if ( not StartingRowIsOneEndOfTheDiagonal )          {'True': the box or wall at [Col,Row] is not the other end point of a diagonal which started with a blocked end}
                      and
                      ( DY = DIRECTION_XY [ Up , RowAxis ] ) then begin {'True': the first part of the diagonal (going upwards) has just been investigated}
                      if (   ( DiagonalSquareValue and WALL ) <> 0 )    {'True': the diagonal ends with a wall}
                         or
                         ( ( ( DiagonalSquareValue and GOAL ) <> 0 )    {'True': the diagonal ends with a goal square}
                           and
                           ( GoalOrWallSquareOnDiagonalCol     = 0 )    {'True': this is the first found goal on the diagonal}
                         )
                         then begin
                         GoalOrWallSquareOnDiagonalCol    := Col;       {remember the starting point for the search along the diagonal in the opposite direction}
                         GoalOrWallSquareOnDiagonalRow    := Row;
                         end;

                      DX                         := - DX; {flip the diagonal direction to prepare visiting the squares along the diagonal in the opposite direction (downwards) starting from the original square}
                      DY                         := - DY;
                      DiagonalSquareValue        := TRY_SQUARES_ON_DIAGONAL_IN_OPPOSITE_DIRECTION_FROM_START_POINT; {investigate the squares on the diagonal in the opposite direction}
                      end
                   else begin {both ends of the diagonal are blocked by boxes or walls, and pushing boxes inwards along the diagonal creates a deadlock}
                      Result                     := True; {'True': there is a closed diagonal deadlock}
                      DiagonalSquareValue        := DONE; {exit the enclosing loops}
                      end;
            end;

        until DiagonalSquareValue <> TRY_SQUARES_ON_DIAGONAL_IN_OPPOSITE_DIRECTION_FROM_START_POINT; {'True': the search along the current diagonal (e.g., the left-up diagonal or the right-up diagonal) is over}

        if StartingRowIsOneEndOfTheDiagonal then
           if   RowDirection                      = Up then
                if ColDirection                   = Left then {'True': the up-left diagonal from the start point has just been investigated}
                   ColDirection                  := Right {check the up-right   diagonal from the start point}
                else begin
                   RowDirection                  := Down; {check the down-left  diagonal from the start point}
                   ColDirection                  := Left;
                   end
           else if   ColDirection                 = Left then
                     ColDirection                := Right {check the down-right diagonal from the start point}
                else DiagonalSquareValue         := DONE  {the search is over; all 4 diagonals emanating from the starting row have been investigated}
        else {the pushed box is located at one of the middle rows, where the search only needs to investigate the two diagonals 'left-up' and 'right-up', but where the search also must find both end points of the diagonal}
           if   ColDirection                      = Left then {'True': the left-up diagonal has just been investigated}
                ColDirection                     := Right {investigate the right-up diagonal}
           else DiagonalSquareValue              := DONE; {the search is over; the left-up and the right-up diagonals have both been investigated}

      until DiagonalSquareValue = DONE; {'True': the search is over; all diagonals have been investigated, or a closed diagonal deadlock has been found, in which case 'Result' = 'True'}
    end; {IsAFreezingMove.BoxIsBlockedOnClosedDiagonal.Check}

  begin {IsAFreezingMove.BoxIsBlockedOnClosedDiagonal}
    OriginalBoxSquareValue                       := Board [ BoxCol__    , BoxRow__    ];
    Board [ BoxCol__    , BoxRow__    ]          := Board [ BoxCol__    , BoxRow__    ] or BOX; {put a box at the box square if there isn't one already}

    {the player cannot be inside a closed diagonal after a push, hence, it's
     always OK to remove the player from the board before testing if there is a
     deadlocked closed diagonal after a push;
     if the function is used for testing if the starting position is a deadlock,
     then there is no preceding push; in that case, pass a wall square as player
     position in 'PlayerCol__' and 'PlayerRow__';
    }
    OriginalPlayerSquareValue                    := Board [ PlayerCol__ , PlayerRow__ ];
    Board [ PlayerCol__ , PlayerRow__ ]          := Board [ PlayerCol__ , PlayerRow__ ] and ( not PLAYER ); {remove the player from the board}

    Result                                       := Check ( BoxCol__ + DIRECTION_XY [ Left  , ColAxis  ] , {check the box square and its left  neighbor}
                                                            BoxRow__ + DIRECTION_XY [ Left  , RowAxis  ] ,
                                                            BoxCol__ )
                                                    or
                                                    Check ( BoxCol__ + DIRECTION_XY [ Right  , ColAxis ] , {check the box square and its right neighbor}
                                                            BoxRow__ + DIRECTION_XY [ Right  , RowAxis ] ,
                                                            BoxCol__ );

    Board [ PlayerCol__ , PlayerRow__ ]          := OriginalPlayerSquareValue;  {restore the player square value}
    Board [ BoxCol__    , BoxRow__    ]          := OriginalBoxSquareValue;     {restore the box square value; in the game there may not be a box at the square, but it was added at the beginning of this function}
  end; {IsAFreezingMove.BoxIsBlockedOnClosedDiagonal}

begin {IsAFreezingMove}
  with BoardTimestamps do
    if Timestamp < High ( Timestamp ) - 5 then
       Inc ( Timestamp , 2 )
    else begin
       FillChar ( Squares , SizeOf ( Squares ) , 0 ); Timestamp := 2;
       end;

  ABoxIsBlockedOnANonGoalSquare := False;

  OriginalFromSquareValue := Board [ FromCol__ , FromRow__ ];
  Board [ FromCol__ , FromRow__ ] := Board [ FromCol__ , FromRow__ ] and (not BOX); {remove box, if any (the from-square is optional), from its current position}

  Result := (( Board [ ToCol__ , ToRow__ ] and ( WALL + ILLEGAL_SQUARE ) ) <> 0 )   {a wall is considered a deadlocked square}
            or
            (( not ReverseMode )
             and
             ( ( BoxIsBlockedAlongOneAxis ( ToCol__ , ToRow__ , Low (       TDirection )   , ABoxIsBlockedOnANonGoalSquare )
                 and
                 BoxIsBlockedAlongOneAxis ( ToCol__ , ToRow__ , Succ( Low ( TDirection ) ) , ABoxIsBlockedOnANonGoalSquare ) {caution: 'Succ(Low...'): assumes 4 directions only}
                 and
                 ABoxIsBlockedOnANonGoalSquare
               )
               or
               BoxIsBlockedOnClosedDiagonal ( ToCol__ , ToRow__ , PlayerCol__ , PlayerRow__ )
             )
            )
            or
            (ReverseMode
             and
             BoxIsBlockedInReverseMode ( ToCol__ , ToRow__ )
            );

  Board [ FromCol__ , FromRow__ ] := OriginalFromSquareValue;                   {put box, if any, back on the board}
end; {TSokoGame.IsAFreezingMove}

function TSokoGame.IsAFrozenSquare(Col__,Row__:Integer; var BoxCount__:Integer; var BoxSquares__:TBoxPositions):Boolean;
var
  OriginalSquareValue : Integer; Visited:TBoardOfBooleans;
  ABoxIsBlockedOnANonGoalSquare : Boolean;

  function  BoxIsBlockedAlongOneAxis( Col__, Row__ : Integer; Direction__ : TDirection;
                                      var ABoxIsBlockedOnANonGoalSquare__ : Boolean):Boolean;
  var i, Neighbor1, Neighbor2, OldCount : Integer;
      Neighbor1Position , Neighbor2Position : TColRow;
  begin
    OldCount              := BoxCount__;

    if   Direction__       = Low  ( Direction__ ) then                          {flip horizontal/vertical direction}
         Direction__      := Succ ( Low ( Direction__ ) )                       {caution: 'Succ(Low...'): assumes 4 directions only}
    else Direction__      := Low  ( Direction__ );

    if   ( Direction__     = Low ( Direction__ ) )
         and
         (BoardTimestamps.Squares [ Col__ , Row__ ] >= BoardTimestamps.Timestamp) then {'True': use the already calculated value}

         Result           := BoardTimestamps.Squares [ Col__ , Row__ ] > BoardTimestamps.Timestamp {relies on Ord ( False , True ) = (0 , 1)}

    else begin
           Neighbor1Position.x  := Col__ - DIRECTION_XY [ Direction__ , ColAxis ];
           Neighbor1Position.y  := Row__ - DIRECTION_XY [ Direction__ , RowAxis ];
           Neighbor1            := Board [ Neighbor1Position.x , Neighbor1Position.y ];

           Neighbor2Position.x  := Col__ + DIRECTION_XY [ Direction__ , ColAxis ];
           Neighbor2Position.y  := Row__ + DIRECTION_XY [ Direction__ , RowAxis ];
           Neighbor2            := Board [ Neighbor2Position.x , Neighbor2Position.y ];

           Inc ( Board [ Col__ , Row__ ] , WALL);                               {temporarily change this square to a wall}

           Result := ((  Neighbor1 and (WALL + INVISIBLE_WALL)) <> 0 )
                     or                                                         {is there a wall on any of the neighbor squares?}
                     ((  Neighbor2 and (WALL + INVISIBLE_WALL)) <> 0 )
                     or                                                         {are both neighbors illegal squares?}
                     ((( Neighbor1 and ILLEGAL_SQUARE         ) <> 0 )
                      and
                      (( Neighbor2 and ILLEGAL_SQUARE         ) <> 0 )
                     );

           if (( Neighbor1 and ( WALL + BOX ) ) = BOX )                         {test if neighbor1 is a blocked box}
              and
              BoxIsBlockedAlongOneAxis( Neighbor1Position.x , Neighbor1Position.y , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
              then Result := True;

           if (( Neighbor2 and ( WALL + BOX ) ) = BOX )                         {test if neighbor2 is a blocked box}
              and
              BoxIsBlockedAlongOneAxis( Neighbor2Position.x , Neighbor2Position.y , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
              then Result:=True;

           Dec ( Board [ Col__ , Row__ ] , WALL );                              {remove the wall again}
         end;

    if   Result and                                                             {if this box is blocked}
         ( ( Board [ Col__ , Row__ ] and GOAL ) = 0 ) then                      {and it's not a goal-square}
         ABoxIsBlockedOnANonGoalSquare__ := True;                               {then set the flag}

    if   Direction__ = Low ( Direction__ ) then with BoardTimestamps do         {reduce the exponential growth by storing the results for one axis}
         Squares [ Col__ , Row__ ] := Timestamp + Cardinal ( Ord ( Result ) );  {relies on Ord ( False , True ) = (0 , 1)}

   if    Result then begin                                                      {if this box is blocked}
         if ( ( Board [ Col__ , Row__ ] and GOAL ) = 0 ) then
            ABoxIsBlockedOnANonGoalSquare__ := True;                            {if it's not a goal-square then set the flag}

         if not Visited [ Col__ , Row__ ] then begin
            Inc ( BoxCount__ );                                                 {add this square to the box set}
            BoxSquares__ [ BoxCount__ ].x := Col__;
            BoxSquares__ [ BoxCount__ ].y := Row__;
            Visited [ Col__ , Row__ ]     := True;                              {mark the square as being a member of the set}
            end;
         end
   else  begin                                                                  {this box is not blocked; restore the box set}
           for i := Succ( OldCount ) to BoxCount__ do with BoxSquares__[i] do
               Visited [ x , y ] := False;
           BoxCount__ := OldCount;
        end;

  end; {IsAFrozenSquare.BoxIsBlockedAlongOneAxis}

  function  BoxIsBlockedInReverseMode( Col__, Row__ : Integer ) : Boolean;
  {the box queue can be shared by all recursive calls of the                   }
  {"BoxIsBlockedInReverseMode()" function because there is always only one     }
  {instance of a box on the queue at a time.                                   }
  var Queue : TBoxPositions;

    function  BoxIsBlockedInReverseMode(
                Col__, Row__, QueueBase__ : Integer ) : Boolean;
    var OldCount, QueueBottom, QueueTop, SquareValue  : Integer;
        ABoxIsBlockedOnANonGoalSquare                 : Boolean;
        Direction                                     : TDirection;
        Square                                        : TColRow;
    begin
      if BoardTimestamps.Squares[ Col__, Row__ ]
         < BoardTimestamps.Timestamp then begin
         {the box square hasn't been investigated yet; do it now}
         Result                               := True;
         ABoxIsBlockedOnANonGoalSquare        := False;
         OldCount                             := BoxCount__; {frozen boxes set}
         {put the box square on the queue for examination}
         QueueBottom                          := QueueBase__;
         QueueTop                             := Succ( QueueBottom );
         Queue[ QueueTop ].X                  := Col__;
         Queue[ QueueTop ].Y                  := Row__;
         {turn the box square into a wall so it isn't visited more than once}
         Inc( Board[ Col__, Row__ ], WALL );
         while QueueBottom < QueueTop do begin
           {get the next box position from the queue}
           Inc( QueueBottom );
           Col__                              := Queue[ QueueBottom ].X;
           Row__                              := Queue[ QueueBottom ].Y;

           if  ( Board[ Col__, Row__ ] and BOX_START_POSITION ) = 0 then
               {the current box square isn't a target square; raise the flag}
               {even though it's not known yet whether the box is blocked}
               ABoxIsBlockedOnANonGoalSquare  := True;

           for Direction := Low( Direction ) to High( Direction ) do
               if Result then begin {'True': not failed yet; continue search}
                  {check neighbor square in this direction}
                  Square.X        := Col__ +
                                     DIRECTION_XY[ Direction , ColAxis ];
                  Square.Y        := Row__ +
                                     DIRECTION_XY[ Direction , RowAxis ];
                  SquareValue     := Board [ Square.X, Square.Y ];
                  if (   ( SquareValue and WALL ) = 0 )  {'True': not a wall}
                     and
                     ( ( ( SquareValue and BOX  ) = 0 )  {'True': not a box}
                       or
                       ( not BoxIsBlockedInReverseMode( Square.X, Square.Y,
                                                        QueueTop ) )
                     ) then begin
                     {the neighbor it neither a wall nor a frozen box; advance}
                     {to the next square in this direction}
                     Inc( Square.X,   DIRECTION_XY[ Direction , ColAxis ] );
                     Inc( Square.Y,   DIRECTION_XY[ Direction , RowAxis ] );
                     SquareValue    := Board [ Square.X, Square.Y ];
                     if ( SquareValue and WALL ) = 0 then
                        {next square is neither a wall nor a visited box square}
                        if ( SquareValue and BOX ) = 0 then begin
                           {the next square is an empty floor square, hence,}
                           {the boxes aren't frozen}
                           Result      := False;    {exit 'for'   loop}
                           QueueBottom := QueueTop; {exit 'while' loop}
                           end
                        else begin
                           {the next square contains an unvisited box; put it}
                           {on the queue for later examination}
                           Inc( QueueTop );
                           Queue[ QueueTop ].X := Square.X;
                           Queue[ QueueTop ].Y := Square.Y;
                           {turn the box square into a wall so it isn't visited}
                           {more than once}
                           Inc( Board[ Square.X, Square.Y ], WALL );
                           end;
                     end;
                  end;
           end;

         Result := Result and ABoxIsBlockedOnANonGoalSquare; {return value}

         if not Result then {'True': restore frozen boxes set}
            BoxCount__ := OldCount;

         if QueueTop > High( Queue ) then
            // sanity check failed. if the program ever gets here, then it has
            // already currupted the memory by overwriting memory outside the
            // queue vector.
            raise Exception.Create(
                    Format( TEXT_INTERNAL_ERROR_FORMAT,
                            [ 'Queue overflow in "TSokoGame.IsAFrozenSquare.' +
                              'BoxIsBlockedInReverseMode"' ] ) );

         {remove temporary walls from all the visited box squares}
         while QueueTop <> QueueBase__ do with Queue[ QueueTop ] do begin
           {avoid exponential growth by saving the result for visited squares;}
           {the stored result relies on Ord( False ) = 0, and Ord( True ) = 1}
           with BoardTimestamps do
             Squares [ X, Y ] := Timestamp + Cardinal ( Ord ( Result ) );

           if Result then begin {add frozen box squares to the box set}
              Inc( BoxCount__ );
              BoxSquares__ [ BoxCount__ ].X := X;
              BoxSquares__ [ BoxCount__ ].Y := Y;
              end;

           Dec( Board [ X, Y ], WALL ); {remove the temporary wall}
           Dec( QueueTop );
           end;
         end
      else
         {the box square has already been investigated; return the stored}
         {result; the stored result relies on Ord( False ) = 0, and}
         {Ord( True ) = 1}
         with BoardTimestamps do
           Result := Squares [ Col__ , Row__ ] > Timestamp;
    end; {IsAFrozenSquare.BoxIsBlockedInReverseMode.BoxIsBlockedInReverseMode}

  begin {IsAFrozenSquare.BoxIsBlockedInReverseMode}
    Result := BoxIsBlockedInReverseMode( Col__, Row__, 0 );
  end; {IsAFrozenSquare.BoxIsBlockedInReverseMode}

begin {IsAFrozenSquare}
  with BoardTimestamps do
    if Timestamp < High ( Timestamp ) - 5 then
       Inc ( Timestamp , 2 )
    else begin
       FillChar ( Squares , SizeOf ( Squares ) , 0 ); Timestamp := 2;
       end;

  ABoxIsBlockedOnANonGoalSquare := False;

  BoxCount__ := 0;
  if not ReverseMode then FillChar( Visited, SizeOf( Visited ), 0 );

  OriginalSquareValue := Board [ Col__ , Row__ ];
  Board [ Col__ , Row__ ] := Board [ Col__ , Row__ ] and ( not BOX );           {temporarily remove the box, if any}

  Result := (( Board [ Col__ , Row__ ] and ( WALL + ILLEGAL_SQUARE ) ) <> 0 )   {a wall is considered a deadlocked square}
            or
            (( not ReverseMode ) and
             BoxIsBlockedAlongOneAxis( Col__ , Row__ , Low(        TDirection )   , ABoxIsBlockedOnANonGoalSquare ) and
             BoxIsBlockedAlongOneAxis( Col__ , Row__ , Succ( Low ( TDirection ) ) , ABoxIsBlockedOnANonGoalSquare ) and  {caution: 'Succ(Low...'): assumes 4 directions only}
             ABoxIsBlockedOnANonGoalSquare
            )
            or
            (ReverseMode
             and
             BoxIsBlockedInReverseMode ( Col__ , Row__  )
            );

  Board [ Col__ , Row__ ] := OriginalSquareValue;            {put the box back on the board, if any}
  if not Result then BoxCount__:=0;
end; {TSokoGame.IsAFrozenSquare}

function TSokoGame.IsALegalMove(dx__,dy__:Integer; Flags__:Integer; var IsAFreezingMove__:Boolean):Boolean;
var i,x,y:Integer;
begin {Precondition: [dx__,dy__] must be a member of 'DIRECTION_XY'}
  x :=PlayerPos.x+dx__;
  y :=PlayerPos.y+dy__;

  Result:=(x>=1) and (x<=BoardWidth) and (y>=1) and (y<=BoardHeight) and (GameState=gsPlay);
  IsAFreezingMove__:=False;

  if Result then
     if (not ReverseMode)
        or
        ((Flags__ and H_FLAG_JUMP)=0) then begin {normal moves}
        i:=Board[x,y] and (WALL+BOX+GOAL+FLOOR+PLAYER);
        Result:=((i=FLOOR)               {an empty floor-square?}
                 or
                 (i=FLOOR+GOAL)          {an empty goal-square?}
                 or
                 ((not ReverseMode) and  {a legal push in a normal mode game?}
                  ((i and BOX)<>0)  and
                  ((Board[x+dx__,y+dy__] and (WALL+BOX+SimpleIllegalMovesMask))=0))
                 or
                 (ReverseMode            {a legal move or pull in a reverse mode game?}
                  and
                  ((i and (WALL+BOX))=0) {'True': player destination square is empty}
                 )
                )
                and
                ((not ReverseMode)
                 or
                 ((Flags__ and H_FLAG_BOX)=0)
                 or
                 ((Board[PlayerPos.x,PlayerPos.y] and (WALL+BOX))=0)
                );
        if Result and (SimpleIllegalMovesMask<>0) then begin
           if ReverseMode then
              if (Flags__ and (H_FLAG_BOX+MOVE_FLAG_KEYBOARD))=MOVE_FLAG_KEYBOARD then
                 i:=0 {don't try to pull box, if any}
              else begin
                 x :=PlayerPos.x-dx__;
                 y :=PlayerPos.y-dy__;
                 i:=Board[x,y] and (WALL+BOX+GOAL+FLOOR+PLAYER);
                 end;
           if (i and BOX)<>0 then begin {'True': the move pushes or pulls a box}
              IsAFreezingMove__:=IsAFreezingMove(x,y,x+dx__,y+dy__,PlayerPos.x,PlayerPos.y); {check if the new box position is a simple deadlock}
              Result:=not IsAFreezingMove__;
              end;
           end;
        end
     else {jumps}
        Result:=((Board[x,y] and (WALL+FLOOR))<>0)
                and
                ((History.PushCount=0)
                 or
                 JumpsAllowedAfterFirstBoxMoveInReverseMode
                 or
                 IsLoading              {when loading a level, respect if jumps were allowed at the time of creation}
                );
end; {TSokoGame.IsALegalMove}

function  TSokoGame.IsAnEmptyCol(Col__:Integer):Boolean;
var Row:Integer;
begin
  Result:=True;
  for Row:=1 to MAX_BOARD_HEIGHT do
      if (Board[Col__,Row] and (WALL+BOX+GOAL+PLAYER))<>0 then begin
         Result:=False; break;
         end;
end; {TSokoGame.IsAnEmptyCol}

function  TSokoGame.IsAnEmptyRow(Row__:Integer):Boolean;
var Col:Integer;
begin
  Result:=True;
  for Col:=1 to MAX_BOARD_WIDTH do
      if (Board[Col,Row__] and (WALL+BOX+GOAL+PLAYER))<>0 then begin
         Result:=False; break;
         end;
end; {TSokoGame.IsAnEmptyRow}

function  TSokoGame.IsAnIdenticalBoard(const Board__:TBoard; BoxNumbersMustMatch__,PlayerPositionMustMatch__:Boolean):Boolean;
var x,y,Mask:Integer;
begin
  Result:=True;

  if   BoxNumbersMustMatch__ then
       Mask:=not (BOX_LEGAL_MOVE+PLAYER_LEGAL_MOVE+BOX_ILLEGAL_MOVE+PLAYER_TRY_MOVE+SQUARE_SET)
  else Mask:=WALL+BOX+GOAL+PLAYER;
  if not PlayerPositionMustMatch__ then Mask:=Mask and (not PLAYER);

  for x:=0 to MAX_BOARD_WIDTH+1 do
      for y:=0 to MAX_BOARD_HEIGHT+1 do
          if (Board  [x,y] and Mask)<>
             (Board__[x,y] and Mask) then begin
             Result:=False; exit;
             end;
end; {TSokoGame.IsAnIdenticalBoard}

function TSokoGame.IsEqualToCurrentGame(Snapshot__:TSnapshot):Boolean;
var i:Integer;
begin {Compares a snapshot to current game}
  Result:=(Snapshot__<>nil) and (Snapshot__.Moves<>nil) and
          {note: current position ('Count') is *not* considered, only 'Top'}
          (Snapshot__.MoveTop=History.Top) and (Snapshot__.ReverseMode=ReverseMode);

  for i:=1 to History.Top do
      if   Result then
           Result:=(History   .Moves [i] and (not H_MASK_MOVE_SEPARATOR))=
                   (Snapshot__.Moves^[i] and (not H_MASK_MOVE_SEPARATOR))
      else break;
end; {TSokoGame.IsEqualToCurrentGame}

function TSokoGame.IsJumpFeatureUsedAfterFirstBoxMove(ReverseMode__:Boolean; Count__:Integer; Moves__:PHistoryMoves; var InitialJumps__:Integer):Boolean;
var i:Integer;
begin {Tests for "cheating", i.e., use of the jump-feature after first box-move}
  if ReverseMode__ then begin
     i:=1;
     while (i<=Count__) and ((Moves__^[i] and H_FLAG_BOX )=0) do Inc(i); {skip moves before first box-move}
     InitialJumps__:=Pred(i);                                            {everything before first box-move are considered jumps}
     while (i<=Count__) and ((Moves__^[i] and H_FLAG_JUMP)=0) do Inc(i); {find a cheating jump, if any}
     Result:=i<=Count__;
     end
  else begin
    Result:=False; InitialJumps__:=0;
    end;
end; {IsJumpFeatureUsedAfterFirstBoxMove}

function TSokoGame.ImportBuiltinSolutions:Boolean; {returns 'True' if anything is imported}
var BetterBuiltInSolutionMoves,BetterBuiltInSolutionPushes:TSnapshot; {Node:TNode;}
begin
  Result:=False;

  if IsABetterBuiltInSolutionAvailable(
       BetterBuiltInSolutionMoves,
       BetterBuiltInSolutionPushes) then begin

     if (BetterBuiltInSolutionPushes<>nil) then begin
        if BestSolutionPushes<>nil then with BestSolutionPushes do begin  {degrade the old solution to a normal snapshot}
           BestSolutionPushes.SetName(Snapshots.MakeUniqueName(SnapshotTypeName(stSolution)+SPACE+
                                                               Format(FORMAT_MOVES_AND_PUSHES,
                                                                      [MoveCount,PushCount])+
                                                               SecondaryMetricsFormattedAsATitleSuffix(
                                                                 SecondaryMetricsInTitles,
                                                                 SecondaryScoreMetrics)+
                                                               SolverNameText(BestSolutionPushes.Name)
                                                               {+NonOptimalText}
                                                               ,
                                                               SnapshotTypeName(stSolution),
                                                               True));
           {
           if not BestSolutionPushes.Notes.Lines.FindKey(TEXT_OPTIMALITY,Node) then
              BestSolutionPushes.Notes.Lines.WriteString(TEXT_OPTIMALITY,TEXT_NON_OPTIMAL);
           }
           Snapshots.InsertAfter(BestSolutionPushes,nil);
           end;
        BestSolutionPushes:=BetterBuiltinSolutionPushes;
        with BestSolutionPushes do SetName(SnapshotTypeName(stBestSolutionPushes)+SPACE+
                                           Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
                                           SecondaryMetricsFormattedAsATitleSuffix(
                                             SecondaryMetricsInTitles,
                                             SecondaryScoreMetrics));
        BestSolutionPushes.Modified:=True;
        if   BetterBuiltInSolutionPushes=BuiltinBestSolutionMoves then   {'BuiltinBestSolutionMoves' may show up in either of the 'Better...' slots}
             BuiltInBestSolutionMoves :=nil
        else BuiltInBestSolutionPushes:=nil;
        Result:=True;
        end;

     if (BetterBuiltInSolutionMoves<>nil) and
        (BuiltinBestSolutionMoves=BetterBuiltInSolutionMoves) then begin {last check: just to be sure}
        if BestSolutionMoves<>nil then
           if BestSolutionPushes=nil then
              {temporarily promote the best-moves solution to the the best-pushes solution;}
              {if it doesn't hold, it will be degraded to a normal snapshot by the code a few lines further down}
              BestSolutionPushes:=BestSolutionMoves
           else with BestSolutionMoves do begin
              {degrade the old solution to a normal snapshot}
              BestSolutionMoves.SetName(Snapshots.MakeUniqueName(SnapshotTypeName(stSolution)+SPACE+
                                                                 Format(FORMAT_MOVES_AND_PUSHES,
                                                                        [MoveCount,PushCount])+
                                                                 SecondaryMetricsFormattedAsATitleSuffix(
                                                                   SecondaryMetricsInTitles,
                                                                   SecondaryScoremetrics)+
                                                                 SolverNameText(BestSolutionMoves.Name)
                                                                 {+NonOptimalText}
                                                                 ,
                                                                 SnapshotTypeName(stSolution),
                                                                 True));
              {
              if not BestSolutionMoves.Notes.Lines.FindKey(TEXT_OPTIMALITY,Node) then
                 BestSolutionMoves.Notes.Lines.WriteString(TEXT_OPTIMALITY,TEXT_NON_OPTIMAL);
              }
              Snapshots.InsertAfter(BestSolutionMoves,nil);
              end;
        BestSolutionMoves:=BetterBuiltinSolutionMoves;
        with BestSolutionMoves do SetName(SnapshotTypeName(stBestSolutionMoves)+SPACE+
                                          Format(FORMAT_MOVES_AND_PUSHES,
                                                 [MoveCount,PushCount])+
                                          SecondaryMetricsFormattedAsATitleSuffix(
                                            SecondaryMetricsInTitles,
                                            SecondaryScoremetrics));
        BestSolutionMoves.Modified:=True;
        BuiltInBestSolutionMoves:=nil;
        if (BestSolutionPushes<>nil) and
           (BestSolutionMoves.PushCount<=BestSolutionPushes.PushCount) then with BestSolutionPushes do begin
           {the old best solution/pushes is obsolete; degrade it to a normal snapshot}
           BestSolutionPushes.SetName(Snapshots.MakeUniqueName(SnapshotTypeName(stSolution)+SPACE+
                                                               Format(FORMAT_MOVES_AND_PUSHES,
                                                                      [MoveCount,PushCount])+
                                                               SecondaryMetricsFormattedAsATitleSuffix(
                                                                 SecondaryMetricsInTitles,
                                                                 SecondaryScoremetrics)+
                                                               SolverNameText(BestSolutionPushes.Name)
                                                               {+NonOptimalText}
                                                               ,
                                                               SnapshotTypeName(stSolution),
                                                               True));
           {
           if not BestSolutionPushes.Notes.Lines.FindKey(TEXT_OPTIMALITY,Node) then
              BestSolutionPushes.Notes.Lines.WriteString(TEXT_OPTIMALITY,TEXT_NON_OPTIMAL);
           }
           Snapshots.InsertAfter(BestSolutionPushes,nil);
           BestSolutionPushes:=nil;
           end;
        Result:=True;
        end;

     end;
end; {ImportBuiltinSolutions}

procedure TSokoGame.LoadBoard(const Board__:TBoard);
begin
  LoadBoard0(Board__);
end; {TSokoGame.LoadBoard}

procedure TSokoGame.LoadBoard0(const Board__:TBoard);
var i,Col,Row:Integer;
begin {precondition: 'Board__' is a legal board which is the result of moving boxes around after the game has been properly initialized; in particular, 'StartBoard' must be valid}
  Board:=Board__;
  for Col:=1 to BoardWidth do {find position for player and boxes...}
      for Row:=1 to BoardHeight do begin
          i:=Board[Col,Row];
          Board[Col,Row]:=(i and (not (BOX_ILLEGAL_MOVE+BOX_LEGAL_MOVE+PLAYER_LEGAL_MOVE+SQUARE_SET)))
                          or
                          (StartBoard[Col,Row] and (ILLEGAL_SQUARE+INVISIBLE_WALL+PLAYER_UNREACHABLE_FLOOR)); {note that 'StartBoard' must be valid}
          if (i and PLAYER)<>0 then begin
             PlayerPos.x:=Col; PlayerPos.y:=Row;
             end;
          i:=i shr BOARD_FLAG_COUNT;
          if i<>0 then begin BoxPos[i].x:=Col; BoxPos[i].y:=Row; end;
          end;

  SetReverseMode(ReverseMode); {ensure that target-masks are set correctly}
  CalculateScoreAndState;
end; {TSokoGame.LoadBoard0}

procedure TSokoGame.LoadBoxes(PlayerPos__:TColRow; BoxPos__:PBoxPositions; ReverseMode__:Boolean);
var BoxNo:Integer;
begin
  with PlayerPos do Board[X,Y]:=Board[X,Y] and (not PLAYER);
  for BoxNo:=1 to BoxCount do with BoxPos[BoxNo] do
      Board[X,Y]:=Board[X,Y] and (BOARD_FLAGS_MASK - BOX - BOX_ILLEGAL_MOVE - BOX_LEGAL_MOVE - PLAYER_LEGAL_MOVE - SQUARE_SET); {remove player, boxes, and box-numbers}

  for BoxNo:=1 to BoxCount do with BoxPos[BoxNo] do begin
      X:=BoxPos__^[BoxNo].X; Y:=BoxPos__^[BoxNo].Y;
      Board[X,Y]:=((Board[X,Y] and BOARD_FLAGS_MASK) or BOX)+(BoxNo shl BOARD_FLAG_COUNT);
      end;
  if (PlayerPos__.X<>0) and (PlayerPos__.Y<>0) then with PlayerPos do begin
     X:=PlayerPos__.X; Y:=PlayerPos__.Y;
     Board[X,Y]:=Board[X,Y] or PLAYER;
     end
  else with PlayerPos do begin
     X:=0; Y:=0;
     end;

  SetReverseMode(ReverseMode__); {set target-masks correctly}
  CalculateScoreAndState;
end; {TSokoGame.LoadBoxes}

function TSokoGame.LoadFromBoardAsText(BoardWidth__,BoardHeight__:Integer; NormalizeBoard__,MovePlayerAndBoxes__,ChangeImmovableBoxesToWalls__,FillUnnecessaryIllegalBoxSquares__:Boolean; const BoardAsText__:String):Boolean;
begin {Precondition: input parameters contain a valid board}
  Clear;
  Result:=TextToBoard(BoardWidth__,BoardHeight__,BoardAsText__,Board);
  if   Result then begin
       BoardWidth:=BoardWidth__; BoardHeight:=BoardHeight__;
       if NormalizeBoard__ then NormalizeBoard(MovePlayerAndBoxes__,ChangeImmovableBoxesToWalls__,ChangeImmovableBoxesToWalls__,FillUnnecessaryIllegalBoxSquares__,True,BoardWidth,BoardHeight,Board,PlayerPos,History);
       CalculateInternalData;
       end
  else Clear;
end; {TSokoGame.LoadFromBoardAsText}

function TSokoGame.LoadFromFile(const LevelName__:String; SokoFile__:TSokoFile; Flush__:Boolean; var ErrorStr__:String):Boolean;
var OriginalSimpleIllegalMovesMask:Integer; FileName:String; Level:TLevel;

  function LoadLevel(Level__:TLevel; SokoFile__:TSokoFile; var ErrorStr__:String):Boolean;
  var s:String;
      Node:TNode;
      NextSnaphotAsText,SnapshotAsText:TSnapshotAsText;
      Snapshot:TSnapshot;
  begin
    SokoFile:=SokoFile__; SokoFileName:=SokoFile__.Name;        {save original filename; note: 'SokoFile' must stay alive until the level is closed}
    if   StrEqual(Level__.Name,TEXT_LEVEL) then begin           {special: if name = 'Level' then use filename as display-name}
         DisplayName:=ExtractFileNameWithoutPathAndExtension(SokoFile__.Name);
         if IsBlank(DisplayName) then DisplayName:=Level__.Name;
         end
    else DisplayName:=Level__.Name;

    Result:=SetName(Level__.Name) and              {self.name := level.name}
            Level__.Notes.CopyTo(Notes);           {load notes}
    Notes.Modified:=False;
    Notes.MacroExpand(SokoFile.FileHeader.Lines,C_STYLE_NEWLINE);

    if    Notes.Lines.ReadString(KEY_TIME,s) and
          SokUtil_.StrToTime(s,OriginalTimeMS) then{total time is calculated during the game as 'OriginalTimeMS' + 'SessionTimeMS'}
    else  OriginalTimeMS:=0;

    if    Notes.Lines.ReadString(KEY_BOARD_TRANSFORMATION,s) then
          SokUtil_.StrToBoardTransformation2D(AnsiLowerCase(s), {note: 's' and the keys must have the same case}
                                              KEY_BOARD_TRANSFORMATION_ROTATE_COUNTER_CLOCKWISE,
                                              KEY_BOARD_TRANSFORMATION_FLIP_VERTICALLY,
                                              KEY_BOARD_TRANSFORMATION_FLIP_HORIZONTALLY,
                                              OriginalBoardTransformation2D); {save original transformation; used in 'CloseLevel' to detect changes}

    if Result then                                 {load and validate the board, and calculate internal data}
       if   Level__.TextLinesToBoard(Board,BoardWidth,BoardHeight,BoxCount,GoalCount,PlayerPos,ErrorStr__) then begin
            {RemoveRedundantWalls;}                {if redundant walls should be removed, it must be done before calculating internal data}
            CalculateInternalData;                 {board ok}
            end
       else Result:=False;                         {fail, return 'ErrorStr__' as explanation}

    SnapshotAsText:=TSnapshotAsText(Level__.SnapshotsAsText.First);
    while (SnapshotAsText<>nil) and Result do begin{load snapshots, and check them for legal moves}
      NextSnaphotAsText:=TSnapshotAsText(SnapshotAsText.Next);
      if      LoadSnapshotAsText(SnapshotAsText,SokoFile__.FileHeader.Lines,Snapshot) then {'True': the text-format snapshot was legal and the internal format snapshot has been created}
      else if History.Top>MAX_MOVES then begin {'True': the text-format snapshot has too many moves to be loaded into the game history}
              {the text-format snapshot may be a legal snapshot from a program without
               the history size limit imposed by this application;
               avoid loosing the snapshot by copying it to the level notes;
              }

              {make snapshot header}
              Result:=Notes.Lines.AddBlankLine and
                      (Notes.Lines.AddTextLine(SnapshotAsText.Name,False)<>nil);

              {copy snapshot move lines}
              Node:=SnapshotAsText.MovesAsTextLines.First;
              while (Node<>nil) and Result do begin
                Result:=Notes.Lines.AddTextLine(Node.Text,False)<>nil;
                Node:=Node.Next;
                end;

              {copy snapshot notes, if any}
              Node:=SnapshotAsText.Notes.Lines.First;
              if Node<>nil then begin {'True': the snapshot has notes}
                 Result:=Result and Notes.Lines.AddBlankLine;
                 while (Node<>nil) and Result do begin
                   Result:=Notes.Lines.AddTextLine(Node.Text,False)<>nil;
                   Node:=Node.Next;
                   end;
                 end;
              if Result then Notes.Modified:=True;
              end;
      SnapshotAsText:=NextSnaphotAsText;           {advance to the next text-format snapshot}
      end;
    Snapshots.Reverse;                             {snapshots are in reversed order: make it right}
    OriginalSnapshotCount:=Snapshots.Count;        {save the number of snapshots; it's used in 'CloseLevel' to detect changes}

    {$IFDEF SokobanYASC}
      if (Self=MainForm.Game) and Assigned(MainForm.MultiView) then OriginalMultiViewCount:=MainForm.MultiView.Items.Count;
    {$ENDIF}

    if BestSolutionMoves <>nil then                {save names of original best solutions so they can be found again later}
       OriginalBestSolutionMovesName :=BestSolutionMoves .Name;
    if BestSolutionPushes<>nil then
       OriginalBestSolutionPushesName:=BestSolutionPushes.Name;

    if Result then begin
       UpdateBestSolutionNames;                    {update the best solution names so they match the current settings and appear on the 'Snapshots' window in a standardized way}
       DoBoardTransformation2D(OriginalBoardTransformation2D,False); {transform the board so it matches last seen view}
       Result:=ResetAndLoadSaveGame(RestoreSaveGame,ResetSaveGameAndLoadItIfItIsANormalModeGame);
       end;

    ClearTimer;
  end; {LoadLevel}

begin {LoadFromFile}
  FileName:=SokoFile__.Name;
  Result:=CloseLevel(Flush__) and SokoFile__.Open(FileName);

  Level:=nil; ErrorStr__:='';
  OriginalSimpleIllegalMovesMask:=SimpleIllegalMovesMask;
  if Result then
     try     Clear;
             IsLoading:=True;
             KeepOriginalSnapshotNames:=True; {let 'TestForNewBestSolution' keep the original best solution names, so this method can collect the original names after creating the internal 'TSnapshot' representations of the best solutions}
             SimpleIllegalMovesMask:=0; {load snapshots without blocking illegal moves}
             try    if      SokoFile__=nil then raise Exception.Create(TEXT_NO_FILE_SPECIFIED)
                    else if SokoFile__.Levels<>nil then
                            Level:=TLevel(SokoFile__.Levels.GetItemByName(LevelName__));
                    if   Level<>nil then
                         if   LoadLevel(Level,SokoFile__,ErrorStr__) then {ok}
                         else if   ErrorStr__<>'' then
                                   raise Exception.Create(ErrorStr__)
                              else Result:=False {error has been reported}
                    else raise Exception.Create(Format(TEXT_LEVEL_NOT_FOUND_FORMAT,[SokoFile__.Name+SUB_TITLE_SEPARATOR+LevelName__]));
             except on E:Exception do begin
                       ErrorStr__:=E.Message;
                       ErrorStr__:=ErrorStr__+NL+NL+FileName;
                       if Assigned(Level) and (Level.Name<>'') and (Level.Name<>TEXT_LEVEL) then ErrorStr__:=ErrorStr__+NL+NL+Level.Name;
                       if Verbose then SokUtil_.Error({E.Message}ErrorStr__,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_OPEN_LEVEL);
                       Result:=False;
                       end;
             end;
     finally IsLoading:=False; KeepOriginalSnapshotNames:=False;
             SimpleIllegalMovesMask:=OriginalSimpleIllegalMovesMask;
             if not Result then Clear;
     end;
end; {TSokoGame.LoadFromFile}

function  TSokoGame.LoadSnapshot(Snapshot__:TSnapshot):Boolean;
begin
  Result:=LoadSnapshot0(Snapshot__);
end; {TSokoGame.LoadSnapshot}

function  TSokoGame.LoadSnapshot0(Snapshot__:TSnapshot):Boolean;
var OldReverseMode:Boolean; OldHistory:THistory;

  function TryToAddContinuation(const OldHistory__:THistory):Boolean;
  var i:Integer;
  begin
     Result:=True;
     for i:=1 to History.Top do                 {check if the old game is identical to this snapshot}
         if   Result then
              Result:=(History     .Moves[i] and (H_MASK_DIRECTION+H_FLAG_BOX+H_FLAG_JUMP))=
                      (OldHistory__.Moves[i] and (H_MASK_DIRECTION+H_FLAG_BOX+H_FLAG_JUMP))
         else break;

     if Result then with History do begin       {the old moves are a legal continuation;}
        for i:=Succ(Top) to OldHistory__.Top do {retain them so the user still can replay them}
            Moves[i]:=OldHistory__.Moves[i];
        Top:=OldHistory__.Top;
        end;
  end;

begin {LoadSnapshot0}
  Result:=(Snapshot__<>nil) and (Snapshot__.Moves<>nil);
  if Result then with History do begin
     OldReverseMode   :=ReverseMode;
     OldHistory       :=History;

     Count            :=Snapshot__.MoveCount;
     Top              :=Snapshot__.MoveTop;
     System.Move(Snapshot__.Moves^,Moves[Low(Moves)],Snapshot__.ByteSize(Top));
     PushCount        :=Snapshot__.PushCount;
     ReverseMode      :=Snapshot__.ReverseMode;
     LastBoxNo        :=Snapshot__.LastBoxNo;
     LastPushIndex    :=Snapshot__.LastPushIndex;
     PlayerLinesCount :=Snapshot__.SecondaryScoreMetrics.PlayerLines;
     LoadBoxes(Snapshot__.PlayerPos,PBoxPositions(Addr(Snapshot__.BoxPos)),Snapshot__.ReverseMode);

     if (OldReverseMode=ReverseMode) and
        (OldHistory.Top>History.Top) then TryToAddContinuation(OldHistory);

     if (SaveGame<>nil) and
        (SaveGame.ReverseMode=ReverseMode) and
        (SaveGame.MoveTop>History.Top) then begin
        OldHistory.Top:=SaveGame.MoveTop;
        System.Move(SaveGame.Moves^,OldHistory.Moves[Low(OldHistory.Moves)],SaveGame.ByteSize(SaveGame.MoveTop));
        TryToAddContinuation(OldHistory);
        end;

     SeparateUndoneMoves;
     end;
end; {TSokoGame.LoadSnapshot0}

function TSokoGame.LoadSnapshotAsTextString(const SnapshotAsTextString__:String; IgnoreIllegalMoves__:Boolean):Boolean;
var i,dx,dy,oSimpleIllegalMovesMask:Integer; IsAFreezingMove:Boolean; Direction:TDirection;
begin {Precondition: 'SnapshotAsText__ contains a valid forward game snapshot, or 'IgnoreIllegalMoves' = 'True'; Postcondition: the game has been reset and is ready to play a forward game}
  Result:=Length(SnapshotAsTextString__)<=MAX_MOVES;
  SetReverseMode(False); History.Top:=0; Reset(False);
  oSimpleIllegalMovesMask:=SimpleIllegalMovesMask;
  try     SimpleIllegalMovesMask:=0;
          for  i:=0 to Pred(Length(SnapshotAsTextString__)) do
               if   Result then
                    if   CharToDirection(SnapShotAsTextString__[STRING_BASE+i],Direction) and
                         HistoryMoveToDxDy(Ord(Direction),dx,dy) then
                         if   IsALegalMove(dx,dy,0,IsAFreezingMove) then
                              DoMoveUpdateGame(dx,dy,History.Count,0)
                         else Result:=IgnoreIllegalMoves__ {after "tube-filling" a board, some redundant non-pushing player moves may not be possible anymore; they are silently dropped when 'IgnoreIllegalMoves__' is 'True'}
                    else Result:=False
               else break;
  finally SimpleIllegalMovesMask:=oSimpleIllegalMovesMask;
          if not Result then History.Top:=0;
          Reset(False); RenumberCombinedMoves;
  end;
end; {TSokoGame.LoadSnapshotAsTextString}

function TSokoGame.LoadSnapshotAsText(SnapshotAsText__     :TSnapshotAsText;  {throws EOutOfMemory}
                                      Macros__             :TList;
                                      var Snapshot__       :TSnapshot):Boolean;
{side-effect: if 'History.Top' > MAX_MOVES when the function returns, then
 the text-format snapshot contains too many moves to be loaded into the
 game history;
}
var i,dx,dy,OriginalSimpleIllegalMovesMask,
    SnapshotLastBoxNo,SnapshotLastPushIndex,SnapshotMoveCount,SnapshotMoveTop:Integer;
    IsABuiltInSolutionName,IsAFreezingMove,OriginalIsLoading:Boolean;
    Move:THistoryMove;
    SnapshotPlayerPos:TColRow; SnapshotBoxPos:TBoxPositions;
    OldBestSolutionMoves,OldBestSolutionPushes:TSnapshot;
    SecondaryScoreMetrics:TSecondaryScoreMetrics;
    {$IFDEF SokHtml}
      MovePlayerBackMoveCount,MovePlayerBackLinesCount,MovePlayerBackToStart:Integer;
      IsPreferredDirectionOK:Boolean;
      Direction:TDirection;
      MovePlayerBackMoves:TPlayerMoves;
    {$ENDIF}

begin
  Result:=True;

  Snapshot__:=nil; History.Top:=0;
  SnapshotLastBoxNo:=0; SnapshotLastPushIndex:=0; SnapshotMoveCount:=0;
  FillChar(SecondaryScoreMetrics,SizeOf(SecondaryScoreMetrics),0);
  {$IFDEF SokHtml} MovePlayerBackToStart:=0; {$ENDIF}

  OldBestSolutionMoves :=BestSolutionMoves;
  OldBestSolutionPushes:=BestSolutionPushes;

  IsABuiltInSolutionName:=StrBeginsWith(SnapshotAsText__.Name,SnapshotTypeName(stBuiltinBestSolutionMoves )) or
                          StrBeginsWith(SnapshotAsText__.Name,SnapshotTypeName(stBuiltinBestSolutionPushes));

  if IsABuiltInSolutionName then begin                          {trick 'TestForNewBestSolution into updating built-in solutions}
     BestSolutionMoves :=BuiltInBestSolutionMoves;
     BestSolutionPushes:=BuiltInBestSolutionPushes;
     end;

  if Result then
     Result:=SnapshotAsText__.TextLinesToMoves(History,ReverseMode {$IFDEF SokHtml} ,MovePlayerBackToStart {$ENDIF}); {text -> internal history-format}

  OriginalIsLoading:=IsLoading;
  OriginalSimpleIllegalMovesMask:=SimpleIllegalMovesMask;

  if Result then                                                {check for legal moves by replaying the game}
     try
             SetReverseMode(ReverseMode);
             SimpleIllegalMovesMask  :=0;                       {load snapshots without blocking illegal moves}
             IsLoading               :=True;
             SnapshotMoveCount       :=History.Count;           {current position in the snapshot}
             SnapshotMoveTop         :=History.Top;
             SnapshotLastBoxNo       :=0;
             SnapshotLastPushIndex   :=0;
             History.Top:=0; Reset(True);                       {reset the game}
             if History.Count=SnapshotMoveCount then begin      {if this is the current position in the snapshot}
                SnapshotBoxPos:=BoxPos;                         {then take a snapshot of the board}
                SnapshotPlayerPos:=PlayerPos;
                end;
             Move:=History.Moves[Succ(History.Count)];          {if 'Count' >= 1: in reverse mode, the player may have left a box-position}
             dx:=DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),ColAxis];
             dy:=DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),RowAxis];

             while (History.Count<SnapshotMoveTop) and
                   IsALegalMove(dx,dy,Move,IsAFreezingMove) do begin {as long as next move is legal, do it}
               DoMoveUpdateGame(dx,dy,-1,Move);

               if History.Count<=SnapshotMoveCount then begin
                  {calculate secondary metrics;}
                  {it's the same calculation as in 'CalculateSecondaryScoreMetrics'}
                  {but it's inlined here in order to avoid an extra pass through the moves}
                  Move:=History.Moves[History.Count];
                  if   (Move and H_FLAG_BOX)<>0 then begin      {'True': pushing a box}
                       if      (History.Moves[Pred(History.Count)] and H_FLAG_BOX)=0 then {'True': the previous move wasn't a push}
                               Inc(SecondaryScoreMetrics.PushingSessions);
                       if      (Move and H_FLAG_BOX_CHANGE)<>0 then begin {'True': the pushed box is different from the last pushed box}
                               Inc(SecondaryScoreMetrics.BoxChanges); Inc(SecondaryScoreMetrics.BoxLines);
                               end
                       else if TDirection(Move and H_MASK_DIRECTION)<>
                               TDirection(History.Moves[SnapshotLastPushIndex] and H_MASK_DIRECTION) then {'True': changing direction}
                               Inc(SecondaryScoreMetrics.BoxLines);
                       SnapshotLastPushIndex:=History.Count;
                       end;
                  if   History.Count>1 then begin
                       if TDirection(Move and H_MASK_DIRECTION)<>
                          TDirection(History.Moves[Pred(History.Count)] and H_MASK_DIRECTION) then
                          Inc(SecondaryScoreMetrics.PlayerLines);
                       end
                  else SecondaryScoreMetrics.PlayerLines:=1;

                  if History.Count=SnapshotMoveCount then begin {current position in the snapshot:}
                     SnapshotBoxPos:=BoxPos;                    {take a snapshot of the board}
                     SnapshotPlayerPos:=PlayerPos;
                     SnapshotLastBoxNo:=History.LastBoxNo;
                     SnapshotLastPushIndex:=History.LastPushIndex;
                     end;

                  {$IFDEF SokHtml}
                    if History.Count>=MovePlayerBackToStart then begin
                       if (History.Count=MovePlayerBackToStart) and
                          PlayerPath(PlayerPos,PlayerStartPos,True,False,Up,MovePlayerBackMoveCount,MovePlayerBackLinesCount,IsPreferredDirectionOK,PPlayerMoves(Addr(MovePlayerBackMoves))) and
                          (SnapshotMoveTop+MovePlayerBackMoveCount<=MAX_MOVES) and
                          (MovePlayerBackMoveCount>0)
                          then begin {insert the "move player back to start position" moves here}
                          for i:=SnapshotMoveTop downto Succ(History.Count) do
                              History.Moves[i+MovePlayerBackMoveCount]:=History.Moves[i];
                          Inc(SnapshotMoveTop,MovePlayerBackMoveCount);

                          MovePlayerBackMoves[0]:=PlayerPos; {current player position}
                          for i:=1 to MovePlayerBackMoveCount do
                              if   (SnapshotMoveTop>0) and
                                   DxDyToDirection(MovePlayerBackMoves[i].X-MovePlayerBackMoves[Pred(i)].X,
                                                   MovePlayerBackMoves[i].Y-MovePlayerBackMoves[Pred(i)].Y,
                                                   Direction) then
                                   History.Moves[History.Count+i]:=Ord(Direction)
                              else SnapshotMoveTop:=0; {failed}

                          if SnapshotMoveCount>=History.Count then
                             Inc(SnapshotMoveCount,MovePlayerBackMoveCount);
                          end;

                       if (GameState=gsSolved) and (History.Count<SnapshotMoveTop) then begin {'True': make the remaining moves even though the level already has been solved}
                          GameState:=gsPlay;
                          if      BestSolutionMoves<>OldBestSolutionMoves then begin   {'True': current game was saved as best solution/moves; delete it so a new one will be created, including the superflous moves}
                                  Snapshots.Remove(BestSolutionMoves,True); {note: the solution doesn't need to be a list member to be destroyed}
                                  BestSolutionMoves:=nil;
                                  end
                          else if BestSolutionPushes<>OldBestSolutionPushes then begin {'True': current game was saved as best solution/pushes; delete it so a new one will be created, including the superflous moves}
                                  Snapshots.Remove(BestSolutionPushes,True); {note: the solution doesn't need to be a list member to be destroyed}
                                  BestSolutionPushes:=nil;
                                  end;
                          end;
                       end;
                  {$ENDIF}
                  end;

               Move:=History.Moves[Succ(History.Count)];        {next move}
               dx:=DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),ColAxis];
               dy:=DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),RowAxis];
               end; {end while more legal moves}

             if SnapshotMoveCount>History.Top then begin        {'True': the current position marker, '*', is after the legal moves;}
                SnapshotMoveCount:=History.Top;                 {set current position to last legal move}
                SnapshotBoxPos:=BoxPos;                         {take a snapshot of current position}
                SnapshotPlayerPos:=PlayerPos;
                SnapshotLastBoxNo:=History.LastBoxNo;
                SnapshotLastPushIndex:=History.LastPushIndex;
                end;

             Result:=History.Top>0;                             {drop empty snapshots}
     finally
             IsLoading:=OriginalIsLoading;                      {restore settings}
             SimpleIllegalMovesMask:=OriginalSimpleIllegalMovesMask;
     end;

  if Result then
     if IsABuiltInSolutionName then begin                       {filter out built-in best solutions}
        if        BuiltInBestSolutionMoves<>BestSolutionMoves then begin   {'True': current game was saved as best solution/moves}
                  Snapshot__:=BestSolutionMoves;
                  end
        else if   BuiltInBestSolutionPushes<>BestSolutionPushes then begin {'True': current game was saved as best solution/pushes}
                  Snapshot__:=BestSolutionPushes;
                  end
             else Result:=False;                                {the snapshot is broken/obsolete, drop it}

        BuiltInBestSolutionMoves :=BestSolutionMoves;           {save built-in best solutions}
        BuiltInBestSolutionPushes:=BestSolutionPushes;
        BestSolutionMoves        :=OldBestSolutionMoves;        {restore best user solutions}
        BestSolutionPushes       :=OldBestSolutionPushes;

        if BuiltInBestSolutionMoves <>nil then                  {don't add 'moves/pushes' numbers because 'TSokoFile.MergeFile' doesn't take it into account}
           Result:=Result and BuiltInBestSolutionMoves .SetName(SnapshotTypeName(stBuiltinBestSolutionMoves ){+SPACE+Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])});
        if BuiltInBestSolutionPushes<>nil then                  {don't add 'moves/pushes' numbers because 'TSokoFile.MergeFile' doesn't take it into account}
           Result:=Result and BuiltInBestSolutionPushes.SetName(SnapshotTypeName(stBuiltinBestSolutionPushes){+SPACE+Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])});
        end
     else                                                       {filter out user's best solutions}
        if      BestSolutionMoves <>OldBestSolutionMoves  then begin {'True': current game was saved as best solution/moves}
                Snapshot__:=BestSolutionMoves;
                Result:=BestSolutionMoves .SetName(SnapshotAsText__.Name); {use original name}
                end
        else if BestSolutionPushes<>OldBestSolutionPushes then begin {'True': current game was saved as best solution/pushes}
                Snapshot__:=BestSolutionPushes;
                Result:=BestSolutionPushes.SetName(SnapshotAsText__.Name); {use original name}
                end
             else begin                                         {make internal version of this snapshot, i.e., take a snapshot of current position}
                RenumberCombinedMoves;                          {optimize the combined move grouping}
                if   (GameState=gsSolved)
                     and
                     (SnapshotMoveCount=History.Top)            {'True': it's not a snapshot in the middle of a solution}
                     and
                     (((not ReverseMode) and StrBeginsWith(SnapshotAsText__.Name,SnapshotTypeName(stSnapshot)))
                      or
                      (     ReverseMode  and StrBeginsWith(SnapshotAsText__.Name,SnapshotTypeName(stReverseSnapshot)))
                     ) then
                     {change name from 'Snapshot' to 'Solution'; 'MakeSnapshot' fills in the name automatically when the function is called with a blank name}
                     Snapshot__:=MakeSnapshot('')
                else Snapshot__:=MakeSnapshot(SnapshotAsText__.Name);
                Result:=Snapshot__<>nil;

                if Result then begin
                   Snapshot__.BoxPos       :=SnapshotBoxPos;
                   Snapshot__.PlayerPos    :=SnapshotPlayerPos;
                   Snapshot__.LastBoxNo    :=SnapshotLastBoxNo;
                   Snapshot__.LastPushIndex:=SnapshotLastPushIndex;
                   Snapshot__.MoveCount    :=SnapshotMoveCount;
                   Snapshot__.PushCount    :=0;
                   for i:=1 to Snapshot__.MoveCount do
                       if (Snapshot__.Moves[i] and H_FLAG_BOX)<>0 then
                          Inc(Snapshot__.PushCount);            {count pushes (or pulls in reverse mode)}
                   if   (SaveGame=nil) and                      {filter out savegame}
                        StrBeginsWith(StrWithoutBrackets(Snapshot__.Name),SnapshotTypeName(stSaveGame)) then
                        SaveGame:=Snapshot__
                   else Snapshots.Push(TNode(Snapshot__));      {add a normal snapshot to the list}
                   Snapshot__.SecondaryScoreMetrics:=SecondaryScoreMetrics; {save secondary statistics}
                   end;
                end;

  if Result then begin
     Result:=SnapshotAsText__.Notes.CopyTo(TNode(Snapshot__.Notes)); {load notes}
     Snapshot__.Notes.Modified:=False;
     end;
end; {TSokoGame.LoadSnapshotAsText}

function  TSokoGame.LookupSnapshot(ReverseMode__:Boolean; MoveCount__,MoveTop__:Integer; Moves__:PHistoryMoves; var Snapshot__:TSnapshot):Boolean;
begin
  if        Assigned(BestSolutionMoves)  and BestSolutionMoves .HasIdenticalMoves(ReverseMode__,MoveCount__,MoveTop__,Moves__) then begin
            Result:=True; Snapshot__:=BestSolutionMoves;
            end
  else if   Assigned(BestSolutionPushes) and BestSolutionPushes.HasIdenticalMoves(ReverseMode__,MoveCount__,MoveTop__,Moves__) then begin
            Result:=True; Snapshot__:=BestSolutionPushes;
            end
       else begin Result:=False;
                  Snapshot__:=TSnapshot(Snapshots.First);
                  while Assigned(Snapshot__) and (not Result) do
                    if   Snapshot__.HasIdenticalMoves(ReverseMode__,MoveCount__,MoveTop__,Moves__) then
                         Result:=True
                    else Snapshot__:=TSnapshot(Snapshot__.Next);
            end;
end; {TSokoGame.LookupSnapshot}

procedure TSokoGame.MakeCombinedMove(FromIndex__,ToIndex__:Integer);
var i,Separator:Integer;
begin {Makes a combined move from individual moves}
  Separator:=H_FLAG_ODD and (not History.Moves[Pred(FromIndex__)]); {flip the odd-move-number flag}
  if FromIndex__<=ToIndex__ then with History do
     for i:=FromIndex__ to ToIndex__ do begin
         Moves[i]:=(Moves[i] and (not H_MASK_MOVE_SEPARATOR)) or Separator;
         if (i<ToIndex__) and
            ((Moves[i      ] and H_FLAG_JUMP)<>
             (Moves[Succ(i)] and H_FLAG_JUMP)) then
             Separator:=H_FLAG_ODD and (not Separator);             {separate jumps from normal moves}
         end;
end;

function TSokoGame.MakeSnapshot(const Name__:String):TSnapshot;
begin
  Result:=MakeSnapshot0(Name__);
end; {TSokoGame.MakeSnapshot}

function TSokoGame.MakeSnapshot0(const Name__:String):TSnapshot;
var i:Integer; Name:String;
begin {Makes a new snapshot based on current position}
  Result:=nil;
  try    Name:=Name__;
         if Name='' then begin {blank name: make a default name (always unique)}
            if        GameState=gsSolved then
                      if   ReverseMode then
                           Name:=TEXT_REVERSE_MODE+SPACE+SnapshotTypeName(stSolution)
                      else Name:=SnapshotTypeName(stSolution)
            else if   ReverseMode then Name:=SnapshotTypeName(stReverseSnapshot)
                 else Name:=SnapshotTypeName(stSnapshot);
            Name:=Snapshots.MakeUniqueName(Name+SPACE+
                                           Format(FORMAT_MOVES_AND_PUSHES,[History.Count,History.PushCount])+
                                           SecondaryMetricsAsText,
                                           Name,
                                           True);
            end;

         Result                         :=TSnapshot.Create;
         if   (Name<>'') and
              Result.SetName(Name)      then begin
              Result.BoxPos             := BoxPos;
              Result.ForcedInitialJumps := ForcedInitialJumps;
              Result.GameState          := GameState;
              Result.LastBoxNo          := History.LastBoxNo;
              Result.LastPushIndex      := History.LastPushIndex;
              Result.Modified           := not IsLoading;
              Result.MoveCapacity       := History.Top;
              Result.MoveCount          := History.Count;
              Result.MoveTop            := History.Top;
              Result.PlayerPos          := PlayerPos;
              Result.PushCount          := History.PushCount;
              i                         := Result.ByteSize(Result.MoveCapacity);
              GetMem(Result.Moves,i);
              System.Move(History.Moves[Low(History.Moves)],Result.Moves^,i); {move: source, destination, bytes}
              Result.ReverseMode     :=ReverseMode;
              CalculateSecondaryScoreMetrics(Result.SecondaryScoreMetrics);
              end
         else raise Exception.Create(TEXT_MEMORY_FULL);
  except on   E:Exception do begin
              Result.Free; Result :=nil;
              SokUtil_.Error(E.Message,'TSokoGame.MakeSnapshot');
              end;
  end;
end; {TSokoGame.MakeSnapshot0}

function TSokoGame.NonOptimalText:String;
begin {The text that is appended to the title of non-optimal solutions}
  Result:=SPACE+LEFT_PAREN+TEXT_NON_OPTIMAL+RIGHT_PAREN;
end; {TSokoGame.NonOptimalText}

function TSokoGame.ObjectType:TObjectType;
begin
  Result:=otSokoGame;
end; {TSokoGame.ObjectType}

function TSokoGame.PlayerDirection(MoveIndex__:Integer):TDirection; {the player is "looking" in this direction}
begin
  with History do
    if   (MoveIndex__>0) and (MoveIndex__<=Top) then
         if   not (ReverseMode and ((Moves[MoveIndex__] and H_FLAG_BOX)<>0)) then
              Result:=                   TDirection(Moves[MoveIndex__] and H_MASK_DIRECTION)
         else Result:=OPPOSITE_DIRECTION[TDirection(Moves[MoveIndex__] and H_MASK_DIRECTION)]
    else Result:=DEFAULT_PLAYER_DIRECTION;
end; {TSokoGame.PlayerDirection}

(*
function TSokoGame.PlayerJump(const FromPos__,ToPos__:TColRow; MakeMoves__:Boolean;
                              var MoveCount__:Integer; var Moves__:TMoves):Boolean;
var i,j,k:Integer;
begin
  Result:=(Board[ToPos__.X,ToPos__.Y] and FLOOR)<>0; {target square must be a floor, i.e., not a wall and not a box}
  if Result then begin
     MoveCount__:=0;
     j:=ToPos__.X-FromPos__.X; k:=j; if k<>0 then k:=k div Abs(k);
     for i:=1 to Abs(j) do begin
         Inc(MoveCount__);
         Moves__[MoveCount__].X:=FromPos__.X+i*k;
         Moves__[MoveCount__].Y:=FromPos__.Y+MOVE_LIST_FLAG_JUMP;
         end;
     j:=ToPos__.Y-FromPos__.Y; k:=j; if k<>0 then k:=k div Abs(k);
     for i:=1 to Abs(j) do begin
         Inc(MoveCount__);
         Moves__[MoveCount__].X:=ToPos__  .X;
         Moves__[MoveCount__].Y:=FromPos__.Y+i*k+MOVE_LIST_FLAG_JUMP;
         end;
     end;
end; {TSokoGame.PlayerJump}
*)

function TSokoGame.PlayerJump(const FromPos__,ToPos__:TColRow; MakeMoves__:Boolean;
                              var MoveCount__,PlayerLinesCount__:Integer; var Moves__:TMoves):Boolean;
var i,dx,dy,UndoCount:Integer; b:Boolean; FromPos:TColRow;
begin
  UndoCount:=0; FromPos:=FromPos__;                            {local version of 'FromPos__' to work on}
  if  (FromPos.X=PlayerPos.X) and (FromPos.Y=PlayerPos.Y) then {is 'FromPos' current position of the player?}
      while (History.Count-UndoCount>ForcedInitialJumps) and
            ((Board[FromPos.X,FromPos.Y] and WALL)<>0) do begin{is player on top of a wall?}
            HistoryMoveToDxDy(History.Moves[History.Count-UndoCount],dx,dy);
            Dec(FromPos.X,dx); Dec(FromPos.Y,dy);              {undo moves to get away from wall squares}
            Inc(UndoCount);                                    {because 'PlayerPath' only works with player on floor-squares}
            end;

  Result:=((Board[FromPos.X,FromPos.Y] and FLOOR)<>0) and      {squares must be proper floor squares,}
          ((Board[ToPos__.X,ToPos__.Y] and FLOOR)<>0) and      {i.e., not walls, and not isolated, un-reachable "islands"}
          ((History.PushCount=0)                               {jumps must be allowed in current position}
           or JumpsAllowedAfterFirstBoxMoveInReverseMode
          );
  if Result then begin
     for i:=1 to BoxCount do with BoxPos[i] do                 {remove all boxes from the board}
         Board[x,y]:=Board[x,y] and (not BOX);
     {jumps must use proper floor-squares because the rest of the kernel}
     {doesn't support walking over squares not reachable by the player}
     Result:=PlayerPath(FromPos,ToPos__,False,(History.Count-UndoCount)>0,TDirection(History.Moves[History.Count-UndoCount] and H_MASK_DIRECTION),MoveCount__,PlayerLinesCount__,b,PPlayerMoves(Addr(Moves__)));
     for i:=1 to BoxCount do with BoxPos[i] do                 {place all boxes on the board again}
         Board[x,y]:=Board[x,y] or BOX;

     Result:=Result and (MoveCount__<=High(Moves__)-UndoCount);

     if Result and (UndoCount>0) then begin
        Inc(MoveCount__,UndoCount);
        for i:=MoveCount__ downto Succ(UndoCount) do           {move player path moves upwards}
            Moves__[i]:=Moves__[i-UndoCount];

        FromPos:=FromPos__;
        for i:=UndoCount downto 1 do begin                     {insert undo-moves, to get away from wall-squares}
            HistoryMoveToDxDy(History.Moves[History.Count-UndoCount+i],dx,dy);
            Dec(FromPos.X,dx); Dec(FromPos.Y,dy);
            Moves__[Succ(UndoCount-i)].X:=FromPos.X;
            Moves__[Succ(UndoCount-i)].Y:=FromPos.Y;
            end;
        end;

     if Result then begin
        for i:=1 to MoveCount__ do
            Inc(Moves__[i].Y, MOVE_LIST_FLAG_JUMP);            {the y-coordinate carries the jump-flag}
        end;
     end;
end; {TSokoGame.PlayerJump}

function TSokoGame.PlayerLegalJumps:Integer;
var i,Col,Row:Integer;
begin
  Result:=0;
  if ReverseMode then
     for Col:=1 to BoardWidth do
         for Row:=1 to BoardHeight do begin
             i:=Board[Col,Row];
             if   (i and (WALL+BOX+FLOOR))=FLOOR then begin
                  Board[Col,Row]:=i or PLAYER_LEGAL_MOVE; Inc(Result);
                  end
             else Board[Col,Row]:=i and (not PLAYER_LEGAL_MOVE)
             end;
end; {TSokoGame.PlayerLegalJumps}

function TSokoGame.PlayerLegalMoves(ContinueFromSquareCol__,ContinueFromSquareRow__: Integer; var TopLeftCol__,TopLeftRow__:Integer):Integer;
  {Returns the number of reachable squares, including current player position}

  {Contrary to 'PlayerPath', finding reachable squares doesn't need}
  {to calculate the exact distance from player's current position to a square,
  {hence, the calculation can use a simpler and faster search}

begin {PlayerLegalMoves}
  if   ContinueFromSquareCol__=0 then
       Result:=CalculatePlayersReachableSquares(PlayerPos.X            ,PlayerPos.Y            ,BoardWidth,BoardHeight,Board,False,TopLeftCol__,TopLeftRow__)
  else Result:=CalculatePlayersReachableSquares(ContinueFromSquareCol__,ContinueFromSquareRow__,BoardWidth,BoardHeight,Board,True ,TopLeftCol__,TopLeftRow__);
end; {TSokoGame.PlayerLegalMoves}
(*
function TSokoGame.PlayerPath(FromPos__,ToPos__:TColRow; MovesLinesOptimization__,Backwards__,IsPreferredDirectionSupplied__:Boolean; PreferredDirection__:TDirection; var {out} MoveCount__,PlayerLinesCount__:Integer; var {out} IsPreferredDirectionOK__:Boolean; {in out} Moves__:PPlayerMoves):Boolean;
const INFINITY=MaxInt div 2;
var   Distance,NeighborTimestamp,TimestampIncrement:Cardinal; Direction:TDirection;
      TimeMS:TTimeMS;
      ParentPosition:array[0..MAX_BOARD_WIDTH,0..MAX_BOARD_HEIGHT] of TColRow;
      QueueBottom,QueueTop:^TColRow;
      QueueItems:array[0..MAX_BOARD_SIZE+1] of TColRow; {'+1': so a pointer after last used item is legal}
begin {Calculates the shortest moves/lines player path between 2 squares;}
      {if the player is going to push a box from 'ToPos__' in the next move,}
      {then 'Backwards__' must be 'True' and 'PreferredDirection__' must specify the push direction}
  TimeMS:=GetTimeMS;
  with PlayerPathBoardTimestamps do begin
    FromSquare:=FromPos__;
    TimestampIncrement:=TIMESTAMP_INCREMENT*((MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT)+1)+2*TIMESTAMP_INCREMENT-1;  {'+2*TIMESTAMP_INCREMENT-1': so the low bits of the timestamp can be truncated, and to make absolutely sure that 'TimeStamp-TIMESTAMP_INCREMENT' cannot occur as a leftover from a previous calculation; the low bits can be used for flags stored together with the timestamps}
    if Timestamp<High(Timestamp)-Cardinal(TimestampIncrement) then Inc(Timestamp,TimestampIncrement)
    else begin
       FillChar(Squares,SizeOf(Squares),0); Timestamp:=TimestampIncrement;
       PlayersReachableSquaresTimestampAfterLastPush:=0; {invalidate the saved timestamp; after the wrap-around, the old value could theoretically become active again}
       end;
    Timestamp:=Timestamp and TIMESTAMP_MASK; {mask the low bits; using '+4' increments for marking the reachable squares makes room for the 2 low bit flags, TIMESTAMP_FLAG_VISITED and TIMESTAMP_FLAG_TARGET}

    QueueBottom        :=Addr(QueueItems[Low(QueueItems)]); QueueTop:=QueueBottom;
    MoveCount__        :=INFINITY; {'INFINITY': no path to the target position has been found (yet)}
    PlayerLinesCount__ :=0; {the number of player lines is not calculated by this function}

    if (Board[FromPos__.x,FromPos__.y] and (WALL+BOX))=0 then begin {'True': 'from-position' is either an empty floor or the player's current position, hence, put it on the queue as root position for the breadth-first search}
       Inc(QueueTop); QueueTop^:=FromPos__;
       Squares[FromPos__.x,FromPos__.y]:=Timestamp; {distance from 'from-position' to a square is calculated as value - timestamp, hence, this sets the distance to 'from-position' = 0}
       end;

    {breadth first search (BFS), starting from 'from-position'; a FIFO (first-in-first-out) queue ensures that the reachable squares are visited in breadth-first order}
    while QueueBottom<>QueueTop do begin {while there are more un-expanded squares}
      Inc(QueueBottom); {points to currently processed square}
      with QueueBottom^ do begin
        NeighborTimestamp:=Squares[x,y]+TIMESTAMP_INCREMENT; {timestamp for a neighboring square is a move-optimal path to the neighbor square goes through the current square}
        if (x<>ToPos__.x) or (y<>ToPos__.y) then begin {'True: this isn't the target square}
           for Direction:=High(Direction) downto (Low(Direction)) do begin {try walking in each direction (the reverse order happens to produce better looking paths according to the writer's taste)}
               Inc(QueueTop); {tentatively put the neighbor-square on the queue, no matter if it's a legal move or not}
               QueueTop^.x:=x+DIRECTION_XY[Direction,ColAxis];
               QueueTop^.y:=y+DIRECTION_XY[Direction,RowAxis];
               with QueueTop^ do
                 if   (Squares[x,y] <   Timestamp) and
                      ((Board [x,y] and (WALL+BOX))=0) then begin
                      Squares [x,y] :=NeighborTimestamp;
                      ParentPosition[x,y]:=QueueBottom^; {remember the path to the neighbor-square}
                      end
                 else Dec(QueueTop); {the neighbor-square is occupied or it has already been visited; remove the fresh item from the queue again}
               end;
           end
        else begin {this is the target square}
           MoveCount__:=Integer((NeighborTimestamp-TIMESTAMP_INCREMENT-Timestamp) div TIMESTAMP_INCREMENT); {update the number of pushes to get to the target square}
           QueueBottom:=QueueTop; {stop searching; the breadth-first search ensures that the first found path to the target square is move-optimal}
           end;
        end;
      end;

    Result:=Abs(MoveCount__)<>INFINITY; {'True': found a path from 'from-position' to 'to-position'}

    if Result and (Moves__<>nil) then begin {'<>nil': return the path in 'Moves__'}
       for Distance:=MoveCount__ downto 1 do with ToPos__ do begin
           Moves__^[Distance]:=ToPos__;
           ToPos__:=ParentPosition[x,y];
           end;
       end;
    end;

  Inc(TimeStatistics[0],CalculateElapsedTimeMS(TimeMS,GetTimeMS));
  {TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS); Msg(IntToStr(TimeMS),'Time',MB_OK);}
end; {TSokoGame.PlayerPath}
*)
(*
function TSokoGame.PlayerPath(FromPos__,ToPos__:TColRow;
                              IsAFinalPreferredDirection__,IsPreferredDirectionSupplied__:Boolean;
                              PreferredDirection__:TDirection;
                              var {out} MoveCount__,PlayerLinesCount__:Integer;
                              var {out} IsPreferredDirectionOK__:Boolean;
                              {in  out} Moves__:PPlayerMoves):Boolean;
const BITS_PER_PLAYER_LINE_LENGTH  = 8;                {each player line has this number of  bits to represent its length; precondition: remainder(BITS_PER_CARDINAL / BITS_PER_PLAYER_LINE_LENGTH) = 0}
      INFINITY                     = MAX_BOARD_SIZE+1; {postcondition: 'MAX_BOARD_SIZE' < 'INFINITY'}
type  PQueueItem                   = ^TQueueItem;      {pointer to a queue item}
      TQueueItem                   = record            {a queue item contains a board square}
        Square                     : TColRow;
      end;
      TQueue                       = record            {a first-in-first-out queue}
        Head                       : PQueueItem;       {the first item on the queue}
        Tail                       : PQueueItem;       {the last  item on the queue}
      end;
var   Distance,LineCount,NeighborLineCount:Integer;
      LineLength,NeighborLineLength,LineLengthFactor:Cardinal;
      NeighborTimestamp,SquareTimestamp,TimestampIncrement:SokUtil_.TTimestamp;
      TimeMS                       : TTimeMS;
      Direction,LineDirection,NextLineDirection: TDirection;
      Directions                   : TDirectionSet;
      NeighborSquare,Square        : TColRow;
      LineCounts                   : array[0..MAX_BOARD_WIDTH,0..MAX_BOARD_HEIGHT] of Integer; {number of lines it takes to get to a square; when a forward preferred direction is supplied, the squares in that direction is reachable in 0 lines}
      LineLengths                  : array[0..MAX_BOARD_WIDTH,0..MAX_BOARD_HEIGHT] of Cardinal; {length of the most recent player lines for a moves/lines optimal path to each square}
      ParentDirections             : array[0..MAX_BOARD_WIDTH,0..MAX_BOARD_HEIGHT] of TDirectionSet; {set of directions from parent squares to a square}
      QueueItems                   : array[0..MAX_BOARD_SIZE+1]                    of TQueueItem; {'+1': so a pointer after last used item is legal}
      Queue                        : TQueue;
begin {Calculates a moves/lines optimal path between 2 squares, using the most recent player line lengths as tiebreakers}
      {
      'IsAFinalPreferredDirection__':
          optimize for a forward path or a backward path, i.e., whether a
          preferred direction refers to the first move or the last move
      IsPreferredDirectionSupplied__':
         if the parameter 'PreferredDirection__' is supplied then first moves
         in the preferred direction count as 0-turn moves, otherwise all
         first moves count as 1-turn moves;
      'PreferredDirection__':
          the preferred start/end direction depending on the search
          direction; searching forwards, it's typically the player's current
          position; searching backwards, it's typically the next push direction;
      'PlayerLinesCount__':
          returns the number of player lines; if the 'PreferredDirection__'
          parameter is supplied and the first/last direction matches the
          preferred direction, then the matching line is discounted;
      'IsPreferredDirectionOK__':
          returns 'True' if the first/last direction matches the preferred
          direction;
      the function returns 'True' if a path exists between the 2 squares
      }

  TimeMS:=GetTimeMS;
  with PlayerPathBoardTimestamps do begin
    FromSquare:=FromPos__;
    TimestampIncrement:=TIMESTAMP_INCREMENT*((MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT)+1)+2*TIMESTAMP_INCREMENT-1;  {'+2*TIMESTAMP_INCREMENT-1': so the low bits of the timestamp can be truncated, and to make absolutely sure that 'TimeStamp-TIMESTAMP_INCREMENT' cannot occur as a leftover from a previous calculation; the low bits can be used for flags stored together with the timestamps}
    if Timestamp<High(Timestamp)-TimestampIncrement then {'True': increasing the timestamp doesn't cause an overflow}
       Inc(Timestamp,TimestampIncrement) {increase the timestamp; after the function returns, the distance to all squares reachable from the starting position can calculated as '(Squares[x,y] - Timestamp) div TIMESTAMP_INCREMENT'}
    else begin {increasing the timestamp would cause an overflow; reset the timestamps}
       FillChar(Squares,SizeOf(Squares),0); Timestamp:=TimestampIncrement;
       PlayersReachableSquaresTimestampAfterLastPush:=0; {invalidate the saved timestamp; after the wrap-around, the old value could theoretically become active again}
       end;
    Timestamp:=Timestamp and TIMESTAMP_MASK; {mask the timestamp low bits; the low bits are reserved for flags stored together with the timestamp for each square; this feature is currently not in use}

    MoveCount__             :=INFINITY; {'INFINITY': no path to the target position has been found (yet)}
    PlayerLinesCount__      :=0; {initialize return value}
    IsPreferredDirectionOK__:=False; {initialize return value}

    Queue.Head              :=Addr(QueueItems[Low(QueueItems)]); Queue.Tail:=Queue.Head; {initialize the queue}
    if (Board[FromPos__.x,FromPos__.y] and (WALL+BOX))=0 then begin {'True': 'from-position' is either an empty floor or the player's current position, hence, it's a valid root position for the breadth-first search}
       Inc(Queue.Tail); Queue.Tail^.Square:=FromPos__; {put the 'from-position' on the queue}
       Squares[FromPos__.x,FromPos__.y]:=Timestamp; {distance from 'from-position' to a square is calculated as (value - timestamp) div TIMESTAMP_INCREMENT, hence, this sets the distance to 'from-position' = 0}
       LineCounts[FromPos__.x,FromPos__.y]:=0;  {the number of lines (almost the same as turns) it takes to get to the square via a moves/lines optimal path}
       LineLengths[FromPos__.x,FromPos__.y]:=0;
       if   IsAFinalPreferredDirection__ or (not IsPreferredDirectionSupplied__) then
            ParentDirections[FromPos__.x,FromPos__.y]:=[] {the set of directions from parent squares to the starting position is an empty set}
       else ParentDirections[FromPos__.x,FromPos__.y]:=[PreferredDirection__]; {moving the player in the specified direction from the starting position is a continuation of an existing player line; it doesn't count as a new player line}
       end;

    {find a moves/lines optimal path using a breadth first search (BFS), starting from 'from-position'; a FIFO (first-in-first-out) queue ensures that the reachable squares are visited in breadth-first order}
    while Queue.Head<>Queue.Tail do begin {while there are more un-expanded squares}
      Inc(Queue.Head); {dequeue the next square from the front of the queue}
      with Queue.Head^.Square do begin
        NeighborTimestamp:=Squares[x,y]+TIMESTAMP_INCREMENT; {timestamp for a neighboring square if a move-optimal path to the neighbor square goes through the square currently being expanded}
        Directions:=ParentDirections[x,y]; {directions from parent squares to the square currently being expanded}
        LineCount:=LineCounts[x,y]; {the number of lines to get to this square via a moves/lines optimal path}
        LineLength:=LineLengths[x,y]; {length of the most recent player lines on a moves/lines optimal path to this square}
        if (x<>ToPos__.x) or (y<>ToPos__.y) then begin {'True': this isn't the target square; the breadth-first search ensures that all paths to the target square have been explored at the time the target square is selected for expansion}

           for Direction:=High(Direction) downto Low(Direction) do begin {try walking in each direction (the reverse order happens to produce better looking paths according to the writer's taste)}
               Inc(Queue.Tail); {tentatively put the neighbor square on the queue, no matter if it's a legal move or not}
               Queue.Tail^.Square.x:=x+DIRECTION_XY[Direction,ColAxis];
               Queue.Tail^.Square.y:=y+DIRECTION_XY[Direction,RowAxis];
               with Queue.Tail^.Square do
                 if   (Board                           [x,y] and (WALL+BOX))=0 then begin {'True': the neighbor square doesn't contain a wall or a box}
                      SquareTimestamp                        :=  Squares[x,y]; {the timestamp for the neighbor square}
                      if SquareTimestamp                     <   Timestamp then begin {'True': this is the first time this neighbor square is visited}
                         Squares                       [x,y] :=  NeighborTimestamp; {update the number of moves it takes to get to this neighbor square}
                         ParentDirections              [x,y] :=  [Direction]; {the current direction is at the moment the only member of the set of directions leading to this neighbor square via a moves/lines optimal path}
                         if Direction                        in  Directions then begin {'True': the move is a continuation of an existing player player line}
                            LineCounts                 [x,y] :=  LineCount;
                            LineLengths                [x,y] :=  Succ(LineLength);
                            end
                         else begin
                            LineCounts                 [x,y] :=  Succ(LineCount); {'Succ': it takes a turn to get to this neighbor square from the current parent square}
                            LineLengths                [x,y] :=  Succ(LineLength shl BITS_PER_PLAYER_LINE_LENGTH); {'shl': each player line has 'BITS_PER_PLAYER_LINE_LENGTH' bits to represent its length}
                            end;
                         end
                      else begin
                         if   SquareTimestamp                =  NeighborTimestamp then begin {'True': this is not the first time this neighbor square is visited; this is a visit from another direction than the first one, but with the same number of moves}
                              if   Direction                 in Directions then begin {'True': the move is a continuation of an existing player player line}
                                   NeighborLineCount         := LineCount;
                                   NeighborLineLength        := Succ(LineLength);
                                   end
                              else begin
                                     NeighborLineCount       := Succ(LineCount); {'Succ': it takes a turn to get to this neighbor square from the current parent square}
                                     NeighborLineLength      := Succ(LineLength shl BITS_PER_PLAYER_LINE_LENGTH); {'shl': each player line has 'BITS_PER_PLAYER_LINE_LENGTH' bits to represent its length}
                                   end;
                              if   NeighborLineCount         <  LineCounts[x,y] then begin {'True': this is a new best moves/lines path to this neighbor square}
                                   LineCounts          [x,y] := NeighborLineCount; {update the number of lines it takes to get to this neighbor square via a moves/lines optimal path}
                                   ParentDirections    [x,y] := [Direction]; {the current direction is at the moment the only member of the set of directions leading to this neighbor square via a moves/lines optimal path}
                                   LineLengths         [x,y] := NeighborLineLength;
                                   end
                              else if   NeighborLineCount    =  LineCounts[x,y] then begin {'True': this is another moves/lines optimal path to this neighbor square}
                                        Include(ParentDirections[x,y],Direction); {update the set of directions from parent squares to this neighbor square via a moves optimal path}
                                        if LineLengths [x,y] <  NeighborLineLength then {'True': the new path to the neighbor square has a better line length score}
                                           LineLengths [x,y] := NeighborLineLength; {save the best line length score; it's used as a tiebreaker between equal moves/lines paths to a square; note that it can only keep the most recent player lines: 'BITS_PER_CARDINAL / BITS_PER_PLAYER_LINE_LENGTH'}
                                        end;
                              end;
                         Dec(Queue.Tail); {the neighbor square in this direction has already been visited; remove it from the queue again}
                         end;
                      end
                 else Dec(Queue.Tail); {the neighbor square in this direction is occupied by a wall or a box; remove it from the queue again}
               end;
           end
        else begin {this is the target square}
           MoveCount__:=Integer((NeighborTimestamp-TIMESTAMP_INCREMENT-Timestamp) div TIMESTAMP_INCREMENT); {update the number of pushes to get to the target square}
           Queue.Head:=Queue.Tail; {exit the 'while' loop, i.e., stop the search; the breadth-first search ensures that all paths to the target square have been explored at the time the target square is selected for expansion}
           end;
        end;
      end;

    Result:=Abs(MoveCount__)<>INFINITY; {'True': a path from 'from-position' to 'to-position' has been found}

    if Result then begin
       PlayerLinesCount__:=LineCounts[ToPos__.x,ToPos__.y]; {return the number of player lines}
       if   IsAFinalPreferredDirection__ and (PlayerLinesCount__>0) and
            IsPreferredDirectionSupplied__ and (PreferredDirection__ in ParentDirections[ToPos__.x,ToPos__.y]) then
            Dec(PlayerLinesCount__); {discount the last player line; it matches the preferred direction, typically the next box push direction}
       if   MoveCount__=0 then IsPreferredDirectionOK__:=True; {the preferred direction is OK if it's an empty path, i.e., when the player already is located at the target square}

       if   (Moves__<>nil) or                                                           {'True': construct the path because the caller requested it}
            IsPreferredDirectionSupplied__ then begin                                   {'True': construct the path in order to check whether the specified preferred direction is ok}

            {construct the path backwards from the target square to the starting position, using the parent square directions}
            LineDirection:=PreferredDirection__; NextLineDirection:=LineDirection;      {if it's a backward preferred direction then first check if there is a moves/lines optimal path ending with the specified direction}
            for  Distance:=MoveCount__ downto 1 do with ToPos__ do begin                {'with ToPos__': makes unqualified references to 'x' and 'y' in the following code block refer to 'ToPos__.x' and 'ToPos__.y' respectively}
                 if Moves__<>nil then Moves__^[Distance]:=ToPos__;                      {store the player position before backtracking}

                 if (not (LineDirection in ParentDirections[x,y])) or                   {'True': the player changes direction}
                    (Distance=MoveCount__) then                                         {'True': this is the target position; find the first direction on the backward path}
                    if (LineDirection in ParentDirections[x,y]) and                     {check whether the preferred final direction, if any, is valid}
                       IsAFinalPreferredDirection__ and
                       IsPreferredDirectionSupplied__ then begin
                       NextLineDirection:=LineDirection;                                {the preferred final direction is all right; use it}
                       end
                    else begin
                       LineLength:=0; LineLengthFactor:=0;                              {initialize the search for the parent square with the highest line length, i.e., the one with the longest early lines}
                       for Direction:=Low(Direction) to High(Direction) do begin        {find the direction to a parent square on a best path to the current square}
                           Square.x:=x-DIRECTION_XY[Direction,ColAxis];
                           Square.y:=y-DIRECTION_XY[Direction,RowAxis];

                           if      Direction in ParentDirections[x,y] then begin        {'True': there is a parent square in this direction which belongs to an optimal moves/lines path}
                                   NeighborLineLength:=LineLengths[Square.x,Square.y];
                                   if not (Direction in ParentDirections[Square.x,Square.y]) then {'True': it requires a turn (on a moves/lines optimal path) to get from this neighbor square to the currently investigated square on the path}
                                      NeighborLineLength:=NeighborLineLength shl BITS_PER_PLAYER_LINE_LENGTH; {'shl': for comparing line lengths, the extra turn to get from the neighbor square to the current square must be taken into account}
                                   NeighborLineLength:=NeighborLineLength shl LineLengthFactor; {if 'LineLengthFactor' is non-zero, then the tiebreak comparison has changed from an 'n-1' lines comparison to an 'n' lines comparison because the current direction (i.e., continue without making a turn) at some point had the best tiebreaking score}
                                   if LineLength<=NeighborLineLength then begin         {'<=': not '<' because the starting position has line length = '0', and 'LineLength' is an unsigned integer which also was initialized to '0'}
                                      LineLength:=NeighborLineLength;
                                      NextLineDirection:=Direction;
                                      end;
                                   end
                           else if (Direction=LineDirection) and (Distance<>MoveCount__) then begin {'True': check if it possible to continue in the current direction instead of making a turn}
                                   NeighborTimestamp:=Squares[x,y]-TIMESTAMP_INCREMENT; {timestamp for a neighboring square if there is a move-optimal to the current square via the neighbor square; note that 'TimeStamp' must increase for each call of 'PlayerPath' so 'TimeStamp-TIMESTAMP_INCREMENT' doesn't occur as a leftover from a previous calculation}
                                   NeighborLineLength:=0;                               {calculate the line length to the square which is the neighbor of the current square on the path}
                                   while (Squares[Square.x,Square.y]=NeighborTimeStamp) {while the next square in this direction is on a move-optimal path to the current position ...}
                                         and
                                         (not (LineDirection in ParentDirections[Square.x,Square.y])) do begin {... and while it requires a turn to reach the next square in this direction}
                                         Dec(Square.x,DIRECTION_XY[Direction,ColAxis]); {backtrack to the next square in this direction}
                                         Dec(Square.y,DIRECTION_XY[Direction,RowAxis]);
                                         Dec(NeighborTimeStamp,TIMESTAMP_INCREMENT);    {timestamp for a neighboring square if there is a move-optimal to the current square via the neighbor square}
                                         Inc(NeighborLineLength);                       {calculate the line length to the square which is the neighbor of the current square on the path}
                                         end;
                                   if    (Squares[Square.x,Square.y]=NeighborTimeStamp) {'True': there is a move-optimal path via 'Square' which doesn't require a turn now, but if may not be moves/lines optimal}
                                         and
                                         (Pred(LineCounts[Square.x,Square.y])=LineCounts[x,y]) then begin {'True': there is a moves/lines optimal path which doesn't require a turn now; ('Pred': it saves one turn not making a turn now)}
                                         Inc(NeighborLineLength,LineLengths[Square.x,Square.y]); {calculate the line length to the square which is the neighbor of the current square on the path}
                                         if LineLength shl BITS_PER_PLAYER_LINE_LENGTH  {'shl': the best line length from the other directions stems from an 'n-1' lines path, while the line length here, without making a turn now, refers to an 'n' lines path (on the resulting forward path)}
                                            <=NeighborLineLength then begin             {'<=': not '<' because the starting position has line length = '0', and 'LineLength' is an unsigned integer which also has been initialized to '0'}
                                            LineLength:=NeighborLineLength;
                                            NextLineDirection:=Direction;
                                            LineLengthFactor:=BITS_PER_PLAYER_LINE_LENGTH; {neighbor squares in the remaining directions, if any, must have their line lengths multiplied for a correct comparison because 'LineLength' now refers to an 'n' lines path (in the forward direction) whereas the stored line lengths for the neighbor squares refer to paths of length 'n-1' lines}
                                            end;
                                         end;
                                   end;
                           end;
                       end;

                 LineDirection:=NextLineDirection;                                      {update the current direction}

                 if (     IsAFinalPreferredDirection__  and (Distance=MoveCount__)) or
                    ((not IsAFinalPreferredDirection__) and (Distance=1          )) then
                    IsPreferredDirectionOK__:=LineDirection=PreferredDirection__;       {return whether the best path uses the preferred direction}

                 Dec(x,DIRECTION_XY[LineDirection,ColAxis]);                            {backtrack to the parent position given by the current direction}
                 Dec(y,DIRECTION_XY[LineDirection,RowAxis]);                            {'ToPos__' now contains the previous square on the path}
                 end;
            end;
       end;
    end;

  Inc(TimeStatistics[0],CalculateElapsedTimeMS(TimeMS,GetTimeMS));
  {TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS); Msg(IntToStr(TimeMS),'Time',MB_OK);}
end; {TSokoGame.PlayerPath}
*)
function TSokoGame.PlayerPath(FromPos__,ToPos__:TColRow;
                              IsAFinalPreferredDirection__,IsPreferredDirectionSupplied__:Boolean;
                              PreferredDirection__:TDirection;
                              var {out} MoveCount__,PlayerLinesCount__:Integer;
                              var {out} IsPreferredDirectionOK__:Boolean;
                              {in  out} Moves__:PPlayerMoves):Boolean;
const BITS_PER_PLAYER_LINE_LENGTH  = 8;                {each player line has this number of  bits to represent its length; precondition: remainder(BITS_PER_CARDINAL div BITS_PER_PLAYER_LINE_LENGTH) = 0}
      INFINITY                     = MAX_BOARD_SIZE+1; {postcondition: 'MAX_BOARD_SIZE' < 'INFINITY'}
type  PQueueItem                   = ^TQueueItem;      {pointer to a queue item}
      TQueueItem                   = record            {a queue item contains a board square}
        Square                     : TColRow;
      end;
      TQueue                       = record            {a first-in-first-out queue}
        Head                       : PQueueItem;       {the first item on the queue}
        Tail                       : PQueueItem;       {the last  item on the queue}
      end;
var   a,b,Distance,LineCount,NeighborLineCount: Integer;
      LineLength,NeighborLineLength: Cardinal;
      NeighborTimestamp,SquareTimestamp,TimestampIncrement: SokUtil_.TTimestamp;
      TimeMS                       : TTimeMS;
      Direction,LineDirection,NextLineDirection: TDirection;
      Directions                   : TDirectionSet;
      NeighborSquare,Square        : TColRow;
      LineCounts                   : array[0..MAX_BOARD_WIDTH,0..MAX_BOARD_HEIGHT] of Integer; {number of lines it takes to get to a square; when a forward preferred direction is supplied, the squares in that direction are reachable in 0 lines}
      LineLengths                  : array[0..MAX_BOARD_WIDTH,0..MAX_BOARD_HEIGHT] of Cardinal; {length of the most recent player lines for a moves/lines optimal path to each square}
      ParentDirections             : array[0..MAX_BOARD_WIDTH,0..MAX_BOARD_HEIGHT] of TDirectionSet; {set of directions from parent squares to a square}
      QueueItems                   : array[0..MAX_BOARD_SIZE+1]                    of TQueueItem; {'+1': so a pointer after last used item is legal}
      Queue                        : TQueue;
begin {Calculates a moves/lines optimal path between 2 squares, using the most recent player line lengths for tiebreaking}
      {
      'IsAFinalPreferredDirection__':
          optimize for a forward path or a backward path, i.e., whether a
          preferred direction refers to the first move or the last move
      IsPreferredDirectionSupplied__':
         if the parameter 'PreferredDirection__' is supplied then first moves
         in the preferred direction count as 0-turn moves, otherwise all
         first moves count as 1-turn moves;
      'PreferredDirection__':
          the preferred start/end direction depending on the search
          direction;
          searching forwards, it's typically the player's current direction;
          searching backwards, it's typically the next push direction;
      'PlayerLinesCount__':
          returns the number of player lines; if the 'PreferredDirection__'
          parameter is supplied and the first/last direction matches the
          preferred direction, then the matching line is discounted;
      'IsPreferredDirectionOK__':
          returns 'True' if the first/last direction matches the preferred
          direction;
      the function returns 'True' if a path exists between the 2 squares
      }

  TimeMS:=GetTimeMS;
  with PlayerPathBoardTimestamps do begin
    FromSquare:=FromPos__;
    TimestampIncrement:=TIMESTAMP_INCREMENT*((MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT)+1)+2*TIMESTAMP_INCREMENT-1;  {'+2*TIMESTAMP_INCREMENT-1': so the low bits of the timestamp can be truncated, and to make absolutely sure that 'TimeStamp-TIMESTAMP_INCREMENT' cannot occur as a leftover from a previous calculation; the low bits can be used for flags stored together with the timestamps}
    if Timestamp<High(Timestamp)-TimestampIncrement then {'True': increasing the timestamp doesn't cause an overflow}
       Inc(Timestamp,TimestampIncrement) {increase the timestamp; after the function returns, the distance to all squares reachable from the starting position can calculated as '(Squares[x,y] - Timestamp) div TIMESTAMP_INCREMENT'}
    else begin {increasing the timestamp would cause an overflow; reset the timestamps}
       FillChar(Squares,SizeOf(Squares),0); Timestamp:=TimestampIncrement;
       PlayersReachableSquaresTimestampAfterLastPush:=0; {invalidate the saved timestamp; after the wrap-around, the old value could theoretically become active again}
       end;
    Timestamp:=Timestamp and TIMESTAMP_MASK; {mask the timestamp low bits; the low bits are reserved for flags stored together with the timestamp for each square; this feature is currently not in use}

    MoveCount__             :=INFINITY; {'INFINITY': no path to the target position has been found (yet)}
    PlayerLinesCount__      :=0; {initialize return value}
    IsPreferredDirectionOK__:=False; {initialize return value}

    Queue.Head              :=Addr(QueueItems[Low(QueueItems)]); Queue.Tail:=Queue.Head; {initialize the queue}
    if (Board[FromPos__.x,FromPos__.y] and (WALL+BOX))=0 then begin {'True': 'from-position' is either an empty floor or the player's current position, hence, it's a valid root position for the breadth-first search}
       Inc(Queue.Tail); Queue.Tail^.Square:=FromPos__; {put the 'from-position' on the queue}
       Squares[FromPos__.x,FromPos__.y]:=Timestamp; {distance from 'from-position' to a square is calculated as (value - timestamp) div TIMESTAMP_INCREMENT, hence, this sets the distance to 'from-position' = 0}
       LineCounts[FromPos__.x,FromPos__.y]:=0;  {the number of lines (almost the same as turns) it takes to get to the square via a moves/lines optimal path}
       LineLengths[FromPos__.x,FromPos__.y]:=0;
       if   IsAFinalPreferredDirection__ or (not IsPreferredDirectionSupplied__) then
            ParentDirections[FromPos__.x,FromPos__.y]:=[] {the set of directions from parent squares to the starting position is an empty set}
       else ParentDirections[FromPos__.x,FromPos__.y]:=[PreferredDirection__]; {moving the player in the specified direction from the starting position is a continuation of an existing player line; it doesn't count as a new player line}
       end;

    {find a moves/lines optimal path using a breadth first search (BFS), starting from 'from-position'; a FIFO (first-in-first-out) queue ensures that the reachable squares are visited in breadth-first order}
    while Queue.Head<>Queue.Tail do begin {while there are more un-expanded squares}
      Inc(Queue.Head); {dequeue the next square from the front of the queue}
      with Queue.Head^.Square do begin
        NeighborTimestamp:=Squares[x,y]+TIMESTAMP_INCREMENT; {timestamp for a neighboring square if a move-optimal path to the neighbor square goes through the square currently being expanded}
        Directions:=ParentDirections[x,y]; {directions from parent squares to the square currently being expanded}
        LineCount:=LineCounts[x,y]; {the number of lines to get to this square via a moves/lines optimal path}
        LineLength:=LineLengths[x,y]; {length of the most recent player lines on a moves/lines optimal path to this square}
        if (x<>ToPos__.x) or (y<>ToPos__.y) then begin {'True': this isn't the target square; the breadth-first search ensures that all paths to the target square have been explored at the time the target square is selected for expansion}

           for Direction:=High(Direction) downto Low(Direction) do begin {try walking in each direction (the reverse order happens to produce better looking paths according to the writer's taste)}
               Inc(Queue.Tail); {tentatively put the neighbor square on the queue, no matter if it's a legal move or not}
               Queue.Tail^.Square.x:=x+DIRECTION_XY[Direction,ColAxis];
               Queue.Tail^.Square.y:=y+DIRECTION_XY[Direction,RowAxis];
               with Queue.Tail^.Square do
                 if   (Board                           [x,y] and (WALL+BOX))=0 then begin {'True': the neighbor square doesn't contain a wall or a box}
                      SquareTimestamp                        :=  Squares[x,y]; {the timestamp for the neighbor square}
                      if SquareTimestamp                     <   Timestamp then begin {'True': this is the first time this neighbor square is visited}
                         Squares                       [x,y] :=  NeighborTimestamp; {update the number of moves it takes to get to this neighbor square}
                         ParentDirections              [x,y] :=  [Direction]; {the current direction is at the moment the only member of the set of directions leading to this neighbor square via a moves/lines optimal path}
                         if Direction                        in  Directions then begin {'True': the move is a continuation of an existing player line}
                            LineCounts                 [x,y] :=  LineCount;
                            LineLengths                [x,y] :=  Succ(LineLength);
                            end
                         else begin
                            LineCounts                 [x,y] :=  Succ(LineCount); {'Succ': it takes a turn to get to this neighbor square from the current parent square}
                            LineLengths                [x,y] :=  Succ(LineLength shl BITS_PER_PLAYER_LINE_LENGTH); {'shl': each player line has 'BITS_PER_PLAYER_LINE_LENGTH' bits to represent its length}
                            end;
                         end
                      else begin
                         if   SquareTimestamp                =  NeighborTimestamp then begin {'True': this is not the first time this neighbor square is visited; this is a visit from another direction than the first one, but with the same number of moves}
                              if   Direction                 in Directions then begin {'True': the move is a continuation of an existing player line}
                                   NeighborLineCount         := LineCount;
                                   NeighborLineLength        := Succ(LineLength);
                                   end
                              else begin
                                     NeighborLineCount       := Succ(LineCount); {'Succ': it takes a turn to get to this neighbor square from the current parent square}
                                     NeighborLineLength      := Succ(LineLength shl BITS_PER_PLAYER_LINE_LENGTH); {'shl': each player line has 'BITS_PER_PLAYER_LINE_LENGTH' bits to represent its length}
                                   end;
                              if   NeighborLineCount         <  LineCounts[x,y] then begin {'True': this is a new best moves/lines path to this neighbor square}
                                   LineCounts          [x,y] := NeighborLineCount; {update the number of lines it takes to get to this neighbor square via a moves/lines optimal path}
                                   ParentDirections    [x,y] := [Direction]; {the current direction is at the moment the only member of the set of directions leading to this neighbor square via a moves/lines optimal path}
                                   LineLengths         [x,y] := NeighborLineLength;
                                   end
                              else if   NeighborLineCount    =  LineCounts[x,y] then begin {'True': this is another moves/lines optimal path to this neighbor square}
                                        Include(ParentDirections[x,y],Direction); {update the set of directions from parent squares to this neighbor square via a moves optimal path}
                                        if LineLengths [x,y] <  NeighborLineLength then {'True': the new path to the neighbor square has a better line length score}
                                           LineLengths [x,y] := NeighborLineLength; {save the best line length score; it's used as a tiebreaker between equal moves/lines paths to a square; note that it can only keep the most recent player lines: 'BITS_PER_CARDINAL / BITS_PER_PLAYER_LINE_LENGTH'}
                                        end;
                              end;
                         Dec(Queue.Tail); {the neighbor square in this direction has already been visited; remove it from the queue again}
                         end;
                      end
                 else Dec(Queue.Tail); {the neighbor square in this direction is occupied by a wall or a box; remove it from the queue again}
               end;
           end
        else begin {this is the target square}
           MoveCount__:=Integer((NeighborTimestamp-TIMESTAMP_INCREMENT-Timestamp) div TIMESTAMP_INCREMENT); {update the number of pushes it takes to get to the target square}
           Queue.Head:=Queue.Tail; {exit the 'while' loop, i.e., stop the search; the breadth-first search ensures that all paths to the target square have been explored at the time the target square is selected for expansion}
           end;
        end;
      end;

    Result:=Abs(MoveCount__)<>INFINITY; {'True': a path from 'from-position' to 'to-position' has been found}

    if Result then begin
       PlayerLinesCount__:=LineCounts[ToPos__.x,ToPos__.y];                              {return the number of player lines}
       if   IsPreferredDirectionSupplied__ and IsAFinalPreferredDirection__  and
            (PlayerLinesCount__>0) and
            (PreferredDirection__ in ParentDirections[ToPos__.x,ToPos__.y]) then begin
            Dec(PlayerLinesCount__);                                                     {discount the last player line; it matches the preferred direction, typically the next box push direction}
            IsPreferredDirectionOK__:=True;                                              {the preferred final direction is ok}
            end;
       if   MoveCount__=0 then IsPreferredDirectionOK__:=True;                           {the preferred direction is ok if it's an empty path, i.e., when the player already is located at the target square}

       if   (Moves__<>nil) or                                                            {'True': construct the path because the caller requested it}
            (IsPreferredDirectionSupplied__ and (not IsAFinalPreferredDirection__)) then begin {'True': construct the path in order to check whether the specified preferred first direction is ok}

            {construct the path backwards from the target square to the starting position, using the parent square directions}
            LineDirection:=PreferredDirection__;                                         {if it's a backward preferred direction then first check if there is a moves/lines optimal path ending with the specified direction}
            for  Distance:=MoveCount__ downto 1 do with ToPos__ do begin                 {'with ToPos__': makes unqualified references to 'x' and 'y' in the following code block refer to 'ToPos__.x' and 'ToPos__.y' respectively}
                 if Moves__<>nil then Moves__^[Distance]:=ToPos__;                       {store the player position before backtracking}
                 NextLineDirection:=LineDirection;                                       {initialize the direction of this move; continue backtracking in the current direction if there is a moves/lines optimal path going through the neighbor square in this direction}

                 if (not (LineDirection in ParentDirections[x,y])) or                    {'True': the player changes direction}
                    (Distance=MoveCount__) then                                          {'True': this is the target position; find the first direction on the backward path}
                    if (LineDirection in ParentDirections[x,y]) and                      {check whether the preferred final direction, if any, is valid}
                       IsAFinalPreferredDirection__ and
                       IsPreferredDirectionSupplied__ then begin
                       {the preferred final direction is all right; use it}
                       end
                    else begin
                       LineLength:=0;                                                    {initialize the search for the parent square with the highest line length, i.e., the one with the longest preceding lines}
                       for Direction:=Low(Direction) to High(Direction) do               {find the direction to a parent square on a best path to the current square}
                           if Direction in ParentDirections[x,y] then begin              {'True': there is a parent square in this direction which belongs to an optimal moves/lines path to the current square}
                              Square.x:=x-DIRECTION_XY[Direction,ColAxis];
                              Square.y:=y-DIRECTION_XY[Direction,RowAxis];
                              NeighborLineLength:=LineLengths[Square.x,Square.y];
                              if not (Direction in ParentDirections[Square.x,Square.y]) then {'True': it requires a turn (on a moves/lines optimal path) to get from this neighbor square to the currently investigated square on the path}
                                 NeighborLineLength:=NeighborLineLength shl BITS_PER_PLAYER_LINE_LENGTH; {'shl': for comparing line lengths, the extra turn to get from the neighbor square to the current square must be taken into account}
                              if LineLength<=NeighborLineLength then begin               {'<=': not '<' because the starting position has line length = '0', and 'LineLength' is an unsigned integer which also was initialized to '0'}
                                 LineLength:=NeighborLineLength;
                                 NextLineDirection:=Direction;
                                 end;
                              end;
                       if  (not (LineDirection in ParentDirections[x,y])) and (Distance<>MoveCount__) then begin {'True': check if it possible to continue in the current direction instead of making a turn now, even though the current direction isn't a member of the optimal parent directions for the current square}
                           Square.x:=x-DIRECTION_XY[LineDirection,ColAxis];
                           Square.y:=y-DIRECTION_XY[LineDirection,RowAxis];
                           NeighborTimestamp:=Squares[x,y]-TIMESTAMP_INCREMENT;          {timestamp for a neighboring square if there is a move-optimal path to the current square via the neighbor square; note that 'TimeStamp' must increase for each call of 'PlayerPath' so 'TimeStamp-TIMESTAMP_INCREMENT' doesn't occur as a leftover from a previous calculation}
                           NeighborLineLength:=0;                                        {calculate the line length to the square next to the current square, moving on the line in the current direction, i.e., 'LineDirection'}
                           while (Squares[Square.x,Square.y]=NeighborTimeStamp)          {while the next square in this direction is on a move-optimal path to the current position ...}
                                 and
                                 (not (LineDirection in ParentDirections[Square.x,Square.y])) do begin {... and while it requires a turn to reach the next square in this direction}
                                 Dec(Square.x,DIRECTION_XY[LineDirection,ColAxis]);      {backtrack to the next square in this direction}
                                 Dec(Square.y,DIRECTION_XY[LineDirection,RowAxis]);
                                 Dec(NeighborTimeStamp,TIMESTAMP_INCREMENT);             {timestamp for a neighboring square if there is a move-optimal path to the current square via the neighbor square}
                                 Inc(NeighborLineLength);                                {calculate the line length to the square which is the neighbor of the current square on the path being produced}
                                 end;
                           if    (Squares[Square.x,Square.y]=NeighborTimeStamp)          {'True': there is a move-optimal path via 'Square' which doesn't require a turn now, but it might not be moves/lines optimal}
                                 and
                                 (Pred(LineCounts[Square.x,Square.y])=LineCounts[x,y]) then begin {'True': there is a moves/lines optimal path which doesn't require a turn now; ('Pred': it saves one turn not making a turn now)}
                                 Inc(NeighborLineLength,LineLengths[Square.x,Square.y]); {calculate the line length to the square which is the neighbor of the current square on the path}
                                 if LineLength shl BITS_PER_PLAYER_LINE_LENGTH           {'shl': the best line length from the other directions stems from an 'n' lines path, while the line length here, without making a turn now, refers to an 'n+1' lines path (on the resulting forward path)}
                                    <=NeighborLineLength then begin                      {'<=': not '<' because the starting position has line length = '0', and 'LineLength' is an unsigned integer which also has been initialized to '0'}
                                    NextLineDirection:=LineDirection;
                                    end;
                                 end;
                           end;
                       end;

                 LineDirection:=NextLineDirection;                                       {update the current direction}

                 if (     IsAFinalPreferredDirection__  and (Distance=MoveCount__)) or
                    ((not IsAFinalPreferredDirection__) and (Distance=1          )) then
                    IsPreferredDirectionOK__:=LineDirection=PreferredDirection__;        {return whether the best path uses the preferred direction}

                 Dec(x,DIRECTION_XY[LineDirection,ColAxis]);                             {backtrack to the parent position given by the current direction}
                 Dec(y,DIRECTION_XY[LineDirection,RowAxis]);                             {'ToPos__' now contains the previous square on the path}
                 end;
            end;
       end;
    end;

  Inc(TimeStatistics[0],CalculateElapsedTimeMS(TimeMS,GetTimeMS));
  {TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS); Msg(IntToStr(TimeMS),'Time',MB_OK);}
end; {TSokoGame.PlayerPath}

function  TSokoGame.PlayerPathLength(const FromPos__,ToPos__:TColRow; var MoveCount__:Integer):Boolean;
var i,j,dx,dy,PlayerLinesCount:Integer; IsPreferredDirectionOK:Boolean; Center:TColRow;
begin {PlayerPathLength; Returns 'True' if there is a path for the player from 'FromPos__' to 'ToPos__' in which case 'MoveCount__' returns the length of the shortest path; precondition: 'FromPos__' is an empty floor square}
  Result:=((Board[ToPos__.x,ToPos__.y] and (WALL+BOX))=0); {target free?}
  if Result then begin                          {ok: try to find a path}
     dx:=ToPos__.x-FromPos__.x; dy:=ToPos__.y-FromPos__.y;
     MoveCount__:=Abs(dx)+Abs(dy);

     if MoveCount__>1 then                      {0=identical squares, 1=neighbor squares}
        if MoveCount__=2 then                   {a 2-squares move}
           if Abs(dy)=1 then                    {a corner move}
              if   ((Board[FromPos__.x,ToPos__  .y] and (WALL+BOX))=0) or
                   ((Board[ToPos__  .x,FromPos__.y] and (WALL+BOX))=0) then
                   {ok: a corner is free}
              else {the corners are blocked: try harder:}
                   Result:=PlayerPath(FromPos__,ToPos__,False,False,Low(TDirection),MoveCount__,PlayerLinesCount,IsPreferredDirectionOK,nil)
           else begin
              Center.x:=FromPos__.x+(dx div 2); {the square between 'FromPos__' and 'ToPos__'}
              Center.y:=FromPos__.y+(dy div 2); i:=-1;
              if (Board[Center.x,Center.y] and (WALL+BOX))=0 then
                 {ok: the square between 'FromPos__' and 'ToPos__' is free}
              else begin                        {check if there is a simple path around the center-square}
                 if   dy=0 then                 {horizontal move: check if a neighbor row is free}
                      repeat Result:=True;
                             for j:=-1 to 1 do  {check if squares in a neighbor row is free}
                                 if (Board[Center.x+j,Center.y+i] and (WALL+BOX))<>0 then begin
                                    Result:=False; break;
                                    end;
                             Inc(i,2);
                      until  Result or (i>1)    {until ok or until both neighbor-rows have been tested}
                 else repeat Result:=True;      {vertical move: check if a neighbor column is free}
                             for j:=-1 to 1 do  {check if squares in a neighbor column is free}
                                 if (Board[Center.x+i,Center.y+j] and (WALL+BOX))<>0 then begin
                                    Result:=False; break;
                                    end;
                             Inc(i,2);
                      until  Result or (i>1);   {until ok or until both neighbor-columns have been tested}
                 if   Result then
                      MoveCount__:=4            {ok: it takes 4 moves to walk around the center square}
                 else {the squares in the neighbor-columns/rows are blocked; try harder:}
                      Result:=PlayerPath(FromPos__,ToPos__,False,False,Low(TDirection),MoveCount__,PlayerLinesCount,IsPreferredDirectionOK,nil);
                 end;
              end
        else Result:=PlayerPath(FromPos__,ToPos__,False,False,Low(TDirection),MoveCount__,PlayerLinesCount,IsPreferredDirectionOK,nil); {longer path: try harder}
     end;
end; {PlayerPathLength}

function  TSokoGame.Redo(RedoCombinedMoves__:Boolean):Boolean;
begin
  Result:=Redo0(RedoCombinedMoves__);
end; {TSokoGame.Redo}

function  TSokoGame.Redo0(RedoCombinedMoves__:Boolean):Boolean;
var OldCount,Separator,Move,dx,dy:Integer;
    IsAFreezingMove,OldJumpsAllowedAfterFirstBoxMoveInReverseMode:Boolean;
begin
  OldJumpsAllowedAfterFirstBoxMoveInReverseMode:=JumpsAllowedAfterFirstBoxMoveInReverseMode;

  Result:=History.Count<History.Top;
  if Result then with History do
     try
       JumpsAllowedAfterFirstBoxMoveInReverseMode:=True; {allow jumps; maybe they were allowed at the time the move originally was performed}

       OldCount         :=Count;
       Move             :=Moves[Succ(Count)];
       Separator        :=Move and (H_MASK_MOVE_SEPARATOR or H_FLAG_JUMP);

{     if (Move and H_FLAG_JUMP)<>0 then RedoCombinedMoves__:=True;} { no! the caller may redo each move individually for proper screen-updating}

       repeat  HistoryMoveToDxDy(Move,dx,dy);
               Result   :=IsALegalMove(dx,dy,Move,IsAFreezingMove);
               if Result then DoMoveUpdateGame(dx,dy,OldCount,Move);
               Move     :=Moves[Succ(Count)];
       until  (not Result) or
              (Count=Top)  or
              (Separator<>(Move and (H_MASK_MOVE_SEPARATOR or H_FLAG_JUMP))) or
              (not RedoCombinedMoves__);

       Result:=Count>OldCount;
     finally
       JumpsAllowedAfterFirstBoxMoveInReverseMode:=OldJumpsAllowedAfterFirstBoxMoveInReverseMode;
     end;
end; {TSokoGame.Redo0}

function TSokoGame.RemoveNonOptimalText(const Text__:String):String;
var i:Integer; s:String;
begin
  Result:=Text__; s:=TEXT_NON_OPTIMAL; i:=AnsiPos(s,Result);
  while i>=STRING_BASE do begin
     System.Delete(Result,i,Length(s)); Result:=Trim(Result);
     i:=AnsiPos(s,Result);
     end;
end; {TSokoFile.RemoveNonOptimalText}

function TSokoGame.RemoveRedundantWalls:Integer;
var Col,Row:Integer;
begin {Returns number of removed walls}
  Result:=SokFile_.RemoveRedundantWalls(BoardWidth,BoardHeight,Board);
  if Result<>0 then
     for Col:=1 to BoardWidth do
         for Row:=1 to BoardHeight do
             if (Board[Col,Row] and WALL)=0 then
                StartBoard[Col,Row]:=StartBoard[Col,Row] and (not WALL);
end; {TSokoGame.RemoveRedundantWalls}

function TSokoGame.RenameSnapshot(const OldName__,NewName__:String):Boolean;
var ErrorStr:String; Snapshot:TSnapshot;
begin
  Snapshot:=TSnapshot(Snapshots.GetItemByName(OldName__));
  Result:=Snapshot<>nil;
  if   Result then
       if   Snapshot.Rename(Snapshots.MakeUniqueName(NewName__,SnapshotTypeName(stSnapshot),True),ErrorStr) then {ok}
       else Result:=Error(ErrorStr,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_RENAME_SNAPSHOT)
  else Error(Format(TEXT_SNAPSHOT_NOT_FOUND_FORMAT,[OldName__]),TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_RENAME_SNAPSHOT);
end; {TSokoGame.RenameSnapshot}

procedure TSokoGame.RenumberCombinedMoves;
var Index,StartIndex,AllMovesLastPushIndex,Move:Integer;
begin {optimizes combined moves}
  with History do begin
    Moves[0]:=0;
    if ForcedInitialJumps<>0 then MakeCombinedMove(1,ForcedInitialJumps); {forced initial jumps: gets the player away from a box position}

    StartIndex:=Succ(ForcedInitialJumps); Index:=StartIndex;
    while (Index<=Top) and ((Moves[Index] and H_FLAG_JUMP)<>0) do Inc(Index);
    if Index>StartIndex then MakeCombinedMove(StartIndex,Pred(Index)); {initial jumps}

    StartIndex:=Index; AllMovesLastPushIndex:=0;
    for Index:=StartIndex to Top do begin
        Move:=Moves[Index];
        if (Move and H_FLAG_BOX)<>0 then begin {box push}
           if (Move and H_FLAG_BOX_CHANGE)<>0 then begin {changing focus to a new box (or focusing one for the first time)}
              if AllMovesLastPushIndex<>0 then {0: first time; otherwise, make a combined move}
                 MakeCombinedMove(StartIndex,AllMovesLastPushIndex); {'MakeCombinedMove' handles embedded jumps, hence, there is no need to separate jumps here}
              StartIndex:=Succ(AllMovesLastPushIndex);
              end;
           AllMovesLastPushIndex:=Index;
           end;
        end;
    MakeCombinedMove(StartIndex,AllMovesLastPushIndex); {last pushes, if any}
    MakeCombinedMove(Succ(AllMovesLastPushIndex),Top);  {remaining moves, if any}

    SeparateUndoneMoves;
    end;
end; {TSokoGame.RenumberCombinedMoves}

procedure TSokoGame.Reset(SeparateUndoneMoves__:Boolean);
var i:Integer;
begin {Resets the game to the start-position, respecting current reverse mode}
  History.Count:=0; History.PushCount:=0; {note: 'Top' is not touched; hence, 'Redo' is still available}
  History.Moves[0]:=0; History.LastBoxNo:=0; History.LastPushIndex:=0; History.PlayerLinesCount:=0;
  ForcedInitialJumps:=0;
  LoadBoard(StartBoard);

  if ReverseMode then with History do begin
     for i:=1 to BoxCount do with BoxPos[i] do       {remove all boxes from the board}
         Dec(Board[x,y],(i shl BOARD_FLAG_COUNT)+BOX);
     for i:=1 to BoxCount do with BoxPos[i] do begin {put the boxes on goal positions}
         x:=GoalPos[i].x; y:=GoalPos[i].y;
         Inc(Board[x,y],(i shl BOARD_FLAG_COUNT)+BOX);
         end;
     CalculateScoreAndState;
     {
     although it isn't implemented, the rest of the kernel (with the exception
     of importing moves from the clipboard) can handle it properly if the player
     automatically is moved away from a box position in the starting position
     for a reverse mode game;
     'ForcedInitialJumps' should hold the number of moves it takes the player
     to get away from the box square;
     }
     end;

  if SeparateUndoneMoves__ then SeparateUndoneMoves;
end; {TSokoGame.Reset}

function  TSokoGame.ResetAndLoadSaveGame(RestoreSaveGame__,
                                         ResetSaveGameAndLoadItIfItIsANormalModeGame__:Boolean):Boolean;
begin {Resets history and opens 'SaveGame' depending on parameters}
  Result:=True;

  History.Count:=0; History.Top:=0; History.PushCount:=0;
  SetReverseMode(False); Reset(True);

  if (SaveGame<>nil) and
     (SaveGame.MoveCount>0) then begin
     Result:=False;

     if      RestoreSaveGame__ then
             Result:=Self.LoadSnapshot(SaveGame)
     else if ResetSaveGameAndLoadItIfItIsANormalModeGame__ then begin
             SetReverseMode(SaveGame.ReverseMode);    {start a savegame from the beginning, not from saved position}
             Reset(True);                             {reset the board position}
             SaveGame.BoxPos   :=Self.BoxPos;         {update board snapshot}
             SaveGame.PlayerPos:=Self.PlayerPos;
             SaveGame.MoveCount:=0;                   {reset current position}
             SaveGame.PushCount:=0;

             if not SaveGame.ReverseMode then
                Result:=Self.LoadSnapshot(SaveGame);  {it might be confusing for the user if the game starts in reverse mode; to avoid that, loading the reverse mode savegame is postponed until the user changes to reverse mode manually}
             end;

     if not Result then begin
        History.Count:=0; History.Top:=0; History.PushCount:=0;
        SetReverseMode(False); Reset(True); Result:=True;
        end;
     end;
end; {TSokoGame.ResetAndLoadSaveGame}

function TSokoGame.SaveToFile(SokoFile__:TSokoFile; UpdateBestSolutions__,UpdateSnapshots__,UpdateTime__,UpdateBoardTransformation2D__,Flush__:Boolean):Boolean;
begin
  Result:=SaveToFile0(SokoFile__,UpdateBestSolutions__,UpdateSnapshots__,UpdateTime__,UpdateBoardTransformation2D__,Flush__);
end; {TSokoGame.SaveToFile}

function TSokoGame.SaveToFile0(SokoFile__:TSokoFile; UpdateBestSolutions__,UpdateSnapshots__,UpdateTime__,UpdateBoardTransformation2D__,Flush__:Boolean):Boolean;
var TimeMS:TTimeMS; s:String; n:TNode; Level,OriginalLevel:TLevel;
    BetterBuiltInSolutionMoves,BetterBuiltInSolutionPushes,v:TSnapshot;
    OriginalBestSolutionMoves,OriginalBestSolutionPushes:TNode;
    CurrentBoardTransformation2D:TBoardTransformation2D;
    SecondaryScoreMetrics:TSecondaryScoreMetrics;
    {$IFDEF SokobanYASC} View:TMultiViewItem; {$ENDIF}
begin {Saves current level to file}
  StopTimer;
  Result:=(SokoFile__<>nil) and (SokoFile__.Levels<>nil) and
          CreateObject(otLevel,TNode(Level));
  if Result then begin
     CurrentBoardTransformation2D:=BoardTransformation2D;
     DoBoardTransformation2D(BOARD_TRANSFORMATION_INVERSE[BoardTransformation2D],False);
     try
       Result:=Level.SetName(Name);

       if   Name='' then
            OriginalLevel:=nil
       else OriginalLevel:=TLevel(SokoFile__.Levels.GetItemByName(Name));
       OriginalBestSolutionMoves:=nil; OriginalBestSolutionPushes:=nil;
       if OriginalLevel=nil then begin
          UpdateBestSolutions__:=True; UpdateSnapshots__:=True;                 {it's a new level; update everything}
          end
       else begin
          if OriginalBestSolutionMovesName <>'' then begin
             OriginalBestSolutionMoves :=OriginalLevel.SnapshotsAsText.GetItemByName(OriginalBestSolutionMovesName);
             if OriginalBestSolutionMoves =nil then UpdateBestSolutions__:=True; {'nil': the original solution was not found in the file; update the best solutions, if any}
             end;
          if OriginalBestSolutionPushesName<>'' then begin
             OriginalBestSolutionPushes:=OriginalLevel.SnapshotsAsText.GetItemByName(OriginalBestSolutionPushesName);
             if OriginalBestSolutionPushes=nil then UpdateBestSolutions__:=True; {'nil': the original solution was not found in the file; update the best solutions, if any}
             end;

          {ensure that existing anonymous best solutions, if any, are renamed from 'Snapshot' to 'Solution'}
          {$IFDEF SokobanYASC}
            if (OriginalBestSolutionMoves <>nil)
               and
               (StrBeginsWith(OriginalBestSolutionMovesName ,SNAPSHOT_TYPE_NAME[stSnapshot])
                or
                (SnapshotsForm=nil)                                             {'nil': for safety, update the best solutions if the snapshots form has been destroyed at this time}
                or
                StrBeginsWith(OriginalBestSolutionMovesName ,SnapshotsForm.NormalModeSnapshotName)
               )
               then UpdateBestSolutions__:=True;
            if (OriginalBestSolutionPushes<>nil)
               and
               (StrBeginsWith(OriginalBestSolutionPushesName,SNAPSHOT_TYPE_NAME[stSnapshot])
                or
                (SnapshotsForm=nil)
                or
                StrBeginsWith(OriginalBestSolutionPushesName,SnapshotsForm.NormalModeSnapshotName)
               )
               then UpdateBestSolutions__:=True;
          {$ELSE}
            if (OriginalBestSolutionMoves <>nil) and StrBeginsWith(OriginalBestSolutionMovesName ,SnapshotTypeName(stSnapshot)) then UpdateBestSolutions__:=True;
            if (OriginalBestSolutionPushes<>nil) and StrBeginsWith(OriginalBestSolutionPushesName,SnapshotTypeName(stSnapshot)) then UpdateBestSolutions__:=True;
          {$ENDIF}
          end;

       if   (OriginalLevel=nil) or Notes.Modified then
            Result:=Notes.CopyTo(Level.Notes)
       else Result:=OriginalLevel.Notes.CopyTo(Level.Notes);

       TimeMS:=OriginalTimeMS;
       if   Result and
            Level.Notes.Lines.ReadString(KEY_TIME,s) and
            SokUtil_.StrToTime(s,TimeMS) and
            (TimeMS=OriginalTimeMS) then
       else OriginalTimeMS:=0;

       if Result then                                                           {board internal form -> text}
          Result:=Level.BoardToTextLines(BoardWidth,BoardHeight,StartBoard);

       if Result then begin
          if UpdateSnapshots__ then begin

             {$IFDEF SokobanYASC}                                               {make text-versions of current multiple views items}
               if Result and (Self=MainForm.Game) and Assigned(MainForm.MultiView) and (not MainForm.MultiView.IsEmpty) then begin
                  Result:=MainForm.MultiView.MarkDuplicates;

                  View:=TMultiViewItem(MainForm.MultiView.Items.First);
                  while Assigned(View) and Result do begin
                    if Assigned(View.Snapshot) and
                       (View.Snapshot.MoveCount>View.Snapshot.ForcedInitialJumps) then begin {drop 0-moves snapshots}
                       if View.Tag=0 then begin                                 {'0': a unique snapshot or a representative for a set of duplicates}
                          n:=TNode(View.Snapshot.MakeSnapshotAsText(PrettyPrintGamesEnabled,RleEnabled,CombinedMoveFormattingEnabled,True));
                          if   n<>nil then begin
                               Level.SnapshotsAsText.Push(n);
                               TSnapshotAsText(n).Notes.Lines.WriteString(TEXT_SCREEN_REGION,RectToStr(RectPlusOffset(View.Rect,-MainForm.MultiView.ClippingRect.Left,-MainForm.MultiView.ClippingRect.Top)));
                               end
                          else Result:=False;
                          end
                       else
                          if View.Tag<>Integer(View) then                     {'True': the view item is a duplicate of a snapshot on the 'Snapshots' list}
                             TSnapshot(Pointer(View.Tag)).Notes.Lines.WriteString(TEXT_SCREEN_REGION,RectToStr(RectPlusOffset(View.Rect,-MainForm.MultiView.ClippingRect.Left,-MainForm.MultiView.ClippingRect.Top))); // store the screen region for the view together with the existing snapshot
                       end;
                    View:=TMultiViewItem(View.Next);
                    end;
                  end;
             {$ENDIF}

             v:=TSnapshot(Snapshots.First);                                     {make text-versions of current snapshots}
             while (v<>nil) and Result do begin
               if v.MoveCount>v.ForcedInitialJumps then begin                   {drop 0-moves snapshots}
                  n:=TNode(v.MakeSnapshotAsText(PrettyPrintGamesEnabled,RleEnabled,CombinedMoveFormattingEnabled,True));
                  if n<>nil then Level.SnapshotsAsText.Push(n)
                  else Result:=False;
                  end;
               v:=TSnapshot(v.Next);
               end;
             Level.SnapshotsAsText.Reverse;                                     {snapshots are in reversed order: make it right}

             DeleteSnapshot(SaveGame);                                          {delete old savegame, if any}
             if Result and
                (History.Count>ForcedInitialJumps) and                          {check if current game should be saved}
                ({$IFDEF SokobanYASC}
                   ((Self=MainForm.Game) and Assigned(MainForm.MultiView) and (not MainForm.MultiView.IsEmpty)) {with multiple views, the savegame is needed to select which one to focus the next time the level is opened}
                   or
                 {$ENDIF}
                   ((not IsEqualToCurrentGame(BestSolutionMoves ))
                    and                                                         {there is no need to store a savegame if it's a part of one of the best solutions}
                    (not IsEqualToCurrentGame(BestSolutionPushes))
                   )
                )
                then begin
                if SecondaryMetricsInTitles then CalculateSecondaryScoreMetrics(SecondaryScoreMetrics);
                SaveGame:=MakeSnapshot(SnapshotTypeName(stSaveGame)+SPACE+
                                       Format(FORMAT_MOVES_AND_PUSHES,
                                              [History.Count,History.PushCount])+
                                       SecondaryMetricsFormattedAsATitleSuffix(
                                         SecondaryMetricsInTitles,
                                         SecondaryScoreMetrics)
                                      );
                n:=nil;
                if   SaveGame<>nil then n:=TNode(SaveGame.MakeSnapshotAsText(PrettyPrintGamesEnabled,RleEnabled,CombinedMoveFormattingEnabled,True));
                if   n<>nil then Level.SnapshotsAsText.Push(n)                  {add savegame to the list}
                else Result:=False;
                end;
             end;

       if Result then
          if UpdateBestSolutions__ then begin                                   {update best solutions}
             UpdateBestSolutionNames;
             if BestSolutionPushes<>nil then begin
                n:=TNode(BestSolutionPushes.MakeSnapshotAsText(PrettyPrintGamesEnabled,RleEnabled,CombinedMoveFormattingEnabled,True));
                if n<>nil then Level.SnapshotsAsText.Push(n)
                else Result:=False;
                end;

             if BestSolutionMoves<>nil then begin
                n:=TNode(BestSolutionMoves.MakeSnapshotAsText(PrettyPrintGamesEnabled,RleEnabled,CombinedMoveFormattingEnabled,True));
                if n<>nil then Level.SnapshotsAsText.Push(n)
                else Result:=False;
                end;

             {remove built-in solutions if the user has found better solutions}
             IsABetterBuiltInSolutionAvailable(BetterBuiltInSolutionMoves,BetterBuiltInSolutionPushes);
             if (BuiltInBestSolutionMoves<>BetterBuiltInSolutionMoves ) and
                (BuiltInBestSolutionMoves<>BetterBuiltInSolutionPushes) then    {caution: 'BuiltinBestSolutionMoves' may show up in either of the 'Better...' slots}
                DeleteSnapshot(BuiltInBestSolutionMoves);
             if BuiltInBestSolutionPushes<>BetterBuiltInSolutionPushes  then
                DeleteSnapshot(BuiltInBestSolutionPushes);
             end
          else begin {save old best solutions}
             if OriginalBestSolutionPushes<>nil then begin
                Level.SnapshotsAsText.Push(OriginalLevel.SnapshotsAsText.Remove(OriginalBestSolutionPushes,False));
                end;
             if OriginalBestSolutionMoves<>nil then begin
                Level.SnapshotsAsText.Push(OriginalLevel.SnapshotsAsText.Remove(OriginalBestSolutionMoves ,False));
                end;
             end;

       if Result and (BuiltInBestSolutionPushes<>nil) then begin                {built-in best solution/pushes}
          n:=TNode(BuiltInBestSolutionPushes.MakeSnapshotAsText(PrettyPrintGamesEnabled,RleEnabled,CombinedMoveFormattingEnabled,True));
          if n<>nil then Level.SnapshotsAsText.Push(n)
          else Result:=False;
          end;

       if Result and (BuiltInBestSolutionMoves <>nil) then begin                {built-in best solution/moves}
          n:=TNode(BuiltInBestSolutionMoves.MakeSnapshotAsText(PrettyPrintGamesEnabled,RleEnabled,CombinedMoveFormattingEnabled,True));
          if n<>nil then Level.SnapshotsAsText.Push(n)
          else Result:=False;
          end;

       if Result and UpdateTime__ and ((SessionTimeMS<>0) or (TimeMS<>OriginalTimeMS)) and (Notes<>nil) then begin
          TimeMS:=OriginalTimeMS+SessionTimeMS;
          if   (not Level.Notes.Lines.ReadString(KEY_TIME,s)) and
               (not Level.Notes.Lines.IsEmpty) then
               Level.Notes.Lines.AddBlankLine;                                  {separate 'time' from the rest of the lines}
          if   Level.Notes.Lines.WriteString(KEY_TIME,SokUtil_.TimeToStr(TimeMS)) then begin
               Notes.Modified:=True; ClearTimer; OriginalTimeMS:=TimeMS;
               end
          else Result:=False;
          end;

       if Result and UpdateBoardTransformation2D__ and (Notes<>nil) then begin
          if CurrentBoardTransformation2D=t2DRotate0DegreesClockwise then       {default transformation: remove old transformation from notes, if any}
             if Level.Notes.Lines.FindKey(KEY_BOARD_TRANSFORMATION,n) then begin
                Level.Notes.Lines.Remove(n,True);
                Level.Notes.Lines.TrimBlankLines;
                Notes.Modified:=True;
                end
             else
          else begin
             if   (not Level.Notes.Lines.ReadString(KEY_BOARD_TRANSFORMATION,s)) and
                  (not Level.Notes.Lines.IsEmpty) then
                  Level.Notes.Lines.AddBlankLine;                               {separate 'transformation' from the rest of the lines}
             if   Level.Notes.Lines.WriteString(KEY_BOARD_TRANSFORMATION,BoardTransformation2DToStr(CurrentBoardTransformation2D)+PERIOD) then
                  Notes.Modified:=True
             else Result:=False;
             end;
          end;

       if Result then begin                                                     {creating a text-version of the level succeeded; now update the file}
          SokoFile__.Modified:=True;
          if   OriginalLevel<>nil then begin
               SokoFile__.Levels.MoveAfter(Level,OriginalLevel);                {add new copy of the level,}
               SokoFile__.Levels.Remove(OriginalLevel,True);                    {and destroy the old one}
               end
          else SokoFile__.Levels.Add(Level);                                    {a new level, at least in this file}
          end;

       if Result and Flush__ then begin
          s:=SokoFile__.Name;

          Result:=SokoFile__.Flush;                                             {flush the file, if requested}

          if (not Result) and (ShowErrorMessages<>semNone) and
             (not StrEqual(SokoFile__.Name,s)) and (SokoFile__.Name<>'') then begin
             {saving the file succeeded, but only under a different file name;}
             {continue as if everything is all right, using the new file name}
             Result:=True;
             end;
          end;

       if Result then begin                                                     {if ok, then reset 'Modified' for all level components}
          Notes.Modified:=False;
          if BestSolutionMoves <>nil then BestSolutionMoves .Modified   :=False;
          if BestSolutionPushes<>nil then BestSolutionPushes.Modified   :=False;
          if BestSolutionMoves <>nil then OriginalBestSolutionMovesName :=BestSolutionMoves .Name;
          if BestSolutionPushes<>nil then OriginalBestSolutionPushesName:=BestSolutionPushes.Name;

          OriginalSnapshotCount:=Snapshots.Count;
          n:=Snapshots.First;
          while n<>nil do begin
            TSnapshot(n).Modified:=False;
            TSnapshot(n).Notes.Modified:=False;
            n:=n.Next;
            end;

          {$IFDEF SokobanYASC}
            if (Self=MainForm.Game) then
               if   Assigned(MainForm.MultiView) then begin
                    OriginalMultiViewCount:=MainForm.MultiView.Items.Count;
                    View:=TMultiViewItem(MainForm.MultiView.Items.First);
                    while Assigned(View) do begin
                      if Assigned(View.Snapshot) then begin
                         View.Snapshot.Modified:=False;
                         View.Snapshot.Notes.Modified:=False;
                         end;
                      View:=TMultiViewItem(View.Next);
                      end;
                    end
               else OriginalMultiViewCount:=0;
          {$ENDIF}

          SokoFile:=SokoFile__; SokoFileName:=SokoFile__.Name;                  {save original filename; note: 'SokoFile' must stay alive until the level is closed}
          end
       else Error(TEXT_TASK_FAILED,'');
       end;
     finally DoBoardTransformation2D(CurrentBoardTransformation2D,False);
             if not Result then
                SokoFile__.Levels.Remove(Level,True);                           {if failed, destroy the new level}
     end;
     end
  else Result:=SokUtil_.Error(TEXT_NO_FILE_SPECIFIED,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_SAVE_LEVEL);
end; {TSokoGame.SaveToFile0}

function TSokoGame.SaveSnapshot:Boolean;
var Snapshot:TSnapshot;
begin
  Result:=(Name<>'') and (Snapshots<>nil); Snapshot:=nil;
  if Result then Snapshot:=MakeSnapshot(''); {make new snapshot from current position, with default name}
  Result:=Snapshot<>nil;
  if Result then Snapshots.Add(Snapshot); {add it to the list}
end; {TSokoGame.SaveSnapshot}

function  TSokoGame.SecondaryMetricsAsText:String; {note: returns an empty string if 'SecondaryMetricsInTitles' is disabled}
var SecondaryScoreMetrics:TSecondaryScoreMetrics;
begin
  if   SecondaryMetricsInTitles then begin
       CalculateSecondaryScoreMetrics(SecondaryScoreMetrics);
       Result:=SecondaryMetricsFormattedAsATitleSuffix(True,SecondaryScoreMetrics);
       end
  else Result:='';
end;

procedure TSokoGame.SeparateMoves(Index__:Integer; var History__:THistory);
{When called after taking back a move, 'Index__' is the last move in the
 (combined) move that just has been undone.
}
var i:Integer;
begin
  with History__ do
    if (Index__< Top  ) and
       ((Moves[     Index__ ] and H_FLAG_ODD)=
        (Moves[Succ(Index__)] and H_FLAG_ODD)) then
       {for moves taken back, the odd-even flag is used for separation only,}
       {and its odd/even state might be wrong;
       {this way, the following scenario is handled correctly:}
       {. do the triple-move ABC,}
       {. undo it,}
       {. do AB instead,}
       {. undo AB again,
       {. finally, choose 'Redo';}
       {now AB is performed correctly, not ABC, because}
       { 'AB' and 'C' have different odd-even flags after the 'undo AB' step}
       for i:=Succ(Index__) to Top do
           Moves[i]:=(Moves[i]   and (not H_FLAG_ODD))  {flip the odd-even-number flag}
                     or
                     (H_FLAG_ODD and (not Moves[i]));
end; {TSokoGame.SeparateMoves}

procedure TSokoGame.SeparateUndoneMoves;
var i:Integer;
begin
  {
  Separating combined moves requires special attention on the
  border between moves actually taken and the moves still in
  the history as a result of undo-actions; thus, all 'undone' moves
  are flagged.
  }
  with History do begin
    Moves[0]:=0;
    for i:=1 to Count         do Moves[i]:=Moves[i] and (not H_FLAG_UNDO);  {performed moves, i.e., <= 'Count'}
    for i:=Succ(Count) to Top do Moves[i]:=Moves[i] or H_FLAG_UNDO;         {moves taken back, i.e., above 'Count'}
    end;
end; {TSokoGame.SeparateUndoneMoves}

procedure TSokoGame.SetReverseMode(ReverseMode__:Boolean);
begin
  ReverseMode:=ReverseMode__;
  if   ReverseMode then begin
       BoxTargetMaskForCalculations:=BOX_START_POSITION;
       if   ShowBoxStartPositionsAsGoalsInReverseMode then
            BoxTargetMaskForDisplay:=BOX_START_POSITION
       else BoxTargetMaskForDisplay:=GOAL;
       end
  else begin
    BoxTargetMaskForCalculations   :=GOAL;
    BoxTargetMaskForDisplay        :=GOAL;
    end;
  PlayersReachableSquaresTimestampAfterLastPush:=0; {invalidate any calculated distances to the player's reachable squares}
end; {TSokoGame.SetReverseMode}

function TSokoGame.SnapshotTypeName(SnapshotType__:TSnapshotType):String;
begin
  Result:=SNAPSHOT_TYPE_NAME[SnapshotType__];
end; {TSokoGame.SnapshotTypeName}

function TSokoGame.SolverNameText(const Text__:String):String;
begin {Solver name formatted as suffix to a solution name}
  Result:=ExtractTextInParenthesis(Text__);
  if Result<>'' then
     if   StrEqual(Result,TEXT_NON_OPTIMAL) or StrDigitsOnly(Result) then
          Result:=''
     else Result:=SPACE+StrWithParenthesis(Result);
end; {TSokoGame.SolverNameText}

procedure TSokoGame.StartTimer;
begin
  if   TimingEnabled then
       repeat StartTimeMS:=GetTickCount;
       until  StartTimeMS<>0 {'0' is a reserved value meaning that the timer is disabled}
  else StartTimeMS:=0;
end; {TSokoGame.StartTimer}

procedure TSokoGame.StopTimer;
begin
  ElapsedTimeMS; {updates time for current session, 'SessionTimeMS'}
  StartTimeMS:=0;
end; {TSokoGame.StopTimer}

function TSokoGame.TestForNewBestSolution:Boolean;
{'History' must contain a solved game when this procedure is called}
{Returns 'True' if this solution is a new best solution}
var SecondaryScoreMetrics:TSecondaryScoreMetrics;
    UserReverseMode:Boolean; UserHistory:THistory;

  function  RemoveRedundantMoves:Integer;
  var i,j:Integer; Move:THistoryMove;
  begin {removes simple redundant moves only, e.g., A->B->A}
    Result:=0; i:=1;
    {$IFDEF SokobanYASC}
      if MainForm.OptimizeMovesBetweenPushes then
    {$ENDIF}
         with History do
           while i<Top do begin
             Move:=Moves[i];
             if   (TDirection(Move and H_MASK_DIRECTION)= {opposite direction?}
                   OPPOSITE_DIRECTION[TDirection(Moves[Succ(i)] and H_MASK_DIRECTION)]) and
                  ((Move and H_FLAG_BOX)=0) and {'True': no boxes}
                  ((Moves[Succ(i)] and H_FLAG_BOX)=0) then begin
                  Dec(Count,2); Dec(Top,2); Inc(Result,2);
                  for j:=i to Top do Moves[j]:=Moves[j+2];
                  if i>1 then Dec(i); {a new pair may be redundant now, e.g., ...ulrd... -> ...ud... -> ......}
                  end
             else Inc(i);
             end;
    RenumberCombinedMoves; {optimize combined moves grouping}
  end; {RemoveRedundantMoves}

  function  ReverseHistory:Boolean;
  var i,InitialNonPushingMovesCount,PlayerLinesCount:Integer;
      IsPreferredDirectionOK:Boolean; Direction:TDirection;
      ForwardHistory:THistory; Moves:TPlayerMoves;
  begin {Only implemented for solved reverse mode games; returns 'False' if there are too many moves}
    Result:=True;

    ForwardHistory.Count:=0; ForwardHistory.PushCount:=0;
    ForwardHistory.Top:=0; ForwardHistory.LastPushIndex:=0;
    ForwardHistory.LastBoxNo:=0; ForwardHistory.PlayerLinesCount:=0;
    ForwardHistory.Moves[0]:=0;

    if ReverseMode and (GameState=gsSolved) then begin
       if PlayerPath(PlayerStartPos,PlayerPos,False,False,Low(TDirection),InitialNonPushingMovesCount,PlayerLinesCount,IsPreferredDirectionOK,PPlayerMoves(Addr(Moves))) then begin
          Moves[0]:=PlayerStartPos; {make moves from the starting position to the player's current position; store them in the 'ForwardHistory' history}
          for i:=1 to InitialNonPushingMovesCount do with Moves[i] do begin {note that the game may be too long and overflow the history when these leading moves and the moves from the reverse-mode game are concatenated}
              if   DxDyToDirection(x-Moves[Pred(i)].x,y-Moves[Pred(i)].y,Direction) then begin
                   Inc(ForwardHistory.Count);
                   ForwardHistory.Moves[ForwardHistory.Count]:=Ord(Direction)+H_FLAG_ODD;
{
                   // adding the final pulls from the current player position to
                   // the player's starting position in the normal forward
                   // game is not in production, and it requires
                   // substantial changes of the logic in the rest of the kernel
                   // if it's activated;
                   j:=UserHistory__.Count+InitialNonPushingMovesCount-Pred(i);
                   if j<=MAX_MOVES then begin // 'True': the user's reverse mode history has room for this non-pushing player move after the last pull
                      UserHistory.Moves[j]:=Ord(OPPOSITE_DIRECTION[Direction])+H_FLAG_ODD;
                      UserHistory.Top     :=Max(UserHistory.Top,j);
                      end;
}
                   end
              else raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,['TSokoGame.ReverseHistory']));
              end;

          Result:=AppendReverseModeSolution(History.Count,Addr(History.Moves),0,ForwardHistory); {'True': 'ForwardHistory' now contains the forward solution derived from the reverse mode solution}
          if Result then begin
             History:=ForwardHistory; SetReverseMode(False);
             end;
          end
       else raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,['TSokoGame.ReverseHistory']));
       end
    else begin
       Error(TEXT_NOT_IMPLEMENTED,'TSokoGame.TestForNewBestSolution.ReverseHistory');
       end;
  end; {ReverseHistory}

begin {TestForNewBestSolution}
  Result:=False;
  if GameState=gsSolved then with History do begin
     UserHistory:=History; UserReverseMode:=ReverseMode;       {save 'History' so it can be restored later; 'MakeSnapshot' and 'ReverseHistory' may modify it}
     if (not ReverseMode) or ReverseHistory then               {reversal of a reverse mode game may fail because of too many moves}
        try
          RemoveRedundantMoves;                                {remove any simple redundant moves from 'History'}
          LastSolutionWasTheFirstOne:=(BestSolutionMoves=nil) and (BestSolutionPushes=nil);

          if   LastSolutionWasTheFirstOne then
               FillChar(SecondaryScoreMetrics,SizeOf(SecondaryScoreMetrics),0)
          else CalculateSecondaryScoreMetrics(SecondaryScoreMetrics);

          if IsABetterSolutionMoves(Count,PushCount,SecondaryScoreMetrics) then begin {save new best solution/moves:}
             if   BestSolutionPushes=nil then
                  BestSolutionPushes:=BestSolutionMoves        {maybe current best solution/moves really is best solution/pushes, hence, put it there and deal with it in the lines below}
             else DethroneBestSolution(BestSolutionMoves);     {dethrone the old best solution, if any}

             BestSolutionMoves:=MakeSnapshot(SnapshotTypeName(stBestSolutionMoves)+SPACE+
                                             Format(FORMAT_MOVES_AND_PUSHES,[History.Count,History.PushCount])+
                                             SecondaryMetricsAsText
                                            );
             if (BestSolutionPushes<>nil) and
                (PushCount<=BestSolutionPushes.PushCount) and
                IsABetterSolutionPushes(Count,PushCount,SecondaryScoreMetrics) then
                DethroneBestSolution(BestSolutionPushes);      {dethrone the old best solution/pushes}

             {$IFDEF SokobanYASC}
               if (BestSolutionMoves<>nil) and (not IsLoading) then
                  MainForm.SaveSnapshotToLogFile(MakeIniFileSectionFileName(Self.SokoFileName,Self.Name),'',BestSolutionMoves.GetMovesAsText);
             {$ENDIF}

             Result:=True;
             end;

          if IsABetterSolutionPushes(Count,PushCount,SecondaryScoreMetrics) then begin {'True': save new best solution/pushes}
             DethroneBestSolution(BestSolutionPushes);     {first dethrone the old best solution/pushes, if any}
             BestSolutionPushes:=MakeSnapshot(SnapshotTypeName(stBestSolutionPushes)+SPACE+
                                              Format(FORMAT_MOVES_AND_PUSHES,[History.Count,History.PushCount])+
                                              SecondaryMetricsAsText
                                             );
             {$IFDEF SokobanYASC}
               if (BestSolutionPushes<>nil) and (not IsLoading) then
                  MainForm.SaveSnapshotToLogFile(MakeIniFileSectionFileName(Self.SokoFileName,Self.Name),'',BestSolutionPushes.GetMovesAsText);
             {$ENDIF}

             Result:=True;
             end;

          if Result and (not KeepOriginalSnapshotNames) then UpdateBestSolutionNames;

        finally History:=UserHistory;                          {restore the user's game, including redundant moves, if any}
                SetReverseMode(UserReverseMode);
        end;
     end;
end; {TSokoGame.TestForNewBestSolution}

procedure TSokoGame.ToggleReverseMode;
var V:TSnapshot;
begin {Besides toggling reverse mode, 'ToggleReverseMode' also loads/saves current 'SaveGame'; don't confuse it with 'SetReverseMode'}
  V:=SaveGame;
  try
    if History.Top>ForcedinitialJumps then {'Top', not 'Count': save moves, if any, even if current position is start position}
       SaveGame:=MakeSnapshot(SnapshotTypeName(stSaveGame)+SPACE+Format(FORMAT_MOVES_AND_PUSHES,[History.Count,History.PushCount]));
    SetReverseMode(not ReverseMode);
    if (V<>nil) and (V.ReverseMode=ReverseMode) then
       LoadSnapshot(V)
    else begin
       History.Top:=0; Reset(True);
       end;
  finally
    if V<>SaveGame then DeleteSnapshot(V);
  end;
end; {TSokoGame.ToggleReverseMode}

function TSokoGame.Undo(UndoCombinedMoves__:Boolean):Boolean;
var i,dx,dy,OldCount,NewCount,BoxNo:Integer;
    PlayerFromPos,BoxFromPos,BoxToPos:TColRow;
begin
  Result:=History.Count>ForcedInitialJumps;          {in reverse mode, the first moves may be forced, to get the player away from a box-position}
  if Result then with History do begin
     if (Moves[Count] and H_FLAG_JUMP)<>0 then
        UndoCombinedMoves__:=True;                   {a jump-sequence cannot be broken}

     OldCount:=Count; NewCount:=Count;
     if UndoCombinedMoves__ then                     {take back all moves belonging to the same combined move group}
        while (NewCount>Succ(ForcedInitialJumps)) and
              ((Moves[NewCount      ] and (H_MASK_MOVE_SEPARATOR or H_FLAG_JUMP))=
               (Moves[Pred(NewCount)] and (H_MASK_MOVE_SEPARATOR or H_FLAG_JUMP))) do
              Dec(NewCount);

     Dec(NewCount);                                  {new position}

     LastBoxNo:=0; LastPushIndex:=0;                 {reset last moved box}
     while (Count>0) and (LastBoxNo=0) do begin      {undo moves until the last moved box, if any, before the new state has been found}
       UnpackLastMove(PlayerFromPos,PlayerPos,BoxFromPos,BoxToPos,BoxNo);
       Dec(Board[PlayerPos.x,PlayerPos.y],PLAYER);   {update player-position}
       PlayerPos:=PlayerFromPos;
       Inc(Board[PlayerPos.x,PlayerPos.y],PLAYER);

       Dec(Count);                                   {decrease number of moves}

       if BoxNo<>0 then begin                        {update box-position, if a box was moved}
          Dec(Board[BoxToPos  .x,BoxToPos  .y],(BoxNo shl BOARD_FLAG_COUNT)+BOX);
          Inc(Board[BoxFromPos.x,BoxFromPos.y],(BoxNo shl BOARD_FLAG_COUNT)+BOX);
          BoxPos[BoxNo]:=BoxFromPos;
          Dec(PushCount);                            {decrease number of pushes}
          if Count<NewCount then begin               {'<': found the last moved box before the new state}
             LastBoxNo:=BoxNo;
             LastPushIndex:=Succ(Count);
             end;
          end;

       if   Count>0 then
            if   TDirection(Moves[Count] and H_MASK_DIRECTION)<>TDirection(Moves[Succ(Count)] and H_MASK_DIRECTION) then
                 Dec(PlayerLinesCount)               {decrease the number of player lines}
            else begin end
       else PlayerLinesCount:=0;
       end;

     while Count<NewCount do begin                   {redo any moves that were taken back in order to find the last moved box, if any}
       if   Count>0 then
            if   TDirection(Moves[Count] and H_MASK_DIRECTION)<>TDirection(Moves[Succ(Count)] and H_MASK_DIRECTION) then
                 Inc(PlayerLinesCount)               {update the number of player lines}
            else
       else PlayerLinesCount:=1;
       Inc(Count);
       if (Moves[Count] and H_FLAG_BOX)<>0 then Inc(PushCount);
       HistoryMoveToDxDy(Moves[Count],dx,dy);
       DoMoveUpdateBoard0(dx,dy,Moves[Count]);       {note: 'DoMoveUpdateBoard0', not 'DoMoveUpdateBoard', i.e., no inherited updating}
       end;

     CalculateScoreAndState;                         {calculate new score and state}

     for i:=Succ(Count) to OldCount do               {update combined move separator for the move(s) taken back}
         Moves[i]:=Moves[i] or H_FLAG_UNDO;

     SeparateMoves(OldCount,History);
     end;
end; {TSokoGame.Undo}

procedure TSokoGame.UnpackLastMove(var PlayerFromPos__,PlayerToPos__,
                                       BoxFromPos__,BoxToPos__:TColRow;
                                   var BoxNo__:Integer);
var dx,dy,Move:Integer;
begin {Unpacks last move from history; precondition: the board must have been updated}
  BoxNo__:=0;
  if History.Count>ForcedInitialJumps then begin
     Move:=History.Moves[History.Count];
     dx:=DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),ColAxis];
     dy:=DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),RowAxis];
     PlayerToPos__    :=PlayerPos;
     PlayerFromPos__.x:=PlayerToPos__.x-dx;
     PlayerFromPos__.y:=PlayerToPos__.y-dy;

     if (Move and (H_FLAG_BOX+H_FLAG_JUMP))=H_FLAG_BOX then begin
        if ReverseMode then begin
           BoxToPos__    :=PlayerFromPos__;
           BoxFromPos__.x:=BoxToPos__.x-dx;
           BoxFromPos__.y:=BoxToPos__.y-dy;
           end
        else begin {normal game mode}
           BoxFromPos__:=PlayerPos;
           BoxToPos__.x:=BoxFromPos__.x+dx;
           BoxToPos__.y:=BoxFromPos__.y+dy;
           end;
        BoxNo__:=Board[BoxToPos__.x,BoxToPos__.y] shr BOARD_FLAG_COUNT;
        end
     else; {'BoxFromPos__' and 'BoxToPos__' are undefined}
     end
  else begin
     PlayerFromPos__:=PlayerStartPos; PlayerToPos__:=PlayerPos;
     end;
end; {TSokoGame.UnpackLastMove}

procedure TSokoGame.UpdateBestSolutionNames;
var s:String;
begin
  if BestSolutionMoves<>nil then with BestSolutionMoves do begin
     if   BestSolutionPushes=nil then
          s:=SnapshotTypeName(stBestSolution)
     else s:=SnapshotTypeName(stBestSolutionMoves);
     SetName(s+
             SPACE+
             Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
             SecondaryMetricsFormattedAsATitleSuffix(
               SecondaryMetricsInTitles,
               SecondaryScoreMetrics)+
             SolverNameText(Name));
     end;

  if BestSolutionPushes<>nil then with BestSolutionPushes do begin
     if   BestSolutionMoves=nil then
          s:=SnapshotTypeName(stBestSolution)
     else s:=SnapshotTypeName(stBestSolutionPushes);
     SetName(s+
             SPACE+
             Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
             SecondaryMetricsFormattedAsATitleSuffix(
               SecondaryMetricsInTitles,
               SecondaryScoreMetrics)+
             SolverNameText(Name));
     end;
end; {TSokoGame.UpdateBestSolutionNames}

{TSnapshot}

constructor TSnapshot.Create; {throws EOutOfMemory}
begin
  Inherited;
  Moves:=nil; Notes:=nil; Modified:=False; ReverseMode:=False; SnapshotTag:=0; GameState:=gsNull; MovesAsText:=''; {clear the object; not strictly necessary in Delphi, but safe programming-style}
  Notes:=TNotes.Create;
end; {TSnapshot.Create}

destructor TSnapshot.Destroy;
begin
  Clear; Notes.Free;
  Inherited;
end; {TSnapshot.Destroy}

function TSnapshot.ByteSize(Capacity__:Integer):Integer;
begin {Returns bytes required to hold the number of moves given by 'Capacity__'}
  Result:=SizeOf(Moves^[Low(Moves^)])*(Capacity__+2); {'+2': Moves[0] isn't used, + one extra, so a pointer beyond the last element is legal}
end; {TSnapshot.ByteSize}

procedure TSnapshot.Clear;
begin
  if Moves<>nil then FreeMem(Moves);
  if Notes<>nil then Notes.Clear;
  Moves:=nil; Modified:=False; MoveCapacity:=0; ReverseMode:=False; GameState:=gsNull;
  Inherited;
end; {TSnapshot.Clear}

function TSnapshot.Clone:TSnapshot;
var i:Integer;
begin {Makes a complete copy of the snapshot}
  Result:=nil;
  try
         Result                           :=TSnapshot.Create;
         if   Result.SetName(Text) and
              Notes.CopyTo(Result.Notes)  then begin
              Result.BoxPos               := BoxPos;
              Result.ForcedInitialJumps   := ForcedInitialJumps;
              Result.GameState            := GameState;
              Result.LastBoxNo            := LastBoxNo;
              Result.LastPushIndex        := LastPushIndex;
              Result.Modified             := Modified;
              Result.MoveCapacity         := MoveCapacity;
              Result.MoveCount            := MoveCount;
              Result.MovesAsText          := MovesAsText;
              Result.MoveTop              := MoveTop;
              Result.PlayerPos            := PlayerPos;
              Result.PushCount            := PushCount;
              i                           := ByteSize(MoveCapacity);
              GetMem(Result.Moves,i);
              System.Move(Moves^,Result.Moves^,i); {move: source, destination, bytes}
              Result.ReverseMode          :=ReverseMode;
              Result.SecondaryScoreMetrics:=SecondaryScoreMetrics;
              Result.SnapshotTag          :=SnapshotTag;
              end
         else raise Exception.Create(TEXT_MEMORY_FULL);
  except on   E:Exception do begin
              Result.Free; Result:=nil;
              SokUtil_.Error(E.Message,'TSnapshot.Clone');
              end;
  end;
end; {TSnapshot.Clone}

function TSnapshot.CopyTo(Destination__:TNode):Boolean;
begin
  Result:=SokUtil_.Error(Format(TEXT_INTERNAL_ERROR_FORMAT,[TEXT_NOT_IMPLEMENTED]),'TSnapshot.CopyTo');
end; {TSnapshot.CopyTo}

function TSnapshot.ExpandMoves(MoveTop__:Integer; Moves__:PHistoryMoves):Boolean;
var i:Integer; OldMoves,NewMoves:PHistoryMoves;
begin {Tries to substitute the existing moves with the longer variation stored in 'Moves__';}
      {Precondition: all moves up to and including the existing move top are identical in the old moves and the new moves}
  Result:=False;
  if MoveTop__>MoveTop then
     try         i:=Self.ByteSize(MoveTop__);
                 GetMem(NewMoves,i);
                 System.Move(Moves__^,NewMoves^,i); {move: source, destination, bytes}
                 OldMoves:=Moves;
                 Moves:=NewMoves;
                 MoveTop:=MoveTop__;
                 MoveCapacity:=MoveTop;
                 if OldMoves<>nil then FreeMem(OldMoves);
                 Modified:=True;
                 Result:=True;
     except on   E:Exception do begin
                 Result:=SokUtil_.Error(E.Message,'TSokoGame.ExpandMoves');
                 end;
  end;
end; {TSnapshot.ExpandMoves}

function TSnapshot.HasIdenticalMoves(ReverseMode__:Boolean; MoveCount__,MoveTop__:Integer; Moves__:PHistoryMoves):Boolean;
var i,CompareMoveTop:Integer;
begin {precondition: 'MoveCount__' <= 'MoveTop__'}
  if   MoveTop>=MoveTop__ then
       CompareMoveTop:=MoveTop__
  else CompareMoveTop:=MoveTop; {check if the existing snapshot is a prefix for the moves stored in 'Moves__'}

  Result:=(ReverseMode=ReverseMode__)
          and
          ((MoveCount__=-1) or (MoveCount= MoveCount__   ))  {if 'MoveCount__' = -1 then don't compare the current position in the game}
          and
          ((MoveTop__  =-1) or (MoveTop  >=CompareMoveTop)); {if 'MoveTop__'   = -1 then don't compare the moves after the current position in the game}
  for i:=Max(MoveCount__,CompareMoveTop) downto 1 do {compare in descending order because differences, if any, typically occur towards the end of the game}
      if   Result then
           Result:=(Moves[i] and H_MASK_DIRECTION) = (Moves__[i] and H_MASK_DIRECTION)
      else break; {quick-and-dirty exit the 'for' loop as soon as a difference has been found}

  if  Result and (CompareMoveTop<MoveTop__) then {'True': the existing snapshot is a prefix for the moves stored in 'Moves__'}
      ExpandMoves(MoveTop__,Moves__); {try to substitute the existing moves with the longer variation stored in 'Moves__'}
end; {TSnapshot.HasIdenticalMoves}

function TSnapshot.GetMovesAsText:String;
begin
  if (MovesAsText='') {'MovesAsText' is calculated on demand only}
     and
     (MoveCount>0)
     and
     (not MovesToText(Moves,MoveCount,MoveCount,ReverseMode,False,False,MovesAsText)) then begin
     MovesAsText:='';
     raise Exception.Create(TEXT_MEMORY_FULL);
     end;
  Result:=MovesAsText;
end;

{$IFDEF SokobanYASC}

  function TSnapshot.MakeName(SecondaryMetricsInTitles__:Boolean):String;

    function IsAStandardName(const Name__:String):Boolean;
    var i,j:Integer;
    begin
      i:=AnsiPos(SnapshotsForm.NormalModeSnapshotName,Name__);
      if i=STRING_BASE then begin
         Inc(i,Pred(Length(SnapshotsForm.NormalModeSnapshotName))); j:=StrLastCharIndex(Name__);
         while (j>=STRING_BASE) and {drop trailing number, if any}
               ((Name__[j]=SPACE) or IsADigitChar(Name__[j])) do Dec(j);
         Result:=i>=j; {'True': nothing else than snapshot-text followed by a sequence number}
         end
      else begin
         i:=AnsiPos(SNAPSHOT_TYPE_NAME[stSnapshot],Name__);
         if i=STRING_BASE then begin
            Inc(i,Pred(Length(SNAPSHOT_TYPE_NAME[stSnapshot]))); j:=StrLastCharIndex(Name__);
            while (j>=STRING_BASE) and {drop trailing number, if any}
                  ((Name__[j]=SPACE) or IsADigitChar(Name__[j])) do Dec(j);
            Result:=i>=j; {'True': nothing else than snapshot-text followed by a sequence number}
            end
         else
            Result:=False;
         end;
    end;

  begin
    try    if   (Name='') or IsAStandardName(Name) then begin
                if   ReverseMode then
                     Result:=SnapshotsForm.ReverseModeSnapshotName
                else Result:=SnapshotsForm.NormalModeSnapshotName;
                Result:=Result+SPACE+
                        Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
                        SecondaryMetricsFormattedAsATitleSuffix(SecondaryMetricsInTitles__,SecondaryScoreMetrics);
                end
           else Result:=Name;
    except on   E:Exception do begin
                SokUtil_.Error(E.Message,'TSnapshot.MakeDefaultName');
                Result:=SnapshotsForm.NormalModeSnapshotName;
                end;
    end;
  end;

{$ELSE}

  function TSnapshot.MakeName(SecondaryMetricsInTitles__:Boolean):String;

    function IsAStandardName(const Name__:String):Boolean;
    var i,j:Integer;
    begin
      i:=AnsiPos(SNAPSHOT_TYPE_NAME[stSnapshot],Name__);
      if i=STRING_BASE then begin
         Inc(i,Pred(Length(SNAPSHOT_TYPE_NAME[stSnapshot]))); j:=StrLastCharIndex(Name__);
         while (j>=STRING_BASE) and {drop trailing number, if any}
               ((Name__[j]=SPACE) or IsADigitChar(Name__[j])) do Dec(j);
         Result:=i>=j; {'True': nothing else than snapshot-text followed by a sequence number}
         end
      else
         Result:=False;
    end;

  begin
    try    if   (Name='') or IsAStandardName(Name) then begin
                if   ReverseMode then
                     Result:=SNAPSHOT_TYPE_NAME[stReverseSnapshot]
                else Result:=SNAPSHOT_TYPE_NAME[stSnapshot];
                Result:=Result+SPACE+
                        Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
                        SecondaryMetricsFormattedAsATitleSuffix(SecondaryMetricsInTitles__,SecondaryScoreMetrics);
                end
           else Result:=Name;
    except on   E:Exception do begin
                SokUtil_.Error(E.Message,'TSnapshot.MakeDefaultName');
                Result:=SNAPSHOT_TYPE_NAME[stSnapshot];
                end;
    end;
  end;

{$ENDIF}

function TSnapshot.MakeSnapshotAsText(PrettyPrintGamesEnabled__,RleEnabled__,CombinedMoveFormattingEnabled__,CopyNotes__:Boolean):TSnapshotAsText;
begin
  Result:=nil;
  if CreateObject(otSnapshotAsText,TNode(Result)) and
     Result.SetName(Self.Name) and
     Result.MovesToTextLines(Moves,MoveCount,MoveTop,ReverseMode,PrettyPrintGamesEnabled__,RleEnabled__,CombinedMoveFormattingEnabled__) and
     ((not CopyNotes__) or Notes.CopyTo(TNode(Result.Notes))) then begin
     end
  else begin
     Result.Free; Result:=nil;
     end;
end;{TSnapshot.MakeSnapshotAsText}

function TSnapshot.ObjectType:TObjectType;
begin
  Result:=otSnapshot;
end; {TSnapshot.ObjectType}

function TSnapshot.Rename(const NewName__:String; var ErrorStr__:String):Boolean;
var SnapshotType:TSnapshotType;
begin
  Result:=NewName__=Name; ErrorStr__:='';
  if not Result then begin
     Result:=not IsBlank(NewName__);
     if   Result then begin
          Result:=(not IsASpecialSnapshotName__(NewName__,SnapshotType))
                  or
                  StrBeginsWith(Name,SNAPSHOT_TYPE_NAME[SnapshotType]); {renaming the same type of snapshot is ok}
          if   Result then begin
               Result:=SetName(NewName__);
               if   Result then Modified:=True
               else ErrorStr__:=TEXT_TASK_FAILED+NL+NL+TEXT_OLD_VALUE_IS_RESTORED;
               end
          else ErrorStr__:=Format(TEXT_NAME_IS_RESERVED_FORMAT,[NewName__]);
          end
     else ErrorStr__:=TEXT_NAME_CANNOT_BE_BLANK_2;
     end;
end; {TSnapshot.Rename}

function TSnapshot.WriteToFile(TextFile__:PTextFile):Integer; {throws EInOutError}
begin
  SokUtil_.Error(Format(TEXT_INTERNAL_ERROR_FORMAT,[TEXT_NOT_IMPLEMENTED]),'TSnapshot.WriteToFile');
  Result:=-1;
end; {TSnapshot.WriteToFile}

{
 --------------------------------------------------------------------
 Initialization and Finalization
 --------------------------------------------------------------------
}

procedure Initialize;
begin
{$WARNINGS OFF} {paranoia checks, just to be sure}
  if MAX_BOARD_WIDTH      <> MAX_BOARD_HEIGHT    then Halt; {rotating and mirroring requires maximum width = maximum height}
  if MAX_BOARD_SIZE       >= ((MaxInt-2*(MAX_MOVES+1)) div 2)                                 then Halt; {in path finding, 'INFINITY' distance is sometimes represented by this right side expression which has the property that 2 'INFINITY' path lengths can be added without causing an integer overflow}
  if (MOVE_LIST_FLAG_BOX  <= MAX_BOARD_WIDTH)    or (MOVE_LIST_FLAG_BOX  <= MAX_BOARD_HEIGHT) then Halt; {SokGame_.TMove: the x-coordinate contains the box-move flag}
  if (MOVE_LIST_FLAG_JUMP <= MAX_BOARD_WIDTH)    or (MOVE_LIST_FLAG_JUMP <= MAX_BOARD_HEIGHT) then Halt; {SokGame_.TMove: the y-coordinate contains the jump-move flag}
  if SizeOf(TMoves)       <  SizeOf(TPlayerMoves)                                             then Halt; {a 'TMoves' array may be used instead of a 'TPlayerMoves' array}
  if (MOVE_FLAG_KEYBOARD  <= High(THistoryMove)) or
     (MOVE_FLAG_KEYBOARD  =  PLAYER_LEGAL_MOVE)  or
     (MOVE_FLAG_KEYBOARD  =  PLAYER_TRY_MOVE)                                                 then Halt; {'TGame.TryMove' mixes history-flags, board-flags, and move-flags}
  if MAX_MOVES            <  MAX_BOARD_SIZE                                                   then Halt; {'SokGame_.NormalizeBoard' may move the player on the board}
{$WARNINGS ON}

{ SetDefaultScoreMetricsWeights;}
  BoardTimestamps.Timestamp:=High(BoardTimestamps.Timestamp);
  PlayerPathBoardTimestamps.Timestamp:=High(PlayerPathBoardTimestamps.Timestamp);
end; {Initialize}

procedure Finalize;
begin
end; {Finalize}

initialization
  Initialize;

finalization
  Finalize;

end.

