unit Dead_; {Deadlock positions}

{$DEFINE LIGHTWEIGHT_DEADLOCK_GENERATION}      {this generates a reasonable number of deadlocks and works pretty fast; if undefined, the deadlock generation can run "forever"}

interface

uses syncObjs,Windows,Classes,
     Sound_,Mandal2_,SokUtil_,SokFile_;

const
  INFINITY                                       = (MaxInt div 2)-1; {(-INFINITY), (2*INFINITY), and (2*(-INFINITY)) must be legal integer values}

  // error codes for calculating pushes lower bound must be negative integers
  PUSHES_LOWER_BOUND_ERROR_NUMERIC_OVERFLOW      = -3;
  PUSHES_LOWER_BOUND_ERROR_TASK_ABANDONED        = -5;
  PUSHES_LOWER_BOUND_ERROR_TASK_FAILED           = -1;
  PUSHES_LOWER_BOUND_ERROR_TASK_FAILED_BUSY      = -4;
  PUSHES_LOWER_BOUND_ERROR_TIMEOUT               = -2;
  PUSHES_LOWER_BOUND_PARITY_EVEN                 = -6;
  PUSHES_LOWER_BOUND_PARITY_ODD                  = -7;

type
  TDeadlocksThreadStateType= (dlIdle,dlLoadGame,dlIsCalculating,dlStopCalculation,dlTerminate,dlTerminated);

  TDeadlocksThread         = class(TThread1)
  private
    fCriticalSection       : TCriticalSection;
    Board                  : SokFile_.TBoard;
    BoardHeight            : Integer;
    BoardWidth             : Integer;
    Hint                   : String;
    Title                  : String;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure   Execute; override;
    procedure   SynchronizedHint;
  end;

  TDeadlocks               = class(TObject) // caution: this is not a general deadlock detection class;
  private                                   // it's hard-wired to work together with 'MainForm' and 'MainForm.Game'
    DeadlocksThread        : TDeadlocksThread;
    fCalculatePushesLowerBoundTimeLimitMS
                           : TTimeMS;
    fDeadlockDetectionMaxPushCount
                           : Integer;
    fDeadlockDetectionTimeLimitMS
                           : TTimeMS;
  protected
    function    GetCalculated:Boolean;
    function    GetCount:Integer;
    function    GetLoggedCount:Integer;
    function    GetOverflowingDeadlockSet:Integer;
    function    GetThreadState:TDeadlocksThreadStateType;
    function    GetTimeMS:TTimeMS;
    procedure   SetOverflowingDeadlockSet(OverflowingDeadlockSet__:Integer);
    procedure   SetDeadlockDetectionTimeLimitMS(DeadlockDetectionTimeLimitMS__:TTimeMS);
  public
    DeadlockedBoxCount     : Integer;       {should have been a property}
    DeadlockedBoxSquares   : TBoxPositions; {should have been a property}
    DeadlockStats          : String;        {should have been a property}

    constructor Create;
    destructor  Destroy; override;

    procedure   Clear;
    function    IsALegalMove(BoxNo__,Col__,Row__:Integer; CheckSimpleDeadlocks__:Boolean):Boolean;
    function    IsALegalPosition:Boolean;
    function    LoadBoard : Boolean;
    procedure   LoadGame(BackgroundJob__,CalculateDeadlockSets__:Boolean);
    function    PushesLowerBound : Integer;
    function    MoveBox(BoxNo__:Integer):Boolean;
    procedure   Resume;
    procedure   Suspend;
    function    WasALegalMove(BoxNo__:Integer; BoxFromPos__,BoxToPos__:TColRow):Boolean;

    property    Calculated :Boolean                   read GetCalculated;
    property    CalculatePushesLowerBoundTimeLimitMS:TTimeMS
                                                      read fCalculatePushesLowerBoundTimeLimitMS  write fCalculatePushesLowerBoundTimeLimitMS;
    property    Count:Integer                         read GetCount;
    property    DeadlockDetectionMaxPushCount:Integer read fDeadlockDetectionMaxPushCount write fDeadlockDetectionMaxPushCount;
    property    DeadlockDetectionTimeLimitMS:TTimeMS  read fDeadlockDetectionTimeLimitMS  write SetDeadlockDetectionTimeLimitMS;
    property    LoggedCount:Integer                   read GetLoggedCount;
    property    OverflowingDeadlockSet:Integer        read GetOverflowingDeadlockSet write SetOverflowingDeadlockSet;
    property    Thread:TDeadlocksThread               read DeadlocksThread;
    property    ThreadState:TDeadlocksThreadStateType read GetThreadState;
    property    TimeMS:TTimeMS                        read GetTimeMS;
  end;

//procedure TestDeadlockSets;
procedure TestDistanceToNearestGoalForEachSquare;
procedure TestReachableGoalsFromEachSquare;

implementation

uses SysUtils,Forms,Misc_,Text_,Pack_,{SokGame_,}Game_,Main_;

{type declarations required before declaring constants and other types}

type
  THashValue               = Int64;
  {caution: after adding more directions to 'TDirection' (e.g., for the Hexoban variant), 'TDeadlockSetFlag' must be updated accordingly}
  TDeadlockSetFlag         = (dsfControllerSet,dsfDiagonalCenterSquares,dsfFreezeSet,
                              dsfHasDisconnectedInnerFloors,dsfHasUnspecifiedInnerFloors,dsfIsADeadlock,
                              dsfPlayerIsInsideSet,dsfPlayerMustBeOutsideSet,dsfTestForFreezingSquare,
                              dsfUp,dsfLeft,dsfDown,dsfRight);
//TDirectionSet            = set of TDirection;

{constants}

const
  FLAG_BOX_REACHABLE_SQUARE= 32;               {flags used for each square}
  FLAG_ILLEGAL_BOX_SQUARE  = 64;
  FLAG_SQUARE_SET          = 128;
  BOARD_SQUARE_FLAGS       = FLAG_BOX_REACHABLE_SQUARE+FLAG_ILLEGAL_BOX_SQUARE+FLAG_SQUARE_SET;

                                               {board, text representation}
  CH_BOX                   = 'b'; CH_BOX_ON_GOAL           = 'B';
  CH_BOX_XSB               = '$'; CH_BOX_ON_GOAL_XSB       = '*';
  CH_GOAL                  = '.';
  CH_FLOOR                 = SPACE;
  CH_FLOOR_NON_BLANK       = '-';
  CH_PLAYER                = 'p'; CH_PLAYER_ON_GOAL        = 'P';
  CH_PLAYER_XSB            = '@'; CH_PLAYER_ON_GOAL_XSB    = '+';
  CH_SQUARE_SET            = '%';
  CH_WALL                  = '#';

  ALL_DIRECTIONS_SET       : TDirectionSet = [Up,Left,Down,Right]; {caution: after adding more directions to 'TDirection' (e.g., for the Hexoban variant), this constant must be updated accordingly}
  DEFAULT_CALCULATE_MINIMUM_LOWER_BOUND_TIME_LIMIT_MS
                           = 20000;                      {max. time (milli seconds) before the pushes lower bound calculation gives up}
  DEFAULT_DEADLOCK_DETECTION_TIME_LIMIT_MS
                           = 100;                        {max. time (milli seconds) before 'IsALegalPosition' stops and declares the position as not being a deadlock}
  DEFAULT_DEADLOCK_DETECTION_MAX_PUSH_COUNT
                           = 10000;                      {maximum number of pushes searched by 'IsALegalPosition' before it stops and declares the position as not being a deadlock}
  DEFAULT_DEADLOCK_SETS_ADJACENT_OPEN_SQUARES_LIMIT      {limits the search for expanded deadlock-sets}
                           = 1;
  DEFAULT_DEADLOCK_SETS_BOX_LIMIT
                           = 10;                         {limits the search for expanded deadlock-sets}
  DEFAULT_MEMORY_BYTE_SIZE = 64*1024*1024;               {64 MB}
  DEFAULT_PUSH_COUNT_LIMIT = MaxInt; //40000000;         {number of pushes to generate before giving up}
  DEFAULT_MOVE_OPTIMAL_SOLUTION_STEP_INCREMENT
                           = 5;
  DIRECTION_TO_DEADLOCK_SET_FLAG
                           : array[TDirection] of TDeadlockSetFlag
                           = (dsfUp,dsfLeft,dsfDown,dsfRight);
//DIRECTION_TO_TEXT        : array[TDirection] of String
//                         = ('up','left','down','right'); {don't localize}
//INFINITY                 = (MaxInt div 2)-1;           {(-INFINITY), (2*INFINITY), and (2*(-INFINITY)) must be legal integer values}
  LOG_FILE_EXT             = '.log';
  MAX_DEADLOCK_DETECTION_TIME_LIMIT_MS
                           = 1000;                       {upper bound (milli seconds) on the timelimit for 'IsALegalPosition'}
  MAX_DEADLOCK_SETS_PER_SQUARE
                           = 63;                         {arbitrary limit, but should at least have room for closed edges}
  MAX_DEADLOCK_SET_CANDIDATE_SEARCH_PUSHES
                           = ONE_MILLION;                {arbitrary limit but the hope is that it provides a reasonable bound on the running time}
  MAX_DEADLOCK_SETS        = 4095;                       {arbitrary limit}
  MAX_DEADLOCK_SETS_SEARCH_TIME_LIMIT_MS
                           = 5 * 60 * 1000;              {arbitrary limit but should provide a reasonable bound on the running time}
  MAX_OVERFLOWING_DEADLOCK_SETS
                           = 10;                         {saved overflowing set-numbers during move-validation}
  MAX_SET_ITEMS            = 255;                        {0..value must fill n*sizeof(machine word) where 'n' is an integer >= 0}
  MAX_SEARCH_DEPTH         = 20;                         {max. search depth, and max. boxes searched by 'IsALegalPosition'}
  MOVE_COUNT_BETWEEN_TIME_CHECKS
                           = 500;                        {avoids calling the operating system's 'GetTime' function for each node during a search}

  TEXT_CREATE_FREEZE_DEADLOCK_SET_FAILED
                           = 'The freeze set in the controller/freeze set pair could not be created because of table overflow, hence, the preceding controller set has been deleted.';
                                                      
{types}

type
  TBoard                   = array[0..MAX_BOARD_SIZE] of Byte;
  TBoardOfBoolean          = array[0..MAX_BOARD_SIZE] of Boolean;
  PBoardOfBoolean          = ^TBoardOfBoolean;
  TBoardOfInt16            = array[0..MAX_BOARD_SIZE] of Int16;
  TBoardOfInteger          = array[0..MAX_BOARD_SIZE] of Integer;
  TBoardAsText             = array[0..MAX_BOARD_HEIGHT] of String[MAX_BOARD_WIDTH];
  TBoardOfTimeStamps       = array[0..MAX_BOARD_SIZE] of SokUtil_.TTimeStamp;
  TBoardSquareSet          = record
    Count                  : Integer;
    Squares                : TBoardOfInt16;
                             end;
  TBoardTimeStamps         = record
    Squares                : TBoardOfTimeStamps;
    TimeStamp              : SokUtil_.TTimeStamp;
                             end;
//TBoxPos                  = array[0..MAX_BOXES] of Integer;                    {change to 'Int16' if the box limit is bigger than 1000; otherwise local temporary arrays may cause stack overflows}
  TBoxPos                  = array[0..MAX_BOXES] of Int16;                      {must be 'Int16' if the box limit is bigger than 1000; otherwise local temporary arrays may cause stack overflows}
  TBoxSet                  = set of 0..MAX_SET_ITEMS;
  TBoxSet2                 = record
    Count                  : Integer;                                   {number of set members}
    Boxes                  : array[0..MAX_BOXES] of Integer;            {unsorted members of the set}
    BoxTimeStamps          : array[0..MAX_BOXES] of Integer;            {members of the set are marked by 'TimeStamp'}
    TimeStamp              : Integer;
                             end;
  TDeadlockSetFlagsSet     = set of TDeadlockSetFlag;
  TDeadlockSets            = record
    AdjacentOpenSquaresLimit                                            {limits the search for expanded deadlock-sets}
                           : Integer;
    BoxLimit               : Integer;                                   {limits the search for expanded deadlock-sets}
    CandidateSearchCount   : Integer;                                   {number of generated pushes for current deadlock-set candidate}
    Calculated             : Boolean;                                   {'True': the data is valid}
    Capacity               : array[0..MAX_DEADLOCK_SETS] of Integer;    {number of boxes that can be pushed to a deadlock-set without causing an overflow}
    CenterSquare           : array[0..MAX_DEADLOCK_SETS] of Int16;
    ControllerAndFreezeSetPairsEnabled                                  {'True': 'IsALegalPush' takes controller/freeze-set deadlock pairs into account; 'False': 'IsALegalPush' ignores them}
                           : Boolean;
    Count                  : Integer;
    Flags                  : array[0..MAX_DEADLOCK_SETS] of TDeadlockSetFlagsSet;
    FloorCount             : array[0..MAX_DEADLOCK_SETS] of Int16;
    HashKey                : array[0..MAX_DEADLOCK_SETS] of THashValue;
    LastRunNo              : Integer;                                   {used for sequentially numbering of the sets}
    LoggedCount            : Integer;                                   {number of sets written to the logfile}
    OverflowingDeadlockSet : Integer;                                   {global result value for 'IsALegalPush' and 'TDeadlocks.IsALegalMove'}
    OverflowingSetCount    : Integer;                                   {global result value for 'IsALegalPush' and 'TDeadlocks.IsALegalMove'}
    OverflowingSets        : array[0..MAX_OVERFLOWING_DEADLOCK_SETS] of Integer; {global result value for 'IsALegalPush' and 'TDeadlock.IsALegalMove'}
    RedundantSetsCount     : Integer;                                   {newcoming sets may make previously found sets redundant}
    RunNo                  : array[0..MAX_DEADLOCK_SETS] of Integer;    {deadlock-sets are sequentially numbered in the log-file, but redundant sets are pruned, hence, 'Index' and 'RunNo' may defer}
    SearchCount            : Integer;
    StartTimeMS            : TTimeMS;                                   {calculation start time}
    SquaresCount           : array[0..MAX_DEADLOCK_SETS] of Integer;    {number of squares for each set}
    SquareSetCount         : array[0..MAX_BOARD_SIZE]    of Integer;    {for each square, the number of deadlock-sets it's a part of}
    SquareSetNumbers       : array[0..MAX_BOARD_SIZE,0..MAX_DEADLOCK_SETS_PER_SQUARE] of Integer; {for each square, the deadlock-sets it's a part of}
    TimeMS                 : TTimeMS;                                   {total calculation time}
                             end;
  TExtendedBoolean         = (ebUnknown,ebFalse,ebTrue);
  TGoalSet                 = set of 0..MAX_SET_ITEMS;

  TBoxGoalMatching         = record
    {Name convention: the domain for a matching is a graph G =(V,E), with V vertices and E edges}
    {contrary to what the structure-name suggests, vertices are squares, not boxes,}
    {except for 'SubSetCount' which really is a box-indexed vector;}
    {the edges in the graph are square-to-goal links, that is, reachable goals for each square}
    Calculated             : Boolean;
    {'SubSetCount'           : number of boxes with reachable goals which is a subset of reachable goals for this box (includes 'self')}
    SubSetCount            : array[0..MAX_BOXES     ] of Byte;     {caution: 'Byte': relies on High(Set)<=255; if this doesn't hold then change to 'Integer'}
    {'EdgeCount'             : number of reachable goals for each square}
    EdgeCount              : array[0..MAX_BOARD_SIZE] of Byte;     {caution: 'Byte': relies on High(Set)<=255; if this doesn't hold then change to 'Integer'}
    {Edges                   : {reachable goals for each square}
    Edges                  : array[0..MAX_BOARD_SIZE] of TGoalSet;
    {EdgeSetNo               : maps identical goal-sets to the same number (used for speeding up comparisons}
    EdgeSetNo              : array[0..MAX_BOARD_SIZE] of Integer;
    TimeMS                 : TTimeMS;
                             end;

  TGoalBoxMatching         = record
    Calculated             : Boolean;
    {'EdgeCount'             : number of boxes that can reach the goal}
    EdgeCount              : array[0..MAX_SET_ITEMS] of Byte;      {caution: 'Byte': relies on High(Set)<=255; if this doesn't hold then change to 'Integer'}
    {'Edges'                 : the boxes that can reach the goal}
    Edges                  : array[0..MAX_SET_ITEMS] of TBoxSet;
                             end;

  {the solver is recursive, but the stack is limited to 1 MB,}
  {hence, large tables must be allocated statically outside the stack}
  TPlayersReachableSquares = record
    MinPlayerPos           : Integer;         {top-left reachable square}
    Squares                : TBoardOfInteger; {reachable squares have the value 'TimeStamp'}
    TimeStamp              : Integer;         {incremented by each call to 'CalculatePlayersReachableSquares'}
                             end;

  TSearchState             = record
    PlayersReachableSquares: TPlayersReachableSquares;
                             end;
  TSearchStatistics        = record   {'statistics' is a bit of a misnomer, the record also contains control-information}
    FenceCount             : Integer;
    MaxPushCount           : Integer; {maximum number of pushes searched by 'IsALegalPosition' before it stops and declares the position as not being a deadlock}
    MaxTimeMS              : TTimeMS; {time limit (milli seconds) for 'IsALegalPosition' before it stops and declares the position as not being a deadlock }
    MoveCountDownToNextTimeCheck      {used for avoiding calling the opearating system's 'GetTime' function for each node during a search}
                           : Integer;
    PushCount              : Integer;
    TimeMS                 : TTimeMS;
    TimeExpired            : Boolean; {'True': 'IsALegalPosition' stopped because it reached the timelimit}
                             end;
  TSquareDirectionArrayOfInteger
                           = array[0..MAX_BOARD_SIZE,TDirection] of Integer;

  TGame                    = record // local game 'object' for the deadlock-set calculation
    Board                  : TBoard;  {current position: 'Board', 'BoxPos', and 'PlayerPos'}
    BoardHeight            : Integer;
    BoardIsLoaded          : Boolean; {'True': the board has been loaded from 'MainForm.Game'}
    BoardSize              : Integer; {(Width+2)*(Height*2): the 2 extra columns and rows are for a wall-filled border}
    BoardTimeStamps        : TBoardTimeStamps; {global timestamp area, e.g., for visited squares/axis during a freeze-test}
    BoardWidth             : Integer;
    BoxGoalMatching        : TBoxGoalMatching; {see also 'GoalBoxMatching}
    BoxCount               : Integer;
    BoxPos                 : TBoxPos; {current position: 'Board', 'BoxPos', and 'PlayerPos'}
    CalculateDeadlockSetsJobNo        {job number for a call to the function 'CalculateDeadlockSets'; if it's different from 'JobNo' then the user abandoned the calculation}
                           : Int64;
    CalculatePushesLowerBoundTimeLimitMS
                           : TTimeMS;
    DeadlockSets           : TDeadlockSets;
    DistanceToNearestBoxStartPosition
                           : TBoardOfInteger;
    DistanceToNearestGoal  : TBoardOfInteger;
    DistanceToNearestTarget  {targets = goals in forward mode, box starting positions in reverse mode}
                           : TBoardOfInteger;
    GoalBoxMatching        : TGoalBoxMatching; {see also 'BoxGoalMatching'}
    GoalCount              : Integer;
    GoalPos                : TBoxPos;
    HashValue              : THashValue;
    //History              : THistory;
    JobNo                  : Int64; {job numbers}
    LogFile                : TextFile;
    LogFileEnabled         : Boolean;
    LogFileName            : String;  {a non-blank name signals that logging really is active, i.e., the file is open and no file-i/o errors have occurred}
    LogSquareGoalDistances : Boolean;
    OriginalBoard          : TBoard;  {original board, i.e., before pre-processing operations like tube-filling}
    OriginalBoxPos         : TBoxPos; {matching 'OriginalBoard'}
    OriginalPlayerPos      : Integer; {matching 'OriginalBoard'}
    OriginalSolution       : String;  {solution from inputfile, not the solution found by this application}
    PlayerPos              : Integer; {current position: 'Board', 'BoxPos', and 'PlayerPos'}
    PlayersDistanceToSquares
                           : TBoardOfInteger;
    ReverseMode            : Boolean;
    SearchStates           : array[0..MAX_SEARCH_DEPTH] of TSearchState;
    SearchStatistics       : TSearchStatistics;
    ShowDeadlockSetsEnabled: Boolean;
    SimpleLowerBound       : Integer;
    StartBoxPos            : TBoxPos;
    StartPlayerPos         : Integer; {player's starting position}
    SquareHashValues       : array[0..MAX_BOARD_SIZE] of THashValue;
    SquareOffsetForward    : array[TDirection] of Integer;
    SquareOffsetLeft       : array[TDirection] of Integer;
    SquareOffsetRight      : array[TDirection] of Integer;
    Title                  : String;
//  TubeFillingMoveCount   : Integer; {player moves}
//  TubeFillingPushCount   : Integer; {box pushes}
    TubeFillingSquareCount : Integer; {number of filled squares}
    XSBNotation            : Boolean;
  end;

  TBoardStateBoxSet        = record
    Count                  : Integer;                              {number of set members}
    Squares                : array[0..MAX_BOXES] of Integer;       {position on the board for each set member; use 'Int16' if the box limit is bigger than 1000; otherwise local temporary arrays may cause stack overflows}
//  Squares                : array[0..MAX_BOXES] of Int16;         {position on the board for each set member; use 'Int16' if the box limit is bigger than 1000; otherwise local temporary arrays may cause stack overflows}
  end;

  TBoardState              = record
    Boxes                  : TBoardStateBoxSet;
    HashValue              : THashValue;
    PlayerPos              : Integer;
  end;

  TSearchHistory           = array[0..MAX_SEARCH_DEPTH] of TBoardState;

{-----------------------------------------------------------------------------}

{Static local variables}

var
  Game                     : TGame;
{
  LogFile                  : TextFile;
  LogFileLineCount         : Integer = 0;
}
{-----------------------------------------------------------------------------}

{Forward Declarations}

procedure BoardToText(var BoardAsText__:TBoardAsText); forward;
procedure CalculateBoxDistanceToAllSquaresForAllDirections(FromSquare__:Integer; Initialize__,ReallyCalculateDistances__:Boolean; var Distance__:TSquareDirectionArrayOfInteger); forward;
procedure CalculateBoxPullOrPushDistances(BoxSquare__,PlayersReachableSquaresIndex__:Integer; PushBox__,UsePlayerPosition__,ContinueCalculation__:Boolean; PlayerCanReachAllFloorsAroundBox__ : PBoardOfBoolean; var Distances__:TSquareDirectionArrayOfInteger); forward;
procedure CalculatePlayerCanReachAllFloorsAroundASingleBoxOnTheBoard( var PlayerCanReachAllFloorsAroundBox__ : TBoardOfBoolean ); forward;
function  CalculatePlayersReachableSquares(Index__,ContinueFromSquare__:Integer):Integer; forward;
function  IsAFloorSquare(Square__:Integer):Boolean; forward;
function  IsAWallSquare(Square__:Integer):Boolean; forward;
procedure SquareToColRow(Square__:Integer; var Col__,Row__:Integer); forward;

{-----------------------------------------------------------------------------}

{General Utilities}

function FileNameWithExtension(const FileName__,Extension__:String):String;
var i:Integer;
begin
  i:=Length(FileName__);
  while (i<>0) and (FileName__[i]<>PERIOD) do Dec(i);
  if   i<>0 then Result:=Copy(FileName__,1,Pred(i))
  else Result:=FileName__;
  Result:=Result+Extension__;
end;

{-----------------------------------------------------------------------------}

{Box Sets}

procedure AddBoxToSet(BoxNo__:Integer; var BoxSet__:TBoxSet2);
begin
  with BoxSet__ do
    if BoxTimeStamps[BoxNo__]<>TimeStamp then begin
       Inc(Count); Boxes[Count]:=BoxNo__; BoxTimeStamps[BoxNo__]:=TimeStamp;
       end;
end;

procedure ClearBoxSet(var BoxSet__:TBoxSet2);
begin
  with BoxSet__ do begin
    Count:=0;
    if TimeStamp<High(TimeStamp) then
       Inc(TimeStamp)
    else begin
       TimeStamp:=1;
       FillChar(BoxTimeStamps,SizeOf(BoxTimeStamps),0);
       end;
    end;
end;

procedure InitializeBoxSet(var BoxSet__:TBoxSet2);
begin
  FillChar(BoxSet__,SizeOf(BoxSet__),0);
end;

procedure RollbackBoxSet(Index__:Integer; var BoxSet__:TBoxSet2);
var i:Integer;
begin
  with BoxSet__ do begin
    for i:=Succ(Index__) to Count do BoxTimeStamps[i]:=0;
    Count:=Index__;
    end;
end;

{-----------------------------------------------------------------------------}

{Utilities}

function ColRowToSquare(Col__,Row__:Integer):Integer;
begin
  Result:=Row__ * (Game.BoardWidth+2) + Col__;
end;

function DxDyToDirection(Dx__,Dy__:Integer):TDirection;
begin {precondition: either 'Dx__' or 'Dy__' is 0}
  if        Dx__=0 then
            if   Dy__<=0 then Result:=Up
            else              Result:=Down
  else if   Dx__<0       then Result:=Left
            else              Result:=Right;
end;

procedure DirectionToDxDy(Direction__:TDirection; var Dx__,Dy__:Integer);
begin
  Dx__:=0; Dy__:=0;
  case Direction__ of
    Up   : Dy__:=-1;
    Left : Dx__:=-1;
    Down : Dy__:= 1;
    Right: Dx__:= 1;
    else    Msg('DirectionToDxDy',InternalErrorText,MB_OK);
  end; {case}
end;

function GoalNoAtSquare(SquareNo__:Integer):Integer;
var GoalNo:Integer;
begin // returns the number of the goal at the square 'SquareNo__', if any
  Result:=0;
  for GoalNo:=1 to Game.GoalCount do
      if Game.GoalPos[GoalNo]=SquareNo__ then begin
         Result:=GoalNo; break;
         end;
end;

function GoalSetToText(GoalSet__:TGoalSet):String;
var i,Count:Integer;
begin
  Result:=LEFT_BRACKET; Count:=0;
  for i:=0 to MAX_SET_ITEMS do
      if i in GoalSet__ then begin
         if Count<>0 then Result:=Result+COMMA;
         Inc(Count); Result:=Result+IntToStr(i);
         end;
  Result:=Result+RIGHT_BRACKET;
end;

function IsABlackSquareOnAChessBoard(Square__:Integer):Boolean;
var Col,Row:Integer;
begin
  SquareToColRow(Square__,Col,Row);
  Result:=(Odd(Col) and Odd(Row)) or ((not Odd(Col)) and (not Odd(Row)));
end;

function IsABoxSquare(Square__:Integer):Boolean;
begin
  Result:=(Game.Board[Square__] and BOX)<>0;
end;

function IsACornerSquare(SquareNo__:Integer):Boolean;
begin
  with Game do
    Result:=IsAFloorSquare(SquareNo__)
            and
            (IsAWallSquare(SquareNo__+SquareOffsetForward[Left ])
             or // a neigbouring wall, horizontally?
             IsAWallSquare(SquareNo__+SquareOffsetForward[Right])
            )
            and
            (IsAWallSquare(SquareNo__+SquareOffsetForward[Up   ])
             or // a neigbouring wall, vertically?
             IsAWallSquare(SquareNo__+SquareOffsetForward[Down ])
            );
end;

function IsADuplicatePosition(Depth__,PlayerPos__:Integer; const History__:TSearchHistory):Boolean;
var i,j,k:Integer;
begin
  Result:=False;
  for i:=Pred(Depth__) downto 1 do {search through the stored positions (backwards since the latest ones probably have the best chances for a match)}
      if not Result then begin
         Result:=(PlayerPos__   =History__[i].PlayerPos) and
                 (Game.HashValue=History__[i].HashValue);
         {a hash-value is not 100% unique, hence, the exact game state must match}
         for j:=1 to Game.BoxCount do begin {try to match each box}
             if Result then
                if (Game.BoxPos[j]<>History__[i].Boxes.Squares[j]) and {'<>': no simple match}
                   (Game.BoxPos[j]<>0) then begin                      {'0' : '0' means that the box has been removed from the board}
                   Result:=False;
                   for k:=1 to Game.BoxCount do {search for another box at the same position as Box[j]}
                       if   not Result then
                            Result:=Game.BoxPos[j]=History__[i].Boxes.Squares[k]
                       else break; {done, i.e., there is another box at the same position as Box[j]}
                   end
                else {Box[j] at the same position, or Box[j] has been removed from the board}
             else break; {fail, i.e., current position isn't identical to the position stored at depth = 'i'}
             end;
         end
      else break; {done, i.e., found a duplicate position}
end;

function IsAFloorSquare(Square__:Integer):Boolean;
begin
  Result:=(Game.Board[Square__] and FLOOR)<>0;
end;

function IsAFreezingMove ( FromSquareNo__ , ToSquareNo__ , PlayerSquareNo__ : Integer;
                           ABoxFreezingOnAGoalSquareCountsAsAFreezingMove__ ,
                           CheckForClosedDiagonals__ : Boolean ) : Boolean;
{returns 'True' if putting a box on 'ToSquareNo__' creates a deadlocked frozen
 position (optionally moving the box from 'FromSquareNo__');
 if the function is used for testing if the box at 'ToSquareNo__' is
 a frozen box in the starting position, then there is no preceding push; in that
 case, pass a wall square as player position in 'PlayerSquareNo__';
}
var OriginalFromSquareValue : Integer; ABoxIsBlockedOnANonGoalSquare : Boolean;

  function  BoxIsBlockedAlongOneAxis( SquareNo__ : Integer; Direction__ : TDirection;
                                      var ABoxIsBlockedOnANonGoalSquare__ : Boolean):Boolean;
  var Neighbor1, Neighbor2, Neighbor1Position , Neighbor2Position : Integer;
  begin
    if   Direction__              = Low  ( Direction__ ) then                   {flip horizontal/vertical direction}
         Direction__             := Succ ( Low ( Direction__ ) )                {caution: 'Succ(Low...'): assumes 4 directions only}
    else Direction__             := Low  ( Direction__ );

    if   ( Direction__            = Low ( Direction__ ) )
         and
         (Game.BoardTimeStamps.Squares [ SquareNo__ ] >= Game.BoardTimeStamps.TimeStamp) then {'True': use the already calculated value}

         Result                  := Game.BoardTimeStamps.Squares [ SquareNo__ ] > Game.BoardTimeStamps.TimeStamp {relies on Ord ( False , True ) = (0 , 1)}

    else begin
           Neighbor1Position     := SquareNo__ - Game.SquareOffsetForward[ Direction__ ];
           Neighbor1             := Game.Board [ Neighbor1Position ];

           Neighbor2Position     := SquareNo__ + Game.SquareOffSetForward[ Direction__ ];
           Neighbor2             := Game.Board [ Neighbor2Position ];

           Inc ( Game.Board [ SquareNo__ ] , WALL);                             {temporarily change this square to a wall}

           Result := ((  Neighbor1 and WALL           ) <> 0 )                  {is there a wall on any of the neighbor squares?}
                     or
                     ((  Neighbor2 and WALL           ) <> 0 )
                     or
                     ((( Neighbor1 and FLAG_ILLEGAL_BOX_SQUARE ) <> 0 )         {are both neighbors illegal squares?}
                      and
                      (( Neighbor2 and FLAG_ILLEGAL_BOX_SQUARE ) <> 0 )
                     );

           if (not ( Result and ABoxIsBlockedOnANonGoalSquare__ ))              {it's not enough to know whether a box is blocked; it's also necessary to know whether a box is blocked on a non-goal square}
              and
              (( Neighbor1 and ( WALL + BOX ) ) = BOX )                         {test if neighbor1 is a blocked box}
              and
              BoxIsBlockedAlongOneAxis( Neighbor1Position , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
              then Result := True;

           if (not ( Result and ABoxIsBlockedOnANonGoalSquare__ ))              {it's not enough to know whether a box is blocked; it's also necessary to know whether a box is blocked on a non-goal square}
              and
              (( Neighbor2 and ( WALL + BOX ) ) = BOX )                         {test if neighbor2 is a blocked box}
              and
              BoxIsBlockedAlongOneAxis( Neighbor2Position , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
              then Result:=True;

           Dec ( Game.Board [ SquareNo__ ] , WALL );                            {remove the wall again}
         end;

    if  Result and                                                              {if this box is blocked}
        ( ( Game.Board [ SquareNo__ ] and GOAL ) = 0 ) then                     {and it's not a goal-square}
        ABoxIsBlockedOnANonGoalSquare__ := True;                                {then set the flag}

    if  Direction__ = Low ( Direction__ ) then with Game.BoardTimeStamps do     {reduce the exponential growth by storing the results for one axis}
        Squares [ SquareNo__ ] := TimeStamp + Cardinal ( Ord ( Result ) );      {relies on Ord ( False , True ) = (0 , 1)}

  end; {BoxIsBlockedAlongOneAxis}

  function BoxIsBlockedOnClosedDiagonal( BoxSquare__ , PlayerSquare__ : Integer ):Boolean; {caution: assumes 4 directions only}
  var OriginalBoxSquareValue , OriginalPlayerSquareValue : Integer;

    function Check( DiagonalSquare__ , BoxSquare__ : Integer) : Boolean;
    {
     preconditions: 'diagonal square' is neighbor to the pushed box and on the
     same row as the pushed box;
    }
    const DONE = BOX + PLAYER; {must be a non-zero impossible board square value with the 'PLAYER' bit}
          TRY_NEXT_DIAGONAL = WALL + PLAYER; {must be a non-zero impossible board square value with the 'PLAYER' bit}
          TRY_SQUARES_ON_DIAGONAL_IN_OPPOSITE_DIRECTION_FROM_START_POINT = WALL + BOX + PLAYER; {must be a non-zero impossible board square value with the 'PLAYER' bit}
    var   DiagonalSquare , DiagonalSquareValue , DX , DY ,                {notice the difference between the currently investigated diagonal square 'DiagonalSquare' and the parameter 'DiagonalSquare__', i.e., the diagonal starting point}
          GoalOrWallSquareOnDiagonal ,
          NextSquareOnSameRowAsDiagonalSquareValue ,                      {'next'    : according to the horizontal direction of the diagonal, i.e., left or right}
          PreviousSquareOnSameRowAsDiagonalSquareValue : Integer;         {'previous': according to the horizontal direction of the diagonal, i.e., left or right}
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

      StartingRowIsOneEndOfTheDiagonal           := ( Game.Board [ DiagonalSquare__ ] and ( BOX + WALL ) ) <> 0; {'True': the starting diagonal square is itself an end point of the diagonal}
      ColDirection                               := Left; {start investigating the left-up diagonal}
      RowDirection                               := Up;

      repeat {until all diagonals have been investigated, or until a closed diagonal deadlock has been found}
        if   ColDirection                         = Left then {'True': investigate the first (part of the) diagonal going left}
             DX                                  := Game.SquareOffsetForward [ Left  ]  {investigate the (part of the) diagonal going left}
        else DX                                  := Game.SquareOffsetForward [ Right ]; {investigate the (part of the) diagonal going right}
        if   RowDirection                         = Up then {'True': investigate the first (part of the) diagonal going up}
             DY                                  := Game.SquareOffsetForward [ Up    ]  {investigate the (part of the) diagonal going up}
        else DY                                  := Game.SquareOffsetForward [ Down  ]; {investigate the (part of the) diagonal going down}

        GoalOrWallSquareOnDiagonal               := 0; {'0': no goals or walls found on the diagonal yet}

        repeat {until the current diagonal has been investigated both upwards and downwards, if that's necessary (diagonals where the start square itself is blocked by a box or a wall only needs investigation in one direction)}
          if   StartingRowIsOneEndOfTheDiagonal then begin {'True': the start point is itself a blocked diagonal square; skip to the next diagonal square in the row above/below the start square}
               {find the correct column of the diagonal square in the row
                above/below the start square; the correct column is above/below
                one of the two boxes or walls on the starting row;
               }
               if   DX > 0 then {'True': investigating 'right-up' or 'right-down' diagonal; the diagonal starts at the leftmost square}
                    if   DiagonalSquare__        < BoxSquare__ then
                         DiagonalSquare          := DiagonalSquare__
                    else DiagonalSquare          := BoxSquare__
               else {investigating 'left-up' or 'left-down' diagonal; the diagonal starts at the rightmost square}
                    if   DiagonalSquare__         > BoxSquare__ then
                         DiagonalSquare          := DiagonalSquare__
                    else DiagonalSquare          := BoxSquare__;

               HasGoalsAndWallsSequence          := ( ( Game.Board [ DiagonalSquare      ] and ( GOAL + WALL ) ) <> 0 )  {'True': the starting diagonal square is a goal square or a wall square}
                                                    and
                                                    ( ( Game.Board [ DiagonalSquare + DX ] and ( GOAL + WALL ) ) <> 0 ); {'True': the next square in the row (according to the horizontal direction of the diagonal) is a goal square or a wall square}

               Inc ( DiagonalSquare , DX + DY ); {move to the row above/below the diagonal starting point}
               end
          else
            if GoalOrWallSquareOnDiagonal         = 0 then begin {'True': no goals or walls found on the diagonal during the first left-going search along the diagonal}
               DiagonalSquare                    := DiagonalSquare__; {notice the difference between the currently investigated diagonal square 'DiagonalSquare' and the parameter 'DiagonalSquare__', i.e., the diagonal starting point}
               HasGoalsAndWallsSequence          := False;
               end
            else begin
               {the first search along the diagonal found a goal square or a
                wall square;
                to test correctly for goal square sequences, this second search
                along the diagonal in the opposite direction must start from
                this square;
               }
               DiagonalSquare                    := GoalOrWallSquareOnDiagonal;

               {if the next square in this row (according to the horizontal
                direction of the current search direction along the diagonal)
                also is a goal or a wall, then this second search along the
                diagonal begins with an 'all goals and walls' sequence;
               }
               HasGoalsAndWallsSequence          := ( Game.Board [ DiagonalSquare + DX ] and ( GOAL + WALL ) ) <> 0;

               Inc ( DiagonalSquare , DX + DY ); {move down to the row below the goal or wall}
               end;

          DiagonalSquareValue                    := Game.Board [ DiagonalSquare ]; {get the board square value for the diagonal square}

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

            PreviousSquareOnSameRowAsDiagonalSquareValue := Game.Board [ DiagonalSquare - DX ]; {'previous': according to the horizontal direction of the diagonal, i.e., left or right}

            if HasGoalsAndWallsSequence then {'True': there is an 'all goals or walls' sequence of most recently visited squares around the diagonal}
               {there is an 'all goals and walls' sequence of most recently
                visited squares on, and around, the diagonal; check if the
                sequence can be extended;

                an example with the diagonal going 'right-up':

                ?!    <---- '!' is the current  diagonal square
                .#    <---- '.' is the previous diagonal square

                the previous diagonal square happened to be a goal square,
                thereby starting a new 'all goals and walls' sequence;

                the next square (in the diagonal direction) after the previous
                diagonal square was a goal or a wall (a wall in this example),
                so it extended the 'all goals and walls' sequence;

                if the '?' square is a goal or a wall, and the current diagonal
                square '!' also is a goal or a wall, then some of the boxes
                around the diagonal can be pushed inwards to goal squares and
                open up the diagonal without creating a deadlock;
               }
               HasGoalsAndWallsSequence          := ( PreviousSquareOnSameRowAsDiagonalSquareValue and ( GOAL + WALL ) ) <> 0;

            if  ( ( DiagonalSquareValue and ( BOX + WALL ) ) = 0 )  then begin {'True': the diagonal square is an empty floor square}
                NextSquareOnSameRowAsDiagonalSquareValue := Game.Board [ DiagonalSquare + DX ];   {get the value of the next square (according to the horizontal direction of the diagonal) in the same row as the current diagonal square}
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
                        if GoalOrWallSquareOnDiagonal     = 0 then {'True': this is the first found goal on the diagonal during the search in this direction}
                           GoalOrWallSquareOnDiagonal    := DiagonalSquare; {remember the first found goal during the search in this direction}
                        end;

                     if HasGoalsAndWallsSequence then {'True': there is an 'all goals and walls ' sequence of squares on, and around, the most recently visited part of the diagonal}
                        HasGoalsAndWallsSequence := ( ( NextSquareOnSameRowAsDiagonalSquareValue and ( GOAL + WALL ) ) <> 0 ); {'True': the next square is a goal or a wall, hence it extends the 'all goals and walls' sequence of squares}

                     if ( ( NextSquareOnSameRowAsDiagonalSquareValue and WALL ) = 0 )
                        or {'True': the current diagonal square doesn't have 2 neigboring walls which block the access to the next diagonal square in the current direction}
                        ( ( Game.Board [ DiagonalSquare + DY ]       and WALL ) = 0 )
                        or
                        HasGoalsAndWallsSequence {'True': there is an 'all goals and walls' sequence of squares, so it's necessary to advance to the next diagonal square and continue the investigation}
                        then begin
                        Inc ( DiagonalSquare , DX + DY ); {advance to the next diagonal square}
                        DiagonalSquareValue      := Game.Board [ DiagonalSquare ];
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
                        GoalOrWallSquareOnDiagonal := DiagonalSquare + DX + DY;
                        end;
                     end
                else DiagonalSquareValue         := TRY_NEXT_DIAGONAL; {try the next diagonal, if any; there is no closed diagonal, or it's not a deadlock because there is a sequence of goal squares making it possible to push boxes inwards without creating a deadlock}
                end
            else {the diagonal is blocked by a box or a wall at [Col,Row]}
                if ( ( PreviousSquareOnSameRowAsDiagonalSquareValue and ( BOX + WALL ) ) = 0 ) {'True': there isn't a box or a wall at the square 'before' the diagonal square on this row, so the diagonal isn't closed by 3 boxes and walls forming an 'L' in this end}
                   or
                   ( HasGoalsAndWallsSequence and ( ( DiagonalSquareValue and ( WALL + GOAL ) ) <> 0 ) ) then {'True': the current diagonal square closes a sequence of 'all goals and walls' sequence, hence, the diagonal isn't a deadlock}
                   DiagonalSquareValue           := TRY_NEXT_DIAGONAL     {try next diagonal, if any}
                else {there are 2 neighboring boxes or walls in the row, closing the diagonal in this end, and pushing boxes inwards along the examined part of the diagonal creates a deadlock}
                   if ( not StartingRowIsOneEndOfTheDiagonal )            {'True': the box or wall at [Col,Row] is not the other end point of a diagonal which started with a blocked end}
                      and
                      ( DY = Game.SquareOffsetForward [ Up ] ) then begin {'True': the first part of the diagonal (going upwards) has just been investigated}
                      if (   ( DiagonalSquareValue and WALL ) <> 0 )      {'True': the diagonal ends with a wall}
                         or
                         ( ( ( DiagonalSquareValue and GOAL ) <> 0 )      {'True': the diagonal ends with a goal square}
                           and
                           ( GoalOrWallSquareOnDiagonal        = 0 )      {'True': this is the first found goal on the diagonal}
                         )
                         then
                         GoalOrWallSquareOnDiagonal := DiagonalSquare;    {remember the starting point for the search along the diagonal in the opposite direction}

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
    OriginalBoxSquareValue                       := Game.Board [ BoxSquare__   ];
    Game.Board [ BoxSquare__    ]                := Game.Board [ BoxSquare__   ] or BOX; {put a box at the box square if there isn't one already}

    {the player cannot be inside a closed diagonal after a push, hence, it's
     always all right to remove the player from the board before testing if
     there is a deadlocked closed diagonal after a push;
     if the function is used for testing if the starting position is a deadlock,
     then there is no preceding push; in that case, pass a wall square as player
     position in 'PlayerSquare__';
    }
    OriginalPlayerSquareValue                    := Game.Board [ PlayerSquare__ ];
    Game.Board [ PlayerSquare__ ]                := Game.Board [ PlayerSquare__ ] and ( not PLAYER ); {remove the player from the board}

    Result                                       := Check ( BoxSquare__ + Game.SquareOffsetForward [ Left   ] , BoxSquare__ )  {check the box square and its left  neighbor}
                                                    or
                                                    Check ( BoxSquare__ + Game.SquareOffsetForward [ Right  ] , BoxSquare__ ); {check the box square and its right neighbor}

    Game.Board [ PlayerSquare__ ]                := OriginalPlayerSquareValue;  {restore the player square value}
    Game.Board [ BoxSquare__    ]                := OriginalBoxSquareValue;     {restore the box square value; in the game there may not be a box at the square, but it was added at the beginning of this function}
  end; {IsAFreezingMove.BoxIsBlockedOnClosedDiagonal}

begin {IsAFreezingMove}
  with Game.BoardTimeStamps do
    if TimeStamp < High ( TimeStamp ) - 5 then
       Inc ( TimeStamp , 2 )
    else begin
       FillChar ( Squares , SizeOf ( Squares ) , 0 ); TimeStamp := 2;
       end;

  ABoxIsBlockedOnANonGoalSquare := False;
  OriginalFromSquareValue := Game.Board [ FromSquareNo__ ];
  Game.Board [ FromSquareNo__ ] := Game.Board [ FromSquareNo__ ] and (not BOX);   {remove box, if any ('FromSquareNo__' is optional), from its current position}

  Result := (( Game.Board [ ToSquareNo__ ] and ( WALL + FLAG_ILLEGAL_BOX_SQUARE ) ) <> 0 )   {a wall is considered a deadlocked square}
            or
            (( Game.Board [ ToSquareNo__ ] and   FLAG_BOX_REACHABLE_SQUARE        ) =  0 )   {inlined '(not IsAReachableBoxSquare( ToSquareNo__ ))'}
            or
            (( not Game.ReverseMode )
             and
             ((BoxIsBlockedAlongOneAxis ( ToSquareNo__ , Low(        TDirection )   , ABoxIsBlockedOnANonGoalSquare )
               and
               BoxIsBlockedAlongOneAxis ( ToSquareNo__ , Succ( Low ( TDirection ) ) , ABoxIsBlockedOnANonGoalSquare ) {caution: 'Succ(Low...'): assumes 4 directions only}
               and
               (ABoxIsBlockedOnANonGoalSquare or ABoxFreezingOnAGoalSquareCountsAsAFreezingMove__)
              )
              or
              (CheckForClosedDiagonals__
               and
               BoxIsBlockedOnClosedDiagonal ( ToSquareNo__ , PlayerSquareNo__ )
              )
             )
            );

  Game.Board [FromSquareNo__] := OriginalFromSquareValue;                       {put box, if any, back on the board}
end; {IsAFreezingMove}

function IsAGoalBoxEdgeSubSet(SubSetGoalNo__,GoalNo__:Integer):Boolean;
begin
  with Game.GoalBoxMatching do
    Result:=(EdgeCount[SubSetGoalNo__]<>0) {empty sets don't count}
            and
            ((Edges[SubSetGoalNo__] * Edges[GoalNo__]) = Edges[SubSetGoalNo__]);
end; {IsAGoalBoxEdgeSubSet}

function IsAGoalSquare(Square__:Integer):Boolean;
begin
  Result:=(Game.Board[Square__] and GOAL)<>0;
end;

function IsALegalBoxSquare(Square__:Integer):Boolean;
begin
  Result:=(Game.Board[Square__] and (WALL+FLAG_ILLEGAL_BOX_SQUARE))=0;
end;

function IsALegalAndReachableBoxSquare(Square__:Integer):Boolean;
begin
  Result:=(Game.Board[Square__] and (WALL+FLAG_ILLEGAL_BOX_SQUARE+FLAG_BOX_REACHABLE_SQUARE+FLOOR))=FLAG_BOX_REACHABLE_SQUARE+FLOOR;
end;

function IsAPlayerSquare(Square__:Integer):Boolean;
begin
  Result:=(Game.Board[Square__] and PLAYER)<>0;
end;

function IsAReachableBoxSquare(Square__:Integer):Boolean;
begin
  Result:=(Game.Board[Square__] and FLAG_BOX_REACHABLE_SQUARE)<>0;
end;

function IsASquareGoalEdgeSubSet(SubSetSquareNo__,SquareNo__:Integer):Boolean;
begin
  with Game.BoxGoalMatching do
    Result:=(EdgeCount[SubSetSquareNo__]<>0) {empty sets don't count}
            and
            ((EdgeSetNo[SubSetSquareNo__]=EdgeSetNo[SquareNo__]) {'=': quick test for identical sets}
             or
             ((Edges[SubSetSquareNo__] * Edges[SquareNo__]) = Edges[SubSetSquareNo__])
            );
end; {IsASquareGoalEdgeSubSet}

function IsAWallSquare(Square__:Integer):Boolean;
begin
  Result:=(Game.Board[Square__] and WALL)<>0;
end;

function IsSquareAMemberOfDeadlockSet(SquareNo__,DeadlockSetNo__:Integer):Boolean;
var Index:Integer;
begin
  Result:=True;
  with Game.DeadlockSets do
    for Index:=1 to SquareSetCount[SquareNo__] do
        if SquareSetNumbers[SquareNo__,Index]=DeadlockSetNo__ then exit; {quick-and-dirty exit when the square number has been found}
  Result:=False;
end;

function HasALegalAndReachableBoxSquareNeighborLeftOrRight(Square__:Integer; Direction__:TDirection):Boolean;
begin
  Result:=IsALegalAndReachableBoxSquare(Square__+Game.SquareOffsetLeft [Direction__])
          or
          IsALegalAndReachableBoxSquare(Square__+Game.SquareOffsetRight[Direction__])
end;

function SquareToChar(Square__:Integer):Char;
//var Col,Row:Integer;
begin
  case Game.Board[Square__] and (PLAYER+BOX+GOAL+WALL) of
    PLAYER     : if   Game.XSBNotation then
                      Result:=CH_PLAYER_XSB
                 else Result:=CH_PLAYER;
    PLAYER+GOAL: if   Game.XSBNotation then
                      Result:=CH_PLAYER_ON_GOAL_XSB
                 else Result:=CH_PLAYER_ON_GOAL;
    BOX        : if   Game.XSBNotation then
                      Result:=CH_BOX_XSB
                 else Result:=CH_BOX;
    BOX+GOAL   : if   Game.XSBNotation then
                      Result:=CH_BOX_ON_GOAL_XSB
                 else Result:=CH_BOX_ON_GOAL;
    GOAL       : Result:=CH_GOAL;
    WALL       : Result:=CH_WALL;
//  else         Result:=UNDERSCORE;
    else         Result:=CH_FLOOR;
  end;
  if Game.Board[Square__]=FLAG_SQUARE_SET then Result:=CH_SQUARE_SET;
  if ((Game.Board[Square__] and WALL)<>0) and (Game.Board[Square__]<>WALL) then
     if not Game.DeadlockSets.ControllerAndFreezeSetPairsEnabled then {kludge: during calculation of deadlock sets, there are sometimes boxes on walls, e.g., during creation of controller/freeze-set deadlock pairs}
        Result:=CH_WALL
     else begin
        //SquareToColRow(Square__,Col,Row);
        //Msg(TEXT_INTERNAL_ERROR+': "SquareToChar"'+SPACE+IntToStr(Square__)+' = ['+IntToStr(Col)+','+IntToStr(Row)+']',TEXT_APPLICATION_TITLE);
        //Msg(TEXT_INTERNAL_ERROR+': "SquareToChar"',TEXT_APPLICATION_TITLE);
        Result:='?'; {error}
        end;
end;

procedure SquareToColRow(Square__:Integer; var Col__,Row__:Integer);
begin
  Row__:=Square__   div   (Game.BoardWidth+2);
  Col__:=Square__ - Row__*(Game.BoardWidth+2);
end;

{-----------------------------------------------------------------------------}

{Log File}

function CloseLogFile:Boolean;
begin {$I-}
  if   Game.LogFileName<>'' then begin
       CloseFile(Game.LogFile); Result:=IOResult=0;
       end
  else Result:=True;
  if   Result then Game.LogFileName:=''
  else Msg('Close Log File: '+FileIOErrorText+': '+Game.LogFileName,TEXT_APPLICATION_TITLE,MB_OK);
end; {$I+}

function CreateLogFile(const FileName__:String):Boolean;
begin {$I-}
  Result:=False;
  if CloseLogFile and Game.LogFileEnabled then begin
     Game.LogFileName:=FileNameWithExtension(FileName__,LOG_FILE_EXT);
     Assign(Game.LogFile,Game.LogFileName); Rewrite(Game.LogFile);
     Result:=IOResult=0;
     if not Result then begin
        Msg('Create Log File: '+FileIOErrorText+': '+Game.LogFileName,TEXT_APPLICATION_TITLE,MB_OK);
        Game.LogFileName:='';
        end;
     end;
end; {$I+}

function WriteBoardToLogFile(const Caption__:String):Boolean;
var i:Integer; BoardAsText:TBoardAsText;
begin {$I-}
  Result:=Game.LogFileName<>'';
  if Result then begin
     BoardToText(BoardAsText);
     Writeln(Game.LogFile);
     if Caption__<>'' then Writeln(Game.LogFile,Caption__);
     for i:=1 to Game.BoardHeight do Writeln(Game.LogFile,BoardAsText[i]);
     Result:=IOResult=0;
     if Result then Inc(Game.DeadlockSets.LoggedCount);
     end;
end; {$I+}

function WritelnToLogFile(const Str__:String):Boolean;
begin {$I-}
  if Game.LogFileName<>'' then begin
     Writeln(Game.LogFile,Str__);
     Result:=IOResult=0;
     end
  else Result:=False;
end; {$I+}


function WriteToLogFile(const Str__:String):Boolean;
begin {$I-}
  if Game.LogFileName<>'' then begin
     Write(Game.LogFile,Str__);
     Result:=IOResult=0;
     end
  else Result:=False;
end; {$I+}

{-----------------------------------------------------------------------------}

{Moves}

procedure DoPush(BoxNo__:Integer; Direction__:TDirection);
var i,FromSquare,ToSquare:Integer;
begin
  if BoxNo__<>0 then with Game do begin {only pushes are handled}
     FromSquare:=BoxPos[BoxNo__];
     ToSquare  :=FromSquare+SquareOffsetForward[Direction__];
     Dec(Board[FromSquare],BOX);
     Inc(Board[ToSquare  ],BOX);
     BoxPos[BoxNo__]:=ToSquare;
     Dec(Board[PlayerPos],PLAYER);
     PlayerPos:=FromSquare;
     Inc(Board[PlayerPos],PLAYER);
     HashValue:=(HashValue xor SquareHashValues[FromSquare])
                xor
                SquareHashValues[ToSquare];
     Inc(SimpleLowerBound,DistanceToNearestTarget[ToSquare]-DistanceToNearestTarget[FromSquare]);

     with DeadlockSets do begin
       for i:=1 to SquareSetCount[FromSquare] do Inc(Capacity[SquareSetNumbers[FromSquare,i]]); {leaving  these deadlock-sets}
       for i:=1 to SquareSetCount[ToSquare  ] do Dec(Capacity[SquareSetNumbers[ToSquare  ,i]]); {entering these deadlock-sets}
       end;
     end;
end;

function IsALegalPush(BoxNo__:Integer; Direction__:TDirection):Boolean;
var i,j,SquareSetNo,FromSquare,ToSquare:Integer;
begin {precondition: simple constraints, e.g., the move isn't blocked by a wall, have already been checked}
  with Game do with DeadlockSets do begin
    Result:=True;
    FromSquare:=BoxPos[BoxNo__];
    ToSquare  :=FromSquare+SquareOffsetForward[Direction__];
    OverflowingSetCount:=0;

    {check for deadlock-set overflows, i.e., pattern matching}
    for i:=1 to SquareSetCount[ToSquare] do begin {for each deadlock-set that 'ToSquare' is a member of}
        SquareSetNo:=SquareSetNumbers[ToSquare,i]; {get current set number}
        if (Capacity[SquareSetNo]<=0) {can a push cause an overflow?}
           {and (not (dsfControllerSet in Flags[SquareSetNo]))} {this test is unnecessary because controller-sets "cheat" by having the "dsfPlayerMustBeOutsideSet" flag}
           then begin
           Result:=False;
           for j:=1 to SquareSetCount[FromSquare] do
               if SquareSetNumbers[FromSquare,j]=SquareSetNo then begin
                  {if 'FromSquare' is in the deadlock-set, the push doesn't cause an overflow}
                  Result:=True; break;
                  end;
           if not Result then
              if not (dsfPlayerMustBeOutsideSet in Flags[SquareSetNo]) then begin
                 {
                 if i<>1 then begin                                  // self-organizing list: doesn't seem to be worthwhile here;
                    j:=SquareSetNumbers[ToSquare,Pred(i)];           // shift current deadlock-set one slot left towards the front of the list,
                    SquareSetNumbers[ToSquare,Pred(i)]:=SquareSetNo; // i.e., the most popular sets are tested first
                    SquareSetNumbers[ToSquare,i]:=j;
                    end;
                 }
                 OverflowingSets[1]:=SquareSetNo;
                 {when the function returns False', the overflowing set is stored}
                 {in 'OverflowingSets[1]' but note that 'OverflowingSetCount' = 0;}
                 {the count is only used for overflowing sets requiring further}
                 {analysis, i.e., after performing the move}
                 exit; {quick-and-dirty exit; pushing the box to 'ToSquare' would overflow the deadlock-set}
                 end
              else begin {the set overflows but maybe the player must be outside the set, or maybe this set is a freeze-set in a controller/freeze-set pair}
                 if DIRECTION_TO_DEADLOCK_SET_FLAG[Direction__] in Flags[SquareSetNo] then begin
                    {when the box was pushed in this direction, then the player is outside the set,}
                    {hence, this is a deadlock}
                    OverflowingSets[1]:=SquareSetNo;
                    {when the function returns False', the overflowing set is stored}
                    {in 'OverflowingSets[1]' but note that 'OverflowingSetCount' = 0;}
                    {the count is only used for overflowing sets requiring further}
                    {analysis, i.e., after performing the move}
                    exit; {quick-and-dirty exit; pushing the box to 'ToSquare' would overflow the deadlock-set}
                    end
                 else if not (dsfControllerSet in Flags[Pred(SquareSetNo)]) then begin {'True': this is a normal deadlock-set or a controller-set; an overflowing controller-set has no interest and only ends up here to avoid an extra 'if' test}
                         {save the overflowing set numbers for later analysis}
                         if OverflowingSetCount<High(OverflowingSets) then begin
                            Inc(OverflowingSetCount);
                            OverflowingSets[OverflowingSetCount]:=SquareSetNo;
                            end;
                         Result:=True;
                         end
                      else {this deadlock-set is the freeze-set in a controller/freeze-set pair; the squares in the controller-set must contain enough boxes before the squares in the freeze-set freeze}
                         if   ControllerAndFreezeSetPairsEnabled
                              and
                              (( Capacity[Pred(SquareSetNo)]>0) {'True': the squares in the controller-set don't contain the required number of boxes, hence, it's a deadlock}
                               or
                               ((Capacity[Pred(SquareSetNo)]=0)
                                and {if the box leaves a square belonging to the controller-set and moves to a square belonging to the freeze-set, then the controller-set must still contain enough boxes}
                                IsSquareAMemberOfDeadlockSet(FromSquare,Pred(SquareSetNo))
                               )
                              )
                              and
                              ((not (dsfTestForFreezingSquare in Flags[SquareSetNo])) {'True': it's known from the precalculation, that when the squares in the freeze set have been filled, then the boxes at these squares have frozen}
                               or
                               IsAFreezingMove(FromSquare,ToSquare,PlayerPos,True,False) {'True': moving a box to the single square in the freeze set causes the box to freeze}
                              ) then begin
                              OverflowingSets[1]:=SquareSetNo;
                              {when the function returns False', the overflowing set is stored}
                              {in 'OverflowingSets[1]' but note that 'OverflowingSetCount' = 0;}
                              {the count is only used for overflowing sets requiring further}
                              {analysis, i.e., after performing the move}
                              exit; {quick-and-dirty exit; pushing the box to 'ToSquare' would overflow the deadlock-set}
                              end
                         else Result:=True;
                 end;
           end;
        end;

    if Result then
       {check if the new box position is a simple deadlock;
        'closed diagonals' deadlocks are not tested because maybe it's a bit too
        time-consuming to do that for every push investigated during a search;

        the 'TSokoGame' engine in 'SokGame_.pas' performs the 'closed deadlocks'
        freeze-test, so when the user clicks a box, the "show legal moves"
        feature filters these illegal pushes out;
       }
       Result:=not IsAFreezingMove(FromSquare,ToSquare,PlayerPos,False,False);
    end;
end;

procedure MovePlayer(PlayerPos__:Integer);
begin
  with Game do begin
    Board[PlayerPos]:=Board[PlayerPos] and (not PLAYER); {remove player from the board}
    PlayerPos:=PlayerPos__;                              {update player position}
    if PlayerPos<>0 then Board[PlayerPos]:=Board[PlayerPos] or PLAYER;
    end;
end;

procedure UndoPush(BoxNo__:Integer; Direction__:TDirection);
var i,FromSquare,ToSquare:Integer;
begin
  if BoxNo__<>0 then with Game do begin {only pushes are handled}
     FromSquare:=BoxPos[BoxNo__];
     ToSquare  :=FromSquare-SquareOffsetForward[Direction__];
     Dec(Board[FromSquare],BOX);
     Inc(Board[ToSquare  ],BOX);
     BoxPos[BoxNo__]:=ToSquare;
     Dec(Board[PlayerPos],PLAYER);
     PlayerPos:=ToSquare-SquareOffsetForward[Direction__];
     Inc(Board[PlayerPos],PLAYER);
     HashValue:=(HashValue xor SquareHashValues[FromSquare])
                xor
                SquareHashValues[ToSquare];
     Inc(SimpleLowerBound,DistanceToNearestTarget[ToSquare]-DistanceToNearestTarget[FromSquare]);

     with DeadlockSets do begin
       for i:=1 to SquareSetCount[FromSquare] do Inc(Capacity[SquareSetNumbers[FromSquare,i]]); {leaving  these deadlock-sets}
       for i:=1 to SquareSetCount[ToSquare  ] do Dec(Capacity[SquareSetNumbers[ToSquare  ,i]]); {entering these deadlock-sets}
       end;
     end;
end;

procedure DoPull(BoxNo__:Integer; Direction__:TDirection);   {reverse mode: pulling boxes instead of pushing them}
begin
  UndoPush(BoxNo__,OPPOSITE_DIRECTION[Direction__]);
end;

procedure UndoPull(BoxNo__:Integer; Direction__:TDirection); {reverse mode: pulling boxes instead of pushing them}
begin
  DoPush  (BoxNo__,OPPOSITE_DIRECTION[Direction__]);
end;

{-----------------------------------------------------------------------------}

{Pathfinding}

procedure CalculateDistanceToNearestBoxStartPositionForAllSquares(UsePlayerStartPosition__:Boolean; var Distances__:TBoardOfInteger);
type TQueueItem=record BoxPos,PlayerPos,Distance:Integer; end;
var  BoxNo,BoxPosition,BoxToSquare,Distance,LastBoxPosition,
     NeighborSquare,OldPlayerPosition,PlayerFromSquare,Square:Integer;
     Direction:TDirection;
     Distances:TSquareDirectionArrayOfInteger;
     QueueBottom,QueueTop:^TQueueItem;
     QueueItems:array[0..MAX_BOARD_SIZE*NUMBER_OF_DIRECTIONS] of TQueueItem;
begin // precondition: the game state matches the starting position
  with Game do begin
    OldPlayerPosition:=PlayerPos;
    for BoxNo:=1 to Game.BoxCount do Board[BoxPos[BoxNo]]:=Board[BoxPos[BoxNo]] and (not BOX); {remove all boxes from the board}

    for Square:=0 to BoardSize do
        if   IsAWallSquare(Square) then
             for Direction:=Low(Direction) to High(Direction) do
                 Distances[Square,Direction]:=-INFINITY {wall square}
        else for Direction:=Low(Direction) to High(Direction) do
                 Distances[Square,Direction]:=INFINITY; {floor square}

    LastBoxPosition:=0;
    QueueBottom:=Addr(QueueItems[Low(QueueItems)]); QueueTop:=QueueBottom;

    for BoxNo:=1 to Game.BoxCount do begin
        Square:=BoxPos[BoxNo];
        if (Board[Square] and WALL)=0 then {'True': the box square hasn't been temporarily disabled}
           if  UsePlayerStartPosition__ then begin
               Inc(Board[Square],BOX);
               CalculatePlayersReachableSquares(0,0);
               Dec(Board[Square],BOX);
               end;
           for Direction:=Low(Direction) to High(Direction) do begin
               NeighborSquare:=Square+SquareOffsetForward[Direction];
               if      (not IsAWallSquare(NeighborSquare)) and
                       ((not UsePlayerStartPosition__)
                        or
                        (SearchStates[0].PlayersReachableSquares.Squares[NeighborSquare]=SearchStates[0].PlayersReachableSquares.TimeStamp)
                       ) then begin
                       Inc(QueueTop);
                       QueueTop^.BoxPos:=Square;
                       QueueTop^.PlayerPos:=NeighborSquare;
                       QueueTop^.Distance:=0;
                       Distances[Square,OPPOSITE_DIRECTION[Direction]]:=0;      {'OPPOSITE_DIRECTION': e.g., after pushing the box to the left, the player is on the right side of the box}
                       end
               else if not UsePlayerStartPosition__ then
                       Distances[Square,Direction]:=0;
               end;
        end;

    while (QueueBottom<>QueueTop) do with SearchStates[0] do begin
      Inc(QueueBottom);
      BoxPosition:=QueueBottom^.BoxPos;
      Game.PlayerPos:=QueueBottom^.PlayerPos;
      Distance:=Succ(QueueBottom^.Distance); {distance after pushing a box in current position}

      Inc(Game.Board[BoxPosition],BOX); {put the box on the board}

      if  (BoxPosition<>LastBoxPosition) or
          (PlayersReachableSquares.Squares[Game.PlayerPos]<>PlayersReachableSquares.TimeStamp) then begin
          {the current set of player's reachable squares isn't valid anymore; recalculate the set}
          LastBoxPosition:=BoxPosition;
          CalculatePlayersReachableSquares(0,0); {calculate player's reachable squares to see which sides of the box the player can reach}
          end;

      for Direction:=Low(Direction) to High(Direction) do begin
          BoxToSquare     :=BoxPosition+Game.SquareOffsetForward[Direction];
          PlayerFromSquare:=BoxPosition-Game.SquareOffsetForward[Direction];

          if (Distances[BoxToSquare,Direction]>Distance) and
             (PlayersReachableSquares.Squares[PlayerFromSquare]=PlayersReachableSquares.TimeStamp) and
             ((Game.Board[BoxToSquare   ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0) then begin
             Distances[BoxToSquare,Direction]:=Distance;
             Inc(QueueTop);
             QueueTop^.BoxPos:=BoxToSquare;
             QueueTop^.PlayerPos:=BoxPosition;
             QueueTop^.Distance:=Distance;
             end;
          end;

      Dec(Game.Board[BoxPosition],BOX); {remove the box from the board again}
      end;

    for Square:=0 to BoardSize do begin
        Distances__[Square]:=INFINITY;
        if not IsAWallSquare(Square) then
           for Direction:=Low(Direction) to High(Direction) do
               Distances__[Square]:=Min(Distances__[Square],Distances[Square,Direction]);
        end;

    for BoxNo:=1 to Game.BoxCount do begin
        Inc(Board[BoxPos[BoxNo]],BOX); {put all boxes back on the board}
        Distances__[BoxPos[BoxNo]]:=0; {ensure that the distance is zero for all box positions; the breadth-first search does not guarantee that for isolated boxes when the player's start position is taken into account}
        end;
    PlayerPos:=OldPlayerPosition; {restore player position}
    end;
end; {CalculateDistanceToNearestBoxStartPositionForAllSquares}

procedure ShowBoxDistanceToAllSquares(const Distance__:TBoardOfInteger);
var Col,Row,Square,Distance:Integer; s:String;
begin
  with Game do begin
    for Row:=1 to BoardHeight do begin
        s:='';
        for Col:=1 to BoardWidth do begin
            Square:=ColRowToSquare(Col,Row);
            Distance:=Distance__[Square];
            if (Board[Square] and WALL)<>0 then
               s:=s+'  #'
            else if   Distance>=BoardSize then s:=s+'  _'
                 else s:=s+Format('%3d',[Distance]);
            end;
        WritelnToLogFile(s);
        end;
    end;
end;

procedure CalculateBoxDistanceToAllSquares( FromSquare__ : Integer; PlayerCanReachAllFloorsAroundBox__ : PBoardOfBoolean; var Distance__ : TBoardOfInteger );
var i {, Col, Row } : Integer; Direction : TDirection; Distance : TSquareDirectionArrayOfInteger;
begin
  CalculateBoxPullOrPushDistances( FromSquare__, 0, True, True, False, PlayerCanReachAllFloorsAroundBox__, Distance );
  for i := 0 to Game.BoardSize do begin
      Distance__[ i ] := INFINITY;
      for Direction := Low( Direction ) to High( Direction ) do
            if Abs( Distance[ i, Direction ] ) <> INFINITY then
               Distance__[ i ] := Min( Distance__[ i ], Distance[ i, Direction ] );
      end;
{
  if CreateLogFile(MainForm.ApplicationDataPath+ExtractFileName(Application.ExeName)) then
     try
       SquareToColRow( FromSquare__, Col, Row );
       WritelnToLogFile( Format( 'Distance to all squares from [%d,%d]', [ Col, Row ] ) );
       ShowBoxDistanceToAllSquares( Distance__ );
     finally
       CloseLogFile;
       Msg( 'Check log file', '', MB_OK );
     end;
}
end; {CalculateBoxDistanceToAllSquares}

function ShowSquareGoalDistances : Boolean;
var // precondition: must only be called after deadlock sets have been
    // calculated and before client threads get access to the internal deadlock
    // detection board;
    // to improve the speed, the function only calculates box->goal distances,
    // and not square->goal distances for all the squares;
  Index, Col, Row, SquareNo, GoalNo, OldBoardSquare, OriginalGoalCount, OldPlayerPos : Integer;
  OldLogFileEnabled, OldXSBNotation : Boolean;
  SquareChar : Char;
  Text : String;
  OldBoard : TBoard;
  Distance : TBoardOfInteger;
  PlayerCanReachAllFloorsAroundBox : TBoardOfBoolean;
  OriginalGoalPos : TBoxPos;
begin
  OldLogFileEnabled           := Dead_.Game.LogFileEnabled;
  OldXSBNotation              := Dead_.Game.XSBNotation;
  OldBoard                    := Dead_.Game.Board;
  OldPlayerPos                := Dead_.Game.PlayerPos;
  try
    Dead_.Game.LogFileEnabled := True;
    Dead_.Game.XSBNotation    := False;
    Result                    := Game.LogSquareGoalDistances and
                                 ( not Game.ReverseMode ) and
                                 CreateLogFile(
                                   MainForm.ApplicationDataPath +
                                   ExtractFileName( Application.ExeName ));

    if Result then with Game do begin
       Dead_.Game.Board := OriginalBoard; // show the original board
       WriteBoardToLogFile(Game.Title);
       OriginalGoalCount := 0;
       for Index := 0 to BoardSize do begin // remove boxes from the board
           Board[ Index ] := Board[ Index ] and ( not BOX );
           if ( Board[ Index ] and GOAL ) <> 0 then
              if   OriginalGoalCount < MAX_BOXES then begin
                   Inc( OriginalGoalCount );
                   OriginalGoalPos[ OriginalGoalCount ] := Index;
                   end
              else Dec( Board[ Index ], GOAL );
           end;
       MovePlayer( OriginalPlayerPos ); // put player at original start position

       // calculate whether the player can reach all floors around a box at a
       // square when the box is the only one on the board; this is useful for
       // speeding up the calculation of the box->goal distances;
       CalculatePlayerCanReachAllFloorsAroundASingleBoxOnTheBoard(
         PlayerCanReachAllFloorsAroundBox );

         WritelnToLogFile( '' );
         WritelnToLogFile( 'Goal Squares' );
         for Index := 1 to OriginalGoalCount do begin
             SquareToColRow( OriginalGoalPos[ Index ], Col, Row );
             WritelnToLogFile( Format( '[%3d,%3d] %5d', [ Col, Row, Col + ( Pred( Row ) * BoardWidth ) ] ) );
             end;
         WritelnToLogFile( '' );
         WritelnToLogFile( 'Box->Goal Distances' );
         for Row := 1 to BoardHeight do begin
             for Col := 1 to BoardWidth do
                 if Result then begin
                    SquareNo := ColRowToSquare( Col, Row );
                    if ( ( OldBoard     [ SquareNo ] and FLAG_BOX_REACHABLE_SQUARE ) = 0 ) or         // 'True': a box (in the player-reachable area of the board) cannot reach the square
                       ( ( OriginalBoard[ SquareNo ] and BOX                       ) = 0 ) then begin // 'True': there is no box on the square
                       end
                    else begin
                       Text := Format( '[%3d,%3d] %5d', [ Col, Row, Col + ( Pred( Row ) * BoardWidth ) ] );

                       OldBoardSquare    := Board[ SquareNo ];
                       Board[ SquareNo ] := OriginalBoard[ SquareNo ]; // show contents of the square from the original board
                       SquareChar        := SquareToChar( SquareNo );
                       Board[ SquareNo ] := OldBoardSquare;

                       if SquareChar = CH_FLOOR then SquareChar := CH_FLOOR_NON_BLANK;
                       Text := Text + SPACE + SquareChar + SPACE;

                       CalculateBoxDistanceToAllSquares( SquareNo, PBoardOfBoolean( Addr( PlayerCanReachAllFloorsAroundBox ) ), Distance );
                       for GoalNo := 1 to OriginalGoalCount do begin
                           if   Abs( Distance[ OriginalGoalPos[ GoalNo ] ] ) <> INFINITY then
                                Text := Text + Format( '%5d', [ Distance[ OriginalGoalPos[ GoalNo ] ] ] )
                           else Text := Text + Format( '%5d', [ 9999 ] );
                           end;

                       Result := WritelnToLogFile( Text ) and
                                 ( Game.CalculateDeadlockSetsJobNo = Game.JobNo ) and
                                 ( MainForm.Deadlocks.Thread.State = Ord( dlIsCalculating ) ) and
                                 Result;
                       end;
                    end;
             end;
       end;
    finally Dead_.Game.Board          := OldBoard;
            Dead_.Game.LogFileEnabled := OldLogFileEnabled;
            Dead_.Game.XSBNotation    := OldXSBNotation;
            MovePlayer( OldPlayerPos );
            CloseLogFile;
    end;
end;

procedure CalculateBoxDistanceToAllSquaresForAllDirections(FromSquare__:Integer; Initialize__,ReallyCalculateDistances__:Boolean;
                                                           var Distance__:TSquareDirectionArrayOfInteger);
var i,OldPlayerPos:Integer; IsABoxPosition:Boolean; Dir:TDirection;

  procedure TrySquare(Square__,PathLength__:Integer);
  var i,BoxToSquare,PlayerFromSquare:Integer; Dir:TDirection;
      PlayerCanReachNeighborSquare:array[TDirection] of Boolean; p:^TDirection;
      Neighbors:array[0..NUMBER_OF_DIRECTIONS] of TDirection;     {at least one extra element so a pointer beyond last used element is legal}
  begin {try to push the box away from the square}
    with Game do begin
       CalculatePlayersReachableSquares(0,0);
       for Dir:=Low(Dir) to High(Dir) do with Game.SearchStates[0].PlayersReachableSquares do
           PlayerCanReachNeighborSquare[Dir]:=                    {save player's reachable squares}
             Squares[Square__+Game.SquareOffsetForward[Dir]]=TimeStamp;

       if PathLength__=0 then begin                               {root square: set distance to zero for the reachable sides}
          i:=0;
          for Dir:=Low(Dir) to High(Dir) do
              if PlayerCanReachNeighborSquare[Dir] then begin
                 Distance__[Square__,OPPOSITE_DIRECTION[Dir]]:=0; {'OPPOSITE_DIRECTION': e.g., with the player to the LEFT of the box, then pushing the box RIGHT to the same square results in an identical position}
                 Inc(i);
                 end;
          if i=0 then
             for Dir:=Low(Dir) to High(Dir) do
                 Distance__[Square__,Dir]:=0;                     {box on isolated square}
          end;

       p:=Addr(Neighbors);

       for Dir:=Low(Dir) to High(Dir) do begin
           BoxToSquare     :=Square__+Game.SquareOffsetForward[Dir];
//         PlayerFromSquare:=Square__-Game.SquareOffsetForward[Dir];
           if  (Succ(PathLength__)<Distance__[BoxToSquare,Dir]) and
               PlayerCanReachNeighborSquare[OPPOSITE_DIRECTION[Dir]] and
               ((Board[BoxToSquare   ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0)  then begin
               if   ReallyCalculateDistances__ then
                    Distance__[BoxToSquare,Dir]:=Succ(PathLength__)
               else Distance__[BoxToSquare,Dir]:=1;               {'1': we're only interested in knowing whether the squares are reachable}
               p^:=Dir; Inc(p);
               end;
           end;

       PlayerFromSquare:=PlayerPos;
       PlayerPos:=Square__; Dec(Board[Square__],BOX);

       while p<>Addr(Neighbors) do begin {visit updated neighbors}
         Dec(p);                         {(updating all neighbors in a breadth-first manner before recursion reduces futile calculations)}
         BoxToSquare := Square__+ Game.SquareOffsetForward[p^];

         Inc(Board[BoxToSquare],BOX);

         TrySquare(BoxToSquare,Succ(PathLength__));

         Dec(Board[BoxToSquare],BOX);
         end;

       PlayerPos:=PlayerFromSquare; Inc(Board[Square__],BOX);
       end;
  end;

begin {CalculateBoxDistanceToAllSquaresForAllDirections}
  with Game do begin
    OldPlayerPos:=PlayerPos;
    IsABoxPosition:=(Board[FromSquare__] and BOX)<>0;
    if IsABoxPosition then Dec(Board[FromSquare__],BOX);

    if Initialize__ then {first time, otherwise we're minimizing for several boxes}
       for i:=0 to BoardSize do
           if   (Board[i] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0 then
                for Dir:=Low(Dir) to High(Dir) do Distance__[i,Dir]:= INFINITY  {open squares}
           else for Dir:=Low(Dir) to High(Dir) do Distance__[i,Dir]:=-INFINITY; {filled squares, e.g., walls}

    if  Distance__[FromSquare__,Low(Dir)]<>-INFINITY then begin
        Inc(Board[FromSquare__],BOX);

        TrySquare(FromSquare__,0);

        Dec(Board[FromSquare__],BOX);
        end;

    PlayerPos:=OldPlayerPos;
    if IsABoxPosition then Inc(Board[FromSquare__],BOX);
    end;
end; {CalculateBoxDistanceToAllSquaresForAllDirections}

procedure CalculateBoxPullOrPushDistances(
            BoxSquare__,PlayersReachableSquaresIndex__:Integer;
            PushBox__,UsePlayerPosition__,ContinueCalculation__:Boolean;
            PlayerCanReachAllFloorsAroundBox__ : PBoardOfBoolean;
            var Distances__:TSquareDirectionArrayOfInteger);
type TQueueItem=record BoxPos,PlayerPos,Distance:Integer; end;
var  BoxPosition,BoxToSquare,Distance,DistanceInitializationValue,
     LastBoxPosition,NeighborSquare,
     OldBoxSquareValue,OldPlayerPosition,PlayerFromSquare,PlayerToSquare,Square:Integer;
     Direction:TDirection;
     QueueBottom,QueueTop:^TQueueItem;
     QueueItems:array[0..MAX_BOARD_SIZE*DIRECTION_COUNT] of TQueueItem;
begin {calculates the distance to all reachable squares by pulling or pushing a box at 'BoxSquare__' around on the board}
  with Game do with SearchStates[PlayersReachableSquaresIndex__] do begin
    OldPlayerPosition:=PlayerPos; OldBoxSquareValue:=Board[BoxSquare__];
    Board[BoxSquare__]:=Board[BoxSquare__] and (not BOX); {remove the box, if any, from the board}

    DistanceInitializationValue:=0;
    if not ContinueCalculation__ then begin {otherwise, the calculation continues with the existing distances in 'Distances__'}
       for Square:=0 to BoardSize do
           if   (Board[Square] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))<>0 then
                for Direction:=Low(Direction) to High(Direction) do
                    Distances__[Square,Direction]:=-INFINITY  {wall square : '-infinity' : the search cannot find a better path to the square}
           else for Direction:=Low(Direction) to High(Direction) do
                    Distances__[Square,Direction]:=+INFINITY; {floor square: '+infinity' : the search might  find a better path to the square}
       end
    else for Direction:=Low(Direction) to High(Direction) do
             if (Abs(Distances__[BoxSquare__,Direction])<>INFINITY) then
                {when the calculation continues it merely taints reachable
                 squares; it doesn't treat each continuation square as a new
                 0-distance starting point;
                }
                DistanceInitializationValue:=Max(DistanceInitializationValue,Distances__[BoxSquare__,Direction]);

    LastBoxPosition:=0;
    QueueBottom:=Addr(QueueItems[Low(QueueItems)]); QueueTop:=QueueBottom;

    if  UsePlayerPosition__ then begin
        Inc(Board[BoxSquare__],BOX);
        CalculatePlayersReachableSquares(PlayersReachableSquaresIndex__,0);
        Dec(Board[BoxSquare__],BOX);
        end;

    for Direction:=Low(Direction) to High(Direction) do begin
        NeighborSquare:=BoxSquare__+SquareOffsetForward[Direction];
        if      ((Board[NeighborSquare] and WALL)=0)
                and
                ((not UsePlayerPosition__)
                 or
                 (PlayersReachableSquares.Squares[NeighborSquare]=PlayersReachableSquares.TimeStamp)
                ) then begin
                Inc(QueueTop);
                QueueTop^.BoxPos   :=BoxSquare__;
                QueueTop^.PlayerPos:=NeighborSquare;
                QueueTop^.Distance :=0;
                if   PushBox__ then                                             {'True': push the box around on the board, as opposed to pulling it around on the board}
                     Distances__[BoxSquare__,OPPOSITE_DIRECTION[Direction]]:=DistanceInitializationValue  {'OPPOSITE_DIRECTION': e.g., after pushing the box to the left, the player is on the right side of the box}
                else Distances__[BoxSquare__,Direction]:=DistanceInitializationValue;
                end
        else if not UsePlayerPosition__ then
                if   PushBox__ then
                     Distances__[BoxSquare__,OPPOSITE_DIRECTION[Direction]]:=DistanceInitializationValue  {'OPPOSITE_DIRECTION': e.g., after pushing the box to the left, the player is on the right side of the box}
                else Distances__[BoxSquare__,Direction]:=DistanceInitializationValue;
        end;

    if  QueueBottom = QueueTop then {'True': the box is located at an isolated square}
        for Direction:=Low(Direction) to High(Direction) do
            Distances__[BoxSquare__,Direction]:=0; {the distance to the box square is 0}

    while QueueBottom<>QueueTop do begin {breadth-first search}
          Inc(QueueBottom);
          BoxPosition   :=QueueBottom^.BoxPos;
          Game.PlayerPos:=QueueBottom^.PlayerPos;
          Distance      :=Succ(QueueBottom^.Distance); {distance after pushing a box in the current position}

          Inc(Game.Board[BoxPosition],BOX); {put the box on the board}

          if  (BoxPosition<>LastBoxPosition) or
              (PlayersReachableSquares.Squares[Game.PlayerPos]<>PlayersReachableSquares.TimeStamp) then begin
              {the current set of player's reachable squares isn't valid anymore; recalculate the set}
              LastBoxPosition:=BoxPosition;
              if   ( not Assigned( PlayerCanReachAllFloorsAroundBox__ ) ) or
                   ( not PlayerCanReachAllFloorsAroundBox__^[ BoxPosition ] ) then
                   CalculatePlayersReachableSquares(PlayersReachableSquaresIndex__,0) {calculate player's reachable squares to see which sides of the box the player can reach}
              else for Direction:=Low(Direction) to High(Direction) do begin {the player can reach all floors around the box}
                       BoxToSquare:=BoxPosition+Game.SquareOffsetForward[Direction];
                       if ( Game.Board[ BoxToSquare ] and ( WALL + BOX + FLOOR ) ) = FLOOR then
                          PlayersReachableSquares.Squares[BoxToSquare] := PlayersReachableSquares.TimeStamp;
                       end;
              end;

          for Direction:=Low(Direction) to High(Direction) do begin
              BoxToSquare   :=BoxPosition+Game.SquareOffsetForward[Direction];
              if PushBox__ then begin                                           {'True': push the box around on the board}
                 PlayerFromSquare:=BoxPosition-Game.SquareOffsetForward[Direction];

                 if (Distances__[BoxToSquare,Direction]>Distance) and           {'True': the existing distance to the square is bigger than the distance via the currently investigated path}
                    (PlayersReachableSquares.Squares[PlayerFromSquare]=PlayersReachableSquares.TimeStamp) and
                    ((Game.Board[BoxToSquare] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0) then begin
                    Distances__[BoxToSquare,Direction]:=Distance;
                    Inc(QueueTop);
                    QueueTop^.BoxPos   :=BoxToSquare;
                    QueueTop^.PlayerPos:=BoxPosition;
                    QueueTop^.Distance :=Distance;
                    end;
                 end
              else begin                                                        {pull the box around on the board}
                 PlayerToSquare:=BoxToSquare+Game.SquareOffsetForward[Direction];

                 if (Distances__[BoxToSquare,Direction]>Distance) and
                    (PlayersReachableSquares.Squares[BoxToSquare]=PlayersReachableSquares.TimeStamp) and
                    ((Game.Board[PlayerToSquare] and (WALL+BOX))=0) and
                    ((Game.Board[BoxToSquare   ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0) and
                    {caution: the following check for the pusher painting itself into a corner assumes 4 directions only}
                    ((not ( ((Game.Board[PlayerToSquare+Game.SquareOffsetForward[Direction]] and WALL)<>0)
                            and
                            ((Game.Board[PlayerToSquare+Game.SquareOffsetLeft   [Direction]] and WALL)<>0)
                            and
                            ((Game.Board[PlayerToSquare+Game.SquareOffsetRight  [Direction]] and WALL)<>0)
                          )
                     )
                     or
                     (PlayerToSquare=StartPlayerPos) {'=': if the player after the pull ends at its starting position in the game, then the square is all right}
                    ) then begin
                    Distances__[BoxToSquare,Direction]:=Distance;
                    Inc(QueueTop);
                    QueueTop^.BoxPos   :=BoxToSquare;
                    QueueTop^.PlayerPos:=PlayerToSquare;
                    QueueTop^.Distance :=Distance;
                    end;
                 end;
              end;

          Dec(Game.Board[BoxPosition],BOX); {remove the box from the board again}
          end;

    Board[BoxSquare__]:=OldBoxSquareValue; {restore box square value}
    PlayerPos:=OldPlayerPosition; {restore player position}
    end;
end; {CalculateBoxPullOrPushDistances}

procedure CalculateDistanceToNearestGoalForAllSquares_A(JobNo__:Int64; var Distances__:TBoardOfInteger);
var i,OldPlayerPos:Integer; Dir:TDirection; Distance:TSquareDirectionArrayOfInteger;

  procedure TrySquare(Square__,PathLength__:Integer);
  var BoxToSquare,PlayerFromSquare,PlayerToSquare:Integer; Dir:TDirection;
      PlayerCanReachNeighborSquare:array[TDirection] of Boolean; p:^TDirection;
      Neighbors:array[0..NUMBER_OF_DIRECTIONS] of TDirection; {at least one extra element so a pointer beyond last used element is legal}
  begin {try to pull the box away from the square}
    with Game do begin
       CalculatePlayersReachableSquares(0,0);
       for Dir:=Low(Dir) to High(Dir) do with Game.SearchStates[0].PlayersReachableSquares do
           PlayerCanReachNeighborSquare[Dir]:=           {save player's reachable squares}
             Squares[Square__+Game.SquareOffsetForward[Dir]]=TimeStamp;
       p:=Addr(Neighbors);

       for Dir:=Low(Dir) to High(Dir) do begin
           BoxToSquare   :=Square__+  Game.SquareOffsetForward[Dir];
           PlayerToSquare:=Square__+2*Game.SquareOffsetForward[Dir];
           if  (Succ(PathLength__)<Distance[BoxToSquare,Dir]) and
               PlayerCanReachNeighborSquare[Dir] and
               ((Board[BoxToSquare   ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0) and
               ((Board[PlayerToSquare] and (WALL+BOX                        ))=0) and
               (JobNo__=Game.JobNo) then begin
               Distance[BoxToSquare,Dir]:=Succ(PathLength__);
               p^:=Dir; Inc(p);
               end;
           end;

       PlayerFromSquare:=PlayerPos; Dec(Board[Square__],BOX);

       while p<>Addr(Neighbors) do begin {visit updated neighbors}
         Dec(p);                         {(updating all neighbors in a breadth-first manner before recursion reduces futile calculations)}
         PlayerPos  :=Square__+2*Game.SquareOffsetForward[p^];
         BoxToSquare:=Square__+  Game.SquareOffsetForward[p^];
         Inc(Board[BoxToSquare],BOX);

         TrySquare(BoxToSquare,Succ(PathLength__));

         Dec(Board[BoxToSquare],BOX);
         end;

       PlayerPos:=PlayerFromSquare; Inc(Board[Square__],BOX);
       end;
  end;

begin {CalculateDistanceToNearestGoalForAllSquares_A}
  with Game do begin
    OldPlayerPos:=PlayerPos;
    for i:=1 to BoxCount do Dec(Board[BoxPos[i]],BOX);                          {remove all boxes from the board}

    for i:=0 to BoardSize do
        if   IsAWallSquare(i) then
             for Dir:=Low(Dir) to High(Dir) do Distance[i,Dir]:=-INFINITY       {walls}
        else for Dir:=Low(Dir) to High(Dir) do Distance[i,Dir]:= INFINITY;      {floors}

    for i:=1 to GoalCount do
        for Dir:=Low(Dir) to High(Dir) do Distance[GoalPos[i],Dir]:=0;

    for i:=1 to GoalCount do begin
        for Dir:=Low(Dir) to High(Dir) do begin
            PlayerPos:=GoalPos[i]+SquareOffsetForward[Dir];
            if Distance[PlayerPos,Dir]<>-INFINITY then begin
               Inc(Board[GoalPos[i]],BOX);

               TrySquare(GoalPos[i],0);

               Dec(Board[GoalPos[i]],BOX);
               end;
            end;
        end;

    for i:=0 to BoardSize do begin
        Distances__[i]:=INFINITY;
        for Dir:=Low(Dir) to High(Dir) do
            if Abs(Distance[i,Dir])<>INFINITY then
               Distances__[i]:=Min(Distances__[i],Distance[i,Dir]);
        if (Distances__[i]=INFINITY) and ((Board[i] and FLOOR)<>0) and (JobNo__=Game.JobNo) then {if the job number has changed, then the calculation didn't run to its completion, and the distances cannot be trusted}
           Board[i]:=Board[i] or FLAG_ILLEGAL_BOX_SQUARE;                       {mark unreachable squares as illegal squares}
        end;

    PlayerPos:=OldPlayerPos;                                                    {restore player position}
    for i:=1 to BoxCount do Inc(Board[BoxPos[i]],BOX);                          {put all boxes back on the board}
    end;
end; {CalculateDistanceToNearestGoalForAllSquares_A}

function  CalculateDistanceToNearestGoalForAllSquares_B(JobNo__:Int64; MarkIllegalBoxSquares__:Boolean; var Distances__:TBoardOfInteger):Boolean;
type TQueueItem=record BoxPos,PlayerPos,Distance:Integer; end;
var  BoxNo,BoxPosition,BoxToSquare,Distance,GoalNo,GoalSquare,LastBoxPosition,
     OldPlayerPosition,PlayerToSquare,Square:Integer;
     Direction:TDirection;
     Distances:TSquareDirectionArrayOfInteger;
     QueueBottom,QueueTop:^TQueueItem;
     QueueItems:array[0..MAX_BOARD_SIZE*NUMBER_OF_DIRECTIONS] of TQueueItem;
begin
  with Game do begin
    OldPlayerPosition:=PlayerPos;
    for BoxNo:=1 to Game.BoxCount do Board[BoxPos[BoxNo]]:=Board[BoxPos[BoxNo]] and (not BOX); {remove all boxes from the board}

    for Square:=0 to BoardSize do
        if   IsAWallSquare(Square) then
             for Direction:=Low(Direction) to High(Direction) do
                 Distances[Square,Direction]:=-INFINITY {wall square}
        else for Direction:=Low(Direction) to High(Direction) do
                 Distances[Square,Direction]:=INFINITY; {floor square}

    LastBoxPosition:=0;
    QueueBottom:=Addr(QueueItems[Low(QueueItems)]); QueueTop:=QueueBottom;

    for GoalNo:=1 to GoalCount do begin
        GoalSquare:=GoalPos[GoalNo];
        if (Board[GoalSquare] and GOAL)<>0 then {'True': the goal square hasn't been temporarily disabled}
           for Direction:=Low(Direction) to High(Direction) do begin
               Distances[GoalSquare,Direction]:=0;
               Square:=GoalSquare+SquareOffsetForward[Direction];
               if not IsAWallSquare(Square) then begin
                  Inc(QueueTop);
                  QueueTop^.BoxPos:=GoalSquare;
                  QueueTop^.PlayerPos:=Square;
                  QueueTop^.Distance:=0;
                  end;
               end;
        end;

    while (QueueBottom<>QueueTop) do with SearchStates[0] do begin
      Inc(QueueBottom);
      BoxPosition   :=QueueBottom^.BoxPos;
      Game.PlayerPos:=QueueBottom^.PlayerPos;
      Distance      :=Succ(QueueBottom^.Distance); {distance after pushing a box in current position}

      Inc(Game.Board[BoxPosition],BOX); {put the box on the board}

      if  (BoxPosition<>LastBoxPosition) or
          (PlayersReachableSquares.Squares[Game.PlayerPos]<>PlayersReachableSquares.TimeStamp) then begin
          {the current set of player's reachable squares isn't valid anymore; recalculate the set}
          LastBoxPosition:=BoxPosition;
          CalculatePlayersReachableSquares(0,0); {calculate player's reachable squares to see which sides of the box the player can reach}
          end;

      for Direction:=Low(Direction) to High(Direction) do begin
          BoxToSquare   :=BoxPosition+  Game.SquareOffsetForward[Direction];
          PlayerToSquare:=BoxPosition+2*Game.SquareOffsetForward[Direction];

          if (Distances[BoxToSquare,Direction]>Distance) and
             (PlayersReachableSquares.Squares[BoxToSquare]=PlayersReachableSquares.TimeStamp) and
             ((Game.Board[BoxToSquare   ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0) and
             ((Game.Board[PlayerToSquare] and (WALL+BOX                        ))=0) and
             {caution: the following check for the pusher painting itself into a corner assumes 4 directions only}
             ((not ( ((Game.Board[PlayerToSquare+Game.SquareOffsetForward[Direction]] and WALL)<>0)
                     and
                     ((Game.Board[PlayerToSquare+Game.SquareOffsetLeft   [Direction]] and WALL)<>0)
                     and
                     ((Game.Board[PlayerToSquare+Game.SquareOffsetRight  [Direction]] and WALL)<>0)
                   )
              )
              or
              (PlayerToSquare=StartPlayerPos)
             ) and
             (JobNo__=Game.JobNo) then begin
             Distances[BoxToSquare,Direction]:=Distance;
             Inc(QueueTop);
             QueueTop^.BoxPos:=BoxToSquare;
             QueueTop^.PlayerPos:=PlayerToSquare;
             QueueTop^.Distance:=Distance;
             end;
          end;

      Dec(Game.Board[BoxPosition],BOX); {remove the box from the board again}
      end;

    Result:=JobNo__=Game.JobNo; {'True': the calculation ran to its completion}

    for Square:=0 to BoardSize do begin
        Distances__[Square]:=INFINITY;
        if Result then begin    {'True': the calculation ran to its completion}
           if not IsAWallSquare(Square) then
              for Direction:=Low(Direction) to High(Direction) do
                  Distances__[Square]:=Min(Distances__[Square],Distances[Square,Direction]);
           if (Distances__[Square]=INFINITY) and IsAFloorSquare(Square) and MarkIllegalBoxSquares__ then
              Board[Square]:=Board[Square] or FLAG_ILLEGAL_BOX_SQUARE; {mark unreachable squares as illegal squares}
           end;
        end;

    for BoxNo:=1 to Game.BoxCount do Inc(Board[BoxPos[BoxNo]],BOX); {put all boxes back on the board}
    PlayerPos:=OldPlayerPosition; {restore player position}
    end;
end; {CalculateDistanceToNearestGoalForAllSquares_B}

{-----------------------------------------------------------------------------}

procedure TestDistanceToNearestGoalForEachSquare;
const
  LEVEL_FILE_NAMES: array[0..7] of String = (
    '',
    'YASGen.sok',
    'Yoshio.sok',
    'du Peloux.sok',
    'Haywood.sok',
    'Holland.sok',
    'GRIGoRusha.sok',
    'Skinner.sok'
    );
var
  i,FileIndex,LevelCount:Integer;
  t1,t2,t3,Total1,Total2,Total3:TTimeMS;
  b:Boolean; s,Path:String;
  Level:TLevel;
  LogFile:TextFile;

  function  Test(var t1__,t2__,t3__:TTimeMS; var ErrorStr__:String):Boolean;
  var Col,Row,Square,ErrorCount:Integer; Distance1,Distance2:TBoardOfInteger;
  begin {Test}
    ErrorStr__:=''; ErrorCount:=0; t3:=0;
    with Dead_.Game do begin
      CalculateDistanceToNearestGoalForAllSquares_A(Game.JobNo,Distance1); // to calculate 'FLAG_ILLEGAL_BOX_SQUARE' flags for all squares so the 'A' test and the 'B' test have the same board state to work with
      t1:=GetTimeMS;
      CalculateDistanceToNearestGoalForAllSquares_A(Game.JobNo,Distance1);
      t1:=CalculateElapsedTimeMS(t1,GetTimeMS);
      t2:=GetTimeMS;
      CalculateDistanceToNearestGoalForAllSquares_B(Game.JobNo,True,Distance2);
      t2:=CalculateElapsedTimeMS(t2,GetTimeMS);

      for Square:=0 to BoardSize do begin
          Inc(t3,Distance1[Square]-Distance2[Square]);
          Inc(ErrorCount,Distance1[Square]-Distance2[Square]);
          if Distance1[Square]<>Distance2[Square] then begin
             SquareToColRow(Square,Col,Row);
             Msg(IntToStr(Col)+COMMA+IntToStr(Row),'Error',MB_OK);
             end;
          end;
      Result:=ErrorCount=0;
      if not Result then ErrorStr__:=ErrorStr__+' ('+IntToStr(ErrorCount)+' errors)';
      end;
  end; {Test}

begin {TestDistanceToNearestGoalForEachSquare}
  LevelCount:=0; Total1:=0; Total2:=0; Total3:=0;
  Path:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath)+DEFAULT_LEVEL_DIRECTORY);
  Assign(LogFile,Path+'Log.txt'); Rewrite(LogFile);
  MainForm.Deadlocks.Suspend;
  try
{
    for FileIndex:=0 to 100 do with MainForm do begin
        if FileIndex<>0 then s:=Format('Levels (%d).sok',[FileIndex])
        else s:='Levels.sok';
        s:='C:\Program Files\Compilers\Delphi4\Pg\Sokoban\Levels\Sokoban for Windows\'+s;
        if FileExists(s) then
        if SokoFile.LoadFromFile(s) then begin
}
    for FileIndex:=1 to High(LEVEL_FILE_NAMES) do with MainForm do begin
        if SokoFile.LoadFromFile(Path+LEVEL_FILE_NAMES[FileIndex]) then begin
           Level:=TLevel(SokoFile.Levels.GetItemByIndex(0));
           while Level<>nil do begin
             s:=MakeIniFileSectionFileName(SokoFile.Name,Level.Name);
             Status.Hint:=ExtractFileName(SokoFile.Name)+FILE_NAME_PATH_DELIMITER+LEFT_BRACKET+Level.Name+RIGHT_BRACKET;
             Application.ProcessMessages;
             if Game.LoadFromFileOrClipboard(s,nil,nil,b) then begin
                Level:=SokoFile.GetLevelByName(Game.Name);
                if Level<>nil then begin
                   Deadlocks.LoadGame(False,False);
                   Inc(LevelCount);
                   if   Test(t1,t2,t3,s) then begin

                        s:=Level.Name;
                        if AnsiPos(COMMA,s)<>0 then s:=Copy(s,Succ(AnsiPos(COMMA,s)),MaxInt);
                        Writeln(LogFile,Format('%5d %20s %10d %10d %10d',[LevelCount,Copy(s,1,20),t1,t2,t3]));
                        Inc(Total1,t1); Inc(Total2,t2); Inc(Total3,t3);

                        Level:=TLevel(Level.Next);
                        //Level:=nil;
                        end
                   else begin
                     Msg('Calculation mismatch: '+NL+s,Level.Name,MB_OK); Level:=nil;
                     end;
                   end
                else Msg('Internal error',Game.Name,MB_OK);
                Game.Clear;
                end
             else begin
                Error('Level not found: '+s,'');
                Level:=nil;
                end;
             end;
           end
        else Error('File not found: '+LEVEL_FILE_NAMES[FileIndex],'');
        end;

    for i:=1 to 70 do Write(LogFile,'-'); Writeln(LogFile);
    Writeln(LogFile,Format('%37d %10d %10d',[Total1,Total2,Total3]));

    Msg('Levels: '+IntToStr(LevelCount),'',MB_OK);
  finally
    MainForm.Deadlocks.Resume; {if the thread really is working, then it has garbage data now}
    CloseFile(LogFile);
  end;
end; {TestDistanceToNearestGoalForEachSquare}

{-----------------------------------------------------------------------------}

function CalculatePushesLowerBound:Integer;
const
  EPSILON_SCALING_DIVISOR = 4;
  EPSILON_START_VALUE = 256; // must be an EPSILON_SCALING_DIVISOR^N value, where N is a nonnegative integer, and it must be <= MAX_BOX_COUNT_FOR_CALCULATING_MINIMUM_LOWER_BOUND
  MAX_BENEFIT = MAX_BOARD_SIZE*DIRECTION_COUNT + 1;
  MAX_BOX_COUNT_FOR_CALCULATING_MINIMUM_LOWER_BOUND = MAX_BOXES; // 'MAX_BOXES': no other restrictions than the maximum number of boxes
  SHOW_CALCULATION_IN_PROGRESS_MESSAGE_TIME_THRESHOLD_MS = 1000;
type
  TBenefits =  array[0..MAX_BOX_COUNT_FOR_CALCULATING_MINIMUM_LOWER_BOUND,0..MAX_BOX_COUNT_FOR_CALCULATING_MINIMUM_LOWER_BOUND] of Integer;
var
  i,j,n,BoxNo,GoalNo,UnassignedBoxesCount,BestGoalNo,SquareNo,Epsilon,AssignedBoxNo:Integer;
  StartTimeMS{, TimeMS} : TTimeMS;
  CalculationInProgressMessage, OK : Boolean;
  Benefits:^TBenefits;
  OldBoard:TBoard;
  Distances:TBoardOfInteger;
  PlayerCanReachAllFloorsAroundBox:TBoardOfBoolean;
  BoxGoalAssignments,GoalBoxAssignments,UnassignedBoxes:array[0..MAX_BOX_COUNT_FOR_CALCULATING_MINIMUM_LOWER_BOUND] of Integer;
  Value,BestValue,SecondBestValue:Int64; // 'Int64': necessary to avoid numeric overflow
  Prices:array[0..MAX_BOX_COUNT_FOR_CALCULATING_MINIMUM_LOWER_BOUND] of Int64;   // 'Int64': necessary to avoid numeric overflow

  function  TimeCheck : Boolean;
  var TimeMS : TTimeMS;
  begin
    TimeMS := CalculateElapsedTimeMS( StartTimeMS, GetTimeMS );
    Result := ( Game.JobNo  = Game.CalculateDeadlockSetsJobNo ) and
              ( TimeMS      < Game.CalculatePushesLowerBoundTimeLimitMS );
    // only show "calculation in progress" message if the calculation takes a long time
    if Result and
       ( TimeMS >= SHOW_CALCULATION_IN_PROGRESS_MESSAGE_TIME_THRESHOLD_MS ) and
       ( not CalculationInProgressMessage ) then begin
       CalculationInProgressMessage := True;
       if Assigned( MainForm.Status ) and
          ( not MainForm.ShutDownApplication ) then begin
          MainForm.Status.Hint := CalculatingPushesLowerBoundText + ELLIPSES;
          MainForm.Update;
          end;
       end;
  end;

begin // CalculatePushesLowerBound: calculates a pushes lower bound using an auction algorithm
  with Game do {$WARNINGS OFF}
    if      ( ( High(Value) div MAX_BENEFIT )  <= ( MAX_BOXES + 1 ) ) or
            ( ( MAX_BENEFIT * ( MAX_BOXES + 1 ) ) >= INFINITY ) then begin  {$WARNINGS ON}
            Result := PUSHES_LOWER_BOUND_ERROR_NUMERIC_OVERFLOW; //Application.MessageBox('Calculate lower bound: Numeric overflow', PChar( Application.Title ), MB_OK);
            end
    else if Game.BoxCount<=MAX_BOX_COUNT_FOR_CALCULATING_MINIMUM_LOWER_BOUND then begin
            StartTimeMS := GetTimeMS;
            try    New( Benefits ); // try to allocate memory for the 'Benefits' table
            except Benefits := nil; // fail
            end;

            if Assigned( Benefits ) then begin
               try
                 Result := 0; // '0': not calculated yet. the calculated result may also be 0, i.e. a solved game position.
                 OK := True;
                 //MaxBenefit:=Low(MaxBenefit);
                 FillChar(BoxGoalAssignments,SizeOf(BoxGoalAssignments),0);
                 FillChar(GoalBoxAssignments,SizeOf(GoalBoxAssignments),0);
                 FillChar(Prices            ,SizeOf(Prices            ),0);
                 //CalculatePushesLowerBoundTimeLimitMS := High( CalculatePushesLowerBoundTimeLimitMS ); {disable time limit for testing}

                 // calculate whether the player can reach all floors around a
                 // box at a square when the box is the only one on the board;
                 // this is useful for speeding up the calculation of the
                 // box->goal distances;
                 CalculatePlayerCanReachAllFloorsAroundASingleBoxOnTheBoard( PlayerCanReachAllFloorsAroundBox );

                 // calculate minimum distances for all box-goal pairs
                 for BoxNo:=1 to BoxCount do begin
                     SquareNo             := BoxPos[ BoxNo    ];
                     OldBoard[ SquareNo ] := Board [ SquareNo ];
                     Board   [ SquareNo ] := Board [ SquareNo ] and ( not BOX ); // remove all boxes from the board
                     end;
                 for BoxNo:=1 to BoxCount do
                     if OK then begin
                        CalculateBoxDistanceToAllSquares(BoxPos[BoxNo], PBoardOfBoolean( Addr( PlayerCanReachAllFloorsAroundBox ) ), Distances);
                        for GoalNo:=0 to GoalCount do    // index 0 is unused, but 'Benefits[BoxNo,0]' must be initialized to 'INFINITY'
                            Benefits[BoxNo,GoalNo]:=Distances[GoalPos[GoalNo]]; // 'Benefits' is a misnomer at this time; here it contains box-goal pathlengths
                        OK := TimeCheck;
                        end
                     else Break; // quick-and-dirty exit loop
                 for BoxNo:=1 to BoxCount do
                     Board[BoxPos[BoxNo]] := OldBoard[BoxPos[BoxNo]]; // put boxes back on the board, and restore board square flags

                 if OK then begin // 'True': not timeout
                    for GoalNo:=1 to GoalCount do          // are all goals reachable?
                        if not IsAReachableBoxSquare(GoalPos[GoalNo]) then
                           Result:=INFINITY;

                    repeat
                      OK:=True;
                      for BoxNo:=1 to BoxCount do
                          if (BoxGoalAssignments[BoxNo]=0) and (Result<INFINITY) then begin
                             n:=0; j:=0;
                             for GoalNo:=1 to GoalCount do
                                 if  Benefits[BoxNo,GoalNo]<INFINITY then begin     // 'Benefits' is a misnomer at this time; here it contains box-goal distances
                                     Inc(n); j:=GoalNo;    // count number of reachable goals for current box
                                     end;
                             if      n=0 then
                                     Result:=INFINITY      // the box cannot reach a goal: unsolvable level
                             else if n=1 then begin        // the box can reach 1 goal only
                                     BoxGoalAssignments[BoxNo]:=j;
                                     GoalBoxAssignments[j    ]:=BoxNo;
                                     for i:=1 to BoxCount do
                                         if i<>BoxNo then  // reserve the goal by blocking it for all other boxes
                                            Benefits[i,j]:=INFINITY; // 'INFINITY': at this point, 'Benefits' still contain distances
                                     OK:=False;            // check all boxes again
                                     end;
                             end;
                    until OK;

                    for BoxNo:=1 to BoxCount do
                        for GoalNo:=0 to GoalCount do      // change distances to 'benefits'. the auction algorithm increases prices.
                            if   Benefits[BoxNo,GoalNo]<MAX_BENEFIT then begin
                                 Benefits[BoxNo,GoalNo]:=(MAX_BENEFIT-Benefits[BoxNo,GoalNo])*(MAX_BOX_COUNT_FOR_CALCULATING_MINIMUM_LOWER_BOUND+1); // '*': for speed, so epsilon can be an integer
                                 //if Benefits[BoxNo,GoalNo]>MaxBenefit then
                                 //   MaxBenefit:=Benefits[BoxNo,GoalNo];
                                 end
                            else Benefits[BoxNo,GoalNo]:=-INFINITY; // goal number 0 is unused, but 'Benefits[BoxNo,0]' must be < 0

                    UnassignedBoxesCount := 0;
                    for BoxNo :=1 to BoxCount do
                        if BoxGoalAssignments[ BoxNo ] = 0 then begin
                           Inc( UnassignedBoxesCount );
                           UnassignedBoxes[ UnassignedBoxesCount ] := BoxNo;
                           end;

                    if (UnassignedBoxesCount<>0) and (Result=0) then begin // 'True': not all boxes have been assigned to a goal, and the game state is not known to be a deadlock
                       Epsilon := EPSILON_START_VALUE;
                       repeat
                         while (UnassignedBoxesCount<>0) and (Result=0) do begin
                           if TimeCheck then begin
                              BoxNo := UnassignedBoxes[ UnassignedBoxesCount ];
                              Dec( UnassignedBoxesCount );
                              BestGoalNo:=0; BestValue:=-INFINITY; SecondBestValue:=-INFINITY;
                              for GoalNo:=1 to GoalCount do
                                  if Benefits[BoxNo,GoalNo]>=0 then begin
                                     Value:=Int64(Benefits[BoxNo,GoalNo])-Prices[GoalNo];
                                     if      Value>BestValue then begin
                                             SecondBestValue:=BestValue;
                                             BestValue      :=Value;
                                             BestGoalNo     :=GoalNo;
                                            end
                                     else if Value>SecondBestValue then
                                             SecondBestValue:=Value;
                                     end;
                              if BestGoalNo>0 then begin
                                 AssignedBoxNo := GoalBoxAssignments[BestGoalNo];
                                 if AssignedBoxNo <> 0 then begin
                                    Inc( UnassignedBoxesCount );
                                    UnassignedBoxes[ UnassignedBoxesCount ] := AssignedBoxNo;
                                    BoxGoalAssignments[ AssignedBoxNo ] := 0;
                                    end;
                                 BoxGoalAssignments[BoxNo     ]:=BestGoalNo;
                                 GoalBoxAssignments[BestGoalNo]:=BoxNo;
                                 Inc(Prices[BestGoalNo],BestValue-SecondBestValue+Epsilon);
                                 end
                              else Result:=INFINITY; // at least one of the goals is overconstrained
                              end
                           else Result:=PUSHES_LOWER_BOUND_ERROR_TIMEOUT;
                           end;

                         Epsilon:=Epsilon div EPSILON_SCALING_DIVISOR;
                         if (Epsilon<>0) and (Result=0) then begin // 'True': reset the assignments and launch a new auction round with the reduced epsilon
                            UnassignedBoxesCount:=0;
                            for BoxNo:=1 to BoxCount do begin
                                GoalNo:=BoxGoalAssignments[BoxNo];
                                if (GoalNo=0) or (Prices[GoalNo]>0) then begin // 'True': the box is unassigned, or it has been assigned to a goal during the auction
                                   BoxGoalAssignments[BoxNo ]:=0;
                                   GoalBoxAssignments[GoalNo]:=0;
                                   Inc(UnassignedBoxesCount);
                                   UnassignedBoxes[UnassignedBoxesCount]:=BoxNo;
                                   end
                                else begin // the box-goal assignment was made prior to the first auction round. the box can only be assigned to this goal
                                   end;
                                end;
                            end;

                       until (Result<>0) or (Epsilon=0);
                       end;

                    if Result>=0 then begin // 'True': the job hasn't been terminated
                       for  BoxNo:=1 to BoxCount do // calculate result = sum of distances for all boxes
                            if   (Result<INFINITY) and
                                 (Benefits[BoxNo,BoxGoalAssignments[BoxNo]]>0) then
                                 Inc(Result,MAX_BENEFIT-(Benefits[BoxNo,BoxGoalAssignments[BoxNo]]) div (MAX_BOX_COUNT_FOR_CALCULATING_MINIMUM_LOWER_BOUND+1))
                            else Result:=INFINITY;
                       end;
                    end
                 else if   JobNo  =  CalculateDeadlockSetsJobNo then
                           Result := PUSHES_LOWER_BOUND_ERROR_TIMEOUT  // time limit exceeded during the calculation of minimum path lengths
                      else Result := PUSHES_LOWER_BOUND_ERROR_TASK_ABANDONED; // the job was abandoned, e.g., the user opened a different level

               finally Dispose( Benefits );
               end;
               end
            else Result := PUSHES_LOWER_BOUND_ERROR_TASK_FAILED; // allocating memory for the 'Benefits' table failed
            //TimeMS := CalculateElapsedTimeMS( StartTimeMS, GetTimeMS );
            end
         else begin // too many boxes
            //Result := PUSHES_LOWER_BOUND_ERROR_TASK_FAILED;

            // the level is so large that it probably will take a very long time to calculate a lower bound,
            // hence, settle for a minimum lower bound which only gives the correct parity
            Result:=0;
            for BoxNo :=1 to BoxCount  do if IsABlackSquareOnAChessBoard(BoxPos [BoxNo ]) then Inc(Result);
            for GoalNo:=1 to GoalCount do if IsABlackSquareOnAChessBoard(GoalPos[GoalNo]) then Dec(Result);
            Result:=Abs(Result);
            // with a chessboard pattern on the board, 'Result' is the number of boxes on a square of the wrong
            // color; after pushing them to a square of the right color, it always takes an even number of pushes
            // to reach another position with the same black/white distribution, hence, an iterative deepening
            // search for a push-optimal solution can increase the search depth by 2 pushes in each step
            end;
end; {CalculatePushesLowerBound}

{-----------------------------------------------------------------------------}
procedure CalculateReachableSquaresForAllBoxes;
var i:Integer; Dir:TDirection; Distance:TSquareDirectionArrayOfInteger;
begin {note that the player's position is considered, but each box is calculated individually with all other boxes removed from the board}
  with Game do begin
    for i:=1 to BoxCount  do Dec(Board[BoxPos[i]],BOX); {remove all boxes from the board}
    for i:=0 to BoardSize do Board[i]:=Board[i] and (not FLAG_BOX_REACHABLE_SQUARE); {reset old flags, if any}

    CalculateBoxDistanceToAllSquaresForAllDirections(0,True,False,Distance); {initialize 'Distance'}

    for i:=1 to BoxCount do {for each box...}
        CalculateBoxDistanceToAllSquaresForAllDirections(BoxPos[i],False,False,Distance);
    for i:=0 to BoardSize do {for each square...}
        for Dir:=Low(Dir) to High(Dir) do
            if Abs(Distance[i,Dir])<>INFINITY then
               Board[i]:=Board[i] or FLAG_BOX_REACHABLE_SQUARE;

    for i:=1 to BoxCount do Inc(Board[BoxPos[i]],BOX); {put all boxes back on the board}
    end;
end; {CalculateReachableSquaresForAllBoxes}


procedure CalculatePlayerCanReachAllFloorsAroundASingleBoxOnTheBoard(
            var PlayerCanReachAllFloorsAroundBox__ : TBoardOfBoolean );
var Index, NeighborSquare, OldPlayerPos, Square : Integer;
    Direction : TDirection;
begin // calculates for each floor square whether the player can reach all floor
      // squares around a box at the square when the box is the only one on the
      // board
  with Game do begin
    OldPlayerPos := PlayerPos;
    for Index := 1 to BoxCount do
        if IsABoxSquare( BoxPos[ Index ] ) then
           Dec( Board[ BoxPos[ Index ] ], BOX )
        else raise Exception.Create(
                     'Calculate "player can reach all floors ' +
                     'around a single box on the board": ' +
                     InternalErrorText );

    FillChar( PlayerCanReachAllFloorsAroundBox__,
              SizeOf( PlayerCanReachAllFloorsAroundBox__ ), 0 );
    for Square := 0 to BoardSize do
        if IsAFloorSquare( Square ) then begin

           if Square = PlayerPos then begin
              // move the player away from the square, but keep it in the same
              // player access area
              CalculatePlayersReachableSquares( 0, 0 );
              for NeighborSquare := 0 to BoardSize do
                  with SearchStates[ 0 ].PlayersReachableSquares do
                  if ( TimeStamp =  Squares[ NeighborSquare ] ) and
                     ( Square    <> NeighborSquare ) then begin
                     MovePlayer( NeighborSquare );
                     break; // quick-and-dirty exit loop
                     end;
              end;

           Inc( Board[ Square ], BOX );
           CalculatePlayersReachableSquares( 0, 0 );
           Dec( Board[ Square ], BOX );

           PlayerCanReachAllFloorsAroundBox__[ Square ] := True;
           for Direction := Low( Direction ) to High( Direction ) do begin
               NeighborSquare := Square + SquareOffsetForward[ Direction ];
               if IsAFloorSquare( NeighborSquare ) then
                  with SearchStates[ 0 ].PlayersReachableSquares do
                    if Timestamp <> Squares[ NeighborSquare ] then begin
                       PlayerCanReachAllFloorsAroundBox__[ Square ] := False;
                       break; // quick-and-dirty exit loop
                       end;
               end;
           end;

    MovePlayer( OldPlayerPos );
    for Index := 1 to BoxCount do Inc( Board[ BoxPos[ Index ] ], BOX );
    end;
end; {CalculatePlayerCanReachAllFloorsAroundASingleBoxOnTheBoard}

function  CalculatePlayersReachableSquares(Index__,ContinueFromSquare__:Integer):Integer;
type TStackItem=UInt16;
     {the only reason for using compact 16-bit stack items instead of
      machine integers is that otherwise (depending on the board size limit and
      the box count limit) the stack can overflow when the function is called
      via this path:

      CalculateDeadlockSets ->
        PatternTypeFrozenGoals ->
          FindDeadlocks ->
            CalculateDistanceToNearestBoxStartPositionForAllSquares ->
              CalculatePlayersReachableSquares

     }
var  Square,NeighborSquare:Integer; Direction:TDirection;
     StackTop:^TStackItem; Stack:array[0..MAX_BOARD_SIZE] of TStackItem;
begin {Returns the result in the global value 'Game.SearchStates[Index__].PlayersReachableSquares'}
  with Game.SearchStates[Index__].PlayersReachableSquares do begin
    Result:=0;

    if ContinueFromSquare__=0 then begin {'True': full calculation}
       MinPlayerPos:=Game.PlayerPos; ContinueFromSquare__:=MinPlayerPos;

       {inline 'ClearPlayersReachableSquares' for speed}
       if TimeStamp<High(TimeStamp) then
          Inc(TimeStamp)
       else begin {a complete reset is only necessary when the timestamp overflows}
          TimeStamp:=1;
          FillChar(Squares,SizeOf(Squares),0);
          end;
       end
    else begin {continue calculation, e.g., after removing a box}
       end;

    if ContinueFromSquare__<>0 then with Game do begin
       Result:=1; StackTop:=Addr(Stack[1]); StackTop^:=ContinueFromSquare__;
       Squares[ContinueFromSquare__]:=TimeStamp;
       while StackTop<>Addr(Stack[0]) do begin
         {the program spends most of its time in this loop;}
         {using a stack makes the program run at least 10% faster than}
         {a version using a more elegant recursive implementation of the loop}
         Square:=StackTop^; Dec(StackTop); {get next square from the stack}
         for Direction:=Low(Direction) to High(Direction) do begin {examine neighbors to this square}
             NeighborSquare:=Square+SquareOffsetForward[Direction];
             if (Board[NeighborSquare] and (WALL+BOX)=0) and {check for walls, boxes, and visited squares}
                (Squares[NeighborSquare]<>TimeStamp) then begin
                Inc(Result); Inc(StackTop); StackTop^:=NeighborSquare;
                Squares[NeighborSquare]:=TimeStamp;
                if NeighborSquare<MinPlayerPos then MinPlayerPos:=NeighborSquare;
                end;
             end;
         end;
       end;
    end;
end; {CalculatePlayersReachableSquares}

procedure WriteSquaresReachableGoalsToLogFile;
var i,SquareNo,Col,Row,Goal:Integer; s:String; B:TBoard;
begin
  if CreateLogFile(MainForm.ApplicationDataPath+ExtractFileName(Application.ExeName)) then
     with Game do with BoxGoalMatching do
       try
         B:=Board;
         if   Game.ReverseMode then begin
              s:='Reverse mode';
              for i:=1 to BoxCount do
                  Board[BoxPos[i]]:=Board[BoxPos[i]] and (not    SokFile_.BOX);
              for i:=1 to GoalCount do Dec(Board[GoalPos    [i]],SokFile_.GOAL);
              for i:=1 to GoalCount do Inc(Board[StartBoxPos[i]],SokFile_.GOAL);
              end
         else s:='Forward mode';
         WriteBoardToLogFile(Game.Title+' - '+s);
         WritelnToLogFile('');
         if Game.ReverseMode then Board:=B;

         if   Game.Title<>'' then
              s:=Game.Title+' - '
         else s:='';
         WritelnToLogFile(s+'Reachable targets for each square');
         //WritelnToLogFile('Size: '+IntToStr(SizeOf(Game.SquaresReachableGoals)));
         WritelnToLogFile('');

         for SquareNo:=0 to BoardSize do
             if (Board[SquareNo] and FLOOR)<>0 then begin
                SquareToColRow(SquareNo,Col,Row);
                s:=Format('%5d [%3d,%3d] : (%4d) : ',[SquareNo,Col,Row,EdgeCount[SquareNo]]);
                for Goal:=1 to GoalCount do
                    if   Goal in Edges[SquareNo] then s:=s+Format('%4d',[Goal])
                    else s:=s+SPACE+SPACE+SPACE+SPACE;
                WritelnToLogFile(s);
                end;
         WritelnToLogFile('Time: '+IntToStr(TimeMS));
       finally CloseLogFile;
       end;
end;

{
there are 3 versions of 'CalculateReachableGoalsForAllSquares':

  A: recursive version, depth first
  B: 'flat' version, i.e., breadth first using a queue
  C: breadth first like B, minimizing the work by finding goals with identical square-sets

B is on average 5-8% faster than A.

for small levels, the difference between B and C is negligable, but for large
levels with, say, 200 boxes, C can save as much as 90% of the computation time.
}

function CalculateReachableGoalsForAllSquares_A(JobNo__:Int64):Boolean;
var i,j,Goal,OldPlayerPos:Integer; StartTimeMS:TTimeMS; Dir:TDirection;
    Visited:array[0..MAX_BOARD_SIZE,TDirection] of Boolean;

  procedure TrySquare(Square__:Integer);
  var BoxToSquare,PlayerFromSquare,PlayerToSquare:Integer; Dir:TDirection;
      PlayerCanReachNeighborSquare:array[TDirection] of Boolean; p:^TDirection;
      Neighbors:array[0..NUMBER_OF_DIRECTIONS] of TDirection; {at least one extra element so a pointer beyond last used element is legal}
  begin {try to pull the box away from the square}
    if JobNo__=Game.JobNo then with Game do begin
       CalculatePlayersReachableSquares(0,0);
       for Dir:=Low(Dir) to High(Dir) do with Game.SearchStates[0].PlayersReachableSquares do
           PlayerCanReachNeighborSquare[Dir]:=           {save player's reachable squares}
             Squares[Square__+Game.SquareOffsetForward[Dir]]=TimeStamp;
       p:=Addr(Neighbors);

       for Dir:=Low(Dir) to High(Dir) do begin
           BoxToSquare   :=Square__+  Game.SquareOffsetForward[Dir];
           PlayerToSquare:=Square__+2*Game.SquareOffsetForward[Dir];

           if  (not Visited[BoxToSquare,Dir]) and
               PlayerCanReachNeighborSquare[Dir] and
               ((Board[BoxToSquare   ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0) and
               ((Board[PlayerToSquare] and (WALL+BOX                        ))=0) then begin
               Visited[BoxToSquare,Dir]:=True;
               p^:=Dir; Inc(p);
               end;
           end;

       PlayerFromSquare:=PlayerPos; Dec(Board[Square__],BOX);

       while p<>Addr(Neighbors) do begin {visit updated neighbors}
         Dec(p);                         {(updating all neighbors in a breadth-first manner before recursion reduces futile calculations)}
         PlayerPos  :=Square__+2*Game.SquareOffsetForward[p^];
         BoxToSquare:=Square__+  Game.SquareOffsetForward[p^];
         Inc(Board[BoxToSquare],BOX);

         TrySquare(BoxToSquare);

         Dec(Board[BoxToSquare],BOX);
         end;

       PlayerPos:=PlayerFromSquare; Inc(Board[Square__],BOX);
       end;
  end;

begin {CalculateReachableGoalsForAllSquares_A (recursive version, depth first)}
  with Game do with BoxGoalMatching do begin
    StartTimeMS:=GetTimeMS; Calculated:=False; GoalBoxMatching.Calculated:=False;

    FillChar(SubSetCount,SizeOf(SubSetCount),0);
    FillChar(Edges      ,SizeOf(Edges      ),0);
    FillChar(EdgeCount  ,SizeOf(EdgeCount  ),0);

    if GoalCount<=MAX_SET_ITEMS then begin

       OldPlayerPos:=PlayerPos;
       for i:=1 to BoxCount do Dec(Board[BoxPos[i]],BOX);                       {remove all boxes from the board}

       for Goal:=1 to GoalCount do
           if JobNo__=JobNo then begin                                          {'True': the user hasn't abandoned this task}
              for i:=0 to BoardSize do
                  if   (Board[i] and (WALL+FLAG_ILLEGAL_BOX_SQUARE+FLOOR))=FLOOR then
                       for Dir:=Low(Dir) to High(Dir) do Visited[i,Dir]:= False {open squares}
                  else for Dir:=Low(Dir) to High(Dir) do Visited[i,Dir]:= True; {filled squares, e.g., walls}

              for Dir:=Low(Dir) to High(Dir) do begin
                  PlayerPos:=GoalPos[Goal]+SquareOffsetForward[Dir];
                  if not Visited[PlayerPos,Dir] then begin
                     Inc(Board[GoalPos[Goal]],BOX);

                     TrySquare(GoalPos[Goal]);

                     Dec(Board[GoalPos[Goal]],BOX);
                     end;
                  end;

              Visited[GoalPos[Goal],Left]:=True;                                {ensure that the goal square itself is included in the set}

              for i:=0 to BoardSize do
                  if (Board[i] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE+FLOOR))=FLOOR then
                     for Dir:=Low(Dir) to High(Dir) do
                         if Visited[i,Dir] then begin
                            Include(Edges[i],Goal);
                            Inc(EdgeCount[i]);
                            break;
                            end;
              end
           else break; {the user abandoned this task}

       for i:=0 to BoardSize do {map squares with identical goal-sets to the same set-number; used for speeding up comparisons}
           if   JobNo__=JobNo then begin
                EdgeSetNo[i]:=i;
                for j:=Pred(i) downto 0 do
                    if (EdgeSetNo[j]=j) and
                       (EdgeCount[i] =EdgeCount[j]) and
                       (Edges    [i] =Edges    [j]) then begin
                       EdgeSetNo [i]:=EdgeSetNo[j];
                       break;
                       end;
                end
           else break;

       PlayerPos:=OldPlayerPos;                                                 {restore player position}
       for i:=1 to BoxCount do Inc(Board[BoxPos[i]],BOX);                       {put all boxes back on the board}

       Calculated:=JobNo__=JobNo;
       end;

    TimeMS:=CalculateElapsedTimeMS(StartTimeMS,GetTimeMS);

//  WriteSquaresReachableGoalsToLogFile;                                        {for testing}

    Result:=Calculated;
    end;
end; {CalculateReachableGoalsForAllSquares_A}

function CalculateReachableGoalsForAllSquares_B(JobNo__:Int64):Boolean;
type
  TBoardDirectionArrayOfBoolean=array[0..MAX_BOARD_SIZE,TDirection] of Boolean;

var
  i,j,Goal,OldPlayerPos:Integer; StartTimeMS:TTimeMS; Direction:TDirection;
  StartStateVisited,Visited:TBoardDirectionArrayOfBoolean;
(*
  procedure CalculateReachableSquaresFromGoal(GoalNo__:Integer; var Visited__:TBoardDirectionArrayOfBoolean);
  type TQueueItem=record BoxPos,PlayerPos:Integer; end;
  var  BoxPos,NeighborSquare,BoxToSquare,PlayerToSquare,LastBoxPos:Integer;
       Direction:TDirection;
       QueueBottom,QueueTop:^TQueueItem; QueueItems:array[0..MAX_BOARD_SIZE*NUMBER_OF_DIRECTIONS] of TQueueItem;
  begin
    Visited__:=StartStateVisited;

    QueueBottom:=Addr(QueueItems[Low(QueueItems)]); QueueTop:=QueueBottom;

    BoxPos:=Game.GoalPos[GoalNo__]; LastBoxPos:=0;
    for Direction:=Low(Direction) to High(Direction) do begin {initialize the calculation by putting legal start moves on the queue}
        NeighborSquare:=BoxPos+Game.SquareOffsetForward[Direction];
        if not Visited__[NeighborSquare,Direction] then begin {'True': neighbor square is free}
           Visited__[BoxPos,Direction]:=True;
           Inc(QueueTop);
           QueueTop^.BoxPos:=BoxPos; QueueTop^.PlayerPos:=NeighborSquare;
           end;
        end;

    while (QueueBottom<>QueueTop) and (JobNo__=Game.JobNo) do
      with Dead_.Game.SearchStates[0].PlayersReachableSquares do begin
        Inc(QueueBottom);
        BoxPos:=QueueBottom^.BoxPos; Game.PlayerPos:=QueueBottom^.PlayerPos;

        Inc(Game.Board[BoxPos],BOX); {put the box on the board}

        if  (BoxPos<>LastBoxPos) or
            (Squares[Game.PlayerPos]<>TimeStamp) then begin
            {the current set of player's reachable squares isn't valid anymore; recalculate the set}
            LastBoxPos:=BoxPos;
            CalculatePlayersReachableSquares(0,0); {calculate player's reachable squares, i.e., to see which sides of the box the player can reach}
            end;

        for Direction:=Low(Direction) to High(Direction) do begin
            BoxToSquare   :=BoxPos+  Game.SquareOffsetForward[Direction];
            PlayerToSquare:=BoxPos+2*Game.SquareOffsetForward[Direction];

            if  (not Visited[BoxToSquare,Direction]) and
                (Squares[BoxToSquare]=TimeStamp) and
                ((Game.Board[BoxToSquare   ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0) and
                ((Game.Board[PlayerToSquare] and (WALL+BOX                        ))=0) then begin
                Visited[BoxToSquare,Direction]:=True;
                Inc(QueueTop);
                QueueTop^.BoxPos:=BoxToSquare; QueueTop^.PlayerPos:=PlayerToSquare;
                end;
            end;

        Dec(Game.Board[BoxPos],BOX); {remove the box from the board again}

        end;

    Visited__[Game.GoalPos[GoalNo__],Left]:=True; {ensure that the goal square itself is included in the set}

  end; {CalculateReachableSquaresFromGoal}
*)

  procedure CalculateReachableSquaresFromGoal(GoalNo__:Integer; var Visited__:TBoardDirectionArrayOfBoolean);
  type TQueueItem=record BoxPos,PlayerPos:Integer; end;
  var  BoxPos,NeighborSquare,BoxToSquare,PlayerToSquare,LastBoxPos,Offset:Integer;
       Direction:TDirection;
       QueueBottom,QueueTop:^TQueueItem; QueueItems:array[0..MAX_BOARD_SIZE*NUMBER_OF_DIRECTIONS] of TQueueItem;
  begin
    Visited__:=StartStateVisited;

    QueueBottom:=Addr(QueueItems[Low(QueueItems)]); QueueTop:=QueueBottom;

    BoxPos:=Game.GoalPos[GoalNo__]; LastBoxPos:=0;
    for Direction:=Low(Direction) to High(Direction) do begin {initialize the calculation by putting legal start moves on the queue}
        NeighborSquare:=BoxPos+Game.SquareOffsetForward[Direction];
        if not Visited__[NeighborSquare,Direction] then begin {'True': neighbor square is free}
           Visited__[BoxPos,Direction]:=True;
           Inc(QueueTop);
           QueueTop^.BoxPos:=BoxPos; QueueTop^.PlayerPos:=NeighborSquare;
           end;
        end;

    while (QueueBottom<>QueueTop) and (JobNo__=Game.JobNo) do
      with Dead_.Game.SearchStates[0].PlayersReachableSquares do begin
        Inc(QueueBottom);
        BoxPos:=QueueBottom^.BoxPos; Game.PlayerPos:=QueueBottom^.PlayerPos;

        Inc(Game.Board[BoxPos],BOX); {put the box on the board}

        for Direction:=Low(Direction) to High(Direction) do begin
            Offset        :=Game.SquareOffsetForward[Direction];
            BoxToSquare   :=BoxPos+  Offset;
            PlayerToSquare:=BoxPos+2*Offset;

            if  (not Visited[BoxToSquare,Direction]) and
                ((Game.Board[BoxToSquare   ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0) and
                ((Game.Board[PlayerToSquare] and (WALL+BOX                        ))=0) then begin

                if  (BoxPos<>LastBoxPos) or
                    (Squares[Game.PlayerPos]<>TimeStamp) then begin
                    {the current set of player's reachable squares isn't valid anymore; recalculate the set}
                    LastBoxPos:=BoxPos;
                    CalculatePlayersReachableSquares(0,0); {calculate player's reachable squares, i.e., to see which sides of the box the player can reach}
                    end;

                if Squares    [BoxToSquare   ]=TimeStamp then
                   repeat
                     Visited[BoxToSquare,Direction]:=True;
                     Inc(QueueTop);
                     QueueTop^.BoxPos:=BoxToSquare; QueueTop^.PlayerPos:=PlayerToSquare;
                     Inc(BoxToSquare   ,Offset);
                     Inc(PlayerToSquare,Offset);
                   until Visited[BoxToSquare,Direction] or
                         ((Game.Board[BoxToSquare   ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))<>0) or
                         ((Game.Board[PlayerToSquare] and (WALL+BOX                        ))<>0);
                end;
            end;

        Dec(Game.Board[BoxPos],BOX); {remove the box from the board again}

        end;

    Visited__[Game.GoalPos[GoalNo__],Left]:=True; {ensure that the goal square itself is included in the set}

  end; {CalculateReachableSquaresFromGoal}

begin {CalculateReachableGoalsForAllSquares_B (breadth first instead of recursive depth first)}
  with Game do with BoxGoalMatching do begin
    StartTimeMS:=GetTimeMS; Calculated:=False; GoalBoxMatching.Calculated:=False;

    FillChar(SubSetCount,SizeOf(SubSetCount),0);
    FillChar(Edges      ,SizeOf(Edges      ),0);
    FillChar(EdgeCount  ,SizeOf(EdgeCount  ),0);

    if GoalCount<=MAX_SET_ITEMS then begin
       for i:=0 to BoardSize do
           if   (Board[i] and (WALL+FLAG_ILLEGAL_BOX_SQUARE+FLOOR))=FLOOR then
                for Direction:=Low(Direction) to High(Direction) do StartStateVisited[i,Direction]:= False  {open squares}
           else for Direction:=Low(Direction) to High(Direction) do StartStateVisited[i,Direction]:= True;  {filled squares, e.g., walls}

       OldPlayerPos:=PlayerPos;
       for i:=1 to BoxCount do Dec(Board[BoxPos[i]],BOX);                       {remove all boxes from the board}

       for Goal:=1 to GoalCount do
           if JobNo__=JobNo then begin                                          {'True': the user hasn't abandoned this task}
              CalculateReachableSquaresFromGoal(Goal,Visited);

              for i:=0 to BoardSize do
                  if (Board[i] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE+FLOOR))=FLOOR then
                     for Direction:=Low(Direction) to High(Direction) do
                         if Visited[i,Direction] then begin
                            Include(Edges[i],Goal);
                            Inc(EdgeCount[i]);
                            break;
                            end;
              end
           else break; {the user abandoned this task}


       for i:=0 to BoardSize do {map squares with identical goal-sets to the same set-number; used for speeding up comparisons}
           if   JobNo__=JobNo then begin
                EdgeSetNo[i]:=i;
                for j:=Pred(i) downto 0 do
                    if (EdgeSetNo[j]=j) and
                       (EdgeCount[i] =EdgeCount[j]) and
                       (Edges    [i] =Edges    [j]) then begin
                       EdgeSetNo [i]:=EdgeSetNo[j];
                       break;
                       end;
                end
           else break;

       PlayerPos:=OldPlayerPos;                                                 {restore player position}
       for i:=1 to BoxCount do Inc(Board[BoxPos[i]],BOX);                       {put all boxes back on the board}

       Calculated:=JobNo__=JobNo;
       end;

    TimeMS:=CalculateElapsedTimeMS(StartTimeMS,GetTimeMS);

//  WriteSquaresReachableGoalsToLogFile;                                        {for testing}

    Result:=Calculated;
    end;
end; {CalculateReachableGoalsForAllSquares_B}

function CalculateReachableTargetsForAllSquares_C(JobNo__:Int64; ReverseMode__,CalculateReversibleMoves__:Boolean):Boolean;
{ calculates reachable targets for all squares; which target squares depends on
  the parameter 'ReverseMode__';
  ReverseMode__ = 'False': calculates reachable goals                  for all squares in forward mode gameplay
  ReverseMode__ = 'True' : calculates reachable box starting positions for all squares in reverse mode gameplay

  Note that the function uses terms like 'GoalMap' and 'SquareGoalNoMap' where
  'Goal' in reality refers to goal squares or box starting positions depending
  on whether the calculation is performed for forward mode or reverse mode
  gameplay;
}
type
  TBoardDirectionArrayOfBoolean=array[0..MAX_BOARD_SIZE,TDirection] of Boolean;
  TGoalMap=array[0..MAX_SET_ITEMS] of Integer;
var
  i,j,Goal,OldPlayerPos:Integer;
  StartTimeMS:TTimeMS;
  Direction:TDirection;
  StartStateVisited,Visited,IsAReversibleMove:TBoardDirectionArrayOfBoolean;
  GoalMap:TGoalMap;
  SquareGoalNoMap:TBoardOfInteger;

  procedure CalculateReversiblePulls;
  var BoxPos,OppositeNeighbor,PlayerToPos,LastBoxPos,LeftNeighbor,RightNeighbor:Integer;
      Direction,OppositeDirection:TDirection;
  begin {precondition: there are no boxes on the board}
    FillChar(IsAReversibleMove,SizeOf(IsAReversibleMove),0);
    Visited:=StartStateVisited; LastBoxPos:=0;

    for BoxPos:=0 to Game.BoardSize do with Game.SearchStates[0].PlayersReachableSquares do
        for Direction:=Low(Direction) to High(Direction) do
            if not Visited[BoxPos,Direction] then begin
               Game.PlayerPos:=BoxPos+Game.SquareOffsetForward[Direction];
               if (Game.Board[Game.PlayerPos] and (WALL+FLOOR))=FLOOR then begin
                  {the neighboring square in this direction is a floor}

                  Inc(Game.Board[BoxPos],BOX); {put the box on the board}

                  if (BoxPos<>LastBoxPos) or
                     (Squares[Game.PlayerPos]<>TimeStamp) then begin
                     {the current set of player's reachable squares isn't valid anymore; recalculate the set}
                     LastBoxPos:=BoxPos;
                     CalculatePlayersReachableSquares(0,0);
                     end;

                  Dec(Game.Board[BoxPos],BOX); {remove the box from the board again}

                  OppositeDirection:=OPPOSITE_DIRECTION[Direction];
                  OppositeNeighbor :=BoxPos+Game.SquareOffsetForward[OppositeDirection];
                  PlayerToPos      :=OppositeNeighbor+Game.SquareOffsetForward[OppositeDirection];

                  if Squares[OppositeNeighbor]=TimeStamp then begin
                     {the player can reach the opposite side of the box; this also means it's a floor}
                     if OppositeNeighbor<BoxPos then
                        {the opposite move has already been calculated}
                        IsAReversibleMove[BoxPos,Direction]:=IsAReversibleMove[OppositeNeighbor,OppositeDirection]
                     else
                        if ((Game.Board[OppositeNeighbor ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0) and
                           ((Game.Board[PlayerToPos      ] and (WALL+BOX))=0) then begin
                           {the box can move in the opposite direction, try it}
                           Inc(Game.Board[OppositeNeighbor],BOX); {put the box on the board}
                           Game.PlayerPos:=PlayerToPos;
                           CalculatePlayersReachableSquares(1,0);
                           Dec(Game.Board[OppositeNeighbor],BOX); {remove the box from the board again}

                           with Game.SearchStates[1].PlayersReachableSquares do
                             if Squares[BoxPos]=TimeStamp then begin
                                {the player can reach the other side of the box}
                                {after making the opposite move, hence, the}
                                {two moves are 'simple' reversible, meaning the}
                                {box can move back and forth}
                                LeftNeighbor :=OppositeNeighbor+Game.SquareOffsetLeft [Direction]; {caution: assumes 4 directions only}
                                RightNeighbor:=OppositeNeighbor+Game.SquareOffsetRight[Direction];

                                if (((Game.Board[LeftNeighbor ] and WALL)=0)
                                    and
                                    (Squares[LeftNeighbor]<>TimeStamp)
                                   )
                                   or
                                   (((Game.Board[RightNeighbor] and WALL)=0)
                                    and
                                    (Squares[RightNeighbor]<>TimeStamp)
                                   ) then begin
                                   {
                                    there may be an irreversible move around a corner;

                                    an example:
                                    first the position after making the move:

                                    P# <-- player position with box at 'BoxPos' and 'Direction' = 'Up'
                                    B# <-- "B" = 'BoxPos', that is, the box position
                                    __ <-- there is a "hole" in the wall, a "tunnel"
                                    _#

                                    now the position after making the opposite move:

                                    _# <-- "_" = player position with box at 'BoxPos' and 'Direction' = 'Up'
                                    _# <-- "_" = 'BoxPos'
                                    B_ <-- "B" = 'NeighborSquare', with the box after making the opposite move, 'Down'
                                    P# <-- "P" = 'PlayerToSquare' player position after making the opposite move

                                    the backward search is PULLING the box
                                    around on the board, and if the player in
                                    this position cannot reach the position to
                                    the right of the box (inside the "tunnel"),
                                    then PUSHING the box from inside the tunnel
                                    to the left cannot bring the box back to the
                                    starting position, i.e., 'BoxPos'.
                                   }
                                   end
                                else with Game.SearchStates[0].PlayersReachableSquares do begin {else analyze it further}
                                   LeftNeighbor :=BoxPos+Game.SquareOffsetLeft [Direction]; {caution: assumes 4 directions only}
                                   RightNeighbor:=BoxPos+Game.SquareOffsetRight[Direction];

                                   if (((Game.Board[LeftNeighbor ] and WALL)=0)
                                       and
                                       (Squares[LeftNeighbor]<>TimeStamp)
                                      )
                                      or
                                      (((Game.Board[RightNeighbor] and WALL)=0)
                                       and
                                       (Squares[RightNeighbor]<>TimeStamp)
                                      ) then begin
                                      {the starting box position, 'BoxPos', may
                                       suffer from an irreversible move around
                                       a corner, similar to the situation
                                       above with the opposite move
                                      }
                                      end
                                   else begin {else the move is reversible}
                                      IsAReversibleMove[BoxPos          ,Direction        ]:=True;
                                      IsAReversibleMove[OppositeNeighbor,OppositeDirection]:=True;
                                      Visited          [OppositeNeighbor,OppositeDirection]:=True;
                                      end;
                                   end;
                                end;
                           end;
                     end;
                  end;
               end;
  end;

  procedure CalculateReversiblePushes;
  var BoxPos,OppositeNeighbor,PlayerFromPos,LastBoxPos,LeftNeighbor,RightNeighbor,Offset:Integer;
      Direction,OppositeDirection:TDirection;
  begin {precondition: there are no boxes on the board}
    FillChar(IsAReversibleMove,SizeOf(IsAReversibleMove),0);
    Visited:=StartStateVisited; LastBoxPos:=0;

    for BoxPos:=0 to Game.BoardSize do with Game.SearchStates[0].PlayersReachableSquares do
        for Direction:=Low(Direction) to High(Direction) do
            if not Visited[BoxPos,Direction] then begin
               Offset          :=Game.SquareOffsetForward[Direction];
               Game.PlayerPos  :=BoxPos        -Offset; {player square   when   the box has been pushed in this direction to its current square}
               PlayerFromPos   :=Game.PlayerPos-Offset; {player square   before the box was      pushed in this direction to its current square}
               OppositeNeighbor:=BoxPos        +Offset; {the next square when   the box has been pushed in this direction to its current square, opposite the player square}
               if ((Game.Board[Game.PlayerPos  ] and (WALL+FLOOR+FLAG_ILLEGAL_BOX_SQUARE))=FLOOR) and
                  ((Game.Board[PlayerFromPos   ] and (WALL+FLOOR                        ))=FLOOR) and {otherwise, the box cannot be pushed in this direction to its current square}
                  ((Game.Board[OppositeNeighbor] and (WALL+FLOOR                        ))=FLOOR) then begin
                  {the 3 involved neighboring squares along the line are all floors}

                  Inc(Game.Board[BoxPos],BOX); {put the box on the board}

                  if (BoxPos<>LastBoxPos) or
                     (Squares[Game.PlayerPos]<>TimeStamp) then begin
                     {the current set of player's reachable squares isn't valid anymore; recalculate the set}
                     LastBoxPos:=BoxPos;
                     CalculatePlayersReachableSquares(0,0);
                     end;

                  Dec(Game.Board[BoxPos],BOX); {remove the box from the board again}

                  if Squares[OppositeNeighbor]=TimeStamp then begin
                     {the player can reach the opposite side of the box; this also means it's a floor}
                     OppositeDirection:=OPPOSITE_DIRECTION[Direction];
                     if Game.PlayerPos<BoxPos then
                        {the opposite move has already been calculated}
                        IsAReversibleMove[BoxPos,Direction]:=IsAReversibleMove[Game.PlayerPos,OppositeDirection]
                     else begin
                        {the box can move in the opposite direction, try it}
                        Inc(Game.Board[Game.PlayerPos],BOX); {put the box on the board}
                        Game.PlayerPos:=BoxPos;
                        CalculatePlayersReachableSquares(1,0);
                        Game.PlayerPos:=BoxPos-Offset; {restore player position}
                        Dec(Game.Board[Game.PlayerPos],BOX); {remove the box from the board again}

                        with Game.SearchStates[1].PlayersReachableSquares do
                          if Squares[PlayerFromPos]=TimeStamp then begin
                             {the player can reach the other side of the box}
                             {after making the opposite move, hence, the}
                             {two moves are 'simple' reversible, meaning the}
                             {box can move back and forth}
                             LeftNeighbor :=Game.PlayerPos+Game.SquareOffsetLeft [Direction]; {caution: assumes 4 directions only}
                             RightNeighbor:=Game.PlayerPos+Game.SquareOffsetRight[Direction];

                             if (((Game.Board[LeftNeighbor ] and WALL)=0)
                                 and
                                 (Squares[LeftNeighbor]<>TimeStamp)
                                )
                                or
                                (((Game.Board[RightNeighbor] and WALL)=0)
                                 and
                                 (Squares[RightNeighbor]<>TimeStamp)
                                ) then begin
                                {there may be an irreversible move around a
                                 corner; see a corresponding example in
                                 'CalculateReversiblePulls';
                                }
                                end
                             else with Game.SearchStates[0].PlayersReachableSquares do begin {else analyze it further}
                                LeftNeighbor :=BoxPos+Game.SquareOffsetLeft [Direction]; {caution: assumes 4 directions only}
                                RightNeighbor:=BoxPos+Game.SquareOffsetRight[Direction];

                                if (((Game.Board[LeftNeighbor ] and WALL)=0)
                                    and
                                    (Squares[LeftNeighbor]<>TimeStamp)
                                   )
                                   or
                                   (((Game.Board[RightNeighbor] and WALL)=0)
                                    and
                                    (Squares[RightNeighbor]<>TimeStamp)
                                   ) then begin
                                   {the starting box position, 'BoxPos', may
                                    suffer from an irreversible move around
                                    a corner, similar to the situation
                                    above with the opposite move;
                                   }
                                   end
                                else begin {else the move is reversible}
                                   IsAReversibleMove[BoxPos        ,Direction        ]:=True;
                                   IsAReversibleMove[Game.PlayerPos,OppositeDirection]:=True;
                                   Visited          [Game.PlayerPos,OppositeDirection]:=True;
                                   end;
                                end;
                             end;
                        end;
                     end;
                  end;
               end;
  end;

  procedure CalculateReachableSquaresFromTarget(TargetNo__:Integer; ReverseMode__:Boolean);
  type TPathType =(ptReversible,ptIrreversible);
       TQueueItem=record BoxPos:Integer; Direction:TDirection; end;
       PQueueItem=^TQueueItem;
  var  BoxPos,BoxToSquare,PlayerMoveSquare,ItemCount,LastBoxPos,Offset,PushOrPullSign:Integer;
       Direction:TDirection; PathType:TPathType;
       QueueBottom,QueueTop:array[TPathType] of PQueueItem;
       QueueItems:array[0..MAX_BOARD_SIZE*NUMBER_OF_DIRECTIONS+1] of TQueueItem;
  begin
    GoalMap[TargetNo__]        :=TargetNo__; {map this target number to itself}
    Visited:=StartStateVisited; {'StartStateVisited' is already filled with wall squares, hence, it saves a pass though all the squares for filling in the walls}

    QueueBottom[ptReversible  ]:=Addr(QueueItems[Low(QueueItems )]);
    QueueTop   [ptReversible  ]:=QueueBottom[ptReversible  ];
    QueueBottom[ptIrreversible]:=Addr(QueueItems[High(QueueItems)]);
    QueueTop   [ptIrreversible]:=QueueBottom[ptIrreversible];

    if not ReverseMode__ then begin {pull boxes around on the board}
       BoxPos:= Game.GoalPos    [TargetNo__]; PushOrPullSign:= 1;
       end
    else begin {push boxes around on the board}
       BoxPos:= Game.StartBoxPos[TargetNo__]; PushOrPullSign:=-1;
       end;
    LastBoxPos:=0; ItemCount:=0;

    for Direction:=Low(Direction) to High(Direction) do begin
        {initialize the calculation by putting all legal starting moves on the queue}
        Visited[BoxPos,Direction]:=True;

        if IsAFloorSquare(BoxPos+PushOrPullSign*Game.SquareOffsetForward[Direction]) then begin
           Inc(ItemCount);
           if IsAReversibleMove[BoxPos,Direction] then begin
              Inc(QueueTop[ptReversible  ]);
              QueueTop[ptReversible      ]^.BoxPos:=BoxPos;
              QueueTop[ptReversible      ]^.Direction:=Direction;
              end
           else begin
              Dec(QueueTop[ptIrreversible]);
              QueueTop[ptIrreversible    ]^.BoxPos:=BoxPos;
              QueueTop[ptIrreversible    ]^.Direction:=Direction;
              end;
           end;
        end;

    while (ItemCount<>0) and (JobNo__=Game.JobNo) do
      with Dead_.Game.SearchStates[0].PlayersReachableSquares do begin

        Dec(ItemCount); {decrease number of items on the queue}
        {explore reversible paths first in order to discover as many targets as}
        {possible using reversible paths only; this save computation time      }
        {because targets which are reachable with reversible moves have the    }
        {same set of reachable squares;                                        }
        if QueueBottom[ptReversible]<>QueueTop[ptReversible] then begin
           Inc(QueueBottom[ptReversible  ]);
           BoxPos        :=QueueBottom[ptReversible  ]^.BoxPos;
           Direction     :=QueueBottom[ptReversible  ]^.Direction;
           PathType      :=ptReversible;
           end
        else begin
           Dec(QueueBottom[ptIrreversible]);
           BoxPos        :=QueueBottom[ptIrreversible]^.BoxPos;
           Direction     :=QueueBottom[ptIrreversible]^.Direction;
           PathType      :=ptIrreversible;
           end;

        Game.PlayerPos   :=BoxPos+PushOrPullSign*Game.SquareOffsetForward[Direction];

        Inc(Game.Board[BoxPos],BOX); {put the box on the board}

        {generate legal moves from this position}
        for Direction:=Low(Direction) to High(Direction) do begin
            Offset          :=Game.SquareOffsetForward[Direction];
            BoxToSquare     :=BoxPos     +Offset;
            { in forward mode gameplay, the targets are goals, and the reachable
              targets from each square are found by pulling boxes around on the
              board, starting from the goal squares;

              in reverse mode gameplay, the targets are box starting positions,
              and the reachable targets from each square are found by pushing
              boxes around on the board, starting from the box starting
              positions;
            }
            if   not ReverseMode__ then
                 PlayerMoveSquare:=BoxToSquare+Offset  // the player ends at this square if the box is pulled in this direction
            else PlayerMoveSquare:=BoxPos     -Offset; // the player ends at this square if the box is pushed in this direction

            if  (not Visited[BoxToSquare,Direction])
                and
                ((Game.Board[BoxToSquare     ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))=0)
                and
                ((Game.Board[PlayerMoveSquare] and (WALL+BOX                        ))=0)
                then begin

                if (BoxPos<>LastBoxPos) or
                   (Squares[Game.PlayerPos]<>TimeStamp) then begin
                   {the current set of player's reachable squares isn't valid anymore; recalculate the set}
                   LastBoxPos:=BoxPos;
                   CalculatePlayersReachableSquares(0,0);
                   end;

                if (Squares[PlayerMoveSquare]=TimeStamp) then begin {'TimeStamp': the player can reach the square next to the box after/before the move}
                   Visited[BoxToSquare,Direction]:=True;
                   Inc(ItemCount); {enqueue the new successor move}
                   if (PathType=ptReversible) and IsAReversibleMove[BoxToSquare,Direction] then begin
                      if (        SquareGoalNoMap[BoxToSquare ]<>0) and  {'True': 'BoxToSquare' is a target square}
                         (GoalMap[SquareGoalNoMap[BoxToSquare]]= 0) then {'True': this other target hasn't been calculated or mapped to a different target}
                         {mark this other target as having the same set of}
                         {reachable squares as 'TargetNo__'}
                         GoalMap [SquareGoalNoMap[BoxToSquare]]:=TargetNo__;

                      Inc(QueueTop[ptReversible  ]);
                      QueueTop[ptReversible      ]^.BoxPos   :=BoxToSquare;
                      QueueTop[ptReversible      ]^.Direction:=Direction;
                      end
                   else begin
                      Dec(QueueTop[ptIrreversible]);
                      QueueTop[ptIrreversible    ]^.BoxPos   :=BoxToSquare;
                      QueueTop[ptIrreversible    ]^.Direction:=Direction;
                      end;
                   end;
                end;
            end;

        Dec(Game.Board[BoxPos],BOX); {remove the box from the board again}
      end;

    {ensure that the target square itself is included in the set}
    if   not ReverseMode__ then
         Visited[Game.GoalPos     [TargetNo__],Left]:=True
    else Visited[Game.StartBoxPos [TargetNo__],Left]:=True;
  end; {CalculateReachableSquaresFromTarget}

begin {CalculateReachableTargetsForAllSquares_C (breadth first, matching targets with identical square-sets)}
  with Game do begin
    StartTimeMS:=GetTimeMS;
    BoxGoalMatching.Calculated:=False; GoalBoxMatching.Calculated:=False;
    FillChar(BoxGoalMatching,SizeOf(BoxGoalMatching),0);

    if GoalCount<=MAX_SET_ITEMS then begin
       FillChar(GoalMap                   ,SizeOf(GoalMap                ),0);
       FillChar(SquareGoalNoMap           ,SizeOf(SquareGoalNoMap        ),0);

       for i:=0 to BoardSize do
           if   (Board[i] and (WALL+FLAG_ILLEGAL_BOX_SQUARE+FLOOR))=FLOOR then
                for Direction:=Low(Direction) to High(Direction) do StartStateVisited[i,Direction]:= False  {open squares}
           else for Direction:=Low(Direction) to High(Direction) do StartStateVisited[i,Direction]:= True;  {filled squares, e.g., walls}

       if   not ReverseMode__ then
            for i:=1 to GoalCount do SquareGoalNoMap[GoalPos     [i]]:=i
       else for i:=1 to GoalCount do SquareGoalNoMap[StartBoxPos [i]]:=i;

       OldPlayerPos:=PlayerPos;
       for i:=1 to BoxCount do Dec(Board[BoxPos[i]],BOX);                       {remove all boxes from the board}

       if   CalculateReversibleMoves__ then
            if   not ReverseMode__ then
                 CalculateReversiblePulls
            else CalculateReversiblePushes
       else FillChar(IsAReversibleMove,SizeOf(IsAReversibleMove),0);

       for Goal:=1 to GoalCount do
           if JobNo__=JobNo then                                                {'True': the user hasn't abandoned this task}
              if GoalMap[Goal]=0 then begin                                     {'True': the goal didn't share square-set with a previously calculated goal}
                 CalculateReachableSquaresFromTarget(Goal,ReverseMode__);

                 for i:=0 to BoardSize do
                     if (Board[i] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE+FLOOR))=FLOOR then
                        for Direction:=Low(Direction) to High(Direction) do
                            if Visited[i,Direction] then begin
                               Include(BoxGoalMatching.Edges    [i],Goal);
                               Inc    (BoxGoalMatching.EdgeCount[i]);
                               break;
                               end;
                 end
              else begin {this goal has the same set of reachable squares as goal number 'GoalMap[Goal]}
                 j:=GoalMap[Goal];
                 for i:=0 to BoardSize do
                     if j in BoxGoalMatching.Edges[i] then begin
                        Include(BoxGoalMatching.Edges    [i],Goal);
                        Inc    (BoxGoalMatching.EdgeCount[i]);
                        end;
                 end
           else break; {the user abandoned this task}

       for i:=0 to BoardSize do {map squares with identical goal-sets to the same set-number; used for speeding up comparisons}
           if   JobNo__=JobNo then with BoxGoalMatching do begin
                EdgeSetNo[i]:=i;
                for j:=Pred(i) downto 0 do
                    if (EdgeSetNo[j]=j) and
                       (EdgeCount[i] =EdgeCount[j]) and
                       (Edges    [i] =Edges    [j]) then begin
                       EdgeSetNo [i]:=EdgeSetNo[j];
                       break;
                       end;
                end
           else break;

       PlayerPos:=OldPlayerPos;                                                 {restore player position}
       for i:=1 to BoxCount do Inc(Board[BoxPos[i]],BOX);                       {put all boxes back on the board}

       BoxGoalMatching.Calculated:=JobNo__=JobNo;
       end;

    BoxGoalMatching.TimeMS:=CalculateElapsedTimeMS(StartTimeMS,GetTimeMS);

{
    if ReverseMode__ then begin
       Game.LogFileEnabled:=True;
       Game.ReverseMode:=ReverseMode__;
       WriteSquaresReachableGoalsToLogFile;
       Game.LogFileEnabled:=False;
       end;
}
    Result:=BoxGoalMatching.Calculated;
    end;
end; {CalculateReachableGoalsForAllSquares_C}

procedure TestReachableGoalsFromEachSquare;
const
  LEVEL_FILE_NAMES: array[0..7] of String = (
    '',
    'YASGen.sok',
    'Yoshio.sok',
    'du Peloux.sok',
    'Haywood.sok',
    'Holland.sok',
    'GRIGoRusha.sok',
    'Skinner.sok'
    );

var
  i,FileIndex,LevelCount:Integer;
  t1,t2,t3,Total1,Total2,Total3:TTimeMS;
  b:Boolean; PathName,s:String;
  Level:TLevel;
  LogFile:TextFile;

  function  Test(var t1__,t2__,t3__:TTimeMS; var ErrorStr__:String):Boolean;
  var ErrorCount:Integer; BGM:TBoxGoalMatching;

    procedure Check;
    var i,Col,Row:Integer;
    begin
      with Dead_.Game do with BoxGoalMatching do
        if Calculated then begin
           if BoardSize=0 then Result:=False;
           for i:=0 to BoardSize do begin
               if Edges[i]<>BGM.Edges[i] then begin
                  SquareToColRow(i,Col,Row);
                  if Length(ErrorStr__)<500 then
                     ErrorStr__:=ErrorStr__+
                                 'Square '+IntToStr(i)+SPACE+IntToStr(Col)+COLON+IntToStr(Row)+NL+
                                           GoalSetToText(BGM.Edges[i])+NL+
                                           GoalSetToText(    Edges[i])+NL+NL;
                  Result:=False; Inc(ErrorCount);
                  end;
               if EdgeCount[i]<>BGM.EdgeCount[i] then begin
                  if Length(ErrorStr__)<500 then
                     ErrorStr__:=ErrorStr__+'Goal count for square '+IntToStr(i)+' differs'+NL;
                  Result:=False; Inc(ErrorCount);
                  end;
               if EdgeSetNo[i]<>BGM.EdgeSetNo[i] then begin
                  if Length(ErrorStr__)<500 then
                     ErrorStr__:=ErrorStr__+'Goal set-no for square '+IntToStr(i)+' differs'+NL;
                  Result:=False; Inc(ErrorCount);
                  end;
               end;
           end;
    end;

  begin {Test}
    Result:=True; ErrorStr__:=''; ErrorCount:=0;
    with Dead_.Game do begin
      CalculateReachableGoalsForAllSquares_A(Game.JobNo);
      t1__:=Game.BoxGoalMatching.TimeMS;
      BGM:=Game.BoxGoalMatching;

      FillChar(BoxGoalMatching,SizeOf(BoxGoalMatching),0);
      CalculateReachableGoalsForAllSquares_B(Game.JobNo);
      t2__:=Game.BoxGoalMatching.TimeMS;
      Check;
{
      FillChar(BoxGoalMatching,SizeOf(BoxGoalMatching),0);
      CalculateReachableTargetsForAllSquares_C(Game.JobNo,True,True);
      t1__:=Game.BoxGoalMatching.TimeMS;

      FillChar(BoxGoalMatching,SizeOf(BoxGoalMatching),0);
      CalculateReachableTargetsForAllSquares_C(Game.JobNo,False,False);
      t2__:=Game.BoxGoalMatching.TimeMS;
      BGM:=Game.BoxGoalMatching;
}
      FillChar(BoxGoalMatching,SizeOf(BoxGoalMatching),0);
      CalculateReachableTargetsForAllSquares_C(Game.JobNo,False,True);
      t3__:=Game.BoxGoalMatching.TimeMS;
      Check;

      if not Result then ErrorStr__:=ErrorStr__+' ('+IntToStr(ErrorCount)+' errors)';
      end;
  end; {Test}

begin {TestReachableGoalsFromEachSquare}
  LevelCount:=0; Total1:=0; Total2:=0; Total3:=0;
  PathName:=StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath+DEFAULT_LEVEL_DIRECTORY);
  Assign(LogFile,PathName+'Log.txt'); Rewrite(LogFile);
  MainForm.Deadlocks.Suspend;

  //Dead_.Game.LogFileEnabled:=True;
  //CreateLogFile(MainForm.ApplicationDataPath+ExtractFileName(Application.ExeName));

  try
    for FileIndex:=1 to High(LEVEL_FILE_NAMES) do with MainForm do begin
        if SokoFile.LoadFromFile(PathName+LEVEL_FILE_NAMES[FileIndex]) then begin
           Level:=TLevel(SokoFile.Levels.GetItemByIndex(0));
           while Level<>nil do begin
             s:=MakeIniFileSectionFileName(SokoFile.Name,Level.Name);
             Status.Hint:=StrWithoutBrackets(ExtractFileName(s)); Application.ProcessMessages;
             if Game.LoadFromFileOrClipboard(s,nil,nil,b) then begin
                Level:=SokoFile.GetLevelByName(Game.Name);
                if Level<>nil then begin
                   Deadlocks.LoadGame(False,False);
                   Inc(LevelCount);
                   if   Test(t1,t2,t3,s) then begin

                        s:=Level.Name;
                        if AnsiPos(COMMA,s)<>0 then s:=Copy(s,Succ(AnsiPos(COMMA,s)),MaxInt);
                        Writeln(LogFile,Format('%5d %20s %10d %10d %10d',[LevelCount,Copy(s,1,20),t1,t2,t3]));
                        Inc(Total1,t1); Inc(Total2,t2); Inc(Total3,t3);

                        Level:=TLevel(Level.Next);
                        //Level:=nil;
                        end
                   else begin
                     Msg('Calculation mismatch: '+NL+s,Level.Name,MB_OK); Level:=nil;
                     end;
                   end
                else Msg('Internal error',Game.Name,MB_OK);
                Game.Clear;
                end
             else begin
                Error('Level not found: '+s,'');
                Level:=nil;
                end;
             end;
           end
        else Error('File not found: '+LEVEL_FILE_NAMES[FileIndex],'');
        end;

    for i:=1 to 70 do Write(LogFile,'-'); Writeln(LogFile);
    Writeln(LogFile,Format('%37d %10d %10d',[Total1,Total2,Total3]));

    Msg('Levels: '+IntToStr(LevelCount),'',MB_OK);
  finally
    MainForm.Deadlocks.Resume; {if the thread really is working, then it has garbage data now}
    CloseFile(LogFile);
    //CloseLogFile;
  end;
end; {TestReachableGoalsFromEachSquare}

{-----------------------------------------------------------------------------}

{Board}

procedure BoardToText(var BoardAsText__:TBoardAsText);
var Col,Row:Integer;
begin
  with Game do begin
    for Row:=1 to BoardHeight do begin
        BoardAsText__[Row]:='';
        for Col:=1 to BoardWidth do
            {$WARNINGS OFF}
              BoardAsText__[Row]:=BoardAsText__[Row]+ {Implicit string cast from 'ShortString' to 'String'}
                SquareToChar(ColRowToSquare(Col,Row));
            {$WARNINGS ON}
        end;
    end;
end;

function CalculateBoxSubSetCount(BoxNo__:Integer):Integer;
var i,BoxEdgeSetNo,BoxSquareNo,SquareNo:Integer;
begin {calculates the number of boxes with a goal-set which is a sub-set of the goals reachable for the box 'BoxNo__', including 'Self'}
  with Game do with BoxGoalMatching do begin
    Result      :=0;
    BoxSquareNo :=BoxPos[BoxNo__];
    BoxEdgeSetNo:=EdgeSetNo[BoxSquareNo];
    for i:=1 to BoxCount do begin
        SquareNo:=BoxPos[i];                                          {inline 'SquaresGoalSetIsASubSet()' for speed}
        if ((EdgeCount[SquareNo]                      <>0))           {empty sets don't count}
           and
           ((EdgeSetNo[SquareNo]                      = BoxEdgeSetNo) {'=': quick test for identical sets}
            or
            (Edges    [SquareNo] * Edges[BoxSquareNo] = Edges[SquareNo])
           ) then
           Inc(Result);
        end;
    end;
end;

function CalculateHashValue:THashValue;
var i:Integer;
begin {player's position isn't considered, it's saved separately in each node}
  Result:=0;
  for i:=1 to Game.BoxCount do
      Result:=Result xor Game.SquareHashValues[Game.BoxPos[i]];
end;

function CalculateGoalSubSetCount(GoalNo__:Integer):Integer;
var i:Integer;
begin {calculates the number of goals with a box-set which is a sub-set of the boxes that can reach the goal 'GoalNo__', including 'Self'}
  Result:=0;
  with Game do with GoalBoxMatching do
    for i:=1 to GoalCount do {inline 'GoalsBoxSetIsASubSet()' for speed}
        if ((EdgeCount[i]<>0)) {empty sets don't count}
           and
           ((Edges[i] * Edges[GoalNo__]) = Edges[i]) then
           Inc(Result);
end;

function CalculateSimpleLowerBound:Integer;
var i,Distance:Integer;
begin {calculates distance to nearest target for all boxes}
  Result:=0;
  for i:=1 to Game.BoxCount do begin
      Distance:=Game.DistanceToNearestTarget[Game.BoxPos[i]];
      if   Result<INFINITY then Inc(Result,Distance)
      else Inc(Result); {the result is meaningless if a box cannot reach a goal}
      end;
end;

procedure ClearBoard; {remove boxes and player from the board}
var i,j,SquareNo:Integer;
begin
  for i:=1 to Game.BoxCount do with Game.DeadlockSets do begin
      SquareNo:=Game.BoxPos[i];
      if SquareNo<>0 then begin
         Dec(Game.Board[SquareNo],BOX); {remove the box from the board}
         for j:=1 to SquareSetCount[SquareNo] do Inc(Capacity[SquareSetNumbers[SquareNo,j]]); {update deadlock set capacities}
         end;
      end;
  Game.BoxCount:=0; MovePlayer(0); Game.HashValue:=0; Game.SimpleLowerBound:=0;
end;

procedure InitializeBoard(StartingFromBoxNo__:Integer); {update board and deadlock capacities, using 'Game.BoxPos' and 'Game.PlayerPos'}
var i,j,SquareNo:Integer;
begin
  for i:=StartingFromBoxNo__ to Game.BoxCount do with Game.DeadlockSets do begin
      SquareNo:=Game.BoxPos[i];
      Inc(Game.Board[SquareNo],BOX); {put the box on the board}
      for j:=1 to SquareSetCount[SquareNo] do Dec(Capacity[SquareSetNumbers[SquareNo,j]]); {update deadlock set capacities}
      end;
  MovePlayer(Game.PlayerPos);
  Game.SimpleLowerBound:=CalculateSimpleLowerBound;
  Game.HashValue:=CalculateHashValue;
end;

procedure ShowBoard;
var Row:Integer; BoardAsText:TBoardAsText;
begin
  BoardToText(BoardAsText);
  for Row:=1 to Game.BoardHeight do
      Writeln(BoardAsText[Row]);
end;
(*
procedure ShowDeadlockSet(Index__,CenterSquareNo__:Integer);
var i,j,Col,Row:Integer; s:String; Direction:TDirection; B:TBoard;
begin {$I-}
  if Game.ShowDeadlockSetsEnabled or (Game.LogFileName<>'') then
     with Game do with DeadlockSets do
       if Game.ShowDeadlockSetsEnabled or (Game.LogFileName<>'') then begin
          if   Game.Title<>'' then
               s:=Game.Title+' - '
          else s:='';
          s:=s+'Deadlock Set '+IntToStr(RunNo[Index__])+'  Capacity: '+IntToStr(Capacity[Index__])+'  Squares:';
          B:=Board;
          for i:=0 to BoardSize do
              if (Board[i] and WALL)=0 then Board[i]:=FLOOR;
          for i:=0 to BoardSize do
              for j:=1 to SquareSetCount[i] do
                  if SquareSetNumbers[i,j]=Index__ then begin
                     s:=s+SPACE+IntToStr(i);
                     Board[i]:=FLAG_SQUARE_SET;
                     if (B[i] and BOX   )<>0 then Inc(Board[i],BOX);
                     if (B[i] and GOAL  )<>0 then Inc(Board[i],GOAL);
                     if (B[i] and PLAYER)<>0 then Inc(Board[i],PLAYER);
                     end;

          if CenterSquareNo__<>0 then begin
             SquareToColRow(CenterSquareNo__,Col,Row);
             s:=s+'  Center: '+IntToStr(CenterSquareNo__)+EQUAL+LEFT_BRACKET+IntToStr(Col)+COMMA+IntToStr(Row)+RIGHT_BRACKET;
             end;

          if dsfPlayerMustBeOutsideSet in Flags[Index__] then begin
             if   CenterSquare[Index__]<>0 then
                  s:=s+'  Player must be outside the set and push the last box:'
             else s:=s+'  Player must push the last box:';
             for Direction:=Low(Direction) to High(Direction) do
                 if DIRECTION_TO_DEADLOCK_SET_FLAG[Direction] in Flags[Index__] then
                    s:=s+SPACE+DIRECTION_TO_TEXT[Direction];
             end;

          if Game.LogFileName<>'' then begin
             WriteBoardToLogFile(s);
             if (not (dsfPlayerIsInsideSet in Flags[Index__])) and (Capacity[Index__]<0) then
                Writeln(Game.LogFile,'*** Deadlock ***');
             if IOResult<>0 then;
             end;

          if Game.ShowDeadlockSetsEnabled then begin
             Writeln; Writeln(s);
             ShowBoard;
             if (not (dsfPlayerIsInsideSet in Flags[Index__])) and (Capacity[Index__]<0) then
                Writeln('*** Deadlock ***');
             Write(PressENTERText); Readln;
             end;

          Board:=B;
          end;
end; {$I+}
*)
procedure ShowDeadlockSet(Index__,CenterSquareNo__:Integer);
var i,j,Col,Row:Integer; s:String; Direction:TDirection; B:TBoard;
begin {$I-}
  if Game.ShowDeadlockSetsEnabled or (Game.LogFileName<>'') then
     with Game do with DeadlockSets do
       if Game.ShowDeadlockSetsEnabled or (Game.LogFileName<>'') then begin
          if   Game.Title='' then
               s:=''
          else s:=Game.Title+' - ';
          s:=s+'Deadlock Set '+IntToStr(RunNo[Index__])+'  Capacity: '+IntToStr(Capacity[Index__])+'  Squares:';
          B:=Board;
          for i:=0 to BoardSize do
              if (Board[i] and WALL)=0 then Board[i]:=FLOOR;
          for i:=0 to BoardSize do
              for j:=1 to SquareSetCount[i] do
                  if SquareSetNumbers[i,j]=Index__ then begin
                     s:=s+SPACE+IntToStr(i);
                     Board[i]:=FLAG_SQUARE_SET;
                     if ((B[i] and BOX   )<>0)
                        //and (Index__<=Game.DeadlockSets.PrecalculatedSetsCount)
                        then
                        Inc(Board[i],BOX);
                     if (B[i] and GOAL  )<>0 then Inc(Board[i],GOAL);
                     if (B[i] and PLAYER)<>0 then Inc(Board[i],PLAYER); {the player is back at its starting position also when deadlocks are added dynamically during the search, hence, it's OK to depict the player position}
                     end;
          if CenterSquareNo__<>0 then begin
             SquareToColRow(CenterSquareNo__,Col,Row);
             s:=s+'  Center: '+IntToStr(CenterSquareNo__)+EQUAL+LEFT_BRACKET+IntToStr(Col)+COMMA+IntToStr(Row)+RIGHT_BRACKET;
             end;

          if (dsfPlayerMustBeOutsideSet in Flags[Index__]) and
             (not (dsfControllerSet in Flags[Index__])) and
             (not (dsfFreezeSet in Flags[Index__])) then begin
             if   CenterSquare[Index__]<>0 then
                  s:=s+'  The player must be outside the set. This is guaranteed if the last push direction is:'
             else s:=s+'  The player must push the last box:';
             for Direction:=Low(Direction) to High(Direction) do
                 if DIRECTION_TO_DEADLOCK_SET_FLAG[Direction] in Flags[Index__] then
                    s:=s+SPACE+DIRECTION_TO_TEXT[Direction];
             end;

          if (dsfControllerSet         in Flags[Index__]) {and (Index__<Count) and (dsfFreezeSet     in Flags[Succ(Index__)])}
             then begin
             s:=s+'  This is a controller set for a controller/freeze set pair. The squares in this set must contain at least '+IntToStr(Capacity[Index__])+' box(es) before other boxes freeze at the squares in the following set.';
             end;
          if (dsfFreezeSet             in Flags[Index__])  and (Index__>0    ) and (dsfControllerSet in Flags[Pred(Index__)])  and
             (not (dsfTestForFreezingSquare in Flags[Index__])) then begin
             s:=s+'  This is a freeze set in a controller/freeze set pair. The squares in this set must not be filled before the squares in the preceding controller set contain enough boxes.';
             end;
          if (dsfTestForFreezingSquare in Flags[Index__])  and (Index__>0    ) and (dsfControllerSet in Flags[Pred(Index__)])  then begin
             s:=s+'  This is a freeze set in a controller/freeze set pair. Boxes at these squares must not freeze before the squares in the preceding controller set contain enough boxes.';
             end;

          if Game.LogFileName<>'' then begin
             WriteBoardToLogFile(s);
             if (not (dsfPlayerIsInsideSet in Flags[Index__])) and
                (Capacity[Index__]<0)
                //and (Index__<=PrecalculatedSetsCount)
                and
                (not (dsfControllerSet in Flags[     Index__ ])) and
                (not (dsfControllerSet in Flags[Pred(Index__)]))
                then
                Writeln(Game.LogFile,'*** Deadlock ***');
             if IOResult<>0 then;
             end;

          {$IFDEF CONSOLE_APPLICATION}
            if Game.ShowDeadlockSetsEnabled then begin
               Writeln; Writeln(s);
               ShowBoard;
               if (not (dsfPlayerIsInsideSet in Flags[Index__])) and (Capacity[Index__]<0) and (Index__<=PrecalculatedSetsCount) then
                  Writeln('*** Deadlock ***');
               //Write(TEXT_PRESS_ENTER); Readln;
               end;
          {$ENDIF}

          Board:=B;
          end;
end; {$I+}

procedure ShowIllegalBoxSquares;
var i:Integer; s:String; B:TBoard;
begin
  if Game.ShowDeadlockSetsEnabled or (Game.LogFileName<>'') then
     with Game do with DeadlockSets do begin
       B:=Board;

       if   Game.Title<>'' then
            s:=Game.Title+' - '
       else s:='';
       s:=s+'Simple illegal squares';
       
       for i:=0 to BoardSize do
           if (Board[i] and WALL)=0 then
              if   (Board[i] and FLAG_ILLEGAL_BOX_SQUARE)<>0 then
                   Board[i]:=FLAG_SQUARE_SET
              else Board[i]:=FLOOR;

       if Game.LogFileName<>'' then WriteBoardToLogFile(s);

       if Game.ShowDeadlockSetsEnabled then begin
          Writeln; Writeln(s);
          ShowBoard;
          Readln;
          end;

       Board:=B;
       end;
end;

{-----------------------------------------------------------------------------}

{Game}

procedure InitializeGame(BoardWidth__,BoardHeight__:Integer; const Board__:SokFile_.TBoard; const Title__:String);
var i,j:Integer;

  procedure FindBoxesAndGoalsAndPlayer;
  var i:Integer;
  begin {precondition: the board has been initialized by calling 'InitializeBoard'}
    with Game do begin
      PlayerPos:=0; BoxCount:=0; GoalCount:=0;
      for i:=0 to BoardSize do begin
          if   (Board[i] and BOX)<>0 then
               if   BoxCount<MAX_BOXES then begin
                    Inc(BoxCount);
                    BoxPos[BoxCount]:=i;
                    end
               else Dec(Board[i],BOX);
          if   (Board[i] and GOAL)<>0 then
               if   GoalCount<MAX_BOXES then begin
                    Inc(GoalCount);
                    GoalPos[GoalCount]:=i;
                    end
               else Dec(Board[i],GOAL);
          if   (Board[i] and PLAYER)<>0 then
               if   PlayerPos=0 then
                    PlayerPos:=i
               else Dec(Board[i],PLAYER);
          Board[i]:=Board[i] and BOARD_PIECES; {reset old flags, if any}
          end;

      if BoxCount<>GoalCount then begin
         for i:=Succ(Min(BoxCount,GoalCount)) to BoxCount  do Dec(Board[BoxPos [i]],BOX);
         for i:=Succ(Min(BoxCount,GoalCount)) to GoalCount do Dec(Board[GoalPos[i]],GOAL);
         BoxCount:=Min(BoxCount,GoalCount); GoalCount:=BoxCount;
         end;
      end;
  end; {FindBoxesAndGoalsAndPlayer}

  procedure FillTubes;
  var i,j,{BoxOnGoalCount,}FloorCount,NewBoxPos,NewPlayerPos:Integer;
      DeadEnd,More:Boolean; Dir,Direction:TDirection;
  begin {precondition: positions of player and boxes have been calculated by 'FindBoxesAndGoalsAndPlayer'}
    with Game do begin
      //BoxOnGoalCount:=0;
      Direction:=Up;
      //TubeFillingMoveCount:=0; TubeFillingPushCount:=0; {tubefilling player moves, box pushes}
      TubeFillingSquareCount:=0; {player moves, box pushes, and squares}
      //for i:=1 to BoxCount do
      //    if IsAGoalSquare(BoxPos[i]) then Inc(BoxOnGoalCount);

      repeat
        More:=False;
        for i:=0 to BoardSize do
            if (Board[i] and (WALL+BOX+GOAL))=0 then begin {for each non-goal, non-box floor-square...}
               FloorCount:=0;
               for Dir:=Low(Dir) to High(Dir) do
                   if IsAFloorSquare(i+SquareOffsetForward[Dir]) then begin
                      Inc(FloorCount); Direction:=Dir;
                      end;
               if FloorCount<=1 then begin {the floor-square is surrounded by 3 or 4 walls}
                  DeadEnd:=True; NewBoxPos:=0; NewPlayerPos:=0;
                  if i=PlayerPos then
                     if FloorCount=0 then
                        DeadEnd:=False {the player is on an isolated square, don't change anything}
                     else begin        {try to move the player to the neighbor floor-square}
                        NewPlayerPos:=i+SquareOffsetForward[Direction];
                        if IsABoxSquare(NewPlayerPos) then begin
                           NewBoxPos:=NewPlayerPos+SquareOffsetForward[Direction];
                           //DeadEnd:=((Board[NewBoxPos] and (WALL+BOX))=0) and {the box can be pushed forward}
                           //         (BoxOnGoalCount<BoxCount) and             {the position isn't a solution}
                           //         (History.Count<High(History.Moves));      {the game history isn't full}

                           {this program cannot allow the tube-filler to move}
                           {boxes because the human player still works on the}
                           {original position, hence, deadlock-sets could block}
                           {the first pushes}
                           DeadEnd:=False;
                           end;
                        end;
                  if DeadEnd then begin {dead-end: place a wall on the square}
                     if NewBoxPos<>0 then begin {move a box}
                        for j:=1 to BoxCount do {find the box number}
                            if BoxPos[j]=NewPlayerPos then begin
                               //if IsAGoalSquare(BoxPos[j]) then Dec(BoxOnGoalCount);
                               Dec(Board[BoxPos[j]],BOX); BoxPos[j]:=NewBoxPos;
                               Inc(Board[BoxPos[j]],BOX);
                               //if IsAGoalSquare(BoxPos[j]) then Inc(BoxOnGoalCount);
                               //Inc(TubeFillingPushCount);
                               //Inc(History.Count);
                               //History.Moves[History.Count].BoxNo:=j;
                               //History.Moves[History.Count].Direction:=Direction;
                               break;
                               end;
                        end;
                     if NewPlayerPos<>0 then begin {move the player}
                        Dec(Board[PlayerPos],PLAYER); PlayerPos:=NewPlayerPos;
                        Inc(Board[PlayerPos],PLAYER);
                        //Inc(TubeFillingMoveCount);
                        end;
                     Board[i]:=WALL; Inc(TubeFillingSquareCount); More:=True;
                     end;
                  end;
               end;
      until not More;
      end;
  end; {FillTubes}

  procedure InitializeBoard(BoardWidth__,BoardHeight__:Integer; const Board__:SokFile_.TBoard);
  var i,Col,Row,RowOffset:Integer; Direction:TDirection;
  begin
    with Game do begin
      BoardWidth:=BoardWidth__; BoardHeight:=BoardHeight__;
      BoardSize:=(BoardWidth__+2)*(BoardHeight__+2); {the extra 2 is for a wall-filled border}
      {left-justify the board so it can be treated as a 1-dimensional vector without gaps}
      for Row:=1 to Game.BoardHeight do begin
          RowOffset:=ColRowToSquare(0,Row);
          for Col:=1 to Game.BoardWidth do
              Game.Board[RowOffset+Col]:=Board__[Col,Row] and (BOARD_PIECES); {'and': remove main-program flags, if any}
          end;

      SquareOffsetForward[Up   ]:=-(BoardWidth__+2);
      SquareOffsetForward[Left ]:=-1;
      SquareOffsetForward[Down ]:=+(BoardWidth__+2);
      SquareOffsetForward[Right]:=+1;

      for Direction:=Low(Direction) to Pred(High(Direction)) do
          SquareOffsetLeft[Direction]:=SquareOffsetForward[Succ(Direction)];
      SquareOffsetLeft[High(Direction)]:=SquareOffsetForward[Low(Direction)];

      for Direction:=Succ(Low(Direction)) to High(Direction) do
          SquareOffsetRight[Direction]:=SquareOffsetForward[Pred(Direction)];
      SquareOffsetRight[Low(Direction)]:=SquareOffsetForward[High(Direction)];

      {add a wall-filled border to ensure that the player always is surrounded by walls}
      for i:=0 to BoardWidth__+1 do Board[i]:=WALL; {top row}
      for i:=BoardSize downto BoardSize-BoardWidth__-2 do Board[i]:=WALL; {bottom row}
      RowOffset:=0;
      for i:=1 to BoardHeight__ do begin
          Inc(RowOffset,BoardWidth__+2);
          Board[RowOffset]:=WALL; Board[RowOffset+BoardWidth__+1]:=WALL; {left and right columns}
          end;
      end;
  end; {InitializeBoard}

begin {InitializeGame}
  Game.Title:=Title__;
  InitializeBoard(BoardWidth__,BoardHeight__,Board__);
  FindBoxesAndGoalsAndPlayer;
  Game.OriginalBoard:=Game.Board; Game.OriginalBoxPos:=Game.BoxPos; Game.OriginalPlayerPos:=Game.PlayerPos;
  FillTubes;
  Game.StartPlayerPos:=Game.PlayerPos; Game.StartBoxPos:=Game.BoxPos;
  Game.ReverseMode:=False;
  Game.BoxGoalMatching.Calculated:=False; Game.GoalBoxMatching.Calculated:=False;
  if Game.SquareHashValues[0]=0 then with Game do begin {initialize the hash square values}
     RandSeed:=1;
     for i:=0 to 255 do if Random(MaxInt)=0 then; {warm up the random number generator}
     for i:=Low(SquareHashValues) to High(SquareHashValues) do
         repeat
           SquareHashValues[i]:=
             Abs(Abs((THashValue(Random(MaxInt)) shl 48))+
                 Abs((THashValue(Random(MaxInt)) shl 32))+
                 Abs((THashValue(Random(MaxInt)) shl 16))+
                 Abs((THashValue(Random(MaxInt)) shl 8 ))+
                 Abs((THashValue(Random(MaxInt)))));
           for j:=0 to Pred(i) do
               if SquareHashValues[j]=SquareHashValues[i] then
                  SquareHashValues[i]:=0;
         until SquareHashValues[i]>MaxInt;
     end;
end; {InitializeGame}

function NewJobNo:Integer;
begin
  if   Dead_.Game.JobNo<High(Dead_.Game.JobNo) then
       Inc(Dead_.Game.JobNo)
  else Dead_.Game.JobNo:=Low(Dead_.Game.JobNo); {wrap around}
  Result:=Dead_.Game.JobNo;
end;

{-----------------------------------------------------------------------------}

{Calculate deadlock sets}

function CalculateDeadlockSets:Boolean; {returns 'False' if start position is a deadlock}
{
Deadlock Sets

--------
Legend:
--------
# :	A wall
- :	A 'simple-illegal-box-square'.
	They are marked directly in the board squares because
	this is faster than using deadlock-sets.
% :	A square in the set, it may or may not be a goal square.
? :     A square in the set unless the square is a wall.
$ :     A box
. :     A goal square
* :     A box at a goal square
X :	A non-empty square
--------

The program only uses precalculated deadlock sets where the player's position
doesn't matter, or can be deduced from the last push direction. This way, it
completely avoids all timeconsuming and complicated pattern matching during the
solving process, and bookkeeping and checking for deadlocks is merely a question
about simple capacity counting.

Finding deadlock sets is a part of the level pre-processing, i.e., it's not
time-critical since it doesn't affect the solver. This means the program can
spend a reasonable amount of time finding quite complicated deadlock situations.


--------
01              Non-goal corner
                ===============
 #              A non-goal corner square is an illegal square.
#-

--------
02              3-block
                =======
  #
 %%             At least one of the squares is not a goal-square.
#%

--------
03              4-Block/A
                =========
XX              Each 'X' is a non-empty square, and at least one
XX              of them is a box on a non-goal square. The non-wall
                squares in the block form a set.

--------
04              4-Block/B
                =========
 #              At least one of the squares is not a goal-square.
%%
#               The 'capacity' of a set is the number of boxes
                that can be added before the set overflows,
                hence, the capacity is calculated as:

                squares - 1 - number of boxes on the squares.

                This applies to all deadlock sets, except for
                type 08 and 09.

                4-block/B's are actually slightly more general
                than depicted: The '#''s aren't required, it
                suffices that the boxes can't move to any of
                these 2 neighbor squares.


--------
05              Double-L
                ========
??              The central floor square must be a non-goal square,
? ?             or each "L" (upper-left, down-right) must
 ??             contain at least one non-goal floor square.

--------
06              Blocked tunnel
                ==============
 #              The central floor square must be a non-goal square.
% %
%##

----------
07              Closed edge without goals
                =========================
 # ##~~# #      A closed edge is a line from which a box cannot
#----~~---#     escape. If there aren't any goals then all
  #     #       squares on the line are illegal.

---------
08              Closed edge with goals
                ======================
 # ##~~# #      The capacity of the set is the number of boxes
#%%%%~~%%%#     that can be added before the line overflows,
  #	#         hence, capacity = goals - boxes.

---------
09              Closed edges sharing a goal corner
                ==================================
 # ##~~~##      2 closed edges sharing a goal corner form
#%%%%~~%%%#     a union of the squares along the edges.
  #      %#     Capacity = goals - boxes.
         %#
         #

----------
10              Closed edge fence
                =================
 #%##~~#%#      The capacity and topology of the fence and the
#         #     enclosed floor squares must make it impossible
 %#%#~~%#%      to break the fence.


----------
11              1-square gate
                =============
#####           A single square splits the board in 2
#   #           separate rooms. The deadlock consists
# @ #           of 2 boxes where at least one of them
##%##           isn't a goal, and the last pushed box
# % #           must be the one that blocks the gate.
#   #
#####

#####           The second variant has the additional
#   #           constraint that the floor between the
# @ #           2 boxes is not a goal square, in other
##%##           words that the first variant is valid.
#   #
# % #
#   #
#####

 #%#            In this third variant, the central
# % #           box does not split the board in 2
                separate rooms.


----------
12              1-way tunnel
                ============
#   #           A single square splits the board in 2
#   #           separate rooms. When the box enters
# @ #           the tunnel from one side, the tunnel
##%##           gets blocked. None of the squares
 # ###          in the tunnel may be goal squares.
 #   #
 #   #
 #####


----------
13              Frozen goals block access to other goals
                ========================================
(Example)
#######         Controller-set
#  %% #         At least one of the squares in the set is
#  ## #         a goal. The number of goals in the set is
                "G".

#######         Freeze-set
#*    #         All squares in the set are goals. If all
#  ## #         of them are filled, their boxes freeze
                and cannot move anymore. In that case it
                is a deadlock unless the squares in the
                controller-set contains at least "G"
                boxes.


----------
14              Frozen goals block paths from squares to goals
                ==============================================
(Example)
####            Example: the box "$" cannot reach the goal
#. #            square "." because of the frozen  box at the
#  #            the corner goal square "*".
# $##
#  *#
# # #
#   #
#   #
#####


----------
15              Fenced in area
                ==============
                A fenced in area is rooted in a corner square or
                the center of a 'double-L'. Using these sets as
                base, a generator builds new fenced in areas by
                moving and adding boxes one at a time, taking
                the level topology into account.

---------
}


  type //TCornerType   =(ctTopLeft,ctTopRight,ctBottomLeft,ctBottomRight);
       //TCornerTypeSet=set of TCornerType;
       TCorner       = record EdgeLengths:array[TDirection] of Integer; SquareNo:Integer; Types:TCornerTypeSet;
                       end;
       TSquareColor  = (scWhite,scLightGray,scDarkGray,scBlack); {order must not change; meaning: outside square, floor, box, wall}
       TSquareColors = array[0..MAX_BOARD_SIZE] of TSquareColor;
       TBoardSquareSet    = record
         Count       : Integer;
         Squares     : TBoardOfInt16;
       end;
       TDeadlockSetCandidate = record
         Boxes               : TBoardSquareSet;
         Capacity            : Integer;
         CenterSquare        : Integer;
         Flags               : TDeadlockSetFlagsSet;
         Floors              : TBoardSquareSet; {not necessarily including the center square and its adjacent squares}
         GoalCount           : Integer;
         PaintedFloorCount   : Integer;
         MaxBoxCount         : Integer;    {max. allowed number of boxes; may differ from 'Game.BoxCount'}
         SquareColors        : TSquareColors;
       end;
  var  i,j,CornerCount:Integer;
       Corners:array[0..MAX_BOARD_SIZE] of TCorner;
       DeadlockSetCandidate:TDeadlockSetCandidate;

    function AddSquareToDeadlockSet(SquareNo__:Integer):Boolean; forward;
    function CommitDeadlockSet(Search__,ExpandSet__,AcceptPseudoLegalSets__:Boolean; var StartPositionIsOk__:Boolean):Boolean; forward;
    function TrySquare(SquareNo__:Integer; Direction__:TDirection):Boolean; forward;

    procedure CountBoxesAndGoalsAndPlayer(SquareNo__:Integer; var BoxCount__,GoalCount__,PlayerCount__:Integer);
    begin
      if (Game.Board[SquareNo__] and BOX   )<>0 then Inc(BoxCount__   );
      if (Game.Board[SquareNo__] and GOAL  )<>0 then Inc(GoalCount__  );
      if (Game.Board[SquareNo__] and PLAYER)<>0 then Inc(PlayerCount__);
    end;

    procedure InitializeSquareColors(var SquareColors__:TSquareColors);
    var i:Integer;
    begin
      DeadlockSetCandidate.PaintedFloorCount:=0;
      for i:=0 to Game.BoardSize do
          if (Game.Board[i] and WALL)<>0 then SquareColors__[i]:=scBlack
          else SquareColors__[i]:=scWhite;
    end;

    function CalculateNeighborsOfColor(SquareNo__:Integer; Color__:TSquareColor; LegalSquaresOnly__:Boolean):Integer;
    var Neighbor:Integer; Dir:TDirection;
    begin
      Result:=0;
      for Dir:=Low(Dir) to High(Dir) do begin
          Neighbor:=SquareNo__+Game.SquareOffsetForward[Dir];
          if  (DeadlockSetCandidate.SquareColors[Neighbor]=Color__)
              and
              ((not LegalSquaresOnly__)
               or
               (IsALegalBoxSquare    (Neighbor) and
                IsAReachableBoxSquare(Neighbor))
              ) then
              Inc(Result);
          end;
    end;

    function TryBox(SquareNo__:Integer):Boolean;
    begin
      Result:=IsALegalBoxSquare     (SquareNo__) and
              IsAReachableBoxSquare (SquareNo__) and
              AddSquareToDeadlockSet(SquareNo__);
      if Result then begin
         DeadlockSetCandidate.SquareColors[SquareNo__]:=scDarkGray; {mark the square for containing a box}
         if not IsABoxSquare(SquareNo__) then Inc(DeadlockSetCandidate.Capacity);
         end;
    end;

    function TryFloor(SquareNo__:Integer):Boolean;
    var i,OldBoxCount,OldFloorCount,OldPaintedFloorCount,OldGoalCount,OldCapacity:Integer;
        OldCandidateSetFlags:TDeadlockSetFlagsSet; Dir:TDirection;
    begin
      Result:=(DeadlockSetCandidate.SquareColors[SquareNo__]=scWhite) and
              ((Game.Board[SquareNo__] and (PLAYER{+BOX+GOAL}))=0) and
              (DeadlockSetCandidate.GoalCount<=Game.BoxCount);
      if Result then begin
         OldBoxCount:=DeadlockSetCandidate.Boxes.Count;
         OldFloorCount:=DeadlockSetCandidate.Floors.Count;
         OldPaintedFloorCount:=DeadlockSetCandidate.PaintedFloorCount;
         OldGoalCount:=DeadlockSetCandidate.GoalCount; OldCapacity:=DeadlockSetCandidate.Capacity;
         OldCandidateSetFlags:=DeadlockSetCandidate.Flags;
         DeadlockSetCandidate.SquareColors[SquareNo__]:=scLightGray;
         Inc(DeadlockSetCandidate.PaintedFloorCount);
         Inc(DeadlockSetCandidate.Floors.Count); DeadlockSetCandidate.Floors.Squares[DeadlockSetCandidate.Floors.Count]:=SquareNo__;
         if IsAGoalSquare(SquareNo__) then Inc(DeadlockSetCandidate.GoalCount);

         for Dir:=Low(Dir) to High(Dir) do
             Result:=Result and TrySquare(SquareNo__+Game.SquareOffsetForward[Dir],Dir);

         if not Result then begin {undo added boxes and floors}
            for i:=Succ(OldBoxCount  ) to DeadlockSetCandidate.Boxes .Count do DeadlockSetCandidate.SquareColors[DeadlockSetCandidate.Boxes .Squares[i]]:=scWhite;
            for i:=Succ(OldFloorCount) to DeadlockSetCandidate.Floors.Count do DeadlockSetCandidate.SquareColors[DeadlockSetCandidate.Floors.Squares[i]]:=scWhite;
            DeadlockSetCandidate.Boxes .Count     :=OldBoxCount;
            DeadlockSetCandidate.Floors.Count     :=OldFloorCount;
            DeadlockSetCandidate.PaintedFloorCount:=OldPaintedFloorCount;
            DeadlockSetCandidate.GoalCount        :=OldGoalCount;
            DeadlockSetCandidate.Capacity         :=OldCapacity;
            DeadlockSetCandidate.Flags            :=OldCandidateSetFlags;
            end;
         end;
    end;

    function TrySquare(SquareNo__:Integer; Direction__:TDirection):Boolean;
    begin
      Result:=DeadlockSetCandidate.SquareColors[SquareNo__]<>scWhite;                    {non-white: square is already ok}
      if not Result then {white square, i.e., a non-visited square}
         case CalculateNeighborsOfColor(SquareNo__,scWhite,False) of

{$IFDEF LIGHTWEIGHT_DEADLOCK_GENERATION}
{   generates a reasonable number of deadlocks and works pretty fast.
}
           0   : Result:=TryFloor(SquareNo__) or TryBox  (SquareNo__);
           1   : Result:=TryFloor(SquareNo__) or TryBox  (SquareNo__);
           2   : Result:=(DeadlockSetCandidate.SquareColors[SquareNo__+Game.SquareOffsetForward[Direction__]]=scWhite) {left or right adjacent square is blocked}
                         and
                         (TryBox(SquareNo__)
                          or
                          (
                           ((not IsALegalBoxSquare    (SquareNo__))
                            or
                            (not IsAReachableBoxSquare(SquareNo__))
                           )
                           and
                           TryFloor(SquareNo__)
                          ));
           else  Result:=False;
{$ELSE}
{  generates deadlocks more thoroughly, but the exponential growth in
   the number of candidates can make the program useless.
}
           0   : Result:=TryBox  (SquareNo__) or TryFloor(SquareNo__);
           1   : Result:=TryBox  (SquareNo__) or TryFloor(SquareNo__);
           2   : Result:=((DeadlockSetCandidate.SquareColors[SquareNo__+Game.SquareOffsetForward[Direction__]]=scWhite) {left or right adjacent square is blocked}
                          or
                          (
                           ((DeadlockSetCandidate.SquareColors[SquareNo__+Game.SquareOffsetLeft[Direction__]]=scWhite)
                            and
                            TryBox(SquareNo__+Game.SquareOffsetLeft[Direction__])
                           )
                           or
                           ((DeadlockSetCandidate.SquareColors[SquareNo__+Game.SquareOffsetRight[Direction__]]=scWhite)
                            and
                            TryBox(SquareNo__+Game.SquareOffsetRight[Direction__])
                           )
                          )
                         )
                         and
                         (TryBox(SquareNo__)
                          or
                          (
                           ((not IsALegalBoxSquare    (SquareNo__))
                            or
                            (not IsAReachableBoxSquare(SquareNo__))
                           )
                           and
                           TryFloor(SquareNo__)
                          ));
           else  Result:=(TryBox(SquareNo__+Game.SquareOffsetLeft[Direction__])
                          or
                          TryBox(SquareNo__+Game.SquareOffsetRight[Direction__])
                         )
                         and
                         TrySquare(SquareNo__,Direction__);
{$ENDIF}
         end; {case}
    end;

    function InitializeDeadlockSetCandidate(Capacity__,CenterSquare__,MaxBoxCount__:Integer):Boolean;
    begin
      Result:=Game.DeadlockSets.Count<MAX_DEADLOCK_SETS;
      if Result then begin
         DeadlockSetCandidate.Capacity    :=Capacity__;
         DeadlockSetCandidate.CenterSquare:=CenterSquare__;
         DeadlockSetCandidate.Flags       :=[];
         DeadlockSetCandidate.MaxBoxCount :=Min(MaxBoxCount__,Game.BoardSize);
         DeadlockSetCandidate.Boxes.Count :=0;
         DeadlockSetCandidate.Floors.Count:=0; DeadlockSetCandidate.PaintedFloorCount:=0;
         DeadlockSetCandidate.GoalCount:=0;
         end
      else begin
         if Game.ShowDeadlockSetsEnabled then
            Writeln('Note: Table full (deadlock sets); not all detected sets are in use.');
         DeadlockSetCandidate.GoalCount:=Succ(Game.BoxCount); {mark current set as invalid}
         end;
    end;

    procedure DeleteDeadlockSet(SetNo__:Integer);
    var i,j,k,SquareNo:Integer;
    begin
      with Game.DeadlockSets do begin
        Dec(Count);
        for i:=SetNo__ to Count do begin {compress the table}
            Capacity         [i]:=Capacity         [Succ(i)];
            CenterSquare     [i]:=CenterSquare     [Succ(i)];
            Flags            [i]:=Flags            [Succ(i)];
            FloorCount       [i]:=FloorCount       [Succ(i)];
            HashKey          [i]:=HashKey          [Succ(i)];
            RunNo            [i]:=RunNo            [Succ(i)];
            SquaresCount     [i]:=SquaresCount     [Succ(i)];
            end;
        for SquareNo:=0 to Game.BoardSize do {remove the reference to the set from each square}
            for i:=SquareSetCount[SquareNo] downto 1 do begin
                j:=SquareSetNumbers[SquareNo,i];
                if j=SetNo__ then begin {reference to the deleted set: remove it}
                   Dec(SquareSetCount[SquareNo]);
                   for k:=i to SquareSetCount[SquareNo] do
                       SquareSetNumbers[SquareNo,k]:=
                         SquareSetNumbers[SquareNo,Succ(k)];
                   end
                else if j>SetNo__ then {reference to a number above the removed set: decrease it}
                        Dec(SquareSetNumbers[SquareNo,i]);
                end;
        end;
    end; {DeleteDeadlockSet}

    function AddSquareToDeadlockSet(SquareNo__:Integer):Boolean;
    begin {precondition: no square is added more than once}
      Result:=(DeadlockSetCandidate.Boxes.Count<DeadlockSetCandidate.MaxBoxCount) and
              (DeadlockSetCandidate.GoalCount<=Game.BoxCount);
      if Result then begin
         Result:=Game.DeadlockSets.SquareSetCount[SquareNo__]<MAX_DEADLOCK_SETS_PER_SQUARE;
         if Result then with Game.SearchStates[0].PlayersReachableSquares do begin
            Inc(DeadlockSetCandidate.Boxes.Count);
            DeadlockSetCandidate.Boxes.Squares[DeadlockSetCandidate.Boxes.Count]:=SquareNo__; {save the square-numbers for 'CommitDeadlockSet'}
            if IsAGoalSquare(SquareNo__) then Inc(DeadlockSetCandidate.GoalCount);
            Squares[SquareNo__]:=TimeStamp; {mark the square as visited}
            end
         else begin
            if Game.ShowDeadlockSetsEnabled then
               Writeln('Note: Table full (squares in a deadlock set); not all detected sets are in use.');
            DeadlockSetCandidate.GoalCount:=Succ(Game.BoxCount); {mark current set as invalid}
            end;
         end
      else
         DeadlockSetCandidate.GoalCount:=Succ(Game.BoxCount); {mark current set as invalid}
    end;

    function CalculateDeadlockSetCandidateInfo:Boolean; {returns 'False' if the the number of goals >= number of boxes}
    var i,CenterSquareRoomSquareCount,SquareNo{,Step}:Integer;

      procedure FloodFill(SquareNo__:Integer); {marks floor-squares surrounded by boxes and walls}
      var Dir:TDirection;
      begin
        if DeadlockSetCandidate.SquareColors[SquareNo__]=scWhite then begin
           DeadlockSetCandidate.SquareColors[SquareNo__]:=scLightGray; {light-grey: floor-squares inside the fence}
           Inc(DeadlockSetCandidate.PaintedFloorCount);
           if  IsAGoalSquare  (SquareNo__) then Inc(DeadlockSetCandidate.GoalCount);
           if  IsAPlayerSquare(SquareNo__) then
               Include(DeadlockSetCandidate.Flags,dsfPlayerIsInsideSet);
           for Dir:=Low(Dir) to High(Dir) do
               FloodFill(SquareNo__+Game.SquareOffsetForward[Dir]);
           end;
      end;

    begin {CalculateDeadlockSetCandidateInfo}
      {initialize square-colours, (re)calculate capacity and number of goals,}
      {and check if player in the start position is inside the deadlock set}
      InitializeSquareColors(DeadlockSetCandidate.SquareColors);
      DeadlockSetCandidate.GoalCount:=0;
      if  DeadlockSetCandidate.CenterSquare<>0 then {only recalculate capacity for sets with a central floor square}
          DeadlockSetCandidate.Capacity:=Pred(DeadlockSetCandidate.Boxes.Count);

      for i:=1 to DeadlockSetCandidate.Boxes.Count do begin
          SquareNo:=DeadlockSetCandidate.Boxes.Squares[i];
          DeadlockSetCandidate.SquareColors[SquareNo]:=scDarkGray; {dark-grey: mark box-squares}
          if (DeadlockSetCandidate.CenterSquare<>0) and IsABoxSquare(SquareNo) then
             Dec(DeadlockSetCandidate.Capacity);
          if IsAGoalSquare  (SquareNo) then Inc(DeadlockSetCandidate.GoalCount);
          if IsAPlayerSquare(SquareNo) then Include(DeadlockSetCandidate.Flags,dsfPlayerIsInsideSet); {it's necessary to assume the player starts inside the set in this situation}
          end;

      FloodFill(DeadlockSetCandidate.CenterSquare);
      CenterSquareRoomSquareCount:=DeadlockSetCandidate.PaintedFloorCount;
      for i:=1 to DeadlockSetCandidate.Floors.Count do FloodFill(DeadlockSetCandidate.Floors.Squares[i]);
{
      if dsfDiagonalCenterSquares in DeadlockSetCandidate.Flags then with DeadlockSetCandidate do begin
         // precondition: the center square is a corner
         if        SquareColors[Pred(CenterSquare)]=scBlack then
                   if   SquareColors[Succ(CenterSquare)]<>scBlack then // 'True': the center square hasn't walls both left and right
                        if   SquareColors[CenterSquare+Game.BoardWidth+2]=scBlack then
                             Step:=Game.BoardWidth+2+1 // "\" diagonal
                        else Step:=Game.BoardWidth+2-1 // "/" diagonal
                   else Step:=0 // not enough information to indentify which diagonal to use; drop the extra internal floors and treat them as external floors
         else if   SquareColors[Succ(CenterSquare)]=scBlack then
                   if   SquareColors[CenterSquare+Game.BoardWidth+2]=scBlack then
                        Step:=Game.BoardWidth+2-1 // "/" diagonal
                   else Step:=Game.BoardWidth+2+1 // "\" diagonal
              else Step:=0;
         SquareNo:=CenterSquare+Step;
         while SquareColors[SquareNo]=scWhite do begin
               FloodFill(SquareNo); Inc(SquareNo,Step);
               end;
         end;
}
      if DeadlockSetCandidate.CenterSquare<>0 then
         if   CenterSquareRoomSquareCount=DeadlockSetCandidate.PaintedFloorCount then {'True': all inner floor squares belong to the room with the center square}
              Exclude(DeadlockSetCandidate.Flags,dsfHasDisconnectedInnerFloors)
         else Include(DeadlockSetCandidate.Flags,dsfHasDisconnectedInnerFloors);

      Result:=(DeadlockSetCandidate.Boxes.Count>DeadlockSetCandidate.GoalCount) or
              (dsfControllerSet in DeadlockSetCandidate.Flags);
    end; {CalculateDeadlockSetCandidateInfo}

    procedure LoadCandidateFromSet(SetNo__:Integer);
    var i,j:Integer;
    begin  {copies set to 'DeadlockSetCandidate'}
      with Game.DeadlockSets do begin
        DeadlockSetCandidate.Capacity    :=Capacity    [SetNo__];
        DeadlockSetCandidate.CenterSquare:=CenterSquare[SetNo__];
        DeadlockSetCandidate.Flags       :=Flags       [SetNo__];
        DeadlockSetCandidate.Boxes.Count :=0;
        DeadlockSetCandidate.Floors.Count:=0; {*not* Game.DeadlockSets.FloorCount[SetNo__]}
        DeadlockSetCandidate.GoalCount   :=0;

        for i:=0 to Game.BoardSize do
            for j:=1 to SquareSetCount[i] do
                if SquareSetNumbers[i,j]=SetNo__ then begin
                   Inc(DeadlockSetCandidate.Boxes.Count);
                   DeadlockSetCandidate.Boxes.Squares[DeadlockSetCandidate.Boxes.Count]:=i;
                   end;

        CalculateDeadlockSetCandidateInfo;

        if FloorCount[SetNo__]<>DeadlockSetCandidate.PaintedFloorCount then
           {some inner floors aren't connected to the central square;}
           {therefore, derived deadlocksets cannot depend on the player}
           {being outside the set (a calculation that requires all inner floors)}
           Include(DeadlockSetCandidate.Flags,dsfHasDisconnectedInnerFloors);
        end;
    end; {LoadCandidateFromSet}

    function IsARedundantDeadlockSet(SetNo__:Integer):Boolean;
    var i,j,SetNo,SquareNo,CommonSquaresCount:Integer;
    begin
      Result:=False;
      if Game.DeadlockSets.CenterSquare[SetNo__]<>0 then with Game.DeadlockSets do begin
         LoadCandidateFromSet(SetNo__);
         for SetNo:=1 to Count do
             if (CenterSquare[SetNo]<>0) and (SetNo<>SetNo__) then begin
                CommonSquaresCount:=0;
                for i:=1 to DeadlockSetCandidate.Boxes.Count do begin {for each square in the new set}
                    SquareNo:=DeadlockSetCandidate.Boxes.Squares[i];
                    for j:=1 to Game.DeadlockSets.SquareSetCount[SquareNo] do {for each set having this square as a member}
                        if SquareSetNumbers[SquareNo,j]=SetNo then
                           Inc(CommonSquaresCount);
                    end;
                if (CommonSquaresCount=SquaresCount[SetNo]) and
                   ((Flags[SetNo__]-[dsfPlayerIsInsideSet])=(Flags[SetNo]-[dsfPlayerIsInsideSet]))
                   then begin
                   Result:=True; break;
                   end;
                end;
         end;
    end; {IsARedundantDeadlockSet}

    function ExpandDeadlockSet(BoxIndex__:Integer; ExpandSet__,AcceptPseudoLegalSets__:Boolean; var StartPositionIsOk__:Boolean):Boolean;
    var i,ExtraBoxSquareNo,NeighborSquareNo,SquareNo,WhiteNeighborCount,
        OldBoxCount,OldFloorCount,OldPaintedFloorCount,OldGoalCount,OldCapacity:Integer;
        Direction:TDirection;
        OldCandidateSetFlags:TDeadlockSetFlagsSet;

     function ExpandDeadlockSet__:Boolean;
     var i,BoxOnIllegalSquareIndex:Integer;
     begin {ExpandDeadlockSet.ExpandDeadlockSet__}
       BoxOnIllegalSquareIndex:=0;
       if DeadlockSetCandidate.CenterSquare<>0 then
          for i:=1 to DeadlockSetCandidate.Boxes.Count do
              if not (IsALegalBoxSquare(DeadlockSetCandidate.Boxes.Squares[i])
                      and
                      IsAReachableBoxSquare(DeadlockSetCandidate.Boxes.Squares[i])
                     ) then begin {simple illegal squares}
                 BoxOnIllegalSquareIndex:=i; break;
                 end;

       if   BoxOnIllegalSquareIndex=0 then
            Result:=CommitDeadlockSet(True,ExpandSet__,AcceptPseudoLegalSets__,StartPositionIsOk__)  {commit new set}
       else Result:=ExpandDeadlockSet(BoxOnIllegalSquareIndex,
                                      ExpandSet__,AcceptPseudoLegalSets__,StartPositionIsOk__); {try to get the box away from the illegal square}
     end; {ExpandDeadlockSet.ExpandDeadlockSet__}

     function TryToAddExtraBox(SquareNo__,ExtraBoxSquareNo__:Integer):Boolean;
     begin {ExpandDeadlockSet.TryToAddExtraBox}
       Inc(DeadlockSetCandidate.Boxes.Count); {add the extra box}
       DeadlockSetCandidate.Boxes.Squares[DeadlockSetCandidate.Boxes.Count]:=ExtraBoxSquareNo__;
       if not IsABoxSquare (ExtraBoxSquareNo__) then Inc(DeadlockSetCandidate.Capacity);
       if     IsAGoalSquare(ExtraBoxSquareNo__) then Inc(DeadlockSetCandidate.GoalCount);
       DeadlockSetCandidate.SquareColors[ExtraBoxSquareNo__]:=scDarkGray;

       Result:=TryFloor(SquareNo__) and ExpandDeadlockSet__;

       Dec(DeadlockSetCandidate.Boxes.Count); {remove the extra box}
       if not IsABoxSquare (ExtraBoxSquareNo__) then Dec(DeadlockSetCandidate.Capacity);
       if     IsAGoalSquare(ExtraBoxSquareNo__) then Dec(DeadlockSetCandidate.GoalCount);
       DeadlockSetCandidate.SquareColors[ExtraBoxSquareNo__]:=scWhite;
     end; {ExpandDeadlockSet.TryToAddExtraBox}

    begin {ExpandDeadlockSet}
      Result:=(DeadlockSetCandidate.CenterSquare<>0) and
              (DeadlockSetCandidate.Boxes.Count<=Game.DeadlockSets.BoxLimit);
      if Result then begin
         SquareNo:=DeadlockSetCandidate.Boxes.Squares[BoxIndex__];

         WhiteNeighborCount:=CalculateNeighborsOfColor(SquareNo,scWhite,False);
         if (WhiteNeighborCount>0) and (WhiteNeighborCount<=Game.DeadlockSets.AdjacentOpenSquaresLimit) then begin
            {'AdjacentOpenSquaresLimit': should preferably be '1';}
            {allowing 2 or 3 outside neighbors finds a lot more deadlock sets,}
            {but it can easily take too much time}
            Dec(DeadlockSetCandidate.Boxes.Count); {remove the box from the candidate set}
            for i:=BoxIndex__ to DeadlockSetCandidate.Boxes.Count do
                DeadlockSetCandidate.Boxes.Squares[i]:=DeadlockSetCandidate.Boxes.Squares[Succ(i)];
            DeadlockSetCandidate.SquareColors[SquareNo]:=scWhite;
            {save current set data}
            OldBoxCount:=DeadlockSetCandidate.Boxes.Count;
            OldFloorCount:=DeadlockSetCandidate.Floors.Count; OldPaintedFloorCount:=DeadlockSetCandidate.PaintedFloorCount;
            OldGoalCount:=DeadlockSetCandidate.GoalCount; OldCapacity:=DeadlockSetCandidate.Capacity;
            OldCandidateSetFlags:=DeadlockSetCandidate.Flags;

            DeadlockSetCandidate.Flags:=[];
            if dsfPlayerIsInsideSet in OldCandidateSetFlags then
               Include(DeadlockSetCandidate.Flags,dsfPlayerIsInsideSet);
            if dsfHasDisconnectedInnerFloors in OldCandidateSetFlags then
               Include(DeadlockSetCandidate.Flags,dsfHasDisconnectedInnerFloors);

            if not IsABoxSquare (SquareNo) then Dec(DeadlockSetCandidate.Capacity);
            if     IsAGoalSquare(SquareNo) then Dec(DeadlockSetCandidate.GoalCount);

            Result:=TryFloor(SquareNo); {try to fence-in this square}

            if Result then
               Result:=ExpandDeadlockSet__
            else
               if (WhiteNeighborCount=1) and
                  (Game.DeadlockSets.AdjacentOpenSquaresLimit=1) and
                  (DeadlockSetCandidate.CenterSquare<>0) and
                  (DeadlockSetCandidate.Boxes.Count<DeadlockSetCandidate.MaxBoxCount) then
                  {try to find additional deadlocks by adding an extra box to close a gap;
                   example:
                   ####
                   %  % <- the leftmost "%" is the box square, having 1 white (outside) floor neighbor
                   ####

                   ####
                   %   % <- moving the box one square to the right has failed creating a new deadlock; (the 'Result:=TryFloor()' statement above
                   ####

                   ####% <- put an extra box here;
                   %   % <- when the box is moved one square to the right, there creation of a new deadlock succeeds unless is violates som contraints, e.g., search depth
                   ####
                  }
                  for Direction:=Low(Direction) to High(Direction) do begin
                      NeighborSquareNo:=SquareNo+Game.SquareOffsetForward[Direction];
                      if DeadlockSetCandidate.SquareColors[NeighborSquareNo]=scWhite then begin
                         if IsALegalAndReachableBoxSquare(NeighborSquareNo) then begin
                            ExtraBoxSquareNo:=NeighborSquareNo+Game.SquareOffsetLeft[Direction]; {caution: assumes 4 directions only}
                            if (DeadlockSetCandidate.SquareColors[ExtraBoxSquareNo]=scWhite)
                               and
                               IsALegalAndReachableBoxSquare(ExtraBoxSquareNo)
                               and
                               TryToAddExtraBox(SquareNo,ExtraBoxSquareNo)
                               then
                               Result:=True;
                            ExtraBoxSquareNo:=NeighborSquareNo+Game.SquareOffsetRight[Direction]; {caution: assumes 4 directions only}
                            if (DeadlockSetCandidate.SquareColors[ExtraBoxSquareNo]=scWhite)
                               and
                               IsALegalAndReachableBoxSquare(ExtraBoxSquareNo)
                               and
                               TryToAddExtraBox(SquareNo,ExtraBoxSquareNo)
                               then
                               Result:=True;
                            end;
                         break; {quick-and-dirty exit the 'for each direction' loop as soon as the outside empty floor square has been found}
                         end;
                      end;

            {restore current set}
            for i:=Succ(OldBoxCount  ) to DeadlockSetCandidate.Boxes .Count do DeadlockSetCandidate.SquareColors[DeadlockSetCandidate.Boxes .Squares[i]]:=scWhite;
            for i:=Succ(OldFloorCount) to DeadlockSetCandidate.Floors.Count do DeadlockSetCandidate.SquareColors[DeadlockSetCandidate.Floors.Squares[i]]:=scWhite;
            DeadlockSetCandidate.Boxes .Count     :=OldBoxCount;
            DeadlockSetCandidate.Floors.Count     :=OldFloorCount;
            DeadlockSetCandidate.PaintedFloorCount:=OldPaintedFloorCount;
            DeadlockSetCandidate.GoalCount        :=OldGoalCount;
            DeadlockSetCandidate.Capacity         :=OldCapacity;
            DeadlockSetCandidate.Flags            :=OldCandidateSetFlags;

            Inc(DeadlockSetCandidate.Boxes.Count); {put the box back into the current set}
            for i:=DeadlockSetCandidate.Boxes.Count downto Succ(BoxIndex__) do
                DeadlockSetCandidate.Boxes.Squares[i]:=DeadlockSetCandidate.Boxes.Squares[Pred(i)];
            DeadlockSetCandidate.Boxes.Squares[BoxIndex__]:=SquareNo;

            for i:=1 to DeadlockSetCandidate.Boxes.Count do
                DeadlockSetCandidate.SquareColors[DeadlockSetCandidate.Boxes.Squares[i]]:=scDarkGray;
            end;
         end;
    end; {ExpandDeadlockSet}

    function CommitDeadlockSet(Search__,ExpandSet__,AcceptPseudoLegalSets__:Boolean; var StartPositionIsOk__:Boolean):Boolean;
    var i,j,SquareNo:Integer; IsALegalSetButViolatesCapacityConstraints:Boolean;
        HashKey:THashValue; BoxSquares:TBoardSquareSet;

      function CheckSimpleDeadlocks:Boolean;
      var i:Integer;

        function  IsAFrozenSquare ( SquareNo__ : Integer ) : Boolean;
        var ABoxIsBlockedOnANonGoalSquare : Boolean;

          function  BoxIsBlockedAlongOneAxis( SquareNo__ : Integer; Direction__ : TDirection;
                                              var ABoxIsBlockedOnANonGoalSquare__ : Boolean):Boolean;
          var Neighbor1, Neighbor2, Neighbor1Position , Neighbor2Position : Integer;
          begin
            if   Direction__       = Low(Direction__) then           {flip horizontal/vertical direction}
                 Direction__      := Succ ( Low ( Direction__ ) )    {caution: 'Succ(Low...'): assumes 4 directions only}
            else Direction__      := Low ( Direction__ );

            Neighbor1Position     := SquareNo__ - Game.SquareOffsetForward[ Direction__ ];
            Neighbor1             := Game.Board [ Neighbor1Position ];

            Neighbor2Position     := SquareNo__ + Game.SquareOffSetForward[ Direction__ ];
            Neighbor2             := Game.Board [ Neighbor2Position ];

            Inc ( Game.Board [ SquareNo__ ] , WALL);                 {temporarily change this square to a wall}

            Result := ((  Neighbor1 and WALL           ) <> 0 )     {is there a wall on any of the neighbor squares?}
                      or
                      ((  Neighbor2 and WALL           ) <> 0 )
                      or
                      ((( Neighbor1 and FLAG_ILLEGAL_BOX_SQUARE ) <> 0 ) {are both neighbors illegal squares?}
                       and
                       (( Neighbor2 and FLAG_ILLEGAL_BOX_SQUARE ) <> 0 )
                      );

            if (not ( Result and ABoxIsBlockedOnANonGoalSquare__ ))  {it's not enough to know whether a box is blocked; it's also necessary to know whether a box is blocked on a non-goal square}
               and
               (( Neighbor1 and ( WALL + BOX ) ) = BOX )             {test is neighbor1 is a blocked box}
               and
               BoxIsBlockedAlongOneAxis( Neighbor1Position , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
               then Result := True;

            if (not ( Result and ABoxIsBlockedOnANonGoalSquare__ ))  {it's not enough to know whether a box is blocked; it's also necessary to know whether a box is blocked on a non-goal square}
               and
               (( Neighbor2 and ( WALL + BOX ) ) = BOX )             {test is neighbor2 is a blocked box}
               and
               BoxIsBlockedAlongOneAxis( Neighbor2Position , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
               then Result:=True;

            Dec ( Game.Board [ SquareNo__ ] , WALL );                {remove the wall again}

            if  Result and                                           {if this box is blocked}
                ( ( Game.Board [ SquareNo__ ] and GOAL ) = 0 ) then  {and it's not a goal-square}
                ABoxIsBlockedOnANonGoalSquare__ := True;             {then set the flag}

            end; {BoxIsBlockedAlongOneAxis}

        begin {IsAFrozenSquare}
          ABoxIsBlockedOnANonGoalSquare := False;

          Result := (( Game.Board [ SquareNo__ ] and ( WALL + FLAG_ILLEGAL_BOX_SQUARE ) ) <> 0 )   {a wall is considered a deadlocked square}
                    or
                    (not IsAReachableBoxSquare(SquareNo__))
                    or
                    (// ( not ReverseMode ) and {always forward gameplay here, so a test for reverse mode is unnecessary here}
                     BoxIsBlockedAlongOneAxis( SquareNo__ , Low(        TDirection )   , ABoxIsBlockedOnANonGoalSquare )
                     and
                     BoxIsBlockedAlongOneAxis( SquareNo__ , Succ( Low ( TDirection ) ) , ABoxIsBlockedOnANonGoalSquare ) {caution: 'Succ(Low...'): assumes 4 directions only}
                     and
                     ABoxIsBlockedOnANonGoalSquare
                    );
        end; {IsAFrozenSquare}

      begin {CheckSimpleDeadlocks}
        Result:=True;

        for i:=1 to Game.DeadlockSets.Count do with Game.DeadlockSets do        {check if the candidate set violates existing deadlock-sets}
            if   Result then
                 Result:=(Capacity[i]>=0)
                         or
                         (dsfPlayerIsInsideSet      in Flags[i])
                         or
                         (dsfPlayerMustBeOutsideSet in Flags[i])
            else break;

        if  ((DeadlockSetCandidate.CenterSquare<>0) or (DeadlockSetCandidate.Floors.Count<>0)) then
            for i:=1 to DeadlockSetCandidate.Boxes.Count do
                if   Result then
                     Result:=(not IsAFrozenSquare(DeadlockSetCandidate.Boxes.Squares[i])) {some simple deadlocks are handled by code instead of sets}
                else break;
      end; {CheckSimpleDeadlocks}

      function CheckCandidateSet(var IsALegalSetButViolatesCapacityConstraints__:Boolean):Boolean;
      const
        MAX_HISTORY_MOVES = 20; {maximum search depth}
      type
        TGameState   = record
          BoxPos     : TBoxPos;
          HashValue  : THashValue;
          PlayerPos  : Integer;
        end;
        THistory     = array[0..MAX_HISTORY_MOVES] of TGameState;
        TCheckType   = (ctCheckPushes,ctCheckPulls);
      var
        i,GameBoxCount,GamePlayerPos:Integer;
        Direction:TDirection; PushesFromTheOutsideOnlyDirectionSet:TDirectionSet;
        GameBoxPos:TBoxPos; History:THistory;

        function CalculatePushDirectionsThatOnlyArePossibleFromOutsideTheDeadlockSet(var PushesFromTheOutsideOnlyDirectionSet__:TDirectionSet):Boolean;
        var BoxNo,NeighborSquareNo,SquareNo:Integer; Direction:TDirection;
            PushesFromTheOutsideDirectionSet:TDirectionSet;
        begin
          PushesFromTheOutsideOnlyDirectionSet__:=ALL_DIRECTIONS_SET;
          PushesFromTheOutsideDirectionSet:=[];
          for BoxNo:=1 to Game.BoxCount do begin
              SquareNo:=Game.BoxPos[BoxNo];
              for Direction:=Low(Direction) to High(Direction) do begin
                  NeighborSquareNo:=SquareNo+Game.SquareOffsetForward[Direction];
                  if ((Game.Board[NeighborSquareNo] and (WALL+BOX+FLOOR))=FLOOR)
                     and
                     ((Game.Board[NeighborSquareNo+Game.SquareOffsetForward[Direction]] and (WALL+BOX+FLOOR))=FLOOR) then
                     {the box can be pulled away, i.e., the starting position}
                     {can occur after pushing the box in the opposite direction;}
                     if (DeadlockSetCandidate.SquareColors[NeighborSquareNo]=scLightGray) then
                        {the neighbor square is an empty floor inside the set, hence,}
                        {pushing a box in the opposite direction cannot be guaranteed}
                        {to have the player outside the set}
                        Exclude(PushesFromTheOutsideOnlyDirectionSet__,OPPOSITE_DIRECTION[Direction])
                     else
                        {the neighbor square is an empty floor outside the set;}
                        {remember the legal push-directions from the outside}
                        Include(PushesFromTheOutsideDirectionSet,OPPOSITE_DIRECTION[Direction]);
                  end;
              end;

          PushesFromTheOutsideOnlyDirectionSet__ := PushesFromTheOutsideOnlyDirectionSet__ * PushesFromTheOutsideDirectionSet; { '*': intersects the guaranteed outside-directions and the possible outside-directions}
          Result:=PushesFromTheOutsideOnlyDirectionSet__<>[];
        end; {CalculatePushDirectionsThatOnlyArePossibleFromOutsideTheDeadlockSet}

        procedure ClearBoard; {remove boxes and player from the board}
        var i,j,SquareNo:Integer;
        begin
          for i:=1 to Game.BoxCount do with Game.DeadlockSets do begin
              SquareNo:=Game.BoxPos[i]; Dec(Game.Board[SquareNo],BOX); {remove the box from the board}
              for j:=1 to SquareSetCount[SquareNo] do Inc(Capacity[SquareSetNumbers[SquareNo,j]]); {update deadlock set capacities}
              end;
          Game.BoxCount:=0; MovePlayer(0); Game.HashValue:=0; Game.SimpleLowerBound:=0;
        end;

        procedure InitializeBoard; {update board and deadlock capacities, using 'Game.BoxPos' and 'Game.PlayerPos'}
        var i,j,SquareNo:Integer;
        begin
          for i:=1 to Game.BoxCount do with Game.DeadlockSets do begin
              SquareNo:=Game.BoxPos[i]; Inc(Game.Board[SquareNo],BOX); {put the box on the board}
              for j:=1 to SquareSetCount[SquareNo] do Dec(Capacity[SquareSetNumbers[SquareNo,j]]); {update deadlock set capacities}
              end;
          MovePlayer(Game.PlayerPos);
          Game.SimpleLowerBound:=CalculateSimpleLowerBound;
          Game.HashValue:=CalculateHashValue;
        end;

        function Check(CheckType__:TCheckType):Boolean;
        {the deadlock candidate is invalid in 4 cases:}
        {1: if the player starting from the outside can push a box to a square outside the set;   }
        {2: if the player starting from the inside can escape the fenced-in area by pulling boxes;}
        {   (in this case, the candidate position could arise after pushing the boxes from a      }
        {    position where the player started from outside the fenced-in area)                   }
        {3: if the player in the game starting position is inside the fenced-in area and any of   }
        {   the boxes can be pulled, meaning the candidate position could arise after a push;     }
        {4: pushing boxes from the outside or pulling boxes from the inside leads to a position   }
        {   where all boxes are located on goal-positions;                                        }
        const PLAYER_START_SQUARE_COLORS:array[TCheckType] of TSquareColor =(scWhite,scLightGray);
        var   i,j:Integer; Visited:TBoardOfBoolean; Frontier:TBoardSquareSet;

          procedure CalculateFrontierSquares;
          var i,SquareNo:Integer; Dir:TDirection;
          begin  {finds exterior floor squares (white squares) adjacent to the boxes}
            Frontier.Count:=0;
            for i:=1 to DeadlockSetCandidate.Boxes.Count do
                for Dir:=Low(Dir) to High(Dir) do begin
                    SquareNo:=DeadlockSetCandidate.Boxes.Squares[i]+Game.SquareOffsetForward[Dir];
                    if DeadlockSetCandidate.SquareColors[SquareNo]=scWhite then begin
                       Inc(Frontier.Count); Frontier.Squares[Frontier.Count]:=SquareNo;
                       DeadlockSetCandidate.SquareColors[SquareNo]:=scLightGray; {temporarily mark the square as visited}
                       end;
                    end;
            for i:=1 to Frontier.Count do
                DeadlockSetCandidate.SquareColors[Frontier.Squares[i]]:=scWhite; {reset colours to white, i.e., an outside square}
          end;

          function Search(Depth__:Integer):Boolean;
          var i,BoxNo,SquareNo,SuccessorDepth:Integer; Direction:TDirection;

            function IsADuplicatePosition(Depth__,PlayerPos__:Integer):Boolean;
            var i,j,k:Integer;
            begin
              Result:=False;
              for i:=1 to Pred(Depth__) do {search through the stored positions}
                  if not Result then begin
                     Result:=(PlayerPos__   =History[i].PlayerPos) and
                             (Game.HashValue=History[i].HashValue);
                     {a hash-value is not 100% safe, hence, the exact game state must match}
                     for j:=1 to Game.BoxCount do begin {try to match each box}
                         if Result then
                            if Game.BoxPos[j]<>History[i].BoxPos[j] then begin {'<>': no simple match}
                               Result:=False;
                               for k:=1 to Game.BoxCount do {search for another box at the same position as Box[j]}
                                   if   not Result then
                                        Result:=Game.BoxPos[j]=History[i].BoxPos[k]
                                   else break; {done, i.e., there is another box at the same position as Box[j]}
                               end
                            else {Box[j] at the same position}
                         else break; {fail, i.e., not a duplicate position}
                         end;
                     end
                  else break; {done, i.e., found a duplicate position}
            end;

            function PlayerHasAccessToAnOuterSquare(Depth__:Integer):Boolean;
            var i:Integer;
            begin {precondition: player's reachable squares have been calculated}
              Result:=False;
              for i:=1 to Frontier.Count do
                  if   not Result then with Game.SearchStates[Depth__].PlayersReachableSquares do
                       Result:=Squares[Frontier.Squares[i]]=TimeStamp
                  else break;
            end;

          begin {Search; precondition: player's reachable squares have been calculated}
            Inc(Game.DeadlockSets.CandidateSearchCount);
            Result:=(Game.SimpleLowerBound<>0) and {'0': all boxes are located at goals}
                    (Depth__<MAX_HISTORY_MOVES) and {if the search goes to deep: be conservative and reject the deadlock-set}
                    (Game.DeadlockSets.CandidateSearchCount<MAX_DEADLOCK_SET_CANDIDATE_SEARCH_PUSHES)
                    and
                    (Game.JobNo=Game.CalculateDeadlockSetsJobNo); {if the job number changes then the user has abandoned this calculation}

            if Result then begin
               SuccessorDepth:=Succ(Depth__);

               for BoxNo:=1 to Game.BoxCount do
                   if Result then begin
                      SquareNo:=Game.BoxPos[BoxNo];

                      for Direction:=Low(Direction) to High(Direction) do
                          if Result then with Game.SearchStates[Depth__].PlayersReachableSquares do
                             if CheckType__=ctCheckPushes then {pushes: try proving the set invalid by pushing a box outside the area}
                                {inline simple checks for legal moves}
                                if (Squares[SquareNo-Game.SquareOffsetForward[Direction]]=TimeStamp)
                                   and
                                   ((Game.Board[SquareNo+Game.SquareOffsetForward[Direction]] and (BOX+WALL+FLAG_ILLEGAL_BOX_SQUARE))=0)
                                   and
                                   {extended checks for legal moves}
                                   IsALegalPush(BoxNo,Direction) then begin

                                   DoPush(BoxNo,Direction);                        {do the move, i.e., update the board}

                                   Result:=DeadlockSetCandidate.SquareColors[Game.BoxPos[BoxNo]]<>scWhite; {'White': pushing the box outside the deadlock-set violates the deadlock condition}

                                   if Result then begin
                                      CalculatePlayersReachableSquares(SuccessorDepth,0); {note that the results from this calculation are passed on to the recursive call}
                                      if not IsADuplicatePosition(SuccessorDepth,Game.SearchStates[SuccessorDepth].PlayersReachableSquares.MinPlayerPos) then begin
                                         {update game history}
                                         for i:=1 to Game.BoxCount do History[SuccessorDepth].BoxPos[i]:=Game.BoxPos[i];
                                         History[SuccessorDepth].HashValue:=Game.HashValue;
                                         History[SuccessorDepth].PlayerPos:=Game.SearchStates[SuccessorDepth].PlayersReachableSquares.MinPlayerPos;

                                         Result:=Search(SuccessorDepth);           {recursive step: explore the new position}
                                         end;
                                      end;

                                   UndoPush(BoxNo,Direction);                      {take back the move, i.e., update the board}
                                   end
                                else {no push in this direction}
                             else {pulls: try proving the set invalid by pulling boxes from the inside so the player can escape the fenced-in area}
                                if (Squares[SquareNo+  Game.SquareOffsetForward[Direction]]=TimeStamp)
                                   and
                                   (Squares[SquareNo+2*Game.SquareOffsetForward[Direction]]=TimeStamp)
                                   and
                                   ((Game.Board[SquareNo+Game.SquareOffsetForward[Direction]] and (BOX+WALL+FLAG_ILLEGAL_BOX_SQUARE))=0) then begin

                                   DoPull(BoxNo,Direction);                        {do the move, i.e., update the board}

                                   CalculatePlayersReachableSquares(SuccessorDepth,0); {note that the results from this calculation are passed on to the recursive call}

                                   if not IsADuplicatePosition(SuccessorDepth,Game.SearchStates[SuccessorDepth].PlayersReachableSquares.MinPlayerPos) then begin
                                      Result:=(not (dsfPlayerIsInsideSet in DeadlockSetCandidate.Flags)) and
                                              (not PlayerHasAccessToAnOuterSquare(SuccessorDepth));
                                      if Result then begin
                                         {update game history}
                                         for i:=1 to Game.BoxCount do History[SuccessorDepth].BoxPos[i]:=Game.BoxPos[i];
                                         History[SuccessorDepth].HashValue:=Game.HashValue;
                                         History[SuccessorDepth].PlayerPos:=Game.SearchStates[SuccessorDepth].PlayersReachableSquares.MinPlayerPos;

                                         Result:=Search(SuccessorDepth);           {recursive step: explore the new position}
                                         end;
                                      end;

                                   UndoPull(BoxNo,Direction);                      {take back the move, i.e., update the board}
                                   end
                                else
                          else break; {Result = False}
                      end
                   else break;
               end;
          end; {Search}

        begin {Check}
          //Writeln('Check ',Game.SearchStates[0].PlayersReachableSquares.TimeStamp);
          Result:=Game.SimpleLowerBound<>0;
          FillChar(Visited,SizeOf(Visited),0);
          Game.DeadlockSets.CandidateSearchCount:=0;

          if   CheckType__=ctCheckPulls then
               CalculateFrontierSquares {'frontier squares' are used to speed up detection of the player's access to the outside area}
          else if Result and
                  (dsfPlayerIsInsideSet in DeadlockSetCandidate.Flags) and
                  ((Game.Board[GamePlayerPos] and (WALL+BOX+FLOOR))=FLOOR) then begin
                  {note: 'GamePlayerPos', not 'Game.PlayerPos', i.e., the real}
                  {player position is a an empty floor inside the set}
                  MovePlayer(GamePlayerPos); CalculatePlayersReachableSquares(0,0);
                  with Game.SearchStates[0].PlayersReachableSquares do
                    if Squares[DeadlockSetCandidate.CenterSquare]<>TimeStamp then begin
                       {if the player is inside the set, but not in the same access area}
                       {as the center square, then check that the player cannot push a box outside the area;}
                       {maybe this check isn't necessary, but it won't hurt to throw away a few deadlock-sets}
                       Result:=Search(0);
                       end;
                  end;

          for i:=0 to Game.BoardSize do {for each player access area}
              if   Result then begin
                   if ((Game.Board[i] and (WALL+BOX+FLOOR))=FLOOR) and
                      (not Visited[i]) then begin
                      MovePlayer(i); CalculatePlayersReachableSquares(0,0);

                      for j:=0 to Game.BoardSize do with Game.SearchStates[0].PlayersReachableSquares do
                          if  Squares[j]=TimeStamp then
                              Visited[j]:=True; {mark access area as visited}

                      if (DeadlockSetCandidate.SquareColors[i]=PLAYER_START_SQUARE_COLORS[CheckType__]) then
                         Result:=Search(0); {use the recursive search procedure to explore the possible moves}
                      end;
                   end
              else break;

          Inc(Game.DeadlockSets.SearchCount,Game.DeadlockSets.CandidateSearchCount);
        end; {Check}

      begin {CheckCandidateSet}
        {initialization}
        GamePlayerPos:=Game.PlayerPos; GameBoxCount:=Game.BoxCount; {save game state}
        for i:=1 to Game.BoxCount do GameBoxPos[i]:=Game.BoxPos[i];
        ClearBoard; {remove boxes and player from the board}

        Game.BoxCount:=DeadlockSetCandidate.Boxes.Count; {put boxes from current deadlock-set on the board}
        for i:=1 to DeadlockSetCandidate.Boxes.Count do Game.BoxPos[i]:=DeadlockSetCandidate.Boxes.Squares[i];

        InitializeBoard; {update board and existing deadlock-set capacities}

        //WriteBoardToLogFile('Deadlock candidate (Boxes only, goal squares are not part of the set)')

        {checking}

//        if Game.ShowDeadlockSetsEnabled then begin
//           ShowBoard;
//           Write('Check candidate: ');
//           Readln;
//           end;

        Result:=Check(ctCheckPushes);
        if Result then
           if   Check(ctCheckPulls) then {ok}
           else if   (DeadlockSetCandidate.CenterSquare<>0)
                     and
                     (Game.SimpleLowerBound<>0)
                     and
                     (not (dsfHasDisconnectedInnerFloors in DeadlockSetCandidate.Flags)) {if 'True', the program hasn't information on all inner floor squares at this time}
                     and
                     CalculatePushDirectionsThatOnlyArePossibleFromOutsideTheDeadlockSet(PushesFromTheOutsideOnlyDirectionSet)
                     then begin
                     {we can later determine if the player is outside the set by}
                     {checking the direction of the last push leading to this position}
                     for Direction:=Low(Direction) to High(Direction) do
                         if Direction in PushesFromTheOutsideOnlyDirectionSet then
                            Include(DeadlockSetCandidate.Flags,DIRECTION_TO_DEADLOCK_SET_FLAG[Direction]);
                     Include(DeadlockSetCandidate.Flags,dsfPlayerMustBeOutsideSet);
                     Result:=True;
                     end
                else Result:=False;

        IsALegalSetButViolatesCapacityConstraints__:=Result and (not CheckSimpleDeadlocks);

        {finalization}
        ClearBoard; {remove deadlock-set from the board}
        Game.BoxCount:=GameBoxCount; Game.PlayerPos:=GamePlayerPos; {restore game state}
        for i:=1 to Game.BoxCount do Game.BoxPos[i]:=GameBoxPos[i];
        InitializeBoard;
      end; {CheckCandidateSet}

      procedure PruneDeadlockSets;
      var i,j,SetNo,SquareNo:Integer; CommonSquaresCount:array[0..MAX_DEADLOCK_SETS] of integer;
      begin {precondition: the candidate set is legal and has been stored as the newest deadlock set}
        FillChar(CommonSquaresCount,SizeOf(CommonSquaresCount),0);

        for i:=1 to DeadlockSetCandidate.Boxes.Count do begin {for each square in the new set}
            SquareNo:=DeadlockSetCandidate.Boxes.Squares[i];
            for j:=1 to Game.DeadlockSets.SquareSetCount[SquareNo] do {for each set having this this square as a member}
                Inc(CommonSquaresCount[Game.DeadlockSets.SquareSetNumbers[SquareNo,j]]); {count number of squares in common with the candidate set}
            end;

        for SetNo:=Pred(Game.DeadlockSets.Count) downto 1 do with Game.DeadlockSets do {'Pred': the candidate was stored as the newest deadlock set}
            if (CommonSquaresCount[SetNo]=DeadlockSetCandidate.Boxes.Count) and
               ((Flags[SetNo]-[dsfPlayerIsInsideSet])=(DeadlockSetCandidate.Flags-[dsfPlayerIsInsideSet])) then begin
               DeleteDeadlockSet(SetNo); Inc(RedundantSetsCount);  {the new candidate makes the set numbered 'SetNo' superflous, hence, remove it}
               end;
      end; {PruneDeadlockSets}

    begin {CommitDeadlockSet}
      IsALegalSetButViolatesCapacityConstraints:=False; HashKey:=0;

      Result:=(DeadlockSetCandidate.Boxes.Count>0) and
              (DeadlockSetCandidate.Boxes.Count<=DeadlockSetCandidate.MaxBoxCount) and
              (DeadlockSetCandidate.GoalCount<=Game.BoxCount) and
              (Game.DeadlockSets.Count<MAX_DEADLOCK_SETS) and
              (CalculateElapsedTimeMS(Game.DeadlockSets.StartTimeMS,GetTimeMS)<=MAX_DEADLOCK_SETS_SEARCH_TIME_LIMIT_MS);

      for  i:=1 to DeadlockSetCandidate.Boxes.Count do {check for overflowing number of sets for each square}
           if   Result then begin
                Result:=Game.DeadlockSets.SquareSetCount[DeadlockSetCandidate.Boxes.Squares[i]]<MAX_DEADLOCK_SETS_PER_SQUARE;
                if (not Result) and Game.ShowDeadlockSetsEnabled then
                   Writeln('Note: Table full (squares in a deadlock set); not all detected sets are in use.');
                end
           else break;

      if   Result then begin                                                    {check that the candidate set isn't a duplicate}
           BoxSquares.Squares[0]:=0;
           for i:=1 to DeadlockSetCandidate.Boxes.Count do begin                {sort the squares using insertion sort (sorting is required for calculating a stable hashkey)}
               SquareNo:=DeadlockSetCandidate.Boxes.Squares[i]; j:=Pred(i);
               while SquareNo<BoxSquares.Squares[j] do begin
                 BoxSquares.Squares[Succ(j)]:=BoxSquares.Squares[j]; Dec(j);    {move bigger items to the right}
                 end;
               BoxSquares.Squares[Succ(j)]:=SquareNo;                           {insert new item}
               end;

           for i:=1 to DeadlockSetCandidate.Boxes.Count do                      {calculate hash-key, using sorted square-numbers}
               HashKey:=HashKey xor Game.SquareHashValues[BoxSquares.Squares[i]];

           if  not (dsfControllerSet in DeadlockSetCandidate.Flags) then        {'not': allow controller/freeze-set pairs to have identical controller sets}
               for i:=1 to Game.DeadlockSets.Count do                           {sequential search for an identical set}
                   if   Result then
                        Result:=(HashKey<>Game.DeadlockSets.HashKey[i])
                                or
                                {when searching for duplicates, only compare}
                                {normal sets against normal sets, and}
                                {controller sets against controller sets;}
                                ((dsfControllerSet in DeadlockSetCandidate.Flags)
                                 <>
                                 (dsfControllerSet in Game.DeadlockSets.Flags[i])
                                )
                   else break;
           end;

      Result:=Result and CalculateDeadlockSetCandidateInfo;

      if   Result then begin
           if   Search__ then
                Result:=CheckCandidateSet(IsALegalSetButViolatesCapacityConstraints) {check that the candidate set is legal}
           else IsALegalSetButViolatesCapacityConstraints:=not CheckSimpleDeadlocks; {simple checks only}
           end;

      if   Result then begin
           {the set is pseudo-legal, i.e., it is valid but it may violate capacity constraints imposed by other deadlock sets}
           if Result and
              (not (dsfPlayerIsInsideSet in DeadlockSetCandidate.Flags)) and
              (DeadlockSetCandidate.Capacity<0) then
              StartPositionIsOk__:=False;                                       {start position is a deadlock}

           Result:=(not IsALegalSetButViolatesCapacityConstraints) or           {is the set really legal?}
                   AcceptPseudoLegalSets__;

           if Result then begin                                                 {commit this deadlock set}
              Inc(Game.DeadlockSets.Count);                                     {update number of sets}
              Inc(Game.DeadlockSets.LastRunNo);
              Game.DeadlockSets.Capacity         [Game.DeadlockSets.Count]:=DeadlockSetCandidate.Capacity;
              Game.DeadlockSets.CenterSquare     [Game.DeadlockSets.Count]:=DeadlockSetCandidate.CenterSquare;
              Game.DeadlockSets.HashKey          [Game.DeadlockSets.Count]:=HashKey;
              Game.DeadlockSets.Flags            [Game.DeadlockSets.Count]:=DeadlockSetCandidate.Flags;
              Game.DeadlockSets.FloorCount       [Game.DeadlockSets.Count]:=DeadlockSetCandidate.PaintedFloorCount; {'PaintedFloorCount' is reliable, 'DeadlockSetCandidate.Floors.Count' isn't because it may not include the center-square and its connected floors}
              Game.DeadlockSets.RunNo            [Game.DeadlockSets.Count]:=Game.DeadlockSets.LastRunNo;
              Game.DeadlockSets.SquaresCount     [Game.DeadlockSets.Count]:=DeadlockSetCandidate.Boxes.Count;
              for i:=1 to DeadlockSetCandidate.Boxes.Count do begin             {add this set to the list for each square}
                  SquareNo:=DeadlockSetCandidate.Boxes.Squares[i];
                  Inc(Game.DeadlockSets.SquareSetCount[SquareNo]);
                  Game.DeadlockSets.SquareSetNumbers[SquareNo,Game.DeadlockSets.SquareSetCount[SquareNo]]:=Game.DeadlockSets.Count;
                  end;

              ShowDeadlockSet(Game.DeadlockSets.Count,DeadlockSetCandidate.CenterSquare);
              if (Game.DeadlockSets.AdjacentOpenSquaresLimit>1) and
                 (not Game.ShowDeadlockSetsEnabled) and
                 False {only in a console-based program, e.g., the YASS solver }
                 then
                 Write(SPACE,Game.DeadlockSets.Count);

              {PruneDeadlockSets;} {check if the new set makes any of the previously created sets redundant}
                {pruning is deferred until all sets have been expanded,}
                {(see the main body of 'CalculateDeadlockSets');}
                {the reason is that it seems to be slightly more efficient}
              end;

           if (ExpandSet__
               or
               ((not Result)
                and
                Search__
               )
              )
              and
              (DeadlockSetCandidate.MaxBoxCount<=Game.BoxCount) then {'MaxBoxCount<=...' don't risk a time-consuming calculation if 'MaxBoxCount' has an artificially high value}
              {'(not Result)': expand the set when the deadlock is only pseudo-legal;}
              {otherwise the chance to do so is lost}
              for i:=1 to DeadlockSetCandidate.Boxes.Count do
                  ExpandDeadlockSet(i,ExpandSet__,AcceptPseudoLegalSets__,StartPositionIsOk__);
           end;
    end; {CommitDeadlockSets}

    function PatternTypeNonGoalCorner:Boolean;
    var i,Item,Col,Row:Integer; WallUp,WallDown,WallLeft,WallRight:Boolean;
    begin {side-effect: finds corners, including interior ones}
      if Game.ShowDeadlockSetsEnabled then Writeln('Dead-end corner');
      Result:=True; CornerCount:=0;
      for i:=0 to Game.BoardSize do with Game do begin                           {check if 'Board[i]' is a corner}
          SquareToColRow(i,Col,Row);
          if (Col>0) and (Col<Game.BoardWidth) and (Row>0) and (Row<Game.BoardHeight) then begin
             Item        := Board[i];
             WallUp      :=(Board[i+SquareOffsetForward[Up   ]] and WALL)<>0;
             WallDown    :=(Board[i+SquareOffsetForward[Down ]] and WALL)<>0;
             WallLeft    :=(Board[i+SquareOffsetForward[Left ]] and WALL)<>0;;
             WallRight   :=(Board[i+SquareOffsetForward[Right]] and WALL)<>0;
             if ((Item and WALL)=0)                                              {not a wall}
                and
                (WallUp or WallDown)                                             {vertical   wall-neigbour}
                and
                (WallRight or WallLeft)                                          {horizontal wall-neighbor}
                then begin                                                       {'Board[i]' is a corner}
                if  (Item and GOAL)=0 then begin
                    Inc(Board[i],FLAG_ILLEGAL_BOX_SQUARE);                       {'Board[i]' is an illegal corner}
                    if (Item and BOX)<>0 then
                       Result:=False;                                            {box on illegal square: deadlock}
                    end;

                Inc(CornerCount);
                Corners[CornerCount].SquareNo:=i;
                FillChar(Corners[CornerCount].EdgeLengths,SizeOf(Corners[CornerCount].EdgeLengths),0);
                if WallUp   and WallLeft  then Include(Corners[CornerCount].Types,ctTopLeft    );
                if WallUp   and WallRight then Include(Corners[CornerCount].Types,ctTopRight   );
                if WallDown and WallLeft  then Include(Corners[CornerCount].Types,ctBottomLeft );
                if WallDown and WallRight then Include(Corners[CornerCount].Types,ctBottomRight);
                end;
             end;
          end;
    end;

    function PatternType3Block:Boolean;
    var b,g,i,j,k,m,p:Integer; Direction:TDirection;
    begin
      if Game.ShowDeadlockSetsEnabled then Writeln('3-Block');
      Result:=True;
      for i:=1 to Game.BoardSize do
          if IsAWallSquare(i) then
             for Direction:=Low(Direction) to High(Direction) do begin
                 j:=i+Game.SquareOffsetForward[Direction]; {neighbor square}
                 if (j>=0) and (j<=Game.BoardSize) and
                    IsAFloorSquare(j) and IsALegalBoxSquare(j) and IsAReachableBoxSquare(j) and
                    IsAFloorSquare(i+Game.SquareOffsetLeft[Direction]) then begin
                    k:=j+Game.SquareOffsetLeft[Direction];
                    if (k>=0) and (k<=Game.BoardSize) and
                       IsAFloorSquare(k) and IsALegalBoxSquare(k) and IsAReachableBoxSquare(k) and
                       IsAFloorSquare(k+Game.SquareOffsetLeft[Direction]) then begin
                       m:=k+Game.SquareOffsetForward[Direction];
                       if (m>=0) and (m<=Game.BoardSize) and
                          IsAFloorSquare(m) and IsALegalBoxSquare(m) and IsAReachableBoxSquare(m) and
                          IsAWallSquare (m+Game.SquareOffsetLeft [Direction]) and
                          IsAFloorSquare(m+Game.SquareOffsetRight[Direction]) then begin
                          b:=0; g:=0; p:=0;
                          CountBoxesAndGoalsAndPlayer(j,b,g,p);
                          CountBoxesAndGoalsAndPlayer(k,b,g,p);
                          CountBoxesAndGoalsAndPlayer(m,b,g,p);
                          if (g<3) and
                             InitializeDeadlockSetCandidate(2-b,0,Min(Game.BoxCount,Game.DeadlockSets.BoxLimit)) then begin
                             AddSquareToDeadlockSet(j);
                             AddSquareToDeadlockSet(k);
                             AddSquareToDeadlockSet(m);
                             CommitDeadlockSet(True,False,False,Result);
                             end;
                          end;
                       end;
                    end;
                 end;
    end;

    function PatternType4BlockA:Boolean;
    var a,b,c,f,g,h,i,j,k:Integer;
    begin {pattern type "4-Block/A" (can be hard-coded with a tiny speed increase (approx. 3%), see 'IsALegalPush'}
      if Game.ShowDeadlockSetsEnabled then Writeln('4-Block/A');
      Result:=True;
      for i:=0 to Game.BoardSize do begin
          f:=0; g:=0; b:=0; c:=0; {floors, goals, boxes, illegal squares}
          for j:=0 to 1 do
              for k:=0 to 1 do  begin
                  h:=i+j+k*Game.SquareOffsetForward[Down];
                  if h<=Game.BoardSize then begin
                     a:=Game.Board[h];
                     if (a and FLAG_ILLEGAL_BOX_SQUARE)<>0 then Inc(c);
                     if (a and FLOOR                  )<>0 then Inc(f);
                     if (a and GOAL                   )<>0 then Inc(g);
                     if (a and BOX                    )<>0 then Inc(b);
                     end
                  else Inc(c);
                  end;
          if (c=0) and (g<f) then begin
             if f=b then Result:=False;                                          {'False': start position is a deadlock}
{            // hard-coded version is used instead of deadlock-sets
             if InitializeDeadlockSetCandidate(Pred(f-b),0,Min(Game.BoxCount,Game.DeadlockSets.BoxLimit)) then begin
                for j:=0 to 1 do
                    for k:=0 to 1 do begin
                        h:=i+j+k*Game.SquareOffsetForward[dDown];
                        if (h<=Game.BoardSize) and ((Game.Board[h] and FLOOR)<>0) then
                           AddSquareToDeadlockSet(h);
                        end;
                CommitDeadlockSet(False,False,Result);
                end;
}
             end;
          end;
    end;

    function PatternType4BlockB:Boolean;
    var b,g,i,j,p,L1,L2,R1,R2:Integer; Direction:TDirection;
    begin {pattern type "4-Block/B"}
      if Game.ShowDeadlockSetsEnabled then Writeln('4-Block/B');
      Result:=True;
      for i:=0 to Game.BoardSize do
          if IsAFloorSquare(i) and IsALegalBoxSquare(i) and IsAReachableBoxSquare(i) then
             for Direction:=Low(Direction) to Succ(Low(Direction)) do begin {caution: 'Succ(Low...'): assumes 4 directions only}
                 j:=i+Game.SquareOffsetForward[Direction]; {neighbor square}
                 if IsAFloorSquare(j) and IsALegalBoxSquare(j) and IsAReachableBoxSquare(j) then begin
                    L1:=i+Game.SquareOffsetLeft [Direction];
                    R1:=i+Game.SquareoffsetRight[Direction];
                    L2:=j+Game.SquareoffsetLeft [Direction];
                    R2:=j+Game.SquareOffsetRight[Direction];
                    if (IsAFloorSquare(L1) and IsAFloorSquare(R2)) or
                       (IsAFloorSquare(R1) and IsAFloorSquare(L2)) then // avoid repeating 4-block/A by insisting on at least one floor 'diagonal'
                       if ({check if a box at 'i' can move left or right}
                           IsAWallSquare(L1)
                           or
                           IsAWallSquare(R1)
                           or
                           ((not (IsALegalBoxSquare(L1) and IsAReachableBoxSquare(L1)))
                            and {the box at 'i' will be stuck if can't move left or right, even if there aren't any wall neighbors}
                            (not (IsALegalBoxSquare(R1) and IsAReachableBoxSquare(R1)))
                           )
                          )
                          and
                          ({check if a box at 'j' can move left or right}
                           IsAWallSquare(L2)
                           or
                           IsAWallSquare(R2)
                           or
                           ((not (IsALegalBoxSquare(L2) and IsAReachableBoxSquare(L2)))
                            and {the box at 'i' will be stuck if can't move left or right, even if there aren't any wall neighbors}
                            (not (IsALegalBoxSquare(R2) and IsAReachableBoxSquare(R2)))
                           )
                          ) then begin // neigher a box at 'i' or 'j' can move left of right, hence, 'i' and 'j' might be a deadlock
                          b:=0; g:=0; p:=0;
                          CountBoxesAndGoalsAndPlayer(i,b,g,p);
                          CountBoxesAndGoalsAndPlayer(j,b,g,p);
                          if (g<2) and InitializeDeadlockSetCandidate(1-b,0,Min(Game.BoxCount,Game.DeadlockSets.BoxLimit)) then begin
                             AddSquareToDeadlockSet(i); AddSquareToDeadlockSet(j);
                             CommitDeadlockSet(True,False,False,Result);
                             end;
                          end;
                    end;
                 end;
    end;

    function PatternTypeDoubleL:Boolean;
    var i,SquareNo:Integer; d,Direction:TDirection;
    begin {pattern type "Double-L"}
      if Game.ShowDeadlockSetsEnabled then Writeln('Double-L');
      Result:=True;
      for i:=1 to Game.BoardSize do
          if IsAFloorSquare(i) then
             for Direction:=Low(Direction) to Succ(Low(Direction)) do            {caution: 'Succ(Low...'): assumes 4 directions only}
                 if InitializeDeadlockSetCandidate(-1,i,Min(Game.BoxCount,Game.DeadlockSets.BoxLimit)) then begin
                    for d:=Low(d) to High(d) do
                        if IsAFloorSquare(i+Game.SquareOffsetForward[d]) then
                           AddSquareToDeadlockSet(i+Game.SquareOffsetForward[d]);
                    SquareNo:=i-Game.SquareOffsetForward[Direction]+Game.SquareOffsetLeft [Direction];
                    if IsALegalAndReachableBoxSquare(SquareNo) and
                       (IsAFloorSquare(i-Game.SquareOffsetForward[Direction])
                        or {avoid squares on the other side of a corner}
                        IsAFloorSquare(i+Game.SquareOffsetLeft [Direction])
                       )
                       then
                       AddSquareToDeadlockSet(SquareNo);
                    SquareNo:=i+Game.SquareOffsetForward[Direction]+Game.SquareOffsetRight[Direction];
                    if IsALegalAndReachableBoxSquare(SquareNo) and
                       (IsAFloorSquare(i+Game.SquareOffsetForward[Direction])
                        or {avoid squares on the other side of a corner}
                        IsAFloorSquare(i+Game.SquareOffsetRight[Direction])
                       )
                       then
                       AddSquareToDeadlockSet(SquareNo);
                    CommitDeadlockSet(True,False,False,Result);
                    end;
    end;

    function PatternTypeBlockedTunnel:Boolean;
    var i:Integer; Direction:TDirection;
    begin {pattern type "Blocked tunnel"}
      if Game.ShowDeadlockSetsEnabled then Writeln('Blocked tunnel');
      Result:=True;
      for i:=1 to Game.BoardSize do
          if IsAFloorSquare(i) and (not IsAGoalSquare(i)) then
             for Direction:=Low(Direction) to High(Direction) do
                 if IsAWallSquare            (i+Game.SquareOffsetLeft   [Direction]) and
                    IsAWallSquare            (i+Game.SquareOffsetRight  [Direction]) and
                    IsALegalBoxSquare        (i-Game.SquareOffsetForward[Direction]) and
                    IsALegalBoxSquare        (i+Game.SquareOffsetForward[Direction]) and
                    (IsAWallSquare           (i+Game.SquareOffsetForward[Direction]+Game.SquareOffsetLeft [Direction])
                     or
                     IsAWallSquare           (i+Game.SquareOffsetForward[Direction]+Game.SquareOffsetRight[Direction])
                    ) then begin
                    if IsALegalBoxSquare     (i-Game.SquareOffsetForward[Direction]+Game.SquareOffsetRight[Direction]) and
                       InitializeDeadlockSetCandidate(-1,i,Min(Game.BoxCount,Game.DeadlockSets.BoxLimit)) then begin
                       AddSquareToDeadlockSet(i+Game.SquareOffsetForward[Direction]);
                       AddSquareToDeadlockSet(i-Game.SquareOffsetForward[Direction]);
                       AddSquareToDeadlockSet(i-Game.SquareOffsetForward[Direction]+Game.SquareOffsetRight[Direction]);
                       CommitDeadlockSet(True,False,False,Result);
                       end;
                    if IsALegalBoxSquare     (i-Game.SquareOffsetForward[Direction]+Game.SquareOffsetLeft [Direction]) and
                       InitializeDeadlockSetCandidate(-1,i,Min(Game.BoxCount,Game.DeadlockSets.BoxLimit)) then begin
                       AddSquareToDeadlockSet(i+Game.SquareOffsetForward[Direction]);
                       AddSquareToDeadlockSet(i-Game.SquareOffsetForward[Direction]);
                       AddSquareToDeadlockSet(i-Game.SquareOffsetForward[Direction]+Game.SquareOffsetLeft [Direction]);
                       CommitDeadlockSet(True,False,False,Result);
                       end;
                    end;
    end;

    function PatternTypeBlockedGateAndOneWayTunnel:Boolean;
    var BoxNo,g,GoalCount,NeighborSquare,OldPlayerPos,OppositeNeighborSquare,Square,Square2:Integer;
        Direction:TDirection;

      function MakeDeadlockCandidate(Square__,NeighborSquare__:Integer; Direction__:TDirection):Boolean;
      begin
        Result:=IsALegalAndReachableBoxSquare(NeighborSquare__) and
                (not (IsAGoalSquare(Square__) and IsAGoalSquare(NeighborSquare__))) and
                InitializeDeadlockSetCandidate(-1,0,Min(Game.BoxCount,Game.DeadlockSets.BoxLimit)) and
                AddSquareToDeadlockSet(Square__) and
                ((NeighborSquare__=Square__) or AddSquareToDeadlockSet(NeighborSquare__));
        if      Result then begin
                if (Game.Board[Square__] and BOX)=0 then Inc(DeadlockSetCandidate.Capacity);
                if (Square__<>NeighborSquare__) and ((Game.Board[NeighborSquare__] and BOX)=0) then Inc(DeadlockSetCandidate.Capacity);

                Include(DeadlockSetCandidate.Flags,DIRECTION_TO_DEADLOCK_SET_FLAG[Direction__]);
                Include(DeadlockSetCandidate.Flags,dsfPlayerMustBeOutsideSet);
                Include(DeadlockSetCandidate.Flags,dsfPlayerIsInsideSet);
                {'dsfPlayerIsInsideSet' is added because of the way the program has grown:}
                {a position is considered a deadlock if a set underflows, unless this flag is raised;}
                {instead of changing this logic now, the gate deadlocks}
                {fake that the player is inside the set}
           end;
      end;

    begin {PatternTypeBlockedGateAndOneWayTunnel}
      {$IFDEF CONSOLE_APPLICATION}
        if Game.ShowDeadlockSetsEnabled then Writeln('Blocked gate');
      {$ENDIF}
      Result:=True;
      OldPlayerPos:=Game.PlayerPos;
      MovePlayer(0);
      for BoxNo:=1 to Game.BoxCount do Dec(Game.Board[Game.BoxPos[BoxNo]],BOX);

      with Game.SearchStates[0].PlayersReachableSquares do begin
        {find all "#_#" gates on the board}
        for Square:=0 to Game.BoardSize do
            if IsALegalAndReachableBoxSquare(Square)
               and                                                              {caution: assumes 4 directions only}
               (
                (IsAWallSquare(Square+Game.SquareOffsetForward[Up])
                 and
                 IsAWallSquare(Square+Game.SquareOffsetForward[Down])
                )
                or
                (IsAWallSquare(Square+Game.SquareOffsetForward[Left])
                 and
                 IsAWallSquare(Square+Game.SquareOffsetForward[Right])
                )
               )
               then
               for Direction:=Low(Direction) to High(Direction) do begin
                   NeighborSquare        :=Square+Game.SquareOffsetForward[Direction];
                   OppositeNeighborSquare:=Square-Game.SquareOffsetForward[Direction];
                   if IsALegalAndReachableBoxSquare(NeighborSquare)
                      and
                      IsALegalAndReachableBoxSquare(OppositeNeighborSquare)
                      then begin

                      {$IFDEF HAS_PRECALCULATED_GATE_SQUARES}                   {precalculating goal squares hasn't been implemented because there is no use for it in the game-playing YASC program; the calculation is implemented in the YASS solver program}
                        if IsAGateSquare(Square) then begin                     {'True': this is a gate square that separates the board in 2 separate rooms}
                      {$ELSE}
                        Inc(Game.Board[Square],BOX);
                        MovePlayer(NeighborSquare);
                        CalculatePlayersReachableSquares(0,0);
                        Dec(Game.Board[Square],BOX);
                        MovePlayer(0);
                        if Squares[NeighborSquare]<>Squares[OppositeNeighborSquare] then begin
                      {$ENDIF}

                           for BoxNo:=1 to Game.BoxCount do Inc(Game.Board[Game.BoxPos[BoxNo]],BOX); {put boxes back on the board for correct capacity calculation}

                           GoalCount:=0;
                           if IsAGoalSquare(Square        ) then Inc(GoalCount);
                           if IsAGoalSquare(NeighborSquare) then Inc(GoalCount);

                           Square2:=NeighborSquare; {check if this is a 1-way tunnel}
                           g:=GoalCount;
                           while (g=0) and
                                 IsAWallSquare(Square2+Game.SquareOffsetLeft [Direction]) and {caution: assumes 4 directions only}
                                 IsAWallSquare(Square2+Game.SquareOffsetRight[Direction]) and {caution: assumes 4 directions only}
                                 IsALegalAndReachableBoxSquare(Square2+Game.SquareOffsetForward[Direction]) do begin
                                 Inc(Square2,Game.SquareOffsetForward[Direction]);
                                 if IsAGoalSquare(Square2) then Inc(g);
                                 end;
                           if    (g=0) and
                                 (not IsALegalAndReachableBoxSquare(Square2+Game.SquareOffsetForward[Direction])) then begin
                                 {a 1-way tunnel}
                                 NeighborSquare:=Square;
                                 end;

                           if    MakeDeadlockCandidate(Square,NeighborSquare,Direction) and
                                 CommitDeadlockSet(False,False,False,Result) then begin
                                 if (NeighborSquare=Square+Game.SquareOffsetForward[Direction]) and
                                    {try to make an extra deadlock set by pushing the secondary}
                                    {box one square further into the room or the tunnel}
                                    MakeDeadlockCandidate(Square,NeighborSquare+Game.SquareOffsetForward[Direction],Direction) then begin
                                    if IsAGoalSquare(NeighborSquare+Game.SquareOffsetForward[Direction]) then Inc(GoalCount);
                                    if GoalCount<2 then CommitDeadlockSet(False,False,False,Result);
                                    end;
                                 end;

                           GoalCount:=0;
                           if IsAGoalSquare(Square                ) then Inc(GoalCount);
                           if IsAGoalSquare(OppositeNeighborSquare) then Inc(GoalCount);

                           Square2:=OppositeNeighborSquare; {check if this is a 1-way tunnel}
                           g:=GoalCount;
                           while (g=0) and
                                 IsAWallSquare(Square2+Game.SquareOffsetLeft [Direction]) and {caution: assumes 4 directions only}
                                 IsAWallSquare(Square2+Game.SquareOffsetRight[Direction]) and {caution: assumes 4 directions only}
                                 IsALegalAndReachableBoxSquare(Square2-Game.SquareOffsetForward[Direction]) do begin
                                 Dec(Square2,Game.SquareOffsetForward[Direction]);
                                 if IsAGoalSquare(Square2) then Inc(g);
                                 end;
                           if    (g=0) and
                                 (not IsALegalAndReachableBoxSquare(Square2-Game.SquareOffsetForward[Direction])) then begin
                                 {a 1-way tunnel}
                                 OppositeNeighborSquare:=Square;
                                 end;

                           if    MakeDeadlockCandidate(Square,OppositeNeighborSquare,OPPOSITE_DIRECTION[Direction]) and
                                 CommitDeadlockSet(False,False,False,Result) then begin
                                 if (OppositeNeighborSquare=Square-Game.SquareOffsetForward[Direction]) and
                                    {try to make an extra deadlock set by pushing the secondary}
                                    {box one square further into the room or the tunnel}
                                    MakeDeadlockCandidate(Square,OppositeNeighborSquare-Game.SquareOffsetForward[Direction],OPPOSITE_DIRECTION[Direction]) then begin
                                    if IsAGoalSquare(OppositeNeighborSquare-Game.SquareOffsetForward[Direction]) then Inc(GoalCount);
                                    if GoalCount<2 then CommitDeadlockSet(False,False,False,Result);
                                    end;
                                 end;

                           for BoxNo:=1 to Game.BoxCount do Dec(Game.Board[Game.BoxPos[BoxNo]],BOX); {remove boxes again}
                           end
                        else begin
                           if (not IsAGoalSquare(NeighborSquare))
                              and
                              ((Game.Board[NeighborSquare+Game.SquareOffsetLeft [Direction]] and (WALL+FLOOR+FLAG_ILLEGAL_BOX_SQUARE))=FLOOR+FLAG_ILLEGAL_BOX_SQUARE)
                              and
                              ((Game.Board[NeighborSquare+Game.SquareOffsetRight[Direction]] and (WALL+FLOOR+FLAG_ILLEGAL_BOX_SQUARE))=FLOOR+FLAG_ILLEGAL_BOX_SQUARE) then begin
                              if InitializeDeadlockSetCandidate(1,0,Min(Game.BoxCount,Game.DeadlockSets.BoxLimit)) and
                                 AddSquareToDeadlockSet(Square) and
                                 AddSquareToDeadlockSet(NeighborSquare) then begin
                                 for BoxNo:=1 to Game.BoxCount do Inc(Game.Board[Game.BoxPos[BoxNo]],BOX); {put boxes back on the board for correct capacity calculation}
                                 if (Game.Board[Square         ] and BOX)<>0 then Dec(DeadlockSetCandidate.Capacity);
                                 if (Game.Board[NeighborSquare] and BOX)<>0 then Dec(DeadlockSetCandidate.Capacity);
                                 CommitDeadlockSet(False,False,False,Result);
                                 for BoxNo:=1 to Game.BoxCount do Dec(Game.Board[Game.BoxPos[BoxNo]],BOX); {remove boxes again}
                                 end;
                              end;
                           end;
                      end;
                   end;
        end;

      for BoxNo:=1 to Game.BoxCount do Inc(Game.Board[Game.BoxPos[BoxNo]],BOX);
      MovePlayer(OldPlayerPos);
      Result:=True;
    end;

    function PatternTypeClosedEdgeWithoutGoals:Boolean;
    var a,a1,a2,b,c,c1,d,d1,e,g,i,j,k,p,dx,dy,Col1,Row1,Col2,Row2:Integer; Direction:TDirection;
    begin {side-effect: calculates edge-lengths for each corner}
      if Game.ShowDeadlockSetsEnabled then Writeln('Closed edge without goals');
      Result:=True;
      for i:=1 to Pred(CornerCount) do with Corners[i] do begin
          SquareToColRow(SquareNo,Col1,Row1);
          for j:=Succ(i) to CornerCount do begin                                 {for each pair of corners...}
              SquareToColRow(Corners[j].SquareNo,Col2,Row2);
              if (Col1=Col2) or (Row1=Row2) then begin                           {corners are on same column or on same row}
                 dx:=Col2-Col1;  dy:=Row2-Row1;
                 k:=Pred(Max(Abs(dx),Abs(dy)));                                  {k = number of squares BETWEEN the endpoints i and j}
                 if dx<>0 then dx:=dx div Abs(dx);                               {make dx,dy to directions, i.e., -1, 0, or 1}
                 if dy<>0 then dy:=dy div Abs(dy);
                 Direction:=DxDyToDirection(dx,dy);
                 a:=SquareNo; c:=0; d:=0; c1:=0; d1:=0; b:=0; g:=0; p:=0;        {b=boxes, g=goals, p=player}

                 for e:=0 to Succ(k) do begin      {for each square [i..j]}
                     if (Game.Board[a] and WALL)=0 then begin
                        CountBoxesAndGoalsAndPlayer(a,b,g,p);
                        a1:=a+Game.SquareOffsetLeft [Direction];
                        a2:=a+Game.SquareOffsetRight[Direction];
                        if      (Game.Board[a1] and FLOOR)= 0 then Inc(c )       {the square on adjacent side 1 is blocked}
                        else if (Game.Board[a2] and FLOOR)= 0 then Inc(c1);      {opposite side is blocked, i.e., side 2}
                        if      (Game.Board[a2] and FLOOR)= 0 then Inc(d )       {the square on adjacent side 2 is blocked}
                        else if (Game.Board[a1] and FLOOR)= 0 then Inc(d1);      {opposite side is blocked, i.e., side 1}
                        Inc(a,Game.SquareOffsetForward[Direction]);
                        end
                     else break; {non-floor square: the direct line between endpoints i and j is broken}
                     end;

                 if (c+c1=k+2) or (d+d1=k+2) then begin
                    {the squares between the end-points are free, and one of the adjacent sides are blocked, or each square is blocked by a wall on either side,}
                    {thus, a box can never get away from the line, i.e., it's a 'closed edge'}

                    Corners[i].EdgeLengths[Direction]:=k+2;                      {save the line for later calculation of adjacent-line patterns ('+2': including end-points)}

                    if b>g then Result:=False;                                   {more boxes than goals: deadlock}

                    if g=0 then                                                  {no goals between or at the endpoints}
                       for e:=1 to k do                                          {flag squares as simple illegal squares}
                           Game.Board[SquareNo+e*Game.SquareOffsetForward[Direction]]:=Game.Board[SquareNo+e*Game.SquareOffsetForward[Direction]] or FLAG_ILLEGAL_BOX_SQUARE;

                    if IsAGoalSquare(Corners[j].SquareNo) then
                       Corners[j].EdgeLengths[OPPOSITE_DIRECTION[Direction]]:=   {creating union-sets requires that}
                         Corners[i].EdgeLengths[Direction];                      {the other corner knows about this edge}
                    end;
                 end;
              end;
          end;
    end;

    function PatternTypeClosedEdgeWithGoals:Boolean;
    var a,b,g,i,j,p,NewSetsCount:Integer; IsAGoalCorner:Boolean; d,Direction:TDirection;
        SetCapacities:array[TDirection] of Integer;
        SetSquares   :array[TDirection] of TBoardSquareSet;
    begin {pattern type "Closed edge with goals"}
      if Game.ShowDeadlockSetsEnabled then Writeln('Closed edge with goals');
      Result:=True;
      for i:=1 to CornerCount do with Corners[i] do begin
          NewSetsCount :=0;                                                      {'0': no deadlock sets created for this corner yet}
          IsAGoalCorner:=(Game.Board[SquareNo] and (GOAL+FLAG_ILLEGAL_BOX_SQUARE+FLAG_BOX_REACHABLE_SQUARE))
                           = GOAL+FLAG_BOX_REACHABLE_SQUARE;

          for Direction:=Low(Direction) to High(Direction) do begin
              SetSquares[Direction].Count:=0;
              if EdgeLengths[Direction]<>0 then begin                            {for each closed edge starting from this corner...}
                 a:=SquareNo; b:=0; g:=0; p:=0;                                  {b=boxes, g=goals, p=player}
                 for j:=1 to EdgeLengths[Direction] do begin                     {for each floor-square along the edge}
                     CountBoxesAndGoalsAndPlayer(a,b,g,p);
                     Inc(a,Game.SquareOffsetForward[Direction]);
                     end;

                 if b>g then Result:=False;                                      {more boxes than goals: deadlock}

                 if (g<>0) and                                                   {'g<>0': goals on the closed edge}
                    InitializeDeadlockSetCandidate(g-b,0,Game.BoardSize) then begin {'BoardSize': allow more box-squares than boxes on the board}
                    a:=SquareNo;
                    for j:=1 to EdgeLengths[Direction] do begin                  {add each square in the line to the deadlock-set}
                        if IsALegalBoxSquare(a) and IsAReachableBoxSquare(a) then
                           AddSquareToDeadlockSet(a);
                        Inc(a,Game.SquareOffsetForward[Direction]);
                        end;

                    if IsAGoalCorner and
                       (DeadlockSetCandidate.Boxes.Count>0) and
                       (EdgeLengths[Direction]>2) then begin
                       Inc(NewSetsCount);
                       SetCapacities[Direction]:=DeadlockSetCandidate.Capacity;  {save the set information for later}
                       SetSquares[Direction]   :=DeadlockSetCandidate.Boxes;
                       end;

                    CommitDeadlockSet(True,False,False,Result);
                    end;
                 end;
              end;

          if NewSetsCount>1 then                                                 {'True': make union sets (the sets shares the corner goal square)}
             for Direction:=Low(Direction) to High(Direction) do
                 if (SetSquares[Direction].Count<>0) and
                    InitializeDeadlockSetCandidate(SetCapacities[Direction],0,Game.BoardSize) then begin
                    DeadlockSetCandidate.Boxes:=SetSquares[Direction];
                    if Direction<High(Direction) then
                       for d:=Succ(Direction) to High(Direction) do
                           if SetSquares[d].Count<>0 then with SetSquares[d] do
                              for j:=1 to Count do begin
                                  a:=Squares[j];
                                  if a<>SquareNo then begin
                                     AddSquareToDeadlockSet(a);                     {add the squares from this set}
                                     if   IsABoxSquare(a) then
                                          if   IsAGoalSquare(a) then {}
                                          else Dec(DeadlockSetCandidate.Capacity)
                                     else if   IsAGoalSquare(a) then
                                               Inc(DeadlockSetCandidate.Capacity);
                                     end;
                                  end;
                    if  DeadlockSetCandidate.Boxes.Count<>SetSquares[Direction].Count then {'True': extra squares were added}
                        CommitDeadlockSet(True,False,True,Result);
                 end;
          end;
    end;

    function PatternTypeClosedEdgeFence:Boolean;
    var a,b,g,i,j,p,ClosedEdgeCapacity:Integer; Direction:TDirection;
        IsIllegalSquare:TBoardOfBoolean;
    begin {pattern type "Closed edge fence"}
      if Game.ShowDeadlockSetsEnabled then Writeln('Closed edge fence');
      Result:=True;
      for i:=1 to Pred(CornerCount) do with Corners[i] do
          for Direction:=Low(Direction) to High(Direction) do
              if (EdgeLengths[Direction]<>0) and                                 {for each closed edge starting from this corner...}
                 InitializeDeadlockSetCandidate(-1,SquareNo,Min(Game.BoxCount,Game.DeadlockSets.BoxLimit)) then begin
                 InitializeSquareColors(DeadlockSetCandidate.SquareColors);
                 a:=SquareNo; b:=0; g:=0; p:=0;                                  {b=boxes, g=goals, p=player}
                 for j:=0 to Pred(EdgeLengths[Direction]) do begin
                     CountBoxesAndGoalsAndPlayer(a,b,g,p);
                     DeadlockSetCandidate.SquareColors[a]:=scLightGray;                     {mark each square along the edge as visited}
                     Inc(a,Game.SquareOffsetForward[Direction]);                 {next square on the closed edge}
                     end;

                 ClosedEdgeCapacity:=g-b; a:=SquareNo;
                 if ClosedEdgeCapacity<=0 then                                   {filled closed edge: no boxes can advance to}
                    for j:=0 to Pred(EdgeLengths[Direction]) do begin            {the center line, hence mark all squares}
                        IsIllegalSquare[a]:=(Game.Board[a] and FLAG_ILLEGAL_BOX_SQUARE)<>0; {along the center line as illegal}
                        Game.Board[a]:=Game.Board[a] or FLAG_ILLEGAL_BOX_SQUARE;
                        Inc(a,Game.SquareOffsetForward[Direction]);              {next square on the closed edge}
                        end;

                 for b:=0 to 1 do {first pass: fill with boxes; second pass: try to fence in any remaining floor-squares}
                     for g:=0 to 1 do                                            {left and right adjacent lines}
                         for j:=0 to Pred(EdgeLengths[Direction]) do             {for each square along the edge}
                             if DeadlockSetCandidate.GoalCount<=Game.BoxCount then begin
                                a:=SquareNo+j*Game.SquareOffsetForward[Direction];
                                if g=0 then Inc(a,Game.SquareOffsetLeft [Direction])
                                else        Inc(a,Game.SquareOffsetRight[Direction]);
                                if DeadlockSetCandidate.SquareColors[a]=scWhite then
                                   if b=0 then TryBox(a)                         {first pass: try to fill adjacent lines with boxes}
                                   else if not TryFloor(a) then                  {second pass: try to fence in any remaining floor-squares}
                                           DeadlockSetCandidate.GoalCount:=Succ(Game.BoxCount);
                                end;

                 CommitDeadlockSet(True,True,False,Result);                      {'True': expand sets while goals on the closed edge are set up most favourably}

                 a:=SquareNo;
                 if ClosedEdgeCapacity<=0 then                                   {restore board flags}
                    for j:=0 to Pred(EdgeLengths[Direction]) do begin
                        if not IsIllegalSquare[a] then Dec(Game.Board[a],FLAG_ILLEGAL_BOX_SQUARE); {Game.Board[a]:=Game.Board[a] and (not FLAG_ILLEGAL_BOX_SQUARE);}
                        Inc(a,Game.SquareOffsetForward[Direction]);              {next square on the closed edge}
                        end;
                 end;
    end;

    function PatternTypeFrozenGoals:Boolean;
    var i,BoxNo,GoalNo,GoalSquare,NeighborSquare,OldPlayerPosition:Integer;
        Direction:TDirection;

      function FindDeadlocks(Goal1__,Goal2__:Integer; IsAFreezingGoalSet__:Boolean):Boolean;
      {'Goal1__' and 'Goal2__' are the freeze-set goal squares; 'Goal2__' may be 0, i.e., not used; 'IsAFreezingGoalSet__' indicates whether boxes freeze at the goal squares in the set when all the squares are filled}
      var BoxNo,GoalNo,GoalSquare,GoalsInSetCount,i,j,NeighborSquare,OldCount,Square,UnreachableGoalsCount:Integer;
          Direction:TDirection;
          OldBoardSquareValues:array[0..MAX_BOXES] of Byte;
          Distances:TBoardOfInteger;
      begin {precondition: if 'IsAFreezingGoalSet__' is 'False' (boxes at the goal squares don't freeze), then the goal set must contain one single goal only}
        with Game do begin
          Result:=False; GoalsInSetCount:=0;
          for GoalNo:=1 to GoalCount do begin
              GoalSquare                  :=GoalPos[GoalNo];
              OldBoardSquareValues[GoalNo]:=Board[GoalSquare]; {keep the original square value for the goal square}
              if (GoalNo=Goal1__) or (GoalNo=Goal2__) then begin
                 Board               [GoalSquare]:=WALL; {put a wall at the goal square}
                 with SearchStates[0].PlayersReachableSquares do Squares[GoalSquare]:=High(TimeStamp) and (not 1); {temporarily give the goal square the timestamp assigned to walls}
                 Inc(GoalsInSetCount);
                 end;
              end;

          if (GoalsInSetCount=1) or IsAFreezingGoalSet__ then begin {the freeze-set deadlock detection only handle A) frozen goal sets, or B) single goal squares requiring a test whether a box at the goal square has frozen}

             CalculateDistanceToNearestBoxStartPositionForAllSquares(False,Distances); {calculate the set of squares that still are reachable from the starting position now that frozen boxes (represented by walls) have been added to the board}

             UnreachableGoalsCount:=0;
             for GoalNo:=1 to GoalCount do
                 if not ((GoalNo=Goal1__) or (GoalNo=Goal2__)) then
                    if   Distances[GoalPos[GoalNo]]<>INFINITY then {'True': the goal 'GoalNo' is reachable from the starting position}
                         Dec(Board[GoalPos[GoalNo]],GOAL) {temporarily disable all reachable goals not belonging to the freeze-set}
                    else Inc(UnreachableGoalsCount); {the goal number 'GoalNo' is not reachable from the starting position (anymore) with boxes frozen at the goals in the set}

             if UnreachableGoalsCount<>0 then begin {'True': some goals are not reachable from the starting position with boxes frozen at the goals in the set}
                if CalculateDistanceToNearestGoalForAllSquares_B(JobNo,False,Distances) and {calculate the set of squares (the controller-set) from which the unreachable goals can be reached}
                   (DeadlockSets.Count<MAX_DEADLOCK_SETS-2) and
                   InitializeDeadlockSetCandidate(UnreachableGoalsCount,0,BoardSize) then begin
                   OldCount:=DeadlockSets.Count;
                   DeadlockSetCandidate.Flags:=[dsfControllerSet,dsfPlayerMustBeOutsideSet];  {controller-sets "cheat" by having the "dsfPlayerMustBeOutsideSet" flag, thereby bypassing legal push tests in 'IsALegalPush' and 'DoPush'}
                   for Square:=0 to BoardSize do
                       if (not IsAWallSquare(Square)) and (Distances[Square]<>INFINITY) then
                          if   AddSquareToDeadlockSet(Square) then begin {collect the floor squares from which the unreachable goals can be reached}
                               if (Board[Square] and BOX)<>0 then Dec(DeadlockSetCandidate.Capacity); {update the deadlock set capacity}
                               end
                          else OldCount:=-1; {collecting the squares belonging to the controller-set failed; no new deadlock sets has been registered yet}
                   if  (OldCount=DeadlockSets.Count) and
                       (DeadlockSetCandidate.GoalCount=UnreachableGoalsCount) and
                       CommitDeadlockSet(False,False,False,Result) and
                       InitializeDeadlockSetCandidate(Pred(GoalsInSetCount),0,GoalsInSetCount) then begin
                       {freeze-sets "cheat" by having the "dsfPlayerMustBeOutsideSet" flag, thereby bypassing legal push tests in 'IsALegalPush' and 'DoPush'}
                       if   IsAFreezingGoalSet__ then
                            DeadlockSetCandidate.Flags:=[dsfFreezeSet                         ,dsfPlayerMustBeOutsideSet]
                       else DeadlockSetCandidate.Flags:=[dsfFreezeSet,dsfTestForFreezingSquare,dsfPlayerMustBeOutsideSet];

                       for GoalNo:=1 to GoalCount do
                           if (GoalNo=Goal1__) or (GoalNo=Goal2__) then {'True': the goal is a member of the "controller-set"}
                              if   AddSquareToDeadlockSet(GoalPos[GoalNo]) then begin
                                   if (Board[GoalPos[GoalNo]] and BOX)<>0 then Dec(DeadlockSetCandidate.Capacity); {update the deadlock set capacity}
                                   end;
                       if (DeadlockSetCandidate.Boxes.Count=GoalsInSetCount) and {'True': all goal squares in the "controller-set" were successfully registered as members of the set}
                          CommitDeadlockSet(False,False,False,Result) then begin
                          {the freeze-set "cheats" by having the "dsfPlayerMustBeOutsideSet" flag for more efficient normal flow in 'IsALegalPush' and 'DoPush'}
                          Include(DeadlockSets.Flags[DeadlockSets.Count],dsfPlayerMustBeOutsideSet);

                          if (GoalsInSetCount=1) and
                             IsAFreezingGoalSet__ and
                             (DeadlockSets.Count<MAX_DEADLOCK_SETS-2) then begin
                             {the freezing goal square is a corner square like
                              this:

                              #. <- freezing goal square
                              ##

                              some corner goals have two neighboring floor squares
                              where at least one of them is a goal, and where
                              boxes at these squares cannot escape from the
                              corner, like this example:

                              #*
                              #.$ <- goal square surrounded by two boxes
                              ###

                              in that case, the two neighboring squares can act as
                              a freeze-set for the same controller-set as the one
                              created for the original freezing goal square;

                              the following code detects the situation, duplicates
                              the controller-set, and makes a new freeze-set
                              containing the two neighbor squares;
                             }
                             GoalNo:=Goal1__;
                             if GoalNo=0 then GoalNo:=Goal2__;
                             if (GoalNo>0) and (GoalNo<=GoalCount) then begin {sanity check; this should always be true}
                                GoalSquare:=GoalPos[GoalNo];
                                for Direction:=Low(Direction) to High(Direction) do begin {look for neighboring goal squares}
                                    Square        :=GoalSquare+SquareOffsetForward[Direction];
                                    NeighborSquare:=GoalSquare+SquareOffsetForward[NEXT_DIRECTION[Direction]];
                                    if (((OriginalBoard[Square        ] and GOAL)<>0)
                                        or
                                        ((OriginalBoard[NeighborSquare] and GOAL)<>0) {'True': the freezing corner goal square is surrounded by two neighbors where at least one of them is a goal square; caution: assume 4 directions only}
                                       )
                                       and
                                       ((Board[Square                 ] and (WALL+GOAL))= 0)
                                       and
                                       ((Board[NeighborSquare         ] and (WALL+GOAL))= 0) {'True': the neighboring squares are both floor squares and not members of the unreachable goal squares}
                                       and
                                       IsAWallSquare(GoalSquare      -SquareOffsetForward[Direction])
                                       and
                                       IsAWallSquare(GoalSquare      -SquareOffsetForward[NEXT_DIRECTION[Direction]]) {'True': the goal square is really a corner square (a sanity check; this should always be true)}
                                       and
                                       IsAWallSquare(Square          +SquareOffsetRight[Direction]) {caution: assumes 4 directions only}
                                       and
                                       IsAWallSquare(NeighborSquare  +SquareOffsetLeft[NEXT_DIRECTION[Direction]]) {'True': boxes at the 2 neighboring squares cannot escape from the corner} {caution: assumes 4 directions only}
                                       and
                                       (DeadlockSets.Count<MAX_DEADLOCK_SETS-2)
                                       and
                                       InitializeDeadlockSetCandidate(UnreachableGoalsCount,0,BoardSize) then begin
                                       Board[GoalPos[GoalNo]]:=OldBoardSquareValues[GoalPos[GoalNo]]; {temporarily restore the original board value for the freezing corner goal square}

                                       i:=GoalNoAtSquare(Square);
                                       j:=GoalNoAtSquare(NeighborSquare);

                                       DeadlockSetCandidate.Flags:=[dsfControllerSet,dsfPlayerMustBeOutsideSet];  {controller-sets "cheat" by having the "dsfPlayerMustBeOutsideSet" flag, thereby bypassing legal push tests in 'IsALegalPush' and 'DoPush'}
                                       for Square:=0 to BoardSize do
                                           if (not IsAWallSquare(Square)) and (Distances[Square]<>INFINITY) then
                                              if   AddSquareToDeadlockSet(Square) then begin {collect the floor squares from which the unreachable goals can be reached}
                                                   if (Board[Square] and BOX)<>0 then Dec(DeadlockSetCandidate.Capacity); {update the deadlock set capacity}
                                                   end
                                              else i:=-Abs(i); {collecting the squares belonging to the controller-set failed; no new deadlock sets have been registered yet}
                                       if   (i>0) and
                                            (DeadlockSetCandidate.GoalCount=UnreachableGoalsCount) and
                                            CommitDeadlockSet(False,False,False,Result) then begin {'True': creating the "controller-set" succeeded}
                                            if InitializeDeadlockSetCandidate(1,0,2) then begin
                                               DeadlockSetCandidate.Flags:=[dsfFreezeSet,dsfPlayerMustBeOutsideSet]; {freeze-sets "cheat" by having the "dsfPlayerMustBeOutsideSet" flag, thereby bypassing legal push tests in 'IsALegalPush' and 'DoPush'}
                                               if   AddSquareToDeadlockSet(GoalPos[i]) and
                                                    AddSquareToDeadlockSet(GoalPos[j]) then begin
                                                    if (Board[GoalPos[i]] and BOX)<>0 then Dec(DeadlockSetCandidate.Capacity); {update the deadlock set capacity}
                                                    if (Board[GoalPos[j]] and BOX)<>0 then Dec(DeadlockSetCandidate.Capacity); {update the deadlock set capacity}
                                                    end
                                               else i:=-Abs(i); {adding the goal squares belonging to the freeze-set failed}
                                               if   (i>0) and {'True': all goal squares in the "freeze-set" were successfully registered as members of the set}
                                                    CommitDeadlockSet(False,False,False,Result) then begin {'True': registering the "freeze-set" succeeded}
{
                                                    for BoxNo:=1 to BoxCount do Board[BoxPos[BoxNo]]:=Board[BoxPos[BoxNo]] and (not BOX); // remove all boxes from the board
                                                    ShowBoard;
                                                    Write(GoalPos[i],SPACE,GoalPos[j],SPACE);
                                                    Write('Frozen goal square set ',DeadlockSets.Count);
                                                    Readln;
}
                                                    end
                                               else i:=-Abs(i); {creating the freeze-set failed}
                                               end;

                                            if   i<=0 then begin {'True': creating the freeze-set failed}
                                                 if dsfControllerSet in DeadlockSets.Flags[DeadlockSets.Count] then {sanity check; this should always be true}
                                                    DeleteDeadlockSet(DeadlockSets.Count); {destroy the controller-set}
                                                 WritelnToLogFile(''); {emit a message to the log file, so the reader won't be confused by the text accompanying the controller-set which refers to a succeeding freeze-set which isn't there}
                                                 WritelnToLogFile(TEXT_CREATE_FREEZE_DEADLOCK_SET_FAILED);
                                                 end;
                                            end;

                                       Board[GoalPos[GoalNo]]:=WALL; {put the wall back at the original freezing goal square}
                                       end;
                                    end;
                                end;
                             end;
                          end
                       else begin {{creating the freeze-set failed; emit a message to the log file, so the reader won't be confused by the text accompanying the controller-set which refers to a succeeding freeze-set which isn't there}
                          WritelnToLogFile('');
                          WritelnToLogFile(TEXT_CREATE_FREEZE_DEADLOCK_SET_FAILED);
                          end;
                       end;

                   if OldCount>=0 then {'True': an attempt has been made to create at least one of the deadlock sets in the controller/freeze-set pair}
                      if   not (Odd(DeadlockSets.Count-OldCount)) then with DeadlockSets do begin {'True': creating both deadlock sets in the controller/freeze-set pair(s) succeeded}
                           end
                      else {creating the controller/freeze-set pair failed; restore the deadlock sets to the old state}
                           while DeadlockSets.Count>OldCount do DeleteDeadlockSet(DeadlockSets.Count);
                   end;
                end;

             for GoalNo:=1 to GoalCount do begin
                 GoalSquare:=GoalPos[GoalNo];
                 if not ((GoalNo=Goal1__) or (GoalNo=Goal2__)) then
                    Board[GoalSquare]:=Board[GoalSquare] or GOAL; {put goals back on the board, except the ones in the goal freeze-set currently under investigation, i.e., the goals in the set}
                 end;

             if IsAFreezingGoalSet__ and
                CalculateDistanceToNearestGoalForAllSquares_B(JobNo,False,Distances) then begin {calculate the set of squares from which the unfrozen goals can be reached}

                for Square:=0 to BoardSize do {find floor squares from which it's impossible to reach a goal square with the frozen goals on the board}
                    if IsALegalAndReachableBoxSquare(Square) and {'True': boxes can reach the square on the normal board without the frozen goals}
                       (Distances[Square]=INFINITY) then begin {'True': with the frozen goals on the board it's impossible to get from this square to a goal, i.e., it's a "dead square"}
                       for BoxNo:=1 to BoxCount do Board[BoxPos[BoxNo]]:=Board[BoxPos[BoxNo]] and (not BOX); {remove all boxes from the board so 'IsAFreezingMove' returns the correct result, with only the frozen goals and the "dead square"}
                       if (not IsAFreezingMove(0,Square,0,False,False)) and {'True': a box can be put at the square without freezing}
                          InitializeDeadlockSetCandidate(GoalsInSetCount,0,BoardSize) then begin {make a deadlock set consisting of the frozen goals + the "dead square"}
                          for BoxNo:=1 to Game.BoxCount do Inc(Board[BoxPos[BoxNo]],BOX); {put all boxes back on the board so the deadlock set capacity can be calculated corectly}
                          for GoalNo:=1 to GoalCount do
                              if (GoalNo=Goal1__) or (GoalNo=Goal2__)  then {'True': the goal is a member of the "controller-set"}
                                 if AddSquareToDeadlockSet(GoalPos[GoalNo]) and
                                    ((Board[GoalPos[GoalNo]] and BOX)<>0) then Dec(DeadlockSetCandidate.Capacity); {update the deadlock set capacity}
                          if AddSquareToDeadlockSet(Square) and
                             ((Board[Square] and BOX)<>0) then Dec(DeadlockSetCandidate.Capacity); {update the deadlock set capacity}
                          if (DeadlockSetCandidate.Boxes.Count=Succ(GoalsInSetCount)) and {'True': all goal squares in the "controller-set" were successfully registered as members of the set, and so was the unreachle square}
                             CommitDeadlockSet(False,False,False,Result) then begin
                             end;
                          end;
                       end;
                end;
             end;

          for GoalNo:=1 to GoalCount do begin
              GoalSquare:=GoalPos[GoalNo];
              if   (GoalNo=Goal1__) or (GoalNo=Goal2__) then begin
                   Board[GoalSquare]:=OldBoardSquareValues[GoalNo]; {restore the board square value for the goals in the goal freeze-set currently under investigation}
                   with SearchStates[0].PlayersReachableSquares do Squares[GoalSquare]:=0; {remove the temporary 'wall' timestamp from the goal squares in the goal freeze-set currently under investigation}
                   end;
              end;

          for BoxNo:=1 to BoxCount do Board[BoxPos[BoxNo]]:=Board[BoxPos[BoxNo]] and (not BOX); {remove all boxes from the board}
          end;
      end; {FindDeadlocks}

    begin {pattern types "Frozen goals block access to other goals" and "Frozen goals block paths from squares to goals"}
      with Game do begin
        {$IFDEF CONSOLE_APPLICATION}
          if ShowDeadlockSetsEnabled then Writeln('Frozen goals blocking access to other goals');
        {$ENDIF}
        Result:=True; //exit;
        //ShowDeadlockSetsEnabled:=True;

        OldPlayerPosition:=PlayerPos; MovePlayer(0); {remove player from the board}
        for BoxNo:=1 to Game.BoxCount do Dec(Board[BoxPos[BoxNo]],BOX); {remove all boxes from the board}

        for GoalNo:=1 to GoalCount do begin
            GoalSquare:=GoalPos[GoalNo];
            Dec(Board[GoalSquare],GOAL); {remove the goal from the board}

            {find deadlocks based on this single goal square}
            FindDeadlocks(GoalNo,0,IsAFreezingMove(0,GoalSquare,0,False,False));  {if 'IsAFreezingMove' is 'True' then the goal square is a corner square, i.e., a box freezes at the goal square}

            {find deadlocks based on two freezing neighbor goal squares}
            for Direction:=Low(Direction) to TDirection(Pred(Ord(Low(Direction))+(DIRECTION_COUNT div 2))) do begin {look for neighboring goal squares which freezes when they are filled with boxes}
                Board[GoalSquare]:=Board[GoalSquare] or BOX;
                NeighborSquare:=GoalSquare+SquareOffsetForward[Direction];
                if IsAGoalSquare(NeighborSquare) and IsAFreezingMove(0,NeighborSquare,0,False,False) then
                   for i:=1 to GoalCount do {find the goal number for the goal square 'NeighborSquare'}
                       if GoalPos[i]=NeighborSquare then begin
                          FindDeadlocks(GoalNo,i,True);
                          break;
                          end;
                Board[GoalSquare]:=Board[GoalSquare] and (not BOX);
                end;

            Inc(Board[GoalSquare],GOAL); {put the goal back on the board}
            end;

        for BoxNo:=1 to Game.BoxCount do Inc(Board[BoxPos[BoxNo]],BOX); {put all boxes back on the board}
        MovePlayer(OldPlayerPosition); {restore player position}
        end;
    end;

    function PatternTypeFencedInArea:Boolean;
    var i,SetNo:Integer;
    begin {pattern type "Fenced-in area"}
      if Game.ShowDeadlockSetsEnabled then Writeln('Fenced-in area');
      Result:=True; SetNo:=0;
      while SetNo<Game.DeadlockSets.Count do with Game.DeadlockSets do begin    {for each deadlock set}
        Inc(SetNo);
        if CenterSquare[SetNo]<>0 then begin
           LoadCandidateFromSet(SetNo);
{
           if dsfDisconnectedInnerFloors in DeadlockSetCandidate.Flags then begin
              Writeln(FloorCount[SetNo],SLASH,DeadlockSetCandidate.PaintedFloorCount);
              Game.ShowDeadlockSetsEnabled:=True;
              ShowDeadlockSet(SetNo,CenterSquare[SetNo]);
              Game.ShowDeadlockSetsEnabled:=False;
              end;
}
           for i:=1 to DeadlockSetCandidate.Boxes.Count do
               ExpandDeadlockSet(i,True,False,Result)                           {'True': try to expand the deadlock set}
           end;
        end;
    end;

begin {CalculateDeadlockSets}
  with Game do begin
    {initialization}
    DeadlockSets.StartTimeMS:=GetTimeMS; Result:=True;    {if 'False': start position is a deadlock}
    CreateLogFile(MainForm.ApplicationDataPath+ExtractFileName(Application.ExeName));
    CalculateDeadlockSetsJobNo:=JobNo;                    {if job number changes then the user abandoned this calculation}

    DeadlockSets.Count:=0; DeadlockSets.LoggedCount:=0;
    DeadlockSets.LastRunNo:=0; DeadlockSets.RedundantSetsCount:=0;
    DeadlockSets.OverflowingDeadlockSet:=0; DeadlockSets.RunNo[0]:=0;
    FillChar(DeadlockSets.SquareSetCount,SizeOf(DeadlockSets.SquareSetCount),0);
    DeadlockSets.Flags[0]:=[]; {so the first deadlock set cannot be mistaken for a freeze-set in a controller/freeze-set pair}
    DeadlockSets.ControllerAndFreezeSetPairsEnabled:=False; {'False: 'IsALegalPush' ignores controller/freeze-set pairs while deadlock sets are being calculated}
    FillChar(Corners,SizeOf(Corners),0);

    if Game.LogFileName<>'' then WriteBoardToLogFile(Game.Title);

//  ShowDeadlockSetsEnabled:=True;

    if      Game.ShowDeadlockSetsEnabled then begin
            Writeln('*** ',Game.Title,' ***');
            ShowBoard;
            end
    else if Game.DeadlockSets.AdjacentOpenSquaresLimit>1 then
            //Write('Calculating deadlocks...')
            ;

    {first step is to find corners, including interior ones, and mark non-goal corners as illegal squares}
    Result:=PatternTypeNonGoalCorner and Result;

    {next step is to find closed edges and to mark simple illegal squares on these edges}
    Result:=PatternTypeClosedEdgeWithoutGoals and Result;

    {now that simple illegal squares are marked, it's possible to calculate squares reachable for at least 1 box}
    CalculateReachableSquaresForAllBoxes;
    {and distance to nearest goal for all squares can also be calculated}
    Result:=CalculateDistanceToNearestGoalForAllSquares_B(CalculateDeadlockSetsJobNo,True,Game.DistanceToNearestGoal) and Result;
    Game.DistanceToNearestTarget:=Game.DistanceToNearestGoal; {initialize distance to nearest target for forward gameplay}
    if Result then CalculateDistanceToNearestBoxStartPositionForAllSquares(False, Game.DistanceToNearestBoxStartPosition);

    ShowIllegalBoxSquares; {actually, this procedure only shows the results if 'ShowDeadlockSetsEnabled' is 'True'}

    {generate remaining deadlock types in an order which limits the number of futile sets}

    if MainForm.Deadlocks.ThreadState=dlIsCalculating then
       Result:=Result and PatternTypeClosedEdgeWithGoals;

    if MainForm.Deadlocks.ThreadState=dlIsCalculating then
       Result:=Result and PatternType4BlockA;

    if MainForm.Deadlocks.ThreadState=dlIsCalculating then
       Result:=Result and PatternType4BlockB;

    if MainForm.Deadlocks.ThreadState=dlIsCalculating then
       Result:=Result and PatternTypeBlockedGateAndOneWayTunnel;

    if MainForm.Deadlocks.ThreadState=dlIsCalculating then
       Result:=Result and PatternTypeDoubleL;

    {there is an overlap between 'Double-L' and '3-Blocks' but the latter}
    {haven't any center-squares and cannot be expanded like 'double-L'}
    {deadlocks; therefore, 'Double-L' deadlocks must be generated first;}

    if MainForm.Deadlocks.ThreadState=dlIsCalculating then
       Result:=Result and PatternType3Block;

    if MainForm.Deadlocks.ThreadState=dlIsCalculating then
       Result:=Result and PatternTypeBlockedTunnel;

    if MainForm.Deadlocks.ThreadState=dlIsCalculating then
       Result:=Result and PatternTypeFencedInArea;

    if MainForm.Deadlocks.ThreadState=dlIsCalculating then
       {do the most complex sets last in order to have simpler sets available for pruning}
       Result:=Result and PatternTypeClosedEdgeFence;

    if MainForm.Deadlocks.ThreadState=dlIsCalculating then
       Result:=Result and PatternTypeFrozenGoals;

    DeadlockSets.ControllerAndFreezeSetPairsEnabled:=True; {'True': 'IsALegalPush' takes controller/freeze-set deadlock pairs into account}

    ClearBoard; {remove boxes and player from the board}

    if MainForm.Deadlocks.ThreadState=dlIsCalculating then with DeadlockSets do
       for i:=Count downto 1 do
           if (CenterSquare[i]<>0) and IsARedundantDeadlockSet(i) then begin
              DeleteDeadlockSet(i); Inc(RedundantSetsCount);
              end;

    if DeadlockSets.RedundantSetsCount<>0 then with DeadlockSets do begin
       WritelnToLogFile('');
       WritelnToLogFile('Redundant deadlock sets:');
       for i:=0 to Pred(Count) do
           for j:=Succ(RunNo[i]) to Pred(RunNo[Succ(i)]) do
               WritelnToLogFile(SPACE+SPACE+IntToStr(j));
       end;

    if (Game.DeadlockSets.AdjacentOpenSquaresLimit>1) and
       (not Game.ShowDeadlockSetsEnabled) then
       //Writeln
       ;

    Game.DeadlockSets.TimeMS:=CalculateElapsedTimeMS(DeadlockSets.StartTimeMS,GetTimeMS);

    if Game.DeadlockSets.Count<>0 then begin
       WritelnToLogFile('');
       WritelnToLogFile('Active deadlock sets: '+IntToStr(Game.DeadlockSets.Count));
       if Game.DeadlockSets.RedundantSetsCount<>0 then
          WritelnToLogFile('Redundant deadlock sets: '+IntToStr(Game.DeadlockSets.RedundantSetsCount));
       WritelnToLogFile('Time: '+IntToStr(Game.DeadlockSets.TimeMS)+' milli seconds');
       WritelnToLogFile('');
       end;

    CloseLogFile;
    end;
end; {CalculateDeadlockSets}

function IsALegalPosition(BoxNo__,MaxPushCount__:Integer;
                          MaxTimeMS__:TTimeMS;
                          ReverseMode__ : Boolean;
                          var DeadlockedBoxCount__:Integer; var DeadlockedBoxSquares__:TBoxPositions):Boolean;
{returns 'False' is the board contains a deadlocked "fence" including 'BoxNo__'; use 'BoxNo__ = 0' to test all fences}
var
  i,j,Col,Row,FenceBoxCount,SquareNo: Integer;
  ThisJobNo:Int64;
  Visited:TBoardOfBoolean;
  OriginalBoard:TBoard;
  OriginalBoardState:TBoardState;
  History:TSearchHistory;

  procedure AddBoxSquareToGame(SquareNo__:Integer; IsAFenceBox__:Boolean);
  begin
    Inc(Game.BoxCount); Game.BoxPos[Game.BoxCount]:=SquareNo__;
    if IsAFenceBox__ then begin
       Game.Board   [SquareNo__]:=Game.Board   [SquareNo__] or FLAG_SQUARE_SET; {mark the box-square as being a member of the fenced-in area}
       OriginalBoard[SquareNo__]:=OriginalBoard[SquareNo__] or FLAG_SQUARE_SET; {mark the box-square as being a member of the fenced-in area}
       end
  end; {AddBoxSquareToGame}

  procedure AddBlockedNeighborBoxesToGame(SquareNo__:Integer);
  var Neighbor,NeighborLeft,NeighborRight:Integer; Direction:TDirection;
  begin
    for Direction:=Low(Direction) to High(Direction) do begin
        Neighbor     :=SquareNo__+Game.SquareOffsetForward[Direction];
        NeighborLeft :=Neighbor  +Game.SquareOffsetLeft   [Direction];          {note it's actually neighbor's left  neighbor}
        NeighborRight:=Neighbor  +Game.SquareOffsetRight  [Direction];          {note it's actually neighbor's right neighbor}
        if ((OriginalBoard[Neighbor] and BOX)<>0)                               {'True': a box on this square (in the original position)}
           and
           ((Game.Board[Neighbor] and FLAG_SQUARE_SET)=0)                       {'True': this box isn't a member of the set yet}
           and {is the box blocked along the other axis?}
           (((( OriginalBoard[NeighborLeft ] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))<>0)
             and
             (((OriginalBoard[NeighborLeft ] and BOX)=0)                        {not a box}
              or
              ((Game.Board   [NeighborLeft ] and FLAG_SQUARE_SET)<>0)           {or the box is already a member of the set}
             )
            )
            or {caution: assumes 4 directions only}
            ((( OriginalBoard[NeighborRight] and (WALL+BOX+FLAG_ILLEGAL_BOX_SQUARE))<>0)
             and
             (((OriginalBoard[NeighborRight] and BOX)=0)                        {not a box}
              or
              ((Game.Board   [NeighborRight] and FLAG_SQUARE_SET)<>0)           {or the box is already a member of the set}
             )
            )
           ) then
           AddBoxSquareToGame(Neighbor,True);
        end;
  end; {AddBlockedNeighborBoxesToGame}

  procedure FindNearestNonBoxFloorSquareNeighbors(SquareNo__:Integer);
  var Direction:TDirection;
  begin
    if (not Visited[SquareNo__]) and
       ((Game.Board[SquareNo__] and WALL)=0) then begin
       Visited[SquareNo__]:=True;
       if (Game.Board[SquareNo__] and BOX)<>0 then                              {'<>': visit neighbors to box-squares recursively}
          for Direction:=Low(Direction) to High(Direction) do
              FindNearestNonBoxFloorSquareNeighbors(SquareNo__+Game.SquareOffsetForward[Direction]);
       end;
  end; {FindNearestNonBoxFloorSquareNeighbors}

  function IsASquareAdjacentToTheFencedInArea(SquareNo__:Integer):Boolean;
  var Neighbor:Integer; Direction:TDirection;
  begin {returns 'True' if the square is adjacent to the fenced-in area}
    Result:=False;
    for Direction:=Low(Direction) to High(Direction) do begin
        Neighbor:=SquareNo__+Game.SquareOffsetForward[Direction];
        if ((Game.Board   [Neighbor] and FLAG_SQUARE_SET)<>0) and
           ((OriginalBoard[Neighbor] and BOX            ) =0) then begin        {don't consider box-squares, only the fenced-in floor-squares}
           Result:=True; break;
           end;
        end;
  end; {IsASquareAdjacentToTheFencedInArea}

  procedure SaveBoard(var BoardState__:TBoardState);
  var i:Integer;
  begin {note: 'HashValue' and 'SimpleLowerBound' aren't saved; they're always recalculated when the board is restored}
    BoardState__.PlayerPos:=Game.PlayerPos;
    BoardState__.Boxes.Count:=Game.BoxCount;
    for i:=1 to Game.BoxCount do BoardState__.Boxes.Squares[i]:=Game.BoxPos[i];
  end; {SaveBoard}

  procedure RestoreBoard(const BoardState__:TBoardState);
  var i:Integer;
  begin
    ClearBoard; {remove current boxes from the board}
    Game.PlayerPos:=BoardState__.PlayerPos; {restore game state}
    Game.BoxCount :=BoardState__.Boxes.Count;
    for i:=1 to Game.BoxCount do Game.BoxPos[i]:=BoardState__.Boxes.Squares[i];
    InitializeBoard(1); {update board and deadlock-set capacities}
  end; {RestoreBoard}

  function Search:Boolean; {returns 'True' if it's a legal position, i.e., if it isn't a deadlock position}

    procedure RemoveBoxFromTheBoard(BoxNo__:Integer);
    var i,SquareNo:Integer;
    begin
      SquareNo:=Game.BoxPos[BoxNo__];
      Dec(Game.Board[SquareNo],BOX);
      Game.BoxPos[BoxNo__]:=0;
      Game.HashValue:=Game.HashValue xor Game.SquareHashValues[SquareNo];
      Dec(Game.SimpleLowerBound,Game.DistanceToNearestTarget[SquareNo]);
      with Game.DeadlockSets do
        for i:=1 to SquareSetCount[SquareNo] do Inc(Capacity[SquareSetNumbers[SquareNo,i]]);     {leaving these deadlock-sets}
    end; {RemoveBoxFromTheBoard}

    function RemoveFrozenGoalSquareBoxesFromDeadlockSet:Integer;
    var BoxNo,SquareNo:Integer;

      procedure AddNeighborBoxSquaresToSquareSet(SquareNo__:Integer);
      var Neighbor:Integer; Direction:TDirection;
      begin
        Game.Board[SquareNo__]:=Game.Board[SquareNo__] or FLAG_SQUARE_SET;
        for Direction:=Low(Direction) to High(Direction) do begin
            Neighbor:=SquareNo__+Game.SquareOffsetForward[Direction];
            if (Game.Board[Neighbor] and (BOX+FLAG_SQUARE_SET))=BOX then
               AddNeighborBoxSquaresToSquareSet(Neighbor);
            end;
      end;

    begin {RemoveFrozenGoalSquareBoxesFromDeadlockSet}
      Result:=0;
      with Game do begin
        for BoxNo:=1 to FenceBoxCount do
            if BoxPos[BoxNo]<>0 then
               AddNeighborBoxSquaresToSquareSet(BoxPos[BoxNo]);                 {treat all neigbouring boxes as being part of the fence}

        for BoxNo:=1 to BoxCount do begin                                       {temporarily remove all non-goal boxes}
            SquareNo:=BoxPos[BoxNo];
            if (SquareNo<>0) and ((Board[SquareNo] and GOAL)=0) then begin
               Dec(Board[SquareNo],BOX); BoxPos[BoxNo]:=-SquareNo;
               end;
            end;

        for BoxNo:=Succ(FenceBoxCount) to BoxCount do begin                     {find frozen goal-square boxes outside the fence;}
            SquareNo:=BoxPos[BoxNo];                                            {since they cannot move, they don't}
            if (SquareNo>0) and                                                 {contribute to the information about the deadlock}
               ((Board[SquareNo] and FLAG_SQUARE_SET)=0) then begin
               Dec(Board[SquareNo],BOX+GOAL);
               if IsAFreezingMove(0,SquareNo,0,False,False) then begin
                  Inc(Board[SquareNo],WALL); Inc(Result);                       {temporarily mark the box-square as a wall}
                  end;
               Inc(Board[SquareNo],BOX+GOAL);
               end;
            end;

        for BoxNo:=1 to BoxCount do begin                                       {put non-goal boxes back on the board}
            SquareNo:=BoxPos[BoxNo];
            if SquareNo<0 then begin
               Inc(Board[-SquareNo],BOX); BoxPos[BoxNo]:=-SquareNo;
               end;
            end;

        if Result<>0 then
           for BoxNo:=Succ(FenceBoxCount) to BoxCount do begin                  {finally, remove frozen goal-square boxes}
               SquareNo:=BoxPos[BoxNo];
               if (Board[SquareNo] and (WALL+BOX))=WALL+BOX then begin
                  Dec(Board[SquareNo],WALL);
                  RemoveBoxFromTheBoard(BoxNo);
                  end;
               end;
        end;
    end; {RemoveFrozenGoalSquareBoxesFromDeadlockSet}

    function Search(Depth__,EscapedBoxesCountDown__:Integer):Boolean;
    {returns 'True' if it's a legal position, i.e., if it isn't a deadlock position}
    var i,BoxNo,SquareNo,SuccessorDepth,PlayerFromSquareNo,BoxToSquareNo:Integer;
        Direction:TDirection;

      procedure PutBoxOnTheBoard(BoxNo__,SquareNo__:Integer);
      var i:Integer;
      begin {precondition: 'BoxNo__' is already a member of the set of boxes, i.e., 'BoxNo__' <= Game.BoxCount}
        Inc(Game.Board[SquareNo__],BOX);
        Game.BoxPos[BoxNo__]:=SquareNo__;
        Game.HashValue:=Game.HashValue xor Game.SquareHashValues[SquareNo__];
        Inc(Game.SimpleLowerBound,Game.DistanceToNearestTarget[SquareNo__]);
        with Game.DeadlockSets do
          for i:=1 to SquareSetCount[SquareNo__] do Dec(Capacity[SquareSetNumbers[SquareNo__,i]]); {entering these deadlock-sets}
      end; {PutBoxOnTheBoard}

      function TimeCheck:Boolean;
      begin {returns 'True' if time expired}
        //Result:=False; Game.SearchStatistics.TimeExpired:=False; exit;
        if Game.SearchStatistics.MoveCountDownToNextTimeCheck<=0 then
           Game.SearchStatistics.MoveCountDownToNextTimeCheck:=MOVE_COUNT_BETWEEN_TIME_CHECKS;
        Game.SearchStatistics.TimeExpired:=
          CalculateElapsedTimeMS(Game.DeadlockSets.StartTimeMS,GetTimeMS)>=Game.SearchStatistics.MaxTimeMS;
        Result:=Game.SearchStatistics.TimeExpired;
      end; {TimeCheck}

    begin {Search}
      {precondition: player's reachable squares have been calculated;}
      {precondition: fence boxes precede outer boxes}
      if (Depth__>=MAX_SEARCH_DEPTH)                                            {'>=': depth limit exceeded: assume the position is legal, i.e., not a deadlock}
         or
         (Game.SimpleLowerBound=0)                                              {'0' : all boxes are located at goals}
         or
         (EscapedBoxesCountDown__=0)                                            {'0' : assume it's not a deadlock because a certain number of boxes could escape from the fenced-in area}
         or
         (Game.SearchStatistics.PushCount>Game.SearchStatistics.MaxPushCount)   {'>' : push limit exceeded}
         or
         ((Game.SearchStatistics.MoveCountDownToNextTimeCheck<=0)               {'<=': time to do a new time check}
          and
          TimeCheck                                                             {'True': time expired}
         )
         or
         (Game.JobNo<>ThisJobNo) then                                           {'<>': another thread cancelled this job}
         Result:=True                                                           {the position is legal, or more correctly, it hasn't been proved to be a deadlock}
      else begin                                                                {generate moves in this position}
         Result:=False; SuccessorDepth:=Succ(Depth__);
         BoxNo:=Game.BoxCount;
         {outer boxes are tried first so the search can remove them as soon}
         {as possible in case they can move around outside the fenced-in area}
         while (BoxNo<>0) and (not Result) do begin
           SquareNo:=Game.BoxPos[BoxNo];
           if SquareNo<>0 then                                                  {'True': the box hasn't been removed from the board}
              for Direction:=Low(Direction) to High(Direction) do
                  if (not Result) and (SquareNo<>0) then with Game.SearchStates[Depth__].PlayersReachableSquares do begin
                     BoxToSquareNo     :=SquareNo+Game.SquareOffsetForward[Direction];
                     if (Game.Board[BoxToSquareNo] and (BOX+WALL+FLAG_ILLEGAL_BOX_SQUARE))=0 then begin {'True': the box destination square is empty and legal}
                        if not Game.ReverseMode then begin
                           PlayerFromSquareNo:=SquareNo-Game.SquareOffsetForward[Direction];
                           Result:=(Squares[PlayerFromSquareNo]=TimeStamp) and  {inline simple check for legal moves}
                                   IsALegalPush(BoxNo,Direction);               {extended checks for legal moves}
                           end
                        else begin
                           PlayerFromSquareNo:=BoxToSquareNo;
                           Result:=(Squares[PlayerFromSquareNo]=TimeStamp) and
                                   ((Game.Board[PlayerFromSquareNo+Game.SquareOffsetForward[Direction]] and (WALL+BOX))=0);
                           end;

                        if Result then begin                                    {'True': legal push/pull}
                           Result:=False;
                           Inc(Game.SearchStatistics.PushCount);
                           Dec(Game.SearchStatistics.MoveCountDownToNextTimeCheck);

                           if (Game.Board[BoxToSquareNo] and FLAG_SQUARE_SET)<>0 then
                              {the box moves inside, or on the border of, the fenced-in area}
                              if   not Game.ReverseMode then
                                   DoPush(BoxNo,Direction)                      {do the push, i.e., update the board}
                              else // DoPull(BoxNo,Direction)                   {do the pull, i.e., update the board}
                                   UndoPush(BoxNo,OPPOSITE_DIRECTION[Direction]) {inlined 'DoPull'}
                           else begin                                           {the box moves outside the fenced-in area}
                              RemoveBoxFromTheBoard(BoxNo);                     {continue the search without this box}
                              if   BoxNo<=FenceBoxCount then                    {'True': the box is part of the fence}
                                   Dec(EscapedBoxesCountDown__)                 {update number of escaped boxes}
                              else SquareNo:=0;                                 {continue to next box}
                              end;

                           if SquareNo<>0 then begin                            {'True': search this position recursively}
                              CalculatePlayersReachableSquares(SuccessorDepth,0);
                              {note that the results from 'CalculatePlayersReachableSquares' are passed on to the recursive call}
                              if not IsADuplicatePosition(SuccessorDepth,Game.SearchStates[SuccessorDepth].PlayersReachableSquares.MinPlayerPos,History) then begin
                                 {update game history}
                                 for i:=1 to Game.BoxCount do History[SuccessorDepth].Boxes.Squares[i]:=Game.BoxPos[i];
                                 History[SuccessorDepth].HashValue:=Game.HashValue;
                                 History[SuccessorDepth].PlayerPos:=Game.SearchStates[SuccessorDepth].PlayersReachableSquares.MinPlayerPos;
                                 Result:=Search(SuccessorDepth,EscapedBoxesCountDown__); {recursive step: explore the new position}
                                 end;
                              end;

                           if   Game.BoxPos[BoxNo]<>0 then                      {'True': the box is still on the board}
                                if   not Game.ReverseMode then
                                     UndoPush(BoxNo,Direction)                  {take back the push, i.e., update the board}
                                else // UndoPull(BoxNo,Direction)               {take back the pull, i.e., update the board}
                                     DoPush  (BoxNo,OPPOSITE_DIRECTION[Direction])  {inlined 'UndoPull'}
                           else if BoxNo<=FenceBoxCount then begin              {fence boxes are kept on the board for further analysis}
                                   MovePlayer(PlayerFromSquareNo);              {ensure that the player is back in the correct access area}
                                   PutBoxOnTheBoard(BoxNo,SquareNo);            {put the box back on the board before the search continues}
                                   Inc(EscapedBoxesCountDown__);                {restore number of escaped boxes}
                                   end
                                else begin
                                   {outer boxes are not put back on the board;}
                                   {they are removed entirely from the search}
                                   {as soon as the player can push them around}
                                   {outside the fenced-in area}
                                   if Game.SimpleLowerBound=0 then              {'True' the remaining boxes are all on goals}
                                      Result:=True;
                                   MovePlayer(PlayerFromSquareNo);              {ensure that the player is back in the correct access area}

                                   if   SquareNo=0 then
                                        {the outer box was removed from the board}
                                        {without calling the search recursively, hence,}
                                        {there is no need to recalculate the player's}
                                        {reachable squares from scratch; it suffices to}
                                        {add any new reachable squares now that the box}
                                        {has gone; to that end, call the calculation with}
                                        {the box position as parameter to signal a continued}
                                        {calculation, as opposed to a calculation from}
                                        {scratch; the box position is, however, not}
                                        {available at this time, so the neighbor}
                                        {position is used instead; this works because}
                                        {the box position has a different timestamp, so}
                                        {flood-filling from the neighbor square will}
                                        {discover and visit the box position and any}
                                        {previously unreachable squares connected to it}
                                        CalculatePlayersReachableSquares(Depth__,PlayerFromSquareNo) {recalculate the player access area, now the box has been removed}
                                   else {at the current search depth, the outer box moved}
                                        {to a square inside the fenced-in area; the box}
                                        {was removed from the board at a deeper search}
                                        {after calling the search recursively; the player's}
                                        {reachable squares must be recalculated from scratch}
                                        CalculatePlayersReachableSquares(Depth__,0);                 {recalculate the player access area from scratch, now the box has been removed}

                                   SquareNo:=0;                                 {continue to next box}
                                   BoxNo:=Succ(Game.BoxCount);                  {retry all outer boxes to see if more of them can be removed now}
                                   end;
                           end; {a legal move}
                        end;
                     end
                  else break; {leave direction loop}
           Dec(BoxNo); {next box}
           end; {each box}
         end; {generate moves in this position}
    end; {Search.Search}

  begin {Search}
    Inc(Game.SearchStatistics.FenceCount);
    InitializeBoard(1); DeadlockedBoxCount__:=0;                                {update deadlock-set capacities, etc.}
    MovePlayer(OriginalBoardState.PlayerPos);                                   {put the player on the board at the original position}
    CalculatePlayersReachableSquares(0,0);                                      {precondition for 'Search'}
    Result:=Search(0,FenceBoxCount);                                            {'FenceBoxCount': all boxes must either reach a goal square or escape the fenced-in area}
    if not Result then
       RemoveFrozenGoalSquareBoxesFromDeadlockSet;
  end; {Search}

begin {IsALegalPosition}
  Result:=True;
  Game.DeadlockSets.StartTimeMS:=GetTimeMS;
  if ReverseMode__<>Game.ReverseMode then begin
     Game.ReverseMode:=ReverseMode__;
     if   ReverseMode__ then
          Game.DistanceToNearestTarget:=Game.DistanceToNearestBoxStartPosition
     else Game.DistanceToNearestTarget:=Game.DistanceToNearestGoal;
     end;
  Game.DeadlockSets.ControllerAndFreezeSetPairsEnabled:=False;                  {'False: 'IsALegalPush' ignores controller/freeze-set deadlock pairs}
  DeadlockedBoxCount__:=0; ThisJobNo:=Game.JobNo;
  FillChar(Game.SearchStatistics,SizeOf(Game.SearchStatistics),0);
  Game.SearchStatistics.MaxPushCount:=MaxPushCount__;                           {store the limits globally, together with the rest of the control information}
  Game.SearchStatistics.MaxTimeMS   :=MaxTimeMS__;                              {store the limits globally, together with the rest of the control information}
  Game.SearchStatistics.MoveCountDownToNextTimeCheck:=MOVE_COUNT_BETWEEN_TIME_CHECKS;

  OriginalBoard:=Dead_.Game.Board; SaveBoard(OriginalBoardState);
  try
    FillChar(Visited,SizeOf(Visited),0);
    if BoxNo__<>0 then begin                                                    {'True': investigate only fences including this box number}
       FindNearestNonBoxFloorSquareNeighbors(Game.BoxPos[BoxNo__]);             {find all non-box floor squares near the box}
       for SquareNo:=0 to Game.BoardSize do
           Visited[SquareNo]:=not Visited[SquareNo];                            {before: 'True' meant a neighbor floor square; after: 'False' means a not yet investigated floor square}
       end;

    if   not Game.ReverseMode then
         SquareNo:=0                                                            {normal forward gameplay: find each fenced-in area}
    else SquareNo:=OriginalBoardState.PlayerPos;                                {reverse mode gameplay: check player's current access area only}

    while SquareNo<=Game.BoardSize do begin
        if Result then                                                          {as long as the position hasn't been proved to be a deadlock}
           if (not Visited[SquareNo]) and
              ((Game.Board[SquareNo] and (WALL+BOX))=0) and                     {for each non-visited free floor-square}
              (not Game.SearchStatistics.TimeExpired) then begin                {time not expired yet}
              MovePlayer(SquareNo); CalculatePlayersReachableSquares(0,0);      {find inner floor-squares in the fenced-in area}
              for i:=0 to Game.BoardSize do with Game.SearchStates[0].PlayersReachableSquares do begin
                  if   Squares[i]=TimeStamp then begin
                       Visited[i]:=True;                                        {mark squares in this access area as visited}
                       Game.Board[i]:=Game.Board[i] or FLAG_SQUARE_SET;         {mark the square as being inside the fenced-in area}
                       end
                  else Game.Board[i]:=Game.Board[i]    and (not FLAG_SQUARE_SET); {mark the square as being outside the fenced-in area}
                  OriginalBoard[i]  :=OriginalBoard[i] and (not FLAG_SQUARE_SET); {reset any flags in the original board}
                  end;

              with Game.SearchStates[0].PlayersReachableSquares do
                if ((not Game.ReverseMode)
                    and
                    (Squares[OriginalBoardState.PlayerPos]<>TimeStamp)          {'True': it's not the player's original access area}
                   )
                   or
                   (Game.ReverseMode
                    and
                    (Squares[OriginalBoardState.PlayerPos]=TimeStamp)           {'True': it's the player's original access area}
                   ) then begin
                                                                                {test this fenced-in area}
                   ClearBoard;

                   {find boxes around the fenced-in area and add them to the set}
                   for i:=1 to OriginalBoardState.Boxes.Count do with OriginalBoardState.Boxes do begin
                       j:=Squares[i];
                       if IsASquareAdjacentToTheFencedInArea(j) then
                          AddBoxSquareToGame(j,True);
                       end;

                   {additionally, treat boxes adjacent to the inner-fence boxes}
                   {as being part of the fence, provided they seem to be blocked}
                   for i:=1 to Game.BoxCount do
                       AddBlockedNeighborBoxesToGame(Game.BoxPos[i]);

                   FenceBoxCount:=Game.BoxCount;

                   if not Game.ReverseMode then
                      {add the rest of the boxes to the set as 'outer boxes',}
                      {i.e., they are subject to removal from the search if they}
                      {can move at all}
                      for i:=1 to OriginalBoardState.Boxes.Count do with OriginalBoardState.Boxes do begin
                          j:=Squares[i];
                          if (OriginalBoard[j] and FLAG_SQUARE_SET)=0 then
                             AddBoxSquareToGame(j,False);
                          end;

                   if (FenceBoxCount>0) and
                      (FenceBoxCount<=MAX_SEARCH_DEPTH) then begin              {test small areas only; otherwise, the search time may blow up}
                      Result:=Search;                                           {'Search' returns 'True' if the position is legal, i.e., not a deadlock}
                      if not Result then with Game do begin                     {'not Result': the position is a deadlock}

//                       if BoxNo__<>0  then begin                              {disregard the deadlock if the original box isn't a member of the fence}
//                          Result:=True;
//                          j:=OriginalBoardState.Boxes.Squares[BoxNo__];       {'j' : square-number for the original box}
//                          for i:=1 to BoxCount do
//                              Result:=Result and(BoxPos[i]<>j);               {'<>': not the original box}
//                          end;
//                       if not Result then                                     {'True': the original box is a member of fence}

                            for i:=1 to BoxCount do
                                if BoxPos[i]<>0 then begin
                                   Inc(DeadlockedBoxCount__);
                                   SquareToColRow(BoxPos[i],Col,Row);
                                   DeadlockedBoxSquares__[DeadlockedBoxCount__].x:=Col;
                                   DeadlockedBoxSquares__[DeadlockedBoxCount__].y:=Row;
                                   end;
                         end;
                      end
                   else Game.BoxCount:=0;                                       {'InitializeBoard()' hasn't been activated, hence, make sure that 'ClearBoard' doesn't update anything}
                   RestoreBoard(OriginalBoardState);
                   end;
              end
           else {a visited square, a wall-square, or a box-square}
        else break; {the position is a deadlock: stop the search}

        if   not Game.ReverseMode then
             Inc(SquareNo)                                                      {advance to the next square}
        else break; {SquareNo:=MAX_BOARD_SIZE+1;}                               {only investigate the player's currenct access area}
        end;
  except
    on E:Exception do begin
       {if something goes wrong, then counters for the precalculated deadlock-sets}
       {are unreliable, hence, clear them and continue without precalculated sets}
       Dead_.Game.Board:=OriginalBoard;
       FillChar(Dead_.Game.DeadlockSets.SquareSetCount,SizeOf(Dead_.Game.DeadlockSets.SquareSetCount),0);
       end;
  end;
  Game.DeadlockSets.ControllerAndFreezeSetPairsEnabled:=True; {'True': 'IsALegalPush' takes controller/freeze-set deadlock pairs into account}
  Game.SearchStatistics.TimeMS:=CalculateElapsedTimeMS(Game.DeadlockSets.StartTimeMS,GetTimeMS);
end; {IsALegalPosition}
(*
procedure TestDeadlockSets;
const
  LEVEL_FILE_NAMES: array[0..7] of String = (
    '',
//  'C:\Program Files\BDSokobanYASC\Levels\Test.sok',
    'YASGen.sok',
    'Yoshio.sok',
    'du Peloux.sok',
    'Haywood.sok',
    'Holland.sok',
    'GRIGoRusha.sok',
    'Skinner.sok'
    );

var
  i,FileIndex,LevelCount,GameCount,ErrorCount,MoveCount:Integer;
  b:Boolean;
  Path,s:String;
  Level:TLevel;
  LogFile:TextFile;

  function  TestLevel:Boolean;
  var Snapshot:TSnapshot;

    function TestGame(Snapshot__:TSnapshot):Boolean;
    begin
      Result:=True;
      if Assigned(Snapshot__) then with MainForm.Game do with History do begin
         if LoadSnapshot(Snapshot__) then begin
            Inc(GameCount);

            Writeln(LogFile,Format('%6d %6d %s    %s',[LevelCount,GameCount,VisualFileName(FileName),Snapshot__.Name]));

            Reset(True);
            while (Count<Top) and Result do begin
              if   Redo0(False) then Inc(MoveCount)
              else Result:=False;
              end;
            if not Result then begin

               end;
            end
         else Result:=Error('Load snapshot failed: '+Snapshot.Name,'');
         end;
    end; {TestGame}

  begin {TestLevel}
    Result:=True;
    with MainForm.Game do begin
      TestGame(BestSolutionMoves);
      TestGame(BestSolutionPushes);
      TestGame(SaveGame);
      TestGame(BuiltInBestSolutionMoves);
      TestGame(BuiltInBestSolutionPushes);
      Snapshot:=TSnapshot(Snapshots.First);
      while Assigned(Snapshot) do begin
        TestGame(Snapshot);
        Snapshot:=TSnapshot(Snapshot.Next);
        end;
      end;
  end; {TestLevel}

begin {TestDeadlockSets}
  LevelCount:=0; GameCount:=0; ErrorCount:=0; MoveCount:=0;
  Path:=StrWithTrailingPathDelimiter(StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath)+DEFAULT_LEVEL_DIRECTORY);
  Assign(LogFile,Path+'Log.txt'); Rewrite(LogFile);
  MainForm.Deadlocks.Suspend;
  try
//  for FileIndex:=0 to 100 do with MainForm do begin
//      if FileIndex<>0 then s:=Format('Levels (%d).sok',[FileIndex])
//      else s:='Levels.sok';
//      s:='Levels\'+s;
//      if FileExists(s) then
//      if SokoFile.LoadFromFile(s) then begin

    for FileIndex:=1 to High(LEVEL_FILE_NAMES) do with MainForm do begin
        if SokoFile.LoadFromFile(Path+LEVEL_FILE_NAMES[FileIndex]) then begin
           Level:=TLevel(SokoFile.Levels.GetItemByIndex(0));
           while Level<>nil do begin
             s:=MakeIniFileSectionFileName(SokoFile.Name,Level.Name);
             Status.Hint:=IntToStr(Succ(LevelCount))+COLON+SPACE+StrWithoutBrackets(ExtractFileName(s));
             Application.ProcessMessages;

             if Level.SnapshotsAsText.IsEmpty then
                Level:=TLevel(Level.Next)
             else
                if Game.LoadFromFileOrClipboard(s,nil,nil,b) then begin
                   Level:=SokoFile.GetLevelByName(Game.Name);
                   if Level<>nil then begin
                      Inc(LevelCount);
                      Deadlocks.LoadGame(False,True);

                      if   TestLevel then begin
                           Level:=TLevel(Level.Next);
                           //Level:=nil;
                           end
                      else begin
                        Msg('Deadlocked position in game: '+VisualFileName(Game.FileName),'Deadlock Tests',MB_OK+MB_ICONERROR);
                        Level:=nil;
                        end;
                      end
                   else Msg('Internal error',Game.Name,MB_OK);
                   Game.Clear;
                   end
                else begin
                   Error('Level not found: '+s,'');
                   Level:=nil;
                   end;
             end;
           end
        else Error('File not found: '+LEVEL_FILE_NAMES[FileIndex],'');
        end;

    for i:=1 to 70 do Write(LogFile,'-'); Writeln(LogFile);
    Writeln(LogFile,'Levels: '+IntToStr(LevelCount)+' Games: '+IntToStr(GameCount)+' Illegal pushes: '+IntToStr(ErrorCount)+' Moves: '+IntToStr(MoveCount));

    Msg('Levels: '+IntToStr(LevelCount)+NL+'Games: '+IntToStr(GameCount)+NL+' Illegal pushes: '+IntToStr(ErrorCount)+NL+'Moves: '+IntToStr(MoveCount),'Deadlock Tests - Results',MB_OK);
  finally
    MainForm.Deadlocks.Resume; {if the thread really is working, then it has garbage data now}
    CloseFile(LogFile);
  end;
end; {TestDeadlockSets}
*)
{-----------------------------------------------------------------------------}

{Deadlocks thread}

constructor TDeadlocksThread.Create;
begin
  fCriticalSection:=nil;
  try       fCriticalSection:=TCriticalSection.Create;
  except    fCriticalSection.Free; fCriticalSection:=nil; raise;
  end;
  inherited Create(fCriticalSection,tpLower);
//inherited Create(fCriticalSection,tpNormal);
end;

destructor TDeadlocksThread.Destroy;
begin
  fCriticalSection.Free;
  Inherited;
end;

procedure TDeadlocksThread.Execute;
begin
  while not Terminated do begin
    case TDeadlocksThreadStateType(State) of
      dlIdle          : Suspended:=True;
      dlLoadGame      : begin State:=Ord(dlIsCalculating);
                              InitializeGame(BoardWidth,BoardHeight,Board,Title);
                        end;
      dlIsCalculating : if CalculateDeadlockSets then begin
                           if State=Ord(dlIsCalculating) then
                              CalculateReachableTargetsForAllSquares_C(Game.CalculateDeadlockSetsJobNo,Dead_.Game.ReverseMode,True);
//                         if Dead_.Game.LogSquareGoalDistances and
//                            (State = Ord( dlIsCalculating ) ) and
//                            ( Game.CalculateDeadlockSetsJobNo = Game.JobNo ) then
//                            ShowSquareGoalDistances; {show square->goal distances before client thread(s) get access to, and control over, the internal deadlock detection board}

                           Enter;
                           try     if (State=Ord(dlIsCalculating)) or
                                      (State=Ord(dlStopCalculation)) then begin
                                      Game.DeadlockSets.Calculated:=Game.CalculateDeadlockSetsJobNo=Game.JobNo; {'True': the calculated deadlocks apply to the currently loaded game}
                                      if Game.DeadlockSets.Calculated and
                                         MainForm.ShowSimpleDeadSquaresEnabled then
                                         PostMessage(MainForm.Handle,Misc_.MSG_UPDATE_DEAD_SQUARES,0,0);
                                      State:=Ord(dlIdle); {nothing more to do right now}
                                      end;
                           finally Leave;
                           end;
                           Synchronize(MainForm.Status.EnableDisableButtons);
                           end
                        else begin
                           Hint:='';
                           Enter;
                           try     if (State=Ord(dlIsCalculating)) or
                                      (State=Ord(dlStopCalculation)) then begin
                                      if Game.CalculateDeadlockSetsJobNo=Game.JobNo then {'True': the calculated deadlocks apply to the currently loaded game}
                                         Hint:=DeadlockedPositionText;
                                      State:=Ord(dlIdle); {nothing more to do right now}
                                      end;
                           finally Leave;
                           end;
                           Synchronize(MainForm.Status.EnableDisableButtons);
                           if Hint<>'' then begin
                              Hint:=VisualFileName(Game.Title)+COLON+SPACE+Hint;
                              Synchronize(SynchronizedHint);
                              end;
                           end;
      dlStopCalculation
                      : State:=Ord(dlIdle);
      dlTerminate     : if not Terminated then Terminate;
      dlTerminated    :;
      else              Suspended:=True;
    end; // case
    end;
  State:=Ord(dlTerminated);
end;

procedure TDeadlocksThread.SynchronizedHint;
begin
  MainForm.Status.Hint:=Hint;
end;

{-----------------------------------------------------------------------------}

{Deadlocks}

constructor TDeadlocks.Create;
begin
  FillChar(Dead_.Game,SizeOf(Dead_.Game),0);
  Dead_.Game.DeadlockSets.AdjacentOpenSquaresLimit:=DEFAULT_DEADLOCK_SETS_ADJACENT_OPEN_SQUARES_LIMIT;
  Dead_.Game.DeadlockSets.BoxLimit:=DEFAULT_DEADLOCK_SETS_BOX_LIMIT;
  Dead_.Game.LogFileEnabled:=False;
  Dead_.Game.XSBNotation:=True;

  DeadlocksThread:=nil;
  fCalculatePushesLowerBoundTimeLimitMS := DEFAULT_CALCULATE_MINIMUM_LOWER_BOUND_TIME_LIMIT_MS;
  fDeadlockDetectionMaxPushCount:=DEFAULT_DEADLOCK_DETECTION_MAX_PUSH_COUNT;
  fDeadlockDetectionTimeLimitMS :=DEFAULT_DEADLOCK_DETECTION_TIME_LIMIT_MS;
  try    DeadlocksThread:=TDeadlocksThread.Create;
  except DeadlocksThread.Free; DeadlocksThread:=nil; raise;
  end;
  Clear;

  //Msg(IntToStr(SizeOf(Dead_.Game)),'',MB_OK);
end;

destructor  TDeadlocks.Destroy;
var StartTimeMS,TimeOutMS:TTimeMS; FileName:String;
begin
  Clear;
  CloseLogFile;
  if DeadlocksThread<>nil then with DeadlocksThread do begin
     Suspend; State:=Ord(dlTerminate); Resume;
     SleepEx(50,False); if not Terminated then Terminate;

     repeat StartTimeMS:=GetTickCount;
            TimeOutMS:=StartTimeMS+WAIT_FOR_THREAD_TO_TERMINATE_MS;
     until  TimeOutMS>=StartTimeMS; // simple clock wrap-around control
     repeat SleepEx(0,False);
     until  (State=Ord(dlTerminated)) or (GetTickCount>=TimeOutMS) or (GetTickCount<StartTimeMS);
     if State<>Ord(dlTerminated) then Suspend;

     DeadlocksThread.Free; DeadlocksThread:=nil;
     end;

  CloseLogFile;
  FileName:=MainForm.ApplicationDataPath+ExtractFileName(FileNameWithExtension(Application.ExeName,LOG_FILE_EXT));
  if FileExists(FileName) then
     SysUtils.DeleteFile(FileName);
end;

procedure TDeadlocks.Clear;

  procedure Clear;
  begin
    Dead_.Game.BoardIsLoaded                 :=False;
    Dead_.Game.DeadlockSets.Calculated       :=False;
    Dead_.Game.BoxGoalMatching.Calculated    :=False;
    Dead_.Game.BoardTimeStamps.TimeStamp     :=High(Dead_.Game.BoardTimeStamps.TimeStamp);
    Dead_.Game.GoalBoxMatching.Calculated    :=False;
    Dead_.Game.TubeFillingSquareCount        :=0;
    OverflowingDeadlockSet                   :=0;
    DeadlockedBoxCount                       :=0;
    DeadlockStats                            :='';
  end;

begin {Clear}
  if   DeadlocksThread<>nil then with DeadlocksThread do begin
       Enter;
       try     State:=Ord(dlIdle);
               Clear;
       finally Leave;
       end;
       end
  else Clear;
end;

function  TDeadlocks.GetCalculated:Boolean;
begin
  Result:=Dead_.Game.DeadlockSets.Calculated;
end;

function  TDeadlocks.GetCount:Integer;
begin
  if   Calculated or (ThreadState=dlIsCalculating) or (ThreadState=dlStopCalculation) then
       Result:=Dead_.Game.DeadlockSets.Count
  else Result:=0;
end;

function  TDeadlocks.GetLoggedCount:Integer;
begin
  if   Calculated or (ThreadState=dlIsCalculating) or (ThreadState=dlStopCalculation) then
       Result:=Dead_.Game.DeadlockSets.LoggedCount
  else Result:=0;
end;

function  TDeadlocks.GetOverflowingDeadlockSet:Integer;
begin
  if   Dead_.Game.DeadlockSets.Calculated then with Dead_.Game.DeadlockSets do
       Result:=RunNo[OverflowingDeadlockSet]
  else Result:=0;
end;

function  TDeadlocks.GetThreadState:TDeadlocksThreadStateType;
begin
  if   DeadlocksThread<>nil then
       Result:=TDeadlocksThreadStateType(DeadlocksThread.State)
  else Result:=dlIdle;
end;

function  TDeadlocks.GetTimeMS:TTimeMS;
begin
  if        Calculated then Result:=Dead_.Game.DeadlockSets.TimeMS
  else if   (ThreadState=dlIsCalculating) or (ThreadState=dlStopCalculation) then
            Result:=CalculateElapsedTimeMS(Dead_.Game.DeadlockSets.StartTimeMS,SokUtil_.GetTimeMS)
       else Result:=0;
end;

function  TDeadlocks.IsALegalMove(BoxNo__,Col__,Row__:Integer; CheckSimpleDeadlocks__:Boolean):Boolean;
var i,j,Square,FromSquare,ToSquare,{NextSquare,}SquareSetNo:Integer; {Direction:TDirection;}
    {IsASinglePush:TExtendedBoolean; d,Direction:TDirection;}
    FromGoalSet,ToGoalSet:TGoalSet;
begin
  Result:=True;
  OverflowingDeadlockSet:=0; Dead_.Game.DeadlockSets.OverflowingSetCount:=0;

  if (BoxNo__<>0) and
     Dead_.Game.DeadlockSets.Calculated and
     (MainForm.Game.DeadlockDetection.Enabled)   and
     (Dead_.Game.BoardIsLoaded or LoadBoard) then with Dead_.Game do with DeadlockSets do begin
     FromSquare   :=BoxPos[BoxNo__];
     ToSquare     :=ColRowToSquare(Col__,Row__);

     if FromSquare<>ToSquare then begin {otherwise it's a null-move which is considered ok, even if the position already is a deadlock}

        {simple check for a legal destination square}
        Result:=(Board[ToSquare] and (BOX+WALL+FLAG_ILLEGAL_BOX_SQUARE))=0;

        {simple check for a box getting stuck against walls and/or other boxes}
        if Result and CheckSimpleDeadlocks__ then begin
           SquareToColRow(FromSquare,i,j);
           Result:=not MainForm.Game.IsAFreezingMove(i,j,Col__,Row__,MainForm.Game.PlayerPos.X,MainForm.Game.PlayerPos.Y); {check if the new box position is a simple deadlock}
           end;

        if Result and (not MainForm.Game.ReverseMode) then begin
           {check for deadlock-set overflows, i.e., pattern matching}
           {IsASinglePush:=ebUnknown; Direction:=Low(Direction);}

           for i:=1 to SquareSetCount[ToSquare] do begin  {for each deadlock-set that 'ToSquare' is a member of}
               SquareSetNo:=SquareSetNumbers[ToSquare,i]; {get current set number}

               if Capacity[SquareSetNo]<=0 then begin {can a push cause an overflow?}
                  Result:=False;

                  for j:=1 to SquareSetCount[FromSquare] do
                      if SquareSetNumbers[FromSquare,j]=SquareSetNo then begin
                         {if 'FromSquare' is in the deadlock-set, the push doesn't cause an overflow}
                         Result:=True; break;
                         end;

                  if not Result then
                     if not (dsfPlayerMustBeOutsideSet in Flags[SquareSetNo]) then begin
                        {
                        if i<>1 then begin                                  // self-organizing list: doesn't seem to be worthwhile here;
                           j:=SquareSetNumbers[ToSquare,Pred(i)];           // shift current deadlock-set one slot left towards the front of the list,
                           SquareSetNumbers[ToSquare,Pred(i)]:=SquareSetNo; // i.e., the most popular sets are tested first
                           SquareSetNumbers[ToSquare,i]:=j;
                           end;
                        }
                        OverflowingDeadlockSet:=SquareSetNo;                    {when the function returns False', the overflowing set is stored here but note that 'OverflowingSetCount' = 0; the count is only used for overflowing sets requiring further analysis, i.e., after performing the move}
                        break; {pushing the box to 'ToSquare' would overflow the deadlock-set}
                        end
                     else if (dsfControllerSet in Flags[Pred(SquareSetNo)])     {'True': the deadlock-set is the freeze-set in a controller/freeze-set pair; the squares in the controller-set must contain enough boxes before the squares in the freeze-set freeze}
                             and
                             (( Capacity[Pred(SquareSetNo)]>0)                  {'True': the squares in the controller-set don't contain the required number of boxes, hence, it's a deadlock}
                              or
                              ((Capacity[Pred(SquareSetNo)]=0)
                               and {if the box leaves a square belonging to the controller-set and moves to a square belonging to the freeze-set, then the controller-set must still contain enough boxes}
                               IsSquareAMemberOfDeadlockSet(FromSquare,Pred(SquareSetNo))
                              )
                             )
                             and
                             ((not (dsfTestForFreezingSquare in Flags[SquareSetNo])) {'True': it's known from the precalculation, that when the squares in the freeze set have been filled, then the boxes at these squares have frozen}
                              or
                              IsAFreezingMove(FromSquare,ToSquare,PlayerPos,True,False) {'True': moving a box to the single square in the freeze set causes the box to freeze}
                             )
                             and
                             (dsfFreezeSet in Flags[SquareSetNo]) then begin    {True': a sanity check, this should always be true here}
                             OverflowingDeadlockSet:=SquareSetNo;               {when the function returns False', the overflowing set is stored here but note that 'OverflowingSetCount' = 0; the count is only used for overflowing sets requiring further analysis, i.e., after performing the move}
                             break; {pushing the box to 'ToSquare' would overflow the deadlock-set}
                             end
                          else begin
                            {the set overflows but the player must be outside the set}

                            {mixing combined pushes and single pushes means that the}
                            {program cannot block moves based on the move direction}
                            {of the last pushed box;
                            {the result would be that a push first may be allowed when}
                            {it was checked as a part of a combined push, and when the}
                            {push later was performed for real in the game, it could}
                            {be blocked because of the single-step direction check,}
                            {causing confusion for the user;}
                            {therefore, always treat the position as legal}
                            Result:=True;

                            {save the overflowing set numbers for later analysis}
                            if (OverflowingSetCount<High(OverflowingSets)) and
                               (not (dsfTestForFreezingSquare in Flags[SquareSetNo])) then begin
                               Inc(OverflowingSetCount);
                               OverflowingSets[OverflowingSetCount]:=SquareSetNo;
                               end;

(*
                            // the following code implements the check for the
                            // player being outside the deadlock set when the move
                            // is a single push; (it may have become outdated)

                            if   IsASinglePush=ebUnknown then begin
                                 IsASinglePush:=ebFalse;
                                 for d:=Low(d) to High(d) do
                                     if ToSquare=FromSquare+Game.SquareOffsetForward[d] then begin
                                        IsASinglePush:=ebTrue; Direction:=d; break;
                                        end;
                                 end;

                            if   IsASinglePush=ebTrue then
                                 if DIRECTION_TO_DEADLOCK_SET_FLAG[Direction] in Flags[SquareSetNo] then begin
                                    {when the box is pushed in this direction, then the player is outside the set,}
                                    {hence, this is a deadlock}
                                    OverflowingDeadlockSet:=SquareSetNo;             {when the function returns False', the overflowing set is stored here but note that 'OverflowingSetCount' = 0; the count is only used for overflowing sets requiring further analysis, i.e., after performing the move}
                                    break; {pushing the box to 'ToSquare' would overflow the deadlock-set}
                                    end
                                 else begin
                                    {save the overflowing set numbers for later analysis}
                                    if OverflowingSetCount<High(OverflowingSets) then begin
                                       Inc(OverflowingSetCount);
                                       OverflowingSets[OverflowingSetCount]:=SquareSetNo;
                                       end;
                                    Result:=True;
                                    end
                            else Result:=True;
*)
                            end;
                  end;
               end;
           end;

        if Result and BoxGoalMatching.Calculated then with BoxGoalMatching do begin
           {"b boxes can reach g goals, b > g" test}
           j:=0; {'j' counts boxes with a reachable goal-set which is a subset of the reachable goals from 'ToSquare'}
           for i:=1 to BoxCount do
               if Result then
                  if i<>BoxNo__ then begin                                      {'True': this isn't the 'self' box}
                     Square:=BoxPos[i];
                     if IsASquareGoalEdgeSubSet(Square,ToSquare) then Inc(j);   {'True': box 'i' can only reach goals which also is reachable from 'ToSquare'}
                     if (SubSetCount[i]=EdgeCount[Square]) and                  {'True': n boxes share n goals, so n+1 boxes cannot share the same n goals}
                        IsASquareGoalEdgeSubSet     (ToSquare  ,Square) and
                        (not IsASquareGoalEdgeSubSet(FromSquare,Square)) then
                        Result:=False;                                          {moving the box to 'ToSquare' overflows the available goals for boxes sharing the goal-set with box 'i'}
                     end
                  else Inc(j)                                                   {'Inc': include 'self' in the count}
               else break;
           if  j>EdgeCount[ToSquare] then Result:=False;                        {'True': b boxes share g goals and b > g, i.e., it's an overflow situation}
           end;

        if Result and GoalBoxMatching.Calculated then with GoalBoxMatching do begin
           {"g goals reachable from b boxes, b < g" test; only implemented for "g=1"}
           FromGoalSet:=BoxGoalMatching.Edges[FromSquare];
           ToGoalSet  :=BoxGoalMatching.Edges[ToSquare  ];

           for i:=1 to GoalCount do
               if (EdgeCount[i]=1) and
                  (i      in FromGoalSet) and
                  (not (i in ToGoalSet)) then begin
                  {this goal is about to loose the last box that can reach the}
                  {goal;}
                  {this simple test is the only one for goal-box assignments}
                  {because a full "b boxes for g goals, b < g" test requires an}
                  {O(goals^2) updating step which is considered too expensive}
                  {in the general case}
                  Result:=False; break;
                  end;
           end;

{       // checking for 4-blocks is obsolete, 'Game.IsAFreezingMove' is more general and finds 4-blocks

        if Result and (not MainForm.Game.ReverseMode) then begin
           // check for 4-blocks; this hard-wired test can be substituted by deadlock-sets with a slightly lower performance

           Dec(Board[FromSquare],BOX); // temporarily remove the box from the board

           for Direction:=Low(Direction) to High(Direction) do
               if Result then begin
                  NextSquare:=ToSquare+SquareOffsetForward[Direction];
                  if (Board[NextSquare] and (WALL+BOX))<>0 then begin
                     // check left 4-block; the right 4-block is covered by checking another direction
                     i:=Board[NextSquare+SquareOffsetLeft [Direction]] and (WALL+BOX+GOAL);
                     j:=Board[ToSquare  +SquareOffsetLeft [Direction]] and (WALL+BOX+GOAL);

                     if ((i and (WALL+BOX))<>0)
                        and
                        ((j and (WALL+BOX))<>0)
                        and
                        ( // check if one of the squares in the 4-block is a box on a non-goal square
                          ((Board[ToSquare] and GOAL)=0)
                          or
                          ((i and (BOX+GOAL))=BOX)
                          or
                          ((j and (BOX+GOAL))=BOX)
                          or
                          ((Board[NextSquare] and (BOX+GOAL))=BOX)
                        ) then
                        Result:=False;
                     end;
                  end
               else break;

           Inc(Board[FromSquare],BOX); // put the box back on the board
           end;
}
        end;
     end;
end;

function  TDeadlocks.IsALegalPosition:Boolean;
var BoxNo:Integer; a,b,c,d:TColRow;
begin {returns 'False' if the position is proved to be a fenced-in area deadlock, otherwise 'True'}
  Result:=True;
  OverflowingDeadlockSet:=0; DeadlockedBoxCount:=0; DeadlockStats:='';

  MainForm.Game.UnpackLastMove(a,b,c,d,BoxNo);

  if Calculated and
     (not MainForm.Game.IsReplaying) and
     MainForm.Game.DeadlockDetection.Enabled and
     (MainForm.Game.GameState<>gsSolved) and
     (BoxNo<>0) and
     (Dead_.Game.BoardIsLoaded or LoadBoard) then with Dead_.Game.SearchStatistics do begin

     //FillChar(Dead_.Game.DeadlockSets.SquareSetCount,SizeOf(Dead_.Game.DeadlockSets.SquareSetCount),0); // test: disabling precalculated deadlock sets
     MovePlayer(ColRowToSquare(MainForm.Game.PlayerPos.x,MainForm.Game.PlayerPos.y)); // set the correct player position before the search
     Result:=Dead_.IsALegalPosition(BoxNo,DeadlockDetectionMaxPushCount,DeadlockDetectionTimeLimitMS,
                                    MainForm.Game.ReverseMode,
                                    DeadlockedBoxCount,DeadlockedBoxSquares);
     DeadlockStats:=Format('Deadlock detection stats: Fences: %d;  Pushes: %d;  Time: %d milli seconds',[FenceCount,PushCount,TimeMS]);
     end;
end;

function  TDeadlocks.LoadBoard : Boolean;
var Col,Row,BoxNo,GoalNo,SquareNo:Integer; GoalSet:TGoalSet; // Square:TColRow; //StartTimeMS:TTimeMS;
begin
  Result:=False;
  Dead_.Game.BoardIsLoaded:=False;
  OverflowingDeadlockSet:=0; Dead_.Game.DeadlockSets.OverflowingSetCount:=0;
  DeadlockedBoxCount:=0; DeadlockStats:='';

  if Calculated and Assigned(MainForm.Game) then with Dead_.Game do begin
     if Assigned(MainForm.GameViewer) then
        for Col:=1 to BoardWidth do
            for Row:=1 to BoardHeight do begin
                SquareNo:=Dead_.ColRowToSquare(Col,Row);
                if ((MainForm.Game.Board[Col,Row] and (FLOOR+INVISIBLE_WALL))=FLOOR) and
                   ((Dead_.Game.Board[SquareNo]   and WALL)<>0) then begin {the 'tubefiller' has filled this square; notify the main game board}
                   MainForm.Game.Board     [Col,Row]:=MainForm.Game.Board     [Col,Row] or (ILLEGAL_SQUARE+INVISIBLE_WALL);
                   MainForm.Game.StartBoard[Col,Row]:=MainForm.Game.StartBoard[Col,Row] or (ILLEGAL_SQUARE+INVISIBLE_WALL);
                   MainForm.GameViewer.Modified:=True;
                   end;
                if ((MainForm.Game.Board[Col,Row] and (FLOOR+ILLEGAL_SQUARE))=FLOOR) and
                   ((Dead_.Game.Board[SquareNo]   and FLAG_ILLEGAL_BOX_SQUARE)<>0) then begin {no path from the square to a goal}
                   MainForm.Game.Board     [Col,Row]:=MainForm.Game.Board     [Col,Row] or  ILLEGAL_SQUARE;
                   MainForm.Game.StartBoard[Col,Row]:=MainForm.Game.StartBoard[Col,Row] or  ILLEGAL_SQUARE;
                   MainForm.GameViewer.Modified:=True;
                   end;
                if ((MainForm.Game.Board[Col,Row] and (FLOOR+BOX_UNREACHABLE_FLOOR))=FLOOR) and
                   ((Dead_.Game.Board[SquareNo]   and FLAG_BOX_REACHABLE_SQUARE)=0) then begin {no boxes can reach the square}
                   MainForm.Game.Board     [Col,Row]:=MainForm.Game.Board     [Col,Row] or  BOX_UNREACHABLE_FLOOR;
                   MainForm.Game.StartBoard[Col,Row]:=MainForm.Game.StartBoard[Col,Row] or  BOX_UNREACHABLE_FLOOR;
                   MainForm.GameViewer.Modified:=True;
                   end;
                end;

     Result:=True; //not MainForm.Game.ReverseMode;
     if Result then begin
        {check if anything changed; otherwise it's not necessary to perform calculations again}
        if (BoxCount<>MainForm.Game.BoxCount) or
           (MainForm.Game.ReverseMode <> Dead_.Game.ReverseMode) then
           Result:=False;
        for BoxNo:=1 to BoxCount do
            if   Result then
                 Result:=BoxPos[BoxNo]=ColRowToSquare(MainForm.Game.BoxPos[BoxNo].x,MainForm.Game.BoxPos[BoxNo].y)
            else break;

        if not Result then begin {'not Result': the board changed; do the full calculation}
           Result:=True;

           if   Dead_.Game.ReverseMode<>MainForm.Game.ReverseMode then begin
                Dead_.Game.ReverseMode:=MainForm.Game.ReverseMode;
                CalculateReachableTargetsForAllSquares_C(Dead_.Game.JobNo,Dead_.Game.ReverseMode,True);
                end;
           if   Dead_.Game.ReverseMode then
                Dead_.Game.DistanceToNearestTarget:=Dead_.Game.DistanceToNearestBoxStartPosition
           else Dead_.Game.DistanceToNearestTarget:=Dead_.Game.DistanceToNearestGoal;

           ClearBoard;

           BoxCount:=MainForm.Game.BoxCount;
           for BoxNo:=1 to BoxCount do
               BoxPos[BoxNo]:=ColRowToSquare(MainForm.Game.BoxPos[BoxNo].x,MainForm.Game.BoxPos[BoxNo].y);
           PlayerPos:=ColRowToSquare(MainForm.Game.PlayerPos.x,MainForm.Game.PlayerPos.y);

           InitializeBoard(1); {update board with boxes and player-position, and update capacities for the deadlock sets}

           //StartTimeMS:=Dead_.GetTimeMS;
           if  BoxGoalMatching.Calculated then begin
               FillChar(GoalBoxMatching,SizeOf(GoalBoxMatching),0);

               for BoxNo:=1 to BoxCount do begin
                   BoxGoalMatching.SubSetCount[BoxNo]:=CalculateBoxSubSetCount(BoxNo);

                   GoalSet:=BoxGoalMatching.Edges[BoxPos[BoxNo]];
                   for GoalNo:=1 to GoalCount do
                       if GoalNo in GoalSet then with GoalBoxMatching do begin
                          Include(Edges[GoalNo],BoxNo); Inc(EdgeCount[GoalNo]);
                          end;
                   end;
               GoalBoxMatching.Calculated:=True;
{
               // check the initial position for (some types of) deadlocks
               for BoxNo:=1 to BoxCount do
                   if Result then begin
                      if   not ReverseMode then
                           SquareToColRow(GoalPos    [BoxNo],Col,Row)
                      else SquareToColRow(StartBoxPos[BoxNo],Col,Row);
                      Square.X:=Col; Square.Y:=Row;
                      Result:=WasALegalMove(-1,Square,Square);
                      end;
               if not Result then begin
                  // report the deadlock to the user
                  Result:=True;
                  end;
}
               end;

           //Msg(IntToStr(CalculateElapsedTimeMS(StartTimeMS,Dead_.GetTimeMS)),'LoadBoard '+IntToStr(BoxCount),MB_OK);
           end
        else
           MovePlayer(ColRowToSquare(MainForm.Game.PlayerPos.x,MainForm.Game.PlayerPos.y));

        if IsAWallSquare( Board[ PlayerPos ] ) then
           { the player is at a wall square; this can happen in the starting
             position and before the first push if the 'tubefiller' has filled
             some dead end squares connected to the original player position;
           }
           MovePlayer( StartPlayerPos );

        BoardIsLoaded:=True;
        end;
     end;
end;

procedure TDeadlocks.LoadGame(BackgroundJob__,CalculateDeadlockSets__:Boolean);
//var a,b,i,j,k:Integer; m:array[0..255,0..225] of Byte; StartTimeMS:TTimeMS;
begin {precondition: 'BackgroundJob__: cannot be mixed in one program session; only use 'True' or 'False' but never both}
{

(*
  StartTimeMS:=GetTickCount;
  k:=0;
  for a:=1 to 100 do for i:=0 to 255 do for j:=0 to 255 do Inc(k);
  Msg(IntToStr(CalculateElapsedTimeMS(StartTimeMS,GetTickCount)),IntToStr(k),MB_OK);

  StartTimeMS:=GetTickCount; j:=0;
  for i:=1 to 1000000 do begin
      Inc(j);
      end;
  Msg(IntToStr(CalculateElapsedTimeMS(StartTimeMS,GetTickCount)),IntToStr(j),MB_OK);

  StartTimeMS:=GetTickCount; j:=0;
  for i:=1 to 1000000 do begin
      if (Dead_.Game.BoxGoalMatching.Edges[i mod MAX_BOARD_SIZE]
          *
          Dead_.Game.BoxGoalMatching.Edges[0]
         )
         =
         Dead_.Game.BoxGoalMatching.Edges[0] then
         Inc(j);
      end;
  Msg(IntToStr(CalculateElapsedTimeMS(StartTimeMS,GetTickCount)),IntToStr(j),MB_OK);

  StartTimeMS:=GetTickCount; j:=0; FillChar(m,SizeOf(m),0);
  for i:=1 to 1000000 do begin
      if m[i and 255,i and 127]=Lo(i) then Inc(j);
      end;
  Msg(IntToStr(CalculateElapsedTimeMS(StartTimeMS,GetTickCount)),IntToStr(j),MB_OK);
*)

  StartTimeMS:=GetTickCount; a:=0; b:=0;
  for i:=0 to 255 do
      for j:=0 to 255 do
          for k:=0 to 255 do with Dead_.Game.BoxGoalMatching do
              if (k in Edges[i]) and (not k in Edges[j]) then Inc(a)
              else
              Inc(m[i,j]);
  Msg(IntToStr(CalculateElapsedTimeMS(StartTimeMS,GetTickCount)),IntToStr(a),MB_OK);
}


  NewJobNo;
  Clear;

  with DeadlocksThread do begin
    Enter;
    try     State                             := Ord(dlLoadGame);
            Board                             := MainForm.Game.StartBoard;
            BoardWidth                        := MainForm.Game.BoardWidth;
            BoardHeight                       := MainForm.Game.BoardHeight;
            Title                             := MainForm.Game.DisplayName;
            Dead_.Game.LogFileEnabled         := MainForm.Game.DeadlockDetection.LogEnabled;
            Dead_.Game.LogSquareGoalDistances := MainForm.Game.DeadlockDetection.LogSquareGoalDistances;
    finally Leave;
    end;
    if BackgroundJob__ and CalculateDeadlockSets__ then
       Resume {the 'Execute' procedure for the thread calculates the deadlock sets}
    else with DeadlocksThread do begin {load the game in the main thread}
       State:=Ord(dlIsCalculating);
       InitializeGame(BoardWidth,BoardHeight,Board,Title);
       if CalculateDeadlockSets__ and
          CalculateDeadlockSets then begin
          if State=Ord(dlIsCalculating) then begin
             CalculateReachableTargetsForAllSquares_C(Game.JobNo,Game.ReverseMode,True);
             if (State=Ord(dlIsCalculating)) or
                (State=Ord(dlStopCalculation)) then begin
                Game.DeadlockSets.Calculated:=True;
                end;
             end;
          end
       else
          CalculateDistanceToNearestGoalForAllSquares_B(Game.JobNo,True,Game.DistanceToNearestGoal);
       State:=Ord(dlIdle);
       end;
    end;
end;

function  TDeadlocks.PushesLowerBound : Integer;
begin
  if MainForm.Game.GameState<>gsSolved then begin
     if Calculated and
        (not MainForm.Game.IsReplaying) and
        MainForm.Game.DeadlockDetection.Enabled and
        (Dead_.Game.BoardIsLoaded or LoadBoard) and
        (not Dead_.Game.ReverseMode) then with Dead_.Game.DeadlockSets do begin
        Dead_.Game.CalculatePushesLowerBoundTimeLimitMS := CalculatePushesLowerBoundTimeLimitMS;
        Result := Dead_.CalculatePushesLowerBound;
        end
     else if   ( ThreadState = dlLoadGame ) or ( ThreadState = dlIsCalculating ) then
               Result := PUSHES_LOWER_BOUND_ERROR_TASK_FAILED_BUSY
          else Result := PUSHES_LOWER_BOUND_ERROR_TASK_FAILED;
     end
  else Result := 0;
end;

function  TDeadlocks.MoveBox(BoxNo__:Integer):Boolean;
var i,FromSquare,ToSquare{,NewPlayerPos}:Integer; FromGoalSet,ToGoalSet:TGoalSet;
begin {precondition: 'MainForm.Game' has just moved the box}
  Result:=Calculated;// and (not MainForm.Game.ReverseMode);

  if Result then with Dead_.Game do
     if BoardIsLoaded then begin
        FromSquare   :=BoxPos[BoxNo__];
        ToSquare     :=ColRowToSquare(MainForm.Game.BoxPos   [BoxNo__].x,MainForm.Game.BoxPos   [BoxNo__].y);

        if FromSquare<>ToSquare then begin
//         NewPlayerPos :=ColRowToSquare(Game.PlayerPos         .x,Game.PlayerPos         .y);

           Dec(Board[FromSquare],BOX);
           Inc(Board[ToSquare  ],BOX);
           BoxPos[BoxNo__]:=ToSquare;

{          // player position, hash-value, and simple lower bound aren't used, hence, don't update them
           Dec(Board[PlayerPos],PLAYER);
           PlayerPos:=NewPlayerPos;
           Inc(Board[PlayerPos],PLAYER);

           HashValue:=(HashValue xor SquareHashValues[FromSquare])
                      xor
                      SquareHashValues[ToSquare];
           Inc(SimpleLowerBound,DistanceToNearestTarget[ToSquare]-DistanceToNearestTarget[FromSquare]);
}
           with DeadlockSets do begin
             for i:=1 to SquareSetCount[FromSquare] do Inc(Capacity[SquareSetNumbers[FromSquare,i]]); {leaving  these deadlock-sets}
             for i:=1 to SquareSetCount[ToSquare  ] do Dec(Capacity[SquareSetNumbers[ToSquare  ,i]]); {entering these deadlock-sets}
             end;

           if BoxGoalMatching.Calculated and
              (BoxGoalMatching.EdgeSetNo[FromSquare] <> BoxGoalMatching.EdgeSetNo[ToSquare]) then begin

              with BoxGoalMatching do begin
                 SubSetCount[BoxNo__]:=0;           {recalculate number of subsets for the pushed box}
                 for i:=1 to BoxCount do begin
                     if IsASquareGoalEdgeSubSet(BoxPos[i],ToSquare) then
                        Inc(SubSetCount[BoxNo__]);  {the reachable goals for box 'i' is a subset of the reachable goals for box 'BoxNo__'}
                     if i<>BoxNo__ then begin       {'True': incrementally update all other boxes than the pushed one}
                        if IsASquareGoalEdgeSubSet(FromSquare,BoxPos[i]) then Dec(SubSetCount[i]); {the reachable goals for box 'BoxNo__' isn't  a subset of the reachable goals for box 'i' anymore, now that box 'BoxNo__' has been removed from 'FromSquare__'}
                        if IsASquareGoalEdgeSubSet(ToSquare  ,BoxPos[i]) then Inc(SubSetCount[i]); {the reachable goals for box 'BoxNo__' is now a subset of the reachable goals for box 'i',         now that box 'BoxNo__' has been moved   to   'ToSquare__'}
                        end;
                     end;
                 end;

              if GoalBoxMatching.Calculated then with GoalBoxMatching do begin
                 FromGoalSet:=BoxGoalMatching.Edges[FromSquare];
                 ToGoalSet  :=BoxGoalMatching.Edges[ToSquare  ];
                 for i:=1 to GoalCount do
                     if   i    in FromGoalSet then
                          if i in ToGoalSet   then {no change}
                          else begin Exclude(Edges[i],BoxNo__); Dec(EdgeCount[i]); {the box 'BoxNo__' cannot reach goal 'i' anymore}
                               end
                     else if i in ToGoalSet   then
                               begin Include(Edges[i],BoxNo__); Inc(EdgeCount[i]); {the box 'BoxNo__' can    reach goal 'i' now}
                               end;
                 end;
              end;
           end;
        end
     else
        Result:=LoadBoard;
end;

procedure TDeadlocks.Resume;
begin
  if DeadlocksThread<>nil then with DeadlocksThread do Resume;
end;

procedure TDeadlocks.SetOverflowingDeadlockSet(OverflowingDeadlockSet__:Integer);
begin
  Dead_.Game.DeadlockSets.OverflowingDeadlockSet:=OverflowingDeadlockSet__;
end;

procedure TDeadlocks.SetDeadlockDetectionTimeLimitMS(DeadlockDetectionTimeLimitMS__:TTimeMS);
begin {only values <= 'MAX_DEADLOCK_DETECTION_TIME_LIMIT_MS' are allowed}
  if (DeadlockDetectionTimeLimitMS__<=MAX_DEADLOCK_DETECTION_TIME_LIMIT_MS) then
     DeadlockDetectionTimeLimitMS:=DeadlockDetectionTimeLimitMS__;
end;

procedure TDeadlocks.Suspend;
begin
  if DeadlocksThread<>nil then with DeadlocksThread do Suspend;
end;

function  TDeadlocks.WasALegalMove(BoxNo__:Integer; BoxFromPos__,BoxToPos__:TColRow):Boolean;
var i,j,k,ControllerSetNo,Col,Row,FromSquare,ToSquare,SquareSetNo:Integer;
    IsASinglePush:TExtendedBoolean; d,Direction:TDirection;
    FromGoalSet:TGoalSet;
begin {returns 'False' if the position is a deadlock according to type>=lmhMedium, meaning that deadlock-sets were calculated when the level was loaded}
  Result:=True;
  OverflowingDeadlockSet:=0; Dead_.Game.DeadlockSets.OverflowingSetCount:=0;
  DeadlockedBoxCount:=0;
  if MainForm.Game.ReverseMode<>Dead_.Game.ReverseMode then
     Dead_.Game.BoardIsLoaded:=False;

  if Calculated and
     (not MainForm.Game.IsReplaying) and
     MainForm.Game.DeadlockDetection.Enabled and
     (MainForm.Game.GameState<>gsSolved) and
     (BoxNo__<>0) and
     (Dead_.Game.BoardIsLoaded or LoadBoard) then with Dead_.Game.DeadlockSets do begin
     IsASinglePush:=ebUnknown; Direction:=Low(Direction);

     FromSquare:=ColRowToSquare(BoxFromPos__.x,BoxFromPos__.y);
     ToSquare  :=ColRowToSquare(BoxToPos__  .x,BoxToPos__  .y);

     if (not Dead_.Game.ReverseMode) and (FromSquare<>ToSquare) then begin
        for i:=1 to SquareSetCount[ToSquare] do begin {for each deadlock-set that 'ToSquare' is a member of}
            SquareSetNo:=SquareSetNumbers[ToSquare,i]; {get current set number}

            if (Capacity[SquareSetNo]<0) then begin {is this deadlock-set overflowing?}
               Result:=False;

               for j:=1 to SquareSetCount[FromSquare] do
                   if SquareSetNumbers[FromSquare,j]=SquareSetNo then begin
                      {if 'FromSquare' is in the deadlock-set, the push didn't cause the overflow}
                      Result:=True; break;
                      end;

               if (not Result) and (dsfPlayerMustBeOutsideSet in Flags[SquareSetNo]) then begin
                  {the set overflows, but the player must be outside the set, or the set is a member of a controller/freeze-set pair (for convenience, they "cheat" by having the 'dsfPlayerMustBeOutsideSet' flag)}
                  if   IsASinglePush=ebUnknown then begin
                       IsASinglePush:=ebFalse;
                       for d:=Low(d) to High(d) do
                           if (BoxToPos__.x=BoxFromPos__.x+DIRECTION_XY[d,ColAxis]) and
                              (BoxToPos__.y=BoxFromPos__.y+DIRECTION_XY[d,RowAxis]) then begin
                              IsASinglePush:=ebTrue; Direction:=d; break;
                              end;
                       end;

                  if   (IsASinglePush=ebTrue) or (dsfControllerSet in Flags[Pred(SquareSetNo)]) then
                       if       DIRECTION_TO_DEADLOCK_SET_FLAG[Direction] in Flags[SquareSetNo] then begin
                                {when the box was pushed in this direction, then the player is outside the set,}
                                {hence, this is a deadlock}
                                end
                       else  if not (dsfControllerSet in Flags[Pred(SquareSetNo)]) then begin {'True': this is a normal deadlock-set or a controller-set; an overflowing controller-set has no interest and only ends up here to avoid an extra 'if' test}
                                {save the overflowing set numbers for later analysis}
                                if OverflowingSetCount<High(OverflowingSets) then begin
                                   Inc(OverflowingSetCount);
                                   OverflowingSets[OverflowingSetCount]:=SquareSetNo;
                                   end;
                                Result:=True;
                                end
                             else begin {this deadlock-set is the freeze-set in a controller/freeze-set pair; the squares in the controller-set must contain enough boxes before the squares in the freeze-set freeze}
                                if   Capacity[Pred(SquareSetNo)]>0 then {'True': the squares in the controller-set don't contain the required minimum number of boxes}
                                     if        not (dsfTestForFreezingSquare in Flags[SquareSetNo]) then {'True': it's known from the precalculation, that when the squares in the freeze set have been filled, then the boxes at these squares have frozen}
                                     else if   IsAFreezingMove(FromSquare,ToSquare,0,True,False) then {'True': moving a box to the single square in the freeze set causes the box to freeze}
                                          else Result:=True {'True': it's not a deadlock}
                                else Result:=True; {'True': it's not a deadlock}
                                end
                  else Result:=True;
                  end;

               if not Result then begin {pushing the box to 'ToSquare' overflowed this deadlock-set}
                  OverflowingDeadlockSet:=SquareSetNo;

                  if   dsfControllerSet in Flags[Pred(SquareSetNo)] then
                       ControllerSetNo:=Pred(SquareSetNo)
                  else ControllerSetNo:=0;

                  for j:=0 to Game.BoardSize do {find squares belonging to this deadlock-set}
                      for k:=1 to SquareSetCount[j] do begin
                          SquareSetNo:=SquareSetNumbers[j,k];
                          if ((SquareSetNo=Dead_.Game.DeadlockSets.OverflowingDeadlockSet)
                              and
                              IsABoxSquare(j)
                             )
                             or
                             (SquareSetNo=ControllerSetNo) then begin
                             SquareToColRow(j,Col,Row);
                             Inc(DeadlockedBoxCount);
                             DeadlockedBoxSquares[DeadlockedBoxCount].x:=Col;
                             DeadlockedBoxSquares[DeadlockedBoxCount].y:=Row;
                             end;
                          end;
                  break;
                  end;
               end;
            end;
        end;

     if Result and Game.GoalBoxMatching.Calculated then
        with Game do with GoalBoxMatching do begin
          FromGoalSet:=BoxGoalMatching.Edges[FromSquare];
          for i:=1 to GoalCount do
              if (EdgeCount[i]=0) and
                 (i in FromGoalSet) then begin
                 if   not Dead_.Game.ReverseMode then
                      SquareToColRow(GoalPos    [i],Col,Row)
                 else SquareToColRow(StartBoxPos[i],Col,Row);
                 Inc(DeadlockedBoxCount); {it's actually a target square (goal or box starting position) which is marked as deadlocked, not a box}
                 DeadlockedBoxSquares[DeadlockedBoxCount].x:=Col;
                 DeadlockedBoxSquares[DeadlockedBoxCount].y:=Row;
                 Result:=False;
                 end;
          end;

     if Result and Game.BoxGoalMatching.Calculated then
        with Game do with BoxGoalMatching do begin
          for i:=1 to BoxCount do
              if (SubSetCount[i]>EdgeCount[BoxPos[i]]) and {'True': b boxes for g goals, b > g}
                 IsASquareGoalEdgeSubSet(ToSquare,BoxPos[i]) then begin
                 for j:=1 to BoxCount do
                     if IsASquareGoalEdgeSubSet(BoxPos[j],BoxPos[i]) then begin
                        SquareToColRow(BoxPos[j],Col,Row);
                        Inc(DeadlockedBoxCount);
                        DeadlockedBoxSquares[DeadlockedBoxCount].x:=Col;
                        DeadlockedBoxSquares[DeadlockedBoxCount].y:=Row;
                        end;
                 Result:=False;
                 break;
                 end;
          end;
     end;
end;

{
procedure Initialize;
begin
  Assign(LogFile,'Dead.log'); Rewrite(LogFile);
end;

procedure Finalize;
begin
  CloseFile(LogFile);
end;

initialization
  Initialize;
finalization
  Finalize;
}

end.

