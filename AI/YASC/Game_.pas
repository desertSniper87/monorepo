unit Game_;

// This module fills the gap between the game kernel, 'SokGame_.TSokGame',
// and the viewer 'GView_.TGameViewer'.

// The kernel contains all the Sokoban game-logic, and the viewer
// displays the game on the screen.

// Between these modules, 'TGame' acts as a 'controller', and handles
// user-commands such as 'Replay', 'Do Move', 'Load Game', etc.

// This is not a clean and abstract module; among other things,
// the move-animation logic is hard-wired to work with the
// 'MainForm' game-viewer and the 'ToolsForm' game-viewer.

interface

uses
  Windows,Graphics,Classes,BitMap_,Misc_,Pack_,SokUtil_,SokFile_,SokGame_,MView_,Dead_;

const
  DEFAULT_ANIMATE_DO_MOVE_MS         = 25; // before version 1.275: 50;
  DEFAULT_ANIMATE_UNDO_MOVE_MS       = 20; // before version 1.275: 25;
  DEFAULT_ANIMATE_REPLAY_MOVES_MS    = 200;
  DEFAULT_ANIMATE_MOVES_ON_MOUSE_WHEEL_UP_DOWN
                                     = False;
  DEFAULT_DELAY_MOVE_MILLI_SECS      = 0;//50;
  DEFAULT_DEADLOCK_DETECTION_TYPE    = lmhHigh;
  DEFAULT_FORKLIFT_DRIVING_DRIVE_IN_REVERSE_SQUARES
                                     = 4;
  DEFAULT_LEVEL_FILE_NAME            = 'du Peloux'+SOKOBAN_FILE_NAME_EXT;

  DIRECTION_TO_KEY                   : array[TDirection] of Word
                                     = (VK_UP,VK_LEFT,VK_DOWN,VK_RIGHT);

  MAX_ANIMATION_TIME_MS              = 1000; // 1 second
  MAX_COLLAPSED_MOVES                = 65000; // the collapsed moves vector is smaller than the maximum number of moves; otherwise allocating the vector on the stack - as the replay functions do - causes stack overflow
  MAX_PATH_FINDING_MAX_TIME_MS       = 60000;
  MAX_PUSHES_LOWER_BOUND_CALCULATION_MAX_TIME_MS
                                     = 10 * 60 * 1000; // minutes * seconds * milliseconds
  MAX_REPLAY_SPEED_MOVES_PER_SEC     = 100;
  MAX_SMOOTH_ANIMATION_THRESHOLD_MAX_PIXELS_PER_MOVE
                                     = 10;
  MAX_TIMING_IDLE_TIME_THRESHOLD_MS  = 3600 * 1000; // milli-seconds
  MIN_PATH_FINDING_MAX_TIME_MS       = 1000; // 1 second;

  BT_WALL_NO_NEIGHBOR_WALLS          = 0;  // flags for wall drawing; 0..16 enumerate the different types, where '16' denotes the wall cap; the values cannot be changed
  BT_WALL_ABOVE                      = 1;  // meaning that there is a wall on the square above
  BT_WALL_TO_THE_RIGHT               = 1 shl 1;
  BT_WALL_BELOW                      = 1 shl 2;
  BT_WALL_TO_THE_LEFT                = 1 shl 3;
  BT_WALL_CAP                        = 1 shl 4; // filling wall gaps
  BT_SQUARE                          = 1 shl 5; // flags for bevel drawing
  BT_LEFT                            = 1 shl 6;
  BT_TOP                             = 1 shl 7;
  BT_RIGHT                           = 1 shl 8;
  BT_BOTTOM                          = 1 shl 9;
  BT_DENT_TOP_LEFT                   = 1 shl 10;
  BT_DENT_TOP_RIGHT                  = 1 shl 11;
  BT_DENT_BOTTOM_LEFT                = 1 shl 12;
  BT_DENT_BOTTOM_RIGHT               = 1 shl 13;
  BT_CORNER_TOP_LEFT                 = 1 shl 14;
  BT_CORNER_TOP_RIGHT                = 1 shl 15;
  BT_CORNER_BOTTOM_LEFT              = 1 shl 16;
  BT_CORNER_BOTTOM_RIGHT             = 1 shl 17;
  BT_INTERIOR_NON_FLOOR_EMPTY_SQUARE = 1 shl 18;
  BT_EDGE                            = BT_LEFT           +BT_TOP             +BT_RIGHT             +BT_BOTTOM;
  BT_DENT                            = BT_DENT_TOP_LEFT  +BT_DENT_TOP_RIGHT  +BT_DENT_BOTTOM_LEFT  +BT_DENT_BOTTOM_RIGHT;
  BT_CORNER                          = BT_CORNER_TOP_LEFT+BT_CORNER_TOP_RIGHT+BT_CORNER_BOTTOM_LEFT+BT_CORNER_BOTTOM_RIGHT;
  BT_WALL_NEIGHBORS_MASK             = BT_WALL_ABOVE+BT_WALL_TO_THE_RIGHT+BT_WALL_BELOW+BT_WALL_TO_THE_LEFT;

  HUFFMAN_BASE64_ENCODING_LAST_2_CHARACTERS  // see "RFC3548 section 4. Base 64 Encoding with URL and Filename Safe Alphabet"
                                     = '-_'; // the Huffman-Base64 encoded boards uses these characters instead of '+' and '-' for the Base64 encoding

type
  TCollapsedMove                     = packed record
    BoxNo                            : Int16;
    Direction                        : TDirection;
    Start                            : Integer;
    // Filler                        : Byte; // don't fill up for alignment here; a reverse-mode solution may crash with a stack overflow in 'TestForNewBestSolution.ReverseHistory'
  end;
  TCollapsedMoves                    = array[0..MAX_COLLAPSED_MOVES+1] of TCollapsedMove;

  TDeadlockDetection                 = record    {kludge: the information in this record should really have}
    BlockMoves                       : Boolean;  {been properties of 'TDeadlocks', but they ended up here as}
    Enabled                          : Boolean;  {a result of the way the program evolved}
    DeadlockDetectionType            : TLowMediumHigh;
    Deadlocks                        : TDeadlocks;
    LogEnabled                       : Boolean;
    LogSquareGoalDistances           : Boolean;
  end;

  TGameAction                        = (gaNull,gaMove,gaUndo,gaRedo);

  TSquareColor                       = (FloorColor,WallColor,PlayerColor,BoxColor,
                                        GoalColor,BoxOnGoalColor);
  TSquareColors                      = array[TSquareColor] of TColor;

  TLegalMoves                        = array[0..Ord(High(TDirection))-Ord(Low(TDirection))+2] of TColRow; {+2: item 0 isn't used}

  TMethod                            = procedure of object;

type
  TGame = class(TSokoGame)
  private
    BrowseChoicePoints:Boolean;    // 'False': browse pushes; 'True': browse boxlines, which roughly can be said to be natural choice points
    BrowseStartPosition:Integer;
    fBrowsePosition:Integer;
    fMaxBrowsePosition:Integer;
    fFileName:String; // full level-name in 'inifile-format': 'Path\FileName\[LevelName]' ('\' is really 'FILE_NAME_PATH_DELIMITER')
    fLastValidFileName:String;
    UpdateMultiNoEnabled:Boolean;
    function  CollapseMoves(Forwards__,UsePlayerImageForMoveAnimationAlsoForGoalSquares__,UseBoxImageForMoveAnimationAlsoForGoalSquares__:Boolean;
                            var Count__:Integer; var CollapsedMoves__:TCollapsedMoves):Boolean;
  protected
    procedure BoxIllegalSquares(Col,Row:Integer; TimeLimitEnabled__:Boolean; var IllegalMovesMask__:Integer);
    procedure SetBrowsePosition(BrowsePosition__:Integer);
    procedure SetFileName(const FileName__:String);
    procedure SetLastValidFileName(const LastValidFileName__:String);
  public
    AnimateDoMoveMS:Integer;       // do   move,    milli-secs
    AnimateUndoMoveMS:Integer;     // undo move,    milli-secs
    AnimateReplayMovesMS:Integer;  // replay moves, milli-secs
    AnimateMovesOnMouseWheelUpDown:Boolean;
    BookmarkCount:Integer;
    Bookmark:array[0..MAX_BOOKMARKS+1] of Integer;
    BTSquare:TBoard;               // board topology flags for wall-drawing and bevel
    BTSquareCount:Integer;
    DeadlockDetection:TDeadlockDetection;
    DelayMoveMS:Integer;
    Color:TSquareColors;
    ForkLiftDrivingEnabled:Boolean;
    ForkliftDrivingDriveInReverseSquares:Integer;
    HourGlassCursor:Boolean;
    IsBrowsing:Boolean;
    IsBusy:Boolean;
    IsReplaying:Boolean;
    LastErrorStr:String;
    MoveAnimationEnabled:Boolean;
    MoveCountSinceLastKeyUpEvent:Integer;
    PlayerCanReachTheBorder:Boolean;
    PlayerDirectionAnimationEnabled:Boolean;     // settings
    SaveOldSolutionsAfterClipboardImport:Boolean;
    SessionSmoothMoveAnimationEnabled:Boolean;   // current value during this session; see also 'SmoothMoveAnimationEnabled'
    ShowGame:TMethod;
    ShowMovesEnabled:Boolean;
    SmoothMoveAnimationEnabled:Boolean;          // settings; see also 'SessionSmoothMoveAnimationEnabled'
    SmoothMoveAnimationThresholdEnabled:Boolean;
    SmoothMoveAnimationThresholdMaxPixelsPerMove:Integer;
    UserBreakWhileReplaying:Boolean;

    constructor Create;
    destructor  Destroy; override;

    function    BoxesToSquare(const Square__:TColRow; var TimeOut__:Boolean):Integer;
    function    BoxLegalMoves(Col,Row:Integer; var TimeOut:Boolean):Integer; override;
    function    BoxPath(StartPos__,EndPos__,PlayerPos__:TColRow;
                        var MoveCount__:Integer; var Moves__:TMoves):Boolean; override;
    function    CalculateIfPlayerCanReachTheBorder:Boolean;
    procedure   CalculateInternalData; override;
    function    CanCombineCurrentPositionAndSnapshotToFormASolution(Snapshot:TSnapshot):Boolean;
    procedure   Clear; override;
    function    CloseLevel(Flush__:Boolean):Boolean; override;
    function    CombineCurrentPositionAndSnapshotToFormASolution(Snapshot:TSnapshot):Boolean;
    function    CopyMovesToClipboard(CopyContinuationMovesOnly__,RunLengthEncoding__:Boolean):Boolean;
    function    CopySnapshotToClipboard(Snapshot__:TSnapshot; RunLengthEncoding__:Boolean):Boolean;
    function    CopyToClipboard(FloorCharacter__:Char; PreserveCombinedMoves__,RunLengthEncoding__,BoardOnly__,CurrentBoardOnly__:Boolean):Boolean;
    procedure   DeleteSnapshot(var Snapshot:TSnapshot); override;
    function    DoMoveUpdateBoard(dx,dy,Flags:Integer):Integer; override;
    function    EnterBrowseMode(BrowseChoicePoints__:Boolean):Boolean; virtual;
    function    GoToPosition(Position:Integer; SeparateUndoneMoves__:Boolean):Integer;
    function    HasModifiedSnapshots:Boolean;
    function    HuffmanBase64EncodedBoard:String;
    function    HuffmanDecodeBoard(Buffer__:PByte; BufferByteSize__:Integer):String;
    function    HuffmanEncodeBoard(var HuffmanEncodedBoard__:TBoardOfBytes1D; var HuffmanEncodedBoardByteSize__:Integer):Boolean;
    function    InheritedRedo(RedoCombinedMoves__:Boolean):Boolean; virtual;
    function    InheritedUndo(RedoCombinedMoves__:Boolean):Boolean; virtual;
    procedure   InitGame;
    function    IsALegalMove(dx__,dy__:Integer; Flags__:Integer; var IsAFreezingMove__:Boolean):Boolean; override;
    function    IsALegalPosition:Boolean;
    function    IsIdenticalBoxPosition(PlayerPos__:TColRow; const BoxPos__:TBoxPositions; BoxNumbersMustMatch__,PlayerPositionMustMatch__:Boolean):Boolean;
    function    IsIdleAndStopReplayingAndBrowsing:Boolean;
    procedure   LeaveBrowseMode(RestorePosition:Boolean); virtual;
    function    MakeForwardSolutionFromBackwardSolution(WorkGame__:TGame; BackwardSolution__:TSnapshot; var ForwardSolution__:TSnapshot):Boolean;
    function    MakeLegalMoves(var MoveCount:Integer; var Moves:TLegalMoves):Boolean;
    procedure   LoadBoard(const Board__:TBoard); override;
    function    LoadFromFileOrClipboard(const FileName__:String; DestGame__:TGame; MultiViewItem__:TMultiViewItem; var Modified__:Boolean):Boolean;
    function    LoadSnapshot(Snapshot:TSnapshot):Boolean; override;
    function    MakeSnapshot(const Name:String):TSnapshot; override;
    function    MoveToStrChessNotation(const FromPos,ToPos:TColRow; BoxNo:Integer):String;
    function    MakeNewFileName(const Path,FileNameStub:String; var FileName:String):Boolean;
    function    Redo(RedoCombinedMoves:Boolean):Boolean; override;
    function    ReplayInMainWindow(Forwards:Boolean):Boolean;
    function    ReplayInToolsWindow(Forwards,SingleMove:Boolean):Boolean;
    procedure   Reset(ResetCombinedMoveNumbers:Boolean); override;
    procedure   RestoreBookmark;
    procedure   SaveBookmark;
    function    SaveToFile(FileName__:String; Flush__:Boolean):Boolean;
    function    SnapshotType(Snapshot__:TSnapshot):TSnapshotType;
    function    SnapshotTypeName(SnapshotType__:TSnapshotType):String; override;
    procedure   StartTimer; override;
    procedure   StopTimer; override;
    function    StrChessNotationToMove(const s:String; var FromPos,ToPos:TColRow; BoardWidth,BoardHeight:Integer):Boolean;
    function    TestForNewBestSolution:Boolean; override;
    function    TryMove(Direction:TDirection; LastMoveIndex,Flags:Integer):Boolean;
    procedure   TryMoves(MoveCount:Integer; var Moves:TMoves);
    function    Undo(UndoCombinedMoves:Boolean):Boolean; override;
    function    VisiblePlayerDirection:TDirection;

    property    BrowsePosition       :Integer read fBrowsePosition    write SetBrowsePosition;
    property    MaxBrowsePosition    :Integer read fMaxBrowsePosition;
    property    FileName             :String  read fFileName          write SetFileName;
    property    LastValidFileName    :String  read fLastValidFileName write SetLastValidFileName;
  end;

  TMovesOrHistory = record
    case Boolean of
      False: (Moves:TMoves);
      True : (History:THistory);
    end;


{
 --------------------------------------------------------------------
 Global variables
 --------------------------------------------------------------------
}

// kludge:
// the move-vector for 'TMainForm.DrawGrid1MouseDown', 'TMainForm.DrawGrid1MouseMove',
//  and 'TMainForm.DrawGrid1DragDrop' must be allocated globally;
// allocating the vector locally on the stack causes a stack overflow in
// 'Dead_.IsALegalPosition';
// 'TGame.Redo', must also allocate its saved history globally in order to avoid stack-overflow;

var
  MovesOrHistory:TMovesOrHistory;

implementation

uses
  Forms,Controls,ComCtrls,SysUtils,Clipbrd,MMSystem,StdCtrls,
  Text_,Status_,LSView_,Tools_,Main_,Sound_,Open1_,Snapshots_;

procedure  TGame.SetFileName(const FileName__:String);
begin
  fFileName:=FileName__;
end;

procedure TGame.SetLastValidFileName(const LastValidFileName__:String);
begin
  fLastValidFileName:=LastValidFileName__;
end;

constructor TGame.Create;
begin
  Inherited;
  SokoFile                                    :=MainForm.SokoFile; // use one file reader/writer only in this application

  Clear;
  fLastValidFileName                          :='';
  MoveAnimationEnabled                        :=True;
  PlayerDirectionAnimationEnabled             :=False;
  SmoothMoveAnimationEnabled                  :=True;
  SmoothMoveAnimationThresholdEnabled         :=False;
  SmoothMoveAnimationThresholdMaxPixelsPerMove:=4;
  SessionSmoothMoveAnimationEnabled           :=SmoothMoveAnimationEnabled;
  AnimateDoMoveMS                             :=DEFAULT_ANIMATE_DO_MOVE_MS;
  AnimateUndoMoveMS                           :=DEFAULT_ANIMATE_UNDO_MOVE_MS;
  AnimateReplayMovesMS                        :=DEFAULT_ANIMATE_REPLAY_MOVES_MS;
  AnimateMovesOnMouseWheelUpDown              :=DEFAULT_ANIMATE_MOVES_ON_MOUSE_WHEEL_UP_DOWN;
  with DeadlockDetection do begin
    BlockMoves                                :=True;
    Enabled                                   :=True;
    DeadlockDetectionType                     :=DEFAULT_DEADLOCK_DETECTION_TYPE;
    Deadlocks                                 :=nil;
    LogEnabled                                :=False;
    LogSquareGoalDistances                    :=False;
    end;
  DelayMoveMS                                 :=DEFAULT_DELAY_MOVE_MILLI_SECS;
  ForkliftDrivingEnabled                      :=False;
  ForkliftDrivingDriveInReverseSquares        :=DEFAULT_FORKLIFT_DRIVING_DRIVE_IN_REVERSE_SQUARES;
  UserBreakWhileReplaying                     :=False;
  IsBusy                                      :=False;
  IsReplaying                                 :=False;
  IsBrowsing                                  :=False;
  SaveOldSolutionsAfterClipboardImport        :=True;
  ShowMovesEnabled                            :=True;
  UpdateMultiNoEnabled                        :=True;
  HourGlassCursor                             :=True;
  LastErrorStr                                :='';

  Color[FloorColor]                           :=clLtGray;
  Color[WallColor ]                           :=clGray;
  Color[PlayerColor]                          :=clNavy;
  Color[BoxColor]                             :=clGreen;
  Color[GoalColor]                            :=clGreen;
  Color[BoxOnGoalColor]                       :=clGreen;

//Msg(IntToStr(Cardinal(Addr(Self.USerBreakWhileReplaying))-Cardinal(Addr(Self.BestSolutionMoves))),'Game Size',MB_OK);
end;

destructor TGame.Destroy;
begin
  fLastValidFileName:=''; // release strings
  Inherited;
end;

function TGame.BoxesToSquare(const Square__:TColRow; var TimeOut__:Boolean):Integer;
const
  COUNT_DOWN_TO_NEXT_TIME_CHECK=1000; {check the time each time this number of queue items have been expanded}
type {calculates boxes that can move to 'Square__'}
  TQueueItem = packed record BoxSquare,PlayerSquare:TColRow; end;
  PQueueItem = ^TQueueItem;
  TQueueItems= array[0..MAX_BOARD_SIZE*NUMBER_OF_DIRECTIONS+1+NUMBER_OF_DIRECTIONS] of TQueueItem;
  TQueue     = record
    Bottom   : PQueueItem;
    Items    : TQueueItems;
    Top      : PQueueItem;
  end;
var
  BoxNo,Col,Row,IllegalSquareMask,InvisibleWallMask:Integer;
  VisitedTimeStamp:SokUtil_.TTimeStamp;
  {TimeNowMS:TTimeMS;}
  Direction:TDirection;
  OriginalPlayerPos:TColRow; OriginalBoard:TBoard;
  Visited:TBoardDirectionArrayOfTimeStamps;
  TargetStates:TBoardDirectionArrayOfBoolean;

  function BackwardSearchForTargetStates(const Square__      :TColRow;
                                         var   TargetStates__:TBoardDirectionArrayOfBoolean;
                                         var   TimeStamp__   :SokUtil_.TTimeStamp;
                                         var   Visited__     :TBoardDirectionArrayOfTimeStamps;
                                         var   TimeOut__      :Boolean):Integer;
  {searches backwards for [col,row,direction] states from which there exists a}
  {path to the target square; later, the forward search for each box can stop}
  {as soon as it finds a path to one of these target states}
  var Col,Row,CountDownToNextTimeCheck,PlayerFromSquareValue:Integer;
      TimeNowMS:TTimeMS; IsALegalMove:Boolean; Direction:TDirection;
      BoxSquare,BoxToSquare,PlayerFromSquare,PlayerToSquare,LastBoxSquare:TColRow;
      Queue:TQueue;
  begin
    Result:=0; FillChar(TargetStates__,SizeOf(TargetStates__),0);

    Inc(TimeStamp__); {prepare updating the 'Visited__' timestamps}
    Queue.Bottom:=Addr(Queue.Items[Low(Queue.Items)]); Queue.Top:=Queue.Bottom;

    for Direction:=Low(Direction) to High(Direction) do begin {initialize the search by putting legal start moves on the queue}
        PlayerFromSquare.x   :=Square__.x+DIRECTION_XY[Direction,ColAxis];
        PlayerFromSquare.y   :=Square__.y+DIRECTION_XY[Direction,RowAxis];
        PlayerFromSquareValue:=Board[PlayerFromSquare.x,PlayerFromSquare.y];
        if (PlayerFromSquareValue and (WALL+BOX+FLOOR))=FLOOR then begin
           Inc(Queue.Top); // initialize the search by putting the legal start positions on the queue
           Queue.Top^.BoxSquare:=Square__; Queue.Top^.PlayerSquare:=PlayerFromSquare;
           end;
        if (PlayerFromSquareValue and (WALL+FLOOR))=FLOOR then begin
           Inc(Result); TargetStates__[Square__.x,Square__.y,OPPOSITE_DIRECTION[Direction]]:=True; {count target states and mark the target position as legal}
           end;
        end;

    LastBoxSquare.x:=0;
    CountDownToNextTimeCheck:=COUNT_DOWN_TO_NEXT_TIME_CHECK;

    while Queue.Bottom<>Queue.Top do begin

          Inc(Queue.Bottom);
          BoxSquare:=Queue.Bottom^.BoxSquare; PlayerPos:=Queue.Bottom^.PlayerSquare;

          Inc(Board[BoxSquare.x,BoxSquare.y],BOX); {put the box on the board}

          if     (BoxSquare.x<>LastBoxSquare.x)   or
                 (BoxSquare.y<>LastBoxSquare.y)   or
                 /////(PlayerPathBoardTimestamps.Squares[PlayerPos.x,PlayerPos.y]<PlayerPathBoardTimestamps.Timestamp) then begin
                 ((Board[PlayerPos.x,PlayerPos.y] and PLAYER_LEGAL_MOVE)=0) then begin
                 {the current set of player's reachable squares isn't valid anymore; recalculate the set}
                 LastBoxSquare:=BoxSquare;
                 /////CalculatePlayersDistanceToAllReachableSquares(PlayerPos); {calculate player's reachable squares, i.e., to see which sides of the box the player can reach}
                 PlayerLegalMoves(0,0,Col,Row); {calculate player's reachable squares, i.e., to see which sides of the box the player can reach}
                 end;

          for Direction:=Low(Direction) to High(Direction) do with PlayerPathBoardTimestamps do begin
              if not ReverseMode then begin // forward mode game: pull the box away from the square (the opposite of what happens in the game)
                 BoxToSquare     .x:=BoxSquare  .x+DIRECTION_XY[Direction,ColAxis];
                 BoxToSquare     .y:=BoxSquare  .y+DIRECTION_XY[Direction,RowAxis];
                 PlayerFromSquare  :=BoxToSquare;
                 PlayerToSquare  .x:=BoxToSquare.x+DIRECTION_XY[Direction,ColAxis];
                 PlayerToSquare  .y:=BoxToSquare.y+DIRECTION_XY[Direction,RowAxis];
                 IsALegalMove      :=(Visited__  [BoxToSquare     .x,BoxToSquare   .y,Direction]<TimeStamp__) and
                                     ((Board     [BoxToSquare     .x,BoxToSquare   .y] and (WALL+InvisibleWallMask+BOX+IllegalSquareMask+PLAYER_LEGAL_MOVE))=PLAYER_LEGAL_MOVE) and
                                     ((Board     [PlayerToSquare  .x,PlayerToSquare.y] and (WALL+BOX))=0);
                 end
              else begin // reverse mode game: push the box away from the square (the opposite of what happens in the game)
                 BoxToSquare     .x:=BoxSquare  .x+DIRECTION_XY[Direction,ColAxis];
                 BoxToSquare     .y:=BoxSquare  .y+DIRECTION_XY[Direction,RowAxis];
                 PlayerFromSquare.x:=BoxSquare  .x-DIRECTION_XY[Direction,ColAxis];
                 PlayerFromSquare.y:=BoxSquare  .y-DIRECTION_XY[Direction,RowAxis];
                 PlayerToSquare    :=BoxSquare;
                 IsALegalMove      :=(Visited__  [BoxToSquare     .x,BoxToSquare     .y,Direction]<TimeStamp__) and
                                     ((Board     [PlayerFromSquare.x,PlayerFromSquare.y] and PLAYER_LEGAL_MOVE)<>0) and
                                     ((Board     [BoxToSquare     .x,BoxToSquare     .y] and (WALL+InvisibleWallMask+BOX+IllegalSquareMask))=0);
                 end;
              if IsALegalMove then begin
                 Inc(Queue.Top); // put the position on the queue for later expansion
                 Queue.Top^.BoxSquare:=BoxToSquare; Queue.Top^.PlayerSquare:=PlayerToSquare;
                 Inc(Result); TargetStates__[BoxToSquare.x,BoxToSquare.y,OPPOSITE_DIRECTION[Direction]]:=True;
                 Visited__[BoxToSquare.x,BoxToSquare.y,Direction]:=TimeStamp__;
                 end;
              end;

          Dec(Board[BoxSquare.x,BoxSquare.y],BOX); {remove the box from the board again}

          Dec(CountDownToNextTimeCheck);
             if (CountDownToNextTimeCheck<=0) then begin {'True': check the time now}
                CountDownToNextTimeCheck:=COUNT_DOWN_TO_NEXT_TIME_CHECK;
                TimeNowMS:=GetTickCount;
                if (TimeNowMS>=PathFindingTimeInterval.StopTimeMS)
                   or
                   (TimeNowMS<PathFindingTimeInterval.StartTimeMS) then
                   if Queue.Bottom<>Queue.Top then begin {'True': the search is not over yet; i.e., it hasn't succeeded and it hasn't examined all legal moves yet}
                      Queue.Top:=Queue.Bottom; // stop the search
                      TimeOut__:=True;
                      end;
                end;
          end;
    {Msg('Nodes: '+IntToStr(Result),'Time: '+IntToStr(CalculateElapsedTimeMS(PathFindingTimeInterval.StartTimeMS,GetTickCount)),MB_OK);}
  end; {BackwardSearchForTargetStates}

  function ForwardSearch(BoxNo__              :Integer;
                         const Square__       :TColRow;
                         const TargetStates__ :TBoardDirectionArrayOfBoolean;
                         const OriginalBoard__:TBoard;
                         var   TimeStamp__    :SokUtil_.TTimeStamp;
                         var   Visited__      :TBoardDirectionArrayOfTimeStamps;
                         var   TimeOut__      :Boolean):Boolean;
  {searches for a path from the box starting position to one of the target states}
  {from which there exists a path to the specified square}
  var Col,Row,CountDownToNextTimeCheck:Integer; TimeNowMS:TTimeMS; IsALegalMove:Boolean; Direction:TDirection;
      BoxSquare,BoxToSquare,PlayerFromSquare,PlayerToSquare,LastBoxSquare:TColRow;
      Queue:TQueue;
  begin
    Result:=False;

    Queue.Bottom:=Addr(Queue.Items[Low(Queue.Items)]); Queue.Top:=Queue.Bottom;
    BoxSquare:=BoxPos[BoxNo__];

    for Direction:=Low(Direction) to High(Direction) do begin {initialize the search by putting legal start moves on the queue}
        PlayerFromSquare.x:=BoxSquare.x+DIRECTION_XY[Direction,ColAxis];
        PlayerFromSquare.y:=BoxSquare.y+DIRECTION_XY[Direction,RowAxis];
        if (OriginalBoard__[PlayerFromSquare.x,PlayerFromSquare.y] and PLAYER_LEGAL_MOVE)<>0 then begin
           Inc(Queue.Top); {initialize the search by putting the legal start positions on the queue}
           Queue.Top^.BoxSquare:=BoxSquare; Queue.Top^.PlayerSquare:=PlayerFromSquare;
           end;
        end;

   if Queue.Bottom<>Queue.Top then begin {'True': the player can reach the box; that's the first condition for pushing it}
      with BoxPos[BoxNo__] do Dec(Board[x,y],BOX); {remove the box from the board}

      {check if it's legal to move this box to 'Square__'}
      if // (not ReverseMode) and
         (SimpleIllegalMovesMask<>0)
         and
         (((Board[Square__.x,Square__.y] and (ILLEGAL_SQUARE+INVISIBLE_WALL))<>0)
          or
          IsAFreezingMove(0,0,Square__.x,Square__.y,OriginalPlayerPos.x,OriginalPlayerPos.y)
          or
          (// (not ReverseMode) and
           (DeadlockDetection.DeadlockDetectionType>=lmhMedium) and
           DeadlockDetection.Enabled and
           Assigned(DeadlockDetection.Deadlocks) and
           (not DeadlockDetection.Deadlocks.IsALegalMove(BoxNo__,Square__.x,Square__.y,False))
          )
         ) then
         Queue.Bottom:=Queue.Top; {the move is illegal}

      with BoxPos[BoxNo__] do Inc(Board[x,y],BOX); {put the box on the board again}
      end;

    if (Queue.Bottom<>Queue.Top) {and (not TimeOut__)} then begin
       with BoxPos[BoxNo__] do Dec(Board[x,y],BOX); {remove the box from the board}

       Inc(TimeStamp__); {prepare updating the 'Visited__' timestamps}

       {check the time after expanding the first few queue items;
        setting the countdown to 'DIRECTION_COUNT * DIRECTION_COUNT + 1'
        guarantees that the box has had the chance to move one step in each
        direction, and if it thereby reaches one of the squares found during the
        backward search for paths leading to the specified square, then this
        forward search finds a path, even if the time limit has been exceeded
       }
       CountDownToNextTimeCheck:=DIRECTION_COUNT*DIRECTION_COUNT+1;

       LastBoxSquare.x:=0;

       while Queue.Bottom<>Queue.Top do begin

             Inc(Queue.Bottom);
             BoxSquare:=Queue.Bottom^.BoxSquare; PlayerPos:=Queue.Bottom^.PlayerSquare;

             Inc(Board[BoxSquare.x,BoxSquare.y],BOX); {put the box on the board}

             if     (BoxSquare.x<>LastBoxSquare.x)   or
                    (BoxSquare.y<>LastBoxSquare.y)   or
                    ((Board[PlayerPos.x,PlayerPos.y] and PLAYER_LEGAL_MOVE)=0) then begin
                    {the current set of player's reachable squares isn't valid anymore; recalculate the set}
                    LastBoxSquare:=BoxSquare;
                    PlayerLegalMoves(0,0,Col,Row); {calculate player's reachable squares, i.e., to see which sides of the box the player can reach}
                    end;

             for Direction:=Low(Direction) to High(Direction) do begin
                 if not ReverseMode then begin // forward mode game: push the box away from the square
                    BoxToSquare     .x:=BoxSquare  .x+DIRECTION_XY[Direction,ColAxis];
                    BoxToSquare     .y:=BoxSquare  .y+DIRECTION_XY[Direction,RowAxis];
                    PlayerFromSquare.x:=BoxSquare  .x-DIRECTION_XY[Direction,ColAxis];
                    PlayerFromSquare.y:=BoxSquare  .y-DIRECTION_XY[Direction,RowAxis];
                    PlayerToSquare    :=BoxSquare;
                    IsALegalMove      :=(Visited__[BoxToSquare     .x,BoxToSquare     .y,Direction]<TimeStamp__) and
                                        ((Board   [PlayerFromSquare.x,PlayerFromSquare.y] and PLAYER_LEGAL_MOVE)<>0) and
                                        ((Board   [BoxToSquare     .x,BoxToSquare     .y] and (WALL+InvisibleWallMask+BOX+IllegalSquareMask))=0);

                    end
                 else begin // reverse mode game: pull the box away from the square
                    BoxToSquare     .x:=BoxSquare  .x+DIRECTION_XY[Direction,ColAxis];
                    BoxToSquare     .y:=BoxSquare  .y+DIRECTION_XY[Direction,RowAxis];
                    PlayerFromSquare  :=BoxToSquare;
                    PlayerToSquare  .x:=BoxToSquare.x+DIRECTION_XY[Direction,ColAxis];
                    PlayerToSquare  .y:=BoxToSquare.y+DIRECTION_XY[Direction,RowAxis];
                    IsALegalMove      :=(Visited__[BoxToSquare  .x,BoxToSquare   .y,Direction]<TimeStamp__) and
                                        ((Board[BoxToSquare     .x,BoxToSquare   .y] and (WALL+InvisibleWallMask+BOX+IllegalSquareMask+PLAYER_LEGAL_MOVE))=PLAYER_LEGAL_MOVE) and
                                        ((Board[PlayerToSquare  .x,PlayerToSquare.y] and (WALL+BOX           ))=0);
                    end;

                 if IsALegalMove then begin
                    if TargetStates__[BoxToSquare.x,BoxToSquare.y,Direction] then begin
                       Result:=True; Queue.Top:=Queue.Bottom; // stop the search
                       end
                    else begin
                      Inc(Queue.Top); // put the position on the queue for later expansion
                      Queue.Top^.BoxSquare:=BoxToSquare; Queue.Top^.PlayerSquare:=PlayerToSquare;
                      end;
                    Visited__[BoxToSquare.x,BoxToSquare.y,Direction]:=TimeStamp__;
                    end;
                 end;

             Dec(Board[BoxSquare.x,BoxSquare.y],BOX); {remove the box from the board again}

             Dec(CountDownToNextTimeCheck);
             if (CountDownToNextTimeCheck<=0) then begin {'True': check the time now}
                CountDownToNextTimeCheck:=COUNT_DOWN_TO_NEXT_TIME_CHECK;
                TimeNowMS:=GetTickCount;
                if (TimeNowMS>=PathFindingTimeInterval.StopTimeMS)
                   or
                   (TimeNowMS<PathFindingTimeInterval.StartTimeMS) then
                   if Queue.Bottom<>Queue.Top then begin {'True': the search is not over yet; i.e., it hasn't succeeded and it hasn't examined all legal moves yet}
                      Queue.Top:=Queue.Bottom; // stop the search
                      TimeOut__:=True;
                      end;
                end;
             end;

       with BoxPos[BoxNo__] do Inc(Board[x,y],BOX); {put the box on the board again}
       end;
  end; {ForwardSearch}

begin {BoxesToSquare}
  Result:=0; TimeOut__:=False; VisitedTimeStamp:=0;
  if SimpleIllegalMovesMask<>0 then begin
     IllegalSquareMask:=BOX_UNREACHABLE_FLOOR+ILLEGAL_SQUARE;
     InvisibleWallMask:=INVISIBLE_WALL;
     end
  else begin
     IllegalSquareMask:=0; InvisibleWallMask:=0;
     end;
  for Col:=0 to BoardWidth+1 do
      for Row:=0 to BoardHeight+1 do begin
          Board[Col,Row]:=Board[Col,Row] and (not BOX_SET_TO_SQUARE); {clear square set}

          if   (Board[Col,Row] and (WALL+IllegalSquareMask+FLOOR))=FLOOR then
               for Direction:=Low(Direction) to High(Direction) do Visited[Col,Row,Direction]:= 0  {open squares}
          else for Direction:=Low(Direction) to High(Direction) do Visited[Col,Row,Direction]:= High(Visited[Col,Row,Direction]);  {filled squares, e.g., walls}
          end;

  if (Board[Square__.x,Square__.y] and (WALL+InvisibleWallMask+BOX+FLOOR+IllegalSquareMask))=FLOOR then begin
     PlayerLegalMoves(0,0,Col,Row);
     OriginalPlayerPos:=PlayerPos; OriginalBoard:=Board; {save player's position and access area}
     CalculatePathFindingTimeInterval;
     MainForm.Deadlocks.Suspend;
     try     BackwardSearchForTargetStates(Square__,TargetStates,VisitedTimeStamp,Visited,TimeOut__);
             if not TimeOut__ then
                for BoxNo:=1 to BoxCount do
                    if ForwardSearch(BoxNo,Square__,TargetStates,OriginalBoard,VisitedTimeStamp,Visited,TimeOut__) then with BoxPos[BoxNo] do begin
                       OriginalBoard[x,y]:=OriginalBoard[x,y] or BOX_SET_TO_SQUARE;
                       Inc(Result);
                       end;
             //TimeNowMS:=GetTickCount;
             //TimeOut__:=TimeOut__ or
             //           (TimeNowMS>=PathFindingTimeInterval.StopTimeMS) or
             //           (TimeNowMS< PathFindingTimeInterval.StartTimeMS);
             //Msg(IntToStr(CalculateElapsedTimeMS(PathFindingTimeInterval.StartTimeMS,TimeNowMS)),'Time',MB_OK);
     finally PlayerPos:=OriginalPlayerPos; Board:=OriginalBoard;
             MainForm.Deadlocks.Resume;
     end;
     end
  else
     if (Board[Square__.x,Square__.y] and BOX)<>0 then begin {'True': there is already a box on the square}
        Result:=1; Inc(Board[Square__.x,Square__.y],BOX_SET_TO_SQUARE);
        end;
end; {BoxesToSquare}

procedure TGame.BoxIllegalSquares(Col,Row:Integer; TimeLimitEnabled__:Boolean; var IllegalMovesMask__:Integer);
var i,j,BoxNo:Integer;
begin {sets the 'BOX_ILLEGAL_MOVE' flag for all illegal squares}
  BoxNo:=Board[Col,Row] shr BOARD_FLAG_COUNT;
  if (BoxNo<>0) and
     // (not ReverseMode) and
     (SimpleIllegalMovesMask<>0) then begin
     Dec(Board[Col,Row],BOX); // temporarily remove the box from the board

     if TimeLimitEnabled__ then CalculatePathFindingTimeInterval;

     for i:=1 to BoardWidth do // mark deadlock squares
         for j:=1 to BoardHeight do
             if        (Board[i,j] and (BOX+WALL+ILLEGAL_SQUARE+INVISIBLE_WALL))<>0 then
                       Board[i,j]:=Board[i,j] or       BOX_ILLEGAL_MOVE   // add illegal box position flag
             else if   ((not TimeLimitEnabled__)
                        or
                        (GetTickCount<PathFindingTimeInterval.StopTimeMS)
                       )
                       and
                       (IsAFreezingMove(Col,Row,i,j,PlayerPos.x,PlayerPos.y)
                        or
                        (// (not ReverseMode) and
                         (DeadlockDetection.DeadlockDetectionType>=lmhMedium) and
                         DeadlockDetection.Enabled and
                         Assigned(DeadlockDetection.Deadlocks) and
                         (not DeadlockDetection.Deadlocks.IsALegalMove(BoxNo,i,j,False))
                        )
                       ) then
                       Board[i,j]:=Board[i,j] or       BOX_ILLEGAL_MOVE   // add illegal box position flag
                  else Board[i,j]:=Board[i,j] and (not BOX_ILLEGAL_MOVE); // remove old flag, if any

     Board[Col,Row]:=Board[Col,Row]      and (not BOX_ILLEGAL_MOVE); // remove old flag, if any, from the start position

     Inc(Board[Col,Row],BOX); // put the box back on the board

     IllegalMovesMask__:=BOX_ILLEGAL_MOVE; // set the appropriate flag for filtering the squares
     end
  else
     IllegalMovesMask__:=SimpleIllegalMovesMask; // use the normal mask to filter the squares
end;

function TGame.BoxLegalMoves(Col,Row:Integer; var TimeOut:Boolean):Integer;
  {Returns number of reachable squares, including current box position}
var BoxNo,oSimpleIllegalMovesMask:Integer;
begin {precondition: Board[Col,Row] contains a box}
  Result:=0; oSimpleIllegalMovesMask:=SimpleIllegalMovesMask;
  BoxNo:=Board[Col,Row] shr BOARD_FLAG_COUNT;
  if BoxNo<>0 then
     try     BoxIllegalSquares(Col,Row,False,SimpleIllegalMovesMask);
             Result:=Inherited BoxLegalMoves(Col,Row,TimeOut);
     finally SimpleIllegalMovesMask:=oSimpleIllegalMovesMask; // restore filter mask
     end;
end;

function TGame.BoxPath(StartPos__,EndPos__,PlayerPos__:TColRow;
                       var MoveCount__:Integer; var Moves__:TMoves):Boolean;
var BoxNo,oSimpleIllegalMovesMask:Integer;
begin {precondition: Board[StartPos__] contains a box}
  Result:=False; oSimpleIllegalMovesMask:=SimpleIllegalMovesMask;
  BoxNo:=Board[StartPos__.x,StartPos__.y] shr BOARD_FLAG_COUNT;
  if BoxNo<>0 then
     try     BoxIllegalSquares(StartPos__.x,StartPos__.y,False,SimpleIllegalMovesMask);
             Result:=Inherited BoxPath(StartPos__,EndPos__,PlayerPos__,MoveCount__,Moves__);
     finally SimpleIllegalMovesMask:=oSimpleIllegalMovesMask; // restore filter mask
     end;
end;

procedure TGame.Clear;
var i:Integer;
begin
  fFileName:=''; PlayerCanReachTheBorder:=False; BrowseChoicePoints:=False; fMaxBrowsePosition:=0;
  if Assigned(DeadlockDetection.Deadlocks) then DeadlockDetection.Deadlocks.Clear;
  if Assigned(MainForm) and (Self=MainForm.Game) and Assigned(SnapshotsForm) then with SnapshotsForm do
     for i:=0 to Pred(Grid.RowCount) do Snapshots[i]:=nil;
  Inherited;
end;

function TGame.CloseLevel(Flush__:Boolean):Boolean;
var OldSokoFileName,SectionName:String;
begin
  OldSokoFileName:=SokoFileName;

  Result:=Inherited CloseLevel(Flush__);

  if (SokoFileName<>'') and (FileName<>'') and
     (not StrEqual(OldSokoFileName,SokoFileName)) then begin {'True': the file name changed; typically this means the file was saved successfully, but under a new name}
     SectionName:=ExtractSectionName(FileName);
     if   SectionName<>'' then {'True': the level is a member of a collection}
          FileName:=MakeIniFileSectionFileName(SokoFileName,SectionName)
     else FileName:=SokoFileName;
     LastValidFileName:=FileName;
     end;
end;

function TGame.CollapseMoves(Forwards__,UsePlayerImageForMoveAnimationAlsoForGoalSquares__,UseBoxImageForMoveAnimationAlsoForGoalSquares__:Boolean;
                             var Count__:Integer; var CollapsedMoves__:TCollapsedMoves):Boolean;
  // postcondition: the move flags for multi-moves and undone/done moves have been destroyed

  // without collapsing moves before replay, there is a tiny but noticable pause
  // after each move; with moves collapsed, the pause only occurs:
  // 1. when the player changes direction, or
  // 2. when the player changes state (on target/not on target), or
  // 3. when a box changes state (on target/not on target);
  // the pause is less noticable in these situations
var Start,Stop,BoxNo,NewBoxNo,BoxTargetMask,PlayerTargetMask,
    PlayerLineStart, // 'PlayerLineStart' is a bit of a misnomer; it tracks the most recent sequence of player moves heading in the same direction, without box changes, and without changing from pushing a box to moving the player without pushing a box
    oCount,oLastBoxNo,oLastPushIndex,oPlayerLinesCount,oPushCount,oTop:Integer;
    NewBoxToPos,NewPlayerToPos:TColRow; Direction:TDirection; oBoard:TBoard;

  procedure StartMove(NewBoxNo:Integer; NewBoxToPos,NewPlayerToPos:TColRow);
  begin
    if  (BoxNo<>NewBoxNo) or
        (Direction<>TDirection(History.Moves[History.Count] and H_MASK_DIRECTION)) then
        // this is a change of direction, or a change of box, or a change from pushing a box to moving the player without pushing a box
        PlayerLineStart:=History.Count;
    Start:=History.Count; Stop:=Start; BoxNo:=NewBoxNo;
    Direction:=TDirection(History.Moves[Start] and H_MASK_DIRECTION);
    if   BoxNo<>0 then
         BoxTargetMask:=Board[NewBoxToPos   .x,NewBoxToPos   .y] and BoxTargetMaskForDisplay;
    PlayerTargetMask  :=Board[NewPlayerToPos.x,NewPlayerToPos.y] and BoxTargetMaskForDisplay;
  end;

  procedure MakeMove(Start,Stop,BoxNo:Integer);
  var Index:Integer;
  begin
    if Start<>0 then begin
       if   ForkLiftDrivingEnabled and
            (BoxNo=0) and
            (Direction=OPPOSITE_DIRECTION[PlayerDirection(Pred(PlayerLineStart))]) then
            if (Succ(Stop)-PlayerLineStart<=ForkliftDrivingDriveInReverseSquares) // 'Succ': the same-direction moves included all moves in the sequence ['Start' .. 'Stop']
               and
               ((Start<>1)
                or
                (Self<>MainForm.Game)
                or
                (Ord(Direction)<>MainForm.GameViewer.PlayerFrameIndex) // after 'undo' back to the starting position, the player may look in the same direction as the first move
               ) then begin
               BoxNo:=-1; // '-1': drive in reverse instead of making a U-turn
               end
            else begin
               // the number of same-direction moves exceed the 'drive in
               // reverse' limit;
               // if there are any preceding same-direction moves, which have
               // been changed to 'drive in reverse' mode, then change them
               // back so they to show the normal direction;
               Index:=Count__;
               while (Index>0) and
                     (CollapsedMoves__[Index].BoxNo<0) do begin
                     CollapsedMoves__ [Index].BoxNo:=0;
                     Dec(Index);
                     end;
               end;

       Inc(Count__);
       CollapsedMoves__[Count__           ].Start    :=Start;
       CollapsedMoves__[Count__           ].BoxNo    :=BoxNo;
       CollapsedMoves__[Count__           ].Direction:=Direction;

       if   Forwards__ then
            CollapsedMoves__[Succ(Count__)].Start    :=Succ(Stop)
       else CollapsedMoves__[Succ(Count__)].Start    :=Pred(Stop);
       end;
  end;

  function IsANewCollapsedMove(var NewBoxNo:Integer; var NewBoxToPos,NewPlayerToPos:TColRow):Boolean;
  var Move:Integer; PlayerFromPos,BoxFromPos:TColRow;
  begin
    with History do begin
      Move:=Moves[Count];
      UnpackLastMove(PlayerFromPos,NewPlayerToPos,BoxFromPos,NewBoxToPos,NewBoxNo);
      Result:=(Start=0)
              or
              (not((Direction             =TDirection(Move and H_MASK_DIRECTION)) and
                   ((Move and H_FLAG_JUMP)=(Moves[Start]   and H_FLAG_JUMP     )) and
                   ((Move and H_FLAG_BOX )=(Moves[Start]   and H_FLAG_BOX      )) and
                   ((BoxNo=0)
                    or
                    ((Board[NewBoxToPos  .x,NewBoxToPos   .y] and BoxTargetMaskForDisplay)=BoxTargetMask)
                    or
                    UseBoxImageForMoveAnimationAlsoForGoalSquares__
                   )
                   and
                   (((Board[NewPlayerToPos.x,NewPlayerToPos.y] and BoxTargetMaskForDisplay)=PlayerTargetMask)
                    or
                    UsePlayerImageForMoveAnimationAlsoForGoalSquares__
                   )
                  ));
      end;
  end;

begin // CollapseMoves
  Result:=True;
  with History do begin
    oBoard:=Board;
    oCount:=Count; oLastBoxNo:=LastBoxNo; oLastPushIndex:=LastPushIndex; oPlayerLinesCount:=PlayerLinesCount; oPushCount:=PushCount; oTop:=Top;
    try Count__:=0; Start:=0; Stop:=0; BoxNo:=-1; BoxTargetMask:=0; PlayerTargetMask:=0; Direction:=Up; PlayerLineStart:=1;
        if Forwards__ then begin
           while  (Count<Top) and (Count__<High(CollapsedMoves__)-2) do
             if   Redo0(False) then begin
                  if   IsANewCollapsedMove(NewBoxNo,NewBoxToPos,NewPlayerToPos) then begin // break up the move here
                       MakeMove(Start,Stop,BoxNo);
                       StartMove(NewBoxNo,NewBoxToPos,NewPlayerToPos);
                       end
                  else Stop:=Count; // same direction and same target state for box-pushes, or the collapsed moves vector is full
                  end
             else begin Count:=Top; Result:=False;
                  end;
           end
        else begin // backwards
           while  (Count>ForcedInitialJumps) and (Count__<High(CollapsedMoves__)-2) do begin
             if   IsANewCollapsedMove(NewBoxNo,NewBoxToPos,NewPlayerToPos) then begin // break up the move here
                  MakeMove(Start,Stop,BoxNo);
                  StartMove(NewBoxNo,NewBoxToPos,NewPlayerToPos);
                  end
             else Stop:=Count; // same direction and same target state for box-pushes, or the collapsed moves vector is full
             if   not Inherited Undo(False) then begin
                  Count:=0; Result:=False;
                  end;
             end;
           end;
        if Start<>0 then MakeMove(Start,Stop,BoxNo);
    finally // restore the history control info; note that that this only is a
            // partial restoration; the moves aren't copied back, both for speed
            // and conveniency; however, if more fields are added to 'THistory'
            // then the restoration here must also be altered
            Count:=oCount; LastBoxNo:=oLastBoxNo; LastPushIndex:=oLastPushIndex; PlayerLinesCount:=oPlayerLinesCount; PushCount:=oPushCount; Top:=oTop;
            LoadBoard(oBoard); // restore the board
    end;
    end;
end;

function TGame.IsALegalMove(dx__,dy__:Integer; Flags__:Integer; var IsAFreezingMove__:Boolean):Boolean;
var x,y,BoxNo:Integer; s:String;
begin {Note: [dx__,dy__] must be a member of 'DIRECTION_XY'}
  Result:=Inherited IsALegalMove(dx__,dy__,Flags__,IsAFreezingMove__);
  if Result then begin
     if ((not ReverseMode)
         or
         ( ((Flags__ and H_FLAG_JUMP)=0)
           and
           ((Flags__ and (H_FLAG_BOX+MOVE_FLAG_KEYBOARD))<>MOVE_FLAG_KEYBOARD)
         )
        )
        and
        (SimpleIllegalMovesMask<>0)
        and
        (DeadlockDetection.DeadlockDetectionType>lmhLow)
        and
        Assigned(DeadlockDetection.Deadlocks) then begin
        if not ReverseMode then begin
           x :=PlayerPos.x+dx__;
           y :=PlayerPos.y+dy__;
           end
        else begin
           x :=PlayerPos.x-dx__;
           y :=PlayerPos.y-dy__;
           end;
        BoxNo:=Board[x,y] shr BOARD_FLAG_COUNT;
        if BoxNo<>0 then
           Result:=DeadlockDetection.Deadlocks.IsALegalMove(BoxNo,x+dx__,y+dy__,False);
        if (not Result) and
           ((Flags__ and PLAYER_TRY_MOVE)<>0) then begin
           s:=MoveLeadingToADeadlockPositionText;
           if (DeadlockDetection.Deadlocks.OverflowingDeadlockSet<>0) and
              (DeadlockDetection.Deadlocks.LoggedCount>=DeadlockDetection.Deadlocks.OverflowingDeadlockSet) and
              DeadlockDetection.LogEnabled then
              s:=s+SPACE+LEFT_PAREN+IntToStr(DeadlockDetection.Deadlocks.OverflowingDeadlockSet)+RIGHT_PAREN;
           MainForm.Status.Hint:=s;
           end;
        end;
     end
  else
     if ((Flags__ and PLAYER_TRY_MOVE)<>0) and
        IsAFreezingMove__ and // freezing deadlocks may not be obvious to the human player; show the explanation on the screen
        (SimpleIllegalMovesMask<>0) then
        MainForm.Status.Hint:=MoveLeadingToADeadlockPositionText;
end;

function SubstitutePlayerPath(var History__:THistory; OldPlayerPos__:TColRow;
                              AfterIndex__,NewMoveCount__:Integer; NewMoves__:PPlayerMoves):Boolean;
// precondition: the new path is shorter than the existing path
var i,dx,dy:Integer; Direction:TDirection;
begin
  Result:=True; NewMoves__[0]:=OldPlayerPos__;
  for i:=1 to NewMoveCount__ do // overwrite the existing moves in the history with the new optimized path
      if Result then with NewMoves__[i] do begin
         dx:=(x                     and (not MOVE_LIST_FLAG_BOX ))-
             (NewMoves__[Pred(i)].x and (not MOVE_LIST_FLAG_BOX ));
         dy:=(y                     and (not MOVE_LIST_FLAG_JUMP))-
             (NewMoves__[Pred(i)].y and (not MOVE_LIST_FLAG_JUMP));
         if DxDyToDirection(dx,dy,Direction) then
            History__.Moves[AfterIndex__+i]:=Ord(Direction)
         else begin // this shouldn't happen
            Result:=False;
            History__.Count:=AfterIndex__+Pred(i);
            History__.Top  :=History__.Count;
            end;
         end;
  if Result then begin
     for i:=Succ(History__.Count) to History__.Top do // slide any old undone moves downwards in the history
         History__.Moves [AfterIndex__+NewMoveCount__+i-History__.Count]:=History__.Moves[i];
     History__.Top  :=AfterIndex__+NewMoveCount__+  History__.Top-History__.Count; // adjust top of the history
     History__.Count:=AfterIndex__+NewMoveCount__; // adjust current history position
     end;
  CalculatePlayerLines(History__);
  MainForm.ShowStatus;
end;

function  TGame.TestForNewBestSolution:Boolean;
begin
  Result:=Inherited TestForNewBestSolution;
  if Result and (Self=MainForm.Game) and (not IsLoading) and (SnapshotsForm<>nil) then with SnapshotsForm do
     LoadSnapshots(Snapshots[Grid.Row]);
end;

function  TGame.TryMove(Direction:TDirection; LastMoveIndex,Flags:Integer):Boolean;
var dx,dy,BoxNo,oCount,oPushCount:Integer; IsAFreezingMove,oIsBusy,oIsReplaying,oIsBrowsing:Boolean;

  function IsASimpleBackMove(Direction:TDirection; Flags:Integer):Boolean;
  begin
    with History do
      Result:=(Count>ForcedInitialJumps) and
              ((Flags        and H_FLAG_BOX)=0) and
              ((Moves[Count] and H_FLAG_BOX)=0) and
              (Direction=OPPOSITE_DIRECTION[TDirection(Moves[Count] and H_MASK_DIRECTION)]);
  end;

  function WasABackMove:Boolean;
  var i,Move,LastMoveIndex,NewMoveCount,NewPlayerLinesCount,PlayerMoveCount:Integer; IsPreferredDirectionOK:Boolean; Position:TColRow;
  begin // precondition: the global variable 'MovesOrHistory' is freely available;
    // note that for efficiency, the function only checks for improvements in
    // the number of moves, not for moves/player-lines improvements;
    // the rationale is that since this function only is used for optimizing
    // moves entered via the keyboard, it's probably a time-critical function,
    // and calculating a full new path after every single player move might
    // cause noticeable and unwanted delays;
    Result:=False;
    with History do
      if   (Count>ForcedInitialJumps) and
           ((Moves[Count] and (H_FLAG_BOX{+H_FLAG_JUMP}))=0) and
           (History.Count>=LastPushIndex+3)
           then begin
           Position:=PlayerPos; PlayerMoveCount:=0;
           for i:=Count downto 1 do begin
               Move:=Moves[i];
               if   (Move and (H_FLAG_BOX{+H_FLAG_JUMP}))=0 then begin // 'True': this a simple player-move, not a push or a jump
                    Dec(Position.x,DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),ColAxis]);
                    Dec(Position.y,DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),RowAxis]);
                    if   ReverseMode and ((Board[Position.x,Position.y] and (WALL+BOX))<>0) then begin
                         Inc(Position.x,DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),ColAxis]);
                         Inc(Position.y,DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),RowAxis]);
                         break;
                         end
                    else Inc(PlayerMoveCount);
                    end
               else break;
               end;
           if  PlayerMoveCount<>0 then with PlayerPathBoardTimestamps do begin
               LastMoveIndex:=Count-PlayerMoveCount;
               if PlayersReachableSquaresTimestampAfterLastPush<>Timestamp then
                  PlayersReachableSquaresTimestampAfterLastPush:=CalculatePlayersDistanceToAllReachableSquares(Position);
               if (Squares[PlayerPos.X,PlayerPos.Y]>=Timestamp) and
                  (((Squares[PlayerPos.X,PlayerPos.Y] -Timestamp) div TIMESTAMP_INCREMENT)<Cardinal(PlayerMoveCount)) and // distance to the square is stored in the timestamp table as (squarevalue - timestamp) div TIMESTAMP_INCREMENT
                  PlayerPath(Position,PlayerPos,False,LastMoveIndex>0,TDirection(History.Moves[LastMoveIndex] and H_MASK_DIRECTION),NewMoveCount,NewPlayerLinesCount,IsPreferredDirectionOK,PPlayerMoves(Addr(MovesOrHistory.Moves))) and
                  (NewMoveCount<=PlayerMoveCount) then begin // '<=': the new path may be better optimized on moves/lines than the old path;
                  Result:=SubstitutePlayerPath(History,Position,LastMoveIndex,NewMoveCount,PPlayerMoves(Addr(MovesOrHistory)));
                  MakeCombinedMove(Succ(LastMoveIndex),History.Count);
                  SeparateUndoneMoves;
                  end;
               end;
           end;
  end; // WasABackMove

begin // TryMove
  dx:=DIRECTION_XY[Direction,ColAxis]; dy:=DIRECTION_XY[Direction,RowAxis];

  Result:=History.Count<MAX_MOVES;
  if Result then begin
     Result:=((Flags and PLAYER_LEGAL_MOVE)<>0) or IsALegalMove(dx,dy,Flags or PLAYER_TRY_MOVE,IsAFreezingMove); // 'PLAYER_LEGAL_MOVE': the move has already been validated
     if (not Result)
        and
        ReverseMode                                  // note: we should only get here when 'TryMove' is called from 'MainForm', trying to perform a single move
        and
        (((Flags and (H_FLAG_BOX+H_FLAG_JUMP))=0)    // '0': [Ctrl]+Arrow-key pressed:
         or
         ((Board[PlayerPos.x,PlayerPos.y] and (BOX+WALL))<>0)
        )
        and
        IsALegalMove(dx,dy,H_FLAG_JUMP,IsAFreezingMove) then begin   // try to make a jump
        Result:=True; Flags:=H_FLAG_JUMP;            // ok: convert the move to a jump
        end;
     end;

  if Result then begin
     if Self=MainForm.Game then with MainForm do begin
        if MPlayer.Visible then MPlayer.Hide;
        if (GameViewer<>nil) and
           ((GameViewer.LegalMovesInfo.Mask<>0)
            or
            ((GameViewer.LegalMovesInfo.ItemIndex>0)
             and
             (GameViewer.LegalMovesInfo.BoxCursor.Enabled
              or
              GameViewer.LegalMovesInfo.PlayerCursor.Enabled
             )
            )
           ) then
           GameViewer.HideLegalMoves;
        if TrackState=tsTrack then TrackState:=tsWait;
        TrackBox.X:=0; TrackBox.Y:=0; MainForm.UpdateCursor;
        end;

     if not ReverseMode then // normal mode game
        BoxNo:=Board[PlayerPos.x+dx,PlayerPos.y+dy] shr BOARD_FLAG_COUNT
     else begin // reverse mode game
        if   ((Flags and (H_FLAG_BOX+H_FLAG_JUMP))=H_FLAG_BOX) and // reverse mode: only treat the move as a box-move if the flag is set
             ((Board[PlayerPos.x,PlayerPos.y] and (SimpleIllegalMovesMask+WALL+BOX))=0) then
             BoxNo:=Board[PlayerPos.x-dx,PlayerPos.y-dy] shr BOARD_FLAG_COUNT
        else BoxNo:=0;

        if   (BoxNo=0) and
             ((Flags and H_FLAG_JUMP)=0) and
             (// convert non-pulling move to a jump if previous move was a jump
              ((History.Count>ForcedInitialJumps) and
               ((History.Moves[History.Count] and H_FLAG_JUMP)<>0))
              or
              // convert an initial move away from a box to a jump
              ((History.Count<=ForcedInitialJumps) and
               ((Board[PlayerPos.x,PlayerPos.y] and (WALL+BOX))<>0))
             ) then begin
             Inc(Flags,H_FLAG_JUMP);
             end;
        end;
     if BoxNo=0 then Flags:=Flags and (not (H_FLAG_BOX+PLAYER_LEGAL_MOVE)); // drop box-flag and legal-player-move-flag if the move doesn't involve a box

     if (BoxNo=0) and MainForm.OptimizeMovesBetweenPushes and IsASimpleBackMove(Direction,Flags) then begin
        oIsBusy:=IsBusy; oIsReplaying:=IsReplaying; oIsBrowsing:=IsBrowsing;
        try     IsBusy:=False; IsReplaying:=False; IsBrowsing:=False;
                MainForm.BtnUndoClick(nil);
        finally IsBusy:=oIsBusy; IsReplaying:=oIsReplaying; IsBrowsing:=oIsBrowsing;
        end;
        end
     else begin
        if ShowMovesEnabled then begin
           if MoveAnimationEnabled then begin
              if ((Flags and MOVE_FLAG_FORKLIFT_DRIVING)<>0) and (BoxNo=0) then
                 BoxNo:=-1; // '-1': drive in reverse instead of making a u-turn
              oCount:=History.Count; oPushCount:=History.PushCount;
              DoMoveUpdateGame(dx,dy,LastMoveIndex,Flags); // update before 'ShowMove'
              if MainForm.Image1.Visible then begin
                 MainForm.ShowMove(Point(PlayerPos.x-dx,PlayerPos.y-dy),
                                   Point(PlayerPos.x   ,PlayerPos.y   ),
                                   BoxNo,AnimateDoMoveMS,
                                   oCount,oPushCount,
                                   False,(Flags and H_FLAG_JUMP)<>0,True);
                 end;
              end
           else begin
              DoMoveUpdateGame(dx,dy,LastMoveIndex,Flags); // update before 'ShowMoveInstantly'
              if MainForm.Image1.Visible then
                 MainForm.ShowMoveInstantly(BoxNo,False,(Flags and H_FLAG_JUMP)<>0,True);
              end;

           if (GameState=gsSolved) and MainForm.Sound.Enabled then
              MainForm.Sound.Play(stSolution);

           if MainForm.DrawGrid1.Visible then
              MainForm.ShowMove(Point(PlayerPos.x,PlayerPos.y),
                                Point(PlayerPos.x+dx,PlayerPos.y+dy),
                                BoxNo,
                                AnimateDoMoveMS,
                                History.Count,History.PushCount,
                                False,
                                (Flags and H_FLAG_JUMP)<>0,
                                True);

           MainForm.ShowStatus;

           if IsALegalPosition then;

           end
        else DoMoveUpdateGame(dx,dy,LastMoveIndex,Flags); // update only

        if ((Flags and MOVE_FLAG_KEYBOARD)<>0) and MainForm.OptimizeMovesBetweenPushes then
           // this is a single move entered via the keyboard, i.e., it's not
           // a move delegated to 'TryMove' from 'TryMoves' which handles
           // optimizations by itself
           WasABackMove;
        end;
     end
  else begin
    if (GameState=gsPlay) and
       (MoveCountSinceLastKeyUpEvent=0) and
       {keyboard users will often walk into a wall, hence,
        don't emit a sound when auto-repeat causes several
        moves in one go;
        it becomes annoying after a few times}
       ((Board[PlayerPos.x+dx,PlayerPos.y+dy] and WALL)=0) then
       with MainForm.Sound do
         if Enabled and (Player[stBlockedStep]<>nil) then
            Play(stBlockedStep);
    if History.Count>=MAX_MOVES then
       Msg(MaxMovesExceededText,Application.Title,MB_OK+MB_ICONINFORMATION);
    end;

  if (GameState=gsSolved) and (Self=MainForm.Game) then begin
     MainForm.MakeSolutionButtonHint;
     MainForm.CheckAutoAdvanceWhenSolved;
     end;
end;

procedure TGame.TryMoves(MoveCount:Integer; var Moves:TMoves);
var i,oCount,oPushCount,LastMoveIndex,BoxNo:Integer;
    oShowMovesEnabled,oSoundEnabled,oUndoRedoCombinedMoves:Boolean; p1,p2,p3,p4:TColRow;

  function  DoMoves(MoveCount:Integer; var Moves:TMoves; var LastMoveIndex:Integer; SoundEnabled:Boolean; NextMove:TColRow):Boolean;
  var i,j,Rest:Integer;

    function  TryMove(Move:TColRow; MoveIndex:Integer; var LastMoveIndex:Integer; SoundEnabled:Boolean; NextMove:TColRow; Flags:Integer):Boolean;
    var dx,dy,ExtraMoveCount,ExtraPlayerLinesCount:Integer;
        oIsReplaying,oIsBrowsing,BackwardPlayerPath,IsPreferredDirectionOK,IsPreferredDirectionSupplied:Boolean;
        Dir,PreferredDirection:TDirection; ExtraMoves:TMoves;
    begin
      Result:=False;

      // kludge: the x-coordinate and the y-coordinate carry flags for box-moves and jump-moves respectively
      if (Move.x and MOVE_LIST_FLAG_BOX)<>0 then begin
         Inc(Flags,H_FLAG_BOX );  Dec(Move.x,MOVE_LIST_FLAG_BOX);
         end;
      if (Move.y and MOVE_LIST_FLAG_JUMP)<>0 then begin
         Inc(Flags,H_FLAG_JUMP); Dec(Move.y,MOVE_LIST_FLAG_JUMP);
         end;

      dx:=Move.x-PlayerPos.x;
      dy:=Move.y-PlayerPos.y;
      oIsReplaying:=IsReplaying; oIsBrowsing:=IsBrowsing;
      if   Abs(dx)+Abs(dy)=1 then // neighbor squares
           if    DxDyToDirection(dx,dy,Dir) and (GameState=gsPlay) then
                 try     IsReplaying:=True; // kludge: protect against resizing the form while the move is performed
                         IsBusy:=False;     // kludge: 'False', so 'IsIdleAndStopReplayingAndBrowsing' succeeds if 'Self.TryMove' converts a move to an undo-action
                         IsBrowsing:=False;
                         UserBreakWhileReplaying:=False;
                         MainForm.Sound.Enabled:=SoundEnabled;
                         if (Flags and (H_FLAG_BOX+H_FLAG_JUMP))=0 then
                            Inc(Flags,PLAYER_LEGAL_MOVE); // the player-move is pseudo-legal, i.e., the player can perform the move unless the history is full
                         Result:=Self.TryMove(Dir,LastMoveIndex,Flags) and
                                 (not UserBreakWhileReplaying);
                 finally IsBusy:=True; IsReplaying:=oIsReplaying; IsBrowsing:=oIsBrowsing;
                         LastMoveIndex:=Min(LastMoveIndex,History.Count);
                         if Result and MoveAnimationEnabled and (DelayMoveMS<>0) then
                            SleepEx(DelayMoveMS,False);
                 end
           else  begin
                 end
      else begin if      (MoveIndex<MoveCount) and
                         DxDyToDirection((Moves[Succ(MoveIndex)].x and (not MOVE_LIST_FLAG_BOX ))-Move.x,
                                         (Moves[Succ(MoveIndex)].y and (not MOVE_LIST_FLAG_JUMP))-Move.y,
                                         PreferredDirection) then begin // take the direction of the next move (normally a push) into account, so the intermediate non-pushing player moves can be optimized on moves/lines instead of moves only
                         IsPreferredDirectionSupplied:=True;
                         BackwardPlayerPath:=True;
                         NextMove:=Moves[Succ(MoveIndex)];
                         end
                 else    if (NextMove.x<>0) and
                            DxDyToDirection((NextMove.x and (not MOVE_LIST_FLAG_BOX ))-Move.x,
                                            (NextMove.y and (not MOVE_LIST_FLAG_JUMP))-Move.y,
                                            PreferredDirection) then begin // take the direction of the next move (normally a push) into account, so the intermediate non-pushing player moves can be optimized on moves/lines instead of moves only
                            IsPreferredDirectionSupplied:=True;
                            BackwardPlayerPath:=True;
                            end
                         else begin
                            PreferredDirection:=TDirection(History.Moves[History.Count] and H_MASK_DIRECTION);
                            IsPreferredDirectionSupplied:=History.Count>0;
                            BackwardPlayerPath:=False;
                            end;
                 if      PlayerPath(PlayerPos,Move,BackwardPlayerPath,IsPreferredDirectionSupplied,PreferredDirection,ExtraMoveCount,ExtraPlayerLinesCount,IsPreferredDirectionOK,PPlayerMoves(Addr(ExtraMoves))) then
                         Result:=DoMoves(ExtraMoveCount,ExtraMoves,LastMoveIndex,SoundEnabled,NextMove);
           end;
    end; // TryMove

    function ConvertMovesToUndoMoves(var Rest,LastMoveIndex:Integer):Boolean;
    var i,j,dx,dy,PriorDX,PriorDY,Move,NewMoveCount,NewPlayerLineCount,oCount,
        PlayerMoveCount,PlayerLineCount,BackTrackIndex:Integer;
        oShowMovesEnabled,BackwardPlayerPath,IsPreferredDirectionOK,IsPreferredDirectionSupplied:Boolean;
        PreferredDirection:TDirection;
        Position,oPlayerPos:TColRow; B:TBoard; NewMoves:TMoves;

      function UndoLastMove(var LastMoveIndex:Integer):Boolean;
      var oIsBusy,oIsReplaying,oIsBrowsing:Boolean;
      begin
        oIsBusy:=False; oIsReplaying:=IsReplaying; oIsBrowsing:=IsBrowsing;
        try     // kludge: set 'IsBusy' to 'False': otherwise 'Undo' won't accept the task;
                // it looks dangerous, and it may not be 100% safe, but it will have to do
                IsBusy:=False;  IsReplaying:=False; IsBrowsing:=False;
                Result:=Undo(True);
        finally IsBusy:=oIsBusy; IsReplaying:=oIsReplaying; IsBrowsing:=oIsBrowsing;
                LastMoveIndex:=Min(LastMoveIndex,History.Count);
        end;
      end; // UndoLastMove

    begin // ConvertMovesToUndoMoves; take back history-moves that can be discarded, given the new moves in 'Moves'
      Result:=True; Rest:=1;

      if MainForm.OptimizeMovesBetweenPushes and
         (oPushCount=History.PushCount) then begin // 'oPushCount': if a box has been moved since 'TryMoves' was called, then the remaining moves all stem from one of the automatical path-finders, hence, they are already optimal
         oShowMovesEnabled:=ShowMovesEnabled;

         if (MoveCount>0) and
            (History.Count>ForcedInitialJumps) and
            ((History.Moves[History.Count] and H_FLAG_JUMP)<>0) and  // previous move was a jump
            ((Moves[1].x and MOVE_LIST_FLAG_BOX)=0) then             // new move isn't a push/pull
            try
              ShowMovesEnabled:=False; oPlayerPos:=PlayerPos;

              Result:=UndoLastMove(LastMoveIndex); // undo last jump

              while (Rest<=MoveCount) and ((Moves[Rest].y and MOVE_LIST_FLAG_JUMP)<>0) do
                Inc(Rest); // find jump-moves, if any

              if Rest=1 then // no jumps: find non-pushing/pulling moves, if any
                 while (Rest<=MoveCount) and
                       ((Moves[Rest].x and MOVE_LIST_FLAG_BOX )=0) and
                       ((Moves[Rest].y and MOVE_LIST_FLAG_JUMP)=0) do
                   Inc(Rest); // non-pushing/pulling moves are converted to jumps and 'added' to previous jump

              if Result and (Rest>1) then begin // perform a new jump
                 Position.x:=Moves[Pred(Rest)].x and (not MOVE_LIST_FLAG_BOX );
                 Position.y:=Moves[Pred(Rest)].y and (not MOVE_LIST_FLAG_JUMP);

                 while Result and // undo previous non-box-moves
                       (History.Count>ForcedInitialJumps) and
                       ((History.Moves[History.Count] and H_FLAG_BOX)=0) do
                       Result:=UndoLastMove(LastMoveIndex);

                 Result:=(PlayerPath(PlayerPos,Position,False,History.Count>0,TDirection(History.Moves[History.Count] and H_MASK_DIRECTION),NewMoveCount,NewPlayerLineCount,IsPreferredDirectionOK,PPlayerMoves(Addr(NewMoves))) // first try to substitute the jump with normal moves
                          or
                          PlayerJump(PlayerPos,Position,True,NewMoveCount,NewPlayerLineCount,NewMoves)
                         )
                         and
                         DoMoves    (NewMoveCount,NewMoves,LastMoveIndex,SoundEnabled,NextMove);

                 if oShowMovesEnabled then // note that 'Result' isn't part of this set of conditions:
                    if PlayerJump(oPlayerPos,PlayerPos,True,NewMoveCount,NewPlayerLineCount,NewMoves) and
                       (NewMoveCount>0) then begin // display a faked move, only moving the player from old visible position to new position
                       if MainForm.Image1.Visible then
                          if   MoveAnimationEnabled then begin
                               NewMoves[0]:=oPlayerPos;
                               if LastMoveIndex=History.Count then // the new position is a result of taking back moves, no new moves are added
                                  for i:=1 to NewMoveCount do with NewMoves[i] do
                                      MainForm.ShowMove(// 'FromPos' := destination because the move is shown as an undo-move
                                                        Point(NewMoves[i      ].x and (not MOVE_LIST_FLAG_BOX),NewMoves[i      ].y and (not MOVE_LIST_FLAG_JUMP)),
                                                        Point(NewMoves[Pred(i)].x and (not MOVE_LIST_FLAG_BOX),NewMoves[Pred(i)].y and (not MOVE_LIST_FLAG_JUMP)),
                                                        0,
                                                        AnimateDoMoveMS,
                                                        Succ(History.Count+NewMoveCount-i),
                                                        History.PushCount,
                                                        True, // undo move
                                                        False,
                                                        (i=NewMoveCount) and SoundEnabled)
                               else
                                  for i:=1 to NewMoveCount do with NewMoves[i] do
                                      MainForm.ShowMove(Point(NewMoves[Pred(i)].x and (not MOVE_LIST_FLAG_BOX),NewMoves[Pred(i)].y and (not MOVE_LIST_FLAG_JUMP)),
                                                        Point(NewMoves[i      ].x and (not MOVE_LIST_FLAG_BOX),NewMoves[i      ].y and (not MOVE_LIST_FLAG_JUMP)),
                                                        0,
                                                        AnimateDoMoveMS,
                                                        Pred(Max(LastMoveIndex,History.Count-NewMoveCount+i)),
                                                        History.PushCount,
                                                        False, // do move
                                                        (History.Moves[History.Count] and H_FLAG_JUMP)<>0,
                                                        (i=NewMoveCount) and SoundEnabled);
                               end
                          else MainForm.ShowMoveInstantly(0,LastMoveIndex=History.Count,(LastMoveIndex<History.Count) and ((History.Moves[History.Count] and H_FLAG_JUMP)<>0),SoundEnabled)
                       else if @ShowGame<>nil then ShowGame
                            else;
                       end
                    else
                       // 'PlayerJump' doesn't succeed if the player starts from a wall square;
                       // that can only happen when the user switches from keyboard control (where
                       // moves over wall-squares are allowed for convenience) to mouse control
                       // where moves over wall-squares does not occur;
                       // it would require too much new machinery to calculate a correct
                       // path in this rare situation, so it seems reasonable to settle for
                       // a simple solution: simply move the player without further ado
                       MainForm.ShowMoveInstantly(0,LastMoveIndex=History.Count,(LastMoveIndex<History.Count) and ((History.Moves[History.Count] and H_FLAG_JUMP)<>0),SoundEnabled);
                 end;
            finally
              ShowMovesEnabled:=oShowMovesEnabled;
              if ShowMovesEnabled then MainForm.ShowStatus;
            end;

         if Rest<=MoveCount then with Moves[Rest] do
            if //((x and MOVE_LIST_FLAG_BOX )=0) and // 'True': the next move is a non-pushing player move
               //((y and MOVE_LIST_FLAG_JUMP)=0) and // 'True': the next player-move isn't a jump move
               (Abs((x and (not MOVE_LIST_FLAG_BOX))-PlayerPos.x)+Abs((y and (not MOVE_LIST_FLAG_JUMP))-PlayerPos.y)=1) then begin // 'True': the next player-move is a single-square move as opposed to a combined move

               with BoardTimestamps do
                 if Timestamp<High(Timestamp)-2 then Inc(Timestamp,2) // there is no particular reason to increase the timestamp by 2 instead of 1 here; it's merely a left-over from a sketch where the timestamps always were even numbers
                 else begin
                    FillChar(Squares,SizeOf(Squares),0); Timestamp:=2;
                    end;

               Position:=PlayerPos; PlayerMoveCount:=0; PlayerLineCount:=0;
               for i:=History.Count downto 1 do with History do begin
                   Move:=Moves[i];
                   if   (Move and (H_FLAG_BOX{+H_FLAG_JUMP}))=0 then begin // consider simple player-moves only, neither pushes nor jumps
                        Dec(Position.x,DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),ColAxis]);
                        Dec(Position.y,DIRECTION_XY[TDirection(Move and H_MASK_DIRECTION),RowAxis]);
                        with BoardTimestamps do Squares[Position.x,Position.y]:=Timestamp; // mark that the player can reach the square [x,y]
                        B[Position.x,Position.y]:=i; // the player can reach the square [x,y] by un-doing history-moves back to 'History.Moves[i]'
                        Inc(PlayerMoveCount); // count the number of non-pushing player-moves in the history after the last push, if any
                        if   i>1 then
                             if   TDirection(Moves[i] and H_MASK_DIRECTION)<>TDirection(Moves[Pred(i)] and H_MASK_DIRECTION) then
                                  Inc(PlayerLineCount) {count the number of player lines in the history after the last push, if any}
                             else begin end
                        else Inc(PlayerLineCount) // the first move counts as a player line
                        end
                   else break;
                   end;

               if PlayerMoveCount<>0 then begin // 'True': the latest moves in the history are non-pushing player-moves
                  oCount:=PlayerMoveCount; oPlayerPos:=Position; Position:=PlayerPos; // 'oPlayerPos' is now the player's position after the last push, if any
                  Moves[Pred(Rest)]:=PlayerPos; BackTrackIndex:=0; // note that 'Moves' here is the procedure argument, not 'History.Moves'
                  with History do begin
                    PriorDX:=DIRECTION_XY[TDirection(Moves[Count] and H_MASK_DIRECTION),ColAxis];
                    PriorDY:=DIRECTION_XY[TDirection(Moves[Count] and H_MASK_DIRECTION),RowAxis];
                    end;
                  for i:=Rest to MoveCount do with Moves[i] do begin
                      dx:=(x                and (not MOVE_LIST_FLAG_BOX ))-
                          (Moves[Pred(i)].x and (not MOVE_LIST_FLAG_BOX ));
                      dy:=(y                and (not MOVE_LIST_FLAG_JUMP))-
                          (Moves[Pred(i)].y and (not MOVE_LIST_FLAG_JUMP));
                      if   ((x and MOVE_LIST_FLAG_BOX )=0) and
                           ((y and MOVE_LIST_FLAG_JUMP)=0) and
                           (Abs(dx)+Abs(dy)=1) then with BoardTimestamps do begin
                           Inc(Position.x,dx); Inc(Position.y,dy);
                           Inc(PlayerMoveCount); // count the player moves if the current path is kept
                           if (dx<>PriorDX) or (dy<>PriorDY) then begin
                              Inc(PlayerLineCount); // count the player lines if the current path is kept
                              PriorDX:=dx; PriorDY:=dy; // remember the direction of the (soon to be) prior move
                              end;
                           if (Squares[x,y]=Timestamp) then // 'True': the position after 'Moves[i]' can be reached by un-doing history-moves back to 'History.Moves[B[x,y]]'
                              BackTrackIndex:=i;
                           end
                      else break; // stop on first box-move, first combined move, or first jump-move
                      end;

                  if (BackTrackIndex<>0) and // 'True': the player re-visits a previously visited square
                     False then begin //
                     // player-backtracking, i.e., where the player returns to a
                     // previously visited square, is obsolete now that the
                     // program has the more powerful "optimize player-path"
                     // mechanism; see below in the 'else' fork of this 'if'
                     Rest:=Succ(BackTrackIndex); // later, the caller must handle any remaining moves normally
                     oCount:=History.Count;

                     BackTrackIndex:=B[Moves[BackTrackIndex].x and (not MOVE_LIST_FLAG_BOX),Moves[BackTrackIndex].y and (not MOVE_LIST_FLAG_JUMP)];
                     if   (BackTrackIndex<=ForcedInitialJumps) or (BackTrackIndex>History.Count) then
                          raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,['Game.TryMoves.DoMoves.ConvertMovesToUndoMoves: Program error.']))
                     else for i:=oCount downto BackTrackIndex do
                              if Result then with History do begin
                                 HistoryMoveToDxDy(Moves[i],dx,dy);
                                 Position.x:=PlayerPos.x-dx;
                                 if (Moves[i] and H_FLAG_BOX )<>0 then Inc(Position.x,MOVE_LIST_FLAG_BOX );
                                 Position.y:=PlayerPos.y-dy;
                                 if (Moves[i] and H_FLAG_JUMP)<>0 then Inc(Position.y,MOVE_LIST_FLAG_JUMP);
                                 Result:=Result and TryMove(Position,MoveCount,LastMoveIndex,SoundEnabled and (i=BackTrackIndex) and (Rest>MoveCount),NextMove,0);  //  undo the player move
                                 end;
                     LastMoveIndex:=Min(LastMoveIndex,History.Count);      // decreases if any moves are taken back
                     while (LastMoveIndex>0) and
                           ((History.Moves[LastMoveIndex] and (H_FLAG_BOX+H_FLAG_JUMP))=0) do
                           Dec(LastMoveIndex); // any non-pushing player-moves must be part of the new combined move
                     for i:=Succ(History.Count) to oCount do with History do
                         Moves[i]:=(Moves[i] and (not H_FLAG_ODD))         // join the undone moves to form a single combined move again
                                   or                                      // ('Undo' has separated them)
                                   (H_FLAG_ODD and (not Moves[Count]));
                     SeparateMoves(oCount,History);                        // separate the newly undone moves from earlier undone moves, if any
                     CalculatePlayerLines(History);
                     end
                  else // check if there are any player path optimization possibilities
                     if PlayerMoveCount>=1  then begin  // '>=1'; the minimum number of moves it takes to create a non-optimal path is 3 moves if player lines are not considered, but taking player lines into account reduces the threshold to 1; (incidentally, the low-level move-functions can handle 2-move forward-and-backward moves on their own, but it's shadowed by the new path calculation here))
                        i:=Rest+PlayerMoveCount-oCount; // 'i': the first remaining move on the list if the current player moves are substituted by new ones
                        LastMoveIndex:=History.Count-oCount; // 'LastMoveIndex' is the index of the last box-pushing move or jump-move in the history, if any
                        if   (i<=MoveCount) and
                             DxDyToDirection((Moves[i].x and (not MOVE_LIST_FLAG_BOX ))-Position.x,
                                             (Moves[i].y and (not MOVE_LIST_FLAG_JUMP))-Position.y,
                                             PreferredDirection) then begin // take the direction of the next move (normally a push) into account, so the intermediate non-pushing player moves can be optimized on moves/lines instead of moves only
                             IsPreferredDirectionSupplied:=True;
                             BackwardPlayerPath:=True;

                             if (DIRECTION_XY[PreferredDirection,ColAxis]<>PriorDX) or
                                (DIRECTION_XY[PreferredDirection,RowAxis]<>PriorDY) then
                                Inc(PlayerLineCount); //it takes one more player line to perform the first of the remaining moves
                             end
                        else if (NextMove.x<>0) and
                                DxDyToDirection((NextMove.x and (not MOVE_LIST_FLAG_BOX ))-Position.x,
                                                (NextMove.y and (not MOVE_LIST_FLAG_JUMP))-Position.y,
                                                PreferredDirection) then begin // take the direction of the next move (normally a push) into account, so the intermediate non-pushing player moves can be optimized on moves/lines instead of moves only
                                IsPreferredDirectionSupplied:=True;
                                BackwardPlayerPath:=True;

                                if (DIRECTION_XY[PreferredDirection,ColAxis]<>PriorDX) or
                                   (DIRECTION_XY[PreferredDirection,RowAxis]<>PriorDY) then
                                   Inc(PlayerLineCount); //it takes one more player line to perform the first of the remaining moves
                                end
                             else begin
                                if   LastMoveIndex>0 then
                                     PreferredDirection:=TDirection(History.Moves[LastMoveIndex] and H_MASK_DIRECTION)
                                else PreferredDirection:=Low(PreferredDirection);
                                IsPreferredDirectionSupplied:=LastMoveIndex>0;
                                BackwardPlayerPath:=False;
                                end;
                        if   PlayerPath(oPlayerPos,Position,BackwardPlayerPath,IsPreferredDirectionSupplied,PreferredDirection,NewMoveCount,NewPlayerLineCount,IsPreferredDirectionOK,PPlayerMoves(Addr(NewMoves))) then begin
                             if IsPreferredDirectionSupplied and IsPreferredDirectionOK and (NewMoveCount>0) then // 'True': it takes one more player line to perform the first of the remaining moves if the current player moves are substituted by the new ones
                                Inc(NewPlayerLineCount);

                             if ((NewMoveCount<PlayerMoveCount)
                                 or
                                 ((NewMoveCount=PlayerMoveCount) and (NewPlayerLineCount<PlayerLineCount))
                                ) then begin
                                if   LastMoveIndex<>History.LastPushIndex then begin
                                     {Error('Internal error: Last pushing move index',Application.Title);}
                                     end;
                                for  i:=Rest to Pred(Rest+PlayerMoveCount-oCount) do // perform the moves so the user sees them on the screen; the path substitution takes place later, a few code lines further down
                                     if Result then Result:=TryMove(Moves[i],i,LastMoveIndex,(i=MoveCount) and SoundEnabled,NextMove,0); // 'Self.TryMove' converts moves to undo-actions when it's possible

                                Inc(Rest,PlayerMoveCount-oCount); // later, the caller must handle any remaining moves normally (here, 'oCount' contains the number of non-pushing player-moves on the top of the history)
                                if   (Rest<=MoveCount) and
                                     DxDyToDirection((Moves[Rest].x and (not MOVE_LIST_FLAG_BOX ))-PlayerPos.x,
                                                     (Moves[Rest].y and (not MOVE_LIST_FLAG_JUMP))-PlayerPos.y,
                                                     PreferredDirection) then begin // take the direction of the next move (normally a push) into account, so the intermediate non-pushing player moves can be optimized on moves/lines instead of moves only
                                     IsPreferredDirectionSupplied:=True;
                                     BackwardPlayerPath:=True;
                                     end

                                else if (NextMove.x<>0) and
                                        DxDyToDirection((NextMove.x and (not MOVE_LIST_FLAG_BOX ))-PlayerPos.x,
                                                        (NextMove.y and (not MOVE_LIST_FLAG_JUMP))-PlayerPos.y,
                                                        PreferredDirection) then begin // take the direction of the next move (normally a push) into account, so the intermediate non-pushing player moves can be optimized on moves/lines instead of moves only
                                        IsPreferredDirectionSupplied:=True;
                                        BackwardPlayerPath:=True;
                                        end
                                     else begin
                                        if   LastMoveIndex>0 then
                                             PreferredDirection:=TDirection(History.Moves[LastMoveIndex] and H_MASK_DIRECTION)
                                        else PreferredDirection:=Low(PreferredDirection);
                                        IsPreferredDirectionSupplied:=LastMoveIndex>0;
                                        BackwardPlayerPath:=False;
                                        end;

                                if   PlayerPath(oPlayerPos,PlayerPos,BackwardPlayerPath,IsPreferredDirectionSupplied,PreferredDirection,NewMoveCount,NewPlayerLineCount,IsPreferredDirectionOK,PPlayerMoves(Addr(NewMoves))) and
                                     (NewMoveCount<=History.Count-LastMoveIndex) then begin
                                     Result:=SubstitutePlayerPath(History,oPlayerPos,LastMoveIndex,NewMoveCount,PPlayerMoves(Addr(NewMoves)));
                                     if Result and (History.Count<History.Top) then with History do begin
                                        if   (Moves[Count] and H_FLAG_ODD)=0 then j:=H_FLAG_ODD
                                        else j:=0;
                                        i:=Succ(Count); // turn non-pushing player moves after the current move into a combined move
                                        while (i<=Top) and ((Moves[i] and H_FLAG_BOX)=0) do begin
                                          Moves[i]:=(Moves[i] and (not H_FLAG_ODD)) or j;
                                          Inc(i);
                                          end;
                                        SeparateMoves(Count,History); // separate non-pushing player moves after the current move from the current move; actually, this is unnecessary here, but for the sake of completeness the procecure is called anyway
                                        end;
                                     end;
                                end;
                             end;
                        end;
                  end;
               end;
         end;
    end; // ConvertMovesToUndoMoves

    function  ForkliftDriving(FirstIndex,LastIndex:Integer; var Rest,LastMoveIndex:Integer):Boolean;
    // when appropriate, drive in reverse instead of making a u-turn;
    var X,Y,dx,dy,NonPushingPlayerMovesCount:Integer;
        Direction:TDirection;
        Move:TColRow;
    begin
      Result:=True; Rest:=FirstIndex;
      if FirstIndex<=LastIndex then begin
         Move:=Moves[FirstIndex];
         if (( Move.X and      MOVE_LIST_FLAG_BOX )=0)  then begin
            X:=Move.X and (not MOVE_LIST_FLAG_BOX ); dx:=X-PlayerPos.x;
            Y:=Move.Y and (not MOVE_LIST_FLAG_JUMP); dy:=Y-PlayerPos.y;
            if DxDyToDirection(dx,dy,Direction) and
               (Direction=OPPOSITE_DIRECTION[VisiblePlayerDirection]) then begin
               NonPushingPlayerMovesCount:=0;
               repeat Inc(NonPushingPlayerMovesCount);
                      Inc(X,dx); Inc(Y,dy);                     // next square in the current direction
                      Inc(FirstIndex); Move:=Moves[FirstIndex]; // next move
               until  (FirstIndex>LastIndex) or
                      (NonPushingPlayerMovesCount>ForkliftDrivingDriveInReverseSquares) or
                      ((Move.X and      MOVE_LIST_FLAG_BOX  )<>0) or
                      ((Move.X and (not MOVE_LIST_FLAG_BOX ))<>X) or // matching X,Y: the player continues in the current direction
                      ((Move.Y and (not MOVE_LIST_FLAG_JUMP))<>Y);

               if     NonPushingPlayerMovesCount<=ForkliftDrivingDriveInReverseSquares then begin
                      repeat Result:=TryMove(Moves[Rest],Rest,LastMoveIndex,(Rest=LastIndex) and SoundEnabled,Moves[Succ(Rest)],MOVE_FLAG_FORKLIFT_DRIVING);
                             Inc(Rest);
                      until  (Rest=FirstIndex) or (not Result);
                      end;
               end;
            end;
         end;
    end;

  begin // DoMoves
    Result:=ConvertMovesToUndoMoves(Rest,LastMoveIndex);     // first check if some of the previous moves can be cancelled out;
    j:=Min(MoveCount,MAX_MOVES-History.Count);
    if  ForkliftDrivingEnabled and Result then
        Result:=ForkliftDriving(Rest,j,Rest,LastMoveIndex);
    for i:=Rest to j do                                      // then perform the rest of the moves normally
        if Result then Result:=TryMove(Moves[i],i,LastMoveIndex,(i=j) and SoundEnabled,NextMove,0);
  end; // DoMoves

begin // TryMoves
  if (not IsBusy) then begin
     MainForm.Deadlocks.Suspend;
     try     IsBusy:=True;
             oCount:=History.Count;                          // current number of moves in the history
             oPushCount:=History.PushCount;
             LastMoveIndex:=oCount;                          // previous move number; it decreases if moves are taken back
             oShowMovesEnabled:=ShowMovesEnabled;
             oUndoRedoCombinedMoves:=MainForm.UndoRedoCombinedMoves;
             oSoundEnabled:=MainForm.Sound.Enabled;

             try     ShowMovesEnabled:=MoveAnimationEnabled; // only show individual moves if move-animation is enabled
                     MainForm.UndoRedoCombinedMoves:=False;     // 'False', so 'Undo' takes one move at a time

                     p1.x:=0; // '0': no successor moves after 'DoMoves' has finished
                     DoMoves(MoveCount,Moves,LastMoveIndex,oSoundEnabled,p1); // perform the moves

             finally ShowMovesEnabled:=oShowMovesEnabled;    // restore settings
                     MainForm.UndoRedoCombinedMoves:=oUndoRedoCombinedMoves;
                     MainForm.Sound.Enabled:=oSoundEnabled;

                     if (not MoveAnimationEnabled) {and (not IsReplaying)} then begin
                        i:=History.Count;
                        while (i>oCount) and
                              ((History.Moves[i] and H_FLAG_BOX)=0) do Dec(i);
                        if (i<History.Count) and (i>oCount) then with History do begin
                           // something went wrong: a box-move was rejected
                           // and the player made one or more moves after the last
                           // accepted push; take back these extra player moves
                           MakeCombinedMove(Succ(LastMoveIndex),i); // split the move after the last box-push
                           MakeCombinedMove(Succ(i),History.Count);
                           Inherited Undo(True); // take back the extra player moves
                           end;
                        end;

                     // moves were performed individually;
                     // now group them to form a combined move
                     MakeCombinedMove(Succ(LastMoveIndex),History.Count);

                     if ShowMovesEnabled and
                        (not MoveAnimationEnabled) and
                        (MoveCount>0) then begin             // show move instantly:
                        UnpackLastMove(p1,p2,p3,p4,BoxNo);   // 'TryMoves' are never called in a way where more than one box is involved, hence, this suffices to get the box number
                        MainForm.ShowMoveInstantly(BoxNo,LastMoveIndex=History.Count,(LastMoveIndex<History.Count) and ((History.Moves[History.Count] and H_FLAG_JUMP)<>0),True);
                        if (GameState=gsSolved) and MainForm.Sound.Enabled then
                           MainForm.Sound.Play(stSolution);
                        end;

                     if IsALegalPosition then;
             end;
     finally IsBusy:=False; MainForm.Deadlocks.Resume;
             if SecondaryMetricsInTitles and Assigned(MainForm.MultiView.Selected) and (not IsBrowsing) and (not IsReplaying) then
                MainForm.MultiView.Selected.ShowCaptionWithMetrics(True);
             if (GameState=gsSolved) and (Self=MainForm.Game) then
                MainForm.CheckAutoAdvanceWhenSolved;
     end;
     end;
end;

function  TGame.DoMoveUpdateBoard(dx,dy,Flags:Integer):Integer;
begin
  Result:=Inherited DoMoveUpdateBoard(dx,dy,Flags);
  Inc(MoveCountSinceLastKeyUpEvent);
  if (Result<>0) and
     Assigned(DeadlockDetection.Deadlocks) and
     DeadlockDetection.Enabled and
     (not IsBrowsing) then
     DeadlockDetection.Deadlocks.MoveBox(Result);
end;

procedure TGame.InitGame;
begin
  Reset(True);
  if @ShowGame<>nil then ShowGame;
  IsBusy:=False; IsReplaying:=False; IsBrowsing:=False; UserBreakWhileReplaying:=False;
end;

function  TGame.InheritedRedo(RedoCombinedMoves__:Boolean):Boolean;
begin
  Result:=Redo0(RedoCombinedMoves__);
end;

function  TGame.InheritedUndo(RedoCombinedMoves__:Boolean):Boolean;
begin
  Result:=Inherited Undo(RedoCombinedMoves__);
end;

procedure TGame.Reset(ResetCombinedMoveNumbers:Boolean);
begin
  Inherited;
  MoveCountSinceLastKeyUpEvent:=0;
  BookmarkCount:=0;
  if Assigned(DeadlockDetection.Deadlocks) then begin
     DeadlockDetection.Deadlocks.LoadBoard;
     MainForm.MakeBookmarksHint;
     end;
end;

function TGame.Undo(UndoCombinedMoves:Boolean):Boolean;
// caution: note that this procedure only works for the game 'MainForm.Game'
var i,j,dx,dy,oCount,oPushCount:Integer;
    PlayerFromPos,PlayerToPos:TColRow;
    BoxNo:array[0..MAX_MOVES] of Int16;
    BoxVisited:array[0..MAX_BOXES] of Boolean;
begin
  Result:=(History.Count>ForcedInitialJumps) and
          (not IsBusy) and
          (not IsReplaying) and
          (not IsBrowsing) and
          (Self=MainForm.Game);
  if Result then with History do begin
     MainForm.Deadlocks.Suspend;
     try     IsBusy:=True; oCount:=Count; oPushCount:=PushCount;
             PlayerToPos:=PlayerPos;

             Inherited Undo(UndoCombinedMoves);

             if Count<oCount then begin
                // replay the moves taken back to find the moved boxes, if any
                for i:=Succ(Count) to oCount do begin
                    if TDirection(Moves[i] and H_MASK_DIRECTION)<>TDirection(Moves[Pred(i)] and H_MASK_DIRECTION) then
                       Inc(PlayerLinesCount); {update the number of player lines}
                    HistoryMoveToDxDy(Moves[i],dx,dy);
                    BoxNo[i]:=DoMoveUpdateBoard0(dx,dy,Moves[i]); // note: 'DoMoveUpdateBoard0', not 'DoMoveUpdateBoard', i.e., no inherited updating
                    end;

                Count:=oCount; PushCount:=oPushCount; // finally, undo the moves again
                Inherited Undo(UndoCombinedMoves);

                // update bookmarks (not in production anymore; superseded by the snapshots feature)
                i:=Bookmark[1];
                while (i<>0) and (i<Count) do begin
                  Inc(BookmarkCount); i:=Bookmark[Succ(BookmarkCount)];
                  end;
                if BookmarkCount<>0 then MainForm.MakeBookmarksHint;

                if ShowMovesEnabled then // update the screen
                   if   MainForm.Image1.Visible then
                        if MoveAnimationEnabled then begin
                           for i:=oCount downto Succ(Count) do begin
                               HistoryMoveToDxDy(Moves[i],dx,dy);
                               PlayerFromPos.x:=PlayerToPos.x-dx;
                               PlayerFromPos.y:=PlayerToPos.y-dy;
                               MainForm.ShowMove(Point(PlayerFromPos.x,PlayerFromPos.y),
                                                 Point(PlayerToPos  .x,PlayerToPos  .y),
                                                 BoxNo[i],AnimateUndoMoveMS,
                                                 i,oPushCount,
                                                 True,
                                                 (Moves[i] and H_FLAG_JUMP)<>0,
                                                 i=Succ(Count));
                               PlayerToPos    :=PlayerFromPos;
                               if BoxNo[i]<>0 then Dec(oPushCount);
                               end;
                           MainForm.ShowStatus;
                           end
                        else begin // move-animation disabled; show move(s) instantly
                           j:=0;
                           if (Moves[oCount] and H_FLAG_JUMP)=0 then begin
                              // nothing prevents a combined move from moving several boxes,
                              // so the screen-position for each box must be updated
                              for i:=oCount downto Succ(Count) do // first hide any boxes involved in the move
                                  if BoxNo[i]<>0 then MainForm.GameViewer.HideBox(BoxNo[i]);
                              FillChar(BoxVisited,SizeOf(BoxVisited),0);
                              for i:=oCount downto Succ(Count) do
                                  if (BoxNo[i]<>0) and (not BoxVisited[BoxNo[i]]) then begin
                                     BoxVisited[BoxNo[i]]:=True; Inc(j);
                                     MainForm.ShowMoveInstantly(BoxNo[i],True,False,True);
                                     end;
                              end;
                           if j=0 then // the move didn't involve any boxes; update player position
                              MainForm.ShowMoveInstantly(0,True,(Moves[oCount] and H_FLAG_JUMP)<>0,True)
                           end
                   else if @ShowGame<>nil then ShowGame;
                end;
             MoveCountSinceLastKeyUpEvent:=0;
     finally IsBusy:=False;
             MainForm.Deadlocks.Resume;
             if Assigned(DeadlockDetection.Deadlocks) then
                DeadlockDetection.Deadlocks.LoadBoard;
             if SecondaryMetricsInTitles and Assigned(MainForm.MultiView.Selected) and (not IsBrowsing) and (not IsReplaying) and (Self=MainForm.Game) then
                MainForm.MultiView.Selected.ShowCaptionWithMetrics(True);
     end;
     end;
end;

function  TGame.Redo(RedoCombinedMoves:Boolean):Boolean;
// caution: note that this procedure only works for the game 'MainForm.Game'
var i,j,oCount,oTop,oSimpleIllegalMovesMask,Move,Separator:Integer; oIsReplaying:Boolean;
    BoxVisited:array[0..MAX_BOXES] of Boolean;

  procedure ShowMove(IsReplaying,SoundEnabled:Boolean);
  var TimeMS,BoxNo,StartCount,StartPushCount:Integer; PlayerFromPos,p1,p2,p3:TColRow;
  begin // 'IsReplaying': either the task is to replay all moves or to redo next (combined) move
    if Self=MainForm.Game then  with History do begin
       UnpackLastMove(PlayerFromPos,p1,p2,p3,BoxNo);
       BoxVisited[BoxNo]:=True;
       if   MoveAnimationEnabled then begin
            if   IsReplaying then
                 TimeMS:=AnimateReplayMovesMS
            else TimeMS:=AnimateDoMoveMS;

            StartCount:=Count; StartPushCount:=PushCount;
            if MainForm.Image1.Visible then begin // use values before the move was performed
               Dec(StartCount); if BoxNo<>0 then Dec(StartPushCount);
               end;

            MainForm.ShowMove(Point(PlayerFromPos.x,PlayerFromPos.y),
                              Point(PlayerPos    .x,PlayerPos    .y),
                              BoxNo,
                              TimeMS,
                              StartCount,StartPushCount,
                              False,
                              (Moves[Count] and H_FLAG_JUMP)<>0,
                              SoundEnabled);

            if IsReplaying and
               (History.Count<>Succ(ForcedInitialJumps)) and
               MainForm.Image1.Visible then begin
               MainForm.Status.MoveCount:=IntToStr(History.Count); // no need for a full 'ShowStatus'
               MainForm.Status.PushCount:=IntToStr(History.PushCount);
               if Assigned(MainForm.MultiView.Selected) then with MainForm.MultiView.Selected do with Panels[mvipCaption] do
                  ShowPanel(mvipCaption,Format(FORMAT_MOVES_AND_PUSHES,[History.Count,History.PushCount]),Enabled,Rect,Alignment);
               end
            else begin
               if not IsReplaying then Self.IsReplaying:=False; // for correct buttons on the statusbar
               MainForm.ShowStatus;
               Self.IsReplaying:=True; // necessary for getting 'IsIdleAndStopReplay' to update 'UserBreakWhileReplaying'
              end;
            end;
       end;
  end;

begin // Redo
  Result:=(History.Count<History.Top) and
          (not IsBusy);

  oIsReplaying                        :=IsReplaying; // either the task is "replay all moves" or "redo next (combined) move"
  oCount                              :=History.Count;
  oTop                                :=History.Top;
  oSimpleIllegalMovesMask             :=SimpleIllegalMovesMask;

  if Result then with History do begin
     if not oIsReplaying then begin
        MainForm.Deadlocks.Suspend;
        MovesOrHistory.History:=History; // the history is saved in a global variable; allocating a large local variable on the stack may cause a stack overflow
        end;
     try
       try     IsBusy                 :=True;
               IsReplaying            :=True; // necessary for getting 'IsIdleAndStopReplay' to update 'UserBreakWhileReplaying'
               UserBreakWhileReplaying:=False;
               SimpleIllegalMovesMask :=0;

               FillChar(BoxVisited,SizeOf(BoxVisited),0);

               Move                   :=Moves[Succ(Count)];
               Separator              :=Move and (H_MASK_MOVE_SEPARATOR or H_FLAG_JUMP);
               if (Move and H_FLAG_JUMP)<>0 then RedoCombinedMoves:=True; // jumps cannot be broken
               repeat Move            :=Moves[Count+2]; // find moves
               until  (not Redo0(False)) or    // do the moves in order to figure out which ones to redo
                      (Count=Top) or
                      (Separator<>(Move and (H_MASK_MOVE_SEPARATOR or H_FLAG_JUMP))) or
                      (not RedoCombinedMoves);

               j:=Count;
               while Count>oCount do begin
                 Top:=Count; // avoid calculating 'SeparateMoves()' for each move
                 Inherited Undo(False); // take the moves back again
                 end;
               Top:=oTop;

               for i:=Succ(oCount) to j do
                   if Result and (not UserBreakWhileReplaying) then begin
                      Result          :=Redo0(False); // do the moves, this time for real
                      if Result then begin
                         MoveCountSinceLastKeyUpEvent:=0;
                         ShowMove(oIsReplaying,i=j);
                         end;
                      end;

               if not oIsReplaying then begin
                  Moves:=MovesOrHistory.History.Moves;  // restore moves; 'Redo0()' has destroyed the combined move flags in 'History.Moves'
                  MakeCombinedMove(Succ(oCount),Count); // group moves to form a combined move
                  SeparateUndoneMoves;
                  end;

               Result:=(Count>oCount) and (not UserBreakWhileReplaying);

               if Result then
                  if (not MoveAnimationEnabled) and (not oIsReplaying) then begin
                     IsReplaying:=False;
                     for i:=1 to BoxCount do     // hide all boxes involved in the move
                         if BoxVisited[i] then MainForm.GameViewer.HideBox(i);
                     j:=0;
                     for i:=1 to BoxCount do     // show all boxes involved in the move
                         if BoxVisited[i] then begin
                            MainForm.ShowMoveInstantly(i,False,False,True);
                            Inc(j);
                            end;
                     if j=0 then // the move didn't involve any boxes; update player position
                        MainForm.ShowMoveInstantly(0,False,(Moves[Count] and H_FLAG_JUMP)<>0,True);
                     end
                  else
               else
                  if (not oIsReplaying) and (@ShowGame<>nil) then
                     ShowGame; // either the user stopped replay/redo, or something went wrong (should not happen): refresh the screen
       finally IsBusy:=False; IsReplaying:=oIsReplaying;
               if (History.Count<>oCount) and (not IsReplaying) then begin
                  if Assigned(DeadlockDetection.Deadlocks) then
                     DeadlockDetection.Deadlocks.LoadBoard;
                  if IsALegalPosition then;
                  end;

       end;
     finally   SimpleIllegalMovesMask:=oSimpleIllegalMovesMask; // the inner try-finally block could raise an exception, hence, this extra try-finally block
               Top:=oTop;
               if not oIsReplaying then begin
                  MainForm.Deadlocks.Resume;
                  if SecondaryMetricsInTitles and Assigned(MainForm.MultiView.Selected) and (not IsBrowsing) and (not IsReplaying) and (Self=MainForm.Game) then
                     MainForm.MultiView.Selected.ShowCaptionWithMetrics(True);
                  end;
     end;
     end;
end;

function  TGame.ReplayInMainWindow(Forwards:Boolean):Boolean;
// caution: note that this procedure only works for the game 'MainForm.Game'
var
  i,CollapsedMoveCount,StartCount,StartPushCount,Stop,oSimpleIllegalMovesMask:Integer;
  oWindowResizeCount:Cardinal;
  PlayerStartPos,BoxStartPos,p1,p2,p3,p4:TColRow;
  oDeadlockDetectionEnabled,oSoundEnabled,oTimingEnabled:Boolean;
  CollapsedMoves:TCollapsedMoves;

begin // ReplayInMainWindow
  Result:=(not IsBusy) and (not IsReplaying) and (not IsBrowsing) and
          (Self=MainForm.Game) and  // caution: note that this procedure only works for the game 'MainForm.Game'
          ((Forwards and (History.Count<History.Top))
           or
           ((not Forwards) and (History.Count>ForcedInitialJumps)));

  if Result then begin
     StopTimer;
     MainForm.Deadlocks.Suspend;
     ToolsForm.Game.History:=History; // the stack may overflow if the copy is stored locally on the stack, hence, use the toolsform game instead
     oSoundEnabled:=MainForm.Sound.Enabled;
     UserBreakWhileReplaying:=False;
     oTimingEnabled:=TimingEnabled;
     oDeadlockDetectionEnabled:=DeadlockDetection.Enabled;
     oSimpleIllegalMovesMask:=SimpleIllegalMovesMask;
     oWindowResizeCount:=MainForm.GameViewer.WindowResizeCount;

     try
       try     IsReplaying:=True; TimingEnabled:=False;
               DeadlockDetection.Enabled:=False; SimpleIllegalMovesMask:=0;

               if   MoveAnimationEnabled and MainForm.Image1.Visible and (not MainForm.MakeScreenshots) then begin
                    if CollapseMoves(Forwards,
                                     MainForm.GamePictures.UsePlayerImageForMoveAnimationAlsoForGoalSquares,
                                     MainForm.GamePictures.UseBoxImageForMoveAnimationAlsoForGoalSquares,
                                     CollapsedMoveCount,CollapsedMoves) then begin

                       if  MainForm.Game.MoveAnimationEnabled then
                           MainForm.Status.EnterReplayMode;

                       repeat // until all moves have been performed/taken back depending on the replay direction
                         for i:=1 to CollapsedMoveCount do with CollapsedMoves[i] do begin // the collapsed moves vector is not necessarily big enough to accomodate all moves in the game, hence, the enclosing 'repeat' loop

                             while (AnimateReplayMovesMS=0) and IsReplaying and (not UserBreakWhileReplaying) do
                               Application.ProcessMessages;

                             if   UserBreakWhileReplaying and
                                  ((Board[PlayerPos.x,PlayerPos.y] and (WALL+BOX))=0) then begin // '0': not in the middle of a jump in reverse mode
                                  IsReplaying:=False;
                                  MainForm.IgnoreKeyUp:=True;
                                  MainForm.IgnoreMouseUp:=True;
                                  end;

                             if   IsReplaying then with History do begin
                                  StartCount    :=Count;
                                  StartPushCount:=PushCount;
                                  PlayerStartPos:=PlayerPos;
                                  BoxStartPos   :=BoxPos[BoxNo];

                                  if Forwards then begin
                                     Stop       :=Pred(CollapsedMoves[Succ(i)].Start);
                                     while Result and (Count<Stop) do
                                       Result   :=Redo0(False); // note: not 'Self.Redo'
                                     if Result then
                                        MainForm.ShowMove(Point(PlayerStartPos.x,PlayerStartPos.y),
                                                          Point(PlayerPos     .x,PlayerPos     .y),
                                                          BoxNo,
                                                          AnimateReplayMovesMS*Succ(Abs(Stop-Start)),
                                                          StartCount,
                                                          StartPushCount,
                                                          False,
                                                          (Moves[Start] and H_FLAG_JUMP)<>0,
                                                          False);
                                     if Start=Succ(ForcedInitialJumps) then
                                        MainForm.ShowStatus; // enable browse-buttons
                                     end
                                  else begin // backwards
                                     Stop       :=Succ(CollapsedMoves[Succ(i)].Start);
                                     while Result and (Count>=Stop) do
                                       Result   :=Inherited Undo(False); // note: not 'Self.Undo'
                                     if Result then
                                        MainForm.ShowMove(Point(PlayerPos     .x,PlayerPos     .y),
                                                          Point(PlayerStartPos.x,PlayerStartPos.y),
                                                          BoxNo,
                                                          AnimateReplayMovesMS*Succ(Abs(Stop-Start)),
                                                          StartCount,
                                                          StartPushCount,
                                                          True,
                                                          (Moves[Start] and H_FLAG_JUMP)<>0,
                                                          False);
                                     if i=1 then
                                        MainForm.ShowStatus; // enable browse-buttons
                                     end;
                                  end;
                             if not (IsReplaying and Result) then break;
                             end;

                         if  IsReplaying and
                             Result and
                             ((     Forwards  and (History.Count<History.Top       ))
                              or
                              ((not Forwards) and (History.Count>ForcedInitialJumps))
                             ) then
                             Result:=CollapseMoves(Forwards,
                                                   MainForm.GamePictures.UsePlayerImageForMoveAnimationAlsoForGoalSquares,
                                                   MainForm.GamePictures.UseBoxImageForMoveAnimationAlsoForGoalSquares,
                                                   CollapsedMoveCount,CollapsedMoves); // collapse the next group of moves

                       until (not (IsReplaying and Result))
                             or
                             (     Forwards  and (History.Count=History.Top        ))
                             or
                             ((not Forwards) and (History.Count<=ForcedInitialJumps));
                       end;
                    end
               else begin
                      if    MainForm.MakeScreenshots then
                            MainForm.CreateScreenshot;
                      while IsReplaying and
                            Result and
                            (History.Count<History.Top) do begin
                            if not MainForm.MakeScreenshots then
                               Result:=Redo(True)
                            else begin
                               Result:=Redo(False);
                               MainForm.CreateScreenshot;
                               end;
                            if UserBreakWhileReplaying and
                               ((Board[PlayerPos.x,PlayerPos.y] and (WALL+BOX))=0) then
                               IsReplaying:=False;
                            end;
                    end;
       finally
               IsReplaying:=False; UserBreakWhileReplaying:=False;
               TimingEnabled:=oTimingEnabled;
               DeadlockDetection.Enabled:=oDeadlockDetectionEnabled;
               History.Moves:=ToolsForm.Game.History.Moves; // restore moves so combined move flags aren't modified
               MainForm.Deadlocks.Resume;
               if @ShowGame<>nil then ShowGame;
               MainForm.ShowStatus;

               with MainForm do begin
                  TrackBox.X:=0; TrackBox.Y:=0; MainForm.UpdateCursor;
                  IgnoreKeyUp:=True;
                  IgnoreMouseUp:=True;
                  end;
               MainForm.Sound.Enabled:=oSoundEnabled;
               if (not MoveAnimationEnabled) then with History do
                  if Count<>ToolsForm.Game.History.Count then begin // play sound for last move:
                     UnpackLastMove(p1,p2,p3,p4,i);
                     MainForm.ShowMoveInstantly(i,Count<ToolsForm.Game.History.Count,(Moves[Count] and H_FLAG_JUMP)<>0,True);
                     end;
               SeparateUndoneMoves;
               if AnimateReplayMovesMS=0 then
                  AnimateReplayMovesMS:=DEFAULT_ANIMATE_REPLAY_MOVES_MS;
               if Assigned(DeadlockDetection.Deadlocks) then
                  DeadlockDetection.Deadlocks.LoadBoard;
               if IsALegalPosition then;

               if (oWindowResizeCount<>MainForm.GameViewer.WindowResizeCount) and
                  (not MainForm.MultiView.IsEmpty) and
                  (not IsBusy) and (not IsReplaying) and (not IsBrowsing) then with MainForm.MultiView do begin
                  DisappearedItemsCount:=0;
                  OnResize(nil); // recalculate information, in particular for showing the correct 'maximize' and 'minimize' status for each view
                  if ReportDisappearedItems(DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
                     DoReportDisappearedItems:=False;
                  end;
       end;
     finally   SimpleIllegalMovesMask:=oSimpleIllegalMovesMask; // the inner try-finally block could raise an exception; hence, this extra try-finally block
     end;
     end;
end; // ReplayInMainWindow

function  TGame.ReplayInToolsWindow(Forwards,SingleMove:Boolean):Boolean;
// caution: note that this procedure only works for the game 'OpenForm.Game'
var
  i,CollapsedMoveCount,StartCount,
  StartPushCount,CurrentBoxNo,Stop{,oSimpleIllegalMovesMask}:Integer;
  PlayerStartPos,BoxStartPos{,p1,p2,p3,p4}:TColRow;
  //oDeadlockDetectionEnabled,oSoundEnabled,oTimingEnabled:Boolean;
  CollapsedMoves:TCollapsedMoves;

begin // ReplayInToolsWindow
  Result:=(not IsBusy) and (not IsReplaying) and (not IsBrowsing) and
          (Self=OpenForm.Game) and  // caution: note that this procedure only works for the game 'OpenForm.Game'
          ((Forwards and (History.Count<History.Top))
           or
           ((not Forwards) and (History.Count>ForcedInitialJumps)));

  if Result then begin
     StopTimer;
     //MainForm.Deadlocks.Suspend;
     ToolsForm.Game.History:=History; // the stack may overflow if the copy is stored locally on the stack, hence, use the toolsform game instead
     //oSoundEnabled:=MainForm.Sound.Enabled;
     UserBreakWhileReplaying:=False;
     //oTimingEnabled:=TimingEnabled;
     //oDeadlockDetectionEnabled:=DeadlockDetection.Enabled;
     //oSimpleIllegalMovesMask:=SimpleIllegalMovesMask;
     MoveAnimationEnabled:=True;
     SmoothMoveAnimationEnabled:=True;
     SessionSmoothMoveAnimationEnabled:=SmoothMoveAnimationEnabled;
     //try
       try     IsReplaying:=True; TimingEnabled:=False;
               DeadlockDetection.Enabled:=False; SimpleIllegalMovesMask:=0;

               if   MoveAnimationEnabled and ToolsForm.Visible then begin
                    if CollapseMoves(Forwards,False,False,CollapsedMoveCount,CollapsedMoves) then begin

                       if not SingleMove then
                          if   (ToolsForm.ReplaySpeedMovesPerSecond>0) and (ToolsForm.ReplaySpeedMovesPerSecond<=MAX_REPLAY_SPEED_MOVES_PER_SEC) then
                               AnimateReplayMovesMS:=1000 div ToolsForm.ReplaySpeedMovesPerSecond
                          else AnimateReplayMovesMS:=DEFAULT_ANIMATE_REPLAY_MOVES_MS
                       else begin

                          i:=1;
                          while (i<=CollapsedMoveCount) and (CollapsedMoves[i].BoxNo=0) do Inc(i); // search for next pushed box, if any
                          if i<CollapsedMoveCount then begin // 'True': got a box
                             CurrentBoxNo:=CollapsedMoves[i].BoxNo;
                             repeat Inc(i); // search for a push involving another box number
                             until (i>CollapsedMoveCount)
                                   or
                                   ((CollapsedMoves[i].BoxNo<>0)
                                    and
                                    (CollapsedMoves[i].BoxNo<>CurrentBoxNo)
                                   );
                             if Forwards then
                                repeat Dec(i); // take back trailing non-pushing moves
                                until  CollapsedMoves[i].BoxNo<>0
                             else Dec(i);
                             CollapsedMoveCount:=i;
                             end;

                          if Forwards then begin
                             AnimateReplayMovesMS:=MainForm.Game.AnimateDoMoveMS;
                             end
                          else begin
                             AnimateReplayMovesMS:=MainForm.Game.AnimateUndoMoveMS;
                             end;
                          end;

                       repeat
                         for i:=1 to CollapsedMoveCount do with CollapsedMoves[i] do begin // the collapsed moves vector is not necessarily big enough to accomodate all moves in the game, hence, the enclosing 'repeat' loop

                             while (AnimateReplayMovesMS=0) and IsReplaying and (not UserBreakWhileReplaying) do
                               Application.ProcessMessages;

                             if   UserBreakWhileReplaying and
                                  ((Board[PlayerPos.x,PlayerPos.y] and (WALL+BOX))=0) then begin // '0': not in the middle of a jump in reverse mode
                                  IsReplaying:=False; //MainForm.IgnoreMouseUp:=True;
                                  end;

                             if   IsReplaying then with History do begin
                                  StartCount    :=Count;
                                  StartPushCount:=PushCount;
                                  PlayerStartPos:=PlayerPos;
                                  BoxStartPos   :=BoxPos[BoxNo];

                                  if Forwards then begin
                                     Stop       :=Pred(CollapsedMoves[Succ(i)].Start);
                                     while Result and (Count<Stop) do
                                       Result   :=Redo0(False); // note: not 'Self.Redo'
                                     if Result then
                                        LevelSetForm.ShowMove(Point(PlayerStartPos.x,PlayerStartPos.y),
                                                              Point(PlayerPos     .x,PlayerPos     .y),
                                                              Point(BoxStartPos   .x,BoxStartPos   .y),
                                                              BoxNo,
                                                              AnimateReplayMovesMS*Succ(Abs(Stop-Start)),
                                                              StartCount,
                                                              StartPushCount,
                                                              False,
                                                              (Moves[Start] and H_FLAG_JUMP)<>0,
                                                              False);
                                     if Start=Succ(ForcedInitialJumps) then begin
                                        ToolsForm.ShowStatus; // enable browse-buttons
                                        end;
                                     end
                                  else begin // backwards
                                     Stop       :=Succ(CollapsedMoves[Succ(i)].Start);
                                     while Result and (Count>=Stop) do
                                       Result   :=Inherited Undo(False); // note: not 'Self.Undo'
                                     if Result then
                                        LevelSetForm.ShowMove(Point(PlayerPos     .x,PlayerPos     .y),
                                                              Point(PlayerStartPos.x,PlayerStartPos.y),
                                                               Point(BoxStartPos   .x,BoxStartPos   .y),
                                                              BoxNo,
                                                              AnimateReplayMovesMS*Succ(Abs(Stop-Start)),
                                                              StartCount,
                                                              StartPushCount,
                                                              True,
                                                              (Moves[Start] and H_FLAG_JUMP)<>0,
                                                              False);
                                     if i=1 then begin
                                        ToolsForm.ShowStatus; // enable browse-buttons
                                        end;
                                     end;
                                  end;
                             if not (IsReplaying and Result) then break;
                             end;

                         if  IsReplaying and
                             Result and
                             (not SingleMove) and // the collapsed move vector is assumed to be big enough to contain all moves for a single box session; if that doesn't hold, the program handles it gracefully by stopping when the collapsed moves vector is full
                             ((     Forwards  and (History.Count<History.Top       ))
                              or
                              ((not Forwards) and (History.Count>ForcedInitialJumps))
                             ) then
                             Result:=CollapseMoves(Forwards,False,False,CollapsedMoveCount,CollapsedMoves); // collapse the next group of moves

                       until (not (IsReplaying and Result))
                             or
                             SingleMove // the collapsed move vector is assumed to be big enough to contain all moves for a single box session; if that doesn't hold, the program handles it gracefully by stopping when the collapsed moves vector is full
                             or
                             (     Forwards  and (History.Count=History.Top        ))
                             or
                             ((not Forwards) and (History.Count<=ForcedInitialJumps));
                       end;
                    end
               else while IsReplaying and
                          Result and
                          (History.Count<History.Top) do begin
                          Result:=Redo(True);
                          if UserBreakWhileReplaying and
                             ((Board[PlayerPos.x,PlayerPos.y] and (WALL+BOX))=0) then
                             IsReplaying:=False;
                          end;
       finally
               IsReplaying:=False; UserBreakWhileReplaying:=False;
               //TimingEnabled:=oTimingEnabled;
               //DeadlockDetection.Enabled:=oDeadlockDetectionEnabled;
               History.Moves:=ToolsForm.Game.History.Moves; // restore moves so combined move flags aren't modified
               //MainForm.Deadlocks.Resume;
               //if @ShowGame<>nil then ShowGame;
               //ToolsForm.ShowStatus;
               SeparateUndoneMoves;
               ToolsForm.StatusBar1.Panels[1].Text:='';
       end;
     //finally   SimpleIllegalMovesMask:=oSimpleIllegalMovesMask; // the inner try-finally block could raise an exception; hence, this extra try-finally block
     //end;
     end;
end; // ReplayInToolsWindow

function  TGame.EnterBrowseMode(BrowseChoicePoints__:Boolean):Boolean;
var i,j:Integer;
begin
  Result:=IsIdleAndStopReplayingAndBrowsing;
  if Result then with History do begin
     IsBusy:=True;
     BrowseChoicePoints:=BrowseChoicePoints__; // save browse mode
     BrowseStartPosition:=Count; // caution: the saved value is a normal number of moves and not the scrollbar position as the name might indicate

     if   Self=MainForm.Game then begin
          OpenForm.Game.History:=History;                               // save flags here
          Result:=MainForm.Status.EnterBrowseMode;
          end;

     if   Result then begin

          fBrowsePosition:=-1; fMaxBrowsePosition:=0; i:=1;
          while i<=Top do begin
            while (i<=Top) and ((Moves[i] and H_FLAG_BOX)=0) do Inc(i); // locate next box-move
            if i<=Top then begin
               Inc(fMaxBrowsePosition); // calculate the maximum number of browse positions, measured in pushes or box lines depending on the browse mode
               if BrowseChoicePoints then begin
                  j:=H_FLAG_BOX+(Moves[i] and H_MASK_DIRECTION);
                  // skip 'boxlines', i.e., boxes moving in a straight line
                  while (i<=Top) and ((Moves[i] and (H_FLAG_BOX+H_MASK_DIRECTION))=j) do Inc(i);
                  end
               else Inc(i); // advance to next move
               end;
            if (i>=Count) and (fBrowsePosition=-1) then
               fBrowsePosition:=Pred(fMaxBrowsePosition); // current browse position, measured in pushes or box lines depending on the browse mode
            end;

          if fMaxBrowsePosition=0 then begin // 'True': no box pushes
             fMaxBrowsePosition:=Top; fBrowsePosition:=Count;
             end;

          BrowsePosition:=fBrowsePosition;

          IsBrowsing:=True; // set switches in this order
          end
     else IsBusy:=False;
     end;
end;

procedure TGame.LeaveBrowseMode(RestorePosition:Boolean);
var i,oSimpleIllegalMovesMask:Integer;
    OK,oTimingEnabled:Boolean;
begin
  if IsBrowsing then with History do begin
     IsBrowsing:=False;

     if (Self=MainForm.Game) and (MainForm.GameViewer.LegalMovesInfo.Mask<>0) then begin
        MainForm.GameViewer.HideLegalMoves;
        MainForm.TrackBox.X:=0; MainForm.TrackBox.Y:=0;
        end;

     if RestorePosition then begin
        oSimpleIllegalMovesMask:=SimpleIllegalMovesMask;
        SimpleIllegalMovesMask:=0;
        oTimingEnabled:=TimingEnabled;
        try     TimingEnabled:=False; OK:=True;
                Reset(False);
                for i:=1 to BrowseStartPosition do // 'GoToPosition' can't do the job because it takes a scrollbar browsing position as input, not the number of moves
                    OK:=OK and Redo0(False);
        finally SimpleIllegalMovesMask:=oSimpleIllegalMovesMask;
                TimingEnabled:=oTimingEnabled;
        end;
        if @ShowGame<>nil then ShowGame;
        end;

     if Self=MainForm.Game then begin
        MainForm.Status.LeaveBrowseMode;
        MainForm.ShowStatus;
        History.Moves:=OpenForm.Game.History.Moves; // restore flags
        end;

     SeparateUndoneMoves;

     IsBusy:=False; // reset switches in this order

     if SecondaryMetricsInTitles and Assigned(MainForm.MultiView.Selected) and (not IsBrowsing) and (not IsReplaying) and (Self=MainForm.Game) then
        MainForm.MultiView.Selected.ShowCaptionWithMetrics(True);
     end;
end;

procedure TGame.SetBrowsePosition(BrowsePosition__:Integer);
begin
  fBrowsePosition:=GoToPosition(Max(0,Min(BrowsePosition__,MaxBrowsePosition)),False);
  if Self=MainForm.Game then MainForm.Status.BrowsePosition:=BrowsePosition;
end;

function TGame.GoToPosition(Position:Integer; SeparateUndoneMoves__:Boolean):Integer;
var i,oSimpleIllegalMovesMask:Integer; oDeadlockDetectionEnabled,oTimingEnabled:Boolean;
begin // note that this destroys existing combined move information
  Result:=0; StopTimer;
  oDeadlockDetectionEnabled:=DeadlockDetection.Enabled;
  oSimpleIllegalMovesMask:=SimpleIllegalMovesMask;
  oTimingEnabled:=TimingEnabled;
  if (Self=MainForm.Game) and (MainForm.GameViewer.LegalMovesInfo.Mask<>0) then begin
     MainForm.GameViewer.HideLegalMoves;
     MainForm.TrackBox.X:=0; MainForm.TrackBox.Y:=0;
     end;

  with History do
    try
      DeadlockDetection.Enabled:=False;
      SimpleIllegalMovesMask:=0;
      TimingEnabled:=False;
      Reset(False);

      if MaxBrowsePosition<>Top then
         while (Result<Position) and (Count<Top) and (Result>=0) do begin
           while (Count<Top) and ((Moves[Succ(Count)] and H_FLAG_BOX)=0) and (Result>=0) do
             if not (Redo0(False)) then Result:=-2; // locate next box-move
           if (Count<Top) and (Result<Position) and (Result>=0) then begin
              i:=H_FLAG_BOX+(Moves[Succ(Count)] and H_MASK_DIRECTION);
              repeat if not Redo0(False) then Result:=-2; // skip 'boxlines', i.e., boxes moving in a straight line
              until  (Count=Top) or (not BrowseChoicePoints) or ((Moves[Succ(Count)] and (H_FLAG_BOX+H_MASK_DIRECTION))<>i) or (Result<0);
              end;
           Inc(Result);
           end;

      while (Count>0) and (Count<Top) and // jump moves cannot be broken
        ((Moves[Count] and H_FLAG_JUMP)<>0) and (Redo0(False)) do;

      if Result<>Position then begin
         Position:=Max(0,Min(Position,Top));
         Reset(False);
         for i:=1 to Position do
             if Result>=0 then if not Redo0(False) then Result:=-1;
         Result:=Count;
         end;

      if SeparateUndoneMoves__ then SeparateUndoneMoves;

      if @ShowGame<>nil then ShowGame;

      if Self=MainForm.Game then begin
         if (Count=0) or (Count=Top) or
            (not MainForm.BtnUndo.Enabled) or
            (not MainForm.BtnRedo.Enabled) then
            MainForm.ShowStatus
         else begin // it's cheaper to update counters only
            MainForm.Status.MoveCount:=IntToStr(Count);
            MainForm.Status.PushCount:=IntToStr(PushCount);
            if Assigned(MainForm.MultiView.Selected) then with MainForm.MultiView.Selected do with Panels[mvipCaption] do
               ShowPanel(mvipCaption,Format(FORMAT_MOVES_AND_PUSHES,[History.Count,History.PushCount]),Enabled,Rect,Alignment);
            end;
         end;

    finally
      DeadlockDetection.Enabled:=oDeadlockDetectionEnabled;
      SimpleIllegalMovesMask:=oSimpleIllegalMovesMask;
      TimingEnabled:=oTimingEnabled;
      if Assigned(DeadlockDetection.Deadlocks) then
         DeadlockDetection.Deadlocks.LoadBoard;
    end;
end;

function  TGame.MakeNewFileName(const Path,FileNameStub:String; var FileName:String):Boolean;
begin
  FileName:=FileNameStub;
  if (FileName<>'') and (FileName[Length(FileName)]<>PERIOD) then
     FileName:=FileName+PERIOD;
  FileName:=Misc_.MakeNewFileName(
             StrWithTrailingPathDelimiter(Path)+FileName,
             SOKOBAN_FILE_NAME_EXT,
             True);
  Result:=FileName<>'';
end;

function  TGame.SaveToFile(FileName__:String; Flush__:Boolean):Boolean;
var s,PackFileName,SectionName:String; ExistingFileTime:TFileTime; //Level : TLevel;
begin
  StopTimer;
  PackFileName:=''; SectionName:='';
  try
    if IsAnIniFileSectionFileName(FileName__) then begin // save level to a collection
       PackFileName:=ExtractIniFileName(FileName__);
       SectionName :=ExtractSectionName(FileName__);
       if   SokoFile.Open(PackFileName) then begin
            SokoFile.AddFileFormatDescriptionToFiles:=MainForm.AddFileFormatDescriptionToFiles;
            Result:=SetName(SectionName) and
                    Inherited SaveToFile(SokoFile,True,True,True,True,Flush__);
            end
       else raise Exception.Create(Format(OpenFileFailedShortText__,[PackFileName]));
       end
    else begin                                           // save level to a single-level textfile
       if   StrEqual(FileName__,SokoFile.Name) then
            ExistingFileTime:=SokoFile.FileTime {remember existing file time}
       else begin ClearFileTime(ExistingFileTime);
                  ExistingFileTime.dwLowDateTime:=1;    // fake a non-zero file time which triggers a merge in case the file already exists
            end;
       SokoFile.AddFileFormatDescriptionToFiles:=MainForm.AddFileFormatDescriptionToFiles;
       if   SokoFile.New(FileName__) then begin
            SokoFile.FileTime:=ExistingFileTime;
            if not Notes.Lines.ReadString(KEY_TITLE,s) then s:=TEXT_LEVEL;
            Result:=SetName(s) and
                    Inherited SaveToFile(SokoFile,True,True,True,True,Flush__);
            end
       else raise Exception.Create(Format(OpenFileFailedShortText__,[FileName__]));
       end;
  except
    on E:Exception do begin
       LastErrorStr:=E.Message;
       if   PackFileName='' then s:=FileName__
       else s:=PackFileName;
       Result:=Error(PChar(Format(SaveFileFailedText__,[s])+NL+NL+
                     TEXT_FAILURE_DESCRIPTION+NL+NL+E.Message),
                     MainForm.SaveDialog1.Title+SUB_TITLE_SEPARATOR+ExtractFileName(s));
       end;
  end;

  if Result then begin
     {in the following, the game name may end up being wrong as the result of
      competing updates of the file from multiple instances of the application;
      it could be partially rectified by searching through the levels in the
      'SokoFile' collection for a level with a matching board, but it feels like
      overkill to do that; the good news is that apart from the name, all data
      is intact, also if the user proceeds from here by adding new solutions and
      snapshots to the wrongly named version of the level; using the file
      timestamps, the catch-updates-from-multiple-instances mechanism detects
      that these new snapshots and solutions should be added to the matching
      level in the file, no matter what the level name is;
     }
     if SokoFile.Name<>'' then
        if        SectionName<>'' then {'True': the level is a member of a collection}
                  FileName__:=MakeIniFileSectionFileName(SokoFile.Name,SectionName)
        else if   (SokoFile.Levels           <>nil) and
                  (SokoFile.Levels.Items     <>nil) and
                  (SokoFile.Levels.Items.Next<>nil) then begin {'True': the level is a member of a collection}
                  SectionName:=ChangeFileExt( ExtractFileName(FileName__),'');
                  FileName__:=MakeIniFileSectionFileName(SokoFile.Name,SectionName);
                  end
             else FileName__:=SokoFile.Name;
     FileName:=FileName__;
     LastValidFileName:=FileName;
     end;
  OpenForm.CurrentCollectionFileName:=''; // triggers an index-load next time
end;

function  TGame.MakeLegalMoves(var MoveCount:Integer; var Moves:TLegalMoves):Boolean;
var IsAFreezingMove:Boolean; d:TDirection;
begin
  MoveCount:=0;
  for d:=Low(d) to High(d) do begin
      if IsALegalMove(DIRECTION_XY[d,ColAxis],DIRECTION_XY[d,RowAxis],0,IsAFreezingMove) then begin
         Inc(MoveCount);
         Moves[MoveCount].x:=PlayerPos.x+DIRECTION_XY[d,ColAxis];
         Moves[MoveCount].y:=PlayerPos.y+DIRECTION_XY[d,RowAxis];
         end;
      end;
  Result:=MoveCount<>0;
end;

function  TGame.MoveToStrChessNotation(const FromPos,ToPos:TColRow; BoxNo:Integer):String;
var Ch:Char;
begin
  if BoxNo=0 then Result:='-'
  else Result:='*';
  if   FromPos.x>Succ(Ord('Z')-Ord('A')) then
       Ch:=Chr(Ord('A')+Pred(FromPos.x)-Succ(Ord('Z')-Ord('A')))
  else Ch:=Chr(Ord('a')+Pred(FromPos.x));
  Result:=Ch+IntToStr(BoardHeight-FromPos.y+1)+Result;
  if   ToPos.x>Succ(Ord('Z')-Ord('A')) then
       Ch:=Chr(Ord('A')+Pred(ToPos  .x)-Succ(Ord('Z')-Ord('A')))
  else Ch:=Chr(Ord('a')+Pred(ToPos.x));
  Result:=Result+Ch+IntToStr(BoardHeight-ToPos.y+1);
end;

function  TGame.StrChessNotationToMove(const s:String; var FromPos,ToPos:TColRow; BoardWidth,BoardHeight:Integer):Boolean;
var i:Integer; Ch:Char;
begin
  Result:=(Length(s)>=5) and (Length(s)<=7);
  if Result then begin
     Ch:=s[1];
     FromPos.x:=Succ(Ord(UpCase(Ch))-Ord('A'));
     if (BoardWidth>Succ(Ord('Z')-Ord('A'))) and
        (Ch>='A') and (Ch<='Z') then
        Inc(FromPos.x,Succ(Ord('Z')-Ord('A')));
     FromPos.y:=Ord(s[2])-Ord('0');
     if   (s[3]>='0') and (s[3]<='9') then begin
          FromPos.y:=FromPos.y*10+(Ord(s[3])-Ord('0'));
          i:=4;
          end
     else i:=3;
     FromPos.y:=Succ(BoardHeight)-FromPos.y;
     Result:=(s[i]='-') or (s[i]='*');
     if Result then begin
        Inc(i); Ch:=s[i];
                ToPos.x:=Succ(Ord(UpCase(Ch))-Ord('A'));
                if (BoardWidth>Succ(Ord('Z')-Ord('A'))) and
                   (Ch>='A') and (Ch<='Z') then
                   Inc(ToPos.x,Succ(Ord('Z')-Ord('A')));
        Inc(i); ToPos.y:=Ord(s[i])-Ord('0');
        Inc(i);
        if (i<=Length(s)) and (s[i]>='0') and (s[i]<='9') then begin
          ToPos.y:=ToPos.y*10+(Ord(s[i])-Ord('0'));
          Inc(i);
          end;
        ToPos.y:=Succ(BoardHeight)-ToPos.y;
        Result:=(i=Succ(Length(s)))                   and
                (FromPos.x>=1) and (FromPos.x<=BoardWidth ) and
                (FromPos.y>=1) and (FromPos.y<=BoardHeight) and
                (ToPos  .x>=1) and (ToPos  .x<=BoardWidth ) and
                (ToPos  .y>=1) and (ToPos  .y<=BoardHeight) and
                (Abs(ToPos.x-FromPos.x+ToPos.y-FromPos.y)=1);
        end;
     end;
end;

procedure TGame.CalculateInternalData;

  procedure CalculateBTSquares;
  // Calculates the set of squares which either contain a wall, or might be reachable for the player,
  // i.e., the 'background squares' used for masking the board onto the background image;
  // At the same time it calculates the board topology information
  var i,j,k:Integer;
  begin
    FillChar(BTSquare,SizeOf(BTSquare),0); BTSquareCount:=0;
    for i:=0 to BoardWidth+1 do
        for j:=0 to BoardHeight+1 do
            if   (i=0) or (i>BoardWidth) or
                 (j=0) or (j>BoardHeight) then
                 BTSquare[i,j]:=BT_SQUARE
            else if (Board[i,j] and (WALL+FLOOR))<>0 then begin
                    BTSquare[i,j]:=BT_SQUARE; Inc(BTSquareCount);
                    end;

    for i:=1 to BoardWidth do
        for j:=1 to BoardHeight do
            if BTSquare[i,j]<>0 then begin
               if (Board[i,j] and WALL)<>0 then begin
                  k:=BT_WALL_NO_NEIGHBOR_WALLS;
                  if (i<>1          ) and ((Board[i-1,  j] and WALL)<>0) then Inc(k,BT_WALL_TO_THE_LEFT );
                  if (i<>BoardWidth ) and ((Board[i+1,  j] and WALL)<>0) then Inc(k,BT_WALL_TO_THE_RIGHT);
                  if (j<>1          ) and ((Board[i  ,j-1] and WALL)<>0) then Inc(k,BT_WALL_ABOVE       );
                  if (j<>BoardHeight) and ((Board[i  ,j+1] and WALL)<>0) then Inc(k,BT_WALL_BELOW       );
                  if (i>1) and  (j>1) and
                     ((k and (BT_WALL_ABOVE+BT_WALL_TO_THE_LEFT)=(BT_WALL_ABOVE+BT_WALL_TO_THE_LEFT))) and
                     ((BTSquare[Pred(i),Pred(j)] and (BT_WALL_TO_THE_RIGHT+BT_WALL_BELOW))
                      =
                      (BT_WALL_TO_THE_RIGHT+BT_WALL_BELOW)
                     ) then Inc(k,BT_WALL_CAP);

                  Inc(BTSquare[i,j],k);
                  end;
               k:=0;
               if (BTSquare[i-1,j  ]=0) or (i=1          ) then begin
                  Inc(k,BT_LEFT  );
                  if (i>1) and (j<BoardHeight) and ((BTSquare[i-1,j+1] and BT_SQUARE)<>0) then k:=k or BT_DENT_BOTTOM_LEFT;
                  if (i>1) and (j>1          ) and ((BTSquare[i-1,j-1] and BT_SQUARE)<>0) then k:=k or BT_DENT_TOP_LEFT;
                  end;
               if (BTSquare[i  ,j-1]=0) or (j=1          ) then begin
                  Inc(k,BT_TOP   );
                  if (i>1)          and (j>1)  and ((BTSquare[i-1,j-1] and BT_SQUARE)<>0) then k:=k or BT_DENT_TOP_LEFT;
                  if (i<BoardWidth) and (j>1)  and ((BTSquare[i+1,j-1] and BT_SQUARE)<>0) then k:=k or BT_DENT_TOP_RIGHT;
                  end;
               if (BTSquare[i+1,j  ]=0) or (i=BoardWidth ) then begin
                  Inc(k,BT_RIGHT );
                  if (i<BoardWidth) and (j>1)           and ((BTSquare[i+1,j-1] and BT_SQUARE)<>0) then k:=k or BT_DENT_TOP_RIGHT;
                  if (i<BoardWidth) and (j<BoardHeight) and ((BTSquare[i+1,j+1] and BT_SQUARE)<>0) then k:=k or BT_DENT_BOTTOM_RIGHT;
                  end;
               if (BTSquare[i  ,j+1]=0) or (j=BoardHeight) then begin
                  Inc(k,BT_BOTTOM);
                  if (i>1)          and (j<BoardHeight) and ((BTSquare[i-1,j+1] and BT_SQUARE)<>0) then k:=k or BT_DENT_BOTTOM_LEFT;
                  if (i<BoardWidth) and (j<BoardHeight) and ((BTSquare[i+1,j+1] and BT_SQUARE)<>0) then k:=k or BT_DENT_BOTTOM_RIGHT;
                  end;
               Inc(BTSquare[i,j],k);
               end;

    for i:=1 to BoardWidth do
        for j:=1 to BoardHeight do
            if (BTSquare[i,j] and BT_DENT)<>0 then begin
               k:=BTSquare[i,j];
               if ((k and BT_DENT_TOP_LEFT)<>0) and
                  (i>1) and (Board[i-1,j]=WALL) then begin
//                ((BTSquare[i-1,j] and (BT_DENT))=0) then begin
                  BTSquare    [i-1,j]:=BTSquare[i-1,j] or BT_CORNER_TOP_RIGHT;
                  if (BTSquare[i-1,j] and BT_LEFT)<>0 then
                      BTSquare[i-1,j]:=BTSquare[i-1,j] or (BT_CORNER_TOP_LEFT+BT_CORNER_BOTTOM_LEFT);
                  end;
               if ((k and BT_DENT_TOP_RIGHT)<>0) and
                  (i<BoardWidth) and (Board[i+1,j]=WALL) then begin
//                  ((BTSquare[i+1,j] and (BT_DENT))=0) then begin
                  BTSquare    [i+1,j]:=BTSquare[i+1,j] or BT_CORNER_TOP_LEFT;
                  if (BTSquare[i+1,j] and BT_RIGHT)<>0 then
                      BTSquare[i+1,j]:=BTSquare[i+1,j] or (BT_CORNER_TOP_RIGHT+BT_CORNER_BOTTOM_RIGHT);
                  end;
               if ((k and BT_DENT_BOTTOM_LEFT)<>0) and
                  (i>1) and (Board[i-1,j]=WALL) then begin
//                  ((BTSquare[i-1,j] and (BT_DENT))=0) then begin
                  BTSquare    [i-1,j]:=BTSquare[i-1,j] or BT_CORNER_BOTTOM_RIGHT;
                  if (BTSquare[i-1,j] and BT_LEFT)<>0 then
                      BTSquare[i-1,j]:=BTSquare[i-1,j] or (BT_CORNER_TOP_LEFT+BT_CORNER_BOTTOM_LEFT);
                  end;
               if ((k and BT_DENT_BOTTOM_RIGHT)<>0) and
                  (i<BoardWidth) and (Board[i+1,j]=WALL) then begin
//                  ((BTSquare[i+1,j] and (BT_DENT))=0) then begin
                  BTSquare    [i+1,j]:=BTSquare[i+1,j] or BT_CORNER_BOTTOM_LEFT;
                  if (BTSquare[i+1,j] and BT_RIGHT)<>0 then
                      BTSquare[i+1,j]:=BTSquare[i+1,j] or (BT_CORNER_TOP_RIGHT+BT_CORNER_BOTTOM_RIGHT);
                  end;
               end;
{
    // floodfill exterior non-floor empty squares as preparation for marking
    // the interior non-floor empty squares, if any
    for i:=1 to BoardWidth do begin // check squares on top row and bottom row
        if (Board[i,1] and (WALL+FLOOR+PLAYER_LEGAL_MOVE))=0 then // check squares in the top row
           PlayerLegalMoves(i,1,j,j); // floodfill connected unreachable floor-squares, marking them with the 'PLAYER_LEGAL_MOVE' flag, starting from the selected square
         if (Board[i,BoardHeight] and (WALL+FLOOR+PLAYER_LEGAL_MOVE))=0 then  // check squares in the bottom row
            PlayerLegalMoves(i,BoardHeight,j,j); // floodfill connected unreachable floor-squares, marking them with the 'PLAYER_LEGAL_MOVE' flag, starting from the selected square
         end;
     for i:=1 to BoardHeight do begin // check squares on leftmost column and rightmost column
         if (Board[1,i] and (WALL+FLOOR+PLAYER_LEGAL_MOVE))=0 then // check squares in the leftmost column
            PlayerLegalMoves(1,i,j,j); // floodfill connected unreachable floor-squares, marking them with the 'PLAYER_LEGAL_MOVE' flag, starting from the selected square
         if (Board[BoardWidth,i] and (WALL+FLOOR+PLAYER_LEGAL_MOVE))=0 then // check squares in the rightmost column
            PlayerLegalMoves(BoardWidth,i,j,j); // floodfill connected unreachable floor-squares, marking them with the 'PLAYER_LEGAL_MOVE' flag, starting from the selected square
         end;

     // mark interior non-floor empty squares, if any
     for i:=1 to BoardWidth do
         for j:=1 to BoardHeight do begin
             if (Board[i,j] and (WALL+FLOOR+PLAYER_LEGAL_MOVE))=0 then // 'True': the square is an interior non-floor empty square, i.e., it's neither a wall nor a floor
                Inc(BTSquare[i,j],BT_INTERIOR_NON_FLOOR_EMPTY_SQUARE);
             Board[i,j]:=Board[i,j] and (not PLAYER_LEGAL_MOVE); // reset board flags
             end;
}
  end;

begin // CalculateInternalData;
  Inherited;
  if   (MainForm<>nil) and MainForm.DrawGrid1.Visible then // 'True': old code which most likely doesn't work anymore
       PlayerCanReachTheBorder:=CalculateIfPlayerCanReachTheBorder
  else CalculateBTSquares;
end;

function  TGame.CalculateIfPlayerCanReachTheBorder:Boolean;
var i,j,oBoxCount:Integer;
begin
  Result:=False; oBoxCount:=BoxCount;
  try     BoxCount:=0; // quick and dirty: ignore boxes in 'CalculatePlayerReachableSquares'
          PlayerLegalMoves(0,0,i,j);

          for i:=1 to BoardWidth do
              if      (Board[i,1] and PLAYER_LEGAL_MOVE)<>0 then begin
                      Result:=True; break;
                      end
              else if (Board[i,BoardHeight] and PLAYER_LEGAL_MOVE)<>0 then begin
                      Result:=True; break;
                      end;
          if not Result then
             for i:=1 to BoardHeight do
                 if      (Board[1,i] and PLAYER_LEGAL_MOVE)<>0 then begin
                         Result:=True; break;
                         end
                 else if (Board[BoardWidth,i] and PLAYER_LEGAL_MOVE)<>0 then begin
                         Result:=True; break;
                         end;
  finally BoxCount:=oBoxCount;
  end;
end;

function  TGame.CanCombineCurrentPositionAndSnapshotToFormASolution(Snapshot:TSnapshot):Boolean;
var MoveCount,PlayerLineCount,InitialJumpsA,InitialJumpsB,Col,Row:Integer; IsPreferredDirectionOK:Boolean;
begin
  Result:=(Snapshot<>nil) and
          (Snapshot.ReverseMode=not ReverseMode) and
          (Snapshot.MoveCount>0) and
          (History.Count>0) and
//        IsIdenticalBoard(Snapshot.Board,False,False) and
          IsIdenticalBoxPosition(Snapshot.PlayerPos,Snapshot.BoxPos,False,False) and
          (PlayerLegalMoves(0,0,Col,Row)>=1) and
          ((Board[Snapshot.PlayerPos.x,Snapshot.PlayerPos.y] and PLAYER_LEGAL_MOVE)<>0) and
          PlayerPath(PlayerPos,Snapshot.PlayerPos,False,History.Count>0,TDirection(History.Moves[History.Count] and H_MASK_DIRECTION),MoveCount,PlayerLineCount,IsPreferredDirectionOK,nil) and
          (not IsJumpFeatureUsedAfterFirstBoxMove(ReverseMode         ,History.Count     ,Addr(History.Moves),InitialJumpsA)) and
          (not IsJumpFeatureUsedAfterFirstBoxMove(Snapshot.ReverseMode,Snapshot.MoveCount,Snapshot.Moves     ,InitialJumpsB)) and
          (History.Count+Snapshot.MoveCount+MoveCount-InitialJumpsA-InitialJumpsB<=MAX_MOVES);
end;

function  TGame.IsIdleAndStopReplayingAndBrowsing:Boolean;
begin
  Result:=not (IsBusy or IsReplaying or IsBrowsing);
  if not Result then
     if      IsReplaying  then UserBreakWhileReplaying:=True
     else if IsBrowsing   then LeaveBrowseMode(True);
end;

procedure TGame.SaveBookmark;
begin // not in production anymore; superseded by the newer snapshot feature
  if BookmarkCount<MAX_BOOKMARKS then
     Bookmark[Succ(BookmarkCount)]:=History.Count;
end;

procedure TGame.RestoreBookmark;
var IsAFreezingMove:Boolean; i,dx,dy,Move,MoveNo:Integer;
begin // not in production anymore; superseded by the newer snapshot feature
  MoveNo:=Bookmark[BookmarkCount];
  if   (BookmarkCount>0) and
       (MoveNo>ForcedInitialJumps) and
       (MoveNo<=History.Count) and
       (not IsBusy) and (not IsReplaying) and (not IsBrowsing) and
       (@ShowGame<>nil) then with History do
       try
         IsBusy:=True; UpdateMultiNoEnabled:=False;
         if (MainForm.Sound.Enabled) and (MainForm.Sound.Player[Sound_.stRestartGame]<>nil) then
            MainForm.Sound.Play(Sound_.stRestartGame);

         Reset(False);
         for i:=Count to MoveNo do begin
             Move:=Moves[i]; HistoryMoveToDxDy(Move,dx,dy);
             if   IsALegalMove(dx,dy,Move,IsAFreezingMove) then
                  DoMoveUpdateGame(dx,dy,-1,Move)
             else break;
             end;
         ShowGame;
       finally
         SeparateUndoneMoves;
         IsBusy:=False;
       end
  else MainForm.BtnResetClick(nil);
end;

function TGame.MakeSnapshot(const Name:String):TSnapshot;
var s:String;
begin
  s:=Name;
  if (s='') and (SnapshotsForm<>nil) then begin // blank name: make a default name
     if        GameState=gsSolved then
               if   ReverseMode then
                    s:=TEXT_REVERSE_MODE+SPACE+TEXT_SOLUTION // there is no user-defined text for this one
               else s:=SnapshotTypeName(SokFile_.stSolution)
     else if   ReverseMode then
               s:=SnapshotTypeName(stReverseSnapshot)
          else s:=SnapshotTypeName(stSnapshot);
     s:=Snapshots.MakeUniqueName(s+SPACE+
                                 Format(FORMAT_MOVES_AND_PUSHES,[History.Count,History.PushCount])+
                                 SecondaryMetricsAsText,
                                 Name,True);
     end;
  Result:=Inherited MakeSnapshot(s);
  MainForm.MakeDynamicHints(nil);
end;

procedure TGame.DeleteSnapshot(var Snapshot:TSnapshot);
begin
  if Assigned(MainForm) and (Self=MainForm.Game) and Assigned(SnapshotsForm) and Assigned(Snapshot) then
     with SnapshotsForm do with Grid do
       if Snapshot=Snapshots[Row] then begin
          Cells[1,Row]:=''; Snapshots[Row]:=nil;
          end;
  Inherited;
  if Assigned(MainForm) then MainForm.MakeDynamicHints(nil);
end;

procedure TGame.LoadBoard(const Board__:TBoard);
begin
  Inherited;
  if Assigned(DeadlockDetection.Deadlocks) then
     DeadlockDetection.Deadlocks.LoadBoard;
end;

function  TGame.LoadSnapshot(Snapshot:TSnapshot):Boolean;
begin // note: this procedure does not display the loaded snapshot; normally this is handled by 'MainForm'
  Result:=(Snapshot<>nil) and
          (not IsBusy) and (not IsReplaying) and (not IsBrowsing);
  if Result then
     try     IsBusy     :=True;
             Result     :=Inherited LoadSnapshot(Snapshot);
     finally IsBusy:=False;
     end;
end;

function  TGame.IsIdenticalBoxPosition(PlayerPos__:TColRow; const BoxPos__:TBoxPositions; BoxNumbersMustMatch__,PlayerPositionMustMatch__:Boolean):Boolean;
var i,BoxNo:Integer;
begin
  Result:=(not PlayerPositionMustMatch__) or
          ((PlayerPos.X=PlayerPos__.X) and (PlayerPos.Y=PlayerPos__.Y));

  if   Result then
       for BoxNo:=1 to BoxCount do
           if   Result then begin
                Result:=(BoxPos[BoxNo].X=BoxPos__[BoxNo].X) and
                        (BoxPos[BoxNo].Y=BoxPos__[BoxNo].Y);
                if (not Result) and (not BoxNumbersMustMatch__) then
                   for i:=1 to BoxCount do
                       if   not Result then
                            Result:=(BoxPos[BoxNo].X=BoxPos__[i].X) and
                                    (BoxPos[BoxNo].Y=BoxPos__[i].Y)
                       else break;
                end
           else break;
end;

function  TGame.CombineCurrentPositionAndSnapshotToFormASolution(Snapshot:TSnapshot):Boolean;
const METHOD_NAME='TGame.Concatenate Current Position and Snapshot to Form a Solution';
var i,NormalMoveCount,oSimpleIllegalMovesMask:Integer; p1,p2,p3,p4:TColRow;
    H:THistory;

  function  AddPlayerMoves(const FromPos,ToPos:TColRow):Boolean;
  var i,MoveCount,PlayerLineCount,LastMoveIndex:Integer; IsPreferredDirectionOK:Boolean; Direction:TDirection; Moves:TPlayerMoves;
  begin
    Result:=True;
    if PlayerPath(FromPos,ToPos,False,H.Count>0,TDirection(H.Moves[H.Count] and H_MASK_DIRECTION),MoveCount,PlayerLineCount,IsPreferredDirectionOK,PPlayerMoves(Addr(Moves))) then begin // to do: calculate the player path with secondary optimization of the number of player lines
       LastMoveIndex:=H.Count; Moves[0]:=FromPos;
       for i:=1 to MoveCount do with Moves[i] do
           if   DxDyToDirection(x-Moves[Pred(i)].x,y-Moves[Pred(i)].y,Direction) then
                if    (H.Count<MAX_MOVES) then begin
                      Inc(H.Count);
                      H.Moves[H.Count]:=Ord(Direction)+(H_FLAG_ODD and (not H.Moves[LastMoveIndex]));
                      end
                else  Result:=False
           else raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,[METHOD_NAME+' (A)']));
       end
  else raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,[METHOD_NAME+' (B)']));
  end;

begin // Concatenate Current Position And Snapshot To Form A Solution
      // Precondition: Concatenation must be legal
  oSimpleIllegalMovesMask:=SimpleIllegalMovesMask;
  try
    IsBusy:=True; H.Moves[0]:=0;
    SimpleIllegalMovesMask:=0;
    if ReverseMode then begin
       for i:=1 to Snapshot.MoveCount do H.Moves[i]:=Snapshot.Moves[i];         // normal moves
       H.Count:=Snapshot.MoveCount; NormalMoveCount:=H.Count;
       Result:=AddPlayerMoves(Snapshot.PlayerPos,PlayerPos) and                 // connect games
               AppendReverseModeSolution(History.Count,Addr(History.Moves),NormalMoveCount,H); // add reverse mode moves
       end
    else begin
       for i:=1 to History.Count do H.Moves[i]:=History.Moves[i];               // normal moves
       H.Count:=History.Count; NormalMoveCount:=H.Count;
       Result:=AddPlayerMoves(PlayerPos,Snapshot.PlayerPos) and                 // connect games
               AppendReverseModeSolution(Snapshot.MoveCount,Snapshot.Moves,NormalMoveCount,H); // add reverse mode moves
       end;

    if Result then with History do begin
       History:=H;
       Count:=0; PushCount:=0; LastPushIndex:=0; LastBoxNo:=0;
       SetReverseMode(False);
       LoadBoard(StartBoard);

       while Result and (Count<Top) do
         Result:=Redo0(True); // note: not 'Self.Redo', i.e., the moves are not shown

       MainForm.MakeDynamicHints(nil);
       if @ShowGame<>nil then ShowGame;

       if Count<>NormalMoveCount then begin // play sound for last move:
          UnpackLastMove(p1,p2,p3,p4,i);
          MainForm.ShowMoveInstantly(i,Count<NormalMoveCount,(Moves[Count] and H_FLAG_JUMP)<>0,True);
          end;

       if (GameState=gsSolved) and MainForm.Sound.Enabled then
          MainForm.Sound.Play(stSolution);
       end;
  finally
    IsBusy:=False;
    SimpleIllegalMovesMask:=oSimpleIllegalMovesMask; // restore filter mask
    if Assigned(DeadlockDetection.Deadlocks) then
       DeadlockDetection.Deadlocks.LoadBoard;
  end;
end;

function  TGame.SnapshotType(Snapshot__:TSnapshot):TSnapshotType;
begin
  if   Snapshot__<>nil then
       if    Snapshot__.GameState=gsSolved then
             if        Snapshot__=BestSolutionMoves then
                       if   BestSolutionPushes=nil then
                            Result:=stBestSolution
                       else Result:=stBestSolutionMoves
             else if   Snapshot__=BestSolutionPushes then
                       if   BestSolutionMoves=nil then
                            Result:=stBestSolution
                       else Result:=stBestSolutionPushes
                  else Result:=SokFile_.stSolution
       else  Result:=stSnapshot
  else raise Exception.Create(InternalErrorText);
end; {TGame.SnapshotType}

function TGame.SnapshotTypeName(SnapshotType__:TSnapshotType):String;
begin
  Result:='';
  if SnapshotsForm<>nil then
     case SnapshotType__ of
       stSnapshot           : Result:=SnapshotsForm.NormalModeSnapshotName;
       stReverseSnapshot    : Result:=SnapshotsForm.ReverseModeSnapshotName;
       SokFile_.stSolution  : Result:=SnapshotsForm.SolutionName;
       stBestSolution       : Result:=SnapshotsForm.BestSolutionName;
       stBestSolutionMoves  : Result:=SnapshotsForm.BestSolutionMovesName;
       stBestSolutionPushes : Result:=SnapshotsForm.BestSolutionPushesName;
       else                   Result:=SNAPSHOT_TYPE_NAME[SnapshotType__];
     end; {case}
  if Result='' then Result:=SNAPSHOT_TYPE_NAME[SnapshotType__];
end; {TSokoGame.SnapshotTypeName}

procedure TGame.StartTimer;
begin
  Inherited;
  if (StartTimeMS<>0) and Assigned(MainForm) and (Self=MainForm.Game) then
     MainForm.Status.StartTimer;
end; {TGame.StartTimer}

procedure TGame.StopTimer;
begin
  Inherited;
  if Assigned(MainForm) and (Self=MainForm.Game) then MainForm.Status.StopTimer;
end; {TSokoGame.StopTimer}

function TGame.HasModifiedSnapshots:Boolean;
var v:TSnapshot;
begin
  Result:=False;
  if Snapshots<>nil then v:=TSnapshot(Snapshots.First)
  else v:=nil;
  while (v<>nil) and (not Result) do begin
    Result:=TSnapshot(v).Modified;
    v:=TSnapshot(v.Next);
    end;
end;

function  TGame.HuffmanBase64EncodedBoard:String;
var
  HuffmanEncodedBoardByteSize:Integer; HuffmanEncodedBoard:TBoardOfBytes1D;

begin // HuffmanBase64EncodedBoard
  Result:='';
  if HuffmanEncodeBoard(HuffmanEncodedBoard,HuffmanEncodedBoardByteSize) then
     try    if   Base64Encode(PByte(Addr(HuffmanEncodedBoard)),HuffmanEncodedBoardByteSize,Result,HUFFMAN_BASE64_ENCODING_LAST_2_CHARACTERS) then
            else raise Exception.Create(TEXT_TASK_FAILED);
     except on E:Exception do begin
            Error(E.Message,TEXT_APPLICATION_TITLE);
            Result:='';
            end;
     end;
end;

function  TGame.HuffmanDecodeBoard(Buffer__:PByte; BufferByteSize__:Integer):String;
{ Huffman tree
    /\
   1  0
  /    \
[-]    /\
 f    1  0
 l   /    \
 o [#]    /\
 o  w    /  \
 r  a   1    0
    l  /      \
    l /        \
     /\        /\
    1  0      1  0
   /    \    /    \
 [$]   [.] [\n]   /\
 box    g  new-  1  0
        o  line /    \
        a      /\    [*]
        l     1  0   box on goal
             /    \
           [@]    [+]
        player    player on goal
}
type
  TNode=packed record Left,Right:Byte; Value:Char; end;
const
  Nodes:array[0..14] of TNode = (
    { 0} (Left: 1; Right: 2; Value:NULL_CHAR     ),
    { 1} (Left: 0; Right: 0; Value:FLOOR_CH      ),
    { 2} (Left: 3; Right: 4; Value:NULL_CHAR     ),
    { 3} (Left: 0; Right: 0; Value:WALL_CH       ),
    { 4} (Left: 5; Right: 8; Value:NULL_CHAR     ),
    { 5} (Left: 6; Right: 7; Value:NULL_CHAR     ),
    { 6} (Left: 0; Right: 0; Value:BOX_CH        ),
    { 7} (Left: 0; Right: 0; Value:GOAL_CH       ),
    { 8} (Left: 9; Right:10; Value:NULL_CHAR     ),
    { 9} (Left: 0; Right: 0; Value:LF            ),
    {10} (Left:11; Right:14; Value:NULL_CHAR     ),
    {11} (Left:12; Right:13; Value:NULL_CHAR     ),
    {12} (Left: 0; Right: 0; Value:PLAYER_CH     ),
    {13} (Left: 0; Right: 0; Value:PLAYER_GOAL_CH),
    {14} (Left: 0; Right: 0; Value:BOX_GOAL_CH   )
  );
  BYTE_HIGH_BIT:Integer=Succ(High(Byte)) div 2;
var
  Index,BitCount,ByteValue:Integer; CurrentChar,LastChar:Char;
begin
  try
    Result:=''; ByteValue:=0; BitCount:=0; LastChar:=NULL_CHAR;
    while (BufferByteSize__<>0) or (BitCount<>0) do begin
      Index:=0; // get ready for the next character, i.e., start at the top node of the decoding tree
      repeat
        if   (BitCount=0) and (BufferByteSize__<>0) then begin
             ByteValue:=Buffer__^; BitCount:=BITS_PER_BYTE;
             Inc(Buffer__); Dec(BufferByteSize__);
             end;
        if   (ByteValue and BYTE_HIGH_BIT)<>0 then // check the high-bit of the currently processed byte
             Index:=Nodes[Index].Left
        else Index:=Nodes[Index].Right;
        ByteValue:=ByteValue shl 1; Dec(BitCount); // advance to the next bit, if any, in the currently processed byte
      until Nodes[Index].Left=0; // repeat until a leaf node has been reached

      CurrentChar:=Nodes[Index].Value;
      if CurrentChar<>LF then
         Result:=Result+CurrentChar
      else begin
         Result:=Result+NL;
         if LastChar=LF then begin // 2 "NewLine" in a row: end-of-data
            BufferByteSize__:=0; BitCount:=0; // exit loop
            end;
         end;
      LastChar:=CurrentChar;
      end;
  except on E:Exception do begin
            Error(E.Message,Application.Title);
            Result:='';
            end;
  end;
end;

function  TGame.HuffmanEncodeBoard(var HuffmanEncodedBoard__:TBoardOfBytes1D; var HuffmanEncodedBoardByteSize__:Integer):Boolean;
{
Huffman encoding
-----------------
     1 - '-'  Floor
    01 - '#'  Wall
  0011 - '$'  Box
  0010 - '.'  Target
  0001 -  \n  NewLine
 00000 - '*'  Box on Target
000011 - '@'  Player
000010 - '+'  Player on Target

end of data: 2 "NewLine" characters, i.e., "0001 0001"
}
var
  Row,CurrentBitCount,CurrentByteValue,Index,LastColumn:Integer;
  BoardAsText:String;
  HuffmanCodes,HuffmanCodesBitCounts:array[Char] of Integer;

  procedure Emit(Value__,BitCount__:Integer);
  begin
    CurrentByteValue:=(CurrentByteValue shl BitCount__) or Value__;
    Inc(CurrentBitCount,BitCount__);
    if CurrentBitCount>BITS_PER_BYTE then begin
       Dec(CurrentBitCount,BITS_PER_BYTE);
       HuffmanEncodedBoard__[HuffmanEncodedBoardByteSize__]:=CurrentByteValue shr CurrentBitCount;
       CurrentByteValue:=CurrentByteValue xor (HuffmanEncodedBoard__[HuffmanEncodedBoardByteSize__] shl CurrentBitCount);
       Inc(HuffmanEncodedBoardByteSize__);
       end;
  end;

begin // HuffmanEncodeBoard
  try
    FillChar(HuffmanCodes         ,SizeOf(HuffmanCodes         ),0);
    FillChar(HuffmanCodesBitCounts,SizeOf(HuffmanCodesBitCounts),0);
    HuffmanCodes[FLOOR_CH          ]:=1; HuffmanCodesBitCounts[FLOOR_CH          ]:=1;
    HuffmanCodes[WALL_CH           ]:=1; HuffmanCodesBitCounts[WALL_CH           ]:=2;
    HuffmanCodes[BOX_CH            ]:=3; HuffmanCodesBitCounts[BOX_CH            ]:=4;
    HuffmanCodes[GOAL_CH           ]:=2; HuffmanCodesBitCounts[GOAL_CH           ]:=4;
    HuffmanCodes[BOX_GOAL_CH       ]:=0; HuffmanCodesBitCounts[BOX_GOAL_CH       ]:=5;
    HuffmanCodes[PLAYER_CH         ]:=3; HuffmanCodesBitCounts[PLAYER_CH         ]:=6;
    HuffmanCodes[PLAYER_GOAL_CH    ]:=2; HuffmanCodesBitCounts[PLAYER_GOAL_CH    ]:=6;
    HuffmanCodes[LF                ]:=1; HuffmanCodesBitCounts[LF                ]:=4;

    BoardAsText:=SokFile_.BoardToText(BoardWidth,BoardHeight,Board,''); // for added flexibility, the function operates on the current game board and not the game starting position

    if   Length(BoardAsText)=BoardWidth*BoardHeight then begin
         HuffmanEncodedBoardByteSize__:=0; CurrentByteValue:=0; CurrentBitCount:=0;

         for Row:=1 to BoardHeight do begin
             Index:=Succ(Pred(Row)*BoardWidth); // first square (character) in row
             LastColumn:=Index+Pred(BoardWidth); // find last filled square (character) in row
             while (LastColumn>=Index) and (BoardAsText[LastColumn]=FLOOR_CH) do Dec(LastColumn);
             while Index<=LastColumn do begin
               Emit(HuffmanCodes[BoardAsText[Index]],HuffmanCodesBitCounts[BoardAsText[Index]]);
               Inc(Index);
               end;
             Emit(HuffmanCodes[LF],HuffmanCodesBitCounts[LF]); // add row terminator
             end;
         Emit(HuffmanCodes[LF],HuffmanCodesBitCounts[LF]); // add end-of-data
         if  CurrentBitCount<>0 then Emit(0,BITS_PER_BYTE); // emit pending bits, if any
         Result:=True;
         end
    else raise Exception.Create(TEXT_TASK_FAILED);

  except on E:Exception do Result:=Error(E.Message,TEXT_APPLICATION_TITLE);
  end;
end;

function  TGame.LoadFromFileOrClipboard(const FileName__:String; DestGame__:TGame; MultiViewItem__:TMultiViewItem; var Modified__:Boolean):Boolean;
var i,OriginalSimpleIllegalMovesMask:Integer;
    s,s1,NewFileName:String; VReverseMode:Boolean;
    VHistory:THistory; Level:TLevel; Snapshot,V,V2:TSnapshot; oCursor:TCursor;

  function  Open(var FileName:String; var Level:TLevel; var ErrorStr:String; DestGame__:TGame):Boolean;
  var i:Integer; s,PackFileName,SectionName:String; n:TNode;
  begin
    Level:=nil; ErrorStr:='';
    if IsBlank(FileName) then begin
       if   Clipboard.HasFormat(CF_TEXT) then begin
            SokUtil_.LastErrorText:='';
            if   SokoFile.Open('') then begin
                 if SokoFile.Levels.IsEmpty then
                    ErrorStr:=NoLevelsFoundText
                 else begin
                    if   SokoFile.Levels.Count>1 then begin
                         PackFileName:='';
                         if      SokoFile.FileHeader.Lines.ReadString(KEY_COLLECTION,PackFileName) then
                                 PackFileName:=StrToFileName(ExtractFileName(PackFileName))
                         else if SokoFile.FileHeader.Lines.ReadString(KEY_SET,PackFileName) then
                                 PackFileName:=StrToFileName(ExtractFileName(PackFileName))
                         else if SokoFile.FileHeader.Lines.ReadString(KEY_TITLE,PackFileName) then
                                 PackFileName:=StrToFileName(ExtractFileName(PackFileName));
                         if (PackFileName<>'') and
                            (not StrEqual(ExtractFileExt(PackFileName),SOKOBAN_FILE_NAME_EXT)) and
                            (not StrEqual(ExtractFileExt(PackFileName),XSB_FILE_NAME_EXT)) then
                            PackFileName:=PackFileName+SOKOBAN_FILE_NAME_EXT;

                         if PackFileName='' then begin
                            // no file name has been found for the collection;
                            // if all non-blank lines in the file header
                            // begins with ";" then use the first of them as
                            // file name;
                            // this is not a recommended convention, but some
                            // files uses it;
                            n:=SokoFile.FileHeader.Lines.First;
                            while Assigned(n)
                                  and
                                  ( (n.Text='')
                                    or
                                    (n.Text[STRING_BASE]=SEMICOLON)
                                  ) do begin
                                  if (n.Text<>'')
                                     and
                                     (PackFileName='') and
                                     (not SafeStrToInt(Trim(Copy(n.Text,2,Length(n.Text))),False,i))
                                     then
                                     PackFileName:=Trim(Copy(n.Text,STRING_BASE+1,Length(n.Text)));
                                  n:=n.Next;
                                  end;
                            if Assigned(n) then PackFileName:=''; // 'True': some lines don't begin with a ";"
                            end;

                         PackFileName:=StrRemoveCharacters(PackFileName,TITLE_ILLEGAL_CHARACTERS);
                         if PackFileName='' then
                            PackFileName:=TEXT_LEVELS+SOKOBAN_FILE_NAME_EXT;
                         end
                    else PackFileName:=TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER;
                    // find a directory for the new file
                    s:=Self.FileName;
                    if (s='') and (DestGame__<>nil) then s:=DestGame__.FileName;
                    if IsAnIniFileSectionFileName(s) then
                       s:=ExtractIniFileName(s);
                    s:=StrWithoutTrailingPathDelimiter(ExtractFilePath(s));
                    if (IsBlank(s)) or (not DirectoryExists(s)) then begin
                       if   Assigned(OpenForm) and (Screen.ActiveForm=OpenForm) and (OpenForm.Task=otGame) and DirectoryExists(OpenForm.FileListBox1.Directory) then
                            s:=OpenForm.FileListBox1.Directory
                       else s:=MainForm.ApplicationDataPath+DEFAULT_LEVEL_DIRECTORY;
                       end;
                    if not DirectoryExists(s) then
                       s:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
                    if not DirectoryExists(s) then
                       s:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
                    if DirectoryExists(s) then
                       PackFileName:=StrWithTrailingPathDelimiter(s)+PackFileName;

                    if   SokoFile.Levels.Count>1 then
                         PackFileName:=Misc_.MakeNewFileName(PackFileName,SOKOBAN_FILE_NAME_EXT,True)
                    else SokoFile.Modified:=False; // the user must save the new level manually before the file is considered modified

                    if   (PackFileName<>'') and SokoFile.SetName(PackFileName) then
                         Level:=TLevel(SokoFile.Levels.First)
                    else ErrorStr:=TEXT_TASK_FAILED;
                    end;
                 end
            else if   SokUtil_.LastErrorText<>'' then
                      ErrorStr:=SokUtil_.LastErrorText
                 else ErrorStr:=TEXT_TASK_FAILED;
            end
       else ErrorStr:=TEXT_CLIPBOARD_NO_LEVEL_TEXT;
       end
    else begin
       if IsAnIniFileSectionFileName(FileName) then begin
          PackFileName:=ExtractIniFileName(FileName);
          SectionName :=ExtractSectionName(FileName);
          end
       else begin
          PackFileName:=FileName; SectionName:=SECTION_NAME_FIRST;
          end;

       if   StrEqual(PackFileName,SokoFile.Name) or
            FileExists(PackFileName) then begin
            SokUtil_.LastErrorText:='';
            if   SokoFile.IsASokobanFile(PackFileName) and
                 SokoFile.Open(PackFileName) then
                 if   SokoFile.Levels.IsEmpty then
                      ErrorStr:=NoLevelsFoundText
                 else if        (SectionName=SECTION_NAME_FIRST) or IsBlank(SectionName) then
                                Level:=TLevel(SokoFile.Levels.First)
                      else if   SectionName=SECTION_NAME_LAST  then
                                Level:=TLevel(SokoFile.Levels.Last)
                           else begin
                                  Level:=TLevel(SokoFile.Levels.GetItemByName(SectionName));
                                  if   Level=nil then
                                       ErrorStr:=Format(PackFileMemberNotFoundText__,[SectionName,PackFileName]);
                                end
            else if   SokUtil_.LastErrorText<>'' then
                      ErrorStr:=SokUtil_.LastErrorText
                 else ErrorStr:=TEXT_TASK_FAILED
            end
       else ErrorStr:=Format(TEXT_FILE_NOT_FOUND_FORMAT,[PackFileName]);
       end;

    Result:=Level<>nil;
    if Result then
       if   (SokoFile.Levels.Count=1) and
            (IsBlank(Level.Name) or StrEqual(Level.Name,TEXT_LEVEL)) then
            FileName:=SokoFile.Name
       else FileName:=MakeIniFileSectionFileName(SokoFile.Name,Level.Name);
  end; // Open

  function UpdateBestSolutions(DestGame__:TGame; var NewBestSolution__:TSnapshot):Boolean;
  var s:String; OldBestSolutionMoves,OldBestSolutionPushes,V:TSnapshot;

    procedure ImportOldSolution(OldSolution__:TSnapshot; var NewBestSolutionPushes__:TSnapshot);
    var V:TSnapshot; // Node:TNode;
    begin
      if OldSolution__<>nil then begin
         LoadSnapshot(OldSolution__);
         IsLoading:=True;                                 // suppresses setting 'Modified' flag and saves dethroned solutions as snapshots
         V:=BestSolutionPushes;                           // remember the best solution/pushes
         if TestForNewBestSolution then begin
            // the old solution is still either the best solution/moves or the best solution/pushes
            if V<>BestSolutionPushes then begin           // 'True': the old solution is now best solution/pushes
               if (NewBestSolutionPushes__<>nil) and
                  (not SaveOldSolutionsAfterClipBoardImport) and
                  (Snapshots.IndexOf(NewBestSolutionPushes__)>=0) then
                  Snapshots.Remove(NewBestSolutionPushes__,True);
               NewBestSolutionPushes__:=BestSolutionPushes;
               end;
            end
         else // the old solution has been dethroned;
            if SaveOldSolutionsAfterClipboardImport then begin // the old solution has been dethroned; save it as a normal snapshot
               V:=MakeSnapshot(Snapshots.MakeUniqueName(SnapshotTypeName(SokFile_.stSolution)+SPACE+
                                                        Format(FORMAT_MOVES_AND_PUSHES,
                                                          [OldSolution__.MoveCount,OldSolution__.PushCount])+
                                                        SecondaryMetricsFormattedAsATitleSuffix(
                                                          DestGame__.SecondaryMetricsInTitles,
                                                          OldSolution__.SecondaryScoreMetrics)+
                                                        SolverNameText(OldSolution__.Name)
                                                        //+NonOptimalText
                                                        ,
                                                        SnapshotTypeName(SokFile_.stSolution),
                                                        True));
               if V<>nil then begin
                  SwapNodes(TNode(OldSolution__.Notes),TNode(V.Notes)); // get the notes from the old solution; since the old solution will be destroyed shortly, there is no reason to copy it instead of taking it
                  //if not V.Notes.Lines.FindKey(TEXT_OPTIMALITY,Node) then
                  //   V.Notes.Lines.WriteString(TEXT_OPTIMALITY,TEXT_NON_OPTIMAL);
                  Snapshots.Push(V);
                  end;
               end;
         end;
    end;

  begin // Give new better solutions, if any, to 'DestGame__'
    NewBestSolution__:=nil;
    Result:=((BestSolutionMoves<>nil)
             and
             (DestGame__.IsABetterSolutionMoves (BestSolutionMoves .MoveCount,BestSolutionMoves .PushCount,BestSolutionMoves.SecondaryScoreMetrics)
              or
              DestGame__.IsABetterSolutionPushes(BestSolutionMoves .MoveCount,BestSolutionMoves .PushCount,BestSolutionMoves.SecondaryScoreMetrics)
             )
            )
            or
            ((BestSolutionPushes<>nil)
             and
             (DestGame__.IsABetterSolutionMoves (BestSolutionPushes.MoveCount,BestSolutionPushes.PushCount,BestSolutionPushes.SecondaryScoreMetrics)
              or
              DestGame__.IsABetterSolutionPushes(BestSolutionPushes.MoveCount,BestSolutionPushes.PushCount,BestSolutionPushes.SecondaryScoreMetrics)
             )
            );
    if Result then begin
       if   DestGame__.DisplayName<>'' then
            s:=TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+DestGame__.DisplayName
       else s:=TEXT_APPLICATION_TITLE;
       Msg(NewBestSolutionsImportedText,s,MB_OK);

       OldBestSolutionMoves :=DestGame__.BestSolutionMoves;
       OldBestSolutionPushes:=DestGame__.BestSolutionPushes;
       V:=nil;
       ImportOldSolution(OldBestSolutionMoves ,V);
       ImportOldSolution(OldBestSolutionPushes,V);

       if BestSolutionMoves <>nil then                     // flip 'Modified' to see it from the 'DestGame__' point of view
          BestSolutionMoves .Modified:=not BestSolutionMoves.Modified;
       if BestSolutionPushes<>nil then
          BestSolutionPushes.Modified:=not BestSolutionPushes.Modified;

       // give best solutions to 'DestGame__'
       DestGame__.DeleteSnapshot(DestGame__.BestSolutionMoves);
       DestGame__.DeleteSnapshot(DestGame__.BestSolutionPushes);
       DestGame__.BestSolutionMoves :=BestSolutionMoves;
       DestGame__.BestSolutionPushes:=BestSolutionPushes;
       DestGame__.UpdateBestSolutionNames;

       if MainForm.ShowSolutionMoves and
          (DestGame__.BestSolutionMoves <>OldBestSolutionMoves ) then NewBestSolution__:=DestGame__.BestSolutionMoves;
       if (not Assigned(NewBestSolution__)) and
          (DestGame__.BestSolutionPushes<>OldBestSolutionPushes) then NewBestSolution__:=DestGame__.BestSolutionPushes;
       if (not Assigned(NewBestSolution__)) and
          (DestGame__.BestSolutionMoves <>OldBestSolutionMoves ) then NewBestSolution__:=DestGame__.BestSolutionMoves;

       BestSolutionMoves:=nil; BestSolutionPushes:=nil;
       DestGame__.LastSolutionWasTheFirstOne:=False;       // the value has no meaning here; just reset it

       if DestGame__=MainForm.Game then begin              // resurrect snapshots, if any, so 'MainForm.Game'
          V:=TSnapshot(DestGame__.Snapshots.First);        // will save them together with the new solutions
          while V<>nil do begin                            // when the level later is closed
            V.Modified:=True;
            V:=TSnapshot(V.Next);
            end;

          if NewBestSolution__<>nil then with DestGame__ do begin
             if (BestSolutionMoves <>nil) and (BestSolutionMoves <>OldBestSolutionMoves) and (BestSolutionMoves <>OldBestSolutionPushes) then
                MainForm.SaveSnapshotToLogFile(MakeIniFileSectionFileName(SokoFileName,Name),'',BestSolutionMoves .GetMovesAsText);
             if (BestSolutionPushes<>nil) and (BestSolutionPushes<>OldBestSolutionMoves) and (BestSolutionPushes<>OldBestSolutionPushes) then
                MainForm.SaveSnapshotToLogFile(MakeIniFileSectionFileName(SokoFileName,Name),'',BestSolutionPushes.GetMovesAsText);
             end;
          end;
       end
    else begin // the solutions, if any, aren't new best solutions
       if BestSolutionMoves<>nil then with BestSolutionMoves do begin
          SetName(SnapshotTypeName(SokFile_.stSolution)+SPACE+
                  Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
                  SecondaryMetricsFormattedAsATitleSuffix(
                    DestGame__.SecondaryMetricsInTitles,
                    SecondaryScoreMetrics)+
                  SolverNameText(Name));

          Snapshots.Push(BestSolutionMoves); BestSolutionMoves:=nil;
          end;
       if BestSolutionPushes<>nil then with BestSolutionPushes do begin
          SetName(SnapshotTypeName(SokFile_.stSolution)+SPACE+
                  Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
                  SecondaryMetricsFormattedAsATitleSuffix(
                    DestGame__.SecondaryMetricsInTitles,
                    SecondaryScoreMetrics)+
                  SolverNameText(Name));
          Snapshots.Push(BestSolutionPushes); BestSolutionPushes:=nil;
          end;
       end;
  end; // UpdateBestSolutions

begin // LoadFromFileOrClipboard
  Result:=False; Modified__:=False; // 'Modified__': if 'True', the destination game has been updated
  Level:=nil; oCursor:=Screen.Cursor; NewFileName:=FileName__;
  OriginalSimpleIllegalMovesMask:=SimpleIllegalMovesMask;
  if (DestGame__<>nil) and (DestGame__.FileName='') then DestGame__:=nil;

  try
    try     if   HourGlassCursor then Screen.Cursor:=crHourGlass;
            SimpleIllegalMovesMask:=0;
            if   Open(NewFileName,Level,s,DestGame__) then begin

                 if   Level.BoardAsTextLines.IsEmpty and
                      (DestGame__<>nil) and
                      (not Level.SnapshotsAsText.IsEmpty) then begin // we're probably importing solutions or snapshots for the current game

                      // first check if input is a continuation of the current game
                      if //(DestGame__.History.Count>DestGame__.ForcedInitialJumps) and
                         (Level.SnapshotsAsText.Count=1) and
                         TSnapshotAsText(Level.SnapshotsAsText.First).TextLinesToMoves(VHistory,VReverseMode) and {text -> internal history-format}
                         (VReverseMode=DestGame__.ReverseMode) and
                         (VHistory.Top<>0) and
                         (VHistory.Top+DestGame__.History.Count<=MAX_MOVES) then begin

                         BoardWidth:=DestGame__.BoardWidth;
                         BoardHeight:=DestGame__.BoardHeight;
                         Board:=DestGame__.StartBoard;
                         CalculateInternalData;
                         SetReverseMode(DestGame__.ReverseMode);

                         DeleteSnapshot(BestSolutionMoves);
                         DeleteSnapshot(BestSolutionPushes);
                         Snapshots.Clear;

                         History:=DestGame__.History;
                         for i:=1 to VHistory.Top do
                             History.Moves[History.Count+i]:=VHistory.Moves[i];
                         History.Top:=History.Count+VHistory.Count;
                         SeparateMoves(History.Count,History);

                         Reset(True);

                         i:=History.Top;
                         IsLoading:=True; {'True': so a new solution isn't tagged as 'Modified' ('UpdateBestSolutions' flips flags)}
                         while Redo0(True) do {replay all legal moves}
                           {restore 'Top' after each move since 'DoMoveUpdateGame' resets it if lowercase/uppercase moves don't match}
                           History.Top:=i;
                         Result:=History.Count=History.Top;

                         if Result then begin
                            RenumberCombinedMoves;
                            if MultiViewItem__<>nil then
                               VHistory:=History;    {save the history; 'UpdateBestSolutions' overwrites the history when it loads existing solutions from the destination game}
                            if UpdateBestSolutions(DestGame__,V) then begin
                               Modified__:=True;

                               if (V<>nil) and (MultiViewItem__<>nil) then begin {update the current view so it contains the new solution}
                                  History:=VHistory; {original moves + imported continuation moves}
                                  Reset(True); {start from the beginning of the game}
                                  while Redo0(True) {replay the moves}
                                        //and
                                        //(History.Count<DestGame__.History.Count)
                                        do begin
                                        end;
                                  Snapshot:=MakeSnapshot('');
                                  if Snapshot<>nil then begin
                                     Snapshot.Modified:=True;
                                     MultiViewItem__.Snapshot.Free;
                                     MultiViewItem__.Snapshot:=Snapshot;
                                     MainForm.MultiView.RefreshItem(MultiViewItem__);
                                     end;
                                  end;

                               Snapshots.Reverse;
                               while not Snapshots.IsEmpty do begin // give any snapshots (in this case dethroned best solutions) to 'DestGame__'
                                  V2:=TSnapshot(Snapshots.Pop);
                                  V2.Modified:=True;
                                  V2.SetName(DestGame__.Snapshots.MakeUniqueName(V2.Name,DestGame__.SnapshotTypeName(SokFile_.stSolution),True));
                                  DestGame__.Snapshots.Add(V2);
                                  if not Assigned(V) then V:=V2; // remember which snapshot or solution to focus in the 'Snapshots' window
                                end;

                               if (SnapshotsForm<>nil) and (DestGame__=MainForm.Game) then with SnapshotsForm do
                                  LoadSnapshots(V); // 'V' makes the 'Snapshots' form focus either a newly created solution or a dethroned solution
                               end
                            else begin
                               Snapshots.Clear;
                               History.Count:=Min(DestGame__.History.Count+VHistory.Count,History.Top);
                               V:=MakeSnapshot('');
                               if V<>nil then begin
                                  V.Modified:=True; Modified__:=True;
                                  if   V.GameState=gsSolved then
                                       V.SetName(DestGame__.Snapshots.MakeUniqueName(V.MakeName(DestGame__.SecondaryMetricsInTitles),SnapshotTypeName(SokFile_.stSolution),True))  // ensure that the name is unique
                                  else V.SetName(DestGame__.Snapshots.MakeUniqueName(V.MakeName(DestGame__.SecondaryMetricsInTitles),SnapshotTypeName(SokFile_.stSnapshot),True)); // ensure that the name is unique

                                  if   (DestGame__=MainForm.Game) and
                                       (V.GameState=gsSolved) then
                                       MainForm.SaveSnapshotToLogFile(MakeIniFileSectionFileName(DestGame__.SokoFileName,DestGame__.Name),'',V.GetMovesAsText);

                                  if   MultiViewItem__=nil then begin
                                       DestGame__.Snapshots.Add(V);
                                       if   (DestGame__=MainForm.Game) and (SnapshotsForm<>nil) then
                                            SnapshotsForm.LoadSnapshots(V);
                                       end
                                  else begin
                                       MultiViewItem__.Snapshot.Free;
                                       MultiViewItem__.Snapshot:=V;
                                       MainForm.MultiView.RefreshItem(MultiViewItem__);
                                       end;
                                  end;
                               end;
                            end;

                         IsLoading:=False; // 'False': otherwise 'Clear' (triggered by 'LoadFromFile') enters an infinite loop
                         end;

                      // if the input wasn't a continuation of the current game then try to load the input as normal snapshots
                      if (not Result) and
                         Level.BoardToTextLines(DestGame__.BoardWidth, // fill in the board...
                                                DestGame__.BoardHeight,
                                                DestGame__.StartBoard) then
                         if   LoadFromFile(Level.Name,SokoFile,True,s) then begin // and then load the level normally
                              if (BestSolutionMoves<>nil) or (BestSolutionPushes<>nil) or (not Snapshots.IsEmpty) then begin
                                 if UpdateBestSolutions(DestGame__,V) then  begin // give new best solutions, if any, to 'DestGame__'
                                    Modified__:=True;
                                    if MultiViewItem__<>nil then begin
                                       if Assigned(DestGame__.BestSolutionMoves ) and DestGame__.BestSolutionMoves .Modified then // 'True': the best solution/moves  is new
                                          MainForm.MultiView.LoadSnapshot(DestGame__.BestSolutionMoves ); // try to load the new solution as a new view
                                       if Assigned(DestGame__.BestSolutionPushes) and DestGame__.BestSolutionPushes.Modified then // 'True': the best solution/pushes is new
                                          MainForm.MultiView.LoadSnapshot(DestGame__.BestSolutionPushes); // try to load the new solution as a new view
                                        end;
                                    end;

                                 Result:=Snapshots.IsEmpty;
                                 while not Snapshots.IsEmpty do begin // give snapshots to 'DestGame__'
                                   V2:=TSnapshot(Snapshots.Pop);
                                   V2.Modified:=True; Modified__:=True;
                                   V2.SetName(DestGame__.Snapshots.MakeUniqueName(V2.MakeName(DestGame__.SecondaryMetricsInTitles),SnapshotTypeName(stSnapshot),True)); // ensure that the name is unique
                                   if   (MultiViewItem__=nil) or
                                        (not MainForm.MultiView.LoadSnapshot(V2)) then // load the snapshot as a new view; if this fails, then put the snapshot on the 'snapshots' list
                                        DestGame__.Snapshots.Add(V2);
                                   if (V2.GameState=gsSolved) and (DestGame__=MainForm.Game) then
                                      MainForm.SaveSnapshotToLogFile(MakeIniFileSectionFileName(DestGame__.SokoFileName,DestGame__.Name),'',V2.GetMovesAsText);
                                   if not Assigned(V) then V:=V2; // remember which snapshot or solution to focus in the 'Snapshots' window
                                   end;
                                 if ((not Result)
                                     or
                                     Modified__
                                    )
                                    and
                                    (SnapshotsForm<>nil)
                                    and
                                    (DestGame__=MainForm.Game) then
                                    SnapshotsForm.LoadSnapshots(V);
                                 end
                              else raise Exception.Create(NoLevelsFoundText)
                              end
                         else if not Verbose then raise Exception.Create(s) // 'Verbose'='True': the error has been reported
                              else;
                      SokoFile.Clear; SokoFile.SetName(''); // don't try to save the file later
                      IsLoading:=False;     // 'False': really clear snapshots, if any; otherwise 'Clear' enters an infinite loop
                      Clear; Result:=False; // 'False': don't load the level after importing solutions and/or snapshots
                      end
                 else if not (Level.BoardAsTextLines.IsEmpty and IsANewFileName(NewFileName)) then begin
                         if   LoadFromFile(Level.Name,SokoFile,True,s) then begin
                              FileName:=NewFileName;
                              if not IsANewFileName(FileName) then
                                 LastValidFileName:=FileName;
                              Result:=True;
                              end
                         else if not Verbose then raise Exception.Create(s) // 'Verbose'='True': the error has already been reported
                              else;
                         end
                      else raise Exception.Create(NoLevelsFoundText); // the clipboard contained moves only, not a board
                 end
            else raise Exception.Create(s);
    finally IsLoading:=False; SimpleIllegalMovesMask:=OriginalSimpleIllegalMovesMask;
            Screen.Cursor:=oCursor;
    end;
  except
    on E:Exception do begin
       LastErrorStr:=E.Message;
       if Verbose then begin
          if   IsBlank(FileName__) then begin
               s:=TheClipboardText; s1:=ClipboardText;
               end
          else begin s:=TheFileText ;
                     if   IsAnIniFileSectionFileName(NewFileName) then
                          s1:=ExtractFileName(ExtractIniFileName(NewFileName))+
                              SUB_TITLE_SEPARATOR+
                              ExtractSectionName(NewFileName)
                     else s1:=NewFileName; //ExtractFileName(NewFileName);
               end;

          s:=Format(OpenFileFailedLongText__,[s]);
          if E.Message<>'' then s:=s+NL+NL+TEXT_FAILURE_DESCRIPTION+NL+NL+E.Message;

          if   MainForm<>nil then
               s1:=MainForm.OpenDialog1.Title+SUB_TITLE_SEPARATOR+s1
          else s1:=Application.Title+SUB_TITLE_SEPARATOR+s1;
          if   IsBlank(FileName__) and (E.Message=NoLevelsFoundText) then
               Msg(Format(OpenFileFailedLongText__,[TheClipboardText]),s1,MB_OK+MB_ICONINFORMATION)
          else SokUtil_.Error(s,s1);
          end;
       Clear; Result:=False;
       end;
  end;
end;

function TGame.IsALegalPosition:Boolean;
var BoxNo:Integer; BoxFromPos,BoxToPos,a,b:TColRow;
begin {returns 'False' if the position is proved to be a deadlock, otherwise 'True'}
  Result:=True;

  if // (not ReverseMode) and
     (GameState<>gsSolved) and
     (not IsReplaying) and
     (DeadlockDetection.Enabled) and
     Assigned(DeadlockDetection.Deadlocks) then begin
     UnpackLastMove(a,b,BoxFromPos,BoxToPos,BoxNo);
     if BoxNo<>0 then with DeadlockDetection.Deadlocks do begin
        OverflowingDeadlockSet:=0; DeadlockedBoxCount:=0; DeadlockStats:='';

        if (SimpleIllegalMovesMask=0)
           // and (not ReverseMode)
           then with BoxPos[BoxNo] do // tests for illegal squares and immovable boxes haven't been made yet; do it now
           if           (Board[x,y] and SIMPLE_ILLEGAL_MOVES_MASK)<>0 then begin // illegal square
                        DeadlockedBoxCount:=1; DeadlockedBoxSquares[1]:=BoxPos[BoxNo];
                        end
           else if      IsAFrozenSquare(x,y,DeadlockedBoxCount,DeadlockedBoxSquares) then
                        // the position is a simple deadlock, i.e., the board has immovable boxes
                else if // (not ReverseMode) and
                        (DeadlockDetection.DeadlockDetectionType<>lmhLow) and
                        (not DeadlockDetection.Deadlocks.WasALegalMove(BoxNo,BoxFromPos,BoxToPos)) then begin
                        // the position is...
                        // 1) a deadlock according to the deadlock-sets calculated when the level was loaded
                        // or
                        // 2) a deadlock caused by overflowing or underflowing a region
                        if not DeadlockDetection.LogEnabled then OverflowingDeadlockSet:=0;
                        end;

        if (DeadlockedBoxCount=0) and
           DeadlockDetection.Enabled and
           (DeadlockDetection.DeadlockDetectionType=lmhHigh) then
           if not DeadlockDetection.Deadlocks.IsALegalPosition then begin
              // the position is a deadlock because of a fenced-in area;
              // (actually, this doesn't need to be the case anymore after adding
              // deadlock-sets which require the player to be outside the set,
              // but it's too much work to change the logic)
              OverflowingDeadlockSet:=0;
              end;

        //if DeadlockStats<>'' then MainForm.Status.Hint:=DeadlockStats;

        Result:=DeadlockedBoxCount=0;
        if not Result and (Self=MainForm.Game) then
           Windows.PostMessage(MainForm.Handle,MSG_SHOW_DEADLOCKED_BOXES,0,0);
        end;
     end;
end;

function TGame.CopyMovesToClipboard(CopyContinuationMovesOnly__,RunLengthEncoding__:Boolean):Boolean;
var Index,Last:Integer; MessageText,MovesAsText:String;
begin
  with History do
    try    Result:=True; MovesAsText:='';
           if   CopyContinuationMovesOnly__ then begin
                Last:=Top;
                MessageText:=ContinuationMovesCopiedToClipboardText;
                end
           else begin
                Last:=Count;
                MessageText:=MovesCopiedToClipboardText;
                end;

           if   (not CopyContinuationMovesOnly__) or (Count<Top) then
                Result:=SokFile_.MovesToText(Addr(Moves),Count,Last,ReverseMode,RunLengthEncoding__,CombinedMoveFormattingEnabled,MovesAsText);
           if   Result then begin
                if   CopyContinuationMovesOnly__ then begin
                     Index:=System.Pos(CURRENT_MOVE_CH,MovesAsText);
                     if   Index<>0 then Delete(MovesAsText,1,Index);
                     if   (MovesAsText<>'') and ReverseMode and
                          (MovesAsText[1]<>JUMP_BEGIN_CH ) and
                          (MovesAsText[1]<>JUMP_BEGIN_CH1) then
                          MovesAsText:=JUMP_BEGIN_CH+JUMP_END_CH+MovesAsText;
                     end;
                if   MovesAsText<>'' then begin
                     Clipboard.AsText:=MovesAsText;
                     if   Screen.ActiveForm=MainForm then
                          if   MainForm.Status<>nil then
                               MainForm.Status.Hint:=MessageText
                          else
                     else Msg(MessageText,PChar(TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+DisplayName),MB_OK);
                     end
                else Result:=False;
                end
           else Msg(TEXT_TASK_FAILED,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+DisplayName,MB_OK);
    except on E:Exception do Result:=Error(E.Message,TEXT_APPLICATION_TITLE);
    end;
end;

function TGame.CopySnapshotToClipboard(Snapshot__:TSnapshot; RunLengthEncoding__:Boolean):Boolean;
var s:String; Node:TNode; SnapshotAsText:TSnapshotAsText;
begin
  Result:=False;
  if Snapshot__<>nil then begin
     SnapshotAsText:=Snapshot__.MakeSnapshotAsText(PrettyPrintGamesEnabled,RunLengthEncoding__,CombinedMoveFormattingEnabled,True);
     if SnapshotAsText<>nil then
        try     try    s:=''; Node:=SnapshotAsText.MovesAsTextLines.First;
                       while Node<>nil do begin
                         if   s<>'' then s:=s+NL+Node.Text
                         else s:=Node.Text;
                         Node:=Node.Next;
                         end;
                       ClipBoard.AsText:=s;
                       Result:=True;
                except on E:Exception do Result:=Error(E.Message,TEXT_APPLICATION_TITLE);
                end;
        finally SnapshotAsText.Free;
        end;
     end;
end;

function TGame.CopyToClipboard(FloorCharacter__:Char; PreserveCombinedMoves__,RunLengthEncoding__,BoardOnly__,CurrentBoardOnly__:Boolean):Boolean;
var oBlankLinesBeforeLevel:Integer;
    TimeMS:TTimeMS;
    s,LevelAsText:String;
    n:TNode; Level:TLevel; v,LocalSaveGame:TSnapshot; View:TMultiViewItem;
    CurrentBoardTransformation2D:TBoardTransformation2D;
begin
  StopTimer;
  oBlankLinesBeforeLevel:=BlankLinesBeforeLevel;
  LocalSaveGame:=nil;

  try
    Result:=CreateObject(otLevel,TNode(Level));

    if Result then
       try

         CurrentBoardTransformation2D:=BoardTransformation2D;
         DoBoardTransformation2D(BOARD_TRANSFORMATION_INVERSE[BoardTransformation2D],False);
         try
           if BoardOnly__ then
              BlankLinesBeforeLevel:=-1 // kludge: negative 'BlankLinesBeforeLevel': 'TLevel.WriteToFile' doesn't emit an anonymous title and a blank lines before the board
           else begin
              BlankLinesBeforeLevel:=0;

              Result:=Level.SetName(StrRemoveCharacters(DisplayName,TITLE_ILLEGAL_CHARACTERS));

              Result:=Result and Notes.CopyTo(Level.Notes);

              if   Result and
                   Level.Notes.Lines.ReadString(KEY_TIME,s) and
                   SokUtil_.StrToTime(s,OriginalTimeMS) then
              else OriginalTimeMS:=0;
              end;

           {board internal form -> text}
           if   (not BoardOnly__) or // if 'not BoardOnly__' then it only makes sense to export the start board; the exported solutions and snapshots, if any, only matches the start board, hence, 'CurrentBoardOnly__' doesn't matter in this case
                (not CurrentBoardOnly__) then
                Result:=Result and Level.BoardToTextLines(BoardWidth,BoardHeight,StartBoard)
           else Result:=Result and Level.BoardToTextLines(BoardWidth,BoardHeight,Board);
           if Result and (FloorCharacter__<>FLOOR_CH) then
              Level.BoardAsTextLines.StrSubstitute(FLOOR_CH,FloorCharacter__,'',True);
           if RunLengthEncoding__ then
              if   Level.BoardToTextRLE(MainForm.RunLengthEncodingFloor,s) then begin
                   Level.BoardAsTextLines.Clear;
                   Result:=Level.BoardAsTextLines.AddTextLine(s,False)<>nil;
                   end
              else Result:=False;

           if not BoardOnly__ then begin
              if Result and (Self=MainForm.Game) and Assigned(MainForm.MultiView) and (not MainForm.MultiView.IsEmpty) then begin
                 Result:=MainForm.MultiView.MarkDuplicates;

                 View:=TMultiViewItem(MainForm.MultiView.Items.First); {make text-versions of current multiple views items}
                 while Assigned(View) and Result do begin
                   if Assigned(View.Snapshot) and
                      (View.Snapshot.MoveCount>View.Snapshot.ForcedInitialJumps) then begin {drop 0-moves snapshots}
                      if View.Tag=0 then begin {'0': a unique snapshot or a representative for a set of duplicates}
                         n:=TNode(View.Snapshot.MakeSnapshotAsText(PrettyPrintGamesEnabled,RunLengthEncoding__,MainForm.CopyLevelToClipboardPreserveCombinedMoves,True));
                         if   n<>nil then begin
                              Level.SnapshotsAsText.Push(n);
                              TSnapshotAsText(n).Notes.Lines.WriteString(TEXT_SCREEN_REGION,RectToStr(View.Rect));
                             end
                         else Result:=False;
                         end
                      else if View.Tag<>Integer(View) then {'True': the view item is a duplicate of a snapshot on the 'Snapshots' list}
                              TSnapshot(Pointer(View.Tag)).Notes.Lines.WriteString(TEXT_SCREEN_REGION,RectToStr(RectPlusOffset(View.Rect,-MainForm.MultiView.ClippingRect.Left,-MainForm.MultiView.ClippingRect.Top))); // store the screen region for the view together with the existing snapshot
                      end;
                   View:=TMultiViewItem(View.Next);
                   end;
                 end;

              v:=TSnapshot(Snapshots.First);           {make text-versions of current snapshots}
              while (v<>nil) and Result do begin
                if v.MoveCount>v.ForcedInitialJumps then begin {drop 0-moves snapshots}
                   n:=TNode(v.MakeSnapshotAsText(PrettyPrintGamesEnabled,RunLengthEncoding__,MainForm.CopyLevelToClipboardPreserveCombinedMoves,True));
                   if   n<>nil then Level.SnapshotsAsText.Push(n)
                   else Result:=False;
                   end;
                v:=TSnapshot(v.Next);
                end;
              Level.SnapshotsAsText.Reverse;           {snapshots are in reversed order: make it right}

              if   Result and
                   (History.Count>ForcedInitialJumps) and {check if current game should be saved}
                   (((Self=MainForm.Game) and Assigned(MainForm.MultiView) and (not MainForm.MultiView.IsEmpty)) {with multiple views, the savegame is needed to select which one to focus the next time the level is opened}
                    or
                    ((not IsEqualToCurrentGame(BestSolutionMoves ))
                     and                                                         {there is no need to store a savegame if it's a part of one of the best solutions}
                     (not IsEqualToCurrentGame(BestSolutionPushes))
                    )
                   ) then begin
                   LocalSaveGame:=MakeSnapshot(SnapshotTypeName(stSaveGame)+SPACE+Format(FORMAT_MOVES_AND_PUSHES,[History.Count,History.PushCount]));
                   n:=nil;
                   if   LocalSaveGame<>nil then n:=TNode(LocalSaveGame.MakeSnapshotAsText(PrettyPrintGamesEnabled,RunLengthEncoding__,MainForm.CopyLevelToClipboardPreserveCombinedMoves,True));
                   if   n<>nil then Level.SnapshotsAsText.Push(n) {add savegame to the list}
                   else Result:=False;
                   end;

              if   BestSolutionPushes<>nil then begin
                   with BestSolutionPushes do SetName(SnapshotTypeName(stBestSolutionPushes)+
                                                      SPACE+
                                                      Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
                                                      SecondaryMetricsFormattedAsATitleSuffix(
                                                        SecondaryMetricsInTitles,
                                                        SecondaryScoreMetrics)+
                                                      SolverNameText(Name));
                   n:=TNode(BestSolutionPushes.MakeSnapshotAsText(PrettyPrintGamesEnabled,RunLengthEncoding__,MainForm.CopyLevelToClipboardPreserveCombinedMoves,True));
                   if   n<>nil then Level.SnapshotsAsText.Push(n)
                   else Result:=False;
                   end;

              if   BestSolutionMoves<>nil then begin
                   if   BestSolutionPushes=nil then
                        with BestSolutionMoves do SetName(SnapshotTypeName(stBestSolution)+
                                                          SPACE+
                                                          Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
                                                          SecondaryMetricsFormattedAsATitleSuffix(
                                                            SecondaryMetricsInTitles,
                                                            SecondaryScoreMetrics)+
                                                          SolverNameText(Name))
                   else with BestSolutionMoves do SetName(SnapshotTypeName(stBestSolutionMoves)+
                                                          SPACE+
                                                          Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
                                                          SecondaryMetricsFormattedAsATitleSuffix(
                                                            SecondaryMetricsInTitles,
                                                            SecondaryScoreMetrics)+
                                                          SolverNameText(Name));
                   n:=TNode(BestSolutionMoves.MakeSnapshotAsText(PrettyPrintGamesEnabled,RunLengthEncoding__,MainForm.CopyLevelToClipboardPreserveCombinedMoves,True));
                   if   n<>nil then Level.SnapshotsAsText.Push(n)
                   else Result:=False;
                   end;

              if   Result and (Level.Notes<>nil) and (SessionTimeMS<>0) then begin
                   TimeMS:=OriginalTimeMS+SessionTimeMS;
                   if   (not Level.Notes.Lines.ReadString(KEY_TIME,s)) and
                        (not Level.Notes.Lines.IsEmpty) then
                        Level.Notes.Lines.AddBlankLine; // separate 'time' from the rest of the lines
                   if   Level.Notes.Lines.WriteString(KEY_TIME,SokUtil_.TimeToStr(TimeMS)) then {}
                   else Result:=False;
                   end;

              if   Result and (Level.Notes<>nil) then begin
                   if CurrentBoardTransformation2D=t2DRotate0DegreesClockwise then {default transformation: remove old transformation from notes, if any}
                      if Level.Notes.Lines.FindKey(KEY_BOARD_TRANSFORMATION,n) then begin
                         Level.Notes.Lines.Remove(n,True);
                         Level.Notes.Lines.TrimBlankLines;
                         end
                      else
                   else begin
                      if   (not Level.Notes.Lines.ReadString(KEY_BOARD_TRANSFORMATION,s)) and
                           (not Level.Notes.Lines.IsEmpty) then
                           Level.Notes.Lines.AddBlankLine; // separate 'transformation' from the rest of the lines
                      if   Level.Notes.Lines.WriteString(KEY_BOARD_TRANSFORMATION,BoardTransformation2DToStr(CurrentBoardTransformation2D)+PERIOD) then {}
                      else Result:=False;
                      end;
                   end;
              end;

           if   Result then
                if   Level.ToText(LevelAsText) then
                     ClipBoard.AsText:=LevelAsText
                else Result:=False;

           if   Result then begin
                if   BoardOnly__ then
                     s:=CopiedBoardToClipboardText
                else s:=LevelCopiedToClipboardText;

                if   Screen.ActiveForm=MainForm then
                     // kludge: Windows XP/2000 compatibility:
                     // for some unknown reason, calling 'MessageBox'
                     // (it doesn't matter whether it's called directly or
                     // through 'Msg') overwrites the clipboard with the
                     // message, but only when this code is activated from
                     // 'MainForm', not when it's activated from 'OpenForm';
                     // it sounds weird, but that's nevertheless what happens;
                     if   MainForm.Status<>nil then
                          MainForm.Status.Hint:=s
                     else
                else //if   Screen.ActiveForm=OpenForm then
                            // this doesn't work because the message immidately is overwritten by a normal hint
                     //     OpenForm.StatusBar1.Panels[OPEN_FORM_PANEL_INDEX_HINT].Text:=LevelCopiedToClipboardText
                     //else
                            Msg(s,PChar(TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+DisplayName),MB_OK);
                end
           else Msg(TEXT_TASK_FAILED,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+DisplayName,MB_OK);

         finally DoBoardTransformation2D(CurrentBoardTransformation2D,False);
                 BlankLinesBeforeLevel:=oBlankLinesBeforeLevel;
         end;
       finally Level.Free; LocalSaveGame.Free;
       end;
  except on E:Exception do Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE);
  end;
end;

function  TGame.MakeForwardSolutionFromBackwardSolution(WorkGame__:TGame; BackwardSolution__:TSnapshot; var ForwardSolution__:TSnapshot):Boolean;
var OldBestSolutionMoves,OldBestSolutionPushes:TSnapshot;
begin
  ForwardSolution__:=nil;
  if BackwardSolution__.ReverseMode and (BackwardSolution__.GameState=gsSolved) then
     try
       WorkGame__.Clear;
       WorkGame__.BoardWidth:=BoardWidth;
       WorkGame__.BoardHeight:=BoardHeight;
       WorkGame__.Board:=StartBoard;
       WorkGame__.CalculateInternalData;

       if WorkGame__.LoadSnapshot(BackwardSolution__) then begin
          if WorkGame__.History.Count=BackwardSolution__.MoveTop then
             WorkGame__.InheritedUndo(True);                        {undo a move so 'Redo' updates the best solutions}
          while (WorkGame__.History.Count<BackwardSolution__.MoveTop) and
             WorkGame__.InheritedRedo(True) do begin end;

          if (WorkGame__.GameState=gsSolved) and (WorkGame__.BestSolutionMoves<>nil) then begin
             SwapNodes(TNode(ForwardSolution__),TNode(WorkGame__.BestSolutionMoves));
             with ForwardSolution__ do
               SetName(Snapshots.MakeUniqueName(
                       SnapshotTypeName(SokFile_.stSolution)+SPACE+
                       Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount])+
                       SecondaryMetricsFormattedAsATitleSuffix(
                       SecondaryMetricsInTitles,
                       SecondaryScoreMetrics)+
                       SolverNameText(BackwardSolution__.Name),
                       SnapshotTypeName(SokFile_.stSolution),
                       True
                      ));
             if ForwardSolution__.Name<>'' then with ForwardSolution__ do begin
                OldBestSolutionMoves :=BestSolutionMoves;
                OldBestSolutionPushes:=BestSolutionPushes;

                if        IsABetterSolutionMoves (MoveCount,PushCount,SecondaryScoreMetrics) or
                          IsABetterSolutionPushes(MoveCount,PushCount,SecondaryScoreMetrics) then begin {'True': new best solution; update the best solutions for the level}
                          SwapNodes(TNode(BestSolutionMoves ),TNode(WorkGame__.BestSolutionMoves )); {transfer the best solutions from this game to the work game}
                          SwapNodes(TNode(BestSolutionPushes),TNode(WorkGame__.BestSolutionPushes));

                          WorkGame__.IsLoading                             :=True; {'True': don't save the new best solution in the error recovery log files a second time}
                          WorkGame__.SaveOldSolutionsAfterFindingBetterOnes:=SaveOldSolutionsAfterFindingBetterOnes;
                          WorkGame__.SecondaryMetricsInTitles              :=SecondaryMetricsInTitles;

                          if WorkGame__.History.Count=BackwardSolution__.MoveTop then
                             WorkGame__.InheritedUndo(True);                    {undo a move so 'Redo' updates the best solutions}
                          while (WorkGame__.History.Count<BackwardSolution__.MoveTop) and
                                WorkGame__.InheritedRedo(True) do begin end;

                          SwapNodes(TNode(BestSolutionMoves ),TNode(WorkGame__.BestSolutionMoves )); {transfer the best solutions back from the work game to this game}
                          SwapNodes(TNode(BestSolutionPushes),TNode(WorkGame__.BestSolutionPushes));
                          while not WorkGame__.Snapshots.IsEmpty do {transfer any saved dethroned best solutions back to this game}
                            Snapshots.Add(TNode(WorkGame__.Snapshots.Pop));
                          end;

                if        OldBestSolutionMoves <>BestSolutionMoves  then begin {'True': the new solution is a new best solution/moves}
                          ForwardSolution__.Free; ForwardSolution__:=BestSolutionMoves;
                          end
                else if   OldBestSolutionPushes<>BestSolutionPushes then begin {'True': the new solution is a new best solution/pushes}
                          ForwardSolution__.Free; ForwardSolution__:=BestSolutionPushes;
                          end
                     else Snapshots.Add(ForwardSolution__); {the new solution isn't a new best solution; add it to the snapshot list}
                end
             else begin
                ForwardSolution__.Free; ForwardSolution__:=nil; {'SetName()' or 'MakeUniqueName()' failed, hence, drop the new snapshot}
                end;
             end;
          end;
     finally WorkGame__.Clear;
     end;
  Result:=Assigned(ForwardSolution__);
  if Result then BackwardSolution__.Notes.CopyTo(ForwardSolution__.Notes);
end;

function  TGame.VisiblePlayerDirection:TDirection;
var PlayerFrameIndex:Integer;
begin
  with History do begin
    Result:=PlayerDirection(Count);
    if (Count=0) and (Top>0) and // after 'undo' back to the starting position, the player may look in the same direction as the first move
       (Self=MainForm.Game) and
       Assigned(MainForm.GameViewer) then begin
       PlayerFrameIndex:=MainForm.GameViewer.PlayerFrameIndex;
       if (PlayerFrameIndex>=Ord(Low (Result))) and
          (PlayerFrameIndex<=Ord(High(Result))) then
          Result:=TDirection(PlayerFrameIndex);
       end;
    end;
end;

end.

