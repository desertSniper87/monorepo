{
YASGen3D - Yet Another Sokoban 3D Level Generator - For Small Levels
Version 1.48 November 26, 2016

Copyright (c) 2001-2017 by Brian Damgaard, Denmark

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

{
YASGen3D - Yet Another Sokoban 3D Level Generator - For Small Levels
====================================================================

Features
--------

* Generates longest path variations of existing levels, either based on
  their start-positions (using boxes) or their end-positions (using
  goals).

  Using goal positions is normally the best choice because it often
  generates fewer states (the depth of the analysis is limited by
  by the memory available for storing the found states).

  A longer variation of a hand-made level does not always mean a better
  level. Sometimes aesthetic qualities are lost, and sometimes the
  level even gets easier. There are many situations, though, where the
  longest variation indeed is the hardest one and the best one.

* Generates new levels using a 'genetic algorithm' (GA) to find longer
  path variations of the level used for seeding the generator.

  The GA generator runs in an endless loop, so the program must be
  terminated manually (press Ctrl+C on the keyboard, that is, hold
  down one of the "Ctrl"-buttons and press "c").

  It's possible to restart the GA generator by specifying the log
  file as input (normally the file 'YASGen Log.sok').

* The GA generator can either create levels with connected goals, or
  levels where goals may be scattered all over the board. Levels with
  connected goals are often more appealing because they look more like
  levels created by humans.

Usage
-----
  Run the program without any parameters to see a complete listing of
  the options.

  Here are a few typical examples:

  * YASGen3D -ga -width 6 -height 6 -walls 10 -boxes 6 -maxboxes 6 -maxopen 100000 -tail 1 -seed 123456

      Seeds the genetic generator with a randomly generated 6x6 level
      with 10 interior walls and 6 boxes which also it set as maximum. The
      search is sped up by pruning large candidates having more than 100000
      positions on the open-queue at any time during the search. Trivial tail
      positions of order 1 (each box is focused at most once) are discounted by
      the fitness function, and the random number generator is seeded with the
      value 123456.

  * YASGen3D -file "YASGen Log.sok" -memory 128

      Restarts the genetic generator using the previously generated log-file
      'YASGen Log.sok'.

  * YASGen3D -file levels.sok  -memory 128

      Processes all levels in the file 'levels.sok', using 128 MiB memory.

  * YASGen3D -file levels.sok -level 5 -memory 128

      Processes the file 'levels.sok', starting from the fifth level.

  * YASGen3D -ga -file levels.sok -level 5 -generations 1000 -memory 128

      Seeds the genetic generator with the fifth level from 'levels.sok'
      and stops after 1000 generations.

  * YASGen3D -ga -type random -fitness pushes -width 6 -height 6 -walls 4 -boxes 6 -maxboxes 6 -mem 128

      Seeds the genetic generator with a randomly generated 6x6 level
      with 4 interior wall squares and 6 boxes, and generates 'random goals'
      levels instead of 'connected goals' levels, using pushes as fitness
      function.

  * YASGen3D -ga -file template.sok -boxes 4 -maxboxes 8 -seed 123456

      Adds 4 boxes to the level in 'template.sok' and uses it as seed for
      the genetic generator.

  * YASGen3D -ga -file template.sok -fixed -type random -memory 128

      Fixates the walls and goals in the level found in 'template.sok'
      and uses this level as seed for the genetic generator. Note that
      it may be necessary to specify level-type "random", otherwise,
      the generator doesn't allow un-connected goals in the template.

  * YASGen3D -ga -forward -file template.sok -fixed

      Runs the genetic generator using a forward search. This makes it
      possible to fixate walls, boxes, and player from the template level
      as opposed to the default backwards search where it is walls and
      goals that can be fixated. Note that it's highly recommended to
      specify a fixed player position for the forward search; otherwise
      each level may need several passes, one for each player access area.
}


{-----------------------------------------------------------------------------}

{Compiler Switches}

{ Before compiling the program, there are 4 choices to make:

  * Compiler...........: Delphi or FPC
  * Platform...........: Windows or not Windows
  * Application Type...: Console or Silent (no console, and no graphical user interface either)
  * Output.............: Verbose of brief

  Compiler
  --------
  At the time of writing the Delphi compiler generates approx. 30% more
  efficient machine code than the FPC compiler.

  Platform
  --------
  On the Windows platform, the program uses timing, e.g., it reports the total
  running time and time of "birth" for the candidates.

  Application Type
  ----------------
  Compiling the program as a console application creates a text-mode window
  where the program displays various information during its execution.

  Compiling the program as a GUI application is an option on the Windows (tm)
  platform only. It's actually a misnomer since the effect is not that the
  generator appears with a graphical user interface. What happens is that it
  runs in "silent mode", without opening a console window.

  The silent mode variant hasn't been maintained and may or may not work in this
  version of the program.

  Output
  ------
  In brief mode, the program generates a single output file named "YASGen.out"
  with the minimum required information:

  * Level name
  * Board
  * Number of pushes
  * Exhaustive search y/n?
  * Solution

  In verbose mode, the program generates two output files named:

  * "<input file name>, YASGen Version<input file name extension>"
  * "<input file name>, YASGen Version, Statistics.txt"

  The first file is almost identical to the one described above under brief
  mode. The second file contains additional statistical information.

  You might argue that choosing output mode should be deferred until run time
  instead of compile time, but for typical usage of the program this will
  suffice.
}

{$DEFINE DELPHI}                               {compiler: select one of these: DELPHI of FPC}
{or}
{///$DEFINE FPC}

{$DEFINE CONSOLE_APPLICATION}                  {application type: select CONSOLE or SILENT_APPLICATION}
{or}
{///$DEFINE SILENT_APPLICATION}                {no console window, but no graphical user interface either, i.e., 'silent mode'}

{$DEFINE WINDOWS}                              {use this on the Windows platform only}

{$DEFINE VERBOSE_OUTPUT}                       {output: select one of these: VERBOSE of BRIEF}
{or}
{///$DEFINE BRIEF_OUTPUT}

{end of manually selected compiler switches;}
{what follows is internal management        }

{$IFDEF DELPHI}
  {$UNDEF FPC}                                 {for safety}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI} {$PACKENUM 1}                 {use this with FPC only}
  {$UNDEF DELPHI}                              {for safety}
{$ENDIF}

{$IFDEF CONSOLE_APPLICATION}
  {$APPTYPE CONSOLE}
  {$UNDEF SILENT_APPLICATION}                  {for safety}
{$ENDIF}

{$IFDEF SILENT_APPLICATION}
  {$APPTYPE GUI}
  {$UNDEF CONSOLE_APPLICATION}                 {for safety}
{$ENDIF}

{$IFDEF VERBOSE_OUTPUT}
  {$UNDEF BRIEF_OUTPUT}                        {for safety}
{$ENDIF}

{-----------------------------------------------------------------------------}

{$R-}                                          {'R-': range checking disabled}

Program YASGen3D;                              {YASGen3D - Yet Another Sokoban 3D Level Generator - For Small Levels}

{$IFDEF SILENT_APPLICATION}
  {$IFNDEF Windows}
    uses Windows;                              {for access to 'MessageBox'}
  {$ENDIF}
{$ENDIF}

{$IFDEF Windows}
  uses Windows;                                {e.g., for access to 'GetTickCount' and 'MessageBox'}
{$ENDIF}

{General Constants}
const
  COLON                    = ':';              {ascii characters}
  DOUBLE_QUOTE             = '"';
  EQUAL                    = '=';
  LEFT_BRACKET             = '[';
  PERIOD                   = '.';
  QUOTE                    = '''';
  RIGHT_BRACKET            = ']';
  SEMICOLON                = ';';
  SLASH                    = '/';
  SPACE                    = ' ';
  TAB                      = Chr(9);

  ONE_KIBI                 = 1024;                                              {"kibi" used to be 'kilo' before year 2000}
  ONE_THOUSAND             = 1000;

  ONE_MEBI                 = ONE_KIBI*ONE_KIBI;                                 {"mebi" used to be "mega" before year 2000}
  ONE_MILLION              = ONE_THOUSAND*ONE_THOUSAND;

{General Types}
type
  UInt8                    = Byte;
  Int16                    = SmallInt;         {nobody can remember that 'SmallInt'  means signed   16 bit integer in Delphi4, hence, this alias comes in handy}
  UInt16                   = Word;             {for historical reasons,  'Word'      means unsigned 16 bit integer in Delphi4, but it's problematic to use a term like that with modern processors}
  UInt32                   = Cardinal;         {for historical reasons,  'Cardinal'  means unsigned 32 bit integer in Delphi4, but it's problematic to use a term like that with modern processors}
  UInt                     = Cardinal;         {unsigned machine word integer; must be the same size as a pointer}

  TTimeMS                  = {$IFDEF WINDOWS} DWORD; {$ELSE} UInt32; {$ENDIF}

{-----------------------------------------------------------------------------}

{Type declarations required before declaring constants and other types}
type
  TDirection               = (dUp,dLeft,dDown,dRight,dIn,dOut); {caution: must match 'DIRECTION_BIT_COUNT' defined below}
  TGeneratorMethod         = (gmForward,gmBackward,gmBidirectional,gmRandom,gmEnumerate,gmGA); {'gmRandom' not in production anymore; it has been superseeded by the genetic algorithm generator}
  TMove                    = packed record BoxNo:Byte; Direction:TDirection; end; {'packed': literal sequence of unaligned slots, without gaps}

{-----------------------------------------------------------------------------}

{Constants}
const
  BOX                      = 1;                {board, internal representation}
  FLOOR                    = 2;
  GOAL                     = 4;
  PLAYER                   = 8;
  WALL                     = 16;
  FLAG_FIXED_SQUARE        = 128;              {precondition: the flag value must be bigger than the value of the ASCII characters in use for the board squares}
                                               {board, text representation}
  CH_BOX                   = 'b'; CH_BOX_ON_GOAL           = 'B';
  CH_BOX_XSB               = '$'; CH_BOX_ON_GOAL_XSB       = '*';
  CH_GOAL                  = '.';
  CH_FLOOR                 = SPACE;
  CH_FIXED_FLOOR           = '='; {fixed floor in a template}
  CH_PLAYER                = 'p'; CH_PLAYER_ON_GOAL        = 'P';
  CH_PLAYER_XSB            = '@'; CH_PLAYER_ON_GOAL_XSB    = '+';
  CH_WALL                  = '#';

  BRIEF_OUTPUT_FILE_EXT    = '.out';
  BOARD_3D_LAYER_SEPARATOR = '&&&';
  BOARD_TEXT_LINE_TEMPLATE_CHAR
                           = SEMICOLON; {template board lines have a prefix character so a level-reader doesn't treat the template as a normal level}
  DEFAULT_TRIVIAL_OPENING_PUSHES_IGNORED
                           = True;
  DEFAULT_MEMORY_BYTE_SIZE = 128*ONE_MEBI;    {128 MiB}
  DEFAULT_PRETTY_PRINTING  = True;
  DEFAULT_TAIL_POSITION_THRESHOLD
                           = 1;
  DIRECTION_BIT_COUNT      = 3; {caution: must match or exceed the number of directions, see 'TDirection' above; moreover, it must leave free bits in the 'Move.Direction' for flags stored together with the direction; see 'POSITION_TAG_XXX' flags below}
  DIRECTION_BIT_MASK       = (1 shl DIRECTION_BIT_COUNT)-1;
  DIRECTION_COUNT          = Succ(Ord(High(TDirection))-Ord(Low(TDirection))); {number of directions}
  DIRECTION_TO_CHAR        : array[TDirection] of Char
                           = ('u','l','d','r','i','o');
  MAX_BOARD_WIDTH          = 10;
  MAX_BOARD_HEIGHT         = MAX_BOARD_WIDTH;
  MAX_BOARD_DEPTH          = MAX_BOARD_WIDTH;
  MAX_BOARD_3D_LAYER_SIZE  = (MAX_BOARD_WIDTH+2) * (MAX_BOARD_HEIGHT+2); {'0' left/top border; '+1': right/bottom border}
  MAX_BOARD_SIZE           = MAX_BOARD_3D_LAYER_SIZE * (MAX_BOARD_DEPTH+2); {'0' foreground border; '+1': background border}
  MAX_BOX_COUNT            = High(Byte);       {limited by the slot 'BoxNo' in 'TMove'}
  MAX_DIRECTION_COUNT      = Ord(High(TDirection))-Ord(Low(TDirection))+1;
  MAX_HASH_COLLISION_COUNT = 32*ONE_KIBI;      {linear search for a matching item or a free slot}
  MAX_HISTORY_MOVES        = 499;              {must be < High(TPosition.Depth)}
  MAX_MOVES                = MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT+Succ(Ord(High(TDirection))-Ord(Low(TDirection)))*MAX_BOX_COUNT;
  MAX_TAIL_POSITION_THRESHOLD
                           = 4;
  MEMORY_BLOCKS_COUNT      = 8;                {must be a power of 2, so (MBC-1) can be used as a mask}
  MIN_BOARD_WIDTH          = 3;
  MIN_BOARD_HEIGHT         = 3;
  MIN_BOARD_DEPTH          = 1;
  OPPOSITE_DIRECTION       : array[TDirection] of TDirection
                           = (dDown,dRight,dUp,dLeft,dOut,dIn);
  POSITION_TAG_CLOSED      = 1 shl (DIRECTION_BIT_COUNT+1); {flag-bit in 'TPosition.Move.Direction'}
  POSITION_TAG_TAIL        = POSITION_TAG_CLOSED shl 1; {flag-bit in 'TPosition.Move.Direction'}
  POSITION_TAG_BIT_MASK    = POSITION_TAG_CLOSED + POSITION_TAG_TAIL; {must be the sum of all the tags in use}
  SOKOBAN_FILE_NAME_EXT  = '.sok';
  SOLUTION_LINE_LENGTH     = 70;

{-----------------------------------------------------------------------------}

{Texts}
const
  TEXT_APPLICATION_COPYRIGHT
                           = '(c) 2001-2017 by Brian Damgaard, Denmark';
  TEXT_APPLICATION_TITLE   = 'YASGen3D';
  TEXT_APPLICATION_VERSION = '1.48';
  TEXT_AUTO_GENERATED_LEVELS_FILE_NAME
                           = TEXT_APPLICATION_TITLE+' Autogenerated Levels'+SOKOBAN_FILE_NAME_EXT;
  TEXT_BACKWARD_SEARCH     = 'Backward search';
  TEXT_BOARD_HAS_TOO_MANY_3D_LAYERS
                           = 'Board has too many 3D layers';
  TEXT_BOARD_HAS_TOO_MANY_ROWS
                           = 'Board has too many rows';
  TEXT_BOARD_LINE_TOO_LONG = 'Board line too long';
  TEXT_KEY_ENABLED_SUFFIX  = 'enabled';
  TEXT_FILE_OPEN_ERROR     = 'File open error (Check the path. Maybe the file was not found in the specified folder)';
  TEXT_FILE_READ_ERROR     = 'File read error';
  TEXT_FILE_WRITE_ERROR    = 'File write error';
  TEXT_FORWARD_SEARCH      = 'Forward search';
  TEXT_GENERATOR_METHOD    : array[TGeneratorMethod] of String = (
    'Forward generator, i.e., using boxes','Backward generator, i.e., using goals',
    'Bidirectional-generator, i.e., forward and backward',
    'Random-generator','Enumeration-generator','Genetic-algorithm-generator');
  TEXT_LEVEL               = 'Level';
  TEXT_LEVEL_TITLE_SUFFIX  =', '+TEXT_APPLICATION_TITLE+' Version';
  TEXT_MILLISECONDS        = 'milliseconds';
  TEXT_NO_YES              : array[Boolean] of String = ('No','Yes');
  TEXT_STATISTICS_FILENAME_SUFFIX
                           =', Statistics.txt';
  TEXT_TIME_MS             = 'Time, milliseconds';
  TEXT_UNLIMITED           = 'Unlimited';

{-----------------------------------------------------------------------------}

{Types}
type
  TBoard                   = array[0..MAX_BOARD_SIZE] of Byte;
  TBoard3D                 = array[0..MAX_BOARD_DEPTH+1,0..MAX_BOARD_HEIGHT+1,0..MAX_BOARD_WIDTH+1] of Byte; {order: depth, row, column}
  TBoardBoolean            = array[0..MAX_BOARD_SIZE] of Boolean;
  TBoardInteger            = array[0..MAX_BOARD_SIZE] of Integer;
  TBoardTextLines          = array[0..MAX_BOARD_DEPTH,0..MAX_BOARD_HEIGHT] of String[MAX_BOARD_WIDTH];
  THashValue               = Int64;
  THistory                 = record
    Count                  : Integer; {number of pushes}
    Fitness                : Integer; {estimated complexity}
    Moves                  : array[0..MAX_HISTORY_MOVES] of TMove;
    Score                  : Integer; {estimated number of box-lines + box-changes}
    TailCount              : Integer; {trivial end-game pushes}
                             end;

  TBoxPositions            = array[0..MAX_BOX_COUNT] of Integer;
  TMoves                   = array[0..MAX_MOVES] of TMove;

  TGame                    = record
    Board                  : TBoard;  {current position: Board, BoxPos, PlayerPos, and BoardHashValue}
    Board3DLayerSize       : Integer; {(Width+2)*(Height*2): the '+2' is for a wall-filled border}
    BoardDepth             : Integer;
    BoardHashValue         : THashValue; {current position: Board, BoxPos, PlayerPos, and BoardHashValue}
    BoardHeight            : Integer;
    BoardSize              : Integer; {(Width+2)*(Height*2)*(Depth+2): the '+2' is for a wall-filled border}
    BoardWidth             : Integer;
    BoxCount               : Integer; {number of boxes/goals}
    BoxPos                 : TBoxPositions; {current position: Board, BoxPos, PlayerPos, and BoardHashValue}
    FloorCount             : Integer;
    GoalCount              : Integer; {number of boxes/goals}
    GoalPos                : TBoxPositions;
    HashSquareValues       : array[0..MAX_BOARD_SIZE] of THashValue; {values per square for the Zobrist hash key calculation}
    History                : THistory;
    PlayerPos              : Integer; {current position: Board, BoxPos, PlayerPos, and BoardHashValue}
    NeighbourSquareOffset  : array[TDirection] of Integer;
    PlayersReachableSquares: TBoardInteger;
    PlayersReachableSquaresTimeStamp  {by using a time-stamp it isn't necessary to perform a time-consuming}
                           : Integer; {reset each time the program calculates the player's reachable squares}
    ReverseMode            : Boolean; {true: generate level by pulling boxes away from goal positions}
    Title                  : String;
                             end;

  TGenerator               = record
    Board                  : TBoard;  {used for enumeration, random generation, etc.}
    BoxCount               : Integer;
    Count                  : Integer; {number of generated levels}
    FixedTemplate          : Boolean; {'True': input template contains fixed squares; the template is found in 'Board'}
    ItemIndex              : Integer; {items: walls, boxes, and player}
    ItemPos                : TBoardInteger; {interior walls, boxes, and player}
    PlayerIndex            : Integer; {auto-generated items: walls, boxes, and player}
    MaxBoxCount            : Integer; {maximum. number of boxes; the GA generator adds walls instead of boxes when this number is reached}
    MaxBoxCountEnabled     : Boolean; {enables/disables the limit}
    MaxOpenPositions       : Integer; {stop searching if the open queue exceeds this number of positions}
    MaxOpenPositionsEnabled: Boolean; {enables/disables the limit}
    Method                 : TGeneratorMethod;
    MidPoint               : Integer; {not exactly the middle, but [(Columns+1) div 2, (Rows+1) div 2]}
    MinPushCount           : Integer; {only save levels with at least this number of pushes}
    OutputFileName         : String;
    PlayerAccessAreaNo     : Integer;
    PrettyPrinting         : Boolean;
    RandomSeed             : Integer;
    RandomSeedEnabled      : Boolean; {use a random number as seed}
    StartNo                : Integer;
    TailPushesThreshold    : Integer; {>0: discount tail positions; <0: don't use tail position information in the fitness function; it's calculated for information only}
    TailPushesThresholdEnabled
                           : Boolean; {enables/disables the threshold}
    TemplatePlayerPos      : Integer; {fixed player-position, if any (forward generator only)}
    TrivialOpeningPushesIgnored
                           : Boolean; {'True': trivial forced opening pushes are discounted by the fitness function}
    VisitedPlayerPos       : TBoardBoolean;
    WallCount              : Integer;
                             end;

  PPosition                = ^TPosition;
  TPosition                = packed record  {'packed': literal sequence of unaligned slots, without gaps}
    HashValue              : THashValue;    {placed first for optimal addressing mode}
    Depth                  : Word;          {minimum number of pushes for reaching this position}
    Move                   : TMove;         {caution: 'Move.Direction' contains flags as well as the direction, hence, use 'TDirection(Ord(Move.Direction) and DIRECTION_BIT_MASK)' to extract the direction}
    Next                   : PPosition;     {linked list of open positions / later re-used for current path}
    Parent                 : PPosition;     {ancestor node}
    PlayerPos              : Word;
    Score                  : Word;          {estimated number of box-lines + box-changes for reaching this position}
                             end;

  TPositionLookupState     = record
    CollisionCount         : Integer;
    ItemIndex              : Integer;
    HashValue              : THashValue;
    VectorIndex            : Integer;
                             end;
  TPositionQueue           = record Count:Integer; Head,Tail: PPosition; end;
  TPositionVector          = array[0..(MaxInt div SizeOf(TPosition))-1] of TPosition;
  PPositionVector          = ^TPositionVector;

  TPositions               = record     {transposition table (hash table) and open-list}
    BestPosition           : PPosition; {best position found so far}
    Capacity               : Integer;   {total capacity}
    Count                  : Integer;   {number of stored positions}
    CurrentPosition        : PPosition; {Game.Board state matches this position}
    DroppedCount           : Integer;   {number of dropped positions}
    LookupState            : TPositionLookupState; {static variables for 'LookupPosition/GetFirstPosition/GetNextPosition}
    MemoryByteSize         : Integer;   {total memory size, all vectors (banks)}
    OpenPositions          : TPositionQueue; {breadth-first search: first-in-first-out set of positions}
    PositionsPerDepthCount : array[0..MAX_HISTORY_MOVES] of Integer;
    PositionVectors        : array[0..MEMORY_BLOCKS_COUNT-1] of Pointer{PPositionVector};
    RepetitionCount        : Integer;
    TailPositionCount      : Integer;   {number of trivial end-game positions (backward search only)}
    TimeStamp              : Integer;   {(not used) by using a time-stamp it isn't necessary to perform a time-consuming reset between each run}
    TotalCount             : Integer;   {current level total for all player access areas}
    TotalDroppedCount      : Integer;   {current level total for all player access areas}
    TotalRepetitionCount   : Integer;   {current level total for all player access areas}
    VectorCapacity         : Integer;   {capacity per vector (bank)}
                             end;

  TLevelReader             = record
    CharToBoardSquareValue : array[Low(Char)..High(Char)] of Byte;
    Count                  : Integer;
    CurrentTextLine        : String;
    InputFile              : TextFile;
    InputFileName          : String;
    IsABoardTemplate       : Boolean;  {'True': use a special character ('CH_FIXED_FLOOR') to show fixed floor squares, and prefix board lines with 'BOARD_TEXT_LINE_TEMPLATE_CHAR' so a level reader doesn't treat the template as a normal level}
    IsAGALogFile           : Boolean;  {'True': current file is a genetic-algorithm log file}
    XSBNotation            : Boolean;  {actually used by the level writer}
                             end;

  PLevelStatisticsItem     = ^TLevelStatisticsItem;
  TLevelStatisticsItem     = record
    ExhaustiveSearch       : Boolean;
    Height                 : Integer;
    Name                   : String;
    Next                   : PLevelStatisticsItem;
    PositionCount          : Integer;
    PushCount              : Integer;
    Score                  : Integer;
    Fitness                : Integer;
    TailCount              : Integer;
    Width                  : Integer;
                             end;

  TRandomState             = record RandomNumber:Integer; end;

{-----------------------------------------------------------------------------}

{Global Variables}
var
  Game                     : TGame;
  Generator                : TGenerator;
  LevelReader              : TLevelReader;
  LevelStatistics          : PLevelStatisticsItem;
  Positions                : TPositions;
  RandomState              : TRandomState;
{ GA                       : TGA; the genetic algorithm generator (see the GA section below) declares this global variable}

{=============================================================================}

{Genetic Algorithm Sokoban Level Generator}

{GA Types - required before declaring constants and other types}
type
  TGAFitnessFunction       = (ffPushes,ffScore);
  TGAMutationType          = (gaAddItem,gaDeleteItem,gaMoveItem,gaAddWall,gaDeleteWall,gaMoveWall); {order must not change}
  TGAMutationTypeAccumulatedFrequencies
                           = array[TGamutationType] of Integer;

{GA Constants}

const
  GA_DEFAULT_AUTO_SAVE_INTERVAL_TIME_MS
                           = 10 {minutes} * 60 {seconds} * ONE_THOUSAND {milliseconds};
  GA_DEFAULT_BOXES_GOALS_COUNT
                           = 4;
  GA_DEFAULT_CROSSOVER_PROBABILITY_PCT
                           = 50;
  GA_DEFAULT_GENERATE_CONNECTED_GOALS
                           = False;
  GA_DEFAULT_FILE_NAME     = TEXT_APPLICATION_TITLE+' Log.sok';
  GA_DEFAULT_FITNESS_FUNCTION
                           = ffPushes;
  GA_DEFAULT_INACTIVITY_THRESHOLD
                           = 20;
  GA_DEFAULT_POPULATION_SIZE
                           = 50; {default population size}
  GA_DEFAULT_MAX_CANDIDATE_COUNT
                           = 100 * ONE_THOUSAND;
  GA_DEFAULT_MAX_MUTATION_COUNT
                           = 2; {maximum number of mutations per candidate; the actual number of mutations 1..N is chosen randomly}
  GA_DEFAULT_MUTATION_PROBABILITY_PCT
                           = 80;
  GA_DEFAULT_MUTATION_TYPE_ACCUMULATED_FREQUENCIES : TGAMutationTypeAccumulatedFrequencies
                           = (10,20,70,80,90,100);
  GA_DEFAULT_OPEN_POSITIONS= 200 * ONE_THOUSAND;
  GA_DEFAULT_SHOW_STATUS_INTERVAL_TIME_MS
                           = 3 {seconds} * ONE_THOUSAND {milliseconds};
  GA_MAX_ALLELES           = MAX_BOARD_SIZE;
  GA_MAX_INDIVIDUALS       = 100; {maximum size of the population; 'GA_MAX_INDIVIDUALS' +'GA_MAX_PENDING_CANDIDATES' must be < High(Byte)}
  GA_MAX_MAX_MUTATION_COUNT= 10; {highest value allowed for the 'GA.Control.MaxMutationCount' option, i.e., the highest allowed number of mutations per candidate (an arbitrary limit)}
  GA_MAX_PENDING_CANDIDATES= 50; {when compiled as a plugin, the user may be able to inject new candidates via an editor while the generator is running}

{GA Texts} {don't localize}

  GA_TEXT_APPLICATION_TITLE_LONG
                           = TEXT_APPLICATION_TITLE+' - Yet Another Sokoban 3D Level Generator';
  GA_TEXT_CANDIDATE        = 'Candidate';
  GA_TEXT_CANDIDATE_NUMBER = 'Candidate number';
  GA_TEXT_CANDIDATES       = 'Candidates';
  GA_TEXT_CHILDREN         = 'Children';
  GA_TEXT_COPYRIGHT        = 'Copyright';
  GA_TEXT_CROSSOVERS       = 'Crossovers';
  GA_TEXT_DUPLICATES       = 'Duplicates';
  GA_TEXT_EVOLUTION        = 'Evolution';
  GA_TEXT_FILE_HEADER_LINES= 'Do not edit this file manually. It contains a Sokoban level candidate set '+
                             'for the '+TEXT_APPLICATION_TITLE+' level generator. The file must be formatted '+
                             'according to the syntax supported by the generator.';
  GA_TEXT_FITNESS          = 'Fitness';
  GA_TEXT_FITNESS_CALCULATION_TIME_MS
                           = 'Fitness calculation time, milliseconds';
  GA_TEXT_FITNESS_FUNCTION = 'Fitness function';
  GA_TEXT_FITNESS_FUNCTION_TYPES
                           : array[TGAFitnessFunction] of String
                           = ('Pushes','Box lines + box changes (estimates)');
  GA_TEXT_FIXED_TEMPLATE   = 'Fixed template';
  GA_TEXT_GENERATION       = 'Generation';
  GA_TEXT_INACTIVITY       = 'Inactivity';
  GA_TEXT_INACTIVITY_THRESHOLD
                           = 'Inactivity threshold';
  GA_TEXT_LAST_NEW_CANDIDATE
                           = 'Last new candidate';
  GA_TEXT_LEVEL_TYPE       = 'Level type';
  GA_TEXT_LEVEL_TYPES      : array[Boolean] of String = ('Goals scattered at random','Connected goals'); {'random' or 'connected goals'}
  GA_TEXT_LOAD_CANDIDATE_SET_FROM_FILE
                           = 'Load Candidate Set From File';
  GA_TEXT_MAX_BOXES        = 'Maximum number of boxes';
  GA_TEXT_MAX_OPEN_POSITIONS
                           = 'Maximum number of open positions';
  GA_TEXT_MAX_POPULATION_SIZE
                           = 'Maximum population';
  GA_TEXT_MEMORY_FULL_CANDIDATES
                           = 'Memory full, candidates';
  GA_TEXT_MUTATIONS        = 'Mutations';
  GA_TEXT_NOT_A_GA_FILE    = 'The file does not contain a candidate set for the YASGen Sokoban level generator';
  GA_TEXT_POPULATION       = 'Population';
  GA_TEXT_POPULATION_SIZE  = 'Current population';
  GA_TEXT_RANDOM_NUMBER_SEED
                           = 'Random number seed';
  GA_TEXT_SEARCH_DIRECTION = 'Search direction';
  GA_TEXT_SELECTIONS       = 'Selections';
  GA_TEXT_SETTINGS         = 'Settings';
  GA_TEXT_STATISTICS       = 'Statistics';
  GA_TEXT_SYNTAX_ERROR_OR_READ_ERROR_IN_FILE
                           = 'Syntax error or read error in file: ';
  GA_TEXT_TAIL_POSITION_THRESHOLD
                           = 'Tail position threshold';
  GA_TEXT_TESTED_CANDIDATES= 'Tested candidates';
  GA_TEXT_TIME_OF_BIRTH_MS = 'Time of birth, milliseconds';
  GA_TEXT_TRIVIAL_OPENING_PUSHES_IGNORED
                           = 'Trivial opening pushes ignored';
  GA_TEXT_TRIVIAL_PUSHES   = 'Trivial pushes';  
  GA_TEXT_VERSION          = 'Version';

{GA Types}

type
  TGAAlleles               = String;     {a candidate board; using a string is a convenient way to allocate the memory dynamically (in biology, a chromosome is a string of alleles)}

  TGAIndividual            = record
    Alleles                : TGAAlleles; {the chromosome (in biology, a chromosome is a string of alleles)}
    CandidateNo            : Integer;    {the generated candidates are sequentially numbered}
    ChildrenCount          : Integer;    {the number of times this individual has "mated", i.e., the number of times is has been used for creating new candidates}
    CrossOverTag           : Integer;    {individuals currently selected for cross-over are tagged with '1' and '2'; this information is used for updating the children counts}
    Fitness                : Integer;    {the estimated complexity of the candidate}
    PushCount              : Integer;    {number of pushes}
    Score                  : Integer;    {estimated box-lines + box-changes}
    TimeMS                 : TTimeMS;    {calculation time}
    TimeOfBirthMS          : TTimeMS;    {time of birth}
                             end;

  TGAExtendedIndividual    = record      {an extended individual contains a board in internal 'TBoard' format instead of alleles in 'TGAAlleles' format; the internal 'TBoard' format is more practical for the work flow}
    Board                  : TBoard;
    Individual             : TGAIndividual;
                             end;

  TGAControl               = record   {setup info}
    AutoSaveIntervalTimeMS : TTimeMS; {autosave periodically}
    CrossOverProbabilityPct: Integer;
    FitnessFunction        : TGAFitnessFunction;
    ForwardSearch          : Boolean;
    GenerateConnectedGoals : Boolean;
    GoodEnoughFitness      : Integer; {stop criteria}
    InactivityThreshold    : Integer; {drop an individual after this number of candidates have failed to improve the population}
    InactivityThresholdEnabled
                           : Boolean; {enables/disables the threshold}
    LastAutoSaveTimeMS     : TTimeMS; {last time the population was saved automatically}
//  MaxCandidateCount      : Integer; {stop criteria}
    MaxGenerationCount     : Integer; {stop criteria}
    MaxMutationCount       : Integer; {maximum number of mutations per candidate}
    MutateWallsOnly        : Boolean;
    MutationProbabilityPct : Integer;
    MutationTypeAccumulatedFrequencies
                           : TGAMutationTypeAccumulatedFrequencies;
    PopulationSize         : Integer; {population size}
                             end;

  PGAHistoryItem           = ^TGAHistoryItem;
  TGAHistoryItem           = record   {seen candidates, ordered as a simple binary tree}
    Key                    : THashValue;
    Left                   : PGAHistoryItem;
    Right                  : PGAHistoryItem;
                             end;

  TGAHistory               = record   {seen candidates}
    Count                  : Integer;
    Items                  : PGAHistoryItem;
    LoggedCount            : Integer;
                             end;

  TGAIndividuals           = array[0..GA_MAX_INDIVIDUALS+GA_MAX_PENDING_CANDIDATES] of TGAIndividual; {the population, item 0 unused}

  TGAStatistics            = record
    CandidatesCount        : Integer;
    CrossoverCount         : Integer;
    DuplicatesCount        : Integer;
    ElapsedTimeMS          : TTimeMS; {total running time}
    InactivityCount        : Integer; {number of individuals dropped as an attempt to increase the diversity after long periods of inactivity}
    LastNewCandidateNo     : Integer; {last time a new candidate was added}
    MemoryFullCandidatesCount
                           : Integer; {number of candidates where a full search failed because of memory exhaustion}
    MutationCount          : Integer;
    SelectionCount         : array[0..GA_MAX_INDIVIDUALS+GA_MAX_PENDING_CANDIDATES] of Integer;
                             end;

  TGA                      = record
    AllelesCount           : Integer; {number of alleles in the chromosomes (in the Sokoban case, the alleles are the board squares)}
    BestIndividualNo       : Integer; {best individual in the population}
    Control                : TGAControl; {setup information}
    FileName               : String;  {file name for the log file}
    GenerationCount        : Integer;
    History                : TGAHistory; {seen candidates}
    IndividualCount        : Integer; {population size + candidates which haven't been analysed yet}
    Individuals            : TGAIndividuals; {the population, item 0 unused}
    PriorSessionsTimeMS    : TTimeMS; {running time for prior sessions,if any; the total running time = prior sessions time + elapsed time since the current session started}
    StartTimeMS            : TTimeMS; {start time for the current session; the total running time = prior sessions time + elapsed time since the current session started}
    Statistics             : TGAStatistics;
//  SolutionAlleles        : TGAAlleles; {solution chromosome; the target for the search}
    WorstIndividualNo      : Integer; {worst individual in the population}
                             end;

{GA Global Variables}

var
  GA                       : TGA;

{-----------------------------------------------------------------------------}

{Forward Declarations}

function  BoardSquareValueToChar(Value__:Integer):Char; forward;
function  BoardToText(BoardWidth__,BoardHeight__,BoardDepth__:Integer; const Board__:TBoard; const LineSeparator__,LayerSeparator__:String):String; forward;
function  CalculateConnectedGoals(GoalSquare__,TimeStamp__,VisitedCount__:Integer;
                                  const Board__:TBoard; var VisitedSquares__:TBoardInteger):Integer; forward;
function  CalculateElapsedTimeMS(StartTimeMS__,StopTimeMS__:TTimeMS):TTimeMS; forward;
function  CalculatePlayerPath(FromSquare__,ToSquare__:Integer; MakeMoves__:Boolean; var MoveCount__:Integer; var Moves__:TMoves):Boolean; forward;
function  CalculatePlayersReachableSquares(var MinPlayerPos__:Integer):Integer; forward;
function  ColRowDepthToSquare(Col__,Row__,Depth__,ColCount__,RowCount__:Integer):Integer; forward;
function  Error({$IFDEF DELPHI} const {$ENDIF} Text__,Caption__:String):Boolean; forward;
function  ExtractFileNameWithoutExt(const FileName__:String):String; forward;
function  ExtractKeyValue(const Text__,Key__:String; var Value__:String):Boolean; forward;
function  FillTubes(MoveItemsEnabled__:Boolean):Integer; forward;
function  FirstMoveToText(var MoveNo__:Integer; var Text__:String):Boolean; forward;
function  GenerateNextBoard_Random:Boolean; forward;
function  GetNextPlayerAccessArea:Boolean; forward;
function  GetTimeMS:TTimeMS; forward;
procedure InitializeBoardGenerator(GeneratorMethod__:TGeneratorMethod;
                                   GameReverseMode__:Boolean;
                                   WallCount__,BoxCount__,RandomSeed__,MinPushCount__,
                                   MaxBoxCount__,
                                   MaxOpenPositions__,TemplatePlayerPos__,StartNo__,TailPushesThreshold__:Integer;
                                   PrettyPrinting__,MaxBoxCountEnabled__,MaxOpenPositionsEnabled__,TrivialOpeningPushesIgnored__,TailPushesThresholdEnabled__,RandomSeedEnabled__,FixedTemplate__:Boolean;
                                   const Board__:TBoard); forward;
procedure InitializeRandomState(RandomNumber__:Integer); forward;
function  KeyEnabledText(const Key__:String):String; forward;
function  LoadFirstLevelFromFile(FileName__:String):Boolean; forward;
function  LoadNextLevelFromFile(MinimumBoardWidth__,MinimumBoardHeight__,MinimumBoardDepth__:Integer):Boolean; forward;
function  LookupPosition(HashValue__:THashValue; PlayerPos__:Integer; var Position__:PPosition):Boolean; forward;
procedure InitializeGame; forward;
procedure InitializeBoard(BoardWidth__,BoardHeight__,BoardDepth__:Integer; FillSquares__:Boolean); forward;
function  IntToStr(i:Integer):String; forward;
function  LoadAndNormalizeBoard(var Board__:TBoard):Integer; forward;
procedure MakeBoard(BoardWidth__,BoardHeight__,BoardDepth__:Integer); forward;
function  MakeLevelFrom3DimensionalBoard(BoardWidth__,BoardHeight__,BoardDepth__,MinBoardWidth__,MinBoardHeight__,MinBoardDepth__:Integer; const Board3D__:TBoard3D):Boolean; forward;
function  Max(a__,b__:Integer):Integer; forward;
function  Min(a__,b__:Integer):Integer; forward;
procedure Msg({$IFDEF DELPHI} const {$ENDIF} Text__,Caption__:String); forward;
procedure MoveBoxesToGoalSquares; forward;
procedure MovePlayer(PlayerPos__:Integer); forward;
function  NextMoveToText(var MoveNo__:Integer; var Text__:String):Boolean; forward;
function  Random(Range__:Integer):Integer; forward;
procedure RemovePositionFromOpenPositions(Position__:PPosition); forward;
function  SaveLevelToFile(const FileName__,LevelName__:String; PrettyPrinting__,GuardAgainstTrimming__,PrintPositionsPerDepth__:Boolean):Boolean; forward;
function  SearchPlayerAccessAreas(const Board__:TBoard):Integer; forward;
procedure SetPosition(Position__:PPosition); forward;
procedure ShowBoard; forward;
function  TrimBoard:Boolean; forward;
procedure ShowTitle; forward;
procedure SquareToColRowDepth(Square__:Integer; var Col__,Row__,Depth__:Integer); forward;
function  StrBeginsWith(const Text__,Key__:String):Boolean; forward;
function  StrToInt(const Str__:String; var Value__:Integer):Boolean; forward;
function  StrTrim(const Str__:String):String; forward;
function  WriteLevelToFile({const} var F:Text; const LevelName__:String; PrettyPrinting__,GuardAgainstTrimming__,PrintPositionsPerDepth__:Boolean):Boolean; forward;

{-----------------------------------------------------------------------------}

{GA Forward Declarations}

function  GACalculateHashKey(const Board__:TBoard):THashValue; forward;
procedure GAClear; forward;
procedure GAClearPopulation; forward;
procedure GAClearStatistics; forward;
function  GAFitnessValue(FitnessFunction__:TGAFitnessFunction; PushCount__,Score__:Integer):Integer; forward;
procedure GAFitnessValueToPushesAndScore(FitnessFunction__:TGAFitnessFunction; Value__:Integer; var PushCount__,Score__:Integer); forward;
function  GAHistoryLookup(Key__:THashValue):PGAHistoryItem; forward;
function  GAIdenticalBoards(const Board1__,Board2__:TBoard):Boolean; forward;
function  GASaveToFile(const FileName__:String):Boolean; forward;
procedure GAShowIndividual(No__:Integer; const Individual__:TGAIndividual); forward;
function  GAWriteIndividualToFile(No__:Integer; const Individual__:TGAIndividual; {const} var F__:TextFile; PrettyPrinting__:Boolean):Boolean; forward;

{-----------------------------------------------------------------------------}

{GA General Utilities}

function RandomBoolean:Boolean;
begin {Returns a random boolean value, i.e., 'True' or 'False'}
  Result:=Random(2)=1;
end;

{-----------------------------------------------------------------------------}

{GA Utilities}

function  GAAddIndividual(const Individual__:TGAIndividual):Integer;
begin {Inserts an individual in the population, sorted on fitness}
  if   GA.IndividualCount<High(GA.Individuals) then with GA do with Control do begin
       Result:=IndividualCount;
       while (Result>=1)
             and
             (( Individual__.Fitness  > Individuals[Result].Fitness) {'>': higher is better}
              or
              ((Individual__.Fitness  = Individuals[Result].Fitness)
               and
               (Individual__.PushCount> Individuals[Result].PushCount)
              )) do begin
             Individuals[Succ(Result)]:=Individuals[Result]; Dec(Result); {insertion sort, descending order}
             end;
       Inc(Result);
       Individuals[Result]:=Individual__; Inc(IndividualCount);
       BestIndividualNo:=1; WorstIndividualNo:=Min(IndividualCount,PopulationSize);
       end
  else Result:=-1;
end;

function GAAllelesToBoard(const Alleles__:TGAAlleles; var Board__:TBoard):Boolean;
var Col,Row,Depth,RowOffset,Index:Integer;
begin
  Result:=Length(Alleles__)=GA.AllelesCount;
  if   Result then begin
       for Index:=0 to Game.BoardSize do Board__[Index]:=WALL;
       Index:=0;
       for Depth:=1 to Game.BoardDepth do
           for Row:=1 to Game.BoardHeight do begin
               RowOffset:=ColRowDepthToSquare(0,Row,Depth,Game.BoardWidth,Game.BoardHeight);
               for Col:=1 to Game.BoardWidth do begin
                   Inc(Index);
                   Board__[RowOffset+Col]:=LevelReader.CharToBoardSquareValue[Alleles__[Index]];
                   end;
           end;
       end
  else for Index:=Low(Board__) to High(Board__) do Board__[Index]:=WALL;
end;

procedure GABoardToAlleles_AllSquaresOrFixedFlagsOnly(const Board__:TBoard; var Alleles__:TGAAlleles; FixedFlagsOnly__:Boolean);
var Col,Row,Depth,RowOffset,Index:Integer; Character:Char;
begin
  Index:=0;
  if  Length(Alleles__)<>GA.AllelesCount then SetLength(Alleles__,GA.AllelesCount);
  for Depth:=1 to Game.BoardDepth do
      for Row:=1 to Game.BoardHeight do begin
          RowOffset:=ColRowDepthToSquare(0,Row,Depth,Game.BoardWidth,Game.BoardHeight);
          for Col:=1 to Game.BoardWidth do begin
              Inc(Index);
              if      not FixedFlagsOnly__ then begin
                      Character:=BoardSquareValueToChar(Board__[RowOffset+Col]);
                      if (Board__[RowOffset+Col] and FLAG_FIXED_SQUARE)<>0 then
                         Character:=Chr(Ord(Character) or FLAG_FIXED_SQUARE);
                      Alleles__[Index]:=Character;
                      end
              else if (Board__[RowOffset+Col] and FLAG_FIXED_SQUARE)<>0 then
                      Alleles__[Index]:=Chr(Ord(Alleles__[Index]) or FLAG_FIXED_SQUARE);
              end;
          end;
end;

procedure GABoardToAlleles(const Board__:TBoard; var Alleles__:TGAAlleles);
begin
  GABoardToAlleles_AllSquaresOrFixedFlagsOnly(Board__,Alleles__,False);
end;

function  GACalculateElapsedTimeMS:TTimeMS;
begin
  with GA do
    if   StartTimeMS<>0 then
         Result:=PriorSessionsTimeMS+CalculateElapsedTimeMS(StartTimeMS,GetTimeMS)
    else Result:=PriorSessionsTimeMS;
end;

function  GACalculateFitnessSum:Integer;
var i:Integer;
begin {Returns sum of fitness values for the population}
  Result:=0;
  for i:=1 to GA.IndividualCount do Inc(Result,GA.Individuals[i].Fitness);
end;

function  GACandidateName(IndividualNo__:Integer):String;
begin
  if   (IndividualNo__>=1) and (IndividualNo__<=GA.IndividualCount) then with GA.Individuals[IndividualNo__] do begin
       if   CandidateNo<>0 then
            Result:=GA_TEXT_CANDIDATE+SPACE+IntToStr(CandidateNo)
       else Result:=GA_TEXT_CANDIDATE+SPACE+IntToStr(IndividualNo__);
       end
  else Result:='';
end;

procedure GADeleteIndividual(IndividualNo__:Integer);
var i:Integer;
begin {Deletes an individual from the population; when compiled as a plugin, the host is responsible for deleting data on the host side}
  with GA do with Control do begin
    for i:=IndividualNo__ to Pred(IndividualCount) do begin
        Individuals[i]:=Individuals[Succ(i)];
        end;
    Individuals[IndividualCount].Alleles:='';
    Dec(IndividualCount);
    if IndividualCount=0 then BestIndividualNo:=0;
    WorstIndividualNo:=Min(IndividualCount,PopulationSize);
    end;
end;

function  GAIsANewChromosome(const Board__:TBoard):Boolean;
//var i:Integer;
begin
//  Result:=True;
//  for i:=1 to GA.IndividualCount__ do
//      if GAIdenticalChromosomes(GA.Individuals[i].Alleles,Alleles__) then begin
//         Result:=False; exit;
//         end;
//  if Result then
     Result:=GaHistoryLookup(GACalculateHashKey(Board__))=nil;
end;

function  GAIndividualWithHighestChildCountExceptTheBest:Integer;
var i,HighestChildCount:Integer;
begin
  Result:=-1; HighestChildCount:=0;
  for i:=2 to GA.IndividualCount do with GA.Individuals[i] do
      if ChildrenCount>=HighestChildCount then begin
         Result:=i; HighestChildCount:=ChildrenCount;
         end;
end;

function  GARankSelection:Integer;
var i:Integer; AccumulatedRank:array[0..GA_MAX_INDIVIDUALS+GA_MAX_PENDING_CANDIDATES+1] of Integer;
begin {Returns the number of a randomly selected individual (1..count), using rank selection}
  if   GA.IndividualCount>=1 then with GA do with Control do begin
       AccumulatedRank[0]:=0;
       for i:=1 to IndividualCount do begin {individuals are sorted on fitness, best first}
           AccumulatedRank[i]:=AccumulatedRank[Pred(i)]+Succ(IndividualCount)-i;
           end;
       Result:=1; i:=Succ(Random(AccumulatedRank[IndividualCount]));
       while i>AccumulatedRank[Result] do Inc(Result);
       end
  else Result:=0;
  Inc(GA.Statistics.SelectionCount[Result]);
end;

{-----------------------------------------------------------------------------}

{GA History}

function  GAHistoryAdd(Key__:THashValue):PGAHistoryItem;
var p,q:PGAHistoryItem;
begin
//{$HINTS OFF} Result:=nil; {$HINTS ON} {'nil': in case 'GetMem' fails and returns 'nil' instead of raising an exception}
  GetMem(Result,SizeOf(Result^));
  if Result<>nil then with Result^ do begin
     FillChar(Result^,SizeOf(Result^),0);
     Key:=Key__;
     p:=GA.History.Items; q:=nil;
     while p<>nil do begin {binary tree search}
       q:=p;
       if Key__<=p^.Key then p:=p^.Left
       else p:=p^.Right;
       end;
     if   q=nil then GA.History.Items:=Result {first item}
     else if   Key<=q^.Key then q^.Left:=Result
          else q^.Right:=Result;
     Inc(GA.History.Count);
     end;
end;

function  GAHistoryAddItemToFile(Key__:THashValue):Boolean;
var i,j:Integer; F:TextFile;
begin {$I-}
  i:=Succ(GA.History.LoggedCount);
  if GA.FileName<>'' then with GA do with Statistics do begin
     Assign(F,FileName); Append(F);
     ElapsedTimeMS:=GACalculateElapsedTimeMS;
     j:=Integer(ElapsedTimeMS);
     Writeln(F,i,EQUAL,
               GenerationCount    ,SPACE,Generator.RandomSeed,SPACE,
               CandidatesCount    ,SPACE,DuplicatesCount     ,SPACE,
               InactivityCount    ,SPACE,LastNewCandidateNo  ,SPACE,
               CrossOverCount     ,SPACE,MutationCount       ,SPACE,
               j                  ,SPACE,Key__);
     CloseFile(F);
     end;
  Result:=IOResult=0;
  if Result then GA.History.LoggedCount:=i;
end; {$I+}

procedure GAHistoryClear;
var ListHead:TGAHistoryItem; Next:PGAHistoryItem;

  procedure Destroy(Item__:PGAHistoryItem);
  begin {(not in production) destroys the items in the tree using a simple recursive traversal; theoretically, it can overflow the stack}
    if Item__<>nil then with Item__^ do begin
       Destroy(Left); Destroy(Right); FreeMem(Item__);
       end;
  end;

  procedure TreeToList(ListHead__:PGAHistoryItem);
  var L,ListTail,Rest:PGAHistoryItem;
  begin
    ListTail:=ListHead__;           // 'ListTail' points to the last node of the 'flattened' part of the tree, i.e., the part with no left branches
    Rest:=ListTail^.Right;          // 'Rest' always points to 'ListTail^.Right' at the beginning of the 'while' loop
    while Rest<>nil do              // 'True': there are more nodes that haven't been 'flattened' yet, i.e., there may be more nodes having a left branch
      if Rest^.Left<>nil then begin // 'True': the node has a left branch, 'flatten' it by rotating the left branch up into to list
         L:=Rest^.Left;             //  ListTail          ListTail
         Rest^.Left:=L^.Right;      //      \     rotate      \
         L^.Right:=Rest;            //      Rest    =>         L <--- new 'Rest'
         Rest:=L;                   //      / \               / \
         ListTail^.Right:=L;        //     L                    Rest
         end                        //    / \                   / \
      else begin                    //       R                 R
         // no left branch, hence, proceed to the next node to the right
         ListTail:=Rest; Rest:=Rest^.Right;
         end;
  end; // TreeToList

begin {GAHistoryClear}
  {flatten the tree to a list; traversing the tree recursively could theoretically overflow the stack}
  ListHead.Right:=GA.History.Items;
  TreeToList(PGAHistoryItem(Addr(ListHead)));
  GA.History.Items:=ListHead.Right;

  {free the items on the list}
  while GA.History.Items<>nil do begin
    Next:=GA.History.Items^.Right; {the items on the flattened list are linked by their 'Right' pointers}
    FreeMem(GA.History.Items);
    GA.History.Items:=Next;
    end;

  FillChar(GA.History,SizeOf(GA.History),0);
end;

function  GAHistoryLookup(Key__:THashValue):PGAHistoryItem;
begin
  Result:=GA.History.Items;
  while (Result<>nil) and (Key__<>Result^.Key) do {binary tree search}
    if   Key__<Result^.Key then Result:=Result^.Left
    else Result:=Result^.Right;
end;

function  GAHistoryWriteToFile(const Text__:String; Item__:PGAHistoryItem; {const} var F__:TextFile; Depth__:Integer; var ItemNumber__:Integer):Boolean;
const MAX_TREE_DEPTH=50*ONE_THOUSAND; {guard against stack overflow}
begin {$I-}
  if        Item__=nil then Result:=True
  else if   Depth__<=MAX_TREE_DEPTH then with Item__^ do begin
            Inc(ItemNumber__);
            Writeln(F__,ItemNumber__,EQUAL,Text__,SPACE,Key);
            Result:=(IOResult=0) and
                    GaHistoryWriteToFile(Text__,Left ,F__,Succ(Depth__),ItemNumber__) and
                    GAHistoryWriteToFile(Text__,Right,F__,Succ(Depth__),ItemNumber__);
            end
       else Result:=False; {guard against stack overflow; in practice it's very unlikely to happen, but the program must check it for completeness}
end; {$I+}

{-----------------------------------------------------------------------------}

{GA Input}

procedure GAAddBoardToCandidateSet(IndividualNo__:Integer; var OldBoardWidth__,OldBoardHeight__,OldBoardDepth__:Integer);
var Index:Integer;

  procedure ResizeAlleles(OldBoardWidth__,OldBoardHeight__,OldBoardDepth__,NewBoardWidth__,NewBoardHeight__,NewBoardDepth__:Integer; NormalizeBoard__:Boolean; var Alleles__:TGAAlleles);
  var Col,Row,Depth,Index,AllelesLength:Integer; Board3D:TBoard3D;
  begin {resizes alleles (a board) and installs the new dimensions as global values for all candidates}
    GA.AllelesCount:=NewBoardWidth__*NewBoardHeight__*NewBoardDepth__; {update the number of alleles in the chromosomes}
    AllelesLength:=Length(Alleles__);

    if (AllelesLength<>0) and
       ((OldBoardWidth__<>NewBoardWidth__) or (OldBoardHeight__<>NewBoardHeight__) or (OldBoardDepth__<>NewBoardDepth__) or (AllelesLength<>GA.AllelesCount)) then begin
       for Row:=0 to MAX_BOARD_HEIGHT+1 do
           for Col:=0 to MAX_BOARD_WIDTH+1 do
               for Depth:=0 to MAX_BOARD_DEPTH+1 do Board3D[Depth,Row,Col]:=WALL; {first fill the board with walls}
       Index:=0;
       for Depth:=1 to OldBoardDepth__ do
           for Row:=1 to OldBoardHeight__ do {then unpack the alleles from their 1-dimensional vector to a 3-dimensional board}
               for Col:=1 to OldBoardWidth__ do begin
                   Inc(Index);
                   if Index<=AllelesLength then Board3D[Depth,Row,Col]:=LevelReader.CharToBoardSquareValue[Alleles__[Index]];
                   end;

       {pack the 3-dimensional board to the 1-dimensional vector, using the new board dimensions}
       if AllelesLength<>GA.AllelesCount then SetLength(Alleles__,GA.AllelesCount);
       // for Square:=1 to Length(Alleles__) do Alleles__[Square]:=CH_WALL;
       Index:=0;
       for Depth:=1 to NewBoardDepth__ do
           for Row:=1 to NewBoardHeight__ do
               for Col:=1 to NewBoardWidth__ do begin
                   Inc(Index); Alleles__[Index]:=BoardSquareValueToChar(Board3D[Depth,Row,Col]);
                   end;
       end;

    if (Game.BoardHeight<>NewBoardHeight__) or (Game.BoardWidth<>NewBoardWidth__) or (Game.BoardDepth<>NewBoardDepth__) then begin
       Game.BoardHeight:=NewBoardHeight__; Game.BoardWidth:=NewBoardWidth__; Game.BoardDepth:=NewBoardDepth__; {update the global game board dimensions}
       InitializeBoard(Game.BoardWidth,Game.BoardHeight,Game.BoardDepth,True); {calculate internal data, e.g., 'Game.BoardSize'}
       end;

    if NormalizeBoard__ and (AllelesLength<>0) and GAAllelesToBoard(Alleles__,Game.Board) then begin {normalize the alleles, i.e., fill all unused squares with walls}
       LoadAndNormalizeBoard(Game.Board);
       GABoardToAlleles(Game.Board,Alleles__);
       end;
  end;

  procedure ResizeTemplate(OldBoardWidth__,OldBoardHeight__,OldBoardDepth__,NewBoardWidth__,NewBoardHeight__,NewBoardDepth__:Integer);
  var Square:Integer; Alleles:TGAAlleles;
  begin
    InitializeBoard(OldBoardWidth__,OldBoardHeight__,OldBoardDepth__,False); // restore the old board dimensions, matching the template dimensions
    GA.AllelesCount:=OldBoardWidth__*OldBoardHeight__*OldBoardDepth__;
    Alleles:=SPACE;
    GABoardToAlleles(Generator.Board,Alleles);
    ResizeAlleles(OldBoardWidth__,OldBoardHeight__,OldBoardDepth__,NewBoardWidth__,NewBoardHeight__,NewBoardDepth__,False,Alleles);
    GAAllelesToBoard(Alleles,Generator.Board);
    Generator.TemplatePlayerPos:=0;
    for Square:=0 to Game.BoardSize do
        if (Generator.Board[Square] and PLAYER)<>0 then
           Generator.TemplatePlayerPos:=Square;
  end;

begin {'GAAddBoardToCandidateSet'; creates alleles matching the current game state and assigns them to the candidate number 'IndividualNo__'}
  with GA do with Individuals[IndividualNo__] do begin
    Game.ReverseMode:=not GA.Control.ForwardSearch;

    if (Game.BoardWidth=OldBoardWidth__) and (Game.BoardHeight=OldBoardHeight__) then
       LoadAndNormalizeBoard(Game.Board); // ensure that unused squares are filled with walls

    Alleles:=BoardToText(Game.BoardWidth,Game.BoardHeight,Game.BoardDepth,Game.Board,'','');

    if (Game.BoardWidth<>OldBoardWidth__) or (Game.BoardHeight<>OldBoardHeight__) or (Game.BoardDepth<>OldBoardDepth__) then begin // 'True': the new candidate board has different dimensions than the boards belonging to the existing candidates
       ResizeAlleles(Game.BoardWidth,Game.BoardHeight,Game.BoardDepth,
                     Max(OldBoardWidth__ ,Game.BoardWidth),
                     Max(OldBoardHeight__,Game.BoardHeight),
                     Max(OldBoardDepth__ ,Game.BoardDepth),
                     True,GA.Individuals[IndividualNo__].Alleles); // resize the new candidate
       if (OldBoardWidth__<>Game.BoardWidth) or
          (OldBoardHeight__<>Game.BoardHeight) or
          (OldBoardDepth__<>Game.BoardDepth) then begin // 'True': the new candidate has bigger board dimensions than the existing ones; resize all candidates to the new bigger dimensions
          for Index:=1 to Max(IndividualNo__,IndividualCount) do
              if Index<>IndividualNo__ then ResizeAlleles (OldBoardWidth__,OldBoardHeight__,OldBoardDepth__,Game.BoardWidth,Game.BoardHeight,Game.BoardDepth,True,Individuals[Index].Alleles); // resize old candidates, if any
          if  Generator.FixedTemplate  then ResizeTemplate(OldBoardWidth__,OldBoardHeight__,OldBoardDepth__,Game.BoardWidth,Game.BoardHeight,Game.BoardDepth); // resize template
          end;
       end;
    GA.AllelesCount:=Game.BoardWidth*Game.BoardHeight*Game.BoardDepth;
    if Generator.FixedTemplate then GABoardToAlleles_AllSquaresOrFixedFlagsOnly(Generator.Board,Alleles,True); // mark fixed squares, if any
    OldBoardWidth__:=Game.BoardWidth; OldBoardHeight__:=Game.BoardHeight; OldBoardDepth__:=Game.BoardDepth; // update the current board dimensions for the candidate set members
    end;
end;

function  GAParseCandidateNotesTextLine(const Text__:String; IsACandidateSetMember__,UseCandidateNumbers__:Boolean; IndividualNo__:Integer):Boolean;
var i:Integer; s:String;
begin
  Result:=True;
  with GA do with Individuals[IndividualNo__] do begin
    if IsACandidateSetMember__ then begin
       s:=Text__;
       if ExtractKeyValue(s,GA_TEXT_FITNESS_FUNCTION_TYPES[ffPushes],s) and StrToInt(s,i) and
          (i>=0) and (i<=MAX_HISTORY_MOVES) then begin
          PushCount:=i;
          Fitness:=GAFitnessValue(GA.Control.FitnessFunction,PushCount,Score);
          end;
       if ExtractKeyValue(s,GA_TEXT_FITNESS_FUNCTION_TYPES[ffScore ],s) and StrToInt(s,i) and
          (i>=0) and (i<MAX_HISTORY_MOVES*2) then begin
          Score:=i;
          Fitness:=GAFitnessValue(GA.Control.FitnessFunction,PushCount,Score);
          end;
       if ExtractKeyValue(s,GA_TEXT_FITNESS,s) and StrToInt(s,i) and
          (i>=0) and (i<=((MAX_HISTORY_MOVES*(2*(MAX_HISTORY_MOVES+1)))+MAX_HISTORY_MOVES)) then begin // 'i<=...': i<=maximum fitness value
          Fitness:=i;
          end;
       if UseCandidateNumbers__ and
          ExtractKeyValue(s,GA_TEXT_CANDIDATE_NUMBER,s) and StrToInt(s,i) and
          (i>0) and (i<=Statistics.CandidatesCount) then CandidateNo:=i;
       if ExtractKeyValue(s,GA_TEXT_TIME_OF_BIRTH_MS,s) and StrToInt(s,i) then
          TimeOfBirthMS:=TTimeMS(i);
       if ExtractKeyValue(s,GA_TEXT_FITNESS_CALCULATION_TIME_MS,s) and StrToInt(s,i) then
          TimeMS:=TTimeMS(i);
       if ExtractKeyValue(s,GA_TEXT_CHILDREN,s) and StrToInt(s,i) and
          (i>=0) and (i<2*GenerationCount) then ChildrenCount:=i;
          end;
       end;
end;

function  GALoadFromFile(const FileName__:String):Boolean;  {Slightly specific to Sokoban}
type
  TParseState=(psNone,psSettings,psEvolution,psStatistics,psSelections,psFixedTemplate,psTestedCandidates,psCandidate,psCandidateNotes);
var
  BoardWidth,BoardHeight,BoardDepth,CandidateNo:Integer;
  s:String; ParseState:TParseState; TemplateBoard:TBoard;

  function ParseTextLine(const Text__:String):Boolean;
  var
    i,j,k,oMaxBoxCount:Integer;
    s:String; HashKey:THashValue;
    LevelType:Boolean; FitnessFunction:TGAFitnessFunction;

    function  GetNextInteger(const Text__:String; Int64__:Boolean; TextLength__:Integer; var Index__,Value__:Integer; var Int64Value__:Int64):Boolean;
    var Start:Integer;
    begin {returns the next integer starting from, or after, position 'Index__'; preconditions 'TextLength__' <= length of 'Text__' and 'TextLength__' < High('TextLength__')}
     Result:=False;
     while (Index__<=TextLength__) and
           (Text__[Index__]<>'-') and {all '-' in the string are assumed to incidate the beginning of a signed integer}
           ((Text__[Index__]<'0') or (Text__[Index__]>'9')) do Inc(Index__);
     if Index__<=TextLength__ then begin
        Start:=Index__;
        repeat Inc(Index__)
        until  (Index__>TextLength__) or (Text__[Index__]<'0') or (Text__[Index__]>'9');
        if   Int64__ then begin
             Val(Copy(Text__,Start,Index__-Start),Int64Value__,Start);
             Result:=Start=0;
             end
        else Result:=StrToInt(Copy(Text__,Start,Index__-Start),Value__);
        end;
    end;

    function  HasNumericKey(const Text__:String; var Key__:Integer; var Value__:String):Boolean;
    var Index,TextLength:Integer;
    begin {limitation: only non-negative key-numbers are considered}
      Index:=1;
      TextLength:=Length(Text__);
      while (Index<=TextLength) and (Text__[Index]>='0') and (Text__[Index]<='9') do Inc(Index);
      Result:=(Index>1) and
              (Index<=TextLength) and
              ((Text__[Index]=EQUAL) or (Text__[Index]=COLON)) and
              StrToInt(Copy(Text__,1,Pred(Index)),Key__);
      if Result then Value__:=StrTrim(Copy(Text__,Succ(Index),TextLength-Index));
    end;

    function  IsYes(const Text__:String):Boolean;
    begin
      Result:=Text__=TEXT_NO_YES[True];
    end;

  begin // ParseTextLine
    Result:=True;
    with GA do begin
               s:=Text__;
               if s<>'' then begin
                  if s[1]=LEFT_BRACKET then begin
                     if        System.Pos(GA_TEXT_SETTINGS         ,s)=2 then ParseState:=psSettings
                     else   if System.Pos(GA_TEXT_EVOLUTION        ,s)=2 then ParseState:=psEvolution
                     else   if System.Pos(GA_TEXT_STATISTICS       ,s)=2 then ParseState:=psStatistics
                     else   if System.Pos(GA_TEXT_SELECTIONS       ,s)=2 then ParseState:=psSelections
                     else   if System.Pos(GA_TEXT_FIXED_TEMPLATE   ,s)=2 then ParseState:=psFixedTemplate
                     else   if System.Pos(GA_TEXT_TESTED_CANDIDATES,s)=2 then ParseState:=psTestedCandidates
                     else   if System.Pos(GA_TEXT_CANDIDATE        ,s)=2 then ParseState:=psCandidate
                     else      ParseState:=psNone;
                     end
                  else
                     case ParseState of
                       psNone:             begin
                                           end;
                       psSettings:         begin
                                             if ExtractKeyValue(s,TEXT_BACKWARD_SEARCH,s) then
                                                Control.ForwardSearch:=not IsYes(s);
                                             if ExtractKeyValue(s,GA_TEXT_LEVEL_TYPE,s) then
                                                for LevelType:=Low(LevelType) to High(LevelType) do
                                                    if  StrBeginsWith(s,GA_TEXT_LEVEL_TYPES[LevelType]) then
                                                        Control.GenerateConnectedGoals:=LevelType;
                                             if ExtractKeyValue(s,GA_TEXT_FITNESS_FUNCTION,s) then
                                                for FitnessFunction:=Low(FitnessFunction) to High(FitnessFunction) do
                                                    if  StrBeginsWith(s,GA_TEXT_FITNESS_FUNCTION_TYPES[FitnessFunction]) then
                                                        Control.FitnessFunction:=FitnessFunction;
                                             if ExtractKeyValue(s,GA_TEXT_FIXED_TEMPLATE,s) then
                                                Generator.FixedTemplate:=IsYes(s);
                                             if ExtractKeyValue(s,GA_TEXT_INACTIVITY_THRESHOLD,s) and StrToInt(s,i) then begin
                                                Control.InactivityThreshold:=Max(0,i);
                                                Control.InactivityThresholdEnabled:=Control.InactivityThreshold>0;
                                                end;
                                             if ExtractKeyValue(s,KeyEnabledText(GA_TEXT_INACTIVITY_THRESHOLD),s) then
                                                Control.InactivityThresholdEnabled:=IsYes(s);
                                             if ExtractKeyValue(s,GA_TEXT_MAX_BOXES,s) and StrToInt(s,i) then begin
                                                Generator.MaxBoxCount:=Max(0,Min(MAX_BOX_COUNT,i));
                                                Generator.MaxBoxCountEnabled:=Generator.MaxBoxCount<>MAX_BOX_COUNT;
                                                end;
                                             if ExtractKeyValue(s,KeyEnabledText(GA_TEXT_MAX_BOXES),s) then
                                                Generator.MaxBoxCountEnabled:=IsYes(s);
                                             if ExtractKeyValue(s,GA_TEXT_MAX_OPEN_POSITIONS,s) then begin
                                                if      s=TEXT_UNLIMITED then
                                                        Generator.MaxOpenPositions:=High(Generator.MaxOpenPositions)
                                                else if StrToInt(s,i) then
                                                        if   i<=0 then
                                                             Generator.MaxOpenPositions:=High(Generator.MaxOpenPositions)
                                                        else Generator.MaxOpenPositions:=i;
                                                Generator.MaxOpenPositionsEnabled:=Generator.MaxOpenPositions<>High(Generator.MaxOpenPositions);
                                                end;
                                             if ExtractKeyValue(s,KeyEnabledText(GA_TEXT_MAX_OPEN_POSITIONS),s) then
                                                Generator.MaxOpenPositionsEnabled:=IsYes(s);
                                             if ExtractKeyValue(s,GA_TEXT_MAX_POPULATION_SIZE,s) and StrToInt(s,i) then begin
                                                Control.PopulationSize:=Max(0,Min(GA_MAX_INDIVIDUALS,i));
                                                end;
                                             if ExtractKeyValue(s,GA_TEXT_TRIVIAL_OPENING_PUSHES_IGNORED,s) then
                                                Generator.TrivialOpeningPushesIgnored:=IsYes(s);
                                             if ExtractKeyValue(s,GA_TEXT_TAIL_POSITION_THRESHOLD,s) and StrToInt(s,i) then begin
                                                Generator.TailPushesThreshold:=Max(-MAX_TAIL_POSITION_THRESHOLD,Min(MAX_TAIL_POSITION_THRESHOLD,i));
                                                Generator.TailPushesThresholdEnabled:=Generator.TailPushesThreshold>0;
                                                end;
                                             if ExtractKeyValue(s,KeyEnabledText(GA_TEXT_TAIL_POSITION_THRESHOLD),s) then
                                                Generator.TailPushesThresholdEnabled:=IsYes(s);
                                           end;
                       psEvolution:        begin
                                             if ExtractKeyValue(s,GA_TEXT_POPULATION_SIZE,s) and StrToInt(s,i) then begin
                                                //IndividualCount:=Max(0,Min(Control.PopulationSize,i));
                                                end;
                                             if ExtractKeyValue(s,GA_TEXT_GENERATION,s) and StrToInt(s,i) then
                                                GenerationCount:=Max(0,i);
                                             if ExtractKeyValue(s,GA_TEXT_RANDOM_NUMBER_SEED,s) and StrToInt(s,i) then
                                                Generator.RandomSeed:=Max(0,i);
                                             if ExtractKeyValue(s,KeyEnabledText(GA_TEXT_RANDOM_NUMBER_SEED),s) then
                                                Generator.RandomSeedEnabled:=IsYes(s);
                                           end;
                       psStatistics:       begin
                                             if ExtractKeyValue(s,GA_TEXT_CANDIDATES,s) and StrToInt(s,i) then
                                                Statistics.CandidatesCount:=Max(0,i);
                                             if ExtractKeyValue(s,GA_TEXT_CROSSOVERS,s) and StrToInt(s,i) then
                                                Statistics.CrossOverCount:=Max(0,i);
                                             if ExtractKeyValue(s,GA_TEXT_DUPLICATES,s) and StrToInt(s,i) then
                                                Statistics.DuplicatesCount:=Max(0,i);
                                             if ExtractKeyValue(s,GA_TEXT_INACTIVITY,s) and StrToInt(s,i) then
                                                Statistics.InactivityCount:=Max(0,i);
                                             if ExtractKeyValue(s,GA_TEXT_LAST_NEW_CANDIDATE,s) and StrToInt(s,i) then
                                                Statistics.LastNewCandidateNo:=Max(0,i);
                                             if ExtractKeyValue(s,GA_TEXT_MEMORY_FULL_CANDIDATES,s) and StrToInt(s,i) then
                                                Statistics.MemoryFullCandidatesCount:=Max(0,i);
                                             if ExtractKeyValue(s,GA_TEXT_MUTATIONS,s) and StrToInt(s,i) then
                                                Statistics.MutationCount:=Max(0,i);
                                             if ExtractKeyValue(s,TEXT_TIME_MS,s) and StrToInt(s,i) then
                                                PriorSessionsTimeMS:=TTimeMS(i);
                                           end;
                       psSelections:       begin
                                             if HasNumericKey(s,i,s) and StrToInt(s,j) and
                                                (i>=1) and (i<=GA_MAX_INDIVIDUALS) and (j>=0) then
                                                Statistics.SelectionCount[i]:=j;
                                           end;
                       psFixedTemplate:    begin
                                             LevelReader.CurrentTextLine:=s; {put the most recently read text line into the level reader buffer}
                                             if   LoadNextLevelFromFile(BoardWidth,BoardHeight,BoardDepth) then begin
                                                  BoardWidth      :=Max(BoardWidth,Game.BoardWidth);
                                                  BoardHeight     :=Max(BoardHeight,Game.BoardHeight);
                                                  BoardDepth      :=Max(BoardDepth,Game.BoardDepth);
                                                  Game.BoardWidth :=BoardWidth;
                                                  Game.BoardHeight:=BoardHeight;
                                                  Game.BoardDepth :=BoardDepth;
                                                  TemplateBoard   :=Game.Board;
                                                  with Generator do begin
                                                    // initialize the board generator before the candidates are loaded so the candidates can inherit fixed squares from the template board when they are loaded
                                                    InitializeBoardGenerator(Method,not Control.ForwardSearch,WallCount,BoxCount,RandomSeed,MinPushCount,MaxBoxCount,MaxOpenPositions,TemplatePlayerPos,StartNo,TailPushesThreshold,
                                                                             PrettyPrinting,MaxBoxCountEnabled,MaxOpenPositionsEnabled,TrivialOpeningPushesIgnored,TailPushesThresholdEnabled,RandomSeedEnabled,FixedTemplate,TemplateBoard);
                                                    end;
                                                  end
                                             else Generator.FixedTemplate:=False;
                                             ParseState:=psNone;
                                           end;
                       psTestedCandidates: with Statistics do begin
                                             j:=Length(s);
                                             i:=System.Pos(EQUAL,s); {skip leading sequence number}
                                             if i=0 then i:=System.Pos(COLON,s); {skip leading sequence number}
                                             if i=0 then i:=Succ(j); {'0': the line does not contain "=" or ":", hence, the line doesn't contain information about a tested candidate}
                                             if GetNextInteger(s,False,j,i,k,HashKey) then GenerationCount:=k;
                                             if GetNextInteger(s,False,j,i,k,HashKey) then Generator.RandomSeed:=Max(0,k);
                                             if GetNextInteger(s,False,j,i,k,HashKey) then CandidatesCount:=Max(0,k);
                                             if GetNextInteger(s,False,j,i,k,HashKey) then DuplicatesCount:=Max(0,k);
                                             if GetNextInteger(s,False,j,i,k,HashKey) then InactivityCount:=Max(0,k);
                                             if GetNextInteger(s,False,j,i,k,HashKey) then LastNewCandidateNo:=Max(0,k);
                                             if GetNextInteger(s,False,j,i,k,HashKey) then CrossOverCount:=Max(0,k);
                                             if GetNextInteger(s,False,j,i,k,HashKey) then MutationCount:=Max(0,k);
                                             if GetNextInteger(s,False,j,i,k,HashKey) then PriorSessionsTimeMS:=TTimeMS(k);
                                             if GetNextInteger(s,True ,j,i,k,HashKey) then GAHistoryAdd(HashKey);
                                           end;
                       psCandidate:        begin
                                             if   CandidateNo<High(Individuals) then begin
                                                  Inc(CandidateNo);
                                                  oMaxBoxCount:=Generator.MaxBoxCount;
                                                  Generator.MaxBoxCount:=MAX_BOX_COUNT; {so existing boards aren't truncated more than necessary}
                                                  LevelReader.CurrentTextLine:=s; {put the most recently read text line into the levelreader buffer}
                                                  Result:=LoadNextLevelFromFile(BoardWidth,BoardHeight,BoardDepth);
                                                  Generator.MaxBoxCount:=oMaxBoxCount; {restore maximum number of boxes}

                                                  if Result then begin
                                                     GAAddBoardToCandidateSet(CandidateNo,BoardWidth,BoardHeight,BoardDepth);
                                                     Individuals[CandidateNo].PushCount:=0;
                                                     Individuals[CandidateNo].Score:=0;
                                                     Individuals[CandidateNo].Fitness:=-1;
                                                     ParseState:=psCandidateNotes;
                                                     end;
                                                  end
                                             else begin // silently drop overflow candidates
                                                        // Result:=False;
                                                  end;
                                           end;
                       psCandidateNotes:   GAParseCandidateNotesTextLine(s,
                                                                         True,
                                                                         True,
                                                                         CandidateNo);
                     end; {case}
                  end
               else begin {empty line}
                  end;
      end;
  end;

begin {$I-} {GALoadFromFile}
  with GA do begin
    BoardWidth:=0; BoardHeight:=0; BoardDepth:=0; CandidateNo:=0;
    ParseState:=psNone;

    GAClear;
    Generator.FixedTemplate:=False;
    Control.PopulationSize:=GA_MAX_INDIVIDUALS;

    FileName:=FileName__;
    Result:=FileName<>'';
    if Result and (LevelReader.InputFileName<>FileName) then
       Result:=LoadFirstLevelFromFile(FileName);
    if Result then begin
       if LevelReader.IsAGALogFile then with Statistics do begin
          LevelReader.CurrentTextLine:='';
          Reset(LevelReader.InputFile); Result:=IOResult=0;
          while Result and
                (not (Eof(LevelReader.InputFile) and (LevelReader.CurrentTextLine=''))) do begin
            if   LevelReader.CurrentTextLine='' then with LevelReader do begin
                 Readln(InputFile,CurrentTextLine);
                 Result:=IOResult=0;
                 end;
            s:=LevelReader.CurrentTextLine; LevelReader.CurrentTextLine:='';

            if   Result then
                 Result:=ParseTextLine(s)
            else Result:=Error(TEXT_FILE_READ_ERROR,'');
            end;

          if not Result then Error(GA_TEXT_SYNTAX_ERROR_OR_READ_ERROR_IN_FILE+FileName,TEXT_APPLICATION_TITLE+' - '+GA_TEXT_LOAD_CANDIDATE_SET_FROM_FILE);
          end
       else Result:=Error(GA_TEXT_NOT_A_GA_FILE+COLON+SPACE+FileName,'');

       if LevelReader.InputFileName<>''  then begin {clean up, i.e., close the input file}
          LevelReader.InputFileName:=''; CloseFile(LevelReader.InputFile);
          end;

       Result:=(IOResult=0) and Result;
       end;
    IndividualCount:=CandidateNo;

    if IndividualCount>0 then BestIndividualNo:=1;
    WorstIndividualNo:=Min(IndividualCount,GA.Control.PopulationSize);
    GA.AllelesCount:=Game.BoardWidth*Game.BoardHeight*Game.BoardDepth;
    Game.ReverseMode:=not GA.Control.ForwardSearch;
    Statistics.ElapsedTimeMS:=PriorSessionsTimeMS;
    Statistics.CandidatesCount:=Max(Statistics.CandidatesCount,History.Count);
    Generator.Count:=Statistics.CandidatesCount;
    end;
end; {$I+}

{-----------------------------------------------------------------------------}

{GA Output}

function GASaveToFile(const FileName__:String):Boolean;  {Slightly specific to Sokoban}
var i,j,oHistoryCount:Integer;
    s,BakFileName,TempFileName:String;
    oBoard:TBoard; F:TextFile;

begin {$I-}
  Result:=FileName__<>'';
  if Result then with GA do begin
     FileName:=FileName__; {note that as a side-effect, the filename is updated with the most recently saved filename}
     TempFileName:=ExtractFileNameWithoutExt(FileName__);
     BakFileName :=TempFileName+'.bak';
     TempFileName:=TempFileName+'.tmp';

     if Result then begin
        Assign(F,TempFileName); Rewrite(F);
        Writeln(F,LEFT_BRACKET,GA_TEXT_APPLICATION_TITLE_LONG,RIGHT_BRACKET);
        Writeln(F);
        Writeln(F,GA_TEXT_FILE_HEADER_LINES);
        Writeln(F);

        Writeln(F,GA_TEXT_VERSION,COLON,SPACE,TEXT_APPLICATION_VERSION);
        Writeln(F,GA_TEXT_COPYRIGHT,COLON,SPACE,TEXT_APPLICATION_COPYRIGHT);
        Writeln(F);

        Writeln(F,LEFT_BRACKET,GA_TEXT_SETTINGS,RIGHT_BRACKET);
        s:=TEXT_NO_YES[not GA.Control.ForwardSearch];
        Writeln(F,TEXT_BACKWARD_SEARCH,COLON,SPACE,s);
        s:=GA_TEXT_LEVEL_TYPES[GA.Control.GenerateConnectedGoals];
        Writeln(F,GA_TEXT_LEVEL_TYPE,COLON,SPACE,s);
        s:=GA_TEXT_FITNESS_FUNCTION_TYPES[GA.Control.FitnessFunction]; {the FPC compiler has a bug so it can't write 'complex' variables to a file, hence, always write simple variables}
        Writeln(F,GA_TEXT_FITNESS_FUNCTION,COLON,SPACE,s);
        s:=TEXT_NO_YES[Generator.FixedTemplate];
        Writeln(F,GA_TEXT_FIXED_TEMPLATE,COLON,SPACE,s);
        Writeln(F,GA_TEXT_INACTIVITY_THRESHOLD,COLON,SPACE,GA.Control.InactivityThreshold);
        s:=TEXT_NO_YES[GA.Control.InactivityThresholdEnabled];
        Writeln(F,KeyEnabledText(GA_TEXT_INACTIVITY_THRESHOLD),COLON,SPACE,s);
        Writeln(F,GA_TEXT_MAX_BOXES,COLON,SPACE,Generator.MaxBoxCount);
        s:=TEXT_NO_YES[Generator.MaxBoxCountEnabled];
        Writeln(F,KeyEnabledText(GA_TEXT_MAX_BOXES),COLON,SPACE,s);
        if   Generator.MaxOpenPositions=High(Generator.MaxOpenPositions) then s:=TEXT_UNLIMITED
        else s:=IntToStr(Generator.MaxOpenPositions);
        Writeln(F,GA_TEXT_MAX_OPEN_POSITIONS,COLON,SPACE,s);
        s:=TEXT_NO_YES[Generator.MaxOpenPositionsEnabled];
        Writeln(F,KeyEnabledText(GA_TEXT_MAX_OPEN_POSITIONS),COLON,SPACE,s);
        Writeln(F,GA_TEXT_MAX_POPULATION_SIZE,COLON,SPACE,GA.Control.PopulationSize);
        s:=TEXT_NO_YES[Generator.TrivialOpeningPushesIgnored];
        Writeln(F,GA_TEXT_TRIVIAL_OPENING_PUSHES_IGNORED,COLON,SPACE,s);
        Writeln(F,GA_TEXT_TAIL_POSITION_THRESHOLD,COLON,SPACE,Generator.TailPushesThreshold);
        s:=TEXT_NO_YES[Generator.TailPushesThresholdEnabled];
        Writeln(F,KeyEnabledText(GA_TEXT_TAIL_POSITION_THRESHOLD),COLON,SPACE,s);
        Writeln(F);

        Writeln(F,LEFT_BRACKET,GA_TEXT_EVOLUTION,RIGHT_BRACKET);
        Writeln(F,GA_TEXT_POPULATION_SIZE,COLON,SPACE,IndividualCount);
        Writeln(F,GA_TEXT_GENERATION,COLON,SPACE,GenerationCount);
        Writeln(F,GA_TEXT_RANDOM_NUMBER_SEED,COLON,SPACE,Generator.RandomSeed);
        s:=TEXT_NO_YES[Generator.RandomSeedEnabled];
        Writeln(F,KeyEnabledText(GA_TEXT_RANDOM_NUMBER_SEED),COLON,SPACE,s);
        Writeln(F);

        Writeln(F,LEFT_BRACKET,GA_TEXT_STATISTICS,RIGHT_BRACKET);
        Writeln(F,GA_TEXT_CANDIDATES,COLON,SPACE,GA.Statistics.CandidatesCount);
        Writeln(F,GA_TEXT_CROSSOVERS,COLON,SPACE,GA.Statistics.CrossoverCount);
        Writeln(F,GA_TEXT_DUPLICATES,COLON,SPACE,GA.Statistics.DuplicatesCount);
        Writeln(F,GA_TEXT_INACTIVITY,COLON,SPACE,GA.Statistics.InactivityCount);
        Writeln(F,GA_TEXT_LAST_NEW_CANDIDATE,COLON,SPACE,GA.Statistics.LastNewCandidateNo);
        Writeln(F,GA_TEXT_MEMORY_FULL_CANDIDATES,COLON,SPACE,GA.Statistics.MemoryFullCandidatesCount);
        Writeln(F,GA_TEXT_MUTATIONS,COLON,SPACE,GA.Statistics.MutationCount);
        i:=Integer(GACalculateElapsedTimeMS);
        Writeln(F,TEXT_TIME_MS,COLON,SPACE,i); // for conveniency, write the time as an integer because the reader uses a common 'read-integer' sub-function to load information
        Writeln(F);

        Writeln(F,LEFT_BRACKET,GA_TEXT_SELECTIONS,RIGHT_BRACKET);
        for i:=1 to Min(IndividualCount,Control.PopulationSize) do begin
            j:=Statistics.SelectionCount[i];
            Writeln(F,i,COLON,SPACE,j);
            end;

        if   Generator.FixedTemplate then begin
             Writeln(F);
             Writeln(F,LEFT_BRACKET,GA_TEXT_FIXED_TEMPLATE,RIGHT_BRACKET);

             oBoard:=Game.Board;
             oHistoryCount:=Game.History.Count; Game.History.Count:=0;

             for i:=0 to Game.BoardSize do begin
                 j:=Generator.Board[i];
                 if   (j and FLAG_FIXED_SQUARE)=0 then
                      Game.Board[i]:=FLOOR
                 else Game.Board[i]:=j;
                 end;
             LevelReader.IsABoardTemplate:=True;
             WriteLevelToFile(F,'',False,False,False);
             LevelReader.IsABoardTemplate:=False;
             Game.Board:=oBoard; Game.History.Count:=oHistoryCount;
             end;

        for i:=1 to GA.IndividualCount do
            if Result then Result:=GAWriteIndividualToFile(i,GA.Individuals[i],F,
                                     i=1  {'1': pretty-print the first (the best) candidate only; the data file is saved each time a new member is added to the population, so it's better to limit the pretty-printing time}
                                   );

        Writeln(F);
        Writeln(F);
        Writeln(F,LEFT_BRACKET,GA_TEXT_TESTED_CANDIDATES,RIGHT_BRACKET);
        GA.History.LoggedCount:=0;
        with Statistics do begin
          ElapsedTimeMS:=GACalculateElapsedTimeMS;
          Result:=Result and
                  GAHistoryWriteToFile(IntToStr(GenerationCount      )+SPACE+
                                       IntTostr(Generator.RandomSeed )+SPACE+
                                       IntTostr(CandidatesCount      )+SPACE+
                                       IntTostr(DuplicatesCount      )+SPACE+
                                       IntTostr(InactivityCount      )+SPACE+
                                       IntTostr(LastNewCandidateNo   )+SPACE+
                                       IntTostr(CrossOverCount       )+SPACE+
                                       IntToStr(MutationCount        )+SPACE+
                                       IntToStr(Integer(ElapsedTimeMS)),
                                       History.Items,F,0,GA.History.LoggedCount);
          end;

        CloseFile(F);
        Result:=(IOResult=0) and Result;

        if Result then begin
           {the user can terminate the console program at any time by pressing  }
           {[Ctrl]+C, hence, it's neccessary to use a renaming-scheme instead of}
           {simple overwriting the previous output file; this ensures that the  }
           {user always can find a consistent candidate set in one of these     }
           {files: 'filename__', 'filename__.bak' or 'filename__.tmp'           }
           Assign(F,BakFileName ); Erase (F            ); if IOResult<>0 then;  {delete   '.bak'-file, if any}
           Assign(F,FileName    ); Rename(F,BakFileName); if IOResult<>0 then;  {rename original file, if any}
           Assign(F,TempFileName); Rename(F,FileName   ); Result:=IOResult=0;   {rename new file}
           {$IFDEF PLUGIN}
             //if Result then begin {'True': saving the file succeeded; delete the backup file}
             //   Assign(F,BakFileName ); Erase (F); if IOResult<>0 then;         {delete   '.bak'-file, if any}
             //   end;
           {$ENDIF}
           end;

        if not Result then Error(TEXT_FILE_WRITE_ERROR,'');
        end;
     end;
end; {$I+}

procedure GAShowPopulation(PressEnter__:Boolean);
{$IFDEF CONSOLE_APPLICATION}
  var i:Integer;
  begin
    with GA do with Control do begin
      Writeln; Writeln('Generation: ',GenerationCount);
      for i:=1 to IndividualCount do begin
          GAShowIndividual(i,Individuals[i]);
          if PressEnter__ then Msg('','');
          end;
      end;
  end;
{$ELSE}
  {$IFDEF SILENT_APPLICATION}
    begin
      Windows.MessageBox(0,'GAShowPopulation',TEXT_APPLICATION_TITLE,MB_OK);
    end;
  {$ELSE}
    begin
    end;
  {$ENDIF}
{$ENDIF}
{-----------------------------------------------------------------------------}

{GA (Genetic Algorithm) - Sokoban specific functions and procedures}

function GACalculateHashKey(const Board__:TBoard):THashValue; {Sokoban specific}
var i,Col,Row,Depth,BoxOrGoalMask,BoardSquareValue:Integer;
begin
  Result:=0;
  if   Game.ReverseMode then BoxOrGoalMask:=GOAL
  else BoxOrGoalMask:=BOX;
  for i:=1 to Game.BoardSize do begin
      BoardSquareValue:=Board__[i] and (not FLAG_FIXED_SQUARE);
      if (BoardSquareValue and BoxOrGoalMask)<>0 then Result:=Result xor Game.HashSquareValues[i];
      if (BoardSquareValue and WALL         )<>0 then begin
         SquareToColRowDepth(i,Col,Row,Depth);
         if (Col>1) and (Col<Game.BoardWidth) and
            (Row>1) and (Row<Game.BoardHeight) and
            (Depth>1) and (Depth<Game.BoardDepth) then {only interior walls are considered}
            Result:=Result xor (-Game.HashSquareValues[i]);
         end;
      end;
  if Result=0 then Result:=1; {'0' is a reserved value}
end;

function GAFitnessValue(FitnessFunction__:TGAFitnessFunction; PushCount__,Score__:Integer):Integer; {Returns 'fitness value' for a position, using either pushes or score as first criteria}
begin {Precondition: 'Score__' <= 2*MAX_HISTORY_MOVES}
  if   FitnessFunction__=ffScore then
       Result:=(Score__    *(2*(MAX_HISTORY_MOVES+1)))+PushCount__
  else Result:=(PushCount__*(2*(MAX_HISTORY_MOVES+1)))+Score__;
end;

procedure GAFitnessValueToPushesAndScore(FitnessFunction__:TGAFitnessFunction; Value__:Integer; var PushCount__,Score__:Integer);
begin
  if FitnessFunction__=ffScore then begin
     PushCount__:= Value__ mod (2*(MAX_HISTORY_MOVES+1));
     Score__    := Value__ div (2*(MAX_HISTORY_MOVES+1));
     end
  else begin
     PushCount__:= Value__ div (2*(MAX_HISTORY_MOVES+1));
     Score__    := Value__ mod (2*(MAX_HISTORY_MOVES+1));
     end;
end;

function GAIdenticalBoards(const Board1__,Board2__:TBoard):Boolean; {Sokoban specific}
var i:Integer;
begin
  Result:=True;
  for i:=1 to Game.BoardSize do
      if (Board1__[i] and (WALL+GOAL)) <> (Board2__[i] and (WALL+GOAL)) then begin
         Result:=False; exit;
         end;
end;

function GAIdenticalChromosomes(const Alleles1__,Alleles2__:TGAAlleles):Boolean; {Sokoban specific}
var i:Integer;
begin
  Result:=Length(Alleles1__)=Length(Alleles2__);
  if Result then
     for i:=1 to Length(Alleles1__) do
         if (LevelReader.CharToBoardSquareValue[Alleles1__[i]] and (WALL+GOAL))
            <>
            (LevelReader.CharToBoardSquareValue[Alleles2__[i]] and (WALL+GOAL)) then begin
            Result:=False; exit;
            end;
end;

function GALoadAllelesFromGame:Integer; {Sokoban specific}
var Col,Row,Depth,Square,RowOffset,PlayerPos:Integer;
begin
  with GA do with Individuals[0] do  begin {use item[0] as temporary item}
    PlayerPos:=0;
    if not GA.Control.ForwardSearch then begin
       MoveBoxesToGoalSquares;
       for Square:=0 to Game.BoardSize do {remove the player from the board}
           if (Game.Board[Square] and PLAYER)<>0 then begin
              PlayerPos:=Square; Dec(Game.Board[PlayerPos],PLAYER);
              end;
       end;

    AllelesCount:=Game.BoardWidth*Game.BoardHeight*Game.BoardDepth;
    GABoardToAlleles(Game.Board,Alleles);

    if PlayerPos<>0 then Inc(Game.Board[PlayerPos],PLAYER); {put the player back on the board, if any}

    Fitness:=-1; {'-1': not calculated yet}
    for Depth:=1 to Game.BoardDepth do
        for Row:=1 to Game.BoardHeight do begin {fill squares outside the exterior wall}
            RowOffset:=ColRowDepthToSquare(0,Row,Depth,Game.BoardWidth,Game.BoardHeight);
            Col:=1;
            while (Col<=Game.BoardWidth) and ((Game.Board[RowOffset+Col] and WALL)=0) do begin
              Alleles[(Pred(Depth)*Game.BoardWidth*Game.BoardHeight)+Pred(Row)*Game.BoardWidth+Col]:=CH_WALL; Inc(Col);
              end;
            Col:=Game.BoardWidth;
            while (Col>0) and ((Game.Board[RowOffset+Col] and WALL)=0) do begin
              Alleles[(Pred(Depth)*Game.BoardWidth*Game.BoardHeight)+Pred(Row)*Game.BoardWidth+Col]:=CH_WALL; Dec(Col);
              end;
            end;
    Result:=GAAddIndividual(Individuals[0]);
    end;
end;

function GACalculateFitnessForChromosome(var Board__:TBoard):Integer; {Sokoban specific}
begin {Fitness Function}
  Inc(Generator.Count);
  Result:=SearchPlayerAccessAreas(Board__);
  if Positions.TotalDroppedCount<>0 then Inc(GA.Statistics.MemoryFullCandidatesCount);
  if (Result>0) or (Game.BoxCount=0) then Board__:=Game.Board; {box count = 0: a fixed template may be a walls-only template with no boxes or goals}
end;

function GACrossOver(No1__,No2__:Integer; var New1__,New2__:TGAExtendedIndividual):Boolean; {Slightly specific to Sokoban}
var i,CrossOverPoint:Integer;
begin
  Result:=(Random(100)<GA.Control.CrossOverProbabilityPct) and
          (No1__<>No2__);

  if Result then begin
     Inc(GA.Statistics.CrossoverCount);
     if   Length(New1__.Individual.Alleles)<>GA.AllelesCount then SetLength(New1__.Individual.Alleles,GA.AllelesCount);
     if   Length(New2__.Individual.Alleles)<>GA.AllelesCount then SetLength(New2__.Individual.Alleles,GA.AllelesCount);

     if   GA.AllelesCount<=3 then CrossOverPoint:=1
     else CrossOverPoint:=Succ(Random(Pred(GA.AllelesCount))); {'Pred': n-1 separators between n items}

     for i:=1 to CrossOverPoint do begin
         New1__.Individual.Alleles[i]:=GA.Individuals[No1__].Alleles[i];
         New2__.Individual.Alleles[i]:=GA.Individuals[No2__].Alleles[i];
         end;
     for i:=Succ(CrossOverPoint) to GA.AllelesCount do begin
         New1__.Individual.Alleles[i]:=GA.Individuals[No2__].Alleles[i];
         New2__.Individual.Alleles[i]:=GA.Individuals[No1__].Alleles[i];
         end;

     New1__.Individual.Fitness:=-1; New2__.Individual.Fitness:=-1; {signals 'not calculated yet'}

     {Sokoban specific:}
     {ensure that all goals are connected for single-target-area-levels, and}
     {ensure that candidates don't exceed the 'MaxBoxCount' limit}
     GAAllelesToBoard(New1__.Individual.Alleles,Game.Board);
     if   Game.ReverseMode then
          for i:=0 to Game.BoardSize do Game.Board[i]:=Game.Board[i] and (not BOX)
     else for i:=0 to Game.BoardSize do Game.Board[i]:=Game.Board[i] and (not GOAL);
     InitializeGame; New1__.Board:=Game.Board; {for efficiency, 'Alleles' is not updated here; only the internal board representation is saved}

     GAAllelesToBoard(New2__.Individual.Alleles,Game.Board);
     if   Game.ReverseMode then
          for i:=0 to Game.BoardSize do Game.Board[i]:=Game.Board[i] and (not BOX)
     else for i:=0 to Game.BoardSize do Game.Board[i]:=Game.Board[i] and (not GOAL);
     InitializeGame; New2__.Board:=Game.Board; {for efficiency, 'Alleles' is not updated here; only the internal board representation is saved}
     end
  else begin {no crossover; copy the first individual}
     New1__.Individual:=GA.Individuals[No1__]; GAAllelesToBoard(New1__.Individual.Alleles,New1__.Board);
     end;

  {clear fields for the new candidates}
  New1__.Individual.CandidateNo   :=0;   New2__.Individual.CandidateNo   :=0;
  New1__.Individual.ChildrenCount :=0;   New2__.Individual.ChildrenCount :=0;
  New1__.Individual.CrossOverTag:=0;   New2__.Individual.CrossOverTag    :=0;
  New1__.Individual.TimeMS        :=0;   New2__.Individual.TimeMS        :=0;
  New1__.Individual.TimeOfBirthMS :=0;   New2__.Individual.TimeOfBirthMS :=0;
end;

function GAMutation(var Board__:TBoard):Integer; {Sokoban specific}
const MAX_MUTATION_ATTEMPTS=100;
var   i,j,ItemCount,Index,MutationCount,MutationAttemptsCountdown,Square:Integer; ItemPos:array[0..MAX_BOX_COUNT] of Integer;

  function RandomInteriorWallSquare:Integer;
  var i,j,Col,Row,Depth:Integer;
  begin
    Result:=-1; i:=Succ(Random(Game.BoardSize)); j:=i;
    repeat SquareToColRowDepth(i,Col,Row,Depth);
           if   ((Board__[i] and (WALL+FLAG_FIXED_SQUARE))=WALL) and
                (Col>1) and (Col<Game.BoardWidth) and
                (Row>1) and (Row<Game.BoardHeight) and
                (Depth>1) and (Depth<Game.BoardDepth) then
                Result:=i
           else if   i=Game.BoardSize then i:=1
                else Inc(i);
    until  (Result>0) or (i=j);
  end;

  function RandomFreeNeighbourFloorSquare(Item__,Square__:Integer):Integer;
  var AvoidMask,Count:Integer; Direction:TDirection;
      NeighbourSquares:array[0..MAX_DIRECTION_COUNT] of Integer;
  begin
    Count:=0; AvoidMask:=Item__+WALL+FLAG_FIXED_SQUARE;
    for Direction:=Low(Direction) to High(Direction) do begin
        NeighbourSquares[Succ(Count)]:=Square__+Game.NeighbourSquareOffset[Direction];
        if (Board__[NeighbourSquares[Succ(Count)]] and AvoidMask)=0 then
           Inc(Count);
        end;
    if   Count<>0 then Result:=NeighbourSquares[Succ(Random(Count))]
    else Result:=0;
  end;

  function RandomSquare(AvoidMask__:Integer):Integer;
  var i,j:Integer;
  begin
    Result:=0; i:=Succ(Random(Game.BoardSize)); j:=i;
    repeat if   (Board__[i] and AvoidMask__)=0 then Result:=i
           else if   i=Game.BoardSize then i:=1
                else Inc(i);
    until  (Result<>0) or (i=j);
  end;

  function RandomNonBoxFloorSquare:Integer;
  begin
    Result:=RandomSquare(BOX+WALL+FLAG_FIXED_SQUARE);
  end;

  function RandomNonGoalFloorSquare:Integer;
  begin
    Result:=RandomSquare(GOAL+WALL+FLAG_FIXED_SQUARE);
  end;

  function RandomFreeFloorSquareNeighbourToAnItem(Item__,ExceptItemPos__:Integer):Integer;
  var AvoidMask,Square,Count:Integer; Direction:TDirection; CandidateSquares:TBoardInteger;
  begin
    Count:=0; AvoidMask:=Item__+WALL+FLAG_FIXED_SQUARE;
    for Square:=1 to Game.BoardSize  do
        if (Board__[Square] and AvoidMask)=0 then
           for Direction:=Low(Direction) to High(Direction) do
               if ((Board__[Square+Game.NeighbourSquareOffset[Direction]] and Item__)<>0) and
                  (         Square+Game.NeighbourSquareOffset[Direction]<>ExceptItemPos__) then begin
                  Inc(Count); CandidateSquares[Count]:=Square;
                  break; {ensure that the square isn't added to the list more than once}
                  end;
    if   Count<>0 then
         Result:=CandidateSquares[Succ(Random(Count))]
    else Result:=-1;
  end;

  function STARandomRemovableGoalIndex:Integer; {single-target-area level: choose a removable goal, if any}
  var i,j,Count:Integer; CandidateSquares,VisitedSquares:TBoardInteger;
  begin {STARandomRemovableGoalIndex}
     if ItemCount<2 then begin
        Result:=ItemCount;
        if (Result=1) and
           ((Board__[ItemPos[1]] and FLAG_FIXED_SQUARE)<>0) then
           Result:=-1;
        end
     else begin
        Count:=0; FillChar(VisitedSquares,SizeOf(VisitedSquares),0);

        for i:=1 to ItemCount do
            if (Board__[ItemPos[i]] and FLAG_FIXED_SQUARE)=0 then begin {try to remove each non-fixed goal}
               Dec(Board__[ItemPos[i]],GOAL); {remove the goal from the board}

               if   i<>1 then j:=1
               else j:=ItemCount;
               if   CalculateConnectedGoals(ItemPos[j],i,0, {check if the rest of the goals are connected}
                                            Board__,
                                            VisitedSquares)=Pred(ItemCount) then begin
                    Inc(Count); CandidateSquares[Count]:=i;
                    end;

               Inc(Board__[ItemPos[i]],GOAL); {put the goal back on the board}
               end;

        if   Count<>0 then
             Result:=CandidateSquares[Succ(Random(Count))]
        else Result:=-1;
        end;
  end;

  function RandomRemovableItemIndex:Integer; {choose a removable item, if any}
  var i,Count:Integer; CandidateSquares:TBoardInteger;
  begin {RandomRemovableItemIndex}
     if ItemCount<2 then begin
        Result:=ItemCount;
        if (Result=1) and
           ((Board__[ItemPos[1]] and FLAG_FIXED_SQUARE)<>0) then
           Result:=-1;
        end
     else begin
        Count:=0;

        for i:=1 to ItemCount do
            if (Board__[ItemPos[i]] and FLAG_FIXED_SQUARE)=0 then begin {find non-fixed goals}
               Inc(Count); CandidateSquares[Count]:=i;
               end;

        if   Count<>0 then
             Result:=CandidateSquares[Succ(Random(Count))]
        else Result:=-1;
        end;
  end;

  function SelectMutationType:TGAMutationType;
  var i:Integer;
  begin {Precondition: Accumulated frequencies > 0}
    if   ItemCount>0 then begin
         Result:=Low(GA.Control.MutationTypeAccumulatedFrequencies);
         if Ga.Control.MutateWallsOnly then with GA.Control do
            i:=MutationTypeAccumulatedFrequencies[gaAddWall] +
               Random(MutationTypeAccumulatedFrequencies[gaMoveWall]-MutationTypeAccumulatedFrequencies[gaAddWall])
         else
            i:=Random(GA.Control.MutationTypeAccumulatedFrequencies[High(GA.Control.MutationTypeAccumulatedFrequencies)]);
         while i>=GA.Control.MutationTypeAccumulatedFrequencies[Result] do Inc(Result);
         end
    else Result:= gaAddItem; {put an item (a box or a goal depending on the search direction) on the board}
  end;

  function AddWall:Integer;
  begin
    Result:=RandomNonGoalFloorSquare;
    if Result>0 then Board__[Result]:=WALL;
  end;

begin {GAMutation}
    Result:=0; ItemCount:=0; MutationAttemptsCountdown:=MAX_MUTATION_ATTEMPTS;

    if   Game.ReverseMode then
         for i:=0 to Game.BoardSize do begin
             Board__[i]:=Board__[i] and (not (PLAYER+BOX)); {remove player and boxes}
             if (Board__[i] and GOAL)<>0 then begin
                Inc(ItemCount); ItemPos[ItemCount]:=i;
                Board__[i]:=Board__[i] or (BOX+GOAL); {put boxes on goal-squares}
                end;
             end
    else for i:=0 to Game.BoardSize do begin
             Board__[i]:=Board__[i] and (not (GOAL)); {remove goals}
             if (Board__[i] and BOX)<>0 then begin
                Inc(ItemCount); ItemPos[ItemCount]:=i;
                end;
             end;

    if   ItemCount>0 then begin
         if   Random(100)<GA.Control.MutationProbabilityPct then
              MutationCount:=Succ(Random(GA.Control.MaxMutationCount))
         else MutationCount:=0;
         end
    else MutationCount:=1; {put an item (a box or a goal depending on the search direction) on the board}

    while (Result<MutationCount) and
          (MutationAttemptsCountdown>0) do begin
          Dec(MutationAttemptsCountdown);
          case SelectMutationType of
            gaAddItem   : if (ItemCount<Generator.MaxBoxCount)
                             or
                             ((not Generator.MaxBoxCountEnabled)
                              and
                              (ItemCount<MAX_BOX_COUNT)
                             ) then begin
                             if Game.ReverseMode then begin
                                if   GA.Control.GenerateConnectedGoals and
                                     (ItemCount<>0) then
                                     Square:=RandomFreeFloorSquareNeighbourToAnItem(GOAL,0)
                                else Square:=RandomNonGoalFloorSquare;
                                if Square>0 then begin
                                   Inc(ItemCount);
                                   ItemPos[ItemCount]:=Square;
                                   Inc(Board__[ItemPos[ItemCount]],BOX+GOAL);
                                   Inc(Result);
                                   //Writeln('Add goal ',Square);
                                   end;
                                end
                             else begin
                                Square:=RandomNonBoxFloorSquare;
                                if Square>0 then begin
                                   Inc(ItemCount);
                                   ItemPos[ItemCount]:=Square;
                                   Inc(Board__[ItemPos[ItemCount]],BOX);
                                   Inc(Result);
                                   //Writeln('Add box ',Square);
                                   end;
                                end;
                             end
                          else
                             if AddWall>0 then Inc(Result);
            gaDeleteItem: if ItemCount>1 then
                             if Game.ReverseMode then begin
                                if   GA.Control.GenerateConnectedGoals then
                                     Index:=STARandomRemovableGoalIndex
                                else Index:=RandomRemovableItemIndex;
                                if Index>0 then begin
                                   //Writeln('Delete goal ',GoalPos[Index]);
                                   Dec(Board__[ItemPos[Index]],BOX+GOAL);
                                   Dec(ItemCount);
                                   for j:=Index to ItemCount do
                                       ItemPos[j]:=ItemPos[Succ(j)];
                                   Inc(Result);
                                   end;
                                end
                             else begin
                                Index:=RandomRemovableItemIndex;
                                if Index>0 then begin
                                   //Writeln('Delete box ',ItemPos[Index]);
                                   Dec(Board__[ItemPos[Index]],BOX);
                                   Dec(ItemCount);
                                   for j:=Index to ItemCount do
                                       ItemPos[j]:=ItemPos[Succ(j)];
                                   Inc(Result);
                                   end;
                                end;
            gaMoveItem  : if ItemCount>1 then
                             if Game.ReverseMode then begin
                                if   GA.Control.GenerateConnectedGoals then
                                     Index:=STARandomRemovableGoalIndex
                                else Index:=RandomRemovableItemIndex;
                                if   (not GA.Control.GenerateConnectedGoals) and
                                     (Random(2)=0) then
                                     Square:=RandomFreeNeighbourFloorSquare(GOAL,ItemPos[Index]) {try moving the goal to a neighbour square}
                                else Square:=0;
                                if Square<=0 then
                                   if   GA.Control.GenerateConnectedGoals then
                                        if   Index>0 then
                                             if   ItemCount=1 then
                                                  Square:=RandomNonGoalFloorSquare
                                             else Square:=RandomFreeFloorSquareNeighbourToAnItem(GOAL,ItemPos[Index])
                                        else Square:=0
                                   else Square:=RandomNonGoalFloorSquare;
                                if (Index>0) and (Square>0) then begin
                                   //Writeln('Move goal ',GoalPos[Index],' -> ',Square);
                                   Dec(Board__[ItemPos[Index]],BOX+GOAL);
                                   ItemPos[Index]:=Square;
                                   Inc(Board__[ItemPos[Index]],BOX+GOAL);
                                   Inc(Result);
                                   end;
                                end
                             else begin
                                Index:=RandomRemovableItemIndex;
                                if   Random(2)=0 then
                                     Square:=RandomFreeNeighbourFloorSquare(BOX,ItemPos[Index]) {try moving the box to a neighbour square}
                                else Square:=0;
                                if Square<=0 then Square:=RandomNonBoxFloorSquare;
                                if (Index>0) and (Square>0) then begin
                                   //Writeln('Move box ',ItemPos[Index],' -> ',Square);
                                   Dec(Board__[ItemPos[Index]],BOX);
                                   ItemPos[Index]:=Square;
                                   Inc(Board__[ItemPos[Index]],BOX);
                                   Inc(Result);
                                   end;
                                end;
            gaAddWall   : if AddWall>0 then Inc(Result);
            gaDeleteWall: begin
                            Square:=RandomInteriorWallSquare;
                            if Square>0 then begin
                               Board__[Square]:=FLOOR; Inc(Result);
                               //Writeln('Delete wall ',Square);
                               end
                            else if AddWall>0 then Inc(Result);
                          end;
            gaMoveWall  : begin
                            Square:=RandomInteriorWallSquare;
                            if   (Square>0) and (Random(4)=0) then
                                 if   Game.ReverseMode then
                                      j:=RandomFreeNeighbourFloorSquare(GOAL,Square)
                                 else j:=RandomFreeNeighbourFloorSquare(BOX ,Square)
                            else j:=0;
                            if j<=0 then j:=RandomNonGoalFloorSquare;
                            if (Square>0) and (j>0) then begin
                               Board__[Square]:=FLOOR;
                               Board__[j     ]:=WALL;
                               Inc(Result);
                               //Writeln('Move wall ',Square,' -> ',j);
                               end
                            else if (Square<=0) and (AddWall>0) then
                                    Inc(Result);
                           end;
          end; {case}
          end;

    Inc(GA.Statistics.MutationCount,Result);
end;

procedure GAShowIndividual(No__:Integer; const Individual__:TGAIndividual); {Sokoban specific}
{$IFDEF CONSOLE_APPLICATION}
  var B:TBoard;
  begin
    with Individual__ do begin
      if No__<>0 then Write(No__:4,SPACE);
      Writeln(GA_TEXT_FITNESS,COLON,SPACE,Fitness,' (',PushCount,', ',Score,') ',
              GA_TEXT_CHILDREN,SPACE,ChildrenCount);
      B:=Game.Board; GAAllelesToBoard(Individual__.Alleles,Game.Board); ShowBoard; Game.Board:=B;
      end;
  end;
{$ELSE}
  {$IFDEF SILENT_APPLICATION}
    begin
      Windows.MessageBox(0,'GAShowIndividual',TEXT_APPLICATION_TITLE,MB_OK);
    end;
  {$ELSE}
    begin
    end;
  {$ENDIF}
{$ENDIF}

function GAWriteIndividualToFile(No__:Integer; const Individual__:TGAIndividual; {const} var F__:TextFile; PrettyPrinting__:Boolean):Boolean; {Sokoban specific}
var i,oBoxCount,oHistoryCount,oPlayerPos:Integer;
    oBoard:TBoard; oBoxPos:TBoxPositions;
begin {$I-}
  with Individual__ do begin
    Writeln(F__);
    Writeln(F__);
    if No__>0 then Writeln(F__,LEFT_BRACKET,GA_TEXT_CANDIDATE,SPACE,CandidateNo,RIGHT_BRACKET);

    oBoard:=Game.Board; oHistoryCount:=Game.History.Count;
    GAAllelesToBoard(Alleles,Game.Board);
    Game.History.Count:=0;

    if PrettyPrinting__ then with Game do begin
       oPlayerPos:=Game.PlayerPos; oBoxCount:=Game.BoxCount; oBoxPos:=Game.BoxPos;

       {parse the board in order to find box-positions and player-position; 'WriteLevelToFile' requires updated information when boards are pretty-printed}
       BoxCount:=0; PlayerPos:=0;
       for i:=0 to BoardSize do
           if      (Board[i] and BOX)<>0 then begin
                   Inc(BoxCount); BoxPos[BoxCount]:=i;
                   end
           else if (Board[i] and PLAYER)<>0 then PlayerPos:=i;
       WriteLevelToFile(F__,'',True,True,False);

       Game.PlayerPos:=oPlayerPos; Game.BoxCount:=oBoxCount; Game.BoxPos:=oBoxPos;
       end
    else
       WriteLevelToFile(F__,'',False,False,False);

    Game.Board:=oBoard; Game.History.Count:=oHistoryCount;

    if Fitness>0 then begin
       Writeln(F__,GA_TEXT_FITNESS_FUNCTION_TYPES[ffPushes],COLON,SPACE,PushCount);
       Writeln(F__,GA_TEXT_FITNESS_FUNCTION_TYPES[ffScore ],COLON,SPACE,Score);
       Writeln(F__,GA_TEXT_FITNESS                         ,COLON,SPACE,Fitness);
       end;
    Writeln(F__,GA_TEXT_CANDIDATE_NUMBER,COLON,SPACE,CandidateNo);
    i:=Integer(TimeOfBirthMS);
    Writeln(F__,GA_TEXT_TIME_OF_BIRTH_MS,COLON,SPACE,i);
    i:=Integer(TimeMS);
    Writeln(F__,GA_TEXT_FITNESS_CALCULATION_TIME_MS,COLON,SPACE,i);
    Writeln(F__,GA_TEXT_CHILDREN,COLON,SPACE,ChildrenCount);

    Result:=IOResult=0;
    end;
end; {$I+}

{End of Sokoban specific GA functions and procedures}

{-----------------------------------------------------------------------------}

{GA (Genetic Algorithm) - General functions and procedures}

function  GACalculateFitness(var Individual__:TGAExtendedIndividual):Boolean;
begin
  with Individual__ do with Individual do begin
    GAHistoryAdd(GACalculateHashKey(Board));
    if CandidateNo=0 then begin
       Inc(GA.Statistics.CandidatesCount);
       CandidateNo:=GA.Statistics.CandidatesCount;
       end;
    Game.Title:=GA_TEXT_CANDIDATE+SPACE+IntToStr(CandidateNo);
    if TimeOfBirthMS=0 then TimeOfBirthMS:=GACalculateElapsedTimeMS;

    Fitness  :=GACalculateFitnessForChromosome(Board);
    PushCount:=Game.History.Count;
    Score    :=Game.History.Score;
    Result   :=Fitness<>-1;
    end;
end;

procedure GAClear;
begin
  GAClearPopulation; GAClearStatistics; GAHistoryClear;
end;

procedure GAClearPopulation;
var i:Integer;
begin
  with GA do with Control do begin
    GenerationCount:=0; Statistics.LastNewCandidateNo:=0;
    StartTimeMS:=0; PriorSessionsTimeMS:=0;
    IndividualCount:=0; BestIndividualNo:=0; WorstIndividualNo:=0; AllelesCount:=0;
    for i:=Low(Individuals) to High(Individuals) do Individuals[i].Alleles:='';
    FillChar(Individuals,SizeOf(Individuals),0);
    for i:=Low(Individuals) to High(Individuals) do Individuals[i].Fitness:=-1;
    end;
end;

procedure GAClearStatistics;
begin
  with GA do FillChar(Statistics,SizeOf(Statistics),0);
end;

function GAReplacement(const Individual__:TGAIndividual):Integer;
begin {Checks whether the new individual should be added to the population}
  if             GA.IndividualCount<GA.Control.PopulationSize then
                 Result:=GAAddIndividual(Individual__)
  else if        ( Individual__.Fitness  >GA.Individuals[GA.WorstIndividualNo].Fitness)
                 //or
                 //((Individual__.Fitness  =GA.Individuals[GA.WorstIndividualNo].Fitness)
                 // and
                 // (Individual__.PushCount>GA.Individuals[GA.WorstIndividualNo].PushCount)
                 //)
                 then begin
                 GADeleteIndividual(GA.WorstIndividualNo);
                 Result:=GAAddIndividual(Individual__);
                 end
       else if   (GA.IndividualCount>1) and
                 GA.Control.InactivityThresholdEnabled and
                 (GA.Statistics.LastNewCandidateNo+GA.Control.InactivityThreshold<GA.Statistics.CandidatesCount) then begin
                 {nothing has happened for a long time;}
                 {try to increase the diversity by deleting the individual}
                 {which had the most chances to create offspring}
                 Inc(GA.Statistics.InactivityCount);
                 GADeleteIndividual(GAIndividualWithHighestChildCountExceptTheBest);
                 Result:=GAReplacement(Individual__);
                 end
            else Result:=-1;
  if Result>0 then with GA.Statistics do LastNewCandidateNo:=CandidatesCount;
end;

procedure GASelection(var No1__,No2__:Integer);
begin
  No1__:=GARankSelection;
  repeat No2__:=GARankSelection;
  until  (No1__<>No2__) or (GA.IndividualCount<=1);
end;

{-----------------------------------------------------------------------------}

{GA Toplevel}

procedure GAFinalize;
begin
  GAHistoryClear;
end;

function GAInitialize(const FileName__:String;
                      GoodEnoughFitness__,MaxGenerationCount__,IndividualCount__,
                      InactivityThreshold__,
                      CrossOverProbabilityPct__,MutationProbabilityPct__,MaxMutationCount__:Integer;
                      MutationTypeAccumulatedFrequencies__:TGAMutationTypeAccumulatedFrequencies;
                      FitnessFunction__:TGAFitnessFunction;
                      MutateWallsOnly__,
                      ForwardSearch__,
                      GenerateConnectedGoals__,
                      InactivityThresholdEnabled__:Boolean;
                      AutoSaveIntervalTimeMS__:TTimeMS
                     ):Boolean;
var i:Integer; F:TextFile;
begin {$I-}
  GA.FileName:='';
  for i:=Low(GA.Individuals) to High(GA.Individuals) do GA.Individuals[i].Alleles:='';
  FillChar(GA,SizeOf(GA),0);
  GA.FileName:=FileName__;
  GA.IndividualCount:=IndividualCount__;
  GA.Control.AutoSaveIntervalTimeMS:=AutoSaveIntervalTimeMS__;
  GA.Control.CrossOverProbabilityPct:=CrossOverProbabilityPct__;
  GA.Control.FitnessFunction:=FitnessFunction__;
  GA.Control.ForwardSearch:=ForwardSearch__;
  GA.Control.GenerateConnectedGoals:=GenerateConnectedGoals__;
  GA.Control.GoodEnoughFitness:=GoodEnoughFitness__;
  GA.Control.InactivityThreshold:=InactivityThreshold__;
  GA.Control.InactivityThresholdEnabled:=InactivityThresholdEnabled__;
  GA.Control.MaxGenerationCount:=MaxGenerationCount__;
  GA.Control.PopulationSize:=GA.IndividualCount;
  GA.Control.MutateWallsOnly:=MutateWallsOnly__;
  GA.Control.MutationProbabilityPct:=MutationProbabilityPct__;
  GA.Control.MaxMutationCount:=MaxMutationCount__;
  GA.Control.MutationTypeAccumulatedFrequencies:=MutationTypeAccumulatedFrequencies__;
  GAClear;
  InitializeRandomState(0);
  if GA.FileName<>'' then begin
     Assign(F,GA.FileName); Rewrite(F); CloseFile(F);
     end;
  Result:=IOResult=0;
end; {$I-}

procedure GARun;
type TCandidateState=(csNewCandidate1,csTestedCandidate1,csNewCandidate2,csTestedCandidate2);
var  i,No1,No2: Integer; s:String; TimeMS:TTimeMS;
     CandidateState:set of TCandidateState; New1,New2:TGAExtendedIndividual;
begin
  if GA.AllelesCount>0 then with GA do begin
     Game.ReverseMode:=not GA.Control.ForwardSearch;

     {ensure that already calculated members of the initial population are registered in the history}
     for i:=1 to IndividualCount do with GA.Individuals[i] do
         if Fitness<>-1 then begin {'True': the fitness has already been calculated}
            GAAllelesToBoard(Alleles,Game.Board);
            LoadAndNormalizeBoard(Game.Board); {normalize the board before checking for duplicates}
            if GaHistoryLookup(GACalculateHashKey(Game.Board))=nil then
               GAHistoryAdd(GACalculateHashKey(Game.Board));
            end;

     repeat StartTimeMS:=GetTimeMS;
     until  StartTimeMS<>0; {'0' is a reserved value meaning 'inactive'}
     GA.Control.LastAutoSaveTimeMS:=StartTimeMS;

     if Generator.RandomSeedEnabled then begin
        Generator.RandomSeed:=StartTimeMS;  {randomize the random seed}
        InitializeRandomState(Generator.RandomSeed);
        for i:=0 to StartTimeMS mod (10*ONE_KIBI) do Generator.RandomSeed:=Random(High(Generator.RandomSeed));
        end;

     //Generator.RandomSeed:=0;
     InitializeRandomState(Generator.RandomSeed);

     {calculate fitness for the members of the initial population}
     i:=Statistics.CandidatesCount;
     s:=Generator.OutputFileName; Generator.OutputFileName:=''; Generator.Count:=0; {'OutputFileName' controls whether statistics and new levels are updated}
     //Inc(Generator.Count);
     repeat No1:=0;
            for No2:=IndividualCount downto 1 do with GA.Individuals[No2] do
                if Fitness=-1 then No1:=No2; {'-1': not calculated yet}
            if  No1<>0 then begin
                New1.Individual:=Individuals[No1]; GADeleteIndividual(No1);
                TimeMS:=GetTimeMS;
                GAAllelesToBoard(New1.Individual.Alleles,New1.Board);
                LoadAndNormalizeBoard(New1.Board); {normalize the board before checking for duplicates}
                {Game.Board:=Individuals[No1].Alleles; ShowBoard; Write('Seed: '); Readln;}
                if GAIsANewChromosome(New1.Board) then begin
                   if   GACalculateFitness(New1) then begin
                        //Game.Board:=New1.Board; ShowBoard; Write('Individual: ',No1,' Fitness: ',New1.Individual.Fitness); Readln;
                        GABoardToAlleles(New1.Board,New1.Individual.Alleles); {convert the internal board representation to alleles, i.e., the 'chromosome'}
                        New1.Individual.TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS);
                        GAReplacement(New1.Individual);
                        end
                   else No1:=-1; {'-1': stop the generator before it really starts; either the user terminated the generator manually, or something went wrong}
                   end;
                end;
     until  No1<=0;
     Generator.OutputFileName:=s;
     if i<>0 then Statistics.CandidatesCount:=i; {restore the number of originally loaded candidates, if any}
     Generator.Count:=Statistics.CandidatesCount;

     {run genetic algorithm}
     if (IndividualCount>=1) and (No1=0) then begin
        while (IndividualCount>0) and
              (GenerationCount<Control.MaxGenerationCount) and
              (Statistics.CandidatesCount<GA_DEFAULT_MAX_CANDIDATE_COUNT) and
              (Individuals[BestIndividualNo].Fitness<Control.GoodEnoughFitness) do begin {'<': the program maximizes the result}

          Inc(GenerationCount);

          {$IFDEF CONSOLE_APPLICATION}
            Writeln;
            Writeln('Generation ',GenerationCount);
          {$ENDIF}

          if   Generator.RandomSeed<High(Generator.RandomSeed) then Inc(Generator.RandomSeed)
          else Generator.RandomSeed:=1;
          InitializeRandomState(Generator.RandomSeed);

          GASelection(No1,No2);             {select 2 individuals based on rank selection}
          Individuals[No2].CrossOverTag:=2; {tag the individuals currently selected for cross-over}
          Individuals[No1].CrossOverTag:=1; {there may be only one individual, hence, set tag '2' before tag '1'}

          if   GACrossOver(No1,No2,New1,New2) then
               CandidateState:=[csNewCandidate1,csNewCandidate2] {the cross-over produced 2 candidates}
          else CandidateState:=[csNewCandidate1]; {the cross-over made a single candidate only}

          if csNewCandidate1 in CandidateState then begin {mutate and evaluate candidate 1}
             Exclude(CandidateState,csNewCandidate1);

             TimeMS:=GetTimeMS;
             GAMutation(New1.Board);
             LoadAndNormalizeBoard(New1.Board); {normalize the board before checking for duplicates}
             if   GAIsANewChromosome(New1.Board) then begin
                  Inc(Individuals[No1].ChildrenCount); {update parent(s)}
                  if csNewCandidate2 in CandidateState then Inc(Individuals[No2].ChildrenCount);

                  if GACalculateFitness(New1) then begin
                     i:=GAReplacement(New1.Individual);
                     if   i>0 then begin {'True': candidate 1 added to the population as number 'i'}
                          GABoardToAlleles(New1.Board,GA.Individuals[i].Alleles);
                          Include(CandidateState,csNewCandidate1);
                          GA.Individuals[i].TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS);

                          {the parents may have changed position or dropped}
                          {out of the population, hence, update 'No1' and 'No2'}
                          No1:=0; for i:=1 to IndividualCount do if Individuals[i].CrossOverTag=1 then begin No1:=i; break; end;
                          No2:=0; for i:=1 to IndividualCount do if Individuals[i].CrossOverTag=2 then begin No2:=i; break; end;
                          end
                     else Include(CandidateState,csTestedCandidate1); {candidate 1 tested but not added to the population}
                     end
                  else Exclude(CandidateState,csNewCandidate2); {don't calculate a second candidate if the user terminated the generator manually during the first calculation}
                  end
             else Inc(Statistics.DuplicatesCount);
             end;

          if csNewCandidate2 in CandidateState then begin {mutate and evaluate candidate 2}
             Exclude(CandidateState,csNewCandidate2);

             TimeMS:=GetTimeMS;
             GAMutation(New2.Board);
             LoadAndNormalizeBoard(New2.Board); {normalize the board before checking for duplicates}
             if   GAIsANewChromosome(New2.Board) and
                  (not GAIdenticalBoards(New1.Board,New2.Board)) and
                  (Statistics.CandidatesCount<GA_DEFAULT_MAX_CANDIDATE_COUNT) then begin
                  Inc(Individuals[No1].ChildrenCount); {update parents}
                  Inc(Individuals[No2].ChildrenCount);

                  if GACalculateFitness(New2) then begin
                     i:=GAReplacement(New2.Individual);
                     if   i>0 then begin {'True': candidate 2 added to the population as number 'i'}
                          GABoardToAlleles(New2.Board,GA.Individuals[i].Alleles);
                          Include(CandidateState,csNewCandidate2);
                          GA.Individuals[i].TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS);

                          {the parents may have changed position or dropped}
                          {out of the population, hence, update 'No1' and 'No2'}
                          No1:=0; for i:=1 to IndividualCount do if Individuals[i].CrossOverTag=1 then begin No1:=i; break; end;
                          No2:=0; for i:=1 to IndividualCount do if Individuals[i].CrossOverTag=2 then begin No2:=i; break; end;
                          end
                     else Include(CandidateState,csTestedCandidate2); {candidate 2 tested but not added to the population}
                     end;
                  end
             else Inc(Statistics.DuplicatesCount);
             end;

          if (csNewCandidate1 in CandidateState) or
             (csNewCandidate2 in CandidateState) then begin
             GASaveToFile(GA.FileName); {new member of the population: save the complete state}
             end
          else begin {update log with new tested candidates, if any}
             if csTestedCandidate1 in CandidateState then
                GAHistoryAddItemToFile(GACalculateHashKey(New1.Board));
             if csTestedCandidate2 in CandidateState then
                GAHistoryAddItemToFile(GACalculateHashKey(New2.Board));
             end;

          Individuals[No1].CrossOverTag:=0; {reset the tags for the individuals currently selected for cross-over}
          Individuals[No2].CrossOverTag:=0;
          end;

        GASaveToFile(GA.FileName);
        end
     else Msg('Empty population',TEXT_APPLICATION_TITLE);
     {GAShowPopulation(False);}
     end
  else Msg('Null length chromosome',TEXT_APPLICATION_TITLE);
end;


{End of Genetic Algorithm Sokoban Level Generator}

{=============================================================================}

{-----------------------------------------------------------------------------}

{General Utilities}

function  CalculateElapsedTimeMS(StartTimeMS__,StopTimeMS__:TTimeMS):TTimeMS;
begin
  if   StopTimeMS__>=StartTimeMS__ then
       Result:=StopTimeMS__-StartTimeMS__
  else Result:=High(StopTimeMS__)-StartTimeMS__+StopTimeMS__+1; {clock wrap-around; assume it only wrapped around once}
end;

function  ExtractFileNameWithoutExt(const FileName__:String):String;
var Index:Integer;
begin
  Index:=Length(FileName__);
  while (Index<>0) and (FileName__[Index]<>PERIOD) do Dec(Index);
  if   Index=0 then Result:=FileName__
  else Result:=System.Copy(FileName__,1,Pred(Index));
end;

function  ExtractKeyValue(const Text__,Key__:String; var Value__:String):Boolean;
var Index,TextLength:Integer;
begin {precondition: 'Key__' <>''}
  Index:=Succ(Length(Key__));
  TextLength:=Length(Text__);
  Result:=(Index<=TextLength)
          and
          (Text__[1]=Key__[1])
          and
          ((Text__[Index]=EQUAL)
           or
           (Text__[Index]=COLON)
          )
          and
          StrBeginsWith(Text__,Key__);
  if Result then Value__:=StrTrim(Copy(Text__,Succ(Index),TextLength-Index));
end;

function  GetTimeMS:TTimeMS;
begin {returns a time measured in milliseconds; the base doesn't matter, the time is only used in relative calculations}
{$IFDEF WINDOWS}
  Result:=GetTickCount; {Windows function}
{$ELSE}
  Result:=0;            {no timing}
{$ENDIF}
end;

procedure InitializeRandomState(RandomNumber__:Integer);
begin
  RandomState.RandomNumber:=Abs(RandomNumber__);
end;

function  IntToStr(i:Integer):String;
begin {local function to avoid linking 'SysUtils' to the program}
  {$WARNINGS OFF}
    Str(i,Result); {Implicit string cast from 'ShortString' to 'String'}
  {$WARNINGS ON}
end;

function  KeyEnabledText(const Key__:String):String;
begin
  Result:=Key__+SPACE+TEXT_KEY_ENABLED_SUFFIX;
end;

function  Max(a__,b__:Integer):Integer;
begin
  if a__>=b__ then Max:=a__
  else Max:=b__;
end;

function  Min(a__,b__:Integer):Integer;
begin
  if a__<=b__ then Min:=a__
  else Min:=b__;
end;

function  Random(Range__:Integer):Integer; {the level generator uses its own simple random number function so results are reproducible; it's a low-quality random number generator and it's not suitable for general use}
const a=1366; c=150889; m=714025; {don't modify these values unless you know what you are doing}
begin
  RandomState.RandomNumber:=(a * (RandomState.RandomNumber mod m) + c) mod m;
  if   Range__<>0 then Result:=RandomState.RandomNumber mod Range__
  else Result:=0;
end;

function  Sign(Value__:Integer):Integer;
begin
  if      Value__>0 then Result:=1
  else if Value__<0 then Result:=-1
  else Result:=0;
end;

function  StrBeginsWith(const Text__,Key__:String):Boolean;
begin {the console version uses a simple case-sensitive 'StrBeginsWith' function as opposed to the plugin version which isn't case-sensitive; maybe the return values also differ if the key is an empty string (it hasn't been tested)}
  Result:=System.Pos(Key__,Text__)=1;
end;

function  StrToInt(const Str__:String; var Value__:Integer):Boolean;
var ErrorPosition:Integer;
begin
  Val(StrTrim(Str__),Value__,ErrorPosition); Result:=ErrorPosition=0;
end;

function  StrTrim(const Str__:String):String;
begin {local function substituting 'Trim' to avoid linking 'SysUtils' to the program}
  Result:=Str__;
  while (Length(Result)<>0) and (Result[1]             <=SPACE) do Delete(Result,1,1);
  while (Length(Result)<>0) and (Result[Length(Result)]<=SPACE) do Delete(Result,Length(Result),1);
end;

{-----------------------------------------------------------------------------}

{Utilities}

function  BoardSquareValueToChar(Value__:Integer):Char;
begin
  case Value__ and (PLAYER+BOX+GOAL+WALL) of
    PLAYER     : if   LevelReader.XSBNotation then
                      Result:=CH_PLAYER_XSB
                 else Result:=CH_PLAYER;
    PLAYER+GOAL: if   LevelReader.XSBNotation then
                      Result:=CH_PLAYER_ON_GOAL_XSB
                 else Result:=CH_PLAYER_ON_GOAL;
    BOX        : if   LevelReader.XSBNotation then
                      Result:=CH_BOX_XSB
                 else Result:=CH_BOX;
    BOX+GOAL   : if   LevelReader.XSBNotation then
                      Result:=CH_BOX_ON_GOAL_XSB
                 else Result:=CH_BOX_ON_GOAL;
    GOAL       : Result:=CH_GOAL;
    WALL       : Result:=CH_WALL;
    else         if   (not LevelReader.IsABoardTemplate)
                      or
                      ((Value__ and FLAG_FIXED_SQUARE)=0) then
                      Result:=CH_FLOOR
                 else Result:=CH_FIXED_FLOOR;
  end; {case}
end;

function  ColRowDepthToSquare(Col__,Row__,Depth__,ColCount__,RowCount__:Integer):Integer;
begin
  Result:= (Depth__ * (ColCount__+2)*(RowCount__+2)) +
           (Row__   * (ColCount__+2)) +
           Col__;
end;

function  GetFirstPlayerAccessArea:Boolean;
begin
  FillChar(Generator.VisitedPlayerPos,SizeOf(Generator.VisitedPlayerPos),0);
  Generator.PlayerAccessAreaNo:=0;
  Result:=GetNextPlayerAccessArea;
end;

function  GetNextPlayerAccessArea:Boolean;
var i,MinPlayerPos:Integer;
begin
  with Game do begin
    MovePlayer(0); {remove player from the board}
    repeat Inc(PlayerPos);
    until  (PlayerPos>BoardSize)
           or
           (((Board[PlayerPos] and (WALL+BOX))=0)
            and
            (not Generator.VisitedPlayerPos[PlayerPos])
            and
            (CalculatePlayersReachableSquares(MinPlayerPos)<>0));
    Result:=PlayerPos<=BoardSize;
    if Result then begin
       MovePlayer(MinPlayerPos); {put player on the board}
       for i:=0 to BoardSize do
           if PlayersReachableSquares[i]=PlayersReachableSquaresTimeStamp then
              Generator.VisitedPlayerPos[i]:=True; {mark visited access area for the player}
       if not Game.ReverseMode then Inc(Generator.PlayerAccessAreaNo);
       end
    else PlayerPos:=0;
    end;
end;

function  IsABoxSquare(Square__:Integer):Boolean;
begin
  Result:=(Game.Board[Square__] and BOX)<>0;
end;

function  IsAFloorSquare(Square__:Integer):Boolean;
begin
  Result:=(Game.Board[Square__] and FLOOR)<>0;
end;

function  IsAGoalSquare(Square__:Integer):Boolean;
begin
  Result:=(Game.Board[Square__] and GOAL)<>0;
end;

function  IsEmptyDepth(Depth__:Integer):Boolean;
var Col,Row:Integer;
begin
  Result:=True;
  for Row:=1 to Game.BoardHeight do
      for Col:=1 to Game.BoardWidth do
          if   Result then
               Result:=(Game.Board[ColRowDepthToSquare(Col,Row,Depth__,Game.BoardWidth,Game.BoardHeight)] and (WALL+PLAYER+BOX+GOAL))=0
          else break;
end;

function  IsEmptyColAtDepth(Col__,Depth__:Integer):Boolean;
var Row:Integer;
begin
  Result:=True;
  for Row:=1 to Game.BoardHeight do
      if (Game.Board[ColRowDepthToSquare(Col__,Row,Depth__,Game.BoardWidth,Game.BoardHeight)] and (WALL+PLAYER+BOX+GOAL))<>0 then begin
         Result:=False; break;
         end;
end;

function  IsEmptyCol(Col__:Integer):Boolean;
var Depth:Integer;
begin
  Result:=True;
  for Depth:=1 to Game.BoardDepth do
      if   Result then
           Result:=IsEmptyColAtDepth(Col__,Depth)
      else break;
end;

function  IsEmptyRowAtDepth(Row__,Depth__:Integer):Boolean;
var Col:Integer;
begin
  Result:=True;
  for Col:=1 to Game.BoardWidth  do
      if (Game.Board[ColRowDepthToSquare(Col,Row__,Depth__,Game.BoardWidth,Game.BoardHeight)] and (WALL+PLAYER+BOX+GOAL))<>0 then begin
         Result:=False; break;
         end;
end;

function  IsEmptyRow(Row__:Integer):Boolean;
var Depth:Integer;
begin
  Result:=True;
  for Depth:=1 to Game.BoardDepth do
      if   Result then
           Result:=IsEmptyRowAtDepth(Row__,Depth)
      else break;
end;

function  SquareToChar(Square__:Integer):Char;
begin
  Result:=BoardSquareValueToChar(Game.Board[Square__]);
  if ((Game.Board[Square__] and WALL)<>0) and
     (Game.Board[Square__]<>WALL) and
     (Game.Board[Square__]<>(WALL+FLAG_FIXED_SQUARE)) then
     Result:='?'; {error}
end;

procedure SquareToColRowDepth(Square__:Integer; var Col__,Row__,Depth__:Integer);
begin
  Depth__:=Square__   div   Game.Board3DLayerSize;
  Dec(Square__, Depth__ *   Game.Board3DLayerSize);
  Row__  :=Square__   div   (Game.BoardWidth+2);
  Col__  :=Square__ - Row__*(Game.BoardWidth+2);
end;

{-----------------------------------------------------------------------------}

{Console}

function  Error({$IFDEF DELPHI} const {$ENDIF} Text__,Caption__:String):Boolean;
begin
  Msg(Text__,Caption__);
  Result:=False;
end;

function  GetCommandLineParameters(
            var InputFileName__:String;
            var GeneratorMethod__:TGeneratorMethod;
            var StartNo__,RandomSeed__,MemoryByteSize__,MinPushCount__,
                MaxBoxCount__,
                MaxOpenPositions__,
                BoardWidth__,BoardHeight__,BoardDepth__,WallCount__,BoxCount__,
                CrossOverProbabilityPct__,MutationProbabilityPct__,MaxMutationCount__,
                MaxGenerationCount__,PopulationSize__,
                InactivityThreshold__,GoodEnoughFitness__,
                TailPushesThreshold__:Integer;
            var FitnessFunction__:TGAFitnessFunction;
            var ForwardSearch__,
                GenerateConnectedGoals__,
                PrettyPrinting__,
                MaxBoxCountEnabled__,
                MaxOpenPositionsEnabled__,
                InactivityThresholdEnabled__,
                TrivialOpeningPushesIgnored__,
                TailPushesThresholdEnabled__,
                RandomSeedEnabled__,
                FixedTemplate__:Boolean;
            var AutoSaveIntervalTimeMS__:TTimeMS):Boolean;
var ItemIndex,LastKeyNameIndex:Integer; s:String;

  function GetParameter(var Value__:Integer; Min__,Max__,Scale__:Integer; var ItemIndex__:Integer):Boolean;
  begin
    Inc(ItemIndex__); {skip argument name}
    if Scale__<1 then Scale__:=1;
    Result:=(ItemIndex<=ParamCount) and
            StrToInt(ParamStr(ItemIndex__),Value__) and
            (Value__>=Min__ div Scale__) and (Value__<=Max__ div Scale__);
    Value__:=Value__*Scale__;
    Inc(ItemIndex__); {advance to next argument name}
  end;

begin {a simple and not fool-proof implementation}
  Result:=(ParamCount>=1); ItemIndex:=1; LastKeyNameIndex:=-1;
  InputFileName__               :='';
  GeneratorMethod__             :=gmBackward;
  StartNo__                     :=0;
  RandomSeed__                  :=0;
  MemoryByteSize__              :=DEFAULT_MEMORY_BYTE_SIZE;
  MinPushCount__                :=0;
  MaxBoxCount__                 :=MAX_BOX_COUNT;
  MaxOpenPositions__            :=High(MaxOpenPositions__);
  BoardWidth__                  :=0;
  BoardHeight__                 :=0;
  BoardDepth__                  :=0;
  WallCount__                   :=0;
  BoxCount__                    :=0;
  CrossOverProbabilityPct__     :=GA_DEFAULT_CROSSOVER_PROBABILITY_PCT;
  MutationProbabilityPct__      :=GA_DEFAULT_MUTATION_PROBABILITY_PCT;
  MaxMutationCount__            :=GA_DEFAULT_MAX_MUTATION_COUNT;
  MaxGenerationCount__          :=High(MaxGenerationCount__);
  PopulationSize__              :=GA_DEFAULT_POPULATION_SIZE;
  InactivityThreshold__         :=GA_DEFAULT_INACTIVITY_THRESHOLD;
  GoodEnoughFitness__           :=High(GoodEnoughFitness__);
  TailPushesThreshold__         :=DEFAULT_TAIL_POSITION_THRESHOLD;
  FitnessFunction__             :=GA_DEFAULT_FITNESS_FUNCTION;
  ForwardSearch__               :=False;
  GenerateConnectedGoals__      :=GA_DEFAULT_GENERATE_CONNECTED_GOALS;
  PrettyPrinting__              :=DEFAULT_PRETTY_PRINTING;
  MaxBoxCountEnabled__          :=False;
  MaxOpenPositionsEnabled__     :=False;
  InactivityThresholdEnabled__  :=InactivityThreshold__>0;
  TrivialOpeningPushesIgnored__ :=DEFAULT_TRIVIAL_OPENING_PUSHES_IGNORED;
  TailPushesThresholdEnabled__  :=TailPushesThreshold__>0;
  RandomSeedEnabled__           :=True;
  FixedTemplate__               :=False;
  AutoSaveIntervalTimeMS__      :=GA_DEFAULT_AUTO_SAVE_INTERVAL_TIME_MS;

  while Result and (ParamCount>=ItemIndex) do begin {get options}
     s:=ParamStr(ItemIndex);
     if (LastKeyNameIndex=Pred(ItemIndex)) and {previous parameter was a name, e.g., 'method', or 'fitness'}
        (s<>'') and (s[1]<>'-') and (s[1]<>SLASH) then s:='-'+s; {prepend "-" if it's missing}
     Result:=(Length(s)>=2) and ((s[1]='-') or (s[1]=SLASH));
     if Result then
        case UpCase(s[2]) of
          'B'       : if            Length(s)<3 then
                                    Result:=False
                      else if       UpCase(s[3])='O' then
                                    Result:=GetParameter(BoxCount__,0,MAX_BOX_COUNT,0,ItemIndex)
                           else if  UpCase(s[3])='I' then begin
                                    GeneratorMethod__:=gmBidirectional; Inc(ItemIndex);
                                    end
                           else if UpCase(s[3])='A' then begin
                                    if   GeneratorMethod__=gmGA then
                                         ForwardSearch__  :=False
                                    else GeneratorMethod__:=gmBackward;
                                    Inc(ItemIndex);
                                    end
                           else Result:=False;
          'C'       : if        (Length(s)>=3) and (UpCase(s[3])='R') then
                                Result    :=GetParameter(CrossOverProbabilityPct__,0,100,0,ItemIndex)
                      else if   (Length(s)>=3) and (UpCase(s[3])='O') then begin {'-connected', i.e., generate levels with connected goals}
                                GenerateConnectedGoals__:=True; Inc(ItemIndex);
                                end
                           else Result:=False;
          'D'       : if        (Length(s)>=3) and (UpCase(s[3])='E') then
                                Result:=GetParameter(BoardDepth__,MIN_BOARD_DEPTH,MAX_BOARD_DEPTH,0,ItemIndex)
                      else      Result:=False;
          'E'       : begin     GeneratorMethod__:=gmEnumerate; Inc(ItemIndex);
                      end;
          'F'       : if        (Length(s)>=4) and (UpCase(s[3])='I') then
                                if        UpCase(s[4])='T' then begin
                                          LastKeyNameIndex:=ItemIndex; Inc(ItemIndex);
                                          end
                                else if   UpCase(s[4])='L' then
                                          if   ParamCount>ItemIndex then begin
                                               InputFileName__:=ParamStr(Succ(ItemIndex)); Inc(ItemIndex,2);
                                               end
                                          else Result:=False
                                     else if   UpCase(s[4])='X' then begin
                                               FixedTemplate__:=True; Inc(ItemIndex);
                                               end
                                          else Result:=False
                      else if   (Length(s)>=4) and (UpCase(s[3])='O') then begin
                                if   GeneratorMethod__=gmGA then begin
                                     ForwardSearch__  :=True;
                                     GenerateConnectedGoals__:=False;
                                     end
                                else GeneratorMethod__:=gmForward;
                                Inc(ItemIndex);
                                end
                           else Result:=False;
          'G'       : if        (Length(s)>=3) and (UpCase(s[3])='A') then begin
                                GeneratorMethod__:=gmGA; Inc(ItemIndex);
                                end
                      else if   (Length(s)>=3) and (UpCase(s[3])='E') then
                                Result:=GetParameter(MaxGenerationCount__,0,High(MaxGenerationCount__),0,ItemIndex)
                           else if   (Length(s)>=4) and (UpCase(s[4])='O') then
                                     Result:=GetParameter(GoodEnoughFitness__,0,High(GoodEnoughFitness__),0,ItemIndex)
                                else Result:=GetParameter(BoxCount__,0,MAX_BOX_COUNT,0,ItemIndex);
          'H'       : if        (Length(s)>=4) and (UpCase(s[4])='I') then
                                Result:=GetParameter(BoardHeight__,MIN_BOARD_HEIGHT-2,MAX_BOARD_HEIGHT-2,0,ItemIndex) {'-2': wall-filled border}
                      else      Result:=False;
          'I'       : begin     Result:=GetParameter(InactivityThreshold__,0,High(InactivityThreshold__),0,ItemIndex);
                                if Result then InactivityThresholdEnabled__:=InactivityThreshold__>0;
                      end;
          'L'       : if        LastKeyNameIndex=Pred(ItemIndex) then begin {'-fitness lines' (for backward compatibility only; use 'fitness score' instead)}
                                FitnessFunction__:=ffScore; Inc(ItemIndex);
                                end
                      else      Result    :=GetParameter(StartNo__,1,High(StartNo__),0,ItemIndex);
          'M'       : if        (Length(s)>=3) and (UpCase(s[3])='I') then
                                Result:=GetParameter(MinPushCount__,0,High(MinPushCount__),0,ItemIndex)
                      else if   (Length(s)>=3) and (UpCase(s[3])='A') then
                                if (Length(s)>=5) and (UpCase(s[5])='B') then begin
                                   Result:=GetParameter(MaxBoxCount__,0,MAX_BOX_COUNT,0,ItemIndex);
                                   if Result then MaxBoxCountEnabled__:=True;
                                   end
                                else begin
                                   Result:=GetParameter(MaxOpenPositions__,0,High(MaxOpenPositions__),0,ItemIndex);
                                   if Result then MaxOpenPositionsEnabled__:=MaxOpenPositions__<>High(MaxOpenPositions__);
                                   end
                      else if   (Length(s)>=3) and (UpCase(s[3])='U') then
                                Result:=GetParameter(MutationProbabilityPct__,0,100,0,ItemIndex)
                      else if   (Length(s)>=4) and (UpCase(s[4])='T') then begin {'-method'}
                                LastKeyNameIndex:=ItemIndex; Inc(ItemIndex);
                                end
                           else Result:=GetParameter(MemoryByteSize__,0,High(MemoryByteSize__),ONE_MEBI,ItemIndex);
          'P'       : if        (Length(s)>=3) and (UpCase(s[3])='U') then begin
                                FitnessFunction__:=ffPushes; Inc(ItemIndex);
                                end
                      else if   (Length(s)>=3) and (UpCase(s[3])='O') then
                                Result:=GetParameter(PopulationSize__,0,GA_MAX_INDIVIDUALS,0,ItemIndex)
                      else if   (Length(s)>=3) and (UpCase(s[3])='R') and
                                (ItemIndex<ParamCount) then begin
                                Inc(ItemIndex); s:=ParamStr(ItemIndex); Inc(ItemIndex);
                                if        (s<>'') and (Upcase(s[1])='Y') then
                                          PrettyPrinting__:=True
                                else if   (s<>'') and (Upcase(s[1])='N') then
                                          PrettyPrinting__:=False
                                     else Result:=False;
                                end
                           else Result:=False;
          'R'       : if        (Length(s)>=3) and (UpCase(s[3])='U') then begin
                                FitnessFunction__:=ffScore; Inc(ItemIndex);
                                end
                      else if  (Length(s)>=8) and (UpCase(s[8])='S') then begin {'-randomseed'}
                                Result:=GetParameter(RandomSeed__,0,High(RandomSeed__),0,ItemIndex);
                                if Result then RandomSeedEnabled__:=False;
                                end
                           else if (Length(s)>=3) and (UpCase(s[3])='A') then begin {'-random', i.e., generate levels with goals scattered at random}

                                   { // the random level generator is disabled,
                                     // hence, the following statements are
                                     // commented out; instead, the program
                                     // launches the genetic generator;

                                   if GeneratorMethod__=gmGA then begin
                                      GenerateConnectedGoals__:=False; Inc(ItemIndex);
                                      end
                                   else begin
                                      GeneratorMethod__:=gmRandom; Inc(ItemIndex);
                                      end;
                                   }

                                   GeneratorMethod__:=gmGA;
                                   GenerateConnectedGoals__:=False;
                                   Inc(ItemIndex);
                                   end
                                else Result:=False;
          'S'       : if        (Length(s)>=3) and (UpCase(s[3])='T') then begin {'-sta': 'single-target-area' levels, i.e., generate levels with connected goals}
                                GenerateConnectedGoals__:=True; Inc(ItemIndex);
                                end
                      else if   LastKeyNameIndex=Pred(ItemIndex) then begin {'-fitness score'}
                                FitnessFunction__:=ffScore; Inc(ItemIndex);
                                end
                      else      begin Result:=GetParameter(RandomSeed__,0,High(RandomSeed__),0,ItemIndex);
                                      if Result then RandomSeedEnabled__:=False;
                                end;
          'T'       : if        Length(s)>=3 then
                                if        UpCase(s[3])='Y' then begin {'-type'}
                                          LastKeyNameIndex:=ItemIndex; Inc(ItemIndex); GeneratorMethod__:=gmGA;
                                          end
                                else if   UpCase(s[3])='A' then begin
                                          Result:=GetParameter(TailPushesThreshold__,0,MAX_TAIL_POSITION_THRESHOLD,0,ItemIndex);
                                          if Result then begin
                                             TailPushesThreshold__:=Min(TailPushesThreshold__,MAX_TAIL_POSITION_THRESHOLD);
                                             if   System.Pos('?',s)<>0 then {"-tail?": tail position information isn't used by the fitness function; it's calculated for information only}
                                                  TailPushesThreshold__:=-TailPushesThreshold__;
                                             TailPushesThresholdEnabled__:=TailPushesThreshold__<>0; {'<>0': calculate the tail pushes, no matter if they are used by the fitness function, or if they are calculated for information only}
                                             end;
                                          end
                                     else Result:=False
                      else      Result:=False;

          'W'       : if        (Length(s)>=3) and (UpCase(s[3])='I') then
                                Result:=GetParameter(BoardWidth__,MIN_BOARD_WIDTH-2,MAX_BOARD_WIDTH-2,0,ItemIndex) {'-2': wall-filled border}
                      else      Result:=GetParameter(WallCount__ ,0,(MAX_BOARD_WIDTH-2)*(MAX_BOARD_HEIGHT-2),0,ItemIndex);
          else        Result:=False;
       end; {case}
     end;

  {if GeneratorMethod__ <>gmGA then FitnessFunction__:=ffPushes;}
  if ForwardSearch__ then begin
     TrivialOpeningPushesIgnored__:=False;                          {discount trivial opening pushes is only implemented for the backward search}
     TailPushesThreshold__:=0; TailPushesThresholdEnabled__:=False; {discount trivial tail positions is only implemented for the backward search}
     end;

  Result:=Result and (ItemIndex>ParamCount);

  if Result then begin
     if   BoardWidth__*BoardHeight__*BoardDepth__<>0 then begin
          Inc(BoardWidth__,2); Inc(BoardHeight__,2); Inc(BoardDepth__,0 {sic}); {add exterior wall}
          Result:=((WallCount__+BoxCount__+1)<=(BoardWidth__-2)*(BoardHeight__-2)*BoardDepth__); {'+1': player}
          if (GeneratorMethod__<>gmGA) and (GeneratorMethod__<>gmEnumerate) then GeneratorMethod__:=gmRandom;
          end
     else Result:=InputFileName__<>'';
     end;
end;

procedure Msg({$IFDEF DELPHI} const {$ENDIF} Text__,Caption__:String);
begin
  {$IFDEF CONSOLE_APPLICATION}
    Writeln;
    if Caption__<>'' then Writeln(Caption__);
    if Text__   <>'' then Writeln(Text__);
    Writeln(''); Write('Press [Enter]');
    Readln;
  {$ENDIF}
  {$IFDEF SILENT_APPLICATION}
    if Text__<>'' then
       if   Caption__<>'' then
            Windows.MessageBox(0,PChar(Text__),PChar(Caption__),Windows.MB_OK)
       else Windows.MessageBox(0,PChar(Text__),TEXT_APPLICATION_TITLE,Windows.MB_OK);
  {$ENDIF}
end;

procedure ShowHelp;
begin
  {$IFDEF CONSOLE_APPLICATION}
    Writeln;
    Writeln('Usage: YASGen3D [options]');
    Writeln;
    Writeln('General options:');
    Writeln('  -help                         : this overview');
    Writeln('  -maxboxes    <number>         : maximum number of boxes');
    Writeln('  -maxopen     <number>         : open-queue size limit');
    Writeln('  -memory      <size> (MiB)     : default ',DEFAULT_MEMORY_BYTE_SIZE div ONE_MEBI,' MiB');
    Writeln('  -method      <method>         : forward.........: Use boxes');
    Writeln('                                : backward........: Use goals (default)');
    Writeln('                                : birectional.....: Forward and backward');
    Writeln('                                : ga..............: Use a genetic algorithm');
    Writeln('  -minpushes   <number>         : threshold for saving generated levels');
{   Writeln('  -prettyprint <no|yes>         : pretty-printing boards, default "',TEXT_NO_YES[DEFAULT_PRETTY_PRINTING_ENABLED],'"');}
    Writeln('  -seed        <number>         : random seed');
    Writeln('  -tail        <number>         : tail removal threshold 0..',MAX_TAIL_POSITION_THRESHOLD,', default ',DEFAULT_TAIL_POSITION_THRESHOLD);
    Writeln('  -tail?       <number>         : show tail stats but don''t discount tails');
    Writeln;
    Writeln('More...');
    Msg('','');
    Writeln;
    Writeln;
    Writeln('File options:');
    Writeln('  -file        <filename>       : level file, template file, or GA log file');
    Writeln('  -fixed                        : template contains fixed squares (GA only)');
    Writeln('  -level       <number>         : level number in file, default 1');
    Writeln;
    Writeln('Genetic-algorithm options');
    Writeln('  -crossover   <pct>            : crossover probability, percent');
    Writeln('  -fitness     <fitness>        : fitness.........: Score or Pushes (default)');
    Writeln('  -generations <number>         : maximum number of generations');
    Writeln('  -inactivity  <number>         : inact. threshold: 0..99999 (default ',GA_DEFAULT_INACTIVITY_THRESHOLD,')');
    Writeln('  -mutation    <pct>            : mutation  probability, percent');
    Writeln('  -population  <number>         : population size.: 0..',GA_MAX_INDIVIDUALS,' (default ',GA_DEFAULT_POPULATION_SIZE,')');
    Writeln('  -type        <level type>     : level type......: Random or Connected (def.)');
    Writeln('                                    Random: goals are scattered at random.');
    Writeln('                                    Connected: All goals are connected.');
    Writeln;
    Writeln('Random-generation options');
    Writeln('  -boxes       <number>         : number of boxes/goals');
    Writeln('  -depth       <number>         : interior depth, without outer wall');
    Writeln('  -goals       <number>         : number of boxes/goals');
    Writeln('  -height      <number>         : interior height, without outer wall');
    Writeln('  -walls       <number>         : number of interior wall squares');
    Writeln('  -width       <number>         : interior width, without outer wall');
  {$ENDIF}
  {$IFDEF SILENT_APPLICATION}
    Windows.MessageBox(0,'Please consult the source code for a list of command line arguments.',TEXT_APPLICATION_TITLE,Windows.MB_OK);
  {$ENDIF}
end;


procedure ShowTitle;
begin
  {$IFDEF CONSOLE_APPLICATION}
    Writeln(GA_TEXT_APPLICATION_TITLE_LONG,' - For Small Levels');
    Writeln(GA_TEXT_VERSION,SPACE,TEXT_APPLICATION_VERSION);
    Writeln(GA_TEXT_COPYRIGHT,SPACE,TEXT_APPLICATION_COPYRIGHT);
  {$ENDIF}
end;

{-----------------------------------------------------------------------------}

{Statistics}

procedure ClearStatistics;
var p:PLevelStatisticsItem;
begin
 while LevelStatistics<>nil do begin
   p:=LevelStatistics^.Next;
   FreeMem(LevelStatistics,SizeOf(LevelStatistics^));
   LevelStatistics:=p;
   end;
end;

procedure InitializeStatistics;
begin
  LevelStatistics:=nil; ClearStatistics;
end;

procedure FinalizeStatistics;
begin
  ClearStatistics;
end;

function MakeLevelStatistics(const Name__:String; ExhaustiveSearch__:Boolean;
                             Width__,Height__,
                             PushCount__,Score__,Fitness__,TailCount__,PositionCount__:Integer):PLevelStatisticsItem;
begin
  {$IFDEF VERBOSE_OUTPUT}
    (* {$HINTS OFF} Result:=nil; {$HINTS ON} *) {'nil': in case 'GetMem' fails and returns 'nil' instead of raising an exception}
    {$WARNINGS OFF} {'Result' not initialized}
      GetMem(Result,SizeOf(Result^));
    {$WARNINGS ON}  
    if Result<>nil then with Result^ do begin
       FillChar(Result^,SizeOf(Result^),0);
       ExhaustiveSearch :=ExhaustiveSearch__;
       Height           :=Height__;
       Name             :=Name__;
       Next             :=LevelStatistics;
       PositionCount    :=PositionCount__;
       PushCount        :=PushCount__;
       Score            :=Score__;
       Fitness          :=Fitness__;
       TailCount        :=TailCount__;
       Width            :=Width__;
       LevelStatistics  :=Result; {updates the global list}
       end;
  {$ELSE}
    Result:=nil;
  {$ENDIF}
end;

function WriteStatistics(OutputFileName__:String):Boolean;
{$IFDEF VERBOSE_OUTPUT}
  const SHORT_LINE_LENGTH=78; TAIL_COUNT_COLUMN_WIDTH=8;
  var
    i,Count,ExhaustiveSearchCount,BestScore,LineLength:Integer; s:String;
    Total:TLevelStatisticsItem;
    This,Next:PLevelStatisticsItem; F:Text;
  begin {$I-}
    s:=ExtractFileNameWithoutExt(OutputFileName__)+TEXT_STATISTICS_FILENAME_SUFFIX;
    Assign(F,s); Rewrite(F);
    Result:=IOResult=0;
    if Result then begin
       Count:=0; ExhaustiveSearchCount:=0; FillChar(Total,SizeOf(Total),0);
       LineLength:=SHORT_LINE_LENGTH;
       This:=LevelStatistics; LevelStatistics:=nil; BestScore:=0;
       while This<>nil do begin {reverse items}
         if This^.Fitness>BestScore then BestScore:=This^.Fitness;
         Inc(Total.TailCount,This^.TailCount);
         Next:=This^.Next; This^.Next:=LevelStatistics; LevelStatistics:=This; This:=Next;
         end;
       if Total.TailCount<>0 then Inc(LineLength,TAIL_COUNT_COLUMN_WIDTH);

       for i:=1 to LineLength do Write(F,'-'); Writeln(F);
       Writeln(F,TEXT_APPLICATION_TITLE,' Results');
       Writeln(F,'File ....: ',OutputFileName__);
       i:=(Positions.MemoryByteSize+(ONE_MEBI div 2)) div ONE_MEBI;
       Writeln(F,'Memory ..: ',i,' MiB');
       Writeln(F,'Method ..: ',TEXT_GENERATOR_METHOD[Generator.Method]);
       Writeln(F,'Fitness .: ',GA_TEXT_FITNESS_FUNCTION_TYPES[GA.Control.FitnessFunction]);
       s:=TEXT_NO_YES[ Generator.TrivialOpeningPushesIgnored ];
       Writeln(F,'Trivial opening pushes ignored: ',s);
       if Generator.TailPushesThreshold<>0 then begin
          i:=Abs(Generator.TailPushesThreshold);
          Write(F,'Tails ...: ',i,' session(s) per box');
          if (not Generator.TailPushesThresholdEnabled) or (Generator.TailPushesThreshold<0) then
             Write(F,'  (Tails are not discounted by the fitness function)');
          Writeln(F);
          end;
       if (Generator.MaxOpenPositions<>High(Generator.MaxOpenPositions)) and Generator.MaxOpenPositionsEnabled then
          Writeln(F,'Max. Open: ',Generator.MaxOpenPositions);
       for i:=1 to LineLength do Write(F,'-'); Writeln(F);
       Write(F,'Level':36,'Minimum':8,'Estimated':12,'Seen':9,'Exhaustive':11);
       if   Total.TailCount<>0 then Writeln(F,'Tail':TAIL_COUNT_COLUMN_WIDTH)
       else Writeln(F);
       Write(F,'No':4,'W':3,'H':3,'Name':26,'Pushes':8,'Score':8,'Positions':13,'Search':11);
       if   Total.TailCount<>0 then Writeln(F,'Pushes':TAIL_COUNT_COLUMN_WIDTH)
       else Writeln(F);

       for i:=1 to LineLength do Write(F,'-'); Writeln(F);

       This:=LevelStatistics;
       while This<>nil do with This^ do begin
         Inc(Count);
         if ExhaustiveSearch then Inc(ExhaustiveSearchCount);
         Inc(Total.PositionCount,PositionCount);
         Inc(Total.PushCount,PushCount);
         Inc(Total.Score,Score);

         Write(F,Count:4,Width:3,Height:3);

         s:=Name;
         i:=Pos(DOUBLE_QUOTE,s);
         if i<>0 then Delete(s,1,i);
         i:=Pos(DOUBLE_QUOTE,s);
         if i<>0 then Delete(s,i,Length(s)-i+1);
         i:=Pos(TEXT_LEVEL_TITLE_SUFFIX,s);
         if i<>0 then Delete(s,i,Length(s)-i+1);
         if Length(s)>25 then Delete(s,26,Length(s)-25);
         Write(F,s:26);

         Write(F,PushCount:8,Score:8,PositionCount:13);

         if ExhaustiveSearch then s:=TEXT_NO_YES[True]
         else s:='';
         Write(F,s:11);

         if   TailCount<>0 then Write(F,TailCount:TAIL_COUNT_COLUMN_WIDTH)
         else if Total.TailCount<>0 then Write(F,'':TAIL_COUNT_COLUMN_WIDTH);

         if (Generator.Method=gmGA) and
            (This^.Fitness=BestScore) then
            Write(F,' *');
         Writeln(F);

         This:=This^.Next;
         end;

       if Count>0 then begin for i:=1 to LineLength do Write(F,'-'); Writeln(F); end;
       with Total do Write(F,Count:4,'Total':32,PushCount:8,Score:8,PositionCount:13,ExhaustiveSearchCount:11);
       if   Total.TailCount<>0 then Writeln(F,Total.TailCount:8)
       else Writeln(F);

       for i:=1 to LineLength do Write(F,'-'); Writeln(F);

       if Total.TailCount<>0 then with Total do begin
          i:=PushCount-TailCount;
          Writeln(F,'Non-tail pushes':36,i:8,'Tail pushes':32,TailCount:TAIL_COUNT_COLUMN_WIDTH);
          for i:=1 to LineLength do Write(F,'-'); Writeln(F);
          end;

       if (Generator.Method=gmGA) and (BestScore<>0) then begin
          Writeln(F,'  *: Level(s) with best fitness');
          for i:=1 to LineLength do Write(F,'-'); Writeln(F);
          end;

       Close(F);

       This:=LevelStatistics; LevelStatistics:=nil;
       while This<>nil do begin {reverse items again}
         Next:=This^.Next; This^.Next:=LevelStatistics; LevelStatistics:=This; This:=Next;
         end;

       Result:=IOResult=0;
       end
    else
       Msg('File Creation Error: '+s,TEXT_APPLICATION_TITLE);
  end; {$I+}
{$ELSE}
  begin
    Result:=False;
  end;
{$ENDIF}

{-----------------------------------------------------------------------------}

{Board}

function  PrepareBoardForPrettyPrinting(GuardAgainstTrimming__:Boolean):Integer;

  function RemoveRedundantWalls:Integer;
  var i,j,Col,Row,Depth,FirstRow,LastRow:Integer; Direction:TDirection; HasNeighbourFloor:TBoardBoolean;
  begin
    with Game do begin
      Result:=0;
      FillChar(HasNeighbourFloor,SizeOf(HasNeighbourFloor),0);
      for i:=1 to BoxCount do Dec(Board[BoxPos[i]],BOX); {remove boxes from the board}
      CalculatePlayersReachableSquares(i);               {find player's reachable squares, i.e., the active squares on the board}
      for i:=1 to BoxCount do Inc(Board[BoxPos[i]],BOX); {put boxes back on the board}

      for i:=0 to BoardSize do
          if PlayersReachableSquares[i]=PlayersReachableSquaresTimeStamp then begin
             for Direction:=Low(Direction) to High(Direction) do begin
                 j:=i+NeighbourSquareOffset[Direction];
                 HasNeighbourFloor[j]:=True;
                 if      (Direction=dUp) or (Direction=dDown) then begin {caution: hardwired to 6 directions only, i.e., 3D}
                         HasNeighbourFloor[j+NeighbourSquareOffset[dLeft ]]:=True; {diagonal neighbour}
                         HasNeighbourFloor[j+NeighbourSquareOffset[dRight]]:=True; {diagonal neighbour}
                         HasNeighbourFloor[j+NeighbourSquareOffset[dIn   ]]:=True; {diagonal neighbour}
                         HasNeighbourFloor[j+NeighbourSquareOffset[dOut  ]]:=True; {diagonal neighbour}
                         end
                 else if (Direction=dLeft) or (Direction=dRight) then begin
                         HasNeighbourFloor[j+NeighbourSquareOffset[dUp   ]]:=True; {diagonal neighbour}
                         HasNeighbourFloor[j+NeighbourSquareOffset[dDown ]]:=True; {diagonal neighbour}
                         HasNeighbourFloor[j+NeighbourSquareOffset[dIn   ]]:=True; {diagonal neighbour}
                         HasNeighbourFloor[j+NeighbourSquareOffset[dOut  ]]:=True; {diagonal neighbour}
                         end
                      else begin {(Direction=dIn) or (Direction=dOut) }
                         HasNeighbourFloor[j+NeighbourSquareOffset[dLeft ]]:=True; {diagonal neighbour}
                         HasNeighbourFloor[j+NeighbourSquareOffset[dRight]]:=True; {diagonal neighbour}
                         HasNeighbourFloor[j+NeighbourSquareOffset[dUp   ]]:=True; {diagonal neighbour}
                         HasNeighbourFloor[j+NeighbourSquareOffset[dDown ]]:=True; {diagonal neighbour}
                         end;
                 end;
             end
          else if (Board[i] and WALL)=0 then begin
                  if (Board[i] and FLOOR)<>0 then Dec(FloorCount);
                  Board[i]:=WALL; Inc(Result);
                  end;

      for i:=0 to BoardSize do
          if ((Board[i] and WALL)<>0) and (not HasNeighbourFloor[i]) then begin
             Board[i]:=FLOOR; Inc(FloorCount); Inc(Result);
             end;

      if GuardAgainstTrimming__ then
         {ensure that the board won't be misaligned with other candidates}
         {because of trimming when the board is loaded}
         for Depth:=1 to Game.BoardDepth do begin
             FirstRow:=0; {find the first non-empty row}
             for Row:=1 to BoardHeight do
                 if   FirstRow=0 then begin
                      if not IsEmptyRowAtDepth(Row,Depth) then FirstRow:=Row;
                      end
                 else break;

             for Row:=1 to Pred(FirstRow) do begin
                 {the row is empty; add a wall to the row to ensure correct alignment when the board is loaded}
                 if (Board[ColRowDepthToSquare(1,Row,Depth,Game.BoardWidth,Game.BoardHeight)] and FLOOR)<>0 then Dec(FloorCount);
                 Board[    ColRowDepthToSquare(1,Row,Depth,Game.BoardWidth,Game.BoardHeight)]:=WALL; Inc(Result);
                 end;

             LastRow:=0; {find the last non-empty row}
             for Row:=BoardHeight downto 1 do
                 if   LastRow=0 then begin
                      if not IsEmptyRowAtDepth(Row,Depth) then LastRow:=Row;
                      end
                 else break;

             Col:=1;
             while (Col<=BoardWidth) and IsEmptyColAtDepth(Col,Depth) do begin
               {the column is empty; add a wall to the bottom row to ensure correct alignment when the board is loaded}
               if (Board[ColRowDepthToSquare(Col,LastRow,Depth,Game.BoardWidth,Game.BoardHeight)] and FLOOR)<>0 then Dec(FloorCount);
               Board[    ColRowDepthToSquare(Col,LastRow,Depth,Game.BoardWidth,Game.BoardHeight)]:=WALL; Inc(Result);
               Inc(Col);
               end;

             {add a wall to each empty row at the bottom of the board at this depth}
             for Row:=Succ(LastRow) to BoardHeight do begin
                 {the row is empty; add a wall to the row to ensure correct alignment when the board is loaded}
                 if (Board[ColRowDepthToSquare(1,Row,Depth,Game.BoardWidth,Game.BoardHeight)] and FLOOR)<>0 then Dec(FloorCount);
                 Board[    ColRowDepthToSquare(1,Row,Depth,Game.BoardWidth,Game.BoardHeight)]:=WALL; Inc(Result);
                 end;
             end;
      end;
  end;

begin {PrepareBoardForPrettyPrinting}
  Result:=FillTubes(False)+RemoveRedundantWalls;
end;

procedure BoardToTextLines(var BoardTextLines__:TBoardTextLines; var FirstNonEmptyRow__,LastNonEmptyRow__,FirstNonEmptyDepth__,LastNonEmptyDepth__:Integer);
var  Col,Row,Depth,First,Last:Integer; Ch:Char; IsEmptyRow:array[0..MAX_BOARD_HEIGHT] of Boolean;
begin
  with Game do begin
    FirstNonEmptyRow__:=1; LastNonEmptyRow__:=BoardHeight;
    FirstNonEmptyDepth__:=BoardHeight+1; LastNonEmptyDepth__:=0;
    for Depth:=1 to BoardDepth do begin
        for Row:=1 to BoardHeight do begin
            IsEmptyRow[Row]:=True;
            BoardTextLines__[Depth,Row]:='';
            for Col:=1 to BoardWidth do begin
                Ch:=SquareToChar(ColRowDepthToSquare(Col,Row,Depth,Game.BoardWidth,Game.BoardHeight));
                {$WARNINGS OFF}
                  BoardTextLines__[Depth,Row]:=BoardTextLines__[Depth,Row]+Ch; {implicit string cast from 'ShortString' to 'String'}
                {$WARNINGS ON}
                if Ch<>SPACE then IsEmptyRow[Row]:=False;
                end;
            end;
        First:=1; Last:=BoardHeight;
        while (First<=Last) and IsEmptyRow[First] do Inc(First);
        while (First<=Last) and IsEmptyRow[Last ] do Dec(Last );
        FirstNonEmptyRow__:=Min(FirstNonEmptyRow__,First);
        LastNonEmptyRow__ :=Max(LastNonEmptyRow__ ,Last );
        if First<=Last then begin {'True': a non-empty depth}
           FirstNonEmptyDepth__:=Min(Depth,FirstNonEmptyDepth__);
           LastNonEmptyDepth__ :=Max(Depth,LastNonEmptyDepth__ );
           end;
        end;
    end;
end;

function  BoardToText(BoardWidth__,BoardHeight__,BoardDepth__:Integer; const Board__:TBoard; const LineSeparator__,LayerSeparator__:String):String;
var Col,Row,Depth,Index,Index2,RowOffset:Integer;
begin
  SetLength(Result,(BoardWidth__*BoardHeight__*BoardDepth__)+
                   Max(0,Pred(BoardDepth__))*Max(0,Pred(BoardHeight__)*Length(LineSeparator__ )*SizeOf(Char))+
                   Max(0,Pred(BoardDepth__))*                          Length(LayerSeparator__)*SizeOf(Char));
  Index:=0;
  for Depth:=1 to BoardDepth__ do begin
      for Row:=1 to BoardHeight__ do begin
          RowOffset:=ColRowDepthToSquare(0,Row,Depth,BoardWidth__,BoardHeight__);
          for Col:=1 to BoardWidth__ do begin
              Inc(Index); Result[Index]:=BoardSquareValueToChar(Board__[RowOffset+Col]);
              end;
          if  Row<BoardHeight__ then
              for Index2:=1 to Length(LineSeparator__) do begin
                  Inc(Index); Result[Index]:=LineSeparator__[Index2];
                  end;
          end;
      if  Depth<BoardDepth__ then
          for Index2:=1 to Length(LayerSeparator__) do begin
              Inc(Index); Result[Index]:=LayerSeparator__[Index2];
              end;
      end;
end;

function  CalculateBoardHashValue:THashValue;
var i:Integer;
begin {player's position isn't considered; it's saved separately}
  Result:=0;
  for i:=1 to Game.BoxCount do
      Result:=Result xor Game.HashSquareValues[Game.BoxPos[i]];
end;

function  CalculateConnectedGoals(GoalSquare__,TimeStamp__,VisitedCount__:Integer;
                                  const Board__:TBoard; var VisitedSquares__:TBoardInteger):Integer;
var NeighbourSquare:Integer; Direction:TDirection;
begin
  Result:=Succ(VisitedCount__); {'+1': 'GoalSquare__' is new, i.e., a not previously visited goal}
  VisitedSquares__[GoalSquare__]:=TimeStamp__;
  for Direction:=Low(Direction) to High(Direction) do begin
      NeighbourSquare:=GoalSquare__+Game.NeighbourSquareOffset[Direction];
      if ((Board__[NeighbourSquare] and GOAL)<>0) and
         (VisitedSquares__[NeighbourSquare]<>TimeStamp__) then
         Result:=CalculateConnectedGoals(NeighbourSquare,TimeStamp__,Result,Board__,VisitedSquares__);
         end;
end;

procedure DumpBoard;
{$IFDEF CONSOLE_APPLICATION}
  var Col,Row,Depth:Integer;
  begin
    with Game do begin
      for Depth:=1 to BoardDepth do begin
          for Row:=1 to BoardHeight do begin
              for Col:=1 to BoardWidth do Write(Board[ColRowDepthToSquare(Col,Row,Depth,Game.BoardWidth,Game.BoardHeight)]:3);
              Writeln;
              end;
          if Depth<BoardDepth then
             Writeln(BOARD_3D_LAYER_SEPARATOR);
          end;
      end;
  end;
{$ELSE}
  begin
  end;
{$ENDIF}

function  FillTubes(MoveItemsEnabled__:Boolean):Integer;
var i,j,BoxOnGoalCount,NeighbourFloorCount,NewBoxPos,NewPlayerPos:Integer;
    {TubeFillingMoveCount,TubeFillingPushCount:Integer;}
    DeadEnd,More:Boolean; Dir,Direction:TDirection;
begin
  with Game do begin
    Result:=0;
    BoxOnGoalCount:=0; Direction:=dUp;
    {TubeFillingMoveCount:=0; TubeFillingPushCount:=0;} {player moves and box pushes}
    for i:=1 to BoxCount do
        if IsAGoalSquare(BoxPos[i]) then Inc(BoxOnGoalCount);

    repeat
      More:=False;
      for i:=0 to BoardSize do
          if (Board[i] and (WALL+BOX+GOAL))=0 then begin {for each non-goal, non-box floor-square...}
             NeighbourFloorCount:=0;
             for Dir:=Low(Dir) to High(Dir) do
                 if IsAFloorSquare(i+NeighbourSquareOffset[Dir]) then begin
                    Inc(NeighbourFloorCount); Direction:=Dir;
                    end;
             if NeighbourFloorCount<=1 then begin {the floor-square is surrounded by walls on all sides}
                DeadEnd:=True; NewBoxPos:=0; NewPlayerPos:=0;
                if i=PlayerPos then
                   if (NeighbourFloorCount=0) or (not MoveItemsEnabled__) then
                      DeadEnd:=False {the player is on an isolated square or moving items isn't allowed; don't change anything}
                   else begin        {try to move the player to the neighbour floor-square}
                      NewPlayerPos:=i+NeighbourSquareOffset[Direction];
                      if IsABoxSquare(NewPlayerPos) then begin
                         NewBoxPos:=NewPlayerPos+NeighbourSquareOffset[Direction];
                         DeadEnd:=((Board[NewBoxPos] and (WALL+BOX))=0) and {the box can be pushed forward}
                                  (BoxOnGoalCount<BoxCount) and             {the position isn't a solution}
                                  (History.Count<High(History.Moves));      {the game history isn't full}
                         end;
                      end;
                if DeadEnd then begin {dead-end: place a wall on the square}
                   if NewBoxPos<>0 then begin {move a box}
                      for j:=1 to BoxCount do {find the box number}
                          if BoxPos[j]=NewPlayerPos then begin
                             if IsAGoalSquare(BoxPos[j]) then Dec(BoxOnGoalCount);
                             Dec(Board[BoxPos[j]],BOX); BoxPos[j]:=NewBoxPos;
                             Inc(Board[BoxPos[j]],BOX);
                             if IsAGoalSquare(BoxPos[j]) then Inc(BoxOnGoalCount);
                             {Inc(TubeFillingPushCount);}
                             Inc(History.Count);
                             History.Moves[History.Count].BoxNo:=j;
                             History.Moves[History.Count].Direction:=Direction;
                             break;
                             end;
                      end;
                   if NewPlayerPos<>0 then begin {move the player}
                      Dec(Board[PlayerPos],PLAYER); PlayerPos:=NewPlayerPos;
                      Inc(Board[PlayerPos],PLAYER);
                      {Inc(TubeFillingMoveCount);}
                      end;
                   Board[i]:=WALL; More:=True; Inc(Result);
                   end;
                end;
             end;
    until not More;
    end;
end; {FillTubes}

procedure InitializeBoard(BoardWidth__,BoardHeight__,BoardDepth__:Integer; FillSquares__:Boolean);
var Depth,i,LayerOffset,RowOffset:Integer;
begin
  with Game do begin
    Board3DLayerSize:=(BoardWidth__+2)*(BoardHeight__+2); {the '+2' is for a wall-filled border}
    BoardSize:=Board3DLayerSize*(BoardDepth__+2); {the '+2' is for a wall-filled border}
    NeighbourSquareOffset[dUp   ]:=-(BoardWidth__+2);
    NeighbourSquareOffset[dLeft ]:=-1;
    NeighbourSquareOffset[dDown ]:=+(BoardWidth__+2);
    NeighbourSquareOffset[dRight]:=+1;
    NeighbourSquareOffset[dIn   ]:=+Board3DLayerSize;
    NeighbourSquareOffset[dOut  ]:=-Board3DLayerSize;

    if FillSquares__ then
       for i:=0 to BoardSize do Board[i]:=WALL
    else
       if (BoardWidth<>BoardWidth__) or (BoardHeight<>BoardHeight__) or (BoardDepth<>BoardDepth__) then begin
          {add a wall-filled border to ensure that the player always is surrounded by walls}
          for i:=0 to ColRowDepthToSquare(0,0,1,BoardWidth__,BoardHeight__) do Board[i]:=WALL; {below depth 1}
          for Depth:=1 to BoardDepth__ do begin
              LayerOffset:=ColRowDepthToSquare(0,0,Depth,BoardWidth__,BoardHeight__);
              for i:=0 to BoardWidth__+1 do Board[LayerOffset+i]:=WALL; {top row}
              for i:=ColRowDepthToSquare(BoardWidth__+1,BoardHeight__,Depth,BoardWidth__,BoardHeight__) to {past last square on last row}
                     Pred(LayerOffset+Board3DLayerSize) do {before next 3D layer}
                  Board[i]:=WALL; {bottom row}
              RowOffset:=0;
              for i:=1 to BoardHeight__ do begin
                  Inc(RowOffset,BoardWidth__+2);
                  Board[LayerOffset+RowOffset]:=WALL; Board[LayerOffset+RowOffset+BoardWidth__+1]:=WALL; {left and right columns}
                  end;
              end;
          LayerOffset:=ColRowDepthToSquare(0,0,Succ(BoardDepth__),BoardWidth__,BoardHeight__);
          for i:=0 to Pred(Board3DLayerSize) do Board[LayerOffset+i]:=WALL; {after highest depth}
          end;
    BoardWidth:=BoardWidth__; BoardHeight:=BoardHeight__; BoardDepth:=BoardDepth__;
    end;
end;

function  LoadAndNormalizeBoard(var Board__:TBoard):Integer;
var BoxNo,BoxesOnBoardCount,MinPlayerPos,OriginalPlayerPos,SquareNo:Integer; BoxesOnBoard:TBoxPositions;
begin {fills dead floor squares with walls; returns number of board modifications, i.e., number of floor squares filled with walls}
  Result:=0;
  Game.Board:=Board__;
  InitializeGame;
  if Game.BoxCount<>0 then with Game do begin
     OriginalPlayerPos:=PlayerPos; BoxesOnBoardCount:=0;
     for BoxNo:=1 to BoxCount do {remove boxes from the board, if any}
         if (Board[BoxPos[BoxNo]] and BOX)<>0 then begin
            Dec(Board[BoxPos[BoxNo]],BOX);
            Inc(BoxesOnBoardCount); BoxesOnBoard[BoxesOnBoardCount]:=BoxPos[BoxNo];
            end;

     for BoxNo:=1 to BoxCount do begin
         if   Game.ReverseMode then PlayerPos:=GoalPos[BoxNo] {'PlayerPos': trick 'CalculatePlayersReachableSquares' so it calculates floor squares connected to the box/goal square}
         else PlayerPos:=BoxPos[BoxNo];
         CalculatePlayersReachableSquares(MinPlayerPos); {calculate floor squares connected to the box/goal square}
         if BoxNo<>BoxCount then Dec(PlayersReachableSquaresTimeStamp); {'Dec': trick 'CalculatePlayersReachableSquares' so it accumulates floor squares for all boxes/goals instead of calculating connected squares for each box/goal square}
         end;

     for SquareNo:=0 to BoardSize do
         if ((Board[SquareNo] and (WALL+FLOOR+BOX+GOAL+PLAYER))=FLOOR) and
            (PlayersReachableSquares[SquareNo]<>PlayersReachableSquaresTimeStamp) and
            (SquareNo<>OriginalPlayerPos) then begin
            Board[SquareNo]:=WALL; Dec(FloorCount); Inc(Result);
            end;

     for BoxNo:=1 to BoxesOnBoardCount do Inc(Board[BoxesOnBoard[BoxNo]],BOX); {put boxes back on the board, if any}
     PlayerPos:=OriginalPlayerPos; {restore the player position}
{
     if Result<>0 then begin
        ShowBoard; Writeln(Result,' after');
        Board:=Board__; ShowBoard; Write('before'); Readln;
        end;
}
     Game.BoardHashValue:=CalculateBoardHashValue;
     end;

  Board__:=Game.Board;
end;

procedure MakeBoard(BoardWidth__,BoardHeight__,BoardDepth__:Integer);
var Col,Row,Depth,RowOffset:Integer;
begin {makes an empty board with a wall border}
  with Game do begin
    InitializeBoard(BoardWidth__,BoardHeight__,BoardDepth__,True);
    for Depth:=1 to BoardDepth do
        for Row:=2 to Pred(BoardHeight) do begin {2..Pred(Height): leave a wall border}
            RowOffset:=ColRowDepthToSquare(0,Row,Depth,Game.BoardWidth,Game.BoardHeight);
            for Col:=2 to Pred(BoardWidth) do Game.Board[RowOffset+Col]:=FLOOR;
            end;
    PlayerPos:=0; BoxCount:=0; GoalCount:=0; FloorCount:=(BoardWidth-2)*(BoardHeight-2)*(BoardDepth-2);
    PlayersReachableSquaresTimeStamp:=High(PlayersReachableSquaresTimeStamp);
    BoardHashValue:=CalculateBoardHashValue;
    end;
end;

function  MakeLevelFrom3DimensionalBoard(BoardWidth__,BoardHeight__,BoardDepth__,MinBoardWidth__,MinBoardHeight__,MinBoardDepth__:Integer; const Board3D__:TBoard3D):Boolean;
var Col,Row,Depth,RowOffset,Square:Integer;
begin
  Result:=True;
  Inc(LevelReader.Count); Generator.Count:=LevelReader.Count;
  if Game.Title='' then Game.Title:=TEXT_LEVEL+SPACE+IntToStr(LevelReader.Count);

  for Square:=Low(Game.Board) to High(Game.Board) do Game.Board[Square]:=WALL; {ensure that unused squares are filled with walls}
  InitializeBoard(Max(BoardWidth__,MinBoardWidth__),Max(BoardHeight__,MinBoardHeight__),Max(BoardDepth__,MinBoardDepth__),False);
  for Depth:=1 to BoardDepth__ do
      for Row:=1 to BoardHeight__ do begin
          RowOffset:=ColRowDepthToSquare(0,Row,Depth,Game.BoardWidth,Game.BoardHeight);
          for Col:=1 to BoardWidth__ do Game.Board[RowOffset+Col]:=Board3D__[Depth,Row,Col] and (WALL+BOX+GOAL+PLAYER+FLOOR+FLAG_FIXED_SQUARE);
          end;

  LoadAndNormalizeBoard(Game.Board);
end;

procedure MakeGoalSquares(Position__:PPosition);
var i,OriginalPlayerPos:Integer;
begin {precondition: 'Position__'<>nil}
  OriginalPlayerPos:=Game.PlayerPos; Game.GoalCount:=0;
  RemovePositionFromOpenPositions(Position__);    {the position might still be a member of 'Open'; get it out before calling 'SetPosition()'}
  SetPosition(Position__);                        {recreate position for this node}
  for i:=0 to Game.BoardSize do begin             {make goals, i.e., use current box-positions as goals}
      Game.Board[i]:=Game.Board[i] and (not GOAL);{remove old goal positions, if any}
      if (Game.Board[i] and BOX)<>0 then begin
         Inc(Game.Board[i],GOAL);
         Inc(Game.GoalCount); Game.GoalPos[Game.GoalCount]:=i;
         end;
      end;
  SetPosition(nil);                               {reset moves again}
  MovePlayer(OriginalPlayerPos);
end;

procedure MoveBoxesToGoalSquares;
var i:Integer;
begin
  with Game do begin
    BoxCount:=0;
    for i:=0 to BoardSize do begin
        Board[i]:=Board[i] and (not BOX);
        if ((Board[i] and GOAL)<>0) and
           (BoxCount<Generator.MaxBoxCount) then begin {put boxes on goal positions}
           Inc(BoxCount); BoxPos[BoxCount]:=i;
           Inc(Board[i],BOX);
           if  (Board[i] and PLAYER)<>0 then begin     {the player cannot be on a box-square}
               PlayerPos:=0; Dec(Board[i],PLAYER);
               end;
           end
        else Board[i]:=Board[i] and (not GOAL);
        Generator.Board[i]:=Board[i];                  {save start position}
        end;
    GoalCount:=BoxCount; for i:=1 to BoxCount do GoalPos[i]:=BoxPos[i];
    end;
end;

procedure RemoveUnconnectedGoals;
var i,j,k,BestGroupNo,BestGroupSize:Integer; VisitedSquares:TBoardInteger;
begin
  FillChar(VisitedSquares,SizeOf(VisitedSquares),0);
  BestGroupNo:=0; BestGroupSize:=0;
  for i:=1 to Game.GoalCount do begin {find largest group}
      j:=CalculateConnectedGoals(Game.GoalPos[i],i,0,Game.Board,VisitedSquares);
      if j>BestGroupSize then begin
         BestGroupSize:=j; BestGroupNo:=i;
         end;
      end;

  if  (BestGroupSize>0) and (BestGroupSize<Game.GoalCount) then begin
      CalculateConnectedGoals(Game.GoalPos[BestGroupNo],Succ(Game.GoalCount),0,Game.Board,VisitedSquares);
      j:=Game.GoalCount;
      for i:=j downto 1 do with Game do
          if VisitedSquares[GoalPos[i]]<=j then begin {'True': goal[i] is not a member of the largest group}
             Dec(GoalCount); BoxCount:=GoalCount;
             Dec(Board[GoalPos[i]],GOAL);
             Dec(Board[BoxPos [i]],BOX );
             for k:=i to GoalCount do begin
                 GoalPos[k]:=GoalPos[Succ(k)];
                 BoxPos [k]:=BoxPos [Succ(k)];
                 end;
             end;
      Game.BoardHashValue:=CalculateBoardHashValue;
      end;
end;

procedure ShowBoard;
{$IFDEF CONSOLE_APPLICATION}
  var Row,Depth,FirstRow,LastRow,FirstDepth,LastDepth:Integer; BoardTextLines:TBoardTextLines;
  begin
    BoardToTextLines(BoardTextLines,FirstRow,LastRow,FirstDepth,LastDepth);
    for Depth:=FirstDepth to LastDepth do begin
        for Row:=FirstRow to LastRow do
            Writeln(BoardTextLines[Depth,Row]);
        if  Depth<LastDepth then
            Writeln(BOARD_3D_LAYER_SEPARATOR);
        end;
    //Readln;
  end;
{$ELSE}
  begin
  end;
{$ENDIF}

function  TrimBoard:Boolean;
var Col,Row,Depth,RowOffset1,RowOffset2,NewBoardWidth,NewBoardHeight,NewBoardDepth:Integer;
begin {removes redundant columns, rows, and depths from the board; returns 'True' if the board contained redundant columns, rows, or depths}
  PrepareBoardForPrettyPrinting(False);
  NewBoardWidth:=Game.BoardWidth; NewBoardHeight:=Game.BoardHeight; NewBoardDepth:=Game.BoardDepth;

  while (NewBoardWidth<>0) and IsEmptyCol(1) do begin
    for Depth:=1 to NewBoardDepth do
        for Row:=1 to NewBoardHeight do begin
            RowOffset1:=ColRowDepthToSquare(0,Row,Depth,Game.BoardWidth,Game.BoardHeight);
            for Col:=1 to Pred(NewBoardWidth) do Game.Board[RowOffset1+Col]:=Game.Board[RowOffset1+Succ(Col)];
            end;
    Dec(NewBoardWidth);
    end;
  while (NewBoardWidth<>0) and IsEmptyCol(NewBoardWidth) do Dec(NewBoardWidth);

  while (NewBoardHeight<>0) and IsEmptyRow(1) do begin
    for Depth:=1 to NewBoardDepth do
        for Row:=1 to Pred(NewBoardHeight) do begin
            RowOffset1:=ColRowDepthToSquare(0,     Row ,Depth,Game.BoardWidth,Game.BoardHeight); {row offset}
            RowOffset2:=ColRowDepthToSquare(0,Succ(Row),Depth,Game.BoardWidth,Game.BoardHeight); {next row offset}
            for Col:=1 to NewBoardWidth do Game.Board[RowOffset1+Col]:=Game.Board[RowOffset2+Col];
            end;
    Dec(NewBoardHeight);
    end;
  while (NewBoardHeight<>0) and IsEmptyRow(NewBoardHeight) do Dec(NewBoardHeight);

  while (NewBoardDepth<>0) and IsEmptyDepth(1) do begin
    for Depth:=1 to Pred(NewBoardDepth) do
        for Row:=1 to NewBoardHeight do begin
            RowOffset1:=ColRowDepthToSquare(0,Row,     Depth ,Game.BoardWidth,Game.BoardHeight); {row offset}
            RowOffset2:=ColRowDepthToSquare(0,Row,Succ(Depth),Game.BoardWidth,Game.BoardHeight); {next [layer,row] offset}
            for Col:=1 to NewBoardWidth do Game.Board[RowOffset1+Col]:=Game.Board[RowOffset2+Col];
            end;
    Dec(NewBoardDepth);
    end;
  while (NewBoardDepth<>0) and IsEmptyDepth(NewBoardDepth) do Dec(NewBoardDepth);

  Result:=(NewBoardWidth<>Game.BoardWidth) or (NewBoardHeight<>Game.BoardHeight) or (NewBoardDepth<>Game.BoardDepth);

  if Result then begin
     for Depth:=1 to NewBoardDepth do
         for Row:=1 to NewBoardHeight do begin
             RowOffset1:=ColRowDepthToSquare(0,Row,Depth,NewBoardWidth,  NewBoardHeight); {co-ordinates with new dimensions}
             RowOffset2:=ColRowDepthToSquare(0,Row,Depth,Game.BoardWidth,Game.BoardHeight); {co-ordinates with old dimensions}
             for Col:=1 to NewBoardWidth do Game.Board[RowOffset1+Col]:=Game.Board[RowOffset2+Col];
             end;
     InitializeBoard(NewBoardWidth,NewBoardHeight,NewBoardDepth,False);
     Game.ReverseMode:=False;
     InitializeGame;
     end;
end;

{-----------------------------------------------------------------------------}

{Game}

procedure InitializeGame;
var i,OriginalMaxBoxCount:Integer;
begin
  with Game do begin
    OriginalMaxBoxCount:=Generator.MaxBoxCount;
    if (not Generator.MaxBoxCountEnabled)
       then
       Generator.MaxBoxCount:=MAX_BOX_COUNT;

    InitializeBoard(BoardWidth,BoardHeight,BoardDepth,False);
    {ShowBoard; Readln;}

    BoxCount:=0; GoalCount:=0; PlayerPos:=0; FloorCount:=0;
    for i:=0 to BoardSize do begin
        if   (Board[i] and BOX)<>0 then
             if   BoxCount<Generator.MaxBoxCount then begin
                  Inc(BoxCount);
                  BoxPos[BoxCount]:=i;
                  end
             else Dec(Board[i],BOX);
        if   (Board[i] and GOAL)<>0 then
             if   GoalCount<Generator.MaxBoxCount then begin
                  Inc(GoalCount);
                  GoalPos[GoalCount]:=i;
                  end
             else Dec(Board[i],GOAL);
        if   (Board[i] and PLAYER)<>0 then
             if   PlayerPos=0 then
                  PlayerPos:=i
             else Dec(Board[i],PLAYER);
        Board[i]:=Board[i] and (FLOOR+PLAYER+BOX+GOAL+WALL+FLAG_FIXED_SQUARE); {reset old flags, if any}
        if (Board[i] and FLOOR)<>0 then Inc(FloorCount);
        if ((Board[i] and WALL)<>0) and (Board[i]<>WALL) and (Board[i]<>(WALL+FLAG_FIXED_SQUARE)) then
           Error('Internal error: "Initialize Game"',TEXT_APPLICATION_TITLE);
        end;
    if BoxCount=0 then begin
       BoxCount:=GoalCount;
       for i:=1 to BoxCount do begin
           BoxPos[i]:=GoalPos[i]; Inc(Board[BoxPos[i]],BOX);
           end;
       end;
    if GoalCount=0 then begin
       GoalCount:=BoxCount;
       for i:=1 to GoalCount do begin
           GoalPos[i]:=BoxPos[i]; Inc(Board[GoalPos[i]],GOAL);
           end;
       end;
    if BoxCount<>GoalCount then begin
       for i:=Succ(Min(BoxCount,GoalCount)) to BoxCount  do Dec(Board[BoxPos [i]],BOX);
       for i:=Succ(Min(BoxCount,GoalCount)) to GoalCount do Dec(Board[GoalPos[i]],GOAL);
       BoxCount:=Min(BoxCount,GoalCount); GoalCount:=BoxCount;
       end;

    if (Generator.Method=gmGA)
       and
       (not GA.Control.ForwardSearch)
       and
       GA.Control.GenerateConnectedGoals
       and
       ReverseMode
       then
       RemoveUnconnectedGoals;

    BoardHashValue:=CalculateBoardHashValue;
    PlayersReachableSquaresTimeStamp:=High(PlayersReachableSquaresTimeStamp);   {reset visited squares before calculating the player's reachable squares the next (first) time}
    Positions.CurrentPosition:=nil;

    Generator.MaxBoxCount:=OriginalMaxBoxCount;
    {ShowBoard; Readln;}
    end;
end;

procedure SaveVariationToHistory(Position__:PPosition);
var BoxNo,NewBoxCount,SquareNo,TruePositionDepth:Integer;
    NewBoxNo:array[0..MAX_BOX_COUNT] of Integer; p,q:PPosition;
begin
  if Position__<>nil then with Game do with History do begin
     if ReverseMode then begin {assign new box-numbers so they are sorted in order of appearance on the board}
        NewBoxCount:=0;
        for SquareNo:=1 to BoardSize do
            if (Board[SquareNo] and BOX)<>0 then
               for BoxNo:=1 to BoxCount do
                   if BoxPos[BoxNo]=SquareNo then begin
                      Inc(NewBoxCount);
                      NewBoxNo[BoxNo]:=NewBoxCount;
                      break;
                      end;
        end;

     if Positions.TailPositionCount<>0 then begin
        TruePositionDepth:=0; p:=Position__; {calculate the true depth for the position (the stored depth is relative to a tail position ancestor, if any)}
        while p^.Parent<>nil do begin
          Inc(TruePositionDepth); p:=p^.Parent;
          end;
        end
     else
        TruePositionDepth:=Position__^.Depth;

     Count:=0; Score:=0; Fitness:=0; TailCount:=0; p:=Position__;
     if TruePositionDepth<=MAX_HISTORY_MOVES then begin
        while (p<>nil) and (Count<MAX_HISTORY_MOVES) do begin
          q:=p^.Parent;
          if q<>nil then begin {'Parent'=nil: the root node is a dummy}
             Inc(Count);
             if   Game.ReverseMode then begin
                  Moves[Count].BoxNo    :=NewBoxNo[p^.Move.BoxNo];
                  Moves[Count].Direction:=OPPOSITE_DIRECTION[TDirection(Ord(p^.Move.Direction) and DIRECTION_BIT_MASK)];
                  end
             else with Moves[Succ(TruePositionDepth)-Count] do begin
                    BoxNo:=p^.Move.BoxNo;
                    Direction:=TDirection(Ord(p^.Move.Direction) and DIRECTION_BIT_MASK);
                    end;

             {calculate box-lines + box-changes; during the search the score may have been a different metric or heuristic score;}
             {this value isn't in use; further down, the score is overwritten with the original score found during the search;}
             if   ((Ord(p^.Move.Direction) and POSITION_TAG_TAIL)=0) or
                  (Positions.TailPositionCount<=0) then begin
                  if      (q^.Move.BoxNo<>p^.Move.BoxNo) then
                          Inc(Score,2) {box-lines and box-changes have the same weights; there is no room in the fitness-value for different weights}
                  else if (Ord(q^.Move.Direction) and DIRECTION_BIT_MASK)<>(Ord(p^.Move.Direction) and DIRECTION_BIT_MASK) then
                          Inc(Score);
                  end
             else Inc(TailCount); {count trivial pushes at the end of the game}
             end;
          p:=q;
          end;

        {the score registered in the position is the minimum number of}
        {box-lines + box-changes found during the search; keep the minimum value}
        {even if the current path to the position has a higher score}
        Score:=Position__^.Score;
        {the fitness value is based on the position depth; if trivial opening}
        {and closing pushes are ignored, then the depth is smaller can the}
        {number of pushes on the path}
        Fitness:=GAFitnessValue(GA.Control.FitnessFunction,Position__^.Depth,Score);
        end;
     end;
end;

{-----------------------------------------------------------------------------}

{Moves}

procedure DoMove(BoxNo__:Integer; Direction__:TDirection);
var FromPos,ToPos:Integer;
begin
  if BoxNo__<>0 then with Game do begin {only pushes are handled}
     Direction__:=TDirection(Ord(Direction__) and DIRECTION_BIT_MASK);
     FromPos:=BoxPos[BoxNo__];
     ToPos  :=FromPos+NeighbourSquareOffset[Direction__];
     Dec(Board[FromPos],BOX);
     Inc(Board[ToPos  ],BOX);
     BoxPos[BoxNo__]:=ToPos;
     Board[PlayerPos]:=Board[PlayerPos] and (not PLAYER);
     if   ReverseMode then
          PlayerPos:=ToPos+NeighbourSquareOffset[Direction__]
     else PlayerPos:=FromPos;
     Inc(Board[PlayerPos],PLAYER);
     BoardHashValue:=(BoardHashValue xor Game.HashSquareValues[FromPos])
                     xor
                     Game.HashSquareValues[ToPos];
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

procedure UndoMove(BoxNo__:Integer; Direction__:TDirection);
var FromPos,ToPos:Integer;
begin
  if BoxNo__<>0 then with Game do begin {only pushes are handled}
     Direction__:=TDirection(Ord(Direction__) and DIRECTION_BIT_MASK);
     FromPos:=BoxPos[BoxNo__];
     ToPos  :=FromPos-NeighbourSquareOffset[Direction__];
     Dec(Board[FromPos],BOX);
     Inc(Board[ToPos  ],BOX);
     BoxPos[BoxNo__]:=ToPos;
     Board[PlayerPos]:=Board[PlayerPos] and (not PLAYER);
     if   ReverseMode then
          PlayerPos:=FromPos
     else PlayerPos:=ToPos-NeighbourSquareOffset[Direction__];
     Inc(Board[PlayerPos],PLAYER);
     BoardHashValue:=(BoardHashValue xor Game.HashSquareValues[FromPos])
                     xor
                     Game.HashSquareValues[ToPos];
     end;
end;

{-----------------------------------------------------------------------------}

{File Reader}

procedure InitializeLevelReader(XSBNotation__:Boolean);
var i:Integer;
begin
  with LevelReader do begin
    CurrentTextLine:=''; {free the strings before the record is zero-filled}
    InputFileName:='';
    FillChar(LevelReader,SizeOf(LevelReader),0); Generator.Count:=0;
    CharToBoardSquareValue[CH_BOX        ]:=BOX+FLOOR   ; CharToBoardSquareValue[CH_BOX_ON_GOAL       ]:=BOX+GOAL+FLOOR;
    CharToBoardSquareValue[CH_BOX_XSB    ]:=BOX+FLOOR   ; CharToBoardSquareValue[CH_BOX_ON_GOAL_XSB   ]:=BOX+GOAL+FLOOR;
    CharToBoardSquareValue[CH_FLOOR      ]:=FLOOR;
    CharToBoardSquareValue[CH_FIXED_FLOOR]:=FLOOR+FLAG_FIXED_SQUARE; {fixed floor in a template}
    CharToBoardSquareValue[CH_GOAL       ]:=GOAL+FLOOR  ; CharToBoardSquareValue[CH_WALL              ]:=WALL;
    CharToBoardSquareValue[CH_PLAYER     ]:=PLAYER+FLOOR; CharToBoardSquareValue[CH_PLAYER_ON_GOAL    ]:=PLAYER+GOAL+FLOOR;
    CharToBoardSquareValue[CH_PLAYER_XSB ]:=PLAYER+FLOOR; CharToBoardSquareValue[CH_PLAYER_ON_GOAL_XSB]:=PLAYER+GOAL+FLOOR;

    CharToBoardSquareValue['_'           ]:=FLOOR; {extra characters for floor squares}
    CharToBoardSquareValue['-'           ]:=FLOOR;
    for i:=0 to Pred(Ord(SPACE)) do CharToBoardSquareValue[Chr(i)]:=FLOOR;

    for i:=0 to Pred(FLAG_FIXED_SQUARE) do
        CharToBoardSquareValue[Chr(i+FLAG_FIXED_SQUARE)]:=CharToBoardSquareValue[Chr(i)] or FLAG_FIXED_SQUARE; {fixed squares from a template}

    XSBNotation:=XSBNotation__; IsABoardTemplate:=False;
    end;
end;

function LoadFirstLevelFromFile(FileName__:String):Boolean;
var ErrorText:String;
begin {$I-}
  with LevelReader do begin
    InputFileName:=''; Count:=0; CurrentTextLine:=''; IsAGALogFile:=False;
    ErrorText:=''; Generator.Count:=0;
    Assign(InputFile,FileName__); Reset(InputFile);
    if IOResult=0 then begin
       InputFileName:=FileName__;
       Result:=LoadNextLevelFromFile(0,0,0);
       if not Result then ErrorText:='No levels found in file';
       end
    else ErrorText:=TEXT_FILE_OPEN_ERROR;
    Result:=ErrorText='';
    if not Result then Error(ErrorText+COLON+SPACE+FileName__,TEXT_APPLICATION_TITLE);
    end;
end; {$I+}

function LoadNextLevelFromFile(MinimumBoardWidth__,MinimumBoardHeight__,MinimumBoardDepth__:Integer):Boolean; {a simple version, parsing board and optional heading titles only}
var i,j,k,LeftCol,LineLength,BoardWidth,BoardHeight,BoardDepth,LayerBoardWidth,LayerBoardHeight:Integer; ErrorText:String;
    Board:TBoard3D;

  procedure ProcessCurrentTextLine;
  var i,j,k:Integer;

    function IsABoardLine(const TextLine__:String; var LeftCol__,Length__: Integer):Boolean;
    var i,RightCol:Integer; Ch:Char; HasAWall:Boolean;
    begin
      RightCol:=Length(TextLine__); HasAWall:=False;
      while (RightCol>0) and (TextLine__[RightCol]<=SPACE) do Dec(RightCol); {skip trailing spaces}
      LeftCol__:=1;
      while (LeftCol__<=RightCol) and (TextLine__[LeftCol__]=BOARD_TEXT_LINE_TEMPLATE_CHAR) do Inc(LeftCol); {skip leading template line tags, if any}
      Length__:=Succ(RightCol-LeftCol__);
      Result:=Length__>0; i:=LeftCol__;
      while Result and (i<=RightCol) do begin
        Ch:=TextLine__[i]; Inc(i);
        Result:=(Ord(Ch)<FLAG_FIXED_SQUARE) and (LevelReader.CharToBoardSquareValue[Ch]<>0);
        HasAWall:=HasAWall or (Ch=CH_WALL);
        end;
      Result:=Result and HasAWall;
    end;

    procedure TextLineToBoard(const TextLine__:String; LeftCol__,Length__:Integer);
    var Col:Integer;
    begin {precondition: 'TextLine__' has been validated}
      if LayerBoardHeight=0 then {'True': first row in 3D board layer}
         Inc(BoardDepth);
      Inc(LayerBoardHeight);
      LayerBoardWidth:=Max(LayerBoardWidth,Length__);
      BoardWidth     :=Max(BoardWidth     ,LayerBoardWidth);
      BoardHeight    :=Max(BoardHeight    ,LayerBoardHeight);
      for Col:=LeftCol__ to Pred(LeftCol__)+Length__ do
          Board[BoardDepth, LayerBoardHeight,Succ(Col-LeftCol)]:=LevelReader.CharToBoardSquareValue[TextLine__[Col]];
    end;

  begin {ProcessCurrentTextLine}
    with LevelReader do begin
                 if   IsABoardLine(CurrentTextLine,LeftCol,LineLength) then
                      if   LayerBoardHeight<MAX_BOARD_HEIGHT then
                           if LineLength>MAX_BOARD_WIDTH then
                              ErrorText:=TEXT_BOARD_LINE_TOO_LONG
                           else begin
                              TextLineToBoard(CurrentTextLine,LeftCol,LineLength);
                              CurrentTextLine:='';
                              end
                      else ErrorText:=TEXT_BOARD_HAS_TOO_MANY_ROWS
                 else if CurrentTextLine=BOARD_3D_LAYER_SEPARATOR then begin
                         LayerBoardHeight:=0;
                         LayerBoardWidth:=0;
                         if BoardDepth>=MAX_BOARD_DEPTH then
                            ErrorText:=TEXT_BOARD_HAS_TOO_MANY_3D_LAYERS;
                         CurrentTextLine:='';
                         end
                 else if (BoardHeight>=MIN_BOARD_WIDTH) and (BoardDepth>=MIN_BOARD_DEPTH) then begin {done}
                         Result:=MakeLevelFrom3DimensionalBoard(BoardWidth,BoardHeight,BoardDepth,Max(BoardWidth,MinimumBoardWidth__),Max(BoardHeight,MinimumBoardHeight__),Max(BoardDepth,MinimumBoardDepth__),Board);
                         end
                      else begin
                         if BoardHeight<>0 then begin
                            BoardHeight:=0; BoardWidth:=0; BoardDepth:=0;
                            for k:=0 to MAX_BOARD_DEPTH do
                                for i:=0 to MAX_BOARD_WIDTH do
                                    for j:=0 to MAX_BOARD_HEIGHT do Board[k,i,j]:=FLOOR;
                            end;
                         if CurrentTextLine<>'' then
                            if   (Count=0) and
                                 (CurrentTextLine[1]=LEFT_BRACKET) and
                                 (System.Pos(GA_TEXT_APPLICATION_TITLE_LONG,CurrentTextLine)=2) then
                                 IsAGALogFile:=True
                            else Game.Title:=CurrentTextLine; {last non-blank line before the board is the title}
                         CurrentTextLine:='';
                         end;
      end;
  end;

begin {LoadNextLevelFromFile} {$I-}
  with LevelReader do begin
    Result:=InputFileName<>'';
    ErrorText:='';
    if Result then begin
       Result:=False; Game.Title:='';
       Game.History.Count:=0; Game.History.Score:=0; Game.History.Fitness:=-1;
       BoardWidth:=0; BoardHeight:=0; BoardDepth:=0; LayerBoardWidth:=0; LayerBoardHeight:=0;
       for k:=0 to MAX_BOARD_DEPTH do
           for i:=0 to MAX_BOARD_WIDTH+1 do
               for j:=0 to MAX_BOARD_HEIGHT+1 do Board[k,i,j]:=FLOOR;

       while  (not Result) and                                                  {not done}
              (ErrorText='') and                                                {no errors}
              (not ((Eof(InputFile)) and (CurrentTextLine=''))) do begin        {not eof}
         if   CurrentTextLine='' then                                           {a line may be left over from one call to the next}
              Readln(InputFile,CurrentTextLine);
         if   IOResult=0 then                                                   {did 'Readln' succeed?}
              ProcessCurrentTextLine
         else ErrorText:=TEXT_FILE_READ_ERROR;
         end;

       if (not Result) and (ErrorText='') and (BoardHeight>=MIN_BOARD_HEIGHT) and (BoardDepth>=MIN_BOARD_DEPTH) then begin
          Result:=MakeLevelFrom3DimensionalBoard(BoardWidth,BoardHeight,BoardDepth,Max(BoardWidth,MinimumBoardWidth__),Max(BoardHeight,MinimumBoardHeight__),Max(BoardDepth,MinimumBoardDepth__),Board);
          end;

       while (Game.Title<>'') and
             ((Game.Title[1]=SEMICOLON) or (Ord(Game.Title[1])<=Ord(SPACE))) do
             Delete(Game.Title,1,1);
       end;

    if ErrorText<>'' then Result:=Error(ErrorText+COLON+SPACE+InputFileName,'Load next level from file');
    if (not Result) and (InputFileName<>'')  then begin
       InputFileName:=''; CloseFile(InputFile); {clean up, i.e., close the file}
       end;
    end;
end; {$I+}

{-----------------------------------------------------------------------------}

{File Writer}

function AppendLevelToFile(const FileName__,LevelName__:String; PrettyPrinting__,GuardAgainstTrimming__,PrintPositionsPerDepth__:Boolean):Boolean;
var F:Text;
begin {$I-}
  AssignFile(F,FileName__);
  Append(F);
  Result:=IOResult=0;
  if Result then begin
     Result:=WriteLevelToFile(F,LevelName__,PrettyPrinting__,GuardAgainstTrimming__,PrintPositionsPerDepth__);
     CloseFile(F);
     Result:=(IOResult=0) and Result;
     end
  else {this is probably the first time; try to save the level to a new file}
    Result:=SaveLevelToFile(FileName__,LevelName__,PrettyPrinting__,GuardAgainstTrimming__,PrintPositionsPerDepth__);
end; {$I+}

function FirstMoveToText(var MoveNo__:Integer; var Text__:String):Boolean;
begin
  MoveNo__:=0;
  Result:=NextMoveToText(MoveNo__,Text__);
end;

function NextMoveToText(var MoveNo__:Integer; var Text__:String):Boolean;
var i,PlayerPushSquare,MoveCount:Integer; Moves:TMoves; OriginalReverseMode:Boolean;
begin
  Inc(MoveNo__); Text__:='';
  Result:=MoveNo__<=Game.History.Count;
  if Result then with Game.History.Moves[MoveNo__] do begin
     PlayerPushSquare:=Game.BoxPos[BoxNo]-Game.NeighbourSquareOffset[Direction];
     if (BoxNo<>0) and
        CalculatePlayerPath(Game.PlayerPos,PlayerPushSquare,True,MoveCount,Moves) then begin
        for i:=1 to MoveCount do Text__:=Text__+DIRECTION_TO_CHAR[Moves[i].Direction];
        Text__:=Text__+UpCase(DIRECTION_TO_CHAR[Direction]);
        OriginalReverseMode:=Game.ReverseMode; Game.ReverseMode:=False;
        DoMove(BoxNo,Direction);
        Game.ReverseMode:=OriginalReverseMode;
        end
     else Result:=False;
     end;
end;

function SaveLevelToFile(const FileName__,LevelName__:String; PrettyPrinting__,GuardAgainstTrimming__,PrintPositionsPerDepth__:Boolean):Boolean;
var F:Text;
begin {$I-}
  AssignFile(F,FileName__);
  Rewrite(F);
  Result:=WriteLevelToFile(F,LevelName__,PrettyPrinting__,GuardAgainstTrimming__,PrintPositionsPerDepth__);
  CloseFile(F);
  Result:=(IOResult=0) and Result;
end; {$I+}

function WriteLevelToFile({const} var F:Text; const LevelName__:String; PrettyPrinting__,GuardAgainstTrimming__,PrintPositionsPerDepth__:Boolean):Boolean;
var {$IFDEF VERBOSE_OUTPUT}
      i,j,
    {$ENDIF}
    MoveNo,Row,Depth,{oFloorCount,}FirstRow,LastRow,FirstDepth,LastDepth:Integer;
    s,s1,s2:String; BoardTextLines:TBoardTextLines;
    {oBoard:TBoard;}
begin {$I-}
  if LevelName__<>'' then begin
     Writeln(F,LevelName__);
     Writeln(F);
     end;

  {pretty-printing is disabled because it's considered too difficult for a human
   reader of the output file to visialize and verify that the pretty-printed
   version of the 3D puzzle is a correct version}
{
  if PrettyPrinting__ then with Game do begin
     oBoard:=Board; oFloorCount:=FloorCount;  // save current board
     PrepareBoardForPrettyPrinting(GuardAgainstTrimming__);
     BoardToTextLines(BoardTextLines,FirstRow,LastRow,FirstDepth,LastDepth);
     Game.Board:=oBoard; Game.FloorCount:=oFloorCount; // restore board
     end
  else
}  
     BoardToTextLines(BoardTextLines,FirstRow,LastRow,FirstDepth,LastDepth);

  for Depth:=FirstDepth to LastDepth do begin
      for Row:=FirstRow to LastRow do begin
          if LevelReader.IsABoardTemplate then Write(F,BOARD_TEXT_LINE_TEMPLATE_CHAR);
          Writeln(F,BoardTextLines[Depth,Row]);
          end;
      if Depth<LastDepth then
         Writeln(F,BOARD_3D_LAYER_SEPARATOR);
      end;
  if LevelName__<>'' then Writeln(F);

  {$IFDEF VERBOSE_OUTPUT}
    if LevelName__<>'' then Writeln(F,'Title: ',LevelName__);
  {$ENDIF}

  if Game.History.Count>0 then with Game.History do begin
     Writeln(F,'Minimum pushes: '       ,Count);
     s:=GA_TEXT_FITNESS_FUNCTION_TYPES[ffScore];
     Writeln(F,s,COLON,SPACE,Score);
     Writeln(F,'Exhaustive search: '    ,TEXT_NO_YES[Positions.TotalDroppedCount=0]);
     {$IFDEF VERBOSE_OUTPUT}
       Writeln(F,'Seen positions: '     ,Positions.TotalCount);
       Writeln(F,'Duplicate positions: ' ,Positions.TotalRepetitionCount);
       if (Generator.RandomSeed<>0) and (Generator.Method<>gmGA) then
          Writeln(F,'Random seed: '     ,Generator.RandomSeed);

       if PrintPositionsPerDepth__ then begin
          Writeln(F);
          Writeln(F,'Positions per search depth:');
          for i:=1 to Min(Game.History.Count,High(Positions.PositionsPerDepthCount)) do begin
              j:=Positions.PositionsPerDepthCount[i];
              Writeln(F,i:5,j:12);
              end;
          end;
     {$ENDIF}
     Writeln(F); Writeln(F,'Solution/Pushes'); s:='';

     if FirstMoveToText(MoveNo,s1) then
        repeat s:=s+s1;
               if Length(s)>=SOLUTION_LINE_LENGTH then begin
                  s2:=Copy(s,1,SOLUTION_LINE_LENGTH);
                  Writeln(F,s2);
                  Delete(s,1,SOLUTION_LINE_LENGTH);
                  end;
        until  not NextMoveToText(MoveNo,s1);

     if s<>'' then Writeln(F,s);

     end;

  if LevelName__<>'' then begin
     Writeln(F); Writeln(F);
     end;

  Result:=IOResult=0;
end; {$I+}

{-----------------------------------------------------------------------------}

{Open Positions}

procedure ClearOpenPositions;
begin
  with Positions do begin
    Inc(DroppedCount,OpenPositions.Count);
    FillChar(OpenPositions,SizeOf(OpenPositions),0);
    end;
end;

function DequeueOpenPosition(var Position__:PPosition):Boolean;
begin
  with Positions.OpenPositions do begin
    Position__:=Head;
    //Result:=(Position__<>nil) and (MainForm.Generator.ThreadState=ptsProcessLevel) and (CalculateElapsedTimeMS(MainForm.Generator.StartTimeMS,GetTimeMS)<20*ONE_THOUSAND);
    Result:=(Position__<>nil);
    if Result then begin
       Head:=Head^.Next;
       if Head=nil then Tail:=nil;
       Position__^.Next:=nil; {after removal from the open-queue, the 'Next' field is reused for storing the current path in the search}
       Dec(Count);
       end;
    end;
end;

procedure EnqueueOpenPosition(Position__:PPosition);
begin
  with Positions.OpenPositions do begin
    if   Tail<>nil then
         Tail^.Next:=Position__
    else Head:=Position__;
    Tail:=Position__;
    Tail^.Next:=nil;
    Inc(Count);
    end;
end;

procedure RemovePositionFromOpenPositions(Position__:PPosition);
begin
  SetPosition(nil);
  while Position__<>nil do begin  {the open queue uses 'next' to link members: reset the links}
    Position__^.Next:=nil; Position__:=Position__^.Parent;
    end;
end;

{-----------------------------------------------------------------------------}

{Positions (Transposition Table)}

procedure ClearPositions;
var i:Integer;
begin
  with Positions do begin
    if TimeStamp=High(TimeStamp) then begin
       for i:=Low(PositionVectors) to High(PositionVectors) do
           if PositionVectors[i]<>nil then
              FillChar(PositionVectors[i]^,VectorCapacity*SizeOf(TPosition),0);
       TimeStamp:={0}Pred(High(TimeStamp)); {'Pred(High(TimeStamp))': always reset memory; the nodes have no timestamps}
       end;
    Inc(TimeStamp);
    ClearOpenPositions;
    Count:=0; DroppedCount:=0; RepetitionCount:=0; TailPositionCount:=0;
    BestPosition:=nil; CurrentPosition:=nil;
    FillChar(PositionsPerDepthCount,SizeOf(PositionsPerDepthCount),0);
    end;
end;

procedure FinalizePositions;
var i:Integer;
begin
  with Positions do begin
    TimeStamp:=Pred(High(TimeStamp)); {so 'ClearPositions' doesn't spend time on zero-filling the memory allocated for the positions}
    ClearPositions;
    for i:=Low(PositionVectors) to High(PositionVectors) do
        if PositionVectors[i]<>nil then begin
           FreeMem(PositionVectors[i],VectorCapacity*SizeOf(TPosition));
           PositionVectors[i]:=nil;
           end;
    Capacity:=0; VectorCapacity:=0;
    end;
end;

function GetFirstPosition(HashValue__:THashValue; var Position__:PPosition):Boolean;
begin {searches for a box constellation, ignoring player position}
  Result:=LookupPosition(HashValue__,0,Position__);
end;

function GetNextPosition(var Position__:PPosition):Boolean;
begin {searches for a box constellation, ignoring player position}
  Result:=False;
  with Positions.LookupState do
    while (not Result) and                                                  {false: found}
          (CollisionCount<MAX_HASH_COLLISION_COUNT) and                     {false: not found, no free slot}
          {(Position__^.TimeStamp=Positions.TimeStamp)} (Position__^.PlayerPos<>0) do begin {false: not found, return free slot}
      Inc(CollisionCount);
      Inc(ItemIndex);                                                       {try next slot}
      if   ItemIndex>=Positions.VectorCapacity then begin
           VectorIndex:=Succ(VectorIndex) and (MEMORY_BLOCKS_COUNT-1);      {try next bank}
           ItemIndex  :=0;
           end;
      Position__:=Addr(PPositionVector(Positions.PositionVectors[VectorIndex])^[ItemIndex]);
      Result    :={(Position__^.TimeStamp=Positions.TimeStamp)} (Position__^.PlayerPos<>0) and {has this slot been used in the current run?}
                  (Position__^.HashValue=HashValue);                        {matching hash-value?}
      end;
end;

function InitializePositions(MemoryByteSize__:Integer):Boolean;
var i,j:Integer;
begin
  {$IFDEF PLUGIN}
    try
  {$ENDIF}
      with Positions do begin
        for i:=Low(PositionVectors) to High(PositionVectors) do {free allocated memory, if any}
            if Assigned(PositionVectors[i]) then FreeMem(PositionVectors[i] {,VectorCapacity*SizeOf(TPosition)} );
        FillChar(Positions,SizeOf(Positions),0);
        VectorCapacity:=(MemoryByteSize__ div MEMORY_BLOCKS_COUNT) div SizeOf(TPosition);
        Result:=VectorCapacity>0;
        if Result then
           for i:=Low(PositionVectors) to High(PositionVectors) do begin {multiple chunks instead of one large block}
               GetMem(PositionVectors[i],VectorCapacity*SizeOf(TPosition));
               if   PositionVectors[i]<>nil then Inc(Capacity,VectorCapacity)
               else Result:=False;
               end;
        MemoryByteSize:=Capacity*SizeOf(TPosition);

        TimeStamp:=High(TimeStamp); ClearPositions;
        end;
  {$IFDEF PLUGIN}
    except on E:Exception do Result:=Error(E.Message,'');
    end;
  {$ENDIF}

  if Game.HashSquareValues[MAX_BOARD_SIZE]<=MaxInt then begin {'True': the square values for the Zobrist hash key calculations haven't been initialized yet}
     InitializeRandomState(0);
     for i:=0 to 255 do if Random(MaxInt)=0 then; {warm up the random number generator}

     for i:=0 to MAX_BOARD_SIZE do {initialize the hash square values}
         repeat
           Game.HashSquareValues[i]:=
             Abs(Abs((THashValue(Random(MaxInt)) shl 48))+
                 Abs((THashValue(Random(MaxInt)) shl 32))+
                 Abs((THashValue(Random(MaxInt)) shl 16))+
                 Abs((THashValue(Random(MaxInt)) shl 8 ))+
                 Abs((THashValue(Random(MaxInt)))));
           for j:=0 to Pred(i) do
               if Game.HashSquareValues[j]=Game.HashSquareValues[i] then
                  Game.HashSquareValues[i]:=0;
         until Game.HashSquareValues[i]>MaxInt;
     end;
end;

function LookupPosition(HashValue__:THashValue; PlayerPos__:Integer;
                        var Position__:PPosition):Boolean;
begin
  Result          :=False;
  with Positions.LookupState do begin
    HashValue     :=Abs(HashValue__);
    CollisionCount:=0; {caution: there is only one set of static information controlling 'Lookup' and 'GetFirst'/'GetNext'}
    VectorIndex   :=HashValue and (MEMORY_BLOCKS_COUNT-1);                  {multiple chunks (vectors)}
    ItemIndex     :=HashValue mod Positions.VectorCapacity;                 {index in chunk}
    Position__    :=PPosition(Addr(PPositionVector(Positions.PositionVectors[VectorIndex])^[ItemIndex]));

    repeat
      Inc(CollisionCount);
      if {Position__^.TimeStamp=Positions.TimeStamp} Position__^.PlayerPos<>0 then {has this slot been used in current run?}
         if ( Position__^.HashValue=HashValue) and
            ((Position__^.PlayerPos=PlayerPos__) or (PlayerPos__=0)) then   {'0': don't care}
            Result:=True                                                    {found}
         else begin
            Inc(ItemIndex);
            if ItemIndex<Positions.VectorCapacity then Inc(Position__)      {try next slot}
            else begin
               VectorIndex:=Succ(VectorIndex) and (MEMORY_BLOCKS_COUNT-1);  {try next bank}
               ItemIndex  :=0;
               Position__ :=PPosition(Addr(PPositionVector(Positions.PositionVectors[VectorIndex])^[ItemIndex]));
               end
            end;
    until Result or                                                         {found}
         {(Position__^.TimeStamp<>Positions.TimeStamp)} (Position__^.PlayerPos=0) or {'0': not found, return free slot}
         (CollisionCount>=MAX_HASH_COLLISION_COUNT);                        {not found, no free slot}
    end;
end;

function MakePosition(HashValue__:THashValue;
                      PlayerPos__,Depth__,Score__,BoxNo__:Integer;
                      Direction__:TDirection; Parent__:PPosition;
                      var Position__:PPosition):Boolean;
begin
  Position__:=nil;
  if      LookupPosition(HashValue__,PlayerPos__,Position__) then {'True': the position already exists in the transposition table}
          with Position__^ do begin
            Inc(Positions.RepetitionCount);

            Result:=False;
            if Depth__<Depth then begin
               if Depth=High(Depth) then begin
                  {this is the first time the position is encountered during}
                  {the normal breadth-first search (as opposed to the initial}
                  {tail-positions search)}
                  EnqueueOpenPosition(Position__);
                  if Depth__<=High(Positions.PositionsPerDepthCount) then
                     {the initial tail-position search cleared}
                     {the depth-statistics, hence, update the stats here}
                     Inc(Positions.PositionsPerDepthCount[Depth__]);
                  end;
               Result:=True;
               Depth:=Depth__;
               end;
            if Score__<Score then begin
               Result:=True;
               Score:=Score__;
               end;

            if Result and {'True': this is a new better path to the position}
               ((Ord(Move.Direction) and POSITION_TAG_CLOSED)=0) then begin
               {the position hasn't been expanded yet, hence, it's ok to}
               {update the path to it because no successor-positions depend on}
               {the current path to this position;}

               {note that the program could use the weaker contraint}
               {"the position hasn't any successors" but in that case, the}
               {push-optimality of the reported solutions cannot be guaranteed}
               {anymore; switching constraint requires text-changes throughout}
               {the program, e.g., "Estimated pushes" versus "Minimum pushes";}
               Move.BoxNo:=BoxNo__;
               Move.Direction:=TDirection(Ord(Direction__) or (Ord(Move.Direction) and POSITION_TAG_BIT_MASK));
               Parent:=Parent__;
               PlayerPos:=PlayerPos__;
               end
            else begin
               {'Depth' and 'Score' may be out of sync, coming from different}
               {paths to this position;}
               {scores (box-lines + box-changes) are generally inaccurate in}
               {this program, but updating the score as it's done here may be a}
               {tiny bit better than insisting on values synchronized with the}
               {stored path to the position}
               end;
            end
  else if {Position__^.TimeStamp=Positions.TimeStamp} Position__^.PlayerPos<>0 then begin {'<>0': no room for a new position, i.e., the memory is full}
          Inc(Positions.DroppedCount);
          ClearOpenPositions; {stop when positions are lost}
          Result:=False; Position__:=nil; {'nil' signals that memory was filled; 'Position__' points to a slot in the hash-table in all other circumstances}
          end
       else with Position__^ do begin {save new position}
          Inc(Positions.Count);
          if Depth__<=High(Positions.PositionsPerDepthCount) then
             Inc(Positions.PositionsPerDepthCount[Depth__]);
          Depth:=Depth__;
          HashValue:=Abs(HashValue__);
          Move.BoxNo:=BoxNo__;
          Move.Direction:=Direction__;
          Parent:=Parent__;
          PlayerPos:=PlayerPos__;
          Score:=Score__;
          {TimeStamp:=Positions.TimeStamp;}
          EnqueueOpenPosition(Position__);
          Result:=True;
          end;
end;

procedure SetPosition(Position__:PPosition);
var p,Next:PPosition;
begin {do/undo moves so the board matches the position at the tree-node 'Position__'}
  with Positions do begin
    {find path back to common ancestor of new position and current position}
    p:=Position__; Next:=nil;
    while (p<>nil) and
          (p^.Parent<>nil) and
          (p^.Next=nil)  and {after a position is removed from the breadth-first queue, 'Next' is reused as forward chain for the current path}
          (p<>CurrentPosition) do begin
      p^.Next:=Next; {'Next' is used as forward chain for the current path}
      Next:=p;
      p:=p^.Parent;
      end;

    {undo the old moves that aren't on the common path, if any}
    while (CurrentPosition<>nil) and
          (CurrentPosition<>p  ) do with CurrentPosition^ do begin
      UndoMove(Move.BoxNo,Move.Direction);
      Next:=nil; {'nil': this position isn't on the current path anymore}
      CurrentPosition:=Parent;
      end;

    {perform new moves, starting with the common ancestor's successor}
    if Next<>nil then begin
       if     Next^.Parent<>nil then
              Next^.Parent^.Next:=Next; {ensure that the forward chain is updated for the common ancestor, if any}
       repeat DoMove(Next^.Move.BoxNo,Next^.Move.Direction);
              Next:=Next^.Next;
       until  Next=nil;
       end
    else if Position__<>nil then MovePlayer(Position__^.PlayerPos);

    CurrentPosition:=Position__; {save new current position}
    end;
end;

procedure ShowPath(Position__:PPosition);
var oCurrentPosition:PPosition;
begin {precondition: 'Position__' isn't on the OPEN-queue}
  with Positions do begin
    oCurrentPosition:=CurrentPosition;

    while (Position__<>nil) do with Position__^ do begin
      SetPosition(Position__);
      ShowBoard;
      Write(Depth,'/',Score,'  ',HashValue);
      Readln;
      Position__:=Parent;
      end;

    SetPosition(oCurrentPosition);
    end;
end;

{-----------------------------------------------------------------------------}

{Pathfinding}
(*
function CalculatePlayerPath(const FromPos__,ToPos__:Integer; MakeMoves__:Boolean;
                             var MoveCount__:Integer; var Moves__:TMoves):Boolean;
const
  INFINITY=MaxInt div 2;
type
  TNode = record Distance,Parent:Integer; Direction:TDirection; end;
  PNode = ^TNode;
var
  i,p:Integer; ToPosNode:PNode;
  Positions:array[0..MAX_BOARD_SIZE] of TNode;

  procedure TrySquare(Square__:Integer);
  var Count:Integer; Dir:TDirection; p:^Integer; q:PNode;
      Neighbours:array[Ord(Low(TDirection))..Ord(High(TDirection))-Ord(Low(TDirection))+2] of Integer; {'+2': one extra element, so a pointer beyond the last used element is legal}
  begin
    Count:=Succ(Positions[Square__].Distance);        {'Count': the distance to neighbour-squares if path goes through 'Square'}
    if Count<MoveCount__ then begin                   {if false, paths to neighbour-squares via 'Square' can never be on a best path}
       p:=Addr(Neighbours);
       for Dir:=Low(Dir) to High(Dir) do begin
           p^ :=Square__+Game.NeighbourSquareOffset[Dir];
           q  :=Addr(Positions[p^]);                  {use a pointer so the address of 'Positions[p^]' is calculated only once}
           if Count<q^.Distance then begin            {new best path to the neighbour 'p^'}
              q^.Distance :=Count;
              q^.Parent   :=Square__;
              q^.Direction:=Dir;
              Inc(p);                                 {next move in next slot}
              if q=ToPosNode then MoveCount__:=Count; {new best result}
              end;
           end;
       while p<>Addr(Neighbours) do begin             {visit updated neighbours}
         Dec(p); TrySquare(p^);                       {(updating all neighbours in a breadth-first manner before recursion reduces futile calculations)}
         end;
       end;
  end; {TrySquare}

begin {CalculatePlayerPath}
  for i:=0 to Game.BoardSize do with Positions[i] do
      if   (Game.Board[i] and (WALL+BOX))=0 then
           Distance:= INFINITY                        {open squares}
      else Distance:=-INFINITY;                       {filled squares, i.e., walls and boxes}

  MoveCount__:=INFINITY;
  ToPosNode  :=Addr(Positions[ToPos__]);              {for speed, use a pointer to identify the goal node}

  if (Positions[FromPos__].Distance<>-INFINITY) then begin
     Positions[FromPos__].Distance:=0;
     MoveCount__:=ToPosNode^.Distance;
     TrySquare(FromPos__);
     end;

  Result:=Abs(MoveCount__)<>INFINITY;
  if Result and MakeMoves__ then begin
     p:=ToPos__;
     while p<>FromPos__ do begin
       Moves__[Positions[p].Distance].BoxNo:=0;
       Moves__[Positions[p].Distance].Direction:=Positions[p].Direction;
       p:=Positions[p].Parent;
       end;
     end;
end;
*)

function  CalculatePlayerPath(FromSquare__,ToSquare__:Integer; MakeMoves__:Boolean;
                              var MoveCount__:Integer; var Moves__:TMoves):Boolean;
const
  INFINITY=MaxInt div 2;
var
  Square,SuccessorDistance:Integer; QueueBottom,QueueTop:^Integer; Direction:TDirection;
  Distances,ParentSquares,QueueItems:TBoardInteger; Directions:array[0..MAX_BOARD_SIZE] of TDirection;
begin
  for Square:=0 to Game.BoardSize do
      if   (Game.Board[Square] and (WALL+BOX))=0 then
           Distances[Square]:= INFINITY  {open squares}
      else Distances[Square]:=-INFINITY; {occupied squares, i.e., walls and boxes; a path length is always >= 0, so an occupied square with the value '-INFINITY' can never be on a best path}

  QueueBottom:=Addr(QueueItems[Low(QueueItems)]); QueueTop:=QueueBottom; {clear the queue for the breadth first search (BFS)}

  if Distances[FromSquare__]<>-INFINITY then begin {'True': 'from-position' is either an empty floor or the player's current position}
     Inc(QueueTop); QueueTop^:=FromSquare__; {put 'from-position' on the queue as the root position for the breadth-first search}
     Distances[FromSquare__]:=0; {the distance to the start position is 0}
     end;

  {breadth first search (BFS), starting from 'from-position'; a FIFO (first-in-first-out) queue ensures that the reachable squares are visited in breadth-first order}
  while QueueBottom<>QueueTop do begin {while there are more un-expanded squares}
    Inc(QueueBottom); Square:=QueueBottom^; {advance to the next square on the queue}
    SuccessorDistance:=Succ(Distances[Square]); {if the best path to a neighbor square goes through 'Square' then the distance to the neighbor will be one higher than the distance to 'Square'}
    for Direction:=High(Direction) downto (Low(Direction)) do begin {try walking in each direction (the reverse order happens to produce better looking paths according to the writer's taste)}
        Inc(QueueTop); {prepare to put the neighbor square on the queue}
        QueueTop^:=Square+Game.NeighbourSquareOffset[Direction]; {tentatively put the neighbor square on the queue}
        if   SuccessorDistance<Distances[QueueTop^] then begin {'True': the neighbor square isn't occupied by a wall or a box, and the best path to the neighbor square is through the current square}
             Distances[QueueTop^]:=SuccessorDistance; {save the best-path distance to the neighbor square}
             ParentSquares[QueueTop^]:=Square; {save the parent square for the path construction in case the best path goes through this neighbor square}
             Directions[QueueTop^]:=Direction; {save the direction for this move}
             if QueueTop^=ToSquare__ then {'True': this is the target square; stop the search}
                QueueTop:=QueueBottom; {terminate the 'while' loop; the breadth-first search ensures that the first found path to the target square is optimal}
             end
        else Dec(QueueTop); {decrease the queue top pointer, thereby removing the neighbor square that tentatively was put on the queue}
        end;
    end;

  MoveCount__:=Abs(Distances[ToSquare__]); {return the number of moves on the best path, if any}
  Result:=MoveCount__<>INFINITY; {'True': there is a path from the start position to the target square; a legal 0-move path is returned if the start position and the target position are identical}

  if Result and MakeMoves__ then
     while ToSquare__<>FromSquare__ do begin {construct the path backward from the target position towards the start position}
       Moves__[Distances[ToSquare__]].BoxNo:=0;
       Moves__[Distances[ToSquare__]].Direction:=Directions[ToSquare__];
       ToSquare__:=ParentSquares[ToSquare__]; {backtrack to the previous square on the best path}
       end;
end;

function CalculatePlayersReachableSquares(var MinPlayerPos__:Integer):Integer;
var Square,NeighbourSquare:Integer; Dir:TDirection;
    StackTop:^Integer; Stack:TBoardInteger;
begin
  Result:=0; MinPlayerPos__:=Game.PlayerPos;

  {inline 'ClearPlayersReachableSquares' for speed}
  if Game.PlayersReachableSquaresTimeStamp=High(Game.PlayersReachableSquaresTimeStamp) then begin
     Game.PlayersReachableSquaresTimeStamp:=0; {a complete reset is only necessary when the timestamp overflows}
     FillChar(Game.PlayersReachableSquares,SizeOf(Game.PlayersReachableSquares[0])*Succ(Game.BoardSize),0);
     end;
  Inc(Game.PlayersReachableSquaresTimeStamp);

  if Game.PlayerPos<>0 then with Game do begin
     Result:=1; StackTop:=Addr(Stack[1]); StackTop^:=PlayerPos;
     PlayersReachableSquares[PlayerPos]:=PlayersReachableSquaresTimeStamp;
     while StackTop<>Addr(Stack[0]) do begin
       {the program spends most of its time in this loop;}
       {using a stack makes the program run at least 10% faster than}
       {a version using a more elegant recursive implementation of the loop}
       Square:=StackTop^; Dec(StackTop); {get next square from the stack}
       for Dir:=Low(Dir) to High(Dir) do begin {examine neighbours to this square}
           NeighbourSquare:=Square+NeighbourSquareOffset[Dir];
           if (Board[NeighbourSquare] and (WALL+BOX)=0) and {check for walls, boxes, and visited squares}
              (PlayersReachableSquares[NeighbourSquare]<>PlayersReachableSquaresTimeStamp) then begin
              Inc(Result); Inc(StackTop); StackTop^:=NeighbourSquare;
              PlayersReachableSquares[NeighbourSquare]:=PlayersReachableSquaresTimeStamp;
              if NeighbourSquare<MinPlayerPos__ then MinPlayerPos__:=NeighbourSquare;
              end;
           end;
       end;
     end;
end;

procedure ShowPlayersReachableSquares; {for test only}
{$IFDEF CONSOLE_APPLICATION}
  var i,Col,Row,Depth,MinPlayerPos:Integer;
  begin
    Write('Player''s reachable squares: ');
    if CalculatePlayersReachableSquares(MinPlayerPos)>0 then begin
       for i:=0 to Game.BoardSize do
           if Game.PlayersReachableSquares[i]=Game.PlayersReachableSquaresTimeStamp then begin
              SquareToColRowDepth(i,Col,Row,Depth);
              Write('[',Row,',',Col,Depth,']');
              end;
       SquareToColRowDepth(MinPlayerPos,Col,Row,Depth);
       Write(' Min: [',Row,',',Col,Depth,']');
       end;
    Writeln;
  end;
{$ELSE}
  begin
  end;
{$ENDIF}

{-----------------------------------------------------------------------------}

{ Board Generator}

function GenerateNextBoard_Enumerate:Boolean;
var i,j:Integer;
begin
  with Generator do begin
    if ItemPos[PlayerIndex]<>0 then
       Dec(Board[ItemPos[PlayerIndex]],PLAYER); {remove the player that was put on the board in the previous iteration}

    Game.PlayerPos:=0; {'0': board not complete (yet)}

    if ItemIndex>=PlayerIndex then ItemIndex:=Pred(PlayerIndex);

    while (ItemIndex>0) and (ItemIndex<PlayerIndex) do begin {leave it to 'RunGenerator' to fill in the player position}
      i:=ItemPos[ItemIndex];
      if i>0 then begin
         Board[i]:=FLOOR; ItemPos[ItemIndex]:=0;
         end
      else if (ItemIndex<>Succ(WallCount)) and
              (ItemIndex<>PlayerIndex) then
              i:=ItemPos[Pred(ItemIndex)];

      repeat
        Inc(i);
      until (i>Game.BoardSize)
            or
            (((Board[i] and (WALL+BOX+GOAL+PLAYER+FLAG_FIXED_SQUARE))=0)
             and
             ((ItemIndex<PlayerIndex)
              or
              (VisitedPlayerPos[i]=False)
             )
            );
      if (i<=Game.BoardSize) and
         ((ItemIndex<>1)
          or {symmetry assumed: stop when the first wall-item passes the mid-point}
          (i<=MidPoint)
          or
          Generator.FixedTemplate {symmetry is assumed not to apply when there are fixed squares}
          or
          (WallCount=0)
         ) then begin
         ItemPos[ItemIndex]:=i;
         if      ItemIndex<=WallCount   then Board[i]:=WALL
         else if ItemIndex< PlayerIndex then
                 if   Game.ReverseMode  then
                      Board[i]:=FLOOR+GOAL+BOX
                 else Board[i]:=FLOOR+BOX
              else begin
                 Board[i]:=FLOOR+PLAYER;
                 Game.PlayerPos:=i;
                 if CalculatePlayersReachableSquares(j)>0 then begin
                    for j:=0 to Game.BoardSize do
                        if Game.PlayersReachableSquares[j]=Game.PlayersReachableSquaresTimeStamp then
                           VisitedPlayerPos[j]:=True;
                    end;
                 end;
         Inc(ItemIndex);
         end
      else
         Dec(ItemIndex);
      end;

    Result:=ItemIndex<>0;
    if Result then begin
       Inc(Count);
       if Count>=StartNo then {otherwise there's no need to make a title}
          Game.Title  :=IntToStr(Game.BoardHeight-2)+'x'+IntToStr(Game.BoardWidth-2)+
                        '-w'+IntToStr(Generator.WallCount)+'-b'+IntToStr(Generator.BoxCount)+
                        '-'+IntToStr(Count);
       end;
    end;
end;

function GenerateNextBoard_Random:Boolean;
var i,j,k,Mask:Integer; 
begin
//Result:=GenerateNextBoard_EnumeratingAll; exit;
  Result:=Generator.WallCount+Generator.BoxCount<>0;
  if Result then with Generator do begin

     Inc(Count);
     if   RandomSeed<High(RandomSeed) then Inc(RandomSeed)
     else RandomSeed:=1;
     InitializeRandomState(RandomSeed);

     for i:=1 to PlayerIndex do begin {clear the board}
         j:=ItemPos[i]; ItemPos[i]:=0; if j>0 then Board[j]:=FLOOR;
         end;

     if TemplatePlayerPos<>0 then begin {take player's position from template, if any}
        ItemPos[PlayerIndex]:=TemplatePlayerPos;
        Board[TemplatePlayerPos]:=Board[TemplatePlayerPos] or PLAYER;
        end;

     for i:=1 to Pred(PlayerIndex) do begin {'Pred': leave it to 'RunGenerator' to fill in the player position}
         if   i<>PlayerIndex then
              if   i<=WallCount then
                   Mask:=WALL+BOX+GOAL+PLAYER+FLAG_FIXED_SQUARE {walls}
              else Mask:=WALL+BOX+GOAL+PLAYER+FLAG_FIXED_SQUARE {boxes}
         else Mask:=WALL+BOX+FLAG_FIXED_SQUARE;                 {player}
         j:=ColRowDepthToSquare(Succ(Random(Game.BoardWidth)),Succ(Random(Game.BoardHeight)),Succ(Random(Game.BoardDepth)),Game.BoardWidth,Game.BoardHeight);
         k:=j;

         repeat if   j<>Game.BoardSize then Inc(j)
                else j:=1;
         until  ((Board[j] and Mask)=0)
                or
                (j=k);
         if (Board[j] and Mask)=0 then begin
            if        i<=WallCount          then Board[j]:=WALL
            else if   i<PlayerIndex         then
                      if   Game.ReverseMode then
                           Board[j]:=BOX+GOAL +FLOOR
                      else Board[j]:=BOX      +FLOOR
                 else      Board[j]:=Board[j] or PLAYER;
            ItemPos[i]:=j;
            end;
         end;

     Game.Title      :=IntToStr(Game.BoardHeight-2)+'x'+IntToStr(Game.BoardWidth-2)+
                       '-w'+IntToStr(Generator.WallCount)+'-b'+IntToStr(Generator.BoxCount)+
                       '-'+IntToStr(Generator.RandomSeed);

     {
     Game.Board:=Board;
     ShowBoard;
     Readln;
     }
     end;
end;

procedure InitializeBoardGenerator(GeneratorMethod__:TGeneratorMethod;
                                   GameReverseMode__:Boolean;
                                   WallCount__,BoxCount__,RandomSeed__,MinPushCount__,
                                   MaxBoxCount__,
                                   MaxOpenPositions__,TemplatePlayerPos__,StartNo__,TailPushesThreshold__:Integer;
                                   PrettyPrinting__,MaxBoxCountEnabled__,MaxOpenPositionsEnabled__,TrivialOpeningPushesIgnored__,TailPushesThresholdEnabled__,RandomSeedEnabled__,FixedTemplate__:Boolean;
                                   const Board__:TBoard);
var i,OldGeneratorCount,BoardSquareValue,TemplateBoxesOrGoalsCount:Integer;
begin
  with Generator do begin
    OldGeneratorCount           :=Generator.Count;
    Generator.OutputFileName    :=''; FillChar(Generator,SizeOf(Generator),0);
    Method                      :=GeneratorMethod__;
    Board                       :=Board__;                  {save board}
    if   Method=gmGA then
         BoxCount               :=Max(1,Min(BoxCount__,MAX_BOX_COUNT))
    else BoxCount               :=Max(0,Min(BoxCount__,MAX_BOX_COUNT));
    WallCount                   :=Max(0,Min(WallCount__,((Game.BoardWidth-2)*(Game.BoardHeight-2))-BoxCount-1)); {'-1': make room for the player}
    PlayerIndex                 :=Succ(WallCount+BoxCount); {player index must be after walls and boxes}
    ItemIndex                   :=1;
    StartNo                     :=StartNo__;
    TailPushesThreshold         :=TailPushesThreshold__;
    MidPoint                    :=ColRowDepthToSquare(Succ(Game.BoardWidth) div 2,Succ(Game.BoardHeight) div 2,Succ(Game.BoardDepth) div 2,Game.BoardWidth,Game.BoardHeight);
    RandomSeed                  :=Abs(RandomSeed__);
    MinPushCount                :=MinPushCount__;
    MaxBoxCount                 :=Max(0,Min(MaxBoxCount__,MAX_BOX_COUNT));
    MaxOpenPositions            :=MaxOpenPositions__;
    TemplatePlayerPos           :=TemplatePlayerPos__;
    PrettyPrinting              :=PrettyPrinting__;
    MaxBoxCountEnabled          :=MaxBoxCountEnabled__;
    MaxOpenPositionsEnabled     :=MaxOpenPositionsEnabled__;
    TrivialOpeningPushesIgnored :=TrivialOpeningPushesIgnored__;
    TailPushesThresholdEnabled  :=TailPushesThresholdEnabled__;
    RandomSeedEnabled           :=RandomSeedEnabled__;
    FixedTemplate               :=FixedTemplate__;

    Game.ReverseMode            :=GameReverseMode__;

    case Method of
      gmForward                 : begin Game.ReverseMode:=False; Generator.Count:=OldGeneratorCount; end;
      gmBackward                : begin Game.ReverseMode:=True;  Generator.Count:=OldGeneratorCount; end;
      gmBidirectional           : begin Game.ReverseMode:=False; Generator.Count:=OldGeneratorCount; end;
      gmRandom                  :;
      gmEnumerate               :;
      gmGA                      :;
      else                 Error('Internal error: "Initialize Board Generator"',TEXT_APPLICATION_TITLE);
    end; // case

    if FixedTemplate then begin
       TemplatePlayerPos :=0; TemplateBoxesOrGoalsCount:=0;

       for i:=0 to MAX_BOARD_SIZE do begin
           BoardSquareValue:=Board[i];
           if Game.ReverseMode then begin
              if (BoardSquareValue and GOAL  )<>0 then Inc(TemplateBoxesOrGoalsCount);
              end
           else begin
              if (BoardSquareValue and BOX   )<>0 then Inc(TemplateBoxesOrGoalsCount);
              if (BoardSquareValue and PLAYER)<>0 then Generator.TemplatePlayerPos:=i;
              end;

           if BoardSquareValue<>(FLOOR+FLAG_FIXED_SQUARE) then
              if   Game.ReverseMode then
                   if   (BoardSquareValue and (GOAL+WALL))<>0 then begin        {searching backwards: walls and goals can be fixed}
                        Board[i]:=BoardSquareValue or FLAG_FIXED_SQUARE;
                        if ((BoardSquareValue and GOAL)<>0) and (PlayerIndex>Succ(WallCount)) then
                           Dec(PlayerIndex); {one less auto-generated box/goal}
                        end
                   else Board[i]:=BoardSquareValue and (not FLAG_FIXED_SQUARE)
              else if   (BoardSquareValue and (PLAYER+BOX+WALL))<>0 then begin  {searching forwards: walls, boxes and player can be fixed}
                        Board[i]:=BoardSquareValue or FLAG_FIXED_SQUARE;
                        if ((BoardSquareValue and BOX)<>0) and (PlayerIndex>Succ(WallCount)) then
                           Dec(PlayerIndex); {one less auto-generated box/goal}
                        end
                   else Board[i]:=BoardSquareValue and (not FLAG_FIXED_SQUARE);
           end;

       if MaxBoxCountEnabled and (MaxBoxCount<TemplateBoxesOrGoalsCount) then
          MaxBoxCount:=TemplateBoxesOrGoalsCount;

       if Game.ReverseMode and
          (TemplateBoxesOrGoalsCount>0) and
          Ga.Control.GenerateConnectedGoals then begin
          FillChar(Game.PlayersReachableSquares,SizeOf(Game.PlayersReachableSquares),0);
          if CalculateConnectedGoals(Game.GoalPos[1],1,0,Game.Board,Game.PlayersReachableSquares)<TemplateBoxesOrGoalsCount then
             Ga.Control.GenerateConnectedGoals:=False;
          end;

       if MaxBoxCountEnabled
          and
          (MaxBoxCount=TemplateBoxesOrGoalsCount)
          and
          (TemplateBoxesOrGoalsCount<>0) then
          GA.Control.MutateWallsOnly:=True;
       end;
    end;
end;

{-----------------------------------------------------------------------------}

{Search}

function Search(var BestDepth__,BestScore__:Integer):Boolean;
var MinPlayerPos,OriginalPlayerPos,OriginalTailPushesThreshold:Integer;
    Position:PPosition;

  function EnqueueStartPositions:Boolean;
  begin {precondition: the game board state matches the starting position, and the OPEN-queue is empty}
    with Game do begin
      if (PlayerPos=0) or Game.ReverseMode then {enqueue all start-positions; this is required when the backward generator is used}
         if GetFirstPlayerAccessArea then
            repeat Result:=MakePosition(BoardHashValue,PlayerPos,0,0,0,dUp,nil,Positions.BestPosition);
            until  (not GetNextPlayerAccessArea) or (not Result)
         else Result:=False
      else {enqueue a single start position}
         Result:=(CalculatePlayersReachableSquares(MinPlayerPos)>0) and
                 MakePosition(BoardHashValue,MinPlayerPos,0,0,0,dUp,nil,Positions.BestPosition);
      end;
  end;

  function IsALegalGoalPosition(Position__:PPosition):Boolean;
  var p:PPosition;
  {a position is not a legal goal position if there exists a shorter           }
  {path to a position with an identical box constellation, ignoring the        }
  {player position                                                             }
  begin
    if Game.ReverseMode then
       {this constraint applied to the backward generator when it handled      }
       {each player's access area separately, but it was became obsolete at    }
       {the time the backward generator switched to handling all access areas  }
       {in parallel                                                            }
       Result:=True
    else begin
       Result:=GetFirstPosition(Position__^.HashValue,p);
       if Result then
          repeat Result:=p^.Depth>=Position^.Depth;
          until  (not Result) or (not GetNextPosition(p));
       end;
  end; {IsALegalGoalPosition}

  procedure GenerateMoves(LastPushedBoxNo__:Integer; var MoveCount__:Integer; var Moves__:TMoves);
  var BoxNo,MinPlayerPos,Square,NeighbourSquare:Integer; Direction:TDirection;
  begin
    MoveCount__:=0;
    CalculatePlayersReachableSquares(MinPlayerPos);
    BoxNo:=LastPushedBoxNo__; if BoxNo=0 then BoxNo:=1;
    if Game.ReverseMode then begin
       while BoxNo<=Game.BoxCount do begin
           Square:=Game.BoxPos[BoxNo];
           for Direction:=Low(Direction) to High(Direction) do begin
               NeighbourSquare:=Square+Game.NeighbourSquareOffset[Direction];
               if ((Game.Board[NeighbourSquare                                      ] and (BOX+WALL))=0) and
                  ((Game.Board[NeighbourSquare+Game.NeighbourSquareOffset[Direction]] and (BOX+WALL))=0) and
                  (Game.PlayersReachableSquares[NeighbourSquare]=Game.PlayersReachableSquaresTimeStamp) then begin
                  Inc(MoveCount__);
                  Moves__[MoveCount__].BoxNo:=BoxNo;
                  Moves__[MoveCount__].Direction:=Direction;
                  end;
               end;
           if   BoxNo<>LastPushedBoxNo__ then Inc(BoxNo)
           else BoxNo:=1;                                 {first time through the loop}
           if   BoxNo= LastPushedBoxNo__ then Inc(BoxNo); {skip last pushed box when seeing it for the second time}
           end
       end
    else begin
       while BoxNo<=Game.BoxCount do begin
           Square:=Game.BoxPos[BoxNo];
           for Direction:=Low(Direction) to High(Direction) do
               if (Game.PlayersReachableSquares[Square-Game.NeighbourSquareOffset[Direction]]=Game.PlayersReachableSquaresTimeStamp)
                  and
                  ((Game.Board[Square+Game.NeighbourSquareOffset[Direction]] and (BOX+WALL))=0) then begin
                  Inc(MoveCount__);
                  Moves__[MoveCount__].BoxNo:=BoxNo;
                  Moves__[MoveCount__].Direction:=Direction;
                  end;
           if   BoxNo<>LastPushedBoxNo__ then Inc(BoxNo)
           else BoxNo:=1;                                 {first time through the loop}
           if   BoxNo= LastPushedBoxNo__ then Inc(BoxNo); {skip last pushed box when seeing it for the second time}
           end;
       end;
  end; {GenerateMoves}

  procedure ExpandPosition(Position__:PPosition);
  var i,MinPlayerPos,MoveCount,SuccessorDepth,SuccessorScore:Integer;
      BoardHashValue:THashValue; Moves:TMoves; Successor:PPosition;
  begin
    with Position__^ do begin
      Move.Direction:=TDirection(Ord(Move.Direction) or POSITION_TAG_CLOSED);   {'CLOSED': with the current machinery, the path to an expanded node cannot change after expansion, even if a new better path is discovered later}

      {$IFDEF CONSOLE_APPLICATION}
        if (Positions.OpenPositions.Count mod 10000)=0 then with Positions do begin
           Write(Generator.Count);
           if Generator.PlayerAccessAreaNo<>0 then Write('.',Generator.PlayerAccessAreaNo);
           Writeln(' Open: ',OpenPositions.Count,' Positions: ',Count,' Pushes: ',Positions.BestPosition^.Depth);
           end;
      {$ENDIF}

      SetPosition(Position__);                              {update board so it matches the position}
      GenerateMoves(Position__^.Move.BoxNo,MoveCount,Moves);

      {
      ShowBoard;
      Writeln('Depth: ',Depth,' Positions: ',Positions.Count,' Duplicates: ',Positions.RepetitionCount);
      Readln;
      }

      for i:=1 to MoveCount do
          if Result then with Moves[i] do begin             {for each move...}
             DoMove(BoxNo,Direction);                       {perform the move}
             BoardHashValue:=Game.BoardHashValue;           {save the hash-value for the new position}
             CalculatePlayersReachableSquares(MinPlayerPos);{board hash-value and minimum player-position identifies the position}
             UndoMove(BoxNo,Direction);                     {take the move back again}

             if (Positions.TailPositionCount<=0)            {'True': include tail positions in the fitness calculation}
                or
                ((Ord(Position__^.Move.Direction) and POSITION_TAG_TAIL)=0) then begin {'True': this isn't a tail position}
                SuccessorDepth:=Succ(Depth);
                if        (Position__^.Move.BoxNo    <>BoxNo) then
                          SuccessorScore:=Position__^.Score+2    {the score is box-lines + box-changes}
                else if   (Ord(Position__^.Move.Direction) and DIRECTION_BIT_MASK)<>(Ord(Direction) and DIRECTION_BIT_MASK) then
                          SuccessorScore:=Succ(Position__^.Score) {the box changes direction}
                     else SuccessorScore:=Position__^.Score;
                end
             else begin {the current position is a tail position, i.e., it's considered a trivial end-game position}
                SuccessorDepth:=1;
                SuccessorScore:=2;
                end;

             if   MakePosition(BoardHashValue,MinPlayerPos,SuccessorDepth,SuccessorScore,BoxNo,Direction,Position__,Successor) then begin
                  if (
                      (
                       (GA.Control.FitnessFunction=ffPushes)
                       and
                       (Successor^.Depth>=Positions.BestPosition^.Depth)
                       and
                       ((Successor^.Depth>Positions.BestPosition^.Depth)
                        or
                        (Successor^.Score>Positions.BestPosition^.Score)
                       )
                      )
                      or
                      (
                       (GA.Control.FitnessFunction=ffScore)
                       and
                       (Successor^.Score>=Positions.BestPosition^.Score)
                       and
                       ((Successor^.Score>Positions.BestPosition^.Score)
                        or
                        (Successor^.Depth>Positions.BestPosition^.Depth)
                       )
                      )
                     )
                     and
                     IsALegalGoalPosition(Successor) then begin
                     Positions.BestPosition:=Successor;     {new best path}
                     end
                  else begin                                {not a new best path: do nothing}
                     end;
                  end
             else if Successor=nil then begin               {'nil': memory was filled}
                     Result:=False;                         {exhaustive search failed}
                     ClearOpenPositions;                    {clear open-queue}
//                   Positions.BestPosition^.Next:=nil;     {best position may still be a member of the open queue}
                     break;                                 {exit 'for' loop, i.e., stop expanding this position}
                     end;
             end;
      end;
  end; {ExpandPosition}

  function TailPushesSearch:Boolean;
  var Position:PPosition;

    procedure ExpandPosition(Position__:PPosition);
    var i,MinPlayerPos,MoveCount,SuccessorDepth,SuccessorScore:Integer;
        BoardHashValue:THashValue; Successor:PPosition; Moves:TMoves;

      procedure GenerateMoves(Position__:PPosition; var MoveCount__:Integer; var Moves__:TMoves);
      var BoxNo,LastPushedBoxNo,MinPlayerPos,Square,NeighbourSquare:Integer;
          Direction:TDirection; p:PPosition; VisitedBoxesCount:TBoxPositions;
      begin
        MoveCount__:=0;
        CalculatePlayersReachableSquares(MinPlayerPos);             {calculate player's reachable squares}

        FillChar(VisitedBoxesCount,SizeOf(VisitedBoxesCount),0);
        p:=Position__;
        while p^.Parent<>nil do begin                               {calculate the boxes that have moved}
          if p^.Move.BoxNo<>p^.Parent^.Move.BoxNo then              {'<>': in a sequence of moves like 'AAABBAAA', box 'A' is counted for 2 box sessions, and box 'B' is counted for 1 box session}
             Inc(VisitedBoxesCount[p^.Move.BoxNo]);
          p:=p^.Parent;
          end;

        LastPushedBoxNo:=Position__^.Move.BoxNo;
        BoxNo:=LastPushedBoxNo; if BoxNo=0 then BoxNo:=1;

        while BoxNo<=Game.BoxCount do begin
          if (VisitedBoxesCount[BoxNo]<Abs(Generator.TailPushesThreshold))
             or
             (BoxNo=Position__^.Move.BoxNo) then begin
             Square:=Game.BoxPos[BoxNo];
             for Direction:=Low(Direction) to High(Direction) do begin
                 NeighbourSquare:=Square+Game.NeighbourSquareOffset[Direction];
                 if ((Game.Board[NeighbourSquare                                      ] and (BOX+WALL))=0) and
                    ((Game.Board[NeighbourSquare+Game.NeighbourSquareOffset[Direction]] and (BOX+WALL))=0) and
                    (Game.PlayersReachableSquares[NeighbourSquare]=Game.PlayersReachableSquaresTimeStamp) then begin
                    Inc(MoveCount__);
                    Moves__[MoveCount__].BoxNo:=BoxNo;
                    Moves__[MoveCount__].Direction:=Direction;
                    end;
                 end;
             end;
          if   BoxNo<>LastPushedBoxNo then Inc(BoxNo)
          else BoxNo:=1;                               {first time through the loop}
          if   BoxNo= LastPushedBoxNo then Inc(BoxNo); {skip last pushed box when seeing it for the second time}
          end;
      end; {TailPushesSearch.ExpandPosition.GenerateMoves}

    begin {TailPushesSearch.ExpandPosition}
      with Position__^ do begin
        SetPosition(Position__);                              {update board so it matches the position}

        {
        ShowBoard;
        Writeln('Depth: ',Depth,' Positions: ',Positions.Count,' Duplicates: ',Positions.RepetitionCount,' Visited: ',VisitedBoxesCount[Move.BoxNo]);
        Readln;
        }

        GenerateMoves(Position__,MoveCount,Moves);

        for i:=1 to MoveCount do
            if Result then with Moves[i] do begin             {for each move...}
               DoMove(BoxNo,Direction);                       {perform the move}
               BoardHashValue:=Game.BoardHashValue;           {save hash-value}
               CalculatePlayersReachableSquares(MinPlayerPos);{board hash-value and minimum player-position identifies the position}
               UndoMove(BoxNo,Direction);                     {take the move back again}
               {$WARNINGS OFF}                                {'Successor' not initialized}
                 SuccessorDepth:=High(Successor^.Depth);
                 SuccessorScore:=High(Successor^.Score);
               {$WARNINGS ON}

               if MakePosition(BoardHashValue,MinPlayerPos,SuccessorDepth,SuccessorScore,BoxNo,Direction,Position__,Successor) then begin
                  {$IFDEF CONSOLE_APPLICATION}
                    if (Positions.Count mod 100000)=0 then begin
                       Write(Generator.Count);
                       if Generator.PlayerAccessAreaNo<>0 then Write('.',Generator.PlayerAccessAreaNo);
                       Writeln(' Tail positions: ',Positions.Count);
                       //ShowBoard; Readln;
                       end;
                  {$ENDIF}
                  end
               else if Successor=nil then begin               {'nil': memory was filled}
                       Result:=False;                         {exhaustive search failed}
                       ClearOpenPositions;                    {clear open-queue}
                       break;                                 {exit 'for' loop, i.e., stop expanding this position}
                       end;
               end;

        Depth:=High(Depth); Score:=High(Score);               {high-values: informs the main search that the position hasn't been visited yet}
        Move.Direction:=TDirection(Ord(Move.Direction) or POSITION_TAG_TAIL); {mark the position as being a tail-position}
        end;
    end; {TailPushesSearch.ExpandPosition}

  begin {TailPushesSearch}
    Result:=EnqueueStartPositions; {put all starting positions on the OPEN-queue}

    if   Generator.MaxOpenPositionsEnabled and (Generator.MaxOpenPositions>=0) then
         while (Positions.OpenPositions.Count<=Generator.MaxOpenPositions) and
               DequeueOpenPosition(Position) do
               ExpandPosition(Position)
    else {no 'maximum number of open positions' limit}
         while DequeueOpenPosition(Position) do
               ExpandPosition(Position);

    SetPosition(nil); {restore the board starting position}
    MovePlayer(OriginalPlayerPos); {restore original player-position, if any}
    ClearOpenPositions;
    FillChar(Positions.PositionsPerDepthCount,SizeOf(Positions.PositionsPerDepthCount),0);
    Positions.RepetitionCount:=0;
    Positions.TailPositionCount:=Positions.Count;

    {$IFDEF CONSOLE_APPLICATION}
      Write(Generator.Count);
      if Generator.PlayerAccessAreaNo<>0 then Write('.',Generator.PlayerAccessAreaNo);
      Writeln(' Tail positions: ',Positions.TailPositionCount);
    {$ENDIF}

    if (not Generator.TailPushesThresholdEnabled) or (Generator.TailPushesThreshold<=0) then
       Positions.TailPositionCount:=-Positions.TailPositionCount; {negated: don't use tail position information in the fitness function}
  end; {TailPushesSearch}

begin {Search}
  with Game do begin
    Result:=False; BestDepth__:=0; BestScore__:=0;
    ClearPositions; InitializeGame;
    OriginalPlayerPos:=PlayerPos;
    OriginalTailPushesThreshold:=Generator.TailPushesThreshold;
    Generator.TailPushesThreshold:=Sign(Generator.TailPushesThreshold)*Min(Abs(Generator.TailPushesThreshold),Pred(Game.BoxCount));
    //ShowBoard; Write('Search: ',Generator.Count); Readln;

    if Generator.TailPushesThresholdEnabled and (Generator.TailPushesThreshold<>0) then
       TailPushesSearch; {find all trivial tail-pushes sequences, so candidates with long tails don't get good scores just because of their length}

    Result:=EnqueueStartPositions;

    if Result then begin
       {the central breadth-first search loop}
       if Generator.MaxOpenPositionsEnabled and (Generator.MaxOpenPositions>=0) then
          while (Positions.OpenPositions.Count<=Generator.MaxOpenPositions) and
                DequeueOpenPosition(Position) do
                ExpandPosition(Position)
       else {no 'maximum number of open positions' limit}
          while DequeueOpenPosition(Position) do
                ExpandPosition(Position);
       end;

    RemovePositionFromOpenPositions(Positions.BestPosition); {the best position may still be a member of the open-list}
    ClearOpenPositions;

    SetPosition(nil); {undo moves, if any}
    MovePlayer(OriginalPlayerPos); {restore original player-position, if any}
    Generator.TailPushesThreshold:=OriginalTailPushesThreshold; {restore tail position threshold}

    if (Positions.BestPosition<>nil) then with Positions.BestPosition^ do begin
       BestDepth__:=Depth; BestScore__:=Score;
       end;
    Result:=Result and (BestDepth__>0);
    end;
end; {Search}

function SearchPlayerAccessAreas(const Board__:TBoard):Integer; {Returns a 'fitness value' for the GA generator}
var i,Depth,Score,BestScore:Integer;
    BestPlayerAccessAreaNo,SearchedPlayerAccessAreaNo:Integer;
    BestBoard:TBoard;

  procedure RandomizePlayerStartPosition;
  var i,j,MinPlayerPos:Integer;
  begin
    j:=Random(CalculatePlayersReachableSquares(MinPlayerPos));
    for i:=0 to Game.BoardSize do
        if Game.PlayersReachableSquares[i]=Game.PlayersReachableSquaresTimeStamp then begin
           if j=0 then MovePlayer(i);
           Dec(j);
           end;
  end;

  function  TrimTrivialOpeningPushes( var BestPosition__ : PPosition; var Depth__, Score__ : Integer ) : Boolean;
  var BoxNo, BoxOnGoalCount, NeighbourFloorCount, NewBoxPos, PlayerPos, NewPlayerPos : Integer;
      IsBoardModified, IsTunnelSquare : Boolean; Dir, Direction : TDirection; OriginalBoard : TBoard;
  begin {trims trivial opening pushes, e.g., forced pushes where the player is in a "tunnel" which is blocked by a box}
        {preconditions: the game board matches 'BestPosition__', and the search leading to the best position was a backward search}
    Result := False; BoxOnGoalCount := 0; Direction := dUp; PlayerPos := Game.PlayerPos;
    System.Move( Game.Board, OriginalBoard, Succ( Game.BoardSize ) * SizeOf( OriginalBoard[ 0 ] ) ); {save a copy of the original board; for efficiency, only the used part fo the board is copied}
    IsBoardModified             := FillTubes( False ) <> 0; {fill tunnels without moving the player or the boxes; this helps squeezing the player out of a tunnel}
    for BoxNo := 1 to Game.BoxCount do {count boxes on goal squares}
        if IsAGoalSquare( Game.BoxPos[ BoxNo ]) then Inc( BoxOnGoalCount );
    repeat
      NeighbourFloorCount       := 0;
      for Dir                   := Low( Dir ) to High( Dir ) do {count neighbour floor squares around the player position}
          if IsAFloorSquare( PlayerPos + Game.NeighbourSquareOffset[ Dir ] ) then begin
             Inc( NeighbourFloorCount ); Direction := Dir;
             end;
      IsTunnelSquare            := ( NeighbourFloorCount = 1 ) and     {'True': the player has exactly one floor square neighbour}
                                   Assigned( BestPosition__^.Parent ); {don't treat the root position as a candidate for pruning}

      if  IsTunnelSquare then begin {'True': the player can only move in one direction}
          NewPlayerPos          := PlayerPos + Game.NeighbourSquareOffset[ Direction ];
          if   IsABoxSquare( NewPlayerPos ) then begin
               NewBoxPos        := NewPlayerPos + Game.NeighbourSquareOffset[ Direction ];
               IsTunnelSquare   := ((Game.Board[ NewBoxPos ] and ( WALL + BOX ) ) = 0 ) and {the box can be pushed forward}
                                   ( BoxOnGoalCount < Game.BoxCount );                      {the position isn't a solution}
               end
          else NewBoxPos        := 0;  {a non-pushing player move}
          if IsTunnelSquare then begin {'True': the player is in a tunnel and has only one legal move; move the player and put a wall on the square}
             if not Result then begin  {'True' : first move; remember that the board has been modified, so it's restored on exit from the function}
                Result          := True;
                IsBoardModified := True;
                end;
             if NewBoxPos <> 0 then begin {push a box}
                Dec( Game.Board[ NewPlayerPos  ], BOX );
                Inc( Game.Board[ NewBoxPos     ], BOX );
                if IsAGoalSquare( NewPlayerPos ) then Dec( BoxOnGoalCount );
                if IsAGoalSquare( NewBoxPos    ) then Inc( BoxOnGoalCount );
                BestPosition__  := BestPosition__^.Parent; {backtrack to the parent node, i.e., drop the push leading to the parent position}
                end;
             Game.Board[ PlayerPos ] := WALL;         {put a wall on the tunnel square}
             PlayerPos               := NewPlayerPos; {move the player}
             end
          end;
    until not IsTunnelSquare;
    if IsBoardModified then
       System.Move( OriginalBoard, Game.Board, Succ( Game.BoardSize ) * SizeOf( OriginalBoard[ 0 ] ) ); {restore the original board}
    if Result then begin
       Depth__    := BestPosition__^.Depth; {update the returned best values 'Depth__' and 'Score__'}
       Score__    := BestPosition__^.Score;
       SetPosition( BestPosition__ );       {update the board so it matches the new best position}
       end;
  end; {TrimTrivialOpeningPushes}

begin {SearchPlayerAccessAreas}
  with Game do begin
    Board:=Board__; BestScore:=0;
    for i:=0 to BoardSize do Board[i]:=Board[i] and (not PLAYER);          {remove player from the board}
    PlayerPos:=0; History.Count:=0; History.Score:=0; History.Fitness:=-1; {clear history and player}
    Positions.TotalCount:=0; Positions.TotalDroppedCount:=0; Positions.TotalRepetitionCount:=0;
    Generator.PlayerAccessAreaNo:=0;
    BestPlayerAccessAreaNo:=0;

    repeat                                                                 {for each player access area}

      if (not Game.ReverseMode) and                                        {forward generator...}
         (Generator.PlayerAccessAreaNo=0) then
         if   Generator.TemplatePlayerPos<>0 then                          {use template player position, if any}
              PlayerPos:=Generator.TemplatePlayerPos
         else GetFirstPlayerAccessArea;                                    {get first player position}

      if (not Game.ReverseMode) and
         (Generator.TemplatePlayerPos<>0) then MovePlayer(PlayerPos);      {put the player on the board for pretty printing}

      if (LevelReader.InputFileName='') and (Generator.PlayerAccessAreaNo<=1) then begin
         {$IFDEF CONSOLE_APPLICATION}
           Writeln; Writeln(Game.Title); ShowBoard;
           {Write(':'); Readln;}
         {$ENDIF}
         end;

      Search(Depth,Score);

      Inc(Positions.TotalCount,Positions.Count);
      Inc(Positions.TotalRepetitionCount,Positions.RepetitionCount);
      Inc(Positions.TotalDroppedCount,Positions.DroppedCount);

      SearchedPlayerAccessAreaNo:=Generator.PlayerAccessAreaNo;

      if GAFitnessValue(GA.Control.FitnessFunction,Depth,Score)>BestScore then begin {new best variation of current level}
         if   ReverseMode then begin
              SetPosition(Positions.BestPosition);                   {use best position as start position}
              if Generator.TrivialOpeningPushesIgnored then          {'True': trim trivial opening pushes, e.g., forced pushes where the player is in a "tunnel" which is blocked by a box}
                 TrimTrivialOpeningPushes(Positions.BestPosition,Depth,Score);
              RandomizePlayerStartPosition;                          {randomize player's startposition; otherwise, the player is right next to the first pushed box}
              end
         else MakeGoalSquares(Positions.BestPosition);               {use best position as goal position}

         if   GAFitnessValue(GA.Control.FitnessFunction,Depth,Score)>BestScore then begin {this is a new best variation of the current level}
              BestScore:=GAFitnessValue(GA.Control.FitnessFunction,Depth,Score);
              BestBoard:=Game.Board;
              //ShowBoard; Write('Best so far'); Readln;
              BestPlayerAccessAreaNo:=SearchedPlayerAccessAreaNo;
              SaveVariationToHistory(Positions.BestPosition);
              {ShowBoard; Write('Best: ',BestScore); Readln;}
              end;
         end;

    until Game.ReverseMode or                                        {'Search' handles all player access areas when the backward generator is used}
          (Generator.TemplatePlayerPos<>0) or                        {the position is fixed}
          (not GetNextPlayerAccessArea);                             {find next player access area, if any}

    if (History.Count<>0) then begin {'True': found a variation}
       if Generator.Method<>gmBidirectional then
          if   (LevelReader.InputFileName<>'') and (Generator.Method<>gmGA) then
               Title:=Title+TEXT_LEVEL_TITLE_SUFFIX                  {processing levels from a file}
          else Title:=Title+'-'+IntToStr(History.Count);             {auto-generating levels}

       Board:=BestBoard; InitializeGame;

       {$IFDEF CONSOLE_APPLICATION}
         ShowBoard;
         Writeln('Minimum pushes: '   ,Game.History.Count);
         Writeln('Exhaustive search: ',TEXT_NO_YES[Positions.TotalDroppedCount=0]);
       {$ENDIF}

       if (Generator.OutputFileName<>'')          and
          (History.Count>=Generator.MinPushCount) and
          (Generator.Method<>gmBidirectional)     and
          (not ((Generator.Method      =gmGA)     and
                (Generator.MinPushCount=0)        and
                (History.Fitness<=GA.Individuals[GA.WorstIndividualNo].Fitness)))
          then begin
          AppendLevelToFile(Generator.OutputFileName,Title,Generator.PrettyPrinting,False,BestPlayerAccessAreaNo=SearchedPlayerAccessAreaNo);
          MakeLevelStatistics(Title,Positions.TotalDroppedCount=0,
                              BoardWidth,BoardHeight,
                              History.Count,History.Score,History.Fitness,History.TailCount,
                              Positions.TotalCount);
          WriteStatistics(Generator.OutputFileName);
          if Generator.Method=gmGA then Board:=BestBoard;            {restore board for the GA-generator}
          end;
       end
    else begin
       History.Count:=0; History.Score:=0;
       if   Game.BoxCount>0 then
            History.Fitness:=-1                                      {a board with boxes and goals, but no legal pushes/pulls}
       else History.Fitness:=0;                                      {a board with no boxes or goals}
       end;

    Result:=History.Fitness;
    end;
end;

{-----------------------------------------------------------------------------}

{Generator Toplevel}

procedure FinalizeGenerator;
begin
  FinalizeStatistics;
  FinalizePositions;
  GAFinalize;
end;

function InitializeGenerator(MemoryByteSize__:Integer):Boolean;
begin
  InitializeRandomState(0);
  FillChar(Game,SizeOf(Game),0); Game.ReverseMode:=Generator.Method<>gmForward;
  FillChar(Positions,SizeOf(Positions),0);
  InitializeLevelReader(True); InitializeStatistics; Generator.OutputFileName:='';
  Result:=InitializePositions(MemoryByteSize__);
  GA.Control.MutationTypeAccumulatedFrequencies:=GA_DEFAULT_MUTATION_TYPE_ACCUMULATED_FREQUENCIES;
  with GA.Control do
    Result:=Result and
            GAInitialize('',
                         GoodEnoughFitness,MaxGenerationCount,PopulationSize,InactivityThreshold,
                         CrossOverProbabilityPct,MutationProbabilityPct,MaxMutationCount,
                         MutationTypeAccumulatedFrequencies,
                         FitnessFunction,MutateWallsOnly,ForwardSearch,GenerateConnectedGoals,InactivityThresholdEnabled,AutoSaveIntervalTimeMS);
end;

procedure RunGenerator;
var
  {$IFDEF VERBOSE_OUTPUT}
    i:Integer;
  {$ENDIF}
  F:Text;

  function GetNextLevel:Boolean;
  begin {loads the next level and returns it in 'Generator.Board'}
    if   LevelReader.InputFileName='' then
         if    Generator.Method=gmEnumerate then
               Result:=GenerateNextBoard_Enumerate
         else  Result:=GenerateNextBoard_Random
    else begin Result:=LoadNextLevelFromFile(0,0,0);
               Generator.Board:=Game.Board;
         end;
  end;

  function GetFirstLevel:Boolean;
  var i, oMaxBoxCount:Integer; oGenerateConnectedGoals:Boolean; FileName:String;
  begin {loads the first level and returns it in 'Generator.Board'}
    if LevelReader.InputFileName<>'' then begin

       oMaxBoxCount:=Generator.MaxBoxCount;
       oGenerateConnectedGoals:=GA.Control.GenerateConnectedGoals;
       if Generator.FixedTemplate then begin
          {trick 'InitializeGame' so it accepts the contents of the template}
          Generator.MaxBoxCount:=MAX_BOX_COUNT;
          GA.Control.GenerateConnectedGoals:=False;
          end;

       FileName:=LevelReader.InputFileName;
       Result:=LoadFirstLevelFromFile(LevelReader.InputFileName);
       Generator.Board:=Game.Board; {the board is returned in 'Generator.Board'}

       {ShowBoard; Write('File input: '); Readln;}

       Generator.MaxBoxCount:=oMaxBoxCount;
       GA.Control.GenerateConnectedGoals:=oGenerateConnectedGoals;

       while Result and (LevelReader.Count<Generator.StartNo) do
         Result:=GetNextLevel;

       if Result and (Generator.StartNo=0) and LevelReader.IsAGALogFile then begin
          Generator.Method:=gmGA; GA.FileName:=LevelReader.InputFileName; Game.ReverseMode:=True; {continue working with this GA data-set}
          end;

       if Result and
          ((Generator.Method=gmRandom) or (Generator.Method=gmGA) or (Generator.Method=gmEnumerate)) and
          (LevelReader.InputFileName<>'') then begin
          LevelReader.InputFileName:=''; CloseFile(LevelReader.InputFile); {clean up, i.e., close the file}
          end;

       if Result and
          ((Generator.Method=gmRandom)
           or
           (((Generator.Method=gmGA) or (Generator.Method=gmEnumerate))
            and
            (Generator.BoxCount+Generator.WallCount<>0))
          )
          then with Generator do begin {use this level as template}
          if BoxCount=0 then begin {'0': no '-boxes' input parameter}
             BoxCount:=Game.BoxCount; {use the number of boxes on the board}
             if not FixedTemplate then
                for i:=0 to Game.BoardSize do Game.Board[i]:=Game.Board[i] and (not (BOX+GOAL+PLAYER)); {remove everything but walls from the board}
             WallCount:=Max(0,Min(WallCount,Game.FloorCount-Game.BoxCount-1));
             end
          else begin
             BoxCount:=Max(0,Min(BoxCount,Game.FloorCount-Game.BoxCount-1)); {'-1': player position}
             if not FixedTemplate then begin
                for i:=0 to Game.BoardSize do Game.Board[i]:=Game.Board[i] and (not (BOX+PLAYER));
                for i:=0 to Game.BoardSize do
                    if (Game.Board[i] and GOAL)<>0 then Inc(Game.Board[i],BOX);
                end;
             WallCount:=Max(0,Min(WallCount,Game.FloorCount-Game.BoxCount-BoxCount-1));
             end;
          TemplatePlayerPos:=Game.PlayerPos;
          if Method=gmGA then Game.ReverseMode:=not GA.Control.ForwardSearch;
          InitializeBoardGenerator(Method,Game.ReverseMode,WallCount,BoxCount,RandomSeed,MinPushCount,MaxBoxCount,MaxOpenPositions,TemplatePlayerPos,StartNo,TailPushesThreshold,
                                   PrettyPrinting,MaxBoxCountEnabled,MaxOpenPositionsEnabled,TrivialOpeningPushesIgnored,TailPushesThresholdEnabled,RandomSeedEnabled,FixedTemplate,Game.Board);
          if   Generator.Method=gmEnumerate then
               Result:=GenerateNextBoard_Enumerate
          else Result:=GenerateNextBoard_Random;
          end
       else with Generator do begin
          if   Method<>gmBidirectional then TemplatePlayerPos:=Game.PlayerPos
          else TemplatePlayerPos:=0;
          if Method=gmGA then Game.ReverseMode:=not GA.Control.ForwardSearch;
          InitializeBoardGenerator(Method,Game.ReverseMode,WallCount,BoxCount,RandomSeed,MinPushCount,MaxBoxCount,MaxOpenPositions,TemplatePlayerPos,StartNo,TailPushesThreshold,
                                   PrettyPrinting,MaxBoxCountEnabled,MaxOpenPositionsEnabled,TrivialOpeningPushesIgnored,TailPushesThresholdEnabled,RandomSeedEnabled,FixedTemplate,Game.Board);
          end;
       end
    else with Generator do begin
       MakeBoard(Game.BoardWidth,Game.BoardHeight,Game.BoardDepth); {initializes the board in global variable 'Game'}
       if Method=gmGA then Game.ReverseMode:=not GA.Control.ForwardSearch;
       InitializeBoardGenerator(Method,Game.ReverseMode,WallCount,BoxCount,RandomSeed,MinPushCount,MaxBoxCount,MaxOpenPositions,TemplatePlayerPos,StartNo,TailPushesThreshold,
                                PrettyPrinting,MaxBoxCountEnabled,MaxOpenPositionsEnabled,TrivialOpeningPushesIgnored,TailPushesThresholdEnabled,RandomSeedEnabled,False,Game.Board);
       if        Method=gmEnumerate then
                 repeat  Result:=GenerateNextBoard_Enumerate;
                         {$IFDEF CONSOLE_APPLICATION}
                           if Count mod 100000=0 then Writeln(Count);
                         {$ENDIF}
                 until   (Count>=StartNo) or (not Result)
       else if   Method=gmGA then begin
                 i:=RandomSeed;
                 GenerateNextBoard_Random;
                 RandomSeed:=i;
                 Result:=True;
                 end
            else Result:=GenerateNextBoard_Random;
       end;
  end;

  function BidirectionalSearch(Board__:TBoard):Integer;
  var i,Score,TotalCount,TotalDroppedCount:Integer; HashKey:THashValue;
      Done,ReverseMode:Boolean; BestBoard:TBoard; BestHistory:THistory;
  begin
    Result:=0; TotalCount:=0; TotalDroppedCount:=0;

    for ReverseMode:=Low(ReverseMode) to High(ReverseMode) do begin
        Game.ReverseMode:=ReverseMode;
        Game.Board:=Board__; GAHistoryClear; {use GAHistory to save seen boards}
        repeat Game.ReverseMode:=not Game.ReverseMode;
               if Game.ReverseMode then MoveBoxesToGoalSquares;
               {$IFDEF CONSOLE_APPLICATION}
                 Writeln; Writeln(Game.Title); ShowBoard;
               {$ENDIF}
               Score:=SearchPlayerAccessAreas(Game.Board);
               Inc(TotalCount,Positions.TotalCount);
               Inc(TotalDroppedCount,Positions.TotalDroppedCount);

               if Score>0 then with Game do begin
                  if Score>Result then begin
                     Result:=Score; BestBoard:=Board; BestHistory:=Game.History;
                     end;

                  HashKey:=0; {calculate a hash-key based on boxes and goals}
                  for i:=0 to BoardSize do begin
                      if (Board[i] and BOX )<>0 then HashKey:=HashKey xor   Game.HashSquareValues[i];
                      if (Board[i] and GOAL)<>0 then HashKey:=HashKey xor (-Game.HashSquareValues[i]);
                      end;
                  Done:=GAHistoryLookup(HashKey)<>nil;
                  if not Done then GAHistoryAdd(HashKey);
                  end
               else Done:=True;
        until  Done;
        end;

    if (Result>0) and (Generator.OutputFileName<>'') then with Game do begin
       Game.Board:=BestBoard; Game.History:=BestHistory; InitializeGame;
       if   LevelReader.InputFileName<>'' then
            Title:=Title+TEXT_LEVEL_TITLE_SUFFIX                     {processing levels from a file}
       else Title:=Title+'-'+IntToStr(History.Count);                {auto-generating levels}
       AppendLevelToFile(Generator.OutputFileName,Title,Generator.PrettyPrinting,False,False);
       MakeLevelStatistics(Title,TotalDroppedCount=0,
                           BoardWidth,BoardHeight,
                           History.Count,History.Score,History.Fitness,History.TailCount,
                           TotalCount);
       WriteStatistics(Generator.OutputFileName);
       end;
  end;

begin {RunGenerator}
  ClearStatistics;

  if   GetFirstLevel then begin

       if LevelReader.InputFileName='' then begin                    {autogenerating levels, either genetic, randomly or by enumeration}
          {$IFDEF VERBOSE_OUTPUT}
            Generator.OutputFileName:=TEXT_AUTO_GENERATED_LEVELS_FILE_NAME;
          {$ELSE}
            Generator.OutputFileName:=TEXT_APPLICATION_TITLE+BRIEF_OUTPUT_FILE_EXT;
          {$ENDIF}
          end
       else with LevelReader do begin                                {processing levels from a file}
          {$IFDEF VERBOSE_OUTPUT}
            i:=Pos(PERIOD,InputFileName);
            if   i<>0 then {the filename includes an extension name}
                 Generator.OutputFileName:=Copy(InputFileName,1,Pred(i))+TEXT_LEVEL_TITLE_SUFFIX+Copy(InputFileName,i,Length(InputFileName)-i+1)
            else Generator.OutputFileName:=InputFileName+TEXT_LEVEL_TITLE_SUFFIX;
          {$ELSE}
            Generator.OutputFileName:=TEXT_APPLICATION_TITLE+BRIEF_OUTPUT_FILE_EXT;
          {$ENDIF}
          end;

       if not (LevelReader.ISaGALogFile and (Generator.StartNo=0) and (Generator.Method=gmGA)) then begin
          AssignFile(F,Generator.OutputFileName); Rewrite(F); CloseFile(F); {clear outputfile}
          end;
       {$IFDEF VERBOSE_OUTPUT}
       AssignFile(F,ExtractFileNameWithoutExt(Generator.OutputFileName)+TEXT_STATISTICS_FILENAME_SUFFIX);
       Rewrite(F); CloseFile(F); {clear statistics file}
       {$ENDIF}

       if Generator.Method=gmGA then begin
          if LevelReader.IsAGALogFile and (Generator.StartNo=0) then begin {continue work on existing data-set}
             {$IFNDEF PLUGIN}
               if GALoadFromFile(GA.FileName) then GARun;
             {$ENDIF}
             end
          else begin {use the loaded level to seed the genetic generator}
             if GA.FileName='' then begin
                GA.FileName:=GA_DEFAULT_FILE_NAME;
                AssignFile(F,GA.FileName); Rewrite(F); CloseFile(F); {clear log file}
                end;
             Game.Board:=Generator.Board;                            {'Generator.Board' contains output from the random generator or the enumerating generator, if one of them have been activated}
             {ShowBoard; Write(':'); Readln;}
             if GALoadAllelesFromGame>0 then GARun;
             end;
          end
       else
          repeat
            if Generator.Method<>gmBidirectional then begin
               if LevelReader.InputFileName<>'' then begin           {processing levels from a file}
                  {$IFDEF CONSOLE_APPLICATION}
                    Writeln; Writeln(Game.Title); ShowBoard;
                  {$ENDIF}
                  if Game.ReverseMode then MoveBoxesToGoalSquares;
                  end;
               SearchPlayerAccessAreas(Generator.Board);
               end
            else BidirectionalSearch(Generator.Board);
          until not GetNextLevel;
       end;
end;

{-----------------------------------------------------------------------------}

{Application Toplevel}

function InitializeApplication:Boolean;
var BoardWidth,BoardHeight,BoardDepth:Integer; InputFileName:String;
begin
  FillChar(Positions,SizeOf(Positions),0);
  FillChar(Generator,SizeOf(Generator),0);
  FillChar(GA,SizeOf(GA),0);
  ShowTitle;

  {$WARNINGS OFF}
    Result:=SizeOf(TPosition) mod 4=0;
    if not Result then Error('Internal error: Positions are not 4-byte aligned (Size='+IntToStr(SizeOf(TPosition))+').',TEXT_APPLICATION_TITLE);

    with Positions.BestPosition^ do
      if (Succ(Ord(High(Move.Direction))-Ord(Low(Move.Direction)))>DIRECTION_BIT_MASK) or
         (POSITION_TAG_BIT_MASK>High(Byte)) or
         (MAX_HISTORY_MOVES>=High(Depth)) or (MAX_HISTORY_MOVES>=High(Score) div 2) or
         (MAX_HISTORY_MOVES>=High(GA.Individuals[Low(GA.Individuals)].Fitness)-(2*(MAX_HISTORY_MOVES+1)*(2*MAX_HISTORY_MOVES))) {fitness value must not overflow; see the function 'GAFitnessValue()'}
         then
         Result:=Error('Internal error: Potentially overflowing information in the stored positions',TEXT_APPLICATION_TITLE);
    if GA_MAX_INDIVIDUALS+GA_MAX_PENDING_CANDIDATES>High(Byte) then
       Result:=Error('Internal error: Maximum number of individuals exceeded',TEXT_APPLICATION_TITLE);
  {$WARNINGS ON}

  if Result then
     Result:=GetCommandLineParameters(InputFileName,
                                      Generator.Method,
                                      Generator.StartNo,Generator.RandomSeed,
                                      Positions.MemoryByteSize,
                                      Generator.MinPushCount,
                                      Generator.MaxBoxCount,
                                      Generator.MaxOpenPositions,
                                      BoardWidth,BoardHeight,BoardDepth,
                                      Generator.WallCount,Generator.BoxCount,
                                      GA.Control.CrossOverProbabilityPct,
                                      GA.Control.MutationProbabilityPct,
                                      GA.Control.MaxMutationCount,
                                      GA.Control.MaxGenerationCount,
                                      GA.Control.PopulationSize,
                                      GA.Control.InactivityThreshold,
                                      GA.Control.GoodEnoughFitness,
                                      Generator.TailPushesThreshold,
                                      GA.Control.FitnessFunction,
                                      GA.Control.ForwardSearch,
                                      GA.Control.GenerateConnectedGoals,
                                      Generator.PrettyPrinting,
                                      Generator.MaxBoxCountEnabled,
                                      Generator.MaxOpenPositionsEnabled,
                                      GA.Control.InactivityThresholdEnabled,
                                      Generator.TrivialOpeningPushesIgnored,
                                      Generator.TailPushesThresholdEnabled,
                                      Generator.RandomSeedEnabled,
                                      Generator.FixedTemplate,
                                      GA.Control.AutoSaveIntervalTimeMS)
             and
             InitializeGenerator     (Positions.MemoryByteSize);
  if Result then begin
     LevelReader.InputFileName  :=InputFileName;   {save filename, if any}
     InitializeBoard(BoardWidth,BoardHeight,BoardDepth,True); {save board dimensions, if any}

     {$IFDEF CONSOLE_APPLICATION}
       Writeln;
       Writeln('Memory: ',(Positions.MemoryByteSize+(ONE_MEBI div 2)) div ONE_MEBI,' MiB',
               '   Position capacity: ',Positions.VectorCapacity*MEMORY_BLOCKS_COUNT,
               '   Position size: ',SizeOf(TPosition));
       {Msg('','');}
     {$ENDIF}
     end
  else begin
     ShowHelp; Msg('','');
     end;
end;

procedure RunApplication;
begin
  RunGenerator;
end;

procedure FinalizeApplication;
begin
  FinalizeGenerator;
  //Msg('Done','');
end;

begin {main}
  if InitializeApplication then
     RunApplication;
  FinalizeApplication;
end.




