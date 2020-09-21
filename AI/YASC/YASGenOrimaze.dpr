{
YASGenOrimaze - Yet Another Sokoban Level Generator for Orimaze Sokoban Levels
Version 1.9 - April 6, 2017
Copyright (c) 2017 by Brian Damgaard, Denmark

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

{   YASGenOrimaze - Yet Another Sokoban Generator for Orimaze Sokoban Levels
    ------------------------------------------------------------------------
}

{///$DEFINE CALCULATE_SEQUENTIALLY}            {enumerate and process the puzzles sequentially. This is a simple and the most trustworthy algorithm, but it's also slower than processing the puzzles one cluster at a time.}

{$IFDEF CALCULATE_SEQUENTIALLY}                {'True': Process puzzles sequentially}
  {$UNDEF CALCULATE_CLUSTERS}                  {for safety}
{$ELSE}                                        {Process puzzles one cluster at a time.}
  {$DEFINE CALCULATE_CLUSTERS}                 {Since Orimaze moves are reversible, all reachable game states from a given starting position belong to one unique cluster.}
{$ENDIF}

{$DEFINE X86_32}                               {compiler: use only one of these: X86_32 or X86_64 (only X86_32 is implemented)}
{///$DEFINE X86_64}                            {compiler: use only one of these: X86_32 or X86_64 (only X86_32 is implemented)}

{$IFDEF X86_32}
  {$UNDEF X86_64}                              {for safety}
{$ELSE}
  {$DEFINE X86_64}
{$ENDIF}

{$DEFINE DELPHI}                               {compiler: use only one of these: DELPHI of FPC}
{///$DEFINE FPC}                               {compiler: use only one of these: DELPHI of FPC}

{$DEFINE WINDOWS}                              {use this on the Windows platform only}

{$IFDEF DELPHI}
  {$APPTYPE CONSOLE}                           {use this with DELPHI only}
  {$UNDEF FPC}                                 {for safety}
  {$M 1048576,2097152}                         {stack size, minimum 1 MiB}
  {///$STACKCHECKS ON}                         {stack checks, only for debugging}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI} {$PACKENUM 1}                 {use this with FPC only}
  {$UNDEF DELPHI}                              {for safety}
  {$M 1048576,0}                               {stack size, minimum 1 MiB}
{$ENDIF}

{$R-}                                          {'R-': range checking disabled}

program YASGenOrimaze;                         {Yet Another Sokoban Generator for Orimaze Sokoban Levels}

{$IFDEF WINDOWS}
  uses
       Windows;                                {for timing and memorystatus}
{$ENDIF}

{General Constants}

const
  ASTERISK                 = '*';              {ascii characters}
  BACKSLASH                = '\';
  COLON                    = ':';
  COMMA                    = ',';
  CR                       = Chr(13);
  DOUBLE_QUOTE             = '"';
  EQUAL                    = '=';
  HYPHEN                   = '-';
  LAZY_COLON               = '..';
  LEFT_BRACKET             = '[';
  LEFT_PARENTHESIS         = '(';
  LF                       = Chr(10); CRLF = CR+LF;
  NL                       = CRLF; {newline}
//NL                       = {$IFDEF WINDOWS} CRLF; {$ELSE} LF; {$ENDIF} {newline}
  NULL_CHAR                = Chr(0);
  PERCENT                  = '%';
  PERIOD                   = '.';
  QUOTE                    = '''';
  RIGHT_BRACKET            = ']';
  RIGHT_PARENTHESIS        = ')';
  SEMICOLON                = ';';
  SLASH                    = '/';
  SPACE                    = ' ';
  TAB                      = Chr(9);
  UNDERSCORE               = '_';

  ONE_KIBI                 = 1024;                                              {"kibi" used to be 'kilo' before year 2000}
  ONE_THOUSAND             = 1000;

  ONE_MEBI                 = ONE_KIBI*ONE_KIBI;                                 {"mebi" used to be "mega" before year 2000}
  ONE_MILLION              = ONE_THOUSAND*ONE_THOUSAND;

  SIXTEEN_KIBI             = 16*ONE_KIBI;

  WINDOWS_FILE_NAME_PATH_DELIMITER
                           = BACKSLASH;

{Constants and type declarations required before declaring other constants and other types}

type                                           {first some number types}
  Int8                     = ShortInt;         {nobody can remember that 'SmallInt'  means signed   16 bit integer in Delphi 4, hence, this alias comes in handy}
  UInt8                    = Byte;             {'byte' is ambiguous; in Delphi 4 it's an unsigned 8-bit number, but some other languages have different conventions; therefore, the 'UInt8' alias comes in handy}
  Int16                    = SmallInt;         {nobody can remember that 'SmallInt'  means signed   16 bit integer in Delphi 4, hence, this alias comes in handy}
  Int32                    = Integer;          {for historical reasons,  'Integer'   means signed   32 bit integer in Delphi 4, but it's problematic to use a term like that with newer processors}
  PInt32                   = ^Int32;
  UInt16                   = Word;             {for historical reasons,  'Word'      means unsigned 16 bit integer in Delphi 4, but it's problematic to use a term like that with newer processors}
  UInt32                   = Cardinal;         {for historical reasons,  'Cardinal'  means unsigned 32 bit integer in Delphi 4, but it's problematic to use a term like that with newer processors}
  PUInt32                  = ^UInt32;
  UInt                     = Cardinal;
  UInt64                   = Int64;            {Delphi 4: no unsigned 64-bit integer}
  TTimeMS                  = {$IFDEF WINDOWS} DWORD; {$ELSE} UInt32; {$ENDIF}
{$IFDEF X86_32}
  NativeInt                = Int32;
  NativeUInt               = UInt32;
{$ELSE}
  NativeInt                = Int64;
  NativeUInt               = UInt64;
{$ENDIF}
  TSize                    = NativeInt;        {for convenience, 'TSize' is a signed integer, not an unsigned integer. often, calculations are easier this way and require fewer type casts.}

const
  LOG2_BITS_PER_BYTE       = 3;
  BITS_PER_BYTE            = 1 shl LOG2_BITS_PER_BYTE;
  BITS_PER_INTEGER         = BITS_PER_BYTE*SizeOf(Integer);
  BITS_PER_UNSIGNED_INTEGER= BITS_PER_BYTE*SizeOf(UInt);
  MAX_BOARD_WIDTH          = 100;
  MAX_BOX_COUNT            = 4095;             {limited by the slot 'BoxNo' in 'TMove'. must be a 2^N-1 UInt16 number, where 'N' is an integer > 0, so the value can be used as a bit mask to extract a box number or a goal number.}
  MAX_HISTORY_BOX_PUSHES   = High(UInt16)-1;
  MAX_ORIMAZE_BOARD_HEIGHT = 8;
  MAX_ORIMAZE_BOARD_WIDTH  = MAX_ORIMAZE_BOARD_HEIGHT; {width x height must be <= number of bits in Int64}
  MAX_ORIMAZE_PLAYER_START_POSITIONS
                           = ((1+MAX_ORIMAZE_BOARD_WIDTH) div 2)*((1+MAX_ORIMAZE_BOARD_HEIGHT) div 2);
  MAX_SEARCH_STATES        = 10;               {size of stack of timestamp boards for calculating the player's reachable squares in a given game state}

type
  TAxis                    = (aVertical,aHorizontal); {order must not change; the ordinal values must match the ordinal values of the directions 'up' and 'left' respectively}
  TAxisMap                 = array[TAxis] of TAxis;
  TAxisSet                 = set of TAxis;
  TBoxNo                   = 0..MAX_BOX_COUNT;
  PBoxNo                   = ^TBoxNo;
  TColRow                  = packed record Col,Row:Int8; end;
  TDirection               = (dUp,dLeft,dDown,dRight); {order must not change, e.g., the directions must be listed in alternate axis order; see also 'TDeadlockSetFlag' for more constraints}
  TDirectionArrayOfIntegers
                           = array[TDirection] of Integer;
  TDirectionMap            = array[TDirection] of TDirection;
  TDirectionSet            = set of TDirection;
  TMoveFlag                = (mfOpen,mfPath,mfPush,mfVisited);
  TMoveFlags               = set of TMoveFlag;
  TMove                    = packed record
    Direction              : TDirection;
    Flags                  : TMoveFlags;
    case Boolean of
      False : ( BoxNo      : TBoxNo;);
      True  : ( Depth      : UInt16;); {position search depth, used by the iFUB algorithm}
    end;
  THashValue               = UInt64;
  THistory                 = record
    Count                  : Integer;
    Moves                  : array[0..MAX_HISTORY_BOX_PUSHES] of TMove;
                             end;
  TScore                   = UInt16;   {e.g., scores and pushes in a position stored in the transposition table; see 'TPosition'}
  TSearchLimits            = record    {limits controlling the currently running search; solving or optimizing a level may require multiple searches with different limits}
    DepthLimit             : Integer;  {maximum search depth}
    PushCountLimit         : Integer;  {maximum number of pushes}
    TimeLimitMS            : TTimeMS;  {maximum search time, milliseconds}
                             end;
  TTimeStamp               = UInt32;

  {2D transformations}
  TTransformation2D        = (t2DRotate0DegreesClockwise, {order must not change}
                              t2DRotate90DegreesClockwise,
                              t2DRotate180DegreesClockwise,
                              t2DRotate270DegreesClockwise,
                              t2DRotate0DegreesClockwiseFlipHorizontally,
                              t2DRotate90DegreesClockwiseFlipHorizontally,
                              t2DRotate180DegreesClockwiseFlipHorizontally,
                              t2DRotate270DegreesClockwiseFlipHorizontally //,
                              //t2DFlipVertically,
                              //t2DFlipHorizontally,
                              //t2DMirrorDiagonallyTopLeftBottomRight,
                              //t2DMirrorDiagonallyTopRightBottomLeft
                              );
  TBoardTransformation2D   =  t2DRotate0DegreesClockwise.. {order must not change}
                              t2DRotate270DegreesClockwiseFlipHorizontally;

{Constants}

const
  BOX                      = 1;                {board, internal representation}
  FLOOR                    = 2;
  GOAL                     = 4;
  PLAYER                   = 8;
  WALL                     = 16;
  BOARD_PIECES             = BOX + FLOOR + GOAL + PLAYER + WALL;

  FLAG_BOX_REACHABLE_SQUARE= 32;               {flags used for each square}
  FLAG_BOX_START_SQUARE    = 64;
  FLAG_DOOR_SQUARE         = 128;              {a floor with neighboring floors on both sides along one axis; all other neighbor squares are walls}
  FLAG_EXTERIOR_WALL       = 256;
  FLAG_GATE_SQUARE         = 512;              {a floor with neighboring floors on both sides along at least one axis; splits the board in separate rooms}
  FLAG_ILLEGAL_BOX_SQUARE  = 1024;
  FLAG_INVISIBLE_WALL      = 2048;             {visually the square is a floor, but functionally it's a wall}
  FLAG_NOT_SELECTED_SQUARE = 4096;
  FLAG_SQUARE_SET          = 8192;
  FLAG_VISITED_SQUARE      = 65536;
  BOARD_SQUARE_FLAGS       = FLAG_BOX_REACHABLE_SQUARE+FLAG_BOX_START_SQUARE+FLAG_DOOR_SQUARE+FLAG_GATE_SQUARE+FLAG_ILLEGAL_BOX_SQUARE+
                             FLAG_NOT_SELECTED_SQUARE+FLAG_SQUARE_SET+
                             FLAG_VISITED_SQUARE; //+FLAG_WALL_GATE_SQUARE;

  HORIZONTAL               = BOX;  {Orimaze board}
  VERTICAL                 = GOAL; {Orimaze board}

  CH_BOX                   = 'b';   CH_BOX_ON_GOAL           = 'B'; {all board characters and move characters must be 7-bit ASCII characters}
  CH_BOX_XSB               = '$';   CH_BOX_ON_GOAL_XSB       = '*';
  CH_GOAL                  = '.';
  CH_FLOOR                 = SPACE; CH_NON_BLANK_FLOOR       = '-';
  CH_PLAYER                = 'p';   CH_PLAYER_ON_GOAL        = 'P';
  CH_PLAYER_XSB            = '@';   CH_PLAYER_ON_GOAL_XSB    = '+';
  CH_SQUARE_SET            = '%';
  CH_WALL                  = '#';

  CH_ORIMAZE_PLAYER        = ASTERISK;

  ALL_AXES_SET             : TAxisSet = [aVertical,aHorizontal];
  ALL_DIRECTIONS_SET       : TDirectionSet = [dUp,dLeft,dDown,dRight]; {caution: after adding more directions to 'TDirection' (e.g., for the Hexoban variant), this constant must be updated accordingly}
  AXIS_COUNT               = Succ(Ord(High(TAxis))-Ord(Low(TAxis))); {number of axis}
  AXIS_TO_DIRECTION        : array[TAxis] of TDirection
                           = (dUp, dLeft);
  AXIS_TO_TEXT             : array[TAxis] of String
                           = ('vertical','horizontal');

  COMMAND_FILE_EXT         = '.bat';
  COMPASS_DIRECTIONS       : array [TDirection] of TColRow = (  (Col: 0; Row: -1), (Col: -1; Row: 0), (Col: 0; Row: 1), (Col: 1; Row: 0) ); {up, left, down, right}
  DEADLOCK_SCORE           = High(TScore);               {must be 'TScore' high value}
  DEAD_END_SCORE           = DEADLOCK_SCORE-1;           {must be 1 less than 'DEADLOCK_SCORE'}
  DEFAULT_LOG_FILE_ENABLED = True;
  DEFAULT_MEMORY_BYTE_SIZE_SMALL
                           = 10*ONE_MEBI;                {10  MiB}
  DEFAULT_MEMORY_BYTE_SIZE_LARGE
                           = 200*ONE_MEBI;               {200 MiB}
  DEFAULT_OPEN_LIST_ESCAPE_INTERVAL
                           = 7;                          {must be a 2^N-1 integer, where N is a positive integer}
  DEFAULT_ORIMAZE_BOARD_HEIGHT
                           = 5;
  DEFAULT_ORIMAZE_BOARD_WIDTH
                           = DEFAULT_ORIMAZE_BOARD_HEIGHT;
  DEFAULT_PUSH_COUNT_LIMIT = High(Integer);              {number of pushes to generate before giving up}
  DEFAULT_REUSE_NODES_ENABLED
                           = False;                      {'True': simple-memory-bounded search (SMA) (it has not been tested for a long time, and it may need refreshment before it works again}
  DEFAULT_SEARCH_DEPTH     = MAX_HISTORY_BOX_PUSHES;     {caution: texts in 'ShowHelp' must change if the value differs from the maximum value}
  DEFAULT_SEARCH_TIME_LIMIT_MS
                           = 10 {minutes} * 60 {seconds} * 1000 {milliseconds};
  DEFAULT_STOP_WHEN_SOLVED = True;                       {'True': stop when a solution has been found, i.e., don't search for shorter solutions}
  DIRECTION_BIT_COUNT      = 2;                          {caution: must cover the number of directions}
  DIRECTION_COUNT          = Succ(Ord(High(TDirection))-Ord(Low(TDirection))); {number of directions}
  DIRECTION_TO_AXIS        : array[TDirection] of TAxis
                           = (aVertical,aHorizontal,aVertical,aHorizontal);
  DIRECTION_TO_CHAR        : array[TDirection] of Char
                           = ('u','l','d','r');          {caution: must be lowercase, otherwise modify 'CharToDirection'}
  DIRECTION_TO_TEXT        : array[TDirection] of String
                           = ('up','left','down','right');
  {caution: if more directions are added to 'TDirection' (e.g., for the Hexoban variant), then 'DIRECTION_TO_TUNNEL_FLAGS' must be updated accordingly}
  FLAG_POSITION_BASE       = 1;
  FLAG_POSITION_VISITED    = 2;
  GAME_LINE_LENGTH         = 70;               {print line length for games, i.e., solutions and snapshots}
  GOAL_BIT_SHIFT_COUNT     = 16;               {each square on the board contains its goal number in the upper bits}
  INFINITY                 = (MaxInt div 2)-1; {(-INFINITY), (2*INFINITY), and (2*(-INFINITY)) must be legal integer values}
  LARGE_ORIMAZE_PUZZLE_THRESHOLD_SQUARES       {Orimaze puzzle size >= threshold: use the large transposition table default byte size}
                           = 7*5;              {Orimaze 7x5 needs the large transposition table byte size}
  SOKOBAN_FILE_EXT         = '.sok';
  LOG_FILE_EXT             = ' (in progress).sok'; {the log file can be loaded as a normal Sokoban puzzle file}
  MAX_BOARD_HEIGHT         = MAX_BOARD_WIDTH;
  MAX_BOARD_SIZE           = (MAX_BOARD_WIDTH+2) * (MAX_BOARD_HEIGHT+2); {'0' left/top border; '+1': right/bottom border}
  MAX_BOX_COUNT_FOR_CALCULATING_MINIMUM_LOWER_BOUND
                           = 100;              {with more boxes it may take a long time to calculate a lower bound}
  MAX_HISTORY_PLAYER_MOVES = Pred(Pred(High(TScore))); {limited by the slot 'Height' in 'TPosition' and the constant values 'DEADLOCK_SCORE' and 'DEAD_END_SCORE'}
  MAX_LOGGED_BEST_PUZZLES_COUNT
                           = 20;
  MAX_ORIMAZE_BOARD_SIZE   = MAX_ORIMAZE_BOARD_WIDTH*MAX_ORIMAZE_BOARD_HEIGHT;  {must be <= the maximum number of boxes because they share data structures}
  MAX_SEARCH_TIME_LIMIT_MS = High(Integer)-1001; {'-1001': high-values are reserved for meaning 'unlimited'}
  MAX_SINGLE_STEP_MOVES    = MAX_BOARD_WIDTH*MAX_BOARD_HEIGHT+DIRECTION_COUNT*MAX_BOX_COUNT;
  MAX_TRANSPOSITION_TABLE_BYTE_SIZE            {limit the transposition table byte size to a signed native integer, and leave some memory free for other purposes}
                           : TSize
                           = (High(NativeInt) div 4)*3; {'High(NativeInt)', not 'High(TSize)'}
  MIN_BOARD_HEIGHT         = 3; {must be >= 3}
  MIN_BOARD_WIDTH          = 3; {must be >= 3}
  NEXT_DIRECTION           : TDirectionMap {next anti-clockwise direction}
                           = (dLeft,dDown,dRight,dUp);
  OPPOSITE_AXIS            : TAxisMap
                           = (aHorizontal,aVertical);
  OPPOSITE_DIRECTION       : TDirectionMap
                           = (dDown,dRight,dUp,dLeft);

  PLAYER_POSITION_MOVE_AXIS= 1 shl (BITS_PER_BYTE*SizeOf(UInt16)-1);            {the high-bit in 'TPosition.PlayerPos' is used for the move-axis}
  PLAYER_POSITION_MASK     = PLAYER_POSITION_MOVE_AXIS-1;

  PLAYERS_REACHABLE_SQUARES_TIMESTAMP_UPPER_BOUND                   {timestamp upper bound; timestamps wrap around when the upper bound is reached; at that time the individual squares are reset to '0' for floors and 'high-value' for wall squares}
                           = High(TTimeStamp)-(2*5*MAX_BOARD_SIZE); {'*5': must be >= '*5' = '*(1+number of directions)' to ensure that 'CalculatePlayersDistanceToReachableSquares' works correctly}
                                                                    {'*2': so 'CalculatePlayersReachableSquares' starting with cleared timestamps can visit all player access areas without a timestamp wrap around}
  PREVIOUS_DIRECTION       : TDirectionMap {previous anti-clockwise direction}
                           = (dRight,dUp,dLeft,dDown);
  SOKOBAN_TEMPLATE         : array[1..57] of String =               {must match 'SOKOBAN_TEMPLATE_GRID_POINTS'. must be a template for an Orimaze puzzle with even dimensions. additional constraints apply for duplicating columns and rows.}
  (
    '#########################################################',
    '#@  *  *   * * *   *  *   * * *   *  *   * * *   *  *   #',
    '# # ## # #   #   # ## # #   #   # ## # #   #   # ## # # #',
    '#          #####          #####          #####          #',
    '#*# #### #*#   #*# #### #*#   #*# #### #*#   #*# #### #*#',
    '#   #  # #   #   # #  # #   #   # #  # #   #   # #  # # #',
    '# # #  #   #####   #  #   #####   #  #   #####   #  #   #',
    '#*# #### #*#   #*# #### #*#   #*# #### #*#   #*# #### #*#',
    '#          #####          #####          #####          #',
    '# # ## # #   #   # ## # #   #   # ## # #   #   # ## # # #',
    '#   *  *   * * *   *  *   * * *   *  *   * * *   *  *   #',
    '#* ## ### *#####* ## ### *#####* ## ### *#####* ## ### *#',
    '#  #  # #  #   #  #  # #  #   #  #  # #  #   #  #  # #  #',
    '#*## ## ##*#   #*## ## ##*#   #*## ## ##*#   #*## ## ##*#',
    '#  #  # #  #   #  #  # #  #   #  #  # #  #   #  #  # #  #',
    '#* ## ### *#####* ## ### *#####* ## ### *#####* ## ### *#',
    '#   *  *   * * *   *  *   * * *   *  *   * * *   *  *   #',
    '# # ## # #   #   # ## # #   #   # ## # #   #   # ## # # #',
    '#          #####          #####          #####          #',
    '#*# #### #*#   #*# #### #*#   #*# #### #*#   #*# #### #*#',
    '#   #  # #   #   # #  # #   #   # #  # #   #   # #  # # #',
    '# # #  #   #####   #  #   #####   #  #   #####   #  #   #',
    '#*# #### #*#   #*# #### #*#   #*# #### #*#   #*# #### #*#',
    '#          #####          #####          #####          #',
    '# # ## # #   #   # ## # #   #   # ## # #   #   # ## # # #',
    '#   *  *   * * *   *  *   * * *   *  *   * * *   *  *   #',
    '#* ## ### *#####* ## ### *#####* ## ### *#####* ## ### *#',
    '#  #  # #  #   #  #  # #  #   #  #  # #  #   #  #  # #  #',
    '#*## ## ##*#   #*## ## ##*#   #*## ## ##*#   #*## ## ##*#',
    '#  #  # #  #   #  #  # #  #   #  #  # #  #   #  #  # #  #',
    '#* ## ### *#####* ## ### *#####* ## ### *#####* ## ### *#',
    '#   *  *   * * *   *  *   * * *   *  *   * * *   *  *   #',
    '# # ## # #   #   # ## # #   #   # ## # #   #   # ## # # #',
    '#          #####          #####          #####          #',
    '#*# #### #*#   #*# #### #*#   #*# #### #*#   #*# #### #*#',
    '#   #  # #   #   # #  # #   #   # #  # #   #   # #  # # #',
    '# # #  #   #####   #  #   #####   #  #   #####   #  #   #',
    '#*# #### #*#   #*# #### #*#   #*# #### #*#   #*# #### #*#',
    '#          #####          #####          #####          #',
    '# # ## # #   #   # ## # #   #   # ## # #   #   # ## # # #',
    '#   *  *   * * *   *  *   * * *   *  *   * * *   *  *   #',
    '#* ## ### *#####* ## ### *#####* ## ### *#####* ## ### *#',
    '#  #  # #  #   #  #  # #  #   #  #  # #  #   #  #  # #  #',
    '#*## ## ##*#   #*## ## ##*#   #*## ## ##*#   #*## ## ##*#',
    '#  #  # #  #   #  #  # #  #   #  #  # #  #   #  #  # #  #',
    '#* ## ### *#####* ## ### *#####* ## ### *#####* ## ### *#',
    '#   *  *   * * *   *  *   * * *   *  *   * * *   *  *   #',
    '# # ## # #   #   # ## # #   #   # ## # #   #   # ## # # #',
    '#          #####          #####          #####          #',
    '#*# #### #*#   #*# #### #*#   #*# #### #*#   #*# #### #*#',
    '#   #  # #   #   # #  # #   #   # #  # #   #   # #  # # #',
    '# # #  #   #####   #  #   #####   #  #   #####   #  #   #',
    '#*# #### #*#   #*# #### #*#   #*# #### #*#   #*# #### #*#',
    '#          #####          #####          #####          #',
    '# # ## # #   #   # ## # #   #   # ## # #   #   # ## # # #',
    '#   *  *   * * *   *  *   * * *   *  *   * * *   *  *   #',
    '#########################################################'
  );
  SOKOBAN_TEMPLATE_GRID_POINTS                                                  {must match 'SOKOBAN_TEMPLATE'}
                           : array[0..8] of Byte = (2,11,17,26,32,41,47,56,56);

  SOKOBAN_PLUGIN_STATUS_TEXT_BUFFER_SIZE
                           = 256;                                               {must match the Sokoban plugin specification; includes a null-character terminator}
  {SOKOBAN_FILE_EXT        = '.sok';}
  SOKOBAN_PLUGIN_CALL_BACK_FUNCTION_THRESHOLD_TIME_MS
                           = 3 {seconds} * 1000;                                {milliseconds}
  TEXT_FILE_EXT            = '.txt';

  TT_AVERAGE_BUCKET_SIZE   = 16; {transposition table: on average, each lookup visits half the number of items in a bucket}

{Texts}

  TEXT_AD_HOC              = 'Ad hoc';
  TEXT_APPLICATION_COPYRIGHT
                           = 'Copyright (c) 2017 by Brian Damgaard';
  TEXT_APPLICATION_TITLE   = 'YASGen Orimaze';
  TEXT_APPLICATION_TITLE_LONG
                           = TEXT_APPLICATION_TITLE+' - Yet Another Sokoban Level Generator for Orimaze Sokoban Levels';
  TEXT_APPLICATION_VERSION_NUMBER
                           = '1.9';
  TEXT_BEST_RESULT_SO_FAR  = 'Best result so far';
  TEXT_DEPTH               = 'depth';
  TEXT_DEPTH_LIMIT_EXCEEDED= 'Depth limit exceeded';
  TEXT_END                 = 'End';
  TEXT_ERROR               = 'Error';
  TEXT_FILE_IO_ERROR       = 'File input/output error';
  TEXT_FILE_OPEN_ERROR     = 'File open error (Check path, maybe file wasn''t found): ';
  TEXT_FILE_READ_ERROR     = 'File read error';
  TEXT_FILE_RENAME_ERROR   = 'Rename file failed';
  TEXT_FORWARD_SEARCH      = 'Forward search';
  TEXT_GENERATED_PUZZLES   = 'Generated puzzles';
  TEXT_HEIGHT              = 'Height';
  TEXT_HISTOGRAM_FOR_GENERATED_PUZZLES
                           = 'Moves histogram for generated puzzles';
  TEXT_INTERNAL_ERROR      = 'Internal Error';
  TEXT_LEGAL_PLAYER_POSTIONS
                           = 'Legal player position indices for the given board width and height';
  TEXT_LEVEL               = 'Level';
  TEXT_LEVEL_HAS_TOO_MANY_BOXES_AND_GOALS
                           = 'Level has too many boxes/goals';
  TEXT_LEVEL_HAS_TOO_MANY_COLUMNS
                           = 'Level has too many columns';
  TEXT_LEVEL_HAS_TOO_MANY_ROWS
                           = 'Level has too many rows';
  TEXT_LEVEL_HAS_NO_PLAYER = 'Level has no player';
  TEXT_LEVEL_HAS_NOT_THE_SAME_NUMBER_OF_BOXES_AND_GOALS
                           = 'Level has not the same number of boxes and goals';
  TEXT_LEVEL_NOT_AN_ORIMAZE_LEVEL
                           = 'Level is not an Orimaze level';
  TEXT_LEVEL_SOLVED        = 'Level solved';
  TEXT_LEVEL_SOLVED_MAY_NOT_BE_OPTIMAL
                           = TEXT_LEVEL_SOLVED+' (solution may not be optimal)';
  TEXT_LEVEL_UNSOLVABLE    = 'Level not solvable';
  TEXT_LEVEL_UNSOLVED      = 'Level not solved';
  TEXT_LOAD_LEVEL_FROM_FILE
                           = 'Load level from file';
  TEXT_MAXIMUM_GENERATED_POSITIONS_FOR_A_PUZZLE
                           = 'Maximum generated positions for a puzzle';
  TEXT_MAXIMUM_ORIMAZE_BOARD_SIZE
                           = 'Implementation limitation: Maximum Orimaze board size, squares';
  TEXT_MEBI_BYTES          = 'MiB';
  TEXT_MEMORY_FULL         = 'Memory full';
  TEXT_MORE_PUZZLES_OF_LENGTH
                           : array[Boolean] of String
                           = ('More puzzles with this solution length has been generated, possibly duplicates',
                              'There are more Orimaze puzzles with solution length');
  TEXT_MOVES               : array[Boolean] of String
                           = ('move','moves'); {singular, plural}
  TEXT_NO_FILE_ERROR       = 'No file';
  TEXT_NO_YES              : array[Boolean] of String = ('No','Yes');
  TEXT_OF                  = 'of';
  TEXT_OPEN                = 'Open';
  TEXT_ORIMAZE             = 'Orimaze';
  TEXT_PLAYER              = 'Player';
  TEXT_PLAYER_             = 'player';
  TEXT_PLAYER_POSITIONS    = 'Player positions';
  TEXT_PLAYER_POSITION_INDEX
                           = 'Player Position Index';
  TEXT_PRESS_ENTER         = 'Press [Enter]';
  TEXT_PUSHES              = 'Pushes';
  TEXT_PUSHES_LIMIT_EXCEEDED
                           = 'Pushes limit exceeded';
  TEXT_PUZZLES_DISCARDED_BY_CLUSTER_FILTER
                           = 'Puzzles discarded because their longest paths were known to fall short';
  TEXT_SAVE_POSITIONS_TO_DISK
                           = 'Saving positions to disk...';
  TEXT_SEARCH_DEPTH        = 'Depth: ';
  TEXT_SECONDS             : array[Boolean] of String
                           = ('second', 'seconds'); {singular, plural}
  TEXT_SEGMENT             = 'Segment';
  TEXT_SEGMENT_TOO_LARGE   = 'Segment too large';
  TEXT_SOLUTION            = 'Solution';
  TEXT_SOLUTION_           = 'solution';
  TEXT_SOLUTIONS           = 'Solutions';
  TEXT_SOLUTION_INFO_1     = 'Solution: '; {number of pushes is inserted here}
  TEXT_SOLUTION_INFO_2     = ' pushes. Search for a shorter solution continues.';
  TEXT_START               = 'Start';
  TEXT_STATISTICS          = 'Statistics';
  TEXT_STATISTICS_EXCLUDE_SYMMETRICAL_DUPLICATES
                           = 'The statistics do not include symmetrical duplicates after rotation and reflection';
  TEXT_STATISTICS_FILENAME_SUFFIX
                           =', Statistics.txt';
  TEXT_TERMINATED_BY_USER  = 'Terminated by user';
  TEXT_TIME                = 'Time';
  TEXT_TIME_LIMIT_EXCEEDED = 'Time-limit exceeded';
  TEXT_TOTAL               = 'Total';
  TEXT_TRIVIAL_SOLUTION_1  = 'This level has a trivial 0-push solution.';
  TEXT_TRIVIAL_SOLUTION_2  = 'The search continues for a nontrivial solution.';
  TEXT_VERSION             = 'Version';
  TEXT_VISITED_ORIMAZE_PUZZLES_TABLE
                           = 'Visited Orimaze puzzles table';
  TEXT_WIDTH               = 'Width';
{Types}

type
  TByteVector              = array[0..High(TSize)-1] of Byte;
  PByteVector              = ^TByteVector;
  TBoardSquareValue        = UInt; {the contents of a board square, as opposed to a square index}
  TBoard                         = array[0..MAX_BOARD_WIDTH +1,
                                         0..MAX_BOARD_HEIGHT+1] of TBoardSquareValue;
//TBoard                   = array[0..MAX_BOARD_SIZE]   of TBoardSquareValue;
  TBoardAsTextLines        = array[0..MAX_BOARD_HEIGHT] of String[255]; //String[MAX_BOARD_WIDTH];
  TBoardOfBooleans         = array[0..MAX_BOARD_WIDTH +1,
                                   0..MAX_BOARD_HEIGHT+1] of Boolean;
  TBoardOfBoxNumbers       = array[0..MAX_BOARD_SIZE]   of TBoxNo;
  TBoardOfBytes            = array[0..MAX_BOARD_SIZE]   of UInt8;
  TBoardOfDirections       = array[0..MAX_BOARD_SIZE]   of TDirection;
  TBoardOfDirectionSets    = array[0..MAX_BOARD_SIZE]   of TDirectionSet;
  TBoardOfIntegers         = array[0..MAX_BOARD_SIZE]   of Integer;
  TBoardOfTimeStamps       = array[0..MAX_BOARD_WIDTH +1,
                                   0..MAX_BOARD_HEIGHT+1] of TTimeStamp;
  TBoardOfUnsignedIntegers = array[0..MAX_BOARD_SIZE]   of UInt;
  TBoardSquareSet          = record
    Count                  : Integer;
    Squares                : TBoardOfIntegers;
                             end;
  TBoardTimeStamps         = record
    Squares                : TBoardOfTimeStamps;
    TimeStamp              : TTimeStamp;
                             end;
  TBoxArrayOfIntegers      = array[TBoxNo] of Integer;
  TBoxArrayOfUnsignedIntegers
                           = array[TBoxNo] of Cardinal;
  TBoxNumbers              = array[TBoxNo] of TBoxNo;
  TBoxSquare               = Integer; {must be a signed integer-type value so negated values can indicate a flag value for the square}
  PBoxSquare               = ^TBoxSquare;
  TBoxSquare2              = Int16; {for compact box square vectors, e.g., TCorralPositions.BoxSquare'}
  PBoxSquare2              = ^TBoxSquare2;
  TBoxSquares2             = array[TBoxNo] of TBoxSquare2;
  TBoxSquares              = array[TBoxNo] of TBoxSquare;
  PBoxSquares              = ^TBoxSquares;
  TBoxSquaresMemoryBlock   = record {corral box squares stored in the transposition table together with the normal game positions}
    BoxSquare              : PBoxSquare2; {next free box square       in the memory block allocated for the box squares}
    BoxSquaresCountDown    : Integer;     {number of free box squares in the memory block allocated for the box squares}
                             end;
  TBoxNumberSet            = record
    Count                  : Integer;
    Numbers                : TBoxNumbers;
                             end;
  TBoxSquareSet            = record
    Count                  : Integer;
    Squares                : TBoxSquares;
                             end;
  TBoxSquareValues         = array[TBoxNo] of TBoardSquareValue; {board square values for the boxes, as opposed to box square indices}
  TGoalSquareValues        = TBoxSquareValues;
  TBoxSquareValueSet       = record
    Count                  : Integer;
    Values                 : TBoxSquareValues;
                             end;
  TGoalSquareValueSet      = TBoxSquareValueSet;
  TSquareDirectionArrayOfInteger
                           = array[0..MAX_BOARD_SIZE,TDirection] of Integer;
  TNeighborSquareSet       = record
    Count                  : Integer;
    Squares                : array[0..DIRECTION_COUNT] of Integer; {element 0 not used}
    end;
  TOrimazeBoardSquareValue = Byte;
  POrimazeBoardSquareValue = ^TOrimazeBoardSquareValue;
  TOrimazeBoard            = array[0..MAX_ORIMAZE_BOARD_SIZE] of TOrimazeBoardSquareValue;
  POrimazeBoard            = ^TOrimazeBoard;
  TOrimazeBitBoard         = Int64; {kludge: Delphi 4 doesn't have unsigned 64-bit integers, hence, it's easiest to code the entire application with bit boards represented by signed 64-bit integers}
  POrimazeBitBoard         = ^TOrimazeBitBoard;
  TGame                    = record
    Board                  : TBoard; {current game state; 'Board' = 'BoxPos' + 'PlayerPos'}
    BoardHeight            : Integer;
    BoxReachableSquaresCount {number of squares on the board which at least one of the boxes can reach}
                           : Integer;
    BoardSize              : Integer; {(Width+2)*(Height*2): the extra 2 is for a wall-filled border}
    BoardWidth             : Integer;
    BoxCount               : Integer;
    BoxPos                 : TBoxSquares; {current game state; 'Board' = 'BoxPos' + 'PlayerPos'}
    DistanceToNearestGoal  : TBoardOfIntegers;
    EndPlayerPos           : Integer; {player's end position in the original snapshot/solution}
    FloorCount             : Integer; {number of floor squares on the board}
    GoalCount              : Integer;
    GoalPos                : TBoxSquares;
    HashValue              : THashValue;
    History                : THistory;
    InitializationTimeMS   : TTimeMS; {'Game.DeadlocksSet.TimeMS'}
    IsAnOptimalSolution    : Boolean; {is the game stored in 'History' an optimal solution?}
    OriginalBoard          : TBoard;  {original board, i.e., before pre-processing operations like tube-filling}
    OriginalBoxPos         : TBoxSquares; {matching 'OriginalBoard'}
    OriginalPlayerPos      : Integer; {matching 'OriginalBoard'}
    OriginalSolution       : String;  {solution from inputfile, not the solution found by this application}
    OriginalSolutionMoveCount
                           : Integer;
    OriginalSolutionPushCount
                           : Integer;
    OrimazeBoardHeight     : Integer;
    OrimazeBoardSize       : Integer;
    OrimazeBoardSquareNeighbors
                           : array[0..MAX_ORIMAZE_BOARD_SIZE-1,TDirection] of Integer;
    OrimazeBoardSquare2DTransformations
                           : array[0..MAX_ORIMAZE_BOARD_SIZE-1,TBoardTransformation2D] of Byte;
    OrimazeBoardWidth      : Integer;
    OrimazeGridSquares     : TBoxSquareSet;
    OrimazeSolutionBoard   : TOrimazeBoard;
    OrimazeStartBoard      : TOrimazeBoard;
    PlayerPos              : TColRow; {current game state; 'Board' = 'BoxPos' + 'PlayerPos'}
    SimpleLowerBound       : Integer;
    StartBoxPos            : TBoxSquares; {box starting positions after board normalization by 'FillTubes'}
    StartPlayerPos         : Integer; {player's starting position after board normalization by 'FillTubes'}
    SolutionPathHashValues : array[0..MAX_HISTORY_BOX_PUSHES+1] of THashValue;
    SquareBoxNo            : TBoardOfBoxNumbers; {maps square number to the number of the box at the square, if any}
    Title                  : String;
    TubeFillingMoveCount   : Integer; {player moves}
    TubeFillingPlayerLines : Integer; {player lines}
    TubeFillingPushCount   : Integer; {box pushes}
                             end;
  PPosition                = ^TPosition;
  PPPosition               = ^PPosition;
  THashValueVector         = array[ 0 .. ( High( TSize ) div SizeOf( THashValue ) ) - 1 ] of THashValue;
  PHashValueVector         = ^THashValueVector;
  TLegend                  = record
    CharToItem             : array[Low(Char)..High(Char)] of UInt8;
    XSBNotation            : Boolean;
                             end;
  TLogFile                 = record
    Enabled                : Boolean;
    FileName               : String;  {a non-blank name signals that logging really is active, i.e., the file is open and no file-i/o errors have occurred}
    TextFile               : TextFile;
                             end;
  TPositionLinks           = record         {the sequence of the fields must not change}
    Prev                   : PPosition;
    Next                   : PPosition;
                             end;
  TPositionType            = (ptNull,ptOpen,ptClosed,ptList);
  TPosition                = packed record  {'packed': literal sequence of unaligned slots, without gaps (they must be carefully aligned manually)}
    HashValue              : THashValue;    {must be first slot for fast addressing}
    HashBucket             : TPositionLinks;{double-linked list of positions that hash into the same bucket. the positions in the bucket are sorted in ascending order on 'HashValue'}
    Parent                 : PPosition;     {predecessor position}
    Move                   : TMove;         {the move leading to this position}
    PlayerPos              : UInt16;
    PushCount              : TScore;        {for the solver, this is the also the search-depth; for the optimizer it's only a metric}
    Eccentricity           : TScore;        {'>0:' the eccentricity (the longest outgoing acyclic path) has been calculated for this position. the position farthest away from this position is stored in 'HashBucket.Prev'}
    SuccessorCount         : UInt16;        {number of immediate successors stored in the transposition table, i.e., nodes having this node as parent}
    case TPositionType of                   {node-type dependent information for open nodes and closed nodes}
      ptNull:
        ();
      ptOpen:                               {open nodes, i.e., nodes on the open-list; precondition: open nodes have no successors, i.e., no 'Parent' pointers back to an open node}
        (ScoreBucket       : TPositionLinks;{double-linked list of positions on the open-list having the same score}
        );
      ptClosed:                             {closed nodes (interior nodes), i.e., nodes stored in the transposition table but not on the open-list}
        (BestForgottenScore: TScore;        {best score for pruned successors, if any}
         Unused6           : UInt16;
         Unused7           : Pointer;);
      ptList:                               {list of transposition table items}
        (ListLinks         : TPositionLinks;{double-linked list. must match the 'ScoreBucket' links}
        );
    end;
  TPositionPointersVector  = array[0..(High(TSize) div SizeOf(PPosition))-1] of PPosition;
  PPositionPointersVector  = ^TPositionPointersVector;
  TPositionVector          = array[0..(High(TSize) div SizeOf(TPosition))-1] of TPosition;
  PPositionVector          = ^TPositionVector;
  TOpenPositions           = record
    Buckets                : array[0..MAX_HISTORY_BOX_PUSHES+(1+MAX_BOX_COUNT)] of PPosition; {bucket-sorting open positions means that the sort is O(1), i.e., as fast as possible}
    Count                  : TSize;     {number of open positions}
    MaxValue               : Integer;   {highest stored score}
    MinValue               : Integer;   {lowest  stored score}
                             end;
  TVisitedOrimazePuzzles   = record     {visited clusters of Orimaze puzzle start positions}
    BitVectorByteSize      : TSize;     {the size of a bit vector allocated for a player start position. see also 'MemoryByteSize'}
    ClusterCount           : UInt64;    {statistics}
    Count                  : UInt64;    {statistics}
    MemoryByteSize         : TSize;     {all allocated bit vectors for player start positions. see also 'BitVectorByteSize'}
    Positions              : array[0..MAX_ORIMAZE_BOARD_SIZE-1] of PByteVector; {bit vectors. 0 = unvisited, 1 = visited.}
                             end;
  TTranspositionTableSearchStatistics
                           = record     {statistics and debugging info}
    BackwardMoveCount      : TSize;
    BackwardPositionCount  : TSize;
    BackwardPullCount      : TSize;
    DroppedCount           : TSize;     {number of dropped positions}
    DuplicatesCount        : TSize;
    EnqueuedPositionsCount : array[Boolean] of TSize; {'True' counter: the player's reachable squares have been calculated}
    Enqueue2Count          : TSize;
    ForwardPositionCount   : TSize;
    Lookup1Count           : TSize;
    Lookup2Count           : TSize;
    Lookup3Count           : TSize;
    MoveCount              : TSize;
    NewBestPositionCount   : TSize;
    NewPathCount           : TSize;
    ReuseCount             : TSize;
    SetPosition1Count      : TSize;
    SetPosition2Count      : TSize;
                             end;
  TTimeStampVector         = array[0..(High(TSize) div SizeOf(TTimeStamp))-1] of TTimeStamp;
  PTimeStampVector         = ^TTimeStampVector;
  TPositions               = record      {transposition-table and open-queue}
    BestPosition           : PPosition;  {the first found position with the best score}
    BestPositionCount      : Cardinal;   {the number of positions with the best score}
    BestScore              : Cardinal;   {best score}
    Capacity               : TSize;      {size of the transposition table measured in 'TPosition' records by the solver, and measured in 'TOptimizerPosition' records by the optimizer}
    Count                  : TSize;      {number of game positions stored in the transposition table}
    CurrentPosition        : PPosition;  {current position during the search, i.e., the game board state matches this position}
    DebugHashValueCount    : TSize;
    DebugHashValueIndex    : TSize;
    DebugHashValues        : PHashValueVector;
    DebugPosition          : PPosition;  {global variable for debugging}
    DebugPositionScore     : TScore;
    FreeList               : PPosition;  {linked list of unused positions}
    HashBucketCount        : TSize;      {vector-length of 'HashBuckets'; it must must be a power of 2 so masking with ('HashBucketCount'-1) produces a valid hash-bucket index}
    HashBucketMask         : TSize;      {'HashBucketCount' - 1}
    HashBucketTimeStamp    : TTimeStamp;
    HashBucketTimeStamps   : PTimeStampVector; {one timestamp for each bucket. this makes zero-filling of the bucket vector unnecessary when the transposition table is initialized.}
    HashBuckets            : PPositionPointersVector; {the vector length must be a power of 2}
    HighWaterMark          : Pointer;    {address past end of the transposition table items in the 'Positions' vector; starting from that address, the memory block contains reserved areas like the hash buckets and the precalculated deadlock-sets}
    MaxPositionCount       : TSize;      {statistics}
    MemoryByteSize         : TSize;      {total memory size in bytes, including 'Positions', 'HashBuckets' and precalculated deadlock-sets}
    OpenPositions          : TOpenPositions;
    Positions              : PPositionVector;
    SearchStatistics       : TTranspositionTableSearchStatistics;
    SolutionPosition       : PPosition;
    StartPosition          : PPosition;  {start position for the search}
    SquareHashValues       : array[0..MAX_BOARD_SIZE] of THashValue; {constants for calculating Zobrist hash-keys}
    UninitializedItemCount : TSize;      {start-value = 'Capacity'}
    VisitedOrimazePuzzles  : TVisitedOrimazePuzzles;
                             end;
  TRandomState             = record RandomNumber:Integer; end;
  TReader                  = record
    CurrentTextLine        : String;
    FirstLevelNo           : Cardinal;
    LastLevelNo            : Cardinal;
    InputFile              : TextFile;
    InputFileName          : String;
    LevelCount             : Cardinal;
    PreviousTextLine       : String;    {not always the previous line; for instance, it may also be the most recently parsed line which might be a game title}
                             end;
  TPlayersReachableSquares = record
    Calculated             : Boolean;   {updated by 'CalculatePlayersReachableSquares'; the caller is responsible for proper initialization before calling 'TTAdd' and 'TTlookup'}
    MinPlayerPos           : Integer;   {the top-left reachable square is used as normalized player position}
    Squares                : TBoardOfTimeStamps; {the player's reachable squares have the value 'TimeStamp'; squares with neighboring boxes have the value 'TimeStamp' + 1}
    TimeStamp              : TTimeStamp;{incremented by 2 on each call to 'CalculatePlayersReachableSquares'}
                             end;
  TSearchState             = record     {the solver is recursive but the machine stack is limited to 1 MiB, hence, large tables must be allocated statically outside the stack}
    case Boolean of False  : (PlayersReachableSquares : TPlayersReachableSquares);
                             end;
  TSingleStepMoves         = array[0..MAX_SINGLE_STEP_MOVES] of TMove;
  TSokobanCallBackFunction = function:Integer; stdcall;
  TSokobanStatus           = packed record {for plugin interface functions like "SolveEx" and its accompanying call-back function}
    Size                   : UInt32;    {size filled in by caller; record size = SizeOf(record)}
    Flags                  : UInt32;
    MovesGenerated         : Int64;
    PushesGenerated        : Int64;
    StatesGenerated        : Int64;     {number of positions}
    StatusText             : String;
    TimeMS                 : UInt32;    {milli seconds}
                             end;
  PSokobanStatus           = ^TSokobanStatus;
  TGeneratorStatistics     = record {statistics for generating a MxN Orimaze puzzle board}
    BFSCountForEnumeratedPuzzles,
    BFSFilterClusterDiameterShorterThanBestResult,
    BFSStartedForMembersOfClustersProcessedEarlier,
    BFSStartedForMembersOfClustersProcessedElsewhere,
    BFSStartedForMembersOfClustersProcessedElsewherePushCount,
    BFSTotalCount,
    BFSTotalPushCount,
    EnumeratedBoardsCount,
    EnumeratedBoardsWhichAreInvalidBecauseThePlayerPositionOverlapsANonZeroBitValueOnTheBoard,
    EnumerationFilterAlreadyVisitedCluster,
    EnumerationFilterColumnWithVerticalTiles,
    EnumerationFilterFencedInPlayer,
    EnumerationFilterFirstOrLastColumnWithHorizontalTiles,
    EnumerationFilterFirstOrLastRowWithVerticalTiles,
    EnumerationFilterOtherFilters,
    EnumerationFilterRowWithHorizontalTiles,
    EnumerationFilterSameReachableSquaresAsPreviousPuzzle,
    EnumerationFilterSymmetry,
    EnumerationFilterZeroPushes,
    GeneratedPuzzlesCount,
    IFUBBreadthFirstSearchCount,
    IFUBClusterCount,
    IFUBClusterPositionCount,
    IFUBClusterStartPositionCount,
    IFUBFilterClusterDiameterShorterThanBestResult
                           : UInt64;
    IFUBHistogram          : array[0..10] of UInt64; {search statistics. 0..9%, 10..19%, ..., 90..99%, 100%.. (may exceed 100% because of multiple BFSes (breadth-first searches) for the same position}
    MoveCount,
    PushCount              : UInt64;
    PushCountHistogram     : array[0..MAX_HISTORY_BOX_PUSHES] of UInt64;
    SearchLimitExceededPushCount
                           : UInt64;
    TimeMS                 : TTimeMS; {milliseconds}
                             end;
  TGenerator               = record
    AdditionalBoxesBetweenGridPoints
                           : TBoxNumberSet;
    AdHocTask              : Integer;   {special (bug repair) tasks. 1: investigate last puzzle in selected segment only. 2: investigate puzzles with more than one legal player move in the starting position.}
    BestOrimazePuzzleLengths {statistics}
                           : array[0..MAX_ORIMAZE_BOARD_WIDTH,0..MAX_ORIMAZE_BOARD_HEIGHT] of Integer;
    BestOrimazeBoardBegin  : TOrimazeBitBoard;
    BestOrimazeBoardEnd    : TOrimazeBitBoard;
    BestOrimazePlayerPosBegin
                           : Integer;
    BestOrimazePlayerPosEnd: Integer;
    BestOrimazePushCount   : Integer;
    BestPuzzlesCount       : UInt64;
    DisplayPushCountdown   : Integer;
    FirstOrimazeBitBoard   : TOrimazeBitBoard; {first Orimaze bit board to generate. see also 'LastOrimazeBitBoard', 'SegmentIndex', and 'SegmentSize'.}
    GenerateAllOrimazePuzzlesUpToSpecifiedDimensions
                           : Boolean;
    GenerateCommandFiles   : Boolean;
    IsMemorySpecified      : Boolean;
    LastCallBackTimeMS     : TTimeMS;   {milliseconds}
    LastOrimazeBitBoard    : TOrimazeBitBoard; {last Orimaze bit board to generate, inclusive. see also 'FirstOrimazeBitBoard', 'SegmentIndex', and 'SegmentSize'.}
    PowersOfTwo            : array[0..MAX_ORIMAZE_BOARD_SIZE] of UInt64; {the last element contains 0. the next to last element contains low(Int64) because Delphi 4 doesn't have unsigned 64-bit integers but only signed 64-bit integers.}
    PlayerPositionIndex    : Integer;   {negative: generate all boards. the absolute value is the number of investigated player starting positions on the board. non-negative: generate boards with the given player position index only.}
    RandomState            : TRandomState; {seed >= 0: randomized enumeration order}
    SearchLimits           : TSearchLimits; {limits controlling the currrently running search; solving or optimizing a level may require multiple searches with different limits}
    SearchStates           : array[0..MAX_SEARCH_STATES] of TSearchState;
    SegmentIndex           : Integer;   {generate boards in segment 'index' of 'count' segments}
    SegmentCount           : Integer;   {generate boards in segment 'index' of 'count' segments}
    SegmentSize            : UInt64;    {may overflow a signed 64-bit integer}
    SokobanCallBackFunction
                           : TSokobanCallBackFunction;
    SokobanStatus          : TSokobanStatus; {only used if the caller doesn't provide a pointer to its own status record}
    SokobanStatusPointer   : PSokobanStatus; {status info; points to 'SokobanStatus' if the caller doesn't provide its own record}
    StartTimeMS            : TTimeMS;   {search time start; defined global here for slightly faster access}
    Statistics             : TGeneratorStatistics;
    Terminated             : Boolean;   {'True' indicates that the solver has been requested to terminate; stopping the currently running search only is done by calling 'TerminateSearch'}
    TimeCheckCount         : UInt64;
    UseAvailableMemory     : Boolean;
    {$IFDEF CALCULATE_SEQUENTIALLY}
      VisitedPlayerSquares : array[0..MAX_ORIMAZE_BOARD_SIZE] of Boolean; {squares visited by the player during the search}
    {$ENDIF}
                             end;
const
  AXIS_TO_ORIMAZE_BOARD_SQUARE_VALUE
                           : array[TAxis] of TOrimazeBoardSquareValue
                           = (VERTICAL,HORIZONTAL);

{Global Variables}

var
  Game                     : TGame;      {Sokoban game}
  Legend                   : TLegend;
  LogFile                  : TLogFile;
  Positions                : TPositions; {global "visited Orimaze start positions" table, transposition-table for BFS (breadth-first search), open-queue, and statistics}
  Generator                : TGenerator; {settings, bookkeeping, control information, and statistics}
{-----------------------------------------------------------------------------}

{Forward Declarations}

procedure BoardToTextLines(var BoardAsTextLines__:TBoardAsTextLines); forward;
procedure OrimazeBoardToTextLines(BoardWidth__,BoardHeight__:Integer; const Board__:TOrimazeBoard; var BoardAsTextLines__:TBoardAsTextLines); forward;
procedure OrimazeSquareToColRow(Square__:Integer; var Col__,Row__:Integer); forward;
procedure InitializeBoard(BoardWidth__,BoardHeight__:Integer); forward;
function  Max(a__,b__:NativeInt):Integer; forward;
function  Msg(const Text__,Caption__:String):Boolean; forward;
procedure OPENRemove(Position__:PPosition); forward;
procedure ShowHelp; forward;
procedure SquareToColRow(Square__:Integer; var Col__,Row__:Integer); forward;
procedure ShowBoard; forward;
function  TimeCheck:Boolean; forward;
procedure Terminate(const Text__:String); forward;
function  TTIsOnPath(Position__,Path__:PPosition):Boolean; forward;
function  TTListLength(Position__:PPosition):Integer; forward;
function  TTLookup(HashValue__:THashValue; PlayerPos__,SearchDepth__:Integer; var Position__:PPosition):Boolean; forward;
procedure TTRemove(Position__:PPosition); forward;
function  WritelnToLogFile(const Text__:String):Boolean; forward;
{-----------------------------------------------------------------------------}

{Operating System Constants, Datatypes, and Functions}

{$IFDEF WINDOWS}
  type
    DWORDLONG                    = UInt64;
    TMemoryStatusEx              = record
      dwLength                   : DWORD;
      ullMemoryLoad              : DWORD;
      ullTotalPhys               : DWORDLONG;
      ullAvailPhys               : DWORDLONG;
      ullTotalPageFile           : DWORDLONG;
      ullAvailPageFile           : DWORDLONG;
      ullTotalVirtual            : DWORDLONG;
      ullAvailVirtual            : DWORDLONG;
      ullAvailExtendedVirtual    : DWORDLONG;
    end;
    PMemoryStatusEx              = ^TMemoryStatusEx;

  procedure GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx); stdcall;
            external 'kernel32.dll' name 'GlobalMemoryStatusEx';
{$ENDIF}

{-----------------------------------------------------------------------------}

{General Utilities}

function  Align( Value__, Alignment__ : Integer ) : Integer;
begin // returns the smallest value >= "Value__" which is a multiple of
      // "Alignment__", except when that value overflows the return value data
      // type;
      // precondition : the value and the alignment are non-negative integers;
  if   Alignment__ <> 0 then
       Result      := Value__ + ( ( Alignment__ - ( Value__ mod Alignment__ ) ) mod Alignment__ )
  else Result      := Value__;
end;

procedure Calculate2DTransformation(Transformation__:TBoardTransformation2D; Col__,Row__,Width__,Height__:Integer; var ResultCol__,ResultRow__:Integer);
var Col,Row,Width,Height:Integer;
    OriginalTransformation:TBoardTransformation2D;
begin
  OriginalTransformation          :=Transformation__;
  if Transformation__             >=t2DRotate0DegreesClockwiseFlipHorizontally then
     Transformation__             :=TBoardTransformation2D(Ord(Transformation__)-Ord(t2DRotate0DegreesClockwiseFlipHorizontally));
  while Transformation__          <>t2DRotate0DegreesClockwise do begin
    Col  :=Col__;            Row  :=Row__;           Width  :=Width__; Height  :=Height__;
    Col__:=Pred(Height)-Row; Row__:=Col;             Width__:=Height ; Height__:=Width; // counterclockwise rotation
//  Col__:=Row  ;            Row__:=Pred(Width)-Col; Width__:=Height ; Height__:=Width; // clockwise rotation
    Transformation__              :=Pred(Transformation__);
    end;
  if   OriginalTransformation     >=t2DRotate0DegreesClockwiseFlipHorizontally then
       ResultCol__                :=Pred(Width__)-Col__
  else ResultCol__                :=Col__;
  ResultRow__                     :=Row__;
end;

procedure Calculate2DDimensionsTransformation(Transformation__:TBoardTransformation2D; Width__,Height__:Integer; var ResultWidth__,ResultHeight__:Integer);
begin
 if Odd(Ord(Transformation__)) then begin
    ResultWidth__:=Height__; ResultHeight__:=Width__;
    end
 else begin
    ResultWidth__:=Width__; ResultHeight__:=Height__;
    end;
end;

function  CalculateElapsedTimeMS(StartTimeMS__,StopTimeMS__:TTimeMS):TTimeMS;
begin
  if   StopTimeMS__>=StartTimeMS__ then
       Result:=StopTimeMS__-StartTimeMS__
  else Result:=High(StopTimeMS__)-StartTimeMS__+StopTimeMS__+1; {clock wrap around; assume it only wrapped around once}
end;

procedure DoNothing;
begin
end;

function  FileNameWithExtension(const FileName__,Extension__:String):String;
var i:Integer;
begin
  i:=Length(FileName__);
  while (i<>0) and (FileName__[i]<>PERIOD) do Dec(i);
  if   i<>0 then Result:=Copy(FileName__,1,Pred(i))
  else Result:=FileName__;
  Result:=Result+Extension__;
end;

function  GetAvailablePhysicalMemoryByteSize:TSize;
{$IFDEF WINDOWS}
  var MemoryStatusEx:TMemoryStatusEx;
  begin
    MemoryStatusEx.dwLength:=SizeOf(MemoryStatusEx);
    GlobalMemoryStatusEx(MemoryStatusEx);
    if   MemoryStatusEx.ullAvailPhys<=High(Result) then
         Result:=MemoryStatusEx.ullAvailPhys
    else Result:=High(Result);
  end;
{$ELSE}
  begin
    Result:=0;
  end;
{$ENDIF}

function  GetAvailableUserMemoryByteSize:TSize;
{$IFDEF WINDOWS}
  var MemoryStatusEx:TMemoryStatusEx;
  begin
    MemoryStatusEx.dwLength:=SizeOf(MemoryStatusEx);
    GlobalMemoryStatusEx(MemoryStatusEx);
    if   MemoryStatusEx.ullAvailVirtual<=High(Result) then
         Result:=MemoryStatusEx.ullAvailVirtual
    else Result:=High(Result);
  end;
{$ELSE}
  begin
    Result:=0;
  end;
{$ENDIF}

function  GetPhysicalMemoryByteSize:TSize;
{$IFDEF WINDOWS}
  var MemoryStatusEx:TMemoryStatusEx;
  begin
    MemoryStatusEx.dwLength:=SizeOf(MemoryStatusEx);
    GlobalMemoryStatusEx(MemoryStatusEx);
    if   MemoryStatusEx.ullTotalPhys<=High(Result) then
         Result:=MemoryStatusEx.ullTotalPhys
    else Result:=High(Result);
  end;
{$ELSE}
  begin
    Result:=0;
  end;
{$ENDIF}

function  GetTimeMS:TTimeMS;
begin {returns a time measured in milliseconds; the base doesn't matter, the time is only used in relative calculations}
{$IFDEF WINDOWS}
  Result:=GetTickCount; {Windows function}
{$ELSE}
  Result:=0;            {no timing}
{$ENDIF}
end;

procedure InitializeRandomState(RandomNumber__:Integer; var RandomState__:TRandomState);
begin
  RandomState__.RandomNumber:=Max(1,Abs(RandomNumber__));
end;

function  IntToStr(Value__:Int64):String;
begin
  Str(Value__,Result);
end;

function  IntToStr_(Value__:Int64):String;
var Index:Integer;
begin
  Result:=IntToStr(Value__);
  Index:=Length(Result)-3;
  while Index>0 do begin
    Insert(COMMA,Result,Succ(Index));
    Dec(Index,3);
    end;
end;

function  IntToStrWithPercentage(Dividend__,Divisor__:Int64):String;
var Percentage:Int64;
begin {returns a string like this example: '123 (45%)'. the percentage calculation may cause a numeric overflow.}
  Result:=IntToStr_(Dividend__);
  if Divisor__<>0 then begin
     if Dividend__<=High(Dividend__) div 100 then
        Dividend__:=Dividend__*100
     else begin
        Divisor__:=Divisor__ div 100;
        if Divisor__= 0 then
           Divisor__:=1;
        end;
     if   Divisor__>0 then
          if   Dividend__<=High(Dividend__)-(Divisor__ div 2) then
               Inc(Dividend__,Divisor__ div 2)  {for rounding}
          else
     else if   Dividend__>=Low (Dividend__)-(Divisor__ div 2) then
               Inc(Dividend__,Divisor__ div 2); {for rounding}
     Percentage:=Dividend__ div Divisor__;
     //if (Percentage>=100) and (Dividend__<Divisor__) then
     //   Percentage:=99;
     Result:=Result+SPACE+LEFT_PARENTHESIS+IntToStr(Percentage)+PERCENT+RIGHT_PARENTHESIS; {'Dividend__ div Divisor__' may cause a numeric overflow}
     end;
end;

function  IntToStrWithSingularOrPluralText(Value__:Int64; const Text__:String):String;
begin
  if   Value__<>1 then
       Result:=IntToStr_(Value__)+SPACE+Text__+'s'
  else Result:=IntToStr_(Value__)+SPACE+Text__;
end;

function  LoCase(Char__:Char):Char;
begin
  Result:=Char__;
  if (Result>='A') and (Result<='Z') then
     Result:=Chr(Ord('a')+Ord(Result)-Ord('A'));
end;

function  Log2(Number__:Integer):Integer;
begin
  Result:=-1;
  while Number__>0 do begin Inc(Result); Number__:=Number__ div 2; end;
end;

function  HasCharCI(const String__:String; Char__:Char):Boolean;
begin {returns 'True' if the string contains the character, case insensitive}
  Result:=(System.Pos(LoCase(Char__),String__)<>0) or
          (System.Pos(UpCase(Char__),String__)<>0);
end;

function  Max(a__,b__:NativeInt):NativeInt;
begin
  if a__>=b__ then Result:=a__
  else Result:=b__;
end;

function  Min(a__,b__:NativeInt):NativeInt;
begin
  if a__<=b__ then Result:=a__
  else Result:=b__;
end;

function  MinimalStandardRandomNumberGenerator( var Seed__ : Integer ) : Integer;
const
  // minimal standard random number generator
  // don't change the following values unless you know what you're doing;
  A = 7 * 7 * 7 * 7 * 7;           // 7^5      = 16807
  M = ( UInt( 1 ) shl 31 ) - 1;    // 2^31 - 1 = 2147483647 (a prime number)
  Q = M div A;                     // 127773     (quotient)
  R = M mod A;                     // 2836       (remainder)
begin // returns a pseudo random number 0..M-2, where M = 2^31-1 (a prime)
      // preconditions:
      // * MAXIMUM_RANDOM_NUMBER = M-2, i.e., the global constant contains the
      //   maximum value returned by this function;
      // * (note that the seed doesn't require initialization;)
      //
      // The algorithm is described in the article:
      // Park, Steven K. and Miller, Keith W., "Random Number Generators:
      // Good Ones are Hard to Find", Communications of the ACM,
      // October 1988, Volume 31 Number 10.
  if ( Seed__ <= 0 ) or   // 'True': the seed hasn't been properly initialized
     ( Seed__ >= M ) then // 'True': the seed hasn't been properly initialized
     Seed__   := 1;       // start a new period, initializing seed to '1'
  //if ( M - 2 ) <> MAXIMUM_RANDOM_NUMBER then
  //   raise Exception.Create( 'Random: Internal error' );
  Seed__      := ( A * ( Seed__ mod Q ) ) - ( R * ( Seed__ div Q ) );
  if Seed__   <= 0 then // '<=': '=' cannot happen, but the check doesn't hurt
     Inc( Seed__, M );
  Result      := Pred( Seed__ ); // 'Pred': return [0..M-2]; seed is [1..M-1]
end;

function  Random(Range__:Integer; var RandomState__:TRandomState):Integer; {private random function so results are reproducible}
begin
  Result:=MinimalStandardRandomNumberGenerator(RandomState__.RandomNumber);
  if Range__>0 then
     Result:=Result mod Range__;
end;

function  StrStartsWith(const Text__,Prefix__:String):Boolean;
begin
  Result:=(Prefix__<>'') and
          (Length(Text__)>=Length(Prefix__)) and
          (Copy(  Text__,1,Length(Prefix__))=Prefix__);
end;
{$IFDEF WINDOWS}
  function StrWithTrailingPathDelimiter(const Str__:String):String; {throws EOutOfMemory}
  begin
    if   (Str__='') or (Str__[Length(Str__)]=WINDOWS_FILE_NAME_PATH_DELIMITER) then
         Result:=Str__
    else Result:=Str__+WINDOWS_FILE_NAME_PATH_DELIMITER;
  end;
{$ENDIF}
function  StrToInt(const s__:String; var i__:Int64):Boolean;
var ErrorPos:Integer;
begin
  Val(s__,i__,ErrorPos); Result:=ErrorPos=0;
end;

function  TriangularNumber(Value__:Integer):Int64;
begin {formula: T(n) = ( n * ( n + 1 ) ) div 2 }
  Result:=(Int64(Value__)*Succ(Int64(Value__))) div 2;
end;

{$IFDEF WINDOWS}
  function  WasKeyPressed( VirtualKeyCode__ : Integer ) : Boolean;
  begin // returns 'True' if the given key has been pressed
    Result := ( GetASyncKeyState( VirtualKeyCode__ ) and
                Succ( 1 shl ( ( SizeOf( SHORT ) * BITS_PER_BYTE ) - 1 ) ) ) <>
              0;
    if Result then // wait until the key isn't pressed
       repeat
       until ( GetASyncKeyState( VirtualKeyCode__ ) and
               Succ( 1 shl ( ( SizeOf( SHORT ) * BITS_PER_BYTE ) - 1 ) ) ) = 0;
  end;
{$ENDIF}

{-----------------------------------------------------------------------------}

{Utilities}

function  BoxNoAtSquare(SquareNo__:Integer):Integer;
begin // returns the number of the box at the square 'SquareNo__', if any
  Result:=Game.BoxCount;
  while (Result>0) and (Game.BoxPos[Result]<>SquareNo__) do
    Dec(Result);
end;

function  CalculateDefaultMemoryByteSize:TSize;
var PhysicalMemoryBytes,PhysicalMemoryMiB:TSize;
begin
  PhysicalMemoryBytes:=GetPhysicalMemoryByteSize;
  if PhysicalMemoryBytes<High(PhysicalMemoryBytes) - (ONE_MEBI div 2) then Inc(PhysicalMemoryBytes,(ONE_MEBI div 2)); // prepare to round up
  {$WARNINGS OFF} {warning: Comparison always evaluates to True}
  if PhysicalMemoryBytes> High(NativeInt) then
     PhysicalMemoryBytes:=High(NativeInt); // limit the memory byte size to a signed integer
  {$WARNINGS ON}
  PhysicalMemoryMiB:=PhysicalMemoryBytes div ONE_MEBI;
  if   PhysicalMemoryMiB>0 then
       Result:=Min(NativeInt((PhysicalMemoryMiB*ONE_MEBI) div 2),DEFAULT_MEMORY_BYTE_SIZE_SMALL) // 'div 2': only use 50% of the physical memory
  else Result:=DEFAULT_MEMORY_BYTE_SIZE_SMALL;
end;

function  CharToDirection(Ch__:Char; var Direction__:TDirection):Boolean;
var Dir:TDirection;
begin
  Result:=False; Ch__:=LoCase(Ch__);
  for Dir:=Low(Dir) to High(Dir) do
      if Ch__=DIRECTION_TO_CHAR[Dir] then begin {caution: 'DIRECTION_CHAR' must be lowercase characters}
         Direction__:=Dir; Result:=True; break;
         end;
end;

function  ColRowToSquare(Col__,Row__:Integer):Integer;
begin
  ColRowToSquare:=Row__ * (Game.BoardWidth+2) + Col__;
end;

function  DxDyToDirection(Dx__,Dy__:Integer):TDirection;
begin {precondition: either 'Dx__' or 'Dy__' is 0}
  if        Dx__=0 then
            if   Dy__<=0 then Result:=dUp
            else              Result:=dDown
  else if   Dx__<0       then Result:=dLeft
            else              Result:=dRight;
end;

procedure DirectionToDxDy(Direction__:TDirection; var Dx__,Dy__:Integer);
begin
  Dx__:=0; Dy__:=0;
  case Direction__ of
    dUp   : Dy__:=-1;
    dLeft : Dx__:=-1;
    dDown : Dy__:= 1;
    dRight: Dx__:= 1;
    else    Msg('DirectionToDxDy',TEXT_INTERNAL_ERROR);
  end; {case}
end;

function  GeneratorTaskCaption(const Text__:String; Width__,Height__,PlayerPositionIndex__,SegmentIndex__,SegmentCount__,AdHocTask__:Integer):String;
begin
  if   Text__<>'' then {use this for making a log file name}
       Result:=Text__      +SPACE+LEFT_BRACKET+IntToStr(Width__)+'x'+IntToStr(Height__)+RIGHT_BRACKET
  else Result:=TEXT_ORIMAZE+SPACE             +IntToStr(Width__)+'x'+IntToStr(Height__);
  if PlayerPositionIndex__>=0 then
     Result:=Result+SPACE+TEXT_PLAYER+SPACE+IntToStr(PlayerPositionIndex__);
  if SegmentIndex__>=0 then
     Result:=Result+SPACE+TEXT_SEGMENT+SPACE+IntToStr(SegmentIndex__)+SPACE+TEXT_OF+SPACE+IntToStr(SegmentCount__);
  if AdHocTask__>0 then
     Result:=Result+SPACE+TEXT_AD_HOC+SPACE+IntToStr(AdHocTask__);
end;

function  GoalNoAtSquare(SquareNo__:Integer):Integer;
var GoalNo:Integer;
begin // returns the number of the goal at the square 'SquareNo__', if any
  Result:=0;
  for GoalNo:=1 to Game.GoalCount do
      if Game.GoalPos[GoalNo]=SquareNo__ then begin
         Result:=GoalNo; break;
         end;
end;

function  IsABlackSquareOnAChessBoard(Square__:Integer):Boolean;
var Col,Row:Integer;
begin
  SquareToColRow(Square__,Col,Row);
  Result:=(Odd(Col) and Odd(Row)) or ((not Odd(Col)) and (not Odd(Row)));
end;

function  IsOnPath(Position__:PPosition):Boolean;
begin
  Result:=(mfPath in Position__^.Move.Flags);
end;

function  ManhattanDistance(Square1__,Square2__:Integer):Integer;
var Col1,Col2,Row1,Row2:Integer;
begin {returns the 'Manhattan distance' between two squares on the board}
  SquareToColRow(Square1__,Col1,Row1);
  SquareToColRow(Square2__,Col2,Row2);
  Result:=Abs(Col1-Col2)+Abs(Row1-Row2);
end;

function  MinimumDistanceToSquare(Square__:Integer; const Distances__:TSquareDirectionArrayOfInteger):Integer;
var Direction:TDirection;
begin {returns the minimum distance to the square, given the distances from each direction}
  Result:=INFINITY;
  for Direction:=Low(Direction) to High(Direction) do
      if (Abs(Distances__[Square__,Direction])<>INFINITY) and
         (    Distances__[Square__,Direction] < Result)   then
         Result:=Distances__[Square__,Direction];
end; {MinimumDistanceToSquare}

function  NeighborSquareDirection(Square__,NeighborSquare__:Integer):TDirection;
begin {precondition: 'Square__' and 'NeighborSquare__' are neighbors}
  if        Square__ >  NeighborSquare__ then
       if   Square__ =  Succ( NeighborSquare__ ) then
            Result   := dLeft
       else Result   := dUp
  else if   Square__ =  Pred( NeighborSquare__ ) then
            Result   := dRight
       else Result   := dDown;
end;

function  OrimazePlayerStartPositionCount(OrimazeBoardWidth__,OrimazeBoardHeight__:Integer):Integer;
begin {returns the number of Orimaze player start positions, taking symmetry into account}
  if   OrimazeBoardWidth__<>OrimazeBoardHeight__ then
       Result:=(Succ(OrimazeBoardWidth__) div 2)*(Succ(OrimazeBoardHeight__) div 2) {the squares in the top-left quadrant}
  else Result:=TriangularNumber(Succ(OrimazeBoardWidth__) div 2); {the squares in a right-angled triangle covering half of the top-left quadrant}
end;

function  OrimazeBitBoardToTableIndex(PlayerPos__:Integer; Board__:TOrimazeBitBoard):UInt64;
begin {returns the Orimaze bit board as if the player square didn't exist, thereby reducing the table index by a factor 2.
       precondition: the board is not so large that the high bit of the bit board is 1. (Delphi 4 doesn't have unsigned 64-bit integers, only signed 64-bit integers.)}
  Result:=((Board__ and Pred(Generator.PowersOfTwo[PlayerPos__]))) + {bits below the player position index}
          ((Board__ shr Succ(PlayerPos__)) shl PlayerPos__);         {bits above the player position index}
end;

function  PerformSokobanCallBackFunction:Integer;
begin {returns a non-zero value if the solver should terminate}
  if   Assigned(Generator.SokobanCallBackFunction) then with Generator.SokobanStatusPointer^ do begin
       MovesGenerated :=Generator.Statistics.MoveCount;
       PushesGenerated:=Generator.Statistics.PushCount;
       StatesGenerated:=Int64(YASGenOrimaze.Positions.Count)+YASGenOrimaze.Positions.SearchStatistics.DroppedCount;
       TimeMS:=UInt32(Game.InitializationTimeMS+Generator.Statistics.TimeMS);
       Result:=Generator.SokobanCallBackFunction();
       end
  else Result:=0;
end;

procedure SetSokobanStatusText(const Text__:String);
begin
  with Generator.SokobanStatusPointer^ do begin
    StatusText:=Text__;
    Writeln(StatusText);
    end;
end;

function  SokobanCallBackFunction:Integer; stdcall;
begin // test function only; not in production
  with Generator.SokobanStatusPointer^ do begin
    Writeln('Pushes: ',PushesGenerated,' Positions: ',StatesGenerated,' Time (ms): ',TimeMS);
    Readln;
    Result:=0; {'0': continue search; anything else terminates the search}
    end;
end;

function  SquareToChar(Col__,Row__:Integer):Char;
begin
  case Game.Board[Col__,Row__] and (PLAYER+BOX+GOAL+WALL) of
    PLAYER     : if   Legend.XSBNotation then
                      Result:=CH_PLAYER_XSB
                 else Result:=CH_PLAYER;
    PLAYER+GOAL: if   Legend.XSBNotation then
                      Result:=CH_PLAYER_ON_GOAL_XSB
                 else Result:=CH_PLAYER_ON_GOAL;
    PLAYER+BOX, {can happen for deadlock-sets}
    BOX        : if   Legend.XSBNotation then
                      Result:=CH_BOX_XSB
                 else Result:=CH_BOX;
    PLAYER+BOX+GOAL, {can happen for deadlock-sets}
    BOX+GOAL   : if   Legend.XSBNotation then
                      Result:=CH_BOX_ON_GOAL_XSB
                 else Result:=CH_BOX_ON_GOAL;
    GOAL       : Result:=CH_GOAL;
    WALL       : Result:=CH_WALL;
//  else         Result:=UNDERSCORE;
//  else         Result:=CH_NON_BLANK_FLOOR;
    else         Result:=CH_FLOOR;
  end;
  if Game.Board[Col__,Row__]=FLAG_SQUARE_SET then Result:=CH_SQUARE_SET;
  if ((Game.Board[Col__,Row__] and WALL)<>0) and (Game.Board[Col__,Row__]<>WALL) then
     Result:=CH_WALL;
end;

function  OrimazeSquareToChar(Square__:Integer; const Board__:TOrimazeBoard):Char;
begin
  case Board__[Square__] and (PLAYER+HORIZONTAL+VERTICAL+WALL) of
    PLAYER     : Result:=CH_ORIMAZE_PLAYER;
    HORIZONTAL : Result:=AXIS_TO_TEXT[aHorizontal][1];
    VERTICAL   : Result:=AXIS_TO_TEXT[aVertical  ][1];
    WALL       : Result:=CH_WALL;
//  else         Result:=UNDERSCORE;
//  else         Result:=CH_NON_BLANK_FLOOR;
    else         Result:=CH_FLOOR;
  end;
end;

procedure SquareToColRow(Square__:Integer; var Col__,Row__:Integer);
begin
  Row__:=Square__   div   (Game.BoardWidth+2);
  Col__:=Square__ - Row__*(Game.BoardWidth+2);
end;

function  SquareToColRowAsText(Square__:Integer):String;
var Col,Row:Integer;
begin
  SquareToColRow(Square__,Col,Row);
  Result:=IntToStr(Col)+COMMA+IntToStr(Row);
end;

function  SAT(Square__:Integer):String; {'SquareToColRowAsText' abbreviated; for debuggin convenience}
begin
  Result:=SquareToColRowAsText(Square__);
end;

{-----------------------------------------------------------------------------}

{Console}

function  GetCommandLineParameters(var OrimazeBoardWidth__,
                                       OrimazeBoardHeight__,
                                       DepthLimit__,
                                       PushCountLimit__,
                                       RandomSeed__,
                                       PlayerPositionIndex__,
                                       SegmentIndex__,
                                       SegmentCount__,
                                       AdHocTask__:Integer;
                                   var FirstOrimazeBitBoard__,
                                       LastOrimazeBitBoard__ : TOrimazeBitBoard;
                                   var MemoryByteSize__:TSize;
                                   var GenerateAllOrimazePuzzlesUpToSpecifiedDimensions__,
                                       GenerateCommandFiles__,
                                       IsMemorySpecified__,
                                       UseAvailableMemory__:Boolean;
                                   var TimeLimitMS__:TTimeMS;
                                   var LogFileEnabled__:Boolean;
                                   var AdditionalBoxesBetweenGridPoints__:TBoxNumberSet
                                   ):Boolean;
var i,ItemIndex,Index,Len,Number:Integer;
    s:String;

  function  GetParameter(var Value__:Integer; Min__,Max__,Scale__:Integer; var ItemIndex__:Integer):Boolean;
  var InputValue:Int64;
  begin
    Inc(ItemIndex__); {skip argument name}
    if Scale__<1 then Scale__:=1;
    Result:=(ItemIndex__<=ParamCount) and
            StrToInt(ParamStr(ItemIndex__),InputValue) and
            (InputValue>=Min__ div Scale__) and (InputValue<=Max__ div Scale__);
    Value__:=InputValue*Scale__;
    Inc(ItemIndex__); {advance to next parameter}
  end;

  function  GetBooleanValue(var Value__:Boolean):Boolean;
  var s:String;
  begin
    Result:=False; Inc(ItemIndex); {advance to next parameter}
    if  ParamCount>=ItemIndex then begin
        s:=ParamStr(ItemIndex); Inc(ItemIndex); {note that 'ItemIndex' now points to the next parameter, if any}
        if   s<>'' then
             if        (s[1]='y') or (s[1]='Y') then begin
                       Value__:=True; Result:=True;
                       end
             else if   (s[1]='n') or (s[1]='N') then begin
                       Value__:=False; Result:=True;
                       end;
        end;
  end;

  function  GetOrimazeBitBoard(var OrimazeBitBoard__:TOrimazeBitBoard; var ItemIndex__:Integer):Boolean;
  begin
    Inc(ItemIndex__); {skip argument name}
    Result:=(ItemIndex__<=ParamCount) and
            StrToInt(ParamStr(ItemIndex__),OrimazeBitBoard__);
    Inc(ItemIndex__); {advance to next parameter}
  end;

begin {a simple and not fool-proof implementation}
  Result                 := (ParamCount>=1);
  GenerateAllOrimazePuzzlesUpToSpecifiedDimensions__ :=False;
  GenerateCommandFiles__ := False;
  IsMemorySpecified__    := False;
  UseAvailableMemory__   := False;
  OrimazeBoardWidth__    := DEFAULT_ORIMAZE_BOARD_WIDTH;
  OrimazeBoardHeight__   := DEFAULT_ORIMAZE_BOARD_HEIGHT;
  MemoryByteSize__       := -1; {undefined so far}
  PushCountLimit__       := DEFAULT_PUSH_COUNT_LIMIT;
  DepthLimit__           := DEFAULT_SEARCH_DEPTH;
  RandomSeed__           := -1; // negative: randomized order is disabled
  PlayerPositionIndex__
                         := -1; // negative: generate all boards. non-negative: generate boards with the given player position index only. a "player position index" isn't the same as a "board square number".
  SegmentIndex__         := -1; // negative: generate all boards
  SegmentCount__         := -1; // negative: generate all boards
  FirstOrimazeBitBoard__ := TOrimazeBitBoard( 0); // begin with a board where all bits are set to 0
  LastOrimazeBitBoard__  := TOrimazeBitBoard(Int64(-1)); // one bit for each square on the board, possibly with the exception of scale factor high bits
  AdHocTask__            := -1; // negative: none.
  LogFileEnabled__       := DEFAULT_LOG_FILE_ENABLED;
  TimeLimitMS__          := High(TimeLimitMS__); // high-value = unlimited
  AdditionalBoxesBetweenGridPoints__.Count :=0;

  ItemIndex              := 1;
  while Result and (ParamCount>=ItemIndex) do begin {get options}
     s:=ParamStr(ItemIndex);
     Result:=(Length(s)>=2) and ((s[1]=HYPHEN) or (s[1]=SLASH));
     if Result then
        case LoCase(s[2]) of
          'a'       : if HasCharCI(s,'c') then begin {ad hoc task}
                         Result:=GetParameter(AdHocTask__,1,High(AdHocTask__),0,ItemIndex);
                         end
                      else begin
                         GenerateAllOrimazePuzzlesUpToSpecifiedDimensions__:=True;
                         Inc(ItemIndex); {advance to next parameter}
                        end;
          'b'       : if HasCharCI(s,'a') then begin {generate ".bat" command files}
                         GenerateCommandFiles__:=True;
                         Inc(ItemIndex); {advance to next parameter}
                         end
                      else begin
                         Inc(ItemIndex); {advance to next parameter}
                         while Result and (ItemIndex<=ParamCount) and (s<>'') do begin
                           s:=ParamStr(ItemIndex);
                           if (s<>'') and (s[1]>='0') and (s[1]<='9') then begin
                              Number:=0; Index:=1; Len:=Length(s);
                              while Result and
                                    (Index<=Len) and
                                    ((s[Index]>='0') and (s[Index]<='9')) do begin
                                if   Number <= (High(Number) - Ord(s[Index]) + Ord('0')) div 10 then begin
                                     Number := 10*Number+Ord(s[Index])-Ord('0');
                                     Inc(Index);
                                     end
                                else Result:=False; {integer overflow}
                                end;
                              if Result then with AdditionalBoxesBetweenGridPoints__ do
                                 if Count<High(Numbers) then begin
                                    Inc(Count);
                                    Numbers[Count]:=Number;
                                    Inc(ItemIndex); {advance to next parameter}
                                    end
                                 else Result:=False; {array index overflow}
                              end
                           else s:=''; {the parameter isn't a number}
                           end;
                         end;
          'h'       : Result:=GetParameter(OrimazeBoardHeight__,1,MAX_ORIMAZE_BOARD_HEIGHT,0,ItemIndex);
          {$IFDEF CALCULATE_SEQUENTIALLY}
            'f'     : Result:=GetOrimazeBitBoard(FirstOrimazeBitBoard__,ItemIndex);
          {$ENDIF}
          'm'       : if           HasCharCI(s,'u') then begin {MaxPushes}
                                   Result:=GetParameter(i,0,High(i),ONE_MILLION,ItemIndex);
                                   PushCountLimit__:=i;
                                   if PushCountLimit__=0 then PushCountLimit__:=High(PushCountLimit__); {special: argument=0 -> limit:=none}
                                   end
                      else if      (Length(s)>=5) and
                                   ((s[5]='d') or (s[5]='D')) {MaxDepth} then
                                   Result:=GetParameter(DepthLimit__,0,MAX_HISTORY_BOX_PUSHES,0,ItemIndex)
                           else if (Length(s)>=5) and
                                   ((s[5]='t') or (s[5]='T')) {MaxTime} then begin
                                   Result:=GetParameter(Integer(TimeLimitMS__),0,MAX_SEARCH_TIME_LIMIT_MS,1000,ItemIndex);
                                   end
                                else
                                   if (Length(s)>=4) and
                                      ((s[4]='m') or (s[4]='M')) then begin {Memory}
                                      IsMemorySpecified__  :=True;
                                      MemoryByteSize__     :=GetPhysicalMemoryByteSize;
                                      if MemoryByteSize__  > MAX_TRANSPOSITION_TABLE_BYTE_SIZE then
                                         MemoryByteSize__  :=MAX_TRANSPOSITION_TABLE_BYTE_SIZE; // limit the transposition table byte size to a signed integer and leave some memory for other purposes
                                      Result:=GetParameter(MemoryByteSize__,
                                                           0,
                                                           {$IFDEF WINDOWS}
                                                             MemoryByteSize__,
                                                           {$ELSE}
                                                             MAX_TRANSPOSITION_TABLE_BYTE_SIZE,
                                                           {$ENDIF}
                                                           ONE_MEBI,ItemIndex);
                                      if (not Result) and (Pred(ItemIndex)<=ParamCount) then begin
                                         s:=ParamStr(Pred(ItemIndex));
                                         if (s<>'') and ((s[1]='a') or (s[1]='A')) then begin
                                            UseAvailableMemory__:=True;
                                            {$IFDEF WINDOWS}
                                              MemoryByteSize__    :=GetAvailableUserMemoryByteSize;
                                              if MemoryByteSize__ > (MAX_TRANSPOSITION_TABLE_BYTE_SIZE) then
                                                 MemoryByteSize__ :=(MAX_TRANSPOSITION_TABLE_BYTE_SIZE); // limit the transposition table byte size to a signed integer and leave some memory for other purposes
                                              Result:=(MemoryByteSize__>=0) and (MemoryByteSize__<=High(NativeInt)-ONE_MEBI);
                                            {$ELSE}
                                              Msg('"-memory available": This parameter is only implemented in the Windows version.','');
                                            {$ENDIF}
                                            end;
                                         end;
                                      end
                                   else Result:=False;
          'p'       : begin Result:=GetParameter(PlayerPositionIndex__,0,High(PlayerPositionIndex__),0,ItemIndex);
                      end;
          'r'       : begin Result:=GetParameter(RandomSeed__,0,High(RandomSeed__),0,ItemIndex);
                      end;
          's'       : begin Result:=GetParameter(SegmentIndex__,0,High(SegmentCount__)-1,0,ItemIndex); {'High(SegmentCount)-1': sic}
                            Result:=Result and GetParameter(SegmentCount__,Succ(SegmentIndex__),High(SegmentCount__),0,ItemIndex);
                      end;
          {$IFDEF CALCULATE_SEQUENTIALLY}
            't'     : Result:=GetOrimazeBitBoard(LastOrimazeBitBoard__,ItemIndex);
          {$ENDIF}
          'w'       : Result:=GetParameter(OrimazeBoardWidth__,1,MAX_ORIMAZE_BOARD_WIDTH,0,ItemIndex);
          else        Result:=False;
       end; {case}
     end;

  if OrimazeBoardHeight__>OrimazeBoardWidth__ then begin {'True': swap width and height, so width >= height}
     i:=OrimazeBoardWidth__;
     OrimazeBoardWidth__:=OrimazeBoardHeight__;
     OrimazeBoardHeight__:=i;
     end;

  if ((OrimazeBoardWidth__*OrimazeBoardHeight__)>=LARGE_ORIMAZE_PUZZLE_THRESHOLD_SQUARES) and
     (MemoryByteSize__          = -1) then begin
     MemoryByteSize__           :=GetPhysicalMemoryByteSize;
     if MemoryByteSize__        > MAX_TRANSPOSITION_TABLE_BYTE_SIZE then
        MemoryByteSize__        :=MAX_TRANSPOSITION_TABLE_BYTE_SIZE; // limit the transposition table byte size to a signed integer and leave some memory for other purposes
     MemoryByteSize__           :=Min(NativeInt(MemoryByteSize__),DEFAULT_MEMORY_BYTE_SIZE_LARGE);
     end;
  if MemoryByteSize__=-1 then {'True': use default value}
     MemoryByteSize__:=CalculateDefaultMemoryByteSize;

  if RandomSeed__>=0 then {'True': generate boards at random}
     GenerateAllOrimazePuzzlesUpToSpecifiedDimensions__:=False; {just generate puzzles with maximum specified dimensions}

  if GenerateCommandFiles__ then
     LogFileEnabled__:=False;

  i:=OrimazePlayerStartPositionCount(OrimazeBoardWidth__,OrimazeBoardHeight__);
  if Result and (PlayerPositionIndex__>=0) then begin
     Result:=PlayerPositionIndex__<i;
     if not Result then begin
        ShowHelp;
        s:=TEXT_LEGAL_PLAYER_POSTIONS+COLON+SPACE+IntToStr(0);
        if i>1 then
           s:=s+LAZY_COLON+IntToStr(Pred(i));
        Msg(s+PERIOD,'');
        end;
     end
  else PlayerPositionIndex__:=-i; {negative: generator boards for all player start positions}

  {for programming convenience, put a limit on the Orimaze board size, so the
   board itself, and statistical sums, can be represented by a signed 64-bit
   integer. this is also a necessity when the program is compiled with the
   Delphi 4 compiler which doesn't have unsigned 64-bit integers.}
  if Result then begin
     i:=MAX_ORIMAZE_BOARD_SIZE;
     while (Int64(1) shl Pred(i))<=0 do
       Dec(i);
     Result:=i>=OrimazeBoardWidth__*OrimazeBoardHeight__;
     if not Result then begin
        ShowHelp;
        s:=TEXT_MAXIMUM_ORIMAZE_BOARD_SIZE+COLON+SPACE+IntToStr(i);
        Msg(s+PERIOD,'');
        end;
     end;

  Result:=Result and (ItemIndex>ParamCount);
end;

function  Msg(const Text__,Caption__:String):Boolean;
begin
  if (Text__<>'') or (Caption__<>'') then begin
     Writeln;
     if Caption__<>'' then Writeln(Caption__);
     if Text__   <>'' then Writeln(Text__);
     end;
  Writeln('');
  Write('Press [Enter]');
  Readln;
  Result:=False; {for convenience, return 'False' so 'Msg' can be used for reporting errors}
end;

procedure ShowHelp;
begin
  Writeln;
  Writeln('Usage: YASGenOrimaze [options]');
  Writeln('');
  Writeln('Options:');
  Writeln('  -width                        : Orimaze board width,  max. ',MAX_ORIMAZE_BOARD_WIDTH);
  Writeln('  -height                       : Orimaze board height, max. ',MAX_ORIMAZE_BOARD_HEIGHT);
  Writeln;
  Writeln('  -all                          : generate puzzles [1,1] - [width, height]');
  Writeln('  -bat                          : generate DOS command files (".bat" files)');
  Writeln('  -boxes   <count> ...          : additional boxes between the grid points');
//Writeln('  -maxpushes <number> (million) : search limit, default none'); // DEFAULT_PUSH_COUNT_LIMIT div ONE_MILLION,' million');
//Writeln('  -maxdepth  <number>           : search limit, default (and max.) ',MAX_HISTORY_BOX_PUSHES,' pushes');
//Writeln('  -maxtime   <seconds>          : search limit, default (and max.) ',MAX_SEARCH_TIME_LIMIT_MS div (24*60*60*ONE_THOUSAND), ' days');
  {$IFDEF WINDOWS}
    Writeln
         ('  -memory  <size>|avail (MiB)   : default ',CalculateDefaultMemoryByteSize div ONE_MEBI,' MiB. Available: physical memory');
  {$ELSE}
    Writeln
         ('  -memory  <size>   (MiB)       : default ',CalculateDefaultMemoryByteSize div ONE_MEBI,' MiB');
  {$ENDIF}
  Writeln;
  Writeln('  -player  <index>              : puzzles with this player position index only');
  Writeln('  -segment <index> of <count>   : generate this 0-based segment of puzzles only');
  {$IFDEF CALCULATE_SEQUENTIALLY}
    Writeln
         ('  -from    <number>             : board as signed 64-bit number, default  0');
    Writeln
         ('  -to      <number>             : board as signed 64-bit number, default -1');
  {$ENDIF}
  Writeln;
  Writeln('  -random  <seed>               : generate puzzles at random, using this seed');
  Writeln('  -statistics                   : display detailed statistics');
  Writeln;
  Writeln('  -?                            : this overview');
end;

procedure ShowTitle;
begin
  Writeln(TEXT_APPLICATION_TITLE_LONG);
  Writeln(TEXT_VERSION,SPACE,TEXT_APPLICATION_VERSION_NUMBER);
  Writeln(TEXT_APPLICATION_COPYRIGHT);
end;

{-----------------------------------------------------------------------------}

{Log File}

function  MakeLogFileName(const FileName__:String; Width__,Height__,PlayerPositionIndex__,SegmentIndex__,SegmentCount__,AdHocTask__:Integer):String;
begin
  if   (Width__>0) and (Height__>0) then
       Result:=GeneratorTaskCaption(FileName__,Width__,Height__,PlayerPositionIndex__,SegmentIndex__,SegmentCount__,AdHocTask__)
  else Result:=FileName__;
end;

function  CloseLogFile(RenameToSokobanFile__:Boolean):Boolean;
var Index:Integer;
    OldName,NewName:String;
begin {$I-}
  if   LogFile.FileName<>'' then begin
       CloseFile(LogFile.TextFile); Result:=IOResult=0;

       if Result and RenameToSokobanFile__ then begin
           OldName:=LogFile.FileName;
           Index:=Pos(LOG_FILE_EXT,OldName);
           if   Index>0 then
                NewName:=Copy(OldName,1,Pred(Index))+SOKOBAN_FILE_EXT
           else NewName:=OldName+SOKOBAN_FILE_EXT;
           Result:=OldName=NewName;
           if not Result then begin
              Assign(LogFile.TextFile,OldName);
              Rename(LogFile.TextFile,NewName);
              Result:=IOResult=0;
              if   Result then
                   LogFile.FileName:=NewName
              else Msg(TEXT_FILE_RENAME_ERROR+COLON+SPACE+OldName+' -> '+NewName,TEXT_APPLICATION_TITLE);
              end;
          end;
       end
  else Result:=True;
  if   Result then LogFile.FileName:=''
  else Msg('Close log file: File error on file: '+LogFile.FileName,TEXT_APPLICATION_TITLE);
end; {$I+}

function  CreateLogFile(const FileName__:String):Boolean;
var i:Integer; s1,s2:String; TemporaryFile:TextFile;
begin {$I-}
  Result:=False; i:=-1;
  if CloseLogFile(False) and LogFile.Enabled then with LogFile do begin
     s1:=FileNameWithExtension(FileName__,'');
     repeat Inc(i);
            if   i=0 then s2:=''
            else s2:=SPACE+IntToStr(i);
            FileName:=FileNameWithExtension(s1+s2,LOG_FILE_EXT);
            Assign(TextFile,FileName); Rewrite(TextFile); Result:=IOResult=0;

            if Result then begin
               {when the program terminates, the log file will be renamed to a
                Sokoban puzzle file; to avoid confusion while the calculation is
                in progress, delete any existing file with that name;}
               s2:=s1+SOKOBAN_FILE_EXT;
               Assign(TemporaryFile,s2);
               Erase(TemporaryFile); {returns a non-zero IO-error also if the file doesn't exist}
               if IOResult<>0 then begin
                  {ignore IO-errors for now; if the file exists and couldn't be
                   deleted, then the final renaming of the log file will fail;}
                  end;
               end;
     until  Result or (i=High(i));
     if not Result then begin
        Msg('Create log file: Create error on file: '+FileName,TEXT_APPLICATION_TITLE);
        LogFile.FileName:='';
        end;
     end;
end; {$I+}

function  FlushLogFile:Boolean; {flush the log-file, if any}
var s:String;
begin {$I-}
  Result:=LogFile.FileName='';
  if not Result then begin
     s:=LogFile.FileName;
     Result:=CloseLogFile(False);
     if Result then begin
        Append(LogFile.TextFile); {re-open the log-file so it's ready for adding more information later}
        Result:=IOResult=0;
        if   Result then LogFile.FileName:=s {restore the file-name}
        else Msg('Append to log file: File error on file: '+s,TEXT_APPLICATION_TITLE);
        end;
     end;
end; {$I+}

function  WriteBoardToLogFile(const Text__:String; Margin__:Integer):Boolean;
var i:Integer; Margin:String; BoardAsTextLines:TBoardAsTextLines;
begin {$I-}
  Result:=LogFile.FileName<>'';
  if Result then with LogFile do begin
     BoardToTextLines(BoardAsTextLines);
     Margin:='';
     for i:= 1 to Margin__ do Margin:=Margin+SPACE;
     if Text__<>'' then begin
        if Margin__>0 then Write(TextFile,Margin);
        Writeln(TextFile,Text__);
        end;
     for i:=1 to Game.BoardHeight do begin
         if Margin__>0 then Write(TextFile,Margin);
         Writeln(TextFile,BoardAsTextLines[i]);
         end;
     //if Text__<>'' then begin
     //   if Margin__>0 then Write(TextFile,Margin);
     //   Writeln(TextFile,Text__);
     //   end;
     Result:=IOResult=0;
     end;
end; {$I+}

function  WriteOrimazeBoardToLogFile(const Title__,Text__:String; Margin__:Integer; const Board__:TOrimazeBoard):Boolean;
var i:Integer; Margin:String; BoardAsTextLines:TBoardAsTextLines;
begin {$I-}
  Result:=LogFile.FileName<>'';
  if Result then with LogFile do begin
     OrimazeBoardToTextLines(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,Board__,BoardAsTextLines);
     Margin:='';
     for i:= 1 to Margin__ do Margin:=Margin+SPACE;
     if Title__<>'' then begin
        if Margin__>0 then Write(TextFile,Margin);
        Writeln(TextFile,Title__);
        end;
     for i:=1 to Game.OrimazeBoardHeight do begin
         if Margin__>0 then Write(TextFile,Margin);
         Writeln(TextFile,BoardAsTextLines[i]);
         end;
     if Text__<>'' then begin
        if Margin__>0 then Write(TextFile,Margin);
        Writeln(TextFile,Text__);
        end;
     Result:=IOResult=0;
     end;
end; {$I+}

function  WritelnToLogFile(const Text__:String):Boolean;
begin {$I-}
  if   LogFile.FileName<>'' then with LogFile do begin
       Writeln(TextFile,Text__);
       Result:=IOResult=0;
       end
  else Result:=False;
end; {$I+}

{-----------------------------------------------------------------------------}

{Board}

function  BoardToText(const LineTerminator__:String):String;
var Row:Integer; s:String; BoardAsTextLines:TBoardAsTextLines;
begin
  Result:='';
  BoardToTextLines(BoardAsTextLines);
  for Row:=1 to Game.BoardHeight do begin
      s:=BoardAsTextLines[Row];
      if Length(s)>Game.BoardWidth then s:=Copy(s,1,Game.BoardWidth); {this shouldn't happen}
      while Length(s)<Game.BoardWidth do s:=s+SPACE;
      if   Result<>'' then
           Result:=Result+LineTerminator__+s
      else Result:=s;
      end;

end;

procedure BoardToTextLines(var BoardAsTextLines__:TBoardAsTextLines);
var Col,Row:Integer;
begin
  with Game do begin
    for Row:=1 to BoardHeight do begin
        BoardAsTextLines__[Row]:='';
        for Col:=1 to BoardWidth do
            BoardAsTextLines__[Row]:=BoardAsTextLines__[Row]+
                                     SquareToChar(Col,Row);
        end;
    end;
end;

procedure OrimazeBoardToTextLines(BoardWidth__,BoardHeight__:Integer; const Board__:TOrimazeBoard; var BoardAsTextLines__:TBoardAsTextLines);
var Index,Col,Row:Integer;
begin
    Index:=0;
    for Row:=1 to BoardHeight__ do begin
        BoardAsTextLines__[Row]:='';
        for Col:=1 to BoardWidth__ do begin
            Inc(Index);
            BoardAsTextLines__[Row]:=BoardAsTextLines__[Row]+OrimazeSquareToChar(Index,Board__);
            end;
        end;
end;

function  CalculateHashValue:THashValue;
var BoxNo,Square:Integer;
begin {only box positions are taken into account; the player position isn't considered, it's saved separately in each node}
  Result:=0;
  for BoxNo:=1 to Game.BoxCount do begin
      Square:=Game.BoxPos[BoxNo];
      if Square<>0 then {'True': the box is on the board, i.e., it hasn't been removed temporarily}
         Result:=Result xor Positions.SquareHashValues[Square];
      end;
end;

function  CalculateSimpleLowerBound:Integer;
var BoxNo,Distance,Square:Integer;
begin {calculates distance to nearest goal for all boxes}
  Result:=0;
  for BoxNo:=1 to Game.BoxCount do begin
      Square:=Game.BoxPos[BoxNo];
      if Square<>0 then begin {'True': the box is on the board, i.e., it hasn't been removed temporarily}
         Distance:=Game.DistanceToNearestGoal[Square];
         if   Result<INFINITY then Inc(Result,Distance)
         else Inc(Result); {the result is meaningless if a box cannot reach a target position}
         end;
      end;
end;

function  CalculateOrimazeSimpleLowerBound(const FromBoard__,ToBoard__:TOrimazeBoard):Integer;
var Index:Integer;
begin {counts number of misplaced tiles}
 Result:=0;
 for Index:=1 to Game.OrimazeGridSquares.Count do
     if FromBoard__[Index]<>ToBoard__[Index] then
       Inc(Result);
end;

procedure ClearBoard(BoardWidth__,BoardHeight__:Integer);
var Col,Row:Integer;
begin
  with Game do begin
    InitializeBoard(BoardWidth__,BoardHeight__);
    for Row:=2 to Pred(BoardHeight) do begin {2..Pred(Height): leave a wall border}
        for Col:=2 to Pred(BoardWidth) do Board[Col,Row]:=FLOOR;
        end;
    PlayerPos.Col:=0; PlayerPos.Row:=0; BoxCount:=0; GoalCount:=0; SimpleLowerBound:=0;
    HashValue:=CalculateHashValue;
    end;
end;

procedure InitializeBoard(BoardWidth__,BoardHeight__:Integer);
var Col,Row:Integer;
begin
  with Game do begin
    BoardWidth:=BoardWidth__; BoardHeight:=BoardHeight__;
    BoardSize:=(BoardWidth__+2)*(BoardHeight__+2); {the extra 2 is for a wall-filled border}
    for Col:=0 to Succ(BoardWidth__) do
        for Row:=0 to Succ(BoardHeight__) do
            Board[Col,Row]:=WALL;
    end;
end;


function  NormalizeBoard(MovePlayerAndBoxes__,
                         ChangeImmovableBoxesOnNonGoalSquaresToWalls__,
                         ChangeImmovableBoxesOnGoalSquaresToWalls__,
                         FillUnnecessaryIllegalBoxSquares__,
                         StableNormalization__:Boolean;
                         var BoardWidth__,BoardHeight__:Integer; var Board__:TBoard;
                         var PlayerPos__:TColRow; var History__:THistory):Integer;
{
 Fills jagged (i.e., "stair-case" like) wall edges;
 Removes redundant walls and floors, and removes unused boxes and goals;
 Returns a non-zero number if some of the squares on the board are changed;
 The function works with multiple players on the board, i.e., the 'Multiban' variant of the game;
}
var Col,Row,NeighborCol,NeighborRow,QueueIndex,QueueTop,OriginalBoardWidth,OriginalBoardHeight:Integer;
    Direction:TDirection;
    Queue:array[0..MAX_BOARD_SIZE] of TColRow;
    OriginalBoard:TBoard;
    Visited:TBoardOfBooleans;

  function  ColRowToSquare(Col__,Row__:Integer):Integer;
  begin
    ColRowToSquare:=Row__ * (BoardWidth__+2) + Col__;
  end;

  procedure SquareToColRow(Square__:Integer; var Col__,Row__:Integer);
  begin
    Row__:=Square__   div   (BoardWidth__+2);
    Col__:=Square__ - Row__*(BoardWidth__+2);
  end;

  function ChangeFrozenBoxesToWalls(ChangeImmovableBoxesOnNonGoalSquaresToWalls__,ChangeImmovableBoxesOnGoalSquaresToWalls__:Boolean;
                                    BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Integer;
  var Col,Row,SquareValue:Integer; BoardTimestamps:TBoardTimeStamps;

    function  IsAFreezingMove ( FromCol__ , FromRow__ , ToCol__ , ToRow__ : Integer ) : Boolean;
    {returns 'True' if putting a box on [ 'ToCol__' , 'ToRow__' ] creates a
     deadlocked frozen position (optionally moving the box from
     [ 'FromCol__' , 'FromRow__' ]);
    }
    var OriginalFromSquareValue : Integer; ABoxIsBlockedOnANonGoalSquare : Boolean;

      function  BoxIsBlockedAlongOneAxis( Col__, Row__ : Integer; Direction__ : TDirection;
                                         var ABoxIsBlockedOnANonGoalSquare__ : Boolean):Boolean;
      var Neighbor1, Neighbor2 : Integer;
          Neighbor1Position , Neighbor2Position : TColRow;
      begin
        if   Direction__              = Low  ( Direction__ ) then                {flip horizontal/vertical direction}
             Direction__             := Succ ( Low ( Direction__ ) )             {caution: 'Succ(Low...'): assumes 4 directions only}
        else Direction__             := Low  ( Direction__ );

        if   ( Direction__            = Low ( Direction__ ) )
             and
             (BoardTimestamps.Squares [ Col__ , Row__ ] >= BoardTimestamps.Timestamp) then {'True': use the already calculated value}

             Result                  := BoardTimestamps.Squares [ Col__ , Row__ ] > BoardTimestamps.Timestamp {relies on Ord ( False , True ) = (0 , 1)}

        else begin
               Neighbor1Position.Col  := Col__ - COMPASS_DIRECTIONS [ Direction__ ].Col;
               Neighbor1Position.Row  := Row__ - COMPASS_DIRECTIONS [ Direction__ ].Row;
               Neighbor1            := Board__ [ Neighbor1Position.Col , Neighbor1Position.Row ];

               Neighbor2Position.Col  := Col__ + COMPASS_DIRECTIONS [ Direction__ ].Col;
               Neighbor2Position.Row  := Row__ + COMPASS_DIRECTIONS [ Direction__ ].Row;
               Neighbor2            := Board__ [ Neighbor2Position.Col , Neighbor2Position.Row ];

               Inc ( Board__ [ Col__ , Row__ ] , WALL);                         {temporarily change this square to a wall}

               Result := ((  Neighbor1 and (WALL + FLAG_INVISIBLE_WALL)) <> 0 )
                         or                                                     {is there a wall on any of the neighbor squares?}
                         ((  Neighbor2 and (WALL + FLAG_INVISIBLE_WALL)) <> 0 )
                         or                                                     {are both neighbors illegal squares?}
                         ((( Neighbor1 and FLAG_ILLEGAL_BOX_SQUARE         ) <> 0 )
                          and
                          (( Neighbor2 and FLAG_ILLEGAL_BOX_SQUARE         ) <> 0 )
                         );

               if (not ( Result and ABoxIsBlockedOnANonGoalSquare__ ))          {it's not enough to know whether a box is blocked; it's also necessary to know whether a box is blocked on a non-goal square}
                  and
                  (( Neighbor1 and ( WALL + BOX ) ) = BOX )                     {test if neighbor1 is a blocked box}
                  and
                  BoxIsBlockedAlongOneAxis( Neighbor1Position.Col , Neighbor1Position.Row , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
                  then Result := True;

               if (not ( Result and ABoxIsBlockedOnANonGoalSquare__ ))          {it's not enough to know whether a box is blocked; it's also necessary to know whether a box is blocked on a non-goal square}
                  and
                  (( Neighbor2 and ( WALL + BOX ) ) = BOX )                     {test if neighbor2 is a blocked box}
                  and
                  BoxIsBlockedAlongOneAxis( Neighbor2Position.Col , Neighbor2Position.Row , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
                  then Result:=True;

               Dec ( Board__ [ Col__ , Row__ ] , WALL );                        {remove the wall again}
             end;

        if   Result and                                                         {if this box is blocked}
             ( ( Board__ [ Col__ , Row__ ] and GOAL ) = 0 ) then                {and it's not a goal-square}
             ABoxIsBlockedOnANonGoalSquare__ := True;                           {then set the flag}

        if   Direction__ = Low ( Direction__ ) then with BoardTimestamps do     {reduce the exponential growth by storing the results for one axis}
             Squares [ Col__ , Row__ ] := Timestamp + Cardinal ( Ord ( Result ) ); {relies on Ord ( False , True ) = (0 , 1)}

      end; {IsAFreezingMove.BoxIsBlockedAlongOneAxis}

    begin {IsAFreezingMove}
      with BoardTimestamps do
        if Timestamp < High ( Timestamp ) - 5 then
           Inc ( Timestamp , 2 )
        else begin
           FillChar ( Squares , SizeOf ( Squares ) , 0 ); Timestamp := 2;
          end;

      ABoxIsBlockedOnANonGoalSquare := False;

      OriginalFromSquareValue := Board__ [ FromCol__ , FromRow__ ];
      Board__ [ FromCol__ , FromRow__ ] := Board__ [ FromCol__ , FromRow__ ] and (not BOX); {remove box, if any (the from-square is optional), from its current position}

      Result := (( Board__ [ ToCol__ , ToRow__ ] and ( WALL {+ FLAG_ILLEGAL_BOX_SQUARE} ) ) <> 0 )   {a wall is considered a deadlocked square}
                or
                (
{
                 ( not ReverseMode )               // this must be checked in normal game play, but it can be ignored by level normalization
                 and
}
                 ( (
                     BoxIsBlockedAlongOneAxis ( ToCol__ , ToRow__ , Low (       TDirection )   , ABoxIsBlockedOnANonGoalSquare )
                     and
                     BoxIsBlockedAlongOneAxis ( ToCol__ , ToRow__ , Succ( Low ( TDirection ) ) , ABoxIsBlockedOnANonGoalSquare ) {caution: 'Succ(Low...'): assumes 4 directions only}
{
                     and
                     ABoxIsBlockedOnANonGoalSquare // this must be checked in normal game play, but it can be ignored by level normalization
}
                   )
                 )
                );

      Board__ [ FromCol__ , FromRow__ ] := OriginalFromSquareValue;             {put box, if any, back on the board}
    end; {IsAFreezingMove}

  begin
    Result:=0;
    BoardTimeStamps.Timestamp:=High(BoardTimeStamps.Timestamp); {initialize the board square timestamps}

    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do begin
            SquareValue:=Board__[Col,Row];
            if   ((SquareValue and BOX)<>0) {'True': the square contains a box}
                 and
                 ((ChangeImmovableBoxesOnNonGoalSquaresToWalls__ and ((SquareValue and GOAL)= 0))
                  {changing a frozen box on a non-goal square to a wall changes
                   the level status from 'unsolvable' to 'invalid' because it
                   creates a mismatch between the number of boxes and the number
                   of goals, hence, normally this operation is only allowed when
                   level normalization is called from a level editor;
                  }
                  or
                  (ChangeImmovableBoxesOnGoalSquaresToWalls__    and ((SquareValue and GOAL)<>0))
                 )
                 and
                 IsAFreezingMove(0,0,Col,Row) then begin {'True': the box cannot move; change it to a wall}
                 Board__[Col,Row]:=WALL;
                 Inc(Result);
                 end;
            end;
  end; {ChangeFrozenBoxesToWalls}

  function FillTubes(MovePlayerAndBoxes__,ChangeImmovableBoxesOnNonGoalSquaresToWalls__,ChangeImmovableBoxesOnGoalSquaresToWalls__,StableNormalization__:Boolean):Integer;
  var Col,Row,BoxCount,BoxOnGoalCount,NeighborFloorCount,SquareValue:Integer;
      Again,DeadEnd:Boolean; Dir,Direction:TDirection;
      PlayerPos,NewBoxPos,NewPlayerPos:TColRow;
      BlockedAxes:TAxisSet;
      IsACornerSquare:TBoardOfBooleans;

    function GetSquareValue(Col__,Row__,Distance__:Integer; Direction__:TDirection):Integer;
    begin {returns the value of the square 'Distance__' squares away from the square [Col__,Row__] in the given direction}
      Result:=Board__[Max(0,Min(MAX_BOARD_WIDTH +1,Col__+Distance__*COMPASS_DIRECTIONS[Direction__].Col)),
                      Max(0,Min(MAX_BOARD_HEIGHT+1,Row__+Distance__*COMPASS_DIRECTIONS[Direction__].Row))];
    end;

    function IsABulge(Col__,Row__:Integer; Direction__:TDirection):Boolean;
    var NeighborCol,NeighborRow,SquareValue,NeighborSquareValue:Integer; Dir:TDirection;
    begin
      {a "bulge" consists of the squares marked "X" and "x" on the following}
      {drawings; the "X" squares don't contain boxes or goals, and the bulge is}
      {restricted by the depicted walls}
      {}
      {variant A:}
      {row 1 :  ?##?  } {'Direction__' is ->}
      {row 2 :  #XX#  } {'Col__,Row__' is the square depicted by the leftmost "X"}
      {row 3a:  ?ff?  } {the squares in the bulge (the "X" squares) must not contain a player, or the "f" square next to the player is an empty floor}
      {row 4a:  ?##?  } {all squares marked by '?' don't matter}
      {}
      {variant B:}
      {row 1 :  ?##?  } {'Direction__' is ->}
      {row 2 :  #XX#  } {'Col__,Row__' is the square depicted by the leftmost "X"}
      {row 3b:  ?ff?  } {the squares in the bulge (the "X" squares) must not contain a player, or the "f" square next to the player is an empty floor}
      {row 4b:  ?#Y?  } {"Y" is a non-goal floor}
      {row 5b:  ??#?  } {all squares marked by '?' don't matter}
      {}
      {variant C:}
      {row 1 :  ?##?  } {'Direction__' is ->}
      {row 2 :  #XX#  } {'Col__,Row__' is the square depicted by the leftmost "X"}
      {row 3c:  #xZ?  } {square "Z" must not contain a box if there is a player in the bulge (X" and "x" squares), otherwise filling an "X" square with a wall has an influence on the possible outwards pushes for the box at "Z"}
      {row 4c:  ?#??  } {all squares marked by '?' don't matter}
      {}
      Result             :=False;
      NeighborCol        :=Col__+COMPASS_DIRECTIONS[Direction__].Col;
      NeighborRow        :=Row__+COMPASS_DIRECTIONS[Direction__].Row;
      SquareValue        :=Board__[Col__,Row__];             {the contents of the anchor square, i.e., the leftmost "X" on the drawings}
      NeighborSquareValue:=Board__[NeighborCol,NeighborRow]; {the contents of the other square in the bulge, i.e., the rightmost "X" on the drawings}

      if IsACornerSquare[NeighborCol,NeighborRow  ] and
         IsACornerSquare[Col__      ,Row__        ] and
         ((NeighborSquareValue                                   and (GOAL + BOX + WALL))                        = 0) and
         ((SquareValue                                           and (GOAL + BOX + WALL))                        = 0) and  {'True': row 2 contains the 2 corner squares without boxes and goals}
         ((GetSquareValue       (Col__           ,Row__      ,1,OPPOSITE_DIRECTION[Direction__]) and WALL       )<>0) and
         ((GetSquareValue       (NeighborCol     ,NeighborRow,1,Direction__                    ) and WALL       )<>0) then {'True': row 2 contains the enclosing walls at the ends}
         for Dir:=Low(Dir) to High(Dir) do
             if (Dir<>Direction__) and (Dir<>OPPOSITE_DIRECTION[Direction__]) and {try all directions; 'FillTubes' is independent of the number of directions and the number of players}
                ((GetSquareValue(NeighborCol     ,NeighborRow,1,Dir                    )         and WALL       )<>0)
                and
                ((GetSquareValue(Col__           ,Row__      ,1,Dir                    )         and WALL       )<>0)      {'True': row 1 contains the 2 required walls}
                and
                (((SquareValue                                                                   and PLAYER     )= 0)
                 or                                                                                                        {'True': the leftmost  "X" does not contain a player, or there is an empty floor square ("f") next to the player in the aisle}
                 ((GetSquareValue(Col__          ,Row__      ,1,OPPOSITE_DIRECTION[Dir])         and (WALL+BOX) )= 0)
                )
                and
                (((NeighborSquareValue                                                           and PLAYER     )= 0)
                 or                                                                                                        {'True': the rightmost "X" does not contain a player, or there is an empty floor square ("f") next to the player in the aisle}
                 ((GetSquareValue(NeighborCol    ,NeighborRow,1,OPPOSITE_DIRECTION[Dir])         and (WALL+BOX) )= 0)
                )
                then
                if ({variant a}
                    ((GetSquareValue(NeighborCol ,NeighborRow,2,OPPOSITE_DIRECTION[Dir])         and WALL       )<>0) and
                    ((GetSquareValue(Col__       ,Row__      ,2,OPPOSITE_DIRECTION[Dir])         and WALL       )<>0)      {'True': row 4a contains the 2 required walls}
                   )
                   or
                   ({variant b}
                    ((GetSquareValue(Col__       ,Row__      ,2,OPPOSITE_DIRECTION[Dir])         and WALL       )<>0) and
                    ((GetSquareValue(NeighborCol ,NeighborRow,3,OPPOSITE_DIRECTION[Dir])         and WALL       )<>0) and  {'True': row 5b contains the required wall}
                    ((GetSquareValue(NeighborCol ,NeighborRow,2,OPPOSITE_DIRECTION[Dir])         and (WALL+GOAL))= 0)      {'True': row 4b contains the required non-goal floor "Y"}
                   )
                   or
                   ({variant c}
                    ((GetSquareValue(Col__-COMPASS_DIRECTIONS[Direction__].Col,
                                     Row__-COMPASS_DIRECTIONS[Direction__].Row,
                                                              1,OPPOSITE_DIRECTION[Dir ])        and WALL       )<>0)      {'True': row 3c contains the required wall}
                    and
                    (((GetSquareValue(NeighborCol,NeighborRow,1,OPPOSITE_DIRECTION[Dir])         and BOX        )= 0)      {'True': row 3c doesn't contain a box on square "Z"}
                     or
                     (((NeighborSquareValue                                                      and PLAYER     )= 0)
                      and
                      ((SquareValue                                                              and PLAYER     )= 0)
                      and
                      ((GetSquareValue(Col__     ,Row__      ,1,OPPOSITE_DIRECTION[Dir])         and PLAYER     )= 0)      {'True': there are no player inside the bulge, i.e, there are no players on the 2 "X" squares or the "x" square in row 3c}
                     )
                    )
                    and
                    ((GetSquareValue(Col__       ,Row__      ,2,OPPOSITE_DIRECTION[Dir])         and WALL       )<>0)      {'True': row 4c contains the required wall}
                   ) then
                   Result:=True;
    end;

  begin  {FillTubes}
    Result:=0;
    BoxCount:=0; BoxOnGoalCount:=0; PlayerPos.Col:=0; Direction:=dUp;
    FillChar(IsACornerSquare,SizeOf(IsACornerSquare),0);
    {TubeFillingMoveCount:=0; TubeFillingPushCount:=0;} {player moves and box pushes}
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do begin
            SquareValue:=Board__[Col,Row];
            if (SquareValue and BOX       )<>0        then Inc(BoxCount );
            if (SquareValue and (BOX+GOAL))= BOX+GOAL then Inc(BoxOnGoalCount);
            if (SquareValue and PLAYER    )<>0        then begin PlayerPos.Col:=Col; PlayerPos.Row:=Row; end;
            end;

    repeat
      Again:=False;
      for Col:=1 to BoardWidth__ do
          for Row:=1 to BoardHeight__ do begin
              SquareValue:=Board__[Col,Row];
              if (SquareValue and WALL)=0 then begin {for each floor-square...}
                 if StableNormalization__ then
                    Board__[Col,Row]:=SquareValue and (not FLAG_ILLEGAL_BOX_SQUARE); {remove the old 'illegal square' flag, if any; this is necessary to guarantee a stable normalization; the original board may contain more accurate 'illegal square' flags}
                 NeighborFloorCount:=0; BlockedAxes:=[];
                 for  Dir:=Low(Dir) to High(Dir) do
                      if   (GetSquareValue(Col,Row,1,Dir) and WALL)=0 then begin
                           Inc(NeighborFloorCount); Direction:=Dir;
                           end
                      else Include(BlockedAxes,DIRECTION_TO_AXIS[Dir]);
                 IsACornerSquare[Col,Row]:=BlockedAxes=ALL_AXES_SET;
                 if   (BlockedAxes=ALL_AXES_SET) and ((SquareValue and (BOX+GOAL))=0) then {'True': the corner square is neither an empty goal square nor a goal square with a box}
                      Board__[Col,Row]:=Board__[Col,Row] or FLAG_ILLEGAL_BOX_SQUARE;
                 if   ((SquareValue and BOX)<>0)
                      and
                      (BlockedAxes=ALL_AXES_SET) {the box is on a corner square and cannot move}
                      and
                      ((ChangeImmovableBoxesOnNonGoalSquaresToWalls__ and ((SquareValue and GOAL)= 0))
                       {changing a frozen box on a non-goal square to a wall changes
                        the level status from 'unsolvable' to 'invalid' because it
                        creates a mismatch between the number of boxes and the number
                        of goals, hence, normally this operation is only allowed when
                        level normalization is called from a level editor;
                       }
                       or
                       (ChangeImmovableBoxesOnGoalSquaresToWalls__    and ((SquareValue and GOAL)<>0))
                      ) then begin
                      NeighborFloorCount:=0; {'0': put a wall on the square instead of the box}
                      Dec(BoxCount); {update counts}
                      if (SquareValue and (BOX+GOAL))=BOX+GOAL then Dec(BoxOnGoalCount); {update counts}
                      end
                 else if ((SquareValue and (BOX+GOAL))<>0) then {goals and boxes must not be touched, even if they are in a dead end tunnel}
                         NeighborFloorCount:=DIRECTION_COUNT+2 {leave the square untouched; '+2': just to be sure to that the 'if NeighborFloorCount<=1' statement below will fail}
                      else if BlockedAxes=ALL_AXES_SET then
                              if NeighborFloorCount=2 then
                                 for  Dir:=Low(Dir) to High(Dir) do
                                      if IsABulge(Col,Row,Dir) then
                                         if      (GetSquareValue(Col,Row,1,Dir) and PLAYER)=0 then begin {'True': the neighbor square doesn't contain a player; fill it with a wall}
                                                 Board__        [Col+COMPASS_DIRECTIONS[Dir].Col,Row+COMPASS_DIRECTIONS[Dir].Row]:=WALL; Again:=True; Inc(Result);
                                                 IsACornerSquare[Col+COMPASS_DIRECTIONS[Dir].Col,Row+COMPASS_DIRECTIONS[Dir].Row]:=False;
                                                 end
                                         else if (Board__[Col,Row] and PLAYER)=0 then begin
                                                 Board__        [Col,Row]:=WALL; Again:=True; Inc(Result);
                                                 IsACornerSquare[Col,Row]:=False;
                                                 end;

                 if NeighborFloorCount<=1 then begin {'True': it's an empty floor-square surrounded by 3 or 4 walls, or it's a corner square with a box}
                    DeadEnd:=True; NewBoxPos.Col:=0; NewPlayerPos.Col:=0;
                    if (Col=PlayerPos.Col) and (Row=PlayerPos.Row) then
                       if (NeighborFloorCount=0) or (not MovePlayerAndBoxes__) then
                          DeadEnd:=False {the player is on an isolated square, or moving player and boxes isn't allowed; don't change anything}
                       else begin        {try to move the player to the neighbor floor-square}
                          NewPlayerPos.Col:=Col+COMPASS_DIRECTIONS[Direction].Col;
                          NewPlayerPos.Row:=Row+COMPASS_DIRECTIONS[Direction].Row;
                          if (Board__[NewPlayerPos.Col,NewPlayerPos.Row] and BOX)<>0 then begin
                             NewBoxPos.Col:=NewPlayerPos.Col+COMPASS_DIRECTIONS[Direction].Col;
                             NewBoxPos.Row:=NewPlayerPos.Row+COMPASS_DIRECTIONS[Direction].Row;
                             DeadEnd:=((Board__[NewBoxPos.Col,NewBoxPos.Row] and (WALL+BOX))=0) and {the box can be pushed forward}
                                      (BoxOnGoalCount<BoxCount);                {the position isn't a solution}
                             end;
                          end;
                    if DeadEnd then begin {dead end: place a wall on the square}
                       if NewBoxPos.Col<>0 then begin {move a box}
                          if (Board__[NewPlayerPos.Col,NewPlayerPos.Row] and GOAL)<>0 then Dec(BoxOnGoalCount);
                          Dec(Board__[NewPlayerPos.Col,NewPlayerPos.Row],BOX);
                          Inc(Board__[NewBoxPos.Col,NewBoxPos.Row],BOX);
                          if (Board__[NewBoxPos.Col,NewBoxPos.Row] and GOAL)<>0 then Inc(BoxOnGoalCount);
                          {Inc(TubeFillingPushCount);}
                          end;
                       if NewPlayerPos.Col<>0 then begin {move the player}
                          Dec(Board__[PlayerPos.Col,PlayerPos.Row],PLAYER);
                          PlayerPos.Col:=NewPlayerPos.Col; PlayerPos.Row:=NewPlayerPos.Row;
                          Inc(Board__[PlayerPos.Col,PlayerPos.Row],PLAYER);
                          {Inc(TubeFillingMoveCount);}
                          with History__ do begin
                            {the history isn't fully updated with statistics
                             like 'PushCount', 'PlayerLines' etc.;
                             only move directions and box-push flags are stored;
                            }
                            Inc(Count);
                            Moves[Count].BoxNo:=0; {undefined}
                            Moves[Count].Direction:=Direction;
                            Moves[Count].Flags:=[];
                            if NewBoxPos.Col<>0 then Include(Moves[Count].Flags,mfPush);
                            {updating the history 'Top' in the next code line is commented out
                             because it wasn't there from the beginning, and even though the
                             update seems natural, it's unknown whether there is code somewhere
                             which depends on 'Top' not being updated;
                            }
                            {if Count>Top then Top:=Count;}
                            end;
                          end;
                       Board__        [Col,Row]:=WALL; Again:=True; Inc(Result);
                       IsACornerSquare[Col,Row]:=False;
                       end;
                    end;
                 end;
              end;
    until not Again;
  end; {FillTubes}

  function FillUnnecessaryIllegalBoxSquares:Integer;
  {fills unnecesssary squares which are:
   1) illegal box squares, and
   2) unnecessary for moving the player around between all legal box squares
  }
  type
    TBoardSquareSet = record
      Count         : Integer;
      Squares       : array[0..MAX_BOARD_SIZE+1] of TColRow;
    end;
  var
    Col,Row,i,Index,{BoxLegalSquaresCount,}PlayerMovesCount:Integer;
    Again:Boolean;
    Direction:TDirection;
    TimeMS:TTimeMS;
    FromSquare,ToSquare,NeighborSquare:TColRow;
    BoardTimeStamps:TBoardTimeStamps;
    PlayerAccessArea,NewPlayerAccessArea,SquaresWithALegalBoxSquareNeighbor:TBoardSquareSet;

    function CalculatePlayerPathExistsOrCalculatePlayerAccessArea(const FromSquare__,ToSquare__:TColRow; var BoardTimeStamps__:TBoardTimeStamps; var PlayerAccessArea__:TBoardSquareSet):Boolean;
    {depending on 'ToSquare__.X', the function performs two different tasks:
     1) 'ToSquare__.X =  0 : calculates the player access area including the square 'FromSquare__'; precondition: 'ToSquare__.Y' = 0 too
     2) 'ToSquare__.X <> 0 : returns 'True' if there is a player path between the squares 'FromSquare__' and 'ToSquare__'; only wall squares are taken into account, i.e., any boxes on the board are not taken into account
    }
    type
      TQueueItem=packed record Square:TColRow; end;
      TQueue=record
        Bottom,Top:Integer;
        Items:array[0..MAX_BOARD_SIZE+1] of TQueueItem;
      end;
    var
      PlayerSquare,NeighborSquare:TColRow;
      Direction:TDirection;
      Queue:TQueue;
    begin
      if BoardTimeStamps__.TimeStamp<High(BoardTimeStamps__.TimeStamp) then
         Inc(BoardTimeStamps__.TimeStamp)
      else begin {timestamp wrap around; clear the square timestamps and start from the beginning with timestamp = '1'}
         BoardTimeStamps__.TimeStamp:=1;
         FillChar(BoardTimeStamps__.Squares,SizeOf(BoardTimeStamps__.Squares),0);
         end;

      Queue.Bottom:=0;
      if   (Board__[FromSquare__.Col,FromSquare__.Row] and WALL)=0 then begin       {'True': the 'FromSquare__' isn't a wall square}
           Queue.Top:=1; Queue.Items[1].Square:=FromSquare__;                   {put the 'FromSquare__' on the queue}
           BoardTimeStamps__.Squares[FromSquare__.Col,FromSquare__.Row]:=BoardTimeStamps__.Timestamp; {mark the 'FromSquare__' as visited}
           end
      else Queue.Top:=0;                                                        {'0': the 'FromSquare__' is a wall square, hence, the function has nothing to do}

      PlayerAccessArea__.Count:=0;
      if (ToSquare__.Col=0) and (Queue.Top<>0) then with PlayerAccessArea__ do begin {'True': the function calculates the player access area including the square 'FromSquare__'}
         Inc(Count); Squares[Count]:=FromSquare__;
         end;
      Inc(PlayerMovesCount);

      {perform a breadth-first search for a path from 'FromSquare__' to 'ToSquare__'}
      while (Queue.Bottom<Queue.Top) and {while there are more items on the queue}
            (BoardTimeStamps__.Squares[ToSquare__.Col,ToSquare__.Row]<>BoardTimeStamps__.TimeStamp) do begin {while the destination square hasn't been visited}
            Inc(Queue.Bottom);                                                  {advance to the next unprocessed item on the queue}
            PlayerSquare:=Queue.Items[Queue.Bottom].Square;                     {get the next item from the queue}

            for Direction:=Low(Direction) to High(Direction) do begin
                NeighborSquare.Col:=PlayerSquare.Col+COMPASS_DIRECTIONS[Direction].Col;
                NeighborSquare.Row:=PlayerSquare.Row+COMPASS_DIRECTIONS[Direction].Row;

                if (BoardTimeStamps__.Squares[NeighborSquare.Col,NeighborSquare.Row]<>BoardTimeStamps__.TimeStamp) and {is the neighbor square an unvisited square?}
                   ((Board__[NeighborSquare.Col,NeighborSquare.Row] and (WALL))=0) then begin {is the neighbor square a floor square or a wall?}
                   Inc(Queue.Top);                                              {put the neighbor square on the queue}
                   Queue.Items[Queue.Top].Square:=NeighborSquare;
                   BoardTimeStamps__.Squares[NeighborSquare.Col,NeighborSquare.Row]:=BoardTimeStamps__.TimeStamp; {mark the neighbor square as visited}
                   if ToSquare__.Col=0 then with PlayerAccessArea__ do begin      {'True': the function calculates the player access area including the square 'FromSquare__'}
                      Inc(Count); Squares[Count]:=NeighborSquare;
                      end;
                   Inc(PlayerMovesCount);
                   end;
                end;
            end;
      if   ToSquare__.Col=0 then {'True': the function calculates the player access area including the square 'FromSquare__'}
           Result:=PlayerAccessArea__.Count<>0 {this is always true}
      else Result:=BoardTimeStamps__.Squares[ToSquare__.Col,ToSquare__.Row]=BoardTimeStamps__.TimeStamp; {'True': there is a player path between the given squares}
    end;

    function CalculateIllegalOrUnreachableBoxSquares(PushBoxes__,InitializeIllegalSquares__:Boolean):Integer; {returns the number of legal box squares}
    {'PushBoxes__' = 'True'  : finds box-reachable squares by pushing boxes around on the board, starting from the box  squares
     'PushBoxes__' = 'False' : finds box-legal     squares by pulling boxes around on the board, starting from the goal squares
     if 'InitializeIllegalSquares__' is 'True' then any existing 'FLAG_ILLEGAL_BOX_SQUARE' flags are reset for all board squares
     returns the number of legal box squares (contrary to what the function name suggests)
     precondition: when 'PushBoxes__' is 'True', then any 'FLAG_ILLEGAL_BOX_SQUARE' flags on the board must be valid since they are taken into account by the search
    }
    type
      TStackItem=packed record BoxPos,PlayerPos:TColRow; end;
      TStack=record
        Top:Integer;
        Items:array[0..MAX_BOARD_SIZE*DIRECTION_COUNT+1] of TStackItem;
      end;
    var
      Col,Row:Integer;
      HasBeenVisited:Boolean;
      BoxSquare,BoxToSquare,PlayerFromSquare,PlayerSquare,PlayerToSquare:TColRow;
      Direction:TDirection;
      Stack:TStack;
      Visited:array[0..MAX_BOARD_WIDTH,0..MAX_BOARD_HEIGHT,TDirection] of Boolean;
      BoardTimeStamps:TBoardTimeStamps;
      PlayerAccessArea:TBoardSquareSet;
    begin
      Result:=0;
      FillChar(Visited,SizeOf(Visited),0); {initialize the box-reachable squares}
      BoardTimeStamps.Timestamp:=High(BoardTimeStamps.Timestamp); {initialize the 'player path exists?' searches}

      {prepare to push/pull boxes around on the board}
      Stack.Top:=0;
      for Col:=1 to BoardWidth__ do
          for Row:=1 to BoardHeight__ do
              if (PushBoxes__       and ((Board__[Col,Row] and BOX )<>0))             {'True': push boxes, and it's a box  square}
                 or
                 ((not PushBoxes__) and ((Board__[Col,Row] and GOAL)<>0)) then begin  {'True': pull boxes, and it's a goal square}
                 for Direction:=Low(Direction) to High(Direction) do begin
                     PlayerSquare.Col:=Col+COMPASS_DIRECTIONS[Direction].Col;
                     PlayerSquare.Row:=Row+COMPASS_DIRECTIONS[Direction].Row;
                     if (Board__[PlayerSquare.Col,PlayerSquare.Row] and (WALL+FLOOR))=FLOOR then begin {'True': the goal's neighbor square in this direction is a player-reachable floor square}
                        Inc(Stack.Top);                                         {put the tuple [goal square, empty floor neighbor square] on the stack}
                        with Stack.Items[Stack.Top] do begin
                             BoxPos.Col:=Col; BoxPos.Row:=Row;
                             PlayerPos:=PlayerSquare;
                             end;
                        if   PushBoxes__ then
                             Visited[Col,Row,OPPOSITE_DIRECTION[Direction]]:=True {'Visited' is set as if the box has been pushed from the neighboring 'PlayerSquare' to the square with coordinates [Col,Row]}
                        else Visited[Col,Row,Direction]:=True;                  {'Visited' is set as if the box has been pulled in the direction 'Direction' to the square, i.e., the player is located at the next square in this direction}
                        end;
                     end;
                 end;

      {push/pull boxes around on the board until all reachable squares have been visited}
      while Stack.Top<>0 do begin
        BoxSquare   :=Stack.Items[Stack.Top].BoxPos;                            {pop box square and player square from the stack}
        PlayerSquare:=Stack.Items[Stack.Top].PlayerPos;
        Dec(Stack.Top);
        Inc(Board__[BoxSquare.Col,BoxSquare.Row],WALL);                             {block the box square by putting a wall on the square}

        for Direction:=Low(Direction) to High(Direction) do begin               {try to pull the box from each direction}
            BoxToSquare   .Col:=BoxSquare  .Col+COMPASS_DIRECTIONS[Direction].Col;    {if possible, the box is pulled to this square; the player starts here}
            BoxToSquare   .Row:=BoxSquare  .Row+COMPASS_DIRECTIONS[Direction].Row;

            if PushBoxes__ then begin
               PlayerFromSquare.Col:=BoxSquare.Col-COMPASS_DIRECTIONS[Direction].Col; {the player starts here before the push}
               PlayerFromSquare.Row:=BoxSquare.Row-COMPASS_DIRECTIONS[Direction].Row;

               if (not Visited[BoxToSquare.Col,BoxToSquare.Row,Direction])          {is it an unvisited square from this direction?}
                  and
                  ((Board__[BoxToSquare     .Col,BoxToSquare     .Row] and (WALL+FLAG_ILLEGAL_BOX_SQUARE))=0) {is the square 'push-box-to-square' free?}
                  and
                  ((Board__[PlayerFromSquare.Col,PlayerFromSquare.Row] and (WALL))=0) {is 'Player-push-from-square' free?}
                  and
                  CalculatePlayerPathExistsOrCalculatePlayerAccessArea(PlayerSquare,PlayerFromSquare,BoardTimeStamps,PlayerAccessArea) then begin {can the player reach 'Player-pull-from-square' from its current position next to the box?}
                  Inc(Stack.Top);                                               {put the new boxposition/playerposition on the stack}
                  Stack.Items[Stack.Top].BoxPos   :=BoxToSquare;
                  Stack.Items[Stack.Top].PlayerPos:=BoxSquare;
                  Visited[BoxToSquare.Col,BoxToSquare.Row,Direction]:=True;         {mark that the square has been visited from this direction}
                  end;
               end
            else begin
               PlayerToSquare.Col:=BoxToSquare.Col+COMPASS_DIRECTIONS[Direction].Col; {the player ends here after the pull}
               PlayerToSquare.Row:=BoxToSquare.Row+COMPASS_DIRECTIONS[Direction].Row;

               if (not Visited[BoxToSquare.Col,BoxToSquare.Row,Direction])          {is it an unvisited square from this direction?}
                  and
                  ((Board__[BoxToSquare   .Col,BoxToSquare   .Row] and (WALL))=0)   {is the square 'Player-pull-from-square' free?}
                  and
                  ((Board__[PlayerToSquare.Col,PlayerToSquare.Row] and (WALL))=0)   {is 'Player-pull-to-square' free?}
                  and
                  CalculatePlayerPathExistsOrCalculatePlayerAccessArea(PlayerSquare,BoxToSquare,BoardTimeStamps,PlayerAccessArea) then begin {can the player reach 'Player-pull-from-square' from its current position next to the box?}
                  Inc(Stack.Top);                                               {put the new boxposition/playerposition on the stack}
                  Stack.Items[Stack.Top].BoxPos   :=BoxToSquare;
                  Stack.Items[Stack.Top].PlayerPos:=PlayerToSquare;
                  Visited[BoxToSquare.Col,BoxToSquare.Row,Direction]:=True;         {mark that the square has been visited from this direction}
                  end;
               end;
            end;

        Dec(Board__[BoxSquare.Col,BoxSquare.Row],WALL);                             {remove the wall from the board again}
        end;

      {mark illegal floor squares, i.e., box-unreachable floors (calculated by pushing boxes around), or box-illegal floors without a push-path to a goal (calculated by pulling boxes around, starting from the goal squares)}
      for Col:=1 to BoardWidth__ do
          for Row:=1 to BoardHeight__ do
              if (Board__[Col,Row] and WALL)=0 then begin                       {'True': it's a floor square}
                 if  InitializeIllegalSquares__ then                            {'True': reset any existing illegal squares flags}
                     Board__[Col,Row]:=Board__[Col,Row] and (not FLAG_ILLEGAL_BOX_SQUARE);
                 HasBeenVisited:=False;
                 for Direction:=Low(Direction) to High(Direction) do
                     if Visited[Col,Row,Direction] then begin                   {'True': it's a legal box square}
                        HasBeenVisited:=True;
                        Inc(Result);                                            {return the number of legal floor squares}
                        break;                                                  {'break': quick-and-dirty exit the loop as soon as it's known that the square has been visited}
                        end;
                 if  (not HasBeenVisited) and                                   {'True': it's an illegal or unreachable square for boxes}
                     ((Board__[Col,Row] and FLAG_ILLEGAL_BOX_SQUARE)=0) then begin       {'True': the square hasn't been marked as being an illegal square yet}
                     Inc(Board__[Col,Row],FLAG_ILLEGAL_BOX_SQUARE);
                     {Inc(Result);}                                             {return the number of (newly found) box-illegal or box-unreachable floor squares}
                     {Board__[Col,Row]:=Board__[Col,Row] or GOAL; // test}
                     end;
                 end;
    end; {CalculateIllegalOrUnreachableBoxSquares}

    function HasALegalBoxSquareNeighbor(Col__,Row__:Integer):Boolean;
    var Direction:TDirection; NeighborSquare:TColRow;
    begin
      Result:=False;
      for Direction:=Low(Direction) to High(Direction) do begin
          NeighborSquare.Col:=Col__+COMPASS_DIRECTIONS[Direction].Col;
          NeighborSquare.Row:=Row__+COMPASS_DIRECTIONS[Direction].Row;
          if (Board__[NeighborSquare.Col,NeighborSquare.Row] and (WALL+FLOOR+FLAG_ILLEGAL_BOX_SQUARE))=(WALL+FLOOR) then begin
             Result:=True; exit; {'exit': quick and dirty exit when found}
             end;
          end;
    end;

    function IsACornerSquare(Col__,Row__:Integer):Boolean;
    var Direction:TDirection; BlockedAxes:TAxisSet;
    begin
      BlockedAxes:=[];
      for Direction:=Low(Direction) to High(Direction) do
          if (Board__[Col__+COMPASS_DIRECTIONS[Direction].Col,Row__+COMPASS_DIRECTIONS[Direction].Row] and (WALL+FLOOR))=WALL then
             Include(BlockedAxes,DIRECTION_TO_AXIS[Direction]);
      Result:=BlockedAxes=ALL_AXES_SET;
    end;

    function IsAMemberOfBoardSquareSet(Col__,Row__:Integer; const BoardSquareSet__:TBoardSquareSet):Boolean;
    var Index:Integer;
    begin
      Result:=False;
      for Index:=1 to BoardSquareSet__.Count do with BoardSquareSet__.Squares[Index] do
          if (Col__=Col) and (Row__=Row) then begin
             Result:=True; exit; {'exit': quick and dirty exit when found}
             end;
    end;

  begin {FillUnnecessaryIllegalBoxSquares}
    Result:=0; TimeMS:=GetTimeMS; PlayerMovesCount:=0;
    CalculateIllegalOrUnreachableBoxSquares(False,True);  {'False' parameter: find box-illegal     squares by pulling boxes around on the board, starting from goal squares}
    CalculateIllegalOrUnreachableBoxSquares(True,False);  {'True'  parameter: find box-unreachable squares by pushing boxes around on the board, starting from box  squares}

    BoardTimeStamps.Timestamp:=High(BoardTimeStamps.Timestamp); {initialize the 'calculate player access area' searches}
    ToSquare.Col:=0; ToSquare.Row:=0; {using [0,0] as destination square makes the function 'CalculatePlayerPathExistsOrCalculatePlayerAccessArea' calculate a player access area}

    {block all legal box squares on the board by putting walls on them}
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do
            if      (Board__[Col,Row] and (WALL+FLOOR+FLAG_ILLEGAL_BOX_SQUARE))=FLOOR then
                    Inc(Board__[Col,Row],WALL) {block all legal box squares on the board by putting walls on them}
            else if (Board__[Col,Row] and (WALL+FLOOR))=(WALL+FLOOR) then
                    Board__[Col,Row]:=WALL; {the current function requires that the input board doesn't contain squares marked as both walls and floors; should such squares exist, they are changed to pure walls here}

    for Col:=1 to BoardWidth__ do {for each square on the board}
        for Row:=1 to BoardHeight__ do
            if (Board__[Col,Row] and (WALL+FLOOR+FLAG_ILLEGAL_BOX_SQUARE))=(FLOOR+FLAG_ILLEGAL_BOX_SQUARE) then begin {'True': it's an unvisited illegal box square}
               FromSquare.Col:=Col; FromSquare.Row:=Row;
               if CalculatePlayerPathExistsOrCalculatePlayerAccessArea(FromSquare,ToSquare,BoardTimeStamps,PlayerAccessArea) then begin

                  {calculate the set of floors in the player access area having legal box square neighbors}
                  SquaresWithALegalBoxSquareNeighbor.Count:=0;
                  for Index:=1 to PlayerAccessArea.Count do with PlayerAccessArea.Squares[Index] do
                      if HasALegalBoxSquareNeighbor(Col,Row) then begin
                         Inc(SquaresWithALegalBoxSquareNeighbor.Count);
                         SquaresWithALegalBoxSquareNeighbor.Squares[SquaresWithALegalBoxSquareNeighbor.Count]:=PlayerAccessArea.Squares[Index];
                         end;

                  if SquaresWithALegalBoxSquareNeighbor.Count<>0 then {'True': the player access area has neighboring legal box squares}
                     {try to fill squares which don't influence the players
                      ability to move around in the access area and still reach
                      all the squares having adjacent legal box squares
                     }
                     repeat
                            Again:=False;

                            for Index:=1 to PlayerAccessArea.Count do with PlayerAccessArea.Squares[Index] do {for each square in the player access area}
                                if  ((Board__[Col,Row] and WALL)=0) and {'True': the square hasn't already been turned into a wall}
                                    IsACornerSquare(Col,Row) and
                                   (not IsAMemberOfBoardSquareSet(Col,Row,SquaresWithALegalBoxSquareNeighbor)) then begin
                                   {try to fill the corner square with a wall,
                                    and check if it affects the player's ability
                                    to reach all the floor squares having
                                    adjacent legal box squares
                                   }
                                   Inc(Board__[Col,Row],WALL); {try to fill the corner square with a wall}

                                   {calculate the new player access area now that the corner square has been filled with a wall}
                                   if CalculatePlayerPathExistsOrCalculatePlayerAccessArea(SquaresWithALegalBoxSquareNeighbor.Squares[1],ToSquare,BoardTimeStamps,NewPlayerAccessArea) then begin
                                      {if the floor squares (in the current access area)
                                       with a box-legal neighbor aren't connected anymore,
                                       then the corner square cannot be filled with a wall;
                                       check if filling the corner has affected the connectivity;
                                      }
                                      for i:=1 to SquaresWithALegalBoxSquareNeighbor.Count do
                                          if {'BoardTimeStamps' can be used for set membership here instead of the function 'IsAMemberOfBoardSquareSet'}
                                             {not IsAMemberOfBoardSquareSet(SquaresWithALegalBoxSquareNeighbor.Squares[i].X,SquaresWithALegalBoxSquareNeighbor.Squares[i].Y,NewPlayerAccessArea)}
                                             (BoardTimeStamps.Squares[SquaresWithALegalBoxSquareNeighbor.Squares[i].Col,
                                                                      SquaresWithALegalBoxSquareNeighbor.Squares[i].Row]
                                              <>
                                              BoardTimeStamps.TimeStamp
                                             )
                                             then
                                             {the corner square affects the connectivity; remove the wall from the square, if it still is there}
                                             Board__[Col,Row]:=Board__[Col,Row] and (not WALL);

                                      if (Board__[Col,Row] and WALL)<>0 then {'True': the corner square doesn't affect the connectivity between the floor squares having box-legal neighbor squares}
                                         {check that all players, if any, in the
                                          access area still can reach the floors
                                          with box-legal neighbor squares
                                         }
                                         for i:=1 to PlayerAccessArea.Count do
                                             if (i<>Index) and {'True': this is a different square than the one currently under investigation; the current square was blocked by a wall and isn't a member of the new access area}
                                                ((Board__[PlayerAccessArea.Squares[i].Col,PlayerAccessArea.Squares[i].Row] and PLAYER)<>0) and {'True': there is a player on this square in the access area}
                                                {'BoardTimeStamps' can be used for set membership here instead of the function 'IsAMemberOfBoardSquareSet'}
                                                {not IsAMemberOfBoardSquareSet(SquaresWithALegalBoxSquareNeighbor.Squares[i].X,SquaresWithALegalBoxSquareNeighbor.Squares[i].Y,NewPlayerAccessArea)}
                                                (BoardTimeStamps.Squares[PlayerAccessArea.Squares[i].Col,PlayerAccessArea.Squares[i].Row]
                                                 <>
                                                 BoardTimeStamps.TimeStamp) then
                                                {filling the corner square blocks the player's access
                                                 to the floors with box-legal neighbor squares;
                                                 remove the wall from the square, if it still is there;
                                                }
                                                Board__[Col,Row]:=Board__[Col,Row] and (not WALL);

                                      if ((Board__[Col,Row] and WALL  )<>0) and {'True': the corner square can be filled with a wall without affecting the connectivity between the box-legal squares and the players in the access area, if any}
                                         ((Board__[Col,Row] and PLAYER)<>0) then begin {'True': there is a player on the corner square}
                                         {find an empty neighbor square and move the player to that square;
                                          this isn't fully implemented; if there are more than
                                          one player on the board, it will fail if all
                                          neighboring empty squares are occupied by other players;
                                         }
                                         Dec(Board__[Col,Row],WALL); {remove the wall; it will be put back if the player may and can move from the corner square to an adjacent square}
                                         if MovePlayerAndBoxes__ then
                                            for Direction:=Low(Direction) to High(Direction) do begin
                                                NeighborSquare.Col:=Col+COMPASS_DIRECTIONS[Direction].Col;
                                                NeighborSquare.Row:=Row+COMPASS_DIRECTIONS[Direction].Row;
                                                if ((Board__[NeighborSquare.Col,NeighborSquare.Row] and (WALL+FLOOR+PLAYER))=FLOOR) and {'True': the neighbor square is an empty floor square}
                                                   ((Board__[Col,Row] and WALL)=0) and {'True': the player hasn't been moved already in one of the other directions}
                                                   (History__.Count<High(History__.Moves)) {'True': making one more move doesn't overflow the move history}
                                                   then begin
                                                   Inc(Board__[Col,Row],WALL); {put the wall back on the unnecessary corner square}
                                                   Dec(Board__[Col,Row],PLAYER); {remove the player from the corner square}
                                                   Inc(Board__[NeighborSquare.Col,NeighborSquare.Row],PLAYER); {move the player to the neighbor square}
                                                   Inc(History__.Count); {update the move history}
                                                   History__.Moves[History__.Count].BoxNo:=0;
                                                   History__.Moves[History__.Count].Direction:=Direction;
                                                   History__.Moves[History__.Count].Flags:=[];
                                                   {updating the history 'Top' in the next code line is commented out
                                                    because it wasn't there from the beginning, and even though the
                                                    update seems natural, it's unknown whether there is code somewhere
                                                    which depends on 'Top' not being updated;
                                                   }
                                                   {if History__.Count>History__.Top then History__.Top:=History__.Count;}
                                                   end;
                                                end;
                                         end;

                                      if (Board__[Col,Row] and WALL)<>0 then begin {'True': turning the floor square into a wall succeeded}
                                         Board__[Col,Row]:=WALL; {remove the 'FLOOR' tag from the corner square; it's a wall now}
                                         Again:=True; {try again, until no more squares in the current player access area can be filled with walls}
                                         Inc(Result); {count the number of filled unnecessary illegal box squares}
                                         end;
                                      end;
                                   end;

                     until  not Again; {until all squares in the player access area have been investigated, and no more of them can be turned into walls}

                  {mark all squares in the current player access area as visited}
                  for Index:=1 to PlayerAccessArea.Count do with PlayerAccessArea.Squares[Index] do
                      Board__[Col,Row]:=Board__[Col,Row] or WALL;
                  end;
               end;

    {clear all legal box squares on the board; currently they're all blocked by walls}
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do
            if (Board__[Col,Row] and (WALL+FLOOR))=(WALL+FLOOR) then
               Dec(Board__[Col,Row],WALL);

    TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS);
{
    SokUtil_.Msg('Unnecessary illegal box squares: '+IntToStr(Result)+NL+
                 'Time: '+IntToStr(TimeMS)+NL+
//               'Legal box squares: '+IntToStr(BoxLegalSquaresCount)+NL+
                 'Total game moves so far: '+IntToStr(History__.Count)+NL+
                 'Player path search moves: '+IntToStr(PlayerMovesCount),'Normalize Board',MB_OK);
}
  end; {FillUnnecessaryIllegalBoxSquares}

  function CalculateExteriorWalls(BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Integer;
  var C,R,Col,Row,SquareValue:Integer; HasAFloorNeighbor,HasANonFloorNonWallNeighbor:Boolean;
  begin {Preconditions: the player's reachable squares on an empty board are marked by 'FLOOR', and other non-wall squares don't have a 'FLOOR' flag}
    Result:=0;
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do
            if   (Board__[Col,Row] and WALL)<>0 then begin {'True': the square contains a wall}
                 HasAFloorNeighbor:=False;
                 HasANonFloorNonWallNeighbor:=(Col=1) or (Col=BoardWidth__) or (Row=1) or (Row=BoardHeight__); {'True': it's a square on the outer border}
                 for C:=Pred(Col) to Succ(Col) do
                     for R:=Pred(Row) to Succ(Row) do begin
                         SquareValue:=Board__[C,R];
                         if (SquareValue and WALL)=0 then
                            if   (SquareValue and FLOOR)<>0 then
                                 HasAFloorNeighbor:=True
                            else HasANonFloorNonWallNeighbor:=True;
                         end;
                 if   HasAFloorNeighbor and HasANonFloorNonWallNeighbor then begin {'True': the wall square is on the boundary of the level}
                      Board__[Col,Row]:=Board__[Col,Row] or FLAG_EXTERIOR_WALL;
                      Inc(Result);
                      end
                 else Board__[Col,Row]:=Board__[Col,Row] and (not FLAG_EXTERIOR_WALL);
                 end;
  end; {CalculateExteriorWalls}

  function  FindPlayerPosition(BoardWidth__,BoardHeight__:Integer; var Board__:TBoard; var PlayerPos__:TColRow):Integer;
  var Col,Row:Integer;
  begin {Returns the number of players found on the board; the first encountered player is returned in 'PlayerPos__' and any extra players are removed from the board}
    Result:=0;
    PlayerPos__.Col:=0; PlayerPos__.Row:=0;
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do
            if (Board__[Col,Row] and PLAYER)<>0 then begin
               Inc(Result);
               if   PlayerPos__.Col=0 then begin
                    PlayerPos__.Col:=Col; PlayerPos__.Row:=Row;
                    end
               else Dec(Board__[Col,Row],PLAYER); {remove extra players from the board}
               end;
  end; {FindPlayerPosition}

  function TrimBoard(MinWidth__:Integer;  {Removes redundant empty columns and rows}
                     MinHeight__:Integer;
                     var Board__:TBoard;
                     var BoardWidth__,BoardHeight__:Integer;
                     var PlayerPos__:TColRow):Integer;
  var i,j,k:Integer;

    function BlankCol(ACol__:Integer):Boolean;
    var i:Integer;
    begin
      Result:=True;
      for i:=1 to BoardHeight__ do
          if (Board__[ACol__,i] and (WALL+BOX+GOAL+PLAYER))<>0 then
             begin Result:=False; exit; end;
    end; {BlankCol}

    function BlankRow(ARow__:Integer):Boolean;
    var i:Integer;
    begin
      Result:=True;
      for i:=1 to BoardWidth__ do
          if (Board__[i,ARow__] and (WALL+BOX+GOAL+PLAYER))<>0 then
             begin Result:=False; exit; end;
    end; {BlankRow}

  begin {TrimBoard}
    Result:=0;
    while (BoardWidth__>MinWidth__) and BlankCol(1) do begin
      for i:=1 to Pred(BoardWidth__) do
          for j:=1 to BoardHeight__ do Board__[i,j]:=Board__[Succ(i),j];
      Dec(BoardWidth__); Inc(Result);
      end;

    while (BoardHeight__>MinHeight__) and BlankRow(1) do begin
      for i:=1 to BoardWidth__ do
          for j:=1 to Pred(BoardHeight__) do Board__[i,j]:=Board__[i,Succ(j)];
      Dec(BoardHeight__); Inc(Result);
      end;

    while (BoardWidth__ >MinWidth__ ) and BlankCol(BoardWidth__ ) do
      begin Dec(BoardWidth__); Inc(Result); end;
    while (BoardHeight__>MinHeight__) and BlankRow(BoardHeight__) do
      begin Dec(BoardHeight__); Inc(Result); end;

    for j:=0 to MAX_BOARD_HEIGHT+1 do begin {unused squares with walls}
        Board__[0,j]:=WALL;
        if   (j>BoardHeight__) or (j=0) then k:=0
        else k:=Succ(BoardWidth__);
        for i:=k to MAX_BOARD_WIDTH+1 do Board__[i,j]:=WALL;
        end;

    FindPlayerPosition(BoardWidth__,BoardHeight__,Board__,PlayerPos__);
  end; {TrimBoard}

  function RemoveRedundantWalls(BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Integer;
  var i,j,Col,Row:Integer; HasAFloorNeighbor:Boolean;
  begin {Returns number of removed walls}
    Result:=0;
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do
            if   (Board__[Col,Row] and WALL)<>0 then begin
                 HasAFloorNeighbor:=False;
                 for i:=Col-1 to Col+1 do
                     if (i>=1) and (i<=BoardWidth__) then
                        if not HasAFloorNeighbor then begin
                           for j:=Row-1 to Row+1 do
                               if (j>=1) and (j<=BoardHeight__) and
                                  ((Board__[i,j] and FLOOR)<>0) then begin
                                  HasAFloorNeighbor:=True;
                                  break;
                                  end;
                           end
                        else
                           break; {found a neighbor floor-square}
                 if not HasAFloorNeighbor then begin
                    Board__[Col,Row]:=Board__[Col,Row] and (not WALL); {note that the square isn't flagged with the value "FLOOR" here; it is up to the caller to do that, if necessary}
                    Inc(Result);
                    end;
                 end;
  end; {RemoveRedundantWalls}

begin {'NormalizeBoard'}
  Result:=0;
  OriginalBoardWidth:=BoardWidth__; OriginalBoardHeight:=BoardHeight__; OriginalBoard:=Board__; History__.Count:=0;

  {'FillTubes' is called twice during normalization; here where it's called for the first time it's mainly for removing immovable boxes on corner squares}
  FillTubes(MovePlayerAndBoxes__,ChangeImmovableBoxesOnNonGoalSquaresToWalls__,ChangeImmovableBoxesOnGoalSquaresToWalls__,StableNormalization__);

  FillChar(Visited,SizeOf(Visited),0); {clear visited squares, i.e., squares that are reachable from a player}
  QueueTop:=Low(Queue); {initialize queue}

  for Row:=1 to BoardHeight__ do
      Board__[Succ(BoardWidth__),Row ]:=WALL; {fill the column to the right of the board with walls; the board must be surrounded by walls; otherwise, rows ending with a box on a goal instead of a wall are not handled properly}
  for Col:=1 to BoardWidth__ do begin
      Board__[Col,Succ(BoardHeight__)]:=WALL; {fill the row below the board with walls; the board must be surrounded by walls; otherwise columns ending with a box on a goal instead of a wall are not handled properly}
      for Row:=1 to BoardHeight__ do
          {find the players on the board, and put their positions on the queue}
          if (Board__[Col,Row] and (PLAYER+BOX+WALL))=PLAYER then begin
             Visited[Col,Row]:=True; {this square is reachable from a player}
             Inc(QueueTop); {put the player square on the queue so its neighbors are explored later}
             Queue[QueueTop].Col:=Col;
             Queue[QueueTop].Row:=Row;
             end;
      end;
  Board__[Succ(BoardWidth__),Succ(BoardHeight__)]:=WALL; {fill the corner square at the bottom-right of the board with a wall; the board must be surrounded by walls; otherwise a board ending with a box on a goal instead of a wall is not handled properly}

  {calculate the reachable squares from the player-positions}
  QueueIndex:=Low(Queue); {'Low': point before the first player-square on the queue, if any}
  if QueueIndex<QueueTop then begin {'True': there is one or more players on the board; if there aren't any players on the board, then don't fill the board squares with walls because they are "unreachable from a player"}
     while QueueIndex<QueueTop do begin
       Inc(QueueIndex); {advance to the next reachable square on the queue}
       with Queue[QueueIndex] do begin
         Board__[Col,Row]:=Board__[Col,Row] or FLOOR; {ensure that all squares reachable from the player-positions are marked as floor squares}
         for Direction:=Low(Direction) to High(Direction) do begin
             NeighborCol:=Col+COMPASS_DIRECTIONS[Direction].Col;
             NeighborRow:=Row+COMPASS_DIRECTIONS[Direction].Row;
             if ((Board__[NeighborCol,NeighborRow] and WALL)=0) and  {'True': the neighbor square doesn't contain a wall, i.e., it's a floor-square}
                (not Visited[NeighborCol,NeighborRow]) then begin  {'True': the neighbor square hasn't been visited before}
                Visited[NeighborCol,NeighborRow]:=True; {this square is reachable from a player}
                Inc(QueueTop); {put the neighbor square on the queue so its neighbors are explored later}
                Queue[QueueTop].Col:=NeighborCol;
                Queue[QueueTop].Row:=NeighborRow;
                end;
             end;
         end;
       end;

     for Col:=1 to BoardWidth__ do
         for Row:=1 to BoardHeight__ do
             if not Visited[Col,Row] then
                Board__[Col,Row]:=WALL; {put walls on all squares that aren't reachable from a player}
     end;

  ChangeFrozenBoxesToWalls(ChangeImmovableBoxesOnNonGoalSquaresToWalls__,ChangeImmovableBoxesOnGoalSquaresToWalls__,BoardWidth__,BoardHeight__,Board__);

  if FillUnnecessaryIllegalBoxSquares__ then FillUnnecessaryIllegalBoxSquares;

  FillTubes(MovePlayerAndBoxes__,ChangeImmovableBoxesOnNonGoalSquaresToWalls__,ChangeImmovableBoxesOnGoalSquaresToWalls__,StableNormalization__);

  RemoveRedundantWalls(BoardWidth__,BoardHeight__,Board__);

  Inc(Result,TrimBoard(MIN_BOARD_WIDTH,MIN_BOARD_HEIGHT,Board__,BoardWidth__,BoardHeight__,PlayerPos__)); {trim the board for any empty exterior columns and rows}

  CalculateExteriorWalls(BoardWidth__,BoardHeight__,Board__); {mark the exterior walls on the board; it's practical to do the calculation now while the player's reachable squares are marked by the 'FLOOR' tag, and unreachable non-wall squares don't have this flag}

  if Result<>0 then {'True': the board contained empty columns and/or rows}
     for Col:=1 to MAX_BOARD_WIDTH do
         for Row:=1 to MAX_BOARD_HEIGHT do
             if (Col>BoardWidth__) or (Row>BoardHeight__) then
                Board__[Col,Row]:=FLOOR; {fill trimmed columns and rows with floors}

  for Col:=1 to OriginalBoardWidth do
      for Row:=1 to OriginalBoardHeight do
          if (OriginalBoard[Col,Row] and BOARD_PIECES)<>(Board__[Col,Row] and BOARD_PIECES) then begin {'True': the square has been changed}
             Inc(Result);
             if (Board__[Col,Row] and WALL)=0 then
                Board__[Col,Row]:=Board__[Col,Row] or FLOOR; {mark all non-wall squares as floors ('RemoveRedundantWalls' doesn't do that)}
             end;
end; {NormalizeBoard}

procedure ShowBoard;
var Row:Integer; BoardAsTextLines:TBoardAsTextLines;
begin
  FillChar(BoardAsTextLines,SizeOf(BoardAsTextLines),0);
  BoardToTextLines(BoardAsTextLines);
  for Row:=1 to Game.BoardHeight do
      Writeln(BoardAsTextLines[Row]);
end;

procedure ShowOrimazeBoard(BoardWidth__,BoardHeight__:Integer; const Board__:TOrimazeBoard);
var Row:Integer; BoardAsTextLines:TBoardAsTextLines;
begin
  FillChar(BoardAsTextLines,SizeOf(BoardAsTextLines),0);
  OrimazeBoardToTextLines(BoardWidth__,BoardHeight__,Board__,BoardAsTextLines);
  for Row:=1 to BoardHeight__ do
      Writeln(BoardAsTextLines[Row]);
end;

procedure ShowBoardWithColumnsAndRows;
var Col,Row:Integer; BoardAsTextLines:TBoardAsTextLines;
begin
  FillChar(BoardAsTextLines,SizeOf(BoardAsTextLines),0);
  for Col:=1 to Game.BoardWidth do
      Write(Col mod 10);
  Writeln;
  BoardToTextLines(BoardAsTextLines);
  for Row:=1 to Game.BoardHeight do
      Writeln(BoardAsTextLines[Row],Row:4);
  for Col:=1 to Game.BoardWidth do
      Write(Col mod 10);
  Writeln;
end;

{-----------------------------------------------------------------------------}

{Game}

function  InitializeGame(OrimazeBoardWidth__,OrimazeBoardHeight__:Integer):Boolean;
var Index,Col,Row,NeighborCol,NeighborRow,Width,Height:Integer;
    Direction:TDirection;
    Transformation:TBoardTransformation2D;
begin
  with Game do begin
    Result:=True;
    OrimazeBoardWidth:=OrimazeBoardWidth__;
    OrimazeBoardHeight:=OrimazeBoardHeight__;
    OrimazeBoardSize:=Game.OrimazeBoardWidth*Game.OrimazeBoardHeight;

    for Index:=0 to Pred(OrimazeBoardSize) do
        for Transformation:=Low(Transformation) to High(Transformation) do begin
            OrimazeSquareToColRow(Index,Col,Row);
            Calculate2DTransformation(Transformation,Col,Row,Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,Col,Row);
            Calculate2DDimensionsTransformation(Transformation,Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,Width,Height);
            OrimazeBoardSquare2DTransformations[Index,Transformation]:=Col+(Row*Width);
            end;

    Index:=0;
    for Row:=0 to Pred(OrimazeBoardHeight) do
        for Col:=0 to Pred(OrimazeBoardWidth) do begin
            for Direction:=Low(Direction) to High(Direction) do begin
                NeighborCol:=Col+COMPASS_DIRECTIONS[Direction].Col;
                NeighborRow:=Row+COMPASS_DIRECTIONS[Direction].Row;
                if   (NeighborCol>=0) and (NeighborCol<Game.OrimazeBoardWidth ) and
                     (NeighborRow>=0) and (NeighborRow<Game.OrimazeBoardHeight) then
                     OrimazeBoardSquareNeighbors[Index,Direction]:=NeighborCol+(NeighborRow*Game.OrimazeBoardWidth) {index of neighbor in this direction}
                else OrimazeBoardSquareNeighbors[Index,Direction]:=-1; {no neighbor in this direction}
                end;
            Inc(Index);
            end;
    end;
end;

{-----------------------------------------------------------------------------}

{Legend}

procedure InitializeLegend(XSBNotation__:Boolean);
var i:Integer;
begin
  with Legend do begin
    FillChar(Legend,SizeOf(Legend),0);
    CharToItem[CH_BOX       ]:=BOX+FLOOR   ; CharToItem[CH_BOX_ON_GOAL       ]:=BOX+GOAL+FLOOR;
    CharToItem[CH_BOX_XSB   ]:=BOX+FLOOR   ; CharToItem[CH_BOX_ON_GOAL_XSB   ]:=BOX+GOAL+FLOOR;
    CharToItem[CH_FLOOR     ]:=FLOOR;
    CharToItem[CH_GOAL      ]:=GOAL+FLOOR  ; CharToItem[CH_WALL              ]:=WALL;
    CharToItem[CH_PLAYER    ]:=PLAYER+FLOOR; CharToItem[CH_PLAYER_ON_GOAL    ]:=PLAYER+GOAL+FLOOR;
    CharToItem[CH_PLAYER_XSB]:=PLAYER+FLOOR; CharToItem[CH_PLAYER_ON_GOAL_XSB]:=PLAYER+GOAL+FLOOR;

    CharToItem[UNDERSCORE   ]:=FLOOR; {extra characters for floor squares}
    CharToItem[HYPHEN       ]:=FLOOR; {extra characters for floor squares}
    for i:=0 to Pred(Ord(SPACE)) do CharToItem[Chr(i)]:=FLOOR; {note: depends on ASCII character set where all characters < SPACE are control characters}
    XSBNotation:=XSBNotation__;
    end;
end;

{-----------------------------------------------------------------------------}

{Open Positions}

function  OPENAdd(Position__:PPosition):Boolean;
var Index:Integer;
    Root:PPosition;
begin {open positions: insert item}
  with Positions.OpenPositions do with Position__^ do begin
    {$WARNINGS OFF}
      {warning: Comparison always evaluates to True}
      Result:=(PushCount>=Low(Buckets)) and (PushCount<=High(Buckets));
    {$WARNINGS ON}
    if Result then begin
       Inc(Count);
       if PushCount<MinValue then begin {keep track of the minimum score}
          if PushCount<=MaxValue then {otherwise, the nil-pointer initialization will be performed a few lines further down when 'MaxValue' is updated}
             for Index:=PushCount to Pred(MinValue) do
                 Buckets[Index]:=nil; {initialize the sort buckets}
          MinValue:=PushCount;
          end;
       if PushCount>MaxValue then begin {keep track of the maximum score}
          for Index:=Succ(MaxValue) to PushCount do
              Buckets[Index]:=nil; {initialize the sort buckets}
          MaxValue:=PushCount;
          end;

       Root:=Buckets[PushCount];
       if Root<>nil then with ScoreBucket do begin {'<>nil': non-empty list}
          if      (True) or (Position__^.PushCount>=Root^.PushCount) then begin {new first item}
                  Prev:=Root^.ScoreBucket.Prev;
                  Next:=Root;
                  Buckets[PushCount]:=Position__;  {insert 'Position__' as the first member of the bucket}
                  end
          else if Position__^.PushCount<=Root^.ScoreBucket.Prev^.PushCount then begin {new last item}
                  Prev:=Root^.ScoreBucket.Prev;
                  Next:=Root;
                  end
               else begin {insert item sorted on 'PushCount'}
                  repeat Root:=Root^.ScoreBucket.Next;
                  until  Position__^.PushCount>Root^.PushCount;
                  Prev:=Root^.ScoreBucket.Prev;
                  Next:=Root;
                  end;
          Prev^.ScoreBucket.Next:=Position__;  {update previous item in the list}
          Next^.ScoreBucket.Prev:=Position__;  {update previous item in the list}
          end
       else with ScoreBucket do begin {the bucket is empty; create new circular list with 1 item}
          Prev:=Position__;
          Next:=Position__;
          Buckets[PushCount]:=Position__;
          end;

       //if mfOpen in Move.Flags then
       //   Msg(TEXT_INTERNAL_ERROR+': "OPENAdd"',TEXT_APPLICATION_TITLE);

       Include(Move.Flags,mfOpen); {flag the position as a member of OPEN}
       end
    else
       {the score exceeds the limit, hence, the position can never be a part of}
       {a solution path}
       if (SuccessorCount=0) and
          (not (mfPath in Move.Flags)) then
          TTRemove(Position__);
    end;
end;

procedure OPENClear;
begin {open positions: clear open-queue}
  with Positions.OpenPositions do begin
    //FillChar(Positions.OpenPositions,SizeOf(Positions.OpenPositions),0);
    Count:=0;
    MaxValue:=Low (Buckets)-1;
    MinValue:=High(Buckets)+1;
    end;
end;

function  OPENDropPositions(DepthLimit__:Integer):TSize;
var OriginalCount:TSize;
    p:PPosition;
begin {open positions: drop positions with score >= 'DepthLimit__'; the positions are removed from the transposition-table as well}
  Result:=0;;
  with Positions.OpenPositions do begin
    OriginalCount:=Count;
    repeat
      while (DepthLimit__<=MaxValue) and (Buckets[MaxValue]=nil) do Dec(MaxValue);
      if  DepthLimit__<=MaxValue then  {'True': drop all positions in the max-value bucket}
          repeat
            Inc(Result);
            p:=Buckets[MaxValue]^.ScoreBucket.Prev; {drop the items in reverse order in order to avoid updating the bucket-root more than once}
            OPENRemove(p);             {remove the position from the open-queue}
            if (p^.SuccessorCount=0) and (not (mfPath in p^.Move.Flags)) then
               TTRemove(p);            {remove the position from the transposition-table}
          until Buckets[MaxValue]=nil; {until all max-value positions have been processed}
    until MaxValue<DepthLimit__;
    if Result<>0 then Writeln('Freeing ',Result,'/',OriginalCount,' open position(s)');
    end;
end;

procedure OPENRemove(Position__:PPosition);
begin {open positions: remove item}
  with Positions.OpenPositions do with Position__^ do with ScoreBucket do begin
    Prev^.ScoreBucket.Next:=Next;
    Next^.ScoreBucket.Prev:=Prev;
    if Position__=Buckets[PushCount] then {'True': removing the root item in this bucket}
       if Next<>Position__ then
          Buckets[PushCount]:=Next {promote next item to root item in this bucket}
       else begin {removing the last item in this bucket}
          Buckets[PushCount]:=nil;
          if PushCount=MinValue then {'True': removing the last item with the current minimum value; find the new minimum value, if any}
             repeat Inc(MinValue);
             until  (MinValue>MaxValue) or (Buckets[MinValue]<>nil);
          end;
//  Prev:=nil; Next:=nil;             {mark that the position isn't on the open-queue anymore}
{
    if mfOpen in Move.Flags then
       Msg(TEXT_INTERNAL_ERROR,'OPENRemove');
}
    Exclude(Move.Flags,mfOpen);
    BestForgottenScore:=DEAD_END_SCORE;
    Dec(Count);
    end;
end;

function  OPENRemoveNextPositionSelectedForExpansion(var Position__:PPosition):Boolean;
begin {open positions: remove best item}
  with Positions.OpenPositions do begin
    while (MinValue<=MaxValue) and (Buckets[MinValue]=nil) do Inc(MinValue);
    Result:=MinValue<=MaxValue;
    if Result then begin
       Position__:=Buckets[MinValue];

       //if Position__^.Score<>MinValue then
       //   Msg(TEXT_INTERNAL_ERROR+': "OPENRemoveBest"',TEXT_APPLICATION_TITLE);

       OPENRemove(Position__);
       end;
    end;
end;

{-----------------------------------------------------------------------------}

{Transposition Table}

function  TTReallocateMemoryBlock( var Block__ : Pointer; var ByteSize__ : TSize; NewByteSize__ : TSize ) : Boolean;
var ItemCount : TSize;

  procedure FreeMemoryBlock( var Block__ : Pointer; var ByteSize__ : TSize );
  var Index : TSize;
  begin // returns the free memory block to the positions free list
    for Index := 0 to Pred( ByteSize__ div SizeOf( TPosition ) ) do begin
        PPosition( Block__ )^.PlayerPos := 0;                                   {marks the item as unused}
        PPosition( Block__ )^.HashBucket.Next := Positions.FreeList;            {return the item to the free-list}
        Positions.FreeList:=PPosition( Block__ );
        Inc( PPosition( Block__ ) );
        end;
    Block__    := nil;
    ByteSize__ := 0;
  end;

begin // 'TTReallocateMemoryBlock' reallocates, allocates, or releases a memory
      // block from the memory originally allocated for the transposition table;
  ByteSize__                  := Align( ByteSize__   , SizeOf( TPosition ) );
  NewByteSize__               := Align( NewByteSize__, SizeOf( TPosition ) );
  ItemCount                   := NewByteSize__ div SizeOf( TPosition );
  Result                      := ( NewByteSize__ >= 0 ) and {'True': valid parameter, e.g., alignment didn't cause a numeric overflow}
                                 ( Positions.UninitializedItemCount >= ItemCount );
  if Result then begin {'True': sufficient free memory for the task}
     Dec( Positions.Capacity,               ItemCount ); {allocate memory from the end of the transposition table}
     Dec( Positions.UninitializedItemCount, ItemCount );
     Positions.HighWaterMark := Pointer( Addr( Positions.Positions^[ Positions.Capacity ] ) );
     if Block__              <> nil then begin
        Move( Block__^, Positions.HighWaterMark^, Min( ByteSize__, NewByteSize__ ) );
        FreeMemoryBlock( Block__, ByteSize__ ); {returns the free memory block to the positions free list}
        end;
     ByteSize__              := NewByteSize__;
     if   ByteSize__         >  0 then
          Block__            := Positions.HighWaterMark
     else Block__            := nil;
     end;
end;

function  TTAdd(HashValue__:THashValue;
                PlayerPos__,PushCount__,Score__,BoxNo__,SearchDepth__:Integer;
                Direction__:TDirection;
                Parent__:PPosition;
                var Position__:PPosition):Boolean;
var HashBucketIndex:TSize; NextPosition:PPosition;
begin {transposition table: add new item unless it's already in the table}
      {precondition for Sokoban game positions (as opposed to Orimaze game positions):
       * 'Solver.SearchStates[PushCount__].PlayersReachableSquares.Calculated' is set correctly;}
  Result:=False; Position__:=nil;
  with Positions do
    if (not TTLookup(HashValue__,PlayerPos__,SearchDepth__,Position__)) then begin {'True': it's a new item}

       NextPosition:=Position__;                                                {save next position in the hash-chain}
       if Parent__<>nil then Inc(Parent__^.SuccessorCount);                     {update count before any pruning of the items in the transposition table takes place}

       if UninitializedItemCount<=0 then begin                                  {'True': all items in memory are either in use or on the free-list}
          if   FreeList<>nil then begin
               Position__:=FreeList;                                            {get next free item}
               FreeList  :=FreeList^.HashBucket.Next;                           {the free-list uses 'HashBucket.Next' for linking}
               end
          else                                                                  {transposition-table full}
               Position__:=nil;                                                 {no free slot for the new position}
          end
       else begin {not all items have been used yet. get the next free item in the memory block.}
          Position__:=PPosition(Addr(Positions^[Capacity-UninitializedItemCount]));
          Dec(UninitializedItemCount);
          end;

       if Position__<>nil then with Position__^ do begin                        {'True': got a free slot: save the new item}
//        HashBucketIndex    :=HashValue__ and HashBucketMask;
          HashBucketIndex    :=(UInt32(HashValue__) xor PUInt32(NativeUInt(Addr(HashValue__))+NativeUInt(SizeOf(UInt32)))^) and HashBucketMask;

          if    NextPosition<>nil then begin                                    {'True': insert the new position before 'NextPosition'}
                HashBucket.Prev :=NextPosition^.HashBucket.Prev;
                HashBucket.Next :=NextPosition;
                HashBucket.Prev^.HashBucket.Next:=Position__;
                NextPosition^.HashBucket.Prev:=Position__;
                if HashBuckets^[HashBucketIndex]=NextPosition then              {'True': 'NextPosition' is the first item in its bucket}
                   HashBuckets^[HashBucketIndex]:=Position__;                   {set the new item to be the first item in its bucket}
                end
          else
             if HashBuckets^[HashBucketIndex]=nil then begin                    {'True': the bucket is empty}
                HashBuckets^[HashBucketIndex]:=Position__;                      {put this item in the bucket}
                HashBucket.Prev :=Position__;                                   {make single-item circular list}
                HashBucket.Next :=Position__;
                //Inc(N); Writeln(N,SPACE,YASGenOrimaze.Positions.Count);
                end
             else begin                                                         {insert as last item in this bucket}
                HashBucket.Next :=HashBuckets^[HashBucketIndex];
                HashBucket.Prev :=HashBucket.Next^.HashBucket.Prev;
                HashBucket.Prev^.HashBucket.Next:=Position__;
                HashBucket.Next^.HashBucket.Prev:=Position__;
                end;

          HashValue                     :=HashValue__;
{         Move.BoxNo                    :=BoxNo__;}                             {box numbers aren't used in the Orimaze game. instead, the field is used for storing a BFS search depth by the iFUB algorithm.}
          Move.Direction                :=Direction__;
          Move.Flags                    :=[];
          Parent                        :=Parent__;
          PlayerPos                     :=PlayerPos__;
          PushCount                     :=PushCount__;
          BestForgottenScore            :=DEAD_END_SCORE;
          {$IFDEF CALCULATE_CLUSTERS}
            Eccentricity                :=0;
          {$ENDIF}
          Result                        :=True;

          if   PlayerPos__<=MAX_BOARD_SIZE then begin {'True': this is a normal game state position}
               Inc(Count);
               Inc(SearchStatistics.ForwardPositionCount);
               if   (BestPosition=nil) or (PushCount>BestScore) then begin
                    BestScore:=PushCount;
                    BestPosition:=Position__;
                    BestPositionCount:=1;
                    Inc(SearchStatistics.NewBestPositionCount);
                    end
               else if PushCount=BestScore then
                       Inc(BestPositionCount);
               end;
          end
       else
          if Parent__<>nil then Dec(Parent__^.SuccessorCount);                  {undo the premature update}
       end
    else with Position__^ do {the position already exists in the transposition-table}

       if (PushCount__>=PushCount)
          or
          (PlayerPos>MAX_BOARD_SIZE) {'True': it's not a normal game position}
          then begin {don't change the path to this position}
          Inc(SearchStatistics.DuplicatesCount);
          end
       else begin {cheaper path to this position: change path to it}
          if mfOpen in Move.Flags then
             OPENRemove(Position__);                                            {remove the position from the open-queue; the caller is expected to put it back on the open-queue at its new position}
          if Parent<>nil then
             Dec(Parent^.SuccessorCount);

{         Move.BoxNo                    :=BoxNo__;}                             {box numbers aren't used in the Orimaze game. instead, the field is used for storing a BFS search depth by the iFUB algorithm.}
          Move.Direction        :=Direction__;

          Parent                :=Parent__;
          PlayerPos             :=PlayerPos__;
          PushCount             :=PushCount__;
          Result                :=True;
          Inc(SearchStatistics.NewPathCount);
          if Parent__<>nil then Inc(Parent__^.SuccessorCount);

          if (BestPosition=nil) or (PushCount>BestScore) then begin
             BestScore:=PushCount;
             BestPosition:=Position__;
             BestPositionCount:=1;
             Inc(SearchStatistics.NewBestPositionCount);
             end
          else if PushCount=BestScore then
                  Inc(BestPositionCount);
          end;
end;

procedure TTClear(ClearOrimazeGamePositions__:Boolean);
var Index:TSize; //TimeMS:TTimeMS;
    Position:PPosition;
begin {transposition table: clear table}
  with Positions do begin
    BestScore                     :=0;   SearchStatistics.DroppedCount :=0;
    BestPosition                  :=nil; BestPositionCount             :=0;
    CurrentPosition               :=nil; DebugPosition                 :=nil;
    StartPosition                 :=nil; SolutionPosition              :=nil;

    if ClearOrimazeGamePositions__ then begin {'True': really clear the transposition table}
       Count                      :=0;
       Capacity                   :=0;
       FreeList                   :=nil;
       if (Positions              <>nil) and
          (HighWaterMark          <>nil) then
          Capacity                := ( NativeUInt( HighWaterMark ) - NativeUInt( Positions ) ) div SizeOf( TPosition );
       UninitializedItemCount     := Capacity; {add one item at a time to the transposition-table instead of building a free-list with all the items in memory}

       if (HashBuckets<>nil) and (HashBucketTimeStamps<>nil) then begin
          //TimeMS:=GetTimeMS;
          //for i:=1 to 1000 do // time test
          //    FillChar(HashBuckets^,HashBucketCount*SizeOf(HashBuckets^[Low(HashBuckets^)]),0);
          //TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS);
          //Write('Time: ', TimeMS);
          //Readln;
          if HashBucketTimeStamp  <High(HashBucketTimeStamp) then
             Inc(HashBucketTimeStamp)
          else begin {timestamp wrap around. clear the hash bucket timestamps and start from the beginning with timestamp = '1'.}
             HashBucketTimeStamp  :=1;
             FillChar(HashBucketTimeStamps^,HashBucketCount*SizeOf(HashBucketTimeStamps^[Low(HashBucketTimeStamps^)]),0);
             end;
          end;
       end
    else begin {keep the Orimaze game positions and just clear the graph edges, i.e., the moves which connect the game states}
       Position                   :=PPosition(Positions);
       for Index                  :=0 to Pred(Count) do begin
           Position^.PushCount    :=High(Position^.PushCount); {'high-value': unvisited position}
           Position^.Parent       :=nil;
           Position^.Move.Flags   :=[];
           Inc(Position); {advance to the next Orimaze game position in the table}
           end;
       end;

    FillChar(SearchStatistics,SizeOf(SearchStatistics),0);
    end;
end;

procedure TTFinalize;
begin
  with Positions do begin
    if Positions<>nil then FreeMem(Positions);
    Positions:=nil; HighWaterMark:=nil; HashBuckets:=nil; HashBucketTimeStamps:=nil;
    TTClear(True);
    end;
end;

function  TTGetOrimazeBoardForPosition(Position__:PPosition):POrimazeBoard;
begin {returns the orimaze board matching the given transposition table item}
  Result:=POrimazeBoard(Cardinal(Position__)+Cardinal(SizeOf(TPosition))); {the saved board is located right after the transposition table item}
end;

function  TTIndexOf(Position__:PPosition):Integer;
begin {transposition table: returns vector index for the item at 'Position__'}
  Result:=(NativeUInt(Position__)-NativeUInt(Positions.Positions)) div SizeOf(Position__^);
end;

function  TTIsOnPath(Position__,Path__:PPosition):Boolean;
begin {transposition table: returns 'True' if 'Position__' is on the path leading to 'Path__'}
  Result:=False;
  while (Path__<>nil) and (not Result) do
    if   Path__<>Position__ then Path__:=Path__^.Parent
    else Result:=True;
end;

function  TTInitialize(MemoryByteSize__:TSize):Boolean;
var i,j,PositionSize,HashBucketVectorSize,HashBucketItemSize,HashBucketTimestampVectorSize,HashBucketTimeStampItemSize:TSize;
    RandomState:TRandomState;
begin {transposition table: allocates memory, initializes data structures for the positions, and initializes constants for Zobrist hash key values}
//MemoryByteSize__:=128*ONE_MEBI;
  Result                        :=False;
  FillChar(Positions,SizeOf(Positions),0);
  Positions.HashBucketTimeStamp :=High(Positions.HashBucketTimeStamp);
  MemoryByteSize__              :=Min(NativeInt(MemoryByteSize__),High(NativeInt));
  PositionSize                  :=SizeOf(Positions.Positions^[Low(Positions.Positions^)]);
  HashBucketItemSize            :=SizeOf(Positions.HashBuckets^[Low(Positions.HashBuckets^)]);
  HashBucketTimeStampItemSize   :=SizeOf(Positions.HashBucketTimeStamps^[Low(Positions.HashBucketTimeStamps^)]);

  if MemoryByteSize__>=PositionSize+(4*HashBucketItemSize) then with Positions do begin
     GetMem(Positions,MemoryByteSize__);
     {'GetMem()' can fail, even if the user specified 'available memory' in    }
     {the command line parameters.                                             }
     {the rationale for not protecting against memory overflow here by using   }
     {'GetAsMuchMemoryAsPossible()' is that it entails linking 'SysUtils' to   }
     {the application for exception-handling, adding so much extra baggage to  }
     {the exe-file that it doesn't seem worth it for this relatively small     }
     {console mode program.                                                    }

     if Positions<>nil then begin
        MemoryByteSize:=MemoryByteSize__;

        {calculate the size of the memory to reserve for the hash-buckets}
        HashBucketCount:=1; Capacity:=0;
        repeat i:=HashBucketCount shl 1;
               HashBucketVectorSize:=i*HashBucketItemSize;
               HashBucketTimestampVectorSize:=i*HashBucketTimeStampItemSize;
               if   MemoryByteSize>=(HashBucketVectorSize+HashBucketTimestampVectorSize) then begin
                    j:=(MemoryByteSize-HashBucketVectorSize-HashBucketTimestampVectorSize) div PositionSize;  // calculate the number of positions after reservation of 'i' buckets
                    if j>0 then begin
                       HashBucketCount:=i; Capacity:=j;
                       end;
                    end
               else j:=0;
        until  ((j div HashBucketCount)<=TT_AVERAGE_BUCKET_SIZE)
               or
               (i>=High(HashBucketCount) div 2);

        HashBucketMask      :=Pred(HashBucketCount); {masking a hash-value with 'HashBucketMask' produces a proper hash-bucket index}
        HashBuckets         :=PPositionPointersVector(NativeUInt(Positions)+NativeUInt(Capacity*PositionSize)); {reserve the upper part of the allocated memory for the hash-buckets}
        HashBucketTimeStamps:=PTimeStampVector(NativeUInt(HashBuckets)+(NativeUInt(NativeUInt(HashBucketCount)*NativeUInt(HashBucketItemSize))));
        {'HighWaterMark: start of the reserved area. memory below the high-water
         mark is used for transposition table items; above the high-water mark,
         there are hash table buckets, bucket timestamps, and dynamically
         allocated data for the deadlock sets}
        HighWaterMark       :=HashBuckets;
        Result              :=Capacity>0;
        end;
     end;

  InitializeRandomState(0, RandomState);
  for i:=0 to 255 do if Random(MaxInt,RandomState)=0 then;                      {warm up the random number generator}

  with Positions do
    for i:=Low(SquareHashValues) to High(SquareHashValues) do                   {initialize the hash square values used for calculating Zobrist hash-keys}
        repeat
          SquareHashValues[i]:=
            Abs(Abs((THashValue(Random(MaxInt,RandomState)) shl 48))+
                Abs((THashValue(Random(MaxInt,RandomState)) shl 32))+
                Abs((THashValue(Random(MaxInt,RandomState)) shl 16))+
                Abs((THashValue(Random(MaxInt,RandomState)) shl 8 ))+
                Abs((THashValue(Random(MaxInt,RandomState)))));
          for j:=0 to Pred(i) do                                                {avoid duplicates}
              if SquareHashValues[j]=SquareHashValues[i] then
                 SquareHashValues[i]:=0;
        until SquareHashValues[i]>MaxInt;
end;

function  TTListLength(Position__:PPosition):Integer;
var BasePosition:PPosition;
begin {transposition table: number of nodes in the chain having 'Position__' as a member}
  Result:=0;
  if Position__<>nil then begin
     Position__:=PPosition(Cardinal(Position__) {and POSITION_POINTER_MASK});
     BasePosition:=Position__;
     repeat Inc(Result);
            Position__:=PPosition(Cardinal(Position__^.HashBucket.Next) {and POSITION_POINTER_MASK});
     until  (Position__=nil) or (Position__=BasePosition);
     end;
end;

function  TTLookup(HashValue__:THashValue; PlayerPos__,SearchDepth__:Integer;
                   var Position__:PPosition):Boolean;
var HashBucketIndex:Integer; BasePosition:PPosition;
begin {transposition table: test if a key has been registered in the table}
  Result:=False;
  with Positions do begin
    HashBucketIndex:=(UInt32(HashValue__) xor PUInt32(NativeUInt(Addr(HashValue__))+NativeUInt(SizeOf(UInt32)))^) and HashBucketMask;
    if HashBucketTimeStamps^[HashBucketIndex]=HashBucketTimeStamp then begin   {'True': this hash bucket has been visited before, i.e., it has been initialized}
       Position__ :=HashBuckets^[HashBucketIndex];
       if Position__<>nil then begin
          BasePosition:=Position__;
          Inc(SearchStatistics.Lookup1Count);
          repeat Inc(SearchStatistics.Lookup2Count);
                 if        HashValue__=Position__^.HashValue then
                           if (Position__^.PlayerPos=PlayerPos__) then begin
                              Result:=True; exit;                               {found: quick-and-dirty exit}
                              end;
                 if        HashValue__>=Position__^.HashValue then begin
                           Position__:=Position__^.HashBucket.Next;             {try next item in this bucket}
                           if Position__=BasePosition then begin                {'True': all items in the chain has been tested}
                              Position__:=nil;                                  {'Position__' = nil: this means "key >= all keys in this bucket" or "empty bucket"}
                              exit;
                           end;
                           end
                 else exit;                                                     {stop searching; the items are sorted in ascending order on keys}
          until  False;                                                         {loop forever; see the body of the loop for quick-and-dirty exit-statements}
          end
       else begin {empty slot}
          end;
       end
    else begin {this hash bucket is accessed for the first time}
       HashBucketTimeStamps^[HashBucketIndex]:=HashBucketTimeStamp;
       HashBuckets^[HashBucketIndex]         :=nil;
       Position__                            :=nil;
       end;
    end;
end;

function  TTMakeForwardPath(Position__:PPosition):Integer;
var Next,This:PPosition;
begin {transposition table: make a forward path from the root position to the given position, using the 'Position.LinstLinks.Prev' field in the positions for the forward links.}
  Result:=0;
  if Position__<>nil then begin
     Next:=nil;
     This:=Position__;
     repeat This^.ListLinks.Prev:=Next; Next:=This; This:=This^.Parent;
            Inc(Result);
     until  (This=nil) or
            ((Next=Position__) and (Next^.ListLinks.Prev<>nil)); {this stop criterion works also when the start position and the end position are identical}
     end;
end;

procedure TTRemove(Position__:PPosition);
var HashBucketIndex:Integer;
begin {transposition table: remove item; precondition: 'Position__' is a fringe-node, i.e., not an interior node (no 'Parent' pointers back to this item)}
//if Position__=Positions.CurrentPosition then
//   Msg(TEXT_INTERNAL_ERROR,'TTRemove - current position');

  if Position__<>nil then with Position__^ do with HashBucket do begin
     Prev^.HashBucket.Next:=Next;
     Next^.HashBucket.Prev:=Prev;
     HashBucketIndex:=(UInt32(HashValue) xor PUInt32(NativeUInt(Addr(HashValue))+NativeUInt(SizeOf(UInt32)))^) and Positions.HashBucketMask;
     if Position__=Positions.HashBuckets^[HashBucketIndex] then                 {'True': removing the bucket root item}
        if   Next<>Position__ then
             Positions.HashBuckets^[HashBucketIndex]:=Next                      {promote the next item to root}
        else Positions.HashBuckets^[HashBucketIndex]:=nil;                      {empty bucket}

     PlayerPos:=0;                                                              {marks the item as unused}

     Position__^.HashBucket.Next:=Positions.FreeList;                           {return the item to the free-list}
     Positions.FreeList:=Position__;

     Dec(Positions.Count); Inc(Positions.SearchStatistics.DroppedCount);        {update counts and statistics}

     if Parent<>nil then with Parent^ do begin
        Dec(SuccessorCount);
        if (not (mfOpen in Move.Flags)) and (not (mfPath in Move.Flags)) then begin {'True': the parent isn't on the open-list, and it isn't a member of the currently investigated path}
           if Position__^.PushCount<BestForgottenScore then
              BestForgottenScore:=PushCount;                                    {save the best forgotten score; note that due to overlapping bytes, it only exists for closed positions}

           if SuccessorCount=0 then begin                                       {'True': the last successor has been removed}
              PushCount:=BestForgottenScore;
              OPENAdd(Position__^.Parent);                                      {put the parent back on the open-queue}
              end;
           end;
        end;
     end;
end;

{-----------------------------------------------------------------------------}

{Generator}

procedure TerminateSearch;
begin // terminates the current search. to terminate the application, call 'Terminate'.
  Generator.SearchLimits.DepthLimit:=-1;
  Generator.SearchLimits.PushCountLimit:=0;
  Positions.OpenPositions.MaxValue:=Low(Positions.OpenPositions.Buckets)-2;     {'-2': signals that some open positions might not have been fully expanded}
end; {TerminateSearch}

procedure Terminate(const Text__:String);
begin
  if not Generator.Terminated then begin
     Generator.Terminated:=True; {the instruction sequence is important}
     TerminateSearch;
     if Text__<>'' then begin
        Msg(Text__,TEXT_APPLICATION_TITLE);
        WritelnToLogFile(Text__);
        WritelnToLogFile('');
        FlushLogFile;
        SetSokobanStatusText(Text__);
        end;
     end;
end; {Terminate}

function  TimeCheck:Boolean;
begin {returns 'True' if the search should continue.}
      {postcondition: if the function returns 'False', the the Sokoban status text explains why the search has been terminated.}
  Result:=True;
  Inc(Generator.TimeCheckCount);
  {$IFDEF WINDOWS}
    if WasKeyPressed( VK_ESCAPE ) then begin
       //Terminate(TEXT_TERMINATED_BY_USER);
       //Result:=False;
       end
    else
  {$ENDIF}
  if (Generator.SearchLimits.TimeLimitMS<>High(Generator.SearchLimits.TimeLimitMS))   {high-value signals unlimited search time}
     or
     Assigned(Generator.SokobanCallBackFunction) then begin
     Generator.Statistics.TimeMS:=CalculateElapsedTimeMS(Generator.StartTimeMS,GetTimeMS);
     if Generator.Statistics.TimeMS<Generator.SearchLimits.TimeLimitMS then begin
        if Assigned(Generator.SokobanCallBackFunction) and
           (Generator.Statistics.TimeMS-Generator.LastCallBackTimeMS>=SOKOBAN_PLUGIN_CALL_BACK_FUNCTION_THRESHOLD_TIME_MS) then begin
           Generator.LastCallBackTimeMS:=Generator.Statistics.TimeMS;
           if PerformSokobanCallBackFunction<>0 then begin                      {'<>0': indicates that the plugin should terminate}
              Terminate(TEXT_TERMINATED_BY_USER);
              Result:=False;
              end;
           end;
        end
     else begin
        Terminate(TEXT_TIME_LIMIT_EXCEEDED);
        Result:=False;
        end;
     end;
end; {TimeCheck}

procedure OrimazeSquareToColRow(Square__:Integer; var Col__,Row__:Integer);
begin {0-based}
  Row__:=Square__   div   Game.OrimazeBoardWidth;
  Col__:=Square__ - Row__*Game.OrimazeBoardWidth;
end;

function  ColRowToOrimazeSquare(Col__,Row__:Integer):Integer;
begin {0-based}
  Result:=(Row__ * Game.OrimazeBoardWidth) + Col__;
end;

function  SetBoardSquare(Board__:TOrimazeBitBoard; Square__:Integer; Value__:TAxis):TOrimazeBitBoard;
begin
  Result:=TOrimazeBitBoard((UInt64(Board__) and (not Generator.PowersOfTwo[Square__])) or (UInt64(Ord(Value__)) shl Square__));
end;

function  WriteOrimazeHeaderToLogFile(Width__,Height__,PlayerPositionIndex__,SegmentIndex__,SegmentCount__,AdHocTask__:Integer):Boolean;
var Index:Integer;
    s:String;
begin
  s:='';
  for Index:=1 to GAME_LINE_LENGTH do s:=s+HYPHEN;
  Result:=WritelnToLogFile(s) and
          WritelnToLogFile(GeneratorTaskCaption('',Width__,Height__,PlayerPositionIndex__,SegmentIndex__,SegmentCount__,AdHocTask__)) and
          WritelnToLogFile(s) and
          WritelnToLogFile('') and
          WritelnToLogFile('');
end;

function  EnumerateOrimazeBoardsOfDimensionsNxM:Boolean;
var
  Col,Row,Index,PlayerSquare,PlayerStartPositionCount,Square:Integer;
  FirstOrimazeBitBoardTableIndex,LastOrimazeBitBoardTableIndex,OrimazeBoardCount:UInt64;
  OrimazeBoard:TOrimazeBoard;
  OriginalFirstOrimazeBitBoard,OriginalLastOrimazeBitBoard,OrimazeBitBoard:TOrimazeBitBoard;
  PlayerStartPositionIndexToSquare:array[0..MAX_ORIMAZE_BOARD_SIZE-1] of Integer;
  PlayerSquareToStartPositionIndex:array[0..MAX_ORIMAZE_BOARD_SIZE-1] of Integer;
  s:String;

  function  OrimazePathToText(Position__:PPosition):String;
  var Index,PushCount:Integer;
  begin
    Result:='';
    if (Position__<>nil) and (Position__^.PushCount>0) then begin
       TTMakeForwardPath(Position__);
       PushCount:=Position__^.PushCount;
       Index:=0;
       SetLength(Result,PushCount);
       Position__:=Positions.StartPosition^.ListLinks.Prev; {skip the root position which doesn't represent a push}
       repeat Inc(Index);
              Result[Index]:=DIRECTION_TO_CHAR[Position__^.Move.Direction];
              Position__:=Position__^.ListLinks.Prev;
       until  (Index=PushCount) or (Position__=nil);
       if Index<>PushCount then
          Msg('OrimazePathToText',TEXT_INTERNAL_ERROR);
       end;
  end;

  procedure OrimazeBitBoard2DTransformation(Transformation__:TBoardTransformation2D; PlayerPos__:Integer; Board__:TOrimazeBitBoard; var NewPlayerPos__:Integer; var NewBoard__:TOrimazeBitBoard);
  var Square:Integer;
  begin
    with Generator do begin
      NewPlayerPos__:=Game.OrimazeBoardSquare2DTransformations[PlayerPos__,Transformation__];
      NewBoard__:=0;
      for Square:=0 to Pred(Game.OrimazeBoardSize) do begin
          if (Board__ and 1)<>0 then
             Inc(NewBoard__,PowersOfTwo[Game.OrimazeBoardSquare2DTransformations[Square,Transformation__]]);
          Board__:=Board__ shr 1;
          end;
      end;
  end;

  procedure OrimazeBitBoardToOrimazeBoard(OrimazeBoardWidth__,OrimazeBoardHeight__,PlayerPos__:Integer;
                                          const OrimazeBitBoard__:TOrimazeBitBoard;
                                          var OrimazeBoard__:TOrimazeBoard);
  var Col,Row,Index:Integer;
  begin
    FillChar(OrimazeBoard__,SizeOf(OrimazeBoard__),0);
    OrimazeBoard__[0]:=WALL;
    Index:=0;
    for Row:=1 to OrimazeBoardHeight__ do
        for Col:=1 to OrimazeBoardWidth__ do begin
            OrimazeBoard__[Succ(Index)]:=AXIS_TO_ORIMAZE_BOARD_SQUARE_VALUE[TAxis((OrimazeBitBoard__ shr Index) and 1)];
            Inc(Index);
            end;
    OrimazeBoard__[Succ(PlayerPos__)]:=PLAYER; {'Succ': from 0-based to 1-based index}
  end;

  procedure DisplayBoardTransformations(PlayerPos__:Integer; const Board__:TOrimazeBitBoard);
  var NewWidth,NewHeight,NewPlayerPos:Integer;
      Transformation:TBoardTransformation2D;
      NewBoard:TOrimazeBitBoard;
      OrimazeBoard:TOrimazeBoard;
  begin
    for Transformation:=Low(Transformation) to High(Transformation) do begin
        OrimazeBitBoard2DTransformation(Transformation,PlayerPos__,Board__,NewPlayerPos,NewBoard);
        Calculate2DDimensionsTransformation(Transformation,Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,NewWidth,NewHeight);
        OrimazeBitBoardToOrimazeBoard(NewWidth,NewHeight,NewPlayerPos,NewBoard,OrimazeBoard);
        ShowOrimazeBoard(NewWidth,NewHeight,OrimazeBoard);
        Writeln('Transformation: ',Ord(Transformation),' Board: ', NewBoard,' Player: ',NewPlayerPos);
        Writeln;
        end;
    //Readln;
  end;

  function  OrimazePuzzleToSokobanPuzzle(Position__:PPosition):Boolean;
  var Col,Row,Index,SquareValue,Count:Integer;
      GridPoint,BottomRightGridPoint:TColRow;
      StartBoard,SolutionBoard:TOrimazeBitBoard;

    function  AddBoxesBetweenGridPoints:Boolean;
    var OrimazeCol,OrimazeRow,AddIndex,Index:Integer;
        GridPoint:array[0..MAX_ORIMAZE_BOARD_WIDTH,0..MAX_ORIMAZE_BOARD_HEIGHT] of TColRow;

      procedure AddColumns(OrimazeCol__:Integer);
      const COPY_COLUMN:array[Boolean] of Byte=(5,4); {copy two columns starting from here}
      var Col,Row:Integer;
      begin
        with Game do
          if BoardWidth<=MAX_BOARD_WIDTH-2 then begin
             Inc(BoardWidth,2);

             for Col:=Succ(BoardWidth) downto GridPoint[OrimazeCol__,0].Col+COPY_COLUMN[Odd(OrimazeCol__)]+2 do
                 for Row:=0 to Succ(BoardHeight) do
                     Board[Col,Row]:=Board[Col-2,Row];

             for Col:=Succ(OrimazeCol__) to Pred(OrimazeBoardWidth) do {update grid point coordinates}
                 for Row:=0 to Pred(OrimazeBoardHeight) do
                     Inc(GridPoint[Col,Row].Col,2);
             end;
      end;

      procedure AddRows(OrimazeRow__:Integer);
      const COPY_ROW:array[Boolean] of Byte=(2,4); {copy two rows starting from here}
      var Col,Row:Integer;
      begin
        with Game do
          if BoardHeight<=MAX_BOARD_HEIGHT-2 then begin
             Inc(BoardHeight,2);

             for Row:=Succ(BoardHeight) downto GridPoint[0,OrimazeRow__].Row+COPY_ROW[Odd(OrimazeRow__)]+2 do
                 for Col:=0 to Succ(BoardWidth) do
                     Board[Col,Row]:=Board[Col,Row-2];

             for Row:=Succ(OrimazeRow__) to Pred(OrimazeBoardHeight) do {update grid point coordinates}
                 for Col:=0 to Pred(OrimazeBoardWidth) do
                     Inc(GridPoint[Col,Row].Row,2);
             end;
      end;

    begin {AddBoxesBetweenGridPoints}
      Result:=Generator.AdditionalBoxesBetweenGridPoints.Count>0;
      if Result then with Game do with Generator.AdditionalBoxesBetweenGridPoints do begin
         for OrimazeCol:=0 to Pred(OrimazeBoardWidth) do
             for OrimazeRow:=0 to Pred(OrimazeBoardHeight) do begin
                 GridPoint[OrimazeCol,OrimazeRow].Col:=SOKOBAN_TEMPLATE_GRID_POINTS[OrimazeCol];
                 GridPoint[OrimazeCol,OrimazeRow].Row:=SOKOBAN_TEMPLATE_GRID_POINTS[OrimazeRow];
                 end;

         AddIndex:=0;

         for OrimazeCol:=0 to OrimazeBoardWidth-2 do begin
             if   AddIndex<Count then
                  Inc(AddIndex)
             else AddIndex:=1;
             for Index:=1 to Numbers[AddIndex] do
                 AddColumns(OrimazeCol);
             end;

         for OrimazeRow:=0 to OrimazeBoardHeight-2 do begin
             if   AddIndex<Count then
                  Inc(AddIndex)
             else AddIndex:=1;
             for Index:=1 to Numbers[AddIndex] do
                 AddRows(OrimazeRow);
             end;

         NormalizeBoard(False,True,True,True,True,BoardWidth,BoardHeight,Board,PlayerPos,History);
         end;
    end;

    procedure UpdateGridPoint(GridPoint__:TColRow; Direction__:TDirection; BoardSquareValue__:TBoardSquareValue);
    var NeighborSquare:TColRow;
    begin
      with Game do begin
        NeighborSquare.Col:=GridPoint__.Col-COMPASS_DIRECTIONS[Direction__].Col;
        NeighborSquare.Row:=GridPoint__.Row-COMPASS_DIRECTIONS[Direction__].Row;
        if (Board[NeighborSquare.Col,NeighborSquare.Row] and (WALL+BoardSquareValue__))=0 then
           Board[NeighborSquare.Col,NeighborSquare.Row]:=Board[NeighborSquare.Col,NeighborSquare.Row] or BoardSquareValue__ or FLOOR;

        NeighborSquare.Col:=GridPoint__.Col+COMPASS_DIRECTIONS[Direction__].Col;
        NeighborSquare.Row:=GridPoint__.Row+COMPASS_DIRECTIONS[Direction__].Row;
        if (Board[NeighborSquare.Col,NeighborSquare.Row] and (WALL+BoardSquareValue__))=0 then
           Board[NeighborSquare.Col,NeighborSquare.Row]:=Board[NeighborSquare.Col,NeighborSquare.Row] or BoardSquareValue__ or FLOOR;
        end;
    end;

  begin {OrimazePuzzleToSokobanPuzzle}
    with Game do begin
      Result:=True;

      BottomRightGridPoint.Col:=SOKOBAN_TEMPLATE_GRID_POINTS[Pred(OrimazeBoardWidth) ];
      BottomRightGridPoint.Row:=SOKOBAN_TEMPLATE_GRID_POINTS[Pred(OrimazeBoardHeight)];

      BoardWidth :=Succ(BottomRightGridPoint.Col);
      BoardHeight:=Succ(BottomRightGridPoint.Row);
      if Odd(OrimazeBoardWidth ) then Inc(BoardWidth ,2);
      if Odd(OrimazeBoardHeight) then Inc(BoardHeight,2);
      ClearBoard(BoardWidth,BoardHeight); {make a wall-framed floor-filled Sokoban board}

      for Row:=1 to BoardHeight do
          for Col:=1 to BoardWidth do
              Board[Col,Row]:=Legend.CharToItem[SOKOBAN_TEMPLATE[Row][Col]]; {fill the Sokoban board with the template}
      for Col:=1 to BoardWidth  do Board[Col,BoardHeight]:=WALL;
      for Row:=1 to BoardHeight do Board[BoardWidth,Row ]:=WALL;

      for Row:=1 to BoardHeight do
          for Col:=1 to BoardWidth do
              if ((Board[Col,Row] and (BOX+GOAL))=(BOX+GOAL))
                 and
                 ((Col>Succ(BottomRightGridPoint.Col))
                  or
                  (Row>Succ(BottomRightGridPoint.Row))) then
                 Board[Col,Row]:=WALL;

      StartBoard:=TOrimazeBitBoard(Positions.StartPosition^.HashValue); {get the Orimaze start position}
      SolutionBoard:=TOrimazeBitBoard(Position__^.HashValue); {get the Orimaze solution position}
      Index:=0;
      for Row:=0 to Pred(OrimazeBoardHeight) do
          for Col:=0 to Pred(OrimazeBoardWidth) do begin {for each grid point on the Sokoban board}
              GridPoint.Col:=SOKOBAN_TEMPLATE_GRID_POINTS[Col];
              GridPoint.Row:=SOKOBAN_TEMPLATE_GRID_POINTS[Row];
              if Index<>Positions.StartPosition^.PlayerPos then
                 UpdateGridPoint(GridPoint,TDirection(TAxis(Cardinal(StartBoard   ) and 1)),BOX );
              if Index<>Position__^.PlayerPos then
                 UpdateGridPoint(GridPoint,TDirection(TAxis(Cardinal(SolutionBoard) and 1)),GOAL);
              StartBoard:=StartBoard       shr 1; {prepare for the next Orimaze square, if any}
              SolutionBoard:=SolutionBoard shr 1; {prepare for the next Orimaze square, if any}
              Inc(Index);
              end;

      Count:=0; {check if the boxes at the column to the right of the used grid points are redundant}
      for Row:=0 to Pred(OrimazeBoardHeight) do begin
          SquareValue:=Board[Succ(BottomRightGridPoint.Col),SOKOBAN_TEMPLATE_GRID_POINTS[Row]];
          if ((SquareValue and (BOX+GOAL))=0) or
             ((SquareValue and (BOX+GOAL))<>(BOX+GOAL)) then Inc(Count);
          end;
      if Count=0 then
         for Row:=0 to Pred(OrimazeBoardHeight) do
             if (Board[Succ(BottomRightGridPoint.Col),SOKOBAN_TEMPLATE_GRID_POINTS[Row]] and (BOX+GOAL))<>0 then
                 Board[Succ(BottomRightGridPoint.Col),SOKOBAN_TEMPLATE_GRID_POINTS[Row]]:=WALL;

      Count:=0; {check if boxes at the row below the used grid points are redundant}
      for Col:=0 to Pred(OrimazeBoardWidth) do begin
          SquareValue:=Board[SOKOBAN_TEMPLATE_GRID_POINTS[Col],Succ(BottomRightGridPoint.Row)];
          if ((SquareValue and (BOX+GOAL))=0) or
             ((SquareValue and (BOX+GOAL))<>(BOX+GOAL)) then Inc(Count);
          end;
      if Count=0 then
         for Col:=0 to Pred(OrimazeBoardWidth) do
             if (Board[SOKOBAN_TEMPLATE_GRID_POINTS[Col],Succ(BottomRightGridPoint.Row)] and (BOX+GOAL))<>0 then
                 Board[SOKOBAN_TEMPLATE_GRID_POINTS[Col],Succ(BottomRightGridPoint.Row)]:=WALL;

      if Odd(OrimazeBoardWidth ) then begin
         for Row:=2 to Pred(BottomRightGridPoint.Row) do
             Board[BottomRightGridPoint.Col+2,Row]:=FLOOR;
         end;
      if Odd(OrimazeBoardHeight) then begin
         for Col:=2 to Pred(BottomRightGridPoint.Col) do
             Board[Col,BottomRightGridPoint.Row+2]:=FLOOR;
         end;

      if Generator.AdditionalBoxesBetweenGridPoints.Count>0 then
         AddBoxesBetweenGridPoints;
      end;
  end;

  function  WriteOrimazePuzzleToLogFile(Position__:PPosition; WriteSokobanPuzzle__:Boolean):Boolean;
  var OrimazeBoard:TOrimazeBoard;
  begin
    Result:=Assigned(Position__);
    if Result then with Generator do begin
       if WriteSokobanPuzzle__ then
          Result:=OrimazePuzzleToSokobanPuzzle(Position__) and
                  WriteBoardToLogFile(TEXT_ORIMAZE+SPACE+IntToStr(Game.OrimazeBoardWidth)+'x'+IntToStr(Game.OrimazeBoardHeight)+SPACE+
                                      IntToStr(Int64(TOrimazeBitBoard(Positions.StartPosition^.HashValue)))+PERIOD+IntToStr(Positions.StartPosition^.PlayerPos)+SPACE+
                                      IntToStr(Position__^.PushCount),0) and
                  WritelnToLogFile('');

       OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,Positions.StartPosition^.PlayerPos,TOrimazeBitBoard(Positions.StartPosition^.HashValue),OrimazeBoard);
       WriteOrimazeBoardToLogFile(IntToStr(Int64(TOrimazeBitBoard(Positions.StartPosition^.HashValue)))+PERIOD+IntToStr(Positions.StartPosition^.PlayerPos),'',0,OrimazeBoard);
       WritelnToLogFile('');
       OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,Position__^.PlayerPos,TOrimazeBitBoard(Position__^.HashValue),OrimazeBoard);
       WriteOrimazeBoardToLogFile(IntToStr(Int64(TOrimazeBitBoard(Position__^.HashValue)))+PERIOD+IntToStr(Position__^.PlayerPos),'',0,OrimazeBoard);
       WritelnToLogFile('');
       WritelnToLogFile(TEXT_ORIMAZE+SPACE+TEXT_SOLUTION_+COLON+SPACE+IntToStr(Position__^.PushCount)+SPACE+TEXT_PLAYER_+SPACE+TEXT_MOVES[Position__^.PushCount<>1]);
       WritelnToLogFile(DOUBLE_QUOTE+OrimazePathToText(Position__)+DOUBLE_QUOTE);
       WritelnToLogFile('');
       WritelnToLogFile('');
       Result:=FlushLogFile and Result;
       end;
  end;

  function  WriteLongestOrimazePuzzlesToLogFile(BestPosition__:PPosition; BestPositionCount__:Integer):Boolean;
  var Index:Integer;
      Position:PPosition;
  begin
    Result:=True;
    BestPosition__^.ListLinks.Next:=nil;      {end of best positions list}
    Index:=0;                                 {rover index}
    Position:=PPosition(Positions.Positions); {rover}
    while BestPositionCount__>0 do begin {write all the longest Orimaze puzzles to the log file}
      Inc(Generator.BestPuzzlesCount);
      if Generator.BestPuzzlesCount<=MAX_LOGGED_BEST_PUZZLES_COUNT then begin
         Result:=Result and WriteOrimazePuzzleToLogFile(BestPosition__,True); {collect maximum length puzzles}
         end
      else if Generator.BestPuzzlesCount=MAX_LOGGED_BEST_PUZZLES_COUNT+1 then begin
              WritelnToLogFile('');
              if   Generator.RandomState.RandomNumber<0 then
                   WritelnToLogFile(TEXT_MORE_PUZZLES_OF_LENGTH[True]+SPACE+IntToStr(BestPosition__^.PushCount))
              else WritelnToLogFile(TEXT_MORE_PUZZLES_OF_LENGTH[False]);
              WritelnToLogFile('');
              FlushLogFile;
              end;

      Include(BestPosition__^.Move.Flags,mfVisited);
      Dec(BestPositionCount__);
      if BestPositionCount__>0 then begin
         while (Index<Positions.Count)
               and
               ((Position^.PushCount<>BestPosition__^.PushCount)
                or
                (mfVisited in Position^.Move.Flags)) do begin
               Inc(Index);    {advance to next position in the transposition table}
               Inc(Position); {advance to next position in the transposition table}
               end;
         if   Index<Positions.Count then begin
              Position^.ListLinks.Next:=BestPosition__; {make list with best positions}
              BestPosition__:=Position;
              end
         else Msg('BreadthFirstSearch 01',TEXT_INTERNAL_ERROR);
         end;
      end;

    {remove 'Visited' flags}
    while Assigned(BestPosition__^.ListLinks.Next) do begin
          Exclude(BestPosition__^.Move.Flags,mfVisited);
          BestPosition__:=BestPosition__^.ListLinks.Next;
          end;
    Exclude(BestPosition__^.Move.Flags,mfVisited);
  end;

  {$IFDEF CALCULATE_CLUSTERS}
    function  IsOrimazePuzzleStartPositionSelectedForEnumeration(PlayerPos__:Integer; const Board__:TOrimazeBitBoard):Boolean;
    var PlayerPositionIndex:Integer;
    begin {returns 'True' if the position is one of the player start positions selected for enumeration}
      PlayerPositionIndex:=PlayerSquareToStartPositionIndex[PlayerPos__];
      Result:=(PlayerPositionIndex>=0)                                          {'True': the player is located at one of the start position squares used by the enumerator}
              and
              ((PlayerPositionIndex=Generator.PlayerPositionIndex)              {'True': the player is located at the start position square selected for investigation}
               or
               (Generator.PlayerPositionIndex<0)                                {'True': all player start positions have been selected for investigation}
              )
              and
              (Board__>=Generator.FirstOrimazeBitBoard)
              and
              (Board__<=Generator.LastOrimazeBitBoard);
    end;
  {$ENDIF}

  function  BreadthFirstSearch(PlayerPos__:Integer; Board__:TOrimazeBitBoard; IsInitialBFSForFindingClusterMembers__:Boolean):Boolean;
  var {$IFDEF CALCULATE_SEQUENTIALLY}
        Index:Integer;
      {$ELSE}
        IsFirstEccentricityCalculationForStartPosition:Boolean;
      {$ENDIF}
      Dividend,Divisor:UInt64;
      Position:PPosition;
      //OrimazeBoard:TOrimazeBoard;

    function  Search(Position__:PPosition):Integer;
    var
      NeighborSquare:Integer;
      Direction:TDirection;
      NeighborSquareAxis:TAxis;
      PositionBoard,SuccessorBoard:TOrimazeBitBoard;
      SuccessorPosition:PPosition;

      procedure ShowPath(Position__:PPosition);
      var OrimazeBoard:TOrimazeBoard;
      begin
        if Position__<>nil then begin
           if Position__^.Parent<>nil then
              ShowPath(Position__^.Parent);
           OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,Position__^.PlayerPos,TOrimazeBitBoard(Position__^.HashValue),OrimazeBoard);
           ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
           Write('Depth: ',Position__^.PushCount,SPACE,DIRECTION_TO_TEXT[Position__^.Move.Direction],' Board: ',TOrimazeBitBoard(Position__^.HashValue),PERIOD,Position__^.PlayerPos);
           Readln;
           end;
      end; {Search.ShowPath}
{
      function  GetCommonAncestor( Position1__,Position2__:PPosition):PPosition;
      var Position:PPosition;
      begin
        Include(Positions.StartPosition^.Move.Flags,mfVisited);
        Position:=Position1__;
        while not (mfVisited in Position^.Move.Flags) do begin
          Include(Position^.Move.Flags,mfVisited);
          Position:=Position^.Parent;
          end;

        Result:=Position2__;
        while not (mfVisited in Result^.Move.Flags) do
          Result:=Result^.Parent;

        Exclude(Positions.StartPosition^.Move.Flags,mfVisited);
        Position:=Position1__;
        while mfVisited in Position^.Move.Flags do begin
          Exclude(Position^.Move.Flags,mfVisited);
          Position:=Position^.Parent;
          end;
      end; // Search.GetCommonAncestor
}
    begin {BreadthFirstSearch.Search}
      Result:=Position__^.PushCount;
      if Position__^.PushCount<Generator.SearchLimits.DepthLimit then with Generator do begin
         PositionBoard:=TOrimazeBitBoard(Position__^.HashValue);
         {$IFDEF CALCULATE_SEQUENTIALLY}
           VisitedPlayerSquares[Position__^.PlayerPos]:=True; {mark the squares the player has reached during this BFS (breadth-first search)}
         {$ENDIF}
         for Direction:=Low(Direction) to High(Direction) do
             if {(Result>=0) and} {if 'False': clean exit from loop. not in production because it's a time-critical loop.}
                ((Direction<>OPPOSITE_DIRECTION[Position__^.Move.Direction]) or (Position__^.PushCount=0)) then begin {don't move back}
                NeighborSquare:=Game.OrimazeBoardSquareNeighbors[Position__^.PlayerPos,Direction];
                if NeighborSquare>=0 then begin
                   NeighborSquareAxis:=TAxis((PositionBoard shr NeighborSquare) and 1);
                   if DIRECTION_TO_AXIS[Direction]=NeighborSquareAxis then begin {'True': a legal move in this direction}
                      Inc(Generator.Statistics.PushCount); {update statistics}
                      if Generator.Statistics.PushCount<=Generator.SearchLimits.PushCountLimit then begin {'True': limit not exceeded}
                         if Cardinal(Generator.Statistics.PushCount and (ONE_MEBI-1))=0 then begin
                            Write('Puzzle: ',Succ(Statistics.GeneratedPuzzlesCount)); {'Succ': including the currently investigated puzzle}
                            if (Generator.RandomState.RandomNumber    <0) then {'True': enumerate Orimaze puzzle boards}
                               Write(SLASH,Statistics.EnumeratedBoardsCount);
                            Write(' Depth: ',Position__^.PushCount);
                            Write(' Pushes: ',Generator.Statistics.PushCount div ONE_MILLION,' million');
                            Write(' Positions: ',Positions.Count);
                            Writeln;
                            if not TimeCheck then
                               Terminate(Generator.SokobanStatusPointer^.StatusText);
                            end;

                         {perform the move on a copy of the board from the current position}
                         SuccessorBoard:=PositionBoard;
                         if Ord(NeighborSquareAxis)<>0 then begin                   {'True': it's necessary to "move the bit" which represents the tile on the board}
                            Dec(SuccessorBoard,PowersOfTwo[NeighborSquare]);        {the tile leaves this square, i.e., the player moves to this square}
                            Inc(SuccessorBoard,PowersOfTwo[Position__^.PlayerPos]); {the tile moves to this square, i.e., the player leaves this square}
                            end;

                         //OrimazeBitBoardToOrimazeBoard(OrimazeBoardWidth,OrimazeBoardHeight,NeighborSquare,SuccessorBoard,OrimazeBoard);
                         //ShowOrimazeBoard(OrimazeBoardWidth,OrimazeBoardHeight,OrimazeBoard);
                         //Writeln(Count__,' Depth: ',Position__^.PushCount);

                         {$IFDEF CALCULATE_CLUSTERS}
                           if IsInitialBFSForFindingClusterMembers__                {'True': this is an initial BFS (breadth-first search) which finds all the puzzle positions in a cluster}
                              and
                              (NeighborSquare<=Positions.StartPosition^.PlayerPos)  {'True': the new player position is less than or equal to the player starting position for the BFS (breadth-first search) which finds all the positions in the cluster}
                              and
                              (PlayerSquareToStartPositionIndex[NeighborSquare]>=0) {'True': the player is located at one of the start position squares used by the enumerator}
                              and
                              (SuccessorBoard<TOrimazeBitBoard(Positions.StartPosition^.HashValue)) {'True': it's a puzzle start position belonging to an already investigated cluster, or a cluster investigated with different task parameters}
                              and
                              (Result>=0)                                           {'True': this is the first time the filter is triggered in this loop}
                              then with Generator.Statistics do begin
                              {the successor position is a puzzle start position which belongs to an already investigated cluster,
                               or a cluster which will be investigated with a different set of task parameters}
                              Inc(BFSStartedForMembersOfClustersProcessedElsewhere);
                              Inc(BFSStartedForMembersOfClustersProcessedElsewherePushCount,Positions.Count);
                              OPENClear;
                              Positions.BestPosition:=nil; {return an empty best path, but also return the function result value 'True' to indicate that the search succeeded}
                              Positions.BestPositionCount:=0;
                              Result:=-1; {clean exit from loop. not in production because it's a time-critical loop.}
                              if IsOrimazePuzzleStartPositionSelectedForEnumeration(NeighborSquare,SuccessorBoard) then
                                 Inc(BFSStartedForMembersOfClustersProcessedEarlier);
                              break; {quick-and-dirty exit the direction loop and the function}
                              end
                           else
                         {$ENDIF}
                             begin
                               {try to add the successor position to the transposition-table.
                                if it already is in the table, then 'TTAdd' returns 'False'
                                and the existing position in 'SuccessorPosition'}
                               if TTAdd(THashValue(SuccessorBoard),             {hash value}
                                        NeighborSquare,                         {player position}
                                        Succ(Position__^.PushCount),            {push count}
                                        Succ(Position__^.PushCount),            {score}
                                        Position__^.PlayerPos,                  {box number (not used in Orimaze)}
                                        Succ(Position__^.PushCount),            {search depth}
                                        Direction,                              {move direction (in Orimaze, it's the player move direction)}
                                        Position,                               {parent position}
                                        SuccessorPosition) then begin           {succesor position}
                                  if   (SuccessorPosition^.PushCount<=Generator.SearchLimits.DepthLimit) then
                                       OPENAdd(SuccessorPosition)               {put the position on the open-queue}
                                  else Terminate(TEXT_DEPTH_LIMIT_EXCEEDED);
                                  end
                               else                                             {the position wasn't added to the transposition-table}
                                  if SuccessorPosition<>nil then begin          {'True': the position already exists in the transposition-table}
                                     end
                                  else begin                                    {the transposition-table is full}
                                     Terminate(TEXT_MEMORY_FULL);
                                     end;
                             end;
                         end
                      else begin                                                {push count limit exceeded}
                         Terminate(TEXT_PUSHES_LIMIT_EXCEEDED);
                         end;
                      end;
                   end;
                end;
         end
      else begin                                                                {depth limit exceeded}
         Terminate(TEXT_DEPTH_LIMIT_EXCEEDED);
       end;
    end; {BreadthFirstSearch.Search}

  begin {BreadthFirstSearch}
    with Generator do begin
      Generator.DisplayPushCountdown:=0;
      Generator.Statistics.PushCount:=0;
      if IsInitialBFSForFindingClusterMembers__ then begin
         {this is a BFS (breadth-first search) from an Orimaze game starting
          position selected by the enumerator, as opposed to a BFS from another
          game starting position belonging to the same cluster}

         {$IFDEF CALCULATE_SEQUENTIALLY}
           {clear the set of visited board squares}
           for Index:=0 to Pred(Game.OrimazeBoardSize) do
               Generator.VisitedPlayerSquares[Index]:=False;
         {$ENDIF}
         end;

      TTClear(IsInitialBFSForFindingClusterMembers__); {clear the transposition table entirely, or just the moves which connect the Orimaze game states}
      OPENClear; {clear open-queue}

      Result:=TTAdd(THashValue(Board__),PlayerPos__,0,0,0,0,dUp,nil,Positions.StartPosition)
              and
              OPENAdd(Positions.StartPosition);
      if   Result then
           Include(Positions.StartPosition^.Move.Flags,mfPath)
      else Terminate(TEXT_MEMORY_FULL);

      while OPENRemoveNextPositionSelectedForExpansion(Position) do
        Search(Position);

      Inc(Statistics.BFSTotalPushCount,Statistics.PushCount);

      if Result and Assigned(Positions.BestPosition) then begin
         if IsInitialBFSForFindingClusterMembers__ then
            Inc(Statistics.BFSCountForEnumeratedPuzzles);
         Inc(Statistics.BFSTotalCount);
         Inc(Statistics.PushCountHistogram[Min(Positions.BestPosition^.PushCount,High(Statistics.PushCountHistogram))]);
         {$IFDEF CALCULATE_CLUSTERS}
           IsFirstEccentricityCalculationForStartPosition:=Positions.StartPosition^.Eccentricity=0;
           Positions.StartPosition^.Eccentricity:=Positions.BestPosition^.PushCount; {store the length of the longest outgoing acyclic path from the starting position}
         {$ENDIF}

         if (Positions.BestPosition^.PushCount>Generator.BestOrimazePushCount)  {'True': new best result}
            {$IFDEF CALCULATE_CLUSTERS}
              or
              ((Positions.BestPosition^.PushCount=Generator.BestOrimazePushCount)
               and
               (PlayerSquareToStartPositionIndex[Generator.BestOrimazePlayerPosBegin]<0) {'True': the best found Orimaze puzzle doesn't have the player located at one of the start position squares used by the enumerator}
               and
               (PlayerSquareToStartPositionIndex[Positions.StartPosition^.PlayerPos]>=0)  {'True': the player is located at one of the start position squares used by the enumerator}
              )
            {$ENDIF}
            then begin
            if Generator.BestOrimazePushCount<>Positions.BestPosition^.PushCount then begin
               Generator.BestOrimazePushCount:=Positions.BestPosition^.PushCount;
               Writeln(TEXT_BEST_RESULT_SO_FAR,COLON,SPACE,Positions.BestPosition^.PushCount);
               end;

            Position:=Positions.BestPosition; {save the moves in the game history}
            Game.History.Count:=Position^.PushCount;
            if Position^.PushCount>0 then
               repeat Game.History.Moves[Position^.PushCount]:=Position^.Move;
                      Position:=Position^.Parent;
               until  Position=Positions.StartPosition; {this works also if the start position and the end position are identical}

            Generator.BestOrimazeBoardBegin:=Board__;
            Generator.BestOrimazePlayerPosBegin:=PlayerPos__;
            Generator.BestOrimazeBoardEnd:=TOrimazeBitBoard(Positions.BestPosition^.HashValue);
            Generator.BestOrimazePlayerPosEnd:=Positions.BestPosition^.PlayerPos;
            Generator.BestPuzzlesCount:=0;

            Result:=WriteLongestOrimazePuzzlesToLogFile(Positions.BestPosition,Positions.BestPositionCount);
            end
         else if (Positions.BestPosition^.PushCount=Generator.BestOrimazePushCount) {'True': found another puzzle of the same length as the best result found so far}
                 {$IFDEF CALCULATE_CLUSTERS}
                   and
                   (PlayerSquareToStartPositionIndex[Positions.StartPosition^.PlayerPos]>=0) {'True': the player is located at one of the start position squares used by the enumerator}
                   and
                   IsFirstEccentricityCalculationForStartPosition {'True': this is the first time the eccentricity has been calculated for this Orimaze puzzle starting position}
                 {$ENDIF}
                 then begin
                 Result:=WriteLongestOrimazePuzzlesToLogFile(Positions.BestPosition,Positions.BestPositionCount);
                 end;

         if (Positions.Count+Positions.SearchStatistics.DroppedCount)>Positions.MaxPositionCount then
            Positions.MaxPositionCount:=Positions.Count+Positions.SearchStatistics.DroppedCount;

         if (Cardinal(Succ(Statistics.GeneratedPuzzlesCount)) and ((256*ONE_KIBI)-1))=0 then begin {'Succ': including the currently investigated puzzle}
            //Writeln(Count__,SPACE,Board__,SPACE,PlayerPos__);
            //OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPos__,Board__,OrimazeBoard);
            //ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
            //Readln;
            Write(IntToStr_(Succ(Statistics.GeneratedPuzzlesCount))); {'Succ': including the currently investigated puzzle}
            if (SegmentSize>0) and (Generator.RandomState.RandomNumber<0) then begin {'True': no numeric overflow, and not generating random boards}
               Write(SLASH,IntToStr_(Statistics.EnumeratedBoardsCount));
               Divisor:=SegmentSize;
               if Generator.PlayerPositionIndex<0 then {'True': generator boards for all player start positions}
                  Divisor:=Divisor*PlayerStartPositionCount;
               Dividend:=(Statistics.EnumeratedBoardsCount+Statistics.EnumeratedBoardsWhichAreInvalidBecauseThePlayerPositionOverlapsANonZeroBitValueOnTheBoard)*100; {may overflow}
               if (Dividend>0) and (Dividend<=High(Dividend)-(Divisor div 2)) then {'True': round up}
                  Inc(Dividend,(Divisor div 2)); {may overflow}
               if (Dividend>0) and (Divisor>0) then {'True': no numeric overflow}
                  Write(SPACE,Dividend div Divisor,PERCENT);
               end;
            Writeln;
            end;
         end;

      Result:=Result and (Generator.SearchLimits.DepthLimit>=0); {'>=0': the search hasn't been terminated}
      end;
  end; {BreadthFirstSearch}

  function  EnumerateBoardsForPlayerPosition(PlayerPos__:Integer; var Count__:UInt64):Boolean;
  var Index:Integer;
      OrimazeBitBoard,PreviousSearchedOrimazeBitBoard:TOrimazeBitBoard;

    function  IsItNecessaryToTestIfThePuzzleHasALongestSolution(PlayerPos__:Integer; const Board__,PreviousSearchedBoard__:TOrimazeBitBoard):Boolean;
    var Col,Row,PlayerCol,PlayerRow,LegalMovesCount:Integer;
        {$IFDEF CALCULATE_SEQUENTIALLY}
          Square,NeighborSquare:Integer;
        {$ENDIF}
        Direction:TDirection;
        NeighborSquareValues:array[TDirection] of TOrimazeBoardSquareValue;
        BitValue:Integer;
        ByteIndex:TSize;
        TableIndex:UInt64;
        //OrimazeBoard:TOrimazeBoard;

      function  OrimazeNeighborSquareValue(Square__:Integer; Direction:TDirection; const Board__:TOrimazeBitBoard):TOrimazeBoardSquareValue;
      var NeighborSquare:Integer;
      begin
        NeighborSquare:=Game.OrimazeBoardSquareNeighbors[Square__,Direction];
        if   NeighborSquare>=0 then
             Result:=AXIS_TO_ORIMAZE_BOARD_SQUARE_VALUE[TAxis((Board__ shr NeighborSquare) and 1)]
        else Result:=WALL;
      end;

      function  IsVerticalSpanWithVerticalTilesOnly(Col__,TopRow__,BottomRow__:Integer; const Board__:TOrimazeBitBoard):Boolean;
      var Square,BottomSquare:Integer;
      begin {bottom row inclusive. postcondition: returns 'True' for an empty span.}
        Square:=ColRowToOrimazeSquare(Col__,TopRow__);
        BottomSquare:=Square+((BottomRow__-TopRow__)*Game.OrimazeBoardWidth);
        while (Square<=BottomSquare) and (TAxis((Board__ shr Square) and 1)=aVertical) do Inc(Square,Game.OrimazeBoardWidth);
        Result:=Square>BottomSquare;
      end;

      function  IsHorizontalSpanWithHorizontalTilesOnly(LeftCol__,RightCol__,Row__:Integer; const Board__:TOrimazeBitBoard):Boolean;
      var Square,RightSquare:Integer;
      begin {right column inclusive. postcondition: returns 'True' for an empty span.}
        Square:=ColRowToOrimazeSquare(LeftCol__,Row__);
        RightSquare:=Square+(RightCol__-LeftCol__);
        while (Square<=RightSquare) and (TAxis((Board__ shr Square) and 1)=aHorizontal) do Inc(Square);
        Result:=Square>RightSquare;
      end;

      function  IsColumnWithHorizontalTilesOnly(Col__:Integer; const Board__:TOrimazeBitBoard):Boolean;
      var Square:Integer;
      begin
        Square:=ColRowToOrimazeSquare(Col__,Pred(Game.OrimazeBoardHeight));
        while (Square>=0) and (TAxis((Board__ shr Square) and 1)=aHorizontal) do Dec(Square,Game.OrimazeBoardWidth);
        Result:=Square<0;
      end;

      function  IsRowWithVerticalTilesOnly(Row__:Integer; const Board__:TOrimazeBitBoard):Boolean;
      var Square,FirstSquareInNextRow:Integer;
      begin
        Square:=ColRowToOrimazeSquare(0,Row__);
        FirstSquareInNextRow:=Square+Game.OrimazeBoardWidth;
        while (Square<FirstSquareInNextRow) and (TAxis((Board__ shr Square) and 1)=aVertical) do Inc(Square);
        Result:=Square=FirstSquareInNextRow;
      end;

    begin {IsItNecessaryToTestIfThePuzzleHasALongestSolution}
      Result:=True;
      {calculate the number of legal player moves in the starting position}
      LegalMovesCount:=0;

      for Direction:=Low(Direction) to High(Direction) do begin
          NeighborSquareValues[Direction]:=OrimazeNeighborSquareValue(PlayerPos__,Direction,Board__);
          if   NeighborSquareValues[Direction]=HORIZONTAL then
               if   DIRECTION_TO_AXIS[Direction]=aHorizontal then {'True': horizontal tile at neighbor square on the horizontal axis}
                    Inc(LegalMovesCount)
               else
          else if (NeighborSquareValues[Direction]=VERTICAL) and
                  (DIRECTION_TO_AXIS[Direction]=aVertical) then   {'True': vertical   tile at neighbor square on the vertical   axis}
                  Inc(LegalMovesCount);
          end;
      if LegalMovesCount=0 then begin
         Result:=False;
         Inc(Generator.Statistics.EnumerationFilterZeroPushes);
         exit; {quick-and-dirty exit function}
         end;

      OrimazeSquareToColRow(PlayerPos__,PlayerCol,PlayerRow);
      {$IFDEF CALCULATE_SEQUENTIALLY}
        {for symmetrical puzzle boards (i.e., NxN boards) with the player
         starting at a diagonal square, it suffices to test boards with at least
         one horizontal tile next to the player along the horizontal axis}
        if (Game.OrimazeBoardWidth=Game.OrimazeBoardHeight) and
           (PlayerCol=PlayerRow) then
           if (NeighborSquareValues[dLeft ]=HORIZONTAL) or
              (NeighborSquareValues[dRight]=HORIZONTAL) or
              (Game.OrimazeBoardWidth=1) then {OK}
           else begin
              Result:=False;
              Inc(Generator.Statistics.EnumerationFilterSymmetry);
              exit; {quick-and-dirty exit function}
              end;
      {$ENDIF}

      {player moves in the Orimaze game are reversible, so the cluster
       consisting of the starting position and all the game states reachable
       from the starting position can be seen as vertices in an undirected
       graph.

       from graph theory it's known that:
         2 * eccentricity( vertex ), i.e., the longest acyclic optimal outgoing path from the vertex
         >=
         maximum eccentricity for all vertices in the graph = the graph diameter.

       this follows from the fact that it's a connected graph with root node S,
       the starting position for the breadth-first search which found all the
       connected nodes. from each node A it's always possible to get to any
       other node B by first going back from A to S, and then go from S to B.
       each of these two parts of the path can at the most have the same length
       as the diameter, which is defined as the longest path between any pair of
       nodes in graph.

       prune the puzzle if it's...
       * registered as having a shorter longest acyclic path than the best
         result found so far, or
       * a registered member of an already processed cluster.
      }
      if Result and Assigned(Positions.VisitedOrimazePuzzles.Positions[PlayerPos__]) then begin {'True': a "Visited?" table has been allocated for this player start position}
         TableIndex:=OrimazeBitBoardToTableIndex(PlayerPos__,Board__)-FirstOrimazeBitBoardTableIndex;
         //if (TableIndex<0) or (TableIndex>LastOrimazeBitBoardTableIndex) then
         //   Msg('IsItNecessaryToTestIfThePuzzleHasALongestSolution 1',TEXT_INTERNAL_ERROR);
         ByteIndex:=TableIndex shr LOG2_BITS_PER_BYTE;
         BitValue:=1 shl (Byte(TableIndex) mod BITS_PER_BYTE);
         if Positions.VisitedOrimazePuzzles.Positions[PlayerPos__]^[ByteIndex] and BitValue<>0 then begin // 'True': a visited Orimaze puzzle start position
            Result:=False;
            Inc(Generator.Statistics.EnumerationFilterAlreadyVisitedCluster);
            exit; //quick-and-dirty exit from the function
            end;
        end;

      {puzzle boards with an impenetrable barrier around the player cannot be the (only) longest puzzles.
       examples showing the player "*" in each of the 4 possible "quadrants":
       .v....    ....v.    ......    ......
       .v....    ....v.    hhhh..    ..hhhh
       .v.*..    ..*.v.    ....v.    .v....
       .v....    ....v.    ..*.v.    .v.*..
       ..hhhh    hhhh..    ....v.    .v....
       ......    ......    ....v.    .v....
         I         II        III       IV
      }
      for Col:=0 to Pred(Game.OrimazeBoardWidth) do
          if (Col<>PlayerCol) then begin
             if IsVerticalSpanWithVerticalTilesOnly(Col,0,Pred(PlayerRow),Board__) then {'True': the column is filled with 'v' tiles from the top down to the row above the player}
                for Row:=Succ(PlayerRow) to Pred(Game.OrimazeBoardHeight) do
                    if TAxis((Board__ shr ColRowToOrimazeSquare(Col,Pred(Row))) and 1)=aVertical then begin {'True': so far, the column is filled with 'v' tiles from the top}
                       if Col<PlayerCol then begin
                          if IsHorizontalSpanWithHorizontalTilesOnly(Succ(Col),Pred(Game.OrimazeBoardWidth),Row,Board__) then begin {quadrant I}
                             Inc(Generator.Statistics.EnumerationFilterFencedInPlayer);
                             Result:=False;
                             //if True then begin
                             //   OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPos__,Board__,OrimazeBoard);
                             //   ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
                             //   Write('Column: ',Col,' Row: ',Row,'    ',Count__);
                             //   Readln;
                             //   end;
                             exit; {quick-and-dirty exit function}
                             end;
                          end
                       else begin {column > player column}
                          if IsHorizontalSpanWithHorizontalTilesOnly(0,Pred(Col),Row,Board__) then begin {quadrant II}
                             Inc(Generator.Statistics.EnumerationFilterFencedInPlayer);
                             Result:=False;
                             //if True then begin
                             //   OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPos__,Board__,OrimazeBoard);
                             //   ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
                             //   Write('Column: ',Col,' Row: ',Row,'    ',Count__);
                             //   Readln;
                             //   end;
                             exit; {quick-and-dirty exit function}
                             end;
                          end;
                       end
                    else break; {the column isn't filled with 'v' tiles from the top. 'break': quick-and-dirty exit the 'for each row' loop.}

             if IsVerticalSpanWithVerticalTilesOnly(Col,Succ(PlayerRow),Pred(Game.OrimazeBoardHeight),Board__) then {'True': the column is filled with 'v' tiles from the bottom up to the row below the player}
                for Row:=Pred(PlayerRow) downto 0 do
                    if TAxis((Board__ shr ColRowToOrimazeSquare(Col,Succ(Row))) and 1)=aVertical then begin {'True': so far, the column is filled with 'v' tiles from the bottom up}
                       if Col<PlayerCol then begin
                          if IsHorizontalSpanWithHorizontalTilesOnly(Succ(Col),Pred(Game.OrimazeBoardWidth),Row,Board__) then begin {quadrant IV}
                             Inc(Generator.Statistics.EnumerationFilterFencedInPlayer);
                             Result:=False;
                             //if True then begin
                             //   OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPos__,Board__,OrimazeBoard);
                             //   ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
                             //   Write('Column: ',Col,' Row: ',Row,'    ',Count__);
                             //   Readln;
                             //   end;
                             exit; {quick-and-dirty exit function}
                             end;
                          end
                       else begin {column > player column}
                          if IsHorizontalSpanWithHorizontalTilesOnly(0,Pred(Col),Row,Board__) then begin {quadrant III}
                             Inc(Generator.Statistics.EnumerationFilterFencedInPlayer);
                             Result:=False;
                             //if True then begin
                             //   OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPos__,Board__,OrimazeBoard);
                             //   ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
                             //   Write('Column: ',Col,' Row: ',Row,'    ',Count__);
                             //   Readln;
                             //   end;
                             exit; {quick-and-dirty exit function}
                             end;
                          end;
                       end
                     else break; {the column isn't filled with 'v' tiles from the bottom. 'break': quick-and-dirty exit the 'for each row' loop and continue to the next column.}
             end;

      {puzzle boards with a vertical-tiles-only column in the starting position cannot be the (only) longest puzzles.
       example:
       ....v.
       ....v.
       ..*.v.
       ....v.
       ....v.
       ....v.
      }
      for Col:=0 to Pred(Game.OrimazeBoardWidth) do
          if (Col<>PlayerCol) and
             IsVerticalSpanWithVerticalTilesOnly(Col,0,Pred(Game.OrimazeBoardHeight),Board__) then begin
             Inc(Generator.Statistics.EnumerationFilterColumnWithVerticalTiles);
             Result:=False;
             //OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPos__,Board__,OrimazeBoard);
             //ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
             //Write('Column: ',Succ(Col),'    ',Count__);
             //Readln;
             exit; {quick-and-dirty exit from the function}
             end;

      {puzzle boards with a horizontal-tiles-only row in the starting position cannot be the (only) longest puzzles.
       example:
       ......
       ......
       ..*...
       ......
       hhhhhh
       ......
      }
      for Row:=0 to Pred(Game.OrimazeBoardHeight) do
          if (Row<>PlayerRow) and
             IsHorizontalSpanWithHorizontalTilesOnly(0,Pred(Game.OrimazeBoardWidth),Row,Board__) then begin
             Result:=False;
             Inc(Generator.Statistics.EnumerationFilterRowWithHorizontalTiles);
             //OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPos__,Board__,OrimazeBoard);
             //ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
             //Write('Row: ',Succ(Row),'    ',Count__);
             //Readln;
             exit; {quick-and-dirty exit from the function}
             end;

      {puzzle boards with horizontal-tiles-only in the first or the last column in the starting position cannot be the (only) longest puzzles.
       board height 1: the filter isn't applicable.
       board height 2: the player must not be in the column.
       board height 3 or more: the player is allowed to be in the column.

       examples:
       h.....    .....h
       h.....    .....h
       *.....    ..*..h
       h.....    .....h
       h.....    .....h
       h.....    .....h
      }
      if Game.OrimazeBoardHeight>1 then begin
         if PlayerCol<>0 then begin {fast lane}
            if IsColumnWithHorizontalTilesOnly(0,Board__) then begin
               Inc(Generator.Statistics.EnumerationFilterFirstOrLastColumnWithHorizontalTiles);
               Result:=False;
               exit; {quick-and-dirty exit from the function}
               end;
            end
         else if (Game.OrimazeBoardHeight>2) and IsColumnWithHorizontalTilesOnly(0,SetBoardSquare(Board__,PlayerPos__,aHorizontal)) then begin
                 //if PlayerPos__<>0 then begin
                 //   OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPos__,Board__,OrimazeBoard);
                 //   ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
                 //   Write('First column');
                 //   Readln;
                 //   end;
                 Inc(Generator.Statistics.EnumerationFilterFirstOrLastColumnWithHorizontalTiles);
                 Result:=False;
                 exit; {quick-and-dirty exit from the function}
                 end;
         if PlayerCol<>Pred(Game.OrimazeBoardWidth) then begin {fast lane}
            if IsColumnWithHorizontalTilesOnly(Pred(Game.OrimazeBoardWidth),Board__) then begin
               Inc(Generator.Statistics.EnumerationFilterFirstOrLastColumnWithHorizontalTiles);
               Result:=False;
               exit; {quick-and-dirty exit from the function}
               end;
            end
         else if (Game.OrimazeBoardHeight>2) and IsColumnWithHorizontalTilesOnly(Pred(Game.OrimazeBoardWidth),SetBoardSquare(Board__,PlayerPos__,aHorizontal)) then begin
                 //if PlayerPos__<>0 then begin
                 //   OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPos__,Board__,OrimazeBoard);
                 //   ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
                 //   Write('Last column');
                 //   Readln;
                 //   end;
                 Inc(Generator.Statistics.EnumerationFilterFirstOrLastColumnWithHorizontalTiles);
                 Result:=False;
                 exit; {quick-and-dirty exit from the function}
                 end;
         end;

      {puzzle boards with vertical-tiles-only in the first or the last row in the starting position cannot be the (only) longest puzzles.
       board width 1: the filter isn't applicable.
       board width 2: the player must not be in the row.
       board width 3 or more: the player is allowed to be in the row.

       examples:
       *vvvvv    ......
       ......    ......
       ......    ..*...
       ......    ......
       ......    ......
       ......    vvvvvv
      }
      if Game.OrimazeBoardWidth>1 then begin
         if PlayerRow<>0 then begin {fast lane}
            if IsRowWithVerticalTilesOnly(0,Board__) then begin
               Inc(Generator.Statistics.EnumerationFilterFirstOrLastRowWithVerticalTiles);
               Result:=False;
               exit; {quick-and-dirty exit from the function}
               end;
            end
         else if (Game.OrimazeBoardWidth>2) and IsRowWithVerticalTilesOnly(0,SetBoardSquare(Board__,PlayerPos__,aVertical)) then begin
                 //if PlayerPos__<>0 then begin
                 //   OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPos__,Board__,OrimazeBoard);
                 //   ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
                 //   Write('First row');
                 //   Readln;
                 //   end;
                 Result:=False;
                 Inc(Generator.Statistics.EnumerationFilterFirstOrLastRowWithVerticalTiles);
                 exit; {quick-and-dirty exit from the function}
                 end;
         if PlayerRow<>Pred(Game.OrimazeBoardHeight) then begin {fast lane}
            if IsRowWithVerticalTilesOnly(Pred(Game.OrimazeBoardHeight),Board__) then begin
               Inc(Generator.Statistics.EnumerationFilterFirstOrLastRowWithVerticalTiles);
               Result:=False;
               exit; {quick-and-dirty exit from the function}
               end;
            end
         else if (Game.OrimazeBoardWidth>2) and IsRowWithVerticalTilesOnly(Pred(Game.OrimazeBoardHeight),SetBoardSquare(Board__,PlayerPos__,aVertical)) then begin
                 //if PlayerPos__<>0 then begin
                 //   OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPos__,Board__,OrimazeBoard);
                 //   ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
                 //   Write('Last row');
                 //   Readln;
                 //   end;
                 Inc(Generator.Statistics.EnumerationFilterFirstOrLastRowWithVerticalTiles);
                 Result:=False;
                 exit; {quick-and-dirty exit from the function}
                 end;
         end;

      {$IFDEF CALCULATE_SEQUENTIALLY}
        {compare the current board with the board from the previous search. at
         least one square has changed. if the player in the previous search
         didn't reach one of the now changed squares, or a neighbor of one of
         these changed squares, then a search with the current board as start
         position cannot produce a longer path than the previous search.}
        if Result and (Board__<>PreviousSearchedBoard__) then begin
           Result:=False;
           for Square:=0 to Pred(Game.OrimazeBoardSize) do
               if ((Board__ and Generator.PowersOfTwo[Square])<>(PreviousSearchedBoard__ and Generator.PowersOfTwo[Square])) and {'True': the square has changed, compared to the previous searched board}
                  (not Result) then begin
                  if Generator.VisitedPlayerSquares[Square] then begin
                     if Generator.AdHocTask<>2 then
                        Result:=True
                     else begin
                        {Ad hoc task 2: Investigate puzzles if the player in the
                         starting position has more than one legal move.

                         An early version of the program applied the logically
                         flawed filter "discard puzzle candidate if the player in
                         the starting position has more than one legal move".

                         The filter is flawed because it doesn't take cyclic paths
                         around the starting position or the solution position
                         into account. An example, courtesy of Matthias Meger:
                         o1 - o2 - o3
                         |         |
                         o4 - o5 - o6 - o7 - o8
                        }
                        Result:=LegalMovesCount>1; {'True': investigate starting positions with more than one legal player move}
                        if not Result then
                           Inc(Generator.Statistics.EnumerationFilterOtherFilters);
                        end;
                     exit; {quick-and-dirty exit from the function}
                     end
                  else for Direction:=Low(Direction) to High(Direction) do begin
                           NeighborSquare:=Game.OrimazeBoardSquareNeighbors[Square,Direction];
                           if (NeighborSquare>=0) and
                              Generator.VisitedPlayerSquares[NeighborSquare] then begin
                              if Generator.AdHocTask<>2 then
                                 Result:=True
                              else begin
                                 Result:=LegalMovesCount>1; {'True': investigate starting positions with more than one legal player move}
                                 if not Result then
                                    Inc(Generator.Statistics.EnumerationFilterOtherFilters);
                                 end;
                              exit; {quick-and-dirty exit from the function}
                              end;
                           end;
                  end;
           if not Result then
              Inc(Generator.Statistics.EnumerationFilterSameReachableSquaresAsPreviousPuzzle);
           end;
      {$ENDIF}
    end; {IsItNecessaryToTestIfThePuzzleHasALongestSolution}

    function  CalculateNumberOfStartPositionsInCluster:Integer;
    var Index:Integer;
        Position:PPosition;
    begin
      Result:=0;
      Position:=PPosition(Positions.Positions);
      for Index:=0 to Pred(Positions.Count) do begin
          if PlayerSquareToStartPositionIndex[Position^.PlayerPos]>=0 then
             Inc(Result);
          Inc(Position); {advance to the next position in the BFS (breadth-first search) transposition table}
          end;
    end;

    procedure UpdateVisitedOrimazePuzzleStartPositions(PlayerPos__:Integer);
    var BitValue:Integer;
        Index,ByteIndex:TSize;
        TableIndex:UInt64;
        Position:PPosition;
    begin
      Inc(Positions.VisitedOrimazePuzzles.ClusterCount);
      Position:=PPosition(Positions.Positions);
      for Index:=0 to Pred(Positions.Count) do begin
          if Assigned(Positions.VisitedOrimazePuzzles.Positions[Position^.PlayerPos]) and {'True': the player is located at one of the player start positions, and there is a global 'Visited?' table for that player position}
             (Position^.PlayerPos>=PlayerPos__) and {'True': the enumeration hasn't finished Orimaze puzzles with this player start position yet}
             (TOrimazeBitBoard(Position^.HashValue)>=Generator.FirstOrimazeBitBoard) and
             (TOrimazeBitBoard(Position^.HashValue)<=Generator.LastOrimazeBitBoard ) then begin
             TableIndex:=OrimazeBitBoardToTableIndex(Position^.PlayerPos,TOrimazeBitBoard(Position^.HashValue))-FirstOrimazeBitBoardTableIndex;
             ByteIndex:=TableIndex shr LOG2_BITS_PER_BYTE;
             BitValue:=1 shl (Byte(TableIndex) mod BITS_PER_BYTE);
             if (Positions.VisitedOrimazePuzzles.Positions[Position^.PlayerPos]^[ByteIndex] and BitValue)=0 then begin {'True': an unvisited Orimaze puzzle start position}
                Inc(Positions.VisitedOrimazePuzzles.Positions[Position^.PlayerPos]^[ByteIndex],BitValue); {mark the Orimaze start position as visited}
                Inc(Positions.VisitedOrimazePuzzles.Count);
                end;
             end;
          Inc(Position); {advance to the next position in the BFS transposition table}
          end;
    end; {UpdateVisitedOrimazePuzzleStartPositions}

    {$IFDEF CALCULATE_CLUSTERS}
      function  CalculateLongestPathForPuzzleCandidatesInCluster(PlayerPos__:Integer):Boolean;
      var Index,Countdown,LowerBound,UpperBound,Depth,NextDepth,StartPositionCount,BFSCount:Integer;
          IsClusterDiameterSmallerThanBestResult:Boolean;
          Position,StartPositionList:PPosition; {positions = game states = puzzle candidates = graph vertices = graph nodes}
          //OrimazeBoard:TOrimazeBoard;

        function  BreadtFirstSearchFromBestPositionAfterPreviousBFS(var {io:} LowerBound__:Integer; var {io:} IsClusterDiameterSmallerThanBestResult__:Boolean; var {io:} BFSCount__:Integer):Boolean;
        begin
          if Positions.BestPosition^.Eccentricity>0 then begin {'True': the eccentricity has already been calculated for this position}
             if Positions.BestPosition^.Eccentricity=Positions.StartPosition^.Eccentricity then begin
                {the existing path from 'StartPosition' to 'BestPosition' has the same length as the longest outgoing acyclic path from 'BestPosition',
                 so there is no need to run a new search. it would just produce a new path of the same length.}
                Result:=True;
                end
             else begin {calculate the eccentricity for this position again, producing its longest outgoing acyclic path and ordering the game states according to their BFS search depth}
                Result:=BreadthFirstSearch(Positions.BestPosition^.PlayerPos,TOrimazeBitBoard(Positions.BestPosition^.HashValue),False);
                if Result then
                   Inc(BFSCount__);
                end;
             end
          else begin {calculate the eccentricity for this position}
             Result:=BreadthFirstSearch(Positions.BestPosition^.PlayerPos,TOrimazeBitBoard(Positions.BestPosition^.HashValue),False);
             if Result then
                Inc(BFSCount__);
             end;
          if Result then begin
             if Positions.BestPosition^.PushCount>LowerBound__ then
                LowerBound__:=Positions.BestPosition^.PushCount;
             if (2*Positions.BestPosition^.PushCount)<Generator.BestOrimazePushCount then {'True': all positions in the cluster have an eccentricity (longest acyclic path) which is smaller than the best result found so far}
                IsClusterDiameterSmallerThanBestResult__:=True;
             end;
        end;

      begin {CalculateLongestPathForPuzzleCandidatesInCluster}
        {Algorithm: iFUB, with 4-Sweep heuristic to select start node.
         Article: On computing the diameter of real-world undirected graphs
         Authors: Pilu Crescenzia, Roberto Grossi, Michel Habib, Leonardo Lanzia, Andrea Marino
         Original Website: https://who.rocq.inria.fr/Laurent.Viennot/road/papers/ifub.pdf
         Retrieved date: 2017-02-19
        }

        {4-Sweep: a heuristic to search for a graph node close to the "middle"
         of the graph. ideally, it finds a node with eccentricity = radius of
         the graph.
         the start node r1 in the article is here the end position of a longest
         acyclic path found by the initial BFS (breadth-first search), which
         found all the positions in the cluster.
        }
        BFSCount:=0; {number of BFSes (breadth-first searches)}
        StartPositionCount:=0; {number of Orimaze puzzle start positions in the cluster}
        IsClusterDiameterSmallerThanBestResult:=False;
        LowerBound:=Positions.BestPosition^.PushCount;
        Index:=4;
        repeat
          {perform a BFS (breadth-first search) from the best position found by
           the previous BFS in order to calculate the eccentricity of the position,
           i.e., the length of its longest acyclic outgoing path}
          Result:=BreadtFirstSearchFromBestPositionAfterPreviousBFS(LowerBound,IsClusterDiameterSmallerThanBestResult,BFSCount);
          if Result then begin
             if not IsClusterDiameterSmallerThanBestResult then begin {'True': there is a chance that the cluster contains one or more longest puzzle candidates}
                if Odd(Index) then begin {'True': select the node in the middle between the start position and the position farthest away}
                   Countdown:=Succ(Positions.BestPosition^.PushCount) div 2;
                   while Countdown>0 do begin
                     Positions.BestPosition:=Positions.BestPosition^.Parent;
                     Dec(Countdown);
                     end;
                   end;
                end
             else begin {all starting positions in the cluster have a maximum acyclic path length < longest found path so far}
                Index:=0; {exit loop}
                end;
             end
          else Index:=0; {BFS failed. exit loop with failure.}
          Dec(Index);
        until Index<=0;

        if Index=0 then begin {'True': the 4-Sweep heuristic returned a starting position in 'Positions.BestPosition' for the iFUB algorithm. otherwise, a BFS failed or revealed that the cluster diameter is smaller than the best found result so far.}
           {start of iFUB algorithm}
           //OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,Positions.BestPosition^.PlayerPos,TOrimazeBitBoard(Positions.BestPosition^.HashValue),OrimazeBoard);
           //ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
           //Write(Index,SPACE,Count__,' Start iFUB: ',LowerBound,SPACE,Positions.BestPosition^.PushCount);
           //Readln;

           {perform a BFS from the iFUB starting position to calculate its eccentricity, and to sort all the nodes (game states) in the cluster according to their BFS search depth from the starting position}
           Result:=BreadtFirstSearchFromBestPositionAfterPreviousBFS(LowerBound,IsClusterDiameterSmallerThanBestResult,BFSCount);
           if Result and (not IsClusterDiameterSmallerThanBestResult) then begin
              {collect the Orimaze puzzle starting positions found by the BFS, if any, sorting them in descending order on distance from the start position.}
              StartPositionCount:=0;
              StartPositionList:=nil;
              Position:=PPosition(Positions.Positions);
              for Index:=0 to Pred(Positions.Count) do begin
                  if   PlayerSquareToStartPositionIndex[Position^.PlayerPos]>=0 then begin {'True': the player is located at one of the start position squares used by the enumerator}
                       Position^.Move.Depth:=Position^.PushCount; {save the BFS depth}
                       Position^.HashBucket.Prev:=StartPositionList; {use the hash bucket 'previous' link for the list links}
                       StartPositionList:=Position;
                       Inc(StartPositionCount);
                       end
                  else Position^.Move.Depth:=0; {'0': don't investigate this position. it isn't an Orimaze puzzle starting position selected for enumeration with the given set of task parameters}
                  Inc(Position); {advance to the next position in the BFS transposition table}
                  end;

              Depth:=Positions.BestPosition^.PushCount;
              UpperBound:=2*Depth;
              while (Depth>0) and
                    (not IsClusterDiameterSmallerThanBestResult) and
                    (LowerBound<=UpperBound) and {'<=' and not '<': ensure that all puzzle candidates are investigated if there is a chance that they have one or more longest outgoing acyclic paths}
                    Result do begin {'True': calculate the eccentricity (the longest path) for the puzzle candidates on the list at the current iFUB depth}

                NextDepth:=Pred(Depth);
                Position:=StartPositionList;
                while Assigned(Position) do begin
                  if Position^.Move.Depth=Depth then begin
                     if Position^.Eccentricity=0 then begin {'True': the eccentricity hasn't been calculated yet. do it now.}
                        Result:=BreadthFirstSearch(Position^.PlayerPos,TOrimazeBitBoard(Position^.HashValue),False);
                        if Result then
                           Inc(BFSCount);
                        end;
                     if Position^.Eccentricity>LowerBound then
                        LowerBound:=Position^.Eccentricity;
                     IsClusterDiameterSmallerThanBestResult:=(2*Position^.Eccentricity)<Generator.BestOrimazePushCount; {'True': all positions in the cluster have an eccentricity (longest path) which is smaller than the best result found so far}
                     if LowerBound>2*NextDepth then begin {'True': all positions on the list with a smaller iFUB depth have a smaller eccentricity than the ones already calculated}
                        NextDepth:=0; {exit outer loop}
                        {continue in the inner loop, i.e., investigate all positions at the current iFUB depth. there may be more than one longest puzzle.}
                        end;
                     end;
                  Position:=Position^.HashBucket.Prev; {advance to the next Orimaze puzzle start position on the list}
                  end;

                Depth:=NextDepth; {advance to the next iFUB depth (decrease the depth)}
                UpperBound:=2*Depth; {update the upper bound according to the new iFUB depth}
                end;
              end;
           end;


        if Result then with Generator.Statistics do begin
           if StartPositionCount=0 then {'True': the number of Orimaze puzzle start positions in the cluster hasn't been calculated yet. do it now.}
              for Index:=0 to Pred(Positions.Count) do
                  if PlayerSquareToStartPositionIndex[Positions.Positions^[Index].PlayerPos]>=0 then {'True': the player is located at one of the start position squares used by the enumerator}
                     Inc(StartPositionCount);

           Inc(IFUBBreadthFirstSearchCount,BFSCount);
           Inc(IFUBClusterCount);
           Inc(IFUBClusterPositionCount,Positions.Count);
           Inc(IFUBClusterStartPositionCount,StartPositionCount);
           Inc(IFUBHistogram[Min(High(IFUBHistogram),(((BFSCount*100)+(StartPositionCount div 2)) div Max(1,StartPositionCount)) div High(IFUBHistogram))]);
           if IsClusterDiameterSmallerThanBestResult then
              Inc(IFUBFilterClusterDiameterShorterThanBestResult);
           end;
      end; {CalculateLongestPathForPuzzleCandidatesInCluster}
    {$ENDIF}

  begin {EnumerateBoardsForPlayerPosition}
    Result:=(not Generator.Terminated) and (Game.OrimazeBoardSize>0);
    if Result and (Generator.SegmentSize>0) then with Generator do begin
       FirstOrimazeBitBoardTableIndex:=OrimazeBitBoardToTableIndex(PlayerPos__,FirstOrimazeBitBoard);
       LastOrimazeBitBoardTableIndex :=OrimazeBitBoardToTableIndex(PlayerPos__,LastOrimazeBitBoard);
       if AdHocTask<>1 then
          OrimazeBitBoard:=FirstOrimazeBitBoard
       else begin { investigate the last board in the segment only. an early version of the program had an off-by-one bug which had the effect that the last board wasn't analyzed.}
          OrimazeBitBoard:=LastOrimazeBitBoard;
          if SegmentSize>0 then
             SegmentSize:=1;
          end;

       PreviousSearchedOrimazeBitBoard:=TOrimazeBitBoard(not (THashValue(OrimazeBitBoard))); {ensure that the current board and the previous searched board are different, so the board triggers a search}
       {$IFDEF CALCULATE_SEQUENTIALLY}
         for Index:=0 to Pred(Game.OrimazeBoardSize) do
             VisitedPlayerSquares[Index]:=True;
       {$ENDIF}
       Index:=0;

       while Result and (not Terminated) and (Index>=0) do begin
         if   (OrimazeBitBoard and Generator.PowersOfTwo[PlayerPos__])=0 then begin {'True': the Orimaze bit board has a zero-value tile at the player position}
              Inc(Statistics.EnumeratedBoardsCount);
              if IsItNecessaryToTestIfThePuzzleHasALongestSolution(PlayerPos__,OrimazeBitBoard,PreviousSearchedOrimazeBitBoard) then begin
                 Result:=BreadthFirstSearch(PlayerPos__,OrimazeBitBoard,True);
                 if Result and                                                     {'True': the BFS (breadth-first search) succeeded}
                    Assigned(Positions.BestPosition) then begin                    {'True': the position doesn't belong to an already investigated cluster, or a cluster which will be investigated with a different set of task parameters}
                    Inc(Statistics.GeneratedPuzzlesCount);
                    if ((2*Positions.BestPosition^.PushCount)<Generator.BestOrimazePushCount) then begin {'True': all starting positions in the cluster have a maximum eccentricity < longest found acyclic outgoing path so far}
                       Inc(Statistics.BFSFilterClusterDiameterShorterThanBestResult);
                       if  Positions.VisitedOrimazePuzzles.MemoryByteSize>0 then begin {'True': there is a table for registering at least some of the visited Orimaze puzzles}
                           {player moves in the Orimaze game are reversible, so the start position
                            for the search and all the game states reachable from the start position
                            can be seen as vertices in an undirected graph.

                            from graph theory it's known that:
                              2 * eccentricity( vertex ), i.e., the longest acyclic optimal outgoing path from the vertex
                              >=
                              maximum eccentricity for all the vertices in the graph = the graph diameter.

                            mark all other potential Orimaze start positions in the cluster as visited,
                            i.e., as positions with a longest acyclic path, which is known to be shorter
                            than the best result found so far.
                           }
                          UpdateVisitedOrimazePuzzleStartPositions(PlayerPos__);
                          end;
                       end
                    else begin
                       {$IFDEF CALCULATE_CLUSTERS}
                         {calculate the diameter (the longest path) of the cluster consisting of all the game states reachable from the puzzle created by the enumeration, i.e., all the positions found by the BFS (breadth-first search)}
                         Result:=CalculateLongestPathForPuzzleCandidatesInCluster(PlayerPos__);
                         if Positions.VisitedOrimazePuzzles.MemoryByteSize>0 then {'True': there is a table for registering at least some of the visited Orimaze puzzles}
                            UpdateVisitedOrimazePuzzleStartPositions(PlayerPos__);
                       {$ENDIF}
                       end;
                    end;
                 PreviousSearchedOrimazeBitBoard:=OrimazeBitBoard;
                 end;
              end
         else Inc(Statistics.EnumeratedBoardsWhichAreInvalidBecauseThePlayerPositionOverlapsANonZeroBitValueOnTheBoard);

         if   OrimazeBitBoard<>LastOrimazeBitBoard then
              Inc(OrimazeBitBoard) {advance to the next Orimaze bit board}
         else Index:=-1; {exit loop}
         end;

       with Positions.VisitedOrimazePuzzles do
         if Assigned(Positions[PlayerPos__]) then begin {'True': a "Visited?" table has been allocated for this player position}
            FreeMem(Positions[PlayerPos__]); {the "Visited?" table for this player start position isn't used anymore. release the memory.}
            Positions[PlayerPos__]:=nil;
            Dec(MemoryByteSize,BitVectorByteSize);
            end;
       end;
  end; {EnumerateBoardsForPlayerPosition}

  function  WriteHistogramToLogFile(const ErrorText__:String):Boolean;
  var Index:Integer;
      Divisor,Total:UInt64;
      s:String;
      TimeSeconds:TTImeMS;
  begin
    with Generator do with Statistics do begin
      Result:=True;
      Total:=0;
      TimeSeconds:=(TimeMS+500) div ONE_THOUSAND;
      s:=TEXT_ORIMAZE+SPACE+IntToStr(Game.OrimazeBoardWidth)+'x'+IntToStr(Game.OrimazeBoardHeight)+ ' - '+TEXT_HISTOGRAM_FOR_GENERATED_PUZZLES;
      if PlayerPositionIndex>=0 then
         s:=s+' - '+TEXT_PLAYER_POSITION_INDEX+SPACE+IntToStr(PlayerPositionIndex);
      if SegmentIndex>=0 then
         s:=s+SPACE+TEXT_SEGMENT+SPACE+IntToStr(SegmentIndex)+SPACE+TEXT_OF+SPACE+IntToStr(SegmentCount);
      if AdHocTask>0 then
         s:=s+SPACE+TEXT_AD_HOC+SPACE+IntToStr(AdHocTask);
      Result:=Result and WritelnToLogFile(s);
      Index:=0;
      while PushCountHistogram[Index]=0 do
        Inc(Index);
      for Index:=Index to BestOrimazePushCount do begin
          Result:=Result and WritelnToLogFile(IntToStr(Index)+COLON+SPACE+IntToStr_(PushCountHistogram[Index]));
          Inc(Total,PushCountHistogram[Index]);
          end;
      if   (((High(Total)-(EnumeratedBoardsCount div 2)) div 100)>=Total) and (ErrorText__='') then begin {'True': no numeric overflow and no errors detected}
           if   EnumeratedBoardsCount<>0 then
                Divisor:=EnumeratedBoardsCount
           else Divisor:=1;
           s:=SPACE+TEXT_OF+SPACE+IntToStr_(EnumeratedBoardsCount)+
              SPACE+LEFT_PARENTHESIS+IntToStr(((Total*100)+(EnumeratedBoardsCount div 2)) div Divisor)+PERCENT+RIGHT_PARENTHESIS;
           end
      else s:='';
      Result:=Result and WritelnToLogFile(TEXT_GENERATED_PUZZLES+COLON+SPACE+IntToStr_(Total)+s);
      Result:=Result and WritelnToLogFile(TEXT_TIME+COLON+SPACE+IntToStr_(TimeSeconds)+SPACE+TEXT_SECONDS[TimeSeconds<>1]);
      Result:=Result and WritelnToLogFile(TEXT_MAXIMUM_GENERATED_POSITIONS_FOR_A_PUZZLE+COLON+SPACE+IntToStr_(Positions.MaxPositionCount));
      if ErrorText__<>'' then
         Result:=Result and WritelnToLogFile(TEXT_ERROR+COLON+SPACE+ErrorText__);
      Result:=FlushLogFile and (Total=OrimazeBoardCount) and Result;
      if Total<>BFSTotalCount then
         Msg(TEXT_INTERNAL_ERROR,TEXT_HISTOGRAM_FOR_GENERATED_PUZZLES);
      end;
  end; {WriteHistogramToLogFile}

  function  WriteStatisticsToLogFile:Boolean;
  var {$IFDEF CALCULATE_CLUSTERS}
        Index:Integer;
      {$ENDIF}
      FilteredCount:UInt64;
  begin
    if True then with Generator.Statistics do begin
       WritelnToLogFile('');
       WritelnToLogFile(GeneratorTaskCaption('',Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,Generator.PlayerPositionIndex,Generator.SegmentIndex,Generator.SegmentCount,Generator.AdHocTask));
       {$IFDEF CALCULATE_SEQUENTIALLY}
         WritelnToLogFile('Statistics (Sequential Puzzle Calculation)');
       {$ELSE}
         WritelnToLogFile('Statistics (Cluster-based Puzzle Calculation)');
       {$ENDIF}
       WritelnToLogFile('Enumerated puzzles....................................: '+IntToStr_(EnumeratedBoardsCount));
       WritelnToLogFile('Enumeration filters');
       WritelnToLogFile('  Column with vertical tiles..........................: '+IntToStrWithPercentage(EnumerationFilterColumnWithVerticalTiles,EnumeratedBoardsCount));
       WritelnToLogFile('  Fenced in player....................................: '+IntToStrWithPercentage(EnumerationFilterFencedInPlayer,EnumeratedBoardsCount));
       WritelnToLogFile('  First/last column with horizontal tiles.............: '+IntToStrWithPercentage(EnumerationFilterFirstOrLastColumnWithHorizontalTiles,EnumeratedBoardsCount));
       WritelnToLogFile('  First/last row    with vertical   tiles.............: '+IntToStrWithPercentage(EnumerationFilterFirstOrLastRowWithVerticalTiles,EnumeratedBoardsCount));
       WritelnToLogFile('  Member of registered, already visited cluster.......: '+IntToStrWithPercentage(EnumerationFilterAlreadyVisitedCluster,EnumeratedBoardsCount));
       if EnumerationFilterOtherFilters<>0 then
          WritelnToLogFile('  Other...............................................: '+IntToStrWithPercentage(EnumerationFilterOtherFilters,EnumeratedBoardsCount));
       WritelnToLogFile('  Row with horizontal tiles...........................: '+IntToStrWithPercentage(EnumerationFilterRowWithHorizontalTiles,EnumeratedBoardsCount));
       {$IFDEF CALCULATE_SEQUENTIALLY}
         WritelnToLogFile('  Same reachable squares as previous puzzle...........: '+IntToStrWithPercentage(EnumerationFilterSameReachableSquaresAsPreviousPuzzle,EnumeratedBoardsCount));
         WritelnToLogFile('  NxN board symmetry for diagonal squares.............: '+IntToStrWithPercentage(EnumerationFilterSymmetry,EnumeratedBoardsCount));
       {$ENDIF}
       WritelnToLogFile('  Zero legal moves....................................: '+IntToStrWithPercentage(EnumerationFilterZeroPushes,EnumeratedBoardsCount));
       FilteredCount:=EnumerationFilterColumnWithVerticalTiles+
                      EnumerationFilterFencedInPlayer+EnumerationFilterFirstOrLastColumnWithHorizontalTiles+
                      EnumerationFilterFirstOrLastRowWithVerticalTiles+EnumerationFilterAlreadyVisitedCluster+
                      EnumerationFilterOtherFilters+EnumerationFilterRowWithHorizontalTiles+
                      EnumerationFilterSameReachableSquaresAsPreviousPuzzle+EnumerationFilterSymmetry+
                      EnumerationFilterZeroPushes;
       WritelnToLogFile('  Total...............................................: '+IntToStrWithPercentage(FilteredCount,EnumeratedBoardsCount));
       if (FilteredCount+GeneratedPuzzlesCount+BFSStartedForMembersOfClustersProcessedElsewhere)<>EnumeratedBoardsCount then begin
          Writeln(EnumeratedBoardsCount,SPACE,
                  FilteredCount+GeneratedPuzzlesCount+BFSStartedForMembersOfClustersProcessedElsewhere,SPACE,
                  EnumeratedBoardsCount-(FilteredCount+GeneratedPuzzlesCount+BFSStartedForMembersOfClustersProcessedElsewhere));
          Msg(TEXT_INTERNAL_ERROR+': Statistics: Filters',TEXT_APPLICATION_TITLE);
          end;
       WritelnToLogFile('BFS (Breadth-first search)');
       WritelnToLogFile( {$IFDEF CALCULATE_SEQUENTIALLY}
                           '  BFSes for enumerated puzzles........................: '
                         {$ELSE}
                           '  Completed BFSes for enumerated puzzles..............: '
                         {$ENDIF}
                         +
                         IntToStrWithPercentage(BFSCountForEnumeratedPuzzles,EnumeratedBoardsCount));
       WritelnToLogFile('    Cluster diameter shorter than best result.........: '+IntToStrWithPercentage(BFSFilterClusterDiameterShorterThanBestResult,BFSCountForEnumeratedPuzzles));
       {$IFDEF CALCULATE_CLUSTERS}
         WritelnToLogFile('  Started for members of clusters processed elsewhere.: '+IntToStrWithPercentage(BFSStartedForMembersOfClustersProcessedElsewhere,EnumeratedBoardsCount));
         WritelnToLogFile('    Moves.............................................: '+IntToStrWithPercentage(BFSStartedForMembersOfClustersProcessedElsewherePushCount,BFSTotalPushCount));
       {$ENDIF}
       {$IFDEF CALCULATE_CLUSTERS}
         WritelnToLogFile('  iFUB algorithm');
         WritelnToLogFile('    Calculated clusters...............................: '+IntToStrWithPercentage(IFUBClusterCount,BFSCountForEnumeratedPuzzles));
         WritelnToLogFile('      Cluster diameter shorter than best result.......: '+IntToStrWithPercentage(IFUBFilterClusterDiameterShorterThanBestResult,IFUBClusterCount));
         WritelnToLogFile('    BFSes for cluster members, mainly start positions.: '+IntToStrWithPercentage(IFUBBreadthFirstSearchCount,IFUBClusterStartPositionCount)); {including initial 4-Sweep BFSes, which may not be puzzle start positions}
         WritelnToLogFile('    Cluster members, Orimaze puzzle start positions...: '+IntToStrWithPercentage(IFUBClusterStartPositionCount,IFUBClusterPositionCount));
         WritelnToLogFile('    Cluster members, total Orimaze puzzle positions...: '+IntToStr_(IFUBClusterPositionCount));
         WritelnToLogFile('    Histogram for BFSes/cluster size..................: (10% intervals)');
         s:='   ';
         for Index:=Low(IFUBHistogram) to High(IFUBHistogram) do
             s:=s+SPACE+IntToStr_(IFUBHistogram[Index]);
         WritelnToLogFile(s);
         WritelnToLogFile('  Completed BFSes.....................................: '+IntToStr_(BFSTotalCount));
       {$ENDIF}
       WritelnToLogFile('  Moves performed by BFSes............................: '+IntToStr_(BFSTotalPushCount));
       WritelnToLogFile('  Maximum number of generated positions for a puzzle..: '+IntToStr_(Positions.MaxPositionCount));
       WritelnToLogFile('Time, seconds.........................................: '+IntToStr_((TimeMS+500) div ONE_THOUSAND));
       Result:=FlushLogFile;
       end
    else Result:=True;
  end; {WriteStatisticsToLogFile}

  function  GenerateCommandFiles(PlayerPositionIndex__:Integer):Boolean;
  var Index,SegmentIndex:Integer;
      ExeFileName,FileName,s:String;
      CommandFile:TextFile;
  begin
    Result:=True;
    if (PlayerPositionIndex__=0) or
       (PlayerPositionIndex__=Generator.PlayerPositionIndex) then begin {generate command files only once for splitting on player positions}
       s:=ParamStr(0);
       Index:=Length(s);
       while (Index>0) and (s[Index]<>BACKSLASH) and (s[Index]<>COLON) do
         Dec(Index);
       if (Index=2) and (s[Index]=COLON) and (Length(s)>2) and (s[3]=BACKSLASH) then
          Inc(Index);
       ExeFileName:=Copy(s,Succ(Index),Length(s));
       Writeln;

       if   Generator.PlayerPositionIndex>=0 then {'True': split on player positions. the actual value of the specified player position index is ignored.}
            PlayerPositionIndex__:=PlayerStartPositionCount
       else PlayerPositionIndex__:=Generator.PlayerPositionIndex;

       repeat {for each player position index, if specified}
         Dec(PlayerPositionIndex__);

         SegmentIndex:=Generator.SegmentCount;
         repeat {for each segment, if specified}
           Dec(SegmentIndex);
           FileName:=MakeLogFileName(TEXT_APPLICATION_TITLE,Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,PlayerPositionIndex__,SegmentIndex,Generator.SegmentCount,Generator.AdHocTask)+COMMAND_FILE_EXT;
           Writeln(FileName);
           AssignFile(CommandFile,FileName);
           Rewrite(CommandFile);
           Writeln(CommandFile,'@echo off');
           s:=ExeFileName+' -width '+IntToStr(Game.OrimazeBoardWidth)+' -height '+IntToStr(Game.OrimazeBoardHeight);
           //if GenerateAllOrimazePuzzlesUpToSpecifiedDimensions then
           //   s:=s+' -all';
           if Generator.PlayerPositionIndex>=0 then
              s:=s+' -player '+IntToStr(PlayerPositionIndex__);
           if Generator.SegmentCount>0 then
              s:=s+' -segment '+IntToStr(SegmentIndex)+ ' of '+IntToStr(Generator.SegmentCount);
           if Generator.AdditionalBoxesBetweenGridPoints.Count>0 then begin
              s:=s+' -boxes';
              for Index:=1 to Generator.AdditionalBoxesBetweenGridPoints.Count do
                  s:=s+SPACE+IntToStr(Generator.AdditionalBoxesBetweenGridPoints.Numbers[Index]);
              end;
           if Generator.IsMemorySpecified then begin
              s:=s+' -memory ';
              if   Generator.UseAvailableMemory then
                   s:=s+ 'available'
              else s:=s+IntToStr(Positions.MemoryByteSize div ONE_MEBI);
              end;
           if Generator.AdHocTask>0 then
              s:=s+' -adhoc '+IntToStr(Generator.AdHocTask);
           Writeln(CommandFile,s);
           CloseFile(CommandFile);
         until SegmentIndex<=0;

       until PlayerPositionIndex__<=0;
       end;
  end; {GenerateCommandFiles}

  function  CalculateFirstAndLastOrimazeBitBoardToEnumerate:Boolean;
  var Index,Scale:Integer;
      BitBoardCount:UInt64;
      First,Last:TOrimazeBitBoard;
  begin
    with Generator do begin
       Scale:=0;
       Index:=Game.OrimazeBoardSize;
       while PowersOfTwo[Index]<=0 do begin {scaling isn't in use, and maybe not fully implemented. the program limits the Orimaze puzzle size so the bit board doesn't overflow a signed 64-bit integer.}
         Dec(Index);
         Inc(Scale);
         end;
       BitBoardCount:=PowersOfTwo[Index];

       //FirstOrimazeBitBoard:=TOrimazeBitBoard(0); {begin with a board where all bits are set to 0}
       //LastOrimazeBitBoard:=TOrimazeBitBoard(Int64(-1)) and Pred(BitBoardCount); {one bit for each square on the board, possibly with the exception of scale factor high bits}
       LastOrimazeBitBoard:=LastOrimazeBitBoard and Pred(BitBoardCount); {one bit for each square on the board, possibly with the exception of scale factor high bits}

       First:=FirstOrimazeBitBoard;
       Last :=LastOrimazeBitBoard;

       if SegmentIndex>=0 then begin {generate boards in segment 'segment index' of 'segment count' segments}
          if BitBoardCount>=SegmentCount then begin {'True': there are enough bit boards to split the enumeration}
             SegmentSize:=BitBoardCount div SegmentCount;
             First:=TOrimazeBitBoard(UInt64(SegmentIndex)*SegmentSize);
             if SegmentIndex<Pred(SegmentCount) then {otherwise, the last segment calculates the rest of the boards}
                Last:=TOrimazeBitBoard(Pred(UInt64(First)+SegmentSize));
             end
          else {there are not enough bit boards to split the computation. let the last segment calculate them all.}
             if SegmentIndex<Pred(SegmentCount) then begin {'True': this is not the last segment. don't do any calculations.}
                First:=TOrimazeBitBoard(0);
                Last:=TOrimazeBitBoard(Int64(-1));
                Scale:=-1;
                end;
          end;

       while Scale>0 do begin {add bits which weren't included in the calculation of the 'from' and 'to' bit boards}
         First:=TOrimazeBitBoard( UInt64(First) shl 1);
         Last :=TOrimazeBitBoard((UInt64(Last ) shl 1) or 1);
         Dec(Scale);
         end;

      {the user may have specified a [Start, End] range. take the intersection.
       this may not work in the general case in Delphi 4, which doesn't have
       unsigned 64-bit integers, but the program limits the Orimaze puzzle
       size so the bit board doesn't overflow a signed 64-bit integer.}
      if UInt64(First)>UInt64(FirstOrimazeBitBoard) then
         FirstOrimazeBitBoard:=First;
      if UInt64(Last)<UInt64(LastOrimazeBitBoard) then
         LastOrimazeBitBoard:=Last;

       SegmentSize:=Succ(LastOrimazeBitBoard-FirstOrimazeBitBoard); {total number of boards to enumerate. may cause a numeric overflow.}

       Result:=(Game.OrimazeBoardSize>0) and ((SegmentSize>0) or (Scale<0));
       if (not Result) and (SegmentSize<=0) and (Scale>=0) then begin
          Terminate(TEXT_SEGMENT_TOO_LARGE);
          WriteHistogramToLogFile(Generator.SokobanStatusPointer^.StatusText);
          end;
       end;
  end; {CalculateFirstAndLastOrimazeBitBoardToEnumerate}

  function  InitializeVisitedOrimazePuzzles:Boolean;
  const MEMORY_RESERVE_BYTE_SIZE=64*ONE_MEBI;
  var Index:Integer;
      AvailableMemoryByteSize,ByteSize:TSize;
  begin
    with Positions.VisitedOrimazePuzzles do begin
      Count:=0;
      ClusterCount:=0;
      BitVectorByteSize:=0;
      MemoryByteSize:=0;

      for Index:=Low(Positions) to High(Positions) do {free the existing "Visited?" bit vectors, if any}
          if Assigned(Positions[Index]) then begin
             FreeMem(Positions[Index]);
             Positions[Index]:=nil;
             end;

      ByteSize:=Generator.SegmentSize div BITS_PER_BYTE;
      if (Generator.SegmentSize mod BITS_PER_BYTE)<>0 then
         Inc(ByteSize);

      if ByteSize>0 then begin {'True': no numeric overflow}
         BitVectorByteSize:=ByteSize;
         {calculate a ball park estimate of the available memory size.}
         {memory is allocated without a "try..except" safety net because it requires linking "SysUtils", adding so much extra baggage to the exe-file that it doesn't seem worth it for this relatively small console mode program.}
         AvailableMemoryByteSize:=GetAvailableUserMemoryByteSize;
         Dec(AvailableMemoryByteSize,
             TSize(MEMORY_RESERVE_BYTE_SIZE)+
             {static memory}
             (SizeOf(Game)+SizeOf(Legend)+SizeOf(LogFile)+SizeOf(Positions)+SizeOf(Generator))+
             {dynamic memory for BFS transposition table, i.e., the hash table for searching for a longest outgoing acyclic optimal path from a given Orimaze puzzle start position}
             YASGenOrimaze.Positions.MemoryByteSize+
             {subtract 75% of the size of one bit vector, hoping this helps to avoid a memory overflow when the last bit vector is allocated.}
             (3*(BitVectorByteSize div 4)));

         for Index:=Pred(PlayerStartPositionCount) downto 0 do {'downto': puzzle start positions with higher player positions have precedence. they can accumulate more "hits" when puzzles with lower player start positions are investigated.}
             if (BitVectorByteSize<=AvailableMemoryByteSize)
                and
                ((Generator.PlayerPositionIndex<0)                 {'<': generate boards for all player positions}
                 or
                 (Generator.PlayerPositionIndex=Index)) then begin {'=': generate boards for this player position index only}
                GetMem(    Positions[PlayerStartPositionIndexToSquare[Index]],BitVectorByteSize);
                ZeroMemory(Positions[PlayerStartPositionIndexToSquare[Index]],BitVectorByteSize);
                Dec(AvailableMemoryByteSize                                  ,BitVectorByteSize);
                Inc(MemoryByteSize                                           ,BitVectorByteSize);
                end;
         end;
      Result:=True;
      end;
  end; {InitializeVisitedOrimazePuzzles}

begin {EnumerateOrimazeBoardsOfDimensionsNxM}
  Generator.StartTimeMS:=GetTimeMS;

  OriginalFirstOrimazeBitBoard:=Generator.FirstOrimazeBitBoard;
  OriginalLastOrimazeBitBoard:=Generator.LastOrimazeBitBoard;
  Game.History.Count:=0;
  Generator.BestOrimazePushCount:=-1;
  Generator.BestPuzzlesCount:=0;
  ZeroMemory(Addr(Generator.Statistics),SizeOf(Generator.Statistics));
  Positions.MaxPositionCount:=0;

  {map player start position indices to square numbers and vice versa}
  for Index:=0 to Pred(Game.OrimazeBoardSize) do
      PlayerSquareToStartPositionIndex[Index]:=-MAX_ORIMAZE_BOARD_SIZE-1; {none, i.e., the square isn't one of the start position squares used by the enumerator. must be smaller than '-MAX_ORIMAZE_PLAYER_START_POSITIONS'.}
  PlayerStartPositionCount:=OrimazePlayerStartPositionCount(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight);
  Index:=0;
  for Row:=0 to     Pred(Succ(Game.OrimazeBoardHeight) div 2) do
      for Col:=0 to Pred(Succ(Game.OrimazeBoardWidth ) div 2) do
          if ((Game.OrimazeBoardWidth<>Game.OrimazeBoardHeight)
              or {use the symmetry of a nxn board to reduce the number of player positions}
              (Col<=Row)) then begin
              Square:=ColRowToOrimazeSquare(Col,Row);
              PlayerStartPositionIndexToSquare[Index] :=Square; {map index  -> square}
              PlayerSquareToStartPositionIndex[Square]:=Index;  {map square -> index}
              Inc(Index);
              end;
  Result:=(Index=PlayerStartPositionCount); {sanity check}

  Result:=Result and
          CalculateFirstAndLastOrimazeBitBoardToEnumerate and
          (Generator.GenerateCommandFiles
           or
           InitializeVisitedOrimazePuzzles);

  if Result and (not Generator.GenerateCommandFiles) then begin
     Writeln;
     Writeln(GeneratorTaskCaption('',Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,Generator.PlayerPositionIndex,Generator.SegmentIndex,Generator.SegmentCount,Generator.AdHocTask));
     Writeln;
     if Positions.VisitedOrimazePuzzles.MemoryByteSize>0 then with Positions.VisitedOrimazePuzzles do begin
        s:=TEXT_VISITED_ORIMAZE_PUZZLES_TABLE+COLON+SPACE+IntToStr((MemoryByteSize+(ONE_MEBI-1)) div ONE_MEBI)+SPACE+TEXT_MEBI_BYTES+PERIOD+
           SPACE+TEXT_PLAYER_POSITIONS+COLON+SPACE+IntToStr(MemoryByteSize div BitVectorByteSize)+SPACE+TEXT_OF+SPACE+IntToStr(PlayerStartPositionCount);
        Writeln(s);
        end;
     Result:=WriteOrimazeHeaderToLogFile(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,Generator.PlayerPositionIndex,Generator.SegmentIndex,Generator.SegmentCount,Generator.AdHocTask);
     end;

  if Generator.RandomState.RandomNumber<0 then begin {'True': fixed enumeration order}
     {for each player starting position, discarding rotations and reflections}
     for Index:=0 to Pred(PlayerStartPositionCount) do
         if Result
            and
            (not Generator.Terminated)
            and
            ((Generator.PlayerPositionIndex<0) {'<0': generate boards for all player positions}
             or
             (Generator.PlayerPositionIndex=Index)
            ) then begin
            if   not Generator.GenerateCommandFiles then
                 Result:=EnumerateBoardsForPlayerPosition(PlayerStartPositionIndexToSquare[Index],OrimazeBoardCount)
            else Result:=GenerateCommandFiles(Index);
            end;
     end
  else with Game do begin {generate boards at random}
     while Result and (not Generator.Terminated) do begin
       for Index:=0 to 1 do {Orimaze board with horizontal and vertical tiles, but without the player}
           PUInt32(Cardinal(Addr(OrimazeBitBoard))+Cardinal(Index*SizeOf(UInt32)))^:=
             (MinimalStandardRandomNumberGenerator(Generator.RandomState.RandomNumber) shl (BITS_PER_BYTE*SizeOf(UInt16))) or
             (MinimalStandardRandomNumberGenerator(Generator.RandomState.RandomNumber) and High(UInt16));
       OrimazeBitBoard:=OrimazeBitBoard and Pred(Generator.PowersOfTwo[Game.OrimazeBoardSize]); {clear surplus bits. the Orimaze board consists of one bit for each square.}

       if   Generator.PlayerPositionIndex<0 then {'True': random Orimaze player start position}
            PlayerSquare:=PlayerStartPositionIndexToSquare[Random(PlayerStartPositionCount,Generator.RandomState)]
       else PlayerSquare:=PlayerStartPositionIndexToSquare[Generator.PlayerPositionIndex]; {fixed Orimaze player start position}
       OrimazeBitBoard:=OrimazeBitBoard and (not Generator.PowersOfTwo[PlayerSquare]); {ensure that the player square bit is cleared}

       Inc(Generator.Statistics.EnumeratedBoardsCount);
       Result:=BreadthFirstSearch(PlayerSquare,OrimazeBitBoard,True);
       if Result then
          Inc(Generator.Statistics.GeneratedPuzzlesCount);
       end;
     end;

  Generator.Statistics.TimeMS:=CalculateElapsedTimeMS(Generator.StartTimeMS,GetTimeMS);

  if Result then with Generator do begin
     if BestOrimazePushCount>=0 then begin
        OrimazeBitBoardToOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,BestOrimazePlayerPosBegin,BestOrimazeBoardBegin,OrimazeBoard);
        Writeln;
        ShowOrimazeBoard(Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,OrimazeBoard);
        Writeln;
        Writeln(TEXT_PLAYER+SPACE+TEXT_MOVES[True],COLON,SPACE,BestOrimazePushCount);
        for Index:=1 to Game.History.Count do with Game.History do
            Write(DIRECTION_TO_CHAR[Moves[Index].Direction]);
        Writeln;
        end;
     Writeln;

     if not GenerateCommandFiles then begin
        WriteHistogramToLogFile(Generator.SokobanStatusPointer^.StatusText);
        WriteStatisticsToLogFile;
        BestOrimazePuzzleLengths[Game.OrimazeBoardWidth,Game.OrimazeBoardHeight]:=BestOrimazePushCount;
        end;
     end;

  Generator.FirstOrimazeBitBoard:=OriginalFirstOrimazeBitBoard; {restore the original value. it may be a user-defined value.}
  Generator.LastOrimazeBitBoard :=OriginalLastOrimazeBitBoard;  {restore the original value. it may be a user-defined value.}
end; {EnumerateOrimazeBoardsOfDimensionsNxM}

function  EnumerateAllOrimazeBoardsUpToSpecifiedDimensions(FromCols__,FromRows__,ToCols__,ToRows__:Integer):Boolean;
var Cols,Rows,DepthLimit,PushCountLimit:Integer;

  procedure DisplayBestOrimazePuzzleLengths;
  var Width,Height:Integer;
  begin
    //Writeln;
    Writeln(TEXT_STATISTICS);
    Write(TEXT_HEIGHT:10,TEXT_WIDTH:10,COLON);
    for Width:=1 to MAX_ORIMAZE_BOARD_WIDTH do
        Write(Width:6);
    Writeln;
    for Height:=1 to MAX_ORIMAZE_BOARD_HEIGHT do begin
        Write(Height:9,COLON,'':11);
        for Width:=1 to MAX_ORIMAZE_BOARD_WIDTH do with Generator do begin
            if   (BestOrimazePuzzleLengths[Width,Height]>0) or
                 ((Width=1) and (Height=1)) then
                 Write(Max(0,BestOrimazePuzzleLengths[Width,Height]):6)
            else Write('':6);
            end;
        Writeln;
        end;
  end;

begin {EnumerateAllOrimazeBoardsUpToSpecifiedDimensions}
  Result:=True;
  DepthLimit:=Generator.SearchLimits.DepthLimit;
  PushCountLimit:=Generator.SearchLimits.PushCountLimit;

  for Rows:=FromRows__ to ToRows__ do
      for Cols:=FromCols__ to ToCols__ do
          if (not Generator.Terminated) and
             (not ((Cols<Rows) and (Rows>=FromCols__) and (Rows<=ToCols__))) then begin {avoid duplicates e.g., after generating 3x2, avoid generating 2x3 too}
             Generator.SearchLimits.DepthLimit:=DepthLimit;
             Generator.SearchLimits.PushCountLimit:=PushCountLimit;
             Result:=Result and
                     InitializeGame(Cols,Rows) and
                     EnumerateOrimazeBoardsOfDimensionsNxM and
                     (Generator.GenerateCommandFiles
                      or
                      (WritelnToLogFile('')
                       and
                       WritelnToLogFile('')
                      ));
             end;

  if Result and (not Generator.GenerateCommandFiles) then
     DisplayBestOrimazePuzzleLengths;
end; {EnumerateAllOrimazeBoardsUpToSpecifiedDimensions}

{-----------------------------------------------------------------------------}

{Generator Toplevel}

procedure Finalize;
begin
  {all global strings must be cleared;}
  {otherwise, the runtime may hold on to address-space addresses even though}
  {the memory itself is released;}
  Game.Title:=''; Game.OriginalSolution:='';

  CloseLogFile(True); LogFile.FileName:=''; {close the file and ensure that the string is cleared}
  TTFinalize;
  Generator.SokobanStatus.StatusText:='';
end;

function  Initialize(OrimazeBoardWidth__,
                     OrimazeBoardHeight__,
                     PushCountLimit__,
                     DepthLimit__,
                     RandomSeed__,
                     PlayerPositionIndex__,
                     SegmentIndex__,SegmentCount__,
                     AdhocTask__:Integer;
                     FirstOrimazeBitBoard__,
                     LastOrimazeBitBoard__ : TOrimazeBitBoard;
                     MemoryByteSize__:TSize;
                     LogFileEnabled__,
                     GenerateAllOrimazePuzzlesUpToSpecifiedDimensions__,
                     GenerateCommandFiles__,
                     IsMemorySpecified__,
                     UseAvailableMemory__:Boolean;
                     TimeLimitMS__:TTimeMS;
                     const AdditionalBoxesBetweenGridPoints__:TBoxNumberSet;
                     SokobanCallBackFunction__:TSokobanCallBackFunction;
                     SokobanStatusPointer__:PSokobanStatus
                    ):Boolean;
var Index:Integer;
begin
  {ensure that strings are properly released before 'FillChar' clears the data-structures;}
  {after 'FillChar', it's not strictly necessary to initialize the strings to '' because}
  {Delphi handles nil-pointers as empty strings, but the initialization is performed here anyway for completeness}
  Game.Title:=''; Game.OriginalSolution:=''; FillChar(Game     ,SizeOf(Game     ),0); Game.Title:=''; Game.OriginalSolution:='';
  LogFile.FileName:='';                      FillChar(LogFile  ,SizeOf(LogFile  ),0); LogFile.FileName:='';
                                             FillChar(Positions,SizeOf(Positions),0);

  Generator.SokobanStatus.StatusText:='';    FillChar(Generator,SizeOf(Generator),0); Generator.SokobanStatus.StatusText:='';

  InitializeLegend(True);

  Generator.SearchLimits.PushCountLimit:=PushCountLimit__;
  Generator.SearchLimits.DepthLimit:=DepthLimit__;
  Generator.SearchLimits.TimeLimitMS:=TimeLimitMS__;
  Generator.GenerateAllOrimazePuzzlesUpToSpecifiedDimensions:=GenerateAllOrimazePuzzlesUpToSpecifiedDimensions__;
  Generator.GenerateCommandFiles:=GenerateCommandFiles__;
  Generator.IsMemorySpecified:=IsMemorySpecified__;
  Generator.UseAvailableMemory:=UseAvailableMemory__;
  Generator.RandomState.RandomNumber:=RandomSeed__; {don't initialize by calling InitializeRandomState(). it discards negative values.}
  Generator.PlayerPositionIndex:=PlayerPositionIndex__;
  Generator.AdditionalBoxesBetweenGridPoints:=AdditionalBoxesBetweenGridPoints__;
  Generator.SegmentIndex:=SegmentIndex__;
  Generator.SegmentCount:=SegmentCount__;
  Generator.AdHocTask:=AdHocTask__;
  Generator.FirstOrimazeBitBoard:=FirstOrimazeBitBoard__;
  Generator.LastOrimazeBitBoard:=LastOrimazeBitBoard__;
  Generator.SokobanCallBackFunction:=SokobanCallBackFunction__;
  if   (SokobanStatusPointer__<>nil) and
       (SokobanStatusPointer__^.Size>=SizeOf(SokobanStatusPointer__^)) then {'>=': otherwise, the program cannot update the fields it expects to find in the record}
       Generator.SokobanStatusPointer:=SokobanStatusPointer__
  else Generator.SokobanStatusPointer:=Addr(Generator.SokobanStatus);
  {initialize the locally defined solver status record (it's only used if a caller doesn't provide its own record)}
  Generator.SokobanStatus.Size:=SizeOf(Generator.SokobanStatus);
  SetSokobanStatusText('');
  Generator.Terminated:=False;
  LogFile.Enabled:=LogFileEnabled__;

  for Index:=Low(Generator.PowersOfTwo) to High(Generator.PowersOfTwo) do
      Generator.PowersOfTwo[Index]:=UInt64(1) shl Index;

  //for Index:=Low(Generator.PowersOfTwo) to High(Generator.PowersOfTwo) do
  //    Writeln(Index:5, Generator.PowersOfTwo[Index]:40);
  //Readln;

  Result:=TTInitialize(MemoryByteSize__)
          and
          InitializeGame(OrimazeBoardWidth__,OrimazeBoardHeight__)
          and
          ((not LogFile.Enabled) or CreateLogFile(MakeLogFileName(TEXT_APPLICATION_TITLE,Game.OrimazeBoardWidth,Game.OrimazeBoardHeight,Generator.PlayerPositionIndex,Generator.SegmentIndex,Generator.SegmentCount,Generator.AdHocTask)));
end;

function  Run:Boolean;
begin
  if   Generator.GenerateAllOrimazePuzzlesUpToSpecifiedDimensions then
       Result:=EnumerateAllOrimazeBoardsUpToSpecifiedDimensions(1,1,Game.OrimazeBoardWidth,Game.OrimazeBoardHeight)
  else Result:=EnumerateOrimazeBoardsOfDimensionsNxM;
end;

{-----------------------------------------------------------------------------}

  {Application Toplevel}

  function  InitializeApplication:Boolean;
  var AdditionalBoxesBetweenGridPoints:TBoxNumberSet;
  begin
    ShowTitle;

    SAT(0); // for debugging convenience, ensure that the abbreviated 'SquareToColRowAsText()' function is included in the compiled program

    {$WARNINGS OFF} {warning: Comparison always evaluates to True}
      Result:=(MAX_HISTORY_BOX_PUSHES                            <= High(Positions.Positions^[0].PushCount)) and
              (MAX_HISTORY_BOX_PUSHES                            <  High(Game.History.Count)) and
              (MAX_HISTORY_PLAYER_MOVES                          <  DEAD_END_SCORE) and {the highest score values are reserved for the constants DEADLOCK_SCORE and DEAD_END_SCORE}
              (MAX_BOX_COUNT*DIRECTION_COUNT+1                   <  High(Positions.Positions^[0].SuccessorCount) div 2) and {'+1': 'SuccessorCount' is increased by 1 during node-expansion to protect the position against recycling}
              (MAX_BOARD_HEIGHT                                  <= High(Int8)) and {board height, board width, and straight-line distances may sometimes be stored in signed 8-bit fields for efficiency}
              (MAX_BOARD_WIDTH                                   <= High(Int8)) and
              (MAX_BOARD_SIZE                                    <= PLAYER_POSITION_MASK) and {the high-bit in 'TPosition.PlayerPos' is reserved for the move-axis}
              (MAX_BOARD_SIZE                                    <= High(Positions.Positions^[0].PlayerPos) div 2) and {deadlock-sets stored in the normal transposition table have player position stored as = player position +'MAX_BOARD_SIZE'}
              (MAX_BOARD_SIZE*DIRECTION_COUNT                    <  High(TTimeStamp)) and {after clearing (zero-filling) timestamps, it must be possible to use 'CalculatePlayersReachableSquares()' to visit all board squares without timestamp wrap around}
              (MAX_BOX_COUNT                                     <= High(Positions.Positions^[0].Move.BoxNo)) and
              (MAX_BOX_COUNT                                     <= MAX_HISTORY_BOX_PUSHES) and {the 'Solver.SearchStates' vector must have room for a depth-first search for each box}
              (High(Positions.Positions^[0].PushCount)           >= MAX_BOX_COUNT) and
              (High(Positions.Positions^[0].BestForgottenScore)  >= High(Positions.Positions^[0].PushCount)) and
              (High(Positions.HashBucketMask)                    >= High(Positions.HashBucketCount)) and
              (DIRECTION_COUNT                                   <= 1 shl DIRECTION_BIT_COUNT) and
              (DIRECTION_BIT_COUNT                               <= 3) and {very dirty: tag-bits are stored in 'TPosition.Move.Direction', e.g., 'POSITION_OPEN_TAG' and 'POSITION_PATH_TAG'}
              (MAX_BOX_COUNT                                     <= High(Game.Board[0,0]) shr GOAL_BIT_SHIFT_COUNT) and {goal-numbers or goal-packing-order-set-numbers are stored in the upper bits for each square}
              (MAX_ORIMAZE_BOARD_SIZE                            <= MAX_BOARD_WIDTH) and {for convenience, the Orimaze board is sometimes mapped to a single row on the Sokoban board}
              (MAX_ORIMAZE_BOARD_SIZE                            <= High(Int8)) and {otherwise some precalculated tables requires datatype changes}
              (MAX_ORIMAZE_BOARD_SIZE                            <= MAX_BOX_COUNT) and
              ((MAX_ORIMAZE_BOARD_SIZE)                          <= BITS_PER_BYTE*SizeOf(TOrimazeBitBoard)) and
              (MAX_ORIMAZE_BOARD_SIZE                            <= BITS_PER_BYTE*SizeOf(Int64)) and
              (SizeOf(UInt)                                      =  SizeOf(Cardinal)) and
              (SizeOf(Integer)                                   =  SizeOf(Cardinal)) and
              (SizeOf(Int32)                                     =  4) and {sanity check}
              (SizeOf(UInt32)                                    =  4) and {sanity check}
              (SizeOf(THashValue)                                =  (2*SizeOf(Int32))) and {hash values are composed of two Int32 integers}
              (SizeOf(TOrimazeBitBoard)                          =  SizeOf(THashValue)) and {Orimaze bit boards are used directly as hash values}
              (SizeOf(TOrimazeBitBoard)                          =  (2*SizeOf(UInt32))) and {random Orimaze bit boards are created by using two UInt32 random numbers}
              (SizeOf(Positions.Positions^[0].Move)              =  SizeOf(Integer)) and {the move record must have a size so the record is aligned with the other fields in the 'TPosition' record}
              (Ord(aVertical)                                    =  0) and {the ordinal value of the vertical   axis is fixed}
              (Ord(aHorizontal)                                  =  1) and {the ordinal value of the horizontal axis is fixed}
              (SizeOf(NativeUInt)                                =  SizeOf(Pointer)) and
              (SizeOf(NativeInt)                                 =  SizeOf(Pointer))
              ;
    {$WARNINGS ON}

    if Result then begin
       Result:=(SizeOf(TPosition) mod SizeOf(Integer))=0;
       if not Result then
          Writeln(TEXT_APPLICATION_TITLE+': Internal error: Positions are not properly aligned (Size=',SizeOf(TPosition),').');
       if Result then begin
          end;
       end
    else begin
       Writeln(TEXT_APPLICATION_TITLE+': Internal error: Basic constraints for the program is violated.');
       end;

    if Result then
       Result:=GetCommandLineParameters(Game.OrimazeBoardWidth,
                                        Game.OrimazeBoardHeight,
                                        Generator.SearchLimits.DepthLimit,
                                        Generator.SearchLimits.PushCountLimit,
                                        Generator.RandomState.RandomNumber,
                                        Generator.PlayerPositionIndex,
                                        Generator.SegmentIndex,
                                        Generator.SegmentCount,
                                        Generator.AdHocTask,
                                        Generator.FirstOrimazeBitBoard,
                                        Generator.LastOrimazeBitBoard,
                                        Positions.MemoryByteSize,
                                        Generator.GenerateAllOrimazePuzzlesUpToSpecifiedDimensions,
                                        Generator.GenerateCommandFiles,
                                        Generator.IsMemorySpecified,
                                        Generator.UseAvailableMemory,
                                        Generator.SearchLimits.TimeLimitMS,
                                        LogFile.Enabled,
                                        AdditionalBoxesBetweenGridPoints
                                       );
    if Result then begin
       Result:=Initialize(              Game.OrimazeBoardWidth,
                                        Game.OrimazeBoardHeight,
                                        Generator.SearchLimits.PushCountLimit,
                                        Generator.SearchLimits.DepthLimit,
                                        Generator.RandomState.RandomNumber,
                                        Generator.PlayerPositionIndex,
                                        Generator.SegmentIndex,
                                        Generator.SegmentCount,
                                        Generator.AdHocTask,
                                        Generator.FirstOrimazeBitBoard,
                                        Generator.LastOrimazeBitBoard,
                                        Positions.MemoryByteSize,
                                        LogFile.Enabled,
                                        Generator.GenerateAllOrimazePuzzlesUpToSpecifiedDimensions,
                                        Generator.GenerateCommandFiles,
                                        Generator.IsMemorySpecified,
                                        Generator.UseAvailableMemory,
                                        Generator.SearchLimits.TimeLimitMS,
                                        AdditionalBoxesBetweenGridPoints,
                                        nil, //SokobanCallbackFunction,
                                        nil);
       end;
    if Result then begin
       Writeln('Static  memory: ',(SizeOf(Game)+SizeOf(Legend)+
                                   +SizeOf(LogFile)+
                                   SizeOf(Positions)+
                                   SizeOf(Generator)
                                   +(ONE_MEBI div 2)
                                 ) div ONE_MEBI:6,' MiB');
       Write('Dynamic memory: ',(Positions.MemoryByteSize+{Cardinal(Game.OrimazeVisitedStartPositionsCapacity)+}(ONE_MEBI div 2)) div ONE_MEBI:6,' MiB');
       Write('   Position capacity: ',( Cardinal( Positions.HighWaterMark ) - Cardinal( Positions.Positions ) ) div SizeOf( TPosition ),
             '  Position size: '     ,SizeOf(TPosition));
       Writeln;

       if (LogFile.FileName<>'') and (not Generator.GenerateCommandFiles) then begin
          Writeln;
          Writeln('Output file...: ',LogFile.FileName);
          Writeln;
          end;
       end
    else begin
       ShowHelp; Msg('','');
       end;
  end;

  procedure RunApplication;
  begin
    Run;
  end;

  procedure FinalizeApplication;
  begin
    Finalize;
    if   not Generator.GenerateCommandFiles then begin
         Writeln('Generator time: ',IntToStr_(Generator.Statistics.TimeMS),' milliseconds');
         //Msg('Done','');
         end
    else Msg('Done','');
  end;

begin {main}
  if InitializeApplication then
     RunApplication;
  FinalizeApplication;
end.


