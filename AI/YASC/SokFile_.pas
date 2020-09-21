unit SokFile_;
{$DEFINE SokobanYASC} {only when this module is used as part of 'Sokoban YASC'}
{$DEFINE SokobanYASC_ProductionVersion} {production mode = safe mode}
{///$DEFINE SokHtml}
{
 File           : SokFile_.pas
 Function       : A Sokoban file-reader/writer
 Copyright      : Public Domain (this applies to this file, *not* the entire program)
 WARRANTY       : ABSOLUTELY NONE - USE THIS CODE AT YOUR OWN RISK.

 Original Author: Brian Damgaard, Denmark.
 E-mail         : BrianDamgaard@jubii.dk
 Date           : 2001-2019

 Modified by    : ...
 E-mail         : ...
 Date           : ...

 This module contains a public domain version of a Sokoban
 file-reader/writer supporting the "Common Sokoban File Format".

 This format is backwards compatible with most xsb-style files, but
 offers all the features needed for easy exchange of levels between
 different Sokoban programs:

 . Multiple levels in one file
 . Multiple savegames/snapshots/bookmarks for each level
 . Optional notes for each level and snapshot
 . Reverse mode games
 . Macros, e.g., all levels can share the same copyright text

 Briefly, the file-format is like this:

  File
    File Header
      File Format Specification
      Notes
      Macros (deprecated)
    Level 1
       Name
       Board
       Notes
       Snapshot 1
         Name
         Moves
         Notes
       Snapshot 2
       ... (more snapshots)
    Level 2
    ... (more levels)

 The parser works according to this structure, and no tags are
 required. Thus, files can be written and edited by hand without
 memorizing a long list of special keywords, and different programs
 do not have to agree on a fixed set of keywords.

 A program can store its own program-specific information by adding
 lines with key/value pairs to the notes, such as "Author: NN". That
 way, the file will still be handled correctly by other implementations.

 All Sokoban programs are encouraged to support this format, and
 since this code is in the public domain, it should be easy to do so.

 Sokoban programs are also encouraged to use '.sok' as the file
 extension name for files in this format. An example: "MyLevels.sok".

 Reading and writing levels requires a substantial part of the
 machinery in a Sokoban program. Thus, the code provides a good
 starting point for writing a complete program.

 The accompanying demo-program is a full-fledged Sokoban program.
 It has a primitive console-interface and is, of course, not meant
 for really playing the game. Its sole purpose is to demonstrate
 and test the underlying file-reader/writer and the game engine.
 It is, however, easy to add a proper graphical interface to the
 program.

 Please note, that this module is written with easy porting to other
 languages in mind, thus, it is not an example of good Delphi
 programming style. Most notably, it doesn't use private/public class
 members, it doesn't use properties, and it doesn't use any standard
 Delphi-components such as string lists.
}

{
 --------------------------------------------------------------------
 The size of this module
 --------------------------------------------------------------------
 Stripping declarations and documentation, this module contains about
 4000-5000 lines of real code.

 If you don't like to include a module of this size in your
 Sokoban program, please bear the following in mind:

 Even the most stripped down Sokoban program must include a
 substantial part of the functionality provided by this module:

 . Load levels from disk
 . Save levels to disk
 . Parse boards
 . Parse snapshots/savegames/bookmarks

 This can hardly be coded in fewer lines, thus, you have to write
 most of this code anyway.

 Internally, all data are stored in single-linked lists.
 This is not always the most efficient datatype, but it is flexible
 and easy to implement, and since this module must be easy to port
 to other languages, the single-linked lists will have to do.

 --------------------------------------------------------------------
}

{
 --------------------------------------------------------------------
 Crash Course in Delphi-Pascal for C Language Family Programmers
 --------------------------------------------------------------------

 1. '=' denotes a comparison, not an assignment
    The Pascal language uses ':=' for assigment, and '=' for
    comparisons.
    Example: IF a = b THEN c := a;

 2. No case sensitivity
    In Pascal the identifiers 'myvar', 'MyVar', and 'Myvar' all denote
    the same variable.

 3. Curly braces are used for comments, not for blocks
    In Pascal, blocks are enclosed in 'BEGIN' ... 'END'.

 4. Case statements don't "fall through"
    In a case block, each group of statements is activated separately.

 5. In comparisons, "not equal" is written as '<>'.
    Example: IF a <> 0 THEN ...;

 6. Var arguments
    Argument-passing by reference. It works like passing a pointer to
    a variable.
    Example: PROCEDURE MatrixMul(a,b: TMatrix; VAR c: TMatrix);

 7. Const arguments
    Similar to 'var' arguments, but the procedure can't change the
    passed argument.
    Example: FUNCTION StrPos(CONST s1,s2: String): Integer;
    Note: The 'MatrixMul' example above should be coded more efficiently
    like this: PROCEDURE MatrixMul(CONST a,b: TMatrix; VAR c: TMatrix);

 8. Object destruction using Object.Free
    All objects descend from 'TObject' which implements the method
    'Free'. This method calls 'Object.Destroy' but 'Object.Free' allows
    'Object' to be a nil-pointer, thus it is more flexible.
    In other languages, you probably have to substitute 'Object.Free'
    with 'if Object<>nil then Object.Destroy'

 9. Strings
    Characters in a string are indexed [1..Length], not [0..Length-1].
    In this module, set 'STRING_BASE' to match your language.

    In Delphi-Pascal, strings are allocated/deallocated automatically
    on the heap. They are reference-counted, so an assignment like
    'StrA := StrB' is very efficient.

    In Delphi-Pascal, a string-variable is identical to a PChar-pointer
    to a null-terminated character-vector, so typecasting is allowed.
    Example: p:=PChar(Str); Str:=String(p);
}

{
 --------------------------------------------------------------------
 Implementing in languages without object-oriented programming (C)
 --------------------------------------------------------------------

 It isn't that hard to rewrite this module to languages without
 support for object-oriented programming, such as C.

 in 'SokUtil_', make 'TNode' a structure with 3 slots:
 'Next', 'Text', and 'ObjectType'.

 Make all other classes to structures, with 'TNode' added as the
 first slot.

 Rewrite all methods to take a pointer to 'This' as first argument,
 i.e., TNode* TListPush(TList* This__; TNode* Node__)

 Using the 'This' pointer it is pretty straightforward to rewrite
 the internal code of each method.
}

{
 --------------------------------------------------------------------
 SokoFile limitations
 --------------------------------------------------------------------
 There is one thing left unhandled by 'SokoFile': It doesn't check
 snapshots for legal moves. It has the methods 'TextLinesToMoves'
 and 'MovesToTextLines' to convert snapshots from text to internal
 form and back again, but checking for legal moves is left to the
 game engine, 'TSokoGame', in the accompanying module 'SokGame_'.

 'TSokoGame' contains all the machinery required by a Sokoban program,
 such as performing moves, collecting best solutions, etc., and it is
 more convenient to check for legal moves here.

 This means, if you plug 'SokFile_' into an existing implementation
 to support the file-format, and you don't use 'TSokGame' as the
 kernel of the program, you will have to check the snapshots for
 legal moves yourself.
 --------------------------------------------------------------------
}

interface

uses Windows,Clipbrd,Forms,
     SokUtil_;

{
 --------------------------------------------------------------------
 Low-level File-manager
 --------------------------------------------------------------------
 On the lowest level, a file is handled as linked lists of raw textlines
 with the same structure as the external textfile:

 File
   File Header
     File Format Description
     Notes
     Macros
     Game Type
   Level 1
      Name
      Board (raw text)
      Notes
      Snapshot 1
        Name
        Moves (raw text)
        Notes
      Snapshot 2
      ... (more snapshots)
   Level 2
   ... (more levels)

 When a file is loaded it is parsed according to this structure,
 but otherwise there is no error checking. For a large collection it
 could be time-consuming to validate all boards and snapshots
 (saved games and solutions), so validation is better deferred until
 the program really opens a level.

 The method 'TSokoFile.LoadFromFile' loads a file, and the
 symmetrical method TSokoFile.SaveToFile' saves a file to disk.
 The method 'TSokoFile.LoadFromTextLines' is the kernel of this module.
 --------------------------------------------------------------------
}
const
  {$IFDEF SokobanYASC_ProductionVersion}
    {$DEFINE SokobanYASC}
    MAX_BOARD_HEIGHT             = 50;     { standard boards, with a reasonably small memory footprint and good speed}
    MAX_BOXES                    = 1200;   { if this limit is much bigger than 1000 in Sokoban YASC, then it may be neccessary to change YASC's 'Dead_.TBoardStateBoxSet.Squares' from integers to 16-bit integers to avoid stack overflow}
    MAX_MOVES                    = 100000; { minimum: width * height * directions * 2 = (for calculating a boxpath)} {~210,000 is the upper limit for box path calculation with integer scores instead of double integer scores}
  {$ELSE}
  {$IFDEF SokobanYASC}
    MAX_BOARD_HEIGHT             = 100;    { large boards, but still with a reasonable memory footprint and speed}
    MAX_BOXES                    = 5000;
    MAX_MOVES                    = 250000;
  {$ELSE}
    MAX_BOARD_HEIGHT             = 200;    { extra large boards, with a huge memory footprint and slow speed}
    MAX_BOXES                    = 15000;  { 8000 is currently the maximum for Sokoban YASC because 18 bits are reserved for flags in 'TBoardSquare', which is a signed 32-bit integer, leaving 13 bit free for storing a box number; 2^13 = 8192}
    MAX_MOVES                    = 500000;
  {$ENDIF}
  {$ENDIF}

  MAX_BOARD_WIDTH                = MAX_BOARD_HEIGHT;
  MAX_BOARD_SIZE                 = (MAX_BOARD_WIDTH+2) * (MAX_BOARD_HEIGHT+2); {'0' left/top border; '+1': right/bottom border}

  MIN_BOARD_HEIGHT               = 3; {must be >= 3}
  MIN_BOARD_WIDTH                = 3; {must be >= 3}

  {Board notation (traditional 'xsb'-style with some extra, more readable, characters for player and boxes)}
  WALL_CH                        = '#';
  PLAYER_CH                      = '@';   PLAYER_CH1         = 'p'; PLAYER_CH2          = 'm'; {accept 'mM' (Man) as player}
  PLAYER_GOAL_CH                 = '+';   PLAYER_GOAL_CH1    = 'P'; PLAYER_GOAL_CH2     = 'M';
  BOX_CH                         = '$';   BOX_CH1            = 'b';
  BOX_GOAL_CH                    = '*';   BOX_GOAL_CH1       = 'B';
  GOAL_CH                        = '.';   GOAL_CH1           = 'o';
  FLOOR_CH                       = SPACE; FLOOR_NON_BLANK_CH1= '_'; FLOOR_NON_BLANK_CH2 = '-';
  EMAIL_QUOTE_CH                 = '>';   {ignore leading '>' characters}

  FLAG_FIXED_SQUARE              = 128;   {precondition: the flag value must be bigger than the value of the ASCII characters in use for the board squares}

  LEGAL_BOARD_CHARACTERS         = WALL_CH         +
                                   PLAYER_CH       +PLAYER_CH1         +PLAYER_CH2+
                                   PLAYER_GOAL_CH  +PLAYER_GOAL_CH1    +PLAYER_GOAL_CH2+
                                   BOX_CH          +BOX_CH1            +
                                   BOX_GOAL_Ch     +BOX_GOAL_CH1       +
                                   GOAL_CH         +GOAL_CH1           +
                                   FLOOR_CH        +FLOOR_NON_BLANK_CH1+FLOOR_NON_BLANK_CH2;

  {Moves notation}
  UP_CH                          = 'U'; {must be uppercase characters, otherwise change 'CharToDirection' and 'MoveToChar'; don't localize}
  DOWN_CH                        = 'D';
  LEFT_CH                        = 'L';
  RIGHT_CH                       = 'R';

  UP_CH1                         = 'u'; {must be lowercase characters, matching uppercase characters defined above; don't localize}
  DOWN_CH1                       = 'd';
  LEFT_CH1                       = 'l';
  RIGHT_CH1                      = 'r';

  CURRENT_MOVE_CH                = '*'; {current position in a snapshot}
  GROUP_BEGIN_CH                 = LEFT_PAREN;
  GROUP_END_CH                   = RIGHT_PAREN;
  JUMP_BEGIN_CH                  = '['; JUMP_BEGIN_CH1       = '<'; {the player can jump when playing in reverse mode}
  JUMP_END_CH                    = ']'; JUMP_END_CH1         = '>'; {'<...>': for backwards compatibility only; new files should use '[...]' for reverse mode jumps}
  {$IFDEF SokHtml}
    MOVE_PLAYER_BACK_TO_START_CH = EXCLAMATION;
  {$ENDIF}

  LEGAL_MOVE_CHARACTERS          = {spaces (and other whitespace characters) between moves are allowed, but SPACE is not listed as a legal character; instead the parser has a more lenient 'isWhiteSpace()' test}
                                   UP_CH  + DOWN_CH  + LEFT_CH  + RIGHT_CH  +   {UDLR} {the proper moves, i.e., "UDLRudlr" must come first}
                                   UP_CH1 + DOWN_CH1 + LEFT_CH1 + RIGHT_CH1 +   {udlr}
                                   GROUP_BEGIN_CH+GROUP_END_CH +                {(...) move group}
                                   CURRENT_MOVE_CH +                            {'*' current position in the game}
                                   '0123456789' +                               {run length encoding}
                                   JUMP_BEGIN_CH+JUMP_END_CH+                   {[...] notation for a jump in reverse mode}
                                   JUMP_BEGIN_CH1+JUMP_END_CH1                  {<...> notation for a jump in reverse mode (for backward compatibility only; new files should use '[...]')}
                                   {+COLON; an early version used '<:' ... ':>' for combined moves}
                                   {$IFDEF SokHtml}
                                     +MOVE_PLAYER_BACK_TO_START_CH              {'!' more player back to start position}
                                   {$ENDIF}
                                   ;

  {Board squares, internal format}
  {Each square contains a bit-set with these values;}
  {Additionally, a square with a box contains the box-number shifted 'BOARD_FLAG_COUNT' bits to the left}
  FLOOR                          = 1;      {value must not change; only squares that the player can reach if all boxes are removed from the board are considered to be true floor squares}
  GOAL                           = 2;      {value must not change}
  BOX                            = 4;      {value must not change}
  PLAYER                         = 8;      {value must not change}
  WALL                           = 16;     {value must not change}
  BOARD_PIECES                   = FLOOR + GOAL + BOX + PLAYER + WALL;
  ILLEGAL_SQUARE                 = 32;     {a box on this square can never reach a target square}
  BOX_ILLEGAL_MOVE               = 64;     {in current position, a selected box cannot or must not move to a square}
  BOX_LEGAL_MOVE                 = 128;    {in current position, a selected box can reach the square}
  BOX_START_POSITION             = 256;    {in start-position, the square contains a box}
  BOX_UNREACHABLE_FLOOR          = 512;    {no boxes can reach the floor square in normal forward play}
  EXTERIOR_WALL                  = 1024;   {the wall is an exterior wall}
  INVISIBLE_WALL                 = 2048;   {visually the square is a floor, but functionally it's a wall}
  PLAYER_LEGAL_MOVE              = 4096;   {in current position, the player can reach the square}
  PLAYER_LEGAL_MOVE_IN_START_POSITION
                                 = 8192;   {in starting position, the player can reach the square}
  PLAYER_TRY_MOVE                = 16384;  {the player is trying to make this move in the game}
  PLAYER_UNREACHABLE_FLOOR       = 32768;  {visually the square is a floor but the player cannot reach it}
  {$IFDEF SokobanYASC}
    SPECIAL_BACKGROUND_SQUARE    = 65536;  {the square has a special background, e.g., a "reverse mode" text, or a "dead square" special color}
    SQUARE_SET                   = 131072; {a set of squares, e.g., squares with boxes that are movable in current position}

    BOARD_FLAG_COUNT             = 18;     {number of bits used as flags, >= flags defined above}
  {$ELSE}
//  SPECIAL_BACKGROUND_SQUARE    = 65536;  {the square has a special background, e.g., a "reverse mode" text, or a "dead square" special color}
    SQUARE_SET                   = 65536; {a set of squares, e.g., squares with boxes that are movable in current position}

    BOARD_FLAG_COUNT             = 17;     {number of bits used as flags, >= flags defined above}
  {$ENDIF}
  {Combined flags}
  BOARD_FLAGS_MASK               = (1 shl BOARD_FLAG_COUNT)-1; {masking all board flags}
  BOARD_GAME_STATE_FLAGS_MASK    = FLOOR + GOAL + BOX + PLAYER + WALL; {flags describing the game as the user sees it}
  BOX_SET_DEADLOCK               = SQUARE_SET+BOX_ILLEGAL_MOVE;
  BOX_SET_TO_SQUARE              = SQUARE_SET+BOX_LEGAL_MOVE;
  SIMPLE_ILLEGAL_MOVES_MASK      = ILLEGAL_SQUARE+BOX_UNREACHABLE_FLOOR+INVISIBLE_WALL;

  {History moves, flags and masks}        {for space efficiency, history moves occupy only 1 byte each}
  H_FLAG_BOX                     = 128;   {the move pushes/pulls a box; must be > MAX_BOARD_WIDTH and > MAX_BOARD_HEIGHT}
  H_FLAG_BOX_CHANGE              = 64;    {the pushed/pulled box is different from the last touched box}
  H_FLAG_JUMP                    = 32;    {the move is a jump-move (reverse mode games only)}
  H_FLAG_ODD                     = 16;    {odd/even combined move number; used for separating combined moves}
  H_FLAG_UNDO                    = 8;     {the move hasn't been performed yet, i.e., an 'undone' move later than current position ('Count')}
  H_MASK_DIRECTION               = 1+2+4; {3 lowest bits for directions (8 values: 0..7) (must be lowest bits for fast masking + typecasting)}
  H_MASK_MOVE_SEPARATOR          = H_FLAG_ODD+H_FLAG_UNDO; {these bits suffice to separate combined moves}

  {Movements}
type
  TBoardAxis                     =(ColAxis,RowAxis);
  TBoardAxesSet                  = set of TBoardAxis;
const
  ALL_BOARD_AXES                 : TBoardAxesSet = [ColAxis,RowAxis];
  AXIS_TO_DIRECTION              : array[TBoardAxis] of TDirection
                                 = (Up,left); {lowest direction along the axis}
  DIRECTION_XY                   : array[TDirection,TBoardAxis] of ShortInt
                                 =   ((0,-1),(-1,0),(0,1),(1,0));
  DIRECTION_TO_AXIS              : array[TDirection] of TBoardAxis
                                 = (ColAxis,RowAxis,ColAxis,RowAxis);
  DIRECTION_TO_CHAR              : array[TDirection] of Char              {must be uppercase characters; don't localize}
                                 =   (UP_CH, LEFT_CH, DOWN_CH, RIGHT_CH); {otherwise change 'CharToDirection' and 'MoveToChar'}
  DIRECTION_TO_TEXT              : array[TDirection] of String
                                 = ('up','left','down','right'); {don't localize}
  NEXT_DIRECTION                 : array[TDirection] of TDirection {direction rotated 90 degrees counterclockwise}
                                 = (Left,Down,Right,Up);
  NUMBER_OF_DIRECTIONS           = Succ(Ord(High(TDirection))-Ord(Low(TDirection))); {number of directions}
  OPPOSITE_DIRECTION             : array[TDirection] of TDirection
                                 =   (Down,Right,Up,Left);
  T2D_DIRECTION                  : array[TBoardTransformation2D,TDirection] of TDirection
                                 = ((Up,Left,Down,Right), {t2DRotate0DegreesClockwise}
                                    (Right,Up,Left,Down), {t2DRotate90DegreesClockwise}
                                    (Down,Right,Up,Left), {t2DRotate180DegreesClockwise}
                                    (Left,Down,Right,Up), {t2DRotate270DegreesClockwise}
                                    (Up,Right,Down,Left), {t2DRotate0DegreesClockwiseFlipHorizontally}
                                    (Left,Up,Right,Down), {t2DRotate90DegreesClockwiseFlipHorizontally}
                                    (Down,Left,Up,Right), {t2DRotate180DegreesClockwiseFlipHorizontally}
                                    (Right,Down,Left,Up)  {t2DRotate270DegreesClockwiseFlipHorizontally}
                                   );

  {Puzzle types supported by the program, e.g., 'Sokoban', 'Hexoban', etc.}
type
  TPuzzleType                    = (ptSokoban);
const
  DEFAULT_PUZZLE_TYPE            = Low(TPuzzleType);

const
  DEFAULT_ADD_FILE_FORMAT_DESCRIPTION_TO_FILES
                                 = True;             {add file format description to files}

const
  {Newline tags embedded in strings}
  C_STYLE_NEWLINE                = '\n';
  SOKOBAN_NEWLINE                = BAR;

  LINE_WIDTH_MOVES               = 70;               {line-width when writing moves to a text-file}


  FILE_NAME_AND_PATH_NAME_ILLEGAL_CHARACTERS
                                 = '\/:*?"<>|';      {invalid filename characters}
  FILE_NAME_WIDE_CHARACTER_TO_ANSI_CHARACTER_CONVERSION_ERROR_CHARACTER
                                 = '?';              {in file names, when wide characters cannot be converted to ansi characters, Windows substitute the wide character with this value}

  {strip these characters from titles to avoid conflicts with path-names:}
  TITLE_ILLEGAL_FIRST_CHARACTER  = '*';              {section-name tag in inifiles for sections not containing a level}
  TITLE_ILLEGAL_CHARACTERS       =':\*?[];<>|';      { '[' and ']' are used internally to separate filenames from levelnames, and ';' are sometimes used in xsb-files as tag in title-lines}

  {Keys, don't localize}
  {Please note, that although these keys are hard-wired into
   the program, the parser does not depend on their presence.
   Parsing is controlled by sequence, not by tags, so a file
   does not need to use any of these keys.
  }
  KEY_AUTHOR                     = 'Author';
  KEY_BEGIN                      = 'Begin';          {e.g., for a selected range of moves for optimization}
  KEY_BOARD_TRANSFORMATION       = 'View';           {example: 'View: rotated 90 degrees, mirrored vertically'}
  KEY_BOARD_TRANSFORMATION_FLIP_VERTICALLY           {must be lowercase}
                                 = 'vertical';       {the board is flipped vertically}
  KEY_BOARD_TRANSFORMATION_FLIP_HORIZONTALLY         {must be lowercase}
                                 = 'horizontal';     {the board is flipped horizontally}
  KEY_BOARD_TRANSFORMATION_ROTATE_COUNTER_CLOCKWISE  {must be lowercase}
                                 = 'counter';        {the board is rotated counter-clockwise}
  KEY_COLLECTION                 = 'Collection';     {tag for level collection names; some files use this tag in the file-header; see also 'KEY_SET'}
  KEY_DATE_CREATED               = 'Date Created';
  KEY_DATE_LAST_CHANGE           = 'Date of Last Change';
  KEY_END                        = 'End';            {e.g., for a selected range of moves for optimization}
  KEY_FILE_FORMAT                = 'File Format';    {the file format description may include a version number after this key}
  KEY_INTERVAL                   = 'Interval';       {e.g., interval size for the optimizer feature "partion solution into subintervals"}
  {$IFDEF SokHtml}
    KEY_OPTIMIZER                = 'Optimizer';
  {$ENDIF}
  KEY_PUZZLE_TYPE                = 'Puzzle Type';    {puzzle type: 'Sokoban', 'Hexoban', etc.}
  KEY_PUZZLE_TYPES               : array[TPuzzleType] of string =
                                   ('Sokoban');      {insert the puzzle types supported by the program}
  KEY_REMOVE_REDUNDANT_WALLS     = 'Remove redundant walls';
  KEY_SELECTED                   = 'Selected';
  KEY_SELECTED_SQUARES           = 'Selected squares';
  KEY_SET                        = 'Set';            {tag for level collection names; some files use this tag in the file-header; see also 'KEY_COLLECTION'}
  KEY_SETTINGS                   = 'Settings';
  KEY_TIME                       = 'Time';           {some files use this tag in notes, indicating time spent on a level}
  KEY_TITLE                      = 'Title';          {some files use this tag when title is a part of the notes}
  KEY_UNOFFICIAL_RELEASE         = '(Unofficial release, subject to change)'; {remove unofficial releases when files are merged}

  SECTION_NAME_FIRST             = '*First*';        {internal use only}
  SECTION_NAME_INFORMATION       = 'Information';    {an early version used this section-header for level notes}
  SECTION_NAME_LAST              = '*Last*';         {internal use only}

  {Snapshot Types}
  {The snapshot names are not reserved names in the file format;}
  {They are merely conventions used in Sokoban YASC}
type
  TSnapshotType                  = (stSnapshot,stReverseSnapshot, {the ordering is fixed and must not change; otherwise, change 'IsASpecialSnapshotName'}
                                    stSolution,stBestSolution,
                                    stBestSolutionMoves,stBestSolutionPushes,
                                    stBuiltinBestSolutionMoves,stBuiltinBestSolutionPushes,
                                    stSaveGame,stBestSolutionMoves1);
const
  SNAPSHOT_TYPE_NAME             : array[TSnapshotType] of String
                                 = (TEXT_SNAPSHOT,
                                    'Reverse Mode Snapshot',
                                    TEXT_SOLUTION,
                                    TEXT_BEST_SOLUTION,
                                    'Solution/Moves',  {best solution/moves}
                                    'Solution/Pushes', {best solution/pushes}
                                    'Solution/Moves (' +TEXT_BUILT_IN+RIGHT_PAREN,
                                    'Solution/Pushes ('+TEXT_BUILT_IN+RIGHT_PAREN,
                                    'SaveGame',      {the user's current game}
                                    'Solution/Steps' {an early version used this key instead of 'Solution/Moves'}
                                   );

  {File Format Description}
const
  FILE_FORMAT_DESCRIPTION_MAJOR_VERSION_NO
                                 ='0';               {caution: changing length of this string requires modifications in 'DEFAULT_FILE_FORMAT_DESCRIPTION'}
  FILE_FORMAT_DESCRIPTION_MINOR_VERSION_NO
                                 ='17';              {caution: changing length of this string requires modifications in 'DEFAULT_FILE_FORMAT_DESCRIPTION'}
  DEFAULT_FILE_FORMAT_DESCRIPTION: array[0..124] of string = (
  '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::',
  '::         '+TEXT_SOKOBAN_COPYRIGHT+            '         ::',
  '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::',
  '::                    File Format '+
  FILE_FORMAT_DESCRIPTION_MAJOR_VERSION_NO+PERIOD+
  FILE_FORMAT_DESCRIPTION_MINOR_VERSION_NO+
                                        '                    ::', {caution: the number of spaces is important for proper formatting}
  '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::',
  '::                                                        ::',
  ':: File Notes                                  Optional   ::',
  ':: Puzzle 1                                    Required   ::',
  '::    Title                                    Optional*  ::',
  '::    Board                                    See legend ::',
  '::    Puzzle Notes                             Optional   ::',
  '::    Saved Game or Solution 1                 Optional   ::',
  '::      Title                                  Optional*  ::',
  '::      Moves                                  See legend ::',
  '::      Notes                                  Optional   ::',
  '::    Saved Game or Solution 2                 Optional   ::',
  '::    ... (more saved games and solutions)                ::',
  ':: Puzzle 2                                    Optional   ::',
  ':: ... (more puzzles)                                     ::',
  '::                                                        ::',
  ':: Remarks:                                               ::',
  '::                                                        ::',
  ':: File Notes                                             ::',
  '::   File notes consist of unstructured text and          ::',
  '::   key/value properties, such as "Author: Name". Lines  ::',
  '::   beginning with "::" are comments meant to be read    ::',
  '::   only by a person examining the file in a text        ::',
  '::   editor, and should not be displayed by the Sokoban   ::',
  '::   program.                                             ::',
  '::                                                        ::',
  '::   The optional but recommended property                ::',
  '::   "Collection: Name" assigns a name to the puzzle      ::',
  '::   collection. When a collection is copied from the     ::',
  '::   internet, for example, and pasted into a Sokoban     ::',
  '::   program, this information allows the collection to   ::',
  '::   be saved with the proper name.                       ::',
  '::                                                        ::',
  ':: Titles                                                 ::',
  '::   A title line is the last non-blank text line before  ::',
  '::   a board, a saved game, or a solution, provided the   ::',
  '::   line is preceded  by a blank line or it is the only  ::',
  '::   text line at this position in the file.              ::',
  '::                                                        ::',
  '::   Title lines are optional unless a single or a last   ::',
  '::   text line from a preceding puzzle, saved game,       ::',
  '::   solution, or file header can be mistaken for a title ::',
  '::   line.                                                ::',
  '::                                                        ::',
//'::   The characters ":\*?[];<>|" are filtered out of      ::',
//'::   puzzle titles as well as game titles.                ::',
//'::                                                        ::',
  ':: Puzzle Notes                                           ::',
  '::   Two special key/value pairs are supported in puzzle  ::',
  '::   notes: "Title" and "Author", hence, titles can       ::',
  '::   either come from a title line or from a key/value    ::',
  '::   pair.                                                ::',
  '::                                                        ::',
  '::::::::::::::::::::::::::: Board ::::::::::::::::::::::::::',
  ':: Legend.................:      :.................Legend ::',
  '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::',
  ':: Wall...................: #  # :...................Wall ::',
  ':: Pusher.................: p  @ :.................Pusher ::',
  ':: Pusher on goal square..: P  + :..Pusher on goal square ::',
  ':: Box....................: b  $ :....................Box ::',
  ':: Box on goal square.....: B  * :.....Box on goal square ::',
  ':: Goal square............: .  . :............Goal square ::',
  ':: Floor..................:      :..................Floor ::',
  ':: Floor..................: -  _ :..................Floor ::',
  '::                                                        ::',
  ':: Remarks:                                               ::',
  '::                                                        ::',
  '::   The first and the last non-empty square in each row  ::',
  '::   must be a wall or a box on a goal. An empty interior ::',
  '::   row is written with at least one "-" or "_".         ::',
  '::                                                        ::',
  '::   Boards may be run-length encoded (RLE), e.g.,        ::',
  '::   "###----p.#" may be encoded as "3#4-p.#", and        ::',
  '::   "#-#-#-##-#-#-#" may be encoded as "2(3(#-)#)".      ::',
  '::   A row cannot be split over multiple lines.           ::',
  '::                                                        ::',
  '::   Rows may be combined on a single line by using "|"   ::',
  '::   as a row separator, e.g., "--3#|3#-#|#pb.#|5#".      ::',
  '::   A "|" at the end of a line is optional and may be    ::',
  '::   omitted.                                             ::',
  '::                                                        ::',
  '::::::::::::::::::::::::::: Moves ::::::::::::::::::::::::::',
  ':: Legend.................:      :.................Legend ::',
  '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::',
  ':: Move pusher up.........: u  U :.......Push/pull box up ::',
  ':: Move pusher down.......: d  D :.....Push/pull box down ::',
  ':: Move pusher left.......: l  L :.....Push/pull box left ::',
  ':: Move pusher right......: r  R :....Push/pull box right ::',
  ':: Begin jump.............: [  ] :...............End jump ::',
  ':: Begin pusher change....: {  } :......End pusher change ::',
  ':: Current position.......: *  * :.......Current position ::',
  '::                                                        ::',
  ':: Remarks:                                               ::',
  '::                                                        ::',
  '::   Moves may be run-length encoded, e.g., "3r4U" means  ::',
  '::   "rrrUUUU", and "2(3(dr)R)" means "drdrdrRdrdrdrR".   ::',
  '::   Each line must, however, have at least one proper    ::',
  '::   non-digit character. Spaces between moves are        ::',
  '::   allowed.                                             ::',
  '::                                                        ::',
  '::   Jumps and pulls: Only in reverse mode saved games    ::',
  '::   and solutions.                                       ::',
  '::                                                        ::',
  '::   Reverse mode saved games and solutions must begin    ::',
  '::   with a jump, even if it is empty. An example:        ::',
  '::   "[]U[rr]d".                                          ::',
  '::                                                        ::',
  '::   Pusher changes: Only in puzzles with multiple        ::',
  '::   pushers, e.g., Multiban. Moves inside the braces     ::',
  '::   depict the relative movement to get from the         ::',
  '::   currently active pusher to the next active pusher.   ::',
  '::   At game start, a "{...}" sequence activates the      ::',
  '::   pusher relative to the top-left pusher. An example:  ::',
  '::   "{rddd}Urr{uul}uLU". If the top-left pusher is the   ::',
  '::   first active pusher, then the empty "{}" can be      ::',
  '::   omitted.                                             ::',
  '::                                                        ::',
  '::   The current position is optional and defaults to the ::',
  '::   position after the last move.                        ::',
  '::                                                        ::',
  '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::',
  KEY_DATE_CREATED+COLON+SPACE,
  KEY_DATE_LAST_CHANGE+COLON+SPACE,
  '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::');

type
  {
  Generally, in vectors and arrays, elements with index 0 are not used;
  thus, iterating over a vector should use
  'for i:=1 to Count do ...', and *not* 'for i:=0 to Count-1 do ...'
  }
  TAlternatingOptimizationMetrics= (aomPrimarySecondary,aomBoxLinesPrimary);
  {$IFDEF SokobanYASC_ProductionVersion}
    TColRow                      = packed record X,Y:Byte; end;
  {$ELSE}
    TColRow                      = packed record X,Y:Word; end;
  {$ENDIF}
  {The internal board is implemented as a [column,row] array, with a wall-filled border}
  TBoardSquare                   = Integer;
  TBoard                         = array[0..MAX_BOARD_WIDTH +1,
                                         0..MAX_BOARD_HEIGHT+1] of TBoardSquare;
  TBoardAsText                   = record {representing the board as a text string is often a convenient way to allocate memory for the board dynamically}
                                     Board  : String;
                                     Height : Integer;
                                     Width  : Integer;
                                   end;
  TBoardOfBooleans               = array[0..MAX_BOARD_WIDTH +1,
                                         0..MAX_BOARD_HEIGHT+1] of Boolean;
  TBoardOfBytes1D                = array[0..MAX_BOARD_SIZE]     of Byte; {1-dimensional vector or byte-sized board squares}
  TBoardOfIntegers               = array[0..MAX_BOARD_WIDTH +1,
                                         0..MAX_BOARD_HEIGHT+1] of Integer;
  TBoardOfUnsignedIntegers       = array[0..MAX_BOARD_WIDTH +1,
                                         0..MAX_BOARD_HEIGHT+1] of Cardinal;
  TBoardSquareTimeStamps         = array[0..MAX_BOARD_WIDTH +1,
                                         0..MAX_BOARD_HEIGHT+1] of SokUtil_.TTimeStamp;
  TBoardTimeStamps               = record
                                     FromSquare : TColRow;
                                     Squares    : TBoardSquareTimeStamps;
                                     Timestamp  : SokUtil_.TTimeStamp;
                                   end;
  TBoxPositions                  = array[0..MAX_BOXES] of TColRow;
  PBoxPositions                  = ^TBoxPositions;
  TGameMetricsExtended           = (gmMoves,gmPushes,gmBoxLines,gmBoxChanges,gmPushingSessions,gmPlayerLines,gmBoxLinesAndChanges); {order must not change; see 'TGameMetrics'}
  TGameMetrics                   = gmMoves..gmPlayerLines; {the order must match the field lists in the type declarations 'TMetrics' and 'TSecondaryScoreMetrics'}
  TGameState                     = (gsNull,gsPlay,gsSolved,gsStop,gsEdit);

  THistoryMove                   = Byte;
  {A history-move is a bit-set:
     0: 3 bits direction (udlr + 4 extra directions for games like Hexoban)
     3: 2 bits combined move separator
          (bit 3: done/undone move
           bit 4: odd/even combined move-number)
     5: 1 bit box-change flag, i.e., the box, if any, is different from the last touched box
     6: 1 bit jump move (reverse mode games only)
     7: 1 bit box-move flag, i.e., '1' means a push or a pull
  }
  THistoryMoves                  = array[0..MAX_MOVES+1] of THistoryMove; {'+1': so a pointer beyond last move is legal, and so the moves may include a sentinel}
  PHistoryMoves                  = ^THistoryMoves;
  THistory                       = record {game history}
                                     Count            :Integer; {number of moves for current position}
                                     LastBoxNo        :Integer; {last pushed/pulled box for current position}
                                     LastPushIndex    :Integer; {last pushed/pulled box: its move number, that is, its index in the 'Moves' vector}
                                     PlayerLinesCount :Integer; {number of player lines for the current position}
                                     PushCount        :Integer; {number of pushes (or pulls in a reverse mode game) for current position}
                                     Top              :Integer; {highest move number}
                                     Moves            :THistoryMoves;
                                   end;

  TNotes = class(TNode)
    Lines             :TList;
    MacroExpanded     :Boolean; {have macros been expanded?}
    Modified          :Boolean;
    constructor Create; {throws EOutOfMemory}
    destructor  Destroy; override;

    procedure   Clear; override;
    function    CopyTo(Destination__:TNode):Boolean; override;
    function    MacroExpand(Macros__:TList; const NewLine__:String):Boolean;
    function    WriteToFile(TextFile__:PTextFile):Integer; override; {throws EInOutError}
  end;

  TSecondaryScoreMetrics
                      = packed record {the order must match the enumerated type 'TGameMetrics'}
   BoxLines           :Integer;
   BoxChanges         :Integer;
   PushingSessions    :Integer;
   PlayerLines        :Integer;
                        end;

  TMetrics            = packed record {the order must match the enumerated type 'TGameMetrics'}
    case Boolean of
      False: (MoveCount
                      :Integer;
              PushCount
                      :Integer;
              SecondaryMetrics
                      :TSecondaryScoreMetrics;
             );
      True:  (Metrics : array[TGameMetrics] of Integer;
             );
                        end;


  {serial numbers are assumed to be safe unique identifiers for items
   (objects) created in one session with the application; its data type must
   be large enough to make wrap arounds impossible in practice
  }
  TSerialNo=Int64;

  TSnapshotAsText = class(TNode)
    MovesAsTextLines  :TList;   {game history as raw text}
    Notes             :TNotes;
    Tag               :Integer; {free to use; while a file is loaded, the field is used for numbering snapshots with identical names}
    constructor Create; {throws EOutOfMemory}
    destructor  Destroy; override;

    procedure   Clear; override;
    function    CopyTo(Destination__:TNode):Boolean; override; {not implemented}
    function    IsASaveGame:Boolean;
    function    MovesToTextLines(Moves__:PHistoryMoves; Count__,Top__:Integer;
                                 ReverseMode__,PrettyPrintGamesEnabled__,RleEnabled__,CombinedMoveFormattingEnabled__:Boolean):Boolean; {internal form -> text}
    function    ObjectType:TObjectType; override;
    function    TextLinesToMoves(var History__:THistory; var ReverseMode__:Boolean {$IFDEF SokHtml}; var MovePlayerBackToStart__:Integer {$ENDIF}):Boolean; {text -> internal form}
    function    WriteToFile(TextFile__:PTextFile):Integer; override; {throws EInOutError}
  end;

  {TSelectedRange     : [ start, end, range, MovesStringStartIndex, MovesStringEndIndex, (player position for next push x next push direction), repeat interval size ]; the pushes selected for optimization, start exclusive, end inclusive}
  TSelectedRange      =array[0..6] of Integer;
  TUpperBoundMetrics  =array[TAlternatingOptimizationMetrics] of TMetrics;

  TExtendedSnapshotAsText = class(TSnapshotAsText)
    ImprovementAsText :String;   {for the optimizer host in YASC; the format is '  (%d/%d/%d/%d/%d/%d)' with the number of moves, pushes, and secondary metrics}
    IsASolution       :Boolean;
    Metrics           :TMetrics;
    OptimizationFlags :Cardinal; {actual optimization flags used by the optimizer; the host may have called the optimizer with a different value}
    SelectedRange     :TSelectedRange;
    SerialNo          :TSerialNo; {sequential numbering}
    TimeMS            :TTimeMS;
    TimeOfBirthMS     :TTimeMS;  {for the YASGen level generator in YASC; the time really belongs to the level but it is easier to add it here}
    constructor Create; {throws EOutOfMemory}
    destructor  Destroy; override;

    {$IFDEF SokobanYASC}
      function  FormattedName(SecondaryMetricsInTitles__:Boolean):String;
    {$ENDIF}

    function    ObjectType:TObjectType; override;
    end;

  TLevelTagFlags           = (ltfSelected,ltfSelectedForFurtherProcessing,ltfLocked,ltfNew,ltfProcessed,ltfBest,ltfSolver,ltfUnsolvable);
  TLevelTagFlagsSet        = set of TLevelTagFlags;
  TLevelTag                = packed record {for conveniency, the special use of the tag field by some plugins is reflected in the definition of the tag field}
    case Boolean of
      False                : (Value       : Integer); {normal use of the tag field for storing an integer value}
      True                 : (BoardHeight : Byte;     {some plugins use the tag to store flags and board dimensions}
                              BoardWidth  : Byte;
                              Flags       : TLevelTagFlagsSet;
                              Tag1        : Byte);
  end;

  TLevel = class(TNode)         {level as text, i.e., external format}
    BoardAsTextLines  :TList;   {board as raw text-lines}
    Flags             :Integer; {free to use; while a file is loaded, the field is used for a linked list of levels that hash to the same bucket}
    IsMadeFromMoves   :Boolean; {'True': the level was created based on moves, e.g., a solution}
    Notes             :TNotes;
{   SerialNo          :TSerialNo;}
    SnapshotsAsText   :TList;   {snapshots as raw text-lines}
    Tag               :TLevelTag; {free to use; while a file is loaded, the field is used for numbering levels with identical names; plugin task queues use the tag field extensively for handling the items (e.g., levels and solutions) on the queues}
    constructor Create; {throws EOutOfMemory}
    destructor  Destroy; override;

    function    BoardToTextLines(BoardWidth__,BoardHeight__:Integer; {internal form -> text}
                                 const Board__:TBoard):Boolean;
    function    BoardToTextRLE(RleFloor__:Char; var Text__:String):Boolean; {board as run length encoded text}
    procedure   Clear; override;
    function    CopyTo(Destination__:TNode):Boolean; override; {not implemented}
    function    GetExtendedSnapshotAsTextBySerialNo(SerialNo__:TSerialNo):TExtendedSnapshotAsText;
    function    GetSnapshotAsTextByIndex(Index__: Integer):TSnapshotAsText;
    function    GetSnapshotAsTextByName(const Name__: String):TSnapshotAsText;
    function    LookupSnapshotAsText(const Moves__:SokUtil_.TList; var SnapshotAsText__:TSnapshotAsText):Boolean;
    procedure   MacroExpandNotes(MacroExpandLevelNotes__,MacroExpandSnapshotNotes__:Boolean; Macros__:SokUtil_.TList);
    function    ObjectType:TObjectType; override;
    function    TextLinesToBoard(var Board__      :TBoard; {text -> internal form}
                                 var BoardWidth__ :Integer;
                                 var BoardHeight__:Integer;
                                 var BoxCount__   :Integer;
                                 var GoalCount__  :Integer;
                                 var PlayerPos__  :TColRow;
                                 var ErrorText__  :String):Boolean;
    function    ToText(var Text__:String):Boolean;
    function    WriteToFile(TextFile__:PTextFile):Integer; override; {throws EInOutError}
  end;

  TSokoFileCallBackFunction=function(Count__:Integer; Level__:TLevel):Boolean;

  TSokoFile = class(TNode) {note that a non-blank name indicates an open file}
    AddFileFormatDescriptionToFiles
               :Boolean;
    FileHeader :TNotes;    {metatext, i.e., text before first level, including notes, macros, etc.}
    FileTime   :TFileTime;
    HasBoardTags
               :Boolean;   {normally, boards aren't tagged but it can be necessary in, say, clipboard files where boards don't have to be well-formed}
    PuzzleType :TPuzzleType; {Sokoban, Hexoban, etc.}
    Levels     :TList;     {levels in raw text format}
    Modified   :Boolean;
    MakeBoardFromMovesEnabled
               :Boolean;   {'True': if the input file contains snapshots but no board, then make a board based on the snapshot(s)}
    ParseSnapshotsWithoutBoardsAsIndividualLevels
               :Boolean;   {'False' (default): a sequence of snapshots (typically solutions) without boards are handled as 1 level with multiple snapshots}
                           {'True': each snapshot without a board creates a new level; this is handy for loading solution-files before merging them with a master level-file}

    constructor Create;    {throws EOutOfMemory}                                {related: Create/Destroy/Clear}
    destructor  Destroy; override;                                              {related: Create/Destroy/Clear}

    procedure   Clear; override;                                                {related: Create/Destroy/Clear}
    function    Close:Boolean;                                                  {related: Open/Close/Flush/New}
    function    CopyTo(Destination__:TNode):Boolean; override;                  {not implemented}
    function    ExportToFile(const FileName__:String; WriteTitleAndAuthor__, WriteNotes__,WriteAllSnapshots__,WriteSnapshotsOnly__,FillFloors__,RleEnabled__:Boolean; FilledFloor__,RleFloor__:Char; var Count__:Integer):Boolean;
    function    FindPriorOrNextLevelName(var LevelName__:String; Prior__,WrapAround__:Boolean):Boolean;
    function    Flush:Boolean;                                                  {related: Open/Close/Flush/New}
    function    GetLevelByBoard( BoardAsTextLines__ : TList ) : TLevel;
    function    GetLevelByExtendedSnapshotAsTextSerialNo(SerialNo__:TSerialNo):TLevel;
    function    GetLevelByIndex(Index__: Integer):TLevel;
    function    GetLevelByName(const Name__: String):TLevel;
{   function    GetLevelBySerialNo(SerialNo__:TSerialNo):TLevel;}
    function    HasFileFormatDescription(var FirstLineNo__,LastLineNo__,MajorVersionNo__,MinorVersionNo__:Integer):Boolean;
    function    HasOldVersionFileFormatDescription(var FirstLineNo__,LastLineNo__:Integer):Boolean;
    function    IsASokobanFile(const FileName__:String):Boolean;
    function    LoadFromClipboard:Boolean;                                      {related: LoadFromFile/SaveToFile/LoadFromClipboard}
    function    LoadFromFile(const FileName__:String):Boolean; override;        {related: LoadFromFile/SaveToFile/LoadFromClipboard}
    function    LoadFromText(const Text__:String):Boolean;                      {related: LoadFromText/LoadFromTextLines}
    function    LoadFromTextLines(Lines__:TList):Boolean;                       {related: LoadFromText/LoadFromTextLines}
    procedure   MacroExpandNotes(MacroExpandLevelNotes__,MacroExpandSnapshotNotes__:Boolean);
    function    MergeSokoFile(NewSokoFile__:TSokoFile; AcceptSmallDifferences__,DeleteEmptyUnofficiallyReleasedLevels__,ImportLevels__:Boolean):Boolean;
    function    MergeTextFile(const FileName__:String; AcceptSmallDifferences__,DeleteEmptyUnofficiallyReleasedLevels__:Boolean):Boolean;
    function    New (const FileName__:String):Boolean;                          {related: Open/Close/Flush/New}
    function    ObjectType:TObjectType; override;
    function    Open(const FileName__:String):Boolean;                          {related: Open/Close/Flush/New}
    function    RemoveFileFormatDescription:Boolean;
    function    SaveToFile(const FileName__:String):Boolean;                    {related: LoadFromFile/SaveToFile/LoadFromClipboard}
    function    UpdateFileFormatDescription(AddFileFormatDescription__:Boolean):Boolean; {keeps format-descriptions updated}
  end;

{
 --------------------------------------------------------------------
 Exported functions
 --------------------------------------------------------------------
}

function  BoardHashValue         (BoardWidth__,BoardHeight__:Integer; const Board__:TBoard):Cardinal;
function  BoardSquareToChar      ( SquareValue__ : Integer ) : Char;
function  BoardToText            (BoardWidth__,BoardHeight__:Integer; const Board__:TBoard; const RowSeparator__:String):String; {throws EOutOfMemory}
function  CalculatePlayerLines   (var History__:THistory):Integer;
function  CalculatePlayersReachableSquares(PlayerCol__,PlayerRow__,BoardWidth__,BoardHeight__: Integer; var Board__:TBoard; ContinueExistingCalculation__:Boolean; var TopLeftCol__,TopLeftRow__:Integer):Integer;
function  CharToBoardSquareValue (Ch:Char):Integer;
function  CharToDirection        (Ch:Char; var Direction:TDirection):Boolean;
procedure ClearBoard             (var Board__:TBoard);
function  CompareMetrics         (PrimaryMetric__,SecondaryMetric__:TGameMetrics; MovesTakesPrecedenceOverPushes__:Boolean; const Metrics1__,Metrics2__:TMetrics):Integer;
function  CompareSecondaryScoreMetrics(const SecondaryScoreMetrics1__,SecondaryScoreMetrics2__:TSecondaryScoreMetrics):Integer;
function  CreateObject           (ObjectType__:TObjectType; var Object__:TNode):Boolean;
function  DxDyToDirection        (dx__,dy__:Integer; var Direction__:TDirection):Boolean;
function  FindPlayerPosition     (BoardWidth__,BoardHeight__:Integer; var Board__:TBoard; var PlayerPos__:TColRow):Integer;
function  HistoryMoveToDxDy      (Move__:Integer; var dx__,dy__:Integer):Boolean; {always returns 'True'}
function  HistoryPushCountAtMoveNumber
                                 (const History__:THistory; MoveNo__:Integer):Integer;
function  IsALegalBoard          (const Board__:TBoard;
                                  BoardWidth__,BoardHeight__,BoxCount__,GoalCount__:Integer;
                                  PlayerPos__:TColRow;
                                  AllowEmptyRows:Boolean;
                                  FirstNonEmptySquareInEachRowMustBeAWallOrABoxOnGoal:Boolean;
                                  DisplayNumberOfBoxesAndGoalsOnMismatch:Boolean;
                                  RowOffset__:Integer;
                                  var ErrorText__:String):Boolean;
function  IsAMoveTextLine(const Text__:String):Boolean;
function  IsASaveGameName(const Name__:String):Boolean;
function  IsASpecialSnapshotName__ (const Name__:String; var SnapshotType__:TSnapshotType):Boolean;
function  IsSameBoardAsText      (const BoardAsText1__,BoardAsText2__:TBoardAsText):Boolean;
function  MakeBoardFromMoves     (const Moves__:PHistoryMoves; Count__:Integer; ReverseMode__:Boolean; var BoardWidth__,BoardHeight__:Integer; var Board__:TBoard; var PlayerPos__:TColRow):Boolean;
function  MakeBoardRectangular   (BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Integer;
function  MetricsAsText          (MoveCount__,PushCount__:Integer; const SecondaryScoreMetrics__:TSecondaryScoreMetrics):String; {throws EOutOfMemory}
function  MovesToText            (Moves__:PHistoryMoves; Count__,Top__:Integer; ReverseMode__,RleEnabled__,CombinedMoveFormattingEnabled__:Boolean; var MovesAsText__:String):Boolean; {throws EOutOfMemory}
function  MoveToChar             (Direction__:TDirection; IsABoxMove__:Boolean; LowerCaseMoves__:Boolean):Char;
function  NormalizeBoard         (MovePlayerAndBoxes__,
                                  ChangeImmovableBoxesOnNonGoalSquaresToWalls__,
                                  ChangeImmovableBoxesOnGoalSquaresToWalls__,
                                  FillUnnecessaryIllegalBoxSquares__,
                                  StableNormalization__:Boolean;
                                  var BoardWidth__,BoardHeight__:Integer; var Board__:TBoard;
                                  var PlayerPos__:TColRow; var History__:THistory):Integer;
function  RemoveRedundantWalls   (BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Integer;
function  SelectedBoardSquaresToText(BoardWidth__,BoardHeight__:Integer; const Board__:TBoard):String; {throws EOutOfMemory}
function  SelectedSquaresAsTextToBoard(const SelectedSquaresAsText__:String; BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Integer;
function  TextThatCannotBeInterpretedAsMoves(const Text__:String):String; {throws EOutOfMemory}
function  TextToBoard            (Width__,Height__:Integer; const BoardAsText__:String; var Board__:TBoard):Boolean;
function  TrimBoard              (MinWidth__:Integer; {remove empty columns and rows}
                                  MinHeight__:Integer;
                                  var Board__:TBoard;
                                  var BoardWidth__,BoardHeight__:Integer;
                                  var PlayerPos__:TColRow):Integer;

{
 --------------------------------------------------------------------
 Global variables
 --------------------------------------------------------------------
}
var
  {$IFDEF SokobanYASC}
    ApplicationMutex   : THandle  = 0; { for multiple processes synchronization}
  {$ENDIF}
  BlankLinesBeforeLevel: Integer  = 2; {number of blank lines written before each level, see 'TLevel.WriteToFile'}

implementation

uses
  SysUtils
  {$IFDEF SokobanYASC}
    ,Snapshots_,SokGame_,Duplicates_,Open1_,Main_
  {$ENDIF}
  ;

{
 --------------------------------------------------------------------
 Local variables
 --------------------------------------------------------------------
}

var
  {normally, boards aren't tagged but it can be necessary in, say, clipboard files where boards don't have to be well-formed}
  HasBoardTags      : Boolean  = False; {used in 'TLevel.WriteToFile'}
  LastSerialNo      : TSerialNo = 0;    {no wrap around control; the number data type must be large enough to make wrap arounds impossible in practice during one session with the program}

{
 --------------------------------------------------------------------
 Miscellaneous
 --------------------------------------------------------------------
}

function  BoardHashValue(BoardWidth__,BoardHeight__:Integer; const Board__:TBoard):THashTableHashKey;
const LEFT_SHIFT=5; RIGHT_SHIFT=BITS_PER_BYTE*SizeOf(Result)-LEFT_SHIFT;
var   Col,Row:Integer;
begin {the players on the board are not taken into account by this hashvalue calculation}
  Result:=THashTableHashKey(BoardWidth__*BoardHeight__);
  Result:=((Result shl LEFT_SHIFT) xor (Result shr RIGHT_SHIFT)) xor THashTableHashKey(BoardWidth__ );
  Result:=((Result shl LEFT_SHIFT) xor (Result shr RIGHT_SHIFT)) xor THashTableHashKey(BoardHeight__);
  for Col:=1 to BoardWidth__ do
      for Row:=1 to BoardHeight__ do
          Result:=((Result shl LEFT_SHIFT) xor (Result shr RIGHT_SHIFT)) xor THashTableHashKey(Board__[Col,Row] and (WALL+BOX+GOAL{+PLAYER})); {a hashdek function (Donald E. Knuth, "The Art Of Computer Programming Volume 3", 6.4)}
end;

function  BoardSquareToChar( SquareValue__ : Integer ) : Char;
begin
  case SquareValue__ and ( WALL + PLAYER + BOX + GOAL ) of
    WALL         : Result := WALL_CH;
    PLAYER       : Result := PLAYER_CH;
    PLAYER+GOAL  : Result := PLAYER_GOAL_CH;
    BOX          : Result := BOX_CH;
    BOX+GOAL     : Result := BOX_GOAL_CH;
    GOAL         : Result := GOAL_CH;
    else {FLOOR}   Result := FLOOR_CH;
  end;
end;

function  BoardToText(BoardWidth__,BoardHeight__:Integer; const Board__:TBoard; const RowSeparator__:String):String; {throws EOutOfMemory}
var i,Col,Row,Index,StartOfLineIndex:Integer; IsEmptyRow:Boolean;
begin
  SetLength(Result,BoardWidth__*BoardHeight__+Max(0,Pred(BoardHeight__))*Length(RowSeparator__));
  Index:=STRING_BASE-1;
  for Row:=1 to BoardHeight__ do begin
      StartOfLineIndex:=Index; IsEmptyRow:=True;
      for Col:=1 to BoardWidth__ do begin
          Inc(Index);
          case Board__[Col,Row] and (WALL+PLAYER+BOX+GOAL) of {inlined 'BoardSquareToChar'}
            WALL         : begin Result[Index]:=WALL_CH;        IsEmptyRow:=False; end;
            PLAYER       : begin Result[Index]:=PLAYER_CH;      IsEmptyRow:=False; end;
            PLAYER+GOAL  : begin Result[Index]:=PLAYER_GOAL_CH; IsEmptyRow:=False; end;
            BOX          : begin Result[Index]:=BOX_CH;         IsEmptyRow:=False; end;
            BOX+GOAL     : begin Result[Index]:=BOX_GOAL_CH;    IsEmptyRow:=False; end;
            GOAL         : begin Result[Index]:=GOAL_CH;        IsEmptyRow:=False; end;
            else {FLOOR}   begin Result[Index]:=FLOOR_CH; end;
          end; {case}
          end;
      if IsEmptyRow then
         for Col:=1 to BoardWidth__ do {fill the empty row with non-blank floor characters}
             Result[StartOfLineIndex+Col]:=FLOOR_NON_BLANK_CH2;
      if Row<BoardHeight__ then
         for i:=1 to Length(RowSeparator__) do begin
             Inc(Index); Result[Index]:=RowSeparator__[i];
             end;
      end;
end; {BoardToText}

function  CalculatePlayerLines(var History__:THistory):Integer;
var i:Integer;
begin
  with History__ do begin
    if   Count<>0 then begin
         PlayerLinesCount:=1;
         for i:=2 to Count do
             if TDirection(Moves[i] and H_MASK_DIRECTION)<>TDirection(Moves[Pred(i)] and H_MASK_DIRECTION) then
                Inc(PlayerLinesCount); {increase the number of player lines}
         end
    else PlayerLinesCount:=0;
    Result:=PlayerLinesCount;
    end;
end; {CalculatePlayerLines}
(*
function CalculatePlayersReachableSquares(PlayerCol__,PlayerRow__,BoardWidth__,BoardHeight__: Integer; var Board__:TBoard; ContinueExistingCalculation__:Boolean; var TopLeftCol__,TopLeftRow__:Integer):Integer;
  {Returns the number of reachable squares, including current player position; the normalized (top left) reachable square is returned in 'TopLeftCol__' and 'TopLeftRow__'}
type
  TStackItem=packed record Col,Row:Byte; end;
  TStack=record
    Bottom,Top:^TStackItem;
    Items:array[0..MAX_BOARD_SIZE+1] of TStackItem;
  end;
var
  Col,Row,NeighborSquareCol,NeighborSquareRow:Integer;
  Direction:TDirection;
  Stack:TStack;
begin {CalculatePlayersReachableSquares}
  Result:=0;
  if not ContinueExistingCalculation__ then begin
     for Col:=1 to BoardWidth__ do
         for Row:=1 to BoardHeight__ do  {clear board flags}
             Board__[Col,Row]:=Board__[Col,Row] and (not PLAYER_LEGAL_MOVE);
     TopLeftCol__:=PlayerCol__; TopLeftRow__:=PlayerRow__;
     end;
  if (PlayerCol__> 0           ) and (PlayerRow__> 0            ) and
     (PlayerCol__<=BoardWidth__) and (PlayerRow__<=BoardHeight__) and
     ((Board__[PlayerCol__,PlayerRow__] and (WALL+BOX+PLAYER_LEGAL_MOVE))=0) then  begin {the player must 'jump' instead of 'move' to get away from a wall or a box}
     Stack.Bottom:=Addr(Stack.Items[0]); Stack.Top:=Stack.Bottom;
     Inc(Stack.Top); Stack.Top^.Col:=PlayerCol__; Stack.Top^.Row:=PlayerRow__;
     while Stack.Top<>Stack.Bottom do begin
       Col:=Stack.Top^.Col; Row:=Stack.Top^.Row; Dec(Stack.Top); {pop the next square from the stack}
       Inc(Board__[Col,Row],PLAYER_LEGAL_MOVE); Inc(Result); {mark the square as reachable and count the number of reachable squares}
       if  (Row<TopLeftRow__) or ((Row=TopLeftRow__) and (Col<TopLeftCol__)) then begin
           TopLeftCol__:=Col; TopLeftRow__:=Row; {lowest [row,col] is used as a representative for the set of reachable squares}
           end;
       for Direction:=Low(Direction) to High(Direction) do begin
           NeighborSquareCol:=Col+DIRECTION_XY[Direction,ColAxis];
           NeighborSquareRow:=Row+DIRECTION_XY[Direction,RowAxis];
           if (Board__[NeighborSquareCol,NeighborSquareRow] and (WALL+BOX+PLAYER_LEGAL_MOVE))=0 then begin {'True': the neighbor square is empty, and it hasn't been visited earlier}
              Inc(Stack.Top); {put the neighbor square on the stack}
              Stack.Top^.Col:=NeighborSquareCol; Stack.Top^.Row:=NeighborSquareRow;
              end;
           end;
       end;
     end;
end; {CalculatePlayersReachableSquares}
*)
function CalculatePlayersReachableSquares(PlayerCol__,PlayerRow__,BoardWidth__,BoardHeight__: Integer; var Board__:TBoard; ContinueExistingCalculation__:Boolean; var TopLeftCol__,TopLeftRow__:Integer):Integer;
var Col,Row:Integer;
  {Returns the number of reachable squares, including current player position; the normalized (top left) reachable square is returned in 'TopLeftCol__' and 'TopLeftRow__'}

  procedure VisitReachableSquare(Col__,Row__:Integer);
  var NeighborSquareCol,NeighborSquareRow:Integer; Direction:TDirection;
  begin
    Inc(Board__[Col__,Row__],PLAYER_LEGAL_MOVE); Inc(Result); {mark the square as reachable and count the number of reachable squares}
    if  (Row__<TopLeftRow__) or ((Row__=TopLeftRow__) and (Col__<TopLeftCol__)) then begin
        TopLeftCol__:=Col__; TopLeftRow__:=Row__;           {lowest [row,col] is used as a representative for the set of reachable squares}
        end;
    for Direction:=Low(Direction) to High(Direction) do begin
        NeighborSquareCol:=Col__+DIRECTION_XY[Direction,ColAxis];
        NeighborSquareRow:=Row__+DIRECTION_XY[Direction,RowAxis];
        if (Board__[NeighborSquareCol,NeighborSquareRow] and (WALL+BOX+PLAYER_LEGAL_MOVE))=0 then {'True': the neighbor square is empty, and it hasn't been visited earlier}
           VisitReachableSquare(NeighborSquareCol,NeighborSquareRow);
        end;
  end; {VisitReachableSquare}

begin {CalculatePlayersReachableSquares}
  Result:=0;
  if not ContinueExistingCalculation__ then begin
     for Col:=1 to BoardWidth__ do
         for Row:=1 to BoardHeight__ do                             {clear board flags}
             Board__[Col,Row]:=Board__[Col,Row] and (not PLAYER_LEGAL_MOVE);
     TopLeftCol__:=PlayerCol__; TopLeftRow__:=PlayerRow__;
     end;
  if (PlayerCol__> 0           ) and (PlayerRow__> 0            ) and
     (PlayerCol__<=BoardWidth__) and (PlayerRow__<=BoardHeight__) and
     ((Board__[PlayerCol__,PlayerRow__] and (WALL+BOX+PLAYER_LEGAL_MOVE))=0) then {the player must 'jump' instead of 'move' to get away from a wall or a box}
     VisitReachableSquare(PlayerCol__,PlayerRow__);
end; {CalculatePlayersReachableSquares}

function CharToBoardSquareValue(Ch:Char):Integer;
begin
  case Ch of
    WALL_CH              : Result:=WALL;
    PLAYER_CH,
    PLAYER_CH1,
    PLAYER_CH2           : Result:=PLAYER+FLOOR;
    PLAYER_GOAL_CH,
    PLAYER_GOAL_CH1,
    PLAYER_GOAL_CH2      : Result:=PLAYER+GOAL+FLOOR;
    BOX_CH,
    BOX_CH1              : Result:=BOX+FLOOR;
    BOX_GOAL_CH,
    BOX_GOAL_CH1         : Result:=BOX+GOAL+FLOOR;
    GOAL_CH,
    GOAL_CH1             : Result:=GOAL+FLOOR;
    FLOOR_CH,
    FLOOR_NON_BLANK_CH1,
    FLOOR_NON_BLANK_CH2  : Result:=FLOOR;
    else                   if   (Ord(Ch)>=FLAG_FIXED_SQUARE) then begin {'True': it may be a fixed square for the level generator}
                                Result:=CharToBoardSquareValue(Chr(Ord(Ch)-FLAG_FIXED_SQUARE));
                                if Result<>0 then Result:=Result or FLAG_FIXED_SQUARE;
                                end
                           else Result:=0;
  end; {case}
end;

function CharToDirection(Ch:Char; var Direction:TDirection):Boolean;
var d:TDirection;
begin
  Result:=False; Ch:=UpCase(Ch);
  for d:=Low(d) to High(d) do
      if Ch=DIRECTION_TO_CHAR[d] then begin {caution: 'DIRECTION_TO_CHAR' must be uppercase characters}
         Direction:=d; Result:=True; break;
         end;
end; {CharToDirection}

procedure ClearBoard(var Board__:TBoard);
var Col,Row:Integer;
begin {Fills the board with floor-squares and adds a wall-filled border}
  for Col:=0 to MAX_BOARD_WIDTH+1 do begin
      for Row:=0 to MAX_BOARD_HEIGHT+1 do Board__[Col,Row]:=FLOOR; {floor}
      Board__[Col,0]:=WALL; Board__[Col,MAX_BOARD_HEIGHT+1]:=WALL; {top+bottom row squares}
      end;
  for Row:=0 to MAX_BOARD_HEIGHT+1 do begin
      Board__[0,Row]:=WALL; Board__[MAX_BOARD_WIDTH+1,Row]:=WALL; {left+right column squares}
      end;
end; {ClearBoard}

function CompareMetrics(PrimaryMetric__,SecondaryMetric__:TGameMetrics; MovesTakesPrecedenceOverPushes__:Boolean; const Metrics1__,Metrics2__:TMetrics):Integer;
begin {Compares metrics, first testing the metrics 'PrimaryMetric' and 'SecondaryMetric';}
      {Returns '0' if the two sets of metrics are identical, a negative number}
      {if the first set is better, and a positive value if the second set is better}
  with Metrics1__ do begin
    Result:=Metrics[PrimaryMetric__]-Metrics2__.Metrics[PrimaryMetric__];
    if Result=0 then begin
       Result:=Metrics[SecondaryMetric__]-Metrics2__.Metrics[SecondaryMetric__];
       if Result=0 then begin
          if MovesTakesPrecedenceOverPushes__ then Result:=MoveCount-Metrics2__.MoveCount;
          if Result=0 then begin
             Result:=PushCount-Metrics2__.PushCount;
             if Result=0 then begin
                Result:=MoveCount-Metrics2__.MoveCount;
                if Result=0 then Result:=CompareSecondaryScoreMetrics(SecondaryMetrics,Metrics2__.SecondaryMetrics);
                end;
             end;
          end;
       end;
    end;
end; {CompareMetrics}

function CompareSecondaryScoreMetrics(const SecondaryScoreMetrics1__,SecondaryScoreMetrics2__:TSecondaryScoreMetrics):Integer;
begin {Returns '0' if the two sets of metrics are identical, a negative number}
      {if the first set is better, and a positive value if the second set is better}
  with SecondaryScoreMetrics1__ do begin
    Result:=BoxLines-SecondaryScoreMetrics2__.BoxLines;
    if Result=0 then begin
       Result:=BoxChanges-SecondaryScoreMetrics2__.BoxChanges;
       if Result=0 then begin
          Result:=PushingSessions-SecondaryScoreMetrics2__.PushingSessions;
          if Result=0 then
             Result:=PlayerLines-SecondaryScoreMetrics2__.PlayerLines;
          end;
       end;
    end;
end; {CompareSecondaryScoreMetrics}

function CreateObject(ObjectType__:TObjectType; var Object__:TNode):Boolean;
begin  {Safe creation of a new object, i.e., show a proper error message in case of errors and return 'nil'}
  Result:=True; Object__:=nil;
  try    case ObjectType__ of
           otNode                   : Object__:=TNode                  .Create;
           otList                   : Object__:=TList                  .Create;
           otRle                    : Object__:=TRle                   .Create;
           otNotes                  : Object__:=TNotes                 .Create;
           otSnapshotAsText         : Object__:=TSnapshotAsText        .Create;
           otExtendedSnapshotAsText : Object__:=TExtendedSnapshotAsText.Create;
           otLevel                  : Object__:=TLevel                 .Create;
           otSokoFile               : Object__:=TSokoFile              .Create;
{          otSokoGame               : Object__:=TSokoGame              .Create;} {'TSokoGame' belongs to another module}
{          otSnapshot               : Object__:=TSnapshot              .Create;} {'TSnapshot' belongs to another module}
           else                     raise Exception.Create(TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_UNKNOWN_TYPE);
         end; {case}
  except on E:Exception do begin
            Object__.Free; Object__:=nil; Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_CREATE_OBJECT);
            end;
  end;
end; {CreateObject}

function DxDyToDirection(dx__,dy__:Integer; var Direction__:TDirection):Boolean;
begin
  Result:=True;
  if        dx__=0 then
            if      dy__=-1 then Direction__:=Up
            else if dy__= 1 then Direction__:=Down
                 else            Result:=False
  else if   dy__=0 then
            if      dx__=-1 then Direction__:=Left
            else if dx__= 1 then Direction__:=Right
                 else            Result:=False

       else Result:=False;
end; {DxDyToDirection}

function  FindPlayerPosition(BoardWidth__,BoardHeight__:Integer; var Board__:TBoard; var PlayerPos__:TColRow):Integer;
var Col,Row:Integer;
begin {Returns the number of players found on the board; the first encountered player is returned in 'PlayerPos__' and any extra players are removed from the board}
  Result:=0;
  PlayerPos__.X:=0; PlayerPos__.Y:=0;
  for Col:=1 to BoardWidth__ do
      for Row:=1 to BoardHeight__ do
          if (Board__[Col,Row] and PLAYER)<>0 then begin
             Inc(Result);
             if   PlayerPos__.X=0 then begin
                  PlayerPos__.X:=Col; PlayerPos__.Y:=Row;
                  end
             else Dec(Board__[Col,Row],PLAYER); {remove extra players from the board}
             end;
end; {FindPlayerPosition}

function HistoryMoveToDxDy(Move__:Integer; var dx__,dy__:Integer):Boolean;
begin
  Result:=True;
  Move__:=Move__ and H_MASK_DIRECTION;
  dx__  :=DIRECTION_XY[TDirection(Move__),ColAxis];
  dy__  :=DIRECTION_XY[TDirection(Move__),RowAxis];
end;

function  HistoryPushCountAtMoveNumber(const History__:THistory; MoveNo__:Integer):Integer;
var i:Integer;
begin
  Result:=0;
  with History__ do begin
    for i:=1 to Min(Top,MoveNo__) do
        if (Moves[i] and H_FLAG_BOX)<>0 then Inc(Result);
    end;
end;

function  IsABlockedBoxSquareAlongAxis( Col__, Row__ :Integer; Direction__ : TDirection; const Board__ : TBoard ) : Boolean;
var Neighbor1, Neighbor2 :TBoardSquare;
begin {considers walls only, i.e., boxes on the board are not taken into account;}
      {contrary to what the function name suggests, the parameter is a direction, not an axis}
  Neighbor1 := Board__[ Col__ + DIRECTION_XY[ Direction__, ColAxis ], Row__ + DIRECTION_XY[ Direction__, RowAxis ] ]; {note that 'Neighbor1' and 'Neighbor2' hold the contents of the squares, not the square numbers}
  Neighbor2 := Board__[ Col__ - DIRECTION_XY[ Direction__, ColAxis ], Row__ - DIRECTION_XY[ Direction__, RowAxis ] ];

  Result:= (
            (( Neighbor1 and ( WALL {+ INVISIBLE_WALL} ) ) <> 0 )               {is there a wall on any of the neighbor squares?}
            or
            (( Neighbor2 and ( WALL {+ INVISIBLE_WALL} ) ) <> 0 )
            or
            ((( Neighbor1 and ILLEGAL_SQUARE ) <> 0 )                           {are both neighbors illegal box squares?}
             and
             (( Neighbor2 and ILLEGAL_SQUARE ) <> 0 )
            )
           )
           and
           (( Board__[ Col__, Row__ ] and ( WALL + ILLEGAL_SQUARE + {FLAG_BOX_REACHABLE_SQUARE} + FLOOR ) ) = ( {FLAG_BOX_REACHABLE_SQUARE + } FLOOR ) ); {inlined 'IsALegalAndBoxReachableSquare()'}
end;

{ The standard SOK file-format is not required to support the chess-like
  notation for moves (e.g., "123: g4-f4"), but since this code was
  written as part of Sokoban YASC, where an early version used this
  notation, the code is still here for backwards compatibility.
}
function IsAChessNotationMoveLine(const Line__:String;
                                  BoardWidth__,BoardHeight__:Integer;
                                  CheckMoveNumberSequence:Boolean;
                                  var ChessNotationMoveCount__:Integer;
                                  var FromPos__,ToPos__:TColRow;
                                  var IsABoxMove__:Boolean):Boolean;
var i,j,n,LastIndex:Integer; Ch:Char;
begin {Chess notation: '99999. X99-X99' or '99999. X99*X99'; example: '23. g5-f5'}
  Result:=False; i:=AnsiPos(PERIOD,Line__);
  if i<>0 then begin {maybe a line with a single move in 'chess-notation'}
     j:=STRING_BASE; n:=0;
     while (j<i) and (Line__[j]<=SPACE) do Inc(j); {skip spaces}
     while (Line__[j]>='0') and (Line__[j]<='9') do begin
       n:=n*10+(Ord(Line__[j])-Ord('0')); {'n' = move number}
       Inc(j);
       end;
     while (j<i) and (Line__[j]<=SPACE) do Inc(j);
     if (j=i) {only blanks between number and period}
        and
        ((not CheckMoveNumberSequence)
         or
         (n=Succ(ChessNotationMoveCount__)) {correct number sequence}
        )
        and
        (n<=MAX_MOVES) then begin {no table overflow}
        Inc(i); LastIndex:=StrLastCharIndex(Line__);
        while (i<LastIndex) and (Line__[i]<=SPACE) do Inc(i); {skip blanks}
        if i<=LastIndex-4 then begin {check for sufficent characters to constitute a move}
           Ch:=Line__[i];
           FromPos__.x:=Succ(Ord(UpCase(Ch))-Ord('A'));
           if (BoardWidth__>Succ(Ord('Z')-Ord('A'))) and
              (Ch>='A') and (Ch<='Z') then {uppercase means high columns}
              Inc(FromPos__.x,Succ(Ord('Z')-Ord('A')));
           FromPos__.y:=Ord(Line__[i+1])-Ord('0');
           if   (Line__[i+2]>='0') and (Line__[i+2]<='9') then begin
                FromPos__.y:=FromPos__.y*10+(Ord(Line__[i+2])-Ord('0')); {two digits}
                Inc(i,3);
                end
           else Inc(i,2);
           FromPos__.y:=Succ(BoardHeight__)-FromPos__.y; {bottom-to-top numbering}
           if (i<=LastIndex-2) and
              ((Line__[i]='-') or (Line__[i]='*')) then begin {'-': a move, '*': a push}
              IsABoxMove__:=Line__[i]='*';
              Inc(i); Ch:=Line__[i];
              ToPos__.x:=Succ(Ord(UpCase(Ch))-Ord('A'));
              if (BoardWidth__>Succ(Ord('Z')-Ord('A'))) and
                 (Ch>='A') and (Ch<='Z') then {uppercase means high columns}
                 Inc(ToPos__.x,Succ(Ord('Z')-Ord('A')));
              Inc(i); ToPos__.y:=Ord(Line__[i])-Ord('0');
              if (i<LastIndex) and
                 (Line__[Succ(i)]>='0') and (Line__[Succ(i)]<='9') then begin
                    Inc(i);
                    ToPos__.y:=ToPos__.y*10+(Ord(Line__[i])-Ord('0')); {two digits}
                    end;
              ToPos__.y:=Succ(BoardHeight__)-ToPos__.y; {bottom-to-top numbering}

              if (FromPos__.x>=1) and (FromPos__.x<=BoardWidth__ ) and
                 (FromPos__.y>=1) and (FromPos__.y<=BoardHeight__) and
                 (ToPos__  .x>=1) and (ToPos__  .x<=BoardWidth__ ) and
                 (ToPos__  .y>=1) and (ToPos__  .y<=BoardHeight__) and
                 (Abs(ToPos__.x-FromPos__.x+ToPos__.y-FromPos__.y)=1) then begin
                 Result:=True;
                 ChessNotationMoveCount__:=n;
                 end;
              end;
           end;
        end;
     end;
end; {IsAChessNotationMoveLine}

function IsALegalBoard(const Board__:TBoard;
                       BoardWidth__,BoardHeight__,BoxCount__,GoalCount__:Integer;
                       PlayerPos__:TColRow;
                       AllowEmptyRows:Boolean;
                       FirstNonEmptySquareInEachRowMustBeAWallOrABoxOnGoal:Boolean;
                       DisplayNumberOfBoxesAndGoalsOnMismatch:Boolean;
                       RowOffset__:Integer;
                       var ErrorText__:String):Boolean;
var Col,FirstWallOrBoxOnGoalColumn,Left,Right,Row,Value:Integer;
begin
  Result:=False; ErrorText__:='';
  try    if      BoardHeight__=0 then
                 ErrorText__:=TEXT_BOARD_ERROR_NO_BOARD
         else if (BoardWidth__ <MIN_BOARD_WIDTH ) or (BoardWidth__ >MAX_BOARD_WIDTH ) or
                 (BoardHeight__<MIN_BOARD_HEIGHT) or (BoardHeight__>MAX_BOARD_HEIGHT) then
                 ErrorText__:=Format(TEXT_BOARD_ERROR_SIZE_FORMAT,[MAX_BOARD_WIDTH,MAX_BOARD_HEIGHT,MIN_BOARD_WIDTH,MIN_BOARD_HEIGHT])
         else if GoalCount__<>BoxCount__ then
                 if   DisplayNumberOfBoxesAndGoalsOnMismatch then
                      ErrorText__:=Format(TEXT_BOARD_ERROR_BOX_GOAL_MISMATCH+NL+TEXT_BOXES_AND_GOALS_FORMAT,[BoxCount__,GoalCount__])
                 else ErrorText__:=TEXT_BOARD_ERROR_BOX_GOAL_MISMATCH
         else if (BoxCount__>MAX_BOXES) or (GoalCount__>MAX_BOXES) then
                 ErrorText__:=Format(TEXT_BOARD_ERROR_BOX_COUNT_FORMAT,[MAX_BOXES])
         else if PlayerPos__.x=0 then
                 ErrorText__:=TEXT_BOARD_ERROR_NO_PLAYER
         else begin
            Result:=True;
            for Row:=1 to BoardHeight__ do
                if Result then begin
                   Col:=1;
                   while (Col<=BoardWidth__) and ((Board__[Col,Row] and (WALL+PLAYER+BOX+GOAL))=0) do Inc(Col);
                   Left:=Col; Right:=0; FirstWallOrBoxOnGoalColumn:=0;
                   while (Col<=BoardWidth__) do begin
                     Value:=Board__[Col,Row];
                     if (FirstWallOrBoxOnGoalColumn=0)
                        and
                        (((Value and WALL)<>0)
                         or
                         ((Value and (BOX+GOAL))=BOX+GOAL)
                        ) then
                        FirstWallOrBoxOnGoalColumn:=Col;
                     if (Value and (WALL+PLAYER+BOX+GOAL))<>0 then Right:=Col;
                     Inc(Col);
                     end;
                   {Result:=Right>=MIN_BOARD_WIDTH;}
                   Result:=(Right<>0);
                   if Result then begin {'True': a non-empty row}
                      if FirstNonEmptySquareInEachRowMustBeAWallOrABoxOnGoal then begin
                        {'TSokoFile' can only load levels obeying this rule,}
                        {but other modules might use the more relaxed rule below}
                        Result:=FirstWallOrBoxOnGoalColumn=Left;
                        if not Result then
                           ErrorText__:=Format(TEXT_BOARD_ERROR_MISSING_WALL1+TEXT_BOARD_ERROR_ROW_NO_FORMAT,[Row+RowOffset__]);
                        end
                      else begin
                        Result:=FirstWallOrBoxOnGoalColumn<>0;
                        if not Result then
                           ErrorText__:=Format(TEXT_BOARD_ERROR_MISSING_WALL2+TEXT_BOARD_ERROR_ROW_NO_FORMAT,[Row+RowOffset__]);
                        end;
                      end
                   else if AllowEmptyRows then
                           Result:=True {allow empty rows}
                        else
                           ErrorText__:=Format(TEXT_BOARD_ERROR_MIN_LENGTH_FORMAT+TEXT_BOARD_ERROR_ROW_NO_FORMAT,[MIN_BOARD_WIDTH,Row+RowOffset__]);
                   end;
            end;
  except on E:Exception do Result:=SokUtil_.Error(E.Message,'IsALegalBoard');
  end;
end; {IsALegalBoard}

function IsALegalBoxSquare(BoardSquareValue__:TBoardSquare):Boolean;
begin
  Result :=(BoardSquareValue__ and (WALL+FLOOR+ILLEGAL_SQUARE))=(FLOOR);
end; {IsALegalBoxSquare}

function IsALegalPlayerSquare(BoardSquareValue__:TBoardSquare):Boolean;
begin
  Result :=(BoardSquareValue__ and (WALL+FLOOR))=(FLOOR);
end; {IsALegalBoxSquare}

function IsAMoveTextLine(const Text__:String):Boolean;
var i,j:Integer; Ch:Char; HasNonDigitChar:Boolean;
begin {Checks if the text contains legal move-characters only}
  i:=STRING_BASE; j:=StrLastCharIndex(Text__); HasNonDigitChar:=False;
  Result:=(StrTrimmedLength(Text__,i,j)>0) and (j<High(j));
  if Result and (i<j) and (Text__[i]=EMAIL_QUOTE_CH) then {skip leading '>' characters}
     repeat Inc(i);
     until  (i=j) or ((Text__[i]>SPACE) and (Text__[i]<>EMAIL_QUOTE_CH));
  if Result and (i<j) and (Text__[j]=BACKSLASH) then {skip trailing '\' characters, and any whitespace characters before them}
     repeat Dec(j);
     until  (i=j) or ((Text__[j]>SPACE) and (Text__[j]<>BACKSLASH));
  while Result and (i<=j) do begin
    Ch:=Text__[i]; Inc(i);
    Result:=AnsiPos(Ch,LEGAL_MOVE_CHARACTERS)>=STRING_BASE;
    if   Result then begin
         if (not HasNonDigitChar) and
            ((Ch<'0') or (Ch>'9')) and
            (Ch<>GROUP_BEGIN_CH) and
            (Ch<>GROUP_END_CH) then
            HasNonDigitChar:=True;
         end
    else Result:=Ch<=SPACE; {allow spaces between moves}
    end;
  if Result then Result:=HasNonDigitChar; {a text with digits only is not considered a proper moves-line; the number may be a title}
end; {IsAMoveTextLine}

function  IsASaveGameName(const Name__:String):Boolean;
begin
  Result:=StrBeginsWith(StrWithoutBrackets(Name__),SNAPSHOT_TYPE_NAME[stSaveGame]);
end; {IsASaveGameName}

function  IsASpecialSnapshotName__(const Name__:String; var SnapshotType__:TSnapshotType):Boolean;
var i:TSnapshotType;
begin
  for i:=stBestSolutionMoves to stBestSolutionMoves1 do
      if StrBeginsWith(Name__,SNAPSHOT_TYPE_NAME[i]) then begin
         Result:=True; SnapshotType__:=i; exit;
         end;
  Result:=False;
end; {IsASpecialSnapshotName}

function  IsSameBoardAsText (const BoardAsText1__,BoardAsText2__:TBoardAsText):Boolean;
begin
  with BoardAsText1__ do
    Result:=(Width =BoardAsText2__.Width)  and
            (Height=BoardAsText2__.Height) and
            (Board =BoardAsText2__.Board);
end;

function MakeBoardFromMoves(const Moves__:PHistoryMoves; Count__:Integer; ReverseMode__:Boolean; var BoardWidth__,BoardHeight__:Integer; var Board__:TBoard; var PlayerPos__:TColRow):Boolean;
var i,dx,dy,Index,Col,Row,Left,Top,Right,Bottom,Move,BoxCount,GoalCount:Integer;
    s:String; History:THistory; {L:TLevel;}

  function IncludeSquare(var Col__,Row__:Integer):Boolean;
  var i,j:Integer;
  begin
    Result:=True;

    if      Col__<Left then
            if        (Col__ >1)
                      or
                      ((Col__=1) and (Right=MAX_BOARD_WIDTH)) then Left:=Col__
            else if   Right<MAX_BOARD_WIDTH then begin
                      Inc(Col__); Left:=Col__; Inc(Right);
                      for i:=Right downto 1 do
                          for j:=Top to Bottom do Board__[i,j]:=Board__[Pred(i),j];
                      end
                 else Result:=False
    else if Col__>Right then
            if        (Col__ <MAX_BOARD_WIDTH)
                      or
                      ((Col__=MAX_BOARD_WIDTH) and (Left=1)) then Right:=Col__
            else if   Left>1 then begin
                      Dec(Left); Dec(Col__); Right:=Col__;
                      for i:=Left to MAX_BOARD_WIDTH do
                          for j:=Top to Bottom do Board__[i,j]:=Board__[Succ(i),j];
                      end
                 else Result:=False;
    if      Row__<Top then
            if        (Row__ >1)
                      or
                      ((Row__=1) and (Bottom=MAX_BOARD_HEIGHT)) then Top:=Row__
            else if   Bottom<MAX_BOARD_HEIGHT then begin
                      Inc(Row__); Top:=Row__; Inc(Bottom);
                      for i:=Left to Right do
                          for j:=Bottom downto 1 do Board__[i,j]:=Board__[i,Pred(j)];
                      end
                 else Result:=False
    else if Row__>Bottom then
            if        (Row__ <MAX_BOARD_HEIGHT)
                      or
                      ((Row__=MAX_BOARD_HEIGHT) and (Top=1)) then Bottom:=Row__
            else if   Top>1 then begin
                      Dec(Top); Dec(Row__); Bottom:=Row__;
                      for i:=Left to Right do
                          for j:=Top to MAX_BOARD_HEIGHT do Board__[i,j]:=Board__[i,Succ(j)];
                      end
                 else Result:=False;
  end;

begin {MakeBoardFromMoves}
  Result:=Moves__<>nil;
  if Result then begin
     for Col:=0 to MAX_BOARD_WIDTH+1 do
         for Row:=0 to MAX_BOARD_HEIGHT+1 do Board__[Col,Row]:=WALL;
     Col:=MAX_BOARD_WIDTH div 2; Row:=MAX_BOARD_HEIGHT div 2;
     Board__[Col,Row]:=FLOOR+PLAYER;
     Left:=Col; Right:=Col; Top:=Row; Bottom:=Row;
     if ReverseMode__ then begin
        Index:=1;
        while (Index<=Count__) and ((Moves__^[Index] and H_FLAG_JUMP)<>0) do Inc(Index); {skip initial jumps}
        while (Index<=Count__) and Result do begin {look for embedded jumps}
          Result:=(Moves__^[Index] and H_FLAG_JUMP)=0; {embedded jumps are illegal in a solution}
          Inc(Index);
          end;
        end;

     for Index:=1 to Count__ do
         if   Result then begin
              if not ReverseMode__ then begin
                 Move:=Moves__^[Index];
                 HistoryMoveToDxDy(Move,dx,dy);
                 end
              else begin
                 Move:=Moves__^[Succ(Count__)-Index];
                 HistoryMoveToDxDy(Move,dx,dy);
                 dx:=-dx; dy:=-dy;
                 end;
              Inc(Col,dx); Inc(Row,dy);

              if IncludeSquare(Col,Row) then begin
                 if (Move and H_FLAG_JUMP)=0 then begin {'0': don't create floors based on the player's jump-moves in a reverse mode game}
                    if   (Board__[Col,Row] and WALL)<>0 then begin
                         Inc(Board__[Col,Row],FLOOR-WALL);
                         if (Move and H_FLAG_BOX)<>0 then
                            Inc(Board__[Col,Row],BOX+BOX_START_POSITION);
                         end;

                    if   (Board__[Col,Row] and BOX)=0 then
                         if   (Move and H_FLAG_BOX)=0 then
                         else Result:=False
                    else if   (Move and H_FLAG_BOX)<>0 then begin
                              Dec(Board__[Col,Row],BOX);
                              Inc(Col,dx); Inc(Row,dy); {new box position}
                              if   IncludeSquare(Col,Row) then begin
                                   if   (Board__[Col,Row] and WALL)<>0 then
                                        Inc(Board__[Col,Row],FLOOR-WALL);
                                   if   (Board__[Col,Row] and BOX)=0 then
                                        Inc(Board__[Col,Row],BOX)
                                   else Result:=False;
                                   end
                              else Result:=False;
                              Dec(Col,dx); Dec(Row,dy); {[Col,Row] back to the new player position}
                              end
                         else Result:=False;
                    end;
                 end
              else Result:=False;
              end
         else break;

     if Result then begin
        dx:=-Max(0,Left-2); dy:=-Max(0,Top-2);
        for Col:=Left to Right do
            for Row:=Top to Bottom do
                if Result then begin
                   i:=Board__[Col,Row];
                   if (i and Box)<>0 then Inc(i,GOAL-BOX); {current box position is its goal position}
                   if (i and BOX_START_POSITION)<>0 then Inc(i,BOX-BOX_START_POSITION); {resurrect boxes at their start position}
                   Board__[Col+dx,Row+dy]:=i; {top-left justify the board}
                   end;

        BoxCount:=0; GoalCount:=0; PlayerPos__.x:=0; PlayerPos__.y:=0;
        for Col:=1 to Right do
            for Row:=1 to Bottom do begin
                if (Col>Right+dx) or (Row>Bottom+dy) then
                   Board__[Col,Row]:=WALL; {fill the gap after the top-left justification}
                i:=Board__[Col,Row];
                if (i and PLAYER)<>0 then with  PlayerPos__ do begin x:=Col; y:=Row; end;
                if (i and BOX   )<>0 then Inc(BoxCount);
                if (i and GOAL  )<>0 then Inc(GoalCount);
                end;

        BoardWidth__ :=Min(MAX_BOARD_WIDTH ,Right-Left+3);
        BoardHeight__:=Min(MAX_BOARD_HEIGHT,Bottom-Top+3);
        NormalizeBoard(False,True,True,False,False,BoardWidth__,BoardHeight__,Board__,PlayerPos__,History);
        TrimBoard(0,0,Board__,BoardWidth__,BoardHeight__,PlayerPos__);
        Result:=IsALegalBoard(Board__,BoardWidth__,BoardHeight__,
                              BoxCount,GoalCount,PlayerPos__,True,True,False,0,s);
        end;
{
     if CreateObject(otLevel,TNode(L)) then
        try     if L.BoardToTextLines(50,50,Board__) then
                   L.BoardAsTextLines.SaveToFile(StrWithTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'t1.$$$');
        finally L.Free;
        end;
}
     end;
end;

function  MakeBoardRectangular(BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Integer;
const BOARD_PIECES_EXCEPT_FLOOR=BOARD_PIECES-FLOOR;
type  TStackItem=packed record Square:TColRow; end;
      TStack=record
        Top:Integer;
        Items:array[0..MAX_BOARD_SIZE+1] of TStackItem;
      end;
var   Col, Row : Integer;
      Direction : TDirection;
      NeighborSquare : TColRow;
      Stack : TStack;

  function  Visit( Col__, Row__ : Integer ) : Boolean;
  begin
    Result := ( Board__[ Col__, Row__ ] and
                ( BOARD_PIECES_EXCEPT_FLOOR + PLAYER_LEGAL_MOVE ) ) = 0;
    if Result then begin
       Inc( Stack.Top );
       Stack.Items[ Stack.Top ].Square.X := Col__;
       Stack.Items[ Stack.Top ].Square.Y := Row__;
       Board__[ Col__, Row__ ] := WALL;
       end;
  end;

begin {Fills outer squares with walls so the board forms a rectangle}
  Result:=0;

  {calculate the player's reachable squares}
  Stack.Top := 0;
  for Col:=1 to BoardWidth__ do
      for Row:=1 to BoardHeight__ do begin
          Board__[Col,Row]:=Board__[Col,Row] and (not PLAYER_LEGAL_MOVE);
          if ( Board__[ Col, Row ] and PLAYER ) <> 0 then begin
             Inc( Stack.Top );
             Stack.Items[ Stack.Top ].Square.X := Col;
             Stack.Items[ Stack.Top ].Square.Y := Row;
             Inc( Board__[Col,Row], PLAYER_LEGAL_MOVE );
             end;
          end;
  while Stack.Top > 0 do begin
    Col := Stack.Items[ Stack.Top ].Square.X;
    Row := Stack.Items[ Stack.Top ].Square.Y;
    Dec( Stack.Top );
    for Direction:=Low(Direction) to High(Direction) do begin
        NeighborSquare.X := Col + DIRECTION_XY[ Direction, ColAxis ];
        NeighborSquare.Y := Row + DIRECTION_XY[ Direction, RowAxis ];
        if ( Board__[ NeighborSquare.X, NeighborSquare.Y ] and
             ( WALL + PLAYER_LEGAL_MOVE ) ) = 0 then begin
           Inc( Stack.Top );
           Stack.Items[ Stack.Top ].Square.X := NeighborSquare.X;
           Stack.Items[ Stack.Top ].Square.Y := NeighborSquare.Y;
           Inc( Board__[ NeighborSquare.X, NeighborSquare.Y ], PLAYER_LEGAL_MOVE );
           end;
        end;
    end;

  {flood fill all unreachable floor squares from the four sides of the board}
  Stack.Top := 0;
  for Col:=1 to BoardWidth__ do
      if BoardHeight__ >= 1 then begin
         Visit( Col, 1 );
         Visit( Col, BoardHeight__ );
         end;
  for Row:=1 to BoardHeight__ do
      if BoardWidth__ >= 1 then begin
         Visit( 1, Row );
         Visit( BoardWidth__, Row );
         end;

  while Stack.Top > 0 do begin
    Inc( Result );
    Col := Stack.Items[ Stack.Top ].Square.X;
    Row := Stack.Items[ Stack.Top ].Square.Y;
    Dec( Stack.Top );
    for Direction:=Low(Direction) to High(Direction) do
        Visit( Col + DIRECTION_XY[ Direction, ColAxis ],
               Row + DIRECTION_XY[ Direction, RowAxis ] );
    end;

  for Col:=1 to BoardWidth__ do
      for Row:=1 to BoardHeight__ do
          Board__[Col,Row]:=Board__[Col,Row] and (not PLAYER_LEGAL_MOVE);
end; {MakeBoardRectangular}

function  MetricsAsText(MoveCount__,PushCount__:Integer; const SecondaryScoreMetrics__:TSecondaryScoreMetrics):String; {throws EOutOfMemory}
begin
  with SecondaryScoreMetrics__ do
    Result:=Format('%d/%d/%d/%d/%d/%d',[MoveCount__,PushCount__,BoxLines,BoxChanges,PushingSessions,PlayerLines]);
end;

function MoveToChar(Direction__:TDirection; IsABoxMove__:Boolean; LowerCaseMoves__:Boolean):Char;
begin
  Result:=DIRECTION_TO_CHAR[Direction__]; {caution: 'DIRECTION_TO_CHAR' must be uppercase characters}
  if LowerCaseMoves__ and (not IsABoxMove__) then
     Result:=Char(Ord(Result)-Ord('A')+Ord('a'));
end; {MoveToChar}

function MovesToText(Moves__:PHistoryMoves; Count__,Top__:Integer; ReverseMode__,RleEnabled__,CombinedMoveFormattingEnabled__:Boolean; var MovesAsText__:String):Boolean; {throws EOutOfMemory}
const MOVE_SEPARATOR=H_MASK_MOVE_SEPARATOR or H_FLAG_JUMP;
var   i,Move:Integer; Rle:TRle;
begin {Converts moves from internal format to text}
  Result:=True; MovesAsText__:='';
  Rle:=TRle.Create;
  try
          Rle.RleEnabled:=RleEnabled__;

          if ReverseMode__ and (Top__>0) and
             ((Moves__[1] and H_FLAG_JUMP)=0) then
             Result:=Rle.Add(JUMP_BEGIN_CH) and                                 {in reverse mode, the game must begin with a jump, even if it's an empty one}
                     Rle.Add(JUMP_END_CH);

          if Result and (Count__=0) and (Top__>0) then
             Result:=Rle.Add(CURRENT_MOVE_CH);                                  {current position: *}

          for i:=1 to Top__ do begin
              Move:=Moves__[i];

              if Result and
                 ((Move             and MOVE_SEPARATOR)<>
                  (Moves__[Pred(i)] and MOVE_SEPARATOR))
                 and
                 (((Move and H_FLAG_JUMP)<>0)
                  or
                  ((i<Top__)
                   and
                   ((Move             and MOVE_SEPARATOR)=
                    (Moves__[Succ(i)] and MOVE_SEPARATOR))
                   )
                 ) then
                 if   (Move and H_FLAG_JUMP)=0 then
                      if   CombinedMoveFormattingEnabled__ then
                           Result:=Rle.Add(GROUP_BEGIN_CH)                      {start a combined move: (...)}
                      else {}
                 else      Result:=Rle.Add(JUMP_BEGIN_CH);                      {start a jump-move : [...]}

              if Result then {the move: u,d,l,r,U,D,L,R}
                 Result:=Rle.Add(MoveToChar(TDirection(Move and H_MASK_DIRECTION),(Move and H_FLAG_BOX)<>0,True));

              if Result and
                 ((i=Top__)
                  or
                  ((Move             and MOVE_SEPARATOR)<>
                   (Moves__[Succ(i)] and MOVE_SEPARATOR))
                 )
                 and
                 (((Move and H_FLAG_JUMP)<>0)
                  or
                  ((Move             and MOVE_SEPARATOR)=
                   (Moves__[Pred(i)] and MOVE_SEPARATOR))
                 ) then
                 if   (Move and H_FLAG_JUMP)=0 then
                      if   CombinedMoveFormattingEnabled__ then
                           Result:=Rle.Add(GROUP_END_CH)                        {end a combined move: (...)}
                      else {}
                 else      Result:=Rle.Add(JUMP_END_CH);                        {end a jump-move : [...]}

              if Result and (i=Count__) and (i<Top__) then
                 Result:=Rle.Add(CURRENT_MOVE_CH);                              {current position: *}
              end;

          if Result then Result:=Rle.Flush;

          if Result and (Rle.Position>0) then
             MovesAsText__:=System.Copy(Rle.Text,STRING_BASE,Rle.Position);

  finally Rle.Free;
  end;
end; {MovesToText}

function  NeighborSquare( Col__, Row__ : Integer; Direction__ : TDirection ) : TColRow;
begin
  Result.X := Col__ + DIRECTION_XY[ Direction__ , ColAxis ];
  Result.Y := Row__ + DIRECTION_XY[ Direction__ , RowAxis ];
end; {NeighborSquare}

function NormalizeBoard(MovePlayerAndBoxes__,
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
var Col,Row,QueueIndex,QueueTop,OriginalBoardWidth,OriginalBoardHeight,PlayerCount:Integer;
    Direction:TDirection;
    Queue:array[0..MAX_BOARD_SIZE] of TColRow;
    OriginalBoard:TBoard;
    Visited:TBoardOfBooleans;

  function FillTubes(MovePlayerAndBoxes__,ChangeImmovableBoxesOnNonGoalSquaresToWalls__,ChangeImmovableBoxesOnGoalSquaresToWalls__,StableNormalization__:Boolean):Integer;
  var Col,Row,BoxCount,BoxOnGoalCount,NeighborFloorCount,SquareValue:Integer;
      Again,DeadEnd:Boolean; Dir,Direction:TDirection;
      PlayerPos,NewBoxPos,NewPlayerPos:TColRow;
      BlockedAxes:TBoardAxesSet;
      IsACornerSquare:TBoardOfBooleans;

    function GetSquareValue(Col__,Row__,Distance__:Integer; Direction__:TDirection):Integer;
    begin {returns the value of the square 'Distance__' squares away from the square [Col__,Row__] in the given direction}
      Result:=Board__[Max(0,Min(MAX_BOARD_WIDTH +1,Col__+Distance__*DIRECTION_XY[Direction__,ColAxis])),
                      Max(0,Min(MAX_BOARD_HEIGHT+1,Row__+Distance__*DIRECTION_XY[Direction__,RowAxis]))];
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
      NeighborCol        :=Col__+DIRECTION_XY[Direction__,ColAxis];
      NeighborRow        :=Row__+DIRECTION_XY[Direction__,RowAxis];
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
                    ((GetSquareValue(Col__-DIRECTION_XY[Direction__,ColAxis],
                                     Row__-DIRECTION_XY[Direction__,RowAxis],
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
    BoxCount:=0; BoxOnGoalCount:=0; PlayerPos.X:=0; Direction:=Up;
    FillChar(IsACornerSquare,SizeOf(IsACornerSquare),0);
    {TubeFillingMoveCount:=0; TubeFillingPushCount:=0;} {player moves and box pushes}
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do begin
            SquareValue:=Board__[Col,Row];
            if (SquareValue and BOX       )<>0        then Inc(BoxCount );
            if (SquareValue and (BOX+GOAL))= BOX+GOAL then Inc(BoxOnGoalCount);
            if (SquareValue and PLAYER    )<>0        then with PlayerPos do begin X:=Col; Y:=Row; end;
            end;

    repeat
      Again:=False;
      for Col:=1 to BoardWidth__ do
          for Row:=1 to BoardHeight__ do begin
              SquareValue:=Board__[Col,Row];
              if (SquareValue and WALL)=0 then begin {for each floor-square...}
                 if StableNormalization__ then
                    Board__[Col,Row]:=SquareValue and (not ILLEGAL_SQUARE); {remove the old 'illegal square' flag, if any; this is necessary to guarantee a stable normalization; the original board may contain more accurate 'illegal square' flags}
                 NeighborFloorCount:=0; BlockedAxes:=[];
                 for  Dir:=Low(Dir) to High(Dir) do
                      if   (GetSquareValue(Col,Row,1,Dir) and WALL)=0 then begin
                           Inc(NeighborFloorCount); Direction:=Dir;
                           end
                      else Include(BlockedAxes,DIRECTION_TO_AXIS[Dir]);
                 IsACornerSquare[Col,Row]:=BlockedAxes=ALL_BOARD_AXES;
                 if   (BlockedAxes=ALL_BOARD_AXES) and ((SquareValue and (BOX+GOAL))=0) then {'True': the corner square is neither an empty goal square nor a goal square with a box}
                      Board__[Col,Row]:=Board__[Col,Row] or ILLEGAL_SQUARE;
                 if   ((SquareValue and BOX)<>0)
                      and
                      (BlockedAxes=ALL_BOARD_AXES) {the box is on a corner square and cannot move}
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
                         NeighborFloorCount:=NUMBER_OF_DIRECTIONS+2 {leave the square untouched; '+2': just to be sure to that the 'if NeighborFloorCount<=1' statement below will fail}
                      else if BlockedAxes=ALL_BOARD_AXES then
                              if NeighborFloorCount=2 then
                                 for  Dir:=Low(Dir) to High(Dir) do
                                      if IsABulge(Col,Row,Dir) then
                                         if      (GetSquareValue(Col,Row,1,Dir) and PLAYER)=0 then begin {'True': the neighbor square doesn't contain a player; fill it with a wall}
                                                 Board__        [Col+DIRECTION_XY[Dir,ColAxis],Row+DIRECTION_XY[Dir,RowAxis]]:=WALL; Again:=True; Inc(Result);
                                                 IsACornerSquare[Col+DIRECTION_XY[Dir,ColAxis],Row+DIRECTION_XY[Dir,RowAxis]]:=False;
                                                 end
                                         else if (Board__[Col,Row] and PLAYER)=0 then begin
                                                 Board__        [Col,Row]:=WALL; Again:=True; Inc(Result);
                                                 IsACornerSquare[Col,Row]:=False;
                                                 end;

                 if NeighborFloorCount<=1 then begin {'True': it's an empty floor-square surrounded by 3 or 4 walls, or it's a corner square with a box}
                    DeadEnd:=True; NewBoxPos.X:=0; NewPlayerPos.X:=0;
                    if (Col=PlayerPos.X) and (Row=PlayerPos.Y) then
                       if (NeighborFloorCount=0) or (not MovePlayerAndBoxes__) then
                          DeadEnd:=False {the player is on an isolated square, or moving player and boxes isn't allowed; don't change anything}
                       else begin        {try to move the player to the neighbor floor-square}
                          NewPlayerPos.X:=Col+DIRECTION_XY[Direction,ColAxis];
                          NewPlayerPos.Y:=Row+DIRECTION_XY[Direction,RowAxis];
                          if (Board__[NewPlayerPos.X,NewPlayerPos.Y] and BOX)<>0 then begin
                             NewBoxPos.X:=NewPlayerPos.X+DIRECTION_XY[Direction,ColAxis];
                             NewBoxPos.Y:=NewPlayerPos.Y+DIRECTION_XY[Direction,RowAxis];
                             DeadEnd:=((Board__[NewBoxPos.X,NewBoxPos.Y] and (WALL+BOX))=0) and {the box can be pushed forward}
                                      (BoxOnGoalCount<BoxCount);                {the position isn't a solution}
                             end;
                          end;
                    if DeadEnd then begin {dead end: place a wall on the square}
                       if NewBoxPos.X<>0 then begin {move a box}
                          if (Board__[NewPlayerPos.X,NewPlayerPos.Y] and GOAL)<>0 then Dec(BoxOnGoalCount);
                          Dec(Board__[NewPlayerPos.X,NewPlayerPos.Y],BOX);
                          Inc(Board__[NewBoxPos.X,NewBoxPos.Y],BOX);
                          if (Board__[NewBoxPos.X,NewBoxPos.Y] and GOAL)<>0 then Inc(BoxOnGoalCount);
                          {Inc(TubeFillingPushCount);}
                          end;
                       if NewPlayerPos.X<>0 then begin {move the player}
                          Dec(Board__[PlayerPos.X,PlayerPos.Y],PLAYER);
                          PlayerPos.X:=NewPlayerPos.X; PlayerPos.Y:=NewPlayerPos.Y;
                          Inc(Board__[PlayerPos.X,PlayerPos.Y],PLAYER);
                          {Inc(TubeFillingMoveCount);}
                          with History__ do begin
                            {the history isn't fully updated with statistics
                             like 'PushCount', 'PlayerLines' etc.;
                             only move directions and box-push flags are stored;
                            }
                            Inc(Count);
                            Moves[Count]:=Ord(Direction);
                            if NewBoxPos.X<>0 then Inc(Moves[Count],H_FLAG_BOX);
                            {updating the history 'Top' in the next code line is commented out
                             because it wasn't there from the beginning, and even though the
                             update seems natural, it's unknown whether there is code somewhere
                             else, which depends on 'Top' not being updated;
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
  {fills unnecessary squares which are:
   * illegal box squares
   * unnecessary for moving the player around between all legal box squares (may destroy symmetry, if any)
   * inside dead end rooms (a time-consuming calculation compared to the other methods)
  }
  type
    TBoardSquareSet = record
      Count         : Integer;
      Squares       : array[0..MAX_BOARD_SIZE+1] of TColRow;
    end;
    TStackItem      = packed record BoxPos,PlayerPos:TColRow; end;
    TStack          = record
      Top           : Integer;
      Items         : array[0..MAX_BOARD_SIZE*NUMBER_OF_DIRECTIONS+1] of TStackItem;
      end;
    TVisited        = array[0..MAX_BOARD_WIDTH,0..MAX_BOARD_HEIGHT,TDirection] of Boolean;
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
      if   (Board__[FromSquare__.X,FromSquare__.Y] and WALL)=0 then begin       {'True': the 'FromSquare__' isn't a wall square}
           Queue.Top:=1; Queue.Items[1].Square:=FromSquare__;                   {put the 'FromSquare__' on the queue}
           BoardTimeStamps__.Squares[FromSquare__.X,FromSquare__.Y]:=BoardTimeStamps__.Timestamp; {mark the 'FromSquare__' as visited}
           end
      else Queue.Top:=0;                                                        {'0': the 'FromSquare__' is a wall square, hence, the function has nothing to do}

      PlayerAccessArea__.Count:=0;
      if (ToSquare__.X=0) and (Queue.Top<>0) then with PlayerAccessArea__ do begin {'True': the function calculates the player access area including the square 'FromSquare__'}
         Inc(Count); Squares[Count]:=FromSquare__;
         end;
      Inc(PlayerMovesCount);

      {perform a breadth-first search for a path from 'FromSquare__' to 'ToSquare__'}
      while (Queue.Bottom<Queue.Top) and {while there are more items on the queue}
            (BoardTimeStamps__.Squares[ToSquare__.X,ToSquare__.Y]<>BoardTimeStamps__.TimeStamp) do begin {while the destination square hasn't been visited}
            Inc(Queue.Bottom);                                                  {advance to the next unprocessed item on the queue}
            PlayerSquare:=Queue.Items[Queue.Bottom].Square;                     {get the next item from the queue}

            for Direction:=Low(Direction) to High(Direction) do begin
                NeighborSquare.X:=PlayerSquare.X+DIRECTION_XY[Direction,ColAxis];
                NeighborSquare.Y:=PlayerSquare.Y+DIRECTION_XY[Direction,RowAxis];

                if (BoardTimeStamps__.Squares[NeighborSquare.X,NeighborSquare.Y]<>BoardTimeStamps__.TimeStamp) and {is the neighbor square an unvisited square?}
                   ((Board__[NeighborSquare.X,NeighborSquare.Y] and (WALL))=0) then begin {is the neighbor square a floor square or a wall?}
                   Inc(Queue.Top);                                              {put the neighbor square on the queue}
                   Queue.Items[Queue.Top].Square:=NeighborSquare;
                   BoardTimeStamps__.Squares[NeighborSquare.X,NeighborSquare.Y]:=BoardTimeStamps__.TimeStamp; {mark the neighbor square as visited}
                   if ToSquare__.X=0 then with PlayerAccessArea__ do begin      {'True': the function calculates the player access area including the square 'FromSquare__'}
                      Inc(Count); Squares[Count]:=NeighborSquare;
                      end;
                   Inc(PlayerMovesCount);
                   end;
                end;
            end;
      if   ToSquare__.X=0 then {'True': the function calculates the player access area including the square 'FromSquare__'}
           Result:=PlayerAccessArea__.Count<>0 {this is always true}
      else Result:=BoardTimeStamps__.Squares[ToSquare__.X,ToSquare__.Y]=BoardTimeStamps__.TimeStamp; {'True': there is a player path between the given squares}
    end;

    function PushOrPullBoxesAroundOnTheBoard( PushBoxes__ : Boolean; var Board__ : TBoard; var Stack__ : TStack; var Visited__ : TVisited; var BoardTimeStamps__ : TBoardTimeStamps; var PlayerAccessArea__:TBoardSquareSet ) : Integer;
    var
      BoxSquare,BoxToSquare,PlayerFromSquare,PlayerSquare,PlayerToSquare:TColRow;
      Direction:TDirection;
      //PlayerAccessArea:TBoardSquareSet;
    begin
      {push/pull boxes around on the board until all reachable squares have been visited from all directions.
       preconditions:
       * the start positions for the boxes have been pushed on the stack.
       * the 'Visited__' area has been properly initialized to match the start positions pushed on the stack.
      }
      while Stack__.Top<>0 do begin
        BoxSquare   :=Stack__.Items[Stack__.Top].BoxPos;                        {pop box square and player square from the stack}
        PlayerSquare:=Stack__.Items[Stack__.Top].PlayerPos;
        Dec(Stack__.Top);
        Inc(Board__[BoxSquare.X,BoxSquare.Y],WALL);                             {block the box square by putting a wall on the square}

        for Direction:=Low(Direction) to High(Direction) do begin               {try to pull the box from each direction}
            BoxToSquare   .X:=BoxSquare  .X+DIRECTION_XY[Direction,ColAxis];    {if possible, the box is pulled to this square; the player starts here}
            BoxToSquare   .Y:=BoxSquare  .Y+DIRECTION_XY[Direction,RowAxis];

            if PushBoxes__ then begin
               PlayerFromSquare.X:=BoxSquare.X-DIRECTION_XY[Direction,ColAxis]; {the player starts here before the push}
               PlayerFromSquare.Y:=BoxSquare.Y-DIRECTION_XY[Direction,RowAxis];

               if (not Visited__[BoxToSquare.X,BoxToSquare.Y,Direction])        {is it an unvisited square from this direction?}
                  and
                  ((Board__[BoxToSquare     .X,BoxToSquare     .Y] and (WALL+ILLEGAL_SQUARE))=0) {is the square 'push-box-to-square' free?}
                  and
                  ((Board__[PlayerFromSquare.X,PlayerFromSquare.Y] and (WALL))=0) {is 'Player-push-from-square' free?}
                  and
                  CalculatePlayerPathExistsOrCalculatePlayerAccessArea(PlayerSquare,PlayerFromSquare,BoardTimeStamps__,PlayerAccessArea__) then begin {can the player reach 'Player-push-from-square' from its current position next to the box?}
                  Inc(Stack__.Top);                                             {put the new boxposition/playerposition on the stack}
                  Stack__.Items[Stack__.Top].BoxPos   :=BoxToSquare;
                  Stack__.Items[Stack__.Top].PlayerPos:=BoxSquare;
                  Visited__[BoxToSquare.X,BoxToSquare.Y,Direction]:=True;       {mark that the square has been visited from this direction}
                  end;
               end
            else begin
               PlayerToSquare.X:=BoxToSquare.X+DIRECTION_XY[Direction,ColAxis]; {the player ends here after the pull}
               PlayerToSquare.Y:=BoxToSquare.Y+DIRECTION_XY[Direction,RowAxis];

               if (not Visited__[BoxToSquare.X,BoxToSquare.Y,Direction])        {is it an unvisited square from this direction?}
                  and
                  ((Board__[BoxToSquare   .X,BoxToSquare   .Y] and (WALL))=0)   {is the square 'Player-pull-from-square' free?}
                  and
                  ((Board__[PlayerToSquare.X,PlayerToSquare.Y] and (WALL))=0)   {is 'Player-pull-to-square' free?}
                  and
                  CalculatePlayerPathExistsOrCalculatePlayerAccessArea(PlayerSquare,BoxToSquare,BoardTimeStamps__,PlayerAccessArea__) then begin {can the player reach 'Player-pull-from-square' from its current position next to the box?}
                  Inc(Stack__.Top);                                             {put the new boxposition/playerposition on the stack}
                  Stack__.Items[Stack__.Top].BoxPos   :=BoxToSquare;
                  Stack__.Items[Stack__.Top].PlayerPos:=PlayerToSquare;
                  Visited__[BoxToSquare.X,BoxToSquare.Y,Direction]:=True;       {mark that the square has been visited from this direction}
                  end;
               end;
            end;

        Dec(Board__[BoxSquare.X,BoxSquare.Y],WALL);                             {remove the wall from the board again}
        end;
      Result := 0;
    end;

    function CalculateIllegalOrUnreachableBoxSquares(PushBoxes__,InitializeIllegalSquares__:Boolean; var BoardTimeStamps__ : TBoardTimeStamps; var PlayerAccessArea__:TBoardSquareSet):Integer; {returns the number of legal box squares}
    {'PushBoxes__' = 'True'  : finds box-reachable squares by pushing boxes around on the board, starting from the box  squares
     'PushBoxes__' = 'False' : finds box-legal     squares by pulling boxes around on the board, starting from the goal squares
     if 'InitializeIllegalSquares__' is 'True' then any existing 'ILLEGAL_SQUARE' flags are reset for all board squares
     returns the number of legal box squares (contrary to what the function name suggests)
     precondition: when 'PushBoxes__' is 'True', then any 'ILLEGAL_SQUARE' flags on the board must be valid since they are taken into account by the search
    }
    var
      Col,Row:Integer;
      HasBeenVisited:Boolean;
      Direction:TDirection;
      PlayerSquare:TColRow;
      Stack:TStack;
      Visited:TVisited;
    begin
      Result:=0;
      FillChar(Visited,SizeOf(Visited),0); {initialize the box-reachable squares}
      BoardTimeStamps__.Timestamp:=High(BoardTimeStamps__.Timestamp); {initialize the 'player path exists?' searches}

      {prepare to push/pull boxes around on the board}
      Stack.Top:=0;
      for Col:=1 to BoardWidth__ do
          for Row:=1 to BoardHeight__ do
              if (PushBoxes__       and ((Board__[Col,Row] and BOX )<>0))             {'True': push boxes, and it's a box  square}
                 or
                 ((not PushBoxes__) and ((Board__[Col,Row] and GOAL)<>0)) then begin  {'True': pull boxes, and it's a goal square}
                 for Direction:=Low(Direction) to High(Direction) do begin
                     PlayerSquare.X:=Col+DIRECTION_XY[Direction,ColAxis];
                     PlayerSquare.Y:=Row+DIRECTION_XY[Direction,RowAxis];
                     if (Board__[PlayerSquare.X,PlayerSquare.Y] and (WALL+FLOOR))=FLOOR then begin {'True': the goal's neighbor square in this direction is a player-reachable floor square}
                        Inc(Stack.Top);                                         {put the tuple [goal square, empty floor neighbor square] on the stack}
                        with Stack.Items[Stack.Top] do begin
                             BoxPos.X:=Col; BoxPos.Y:=Row;
                             PlayerPos:=PlayerSquare;
                             end;
                        if   PushBoxes__ then
                             Visited[Col,Row,OPPOSITE_DIRECTION[Direction]]:=True {'Visited' is set as if the box has been pushed from the neighboring 'PlayerSquare' to the square with coordinates [Col,Row]}
                        else Visited[Col,Row,Direction]:=True;                  {'Visited' is set as if the box has been pulled in the direction 'Direction' to the square, i.e., the player is located at the next square in this direction}
                        end;
                     end;
                 end;

      {push/pull boxes around on the board until all reachable squares have been visited from all directions}
      PushOrPullBoxesAroundOnTheBoard( PushBoxes__, Board__, Stack, Visited, BoardTimeStamps__, PlayerAccessArea__ );

      {mark the illegal floor squares, i.e., box-unreachable floors (calculated by pushing boxes around),
       or the box-illegal floors without a push-path to a goal (calculated by pulling boxes around,
       starting from the goal squares)}
      for Col:=1 to BoardWidth__ do
          for Row:=1 to BoardHeight__ do
              if (Board__[Col,Row] and WALL)=0 then begin                       {'True': it's a floor square}
                 if  InitializeIllegalSquares__ then                            {'True': reset any existing illegal squares flags}
                     Board__[Col,Row]:=Board__[Col,Row] and (not ILLEGAL_SQUARE);
                 HasBeenVisited:=False;
                 for Direction:=Low(Direction) to High(Direction) do
                     if Visited[Col,Row,Direction] then begin                   {'True': it's a legal box square}
                        HasBeenVisited:=True;
                        Inc(Result);                                            {return the number of legal floor squares}
                        break;                                                  {'break': quick-and-dirty exit the loop as soon as it's known that the square has been visited}
                        end;
                 if  (not HasBeenVisited) and                                   {'True': it's an illegal or unreachable square for boxes}
                     ((Board__[Col,Row] and ILLEGAL_SQUARE)=0) then begin       {'True': the square hasn't been marked as being an illegal square yet}
                     Inc(Board__[Col,Row],ILLEGAL_SQUARE);
                     {Inc(Result);}                                             {return the number of (newly found) box-illegal or box-unreachable floor squares}
                     {Board__[Col,Row]:=Board__[Col,Row] or GOAL; // test}
                     end;
                 end;
    end; {CalculateIllegalOrUnreachableBoxSquares}

    function CalculateBoxReachableSquares( PushBoxes__ : Boolean; BoxPos__ : TColRow; PlayerNeighborSquareDirection__ : TDirection; var BoardTimeStamps__ : TBoardTimeStamps; var PlayerAccessArea__:TBoardSquareSet ) : Integer;
    var
      Col,Row:Integer;
      Direction:TDirection;
      Stack:TStack;
      Visited:TVisited;
    begin {preconditions: the box square is a legal box-reachable square, and the player square is a neighbor floor square}
      Result:=0;
      FillChar(Visited,SizeOf(Visited),0); {initialize the box-reachable squares}
      BoardTimeStamps__.Timestamp:=High(BoardTimeStamps__.Timestamp); {initialize the 'player path exists?' searches}
      Stack.Top := 1;
      Stack.Items[1].BoxPos := BoxPos__;
      Stack.Items[1].PlayerPos := SokFile_.NeighborSquare( BoxPos__.X, BoxPos__.Y, PlayerNeighborSquareDirection__ );

      if   PushBoxes__ then
           Visited[BoxPos__.X,BoxPos__.Y,OPPOSITE_DIRECTION[PlayerNeighborSquareDirection__]]:=True {'Visited' is set as if the box has been pushed from the neighboring 'PlayerSquare' to the square with coordinates [Col,Row]}
      else Visited[BoxPos__.X,BoxPos__.Y,PlayerNeighborSquareDirection__]:=True;                    {'Visited' is set as if the box has been pulled in the direction 'Direction' to the square, i.e., the player is located at the next square in this direction}

      {push/pull boxes around on the board until all reachable squares have been visited from all reachable directions}
      PushOrPullBoxesAroundOnTheBoard( PushBoxes__, Board__, Stack, Visited, BoardTimeStamps__, PlayerAccessArea__ );

      {mark the reachable box squares}
      for Col:=1 to BoardWidth__ do
          for Row:=1 to BoardHeight__ do
              if (Board__[Col,Row] and WALL)=0 then begin                       {'True': it's a floor square}
                 BoardTimeStamps__.Squares[ Col, Row ] := 0;
                 for Direction:=Low(Direction) to High(Direction) do
                     if Visited[Col,Row,Direction] then begin                   {'True': it's a reachabled box square}
                        BoardTimeStamps__.Squares[ Col, Row ] := BoardTimeStamps__.TimeStamp;
                        Inc(Result);                                            {return the number of reachable box  squares}
                        break;                                                  {'break': quick-and-dirty exit the loop as soon as it's known that the square has been visited}
                        end;
                 end;
    end;

    function HasALegalBoxSquareNeighbor(Col__,Row__:Integer):Boolean;
    var Direction:TDirection; NeighborSquare:TColRow;
    begin
      Result:=False;
      for Direction:=Low(Direction) to High(Direction) do begin
          NeighborSquare.X:=Col__+DIRECTION_XY[Direction,ColAxis];
          NeighborSquare.Y:=Row__+DIRECTION_XY[Direction,RowAxis];
          if (Board__[NeighborSquare.X,NeighborSquare.Y] and (WALL+FLOOR+ILLEGAL_SQUARE))=(WALL+FLOOR) then begin
             Result:=True; exit; {'exit': quick and dirty exit when found}
             end;
          end;
    end;

    function IsACornerSquare(Col__,Row__:Integer):Boolean;
    var Direction:TDirection; BlockedAxes:TBoardAxesSet;
    begin
      BlockedAxes:=[];
      for Direction:=Low(Direction) to High(Direction) do
          if (Board__[Col__+DIRECTION_XY[Direction,ColAxis],Row__+DIRECTION_XY[Direction,RowAxis]] and (WALL+FLOOR))=WALL then
             Include(BlockedAxes,DIRECTION_TO_AXIS[Direction]);
      Result:=BlockedAxes=ALL_BOARD_AXES;
    end;

    function IsAMemberOfBoardSquareSet(Col__,Row__:Integer; const BoardSquareSet__:TBoardSquareSet):Boolean;
    var Index:Integer;
    begin
      Result:=False;
      for Index:=1 to BoardSquareSet__.Count do with BoardSquareSet__.Squares[Index] do
          if (Col__=X) and (Row__=Y) then begin
             Result:=True; exit; {'exit': quick and dirty exit when found}
             end;
    end;

    function  FillDeadEndRooms( var BoardTimeStamps__ : TBoardTimeStamps; var PlayerAccessArea__:TBoardSquareSet ) : Integer;
    var Col, Row, Col2, Row2, Count, Index : Integer;
        Direction, DeadEndRoomDirection : TDirection;
        Axis, OpenAxis : TBoardAxis;
        NeighborSquare1, NeighborSquare2, BoxSquare, PlayerSquare, WallSquare : TColRow;
    begin // fills dead end rooms with walls. an example room:
          // ##-##   <-- the floor square is a gate square
          // #---#
          // #---#
          // #####
          // a dead end room is an empty room which only is accessible via a gate square,
          // i.e., a square which - when occupied by a box - splits the board in separate
          // rooms for the player.
          //
          // if the player happens to be located on the gate square, then the
          // room isn't classified as a dead end room by this calculation

      Result := 0; 
      if PlayerCount = 1 then begin // otherwise, a more involved logic is required to check whether a box can be pushed from one side only
         BoardTimeStamps__.Timestamp := High( BoardTimeStamps__.Timestamp ); // initialize the "player path exists?" searches
         WallSquare.X:=0; WallSquare.Y:=0; // using [0,0] as destination square makes the function "CalculatePlayerPathExistsOrCalculatePlayerAccessArea" calculate a player access area
         DeadEndRoomDirection := Low( Direction ); // initialize it to avoid compiler warnings about an uninitialized variable
         OpenAxis := Low( Axis ); // initialize it to avoid compiler warnings about an uninitialized variable

         // find the player square
         PlayerSquare.X := 0; PlayerSquare.Y := 0;
         for Col:=1 to BoardWidth__ do // for each square on the board
             for Row:=1 to BoardHeight__ do
                 if ( Board__[ Col, Row ] and PLAYER ) <> 0 then begin
                    PlayerSquare.X := Col; PlayerSquare.Y := Row;
                    end;

         // visit all squares on the board, looking for gate squares
         for Col:=1 to BoardWidth__ do
             for Row:=1 to BoardHeight__ do
                 if IsALegalBoxSquare(Board__[Col,Row]) and // a box-reachable floor square, not a wall square
                    ( ( Board__[ Col, Row ] and PLAYER ) = 0 ) then begin
                    // check if the square is a gate square
                    BoxSquare.X := Col; BoxSquare.Y := Row;
                    Count := -1;
                    for Axis := Low( Axis ) to High (Axis ) do
                        if not IsABlockedBoxSquareAlongAxis( Col, Row, AXIS_TO_DIRECTION[ Axis ], Board__ ) then begin
                           Inc( Count ); OpenAxis := Axis;
                           end;
                    if Count = 0 then begin // 'True': a box on this square can move along one axis only, i.e., the square may be a gate square
                       for Direction := Low( Direction) to High( Direction ) do
                           if ( DIRECTION_TO_AXIS[ Direction ] = OpenAxis ) and
                              ( Count = 0 ) then begin // "0": maybe the square is a dead square
                              // find player-reachable squares on this side of the gate candidate square,
                              // with a box on the square and with the player starting on the neighbor square in the given direction
                              NeighborSquare1 := SokFile_.NeighborSquare( Col, Row, Direction );
                              Inc( Board__[ Col, Row ], WALL ); // temporarily, put a wall on the gate candidate square
                              CalculatePlayerPathExistsOrCalculatePlayerAccessArea( NeighborSquare1, WallSquare, BoardTimeStamps__, PlayerAccessArea__); // calculates the player access area
                              Dec( Board__[ Col, Row ], WALL ); // remove the temporary wall from the gate candidate square

                              if PlayerAccessArea__.Count > 0 then begin // 'True': the neighbor square in the given direction, is a floor square, not a wall square. this should always be true here
                                 // check if the player can reach both sides of a box on the gate candidate square, in which case the square isn't a gate square
                                 NeighborSquare2 := SokFile_.NeighborSquare( Col, Row, OPPOSITE_DIRECTION[ Direction ] ); // the neighbor square in the opposite direction
                                 if ( BoardTimeStamps__.Squares[ NeighborSquare2.X, NeighborSquare2.Y ] = BoardTimeStamps__.Timestamp ) then begin
                                    // the player can get from one side of a box on the candidate square to the other side.
                                    // this means, the square isn't a gate square and it's not a dead square according to this calculation
                                    Inc( Count ); // "not 0": not a dead square
                                    end
                                 else begin
                                    // with a box on the gate candidate square, the player cannot get from one side of the box to the other side.
                                    // this means, the square is a gate square which splits the board in separate rooms for the player
                                    if BoardTimeStamps__.Squares[ PlayerSquare.X, PlayerSquare.Y ] = BoardTimeStamps__.Timestamp then begin
                                       // the player is on the calculated side of the gate square. check if the player can push a box on the gate square to a goal.
                                       // calculate the box push-reachable squares, starting with the box on the gate square and the player on the given neighbor square
                                       CalculateBoxReachableSquares( True, BoxSquare, Direction, BoardTimeStamps__, PlayerAccessArea__ );
                                       // check if one of the box push-reachable squares is a goal square
                                       for Col2 := 1 to BoardWidth__ do
                                           for Row2 := 1 to BoardHeight__ do
                                               if ( ( Board__[ Col2, Row2 ] and GOAL ) <> 0 ) and
                                                  ( BoardTimeStamps__.Squares[ Col2, Row2 ] = BoardTimeStamps__.TimeStamp ) then begin // 'True': the box can reach a goal
                                                  Inc( Count ); // "not 0": not a dead square
                                                  break; // quick-and-dirty exit loop when found
                                                  end;
                                       end
                                    else begin
                                       // the player isn't on the calculated side of the gate square.
                                       // (this doesn't entail that the player is on the other side;
                                       // the entire area around the gate square may be disconnected from the player access area.)

                                       // check whether the room on the calculated side of the gate square is an empty room
                                       for Index := 1 to PlayerAccessArea__.Count do with PlayerAccessArea__.Squares[ Index ] do
                                           if   ( Board__[ X, Y ] and ( BOX + GOAL + PLAYER ) ) <> 0  then begin // 'True': not an empty room
                                                Inc( Count ); // "not 0": not a dead square
                                                break; // quick-and-dirty exit loop when found
                                                end;
                                       if Count = 0 then // 'True': an empty room
                                          DeadEndRoomDirection := Direction; // remember that this side of the gate square is an empty room
                                       end;
                                    end;
                                 end
                              else Inc( Count ); // there are no player-reaachable squares on this side of the box on the square. this shouldn't happen here. bail out
                              end;


                       if Count = 0 then begin
                          // there is a dead end room on one side of the gate square. fill the room with walls, and maybe the gate square too.
                          // recalculate the squares in the dead end room
                          NeighborSquare1 := SokFile_.NeighborSquare( Col, Row, DeadEndRoomDirection );
                          Inc( Board__[ Col, Row ], WALL ); // temporarily, put a wall on the gate square
                          CalculatePlayerPathExistsOrCalculatePlayerAccessArea( NeighborSquare1, WallSquare, BoardTimeStamps__, PlayerAccessArea__);
                          Dec( Board__[ Col, Row ], WALL ); // remove the temporary wall from the gate square

                          // the gate square itself can be filled with a wall if the square is
                          // completely surrounded by walls along all axes except the open axis.
                          // an example where the gate square must not be filled with a wall:
                          // ##--   <-- the leftmost floor square is a gate square which must not be filled with a wall
                          // #--##
                          // #---#
                          // #####
                          //
                          for Direction := Low( Direction ) to High( Direction ) do
                              if ( Board__[ Col + DIRECTION_XY[ Direction, ColAxis ], Row + DIRECTION_XY[ Direction, RowAxis ] ] and WALL ) <> 0 then // 'True': a wall square neighbor in this direction
                                 Inc( Count ); // count wall square neighbors

                          if Count = DIRECTION_COUNT - 2 then begin
                             // the gate square is surrounded by walls on all sides, except along the open axis.
                             // fill the gate square with a wall
                             Board__[ Col, Row ] := WALL;
                             Inc( Result ); // count filled squares
                             end;

                          // fill the room with walls
                          for Index := 1 to PlayerAccessArea__.Count do with PlayerAccessArea__.Squares[ Index ] do begin
                              Board__[ X, Y ] := WALL;
                              Inc( Result ); // count filled squares
                              end;
                          end;
                       end;
                    end;
         end;
    end; // FillDeadEndRooms

  begin {FillUnnecessaryIllegalBoxSquares}
    Result:=0; TimeMS:=GetTimeMS;

    PlayerMovesCount:=0;
    CalculateIllegalOrUnreachableBoxSquares(False,True,BoardTimeStamps,PlayerAccessArea);  {'False' parameter: find box-illegal     squares by pulling boxes around on the board, starting from goal squares}
    CalculateIllegalOrUnreachableBoxSquares(True,False,BoardTimeStamps,PlayerAccessArea);  {'True'  parameter: find box-unreachable squares by pushing boxes around on the board, starting from box  squares}

    BoardTimeStamps.Timestamp:=High(BoardTimeStamps.Timestamp); {initialize the 'calculate player access area' searches}
    ToSquare.X:=0; ToSquare.Y:=0; {using [0,0] as destination square makes the function 'CalculatePlayerPathExistsOrCalculatePlayerAccessArea' calculate a player access area}

    {block all legal box squares on the board by putting walls on them}
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do
            if      (Board__[Col,Row] and (WALL+FLOOR+ILLEGAL_SQUARE))=FLOOR then
                    Inc(Board__[Col,Row],WALL) {block all legal box squares on the board by putting walls on them}
            else if (Board__[Col,Row] and (WALL+FLOOR))=(WALL+FLOOR) then
                    Board__[Col,Row]:=WALL; {the current function requires that the input board doesn't contain squares marked as both walls and floors; should such squares exist, they are changed to pure walls here}

    for Col:=1 to BoardWidth__ do {for each square on the board}
        for Row:=1 to BoardHeight__ do
            if (Board__[Col,Row] and (WALL+FLOOR+ILLEGAL_SQUARE))=(FLOOR+ILLEGAL_SQUARE) then begin {'True': it's an unvisited illegal box square}
               FromSquare.X:=Col; FromSquare.Y:=Row;
               if CalculatePlayerPathExistsOrCalculatePlayerAccessArea(FromSquare,ToSquare,BoardTimeStamps,PlayerAccessArea) then begin

                  {calculate the set of floors in the player access area having legal box square neighbors}
                  SquaresWithALegalBoxSquareNeighbor.Count:=0;
                  for Index:=1 to PlayerAccessArea.Count do with PlayerAccessArea.Squares[Index] do
                      if HasALegalBoxSquareNeighbor(X,Y) then begin
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
                                if  ((Board__[X,Y] and WALL)=0) and {'True': the square hasn't already been turned into a wall}
                                    IsACornerSquare(X,Y) and
                                   (not IsAMemberOfBoardSquareSet(X,Y,SquaresWithALegalBoxSquareNeighbor)) then begin
                                   {try to fill the corner square with a wall,
                                    and check if it affects the player's ability
                                    to reach all the floor squares having
                                    adjacent legal box squares
                                   }
                                   Inc(Board__[X,Y],WALL); {try to fill the corner square with a wall}

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
                                             (BoardTimeStamps.Squares[SquaresWithALegalBoxSquareNeighbor.Squares[i].X,
                                                                      SquaresWithALegalBoxSquareNeighbor.Squares[i].Y]
                                              <>
                                              BoardTimeStamps.TimeStamp
                                             )
                                             then
                                             {the corner square affects the connectivity; remove the wall from the square, if it still is there}
                                             Board__[X,Y]:=Board__[X,Y] and (not WALL);

                                      if (Board__[X,Y] and WALL)<>0 then {'True': the corner square doesn't affect the connectivity between the floor squares having box-legal neighbor squares}
                                         {check that all players, if any, in the
                                          access area still can reach the floors
                                          with box-legal neighbor squares
                                         }
                                         for i:=1 to PlayerAccessArea.Count do
                                             if (i<>Index) and {'True': this is a different square than the one currently under investigation; the current square was blocked by a wall and isn't a member of the new access area}
                                                ((Board__[PlayerAccessArea.Squares[i].X,PlayerAccessArea.Squares[i].Y] and PLAYER)<>0) and {'True': there is a player on this square in the access area}
                                                {'BoardTimeStamps' can be used for set membership here instead of the function 'IsAMemberOfBoardSquareSet'}
                                                {not IsAMemberOfBoardSquareSet(SquaresWithALegalBoxSquareNeighbor.Squares[i].X,SquaresWithALegalBoxSquareNeighbor.Squares[i].Y,NewPlayerAccessArea)}
                                                (BoardTimeStamps.Squares[PlayerAccessArea.Squares[i].X,PlayerAccessArea.Squares[i].Y]
                                                 <>
                                                 BoardTimeStamps.TimeStamp) then
                                                {filling the corner square blocks the player's access
                                                 to the floors with box-legal neighbor squares;
                                                 remove the wall from the square, if it still is there;
                                                }
                                                Board__[X,Y]:=Board__[X,Y] and (not WALL);

                                      if ((Board__[X,Y] and WALL  )<>0) and {'True': the corner square can be filled with a wall without affecting the connectivity between the box-legal squares and the players in the access area, if any}
                                         ((Board__[X,Y] and PLAYER)<>0) then begin {'True': there is a player on the corner square}
                                         {find an empty neighbor square and move the player to that square;
                                          this isn't fully implemented; if there are more than
                                          one player on the board, it will fail if all
                                          neighboring empty squares are occupied by other players;
                                         }
                                         Dec(Board__[X,Y],WALL); {remove the wall; it will be put back if the player may and can move from the corner square to an adjacent square}
                                         if MovePlayerAndBoxes__ then
                                            for Direction:=Low(Direction) to High(Direction) do begin
                                                NeighborSquare.X:=X+DIRECTION_XY[Direction,ColAxis];
                                                NeighborSquare.Y:=Y+DIRECTION_XY[Direction,RowAxis];
                                                if ((Board__[NeighborSquare.X,NeighborSquare.Y] and (WALL+FLOOR+PLAYER))=FLOOR) and {'True': the neighbor square is an empty floor square}
                                                   ((Board__[X,Y] and WALL)=0) and {'True': the player hasn't been moved already in one of the other directions}
                                                   (History__.Count<High(History__.Moves)) {'True': making one more move doesn't overflow the move history}
                                                   then begin
                                                   Inc(Board__[X,Y],WALL); {put the wall back on the unnecessary corner square}
                                                   Dec(Board__[X,Y],PLAYER); {remove the player from the corner square}
                                                   Inc(Board__[NeighborSquare.X,NeighborSquare.Y],PLAYER); {move the player to the neighbor square}
                                                   Inc(History__.Count); {update the move history}
                                                   History__.Moves[History__.Count]:=Ord(Direction);
                                                   {updating the history 'Top' in the next code line is commented out
                                                    because it wasn't there from the beginning, and even though the
                                                    update seems natural, it's unknown whether there is code somewhere
                                                    else, which depends on 'Top' not being updated;
                                                   }
                                                   {if History__.Count>History__.Top then History__.Top:=History__.Count;}
                                                   end;
                                                end;
                                         end;

                                      if (Board__[X,Y] and WALL)<>0 then begin {'True': turning the floor square into a wall succeeded}
                                         Board__[X,Y]:=WALL; {remove the 'FLOOR' tag from the corner square; it's a wall now}
                                         Again:=True; {try again, until no more squares in the current player access area can be filled with walls}
                                         Inc(Result); {count the number of filled unnecessary illegal box squares}
                                         end;
                                      end;
                                   end;

                     until  not Again; {until all squares in the player access area have been investigated, and no more of them can be turned into walls}

                  {mark all squares in the current player access area as visited}
                  for Index:=1 to PlayerAccessArea.Count do with PlayerAccessArea.Squares[Index] do
                      Board__[X,Y]:=Board__[X,Y] or WALL;
                  end;
               end;

    {clear all legal box squares on the board; currently they're all blocked by walls}
    for Col:=1 to BoardWidth__ do
        for Row:=1 to BoardHeight__ do
            if (Board__[Col,Row] and (WALL+FLOOR))=(WALL+FLOOR) then
               Dec(Board__[Col,Row],WALL);

    Inc( Result, FillDeadEndRooms( BoardTimeStamps, PlayerAccessArea ) );

    TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTimeMS);
{
    SokUtil_.Msg('Unnecessary illegal box squares: '+IntToStr(Result)+NL+
                 'Time: '+IntToStr(TimeMS)+NL+
//               'Legal box squares: '+IntToStr(BoxLegalSquaresCount)+NL+
                 'Total game moves so far: '+IntToStr(History__.Count)+NL+
                 'Player path search moves: '+IntToStr(PlayerMovesCount),'Normalize Board',MB_OK);
}
  end; {FillUnnecessaryIllegalBoxSquares}


  function ChangeFrozenBoxesToWalls(ChangeImmovableBoxesOnNonGoalSquaresToWalls__,ChangeImmovableBoxesOnGoalSquaresToWalls__:Boolean;
                                    BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Integer;
  var Col,Row,SquareValue:Integer; Again:Boolean; BoardTimestamps:TBoardTimeStamps;

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
               Neighbor1Position.x  := Col__ - DIRECTION_XY [ Direction__ , ColAxis ];
               Neighbor1Position.y  := Row__ - DIRECTION_XY [ Direction__ , RowAxis ];
               Neighbor1            := Board__ [ Neighbor1Position.x , Neighbor1Position.y ];

               Neighbor2Position.x  := Col__ + DIRECTION_XY [ Direction__ , ColAxis ];
               Neighbor2Position.y  := Row__ + DIRECTION_XY [ Direction__ , RowAxis ];
               Neighbor2            := Board__ [ Neighbor2Position.x , Neighbor2Position.y ];

               Inc ( Board__ [ Col__ , Row__ ] , WALL);                         {temporarily change this square to a wall}

               Result := ((  Neighbor1 and (WALL + INVISIBLE_WALL)) <> 0 )
                         or                                                     {is there a wall on any of the neighbor squares?}
                         ((  Neighbor2 and (WALL + INVISIBLE_WALL)) <> 0 )
                         or                                                     {are both neighbors illegal squares?}
                         ((( Neighbor1 and ILLEGAL_SQUARE         ) <> 0 )
                          and
                          (( Neighbor2 and ILLEGAL_SQUARE         ) <> 0 )
                         );

               if (not ( Result and ABoxIsBlockedOnANonGoalSquare__ ))          {it's not enough to know whether a box is blocked; it's also necessary to know whether a box is blocked on a non-goal square}
                  and
                  (( Neighbor1 and ( WALL + BOX ) ) = BOX )                     {test if neighbor1 is a blocked box}
                  and
                  BoxIsBlockedAlongOneAxis( Neighbor1Position.x , Neighbor1Position.y , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
                  then Result := True;

               if (not ( Result and ABoxIsBlockedOnANonGoalSquare__ ))          {it's not enough to know whether a box is blocked; it's also necessary to know whether a box is blocked on a non-goal square}
                  and
                  (( Neighbor2 and ( WALL + BOX ) ) = BOX )                     {test if neighbor2 is a blocked box}
                  and
                  BoxIsBlockedAlongOneAxis( Neighbor2Position.x , Neighbor2Position.y , Direction__ , ABoxIsBlockedOnANonGoalSquare__ )
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

      Result := (( Board__ [ ToCol__ , ToRow__ ] and ( WALL {+ ILLEGAL_SQUARE} ) ) <> 0 )   {a wall is considered a deadlocked square}
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

    repeat
      Again:=False;
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
                   Again:=True;
                   Inc(Result);
                   end;
              end;
      if Again then {let 'FillTubes' recalculate illegal box squares, and then check for more frozen boxes}
         FillTubes(MovePlayerAndBoxes__,ChangeImmovableBoxesOnNonGoalSquaresToWalls__,ChangeImmovableBoxesOnGoalSquaresToWalls__,StableNormalization__);
    until not Again;
  end; {ChangeFrozenBoxesToWalls}

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
                      Board__[Col,Row]:=Board__[Col,Row] or EXTERIOR_WALL;
                      Inc(Result);
                      end
                 else Board__[Col,Row]:=Board__[Col,Row] and (not EXTERIOR_WALL);
                 end;
  end; {CalculateExteriorWalls}

begin {'NormalizeBoard'}
  Result:=0;
  OriginalBoardWidth:=BoardWidth__; OriginalBoardHeight:=BoardHeight__; OriginalBoard:=Board__; History__.Count:=0; PlayerCount:=0;

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
             Queue[QueueTop].X:=Col;
             Queue[QueueTop].Y:=Row;
             Inc(PlayerCount);
             end;
      end;
  Board__[Succ(BoardWidth__),Succ(BoardHeight__)]:=WALL; {fill the corner square at the bottom-right of the board with a wall; the board must be surrounded by walls; otherwise a board ending with a box on a goal instead of a wall is not handled properly}

  {calculate the reachable squares from the player-positions}
  QueueIndex:=Low(Queue); {'Low': point before the first player-square on the queue, if any}
  if QueueIndex<QueueTop then begin {'True': there is one or more players on the board; if there aren't any players on the board, then don't fill the board squares with walls because they are "unreachable from a player"}
     while QueueIndex<QueueTop do begin
       Inc(QueueIndex); {advance to the next reachable square on the queue}
       with Queue[QueueIndex] do begin
         Board__[X,Y]:=Board__[X,Y] or FLOOR; {ensure that all squares reachable from the player-positions are marked as floor squares}
         for Direction:=Low(Direction) to High(Direction) do begin
             Col:=X+DIRECTION_XY[Direction,ColAxis];
             Row:=Y+DIRECTION_XY[Direction,RowAxis];
             if ((Board__[Col,Row] and WALL)=0) and  {'True': the neighbor square doesn't contain a wall, i.e., it's a floor-square}
                (not Visited[Col,Row]) then begin  {'True': the neighbor square hasn't been visited before}
                Visited[Col,Row]:=True; {this square is reachable from a player}
                Inc(QueueTop); {put the neighbor square on the queue so its neighbors are explored later}
                Queue[QueueTop].X:=Col;
                Queue[QueueTop].Y:=Row;
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

function  SelectedBoardSquaresToText(BoardWidth__,BoardHeight__:Integer; const Board__:TBoard):String; {throws EOutOfMemory}
var Col,Row,Index,Length:Integer;
begin
  Length:=0;
  for Row:=BoardHeight__ downto 1 do
      for Col:=BoardWidth__ downto 1 do
          if (Board__[Col,Row] and SQUARE_SET)<>0 then begin
             if Length=0 then {makes the function work correctly, also without the quick-and-dirty loop exit below}
                Length:=(Pred(Row)*BoardWidth__)+ Col;
             break; {quick-and-dirty exit loop when a marked square has been found}
             end;
  if Length=0 then
     Result:=''
  else begin
     SetLength(Result,Length);
     Index:=0;
     for Row:=1 to BoardHeight__ do
         for Col:=1 to BoardWidth__ do
             if Index<Length then begin
                if   (Board__[Col,Row] and SQUARE_SET)=0 then
                     Result[Index+STRING_BASE]:='0'
                else Result[Index+STRING_BASE]:='1';
                Inc(Index);
                end;
     end;
end; {SelectedBoardSquaresToText}

function  SelectedSquaresAsTextToBoard(const SelectedSquaresAsText__:String; BoardWidth__,BoardHeight__:Integer; var Board__:TBoard):Integer;
var Col,Row,Index,Length:Integer;
begin
  Result:=0;
  Index:=0;
  Length:=System.Length(SelectedSquaresAsText__);
  for Row:=1 to BoardHeight__ do
      for Col:=1 to BoardWidth__ do begin
          if   (Index<Length) and (SelectedSquaresAsText__[Index+STRING_BASE]='1') then begin
               Inc(Result);
               Board__[Col,Row]:=Board__[Col,Row] or       SQUARE_SET;
               end
          else Board__[Col,Row]:=Board__[Col,Row] and (not SQUARE_SET);
          Inc(Index);
          end;
end; {SelectedSquaresAsTextToBoard}

function TextThatCannotBeInterpretedAsMoves(const Text__:String):String; {throws EOutOfMemory}
begin {Ensures that a text cannot be interpreted as moves, e.g., 'DULL'}
  Result:=Text__;
  if IsAMoveTextLine(Result) then Result:=QUOTE+Result+QUOTE;
end; {TextThatCannotBeInterpretedAsMoves}

function TextToBoard(Width__,Height__:Integer; const BoardAsText__:String; var Board__:TBoard):Boolean;
var Col,Row,Index:Integer;
begin {Precondition: 'Board__' contains a valid board}
  Result:=(Width__ >=MIN_BOARD_WIDTH ) and (Width__ <=MAX_BOARD_WIDTH ) and
          (Height__>=MIN_BOARD_HEIGHT) and (Height__<=MAX_BOARD_HEIGHT) and
          (Width__*Height__=Length(BoardAsText__));
  if Result then begin
     ClearBoard(Board__); Index:=STRING_BASE;
     for Row:=1 to Height__ do begin
         for Col:=1 to Width__ do begin
             Board__[Col,Row]:=CharToBoardSquareValue(BoardAsText__[Index]);
             Inc(Index);
             end;
         end;
     end;
end; {TextToBoard}

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

{
 --------------------------------------------------------------------
 Low-level Sokoban-file management
 --------------------------------------------------------------------
}

{TNotes}

constructor TNotes.Create; {throws EOutOfMemory}
begin
  Inherited;
  Lines:=nil; MacroExpanded:=False; Modified:=False; {clear the object; not strictly necessary in Delphi, but safe programming-style}
  Lines:=TList.Create;
end; {TNotes.Create}

destructor TNotes.Destroy;
begin
  Lines.Free;
end; {TNotes.Destroy}

procedure TNotes.Clear;
begin
  Lines.Clear; MacroExpanded:=False; Modified:=False;
end; {TNotes.Clear}

function TNotes.CopyTo(Destination__:TNode):Boolean;
begin
  Result:=Destination__ is TNotes;
  if Result then begin
     Result:=Destination__.SetName(Name) and
             Lines.CopyTo(TNotes(Destination__).Lines);
     TNotes(Destination__).MacroExpanded:=MacroExpanded;
     TNotes(Destination__).Modified:=Modified;
     end;
end; {TNotes.CopyTo}

function TNotes.MacroExpand(Macros__:TList; const NewLine__:String):Boolean;
begin
  if not MacroExpanded then MacroExpanded:=Lines.MacroExpand(Macros__,NewLine__);
  Result:=MacroExpanded; {note: macroexpansion doesn't count as a modification}
end; {TNotes.MacroExpand}

function TNotes.WriteToFile(TextFile__:PTextFile):Integer; {throws EInOutError}
begin
  Result:=0;
  if not Lines.IsEmpty then Result:=Lines.WriteToFile(TextFile__);
end; {TNotes.WriteToFile}

{TSnapshotAsText}

constructor TSnapshotAsText.Create; {throws EOutOfMemory}
begin
  Inherited;
  MovesAsTextLines:=nil; Notes:=nil; {clear the object; not strictly necessary in Delphi, but safe programming-style}
  MovesAsTextLines:=TList.Create; Notes:=TNotes.Create; Tag:=0;
end; {TSnapshotAsText.Create}

destructor TSnapshotAsText.Destroy;
begin
  MovesAsTextLines.Free; Notes.Free;
  Inherited;
end; {TSnapshotAsText.Destroy}

procedure TSnapshotAsText.Clear;
begin
  MovesAsTextLines.Clear;
  Notes.Clear;
  inherited;
end;

function TSnapshotAsText.CopyTo(Destination__:TNode):Boolean;
begin
  Result:=(Destination__ is TSnapshotAsText)
          and
          Destination__.SetName(Name)
          and
          MovesAsTextLines.CopyTo(TSnapshotAsText(Destination__).MovesAsTextLines)
          and
          Notes           .CopyTo(TSnapshotAsText(Destination__).Notes);
end; {TSnapshotAsText.CopyTo}

function  TSnapshotAsText.IsASaveGame:Boolean;
begin
  Result:=IsASaveGameName(Name);
end;

function TSnapshotAsText.MovesToTextLines(Moves__:PHistoryMoves; Count__,Top__:Integer; ReverseMode__,PrettyPrintGamesEnabled__,RleEnabled__,CombinedMoveFormattingEnabled__:Boolean):Boolean;
const MOVE_SEPARATOR=H_MASK_MOVE_SEPARATOR or H_FLAG_JUMP;
var i,LineWidth,TextLength:Integer; MovesAsText,s:String; n:TNode;
begin {Converts moves from internal format to textlines}
  Result:=Moves__<>nil;
  if Result then
     try    MovesAsTextLines.Clear;
            if   MovesToText(Moves__,Count__,Top__,ReverseMode__,RleEnabled__,CombinedMoveFormattingEnabled__,MovesAsText) then begin
                 if   PrettyPrintGamesEnabled__ then LineWidth:=LINE_WIDTH_MOVES {break the game down into lines}
                 else LineWidth:=High(LineWidth);                                {save the game as one long text-line}

                 i:=0; TextLength:=Length(MovesAsText);
                 if TextLength>0 then
                    repeat s:=System.Copy(MovesAsText,STRING_BASE+i,Min(LineWidth,TextLength-i));
                           Result:=CreateObject(otNode,n);
                           if Result then begin
                              MovesAsTextLines.Push(n);
                              Result:=n.SetText(s);
                              end;
                           Inc(i,Length(s));
                    until  (not Result) or (i>=TextLength);

                 MovesAsTextLines.Reverse; {lines are in reversed order: make it right}
                 end
            else raise Exception.Create(TEXT_TASK_FAILED);

     except on E:Exception do Result:=SokUtil_.Error(E.Message,'TSnapshotAsText.MovesToTextLines');
     end;
  if not Result then MovesAsTextLines.Clear;
end; {TSnapshotAsText.MovesToTextLines}

function TSnapshotAsText.ObjectType:TObjectType;
begin
  Result:=oTSnapshotAsText;
end; {TSnapshotAsText.ObjectType}

function TSnapshotAsText.TextLinesToMoves(var History__:THistory; var ReverseMode__:Boolean {$IFDEF SokHtml}; var MovePlayerBackToStart__:Integer {$ENDIF}):Boolean; {text -> internal form}
const MAX_MOVE_GROUP_NESTING_LEVEL=255;
var   LastCombinedMoveIndex,LeadingEmailCharactersCount,MoveGroupNestingLevel,
      JumpMoveStartIndex,i,Index,LastIndex,CountDown:Integer;
      IsABoxMove:Boolean;
      Ch:Char; Direction:TDirection; Move:THistoryMove;
      FromPos,ToPos:TColRow;
      n:TNode;
      RleCount,MoveGroupStartIndex:array[0..MAX_MOVE_GROUP_NESTING_LEVEL] of Integer;

  procedure BeginMoveGroup;
  begin
    Inc(MoveGroupNestingLevel);
    if MoveGroupNestingLevel<=High(RleCount) then begin
       RleCount[MoveGroupNestingLevel]:=RleCount[0];
       MoveGroupStartIndex[MoveGroupNestingLevel]:=Succ(History__.Top);
       end;
  end;

  procedure EndMoveGroup;

    procedure RepeatGroupMoves(RepeatCount__,FirstIndex__,LastIndex__:Integer);
    var Index,Count:Integer;
    begin
      for Count:=1 to RepeatCount__ do
          for Index:=FirstIndex__ to LastIndex__ do
              if History__.Top<MAX_MOVES then begin
                 Inc(History__.Top); History__.Moves[History__.Top]:=History__.Moves[Index];
                 end;
    end;

  begin {EndMoveGroup}
    if MoveGroupNestingLevel>0 then begin
       if MoveGroupNestingLevel<=High(RleCount) then
          RepeatGroupMoves(Pred(RleCount[MoveGroupNestingLevel]),MoveGroupStartIndex[MoveGroupNestingLevel],History__.Top); {'Pred(RleCount[])': the moves in the group have already been emitted once}
       if (JumpMoveStartIndex=0) or (MoveGroupNestingLevel>1) then {if 'JumpMoveStartIndex<>0' here, then there are unbalanced parenthesis; keep one nesting level for the jump even though its repeat factor may be wrong due to nesting unbalance}
          Dec(MoveGroupNestingLevel);
       if (MoveGroupNestingLevel=0) and {'0': only moves with group nesting level 1 are treated as combined moves, and only if the group isn't an rle-group like "2(ru)"}
          (RleCount[1]=0) and           {group nesting levels > 1 are not treated as combined moves; they are rle-groups, e.g., "2(r3(rU))"}
          (JumpMoveStartIndex=0) and
          (History__.Top<=MAX_MOVES) then
          LastCombinedMoveIndex:=History__.Top;
       end;
  end;

begin {TSnapshotAsText.TextLinesToMoves}
  {Converts the text-version of a game to internal form, but without
   checking for legal moves.

   The accompanying module 'SokGame_' has a legal-moves check, but
   since 'SokGame_' almost is a complete Sokoban program, you
   probably don't want to include that module if you only are plugging
   'SokFile_' into an existing program to support the file-format.
   Thus, in that case you will have to check for legal moves yourself
   with code like this:

     Local Variable: LegalMovesCount:=0;
     Game.ReverseMode:=Snapshot.ReverseMode;
     Game.Reset();
     for i:=1 to Snapshot.History.Top do
         if Game.IsALegalMove(History.Moves[i]) then begin
            Inc(LegalMovesCount);
            Game.DoMove(Snapshot.History.Moves[i])
            end
         else break;
     Snapshot.History.Top:=LegalMovesCount; // drops illegal moves, if any
     Snapshot.History.Index:=Min(Snapshot.History.Index,Snapshot.History.Top); // the current position ('History.Index') is called 'History.Count' in this module
  }
  Result:=True;
  ReverseMode__:=False;
  History__.Count:=MAX_MOVES; History__.Top:=0; History__.Moves[0]:=0;
  History__.LastBoxNo:=0; History__.LastPushIndex:=0; {note that last boxno isn't calculated in this procedure}
  JumpMoveStartIndex:=0; MoveGroupStartIndex[0]:=0; LastCombinedMoveIndex:=0; MoveGroupNestingLevel:=0; RleCount[0]:=0;
  LeadingEmailCharactersCount:=-1;
  {$IFDEF SokHtml}; MovePlayerBackToStart__:=-1; {$ENDIF};

  n:=MovesAsTextLines.First;
  while (n<>nil) and Result do begin
    Index:=STRING_BASE; LastIndex:=StrLastCharIndex(n.Text);
    if    (Index< LastIndex) and (n.Text[Index    ]= DOUBLE_QUOTE) then Inc(Index);
    if    (Index<=LastIndex) and (n.Text[LastIndex]= DOUBLE_QUOTE) then Dec(LastIndex);
    while (Index<=LastIndex) and (n.Text[LastIndex]<=SPACE)        do   Dec(LastIndex);
    if    (Index< LastIndex) and (n.Text[LastIndex]= BACKSLASH   ) then Dec(LastIndex); {drop a trailing backslash, if any}

    if IsACommentLine(n.Text) or (Index>LastIndex) then {skip the line}
    else begin
       {any leading email quotation characters ('>') need special treatment}
       {because '>' also used to mark the ending of a jump move;}
       {if the old '>' jump-end-marker happens to be the first character in a}
       {line, it should not be confused with an email quotation character;}
       {this implementation makes the reasonable assumption, that if the lines}
       {have leading email quotation characters, then all lines have the same}
       {number of leading email quotation characters; this seems a reasonable}
       {assumption because that is how email quotation normally works}
       if LeadingEmailCharactersCount<>0 then begin {'True': this is the first line, or the first line had leading '>' characters}
          CountDown:=LeadingEmailCharactersCount;
          {the first line is special in that the counter initially is -1, hence,}
          {the first line will have all its leading '>' characters trimmed}
          repeat while (Index<=LastIndex) and (n.Text[Index]<=SPACE) do Inc(Index);
                 if   (Index<=LastIndex) and (n.Text[Index]=EMAIL_QUOTE_CH) and (CountDown<>0) then begin
                      Inc(Index); Dec(CountDown); Ch:=EMAIL_QUOTE_CH;
                      end
                 else Ch:=SPACE;
          until  Ch<>EMAIL_QUOTE_CH; {leading '>' characters may be separated like '> > >'}
          if CountDown<0 then LeadingEmailCharactersCount:=Pred(-CountDown); {'<0': this is the first line; use the number of leading '>' characters as countdown for the remaining lines}
          end;

       i:=Index;
       while (i<=LastIndex) and
             ((n.Text[i]<=SPACE) or (AnsiPos(n.Text[i],LEGAL_MOVE_CHARACTERS)>=STRING_BASE)) do
             Inc(i);
       if i>LastIndex then begin {normal line with moves in udlr-format}
          while (Index<=LastIndex) and Result do begin
            Ch:=n.Text[Index]; Inc(Index);

            if   CharToDirection(Ch,Direction) then begin
                 Move:=Ord(Direction);
                 if Ch=UpCase(Ch)         then Inc(Move,H_FLAG_BOX);
                 if JumpMoveStartIndex<>0 then Move:=(Move or H_FLAG_JUMP) and (not H_FLAG_BOX);
                 Inc(Move,H_FLAG_ODD and  (not History__.Moves[LastCombinedMoveIndex])); {combined move separator: flip bit from last move}

                 if RleCount[0]=0 then RleCount[0]:=1;
                 for i:=1 to RleCount[0] do begin
                     Inc(History__.Top);
                     if History__.Top<=MAX_MOVES then
                        History__.Moves[History__.Top]:=Move
                     else begin {too many moves: drop them}
                        end;
                     if (MoveGroupNestingLevel=0) and
                        (History__.Top<=MAX_MOVES) then
                        LastCombinedMoveIndex:=History__.Top;
                     end;
                 RleCount[0]:=0;
                 end
            else if (Ch>='0') and (Ch<='9') then begin
                    if   RleCount[0]<=(High(RleCount[0]) - Ord(Ch) + Ord('0')) div 10 then
                         RleCount[0]:=RleCount[0]*10+Ord(Ch)-Ord('0')
                    else Result:=False; {numerical overflow}
                    end
                 else begin
                    case Ch of
                      NULL_CHAR..SPACE   :; {skip blanks}
                      GROUP_BEGIN_CH     : BeginMoveGroup;
                      GROUP_END_CH       : EndMoveGroup;
                      JUMP_BEGIN_CH,
                      JUMP_BEGIN_CH1     : if   (Index         <=LastIndex) and
                                                (n.Text[Index]  =COLON) and
                                                (Ch             ='<') then begin {an early version used '<:' to start a combined move}
                                                Inc(Index); {skip the next character, i.e., ':'}
                                                BeginMoveGroup;
                                                end
                                           else if JumpMoveStartIndex=0 then begin
                                                   if not ReverseMode__ then begin {first jump: initialize reverse mode play}
                                                      ReverseMode__:=True; LastCombinedMoveIndex:=0;
                                                      History__.Count:=MAX_MOVES; History__.Top:=0;
                                                      end;
                                                   JumpMoveStartIndex:=Succ(History__.Top);
                                                   BeginMoveGroup;
                                                   end;
                      JUMP_END_CH,
                      JUMP_END_CH1       : if   JumpMoveStartIndex<>0 then begin
                                                JumpMoveStartIndex:=0;
                                                EndMoveGroup;
                                                end;
                      {$IFDEF SokHtml}
                        MOVE_PLAYER_BACK_TO_START_CH
                                         : if   MovePlayerBackToStart__<0 then begin {found '!': make the player jump back to the start position at this point}
                                                MovePlayerBackToStart__:=History__.Top;
                                                end
                                           else Result:=False; {implementation limitation: only one jump back to the player start position}
                      {$ENDIF}
                      CURRENT_MOVE_CH    : if   MoveGroupNestingLevel<>0 then        {'True': current position is in the middle of a combined move/jump-move:}
                                                History__.Count:=LastCombinedMoveIndex  {set current position before the combined move}
                                           else History__.Count:=History__.Top;
                      COLON              : if   (Index<=LastIndex) and {an early version used ':>' to end a combined move}
                                                (n.Text[Index]='>') then begin
                                                Inc(Index); {skip the next character, i.e., '>'}
                                                EndMoveGroup;
                                                end;
                    end; {case}
                    RleCount[0]:=0;
                    end;
            end;
          end
       else begin
          i:=History__.Top;
          if  IsAChessNotationMoveLine(n.Text,MAX_BOARD_WIDTH,MAX_BOARD_HEIGHT,True,i,FromPos,ToPos,IsABoxMove) and
              DxDyToDirection(ToPos.x-FromPos.x,ToPos.y-FromPos.y,Direction) then begin
              Move:=Ord(Direction);
              if      JumpMoveStartIndex<>0 then Inc(Move,H_FLAG_JUMP)
              else if IsABoxMove            then Inc(Move,H_FLAG_BOX);
              Inc(Move,H_FLAG_ODD and (not History__.Moves[LastCombinedMoveIndex]));
              Inc(History__.Top);
              if History__.Top<=MAX_MOVES then
                 History__.Moves[History__.Top]:=Move
              else begin {too many moves: drop them}
                 end;
              if (MoveGroupNestingLevel=0) and
                 (History__.Top<=MAX_MOVES) then
                 LastCombinedMoveIndex:=History__.Top;
              end;
          end;
       end;
    n:=n.Next; {next line}
    end;

  with History__ do begin
    Count:=Min(Count,Top); {current position in the snapshot}
    Result:=Result and (Top>0) and (Top<=MAX_MOVES); {if there are more than 'MAX_MOVES' moves, then the snapshot is considered invalid because some of the moves could not be loaded into the game history}
    end;
end; {TSnapshotAsText.TextLinesToMoves}

function TSnapshotAsText.WriteToFile(TextFile__:PTextFile):Integer; {throws EInOutError}
begin
  Result:=0;
  if MovesAsTextLines.IsEmpty and Notes.Lines.IsEmpty then {empty: drop it}
  else begin
     Writeln(TextFile__^);
     if   Name<>'' then
          Writeln(TextFile__^,Name)
     else Writeln(TextFile__^,SNAPSHOT_TYPE_NAME[stSnapshot]);
     Result:=MovesAsTextLines.WriteToFile(TextFile__);
     if (Result=0) and (Notes<>nil) and (not Notes.Lines.IsEmpty) then begin
        Writeln(TextFile__^);
        Result:=Notes.WriteToFile(TextFile__);
        end;
     end;
end; {TSnapshotAsText.WriteToFile}

{TExtendedSnapshotAsText}

constructor TExtendedSnapshotAsText.Create;
var Index:Integer;
begin
  Inherited;
  ImprovementAsText:=''; IsASolution:=False; TimeMS:=0; OptimizationFlags:=0;
  Inc(LastSerialNo);
  SerialNo:=LastSerialNo;
  FillChar(Metrics,SizeOf(Metrics),0);
  for Index:=Low(SelectedRange) to High(SelectedRange) do SelectedRange[Index]:=0;  
end;

destructor  TExtendedSnapshotAsText.Destroy;
begin
  ImprovementAsText:='';
  Inherited;
end;

{$IFDEF SokobanYASC}
  function  TExtendedSnapshotAsText.FormattedName(SecondaryMetricsInTitles__:Boolean):String; {throws EOutOfMemory}
  var SolverName:String;
  begin
    SolverName:=ExtractTextInParenthesis(Name);
    if SolverName<>'' then SolverName:=SPACE+StrWithParenthesis(SolverName);
    if   IsASolution then
         Result:=SnapshotsForm.SolutionName
    else Result:=SnapshotsForm.NormalModeSnapshotName; {not strictly correct but this will have to do; currently, Sokoban YASC only uses the function for solutions}
    Result:=Result+SPACE+
            Format(FORMAT_MOVES_AND_PUSHES,[Metrics.MoveCount,Metrics.PushCount])+
            SecondaryMetricsFormattedAsATitleSuffix(SecondaryMetricsInTitles__,Metrics.SecondaryMetrics)+
            SolverName;
  end;
{$ENDIF}

function  TExtendedSnapshotAsText.ObjectType:TObjectType; {override;}
begin
  Result:=otExtendedSnapshotAsText;
end;

{TLevel}

constructor TLevel.Create; {throws EOutOfMemory}
begin
  Inherited;
  BoardAsTextLines:=nil; Notes:=nil; SnapshotsAsText:=nil; {clear the object; not strictly necessary in Delphi, but safe programming-style}
  BoardAsTextLines:=TList.Create;
  Flags           :=0;
  IsMadeFromMoves :=False;
  Notes           :=TNotes.Create;
{
  Inc(LastSerialNo);
  SerialNo        :=LastSerialNo;
}
  SnapshotsAsText :=TList.Create;
  Tag.Value       :=0;
end; {TLevel.Create}

destructor TLevel.Destroy;
begin
  BoardAsTextLines.Free; Notes.Free; SnapshotsAsText.Free;
  Inherited;
end; {TLevel.Destroy}

function TLevel.BoardToTextLines(BoardWidth__,BoardHeight__:Integer; const Board__:TBoard):Boolean;
var i,Col,Row:Integer; IsEmptyRow:Boolean; s:String; n:TNode;
begin {Converts an internal board to textformat}
  Result:=True; BoardAsTextLines.Clear;
  try    SetLength(s,BoardWidth__);
         for Row:=1 to BoardHeight__ do {for each row}
             if Result then begin
                i:=STRING_BASE; IsEmptyRow:=True;
                for Col:=1 to BoardWidth__ do begin {for each column}
                    case Board__[Col,Row] and (WALL+PLAYER+BOX+GOAL) of
                      WALL         : begin s[i]:=WALL_CH;        IsEmptyRow:=False; end;
                      PLAYER       : begin s[i]:=PLAYER_CH;      IsEmptyRow:=False; end;
                      PLAYER+GOAL  : begin s[i]:=PLAYER_GOAL_CH; IsEmptyRow:=False; end;
                      BOX          : begin s[i]:=BOX_CH;         IsEmptyRow:=False; end;
                      BOX+GOAL     : begin s[i]:=BOX_GOAL_CH;    IsEmptyRow:=False; end;
                      GOAL         : begin s[i]:=GOAL_CH;        IsEmptyRow:=False; end;
                      else {FLOOR}   begin s[i]:=FLOOR_CH; end;
                    end; {case}
                    Inc(i);
                    end;
                Result:=CreateObject(otNode,n);
                if Result then begin
                   BoardAsTextLines.Push(n);
                   if   IsEmptyRow then
                        Result:=n.SetText( FLOOR_NON_BLANK_CH2 ) {emit a line with a single non-blank floor character only}
                   else Result:=n.SetText(TrimRight(Copy(s,STRING_BASE,BoardWidth__))); {'Copy': ensure that 'TrimRight' works on a copy of 's'}
                   end;
                end;
         BoardAsTextLines.Reverse; {lines in reversed order: make it right}
  except on E:Exception do Result:=SokUtil_.Error(E.Message,'TLevel.BoardToTextLines');
  end;
  if not Result then BoardAsTextLines.Clear;
end; {TLevel.BoardToTextLines}

function  TLevel.BoardToTextRLE(RleFloor__:Char; var Text__:String):Boolean; {Board as run length encoded text}
var Count:Integer; Rle:TRle;
begin
  Result:=False;
  try    if   CreateObject(otRle,TNode(Rle)) then
              try     if   BoardAsTextLines.ToText(SOKOBAN_NEWLINE,Text__) then begin
                           Text__:=SokUtil_.StrSubstitute(Text__,FLOOR_CH,RleFloor__,Count);
                           if Rle.Compress(Text__) then begin
                              Text__:=Copy(Rle.Text,STRING_BASE,Rle.Position);
                              Result:=True;
                              end;
                           end;
              finally Rle.Free;
              end;
  except on E:Exception do Result:=SokUtil_.Error(E.Message,'TLevel.BoardToTextRLE');
  end;
end;

procedure TLevel.Clear;
begin
  BoardAsTextLines.Clear; Notes.Clear; SnapshotsAsText.Clear;
  Inherited;
end;

function TLevel.CopyTo(Destination__:TNode):Boolean;
begin
  Result:=SokUtil_.Error(Format(TEXT_INTERNAL_ERROR_FORMAT,[TEXT_NOT_IMPLEMENTED]),'TLevel.CopyTo');
end; {TLevel.CopyTo}

function TLevel.GetExtendedSnapshotAsTextBySerialNo(SerialNo__:TSerialNo):TExtendedSnapshotAsText;
var Node:TNode;
begin
  Result:=nil;
  if SnapshotsAsText<>nil then begin
     Node:=SnapshotsAsText.First;
     while (Node<>nil) and (Result=nil) do begin
       if Node is TExtendedSnapshotAsText then with Node as TExtendedSnapshotAsText do
          if SerialNo=SerialNo__ then Result:=TExtendedSnapshotAsText(Node);
       Node:=Node.Next;
       end;
     end;
end; {TLevel.GetExtendedSnapshotAsTextBySerialNo}

function  TLevel.GetSnapshotAsTextByIndex(Index__:Integer): TSnapshotAsText;
begin
  if   SnapshotsAsText=nil then Result:=nil
  else Result:=TSnapshotAsText(SnapshotsAsText.GetItemByIndex(Index__));
end; {TLevel.GetSnapshotAsTextByIndex}

function  TLevel.GetSnapshotAsTextByName(const Name__: String): TSnapshotAsText;
begin
  if   SnapshotsAsText=nil then Result:=nil
  else Result:=TSnapshotAsText(SnapshotsAsText.GetItemByName(Name__));
end; {TLevel.GetSnapshotAsTextByName}

function  TLevel.LookupSnapshotAsText(const Moves__:SokUtil_.TList; var SnapshotAsText__:TSnapshotAsText):Boolean;
begin {Returns 'True' if a snapshot with matching moves exists}
  Result:=False;
  if SnapshotsAsText<>nil then begin
     SnapshotAsText__:=TSnapshotAsText(SnapshotsAsText.First);
     while (SnapshotAsText__<>nil) and (not Result) do
       if   (SnapshotAsText__.MovesAsTextLines<>nil) and
             SnapshotAsText__.MovesAsTextLines.IsEqual(Moves__) then
            Result:=True
       else SnapshotAsText__:=TSnapshotAsText(SnapshotAsText__.Next);
     end;
end; {TLevel.LookupSnapshot}

procedure TLevel.MacroExpandNotes(MacroExpandLevelNotes__,MacroExpandSnapshotNotes__:Boolean; Macros__:SokUtil_.TList);
var Snapshot:TSnapshotAsText;
begin
  if MacroExpandLevelNotes__ then Notes.MacroExpand(Macros__,C_STYLE_NEWLINE);
  if MacroExpandSnapshotNotes__ and (SnapshotsAsText<>nil) then begin
     Snapshot:=TSnapshotAsText(SnapshotsAsText.First);
     while Snapshot<>nil do begin
       if Snapshot.Notes<>nil then Snapshot.Notes.MacroExpand(Macros__,C_STYLE_NEWLINE);
       Snapshot:=TSnapshotAsText(Snapshot.Next);
       end;
     end;
end;

function TLevel.ObjectType:TObjectType;
begin
  Result:=otLevel;
end; {TLevel.ObjectType}

function TLevel.TextLinesToBoard(var Board__      :TBoard;
                                 var BoardWidth__ :Integer;
                                 var BoardHeight__:Integer;
                                 var BoxCount__   :Integer;
                                 var GoalCount__  :Integer;
                                 var PlayerPos__  :TColRow;
                                 var ErrorText__  :String):Boolean;
const MAX_GROUP_NESTING_LEVEL=255;
var   i,Col,Index,FirstIndex,GroupNestingLevel,LastIndex:Integer;
      Ch:Char; n:TNode;
      RleCount,GroupStartIndex:array[0..MAX_GROUP_NESTING_LEVEL] of Integer;

  procedure BeginGroup;
  begin
    Inc(GroupNestingLevel);
    if GroupNestingLevel<=High(RleCount) then begin
       RleCount[GroupNestingLevel]:=RleCount[0];
       GroupStartIndex[GroupNestingLevel]:=Succ(Col);
       end;
  end; {TLevel.TextLinesToBoard.BeginGroup}

  function EndGroup:Boolean;

    procedure RepeatGroupSquares(RepeatCount__,FirstIndex__,LastIndex__:Integer);
    var Index,Count:Integer;
    begin
      for Count:=1 to RepeatCount__ do
          for Index:=FirstIndex__ to LastIndex__ do
              if   Col<MAX_BOARD_WIDTH then begin
                   Inc(Col);
                   Board__[Col,BoardHeight__]:=Board__[Index,BoardHeight__];
                   end
              else Result:=False;
      if  Col>BoardWidth__ then BoardWidth__:=Col; {keep track of the maximum width}
    end; {TLevel.TextLinesToBoard.EndGroup.RepeatGroupSquares}

  begin {TLevel.TextLinesToBoard.EndGroup}
    Result:=True;
    if GroupNestingLevel>0 then begin
       if GroupNestingLevel<=High(RleCount) then
          RepeatGroupSquares(Pred(RleCount[GroupNestingLevel]),GroupStartIndex[GroupNestingLevel],Col); {'Pred(RleCount[])': the squares in the group have already been emitted once}
       Dec(GroupNestingLevel);
       end;
  end; {TLevel.TextLinesToBoard.EndGroup}

  function LeftJustifyBoard(var FirstIndex__:Integer):Boolean;
  var i,j:Integer; n:TNode;
  begin {limitation: no support for run length encoded boards which aren't left-justified; such a construct is, however, very unlikely to happen in practice}
    Result:=False;
    if FirstIndex__=STRING_BASE then begin {'True': try if left-justification helps}
       FirstIndex__:=High(FirstIndex__);
       n:=BoardAsTextLines.First;
       while n<>nil do begin {look for first wall character and first box-on-goal character in each line; note that this only is implemented for boards that aren't run length encoded}
          i:=System.Pos(WALL_CH     ,n.Text);
          j:=System.Pos(BOX_GOAL_CH ,n.Text);  if (j<>0) and ((j<i) or (i=0)) then i:=j;
          j:=System.Pos(BOX_GOAL_CH1,n.Text);  if (j<>0) and ((j<i) or (i=0)) then i:=j;
          if i<>0 then begin
             i:=STRING_BASE+Pred(i); {'Pred(): 'Delphi's 'System.Pos()' returns a 1-based position}
             if i<FirstIndex__ then FirstIndex__:=i;
             end;
          n:=n.Next;
          end;
       Result:=(FirstIndex__>STRING_BASE) and (FirstIndex__<>High(FirstIndex__)); {'True': try to parse the board again, this time with left-justification}
       end;
    if not Result then FirstIndex__:=STRING_BASE; {left-justification didn't help; the board is still too wide}
  end; {TLevel.TextLinesToBoard.LeftJustifyBoard}

begin {Converts textlines to internal board-format}
  ErrorText__:=''; FirstIndex:=STRING_BASE;
  try {try...except, because 'Format' may throw 'EOutOfMemory'}
    repeat
      Result:=True;
      BoardWidth__:=0; BoardHeight__:=0; BoxCount__:=0; GoalCount__:=0;
      PlayerPos__.x:=0; PlayerPos__.y:=0; GroupNestingLevel:=0;
      ClearBoard(Board__);

      n:=BoardAsTextLines.First;
      while (n<>nil) and Result do begin {for each line}
        RleCount[0]:=0; Col:=0;
        if   BoardHeight__<MAX_BOARD_HEIGHT then
             Inc(BoardHeight__)
        else Result:=False;

        Index:=FirstIndex; LastIndex:=StrLastCharIndex(n.Text);
        if    (Index< LastIndex) and (n.Text[Index    ]= DOUBLE_QUOTE) then Inc(Index);
        if    (Index<=LastIndex) and (n.Text[LastIndex]= DOUBLE_QUOTE) then Dec(LastIndex);
        while (Index<=LastIndex) and (n.Text[LastIndex]<=SPACE) do Dec(LastIndex);

        if Result then begin
           Result:=LastIndex<High(LastIndex);
           if not Result then ErrorText__:=TEXT_TEXT_TOO_LARGE;
           end;

        while (Index<=LastIndex) and Result do begin {for each character in the line}
          Ch:=n.Text[Index]; Inc(Index);
          if (Ch>='0') and (Ch<='9') then begin
             if   RleCount[0]<=(MAX_BOARD_WIDTH - Ord(Ch) + Ord('0')) div 10 then
                  RleCount[0]:=RleCount[0]*10+Ord(Ch)-Ord('0')
             else begin Result:=False; ErrorText__:=Format(TEXT_ILLEGAL_CHAR_FORMAT,[Ch]);
                  end
             end
          else if Ch=GROUP_BEGIN_CH then begin
                  if RleCount[0]=0 then RleCount[0]:=1;
                  BeginGroup;
                  RleCount[0]:=0;
                  end
          else if Ch=GROUP_END_CH then begin
                  if RleCount[0]=0 then RleCount[0]:=1;
                  for i:=1 to RleCount[0] do
                      if Result then Result:=EndGroup;
                  if  not Result then {'True': the board is too wide}
                      LeftJustifyBoard(FirstIndex); {if the board can be left-justified then try to parse it again}
                  RleCount[0]:=0;
                  end
          else begin
             if RleCount[0]=0 then RleCount[0]:=1;
             i:=Col;
             if (Ch<>SOKOBAN_NEWLINE) and (Ch<>EMAIL_QUOTE_CH) then
                Inc(i,RleCount[0]); {theoretically, this could overflow, but with the restrictions imposed on 'MAX_BOARD_WIDTH' it cannot happen (see 'Initialize')}
             Result:=i<=MAX_BOARD_WIDTH;
             if Result then begin
                if  i>BoardWidth__ then BoardWidth__:=i; {keep track of the maximum width}
                for i:=1 to RleCount[0] do
                    if Result then begin
                       Inc(Col);
                       case Ch of
                         WALL_CH           : begin Board__[Col,BoardHeight__]:=WALL;
                                             end;
                         PLAYER_CH,
                         PLAYER_CH1,
                         PLAYER_CH2        : begin Board__[Col,BoardHeight__]:=PLAYER+FLOOR;
                                                   if PlayerPos__.x>0 then
                                                      Dec(Board__[PlayerPos__.x,PlayerPos__.y],PLAYER);
                                                   PlayerPos__.x:=Col; PlayerPos__.y:=BoardHeight__;
                                             end;
                         PLAYER_GOAL_CH,
                         PLAYER_GOAL_CH1,
                         PLAYER_GOAL_CH2   : begin Board__[Col,BoardHeight__]:=PLAYER+GOAL+FLOOR;
                                                   if PlayerPos__.x>0 then
                                                      Dec(Board__[PlayerPos__.x,PlayerPos__.y],PLAYER);
                                                      PlayerPos__.x:=Col; PlayerPos__.y:=BoardHeight__;
                                                   Inc(GoalCount__);
                                             end;
                         BOX_CH,
                         BOX_CH1           : begin Board__[Col,BoardHeight__]:=BOX+FLOOR;
                                                   Inc(BoxCount__);
                                             end;
                         BOX_GOAL_CH,
                         BOX_GOAL_CH1      : begin Board__[Col,BoardHeight__]:=BOX+GOAL+FLOOR;
                                                   Inc(BoxCount__);
                                                   Inc(GoalCount__);
                                             end;
                         GOAL_CH,
                         GOAL_CH1          : begin Board__[Col,BoardHeight__]:=GOAL+FLOOR;
                                                   Inc(GoalCount__);
                                             end;
                         NULL_CHAR..Char(Pred(Ord(SPACE))), {'SPACE=FLOOR_CH: duplicate case items are not allowed in the Delphi programming language}
                         FLOOR_CH,
                         FLOOR_NON_BLANK_CH1,
                         FLOOR_NON_BLANK_CH2
                                           : begin Board__[Col,BoardHeight__]:=FLOOR;
                                             end;
                         SOKOBAN_NEWLINE   : begin Col:=0;
                                                   if Index<=LastIndex then Inc(BoardHeight__); {'True': the current character isn't the last one in the line}
                                                   Result:=BoardHeight__<=MAX_BOARD_HEIGHT;
                                             end;
                         EMAIL_QUOTE_CH    : begin Dec(Col); {ignore leading '>' characters}
                                             end;
                         else                begin Result:=False;
                                                   ErrorText__:=Format(TEXT_ILLEGAL_CHAR_FORMAT,[Ch]);
                                             end;
                       end; {case}
                       end;
                end
             else {the board is too wide}
                if   LeftJustifyBoard(FirstIndex) then begin
                     {try to parse the board again, this time with left-justification}
                     end
                else begin {left-justification didn't help; the board is still too wide}
                     end;

             RleCount[0]:=0;
             end;
          end;

        n:=n.Next; {next line}
        end;

    until Result or (ErrorText__<>'') or (FirstIndex=STRING_BASE);

    if      Result then
            Result:=IsALegalBoard(Board__,BoardWidth__,BoardHeight__,BoxCount__,GoalCount__,
                                  PlayerPos__,True,True,True,0,ErrorText__)
    else if ErrorText__='' then
            ErrorText__:=Format(TEXT_BOARD_ERROR_SIZE_FORMAT,[MAX_BOARD_WIDTH,MAX_BOARD_HEIGHT,MIN_BOARD_WIDTH,MIN_BOARD_HEIGHT]);
  except on E:Exception do Result:=SokUtil_.Error(E.Message,'TLevel.TextLinesToBoard');
  end;
  if Result then TrimBoard(MIN_BOARD_WIDTH,MIN_BOARD_HEIGHT,Board__,BoardWidth__,BoardHeight__,PlayerPos__);
end; {TLevel.TextLinesToBoard}

function TLevel.ToText(var Text__:String):Boolean;
var IOResult:Integer; TempFileName:String; F:TextFile; FileTime:TFileTime;
begin {A primitive and slow implementation of 'ToText', heavily re-using existing code and a diskfile as temporary storage}
  Result:=False; Text__:=''; IOResult:=0;
  try {$IFDEF SokobanYASC}
        TempFileName:=MainForm.ApplicationDataPath+ExtractFileName(ChangeFileExt(Application.ExeName,WORK_FILE_NAME_EXT));
      {$ELSE}
        TempFileName:=ChangeFileExt(Application.ExeName,WORK_FILE_NAME_EXT);
      {$ENDIF}
      AssignFile(F,TempFileName);
      Rewrite(F);
      try     Inc(IOResult,WriteToFile(Addr(F)));  {write level}
      finally CloseFile(F); Inc(IOResult,System.IOResult);
              if IOResult=0 then {writing the file succeeded}
                 Result:=LoadTextFromFile(Text__,FileTime,TempFileName);
                 while StrBeginsWith(Text__,NL) do Delete(Text__,STRING_BASE,Length(NL));
              SysUtils.DeleteFile(TempFileName);
      end;
      if IOResult<>0 then raise Exception.Create(TEXT_WRITE_FILE_ERROR);
  except on E:Exception do
            Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_SAVE_FILE);
  end;
end; {TLevel.ToText}

function TLevel.WriteToFile(TextFile__:PTextFile):Integer; {throws EInOutError}
var i:Integer;
begin
  Result:=0;
  for  i:=1 to BlankLinesBeforeLevel do Writeln(TextFile__^);
  if   Name<>'' then
       Writeln(TextFile__^,Name)
  else if BlankLinesBeforeLevel>=0 then Writeln(TextFile__^,TEXT_LEVEL); {negative 'BlankLinesBeforeLevel': don't emit an anonymous title}
  if BlankLinesBeforeLevel>=0 then Writeln(TextFile__^); {negative 'BlankLinesBeforeLevel': don't emit a line separator before the board}
  if (Result=0) and (BoardAsTextLines<>nil) and (not BoardAsTextLines.IsEmpty) then begin
     if HasBoardTags then Writeln(TextFile__^,TAG_BOARD_BEGIN);
     Result:=BoardAsTextLines.WriteToFile(TextFile__);
     if (Result=0) and HasBoardTags then Writeln(TextFile__^,TAG_BOARD_END);
     end;
  if (Result=0) and (Notes<>nil) and (not Notes.Lines.IsEmpty) then begin
     Writeln(TextFile__^);
     Result:=Notes.WriteToFile(TextFile__);
     end;
  if (Result=0) and (SnapshotsAsText<>nil) and (not SnapshotsAsText.IsEmpty) then
     Result:=SnapshotsAsText.WriteToFile(TextFile__);
end; {TLevel.WriteToFile}

{TSokoFile}

constructor TSokoFile.Create; {throws EOutOfMemory}
begin
  Inherited;
  Modified  :=False; PuzzleType:=DEFAULT_PUZZLE_TYPE; HasBoardTags:=False; MakeBoardFromMovesEnabled:={$IFDEF SokHtml} True; {$ELSE} False; {$ENDIF}
  AddFileFormatDescriptionToFiles:=DEFAULT_ADD_FILE_FORMAT_DESCRIPTION_TO_FILES;
  ParseSnapshotsWithoutBoardsAsIndividualLevels:=False;
  FileHeader:=nil; Levels:=nil;   {clear the object; not strictly necessary in Delphi, but safe programming-style}
  FileHeader:=TNotes.Create;
  Levels    :=TList.Create;
end; {TSokoFile.Create}

destructor TSokoFile.Destroy;
begin
  Close; FileHeader.Free; Levels.Free;
  Inherited;
end; {TSokoFile.Destroy}
{
function TSokoFile.AddDefaultFileHeader:Boolean;
var i:Integer; Node:TNode;
begin
  Result:=FileHeader<>nil;
  if Result then begin
     FileHeader.Modified:=True;
     for i:=High(DEFAULT_FILE_FORMAT_DESCRIPTION) downto Low(DEFAULT_FILE_FORMAT_DESCRIPTION) do
         Result:=CreateObject(otNode,Node) and
                 FileHeader.Lines.Push(Node).SetText(DEFAULT_FILE_FORMAT_DESCRIPTION[i]);
     end;
end;
}
procedure TSokoFile.Clear;
begin {Note: the filename is retained; a non-blank filename indicates an open file}
  Modified:=False; PuzzleType:=DEFAULT_PUZZLE_TYPE; ClearFileTime(FileTime);
  if FileHeader<>nil then FileHeader.Clear;
  if Levels    <>nil then Levels    .Clear;
end; {TSokoFile.Clear}

function TSokoFile.Close:Boolean;
begin
  Result:=Flush;
  if Result then Result:=SetName('');
  if Result then Clear;
end; {TSokoFile.Close}

function TSokoFile.CopyTo(Destination__:TNode):Boolean;
begin
  Result:=SokUtil_.Error(Format(TEXT_INTERNAL_ERROR_FORMAT,[TEXT_NOT_IMPLEMENTED]),'TSokoFile.CopyTo');
end; {TSokoFile.CopyTo}

function TSokoFile.ExportToFile(const FileName__:String; WriteTitleAndAuthor__, WriteNotes__,WriteAllSnapshots__,WriteSnapshotsOnly__,FillFloors__,RleEnabled__:Boolean; FilledFloor__,RleFloor__:Char; var Count__:Integer):Boolean;
var i,IOResult:Integer; Author,AuthorFromFileNotes,s:String;
    Notes:TNotes; Level:TLevel; Node:TNode; F:TextFile;
begin
  Result:=False; Count__:=0; IOResult:=0; AuthorFromFileNotes:=''; Notes:=nil;
  try     FileHeader.Lines.ReadString(KEY_AUTHOR,AuthorFromFileNotes);
          if CreateObject(otNotes,TNode(Notes)) then
             try
               AssignFile(F,FileName__);
               Rewrite(F);
               try     Level:=TLevel(Levels.First);
                       while Level<>nil do begin
                         Level.Notes.CopyTo(Notes);
                         Notes.MacroExpand(FileHeader.Lines,C_STYLE_NEWLINE);

                         if not Notes.Lines.ReadString(KEY_AUTHOR,Author) then
                            Author:=AuthorFromFileNotes;

                         if WriteNotes__ then begin
                            if Notes.Lines.FindKey(KEY_AUTHOR,Node) then
                               Notes.Lines.Remove(Node,True);
                            if Notes.Lines.FindKey(KEY_TITLE,Node) then
                               Notes.Lines.Remove(Node,True);
                            end;

                         if   WriteSnapshotsOnly__ then
                              Node:=Level.SnapshotsAsText.First
                         else Node:=Level.BoardAsTextLines;

                         while Node<>nil do begin
                            if Count__<>0 then begin
                               Writeln(F,'');
                               if not WriteSnapshotsOnly__ then Writeln(F,'');
                               end;
                            Inc(Count__);

                            if WriteSnapshotsOnly__ then
                               Node.WriteToFile(Addr(F))
                            else begin
                               if   WriteTitleAndAuthor__ then
                                    Writeln(F,Level.Name);
                               if   Node=Level.BoardAsTextLines then
                                    if         RleEnabled__ then begin
                                               if   Level.BoardToTextRLE(RleFloor__,s) then
                                                    Writeln(F,s)
                                               else raise Exception.Create(TEXT_TASK_FAILED);
                                               Node:=nil;
                                               end
                                    else if    FillFloors__ then begin
                                               Node:=TList(Node).First;
                                               while Node<>nil do begin
                                                 Writeln(F,SokUtil_.StrSubstitute(Node.Text,FLOOR_CH,FilledFloor__,i));
                                                 Node:=Node.Next;
                                                 end;
                                               end
                                         else  Node.WriteToFile(Addr(F))
                               else Node.WriteToFile(Addr(F));
                               end;

                            if WriteTitleAndAuthor__ then begin
                               Writeln(F,KEY_TITLE ,COLON,SPACE,Level.Name);
                               Writeln(F,KEY_AUTHOR,COLON,SPACE,Author);
                               end;

                            if WriteNotes__ and
                               (Notes.Lines.Count<>0) then begin {'<>0': the notes contains more than an author and a title}
                               Writeln(F,'');
                               Notes.WriteToFile(Addr(F));
                               end;

                            if Node<>nil then Node:=Node.Next;
                            end;

                         if WriteAllSnapshots__ and (not WriteSnapshotsOnly__) then begin
                            Node:=Level.SnapshotsAsText;
                            while Node<>nil do begin
                              TSnapshotAsText(Node).WriteToFile(Addr(F));
                              Node:=Node.Next;
                              end;
                            end;

                         Level:=TLevel(Level.Next);
                         end;
               finally CloseFile(F);
                       Inc(IOResult,System.IOResult); Result:=IOResult=0;
                       if not Result then SysUtils.DeleteFile(FileName__);

               end;
             finally Notes.Free; Result:=Result and (IOResult=0);
                     if not Result then raise Exception.Create(TEXT_WRITE_FILE_ERROR);
             end
          else raise Exception.Create(TEXT_MEMORY_FULL);
  except on E:Exception do
            Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE);
  end;
end; {TSokoFile.ExportToFile}

function TSokoFile.FindPriorOrNextLevelName(var LevelName__:String; Prior__,WrapAround__:Boolean):Boolean;
var Level,FirstLevel,LastLevel,PrevLevel:TNode;
begin
  Result:=False; FirstLevel:=Levels.First; LastLevel:=Levels.Last;
  Level:=Levels.GetItemByName(StrWithoutBrackets(LevelName__));
  if Level<>nil then
     if Prior__ then begin
        PrevLevel:=Levels.Prev(Level);
        if (PrevLevel=nil) and WrapAround__ and (Level<>LastLevel) then
           PrevLevel:=LastLevel;
        Result:=PrevLevel<>nil;
        if Result then LevelName__:=PrevLevel.Name;
        end
     else begin
        if   (Level.Next=nil) and WrapAround__ and (Level<>FirstLevel) then
             Level:=FirstLevel
        else Level:=Level.Next;
        Result:=Level<>nil;
        if Result then LevelName__:=Level.Name;
        end;
end; {TSokoFile.FindPriorOrNextLevel}

function TSokoFile.Flush:Boolean;
var s:String;
begin
  if (Name='') or
     (AnsiPos(TITLE_ILLEGAL_FIRST_CHARACTER,Name)>=STRING_BASE) then
     {if the name is blank or contains an invalid character, then don't save
      the file; this is not only to catch malformed data, but it's also a
      handy convention; the caller can create a temporary unnamed file and use
      the 'Modified' flag normally during the process; as long as the file
      hasn't a name, the caller can rely on the file destroying itself when it's
      closed;
     }
     Modified:=False;

  Result:=not Modified;
  if not Result then begin
     try    if FileHeader<>nil then begin
               UpdateFileFormatDescription(False);
               if AddFileFormatDescriptionToFiles or
                  FileHeader.Lines.ReadString(KEY_DATE_LAST_CHANGE,s) then
                  FileHeader.Lines.WriteString(KEY_DATE_LAST_CHANGE,FormatDateTime(FORMAT_DATE_TIME,Now));
               end;
     except on E:Exception do; {silent error, not: SokUtil_.Error__(E.Message,'TSokoFile.Close');}
     end;
     Result:=SaveToFile(Name);
     end;
end; {TSokoFile.Flush}

function TSokoFile.GetLevelByBoard( BoardAsTextLines__ : TList ) : TLevel;
begin // searches for a level with a matching board
  if ( Levels = nil ) or ( BoardAsTextLines__ = nil ) or ( not ( BoardAsTextLines__ is TList ) ) then
     Result:=nil
  else begin
     Result := TLevel( Levels.First );
     while ( Result <> nil ) and
           ( not Result.BoardAsTextLines.IsEqual( BoardAsTextLines__ ) ) do
           Result := TLevel( Result.Next );
     end;
end; {TSokoFile.GetLevelByBoard}

function TSokoFile.GetLevelByExtendedSnapshotAsTextSerialNo(SerialNo__:TSerialNo):TLevel;
var Node:TNode;
begin
  Result:=nil;
  if Levels<>nil then begin
     Node:=Levels.First;
     while (Node<>nil) and (Result=nil) do
       if   TLevel(Node).GetExtendedSnapshotAsTextBySerialNo(SerialNo__)<>nil then
            Result:=TLevel(Node)
       else Node:=Node.Next;
     end;
end;

function TSokoFile.GetLevelByIndex(Index__: Integer): TLevel;
begin
  if   Levels=nil then Result:=nil
  else Result:=TLevel(Levels.GetItemByIndex(Index__));
end; {TSokoFile.GetLevelByIndex}

function TSokoFile.GetLevelByName(const Name__: String): TLevel;
begin
  if   Levels=nil then Result:=nil
  else Result:=TLevel(Levels.GetItemByName(Name__));
end; {TSokoFile.GetLevelByName}
{
function TSokoFile.GetLevelBySerialNo(SerialNo__:TSerialNo):TLevel;
var Node:TNode;
begin
  Result:=nil;
  if Levels<>nil then begin
     Node:=Levels.First;
     while (Node<>nil) and (Result=nil) do
       if   TLevel(Node).SerialNo=SerialNo__ then
            Result:=TLevel(Node)
       else Node:=Node.Next;
     end;
end;
}
function TSokoFile.HasFileFormatDescription(var FirstLineNo__,LastLineNo__,MajorVersionNo__,MinorVersionNo__:Integer):Boolean;
var i,FirstRawFileNotesLineNo,LineNo:Integer; Line:TNode;

  function HasRawFileNotesLineStart(const Text__:String):Boolean;
  begin {'::' identifies lines belonging to the raw file notes}
    Result:=(Length(Text__)>=2) and (Text__[1]=COLON) and (Text__[2]=COLON);
  end; {HasRawFileNotesLineStart}

  function HasOldStyleFileFormatDescriptionLineStart(const Text__:String):Boolean;
  begin {'--' identified lines belonging to the file format description in the first versions}
    Result:=(Length(Text__)>=2) and (Text__[1]='-') and (Text__[2]='-');
  end; {HasOldStyleFileFormatDescriptionLineStart}

begin {HasFileFormatDescription} {not fool-proof, but it will have to do}
  Result:=False; MajorVersionNo__:=0; MinorVersionNo__:=0;
  if (FileHeader<>nil) and (FileHeader.Lines<>nil) then with FileHeader.Lines do begin
     Line:=First; FirstLineNo__:=0; LastLineNo__:=0; LineNo:=0; FirstRawFileNotesLineNo:=0;
     while (Line<>nil) and (Line.Text='') do begin
       Inc(LineNo); Line:=Line.Next;
       end;
     while (Line<>nil) and (LineNo>=0) do with Line do begin
       Inc(LineNo);
       if   Text='' then
            if   Result then LineNo:=-1 {stop when seeing the first blank line after the first group of raw file notes lines}
            else FirstLineNo__:=0       {there are no blank lines in the file-format-description}
       else if      HasRawFileNotesLineStart(Text) then begin
                    if FirstRawFileNotesLineNo=0 then begin
                       FirstRawFileNotesLineNo:=LineNo; FirstLineNo__:=0;
                       end;
                    if FirstLineNo__=0 then FirstLineNo__:=LineNo;
                    LastLineNo__:=LineNo;
                    i:=AnsiPos(KEY_FILE_FORMAT,Text);
                    if i>=STRING_BASE then begin {the text 'file format' is present}
                       Inc(i,Length(KEY_FILE_FORMAT)-STRING_BASE); {''-STRING_BASE': i = position is 0-based}
                       if ReadUnsignedInteger(Text,i,MajorVersionNo__) and
                          ReadUnsignedInteger(Text,i,MinorVersionNo__) then
                          Result:=True;
                       end;
                    end
            else if HasOldStyleFileFormatDescriptionLineStart(Text) then begin
                    if FirstLineNo__=0 then FirstLineNo__:=LineNo;
                    LastLineNo__:=LineNo;
                    if AnsiPos(KEY_FILE_FORMAT,Text)<>0 then Result:=True; {the text 'file format' is present}
                    if AnsiPos(TEXT_SOKOBAN_COPYRIGHT_YEAR_0,Text)<>0 then Result:=True; {it's probably the line with the Sokoban copyright notice}
                    end;
       Line:=Next;
       end;
     end;
end; {TSokoFile.HasFileFormatDescription}

function TSokoFile.HasOldVersionFileFormatDescription(var FirstLineNo__,LastLineNo__:Integer):Boolean;
var MajorVersionNo,MinorVersionNo:Integer;
begin
  Result:=HasFileFormatDescription(FirstLineNo__,LastLineNo__,MajorVersionNo,MinorVersionNo)
          and
          (MajorVersionNo <=StrToInt(FILE_FORMAT_DESCRIPTION_MAJOR_VERSION_NO)) and
          ((MajorVersionNo< StrToInt(FILE_FORMAT_DESCRIPTION_MAJOR_VERSION_NO))
           or
           (MinorVersionNo< StrToInt(FILE_FORMAT_DESCRIPTION_MINOR_VERSION_NO)));
end; {TSokoFile.HasOldVersionFileFormatDescription}

function TSokoFile.IsASokobanFile(const FileName__:String):Boolean;
var oShowErrorMessages:TShowErrorMessages;
begin {Side-effect: the current file is closed before an attempt is made to open the new file}
  oShowErrorMessages:=ShowErrorMessages;
  try ShowErrorMessages:=semNone;
      if   IsBlank(FileName__) then                       {blank: clipboard}
           if Clipboard.HasFormat(CF_TEXT) then
              if Close then begin
                 Result:=LoadFromClipboard and (Levels.Count>0) and {a file must contain at least 1 level}
                         (not ((Levels.Count=1) and TLevel(Levels.First).BoardAsTextLines.IsEmpty));
                 if not Result then begin Clear; SetName(''); end;
                 end
              else Result:=False {not strictly correct, but it will have to do}
           else Result:=False
      else if StrEqual(FileName__,Name) then Result:=True {current file is ok, even if it is empty}
           else if   FileExists(FileName__) then begin
                     if   Close then begin                {a file must contain at least 1 level...:}
                          Result:=Open(FileName__) and (Levels.Count>0) and
                                  (not ((Levels.Count=1) and TLevel(Levels.First).BoardAsTextLines.IsEmpty));
                          if not Result then begin Clear; SetName(''); end;
                          end
                     else Result:=False;                  {not strictly correct, but it will have to do}
                     end
                else Result:=False;
  finally ShowErrorMessages:=oShowErrorMessages;
  end;
end; {TSokoFile.IsASokobanFile}

function TSokoFile.LoadFromClipboard:Boolean;
begin
  try    if   Clipboard.HasFormat(CF_TEXT) then
              Result:=LoadFromText(Clipboard.AsText)
         else raise Exception.Create(TEXT_CLIPBOARD_NO_LEVEL_TEXT);
  except on E:Exception do
            Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_OPEN_LEVEL_FROM_CLIPBOARD);
  end;
  if   Result then Modified:=Levels.Count>0 {if no levels, then don't try to save the file later}
  else begin Clear; SetName(''); end;
end; {TSokoFile.LoadFromClipboard}

function TSokoFile.LoadFromFile(const FileName__:String):Boolean;
var Lines:TList; AFileTime:TFileTime;
begin
  Result:=False;
  if Close and SetName(Trim(FileName__)) then begin
     try    if   IsBlank(Name) then
                 Result:=LoadFromClipboard
            else if   FileExists(Name) then
                      if CreateObject(otList,TNode(Lines)) then
                         try     {$IFDEF SokobanYASC}
                                   if (ApplicationMutex<>0) and
                                      (WaitForSingleObject(ApplicationMutex,INFINITE)<>WAIT_OBJECT_0) then
                                      raise Exception.Create(TEXT_PROCESS_SYNCHRONIZATION_ERROR);
                                   try
                                 {$ENDIF}
                                     Result:=Lines.LoadFromFileWithFileTime(Name,AFileTime) and
                                             Self .LoadFromTextLines(Lines);
                                     if Result then FileTime:=AFileTime;
                                 {$IFDEF SokobanYASC}
                                   finally
                                     if (ApplicationMutex<>0) and
                                        (not ReleaseMutex(ApplicationMutex)) then
                                        raise Exception.Create(TEXT_PROCESS_SYNCHRONIZATION_ERROR);
                                   end;
                                 {$ENDIF}
                         finally Lines.Free;
                         end
                      else {}
                 else raise Exception.Create(Format(TEXT_FILE_NOT_FOUND_FORMAT,[Name]));
     except on E:Exception do
               Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_OPEN_FILE);
     end;
     if not Result then begin Clear; SetName(''); end;
     end;
end; {TSokoFile.LoadFromFile}

function TSokoFile.LoadFromText(const Text__:String):Boolean;
var Lines:TList;
begin
  Result:=Close and CreateObject(otList,TNode(Lines));
  if Result then
     try     Result:=Lines.LoadFromText(Text__) and
                     Self .LoadFromTextLines(Lines);
     finally Lines.Free;
             if not Result then begin Clear; SetName(''); end;
     end;
end; {TSokoFile.LoadFromText}

function TSokoFile.LoadFromTextLines(Lines__:TList):Boolean;
const
  HASH_TABLE_BUCKET_INDEX_MASK = 2*1024-1; {since it's a mask, it must be a '2^n - 1' where 'n' is an integer > 0; the buckets are stack-allocated and must be cleared before use, so the size must be reasonably small}
  BLOOM_FILTER_TABLE_BYTE_SIZE = 4*1024;   {the table is stack-allocated and must be cleared before use, so the size must be reasonably small}
var
  i:Integer;
  Title:String; {TimeMS:TTimeMS;}
  Level,AnonymousLevel:TLevel; SnapshotAsText:TSnapshotAsText;
  {a hash table in connection with a Bloom filter is used for}
  {speeding up the search for duplicate level-names;}
  {obviously, it doesn't scale well when the tables are small and of a fixed}
  {size like here, but even so, it reduces the number of linked-list traversals}
  LevelHashBucketVector :array[0..HASH_TABLE_BUCKET_INDEX_MASK]   of TLevel;
  LevelNameSet:TBloomFilter;
  LevelNameSetByteVector:array[0..BLOOM_FILTER_TABLE_BYTE_SIZE-1] of Byte;

  procedure TrimFileHeaderForDuplicateMacros( const FirstLevelTitle__ : String );
  var m,n:TNode;
  begin {the function name is a bit of a misnomer; for efficiency, the function
         only scans for consecutive duplicate macros and "false macros" at the
         end of the file header, as seen when it's in correct order;

         "false macros" appears when the first level has a title which
         includes a "=" character; a title header line above the first level
         with a "=" character will not be read as a title line, but as a
         key-value pair macro definition belonging to the file header section;

         the level can, however, get its title from a "Title: xyz" key-value
         pair in its comment lines after the board, and this title can contain
         "=". When the level is saved, this title will also be written as a
         title header line above the level, and the next time the file is
         opened, this line will be read, not as a part of the first level, but
         as a macro definition belonging to the file header section;

         This happens repeatedly each time the file is saved and re-opened, each
         time adding one more "false macro" line to the file header section.

         precondition: the file header lines are in reversed order}

    with FileHeader.Lines do begin
      repeat
        m:=nil;
        n:=First; {find last non-blank-line}
        while (n<>nil) and IsBlank(n.Text) do n:=n.Next;
        if (n<>nil) and
           (AnsiPos(EQUAL,n.Text)>=STRING_BASE) then begin {'True': a macro}
           m:=n.Next;
           while (m<>nil) and IsBlank(m.Text) do m:=m.Next;
           if (m<>nil) and StrEqual(n.Text,m.Text) then begin
              {a duplicate macro, probably a false one; delete it;}
              while First<>m do Pop.Free; {destroy the duplicate}
              end
           else m:=nil;
           end;
      until m=nil; {until no more duplicate macros at the end of the lines}

      if AnsiPos(EQUAL,FirstLevelTitle__)>=STRING_BASE then begin
         {the first level contains "=" in its title; delete any "false macros"
          at the end of the file header section matching this level title}
         repeat
           n:=First; {find last non-blank-line}
           while (n<>nil) and IsBlank(n.Text) do n:=n.Next;
           if   (n<>nil) and StrEqual(n.Text,FirstLevelTitle__) then begin
                while First<>n do Pop.Free;
                Pop.Free; {destroy the presumably "false macro"}
                end
           else n:=nil;
         until n=nil; {until no more matching macros at the end of the lines}
         end;

      while (First<>nil) and IsBlank(First.Text) do Pop.Free; {trim blank lines}
      end;
  end; {TrimFileHeaderForDuplicateMacros}

  function Load(Lines__:TList; var AnonymousLevel__:TLevel):Boolean;
  type TState=(stFileHeader,stLevel,stLevelBoard,stLevelNotes, {the state controls the parser}
               stSnapshot,stSnapshotMoves,stSnapshotNotes);
  var  h,i,j,k,BoardLinesCount,MoveLinesCount,ChessNotationMoveCount,LevelCount:Integer;
       OK:Boolean; p:TPuzzleType;
       BracketedSectionNameInfo,Title,PuzzleTypeString:String;
       State:TState; Level:TLevel; SnapshotAsText:TSnapshotAsText;

    function IsABoardLine(Line__:TNode; {a text-line may contain a (part of a) run length encoded board, with several rows separated by "|"}
                          BoardProperRowsCount__,BoardShortRowsCount__:Integer; {totals for previous lines}
                          var ProperRowsCount__,ShortRowsCount__,               {totals for this line and tested successor lines, if any}
                              EmptyNotBlankTailRowsCount__,                     {tail of empty non-blank rows at the end of lines tested by this call to the function}
                              LineCount__:Integer):Boolean;                     {number of tested lines, including the current line}
    var i,j,k,LastCharIndex,RunLengthCount,ColumnCount,FloorCount,NonBlankFloorCount:Integer;
        HasLegalFirstItem:Boolean; Ch:Char;
    begin {First non-blank character in each row must be a wall or a box on a goal square, and the rest must be legal board-characters}
      Result:=Line__.Text<>'';
      if Result then begin
         ProperRowsCount__:=0; ShortRowsCount__:=0; LineCount__:=1;
         LastCharIndex:=StrLastCharIndex(Line__.Text); i:=STRING_BASE;

         repeat
           {search first legal item in the current row}
           ColumnCount:=0; FloorCount:=0; NonBlankFloorCount:=0; HasLegalFirstItem:=False; Ch:=Line__.Text[i];

           repeat if       (Ch=WALL_CH) {'True': this is the first wall square in the line}
                           or
                           (Ch=BOX_GOAL_CH) or (Ch=BOX_GOAL_CH1)
                           then begin {a board may have an outer 'wall' made up of un-movable boxes, hence, a box on a goal is a legal first item in a row}
                           HasLegalFirstItem:=True; Inc(ColumnCount);
                           end
                  else if  i=LastCharIndex then begin
                           if       Ch <> SOKOBAN_NEWLINE then {ignore a trailing row separator}
                                    Inc(ColumnCount);
                           if       (Ch<=SPACE) or (Ch=FLOOR_CH) then {control characters and space are handled as floor-squares}
                                    Inc( FloorCount)
                           else  if (Ch=FLOOR_NON_BLANK_CH1) or (Ch=FLOOR_NON_BLANK_CH2) then begin
                                    Inc( FloorCount);
                                    Inc( NonBlankFloorCount );
                                    end;
                           {check if it's an empty row written as a blank line with at least one non-blank floor character}
                           if   ( ColumnCount = FloorCount ) and
                                ( NonBlankFloorCount > 0) and
                                ( ( BoardProperRowsCount__+ BoardShortRowsCount__ + ProperRowsCount__ + ShortRowsCount__ + EmptyNotBlankTailRowsCount__ ) > 0 ) then {'>0': only interior empty rows are a part of the board, not leading or trailing empty rows}
                                {tentatively accept an empty row with at least one non-blank floor character as a board line}
                                HasLegalFirstItem:=True   {fake that the empty row has a legal first item}
                           else Result:=False; {no legal first item in the row}
                           end
                  else if  (Ch<=SPACE) or {control characters and space are handled as floor-squares}
                           (Ch=FLOOR_CH) then begin {ok: 'blank', i.e., a floor-square}
                           Inc(ColumnCount);
                           Inc(i); Ch:=Line__.Text[i];
                           Inc( FloorCount );
                           end
                  else if  (Ch=FLOOR_NON_BLANK_CH1) or
                           (Ch=FLOOR_NON_BLANK_CH2) then begin {ok: 'blank', i.e., a floor-square}
                           Inc(ColumnCount);
                           Inc(i); Ch:=Line__.Text[i];
                           Inc( FloorCount );
                           Inc( NonBlankFloorCount );
                           end
                  else if  (Ch=EMAIL_QUOTE_CH) then begin {leading mail quotation character}
                           if   ProperRowsCount__=0 then begin
                                Inc(i); Ch:=Line__.Text[i];
                                end
                           else Result:=False; {leading quotation characters should only occur in front of the text}
                           end
                  else if  (Ch>='0') and (Ch<='9') then begin
                           RunLengthCount:=0;
                           repeat if   RunLengthCount<=(MAX_BOARD_WIDTH - Ord(Ch) + Ord('0')) div 10 then begin
                                       RunLengthCount:=RunLengthCount*10+Ord(Ch)-Ord('0');
                                       Inc(i); Ch:=Line__.Text[i];
                                       end
                                  else Result:=False;
                           until  (Ch<'0') or (Ch>'9') or (i=LastCharIndex) or (not Result);
                           Inc(ColumnCount,Pred(RunLengthCount)); {'Pred': the next character is counted for itself}
                           if       (Ch<=SPACE) or (Ch=FLOOR_CH) then {control characters and space are handled as floor-squares}
                                    Inc( FloorCount       ,Pred(RunLengthCount))  {'Pred': the next character is counted for itself}
                           else if  (Ch=FLOOR_NON_BLANK_CH1) or (Ch=FLOOR_NON_BLANK_CH2) then begin
                                    Inc(FloorCount        ,Pred(RunLengthCount)); {'Pred': the next character is counted for itself}
                                    Inc(NonBlankFloorCount,Pred(RunLengthCount)); {'Pred': the next character is counted for itself}
                                    end;
                           end
                  else if  (Ch=GROUP_BEGIN_CH) or (Ch=GROUP_END_CH) then begin
                           Inc(i); Ch:=Line__.Text[i]; {'(...)' run length encoding may use '(...)' groups}
                           end
                  else if  (Ch=SOKOBAN_NEWLINE) and (NonBlankFloorCount>0) and (FloorCount=ColumnCount) and
                           ( ( BoardProperRowsCount__+ BoardShortRowsCount__ + ProperRowsCount__ + ShortRowsCount__ + EmptyNotBlankTailRowsCount__ ) > 0 ) then begin {'>0': only interior empty rows are a part of the board, not leading or trailing empty rows}
                           {tentatively accept an empty row with at least one non-blank floor character as a board line}
                           HasLegalFirstItem:=True; {fake that the empty row has a legal first item}
                           Dec(i); {adjust the character position so the row separator will be read again as the next character during postprocessing of the row}
                           end
                  else Result:=False; {characters before the first legal item in each row must be 'blank'}
           until  (not Result) or HasLegalFirstItem; {until parsing the line segment as a board row failed, or until a legal first item in the row has been found}

           {validate the rest of the row, i.e., after the first legal item (wall or box on a goal square) in the row}
           while HasLegalFirstItem and (i<LastCharIndex) do begin
             Inc(i); Ch:=Line__.Text[i];
             if      (Ch<=SPACE) or {the rest of the line must contain legal board-characters only}
                     (AnsiPos(Ch,LEGAL_BOARD_CHARACTERS)>=STRING_BASE) then
                     Inc(ColumnCount)
             else if (Ch>'0') and (Ch<='9') then begin
                     RunLengthCount:=0;
                     repeat if   (i<LastCharIndex) and
                                 (RunLengthCount<=(MAX_BOARD_WIDTH - Ord(Ch) + Ord('0')) div 10) then begin
                                 RunLengthCount:=RunLengthCount*10+Ord(Ch)-Ord('0');
                                 Inc(i); Ch:=Line__.Text[i];
                                 end
                            else Result:=False; {numeric overflow or the row ends with a number; in both cases, the row isn't a part of a board}
                     until  (Ch<'0') or (Ch>'9') or (not Result);
                     Inc(ColumnCount,Pred(RunLengthCount)); {'Pred': the next character is counted for itself}
                     Result:=Result and (Ch<>SOKOBAN_NEWLINE) and (Ch<>GROUP_END_CH); {the run length count must be followed by some valid board characters; if it's followed by ')', then the line may be a title line with a number, e.g., "#50 (3)"}
                     if   Result then
                          Dec(i) {adjust the index into the text before the 'while' loop increments it again}
                     else HasLegalFirstItem:=False; {exit the 'while HasLegalFirstItem...' loop and return from the function}
                     end
             else if Ch=SOKOBAN_NEWLINE then begin
                     if i<LastCharIndex then begin
                        HasLegalFirstItem:=False; {'False': exit the enclosing 'while' loop to finish the current row and start searching for the first legal item after the line separator, in the next row, if any}
                        Inc(i); {advance to the next character}
                        end
                     else begin
                        {the run length encoded string ends with a line-separator; ignore it}
                        end;
                     end
             else if (Ch=GROUP_BEGIN_CH) or (Ch=GROUP_END_CH) then begin
                     {run length encoding may use '(...)' groups, e.g.,
                      "4(#--)"; there is an implementation limitation here in
                      the board line validation (but not later during
                      expansion): finding the first legal item in a row doesn't
                      expand groups, meaning that the group members are only
                      counted once when the number of columns in the row is
                      calculated; theoretically, this can influence the
                      classification of a text line as a board line or not a
                      board line, but in practice, it's very unlikely to happen;
                      at worst, a row can falsely be categorized as a short row,
                      but for any playable valid board, there will be enough
                      neighboring rows categorized as long rows, thereby making
                      the short rows valid too;}
                     end
             else Result:=False;
             end;

           if Result then
              if ColumnCount > FloorCount then begin {'True': its' not an empty row with at least one non-blank floor character}
                 if        ColumnCount>=MIN_BOARD_WIDTH then {'True': the row is long enough to be a proper board row}
                           Inc(ProperRowsCount__)
                 else if   ColumnCount<>0 then   {'True': the row has legal board characters but it's too short to be a proper board row; don't discard the row right away, it could there for a decorative purpose}
                           Inc(ShortRowsCount__) {open up for parsing successor rows, if any, before making the decision whether this short row should be accepted as a board row}
                      else Result:=False; {the row is empty or contains illegal board characters}
                 Inc( ShortRowsCount__, EmptyNotBlankTailRowsCount__ ); {if the previous rows were filled with non-blank floor characters, then promote these rows to short rows now that a non-empty row has been found}
                 EmptyNotBlankTailRowsCount__ := 0; {the latest parsed row isn't filled with non-blank floor characters}
                 end
              else begin
                 {the row is empty with at least one non-blank floor character;
                  tentatively accept is as a board row; count the number of
                  consequtive rows of this type; if the board ends with empty
                  rows, then they should not be considered a part of the board;}
                 Inc( EmptyNotBlankTailRowsCount__ );
                 end;

         until (not Result) or HasLegalFirstItem; {until parsing the line as a board failed, or until this isn't a run length encoded board where some characters after a "|" line-separator haven't been parsed yet}

         if Result and ( EmptyNotBlankTailRowsCount__ > 0 ) and ( ( ProperRowsCount__ + ShortRowsCount__ ) = 0 ) then {'True': the text line contained only empty rows filled with non-blank floor characters}
            if (Line__.Next<>nil) and    {'True': there are more text lines}
               IsABoardLine(Line__.Next,BoardProperRowsCount__+ProperRowsCount__,BoardShortRowsCount__+ShortRowsCount__,i,j,EmptyNotBlankTailRowsCount__,k) and {'True': the following text lines contain one or more board rows}
               ( ( i + j ) <> 0 ) then begin {'True': the following text lines contain at least one proper row or a short row, meaning that they aren't all empty rows filled with non-blank floor characters}
               Inc(ProperRowsCount__,i);  {accumulate the number of proper board rows}
               Inc(ShortRowsCount__ ,j);  {accumulate the number of short  board rows}
               Inc(LineCount__      ,k);  {accumulate the number of tested board text lines}
               for i:=1 to k do Line__:=Line__.Next; {advance to the last tested board text line}
               end
            else Result:=False; {the current text line with only empty rows filled with non-blank floor squares should not be considered a part of the board because it's not followed by proper rows}

         if Result and (ShortRowsCount__<> 0) then begin {'True': some board rows were too short to be proper board rows}
            while (BoardProperRowsCount__+                      ProperRowsCount__                                              < MIN_BOARD_HEIGHT) and
                  (BoardProperRowsCount__+BoardShortRowsCount__+ProperRowsCount__+ShortRowsCount__+EmptyNotBlankTailRowsCount__<=MAX_BOARD_HEIGHT)and
                  (Line__.Next<>nil) and
                  IsABoardLine(Line__.Next,BoardProperRowsCount__+ProperRowsCount__,BoardShortRowsCount__+ShortRowsCount__,i,j,EmptyNotBlankTailRowsCount__,k) do begin
                  Inc(ProperRowsCount__,i); {accumulate the number of proper board rows}
                  Inc(ShortRowsCount__ ,j); {accumulate the number of short  board rows}
                  Inc(LineCount__      ,k); {accumulate the number of tested board text lines}
                  for i:=1 to k do Line__:=Line__.Next; {advance to the last tested board text line}
                  end;

            Result:=(BoardProperRowsCount__+ProperRowsCount__                                       >=MIN_BOARD_HEIGHT) and
                    (BoardProperRowsCount__+BoardShortRowsCount__+ProperRowsCount__+ShortRowsCount__<=MAX_BOARD_HEIGHT);
            end;
         end;
    end; {IsABoardLine}

    function IsAMoveLine(const Line__:String; MoveLinesCount__:Integer; var ChessNotationMoveCount__:Integer):Boolean;
    var i,j:Integer; Ch:Char; HasNonDigitChar,IsABoxMove:Boolean; FromPos,ToPos:TColRow;
    begin {Legal move-characters only, or a move in chess-notation}
{
      Result:=IsAMoveTextLine(Line__)
              or
              IsAChessNotationMoveLine(Line__,MAX_BOARD_WIDTH,MAX_BOARD_HEIGHT,True,ChessNotationMoveCount__,FromPos,ToPos,IsABoxMove);
}
     {inline 'IsAMoveTextLine' with the addition of a slightly more refined email quotation character support}
     i:=STRING_BASE; j:=StrLastCharIndex(Line__); HasNonDigitChar:=False;
      Result:=(StrTrimmedLength(Line__,i,j)>0) and (j<High(j));
      if Result and (i<j) and (Line__[i]=EMAIL_QUOTE_CH) then {skip leading '>' characters}
         repeat Inc(i);
         until  (i=j) or ((Line__[i]>SPACE) and (Line__[i]<>EMAIL_QUOTE_CH));
      if Result and (i<j) and (Line__[j]=BACKSLASH) then {skip trailing '\' characters, and any whitespace characters before them}
         repeat Dec(j);
         until  (i=j) or ((Line__[j]>SPACE) and (Line__[j]<>BACKSLASH));
      while Result and (i<=j) do begin
         Ch:=Line__[i]; Inc(i);
         Result:=AnsiPos(Ch,LEGAL_MOVE_CHARACTERS)>=STRING_BASE;
         if   Result then begin
              if (not HasNonDigitChar)
                 and
                 ((Ch<'0') or (Ch>'9'))
                 and
                 (Ch<>GROUP_BEGIN_CH)
                 and
                 (Ch<>GROUP_END_CH)
                 and
                 {if this is the first candidate for a move-line,}
                 {then an empty email quotation line like '>'}
                 {should not qualify as a move-line}
                 ((MoveLinesCount__<>0)
                  or
                  (Ch<>EMAIL_QUOTE_CH)
                 ) then
                 HasNonDigitChar:=True;
              end
         else Result:=Ch<=SPACE; {allow spaces between moves}
         end;
      if   Result then
           Result:=HasNonDigitChar {a line with digits only is not considered a proper moves-line; the number might be a title}
      else Result:=IsAChessNotationMoveLine(Line__,MAX_BOARD_WIDTH,MAX_BOARD_HEIGHT,True,ChessNotationMoveCount__,FromPos,ToPos,IsABoxMove);
    end; {IsAMoveLine}

    function IsBoard(Lines__:TList; var BoardLinesCount__:Integer):Boolean;
    var i,j,BoardProperRowsCount,BoardShortRowsCount,NextLinesCount,NextLinesProperRowsCount,NextLinesShortRowsCount,EmptyNotBlankTailRowsCount:Integer;
        Ch:Char; n,p:TNode;
    begin {Checks if the following lines contain a board, and counts these lines}
      BoardLinesCount__:=0; BoardProperRowsCount:=0; BoardShortRowsCount:=0; EmptyNotBlankTailRowsCount:=0; n:=Lines__.First;
      if not HasBoardTags then begin
         while (n<>nil) and
               IsABoardLine(n,BoardProperRowsCount    ,BoardShortRowsCount    , {totals for lines parsed until now}
                              NextLinesProperRowsCount,NextLinesShortRowsCount, {totals for lines parsed by this call}
                              EmptyNotBlankTailRowsCount,                       {tail of empty non-blank rows at the end of the lines tested by this call}
                              NextLinesCount) do begin                          {lines parsed by this call}
           Inc(BoardLinesCount__,NextLinesCount); {count the parsed board text lines}
           Inc(BoardProperRowsCount,NextLinesProperRowsCount); {count proper (long enough) board-rows in the following text lines; each text line can contain several rows separated by "|"}
           Inc(BoardShortRowsCount ,NextLinesShortRowsCount ); {count short board rows (width < MIN_BOARD_WIDTH)}
           for i:=1 to NextLinesCount do n:=n.Next; {advance to the next untested text line}
           end;
         Result:=BoardProperRowsCount>=MIN_BOARD_HEIGHT; {it takes at least 'MIN_BOARD_HEIGHT' rows to make a board}
         end
      else begin {a board enclosed by <board> ... </board> tags}
         if (n<>nil) and (AnsiPos(TAG_BOARD_BEGIN,n.Text)<>0) then begin
            p:=n; n:=n.Next;
            while (n<>nil) and
                  (BoardLinesCount__<=MAX_BOARD_HEIGHT) and
                  (AnsiPos(TAG_BOARD_END,n.Text)=0) do begin
                  Inc(BoardLinesCount__); p:=n; n:=n.Next;
                  end;
            Result:=(n<>nil) and (BoardLinesCount__<=MAX_BOARD_HEIGHT);

            if Result then begin
               {drop the line with the end-of-board tag}
               p.Next:=n.Next; n.Free;
               {drop the line with the beginning-of-board tag}
               Lines__.Pop.Free;

               {ensure that all characters on the board are legal}
               n:=Lines__.First; i:=BoardLinesCount__;
               while (i<>0) and (n<>nil) do begin {'i' and 'n' should be in sync but better safe than sorry}
                 for j:=0 to Pred(Length(n.Text)) do begin
                     Ch:=n.Text[STRING_BASE+j];
                     if (j>=MAX_BOARD_WIDTH) or
                        (AnsiPos(Ch,LEGAL_BOARD_CHARACTERS)<STRING_BASE) then
                        n.Text[STRING_BASE+j]:=SPACE;
                     end;
                 Dec(i); n:=n.Next;
                 end;
               end;
            end
         else Result:=False;
         end;
    end; {IsBoard}

    function IsMoves(Lines__:TList; var MoveLinesCount__,ChessNotationMoveCount__:Integer):Boolean;
    var n:TNode; {Checks if following lines contain a snapshot, and counts these lines}

      function IsChessNotationMoveLinesWithCommentLines(var Node__:TNode; var MoveLinesCount__,ChessNotationMoveCount__:Integer):Boolean;
      var h,i,j,k,PendingLinesCount,NextMoveCount:Integer; IsABoxMove:Boolean;
          FromPos,ToPos:TColRow; n:TNode;
      begin {Using chess notation, comment-lines may be inserted between move-lines}
        Result:=False;
        if   ChessNotationMoveCount__>0 then begin {is chess-notation used for this snapshot?}
             NextMoveCount:=-1;
             n:=Node__.Next; PendingLinesCount:=0;
             while (n<>nil) and (not Result) do begin {search next move-line with chess-notation}
               Inc(PendingLinesCount);
               if   IsABoardLine(n,0,0,h,i,j,k) then n:=nil {board found: no more moves}
               else if IsAChessNotationMoveLine( {line with a move in chess-notation?}
                         n.Text,
                         MAX_BOARD_WIDTH,MAX_BOARD_HEIGHT,
                         False,NextMoveCount,FromPos,ToPos,IsABoxMove) then {'False': don't check number sequence}
                         if NextMoveCount=Succ(ChessNotationMoveCount__) then begin
                               {next move found}
                               Result:=True;
                               Inc(MoveLinesCount__,PendingLinesCount);
                               ChessNotationMoveCount__:=NextMoveCount;
                               Node__:=n; {skip tested lines}
                               end
                            else {the move does not have the correct sequence number, i.e., it probably belongs to another snapshot}
                               n:=nil
                         else n:=n.Next; {try next line}
               end;
             end;
      end;

    begin {IsMoves}
      MoveLinesCount__:=0; n:=Lines__.First;
      while (n<>nil)
            and
            (IsAMoveLine(n.Text,MoveLinesCount__,ChessNotationMoveCount__) {regular move-line}
             or
             ((not IsBlank(n.Text)) {non-blank comment-lines are allowed}
              and
              (MoveLinesCount__>0)
              and
              IsACommentLine(n.Text)
             )
             or {using chess-notation, text-lines may be inserted between move-lines}
             IsChessNotationMoveLinesWithCommentLines(n,MoveLinesCount__,ChessNotationMoveCount__)
            ) do begin
            Inc(MoveLinesCount__); n:=n.Next;
            end;
      Result:=MoveLinesCount__>0;
    end; {IsMoves}

    function MakeTitleThatCannotBeInterpretedAsMoves(const Title__:String; var HasChanged__:Boolean):String;
    var i:Integer;
    begin {ensures that a title cannot be interpreted as moves, e.g., 'DULL'}
      Result:=Trim(Title__);
      i:=0;
      HasChanged__:=IsAMoveLine(Result,MAX_BOARD_HEIGHT,i); {'MAX_BOARD_HEIGHT': actually, any non-zero value will do}
      if HasChanged__ then Result:=QUOTE+Result+QUOTE;
    end; {MakeTitleThatCannotBeInterpretedAsMoves}

    function ConvertLastLinesToATitle(var   Lines__        :TList;
                                      const IllegalTitle__ :String):String; {throws EOutOfMemory}
    var HasChanged:Boolean; n:TNode;
    begin { lines are in reverse order, i.e., the newest one is the first one
            on the list;

            a non-empty return value is the title of the next object;

            there is also a scenario where the lines are interpreted as having
            the title of the currently active object ("first line" refers to
            normal order and not reverse order):

            * the first line contains a name preceded by ";", e.g., "; 1"
            * all lines except the first one are empty
            * there is a least one blank line after the first non-blank line

            in that case, the active object gets its title from the first line
            (see 'PostprocessLevel');

            this is not - and doesn't want to be - a full or fool-proof
            implementation of ";" as a sort of keyword indicator for titles; it
            is merely a hack for importing levels from plain level files without
            notes and without snapshots, but with ";"-tagged titles after the
            boards;
          }

      with Lines__ do begin
        n:=First;
        while (n<>nil) and IsBlank(n.Text) do n:=n.Next; {find last non-blank-line}
        {a title-line is a non-blank line preceded by a blank line, or a first line}
        if (n<>nil) and
           (AnsiPos(IllegalTitle__,n.Text)<STRING_BASE) and {don't accept a line including this string in the title}
           (AnsiPos(COLON         ,n.Text)<STRING_BASE) and {a line with ":" is probably a comment-line with tagged information such as "Author: NN"}
           ((not Levels.IsEmpty)                            {not "IsEmpty": not the title of the first level; a file-header textline with "=" is probably a macro-line such as "Author=NN", so the first level cannot have a title line with "="}
            or
            (n.Text[STRING_BASE]=SEMICOLON)                 {some xsb-files use ";" as tag in title-lines}
            or
            (AnsiPos(EQUAL        ,n.Text)<STRING_BASE)     {a file-header textline with "=" is probably a macro-line such as "Author=NN"}
           )
           and
           ((n.Next=nil) {nil: 'n' is the first/only line}
            or
            IsBlank(n.Next.Text)
            or
            ( (n.Text[STRING_BASE]=SEMICOLON) {some xsb-files use ";" as tag in title-lines}
              and
              (n.Next.Text[STRING_BASE]<>SEMICOLON) {if the preceding line also begins with ";" then it's probably a multi-line comment}
            )
           ) then begin
           Result:=MakeTitleThatCannotBeInterpretedAsMoves(
                     StrRemoveCharacters(Trim(n.Text),TITLE_ILLEGAL_CHARACTERS),
                     HasChanged);
           if   n.Next=nil then begin                     {nil: 'n' is the first/only line}
                if   (n                  <>First)     and {<>: there is at least one blank line after (in normal forward order) the non-empty line}
                     (n.Text[STRING_BASE]= SEMICOLON) and
                     (Result             <>'') and
                     (Levels.First<>nil) then begin       {<>: there is a currently active object}
                     {use the "; <Name>" line as title for the currently active object}
                     n:=First;   {leave all lines on the list}
                     Result:=''; {don't return a title for the next object}
                     end
                else {use the line as title for the next object}
                     n:=nil;     {destroy the used lines; see 'Pop.Free' below}
                end
           else if   IsBlank(n.Next.Text) then n:=n.Next.Next
                else n:=n.Next;
           while First<>n do Pop.Free; {destroy the converted lines}
           end
        else Result:=''; {no title found}
        end;
    end; {ConvertLastLinesToATitle}

    function  TrimLeadingEmailQuotes(Node__:TNode):TNode;
    var Index,LastIndex,LastEmailQuoteCharacterIndex:Integer; Ch:Char;
    begin {the text in 'Node'__' is modified destructively if it contains leading '>' characters}
      with Node__ do begin
        Index:=STRING_BASE; LastIndex:=STRING_BASE+Pred(Length(Text));
        LastEmailQuoteCharacterIndex:=STRING_BASE-1;
        while Index<=LastIndex do begin
          Ch:=Text[Index];
          if   Ch>SPACE then begin
               if   Ch<>EMAIL_QUOTE_CH then
                    LastIndex:=STRING_BASE-1 {found first character in the line that isn't a '>' and isn't a white-space: stop parsing}
               else if   (Index<LastIndex) and (Text[Succ(Index)]<=SPACE) then {'True': '>' is followed by a white-space character}
                         LastEmailQuoteCharacterIndex:=Succ(Index) {trim '> ' rather than just '>'; in most cases this is what was intended, and it won't hurt because board-lines are not trimmed for leading '>' characters}
                    else LastEmailQuoteCharacterIndex:=Index;
               end;
          Inc(Index);
          end;
        if LastEmailQuoteCharacterIndex>=STRING_BASE then
           Delete(Text,STRING_BASE,LastEmailQuoteCharacterIndex-STRING_BASE+1);
        end;
      Result:=Node__;
    end; {TrimLeadingEmailQuotes}

    function  PostProcessLevel(var Level__:TLevel):Boolean;
    {$IFDEF SokHtml}
      var Text1,Title:String; NextSnapshot,SnapshotAsText:TSnapshotAsText;
    {$ELSE}
      var Title:String; SnapshotAsText:TSnapshotAsText;
    {$ENDIF}

      procedure AddLevelToSet(Level__:TLevel); {save level names for fast duplicate keys checks}
      var HashTableBucketIndex:Integer;
      begin
        Inc(LevelCount);
        HashTableBucketIndex :=BloomFilterAdd(LevelNameSet,Level__.Text) and HASH_TABLE_BUCKET_INDEX_MASK; {first add the level-name to the Bloom filter; 'BloomFilterAdd' returns the primary hash-key}
        TLevel(Level__.Flags):=LevelHashBucketVector[HashTableBucketIndex]; {'Flags' is used for building a linked list of items that hash to the same bucket, so here 'Flags' is set to the old root of the bucket}
        LevelHashBucketVector[HashTableBucketIndex]:=Level__; {add the level to the hash-table; the level is put in front of the linked list of items in the bucket; in other words, the buckets are unsorted}
      end;

      function GetTitleFromCommentLineBeginningWithSEMICOLON(Lines__ :TList):String;
      var HasChanged: Boolean; n:TNode;
      begin { lines are in reverse order, i.e., the newest one is the first one on the list}
        n:=Lines__.First;
        while (n<>nil) and IsBlank(n.Text) do n:=n.Next; {find last non-blank-line}
        if (n<>nil) and {'True': found a non-blank line}
           (n.Next=nil) {'True': this is the first and only non-blank line on the list}
           and
           (n.Text[STRING_BASE]=SEMICOLON) then begin {some xsb-files use ";" as tag in title-lines, in this case a title after right after the last board line}
           Result:=MakeTitleThatCannotBeInterpretedAsMoves(
                     StrRemoveCharacters(Trim(n.Text),TITLE_ILLEGAL_CHARACTERS),
                     HasChanged);
           if Result<>'' then {'True': found a title}
              while Lines__.First<>nil do Lines__.Pop.Free; {destroy the converted lines}
           end
        else Result:=''; {no title found}
      end;

      function SetLevelName(Level__:TLevel; const Name__:String):Boolean;
      var i,OldTagValue:Integer; HasChanged,IsAnAnonymousLevel:Boolean; s,Title:String; L:TLevel;

        function  GetLevelByName(const Name__:String):TLevel;
        var HashKey:THashTableHashKey;
        begin {Look up the name in the Bloom filter and the hash-table}
          if   BloomFilterLookup(LevelNameSet,Name__,HashKey) then begin {note: a Bloom filter can return false positives but never false negatives}
               Result:=LevelHashBucketVector[HashKey and HASH_TABLE_BUCKET_INDEX_MASK];
               while (Result<>nil) and (not StrEqual(Name__,Result.Text)) do
                 Result:=TLevel(Result.Flags);
               end
          else Result:=nil;
        end;

        function MakeUniqueLevelName(const Name__,DefaultName__:String; UseParens__:Boolean; var No__:Integer):String;
        var s:String;
        begin {the normal 'TList.MakeUniqueName1' with it's linked list search is too slow if there is a large number of levels, e.g., 10,000 or more}
          Result:='';
          s     :=Trim(Name__);
          if s  = '' then s:=Trim(DefaultName__);
          if s  <>'' then s:=s+SPACE;
          if (No__<=0) {and UseParens__} then Inc(No__);
          while (Result='') and (No__<High(No__)) do begin
              Inc(No__); {the number increases before the first candidate name}
              if   (Name__='') or (not UseParens__) then
                   Result:=s+IntToStr(No__)
              else Result:=s+LEFT_PAREN+IntToStr(No__)+RIGHT_PAREN;
              if   GetLevelByName(Result)<>nil then Result:='';
              end;
          if Result='' then raise Exception.Create(TEXT_TASK_FAILED);
        end; {MakeUniqueLevelName}

      begin {SetLevelName}
        Title:=Name__;
        for i:=0 to Pred(Length(Title)) do
            if Title[i+STRING_BASE]<SPACE then Title[i+STRING_BASE]:=UNDERSCORE; {change control characters like TAB and LINEFEED to underscores}
        Title:=MakeTitleThatCannotBeInterpretedAsMoves(
                 StrRemoveCharacters(Title,TITLE_ILLEGAL_CHARACTERS),
                 HasChanged);

        if Title='' then Title:=TEXT_LEVEL;
        i:=AnsiPos(XSB_FILE_NAME_EXT,Title);
        if i>=STRING_BASE then {sometimes xsb-collections add '.xsb' to the title}
           System.Delete(Title,i,Length(XSB_FILE_NAME_EXT));

        OldTagValue:=Level.Tag.Value;
        Level.Tag.Value:=1; {'Tag' is used for numbering levels with identical names}
        Result:=Level__.SetText(''); {temporarily clear the name}
        if Result then begin
           L:=GetLevelByName(Title); {find a level with the same title, if any}
           if L<>nil then begin
              if (L.Tag.Value=0) and (OldTagValue<>0) then
                 {the current level is the one carrying the number of
                 {levels with a matching title; the current level changes}
                 {name in this function, hence, transfer the number to the next}
                 {level, so creating unique names doesn't start all over}
                 {again from number 0; starting from 0 each time would lead to}
                 {exponential growth, and a few hundred levels with identical}
                 {names would be enough to bring the application to its knees}
                 L.Tag.Value:=OldTagValue;

              IsAnAnonymousLevel:=StrEqual(Title,TEXT_LEVEL);
              s:=MakeUniqueLevelName(Title,TEXT_LEVEL,not IsAnAnonymousLevel,L.Tag.Value);
              if   s<>'' then begin
                   Title:=s;
                   if IsAnAnonymousLevel then AnonymousLevel__:=L; {remember the level with the title 'Level'}
                   end
              else raise Exception.Create(TEXT_MEMORY_FULL);
              end;
           end;
        Result:=Level__.SetText(Title) and Result;
      end; {PostProcessLevel.SetLevelName}

      function SetSnapshotName(Level__:TLevel; SnapshotAsText__:TSnapshotAsText; const Name__:String):Boolean;
      var i,OldTag:Integer; HasChanged:Boolean; s,Title:String; V:TSnapshotAsText;
      begin
        Title:=Name__;
        for i:=0 to Pred(Length(Title)) do
            if Title[i+STRING_BASE]<SPACE then Title[i+STRING_BASE]:=UNDERSCORE; {change control characters like TAB and LINEFEED to underscores}
        Title:=MakeTitleThatCannotBeInterpretedAsMoves(
                 StrRemoveCharacters(Title,TITLE_ILLEGAL_CHARACTERS),
                 HasChanged);
        {$IFDEF SokobanYASC}
          if Title='' then Title:=MainForm.Game.SnapshotTypeName(SokFile_.stSnapshot);
        {$ELSE}
          if Title='' then Title:=TEXT_SNAPSHOT;
        {$ENDIF}

        OldTag:=SnapshotAsText__.Tag;
        SnapshotAsText__.Tag:=1; {'Tag' is used for numbering snapshots with identical names}
        Result:=SnapshotAsText__.SetName(''); {temporarily clear the name}
        if Result then begin
           V:=TSnapshotAsText(Level__.SnapshotsAsText.GetItemByName(Title)); {find snapshot with the same title, if any}
           if V<>nil then begin
              if (V.Tag=0) and (OldTag<>0) then
                 {the current snapshot is the one carrying the number of
                 {snapshots with a matching title; the current snapshot changes}
                 {name in this function, hence, transfer the number to the next}
                 {snapshot, so creating unique names doesn't start all over}
                 {again from number 0; starting from 0 each time would lead to}
                 {exponential growth, and a few hundred snapshots with identical}
                 {names would be enough to bring the application to its knees}
                 V.Tag:=OldTag;

              {$IFDEF SokobanYASC}
                s:=Level__.SnapshotsAsText.MakeUniqueName1(Title,MainForm.Game.SnapshotTypeName(SokFile_.stSnapshot),True,V.Tag);
              {$ELSE}
                s:=Level__.SnapshotsAsText.MakeUniqueName1(Title,TEXT_SNAPSHOT,True,V.Tag);
              {$ENDIF}
              if   s<>'' then Title:=s
              else raise Exception.Create(TEXT_MEMORY_FULL);
              end;
           end;
        Result:=SnapshotAsText__.SetText(Title) and Result;
      end; {PostProcessLevel.SetSnapshotName}

      function MakeLevelFromSnapshot(var SnapshotAsText__:TSnapshotAsText):Boolean;
      var BoardWidth,BoardHeight,i{$IFDEF SokHtml}, MovePlayerBackIndex {$ENDIF}:Integer;
          HasChanged,ReverseMode:Boolean;
          s,Title:String;
          SnapshotType:TSnapshotType;
          PlayerPos:TColRow; Board:TBoard; History:THistory; Level:TLevel; V:TSnapshotAsText;

        procedure ReverseHistoryEliminatingJumps(var History__:THistory);
        var Index:Integer; Move:THistoryMove; H:THistory;
        begin
          FillChar(H,SizeOf(H),0);
          for Index:=History__.Top downto 1 do begin
              Move:=History__.Moves[Index];
              if (Move and H_FLAG_JUMP)=0 then begin
                 Move:=(Move and (not H_MASK_DIRECTION))+ Ord(OPPOSITE_DIRECTION[TDirection(Move and H_MASK_DIRECTION)]);
                 Inc(H.Top);
                 H.Moves[H.Top]:=Move;
                 end;
              end;
          History__:=H;
        end;

        function AddSnapshotToLevel(Level__:TLevel;
                                    BoardWidth__,BoardHeight__:Integer;
                                    Board__:TBoard; PlayerPos__:TColRow):Boolean;
        var BoardWidth,BoardHeight,BoxCount,GoalCount,dx,dy,Col,Row,A,B:Integer;
            s:String; PlayerPos:TColRow; Board:TBoard; TempLevel:TLevel;

          procedure MarkConnectedFloorSquares(var Board__:TBoard; PlayerPos__:TColRow); {mark all floor-connected squares, starting from the player's position}
          var   Col,Row,X,Y,StackTop:Integer; Direction:TDirection;
                Stack:array[0..MAX_BOARD_SIZE] of TColRow;
          begin
            if (PlayerPos__.X<>0) and (PlayerPos__.Y<>0) then begin
               StackTop:=1; Stack[1].X:=PlayerPos__.X; Stack[1].Y:=PlayerPos__.Y;
               Dec(Board__[PlayerPos__.X,PlayerPos__.Y],FLOOR); {mark the player-square as visited}
               while StackTop<>0 do begin
                 Col:=Stack[StackTop].X; Row:=Stack[StackTop].Y; Dec(StackTop);
                 for Direction:=Low(Direction) to High(Direction) do begin
                     X:=Col+DIRECTION_XY[Direction,ColAxis];
                     Y:=Row+DIRECTION_XY[Direction,RowAxis];
                     if (Board__[X,Y] and (WALL+FLOOR))=FLOOR then begin {'True': the square is an unvisited floor-square}
                        Dec(Board__[X,Y],FLOOR); {mark the square as visited}
                        Inc(StackTop);
                        Stack[StackTop].X:=X; Stack[StackTop].Y:=Y;
                        end;
                     end;
                 end;
               end;
          end;

        begin {AddSnapshotToLevel}
          Result:=False;
          if Level__<>nil then begin
             Level__.BoardAsTextLines.Reverse;
             try
               if Level__.TextLinesToBoard(Board,BoardWidth,BoardHeight,BoxCount,GoalCount,PlayerPos,s) then begin {text -> internal form}
                  Result:=True;
                  TrimBoard(0,0,Board,BoardWidth,BoardHeight,PlayerPos);

                  {align the boards based on player's position}
                  dx:=PlayerPos.x-PlayerPos__.x; dy:=PlayerPos.y-PlayerPos__.y;
                  if      dx<0 then
                          for Col:=MAX_BOARD_WIDTH downto -dx do
                              for Row:=1 to MAX_BOARD_HEIGHT do Board[Col,Row]:=Board[Col+dx,Row]
                  else if dx>0 then
                          for Col:=1 to MAX_BOARD_WIDTH+1-dx do
                              for Row:=1 to MAX_BOARD_HEIGHT do Board[Col,Row]:=Board[Col+dx,Row];
                  if      dy<0 then
                          for Col:=1 to MAX_BOARD_WIDTH do
                              for Row:=MAX_BOARD_HEIGHT downto -dy do Board[Col,Row]:=Board[Col,Row+dy]
                  else if dy>0 then
                          for Col:=1 to MAX_BOARD_WIDTH do
                              for Row:=1 to MAX_BOARD_HEIGHT+1-dy do Board[Col,Row]:=Board[Col,Row+dy];

                  {fill the boards with outer walls}
                  for Row:=1 to MAX_BOARD_HEIGHT do begin
                      for Col:=1 to MAX_BOARD_WIDTH do begin {change '0'-squares, if any, to walls}
                          if Board  [Col,Row]=0 then
                             Board  [Col,Row]:=WALL;
                          if Board__[Col,Row]=0 then
                             Board__[Col,Row]:=WALL;
                          end;
                      end;

                  {calculate player's reachable squares as if there weren't any boxes on the board}
                  MarkConnectedFloorSquares(Board,PlayerPos);

                  {fill everything but connected floors with walls}
                  for Row:=1 to MAX_BOARD_HEIGHT do
                      for Col:=1 to MAX_BOARD_WIDTH do begin
                          A:=Board[Col,Row];
                          if A<>WALL then
                             if   (A and FLOOR)=0 then {'True': the player can reach the square if there are no boxes on the board}
                                  Inc(Board[Col,Row],FLOOR) {put the FLOOR flag back}
                             else if (A and (PLAYER+BOX+GOAL))=0 then Board[Col,Row]:=WALL;
                          end;

                  {compare the board from the existing level and the new one based on the snapshot}
                  for Col:=1 to Max(BoardWidth__,BoardWidth) do
                      if   Result then
                           for Row:=1 to Max(BoardHeight__,BoardHeight) do
                               if   Result then begin
                                    A:=Board  [Col,Row]; {'A': the square contents in the existing level}
                                    B:=Board__[Col,Row]; {'B': the square contents in the new level based on the snapshot}
                                    if A<>B then begin
                                       if       (A=WALL) and (B=FLOOR) then
                                                Board[Col,Row]:=FLOOR
                                       else if  (A=FLOOR) and (B=WALL) then
                                            else Result:=False;
                                       end;
                                    end
                               else break
                      else break;

                  if Result then begin
                     {NormalizeBoard(BoardWidth,BoardHeight,Board);}
                     RemoveRedundantWalls(BoardWidth,BoardHeight,Board);
                     if CreateObject(otLevel,TNode(TempLevel)) then
                        try     Result:=TempLevel.BoardToTextLines(BoardWidth,BoardHeight,Board);
                                if Result then SwapNodes(TNode(Level__.BoardAsTextLines),TNode(TempLevel.BoardAsTextLines));
                        finally TempLevel.Free;
                        end;
                     end;
                  end;
             finally
               Level__.BoardAsTextLines.Reverse;
             end;
             end;
        end;

      begin {MakeLevelFromSnapshot}
        Result:=False;
        if SnapshotAsText__<>nil then begin
           SnapshotAsText__.MovesAsTextLines.Reverse; {put the lines in correct order}
           if SnapshotAsText__.TextLinesToMoves(History,ReverseMode {$IFDEF SokHtml}, MovePlayerBackIndex {$ENDIF}) and
              {$IFDEF SokHtml} (MovePlayerBackIndex<=0) and {$ENDIF}
              MakeBoardFromMoves(PHistoryMoves(Addr(History.Moves)),History.Top,ReverseMode,BoardWidth,BoardHeight,Board,PlayerPos) then begin
              if   SnapshotAsText__.Notes.Lines.ReadString(KEY_TITLE,Title) and {search snapshot notes for a title}
                   (Title<>'') then {use title from notes}
              else Title:=SnapshotAsText__.Text;
              for  i:=0 to Pred(Length(Title)) do
                   if Title[i+STRING_BASE]<SPACE then Title[i+STRING_BASE]:=UNDERSCORE; {change control characters like TAB and LINEFEED to underscores}

              Title:=MakeTitleThatCannotBeInterpretedAsMoves(
                       StrRemoveCharacters(Title,TITLE_ILLEGAL_CHARACTERS),
                       HasChanged);

              if (Title='') or IsASpecialSnapshotName__(Title,SnapshotType) then Title:=TEXT_LEVEL;

              Level:=TLevel(Levels.GetItemByName(Title));

              Result:=AddSnapshotToLevel(Level,BoardWidth,BoardHeight,Board,PlayerPos);
              if Result then
                 Title:=SnapshotAsText__.Text
              else begin
                 Level:=nil;
                 if CreateObject(otLevel,TNode(Level)) then
                    try
                      if SetLevelName(Level,Title) and
                         Level.BoardToTextLines(BoardWidth,BoardHeight,Board) then begin
                         Level.BoardAsTextLines.Reverse; {put the board lines in reverse order}
                         SwapNodes(TNode(Level.Notes),TNode(SnapshotAsText__.Notes));
                         Title:=SnapshotAsText__.Text;
                         Level.IsMadeFromMoves:=True;
                         Levels.Push(Level);
                         Result:=True;
                         end;
                    finally
                         if Level=Levels.First then begin {'True': the level was successfully built from the snapshot}
                            AddLevelToSet(Level); {save level names for fast duplicate keys checks}
                            end
                         else begin
                            Level.Free; Level:=nil; Result:=False;
                            end;
                    end;
                 end;

              if Result then begin {give the snapshot to 'Level'}
                 if ReverseMode then begin
                    {$IFDEF SokobanYASC}
                      Title:=MainForm.Game.SnapshotTypeName(SokFile_.stSolution);
                    {$ELSE}
                      Title:=TEXT_SOLUTION;
                    {$ENDIF}
                    ReverseHistoryEliminatingJumps(History);
                    Result:=SnapshotAsText__.MovesToTextLines(PHistoryMoves(Addr(History.Moves)),History.Top,History.Top,False,True,False,False);
                    end;

                 V:=TSnapshotAsText(Level.SnapshotsAsText.GetItemByName(Title)); {find snapshot with the same title, if any}
                 if (V<>nil) and Result then begin
                    {$IFDEF SokobanYASC}
                      s:=Level.SnapshotsAsText.MakeUniqueName1(Title,MainForm.Game.SnapshotTypeName(SokFile_.stSolution),True,V.Tag);
                    {$ELSE}
                      s:=Level.SnapshotsAsText.MakeUniqueName1(Title,TEXT_SNAPSHOT,True,V.Tag);
                    {$ENDIF}
                    if   s<>'' then Title:=s
                    else raise Exception.Create(TEXT_MEMORY_FULL);
                    end;
                 Result:=Result and SnapshotAsText__.SetText(Title);

                 if Result then begin
                    SnapshotAsText__.MovesAsTextLines.Reverse; {put the lines back in reverse order}
                    V:=SnapshotAsText__;
                    SnapshotAsText__:=nil; {'nil': so the caller doesn't destroy it later}
                    Level.SnapshotsAsText.Push(V);
                    end;
                 end;
              end;
           end;
      end; {PostProcessLevel.MakeLevelFromSnapshot}

    begin {'PostProcessLevel'; returns 'True' if postprocessing succeeds}
      Result:=True;
      if Level__<>nil then
         if not (Level__.BoardAsTextLines.IsEmpty and MakeBoardFromMovesEnabled) then begin

            Level__.SnapshotsAsText.Reverse; {put the snapshots in correct order before items with identical names are given unique names}
            SnapshotAsText:=TSnapshotAsText(Level__.SnapshotsAsText.First);
            while  (SnapshotAsText<>nil) and Result do begin {for each snapshot...}
                   if SnapshotAsText.Notes.Lines.ReadString(KEY_TITLE,Title) and {search snapshot notes for a title}
                      (Title<>'') then {use title from notes}
                   else begin
                      Title:=SnapshotAsText.Text;
                      if Title='' then
                         Title:=GetTitleFromCommentLineBeginningWithSEMICOLON(SnapshotAsText.Notes.Lines);
                      if (Title='') and                                         {'True': the snapshot doesn't have a name}
                         Level__.BoardAsTextLines.IsEmpty and                   {'True': the level doesn't have a board}
                         (Level__.Text<>'') and                                 {'True': the level has a name from a preceding text line}
                         (SnapshotAsText=TSnapshotAsText(Level__.SnapshotsAsText.First)) and {'True': this is the first snapshot belonging to the level}
                         Level__.Notes.Lines.IsEmpty then begin                 {'True': the level doesn't have any notes, i.e., the moves for the first snapshot come right after the level name from a preceding text line}
                         Title:=Level__.Text;                                   {it's ambiguous whether the name from the preceding text line is the level name of the snapshot name; here it's used for both purposes}
                         end;
                      end;

                   Result:=SetSnapshotName(Level__,SnapshotAsText,Title);

                   SnapshotAsText:=TSnapshotAsText(SnapshotAsText.Next); {next snapshot}
                   end;

            {$IFDEF SokHtml}
              {move optimized solutions to the front of the list; if there are
               duplicate solutions, the optimized versions should take
               precedence because they may have more informative notes, e.g.,
               optimization type and date;}
              SnapshotAsText:=TSnapshotAsText(Level__.SnapshotsAsText.First);
              while  SnapshotAsText<>nil do begin {for each snapshot...}
                     NextSnapshot:=TSnapshotAsText(SnapshotAsText.Next); {next snapshot}
                     if SnapshotAsText.Notes.Lines.ReadString(KEY_OPTIMIZER,Text1) and {search snapshot notes for an optimizer name}
                        (Text1<>'') then {move optimized solutions to the front of the list}
                        Level__.SnapshotsAsText.MoveToFront( TNode( SnapshotAsText ) );
                     SnapshotAsText:=NextSnapshot; {next snapshot}
                     end;
            {$ENDIF}

            if   Level__.Notes.Lines.ReadString(KEY_TITLE,Title) and {search notes for a title}
                 (Title<>'') then {use title from the notes}
            else Title:=Level__.Text;
            if Title='' then begin
               Title:=GetTitleFromCommentLineBeginningWithSEMICOLON(Level__.Notes.Lines);
               if Title<>'' then Level__.Notes.Clear; {drop the "; <title>" line}
               end;

            Result:=SetLevelName(Level__,Title) and Result;

            if Result then begin
               AddLevelToSet(Level__); {save level names for fast duplicate keys checks}
               if   (Level__.Next=nil) and  {'True': first level}
                    (not Fileheader.Lines.IsEmpty) then
                    {search for "false macros", i.e., lines with "=" coming from
                     the first level's title header line, but interpreted as
                     key-value macros belonging to the file header because of
                     the "=" character}
                    TrimFileHeaderForDuplicateMacros(Level__.Name);
               {$IFDEF SokobanYASC}
                 if ((LevelCount mod 1000)=0) then begin {show progress for large files}
                    if      Screen.ActiveForm=OpenForm then with OpenForm.StatusBar1 do begin
                            Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=IntToStr(LevelCount);
                            Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:=Level__.Name;
                            if not OpenForm.PanelToolTips.Visible then
                               Repaint
                            else begin
                               OpenForm.PanelToolTips.Hide; OpenForm.Update;
                               end;
                            end
                    else if Screen.ActiveForm=DuplicatesForm then with DuplicatesForm.StatusBar1 do begin
                            Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:=IntToStr(LevelCount);
                            Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:=Level__.Name;
                            Repaint;
                            end;
                    end;
               {$ENDIF}
               end;

            Level__:=nil;
            end
         else {try to create levels based on snapshots, i.e., solutions and saved games}
            if Level__=Levels.First then begin  {'=': should always be true}
               Levels.Pop; {remove the level from the collection}
               try
                  Level__.SnapshotsAsText.Reverse;
                  while  (not Level__.SnapshotsAsText.IsEmpty) and Result do begin {for each snapshot...}
                         SnapshotAsText:=TSnapshotAsText(Level__.SnapshotsAsText.Pop);
                         try     MakeLevelFromSnapshot(SnapshotAsText); {note that the return value isn't used, in effect dropping the snapshot in case it cannot create a level}
                         finally SnapshotAsText.Free; {destroy the snapshot unless it was given to another level by 'MakeLevelFromSnapshot'}
                         end;
                         end;
               finally
                  Level__.Free; Level__:=nil;
               end;
               end
            else Result:=False;
    end; {PostProcessLevel}

{
    The state-machine in "Load"
    ---------------------------
    The easiest way to describe the load function is with a piece of pseudo-code,
    concentrating on file-header, boards, titles, and notes, but omitting snapshots.

    set state to fileHeader
    while more lines do
      case state of
        fileHeader        if   next line is a board-line then
                               change state to newLevel
                               check if file-header contains a title for first level
                          else add next line to file-header and advance
                          (break, for C-programmers)

        newLevel          create new level
                          set level-title to the stored title, if any
                          repeat
                            add next line to board-lines and advance
                          until eof or the next line isn't a board-line
                          set state to levelNotes
                          (break, for C-programmers)

       levelNotes        if   next line is a board-line then
                              change state to newLevel
                              check if level-notes contains a title for next level
                         else add next line to level-notes and advance
      endcase
    endwhile

    The reason for using a state-machine is that when the parser reads a text-
    line, it cannot know whether it is a comment or the title of the next
    level (or snapshot).
}

  begin {Load}
    Result:=True; AnonymousLevel__:=nil;
    Level:=nil; SnapshotAsText:=nil; Title:='';
    LevelCount:=0; BoardLinesCount:=0; MoveLinesCount:=0; ChessNotationMoveCount:=0;
    State:=stFileHeader;
    BracketedSectionNameInfo  :=StrWithBrackets(SECTION_NAME_INFORMATION);
    FillChar(LevelHashBucketVector,SizeOf(LevelHashBucketVector),0);
    BloomFilterInitialize(LevelNameSet,BLOOM_FILTER_TABLE_BYTE_SIZE,Addr(LevelNameSetByteVector));
    while (Lines__.First<>nil) and Result do
      case State of
        stFileHeader    : if        IsBoard(Lines__,BoardLinesCount) or
                                    IsMoves(Lines__,MoveLinesCount,ChessNotationMoveCount) then begin
                                    Title:=ConvertLastLinesToATitle(FileHeader.Lines,'');
                                    State:=stLevel;
                                    {check if the puzzle type is supported by this program (disabled; the "puzzle type" keyword isn't implemented}
                                    OK:=True or (not FileHeader.Lines.ReadString(KEY_PUZZLE_TYPE,PuzzleTypeString));
                                    if not OK then
                                       for p:=Low(p) to High(p) do
                                           if StrEqual(KEY_PUZZLE_TYPES[p],PuzzleTypeString) then begin
                                              OK:=True; PuzzleType:=p;
                                              end;
                                    if not OK then Lines__.Clear;   {the lines don't contain any levels supported by this program}
                                    end
                          else      FileHeader.Lines.Push(TrimLeadingEmailQuotes(Lines__.Pop)); {save the header line}
        stLevel         : begin     Result:=PostProcessLevel(Level) and {finish processing the previous level, if any}
                                            CreateObject(oTLevel,TNode(Level));
                                    if Result then begin
                                       Levels.Push(Level);
                                       while BoardLinesCount>0 do begin
                                         Level.BoardAsTextLines.Push(Lines__.Pop);
                                         Dec(BoardLinesCount);
                                         end;
                                       if MoveLinesCount>0 then begin  {no board: (we're probably loading snapshots or solutions of the current game)}
                                          State:=stSnapshot;
                                          end
                                       else begin
                                          Level.Text:=Title; Title:='';
                                          State:=stLevelBoard;
                                          end;
                                       end;
                          end;
        stLevelBoard    : if        IsABoardLine(Lines__.First,0,0,h,i,j,k) then
                                    Level.BoardAsTextLines.Push(Lines__.Pop)
                          else      State:=stLevelNotes;
        stLevelNotes    : if        IsBoard(Lines__,BoardLinesCount) then begin
                                    Title:=ConvertLastLinesToATitle(Level.Notes.Lines,'');
                                    State:=stLevel;
                                    end
                          else if   IsMoves(Lines__,MoveLinesCount,ChessNotationMoveCount) then begin
                                    Title:=ConvertLastLinesToATitle(Level.Notes.Lines,'');
                                    State:=stSnapshot;
                                    end
                               else if   StrEqual(BracketedSectionNameInfo,Lines__.First.Text) then
                                         Lines__.Pop.Free {an early version used '[Information]' as tag for level-notes}
                                    else Level.Notes.Lines.Push(TrimLeadingEmailQuotes(Lines__.Pop));
        stSnapshot      : begin     Result:=CreateObject(oTSnapshotAsText,TNode(SnapshotAsText));
                                    if Result then begin
                                       if (Level.Text='') and (Title<>'') and
                                          Level.BoardAsTextLines.IsEmpty and
                                          (not MakeBoardFromMovesEnabled) then begin
                                          Level.Text:=Title; Title:='';
                                          end;
                                       SnapshotAsText.Text:=Title; Title:='';
                                       Level.SnapshotsAsText.Push(SnapshotAsText);
                                       while MoveLinesCount>0 do begin
                                         SnapshotAsText.MovesAsTextLines.Push(Lines__.Pop);
                                         Dec(MoveLinesCount);
                                         end;
                                       State:=stSnapshotMoves;
                                       end;
                          end;
        stSnapshotMoves:  if        IsAMoveLine(Lines__.First.Text,MAX_BOARD_HEIGHT,ChessNotationMoveCount) then {'MAX_BOARD_HEIGHT': actually, any non-zero value will do}
                                    SnapshotAsText.MovesAsTextLines.Push(Lines__.Pop)
                          else      begin ChessNotationMoveCount:=0;
                                          State:=stSnapshotNotes;
                                    end;
        stSnapshotNotes:  if        IsBoard(Lines__,BoardLinesCount) then begin
                                    Title:=ConvertLastLinesToATitle(SnapshotAsText.Notes.Lines,'');
                                    if Level.BoardAsTextLines.IsEmpty and (not MakeBoardFromMovesEnabled) then begin {current level has no board; drop it}
                                       Levels.Pop.Free; Level:=nil;
                                       end;
                                    State:=stLevel;
                                    end
                          else if   IsMoves(Lines__,MoveLinesCount,ChessNotationMoveCount) then begin
                                    Title:=ConvertLastLinesToATitle(SnapshotAsText.Notes.Lines,'');
                                    if   Level.BoardAsTextLines.IsEmpty
                                         and
                                         ParseSnapshotsWithoutBoardsAsIndividualLevels then
                                         State:=stLevel
                                    else State:=stSnapshot;
                                    end
                               else if   StrEqual(BracketedSectionNameInfo,Lines__.First.Text) then begin
                                         State:=stLevelNotes; {an early version used '[Information]' as tag for level-notes}
                                         Lines__.Pop.Free;
                                         end
                                    else SnapshotAsText.Notes.Lines.Push(TrimLeadingEmailQuotes(Lines__.Pop));
      end; {case}
    if Result then;
    Result:=Result and PostProcessLevel(Level); {finish processing the last level, if any}

    {$IFDEF SokobanYASC}
      if (LevelCount>=1000) and Assigned(OpenForm) then with OpenForm.StatusBar1 do begin
         Panels[OPEN_FORM_PANEL_INDEX_STATE].Text:='';
         Panels[OPEN_FORM_PANEL_INDEX_HINT ].Text:='';
         Repaint;
         end;
    {$ENDIF}

  end; {Load}

begin {LoadFromTextLines}
{ TimeMS:=GetTickCount;}
  Result:=(FileHeader<>nil) and (Levels<>nil) and Flush and (Lines__<>nil);
  if Result then
     try    Clear;
            Result:=Load(Lines__,AnonymousLevel);

            if  (Levels.Count=1) and Result then begin
                Level:=TLevel(Levels.First); Title:=Level.Name; i:=-1;
                if   Level.SetName('') and
                     Level.SetName(Levels.MakeUniqueName1('',TEXT_LEVEL,False,i)) and
                     (Level.Name=Title) then
                     {substitute the title 'Level 1' by 'Level',}
                     {indicating to show filename as level title instead of level name}
                     Result:=Level.SetName(TEXT_LEVEL)
                else Result:=Level.SetName(Title);     {restore the original title}
                end;

            if (AnonymousLevel<>nil) and (AnonymousLevel.Tag.Value>1) and Result then begin
               {try to substitute the title 'Level' with 'Level 1'}
               Title:=TEXT_LEVEL+' 1';
               if not Levels.ItemExists(Title) then
                  Result:=AnonymousLevel.SetName(Title);
               end;

            if Result then begin
               {most of the items are in reverse order: make it right before exiting}
               FileHeader.Lines.Reverse; FileHeader.Lines.TrimBlankLines;
               Levels.Reverse;
               Level:=TLevel(Levels.First);
               while (Level<>nil) and Result do begin {for each level...}
                 Level.BoardAsTextLines.Reverse;
                 Level.Notes.Lines.Reverse; Level.Notes.Lines.TrimBlankLines;

                 SnapshotAsText:=TSnapshotAsText(Level.SnapshotsAsText.First);
                 while (SnapshotAsText<>nil) and Result do begin {for each snapshot...}
                   SnapshotAsText.MovesAsTextLines.Reverse;
                   SnapshotAsText.Notes.Lines.Reverse; SnapshotAsText.Notes.Lines.TrimBlankLines;
                   SnapshotAsText.Tag:=0; {clear the tag; it was used during loading for numbering levels with identical names}
                   SnapshotAsText:=TSnapshotAsText(SnapshotAsText.Next);
                   end;

                 Level.Flags:=0; {clear the flags; it was used during loading for hash-table bucket-chains}
                 Level.Tag.Value:=0; {clear the tag; it was used during loading for numbering levels with identical names}
                 Level:=TLevel(Level.Next);
                 end;
               end;

     except on E:Exception do Result:=SokUtil_.Error(E.Message,'TSokoFile.LoadFromTextLines');
     end;
{
  TimeMS:=CalculateElapsedTimeMS(TimeMS,GetTickCount);
  if TimeMS>100 then SokUtil_.Msg(IntToStr(TimeMS),'Time',MB_OK);
}
  if not Result then Clear;
end; {TSokoFile.LoadFromTextLines}

procedure TSokoFile.MacroExpandNotes(MacroExpandLevelNotes__,MacroExpandSnapshotNotes__:Boolean);
var Level:TLevel;
begin
  Level:=TLevel(Levels.First);
  while Level<>nil do begin
    Level.MacroExpandNotes(MacroExpandLevelNotes__,MacroExpandSnapshotNotes__,FileHeader.Lines);
    Level:=TLevel(Level.Next);
    end;
end;

function TSokoFile.MergeSokoFile(NewSokoFile__:TSokoFile; AcceptSmallDifferences__,DeleteEmptyUnofficiallyReleasedLevels__,ImportLevels__:Boolean):Boolean;
{Merges new levels, new solutions, and new snapshots from another file; items from the new file have precedence, except for level notes;
 if 'ImportLevels__' is 'True', then the merged levels end up in this file and the other file is emptied;
 if 'ImportLevels__' is 'False', then the merged levels end up in the other file and this file is emptied;}
var i:Integer; IsAnUnOfficiallyReleasedLevel:Boolean; s:String; TimeMS:TTimeMS;
    Node:TNode; UndefinedMacros:TList;
    NewLevel,OldLevel:TLevel; NewSnapshotAsText,OldSnapshotAsText:TSnapshotAsText;

  procedure CheckForUndefinedMacros(Notes__,FileHeader__:TNotes; var UndefinedMacros__:TList);
  var i,MacroBeginLength,MacroEndLength:Integer; s,Key:String; Node,n:TNode;
  begin
    if (Notes__<>nil) and (Notes__.Lines<>nil) then begin
       MacroBeginLength:=System.Length(MACRO_BEGIN);
       MacroEndLength  :=System.Length(MACRO_END);

       Node:=Notes__.Lines.First;
       while Node<>nil do begin                                      {for each line}
         i:=AnsiPos(MACRO_BEGIN,Node.Text);                          {look for macro-start}
         if i>=STRING_BASE then begin
            s:=Node.Text;
            repeat
              Delete(s,STRING_BASE,i-STRING_BASE+MacroBeginLength);
              i:=AnsiPos(MACRO_END,s);                               {look for macro-end}
              if i>=STRING_BASE then begin
                 Key:=System.Copy(s,STRING_BASE,i-STRING_BASE);
                 Delete(s,STRING_BASE,i-STRING_BASE+MacroEndLength);
                 if (Key<>'') and
                    (not FileHeader__.Lines.FindKey(Key,n)) and      {is the macro defined in the fileheader?}
                    (UndefinedMacros__.GetItemByName(Key)=nil) and   {has this macro already been listed as undefined?}
                    CreateObject(otNode,n) then begin
                    UndefinedMacros__.Push(n); n.SetText(Key);       {save name of this undefined macro}
                    end;
                 end;
              i:=AnsiPos(MACRO_BEGIN,s);                             {look for next macro-start}
            until i<STRING_BASE;
            end;
         Node:=Node.Next;                                            {next line}
         end;
       end;
  end;

  function  HasAnIdenticalBuiltinSolution(const Level1__,Level2__:TLevel):Boolean;
  var Solution1,Solution2:TSnapshotAsText;
  begin {Returns 'True' if the levels have at least 1 built-in solution in common}
    Solution1:=TSnapshotAsText(Level1__.SnapshotsAsText.GetItemByName(SNAPSHOT_TYPE_NAME[stBestSolutionMoves]));
    {$IFDEF SokobanYASC}
      if Solution1=nil then
         Solution1:=TSnapshotAsText(Level1__.SnapshotsAsText.GetItemByName(MainForm.Game.SnapshotTypeName(stBestSolutionMoves)));
      if Solution1=nil then
         Solution1:=TSnapshotAsText(Level1__.SnapshotsAsText.GetItemByName(MainForm.Game.SnapshotTypeName(stSolution)));
    {$ENDIF}
    Solution2:=TSnapshotAsText(Level2__.SnapshotsAsText.GetItemByName(SNAPSHOT_TYPE_NAME[stBestSolutionMoves]));
    {$IFDEF SokobanYASC}
      if Solution2=nil then
         Solution2:=TSnapshotAsText(Level2__.SnapshotsAsText.GetItemByName(MainForm.Game.SnapshotTypeName(stBestSolutionMoves)));
      if Solution2=nil then
         Solution2:=TSnapshotAsText(Level2__.SnapshotsAsText.GetItemByName(MainForm.Game.SnapshotTypeName(stSolution)));
    {$ENDIF}
    Result:=(Solution1<>nil) and
            (Solution2<>nil) and
            Solution1.MovesAsTextLines.IsEqual(Solution2.MovesAsTextLines);

    Solution1:=TSnapshotAsText(Level1__.SnapshotsAsText.GetItemByName(SNAPSHOT_TYPE_NAME[stBestSolutionPushes]));
    {$IFDEF SokobanYASC}
      if Solution1=nil then
         Solution1:=TSnapshotAsText(Level1__.SnapshotsAsText.GetItemByName(MainForm.Game.SnapshotTypeName(stBestSolutionPushes)));
      if Solution1=nil then
         Solution1:=TSnapshotAsText(Level1__.SnapshotsAsText.GetItemByName(MainForm.Game.SnapshotTypeName(stSolution)));
    {$ENDIF}
    Solution2:=TSnapshotAsText(Level2__.SnapshotsAsText.GetItemByName(SNAPSHOT_TYPE_NAME[stBestSolutionPushes]));
    {$IFDEF SokobanYASC}
      if Solution2=nil then
         Solution2:=TSnapshotAsText(Level2__.SnapshotsAsText.GetItemByName(MainForm.Game.SnapshotTypeName(stBestSolutionPushes)));
      if Solution2=nil then
         Solution2:=TSnapshotAsText(Level2__.SnapshotsAsText.GetItemByName(MainForm.Game.SnapshotTypeName(stSolution)));
    {$ENDIF}
    Result:=Result or
            ((Solution1<>nil) and
             (Solution2<>nil) and
             Solution1.MovesAsTextLines.IsEqual(Solution2.MovesAsTextLines));
  end;

  function  HasMacros(Notes__:TNotes):Boolean;
  var Node:TNode;
  begin
    Result:=False;
    if (Notes__<>nil) and (Notes__.Lines<>nil) then begin
       Node:=Notes__.Lines.First;
       while (Node<>nil) and (not Result) do begin                   {for each line}
         if (AnsiPos(MACRO_BEGIN,Node.Text)>=STRING_BASE) and        {look for a macro-start}
            (AnsiPos(MACRO_END  ,Node.Text)>=STRING_BASE) then       {look for a macro-end}
            Result:=True;                                            {this check is not fool-proof, but there is no reason to make a more complicated analysis}
         Node:=Node.Next;                                            {next line}
         end;
       end;
  end;

  function  IsFloor(Char__:Char):Boolean;
  begin {returns 'True' if the character is a control character, a SPACE, or
         one of the other characters which represent a floor character}
    Result := (Char__<=SPACE) or {control characters and space are handled as floor-squares}
              (Char__=FLOOR_CH) or
              (Char__=FLOOR_NON_BLANK_CH1) or
              (Char__=FLOOR_NON_BLANK_CH2);
  end;

  function  FirstNonFloorCharIndex(const Str__:String):Integer;
  var LastCharIndex:Integer;
  begin
    if   Str__<>'' then begin
         Result:=0; LastCharIndex:=StrLastCharIndex(Str__)-STRING_BASE; {'-STRING_BASE: 0-based 'while' iteration in the next line}
         while (Result<=LastCharIndex) and IsFloor(Str__[Result+STRING_BASE]) do Inc(Result);
         Inc( Result, STRING_BASE ); {'+STRING_BASE': in Pascal, string indices are 1-based}
         end
    else Result:=Pred(STRING_BASE);
  end;

  function  IsEqualExceptFloorsAndWalls(OldBoardAsTextLines1__,NewBoardAsTextLines2__:TList):Boolean;
  var i,j,k,m:Integer; Node1,Node2:TNode;
  begin
    Result:=True;
    Node1:=OldBoardAsTextLines1__.First; Node2:=NewBoardAsTextLines2__.First;
    while Result and (Node1<>nil) and (Node2<>nil) do begin
       j:=StrLastCharIndex(Node1.Text); k:=StrLastCharIndex(Node2.Text);
       while (j>STRING_BASE) and (Node1.Text[j]<=SPACE) do Dec(j);   {drop trailing spaces}
       while (k>STRING_BASE) and (Node2.Text[k]<=SPACE) do Dec(k);
       m:=Min(j,k); {common characters length}
       for i:=STRING_BASE to m do
           if   Result then
                Result:=( Node1.Text[i]=Node2.Text[i])
                        or
                        (IsFloor(Node1.Text[i])   and IsFloor(Node2.Text[i]))
                        or
                        ((Node1.Text[i]= WALL_CH) and IsFloor(Node2.Text[i]))
                        or
                        ((Node2.Text[i]= WALL_CH) and IsFloor(Node1.Text[i]))
           else break;
       for i:=Succ(m) to j do {if the old line is longer, then the extra characters must be walls or spaces}
           if   Result then
                Result:= (Node1.Text[i]= WALL_CH) or  IsFloor(Node1.Text[i])
           else break;
       for i:=Succ(m) to k do {if the new line is longer, then the extra characters must be walls or spaces}
           if   Result then
                Result:= (Node2.Text[i]= WALL_CH) or  IsFloor(Node2.Text[i])
           else break;

       Node1:=Node1.Next; Node2:=Node2.Next; {next line}
       end;
    Result:=Result and (Node1=nil) and (Node2=nil);
  end;

  function LeftJustify(var BoardAsTextLines__:TList):Integer; {throws EOutOfMemory}
  var Node:TNode;
  begin
     Result:=High(Result); Node:=BoardAsTextLines__.First;
     while (Result>STRING_BASE) and (Node<>nil) do begin
       if Node.Text<>'' then
          Result:=Min(Result,FirstNonFloorCharIndex(Node.Text));
       Node:=Node.Next;
       end;
     if   (Result>STRING_BASE) and (Result<High(Result)) then begin
          Node:=BoardAsTextLines__.First;
          while Node<>nil do begin
            Node.Text:=System.Copy(Node.Text,Result,Length(Node.Text)); {caution: 'Copy(source,index,count)': index is a string-index, in Pascal they are 1-based}
            Node:=Node.Next;
            end;
          end
     else Result:=STRING_BASE;
  end;

begin {MergeSokoFile}
  Result:=(NewSokoFile__<>nil) and (PuzzleType=NewSokoFile__.PuzzleType);
  if Result then begin
     UndefinedMacros:=nil;
     if Flush then begin {save any changes to disk}
        try
          try
            if CreateObject(otList,TNode(UndefinedMacros)) then begin
               NewLevel:=TLevel(NewSokoFile__.Levels.First); {find macros used in the other file, but undefined in this file}
               while NewLevel<>nil do begin
                  CheckForUndefinedMacros(NewLevel.Notes,NewSokoFile__.FileHeader,UndefinedMacros);
                  NewSnapshotAsText:=TSnapshotAsText(NewLevel.SnapshotsAsText.First);
                  while NewSnapshotAsText<>nil do begin
                    CheckForUndefinedMacros(NewSnapshotAsText.Notes,NewSokoFile__.FileHeader,UndefinedMacros);
                    NewSnapshotAsText:=TSnapshotAsText(NewSnapshotAsText.Next);
                    end;
                  NewLevel:=TLevel(NewLevel.Next); {next level}
                  end;

               OldLevel:=TLevel(Levels.First);
               while OldLevel<>nil do begin
                 Node:=OldLevel.Next; {next level}

                 IsAnUnOfficiallyReleasedLevel:=False;

                 NewLevel:=TLevel(NewSokoFile__.Levels.GetItemByName(OldLevel.Name));
                 if NewLevel=nil then begin
                    i:=AnsiPos(KEY_UNOFFICIAL_RELEASE,OldLevel.Name);
                    if i>=STRING_BASE then begin
                       IsAnUnOfficiallyReleasedLevel:=True;
                       s:=OldLevel.Name; Delete(s,i,Length(KEY_UNOFFICIAL_RELEASE)); s:=Trim(s);
                       NewLevel:=TLevel(NewSokoFile__.Levels.GetItemByName(s));
                       end;
                    end;

                 if (NewLevel<>nil)
                    and
                    (not OldLevel.BoardAsTextLines.IsEmpty) {if the board is empty, then the 'level' is probably a solution}
                    and
                    (not OldLevel.BoardAsTextLines.IsEqual(NewLevel.BoardAsTextLines))
                    and
                    (not (((LeftJustify(OldLevel.BoardAsTextLines)+LeftJustify(NewLevel.BoardAsTextLines))>(2*STRING_BASE)) {ensure that the boards are left-justified}
                          and
                          OldLevel.BoardAsTextLines.IsEqual(NewLevel.BoardAsTextLines)
                         )
                    )
                    and
                    (not (AcceptSmallDifferences__
                          and
                          IsEqualExceptFloorsAndWalls(OldLevel.BoardAsTextLines,NewLevel.BoardAsTextLines)
                         )
                         {if the boards are the same except along the edges, then it's probably}
                         {a 'beautified' version of the same level where some outer walls have}
                         {been removed}
                    )
{
                    and
                    (not HasAnIdenticalBuiltinSolution(OldLevel,NewLevel))
}
                    then
                    NewLevel:=nil; {levels don't match, although they have identical names}

                 if NewLevel=nil then begin {'True': level not found by name lookup; try harder, i.e., by comparing levels}
                    NewLevel:=TLevel(NewSokoFile__.Levels.First);
                    while (NewLevel<>nil) {look for an identical level}
                          and
                          (not OldLevel.BoardAsTextLines.IsEqual(NewLevel.BoardAsTextLines))
{
                          and
                          (not HasAnIdenticalBuiltinSolution(OldLevel,NewLevel))
}
                          do
                          NewLevel:=TLevel(NewLevel.Next);
                    end;

                 if NewLevel<>nil then begin {matching levels}
                    if   (not NewLevel.Notes.Lines.IsEmpty)
                         and
                         ((not StrEqual(OldLevel.Name,NewLevel.Name))
                          or
                          HasMacros(OldLevel.Notes)
                         ) then begin
                         {use new notes if the original notes haven't been macro-expanded}
                         {because this means that the notes probably haven't been modified;}
                         {this is the best choice, although it's not 100% fool-proof}
                         if    OldLevel.Notes.Lines.ReadString (KEY_TIME,s) and
                               SokUtil_.StrToTime(s,TimeMS) then begin {transfer 'Time' to the new notes}
                               NewLevel.Notes.Lines.AddBlankLine;
                               NewLevel.Notes.Lines.WriteString(KEY_TIME,SokUtil_.TimeToStr(TimeMS));
                               end;
                         if    OldLevel.Notes.Lines.ReadString (KEY_BOARD_TRANSFORMATION,s) then begin {transfer rotate/mirror state to the new notes}
                               NewLevel.Notes.Lines.AddBlankLine;
                               NewLevel.Notes.Lines.WriteString(KEY_BOARD_TRANSFORMATION,s);
                               end;
                         end
                    else if not OldLevel.Notes.Lines.IsEmpty then
                            SwapNodes(TNode(OldLevel.Notes),TNode(NewLevel.Notes)); {use old notes}

                    {transfer snapshots and solutions from the old file to the new file}
                    while not OldLevel.SnapshotsAsText.IsEmpty do begin
                      OldSnapshotAsText:=TSnapshotAsText(OldLevel.SnapshotsAsText.Pop);
                      if   NewLevel.LookupSnapshotAsText(OldSnapshotAsText.MovesAsTextLines,NewSnapshotAsText) then begin
                           if        NewSnapshotAsText.IsASaveGame and
                                     (not OldSnapshotAsText.IsASaveGame) and
                                     (System.Pos(TEXT_BUILT_IN,OldSnapshotAsText.Name)<STRING_BASE) and
                                     NewSnapshotAsText.SetText(OldSnapshotAsText.Name) then begin
                                     {the new snapshot has now changed its name from a (temporary) savegame to a persistent snapshot or solution}
                                     OldSnapshotAsText.Free;
                                     end
                           else if   StrEqual(OldSnapshotAsText.Name,NewSnapshotAsText.Name)
                                     or
                                     ((System.Pos(TEXT_BUILT_IN,OldSnapshotAsText.Name)>=STRING_BASE)
                                      and
                                      (System.Pos(TEXT_BUILT_IN,NewSnapshotAsText.Name)< STRING_BASE)
                                     )
                                     or
                                     (OldSnapshotAsText.IsASaveGame and (not NewSnapshotAsText.IsASaveGame))
                                     then
                                     OldSnapshotAsText.Free {the new file contains a matching snapshot or solution; drop the old one}
                                else NewLevel.SnapshotsAsText.Add(OldSnapshotAsText); {add the old snapshot to the new file}
                           end
                      else NewLevel.SnapshotsAsText.Add(OldSnapshotAsText); {add the old snapshot to the new file}
                      end;
                    end
                 else
                    if   IsAnUnOfficiallyReleasedLevel and DeleteEmptyUnofficiallyReleasedLevels__ and OldLevel.SnapshotsAsText.IsEmpty then
                         {do nothing, i.e., drop unofficially released levels if there are no snapshots}
                    else NewSokoFile__.Levels.Add(Levels.Remove(OldLevel,False)); {transfer the old level from this file to the new collection}

                 OldLevel:=TLevel(Node); {next level}
                 end;

               if not UndefinedMacros.IsEmpty then begin {add user-defined macros to the file-header}
                  if CreateObject(otNode,Node) then NewSokoFile__.FileHeader.Lines.Add(Node); {insert blank line}
                  UndefinedMacros.Reverse;
                  while not UndefinedMacros.IsEmpty do begin
                    Node:=UndefinedMacros.Pop;
                    if   (Node.Text<>'') and
                         FileHeader.Lines.FindKey(Node.Text,Node) then begin
                         NewSokoFile__.FileHeader.Lines.Add(FileHeader.Lines.Remove(Node,False));
                         if CreateObject(otNode,Node) then NewSokoFile__.FileHeader.Lines.Add(Node); {insert blank line}
                         end
                    else Node.Free;
                    end;
                  NewSokoFile__.FileHeader.Lines.TrimBlankLines;
                  end;

               if FileHeader.Lines.ReadString(KEY_DATE_LAST_CHANGE,s) then
                  NewSokoFile__.FileHeader.Lines.WriteString(KEY_DATE_LAST_CHANGE,s);

               NewSokoFile__.FileHeader.Lines.TrimBlankLines;

               if ImportLevels__ then begin {the merged levels should end up in this file}
                  SwapNodes(TNode(FileHeader),TNode(NewSokoFile__.FileHeader)); {transfer the new fileheader to this file}
                  SwapNodes(TNode(Levels),TNode(NewSokoFile__.Levels)); {transfer the merged levels to this file}
                  NewSokoFile__.Clear; NewSokoFile__.SetName('');
                  Modified:=True;
                  end
               else begin {the merged levels should end up in the other file}
                  if not NewSokoFile__.Levels.IsEmpty then
                     NewSokoFile__.Modified:=True;
                  Clear; SetName(''); 
                  end;

               Result:=True;
               end
            else Result:=False;
          finally UndefinedMacros.Free;
          end;
        except on E:Exception do Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_MERGE_FILES);
        end;
        if Result then Result:=Flush; {save changes to disk}
        if not Result then begin Clear; SetName(''); end;
        end;
     end;
end; {MergeSokoFile}

function TSokoFile.MergeTextFile(const FileName__:String; AcceptSmallDifferences__,DeleteEmptyUnofficiallyReleasedLevels__:Boolean):Boolean;
{Merges new levels, new solutions, and new snapshots from another file; items from the new file have precedence, except for level notes}
var NewSokoFile:TSokoFile;
begin {MergeTextFile}
  Result:=False; NewSokoFile:=nil;
  if Flush then begin {save any changes to disk}
     try    if CreateObject(otSokoFile,TNode(NewSokoFile)) then
               try    if   FileExists(FileName__) then
                           if   NewSokoFile.Open(FileName__) then
                                Result:=MergeSokoFile(NewSokoFile,AcceptSmallDifferences__,DeleteEmptyUnofficiallyReleasedLevels__,True)
                           else Result:=False
                      else raise Exception.Create(Format(TEXT_FILE_NOT_FOUND_FORMAT,[FileName__]));
               finally NewSokoFile.Free;
               end;
     except on E:Exception do Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_MERGE_FILES);
     end;
     if Result then Result:=Flush; {save changes to disk}
     if not Result then begin Clear; SetName(''); end;
     end;
end; {TSokoFile.MergeTextFile}

function  TSokoFile.New(const FileName__:String):Boolean;
begin
  Result:=Close;
  if Result then begin
     Result:=SetName(FileName__)
             and
             (UpdateFileFormatDescription(False)
              or
              (not AddFileFormatDescriptionToFiles)
             )
             and
             ((not AddFileFormatDescriptionToFiles)
              or
              FileHeader.Lines.WriteString(KEY_DATE_CREATED,FormatDateTime(FORMAT_DATE_TIME,Now))
             );
     if   Result then Modified:=True
     else begin Clear; SetName(''); end;
     end;
end; {TSokoFile.New}

function TSokoFile.ObjectType:TObjectType;
begin
  Result:=otSokoFile;
end; {TSokoFile.ObjectType}

function TSokoFile.Open(const FileName__:String):Boolean;
begin
  Result:=StrEqual(FileName__,Name) and (not IsBlank(FileName__)); {is file already open? ('IsBlank': always open clipboard)}
  if (not Result) and {no: try to open or create it}
     Close then begin {close current file, if any}
     try       if   IsBlank(FileName__) or FileExists(FileName__) then
                    Result:=LoadFromFile(FileName__) {load existing file (or load from clipboard if name='')}
               else Result:=New(FileName__);         {create new file}
     except on E:Exception do Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_OPEN_FILE);
     end;
     if not Result then begin Clear; SetName(''); end;
     end;
end; {TSokoFile.Open}

function TSokoFile.RemoveFileFormatDescription:Boolean;
var i,FirstLineNo,LastLineNo,MajorVersionNo,MinorVersionNo:Integer;
begin
  Result:=HasFileFormatDescription(FirstLineNo,LastLineNo,MajorVersionNo,MinorVersionNo);
  if Result then with FileHeader.Lines do
     for i:=FirstLineNo to LastLineNo do
         Remove(GetItemByIndex(Pred(FirstLineNo)),True); {'Pred': 'FirstLineNo'  is 1-based; substract 1 to change it to the 0-based number}
end; {TSokoFile.RemoveFileFormatDescription}

function TSokoFile.SaveToFile(const FileName__:String):Boolean;
var IOResult:Integer; OldHasBoardTags:Boolean;
    s,TempName:String; F:TextFile;
    {$IFDEF SokobanYASC}
      OriginalName:String; ExistingFileFileTime:TFileTime;
    {$ENDIF}
begin
  Result:=(FileHeader<>nil) and (Levels<>nil);
  if Result then
     try TempName:=Trim(FileName__);
{        if IsBlank(TempName) then
            TempName:=MakeUniqueFileName('',TEXT_APPLICATION_TITLE,SOKOBAN_FILE_NAME_EXT);
}
         Result:=SetName(TempName); IOResult:=0;
         if Result then
            if Name='' then
               raise Exception.Create(TEXT_NO_FILE_SPECIFIED)
            else begin
               {$IFDEF SokobanYASC}
                 if ApplicationMutex<>0 then begin
                    if   WaitForSingleObject(ApplicationMutex,INFINITE)=WAIT_OBJECT_0 then begin
                         if ((FileTime.dwLowDateTime<>0) or (FileTime.dwHighDateTime<>0)) then
                            {try to make the file time for the file saved
                             further down differ from the file time for any
                             previously saved version of file with a measurable
                             amount
                            }
                            SleepEx(10,False);
                         end
                    else raise Exception.Create(TEXT_PROCESS_SYNCHRONIZATION_ERROR);
                    end;
                 try
               {$ENDIF}
                   TempName:=MakeUniqueFileName(ExtractFilePath(Name),ExtractFileNameWithoutPathAndExtension(Name),ExtractFileExt(Name));
                   if TempName<>'' then begin
                      AssignFile(F,TempName);
                      Rewrite(F);
                      OldHasBoardTags:=SokFile_.HasBoardTags;
                      try     SokFile_.HasBoardTags:=Self.HasBoardTags;
                              Inc(IOResult,FileHeader.WriteToFile(Addr(F))); {write file header, ie., notes, macros, etc.}
                              if IOResult=0 then
                                 Inc(IOResult,Levels.WriteToFile(Addr(F)));  {write levels}
                      finally SokFile_.HasBoardTags:=OldHasBoardTags;
                              CloseFile(F);
                      end;
                      end
                   else IOResult:=-1;

                   if IOResult=0 then {writing the file to disk succeeded}
                      if not StrEqual(Name,TempName) then begin
                         {$IFDEF SokobanYASC}
                           {if the existing file has been updated after the
                            levels were loaded by this application, then merge
                            the loaded levels and the levels from the existing
                            file; this doesn't solve all synchronization issues
                            with multiple concurrently running instances of this
                            application, but it's better than nothing;
                           }
                           if ((FileTime.dwLowDateTime<>0) or (FileTime.dwHighDateTime<>0)) and
                              GetFileTime(Name,ExistingFileFileTime) and
                              ((ExistingFileFileTime.dwLowDateTime <>FileTime.dwLowDateTime ) or
                               (ExistingFileFileTime.dwHighDateTime<>FileTime.dwHighDateTime)) then begin
                              ExistingFileFileTime.dwLowDateTime   :=FileTime.dwLowDateTime; // remember original file time;
                              ExistingFileFileTime.dwHighDateTime  :=FileTime.dwHighDateTime;
                              ClearFileTime(FileTime);
                              OriginalName:=Name;
                              if SetName(TempName) then
                                 try     Modified:=False;
                                         Result:=MergeTextFile(OriginalName,False,False);
                                 finally if not SetName(OriginalName) then
                                            raise Exception.Create(TEXT_MEMORY_FULL);
                                 end
                              else raise Exception.Create(TEXT_MEMORY_FULL);
                              end
                           else ClearFileTime(ExistingFileFileTime);
                         {$ENDIF}

                         if Result then begin {'True': no merge, or merge succeeded}
                            if SysUtils.DeleteFile(Name) then
                               if SysUtils.RenameFile(TempName,Name) then begin {OK}
                                  {$IFDEF SokobanYASC}
                                    if   (ExistingFileFileTime.dwLowDateTime <>0) or
                                         (ExistingFileFileTime.dwHighDateTime<>0) then begin
                                         { the new file has been merged with the original file;
                                           keep the timestamp from the time the file was loaded
                                           by this instance of the application, so subsequent
                                           saving operations by this instance keep merging old
                                           and new versions;
                                         }
                                         FileTime.dwLowDateTime   :=ExistingFileFileTime.dwLowDateTime;
                                         FileTime.dwHighDateTime  :=ExistingFileFileTime.dwHighDateTime;
                                         end
                                    else GetFileTime(Name,FileTime);
                                  {$ELSE}
                                    GetFileTime(Name,FileTime);
                                  {$ENDIF}
                                  end
                               else begin
                                 SetName(TempName); {if 'SetName' fails, there is nothing to do about it here}
                                 GetFileTime(Name,FileTime);
                                 raise Exception.Create(Format(TEXT_FILE_RENAME_FAILED_FORMAT,[Name])+
                                                        Format(TEXT_NEW_NAME_IS_FORMAT,[Name]));
                                 end
                            else begin
                              s:=Name; SetName(TempName); {if 'SetName' fails, there is nothing to do about it here}
                              GetFileTime(Name,FileTime);
                              raise Exception.Create(Format(TEXT_FILE_DELETE_FAILED_FORMAT,[s])+
                                                     Format(TEXT_NEW_NAME_IS_FORMAT,[Name]));
                              end;
                            end
                         else begin
                            s:=Name; SetName(TempName); {if 'SetName' fails, there is nothing to do about it here}
                            GetFileTime(Name,FileTime);
                            raise Exception.Create(Format(TEXT_FILE_DELETE_FAILED_FORMAT,[s])+
                                                   Format(TEXT_NEW_NAME_IS_FORMAT,[Name]));
                            end;
                         end
                      else begin {no file with the same name exists; no rename or merge is necessary}
                         end
                   else begin {unspecified file write error}
                      if not StrEqual(Name,TempName) then SysUtils.DeleteFile(TempName);
                      raise Exception.Create(TEXT_WRITE_FILE_ERROR);
                      end;
               {$IFDEF SokobanYASC}
                 finally
                   if (ApplicationMutex<>0) and
                      (not ReleaseMutex(ApplicationMutex)) then
                      raise Exception.Create(TEXT_PROCESS_SYNCHRONIZATION_ERROR);
                 end;
               {$ENDIF}
               end;
     except on E:Exception do begin
               Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE+SUB_TITLE_SEPARATOR+TEXT_SAVE_FILE);
               end;
     end;
  if Result then Modified:=False;
end; {TSokoFile.SaveToFile}

function TSokoFile.UpdateFileFormatDescription(AddFileFormatDescription__:Boolean):Boolean;
var i,j,FirstLineNo,LastLineNo:Integer; DateCreated,DateOfLastChange:String; Node:TNode;
begin {Returns 'True' if the file-format-description is updated}
  Result:=False;
  if  (FileHeader<>nil) and (FileHeader.Lines<>nil) then begin
      if AddFileFormatDescription__ and
         HasFileFormatDescription(FirstLineNo,LastLineNo,i,j) then
      else begin
         FirstLineNo:=0; LastLineNo:=0;
         end;

      Result:=AddFileFormatDescription__
              or
              (FileHeader.Lines.IsEmpty and AddFileFormatDescriptionToFiles)
              or
              HasOldVersionFileFormatDescription(FirstLineNo,LastLineNo);
      if Result then with FileHeader.Lines do begin
         DateCreated     :=''; ReadString(KEY_DATE_CREATED    ,DateCreated);
         DateOfLastChange:=''; ReadString(KEY_DATE_LAST_CHANGE,DateOfLastChange);
         if FirstLineNo<>0 then {remove the first block of raw file notes which is supposed to contain the old file format description}
            for i:=FirstLineNo to LastLineNo do
                Remove(GetItemByIndex(Pred(FirstLineNo)),True); {'Pred': 'FirstLineNo'  is 1-based, hence, change it to a 0-based number}
         if (DateCreated     <>'') and FindKey(KEY_DATE_CREATED    ,Node) then Remove(Node,True);
         if (DateOfLastChange<>'') and FindKey(KEY_DATE_LAST_CHANGE,Node) then Remove(Node,True);

         FileHeader.Modified:=True;
         for i:=High(DEFAULT_FILE_FORMAT_DESCRIPTION) downto Low(DEFAULT_FILE_FORMAT_DESCRIPTION) do
             if Result then
                Result:=CreateObject(otNode,Node) and
                        Push(Node).SetText(DEFAULT_FILE_FORMAT_DESCRIPTION[i]);

         if DateCreated     <>'' then WriteString(KEY_DATE_CREATED    ,DateCreated);
         if DateOfLastChange<>'' then WriteString(KEY_DATE_LAST_CHANGE,DateOfLastChange);
         Modified:=True;
         Result:=True;
         end;
     end;
end; {TSokoFile.UpdateFileFormatDescription}

{
 --------------------------------------------------------------------
 Initialization and Finalization
 --------------------------------------------------------------------
}
procedure Initialize;
begin
{$WARNINGS OFF} {paranoia checks, just to be sure}
  if (MIN_BOARD_WIDTH  < 3)                 or (MIN_BOARD_HEIGHT < 3)           then Halt; {smaller boards don't make sense}
  if MAX_BOARD_WIDTH   * MAX_BOARD_HEIGHT   >=  MaxInt div 2                    then Halt; {'MaxInt div 2' is used as 'infinity' distance}
  if (MAX_BOARD_WIDTH  > High(Byte))        or (MAX_BOARD_HEIGHT> High(Byte))   then Halt; {for convenience, so board-width and board-height sometimes can be stored in a byte or packed in an integer using 8-bit shifts; additionally, 2*MAX_BOARD_WIDTH must not cause a numeric overflow related to run length encoding in 'TLevel.TextLinesToBoard'}
  if MAX_BOARD_SIZE    >=MAX_MOVES                                              then Halt; {'NormalizeBoard' may fill squares with walls and thereby move the player and the boxes}
  if MAX_BOXES         >=High(Int16)                                            then Halt; {box numbers are sometimes stored as 16-bit integers in order to save memory in large tables}
  if MAX_BOXES         >=High(TBoardSquare) shr BOARD_FLAG_COUNT                then Halt; {a board square contains a box-number shifted 'BOARD_FLAG_COUNT' bits to the left}
  if MAX_MOVES         >=MaxInt                                                 then Halt; {'MaxInt' is reserved as an illegal move-number}
  if PLAYER_LEGAL_MOVE <=High(THistoryMove)                                     then Halt; {'TGame.TryMove' mixes history-flags, board-flags, and move-flags}
  if PLAYER_TRY_MOVE   <=High(THistoryMove)                                     then Halt; {'TGame.TryMove' mixes history-flags, board-flags, and move-flags}
  if SizeOf(Pointer)   <>SizeOf(Cardinal)                                       then Halt; {cardinals are sometimes used for storing pointers}
  if SizeOf(Pointer)   <>SizeOf(Integer)                                        then Halt; {integers  are sometimes used for storing pointers}
  if SizeOf(Integer)   < 2*SizeOf(Int16)                                        then Halt; {integers should at least be 4 bytes, with room for 2 16-bit integers}
  if SizeOf(Int16)     <>2                                                      then Halt; {if this doesn't hold, then elementary integer types like 'WORD' have changed size since this application originally was written}
  if SizeOf(UInt16)    <>2                                                      then Halt; {if this doesn't hold, then elementary integer types like 'WORD' have changed size since this application originally was written}
  if (STRING_BASE      < 0) or (STRING_BASE > 1)                                then Halt; {first character in a string must have index 0 or 1}
  if (Ord(High(TDirection)) - Ord(Low(TDirection)) + 1) > High( Byte )          then Halt; {for convenience, it must be possible to store a direction in a byte}
  {$IFDEF SokobanYASC_ProductionVersion}                                        {the production version must avoid the risk of small integer overflows}
    if MAX_BOARD_SIZE  >=High(Int16)                                            then Halt; {square numbers, box positions and player positions are sometimes stored as 16-bit integers in order to save memory in large tables}
    if MAX_BOARD_SIZE * NUMBER_OF_DIRECTIONS  >= High(UInt16)                   then Halt; {the tuple [col,row,direction] are sometimes packed in a 16-bit unsigned integer in order to save memory in large tables}
//  if MAX_MOVES <
//     MAX_BOARD_WIDTH * MAX_BOARD_HEIGHT *  NUMBER_OF_DIRECTIONS * 2           then Halt; {'TSokoGame.BoxPath' needs this number of moves to ensure that table overflows never occur (seems to be unnessary with the existing code)}
  {$ENDIF}
{$WARNINGS ON}
{
  FillChar(IS_A_LEGAL_BOARD_CHARACTER,SizeOf(IS_A_LEGAL_BOARD_CHARACTER),0);    // not used; the 'AnsiPos(Ch,LEGAL_BOARD_CHARACTERS)>=STRING_BASE' statement in 'IsABoardLine()' is as fast as a lookup in a boolean table
  for i:=Low(LEGAL_BOARD_CHARACTERS) to High(LEGAL_BOARD_CHARACTERS) do
      IS_A_LEGAL_BOARD_CHARACTER[LEGAL_BOARD_CHARACTERS[i]]:=True;
}
end; {Initialize}

procedure Finalize;
begin
end; {Finalize}

initialization
  Initialize;

finalization
  Finalize;

end.

