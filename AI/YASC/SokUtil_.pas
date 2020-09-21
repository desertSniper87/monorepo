unit SokUtil_;
{$DEFINE SokobanYASC} {only when this module is used as part of 'Sokoban YASC'}

{
 File           : SokUtil_.pas
 Function       : Utilities for a Sokoban program
 Copyright      : Public Domain (this applies to this file, *not* the entire program)
 WARRANTY       : ABSOLUTELY NONE - USE THIS CODE AT YOUR OWN RISK.

 Original Author: Brian Damgaard, Denmark.
 E-mail         : BrianDamgaard@jubii.dk
 Date           : 2001-2014

 Modified by    : ...
 E-mail         : ...
 Date           : ...

 --------------------------------------------------------------------
 Contents
 --------------------------------------------------------------------
 . Character declarations, e.g. 'COLON', 'CR', 'LF'.
 . Texts, may be localized
 . String functions, such as 'StrEqual' and 'StrMacroExpand'
 . Single-linked lists
 . Run length encoder/decoder

 --------------------------------------------------------------------
 String functions
 --------------------------------------------------------------------
 Great care has been taken to ensure a high portability for string
 functions:

 1. Independency of string index-base
    Set the constant 'STRING_BASE' to '0' or '1' according to the
    base for the used programming language.

 2. No dependency of the internal string format
    The code never uses an index out of range, i.e., it does not
    depend on the c-style implementation of strings with a trailing
    NULL character (with the exception of 'TRle.Expand' and
    'LoadTextFromFile').
 --------------------------------------------------------------------
}

interface

uses Windows
     {$IFDEF SokobanYASC} ,Classes {$ENDIF} {for 'TStringList' type}
     ;

const
  {
  In Delphi/Pascal, individual characters in a string are indexed [1..length];
  in most other languages, they are indexed [0..length-1].
  Thus, you must set 'STRING_BASE=0' when translating to languages like
  C/C++, C#, and Java.
  Changing this does not solve all compatibility issues, but it's a start.
  }
  STRING_BASE                    = 1;

  {ASCII characters}
  AMPERSAND                      = '&';
  BACKSLASH                      = '\';
  BAR                            = '|';
  COLON                          = ':';
  COMMA                          = ',';
  CR                             = #13;
  DASH                           = '-';
  DOUBLE_QUOTE                   = '"';
  EQUAL                          = '=';
  EXCLAMATION                    = '!';
  GREATER_THAN                   = '>';
  HYPHEN                         = '-';
  LEFT_BRACE                     = '{';
  LEFT_BRACKET                   = '[';
  LEFT_PAREN                     = '(';
  LESS_THAN                      = '<';
  LF                             = #10;
  NL                             = CR+LF;
  NULL_CHAR                      = #00;
  PERIOD                         = '.';
  QUESTION                       = '?';
  QUOTE                          = '''';
  RIGHT_BRACE                    = '}';
  RIGHT_BRACKET                  = ']';
  RIGHT_PAREN                    = ')';
  SEMICOLON                      = ';';
  SLASH                          = '/';
  SPACE                          = ' ';
  STAR                           = '*';
  TAB                            = #09;
  UNDERSCORE                     = '_';

  BOMs                           : array[ 0 .. 2 ] of String {selected byte order marks}
                                 = ( Chr( $FF ) + Chr( $FE ),                {UTF-16 LE}
                                     Chr( $FE ) + Chr( $FF ),                {UTF-16 BE}
                                     Chr( $EF ) + Chr( $BB ) + Chr( $BF ) ); {UTF-8}

  FILE_NAME_EXT_DELIMITER        = PERIOD;
  FILE_NAME_PATH_DELIMITER       = BACKSLASH; {'SLASH' on unix systems}
  FORMAT_DATE_TIME               = 'yyyy-mm-dd  hh:mm:ss';
  FORMAT_MOVES_AND_PUSHES        = '%d/%d';

  {Filename extensions}
  BACKUP_FILE_NAME_EXT           = '.bak';
  SOKOBAN_FILE_NAME_EXT          = '.sok'; {recommended extension for Sokoban level files}
  TEMP_FILE_NAME_EXT             = '.tmp';
  WORK_FILE_NAME_EXT             = '.$$$'; {temporary files for current session; it's ok to overwrite them next time in case the program terminates without deleting them}
  XSB_FILE_NAME_EXT              = '.xsb'; {extension often used by other Sokoban programs (the name comes from the XSokoban program)}

  {Macros - only recognized in level-notes and snapshot-notes}
  MACRO_BEGIN                    = '<#';
  MACRO_END                      = '#/>';

  MAX_FILE_SIZE                  = 64*1024*1024; // a reasonably small maximum size is required because files are loaded into memory

  TAG_BOARD_BEGIN                = '<board>';  {don't localize} {normally, boards aren't tagged but it can be necessary in, say, clipboard files where boards don't have to be well-formed}
  TAG_BOARD_END                  = '</board>'; {don't localize}

  TEXT_SOKOBAN                   = 'Sokoban';  {don't localize}

  {Texts - may be localized, but formatting tags (%d, %f, and %s) must be left unchanged}
  TEXT_OLD_VALUE_IS_RESTORED     = 'The old value is restored.';


  TEXT_APPLICATION_TITLE         = TEXT_SOKOBAN;
  TEXT_APPLICATION_TITLE_LONG    = TEXT_APPLICATION_TITLE+' YASC';
  TEXT_APPLICATION_TITLE_SHORT   = 'YASC';
  TEXT_BEST_SOLUTION             = 'Best Solution';
  TEXT_BOARD_ERROR_BOX_COUNT_FORMAT
                                 = 'The maximum number of boxes for a level is %d.';
  TEXT_BOARD_ERROR_BOX_GOAL_MISMATCH
                                 = 'The number of boxes must match the number of goal squares.';
  TEXT_BOARD_ERROR_MIN_LENGTH_FORMAT
                                 = 'Each row must have at least %d squares.';
  TEXT_BOARD_ERROR_MISSING_WALL1 = 'The first non-empty square in each row must be a wall or a goal square with a box.';
  TEXT_BOARD_ERROR_MISSING_WALL2 = 'Each row must have at least one wall or a goal square with a box.';
  TEXT_BOARD_ERROR_NO_BOARD      = 'No Sokoban level found.';
  TEXT_BOARD_ERROR_NO_PLAYER     = 'The level needs a "player" before it is a legal level.';
  TEXT_BOARD_ERROR_ROW_NO_FORMAT = NL+NL+'(Row: %d)';
  TEXT_BOARD_ERROR_SIZE_FORMAT   = 'The maximum level size is %d x %d squares, and the minimum size is %d x %d squares.';
  TEXT_BOXES_AND_GOALS_FORMAT    = 'Boxes: %d. Goals: %d.';
  TEXT_BUILT_IN                  = 'Built-in';
  TEXT_ILLEGAL_CHAR_FORMAT       = 'The text contains an illegal character: "%s".';
  TEXT_CLIPBOARD_NO_LEVEL_TEXT   = 'The clipboard does not contain plain text'+NL+
                                   'and cannot be interpreted as a Sokoban level.';
  TEXT_CLIPBOARD_NO_TEXT         = 'The clipboard does not contain plain text.';
  TEXT_CLOSE_FILE_ERROR          = 'Close file error';
  TEXT_CREATE_FILE_ERROR         = 'Create file error';
  TEXT_CREATE_OBJECT             = 'Create Object';
  TEXT_FAILURE_DESCRIPTION       = 'Failure description:';
  TEXT_FILE_DELETE_FAILED_FORMAT = 'File cannot be deleted:'+NL+'%s';
  TEXT_FILE_FULL_FORMAT          = 'File is full:'+NL+'%s';
  TEXT_FILE_NOT_FOUND_FORMAT     = 'File not found:'+NL+'%s';
  TEXT_FILE_RENAME_FAILED_FORMAT = 'File cannot be renamed:'+NL+'%s';
  TEXT_FILE_TOO_LARGE_FORMAT     = 'File is too large:'+NL+'%s';
  TEXT_INTEGER_OVERFLOW          = 'Number too large.';
  TEXT_INTERNAL_ERROR_FORMAT     = 'Internal application error.'+NL+
                                   'Please contact the vendor if this happens frequently.'+NL+NL+
                                   TEXT_FAILURE_DESCRIPTION+NL+'%s';
  TEXT_INVALID_DATA              = 'The application found some unexpected data that it cannot process.';
  TEXT_LEVEL                     = 'Level';
  TEXT_LEVELS                    = 'Levels';
  TEXT_LEVEL_NOT_FOUND_FORMAT    = 'Level not found:'+NL+'%s';
  TEXT_LOAD_TEXT_FILE            = 'Load text-file';
  TEXT_MEMORY_FULL               = 'The operating system is out of memory or other system resources.';
  TEXT_FLIPPED_HORIZONTALLY      = 'flipped horizontally';
  TEXT_MERGE_FILES               = 'Merge Files';
  TEXT_NAME_ALREADY_EXISTS_FORMAT= 'The name'+NL+'"%s"'+NL+'already exists.'+NL+NL+TEXT_OLD_VALUE_IS_RESTORED;
  TEXT_NAME_CANNOT_BE_BLANK      = 'The name cannot be blank.';
  TEXT_NAME_CANNOT_BE_BLANK_2    = TEXT_NAME_CANNOT_BE_BLANK+NL+NL+TEXT_OLD_VALUE_IS_RESTORED;
  TEXT_NAME_IS_RESERVED_FORMAT   = 'Then name'+NL+'"%s"'+NL+'is reserved.'+NL+NL+TEXT_OLD_VALUE_IS_RESTORED;
  TEXT_NEW_NAME_IS_FORMAT        = NL+NL+'New name is:'+NL+'%s';
  TEXT_NO                        = 'No';
  TEXT_NO_FILE_SPECIFIED         = 'No file specified.';
  TEXT_NON_OPTIMAL               = 'Non-optimal';
  TEXT_NOT_IMPLEMENTED           = 'Not implemented.';
  TEXT_OPEN_FILE                 = 'Open file';
  TEXT_OPEN_FILE_ERROR           = 'Open file error';
  TEXT_OPEN_LEVEL                = 'Open Level';
  TEXT_OPEN_LEVEL_FROM_CLIPBOARD = 'Open level from clipboard';
  TEXT_OPTIMALITY                = 'Optimality';
  TEXT_PROCESS_SYNCHRONIZATION_ERROR
                                 = 'Process synchronization error';
  TEXT_RANGE_ERROR               = 'Range error.';
  TEXT_READ_FILE_ERROR           = 'File read error.';
  TEXT_RENAME_SNAPSHOT           = 'Rename snapshot';
  TEXT_REVERSE_MODE              = 'Reverse Mode';
//TEXT_REVERSE_MODE_SNAPSHOT     = 'Reverse Mode Snapshot';
  TEXT_ROTATED_DEGREES_CLOCKWISE_FORMAT
                                 = 'Rotated %d degrees clockwise';
  TEXT_SAVE_FILE                 = 'Save file';
  TEXT_SAVE_LEVEL                = 'Save level';
  TEXT_SCREEN_REGION             = 'Screen region';
  TEXT_SNAPSHOT                  = 'Snapshot';
  TEXT_SNAPSHOT_NOT_FOUND_FORMAT = 'Snapshot not found:'+NL+'"%s"';
  TEXT_SOKOBAN_COPYRIGHT_YEAR_0  = '1982';
  TEXT_SOKOBAN_COPYRIGHT_YEAR__  = ''; {'1989, 1990, 2001-2004'}
  TEXT_SOKOBAN_COPYRIGHT         = 'Sokoban (c) '+'by Falcon Co., Ltd., Japan';
  TEXT_SOKOBAN_COPYRIGHT_0       = 'Sokoban (c) '+TEXT_SOKOBAN_COPYRIGHT_YEAR_0+' by Hiroyuki Imabayashi, Japan';
  TEXT_SOLUTION                  = 'Solution';
  TEXT_TASK_FAILED               = 'Task failed.';
  TEXT_TEXT_TOO_LARGE            = 'Text too large.';
  TEXT_UNKNOWN_TYPE              = 'Unknown type.';
  TEXT_UNSUPPORTED_GAME_TYPE     = 'This application doesn''t support this type of game';
  TEXT_WRITE_FILE_ERROR          = 'File write error.';
  TEXT_YES                       = 'Yes';

type
  Int16                          = SmallInt; {nobody can remember that 'SmallInt' means a 16 bit signed integer, hence, this alias comes in handy}
  UInt16                         = Word; {for historical reasons, 'Word' means unsigned 16-bit integer in the Delphi versions that was around at the time this application originally was written}
  UInt32                         = Cardinal; {'Cardinal' was a 32-bit unsigned integer at the time this application originally was written}

  {2D transformations}

  TTransformation2D              = (t2DRotate0DegreesClockwise, {order must not change}
                                    t2DRotate90DegreesClockwise,
                                    t2DRotate180DegreesClockwise,
                                    t2DRotate270DegreesClockwise,
                                    t2DRotate0DegreesClockwiseFlipHorizontally,
                                    t2DRotate90DegreesClockwiseFlipHorizontally,
                                    t2DRotate180DegreesClockwiseFlipHorizontally,
                                    t2DRotate270DegreesClockwiseFlipHorizontally,
                                    t2DFlipVertically,
                                    t2DFlipHorizontally,
                                    t2DMirrorDiagonallyTopLeftBottomRight,
                                    t2DMirrorDiagonallyTopRightBottomLeft);
  TBoardTransformation2D         =  t2DRotate0DegreesClockwise.. {order must not change}
                                    t2DRotate270DegreesClockwiseFlipHorizontally;
  TBoardTransformation2DSet      = set of TBoardTransformation2D;

  TDirection                     = (Up,Left,Down,Right); {order must not change; for Hexoban, add 2 extra like: NorthEast, SouthWest}
  TDirectionMap                  = array[Low(TDirection)..High(TDirection)] of TDirection;
  TDirectionSet                  = set of TDirection;
  TShowErrorMessages             = ( semNone,semNew,semAll );

const
  BITS_PER_BYTE                  = 8;
  BITS_PER_INTEGER               = SizeOf(Integer )*BITS_PER_BYTE; {unsigned machine integer size in bits}
  BITS_PER_CARDINAL              = SizeOf(Cardinal)*BITS_PER_BYTE; {signed   machine integer size in bits}

  COUNTER_CLOCKWISE_ROTATION_TO_CLOCKWISE_ROTATION
                                 : array[t2DRotate0DegreesClockwise..t2DRotate270DegreesClockwise] of TTransformation2D =
                                   (t2DRotate0DegreesClockwise,
                                    t2DRotate270DegreesClockwise,
                                    t2DRotate180DegreesClockwise,
                                    t2DRotate90DegreesClockwise);
  BOARD_TRANSFORMATION_ANGLE     : array[TTransformation2D] of Integer =
                                   (0,90,180,270,0,90,180,270,0,0,0,0);
  BOARD_TRANSFORMATION_INVERSE   : array[TBoardTransformation2D] of TBoardTransformation2D =
                                   (t2DRotate0DegreesClockwise,
                                    t2DRotate270DegreesClockwise,
                                    t2DRotate180DegreesClockwise,
                                    t2DRotate90DegreesClockwise,
                                    t2DRotate0DegreesClockwiseFlipHorizontally,
                                    t2DRotate90DegreesClockwiseFlipHorizontally,
                                    t2DRotate180DegreesClockwiseFlipHorizontally,
                                    t2DRotate270DegreesClockwiseFlipHorizontally
                                   );
  BOARD_TRANSFORMATION_FLIP_HORIZONTALLY
                                 : array[TBoardTransformation2D] of TBoardTransformation2D =
                                   (t2DRotate0DegreesClockwiseFlipHorizontally,
                                    t2DRotate90DegreesClockwiseFlipHorizontally,
                                    t2DRotate180DegreesClockwiseFlipHorizontally,
                                    t2DRotate270DegreesClockwiseFlipHorizontally,
                                    t2DRotate0DegreesClockwise,
                                    t2DRotate90DegreesClockwise,
                                    t2DRotate180DegreesClockwise,
                                    t2DRotate270DegreesClockwise);
  BOARD_TRANSFORMATION_FLIP_VERTICALLY
                                 : array[TBoardTransformation2D] of TBoardTransformation2D =
                                   (t2DRotate180DegreesClockwiseFlipHorizontally,
                                    t2DRotate270DegreesClockwiseFlipHorizontally,
                                    t2DRotate0DegreesClockwiseFlipHorizontally,
                                    t2DRotate90DegreesClockwiseFlipHorizontally,
                                    t2DRotate180DegreesClockwise,
                                    t2DRotate270DegreesClockwise,
                                    t2DRotate0DegreesClockwise,
                                    t2DRotate90DegreesClockwise);
  BOARD_TRANSFORMATION_ROTATE_COUNTER_CLOCKWISE
                                 : array[TBoardTransformation2D] of TBoardTransformation2D =
                                   (t2DRotate270DegreesClockwise,
                                    t2DRotate0DegreesClockwise,
                                    t2DRotate90DegreesClockwise,
                                    t2DRotate180DegreesClockwise,
                                    t2DRotate90DegreesClockwiseFlipHorizontally,
                                    t2DRotate180DegreesClockwiseFlipHorizontally,
                                    t2DRotate270DegreesClockwiseFlipHorizontally,
                                    t2DRotate0DegreesClockwiseFlipHorizontally);
  BOARD_TRANSFORMATION_ROTATE_CLOCKWISE
                                 : array[TBoardTransformation2D] of TBoardTransformation2D =
                                   (t2DRotate90DegreesClockwise,
                                    t2DRotate180DegreesClockwise,
                                    t2DRotate270DegreesClockwise,
                                    t2DRotate0DegreesClockwise,
                                    t2DRotate270DegreesClockwiseFlipHorizontally,
                                    t2DRotate0DegreesClockwiseFlipHorizontally,
                                    t2DRotate90DegreesClockwiseFlipHorizontally,
                                    t2DRotate180DegreesClockwiseFlipHorizontally);

  DIRECTION_COUNT                = Succ(Ord(High(TDirection))-Ord(Low(TDirection))); {number of directions}

  SUB_TITLE_SEPARATOR            = ' - ';

type

  TByteVector                    = array[0..(MaxInt div SizeOf(Byte))-1] of Byte;
  PByteVector                    = ^TByteVector;
  TCompareFunction               = function(Item1,Item2:Pointer):Integer; // returns <0, 0, >0 for less, equal, and greater
  THashTableHashKey              = Cardinal;
  PTextFile                      = ^TextFile;
  TTimeMS                        = DWORD; // milli-seconds

  TTimeIntervalMS                = record StartTimeMS,StopTimeMS:TTimeMS;
                                   end;
  TTimeStamp                     = Cardinal; {'Cardinal' = unsigned machine integer}

  {Objects}

  TObjectType  = (otNode,otList,otRle,otNotes,otSnapshotAsText,otExtendedSnapshotAsText,
                  otLevel,otSokoFile,otSokoGame,otSnapshot); {all used object types}
  {
  'TNode' implements single-linked textlines.
  In languages without a string-type, memory-allocation/deallocation
  for the text must be handled manually.
  The class is used as baseclass for all entities: files, levels, variations, etc.
 }
  TNode = class
    Next       :TNode;
    Text       :String; {'Name' is an alias for 'Text'}

    constructor Create; {throws EOutOfMemory}
    destructor  Destroy; override;

    procedure   Clear; virtual;
    function    CopyTo(Destination__:TNode):Boolean; virtual;
    procedure   Insert(Node__:TNode); {inserts 'Node__' after self}
    function    IsEqual(Node__:TNode):Boolean; virtual;
    function    IsMember(Node__:TNode):Boolean; virtual;
    function    Last:TNode; {returns last node in the chain}
    function    LoadFromFile(const FileName__:String):Boolean; virtual;
    function    Name:String; virtual; {'Name' is an alias for 'Text'}
    function    ObjectType:TObjectType; virtual;
    function    SetName(const Name__:String):Boolean; {'Name' is an alias for 'Text'}
    function    SetText(const Text__:String):Boolean;
    function    WriteToFile(TextFile__:PTextFile):Integer; virtual; {throws EInOutError}
  end;

  PNode          = ^TNode;
  TNodeMethod    = function(Node__:TNode):Integer of object;
  TPNodeVector   = array[0..(MaxInt div SizeOf(PNode))-1] of PNode;
  PPNodeVector   = ^TPNodeVector;

  {'TList' implements single-linked lists}
  TList = class(TNode)
    Items      :TNode;

    constructor Create; {throws EOutOfMemory}
    destructor  Destroy; override;

    procedure   Add(Node__:TNode);
    function    AddBlankLine:Boolean;
    function    AddTextLine(const Text__:String; InFront__:Boolean):TNode;
    procedure   Clear; override;
    function    ConvertNewLineToLines(const NewLine__:String):Boolean;
    function    CopyTo(Destination__:TNode):Boolean; override;
    function    Count:Integer;
    function    DeleteKey(const Key__:String):String;
    function    FindKey(const Key__:String; var Node__:TNode):Boolean;
    function    First:TNode;
    function    GetItemByIndex(Index__:Integer):TNode;
    function    GetItemByName(const Name__:String):TNode;
    function    IndexOf(Node__:TNode):Integer;
    function    InsertAfter(Node__,AfterNode__:TNode):TNode;
    function    InsertBefore(Node__,BeforeNode__:TNode):TNode;
    function    IsEmpty:Boolean;
    function    IsEqual(Node__:TNode):Boolean; override;
    function    IsMember(Node__:TNode):Boolean; override;
    function    ItemExists(const Name__:String):Boolean;
    function    Last:TNode;
    function    LoadFromFile(const FileName__:String):Boolean; override;
    function    LoadFromFileWithFileTime(const FileName__:String; var FileTime__:TFileTime):Boolean;
    function    LoadFromText(const Text__:String):Boolean;
    function    LoadFromVector(Count__:Integer; var Vector__:PPNodeVector):Boolean;
    function    MakeUniqueName(const Name__,DefaultName__:String; UseParens__:Boolean):String;
    function    MakeUniqueName1(const Name__,DefaultName__:String; UseParens__:Boolean; var No__:Integer):String;
    function    MacroExpand(MacroStr__:TList; const NewLine__:String):Boolean;
    function    MergeSort(CompareFunction__:TCompareFunction):TTimeMS;
    function    MoveAfter(Node__,AfterNode__: TNode):TNode;
    function    MoveToBack (Node__:TNode):TNode;
    function    MoveToFront(Node__:TNode):TNode;
    function    NextWithWrapAround(Node__:TNode):TNode;
    function    ObjectType:TObjectType; override;
    function    Prev(Node__:TNode): TNode;
    function    PrevWithWrapAround(Node__:TNode):TNode;
    function    Pop:TNode;
    function    Push(Node__:TNode):TNode;
    function    ReadString(const Key__:String; var Value__:String):Boolean; {for easy handling of key/value information, such as 'Author'}
    function    Remove(Node__:TNode; Destroy__:Boolean):TNode;
    function    RenameItem(const OldName__,NewName__:String):Boolean;
    function    Reverse:TNode;
    function    SaveToFile(const FileName__:String):Boolean;
    function    SaveToVector(var Count__:Integer; var Vector__:PPNodeVector):Boolean;
    function    SetSingleTextLine(const Text__:String):TNode;
    function    StrSubstitute(const OldString__,NewString__,IniFileSection__:String; AllOccurrences__:Boolean):Integer;
    function    Swap(List__:TList):Boolean;
    {$IFDEF SokobanYASC}
      function  ToStringList(var SL__:TStringList):Boolean;
    {$ENDIF}
    function    ToText(const LineSeparator__:String; var Text__:String):Boolean;
    function    TrimBlankLines:Integer;
    function    WriteString(const Key__,Value__:String):Boolean; {for easy handling of key/value information, such as 'Author: NN'}
    function    WriteToFile(TextFile__:PTextFile):Integer; override;
  end;

  TRle = class(TNode)    {run length encoded string}
    Capacity   :Integer; {string size}
    Position   :Integer; {string 'fill-pointer', 0-based}
    RleCh      :Char;    {current pending character}
    RleCount   :Integer; {number of pending characters}
    RleEnabled :Boolean; {run length encoding enabled/disabled}

    constructor Create;  {throws EOutOfMemory}
    destructor  Destroy; override;

    function    Add(Ch__:Char):Boolean;
    procedure   Clear; override;
    function    Compress(const Str__: String):Boolean;
    function    CopyTo(Destination__:TNode):Boolean; override; {not implemented}
    function    Expand  (const Str__: String):Boolean;
    function    Flush:Boolean;
  end;

  {Bloom filters are not implemented as a class but as a "plain old data}
  {structure"; the rationale is that this application typically uses a small}
  {local Bloom filter which for speed and conveniency is allocated on the stack}
  {instead of the heap, which a class-oriented implementation would require;}
  TBloomFilter = record
   TableByteSize:Integer;
   Table:PByteVector;
  end;

{
 --------------------------------------------------------------------
 Exported functions
 --------------------------------------------------------------------
}

function  AddTimeMS(Time1MS__,Time2MS__:TTimeMS):TTimeMS;
function  BloomFilterAdd(var BloomFilter__:TBloomFilter; const Key__:String):THashTableHashKey;
procedure BloomFilterAddNumber(var BloomFilter__:TBloomFilter; Key__:Integer);
procedure BloomFilterInitialize(var BloomFilter__:TBloomFilter; TableByteSize__:Integer; Table__:PByteVector);
function  BloomFilterLookup(const BloomFilter__:TBloomFilter; const Key__:String; var HashKey__:THashTableHashKey):Boolean; {note that a Bloom filter can return false positives but never false negatives}
function  BloomFilterLookupNumber(const BloomFilter__:TBloomFilter; Key__:Integer):Boolean; {note that a Bloom filter can return false positives but never false negatives}
function  BoardTransformation2DToStr(BoardTransformation2D__:TBoardTransformation2D):String; {throws EOutOfMemory}
function  CalculateElapsedTimeMS(StartTimeMS__,StopTimeMS__:TTimeMS):TTimeMS;
function  CalculateTimeIntervalMS(IntervalMS__:TTimeMS):TTimeIntervalMS;
procedure CalculateTransformation2D(Transformation2D__:TTransformation2D; Col__,Row__,Width__,Height__:Integer; var NewCol__,NewRow__:Integer);
procedure CalculateTransformation2DDirections(Initialize__:Boolean; Transformation2D__:TTransformation2D; var NewDirections__:TDirectionMap);
procedure ClearFileTime(var FileTime__:TFileTime);
function  CompareNodes(A__,B__:Pointer):Integer;
function  DirectoryExists(const Path__: String): Boolean;
function  Error(const Text__,Caption__:String):Boolean; {always returns 'False'}
function  ExtractFileNameWithoutPathAndExtension(const FileName__:String):String; {throws EOutOfMemory}
function  ExtractTextInParenthesis(const Text__:String):String; {throws EOutOfMemory}
function  FileHasFileTime( const FileName__ : String; FileTime__ : TFileTime ) : Boolean;
function  FileSize(const FileName__:String):Integer; // doesn't work for large files > 2.14 GB
function  GetFileTime(const FileName__:String; var FileTime__:TFileTime):Boolean;
function  GetTimeMS:TTimeMS;
function  IsACommentLine(const Line__:String):Boolean;
function  IsADigitChar(Char__:Char):Boolean;
function  IsBlank(Str__:String):Boolean;
function  IsText(const Text__:String):Boolean;
function  LoadTextFromFile(var Text__:String; var FileTime__: TFileTime; const FileName__:String):Boolean;
function  MakeUniqueFileName(const Path__,FileName__,Extension__:String):String; {throws EOutOfMemory}
function  Max(a__,b__:Integer):Integer;
function  Min(a__,b__:Integer):Integer;
function  Msg(const Text__,Caption__:String; Flags__:Integer):Integer;
function  ReadLine           (const Text__    :String; {throws EOutOfMemory}
                              const Length__  :Integer;
                              var   Position__:Integer; {current position, 0-based}
                              var   Line__    :String):Boolean;
function  ReadUnsignedInteger(const Text__    :String;
                              var   Position__:Integer; {current position, 0-based}
                              var   Value__   :Integer): Boolean;
function  SafeGetMem(var MemoryBlock__:Pointer; ByteSize__:Integer; Clear__,Verbose__:Boolean):Boolean;
function  SafeRealloc(var MemoryBlock__:Pointer; OldByteSize__,NewByteSize__:Integer; Clear__,Verbose__:Boolean):Boolean;
function  SaveTextToFile(const Text__,FileName__:String):Boolean;
function  StrBeginsWith(const Str__,Token__:String):Boolean;
function  StrDigitsOnly(const s:String):Boolean;
function  StrEndsWith(const Str__,Token__:String):Boolean;
function  StrEqual(const Str1__,Str2__:String):Boolean; {Compares strings without case sensitivity}
function  StrFirstNonBlankCharIndex(const Str__:String):Integer;
function  StrHashValuePJW(const Text__:String):THashTableHashKey;
function  StrHashValueDEK(const Text__:String):THashTableHashKey;
function  StrLastCharIndex(const Str__:String):Integer;
function  StrMacroExpand(var Str__:String; var Modified__:Boolean; MacroStr__:TList):Boolean;
function  StrRemoveBOM( const Str__ : String ) : String;
function  StrRemoveCharacters(const Str__,Characters__:String):String; {throws EOutOfMemory}
function  StrSubstituteCharacters(const Str__,Characters__:String; New__:Char):String;
function  StrToBoardTransformation2D(const Str__,KeyRotateCounterClockwise__,KeyMirrorHorisontally__,KeyFlipHorizontally__:String;
                                     var   BoardTransformation2D__:TBoardTransformation2D):Boolean;
function  StrToTime(const Str__:String; var TimeMS__:TTimeMS):Boolean;
function  StrTrimmedLength(const Str__:String; var FirstIndex__,LastIndex__:Integer):Integer;
function  StrSubstitute (const Str__,Old__,New__:String; var Count__:Integer):String; {throws EOutOfMemory}
function  StrSubstituteAll(const Str__,Old__,New__:String):String; {throws EOutOfMemory}
function  StrSubstitute1(const Str__,Old__,New__:String; var Count__:Integer):String; {throws EOutOfMemory}
function  StrWith(const Str__:String; Ch1__,Ch2__:Char):String; {throws EOutOfMemory}
function  StrWithBrackets(const Str__:String):String; {throws EOutOfMemory}
function  StrWithDoubleQuotes(const Str__:String):String; {throws EOutOfMemory}
function  StrWithParenthesis(const Str__:String):String; {throws EOutOfMemory}
function  StrWithout(const Str__:String; Ch1__,Ch2__:Char):String; {throws EOutOfMemory}
function  StrWithoutBrackets(const Str__:String):String; {throws EOutOfMemory}
function  StrWithoutDoubleQuotes(const Str__:String):String; {throws EOutOfMemory}
function  StrWithoutQuotes(const Str__:String):String; {throws EOutOfMemory}
function  StrWithoutTrailingPathDelimiter(const Str__:String):String; {throws EOutOfMemory}
function  StrWithTrailingPathDelimiter(const Str__:String):String; {throws EOutOfMemory}
procedure SwapIntegers(var Integer1__,Integer2__:Integer);
procedure SwapNodes(var Node1__,Node2__:TNode);
function  TimeToStr(TimeMS:TTimeMS):String;

{
 --------------------------------------------------------------------
 Global variables
 --------------------------------------------------------------------
}
var
  LastErrorText     : String  = '';
  ShowErrorMessages : TShowErrorMessages = semAll; { set to 'semNone' to suppress error messages, or to 'semNew' to suppress cascading identical error messages}

implementation

uses
  {$IFDEF SokobanYASC} Controls, Forms, {$ENDIF} {for 'Screen' and 'Application' variables}
  SysUtils,Clipbrd,
  SokFile_; {for 'CreateObject'}

function AddTimeMS(Time1MS__,Time2MS__:TTimeMS):TTimeMS;
begin
  if   Time1MS__<=High(Result)-Time2MS__ then
       Result:=Time1MS__+Time2MS__
  else Result:=High(Result);
end;

function BoardTransformation2DToStr(BoardTransformation2D__:TBoardTransformation2D):String; {throws EOutOfMemory}
begin
  Result:=Format(TEXT_ROTATED_DEGREES_CLOCKWISE_FORMAT,[BOARD_TRANSFORMATION_ANGLE[BoardTransformation2D__]]);
  if BoardTransformation2D__>=t2DRotate0DegreesClockwiseFlipHorizontally then
     Result:=Result+COMMA+SPACE+TEXT_FLIPPED_HORIZONTALLY;
end;

function CalculateElapsedTimeMS(StartTimeMS__,StopTimeMS__:TTimeMS):TTimeMS;
begin
  if   StopTimeMS__>=StartTimeMS__ then
       Result:=StopTimeMS__-StartTimeMS__
  else Result:=High(StopTimeMS__)-StartTimeMS__+StopTimeMS__+1; // clock wrap-around; assume it only wrapped around once
end;

function CalculateTimeIntervalMS(IntervalMS__:TTimeMS):TTimeIntervalMS;
const EXTRA_TIME_MS=2;
begin {Returns TimeOut := Now + Interval, with a primitive clock wrap-around control}
  if     IntervalMS__<High(IntervalMS__)-EXTRA_TIME_MS then Inc(IntervalMS__,EXTRA_TIME_MS); {add a little extra time for overhead}
  repeat Result.StartTimeMS :=GetTickCount;
         Result.StopTimeMS  :=Result.StartTimeMS+IntervalMS__;
  until  (Result.StopTimeMS >=Result.StartTimeMS) and (Result.StartTimeMS<>0); {primitive clock wrap-around control: pauses when wrap-around occurs; '<>0': reserves '0' for the special meaning 'timer inactive'}
end; {CalculateTimeIntervalMS}

procedure CalculateTransformation2DDirections(Initialize__:Boolean; Transformation2D__:TTransformation2D; var NewDirections__:TDirectionMap);
var Direction:TDirection;

  procedure FlipVertically;
  var Direction:TDirection;
  begin
    for Direction:=Low(NewDirections__) to High(NewDirections__) do
        case NewDirections__[Direction] of
             Up    : NewDirections__[Direction]:=Down;
             Down  : NewDirections__[Direction]:=Up;
        end; {case}
  end;

  procedure FlipHorizontally;
  var Direction:TDirection;
  begin
    for Direction:=Low(NewDirections__) to High(NewDirections__) do
        case NewDirections__[Direction] of
             Left  : NewDirections__[Direction]:=Right;
             Right : NewDirections__[Direction]:=Left;
        end; {case}
  end;

  procedure RotateClockwise(Count__:Integer);
  var i:Integer; d,Direction:TDirection;
  begin {rotations could have been handled more elegantly with a lookup table,}
        {but doing it algorithmically at least protects against simple errors in a table;}
        {rotation is a high risk operation; any error will destroy user solutions}
    for Direction:=Low(NewDirections__) to High(NewDirections__) do begin
        d:=NewDirections__[Direction];
        for i:=1 to Count__ do
            if   d<>Low (d) then d:=Pred(d)
            else d:=High(d);
        NewDirections__[Direction]:=d;
        end;
  end;

begin {CalculateTransformation2DDirections}
  if Initialize__ then // 'True': initialize the new directions; the alternative is that the transformation updates the existing mapping, typically when a transformation is composed by more than one transformation
     for Direction:=Low(NewDirections__) to High(NewDirections__) do
         NewDirections__[Direction]:=Direction;
  case Transformation2D__ of
    t2DRotate0DegreesClockwise                   :;
    t2DRotate90DegreesClockwise                  : RotateClockwise(1);
    t2DRotate180DegreesClockwise                 : RotateClockwise(2);
    t2DRotate270DegreesClockwise                 : RotateClockwise(3);
    t2DRotate0DegreesClockwiseFlipHorizontally   : FlipHorizontally;
    t2DRotate90DegreesClockwiseFlipHorizontally  : begin RotateClockwise(1); FlipHorizontally; end;
    t2DRotate180DegreesClockwiseFlipHorizontally : begin RotateClockwise(2); FlipHorizontally; end;
    t2DRotate270DegreesClockwiseFlipHorizontally : begin RotateClockwise(3); FlipHorizontally; end;
    t2DFlipVertically                            : FlipVertically;
    t2DFlipHorizontally                          : FlipHorizontally;
    else Error(Format(TEXT_INTERNAL_ERROR_FORMAT,['Unhandled board transformation']),'SokUtil_.CalculateTransformation2DDirections');
  end; {case}
end; {CalculateTransformation2DDirections}

procedure CalculateTransformation2D(Transformation2D__:TTransformation2D; Col__,Row__,Width__,Height__:Integer; var NewCol__,NewRow__:Integer);
begin {precondition: mirroring diagonally only works when 'Width__' = 'Height__'}
  case Transformation2D__ of
    t2DRotate0DegreesClockwise           : begin NewCol__:=Col__;                NewRow__:=Row__; end;
    t2DRotate90DegreesClockwise          : begin NewCol__:=Succ(Height__)-Row__; NewRow__:=Col__; end;
    t2DRotate180DegreesClockwise         : begin NewCol__:=Succ(Width__ )-Col__; NewRow__:=Succ(Height__)-Row__; end;
    t2DRotate270DegreesClockwise         : begin NewCol__:=Row__;                NewRow__:=Succ(Width__ )-Col__; end;
    t2DRotate0DegreesClockwiseFlipHorizontally
                                         : begin
                                             CalculateTransformation2D(t2DFlipHorizontally         ,Col__   ,Row__   ,Width__ ,Height__,NewCol__,NewRow__);
                                           end;
    t2DRotate90DegreesClockwiseFlipHorizontally
                                         : begin
                                             CalculateTransformation2D(t2DRotate90DegreesClockwise ,Col__   ,Row__   ,Width__ ,Height__,NewCol__,NewRow__);
                                             CalculateTransformation2D(t2DFlipHorizontally         ,NewCol__,NewRow__,Height__,Width__ ,NewCol__,NewRow__);
                                           end;
    t2DRotate180DegreesClockwiseFlipHorizontally
                                         : begin
                                             CalculateTransformation2D(t2DRotate180DegreesClockwise,Col__   ,Row__   ,Width__ ,Height__,NewCol__,NewRow__);
                                             CalculateTransformation2D(t2DFlipHorizontally         ,NewCol__,NewRow__,Width__ ,Height__,NewCol__,NewRow__);
                                           end;
    t2DRotate270DegreesClockwiseFlipHorizontally
                                         : begin
                                             CalculateTransformation2D(t2DRotate270DegreesClockwise,Col__   ,Row__   ,Width__ ,Height__,NewCol__,NewRow__);
                                             CalculateTransformation2D(t2DFlipHorizontally         ,NewCol__,NewRow__,Height__,Width__ ,NewCol__,NewRow__);
                                           end;
    t2DFlipVertically                    : begin NewCol__:=Col__;                NewRow__:=Succ(Height__)-Row__; end;
    t2DFlipHorizontally                  : begin NewCol__:=Succ(Width__ )-Col__; NewRow__:=Row__; end;
    t2DMirrorDiagonallyTopLeftBottomRight: begin NewCol__:=Row__;                NewRow__:=Col__; end;
    t2DMirrorDiagonallyTopRightBottomLeft: begin NewCol__:=Succ(Height__)-Row__; NewRow__:=Succ(Width__ )-Col__; end;
    else                                   begin NewCol__:=Col__;                NewRow__:=Row__; end;
  end; {case}
end; {CalculateTransformation2D}

procedure ClearFileTime(var FileTime__:TFileTime);
begin
  FileTime__.dwLowDateTime:=0; FileTime__.dwHighDateTime:=0;
end;

function  CompareNodes(A__,B__:Pointer):Integer;
begin
  Result:=AnsiCompareText(TNode(A__).Text,TNode(B__).Text);
end;

function  DirectoryExists(const Path__: String): Boolean;
var
  Code: Integer; OldErrorMode: Cardinal; s: String;
begin
  Code:=-1;
  s:=Path__;
  if (s<>'') and (s[Length(s)] = FILE_NAME_PATH_DELIMITER) then
     SetLength(s, Pred(Length(s)));
  if s<>'' then begin
     OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS+SEM_NOOPENFILEERRORBOX); // avoids that 'GetVolumeInformation' asks the user to insert a media (e.g., a floppy disk or a CD) if a removable drive is empty
     try     Code := GetFileAttributes(PChar(s));
     finally SetErrorMode(OldErrormode);
     end;
     end;
  Result := (Code <> -1) and ((FILE_ATTRIBUTE_DIRECTORY and Code) <> 0);
end; {DirectoryExists}

function  Error( const Text__, Caption__ : String ) : Boolean;
begin
  if (   ShowErrorMessages = semAll )
     or
     ( ( ShowErrorMessages = semNew ) and ( not StrEqual( Text__, LastErrorText ) ) ) then
     Msg(Text__, Caption__, MB_OK + MB_ICONERROR );
  LastErrorText := Text__;
  Result := False;
end; {Error}

function ExtractBracketedText(const Text__:String):String;
var CurrentPos,StartPos,LastPos:Integer;
begin
  Result:='';
  StartPos:=System.Pos(LEFT_BRACKET,Text__);
  if StartPos>=STRING_BASE then begin
     Dec(StartPos,STRING_BASE); CurrentPos:=StartPos; LastPos:=Pred(Length(Text__)); {normalizing to 0-offset numbers}
     if CurrentPos<LastPos then
        repeat Inc(CurrentPos);
        until  (CurrentPos=LastPos) or (Text__[STRING_BASE+CurrentPos]=RIGHT_BRACKET);
     if Text__[STRING_BASE+CurrentPos]=RIGHT_BRACKET then
        Result:=System.Copy(Text__,STRING_BASE+Succ(StartPos),Pred(CurrentPos-StartPos));
     end;
end;

function ExtractFileNameWithoutPathAndExtension(const FileName__:String):String; {throws EOutOfMemory}
var i,j:Integer;
begin
  i:=Pred(Length(FileName__));
  while (i>=0) and (FileName__[STRING_BASE+i]<>FILE_NAME_EXT_DELIMITER) do Dec(i);
  if i<0 then Result:=FileName__
  else Result:=System.Copy(FileName__,STRING_BASE,i);

  i:=Pred(Length(Result)); j:=i;
  while (i>=0) and
        {(Result[STRING_BASE+i]<>SLASH) and} {both '/' and '\' are treated as folder name separators (disabled)}
        (Result[STRING_BASE+i]<>BACKSLASH) and
        (Result[STRING_BASE+i]<>COLON) do Dec(i);
  if i>=0 then Result:=System.Copy(Result,STRING_BASE+Succ(i),j-i);
end;

function  ExtractTextInParenthesis(const Text__:String):String; {throws EOutOfMemory}
var CurrentPos,StartPos,LastPos:Integer;
begin
  Result:='';
  StartPos:=System.Pos(LEFT_PAREN,Text__);
  if StartPos>=STRING_BASE then begin
     Dec(StartPos,STRING_BASE); CurrentPos:=StartPos; LastPos:=Pred(Length(Text__)); {normalizing to 0-offset numbers}
     if CurrentPos<LastPos then
        repeat Inc(CurrentPos);
        until  (CurrentPos=LastPos) or (Text__[STRING_BASE+CurrentPos]=RIGHT_PAREN);
     if Text__[STRING_BASE+CurrentPos]=RIGHT_PAREN then
        Result:=System.Copy(Text__,STRING_BASE+Succ(StartPos),Pred(CurrentPos-StartPos));
     end;
end;

function  FileHasFileTime( const FileName__ : String; FileTime__ : TFileTime ) : Boolean;
var FileFileTime : TFileTime;
begin
  Result := ( ( FileTime__.dwLowDateTime  <> 0 )
              or
              ( FileTime__.dwHighDateTime <> 0 )
            )
            and
            GetFileTime(FileName__, FileFileTime )
            and
            ( ( FileFileTime.dwLowDateTime  = FileTime__.dwLowDateTime  )
              or
              ( FileFileTime.dwHighDateTime = FileTime__.dwHighDateTime ) );
end;

function  FileSize(const FileName__:String):Integer; // doesn't work for large files > 2.14 GB
var F:File;
begin {$I-}
  if   FileExists(FileName__) then begin
       if IOResult<>0 then begin end; // clear the IO error flag
       AssignFile(F,FileName__); Reset(F,1); Result:=System.FileSize(F);
       CloseFile(F);
       if IOResult<>0 then Result:=-2
       end
  else Result:=-1;
end; {$I+}

function  GetFileTime(const FileName__:String; var FileTime__:TFileTime):Boolean;
var FileHandle:THandle;
begin
  Result:=False; ClearFileTime(FileTime__);
  FileHandle:=CreateFile(PChar(FileName__),
                         GENERIC_READ+GENERIC_WRITE,
                         FILE_SHARE_DELETE OR FILE_SHARE_READ OR FILE_SHARE_WRITE,
                         nil,
                         OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL,
                         0);
  if FileHandle<>INVALID_HANDLE_VALUE then begin
     Result:=Windows.GetFileTime(FileHandle,nil,nil,Addr(FileTime__));
     if not CloseHandle(FileHandle) then
        {raise Exception.Create(TEXT_CLOSE_FILE_ERROR);}
        Result:=False;
     end;
  if not Result then
     ClearFileTime( FileTime__ );
end;

function GetTimeMS:TTimeMS;
begin {returns a time in milliseconds; for relative calculations only}
  Result:=GetTickCount;
end;

function IsACommentLine(const Line__:String):Boolean;
begin
  Result:=( IsBlank(Line__)) or
         {( Line__[STRING_BASE]=SEMICOLON) or} {NO: some xsb-files use ';' in title-lines}
          ((Line__[STRING_BASE]=SLASH) and
           (Length(Line__)>=2) and
           (Line__[STRING_BASE+1]=SLASH)
          );
end; {IsACommentLine}

function  IsADigitChar(Char__:Char):Boolean;
begin
  Result:=(Char__>='0') and (Char__<='9');
end; {IsADigitChar}

function IsBlank(Str__:String):Boolean;
var i,j:Integer;
begin
  if Str__='' then Result:=True
  else begin
    i:=STRING_BASE; j:=StrLastCharIndex(Str__);
    while (i<j) and (Str__[i]<=SPACE) do Inc(i); {'i<j': don't rely on j<High(j)}
    Result:=(i=j) and (Str__[i]<=SPACE);
    end;
end; {IsBlank}

function IsDigit(Char__:Char):Boolean;
begin
  Result:=(Char__>='0') and (Char__<='9');
end;

function IsText(const Text__:String):Boolean;
var i,Len:Integer; Ch:Char;
begin {tries to detect whether 'Text__' is printable text or binary data}
  Result:=True; Len:=Length(Text__);
  for i:=0 to Pred(Len) do begin
      Ch:=Text__[i+STRING_BASE];
      if (Ch<SPACE) and (Ch<>CR) and (Ch<>LF) and (Ch<>TAB) and
         ((Ch<>NULL_CHAR) or (i<Pred(Len))) then begin {last character is allowed to be NULL}
         Result:=False; break;
         end;
      end;
end; {IsText}

function LoadTextFromFile(var Text__:String; var FileTime__: TFileTime; const FileName__:String):Boolean;
const FILE_MODE_READ_ONLY=0;
var   i,j,OldFileMode:Integer; F:File;
begin
  Text__:=''; ClearFileTime(FileTime__);
  try
    Result:=FileExists(FileName__);

    if   Result then begin
         AssignFile(F,FileName__);

         OldFileMode:=FileMode;
         try     FileMode:=FILE_MODE_READ_ONLY;
                 Reset(F,1); {'1': access the file as a file of bytes, so 'FileSize' returns the correct size}
         finally FileMode:=OldFileMode;
         end;

         try    i:=System.FileSize(F);
                if    (i>=0) and
                      //(i<=High(i)-SizeOf(Char)) then begin
                      {the code allocates space for an extra NULL-character terminator;}
                      {this is not really necessary in this case where a Delphi string is used as read buffer,}
                      {but it makes it easier to rewrite the code in another programming language}
                      (i<=MAX_FILE_SIZE) then begin
                      SetLength(Text__,i+SizeOf(Char)); {allocate buffer, one extra terminating NULL-char}
                      try     BlockRead(F,PChar(Addr(Text__[STRING_BASE]))^,i,j); {i=bytes to read; j=bytes read}
                              if   (i=j) and
                                   Windows.GetFileTime(TFileRec(F).Handle,nil,nil,Addr(FileTime__))
                                   then begin
                                   {add a NULL-character terminator;}
                                   {this is the only real use of pointers in this module;}
                                   PChar(Cardinal(Addr(Text__[STRING_BASE]))+Cardinal(i))^:=NULL_CHAR;
                                   end
                              else raise Exception.Create(TEXT_READ_FILE_ERROR);
                      finally if not Result then begin
                                 Text__:=''; ClearFileTime(FileTime__); {free the buffer in case of errors}
                                 end;
                      end;
                      end
                 else raise Exception.Create(Format(TEXT_FILE_TOO_LARGE_FORMAT,[FileName__]));
         finally CloseFile(F);
         end;
         end
    else raise Exception.Create(Format(TEXT_FILE_NOT_FOUND_FORMAT,[FileName__]));
  except on E:Exception do Result:=Error(E.Message,TEXT_APPLICATION_TITLE+' - '+TEXT_LOAD_TEXT_FILE);
  end;
  if not Result then begin Text__:=''; ClearFileTime(FileTime__); end;
end; {LoadTextFromFile}

function MakeUniqueFileName(const Path__,FileName__,Extension__:String):String; {throws EOutOfMemory}
var i:Integer; e,p:String;
begin
  p:=StrWithTrailingPathDelimiter(Path__); e:=Extension__;
  if (e<>'') and (e[STRING_BASE]<>FILE_NAME_EXT_DELIMITER) then
     e:=FILE_NAME_EXT_DELIMITER+e;
  Result:=p+FileName__+e; i:=1;
  if FileExists(Result) or DirectoryExists(Result) then
     repeat Inc(i); Result:=p+FileName__+SPACE+IntToStr(i)+e;
            if FileExists(Result) or DirectoryExists(Result) then Result:='';
     until  (Result<>'') or (i=High(i));
end; {MakeUniqueFileName}

function Max(a__,b__:Integer):Integer;
begin
  if a__>=b__ then Result:=a__
  else Result:=b__;
end; {Max}

function Min(a__,b__:Integer):Integer;
begin
  if a__<=b__ then Result:=a__
  else Result:=b__;
end; {Min}

{$IFDEF SokobanYASC}

  function Msg(const Text__,Caption__:String; Flags__:Integer):Integer;
  var i:Integer; Caption:String; oCursor:TCursor;
  begin
    try    if   Caption__='' then Caption:=Application.Title
           else Caption:=Caption__;
           i:=StrLastCharIndex(Caption);
           if (i>=STRING_BASE) and (Caption[i]=PERIOD) then Caption[i]:=SPACE;
    except on E:Exception do;
    end;

    oCursor:=Screen.Cursor;
    try     if Screen.Cursor<>crDefault then Screen.Cursor:=crDefault;
            if Flags__=0 then Flags__:=MB_OK+MB_ICONINFORMATION;
            Result:=Application.MessageBox(PChar(Text__),PChar(Caption),Flags__);
            //Result:=Windows.MessageBox(0,PChar(Text__),PChar(Caption),Flags__);
    finally if Screen.Cursor<>oCursor   then Screen.Cursor:=oCursor;
    end;
  end; {Msg}

{$ELSE}

  function Msg(const Text__,Caption__:String; Flags__:Integer):Integer;
  begin
    Writeln;
    if (Text__<>'') or (Caption__<>'') then begin
       if Caption__<>'' then Writeln(Caption__)
       else                  Writeln(TEXT_APPLICATION_TITLE);
       if Text__   <>'' then Writeln(Text__);
       end;
    Writeln;
    Write('Press [Enter]');
    Readln;
    Result:=0;
  end; {Msg}

{$ENDIF}

function ReadLine(const Text__    :String;  {throws EOutOfMemory}
                  const Length__  :Integer;
                  var   Position__:Integer; {current position, 0-based, input-output parameter}
                  var   Line__    :String):Boolean;
var StartPos,EndPos:Integer; Ch:Char;
begin {Returns the next line in 'Line__'; 'Position__': input: where to start reading; output: where to start reading the next line}
 {Note: this procedure could be coded more efficiently using pointers,
  but that would make it less portable.
 }
  Result:=(Text__<>'') and (Position__<Length__);
  if Result then begin
     StartPos:=Position__; EndPos:=Position__; Ch:=Text__[STRING_BASE+Position__];
     while (Ch<>CR) and (Ch<>LF) and (Ch<>NULL_CHAR) and (Position__<Length__) do begin
       Inc(Position__);
       if Ch>SPACE then EndPos:=Position__; {drop trailing spaces; 'EndPos' points after last non-blank character}
       Ch:=Text__[STRING_BASE+Position__];
       end;

     Line__:=System.Copy(Text__,STRING_BASE+StartPos,EndPos - StartPos); {throws EOutOfMemory}

     if Position__<Length__ then
        if Ch=NULL_CHAR then Position__:=Length__
        else begin {a CR or LF found}
           Inc(Position__);
           if Position__<Length__ then
              {CR+LF and LF+CR and CR+CR+LF are considered a single newline control-character}
{             if      (Ch=CR) and (Text__[STRING_BASE+Position__]=LF) then Inc(Position__)}
              if      Ch=CR then begin
                      Ch:=Text__[STRING_BASE+Position__]; {get the next character after 'CR'}
                      if   Ch=LF then Inc(Position__) {CR+LF}
                      else if (Ch=CR) and
                              (Succ(Position__)<Length__) and
                              (Text__[STRING_BASE+Succ(Position__)]=LF) then {CR+CR+LF; this combination occurs sometimes when CTRL+C is used to copy a text selection to the clipboard}
                              Inc(Position__,2)
                      end
              else if (Ch=LF) and (Text__[STRING_BASE+Position__]=CR) then Inc(Position__);
           end;
     end
end; {ReadLine}

function ReadUnsignedInteger(const Text__    :String;
                             var   Position__:Integer; {current position, 0-based}
                             var   Value__   :Integer): Boolean;
var Digit,Length:Integer;
begin
 {Note: this procedure could be coded more efficiently using pointers,
  but that would make it less portable.
 }
  Length:=System.Length(Text__);
  while (Position__<Length) and (not IsDigit(Text__[STRING_BASE+Position__])) do Inc(Position__);
  Result:=Position__<Length; Value__:=0;
  while (Position__<Length) and      IsDigit(Text__[STRING_BASE+Position__])  and Result do begin
    Digit:=Ord(Text__[STRING_BASE+Position__])-Ord('0');
    Result:=Value__<=(High(Value__)-Digit) div 10;
    if Result then begin
       Value__:=Value__*10+Digit;
       Inc(Position__);
       end;
    end;
end; {ReadUnsignedInteger}

function SaveTextToFile(const Text__,FileName__:String):Boolean;
var F:TextFile;
begin
  Result:=True;
  try    AssignFile(F,FileName__); Rewrite(F); Write(F,Text__); CloseFile(F);
  except on E:Exception do Result:=Error(E.Message,'Save Text To File');
  end;
end;

function SafeGetMem(var MemoryBlock__:Pointer; ByteSize__:Integer; Clear__,Verbose__:Boolean):Boolean;
begin
  if ByteSize__>0 then begin
     try    GetMem(MemoryBlock__,ByteSize__);
     except on E:Exception do MemoryBlock__:=nil;
     end;
     Result:=MemoryBlock__<>nil;
     if Result then begin
        if Clear__ then FillChar(MemoryBlock__^,ByteSize__,0);
        end
     else if VerBose__ then Error(TEXT_MEMORY_FULL,'SafeGetMem');
     end
  else begin
     Result:=ByteSize__=0; MemoryBlock__:=nil;
     if (not Result) and Verbose__ then Error(TEXT_RANGE_ERROR,'SafeGetMem');
     end;
end;

function SafeRealloc(var MemoryBlock__:Pointer; OldByteSize__,NewByteSize__:Integer; Clear__,Verbose__:Boolean):Boolean;
var New:Pointer;
begin
  Result:=OldByteSize__=NewByteSize__;
  if not Result then begin
     if NewByteSize__>0 then begin
        Result:=SafeGetMem(New,NewByteSize__,Clear__ and (MemoryBlock__=nil),Verbose__);
        if Result then begin
           if MemoryBlock__<>nil then begin
              Move(MemoryBlock__^,New^,Min(OldByteSize__,NewByteSize__)); {Move(source,destination,bytesize)}
              FreeMem(MemoryBlock__,OldByteSize__);
              if Clear__ and (NewByteSize__>OldByteSize__) then
                 if   OldByteSize__>=0 then begin {clear new area}
                      MemoryBlock__:=Pointer(Cardinal(New)+Cardinal(OldByteSize__)); {calculate address of the un-initialized area}
                      FillChar(MemoryBlock__^,NewByteSize__-OldByteSize__,0);
                      end
                 else Result:=Error(TEXT_RANGE_ERROR,'SafeRealloc'); {negative old size: something is terribly wrong with the caller}
              end;
           MemoryBlock__:=New;
           end;
        end
     else begin
        if MemoryBlock__<>nil then FreeMem(MemoryBlock__,OldByteSize__);
        MemoryBlock__:=nil;
        Result:=True;
        end;
     end;
end;

function StrBeginsWith(const Str__,Token__:String):Boolean;
var Len:Integer;
begin {Compares strings without case sensitivity; note: empty 'Token__' returns 'True'}
  Len:=Length(Token__);
  Result:={kludge: without the length guard, the Windows function 'CompareText'}
          {crashes with an 'access violation' exception when Sokoban YASC}
          {searches through level-collections to find prior/next unsolved level}
          (Len<=Length(Str__))
          and
          (AnsiStrLIComp(PChar(Str__),PChar(Token__),Len)=0);
end;

function StrDigitsOnly(const s:String):Boolean;
var i,Len:Integer;
begin
  Result:=False;
  Len:=Length(s);
  for i:=1 to Len do
      if not IsADigitChar(s[i]) then exit; {quick-and-dirty exit if a character isn't a digit}
  Result:=Len>0;
end;

function StrEndsWith(const Str__,Token__:String):Boolean;
var StringLength, TokenLength:Integer; P :PChar;
begin {Compares strings without case sensitivity; note: empty 'Token__' returns 'True'}
  StringLength:= Length(Str__);
  TokenLength:=Length(Token__);
  P := PChar( Str__ );
  Inc( P, StringLength - TokenLength );
  Result:=(TokenLength<=StringLength)
          and
          (AnsiStrLIComp(P,PChar(Token__),TokenLength)=0);
end;

function StrEqual(const Str1__,Str2__:String):Boolean;
begin {Compares strings without case sensitivity}
  Result:=AnsiCompareText(Str1__,Str2__)=0;
end; {StrEqual}

function StrFirstNonBlankCharIndex(const Str__:String):Integer;
var LastCharIndex:Integer;
begin
  if    Str__<>'' then begin
        Result:=STRING_BASE; LastCharIndex:=StrLastCharIndex(Str__);
        while (Result<LastCharIndex) and (Str__[Result]<=SPACE) do Inc(Result);
        end
  else Result:=Pred(STRING_BASE);
end;

function StrHashValuePJW(const Text__:String):THashTableHashKey; // a hashpjw-based function (Peter J. Weinberger), adding in the length of the text, and with a upper limit on the number of hashed characters
const BITS_PER_BYTE         = SokUtil_.BITS_PER_BYTE;
      BITS_PER_HASH_KEY     = SizeOf(THashTableHashKey) * BITS_PER_BYTE;
      HIGH_4_BITS           = THashTableHashKey($f) shl (BITS_PER_HASH_KEY-4);
      MAX_TESTED_CHARACTERS = 64;
var   i,Count,Index,Step:Integer; HighBits:THashTableHashKey;
begin
  Result:=Length(Text__);
  if Result<=MAX_TESTED_CHARACTERS then begin
     Count:=Result; Step:=1; // test the complete text
     end
  else begin // test characters distributed over the entire text
     Count:=MAX_TESTED_CHARACTERS; Step:=Result div MAX_TESTED_CHARACTERS;
     end;
  Index:=STRING_BASE;
  for i:=1 to Count do begin
      Result:=(Result shl 4) + Ord(Text__[Index]);
      HighBits:=Result and HIGH_4_BITS;
      if HighBits<>0 then Result:=(Result xor (HighBits shr (BITS_PER_HASH_KEY-8))) xor HighBits; // the high-bits are cleared after xor'ing them with the bits 4..7
      Inc(Index,Step); // 'Index' is the next character number to test
      end;
end;

function StrHashValueDEK(const Text__:String):THashTableHashKey; // a hashdek function (Donald E. Knuth, "The Art Of Computer Programming Volume 3", 6.4)
const LEFT_SHIFT=5; RIGHT_SHIFT=BITS_PER_BYTE*SizeOf(Result)-LEFT_SHIFT;
var   Index:Integer;
begin
  Result:=Length(Text__);
  for Index:=1 to Result do
      Result:=((Result shl LEFT_SHIFT) xor (Result shr RIGHT_SHIFT)) xor Ord(Text__[Index]);
end;

function StrLastCharIndex(const Str__:String):Integer;
begin
  Result:=STRING_BASE+Pred(Length(Str__));
end; {StrLastCharIndex}

function StrMacroExpand(var Str__:String; var Modified__:Boolean; MacroStr__:TList):Boolean;
var a,b,i,j:Integer; Key,Value:String;
begin
  Result:=True; Modified__:=False;
  if MacroStr__<>nil then
     try
       a:=Length(MACRO_BEGIN); b:=Length(MACRO_END);
       i:=AnsiPos(MACRO_BEGIN,Str__);
       j:=AnsiPos(MACRO_END  ,Str__);
       while (i>=STRING_BASE) and (j>i) do begin
         Key:=System.Copy(Str__,i+a,j-i-a);
         System.Delete(Str__,j,b);
         System.Delete(Str__,i,a+Length(Key));
         if MacroStr__.ReadString(Key,Value) then Value:=StrWithoutDoubleQuotes(Value)
         else Value:=Key;
         Modified__:=True;
         System.Insert(Value,Str__,i);

         i:=AnsiPos(MACRO_BEGIN,Str__);
         j:=AnsiPos(MACRO_END  ,Str__);
         end;
     except on E:Exception do Result:=Error(E.Message,TEXT_APPLICATION_TITLE+' - StrMacroExpand');
     end;
end; {StrMacroExpand}

function  StrRemoveBOM( const Str__ : String ) : String;
var Index : Integer;
begin {removes a byte order mark (BOM) if the string begins with one of the defined byte order marks}
  Result := Str__;
  Index  := Low( BOMs ); {remove byte order mark, if any}
  while  ( Index  <= High( BOMs ) ) and
         not( StrBeginsWith( Result, BOMs[ Index ] ) ) do
         Inc( Index );
  if     Index  <= High( BOMs ) then
         Result := Copy( Result, Succ( Length( BOMs [ Index ] ) ), Length( Result ) );
end;

function StrRemoveCharacters(const Str__,Characters__:String):String; {throws EOutOfMemory}
var i:Integer;
begin
  Result:=Str__;
  for i:=Pred(Length(Result)) downto 0 do
      if AnsiPos(Result[STRING_BASE+i],Characters__)<>0 then
         Delete(Result,STRING_BASE+i,1);
end; {StrRemoveCharacters}

function StrSubstituteCharacters(const Str__,Characters__:String; New__:Char):String; {throws EOutOfMemory}
var i:Integer;
begin
  Result:=Str__;
  for i:=Pred(Length(Result)) downto 0 do
      if AnsiPos(Result[STRING_BASE+i],Characters__)<>0 then
         Result[STRING_BASE+i] := New__;
  if New__<=SPACE then
     Result:=Trim(Result);
end; {StrSubstituteCharacters}

function StrSubstituteNTimes(const Str__,Old__,New__:String; MaxTimes__:Integer; var Count__:Integer):String; {throws EOutOfMemory}
var i,j,OldLength:Integer; SameLength:Boolean;
begin {Substitutes 'Old__' with 'New__' in 'Str__'}
  Result:=Str__; Count__:=0;
  i:=AnsiPos(Old__,Result);
  if (i>=STRING_BASE) and (Count__<MaxTimes__) then begin
     OldLength:=Length(Old__);
     SameLength:=OldLength=Length(New__);

     repeat

       if   not SameLength then begin
            Delete(Result,i,OldLength); Insert(New__,Result,i);
            end
       else {modify the string destructively one character at a time;}
            {there is a chance that this is faster than 'delete + insert'}
            for j:=STRING_BASE to STRING_BASE-1+OldLength do
                Result[i+j-STRING_BASE]:=New__[j];

       Inc(Count__);
       if   Count__<MaxTimes__ then begin
            i:=AnsiPos(Old__,Result);                   {search for next occurence of 'Old__', if any}
            if (i                   >=STRING_BASE) and  {'True': there are more occurences}
               (Count__             = 1)           and  {'True': this is the first substitution}
               (AnsiPos(Old__,New__)>=STRING_BASE) then {'True': the new string contains the old one; stop, otherwise the function enters an infinite loop}
               i:=STRING_BASE-1; {stop substitution}
            end
       else i:=STRING_BASE-1; {stop substitution}

    until i<STRING_BASE;
    end;
end;

function StrSubstitute (const Str__,Old__,New__:String; var Count__:Integer):String; {throws EOutOfMemory}
begin {Substitutes all occurrences (if any) of 'Old__' in 'Str__' with 'New__'}
  Result:=StrSubstituteNTimes(Str__,Old__,New__,MaxInt,Count__);
end;

function StrSubstituteAll(const Str__,Old__,New__:String):String; {throws EOutOfMemory}
var Count:Integer;
begin {Substitutes all occurrences (if any) of 'Old__' in 'Str__' with 'New__'}
  Result:=StrSubstituteNTimes(Str__,Old__,New__,MaxInt,Count);
end;

function StrSubstitute1(const Str__,Old__,New__:String; var Count__:Integer):String; {throws EOutOfMemory}
begin {Substitutes the first occurrence (if any) of 'Old__' in 'Str__' with 'New__'}
  Result:=StrSubstituteNTimes(Str__,Old__,New__,1,Count__);
end;

function StrToBoardTransformation2D(const Str__,KeyRotateCounterClockwise__,KeyMirrorHorisontally__,KeyFlipHorizontally__:String;
                                    var   BoardTransformation2D__:TBoardTransformation2D):Boolean;
const
  DEGREES_AS_STRING:array[t2DRotate90DegreesClockwise..t2DRotate270DegreesClockwise] of String =
    ('90','180','270');
var i,j,k:Integer; t:TBoardTransformation2D;
begin {precondition: 'Str__' and keys must have the same case, i.e., they are either uppercase or lowercase}
  Result:=True; BoardTransformation2D__:=t2DRotate0DegreesClockwise;

  i:=AnsiPos(KeyRotateCounterClockwise__,Str__);
  j:=AnsiPos(KeyMirrorHorisontally__,Str__);
  k:=AnsiPos(KeyFlipHorizontally__,Str__);

  for t:=High(DEGREES_AS_STRING) downto Low(DEGREES_AS_STRING) do
      if  (AnsiPos(DEGREES_AS_STRING[t],Str__)>=STRING_BASE) then
          if   i<STRING_BASE then {clockwise rotation}
               BoardTransformation2D__:=t
          else BoardTransformation2D__:=COUNTER_CLOCKWISE_ROTATION_TO_CLOCKWISE_ROTATION[t]; {counter-clockwise rotation}

  if      j>=STRING_BASE then {flip vertically}
          if   k>=STRING_BASE then
               if   j<k then
                    {flip vertically, then horizontally}
                    BoardTransformation2D__:=BOARD_TRANSFORMATION_FLIP_HORIZONTALLY[BOARD_TRANSFORMATION_FLIP_VERTICALLY[BoardTransformation2D__]]
               else {flip horizontally, then vertically}
                    BoardTransformation2D__:=BOARD_TRANSFORMATION_FLIP_VERTICALLY[BOARD_TRANSFORMATION_FLIP_HORIZONTALLY[BoardTransformation2D__]]
          else {flip vertically}
               BoardTransformation2D__:=BOARD_TRANSFORMATION_FLIP_VERTICALLY[BoardTransformation2D__]
  else if k>=STRING_BASE then {flip horizontally}
          BoardTransformation2D__:=BOARD_TRANSFORMATION_FLIP_HORIZONTALLY[BoardTransformation2D__];
end;

function StrToTime(const Str__:String; var TimeMS__:TTimeMS):Boolean;
var i,Len,No,Separators:Integer; Ch:Char;
begin // string format "H:M:S" or "H.M.S"
  Result:=True; TimeMS__:=0; Separators:=0; Len:=Length(Str__); i:=0; No:=0;
  while (i<Len) and (Str__[STRING_BASE+i]<=SPACE) do Inc(i);
  while (i<Len) and Result do begin
    Ch:=Str__[STRING_BASE+i]; Inc(i);
    if   (Ch>='0') and (Ch<='9') then
         No:=(No*10)+Ord(Ch)-Ord('0')
    else if ((Ch=COLON) or (Ch=PERIOD)) and (Separators<=1) then begin
            Inc(Separators);
            if   Separators=1 then
                 Inc(TimeMS__,No*3600*1000)  // hours
            else Inc(TimeMS__,No*  60*1000); // minutes
            No:=0;
            end
         else Result:=False;
    end;
  Inc(TimeMS__,No*1000); // 'No' contains seconds at this time if the string contains a proper formatted time
  Result:=Result and (Separators=2);
end;

function StrTrimmedLength(const Str__:String; var FirstIndex__,LastIndex__:Integer):Integer;
begin
  FirstIndex__:=STRING_BASE; LastIndex__:=StrLastCharIndex(Str__);
  while (FirstIndex__< LastIndex__) and (Str__[FirstIndex__]<=SPACE) do Inc(FirstIndex__);
  while (FirstIndex__<=LastIndex__) and (Str__[LastIndex__ ]<=SPACE) do Dec(LastIndex__ );
  Result:=Succ(LastIndex__-FirstIndex__);
end; {StrTrimmedLength}

function StrWith(const Str__:String; Ch1__,Ch2__:Char):String; {throws EOutOfMemory}
begin
  Result:=Str__;
  if (Result='') or (Result[STRING_BASE]<>Ch1__) then
     Result:=Ch1__+Result;
  if (Length(Result)=1) or
     (Result[StrLastCharIndex(Result)]<>Ch2__) then Result:=Result+Ch2__;
end; {StrWith}

function StrWithBrackets(const Str__:String):String; {throws EOutOfMemory}
begin
  Result:=StrWith(Str__,LEFT_BRACKET,RIGHT_BRACKET);
end; {StrWithBrackets}

function StrWithDoubleQuotes(const Str__:String):String; {throws EOutOfMemory}
begin
  Result:=StrWith(Str__,DOUBLE_QUOTE,DOUBLE_QUOTE);
end; {StrWithDoubleQuotes}

function  StrWithParenthesis(const Str__:String):String; {throws EOutOfMemory}
begin
  Result:=StrWith(Str__,LEFT_PAREN,RIGHT_PAREN);
end;

function StrWithout(const Str__:String; Ch1__,Ch2__:Char):String; {throws EOutOfMemory}
begin
  Result:=Str__;
  if (Result<>'') and (Result[STRING_BASE]=Ch1__) then
     System.Delete(Result,STRING_BASE,1);
  if (Result<>'') and (Result[StrLastCharIndex(Result)]=Ch2__) then
     System.Delete(Result,StrLastCharIndex(Result),1);
end; {StrWithout}

function StrWithoutBrackets(const Str__:String):String; {throws EOutOfMemory}
begin
  Result:=StrWithout(Str__,LEFT_BRACKET,RIGHT_BRACKET);
end; {StrWithoutDoubleQuotes}

function StrWithoutDoubleQuotes(const Str__:String):String; {throws EOutOfMemory}
begin
  Result:=StrWithout(Str__,DOUBLE_QUOTE,DOUBLE_QUOTE);
end; {StrWithoutDoubleQuotes}

function StrWithoutQuotes(const Str__:String):String; {throws EOutOfMemory}
begin
  Result:=StrWithout(Str__,QUOTE,QUOTE);
end; {StrWithoutQuotes}

function StrWithoutTrailingPathDelimiter(const Str__:String):String; {throws EOutOfMemory}
begin
  Result:=Str__;
  if (Result<>'') and (Result[STRING_BASE+Pred(Length(Result))]=FILE_NAME_PATH_DELIMITER) then
     Delete(Result,STRING_BASE+Pred(Length(Result)),1);
end;

function  StrWithPadding(const s:String; PaddingChar:Char; Width:Integer):String; {throws EOutOfMemory}
begin
  Result:=s;
  while Width>Length(Result) do Result:=PaddingChar+Result;
end;

function StrWithTrailingPathDelimiter(const Str__:String):String; {throws EOutOfMemory}
begin
  if   (Str__='') or (Str__[STRING_BASE+Pred(Length(Str__))]=FILE_NAME_PATH_DELIMITER) then
       Result:=Str__
  else Result:=Str__+FILE_NAME_PATH_DELIMITER;
end;

procedure SwapIntegers(var Integer1__,Integer2__:Integer);
var Temp:Integer;
begin
  Temp:=Integer1__; Integer1__:=Integer2__; Integer2__:=Temp;
end;

procedure SwapNodes(var Node1__,Node2__:TNode);
var Temp:TNode;
begin
  Temp:=Node1__; Node1__:=Node2__; Node2__:=Temp;
end;

function TimeToStr(TimeMS:TTimeMS):String;
var Hours,Minutes,Seconds:Cardinal;
begin
  // convert time to seconds, rounded
  if High(TimeMS)-500>=TimeMS then Inc(TimeMS,500);
  Seconds:=TimeMS  div 1000;
  Hours  :=Seconds div (60*60); Dec(Seconds,Hours  *60*60);
  Minutes:=Seconds div 60;      Dec(Seconds,Minutes*60);
  Result :=StrWithPadding(IntToStr(Hours),'0',2)+COLON+StrWithPadding(IntToStr(Minutes),'0',2)+COLON+StrWithPadding(IntToStr(Seconds),'0',2);
end;

{ TNode }

constructor TNode.Create; {throws EOutOfMemory}
begin
  Next:=nil; Text:='';
end; {TNode.Create}

destructor TNode.Destroy;
begin
  Next:=nil; Text:=''; {in languages without a garbage-collected string-type, the memory for 'Text' must be deallocated manually}
  Inherited;
end; {TNode.Destroy}

procedure TNode.Clear;
begin
  Next:=nil; Text:=''; {in languages without a garbage-collected string-type, the memory for 'Text' must be deallocated manually}
end; {TNode.Clear}

function TNode.CopyTo(Destination__:TNode):Boolean;
begin
  Result:=Destination__.SetText(Copy(Text,STRING_BASE,Length(Text))); {'Copy': ensure that the text isn't shared among different objects}
  {Destination__.Next:=Next;} {'Next' is not copied; it's considered an internal administrative field for linking the nodes}
end; {TNode.CopyTo}

procedure TNode.Insert(Node__:TNode); {inserts 'Node__' after self}
begin {if 'Node__' is a list, this method inserts the complete list after this node}
  if Node__<>nil then begin
     Node__.Last.Next:=Self.Next;
     Self.Next:=Node__;
     end;
end; {TNode.Insert}

function TNode.IsEqual(Node__:TNode):Boolean;
begin
  Result:=(Node__<>nil) and (ObjectType=Node__.ObjectType) and (Text=Node__.Text); {case sensitive comparison}
end; {TNode.IsEqual}

function TNode.IsMember(Node__:TNode):Boolean;
var n:TNode;
begin {Returns 'True' if 'Node__' is a member of the list starting with this node}
  n:=Self;
  while (n<>nil) and (n<>Node__) do n:=n.Next;
  Result:=n<>nil;
end; {TNode.IsMember}

function TNode.Last:TNode;
begin
  Result:=Self;
  while Result.Next<>nil do Result:=Result.Next;
end; {TNode.Last}

function TNode.LoadFromFile(const FileName__:String):Boolean;
var FileTime:TFileTime;
begin
  Result:=LoadTextFromFile(Text,FileTime,FileName__);
  if Result then SetText( StrRemoveBOM(Text));
end; {TNode.LoadFromFile}

function TNode.Name:String; {'Name' is an alias for 'Text'}
begin
  Result:=Text;
end; {TNode.Name}

function TNode.ObjectType:TObjectType;
begin
  Result:=otNode;
end; {TNode.ObjectType}

function TNode.SetName(const Name__:String):Boolean; {'Name' is an alias for 'Text'}
begin
  Result:=SetText(Name__);
end; {TNode.SetName}

function TNode.SetText(const Text__:String):Boolean;
begin
  {
  Note: in Delphi, the following text-assigment
  is a simple pointer assignment + a reference-count update;
  In other languages, it may be necessary to allocate memory and copy
  the text from the source to the destination, hence, the 'try...except' block
  }
  Result:=True;
  try    Text:=Text__;
  except on E:Exception do Result:=Error(E.Message,TEXT_APPLICATION_TITLE+' - TNode.SetText');
  end;
end; {TNode.SetText}

function TNode.WriteToFile(TextFile__:PTextFile):Integer; {throws EInOutError}
begin
  Writeln(TextFile__^,Text);
  Result:=0;
end; {TNode.WriteToFile}

{ TList }

constructor TList.Create; {throws EOutOfMemory}
begin
  Inherited;
  Items:=nil;
end; {TList.Create}

destructor TList.Destroy;
begin
  Clear;
  Inherited;
end; {TList.Destroy}

procedure TList.Add(Node__: TNode);
begin {Note: 'Node__' must be a single item, not a list}
  if Node__<>nil then begin
     if   Items=nil then Items:=Node__
     else Items.Last.Next:=Node__;
     Node__.Next:=nil;
     end;
end; {TList.Add}

function TList.AddBlankLine:Boolean;
var Node:TNode;
begin
  Result:=CreateObject(otNode,Node);
  if Result then Add(Node);
end;

function TList.AddTextLine(const Text__:String; InFront__:Boolean):TNode;
var Node:TNode;
begin
  Result:=nil;
  if   CreateObject(otNode,Node) then
       if   Node.SetText(Text__) then begin
            if   InFront__ then
                 Push(Node)
            else Add(Node);
            Result:=Node;
            end
       else Node.Free;
end; {TList.AddTextLine}

procedure TList.Clear;
begin {Destroys all list members}
  while Items<>nil do Pop.Free;
  {Inherited;} {don't call 'Inherited': the list may itself be a member of a list}
end; {TList.Clear}

function TList.ConvertNewLineToLines(const NewLine__:String):Boolean;
var i,j:Integer; n,p:TNode;
begin {Embedded linebreaks, such as '\n', are converted to real linebreaks}
  n:=Items; j:=Length(NewLine__); Result:=j>0;
  while (n<>nil) and Result do begin
    i:=AnsiPos(NewLine__,n.Text);
    if i>=STRING_BASE then
       try    p:=TNode.Create;
              n.Insert(p);
              Result:=p.SetText(System.Copy(n.Text,i+j,Length(n.Text)-Pred(i+j)));
              if Result then Result:=n.SetText(System.Copy(n.Text,STRING_BASE,i-STRING_BASE));
       except on E:Exception do Result:=Error(E.Message,'TList.ConvertNewLineToLines');
       end;
    n:=n.Next;
    end;
end; {TList.ConvertNewLineToLines}

function TList.CopyTo(Destination__:TNode):Boolean;
var n,p:TNode;
begin
  Result:=Destination__ is TList;
  if Result then begin
     TList(Destination__).Clear;
     n:=Items;
     while (n<>nil) and Result do begin
       Result:=CreateObject(n.ObjectType,p) and
               n.CopyTo(p);
       if   Result then TList(Destination__).Push(p)
       else p.Free;
       n:=n.Next;
       end;
     if   Result then TList(Destination__).Reverse {the items on the new list are in reverse order; make it right}
     else TList(Destination__).Clear;
     end;
end; {TList.CopyTo}

function TList.Count:Integer;
var n:TNode;
begin
  Result:=0; n:=Items;
  while n<>nil do begin Inc(Result); n:=n.Next; end;
end; {TList.Count}

function TList.DeleteKey(const Key__:String):String;
var Node:TNode;
begin {Deletes the first found line holding a key/value pair with the given key; returns the value if the key/value pair exists}
  if   FindKey(Key__,Node) then begin
       MoveToFront(Node); {put the key/value pair first on the list for fast 'ReadString' performance}
       if not ReadString(Key__,Result) then Result:='';
       Remove(Node,True); {destroy the line with the key/value pair}
       TrimBlankLines;
       end
  else Result:='';
end; {TList.DeleteKey}

function TList.FindKey(const Key__:String; var Node__:TNode):Boolean;
var Index,KeyLength,StrLength:Integer; Ch:Char; s:String;
begin {Searches text lines for a key/value pair; if found, the line is returned in 'Node__'}
  Result:=False;
  if Key__<>'' then begin
     KeyLength:=Length(Key__);
     Node__:=Items;
     while (not Result) and (Node__<>nil) do begin
       s:=Node__.Text; StrLength:=Length(s);
       if StrLength>KeyLength then begin
          Index:=KeyLength;
          Ch:=s[STRING_BASE+Index];

          while Ch=SPACE do begin {allow spaces between the key and the separator, e.g., "key : value"}
            Inc(Index); {search for the next non-blank character, if any}
            if   Index<StrLength then
                 Ch:=s[STRING_BASE+Index]
            else Ch:=NULL_CHAR;
            end;

          Result:=((Ch=EQUAL) {accept both 'key:value' and 'key=value'}
                   or
                   (Ch=COLON)
                  )
                  and
                  StrBeginsWith(s,Key__);
          end;

       if not Result then Node__:=Node__.Next;
       end;
     end;
end; {TList.FindKey}

function TList.First:TNode;
begin
  Result:=Items;
end; {TList.First}

function TList.GetItemByIndex(Index__:Integer):TNode;
begin {0-based indexing}
  if Index__<0 then Result:=nil
  else begin
     Result:=Items;
     while (Index__>0) and (Result<>nil) do begin
       Result:=Result.Next; Dec(Index__);
       end;
     end;
end; {TList.GetItemByIndex}

function TList.GetItemByName(const Name__:String):TNode;
begin
  Result:=Items;
  while (Result<>nil) and (not StrEqual(Name__,Result.Text)) do
    Result:=Result.Next;
end; {TList.GetItemByName}

function TList.IndexOf(Node__:TNode):Integer;
var n:TNode;
begin {'IndexOf' is 0-based}
  Result:=0; n:=Items;
  while (n<>nil) and (n<>Node__) do begin
    Inc(Result); n:=n.Next;
    end;
  if n=nil then Result:=-1;
end; {TList.IndexOf}

function TList.InsertAfter(Node__,AfterNode__:TNode):TNode;
begin {Note: if 'Node__' is a list, this method inserts the complete list}
  if   AfterNode__<>nil then AfterNode__.Insert(Node__)
  else if Node__<>nil then begin
          Node__.Last.Next:=Items; {splice 'Node__' into the start of the list}
          Items:=Node__;
          end;
  Result:=Node__;
end; {TList.InsertAfter}

function TList.InsertBefore(Node__,BeforeNode__:TNode):TNode;
begin {Note: if 'Node__' is a list, this method inserts the complete list}
  if Node__<>nil then
     if        BeforeNode__<>nil then
               InsertAfter(Node__,Prev(BeforeNode__)) {insert the new node after the item prior to 'BeforeNode__' on the list}
     else if   Items<>nil then Last.Insert(Node__) {append the new node to the end of the list}
          else Items:=Node__; {the new node is the new first item on the list}
  Result:=Node__;
end; {TList.InsertBefore}

function TList.IsEmpty:Boolean;
begin
  Result:=Items=nil;
end; {TList.IsEmpty}

function TList.IsEqual(Node__:TNode):Boolean;
var p,q:TNode;
begin
  Result:=(Node__<>nil) and (ObjectType=Node__.ObjectType); {'True': 'Node__' is a list too}
  if Result then begin
     p:=Items; q:=TList(Node__).Items;
     while Result and (p<>nil) and (q<>nil) do begin
       Result:=p.IsEqual(q); p:=p.Next; q:=q.Next;
       end;
     Result:=Result and (p=nil) and (q=nil);
     end;
end; {TList.IsEqual}

function TList.IsMember(Node__:TNode):Boolean;
var n:TNode;
begin {Returns 'True' if 'Node__' is a member of the list}
  n:=Items; {note that calling 'Items.IsMember' would not work because 'Items' may itself be a list, in effect making the search a deep-structure search instead of a shallow 1-level search}
  while (n<>nil) and (n<>Node__) do n:=n.Next;
  Result:=n<>nil;
end; {TList.IsMember}

function TList.ItemExists(const Name__:String):Boolean;
begin
  Result:=GetItemByName(Name__)<>nil;
end; {TList.ItemExists}

function TList.Last:TNode;
begin
  Result:=Items;
  if Result<>nil then Result:=Result.Last;
end; {TList.Last}

function TList.LoadFromFile(const FileName__:String):Boolean;
var FileTime:TFileTime;
begin
  Result:=LoadFromFileWithFileTime(FileName__,FileTime);
end;

function TList.LoadFromFileWithFileTime(const FileName__:String; var FileTime__:TFileTime):Boolean;
var TextBuffer:String;
begin
  Result:=LoadTextFromFile(TextBuffer,FileTime__,FileName__) and
          LoadFromText(TextBuffer);
  if not Result then Clear;
end;

function TList.LoadFromText(const Text__:String):Boolean;
var Len,Position:Integer; Line:String; Node:TNode;
begin
  Result:=True; Clear; Position:=0; Len:=Length(Text__);
  try    if   Len<High(Len)-STRING_BASE then
              while ReadLine(Text__,Len,Position,Line) do begin
                Node:=TNode.Create;
                Push(Node);
                Node.Text:=Line;
                end
         else raise Exception.Create(TEXT_TEXT_TOO_LARGE)
  except on E:Exception do Result:=Error(E.Message,'TList.LoadFromText');
  end;
  if   Result then begin
       Reverse; {the lines are in reverse order: make it right}
       if Items<>nil then {remove byte order mark (BOM), if any}
          Items.SetText(StrRemoveBOM(Items.Text));
       end
  else Clear;
end; {TList.LoadFromText}

function TList.LoadFromVector(Count__:Integer; var Vector__:PPNodeVector):Boolean;
var i:Integer;
begin {Add items from a vector, and destroys the vector}
  for i:=Pred(Count__) downto 0 do Push(TNode(Vector__[i]));
  Result:=SafeRealloc(Pointer(Vector__),Count__*SizeOf(Vector__^[Low(Vector__^)]),0,False,True); {'0': the vector is destroyed}
end; {TList.LoadFromVector}

function TList.MakeUniqueName(const Name__,DefaultName__:String; UseParens__:Boolean):String;
var i:Integer;
begin
  i:=0; Result:=MakeUniqueName1(Name__,DefaultName__,UseParens__,i);
end; {TList.MakeUniqueName}

function TList.MakeUniqueName1(const Name__,DefaultName__:String; UseParens__:Boolean; var No__:Integer):String;
var s:String;
begin
  try    Result:=Trim(Name__);
         if (Result='') or (GetItemByName(Result)<>nil) then begin
            s     :=Result;
            if s  = '' then s:=Trim(DefaultName__);
            if s  <>'' then s:=s+SPACE;
            Result:='';
            if (No__<=0) {and UseParens__} then Inc(No__);
            while (Result='') and (No__<High(No__)) do begin
              Inc(No__); {the number increases before the first candidate name}
              if   (Name__='') or (not UseParens__) then
                   Result:=s+IntToStr(No__)
              else Result:=s+LEFT_PAREN+IntToStr(No__)+RIGHT_PAREN;
              if   GetItemByName(Result)<>nil then Result:='';
              end;
            if Result='' then raise Exception.Create(TEXT_MEMORY_FULL);
            end;
  except on E:Exception do begin
            Result:=''; Error(E.Message,'TList.MakeUniqueName1');
            end;
  end;
end; {TList.MakeUniqueName1}

function TList.MacroExpand(MacroStr__:TList; const NewLine__:String):Boolean;
var Modified:Boolean; n:TNode;
begin {Returns 'True' if any expansion takes place}
  Result:=False; n:=Items;
  if (n<>nil) and (MacroStr__<>nil) and (MacroStr__.Items<>nil) then
     repeat if   StrMacroExpand(n.Text,Modified,MacroStr__) then begin
                 if Modified then Result:=True;
                 n:=n.Next;
                 end
            else n:=nil;
     until  n=nil;
  if Result and (NewLine__<>'') then ConvertNewLineToLines(NewLine__);
end; {TList.MacroExpand}

function TList.MergeSort(CompareFunction__:TCompareFunction):TTimeMS;  {'MergeSort' is stable, i.e., it preserves the original order of items with equal keys}
var SortedListSize,PCount,QCount,MergeCount:Integer;
    P,Q,NextItem,LastItem:TNode;
begin {precondition: the number of items on the list is less than or equal to the highest number that can be represented by an integer value}
  Result:=GetTimeMS;
  if Items<>nil then begin
     SortedListSize:=1; {initially, each item is a sorted sublist of length 1}
     repeat P:=Items; Items:=nil; LastItem:=nil; MergeCount:=0;
            while P<>nil do begin {while there are more items to sort in this pass where each sublist has maximum size 'SortedListSize'}
              Inc(MergeCount);
              Q:=P; PCount:=0;
              while (PCount<SortedListSize) and (Q.Next<>nil) do begin {locate the item 'Q' which is 'SortedListSize' nodes ahead of 'P'}
                Inc(PCount); Q:=Q.Next;
                end;
              QCount:=SortedListSize; {merge 'PCount' items starting from 'P' and maximum 'QCount' items starting from 'Q'}
              while (PCount<>0) or ((QCount<>0) and (Q<>nil)) do begin
                {find out which item to take next}
                if PCount<>0 then begin {'True': more 'P' items}
                   if (QCount<>0) and (Q<>nil) then begin {'True': more 'Q' items}
                      if CompareFunction__(P,Q)<=0 then begin {'True': P.Key <= Q.Key}
                         NextItem:=P; Dec(PCount); P:=P.Next;
                         end
                      else begin {P.Key > Q.Key}
                         NextItem:=Q; Dec(QCount); Q:=Q.Next;
                         end;
                      end
                   else begin {no more 'Q' items}
                      NextItem:=P; Dec(PCount); P:=P.Next;
                      end;
                   end
                else begin {no more 'P' items}
                   NextItem:=Q; Dec(QCount); Q:=Q.Next;
                   end;

                {add 'NextItem' to the merged list}
                if   LastItem<>nil then
                     LastItem.Next:=NextItem
                else Items:=NextItem;
                LastItem:=NextItem;
                end;
              P:=Q; {get ready to sort the next two sublists in this pass}
              end;
            LastItem.Next:=nil; {update the last item on the merged list with a list terminator}
            SortedListSize:=SortedListSize*2; {note that 'SortedListSize' cannot overflow before the last iteration has completed the sort, that is, provided the number of items fits in an integer value}
     until  MergeCount<=1; {the sort completes when only 2 sublists were merged}
     end;
  Result:=CalculateElapsedTimeMS(Result,GetTimeMS);
end; {TList.MergeSort}

function TList.MoveAfter(Node__,AfterNode__:TNode):TNode;
begin {'Node__' doesn't have to be a member of the list}
  Result:=Node__;
  if (Node__<>nil) and (Node__<>AfterNode__) then begin
     Remove(Node__,False);
     InsertAfter(Node__,AfterNode__);
     end
end; {TList.MoveAfter}

function TList.MoveToBack(Node__:TNode):TNode;
begin {'Node__' doesn't need to be a member of the list}
  Remove(Node__,False);
  Add(Node__);
  Result:=Node__;
end; {TList.MoveToBack}

function TList.MoveToFront(Node__:TNode):TNode;
begin {'Node__' doesn't need to be a member of the list}
  if   Node__<>Items then // 'True': the node isn't the first member of the list
       Result:=MoveAfter(Node__,nil)
  else Result:=Node__;
end; {TList.MoveToFront}

function TList.NextWithWrapAround(Node__:TNode):TNode;
begin
  if   Node__<>nil then
       if   Node__.Next<>nil then
            Result:=Node__.Next
       else Result:=Items {wrap-around}
  else Result:=nil;
end; {TList.NextWithWrapAround}

function TList.ObjectType:TObjectType;
begin
  Result:=otList;
end; {TList.ObjectType}

function TList.Prev(Node__:TNode):TNode;
var n:TNode;
begin
  Result:=nil; n:=Items;
  while (n<>nil) and (n<>Node__) do begin Result:=n; n:=n.Next; end;
  if n<>Node__ then Result:=nil;
end; {TList.Prev}

function TList.PrevWithWrapAround(Node__:TNode):TNode;
begin
  if   Node__<>nil then
       if   Node__<>Items then
            Result:=Prev(Node__)
       else Result:=Last {wrap-around}
  else Result:=nil;
end; {TList.PrevWithWrapAround}

function TList.Pop:TNode;
begin
  Result:=Items;
  if Items<>nil then Items:=Items.Next;
end; {TList.Pop}

function TList.Push(Node__:TNode):TNode;
begin {Note: for efficiency, 'Node__' must be a single item, not a list (use 'Insert' to add a list of items in one operation)}
  Node__.Next:=Items; {'Node__.Next', not 'Node__.Last.Next'; thus 'Node__' must be a single item}
  Items:=Node__; Result:=Items;
end; {TList.Push}

function TList.ReadString(const Key__:String; var Value__:String):Boolean;
var i,j:Integer; n:TNode;
begin {For easy handling of key/value information, such as 'Author: NN'}
  try    Result:=FindKey(Key__,n);
         if Result then begin
            i:=AnsiPos(EQUAL,n.Text);
            j:=AnsiPos(COLON,n.Text); {either 'key:value', or 'key=value'}
            if (i<STRING_BASE) or ((j>=STRING_BASE) and (j<i)) then i:=j;
            Value__:=Trim(System.Copy(n.Text,Succ(i),Length(n.Text)-i));
            end;
  except on E:Exception do Result:=Error(E.Message,'TList.ReadString');
  end;
end; {TList.ReadString}

function TList.Remove(Node__:TNode; Destroy__:Boolean):TNode;
var n:TNode;
begin {Returns the node, or 'nil' if the node is destroyed; the node doesn't need to be a member of the list}
  Result:=Node__;
  if Node__<>nil then begin
     if Node__=Items then Items:=Items.Next
     else begin
        n:=Prev(Node__);
        if n<>nil then n.Next:=Node__.Next; {nil-check necessary, because the node doesn't need to be a member of the list}
        end;
     Node__.Next:=nil; {ensure that the caller doesn't treat the node as a member of a list}
     if Destroy__ then begin Node__.Free; Result:=nil; end;
     end;
end; {TList.Remove}

function TList.RenameItem(const OldName__,NewName__:String):Boolean;
var Node:TNode;
begin
  Node:=GetItemByName(OldName__);
  Result:=(Node<>nil)
          and
          (StrEqual(OldName__,NewName__) // item keys are not case sensitive
           or
           (GetItemByName(NewName__)=nil)
          )
          and
          Node.SetName(NewName__);
end;

function TList.Reverse:TNode;
var Next,Prev:TNode;
begin
  Prev:=nil;
  while Items<>nil do begin
    Next:=Items.Next; Items.Next:=Prev; Prev:=Items; Items:=Next;
    end;
  Items:=Prev; Result:=Items;
end; {TList.Reverse}

function TList.SaveToFile(const FileName__:String):Boolean;
var IOResult:Integer; F:TextFile;
begin
  try    AssignFile(F,FileName__); Rewrite(F); IOResult:=0;
         try     Inc(IOResult,WriteToFile(Addr(F)));  {write items}
         finally CloseFile(F); Inc(IOResult,System.IOResult);
         end;
         Result:=IOResult=0;
         if not Result then raise Exception.Create(TEXT_WRITE_FILE_ERROR);
  except on E:Exception do
            Result:=SokUtil_.Error(E.Message,TEXT_APPLICATION_TITLE+' - '+TEXT_SAVE_FILE+' - '+FileName__);
  end;
end; {TList.SaveToFile}

function TList.SaveToVector(var Count__:Integer; var Vector__:PPNodeVector):Boolean;
var i:Integer; Node:TNode;
begin {Creates a vector with pointers to each item, and clears the list itself}
  Count__:=Count; Node:=Items;
  Result:=SafeGetMem(Pointer(Vector__),Count__*SizeOf(Vector__^[Low(Vector__^)]),False,True);
  if Result then begin
     for i:=0 to Pred(Count__) do begin Vector__[i]:=Pointer(Node); Node:=Node.Next; end;
     Items:=nil; {caution: the vector owns the items now}
     end;
end; {TList.SaveToVector}

function TList.SetSingleTextLine(const Text__:String):TNode;
begin {makes the list contain a single text line, recycling an existing single node, if present}
 if        Items=nil then
           Result:=AddTextLine(Text__,True)
 else if   (Items.Next=nil) and {'True': there is only one node on the list}
           Items.SetText(Text__) then
           Result:=Items
      else begin Clear;
                 Result:=AddTextLine(Text__,True)
           end;
end;

function TList.StrSubstitute(const OldString__,NewString__,IniFileSection__:String; AllOccurrences__:Boolean):Integer;
var Count:Integer; Enabled:Boolean; s:String; Node:TNode;
begin {Substitutes 'OldString__' with 'NewString' in names for all members of the list, provided 'IniFileSection__' is empty; otherwise, substitution is limited to that section}
  Result:=0;
  Node:=Items; Enabled:=IniFileSection__='';
  try    while Node<>nil do begin
            s:=Node.Text;

            if (IniFileSection__<>'') and
               (s<>'') and (s[STRING_BASE]=LEFT_BRACKET) then
               Enabled:=StrEqual(s,IniFileSection__)
            else
               if Enabled then begin
                  if   AllOccurrences__ then
                       s:=SokUtil_.StrSubstituteNTimes(s,OldString__,NewString__,MaxInt,Count)
                  else s:=SokUtil_.StrSubstituteNTimes(s,OldString__,NewString__,     1,Count);
                  Inc(Result,Count);
                  end;

            Node.Text:=s;
            Node:=Node.Next;
            end;
  except on E:Exception do begin
            Inc(Result); Result:=-Result;
            Error(E.Message,'TList.StrSubstitute');
            end;
  end;
end; {TList.StrSubstitute}

function TList.Swap(List__:TList):Boolean;
begin {Swaps the contents of two lists, *not* the lists}
  Result:=List__ is TList;
  if Result then SokUtil_.SwapNodes(Items,List__.Items);
end; {TList.Swap}

{$IFDEF SokobanYASC}
  function  TList.ToStringList(var SL__:TStringList):Boolean;
  var Node:TNode;
  begin
    try    SL__.Clear; SL__.Sorted:=False; SL__.Capacity:=Count;
           Node:=Items;
           while Node<>nil do begin
             SL__.Add(Node.Text);
             Node:=Node.Next;
             end;
           Result:=True;
    except on E:Exception do begin
           Result:=Error(E.Message,'TList.ToStringList');
           end;
    end

  end;
{$ENDIF}

function TList.ToText(const LineSeparator__:String; var Text__:String):Boolean;
var Index,TextLength,LineSeparatorLength:Integer; Node:TNode;
begin
  Text__:='';
  try    TextLength:=0;
         LineSeparatorLength:=Length(LineSeparator__);
         Node:=Items;
         while Node<>nil do begin {calculate text length including line separators, if any}
           if   TextLength   < High(TextLength)-Length(Node.Text)   then Inc(TextLength,Length(Node.Text))
           else TextLength   :=High(TextLength);
           Node:=Node.Next;
           if Node<>nil then
              if   TextLength< High(TextLength)-LineSeparatorLength then Inc(TextLength,LineSeparatorLength)
              else TextLength:=High(TextLength);
           end;

         if TextLength<High(TextLength) then begin
            if TextLength<>0 then begin
               SetLength(Text__,TextLength); Index:=STRING_BASE;
               Node:=Items;
               while Node<>nil do begin
                 TextLength:=Length(Node.Text);
                 Move(Pointer(Addr(Node.Text[STRING_BASE])^),Pointer(Addr(Text__[Index]))^,TextLength);
                 Inc(Index,TextLength);
                 Node:=Node.Next;
                 if (Node<>nil) and (LineSeparatorLength<>0) then begin
                    Move(Pointer(Addr(LineSeparator__[STRING_BASE]))^,Pointer(Addr(Text__[Index]))^,LineSeparatorLength);
                    Inc(Index,LineSeparatorLength);
                    end;
                 end;
               end;
            Result:=True;
            end
         else raise Exception.Create(TEXT_MEMORY_FULL);
  except on E:Exception do begin
            Text__:='';
            Result:=Error(E.Message,'TList.ToText');
            end;
  end;
end;

function TList.TrimBlankLines:Integer;
var i:Integer;
begin
  Result:=0;
  for i:=0 to 1 do begin {for all items, forwards and backwards}
    while (Items<>nil) and (IsBlank(Items.Text)) do begin
      Pop.Free; Inc(Result);
      end;
    Reverse; {i=0: prepare for the second run; i=1: correct the order after the second run}
    end;
end; {TList.TrimBlankLines}

function TList.WriteString(const Key__,Value__:String):Boolean;
var n:TNode;
begin {For easy handling of key/value information, such as 'Author: NN'}
  Result:=True;
  try    if not FindKey(Key__,n) then begin
            n:=TNode.Create;
            Add(n);
            end;
         {for a human reader, 'Key: Value' looks more natural than 'Key=Value'}
         n.Text:=Key__+COLON+SPACE+Value__;
  except on E:Exception do Result:=Error(E.Message,'TList.WriteString');
  end;
end; {TList.WriteString}

function TList.WriteToFile(TextFile__:PTextFile):Integer;
var n:TNode;
begin
  Result:=0; n:=Items;
  while (Result=0) and (n<>nil) do begin
    Result:=n.WriteToFile(TextFile__);
    n:=n.Next;
    end;
end; {TList.WriteToFile}

{ TRle }

constructor TRle.Create; {throws EOutOfMemory}
begin
  Inherited;
  Clear; RleEnabled:=True;
end;

destructor TRle.Destroy;
begin
  Inherited;
end;

function TRle.Add(Ch__:Char):Boolean;
begin
  if (Ch__<>RleCh) or (not RleEnabled) then begin
     Result:=Flush; RleCh:=Ch__;
     end
  else Result:=True;
  Inc(RleCount);
end;

procedure TRle.Clear;
begin
  Capacity:=0; Position:=0; RleCh:=NULL_CHAR; RleCount:=0;
  {Inherited;} {don't call 'Inherited': the run length encoder may be a member of a list}
end;

function  TRle.Compress(const Str__:String):Boolean;
var i:Integer;
begin {Converts strings like 'RRRLL' to '3RLL'}
 Result:=True; Clear; RleEnabled:=True;
 for i:=0 to Pred(System.Length(Str__)) do
     if Result then Result:=Add(Str__[STRING_BASE+i]);
 if Result then Result:=Flush;
end;

function TRle.CopyTo(Destination__:TNode):Boolean;
begin
  Result:=Error(Format(TEXT_INTERNAL_ERROR_FORMAT,[TEXT_NOT_IMPLEMENTED]),'TRle.CopyTo');
end; {TRle.CopyTo}

function  TRle.Expand(const Str__:String):Boolean;
var i,j,k:Integer; Ch:Char;
begin {Converts strings like '3RLL' to 'RRRLL'}
  Result:=True;
  try    Clear; k:=-1;
         for i:=0 to Pred(System.Length(Str__)) do
             if Result then begin
                Ch:=Str__[STRING_BASE+i];
                if (Ch>='0') and (Ch<='9') then begin
                   if k=-1 then k:=i; {save start position for the number}
                   j:=RleCount*10+Ord(Ch)-Ord('0');
                   if   j>=RleCount then RleCount:=j
                   else raise Exception.Create(TEXT_INTEGER_OVERFLOW);
                   end
                else begin
                  k:=Max(1,RleCount); RleCount:=0;
                  for j:=1 to k do
                      if Result then
                         Result:=Add(Ch) and Flush;
                  k:=-1; {no pending digits right now}
                  end;
                end;
         if k<>-1 then {add pending digits, if any}
            for i:=k to Pred(System.Length(Str__)) do
                if Result then Result:=Add(Str__[STRING_BASE+i]) and Flush;
         if Result then Result:=Add(NULL_CHAR) and Flush; {add an extra string-terminator}
         if Result then Dec(Position); {don't count the extra string-terminator}
  except on E:Exception do Result:=Error(E.Message,'TRle.Expand');
  end;
end;

function TRle.Flush:Boolean;
const MIN_CAPACITY=4090;
var i,DigitCount:Integer;
    Digits:array[0..511] of Char; {Caution: must be large enough to hold the largest machine-word integer as a string; 511 will suffice in the near future (2002) ;)}
begin
  Result:=True;
  try    DigitCount:=0;
         if   RleCount>2 then begin
              repeat Inc(DigitCount);
                     Digits[DigitCount]:=Char(Ord('0')+(RleCount mod 10));
                     RleCount:=RleCount div 10;
              until  (RleCount=0) or (DigitCount=High(Digits));
              if RleCount=0 then RleCount:=1; {emit the buffered character once}
              end;
         i:=Position+DigitCount+RleCount+1; {'+1': for safety (trailing null-character)}
         if (i>Capacity) and (i<High(i)) then begin
            i:=Max(MIN_CAPACITY,Max(2*Capacity,i)); SetLength(Text,i); {throws EOutOfMemory}
            Capacity:=i;
            end;
         if (i<=Capacity) and (i>=0) then begin
            for i:=DigitCount downto 1 do begin
                Text[STRING_BASE+Position]:=Digits[i]; Inc(Position);
                end;
            for i:=1 to RleCount do begin
                Text[STRING_BASE+Position]:=RleCh; Inc(Position);
                end;
            RleCount:=0; RleCh:=NULL_CHAR;
            end
         else raise Exception.Create(TEXT_TEXT_TOO_LARGE); {capacity overflow}
  except on E:Exception do Result:=Error(E.Message,'TRle.Flush');
  end;
end;

{TBloomFilter}

procedure BloomFilterInitialize(var BloomFilter__:TBloomFilter; TableByteSize__:Integer; Table__:PByteVector);
begin // precondition: 'Table__^' is a memory area of byte-size 'TableByteSize__'
  with BloomFilter__ do begin
    TableByteSize:=TableByteSize__; Table:=Table__;
    FillChar(Table^,TableByteSize,0);
    end;
end;

function BloomFilterAdd(var BloomFilter__:TBloomFilter; const Key__:String):THashTableHashKey;
var BitIndex:Integer; LowerCaseKey:String;
begin
  with BloomFilter__ do begin
    LowerCaseKey:=AnsiLowerCase(Key__);
    BitIndex:=StrHashValueDEK(LowerCaseKey) mod Cardinal(TableByteSize*BITS_PER_BYTE);
    Table[BitIndex div BITS_PER_BYTE]:=Table[BitIndex div BITS_PER_BYTE] or (1 shl (BitIndex mod BITS_PER_BYTE));
    BitIndex:=StrHashValuePJW(LowerCaseKey) mod Cardinal(TableByteSize*BITS_PER_BYTE);
    Table[BitIndex div BITS_PER_BYTE]:=Table[BitIndex div BITS_PER_BYTE] or (1 shl (BitIndex mod BITS_PER_BYTE));
    Result:=BitIndex; {return the primary hash-key value}
    end;
end;

procedure BloomFilterAddNumber(var BloomFilter__:TBloomFilter; Key__:Integer);
var ByteIndex, BitIndex:Integer;
begin // precondition: the table byte size must be a non-negative 2^N number
  with BloomFilter__ do begin
    ByteIndex       :=Cardinal(Key__) and Cardinal(Pred(TableByteSize));
    BitIndex        :=Cardinal(Key__) mod BITS_PER_BYTE;
    Table[ByteIndex]:=Table[ByteIndex] or (1 shl BitIndex);
    end;
end;

function BloomFilterLookup(const BloomFilter__:TBloomFilter; const Key__:String; var HashKey__:THashTableHashKey):Boolean; {note that a Bloom filter can return false positives but never false negatives}
var BitIndex:Integer; LowerCaseKey:String;
begin
  Result:=False;
  with BloomFilter__ do begin
    LowerCaseKey:=AnsiLowerCase(Key__);
    BitIndex:=StrHashValuePJW(LowerCaseKey) mod Cardinal(TableByteSize*BITS_PER_BYTE);
    HashKey__:=BitIndex; {return the primary hash-key value}
    if (Table[BitIndex div BITS_PER_BYTE] and (1 shl (BitIndex mod BITS_PER_BYTE)))=0 then exit; {'0': the key isn't in the set; quick and dirty exit}
    BitIndex:=StrHashValueDEK(LowerCaseKey) mod Cardinal(TableByteSize*BITS_PER_BYTE);
    if (Table[BitIndex div BITS_PER_BYTE] and (1 shl (BitIndex mod BITS_PER_BYTE)))=0 then exit; {'0': the key isn't in the set; quick and dirty exit}
    end;
  Result:=True; {note: a Bloom filter can return false positives but never false negatives}
end;

function BloomFilterLookupNumber(const BloomFilter__:TBloomFilter; Key__:Integer):Boolean; {note that a Bloom filter can return false positives but never false negatives}
var ByteIndex,BitIndex:Integer;
begin // precondition: the table byte size must be a non-negative 2^N number
  with BloomFilter__ do begin
    ByteIndex       :=Cardinal(Key__) and Cardinal(Pred(TableByteSize));
    BitIndex        :=Cardinal(Key__) mod BITS_PER_BYTE;
    Result          :=(Table[ByteIndex] and (1 shl BitIndex))<>0;
    end;
end;

end.

