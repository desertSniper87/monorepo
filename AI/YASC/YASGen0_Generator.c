/*
YASGen0 - Yet Another Sokoban Level Generator - For Small Levels
Copyright (c) 2003 by Brian Damgaard, Denmark

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
    YASGen0 - Yet Another Sokoban Level Generator - For Small Levels
    Copyright (c) 2003 by Brian Damgaard, Denmark

    Generator Interface
    ===================
    The generator has 3 functions meant for calling from the outside:

    1.  bool InitializeGenerator(int TranspositionTableSizeMegaBytes__)
    ----------------------------

        Initializes the generator before it's used.

    2.  void FinalizeGenerator(void)
    --------------------------

        Finalizes the generator after use.

    3.  void RunGenerator(int BoardWidth__,                         -- input
    --------------------- int BoardHeight__,                        -- input
                          TBoardAsChars *BoardAsChars__;            -- input/output; input must be a legal board
                          int  *PushCount__,                        -- output
                          bool *ExhaustiveSearch)                   -- output

        Generates the longest level, counting pushes, based on the input level
        (provided the memory constraints allows an exhaustive search).
        See declarations below for a definition of the board format 'TBoardAsChars'.

    Additionally, there are some options buried inside the global variables.
    They can be modified before calling 'RunGenerator'.

    1.  Game.ReverseMode: 'TRUE': Generate the level backwards using the
        goal positions (gives the best results).

    2.  Game.RandomizePlayerStartPosition: 'TRUE': Avoids that the player
        is next to the first pushed box in a backwards generated level.

    3.  Positions.MaxOpenPositions: Set this to limit the search.

    Limitations
    ===========
    The forwards generator only generates levels based on the player's
    current position. Other access areas aren't investigated.

    Miscellaneous
    =============
    Set TAB SIZE = 4 for correct indentation.
*/


/* --- compiler settings --- */

/*  __CONSOLE_APP__

    Define '__CONSOLE_APP__' in order to write progress messages and errors
    to the console.
*/

#define __CONSOLE_APP__

/*  __TIMESTAMP__

    The transposition table can work in two ways:

    1.  With timestamps in each item.
        This means the generator doesn't need to clear the table
        between each level, thus, initialization is fast.
    2.  Without timestamps in each item.
        This means the generator must clear the table physically for
        each level, thus, the initialization takes somewhat longer, but
        there are room for more items

    Define '__TIMESTAMP__' in order to choose the first method.
*/

/*
#define __TIMESTAMP__
*/

/* --- include files --- */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --- general constants --- */

#define MAX_INT                     0x7fffffff          /* maximum integer value */
#define MAX_SHORT                   0x7fff              /* maximum short value (32767) */
#define MAX_STRING_LENGTH           255                 /* just bad luck if a textline is longer */

#define ASCII_NULL                  0                   /* ascii characters */
#define COLON                       ':'
#define DOUBLE_QUOTE                '"'
#define NEWLINE                     "\n"
#define PERIOD                      '.'
#define QUOTE                       '\''
#define SEMICOLON                   ';'
#define SPACE                       ' '
#define TAB                         '\t'

/* --- general types --- */

typedef enum {FALSE = 0, TRUE}      bool;               /* boolean values */
typedef unsigned char               byte;
typedef char                        TString[MAX_STRING_LENGTH + 1];

/* --- constants --- */

#define BOX                         1                   /* board, internal representation */
#define FLOOR                       2
#define GOAL                        4
#define PLAYER                      8
#define WALL                        16
#define BOARD_PIECES                (BOX + FLOOR + GOAL + PLAYER + WALL)
#define BOARD_FLAGS                 0                   /* no flags in use, but '32', '64', and '128' are available */

#define CH_BOX                      'b'                 /* board, text representation */
#define CH_BOX_ON_GOAL              'B'
#define CH_BOX_XSB                  '$'
#define CH_BOX_ON_GOAL_XSB          '*'
#define CH_GOAL                     '.'
#define CH_FLOOR                    SPACE
#define CH_PLAYER                   'p'
#define CH_PLAYER_ON_GOAL           'P'
#define CH_PLAYER_XSB               '@'
#define CH_PLAYER_ON_GOAL_XSB       '+'
#define CH_WALL                     '#'

#define DEFAULT_TRANSPOSITION_TABLE_SIZE_MEBI_BYTES 128 /* 128 MiB */

#define MAX_BOARD_WIDTH             50
#define MAX_BOARD_HEIGHT            MAX_BOARD_WIDTH
#define MAX_BOARD_SIZE              ((MAX_BOARD_WIDTH + 2) * (MAX_BOARD_HEIGHT + 2))
        /* board size: '0' left/top border; '+1': right/bottom border */
#define MAX_BOX_COUNT               255                 /* limited by the slot 'BoxNo' in 'TMove' */
#define MAX_DIRECTIONS              4                   /* {up,left,down,right} */
#define MAX_HASH_COLLISION_COUNT    32*1024             /* linear search for a matching item or a free slot */
        /* 'MAX_MOVES' only needs to hold the maximum number of moves */
        /* it takes to get the player from one square to another */
        /* plus the maximum number of one-step box-pushes in the position */
#define MAX_MOVES                   ((MAX_BOARD_WIDTH * MAX_BOARD_HEIGHT) + (MAX_BOX_COUNT * MAX_DIRECTIONS))
#define MAX_SEARCH_DEPTH            400
#define MEMORY_BLOCKS_COUNT         8                   /* MBC must be a power of 2, so (MBC-1) can be used as a mask */
#define MEMORY_BLOCKS_MASK          (MEMORY_BLOCKS_COUNT - 1)
#define MIN_BOARD_WIDTH             3
#define MIN_BOARD_HEIGHT            MIN_BOARD_WIDTH

#define UP                          0                   /* moves */
#define LEFT                        1
#define DOWN                        2
#define RIGHT                       3

byte    OPPOSITE_DIRECTION [MAX_DIRECTIONS] = {DOWN, RIGHT, UP, LEFT};

/* --- texts --- */

char    DIRECTION_TO_CHAR           [MAX_DIRECTIONS]    = { 'u', 'l', 'd', 'r' }; /* must be lowercase characters */
char*   TEXT_GENERATOR_TITLE        = "YASGen0";
char*   TEXT_MEMORY_FULL            = "Memory full";
char*   TEXT_YES_NO[]               = {"No", "Yes"};

/* --- types --- */

typedef byte                        TBoard[MAX_BOARD_SIZE + 1];
typedef char                        TBoardAsChars[MAX_BOARD_HEIGHT + 2][MAX_BOARD_WIDTH + 2];
typedef long long                   THashValue;         /* 64-bit integer */

typedef struct TMove {              /* TMove */
  byte                              BoxNo;
  byte                              Direction;
} TMove;

typedef TMove                       TMoves[MAX_MOVES + 1];
typedef int                         TBoxPos[MAX_BOX_COUNT + 1];
typedef int                         TSearchDepthArrayOfInt[MAX_SEARCH_DEPTH + 1];

typedef struct {                    /* TPlayerPathSquare, used by 'CalculatePlayerPath' */
  int                               Distance;
  int                               Parent;
  int                               Direction;
} TPlayerPathSquare;

typedef TPlayerPathSquare           TPlayerPathSquares[MAX_BOARD_SIZE + 1];

typedef struct {                    /* TPlayersReachableSquares */
  int                               Squares[MAX_BOARD_SIZE + 1];
  int                               TimeStamp;
  /* by using a time-stamp it isn't necessary to perform a time-consuming  */
  /* reset each time the program calculates the player's reachable squares */
} TPlayersReachableSquares;

typedef struct TGame {              /* TGame */
  TBoard                            Board;
  THashValue                        BoardHashValue;
  int                               BoardHeight;
  int                               BoardSize;          /* (Width+2)*(Height*2): the extra 2 is for a wall-filled border */
  int                               BoardWidth;
  int                               BoxCount;           /* number of boxes/goals */
  TBoxPos                           BoxPos;             /* position of each box */
  int                               GoalCount;          /* number of boxes/goals */
  TBoxPos                           GoalPos;            /* position of each goal */
  int                               PlayerPos;          /* player's position */
  int                               NeighbourSquareOffset[MAX_DIRECTIONS]; /* {up,left,down,right} square offsets */
  int                               No;                 /* game number */
  TPlayersReachableSquares          PlayersReachableSquares;
  bool                              RandomizePlayerStartPosition; /* true: avoid that player in a reverse mode generated levels starts next to the first pushed box */
  bool                              ReverseMode;        /* true: generate level by pulling boxes away from goal positions */
  TBoard                            StartBoard;         /* board state before first move */
  TString                           Title;
  bool                              VisitedPlayerAccessArea[MAX_BOARD_SIZE+1];
} TGame;

typedef struct TLegend {            /* TLegend */
  byte                              CharToItem[256];    /* mapping characters to internal values */
  bool                              XSBNotation;        /* generate output using XSB notation */
} TLegend;

typedef struct TPosition {          /* TPosition, i.e., a hash-table item */
  THashValue                        HashValue;
  short                             Depth;              /* minimum search depth for reaching this position */
  TMove                             Move;
  struct TPosition                  *Next;              /* linked list of open positions / later re-used for current path */
  struct TPosition                  *Parent;            /* ancestor node */
  short                             PlayerPos;
  short                             Score;
#ifdef __TIMESTAMP__
  int                               TimeStamp;
#endif
} TPosition;

typedef struct TPositionLookupState{/* TPositionLookupState */
  int                               CollisionCount;
  int                               ItemIndex;
  THashValue                        HashValue;
  int                               VectorIndex;
} TPositionLookupState;             /* 'Positions.LookupState' is a static variable for 'LookupPosition/GetFirstPosition/GetNextPosition */

typedef struct TPositionQueue {     /* TPositionQueue, the list of open positions, i.e., not yet expanded nodes */
  int                               Count;
  TPosition                         *Head;
  TPosition                         *Tail;
} TPositionQueue;

typedef TPosition                   TPositionVector[];

typedef struct TPositions {         /* TPositions */    /* the database with seen positions and not yet expanded positions */
  TPosition                         *BestPosition;      /* best position found so far */
  int                               Capacity;           /* total capacity */
  int                               Count;              /* number of stored positions */
  TPosition                         *CurrentPosition;   /* board state matches this position */
  int                               DroppedCount;       /* number of dropped positions */
  int                               DuplicatesCount;
  THashValue                        HashSquareValues[MAX_BOARD_SIZE + 1];
  TPositionLookupState              LookupState;        /* static variables for 'LookupPosition/GetFirstPosition/GetNextPosition */
  int                               MaxOpenPositions;   /* max. open nodes, i.e., search limit */
  size_t                            MemoryByteSize;     /* total memory size, all vectors (banks) */
  TPositionQueue                    OpenPositions;      /* queue: first-in first-out, unexpanded nodes */
  TSearchDepthArrayOfInt            PositionsPerDepthCount; /* number of positions per depth */
  TPositionVector                   *PositionVectors[MEMORY_BLOCKS_COUNT];  /* hash table */
  int                               TimeStamp;          /* by using a time-stamp it isn't necessary to perform a time-consuming reset between each run */
  int                               VectorCapacity;     /* capacity per vector (bank) */
} TPositions;

typedef struct TRandomState {       /* TRandomState */
  int                               RandomNumber;
} TRandomState;

/* --- general macros --- */

#define Min(a, b)                               (((a) <= (b)) ? (a) : (b))
#define Max(a, b)                               (((a) >= (b)) ? (a) : (b))
#define Pred(n)                                 ((n) - 1)
#define StrDelete(String, Pos, Count)           strcpy((String) + (Pos), (String) + (Pos) + (Count))
#define Succ(n)                                 ((n) + 1)
#define ZeroMemory(Destination, Length)         (memset((Destination), 0, (Length)))

/* --- macros --- */

#define ColRowToSquare(Col__, Row__)            ((Row__) * (Game.BoardWidth + 2) + (Col__))
#define CopyBoard(From__, To__)                 memmove(&(To__), &(From__), (Game.BoardSize + 1) * sizeof(Game.Board[0]))
#define InitializeRandomState(RandomNumber__)   RandomState.RandomNumber = (RandomNumber__)
#define IsAnEmptyString(String__)               (String__[0] == ASCII_NULL)
#define MakeEmptyString(String__)               (String__[0] =  ASCII_NULL)
/* 'StrAppend' only works when first argument is of type 'TString' and second argument is of type 'char*' */
#define StrAppend(TString__, PChar__)           (strncat((char*)&(TString__), PChar__, sizeof((TString__)) - 1 - strlen((TString__))))

/* --- global variables --- */

TGame                               Game;
TLegend                             Legend;
TPositions                          Positions;
TRandomState                        RandomState;

/* --- forward declarations --- */

char BoardSquareToChar(int Square__);
bool CalculatePlayerPath(int FromPos__, int ToPos__, bool MakeMoves__, int *MoveCount__, TMoves *Moves__);
int  CalculatePlayersReachableSquares(int *MinPlayerPos__);
bool GetNextPlayerAccessArea();
void InitializeBoard(int BoardWidth__, int BoardHeight__, bool FillSquares__);
void InitializeGame(void);
bool LookupPosition(THashValue HashValue__, int PlayerPos__, TPosition **Position__);
void MovePlayer(int PlayerPos__);
void RemovePathFromOpenPositions(TPosition *Position__);
void SaveBoardToBoardAsChars(TBoardAsChars *BoardAsChars__);
void SetPosition(TPosition *Position__);
void ShowBoard(void);

/* --- general utilities --- */

bool StrToInt(char *s, int *i)
{
    *i = atoi(s);
    return TRUE;                                    /* quick and dirty implementation: no error checking */
}

#define a               1366L                       /* constants for the random function */
#define c               150889L                     /* don't change them unless you know what you're doing */
#define m               714025L

int Random(int Range__)                             /* private random function for reproducable results */
{
    RandomState.RandomNumber = (a * RandomState.RandomNumber + c) % m;
    if (Range__) return RandomState.RandomNumber % Range__;
    else         return 0;
}

#undef  a
#undef  c
#undef  m

/* --- utilitities --- */

void SquareToColRow(int Square__, int *Col__, int *Row__)
{
  *Row__ = Square__ / (Game.BoardWidth + 2);
  *Col__ = Square__ - *Row__ * (Game.BoardWidth + 2);
}

/* --- legend --- */

void InitializeLegend(bool XSBNotation__)
{
    int i;

    ZeroMemory(&Legend,sizeof(Legend));
    Legend.CharToItem[CH_BOX               ] = BOX    + FLOOR;
    Legend.CharToItem[CH_BOX_ON_GOAL       ] = BOX    + GOAL + FLOOR;
    Legend.CharToItem[CH_BOX_XSB           ] = BOX    + FLOOR;
    Legend.CharToItem[CH_BOX_ON_GOAL_XSB   ] = BOX    + GOAL + FLOOR;
    Legend.CharToItem[CH_FLOOR             ] = FLOOR;
    Legend.CharToItem[CH_GOAL              ] = GOAL   + FLOOR;
    Legend.CharToItem[CH_WALL              ] = WALL;
    Legend.CharToItem[CH_PLAYER            ] = PLAYER + FLOOR;
    Legend.CharToItem[CH_PLAYER_ON_GOAL    ] = PLAYER + GOAL + FLOOR;
    Legend.CharToItem[CH_PLAYER_XSB        ] = PLAYER + FLOOR;
    Legend.CharToItem[CH_PLAYER_ON_GOAL_XSB] = PLAYER + GOAL + FLOOR;

    Legend.CharToItem['_'                  ] = FLOOR;   /* extra characters for floor squares */
    for (i = 1; i <= SPACE; i++) Legend.CharToItem[i] = FLOOR;

    Legend.XSBNotation = XSBNotation__;
}

/* --- console --- */

bool Msg(char* Text__, char* Caption__)
{
#ifdef __CONSOLE_APP__
  fprintf(stderr,NEWLINE);
  if (Caption__) { fprintf(stderr,Caption__); fprintf(stderr,NEWLINE); }
  if (Text__   ) { fprintf(stderr,Text__);    fprintf(stderr,NEWLINE); }
  fprintf(stderr,"\nPress [Enter]");
  fflush(NULL);
  getchar();
#else
  /* TO DO
     insert any Windows message handling here
  */
#endif
  return FALSE; /* returns 'FALSE' so 'Msg' conveniently can be used for reporting errors */
}

/* --- board --- */

THashValue CalculateBoardHashValue(void)
{   /* player's position isn't considered, it's saved separately in each tree-node ('TPosition') */
    int i; THashValue Result = 0;

    for (i = 1; i <= Game.BoxCount; i++)
        Result = Result ^ Positions.HashSquareValues[Game.BoxPos[i]];

    return Result;
}

bool GetFirstPlayerAccessArea(void)
{
    ZeroMemory(&Game.VisitedPlayerAccessArea,sizeof(Game.VisitedPlayerAccessArea));
    return GetNextPlayerAccessArea();
}

bool GetNextPlayerAccessArea(void)
{ /* moves the player to the next access area, if any */
    int i, MinPlayerPos;

    MovePlayer(0);                                    /* remove player from the board */
    while ((Game.PlayerPos <= Game.BoardSize)         /* search for an unvisited floor-square */
           &&
           ((Game.Board[Game.PlayerPos] & (WALL + BOX))     ||
            Game.VisitedPlayerAccessArea[Game.PlayerPos]    ||
            (!CalculatePlayersReachableSquares(&MinPlayerPos))
           )
          )
          Game.PlayerPos++;

    if (Game.PlayerPos <= Game.BoardSize) {           /* 'true': found a position */
       MovePlayer(MinPlayerPos);                      /* put player on the board */
       for (i = 0; i <= Game.BoardSize; i++)
           if (Game.PlayersReachableSquares.Squares[i] == Game.PlayersReachableSquares.TimeStamp)
              Game.VisitedPlayerAccessArea[i] = TRUE; /* mark visited access area for the player */
       }
    else Game.PlayerPos = 0;

    return (Game.PlayerPos != 0);
}

void InitializeBoard(int BoardWidth__, int BoardHeight__, bool FillSquares__)
{
    int i, RowOffset;

    BoardWidth__                        = Max(MIN_BOARD_WIDTH,  Min(MAX_BOARD_WIDTH,  BoardWidth__ ));
    BoardHeight__                       = Max(MIN_BOARD_HEIGHT, Min(MAX_BOARD_HEIGHT, BoardHeight__));

    Game.BoardSize                      = (BoardWidth__ + 2) * (BoardHeight__ + 2); /* the extra 2 is for a wall-filled border */

    Game.NeighbourSquareOffset[UP]      = -(BoardWidth__ + 2);
    Game.NeighbourSquareOffset[LEFT]    = -1;
    Game.NeighbourSquareOffset[DOWN]    =  (BoardWidth__ + 2);
    Game.NeighbourSquareOffset[RIGHT]   = 1;

    if  (FillSquares__)
        for (i = 0; i <= Game.BoardSize; i++) Game.Board[i] = WALL;
    else
        if  ((Game.BoardWidth != BoardWidth__) || (Game.BoardHeight != BoardHeight__)) { /* new size? */
            /* add a wall-filled border to ensure that the player always is fenced in by walls */
            for (i = 0; i <= Succ(BoardWidth__); i++)
                Game.Board[i] = WALL;               /* top row */
            for (i = Game.BoardSize - BoardWidth__-2; i <= Game.BoardSize;  i++)
                Game.Board[i] = WALL;               /* bottom row */
            RowOffset = 0;
            for (i = 1 ; i <= BoardHeight__; i++) { /*left and right columns */
                RowOffset += BoardWidth__ + 2;
                Game.Board[RowOffset] = WALL;
                Game.Board[RowOffset+BoardWidth__ + 1] = WALL;
                }
            }

    Game.BoardWidth                     = BoardWidth__; /* save new size */
    Game.BoardHeight                    = BoardHeight__;
}

void LoadBoardFromBoardAsChars(int BoardWidth__, int BoardHeight__, TBoardAsChars *BoardAsChars__)
{   /* precondition: '*BoardAsChars__' contains a legal board */
    /* note: [Top, Left] = [1, 1], not [0, 0] */

    int Col, Row, RowOffset;

    InitializeBoard(BoardWidth__,BoardHeight__,FALSE);

    /* transfer text version of the board to 'Game.Board' in correct left-justified format */
    for (Row = 1; Row <= Game.BoardHeight; Row++) {
        RowOffset = ColRowToSquare(0,Row);
        for (Col = 1; Col <= Game.BoardWidth; Col++)
            Game.Board[RowOffset + Col] = Legend.CharToItem[(byte)*((char*)BoardAsChars__ + (Row * (MAX_BOARD_WIDTH + 2)) + Col)];
        }

    InitializeGame();
}

void MakeGoalSquares(TPosition *Position__)
{
    int i, OriginalPlayerPos;

    OriginalPlayerPos = Game.PlayerPos; Game.GoalCount = 0;
    RemovePathFromOpenPositions(Position__);            /* the position may still be a member of 'Open'; get it out before calling 'SetPosition()' */
    SetPosition(Position__);                            /* recreate position for this node */
    for (i = 0; i <= Game.BoardSize; i++) {             /* make goals, i.e. use current box-positions as goals */
        Game.Board[i] &= (~GOAL);                       /* remove old goal positions, if any */
        if  (Game.Board[i] & BOX) {
            Game.Board[i] += GOAL;
            Game.GoalCount++;
            Game.GoalPos[Game.GoalCount] = i;
            }
        }
    SetPosition(NULL);                                  /* reset moves again */
    MovePlayer(OriginalPlayerPos);
    CopyBoard(Game.Board, Game.StartBoard);
}

void MoveBoxesToGoalSquares(void)
{
    int i;

    Game.BoxCount = 0;
    for (i = 0; i <= Game.BoardSize; i++) {
        Game.Board[i] &= (~BOX);
        if ((Game.Board[i] & GOAL) && (Game.BoxCount < MAX_BOX_COUNT)) {
           Game.BoxCount++;
           Game.BoxPos[Game.BoxCount] = i;
           Game.Board[i] += BOX;
           if (Game.Board[i] & PLAYER) {
              Game.PlayerPos = 0;
              Game.Board[i] -= PLAYER;
              }
           }
        else
           Game.Board[i] &= (~GOAL);
        }

    CopyBoard(Game.Board, Game.StartBoard);
    Game.GoalCount = Game.BoxCount;
    memcpy(&Game.GoalPos, &Game.BoxPos, sizeof(Game.GoalPos));
}

void RandomizePlayerPosition(void)
{
    int i, j, MinPlayerPos;

    j = Random(CalculatePlayersReachableSquares(&MinPlayerPos));
    for (i = 0; i <= Game.BoardSize; i++)
        if (Game.PlayersReachableSquares.Squares[i] == Game.PlayersReachableSquares.TimeStamp) {
           if (!j) MovePlayer(i);
           j--;
           }
}

void SaveBoardToBoardAsChars(TBoardAsChars *BoardAsChars__)
{   /* note: [Top, Left] = [1, 1], not [0, 0] */

    int Col, Row;

    ZeroMemory(BoardAsChars__, sizeof(TBoardAsChars));

    for (Row = 1; Row <= Game.BoardHeight; Row++)
        for (Col = 1; Col <= Game.BoardWidth; Col++)
            *((char*)BoardAsChars__ + (Row * (MAX_BOARD_WIDTH + 2)) + Col) =
                BoardSquareToChar(ColRowToSquare(Col,Row));
}

void ShowBoard(void)
{
#ifdef __CONSOLE_APP__
    int Row; TBoardAsChars BoardAsChars;

    SaveBoardToBoardAsChars(&BoardAsChars);
    for (Row = 1; Row <= Game.BoardHeight; Row++)
        printf("%s\n",&(BoardAsChars[Row][1]));
#endif
}

char BoardSquareToChar(int Square__)
{
    switch (Game.Board[Square__] & (PLAYER + BOX + GOAL + WALL)) {

        case PLAYER:        if      (Legend.XSBNotation)
                                    return CH_PLAYER_XSB;
                            else    return CH_PLAYER;
        case PLAYER + GOAL: if      (Legend.XSBNotation)
                                    return CH_PLAYER_ON_GOAL_XSB;
                            else    return CH_PLAYER_ON_GOAL;
        case BOX:           if      (Legend.XSBNotation)
                                    return CH_BOX_XSB;
                            else    return CH_BOX;
        case BOX + GOAL:    if      (Legend.XSBNotation)
                                    return CH_BOX_ON_GOAL_XSB;
                            else    return CH_BOX_ON_GOAL;
        case GOAL:          return CH_GOAL;
        case WALL:          return CH_WALL;
        default:            return CH_FLOOR;
        }
}

/* --- moves --- */

void DoMove(int BoxNo__, int Direction__)
{
    int FromPos, ToPos;

    if  (BoxNo__) { /* only pushes are handled */
        FromPos                     = Game.BoxPos[BoxNo__];
        ToPos                       = FromPos + Game.NeighbourSquareOffset[Direction__];
        Game.Board[FromPos       ] -= BOX;
        Game.Board[ToPos         ] += BOX;
        Game.BoxPos[BoxNo__      ]  = ToPos;
        Game.Board[Game.PlayerPos] &= (~PLAYER);
        if   (Game.ReverseMode)
             Game.PlayerPos         = ToPos + Game.NeighbourSquareOffset[Direction__];
        else Game.PlayerPos         = FromPos;
        Game.Board[Game.PlayerPos] += PLAYER;
        Game.BoardHashValue         = (Game.BoardHashValue ^ Positions.HashSquareValues[FromPos]) ^ Positions.HashSquareValues[ToPos];
        }
}

void UndoMove(int BoxNo__, int Direction__)
{
    int FromPos, ToPos;

    if (BoxNo__) { /* only pushes are handled */
        FromPos                     = Game.BoxPos[BoxNo__];
        ToPos                       = FromPos - Game.NeighbourSquareOffset[Direction__];
        Game.Board[FromPos       ] -= BOX;
        Game.Board[ToPos         ] += BOX;
        Game.BoxPos[BoxNo__      ]  = ToPos;
        Game.Board[Game.PlayerPos] &= (~PLAYER);
        if   (Game.ReverseMode)
             Game.PlayerPos         = FromPos;
        else Game.PlayerPos         = ToPos - Game.NeighbourSquareOffset[Direction__];
        Game.Board[Game.PlayerPos] += PLAYER;
        Game.BoardHashValue         = (Game.BoardHashValue ^ Positions.HashSquareValues[FromPos]) ^ Positions.HashSquareValues[ToPos];
        }
}

void MovePlayer(int PlayerPos__)
{
    Game.Board[Game.PlayerPos] &= (~PLAYER);                            /* remove player from the board */
    Game.PlayerPos = PlayerPos__;                                       /* update player position */
    if (Game.PlayerPos) Game.Board[Game.PlayerPos] += PLAYER;           /* put player on the board */
}

/* --- game --- */

void InitializeGame(void)
{
    int i;

    InitializeBoard(Game.BoardWidth,Game.BoardHeight,FALSE);                        /* ensure there is a wall-filled border */

    Game.BoxCount = 0; Game.GoalCount = 0; Game.PlayerPos = 0;
    for (i = 0; i <= Game.BoardSize; i++) {
        if  ((Game.Board[i] & BOX))                                                 /* find boxes */
            if   (Game.BoxCount < MAX_BOX_COUNT) {
                 Game.BoxCount++;
                 Game.BoxPos[Game.BoxCount] = i;
                 }
            else Game.Board[i] -= BOX;
        if  ((Game.Board[i] & GOAL))                                                /* find goals */
            if   (Game.BoxCount < MAX_BOX_COUNT) {
                 Game.GoalCount++;
                 Game.GoalPos[Game.GoalCount] = i;
                 }
            else Game.Board[i] -= GOAL;
        if  ((Game.Board[i] & PLAYER))                                              /* find player */
            if   (Game.PlayerPos == 0)
                 Game.PlayerPos = i;
            else Game.Board[i] -= PLAYER;
        Game.Board[i] = Game.Board[i] & BOARD_PIECES;                               /* reset old flags, if any */
        }

    if  (Game.BoxCount == 0) {
        Game.BoxCount = Game.GoalCount;
        for (i = 1; i <= Game.BoxCount; i++) {
            Game.BoxPos[i] = Game.GoalPos[i]; Game.Board[Game.BoxPos[i]] += BOX;
            }
        }

    if  (Game.GoalCount == 0) {
        Game.GoalCount = Game.BoxCount;
        for (i = 1; i <= Game.GoalCount; i++) {
            Game.GoalPos[i] = Game.BoxPos[i]; Game.Board[Game.GoalPos[i]] += GOAL;
            }
       }

    if (Game.BoxCount != Game.GoalCount) {                                          /* enforce same number of boxes and goals */
       for (i = Succ(Min(Game.BoxCount, Game.GoalCount)); i <= Game.BoxCount; i++)
           Game.Board[Game.BoxPos  [i]] -= BOX;
       for (i = Succ(Min(Game.BoxCount, Game.GoalCount)); i <= Game.GoalCount; i++)
           Game.Board[Game.GoalPos [i]] -= GOAL;
       Game.BoxCount  = Min(Game.BoxCount, Game.GoalCount);
       Game.GoalCount = Game.BoxCount;
       }

    CopyBoard(Game.Board, Game.StartBoard);                                         /* save start position */
    Game.BoardHashValue = CalculateBoardHashValue();
    Game.PlayersReachableSquares.TimeStamp = MAX_INT; /* reset before calculating player's reachable squares next (or first) time */
    Positions.CurrentPosition = NULL;
}

/* --- open positions, i.e., not yet expanded positions --- */

void ClearOpenPositions(void)
{
  Positions.DroppedCount += Positions.OpenPositions.Count;
  ZeroMemory(&Positions.OpenPositions, sizeof(Positions.OpenPositions));
}

bool DequeueOpenPosition(TPosition **Position__)
{
    *Position__ = Positions.OpenPositions.Head;
    if  (*Position__) {
        Positions.OpenPositions.Head = Positions.OpenPositions.Head->Next;
        if  (!Positions.OpenPositions.Head)
            Positions.OpenPositions.Tail = NULL;
        (*Position__)->Next = NULL;   /* clear 'Next' because it's reused for current path after removal from the open-queue */
        Positions.OpenPositions.Count--;
        return TRUE;
        }
    else
       return FALSE;
}

void EnqueueOpenPosition(TPosition *Position__)
{
    if   (Positions.OpenPositions.Tail)
         Positions.OpenPositions.Tail->Next = Position__;
    else Positions.OpenPositions.Head       = Position__;
    Positions.OpenPositions.Tail            = Position__;
    Positions.OpenPositions.Tail->Next      = NULL;
    Positions.OpenPositions.Count++;
}

void RemovePathFromOpenPositions(TPosition *Position__)
{   /* note that this destroys the open-set, hence, it's only meant for use when the search is over */
  SetPosition(NULL);    /* reset to start position */
  while (Position__) {  /* the open queue uses 'next' to link members: reset the links */
    Position__->Next    = NULL;
    Position__          = Position__->Parent;
    }
}

/* --- positions (transposition table) --- */

void ClearPositions(void)
{
    int i;

#ifdef __TIMESTAMP__
    if  (Positions.TimeStamp == MAX_INT) {
#endif
        for (i = 0; i < MEMORY_BLOCKS_COUNT; i++)
            if  (Positions.PositionVectors[i])
                ZeroMemory(Positions.PositionVectors[i],Positions.VectorCapacity * sizeof(TPosition));

        Positions.TimeStamp = 0;
#ifdef __TIMESTAMP__
        }
    Positions.TimeStamp++;
#endif
    ClearOpenPositions();
    Positions.Count = 0; Positions.DroppedCount = 0; Positions.DuplicatesCount = 0;
    Positions.BestPosition = NULL; Positions.CurrentPosition = NULL;
    ZeroMemory(&Positions.PositionsPerDepthCount,sizeof(Positions.PositionsPerDepthCount));
}

void FinalizePositions(void)
{
    int i;

    ClearPositions();
    for (i = 0; i < MEMORY_BLOCKS_COUNT; i++)
        if (Positions.PositionVectors[i]) {
            free(Positions.PositionVectors[i]);
            Positions.PositionVectors[i] = NULL;
            }
}

bool GetFirstPosition(THashValue HashValue__, TPosition **Position__)
{   /* searches for a position, ignoring player position */
    return LookupPosition(HashValue__, 0, Position__);
}

bool GetNextPosition(TPosition **Position__)
{   /* searches for a position, ignoring player position */

    while   (
             (Positions.LookupState.CollisionCount < MAX_HASH_COLLISION_COUNT) && /* if false: not found, no free slot */
#ifdef __TIMESTAMP__
             ((*Position__)->TimeStamp == Positions.TimeStamp)
#else
             ((*Position__)->PlayerPos)
#endif
            ) {                                                                   /* if false: not found, return free slot */
        Positions.LookupState.CollisionCount++;
        Positions.LookupState.ItemIndex++;                                        /* try next slot */
        if  (Positions.LookupState.ItemIndex   >= Positions.VectorCapacity) {     /* try next bank */
            Positions.LookupState.VectorIndex   = Succ(Positions.LookupState.VectorIndex) & MEMORY_BLOCKS_MASK;
            Positions.LookupState.ItemIndex     = 0;
            *Position__ = &((*Positions.PositionVectors[Positions.LookupState.VectorIndex])[0]);
            }
         else
            (*Position__)++;                                                      /* advance pointer to next slot */

        if  (                                                                     /* has this slot been used in current run? */
#ifdef __TIMESTAMP__
             ((*Position__)->TimeStamp == Positions.TimeStamp)
#else
             ((*Position__)->PlayerPos)
#endif
             &&
             ((*Position__)->HashValue == Positions.LookupState.HashValue))       /* does the hash-value match? */
            return TRUE;                                                          /* found */
        }

    return FALSE;                                                                 /* not found */
}

bool InitializePositions(size_t MemoryByteSize__)
{
    int i, j; bool Result;

    ZeroMemory(&Positions,sizeof(Positions));
    Positions.MaxOpenPositions  = MAX_INT;

    Positions.VectorCapacity = (MemoryByteSize__ / MEMORY_BLOCKS_COUNT) / sizeof(TPosition);
    Result = Positions.VectorCapacity > 0;
    for (i = 0; i < MEMORY_BLOCKS_COUNT; i++) {         /* multiple chunks instead of one large block */
        Positions.PositionVectors[i] = (TPositionVector*) malloc(Positions.VectorCapacity * sizeof(TPosition));
        if      (Positions.PositionVectors[i])
                Positions.Capacity += Positions.VectorCapacity;
        else    Result = FALSE;
        }
    Positions.MemoryByteSize    = Positions.Capacity * sizeof(TPosition);

    Positions.TimeStamp = MAX_INT; ClearPositions();    /* 'MAX_INT': really clear memory */

    for (i = 0; i < 256; i++)   if (Random(MAX_INT) == Random(MAX_INT));

    for (i = 0; i <= MAX_BOARD_SIZE; i++)               /* initialize the hash square values */
        do {
            Positions.HashSquareValues[i] =
                ((THashValue)Random(MAX_INT) << 48) +
                ((THashValue)Random(MAX_INT) << 32) +
                ((THashValue)Random(MAX_INT) << 16) +
                ((THashValue)Random(MAX_INT) << 8)  +
                ((THashValue)Random(MAX_INT));

            if  (Positions.HashSquareValues[i] < 0)
                Positions.HashSquareValues[i] = -Positions.HashSquareValues[i];

            for (j = 0; j < i; j++)
                if  (Positions.HashSquareValues[j] == Positions.HashSquareValues[i])
                    Positions.HashSquareValues[i] = 0;
            }
        while (Positions.HashSquareValues[i] <= MAX_INT);

    if  (!Result)   Msg(TEXT_MEMORY_FULL, TEXT_GENERATOR_TITLE);

    return Result;
}

bool LookupPosition(THashValue HashValue__, int PlayerPos__, TPosition **Position__)
{
    if (HashValue__ < 0)  HashValue__       = -HashValue__;
    Positions.LookupState.HashValue         = HashValue__;
    Positions.LookupState.CollisionCount    = 0;   /* caution: there is only one set of static information controlling 'Lookup' and 'GetFirst'/'GetNext' */
    Positions.LookupState.VectorIndex       = Positions.LookupState.HashValue & MEMORY_BLOCKS_MASK;   /* multiple chunks */
    Positions.LookupState.ItemIndex         = Positions.LookupState.HashValue % Positions.VectorCapacity;

    *Position__ = &((*Positions.PositionVectors[Positions.LookupState.VectorIndex])[Positions.LookupState.ItemIndex]);

    do  {
        Positions.LookupState.CollisionCount++;
        if  (   /* has this slot been used in current run? */
#ifdef __TIMESTAMP__
             (*Position__)->TimeStamp == Positions.TimeStamp
#else
             ((*Position__)->PlayerPos)
#endif
            )
            if  ( ((*Position__)->HashValue == Positions.LookupState.HashValue) &&
                 (((*Position__)->PlayerPos == PlayerPos__) || (PlayerPos__ == 0))) /* '0': don't care */
                return TRUE;                                                        /* found */
            else {
                Positions.LookupState.ItemIndex++;
                if  (Positions.LookupState.ItemIndex < Positions.VectorCapacity)
                    (*Position__)++;                                                /* try next slot */
                else {                                                              /* try next bank */
                    Positions.LookupState.VectorIndex = (1 + Positions.LookupState.VectorIndex) & MEMORY_BLOCKS_MASK;
                    Positions.LookupState.ItemIndex = 0;
                    *Position__ = &((*Positions.PositionVectors[Positions.LookupState.VectorIndex])[0]);
                    }
                }
        else
            break;                                                                  /* not found, return free slot */
        }
    while (Positions.LookupState.CollisionCount < MAX_HASH_COLLISION_COUNT);        /* not found, no free slot */

    return FALSE;
}

bool MakePosition(  THashValue  HashValue__,
                    int PlayerPos__, int Depth__, int Score__, int BoxNo__, int Direction__,
                    TPosition *Parent__, TPosition **Position__)
{
    *Position__ = NULL;
    if  (LookupPosition(HashValue__, PlayerPos__, Position__)) {        /* the position is already recorded */
        Positions.DuplicatesCount++;
        
        if   ((Score__ < (*Position__)->Score) &&
              (Depth__ <=(*Position__)->Depth)) {                       /* 'True': new best path to this position */
              
              (*Position__)->Move.BoxNo        = BoxNo__;
              (*Position__)->Move.Direction    = Direction__;
              (*Position__)->Parent            = Parent__;
              (*Position__)->PlayerPos         = PlayerPos__;
              (*Position__)->Score             = Score__;              
             
              return TRUE; 
             } 
        else return FALSE;
        }
    else     if (
                    (
#ifdef __TIMESTAMP__
                        (*Position__)->TimeStamp == Positions.TimeStamp
#else
                        ((*Position__)->PlayerPos)
#endif
                    )
                    ||
                    (Depth__ > MAX_SHORT)
                ) { /* no room for new position or depth limit exceeded */
                Positions.DroppedCount++;
                ClearOpenPositions(); /* stop when positions are lost */
                (*Position__) = NULL; /* 'NULL' signals that memory was filled; '*Position__' points to a slot in the hash-table in all other circumstances */
                return FALSE;
                }
            else {  /* save new position */
                Positions.Count++;
                if  (Depth__ <= MAX_SEARCH_DEPTH) 
                    Positions.PositionsPerDepthCount[Depth__]++;                                                
                    
                (*Position__)->Depth             = Depth__;
                if (HashValue__ < 0) HashValue__ = -HashValue__;
                (*Position__)->HashValue         = HashValue__;
                (*Position__)->Move.BoxNo        = BoxNo__;
                (*Position__)->Move.Direction    = Direction__;
                (*Position__)->Parent            = Parent__;
                (*Position__)->PlayerPos         = PlayerPos__;
                (*Position__)->Score             = Score__;
#ifdef __TIMESTAMP__
                (*Position__)->TimeStamp         = Positions.TimeStamp;
#endif
                EnqueueOpenPosition(*Position__);
                return TRUE;
                }
}

void SetPosition(TPosition *Position__)
{   /* do/undo moves so the board matches the position at the tree-node 'Position__' */
    TPosition   *p, *q;

    /* find path back to common ancestor of new position and current position */
    p = Position__; q = NULL;
    /* after a position is removed from the breadth-first queue, 'Next' is reused as forward chain for current path */
    while ((p) && (p->Parent) && (!p->Next) && (p != Positions.CurrentPosition)) {
        p->Next = q;    /* 'Next' used as forward chain for current path */
        q = p;
        p = p->Parent;
        }

    /* undo old moves not on common path */
    while ((Positions.CurrentPosition) && (Positions.CurrentPosition != p)) {
        UndoMove(Positions.CurrentPosition->Move.BoxNo, Positions.CurrentPosition->Move.Direction);
        Positions.CurrentPosition->Next = NULL;   /* 'NULL': this position isn't on current path any more */
        Positions.CurrentPosition = Positions.CurrentPosition->Parent;
        }

    /* perform new moves, starting with common ancestor's successor */
    if  (q) {
        if  (q->Parent)
            q->Parent->Next = q;   /* ensure that the forward chain is updated for common ancestor, if any */
        do  {   DoMove(q->Move.BoxNo,q->Move.Direction);
                q = q->Next;
            }
        while (q);
        }
    else
       if (Position__)  MovePlayer(Position__->PlayerPos);

    Positions.CurrentPosition = Position__;   /* save new current position */
}

/* --- pathfinding --- */

void CalculatePlayerPath_TrySquare(int Square__, int *MoveCount__, TPlayerPathSquares *PlayerPathSquares__, TPlayerPathSquare *ToPosNode__)
{
    int Distance, Direction; int *p;    TPlayerPathSquare *q;
    int Neighbours[MAX_DIRECTIONS + 1];                     /* '+1': one extra element, so a pointer beyond last used element is legal */

    Distance = Succ((*PlayerPathSquares__)[Square__].Distance); /* 'Distance': the distance to neighbour-squares if path goes through 'Square__' */
    if  (Distance < *MoveCount__) {                         /* if false, paths to neighbour-squares via 'Square__' can never be on a best path */
        p = (int*) &Neighbours;
        for (Direction = 0; Direction < MAX_DIRECTIONS; Direction++) {
            *p = Square__ + Game.NeighbourSquareOffset[Direction];

            q  = &((*PlayerPathSquares__)[*p]);             /* use a pointer so the address of 'PlayerPathSquares[*p]' is calculated only once */
            if (Distance < q->Distance) {                   /* new best path to current neighbour */
                q->Distance     = Distance;
                q->Parent       = Square__;
                q->Direction    = Direction;
                p++;                                        /* next move in next slot */
                if  (q == ToPosNode__)
                    *MoveCount__ = Distance;                /* new best result */
                }
            }
        while (p != (int*) &Neighbours) {                   /* visit updated neighbours */
            /* (updating all neighbours in a breadth-first manner before recursion reduces futile calculations) */
            CalculatePlayerPath_TrySquare(*(--p), MoveCount__, PlayerPathSquares__, ToPosNode__);
            }
        }
}

#define INFINITY    (MAX_INT >> 1)

bool CalculatePlayerPath(int FromPos__, int ToPos__, bool MakeMoves__, int *MoveCount__, TMoves *Moves__)
{
    int i; TPlayerPathSquare *ToPosNode; TPlayerPathSquares PlayerPathSquares;

    for (i = 0; i <= Game.BoardSize; i++) {
        if      (!(Game.Board[i] & (WALL + BOX)))
                PlayerPathSquares[i].Distance =  INFINITY;  /* open squares */
        else    PlayerPathSquares[i].Distance = -INFINITY;  /* filled squares, i.e. walls and boxes */
        }

    *MoveCount__ = INFINITY;
    ToPosNode = &PlayerPathSquares[ToPos__];                /* for speed, use a pointer to identify the goal node */

    if  ((PlayerPathSquares[FromPos__].Distance != -INFINITY)) {
        PlayerPathSquares[FromPos__].Distance = 0;
        *MoveCount__ = ToPosNode->Distance;
        CalculatePlayerPath_TrySquare(FromPos__, MoveCount__, &PlayerPathSquares, ToPosNode);
        }

    if  (abs(*MoveCount__) != INFINITY) {
        if  (MakeMoves__) {
            i = ToPos__;
            while (i != FromPos__) {
                (*Moves__)[PlayerPathSquares[i].Distance].BoxNo = 0;
                (*Moves__)[PlayerPathSquares[i].Distance].Direction = PlayerPathSquares[i].Direction;
                i = PlayerPathSquares[i].Parent;
                }
            }
        return TRUE;
        }
    else return FALSE;
}

#undef INFINITY

int CalculatePlayersReachableSquares(int *MinPlayerPos__)
{
    int Result, Square, NeighbourSquare, Direction, *StackTop, Stack[MAX_BOARD_SIZE + 1];

    Result                                           = 0;
    *MinPlayerPos__                                  = Game.PlayerPos;

    if  (Game.PlayersReachableSquares.TimeStamp      == MAX_INT) {        /* a complete reset is only required */
        Game .PlayersReachableSquares.TimeStamp      = 0;                 /* when the timestamp overflows */
        ZeroMemory(&Game.PlayersReachableSquares.Squares, sizeof(Game.PlayersReachableSquares.Squares[0]) * Succ(Game.BoardSize));
        }
    Game.PlayersReachableSquares.TimeStamp++;

    if  (Game.PlayerPos) {
        Result                                       = 1;
	    StackTop                                     = &Stack[1];
	    *StackTop                                    = Game.PlayerPos;
        Game.PlayersReachableSquares.Squares[Game.PlayerPos] = Game.PlayersReachableSquares.TimeStamp;
        while (StackTop != Stack) {
           /* the program spends most of its time in this loop; */
           /* using a stack makes the program run at least 10% faster than */
           /* a version using a more elegant recursive implementation of the loop */
           Square                                    = *StackTop--;       /* get next square from the stack */
           for (Direction = 0; Direction < MAX_DIRECTIONS; Direction++) { /* examine neighbours to this square */
               NeighbourSquare                       = Square + Game.NeighbourSquareOffset[Direction];
               if ((!(Game.Board[NeighbourSquare] & (WALL + BOX))) &&     /* check for walls, boxes, and visited squares */
                  (Game.PlayersReachableSquares.Squares[NeighbourSquare] != Game.PlayersReachableSquares.TimeStamp)) {
                  Game .PlayersReachableSquares.Squares[NeighbourSquare]  = Game.PlayersReachableSquares.TimeStamp;
                  Result++;
                  *(++StackTop)                      = NeighbourSquare;
                  if (NeighbourSquare < *MinPlayerPos__)
                     *MinPlayerPos__                 = NeighbourSquare;
			      }
		       }
           }
        }

    return Result;
}

/* --- search --- */

void GenerateMoves(int LastPushedBoxNo__, int *MoveCount__, TMove *Moves__)
{
    int BoxNo, MinPlayerPos, Square, NeighbourSquare, Direction;

    *MoveCount__ = 0;
    CalculatePlayersReachableSquares(&MinPlayerPos);
    BoxNo = LastPushedBoxNo__; if (!BoxNo) BoxNo = 1;

    if (Game.ReverseMode)
       while (BoxNo <= Game.BoxCount) {
         Square = Game.BoxPos[BoxNo];
         for (Direction = 0; Direction < MAX_DIRECTIONS; Direction++) {
             NeighbourSquare = Square + Game.NeighbourSquareOffset[Direction];
             if ((!(Game.Board[NeighbourSquare                                        ] & (BOX+WALL))) &&
                 (!(Game.Board[NeighbourSquare + Game.NeighbourSquareOffset[Direction]] & (BOX+WALL))) &&
                 (Game.PlayersReachableSquares.Squares[NeighbourSquare] ==
                  Game.PlayersReachableSquares.TimeStamp)) {
                Moves__->BoxNo      = BoxNo;
                Moves__->Direction  = Direction;
                (*MoveCount__)++;
                Moves__++;
                }
              }
         if   (BoxNo != LastPushedBoxNo__)
              BoxNo++;
         else BoxNo = 1; /* first time through the loop */
         if   (BoxNo == LastPushedBoxNo__)
              BoxNo++;   /* skip last pushed box when seeing it for the second time */
         }
    else
       while (BoxNo <= Game.BoxCount) {
         Square = Game.BoxPos[BoxNo];
         for (Direction = 0; Direction < MAX_DIRECTIONS; Direction++)
             if ((Game.PlayersReachableSquares.Squares[Square - Game.NeighbourSquareOffset[Direction]] ==
                  Game.PlayersReachableSquares.TimeStamp)
                 &&
                 (!(Game.Board[Square + Game.NeighbourSquareOffset[Direction]] & (BOX + WALL)))) {
                Moves__->BoxNo      = BoxNo;
                Moves__->Direction  = Direction;
                (*MoveCount__)++;
                Moves__++;
                }
         if   (BoxNo != LastPushedBoxNo__)
              BoxNo++;
         else BoxNo = 1; /* first time through the loop */
         if   (BoxNo == LastPushedBoxNo__)
              BoxNo++;   /* skip last pushed box when seeing it for the second time */
         }
}

bool IsALegalGoalPosition(TPosition *Position__)
{
/*  bool Result; TPosition *p; */

    /* a position is not a legal goal position if there exists a shorter path to a */
    /* position with an identical box constellation, ignoring the player position  */

    /* note: this constraint applied to the backwards generator when it handled    */
    /* each player's access area separately, but it was rendered obsolete at the   */
    /* time the backwards generator switched to handling all access areas in       */
    /* parallel                                                                    */

    return TRUE;

/*  Result = GetFirstPosition(Position__->HashValue, &p);                          */
/*  if  (Result)                                                                   */ 
/*      do      Result = (p->Depth >= Position__->Depth);                          */
/*      while   (Result && GetNextPosition(&p));                                   */
/*  return Result;                                                                 */
}

bool ExpandPosition(TPosition *Position__, int *BestDepth__)
{   /* returns 'FALSE' in case exhaustive analysis fails */

    int i, MinPlayerPos, MoveCount, SuccessorDepth, SuccessorScore; bool Result = TRUE;
    THashValue BoardHashValue; TMoves Moves; TMove *Move; TPosition *Successor;

#ifdef __CONSOLE_APP__
    if  ((Positions.OpenPositions.Count % 10000 == 0)) {
        if  (Game.No)   printf("%d ",Game.No);
        printf("Open: %d Positions: %d Pushes: %d\n", Positions.OpenPositions.Count, Positions.Count, *BestDepth__);
        }
#endif
    SetPosition(Position__);                                /* update board so it matches this position */

    GenerateMoves(Position__->Move.BoxNo,&MoveCount, (TMove*)&Moves);

    Move = (TMove*)&Moves;

    for (i = 0; i < MoveCount; i++) {                       /* for each move */
        DoMove(Move->BoxNo,Move->Direction);                /* perform the move */
        BoardHashValue = Game.BoardHashValue;               /* save hash-value */
        CalculatePlayersReachableSquares(&MinPlayerPos);    /* board hash-value and lowest player-position identifies the position */
        UndoMove(Move->BoxNo, Move->Direction);             /* take the move back again */

        SuccessorDepth = Succ(Position__->Depth);

        if  ((Position__->Move.BoxNo     != Move->BoxNo) ||   /* increase score if the player shifts from one box to another */
             (Position__->Move.Direction != Move->Direction)) /* or if a box changes direction */    
            SuccessorScore = Succ(Position__->Score);
        else
            SuccessorScore = Position__->Score;

        if  (MakePosition(BoardHashValue, MinPlayerPos, SuccessorDepth, SuccessorScore, Move->BoxNo, Move->Direction, Position__, &Successor))
            if  ( (SuccessorDepth >= *BestDepth__) &&
                 ((SuccessorDepth >  *BestDepth__) || (Successor->Score > Positions.BestPosition->Score)) &&
                 IsALegalGoalPosition(Successor)) {
                *BestDepth__ = SuccessorDepth;              /* new best path */ 
                Positions.BestPosition = Successor;  
                }
            else;
        else
            if  (!Successor) {                              /* 'NULL': memory was filled */
                Result = FALSE;                             /* exhaustive analysis failed */
                ClearOpenPositions();                       /* clear open-queue */
                break;                                      /* exit 'for' loop, i.e., stop expanding this position */
                }
        Move++;                                             /* advance pointer to next move */
        }
    return Result;
}

void Search(int *BestDepth__, bool *ExhaustiveSearch__)
{
    int MinPlayerPos, OriginalPlayerPos; TPosition *Position;

    *BestDepth__ = 0; *ExhaustiveSearch__ = TRUE;
    ClearPositions(); OriginalPlayerPos = Game.PlayerPos;

    if ((!Game.PlayerPos) || Game.ReverseMode) /* enqueue all start-positions; this is required by the backwards generator */
       if (GetFirstPlayerAccessArea())
          do     *ExhaustiveSearch__ = MakePosition(Game.BoardHashValue,Game.PlayerPos,0,0,0,UP,NULL,&Positions.BestPosition)
                                       &&
                                       (Positions.OpenPositions.Count <= Positions.MaxOpenPositions);
          while (*ExhaustiveSearch__ && GetNextPlayerAccessArea());
       else  *ExhaustiveSearch__ = FALSE;
    else /* enqueue a single start position */
        *ExhaustiveSearch__ = CalculatePlayersReachableSquares(&MinPlayerPos) &&
                              MakePosition(Game.BoardHashValue,MinPlayerPos,0,0,0,UP,NULL,&Positions.BestPosition)
                              &&
                              (Positions.OpenPositions.Count <= Positions.MaxOpenPositions);

    while   (*ExhaustiveSearch__                                /* the central breadth-first search loop */
             &&   
             DequeueOpenPosition(&Position)
            )
            *ExhaustiveSearch__ = 
              ExpandPosition(Position, BestDepth__)
              &&
              (Positions.OpenPositions.Count <= Positions.MaxOpenPositions);

    RemovePathFromOpenPositions(Positions.BestPosition);        /* best position may still be a member of the open-list */
    ClearOpenPositions();

    SetPosition(NULL); /* undo moves, if any */
    MovePlayer(OriginalPlayerPos); /* restore original player-position, if any */
}

/* --- generator toplevel --- */

void FinalizeGenerator(void)
{
    FinalizePositions();
}

bool InitializeGenerator(int TranspositionTableSizeMegaBytes__)
{
    bool Result;

    ZeroMemory(&Game, sizeof(Game));
    ZeroMemory(&Positions, sizeof(Positions));
    InitializeRandomState(0);
    InitializeLegend(TRUE);

    Result = InitializePositions(TranspositionTableSizeMegaBytes__ * 1024 * 1024);

    Game.ReverseMode                    = TRUE;             /* default settings */
    Game.RandomizePlayerStartPosition   = TRUE;
    Positions.MaxOpenPositions          = MAX_INT;

    return Result;
}

void RunGenerator(int BoardWidth__,                         /* input */
                  int BoardHeight__,                        /* input */
                  TBoardAsChars *BoardAsChars__,            /* input/output; input must be a legal board */
                  int  *PushCount__,                        /* output */
                  bool *ExhaustiveSearch__)                 /* output */
{
    LoadBoardFromBoardAsChars(BoardWidth__, BoardHeight__, BoardAsChars__);

    if  (Game.ReverseMode)
        MoveBoxesToGoalSquares();

    Search(PushCount__, ExhaustiveSearch__);

    if  (*PushCount__) {                                    /* 'True': a level was generated, save it */
        if  (Game.ReverseMode) {
            SetPosition(Positions.BestPosition);            /* use position as start position */
            if  (Game.RandomizePlayerStartPosition)
                RandomizePlayerPosition();                  /* otherwise the player is right next to the first pushed box */
            }
        else
            MakeGoalSquares(Positions.BestPosition);        /* use position as goal position */

        CopyBoard(Game.Board, Game.StartBoard);             /* save the generated level */

        SaveBoardToBoardAsChars(BoardAsChars__);            /* save output */

#ifdef __CONSOLE_APP__
        printf("Pushes: %d\n", *PushCount__);
        printf("Exhaustive Search: %s\n", TEXT_YES_NO[Positions.DroppedCount == 0]);
        ShowBoard(); /* Msg(NULL,NULL); */
#endif
        }
}

/*==================================================================*/

