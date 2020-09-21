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

    File Writer Interface
    =====================
    The file writer has 2 functions meant for calling from the outside:

    1. bool AppendToFile(char *FileName__, char *LevelName__)
    --------------------

       Appends the level to an existing file.

    2. bool SaveToFile(char *FileName__, char *LevelName__)
    ------------------

       Writes the level to a new file.

    Miscellaneous
    =============
    Set TAB SIZE = 4 for correct indentation.
    
*/

/* --- compiler settings --- */

/*  __POSITIONS_PER_DEPTH__

    Define '__POSITIONS_PER_DEPTH__' in order to print them to the output file
*/    

/*
#define __POSITIONS_PER_DEPTH__
*/

/* --- texts --- */

char*   OUTPUT_FILE_EXT             = ".out";

/* --- forward declarations --- */

bool NextMoveToText(TPosition **Position__, char *Text__);
bool WriteLevelToFile(FILE *F__, char *LevelName__);

/* --- file writer --- */

bool AppendLevelToFile(char *FileName__, char *LevelName__)
{
    bool Result = FALSE; FILE *F;
    if  ((F = fopen(FileName__,"a"))) {
        Result = WriteLevelToFile(F, LevelName__);
        if (fclose(F)) Result = FALSE;
        }
  return Result;
}

bool FirstMoveToText(TPosition *Position__, TPosition **FirstPosition__, char *Text__)
{   /* caution: the buffer '*Text__' must be large enough to contain the longest possible 1-push move-path */

    TPosition *Prev, *This, *Tmp;

    *FirstPosition__ = Position__;
    while ((*FirstPosition__) && ((*FirstPosition__)->Parent))          /* find root-node (a null-move, not the first real move) */
        *FirstPosition__ = (*FirstPosition__)->Parent;

    This = Position__; Prev = NULL;

    if  (Game.ReverseMode) {
        while (This && This->Parent) {  /* establish forward links */
            This->Next = This->Parent; This = This->Parent;
            }
        if  (*FirstPosition__)          /* insert root-node before the first real move */
            (*FirstPosition__)->Next = Position__;
        }
    else
        while (This) {                  /* establish forward links */
            Tmp = This->Parent; This->Next = Prev; Prev = This; This = Tmp;
            }

    return NextMoveToText(FirstPosition__, Text__);                     /* this works because the root-node is a null-move, it isn't the first real move */
}

bool NextMoveToText(TPosition **Position__, char *Text__)
{   /* caution: the buffer '*Text__' must be large enough to contain the longest possible 1-push move-path */
    int i, PushSquare, MoveCount; bool Result = TRUE; TMoves Moves;

    if  (*Position__)
        *Position__ = (*Position__)->Next;

    if  (*Position__ && ((*Position__)->Move.BoxNo)) {
        if   (Game.ReverseMode)
             PushSquare = Game.BoxPos[(*Position__)->Move.BoxNo] + Game.NeighbourSquareOffset[(*Position__)->Move.Direction];
        else PushSquare = Game.BoxPos[(*Position__)->Move.BoxNo] - Game.NeighbourSquareOffset[(*Position__)->Move.Direction];

        if  (((*Position__)->Move.BoxNo) &&
             CalculatePlayerPath(Game.PlayerPos, PushSquare, TRUE, &MoveCount, &Moves)) {
            for (i = 0; i < MoveCount; i++) /* insert non-pushing player moves */
                *(Text__ + i) = DIRECTION_TO_CHAR[Moves[Succ(i)].Direction];
            if  (Game.ReverseMode) {
                *(Text__ + MoveCount) = toupper(DIRECTION_TO_CHAR[OPPOSITE_DIRECTION[(*Position__)->Move.Direction]]);
                Game.ReverseMode = FALSE;
                DoMove((*Position__)->Move.BoxNo, OPPOSITE_DIRECTION[(*Position__)->Move.Direction]);
                Game.ReverseMode = TRUE;
                }
            else {
                *(Text__ + MoveCount) = toupper(DIRECTION_TO_CHAR[(*Position__)->Move.Direction]);
                DoMove((*Position__)->Move.BoxNo, (*Position__)->Move.Direction);
                }
            *(Text__ + Succ(MoveCount)) = ASCII_NULL; /* add string terminator */
            Result = TRUE;
            }
        else Result = FALSE;
        }
    else
        Result = FALSE;

    if  (!Result) CopyBoard(Game.StartBoard, Game.Board); /* restore start position */
    return Result;
}

bool SaveLevelToFile(char *FileName__, char *LevelName__)
{
    bool Result = FALSE; FILE *F;
    if  ((F = fopen(FileName__,"w"))) {
        Result = WriteLevelToFile(F, LevelName__);
        if (fclose(F)) Result = FALSE;  /* 'fclose' returns 0 if the file was succesfully closed */
        }
  return Result;
}

#define MAX_LINE_LENGTH 70

bool WriteLevelToFile(FILE *F__, char *LevelName__)
{
    int i, Row, MovesCount, LineLength; char *PLine, *PMoves;
    TString Line; TPosition *CurrentPosition; TBoardAsChars BoardAsChars;
    char Moves[MAX_MOVES + 1];

    if  (MAX_LINE_LENGTH < sizeof(Line)) {

        PLine = (char*)&Line;

        fprintf(F__,"%s\n\n",LevelName__);

        SaveBoardToBoardAsChars(&BoardAsChars);
        for (Row = 1; Row <= Game.BoardHeight; Row++) fprintf(F__,"%s\n",&(BoardAsChars[Row][1]));
        fprintf(F__,"\nTitle: %s\n",LevelName__);

        if  (Positions.BestPosition && (Positions.BestPosition->Depth > 0)) {
            fprintf(F__, "Pushes: %d\n", Positions.BestPosition->Depth);
            fprintf(F__, "Lines: %d\n", Positions.BestPosition->Score);
            fprintf(F__, "Seen Positions: %d\n", Positions.Count);
            fprintf(F__, "Duplicate Positions: %d\n", Positions.DuplicatesCount);
            fprintf(F__, "Exhaustive Search: %s\n", TEXT_YES_NO[Positions.DroppedCount == 0]);
#ifdef __POSITIONS_PER_DEPTH__
              fprintf(F__, "\nPositions per search depth:\n");
              MovesCount = Min(Positions.BestPosition->Depth, MAX_SEARCH_DEPTH);
              for (i = 1; i <= MovesCount; i++)
                  fprintf(F__, "%5d  %12d\n", i, Positions.PositionsPerDepthCount[i]);
#endif            
            fprintf(F__, "\nSolution/Pushes\n");

            ZeroMemory(&Line, sizeof(Line)); LineLength = 0;

            if  (FirstMoveToText(Positions.BestPosition, &CurrentPosition, (char*)&Moves))
                do  {

                    PMoves = (char*)&Moves; MovesCount = strlen(PMoves);

                    while (MovesCount) {
                        if  (LineLength >= MAX_LINE_LENGTH) {
                            fprintf(F__, "%s\n", Line);
                            MakeEmptyString(Line); LineLength = 0;
                            }
                        i = Min(MovesCount, MAX_LINE_LENGTH - LineLength);
                        strncat(PLine, PMoves, i);
                        LineLength += i; MovesCount -= i; PMoves += i;
                        }
                    }
                while (NextMoveToText(&CurrentPosition, Moves));

            if (!IsAnEmptyString(Line)) fprintf(F__,"%s\n",Line);
            }

        fprintf(F__,"\n\n"); /* insert seperator in case more levels are added to the file later */

        return TRUE; /* caution: there is no error checking in this function */
        }
    else return Msg("YASGen0: Internal error: Line length range error.", "Write Level to File");
}

#undef MAX_LINE_LENGTH

/*==================================================================*/

