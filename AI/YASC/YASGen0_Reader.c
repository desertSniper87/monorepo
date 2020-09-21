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

    File Reader Interface
    =====================
    The file reader has 3 functions meant for calling from the outside:

    1.  void InitializeFileReader(void)
    -----------------------------

        Initializes the reader before it's used.

    2.  bool LoadFirstLevelFromFile(char *FileName__)
    -------------------------------

        Opens the file and loads the first level.

    3.  bool LoadNextLevelFromFile(void)
    ------------------------------

        Loads next level from file and closes the file on eof.

    Miscellaneous
    =============
    Set TAB SIZE = 4 for correct indentation.
*/

/* --- texts --- */

char*   SOKOBAN_FILE_EXT            = ".sok";

/* --- types --- */

typedef struct TFileReader {        /* TFileReader */
  TString                           CurrentTextLine;
  FILE                              *InputFile;
  TString                           InputFileName;
  int                               LevelCount;
  int                               StartNo;
} TFileReader;

/* --- global variables --- */

TFileReader                         FileReader;

/* --- forward declarations --- */

bool LoadNextLevelFromFile(void);

/* --- file reader --- */

void InitializeFileReader(void)
{
    ZeroMemory(&FileReader, sizeof(FileReader));
}

bool LoadFirstLevelFromFile(char *FileName__)
{
    bool Result = FALSE; TString ErrorText;

    FileReader.LevelCount = 0;
    MakeEmptyString(FileReader.InputFileName);
    MakeEmptyString(FileReader.CurrentTextLine);
    MakeEmptyString(ErrorText);

    FileReader.InputFile = fopen(FileName__,"r");
    if  (FileReader.InputFile) {
        StrAppend(FileReader.InputFileName, FileName__);
        Result = LoadNextLevelFromFile();
        if (!Result) StrAppend(ErrorText,"No levels found in file: ");
        }
    else
        StrAppend(ErrorText,"File open error (Check path, maybe file wasn't found): ");

    if  (!Result) {
        StrAppend(ErrorText, FileName__);
        Msg((char*)&ErrorText, "YASGen0 Demo: Load first level from file");
        }

    return Result;
}

bool LoadNextLevelFromFile_IsABoardLine(char *TextLine__)
{   /* note: modifies the input-string */

    int Length; char *p;

    Length = strlen(TextLine__);

    if  (Length) {
        p = TextLine__ + Pred(Length);

        while ((p != TextLine__) && (isspace(*p))) p--; /* skip trailing spaces */
        if      ((p == TextLine__) && isspace(*p))
                *p      = ASCII_NULL;                   /* empty line */
        else    p[1]    = ASCII_NULL;                   /* cut trailing spaces */

        p = TextLine__;
        while (Legend.CharToItem[(byte)*p]) p++;        /* legal board characters? */

        if  ((p != TextLine__) && (!*p)) return TRUE;   /* all characters (minimum 1) are legal */
        else return FALSE;
        }
    else
        return FALSE;
}

bool LoadNextLevelFromFile_MakeLevel(int BoardWidth__, int BoardHeight__, TBoardAsChars *BoardAsChars__)
{
    LoadBoardFromBoardAsChars(BoardWidth__, BoardHeight__, BoardAsChars__);

    FileReader.LevelCount++;
    Game.No = FileReader.LevelCount;

    if  (IsAnEmptyString(Game.Title))
        sprintf((char*)&Game.Title,"Level %d", FileReader.LevelCount);

    while (!IsAnEmptyString(Game.Title) && (Game.Title[0] == SEMICOLON))
        StrDelete((char*)&Game.Title,0,1);
    while (!IsAnEmptyString(Game.Title) && (Game.Title[0] <= SPACE))
        StrDelete((char*)&Game.Title,0,1);
    return TRUE;
}

bool LoadNextLevelFromFile(void)
{   /* a simple version, parsing board and optional heading titles only */

    int Col, Row, LineLength, BoardWidth, BoardHeight; bool Result;
    TString ErrorText; TBoardAsChars BoardAsChars; char *p;

    MakeEmptyString(ErrorText);
    Result = (FileReader.InputFile != NULL) && (FileReader.LevelCount < MAX_INT);
    if  (Result) {
        Result = FALSE;

        BoardWidth = 0; BoardHeight = 0; MakeEmptyString(Game.Title);

        for (Row = 0; Row <= MAX_BOARD_HEIGHT; Row++)
            for (Col = 0; Col <= MAX_BOARD_WIDTH; Col++)
                BoardAsChars[Row][Col] = CH_FLOOR;

        while   ((!Result) &&
                 IsAnEmptyString(ErrorText) &&
                 (!((feof(FileReader.InputFile)) && (IsAnEmptyString(FileReader.CurrentTextLine))))) {   /* not eof */

            if  (IsAnEmptyString(FileReader.CurrentTextLine)) { /* a line may be left over from one call to the next */
                p = fgets((char*)&FileReader.CurrentTextLine, sizeof(FileReader.CurrentTextLine), FileReader.InputFile);
                if  (!p) {                                      /* NULL: file read error or eof */
                    MakeEmptyString(FileReader.CurrentTextLine);
                    break;                                      /* assume eof, in effect skipping rest of the file if a read error occured */
                    }
                }

            if  (!IsAnEmptyString(FileReader.CurrentTextLine)) {
                if  (LoadNextLevelFromFile_IsABoardLine((char*)&FileReader.CurrentTextLine)) { /* is it a board line? */
                    LineLength = strlen((char*)&FileReader.CurrentTextLine);
                    if  (BoardHeight < MAX_BOARD_HEIGHT)
                        if (LineLength > MAX_BOARD_WIDTH)
                            StrAppend(ErrorText,"Board has too many columns: ");
                        else {
                            BoardHeight++; BoardWidth = Max(BoardWidth, LineLength);
                            for (Col = 1; Col <= LineLength; Col++)
                                BoardAsChars[BoardHeight][Col] = FileReader.CurrentTextLine[Pred(Col)];
                            MakeEmptyString(FileReader.CurrentTextLine);
                            }
                    else StrAppend(ErrorText,"Board has too many rows: ");
                    }
                else /* the line is normal text, not a board line */
                    if  (BoardHeight >= MIN_BOARD_HEIGHT)   /* done, board is ok */
                        Result = LoadNextLevelFromFile_MakeLevel(BoardWidth, BoardHeight, &BoardAsChars);
                    else { /* no board, or partial board is too small */
                        if  (BoardHeight) {
                            BoardWidth = 0; BoardHeight = 0;
                            for (Col = 1; Col <= MAX_BOARD_WIDTH; Col++)
                                for (Row = 1; Row <= MAX_BOARD_HEIGHT; Row++)
                                    BoardAsChars[Row][Col] = CH_FLOOR;
                            }
                        if  (!IsAnEmptyString(FileReader.CurrentTextLine)) {
                            strcpy((char*)&Game.Title,(char*)&FileReader.CurrentTextLine);  /* last non-blank line before a board is used as title */
                            MakeEmptyString(FileReader.CurrentTextLine);
                            }
                        }
                }
            else StrAppend(ErrorText,"File read error: ");
            }

        if  ((!Result) && (IsAnEmptyString(ErrorText)) && (BoardHeight >= MIN_BOARD_HEIGHT))
            Result = LoadNextLevelFromFile_MakeLevel(BoardWidth, BoardHeight, &BoardAsChars);

        if  (!IsAnEmptyString(ErrorText)) {
            StrAppend(ErrorText, (char*)&FileReader.InputFileName);
            Result  = Msg((char*)&ErrorText, "YASGen0 Demo: Load next level from file");
            }
        }

    if  ((!Result) && (FileReader.InputFile)) {
        fclose(FileReader.InputFile); FileReader.InputFile = NULL; /* clean up, i.e., close the file */
        }

    return Result;
}

/*==================================================================*/

