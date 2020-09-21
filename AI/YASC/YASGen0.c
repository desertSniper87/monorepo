/*
YASGen0 - Yet Another Sokoban Level Generator - For Small Levels
Copyright (c) 2003 by Brian Damgaard, Denmark

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/* --- compiler settings --- */

/*  __STATISTICS__

    Define '__STATISTICS__' in order to produce a log file.
*/

#define __STATISTICS__


/* --- include files --- */

#include "YASGen0_Generator.c"      /* the generator */
#include "YASGen0_Reader.c"         /* a file reader */
#include "YASGen0_Writer.c"         /* a file writer */

/* --- texts --- */

char*   TEXT_APPLICATION_TITLE      = "YASGen0";
char*   TEXT_GENERATOR_METHOD[]     = { "Forwards generator, i.e., using boxes",
                                        "Backwards generator, i.e., using goals"};
char*   TEXT_LEVEL_TITLE_SUFFIX     = ", YASGen0 Version";

/* --- console --- */

bool GetCommandLineParameters(  int     argc, char *argv[],
                                TString *InputFileName__,
                                int     *MemorySizeMB__,
                                int     *MaxOpenPositions__,
                                int     *StartNo__,
                                bool    *ReverseMode__)
{   /* a simple and not fool-proof implementation */
    int i; bool Result = FALSE;

    MakeEmptyString(*InputFileName__);
    *MemorySizeMB__     = DEFAULT_TRANSPOSITION_TABLE_SIZE_MEBI_BYTES;
    *MaxOpenPositions__ = MAX_INT;
    *StartNo__          = 1;
    *ReverseMode__      = TRUE;

    if  ((argc >= 2) && (argc <= 6)) {
        strncpy((char*)InputFileName__, argv[1], MAX_STRING_LENGTH);

        if  (argc == 2)
            Result = TRUE;
        else {
            Result = StrToInt(argv[2], &i);
            if  (i > 0) *MemorySizeMB__ = i;

            if  (Result && (argc >= 4)) {
                Result = StrToInt(argv[3], MaxOpenPositions__);
                if (*MaxOpenPositions__ <= 0) *MaxOpenPositions__ = MAX_INT;
                }

            if  (Result && (argc >= 5))
                Result = StrToInt(argv[4], StartNo__);

            if  (Result && (argc >= 6))
                if          ((argv[5][0] == 'F') || (argv[5][0] == 'f'))
                            *ReverseMode__ = FALSE;
                else    if  ((argv[5][0] == 'B') || (argv[5][0] == 'b'))
                            *ReverseMode__ = TRUE;
                        else Result = FALSE;
            }
        }

    return Result;
}

void ShowHelp(void)
{
#ifdef __CONSOLE_APP__
    fprintf(stderr,"Usage:\n");
    fprintf(stderr,"  %s FileName [Memory (MB)] [Max Open] [Level Number] [f | b]\n\n", TEXT_APPLICATION_TITLE);
    fprintf(stderr,"  Memory .........: Transposition table size, mebibytes; default %d MiB.\n",  DEFAULT_TRANSPOSITION_TABLE_SIZE_MEBI_BYTES);
    fprintf(stderr,"  Max Open .......: Max. open nodes, i.e., search limit; default 0 = no limit.\n");
    fprintf(stderr,"  Level Number ...: Number of the first level to process; default 1.\n");
    fprintf(stderr,"  F or B .........: Forwards or backwards generator; default backwards.\n");
    fflush(stderr);
#endif
}

void ShowTitle(void)
{
#ifdef __CONSOLE_APP__
    fprintf(stderr,"%s - Yet Another Sokoban Level Generator - For Small Levels\n", TEXT_APPLICATION_TITLE);
    fprintf(stderr,"Copyright (c) 2003 by Brian Damgaard\n\n");
    fflush(stderr);
#endif
}

#ifdef __STATISTICS__

/* -- level statistics --- */

typedef struct TLevelStatistics {   /* TLevelStatistics */
  bool                              ExhaustiveSearch;
  int                               Height;
  struct TLevelStatistics           *Next;
  int                               PositionCount;
  int                               PushCount;
  int                               Score;
  TString                           Title;
  int                               Width;
} TLevelStatistics;

TLevelStatistics                    *LevelStatistics = NULL;

void ClearStatistics(void)
{
    TLevelStatistics *p;

    while (LevelStatistics) {
        p = LevelStatistics->Next;
        free(LevelStatistics);
        LevelStatistics = p;
        }
}

void FinalizeStatistics(void)
{
  ClearStatistics();
}

void InitializeStatistics(void)
{
  LevelStatistics = NULL; ClearStatistics();
}

TLevelStatistics *MakeLevelStatistics(  char    *Title__,
                                        bool    ExhaustiveSearch__,
                                        int     Width__,
                                        int     Height__,
                                        int     PushCount__,
                                        int     Score__,
                                        int     PositionCount__)
{
    TLevelStatistics    *Result;

    Result  = (TLevelStatistics*) calloc(1,sizeof(TLevelStatistics));
    if  (Result) {
        Result->ExhaustiveSearch    = ExhaustiveSearch__;
        Result->Height              = Height__;
        Result->Next                = LevelStatistics; /* linked list of items, newest first */
        Result->PositionCount       = PositionCount__;
        Result->PushCount           = PushCount__;
        Result->Score               = Score__;
        Result->Width               = Width__;
        StrAppend(Result->Title,Title__);
        LevelStatistics             = Result;   /* updates the global list */
        }
    else
        Msg(TEXT_MEMORY_FULL,TEXT_APPLICATION_TITLE);

    return  Result;
}

#define LINE_LENGTH                 77

bool WriteStatisticsToTextFile(char *OutputFileName__)
{   /* caution: 'OutputFileName__' is the name of the generated level-file, *not* the statistics-file */

    static char *STATISTICS_FILENAME_SUFFIX = ", Statistics.txt";

    int i, Count, ExhaustiveSearchCount;
    TString s; char *p;
    TLevelStatistics Total;
    TLevelStatistics *This, *Next;
    FILE *F;

    MakeEmptyString(s); StrAppend(s, OutputFileName__);
    p = strchr((char*)&s, PERIOD);
    if  (p) *p = ASCII_NULL;
    StrAppend(s,STATISTICS_FILENAME_SUFFIX);

    F = fopen((char*)s,"w");
    if  (F) {
        This = LevelStatistics; LevelStatistics = NULL;
        while (This) {   /* reverse items */
            Next = This->Next; This->Next = LevelStatistics; LevelStatistics = This; This = Next;
            }

        Count = 0; ExhaustiveSearchCount = 0; ZeroMemory(&Total,sizeof(Total));

        for (i = 0; i < LINE_LENGTH; i++) fprintf(F,"-"); fprintf(F,NEWLINE);
        fprintf(F,"%s Results\n", TEXT_APPLICATION_TITLE);
        fprintf(F,"File ....: %s\n",OutputFileName__);
        fprintf(F,"Memory ..: %d MiB\n",(Positions.MemoryByteSize + (512 * 1024)) / (1024 * 1024));
        fprintf(F,"Method ..: %s\n",TEXT_GENERATOR_METHOD[Game.ReverseMode]);
        if  (Positions.MaxOpenPositions != MAX_INT)
            fprintf(F,"Max. Open: %d\n", Positions.MaxOpenPositions);
        for (i = 0; i < LINE_LENGTH; i++) fprintf(F,"-"); fprintf(F,NEWLINE);
        fprintf(F, "%36s%8s%12s%9s%11s\n","Level", "Minimum", "Estimated", "Seen", "Exhaustive");
        fprintf(F, "%4s%3c%3c%26s%8s%8s%13s%11s\n", "No", 'W', 'H', "Name", "Pushes", "Lines", "Positions", "Search");
        for (i = 0; i < LINE_LENGTH; i++) fprintf(F,"-"); fprintf(F,NEWLINE);

        This = LevelStatistics;
        while (This) {
            Count++;
            if (This->ExhaustiveSearch) ExhaustiveSearchCount++;
            Total.PositionCount     += This->PositionCount;
            Total.PushCount         += This->PushCount;
            Total.Score             += This->Score;

            fprintf(F, "%4d%3d%3d", Count, This->Width, This->Height);

            strcpy(s,This->Title);
            p = strchr((char*)&s,DOUBLE_QUOTE);
            if (p)  StrDelete(s, 0, (p - s + 1));
            p = strchr((char*)&s, DOUBLE_QUOTE);
            if (p)  *p = ASCII_NULL;
            p = strstr((char*)&s, TEXT_LEVEL_TITLE_SUFFIX);
            if (p)  *p = ASCII_NULL;
            if (strlen(s) > 25) *(s + 25) = ASCII_NULL;
            fprintf(F, "%26s",s);

            fprintf(F, "%8d%8d%13d", This->PushCount, This->Score, This->PositionCount);
            if (This->ExhaustiveSearch) fprintf(F, "%11s", TEXT_YES_NO[TRUE]);
            fprintf(F,NEWLINE);

            This = This->Next;
            }

        if  (Count > 0) {
            for (i = 0; i < LINE_LENGTH; i++) fprintf(F,"-"); fprintf(F,NEWLINE);
            }

        fprintf(F, "%4d%32s%8d%8d%13d%11d\n",Count,"Total",Total.PushCount, Total.Score, Total.PositionCount, ExhaustiveSearchCount);

        for (i = 0; i < LINE_LENGTH; i++) fprintf(F,"-"); fprintf(F,NEWLINE);

        fclose(F);

        This = LevelStatistics; LevelStatistics = NULL;
        while (This) {   /* reverse items again*/
            Next = This->Next; This->Next = LevelStatistics; LevelStatistics = This; This = Next;
            }

        return TRUE; /* note: there is no error checking in this function */
        }
    else
        return Msg((char*)&s, "File Creation Error:");
}

#undef  LINE_LENGTH

#endif

/* --- process file --- */

void ProcessFile(char *InputFileName__, int StartNo__)
{
    int BestDepth = 0;
    bool ExhaustiveSearch;
    TString InputFileName, OutputFileName;
    FILE *F;
    TBoardAsChars BoardAsChars;

#ifdef __STATISTICS__
    ClearStatistics();
#endif

    MakeEmptyString(InputFileName ); StrAppend(InputFileName, InputFileName__); /* local copy */

    MakeEmptyString(OutputFileName);
    StrAppend(OutputFileName,TEXT_APPLICATION_TITLE);
    StrAppend(OutputFileName,OUTPUT_FILE_EXT);

    if  (LoadFirstLevelFromFile(InputFileName))
        do
            if  (FileReader.LevelCount >= StartNo__) {
#ifdef __CONSOLE_APP__
                printf("\n%s\n",Game.Title);
#endif
                ShowBoard();

                if  ((FileReader.LevelCount == StartNo__) &&
                     (F = fopen(OutputFileName,"w")))
                    fclose(F);                              /* clear file */

                StrAppend(Game.Title, TEXT_LEVEL_TITLE_SUFFIX);

                SaveBoardToBoardAsChars(&BoardAsChars); /* 'RunGenerator()' takes a 'TBoardAsChars' as input, hence, this (extra) conversion */

                RunGenerator(Game.BoardWidth, Game.BoardHeight, &BoardAsChars, &BestDepth, &ExhaustiveSearch);

                if  (BestDepth > 0) {
                    AppendLevelToFile(OutputFileName, Game.Title);
#ifdef __STATISTICS__
                    MakeLevelStatistics(
                        (char*)&Game.Title, Positions.DroppedCount == 0,
                        Game.BoardWidth, Game.BoardHeight,
                        BestDepth, Positions.BestPosition->Score, Positions.Count);
                    WriteStatisticsToTextFile((char*)&OutputFileName);
#endif
                    }
                }
        while (LoadNextLevelFromFile());
}

/* --- application toplevel --- */

void FinalizeApplication(void)
{
    FinalizeGenerator();
#ifdef __CONSOLE_APP__
    printf("Done");
#endif
}

bool InitializeApplication(int argc, char *argv[])
{   int MaxOpenPositions, MemorySizeMB, StartNo; bool Result, ReverseMode; TString InputFileName;

    ShowTitle();
    InitializeFileReader();

    Result = ((sizeof(TPosition) % 4) == 0);
    if  (!Result) {
#ifdef __CONSOLE_APP__
        fprintf(stderr,"YASGen0 Demo: Internal error: Positions are not 4-byte aligned (Size = %d).", sizeof(TPosition));
#endif
        }

    if  (Result)
        Result = GetCommandLineParameters(argc, argv,
                    &InputFileName, &MemorySizeMB,
                    &MaxOpenPositions, &StartNo, &ReverseMode);

    if  (Result) {
        Result = InitializeGenerator(MemorySizeMB);                             /* allocate memory etc. */
        strcpy((char*)&FileReader.InputFileName, (char*)&InputFileName);        /* set filename */
        FileReader.StartNo          = StartNo;                                  /* set start level number */
        Game.ReverseMode            = ReverseMode;                              /* set reverse mode */
        Positions.MaxOpenPositions  = MaxOpenPositions;                         /* set search limit */
        }

    if  (Result) {
#ifdef __CONSOLE_APP__
        fprintf(stdout,"\nMemory: %d MB   Positions Capacity: %d   Position Size: %d\n",
                (Positions.MemoryByteSize + (512 * 1024)) / (1024 * 1024),
                Positions.Capacity,
                sizeof(TPosition));
#endif
        }
    else { ShowHelp(); Msg(NULL, NULL); }

    return Result;
}

void RunApplication(void)
{
    ProcessFile(FileReader.InputFileName, FileReader.StartNo);
}


/* --- main --- */

int main(int argc,char *argv[])
{
    int Result = 0;

    if  (InitializeApplication(argc, argv))
        RunApplication();
    else
        Result = 1;

    FinalizeApplication();

    return Result;
}

/*==================================================================*/

