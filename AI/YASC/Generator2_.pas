unit Generator2_;

interface

uses
  SokUtil_,SokFile_,SokGame_;

const
  GENERATOR_TEMPLATE_HEIGHT    = 5;
  GENERATOR_TEMPLATE_WIDTH     = GENERATOR_TEMPLATE_HEIGHT;
  GENERATOR_BOARD_REGION_HEIGHT
                               = GENERATOR_TEMPLATE_HEIGHT - 2; // "-2": template border with neighbor square constraints
  GENERATOR_BOARD_REGION_WIDTH = GENERATOR_TEMPLATE_WIDTH  - 2; // "-2": template border with neighbor square constraints

function  CreateBoardPrefilledWithWalls(var BoardWidth__,BoardHeight__:Integer; RandomSeed__:Integer; var Board__:SokFile_.TBoard):Boolean;
// postcondition: the function doesn't modify 'BoardWidth__', 'BoardHeight__', and 'Board__' if the function fails and returns 'False'

// _____________________________________________________________________________

implementation

uses
  SysUtils,Windows,Clipbrd,Text_,Misc_,YASGen_;
//        _____________________________
function  CreateBoardPrefilledWithWalls(var BoardWidth__,BoardHeight__:Integer; RandomSeed__:Integer; var Board__:SokFile_.TBoard):Boolean;
// postcondition: the function doesn't modify 'BoardWidth__', 'BoardHeight__', and 'Board__' if the function fails and returns 'False'
const
  _2D_TRANSFORMATION_COUNT     = Ord(High(TBoardTransformation2D))-Ord(Low(TBoardTransformation2D))+1;
  BOARD_CHAR_ILLEGAL_BOX_SQUARE
                               = FLOOR_NON_BLANK_CH1;
  COMPASS_DIRECTIONS           : array[TDirection,0..1] of ShortInt = ((0,-1), (-1,0), (0,1), (1,0)); // up, left, down, right. order fixed, counterclockwise.
  DISABLED_TEMPLATE            = ILLEGAL_SQUARE;
  DISABLED_TEMPLATES           : array[0..3] of Byte = (0, 1, 11, 12); // all-floors, all-walls, and single-wall templates are disabled. the generated boards seem to be better that way.
  EMPTY_SQUARE                 = 0; // empty square on the board. unspecified square in a template.
  GENERATOR_TEMPLATE_COUNT     = 17;
  ILLEGAL_BOX_SQUARE           = FLOOR+BOX_ILLEGAL_MOVE;
  MAX_GENERATOR_BOARD_HEIGHT   = MAX_BOARD_HEIGHT+GENERATOR_TEMPLATE_HEIGHT;
  MAX_GENERATOR_BOARD_WIDTH    = MAX_BOARD_WIDTH +GENERATOR_TEMPLATE_WIDTH;
  MAX_GENERATOR_BOARD_SIZE     = MAX_GENERATOR_BOARD_WIDTH*MAX_GENERATOR_BOARD_HEIGHT;
  MAX_BOARD_REGION_COL_COUNT   = ((MAX_GENERATOR_BOARD_WIDTH +GENERATOR_BOARD_REGION_WIDTH -1) div GENERATOR_BOARD_REGION_WIDTH )*GENERATOR_BOARD_REGION_WIDTH;
  MAX_BOARD_REGION_ROW_COUNT   = ((MAX_GENERATOR_BOARD_HEIGHT+GENERATOR_BOARD_REGION_HEIGHT-1) div GENERATOR_BOARD_REGION_HEIGHT)*GENERATOR_BOARD_REGION_HEIGHT;
  MAX_BOARD_REGION_COUNT       = MAX_BOARD_REGION_COL_COUNT*MAX_BOARD_REGION_ROW_COUNT;
  MAX_FILL_BOARD_REGION_COUNT  = 10000; // to ensure a reasonably small search time
  TEMPLATES_AS_TEXT            : array[0..GENERATOR_TEMPLATE_COUNT-1,0..GENERATOR_TEMPLATE_HEIGHT-1] of PChar =
    // templates from:
    //   The Procedural Generation of Interesting Sokoban Levels
    //   by Joshua Taylor
    (  ('     ', ' --- ', ' --- ', ' --- ', '     '),   // 00 // disabled by default
       ('     ', ' #-- ', ' --- ', ' --- ', '     '),   // 01 // disabled by default
       ('   --', ' ##--', ' --- ', ' --- ', '     '),   // 02
       ('     ', ' ### ', ' --- ', ' --- ', '     '),   // 03
       ('     ', ' ### ', ' #-- ', ' #-- ', '     '),   // 04
       ('  -  ', ' #-- ', '---- ', ' --# ', '     '),   // 05
       ('     ', ' #-- ', '---- ', ' #-- ', '     '),   // 06
       ('  -  ', ' #-- ', '---- ', ' #-# ', '  -  '),   // 07
       ('  -  ', ' #-# ', '-----', ' #-# ', '  -  '),   // 08
       ('  -  ', ' #-# ', ' #_--', ' ### ', '     '),   // 09 '_': a floor, but considered a wall square by the floor connectivity calculation
       ('     ', ' ### ', '-----', ' ### ', '     '),   // 10
       ('     ', ' ----', ' -#--', ' --- ', '     '),   // 11 // disabled by default
       ('     ', ' ### ', ' ### ', ' ### ', '     '),   // 12 // disabled by default
       ('     ', ' ### ', ' #-- ', '---- ', '--   '),   // 13
       (' - - ', ' --- ', ' #-# ', ' --- ', ' - - '),   // 14
       ('     ', ' ### ', ' ### ', ' --- ', ' --- '),   // 15
       ('     ', ' ### ', '--#--', ' --- ', ' --  '));  // 16
type
  TGeneratorBoardSquareValue   = Byte;
  TBoardRegionBackground       = array[0..GENERATOR_TEMPLATE_WIDTH-1,0..GENERATOR_TEMPLATE_HEIGHT-1] of TGeneratorBoardSquareValue;
  TBoardRegion                 = record
    Col                        : Integer;
    Row                        : Integer;
    Template                   : Integer;
    Transformation             : TBoardTransformation2D;
    Background                 : TBoardRegionBackground;
  end;
  TBoardRegions                = record
    Count                      : Integer;
    Items                      : array[0..MAX_BOARD_REGION_COUNT-1] of TBoardRegion;
  end;
  TChars                       = array[ 0 .. ( MaxInt div SizeOf( Char ) ) - 1 ] of Char;
  PChars                       = ^TChars;
  TTemplateNumber              = Byte;
  TTemplateAndTransformation   = packed record
    Template                   : TTemplateNumber;
    Transformation             : TBoardTransformation2D;
    end;
  TTemplateList                = record
    Count                      : Integer;
    Items                      : array[0..(GENERATOR_TEMPLATE_COUNT*_2D_TRANSFORMATION_COUNT)-1] of TTemplateAndTransformation;
  end;
  TTemplateSquares             = array[0..GENERATOR_TEMPLATE_COUNT-1,TBoardTransformation2D,0..GENERATOR_TEMPLATE_HEIGHT-1,0..GENERATOR_TEMPLATE_WIDTH-1] of TGeneratorBoardSquareValue; // row-major order: a debugging convenience
  TGeneratorBoard              = array[0..MAX_GENERATOR_BOARD_WIDTH-1,0..MAX_GENERATOR_BOARD_HEIGHT-1] of TGeneratorBoardSquareValue;
  TTimestampBoard              = array[0..MAX_GENERATOR_BOARD_WIDTH-1,0..MAX_GENERATOR_BOARD_HEIGHT-1] of Integer;

var
  BoardWidth,BoardHeight, BorderSquaresCount,InnerSquaresCount,Timestamp,FillBoardRegionCount:Integer;
  Board:TGeneratorBoard;
  BoardRegions:TBoardRegions;
  TemplateSquares:TTemplateSquares;
  TimestampBoard:TTimestampBoard;
  TemplateStatistics:array[0..GENERATOR_TEMPLATE_COUNT-1] of Integer;
  //        ________________________
  function  TemplateStatisticsToText:String;
  var Index:Integer;
  begin
    Result:='';
    for Index:=Low(TemplateStatistics) to High(TemplateStatistics) do
        Result:=Result+IntToStr(Index)+COLON+SPACE+IntToStr(TemplateStatistics[Index])+NL;
  end;
  //        ____________
  function  CharAtIndex( Text__ : PChar; Index__ : Integer ) : Char;
  begin // returns the code unit at the given index;
        // precondition: the index is a valid index into the text;
    Result := PChars( Text__ )^[ Index__ ];
  end;
  //        _________________
  function  CharToSquareValue(Char__:Char):Integer;
  begin
    case Char__ of
      FLOOR_NON_BLANK_CH2 : Result:=FLOOR;              // '-'
      FLOOR_CH            : Result:=EMPTY_SQUARE;       // space
      WALL_CH             : Result:=WALL;               // '#'
      FLOOR_NON_BLANK_CH1 : Result:=ILLEGAL_BOX_SQUARE; // '_'
      else                  Result:=EMPTY_SQUARE;
    end;
  end;
  //        _________________
  function  SquareValueToChar(Value__:Integer):Char;
  begin
    case Value__ and BOARD_PIECES of // note that this strips the BOX_ILLEGAL_MOVE flag, if any
      FLOOR              : Result:=FLOOR_NON_BLANK_CH2; // '-'
      EMPTY_SQUARE       : Result:=FLOOR_CH;            // space
      WALL               : Result:=WALL_CH;             // '#'
      else                 Result:=QUESTION;            // '?'
    end;
  end;
  //        _________________________
  procedure Calculate2DTransformation(Transformation__:TBoardTransformation2D; Col__,Row__,Width__,Height__:Integer; var ResultCol__,ResultRow__:Integer);
  var Col,Row,Width,Height:Integer;
      OriginalTransformation:TBoardTransformation2D;
  begin
    OriginalTransformation          :=Transformation__;
    if Transformation__             >=t2DRotate0DegreesClockwiseFlipHorizontally then
       Transformation__             :=TBoardTransformation2D(Ord(Transformation__)-Ord(t2DRotate0DegreesClockwiseFlipHorizontally));
    while Transformation__          <>t2DRotate0DegreesClockwise do begin
      Col  :=Col__;            Row  :=Row__;           Width  :=Width__; Height  :=Height__;
      Col__:=Pred(Height)-Row; Row__:=Col;             Width__:=Height ; Height__:=Width; // clockwise rotation
//    Col__:=Row  ;            Row__:=Pred(Width)-Col; Width__:=Height ; Height__:=Width; // counterclockwise rotation
      Transformation__              :=Pred(Transformation__);
      end;
    if   OriginalTransformation     >=t2DRotate0DegreesClockwiseFlipHorizontally then
         ResultCol__                :=Pred(Width__)-Col__
    else ResultCol__                :=Col__;
    ResultRow__                     :=Row__;
  end;
  //        __________________
  function  IsDisabledTemplate(Template__:Integer; Transformation__:TBoardTransformation2D; const TemplateSquares__:TTemplateSquares):Boolean;
  begin
    Result:=(TemplateSquares__[Template__,Transformation__,0,0] and DISABLED_TEMPLATE)<>0;
  end;
  //        _______________
  procedure DisableTemplate(Template__:Integer; Transformation__:TBoardTransformation2D; var TemplateSquares__:TTemplateSquares);
  begin
    TemplateSquares__[Template__,Transformation__,0,0]:=TemplateSquares__[Template__,Transformation__,0,0] or DISABLED_TEMPLATE;
  end;
  //        ___________________
  function  InitializeTemplates:Boolean;
  var Col,Row,NewCol,NewRow,Index,Template:Integer;
      Transformation:TBoardTransformation2D;
  begin
    for Template:=Low(TEMPLATES_AS_TEXT) to High(TEMPLATES_AS_TEXT) do
        for Transformation:=Low(Transformation) to High(Transformation) do
            for Col:=0 to Pred(GENERATOR_TEMPLATE_WIDTH) do
                for Row:=0 to Pred(GENERATOR_TEMPLATE_HEIGHT) do begin
                    Calculate2DTransformation(Transformation,Col,Row,GENERATOR_TEMPLATE_WIDTH,GENERATOR_TEMPLATE_HEIGHT,NewCol,NewRow);
                    TemplateSquares[Template,Transformation,NewRow,NewCol]:=CharToSquareValue(CharAtIndex(TEMPLATES_AS_TEXT[Template,Row],Col));
                    end;
    for Index:=Low(DISABLED_TEMPLATES) to High(DISABLED_TEMPLATES) do
        for Transformation:=Low(Transformation) to High(Transformation) do
            DisableTemplate(DISABLED_TEMPLATES[Index],Transformation,TemplateSquares);
    FillChar(TemplateStatistics,SizeOf(TemplateStatistics),0); // clear statistics
    {$WARNINGS OFF}
      Result:=(GENERATOR_TEMPLATE_COUNT<=High(TTemplateNumber)) and // sanity check: it must be possible to store the template numbers in the defined field type
              (DISABLED_TEMPLATE<High(TGeneratorBoardSquareValue)); // sanity check: it must be possible to store the flag value in the template squares
    {$WARNINGS ON}
  end;
  //        __________________________
  function  CalculateWallNeighborCount(Col__,Row__:Integer):Integer;
  var Direction:TDirection;
  begin
    Result:=0;
    for Direction:=Low(Direction) to High(Direction) do
        if (Board[Col__+COMPASS_DIRECTIONS[Direction,0],Row__+COMPASS_DIRECTIONS[Direction,1]] and WALL)<>0 then
           Inc(Result);
  end;
  //        _____________
  function  IsFloorSquare(Col__,Row__:Integer):Boolean;
  begin
    Result:=(Board[Col__,Row__] and FLOOR)<>0;
  end;
  //        __________________
  function  IsIllegalBoxSquare(Col__,Row__:Integer):Boolean;
  begin
    Result:=(Board[Col__,Row__] and (FLOOR+BOX_ILLEGAL_MOVE))=(FLOOR+BOX_ILLEGAL_MOVE);
  end;
  //        ____________
  function  IsWallSquare(Col__,Row__:Integer):Boolean;
  begin
    Result:=(Board[Col__,Row__] and WALL)<>0;
  end;
  //        ______________
  function  IsCornerSquare(Col__,Row__:Integer; var CornerTypeSet__:TCornerTypeSet):Boolean;
  begin
    CornerTypeSet__:=[];
    if IsFloorSquare(Col__,Row__) and
       (Col__>0) and (Row__>0) and (Col__<Pred(BoardWidth)) and (Row__<Pred(BoardHeight)) then begin
       if IsWallSquare(Pred(Col__),Row__) then begin
          if IsWallSquare(Col__,Pred(Row__)) then
             Include(CornerTypeSet__,ctTopLeft);
          if IsWallSquare(Col__,Succ(Row__)) then
             Include(CornerTypeSet__,ctBottomLeft);
          end;
       if IsWallSquare(Succ(Col__),Row__) then begin
          if IsWallSquare(Col__,Pred(Row__)) then
             Include(CornerTypeSet__,ctTopRight);
          if IsWallSquare(Col__,Succ(Row__)) then
             Include(CornerTypeSet__,ctBottomRight);
          end;
       end;
    Result:=CornerTypeSet__<>[];
  end;
  //        _______________
  function  InitializeBoard:Boolean;
  var Col,Row:Integer;
    //        ___________
    procedure SpiralOrder(Width__,Height__:Integer; var BoardRegions__:TBoardRegions);
    var Left,Top,Right,Bottom,Direction,Index,Offset:Integer;
      //        ___
      procedure Add(Col__,Row__:Integer; var Index__:Integer);
      begin
        Inc(BoardRegions__.Count);
        Dec(Index__);
        with BoardRegions__.Items[Index__] do begin
          Col:=Succ(Col__*GENERATOR_BOARD_REGION_WIDTH);  // region array indices to board array indices
          Row:=Succ(Row__*GENERATOR_BOARD_REGION_HEIGHT);
          end;
      end;

    begin // SpiralOrder
      Left:=0; Top:=0; Right:=Pred(Width__); Bottom:=Pred(Height__);
      Direction:=0; Index:=Width__*Height__; // fill regions in descending order. this puts the spiral center in element[0].
      while (Left <= Right) and (Top <= Bottom) do begin
        case Direction of
          0: begin for Offset:=0 to Right-Left do Add(Left+Offset,Top,Index);       // going right, top row
                   Inc(Top);
             end;
          1: begin for Offset:=0 to Bottom-Top do Add(Right,Top+Offset,Index);      // going down, right column
                   Dec(Right);
             end;
          2: begin for Offset:=0 to Right-Left do Add(Right-Offset,Bottom,Index);   // going left, bottom row
                   Dec(Bottom);
             end;
          3: begin for Offset:=0 to Bottom-Top do Add(Left,Bottom-Offset,Index);    // going up, left column
                   Inc(Left);
             end;
        end;
        Direction:=Succ(Direction) mod 4; // advance to the next direction. order: right, down, left, up.
        end;
    end;

  begin // InitializeBoard
    // initialize game board
    BoardWidth :=Max(MIN_BOARD_WIDTH ,Min(MAX_BOARD_WIDTH -2,BoardWidth__ )); // -2: for a wall-filled border
    BoardHeight:=Max(MIN_BOARD_HEIGHT,Min(MAX_BOARD_HEIGHT-2,BoardHeight__));
    while (BoardWidth  mod GENERATOR_BOARD_REGION_WIDTH )<>0 do Dec(BoardWidth );
    while (BoardHeight mod GENERATOR_BOARD_REGION_HEIGHT)<>0 do Dec(BoardHeight);
    Inc(BoardWidth,2); Inc(BoardHeight,2); // +2: for a wall-filled border
    Result:=(BoardWidth>=MIN_BOARD_WIDTH) and (BoardHeight>=MIN_BOARD_HEIGHT); // sanity check

    InnerSquaresCount  :=(BoardWidth-2)*(BoardHeight-2);
    BorderSquaresCount :=(BoardWidth*BoardHeight)-InnerSquaresCount;

    for Col:=0 to MAX_GENERATOR_BOARD_WIDTH-1 do
        for Row:=0 to MAX_GENERATOR_BOARD_HEIGHT-1 do
            Board[Col,Row]:=WALL;
    for Col:=1 to BoardWidth-2 do
        for Row:=1 to BoardHeight-2 do
            Board[Col,Row]:=EMPTY_SQUARE;

    // initialize board regions
    // make regions starting from the center of the board and then going round
    // in a spiral. (it happens to be a counterclockwise spiral.) the board
    // constraints are checked each time a region tentatively is filled with a
    // template. filling touching regions first helps catching constraint
    // violations early on, such as the "no open 3x4 floor areas" constraint.
    FillChar(BoardRegions,SizeOf(BoardRegions),0);
    SpiralOrder((BoardWidth -2) div GENERATOR_BOARD_REGION_WIDTH,
                (BoardHeight-2) div GENERATOR_BOARD_REGION_HEIGHT,
                BoardRegions);
  end;
  //        ____________________
  function  TryToFillBoardRegion(BoardRegionIndex__:Integer):Boolean;
  var Index:Integer;
      TemplateCandidates:TTemplateList;
    //        ________________
    procedure ShuffleTemplates(var TemplateList__:TTemplateList);
    var Index,Index2:Integer;
        Item:TTemplateAndTransformation;
    begin // algorithm: Knuth shuffle
      with TemplateList__ do
        for Index:=0 to Count-2 do begin
            Index2:=Index+Random(Count-Index);
            Item:=Items[Index];
            Items[Index]:=Items[Index2];
            Items[Index2]:=Item;
            end;
    end;
    //        ______________________
    procedure FindTemplateCandidates(Col__, Row__:Integer; var TemplateList__:TTemplateList);
    var Template:Integer;
        Transformation:TBoardTransformation2D;
      //        ___________________
      function  IsTemplateCandidate(Template__:Integer; Transformation__:TBoardTransformation2D; Col__,Row__:Integer):Boolean;
      var TemplateCol,TemplateRow:Integer;
        //        ________________________
        function  IsCompatibleSquareValues(TemplateSquareValue__,BoardSquareValue__:Integer):Boolean;
        begin
          Result:=(TemplateSquareValue__=EMPTY_SQUARE) or
                  (BoardSquareValue__   =EMPTY_SQUARE) or
                  (TemplateSquareValue__=BoardSquareValue__);
        end;

      begin // IsTemplateCandidate: returns 'True' if the template fits on the board at [Col__,Row__], taking the template border square constraints into account
        Result:=not IsDisabledTemplate(Template__,Transformation__,TemplateSquares);
        for TemplateCol:=0 to GENERATOR_TEMPLATE_WIDTH-1 do
            if Result then begin
               for TemplateRow:=0 to GENERATOR_TEMPLATE_HEIGHT-1 do
                   if Result then
                      Result:=IsCompatibleSquareValues(TemplateSquares[Template__,Transformation__,TemplateRow,TemplateCol],
                                                       Board[Col__+TemplateCol,Row__+TemplateRow]);
               end
            else break; // quick-and-dirty exit loop
      end;

    begin // FindTemplateCandidates
      TemplateList__.Count:=0;
      for Template:=0 to GENERATOR_TEMPLATE_COUNT-1 do
          for Transformation:=Low(Transformation) to High(Transformation) do
              if IsTemplateCandidate(Template,Transformation,Col__,Row__) then with TemplateList__ do begin
                 Items[Count].Template:=Template;
                 Items[Count].Transformation:=Transformation;
                 Inc(Count);
                 Inc(TemplateStatistics[Template]); // count the number of valid template candidates
                 end;
    end;
    //        __________________
    procedure PutTemplateOnBoard(Template__:Integer; Transformation__:TBoardTransformation2D; BoardRegionIndex__:Integer);
    var TemplateCol,TemplateRow,TemplateSquareValue:Integer;
    begin
      with BoardRegions.Items[BoardRegionIndex__] do begin
        Template:=Template__;             // remember the template currently used for filling this board region (not used)
        Transformation:=Transformation__; // remember the template currently used for filling this board region (not used)
        for TemplateCol:=0 to GENERATOR_TEMPLATE_WIDTH-1 do
            for TemplateRow:=0 to GENERATOR_TEMPLATE_HEIGHT-1 do begin
                Background[TemplateCol,TemplateRow]:=Board[Pred(Col)+TemplateCol,Pred(Row)+TemplateRow]; // save the current contents of the board so it can be restored on backtracking. // 'Pred': templates have a one square border with contraints.
                TemplateSquareValue:=TemplateSquares[Template__,Transformation__,TemplateRow,TemplateCol];
                if TemplateSquareValue<>EMPTY_SQUARE then
                   Board[Pred(Col)+TemplateCol,Pred(Row)+TemplateRow]:=TemplateSquareValue; // 'Pred': templates have a one square border with neighbor square constraints
                end;
        Inc(FillBoardRegionCount); // count the number of attempts to fill a board region
        end;
    end;
    //        ______________________
    procedure UndoPutTemplateOnBoard(BoardRegionIndex__:Integer);
    var TemplateCol,TemplateRow:Integer;
    begin
      with BoardRegions.Items[BoardRegionIndex__] do
        for TemplateCol:=0 to GENERATOR_TEMPLATE_WIDTH-1 do
            for TemplateRow:=0 to GENERATOR_TEMPLATE_HEIGHT-1 do
                Board[Pred(Col)+TemplateCol,Pred(Row)+TemplateRow]:=Background[TemplateCol,TemplateRow]; // 'Pred': templates have a one square border with neighbor square constraints
    end;
    //        _________________________
    function  BoardSatisfiesConstraints:Boolean;
    var
      Col,Row,ConnectedFloorsCount,WallCount:Integer;
      //        ______________________
      function  IsIsolated2xNFloorArea(Col__,Row__:Integer):Boolean;
      var ColCount,RowCount,RowOffset,NeighborRow,ConnectorCount:Integer;
          CornerSquareTypes:TCornerTypeSet;

        function  FloorsOrUnfilledSquaresConnectingFloorAreaToTheRestOfTheBoard(Left__,Top__,Width__,Height__:Integer):Integer;
        var Bottom,Right,ColOffset,RowOffset:Integer;
        begin // returns the number of non-wall squares connecting the floor area with the rest of the board
          Result:=0;
          Right:=Left__+Width__;  // exclusive
          Bottom:=Top__+Height__; // exclusive
          for ColOffset:=0 to Pred(Width__) do begin
               if not IsWallSquare(Left__+ColOffset,Pred(Top__)) then Inc(Result);
               if not IsWallSquare(Left__+ColOffset,Bottom     ) then Inc(Result);
               end;
          for RowOffset:=0 to Pred(Height__) do begin
              if not IsWallSquare(Pred(Left__),Top__+RowOffset) then Inc(Result);
              if not IsWallSquare(Right       ,Top__+RowOffset) then Inc(Result);
              end;
        end;

      begin // IsIsolated2xNFloorArea: returns 'true' if there is a 2xN or an Nx2 floor area at the given column and row with at the most one floor or unfilled square connecting it to the rest of the board
        Result:=False;
        if IsCornerSquare(Col__,Row__,CornerSquareTypes) then begin
           if        ctTopLeft    in CornerSquareTypes then
                     RowOffset:=COMPASS_DIRECTIONS[dDown,1]
           else if   ctBottomLeft in CornerSquareTypes then
                     RowOffset:=COMPASS_DIRECTIONS[dUp  ,1]
                else RowOffset:=0; // not a left side corner square
           if (RowOffset<>0) // 'True': a left side corner square. it suffices to check these types of corner squares.
              and
              ((BoardWidth >GENERATOR_BOARD_REGION_WIDTH +2) // '+2': wall border
               or
               (BoardHeight>GENERATOR_BOARD_REGION_HEIGHT+2)) then begin // 'True': the board is big enough to have isolated floor areas. the rationale for first checking it here is that in practice, the user never generates minimum size boards.
              // look for an N columns x 2 rows area with floors or unfilled squares, i.e., with no walls
              ColCount:=0;
              NeighborRow:=Row__+RowOffset;
              while (not IsWallSquare(Col__+ColCount,Row__)) and
                    (not IsWallSquare(Col__+ColCount,NeighborRow)) do
                    Inc(ColCount);
              if (ColCount>1) and
                 (FloorsOrUnfilledSquaresConnectingFloorAreaToTheRestOfTheBoard(Col__,Min(Row__,NeighborRow),ColCount,2)<=1) then begin
                 Result:=True;
                 end
              else begin
                 // look for a 2 columns x N rows area with floors or unfilled squares, i.e., with no walls
                 RowCount:=0;
                 while (not IsWallSquare(Col__      ,Row__+(RowCount*RowOffset))) and
                       (not IsWallSquare(Succ(Col__),Row__+(RowCount*RowOffset))) do
                       Inc(RowCount);
                 if RowCount>1 then begin
                    ConnectorCount:=FloorsOrUnfilledSquaresConnectingFloorAreaToTheRestOfTheBoard(Col__,Min(Row__,Row__+(Pred(RowCount)*RowOffset)),2,RowCount);
                    if ConnectorCount<=1 then
                       Result:=True
                    else if (ConnectorCount=2) and (ColCount=2) and (RowCount=2) then
                            // look for a dead-end 2x2 floor area
                            // ####
                            // #--#
                            // #---
                            // ##-
                            if (CalculateWallNeighborCount(Col__      ,Row__      )=0) or
                               (CalculateWallNeighborCount(Succ(Col__),Row__      )=0) or
                               (CalculateWallNeighborCount(Col__      ,NeighborRow)=0) or
                               (CalculateWallNeighborCount(Succ(Col__),NeighborRow)=0) then
                               Result:=True;
                    end;
                 end;
              end;
           end;
      end;
      //        ______________
      function  Is4x3FloorArea(Col__,Row__:Integer):Boolean;
      var ColOffset,RowOffset:Integer;
      begin // returns 'true' if there is at least a 4x3 or 3x4 open floor area at the specified position
        Result:=True;
        for ColOffset:=0 to 2 do // first check for a 3x3 floor block
            if Result then
               for RowOffset:=0 to 2 do
                   if not IsFloorSquare(Min(BoardWidth,Col__+ColOffset),Min(BoardHeight,Row__+RowOffset)) then
                      Result:=False;

        if Result then begin // true: found a 3x3 floor block
           for ColOffset:=0 to 2 do // check 3x4 columns x rows
               if not IsFloorSquare(Min(BoardWidth,Col__+ColOffset),Min(BoardHeight,Row__+3)) then
                  Result:=False;
           if not Result then begin // didn't find a 3x4 floor block. check for a 4x3 floor block.
              Result:=True;
              for RowOffset:=0 to 2 do // check 4x3 columns x rows
                  if not IsFloorSquare(Min(BoardWidth,Col__+3),Min(BoardHeight,Row__+RowOffset)) then
                     Result:=False;
              end;
           end;
      end;
      //        _______________
      function  VisitAccessArea(Col__,Row__:Integer):Integer;
      var NeighborCol,NeighborRow,StackTop:Integer;
          Direction:TDirection;
          Stack:array[0..MAX_GENERATOR_BOARD_SIZE*2] of Integer; // *2: for col, row
        //        ____
        procedure Push(Col__,Row__:Integer);
        begin
          Inc(Result); // count connected floors and unfilled squares
          TimestampBoard[Col__,Row__]:=Timestamp; // mark the square as visited
          Stack[StackTop]:=Col__;
          Stack[Succ(StackTop)]:=Row__;
          Inc(StackTop,2);
        end;
        //        ___
        procedure Pop(var Col__,Row__:Integer);
        begin
          Dec(StackTop,2);
          Col__:=Stack[StackTop];
          Row__:=Stack[Succ(StackTop)];
        end;

      begin // VisitAccessArea: returns the number of connected floors and unfilled squares, starting from the specified square
        Result:=0;
        StackTop:=0;
        Push(Col__,Row__);
        while StackTop>0 do begin
           Pop(Col__,Row__);
           for Direction:=Low(Direction) to High(Direction) do begin
               NeighborCol:=Col__+COMPASS_DIRECTIONS[Direction,0]; NeighborRow:=Row__+COMPASS_DIRECTIONS[Direction,1];
               if (Timestamp<>TimestampBoard[NeighborCol,NeighborRow]) and
                  (not IsWallSquare(NeighborCol,NeighborRow)) and
                  (not IsIllegalBoxSquare(NeighborCol,NeighborRow)) then
                  Push(NeighborCol,NeighborRow);
               end;
           end;
      end;

    begin // BoardSatisfiesConstraints
      Result:=True;
      if Timestamp>=High(TimestampBoard[0,0]) then begin // 'True': reset the timestamp board used for calculating connected floors and unfilled squares
         FillChar(TimestampBoard,SizeOf(TimestampBoard),0);
         Timestamp:=0;
         end;
      Inc(Timestamp);

      WallCount:=0; ConnectedFloorsCount:=0;

      for Col:=0 to Pred(BoardWidth) do
          for Row:=0 to Pred(BoardHeight) do
              if IsWallSquare(Col,Row) then
                 Inc(WallCount)
              else begin // a floor square or an unfilled square
                 // all floors and unfilled squares must be connected
                 if Result and
                    (Timestamp<>TimestampBoard[Col,Row]) and // true: an unvisited floor or unfilled square
                    (not IsIllegalBoxSquare(Col,Row)) then // otherwise, it's a special floor square which a box cannot pass. see the templates.
                    if   ConnectedFloorsCount=0 then // true: this is the first unvisited area of floors and unfilled squares
                         // visit and mark the connected floors and unvisited squares
                         Inc(ConnectedFloorsCount,VisitAccessArea(Col,Row))
                    else Result:=False; // found more than one area with floors and unfilled squares

                 // there must be no 3x4 or 4x3 open floor areas
                 if Result then
                    Result:=not Is4x3FloorArea(Col,Row);

                 // a floor must not have more than 2 wall neighbors
                 if Result and
                    IsFloorSquare(Col,Row) and
                    (CalculateWallNeighborCount(Col,Row)>2) then
                    Result:=False;

                 // there must be no Nx2 or 2xN floor areas with only one floor or unfilled square connecting it to the rest of the board (not a constraint from Taylor's thesis)
                 if Result and
                    IsIsolated2xNFloorArea(Col,Row) then
                    Result:=False;
                 end;

      // only 50% of the inner squares may be filled with walls (not a constraint from Taylor's thesis)
      if (WallCount - BorderSquaresCount) > (InnerSquaresCount div 2) then
         Result:=False;
    end;

  begin // TryToFillBoardRegion
    with BoardRegions do begin
      Result:=BoardRegionIndex__>=Count; // 'True': all board regions have been filled
      if not Result then with Items[BoardRegionIndex__] do begin // 'True': board not filled yet
         // first try to fill the selected board region with a template, and if
         // that succeeds, then try to fill the remaining board regions
         FindTemplateCandidates(Pred(Col),Pred(Row),TemplateCandidates); // 'Pred': templates have a one square border with neighbor square constraints
         ShuffleTemplates(TemplateCandidates); // randomize the template candidate order
         for Index:=0 to Pred(TemplateCandidates.Count) do
             if (not Result) and
                (FillBoardRegionCount<MAX_FILL_BOARD_REGION_COUNT) then with TemplateCandidates.Items[Index] do begin
                PutTemplateOnBoard(Template,Transformation,BoardRegionIndex__);
                if   BoardSatisfiesConstraints and
                     TryToFillBoardRegion(Succ(BoardRegionIndex__)) then // 'True': the entire board has been filled with templates
                     Result:=True
                else UndoPutTemplateOnBoard(BoardRegionIndex__); // undo and try the next template, if any. if there are no more template candidates, then backtrack to the previously filled board region and try to fill it with a different template.
                end;
         end;
      end;
  end;
  //        _________
  procedure SaveBoard(var BoardWidth__,BoardHeight__:Integer; var SokobanBoard__:SokFile_.TBoard);
  var Col,Row:Integer;
  begin // returns the generated board to the caller
    BoardWidth__:=BoardWidth;
    BoardHeight__:=BoardHeight;
    SokFile_.ClearBoard(SokobanBoard__);
    for Col:=0 to Pred(BoardWidth) do
        for Row:=0 to Pred(BoardHeight) do
            SokobanBoard__[Succ(Col),Succ(Row)]:=Board[Col,Row];
  end;

begin // CreateBoardPrefilledWithWalls
  Result:=False;
  if InitializeBoard and
     InitializeTemplates then begin
     //RandomSeed__:=33904796;
     if   RandomSeed__>0 then // 'True': user the value from the caller as seed for the random number generator
          InitializeRandomState(RandomSeed__)
     else InitializeRandomState(GetTickCount);
     //ToolsForm.Caption:=IntToStr(RandomState.RandomNumber);
     Timestamp:=High(Timestamp);
     FillBoardRegionCount:=0;
     Result:=TryToFillBoardRegion(0);
     //Clipboard.AsText:=TemplateStatisticsToText;
     if Result then // 'True': the entire board has been filled with templates
        SaveBoard(BoardWidth__,BoardHeight__,Board__);
     end;
end;

end.

