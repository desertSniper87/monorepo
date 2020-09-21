unit GView_;  // Game Viewer

interface

uses Windows,Graphics,
     Misc_,IniFile_,Sound_,BitMap_,Pict_,Sprite_,SokUtil_,SokFile_,Game_;

type
  TPictureType  = (ptScreenBackground,                                // caution: the order cannot change; (a lot of code logic depends on this order)
                   ptBoardBackground,
                   ptReverseModeBackground,
                   ptPlayer,ptPlayerOnGoal,ptBox,ptBoxOnGoal,ptGoal,ptWall,ptFloor,
                   ptPlayerAnimation,ptPlayerOnGoalAnimation,         // highlight object animation, i.e., "stand still" animation
                   ptBoxAnimation,ptBoxOnGoalAnimation,               // highlight object animation, i.e., "stand still" animation
                   ptPlayerMoveAnimation,ptPlayerOnGoalMoveAnimation, // move object animation
                   ptBoxMoveAnimation,ptBoxOnGoalMoveAnimation        // move object animation
                  );

const // don't localize
  DEFAULT_BACKGROUND_ANTI_ALIASING  = aaBilinear;
  DEFAULT_GRID_COLOR_1              = clWhite;
  DEFAULT_GRID_COLOR_2              = clSilver; 
  DEFAULT_MASK_BITMAP_PCT           = 5;
  DEFAULT_OBJECT_ANTI_ALIASING      = aaBilinear;
  DEFAULT_PICTURE_COLOR             : array[TPictureType] of TColor =
    (clNavy,clNavy,clNavy,clYellow,clOlive,clGreen,clLime,clTeal,clGray,clLtGray,clYellow,clOlive,clGreen,clLime,clYellow,clOlive,clGreen,clLime);
  DEFAULT_PICTURE_ANTI_ALIASING     : array[TPictureType] of TAntiAliasing =
    (DEFAULT_BACKGROUND_ANTI_ALIASING,DEFAULT_BACKGROUND_ANTI_ALIASING,DEFAULT_BACKGROUND_ANTI_ALIASING, // ptScreenBackground..ptReverseModeBackground
     aaFilter,aaFilter,aaFilter,aaFilter,                                                                // ptPlayer..ptBoxOnGoal
     DEFAULT_OBJECT_ANTI_ALIASING,DEFAULT_OBJECT_ANTI_ALIASING,DEFAULT_OBJECT_ANTI_ALIASING,             // ptGoal..ptFloor
     aaFilter,aaFilter,aaFilter,aaFilter,aaFilter,aaFilter,aaFilter,aaFilter);                           // ptPlayerAnimation..ptBoxOnGoalAnimation
  DEFAULT_PLAYER_START_POSITION_GRID_COLOR
                                    = clWhite;
  DEFAULT_PLAYER_START_POSITION_GRID_SHADOW_COLOR
                                    = TColor( $404040 );
  DEFAULT_PLAYER_START_POSITION_GRID_SIZE
                                    = 6;
  DEFAULT_SKIN_MASK_BITMAP_PCT      = 7; // note the difference between 'DEFAULT_MASK_BITMAP_PCT' and 'DEFAULT_SKIN_MASK_BITMAP_PCT'
  DEFAULT_SKIN_YELLOW_CRATE_MASK_BITMAP_PCT
                                    = 10;
  DEFAULT_TILE_HEIGHT               = 50;
  DEFAULT_TILE_WIDTH                = DEFAULT_TILE_HEIGHT;
  DEFAULT_SKIN_YELLOW_BOX_LEFT      = 1;
  DEFAULT_SKIN_YELLOW_BOX_TOP       = 52;
  DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_LEFT
                                    = 1;
  DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_TOP
                                    = 103;
  DEFAULT_ZOOM_FACTOR_PCT           = 200;                                    
  PLAYER_SPRITE                     = 0;                              // caution: sprite-constants must not change
  GAME_VIEWER_INI_FILE_SECTION_NAME = 'GameViewer';
  GOAL_SPRITE                       = MAX_BOXES+1;  // Sprites[PLAYER_SPRITE .. Game.BoxCount] contain player and boxes
  WALL_SPRITE                       = GOAL_SPRITE+1;
  FLOOR_SPRITE                      = WALL_SPRITE+1;
  CURSOR_SPRITE                     = FLOOR_SPRITE+1;
  PLAYER_ANIMATION_SPRITE           = CURSOR_SPRITE+1;
  PLAYER_ON_GOAL_ANIMATION_SPRITE   = PLAYER_ANIMATION_SPRITE+1;
  BOX_ANIMATION_SPRITE              = PLAYER_ON_GOAL_ANIMATION_SPRITE+1;
  BOX_ON_GOAL_ANIMATION_SPRITE      = BOX_ANIMATION_SPRITE+1;
  MAX_LEGAL_MOVES_CURSOR_PEN_WIDTH  = 8;
  MIN_PLAYER_START_POSITION_GRID_SIZE
                                    = 2;
  MAX_PLAYER_START_POSITION_GRID_SIZE
                                    = 10;
  MAX_SPRITES                       = BOX_ON_GOAL_ANIMATION_SPRITE;
  MAX_ZOOM_FACTOR_PCT               = 999;
  MIN_ZOOM_FACTOR_PCT               = 100;
  PICTURE_TYPE_NAME                 : array[TPictureType] of String = // don't localize
    ('ScreenBackground','BoardBackground',
     'ReverseModeBackground',
     'Player','PlayerOnGoalSquare','Box','BoxOnGoalSquare','GoalSquare','Wall','Floor',
     'PlayerAnimation','PlayerOnGoalSquareAnimation','BoxAnimation','BoxOnGoalSquareAnimation',
     '','','',''
    );
  PLAYER_AND_BOX_FRAME_COUNT        = 30; // animated players and boxes; must be >= 'WALL_TILE_COUNT'
  WALL_TILE_COUNT                   = 17; // seamless wall elements

type
  TPictures                         = array[TPictureType] of TPict;
  TReverseModePlayerStartPosition   = record
    Visible                         : Boolean;
    GridSize                        : Integer;
    GridColor                       : TRGB;
    GridShadowColor                 : TRGB;
    end;

  TGamePictures = class
  private
    fInitialized:Boolean;
    fMaxZoomFactorPct:Integer;
  protected
    function    GetHasPlayerDirectionAnimation:Boolean;
    function    GetIsASeamlessWall:Boolean;
    procedure   SetMaxZoomFactorPct(MaxZoomFactorPct__:Integer);
  public
    DefaultRect    :array[TPictureType] of TRect;         // default section of the original image
    FrameCount     :array[TPictureType] of Integer;       // frame count for animated items
    GridEnabled    :Boolean;
    GridColor1     :TColor;
    GridColor2     :TColor;
    InBetweenFramesType
                   :array[TPictureType] of TInBetweenFramesType; // animation pictures only
    Pictures       :TPictures;                            // the pictures
    ReverseModePlayerStartPosition
                   :TReverseModePlayerStartPosition;
//  RotatingAnimation
//                 :array[TPictureType] of Boolean;       // animation pictures only
    Transparency   :array[tPictureType] of Boolean;       // board backgrounds only
    TransparencyPct:array[tPictureType] of Integer;       // board backgrounds only
    OuterWallTrimming
                   :TRect;
    UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares
                   :Boolean;                              // if 'True': for goal squares, use "Box on floor" image for highlighting reachables squares instead of the "Box on goal" image
    UseBoxImageForMoveAnimationAlsoForGoalSquares         // if 'True': for goal squares, use "Box on floor" image for move animation instead of the "Box on goal" image
                   :Boolean;
    UseFloorTilesAsBackground // not implemented;         // 'if 'True', use the floor tile as background for the entire screen, properly scaled and aligned with the floors on the board
                   :Boolean;
    UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares
                   :Boolean;                              // if 'True': for goal squares, use "Player on floor" image for highlighting reachables squares instead of the "Player on goal" image
    UsePlayerImageForMoveAnimationAlsoForGoalSquares      // if 'True': for goal squares, use "Player on floor" image for move animation instead of the "Player on goal" image
                   :Boolean;
    WallCap        :TPoint;
    WallType       :TWallType;                            // e.g., 'single wall' or 'seamless wall'

    constructor Create;
    destructor  Destroy; override;

    function    IsFileInUse( const FileName__ : String ) : Boolean;
    function    LoadPictures:Boolean;
    function    LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    function    SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
    function    SaveToFile(const FileName:String):Boolean;
    procedure   SetDefaultValues;
    procedure   SetDefaultWallType;

    property    HasPlayerDirectionAnimation:Boolean read GetHasPlayerDirectionAnimation;
    property    Initialized        :Boolean read fInitialized; // write fInitialized;
    property    IsASeamlessWall:Boolean read GetIsASeamlessWall;
    property    MaxZoomFactorPct:Integer read fMaxZoomFactorPct write SetMaxZoomFactorPct;
  end;

  TLegalMovesCursor
                = record
                         Color                   :TColor;
                         Enabled                 :Boolean; // highlight player/current box?
                         PenWidth                :Integer;
                         ShadowEnabled           :Boolean;
                         ShadowColor             :TColor;
                         Size                    :Integer; // size in pixels;

                  end;

  TLegalMovesInfo
                = record BoxCursor               :TLegalMovesCursor;
                         BoxAnimationEnabled     :Boolean;
                         CursorPos               :TColRow; // cursor position
                         DeadlocksColor          :TColor;
                         DeadlocksEnabled        :Boolean;
                         Enabled                 :Boolean; // show legal moves?
                         ItemIndex               :Integer; // currently selected item; the cursor position matches this item
                         JumpMovesBackgroundColor:TColor;
                         JumpMovesEnabled        :Boolean; // show legal jumps (in reverse mode)?
                         JumpMovesTextColor      :TColor;
                         JumpMovesTextShadowColor:TColor;
                         PlayerCursor            :TLegalMovesCursor;
                         Mask                    :Integer; // type of currently shown squares, if any
                         PlayerAnimationEnabled  :Boolean;
                         Size                    :Integer; // percent of square width
                         StartTime               :TTimeMS;
                         TransparentImage        :Boolean; // transparent?
                         Transparency            :Integer; // percent
                  end;
  TLegalMoveBitMapsType
                = (lmJump,lmPlayer,lmPlayerOnGoal,lmBox,lmBoxOnGoal,lmTemporary); // order must not change
  TSolutionsInfo= record BoxAnimationEnabled     :Boolean;
                         PlayerAnimationEnabled  :Boolean;
                  end;

  TGameViewer   = class
  private
    BackBitMap  :TBitMap; // not in use; 'TopLayerBitMap' must be disabled on order to use 'BackBitMap'
    Board       :TBoard;
    BoardMaskBitMap
                :TBitMap;
    BoxCount    :Integer;
    BoxTargetMaskForDisplay
                :Integer;
    ColCount    :Integer;
    HighlightedSquareCursorColor
                :TColor;
    fBorderWidth:Integer;
    FileName    :String;
    fInitialized:Boolean;
    fWindowResizeCount
                :Cardinal;
    FloorTilesVisible
                :Boolean;
    fWindowRect :TRect; // 'window', i.e., position on owner-image; 'BoardRect' is a part of 'fWindowRect'
    HasPlayerDirectionAnimation
                :Boolean;
    Height      :Integer;
    IsIdle      :Boolean;
    IsASeamlessWall
                :Boolean;
    LegalMoveBitMapsCreated // legal move images are stored in 'LegalMoveBitMaps[lmTemporary,1]'
                :array[0..MAX_BOARD_WIDTH,0..MAX_BOARD_HEIGHT,lmJump..lmBox] of Boolean;
    LegalMoveBitMaps:array[TLegalMoveBitMapsType,0..1] of TBitMap; // 0: bitmap; 1: mask-bitmap
    LegalMoveBitMapsInitialized
                :Boolean;
    PlayerDirectionRect
                :array[TDirection] of TRect;
    ReverseMode :Boolean;
    RowCount    :Integer;
    Sprites     :array[0..MAX_SPRITES] of TSprite1;
    TopLayerBitMap
                :TBitMap;
    UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares,
    UseBoxImageForMoveAnimationAlsoForGoalSquares
                :Boolean;
    UseFloorTilesAsBackground // not implemented;
                :Boolean;
    UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares,
    UsePlayerImageForMoveAnimationAlsoForGoalSquares
                :Boolean;
    Width       :Integer;
    OuterWallTrimming
                :TRect;
    WallCap     :TPoint;

//  procedure   AlphaBlendColor(const Rect__:TRect; Color__:TColor; Pct__:Integer);
    procedure   ClearFPS;
    procedure   DestroyBoardMaskBitMap;
    procedure   DrawCursorSprite(Canvas__:TCanvas; const Rect__:TRect);
    procedure   HideLegalMovesCursor;
    procedure   PutTilesOnTheBoard(SpriteNo,Mask:Integer; Transparent:Boolean; TransparencyPct:Integer; BitMap:TBitMap);
    procedure   MakeSprite(Index:Integer; Pict:TPict; OnDrawSprite__:TDrawSpriteEvent; OnHideSprite__:THideSpriteEvent; FrameCount__:Integer);
    procedure   ShowCursor(Col__,Row__,SizeX__,SizeY__:Integer; Color__:TColor; const LegalMovesCursor__:TLegalMovesCursor);
    procedure   ShowGoal(X,Y:Integer);
    procedure   ShowLegalMovesCursor(ItemIndex__:Integer);
  protected
    function    GetBoxStandStillPictureType(Col__,Row__:Integer):TPictureType;
    function    GetFPS:Integer;
    function    GetIsFloorTileVisible:Boolean;
    function    GetPlayerFrameIndex:Integer;
    function    GetPlayerStandStillPictureType(Col__,Row__:Integer):TPictureType;
    procedure   SetBorderWidth(BorderWidth__:Integer);
    procedure   SetPlayerFrameIndex(FrameIndex__:Integer);
  public // most of the public variables are 'read only' and should really have been implemented as properties
    BoardRect   :TRect;   // Rect(Left,Top,Left+Width,Top+Height);
    Canvas      :TCanvas;
    ColWidth    :Integer;
    FrameCount  :DWORD;
    FrameTime   :TTimeMS;
    Game        :TGame; // this is a reference to a game; the game viewer is not the owner of the game
    Left        :Integer;
    Modified    :Boolean;
    RowHeight   :Integer;
    LegalMovesInfo
                :TLegalMovesInfo;
    Pictures    :TPictures;
    SolutionsInfo
                :TSolutionsInfo;
    Top         :Integer;

    constructor Create;
    destructor  Destroy; override;

    procedure   CellToPos(ACol,ARow:Integer; var X,Y:Integer); // 0-based col,row
    procedure   Clear;
    procedure   HideBox(BoxNo:Integer);
    function    HideLegalMoves:Integer;
    function    IsBoardBackgroundOrReverseModeBackground(Pict:TPict):Boolean;
    function    LoadGame(Game__:TGame):Boolean;
    function    LoadPictures:Boolean;
    function    LoadSettingsFromIniFile(const IniFile:TIniFile; const Section:String):Boolean;
    procedure   MouseToCell(X,Y:Integer; var ACol,ARow:Integer); // 0-based col,row
    procedure   OnIdle;
    procedure   RestoreBackground(const Rect__:TRect);
    function    SaveSettingsToIniFile(const IniFile:TIniFile; const Section:String):Boolean;
    procedure   SetDefaultValues;
    procedure   SetWindow(Canvas__:TCanvas; const WindowRect__:TRect);
    procedure   Show;
    procedure   ShowBackground;
    procedure   ShowBoard(Reset:Boolean);
    function    ShowLegalMoves(Mask__:Integer; CurrentPosition__:TColRow; HighlightedSquareCursorColor__:TColor):Integer;
    procedure   ShowMove(const FromPos,ToPos:TPoint; BoxNo,TimeMS,MoveCount,PushCount:Integer; Undo,Jump,LastMove:Boolean);
    procedure   ShowMoveInstantly(BoxNo:Integer; Undo,Jump,LastMove:Boolean);

    property    BorderWidth:Integer                                           read fBorderWidth        write SetBorderWidth;
    property    BoxStandStillPictureType[Col__,Row__:Integer]:TPictureType    read GetBoxStandStillPictureType;
    property    IsFloorTileVisible:Boolean                                    read GetIsFloorTileVisible;
    property    FPS:Integer                                                   read GetFPS;
    property    Initialized:Boolean                                           read fInitialized;
    property    PlayerFrameIndex:Integer                                      read GetPlayerFrameIndex write SetPlayerFrameIndex;
    property    PlayerStandStillPictureType[Col__,Row__:Integer]:TPictureType read GetPlayerStandStillPictureType;
    property    WindowRect:TRect                                              read fWindowRect;
    property    WindowResizeCount:Cardinal                                    read fWindowResizeCount;
  end;

implementation

uses SysUtils,Classes,Forms,
     Text_,Res_,BitMap2_,Status_,MView_,SokGame_,Main_;

const
  DEFAULT_BORDER_WIDTH              = 0;
  DEFAULT_TILE_COL_BASE             = 1;
  DEFAULT_TILE_NO                   :array[TPictureType,0..1] of TInt8 =
    ((-1,-1),(-1,-1),(-1,-1),
     (0,0),(0,0),(1,1),(1,2),(4,0),(1,3),(3,0),
     (0,0),(0,0),(1,1),(1,2),
     (0,0),(0,0),(1,1),(1,2)
    );
  DEFAULT_TILE_ROW_BASE             = 1;
  MAX_BORDER_WIDTH                  = 24; // arbitrary limit;

procedure DestroyObjectAnimationPictures(var Pictures__:TPictures);
var p,q:TPictureType;
begin // "move object" animations may share images with "stand still" images
  for p:=ptPlayerMoveAnimation to ptBoxOnGoalMoveAnimation do
      if Assigned(Pictures__[p]) then with Pictures__[p] do begin
         for q:=ptPlayer to ptBoxOnGoal do
             if Assigned(Pictures__[q]) then begin
                if BitMap    =Pictures__[q].BitMap     then BitMap:=nil; // the move animation bitmap is shared with, and owned by, a "stand still" picture
                if MaskBitMap=Pictures__[q].MaskBitMap then MaskBitMap:=nil;
                end;
         BitMap.Free; BitMap:=nil; // clear the move animation bitmaps
         MaskBitMap.Free; MaskBitMap:=nil;
         end;
end;

{TGamePictures}

constructor TGamePictures.Create;
var p:TPictureType;
begin
  FillChar(Pictures,SizeOf(Pictures),0); fInitialized:=False;
  try    fInitialized:=True;
         for p:=Low(p) to High(p) do Pictures[p]:=TPict.Create;
         SetDefaultValues;
         if PLAYER_AND_BOX_FRAME_COUNT<WALL_TILE_COUNT then
            raise Exception.Create('Internal error in "TGamePictures.Create"');
  except on E:Exception do fInitialized:=Error(E.Message,'');
  end;
end;

destructor TGamePictures.Destroy;
var p:TPictureType;
begin
  DestroyObjectAnimationPictures(Pictures);
  for p:=Low(p) to High(p) do Pictures[p].Free;
end;

function  TGamePictures.GetHasPlayerDirectionAnimation:Boolean;
var H1,H2,W1,W2:Integer;
begin // returns 'True' if 'player' and 'player on goal' have tiles for all directions
  if   Initialized then begin
       with Pictures[ptGoal  ].OrgBitMap do begin W1:=Width; H1:=Height; end; // use aspect ratio for the goal as measure

       with Pictures[ptPlayer].OrgBitMap do begin W2:=Width; H2:=Height; end;
       Result:=(W2 div (NUMBER_OF_DIRECTIONS - 1)) >= // '-1': give a little slack for rounding errors
               (W1*H2 div H1);

       if Result then begin
          with Pictures[ptPlayerOnGoal].OrgBitMap do begin W2:=Width; H2:=Height; end;
          Result:=(W2 div (NUMBER_OF_DIRECTIONS - 1)) >= // '-1': give a little slack for rounding errors
                  (W1*H2 div H1);
          end;
       end
  else Result:=False;
end;

function  TGamePictures.GetIsASeamlessWall:Boolean;
var H1,H2,W1,W2:Integer;
begin // returns 'True' if wall bitmap width >= height * (walltiles/2), that is, more than half the number of wall-tiles for a seamless wall seem to be present
  if   Initialized then begin
       with Pictures[ptGoal] do
         if Assigned(OrgBitMap) then with OrgBitMap do begin // use aspect ratio for the goal as measure
            W1:=Width; H1:=Height;
            end
         else begin
            W1:=1; H1:=1;
            end;
       with Pictures[ptWall] do
          if Assigned(OrgBitMap) then with OrgBitMap do begin
             W2:=Width; H2:=Height;
             end
          else begin
             W2:=W1; H2:=H1;
             end;
       Result:=(W2 div (WALL_TILE_COUNT div 2)) >=
               (W1*H2 div H1);
       end
  else Result:=False;
end;

function  TGamePictures.IsFileInUse( const FileName__ : String ) : Boolean;
var p : TPictureType;
begin // returns 'True' if the file is assigned to one or more of the picture types
  Result := False;
  for p := Low( Pictures ) to High( Pictures ) do
      if StrEqual( FileName__, Pictures[ p ].FileName ) then begin
         Result := True;
         exit; // 'exit': quick-and-dirty exit when a matching file name has been found
         end;
end;

function  TGamePictures.LoadPictures:Boolean;
var OK:Boolean; oShowErrorMessages:TShowErrorMessages; s:String; p,q:TPictureType; oSourceRect:TRect;
    Visited:array[TPictureType] of Boolean;

(*
  // experimental version of 'MakeAnimationFrames' with support for rotation of the objects
  procedure MakeAnimationFrames(PictureType__:TPictureType);
  var i,j,k,m,n,W,H,W1,H1,W2,H2,
      AngleDegrees,FrameAngleDegrees,NextFrameAngleDegrees,
      Count,Frame,NextFrame:Integer;
      b,oMasked:Boolean; oMaskColor:TRGB;
      pt:TPictureType;
      R0,R1,ObjectBoundsRect:TRect;
      Temp:array[0..5] of TBitMap;
      p:PRGB;
  begin
    FrameCount[PictureType__]:=0;
    with MainForm.GameViewer.LegalMovesInfo do
      if ((PictureType__ in [ptPlayerAnimation,ptPlayerOnGoalAnimation]) and (PlayerAnimationEnabled or MainForm.GameViewer.SolutionsInfo.PlayerAnimationEnabled))
         or
         ((PictureType__ in [ptBoxAnimation   ,ptBoxOnGoalAnimation   ]) and (BoxAnimationEnabled    or MainForm.GameViewer.SolutionsInfo.BoxAnimationEnabled)) then
         with Pictures[PictureType__] do begin
           if (OrgBitMap<>nil) and (OrgBitMap.Width=OrgBitMap.Height) then begin
              // width = height: it's assumed to be a single image;
              // make a 2-image animation by adding the normal image
              R0:=Rect(0,0,OrgBitMap.Width,OrgBitMap.Height);
              pt:=TPictureType(Ord(ptPlayer)+Ord(PictureType__)-Ord(ptPlayerAnimation));
              if (Pictures[pt].OrgBitMap<>nil) and
                 (Pictures[pt].OrgBitMap.Width =OrgBitMap.Width ) and
                 (Pictures[pt].OrgBitMap.Height=OrgBitMap.Height) and
                 BitMapResize(OrgBitMap,2*OrgBitMap.Width,OrgBitMap.Height) then with OrgBitMap.Canvas do begin
                 CopyMode:=cmSrcCopy;
                 CopyRect(Rect(R0.Right,0,OrgBitMap.Width,OrgBitMap.Height),OrgBitMap.Canvas,R0);
                 CopyRect(R0,Pictures[pt].BitMap.Canvas,R0);
                 end;
              end;

           if (OrgBitMap<>nil) and (OrgBitMap.Height>0) then begin
              pt:=TPictureType(Ord(ptPlayer)+Ord(PictureType__)-Ord(ptPlayerAnimation)); // 'p': the normal image
              if   (Pictures[pt].OrgBitMap<>nil) and
                   (Pictures[pt].OrgBitMap.Width<>Pictures[pt].OrgBitMap.Height) and
                   (Pictures[pt].OrgBitMap.Width>0) and
                   (Pictures[pt].OrgBitMap.Height=OrgBitMap.Height) then
                   W:=Pictures[pt].OrgBitMap.Width div Max(1,Self.FrameCount[pt]) // assume that width = normal image width
              else W:=OrgBitMap.Height; // assume that width = height
              H:=OrgBitMap.Height;
              Count:=OrgBitMap.Width div W; // 'Count' = number of frames in the source image
              FillChar(Temp,SizeOf(Temp),0);
              if (Count>0) and (Count<PLAYER_AND_BOX_FRAME_COUNT) and
                 BitMapResize(BitMap,PLAYER_AND_BOX_FRAME_COUNT*W,H) then
                 try
                   // make frames for smooth animation

                   if RotatingAnimation[PictureType__] and
                      (InBetweenFramesType[PictureType__]=ibftInterpolate) then begin
                      ObjectBoundsRect.Left :=W; ObjectBoundsRect.Top   :=H; // initialize the bounding rectangle of the object
                      ObjectBoundsRect.Right:=0; ObjectBoundsRect.Bottom:=0;
                      if   (PictureType__=ptPlayerAnimation) or (PictureType__=ptBoxAnimation) then
                           pt:=ptFloor // if the object isn't masked with a color then it has been drawn on top of a floor
                      else pt:=ptGoal; // if the object isn't masked with a color then it has been drawn on top of a goal
                      if   (not Masked) and
                           Assigned(Pictures[pt]) and
                           Pictures[pt].Visible then
                           if   Assigned(Pictures[pt].OrgBitMap) and
                                Pictures[pt].Resize(W,H) and
                                ResizeFrames(W,H,Count,Pictures[pt].BitMap) then begin
                                //BitMapDump(BitMap);
                                end
                           else Count:=0
                       else begin
                           if not ResizeFrames(W,H,Count,nil) then Count:=0;
                           end;

                      if Assigned(BitMap) and (BitMap.PixelFormat=pf24Bit) and (BitMap.Width=W*Count) then begin
                         for j:=0 to Pred(BitMap.Height) do begin
                             p:=BitMap.ScanLine[j];
                             for i:=0 to Pred(BitMap.Width) do begin
                                 if (p^.r=0) and (p^.g=0) and (p^.b=0) then begin // 'True': the pixel is black, i.e., a background pixel
                                    //p^:=RGB_LIGHT_GRAY; // during rotation it's better to antialias towards a gray background than a black background
                                    end
                                 else begin // find bounding rectangle of the object
                                    k:=i mod W; // relative x co-ordinate for the frame
                                    if      k <ObjectBoundsRect.Left   then ObjectBoundsRect.Left  :=k;
                                    if Succ(k)>ObjectBoundsRect.Right  then ObjectBoundsRect.Right :=Succ(k);
                                    if      j <ObjectBoundsRect.Top    then ObjectBoundsRect.Top   :=j;
                                    if Succ(j)>ObjectBoundsRect.Bottom then ObjectBoundsRect.Bottom:=Succ(j);
                                    end;
                                 Inc(p);
                                 end;
                             end;
                         //BitMapDump(BitMap);

                         W1:=RectWidth(ObjectBoundsRect); H1:=RectHeight(ObjectBoundsRect);
                         if (W1>0) and (H1>0) then begin // 'True': found the object
                            for i:=0 to PLAYER_AND_BOX_FRAME_COUNT-1 do begin // check if the object during rotation exceeds the bounds
                                AngleDegrees:=(i*360) div PLAYER_AND_BOX_FRAME_COUNT;
                                BitMapRotatedImageSize(RectWidth(ObjectBoundsRect),RectHeight(ObjectBoundsRect),AngleDegrees,W2,H2);
                                W1:=Max(W1,W2); H1:=Max(H1,H2); // 'W1' and 'H1' are maximum dimensions after the rotation
                                end;
                            if (W1>W) or (H1>H) then begin // 'True': the rotated object exceeds the bounds; decrease its size
                               W2:=Min((W*RectWidth (ObjectBoundsRect)) div W1,(H*RectWidth (ObjectBoundsRect)) div H1);
                               H2:=Min((W*RectHeight(ObjectBoundsRect)) div W1,(H*RectHeight(ObjectBoundsRect)) div H1);
                               oMasked:=Masked; oMaskColor:=MaskBitMapColor;
                               Masked:=True; MaskBitMapColor:=RGB_BLACK;
                               BitMapSwap(BitMap,OrgBitMap);
                               //BitMapDump(OrgBitMap);
                               if   ResizeFrames(W2,H2,Count,nil) then with OrgBitMap.Canvas do begin
                                    //BitMapDump(BitMap);
                                    Brush.Color:=clBlack; Brush.Style:=bsSolid; Copymode:=cmSrcCopy;
                                    FillRect(Rect(0,0,OrgBitMap.Width,OrgBitMap.Height));
                                    for  i:=0 to Pred(Count) do begin
                                         R1.Left:=i*W+(W-W2) div 2; R1.Right :=R1.Left+W2;
                                         R1.Top :=    (H-H2) div 2; R1.Bottom:=R1.Top +H2;
                                         CopyRect(R1,BitMap.Canvas,CellToRect(i,0,W2,H2));
                                         end;
                                    if   BitMapResize(BitMap,PLAYER_AND_BOX_FRAME_COUNT*W,H) then
                                    else Count:=0;
                                    end
                               else Count:=0;
                               Masked:=oMasked; MaskBitMapColor:=oMaskColor;
                               //BitMapDump(OrgBitMap);
                               end
                            else begin // use the resized bitmap as original bitmap
                               BitMapSwap(BitMap,OrgBitMap);
                               if   BitMapResize(BitMap,PLAYER_AND_BOX_FRAME_COUNT*W,H) then
                               else Count:=0;
                               end;

                            for  i:=Low(Temp) to High(Temp) do
                                 if   BitMapCreate(Temp[i],W,H) then // create temporary bitmaps for rotations
                                 else Count:=0;
                            end
                         else Count:=0;
                         //BitMapDump(OrgBitMap);
                         end
                      else Count:=0;
                      end;

                   R0:=Rect(0,0,W,H); m:=-1; n:=-1;
                   for i:=0 to PLAYER_AND_BOX_FRAME_COUNT-1 do
                       if Count>0 then begin
                          j:=100*i*Count div PLAYER_AND_BOX_FRAME_COUNT;
                          Frame:=j div 100; NextFrame:=Succ(Frame) mod Count; // source frame indices
                          if InBetweenFramesType[PictureType__]=ibftInterpolate then // interpolate frames
                             if RotatingAnimation[PictureType__] then begin
                                AngleDegrees:=(i*360) div PLAYER_AND_BOX_FRAME_COUNT; // target rotation for frame 'i' in the final animation (not 'Frame' which refers to the source frame index)
                                FrameAngleDegrees:=(Frame*360) div Count; // the source frames are supposed to depict a full rotation
                                NextFrameAngleDegrees:=FrameAngleDegrees+(360 div Count);
                                if   Frame<>m then begin // get source frame
                                     m:=Frame;
                                     Temp[1].Canvas.CopyRect(Rect(0,0,W,H),OrgBitMap.Canvas,CellToRect(Frame,0,W,H));
                                     end;
                                if   NextFrame<>n then begin // get next source frame
                                     n:=NextFrame;
                                     Temp[2].Canvas.CopyRect(Rect(0,0,W,H),OrgBitMap.Canvas,CellToRect(NextFrame,0,W,H));
                                     end;
                                // rotate the current source frame and the next one so they have the same angle; then alphablend them (i.e., morph them) to create the final frame
                                if   BitMapRotate(Temp[3],Temp[1],AngleDegrees-    FrameAngleDegrees,clBlack,False) and
                                     BitMapRotate(Temp[4],Temp[2],AngleDegrees-NextFrameAngleDegrees,clBlack,False) then begin
                                     //BitMapDump(Temp3);
                                     //BitMapDump(Temp4);
                                     b:=False;
                                     BitMapAlphaBlend(Temp[0],Temp[3],Temp[4],j mod 100,b); // 'Temp[0]' := morph between 'Temp[3]' and 'Temp[4]'
                                     BitMap.Canvas.CopyMode:=cmSrcCopy;
                                     if   (not Masked) and
                                          Assigned(Pictures[pt]) and
                                          Pictures[pt].Visible then begin
                                          R1:=CellToRect(i,0,W,H);
                                          BitMap.Canvas.CopyRect(R1,Pictures[pt].BitMap.Canvas,Rect(0,0,W,H)); // paint the background
                                          Temp[5].Free;
                                          BitMapCreateMask(Temp[0],Temp[5],RGB_BLACK,MaskBitMapPct); // 'Temp[5]' = mask bitmap for the rotated frame in 'Temp[0]'

                                          // paint the object no top of the backround
                                          if not Assigned(Temp[5]) then // 'True': no mask
                                             BitMap.Canvas.CopyMode:=cmSrcCopy
                                          else begin // 'True': has mask
                                             BitMap.Canvas.CopyMode:=cmSrcAnd;   // and-ing
                                             BitMap.Canvas.CopyRect(R1,Temp[5].Canvas,Rect(0,0,W,H));
                                             BitMap.Canvas.CopyMode:=cmSrcPaint; // or-ing
                                             end;
                                          BitMap.Canvas.CopyRect(R1,Temp[0].Canvas,Rect(0,0,W,H));
                                          BitMap.Canvas.CopyMode:=cmSrcCopy;
                                          end
                                     else begin
                                          BitMap.Canvas.CopyRect(CellToRect(i,0,W,H),Temp[0].Canvas,Rect(0,0,W,H));
                                          end;
                                     end
                                else Count:=0;
                                end
                             else
                                BitMapAlphaBlendRect(
                                  BitMap,OrgBitMap,OrgBitMap,
                                  R0,Rect(Frame*W,0,Succ(Frame)*W,H),Rect(NextFrame*W,0,Succ(NextFrame)*W,H),
                                  j mod 100,
                                  False,clBlack,0)
                          else with BitMap.Canvas do begin // duplicate frames
                             CopyMode:=cmSrcCopy;
                             CopyRect(R0,OrgBitMap.Canvas,Rect(Frame*W,0,Succ(Frame)*W,H));
                             end;
                          R0.Left:=R0.Right; R0.Right:=R0.Left+W; // ready for next frame
                          end;

                   if Count>0 then begin
                      BitMapSwap(BitMap,OrgBitMap);
                      //BitMapDump(OrgBitMap);
                      Count:=PLAYER_AND_BOX_FRAME_COUNT;
                      end;
                 finally
                   for i:=Low(Temp) to High(Temp) do Temp[i].Free;
                 end
              else Count:=0;
              Self.FrameCount[PictureType__]:=Count;
              end;
           end;
  end; // MakeAnimationFrames
*)
  procedure MakeAnimationFrames(PictureType__:TPictureType);
  var i,j,W,H,Count,Frame,NextFrame:Integer; p:TPictureType; R0:TRect;
      MaskColor:TColor;
  begin
    FrameCount[PictureType__]:=0;
    with MainForm.GameViewer.LegalMovesInfo do
      if ((PictureType__ in [ptPlayerAnimation,ptPlayerOnGoalAnimation]) and (PlayerAnimationEnabled or MainForm.GameViewer.SolutionsInfo.PlayerAnimationEnabled))
         or
         ((PictureType__ in [ptBoxAnimation   ,ptBoxOnGoalAnimation   ]) and (BoxAnimationEnabled    or MainForm.GameViewer.SolutionsInfo.BoxAnimationEnabled)) then
         with Pictures[PictureType__] do begin
           if (OrgBitMap<>nil) and (OrgBitMap.Width=OrgBitMap.Height) then begin
              // width = height: it's assumed to be a single image;
              // make a 2-image animation by adding the normal image
              R0:=Rect(0,0,OrgBitMap.Width,OrgBitMap.Height);
              p:=TPictureType(Ord(ptPlayer)+Ord(PictureType__)-Ord(ptPlayerAnimation));
              if (Pictures[p].OrgBitMap<>nil) and
                 (Pictures[p].OrgBitMap.Width =OrgBitMap.Width ) and
                 (Pictures[p].OrgBitMap.Height=OrgBitMap.Height) and
                 BitMapResize(OrgBitMap,2*OrgBitMap.Width,OrgBitMap.Height) then with OrgBitMap.Canvas do begin
                 CopyMode:=cmSrcCopy;
                 CopyRect(Rect(R0.Right,0,OrgBitMap.Width,OrgBitMap.Height),OrgBitMap.Canvas,R0);
                 CopyRect(R0,Pictures[p].BitMap.Canvas,R0);
                 end;
              end;

           if (OrgBitMap<>nil) and (OrgBitMap.Height>0) then begin
              p:=TPictureType(Ord(ptPlayer)+Ord(PictureType__)-Ord(ptPlayerAnimation)); // 'p': the normal image
              if   (Pictures[p].OrgBitMap<>nil) and
                   (Pictures[p].OrgBitMap.Width<>Pictures[p].OrgBitMap.Height) and
                   (Pictures[p].OrgBitMap.Width>0) and
                   (Pictures[p].OrgBitMap.Height=OrgBitMap.Height) then
                   W:=Pictures[p].OrgBitMap.Width div Max(1,Self.FrameCount[p]) // assume that width = normal image width
              else W:=OrgBitMap.Height; // assume that width = height
              H:=OrgBitMap.Height;
              Count:=OrgBitMap.Width div W;
              if (Count>0) and (Count<=PLAYER_AND_BOX_FRAME_COUNT) and
                 BitMapResize(BitMap,PLAYER_AND_BOX_FRAME_COUNT*W,H) then begin
                 if   Masked {or (MaskBitMap<>nil)} then MaskColor:=RGBToColor(MaskBitMapColor)
                 else MaskColor:=clBlack;
                 // make frames for smooth animation
                 R0:=Rect(0,0,W,H);
                 for i:=0 to PLAYER_AND_BOX_FRAME_COUNT-1 do begin
                     j:=100*i*Count div PLAYER_AND_BOX_FRAME_COUNT;
                     Frame:=j div 100; NextFrame:=Succ(Frame) mod Count;
                     if (InBetweenFramesType[PictureType__]=ibftInterpolate) and (Count<PLAYER_AND_BOX_FRAME_COUNT) then begin // interpolate frames
                        BitMapAlphaBlendRect(
                          BitMap,OrgBitMap,OrgBitMap,
                          R0,Rect(Frame*W,0,Succ(Frame)*W,H),Rect(NextFrame*W,0,Succ(NextFrame)*W,H),
                          j mod 100,
                          Masked,MaskColor,MaskBitMapPct);
                        end
                     else with BitMap.Canvas do begin // duplicate frames
                        CopyMode:=cmSrcCopy;
                        CopyRect(R0,OrgBitMap.Canvas,Rect(Frame*W,0,Succ(Frame)*W,H));
                        end;
                     R0.Left:=R0.Right; R0.Right:=R0.Left+W; // ready for next frame
                     end;
                 BitMapSwap(BitMap,OrgBitMap);
                 //BitMapDump(OrgBitMap);
                 Count:=PLAYER_AND_BOX_FRAME_COUNT;
{
                 if (PictureType__=ptBoxAnimation) and BitMapResize(BitMap,W,H) then
                    for Frame:=0 to Pred(Count) do begin
                        BitMap.Canvas.CopyRect(Rect(0,0,W,H),OrgBitMap.Canvas,Rect(Frame*W,0,Succ(Frame)*W,H));
                        BitMapDump(BitMap);
                        end;
}
                 end
              else Count:=0;
              Self.FrameCount[PictureType__]:=Count;
              end;
           end;
  end; // MakeAnimationFrames

  procedure DrawGrid(PictureType:TPictureType);
  var i,W,H,Count:Integer;
      CornerColor:TColor;

    procedure DrawGridRectangle(BitMap:TBitMap; const Rect:TRect; Sides,Corners:TDirectionSet);
    begin
      with BitMap do with Canvas do begin
        Brush.Style   := bsSolid;
        Pen.Mode      := PmCopy;
        Pen.Style     := psSolid;
        Pen.Width     := 1;
        Pen.Color     := GridColor1;
        if Up in Sides then with Rect do begin
           MoveTo(Left,Top); LineTo(Right,Top);
           Pixels[Pred(Right),Top]:=CornerColor;
           end;
        if Left in Sides then with Rect do begin
           MoveTo(Left,Top); LineTo(Left,Bottom);
           Pixels[Left,Pred(Bottom)]:=CornerColor;
           end;
        Pen.Color     :=GridColor2;
        if Down in Sides then with Rect do begin
           MoveTo(Left,Pred(Bottom)); LineTo(Right,Pred(Bottom));
           Pixels[Left,Pred(Bottom)]:=CornerColor;
           end;
        if Right in Sides then with Rect do begin
           MoveTo(Pred(Right),Top); LineTo(Pred(Right),Bottom);
           Pixels[Pred(Right),Top]:=CornerColor;
           end;
        if Up in Corners then with Rect do begin    // top-left
           Pixels[Left,Top]:=GridColor1;
           end;
        if Left in Corners then with Rect do begin  // bottom-left
           Pixels[Left,Pred(Bottom)]:=CornerColor;
           end;
        if Down in Corners then with Rect do begin  // bottom-right
           Pixels[Pred(Right),Pred(Bottom)]:=GridColor2;
           end;
        if Right in Corners then with Rect do begin // top-right
           Pixels[Pred(Right),Top]:=CornerColor;
           end;
        end;
    end;

  begin
    with Pictures[PictureType] do begin
      Count:=Max(1,Self.FrameCount[PictureType]);
      W:=OrgBitMap.Width div Count;
      H:=OrgBitMap.Height;
      CornerColor:=RGBToColor(RGBInterpolate(ColorToRGB(GridColor1),ColorToRGB(GridColor2),50));
      if (PictureType<>ptWall) or (not IsASeamlessWall) then
         for i:=0 to Pred(Count) do
             DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[Up,Left,Down,Right],[])
      else
         for i:=0 to Pred(Count) do
             case i of
                  00: // no wall neighbours, i.e., a single wall square
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[Up,Left,Down,Right],[]);
                  01: // wall above
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[   Left,Down,Right],[]);
                  02: // wall to the right
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[Up,Left,Down      ],[]);
                  03: // wall above and to the right
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[   Left,Down      ],[             Right]);
                  04: // wall below
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[Up,Left,     Right],[]);
                  05: // wall above and below
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[   Left,     Right],[]);
                  06: // wall below and to the right
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[Up,Left           ],[        Down      ]);
                  07: // wall above, below, and to the right
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[   Left           ],[        Down,Right]);
                  08: // wall to the left
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[Up,     Down,Right],[]);
                  09: // wall above and to the left
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[        Down,Right],[Up                ]);
                  10: // wall to the left and to the right
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[Up,     Down      ],[]);
                  11: // wall above, to the left, and to the right
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[        Down      ],[Up,          Right]);
                  12: // wall below and to the left
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[Up,          Right],[   Left           ]);
                  13: // wall above, below, and to the left
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[             Right],[Up,Left           ]);
                  14: // wall below, to the left, and to the right
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[Up                ],[   Left,Down      ]);
                  15: // walls on all 4 sides
                  DrawGridRectangle(OrgBitMap,CellToRect(i,0, W,H),[                  ],[Up,Left,Down,Right]);
                  16:; // wall cap
         end;
      end;
  end;

begin // LoadPictures
  Result:=True; fInitialized:=False;
  try
    oShowErrorMessages:=ShowErrorMessages;
    try
      ShowErrorMessages:=semNew; LastErrorText:='';
      DestroyObjectAnimationPictures(Pictures);

      SetDefaultDirectory;

      for p:=ptScreenBackground to ptReverseModeBackground do with Pictures[p] do begin
          OK:=False;

          if (FileName<>'') and (FileName<>DEFAULT_VALUE) then begin
             OK:=FileExists(FileName);
             if not OK then begin
                s:=FileName; OK:=FileHasMoved(s);
                if OK then FileName:=s;
                end;
             OK:=OK and LoadFromFile(FileName);
             end;

          if (not OK) and (FileName<>'') then begin
              if FileName<>DEFAULT_VALUE then begin
                 FileName:=DEFAULT_VALUE; // change back to default setting
                 SourceRect:=DefaultRect[p];
                 View:=ivFill;
                 end;

              if      p=ptScreenBackground then
                      OK:=LoadFromResource(BACKGROUND_RES_NAME,RC_JPG)
              else if p=ptBoardBackground then
                      OK:=LoadFromResource(BOARD_BACKGROUND_RES_NAME,RC_JPG)
              else if p=ptReverseModeBackground then
                      OK:=LoadFromBitMap  (MainForm.ReverseModeBackgroundImage.Picture.BitMap);
              FileName:=DEFAULT_VALUE; // 'LoadFromResource' modified 'FileName'

              if OK and (RectWidth(SourceRect)*RectHeight(SourceRect)<>0) then begin
                 OK:=BitMapExtractRect(BitMap,SourceRect);
                 if OrgBitMap=nil then OrgBitMap:=TBitMap.Create;
                 if OK and (OrgBitMap<>nil) then OrgBitMap.Assign(BitMap);
                 end;
              end;

           if OK then begin
              if Pict<>nil then begin // make a bitmap version of the image, since 'Canvas.Draw' only uses 'CopyMode' for bitmaps
                 MakeOrgBitMapFromPict;
                 if OrgBitMap<>nil then begin
                    Pict.Free; Pict:=nil; // 'Pict' isn't used anymore
                    end;
                 end;
              end
           else begin
              MakeBlank(RectWidth(SourceRect),RectHeight(SourceRect),Color);
              if FileName<>'' then begin Result:=False; FileName:=''; end;
              end;
           end;

      FillChar(Visited,SizeOf(Visited),0);
      for p:=ptPlayer to ptBoxOnGoalAnimation do
          if not Visited[p] then with Pictures[p] do begin
             oSourceRect:=SourceRect;
             try     FillChar(SourceRect,SizeOf(SourceRect),0); // avoid that 'LoadFromFile' extracts a section
                     MakeBitMap; // necessary to have a fresh bitmap in special cases

                     if        FileName=DEFAULT_VALUE then begin
                               if   IdenticalRects(oSourceRect,DefaultRect[p]) then begin
                                    if p=ptWall then begin
                                       if (WallType<>wtTiledWall) and
                                          LoadFromCompressedImage(Addr(DefaultWallCompressedImage),SizeOf(DefaultWallCompressedImage),nil,Rect(0,0,0,0)) then begin
                                          Visited[ptWall]:=True;
                                          FillChar(OuterWallTrimming,SizeOf(OuterWallTrimming),0);
                                          WallCap.X                  :=-1; // '-1': default offset = midways between the squares
                                          WallCap.Y                  :=-1; // '-1': default offset = midways between the squares
                                          MaskBitMapColor            :=RGB_BLACK;
                                          MaskBitMapPct              :=DEFAULT_MASK_BITMAP_PCT;
                                          Masked                     :=True;
                                          Antialiasing               :=DEFAULT_PICTURE_ANTI_ALIASING[ptWall];
                                          end;
                                       end
                                    else if (p=ptPlayerAnimation) or (p=ptPlayerOnGoalAnimation) then begin
                                            if (MainForm.GameViewer.LegalMovesInfo.PlayerAnimationEnabled or MainForm.GameViewer.SolutionsInfo.PlayerAnimationEnabled) and
                                               LoadFromCompressedImage(Addr(DefaultPlayerAnimationCompressedImage),SizeOf(DefaultPlayerAnimationCompressedImage),MainForm.DefaultTileSetImage.Picture.BitMap,DefaultRect[p]) then begin
                                               Visited[p]:=True;
                                               if MaskBitMapPct=DEFAULT_MASK_BITMAP_PCT then
                                                  MaskBitMapPct:=DEFAULT_SKIN_MASK_BITMAP_PCT;
                                               if (not Visited[ptPlayerOnGoalAnimation]) and
                                                  IdenticalRects(Pictures[ptPlayerOnGoalAnimation].SourceRect,DefaultRect[p]) then begin
                                                  Visited[ptPlayerOnGoalAnimation]:=Pictures[ptPlayerOnGoalAnimation].LoadFromBitMap(Pictures[p].OrgBitMap);
                                                  if Visited[ptPlayerOnGoalAnimation] and (Pictures[ptPlayerOnGoalAnimation].MaskBitMapPct=DEFAULT_MASK_BITMAP_PCT) then
                                                     Pictures[ptPlayerOnGoalAnimation].MaskBitMapPct:=DEFAULT_SKIN_MASK_BITMAP_PCT;
                                                  end;
                                               end;
                                            end
                                    else if p=ptBoxAnimation then begin
                                            if (MainForm.GameViewer.LegalMovesInfo.BoxAnimationEnabled or MainForm.GameViewer.SolutionsInfo.BoxAnimationEnabled) and
                                               LoadFromCompressedImage(Addr(DefaultRedBoxAnimationCompressedImage),SizeOf(DefaultRedBoxAnimationCompressedImage),MainForm.DefaultTileSetImage.Picture.BitMap,DefaultRect[p]) then begin
                                               Visited[p]:=True;
                                               if MaskBitMapPct=DEFAULT_MASK_BITMAP_PCT then
                                                  MaskBitMapPct:=DEFAULT_SKIN_MASK_BITMAP_PCT;
                                               end;
                                            end
                                    else if p=ptBoxOnGoalAnimation then begin
                                           if (MainForm.GameViewer.LegalMovesInfo.BoxAnimationEnabled or MainForm.GameViewer.SolutionsInfo.BoxAnimationEnabled) and
                                              LoadFromCompressedImage(Addr(DefaultRedBoxOnGoalAnimationCompressedImage),SizeOf(DefaultRedBoxOnGoalAnimationCompressedImage),MainForm.DefaultTileSetImage.Picture.BitMap,DefaultRect[p]) then begin
                                               Visited[p]:=True;
                                               if MaskBitMapPct=DEFAULT_MASK_BITMAP_PCT then
                                                  MaskBitMapPct:=DEFAULT_SKIN_MASK_BITMAP_PCT;
                                               end;
                                            end
                                    else if (p in [ptPlayer,ptPlayerOnGoal,ptBox,ptBoxOnGoal]) and (MaskBitMapPct=DEFAULT_MASK_BITMAP_PCT) then
                                            MaskBitMapPct:=DEFAULT_SKIN_MASK_BITMAP_PCT;
                                    end
                               else if (MainForm.GameViewer.LegalMovesInfo.BoxAnimationEnabled or MainForm.GameViewer.SolutionsInfo.BoxAnimationEnabled) then begin
                                       if      (p=ptBoxAnimation) and
                                               IdenticalRects(oSourceRect,Rect(DEFAULT_SKIN_YELLOW_BOX_LEFT,DEFAULT_SKIN_YELLOW_BOX_TOP,DEFAULT_SKIN_YELLOW_BOX_LEFT+DEFAULT_TILE_WIDTH,DEFAULT_SKIN_YELLOW_BOX_TOP+DEFAULT_TILE_WIDTH)) and
                                               LoadFromCompressedImage(Addr(DefaultYellowBoxAnimationCompressedImage),SizeOf(DefaultYellowBoxAnimationCompressedImage),MainForm.DefaultTileSetImage.Picture.BitMap,Rect(DEFAULT_SKIN_YELLOW_BOX_LEFT,DEFAULT_SKIN_YELLOW_BOX_TOP,DEFAULT_SKIN_YELLOW_BOX_LEFT+DEFAULT_TILE_WIDTH,DEFAULT_SKIN_YELLOW_BOX_TOP+DEFAULT_TILE_WIDTH)) then begin
                                               Visited[p]:=True;
                                               if MaskBitMapPct=DEFAULT_MASK_BITMAP_PCT then
                                                  MaskBitMapPct:=DEFAULT_SKIN_MASK_BITMAP_PCT;
                                               end
                                       else if (p=ptBoxOnGoalAnimation) and
                                               IdenticalRects(oSourceRect,Rect(DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_LEFT,DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_TOP,DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_LEFT+DEFAULT_TILE_WIDTH,DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_TOP+DEFAULT_TILE_WIDTH)) and
                                               LoadFromCompressedImage(Addr(DefaultYellowBoxOnGoalAnimationCompressedImage),SizeOf(DefaultYellowBoxOnGoalAnimationCompressedImage),MainForm.DefaultTileSetImage.Picture.BitMap,Rect(DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_LEFT,DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_TOP,DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_LEFT+DEFAULT_TILE_WIDTH,DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_TOP+DEFAULT_TILE_WIDTH)) then begin
                                               Visited[p]:=True;
                                               if MaskBitMapPct=DEFAULT_MASK_BITMAP_PCT then
                                                  MaskBitMapPct:=DEFAULT_SKIN_MASK_BITMAP_PCT;
                                               end
                                       else if (p in [ptPlayer,ptPlayerOnGoal,ptBox,ptBoxOnGoal]) and
                                               (MaskBitMapPct=DEFAULT_MASK_BITMAP_PCT) and
                                               (IdenticalRects(oSourceRect,Rect(DEFAULT_SKIN_YELLOW_BOX_LEFT,DEFAULT_SKIN_YELLOW_BOX_TOP,DEFAULT_SKIN_YELLOW_BOX_LEFT+DEFAULT_TILE_WIDTH,DEFAULT_SKIN_YELLOW_BOX_TOP+DEFAULT_TILE_WIDTH))
                                                or
                                                IdenticalRects(oSourceRect,Rect(DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_LEFT,DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_TOP,DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_LEFT+DEFAULT_TILE_WIDTH,DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_TOP+DEFAULT_TILE_WIDTH))
                                               ) then
                                               MaskBitMapPct:=DEFAULT_SKIN_MASK_BITMAP_PCT;

                                       end;
                               if not Visited[p] then
                                  Visited[p]:=LoadFromBitMapRect(MainForm.DefaultTileSetImage.Picture.BitMap,oSourceRect,Point(0,0),clBlack);
                               end
                     else if   (FileName<>'')
                               and
                               LoadFromFile(FileName)
                               and
                               ((Pict=nil)             // simple bitmap
                                or
                                MakeOrgBitMapFromPict) // the image is able to draw itself
                               and
                               (OrgBitMap<>nil) then begin // has an original bitmap

                               for q:=ptPlayer to ptBoxOnGoalAnimation do // load other tiles from the same file
                                   if (not Visited[p]) and (q<>p) and
                                      (AnsiCompareText(Pictures[q].FileName,Pictures[p].FileName)=0) then
                                      if Pictures[q].LoadFromBitMapRect(Pictures[p].OrgBitMap,Pictures[q].SourceRect,Point(0,0),clBlack) then begin
                                         Visited[q]:=True;
                                         end;

                               Visited[p]:=LoadFromBitMapRect(OrgBitMap,oSourceRect,Point(0,0),clBlack);
                               end;

                     if not Visited[p] then begin
                        Visited[p]:=True;
                        MakeBlank(RectWidth(oSourceRect),RectHeight(oSourceRect),Color);
                        end;

             finally SourceRect:=oSourceRect;
             end;
             end;

      for p:=Low(Pictures) to High(Pictures) do with Pictures[p] do begin
          if Pict<>nil then MakeOrgBitMapFromPict;
          Pict.Free; Pict:=nil; // always use 'OrgBitMap' instead of original image
          end;

      for p:=ptPlayer to High(p) do with Pictures[p] do // ensure that all game tiles have an original bitmap, otherwise sprites will crash
          if (OrgBitMap=nil) or (OrgBitMap.Width=0) or (OrgBitMap.Height=0) then
             if not BitMapResize(OrgBitMap,2,2) then
                raise Exception.Create(TEXT_TASK_FAILED);

      FillChar(FrameCount,SizeOf(FrameCount),0);
      fInitialized:=True; // set this before calling 'HasPlayerDirectionAnimation' and 'IsASeamlessWall'

      if IsASeamlessWall then
         FrameCount[ptWall]:=WALL_TILE_COUNT
      else begin
         WallType:=wtTiledWall; FrameCount[ptWall]:=1;
         end;

      if HasPlayerDirectionAnimation then begin
         FrameCount[ptPlayer]:=NUMBER_OF_DIRECTIONS;
         FrameCount[ptPlayerOnGoal]:=NUMBER_OF_DIRECTIONS;

         with Pictures[ptPlayer].OrgBitMap do
           Pictures[ptFloor].ResizeOriginal(Width div FrameCount[ptPlayer      ],Height,aaNone);

         with Pictures[ptPlayerOnGoal].OrgBitMap do
           Pictures[ptGoal ].ResizeOriginal(Width div FrameCount[ptPlayerOnGoal],Height,aaNone);
         end
      else begin
        FrameCount[ptPlayer]:=1; FrameCount[ptPlayerOnGoal]:=1;
        end;
      FrameCount[ptPlayerMoveAnimation      ]:=FrameCount[ptPlayer];
      FrameCount[ptPlayerOnGoalMoveAnimation]:=FrameCount[ptPlayerOnGoal];

      for p:=ptPlayerAnimation to ptBoxOnGoalAnimation do MakeAnimationFrames(p);

      if  Pictures[ptFloor].Visible and
          (not Pictures[ptGoal].Masked) then with Pictures[ptGoal].OrgBitMap do //'ptGoal':  uses the goal tile as normalization measure stick for all the tiles
          if Pictures[ptFloor ].ResizeOriginal(Width,Height,aaNone) then begin
             if ((Pictures[ptPlayer].OrgBitMap.Width <>Width*FrameCount[ptPlayer])
                 or
                 (Pictures[ptPlayer].OrgBitMap.Height<>Height)
                )
                and
                (not Pictures[ptPlayer].Masked) // 'True': the player isn't masked with the background color but with the floor image, hence, the image sizes must match
                then
                if FrameCount[ptPlayer]=1 then
                   Pictures[ptPlayer].ResizeOriginal(Width,Height,aaNone)
                else if Pictures[ptPlayer].ResizeFrames(Width,Height,FrameCount[ptPlayer],nil) then
                        // 'ResizeFrames' will have to do,
                        // but it's bound to cause masking artifacts
                        // because it resizes with current 'Antialiasing' setting
                        // as opposed to the floor - and maybe also the player-on-goal;
                        // they resize using 'aaNone' for antialising;
                        // the bottom line is that the user should use a common size for all tiles;
                        with Pictures[ptPlayer] do
                          if (BitMap<>nil) and (OrgBitMap<>nil) then
                             BitMapSwap(BitMap,OrgBitMap);
             Pictures[ptBox].ResizeOriginal(Width,Height,aaNone);
             end;

      with Pictures[ptGoal] do with OrgBitMap do
           if not Pictures[ptBoxOnGoal].Masked then // 'True': the box-on-goal isn't masked with the background color but with the goal image, hence, the image sizes must match
              Pictures[ptBoxOnGoal].ResizeOriginal(Width,Height,AntiAliasing);

      if GridEnabled and Pictures[ptFloor].Visible then begin
         for p:=ptPlayer to ptBoxOnGoalMoveAnimation do with Pictures[p] do
             if Assigned(OrgBitMap) then
                 DrawGrid(p);
         end;

      //for p:=Low(Pictures) to High(Pictures) do with Pictures[p] do begin
      //    BitMapDump(OrgBitMap);
      //    end;

    finally ShowErrorMessages:=oShowErrorMessages;
    end;
  except on E:Exception do fInitialized:=Error(E.Message,'');
  end;

  Result:=Result and fInitialized;
end;

function  TGamePictures.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
var i:Integer; p:TPictureType;
begin
  Result:=True;

  for p:=Low(p) to {High(p)} ptBoxOnGoalAnimation do with Pictures[p] do begin
      if p<ptPlayerAnimation then
         Color             :=TColor(IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'Color',Color));
      FileName             :=KeepDataPathUpToDate(IniFile.ReadString (PICTURE_TYPE_NAME[p],'FileName',FileName));
      i                    :=IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'Antialiasing',Ord(AntiAliasing));
      if (i>=Ord(Low(Misc_.TAntiAliasing))) and (i<=Ord(High(Misc_.TAntiAliasing))) then
         Antialiasing      :=Misc_.TAntiAliasing(i);
      MaskBitMapColor      :=ColorToRGB(IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'BackgroundColor',Integer(RGBToColor(MaskBitMapColor))));
      MaskBitMapPct        :=Max(0,Min(100,IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'BackgroundColorTolerance',MaskBitMapPct)));
      if p<>ptScreenBackground then
         Masked            :=IniFile.ReadBool   (PICTURE_TYPE_NAME[p],'Masked'  ,Masked);
      if (p>=ptPlayer) and (p<=ptBoxOnGoalAnimation) then
         MaskExteriorOnly  :=IniFile.ReadBool   (PICTURE_TYPE_NAME[p],'MaskExteriorPixelsOnly',MaskExteriorOnly);
      SourceRect.Left      :=IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'Left'    ,SourceRect.Left);
      SourceRect.Top       :=IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'Top'     ,SourceRect.Top);
      SourceRect.Right     :=IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'Right'   ,SourceRect.Right);
      SourceRect.Bottom    :=IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'Bottom'  ,SourceRect.Bottom);
      i                    :=IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'View'    ,Ord(View));
      if (i>=Ord(Low(TImageView))) and (i<=Ord(High(TImageView))) then begin
         View              :=TImageView(i);
         if (View          = ivFloorTile) and (p <> ptScreenBackground) then
            View           := ivTile;
         end;
      if (p<=ptReverseModeBackground) or (p=ptFloor) then
         Visible           :=IniFile.ReadBool   (PICTURE_TYPE_NAME[p],'Visible' ,Visible);
      if ((p>=ptBoardBackground) and (p<=ptReverseModeBackground)) or (p=ptFloor) or (p=ptGoal) then begin
         Transparency[p]   :=IniFile.ReadBool   (PICTURE_TYPE_NAME[p],'Transparency' ,Transparency[p]);
         TransparencyPct[p]:=Max(0,Min(100,IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'TransparencyPct' ,TransparencyPct[p])));
         if p = ptReverseModeBackground then begin
            ReverseModePlayerStartPosition.Visible := IniFile.ReadBool (PICTURE_TYPE_NAME[p],'Reverse mode player start position - Visible', ReverseModePlayerStartPosition.Visible);
            ReverseModePlayerStartPosition.GridSize := Max( MIN_PLAYER_START_POSITION_GRID_SIZE, Min( MAX_PLAYER_START_POSITION_GRID_SIZE, IniFile.ReadInteger(PICTURE_TYPE_NAME[p], 'Reverse mode player start position - Gridsize', ReverseModePlayerStartPosition.GridSize) ) );
            ReverseModePlayerStartPosition.GridColor := ColorToRGB( IniFile.ReadInteger( PICTURE_TYPE_NAME[p], 'Reverse mode player start position - GridColor', Integer( RGBToColor( ReverseModePlayerStartPosition.GridColor ) ) ) );
            ReverseModePlayerStartPosition.GridShadowColor := ColorToRGB( IniFile.ReadInteger( PICTURE_TYPE_NAME[p], 'Reverse mode player start position - GridShadowColor', Integer( RGBToColor( ReverseModePlayerStartPosition.GridShadowColor ) ) ) );
            end;
         end;
      if p=ptPlayerOnGoal then begin
         UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares:=
                             IniFile.ReadBool   (PICTURE_TYPE_NAME[p],'Use "Player" image for highlighting reachable squares',UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares);
         UsePlayerImageForMoveAnimationAlsoForGoalSquares:=
                             IniFile.ReadBool   (PICTURE_TYPE_NAME[p],'Use "Player" image for move animation',UsePlayerImageForMoveAnimationAlsoForGoalSquares);
         end;
      if p=ptBoxOnGoal then begin
         UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares:=
                             IniFile.ReadBool   (PICTURE_TYPE_NAME[p],'Use "Box" image for highlighting reachable squares',UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares);
         UseBoxImageForMoveAnimationAlsoForGoalSquares:=
                             IniFile.ReadBool   (PICTURE_TYPE_NAME[p],'Use "Box" image for move animation',UseBoxImageForMoveAnimationAlsoForGoalSquares);
         end;
      if p=ptWall then begin
         i:=IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'WallType' ,Ord(WallType));
         if (i>=Ord(Low(WallType))) and (i<=Ord(High(WallType))) then
            WallType:=TWallType(i);
         OuterWallTrimming.Left  :=Max( 0,Min(100,IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'WallTrimmingLeft'  ,OuterWallTrimming.Left)));
         OuterWallTrimming.Top   :=Max( 0,Min(100,IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'WallTrimmingTop'   ,OuterWallTrimming.Top)));
         OuterWallTrimming.Right :=Max( 0,Min(100,IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'WallTrimmingRight' ,OuterWallTrimming.Right)));
         OuterWallTrimming.Bottom:=Max( 0,Min(100,IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'WallTrimmingBottom',OuterWallTrimming.Bottom)));
         WallCap       .X        :=Max(-1,Min(100,IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'WallTop.Left'      ,WallCap.X))); // for backwards compatibility;
         WallCap       .Y        :=Max(-1,Min(100,IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'WallTop.Top'       ,WallCap.Y))); // wall caps changed names from 'WallTop' to 'WallCap' in version 1.447
         WallCap       .X        :=Max(-1,Min(100,IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'WallCap.Left'      ,WallCap.X)));
         WallCap       .Y        :=Max(-1,Min(100,IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'WallCap.Top'       ,WallCap.Y)));
         end;
//    if p=ptFloor then
//       UseFloorTilesAsBackground:= IniFile.ReadBool(PICTURE_TYPE_NAME[p],'Use "Floor" image as screen background',UseFloorTilesAsBackground);
      if (p=ptPlayerAnimation) or (p=ptPlayerOnGoalAnimation) or
         (p=ptBoxAnimation   ) or (p=ptBoxOnGoalAnimation   ) then begin
         i                       :=IniFile.ReadInteger(PICTURE_TYPE_NAME[p],'InBetweenFramesType',Ord(InBetweenFramesType[p]));
         if (i>=Ord(Low(Misc_.TInBetweenFramesType))) and (i<=Ord(High(Misc_.TInBetweenFramesType))) then
            InBetweenFramesType[p]:=Misc_.TInBetweenFramesType(i);
         end;
      end;
   MaxZoomFactorPct:=IniFile.ReadInteger(GAME_VIEWER_INI_FILE_SECTION_NAME,'MaxZoomFactorPct',MaxZoomFactorPct);
   GridEnabled:=IniFile.ReadBool(GAME_VIEWER_INI_FILE_SECTION_NAME,'GridEnabled',GridEnabled);
   GridColor1:=TColor(IniFile.ReadInteger(GAME_VIEWER_INI_FILE_SECTION_NAME,'GridColor1',Integer(GridColor1)));
   GridColor2:=TColor(IniFile.ReadInteger(GAME_VIEWER_INI_FILE_SECTION_NAME,'GridColor2',Integer(GridColor2)));
end;

function  TGamePictures.SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
var p:TPictureType;
begin
  Result:=True;
  for p:=Low(p) to {High(p)} ptBoxOnGoalAnimation do with Pictures[p] do begin
      if p<ptPlayerAnimation then
         IniFile.WriteInteger(PICTURE_TYPE_NAME[p],'Color'                   ,Integer(Color));
      IniFile   .WriteString (PICTURE_TYPE_NAME[p],'FileName'                ,FileName);
      IniFile   .WriteInteger(PICTURE_TYPE_NAME[p],'Antialiasing'            ,Ord(AntiAliasing));
      IniFile   .WriteInteger(PICTURE_TYPE_NAME[p],'BackgroundColor'         ,Integer(RGBToColor(MaskBitMapColor)));
      IniFile   .WriteInteger(PICTURE_TYPE_NAME[p],'BackgroundColorTolerance',MaskBitMapPct);
      if p<>ptScreenBackground then
         IniFile.WriteBool   (PICTURE_TYPE_NAME[p],'Masked'                  ,Masked);
      if (p>=ptPlayer) and (p<=ptBoxOnGoalAnimation) then
         IniFile.WriteBool   (PICTURE_TYPE_NAME[p],'MaskExteriorPixelsOnly'  ,MaskExteriorOnly);
      IniFile   .WriteInteger(PICTURE_TYPE_NAME[p],'Left'                    ,SourceRect.Left);
      IniFile   .WriteInteger(PICTURE_TYPE_NAME[p],'Top'                     ,SourceRect.Top);
      IniFile   .WriteInteger(PICTURE_TYPE_NAME[p],'Right'                   ,SourceRect.Right);
      IniFile   .WriteInteger(PICTURE_TYPE_NAME[p],'Bottom'                  ,SourceRect.Bottom);
      IniFile   .WriteInteger(PICTURE_TYPE_NAME[p],'View'                    ,Integer(View));
      if (p<=ptReverseModeBackground) or (p=ptFloor) then
         IniFile.WriteBool   (PICTURE_TYPE_NAME[p],'Visible'                 ,Visible);
      if ((p>=ptBoardBackground) and (p<=ptReverseModeBackground)) or (p=ptFloor) or (p=ptGoal) then begin
         IniFile.WriteBool   (PICTURE_TYPE_NAME[p],'Transparency'            ,Transparency[p]);
         IniFile.WriteInteger(PICTURE_TYPE_NAME[p],'TransparencyPct'         ,TransparencyPct[p]);
         if p = ptReverseModeBackground then begin
            IniFile.WriteBool (PICTURE_TYPE_NAME[p],'Reverse mode player start position - Visible',ReverseModePlayerStartPosition.Visible);
            IniFile.WriteInteger(PICTURE_TYPE_NAME[p], 'Reverse mode player start position - Gridsize', ReverseModePlayerStartPosition.GridSize);
            IniFile.WriteInteger( PICTURE_TYPE_NAME[p], 'Reverse mode player start position - GridColor', Integer( RGBToColor( ReverseModePlayerStartPosition.GridColor ) ) );
            IniFile.WriteInteger( PICTURE_TYPE_NAME[p], 'Reverse mode player start position - GridShadowColor', Integer( RGBToColor( ReverseModePlayerStartPosition.GridShadowColor ) ) );
            end;
         end;
      if p=ptPlayerOnGoal then begin
         IniFile.WriteBool   (PICTURE_TYPE_NAME[p],'Use "Player" image for highlighting reachable squares',UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares);
         IniFile.WriteBool   (PICTURE_TYPE_NAME[p],'Use "Player" image for move animation',UsePlayerImageForMoveAnimationAlsoForGoalSquares);
         end;
      if p=ptBoxOnGoal then begin
         IniFile.WriteBool   (PICTURE_TYPE_NAME[p],'Use "Box" image for highlighting reachable squares',UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares);
         IniFile.WriteBool   (PICTURE_TYPE_NAME[p],'Use "Box" image for move animation',UseBoxImageForMoveAnimationAlsoForGoalSquares);
         end;
      if p=ptWall then begin
         IniFile.WriteInteger(PICTURE_TYPE_NAME[p],'WallType'                ,Ord(WallType));
         IniFile.WriteInteger(PICTURE_TYPE_NAME[p],'WallTrimmingLeft'        ,OuterWallTrimming.Left);
         IniFile.WriteInteger(PICTURE_TYPE_NAME[p],'WallTrimmingTop'         ,OuterWallTrimming.Top);
         IniFile.WriteInteger(PICTURE_TYPE_NAME[p],'WallTrimmingRight'       ,OuterWallTrimming.Right);
         IniFile.WriteInteger(PICTURE_TYPE_NAME[p],'WallTrimmingBottom'      ,OuterWallTrimming.Bottom);
         IniFile.WriteInteger(PICTURE_TYPE_NAME[p],'WallCap.Left'            ,WallCap.X);
         IniFile.WriteInteger(PICTURE_TYPE_NAME[p],'WallCap.Top'             ,WallCap.Y);
         end;
//    if p=ptFloor then
//       IniFile.WriteBool(PICTURE_TYPE_NAME[p],'Use "Floor" image as screen background',UseFloorTilesAsBackground);
      if (p=ptPlayerAnimation) or (p=ptPlayerOnGoalAnimation) or
         (p=ptBoxAnimation   ) or (p=ptBoxOnGoalAnimation   ) then
         IniFile.WriteInteger(PICTURE_TYPE_NAME[p],'InBetweenFramesType'     ,Ord(InBetweenFramesType[p]));
      end;
  IniFile.WriteInteger(GAME_VIEWER_INI_FILE_SECTION_NAME,'MaxZoomFactorPct',MaxZoomFactorPct);
  IniFile.WriteBool   (GAME_VIEWER_INI_FILE_SECTION_NAME,'GridEnabled',GridEnabled);
  IniFile.WriteInteger(GAME_VIEWER_INI_FILE_SECTION_NAME,'GridColor1',Integer(GridColor1));
  IniFile.WriteInteger(GAME_VIEWER_INI_FILE_SECTION_NAME,'GridColor2',Integer(GridColor2));
end;

function  TGamePictures.SaveToFile(const FileName:String):Boolean;
var W,H:Integer; p:TPictureType; B:TBitMap;
begin
  Result:=Initialized;
  if Result then
     for p:=Low(p) to High(p) do with Pictures[p] do
         Result:=Result and (OrgBitMap<>nil);
  if Result then begin
     W:=Pictures[ptGoal].OrgBitMap.Width;
     H:=Pictures[ptGoal].OrgBitMap.Height;
     Result:=Result and BitMapCreate(B,W,H);
     if Result then
        try
        finally B.Free;
        end;
     end;
end;

procedure TGamePictures.SetDefaultValues;
var p:TPictureType;
begin
  for p:=Low(p) to High(p) do with Pictures[p] do begin
      Color                  :=DEFAULT_PICTURE_COLOR[p];
      FileName               :=DEFAULT_VALUE;
      Antialiasing           :=DEFAULT_PICTURE_ANTI_ALIASING[p];
      KeepAspectRatio        :=p<ptPlayer; // keep aspect ratio for backgrounds
      MaskBitMapColor        :=RGB_BLACK;
      MaskBitMapPct          :=DEFAULT_MASK_BITMAP_PCT;
      Masked                 :=p>=ptReverseModeBackground;
      Transparent            :=False; //(p>=ptPlayer) and (p<ptWall); // 'True': treat the image as 'transparent' instead of working with a masked bitmap
      if   DEFAULT_TILE_NO[p,0]>=0 then
           SourceRect        :=GridCellToRect(DEFAULT_TILE_NO[p,0],
                                              DEFAULT_TILE_NO[p,1],
                                              DEFAULT_TILE_WIDTH,
                                              DEFAULT_TILE_HEIGHT,
                                              DEFAULT_TILE_GRIDLINE_WIDTH)
      else FillChar(SourceRect,SizeOf(SourceRect),0);
      DefaultRect[p]         :=SourceRect;
      if   p=ptReverseModeBackground then
           View              :=ivTile
      else View              :=ivFill;
      Visible                :=(p<>ptBoardBackground) and (p<>ptFloor) and
                               ((p<ptPlayerAnimation) or (p>ptBoxOnGoalAnimation));
      Transparency[p]        :=p=ptReverseModeBackground;
      if   (p=ptBoardBackground) or (p=ptFloor) or (p=ptGoal) then
           TransparencyPct[p]:=50
      else TransparencyPct[p]:=75;
      InBetweenFramesType [p]:=ibftInterpolate;
//    RotatingAnimation   [p]:=False;
      end;
  fMaxZoomFactorPct:=DEFAULT_ZOOM_FACTOR_PCT;
  ReverseModePlayerStartPosition.Visible := True;
  ReverseModePlayerStartPosition.GridSize := DEFAULT_PLAYER_START_POSITION_GRID_SIZE;
  ReverseModePlayerStartPosition.GridColor := BitMap_.ColorToRGB( DEFAULT_PLAYER_START_POSITION_GRID_COLOR );
  ReverseModePlayerStartPosition.GridShadowColor := BitMap_.ColorToRGB( DEFAULT_PLAYER_START_POSITION_GRID_SHADOW_COLOR );
  UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares:=False;
  UseBoxImageForMoveAnimationAlsoForGoalSquares:=False;
  UseFloorTilesAsBackground:=False;
  UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares:=False;
  UsePlayerImageForMoveAnimationAlsoForGoalSquares:=False;
  GridEnabled:=False;
  GridColor1:=DEFAULT_GRID_COLOR_1;
  GridColor2:=DEFAULT_GRID_COLOR_2;
  SetDefaultWallType;
end;

procedure TGamePictures.SetDefaultWallType;
begin
  if Assigned(Pictures[ptWall]) then with Pictures[ptWall] do begin
     FileName                   :=DEFAULT_VALUE;
     FillChar(OuterWallTrimming,SizeOf(OuterWallTrimming),0);
     WallCap.X                  :=-1; // '-1': default offset = midways between the squares
     WallCap.Y                  :=-1; // '-1': default offset = midways between the squares
     WallType                   :=wtSeamlessWallWithCap;
     MaskBitMapColor            :=RGB_BLACK;
     MaskBitMapPct              :=DEFAULT_MASK_BITMAP_PCT;
     Masked                     :=True;
     Antialiasing               :=DEFAULT_PICTURE_ANTI_ALIASING[ptWall];
     SourceRect                 :=DefaultRect[ptWall];
    end;
end;

procedure TGamePictures.SetMaxZoomFactorPct(MaxZoomFactorPct__:Integer);
begin
  if (MaxZoomFactorPct__>=MIN_ZOOM_FACTOR_PCT) and (MaxZoomFactorPct__<=MAX_ZOOM_FACTOR_PCT) then
     fMaxZoomFactorPct:=MaxZoomFactorPct__;
end;

{TGameViewer}

constructor TGameViewer.Create;
begin
  FillChar(Pictures          ,SizeOf(Pictures          ),0);
  FillChar(Sprites           ,SizeOf(Sprites           ),0);
  FillChar(LegalMoveBitMaps  ,SizeOf(LegalMoveBitMaps  ),0);
  FillChar(BoardRect         ,SizeOf(BoardRect         ),0);
  FillChar(fWindowRect       ,SizeOf(fWindowRect       ),0);
  FillChar(Board             ,SizeOf(Board             ),0);
  FillChar(LegalMovesInfo,SizeOf(LegalMovesInfo),0);
  FillChar(SolutionsInfo,SizeOf(SolutionsInfo),0);
  Left:=0; Top:=0; Width:=0; Height:=0; ColCount:=-1; RowCount:=-1;
  ReverseMode:=False; BoxTargetMaskForDisplay:=0; BoxCount:=-1;
  ColWidth:=1; RowHeight:=1; ClearFPS;
  Canvas:=nil; Game:=nil;
  BackBitMap:=nil; BoardMaskBitMap:=nil; TopLayerBitMap:=nil;
  FileName:=''; fWindowResizeCount:=0;
  LegalMovesInfo.ItemIndex:=-1;
  LegalMoveBitMapsInitialized:=False;
  FloorTilesVisible:=False; UseFloorTilesAsBackground:=False;
  HasPlayerDirectionAnimation:=False; IsASeamlessWall:=False;
  IsIdle:=False;
  SetDefaultValues; fInitialized:=False;
end;

destructor TGameViewer.Destroy;
begin
  Clear;
end;
{
procedure TGameViewer.AlphaBlendColor(const Rect__:TRect; Color__:TColor; Pct__:Integer);
var W,H:Integer; R:TRect;
begin
  W:=RectWidth(Rect__); H:=RectHeight(Rect__);
  if MakeStaticWorkBitMap(W,H,True) then begin
     R:=Classes.Rect(0,0,W,H);
     StaticWorkBitMap.Canvas.CopyMode:=cmSrcCopy;
     StaticWorkBitMap.Canvas.CopyRect(R,Canvas,Rect__);
     BitMapAlphaBlendColor(StaticWorkBitMap,Color__,Pct__,R);
     Canvas.CopyRect(Rect__,StaticWorkBitMap.Canvas,R);
     end;
end;
}
procedure TGameViewer.CellToPos(ACol,ARow:Integer; var X,Y:Integer); // 0-based col,row
begin
  X:=Left+ACol*ColWidth +BorderWidth;
  Y:=Top +ARow*RowHeight+BorderWidth;
end;

procedure TGameViewer.Clear;
var i:Integer; p:TPictureType; s:TLegalMoveBitMapsType;
begin
  BackBitMap.Free; BackBitMap:=nil;
  DestroyBoardMaskBitMap;
  DestroyObjectAnimationPictures(Pictures);

  if Assigned(MainForm) and (Self<>MainForm.GameViewer) then                    // 'MainForm' is responsible for destroying 'MainForm.GamePictures'
     for p:=Low(p) to High(p) do
         if Assigned(Pictures[p]) then with Pictures[p] do begin
            if (OrgBitMap<>nil) and
               (MainForm.GameViewer.Pictures[p]<>nil) and
               (MainForm.GameViewer.Pictures[p].OrgBitMap<>OrgBitMap) then
               OrgBitMap.Free;                          // 'OrgBitMap' belongs to this picture
            OrgBitMap:=nil; Pict:=nil;                  // 'Pict' and possibly 'OrgBitMap' belong to 'MainForm.GamePictures'
            Pictures[p].Free;
            end;
  FillChar(Pictures,SizeOf(Pictures),0);

  for i:=Low(Sprites) to High(Sprites) do Sprites[i].Free;
  FillChar(Sprites,SizeOf(Sprites ),0);

  for s:=Low(s) to High(s) do
      for i:=0 to 1 do LegalMoveBitMaps[s,i].Free;
  FillChar(LegalMoveBitMaps,SizeOf(LegalMoveBitMaps),0);
  LegalMoveBitMapsInitialized:=False;

  FillChar(PlayerDirectionRect,SizeOf(PlayerDirectionRect),0);

  FloorTilesVisible:=False; HasPlayerDirectionAnimation:=False; IsASeamlessWall:=False;
  UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares:=False;
  UseBoxImageForMoveAnimationAlsoForGoalSquares:=False;
  UseFloorTilesAsBackground:=False;
  UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares:=False;
  UsePlayerImageForMoveAnimationAlsoForGoalSquares:=False;
  BoxCount:=-1; ColCount:=-1; RowCount:=-1; ColWidth:=1; RowHeight:=1;
  LegalMovesInfo.Mask:=0; LegalMovesInfo.ItemIndex:=-1;
  FileName:=''; TopLayerBitMap:=nil;
  Modified:=True; fInitialized:=False;
end;

procedure TGameViewer.ClearFPS;
begin
  FrameTime:=0; FrameCount:=0;
end;

procedure TGameViewer.DestroyBoardMaskBitMap;
begin
  if      Pictures[ptBoardBackground      ]<>nil then
     with Pictures[ptBoardBackground      ] do begin
       if MaskBitMap<>BoardMaskBitMap then MaskBitMap.Free;
       MaskBitMap:=nil;
       end;

  if      Pictures[ptReverseModeBackground]<>nil then
     with Pictures[ptReverseModeBackground] do begin
       if MaskBitMap<>BoardMaskBitMap then MaskBitMap.Free;
       MaskBitMap:=nil;
       end;

  BoardMaskBitMap.Free; BoardMaskBitMap:=nil;               // background layers share one common board mask-bitmap
end;

function TGameViewer.GetBoxStandStillPictureType(Col__,Row__:Integer):TPictureType;
var BoardSquareValue:TBoardSquare;
begin
  BoardSquareValue:=Board[Col__,Row__];
  if   (BoardSquareValue and Game.BoxTargetMaskForDisplay)=0 then // 'True': a floor image square (in reverse mode this may be a goal position in the normal forwards game)
       if   (BoardSquareValue and SPECIAL_BACKGROUND_SQUARE)=0 then
            Result:=ptBox
       else Result:=ptBoxMoveAnimation
  else // a goal image square (in reverse mode this may be a box starting position in the normal forwards game)
       if   (BoardSquareValue and SPECIAL_BACKGROUND_SQUARE)=0 then
            Result:=ptBoxOnGoal
       else Result:=ptBoxOnGoalMoveAnimation;
end;

function TGameViewer.GetFPS:Integer;
begin
  if   FrameTime>0 then Result:=FrameCount*1000 div FrameTime
  else Result:=0;
end;

function TGameViewer.GetIsFloorTileVisible:Boolean;
begin
 Result:=(Pictures[ptFloor]<>nil) and Pictures[ptFloor].Visible;
end;

function TGameViewer.GetPlayerFrameIndex:Integer;
begin
 if   Initialized and
      (Sprites[PLAYER_SPRITE]<>nil) then with TSpriteN(Sprites[PLAYER_SPRITE]) do
      Result:=FrameIndex
 else Result:=0;
end;

function TGameViewer.GetPlayerStandStillPictureType(Col__,Row__:Integer):TPictureType;
var BoardSquareValue:TBoardSquare;
begin
  BoardSquareValue:=Board[Col__,Row__];
  if   (BoardSquareValue and Game.BoxTargetMaskForDisplay)=0 then // 'True': a floor image square (in reverse mode this may be a goal position in the normal forwards game)
       if   (BoardSquareValue and SPECIAL_BACKGROUND_SQUARE)=0 then
            Result:=ptPlayer
       else Result:=ptPlayerMoveAnimation
  else // a goal image square (in reverse mode this may be a box starting position in the normal forwards game)
       if   (BoardSquareValue and SPECIAL_BACKGROUND_SQUARE)=0 then
            Result:=ptPlayerOnGoal
       else Result:=ptPlayerOnGoalMoveAnimation;
end;

function TGameViewer.HideLegalMoves:Integer;
var i,Col,Row,dx,dy,X,Y,W,H:Integer; R:TRect;
begin
//MainForm.Status.Hint:='Hiding legal moves...'; Application.ProcessMessages; SleepEx(250,False);
  Result:=0; IsIdle:=False;
  HideLegalMovesCursor;
  if (LegalMovesInfo.Mask<>0)
     and
     (LegalMoveBitMapsInitialized
      or
      ( // the solution animation is independent of 'LegalMoveBitMapsInitialized'
       ((LegalMovesInfo.Mask and SQUARE_SET)<>0)
       and
       (Game.GameState=gsSolved)
       and
       (SolutionsInfo.PlayerAnimationEnabled or SolutionsInfo.BoxAnimationEnabled)
      )
     )
     then with LegalMovesInfo do begin
     for i:=PLAYER_SPRITE to Game.BoxCount do
         if (Sprites[i]<>nil) and
            (not Sprites[i].Visible) then // 'not Visible': move animation has removed the sprite from the board
            with Sprites[i] do begin
              SetVisibleStateWithoutUpdatingTheDisplay(True); // fake 'Visible'
              Hide; // the background is still valid, hence, hiding the sprite restores the background
              Show; // and finally, show the sprite normally
              end;

     if (Mask and SQUARE_SET)<>0 then begin
        for Col:=1 to ColCount do
            if   Mask<>0 then
                 for Row:=1 to RowCount do begin
                     i:=Game.Board[Col,Row];
                     if ((i and Mask)=Mask) and ((i and BOX)=0) then begin
                        // 'ShowBoard' can only remove highlights for boxes,
                        // not for goals, hence, refresh the display
                        Mask:=0; break;
                        end;
                     end
            else break;

        if   Mask<>0 then begin
             Mask:=0; ShowBoard(False);
             end
        else Show;
        end
     else begin
        W:=LegalMoveBitMaps[lmPlayer,0].Width;
        H:=LegalMoveBitMaps[lmPlayer,0].Height;
        dx:=(ColWidth -W) div 2;
        dy:=(RowHeight-H) div 2;

        for Col:=1 to ColCount do
            for Row:=1 to RowCount do begin
                i:=Game.Board[Col,Row];
                if ((i and Mask)=Mask)
                   or
                   (Game.ReverseMode and
                    (Mask=PLAYER_LEGAL_MOVE) and
                    ((i and (WALL+BOX+PLAYER+FLOOR))=FLOOR)) then begin

                   if (i and PLAYER)<>0 then Sprites[PLAYER_SPRITE].Hide;

                   CellToPos(Pred(Col),Pred(Row),X,Y); Inc(X,dx); Inc(Y,dy);
                   R:=Rect(X,Y,X+W,Y+H);

                   RestoreBackground(R);

                   if (i and BoxTargetMaskForDisplay)<>0 then
                      ShowGoal(X-dx,Y-dy);

                   if (i and PLAYER)<>0 then Sprites[PLAYER_SPRITE].Show;

                   Inc(Result);
                   end;
                end;
        end;
     end;
  LegalMovesInfo.Mask:=0; // no visible squares anymore
  MainForm.Status.Hint:='';
end;

procedure TGameViewer.HideBox(BoxNo:Integer);
begin
  if (BoxNo>=0) and (BoxNo<=BoxCount) and (Sprites[BoxNo]<>nil) then
     Sprites[BoxNo].Hide;
end;

procedure TGameViewer.HideLegalMovesCursor;
begin
  if Initialized then with LegalMovesInfo do begin
     if ((ItemIndex>=PLAYER_SPRITE) and (ItemIndex<=Game.BoxCount) or (ItemIndex=CURSOR_SPRITE))
        and
        (Sprites[ItemIndex]<>nil) then with Sprites[ItemIndex] do
        if   ItemIndex<>CURSOR_SPRITE then begin
             if   not Visible then begin // highlight-animation has removed the sprite from the board
                  SetVisibleStateWithoutUpdatingTheDisplay(True); // fake 'Visible'
                  Hide; // the background is still valid, hence, hiding the sprite restores the background
                  end;
             MoveTo(R.Left,R.Top) // showing the sprite normally removes the cursor from the screen
             end
        else Hide; // the cursor-sprite is not really used as a sprite; it's only used for saving and restoring the background
     ItemIndex:=-1;
     end;
end;

function TGameViewer.IsBoardBackgroundOrReverseModeBackground(Pict:TPict):Boolean;
begin
  Result:=(Pict<>nil) and
          ((Pict=Pictures[ptBoardBackground]) or (Pict=Pictures[ptReverseModeBackground]));
end;

function  TGameViewer.LoadGame(Game__:TGame):Boolean;
var i,j,W,H:Integer; BoardIsOK,MaskBitMapIsOK,OldMasked,RefreshScreenBackground:Boolean;
    d:TDirection; OldMaskColor:TRGB;
    p,q,r:TPictureType;
    OnHideSprite:THideSpriteEvent;

    procedure ShowSimpleDeadSquares(PictureType:TPictureType);
    var Col,Row,X,Y:Integer;
    begin
      if MainForm.ShowSimpleDeadSquaresEnabled
         //and (Self=MainForm.GameViewer)
         then
         with Pictures[PictureType] do
           for Col:=1 to ColCount do
               for Row:=1 to RowCount do
                   if (Board[Col,Row] and (ILLEGAL_SQUARE+BOX_UNREACHABLE_FLOOR+PLAYER_UNREACHABLE_FLOOR))<>0 then begin
                      CellToPos(Pred(Col),Pred(Row),X,Y);
                      if BitMap.Width=Self.Width then Dec(X,Self.Left)
                      else Dec(X,WindowRect.Left);
                      if BitMap.Height=Self.Height then Dec(Y,Self.Top)
                      else Dec(Y,WindowRect.Top);
                      BitMapAlphaBlendColor(BitMap,MainForm.ShowSimpleDeadSquaresColor,100-MainForm.ShowSimpleDeadSquaresTransparencyPct,Rect(X,Y,X+ColWidth,Y+RowHeight));
                      end;
    end;

  procedure MakeBackground(PictureType:TPictureType);
  var R,R0:TRect; B:TBitMap;

    // ---------------------------------------
    function MarkPlayerStartPositionInReverseModePlay : Boolean;
    var   Col, Row, Cols, Rows, W, H, GridSize, Index, Countdown, MaskLimit : Integer;
          RGB : TRGB;
          R : TRect;
    begin
      Result := Assigned( MainForm.GamePictures ) and
                MainForm.GamePictures.ReverseModePlayerStartPosition.Visible and
                Assigned( Game ) and
                Assigned( Pictures[ ptReverseModeBackground ] ) and
                Assigned( Pictures[ ptReverseModeBackground ].BitMap );
      if Result then
         with Pictures[ ptReverseModeBackground ] do with BitMap.Canvas do begin
           // calculate bitmap rectangle for the player starting position
           CellToPos( Pred( Game.PlayerStartPos.X ),
                      Pred( Game.PlayerStartPos.Y ), R.Left, R.Top );
           if   BitMap.Width  = Self.Width  then
                Dec( R.Left, Self.Left )
           else Dec( R.Left, WindowRect.Left );
           if   BitMap.Height = Self.Height then
                Dec( R.Top, Self.Top )
           else Dec( R.Top, WindowRect.Top );
           R.Right  := R.Left + ColWidth;
           R.Bottom := R.Top  + RowHeight;

           Pen.Style := psSolid;
           Pen.Width := 1;

           GridSize  := MainForm.GamePictures.ReverseModePlayerStartPosition.GridSize;

           W := RectWidth ( R );
           H := RectHeight( R );
           Cols := Pred( ( W - 4 ) div GridSize ); // grid columns
           Rows := Pred( ( H - 4 ) div GridSize ); // grid rows

           if ( Cols > 1 ) and ( Rows > 1 ) then begin
              // center grid
              Inc( R.Left, Succ( W   - ( Cols * GridSize ) ) div 2 ); // 'Succ':
              Inc( R.Top , Succ( H   - ( Rows * GridSize ) ) div 2 ); // 'Succ': the grid shadow is drawn first
              R.Right  := R.Left + ( Cols * GridSize );
              R.Bottom := R.Top  + ( Rows * GridSize );
              MaskLimit:=((3*255)*MaskBitMapPct) div 100;

              for Index := 0 to 1 do begin
                  if Index = 0 then
                     RGB := MainForm.GamePictures.ReverseModePlayerStartPosition.GridShadowColor
                  else begin
                     Dec( R.Left ); Dec( R.Right ); Dec( R.Top ); Dec( R.Bottom );
                     RGB := MainForm.GamePictures.ReverseModePlayerStartPosition.GridColor;
                     end;

                  if Assigned( Pictures[ptReverseModeBackground] ) then
                     with Pictures[ptReverseModeBackground] do
                       if Masked then begin
                          Countdown := High( RGB.r ) div 4;
                          while ( ((Abs(RGB.r-MaskBitMapColor.r)+
                                    Abs(rgb.g-MaskBitMapColor.g)+
                                    Abs(rgb.b-MaskBitMapColor.b))) <= MaskLimit ) and
                                ( Countdown > 0 ) do begin
                                Inc( RGB.r, 4 ); Inc( RGB.g, 4 ); Inc( RGB.b, 4 ); // avoid that the grid colors are so close to the background color that the grid is masked out
                                Dec( Countdown );
                                end;
                          end;

                  Pen.Color := BitMap_.RGBToColor( RGB );
                  // draw grid - horizontal lines
                  Row := R.Top;
                  while Row <= R.Bottom do begin
                    MoveTo( R.Left, Row ); LineTo( Succ( R.Right ), Row );
                    Inc ( Row, GridSize );
                    end;
                  // draw grid - vertical lines
                  Col := R.Left;
                  while Col <= R.Right do begin
                    MoveTo( Col, R.Top ); LineTo( Col, R.Bottom );
                    Inc ( Col, GridSize );
                    end;
                  end;
              end;
           end;
    end;

    // ---------------------------------------
    function  MakeBoardMaskBitMap:Boolean;
    var a,b,c,x,y,Right__,Bottom__:Integer;
        R,R1:TRect; //Time:Cardinal;
        LeftFirstSolidCol,RightLastSolidCol,
        TopFirstSolidRow,BottomLastSolidRow:Integer;
        MakeCorners,MakeTopLeftCorner,MakeTopRightCorner,
        MakeBottomLeftCorner,MakeBottomRightcorner:Integer;
        WallMaskBitMap:TBitMap;

      // - - - - - - - - - - - - - - - - - - - -
      procedure Check(x,y,Width,Height:Integer; const R:TRect; WallMaskBitMap:TBitMap);
      var i,j:Integer;
      begin
        exit;

        if (x>=0) and (y>=0) and (x<Width) and (y<Height) and
           (BoardMaskBitMap .Canvas.Pixels[R.Left+x,R.Top+y] =clWhite) and
           (WallMaskBitMap  .Canvas.Pixels[x       ,y      ] =clWhite) then begin
           BoardMaskBitMap  .Canvas.Pixels[R.Left+x,R.Top+y]:=clBlack;
           for i:=-1 to 1 do
               for j:=-1 to 1 do
                   Check(x+i,y+j,Width,Height,R,WallMaskBitMap);
           end;
      end;

      // - - - - - - - - - - - - - - - - - - - -
      procedure AnalyzeWallMaskBitMap;
      var i,j,H,W:Integer; R:TRect;
      begin
        WallMaskBitMap:=Pictures[ptWall].MaskBitMap;
        LeftFirstSolidCol:=0; RightLastSolidCol :=0;
        TopFirstSolidRow :=0; BottomLastSolidRow:=0;

        if WallMaskBitMap<>nil then with WallMaskBitMap do begin
           W:=Width div MainForm.GamePictures.FrameCount[ptWall];
           H:=Height;
           for i:=0 to  Pred(W) do begin
               LeftFirstSolidCol:=i;
               for j:=0 to Pred(H) do
                   if Canvas.Pixels[i,j]=clWhite then begin
                      LeftFirstSolidCol:=-1; break
                      end;
               if LeftFirstSolidCol<>-1 then break;
               end;

           if   LeftFirstSolidCol<1 then LeftFirstSolidCol:=0
           else for i:=Pred(W) downto LeftFirstSolidCol do begin
                    RightLastSolidCol:=i;
                    for j:=0 to Pred(H) do
                        if Canvas.Pixels[i,j]=clWhite then begin
                           RightLastSolidCol:=-1; break
                        end;
                    if RightLastSolidCol<>-1 then break;
                    end;
           if   RightLastSolidCol<1 then RightLastSolidCol:=0
           else RightLastSolidCol:=Pred(W-RightLastSolidCol);

           for j:=0 to  Pred(H) do begin
               TopFirstSolidRow:=j;
               for i:=0 to Pred(W) do
                   if Canvas.Pixels[i,j]=clWhite then begin
                      TopFirstSolidRow:=-1; break
                      end;
               if TopFirstSolidRow<>-1 then break;
               end;

           if   TopFirstSolidRow<1 then TopFirstSolidRow:=0
           else for j:=Pred(H) downto TopFirstSolidRow do begin
                    BottomLastSolidRow:=j;
                    for i:=0 to Pred(W) do
                        if Canvas.Pixels[i,j]=clWhite then begin
                           BottomLastSolidRow:=-1; break
                           end;
                    if BottomLastSolidRow<>-1 then break;
                    end;
           if   BottomLastSolidRow<1 then BottomLastSolidRow:=0
           else BottomLastSolidRow:=Pred(Height-BottomLastSolidRow);
           end;

        if IsASeamlessWall and
           (Pictures[ptWall]<>nil) and
           (Pictures[ptWall].BitMap<>nil) then
           with Pictures[ptWall].BitMap do begin
             W:=Width div MainForm.GamePictures.FrameCount[ptWall]; H:=Height;
             R:=Rect(0,0,W,H);
             LeftFirstSolidCol     :=Max(LeftFirstSolidCol,OuterWallTrimming.Left);
             TopFirstSolidRow      :=Max(TopFirstSolidRow,OuterWallTrimming.Top);
             RightLastSolidCol     :=Max(RightLastSolidCol,OuterWallTrimming.Right);
             BottomLastSolidRow    :=Max(BottomLastSolidRow,OuterWallTrimming.Bottom);

             // walls may exceed the area painted by the board background;
             // hence, the following assignments are disabled; otherwise,
             // irregular walls with, say, rounded corners would be razored
             // for no reason;
             // OuterWallTrimming.Left   :=LeftFirstSolidCol;
             // OuterWallTrimming.Top    :=TopFirstSolidRow;
             // OuterWallTrimming.Right  :=RightLastSolidCol;
             // OuterWallTrimming.Bottom :=BottomLastSolidRow;
             end;
      end;

    begin // MakeBoardMaskBitMap
      Result:=Initialized;
      DestroyBoardMaskBitMap;

      if Result then begin
         Result:=BitMapCreate(BoardMaskBitMap,Width,Height);
         if Result then begin

            if  (Game.BoardWidth*Game.BoardHeight>Game.BTSquareCount) or
                (BorderWidth=0) then begin

                BoardMaskBitMap.Canvas.Brush.Color:=clWhite;
                BoardMaskBitMap.Canvas.FillRect(Rect(0,0,BoardMaskBitMap.Width,BoardMaskBitMap.Height));

                AnalyzeWallMaskBitMap;

                // 'Make Corners' is actually not making corners for the board.
                // Instead it controls how the enclosing background 'floodfills'
                // corners not in contact with the part of the board, which the
                // player has access to
                //
                // note that 'RightLastSolidCol' and 'BottomLastSolidRow' are
                // not absolute values; they are relative to width/height,
                if (LeftFirstSolidCol<>0) and (TopFirstSolidRow  <>0) then MakeTopLeftCorner    :=BT_CORNER_TOP_LEFT
                else MakeTopLeftCorner:=0;
                if (RightLastSolidCol<>0) and (TopFirstSolidRow  <>0) then MakeTopRightCorner   :=BT_CORNER_TOP_RIGHT
                else MakeTopRightCorner:=0;
                if (LeftFirstSolidCol<>0) and (BottomLastSolidRow<>0) then MakeBottomLeftCorner :=BT_CORNER_BOTTOM_LEFT
                else MakeBottomLeftCorner:=0;
                if (RightLastSolidCol<>0) and (BottomLastSolidRow<>0) then MakeBottomRightCorner:=BT_CORNER_BOTTOM_RIGHT
                else MakeBottomRightCorner:=0;
                MakeCorners:=MakeTopLeftCorner or MakeTopRightCorner or MakeBottomLeftCorner or MakeBottomRightCorner;

                BoardMaskBitMap.Canvas.Brush.Color:=clBlack;
                for a:=1 to Game.BoardWidth do
                    for b:=1 to Game.BoardHeight do
                        if Game.BTSquare[a,b]<>0 then begin
                           c       :=Game.BTSquare[a,b];
                           x       :=Pred(a)*ColWidth +BorderWidth;
                           y       :=Pred(b)*RowHeight+BorderWidth;

                           R.Left  :=x;
                           R.Right :=x+ColWidth;  Right__ :=R.Right;
                           R.Top   :=y;
                           R.Bottom:=y+RowHeight; Bottom__:=R.Bottom;

                           if BorderWidth=0 then begin
                              if (c and BT_LEFT  )<>0 then Inc(R.Left  ,LeftFirstSolidCol);
                              if (c and BT_RIGHT )<>0 then Dec(R.Right ,RightLastSolidCol);

                              if (c and BT_TOP   )<>0 then Inc(R.Top   ,TopFirstSolidRow);
                              if (c and BT_BOTTOM)<>0 then Dec(R.Bottom,BottomLastSolidRow);

                              if (c and MakeCorners)<>0 then begin
                                 if ((c and MakeTopLeftCorner)<>0) and
                                    (R.Left=x) and
                                    (R.Top=y) then begin
                                    R1.Left  :=R.Left+LeftFirstSolidCol;
                                    R1.Top   :=R.Top;
                                    if   (c and MakeTopRightCorner)=0 then
                                         R1.Right:=R.Right
                                    else R1.Right:=R.Right-RightLastSolidCol;
                                    R1.Bottom:=R1.Top+TopFirstSolidRow;
                                    BoardMaskBitMap.Canvas.FillRect(R1);
                                    R.Top:=R1.Bottom;
                                    end;
                                 if ((c and MakeTopRightCorner)<>0) and
                                    (R.Right=Right__) and
                                    (R.Top=y) then begin
                                    R1.Left  :=R.Left;
                                    R1.Top   :=R.Top;
                                    R1.Right :=R.Right-RightLastSolidCol;
                                    R1.Bottom:=R1.Top+TopFirstSolidRow;
                                    BoardMaskBitMap.Canvas.FillRect(R1);
                                    R.Top:=R1.Bottom;
                                    end;
                                 if ((c and MakeBottomLeftCorner)<>0) and
                                    (R.Left=x) and
                                    (R.Bottom=Bottom__) then begin
                                    R1.Left  :=R.Left+LeftFirstSolidCol;
                                    R1.Top   :=R.Bottom-BottomLastSolidRow;
                                    if   (c and MakeBottomRightCorner)=0 then
                                         R1.Right:=R.Right
                                    else R1.Right:=R.Right-RightLastSolidCol;
                                    R1.Bottom:=R.Bottom;
                                    BoardMaskBitMap.Canvas.FillRect(R1);
                                    R.Bottom:=R1.Top;
                                    end;
                                 if ((c and MakeBottomRightCorner)<>0) and
                                    (R.Right=Right__) and
                                    (R.Bottom=Bottom__) then begin
                                    R1.Left  :=R.Left;
                                    R1.Top   :=R.Bottom-BottomLastSolidRow;
                                    R1.Right :=R.Right-RightLastSolidCol;
                                    R1.Bottom:=R.Bottom;
                                    BoardMaskBitMap.Canvas.FillRect(R1);
                                    R.Bottom:=R1.Top;
                                    end;
                                 end;
                              BoardMaskBitMap.Canvas.FillRect(R);
                              end
                           else begin
                             if (c and BT_LEFT  )<>0 then Dec(R.Left  ,BorderWidth);
                             if (c and BT_RIGHT )<>0 then Inc(R.Right ,BorderWidth);
                             if (c and BT_TOP   )<>0 then Dec(R.Top   ,BorderWidth);
                             if (c and BT_BOTTOM)<>0 then Inc(R.Bottom,BorderWidth);
                             BoardMaskBitMap.Canvas.FillRect(R);
                             end;

                           {
                           //Time:=GetTickCount;
                           if (BorderWidth=0) and
                              ((Game.Board[a,b] and WALL)<>0) then begin
                              if ((c and BT_RIGHT)<>0) then begin
                                 Check(Pred(Board.ColWidth),0,Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                 Check(Pred(Board.ColWidth),Pred(Board.RowHeight),Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                 if ((c and BT_TOP)<>0) then
                                    Check(0,0,Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                 if ((c and BT_BOTTOM)<>0) then
                                    Check(0,Pred(Board.RowHeight),Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                 end
                              else if ((c and BT_TOP)<>0) then begin
                                      Check(0,0,Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                      Check(Pred(Board.ColWidth),0,Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                      if ((c and BT_LEFT)<>0) then
                                         Check(0,Pred(Board.RowHeight),Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                      end
                              else if ((c and BT_LEFT)<>0) then begin
                                      Check(0,0,Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                      Check(0,Pred(Board.RowHeight),Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                      if ((c and BT_BOTTOM)<>0) then
                                         Check(Pred(Board.ColWidth),Pred(Board.RowHeight),Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                      end
                              else if ((c and BT_BOTTOM)<>0) then begin
                                      Check(0,Pred(Board.RowHeight),Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                      Check(Pred(Board.ColWidth),Pred(Board.RowHeight),Board.ColWidth,Board.RowHeight,R,Board.Picture[ptWall].AndBitMap);
                                      end;
                              end;
                           //Msg(IntToStr(GetTickCount-Time),MB_OK);
                           }
                           end;
                end;
            //BitMapDump(BoardMaskBitMap);
            end;
         end;
    end;
    // ---------------------------------------

  begin // MakeBackground
    if Pictures[PictureType]<>nil then with Pictures[PictureType] do begin
       if BoardMaskBitMap=nil then MakeBoardMaskBitMap; // make new mask; only created on demand
       if MaskBitMap<>BoardMaskBitMap then MaskBitMap.Free;
       MaskBitMap:=nil;
       R0:=Rect(0,0,Self.Width,Self.Height);

       if Visible then begin
          SetView(View,Self.Width,Self.Height,Color);
          if PictureType = ptReverseModeBackground then
             MarkPlayerStartPositionInReverseModePlay;
          end
       else
         if BitMapResize(BitMap,Self.Width,Self.Height) then with BitMap do begin
            Canvas.Brush.Color:=Pictures[PictureType].Color;
            Canvas.CopyMode:=cmSrcCopy;
            if   Pictures[ptScreenBackground].Visible and
                 (Pictures[ptScreenBackground].BitMap<>nil) then
                 Canvas.CopyRect(R0,Pictures[ptScreenBackground].BitMap.Canvas,Rect(Left,Top,Left+Width,Top+Height))
            else Canvas.FillRect(R0);
            end;

       MaskBitMap:=BoardMaskBitMap;

       if (BitMap<>nil) and
          MainForm.GamePictures.Transparency[PictureType] then begin

          if      (PictureType<>ptBoardBackground) and
                  (Pictures[ptBoardBackground].Visible
                   or
                   Pictures[ptFloor].Visible
                  ) then begin
                  B:=Pictures[ptBoardBackground].BitMap;
                  R:=R0;
                  end
          else if Pictures[ptScreenBackground].Visible then begin
                  B:=Pictures[ptScreenBackground].BitMap;
                  R:=Rect(Left,Top,Left+Width,Top+Height);
                  end
               else with Pictures[ptScreenBackground] do
                  if        (Self=MainForm.GameViewer) and
                            BitMapResize(BitMap,MainForm.Image1.ClientWidth,MainForm.Image1.ClientWidth) then begin
                            B:=BitMap;
                            R:=Rect(Self.Left,Self.Top,Self.Left+Self.Width,Self.Top+Self.Height);
                            B.Canvas.Brush.Color:=Color;
                            B.Canvas.FillRect(Rect(0,0,BitMap.Width,BitMap.Height));
                            end
                  else if   (Self<>MainForm.GameViewer) and
                            BitMapResize(BitMap,Self.Width,Self.Height) then begin
                            B:=BitMap;
                            R:=R0;
                            B.Canvas.Brush.Color:=Color;
                            B.Canvas.FillRect(R);
                            end
                       else B:=nil;

          if (BitMap<>nil) and (B<>nil) and
             (B.Width>=R.Right) and (B.Height>=R.Bottom) then // just to be sure
             BitMapAlphaBlendRect(BitMap,BitMap,B,R0,R0,R,
                                  MainForm.GamePictures.TransparencyPct[PictureType],
                                  Masked,RGBToColor(MaskBitMapColor),MaskBitMapPct);
          end;

       if PictureType=ptBoardBackground then begin
          if Pictures[ptFloor].Visible then begin
             BitMapResize(BitMap,Self.Width,Self.Height);
             PutTilesOnTheBoard(FLOOR_SPRITE,WALL+FLOOR,MainForm.GamePictures.Transparency[ptFloor],MainForm.GamePictures.TransparencyPct[ptFloor],BitMap);
             end;
          ShowSimpleDeadSquares(ptBoardBackground);
          end;

       if PictureType=ptReverseModeBackground then begin
          if (not Pictures[ptBoardBackground].Visible)
             and
             (not Pictures[ptFloor].Visible)
             then
             ShowSimpleDeadSquares(ptReverseModeBackground);
          end;

       BitMapApplyMask(BitMap,MaskBitMap);
       end;
  end; // MakeBackground

  procedure MakeLegalMoveBitMaps;
  var i,j:Integer; s:TLegalMoveBitMapsType; p,t:TPictureType; Pict:TPict;
      oOrgBitMap,oOrgBitMapWithFloodFilledMaskedPixels:TBitMap;

    function MakeJumpMoveBitMap(Width__,Height__:Integer):Boolean;
    var x,y:Integer; Size:TSize; R:TRect;
    begin
      Result:=BitMapResize(LegalMoveBitMaps[lmJump,0],Width__,Height__) and
              BitMapResize(LegalMoveBitMaps[lmJump,1],Width__,Height__);
      if Result then with LegalMoveBitMaps [lmJump,0] do with Canvas do with Size do begin
         Brush.Color:=clBlack;
         FillRect(Rect(0,0,Width,Height));

         Font.Name:='Arial'; Font.Style:=[fsBold]; Font.Size:=11;
         repeat
           Font.Size:=Font.Size-1;
           Size:=TextExtent(JumpText);
         until (cx<Pred(Width)) and (cy<Pred(Height)) or
               (Font.Size=1);
         x:=(Width -cx) div 2;
         y:=(Height-cy) div 2;
         R:=Rect(Max(0,Pred(x)),Max(0,Pred(y)),Min(Width,x+cx+2),Min(Height,y+cy+2));

         Brush.Color:=LegalMovesInfo.JumpMovesBackgroundColor;
         FillRect(R);

         Font.Color:=LegalMovesInfo.JumpMovesTextShadowColor;
         TextOut(Succ(x),Succ(y),JumpText);
         Windows.SetBkMode(Handle,Windows.TRANSPARENT);
         Font.Color:=LegalMovesInfo.JumpMovesTextColor;
         TextOut(x,y,JumpText);

         with LegalMoveBitMaps[lmJump,1] do with Canvas do begin
           Brush.Color:=clWhite;
           FillRect(Rect(0,0,Width,Height));
           Brush.Color:=clBlack;
           FillRect(R);
           end;
         end;
    end;

  begin // MakeLegalMoveBitMaps
    LegalMoveBitMapsInitialized:=True;
    FillChar(LegalMoveBitMapsCreated,SizeOf(LegalMoveBitMapsCreated),0);
    i:=Max(2,ColWidth *LegalMovesInfo.Size div 100);
    j:=Max(2,RowHeight*LegalMovesInfo.Size div 100);
    if Odd(ColWidth -i) then Inc(i); // avoid non-centered positions due to rounding
    if Odd(RowHeight-j) then Inc(j); //

    for s:=Low(s) to High(s) do
        if LegalMoveBitMapsInitialized then
           if      s=lmJump then
                   LegalMoveBitMapsInitialized:=MakeJumpMoveBitMap(i,j)
           else if s=lmTemporary then
                   LegalMoveBitMapsInitialized:=
                     BitMapResize(LegalMoveBitMaps[s,0],i,j) and
                     BitMapResize(LegalMoveBitMaps[s,1],i*ColCount*3,j*RowCount) // '*3': for player, box, and jump
                else begin
                   oOrgBitMap:=nil; oOrgBitMapWithFloodFilledMaskedPixels:=nil;
                   Pict:=nil;
                   try
                     case s of
                       lmPlayer      : begin Pict:=Pictures[ptPlayer];
                                             if HasPlayerDirectionAnimation and
                                                Pict.MakeFrameBitMap(Ord(DEFAULT_PLAYER_DIRECTION),MainForm.GamePictures.FrameCount[ptPlayer]) then begin
                                                oOrgBitMap:=Pict.OrgBitMap;
                                                Pict.OrgBitMap:=Pict.BitMap;
                                                Pict.BitMap:=nil;
                                                oOrgBitMapWithFloodFilledMaskedPixels:=Pict.OrgBitMapWithFloodFilledMaskedPixels;
                                                Pict.OrgBitMapWithFloodFilledMaskedPixels:=nil;
                                                end;
                                       end;
                       lmPlayerOnGoal: begin if   UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares then
                                                  p:=ptPlayer
                                             else p:=ptPlayerOnGoal;
                                             Pict:=Pictures[p];
                                             if HasPlayerDirectionAnimation and
                                                Pict.MakeFrameBitMap(Ord(DEFAULT_PLAYER_DIRECTION),MainForm.GamePictures.FrameCount[p]) then begin
                                                oOrgBitMap:=Pict.OrgBitMap;
                                                Pict.OrgBitMap:=Pict.BitMap;
                                                Pict.BitMap:=nil;
                                                oOrgBitMapWithFloodFilledMaskedPixels:=Pict.OrgBitMapWithFloodFilledMaskedPixels;
                                                Pict.OrgBitMapWithFloodFilledMaskedPixels:=nil;
                                                end;
                                       end;
                       lmBox         : Pict:=Pictures[ptBox];
                       else            if   UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares then
                                            Pict:=Pictures[ptBox]
                                       else Pict:=Pictures[ptBoxOnGoal];
                     end; //case
                     if (Pict<>nil) and
                        Pict.Resize(i,j) and // let 'Pictures' do the work with resizing and mask-creation
                        (Pict.BitMap<>nil) then begin

                        if (not Pict.Masked) and
                           (Pictures[ptFloor]<>nil) and
                           Pictures[ptFloor].Visible then begin
                           if   (s=lmPlayer) or (s=lmBox) or
                                ((s=lmPlayerOnGoal) and UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares) or
                                ((s=lmBoxOnGoal   ) and UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares   ) then
                                t:=ptFloor
                           else t:=ptGoal;
                           if (Pictures[t].BitMap<>nil)
                              and
                              (((Pictures[t].BitMap.Width=i)
                                and
                                (Pictures[t].BitMap.Height=j)
                               )
                               or
                               Pictures[t].Resize(i,j)
                              ) then
                              Pict.MaskBackgroundBitMap(Pictures[t].BitMap);
                           end;

                        LegalMoveBitMaps[s,0].Free;
                        LegalMoveBitMaps[s,0]:=Pict.GiveBitMapAway;
                        LegalMoveBitMaps[s,1].Free;
                        LegalMoveBitMaps[s,1]:=Pict.GiveMaskBitMapAway;

                        //BitMapDump(LegalMoveBitMaps[s,0]);
                        //BitMapDump(LegalMoveBitMaps[s,1]);
                        end
                     else
                        LegalMoveBitMapsInitialized:=False;
                   finally
                     if (oOrgBitMap<>nil) and (Pict<>nil) and
                        (oOrgBitMap<>Pict.OrgBitMap) then begin
                        Pict.OrgBitMap.Free;
                        Pict.OrgBitMap:=oOrgBitMap; // restore original bitmap with all the frames
                        end;
                     if (oOrgBitMapWithFloodFilledMaskedPixels<>nil) and (Pict<>nil) and
                        (oOrgBitMapWithFloodFilledMaskedPixels<>Pict.OrgBitMapWithFloodFilledMaskedPixels) then begin
                        Pict.OrgBitMapWithFloodFilledMaskedPixels.Free;
                        Pict.OrgBitMapWithFloodFilledMaskedPixels:=oOrgBitMapWithFloodFilledMaskedPixels; // restore original floodfilled bitmap with all the frames
                        end;
                   end;
                   end;
  end; // MakeLegalMoveBitMaps

  function CalculateMaskBitMapIsOK:Boolean;
  begin
    Result:=True;
    if Pictures[ptBoardBackground].Visible or // the mask is only required when one of the board backgrounds are visible
       (Game.ReverseMode and Pictures[ptReverseModeBackground].Visible) or
       Pictures[ptFloor].Visible then // if individual floor-tiles are visible then they are drawn on the board background
       Result:=BoardIsOK and
               (BoardMaskBitMap<>nil) and
               (BoardMaskBitMap.Width =RectWidth (BoardRect)) and
               (BoardMaskBitMap.Height=RectHeight(BoardRect));
  end;

  function CalculateBoardIsOK:Boolean;
  const MASK=WALL+FLOOR+GOAL+BOX_START_POSITION;
  var Col,Row:Integer;
  begin
    Result:=(ColCount=Game.BoardWidth) and (RowCount=Game.BoardHeight);
    for Col:=1 to ColCount do
        if   Result then
             for Row:=1 to RowCount do
                 if   Result then
                      Result:=(Board[Col,Row] and MASK) = (Game.Board[Col,Row] and MASK)
                 else break
        else break;
  end;

  procedure CalculateSpecialBackgroundSquares;
  var Col,Row:Integer;
  begin // flag squares with a special background, i.e., the "reverse mode" text, or a special "dead square" color
    for Col:=1 to ColCount do
        for Row:=1 to RowCount do
            if   not ReverseMode then
                 if   ((not MainForm.ShowSimpleDeadSquaresEnabled)
                       or
                       ((Board[Col,Row] and (ILLEGAL_SQUARE+BOX_UNREACHABLE_FLOOR+PLAYER_UNREACHABLE_FLOOR))=0)
                      ) then
                      Board[Col,Row]:=Board[Col,Row] and (not SPECIAL_BACKGROUND_SQUARE)
                 else Board[Col,Row]:=Board[Col,Row] or SPECIAL_BACKGROUND_SQUARE // illegal squares have a special background
            else if   (Board[Col,Row] and BoxTargetMaskForDisplay)<>0 then
                      //Board[Col,Row]:=Board[Col,Row] and (not SPECIAL_BACKGROUND_SQUARE) // squares shown as goals haven't a special background
                      Board[Col,Row]:=Board[Col,Row] or SPECIAL_BACKGROUND_SQUARE // squares shown as goals have a special background
                 else Board[Col,Row]:=Board[Col,Row] or SPECIAL_BACKGROUND_SQUARE // squares shown as non-goal floors have a special background
  end;

  function BoardBackgroundIsOK:Boolean;
  begin
    with Pictures[ptBoardBackground] do
      Result:=((not Visible)
               and
               (not Pictures[ptFloor].Visible)
               and
               (not (MainForm.ShowSimpleDeadSquaresEnabled {and (Self=MainForm.GameViewer)}))
              )
              or
              ((BitMap<>nil)
               and
               (BitMap.Width =RectWidth (BoardRect))
               and
               (BitMap.Height=RectHeight(BoardRect))
               and
               (MaskBitMap<>nil)
               and
               (FloorTilesVisible=Pictures[ptFloor].Visible)
               and
               (not (MainForm.ShowSimpleDeadSquaresEnabled {and (Self=MainForm.GameViewer)}))
               and
               (Pictures[ptScreenBackground].View<>ivFloorTile)
              );
  end;

  function ReverseModeBackgroundIsOK:Boolean;
  begin
    with Pictures[ptReverseModeBackground] do
      Result:=(not Game.ReverseMode)
              or
              (not Visible)
              or
              ((BitMap<>nil)
               and
               (BitMap.Width =RectWidth (WindowRect))
               and
               (BitMap.Height=RectHeight(WindowRect))
               and
               (MaskBitMap<>nil)
               and
               (FloorTilesVisible=Pictures[ptFloor].Visible)
               and
               (not (MainForm.ShowSimpleDeadSquaresEnabled {and (Self=MainForm.GameViewer)}))
               and
               (Pictures[ptScreenBackground].View<>ivFloorTile)
              );
  end;
{
  function  AdHocTask:Boolean;
  var B:TBitMap;
  begin
    Result:=False;
    //BitMapDump(Pictures[ptPlayerMoveAnimation].BitMap);
    //BitMapDump(Pictures[ptPlayerMoveAnimation].MaskBitMap);
    //BitMapDump(Pictures[ptGoal].BitMap);

    if BitMapCreate(B,Pictures[ptGoal].BitMap.Width,Pictures[ptGoal].BitMap.Height) then with B do with Canvas do
       try
         CopyMode:=cmSrcCopy;
         CopyRect(Rect(0,0,Width,Height),Pictures[ptGoal].BitMap.Canvas,Rect(0,0,Width,Height));
         Sprites[PLAYER_SPRITE].Canvas:=B.Canvas;
         Sprites[PLAYER_SPRITE].Resize(Width,Height);
         Sprites[PLAYER_SPRITE].Pict:=Pictures[ptPlayerMoveAnimation];
         TSpriteN(Sprites[PLAYER_SPRITE]).DrawFrame(0,0,1);
         BitMapDump(B);
       finally B.Free;
               Sprites[PLAYER_SPRITE].Canvas:=Self.Canvas;
       end;
  end;
}
begin // LoadGame
  if not Initialized then LoadPictures;
  Game:=Game__;
  FillChar(LegalMoveBitMapsCreated,SizeOf(LegalMoveBitMapsCreated),0);
  IsIdle:=False;

  Result:=Initialized and (Game<>nil) and (Canvas<>nil);

  if Result then begin
     BoardIsOK     :=CalculateBoardIsOK;
     MaskBitMapIsOK:=CalculateMaskBitMapIsOK;
     Modified      :=Modified                         or
                     (not BoardIsOK)                  or
                     (ReverseMode<>Game.ReverseMode)  or
                     (BoxTargetMaskForDisplay <>
                      Game.BoxTargetMaskForDisplay)   or
                     (BoxCount<>Game.BoxCount)        or
                     (FileName<>Game.FileName)        or
                     (not  MaskBitMapIsOK)            or
                     ((not LegalMoveBitMapsInitialized)
                      and
                      LegalMovesInfo.Enabled)         or
                     (FloorTilesVisible <>
                      Pictures[ptFloor].Visible)      or
                     (not  BoardBackgroundIsOK)       or
                     (not  ReverseModeBackgroundIsOK) or
                     (Pictures[ptScreenBackground].View=ivFloorTile);

     if Modified then
        try
           FileName:=Game.FileName;

           if BoxCount<>Game.BoxCount then begin
              BoxCount:=Game.BoxCount;

              for i:=Succ(BoxCount) to MAX_BOXES do
                  if Sprites[i]<>nil then begin
                     Sprites[i].Enabled:=False;
                     Sprites[i].SetVisibleStateWithoutUpdatingTheDisplay(False);
                     //Sprites[i].Free; Sprites[i]:=nil;
                     end;

              if   Self=MainForm.GameViewer then OnHideSprite:=nil
              else // with an 'OnHideSprite' procedure, the sprites don't create back-buffers, hence, it saves time and space
                   OnHideSprite:=RestoreBackground;

              for i:=1 to BoxCount do
                  MakeSprite(i,Pictures[ptBox],nil,OnHideSprite,0);
              MakeSprite(PLAYER_SPRITE                  ,Pictures[ptPlayer               ],nil,OnHideSprite,MainForm.GamePictures.FrameCount[ptPlayer]);
              MakeSprite(GOAL_SPRITE                    ,Pictures[ptGoal                 ],nil,OnHideSprite,0);
              MakeSprite(WALL_SPRITE                    ,Pictures[ptWall                 ],nil,OnHideSprite,MainForm.GamePictures.FrameCount[ptWall]);
              MakeSprite(FLOOR_SPRITE                   ,Pictures[ptFloor                ],nil,OnHideSprite,0);
              MakeSprite(CURSOR_SPRITE                  ,nil,DrawCursorSprite,nil,0);
              MakeSprite(PLAYER_ANIMATION_SPRITE        ,Pictures[ptPlayerAnimation      ],nil,OnHideSprite,MainForm.GamePictures.FrameCount[ptPlayerAnimation]);
              MakeSprite(PLAYER_ON_GOAL_ANIMATION_SPRITE,Pictures[ptPlayerOnGoalAnimation],nil,OnHideSprite,MainForm.GamePictures.FrameCount[ptPlayerOnGoalAnimation]);
              MakeSprite(BOX_ANIMATION_SPRITE           ,Pictures[ptBoxAnimation         ],nil,OnHideSprite,MainForm.GamePictures.FrameCount[ptBoxAnimation]);
              MakeSprite(BOX_ON_GOAL_ANIMATION_SPRITE   ,Pictures[ptBoxOnGoalAnimation   ],nil,OnHideSprite,MainForm.GamePictures.FrameCount[ptBoxOnGoalAnimation]);
              end;

           if (ColCount<>Game.BoardWidth)  or
              (RowCount<>Game.BoardHeight) or
              ((not LegalMoveBitMapsInitialized) and LegalMovesInfo.Enabled) or
              (FloorTilesVisible<>Pictures[ptFloor].Visible) then begin
              DestroyObjectAnimationPictures(Pictures);

              ColCount :=Game.BoardWidth;
              RowCount :=Game.BoardHeight;
              W:=RectWidth(WindowRect); H:=RectHeight(WindowRect);

              i:=W-2*BorderWidth-6;
              j:=H-2*BorderWidth-8;

              if (i>=ColCount) and(j>=RowCount) then begin
                 ColWidth :=Min(i div ColCount,j div RowCount);

                 if Pictures[ptGoal]<>nil then with Pictures[ptGoal] do
                    if OrgBitMap<>nil then
                       ColWidth:=Min(ColWidth,
                                     Max( OrgBitMap.Width,
                                         (OrgBitMap.Width*MainForm.GamePictures.MaxZoomFactorPct) div 100));

                 if Odd(ColWidth) and (ColWidth>1) and (Self=MainForm.GameViewer) then
                    // kludge: on Win95 machines, the player leaves strange pixels
                    // behind during animation if the width is odd;
                    // it has something to with the fact,
                    // that 24-bit pixels (3 bytes) multiplied by an
                    // odd width results in an odd number of bytes;
                    Dec(ColWidth);

                 //ColWidth:=50;
                 RowHeight:=ColWidth;

                 if Pictures[ptGoal]<>nil then with Pictures[ptGoal] do
                    if OrgBitMap<>nil then
                       if      OrgBitMap.Width>Succ(OrgBitMap.Height) then begin
                               RowHeight:=Max(1,ColWidth*OrgBitMap.Height div OrgBitMap.Width);
                               if Odd(RowHeight) and (RowHeight<ColWidth) and (Self=MainForm.GameViewer) then
                                  Inc(RowHeight);
                               end
                       else if OrgBitMap.Width<Pred(OrgBitMap.Height) then begin
                               ColWidth:=Max(1,RowHeight*OrgBitMap.Width div OrgBitMap.Height);
                               if Odd(ColWidth) and (ColWidth<RowHeight)  and (Self=MainForm.GameViewer) then
                                  Inc(ColWidth);
                               end;

                 if LegalMovesInfo.Enabled and (Self=MainForm.GameViewer) then
                    MakeLegalMoveBitMaps;

                 if Sprites[PLAYER_SPRITE]<> nil then with TSpriteN(Sprites[PLAYER_SPRITE]) do begin
                    SetVisibleStateWithoutUpdatingTheDisplay(False);
                    if   HasPlayerDirectionAnimation then
                         FrameIndex:=Ord(Game.PlayerDirection(Game.History.Count))
                    else FrameIndex:=0;
                    end;

                 if Sprites[WALL_SPRITE]<>nil then with TSpriteN(Sprites[WALL_SPRITE]) do begin
                    SetVisibleStateWithoutUpdatingTheDisplay(False);
                    if   (    MainForm.GamePictures.WallType=wtTiledWall) and
                         (not MainForm.GamePictures.IsASeamlessWall) then begin
                         FrameCount:=1; FrameIndex:=0;
                         end
                    else FrameCount:=WALL_TILE_COUNT;
                    end;

                 for p:=ptPlayer to ptPlayerOnGoal do
                     if Pictures[p]<>nil then with Pictures[p] do begin
                        OldMasked:=Masked; OldMaskColor:=MaskBitMapColor;
                        if LegalMovesInfo.Enabled and (Self=MainForm.GameViewer) and
                           HasPlayerDirectionAnimation and Masked and (AntiAliasing=aaFilter) then begin
                           // 'MakeLegalMoveBitMaps' has created a single-frame flood-filled version of the original bitmap;
                           // destroy it, so a new all-frames version will be created when the bitmap is resized
                           OrgBitMapWithFloodFilledMaskedPixels.Free; OrgBitMapWithFloodFilledMaskedPixels:=nil;
                           end;
{                       masking with the mask color from a transparent floor/goal background is disabled because it's a dilemma; sometimes is produces the wanted effect, sometimes it doesn't
                        if   not Masked then begin
                             if   p=ptPlayer then q:=ptFloor
                             else q:=ptGoal;
                             if (Pictures[q]<>nil) and Pictures[q].Visible and Pictures[q].Masked then begin
                                Masked:=True; MaskBitMapColor:=Pictures[q].MaskBitMapColor;
                                end;
                             end;
}
                        if   HasPlayerDirectionAnimation and (Sprites[PLAYER_SPRITE]<>nil) then
                             ResizeFrames(ColWidth,RowHeight,NUMBER_OF_DIRECTIONS,nil)
                        else Resize(ColWidth,RowHeight);
                        Masked:=OldMasked; MaskBitMapColor:=OldMaskColor;
                        end;

                 for p:=ptBox to ptBoxOnGoal do
                     if Pictures[p]<>nil then with Pictures[p] do begin
                        OldMasked:=Masked; OldMaskColor:=MaskBitMapColor;
{                       masking with the mask color from a transparent floor/goal background is disabled because it's a dilemma; sometimes is produces the wanted effect, sometimes it doesn't
                        if   not Masked then begin
                             if   p=ptBox then q:=ptFloor
                             else q:=ptGoal;
                             if (Pictures[q]<>nil) and Pictures[q].Visible and Pictures[q].Masked then begin
                                Masked:=True; MaskBitMapColor:=Pictures[q].MaskBitMapColor;
                                end;
                             end;
}
                        Resize(ColWidth,RowHeight);
                        Masked:=OldMasked; MaskBitMapColor:=OldMaskColor;
                        end;

                 for p:=ptGoal to ptFloor do
                     if (p<>ptWall) and (Pictures[p]<>nil) then with Pictures[p] do
                        Resize(ColWidth,RowHeight);

                 if Pictures[ptWall]<>nil then with Pictures[ptWall] do
                    if   Sprites[WALL_SPRITE]<>nil then
                         ResizeFrames(ColWidth,RowHeight,TSpriteN(Sprites[WALL_SPRITE]).FrameCount,nil)
                    else Resize(ColWidth,RowHeight);

                 if (Pictures[ptFloor]<>nil) and  Pictures[ptFloor].Visible and
                    (Pictures[ptGoal ]<>nil) and (Pictures[ptGoal ].BitMap<>nil) and
                    ((Self=MainForm.GameViewer)
                     or
                     Pictures[ptFloor].Masked
                     or
                     Pictures[ptGoal ].Masked
                    ) then begin
                    for p:=ptPlayerMoveAnimation to ptBoxOnGoalMoveAnimation do begin
                        q:=TPictureType(Ord(ptPlayer)+Ord(p)-Ord(ptPlayerMoveAnimation));
                        //if (q=ptPlayerOnGoal) and UsePlayerImageForMoveAnimationAlsoForGoalSquares then // 'True': use "Player" image for move animation, also when the player is on a goal square
                        //   q:=ptPlayer;
                        //if (q=ptBoxOnGoal) and UseBoxImageForMoveAnimationAlsoForGoalSquares then       // 'True': use "Box"    image for move animation, also when the box    is on a goal square
                        //   q:=ptBox;

                        if not Pictures[q].Masked then with Pictures[p] do begin // 'True': the player or box object isn't masked by a color; instead the object is drawn on top of a floor square or a goal square
                           MaskBitMapPct:=Pictures[q].MaskBitMapPct;
                           Masked       :=Pictures[q].Masked;
                           BitMapRect   :=Pictures[q].BitMapRect;

                           if BitMapCreate(BitMap,Pictures[q].BitMap.Width,Pictures[q].BitMap.Height) then begin
                              BitMap.Canvas.Draw(0,0,Pictures[q].BitMap);
                              r:=p; // use separate images for "stand still" and "move animation"; 'r': move animation image
                              end
                           else begin
                              BitMap.Free; BitMap:=nil; // clear the move animation image; (there shouldn't be any at this time, but better safe than sorry)
                              r:=TPictureType(Ord(ptPlayer)+Ord(p)-Ord(ptPlayerMoveAnimation)); // use the same image for "stand still" and "move animation"
                              q:=r; // 'q': source image; 'r': destination image
                              end;

                           with Pictures[r] do // 'r': destination image
                             if   (q=ptPlayer) or (q=ptBox) then // 'q': source image
                                  MaskBackgroundBitMap(Pictures[ptFloor].BitMap)
                             else MaskBackgroundBitMap(Pictures[ptGoal ].BitMap);
                           end;
                        end;

                    with Pictures[ptGoal] do
                      if not Masked then
                         MaskBackgroundBitMap(Pictures[ptFloor].BitMap);
                    end;

                 for p:=ptPlayerMoveAnimation to ptBoxOnGoalMoveAnimation do with Pictures[p] do
                     if not Assigned(BitMap) then begin
                        // use "stand still" image for "move object" animation;
                        q:=TPictureType(Ord(ptPlayer)+Ord(p)-Ord(ptPlayerMoveAnimation));
                        if (q=ptPlayerOnGoal) and UsePlayerImageForMoveAnimationAlsoForGoalSquares and // 'True': use "Player" image for move animation, also when the player is on a goal square
                           Assigned(Pictures[ptPlayer].BitMap) then q:=ptPlayer;
                        if (q=ptBoxOnGoal) and UseBoxImageForMoveAnimationAlsoForGoalSquares and       // 'True': use "Box"    image for move animation, also when the box    is on a goal square
                           Assigned(Pictures[ptBox].BitMap) then q:=ptBox;

                        if   Assigned(Pictures[q].BitMap) then begin
                             BitMap    :=Pictures[q].BitMap;
                             MaskBitMap:=Pictures[q].MaskBitMap;
                             BitMapRect:=Pictures[q].BitMapRect;
                             end
                        else raise Exception.Create(TEXT_TASK_FAILED); // the "stand still" image didn't have a bitmap either
                        end;

                 if LegalMovesInfo.PlayerAnimationEnabled or SolutionsInfo.PlayerAnimationEnabled then
                    for p:=ptPlayerAnimation to ptPlayerOnGoalAnimation do
                        if (Pictures[p]<>nil) and (Pictures[p].OrgBitMap<>nil) then with Pictures[p] do
                           ResizeFrames(ColWidth,RowHeight,OrgBitMap.Width div OrgBitMap.Height,nil);

                 if LegalMovesInfo.BoxAnimationEnabled or SolutionsInfo.BoxAnimationEnabled then
                    for p:=ptBoxAnimation to ptBoxOnGoalAnimation do
                        if (Pictures[p]<>nil) and (Pictures[p].OrgBitMap<>nil) then with Pictures[p] do
                           ResizeFrames(ColWidth,RowHeight,OrgBitMap.Width div OrgBitMap.Height,nil);

                 if   (Pictures[ptPlayer]<>nil) and (Pictures[ptPlayer].BitMap<>nil) then
                      for d:=Low(d) to High(d) do with Pictures[ptPlayer].BitMap do
                          if   HasPlayerDirectionAnimation then
                               if   Game.PlayerDirectionAnimationEnabled then
                                    PlayerDirectionRect[d]:=CellToRect(Ord(d),0,ColWidth,RowHeight)
                               else PlayerDirectionRect[d]:=CellToRect(Ord(DEFAULT_PLAYER_DIRECTION),0,ColWidth,RowHeight)
                          else PlayerDirectionRect[d]:=Rect(0,0,ColWidth,RowHeight)
                 else FillChar(PlayerDirectionRect,SizeOf(PlayerDirectionRect),0);

                 Width     :=ColCount*ColWidth +2*BorderWidth;
                 Height    :=RowCount*RowHeight+2*BorderWidth;

                 if (Self=MainForm.GameViewer) and
                    (not MainForm.MultiView.Items.IsEmpty) and
                    (MainForm.MultiView.Items.Count>1) then begin // top-left justified boards for multiple views
                    Left   :=WindowRect.Left+BorderWidth;
                    Top    :=WindowRect.Top +BorderWidth;
                    end
                 else begin // centered board
                    Left   :=WindowRect.Left+((W-Width ) div 2);
                    Top    :=WindowRect.Top +((H-Height) div 2);
                    end;

                 BoardRect :=Rect(Left,Top,Left+Width,Top+Height);

                 for i:=Low(Sprites) to High(Sprites) do
                     if (Sprites[i]<>nil) and Sprites[i].Enabled then
                        Sprites[i].Resize(ColWidth,RowHeight);

                 if (Pictures[ptWall]<>nil) and (Pictures[ptWall].BitMap<>nil) then
                    with Pictures[ptWall].BitMap do with OuterWallTrimming do begin
                      if Assigned(Pictures[ptWall].OrgBitMap) then begin
                         W:=Pictures[ptWall].OrgBitMap.Width;
                         H:=Pictures[ptWall].OrgBitMap.Height;
                         end
                      else begin
                         W:=1; H:=1;
                         end;
                      Left     :=MainForm.GamePictures.OuterWallTrimming.Left   *Width  div W;
                      Top      :=MainForm.GamePictures.OuterWallTrimming.Top    *Height div H;
                      Right    :=MainForm.GamePictures.OuterWallTrimming.Right  *Width  div W;
                      Bottom   :=MainForm.GamePictures.OuterWallTrimming.Bottom *Height div H;

                      WallCap  :=MainForm.GamePictures.WallCap;
                      if   WallCap.X<0 then // '<0': default offset = midways between the squares
                           WallCap.X:=ColWidth  div 2
                      else WallCap.X:=Max(0,Min(ColWidth ,ColWidth -(WallCap.X*Width  div W)));
                      if   WallCap.Y<0 then // '<0': default offset = midways between the squares
                           WallCap.Y:=RowHeight div 2
                      else WallCap.Y:=Max(0,Min(RowHeight,RowHeight-(WallCap.Y*Height div H)));
                      end
                 else begin
                   FillChar(OuterWallTrimming,SizeOf(OuterWallTrimming),0);
                   FillChar(WallCap,SizeOf(WallCap),0);
                   end;

                 MaskBitMapIsOK:=False;

                 with Pictures[ptBox] do begin
                   //BitMapDump(BitMap);
                   //BitMapDump(MaskBitMap);
                   end;

{
                 BitMapDump(Pictures[ptWall  ].BitMap);
                 BitMapDump(Pictures[ptBox   ].BitMap);
                 BitMapDump(Pictures[ptGoal  ].BitMap);
                 BitMapDump(Pictures[ptPlayer].BitMap);
}
                 end;
              end
           else begin
              for i:=1 to BoxCount do // ensure that box-sprites have the proper size;
                  if Sprites[i]<>nil then Sprites[i].Resize(ColWidth,RowHeight);
              if Sprites[CURSOR_SPRITE]<>nil then Sprites[CURSOR_SPRITE].Resize(ColWidth,RowHeight);
              end;

           if MaskBitMapIsOK then begin
              if   Pictures[ptBoardBackground       ].Visible
                   or
                   Pictures[ptFloor                 ].Visible
                   or
                   (Game.ReverseMode
                    and
                    Pictures[ptReverseModeBackground].Visible
                   ) then {}                                            // use current mask-bitmap
              else
                 DestroyBoardMaskBitMap;                                // the mask isn't used anymore
              end
           else                                                         // board topology has changed
              DestroyBoardMaskBitMap;                                   // destroy old mask, if any

           Board:=Game.Board;                                           // save board background-squares info

           if ReverseMode<>Game.ReverseMode then begin
              ReverseMode:=Game.ReverseMode;

              if Self=MainForm.GameViewer then begin
                 MainForm.Menu  .ReverseMode:=ReverseMode;
                 MainForm.Status.ReverseMode:=ReverseMode;
                 end;

              if MainForm .GamePictures.Transparency   [ptGoal] and
                 (MainForm.GamePictures.TransparencyPct[ptGoal]<>0) then
                 FloorTilesVisible:=not Pictures[ptFloor].Visible; // ensure that both the board-background and the reversemode-background are recreated so target-squares are drawn correctly
              end;

           RefreshScreenBackground :=
             Assigned(Pictures[ptScreenBackground]) and
             (Pictures[ptScreenBackground].View=ivFloorTile) and
             Assigned(Pictures[ptScreenBackground].BitMap) and
             Pictures[ptScreenBackground].Visible and
             (RectWidth(BoardRect)>0) and
             (RectHeight(BoardRect)>0) and
             Assigned(MainForm.Status) and
             (MainForm.Status.Panels[spBrowse].RectForm.Top>=MainForm.Status.Panels[spBrowse].RectForm.Bottom); // ">=": no game browsing in progress

           if RefreshScreenBackground then with Pictures[ptScreenBackground] do
              SetView(View,RectWidth(BitMapRect),RectHeight(BitMapRect),Color);

           if not BoardBackgroundIsOK then
              MakeBackground(ptBoardBackground);

           if not ReverseModeBackgroundIsOK then
              MakeBackground(ptReverseModeBackground);

           BoxTargetMaskForDisplay:=Game.BoxTargetMaskForDisplay;
           FloorTilesVisible      :=Pictures[ptFloor].Visible;

           CalculateSpecialBackgroundSquares;

           if RefreshScreenBackground and (Self=MainForm.GameViewer) then
              with Pictures[ptScreenBackground] do begin
                if      (not Assigned(MainForm.MultiView)) or MainForm.MultiView.IsEmpty then begin
                        Draw(0,0,MainForm.Image1.Picture.BitMap.Canvas);
                        end
                else if Assigned(MainForm.MultiView.Selected) then begin
                        DrawRect(0,0,
                                 Rect(0,0,Min(MainForm.MultiView.ClippingRect.Right, MainForm.MultiView.ClippingRect.Left+MULTI_VIEW_ITEM_BOARD_BORDER_SIZE-4),MainForm.Image1.Picture.BitMap.Height),
                                 MainForm.Image1.Picture.BitMap.Canvas); // the area to the left of the boards
                        DrawRect(     MainForm.MultiView.ClippingRect.Left,Max(0,MainForm.MultiView.ClippingRect.Bottom-MULTI_VIEW_ITEM_BOARD_BORDER_SIZE+2),
                                 Rect(MainForm.MultiView.ClippingRect.Left,Max(0,MainForm.MultiView.ClippingRect.Bottom-MULTI_VIEW_ITEM_BOARD_BORDER_SIZE+2),MainForm.Image1.Picture.BitMap.Width,MainForm.Image1.Picture.BitMap.Height),
                                 MainForm.Image1.Picture.BitMap.Canvas); // the area below the boards
                        end
                     else RefreshScreenBackground:=False;

                if RefreshScreenBackground then begin
                   if MainForm.Menu<>nil then begin
                      MainForm.Menu.MakeButtons;
                      MainForm.Menu.Invalidate;
                      end;
                   if MainForm.Status<>nil then MainForm.Status.Invalidate;
                   end;
                end;

           Show;

           if Assigned(MainForm.MultiView) and
              Assigned(MainForm.MultiView.Selected) and
              (Self=MainForm.GameViewer) then
              with MainForm.MultiView.Selected do begin
                ShowPanels(Panels[mvipCaption].Caption);
                DrawFrame;
                end;

        except on E:Exception do begin Result:=Error(E.Message,''); Clear; end;
        end
     else // not modified
        ShowBoard(True);
     end;
end;

function  TGameViewer.LoadPictures:Boolean;
var p:TPictureType;
begin
  Result:=MainForm.GamePictures.Initialized;
  Clear;
  if Result then
     try    for p:=Low(p) to High(p) do begin
                if      Self=MainForm.GameViewer then
                        Pictures[p]                :=MainForm.GamePictures.Pictures[p]
                else if (p<ptPlayerAnimation) or (p>ptBoxOnGoalAnimation) then begin // the main window is the only one using highlight animation
                        if Pictures[p]=nil then Pictures[p]:=TPict.Create;
                        if Pictures[p].BitMap=nil then with Pictures[p] do begin
                           BitMap:=TBitMap.Create; BitMap.PixelFormat:=pf24Bit;
                           end;

                        // caution: 'OrgBitMap' and 'Pict' belong to 'MainForm.GamePictures.Pictures[]
                        Pictures[p].OrgBitMap      :=MainForm.GamePictures.Pictures[p].OrgBitMap;
                        Pictures[p].Pict           :=MainForm.GamePictures.Pictures[p].Pict;

                        Pictures[p].Antialiasing   :=MainForm.GamePictures.Pictures[p].Antialiasing;
                        Pictures[p].Color          :=MainForm.GamePictures.Pictures[p].Color;
                        Pictures[p].FileName       :=MainForm.GamePictures.Pictures[p].FileName;
                        Pictures[p].KeepAspectRatio:=MainForm.GamePictures.Pictures[p].KeepAspectRatio;
                        Pictures[p].MaskBitMapColor:=MainForm.GamePictures.Pictures[p].MaskBitMapColor;
                        Pictures[p].MaskBitMapPct  :=MainForm.GamePictures.Pictures[p].MaskBitMapPct;
                        Pictures[p].Masked         :=MainForm.GamePictures.Pictures[p].Masked;
                        Pictures[p].Transparent    :=MainForm.GamePictures.Pictures[p].Transparent;
                        Pictures[p].View           :=MainForm.GamePictures.Pictures[p].View;
                        Pictures[p].Visible        :=MainForm.GamePictures.Pictures[p].Visible;
                        end;
                end;

            LegalMoveBitMapsInitialized:=False;
            HasPlayerDirectionAnimation:=(MainForm.GamePictures.FrameCount[ptPlayer      ]=NUMBER_OF_DIRECTIONS) and
                                         (MainForm.GamePictures.FrameCount[ptPlayerOnGoal]=NUMBER_OF_DIRECTIONS);
            IsASeamlessWall:=MainForm.GamePictures.IsASeamlessWall;
            UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares
                                                            :=MainForm.GamePictures.UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares;
            UseBoxImageForMoveAnimationAlsoForGoalSquares   :=MainForm.GamePictures.UseBoxImageForMoveAnimationAlsoForGoalSquares;
            UseFloorTilesAsBackground                       :=MainForm.GamePictures.UseFloorTilesAsBackground and IsFloorTileVisible;
            UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares
                                                            :=MainForm.GamePictures.UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares;
            UsePlayerImageForMoveAnimationAlsoForGoalSquares:=MainForm.GamePictures.UsePlayerImageForMoveAnimationAlsoForGoalSquares;
            fInitialized:=True; // all pictures have been loaded
     except on E:Exception do begin Result:=Error(E.Message,''); Clear; end;
     end;
end;

function  TGameViewer.LoadSettingsFromIniFile(const IniFile:TIniFile; const Section:String):Boolean;
begin
  Result:=True;
  fBorderWidth:=IniFile.ReadInteger(Section,'BorderWidth',BorderWidth);
  if (BorderWidth<0) or (BorderWidth>MAX_BORDER_WIDTH) then fBorderWidth:=DEFAULT_BORDER_WIDTH;
  with LegalMovesInfo do begin
    BoxCursor.Color:=TColor(IniFile.ReadInteger(Section,'ShowLegalMovesBoxCursorColor',Integer(BoxCursor.Color)));
    BoxCursor.Enabled:=IniFile.ReadBool(Section,'ShowLegalMovesBoxCursorEnabled',BoxCursor.Enabled);
    BoxCursor.PenWidth:=Max(1,Min(MAX_LEGAL_MOVES_CURSOR_PEN_WIDTH,IniFile.ReadInteger(Section,'ShowLegalMovesBoxCursorPenWidth',BoxCursor.PenWidth)));
    BoxCursor.ShadowEnabled:=IniFile.ReadBool(Section,'ShowLegalMovesBoxCursorShadowEnabled',BoxCursor.ShadowEnabled);
    BoxCursor.ShadowColor:=TColor(IniFile.ReadInteger(Section,'ShowLegalMovesBoxCursorShadowColor',Integer(BoxCursor.ShadowColor)));
    BoxCursor.Size:=Max(0,Min(100,IniFile.ReadInteger(Section,'ShowLegalMovesBoxCursorSize',BoxCursor.Size)));

    PlayerCursor.Color:=TColor(IniFile.ReadInteger(Section,'ShowLegalMovesPlayerCursorColor',Integer(PlayerCursor.Color)));
    PlayerCursor.Enabled:=IniFile.ReadBool(Section,'ShowLegalMovesPlayerCursorEnabled',PlayerCursor.Enabled);
    PlayerCursor.PenWidth:=Max(1,Min(MAX_LEGAL_MOVES_CURSOR_PEN_WIDTH,IniFile.ReadInteger(Section,'ShowLegalMovesPlayerCursorPenWidth',PlayerCursor.PenWidth)));
    PlayerCursor.ShadowEnabled:=IniFile.ReadBool(Section,'ShowLegalMovesPlayerCursorShadowEnabled',PlayerCursor.ShadowEnabled);
    PlayerCursor.ShadowColor:=TColor(IniFile.ReadInteger(Section,'ShowLegalMovesPlayerCursorShadowColor',Integer(PlayerCursor.ShadowColor)));
    PlayerCursor.Size:=Max(0,Min(100,IniFile.ReadInteger(Section,'ShowLegalMovesPlayerCursorSize',PlayerCursor.Size)));

    DeadlocksColor:=TColor(IniFile.ReadInteger(Section,'ShowLegalMovesDeadlocksColor',Integer(DeadlocksColor)));
    DeadlocksEnabled:=IniFile.ReadBool(Section,'ShowLegalMovesDeadlocksEnabled',DeadlocksEnabled);
    JumpMovesBackgroundColor:=TColor(IniFile.ReadInteger(Section,'ShowLegalMovesJumpMovesBackgroundColor',Integer(JumpMovesBackgroundColor)));
    JumpMovesEnabled:=IniFile.ReadBool(Section,'ShowLegalMovesJumpMovesEnabled',JumpMovesEnabled);
    JumpMovesTextColor:=TColor(IniFile.ReadInteger(Section,'ShowLegalMovesJumpMovesTextColor',Integer(JumpMovesTextColor)));
    JumpMovesTextShadowColor:=TColor(IniFile.ReadInteger(Section,'ShowLegalMovesJumpMovesTextShadowColor',Integer(JumpMovesTextShadowColor)));
    Enabled:=IniFile.ReadBool(Section,'ShowLegalMovesEnabled',Enabled);
    Size:=Max(0,Min(100,IniFile.ReadInteger(Section,'ShowLegalMovesSize',Size)));
    TransparentImage:=IniFile.ReadBool(Section,'ShowLegalMovesTransparentImage',TransparentImage);
    Transparency:=Max(0,Min(100,IniFile.ReadInteger(Section,'ShowLegalMovesTransparency',Transparency)));
    PlayerAnimationEnabled:=IniFile.ReadBool(Section,'ShowLegalMovesPlayerAnimationEnabled',PlayerAnimationEnabled);
    BoxAnimationEnabled:=IniFile.ReadBool(Section,'ShowLegalMovesBoxAnimationEnabled',BoxAnimationEnabled);
    SolutionsInfo.PlayerAnimationEnabled:=IniFile.ReadBool(Section,'SolutionsPlayerAnimationEnabled',SolutionsInfo.PlayerAnimationEnabled);
    SolutionsInfo.BoxAnimationEnabled:=IniFile.ReadBool(Section,'SolutionsBoxAnimationEnabled',SolutionsInfo.BoxAnimationEnabled);
    end;
end;

procedure TGameViewer.MakeSprite(Index:Integer; Pict:TPict; OnDrawSprite__:TDrawSpriteEvent; OnHideSprite__:THideSpriteEvent; FrameCount__:Integer);
begin
  if Sprites[Index]=nil then
     if   (Index=PLAYER_SPRITE) or (Index=WALL_SPRITE) or
          ((Index>=PLAYER_ANIMATION_SPRITE) and (Index<=BOX_ON_GOAL_ANIMATION_SPRITE)) then begin
          Sprites[Index]:=TSpriteN.Create(Canvas,Pict,OnDrawSprite__,OnHideSprite__);
          if Sprites[Index]<>nil then TSpriteN(Sprites[Index]).FrameCount:=FrameCount__;
          end
     else Sprites[Index]:=TSprite1.Create(Canvas,Pict,OnDrawSprite__,OnHideSprite__)
  else begin
     Sprites[Index].SetVisibleStateWithoutUpdatingTheDisplay(False);
     Sprites[Index].Canvas      :=Canvas;
     Sprites[Index].Pict        :=Pict;
     Sprites[Index].OnDrawSprite:=OnDrawSprite__;
     Sprites[Index].OnHideSprite:=OnHideSprite__;
     Sprites[Index].Enabled     :=True;
     if Sprites[Index] is TSpriteN then TSpriteN(Sprites[Index]).FrameCount:=FrameCount__;
     end;
end;

procedure TGameViewer.MouseToCell(X,Y:Integer; var ACol,ARow:Integer);
begin
  ACol:=X-Left-BorderWidth;
  if ACol>=0 then begin
     ACol:=ACol div ColWidth;

     ARow:=Y-Top -BorderWidth;
     if ARow>=0 then
        ARow:=ARow div RowHeight
     else begin
        ACol:=MaxInt-1; ARow:=MaxInt-1;
        end;
     end
  else begin
     ACol:=MaxInt-1; ARow:=MaxInt-1;
     end;
end;

procedure TGameViewer.OnIdle;
var BoxNo,FrameNo:Integer; TimeNow:TTimeMS;

  procedure ShowPlayerAnimation(FrameNo:Integer);
  var i,SpriteNo:Integer; Sprite:TSprite1;
  begin
    with LegalMovesInfo do with Game.PlayerPos do begin
      i:=Game.Board[x,y];
      if (i and BoxTargetMaskForDisplay)=0 then SpriteNo:=PLAYER_ANIMATION_SPRITE
      else SpriteNo:=PLAYER_ON_GOAL_ANIMATION_SPRITE;
      Sprite:=Sprites[PLAYER_SPRITE];
      if Sprite<>nil then begin
         if Assigned(TSpriteN(Sprites[SpriteNo]).Pict.MaskBitMap) then // 'True': the animation-images contain masked pixels, hence, it's necessary to restore the background between each frame
            Sprite.SetVisibleStateWithoutUpdatingTheDisplay(True);
         if Sprite.Visible then Sprite.Hide; // hide the normal player sprite
         TSpriteN(Sprites[SpriteNo]).DrawFrame(Sprite.R.Left,Sprite.R.Top,{PLAYER_AND_BOX_FRAME_COUNT-1-}FrameNo);
         if PlayerCursor.Enabled and (not (Game.GameState=gsSolved)) then
            ShowCursor(x,y,PlayerCursor.Size,PlayerCursor.Size,PlayerCursor.Color,PlayerCursor);
         end;
      end;
  end;

  procedure ShowBoxAnimation(BoxNo,FrameNo:Integer);
  var i,SpriteNo:Integer; Sprite:TSprite1;
  begin
    with LegalMovesInfo do with Game.BoxPos[BoxNo] do begin
      i:=Game.Board[x,y];
      if (i and BoxTargetMaskForDisplay)=0 then SpriteNo:=BOX_ANIMATION_SPRITE
      else SpriteNo:=BOX_ON_GOAL_ANIMATION_SPRITE;
      Sprite:=Sprites[BoxNo];
      if Sprite<>nil then begin
         if Assigned(TSpriteN(Sprites[SpriteNo]).Pict.MaskBitMap) then // 'True': the animation-images contain masked pixels, hence, it's necessary to restore the background between each frame
            Sprite.SetVisibleStateWithoutUpdatingTheDisplay(True);
         if Sprite.Visible then Sprite.Hide; // hide the normal box sprite
         TSpriteN(Sprites[SpriteNo]).DrawFrame(Sprite.R.Left,Sprite.R.Top,{PLAYER_AND_BOX_FRAME_COUNT-1-}FrameNo);
         end;
      end;
  end;

begin // OnIdle
  with LegalMovesInfo do
    if (Mask<>0) and
       (PlayerAnimationEnabled or BoxAnimationEnabled or SolutionsInfo.PlayerAnimationEnabled or SolutionsInfo.BoxAnimationEnabled) and
       (not IsIdle) and
       (Screen.ActiveForm=MainForm) then begin
       IsIdle:=True;
       TimeNow:=GetTickCount mod 1000;
       if   TimeNow>=StartTime then Dec(TimeNow,StartTime)
       else TimeNow:=1000-StartTime+TimeNow; // clock wrap around
       FrameNo:=(TimeNow*PLAYER_AND_BOX_FRAME_COUNT) div 1000;

       if      PlayerAnimationEnabled and (Mask=PLAYER_LEGAL_MOVE) then
               ShowPlayerAnimation(FrameNo)
       else if BoxAnimationEnabled and (Mask=BOX_LEGAL_MOVE) and
               (ItemIndex>=1) and (ItemIndex<=Game.BoxCount) then with Game.BoxPos[ItemIndex] do begin
               ShowBoxAnimation(ItemIndex,FrameNo);
               if BoxCursor.Enabled and (not (Game.GameState=gsSolved)) then
                  ShowCursor(x,y,BoxCursor.Size,BoxCursor.Size,BoxCursor.Color,BoxCursor);
               end
       else if (Mask and SQUARE_SET)<>0 then begin
               if ((Game.GameState<>gsSolved) and BoxAnimationEnabled)
                  or
                  ((Game.GameState= gsSolved) and SolutionsInfo.BoxAnimationEnabled) then
                  for BoxNo:=1 to Game.BoxCount do with Game.BoxPos[BoxNo] do
                      if (Game.Board[x,y] and Mask)=Mask then begin
                         ShowBoxAnimation(BoxNo,FrameNo);
                         if BoxCursor.Enabled and (not (Game.GameState=gsSolved)) then
                            ShowCursor(x,y,ColWidth div 2,RowHeight div 2,HighlightedSquareCursorColor,BoxCursor);
                         end;
               if (Game.GameState=gsSolved) and SolutionsInfo.PlayerAnimationEnabled then
                  ShowPlayerAnimation(FrameNo);
               end;

       IsIdle:=False;
       //MainForm.Status.Hint:='Idle '+IntToStr(FrameNo);
       end;
end;

procedure TGameViewer.PutTilesOnTheBoard(SpriteNo,Mask:Integer; Transparent:Boolean; TransparencyPct:Integer; BitMap:TBitMap);
var Col,Row,X,Y,SquareSpriteNo,SquareTransparencyPct:Integer;
begin
  if (BitMap                <>nil) and
     (Sprites[SpriteNo]     <>nil) and
     (Sprites[SpriteNo].Pict<>nil) then begin
     with MainForm.GamePictures.OuterWallTrimming do
       if (SpriteNo=FLOOR_SPRITE) and ((Mask and Wall)<>0)
          //and
          //((Left+Top+Right+Bottom)<>0)
          then
          Dec(Mask,Wall);
     for Col:=1 to ColCount do
         for Row:=1 to RowCount do
             if (Board[Col,Row] and Mask)<>0 then begin
                CellToPos(Pred(Col),Pred(Row),X,Y);
                if BitMap.Width=Self.Width then Dec(X,Self.Left)
                else Dec(X,WindowRect.Left);
                if BitMap.Height=Self.Height then Dec(Y,Self.Top)
                else Dec(Y,WindowRect.Top);
                SquareTransparencyPct:=TransparencyPct;
                if   (not Transparent) or (TransparencyPct=0) then begin
                     Sprites[SpriteNo].Pict.Draw(X,Y,BitMap.Canvas);

                     if   (SpriteNo=FLOOR_SPRITE) and
                          MainForm.GamePictures.Transparency[ptGoal] and
                          (MainForm.GamePictures.TransparencyPct[ptGoal]<>0) and
                          ((Board[Col,Row] and Game.BoxTargetMaskForDisplay)<>0) then begin
                          // draw a transparent goal tile on top of the floor
                          SquareSpriteNo:=GOAL_SPRITE;
                          SquareTransparencyPct:=MainForm.GamePictures.TransparencyPct[ptGoal];
                          end
                     else SquareSpriteNo:=Low(Sprites)-1;
                     end
                else SquareSpriteNo:=SpriteNo;

                if (SquareSpriteNo=FLOOR_SPRITE) and
                   ((Board[Col,Row] and Game.BoxTargetMaskForDisplay)<>0) and
                   MainForm.GamePictures.Transparency[ptGoal] and
                   (MainForm.GamePictures.TransparencyPct[ptGoal]<>0) and
                   (Sprites[GOAL_SPRITE]<>nil) and
                   (Sprites[GOAL_SPRITE].Pict<>nil) then begin
                   // assume that transparent goal tiles substitute the floor tiles
                   SquareSpriteNo:=GOAL_SPRITE;
                   SquareTransparencyPct:=MainForm.GamePictures.TransparencyPct[ptGoal];
                   end;

                if SquareSpriteNo>=Low(Sprites) then with Sprites[SquareSpriteNo] do begin
                   R:=Rect(X,Y,X+PictureSizeRect.Right,Y+PictureSizeRect.Bottom);
                   BitMapAlphaBlendRect(BitMap,Pict.BitMap,BitMap,
                                        R,PictureSizeRect,R,
                                        SquareTransparencyPct,
                                        Pict.Masked,
                                        clBlack, // not 'RGBToColor(Pict.MaskBitMapColor'; if the image originally had a non-black mask color, then it has been changed to black at this time
                                        Pict.MaskBitMapPct);
                   end;
                end;
     end;
end;

procedure TGameViewer.RestoreBackground(const Rect__:TRect);
begin
  if   TopLayerBitMap<>nil then
       if   TopLayerBitMap=Pictures[ptScreenBackground].BitMap then
            Canvas.CopyRect(Rect__,TopLayerBitMap.Canvas,Rect__)
       else Canvas.CopyRect(Rect__,TopLayerBitMap.Canvas,Rect(Rect__.Left-Left,Rect__.Top-Top,Rect__.Right-Left,Rect__.Bottom-Top))
  else if   BackBitMap<>nil then
            Canvas.CopyRect(Rect__,BackBitMap.Canvas,Rect(Rect__.Left-WindowRect.Left,Rect__.Top-WindowRect.Top,Rect__.Right-WindowRect.Left,Rect__.Bottom-WindowRect.Top))
       else with Canvas do begin // no screen background picture
            Brush.Color:=Pictures[ptScreenBackground].Color;
            FillRect(Rect__); // clear rectangle
            end;
end;

function  TGameViewer.SaveSettingsToIniFile(const IniFile:TIniFile; const Section:String):Boolean;
begin
  Result:=True;
  IniFile.WriteInteger(Section,'BorderWidth',BorderWidth);
  with LegalMovesInfo do begin
    IniFile.WriteInteger(Section,'ShowLegalMovesBoxCursorColor',Integer(BoxCursor.Color));
    IniFile.WriteBool(Section,'ShowLegalMovesBoxCursorEnabled',BoxCursor.Enabled);
    IniFile.WriteInteger(Section,'ShowLegalMovesBoxCursorPenWidth',BoxCursor.PenWidth);
    IniFile.WriteBool(Section,'ShowLegalMovesBoxCursorShadowEnabled',BoxCursor.ShadowEnabled);
    IniFile.WriteInteger(Section,'ShowLegalMovesBoxCursorShadowColor',Integer(BoxCursor.ShadowColor));
    IniFile.WriteInteger(Section,'ShowLegalMovesBoxCursorSize',BoxCursor.Size);

    IniFile.WriteInteger(Section,'ShowLegalMovesPlayerCursorColor',Integer(PlayerCursor.Color));
    IniFile.WriteBool(Section,'ShowLegalMovesPlayerCursorEnabled',PlayerCursor.Enabled);
    IniFile.WriteInteger(Section,'ShowLegalMovesPlayerCursorPenWidth',PlayerCursor.PenWidth);
    IniFile.WriteBool(Section,'ShowLegalMovesPlayerCursorShadowEnabled',PlayerCursor.ShadowEnabled);
    IniFile.WriteInteger(Section,'ShowLegalMovesPlayerCursorShadowColor',Integer(PlayerCursor.ShadowColor));
    IniFile.WriteInteger(Section,'ShowLegalMovesPlayerCursorSize',PlayerCursor.Size);

    IniFile.WriteInteger(Section,'ShowLegalMovesDeadlocksColor',Integer(DeadlocksColor));
    IniFile.WriteBool(Section,'ShowLegalMovesDeadlocksEnabled',DeadlocksEnabled);
    IniFile.WriteInteger(Section,'ShowLegalMovesJumpMovesBackgroundColor',Integer(JumpMovesBackgroundColor));
    IniFile.WriteBool(Section,'ShowLegalMovesJumpMovesEnabled',JumpMovesEnabled);
    IniFile.WriteInteger(Section,'ShowLegalMovesJumpMovesTextColor',Integer(JumpMovesTextColor));
    IniFile.WriteInteger(Section,'ShowLegalMovesJumpMovesTextShadowColor',Integer(JumpMovesTextShadowColor));
    IniFile.WriteBool(Section,'ShowLegalMovesEnabled',Enabled);
    IniFile.WriteInteger(Section,'ShowLegalMovesSize',Size);
    IniFile.WriteBool(Section,'ShowLegalMovesTransparentImage',TransparentImage);
    IniFile.WriteInteger(Section,'ShowLegalMovesTransparency',Transparency);
    IniFile.WriteBool(Section,'ShowLegalMovesPlayerAnimationEnabled',PlayerAnimationEnabled);
    IniFile.WriteBool(Section,'ShowLegalMovesBoxAnimationEnabled',BoxAnimationEnabled);
    IniFile.WriteBool(Section,'SolutionsPlayerAnimationEnabled',SolutionsInfo.PlayerAnimationEnabled);
    IniFile.WriteBool(Section,'SolutionsBoxAnimationEnabled',SolutionsInfo.BoxAnimationEnabled);
    end;
end;

procedure TGameViewer.SetBorderWidth(BorderWidth__:Integer);
begin
  if (fBorderWidth<>BorderWidth__) and
     (BorderWidth__>=0) and
     (BorderWidth__<=MAX_BORDER_WIDTH) then begin
     fBorderWidth:=BorderWidth__; Modified:=True;
     end;
end;

procedure TGameViewer.SetDefaultValues;
begin
  Modified:=True;
  fBorderWidth:=DEFAULT_BORDER_WIDTH;
  with LegalMovesInfo do begin
    BoxCursor.Color:=clYellow; BoxCursor.Enabled:=True; BoxCursor.PenWidth:=1; BoxCursor.Size:=4;
    BoxCursor.ShadowEnabled:=True; BoxCursor.ShadowColor:=RGB(64,64,64);
    PlayerCursor:=BoxCursor;
    DeadlocksColor:=clWhite; DeadlocksEnabled:=True;
    Enabled:=True;
    JumpMovesBackgroundColor:=clLtGray; JumpMovesEnabled:=True;
    JumpMovesTextColor:=clGreen; JumpMovesTextShadowColor:=clNavy;
    Size:=75; TransparentImage:=True; Transparency:=60;
    PlayerAnimationEnabled:=False; BoxAnimationEnabled:=False;
    SolutionsInfo.PlayerAnimationEnabled:=False; SolutionsInfo.BoxAnimationEnabled:=False;
    FillChar(OuterWallTrimming,SizeOf(OuterWallTrimming),0);
    UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares:=False;
    UseBoxImageForMoveAnimationAlsoForGoalSquares:=False;
    UseFloorTilesAsBackground:=False;
    UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares:=False;
    UsePlayerImageForMoveAnimationAlsoForGoalSquares:=False;
    end;
end;

procedure TGameViewer.SetPlayerFrameIndex(FrameIndex__:Integer);
begin
 if   Sprites[PLAYER_SPRITE]<>nil then with TSpriteN(Sprites[PLAYER_SPRITE]) do
      if (FrameIndex__<>FrameIndex) and (FrameIndex__>=-1) and (FrameIndex__<FrameCount) then begin
         FrameIndex:=FrameIndex__;
         if Visible then MoveTo(R.Left,R.Top);
         end;
end;

procedure TGameViewer.SetWindow(Canvas__:TCanvas; const WindowRect__:TRect);
var i,W,H:Integer;
begin
  Canvas:=Canvas__; fWindowRect:=WindowRect__; Modified:=True; Inc(fWindowResizeCount);
  ColCount:=-1; BoxTargetMaskForDisplay:=0; // ensure that backgrounds are updated before a game is shown
  LegalMoveBitMapsInitialized:=False; ClearFPS;
  for i:=Low(Sprites) to High(Sprites) do   // update canvas for each sprite
      if (Sprites[i]<>nil) and Sprites[i].Enabled then
         if   Sprites[i] is TSpriteN then
              MakeSprite(i,Sprites[i].Pict,Sprites[i].OnDrawSprite,Sprites[i].OnHideSprite,TSpriteN(Sprites[i]).FrameCount)
         else MakeSprite(i,Sprites[i].Pict,Sprites[i].OnDrawSprite,Sprites[i].OnHideSprite,0);

  if Pictures[ptScreenBackground]<>nil then
     with Pictures[ptScreenBackground] do
       if (OrgBitMap<>nil) or MakeOrgBitMapFromPict then begin
          if   Self=MainForm.GameViewer then with MainForm.Image1 do
               begin W:=ClientWidth;           H:=ClientHeight;
//                   BitMapResize(BackBitMap,RectWidth(WindowRect),RectHeight(WindowRect));
               end
          else begin W:=RectWidth(WindowRect); H:=RectHeight(WindowRect);
               end;

          if   BitMap=nil then BitMapCreate(BitMap,W,H);

          if           (Self=MainForm.GameViewer) and Visible then begin
                       // background used (and resized) by 'MainForm'
                       end
          else if      Visible then
                       SetView(View,W,H,Color)
               else if BitMapResize(BitMap,W,H) then with BitMap.Canvas do begin
                       Brush.Color:=Pictures[ptScreenBackground].Color;
                       FillRect(WindowRect); // clear rectangle
                       end;

          if   (BackBitMap<>nil) and (BitMap<>nil) then begin
               BackBitMap.Canvas.CopyMode:=cmSrcCopy;
               BackBitMap.Canvas.CopyRect(Rect(0,0,BackBitMap.Width,BackBitMap.Height),BitMap.Canvas,WindowRect);
               end;
          end;
end;

procedure TGameViewer.Show;
var i:Integer;
begin
  if Initialized and (Canvas<>nil) and (Game<>nil) then begin
     Modified:=False;

     if Self=MainForm.GameViewer then
        MainForm.ShowTitle(Game.FileName);

     ShowBackground;

     with Pictures[ptBoardBackground] do
       if (Visible or FloorTilesVisible or (MainForm.ShowSimpleDeadSquaresEnabled))
          and
          (BitMap<>nil) then begin
          Draw(Left,Top,Canvas);
          TopLayerBitMap:=BitMap;
          end;

     with Pictures[ptReverseModeBackground] do
       if Game.ReverseMode and Visible and (BitMap<>nil) then begin
          Draw(Left,Top,Canvas);
          TopLayerBitMap:=BitMap;
          end;

     if BackBitMap<>nil then begin
        BackBitMap.Canvas.CopyMode:=cmSrcCopy;
        BackBitMap.Canvas.CopyRect(Rect(0,0,BackBitMap.Width,BackBitMap.Height),Self.Canvas,WindowRect);
        PutTilesOnTheBoard(GOAL_SPRITE,BoxTargetMaskForDisplay,MainForm.GamePictures.Transparency[ptGoal],MainForm.GamePictures.TransparencyPct[ptGoal],BackBitMap);
        end;

     for i:=Low(Sprites) to High(Sprites) do // ensure that 'ShowBoard' doesn't try to move the sprites
         if Sprites[i]<>nil then with Sprites[i] do
            SetVisibleStateWithoutUpdatingTheDisplay(False);

     ShowBoard(False);

     if Self=MainForm.GameViewer then with MainForm do begin
        if BevelVisible and (Game.FileName<>'') then DrawBoardBevel;
        ShowStatus;
        end;
     end;
end;

procedure TGameViewer.ShowBackground;
var R:TRect;
begin
  TopLayerBitMap:=nil;
  if Initialized and (Canvas<>nil) then with Pictures[ptScreenBackground] do
     if Visible and (BitMap<>nil) then begin
        if Self=MainForm.GameViewer then begin
           Canvas.CopyMode:=cmSrcCopy;
           if Assigned(MainForm.MultiView) and (not MainForm.MultiView.IsEmpty) then begin
              R.Left :=WindowRect.Left   -MULTI_VIEW_ITEM_BOARD_BORDER_SIZE;
              R.Top  :=WindowRect.Top    -MULTI_VIEW_ITEM_BOARD_BORDER_SIZE;
              R.Right:=WindowRect.Right  +MULTI_VIEW_ITEM_BOARD_BORDER_SIZE;
              R.Bottom:=WindowRect.Bottom+MULTI_VIEW_ITEM_BOARD_BORDER_SIZE;
              ClipRect(R,MainForm.MultiView.ClippingRect);
              Canvas.CopyRect(R,BitMap.Canvas,R);
              end
           else Canvas.CopyRect(WindowRect,BitMap.Canvas,WindowRect);
           //with MainForm.MultiView do
           //  if Assigned(Selected) and (Selected=Opening) then with WindowRect do
           //     AlphaBlendColor(Classes.Rect(Left-MULTI_VIEW_ITEM_BOARD_BORDER_SIZE+2,Top-MULTI_VIEW_ITEM_BOARD_BORDER_SIZE+2,Right+MULTI_VIEW_ITEM_BOARD_BORDER_SIZE-2,Bottom+MULTI_VIEW_ITEM_BOARD_BORDER_SIZE-2),
           //                     clWhite,15);
           end
        else begin
           if View=ivFloorTile then
              SetView(ivFloorTile,RectWidth(WindowRect),RectHeight(WindowRect),Color);
           Draw(WindowRect.Left,WindowRect.Top,Canvas);
           end;
        TopLayerBitMap:=BitMap;
        end
     else with Canvas do begin // no screen background picture
        Brush.Color:=Pictures[ptScreenBackground].Color;
        FillRect(WindowRect); // clear rectangle
        end;
end;

procedure TGameViewer.ShowBoard(Reset:Boolean);
var i,j,k,X,Y:Integer;

  procedure ShowWall(Col,Row,X,Y:Integer);
  var i:Integer; R0,R1,R2,R3:TRect;
  begin //exit;
    if Sprites[WALL_SPRITE]<>nil then with TSpriteN(Sprites[WALL_SPRITE]) do
       if MainForm.GamePictures.WallType=wtTiledWall then
          if   not IsASeamlessWall then
               Draw(X,Y)
          else DrawFrame(X,Y,BT_WALL_NO_NEIGHBOR_WALLS) // it's probably a seamless wall where the user selected single-wall display
       else begin
          i:=Game.BTSquare[Col,Row];
//        DrawFrame(X,Y,i and BT_WALL_NEIGHBORS_MASK);
          R0:=CellToRect(i and BT_WALL_NEIGHBORS_MASK,0,ColWidth,RowHeight);

          with OuterWallTrimming do
            if (Left+Top+Right+Bottom)=0 then
               Pict.DrawRect(X,Y,R0,Canvas)
            else begin
               R1:=R0; R2:=R0; R3:=R0;

               if ((i and (BT_CORNER_TOP_LEFT+BT_TOP))=BT_CORNER_TOP_LEFT) then begin
                  Inc(R1.Left,Left  ); R1.Bottom:=R1.Top+Top;
                  R3.Top:=R1.Bottom;
                  end;
               if ((i and (BT_CORNER_TOP_RIGHT+BT_TOP))=BT_CORNER_TOP_RIGHT) then begin
                  Dec(R1.Right,Right); R1.Bottom:=R1.Top+Top;
                  R3.Top:=R1.Bottom;
                  end;
               if ((i and (BT_CORNER_BOTTOM_LEFT))<>0) //then begin
                  and
                  ((Board[Col,Succ(Row)] and (FLOOR+WALL))<>0) and
                  (Row<Game.BoardHeight) then begin
                  Inc(R2.Left,Left  ); R2.Top:=R2.Bottom-Bottom;
                  R3.Bottom:=R2.Top;
                  end;
               if ((i and (BT_CORNER_BOTTOM_RIGHT))<>0) //then begin
                  and
                  ((Board[Col,Succ(Row)] and (FLOOR+WALL))<>0)
                  and (Row<Game.BoardHeight) then begin
                  Dec(R2.Right,Right); R2.Top:=R2.Bottom-Bottom;
                  R3.Bottom:=R2.Top;
                  end;

               if (i and BT_LEFT  )<>0 then R3.Left  :=R0.Left  +Left;
               if (i and BT_TOP   )<>0 then R3.Top   :=R0.Top   +Top;
               if (i and BT_RIGHT )<>0 then R3.Right :=R0.Right -Right;
               if (i and BT_BOTTOM)<>0 then R3.Bottom:=R0.Bottom-Bottom;
{
               if ((i and BT_LEFT  )<>0) and ((Game.BTSquare[Pred(Col),Row] and BT_INTERIOR_NON_FLOOR_EMPTY_SQUARE)=0) then R3.Left  :=R0.Left  +Left;
               if ((i and BT_TOP   )<>0) and ((Game.BTSquare[Col,Pred(Row)] and BT_INTERIOR_NON_FLOOR_EMPTY_SQUARE)=0) then R3.Top   :=R0.Top   +Top;
               if ((i and BT_RIGHT )<>0) and ((Game.BTSquare[Succ(Col),Row] and BT_INTERIOR_NON_FLOOR_EMPTY_SQUARE)=0) then R3.Right :=R0.Right -Right;
               if ((i and BT_BOTTOM)<>0) and ((Game.BTSquare[Col,Succ(Row)] and BT_INTERIOR_NON_FLOOR_EMPTY_SQUARE)=0) then R3.Bottom:=R0.Bottom-Bottom;
}
               if R1.Bottom<>R0.Bottom then
                  Pict.DrawRect(X+R1.Left-R0.Left,Y+R1.Top-R0.Top,R1,Canvas);
               if R2.Top   <>R0.Top then
                  Pict.DrawRect(X+R2.Left-R0.Left,Y+R2.Top-R0.Top,R2,Canvas);
               if R3.Top<R3.Bottom then
                  Pict.DrawRect(X+R3.Left-R0.Left,Y+R3.Top-R0.Top,R3,Canvas)
               end;

          if ((i and BT_WALL_CAP)<>0) and
             (MainForm.GamePictures.WallType<>wtSeamlessWallNoCap) then begin
             ;//DrawFrame(X-(ColWidth div 2),(Y-RowHeight div 2),16);
             if Assigned(Pict) then begin
                Dec(X,WallCap.X); Dec(Y,WallCap.Y);
                R1:=CellToRect(16,0,ColWidth,RowHeight);
                //if ColWidth >=4 then begin Inc(X); Inc(R1.Left); Dec(R1.Right ,2); end;
                //if RowHeight>=4 then begin Inc(Y); Inc(R1.Top ); Dec(R1.Bottom,2); end;
                Pict.DrawRect(X,Y,R1,Canvas);
                end;
             end;
          end;
  end;

begin // ShowBoard
  if Initialized and (Canvas<>nil) then begin
     if   Reset then // reset sprites if game position and sprites are out of sync.
          for i:=Low(Sprites) to High(Sprites) do
              if (Sprites[i]<>nil) and Sprites[i].Visible then Sprites[i].Hide;

     if   Sprites[PLAYER_SPRITE]<>nil then with TSpriteN(Sprites[PLAYER_SPRITE]) do
          if   not Game.PlayerDirectionAnimationEnabled then
               FrameIndex:=Ord(DEFAULT_PLAYER_DIRECTION)
          else FrameIndex:=Ord(Game.PlayerDirection(Game.History.Count));

     if   BoxTargetMaskForDisplay=BOX_START_POSITION then
          for i:=1 to ColCount do
              for j:=1 to RowCount do begin
                  k:=Abs(Game.Board[i,j]);
                  CellToPos(Pred(i),Pred(j),X,Y);

                  case k and (WALL+PLAYER+BOX+BOX_START_POSITION) of
                    WALL              : ShowWall(i,j,X,Y);
                    PLAYER            : begin Sprites[PLAYER_SPRITE].Pict:=Pictures[GetPlayerStandStillPictureType(i,j)];
                                              Sprites[PLAYER_SPRITE].MoveTo(X,Y);
                                        end;
                    PLAYER+
                    BOX_START_POSITION: begin ShowGoal(X,Y);
                                              Sprites[PLAYER_SPRITE].Pict:=Pictures[GetPlayerStandStillPictureType(i,j)];
                                              Sprites[PLAYER_SPRITE].MoveTo(X,Y);
                                        end;
                    PLAYER+
                    BOX_START_POSITION+ // because of jump-moves in reverse-mode, the player can be on top of a box or a wall
                    BOX               : begin ShowGoal(X,Y);
                                              k:=k shr BOARD_FLAG_COUNT;
                                              if (k<=Game.BoxCount) and Assigned(Sprites[k]) then begin
                                                 Sprites[k].Pict:=Pictures[GetBoxStandStillPictureType(i,j)];
                                                 Sprites[k].MoveTo(X,Y);
                                                 end;
                                              Sprites[PLAYER_SPRITE].Pict:=Pictures[GetPlayerStandStillPictureType(i,j)];
                                              Sprites[PLAYER_SPRITE].MoveTo(X,Y);
                                        end;
                    PLAYER+BOX        : begin k:=k shr BOARD_FLAG_COUNT;
                                              if (k<=Game.BoxCount) and Assigned(Sprites[k]) then begin
                                                 Sprites[k].Pict:=Pictures[GetBoxStandStillPictureType(i,j)];
                                                 Sprites[k].MoveTo(X,Y);
                                                 end;
                                              Sprites[PLAYER_SPRITE].Pict:=Pictures[GetPlayerStandStillPictureType(i,j)];
                                              Sprites[PLAYER_SPRITE].MoveTo(X,Y);
                                        end;
                    PLAYER+WALL       : begin ShowWall(i,j,X,Y);
                                              Sprites[PLAYER_SPRITE].Pict:=Pictures[GetPlayerStandStillPictureType(i,j)];
                                              Sprites[PLAYER_SPRITE].MoveTo(X,Y);
                                        end;
                    BOX               : begin k:=k shr BOARD_FLAG_COUNT;
                                              if (k<=Game.BoxCount) and Assigned(Sprites[k]) then begin
                                                 Sprites[k].Pict:=Pictures[GetBoxStandStillPictureType(i,j)];
                                                 Sprites[k].MoveTo(X,Y);
                                                 end;
                                        end;
                    BOX+
                    BOX_START_POSITION: begin ShowGoal(X,Y);
                                              k:=k shr BOARD_FLAG_COUNT;
                                              if (k<=Game.BoxCount) and Assigned(Sprites[k]) then begin
                                                 Sprites[k].Pict:=Pictures[GetBoxStandStillPictureType(i,j)];
                                                 Sprites[k].MoveTo(X,Y);
                                                 end;
                                        end;
                    BOX_START_POSITION: ShowGoal(X,Y);
                    else {FLOOR}      ;
                  end; // case
                  end
     else for i:=1 to ColCount do
              for j:=1 to RowCount do begin
                  k:=Abs(Game.Board[i,j]);
                  CellToPos(Pred(i),Pred(j),X,Y);

                  case k and (WALL+PLAYER+BOX+GOAL) of
                    WALL              : ShowWall(i,j,X,Y);
                    PLAYER            : begin Sprites[PLAYER_SPRITE].Pict:=Pictures[GetPlayerStandStillPictureType(i,j)];
                                              Sprites[PLAYER_SPRITE].MoveTo(X,Y);
                                        end;
                    PLAYER+GOAL       : begin ShowGoal(X,Y);
                                              Sprites[PLAYER_SPRITE].Pict:=Pictures[GetPlayerStandStillPictureType(i,j)];
                                              Sprites[PLAYER_SPRITE].MoveTo(X,Y);
                                        end;
                    PLAYER+GOAL+        // because of jump-moves in reverse-mode, the player can be on top of a box or a wall
                    BOX               : begin ShowGoal(X,Y);
                                              k:=k shr BOARD_FLAG_COUNT;
                                              if (k<=Game.BoxCount) and Assigned(Sprites[k]) then begin
                                                 Sprites[k].Pict:=Pictures[GetBoxStandStillPictureType(i,j)];
                                                 Sprites[k].MoveTo(X,Y);
                                                 end;
                                              Sprites[PLAYER_SPRITE].Pict:=Pictures[GetPlayerStandStillPictureType(i,j)];
                                              Sprites[PLAYER_SPRITE].MoveTo(X,Y);
                                        end;
                    PLAYER+BOX        : begin k:=k shr BOARD_FLAG_COUNT;
                                              if (k<=Game.BoxCount) and Assigned(Sprites[k]) then begin
                                                 Sprites[k].Pict:=Pictures[GetBoxStandStillPictureType(i,j)];
                                                 Sprites[k].MoveTo(X,Y);
                                                 end;
                                              Sprites[PLAYER_SPRITE].Pict:=Pictures[GetPlayerStandStillPictureType(i,j)];
                                              Sprites[PLAYER_SPRITE].MoveTo(X,Y);
                                        end;
                    PLAYER+WALL       : begin ShowWall(i,j,X,Y);
                                              Sprites[PLAYER_SPRITE].Pict:=Pictures[GetPlayerStandStillPictureType(i,j)];
                                              Sprites[PLAYER_SPRITE].MoveTo(X,Y);
                                        end;
                    BOX               : begin k:=k shr BOARD_FLAG_COUNT;
                                              if (k<=Game.BoxCount) and Assigned(Sprites[k]) then begin
                                                 Sprites[k].Pict:=Pictures[GetBoxStandStillPictureType(i,j)];
                                                 Sprites[k].MoveTo(X,Y);
                                                 end;
                                        end;
                    BOX+GOAL          : begin ShowGoal(X,Y);
                                              k:=k shr BOARD_FLAG_COUNT;
                                              if (k<=Game.BoxCount) and Assigned(Sprites[k]) then begin
                                                 Sprites[k].Pict:=Pictures[GetBoxStandStillPictureType(i,j)];
                                                 Sprites[k].MoveTo(X,Y);
                                                 end;
                                        end;
                    GOAL              : ShowGoal(X,Y);
                    else {FLOOR}      ;
                  end; // case
                  end;
     with LegalMovesInfo do
       if      LegalMovesInfo.Mask=BOX_LEGAL_MOVE    then
               ShowLegalMoves(Mask,Game.BoxPos[Max(0,ItemIndex)],clBlack)
       else if LegalMovesInfo.Mask=SQUARE_SET then
               ShowLegalMoves(Mask,Game.BoxPos[0    ],HighlightedSquareCursorColor)
       else if LegalMovesInfo.Mask=BOX_SET_DEADLOCK then
               ShowLegalMoves(Mask,Game.BoxPos[0    ],HighlightedSquareCursorColor)
       else if LegalMovesInfo.Mask=BOX_SET_TO_SQUARE then
               ShowLegalMoves(Mask,CursorPos         ,HighlightedSquareCursorColor)
       else if LegalMovesInfo.Mask=PLAYER_LEGAL_MOVE then
               ShowLegalMoves(Mask,Game.PlayerPos    ,clBlack);
     end;
end;

procedure TGameViewer.ShowCursor(Col__,Row__,SizeX__,SizeY__:Integer; Color__:TColor; const LegalMovesCursor__:TLegalMovesCursor);
var i,dx,dy,PenWidth,Top,Left:Integer; Points:array[0..7] of TPoint;
begin
  if (Col__>0) and (Row__>0) then with Canvas do begin
     //LockWindowUpdate(Handle);
     Pen.Color     := Color__;
     Pen.Mode      := PmCopy;
     Pen.Style     := psSolid;
     Pen.Width     := 1;
     PenWidth      := LegalMovesCursor__.PenWidth;
     if (ColWidth<=2*PenWidth) or (RowHeight<=2*PenWidth) then PenWidth :=1;

     CellToPos(Pred(Col__),Pred(Row__),Left,Top);
     Points[0].X:=Left;            Points[0].Y:=Top;                            // top    left
     Points[1].X:=Left+ColWidth-2; Points[1].Y:=Points[0].Y;                    // top    right, note: 1 pixel left for the shadow
     Points[2].X:=Points[0].X;     Points[2].Y:=Top+RowHeight-2;                // bottom left,  note: 1 pixel left for the shadow
     Points[3].X:=Points[1].X;     Points[3].Y:=Points[2].Y;                    // bottom right

     dx:=Max(1,Min(PenWidth+SizeX__,Succ((Points[1].X-Points[0].X) div 2)));    // 'Succ':  'LineTo' draws up to but not including the destination point
     dy:=Max(1,Min(PenWidth+SizeY__,Succ((Points[2].Y-Points[0].Y) div 2)));

     if LegalMovesCursor__.ShadowEnabled then begin
        Pen.Color:=LegalMovesCursor__.ShadowColor;
        for i:=0 to 3 do with Points[i+4] do begin
            X:=Succ(Points[i].X); Y:=Succ(Points[i].Y);
            end;

        for i:=1 to PenWidth do begin
            // draw the cursor manually one line at a time with pen.width = 1;
            // that way the program doesn't depend on drawing conventions when pen.width <> 1
            with Points[4] do begin {Inc(X); Inc(Y);} MoveTo(X,Y); LineTo(X+dx,Y); MoveTo(X,Y); LineTo(X,Y+dy); Inc(X); Inc(Y); end; // top    left
            with Points[5] do begin {Dec(X); Inc(Y);} MoveTo(X,Y); LineTo(X-dx,Y); MoveTo(X,Y); LineTo(X,Y+dy); Dec(X); Inc(Y); end; // top    right
            with Points[6] do begin {Inc(X); Dec(Y);} MoveTo(X,Y); LineTo(X+dx,Y); MoveTo(X,Y); LineTo(X,Y-dy); Inc(X); Dec(Y); end; // bottom left
            with Points[7] do begin {Dec(X); Dec(Y);} MoveTo(X,Y); LineTo(X-dx,Y); MoveTo(X,Y); LineTo(X,Y-dy); Dec(X); Dec(Y); end; // bottom right
            Dec(dx); Dec(dy);
            end;

        Inc(dx,PenWidth); Inc(dy,PenWidth); // restore values so they're ready for drawing the cursor
        Pen.Color:=Color__;
        end;

     for i:=1 to PenWidth do begin
         // draw the cursor manually with pen.width = 1;
         // that way the program doesn't depend on drawing conventions when pen.width <> 1
         with Points[0] do begin {Inc(X); Inc(Y);} MoveTo(X,Y); LineTo(X+dx,Y); MoveTo(X,Y); LineTo(X,Y+dy); Inc(X); Inc(Y); end; // top    left
         with Points[1] do begin {Dec(X); Inc(Y);} MoveTo(X,Y); LineTo(X-dx,Y); MoveTo(X,Y); LineTo(X,Y+dy); Dec(X); Inc(Y); end; // top    right
         with Points[2] do begin {Inc(X); Dec(Y);} MoveTo(X,Y); LineTo(X+dx,Y); MoveTo(X,Y); LineTo(X,Y-dy); Inc(X); Dec(Y); end; // bottom left
         with Points[3] do begin {Dec(X); Dec(Y);} MoveTo(X,Y); LineTo(X-dx,Y); MoveTo(X,Y); LineTo(X,Y-dy); Dec(X); Dec(Y); end; // bottom right
         Dec(dx); Dec(dy);
         end;
     //LockWindowUpdate(0);
     end;
end;

procedure TGameViewer.ShowGoal(X,Y:Integer);
begin
  if Assigned(Sprites[GOAL_SPRITE]) then with Sprites[GOAL_SPRITE] do
     if (not MainForm.GamePictures.Transparency[ptGoal]) or
        (MainForm.GamePictures.TransparencyPct[ptGoal]=0) or
        (not Assigned(Pict)) then
        Draw(X,Y)
     else begin
        R:=Rect(X,Y,X+PictureSizeRect.Right,Y+PictureSizeRect.Bottom);
        RestoreBackground(R);
        if (not Pictures[ptFloor].Visible) and // if the floor tile is visible, then the transparent goal has been drawn directly on the background instead of the floor tile
           MakeStaticWorkBitMap(PictureSizeRect.Right,PictureSizeRect.Bottom,True) then begin
           StaticWorkBitMap.Canvas.CopyRect(PictureSizeRect,Canvas,R);
           BitMapAlphaBlendRect(StaticWorkBitMap,Pict.BitMap,StaticWorkBitMap,
                                PictureSizeRect,
                                PictureSizeRect,
                                PictureSizeRect,
                                MainForm.GamePictures.TransparencyPct[ptGoal],
                                Pict.Masked,
                                clBlack, // not 'RGBToColor(Pict.MaskBitMapColor'; if the image originally had a non-black mask color, then it has been changed to black at this time
                                Pict.MaskBitMapPct);
           Canvas.CopyRect(R,StaticWorkBitMap.Canvas,PictureSizeRect);
           end;
        end;
end;

function TGameViewer.ShowLegalMoves(Mask__:Integer; CurrentPosition__:TColRow; HighlightedSquareCursorColor__:TColor):Integer;
var i,dx,dy,X,Y,W,H,Col,Row:Integer; s,t:TLegalMoveBitMapsType; R,R0:TRect;
begin
  Result:=0; HighlightedSquareCursorColor:=HighlightedSquareCursorColor__; IsIdle:=False;
  if Mask__=0 then Result:=HideLegalMoves
  else if LegalMoveBitMapsInitialized and
          (LegalMovesInfo.Enabled
           or
           (Mask__=SQUARE_SET)
           or
           (Mask__=BOX_SET_TO_SQUARE)
          ) then with LegalMovesInfo do begin
          if   (Mask__=BOX_LEGAL_MOVE   ) or
               (Mask__=SQUARE_SET) or
               (Mask__=BOX_SET_DEADLOCK) or
               (Mask__=BOX_SET_TO_SQUARE) then Mask:=Mask__
          else Mask:=PLAYER_LEGAL_MOVE;
          HideLegalMovesCursor;
          CursorPos:=CurrentPosition__;

          with CursorPos do
            if x*y<>0 then begin
               Game.Board[x,y]:=Game.Board[x,y] and (not (Mask and (not BOARD_GAME_STATE_FLAGS_MASK))); // don't show current position
               if      Mask=BOX_LEGAL_MOVE then
                       ShowLegalMovesCursor(Game.Board[x,y] shr BOARD_FLAG_COUNT)
               else if Mask=PLAYER_LEGAL_MOVE then
                       ShowLegalMovesCursor(PLAYER_SPRITE)
               else if Mask=BOX_SET_TO_SQUARE then
                       ShowLegalMovesCursor(CURSOR_SPRITE);
               end;

          W:=LegalMoveBitMaps[lmPlayer,0].Width;
          H:=LegalMoveBitMaps[lmPlayer,0].Height;
          R0:=Rect(0,0,W,H);
          dx:=(ColWidth -W) div 2;
          dy:=(RowHeight-H) div 2;

          for Col:=1 to ColCount do
              for Row:=1 to RowCount do begin
                  i:=Game.Board[Col,Row];
                  if ((i and Mask)=Mask)
                     or
                     (Game.ReverseMode
                      and
                      (Mask=PLAYER_LEGAL_MOVE)
                      and
                      ((i and (WALL+BOX+PLAYER+FLOOR+PLAYER_UNREACHABLE_FLOOR))=FLOOR)
                      and
                      ((Game.History.PushCount=0)
                       or
                       Game.JumpsAllowedAfterFirstBoxMoveInReverseMode
                      )
                      and
                      JumpMovesEnabled
                     ) then begin

                     CellToPos(Pred(Col),Pred(Row),X,Y);

                     if (Mask and SQUARE_SET)<>0 then begin
                        if BoxCursor.Enabled or (not BoxAnimationEnabled) then
                           ShowCursor(Col,Row,ColWidth div 2,RowHeight div 2,HighlightedSquareCursorColor,BoxCursor);
                        end
                     else begin

                        Inc(X,dx); Inc(Y,dy);
                        R:=Rect(X,Y,X+W,Y+H);

                        if   Mask=BOX_LEGAL_MOVE then
                             if        (i and BoxTargetMaskForDisplay)=0 then
                                       begin s:=lmBox;          t:=lmBox;    end
                             else      begin s:=lmBoxOnGoal;    t:=lmBox;    end
                        else if   (i and Mask)=Mask then // 'True': player's legal moves; 'False': player jumps
                                  if   (i and BoxTargetMaskForDisplay)=0 then
                                       begin s:=lmPlayer;       t:=lmPlayer; end
                                  else begin s:=lmPlayerOnGoal; t:=lmPlayer; end
                             else      begin s:=lmJump;         t:=lmJump;   end;

                        if LegalMoveBitMapsCreated[Col,Row,t] and
                           ((Col<>Game.PlayerPos.X) or (Row<>Game.PlayerPos.Y)) then begin
                           if        t=lmPlayer then i:=0
                           else if   t=lmBox    then i:=1
                                else i:=2;
                           X:=Pred(Col)*W*3+W*i; Y:=Pred(Row)*H;
                           Canvas.CopyMode:=cmSrcCopy;
                           Canvas.CopyRect(R,LegalMoveBitMaps[lmTemporary,1].Canvas,Rect(X,Y,X+W,Y+H));
                           end
                        else begin
                           if LegalMovesInfo.TransparentImage then begin
                              LegalMoveBitMaps[lmTemporary,0].Canvas.CopyRect(R0,Canvas,R);
                              BitMapAlphaBlendRect(LegalMoveBitMaps[lmTemporary,0],LegalMoveBitMaps[s,0],LegalMoveBitMaps[lmTemporary,0],
                                                   R0,R0,R0,LegalMovesInfo.Transparency,LegalMoveBitMaps[s,1]<>nil,clBlack,0);

                              Canvas.CopyRect(R,LegalMoveBitMaps[lmTemporary,0].Canvas,R0);
                              end
                           else begin
                              if LegalMoveBitMaps[s,1]=nil then
                                 Canvas.CopyMode:=cmSrcCopy              // no mask
                              else begin
                                 Canvas.CopyMode:=cmSrcAnd;              // and-ing
                                 Canvas.Draw(X,Y,LegalMoveBitMaps[s,1]); // apply the mask
                                 Canvas.CopyMode:=cmSrcPaint;            // or-ing
                                 end;
                              Canvas.Draw(X,Y,LegalMoveBitMaps[s,0]);    // draw the image
                              Canvas.CopyMode:=cmSrcCopy;
                              end;

                           // save the image, unless it is the player-position
                           if ((Col<>Game.PlayerPos.X) or (Row<>Game.PlayerPos.Y)) then begin
                              LegalMoveBitMaps[lmTemporary,1].Canvas.CopyMode:=cmSrcCopy;
                              if        t=lmPlayer then i:=0
                              else if   t=lmBox    then i:=1
                                   else i:=2;
                              X:=Pred(Col)*W*3+W*i; Y:=Pred(Row)*H;
                              LegalMoveBitMaps[lmTemporary,1].Canvas.CopyRect(Rect(X,Y,X+W,Y+H),Canvas,R);
                              LegalMoveBitMapsCreated[Col,Row,t]:=True;
                              end;
                           end;
                        end;
                     Inc(Result);
                     end;
                  end;
          if Result=0 then Mask:=0; // no visible squares
          end
       else with CurrentPosition__ do begin
              HideLegalMovesCursor;
              LegalMovesInfo.CursorPos:=CurrentPosition__;
              if      (Mask__=BOX_LEGAL_MOVE) and (x*y<>0)  then
                       ShowLegalMovesCursor(Game.Board[x,y] shr BOARD_FLAG_COUNT)
              else if (Mask__=BOX_SET_TO_SQUARE) and (x*y<>0) then
                       ShowLegalMovesCursor(CURSOR_SPRITE)
              else if (Mask__=PLAYER_LEGAL_MOVE) then
                      ShowLegalMovesCursor(PLAYER_SPRITE);
              end;
  LegalMovesInfo.StartTime:=GetTickCount mod 1000; // 'mod': only the fraction of a second is used by the animation
end;

procedure TGameViewer.ShowLegalMovesCursor(ItemIndex__:Integer);
var Point:TPoint;
begin
  with LegalMovesInfo do begin
    HideLegalMovesCursor;
    if (ItemIndex__>=PLAYER_SPRITE) and (ItemIndex__<=Game.BoxCount) then begin
       ItemIndex:=ItemIndex__;
       if   (ItemIndex>=1) and (ItemIndex<=Game.BoxCount) then
            if BoxCursor.Enabled then
               ShowCursor(Game.BoxPos[ItemIndex].x,Game.BoxPos[ItemIndex].y,BoxCursor.Size,BoxCursor.Size,BoxCursor.Color,BoxCursor)
            else
       else if PlayerCursor.Enabled then
               ShowCursor(Game.PlayerPos        .x,Game.PlayerPos        .y,PlayerCursor.Size,PlayerCursor.Size,PlayerCursor.Color,PlayerCursor);
       end
    else if ItemIndex__=CURSOR_SPRITE then begin
            ItemIndex:=ItemIndex__;
            if Sprites[CURSOR_SPRITE]<>nil then begin
               CellToPos(Pred(CursorPos.x),Pred(CursorPos.y),Point.X,Point.Y); // 'Pred': 'CellPos' uses 0-based columns and rows
               Sprites[CURSOR_SPRITE].MoveTo(Point.X,Point.Y);
               end;
            end;
    end;
end;

procedure TGameViewer.DrawCursorSprite(Canvas__:TCanvas; const Rect__:TRect);
begin // kludge: the cursor-sprite is not really used as a sprite; it's only used for saving and restoring the background
  with LegalMovesInfo do ShowCursor(CursorPos.x,CursorPos.y,BoxCursor.Size,BoxCursor.Size,BoxCursor.Color,BoxCursor);
end;

procedure TGameViewer.ShowMove(const FromPos,ToPos:TPoint; BoxNo,TimeMS,MoveCount,PushCount:Integer; Undo,Jump,LastMove:Boolean);
var i,j,k,x,y,
    HalfColWidth,HalfRowHeight,
    MoveCountSign,oAnimateReplayMovesMS:Integer; oWindowResizeCount:Cardinal;
    oIsBusy,DoMove:Boolean; Direction:TDirection;
    Time,StartTime,StopTime,PrevTime:TTimeMS;
    CurrentCell,NewCell,CurrentPlayerPos:TPoint;
    SpriteNo:array[0..1] of Integer; // box-sprites and player-sprites sorted according to the task
    SoundType:TSoundType; //q:Double;
    BoxStandStillPicture,PlayerStandStillPicture:TPict;
begin // The board must be updated BEFORE calling this procedure,
      // but all parameters ('MoveCount' etc.) must reflect the state before performing the move
  oIsBusy:=Game.IsBusy;
  //try
    Game.IsBusy:=True;
    oWindowResizeCount:=WindowResizeCount; oAnimateReplayMovesMS:=MainForm.Game.AnimateReplayMovesMS;
    HalfColWidth:=ColWidth div 2; HalfRowHeight:=RowHeight div 2;
    ClearFPS;

    if   not Undo then
         CurrentPlayerPos:=FromPos
    else CurrentPlayerPos:=ToPos;

    if TimeMS<1 then TimeMS:=1;

    x:=ToPos.x-FromPos.x;
    y:=ToPos.y-FromPos.y;

    if Sprites[PLAYER_SPRITE]<>nil then with TSpriteN(Sprites[PLAYER_SPRITE]) do
      if not Game.PlayerDirectionAnimationEnabled then
         FrameIndex:=Ord(DEFAULT_PLAYER_DIRECTION)
      else begin
         if   not (Undo and LastMove and (Abs(x)+Abs(y)=1)) then begin
              if   x=0 then
                   if   y<0 then
                        Direction:=SokUtil_.Up
                   else Direction:=SokUtil_.Down
              else if   x>0 then
                        Direction:=SokUtil_.Right
                   else Direction:=SokUtil_.Left;

              if   not  (ReverseMode and (BoxNo>0)) then
                   FrameIndex:=Ord(Direction)
              else FrameIndex:=Ord(OPPOSITE_DIRECTION[Direction]);
              if   BoxNo<0 then FrameIndex:=Ord(OPPOSITE_DIRECTION[TDirection(FrameIndex)]);
              end
         else FrameIndex:=Ord(Game.PlayerDirection(Game.History.Count));
         end;

    if   (BoxNo<=0) or (Sprites[BoxNo]=nil) then begin
         BoxNo:=-1; SoundType:=stMove;
         end
    else with Sprites[BoxNo] do begin
      // calculate [x,y] = box position 1 square away from the destination
      if not Game.ReverseMode then begin // normal game mode
         if Undo then begin
            if x=0 then x:=FromPos.x else x:=FromPos.x+(x div Abs(x));
            if y=0 then y:=FromPos.y else y:=FromPos.y+(y div Abs(y));
            end
         else begin
            if x=0 then x:=FromPos.x else x:=ToPos  .x+(x div Abs(x));
            if y=0 then y:=FromPos.y else y:=ToPos  .y+(y div Abs(y));
            end;
         end
      else begin // reverse mode
         if Undo then begin
            if x=0 then x:=FromPos.x else x:=FromPos.x-(x div Abs(x));
            if y=0 then y:=FromPos.y else y:=FromPos.y-(y div Abs(y));
            end
         else begin
            if x=0 then x:=FromPos.x else x:=ToPos  .x-(x div Abs(x));
            if y=0 then y:=FromPos.y else y:=ToPos  .y-(y div Abs(y));
            end;
         end;

      // caution: whether the box moves to a goal-square or a non-goal square
      // is calculated only once and not for each move,
      // hence, mixing is not allowed
      if   (Game.Board[x,y] and Game.BoxTargetMaskForDisplay)=0 then
           begin Pict:=Pictures[ptBoxMoveAnimation];
                 SoundType:=stPush;
           end
      else begin if   not UseBoxImageForMoveAnimationAlsoForGoalSquares then
                      Pict:=Pictures[ptBoxOnGoalMoveAnimation]
                 else Pict:=Pictures[ptBoxMoveAnimation];
                 SoundType:=stGoal;
           end;
      BoxStandStillPicture:=Pictures[GetBoxStandStillPictureType(x,y)];

      StartPosition.x:=R.Left; StartPosition.y:=R.Top;
      CellToPos(Pred(x),Pred(y),StopPosition.x,StopPosition.y);
      dx:=(StopPosition.x-StartPosition.x)  / TimeMS; // calculate velocities for x-axis and y-axis
      dy:=(StopPosition.y-StartPosition.y ) / TimeMS;
      end;

    if Sprites[PLAYER_SPRITE]<>nil then with Sprites[PLAYER_SPRITE] do begin
       with CurrentPlayerPos do begin
         if (Game.Board[x,y] and Game.BoxTargetMaskForDisplay)=0 then begin
            Pict:=Pictures[ptPlayerMoveAnimation];
            end
         else begin
            if   not UsePlayerImageForMoveAnimationAlsoForGoalSquares then
                 Pict:=Pictures[ptPlayerOnGoalMoveAnimation]
            else Pict:=Pictures[ptPlayerMoveAnimation];
            end;
         PlayerStandStillPicture:=Pictures[GetPlayerStandStillPictureType(x,y)];
         end;

       StartPosition.x:=R.Left; StartPosition.y:=R.Top;
       if   Undo then begin
            CellToPos(Pred(FromPos.x),Pred(FromPos.y),StopPosition.x,StopPosition.y);
            SpriteNo[0]:=PLAYER_SPRITE; SpriteNo[1]:=BoxNo; // show player before box
            SoundType:=stUndo;
            MoveCountSign:=-1;
            end
       else begin
            CellToPos(Pred(ToPos  .x),Pred(ToPos  .y),StopPosition.x,StopPosition.y);
            SpriteNo[0]:=BoxNo; SpriteNo[1]:=PLAYER_SPRITE; // show box before player
            MoveCountSign:=1;
            end;
       dx:=(StopPosition.x-StartPosition.x) / TimeMS;
       dy:=(StopPosition.y-StartPosition.y) / TimeMS;

       CurrentCell.x:=(StartPosition.x+HalfColWidth ) div ColWidth;
       CurrentCell.y:=(StartPosition.y+HalfRowHeight) div RowHeight;

       if Game.ReverseMode then begin // reverse mode: the sprites must be shown in opposite order
          i:=SpriteNo[0]; SpriteNo[0]:=SpriteNo[1]; SpriteNo[1]:=i;
          if (not Undo) and Jump then SoundType:=stJump;
          end;

       repeat StartTime:=GetTickCount;
              StopTime :=StartTime+TTimeMS(TimeMS);
       until  StopTime >=StartTime; // primitive clock wrap-around control

       Time:=StartTime; PrevTime:=High(PrevTime);
       while (Time>=StartTime) and (Time<StopTime) do begin
         Dec(Time,StartTime);
         if Time<>PrevTime then with Sprites[PLAYER_SPRITE] do begin

            x:=StartPosition.x+Trunc(dx*Time)-R.Left;
            y:=StartPosition.y+Trunc(dy*Time)-R.Top;

            //q:=Time/TimeMS;
            //q:=q*q*(3-2*q);
            //x:=StartPosition.x+Trunc((StopPosition.x-StartPosition.x)*q)-R.Left;
            //y:=StartPosition.y+Trunc((StopPosition.y-StartPosition.y)*q)-R.Top;

            if Game.SessionSmoothMoveAnimationEnabled then begin
               DoMove:=(x<>0) or (y<>0);
               if DoMove and
                  Game.SmoothMoveAnimationThresholdEnabled and
                  (x+y>Game.SmoothMoveAnimationThresholdMaxPixelsPerMove) then
                  Game.SessionSmoothMoveAnimationEnabled:=False;
               end
            else begin
               DoMove:=(Abs(x)>=ColWidth) or (Abs(y)>=RowHeight);
               if DoMove then begin
                  x:=(x div ColWidth )*ColWidth;
                  y:=(y div RowHeight)*RowHeight;
                  end;
               end;

            if DoMove then begin
               if oWindowResizeCount=WindowResizeCount then begin // 'True': the window hasn't been resized during the move animation
                  NewCell.x:=(R.Left+x+HalfColWidth ) div ColWidth;
                  NewCell.y:=(R.Top +y+HalfRowHeight) div RowHeight;
                  if (NewCell.x<>CurrentCell.x) or (NewCell.y<>CurrentCell.y) then begin
                     Inc(CurrentPlayerPos.x,NewCell.x-CurrentCell.x);
                     Inc(CurrentPlayerPos.y,NewCell.y-CurrentCell.y);
                     with CurrentPlayerPos do begin
                       if (Game.Board[x,y] and Game.BoxTargetMaskForDisplay)=0 then begin
                          Pict:=Pictures[ptPlayerMoveAnimation];
                          end
                       else begin
                          if   not UsePlayerImageForMoveAnimationAlsoForGoalSquares then
                               Pict:=Pictures[ptPlayerOnGoalMoveAnimation]
                          else Pict:=Pictures[ptPlayerMoveAnimation];
                          end;
                       PlayerStandStillPicture:=Pictures[GetPlayerStandStillPictureType(x,y)];
                       end;

                     k:=(Abs(CurrentCell.x-NewCell.x)+Abs(CurrentCell.y-NewCell.y))*MoveCountSign;
                     Inc(MoveCount,k); if BoxNo>0 then Inc(PushCount,k);
                     CurrentCell.x:=NewCell.x; CurrentCell.y:=NewCell.y;
                     MainForm.Status.MoveCount:=IntToStr(MoveCount);
                     MainForm.Status.PushCount:=IntToStr(PushCount);

                     if Assigned(MainForm.MultiView.Selected) and (Game=MainForm.Game) and (not LastMove) then with MainForm.MultiView.Selected do with Panels[mvipCaption] do
                        ShowPanel(mvipCaption,Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount]),Enabled,Rect,Alignment);
                     end;

                  if (BoxNo>0) and (PLAYER_SPRITE=SpriteNo[1]) then with Sprites[BoxNo] do
                     MoveTo(R.Left+x,R.Top+y);

                  MoveTo(R.Left+x,R.Top+y);

                  if (BoxNo>0) and (PLAYER_SPRITE=SpriteNo[0]) then with Sprites[BoxNo] do
                     MoveTo(R.Left+x,R.Top+y);

                  Inc(FrameCount);
                  end
               else begin
                  StopTime:=0; // the window has been resized; stop the animation
                  end;

               Application.ProcessMessages;
               end;
            PrevTime:=Time;
            end;

         if (oAnimateReplayMovesMS<>MainForm.Game.AnimateReplayMovesMS) and
            (MainForm.Game.AnimateReplayMovesMS>0) and
            (oWindowResizeCount=WindowResizeCount) then begin
            TimeMS:=Max(Max(1,Min(100,StopTime-StartTime-Time)), // '100': give the animation a chance to finish smoothly
                        (Integer(TimeMS)*MainForm.Game.AnimateReplayMovesMS div Max(1,oAnimateReplayMovesMS))-Integer(Time));
            oAnimateReplayMovesMS:=MainForm.Game.AnimateReplayMovesMS;

            ClearFPS;
            Game.SessionSmoothMoveAnimationEnabled:=Game.SmoothMoveAnimationEnabled;

            repeat StartTime:=GetTickCount;
                   StopTime :=StartTime+TTimeMS(TimeMS);
            until  StopTime >=StartTime; // primitive clock wrap-around control

            for i:=0 to 1 do
                if SpriteNo[i]>=0 then with Sprites[SpriteNo[i]] do begin
                   StartPosition.x:=R.Left; StartPosition.y:=R.Top;
                   dx:=(StopPosition.x-StartPosition.x) / TimeMS;
                   dy:=(StopPosition.y-StartPosition.y) / TimeMS;
                   end;
            end;

         Time:=GetTickCount;
         end;

       if Time>=StartTime then begin
          Time:=Time-StartTime; Inc(FrameTime,Time);
          end
       else ClearFPS; // clock wrap-around

       // ensure that the sprites reached their destination

       if (BoxNo>0) and (PLAYER_SPRITE=SpriteNo[1]) then with Sprites[BoxNo] do
          if (R.Left<>StopPosition.x) or (R.Top<>StopPosition.y) or
             (Pict<>BoxStandStillPicture) then begin
             Pict:=BoxStandStillPicture;
             if oWindowResizeCount=WindowResizeCount then
                MoveTo(StopPosition.x,StopPosition.y);
             end;

       with Sprites[PLAYER_SPRITE] do begin

         if Undo and LastMove and Game.PlayerDirectionAnimationEnabled then
            with TSpriteN(Sprites[PLAYER_SPRITE]) do begin
              Direction:=Game.PlayerDirection(Game.History.Count);

              with Game do with History do
                if (Count=0) and (Top<>0) and
                   (PlayerPos.x<>0) and (PlayerPos.y<>0) and
                   ((Board[PlayerPos.x+DIRECTION_XY[Direction,ColAxis],PlayerPos.y+DIRECTION_XY[Direction,RowAxis]] and WALL)<>0) then begin
                   // by 'undo' back to the starting position, if the player is facing a wall then use the direction from the first move instead
                   if   not (ReverseMode and ((Moves[1] and H_FLAG_BOX)<>0)) then
                        Direction:=TDirection (Moves[1] and H_MASK_DIRECTION)
                   else Direction:=OPPOSITE_DIRECTION[TDirection(Moves[1] and H_MASK_DIRECTION)];
                   end;

              if Ord(Direction)<>FrameIndex then begin
                 FrameIndex:=Ord(Direction);
                 R.Left:=Pred(StopPosition.x);
                 end;
              end;

         if (R.Left<>StopPosition.x) or (R.Top<>StopPosition.y) or
            (Pict<>PlayerStandStillPicture) or
            Assigned(MainForm.MultiView.Selected) then begin
            NewCell.x:=(StopPosition.x+HalfColWidth ) div ColWidth;
            NewCell.y:=(StopPosition.y+HalfRowHeight) div RowHeight;
            k:=Abs(CurrentCell.x-NewCell.x)+Abs(CurrentCell.y-NewCell.y);

            if k<>0 then begin
               Inc(CurrentPlayerPos.x,NewCell.x-CurrentCell.x);
               Inc(CurrentPlayerPos.y,NewCell.y-CurrentCell.y);
               k:=k*MoveCountSign;
               Inc(MoveCount,k); if BoxNo>0 then Inc(PushCount,k);
               MainForm.Status.MoveCount:=IntToStr(MoveCount);
               MainForm.Status.PushCount:=IntToStr(PushCount);
               end;

            with CurrentPlayerPos do begin
              //if   (Game.Board[x,y] and Game.BoxTargetMaskForDisplay)=0 then
              //     Pict:=Pictures[ptPlayer]
              //else Pict:=Pictures[ptPlayerOnGoal];
              Pict:=Pictures[GetPlayerStandStillPictureType(x,y)];
              end;
            if oWindowResizeCount=WindowResizeCount then
               MoveTo(StopPosition.x,StopPosition.y);

            //if LastMove and Assigned(MainForm.MultiView.Selected) then with MainForm.MultiView.Selected do with Panels[mvipCaption] do
            //   ShowPanel(mvipCaption,Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount]){+Game.SecondaryMetricsAsText},Enabled,Rect,Alignment);
            end;
         end;

       if (BoxNo>0) and (PLAYER_SPRITE=SpriteNo[0]) then with Sprites[BoxNo] do
          if (R.Left<>StopPosition.x) or (R.Top<>StopPosition.y) or
             (Pict<>BoxStandStillPicture) then begin
             Pict:=BoxStandStillPicture;
             if oWindowResizeCount=WindowResizeCount then
                MoveTo(StopPosition.x,StopPosition.y);
             end;

       if LastMove and MainForm.Sound.Enabled then
          MainForm.Sound.Play(SoundType);

       Application.ProcessMessages;

       if oWindowResizeCount<>WindowResizeCount then begin
          Self.Show; // the window has been resized, and move animation used the old settings: refresh the board

          if (not MainForm.MultiView.IsEmpty) and (not Game.IsBusy) and (not Game.IsBrowsing) and  (not Game.IsReplaying) and (Game=MainForm.Game) then with MainForm.MultiView do begin
             Game.IsBusy:=False;
             DisappearedItemsCount:=0;
             OnResize(nil); // recalculate information, in particular for showing the correct 'maximize' and 'minimize' status for each view
             if ReportDisappearedItems(DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
                DoReportDisappearedItems:=False;
             end;
          end;
       end;

    //finally
      Game.IsBusy:=oIsBusy;

      if LastMove and Assigned(MainForm.MultiView.Selected) and (Game=MainForm.Game) then
         if   (not Game.IsBusy) and (not Game.IsBrowsing) and (not Game.IsReplaying) then
              MainForm.MultiView.Selected.ShowCaptionWithMetrics(True)
         else with MainForm.MultiView.Selected do with Panels[mvipCaption] do begin
                //ShowPanel(mvipCaption,Format(FORMAT_MOVES_AND_PUSHES,[MoveCount,PushCount]){+Game.SecondaryMetricsAsText},Enabled,Rect,Alignment);
                end;
    //end;
end;

procedure TGameViewer.ShowMoveInstantly(BoxNo:Integer; Undo,Jump,LastMove:Boolean);
var oWindowResizeCount:Cardinal; SoundType:TSoundType;
begin // The board must be updated BEFORE calling this procedure
  oWindowResizeCount:=WindowResizeCount;

  if        Undo then SoundType:=stUndo
  else if   Jump then SoundType:=stJump
       else SoundType:=stMove;

  if   (Sprites[PLAYER_SPRITE]<>nil) and
       ((BoxNo=0) or (Sprites[BoxNo]<>nil))  then begin
       Sprites[PLAYER_SPRITE].Hide;

       if BoxNo<>0 then with Sprites[BoxNo] do begin
          if   (Game.Board[Game.BoxPos[BoxNo].x,Game.BoxPos[BoxNo].y] and Game.BoxTargetMaskForDisplay)=0 then
               begin {Pict:=Pictures[ptBox      ];} if not Undo then SoundType:=stPush; end
          else begin {Pict:=Pictures[ptBoxOnGoal];} if not Undo then SoundType:=stGoal; end;
          Pict:=Pictures[GetBoxStandStillPictureType(Game.BoxPos[BoxNo].x,Game.BoxPos[BoxNo].y)];
          CellToPos(Pred(Game.BoxPos[BoxNo].x),Pred(Game.BoxPos[BoxNo].y),StopPosition.x,StopPosition.y);
          MoveTo(StopPosition.x,StopPosition.y);
          end;

       with TSpriteN(Sprites[PLAYER_SPRITE]) do with Game.PlayerPos do begin
         //if   (Game.Board[x,y] and Game.BoxTargetMaskForDisplay)=0 then
         //     Pict:=Pictures[ptPlayer]
         //else Pict:=Pictures[ptPlayerOnGoal];
         Pict:=Pictures[GetPlayerStandStillPictureType(x,y)];
         if   not Game.PlayerDirectionAnimationEnabled then
              FrameIndex:=Ord(DEFAULT_PLAYER_DIRECTION)
         else FrameIndex:=Ord(Game.PlayerDirection(Game.History.Count));
         CellToPos(Pred(x),Pred(y),StopPosition.x,StopPosition.y);
         MoveTo(StopPosition.x,StopPosition.y);
         end;
       end;

  if   LastMove and MainForm.Sound.Enabled then MainForm.Sound.Play(SoundType);

  if   oWindowResizeCount<>WindowResizeCount then begin
       Show; // the window was resized, and move animation has used the old settings: refresh the board

       if (not MainForm.MultiView.IsEmpty) and (not Game.IsBusy) and (not Game.IsBrowsing) and  (not Game.IsReplaying) and (Game=MainForm.Game) then with MainForm.MultiView do begin
          DisappearedItemsCount:=0;
          OnResize(nil); // recalculate information, in particular for showing the correct 'maximize' and 'minimize' status for each view
          if ReportDisappearedItems(DoReportDisappearedItems,ThisMessageWillNotAppearAgainInThisSessionText) then
             DoReportDisappearedItems:=False;
          end;
       end
  else if Self=MainForm.GameViewer then MainForm.ShowStatus;
end;

end.

