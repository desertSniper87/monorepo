unit Sprite_;

interface

uses Windows,ExtCtrls,Graphics,BitMap_,Pict_;

const
  MAX_SPRITE_FRAMES = 24;
type
  TDrawSpriteEvent = procedure(Canvas__:TCanvas; const Rect__:TRect) of object;
  THideSpriteEvent = procedure(const Rect__:TRect) of object;

  TSprite1 = class
  private
    fVisible:Boolean;
    MouseDX,MouseDY:Integer; // for dragging
  protected
    BackBitMap:TBitMap;
    BackRect:TRect; // the portion of the background that is saved in 'BackBitMap'; due to clipping this is not necessarily identical to the sprite position stored in 'R'
    fPictureSizeRect:TRect; // Rect(0,0,Width,Height); pre-calculated for fast and easy access
  public
    Canvas:TCanvas; // kludge: the canvas is public for easy updating, like so many other of the fields
    Enabled:Boolean;
    OnDrawSprite:TDrawSpriteEvent; // only activated when no 'Pict' is assigned to the sprite
    OnHideSprite:THideSpriteEvent; // when the sprite has an 'OnHideSprite' handler it doesn't create its own background bitmap
    dx,dy:Double; // velocity, pixels per second
    MaxDX,MaxDY:Integer;
    Pict:TPict; // the sprite does not own the picture; note: remember to call 'Resize()' or 'ResizeFrames()' after changing the size
    R:TRect; // current position
    StartPosition,StopPosition:TPoint; // move
    constructor Create(Canvas__:TCanvas; Pict__:TPict; OnDrawSprite__:TDrawSpriteEvent; OnHideSprite__:THideSpriteEvent);
    destructor  Destroy; override;
    procedure   BeginDrag(X,Y:Integer);
    procedure   Clear; virtual;
    procedure   Drag(X,Y:Integer);
    procedure   Draw(X,Y:Integer); virtual;
    procedure   Hide;
    procedure   MoveTo(NewX,NewY:Integer);
    procedure   MoveTowardsXYSmoothly(NewX,NewY:Integer);
    procedure   Resize(Width__,Height__:Integer);
    procedure   SetVisibleStateWithoutUpdatingTheDisplay(Visible__:Boolean);
    procedure   Show;

    property    PictureSizeRect:TRect read fPictureSizeRect;
    property    SpriteBackground:TBitMap read BackBitMap;
    property    Visible:Boolean read fVisible;
  end;

  TSpriteN = class(TSprite1)
  private
    fFrameCount:Integer;
//  fFrameIndex:Integer;
  protected
    procedure   SetFrameCount(FrameCount__:Integer);
//  procedure   SetFrameIndex(FrameIndex__:Integer);
  public
    FrameIndex :Integer; // for speed, this is not a property but a simple public variable

    constructor Create(Canvas__:TCanvas; Pict__:TPict; OnDrawSprite__:TDrawSpriteEvent; OnHideSprite__:THideSpriteEvent);

    procedure   Clear; override;
    procedure   Draw(X,Y:Integer); override;
    procedure   DrawFrame(X__,Y__,FrameIndex__:Integer);

    property    FrameCount:Integer read fFrameCount write SetFrameCount;
//  property    FrameIndex:Integer read fFrameIndex write SetFrameIndex;
  end;

implementation

uses SysUtils,Forms,Classes,Dialogs,
     Misc_,SokUtil_,Game_,Main_;

constructor TSprite1.Create(Canvas__:TCanvas; Pict__:TPict; OnDrawSprite__:TDrawSpriteEvent; OnHideSprite__:THideSpriteEvent);
begin
//Inherited Create;
  fVisible    :=False;
  BackBitMap  :=nil;
  Clear;

  Canvas      :=Canvas__;
  Pict        :=Pict__;
  OnDrawSprite:=OnDrawSprite__;
  OnHideSprite:=OnHideSprite__;
  Enabled     :=True;
  Resize(-1,-1); // '(-1,-1)': use 'Pict.BitMap' size
end;

destructor  TSprite1.Destroy;
begin
  fVisible:=False; Clear;
  Inherited Destroy;
end;

procedure   TSprite1.Clear;
begin
  if Visible then Hide;
  BackBitMap.Free; BackBitMap:=nil;
  FillChar(R,SizeOf(R),0); BackRect:=R; fPictureSizeRect:=R;
  MaxDX:=1; MaxDY:=1;
  StopPosition.X:=0; StopPosition.Y:=0;
  fVisible:=False; Enabled:=False;
end;

procedure   TSprite1.MoveTo(NewX,NewY:Integer);
begin
  Canvas.CopyMode:=cmSrcCopy;
  if Visible then begin
     if      Assigned(BackBitMap  ) then Canvas.CopyRect(BackRect,BackBitMap.Canvas,PictureSizeRect)
     else if Assigned(OnHideSprite) then OnHideSprite(BackRect);
     fVisible:=False;
     end;

  BackRect:=Rect(Max(0,NewX),Max(0,NewY),Max(1,NewX+PictureSizeRect.Right),Max(1,NewY+PictureSizeRect.Bottom));
  if Assigned(BackBitMap) then BackBitMap.Canvas.CopyRect(PictureSizeRect,Canvas,BackRect);
  fVisible:=True; Draw(NewX,NewY);
end;

procedure   TSprite1.MoveTowardsXYSmoothly(NewX,NewY:Integer);
var i:Integer;
begin
  i:=NewX-R.Left; NewX:=R.Left+Sign(i)*MaxDX;
  i:=NewY-R.Top ; NewY:=R.Top +Sign(i)*MaxDY;
  MoveTo(NewX,NewY);
end;

procedure   TSprite1.Show;
begin
  if not Visible then MoveTo(R.Left,R.Top);
end;

procedure   TSprite1.Hide;
begin
  if Visible then begin
     fVisible:=False;
     if      Assigned(BackBitMap  ) then Canvas.CopyRect(BackRect,BackBitMap.Canvas,PictureSizeRect)
     else if Assigned(OnHideSprite) then OnHideSprite(BackRect);
     end;
end;

procedure   TSprite1.BeginDrag(X,Y:Integer);
begin
  MouseDX:=X-R.Left; MouseDY:=Y-R.Top; // [dx,dy]: offset from [top,left]
  MoveTo(R.Left,R.Top);
end;

procedure   TSprite1.Drag(X,Y:Integer);
begin
  MoveTo(X-MouseDX,Y-MouseDY);
end;

procedure   TSprite1.Resize(Width__,Height__:Integer);
begin
  fVisible:=False; // kludge: 'Resize' doesn't hide the sprite; hence, the sprite must not be visible at this time

  if   (Pict<>nil) and (Pict.BitMap<>nil) then begin
       if Width__ <0 then Width__ :=Pict.BitMap.Width;
       if Height__<0 then Height__:=Pict.BitMap.Height;
       end;

  Width__ :=Max(2,Width__);
  Height__:=Max(2,Height__);

  FillChar(BackRect,SizeOf(BackRect),0);
  if Assigned(OnHideSprite) or BitMapResize(BackBitMap,Width__,Height__) then
     fPictureSizeRect:=Rect(0,0,Width__,Height__)
  else begin
     BackBitMap.Free; BackBitMap:=nil;
     FillChar(fPictureSizeRect,SizeOf(fPictureSizeRect),0);
     raise Exception.Create(TEXT_MEMORY_FULL);
     end;
end;

procedure   TSprite1.Draw(X,Y:Integer);
begin
  R.Left :=X; R.Top:=Y;
  R.Right:=X+PictureSizeRect.Right; R.Bottom:=Y+PictureSizeRect.Bottom;
  if      Assigned(Pict)         then Pict.Draw(X,Y,Canvas)
  else if Assigned(OnDrawSprite) then OnDrawSprite(Canvas,R);
end;

procedure   TSprite1.SetVisibleStateWithoutUpdatingTheDisplay(Visible__:Boolean);
begin
  fVisible:=Visible__;
end;

constructor TSpriteN.Create(Canvas__:TCanvas; Pict__:TPict; OnDrawSprite__:TDrawSpriteEvent; OnHideSprite__:THideSpriteEvent);
begin
  Inherited;
  fFrameCount:=0; FrameIndex:=-1; //fFrameIndex:=-1;
end;

procedure   TSpriteN.Clear;
begin
  Inherited;
  fFrameCount:=0; FrameIndex:=-1; //fFrameIndex:=-1;
end;

procedure   TSpriteN.Draw(X,Y:Integer);
begin
  R.Left :=X; R.Top:=Y;
  R.Right:=X+PictureSizeRect.Right; R.Bottom:=Y+PictureSizeRect.Bottom;
  if Assigned(Pict) then
     if   FrameCount<=1 then
          Pict.Draw(X,Y,Canvas)
     else Pict.DrawRect(X,Y,CellToRect(FrameIndex,0,PictureSizeRect.Right,PictureSizeRect.Bottom),Canvas);
end;

procedure   TSpriteN.DrawFrame(X__,Y__,FrameIndex__:Integer);
begin
  FrameIndex:=FrameIndex__; // fFrameIndex:=FrameIndex__;
  Draw(X__,Y__);
end;

procedure   TSpriteN.SetFrameCount(FrameCount__:Integer);
begin
  if FrameCount__<>FrameCount then begin
     fFrameCount:=FrameCount__;
     end;
end;
{
procedure   TSpriteN.SetFrameIndex(FrameIndex__:Integer);
begin
  if FrameIndex__<>FrameIndex then begin
     fFrameIndex:=FrameIndex__;
     if Visible then begin Hide; Show; end;
     end;
end;
}
end.

