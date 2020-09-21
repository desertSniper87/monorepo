unit Sprite_;

interface

uses
  Windows, Classes, Graphics;

type
  TSprite = class
  private
    BackBitMap,BitMap,MaskBitMap:TBitMap;
    Canvas:TCanvas;
    fVisible:Boolean;
    X,Y,W,H:Integer;
  protected
    procedure   SetVisible(Visible__:Boolean);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure   Hide;
    procedure   Initialize(Canvas__:TCanvas; BitMap__,MaskBitMap__:TBitMap);
    procedure   MoveTo(X__,Y__:Integer);
    procedure   Show;

    property    Visible:Boolean read fVisible write SetVisible;

  end;

implementation

constructor TSprite.Create;
begin
  X:=0; Y:=0; W:=0; H:=0; fVisible:=False; Canvas:=nil;
  BitMap:=nil; MaskBitMap:=nil;
  BackBitMap:=TBitMap.Create; BackBitMap.Canvas.CopyMode:=cmSrcCopy;
end;

destructor TSprite.Destroy;
begin
  BackBitMap.Free;
end;

procedure TSprite.Hide;
begin
  if fVisible then begin
     fVisible:=False;
//   Canvas.CopyRect(Rect(X,Y,X+W,Y+H),BackBitMap.Canvas,Rect(0,0,W,H));
     BitBlt(Canvas.Handle,X,Y,W,H,BackBitMap.Canvas.Handle,0,0,cmSrcCopy);
     end;
end;

procedure TSprite.Initialize(Canvas__:TCanvas; BitMap__,MaskBitMap__:TBitMap);
begin
  Canvas:=Canvas__; Canvas.CopyMode:=cmSrcCopy;
  BitMap:=BitMap__; MaskBitMap:=MaskBitMap__;

  fVisible:=False;
  X:=0; Y:=0; W:=BitMap.Width; H:=BitMap.Height;
  BackBitMap.Width:=W; BackBitMap.Height:=H;
end;

procedure TSprite.MoveTo(X__,Y__:Integer);
begin
  Hide;
  X:=X__; Y:=Y__;
  Show;
end;

procedure TSprite.SetVisible(Visible__:Boolean);
begin
  if   Visible__ then Show
  else Hide;
end;

procedure TSprite.Show;
var R,R0:TRect;
begin
  if not fVisible then begin
     fVisible:=True;
     R0:=Rect(0,0,W,H); R:=Rect(X,Y,X+W,Y+H);
//   Canvas           .CopyMode:=cmSrcCopy;
//   BackBitMap.Canvas.CopyRect(R0,Canvas       ,R );
     BitBlt(BackBitMap.Canvas.Handle,0,0,W,H,Canvas.Handle,X,Y,cmSrcCopy);

//   Canvas           .CopyMode:=cmSrcAnd;
//   Canvas           .CopyRect(R ,MaskBitMap.Canvas,R0);
     BitBlt(Canvas.Handle,X,Y,W,H,MaskBitMap.Canvas.Handle,0,0,cmSrcAnd);

//   Canvas           .CopyMode:=cmSrcPaint;
//   Canvas           .CopyRect(R ,BitMap.Canvas,R0);
//   Canvas           .CopyMode:=cmSrcCopy;
     BitBlt(Canvas.Handle,X,Y,W,H,BitMap.Canvas.Handle,0,0,cmSrcPaint);
     end;
end;

end.
