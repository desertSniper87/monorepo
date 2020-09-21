unit Yantra_;

interface

uses Windows,Classes,Graphics;

procedure Yantra(BitMap__:TBitMap; BackgroundColor__:TColor);

implementation

procedure Yantra(BitMap__:TBitMap; BackgroundColor__:TColor);
var R2,Radius:Integer; Origin:TPoint; Points:array[0..50] of TPoint;

  procedure Lines(Count__:Integer);
  var i:Integer;
  begin
    if Count__<=High(Points) then begin
       Points[Count__]:=Points[0]; // add a line back to the first point
       for i:=Low(Points) to High(Points) do with Points[i] do begin
           Inc(X,Origin.X); Inc(Y,Origin.Y);
           end;
       BitMap__.Canvas.PolyLine(Slice(Points,Succ(Count__)));
       end;
  end;

  procedure Frame(Petals__:Integer);
  var i,j:Integer; R:TRect;

    procedure Petals(Petals__,InnerRadius__,OuterRadius__:Integer);
    var i,r,PetalRadius:Integer; V:Double; A,B,C,D:TPoint;
    begin
      if Petals__>=3 then with BitMap__.Canvas do begin
         V:=2*PI/Petals__;
         A.X:=-Trunc(InnerRadius__*Sin(V/2.0)+0.5);
         A.Y:= Trunc(InnerRadius__*Cos(V/2.0)+0.5);
         B.X:= 0;
         B.Y:= InnerRadius__+(9*(OuterRadius__-InnerRadius__) div 10);
         C.X:=-A.X;
         C.Y:= A.Y;

         PetalRadius:=Trunc((((B.Y-A.Y)*(B.Y-A.Y)+((C.X-A.X)*(C.X-A.X)/4))/(2*(B.Y-A.Y)))+0.5);
         r:=B.Y-PetalRadius;
         Brush.Color:=clLtGray; //Pen.Color:=clDkGray;

         for i:=0 to Pred(Petals__) do begin
             D.X:=Trunc(r*Cos(i*V)+0.5);
             D.Y:=Trunc(r*Sin(i*V)+0.5);
             Ellipse(Origin.X+D.X-PetalRadius,Origin.Y+D.Y-PetalRadius,
                     Origin.X+D.X+PetalRadius,Origin.Y+D.Y+PetalRadius);
             end;
         end;
    end;

  begin // Frame
    Radius:=0;
    if (BitMap__<>nil) and (BitMap__.Width>=64) and (BitMap__.Height>=64) then
       with BitMap__ do with Canvas do begin
         Origin.X:=Width div 2; Origin.Y:=Height div 2;
         if Origin.X<=Origin.Y then Radius:=Origin.X
         else Radius:=Origin.Y;
         if Radius>128 then R2:=3
         else R2:=2;

         R:=Rect(0,0,Width,Height); Brush.Color:=BackgroundColor__; Pen.Color:=clBlack; Pen.Color:=clDkGray;
         if Brush.Color<>Pen.Color then FillRect(R);

         if Origin.X<=Origin.Y then Radius:=Origin.X
         else Radius:=Origin.Y;

         for j:=-1 to 1 do
             if j<>0 then begin

                Points[0].X:= Round(Cos(PI/8)*Radius)+R2*j;
                Points[0].Y:=-Round(Sin(PI/8)*Radius)-R2*j;

                Points[4].X:= Round(Cos(PI/4)*Radius)+R2*j;
                Points[4].Y:=-Points[4].X;

                Points[1].X:= Points[4].X+((Points[0].X-Points[4].X) div 2)-2*R2*j-R2;
                Points[1].Y:= Points[0].Y;

                Points[2].X:= Points[1].X;
                Points[2].Y:= ((Points[1].Y+R2*j) div 2)-R2*j;

                Points[3].X:= Points[4].X;
                Points[3].Y:= Points[2].Y;

                Points[8].X:=-Points[0].Y;
                Points[8].Y:=-Points[0].X;

                Points[7].X:=-Points[1].Y;
                Points[7].Y:=-Points[1].X;

                Points[6].X:= Points[7].X-(Points[2].Y-Points[1].Y);
                Points[6].Y:= Points[7].Y;

                Points[5].X:= Points[6].X;
                Points[5].Y:= Points[4].Y;

                for i:=9 to 17 do with Points[i] do begin
                    X:=-Points[Abs(i-17)].X; Y:= Points[Abs(i-17)].Y;
                    end;

                for i:=18 to 35 do with Points[i] do begin
                    X:= Points[Abs(i-35)].X; Y:=-Points[Abs(i-35)].Y;
                    end;

                Lines(36);
                end;

         Brush.Color:=clDkGray; FloodFill(Points[0].X-1,Points[0].Y+1,BackgroundColor__,fsSurface);

         Brush.Style:=bsClear; 
         Radius:=Trunc((Cos(PI/4)*(Radius-R2))+0.5);
         Ellipse(Origin.X-Radius,Origin.Y-Radius,Origin.X+Radius,Origin.Y+Radius);
         Dec(Radius,2*R2);
         Ellipse(Origin.X-Radius,Origin.Y-Radius,Origin.X+Radius,Origin.Y+Radius);
         Brush.Color:=clDkGray; FloodFill(Origin.X-Radius-1,Origin.Y,BackgroundColor__,fsSurface);

         i:=Radius;
         Radius:=(Radius*4) div 5;
         Petals(Petals__,Radius,i);

         Brush.Color:=BackgroundColor__; Brush.Style:=bsSolid;
         Ellipse(Origin.X-Radius,Origin.Y-Radius,Origin.X+Radius,Origin.Y+Radius);
         if Radius>2*R2 then Dec(Radius,2*R2);
         Ellipse(Origin.X-Radius,Origin.Y-Radius,Origin.X+Radius,Origin.Y+Radius);
         Brush.Color:=clDkGray; FloodFill(Origin.X-Radius-1,Origin.Y,BackgroundColor__,fsSurface);

         Brush.Color:=clBlack; Brush.Style:=bsSolid; //Pen.Color:=clDkGray;
         Ellipse(Origin.X-R2,Origin.Y-R2,Origin.X+R2,Origin.Y+R2);
         end;
  end;

  procedure Durga;
  var C,S:Double;
  begin
    Frame(8);
    if Radius>1 then with BitMap__.Canvas do begin
       Dec(Radius,1); //Pen.Color:=clDkGray;

       Points[0].X:=0;
       Points[0].Y:=Radius;
       Points[1].X:= Trunc((Cos(PI*30/180)*Radius)+0.5);
       Points[1].Y:=-Trunc((Sin(PI*30/180)*Radius)+0.5);
       Points[2].X:=-Points[1].X;
       Points[2].Y:= Points[1].Y;
       Lines(3);

       Points[0].X:=-Round(Cos(PI/4)*Radius);
       Points[0].Y:=-Points[0].X;
       Points[1].X:=-Points[0].X;
       Points[1].Y:= Points[0].Y;

       C          :=Cos(PI*4/3);
       S          :=Sin(PI*4/3);
       Points[2].X:=-Trunc(Points[1].X*C - Points[1].Y*S+0.5);
       Points[2].Y:= Trunc(Points[1].X*S + Points[1].Y*C+0.5)-1; // '-1': kludge: gives the best result for a 256x256 image

       C          :=Cos(PI*2/3);
       S          :=Sin(PI*2/3);
       Points[3].X:= Trunc(Points[1].X*C - Points[1].Y*S+0.5);
       Points[3].Y:= Trunc(Points[1].X*S + Points[1].Y*C+0.5)-1; // '-1': kludge: gives the best result for a 256x256 image
       Points[4].X:=-Points[3].X;
       Points[4].Y:= Points[3].Y;
       Points[5].X:=-Points[2].X;
       Points[5].Y:= Points[2].Y;

       Lines(6);

       Brush.Color:=clLtGray;
       FloodFill(Origin.X-R2-1,Origin.Y         ,BackgroundColor__,fsSurface);
       FloodFill(Origin.X     ,Origin.Y+Radius-2,BackgroundColor__,fsSurface);
       FloodFill(Points[0].X+2,Points[0].Y-2    ,BackgroundColor__,fsSurface);
       FloodFill(Points[1].X-2,Points[1].Y-2    ,BackgroundColor__,fsSurface);
       FloodFill(Points[2].X  ,Points[2].Y+2    ,BackgroundColor__,fsSurface);
       FloodFill(Points[3].X+2,Points[3].Y-2    ,BackgroundColor__,fsSurface);
       FloodFill(Points[4].X-2,Points[4].Y-2    ,BackgroundColor__,fsSurface);
       FloodFill(Points[5].X  ,Points[5].Y+2    ,BackgroundColor__,fsSurface);
       FloodFill(((Points[2].X+Points[3].X) div 2)-2,((Points[2].Y+Points[3].Y) div 2)-2,BackgroundColor__,fsSurface);
       FloodFill(((Points[4].X+Points[5].X) div 2)+2,((Points[4].Y+Points[5].Y) div 2)+2,BackgroundColor__,fsSurface);
       Brush.Color:=BackgroundColor__;
       FloodFill(Origin.X     ,Origin.Y         ,clBlack,fsSurface);
       FloodFill(Origin.X     ,Origin.Y         ,clLtGray,fsBorder);
       end;
  end;

begin
  Durga; // 'Durga Yantra' is the only one implemented
end;

end.
