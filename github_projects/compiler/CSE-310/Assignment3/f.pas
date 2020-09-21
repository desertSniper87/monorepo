program example(input, output);
var x, y : integer;
var q : real;
var c : array [3 .. 10] of integer;
begin
	x:=4;
	x:=123+432;
	x:=-x;
	y:=7;
	x := 3+2+5+6-2-1*5;
	y := (0 and 4)*4+1 mod 5;
{
}
	if(x>3) then
		x:=x+5
	else
		x:=x+4;
	
	write(x);
	while (x<y) do
		begin
			c := x;
			x := x+1
		end
end
