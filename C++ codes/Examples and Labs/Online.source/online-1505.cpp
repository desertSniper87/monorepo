#include <iostream>
#include <cmath>

using namespace std;

class point
{
	double x,y;
	public:
		point(double a, double b);
		point() 
		{
			x = 0;
			y = 0;
		}
		void setx (double a);
		void sety (double b);
		void setxy (double a, double b);
		double getx();
		double gety();

		double disOrigin();
		double distance(point P);
		double PolarAngle();
		point centroid (point p, point q);
		bool onXaxis ();
		bool onYaxis();
};


point::point (double a,double b)
{
	x = a;
	y = b;
}

void point::setx (double a)
{
	x = a;
}

void point::sety (double b)
{
	y = b;
}

void point::setxy (double a,double b)
{
	x = a;
	y = b;
}

double point::getx()
{
	return x;
}

double point::gety()
{
	return y;
}

double point::disOrigin()
{
	double x2;
	double y2;
	x2 = x*x;
	y2 = y*y;
	return sqrt(x2+y2);
}

double point::distance( point p )
{
	double disA;
	double disB;
	double i;
	double j;
	i = p.x;
	j = p.y;
	disA = x-i;
	disB = y-j;
	double disdisA;
	double disdisB;
	disdisA = disA*disA;
	disdisB = disB*disB;
	double disdis;
	disdis = disdisA + disdisB;
	double dis;
	dis  = sqrt (disdis);
	return dis;
	 
}

double point::PolarAngle()
{
	double byxy;
	byxy = y / x;
	double rad;
	rad = atan (byxy) ;
	double deg;
	deg = rad * 180 / 3.1416;
	return deg;
}

point point::centroid (point p, point q)
{
	point t;

	double i = p.x;
	double j = p.y;
	double yy=q.x;
	double z=q.y;
	double centx;
	double centy;

	centx = ( x+i+yy )/3.0;
	centy = ( y+j+z )/3.0;
	
	t.x = centx;
	t.y = centy;

	return t;
} 
 

bool point::onXaxis ()
{
	if (y==0.0)
		return true;
	else
		return false;
}

bool point::onYaxis ()
{
	if (x==0.0)
		return true;
	else
		return false;
}

int main ()
{
	point p,q(7,14),r,s;

	p.setx(1);
	p.sety(2);
	r.setxy(18,36);

	cout<< q.getx()<< " "<< q.gety()<< endl;
	cout<< s.getx()<< " "<< s.gety()<< endl;
	cout<< r.disOrigin()<< endl;
	cout<< q.PolarAngle() << endl;
	cout<< q.distance(p) << endl;
	cout<< q.onXaxis()<< " "<< s.onYaxis()<< endl;

	s = p.centroid (q,r);
	cout<< s.getx()<<" "<< s.gety()<< endl;

	return 0;
}
