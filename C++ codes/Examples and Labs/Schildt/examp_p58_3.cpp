#include <iostream>
using namespace std;

class box
{
      double length;
      double width;
      double height;
      double volume;
      public:
             box (double l, double w, double h);
             void vol();
};

box::box (double l,double w,double h)
{
    length = l;
    width = w;
    height = h;
    
    volume = l * w * h;
}

void box::vol()
{
     cout<< "The volume is: "<< volume << "\n";
}

int main ()
{
    doubleo x,y,z;
    
    cout<< "Please enter length, width and height: \n";
    cin>> x >> y >> z;
    box ob1(x,y,z);
    ob1.vol();
    
    getchar ();
    getchar ();
    return 0;
}
