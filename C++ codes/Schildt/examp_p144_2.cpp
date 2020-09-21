#include <iostream>
#include <conio.h>
#include <cmath>
using namespace std;

void round (double &num);

int main ()
{
    double i = 100.9932;
    
    cout<< "Rounding "<< i;
    round (i);
    cout<< " into"<< i;
    
    getch ();
    return 0;
}

void round (double &num)
{
     double value;
     double frac;
     
     frac = modf ( num,&value );
     
     if ( frac>=0.5 )
     {
          num = val+1.0;
     }
     else 
     {
          num = val;
     }
}
