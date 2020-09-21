#include <iostream>
#include <conio.h>
using namespace std;

template <class X> void swapargs ( X &a, X &b )
{
         X temp;
         
         temp = a;
         a = b;
         b = temp;
}

int main ()
{
    int a, b;
    float x, y;
    
    a = 10;
    b = 20;
    
    x = 10.20;
    y = 20.10;
    
    
    cout<< "Before:"<< endl;
    
    cout<< a<< ' '<< b<< endl;
    cout<< x<< ' '<< y<< endl;
    
    swapargs (a,b);
    swapargs (x,y);
    
    cout<< "After:"<< endl;
    
    cout<< a<< ' '<< b<< endl;
    cout<< x<< ' '<< y<< endl;
    
    getch ();
    return 0;
}
