#include <iostream>
using namespace std;

template <class X>void  swapargs ( X &a, X &b )
{
        X temp;

        temp = a;
        a = b;
        b = temp;
}

void swapargs ( int a, int b )
{
    int temp;

    temp = a;
    a = b;
    b = temp;
}

int main ()
{
    int a = 10;
    int b = 20;
    float x = 101.20;
    float y = 33.33;

    swapargs ( a,b );
    swapargs ( x,y );

    cout<< "Swaped ints:"<< a<<' '<< b<< endl;
    cout<< "Swaped floats"<< x<<' ' << y<< endl;

    return 0;
}
