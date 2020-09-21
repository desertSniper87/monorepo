#include <iostream>
#include <conio.h>
using namespace std;

void swapargs ( int &a,int &b );
void swapargs_withpointers ( int *a, int *b );

int main ()
{
    int i = 1220;
    int j = 4312;
    
    cout<< "i = "<< i<< "\tj = "<< j<< "\n";
    
    swapargs( i,j );
    
    cout<< "i = "<< i<< "\tj = "<< j<< "\n";
    
    swapargs_withpointers ( &i,&j );
    
    cout<< "i = "<< i<< "\tj = "<< j<< "\n";
    
    getch ();
    return 0;
}

void swapargs ( int &a,int &b )
{
     int t;
     
     t = a;
     a = b;
     b = t;
}

void swapargs_withpointers ( int *a, int *b )
{
     int t;
     
     t = *a;
     *a = *b;
     *b = t;
}
