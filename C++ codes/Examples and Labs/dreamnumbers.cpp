#include <iostream>
#include <conio.h>
using namespace std;

int main ()
{
    long x;
    long y;
    int iter;
    
    cout<< "Please enter number 1:\t";
    cin>> x;
    
    cout<< "Please enter number 2:\t";
    cin>> y;
    
    cout<< "Please enter iterations:\t";
    cin>> iter;
    
    long numbig, numsmall, temp;
    
    if ( x>y )
    {
         numbig = x;
         numsmall = y;
    }
    else 
    {
         numsmall = x;
         numbig = y;
    }
    
    for ( ;iter!=-1;iter-- )
    {
        temp = numbig;
        numbig = numbig + numsmall;
        cout<< numbig<< "\n";
        numsmall = temp;
        
    }
    
    getchar ();
    getchar ();
    return 0;
}
    
