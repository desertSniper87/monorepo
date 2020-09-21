#include <iostream>
#include <conio.h>
using namespace std;

inline int even (int x)
{
       return (!(x%2));
}

int main ()
{
    if ( even(3) )
    {
         cout<< "EVEN";
    }
    
    else 
    {
         cout<< "ODD";
    }
    
    getch();
    return 0;
}
