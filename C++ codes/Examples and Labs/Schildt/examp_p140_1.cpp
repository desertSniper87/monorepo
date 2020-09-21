#include <iostream>
#include <conio.h>
using namespace std;

void f(int *p);

int main ()
{
    int i = 0;
    
    f( &i );
    
    cout<< "The new value of i = "<< i;
    
    getchar ();
    return 0;
}

void f(int *p)
{
     *p = 100;
}
