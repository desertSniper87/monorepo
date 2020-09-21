#include <stdio.h>
#include <conio.h>
//using namespace std;

void f(int &p);

int main ()
{
    int i = 0;
    
    f( i );
    
    printf ("The new value of i = %d",i);
    
    getchar ();
    return 0;
}

void f(int &p)
{
     p = 100;
}
        
