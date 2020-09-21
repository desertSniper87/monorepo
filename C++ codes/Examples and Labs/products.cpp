#include <iostream>
#include <conio.h>
using namespace std;

int main ()
{
    int a[100];
    int b;
    
    cin>> b;
    
    int i;
    int j=0;
    
    for ( i=1;i<=b;i++ )
    {
        if ( b%i==0 )
        {
             a[j] = i;
             j++;
        }
    }
    
    for ( i=0;i<j;i++ )
    {
        cout<< a[i]<< " ";
    }
    
    getch ();
    return 0;
}
