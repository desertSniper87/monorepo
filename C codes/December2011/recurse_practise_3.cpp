#include <stdio.h>
int power ( int x, int i );

int main ( void )
{
    int i = 0;
    int x = 0;
    
    printf ("Enter x: ");
    scanf ("%d",&x);
    printf ("Enter i: ");
    scanf ("%d",&i);
    
    printf ("%d", power ( x , i ) );
    
    scanf ("%d",&i);
    return 0;
}

int power ( int x, int i )
{
    if ( i==0 ) return 1;
    
    else
    {
        return x * power ( x, i-1 );
    }
}
