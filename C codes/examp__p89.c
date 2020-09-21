#include <stdio.h>

int main (void)

{
    int i;
    
    for ( i=1; i<=100; i++)
    {
        printf ("%d\t",i);
        if ( i==10 ) break;
    }
    
    getch ();
    return 0;
    
}
