#include <stdio.h>

int main (void)

{
    int i;
    
    for ( i=1; i<=100; i++ )
    {
        if ( i%2 == 0 )
        {
             continue;
        }
        printf ("%d\t",i);
    }
    
    getch ();
    return 0;
}
