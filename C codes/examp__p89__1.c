#include <stdio.h>

int main (void)

{
    int i;
    char ch;
    
    for ( i=1; i<=10000; i++ )
    {
        if (!(i%6))
        {
                 printf ("%d more ?( Y/N )\n",i);
                 ch = getch ();
                 if ( ch == 'N') break;
        }
    }
    getch ();
    return 0;
}
