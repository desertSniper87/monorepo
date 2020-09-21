#include <stdio.h>
#include <conio.h>

int main (void)

{
    char ch;
    int i;
    
    printf ("Enter a character: \n");
    while (1)
    {
          ch = getche ();
          for ( i=128;i>0;i=i/2 )
          {
              if ( i&ch ) printf ("1");
              else printf ("0");
          }
          printf ("\n");
    }
    
    getchar ();
    return 0;
}
