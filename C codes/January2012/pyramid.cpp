#include <stdio.h>
#include <conio.h>

int main (void)

{
    int n = 0;
    int i = 0;
    int line = 1;
    int x = 0;
    
    printf ("Enter n: \n");
    scanf ("%d",&n);
    i = (n-4)/2;
    
    while ( n>0 )
    {
          for ( x=0;x<i;x++ )
              printf (" ");
          for ( x=0;x<(line%3);x++ )
          {
              printf ("*");
              if (line%3)
                 printf ("*");
          }    
          for ( x=0;x<(line/3);x++ )
              printf (" ");
          printf (" ");
          for ( x=0;x<(line%3);x++ )
          {
              printf ("*");
              if (line%3)
                 printf ("*");
          }    
          for ( x=0;x<(i);x++ )
              printf (" ");
          printf ("\n");
          line++;
          n--;
          i = i-1;
    }
    
    getch();
    return 0;
    
}
