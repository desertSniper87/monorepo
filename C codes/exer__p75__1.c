#include <stdio.h>

int main (void)

{
    int ch,smallch,newch,i;
    
    printf ("Please enter 10 characters: ");
    
    scanf ("%c",&ch);
    printf ("\n");
    smallch = ch;
    
    for ( i=2; i<=10; i++)
        { newch = getche ();
          printf ("\n");
          if ( newch < ch )
             smallch = newch ;}
           
    printf ("The smallest character is %c",smallch);
    
    getch ();
    return 0;
}
