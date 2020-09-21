#include <stdio.h>

int main (void)

{
    char ch;
    
    printf ("Please enter your message: ");
    ch = getche();
    
    while ( ch != '\r' )/*can also use '\n'*/
    {
          printf ("%c",ch+1);
          ch = getche();
    }
    
    getch ();
    return 0;
}
