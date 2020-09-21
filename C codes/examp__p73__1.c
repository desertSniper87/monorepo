#include <stdio.h>

int main (void)

{
    char ch,i=100;
    
    for (;i<200;)
    {
    ch = getche () ;
    printf ("\tThe ASCII code for the char. is %d \n",ch);
    }
    getch ();
    return 0;
    
}
