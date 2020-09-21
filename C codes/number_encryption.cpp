#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main (void)

{
    char s1[100];
    int x,y,i,j;


    printf ("Enter a number: ");
    gets (s1);

    for ( i=0;i<strlen(s1);i++ )
    {

        y = x*x;

        for ( ;y>9; )
        {
            j = y-9;
        }

        x = atoi (s1[i]) + y;
        s1[i] = itoa (x);

    }

    printf (s1);


}
