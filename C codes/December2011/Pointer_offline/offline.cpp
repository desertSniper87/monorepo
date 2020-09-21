#include <stdio.h>
#include <conio.h>
int firstindex (char *s1, char ch);
int main (void)

{
    char s1[100];
    char ch;
    int x;

    printf ("Please enter s1: \n");
    gets (s1);
    printf ("Please enter ch: \n");
    scanf ("%c",&ch);

    x = firstindex ( &s1[0],ch );
    printf ("%d",x);

	getch();
	return 0;
}


int firstindex (char *s1, char ch)
{
    int i;
    int x = -1;

    for ( i=0;*(s1+i);i++ )
    {
        if ( *( s1+i ) ==ch )
        {
        x = i;
        break;
        }
    }
    return x;
}
