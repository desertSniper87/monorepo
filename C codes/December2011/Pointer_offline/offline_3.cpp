#include <stdio.h>
#include <conio.h>
#include <string.h>
int conof (char *s1, char *s2);

int main (void)

{
	char s1[100];
	char s2[100];

	printf ("Please enter s1: \n");
	gets (s1);
	printf ("Please enter s2: \n");
	gets (s2);

	int x;
    x = conof ( &s1[0], &s2[0] );

    if ( x == 1 )
    printf ("YES");
    else
    printf ("NO");

	getch();
	return 0;
}

int conof (char *s1, char *s2)
{
    int x = strlen (s1);
    int y = strlen (s2);
    int exist = 1;

    int i,j;

    for ( i=0;i<x;i++ )
    {
        for ( j=0;j<y;j++ )
        {
            if ( *( s1 + i ) != *( s2 + j )  )
            exist = 0;
        }
    }

    return exist;
}
