#include <stdio.h>

int main (void)

{
    int n,i,j;

    printf ("Please enter n: ");
    scanf ("%d",&n);

    for ( i=1; i<=n; i++ )
    {
            for ( j=1; j<=i; j++ )
            {
                    printf ("* ");
            }
            printf ("\n");
    }
    getch ();
    return 0;
}

