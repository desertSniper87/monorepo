#include <stdio.h>

int main (void)

{
    int n,i;

    printf ("Please enter n: ");
    scanf ("%d",&n);

    for ( i=1; i<=n; i++ )
        {
            printf ("* ");
        }

    getch ();
    return 0;
}
