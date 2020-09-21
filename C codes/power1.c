#include <stdio.h>

int main (void)

{
    int n, x, result;

    printf ("Enter n: ");
    scanf ("%d",&n);
    printf ("Enter x: ");
    scanf ("%d",&x);

    result = power ( n, x );

    printf ("%d",result);

    getch ();
    return 0;
}

int power ( int n, int x )
{
    int raven , i = 1;

    for ( i=1; i<=n; i++ )
        {
            raven = raven * n;
        }
    return raven;
}
