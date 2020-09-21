#include <stdio.h>

int main (void)

{
    int n, i;

    printf ("Please enter n:\n");
    scanf ("%d",&n);

    for ( i=1; n!=1; i++)
    {
        if (( n%2 ) == 0)
        {
            printf ("%d\t",n/2);
            n = n/2;
        }
        else
        {
            printf ("%d\t",3*n+1);
            n = 3*n+1;

        }
    }
    return 0;
}
