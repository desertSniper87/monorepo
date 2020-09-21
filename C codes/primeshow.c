#include <stdio.h>

int main (void)

{
    int n,i;

    printf ("Enter n: ");
    scanf ("%d",&n);

    for ( i=1; i<=n; i++ )
        {
            if (is_prime (i) == 1)
                printf ("%d\t",i);
        }

    getch();
    return 0;

}

int is_prime (int nu)
{
    int i, isprime;

    for ( i=2; i<=nu/2; i++ )
        {if ( nu%i==1 ) isprime = 0;}

    return isprime;

}

