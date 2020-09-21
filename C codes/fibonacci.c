#include <stdio.h>

int main (void)

{
    int i=1, a=0, b=1, c=0, n;

    printf ("Please enter n:");
    scanf ("%d",&n);

    printf ("%d\n",1);

    while (i<n)
    {
        c = a+b;
        printf ("%d\n",c);

        a=b;
        b=c;
        i++;

    }

    getch ();
    return 0;
}
