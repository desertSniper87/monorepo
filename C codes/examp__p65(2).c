#include <stdio.h>

int XOR (int a, int b);
int main (void)

{
    int p,q;

    printf ("Enter P (0 or 1): ");
    scanf ("%d",&p);
    printf ("Enter Q (0 or 1): ");
    scanf ("%d",&q);

    printf("P AND Q: %d", p && q);
    printf("P OR Q: %d", p || q);
    printf("P XOR Q: %d", XOR (p,q));

    return 0;

}

int XOR (int a, int b)
{
    return (a||b) && !(a&&b);
}

