#include <stdio.h>

int main (void)

{
    int i;
    int *p, q;
    
    p = &q;
    
    *p = 100;
    
    printf ("%d",q);
    
    
    scanf ("%d",&i);
    
    return 0;
}
