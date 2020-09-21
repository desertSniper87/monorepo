#include <stdio.h>

int main (void)

{
    int *p , q;
    
    q = 100;
    
    p = &q;
    
    printf ("%d",*p);
    
    scanf ("%d",&q);
    
    return 0; 
}
