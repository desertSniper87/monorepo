#include <stdio.h>

int main (void)

{
    int i, *p;
    
    p = &i;
    
    for ( i=0;i<10;i++)
    {
        printf ("%d",*p);
    }
    
    scanf ("%d",&i);
    
    return 0;
}
