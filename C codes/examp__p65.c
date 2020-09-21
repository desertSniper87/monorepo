#include <stdio.h>

int xor (int a, int b)

{
    int x;
    x = (a||b) && !(a&&b);
    
    return x;
    
}

int main (void)

{   
    int p,q;
    int result = xor (p,q);
    
    printf ("Enter p: ");
    scanf ("%d",&p);
    printf ("Enter q: ");
    scanf ("%d",&q);
    
    printf ("The result of xor function: %d",&result);
    
    getch();
    return 0;
    
}
    
    
