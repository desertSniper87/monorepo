#include <stdio.h>
void increment (int *x);

int main (void)

{
    int x;
    scanf ("%d",&x);
    
    increment (&x);
    
    printf ("%d",x);
    
    scanf ("%d",&x);
    return 0;
}


void increment (int &x)
{ 
    x++;  
}
