#include <stdio.h>

int fact (int n);

int main (void)

{
    int i = 0;
    int n;
    printf ("Please enter n: \n");
    scanf ("%d",&n);
    
    printf ("%d",fact( n ));
    
    scanf ("%d",&i);
    return 0;
}

int fact (int n)
{
    int f;
    
    if ( n==1 )
    return 1;
    
    else 
    {
         return f = n*fact (n-1);
    }
}


