#include <stdio.h>

int main (void)

{
    int i,n,j,match;
    char a[100];
    
    printf ("Enter 10 numbers: ");
    scanf ("%d",&a);
    
    for ( i=0; i<10 && match == 0; i++ )
    {
        for ( j=i+1; j<10; j++ )
            {
                     if ( a[i] == a[j] )
                        match = 1;
                        break;       
            }     
    }
    if (match == 1)
    {
             printf ("Match!");
    }
    scanf ("%d",&i);
    return 0;    
}
