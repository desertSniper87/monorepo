#include <stdio.h>

int main (void)

{
    int i,j;
    int rose [10][10];
    
    for ( i=0;i<10;i++ )
    {
        for ( j=0;j<10;j++ )
        {
            rose [i][j] = i*j;
        }
    }
    
    for ( i=0;i<10;i++ )
    {
        for ( j=0;j<10;j++ )
        {
            printf ("%d\t", rose[i][j] );
        }
        printf ("\n");
    }
    
    scanf ("%d",&i);
    return 0;
}
