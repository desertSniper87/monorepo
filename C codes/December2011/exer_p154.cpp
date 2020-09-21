#include <stdio.h>

int main (void)

{
    int i,j,k,t=1;
    
    int orion[3][3][3];
    
    for ( i=0;i<3;i++ )
    { 
        for ( j=0;j<3;j++ )
        {
            for ( k=0;k<3;k++ )
            {
                orion [i][j][k] = t++;
            }
        }
    }
    
    for ( i=0;i<3;i++ )
    { 
        for ( j=0;j<3;j++ )
        {
            for ( k=0;k<3;k++ )
            {
                printf ("%d\t",orion [i][j][k]);
            }
            printf ("\n");
        }
        printf ("\n");
    }
    
    scanf ("%d",&i);
    return 0;
}
