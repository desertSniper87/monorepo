#include <stdio.h>

int main (void)

{
    int x,i;
    scanf ("%d",&x);
    
    for ( i=17; i<=x; i++ )
        if( i%17==0 ) printf("%d ",i);
        
    getch();
    
    return 0;
    
}
    
