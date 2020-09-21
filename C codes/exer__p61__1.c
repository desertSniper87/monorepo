#include <stdio.h>

int main (void)

{   
    int i;
    
    for ( i=1; i<=10; i++ )
        printf ("%d \t%d \t%d \n ",i,i*i,i*i*i);
    
    getch();    
    return 0;
    
}
