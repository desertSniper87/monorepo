#include <stdio.h>

int main (void)

{
    int i;
    
    for ( i=65; i<=127 ; i++ )
        printf ("Standard key -- %c\t ASCII key -- %d\n",i,i);
        
        getch();
        return 0;
        
}
