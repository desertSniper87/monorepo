#include <stdio.h>

int main (void)

{
    int i,j,total=0;
    
    for ( ;i!=0; )
    {
        printf ("Enter next number: (0 to stop)");
        scanf ("%d",&i);
        
        printf ("Enter number again: ");
        scanf ("%d",&j);
        
        if ( i!=j )
        {
             printf ("Mismatch!\n");
             continue;
        }
        total = total + i;
    }
    printf ("The total sum is %d",total);
    
    getch ();
    return 0;
}
