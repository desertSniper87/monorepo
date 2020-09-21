#include <stdio.h>

int main (void)

{
    int i;
    
    for ( ;; )
    {
        if (i=0) i=1;
        printf ("%d",i);
        else if (i=1) i=0;
        printf ("%d",i);
        
    }
    
    return 0;
}
