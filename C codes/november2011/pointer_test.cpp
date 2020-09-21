#include <stdio.h>

int main (void)

{
    int i;
    int *ptr;
    
    i=1;
    //ptr = i;
    
    printf ("%d",(void*)*ptr);
    
    *ptr = i;
     printf ("%d",(void*)*ptr);
     
     getchar ();
     return 0;
}
