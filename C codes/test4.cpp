#include <stdio.h>

int main (void)

{
    char name;
    printf ("Please enter your name: ");
    
    scanf ("%lc",&name);
    
    printf ("Hello %lc",name);
    
    return 0;  
}
