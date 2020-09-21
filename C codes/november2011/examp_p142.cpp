#include <stdio.h>

int main (void)

{
    int i;
    int a1[10], a2[10];
    
    for ( i=0;i<10;i++ ) a1[i] = i+1;
    
    for ( i=0;i<10;i++ ) a2[i] = a1[i];
    
    for ( i=0;i<10;i++ ) printf ("%d\t",a2[i]);
    
    getchar ();
    return 0;
}
