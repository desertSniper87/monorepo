#include <stdio.h>

int main (void)

{
    int i;
    int *p;
    int array[100];
    
    p=&array[i];
    
    for ( i=0;i<9;i++ )
    {
        array[i] = i+1;
    }
    
    for ( i=0;i<9;i++ )
    {
        printf ("%d\n",*p);  
    }
    
    scanf ("%d",&i);
    
    return 0;
}
