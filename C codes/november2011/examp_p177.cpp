#include <stdio.h>

int main (void)

{
    int i;
    
    int a[10] = {10,20,30};
    
    int *p;
    
    p=a;
      
    printf ("%d\t%d\t",*(p+1),*(p+2));
    
    scanf ("%d",&i);
    
    return 0;
}
