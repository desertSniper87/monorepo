#include <stdio.h>

int main (void)

{
    int i,j;
    int *ptr;
    
    i=2;
    j=3;
    ptr = &j;
    
    
    printf ("%d\t%p\n",i,(void *)&i);
    printf ("%d\t%p\n",j,(void *)&j);
    printf ("%d\t%p\n",ptr,(void *)&ptr);
    
    printf ("The value of integer indicated by the pointer: %d" ,*ptr);
    
    getchar();
    return 0;
}
