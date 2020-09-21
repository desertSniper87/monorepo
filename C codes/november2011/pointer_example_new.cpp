#include <stdio.h> 
 
int j, k; 
int *ptr; 
 
int main(void) 
{ 
    j = 2; 
    k = 3; 
    ptr = &k; 
    printf("\n"); 
    printf("j has the value %d and is stored at %p\n", j, (void *)&j); 
    printf("k has the value %d and is stored at %p\n", k, (void *)&k); 
    printf("ptr has the value %p and is stored at %p\n", ptr, (void 
    *)&ptr); 
    printf("The value of the integer pointed to by ptr is %d\n", *ptr); 
 
    getchar ();
    return 0; 
}   
