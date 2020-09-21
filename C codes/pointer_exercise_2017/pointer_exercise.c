#include <stdio.h>

int main(void){
    int variable;
    int *ptr = &variable;
    *ptr = 20;
    /*int *ptr_a;*/
    /**ptr_a = 20;*/

    printf("%d", *ptr);
    return 0; 
}
