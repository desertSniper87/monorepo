#include <stdio.h>

int main(void)

{   
    int i,x;
    printf("Please enter a number x to print from 1 to x: \n");
    scanf("%d",&x);
    
    for ( i=1; i<=x; i++)
        printf ("%d ",i);
        
    getch();
        
    return 0;
    
}
