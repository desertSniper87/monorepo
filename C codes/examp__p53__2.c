/*Prime number tester*/
#include <stdio.h>

int main (void)

{
    int x,i,is_prime;
    is_prime =1;
    
    scanf("%d",&x);
    
    for (i=2; i<=x/2; i=i+1)
        if (x%i==0) is_prime = 0;
    
    if (is_prime==1)
       printf("The number is prime");
    else
        printf("The number is not prime");
        
    getch();
        
    return 0;
    
}    
    
