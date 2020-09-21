#include <stdio.h>

int main (void)

{
    int x,i;
    
    printf("Please input number: ");
    scanf("%d",&x);
    
    for ( i=x; i>=0; i-- )
        printf ("%d\n",i);
        
    printf("Lunch the rocket");
    
    getch();
    return 0;
    
}
