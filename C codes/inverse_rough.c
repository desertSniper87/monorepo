#include <stdio.h>
#include <math.h>

int main (void)

{
    float x, i, sum;
    
    printf ("Please enter 10 number: ");
    
    for ( i=1; i<=10; i++ )
    {
        scanf ("%f",&x);
        sum = sum + pow ( x,-1 );
    }
    
    printf ("%f",sum);
    
    getch ();
    return 0;   
     
}
        
