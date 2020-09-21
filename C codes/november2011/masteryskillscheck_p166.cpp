#include <stdio.h>

int main (void)

{
    int n,input,i;
    
    printf ("How many inputs: \n");
    scanf ("%d",&n);
    printf ("--------------input-------------\n");
    
    int array[10];
    
    for ( i=0;i<n;i++ )
    {
        scanf ("%d",&input);
        array [i] = input;
    }
    
    int sum_even , sum_odd;
    
    for (i=0;i<n;i++)
    {
        if (array [i] % 2 == 0)
           sum_even = sum_even + array [i];
        else 
             sum_odd = sum_odd + array [i];
    }
    
    printf ("The sum of even numbers: \t%d",sum_even);
    printf ("The sum of odd numbers: \t%d",sum_odd);
    
    scanf ("%d",&i);
    return 0;
}
