#include <stdio.h>
int amicable(int num1, int num2);

int main (void)

{
    int n1, n2;
    int result;
    
    printf ("Please enter 2 numbers \n");
    scanf ("%d%d",&n1,&n2);
    
    result = amicable (n1,n2);
    
    if (result == 1) printf ("Numbers are amicable\n");
    else printf ("Numbers are not amicable\n");
     
    scanf ("%d",&n1);
    return 0;
}

int amicable(int num1, int num2)
{
    int sum1;
    int sum2;
    int i;
    
    for ( i=1;i<num1;i++ )
    {
        if (num1%i == 0)
        {
                   sum1 = sum1 + i;
        }
    }
    printf ("%d\n",sum1);
    
    for ( i=1;i<num2;i++ )
    {
        if (num2%i == 0)
        {
                   sum2 = sum2 + i;
        }
    }
    
    printf ("%d\n",sum2);
    if ( num2 == sum1 && num1 == sum2 )
       return 1;
    else 
         return 0; 
}

