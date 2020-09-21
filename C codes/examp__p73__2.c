#include <stdio.h>

int main (void)

{
    float a,b;
    char ch;
    int i;
    
    for (;i=10;)
    
    {
    printf ("Please enter: Add, Substract, Multiply, Divide\n");
    printf ("Please enter uppercase letters: ");
    
    scanf ("%c",&ch);
    
    printf ("Please enter a:");
    scanf ("%f",&a);
    printf ("Please enter b: ");
    scanf ("%f",&b);
    
    if ( ch=='A') printf ("The sum is %f\n",a+b);
    if ( ch=='S') printf ("The substraction is %f\n",a-b);
    if ( ch=='M') printf ("The Multiplication is %f\n",a*b);
    if ( ch=='D' && b!=0) printf ("The division is %f\n",a/b );
    }
    
    getch ();
    return 0;
    
}
    
