#include <stdio.h>

int main (void)

{
    int a,b;
    char ch;
    
    printf ("Do you want to:");
    printf ("Add, Substract, Multiply or Divide: ");
    
    printf ("Enter the first capital letter: ");
    ch = getche ();
    if ( ch!='A' && ch!='S' && ch!='M' && ch!='D')
    {
         printf ("Thank You :-)");
    }
    
    printf ("Enter first number: ");
    scanf ("%d",&a);
    printf ("Enter second number: ");
    scanf ("%d",&b);
    
    switch (ch)
    {
           case 'A': printf ("%d",a+b);
           break;
           case 'S': printf ("%d",a-b);
           break;
           case 'M': printf ("%d",a*b);
           break;
           case 'D': 
           if (b!=0) printf ("%d",a+b);
           
    }
    getch ();
    return 0;
}
