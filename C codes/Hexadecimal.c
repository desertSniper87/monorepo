#include <stdio.h>

int main (void)

{
   int num, i, divisor, quotient, dividend;

   printf ("Please enter number in decimal \n");
   scanf ("%d",&num);
   divisor = 16;

   for ( ;dividend!= 0; )
   {
       quotient = num % 16;
       if ( quotient == 10 ) printf ("A");
       else if ( quotient == 11 ) printf ("B");
       else if ( quotient == 12 ) printf ("C");
       else if ( quotient == 13 ) printf ("D");
       else if ( quotient == 14 ) printf ("E");
       else if ( quotient == 15 ) printf ("F");
       else printf ("%d",quotient);
            break;

       num = quotient;

   }

   getch ();
   return 0;
}
