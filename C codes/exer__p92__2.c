#include <stdio.h>

int main (void)

{
    int pay;
    float first,second,third;
    char ch;
    
     printf ("Amount $\tThird Class\tSecond Class\tFirst Class\n");
     
     for ( pay=1; pay<=100; pay++ )
     {
         third = (10.0/100)*pay;
         second = (15.0/100)*pay;
         first = (20.0/100)*pay;
         printf ("%d\t\t%f\t%f\t%f",pay,third,second,first);
         printf("More ? (Y/N)\n");
         ch = getch ();
         if ( ch== 'N' ) break;
     }
}
