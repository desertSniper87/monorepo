/* An electronic card catalog */
#include <stdio.h>
#include <conio.h>
#include <string.h>

struct s_type 
{
           int i;
           char ch;
           double f;
           char str[100];
}s;

int main (void)

{   
    printf ("Enter an integer: ");
    scanf ("%d",&s.i);
    printf ("Enter a character: ");
    scanf (" %c",&s.ch);
    printf ("Enter a floating point number: ");
    scanf (" %lf",&s.f);
    printf ("Enter a string: ");
	scanf (" %s",&s.str);
	
	printf ("%d %c %f %s", s.i, s.ch, s.f, s.str);
	
	
    getch ();
	return 0;
}
