#include <stdio.h>
#include <conio.h>

int main (void)

{
    struct examp
    {
           int i;
           char ch;
           int *p;
           double d;
    } s;
    
    printf ("The structure is %d bytes long",sizeof (struct examp ));
    
	getch ();
	return 0;
}
