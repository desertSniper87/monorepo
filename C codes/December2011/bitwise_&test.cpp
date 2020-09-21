#include <stdio.h>
#include <conio.h>

int main (void)

{
    int p;
    int q;
    
    for ( ;; )
    {
        printf ("Enter P:\n");
        scanf ("%d",&p);
        printf ("Enter Q: \n");
        scanf ("%d",&q);
        printf ("P&Q = %d",p&q);
        printf ("\n");
    }
    
	getch ();
	return 0;
}
