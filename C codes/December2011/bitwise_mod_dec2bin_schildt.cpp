#include <stdio.h>
#include <conio.h>

int main (void)

{
    int n;
    int i;
    
    printf ("Enter a numeber: \n");
    scanf ("%d",&n);
    
    for ( i=1024;i>0;i=i/2 )
    {
        if ( n & i ) printf ("1");
        else printf ("0");
    }
    
	getch ();
	return 0;
}
