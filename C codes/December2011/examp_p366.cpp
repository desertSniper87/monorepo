#include <stdio.h>
#include <conio.h>

int main (void)

{
    int i;
    
    scanf ("%d",&i);
    
    i = i>0 ? 1 : -1;
    
    printf ("%d",i);
    
	getch ();
	return 0;
}
