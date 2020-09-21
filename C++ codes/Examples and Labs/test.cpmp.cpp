#include <stdio.h>
#include <conio.h>

int main (void)
{
    int i;
    i = 5;
    
    printf ( "%d\n" , --i - ++i );
    printf ( "%d\n" , ++i + ++i );
    printf ("%d",i);
    //printf ( "%d %d %d" , i ,i++, ++i );
    
    
    getch ();
    return 0;
}
