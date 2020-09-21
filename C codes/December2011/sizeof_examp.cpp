#include <stdio.h>
#include <conio.h >

int main (void)

{
    int i;
    scanf ("%d",&i);
    
    printf ("i is %d bytes long",sizeof( i ));
    
    getch ();
    return 0;
}
