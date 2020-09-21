#include <stdio.h>
int recurse (int n);

int main (void)

{
    int i;
    int n;
    
    printf ("Enter n: ");
    scanf ("%d",&n);
    recurse (n);
    scanf ("%d",&i);
    return 0;
}

int recurse (int n)
{
    if ( n<0 ) return 123;
    
    printf ( "Before recursion: %d\n" , n );
    recurse (n-1);
    printf ( " After recursion: %d \n" , n );
}

