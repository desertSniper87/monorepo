#include <stdio.h>
#include <string.h>

int main (void)

{
    int i;
    
    char s1[100], s2[100];
    char *p1, *p2;
    printf ("please enter a string");
    gets (s1);
    
    p1 = s1 + strlen (s1) -1;
    p2 = s2;
    
    while ( p1>=p2 )
    {
         *p2++ = *p1--;
    }
    
    printf (s2);
    
    scanf ("%d",&i);
    return 0;
}
