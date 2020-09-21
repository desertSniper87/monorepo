#include <stdio.h>
#include <ctype.h>

int main (void)

{
    int i;
    char s1[100];
    char *p;
    
    printf ("Enter a string: \n");
    gets (s1);
    
    p = s1;
    
    for ( ;*p; )
    {
        *p++ = toupper(*p);
    }
    
    printf("%s\n",&s1);
    
    p = s1;
    
    for ( ;*p; )
    {
        *p++ = tolower(*p);
    }
    
    printf ("%s",&s1);
    
    scanf ("%d",&i);
    return 0;
} 
