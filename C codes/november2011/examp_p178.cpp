#include <stdio.h>

int main (void)

{
    char str[100] = "Welcome to CSE fest!";
    int i;
    char *p;
    
    p = str;
    
    for ( i=0;p[i];i++ )
    {
        printf ("%c\n",p[i]);
    }
    
    getchar ();
    return 0;
}
