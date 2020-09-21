#include <stdio.h>
#include <string.h>


int main (void)

{
    int i;
    char s1[100];
    char *p;
    
    gets (s1);
    p = s1;
    
    for ( i=0;p[i]!=' ';i++ )
    {}
    i++;
    //printf ("%d",i);
    
    for ( ;p[i]!='\0';i++ )
    {
        printf ("%c",p[i]);
    }
     
    
    
    getchar ();
}
