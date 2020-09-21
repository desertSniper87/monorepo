#include <stdio.h>
#include <string.h>


int main (void)

{
    int i;
    char s1[100];
    
    gets (s1);
    
    for ( i=0;s1[i]!=' ';i++ )
    {}
    i++;
    //printf ("%d",i);
    
    for ( ;s1[i]!='\0';i++ )
    {
        printf ("%c",s1[i]);
    }
     
    
    
    getchar ();
}
