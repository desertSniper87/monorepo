#include <stdio.h>
#include <string.h>

int main (void)

{
    int i;
    char str[100];
    char *p = "stop";
    //char bigstr[100];
    
    
    
    while ( strcmp(p,str) )
    {
        gets (str);
        //strcat (str,"\n");
        //strcat( bigstr,str );
    }
    
    //printf (bigstr);
    
    scanf ("%d",&i);
    return 0;
}
