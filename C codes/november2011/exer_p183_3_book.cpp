#include <stdio.h>

int main (void)

{
    int i;
    
    char str [100];
    char *p;
    
    gets (str);
    
    p = str;
    
    while (*p && *p! = ' ')
    (*p)++;
    
    printf (p);
    
    scanf ("%d",&i);
    
    return 0;
}
