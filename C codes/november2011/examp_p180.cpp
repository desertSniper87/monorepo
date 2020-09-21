#include <stdio.h>
#include <ctype.h>

int main (void)

{
    int i;
    char s1[100];
    
    printf ("Please enter a string: \n");
    gets(s1);
    
    for ( i=0;s1[i];i++ )
    {
        s1[i] = toupper(s1[i]);
    }
    
    printf ("The uppercase string: \n%s\n",s1);
    
    for ( i=0;s1[i];i++ )
    {
        s1[i] = tolower(s1[i]);
    }
    
    printf ("The lowercase string: \n%s",s1);
    
    scanf ("%d",&i);
    return 0; 
}
