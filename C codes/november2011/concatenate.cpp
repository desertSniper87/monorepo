#include <stdio.h>
#include <string.h>

int main (void)
{
    char str1 [100], str2 [100];
    
    printf ("Enter first string: \n");
    gets (str1);
    
    printf ("Enter second string: \n");
    gets (str2);
    
    strcat (str1,str2);
    
    printf ("Concatenated stringword: ");
    printf ("%s",str1);
    
    getchar ();
    return 0;
}
