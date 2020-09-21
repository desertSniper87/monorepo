/*Creating an encrypted code*/

#include <stdio.h>
#include <string.h>

int main (void)

{
    int i;
    char ch[100];
    
    printf ("Please enter a string to encrypt:\n");
    gets(ch);
    
    while ( i<strlen(ch) )
    {
          ch[i] = ch [i]+1;
          i++;
    }
    
    printf ("The encrypted string is: \n %s",ch);
    
    getch ();
    return 0;
}
