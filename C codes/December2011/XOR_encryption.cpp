#include <stdio.h>
#include <string.h>

int main (void)

{
    char text[] = "A nice cat";
    char key[] = "ABCDEFGHIJK";
    int x;
    
    for ( x=0;x<=11;x++ )
        printf ("%c",text[x]^key[x]);
        
    getchar();
    return 0;
}
