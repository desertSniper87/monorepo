#include <stdio.h>
#include <string.h>

char *name[6][2] = 
    {
         "A" , "apple",
         "B" , "book",
         "C" , "cat",
         "D" , "dog",
         "E" , "elephant",
         "" , ""
    };
    
int main (void)
{   
    int i = 0;
    char ch[] = "";
    
    printf ("Enter letter: ");
    gets(ch);
    
    for ( i=0;*name[i][0];i++ )
    {
    if ( !strcmp( ch,name[i][0] ) ) 
    {
         printf (name[i][1]);
    }
    }
            
    scanf ("%d",&i);
    return 0;
}
