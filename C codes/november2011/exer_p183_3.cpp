#include <stdio.h>
#include <string.h>

int main (void)

{
    char s1[100];
    char *p;
    int flag;
    int i=0;
        
    printf ("Enter a string: \n");
    gets (s1);
    p = s1;
    
    while ( p )
    {
          if (p+i == ' '  )
          {
                 while (p)
                 {
                       printf ("%c",*p+i);
                       i++;
                 }
                 break;
                 flag = 1;
          }  
          i++;
          if (flag = 1) break;
    }
    
    scanf ("%d",&i);
    return 0;
}
