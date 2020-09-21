#include <stdio.h>
#include <string.h>

int main (void)

{
    char str1[100], str2[100], str3[100];
    int i, j, N;
    
    printf ("Enter first string: \n");
    gets (str1);
    
    printf ("Enter second string: \n");
    gets (str2);
    
    printf ("Enter N: \n");
    scanf ("%d",&N);
    
    if (N<=0)
    {
             strcat ( str2,str1 );
             printf (str2);
    }
    
    else if (N>strlen(str2))
    {
         strcat (str1,str2);
         printf (str1);
    }
    
    else 
    {
         i=0;
         while (i<N)
         {
               str3 [i] = str1 [i];
               i++;
         }
         
         strcat (str3,str2);
         //i= strlen (str3) + 1;
         //j = strlen (str3) - 1;
         
         //while (j>0)
         {
              // str3[i] = str1 [strlen (str1) - j];
              // j++;
         } 
         
         //str3 [i] = '\0';
         
         printf (str3);
    }
    
    scanf ("%d",&i);
    return 0;
    
    
}
