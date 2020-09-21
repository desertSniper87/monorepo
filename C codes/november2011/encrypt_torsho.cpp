#include <stdio.h>
#include <string.h>

int main (void)

{
    char str1[100];
    int i;

    printf ("Enter your string: \n");
    gets (str1);

    i=0;
    while (i<strlen(str1))
    {
        if (str1[i]>=65 and str1[i]<=85)
        {
            printf ("%c",str1[i]+5);
            i++;
        }

        if (str1[i]>=86 and str1[i]<=90)
        {
            printf ("%c",str1[i]-20);
            i++;
        }

        else if (str1[i]>=97 and str1[i]<=117)
        {
            printf ("%c",str1[i]+5);
            i++;
        }

        else if (str1[i]>=118 and str1[i]<=122)
        {
            printf ("%c",str1[i]-20);
            i++;
        }

        else if (str1[i] == ' ')
        {
            printf (" ");
            i++;
        }

        else i++;
    }


    return 0;




}
