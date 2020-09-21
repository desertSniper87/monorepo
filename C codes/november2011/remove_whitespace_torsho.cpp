#include <stdio.h>
#include <string.h>

int main (void)

{
    char str1[100], str2[100];
    int i,k;

    printf ("Enter your string: \n");
    gets (str1);

    i=0;
    //while (i<strlen(str1))
    while (i!='\0')
    {
        if (str1[i]>=65 and str1[i]<=85)
        {
            str2[k] = str1[i];
            i++;
            k++;
        }

        if (str1[i]>=86 and str1[i]<=90)
        {
            str2[k] = str1[i];
            i++;
            k++;
        }

        else if (str1[i]>=97 and str1[i]<=117)
        {
            str2[k] = str1[i];
            i++;
            k++;
        }

        else if (str1[i]>=118 and str1[i]<=122)
        {
            str2[k] = str1[i];
            i++;
            k++;
        }

        else if (str1[i] == ' ')
        {
            str2[k] = ' ';
            i++;
            k++;
        }

        else i++;
    }

    str2 [k] = '/0';
    printf (str2);


    return 0;




}
