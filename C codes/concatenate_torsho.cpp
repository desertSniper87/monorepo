#include <stdio.h>
#include <string.h>

int main (void)

{
    char str1[100], str2[100], str3[100];
    int i,j;
    int n,k;
    printf ("Enter first string \n");
    gets (str1);
    printf ("Enter second string: \n");
    gets (str2);
    printf ("Please enter N: \n");
    scanf ("%d",&n);

    i = strlen (str1);
    j = strlen (str2);

    if (n<=0) n=0;
    if ( n>i ) n=i;

    for (k=0;k<n;k++)
    {
        str3[k] = str1[k];
    }

    for (;k<n+j;k++)
    {
        str3 [k] = str2 [k-n];
    }

    for ( ; n<i+j; n++,k++ )
    {
        str3 [k] = str1 [n];
    }

    str3 [k] = '\0';

    printf (str3);

    return 0;



}
