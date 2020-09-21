#include <stdio.h>

int main (void)

{
    int n,result;

    printf ("Please enter n: ");
    scanf ("%d",&n);

    result = fact (n);
    printf ("The result is %d",result);

    getch ();
    return 0;

}

int fact (int nu)

{
    int res = 1;
    int i;
    for ( i=1; i<=nu; i++ )
        res = res * i;

    return res;
}
