#include <stdio.h>
#include <conio.h>

int main (void)
{
    int t;
    int array[100];
    int c = 0;
    int d = 0;
    int n;
    
    printf ("Please enter number of numbers: \n");
    scanf ("%d",&n);
    
    printf ("Please enter %d numbers\n",n);
    
    for ( c=0;c<n;c++ ) // for ( c=1;c<=n-1;c++ ) gives wrong output
    {
        scanf ("%d",&array[c]);
        //printf ("\t");
    }
    
    for ( c=1;c<=n-1;c++ ) //( c=0;c<n-1;c++ ) gives wrong output
    {
        d = c;
        while ( d>0 && array[d]<array[d-1] )
        {
              t = array[d];
              array[d] = array[d-1];
              array[d-1] = t;
              
              d--;
        } 
    }
    
    printf ("Now printing the numbers in accending order: \n");
    for ( c=0;c<=n-1;c++ )
    {
        printf ("%d\n",array[c]);
    }
    
    getch();
    return 0;
    
}
