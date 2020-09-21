#include <stdio.h>
#include <conio.h>

int array[100];
int n;
int flag;
int search;

int binary_search (int search);

int main (void)
{
    int t;
    //int array[100];
    int c = 0;
    int d = 0;
    //int n;
    int x = 0;
    //int search;

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
        while ( d>0 && array[d]>array[d-1] )
        {
              t = array[d];
              array[d] = array[d-1];
              array[d-1] = t;

              d--;
        }
    }

    /*
    printf ("Now printing the numbers in DESCENDING order: \n");
    for ( c=0;c<=n-1;c++ )
    {
        printf ("%d\n",array[c]);
    }
    */

    printf ("Please enter the number to search for: ");
    scanf ("%d",&search);

    x = binary_search (search);

    if (flag == 1)
    {
    printf ("%d is found at %d\n",search,x);
    }

    getch();
    return 0;
}

int binary_search (int search)
{
    int start;
    int mid;
    int end;
    //int n;
    int i;
    flag = 0;
    //int array[100];
    //int search;
    int x;
    /*
    printf("Please enter number of numbers: \n");
    scanf ("%d",&n);

    printf ("Please enter %d numbers\n",n);

    for ( i=0;i<n;i++ )
    {
        scanf ("%d",&array[i]);
    }
    */
    start = 0;
    end = n-1;
    mid = (start+end)/2;

    while (start<=end)
    {
          if ( search>array[mid] )
          {
               end = mid - 1;
               mid = (start+end)/2;
          }
          else if ( search<array[mid] )
          {
               start = mid + 1;
               mid = (start+end)/2;
          }
          else
          {
               x = mid + 1;
               flag = 1;
               break;
          }
    }
    if (flag == 0)
    {
        printf ("Not found\n");
    }

    //getch();
    return x;
}
