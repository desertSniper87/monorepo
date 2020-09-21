#include <stdio.h>
#include <conio.h>

int main (void)

{
    int start;
    int mid;
    int end;
    int n;
    int i;
    int flag = 0;
    int array[100];
    int search;

    printf("Please enter number of numbers: \n");
    scanf ("%d",&n);

    printf ("Please enter %d numbers\n",n);

    for ( i=0;i<n;i++ )
    {
        scanf ("%d",&array[i]);
    }

    printf ("Please enter the number to search for: ");
    scanf ("%d",&search);

    start = 0;
    end = n-1;
    mid = (start+end)/2;

    while (start<=end)
    {
          if ( search<array[mid] )
          {
               end = mid - 1;
               mid = (start+end)/2;
          }
          else if ( search>array[mid] )
          {
               start = mid + 1;
               mid = (start+end)/2;
          }
          else
          {
               printf ("%d is found at %d\n",search,mid+1);
               flag = 1;
               break;
          }
    }
    if (flag == 0)
    {
        printf ("Not found\n");
    }

    getch();
    return 0;
}
