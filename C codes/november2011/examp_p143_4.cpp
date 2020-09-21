#include <stdio.h>

int main (void)

{
    int count;
    int i;
    int j;
    int t;
    int arr [100];
    
    printf ("How many numbers: ");
    scanf ("%d",&count);
    
    i=0;
    while (i<count)
    {
          scanf ("%d",arr[i]);
    }
    
    for ( i=0;i<count;i++ )
    {
        for ( j=count-1;j>=i;--j )
        {
            if (arr[j-1] > arr[j])
            {
                        t = arr[j];
                        arr[j] = arr[j-1];
                        arr[j-1] = arr[t];
                        
            }
        }
    }
    
    for ( t=0;t<count;t++)
    printf ("%d",arr[t]);
    
    scanf ("%d",&t);
    return 0;
    
}
