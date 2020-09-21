#include <stdio.h>

int main (void)
{
    int locker[100];
    int i;
    int j;
    for ( i=0;i<100;i=i+1 )
    {
        if (locker[i]%2)
        locker[i] = 1;
                  for ( j=0;j<100;j=j+2)
                  {
                      if ((locker[j]%2)!=0)
                      locker[j]=2;
                  }
    }
    
    
    for ( i=0;i<100;i++ )
    {
        if ( locker[i]%2 )
        {
             //printf ("%d\t",locker[i]);
             printf ("%d is closed\n",i+1);
        }
        else 
        {
             //printf ("%d\t",locker[i]);
             printf ("%d is open\n",i+1);
        }
    }
    
    getchar ();
    return 0;
}
