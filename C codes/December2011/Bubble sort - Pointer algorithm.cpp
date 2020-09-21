//Bubble sort - Pointer algorithm
//Decending order [Larger to smaller]

#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
void bubblesort ( int n, int *x);

int main (void)

{
    int *x, n,;
    int i=0;
    printf ("Enter the list of numbers: \n");
    scanf ("%d",&n);
    
    x = ( int * ) malloc ( n * sizeof( int ) );  //Usage of malloc
     
    for ( i=0;i<n;i++ )
    {
        scanf ("%d",x+i);
    }
    
    bubblesort ( n,x );
    
    printf ("{ ");
    for ( i=0;i<n;i++ )
    {
        printf ("%d ",*( x+i ));
    }
    printf (" }");
        
	getch ();
	return 0;
}

void bubblesort ( int n, int *x)
{
     int i = 0;
     int j = 0;
     int temp = 0;
     
     for ( i=0;i<n-1;i++ )
     {
         for ( j=i+1;j<n;j++ )
         {
             if ( *( x+i ) < *( x+j ) )
             {
                  temp = *( x+i );
                  *( x+i ) = *( x+j );
                  *( x+j ) = temp;
             }
         }
     }
     
}

