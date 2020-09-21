#include <stdio.h>
#include <conio.h>

int main (void)

{
    int high = 0;
    int low = 0;
    int mid = 0;
    int x=0;
    int FOUND = 0;
    
    int array [50] = { 10,23,43,56,77,87,98,111,133,145 };
    
    /*
    int i;
    for ( i=0;i<=10;i++ )
    printf ("%d\t",array[i]);
    */
    
    
    printf ("Enter the element to search for: \n");
    scanf ("%d",&x);
    
    int i;
    
    high = 10-1;
    low = 0;
    
    while ( low<=high )
    {
          printf ("Searching...\n");
          mid = (low+high)/2;
          if ( x<array[mid] )
          {
               high = mid - 1;
          }
          
          else if ( x>array[mid] )
          {
               low = mid + 1;
          }
          
          else if ( x == array[mid] )
          {
               printf ("Found at position %d\n",mid);
               FOUND = 1;
               break;
          }
          
    }
    
    if ( FOUND == 0 )
    {
         printf ("Not found!");
    }
    
     
	getch ();
	return 0;
	
}
