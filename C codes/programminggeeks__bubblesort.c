#include "stdio.h"
#include "conio.h"

void main( )
{
	int arr[7] = { 25, 17, 31, 13, 2, 26, 30 } ;
	int i, j, temp ;
	for ( i = 0 ; i <= 5 ; i++ )
	{
		for ( j = 0 ; j <= 5 - i ; j++ )
		{
			if ( arr[j] > arr[j + 1] )
			{
				temp = arr[j] ;
				arr[j] = arr[j + 1] ;/* Dane chotota chole aslo */
				arr[j + 1] = temp ;
			}
		}
	}

	printf ( "\n\nArray after sorting:\n") ;

	for ( i = 0 ; i <= 6 ; i++ )
		printf ( "%d\t", arr[i] ) ;
		
	getch ();
}
