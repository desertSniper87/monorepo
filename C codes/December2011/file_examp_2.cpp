#include <stdio.h>
#include <conio.h>
#include <stdlib.h>

#define NULL 0

int main (void)

{
    int i;
    FILE *fp;
    
    if (( fp = fopen ( "newfile.txt" , "r" )) == NULL )
    {
         printf ("Error reading file");
         exit (0);
    }
    
    while ( i!=EOF )
    {
          putchar ( i = fgetc (fp) ); 
    }
    
    fclose ( fp );
    
    getch();
	return 0;
}
