#include <stdio.h>
#include <conio.h>
#include <stdlib.h>

int main (void)

{
    char str[ 100 ] = "Dr.KP Sarker";
    char *p;
    FILE *fp;
    int i;
    
    fp = fopen ( "myfile.txt" , " w " );
    
    p = str;
    
    while (*p)
    {
        if ( fputc ( *p++, fp ) == EOF )
            {
                 printf ("Error writing file\n");
                 exit (1);
            }
    } 
    
    fclose ( fp );
    
	getch ();
	return 0;
}
