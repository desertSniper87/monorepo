#include <stdio.h>

int main (void)

{
    int comp;
    float s,v,t;
    int i = 0;
    char choice;
    
    printf ("Please enter the number of computations: ");
    scanf ("%d",&comp);
    
    while ( i<comp && choice != 'N' )
    {
          printf ("Please enter a distance: ");
          scanf ("%f",&s);
          
          printf ("Please enter a time: ");
          scanf ("%f",&t);
          
          v = ( s/t );
          
          printf ("The velocity is %f",v);
          
          printf ("Continue ? (Y/N)");
          scanf ("%s",&choice);
    }
    getch ();
    return 0;      
}
