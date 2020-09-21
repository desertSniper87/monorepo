#include <stdio.h>

int main (void)

{
    char name[100];
    int t;
    
    printf ("Enter his name: ");
    gets (name);
    
    printf ("Enter his time in microseconds: ");
    scanf ("%d",&t);
    
    while ( t!= 0 )
    {
          printf ("%d\n",t);
          t--;
    }
    printf ("Kaboom!\n");
    printf ("%s is history",name);
    
    getch ();
    return 0;
}
