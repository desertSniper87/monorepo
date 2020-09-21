#include <stdio.h>

int main (void)

{
    int i,a;
    
    printf ("Enter i: \n");
    scanf ("%d",&i);
    
    switch (i)
    {
           case 1 : a=1;
           case 2 : a=2;
                break;
                
           case 3 : a=3;
                break;
                
           case 4 : ;
           case 5 : a=5;
                
    }
    
    printf ("%d",a);
    
    scanf ("%d",&i);
    return 0;
}
