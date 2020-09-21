#include <stdio.h>

int main (void)

{
    int i,j;
    
    printf ("Enter first number: ");
    scanf ("%d",&i);
    printf ("Enter second number: ");
    scanf ("%d",&j);
    
    /*Relational operations*/
    printf ("i < j %d\n",i<j);
    printf ("i <= j %d\n", i<=j);
    printf ("i == j %d\n", i==j);
    printf ("i > j %d\n",i>j);
    printf ("i >= j %d\n", i>=j);
    
    /*Logical Operations*/
    
    printf ("i&&j %d\n",i&&j);
    printf ("i||j %d\n",i||j);
    printf ("!i !j %d\n",!i, !j);
    
    getch ();
    return 0;
    
}
    
