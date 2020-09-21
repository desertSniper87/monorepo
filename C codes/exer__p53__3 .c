#include <stdio.h>
 
int main(void)
 
{
     int i,x;
     
     printf("Enter x: ");
     scanf("%d",&x);
     
     for ( i=1; i<=x; i++ )
         if (x%i==0)
            printf("%d\t",i);
            
     printf("\n(c)STPBinc 2011");
     getch();
     return 0;
     
}
