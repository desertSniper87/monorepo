#include <stdio.h>

int main(void)

{
    int answer, count, sum;
    
    for ( count=1; count<=10; count++ )
    {
        printf("What is %d and %d\n",count,count);
        sum = count+count;
        scanf("%d",&answer);
        
        
    
    
     if (answer == count*2 )
        printf("Your answer is right!\n");
        
     else {printf("Your answer is wrong!\n");
           printf("The correct answer is %d\n", sum );};
     
     }
     printf ("(C) 2011 TABSinc");     
     getch();
     return 0;
     
}
