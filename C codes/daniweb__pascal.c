#include<stdio.h>
#include<conio.h>

void main()
{
    int i, j,n,*p,check=1, blank, memspace, mem=0, rowdiff=1;
    
    printf("Enter the no. of rows to be printed :");
    scanf("%d",&n);
    
    blank=n-1;
    memspace=((n*(n+1))/2);
    p=(int*)malloc(sizeof(int)* memspace);
    
    for(i=0;i<n;i++)
    {
        printf("\n");
        for(j=0;j<blank;j++)
                            printf(" ");
        for(j=0;j<check;j++)
        {
            if(j==0 || j==(check-1))
            p[mem]=1;
            else
            p[mem]=p[mem-rowdiff] + p[mem-rowdiff+1];
            printf("%d ",p[mem]);
            mem++;
        }
    for(j=0;j<blank;j++)
    printf(" ");
    blank--;
    check++;
    rowdiff++;
    }
    getch();
}
