#include<stdio.h>
int amicable(int n1,int n2);
int main(void)
{
    int n1,n2,s;
    printf("enter n1:");
    scanf("%d",&n1);
    printf("enter n2:");
    scanf("%d",&n2);
    s=amicable(n1,n2);
    if(s==1)
    printf("yes numbers are amicable");
    else
    printf("no the numbers are not amicable");
    
    scanf ("%d",&s);
    return 0;
}
int amicable(int n1,int n2)
{
    int i,sum1,sum2,j;
    sum1=0;
    sum2=0;
    for(i=1;i<n1;i++)
    {
        if(n1%i==0)
        sum1=sum1+i;
        else
        sum1=sum1;
    }
    
    printf ("%d",sum1);
    for(j=1;j<n2;j++)
    {
        if(n2%j==0)
        sum2=sum2+j;
        else
        sum2=sum2;
    }
    
    printf ("%d",sum2);
    
    if(sum1==n2&&sum2==n1)
    return 1;
    else
    return 0;
}
