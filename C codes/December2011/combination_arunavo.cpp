#include<stdio.h>
#include<conio.h>
int comb(int n,int r)
{
	int k;
	if(n<r)k=0;
	else if(n==r)k=1;
	else if(n>0 && r==0)k=1;
	else
	{
		int i,j;
		i=comb(n-1,r-1);
		j=comb(n-1,r);
		k=i+j;
	}
	return k;

}
int main(void)
{
	int n,r,k;
	printf("Enter your n");
	scanf("%d",&n);
	printf("Enter your r");
	scanf("%d",&r);
	k=comb(n,r);
	printf("The answer is %d",k);
	getch();

}


