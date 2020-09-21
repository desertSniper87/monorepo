#include<stdio.h>
int main()
{
	int i,j,count[100];
	count[0]=1;
	count[1]=2;
	for(i=2;i<100;i++)
	{
		count[i]=0;
	}
	for(i=3;i<=100;i++)
	{
		for(j=i;j<=100;j++)
		{
			if(j%i==0)
			{
				count[j-1]=count[j-1]+3;
			}
		}
	}
	for(i=0;i<100;i++)
	{
		if(count[i]%2!=0)
		{
			printf("%d ",i+1);
		}
	}
	getchar ();
	return 0;
}
