#include<stdio.h>
#include<string.h>

int main(void)
{
	int n,i,j,k;
	char s1[500],s2[500],s3[1000];
	printf("Enter your 1st string: ");
	gets(s1);
	printf("Enter your 2nd string: ");
	gets(s2);
	printf("Enter your number:");
	scanf("%d",&n);
	
	i = strlen (s1);
	j = strlen (s2);
	
	
	//for(i=1;s1[i]!='\0';i++);
	//for(j=1;s2[j]!='\0';j++);
	
	
	if(n<=0)n=0;
	else if(n>i)n=i;
	for(k=0;k<n;k++)
		{
			s3[k]=s1[k];
		}
	for(;k<n+j;k++)
		{
			s3[k]=s2[k-n];
		}
	for(;s1[n]!='\0';n++)
		{
			s3[k]=s1[n];
			k++;
		}
		
	s3 [k] = '\0';	
	printf (s3);
	
	
	//for(k=0;k<i+j;k++)
		{
			//printf("%c",s3[k]);
		}

	
	
	//getchar();
	scanf ("%d",&i);
	
	return 0;
}
