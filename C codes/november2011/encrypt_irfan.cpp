#include<stdio.h>
main()
{
	int i,j=0;
	char str1[100],str2[100];
	gets(str1);

	for(i=0;str1[i]!=0;i++)
	{
		if(str1[i]==' ')
		{
			str2[j]=' ';
			j++;
		//	printf("%c",str2[j]);
		}
		else
		{
			str2[j]=str1[i]+5;//printf("%c",str2[j]);
			j++;//printf("%d",j);
		}
	}
	str2[j]='\0';
	printf(str2);
	printf("\n");
	
	getchar ();
	return 0;
}
