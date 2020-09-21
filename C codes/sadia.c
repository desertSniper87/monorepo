#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
int rotation(int r);
int prime(int x)
{
    int h;
    if (x>1)
    {
            for (h=2; h<=sqrt(x); h++)
            {
                if ((x%h)==0) return 0;
                }
            return 1;
            }
    return 0;
}

int rotation(char num1[30],int n)
{
    int l,i,j,k,m,rt,cr=0;
    char num2[30]={0};
    for(j=1; j<=n; j++)
    {
             l=strlen(num1);
             num2[0]=num1[l-1];
             for(i=1; i<l; i++) num2[i]=num1[i-1];
             num2[i]='\0';
             strcpy(num1,num2);
             }
    rt=atoi(num2);
    return rt;
}
int main()
{
    char a[30];
    int l1,s,r,at,cn=1,c=0;
    printf("Enter number:");
    gets(a);
    at=atoi(a);
    if(prime(at))
    {
                l1=strlen(a);
                for (s=1; s<l1; s++)
                {
                    r=rotation(a,s);
                    if (!prime(r))
                    {
                                  printf("%d is not circular prime\n",at);
                                  break;
                                  }
                    }
                if (s==l1) printf("%d is a circular prime\n",at);
                }
    else printf("%d is not prime\n",at);
    return 0;
}
