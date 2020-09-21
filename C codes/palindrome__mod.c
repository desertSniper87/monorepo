#include <stdio.h>
int main()
{
    int i=0;
    int p[i];
    int f=0;
    int y=0;
    int n;
    scanf("%d",&n);
    int j=0;
    int k;
    while(n!=0)
    {
        k=n%2;
        p[i]=k;
        n=n/2;


        i++;
        f++;


    }

    f--;
    while(f>=0)
    {

        if(p[j]!=p[f])
        {

            y=1;


        }

        j++;
        f--;


   }

   if(y==0)
   {
        printf("palinodrome");

   }
   else

        printf("Not palinodrome");








}

