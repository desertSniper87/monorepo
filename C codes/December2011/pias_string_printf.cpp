#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
int func(char *s1,char *s2,int *a);

int main()
{
    char s1[100],s2[100];
    int a[100];
    gets(s1);
    gets(s2);
    int m=0;


    func(&s1[0],&s2[0],&a[0]);
   // printf("%d  %d",a[0],a[1]);
    while(a[m]!='\0')
    {

        printf("%d  ",a[m]);
        m++;

    }








}
int func(char *s1,char *s2,int *a)
{

    int i=0,j=0,sum,f=0,m=0;
    char P[100];
    while(s2[i]!='\0')
    {
        if(s2[i]=='%' && s2[i+1]=='d')
        {

            while(s1[j]!='\0')
            {





                     int p;
                p=s1[j];
                if(p>=49 && p<=57)
                {
                  int g=j;
                  int  G=0;
                   int u=0;
                   while(s1[g]!='\0')
                   {
                       int q=s1[g];
                       if(q>=49 && q<=57)
                       {

                           P[u]=s1[g];
                           //printf("%c\n",P[u]);
                           u++;
                       }
                       else
                       {
                          f=1;
                          break;
                       }


                       G++;
                       g++;
                   }
                   P[u]='\0';
             //printf("%s",P);
             int I=strlen(P);
             I--;
             int J=0;
             int sum=0,su;
             while(I>=0)
             {
               int r=P[J];
               if(r>=49 && r<=57 )
               {
                   r=r-48;
               }

                su=r*pow(10,I);
                //printf("%d\n",su);
                sum=sum+su;

                J++;
                I--;
             }


             // printf("%d\n",sum);

             if(sum>=0)
             {
                 a[m]=sum;
                 m++;
             }
                   j=j+G;
                   if(f==1)
                   {
                       break;
                   }





                }



                j++;
            }






        }





        i++;
    }
      a[m]='\0';





}

