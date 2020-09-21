/*Tasmia Akila Orni*/
#include<stdio.h>
int main()
{
    int a[10];
    int i,num,j,k,l;
    for(i=0;i<10;i++)
    {
                     scanf("%d",&num);
                     a[i]=num;
    }
    int b[10];
    for(j=0;j<10;j++)
    {
                     for(k=j+1;k<10;k++)
                     {
                                        if(a[j]>a[k])
                                        {
                                                     b[j]=a[k];
                                                     a[k]=a[j];
                                                     a[j]=b[j];
                                        }
    
                     }
    
    
    }
    for(l=0;l<10;l++)printf("%d",b[l]);
    
    getch ();
    return 0;
}
