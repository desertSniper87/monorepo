#include<stdio.h>
#include<conio.h>

int main()
{
 int a[10][10];
 int i,j,c,n;
 
 printf("Enter how many lines do you want");
 scanf("%d",&n);
 
 a[1][1]=1;
 printf("%d",a[1][1]);
 a[2][1]=1;a[2][2]=2;a[2][3]=1;
 printf("%d %d %d",a[2][1],a[2][2],a[2][3]);
 
 for(i=3;i<=n;i++)
  {
   a[i][1]=1;
   printf("%d",a[i][1]);
   j=2;c=3;
   while(j<=i)
   {
    a[i][j]=a[i-1][c-1]+a[i-1][c-2];
    printf("%5d",a[i][j]);
    c=c+1;
    j=j+1;
   }
   
 a[i][j]=1;
 printf("%d",a[i][j]);
 }
 
 getch();
 return 0;
}
