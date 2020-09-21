#include<stdio.h>  
/* By the way the code is for binary to decimal */
#include<conio.h>  
void main()  
{  
unsigned long int n,p,x[20],a[20],count=0,sum=0,i,j;  
printf("Enter binary number:   ");  
scanf("%lu",&n);  
for(i=0;n>0;i++)  
{  
x[i]=n%10;  
n=n/10;  
count++;  
}  
  
      for(i=1;i<count;i++)  
      {  
      p=1;  
      for(j=1;j<=i;j++)  
  
      {  
      a[i]=p*2;  
      p=a[i];  
      }   }  
  
     for(i=1;i<count;i++)  
     sum=sum+a[i]*x[i];  
  
     sum=sum+x[0]*1;  
  
  
  
     printf("THE DECIMEL NUMBER IS:%lu",sum); 
     
     getch (); 
}  
