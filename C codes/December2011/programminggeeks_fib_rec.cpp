#include "stdio.h"
#include "conio.h"

int y;

int fib(int n)
	{ if(n==1 || n==0)
	   return n;
	  y=fib(n-1)+fib(n-2);
	   return y;
	}
int main()
	{ while (1)
      {
      int a,r;
	  printf("Enter any number : ");
	  scanf("%d",&a);
	  r=fib(a);
	  printf("The no. at position %d is %d\n",a,r);
      }
	  return 0;
	}
