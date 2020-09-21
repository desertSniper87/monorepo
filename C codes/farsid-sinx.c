/*Mohammad Ullah Farsid */
/*This program computes sinx*/
#include<stdio.h>
#include<math.h>

int main()
{
    double sum=0;
    double deno=1,x,num;
    int i;
    
    printf("Print the value of (in radian)sine");
    scanf("%lf",&x);
    for(i=1;i<=20;i++)
    {
            num=pow(-1.00,i+1)*pow(x,2*i-1);
            sum=sum+(num/deno);
            /*printf("%lf ",num)*/;
            /*printf("%lf ",deno)*/;
            deno=deno*2*i*(2*i+1);
    }
    
    printf("=%.2lf",sum);
    
    getch ();
    return 0;
}
