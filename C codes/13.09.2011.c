#include <stdio.h>

double rec_area(double a,double b);

int main(void)
{
    double area,length,width;
    
    printf("Length: ");
    scanf("%lf",&length);
    
    printf("Width: ");
    scanf("%lf",&width);
    
    area = rec_area(length,width);
    
    printf("Area: %lf\n",area);
    
    printf("(c)2011 TBSinc");
    getch();
    return 0;
    
}

double rec_area(double x,double y)

{
       double a;
       a=x*y;
       return a;
       
}

       
       
