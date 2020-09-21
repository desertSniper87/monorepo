/*Jisan Mahmud*/
/*Decimal to Hexadecimal Conversion */

#include<stdio.h>

int main()

{
    
    long a,s,b;
    
    printf("In decimal = ");
    scanf("%ld",&a);
    
    for(s=0;a;a/=16)
    {
        b=a%16;
        
        if(b<10)
        b+=48;
        
        else
        b+=55;
        s=100*s+b;
    }
    printf("In hexa-decimal = ");
    for(;s;s/=100)
        {
        printf("%c",s%100);
        }
        
    getch ();
    return 0;
}
