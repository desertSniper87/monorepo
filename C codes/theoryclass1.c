#include <stdio.h>

int main (void)

{
    int x,y,z1,z2;
    printf("x=");
    scanf("%d",&x);
    printf("y=");
    scanf("%d",&y);
    int k;
    printf("k=");
    scanf("%d",&k);
    z1 = x*k/y; z2 = x/y*k;
    printf("%d %d",z1,z2);
    getch();
    return 0;
    
}
