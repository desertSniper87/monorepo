#include <stdio.h>

int main(void)

{
    char name;
    printf("Please enter your name: ");
    scanf("%c",&name);
    printf("Hello %c",name);
    getch();
    return 0;
}
