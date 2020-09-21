#include <stdio.h>

int main(int argc, char *argv[])
{
    int i = 0;

    for(i=0;i<1024;i++) {
        printf("i: %d\n", i);
        printf("i&255: %d\n", !(i&255));
    
    }


    return 0;
}
