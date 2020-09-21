#include <stdio.h>

int option = 0;
int i;
int numbers[3];

while (option!=3){
    printf("Please choose an option and press enter:\n");
    printf("1. Read 3 numbers\n 2. Print the max\n 3.Exit\n");
    scanf("%i",&option);
    if (option==1) {
        for (i=0; i<3; i++) {
            printf("\nnumbers[%i]=",i+1);
            scanf("%i",&numbers[i]);
        }
    } else if (option==2) {
        int max = 0;
        for (i=0; i<3; i++) {
            if(numbers[i] > max) {
                max = numbers[i];
            }
        }
        printf("\nMax=%i",max);
    }
}

