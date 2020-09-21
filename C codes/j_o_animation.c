#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>
int main (void)
{
    for(int k=0;k<5;k++)
    {
        for(int i=0;i<20;i++)
        {
                usleep(200000-k*50000);
            system("clear");
            printf("\n");printf("\n");
            printf("8");for(int j=0;j<i;j++){printf("=");}printf("o");for(int j=i+1;j<20;    j++){printf("=");}printf("D\n");
        }
        for(int i=0;i<20;i++)
        {
            usleep(200000-k*40000);
            system("clear");
            printf("\n");printf("\n");
            printf("8");for(int j=i+1;j<20;j++){printf("=");}printf("o");for(int j=0;j<i;j++){printf("=");}printf("D\n");
        }
    }
    usleep(200000);
    printf("\n");
    system("clear");
    printf("8");for(int j=0;j<20;j++){printf("=");}printf("D- _ \n");
    usleep(200000);
    printf(" ");for(int j=0;j<20;j++){printf(" ");}printf("    _  _  _ \n");
    usleep(200000);
    printf(" ");for(int j=0;j<20;j++){printf(" ");}printf("       _   _   _ \n");
    usleep(200000);
    system("clear");
}