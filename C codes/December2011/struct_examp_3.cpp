#include <stdio.h>
#include <conio.h>
struct student
{
       int id;
       char *name;
       float cgpa;
}
s1,
s2,
s3;

int main (void)

{
    struct student;
    
    s1.id = 1005087;
    s1.name = "Samidhya Sarker";
    s1.cgpa = 3.07;
    
    printf ("%d\t",s1.id);
    printf ("%s\t",s1.name);
    printf ("%f\t",s1.cgpa);
    
    getch();
    return 0;
}
