#include <stdio.h>
#include <conio.h>
void print (char *s1);

int main (void)

{
    int i;
    char s1[] = "Hello";

    print(&s1[0]);

    getch ();
    return 0;
}

void print (char *s1[])
{
     int i = 0;
     print(char *( s1+i ));
     printf ("%c",*(s1+i)];
}
