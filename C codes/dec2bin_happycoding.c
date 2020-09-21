/*Convert decimal to binary*/

#include <stdio.h>

void dec_bin(int number);

int main(void) 
{
 int input = 0;

 printf("Digit (0-255): ");
 scanf("%d", &input);

 (input >= 0) && (input < 256) ? dec_bin(input) : exit(1);

 return 0;
}

void dec_bin(int number) 
{
 int x, y;
 x = y = 0;

 for(y = 7; y >= 0; y--) {
  x = number / (1 << y);
  number = number - x * (1 << y);
  printf("%d", x);
 }

 printf("\n");
 getch ();
}
