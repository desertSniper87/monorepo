/* Convert a decimal integer do a binary string */
/* added a test printf() you can remove later */
/* Turbo C modified for Pelles C     vegaseat    19nov2004 */

#include <stdio.h>

void dec2bin(long decimal, char *binary);

int main()
{
  long decimal;
  char binary[80];

  printf("\n\n Enter an integer value : ");
  scanf("%ld",&decimal);
  dec2bin(decimal,binary);
  printf("\n The binary value of %ld is %s \n",decimal,binary);

  getchar();  /* trap enter */
  getchar();  /* wait */
  return 0;
}

/*
 accepts a decimal integer and returns a binary coded string
*/
void dec2bin(long decimal, char *binary)
{
  int  k = 0, n = 0;
  int  neg_flag = 0;
  int  remain;
  int  old_decimal;  /* for test */
  char temp[80];

  /* take care of negative input */
  if (decimal < 0)
  {
    decimal = -decimal;
    neg_flag = 1;
  }
  do
  {
    old_decimal = decimal;   /* for test */
    remain    = decimal % 2;
    /* whittle down the decimal number */
    decimal   = decimal / 2;
    /* this is a test to show the action */
    printf("%d/2 = %d  remainder = %d\n", old_decimal, decimal, remain);
    /* converts digit 0 or 1 to character '0' or '1' */
    temp[k++] = remain + '0';
  } while (decimal > 0);

  if (neg_flag)
    temp[k++] = '-';       /* add - sign */
  else
    temp[k++] = ' ';       /* space */

  /* reverse the spelling */
  while (k >= 0)
    binary[n++] = temp[--k];

  binary[n-1] = 0;         /* end with NULL */

  getch ();
  return 0;
}
