#include <stdio.h>

void dec2bin(long decimal, char binary[80]);

int main()
{
  long decimal;
  char binary[80];
  int i=0;

  printf("\n\n Enter an integer value : ");
  scanf("%ld",&decimal);
  dec2bin(decimal,binary);
  printf("\n The binary value of %ld is \n",decimal);
  for ( ;binary[i]!=0; i++)
  {
      printf ("%d",binary[i]);
  }


  getchar();  /* trap enter */
  getchar();  /* wait */
  return 0;
}

void dec2bin(long decimal, char binary[80])
{
  int  k = 0, n = 0;
  int  remain;
  char temp[80];

  do
  {
    remain    = decimal % 2;
    decimal   = decimal / 2;
    binary [n] = remain;
    n++;
    //temp[k++] = remain + '0';
  } while (decimal > 0);

  /* reverse the spelling */
for ( ; k!=0, n>=0 ; --n,k++ )
{
        binary [n] = temp [k];
}

  //binary[n-1] = 0;         /* end with NULL */


  getch ();
  return binary;
}