#include <stdio.h>

double power(double x, int n);

int main() {
  double x = 0.0;
  int n = 0;
  for(x = 2.0 ; x<= 5.0; x += 0.5)
    for(n = 0 ; n<5 ; n++)
      printf("%.2lf raised to the power %d = %.2lf\n", x, n, power(x,n));
}

/* Function to raise x to the power n.*/
double power(double x, int n) {
  if(n == 0)
    return 1.0;
  else
    return x * power( x , n - 1 );
}
