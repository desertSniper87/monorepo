#include <iostream>
#include <cstdlib>
using namespace std;

void sleep (int n);
void sleep (char *n);

#define delay 10000000

int main (void)
{
    cout<< '.';
    sleep (3);
    cout<< '.';
    sleep ("2");
    cout<< '.';
    
    
    getchar ();
    return 0;
}

void sleep (int n)
{
     int i;
     
     for ( ;n;n-- )
     {
         for ( i=0;i<delay;i++ )
         {
         }
     }
}

void sleep (char *n)
{
     int j = atoi (n);
     int i;
     
     for ( ;j;j-- )
     {
         for ( i=0;i<delay;i++ )
         {
         }
     }
}



