#include <iostream>
#include <ctime>
#include <cstdlib>
using namespace std;

int main()
{
//   int i, n;
//   time_t t;
//
//   n = 5;
//
//   /* Intializes random number generator */
//   srand((unsigned) time(&t));

    int i,n ;
    n = 5;
   /* Print 5 random numbers from 0 to 49 */
   for( i = 0 ; i < n ; i++ )
   {
        int x = rand()%32760;
        cout<< x<< " ";
        double r = (x/32760);
        cout<< r<< endl;
        //cout<< (rand()%32760)/32760<<endl;
   }

   return(0);
}
