#include <iostream>
#include <cstdlib>
#include <cstring>
using namespace std;

int main ()
{
    int x;
    int y;
    char array_x[11];
    char array_y[11];
    int i;
    int j;
    int carry;

    while ( cin>>x && cin>>y  )
    {
        carry = 0;

        itoa ( x, array_x, 1 );
        itoa ( y, array_y, 1 );

        if ( x>y )
        {
            i = array_x.size;
            j = array_y.size;

            while ( array_y[j] )
            {
                if ( array_x[i] + array_y[j] > 9 )
                {
                    carry++;
                }
                i--;
                j--;
            }
        }

        else
        {
            i = array_x.size;
            j = array_y.size;

            while ( array_x[i] )
            {
                if ( array_x[i] + array_y[j] > 9 )
                {
                    carry++;
                }
                i--;
                j--;
            }
        }

    }
    return 0;
}
