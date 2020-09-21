#include <iostream>
#include <cstdlib>
using namespace std;

int main ()
{
    int n;
    int i;
    int sub1;
    int sub2;
    bool flag = true;

    while ( cin>> n )
    {
        int numArray[n];

        for ( i=0;i<n;i++ )
        {
            cin>> numArray[i];
        }

        sub1 = abs(numArray[1]-numArray[0]);

        for ( i=1;i<n-1;i++ )
        {
            sub2 = abs(numArray[i+1]-numArray[i]);

            cout<< sub1<< '\t'<< sub2<< endl;

            if ( sub2==(sub1-1) )
            {
                sub1 = sub2;
            }

            else
            {
                flag = 0;
                break;
            }
        }

        if ( flag )
            cout<< "Jolly"<< endl;
        else
            cout<< "Not jolly"<< endl;
    }

    return 0;
}
