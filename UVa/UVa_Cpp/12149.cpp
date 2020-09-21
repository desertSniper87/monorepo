#include <iostream>
using namespace std;

int main ()
{
    int sum = 0;
    int i;

    while ( cin>>i )
    {
        if ( i==0 )
            break;

        for ( ;i>=1;i-- )
        {
            sum += i*i;
        }

        cout<< sum<< endl;
        sum = 0;
        /*
        if ( i==0 )
            break;
        */
    }

    return 0;
}
