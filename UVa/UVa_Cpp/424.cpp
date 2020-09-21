#include <iostream>
using namespace std;

int main ()
{
    unsigned long long int veryLongIntegers;
    unsigned long long int sum = 0;

    while ( cin>>veryLongIntegers )
    {
        if ( veryLongIntegers==0 )
            break;
        else
            sum += veryLongIntegers;
    }

    cout<< sum;

    return 0;
}
