#include <iostream>
#include <cstdlib>
using namespace std;

int greatestCommonDivisor ( int n, int m )
{
    if ( n<m )
    {
        swap ( n , m );

    }
    int r = 0;
    while (  n%m  )
    {
        r = n%m;
        n = m;
        m = r;
    }

    return m;
}

int main ()
{
    int a = 0;
    int b = 0;

    cin>> a>> b;
    cout<< greatestCommonDivisor( a,b );

    return 0;
}
