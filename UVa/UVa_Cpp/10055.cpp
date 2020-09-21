#include <iostream>
using namespace std;

int main ()
{
    long long int i = 0;
    long long int j = 0;

    while ( cin>> i && cin>> j )
    {
        if ( j>=i )
            cout<< ( j - i )<< endl;
        else
            cout<< ( i - j )<< endl;
    }

    return 0;
}
