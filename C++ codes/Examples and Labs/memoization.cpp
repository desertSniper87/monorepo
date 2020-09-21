#include <iostream>
using namespace std;

int memTable [50];

int factorial ( int n )
{
    if ( n == 0 )
        return 1;
    else if ( memTable[n]==-1 )
    {
        memTable[n] = factorial (n-1) * n;
        return memTable[n];
    }
    else
        return memTable [n];
}

int main ()
{
    int n;

    for ( n=0;n<50;n++ )
        memTable[n] = -1;

    while ( cin>>n )
    {
        cout<< factorial(n)<< endl;
    }

    return 0;
}
