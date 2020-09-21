#include <iostream>
using namespace std;

bool prime_Detector ( int n )
{
    bool flag = 1;

    for ( int i=2;i<n/2;i++ )
    {
        if ( !(n%i) )
        {
            flag = 0;
            break;
        }
    }

    return flag;
}

int main ()
{
    long long int n = 0;

    //*
    while ( cin>>n )
    {
        for ( int i=1;i<=n;i++ )
        {
            if ( (prime_Detector( i )) )
            {
                cout<< i<< endl;
            }
        }
    }
    //*/
    //cout<< prime_Detector(10);

    return 0;


}

