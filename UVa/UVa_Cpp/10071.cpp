#include <iostream>
using namespace std;

int main ()
{
    int v;
    int t;


    while (cin>> v&&cin>> t)
    {
        if ( -100<=v&&v<=100&&t>=0&&t<=200 )
        {
            cout<< 2*v*t<< endl;
        }

    }

    return 0;
}
