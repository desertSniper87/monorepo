#include <iostream>
#include <cstring>
using namespace std;

int main ()
{
    char x[100];
    char output[100];
    int i = 0;
    int len;

    while ( cin>>x )
    {
        while ( i<strlen( x ) )
        {
            output[i] = x[i]-7;
            i++;
        }

        for ( i=0;i<strlen(x);i++ )
        {
            cout<< output[i];
        }

        cout<< endl;
        i = 0;

    }


    return 0;
}
