#include <iostream>
#include <cstring>
using namespace std;

int main ()
{
    /*
    int a = 'a' - 96;
    cout<< a;
    */
    char a[4];
    int i = 0;
    int sum = 0;
    int len;
    bool flag = 1;

    while ( cin>> a )
    {
        for ( i=0;i<strlen(a);i++ )
            sum += a[i]-96 + i*26;

        cout<< sum<< endl;

        sum = 0;

    }




    return 0;
}
