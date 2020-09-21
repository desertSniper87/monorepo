#include <iostream>
#include <cstring>
using namespace std;

int main ()
{
    int number[62];
    int i;
    int temp = 0;

    while ( cin>> i )
    {


        if ( 1<=i && i<10 )
        {
            temp = i+1;
        }

        else if ( 65<=i && i<=90 || 97<=i && i<=122 )
        {
            i =  i - 55;
            temp = i + 1;
        }

        else
        {
            cout<< "Such number is impossible!"<< endl;
        }

        cout<< temp<< endl;
    }

    return 0;
}
