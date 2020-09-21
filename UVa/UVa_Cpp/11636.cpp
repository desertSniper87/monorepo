#include <iostream>
using namespace std;

int main ()
{
    int n;
    int i;
    int ans = 1;
    int case_ = 1;

    while ( cin>>n )
    {
        if ( n<1 )
            break;

        else if ( n==1 )
            cout<< "Case"<< ' '<< case_<< ": "<< "0"<< endl;

        else
        {

            for ( i=2;i<n;i = i*2 )
            {
                ans++;
            }

            cout<< "Case"<< ' '<< case_<< ": "<< ans<< endl;

        }

        ans = 1;
        case_++;
    }

    return 0;
}
