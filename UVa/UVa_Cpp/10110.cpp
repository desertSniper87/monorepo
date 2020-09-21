#include <iostream>
using namespace std;

int main ()
{
    int n;
    int i;
    int j;

    int _case;

    cin>> _case;

    while ( _case>0 )
    {
        cin>>n;
        bool _switch[n+1];

        for ( i=0;i<n;i++ )
        {
            _switch[i] = false;
        }

        for ( i=0;i<n;i++ )
        {
            if ( !( _switch[i+1]%(i+1)) )
            {
                _switch[i+1] = !_switch[i+1];
            }
        }

        //*

        //*/

        if ( _switch[n-1] )
            cout<< "No"<< endl;
        else
            cout<< "Yes"<< endl;

        delete _switch;

        _case--;
    }

    return 0;
}
