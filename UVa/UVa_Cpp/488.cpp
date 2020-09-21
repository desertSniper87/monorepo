#include <iostream>
using namespace std;

int main ()
{
    int _case;
    cin>> _case;
    cout<< endl;

    int a, f;
    char char_num;
    int i, j, k;

    for ( int c=0;c<_case;c++ )
    {
        cin>> a>> f;

        for ( i=0;i<f;i++ )
        {
            for ( j=0;j<a-1;j++ )
            {
                for ( k=0;k<=j;k++ )
                {
                    cout<< (j+1);
                }

                cout<< endl;

            }

            for ( ;j>-1;j-- )
            {
                char_num = '0' + j;

                for ( k=0;k<=j;k++ )
                {
                    cout<< (j+1);
                }

                cout<< endl;

            }

            cout<< endl;
        }
    }

    return 0;
}
