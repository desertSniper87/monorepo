#include <iostream>
using namespace std;

int main ()
{
    int i;
    int j;
    int k;

    while ( cin>>i )
    {
        if ( !i )
            break;

        else if ( i<10 )
            cout<< i<< endl;

        else
        {
            j = 0;

            while (1)
            {
                j += i%10;
                i = i/10;

                if ( j>10 )
                {///DO NOTHING
                }

                else if ( i<10 )
                {
                    j = j+i;
                    cout<< j<< endl;
                    break;
                }

                else
                {
                    i = j;
                    j = 0;
                }
            }
        }

    }

        /**
        else
        {
              j = i;
            while ( i>10 )
            {
                k = 0;

                while ( i>10 )
                {
                    k += i%10;
                    i = i/10;
                }

                if ( k<10 )
                {
                    if ( k==0 )
                        cout<< i<< endl;
                    else
                    {
                        cout<< k<< endl;
                        break;
                    }
                }

                else
                    i = k;
            }
        }

        **/
}
