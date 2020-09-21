#include <iostream>
#include <cstring>
#include <cstdio>
using namespace std;

int main ()
{
    int c, r;
    int i = 1;
    int j = 1;
    int eats = 0;
    int curEat;
    int temp_j;
    int cake[2][10];
    while ( cin>> c >> r )
    {
        for ( i=1;i<=r;i++ )
        {
            for ( j=1;j<=c;j++ )
            {
                cin>> cake[i][j];
            }
        }

        for ( i=1;i<=r;i++ )
        {
            for ( j=1;j<=c;j++ )
            {
                if ( cake[i][j] == 's' )
                {
                    break;
                }
                else if ( cake[i][j] == '.' )
                {
                    int temp_j = j;
                    while ( j<=c )
                    {
                        if ( cake[i][j]== '.' )
                        {
                            curEat++;
                            cake[i][j] == '0';
                            if ( cake [i][j] == 's' )
                            {
                                break;
                            }
                        }
                        j++;
                    }
                    eats = eats + curEat;
                }

                else if ( cake[i][j]==0 )
                    j++;
            }
        }


        for ( j=1;j<=c;j++ )
        {
            for ( i=1;i<=r;i++ )
            {
                if ( cake[i][j] == 's' )
                {
                    break;
                }
                else if ( cake[i][j] == '.' )
                {
                    int temp_j = j;
                    while ( i<=r )
                    {
                        if ( cake[i][j]== '.' )
                        {
                            curEat++;
                            if ( cake [i][j] == 's' )
                            {
                                break;
                            }
                        }
                        i++;
                    }
                    eats = eats + curEat;
                }
            }
        }

    cout<< eats;
    }

    return 0;
}
