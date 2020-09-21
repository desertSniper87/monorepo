#include <iostream>
using namespace std;

int main ()
{
    int column, row;
    int i, j;

    while ( cin>> column && cin>> row )
    {
        //cout<< "Success";
        int field[100][100];
        i = 0;
        j = 0;

        for ( i=1;i<=column;i++ )
            for( j=1;j<=row;j++ )
            {
                cin>> field[i][j];
            }

        int sweep[100][100];

        for ( i=1;i<=column;i++ )
        {
            for ( j=1;j<=row;j++ )
            {
                if ( field[i][j]=='.' )
                {
                    sweep[i][j] = 0;
                }
                else if ( field[i][j]=='*' )
                {
                    sweep[i][j] = 1;
                }
            }
        }

        int mine[100][100];

        for ( i=1;i<=column;i++ )
        {
            for ( j=1;j<=row;j++ )
            {
                if ( !(sweep[i][j]) )
                {
                    /**
                    Now the fun begins
                    ******************
                    **/

                    if ( i==1&&j==1 )
                        mine[i][j] = sweep[2][1] + sweep[2][2] + sweep[1][2];
                    else if ( i==1&&j<column )
                        mine[i][j] = sweep[1][j-1] + sweep[1][j+1] + mine[2][j] + mine [2][j] + mine[2][j+1];
                    ///This line obviously has bugs
                    else if ( i==row && j==column )
                        mine[i][j] = sweep[i][j-1] + sweep[i-1][j-1] + mine[i][j+1];
                    else if ( i==1&&j>1 )
                        mine[i][j] = sweep[i][j-1] + sweep[i-1][j] + sweep[i-1][j] + mine[i+1][j] + mine[i+1][j+1];
                    else if ( i==row&&j>1 )
                        mine[i][j] = sweep[i][j-1] + sweep [i-1][j-1] + sweep[i-1][j] + sweep[i][j+1] + sweep[i-1][j];
                    else if ( i==1&&j==row )
                        mine[i][j] = sweep[i+1][j+1] + sweep[i][j+1] + sweep[i+1][j];
                    else if ( i==column&&j==row )
                        mine[i][j] = sweep[i][j-1] + sweep[i-1][j-1] + sweep[i-1][j];
                    else
                    {
                        mine[i][j] = sweep[i][j-1] + sweep[i-1][j-1] + sweep[i+1][j] + sweep[i+1][j+1] + sweep[i-1][j+1] + sweep[i-1][j+1] + sweep[i][j+1] + sweep[i-1][j] + sweep[i+1][j-1];
                    }
                }
            }
        }

    }

    return 0;
}
