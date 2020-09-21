#include <iostream>
using namespace std;

int m , n;
int grid[100][100];

int probLocator ( int curRow, int curCol )
{
    curRow = 0;
    cutCol = 0;
    int saved = 0;

    if ( !(grid[curRow][curCol]) )
    {
        saved += grid[curRow][curCol];
    }

    if ( grid[curRow][curCol+1]<=grid[curRow+1][curCol] )
    {
        cur
    }
}

int main ()
{
    char space[1];
    int t;
    cin>> t;
    cin.getline ( space,1 );
    cin>> m>> n;

    for ( int i = 0;i<m;i++ )
    {
        for ( int j = 0;j<n;j++ )
        {
            cin>> grid[i][j];
        }
    }

    while ( t )
    {


        t--;
    }

    return 0;
}
