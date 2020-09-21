#include <iostream>
#include <fstream>
#include <cstring>
using namespace std;

int main ()
{
    int i;
    int j;
    int k;
    int l;
    int vertices;
    int edges;
    int cost[20][20];

    ifstream myFile( "prim_data.txt" );
    if ( myFile.is_open() )
    {
        while ( !myFile.eof() )
        {
            myFile>> i;
            vertices = i;

            myFile>> j;
            edges = j;

            for ( l=1;l<=vertices;l++ )
            {
                myFile>> i>> j>> k;

                cost[i][j] = k;
            }
        }

        myFile.close();
    }

    else
        cout<< "Unable to open file"<< endl;

    int threshold;
    int u;
    int v;
    int visit[edges];
    int visited[edges];
    int m;
    int stack[20];
    int top = 0;


    for ( i=1;i<=edges;i++ )
    {
        for ( j=1;j<=edges;j++ )
        {
            if ( cost[i][j] == 0 )
                cost[i][j] = 42000;
        }
    }

    //cout<< "test";

    while ( k<edges )
    {
        threshold  = 42000;
        if ( k==1 )
        {
            for ( i=1;i<=edges;i++ )
            {
                for ( j=1;j<=threshold;j++ )
                {
                    if (cost[i][j] < threshold)
                    {
                        m = cost[i][j];
                        k = i;
                    }
                }
            }
        }

        else
        {
            for ( j=edges;j>=1;j-- )
            {
                if ( cost[v][j]<threshold && visited[j]!=1 && visit[j]!=1 )
                {
                    visit[j] = 1;
                    stack [top] = j;
                    top++;
                    m = cost[v][j];
                    u = j;
                }

            cost[v][u] = 42000;
            v = u;
            cout<< v<< " ";

            k++;
            visit[v] = 0;
            visited [v] = 1;
        }
    }

    //*
    for ( int i=1;i<=10;i++ )
    {
        for ( int j=1;j<=10;j++ )
            cout<< cost [i][j]<< " ";

        cout<< "\t";
    }
    //*/


    }
    return 0;
}
