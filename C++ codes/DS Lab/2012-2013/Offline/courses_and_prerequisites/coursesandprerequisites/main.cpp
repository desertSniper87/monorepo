#include <iostream>
#include <cstdio>

using namespace std;

int q[ 20 ], top = -1, front = -1, rear = -1, array[ 20 ][ 20 ], visited_flag[ 20 ], stack[ 20 ];

void push( int item )
{
    if ( top == 19 )
        printf( "Stack overflow " );
    else
        stack[ ++top ] = item;
}

int pop()
{
    int k;

    if ( top == -1 )
        return ( 0 );
    else
        {
            k = stack[ top-- ];
            return ( k );
        }
}

void add
    ( int item )
    {
        if ( rear == 19 )
            printf( "Queue full" );
        else
            {
                if ( rear == -1 )
                    {
                        q[ ++rear ] = item;
                        front++;
                    }
                else
                    q[ ++rear ] = item;
            }
    }
//end of add()
/***
int delete()
{
    int k;

    if ( ( front > rear ) || ( front == -1 ) )
        return ( 0 );
    else
        {
            k = q[ front++ ];
            return ( k );
        }
}
***/
void bfs( int source, int vertex_number )
{
    int p, i;

    add
        ( source );

    visited_flag[ source ] = 1;

    ///p = delete();

    if ( p != 0 )
        printf( " %d", p );

    while ( p != 0 )
        {
            for ( i = 1;i <= vertex_number;i++ )
                if ( ( array[ p ][ i ] != 0 ) && ( visited_flag[ i ] == 0 ) )
                    {
                        add
                            ( i );

                        visited_flag[ i ] = 1;
                    }

            ///p = delete();

            if ( p != 0 )
                printf( " %d ", p );
        }

    for ( i = 1;i <= vertex_number;i++ )
        if ( visited_flag[ i ] == 0 )
            bfs( i, vertex_number );
}



void dfs( int source, int vertex_number )
{
    int i, k;
    push( source );
    visited_flag[ source ] = 1;
    k = pop();

    if ( k != 0 )
        printf( " %d ", k );

    while ( k != 0 )
        {
            for ( i = 1;i <= vertex_number;i++ )
                if ( ( array[ k ][ i ] != 0 ) && ( visited_flag[ i ] == 0 ) )
                    {
                        push( i );
                        visited_flag[ i ] = 1;
                    }

            k = pop();

            if ( k != 0 )
                printf( " %d ", k );
        }

    for ( i = 1;i <= vertex_number;i++ )
        if ( visited_flag[ i ] == 0 )
            dfs( i, vertex_number );
}


main()
{
    /**
    Main code for requisite and pre-requisite input
    **/

    char num_crs;
    char pre;

    cout<< "Please enter the number of courses"<< endl;
    cin>> num_crs;

    /**
    Main code for bfs and dfs input
    NB. only dfs will be used
    NB2. the courses and pre-reqs will be transformed to graph
    **/

    int vertex_number, i, source, choice, j;
    char character_char, dummy;
    char crs_arr[num_crs];

    for ( i=0;i<num_crs;i++ )
    {
        cout<< "Please enter the the name of course"<< i<< "in char"<< endl;
        cin>> character_char;
        crs_arr[i] = character_char;
    }

    /**
    transforming input
    **/

    vertex_number = num_crs;

    ///scanf( "%d", &vertex_number );

    for ( i=1;i<=vertex_number;i++ )
        {
            for ( j=1;j<=vertex_number;j++ )
                {
                    printf( "Enter 1 if %ch has a node with %d ELSE 0 ", crs_arr[i], crs_arr[j] );
                    scanf( "%d", &array[ i ][ j ] );
                }
        }

    printf( "The adjacency matrix is\n" );

    for ( i = 1;i <= vertex_number;i++ )
        {
            for ( j = 1;j <= vertex_number;j++ )
                {
                    printf( " %d", array[ i ][ j ] );
                }

            printf( "\n" );
        }

    do
        {
            for ( i=1;i<=vertex_number;i++ )
                visited_flag[ i ] = 0;

            ///printf( "\nMenu" );

            ///printf( "\n1.B.F.S" );

            ///printf( "\n2.D.F.S" );

            ///printf( "\nEnter your choice" );

            ///scanf( "%d", &choice );

            ///printf( "Enter the source vertex :" );

            ///scanf( "%d", &source );

            /***

            switch ( choice )
                {

                case 1:
                    bfs( source, vertex_number );
                    break;

                case 2:
                    dfs( source, vertex_number );
                    break;
                }
            ***/

            ///Modifying code

            bfs( source, vertex_number );


            printf( "Do you want to continue(Y/N) ? " );
            scanf( "%c", &dummy );
            scanf( "%c", &character_char );
        }
    while ( ( character_char == 'y' ) || ( character_char == 'Y' ) );
}

//End of main
