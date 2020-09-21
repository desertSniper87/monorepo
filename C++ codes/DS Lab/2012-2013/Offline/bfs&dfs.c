#include<stdio.h>

int q[ 20 ], top = -1, front = -1, rear = -1, a[ 20 ][ 20 ], visited_flag[ 20 ], stack[ 20 ];

int delete();

void add ( int item );

void bfs( int s, int vertex_number );

void dfs( int s, int vertex_number );

void push( int item );

int pop();

main()
{
    int vertex_number, i, s, choice, j;
    char character_char, dummy;
    printf( "Enter the number of vertices " );
    scanf( "%d", &vertex_number );

    for ( i = 1;i <= vertex_number;i++ )
        {
            for ( j = 1;j <= vertex_number;j++ )
                {
                    printf( "Enter 1 if %d has a node with %d ELSE 0 ", i, j );
                    scanf( "%d", &a[ i ][ j ] );
                }
        }

    printf( "The adjacency matrix is\n" );

    for ( i = 1;i <= vertex_number;i++ )
        {
            for ( j = 1;j <= vertex_number;j++ )
                {
                    printf( " %d", a[ i ][ j ] );
                }

            printf( "\n" );
        }

    do
        {
            for ( i = 1;i <= vertex_number;i++ )
                visited_flag[ i ] = 0;

            printf( "\nMenu" );

            printf( "\n1.B.F.S" );

            printf( "\n2.D.F.S" );

            printf( "\nEnter your choice" );

            scanf( "%d", &choice );

            printf( "Enter the source vertex :" );

            scanf( "%d", &s );

            switch ( choice )
                {

                case 1:
                    bfs( s, vertex_number );
                    break;

                case 2:
                    dfs( s, vertex_number );
                    break;
                }

            printf( "Do you want to continue(Y/N) ? " );
            scanf( "%c", &dummy );
            scanf( "%c", &character_char );
        }
    while ( ( character_char == 'y' ) || ( character_char == 'Y' ) );
}

void bfs( int s, int vertex_number )
{
    int p, i;

    add
        ( s );

    visited_flag[ s ] = 1;

    p = delete();

    if ( p != 0 )
        printf( " %d", p );

    while ( p != 0 )
        {
            for ( i = 1;i <= vertex_number;i++ )
                if ( ( a[ p ][ i ] != 0 ) && ( visited_flag[ i ] == 0 ) )
                    {
                        add
                            ( i );

                        visited_flag[ i ] = 1;
                    }

            p = delete();

            if ( p != 0 )
                printf( " %d ", p );
        }

    for ( i = 1;i <= vertex_number;i++ )
        if ( visited_flag[ i ] == 0 )
            bfs( i, vertex_number );
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

void dfs( int s, int vertex_number )
{
    int i, k;
    push( s );
    visited_flag[ s ] = 1;
    k = pop();

    if ( k != 0 )
        printf( " %d ", k );

    while ( k != 0 )
        {
            for ( i = 1;i <= vertex_number;i++ )
                if ( ( a[ k ][ i ] != 0 ) && ( visited_flag[ i ] == 0 ) )
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
