#include <stdio.h>

int main ( void )
{
    int n;
    int i;
    int ans = 1;
    int case_ = 1;

    while ( scanf( "%d",&n ) )
    {
        if ( n<0 )
            break;

        else if ( n==0 )
        {
            printf ( "Case" );
            printf ( " %d:",case_ );
            printf ( " %d",0 );
            printf ( "\n" );
        }

        else
        {

            for ( i=2;i<n;i = i*2 )
            {
                ans++;
            }

            printf ( "Case" );
            printf ( " %d:",case_ );
            printf ( " %d",ans );
            printf ( "\n" );

        }

        ans = 1;
        case_++;
    }

    return 0;
}
