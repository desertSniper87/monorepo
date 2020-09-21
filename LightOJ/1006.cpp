#include <iostream>
#include <cstdio>
#include <ctime>
using namespace std;

int a, b, c, d, e, f;
int dp_table[10001];

int fn( int n )
{
    if( n == 0 ) return a;
    if( n == 1 ) return b;
    if( n == 2 ) return c;
    if( n == 3 ) return d;
    if( n == 4 ) return e;
    if( n == 5 ) return f;

    if ( dp_table[n] = -1 )
    {
        dp_table[n]=( (((((fn(n-1)+ fn(n-2) )%10000007 + fn(n-3))%10000007 + fn(n-4))%10000007 + fn(n-5))%10000007 + fn(n-6))%10000007 );
        return dp_table[n];
    }
    else
        return dp_table[n];
}
int main()
{
    clock_t begin;
    for ( int i = 0;i<10001;i++ )
        dp_table[i] = -1;

    int n, caseno = 0, cases;
    scanf("%d", &cases);
    while( cases-- )
    {
        scanf("%d %d %d %d %d %d %d", &a, &b, &c, &d, &e, &f, &n);
        //for ( int i=0;i<=70;i++ )
        //    cout<< dp_table[i];
        clock_t begin = clock();
        printf("Case %d: %d\n", ++caseno, fn(n) % 10000007);

    }

    clock_t end = clock();

    cout<< double(end-begin);
    return 0;
}
