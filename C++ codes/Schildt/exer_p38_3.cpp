#include <iostream>
#include <cstring>
using namespace std;

int min ( int a ,int b );
double min ( double a ,double b );
char min ( char* a ,char* b );

int main (void)
{
    int a, b;
    cout<<"Please enter two integers: "<< "\n";
    cin>> a >> b;
    cout<< "The minimum of the two integer arguments: " <<  min ( a , b )<< "\n";
    
    double x , y;
    cout<<"Please enter two doubles"<<"\n";
    cin>> x >> y;
    cout<< "The minimum of the two double arguments: " <<  min ( x , y )<<"\n";
    
    char e[100], f[100] ;
    cout<<"Please enter two characters: "<< "\n";
    cin >> e >> f;
    cout<< "The minimum of the two character arguments: "<< min ( *e , *f )<< "\n";
    
    getchar ();
    getchar ();
    return 0;
}   

int min ( int a ,int b )
{
    if ( a<b )
    return a;
    else if (b<a)
    return b;
    else
    return 0;
}

double min ( double a ,double b )
{
    if ( a<b )
    return a;
    else if (b<a)
    return b;
    else
    return 0;
}

char min ( char* a ,char* b )
{
    if ( *a<*b )
    return *a;
    else if (*b<*a)
    return *b;
    else
    return 0;
}




