#include <iostream>
using namespace std;

int abs (int x);
long abs (long x);
double abs (double x);


int main (void)
{
    cout<< "In int abs(): "<< abs (-10)<<"\n";    
    cout<< "In long abs(): "<< abs (-10L)<<"\n";
    cout<< "In double abs(): "<< abs (-10.001)<<"\n";
    
    getchar ();
    return 0;
}

int abs (int x)
{
    return x<0 ? -x : x;
}

long abs (long x)
{
    return x<0 ? -x : x;
}

double abs (double x)
{
    return x<0 ? -x : x;
}
