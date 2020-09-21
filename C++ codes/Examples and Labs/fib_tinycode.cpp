#include <iostream>
using namespace std;

int main ()
{
    int n=100;

    int a=1,
    b=0,
    x,i;

    for(i=0;i<n;n--)
    {
        cout<<a+b << " ";
        x=a+b;
        a=b;
        b=x;
    }

    return 0;
}
