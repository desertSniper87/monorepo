#include <iostream>
#include <cmath>
using namespace std;

int main ()
{
    float num;
    int t;
    float guess;
    int i;
    float x;

    cin>> t;
    while ( t )
    {
        cin>> num;
        guess = num/2;
        for ( i=0;i<100;i++ )
        {
            x = guess;
            x += 2;
            x /= guess;
            x += guess;
            guess = x;
        }

        cout<< guess<< endl;

        t--;
    }

    return 0;
}
