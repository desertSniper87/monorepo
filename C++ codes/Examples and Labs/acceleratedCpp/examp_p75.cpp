#include <iostream>
#include <algorithm>
#include <vector>
using namespace std;

int main ()
{
    vector <int> number_vector;

    cout<< "Please enter your exam nums"<< endl;
    int i;

    while ( cin>>i )
    {
        if ( i<0 )
            break;
        else
        {
            number_vector.push_back( i );
        }
    }

    sort ( number_vector.begin(), number_vector.end() );

    int size = number_vector.size();

    int median;

    if ( size%2 )
    {
        median = number_vector[size/2];
        cout<< median<< endl;
    }

    else
    {
        median = (( number_vector[ (size/2) -1 ] + number_vector[ ( size/2 ) ] ) /2);
        cout<< median<< endl;
    }

    return 0;
}
