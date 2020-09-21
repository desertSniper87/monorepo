
/**
Solved
**/

#include <iostream>
#include <cstring>
#include <sstream>
#include <algorithm>
using namespace std;

template <typename T>
  string NumberToString ( T Number )
  {
     ostringstream ss;
     ss << Number;
     return ss.str();
  }

bool prime_Detector ( int n )
{
    bool flag = 1;

    for ( int i=2;i<n/2;i++ )
    {
        if ( !(n%i) )
        {
            flag = 0;
            break;
        }
    }

    return flag;
}


int main ()
{
    int i;

    while ( cin>> i )
    {
        string reverse_number_string = NumberToString( i );
        std::reverse(reverse_number_string.begin(), reverse_number_string.end());

        int reverse_number;
        stringstream ( reverse_number_string )>> reverse_number;

        if ( prime_Detector( i ) && prime_Detector(reverse_number) && i!=reverse_number )
            cout<< i<< " is emirp."<< endl;
        else if ( prime_Detector( i ) )
            cout<< i<< " is prime."<< endl;
        else
            cout<< i<< " is not prime."<< endl;
    }

    return 0;

}
