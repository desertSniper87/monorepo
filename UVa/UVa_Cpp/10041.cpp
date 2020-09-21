#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

int main ()
{
    int t, r, i ,j, median, ans;
    vector <int> addr;
    cin>> t;
    while ( t )
    {
        i = 0;
        cin>> r;
        j = r;

        while ( j )
        {
            cin>> i;
            addr.push_back ( i );
            i++;
            j--;
        }


        sort ( addr.begin() , addr.end() );

        if (!(r%2))
            median = addr[r/2 + 1];
        else
            median = ((addr[r/2] + addr[(r/2)+1]))/2;

        ans = 0;

        for ( i=0;i<r;i++ )
        {
            ans += abs ( addr[i] - median );
        }

        //cout<< median<< endl;
        cout<< ans<< endl;

        t--;
    }

    return 0;
}
