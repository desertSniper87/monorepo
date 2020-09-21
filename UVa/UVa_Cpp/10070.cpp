#include <iostream>
using namespace std;

int main ()
{
    unsigned long long int year;
    bool huluculu;

    while ( cin>>year )
    {
        if ( !(year%15) )
            huluculu = 1;
        else
            huluculu = 0;

        if ( !(year%4) )
        {
            if ( year%100 || !(year%400) )
            {
                cout<< "This is leap year."<< endl;

                if ( huluculu )
                    cout<< "This is huluculu festival year."<< endl;

                if ( !(year%55) )
                {
                    cout<< "This is bulukulu festival year."<< endl;
                }

            }

            else
                cout<< "This is an ordinary year."<< endl;
        }

        else if ( huluculu )
            cout<< "This is huluculu festival year."<< endl;


        else
             cout<< "This is an ordinary year."<< endl;

        cout<< endl;
    }

    return 0;
}
