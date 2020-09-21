#include <iostream>
#include <cmath>
using namespace std;

int rev_num ( int num );

int main ()
{
    int i;
    int step;
    int _case;
//    cin>> _case;
//
////    for ( int c=0;c<_case;c++ )
////    {
//
        unsigned int num;
//        cin>> num;
//        step = 0;

//        for ( ;;step++ )
//        {
//            num += rev_num(num);
//            cout<< num<< endl;
//        }

//        cout<< step<< " "<< num<< endl;

        while ( cin>>num )
            cout<< rev_num(num)<< endl;



//    }

    return 0;
}

int rev_num ( int num )
{
    int output;

    while ( num )
    {
        output = output * 10 + ( num%10 );
        num = num/10;
    }

    return output;
}
