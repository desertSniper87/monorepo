#include <iostream>
using namespace std;

int main ()
{
    long long int b1, b2, b3;
    long long int g1, g2, g3;
    long long int c1, c2, c3;

    /*
    long long int brBottle = 0;
    long long int grBottle = 0;
    long long int clBottle = 0;
    */

    //long long int flag = 1;
    long long int bin1;
    long long int bin2;
    long long int bin3;
    long long int bin;

    int lowestMove = 999999999999L;
    long long int lowestBin;
    long long int i= 0;

    /**
    LET,
    BCG -> 0
    CBG -> 1
    CGB -> 2
    BGC -> 3
    GCB -> 4
    GBC -> 5
    **/

    while ( cin>> b1>> g1>> c1>> b2>> g2>> c2>> b3>> g3>> c3 )
    {
        for ( i=0;i<6;i++ )
        {
            if ( i==0 )
            {
                bin1 = b2 + b3;
                bin2 = c1 + c3;
                bin3 = g1 + g2;

                bin  = bin1 + bin2 + bin3;
                if ( lowestMove>bin )
                {
                    lowestMove = bin;
                    lowestBin = i;
                }
            }

            if ( i==1 )
            {
                bin1 = c2 + c3;
                bin2 = b1 + b3;
                bin3 = g1 + g2;

                bin  = bin1 + bin2 + bin3;
                if ( lowestMove>bin )
                {
                    lowestMove = bin;
                    lowestBin = i;
                }
            }

            if ( i==2 )
            {
                bin1 = c2 + c3;
                bin2 = g1 + g3;
                bin3 = b1 + b2;

                bin  = bin1 + bin2 + bin3;
                if ( lowestMove>bin )
                {
                    lowestMove = bin;
                    lowestBin = i;
                }
            }

            if ( i==3 )
            {
                bin1 = b2 + b3;
                bin2 = g1 + g3;
                bin3 = c1 + c2;

                bin  = bin1 + bin2 + bin3;
                if ( lowestMove>bin )
                {
                    lowestMove = bin;
                    lowestBin = i;
                }
            }

            if ( i==4 )
            {
                bin1 = g2 + g3;
                bin2 = c1 + c3;
                bin3 = b1 + b2;

                bin  = bin1 + bin2 + bin3;
                if ( lowestMove>bin )
                {
                    lowestMove = bin;
                    lowestBin = i;
                }
            }

            if ( i==5 )
            {
                bin1 = g2 + g3;
                bin2 = b1 + b3;
                bin3 = c1 + c2;

                bin  = bin1 + bin2 + bin3;
                if ( lowestMove>bin )
                {
                    lowestMove = bin;
                    lowestBin = i;
                }
            }
        }

        if ( lowestBin == 0 )
            cout<< "BCG"<< " "<< lowestMove<< endl;
        else if ( lowestBin == 1 )
            cout<< "CBG"<<" "<< lowestMove<< endl;
        else if ( lowestBin == 2 )
            cout<< "CGB"<<" "<< lowestMove<< endl;
        else if ( lowestBin == 3 )
            cout<< "BGC"<<" "<< lowestMove<< endl;
        else if ( lowestBin == 4 )
            cout<< "GCB"<<" "<< lowestMove<< endl;
        else if ( lowestBin == 5 )
            cout<< "GBC"<<" "<< lowestMove<< endl;

        lowestBin = 99999;
        lowestMove = 999999999999L;

    }

    return 0;

}
