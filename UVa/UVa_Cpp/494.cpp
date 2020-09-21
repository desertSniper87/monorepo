#include <iostream>
#include <cstring>
using namespace std;

int main ()
{
    string str;
    char sentence[100];
    int len;
    int i;
    int space = 0;

    while ( getline( std::cin,str ) )
    {
        strcpy( sentence,str.c_str() );

        len = strlen ( sentence );

        for ( i=0;i<len;i++ )
        {
            if ( sentence[i]>='a' && sentence[i]<='z' || sentence[i]>='A' && sentence[i]<='Z' || sentence[i] == ' ' )
            {
                if ( sentence[i]==' ' )
                {
                    space++;
                    if ( sentence[i+1]==' ' )
                    {
                        i++;

                        for ( ;i<len;i++ )
                        {
                            if ( sentence[i]==' ' )
                                i++;
                        }
                    }
                }

            }

            else
            {
                for ( ;sentence[i]<='a' && sentence[i]>='z' || sentence[i]<='A' && sentence[i]>='Z';i++ )
                {

                }
            }

        }

        cout<< space+1<< endl;

        space = 0;
    }

    return 0;
}
