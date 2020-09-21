#include <iostream>
//#include <conio.h>
#include <cstring>
using namespace std;

class StringType
{
    char *p;
    public:
        StringType ( char *s );
        StringType ( StringType const &s );
        ~StringType ()
        {
            delete[] p;
        }
        char *get ()
        {
            return p;
        }
};

StringType::StringType ( char *s )
{
    int len = strlen( s ) + 1;
    for ( int i=0;i<len;i++ )
    {
        cout<< *(s+i);
    }
    cout<< endl;
}

StringType::StringType ( StringType const &s )
{
    int len = strlen ( s.p ) + 1;
    for ( int i=0;i<len;i++ )
    {
        cout<< *(s.p+i);
    }
    cout<< endl;
}

int main ()
{
    StringType a("Hello");
    StringType b("World!");

    return 0;
}
