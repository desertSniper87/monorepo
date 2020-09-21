#include <iostream>
#include <cstdio>
#include <cstring>
using namespace std;

int main ()
{
    char str[100];
    int i ;
    int j;
    bool flag;
    while ( cin>> str )
    {
        i = 0;
        flag = 0;
        while ( str[i] != '\0' )
        {
            if ( str[i] == 34 && flag == 0 )
            {
                flag = 1;
                str[i] = 96;
                j = i;
                while ( str[j] != '\0' )
                {
                    str[j+1] = str[j];
                    j++;
                }
                str[i+1] = 96;
            }
            else if ( str[i] == 34 && flag == 1 )
            {
                flag = 0;
                str[i] = 39;
                j = i;
                while ( str[j] != '\0' )
                {
                    str[j+1] = str[j];
                    j++;
                }
                str[i+1] = 39;
            }

            i++;
        }

        cout<< str<< endl;
    }

    return 0;
}
