#include <iostream>
#include <cstring>
using namespace std;

int main  ()
{
    string word;
    int cas = 1;

    while ( cin>>word )
    {
        if ( word=="HELLO" )
            cout<< "Case "<< cas<< ": "<< "ENGLISH"<< endl;
        else if ( word=="HOLA" )
            cout<< "Case "<< cas<< ": "<<  "SPANISH"<< endl;
        else if ( word=="HALLO" )
            cout<< "Case "<< cas<< ": "<<  "GERMAN"<< endl;
        else if ( word=="BONJOUR" )
            cout<< "Case "<< cas<< ": "<<  "FRENCH"<< endl;
        else if ( word=="CIAO" )
            cout<< "Case "<< cas<< ": "<<  "ITALIAN"<< endl;
        else if ( word=="ZDRAVSTVUJTE" )
            cout<< "Case "<< cas<< ": "<<  "RUSSIAN"<< endl;
        else if ( word=="#" )
            break;
        else
            cout<< "Case "<< cas<< ": "<<  "UNKNOWN"<< endl;

        cas++;

    }

    return 0;
}
