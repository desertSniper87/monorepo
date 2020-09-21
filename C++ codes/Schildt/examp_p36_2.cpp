#include <iostream>
using namespace std;

int date ( int date, int month, int year );
int date ( char *date);

int main (void)
{
    date (28 , 04 , 12);
    date ( "28/04/12" );
    
    getchar ();
    return 0;
}

int date ( int date, int month, int year )
{
    cout<< "The date is: " << date << " " <<month << " "<< year <<"\n";
}

int date ( char *date)
{
    cout<< "In short form: " << date; 
}
