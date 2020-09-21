#include <iostream>
using namespace std;

int main (void)

{
    double pay;
    double hour;
    double gross;
    
    cout<< "Please enter pay per hour: ";
    cin>> pay;
    cout<< "Please enter daily work hour: ";
    cin>> hour;
    
    gross = pay * hour;
    
    cout<< "The gross payment is: " << gross ;
    
    getchar ();
    getchar ();
    return 0;
    
}
