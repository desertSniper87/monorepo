#include <iostream>
using namespace std;

int main (void)
{
    int i;
    
    cout<< "Please enter number: ";
    cin>> i;
    
    int fact = 1;
    
    for ( ;i>=1;i-- )
    {
        fact = fact * i;
    }
    
    cout<< "The factorial is: " << fact;
    
    getchar ();
    getchar ();
    return 0;
}
