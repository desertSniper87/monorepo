#include <iostream>
using namespace std;

int main (void)

{
    double inch;
    double foot;
    
    
    cout<< "Please enter feet: ";
    cin>>foot ;
        
    inch = foot * 12;
    
    cout<< "The inch is: " << inch;
    
    getchar ();
    getchar ();
    return 0;
    
}
