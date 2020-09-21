#include <iostream>
using namespace std;

int main (void)
{
    int m,n;
    
    cout<< "Please enter number: \n";
    cin>> m;
    cout<< "Please enter power: \n";
    cin>> n;
    
    int result = 1;
    int i;
    
    for ( i=0;i<n;i++ )  
    {
        result = result * m;
    }
    
    cout<< "Result = " << result;
    
    getchar ();
    getchar ();
    return 0;
}
