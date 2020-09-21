#include <iostream>
using namespace std;

int main (void)

{
    char ch;
    cout<< "Enter characters, press x to stop";
    
    for ( ;ch!='x'; )
    {
        cin>> ch;
    }
    
    getchar ();
    return 0;

}
