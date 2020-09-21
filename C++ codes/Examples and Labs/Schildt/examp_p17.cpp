#include <iostream>
using namespace std;

int main (void)

{
    int i;
    int j;
    char name[100];
    
    cout<<"Please enter 2 integers and a string: ";
    cin>> i >> j >> name;
    cout<<"The things are: " << i <<' ' << j << ' '<< name;
    
    getchar ();
    getchar ();
    return 0; 
}
