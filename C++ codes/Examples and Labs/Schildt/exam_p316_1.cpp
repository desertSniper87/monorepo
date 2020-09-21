#include <iostream>
#include <fstream>
#include <conio.h>
using namespace std;

int main ()
{
    ofstream fout ("c:\\test.txt");
    
    if ( !fout )
    {
         cout<< "Write error!";
    }
    
    fout<< "Hello_world"<< endl;
    fout<< 100<< endl;
    fout<< hex<< 100<< endl;
    
    fout.close();
    
    ifstream fin ( "c:\\test.txt" );
    
    if ( !fin )
    {
         cout<< "Read error";
    }
    
    int i;
    char ch[100];
    
    fin>> ch>> i;
    cout<< ch<< endl<< i<< endl;
    
    getch ();
    return 0;
}
