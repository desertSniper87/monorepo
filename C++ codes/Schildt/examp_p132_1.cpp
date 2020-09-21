#include <iostream>
#include <conio.h>
using namespace std;

int main ()
{
    int *p;
    
    p = new int;
    
    *p = 1000;
     cout<< "P: "<<"\t"<< *p;
     
     delete p;
     
          
     getch ();
     return 0;
}
