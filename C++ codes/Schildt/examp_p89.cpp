#include <iostream>
#include <conio.h>
using namespace std;

class myclass 
{
      int a;
      int b;
      public:
             void set ( int x,int y )
             {
                  a= x;
                  b= y;
             }
             void show ()
             {
                  cout<< "a = " << a << "  b = " << b << "\n";
             }
};

int main ()
{
    myclass ob1, ob2;
    
    ob1.set( 10,4 );
    
    ob2 = ob1;
    
    ob1.show();
    ob2.show();
    
    getch();
    return 0;
    
}
