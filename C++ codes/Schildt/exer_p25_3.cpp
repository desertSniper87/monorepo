#include <iostream>
using namespace std;

class myclass 
{
      public:
             int a;
};

int main (void)
{
    myclass ob1, ob2;
    
    ob1.a = 100;
    ob2.a = 200;
    
    cout<< ob1.a << '\n';
    cout<< ob2.a ;
    
    getchar ();
    return 0;
}
