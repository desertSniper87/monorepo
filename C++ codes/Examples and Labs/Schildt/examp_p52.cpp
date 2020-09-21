#include <iostream>
using namespace std;

class myclass 
{
      int a;
      public:
             myclass (int x);
             void show();
};

myclass::myclass (int x)
{
                 cout<< "Constructing..."<< "\n";
                 a= x;
}

void myclass::show()
{
               cout<< "a=" << a << "\n";
}

int main (void)
{
    myclass ob(10);
    ob.show ();
    
    getchar ();
    return 0;   
}
