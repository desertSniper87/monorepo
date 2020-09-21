#include <iostream>
using namespace std;

class myclass 
{
      int a;
      int b;
      public:
             myclass (int x,int y);
             void show();
};

myclass::myclass (int x,int y)
{
                 cout<< "Constructing..."<< "\n";
                 a= x;
                 b= y;
}

void myclass::show()
{
               cout<< "a=" << a << "\n";
               cout<< "b=" << b << "\n";
               cout<< "____"<<"\n";
               cout<< "a+b="<<(a+b)<<"\n";
}

int main (void)
{
    myclass ob(246,985);
    ob.show ();
    
    getchar ();
    return 0;   
}
