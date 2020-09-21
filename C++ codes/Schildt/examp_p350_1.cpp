#include <iostream>
#include <conio.h>
using namespace std;

class myclass 
{
      int x;
      public:
             myclass ( int i )
             {
                     x = i;
             }
             virtual int func ()
             {
                     cout<< "Using base function"<< endl;
                     return x;
             }
};

class derived1: public myclass
{
      cout<< "Using class derived1's function"<< endl;
      return i+i;
};

class derived2: public myclass
{
      cout<< "Using class derived2's function"<< endl;
      return i*i;
};


int main ()
{
    base *p;
    myclass ob;
    derived1 d_ob1;
    derived2 d_ob2;
    
    p = &ob;
    p->func();
    
    p = &d_ob1;
    p->func();
    
    p = &d_ob2;
    p->func();
    
    getch ();
    return 0;
}
