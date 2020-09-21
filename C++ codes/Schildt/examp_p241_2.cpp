#include <iostream>
#include <conio.h>
using namespace std;

class base
{
      protected:
                int a, b;
      public:
             void seta ( int m )
             {
                  a = m;
             }
             void setb ( int n ) 
             {
                  b = n;
             }
};

class derived: public base
{
      int c;
      public:
             void setc ( int o )
             {
                  c = o;
             }
             
             void showabc ()
             {
                  cout<< a<< ' '<<  b<< ' '<< c<< '\n';
             }
};

int main ()
{
    derived ob;
    
    ob.seta (10);
    ob.setb (20);
    
    ob.setc (30);
    
    ob.showabc();
    
    getch ();
    return 0;
}
