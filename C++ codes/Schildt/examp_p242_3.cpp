#include <iostream>
#include <conio.h>
using namespace std;

class base
{
      protected:
                int a,b;
      public:
             void setab ( int n, int m )
             {
                  a = n;
                  b = m;
             }
};

class derived: protected base
{
      int c;
      public:
             void setc ( int o )
             {
                  c = o;
             }
      void showabc ()
      {
           cout<< a<< ' '<< b<< ' '<< c<< '\n';
      }
};

int main ()
{
    derived ob;
    
    ob.setab ( 10,20 );
    ob.setc (30);
    
    ob.showabc();
    
    getch ();
    return 0;
}
