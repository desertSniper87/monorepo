#include <iostream>
#include <conio.h>
using namespace std;

namespace NS1
{
          class NS1Class
              {
              int i;
              int j;
              public:
                     NS1Class ( int x, int y )
                     {
                          i = x;
                          j = y;
                     }
                     void seti ( int x )
                     {
                          i = x;
                     }
                     void setj ( int x )
                     {
                          j = x;
                     }
                     int geti ()
                     {
                         return i;
                     }
                     int getj ()
                     {
                         return j;
                     }
              };
              char str[] = "That's a test";
              int counter = 0;
};

namespace NS2
{
          int x;
          int y;
}

int main ()
{
    NS1::NS1Class NS1ob( 10,20 );
    
    cout<< NS1ob.geti()<< endl;
    
    NS1ob.seti ( 30 );
    
    cout<< NS1ob.geti()<< endl;
    
    using NS1::counter;
    for ( counter=10;counter;counter-- )
    {
        cout<< " * "<< endl;
    }
    
    using namespace NS1;
    cout<< str<< endl;
    
    cout<< NS1ob.geti()<< endl;
    
    NS1ob.setj ( 50 );
    cout<< NS1ob.getj();
    
    NS2::x = 10;
    NS2::y = 20;
    
    using namespace NS2;
    NS1Class NS2ob( 10,20 );
    
    cout<< NS2ob.geti()<<endl;
    cout<< NS2ob.getj()<<endl;
    
    getch();
    return 0;  
}
