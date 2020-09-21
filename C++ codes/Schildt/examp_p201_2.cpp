#include <iostream>
#include <conio.h>
using namespace std;

class coord
{
      int x, y;
      public:
             coord ()
             {
                   x = 0;
                   y = 0;
             }
             coord ( int i, int j )
             {
                   x = i;
                   y = j;
             }
             void get_xy ( int &i, int &j )
             {
                  i = x;
                  j = y;
             }
             coord operator+( coord ob2 )
             {
                   coord temp;
                   
                   temp.x = x + ob2.x;
                   temp.y = y + ob2.y;
                   
                   return temp;
             }
             coord operator-( coord ob2 )
             {
                   coord temp;
                   
                   temp.x = x - ob2.x;
                   temp.y = y - ob2.y;
                   
                   return temp;
             }
             coord operator=( coord ob2 )
             {
                   x = ob2.x;
                   y = ob2.y;
                   
                   return *this;
             }
};

int main ()
{
    int x, y;
    coord ob1( 10,20 ), ob2 ( 30,40 );
    
    coord ob3 = ob1 + ob2;
    ob3.get_xy( x, y );
    cout<< "ob1+ob2 = "<< x<<" , "<< y<< endl;
    
    coord ob4 = ob1 - ob2;
    ob4.get_xy( x, y );
    cout<< "ob1-ob2 = "<< x<<" , "<< y<< endl;
    
    coord ob5 = ob1;
    ob5.get_xy ( x,y );
    cout<< "ob1="<< x<<" , "<< y<< endl;
    
    getch ();
    return 0;  
}
