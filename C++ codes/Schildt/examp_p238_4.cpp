#include <iostream>
#include <conio.h>
using namespace std;

class base
{
      int x;
      public:
          /*
          base ( int n )
          {
               x = n;
          }
          */
          void set_x( int n )
          {
               x = n;
          }
          void show_x ()
          {
               cout<<x <<'\n';
          }

};

class derived : private base
//class derived : public base
{
    int y;
    public:
        void set_xy (int n, int m)
        {
             set_x (n);
             y = m;
        }
        void show_xy ()
        {
             show_x();
             //cout<< '\n';
             cout<<y <<'\n';
        }
};

int main ()
{
    derived ob;

    //cout<< ob.x;
	ob.set_xy (10,20);

	ob.show_xy ();
    
    getch ();
	return 0;
}
