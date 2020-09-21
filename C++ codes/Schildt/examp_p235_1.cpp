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

//class derived : private base
class derived : public base
{
    int y;
    public:
        void set_y (int n)
        {
            y = n;
        }
        void show_y ()
        {
            cout<<y <<'\n';
        }
};

int main ()
{
    derived ob;

    //cout<< ob.x;
	ob.set_x (10);
	ob.set_y (20);

	ob.show_x();
	ob.show_y();

	return 0;
}
