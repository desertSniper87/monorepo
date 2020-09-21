#include <iostream>
#include <conio.h>
using namespace std;

class array
{
      int size;
      char *p;
      public:
             array( int num );
             ~array ()
             {
                    delete[] p;
             }
             char &put ( int i );
             char get ( int i );
};

array::array( int num )
{
              
}

int main ()
{
    getch ();
    return 0
}
