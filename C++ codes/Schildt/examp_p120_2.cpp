#include <iostream>
#include <conio.h>
using namespace std;

class samp 
{
      int i;
      public:
             samp (int n)
             {
                  i = n;
             }
             int get ();
};

int samp::get()
{
    return i;
}

int main ()
{
    int i;
    samp ob[4] = {12,32,-32,123};
    
    for ( i=0;i<4;i++ )
    {
        cout<< ob[i].get()<< "\n";
    }
    
    getch ();
    return 0;
}
