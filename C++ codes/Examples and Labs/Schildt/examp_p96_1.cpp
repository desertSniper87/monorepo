#include <iostream>
#include <conio.h>
using namespace std;

class myclass 
{
      int a;
      public:
             myclass (int n)
             {
                  a = n;
             }
             int get_a ();
};

int myclass::get_a ()
{
    return a;
}

int sqr_it (myclass n)
{
    return ( n.get_a() * n.get_a() );
}

int main ()
{
    myclass a(10),  b(20);
    
    cout<< sqr_it (a) << "\n" << sqr_it (b) << "\n";
    
    getch();
    return 0; 
}
