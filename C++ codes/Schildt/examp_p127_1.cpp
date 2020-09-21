#include <iostream>
#include <conio.h>
#include <cstring>
using namespace std;

class inventory 
{
      char item[20];
      double cost;
      int on_hand;
      public:
             inventory (char *i, double c, int o)
             {
                       strcpy ( item,i );
                       cost = c;
                       on_hand = o;
             }
             void show ()
             {
                  cout<< item<< "\n"<< cost<< "\n"<<on_hand;
             }
};

int main ()
{
    inventory obj("Wrench",120.55,12);
    
    obj.show();
    
    getch ();
    return 0;
}
