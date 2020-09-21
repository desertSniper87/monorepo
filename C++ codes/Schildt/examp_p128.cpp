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
                       strcpy ( this->item,i );
                       this->cost = c;
                       this->on_hand = o;
             }
             void show ()
             {
                  cout<< this->item<< "\n"<< this->cost<< "\n"<<this->on_hand;
             }
};

int main ()
{
    inventory obj("Wrench",120.55,12);
    
    obj.show();
    
    getch ();
    return 0;
}
