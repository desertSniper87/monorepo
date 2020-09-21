#include <iostream>
using namespace std;

class myclass
{
      int a;
      public:
             myclass();
             void show();
};

myclass::myclass()
{
                  cout<< "In constructor \n";
                  a= 10;
}

void myclass::show()
{
     cout <<"a= " << a;
}

int main (void)
{
    myclass object;
    
    object.show();
    
    getchar ();
    return 0;    
}
