#include <iostream>
using namespace std;

class myclass
{
      int a;
      public:
             myclass();
             ~myclass();
             void show();
};

myclass::myclass()
{
                  cout<< "In constructor \n";
                  a= 10;
}

myclass::~myclass()
{
                   cout<< "Destructing... \n";
                   getchar();
}

void myclass::show()
{
     cout <<"a= " << a<< "\n";
}

int main (void)
{
    myclass object;
    
    object.show();
    
    //getchar ();
    return 0;    
}
