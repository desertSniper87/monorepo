#include <iostream>
using namespace std;

class myclass
{     
      int x;
      int y;
      public:
             
             void show ();
             myclass (int i, int j);
};

myclass::myclass (int i, int j)
{
                 x = i;
                 y = j;                
}

void myclass::show()
{
     cout<< "x + y = " << x + y << "\n";
}

int main ()
{
    int a;
    int b;
    
    cout<< "Please enter x: \n";
    cin>> a; 
    cout<< "Please enter y: \n";
    cin>> b;
    
    myclass ob1(a,b);
    ob1.show();
    
    getchar ();
    getchar ();
    return 0;
}
