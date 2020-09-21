#include <iostream>
#include <conio.h>
using namespace std;

class myclass 
{
      int n , d;
      public:
             myclass( int i, int j )
             {
                      n= i;
                      d= j;
             }
             friend int isFactor (myclass ob);
};

int isFactor (myclass ob)
{
    if ( ob.n%ob.d )
    {
         return 1;
    }
    else 
         return 0;
}

int main ()
{
    myclass ob1(10,5), ob2 (14,3);
    
    if (!(isFactor(ob1))) printf ("5 is a factor of 10\n");
    else printf ("5 is not a factor of 10\n");
    
    if (!(isFactor(ob2))) printf ("3 is a factor of 14\n");
    else printf ("3 is not a factor of 14\n");
    
    getch();
    return 0;
}
