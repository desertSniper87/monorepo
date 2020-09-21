#include <iostream>
#include <conio.h>
using namespace std;

class samp 
{
      int m;
      int n;
      public:
             samp (int a, int b)
             {
                  m = a;
                  n = b;
             }
             int get_a() { return m; }
             int get_b() { return n; }
};

int main ()
{
    int i = 0;
    
    samp obj[4] = 
    {
         samp (21,32),
         samp (34,54),
         samp (54,32),
         samp (21,57),
    };
    /*
    for ( i=0;i<4;i++ )
    {
        obj( i,i*2 );
    }
    */      
    samp *p;
    p = obj;
    
    for ( i=0;i<4;i++ )
    {
        cout<< p->get_a ()<< " ";
        cout<< p->get_b ()<< "\n"; 
        
        p++;
    }
    
    getch();
    return 0;
}
