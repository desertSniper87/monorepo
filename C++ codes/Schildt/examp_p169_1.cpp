#include <iostream>
#include <conio.h>
using namespace std;

class array
{
      int *p;
      int size;
      public:
             /*
             using normal constructor
             */
             array ( int sz )
             {
                   p = new int[sz];
                   if ( !p )
                   {
                        exit(1);
                   }
                   size = sz;
                   cout<< "Using normal constructor"<< endl;
             }
             ~array()
             {
                     delete[] p;
             }
             // Using copy constructor
             array ( const array &a );
             void put ( int i,int j )
             {
                  if ( i>=0 && i<size )
                  {
                       p[i] = j;
                  }
             }
             int get ( int i )  
             {
                 return p[i];
             }
};

// Allocating memory for the copy

array::array ( const array &a ) 
{
             int i;
             
             size = a.size;
             p = new int[ size ];
             for ( i=0;i<a.size;i++ )
             {
                 p[i] = a.p[i];
             }
             cout<< "Using copy constructor"<< endl;
}

int main ()
{
    array num(10);
    cout<< "Invoking normal constructor"<< endl;
    int i;
    
    for ( i=0;i<10;i++ )
    {
        num.put ( i,i );
    }
    for ( i=9;i>=0;i-- )
    {
        cout<< num.get ( i );
    } 
    cout<< endl;
    
    cout<<"Invoking copy constructor"<< endl;
    array x = num;
    
    for ( i=0;i<10;i++ )
    {
        cout<< x.get(i);
    }
    
    getch();
    return 0;
    
}
