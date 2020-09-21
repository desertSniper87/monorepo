#include <iostream>
#include <conio.h>
using namespace std;

template < class data_t > class list
{
         data_t data;
         list *next;
         public:
                list ( data_t d )
                {
                     data = d;
                     next = 0;
                }
                    void add ( list *node )
                {
                     //node->data = d;
                     node->next = this;
                     next = 0;
                }
                list *getnext()
                {
                     return next;
                }
                data_t getdata()
                {
                     return data;
                }
}; 

int main ()
{
    list < char > start ( 'a' );
    list < char > *p, *last;
    
    last = &start;
    for ( int i=1;i<26;i++ )
    {
        p = new list < char > ( 'a' + i );
        p->add ( last );
        last = p;
    }
    
    p = &start;
    while ( p )
    {
          cout<< p->getdata();
          p = p->getnext();
    }
    
    getch();
    return 0;
}
