#include <iostream>
using namespace std;


class stack
{
      char stck[10];
      int tos;
      public:
             stack();
             //void init_stack ();
             void push_stack (char ch);
             char pop_stack();     
};

stack::stack()
{
     tos = 0;
}

void stack::push_stack (char ch)
{ 
     stck[tos] = ch;
     tos++;
}

char stack::pop_stack()
{
     tos--;
     return stck[tos];
     
}


int main (void)
{
    int i;
    stack s1, s2;
    
    //s1.init_stack();
    //s2.init_stack();
    
    
    s1.push_stack('a');
    //s2.push_stack('x');
    s1.push_stack('b');
    //s2.push_stack('y');
    s1.push_stack('c');
    //s2.push_stack('z');
    
    s2 = s1;
    
    for ( i=0;i<3;i++ )
    {
        cout<< s1.pop_stack() << '\n';
    }
    
    for ( i=0;i<3;i++ )
    {
        cout<< s2.pop_stack() << '\n';
    }
    
    getchar();
    return 0;
    
}
