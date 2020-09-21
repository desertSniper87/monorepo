#include <iostream>
#include <cstdlib>
#include <cstring>
using namespace std;

#define SIZE 10
/*
void stck (char c);
void push (char ch);
void pop();
*/
class stack 
{
      int tos;
      char who;
      int stck[SIZE];
      public:
             stack( char c );
             void push (char ch);
             void pop ();
};

stack::stack (char c)
{
           tos = 0;
           who = c;
           cout<< "Constructing stack"<< who <<"\n";
}

void stack::push (char ch)
{
     if (tos == SIZE)
     {
             cout<< "Stack "<< who <<" is full" <<"\n";
     }
     stck[tos] = ch;
     tos++;
}

void stack::pop()
{
     if (tos == 0)
     {
             cout<< "Stack " << who << " is empty" << "\n";
     }
     cout<< stck[tos];
     tos--;
}


int main (void)
{
    stack s1('A') , s2('B');
    
    s1.push ('a');
    s1.push ('b');
    s1.push ('c');
    s2.push ('x');
    s2.push ('y');
    s2.push ('z');
    
    int i = 0;
    for ( i=0;i<5;i++ )
    {
        cout<<"Pop s1: "<< s1.pop() << "\n";
    }
    for ( i=0;i<5;i++ )
    {
        cout<<"Pop s2: "<< s2.pop() << "\n";
    }
    
    getchar ();
    return 0;
}
