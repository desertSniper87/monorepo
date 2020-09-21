#include <iostream>
using namespace std;

int SIZE = 10;

struct stack {
  stack();
  void push(char ch);
  char pop();
  char stackData[SIZE];
public:
  int topOfStack;
};

stack::stack()
{
  cout << "Constructing a stack\n";
  topOfStack = 0;
}

void stack::push(char ch)
{
  if(topOfStack==SIZE)
  {
    cout << "Stack is full\n";
    return;
  }
  stackData[topOfStack] = ch;
  topOfStack++;
}

char stack::pop()
{
  if(topOfStack==0) {
    cout << "Stack is empty\n";
    return 0; // return null on empty stack
  }
  topOfStack--;
  return stackData[topOfStack];
}

int main()
{
  cout<< "How long stack"<< endl;

  cin>> SIZE;

  int choice;

  cout<< "What do you want to do?"<< endl
  << "1. Push"<< endl
  << "2. Pop"<< endl;


  cin>> choice;

  if ( choice==1 )
  {
      cout<< "Enter character to push"<< endl;
      cin>> i;
      push (i);
  }

  if ( choice==2 )
  {
      cout<< "POP"<< endl;
      pop ();
  }



  return 0;
}
