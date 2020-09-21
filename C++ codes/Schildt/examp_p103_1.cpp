#include <iostream>
#include <cstring>
#include <conio.h>
using namespace std;

class samp
{
      char s[100];
      public:
             void show()
             {
                  cout<< s<< "\n";
             }
             void set (char *str)
             {
                  strcpy (s,str);
             }
};

samp input_function ()
{
     samp str;
     char s[100];
     
     cout<< "Please enter a string: \n";
     cin>> s;
     
     str.set (s);
     
     return str;
     
}

int main ()
{
    samp obj;
    
    obj = input_function();
    obj.show();
    
    getch ();
    return 0;
}

