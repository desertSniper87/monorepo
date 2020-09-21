#include <iostream>
#include <cstring>
#include <cstdlib>
using namespace std;

#define SIZE 255

class strtype
{
      char *p;
      int len;
      public:
             strtype();
             ~strtype();
             void set (char *ptr);
             void show();
             
};

strtype::strtype ()
{
                 cout<< "Allocating memory...\n";
                 p = (char*) malloc (SIZE);
                 if (!p)
                 {
                        cout<< "Memory allocation error...\n";
                        exit(1);
                 }
                 *p = '\0';
                 len = 0;
}

strtype::~strtype ()
{
                  cout<< "Calling destructor function...\n";
                  free (p);
                  getchar ();
                  getchar ();
}

void strtype::set(char *ptr)
{
                if (strlen(ptr)>=SIZE)
                {
                                      cout<< "String too big...\n";
                }
                strcpy (p,ptr);
                len = strlen (p);
}

void strtype::show()
{
               cout<< "Length of " << p << " :" << len << "\n";
}

int main (void)
{
    strtype str1, str2;
    str1.set ("I am liking C++");
    str2.set ("But I don't know about the upcoming 3 months :P");
    str1.show();
    str2.show();
    
    //getchar ();
    return 0;
}
