#include <iostream>
#include <cstring>
#include <cstdlib>
using namespace std;

class strtype
{
      char *p;
      int len;
      public:
             strtype(char *ptr);
             ~strtype();
             //void set (char *ptr);
             void show();
             
};

strtype::strtype (char *ptr)
{
                 len = strlen (ptr);
                 cout<< "Allocating memory...\n";
                 p = (char*) malloc (len+1);
                 if (!p)
                 {
                        cout<< "Memory allocation error...\n";
                        exit(1);
                 }
                 strcpy (p,ptr);
                 //*p = '\0';
                 //len = 0;
}

strtype::~strtype ()
{
                  cout<< "Calling destructor function...\n";
                  cout<< "Freeing...\n";
                  free (p);
                  getchar ();
                  getchar ();
}
/*
void strtype::set(char *ptr)
{
                if (strlen(ptr)>=SIZE)
                {
                                      cout<< "String too big...\n";
                }
                strcpy (p,ptr);
                len = strlen (p);
}
*/
void strtype::show()
{
               cout<< "Length of " << p << " :" << len << "\n";
}

int main (void)
{
    strtype str1("I am liking C++"), str2("But I don't know about the upcoming 3 months :P");
    //str1.set ;
    //str2.set ;
    str1.show();
    str2.show();
    
    //getchar ();
    return 0;
}
