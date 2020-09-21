#include <iostream>
#include <cstring>
using namespace std;

class library
{
      char book[100];
      char author[100];
      int num;
      public:
             void enter (char *b , char *writer, int n);
             void show ();
};

void library::enter (char *b , char *writer, int n)
{
     strcpy (book , b);
     strcpy (author , writer);
     num = n;
}

void library::show ()
{
     cout<< "The book name is: " << " " << book << '\n' << "The author is:" << '\t' << author  << '\n' << "The number of book at hand is: " << '\t' << num << '\n' << '\n' << '\n';
}

int main (void)
{
    library book1, book2, book3;
    
    book1.enter( "The foundation trilogy" , "Issac Asimov" , 250 );
    book2.enter( "Dune" , "Frank Herbert" , 150 );
    book3.enter( "The Art of War" , "Sun Tzu" , 50 );
    
    book1.show();
    book2.show();
    book3.show();
    
    getchar();
    return 0;
    
}
