#include <iostream>
using namespace std;

struct node
{
    int data;
    struct node *link;

} *start = NULL;

int item;

int main ()
{
    int choice;

    cout<< endl<< endl <<
    "1. Insert Last"<< endl<<
    "2. Reverse"<< endl<<
    "3. Display"<< endl<<
    "4. Exit"<< endl<< endl;

    cout<< "Enter your choice:" << endl;

    cin>> choice;

    switch (choice)
    {
        case 1 : insert_last();
            break;

        case 2 : _reverse ();
            break;

        case 3: display ();
            break;

        case 4: exit(0);
    }

    return 0;
}

void insert_last()
{
    struct node *ptr;

    cout<< "Enter item"<< endl;
    cin>> item;

    if ( start==NULL )
    {
        start = ( struct node*  ) malloc ( sizeof ( struct *node ) );
        start->data =item;
        start->link = NULL;
    }

    else
    {
        ptr = start;

        while ( ptr->link!=null )
        {
            ptr = ptr->link;
        }
    }
}
