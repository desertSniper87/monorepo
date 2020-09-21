#include <iostream>
using namespace std;

class pr1
{
    public:
        int printing;
        pr1()
        {
            printing = 0;
        }
        void set_print ( int status )
        {
            printing = status;
        }
        friend int inUse ();
};

class pr2
{
    public:
        int printing;
        pr2()
        {
            printing = 0;
        }
        void set_print ( int status )
        {
            printing = status;
        }
        friend int inUse();
};

int inUse ( pr1 p1, pr2 p2 )
{
    if ( p1.printing||p2.printing )
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

int main ()
{
    pr1 p1;
    pr2 p2;

    if ( !inUse(p1,p2) )
    {
        cout<< "Printer is idle."<< endl;
    }
    else
    {
        cout<< "Printing in prog        {
ress."<< endl;
    }

    p1.set_print (1);

    if ( !inUse(p1,p2) )
    {
        cout<< "Printer is idle."<< endl;
    }
    else
    {
        cout<< "Printing in progress."<< endl;
    }

    p1.set_print (0);

    if ( !inUse(p1,p2) )
    {
        cout<< "Printer is idle."<< endl;
    }
    else
    {
        cout<< "Printing in progress."<< endl;
    };

    p2.set_print (1);

    if ( !inUse(p1,p2) )
    {
        cout<< "Printer is idle."<< endl;
    }
    else        int printing;

    {
        cout<< "Printing in progress."<< endl;
    }

    p2.set_print (0);

    if ( !inUse(p1,p2) )
    {
        cout<< "Printer is idle."<< endl;
    }
    else
    {
        cout<< "Printing in progress."<< endl;
    }

    return 0;

}
