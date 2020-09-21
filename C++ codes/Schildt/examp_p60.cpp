#include <iostream>
using namespace std;

class b
{
    int i;
    //int j;
    public:
        int get_i();
        void set_i(int n);
};

class d: public b
{
    int j;
    public:
        void set_j(int n);
        int mul();
};

void b::set_i(int n)
{
    i = n;
}

int b::get_i()
{
    return i;
}

void d::set_j(int n)
{
    j = n;
}


int d::mul()
{
    return (get_i()*j);
}


int main ()
{
    //b ob1;
    d ob;

    ob.set_i(10);
    ob.set_j(40);
    cout<< ob.mul();

    return 0;
}

