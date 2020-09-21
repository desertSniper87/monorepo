#include <iostream>
using namespace std;

class myclass
{
    int i;
    public:
        void get();
        myclass (int n);
};

myclass::myclass (int n);
{
    i = n;
}

void myclass::get();
{
    return i;
}

int main ()
{
    myclass *p;
    myclass ob1(102);

    p = &ob1;

    cout<< ob1.get()<< "\n";

    cout<< p->get()<< "\n";

    return 0;
}
