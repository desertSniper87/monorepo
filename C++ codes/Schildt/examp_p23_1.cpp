#include <iostream>
using namespace std;

    class myclass 
    {
         int a;
         public:
                void set_a(int num);
                int return_a();
    };
    
    void myclass::set_a (int num)
    {
         a = num;
    }
    
    int myclass::return_a ()
    {
        return a;
    } 
    
int main (void)
{
    
    myclass ob1, ob2;
    
    ob1.set_a(100);
    ob2.set_a(200);
    
    cout<< ob1.return_a() << '\n';
    cout<< ob2.return_a();
    
    getchar();
    return 0;
}
