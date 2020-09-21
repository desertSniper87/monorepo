#include <iostream>
#include <ctime>
#include <cstdlib>
using namespace std;

int main (){
    cout<< "This is a test"<< endl;

    int a = 30;
    int b = 50;

    a = a+b-(b=a);
    cout<< a<< b;
    return 0;
}
