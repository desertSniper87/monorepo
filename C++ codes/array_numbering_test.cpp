#include <iostream>
#include <ctime>
#include <cstdlib>
using namespace std;

int main (){
    cout<< "This is a test"<< endl;

    int a[8][8];
    int count = 0;

    for (int i=0;i<8;i++){
        for (int j=0;j<8;j++){
            a[i][j] = count;
            count++;
        }
    }

    for (int i=0;i<8;i++){
        for (int j=0;j<8;j++){
            cout<< a[i][j]<< " ";
        }
        cout<< endl;
    }

    int b[64];
    for (int i=0;i<64;i++){
            b[i] = i;
    }

    for (int i=0;i<8;i++){
        for (int j=0;j<8;j++){
            cout<< b[i*8+j]<< " ";
        }
        cout<< endl;
    }

    return 0;
}
