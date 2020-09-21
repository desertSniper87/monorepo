#include <iostream>
#include <fstream>
using namespace std;

int main ()
{
    ifstream inputData;

    inputData.open ("inputFile.txt");
    if ( !inputData )
    {
        cout<< "Error:Can't open file"<< endl;
    }

    int num = 0;
    int sum = 0;

    inputData>> num;

    while ( !inputData.eof() )
    {
        inputData>> num;
        sum += num;
    }

    inputData.close();

    cout<< "The sum is: "<< sum<< endl;

    return 0;
}
