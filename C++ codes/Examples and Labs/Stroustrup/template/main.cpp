#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main (int argc, char* argv[])
{
    if (argc!=3)
        cerr<< "Wrong number of arguments"<< endl;

    std::ifstream from (argv[1]);
    if (!argv[1])
        cerr<< "Can't open input file"<< endl;

    std::ofstream to (argv[2]);
    if (!argv[2])
        cerr<< "Can't opern output file"<< endl;

    char ch;
    while (from.get(ch))
        to.put(ch);

    if (!from.eof() || !to)
        cerr<< "Something error happened"<< endl;

    return 0;
}
