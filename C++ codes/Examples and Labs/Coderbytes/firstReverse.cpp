#include <iostream>
#include <cstring>
using namespace std;

string FirstReverse(string str) {
    const char * c = str.c_str();
    int len = strlen (c);
    char reverse[len];

    for ( int i=1;i<=len;i++ ) {
        reverse[i-1] = c[len-i];
    }

    reverse[len] = '\0';
  // code goes here
  return reverse;

}

int main() {

  // keep this function call here
  cout << FirstReverse("Hello");
  return 0;

}
