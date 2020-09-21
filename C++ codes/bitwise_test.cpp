#include <iostream>
using namespace std;

int hash_key(string word, unsigned int hashtable_size) {
    unsigned int counter, hashAddress =0;
    for (counter =0; word[counter]!='\0'; counter++) {
        cout<< "Counter\t"<< counter<< "word[counter]\t"<< word[counter]<< "hashAddress\t"<< hashAddress;
        hashAddress = word[counter] + (hashAddress << 7) + (hashAddress << 17) - hashAddress;
        cout<< endl;
    }
    return (hashAddress%hashtable_size);
}

int main()
{
   //int i,n ;
   //n = 5;
   int hashtable_size = 7;
   /* Print 5 random numbers from 0 to 49 */
   hash_key("HELLO", hashtable_size);
   //for( i = 0 ; i < n ; i++ )
   //{
        //int x = rand()%32760;
        //cout<< x<< " ";
        //cout<< x<<7<< endl;
        ////cout<< (rand()%32760)/32760<<endl;
   //}

   return(0);
}
