#include <iostream>
#include <fstream>
#include <malloc.h>
#include <sstream>
#include <cstdio>
using namespace std;

class Symbol_info   {
    public:
        string symbol, symbol_type;
        Symbol_info *next;

};

class Symbol_table {
    public:
        int scope_id = 1;
        Symbol_info* arr[7];

        bool lookup(int count,string s); 
        void remove(int count,string s);
        void insert(string s1,string s2, int hash_buckets);
        void print(FILE *outfile); 
        int hash_key(string word,unsigned int hashtable_size);
};
