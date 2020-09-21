#include <iostream>
#include <fstream>
#include <malloc.h>
#include <sstream>
#include <cstdio>

#ifndef SYMBOLINFO_H_INCLUDED
#define SYMBOLINFO_H_INCLUDED

using namespace std;

class Symbol_info   {
    public:
        string symbol, symbol_type;
        Symbol_info *next;
        string code;

        Symbol_info(){
            symbol="";
            symbol_type="";
            code="";
        }

        Symbol_info(char *symbol, char *type){
            this->symbol=string(symbol);
            this->symbol_type=string(type);
            code="";
        }

        Symbol_info(string symbol, string type){
            symbol=symbol;
            symbol_type=type;
            code="";
        }

        Symbol_info(Symbol_info *sym){
         	symbol=sym->symbol;
         	symbol_type=sym->symbol_type;
         	code=sym->code;
        }

        string getSymbol(){
            return symbol;
        }

        string getType(){
            return symbol_type;
        }
        
        void setSymbol(char *symbol_name){
            this->symbol = string(symbol_name);
        }

        void setSymbol(string symbol_name){
            this->symbol = symbol_name;
        }

        void setType(char *type){
            this->symbol_type= string(type);
        }

        void setType(string type){
            this->symbol_type= type;
        }

        void print_info(){
            string s = this->symbol;
            string t = this->symbol_type;

            printf("Symbol name: %s, Symbol type: %s\n", s.c_str(), t.c_str());
        }
};

#endif

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
