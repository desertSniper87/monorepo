#include <iostream>
#include <fstream>
#include <malloc.h>
#include <sstream>
#include <cstdio>
#include <string>
#include <string.h>

using namespace std;

class Symbol_info   {
    public:
        string symbol, symbol_type;
        Symbol_info *next;
        string code;

        Symbol_info(){
            this->symbol="";
            this->symbol_type="";
            code="";
        }

        Symbol_info(char *symbol, char *type){
            this->symbol=string(symbol);
            this->symbol_type=string(type);
            this->code="";
        }

        Symbol_info(string symbol, string type){
            this->symbol=symbol;
            this->symbol_type=type;
            this->code="";
        }

        Symbol_info(const Symbol_info *sym){
         	this->symbol=sym->symbol;
         	this->symbol_type=sym->symbol_type;
         	this->code=sym->code;
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
            string c = this->code;

            printf("~~~~~~~~~~~~~~~~~Printing symbol information~~~~~~~~~~~~~~~~\n");
            printf("************************************************************\n");
            printf("\nSymbol name: %s, Symbol type: %s\nCode: \n%s\n", s.c_str(), t.c_str(), c.c_str());
            printf("************************************************************\n");
        }
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
