#ifndef _SYMBOL_TABLE_H
#define _SYMBOL_TABLE_H

#include<iostream>
#include<cstring>
#include<vector>
#include<fstream> 
using namespace std;
int hash(string key);
#define SIZE 100

class SymbolInfo
{
public:
    string name;
    string type;
    vector<int> lineNumbers;
    SymbolInfo(string name,string type)
    {
        this->name = name;
        this->type = type;
    }
    void addLineNumber(int lineNumber) {
    lineNumbers.push_back(lineNumber);
    }
};

class SymbolTable
{
    public:
    vector <SymbolInfo> table[SIZE];
    void insert(SymbolInfo symbol,int lineNumber);
    bool lookup(SymbolInfo symbol);
    void dump();
    int  hash(string key);
};

#endif
