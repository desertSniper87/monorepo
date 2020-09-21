#include "SymbolTable.h"

void SymbolTable::insert(SymbolInfo symbol,int lineNumber)
{
    if(lookup(symbol)==false)
    {
        int hashed_index = hash(symbol.name);
        table[hashed_index].push_back(symbol);
        symbol.addLineNumber(lineNumber);
    }
    else {
        //cout<<"symbol already exists\n";
        symbol.addLineNumber(lineNumber);
    }
}

bool SymbolTable::lookup(SymbolInfo symbol)
{
    int i;
    int hashed_index = hash(symbol.name);
    if(!table[hashed_index].empty())
    {
        for(i=0;i<table[hashed_index].size();i++)
        {
            if(table[hashed_index][i].name == symbol.name && table[hashed_index][i].type == symbol.type)
                return true;
        }
    }
    return false;
}

void SymbolTable::dump()
{
    for(int i = 0; i<SIZE ;i++)
    {
        for(int j = 0;j<table[i].size();j++)
        {
            cout<<table[i][j].name<<"-->"<<table[i][j].type<<"\n";
        }
    }
}

int SymbolTable::hash(string key)
{

    int i, sum = 0;
    for(i=0;i<key.size();i++)
    {
        sum+=key[i];
    }
    return (sum%SIZE);
}

