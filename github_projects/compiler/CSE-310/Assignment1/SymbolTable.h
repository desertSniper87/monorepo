/*
 * SymbolTable.h
 *
 *  Created on: Mar 8, 2015
 *      Author: amlansaha
 */

#include<bits/stdc++.h>
#include "LinkedList.h"

using namespace std;

#ifndef SYMBOLTABLE_H_
#define SYMBOLTABLE_H_

class SymbolInfo
{
	string symbol, type;
public:
	SymbolInfo(string s="", string t="")	{
		symbol = s;
		type = t;
	}

	const string& getSymbol() const {
		return symbol;
	}

	void setSymbol(const string& symbol) {
		this->symbol = symbol;
	}

	const string& getType() const {
		return type;
	}

	void setType(const string& type) {
		this->type = type;
	}

	void print()	{
		cout << "(" << symbol << "," <<type <<")";
	}
};

class SymbolTable
{
	LinkedList <SymbolInfo> *table;
	int size;

	int hashFunction(string token)	{
		int ret = 1, l = token.size();
		for ( int i = 0; i < l; i++ )	{
			ret = (ret*token[i]);
			while(ret%size == 0 && ret > size)	ret/= size;
		}
		return ret%size;
	}

public:
	SymbolTable(int size=10)	{	//size contains the number of buckets of the hashtable. By default it is 10.
		table = new LinkedList<SymbolInfo>[size];
		this->size = size;
	}
	void insert(SymbolInfo si)	{
		if ( lookup(si.getSymbol()) )	{
			cout << "The symbol already exists in the table.\n";
			return;
		}
		int hashKey = hashFunction(si.getSymbol());
		table[hashKey].insert(si);
	}
	bool lookup(string token)	{	//if token already exists in the table, it'll return true, otherwise false.
		int hashKey = hashFunction(token);
		if (table[hashKey].searchByToken(token) == NULL )	return false;
		return true;
	}
	void remove(string token)	{
		int hashKey = hashFunction(token);
		table[hashKey].deleteNodeByToken(token);
	}
	void dump()	{
		for (int i = 0; i < size; i++ )	{
			cout << "Index " << i << ": " << endl;
			table[i].print();
			cout << endl;
		}
	}
};

#endif /* SYMBOLTABLE_H_ */
