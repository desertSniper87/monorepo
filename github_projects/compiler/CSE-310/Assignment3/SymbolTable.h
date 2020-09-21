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
public:
	string symbol, type;
	double value;
	string code;
    string labelTrue;
    string labelFalse;
    
	SymbolInfo(string s="", string t="", double v=0)	{
		symbol = s;
		type = t;
		value = v;
	}

	const string& getSymbol() const {
		return symbol;
	}

	const string& getType() const {
		return type;
	}
	
	const string& getCode() const	{
		return code;
	}
	
	const double getValue() const	{
		return value;
	}
	
	const string& getLabelTrue() const	{
		return labelTrue;
	}
	
	const string& getLabelFalse() const	{
		return labelFalse;
	}
	
	void setSymbol(const string& symbol)	{
		this->symbol = symbol;
	}

	void setType(const string& type) {
		this->type = type;
	}

	void setCode(const string& code) {
		this->code = code;
	}
	
	void setLabelTrue(const double labelTrue) {
		this->labelTrue = labelTrue;
	}
	
	void setLabelFalse(const string& labelFalse) {
		this->labelFalse = labelFalse;
	}
	
	void print(FILE *fout=stdout)	{
		fprintf (fout, "< %s, %s, %lf >", type.c_str(), symbol.c_str(), value );
//		cout << "< " << type << ", " <<symbol <<")";
	}
};

class SymbolTable
{
	LinkedList <SymbolInfo> *table;
	int size;

	int hashFunction(string token)	{
		int ret = 1, l = token.size();
		for ( int i = 0; i < l; i++ )	{
			ret = (i*ret+(token[i]<<(i%25)))%size;
//			while(ret%size == 0 && ret > size)	ret/= size;
		}
		return ret%size;
	}

public:
	SymbolTable(int size=263)	{	//size contains the number of buckets of the hashtable. By default it is 200.
		table = new LinkedList<SymbolInfo>[size];
		this->size = size;
	}
	
	bool lookup(string token)	{	//if token already exists in the table, it'll return true, otherwise false.
		int hashKey = hashFunction(token);
		if (table[hashKey].searchByToken(token) == NULL )	return false;
		return true;
	}
	
	SymbolInfo* uplook (string token)	{//if token already exists in the table, it'll return pointer of the node, otherwise NULL.
		int hashKey = hashFunction(token);
		Node<SymbolInfo> * tempNode = table[hashKey].searchByToken(token);
		if ( tempNode == NULL )	return NULL;
		else	return tempNode->getValPointer();
	}
	
	bool insert(SymbolInfo si)	{
		if ( lookup(si.getSymbol()) )	{
			//The symbol already exists in the table.
			return false;
		}
		int hashKey = hashFunction(si.getSymbol());
		table[hashKey].insert(si);
		return true;
	}
	
	bool insert(string type, string symbol)
	{
		double v = 0;
		if ( type == "NUM" )	{
			v = atoi(symbol.c_str());
//			cout << symbol << " " << v << endl;
		}
		return insert(SymbolInfo(symbol,type,v));
	}
	
	void remove(string token)	{
		int hashKey = hashFunction(token);
		table[hashKey].deleteNodeByToken(token);
	}
	
	void dump(FILE *fout=stdout)	{
		for (int i = 0; i < size; i++ )	{
			if(table[i].isEmpty())	continue;
			fprintf(fout, "[%d]--> ", i);
			table[i].print(fout);
			fprintf(fout, "\n");
		}
	}
};

#endif /* SYMBOLTABLE_H_ */
