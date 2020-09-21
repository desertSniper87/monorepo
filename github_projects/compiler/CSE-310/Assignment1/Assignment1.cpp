/*
prob: Assignment1
id: amlansaha
lang: C++
date: Mar 7, 2015
algo: 
*/
#include <bits/stdc++.h>
//#include "LinkedList.h"
#include "SymbolTable.h"

using namespace std;

typedef long long LL;
typedef unsigned long long LLU;
typedef vector<int> VI;
typedef vector<long long> VLL;
typedef map<int, int> MAPII;
typedef map<string,int> MAPSI;
typedef pair<int, int> PII;

#define FOR(i,a,b) for(i=a;i<=b;i++)
#define ROF(i,a,b) for(i=a;i>=b;i--)
#define FR(i,n)	for(i=0;i<n;i++)
#define RF(i,n) for(i=n;i>0;i--)
#define CLR(a) memset ( a, 0, sizeof ( a ) )
#define RESET(a) memset ( a, -1, sizeof ( a ) )
#define PB(a)	push_back ( a )


int main ()
{
	ifstream fin("Assignment1.in");
//	ofstream fout("Assignment1.out");

    int i, j, k, l, temp, opt, t, n, m, ans, caseno = 0;
    SymbolInfo si;
    SymbolTable st;
    string str1, str2;
    
    //at first this program will try to take input from file.
    while ( fin >> str1 >> str2)	{
    	si.setSymbol(str1);
    	si.setType(str2);
    	st.insert(si);
    	si.print();
    }

    //then from console
    while (1)	{
    	cout << "~~~****MENU****~~~\n";
    	cout << "1. Insert\n";
    	cout << "2. Lookup\n";
    	cout << "3. Delete\n";
    	cout << "4. Dump\n";
    	cin >> opt;
    	switch(opt)	{
    		case 1:
    			cin >> str1 >> str2;
    			si.setSymbol(str1);
    			si.setType(str2);
    			st.insert(si);
    			break;
    		case 2:
    			cin >> str1;
    			if (st.lookup(str1))	cout << "Symbol is in the table.\n";
    			else	cout << "Symbol is not in the table.\n";
    			break;
    		case 3:
    			cin >> str1;
    			st.remove(str1);
    			break;
    		case 4:
    			st.dump();
    			break;
    		default:
    			break;
    	}
    }
    return 0;
}
