#include <cstring>
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <set>

using namespace std;

class SymbolInfo
{
public:
    char *symbol;
    char *symboltype;
    double value;
    string code;
    string labelTrue;
    string labelFalse;
    SymbolInfo *prev;
    SymbolInfo *next;
    SymbolInfo(char *sym,char *symtype)
    {
        int symlen=strlen(sym);
        int symtypelen=strlen(symtype);
        code="";
        if(strcmp(symtype,"NUM")==0)
        {
        	sscanf(sym,"%lf",&value);
        }
        else
        {
        	value=0;
        }
        symbol=new char[symlen+1];
        symboltype=new char[symtypelen+1];
        strcpy(symbol,sym);
        strcpy(symboltype,symtype);
        code="";
        prev=next=0;
    }
};
class SymbolList
{
public:
    SymbolInfo *first;
    SymbolInfo *last;
    SymbolList()
    {
        first=new SymbolInfo("","");
        last=new SymbolInfo("","");
        first->next=last;
        last->prev=first;
    }
    void insert(char *sym,char * symtype)
    {
        SymbolInfo *n=new SymbolInfo(sym,symtype);
        n->next=last;
        n->prev=last->prev;
        last->prev->next=n;
        last->prev=n;
    }
    bool lookup(char *sym)
    {
        SymbolInfo *n=first->next;
        while(n!=last)
        {
            if(strcmp(sym,n->symbol)==0)break;
            n=n->next;
        }
        if(n==last)return false;
        return true;
    }
	SymbolInfo* uplook(char *sym)
    {
        SymbolInfo *n=first->next;
        while(n!=last)
        {
            if(strcmp(sym,n->symbol)==0)break;
            n=n->next;
        }
        if(n==last)return NULL;
        return n;
    }
    void dump(int ind=0)
    {
        SymbolInfo *n=first->next;
        while(n!=last)
        {
			if(n==first->next)
			{
				//fprintf(log,"[%d]-->",ind);
				cout<<"["<<ind<<"]-->\n";
			}
            cout<<"\t"<<(n->symbol)<<" "<<(n->symboltype)<<" "<<(n->value)<<" "<<endl;
            n=n->next;
            //printf("%s %s\n",n->symbol,n->symboltype);
        }
    }
    bool remove(char *sym)
    {
        SymbolInfo *n=first->next;
        while(n!=last)
        {
            if(strcmp(sym,n->symbol)==0)
            {
                n->next->prev=n->prev;
                n->prev->next=n->next;
                delete n;
                return true;
            }
            n=n->next;
        }
        return false;
    }
    ~SymbolList()
    {
        SymbolInfo *n=last;
        while(n!=first)
        {
            n=n->prev;
            delete n->next;
        }
        delete n;
    }
};

class SymbolTable
{
public:
    SymbolList *arr;
    int length;
    SymbolTable(int len=50)
    {
        arr=new SymbolList[len];
        length=len;
    }
    int hash(char *sym)//MAD multiply add and divide
    {
        int a,b,k;
        int len=strlen(sym);
        int i;
        a=0;
        b=0;
        k=0;
        for(i=0;i<len;i++)
        {
            k+=sym[i];
            if((i%2)==0)
            {
                a+=sym[i];
            }
            else
            {
                b+=sym[i];
            }
        }
        return (a*k+b)%length;
    }
    bool lookup(char *sym)
    {
        int key=hash(sym);
        return arr[key].lookup(sym);
    }
	SymbolInfo* uplook(char *sym)
    {
        int key=hash(sym);
        return arr[key].uplook(sym);
    }
    void insert(char *sym,char *symtype)
    {
        if(lookup(sym))
        {
            //cout<<"Symbol already present in the HashTable"<<endl;
            return;
        }
        int key=hash(sym);
        arr[key].insert(sym,symtype);
        if(strcmp(symtype,"NUM")==0||strcmp(symtype,"ID")==0)
        {
        	//sscanf(sym,"%lf",&value);
        	//printf("%s\n",sym);
        }
    }
    void dump()
    {
        int i;
        printf("\nThere goes symbol table:\n");
        for(i=0;i<length;i++)
        {
            //cout<<"Place No.: "<<i<<endl;
            arr[i].dump(i);
        }
    }
    bool remove(char *sym)
    {
        int key=hash(sym);
        return arr[key].remove(sym);
    }
};
