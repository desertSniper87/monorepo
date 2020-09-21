#include <iostream>
#include <string>
#include <fstream>
#include <cstring>
#include <bits/stdc++.h>
#include <stdio.h>
using namespace std;
class symbolInfo
{
	public:
		string name;
		string type;
		int iValue;
		float fValue;
		int aSize;
		int row;
		int col;
		int *iara;
		float *farray;
		int index;
		string code;
		int table=1;
		string address;
		symbolInfo* next;
		symbolInfo()
		{
			row=0;
			col=0;
			iValue=0;
			fValue=0.0;
			aSize=0;
			index=0;
			name = "head";
			type = "head";
			iara=NULL;
			farray=NULL;
			next = NULL;
			code="";
			address=name+to_string(table);
		}
		symbolInfo(string symbolName,string symbolType,int table_no=1,int intValue=0,int floatValue=0.0,int arraySize=0)
		{
			name=symbolName;
			type=symbolType;
			row=0;
			col=0;
			index=0;
			iValue=intValue;
			fValue=floatValue;
			aSize=arraySize;
			table=table_no;
			address=name+to_string(table_no);
			code="";
			if(aSize>0)
			{
				if(type=="int")
					iara=new int[aSize];
				else if(type=="float")
					farray=new float[aSize];
			}
			else
			{
				iara=NULL;
				farray=NULL;
			}

			next=NULL;
		}
		symbolInfo(symbolInfo *p)
		{
			this->name=p->name;
			this->type=p->type;
			this->row=p->row;
			this->col=p->col;
			this->index=p->index;
			this->iValue=p->iValue;
			this->aSize=p->aSize;
			this->iara=p->iara;
			this->farray=p->farray;
			this->next=p->next;
			this->code=p->code;
			this->table=p->table;
			this->address=p->address;
		}

		string getname(){return name;}
		string gettype(){return type;}
};
class scopeTable
{
	symbolInfo* hashtable;
	public:
			int bucket;
			int tableNo = 1;
			scopeTable *parent;
			scopeTable *next;
			scopeTable(int n)
			{
				bucket=n;
				parent=NULL;
				next=NULL;
				hashtable= new symbolInfo[n];
			}
			int hash(string symbolName){return symbolName.length()%bucket;}
			symbolInfo* insert(string symbolName,string symbolType,int table_no=1,int intValue=0,float floatValue=0.0,int arraySize=0)
			{
				if(lookup(symbolName)==NULL)
				{
					symbolInfo* s =new symbolInfo(symbolName,symbolType,table_no,intValue,floatValue,arraySize);
					int location=hash(symbolName);
					int i=1;
					if(hashtable[location].next==NULL)
					{
						s->row=location;
						s->col=i;
						s->iValue=intValue;
						s->fValue=floatValue;
						s->aSize=arraySize;
						hashtable[location].next = s;
					}
					else
					{
						symbolInfo *temp=hashtable[location].next;
						while(temp->next!=NULL)
						{
							temp=temp->next;
							i++;
						}
						i++;
						s->row=location;
						s->col=i;
						s->iValue=intValue;
						s->fValue=floatValue;
						s->aSize=arraySize;
						temp->next=s;
					}
					cout << "Inserted in Scopetable# "<< tableNo << " at position <" << location << ","<<i<<">\n";
					return s;
				}
				else
				{
					cout<<"already Inserted in current Scopetable"<<endl;
					return NULL;
				}
			}
			symbolInfo* lookup(string symbolName)
			{
				int location=hash(symbolName);
				symbolInfo* temp=hashtable[location].next;
				if(temp==NULL)
					return NULL;
				else
				{
					while(temp!=NULL&&temp->getname()!=symbolName)
						temp=temp->next;
					return temp;
				}
			}
			bool del(string symbolName)
			{
				symbolInfo* temp= lookup(symbolName);
				if(temp==NULL)
				{
					cout<<"variable is not declared\n";
					return false;
				}
				else
				{
					//cout<<"Found in Scopetable# "<<tableNo<<" at position <"<<temp->row<<","<<temp->col<<">\n";
					symbolInfo* curr=hashtable[temp->row].next;
					symbolInfo* prev=&hashtable[temp->row];
					while(curr->getname()!=symbolName)
					{
						prev=prev->next;
						curr=curr->next;
					}
					if (curr->next == NULL)
					{
						delete curr;
						prev->next = NULL;
						//curr = NULL;
						//prev->next = curr;
					}
					else
					{
						prev->next=curr->next;
						//curr=NULL;
						delete curr;
					}
					return true;
				}
			}
			void print(FILE *l)
			{
				for(int i=0;i<bucket;i++)
				{
					fprintf(l,"%d --> ",i);
					if(hashtable[i].next!=NULL)
					{
						symbolInfo* temp =hashtable[i].next;
						while(temp)
						{
							fprintf(l, "< %s : %s : %d : %f : %d >\n",temp->gettype().c_str(),temp->getname().c_str(),temp->iValue,temp->fValue,temp->aSize);
							//cout << "<" << temp->getname() << ":" << temp->gettype() << ">";
							if(temp->aSize>0)
							{
								for(int i=0;i<temp->aSize;i++)
								{
									if(temp->iara==NULL)
									{
										printf("iara is null\n");
										break;
									}
									if(temp->type=="int")
										fprintf(l, "%d\t",temp->iara[i]);
									else
										fprintf(l, "%f\t",temp->farray[i]);
								}
							}
							temp=temp->next;
						}
					}
					fprintf(l, "\n");
					//cout << endl;
				}
			}
		
};
class symbolTable
{
public:
	scopeTable* curr;
	scopeTable* sTable;
	symbolTable(int n)
	{
		sTable=new scopeTable(n);
		curr=sTable;
		cout<<"entering scope# "<<curr->tableNo<<'\n';
	}
	void enterScope()
	{
		scopeTable* temp=new scopeTable(curr->bucket);
		temp->parent=curr;
		temp->tableNo=curr->tableNo+1;
		curr->next=temp;
		curr=temp;
		cout<<"entering scope# "<<curr->tableNo<<endl;
	}
	void exitScope()
	{
		if (curr->parent == NULL)
		{
			cout << "exiting scope #" << curr->tableNo << endl;
			cout << "out of all scope\n";
			delete curr;
		}
		else
		{
			scopeTable* prev = curr->parent;
			prev->next = curr->next;
			if(curr->next!=NULL)
				curr->next->parent = curr->parent;
			cout << "exiting scope # " << curr->tableNo << endl;
			delete curr;
			curr = prev;
		}
	}
	void skipScope()
	{
		if (curr->parent == NULL)
		{
			cout << "exiting scope #" << curr->tableNo << endl;
			cout << "out of all scope\n";
		}
		else
		{
			scopeTable* prev = curr->parent;
			prev->next = curr->next;
			if(curr->next!=NULL)
				curr->next->parent = curr->parent;
			cout << "exiting scope #" << curr->tableNo << endl;
			curr = prev;
		}
	}
	scopeTable* lookup_syt(string symbolName)
	{
		scopeTable* temp = curr;
		while (temp)
		{
			symbolInfo* found= temp->lookup(symbolName);
			if ( found!= NULL)
			{
				cout<<"Found in Scopetable# "<<curr->tableNo<<" at position<"<<found->row<<","<<found->col<<">\n"<<endl;
				return curr;
			}
			else
			{
				temp= temp->parent;
			}
		}
		return temp;
	}
	symbolInfo* lookup_syminfo(string symbolName)
	{
		scopeTable* temp = curr;
		while (temp)
		{
			symbolInfo* found= temp->lookup(symbolName);
			if ( found!= NULL)
			{
				cout<<"Found in Scopetable# "<<curr->tableNo<<" at position<"<<found->row<<","<<found->col<<">\n"<<endl;
				return found;
			}
			else
			{
				temp= temp->parent;
			}
		}
		return NULL;
	}
	void remove(string symbolName)
	{
		scopeTable* temp = lookup_syt(symbolName);
		if (temp != NULL)
		{
			symbolInfo* found = temp->lookup(symbolName);
			cout << "Deleted entry at <" << found->row << "," << found->col<<"from current scopeTable>\n";
			curr->del(symbolName);
		}
	}
	void PrintCurrentScopeTable(FILE *f){curr->print(f);}
	void PrintAllScopeTable(FILE *f)
	{
		scopeTable* temp = sTable;
				while (temp)
				{
					//cout << "Scopetable #" << temp->tableNo << endl;
					fprintf(f, "Scopetable #%d \n",curr->tableNo);
					temp->print(f);
					cout << endl;
					temp = temp->next;
				}
	}
};
