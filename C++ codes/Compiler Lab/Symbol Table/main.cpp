#include <iostream>
#include <cstring>
#include <cstdio>
#include <sstream>
#include <vector>
#include <algorithm>

using namespace std;

const int LENGTH=20;

class SymbolInfo
{
	char *symbolName;
	char *symbolType;

public:

	SymbolInfo *next;
	SymbolInfo *prev;

	SymbolInfo()
	{
		symbolName=NULL;
		symbolType=NULL;
	}

	void setSymbol(char *name)
	{
		symbolName=new char[strlen(name)+1];
		strcpy(symbolName,name);
	}

	void setType(char *type)
	{
		symbolType=new char[strlen(type)+1];
		strcpy(symbolType,type);
	}

	char *getSymbol()
	{
		return symbolName;
	}

	char *getType()
	{
		return symbolType;
	}
};

class SymbolTable
{
	SymbolInfo *headColumn[LENGTH];
	SymbolInfo *tailColumn[LENGTH];

public:

	SymbolTable()
	{
		for(int i=0;i<LENGTH;i++)
		{
			headColumn[i]=new SymbolInfo;
			tailColumn[i]=new SymbolInfo;
			headColumn[i]->next=tailColumn[i];              /****** Linking Between Head And Tail ******/
			tailColumn[i]->prev=headColumn[i];              /****** Linking Between Head And Tail ******/
		}
	}

    /****** HashFunction Implementation using ASCII code Multiplying By Increasing sequence of Integer ******/
	int HashFunction(char *s)
	{
		unsigned int symbolIndex=0;
		unsigned int random;
		for(int i=0,random=1;i<strlen(s);i++,random*=2)
		{
			random=(unsigned int) s[i]*random;
		    symbolIndex+=random;
		}
		symbolIndex=symbolIndex%LENGTH;
		return symbolIndex;
	}

	/****** Inserting A Symbol In SymbolTable if that doesn't Exist In List ******/
	void insert(char *symbolname, char *symboltype)
	{
		int key=HashFunction(symbolname);

		//checking existence
		bool b=lookup(symbolname);
		if(b)
		{
			cout<<"'"<<symbolname<<"' is Already exist,Try another Symbol\n";
			return;
		}

		/****** Adding Node Between The Last ans Last Before Element ******/
		SymbolInfo *newSymbol= new SymbolInfo;
		newSymbol->next=tailColumn[key];
		newSymbol->prev=tailColumn[key]->prev;
		tailColumn[key]->prev->next=newSymbol;
		tailColumn[key]->prev=newSymbol;

		newSymbol->setSymbol(symbolname);
		newSymbol->setType(symboltype);
	}

	/****** Look Up For a Symbol's Existance ******/
	bool lookup(char *symbolname)
	{
		int key=HashFunction(symbolname);  /****** At first get the Hash Value and go to that row to find that Symbol ******/

		SymbolInfo *saveHead;              /****** Save The Head of SymbolName Colum's Row Address ******/
		saveHead=headColumn[key];          /****** Head Saved :D ******/

		while(headColumn[key]->next!=tailColumn[key])                       /****** Go Ahead Head to the End Searching ******/
		{
			if(strcmp(headColumn[key]->next->getSymbol(),symbolname)==0)
			{
				headColumn[key]=saveHead;                                   /****** Return Head his Own Address ******/
				return true;
			}
			headColumn[key]=headColumn[key]->next;                          /****** Go Ahead by Jumping Next ******/
		}
		headColumn[key]=saveHead;                                           /****** Return Head his Own Address ******/
		return false;
	}

	/****** Dumping The Symbol Table in The Console ******/
	void dump()
	{
		SymbolInfo *saveHead;
		bool tableEntry=false;

		cout<<endl;
		for(int i=0;i<LENGTH;i++)
		{
			saveHead=headColumn[i];
			while(headColumn[i]->next!=tailColumn[i])
			{
				cout<<i<<"\t\t"<<headColumn[i]->next->getSymbol()<<"\t\t,\t\t";
				cout<<headColumn[i]->next->getType()<<"\n";
				headColumn[i]=headColumn[i]->next;
				tableEntry=true;
			}
			headColumn[i]=saveHead;
		}
		if(tableEntry==false)cout<<"No Data to Show\n\n";
	}
};

//char *convert(const std::vector<string> & s)
//{
//   char *pc = new char[s.size()+1];
//   std::strcpy(pc, s.c_str());
//   return pc;
//}

int main(int argc, char* argv[])
{
//	int choice;
//	int i;
//	int j;
//	int number_of_inputs;
////	vector<basic_string> inputSymbol(40);                   /********* Taking Input Symbol From Console *********/
////	vector<basic_string> inputSymbolType(40);               /****** Taking Input Symbol Type From Console ******/
//    char inputSymbol[40];
//    char inputSymbolType[40];
//	char totalInput[40];
//
//	SymbolTable symbolTable;
//
//    //int input_size;
//    //cin>> input_size;
//
//	string input_line;
//
//	cin >> number_of_inputs;
//	for (int i=0;i<number_of_inputs;i++){
//		getline(cin,input_line);
//
//		istringstream iss(input_line);
//    	string word;
//
//    	while(iss >> word) {
//	    	if (word=="I"){
//	    		iss >> word;
////	    		std::vector<char> writable(word.begin(), word.end());
////				writable.push_back('\0');
////				char c[40];
////				strcpy(c, writable);
////	    		inputSymbol[i]=(char)&writable[0];
//                //inputSymbol[i] = strdup(word.c_str());
//                std::transform(word.begin(), word.end(), std::back_inserter(word), convert);
//                cout<< word;
//                //inputSymbol.insert(word);
//
//	    	}
//
//	    	else if (word=="P") {
//
//	    	}
//
//	    	else if (word=="L") {
//
//	    	}
//
//	    	else if (word=="D") {
//
//	    	}
//
//
//	    }// end of while
//	}// End of for
//
//

    int i;
    int j;
    int number_of_inputs;
    char inputSymbol[40];
    char inputSymbolType[40];
    char character_input_stream;

    SymbolTable symbolTable;

    cin>> number_of_inputs;

    for ( i=0; i<number_of_inputs; i++ ) {
        while (cin>> character_input_stream) {
            if (character_input_stream == '\n')
                break;
            else    {
                j++;
                inputSymbol[j]=character_input_stream;
                if (character_input_stream!=' ') {
                    j=0;
                    break;
                }
            }

            //symbolTable.insert
        }
    }
}








	//while(1)
	//{
	    /*
        cout<<"\n1. INSERT\n2. LOOKUP\n3. DUMP\nInsert You Choice:\t";
		cin>>choice;

		/****** If Total Input String contain space=' ' or spaces='    ' following to comma=',' Input after SymbolName ******/
		/*if(choice==1)
		{
		    cout<<"\nEnter Your Symbol name, Symbol type:\t";
			cin>>inputSymbol;               /****** Taking Input Symbol With ',' operator ******/
			/*cin>>inputSymbolType;           /****** Taking Input Symbol Type Giving Space ' ' previous 'name,' input format ******/

			/*j=strlen(inputSymbol);          /****** Just Finding The length Of Input Symbol Name ******/
			/*inputSymbol[j-1]='\0';          /****** As Input Symbol Name contains a ',' in last position, just erase that :D ******/

			/*symbolTable.insert(inputSymbol,inputSymbolType); /****** Inserting Input in SymbolTable ******/
		/*}*/

		/****** If Total Input String Doesn't contain Any space=' ' following to comma=',' Input after SymbolName ******/
		/*
		if(choice==1)
		{
		    cout<<"\nEnter Your Symbol name,Symbol type:\t";
			cin>>totalInput;
			for(i=0;totalInput[i]!=',';i++)
			{
				inputSymbol[i]=totalInput[i];
			}
			inputSymbol[i]=NULL;

			int len=strlen(totalInput);
			for(++i,j=0;i<=len;i++,j++)
			{
				inputSymbolType[j]=totalInput[i];
			}

			symbolTable.insert(inputSymbol,inputSymbolType); /****** Inserting Input in SymbolTable ******/
		/*}
		/*

		else if(choice==2)
		{
		    cout<<"\nEnter Your Symbol name:\t";
			cin>>inputSymbol;
			bool b=symbolTable.lookup(inputSymbol);
			if(b)cout<<"\nExist.\n";
			else cout<<"\nNot Exist.\n";
		}

		else if(choice==3)
		{
		    cout<<"\nIndex Value\tSymbol Name\t\t\tSymbol Type\n";
			symbolTable.dump();
		}

		else
		{
			cout<<"Wrong Menu. Try 1 / 2 / 3 ...\n";
		}
	}
	return 0;
	*//**


	if(choice==1)
		{
		    cout<<"\nEnter Your Symbol name,Symbol type:\t";
			cin>>totalInput;
			for(i=0;totalInput[i]!=',';i++)
			{
				inputSymbol[i]=totalInput[i];
			}
			inputSymbol[i]=NULL;

			int len=strlen(totalInput);
			for(++i,j=0;i<=len;i++,j++)
			{
				inputSymbolType[j]=totalInput[i];
			}

			symbolTable.insert(inputSymbol,inputSymbolType); /****** Inserting Input in SymbolTable ******/
		///}
/**

		else if(choice==2)
		{
		    cout<<"\nEnter Your Symbol name:\t";
			cin>>inputSymbol;
			bool b=symbolTable.lookup(inputSymbol);
			if(b)cout<<"\nExist.\n";
			else cout<<"\nNot Exist.\n";
    }
}
*/

