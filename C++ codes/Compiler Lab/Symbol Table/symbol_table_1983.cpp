#include <iostream>
#include <fstream>
#include <stdio.h>
#include <string>
#include <vector>
#include <malloc.h>
#include <sstream>
using namespace std;

int number_of_inputs;

//#include <boost/algorithm/string.hpp>
//using namespace boost;

class SymbolInfo    ///      স্যার যে দুইটা ক্লাস ডিক্লেয়ার করতে বলেছে, তার মধ্যে একটা
{
public:
	string symbol,symbol_type;
	SymbolInfo *next;

};

class SymbolTable   /// আরেকটা ক্লাস
{
public:
	SymbolInfo* arr[30];

	bool lookup(int count,string s)
	{
	    ofstream outfile;
		outfile.open("output.txt", ios::app);

		int flag=0;
		SymbolInfo *cur = arr[count];
		while(cur) {
  		    cout << cur->symbol;
			if(cur->symbol==s)  ///         সিম্বল যদি s হয়, তাইলে ফ্ল্যাগ 1 করবে। ব্রেক করবে
			{
				flag=1;
				cout <<"\nSymbol found: " <<cur->symbol<<endl;  ///পয়েন্টার কোড :p
				outfile <<"\nSymbol found: " <<cur->symbol<<endl;
				break;

			}
			cur=cur->next;

		}
		if(flag==1)
		{
			return true;
		}
		else
		{
		    cout << "Symbol not found"<< endl;
		    outfile<< "\nSymbol not found\n";
			return false;
		}

		outfile.close();
	}


	void insert(string s1,string s2)
	{
		/// should return a hash key where to insert
		int count;

		count=hash_key(s1,20);


		SymbolInfo *cur = arr[count];

		if(cur!=NULL)
		{

			//bool search=lookup(count,s1);

			/*if(search)/// the symbol is already in the table
			{
				cout <<"(The symbol is already exists)"<<count<<endl;
			}
			else
			{*/
				while(cur)
				{
					if(cur->next!=NULL)
					{
						cur=cur->next;
					}
					else
					{
						SymbolInfo *newOb=new SymbolInfo;
						newOb->symbol=s1;
						newOb->symbol_type=s2;
						cur->next=newOb;

						cur->next->next=NULL;
						break;
					}

				}
			//}

		}
		else
		{

			arr[count]=new SymbolInfo;

			arr[count]->symbol=s1;
			arr[count]->symbol_type=s2;
			arr[count]->next=NULL;


		}

	}

	void dump()
	{

		ofstream outfile;
		outfile.open("output.txt", ios::out);

		for(int i=0;i<30;i++)
		{
			SymbolInfo *cur = arr[i];
			if(cur)
			{
				outfile <<"In slot:"<<i<<endl;
			}

			while(cur)
			{

				//outfile << "("<<cur->symbol<<","<<cur->symbol_type<<")->";
				outfile <<cur->symbol<<","<<cur->symbol_type<< endl;

				/*string st=cur->symbol+","+cur->symbol_type;
				cout << st<<endl;
				cout <<"-->"<<endl;*/
				//cout << "("<<cur->symbol<<","<<cur->symbol_type<<")->";


				cur=cur->next;
				if(!cur)
				{
				   //outfile <<"NULL";
				   //cout << "NULL";
				}

			}
			if(arr[i])
			{
				outfile <<endl;
				//cout << endl<<endl;
			}

		}


		outfile.close();
	}

	/// return the hash key for the string
	int hash_key(string word,unsigned int hashtable_size)
	{
		 unsigned int counter, hashAddress =0;
		 for (counter =0; word[counter]!='\0'; counter++)
		 {

			 hashAddress = word[counter] + (hashAddress << 6) + (hashAddress << 16) - hashAddress;
		 }
		 return (hashAddress%hashtable_size);
	}




};

int main()
{

//  int choice;
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
//    int number_of_inputs;
    string temp;
	fstream datafile("input.txt", ios::in);
	string input0,input1,input2;
	SymbolTable obtable;

	for(int i=0;i<30;i++)
	{
		obtable.arr[i]=NULL;    ///     টেবল পপুলেট করছে
	}

    if(!datafile)
	{
		cout << "can't open file"<<endl;
		return 0;

	}

	getline(datafile, temp, '\n');
	number_of_inputs = stoi(temp);

//	while(datafile)
//	{
//	    getline(datafile, input0, ' ');
//	    if (input0=="P") {
//            obtable.dump();
//            cout<< "Printing"<< endl;
//            //break;
//	    }
//
//	    else if (input0=="I") {
//            getline(datafile, input1, ' ');     ///ফাইল থেকে ইনপুট নিচ্ছে।  গেটলাইন কাজ করে, getline ( স্ট্রিম, ভারিয়েবল, ডেলিমিটার ) । স্ট্রিম থেকে ভারিয়েবলএ ইনপুট নিবে,ব   ডেলিমিটার পেলে ইনপুট নেয়া বন্ধ করবে।
//            getline(datafile, input2, '\n');    /// দুইটা ইনপুট নিল। প্রথম ইনপুট লাইনের কাজ শেষ
//
//
//            if(input1!="" && input2!="")
//            {
//                cout<< "Inserting"<< endl;
//                obtable.insert(input1,input2);
//                input1="";
//                input2="";
//            }
//	    }
//
//	    else if (input0=="L") {
//            getline (datafile, input1, '\n');
//
//            int count2=obtable.hash_key(input1,20);
//            obtable.lookup(count2,input1);
//	    }
//
//
//        cout<< number_of_inputs<< endl;
//		number_of_inputs--;
//	}

    stringstream ss;
    string line;

	if(datafile)
	{
	    while (getline(datafile, line)){
//                cout<< "getting line"<< endl;
//                cout<< line<< endl;
                ss<< line;
                //cout<< ss.str()<< endl;
                while (getline(ss, input0, ' ')) {
                        //cout<< input0;
                        //cout<< "In while loop 2"<< endl;
                        if (input0=="P") {
                            obtable.dump();
                            //cout<< "Printing"<< endl;
                        //break;
                        }

                        else if (input0=="I") {
                            getline(ss, input1, ' ');     ///ফাইল থেকে ইনপুট নিচ্ছে।  গেটলাইন কাজ করে, getline ( স্ট্রিম, ভারিয়েবল, ডেলিমিটার ) । স্ট্রিম থেকে ভারিয়েবলএ ইনপুট নিবে,ব   ডেলিমিটার পেলে ইনপুট নেয়া বন্ধ করবে।
                            getline(ss, input2);    /// দুইটা ইনপুট নিল। প্রথম ইনপুট লাইনের কাজ শেষ


                            if(input1!="" && input2!="" && number_of_inputs)
                            {
                                //cout<< "Inserting"<< endl;
                                obtable.insert(input1,input2);
                                input1="";
                                input2="";
                                number_of_inputs--;
                            }

                            else if (!number_of_inputs)
                                cout<< "Symbol table full"<< endl;
                        }

                        else if (input0=="L") {
                            getline (ss, input1, '\n');

                            int count2=obtable.hash_key(input1,20);
                            obtable.lookup(count2,input1);

                            input1="";
                        }
                }       /// End of while

                ss.clear();
            }           /// End of while

            //cout<< number_of_inputs<< endl;
    }


//	while (datafile){
//        std::string delimiters("'\n', ");
//        std::vector<std::string> parts;
//        boost::split(parts, datafile, boost::is_any_of(delimiters));
//	}

	int choice;
	while(true)
	{
        cout << "1.insert\n2.lookup\n3.dump\n4.other number to exit\nEnter choice:";
        cin >> choice;
        switch(choice)
        {
            case 1:
            {
                cout << "Input1:";
                cin >> input1;
                cout << "Input2:";
                cin>>input2;
                obtable.insert(input1,input2);
                break;
            }

            case 2:
            {
                cout << "Enter:";
                cin >> input1;
                int count2=obtable.hash_key(input1,20);
                obtable.lookup(count2,input1);
                break;
            }

            case 3:
            {
                obtable.dump();
                break;
            }
            default:
            {
                return 0;
            }

        }
	}



}
