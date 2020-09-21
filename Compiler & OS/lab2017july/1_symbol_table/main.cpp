#include <iostream>
#include <fstream>
#include <malloc.h>
#include <sstream>
using namespace std;


int hash_buckets;
bool scope_flag;

class Symbol_info   {
    string symbol_name, symbol_type;
    public:
        Symbol_info *next;
        string get_name(){
            return this->symbol_name;
        }
        string get_type(){
            return this->symbol_type;
        }

        void set_symbol(string s){
            this->symbol_name = s;
        }

        void set_type(string t){
            this->symbol_type = t;
        }

};

class Symbol_table  {
/** TODO: Create a destructor function **/
    public:
        int scope_id;
        Symbol_info* arr[7];

        bool lookup(int count,string s) {
            ofstream outfile;
            outfile.open("output.txt", ios::app);

            int flag=0;
            Symbol_info *cur = arr[count];
            while(cur) {
                if(cur->get_name()==s) {
                    flag=1;
                    outfile <<"Symbol found: " <<cur->get_name()<<" in scope: "<< scope_id<< endl<< endl;
                    break;
                }
                cur=cur->next;
            } 
            
            if(flag==1) {
                return true;
            }
            else {
                outfile<< "Symbol not found in scope: "<< scope_id<< "\n"<< endl;
                return false;
            }
            outfile.close();
        }

        void remove(int count, string s) {          // count = position of the string s in the table
            ofstream outfile;
            outfile.open("output.txt", ios::app);

            int flag=0;
            Symbol_info *cur = arr[count];          // As arr is public, we are accessing the symbol_table object
            while(cur) {
                if(cur->get_name()==s) {
                    flag=1;
                    outfile <<"Symbol found:" <<cur->get_name()<<" in scope "<< scope_id<< ". Deleting."<< endl<< endl;
                    arr[count] = NULL;
                    break;
                }
                cur=cur->next;
            }

            if(flag==1) {
                outfile.close();
            }

            else {
                outfile<< "Symbol not found in scope "<< scope_id<< ", Cannot delete\n"<< endl;
                outfile.close();
            }
        }


        void insert(string s1,string s2, int hash_buckets) {
             /*
              *should return a hash key where to insert
              *cur is the position of has in the table
              */
            int count;
            count= hash_key(s1, hash_buckets);

            Symbol_info *cur = arr[count];

                bool search=lookup(count,s1);
            if(cur!=NULL) {

                if(search) {
                    ofstream outfile;
                    outfile.open("output.txt", ios::app);
                    outfile <<"The symbol "<< s1<< " already exists at " <<count <<endl<< endl;
                    outfile.close();
                } else {
                    int level = 0;
                    
                    while(cur) {
                        if(cur->next!=NULL) {
                            cur=cur->next;
                            level++;
                        }
                        else {
                            Symbol_info *newOb=new Symbol_info;
                            newOb->set_symbol(s1);
                            newOb->set_type(s2);
                            cur->next=newOb;

                            cur->next->next=NULL;

                            ofstream outfile;
                            outfile.open("output.txt", ios::app);
                            outfile<< s1<< " Inserted in: Table "<< scope_id<< " at position "<< count<< ", "<< level<< endl<<endl; 
                            outfile.close();

                            break;
                        }

                    }
                }

            }
            else {
                arr[count]=new Symbol_info;

                arr[count]->set_symbol(s1);
                arr[count]->set_type(s2);
                arr[count]->next=NULL;

                ofstream outfile;
                outfile.open("output.txt", ios::app);
                outfile<< s1<< " Inserted in scope table: "<< scope_id<< " at position "<< count<< ", 0"<< endl<< endl;
                outfile.close();

            }

        }


        void print() {
            ofstream outfile;
            outfile.open("output.txt", ios::app);

            for(int i=0;i<7;i++)
            {
                Symbol_info *cur = arr[i];
                if(cur) {
                    outfile<< i;
                    while(cur) {
                        outfile<< " -->"<< " < "<< cur->get_name()<< " "<< cur->get_type()<< " > ";
                        cur=cur->next;
                        }
                }

                else {
                    outfile<< i<< "-->"<< endl;
                }

                if(arr[i]) {
                    outfile <<endl;
                }

            }


            outfile<< endl;
            outfile.close();
        }

        /// return the hash key for the string
        int hash_key(string word, unsigned int hashtable_size) {
            unsigned int counter, hashAddress =0;
            for (counter =0; word[counter]!='\0'; counter++) {
                hashAddress = word[counter] + (hashAddress << 7) + (hashAddress << 17) - hashAddress;
            }
            return (hashAddress%hashtable_size);
        }
};      //End of symboltable class

class ScopeTable {
    Symbol_info* hashtable;

    public:
    int bucket;
    int table_no = 1;
    ScopeTable* parent;
    ScopeTable* child;

    ScopeTable(int n){
        bucket     = n;
        parent     = NULL;
        child      = NULL;
        hashtable  = new Symbol_info[n];
    }

    /*! \brief The scopetable insert function.
     *
     *  Detailed description of the function
     *
     * \return Returns a pointer to a symbol info
     */
    Symbol_info* lookup(string name) {
        
    }
    Symbol_info* insert(string name, string type, int table_no=1) {
        
    }
};

int main() {
    string temp;
    fstream datafile("input.txt", ios::in);
    string input0, input1, input2;

    Symbol_table obtable;
    obtable.scope_id = 1;
    Symbol_table obtable2;
    obtable2.scope_id = 2;

    ofstream outfile;
    outfile.open("output.txt", ios::out);
    outfile.close();

    for (int i = 0; i < 7; i++) {
        obtable.arr[i] = NULL;  
    }

    for (int i = 0; i < 7; i++) {
        obtable2.arr[i] = NULL;  
    }

    if (!datafile) {
        cout << "can't open file" << endl;
        return 0;

    }

    getline(datafile, temp, '\n');
    hash_buckets = stoi(temp);

    stringstream ss;
    string line;

    if (datafile) {
        while (getline(datafile, line)) {
            ofstream outfile;
            outfile.open("output.txt", ios::app);

            outfile<< line<< endl;
            outfile.close();
            ss << line;
            while (getline(ss, input0, ' ')) {

                if (input0 == "P") {
                    ofstream outfile;
                    outfile.open("output.txt", ios::app);
                    outfile<< "Printing Table "<< obtable.scope_id << endl;
                    obtable.print();
                    if (scope_flag) {
                        outfile<< "Printing Table "<< obtable2.scope_id << endl;
                        obtable2.print();
                    }
                    outfile.close();
                }

                else if (input0 == "I") {
                    getline(ss, input1, ' ');
                    getline(ss, input2);

                    if (input1 != "" && input2 != "" ) {
                        if (!scope_flag) {
                            obtable.insert(input1, input2, hash_buckets);
                        }
                        else {
                            obtable2.insert(input1, input2, hash_buckets);
                        }
                        
                        input1 = "";
                        input2 = "";
                    }
                    //else if (!hash_buckets)
                        //cout << "Symbol table full cannot insert " << input1<< endl;

                } else if (input0 == "L") {
                    getline(ss, input1, '\n');
                    input1.pop_back();
                    int key = obtable.hash_key(input1, 7);
                    obtable.lookup(key, input1);
                    if (scope_flag) {
                        int key2 = obtable2.hash_key(input1, 7);
                        obtable2.lookup(key2, input1);
                    }
                    input1 = "";
                }

                else if (input0 == "D"){
                    getline (ss, input1, '\n');
                    input1.pop_back();
                    int key = obtable.hash_key(input1, 7);
                    obtable.remove(key, input1);
                    if (scope_flag) {
                        int key2 = obtable2.hash_key(input1, 7);
                        obtable2.remove(key2, input1);
                    }
                    input1 = "";
                }

                else if (input0 == "S\r"){
                    scope_flag = 1;
                    ofstream outfile;
                    outfile.open("output.txt", ios::app);

                    outfile<< "Changing scope table to table 2" << endl;
                    outfile.close();
                }

                else if (input0 == "E\r"){
                    scope_flag = 0;
                    ofstream outfile;
                    outfile.open("output.txt", ios::app);

                    outfile<< "Reverting scope table to table 1" << endl;
                    outfile.close();
                }

            }       /// End of while

            ss.clear();
        }           /// End of while

        cout<< "Program ran successfully."<< endl;

    }

    /*
     *int choice;
     */
/*
 *    while (true) {
 *        cout << "\n1.insert\n2.lookup\n3.print\n4.Delete\n5.other number to exit\nEnter choice:";
 *        cin >> choice;
 *        switch (choice) {
 *            case 1: {
 *                        cout << "Input1:";
 *                        cin >> input1;
 *                        cout << "Input2:";
 *                        cin >> input2;
 *                        obtable.insert(input1, input2);
 *                        break;
 *                    }
 *
 *            case 2: {
 *                        cout << "Enter:";
 *                        cin >> input1;
 *                        int count2 = obtable.hash_key(input1, 7);
 *                        obtable.lookup(count2, input1);
 *                        break;
 *                    }
 *
 *            case 3: {
 *                        obtable.print();
 *                        break;
 *                    }
 *            case 4: {
 *                        cout << "Enter:";
 *                        cin >> input1;
 *                        int count2 = obtable.hash_key(input1, 7);
 *                        obtable.remove(count2, input1);
 *                        break;
 *                    }
 *            default: {
 *                         return 0;
 *                     }
 *
 *        }
 *    }
 */


}

