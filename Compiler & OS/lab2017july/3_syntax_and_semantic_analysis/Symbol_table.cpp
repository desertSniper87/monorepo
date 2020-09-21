#include "Symbol_table.h"

bool scope_flag;

bool Symbol_table::lookup(int count,string s) {
    ofstream outfile;
    outfile.open("output.txt", ios::app);

    int flag=0;
    Symbol_info *cur = arr[count];
    while(cur) {
        if(cur->symbol==s) {
            flag=1;
            outfile <<"Symbol found: " <<cur->symbol<<" in scope: "<< scope_id<< endl<< endl;
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

void Symbol_table::remove(int count,string s) {
    ofstream outfile;
    outfile.open("output.txt", ios::app);

    int flag=0;
    Symbol_info *cur = arr[count];
    while(cur) {
        if(cur->symbol==s)
        {
            flag=1;
            outfile <<"Symbol found:" <<cur->symbol<<" in scope "<< scope_id<< ". Deleting."<< endl<< endl;
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
    }
}


void Symbol_table::insert(string s1,string s2, int hash_buckets)        {
    /// should return a hash key where to insert
    /// cur is the position of has in the table
    printf("Considering insertion of %s, %s\n", s1.c_str(), s2.c_str());
    int key = hash_key(s2, 7);
    if (lookup(key, s2))
        printf("\nThe symbol %s already exists in position %d\n", s2.c_str(), key);
    else {
        int count;
        count= hash_key(s2, hash_buckets);

        Symbol_info *cur = arr[count];

        if(cur!=NULL) {
            int level = 0;
            
            while(cur) {
                if(cur->next!=NULL) {
                    cur=cur->next;
                    level++;
                }
                else {
                    Symbol_info *newOb=new Symbol_info;
                    newOb->symbol=s2;
                    newOb->symbol_type=s1;
                    cur->next=newOb;

                    cur->next->next=NULL;

                    ofstream outfile;
                    outfile.open("log.txt", ios::app);
                    outfile<< s1<< " Inserted in: Table "<< scope_id<< " at position "<< count<< ", "<< level<< endl<<endl; 
                    outfile.close();

                    break;
                }

            }
        }

        else {
            arr[count]=new Symbol_info;

            arr[count]->symbol=s2;
            arr[count]->symbol_type=s1;
            arr[count]->next=NULL;


            ofstream outfile;
            outfile.open("log.txt", ios::app);
            outfile<< s1<< " Inserted in scope table: "<< scope_id<< " at position "<< count<< ", 0"<< endl<< endl;
            outfile.close();

        }
    }

}


void Symbol_table::print(FILE *outfile) {
    //ofstream outfile;
    //outfile.open("log.txt", ios::app);
    fprintf(outfile, "Printing table:\n");

    for(int i=0;i<7;i++)
    {
        Symbol_info *cur = arr[i];
        if(cur) {
            //outfile<< i;
            fprintf(outfile, "%d -->",i);
            while(cur) {
                //fprintf(outfile, i);
                fprintf(outfile, " <%s %s>  ", cur->symbol.c_str(), cur->symbol_type.c_str());
                //outfile<< " -->"<< cur->symbol<< " "<< cur->symbol_type;
                cur=cur->next;
                }
        }

        else {
            //outfile<< i<< "-->"<< endl;
            //fprintf(outfile, "-->\n");
        }

        if(arr[i]) {
            //outfile <<endl;
            fprintf(outfile, "\n");
        }

    }


    //outfile<< endl;
    fprintf(outfile, "\n");
    //outfile.close();
}

/// return the hash key for the string
int Symbol_table::hash_key(string word,unsigned int hashtable_size)
{
    unsigned int counter, hashAddress =0;
    for (counter =0; word[counter]!='\0'; counter++)
    {

        hashAddress = word[counter] + (hashAddress << 7) + (hashAddress << 17) - hashAddress;
    }
    return (hashAddress%hashtable_size);
}
