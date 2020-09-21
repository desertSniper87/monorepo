#include <iostream>
using namespace std;

void read_input( int i );
void write_output( int i );

struct date
{
    char day[100];
    char month[100];
    char year[100];
};

struct account
{
    char name[100];
    char city[100];
    struct date creDate;
    float current_balance;
    float payment;

}customer[100];

int main ()
{
    cout<< "CUSTOMER BILLING SYSTEM"<< endl<< endl;
    cout<< "Please enter the number of customers"<< endl;

    int numCustomer;
    cin>> numCustomer;

    for ( int i=0;i<numCustomer;i++ )
    {
        read_input(i);
    }

    for ( ;; )
    {
        cout<<"Please enter the serial no. of the customer: "<< endl;
        int i;
        cin>> i;
        write_output( i-1 );
    }

    return 0;
}

void read_input ( int i )
{
    cout<< "CUSTOMER "<< i+1<< endl;

    cout<< "Please enter name:"<< endl;
    cin>> customer[i].name;

    cout<< "Please enter city:"<< endl;
    cin>> customer[i].name;

    cout<< "Please enter date of account creation(dd mm yy)"<< endl;
    cin>> customer[i].creDate.day>> customer[i].creDate.month>> customer[i].creDate.year;

    cout<< "Please enter current balance"<< endl;
    cin>> customer[i].current_balance;

    cout<< "Please enter payment"<< endl;
    cin>> customer[i].payment;

    cout<< endl<< endl;
}

void write_output (int i)
{
    cout<< "CUSTOMER "<< i+1<< endl;

    cout<< "NAME:"<< customer[i].name<< endl;
    cout<< "CITY :"<< customer[i].city<< endl;
    cout<< "DATE OF CREATION: "<<customer[i].creDate.day<< "/"<< customer[i].creDate.month<< "/"<< customer[i].creDate.year<< endl;
    cout<< "LAST PAYMENT: "<<  customer[i].payment<< endl;
    cout<< "CURRENT BALANCE: "<< customer[i].current_balance - customer[i].payment;

    cout<< endl<< endl;
}
