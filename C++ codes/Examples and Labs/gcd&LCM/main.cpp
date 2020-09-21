#include <iostream>
using namespace std;

int gcdByEuclid ( int i , int j );
int gcdByPrimeFactorization ( int i, int j );
int gcdByShortcut ( int i , int j );
int lcmByEuclid ( int i, int j );

bool primeDetector ( int x );

int main ()
{
    int i = 0;
    int j = 0;

    for ( ;; )
    {
        cout<< "Please enter first number: ";
        cin>> i;
        cout<< "Please enter second number: ";
        cin>> j;

//        cout<< "GCD by Euclid is "<< gcdByEuclid ( i , j )<< endl;
    //    cout<< "GCD by Prime factorization is "<< gcdByPrimeFactorization ( i , j )<< endl;
//        cout<< "GCD by shortcut is "<< gcdByShortcut( i , j )<<endl;
        cout<< "LCM by Euclid is "<< lcmByEuclid ( i , j )<< endl;
    //    cout<< "LCM by Prime Factorization is"<< lcmByPrimeFactorization ( i , j );
    }

    return 0;
}

int gcdByEuclid ( int i , int j )
{
    /**
    Let i be greater than j
    **/

    int temp;

    if ( i<j )
    {
        temp = i;
        i = j;
        j = temp;
    }
    else if ( !(i%j) )
    {
         return j;
    }

    while ( i%j )
    {
        temp = j;
        j = i%j;

        i = temp;

        if  ( !(i%j) )
        {
            return j;
        }
    }

    return temp;
}

int gcdByPrimeFactorization ( int i, int j )
{
    int x = 0;
    int y = 0;
    int maxPrime;
    int tempPrime; /** temp Prime is smaller than maxPrime **/
    int result = 1;

    int n = 0;
    int temp = 0;

    int primeArray1[100];
    int primeArray2[100];
    int finalPrimeArray[100];

    bool primeFlag = 0;

    /**
    If primeFlag is 0 we say that the prime factors of the first int is greater than the
    second int

    else if primeFlag is 1 we say that prime factors of the second int is greater

        if primeFlag = 1 we use the first primeArray
        else we use the second

    its that simple :)

    **/

    for ( n=1;n<=i;n++ )
    {
        if (!(i%n))
        {
            if ( primeDetector( n ) )
            {
                primeArray1[x] = n;
                maxPrime++;
            }
        }
    }

    tempPrime = maxPrime;
    maxPrime = 0;

    for ( n=1;n<=j;n++ )
    {
        if ( primeDetector( n ) )
        {
                primeArray1[x] = n;
                maxPrime++;
        }
    }

    if ( n>=maxPrime )
    {
        maxPrime = n;
        primeFlag = 1;
    }

    else
    {
        temp = tempPrime;
        tempPrime = maxPrime;
        maxPrime = temp;

    }

    /**
    primeFlag = 0 then size[ primeArray1 ] > size[ primeArray2 ]
    else size[primeArray2]>size[primeArray2]
    **/

    y = 0;

    if ( primeFlag == 1 )
    {
        for ( n=0;n<=temp;n++ )
        {
           for ( x=0;x<maxPrime;x++ )
           {
               if ( primeArray1[n] == primeArray2[x] )
               {
                   finalPrimeArray[n] = primeArray2[y];
               }
           }
        }
    }

    else if ( primeFlag == 0 )
    {
        for ( n=0;n<=tempPrime;n++ )
        {
           for ( x=0;x<maxPrime;x++ )
           {
               if ( primeArray2[n] == primeArray1[x] )
               {
                   finalPrimeArray[n] = primeArray1[y];
               }
           }
        }
    }

    for ( i=0;i<n;i++ )
    {
        result *= finalPrimeArray[i];
    }

    return result;
}

bool primeDetector ( int x )
{
    bool flag = true;
    for ( int i=1;i<=x;i++ )
    {
        if ( x%i )
        {
            flag = false;
            break;
        }
    }

    return flag;
}

int gcdByShortcut ( int i , int j )
{
    int temp;

    if ( i>j )
    {
        temp = i;
    }

    else if ( i<j )
    {
        temp = j;
    }

    for ( ;temp>0;temp-- )
    {
        if ( (!(i%temp))&&(!(j%temp)) )
            return temp;
    }
}

int lcmByEuclid ( int i , int j )
{
    int gcd = gcdByShortcut ( i , j );
    return ( ( i*j )/gcd );
}
