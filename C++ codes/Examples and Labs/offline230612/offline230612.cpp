#include <iostream>
#include <cstdlib>
#include <conio.h>
using namespace std;

int SIZE;

class Bitset
{
	//int *bitarray;  //Holds the bit values
	int LSB;
	int MSB;
public:
    int *bitarray;
	//*
	Bitset ()
	{
		*bitarray = 0;
	}
	//*/
	Bitset (int num_bit)
	{
	    int i;
		bitarray = (int *) malloc (num_bit*4);
		//*
		SIZE = num_bit;
		for ( i=0;i<num_bit;i++ )
		{
		    *( bitarray + i ) = 0;
		}
		//*/
	}
	int cardinality();
	void clear ();
	void clear(int index);
	void set(int index);
	void set(int startIndex, int endIndex);
	Bitset get(int startIndex,int endIndex);
	Bitset andd(Bitset bitset);
	Bitset orr(Bitset bitset);
	//Bitset xorr(Bitset bitset);
	//boolean intersects(Bitset bitset);
	void andNot(Bitset bitset);
	void bitReplaceLeft(Bitset bitset,int p, int n);
	void bitReplaceRight(Bitset bitset,int p, int n);
	void show();
};
/*
int Bitset::cardinality()
{

}
*/
//*
void Bitset::clear ()
{
	int i;
	for ( i=0;i<SIZE;i++ )
	{
		*(bitarray + i) = 0;
	}
}

void Bitset::clear (int index)
{
	*(bitarray + index - 1) = 0;
}

void Bitset::set (int index)
{
	*(bitarray + index - 1) = 1;
};

void Bitset::set (int startIndex, int endIndex)
{
	int i;
	i = 0;
	for ( i=startIndex;i<=endIndex;i++ )
	{
		*(bitarray + i - 1) = 1;
	}
}
//*/
///*
Bitset Bitset::get (int startIndex,int endIndex)
{

	int size_n;
	size_n = endIndex - startIndex;
	int i;
	i = 0;
	int j;
	j = 0;
	int array[SIZE];

	for ( i=startIndex,j=0;i<=endIndex;i++,j++ )
	{
		array[j] = *(bitarray+i);
	}
	Bitset temp;
	for ( i=0;i<=size_n;i++ )
	{
	    array[i] = *(temp.bitarray + i);
	}
	return temp;
}

Bitset Bitset::andd (Bitset bitset)
{
	Bitset temp;
	*temp.bitarray = *bitarray & *bitset.bitarray;
	return *temp.bitarray;
}

Bitset Bitset::orr (Bitset bitset)
{
	Bitset temp;
	*temp.bitarray = *bitarray | *bitset.bitarray;
	return *temp.bitarray;
}
//*/
/*
void Bitset::xorr (Bitset bitset)
{
	Bitset temp;
	temp.bitarray = bitarray | bitset.bitarray;
	return temp.bitarray;
}
*/
/*
boolean Bitset::intersects(Bitset Bitset)
{

}
*/
/*
void Bitset::andNot(Bitset Bitset)
{

}

void Bitset::bitReplaceLeft(Bitset Bitset,int p, int n)
{

}

void bitReplaceRight(Bitset Bitset,int p, int n)
{

}
*/
void Bitset::show()
{
	int i;
	for ( i=SIZE;i>=0;i-- )
	{
		//cout<< *(bitarray - i + 1)<< endl;
		cout<< *(bitarray - i )<< endl;
	}
}

int main()
{
    int i;
	Bitset bs1(8), bs2(8);

    /*write code for setting the odd bits of bs1*/
    for ( i=1;i<SIZE;i++ )
    {
        if ( i%2==0 )
        {
            *(bs1.bitarray + i) = 1;
        }
        else
        {
            *bs1.bitarray = 0;
        }
    }


    /*write code for setting the even bits of bs2*/
    for ( i=1;i<SIZE;i++ )
    {
        if ( i%2!=0 )
        {
            *(bs2.bitarray + i) = 1;
        }
        else
        {
            *bs2.bitarray = 0;
        }
    }
	//cout<<bs1.cardinality()<<endl;
	//cout<<bs2.cardinality()<<endl;
    /*
	Bitset tempBS = bs2.get(2,7);

	tempBS.show();
	tempBS.andd(bs1);
	tempBS.show();
	tempBS.orr(bs2);
	tempBS.show();
	//tempBS.xorr(bs1);
	tempBS.show();
/*
	if(bs2.intersects(tempBS))
		cout<< "Intersection!!!"<<endl;
	else
		cout<< "No Intersection!!!"<<endl;
*/
	//tempBS.andNot(bs2);
	//tempBS.show();

	//bs1.bitReplaceLeft(bs2, 5, 4);
	bs1.show();
	cout<< endl;
	//bs2.bitReplaceRight(tempBS, 5, 4);
	bs2.show();
	//*/

	getch ();
	return 0;
}
