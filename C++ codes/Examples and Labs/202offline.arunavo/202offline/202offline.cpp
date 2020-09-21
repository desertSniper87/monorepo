#include<iostream>
#include<stdio.h>
using namespace std;
class BitSet{
public:
	int *bitArray;
	BitSet(int size=16){bitArray = new int[size];}
	void signext(BitSet &bitset)
	{
		int i,j;
		for(i=0;bitArray[i]==0 || bitArray[i]==1;i++);
		for(j=0;bitset.bitArray[j]==0 || bitset.bitArray[j]==1;j++);
		if(i>j)
		{
			int *bits,k;
			k=i-j;
			bits=new int[j--];
			for(;j>=0;j--)bits[j]=bitset.bitArray[j];
			for(j=0;j<k;j++)bitset.bitArray[j]=bits[0];
			for(k=0;j<i;j++,k++)bitset.bitArray[j]=bits[k];
		}
		if(i<j)
		{
			int *bits,k;
			k=j-i;
			bits=new int[i--];
			for(;i>=0;i--)bits[i]=bitArray[i];
			for(i=0;i<k;i++)bitArray[i]=bits[0];
			for(k=0;i<j;i++,k++)bitArray[i]=bits[k];
		}
		return;
	}
	void andd(BitSet bitset)
	{
		(*this).signext(bitset);
		int i;
		for(i=0;bitset.bitArray[i]==0 || bitset.bitArray[i]==1;i++)
			{
				if(bitArray[i] && bitset.bitArray[i])bitArray[i]=1;
				else bitArray[i]=0;
			}
		return;
	}
	void orr(BitSet bitset)
	{
		(*this).signext(bitset);
		int i;
		for(i=0;bitset.bitArray[i]==0 || bitset.bitArray[i]==1;i++)
			{
				if(bitArray[i] || bitset.bitArray[i])
				{
					bitArray[i]=1;
					cout<<bitArray[i]<<endl;
				}
				else
				{
					bitArray[i]=0;
					cout<<bitArray[i]<<endl;
				}
			}
		return;
	}
	void xorr(BitSet bitset)
	{
		(*this).signext(bitset);
		int i;
		for(i=0;bitset.bitArray[i]==0 || bitset.bitArray[i]==1;i++)
			{
				if((bitArray[i] && bitset.bitArray[i]) || !(bitArray[i] || bitset.bitArray[i]))bitArray[i]=0;
				else bitArray[i]=1;
			}
		return;
	}
	bool intersects(BitSet bitset)
	{
		(*this).signext(bitset);
		int i;
		bool found=false;
		for(i=0;bitset.bitArray[i]==0 || bitset.bitArray[i]==1;i++)
			{
				if(bitArray[i] == bitset.bitArray[i])
				{
					found=true;
					break;
				}
			}
		return found;
	}
	void andNot(BitSet bitset)
	{
		(*this).signext(bitset);
		int i;
		for(i=0;bitset.bitArray[i]==0 || bitset.bitArray[i]==1;i++)
			{
				if(bitset.bitArray[i]==1)bitArray[i]=0;
			}
		return;
	}
	void bitReplaceLeft(BitSet bitset,int p,int n)
	{
		(*this).signext(bitset);
		int i;
		p--;
		for(i=0;(bitset.bitArray[p]==0 ||bitset.bitArray[p]==1) && i<n;p++,i++)bitArray[p]=bitset.bitArray[i];
		return;

	}
	void bitReplaceRight(BitSet bitset,int p,int n)
	{
		(*this).signext(bitset);
		int i,j;
		p--;
		for(j=0;bitset.bitArray[j]==0 || bitset.bitArray[j]==1;j++);
		j--;
		for(i=0;i<n;p++,i++)
			{
				bitArray[p]=bitset.bitArray[j];
				j--;
			}
		return;
	}
	friend void show(BitSet bitset)
	{

		int i;
		for(i=0;bitset.bitArray[i]==0 || bitset.bitArray[i]==1;i++)cout<<bitset.bitArray[i];
		cout<<endl;
		return;
	}

};
int main()
{
	BitSet a(4),b(4);
	a.bitArray[0]=1;
	a.bitArray[1]=0;
	a.bitArray[2]=0;
	a.bitArray[3]=1;
	b.bitArray[0]=1;
	b.bitArray[1]=0;
	b.bitArray[2]=1;
	//b.bitArray[3]=0;
	a.bitReplaceLeft(b,2,2);
	show(a);

	return 0;
}
