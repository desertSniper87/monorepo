#include<iostream>  
#include<cstring>
#include<algorithm>
#include<string>
#include<cassert>
#include<iomanip>
using namespace std;

#define MAX 100
//#define for(i,a,b) for(i=a;i<b; i++)

string grammar[MAX][MAX];	//to store entered grammarmar
string dpr[MAX];
int p,np;		//np-> number of productions


void break_grammar(string a)	//seperates right hand side of entered grammarmar
{
	int i;
	p=0;
	while(a.length())
	{
		i=a.find("|");
		if(i>a.length())
		{
			dpr[p++] = a;
			a="";
		}
		else
		{
			dpr[p++] = a.substr(0,i);
			a=a.substr(i+1,a.length());
		}
	}
}

int lchomsky(string a)	//checks if LHS of entered grammarmar is in CNF
{
	if(a.length()==1 && a[0]>='A' && a[0]<='Z')
		return 1;
	return 0;
}

int rchomsky(string a)	//checks if RHS of grammarmar is in CNF
{
	if (a.length() == 1 && a[0]>='a' && a[0] <='z')
		return 1;
	if (a.length()==2 && a[0]>='A' && a[0]<='Z' && a[1]>='A' && a[1]<='Z' )
		return 1;
	return 0;
}

string concat( string a, string b)	//concatenates unique non-terminals
{
	int i;
	string str=a;
	for(i=0;i<b.length();i++)
	{
		if(str.find(b[i]) > str.length())
			str+=b[i];
	}
	return str;
}

string find_production(string p)	//returns a concatenated string of variables which can produce string p
{
	int j,k;
	string str="";
	for(j=0;j<np;j++)
	{
		k=1;
		while(grammar[j][k] != "")
		{
			if(grammar[j][k] == p)
			{
				//str=concat(str,grammar[j][0]);
				str = str+ grammar[j][0];
			}
			k++;
		}
	}	
	return str;
}

string multiply_var(string a, string b)	//creates every combination of variables from a and b . For eg: BA * AB = {BA, BB, AA, BB}
{
	int i,j;
	string onevar="";
	//srting str="";
	string str="";

	for(i=0;i<a.length();i++)
	{
		for(j=0;j<b.length();j++)
		{
			onevar="";
			onevar=onevar+a[i]+b[j];
			str=str+find_production(onevar);		//searches if the generated productions can be created or not
		}
	}
	return str;
}

int main()
{
	int i,indx,j,l,k;
	string a,str,r,pr,start;

	//cout<<"\nEnter the start Variable ";
	//cin >> start;

	cout<<"\nNumber of productions ";
	cin >> np;
	for(i=0;i<np;i++)
	{
		cin >> a;
		indx=a.find("->");
		grammar[i][0] = a.substr(0,indx);
		if (lchomsky(grammar[i][0]) == 0)
		{
			cout<<"\ngrammarmar not in Chomsky Form";
			//abort();
		}
		a = a.substr(indx+2, a.length());
		break_grammar(a);
		for(j=0;j<p;j++)
		{
			grammar[i][j+1]=dpr[j];
			if (rchomsky(dpr[j]) == 0)
			{
				cout<<"\ngrammarmar not in Chomsky Form";
				//abort();
			}
		}
	}

	start= grammar[0][0];

	//string matrix[MAX][MAX];
	
	string itoj;
	string onebin;
	cout<<"\nEnter string to be checked : ";
	cin >> str;

	int len = str.length();
	string matrix[5][5];

	for(int x= 0;x<5;x++)
	{
		for(int y = 0;y<5;y++)
		{
			matrix[x][y] = "-";
		}
	}

	for(i=0;i<str.length();i++)		//Assigns values to principal diagonal of matrix
	{
		onebin="";
		itoj = "";
		itoj+=str[i];
		for(j=0;j<np;j++)
		{
			k=1;
			while(grammar[j][k] != "")
			{
				if(grammar[j][k] == itoj)
				{
					onebin=concat(onebin,grammar[j][0]);
				}
				k++;
			}
		}
		matrix[i][i]=onebin;
	}

	//for(i=0;i<str.length();i++)
	//{
	//	//l = str.length()-i-1;
	//	for(j=0;j<str.length();j++)
	//	{
	//		cout<<setw(5)<<matrix[i][j]<<" ";
	//	}
	//	cout<<endl;
	//}

	for(k=1;k<str.length();k++)		//Assigns values to upper half of the matrix
	{
		for(j=k;j<str.length();j++)
		{
			onebin="";
			for(l=j-k;l<j;l++)
			{
				pr = multiply_var(matrix[j-k][l],matrix[l+1][j]);
				onebin = concat(onebin,pr);
			}
			matrix[j-k][j] = onebin;
		}
	}


	//cout<< matrix [0][0];
	//for(i=0;i<str.length();i++)	//prints the matrix
	//{
	//	k=0;
	//	l=str.length()-i-1;
	//	for(j=l;j<str.length();j++)
	//	//for(j=i+1;j<str.length();j++)
	//	{
	//		cout<<setw(5)<<matrix[k][j]<<" ";
	//	}
	//	cout<<endl;
	//}

	int inc = 1;
	for(i=0;i<str.length();i++)	//prints the matrix
	{
		k=0;
		l=str.length()-i-1;
		//for(j=l;j<str.length();j++)
		if(i == 0) j =0;
		else j = i;	
		for(j=j;j<str.length();j++)
		{
			cout<<matrix[k++][j]<<" ";
		}
		cout<<endl;
		inc++;
	}
			
	int f=0;
	for(i=0;i<start.length();i++)
		if(matrix[0][str.length()-1].find(start[i]) <= matrix[0][str.length()-1].length())	//Checks if last element of first row contains a Start variable
		{
			cout<<"Pattern matched\n";
			int stop;
			cin>>stop;
			return 0;
		}
	cout<<"Not in the gramamr\n";
	int stop;
	cin>>stop;

	
	return 0;
}