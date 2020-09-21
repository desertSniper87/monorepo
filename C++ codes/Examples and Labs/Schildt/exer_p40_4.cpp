#include <iostream>
using namespace std;
#include <cstring>

void rev_str (char s1[100],char s2[100]);
void rev_str (char s1[100]);

int main (void)
{
    char s1[100], s2[100];
    strcpy ( s1,"hello" );
    rev_str (s1,s2);
    cout<<"\n";
    rev_str (s1);
    
    getchar ();
    return 0;
}

void rev_str (char s1[100],char s2[100])
{
     int len = strlen (s1);
     int i;
     int j = len -1 ;
     for ( i=0;i<len;i++,j-- )
     {
         s2[i] = s1[j];
     }
     
     //s2[i+1] ='\0';
     for ( i=0;i<len;i++ )
     {
         cout<< s2[i];
     }
     
}

void rev_str (char s1[100])
{
     strrev (s1);
     cout<< s1;
}
