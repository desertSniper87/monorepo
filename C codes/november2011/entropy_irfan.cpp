#include<stdio.h>
#include<string.h>
int main(void)
{
    int i,j,k,sum=0;
    
    char str[100],str1[100];
    gets(str);
    j=0;
    for(i=0;str[i]!='\0';i++)
    {
                      if(str[i]==','||str[i]=='.'||str[i]==';')//PROB
                      {
                                  printf("l");
                                  
                      }
                      else
                     { str1[j]=str[i];
                      //printf("%c",str1[j]);  
                      j++;}                
    }
    k=strlen(str1);
    //printf("\n%d %c\n",k,str1[2]);
    for(j=0;j<k;j++)
    {
                              //printf("%c",str1[j]);
                              sum=sum+str1[j];
                              //printf("%d",sum);
    }
    printf("%d",sum);
    
    scanf("%d");
    
    
}
