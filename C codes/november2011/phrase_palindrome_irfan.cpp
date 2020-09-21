
#include<stdio.h>
#include<string.h>
int main(void)
{
    int i,j,k,m;
    char s1[100],s2[100],s3[100];
    gets(s1);
    j=0;
    i=0;
    k=strlen(s1);


    for(i=0;i<k;)
    {
                        // printf("l");
                         if(65<=s1[i] && s1[i]<97)
                         {
                                     printf("k");
                                     printf("%c",s1[i]);
                                     s1[i]=s1[i]+32;
                                     printf("%c",s1[i]);
                         }
                         else if(s1[i]<65)
                         {i++;}
                         else
                         {
                             printf(" %d ",j);
                             s2[j]=s1[i];
                             printf("%c",s2[j]);
                             j++;
                             printf("%d",j);
                             i++;

                         }
    }
    s2[j]='\0';
    k=strlen(s2);m=k-1;
    printf("%d",k);
    for(j=0;j<=k-1;j++)
    {
//                  printf("\n%c\n",s2[m]);
                    s3[j]=s2[m];
                    m--;
                    printf("%c",s3[j]);
    }
    s3[k]='\0';
    printf(s3);
    printf(" %d ",strcmp(s2,s3));
    if(strcmp(s3,s2)==0)
    {
                        printf("yes");
    }
    else printf("no");
    scanf("%d");
    return 0;
}
