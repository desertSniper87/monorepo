#include <stdio.h>
#include <conio.h>
#include <string.h>

void enterinput ( int i );
void fileoutput ( int i, FILE *fp );

struct student_database
{
       char name[100];
       char id[100];
       char hometown[100];
       char department[10];
}
student[100];

int main (void)

{
    FILE *fp;
    int n;
    int i = 0;
    printf ("Enter the number of students: \n");
    scanf ("%d",&n);
    
    for ( i=0;i<n;i++ )
    {
        enterinput ( i );
    }
    
    if ( (fp = fopen ( "stddbase.txt" , "wt" ))== 0  )
       printf ("Cannot open file.\n");
       
    char ch[] = "Name\t\tID\t\tHometown\t\tDepartment\n";
    fprintf ( fp , ch );
       
    for ( i=0;i<n;i++ )
    {
        fileoutput ( i,fp );
    }
       
    fclose (fp);
    printf ("\nThank you for using student Database.");

	getch ();
	return 0;
}

  

void enterinput ( int i )
{    
     printf ("--- --- Student %d --- ---\n",i+1);    
     printf ("Please enter name: \n");
     scanf ("%s",&student[i].name);
     printf ("Please enter ID: \n");
     scanf ("%s",&student[i].id) ;  
     printf ("Please enter hometown: \n");
     scanf ("%s",&student[i].hometown);
     printf ("Please enter department: \n");
     scanf ("%s",&student[i].department);
}

void fileoutput ( int i, FILE *fp )
{
     fprintf ( fp , student[i].name );
     fprintf ( fp , "\t\t" );
     fprintf ( fp , student[i].id );
     fprintf ( fp , "\t\t" );
     fprintf ( fp , student[i].hometown );
     fprintf ( fp , "\t\t" );
     fprintf ( fp , student[i].department );
     fprintf ( fp , "\n" );
     
}
