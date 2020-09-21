#include <stdio.h>
#include <conio.h>

int main (void)

{
    int roll;
    printf ("Enter a roll from 86 and 90\n");
    
    for ( ;; )
    {
        scanf ("%d",&roll);
        
        switch (roll)
        {
               case 86 : 
                    printf ("Sunny Chowdhury");
                    printf ("\n");
                    break;
               case 87 : 
                    printf ("Samidhya Sarker");
                    printf ("\n");
                    break;
               case 88 : 
                    printf ("Arannya Monzur");
                    printf ("\n");
                    break;
               case 89 : 
                    printf ("Farzana Ahmed Siddiqui");
                    printf ("\n");
                    break;
               case 90 :
                    printf ("Wahid-Uz-Zaman");
                    printf ("\n");
                    break;
                    
               default    :
                          printf ("Not found\n");
                    
        }
    }
    
	getch ();
	return 0;
}
