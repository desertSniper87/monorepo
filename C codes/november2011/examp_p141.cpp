#include <stdio.h>

int main (void)

{
    int temp [31];
    int min;
    int max;
    int sum;
    
    int days;
    int i;
    
    printf ("How many days: ");
    scanf ("%d",&days);
    
    for ( i=0;i<days;i++ )
    {
        printf ("Enter temparature for day %d:\t",i+1);
        scanf ("%d",& temp[i] );
    }
    
    min = 200;
    max = 0;
    
    for ( i=0;i<days;i++ )
    {
        sum += temp [i];
        if (min > temp [i]) min = temp [i];
        if (max < temp [i]) max = temp [i];
    }
    
    double avg = (1.0*sum/days);
    
    printf ("The average: %lf\n",avg); 
    printf ("The minimum: %d\n",min);
    printf ("The maximum: %d\n",max);
    
    scanf ("%d",&days);
    return 0;


}
