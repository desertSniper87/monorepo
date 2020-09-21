#include <iostream>
#include <cstdio>
#include <cstring>
using namespace std;

int main ()
{
    char temp_string[100];

    int count = 1;

    int rem = 100;
    int shata = 100;
    int hajar = 1000;
    int lakh = 100000;
    int kuti = 10000000;
    int kuti_rem = 0;

    int kuti_shata;
    int kuti_hajar;
    int kuti_lakh;
    int kuti_kuti;

    char rem_string[100] = "";
    char shata_string[100] = "";
    char hajar_string[100] = "";
    char lakh_string[100] = "";
    char kuti_string[100] = "";

    char kuti_shata_string[100] = "";
    char kuti_hajar_string[100] = "";
    char kuti_lakh_string[100] = "";
    char kuti_kuti_string[100] = "";

    long long int n;
    while ( cin>>n )
    {
        rem = n%rem;
        shata = (n/shata)%10;
        hajar = ( n/hajar ) %100;
        lakh = ( n/lakh ) % 100;
        kuti = ( n/kuti );
        kuti_rem = kuti%100;


        if ( kuti>=100 )
        {
            kuti_shata = ( kuti/100 )%10;
            kuti_hajar = ( kuti/1000 )%100;
            kuti_lakh = ( kuti/100000 )%100;
            kuti_kuti = ( kuti/10000000 )%100;
        }

        else
        {
            kuti_shata = 0;
            kuti_hajar = 0;
            kuti_lakh = 0;
            kuti_kuti = 0;
        }

        if ( !(n) )
        {

            cout<< count<< ". "<< n<< endl;
            count++;
        }
        else
        {
            if ( rem )
            {
                sprintf ( temp_string , "%d" , rem );
                strcat ( rem_string , temp_string );
            }

            if ( shata )
            {
                sprintf ( temp_string , "%d" , shata );
                strcat ( shata_string , temp_string );
                strcat ( shata_string , " shata " );
            }

            if ( hajar )
            {
                sprintf ( temp_string , "%d" , hajar );
                strcat ( hajar_string , temp_string );
                strcat ( hajar_string , " hajar " );
            }

            if ( lakh )
            {
                sprintf ( temp_string , "%d" , lakh );
                strcat ( lakh_string , temp_string );
                strcat ( lakh_string , " lakh " );
            }

            if ( kuti )
            {
                if ( !(kuti_rem ) )
                    strcat ( kuti_string , " kuti " );
                else
                {
                    sprintf ( temp_string , "%d" , kuti_rem );
                    strcat ( kuti_string , temp_string );
                    strcat ( kuti_string , " kuti " );
                }
            }

            if ( kuti_shata )
            {
                sprintf ( temp_string , "%d" , kuti_shata );
                strcat ( kuti_shata_string , temp_string );
                if (kuti_shata == 1)
                    strcat ( kuti_shata_string , " shata" );
                else
                    strcat ( kuti_shata_string , " shata " );
            }

            if ( kuti_hajar )
            {
                sprintf ( temp_string , "%d" , kuti_hajar );
                strcat ( kuti_hajar_string , temp_string );
                if (kuti_hajar == 1 || kuti_hajar == 10)
                    strcat ( kuti_hajar_string , " hajar" );
                else
                    strcat ( kuti_hajar_string , " hajar " );
            }

            if ( kuti_lakh )
            {
                sprintf ( temp_string , "%d" , kuti_lakh );
                strcat ( kuti_lakh_string , temp_string );
                if (kuti_lakh == 1 || kuti_lakh==10)
                    strcat ( kuti_lakh_string , " lakh" );
                else
                    strcat ( kuti_lakh_string , " lakh " );
            }

            if ( kuti_kuti )
            {
                sprintf ( temp_string , "%d" , kuti_kuti );
                strcat ( kuti_kuti_string , temp_string );
                if (kuti_kuti == 1 || kuti_kuti==10)
                    strcat ( kuti_kuti_string , " kuti" );
                else
                    strcat ( kuti_kuti_string , " kuti " );
            }

            cout<< count<< ". "<< kuti_kuti_string<< kuti_lakh_string<< kuti_hajar_string<< kuti_shata_string<< kuti_string<< lakh_string<< hajar_string<< shata_string<< rem_string<< endl;
            count++;
        }
        rem = 100;
        shata = 100;
        hajar = 1000;
        lakh = 100000;
        kuti = 10000000;
        kuti_rem = 0;

        kuti_shata = 100;
        kuti_hajar = 1000;
        kuti_lakh = 100000;
        kuti_kuti = 10000000;

        *rem_string = '\0';
        *shata_string = '\0';
        *shata_string = '\0';
        *hajar_string = '\0';
        *lakh_string = '\0';
        *kuti_string = '\0';

        *kuti_shata_string = '\0';
        *kuti_hajar_string = '\0';
        *kuti_lakh_string = '\0';
        *kuti_kuti_string = '\0';
    }

    return 0;
}
