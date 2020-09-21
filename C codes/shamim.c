#include <stdio.h>

int days[13] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

int main()
{
printf("---%d---", sunonfirstofthemonth(2000));
}

int sunonfirstofthemonth(int year)
{
int day, i, j, s=0;
day = 2; // Sunday
for(i=1901; i<=year; i++)
{
for(j=1; j<=12; j++)
{
if(startofmonth(i, j)==day)
s++;
}
}
return s;
}

int startofmonth(int year, int month)
{
int i, totaldays;
totaldays = (year-1901) * 365 - 3 + countleap(year-1);
for(i=1; i<=month; i++)
{
totaldays += days[i-1];
}
if(leapyear(year) && month>2)
{
totaldays += 1;
}

return (totaldays%7);

}
int countleap(int year)
{
int i, k;
for(i=1901, k=0; i<=year; i++)
{
if(leapyear(i))
k++;
}
return k;
}
int leapyear(int year)
{
if(!(year%4))
{
if(!(year%100))
{
if(!(year%400))
return 1;

return 0;
}
return 1;
}

return 0;
}
