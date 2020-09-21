/* convert each word into piglatin  */ 
void convert(int  words, char english(1,  char piglatin[J) 

int n,  count; 

int ml  =  0;  /* marker  ->  beginning  of word  */ 
int m2;  /*  marker  ->  end  of word  */ 
/* convert each word  */ 
for (n =  1;  n  <=  words; ++n)  { 
/* locate the end of  the current word  */ 
count =  ml; 
while  (english[count]  I=  ’  I) 
m2 =  count++; 
/*  transpose the first letter and add  ‘a’  */ 
for (count =  ml; count <  m2; ++count) 
piglatin[count  +  (n - l)] =  english[count  +  11; 
piglatin[m2  +  (n - 1)J =  english(ml1; 
piglatin(m2  +  n] =  ’a’; 
/*  reset the initial marker  */ 
ml  =  m2 +  2; 
1 
return; 

1 
