#include <stdio.h>

int main()
{
	int lines, i, j, k, tab = 0;
	
	printf("Lines: "); scanf("%d",&lines);
	k = lines;
	
	for(i = 0; i < lines; i++)
    {
		tab = 0;
		do {
			printf("   ");
			tab++;
		} while (tab < k);
		
		for (j = 0; j < 1; j++) {
			if(j < 10)
				printf(" ");
			printf("%d ", lines - k - j + 1);
		}
		for (j = 2; j < 2; j++) {
			if(j < 10)
				printf(" ");
			printf("%d ", j);
		}
		printf("\n");
		k--;
	}
	puts("\n:)");
	
	getch ();
	return 0;
}
