
    #include <stdio.h> /* Standard library for C input/output */
    #include <stdlib.h> /* Standard library definitions */
    #define TERMINAL -9 /* This is a macro that is defined globally */
    /* it is used to tell the program that the user is */
    /* done with their input. */

    int main() /* Main declaration with arguments */
    {

    int userInput1,
    userInput2,
    nFact = 1,
    kFact = 1,
    n_k,
    nkFact = 1,
    combination;



    do {
    printf("\nEnter the number of items in the list (n): ");
    scanf("%d", &userInput1);
    if ( userInput1 < 1 || userInput1 > 10 ) /* is n between 1 - 10 */
    printf("\n?Invalid input: Number must be between 1 and 10\n");
    else
    break;
    } while ( userInput1 < 1 || userInput1 > 10 );



    /* As long as n>=1 program will loop the following n*n-1 */

    int n;
    for ( n = userInput1; n > 1; n--) {
    nFact = ( nFact * n );
    }

    printf("n! = %d", nFact);


    do {
    printf("\nEnter the number of items to choose (k): ");
    scanf("%d", &userInput2);
    if ( userInput2 < 1 || userInput2 > 10 ) /* is k between 1 - 10 */
    printf("\n?Invalid input: Number must be between 1 and 10\n");
    else
    break;
    } while ( userInput2 < 1 || userInput2 > 10 );


    int k;
    for ( k = userInput2; k > 1; k--) {
    kFact = ( kFact * k );
    }


    printf("k! = %d", kFact);
    printf("n = %d k = %d", userInput1, userInput2);

    n_k = userInput1 - userInput2;

    while (n_k>=1)
    {
    nkFact = ( nkFact * n_k );
    n_k = ( n_k - 1 );
    }

    printf("\n(n-k)! = %d", nkFact);


    combination = nFact / ( (nkFact) * kFact );


    printf("\nNumber of combinations: %d", combination);




    printf("\n");
    system("PAUSE");
    return(0);
    }
