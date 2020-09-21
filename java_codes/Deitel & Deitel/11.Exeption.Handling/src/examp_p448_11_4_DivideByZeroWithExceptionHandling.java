import java.util.Scanner;
import java.util.InputMismatchException;


public class examp_p448_11_4_DivideByZeroWithExceptionHandling
{
	public static int quotient ( int numerator, int denominator )
			throws ArithmeticException
	{
			return numerator / denominator;
	}
	
	public static void main ( String[] args )
	{
		int a; a=0;
		int b; b=0;
		
		Scanner input = new Scanner( System.in );
		boolean continueloop;
		continueloop = true;
		
		while ( continueloop==true )
		{
			try
			{
				System.out.println ( "Please enter numerator:\n" );
				a = input.nextInt();
				System.out.println( "Please eneter denominator:\n" );
				b = input.nextInt();
				
				int result;
				result = quotient ( a,b );
				System.out.printf ( "Result = %d",result );
				continueloop = false;
			}
			catch ( InputMismatchException inputMismatchException )
			{
				System.err.printf( "\nException: \n%s\n",inputMismatchException );
				input.nextLine();
				System.out.println ("You must enter integers\nPlease try again.");
			}
			catch ( ArithmeticException arithmeticException )
			{
				System.err.printf( "\nException:\n%s\n",arithmeticException );
				input.nextLine();
				System.out.println ("Cannot divide by zero, please try again");
			}
		}
	}

}
