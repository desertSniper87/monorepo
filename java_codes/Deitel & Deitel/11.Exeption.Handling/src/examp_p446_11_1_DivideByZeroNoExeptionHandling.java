import java.util.Scanner;

public class examp_p446_11_1_DivideByZeroNoExeptionHandling
{
	public static int quotient ( int numerator, int denominator )
	{
		return numerator / denominator ;
	}
	
	public static void main ( String[] args )
	{
		int a;
		int b;
		
		Scanner input = new Scanner( System.in );
		
		System.out.println ( "Please enter numerator:\n" );
		a = input.nextInt();
		System.out.println( "Please eneter denominator:\n" );
		b = input.nextInt();
		
		int result;
		result = quotient ( a,b );
		System.out.printf ( "Result = %d",result );
	}

}
