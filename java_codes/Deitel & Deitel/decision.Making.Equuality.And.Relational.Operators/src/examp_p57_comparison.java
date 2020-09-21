import java.util.Scanner;
public class examp_p57_comparison 
{

	/**
	 * @param args
	 */
	public static void main(String[] args) 
	{
		// TODO Auto-generated method stub
		Scanner input = new Scanner (System.in) ;
		
		int num1;
		int num2;
		
		System.out.print("Enter first Integer");
		num1 = input.nextInt();
		
		System.out.print("Enter second Integer");
		num2 = input.nextInt();
		
		if ( num1==num2 )
		{
			System.out.println( "The numbers are equal" );
		}
		else if ( num1>num2 )
		{
			System.out.println ( "The first number is greater than the second number" );
		}
		else if ( num1<num2 )
		{
			System.out.println( "The second number is greater than the first number" );
		}
	}

}
