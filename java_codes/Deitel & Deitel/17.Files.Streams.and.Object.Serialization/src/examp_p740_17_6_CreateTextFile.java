import java.io.*;
import java.util.Scanner;
import java.util.Formatter;
import java.util.FormatterClosedException;
import java.lang.SecurityException;
import java.util.NoSuchElementException;



public class examp_p740_17_6_CreateTextFile 
{
	examp__p17_5_AccountRecord account = new examp__p17_5_AccountRecord();
	private Formatter output;
	
	public void openFile()
	{
		try
		{
			output = new Formatter ("c:\\Account Records.txt"); 
		}
		catch ( SecurityException secExc )
		{
			System.out.println ("Administrative permission required");
		}
		catch (FileNotFoundException e)
		{
			e.printStackTrace();
		}
	}
	Scanner input = new Scanner (System.in);
	
	public void enterRecord()
	{
		System.out.println ("\nPlease enter ctrl+z and the press enter to end input\n");
		System.out.println ("\nPlease enter Account number, First Name, Last Name and Balance\n");
	
		while ( input.hasNext() )
		{
			try
			{
				account.setAccount( input.nextInt() );
				account.setFirstName( input.next() );
				account.setLastName( input.next() );
				account.setBalance( input.nextDouble() );
			
			
				if ( account.getAccount()<1 )
				{
					System.out.println ("Account number must be greater than 0");
				}
				else
				{
					output.format ("%d\t%s\t%s\t%f\n",account.getAccount(), account.getFirstName(), account.getLastName(), account.getBalance() );
				}
			}
		
			catch ( FormatterClosedException forCloExc )
			{
				System.out.println ("I/O error");
				return;
			}
			catch ( NoSuchElementException noSucEleExc )
			{
				System.out.println ("Please enter valid numeber");
				input.nextLine();
			}
		}
	}	
	public void closeFile()
	{
		if ( output!=null )
		output.close();
	}
	}
