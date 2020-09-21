
public class examp__p17_5_AccountRecord
{
	private int account;
	private String firstName;
	private String lastName;
	private double balance;
	
	public examp__p17_5_AccountRecord ()
	{
		this ( 0, "", "", 0.0 );
	}
	
	public examp__p17_5_AccountRecord( int acc, String first, String last,double bal )
	{
		account = acc;
		firstName = first;
		lastName = last;
		balance = bal;
		
	}
	
	public void setAccount (int acc)
	{
		account = acc;
	}
	
	public int getAccount ()
	{
		return account; 
	}
	
	public void setFirstName ( String first )
	{
		firstName = first;
	}
	
	public String getFirstName()
	{
		return firstName;
	}
	
	public void setLastName (String last)
	{
		lastName = last;
	}
	
	public String  getLastName ()
	{
		return lastName;
	}
	
	public void setBalance ( double bal )
	{
		balance = bal;
	}
	
	public double getBalance ()
	{
		return balance;
	}
	
	
	
}
