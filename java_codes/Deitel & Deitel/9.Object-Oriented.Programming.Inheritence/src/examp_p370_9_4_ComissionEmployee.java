
public class examp_p370_9_4_ComissionEmployee 
{
	private String firstName;
	private String lastName;
	private String socialSecurityNumber;
	private double grossSales;
	private double comissionRate;
	
	private examp_p370_9_4_ComissionEmployee ( String first, String last, String ssn, double sales, double rate )
	{
		firstName = first;
		lastName = last;
		socialSecurityNumber = ssn;
		grossSales = sales;
		comissionRate = rate;
	}
	
	public void setFirstName ( String first )
	{
		firstName = first;
	}
	public String getFirstName ()
	{
		return firstName;
	}
	
	public void setLastName ( String last )
	{
		lastName = last;
	}
	public String getLastName ()
	{
		return lastName;
	}
	
	public void setSocialSecurityNumber ( String ssn )
	{
		socialSecurityNumber = ssn;
	}
	public String getSocialSecurityNumber ()
	{
		return socialSecurityNumber;
	}
	
	public void setGrossSales ( double sales )
	{
		grossSales = sales;
	}
	public double getGrossSales ()
	{
		return grossSales;
	}
	
	public void setComissionRate ( double rate )
	{
		comissionRate = rate;
	}
	public double getComissionRate ()
	{
		return comissionRate;
	}
	
	public double earnings()
	{
		return comissionRate * grossSales;
	}
	
	@Override
	public String toString ()
	{
		return String.format("%s: %s %s\n%s:\n%s: %.2f\n%s: %.2f\n", "Comission Employee",firstName,lastName,"Social Security number",socialSecurityNumber,"Gross sales",grossSales,"Comission rate",comissionRate);
	}
	
	
}
