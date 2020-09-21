import java.util.Random;

public class examp_p1065_26_4_PrintTask implements Runnable
{
	private static final int sleepTime;
	private static final  String taskName;
	private final static Random generator = new Random();
	
	public examp_p1065_26_4_PrintTask ( String name )
	{
		taskName = name;
		sleepTime = generator.nextInt( 5000 );
	}
	
	public void run ()
	//public static void main (String[] args)
	{
		try
		{
			System.out.printf ("%s is going to sleep for %d milliseconds\n",taskName,sleepTime);
		}
		
		catch ( InterruptedException exception ) 
		{
			System.err.printf("%s terminated prematurely because of exception",taskName);
		}
		
		System.out.printf("%s is done sleeping",taskName);
	}
}
