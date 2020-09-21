import java.io.*;

public class FileChecker
{
	File x = new File("c:\\NewFile.txt");
	
	/*
	try
	{
		x = new formatter("C:\\NewFile.txt");
	
		x.
	}
	*/
	/*
	catch ( IOException ioe )
	{
		
	}
	*/
	
	if ( x.exist() )
	{
		System.out.print(x.getName() + "exists");
	}
	
}
