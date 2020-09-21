
public class examp_p252_InitArray 
{
	public static void main (String[] args)
	{
		int[] array;
		array = new int[10];
		System.out.printf ("%s\t%s\n","Index","Value");
		//System.out.printf ("%s\t%value\n");
		
		int counter; counter = 0;
		for ( counter=0;counter<10;counter++ )
		{
			System.out.printf ("%d\t%d\n",counter,array[counter]);
		}
		
		System.out.println ("Initializing array2\n");
		
		//int[] array2;
		//array2 = new int[10];
		//int[] array2 = new int;
		int[] array2 = {10,20,30,40,50,60,70,80,90,100};
		for ( counter=0;counter<10;counter++ )
		{
			System.out.printf ("%d\t%d\n",counter,array2[counter]);
		}
		
	}
	
}
