/**
 * Created with IntelliJ IDEA.
 * User: Torsho
 * Date: 1/18/14
 * Time: 12:57 PM
 * To change this template use File | Settings | File Templates.
 */
import java.util.*;
import java.lang.*;
import java.io.*;
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;

public class Logger
{
    public static void main ( String[] args ) throws Exception
    {
        GregorianCalendar gc = new GregorianCalendar();
        SimpleDateFormat dateformat = new SimpleDateFormat( "EEE, MMM, d, yyyy 'at' hh:mm:ss a"  );
        String datestr = dateformat.format ( gc.getTime() );
//        System.out.println ( datestr );

        int i;

        int A;
        int B;

        /*input = new ObjectInputStream (
                new FileInputStream( "text.txt" )
        );
*/

        FileOutputStream filestream = new FileOutputStream("logger.ser");
        ObjectOutputStream os = new ObjectOutputStream( filestream );


        Random randomGenerator = new Random();

        for ( i=0;i<1000;i++ )
        {
            A = randomGenerator.nextInt(100);
            B = randomGenerator.nextInt(100);

            if ( A%B==0 )
                os.writeChars( A +"  " + B + "\n" );


        }

        os.close();
    }
}
