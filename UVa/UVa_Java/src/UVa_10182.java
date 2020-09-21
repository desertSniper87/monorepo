
import java.util.*;

class Main
{
    public static void main (String args[])
    {
        Main main = new Main ();
        main.Code ();
    }

    void Code ()
    {
        Scanner input = new Scanner (System.in);
        int _case;

        long a;
        long b;

        _case = input.nextInt ();

        while ( _case!=0 )
        {
            a =  input.nextInt();
            b =  input.nextInt();

            if ((a > b) && (((a + b) & 1) == 0))
            {
                System.out.print( ( a+b )/2 );
                System.out.print(" ");
                System.out.print( ( a-b )/2 );
                System.out.print ("\n");
            }       //  END OF IF

            else
                System.out.println ("impossible");

            _case--;
        }       // END OF WHILE
    }
}

