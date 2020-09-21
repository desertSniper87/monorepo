/**
 * Created with IntelliJ IDEA.
 * User: Torsho
 * Date: 1/18/14
 * Time: 12:57 PM
 * To change this template use File | Settings | File Templates.
 */
public class Singleton
{
    private static Singleton instance = null;
    private Singleton ()
    {

    }

    static public Singleton getInstance()
    {
        if ( instance==null )
            instance = new Singleton ();

        return instance;
    }

}
