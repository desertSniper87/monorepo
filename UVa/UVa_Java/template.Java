import java.util.StringTokenizer;

public class Main {

    public static char[] cinbuf = new char[256];

    public static String readLine() {
        int ch;
        int offset = 0;

        try {
            do{
                ch = System.in.read();

                if(ch!='\r'&&ch!='\n'&&ch!=-1) {
                    cinbuf[offset++] = (char)ch;
                }
            }while(ch!='\n'&&ch!=-1);

            if(ch==-1&&offset==0)
                return null;
        } catch(Exception e) {
            return null;
        }

        return new String(cinbuf,0,offset);
    }
// Code for problem