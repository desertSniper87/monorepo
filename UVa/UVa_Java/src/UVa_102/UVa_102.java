/** i=
 * Created by torsho on 3/21/17.
 */

package UVa_102;

import java.io.IOException;
import java.util.*;

class Main
{
    static String ReadLn (int maxLg)  // utility function to read from stdin
    {
        byte lin[] = new byte [maxLg];
        int lg = 0, car = -1;
        String line = "";

        try
        {
            while (lg < maxLg)
            {
                car = System.in.read();
                if ((car < 0) || (car == '\n')) break;
                lin [lg++] += car;
            }
        }
        catch (IOException e)
        {
            return (null);
        }

        if ((car < 0) && (lg == 0)) return (null);  // eof
        return (new String (lin, 0, lg));
    }

    public static void main (String args[])  // entry point from OS
    {
        Main myWork = new Main();  // create a dinamic instance
        myWork.Begin();            // the true entry point
    }

    void Begin()
    {
        String input;
        StringTokenizer idata;

        while ((input = Main.ReadLn (255)) != null)
        {
            idata = new StringTokenizer (input);

            ArrayList<Integer> resultBin = new ArrayList<>();
            ArrayList<Integer> arrayList = null;

            arrayList = new ArrayList();
            for (int i = 0; i < 9; i++) {
                arrayList.add(Integer.valueOf(idata.nextToken()));
            }

            int i = 0;
            int result;
            /** i=0 bcg*/
            result = arrayList.get(1)+arrayList.get(2) + arrayList.get(3)+arrayList.get(4)+arrayList.get(6)+arrayList.get(8);
            resultBin.add(i, result);
            i++;
            /** i=1 bgc*/
            result = arrayList.get(1)+arrayList.get(2) + arrayList.get(3)+arrayList.get(5)+arrayList.get(6)+arrayList.get(7);
            resultBin.add(i, result);
            i++;
            /** i=2 cbg*/
            result = arrayList.get(0)+arrayList.get(1) + arrayList.get(4)+arrayList.get(5)+arrayList.get(6)+arrayList.get(8);
            resultBin.add(i, result);
            i++;
            /** i=3 cgb*/
            result = arrayList.get(0)+arrayList.get(1) + arrayList.get(3)+arrayList.get(5)+arrayList.get(7)+arrayList.get(8);
            resultBin.add(i, result);
            i++;
            /** i=4 gbc*/
            result = arrayList.get(0)+arrayList.get(2) + arrayList.get(3)+arrayList.get(4)+arrayList.get(7)+arrayList.get(8);
            resultBin.add(i, result);
            i++;
            /** i=5 gcb*/
            result = arrayList.get(0)+arrayList.get(2) + arrayList.get(3)+arrayList.get(4)+arrayList.get(7)+arrayList.get(8);
            resultBin.add(i, result);
            i++;



            int finalResult = Collections.min(resultBin);
            int index = resultBin.indexOf(finalResult);

            switch(index){
                case 0 :
                    System.out.print("BCG "+ finalResult);
                    break;
                case 1 :
                    System.out.print("BGC "+ finalResult);
                    break;
                case 2 :
                    System.out.print("CBG "+ finalResult);
                    break;
                case 3 :
                    System.out.print("CGB "+ finalResult);
                    break;
                case 4 :
                    System.out.print("GCB "+ finalResult);
                    break;
                case 5 :
                    System.out.print("GBC "+ finalResult);
                    break;
            }

            resultBin.clear();
            System.out.println();
        }
    }
}
