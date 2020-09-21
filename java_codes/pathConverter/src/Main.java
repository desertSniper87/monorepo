import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;

/**
 * Created by Torsho on 15-Nov-16.
 */
public class Main {
    public static void main(String args[]){


        String oldPath = "E:\\Khulna PC\\iGraphics\\SM";
        String newPath = "/mnt/";



        for (String i: oldPath.split("\\\\")
             ) {
            StringBuilder sb = new StringBuilder();
            if (i.contains(":")) {

                sb.append(i.toLowerCase().replace(":", ""));
                newPath += sb + "/";
            }

            else if (i.contains(" ")){
                sb.append(i.replace(" ", "\\ "));
                newPath += sb + "/";
            }

            else
                newPath += i + "/";


                newPath.concat(sb.toString()).concat("/");


        }
        System.out.println(newPath);
    }

}
