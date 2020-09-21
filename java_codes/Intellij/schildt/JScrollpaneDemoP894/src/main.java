/**
 * Created by Torsho on 12-May-16.
 */
import java.awt.*;
import javax.swing.*;

public class main extends JApplet{
    public void init() {
        try{
            SwingUtilities.invokeAndWait(
                    new Runnable() {
                        @Override
                        public void run() {
                            makeGUI();
                        }
                    }
            );
        }catch (Exception ex) {
            System.out.println("Cant create applet because of" + ex);
        }
    }           //          end of makegui

    public void makeGUI() {
        JPanel jp = new JPanel();
        jp.setLayout(new GridLayout(20, 20));
        int b = 0;
        for (int i=0;i<20;i++) {
            for (int j=0;j<20;j++){
                jp.add(new JButton("Button " + b));
                ++b;
            }
        }

        JScrollPane jsp = new JScrollPane(jp);

        add(jsp, BorderLayout.CENTER);
    }           ///         End of makeGUI


}
