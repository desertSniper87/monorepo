import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


/**
 * Created by Torsho on 15-Nov-16.
 */
public class GUI implements ActionListener{
    private JTextField textField1;

    public void createGUI() {
    JFrame frame = new JFrame("Dos to Bash path converter");
    JPanel panel = new JPanel();
    panel.setLayout(new BorderLayout());

    frame.setSize(400,100);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    JTextField oldPathField = new JTextField("E:\\Khulna PC\\iGraphics\\SMI");
    oldPathField.setSize(100, 50);
    oldPathField.setLocation(200, 50);
    JTextField newPathField = new JTextField("");
    //newPathField.setSize(100, 50);
    //newPathField.setLocation(200, 50);
    JButton button = new JButton("Go!");

    panel.add(oldPathField);
    panel.add(newPathField);
    //panel.add(button);

    frame.setContentPane(panel);
    frame.setVisible(true);

    }

    public void actionPerformed(ActionEvent event){

    }
}
