package com.company;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

public class Gui implements ActionListener{

    JButton button;

    public static void main(String[] args) {
	// write your code here
        Gui gui = new Gui();
        gui.go();
    }

    public void go() {
        JFrame frame = new JFrame();
        button = new JButton("Click me");

        button.addActionListener(this);

        frame.getContentPane().add(button);
        frame.getDefaultCloseOperation();
        frame.setSize(300, 300);
        frame.setVisible(true);
    }

    public void actionPerformed(ActionEvent event) {
        button.setText("I've been clicked");
    }
}
