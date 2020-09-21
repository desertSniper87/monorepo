package com.company;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class Main {

    private static int counter;

    public static void main (String[] args) {
	// write your code here
        new JFrame().setVisible(true);
        ActionListener actionListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                counter ++ ;
                System.out.println("Counter=" + counter);
            }
        };

        Timer timer = new Timer(1000, actionListener);
        timer.start();

    }           //          End of Main


}
