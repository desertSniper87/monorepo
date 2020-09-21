package com.company;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

public class Main implements ActionListener{

    private JFrame frame;
    private JPanel panel;

    private static JLabel timeLabel = new JLabel();

    private JButton startButton = new JButton("Start");
    private JButton pauseButton = new JButton("Pause");
    private JButton endButton = new JButton("End");
    private JButton resumeButton = new JButton("Resume");
    private JButton stopButton = new JButton("Stop");
    private JButton resetButton = new JButton("Reset");

    private JButton greenButton = new JButton("Green");
    private JButton redButton = new JButton("Red");

    private CountDownTimer countDownTimer;

    public Main () {
        setTimerText("      ");
        initGUI();
    }

    private void initGUI() {
        frame = new JFrame();
        panel = new JPanel();

        panel.setLayout(new BorderLayout());
        timeLabel.setBorder(BorderFactory.createRaisedBevelBorder());
        panel.add(timeLabel, BorderLayout.CENTER);

        startButton.addActionListener(this);
        pauseButton.addActionListener(this);
        resumeButton.addActionListener(this);
        endButton.addActionListener(this);
        stopButton.addActionListener(this);
        resetButton.addActionListener(this);
        greenButton.addActionListener(this);
        redButton.addActionListener(this);

        JPanel commandPanel = new JPanel();
        commandPanel.setLayout(new GridLayout());

        commandPanel.add(startButton);
        commandPanel.add(pauseButton);
        commandPanel.add(resumeButton);
        commandPanel.add(stopButton);
        commandPanel.add(resetButton);

        panel.add(commandPanel, BorderLayout.SOUTH);

        JPanel colorPanel = new JPanel();
        colorPanel.setLayout(new GridLayout(0,1));
        
        colorPanel.add(redButton);
        colorPanel.add(greenButton);

        panel.add(colorPanel, BorderLayout.EAST);

        frame.setContentPane(panel);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
        frame.pack();

        Font font = new
        frame.setFont("Verdana", Font.BOLD, 12);

        countDownTimer = new CountDownTimer();
    }

    public static void setTimerText(String stringTime) {
        timeLabel.setText(stringTime);
    }

    private void setTimerColor(Color stringColor) {
        timeLabel.setForeground(stringColor);
    }

    @Override
    public void actionPerformed(ActionEvent event){
        JButton button = (JButton) event.getSource();

        if (button.equals(greenButton))
            setTimerColor(Color.GREEN.darker());
        else if (button.equals(redButton))
            setTimerColor((Color.RED.darker()));

        else if (button.equals(startButton))   { countDownTimer.start(); }
        else if (button.equals(pauseButton))   { countDownTimer.pause(); }
        else if (button.equals(resumeButton))  { countDownTimer.resume(); }
        else if (button.equals(stopButton))    { countDownTimer.stop(); }
        else if (button.equals(resetButton))   { countDownTimer.reset(); }
    }

    public static void main(String[] args) {
	// write your code here
        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                new Main();
            }
        });

    }

}
