package com.company;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

/**
 * Created by Torsho on 15-May-16.
 */
public class CountDownTimer implements ActionListener {
    private static final int ONE_SECOND = 1000;
    private int count = 0;
    private boolean isTimerActive = false;
    private Timer timer = new Timer(ONE_SECOND, this);
    //private Main main;
    public CountDownTimer() {
        count = 0;                         ///The counting is in seconds. It is converted to hh:mm:ss
        Main.setTimerText(TimeFormat(count));
    }

    @Override
    public void actionPerformed(ActionEvent event) {
        if (isTimerActive) {
            count++;
            Main.setTimerText(TimeFormat(count));
        }
    }

    public void start() {
        count = 0;
        isTimerActive = true;
        timer.start();
    }

    public void stop() {
        timer.stop();
    }

    public void pause() {
        isTimerActive = false;
    }

    public void resume() {
        isTimerActive = true;
        timer.restart();
    }

    public void reset() {
        count = 0;
        isTimerActive = true;
        timer.restart();
    }


    private String TimeFormat(int count) {
        int hours = count / 3600;
        int minutes = count / 60;
        int seconds = count - minutes*60;

        return String.format("%02d", hours) + " : " + String.format("%02d", minutes) + " : " + String.format("%02d", seconds);
    }
}
