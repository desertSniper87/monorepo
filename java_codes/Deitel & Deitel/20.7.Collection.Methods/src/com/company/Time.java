package com.company;

/**
 * Created by Torsho on 27-Nov-16.
 */
public class Time {
    private int hour;
    private int miniute;
    private int second;

    public int getHour() {
        return hour;
    }

    public int getMiniute() {
        return miniute;
    }

    public int getSecond() {
        return second;
    }

    public Time (int hour, int miniute, int second){
        this.hour = hour;
        this.miniute = miniute;
        this.second = second;
    }
}
