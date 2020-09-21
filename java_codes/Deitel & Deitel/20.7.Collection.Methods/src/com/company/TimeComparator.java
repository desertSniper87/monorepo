package com.company;

import java.util.Comparator;

/**
 * Created by Torsho on 27-Nov-16.
 */
public class TimeComparator implements Comparator<Time> {
    public int compare (Time time1, Time time2){
        int hourCompare = time1.getHour()-time2.getHour();
        if (hourCompare!=0) return hourCompare;

        int miniuteCompare = time1.getMiniute()-time2.getMiniute();
        if (miniuteCompare!=0) return miniuteCompare;

        int secondCompare = time1.getSecond()-time2.getSecond();
        return secondCompare;
    }
}
