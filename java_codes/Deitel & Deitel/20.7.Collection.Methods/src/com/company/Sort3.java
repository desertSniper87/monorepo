package com.company;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Sort3 {

    public static void main(String[] args) {
	// write your code here
        List<Time> list = new ArrayList<Time>();

        list.add(new Time(1, 2, 3));
        list.add(new Time(22, 33, 33));
        list.add(new Time(12, 33, 32));
        list.add(new Time(5, 33, 3));
        list.add(new Time(8, 2, 3));
        list.add(new Time(14, 2, 3));

        for (Time t: list)
            System.out.printf("The time is now %d:%d:%d\n",t.getHour(),t.getMiniute(),t.getSecond());

        System.out.println("Sorting...");
        Collections.sort(list, new TimeComparator());

        for (Time t: list)
            System.out.printf("The time is now %d:%d:%d\n",t.getHour(),t.getMiniute(),t.getSecond());
    }
}
