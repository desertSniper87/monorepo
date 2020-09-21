package com.company;

import java.util.*;

public class Main {

    public static void main(String[] args) {
	// write your code here
        List <String> items = new ArrayList<String>();

        items.add("Green");
        items.add(0, "Yellow");

        //items.remove("Green");

        //items.add();

        for (int i=0;i<items.size();i++) {
            System.out.printf("%s\n", items.get(i));
        }

        display(items);

        if (items.contains("Green"))
            System.out.println("The items contains Green\n");
    }

    public static void display(List <String> items) {
        for (String item : items){
            System.out.printf("%s\n", item);
        }
    }
}

