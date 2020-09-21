package com.company;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

public class Main {
    public static void main(String[] args) throws IOException{
	// write your code here

        //String splitBy = ",";
        BufferedReader br = new BufferedReader(new FileReader("data.csv"));
        String line = br.readLine();
        Data data;
        while((line = br.readLine()) != null) {
            //String x
            String[] x = line.split(",");
            System.out.println(x);
            //System.out.println(line);

        }
    }
};

class Instances {

        };

class Node {
    Node parent;
    Node children[];

    int testFts;
    int numberOfFts;

    //List<Instances> instances;

    int predictedLabel = -1;

    //Node ()
    //};
}
/*class Entropy {
    public static double calculateEntropy(ArrayList<Record> data){
        double entropy = 0;

        if (data.size() == 0) {
            System.out.println("Nothing to do");
        }
    }
};*/

class Data {
    ArrayList<Integer> attributes;
    int decision;
    Data(ArrayList<Integer> attributes, int i){
        this.attributes = attributes;
        decision = i;
    }
};