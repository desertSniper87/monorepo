package com.company;

import java.io.*;
import java.util.Random;

/**
 * Created by Torsho on Nov-16.
 */
public class Node {
    public static int[][] assignment;

    public static int number_of_machines;
    public static int number_of_jobs;
    public static int maximum_iteration;
    public static int optimum_solution;

    public static double temparature;
    public static double initial_temparature;
    public static double terminal_temparature;
    public static double cooldown;

    public static int minimum_span;
    public static int current_span;
    public static int[][] table;
    public static int[] order;
    public static int I;              /** I and J are 2 integers to keep track of Order[] **/
    public static int J;
    public static int[] temp_order;
    public static int[] timebar;



    public Node(int[][] assignment, int number_of_machines, int number_of_jobs){
        this.assignment = assignment;
        this.number_of_jobs = number_of_jobs;
        this.number_of_machines = number_of_machines;
        order = new int[number_of_jobs];

        table = new int[number_of_jobs][number_of_machines];
        for (int i = 0; i < number_of_jobs; i++) {
            for (int j = 0; j < number_of_machines; j++) {
                table[i][j] = assignment[i][j];
            }
        }
        init();
    }

    public void init() {
        for (int i = 0; i < number_of_jobs; i++) {
            order[i] = i;
            //timebar[i] = 0;
        }

        for (int i = number_of_jobs-1; i >=0 ; i--) {
            Random random = new Random();
            int r = Math.abs(random.nextInt())%number_of_jobs;

            int temp = order[r];
            order[r] = order[i];
            order[i] = temp;
        }

        initial_temparature = 10000;
        terminal_temparature = 1;
        cooldown = 0.75;
        maximum_iteration = 20;
        optimum_solution = 99999;
        minimum_span = 99999;
        current_span = 99999;
    }

    private void reset(){
        for (int i = 0; i < number_of_jobs ; i++) {
            order[i] = i;
        }

        for (int j = 0; j < number_of_machines ; j++) {
            timebar[j] = 0;
        }

        initial_temparature = 10000;
        terminal_temparature = 1000;
        cooldown = 0.85;
        maximum_iteration = 20;
        optimum_solution = 99999;
        minimum_span = 99999;
        current_span = 99999;
    }

    public boolean accept(int current, int neighbour) throws FileNotFoundException {
        double r = Math.random();
        double delta;
        PrintStream file = new PrintStream(new FileOutputStream("neighbour_comparison.txt",true));

        delta = Math.exp((current-neighbour)/temparature);
        file.print(current+"< - - - - - - - >"+ neighbour +" "+ r +" "+ delta);

        if (delta>r){
            file.println(" \tAccept");
            return true;
        }
        else
            file.println(" \tReject");
            return false;
    }

    public int simulated_annealing() throws FileNotFoundException {
        int initial_span;
        timebar = new int[number_of_machines];
        temp_order = new int[number_of_jobs];
        minimum_span = get_maximum_span();

        PrintStream file = new PrintStream(new FileOutputStream("output.txt"));
        PrintWriter writer = new PrintWriter("neighbour_comparison.txt");
        writer.println("Current - - - - - - Neighbour Random Value          Delta");
        writer.close();

        temparature = initial_temparature;
        int iteration_count;

        for (iteration_count=0;temparature>=terminal_temparature;iteration_count++){
            initial_span = gererate_neighbour();
            if (accept(current_span, initial_span)){
                current_span = initial_span;
                if(current_span<minimum_span){
                    minimum_span = current_span;
                                                    // TODO: Write a swap func.. err.. method
                    int temp = order[I];
                    order[I] = order[J];
                    order[J] = temp;
                }

                file.println(current_span);
            }
            if (iteration_count== maximum_iteration){
                temparature = temparature*cooldown;
                iteration_count = 0;
            }
        }
        return minimum_span;
    }

    public int get_maximum_span(){
        int maximum_span;
        for (int i = 0; i < number_of_machines; i++) {
            timebar[i] = 0;
        }

        for ( int i=0;i<number_of_jobs;i++ ){
            timebar[0] += (table[order[i]][0]);
            maximum_span = timebar[0];

            if (maximum_span<timebar[1])
                maximum_span = timebar[1];

            for (int j = 1; j < number_of_machines; j++) {
                if (maximum_span<timebar[j])
                    maximum_span = timebar[j];

                timebar[j] = maximum_span+table[order[i]][j];

                if (maximum_span<timebar[j])
                    maximum_span = timebar[j];
            }
        }
        return timebar[number_of_machines-1];
    }

    public int get_neighbour_span(){
        int max_span;
        for (int i = 0; i < number_of_machines; i++) {
            timebar[i] = 0;
        }

        for ( int i=0;i<number_of_jobs;i++ ){
            timebar[0] += (table[temp_order[i]][0]);
            max_span = timebar[0];

            if (max_span<timebar[1])
                max_span = timebar[1];

            for (int j = 1; j < number_of_machines; j++) {
                if (max_span<timebar[j])
                    max_span = timebar[j];

                timebar[j] = max_span+table[temp_order[i]][j];

                if (max_span<timebar[j])
                    max_span = timebar[j];
            }
        }
        return timebar[number_of_machines-1];
    }

    public int gererate_neighbour(){
        int a, b;
        Random random = new Random();
        a = Math.abs(random.nextInt())%number_of_jobs;
        b = Math.abs(random.nextInt())%number_of_jobs;

        for (int i = 0; i < number_of_jobs; i++) {
            temp_order[i] = order[i];                       //TODO:Stop manual array copy
        }

        if (a!=b) {
            int temp = temp_order[b];
            temp_order[b] = temp_order[a];
            temp_order[a] = temp;

            I = a;
            J = b;
        }

        return get_neighbour_span();
    }
}
