package com.company;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Main {

    public static void main(String[] args) throws FileNotFoundException {
        //Scanner scanner = new Scanner(new File("Carlier 7x7 instance.txt"));
        //Scanner scanner = new Scanner(new File("20x20 instance.txt"));
        Scanner scanner = new Scanner(new File("Carlier 14x4 instance.txt"));
        //Scanner scanner = new Scanner(new File("4x3 instance.txt"));

        while (scanner.hasNext()){
            int number_of_jobs = scanner.nextInt();
            int number_of_machines = scanner.nextInt();

            int[][] processing_queue = new int[number_of_machines][number_of_jobs];

            for (int i = 0; i < number_of_machines; i++) {
                for (int j = 0; j < number_of_jobs; j++) {
                        scanner.nextInt();
                        processing_queue[i][j] = scanner.nextInt();
                }
            }

            Node node = new Node(processing_queue, number_of_jobs, number_of_machines);

            System.out.println(node.simulated_annealing());

//            for (int i = 0; i < number_of_machines; i++) {
//                for (int j = 0; j < number_of_jobs; j++) {
//                    System.out.print(processing_queue[i][j]+" ");
//                }
//                System.out.println();
//            }
        }
    }
}
