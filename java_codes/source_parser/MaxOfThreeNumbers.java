import java.util.Scanner;
public class MaxOfThreeNumbers {
    public static void main(String[] args) {
        int option = 0;
        int i;
        int[] numbers = new int[3];

        while (option!=3){
            System.out.println("Please choose an option and press enter:\n");
            System.out.println("1. Read 3 numbers\n 2. Print the max\n 3.Exit\n");

            Scanner scan = new Scanner(System.in);
            option = scan.nextInt();

            if (option==1) {
                for (i=0; i<3; i++) {
                    System.out.printf("\nnumbers[%d]=",i+1);
                    numbers[i] = scan.nextInt();
                }
            } else if (option==2) {
                int max = 0;
                for (i=0; i<3; i++) {
                    if(numbers[i] > max) {
                        max = numbers[i];
                    }
                }
                System.out.printf("\nMax=%d",max);
            }
        }


    }
}


