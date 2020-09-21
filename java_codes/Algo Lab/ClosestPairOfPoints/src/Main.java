import com.sun.org.apache.xpath.internal.SourceTree;

import java.lang.Integer.*;
import java.util.Arrays;
import java.util.Arrays.*;
import java.util.Collections;
import java.util.Scanner;

import static java.lang.Math.abs;

public class  Main {

    private static double distance (Point a, Point b) {
        return Math.sqrt(Math.pow(((a.getX()-b.getX())),2)+(Math.pow(((a.getY()-b.getY())),2)));
    }

    private static double bruteForce(Point[] P, int n){
        double max = Integer.MAX_VALUE;
        for (int i = 0;i<n;i++){
            for (int j=i+1;j<n;j++){
                if (distance(P[i], P[j])<max)
                    max = distance(P[i], P[j]);
            }//     End of Inner loop
        }//     End of Outer loop
        return max;
    }

    public static double minimum(double x, double y) {
        return (x<y) ? x : y;
    }

    public static double stripClosest(Point[] strip, int size, double d) {
        double min = d;

        for (int i=0;i<size;i++) {
            for (int j=i+1;j<size && (strip[j].getY()-strip[i].getY())<min;j++) {
                if (distance(strip[i],strip[j])<min)
                    min = distance(strip[i],strip[j]);
            }//     End of inner loop
        }//     End of outer loop

        return min;
    }

    public static double recursivelyFindClosest(Point[] Px, Point[] Py, int n)         //      Px sorted by X-coord and Py vice versa.
    {
        if (n <= 3)
            return bruteForce(Px, n);

        int mid = n/2;
        Point midPoint = Px[mid];


        //Point[] pointOnLeftSideOfTheLine = new Point[mid+1];
        Point[] pointOnLeftSideOfTheLine = new Point[n];   // y sorted points on left of vertical line
        //Point[] pointOnRightSideOfTheLine = new Point[n-mid-1];  // y sorted points on right of vertical line
        Point[] pointOnRightSideOfTheLine = new Point[n];
        int li = 0, ri = 0;  // indexes of left and right subarrays
        for (int i = 0; i < n; i++)
        {
            if (Py[i].getX() <= midPoint.getX())
                pointOnLeftSideOfTheLine[li++] = Py[i];
            else
                pointOnRightSideOfTheLine[ri++] = Py[i];
        }



        double dl = recursivelyFindClosest(Px, pointOnLeftSideOfTheLine, mid);
        double dr = recursivelyFindClosest(Px, pointOnRightSideOfTheLine, n-mid);

        double d = minimum(dl, dr);

        Point[] strip = new Point[n];
        int j = 0;
        for (int i = 0; i < n; i++)
            if (abs(Py[i].getX() - midPoint.getX()) < d) {
                strip[j] = Py[i];
                j++;
            }

        return minimum(d, stripClosest(strip, j, d) );
    }


    public static double closest(Point[] P, int n)
    {
        Point[] Px = new Point[n];
        Point[] Py = new Point[n];
        for (int i = 0; i < n; i++)
        {
            Px[i] = P[i];
            Py[i] = P[i];
        }


        Quicksort sorter = new Quicksort();
        sorter.sort(Px);
        sorter.sort(Py);

        return recursivelyFindClosest(Px, Py, n);
    }


    public static void main(String[] args) {
        //Point a = new Point(1.2,2.1);
        //Point b = new Point(3.1,5.8);

        //int[][] point_array = new int[5][2]
                //{(2, 3), (12, 30), (40, 50), (5, 1), {12, 10}, {3, 4}};

//        int [][] point_array = new int[][] {
//                {2, 3}, {12, 30}, {40, 50}, {5, 1}, {12, 10}, {3, 4}
//        };
//
//
//        Point[] P = new Point[6];
//        for( int i=0; i<6; i++ )
//            P[i] = new Point(point_array[i][0], point_array[i][1]);
//        int n = 6;
//        System.out.println("The smallest distance is "+ closest(P, n));

        int test_cases, cur_case = 1;
        int integer_input;
        Scanner sc = new Scanner(System.in);

        test_cases = sc.nextInt();

        while (test_cases!=0){
            integer_input = sc.nextInt();
            Point[] P = new Point[integer_input];
            for (int i=0;i<integer_input;i++){
                P[i] = new Point(sc.nextDouble(), sc.nextDouble());
            }
            System.out.println("Case "+ cur_case + " #: " + closest(P, integer_input));

            test_cases--;
            cur_case++;
        }

    }

}