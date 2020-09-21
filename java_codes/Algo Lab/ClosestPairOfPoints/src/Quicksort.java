/**
 * Created by Torsho on 16-Oct-16.
 */

public class Quicksort  {
    private Point[] point_array;
    private int number;

    public void sort(Point[] values) {
        // check for empty or null array
        if (values ==null || values.length==0){
            return;
        }
        this.point_array = values;
        number = values.length;
        quicksort(0, number - 1);
    }

    public void quicksort(int low, int high) {
        int i = low, j = high;
        // Get the pivot element from the middle of the list
        double pivot = point_array[low + (high-low)/2].getX();

        // Divide into two lists
        while (i <= j) {
            // If the current value from the left list is smaller then the pivot
            // element then get the next element from the left list
            while (point_array[i].getX() < pivot) {
                i++;
            }
            // If the current value from the right list is larger then the pivot
            // element then get the next element from the right list
            while (point_array[j].getX() > pivot) {
                j--;
            }

            // If we have found a values in the left list which is larger then
            // the pivot element and if we have found a value in the right list
            // which is smaller then the pivot element then we exchange the
            // values.
            // As we are done we can increase i and j
            if (i <= j) {
                exchange(i, j);
                i++;
                j--;
            }
        }
        // Recursion
        if (low < j)
            quicksort(low, j);
        if (i < high)
            quicksort(i, high);
    }

    private void exchange(int i, int j) {
        Point temp = point_array[i];
        point_array[i] = point_array[j];
        point_array[j] = temp;
    }
}