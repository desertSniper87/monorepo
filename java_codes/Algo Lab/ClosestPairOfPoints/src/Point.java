import java.lang.*;
import java.util.Comparator;

/**
 * Created by Torsho on 15-Oct-16.
 */
public class Point {
    private double x;
    private double y;

    public Point(double x, double y){
        this.x = x;
        this.y = y;
    }

    public double getX () {
        return this.x;
    }

    public double getY () {
        return this.y;
    }

    public double compareX (Point a, Point b) { return a.x - b.x; }

    public double compareY (Point a, Point b) {
        return a.y - b.y;
    }
}
