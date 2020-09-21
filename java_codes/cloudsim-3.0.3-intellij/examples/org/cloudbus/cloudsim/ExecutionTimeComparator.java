package org.cloudbus.cloudsim;

import java.util.Comparator;

/**
 * Created by Torsho on 09-Feb-17.
 */
public class ExecutionTimeComparator implements Comparator<Cloudlet> {
    @Override
    public int compare(Cloudlet c1, Cloudlet c2) {
        return Double.compare(c1.getExecutionTime(), c2.getExecutionTime());
    }
}
