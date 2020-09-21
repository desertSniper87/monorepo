package org.cloudbus.cloudsim.util;

import org.cloudbus.cloudsim.Cloudlet;
import org.cloudbus.cloudsim.ResCloudlet;

import java.util.Comparator;
import java.util.List;

/**
 * Created by Torsho on 26-Nov-16.
 */
public class WaitingListComparator implements Comparator<Cloudlet> {
    @Override
    public int compare(Cloudlet c1, Cloudlet c2) {
        return Long.compare(c1.getCloudletLength(), c2.getCloudletLength());
    }
}
