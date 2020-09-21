package cloudproject.transcoding;


import org.cloudbus.cloudsim.Cloudlet;
import org.cloudbus.cloudsim.lists.VmList;

import java.util.Comparator;
import java.util.List;

/**
 * Created by Torsho on 08-Feb-17.
 */
public class WaitingListComparator implements Comparator<VideoSegment> {
    @Override
    public int compare(VideoSegment c1, VideoSegment c2) {

        return Long.compare(c1.getCloudletLength(), c2.getCloudletLength());
    }
}
