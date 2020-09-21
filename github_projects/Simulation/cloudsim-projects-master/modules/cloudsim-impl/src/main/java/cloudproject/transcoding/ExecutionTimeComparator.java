package cloudproject.transcoding;
import org.cloudbus.cloudsim.Vm;
import org.cloudbus.cloudsim.lists.VmList;

import java.util.List;

/**
 * Created by Torsho on 09-Feb-17.
 */
public class ExecutionTimeComparator implements java.util.Comparator<VideoSegment> {
    public int compare(VideoSegment c1, VideoSegment c2) {
        return Double.compare(c1.executionTime, c2.executionTime);
    }
}
