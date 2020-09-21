package cloudproject.transcoding;

/**
 * Created by Torsho on 15-Feb-17.
 */
public class CloudletDeadlineComparator implements java.util.Comparator<VideoSegment> {
    public int compare(VideoSegment c1, VideoSegment c2) {
        return Double.compare(c1.getCloudletDeadline(), c2.getCloudletDeadline());
    }
}
