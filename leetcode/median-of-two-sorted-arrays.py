import statistics as s

class Solution:
    def findMedianSortedArrays(self, nums1, nums2):
        """
        :type nums1: List[int]
        :type nums2: List[int]
        :rtype: float
        """
        try:
            return (s.median(nums1) + s.median(nums2)) / 2
        except s.StatisticsError:
            if nums1==[]:
                return s.median(nums2)
            elif nums2==[]:
                return s.median(nums1)

so = Solution()

nums1 = [1, 3]
nums2 = [2]
print(so.findMedianSortedArrays(nums1, nums2))

nums1 = [1, 2]
nums2 = [3, 4]
print(so.findMedianSortedArrays(nums1, nums2))

nums1 = []
nums2 = [1]
print(so.findMedianSortedArrays(nums1, nums2))
