class Solution:
    def permute(self, nums, i=0):
        """
        :type nums: List[int]
        :rtype: List[List[int]]
        """
        i += 1
        return [nums[i:], nums[:i]]


        
s = Solution()
print(s.permute([1, 2, 3])) 

