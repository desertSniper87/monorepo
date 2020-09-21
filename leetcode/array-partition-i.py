class Solution:
    def arrayPairSum(self, nums):
        """
        :type nums: List[int]
        :rtype: int
        """

        sum = 0
        nums.sort()
        # n1 = nums[:len(nums)//2]
        # n2 = nums[len(nums)//2:]
        # print(n1, n2)
        for i, j in zip(nums[0::2], nums[1::2]):
            sum += min(i, j)
            print(i, j)

        return sum
        
x = [1, 4, 3, 2]
s = Solution()
r = s.arrayPairSum(x)
print(r)
