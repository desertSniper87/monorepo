import itertools
# nums = [2, 7, 11, 15]
# target = 9
nums = [3,3]
target = 6
class Solution:
    def twoSum(self, nums, target):
        """
        :type nums: List[int]
        :type target: int
        :rtype: List[int]
        """

        # for i in itertools.combinations(nums, 2):
            # if sum(i) == target:
                # if i[0]!=i[1]:
                    # return nums.index(i[0]), nums.index(i[1])
                # else:
                    # return [j for j, x in enumerate(nums) if x==i[1]]

        for i, x in enumerate(nums):
            for j, y in enumerate(nums):
                if x+y == target:
                    if i==j:
                        pass
                    else :
                        return i, j

s = Solution()
x = s.twoSum(nums, target)
print(x)
