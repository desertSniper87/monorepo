class Solution:
    def transpose(self, A):
        """
        :type A: List[List[int]]
        :rtype: List[List[int]]
        """
        # z = zip(*A)
        # for j in z:
            # print(j)

        return list(list(i) for i in zip(*A))

        
x = [[1,2,3],[4,5,6],[7,8,9]]
s = Solution()
print(s.transpose(x))
        
