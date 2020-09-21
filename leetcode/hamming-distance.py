class Solution:
    def hammingDistance(self, x, y):
        """
        :type x: int
        :type y: int
        :rtype: int
        """
        a = "{0:031b}".format(x)
        b = "{0:031b}".format(y)

        diff = 0
        print(a, b)
        for i, j in zip(a, b):
            print(i, j)
            if i != j:
                diff += 1

        return diff

        
s = Solution()

x, y = 1, 2
print(s.hammingDistance(x, y))


