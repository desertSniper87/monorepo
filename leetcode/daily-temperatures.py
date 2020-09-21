class Solution:
    def dailyTemperatures(self, temperatures):
        """
        :type temperatures: List[int]
        :rtype: List[int]
        """
        res = []
        stack = []
        for idx, i in enumerate(temperatures):
            print("Loop", idx)
            if len(stack) is 0 or i<stack[0]:
                print("if len(stack) is 0 or i<stack[-1]:")
                stack.append(i)
                print("i: ", i)
                print("stack: ", stack)
            else:
                print("else")
                stack.pop()
                stack.append(i)
                print("i: ", i)
                print("stack: ", stack)
            

            


        




temperatures = [73, 74, 75, 71, 69, 72, 76, 73]
s = Solution()
print(s.dailyTemperatures(temperatures))
