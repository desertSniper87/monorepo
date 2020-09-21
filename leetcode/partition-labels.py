from collections import OrderedDict
class Solution:
    def partitionLabels(self, S):
        """
        :type S: str
        :rtype: List[int]
        """
        first = OrderedDict()
        last = {}
        result = []
        for idx, i in enumerate(S):
            if i not in first.keys():
                first[i] = idx
            last[i] = idx
        
        print("first: ", first)
        print("last: ", last)
        stack = []

        for i in first:
            print(stack)
            if len(stack) == 0:
                stack.append(i)
            elif i not in stack:
                if max(last[i] for i in stack) > first[i]:
                    stack.append(i)
                else:
                    result.append((max(last[i] for i in stack) 
                                 - min(first[i] for i in stack)) + 1)
                    print(" ".join(k for k in stack))
                    stack = [i]
        result.append((max(last[i] for i in stack) 
                     - min(first[i] for i in stack)) + 1)

        return result
            


s = Solution()
x = "ababcbacadefegdehijhklij"
# x = "caedbdedda"
print(s.partitionLabels(x))
