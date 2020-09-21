# class Solution:
    # def lengthOfLongestSubstring(self, s):
        # """
        # :type s: str
        # :rtype: int
        # """
        # sum = ""
        # l = []
        # if s=="":
            # return 0

        # for i in s:
            # if i not in sum:
                # sum += i
            # else:
                # l.append(sum)
                # if sum[-1]!=i:
                    # sum = sum[-1] + i
                # else:
                    # sum = i
                # print(sum)

        # l.append(sum)

        # print(l)
        # print(sum)
        # if l==[]:
            # return len(sum)
        # return len(max(l, key=len))

class Solution:
    def lengthOfLongestSubstring(self, s):
        """
        :type s: str
        :rtype: int
        """
        max = 0
        start = 0
        end  = 1

        dict = {}

        while(end!=len(s)):
            if s[end] != s[start]:
                end += 1
            else:
                print(s[start:end])
                if max<(end - start):
                    max = end - start
                start = end - 1
                end = end + 1

        # print(max)
        return max



s = Solution()

i = "abcabcbb"
print(s.lengthOfLongestSubstring(i))
i = "bbbbb"
print(s.lengthOfLongestSubstring(i))
i = "pwwkew"
print(s.lengthOfLongestSubstring(i))
i = "c"
print(s.lengthOfLongestSubstring(i))
i = "aab"
print(s.lengthOfLongestSubstring(i))
i = "dvdf"
print(s.lengthOfLongestSubstring(i))
