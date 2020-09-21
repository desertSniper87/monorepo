from unittest import TestCase 

class Solution:
    def maxIncreaseKeepingSkyline(self, grid):
        """
        :type grid: List[List[int]]
        :rtype: int
        """

        left_right = list(max(l) for l in grid)
        print("left_right: ", left_right)
        top_bottom = list(max(l) for l in zip(*grid))
        print("top_bottom: ", top_bottom)

        print()
        l = len(grid)
        res = [[None for _ in range(l)] for _ in range(l)]

        for ix, i in enumerate(grid):
            for jx, j in enumerate(i):
                # print("ix, jx, grid[ix][jx], left_right[jx], top_bottom[ix]: ", ix, jx, grid[ix][jx], left_right[jx], top_bottom[ix])
                res[ix][jx] = max(grid[ix][jx], \
                        min(left_right[ix], top_bottom[jx]))

        print("grid: ", grid)
        print("res: ", res)

        s = 0
        for (x, y) in zip(grid, res):
            print("x y: ", x, y)
            s += sum([a - b for (a, b) in zip(y, x)])
            print(s)

        # print(res)
        return s

        

s = Solution()
grid = [[3,0,8,4],[2,4,5,7],[9,2,6,3],[0,3,1,0]]
gridNew = [ [8, 4, 8, 7],
            [7, 4, 7, 7],
            [9, 4, 8, 7],
            [3, 3, 3, 3] ]
Test = TestCase()
try:
    Test.assertEqual(s.maxIncreaseKeepingSkyline(grid), gridNew)
except Exception as e:
    print(e)

