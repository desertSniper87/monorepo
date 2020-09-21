class Solution:
    def longestUnivaluePath(self, root, l=[], res=0):
        print("res: ", res)

        try:
            print("root.val: ", root.val)
        except Exception as e:
            print ("e: ", e)

        if root == None:
            return

        if root.left!=None and root.left.val == root.val:
            print("going left")
            res += 1
            self.longestUnivaluePath(root.left, res=res)

        if root.right!=None and root.right.val == root.val:
            print("going right")
            res += 1
            self.longestUnivaluePath(root.right, res=res)

        else:
            l.append(res)
            res = 0
            self.longestUnivaluePath(root.left, res=res)
            self.longestUnivaluePath(root.right, res=res)

        if l==[]:
            return 0

        return max(l)


