# Definition for a binary tree node.
class TreeNode:
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

class Solution:
    def createNode(self, node):
        return TreeNode(node)

    def makeTree(self, l):
        x = None
        for i in l:
            x = self.insert(x, i)

        return x

    def insert(self, treeNode, newNodeData):
        if treeNode==None:
            return self.createNode(newNodeData)
        if (newNodeData<treeNode.val):
            treeNode.left = self.insert(treeNode.left, newNodeData)
        else:
            treeNode.right = self.insert(treeNode.right, newNodeData)

        return treeNode

    def uniVal(self, treeNode, l=[], res=0):
        print("res: ", res)
        print("l: ", l)
        # __import__('pudb').set_trace()


        if treeNode == None:
            # __import__('pudb').set_trace()
            return

        if treeNode.left!=None and treeNode.left.val == treeNode.val:
            print("Going left")
            if treeNode.right!=None and treeNode.left.val == treeNode.right.val:
                print("Left and Right value same")
                res += 1
                self.uniVal(treeNode.right, res=res, l=l)
            res += 1
            self.uniVal(treeNode.left, res=res, l=l)

        if treeNode.right!=None and treeNode.right.val == treeNode.val:
            print("Going right")
            if treeNode.left!=None and treeNode.left.val == treeNode.right.val:
                print("Left and Right value same")
                res += 1
                self.uniVal(treeNode.left, res=res, l=l)
            res += 1
            self.uniVal(treeNode.right, res=res, l=l)

        else:
            try:
                print("treeNode.val: ", treeNode.val)
            except Exception as e:
                print ("e: ", e)
            l.append(res)
            print("l: ", l)
            res = 0
            self.uniVal(treeNode.left, res=res, l=l)
            self.uniVal(treeNode.right, res=res, l=l)

        try: 
            return max(l)
        except ValueError:
            return 0



    def inorderTraversal(self, root, l=[]):
        """
        :type root: TreeNode
        :rtype: List[int]
        """
        if (root.left!=None):
            self.inorderTraversal(root.left, l)
        l.append(root.val)
        if (root.right!=None):
            self.inorderTraversal(root.right, l)

        return l

    def longestUnivaluePath(self, root):
        """
        :type root: TreeNode
        :rtype: int
        """
        if root==None:
            return 0
        return self.uniVal(root, l=[], res=0)

xx = [[5,4,5,1,1,5],\
        [1],\
        [],\
        [1,2,2,2,2,2,2,2],\
        [1,4,5,4,4,5],\
        ]

for x in xx:
    t = Solution()
    root = t.makeTree(x)
    print("t.longestUnivaluePath(root): ", t.longestUnivaluePath(root))
    # print("t.uniVal(root, l=[], res=0): ", t.uniVal(root, l=[], res=0))
    # print("t.inorderTraversal(root): ", t.inorderTraversal(root))
