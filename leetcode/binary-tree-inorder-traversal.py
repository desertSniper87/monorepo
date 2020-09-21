# Definition for a binary tree node.
# class TreeNode(object):
#     def __init__(self, x):
#         self.val = x
#         self.left = None
#         self.right = None

class Solution(object):
    def inorderTraversal(self, root, l=[]):
        """
        :type root: TreeNode
        :rtype: List[int]
        """
        if (root.left!=None):
            Solution.inorderTraversal(self, root.left, l)
        if (root.right!=None):
            Solution.inorderTraversal(self, root.right, l)
        l.append(root.val)

        return l

        

        
        
        
