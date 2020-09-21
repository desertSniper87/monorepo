//Binary Search Tree Program
//Author @Torsho
#include <iostream>
#include <cstdlib>
using namespace std;

class BinarySearchTree
{
    private:
        struct tree_node
        {
           tree_node* left;
           tree_node* right;
           int key;
        };
        tree_node* root;

    public:
        BinarySearchTree()
        {
           root = NULL;
        }

        bool isEmpty() const { return root==NULL; }
        void print_inorder();
        void inorder(tree_node*);
		//void print_preorder();
		//void preorder(tree_node*);
        // void print_postorder();
        // void postorder(tree_node*);
        void insert(int);
        //void remove(int);
        bool search(int);
        int searchCommonAncestor(int, int);
};

void BinarySearchTree::insert(int k)
{
    tree_node* node = new tree_node;
    tree_node* parent;
    node->key = k;
    node->left = NULL;
    node->right = NULL;
    parent = NULL;

    if(isEmpty())
        root = node;
    else
    {
        tree_node* current;
        current = root;
        while(current)
        {
            parent = current;
            if(node->key > current->key)
                current = current->right;
            else current = current->left;
        }

        if(node->key < parent->key)
           parent->left = node;
        else
           parent->right = node;
    }
}

int BinarySearchTree::searchCommonAncestor( int key1, int key2 ) {
    int commonAncestor = -1;
    int ancestor1 = -1;
    bool found = false;
    if(isEmpty())
    {
        cout<<" This Tree is empty! "<<endl;
    }

    tree_node* current;
    tree_node* parent;
    current = root;

    while(current != NULL)
    {
         if(current->key == key1)
         {
            ancestor1 = parent->key;
            cout<<"Test";
            found = true;
            break;
         }

         else
         {
             parent = current;
             if(key1>current->key) current = current->right;
             else current = current->left;
         }
    }
    if(!found)
		 {
        cout<< key1<< "is not found"<< endl;   // Returns false
    }

    /** End of ancestry hund for key1... Should have used separate function**/

    int ancestor2 = -1;

    while(current != NULL)
    {
         if(current->key == key2)
         {
            ancestor2 = parent->key;
            cout<< ancestor2;
            found = true;
            break;
         }
         else
         {
             parent = current;
             if(key2>current->key) current = current->right;
             else current = current->left;
         }
    }
    if(!found)
        {
        cout<< key2<< "is not found"<< endl;   // Returns false
    }

    if (ancestor1==ancestor2) {
        commonAncestor = ancestor1;
    }

    else if (ancestor1==key2) {
        commonAncestor = key2;
    }

    else if (ancestor2==key1) {
        commonAncestor = key1;
    }

    return commonAncestor;     // Returns true

}
/*
void BinarySearchTree::remove(int k)
{
    //Locate the element
    bool found = false;
    if(isEmpty())
    {
        cout<<" This Tree is empty! "<<endl;
        return;
    }

    tree_node* current;
    tree_node* parent;
    current = root;

    while(current != NULL)
    {
         if(current->key == k)
         {
            found = true;
            break;
         }
         else
         {
             parent = current;
             if(k>current->key) current = current->right;
             else current = current->left;
         }
    }
    if(!found)
		 {
        cout<<" key not found! "<<endl;
        return;
    }


		 // 3 cases :
    // 1. We're removing a leaf node
    // 2. We're removing a node with a single child
    // 3. we're removing a node with 2 children

    // Node with single child
    if((current->left == NULL && current->right != NULL)|| (current->left != NULL
&& current->right == NULL))
    {
       if(current->left == NULL && current->right != NULL)
       {
           if(parent->left == current)
           {
             parent->left = current->right;
             delete current;
           }
           else
           {
             parent->right = current->right;
             delete current;
           }
       }
       else // left child present, no right child
       {
          if(parent->left == current)
           {
             parent->left = current->left;
             delete current;
           }
           else
           {
             parent->right = current->left;
             delete current;
           }
       }
     return;
    }

		 //We're looking at a leaf node
		 if( current->left == NULL && current->right == NULL)
    {
        if(parent->left == current) parent->left = NULL;
        else parent->right = NULL;
		 		 delete current;
		 		 return;
    }


    //Node with 2 children
    // replace node with smallest value in right subtree
    if (current->left != NULL && current->right != NULL)
    {
        tree_node* chkr;
        chkr = current->right;
        if((chkr->left == NULL) && (chkr->right == NULL))
        {
            current = chkr;
            delete chkr;
            current->right = NULL;
        }
        else // right child has children
        {
            //if the node's right child has a left child
            // Move all the way down left to locate smallest element

            if((current->right)->left != NULL)
            {
                tree_node* lcurr;
                tree_node* lcurrp;
                lcurrp = current->right;
                lcurr = (current->right)->left;
                while(lcurr->left != NULL)
                {
                   lcurrp = lcurr;
                   lcurr = lcurr->left;
                }
		current->key = lcurr->key;
                delete lcurr;
                lcurrp->left = NULL;
           }
           else
           {
               tree_node* tmp;
               tmp = current->right;
               current->key = tmp->key;
	       current->right = tmp->right;
               delete tmp;
           }

        }
		 return;
    }

}
*/
void BinarySearchTree::print_inorder()
{
  inorder(root);
}

void BinarySearchTree::inorder(tree_node* p)
{
    if(p != NULL)
    {
        if(p->left) inorder(p->left);
        cout<<p->key<<" ";
        if(p->right) inorder(p->right);
    }
    else return;
}
/*
void BinarySearchTree::print_preorder()
{
    preorder(root);
}

void BinarySearchTree::preorder(tree_node* p)
{
    if(p != NULL)
    {
        cout<<" "<<p->key<<" ";
        if(p->left) preorder(p->left);
        if(p->right) preorder(p->right);
    }
    else return;
}

void BinarySearchTree::print_postorder()
{
    postorder(root);
}

void BinarySearchTree::postorder(tree_node* p)
{
    if(p != NULL)
    {
        if(p->left) postorder(p->left);
        if(p->right) postorder(p->right);
        cout<<" "<<p->key<<" ";
    }
    else return;
}
*/
//bool BinarySearchTree::search (int key) {
//    if (root == NULL)
//        return false;
//
//    if (key == this->key)
//        return true;
//    else if (key<this->key) {}
//
//
//}

bool BinarySearchTree::search(int k)
{
    //Locate the element
    bool found = false;
    if(isEmpty())
    {
        cout<<" This Tree is empty! "<<endl;
        return found;   //returns false
    }

    tree_node* current;
    tree_node* parent;
    current = root;

    while(current != NULL)
    {
         if(current->key == k)
         {
            found = true;
            break;
         }
         else
         {
             parent = current;
             if(k>current->key) current = current->right;
             else current = current->left;
         }
    }
    if(!found)
		 {
        return found;   // Returns false
    }


    return found;     // Returns true


}



int main()
{
    BinarySearchTree b;
    char ch;
    int tmp, tmp2;

    while(1){
        cin>> ch;
        if (ch=='I' || ch=='i'){
            cin>> tmp;
            b.insert(tmp);
        }

        else if (ch=='P' || ch=='p'){
            b.print_inorder();
        }

        else if (ch=='S' || ch=='s'){
            cin>> tmp;
            if(b.search(tmp))
                cout<< "Found"<< endl;
            else
                cout<< "Not found"<< endl;
            }

        else if (ch=='C'|| ch=='c'){
            //cout<< "Test";
            cin>> tmp>> tmp2;
            cout<< (b.searchCommonAncestor(tmp, tmp2));
        }


    }

    return 0;
}
