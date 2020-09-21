#include <iostream>
#include <cstdlib>
#include <cstdio>
using namespace std;

//bool isEmpty(tree_node* root);
//void insert(int k);





int main(){
    struct tree_node{
        tree_node *left;
        tree_node *right;
        int key;
    };


    void insert (int k) {
    tree_node* node = new tree_node;
    tree_node *parent;

    node->key = k;
    node->left = nullptr;
    node->right = nullptr;

    parent = nullptr;

    //Check if we are creating a new tree

    if (isEmpty(root))
        root = node;
    else() {
        tree_node * current;
        current = root;
    }

    while(current) {
        current = root;
        if (node->key > current->key) {
            current = current->right;
        else
            current = current->left;
        }
    }

    if (node->key<parent->key) {
        parent->left = node;
    else
        parent->right = node;
    }
}




    tree_node *root;

    return 0;
}



bool isEmpty(root) {
    if (root == nullptr)
        return true;
    else
        return false;
}

