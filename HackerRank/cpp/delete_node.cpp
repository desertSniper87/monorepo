#include <bits/stdc++.h>

using namespace std;

class SinglyLinkedListNode {
    public:
        int data;
        SinglyLinkedListNode *next;

        SinglyLinkedListNode(int node_data) {
            this->data = node_data;
            this->next = nullptr;
        }
};

class SinglyLinkedList {
    public:
        SinglyLinkedListNode *head;
        SinglyLinkedListNode *tail;

        SinglyLinkedList() {
            this->head = nullptr;
            this->tail = nullptr;
        }

        void insert_node(int node_data) {
            SinglyLinkedListNode* node = new SinglyLinkedListNode(node_data);

            if (!this->head) {
                this->head = node;
            } else {
                this->tail->next = node;
            }

            this->tail = node;
        }
};

void print_singly_linked_list(SinglyLinkedListNode* node, string sep, ofstream& fout) {
    while (node) {
        fout << node->data;

        node = node->next;

        if (node) {
            fout << sep;
        }
    }
}

void free_singly_linked_list(SinglyLinkedListNode* node) {
    while (node) {
        SinglyLinkedListNode* temp = node;
        node = node->next;

        free(temp);
    }
}// Complete the deleteNode function below.

/*
 * For your reference:
 *
 * SinglyLinkedListNode {
 *     int data;
 *     SinglyLinkedListNode* next;
 * };
 *
 */
SinglyLinkedListNode* deleteNode(SinglyLinkedListNode* head, int position) {
    if (head == NULL) {
        return head;
    }

    SinglyLinkedListNode * root = head;

    if (position == 0) {
        SinglyLinkedListNode * next = head->next;
        free(root);
        return next;
        
    }

    for (int i=0; i<position-1; i++) {
        head=head->next;
    }

    SinglyLinkedListNode * freeNode = head->next;
    head->next = head->next->next;
    free(freeNode);
    
    return root;
}

int main()
{
    ofstream fout("./output.txt");

    SinglyLinkedList* llist = new SinglyLinkedList();

    int llist_count;
    ifstream outfile;
    outfile.open("delete_node_input_2.txt", ios::in);
    outfile >> llist_count;
    outfile.ignore(numeric_limits<streamsize>::max(), '\n');
    

    for (int i = 0; i < llist_count; i++) {
        int llist_item;
        outfile >> llist_item;
        outfile.ignore(numeric_limits<streamsize>::max(), '\n');

        llist->insert_node(llist_item);
    }

    int position;
    outfile >> position;
    outfile.ignore(numeric_limits<streamsize>::max(), '\n');

    SinglyLinkedListNode* llist1 = deleteNode(llist->head, position);

    print_singly_linked_list(llist1, " ", fout);
    fout << "\n";

    free_singly_linked_list(llist1);

    fout.close();

    return 0;
}
