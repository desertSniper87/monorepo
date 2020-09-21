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
}

// Complete the mergeLists function below.

/*
 * For your reference:
 *
 * SinglyLinkedListNode {
 *     int data;
 *     SinglyLinkedListNode* next;
 * };
 *
 */
SinglyLinkedListNode* mergeLists(SinglyLinkedListNode* head1, SinglyLinkedListNode* head2) {
    SinglyLinkedListNode* root = (SinglyLinkedListNode*) malloc(sizeof(SinglyLinkedListNode));
    if (head1->data < head2->data) {
        SinglyLinkedListNode* root = head1;
    } else {
        SinglyLinkedListNode* root = head2;
    }
    while(true) {
        if (head1==NULL) {
            return head2;
        } else if (head2==NULL) {
            return head1;
        } else if (head1->data <= head2->data) {
            SinglyLinkedListNode* temp = (SinglyLinkedListNode*) malloc(sizeof(SinglyLinkedListNode));
            temp = head1->next;

            head1->next = head2;
            head2->next = temp;
            head2 = head2->next;

            free(temp);
        } else {
            SinglyLinkedListNode* temp = (SinglyLinkedListNode*) malloc(sizeof(SinglyLinkedListNode));
            temp = head2->next;

            head2->next = head1;
            head1->next = temp;
            head1 = head1->next;

            free(temp);
        }
    }
    return root;

}



int main()
{
    //ofstream fout(getenv("OUTPUT_PATH"));
    ofstream fout("./merge_sorted_linked_list_output.txt");
    ifstream myfile;
    myfile.open("./merge_sorted_linked_list_input_1.txt", ios::in);

    int tests;
    myfile >> tests;
    myfile.ignore(numeric_limits<streamsize>::max(), '\n');

    for (int tests_itr = 0; tests_itr < tests; tests_itr++) {
        SinglyLinkedList* llist1 = new SinglyLinkedList();

        int llist1_count;
        myfile >> llist1_count;
        myfile.ignore(numeric_limits<streamsize>::max(), '\n');

        for (int i = 0; i < llist1_count; i++) {
            int llist1_item;
            myfile >> llist1_item;
            myfile.ignore(numeric_limits<streamsize>::max(), '\n');

            llist1->insert_node(llist1_item);
        }
      
        SinglyLinkedList* llist2 = new SinglyLinkedList();

        int llist2_count;
        myfile >> llist2_count;
        myfile.ignore(numeric_limits<streamsize>::max(), '\n');

        for (int i = 0; i < llist2_count; i++) {
            int llist2_item;
            myfile >> llist2_item;
            myfile.ignore(numeric_limits<streamsize>::max(), '\n');

            llist2->insert_node(llist2_item);
        }

        SinglyLinkedListNode* llist3 = mergeLists(llist1->head, llist2->head);

        print_singly_linked_list(llist3, " ", fout);
        fout << "\n";

        free_singly_linked_list(llist3);
    }

    fout.close();

    return 0;
}
