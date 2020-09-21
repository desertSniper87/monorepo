#include<stdio.h>
#include<stdlib.h>
 
/* Link list node */
struct Node
{
    int data;
    struct Node* next;
};

void push(struct Node** head_ref, int new_data)
{
    /* allocate node */
    struct Node* new_node =
            (struct Node*) malloc(sizeof(struct Node));
            
    /* put in the data  */
    new_node->data  = new_data;
                
    /* link the old list off the new node */
    new_node->next = (*head_ref);    
        
    /* move the head to point to the new node */
    (*head_ref)    = new_node;
}

/* Function to print linked list */
void printList(struct Node *head)
{
    struct Node *temp = head;
    while(temp != NULL)
    {
        printf("%d  ", temp->data);    
        temp = temp->next;  
    }
}   

int main()
{
    /* Start with the empty list */
    struct Node* head = NULL;
   
    push(&head, 20);
    push(&head, 4);
    push(&head, 15); 
    push(&head, 85);      

    printf("Given linked list\n");
    printList(head);    
    /*reverse(&head);                      */
    printf("\nReversed Linked list \n");
    printList(head);    
    getchar();
}
