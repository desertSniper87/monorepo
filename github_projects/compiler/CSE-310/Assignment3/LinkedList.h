#include<cstdio>
#include<iostream>
#include<string>

using namespace std;

template<class X> 
class Node
{
	X val;
	Node<X> *next, *prev;
public:
	Node()	{next = NULL; prev = NULL;}

	Node<X>* getNext() {
		return next;
	}

	void setNext(Node<X>* next) {
		this->next = next;
	}

	Node<X>* getPrev() {
		return prev;
	}

	void setPrev(Node<X>*& prev) {
		this->prev = prev;
	}

	X getVal() {
		return val;
	}

	void setVal(X val) {
		this->val = val;
	}
	
	X* getValPointer()	{
		return &val;
	}
};

template <class X>
class LinkedList
{
	Node<X> *head;
	int total;
public:
	LinkedList()	{head = NULL; total = 0;}
	void insert(X n)	{
		Node<X> *temp = new Node<X>;
		temp->setVal(n);
		temp->setNext(head);
		if (head != NULL)	head->setPrev(temp);
		head = temp;
		total++;
	}

	//the following search function returns the address of the node if the node exists in the list.
	//otherwise it returns NULL;
	Node<X> *search(X s)	{
		Node<X> *temp = head;
		while ( temp != NULL )	{
			if ( temp->val == s )	return temp;
			temp = temp->next;
		}
		return NULL;
	}

	//the following search function search a node by a key/token of it. Here it searches by a string.
	//It returns the address of the node if the token is found. Otherwise it returns NULL.
	Node<X> * searchByToken(string token)	{
		Node<X> *temp = head;
		while ( temp != NULL )	{
			if ( temp->getVal().getSymbol() == token )	return temp;
			temp = temp->getNext();
		}
		return NULL;
	}

	void print(FILE *fout=stdout)	{
		Node<X> *temp = head;
		while ( temp != NULL )	{
			temp->getVal().print(fout);
			temp = temp->getNext();
			if (temp != NULL)	fprintf(fout, " , ");
		}
	}

	void deleteNode(X n)	{
		Node<X> *temp = search(n), *pr, *nx;
		if (temp == NULL )	return;
		pr = temp->getPrev();
		nx = temp->getNext();
		if ( pr != NULL )	pr->setNext(nx);
		if ( nx != NULL )	nx->setPrev(pr);
		if ( temp == head )	head = temp;
		delete temp;
	}

	void deleteNodeByToken(string token)	{
		Node<X> *temp = searchByToken(token), *pr, *nx;
		if (temp == NULL )	return;
		pr = temp->getPrev();
		nx = temp->getNext();
		if ( pr != NULL )	pr->setNext(nx);
		if ( nx != NULL )	nx->setPrev(pr);
		if ( temp == head )	head = nx;
		delete temp;
	}
	
	bool isEmpty()	{
		return head==NULL;
	}
	void clear()	{
		Node<X> *temp;
		while (head != NULL)	{
			temp = head->getNext();
			delete head;
			head = temp;
		}
	}
};
