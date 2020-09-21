#include <iostream>
using namespace std;

const short int TRUE = 1, FALSE = 0;
template <class ListType>//type of data each evrtex stores
class Graph
{
private:
    int max;
	struct vertex;//forward declaration for vertex structure
	struct node //linked list for mapping edges in the graph
	{
		vertex *vertexPtr;//points to the vertex to which the edge is adjecent
		node *next;//points to the next edge belong to the same vertex
	};

	struct vertex //struct to store details of a vertex
	{
		ListType data; //template type of data
		int visited;   //to check if the vertex is visited or not
		node *list;	   //pointer to all the edges (linked lists)
	};

	vertex *vertexArray;//array for all the vertices
	node *tempList;//to temporarily store the lists
	vertex **vertexQueue;//queue of the vertices
	ListType SearchElement;
	int queueHead, queueTail;//head and the tail of the queue

 	//allocates initializes and returns a new node from the memory
	node* getNode(vertex *v)
	{
		node *newNode = new node;
		newNode->vertexPtr = v;
		newNode->next = NULL;
		return newNode;
	}

 	//adds a node at the end of the list
 	//accepts refference to pointer and pointer to vertex
	void addAtEnd(node *&n, vertex *v)
	{
		node *temp;//newly allocated node is stored here
		if(n == NULL)//if no nodes exist allocate a node directly and return
		{
			n = getNode(v);
			return;
		}
		node *endNode = n;//copy of the first node to traverse to last node
		temp = getNode(v);//allocating a lode to attach at the end of the list
		while(endNode->next != NULL)
		//traverse till the last node (next of the node is null)
		{
			endNode = endNode->next;
		}
		endNode->next = temp;//attach the new node to the last node
	}

 	//add node to the tail of the queue
 	void enqueue(vertex *v)
 	{
 		if(queueTail < max)
 			vertexQueue[queueTail++] = v;
 	}

 	//remove a node from the head of the queue
 	vertex* dequeue()
 	{
 		if(queueHead < max)
 			return vertexQueue[queueHead++];
 		else
 			return NULL;
 	}

 	//delete all the nodes after the specified node
	void deleteAllAfter(node *n)
	{
		node *temp;//store the last node
		while(n != NULL)
		//till n is not the last node (null) delete node and move to next node
		{
			temp = n -> next;
			delete n;
			n = temp;
		}
	}

public:
	Graph(){}
	Graph(ListType *value, int size)
	//constructor that allocates and initializes the vector array
	{
		max = size;
		vertexArray = new vertex [max];
		for (int i = 0; i < max ; ++i)
		{
			vertexArray[i].data = value[i];
			vertexArray[i].list = NULL;
			vertexArray[i].visited = 0;
		}
	}
	void setEdge(ListType vert1, ListType vert2)
	//set edges between vertuces vert1 and vert2 by iterating
	{
		for (int i = 0; i < max; ++i)//loop through all the vertices
		{
			if(vertexArray[i].data == vert1)//when vert1 is found
			{
				for (int j = 0; j < max; ++j)//loop through all the vertices
				{
					if(vertexArray[j].data == vert2)//when vert2 is found
					{
						//connect vert1 to vert2 and vice versa
						addAtEnd(vertexArray[i].list,&vertexArray[j]);
						addAtEnd(vertexArray[j].list,&vertexArray[i]);
						break;
					}
				}
				break;
			}
		}
	}
	void dfs(vertex *v)
	{
		v->visited = TRUE;//since we visited this node
		if (v->data == SearchElement)//output the data to know we have visited
			cout<<"("<<v->data<<") ";//element found
		else
			cout<<v->data<<" ";//element not found
		while(1)
		{
			if(v->list == NULL)//if we reach the leaf node
				break;
			//if the node connected to this node is not visited,
			if(v->list->vertexPtr->visited == FALSE)
				dfs(v->list->vertexPtr);//pass that node to this function
			v->list = v->list->next;//point to the next node
		}
		return;
	}
	void clearVisited()
	{
		for (int i = 0; i < max; ++i)//iterate and set all nodes to not visited
		{
			vertexArray[i].visited = FALSE;
		}
	}
	void bfs(vertex *v)
	{
		enqueue(v);
		v->visited = TRUE;
		for (int i = 0; i < max; ++i)
		{
			if(queueHead >= queueTail)//if head crosses tail during incrementing
			{
				cout<<endl<<"FATAL ERROR IN TRAVERSING";
				break;
			}
			while(v->list->next != NULL)//while there are no more child nodes
			{
				if(v->list->next->vertexPtr->visited == TRUE)//if child node is visited
				{
					v = vertexQueue[i];//set element form the queue where the head is pointing
					break;
				}
				v->list = v->list->next;//set the current node to its sibling node
				enqueue(v);//enqueue the node to the list
				v->visited = TRUE;//set that node as visited
			}
			if (vertexArray[i].data == SearchElement)//output the data to know we have visited
				cout<<"("<<vertexArray[i].data<<") ";//if found
			else
				cout<<vertexArray[i].data<<" ";//if not found
		}
	}
	void search(char type = 'd')
	{
		if(type == 'd')
			dfs(vertexArray);
		else if(type == 'b')
		{
			vertexQueue = new vertex*[max];
			queueHead = 0, queueTail = 0;
			bfs(vertexArray);
		}
	}
	void setSearchElement(ListType value)
	{
		SearchElement = value;
	}
	void display()
	//display all the vertices and their connections
	{
		node *n;
		for (int i = 0; i < max; ++i)
		{
			cout<<"Vertex: "<<vertexArray[i].data<<" ";
			cout<<"Connections: ";
			n = vertexArray[i].list;
			while(n != NULL)
			{
				cout<<n->vertexPtr->data;
				n = n->next;
				cout<<" ";
			}
			cout<<endl;
		}
	}

	~Graph(){}
};

int main()
{
	int size;
	int edge;
	cout<<"Size: ";
	cin>>size;
	cout<<"Edge: ";
	cin>>edge;
	char *value;
	value = new char [size];
	//cout<<"Enter vertices: ";
	for (int i = 0; i < size; ++i)
	{
		value[i] = i+1;
	}

	Graph <char>g1(value,size);
	Graph <char>g2(value,size);
	//int choice = 1;
	char vertA, vertB, searchElement;

	while(1)
	{
		cout<<"Do you want to set an edge?(1/0): ";
		cin>>choice;
		if(choice)
		{
			cin>>vertA>>vertB;
			g1.setEdge(vertA,vertB);
			g2.setEdge(vertA,vertB);
		}
		else
			break;
	}
    /*
	for (int i=0;i<edge;i++){
            cin>>vertA>>vertB;
			g1.setEdge(vertA,vertB);
			g2.setEdge(vertA,vertB);
	}*/
	cout<<endl;
	cout<<"Search element: ";
	cin>>searchElement;
	cout<<endl;
	g1.display();
	g1.setSearchElement(searchElement);
	g2.setSearchElement(searchElement);
	cout<<"BFS: ";
	g2.search('b');
	cout<<endl;
	cout<<"DFS: ";
	g1.search('d');
	cout<<endl;
}
