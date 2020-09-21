#include <iostream>
#include <vector>
using namespace std;

void quickSort(vector<int>&,int,int);
int  partition(vector<int>&, int,int);
void print_array(int array[],int size);
void heapsort(vector<int>& array);
//void heapsort(vector<int>& array, int n);
void insertion_sort(vector<int>& array, int length);
void build_max_heap(vector <int>&a, int n);
void max_heapify(vector <int>&a, int i, int n);

int main()
{
    vector<int> array = {6,10,13,5,8,3,2,25,4,11};
    int pivot=0;                    //Signifies the pivot postition
    int n=10;
    int choice;

    cout<< "Enter a number"<< endl;
    cout<< "1.QuickSort     2. Heap Sort      3.Insertion Sort"<< endl;
    cin>> choice;

    cout<<"= = = = = = Original= = = = = = "<<endl;
    for(int i : array)
        cout<< i <<" ";
    cout<< endl;

    if (choice==1)
        quickSort(array,pivot,n);
    else if (choice==2)
        heapsort(array);
        //heapsort(array, n);
    else if (choice==3)
        insertion_sort(array, n);


    for(int i : array)
        cout << i << " ";
    return 0;
}

//<<<<<<<<<<=====================Quick Sort=====================>>>>>>>>>>>>>>>

void quickSort(vector<int>& array, int pivot,int n)
{
    int r;
    if(pivot<n)
    {
        r = partition(array, pivot,n);
        quickSort(array,pivot,r);
        quickSort(array,r+1,n);
    }
}


int partition(vector<int>& array, int pivot,int n)
{
    int x= array[pivot];
    int i=pivot;
    int j;

    for(j=pivot+1; j<n; j++)
    {
        if(array[j]<=x)
        {
            i=i+1;
            swap(array[i],array[j]);
        }

    }

    swap(array[i],array[pivot]);
    return i;
}


//<<<<<<<<<<<<===================  Heap sort  ========================>>>>>>>>>>>>>

/*


void sift_down(vector<int>& heap,int i, int max) {
    int i_big, c1, c2;

    while(i < max) {
        i_big = i;
        c1 = (2*i) + 1;
        c2 = c1 + 1;
        if( c1<max && heap[c1]>heap[i_big] )
            i_big = c1;
        if( c2<max && heap[c2]>heap[i_big] )
            i_big = c2;
        if(i_big == i) return;
        swap(heap[i],heap[i_big]);
        i = i_big;
    }
}

void to_heap(vector<int>& array) {
    int i = (array.size()/2) - 1;

    while(i >= 0) {
        sift_down(array, i, array.size());
        --i;
    }
}

void heapsort(vector<int>& array) {
    to_heap(array);
    int end = array.size() - 1;
    while (end > 0) {
        swap(array[0], array[end]);
        sift_down(array, 0, end);
        --end;
    }
}

*/

///Trinary Tree version

void sift_down(vector<int>& heap,int i, int max) {
    int i_big, c1, c2, c3;

    while(i < max) {
        i_big = i;
        c1 = 3*i + 1;
        c2 = 3*i + 2;
        c3 = 3*i + 3;
        if( c1<max && heap[c1]>heap[i_big] )
            i_big = c1;
        if( c2<max && heap[c2]>heap[i_big] )
            i_big = c2;
        if( c3<max && heap[c3]>heap[i_big] )
            i_big = c3;
        if(i_big == i) return;
        swap(heap[i],heap[i_big]);
        i = i_big;
    }
}

void to_heap(vector<int>& array) {
    int i = (array.size() - 1)/3;

    while(i >= 0) {
        sift_down(array, i, array.size());
        --i;
    }
}

void heapsort(vector<int>& array) {
    to_heap(array);
    int end = array.size() - 1;
    while (end > 0) {
        swap(array[0], array[end]);
        sift_down(array, 0, end);
        --end;
    }
}

//<<<<<<<<<<<<=================Insertion sort===================>>>>>>>>>>>>>>



void insertion_sort(vector<int>& array, int length)   {
    int i, j ,tmp;
        for (i = 1; i < length; i++) {
            j = i;
            while (j > 0 && array[j - 1] > array[j]) {
                tmp = array[j];
                array[j] = array[j - 1];
                array[j - 1] = tmp;
                j--;
        }                   //end of while loop
    }                   //end of for loop
}                  //end of insertion_sort.



/** Trinary tree Heap Sort second Implementation **/

/*

void max_heapify(vector <int>&a, int i, int n)
{
    int largest=0;
    int l=3*i;
    int r=(3*i)+1;

    if ((l<=n)&&(a[l]>a[i])) {
        largest=l;
    }
    else {
        largest=i;
    }

    if ((r<=n)&&(a[r]>a[largest])) {
        largest=r;
    }

    if (largest!=i) {
        swap(a[i], a[largest]);
        max_heapify(a, largest, n);
    }
}

void build_max_heap(vector <int>&a, int n) {
    for (int i=(n/3);i>0;i--) {
        max_heapify(a, i, n);
    }
}

void heapsort(vector <int>& a, int n) {
    build_max_heap(a, n);
    for (int i=n;i>0;i--) {
        swap(a[0], a[i]);
        max_heapify(a, 1, i-1);
    }
}

*/

