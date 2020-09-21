#include <iostream>
using namespace std;

int main() {
    int high_index;
    int low_index;
    int temp;
    int pivot;
    int i, j;

    int test_cases;
    cin>> test_cases;

    while (test_cases){
        int number_of_inputs;
        cin>> number_of_inputs;
        int array[number_of_inputs];
        high_index = number_of_inputs;
        i=0;
        while (number_of_inputs){
            cin>> array[i];
            i++;
            number_of_inputs--;
        }

        low_index = 0;

        cout<< low_index;
        cout<< high_index;

        pivot = array[high_index-1];

        while (high_index>low_index){
            if (array[low_index]<pivot) {
                if (array[high_index] > pivot) {
                    temp = array[low_index];
                    //cout<< temp;
                    array[low_index] = array[high_index];
                    array[high_index] = temp;
                }
                high_index--;
            }
            low_index++;

            for ( i=0;i<number_of_inputs;i++ ) {
                cout<< array[i]<< " ";
            }
        }   // end of while

        cout<< endl;

        test_cases--;
    }



    //for ()

    return 0;
}

