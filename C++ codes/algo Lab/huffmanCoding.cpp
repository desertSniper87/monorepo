#include <iostream>
#include <algorithm>
#include <vector>
using namespace std;

int extract_min1(vector<int> freq);
int extract_min2(vector<int> freq);

int main (){

    int input_number;
    cin>> input_number;
    int freq_n;
    int i;

    while (input_number!=0) {
        cin>> freq_n;

        vector<int> frequency;
        vector<int> bits;
        vector<int> parents;

        while (freq_n!=0) {
            cin>> i;
            frequency.insert(frequency.end() ,i);
            freq_n--;
        }

        int min1 = extract_min1(frequency);
        int min2 = extract_min2(frequency);


    }

    input_number--;

    return 0;
}

int extract_min1(vector<int> freq) {
    sort(freq.begin(), freq.end());
    return freq[0];

}

int extract_min2 (vector<int> freq){
    sort(freq.begin(), freq.end());

    return (freq.begin()+1);
}
//
//int extract_min2(int freq[]) {
//    sort(freq[]);
//}
