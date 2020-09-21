#include <iostream>
#include <fstream>

int main() {
    char s;
    std::ifstream inputfile("input.txt");
    std::ofstream outputfile;
    outputfile.open("output.txt", std::ofstream::out);



//    size_t pos = 0;
//    std::string token;
//    while ((pos = s.find(delimiter)) != std::string::npos) {
//        token = s.substr(0, pos);
//        std::cout << token << std::endl;
//        s.erase(0, pos + delimiter.length());
//    }
//    std::cout << s << std::endl;

    outputfile<< "HEllo";

    while (inputfile>> s){
        if (s>='A' && s<='Z')
            outputfile<< s;
    }

    return 0;
}