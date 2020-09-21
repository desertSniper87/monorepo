var option = 0;
var i = 0;
var numbers = new Array();

while (option!=3){
    document.write("Please choose an option and press enter:\n");
    document.write("1. Read 3 numbers\n 2. Print the max\n 3.Exit\n");
    option = prompt("Option");
    if (option == 1) {
        for (i=0; i<3; i++) {
            numbers[i] = prompt("numbers[" + (i+1) + "]");
        }
    } else if (option == 2) {
        var max = 0;
        for (i=0; i<3; i++) {
            if(numbers[i] > max) {
                max = numbers[i];
            }
        }
        document.write("\nMax=" + max + "\n");
    }
}
