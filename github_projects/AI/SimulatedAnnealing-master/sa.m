% Calling program and execute it 



    dos('g++ sa.cpp -o sa.exe');
    dos('sa.exe');

    load('SA_output.txt')
    y = SA_output;
    x = 1:length(SA_output);
    figure,plot(x,y(x));