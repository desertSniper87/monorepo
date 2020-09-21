/*****************************************************************************************************
    Program     : Simulated Annealing
    Discription : Implement Simulated Annealing Algorithm on Permutation flow shop scheduling problem


======================================================================================================
    Algorithm Details

    Objective            : Find the schedule with the shortest makespan
    Accept Rules         : Metroplis acceptance rule


    ** 3 parameter for this algorithm
    Initial Temperature  : 1000 K as default
    Terminal temperature : 100 K as default
    Cooling coefficient  : 0.75 as default
    Max Iteration        : 20 as default
    you can adjust the algorithm by calling "setpara()" function or reset the algorithm by calling
    " reset() " , or by using " constructor ".

*******************************************************************************************************/
#include<iostream>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>
#include<math.h>
#include<limits.h>
using namespace std;

int Counttt=0;
time_t tt;
class SimulatedAnnealing{
    public:

        /* Initialize and setting function set*/
        void init(char*);
        void reset(void);
        void setpara(char *para,double val);/* Know more details in SPEC */
        void setparaConsole(void);/*Adjust parameter on command line */
        /* Operating Function set*/
        bool accept(int ce,int oe);
        void generateNeighbor(void);
        int algorithm(void); /** Core !!!! */
        int getMaxspan(void);
        int gettmpspan(void);
        int findBestNeighbor(void);
        int genNeighbor(void);

        /*inline function*/
        inline double refreshTemperature(double*t){ return (*t)*cooldown;}
        inline double getEnergy(int*e){ return (double)(*e); }

        /*Getting Function */
         /**
            None
         */

        /* Display Function : Use for debug */
        void dispPara(void);
        void dispState(void);

    private:
        /* Temperature Coeffcients*/
        double initT;
        double terminalT;
        double Temperature;/*Current Temperature */
        double cooldown;
        /* Variable for algorithm*/
        int optSolution;
        int MaxIt;
        int njobs;
        int nmachine;
        char testname[50];
        int NumOfNeighbor;
        int Minspan;/* Minspan */
        int Curspan;
        int selectI;
        int selectJ;
        double curEnergy;/* Energy State : Convert Maxspan to energy value*/
        double optEnergy;/* Optimal Energy State*/
        /* Data structure*/
        int *timebar;/*idle time for each machine*/
        int *order; /*list of solution*/
        int *tmporder;/*list to store state*/
        int *shufflelist;/*list for generate new neighbor */
        int *shufflelist2;
        int **table; /* Computed Table */
        int *pt; /* pilot pointer */
        char fname[50];/*Input File name */
};
/**
    Main Function
*/
int main(){
    char fn[50],pp[50];
    int ommmm;
    int Ommmmm;
    /* Set */
    strcpy(fn,"tai20_20_1.txt");
    strcpy(pp,"initT");


    /* Test*/
    SimulatedAnnealing sa;
    sa.init(fn);
    ommmm = sa.algorithm();
    printf("Optima : %d\n",ommmm);
    //printf("Accept Second One : %d\n",Counttt);


return 0;
}


/**
    Function Set
*/
void SimulatedAnnealing::init(char* fn){
    FILE*fp;
    int i,j,r;
    strcpy(fname,fn);
    if(!(fp = fopen(fn,"r"))){
        printf("Files Not Found!\n");
        exit(0);
    }

    fscanf(fp,"%d %d %s",&njobs,&nmachine,&testname);           // Setting up the params

    /* Memory Allocation */
    order = (int*)malloc(sizeof(int)*njobs);
    tmporder = (int*)malloc(sizeof(int)*njobs);
    shufflelist = (int*)malloc(sizeof(int)*njobs);
    shufflelist2 = (int*)malloc(sizeof(int)*njobs);
    timebar = (int*)malloc(sizeof(int)*nmachine);
    table =(int**)malloc(njobs*sizeof(int*)+njobs*nmachine*sizeof(int));
    pt = (int*)(table+njobs);

    for(i=0;i<njobs;i++,pt+=nmachine)
        table[i] =pt;
    //Print table
    //for(i=0)

    /* Read data*/
    for(i=0;i<nmachine;i++)
        for(j=0;j<njobs;j++)
            fscanf(fp,"%d",&table[j][i]);

    /* Initialize Parameter */
    srand(time(NULL));
    for(i=0;i<njobs;i++){
        order[i] = i;
    }

    for(i=njobs-1;i>=0;i--){
        r = rand()%njobs;
        order[i]= order[i]+order[r]-(order[r]=order[i]);
    }

    for(i=0;i<nmachine;i++)
        timebar[i] = 0;

    initT = 10000;
    terminalT = 1;
    cooldown = 0.75;
    MaxIt = 20;
    NumOfNeighbor = 15;
    optSolution =INT_MAX;
    Minspan = INT_MAX;
    Curspan = INT_MAX;
    optEnergy = Minspan;
    curEnergy = Curspan;
    /* Testing */

}

void SimulatedAnnealing::reset(void){
    int i;
    for(i=0;i<njobs;i++)
        order[i] = i;
    for(i=0;i<nmachine;i++)
        timebar[i] = 0;
    initT = 10000;
    terminalT = 1000;
    cooldown = 0.85;
    MaxIt = 20;
    NumOfNeighbor = 10;
    optSolution = INT_MAX;
    Minspan = INT_MAX;
    Curspan = INT_MAX;
    optEnergy = Minspan;
    curEnergy = Curspan;

    order = NULL;
    pt = NULL;
    timebar = NULL;
    table = NULL;
    shufflelist = NULL;
    shufflelist2 = NULL;
    tmporder = NULL;

}

void SimulatedAnnealing::setpara(char*para,double val){

        if(!strcmp(para,"initT"))
            initT = val;
        else if(!strcmp(para,"terminalT"))
            terminalT = val;
        else if(!strcmp(para,"cooldown"))
            cooldown = val;
        else if(!strcmp(para,"maxit"))
            MaxIt =(int)val;
        else if(!strcmp(para,"nn"))
            NumOfNeighbor =(int)val;
        else{
            printf("No such parameter!\n");
            printf("parameter list\n");
            printf("initT     | initialial temperature\n");
            printf("terminalT | terminal temperature\n");
            printf("cooldown  | cool down coefficient\n");
            printf("maxit     | max iteration\n");
            printf("nn        | number of neighbor\n");
            printf("cs        | iteration cycle set\n");
        }
}

void SimulatedAnnealing::setparaConsole(void){

    // There is nothing here.


}



bool SimulatedAnnealing::accept(int current,int neighbor){

    //srand(time(NULL));
    //printf("N : %d %d\n",current,neighbor);
    /*if(neighbor<=current)
        return true;*/

    double r,delta;
    int x;
    //srand(time(NULL));
    x = rand()%32760;
    r = ((double)x)/32760;
    cout<< r<< " ";
    delta = exp((current-neighbor)/Temperature);
    printf(" %d %d |Random : %lf | Threshold : %lf",current,neighbor,r,delta);
    if(delta>r){
        printf("   Accept!\n");
        return true;
    }
    else{
        printf("    Reject!\n");
        return false;
    }
}


int SimulatedAnnealing::algorithm(void){
    FILE*fpp;
    int initS;
    int initE,i;            //Unused
    int iterCount;

    if(!(fpp = fopen("SA_output.txt","w"))){
        printf("File not found!\n");
        exit(0);
    }
    srand(time(NULL));
    Minspan = getMaxspan();
    //printf("First Span : %d\n",Minspan);
    fprintf(fpp,"%d\n",Minspan);/*Initial State */
    Temperature = initT;/*Initial Temperature */

    for(iterCount=0;Temperature>=terminalT;iterCount++){
        /* Find Neighbor */
        //initS = findBestNeighbor();
        initS = genNeighbor();
        if(accept(Curspan,initS)){/* Accept*/
            Curspan =initS;
            if(Curspan<Minspan)
                Minspan = Curspan;
            order[selectI] = order[selectI]+order[selectJ]-(order[selectJ]=order[selectI]);/*Swap this two element*/
        }
        fprintf(fpp,"%d\n",Curspan);

        if(iterCount==MaxIt){
            Temperature*=cooldown;
            iterCount = 0;
        }
    }
    return Minspan;
}

int SimulatedAnnealing::findBestNeighbor(void){/*Find Minspan of Best Neighbor*/
    int i,j,r,q,sw,st,tmpSpan,tmpmin;
    int temp;

    /* Random Generate Neighbor */
    for(i=0;i<njobs;i++){
        shufflelist[i] = i;
        shufflelist2[i] = i;
    }
    for(i=njobs-1;i>=0;i--){
        r = rand()%njobs;
        shufflelist[i] = shufflelist[i]+shufflelist[r]-(shufflelist[r]=shufflelist[i]);
        while((q = rand()%njobs)==r);
        shufflelist2[i] = shufflelist2[i]+shufflelist[q]-(shufflelist2[q]=shufflelist2[i]);
    }

    /*
    printf("Origin : ");
    for(i=0;i<njobs;i++)
       printf("%d ",order[i]);
    printf("\n");
    printf("Neighbor : ");
    */
    for(i=0;i<NumOfNeighbor;i++){
        for(j=0;j<njobs;j++)
            tmporder[j] = order[j];
        sw = shufflelist[i];
        st = shufflelist2[i];
        //printf("SWAP (%d %d)  :  %d %d --> ",sw,st,tmporder[sw],tmporder[st]);
        tmporder[sw] = tmporder[sw]+tmporder[st]-(tmporder[st]=tmporder[sw]);

        tmpSpan = gettmpspan();
        //printf(" %d(%d|%d)",tmpSpan,sw,st);
        if(tmpSpan<tmpmin||i==0){
            tmpmin = tmpSpan;
            selectI = sw;
            selectJ = st;
        }
    }
return tmpmin;
}

int SimulatedAnnealing::genNeighbor(){
    int a,b,j;
    //srand(time(NULL));
    //_getsystime(&tt);
    a = rand()%njobs;
    while((b=rand()%njobs)==a);

    /* Check neighbor */
    //printf("Neighbor : %d %d\n",a,b);
    for(j=0;j<njobs;j++)
            tmporder[j] = order[j];
    tmporder[a] = tmporder[a]+tmporder[b]-(tmporder[b]=tmporder[a]);
    selectI = a;
    selectJ = b;
    return gettmpspan();
}


int SimulatedAnnealing::getMaxspan(void){

    int i,j,maxspan;
    for(i=0;i<nmachine;i++)/*initialize*/
    timebar[i] = 0;

    /* find maxspan */
    maxspan = 0;
    for(i=0;i<njobs;i++) {
        /*Machine 0*/
        timebar[0]+=(table[order[i]][0]);
        maxspan = timebar[0];
        //printf("jobs: %d  |time: %d |task: %d |maxspan: %d\n",i,timebar[0],table[order[i]][0],maxspan);
        if(maxspan<timebar[1])
            maxspan = timebar[1];
        for(j=1;j<nmachine;j++) {
            if(maxspan<timebar[j])
                maxspan = timebar[j];
            timebar[j] = maxspan + table[order[i]][j];
            if(maxspan<timebar[j])
                maxspan = timebar[j];
            //printf("jobs: %d  |time: %d |task: %d |maxspan: %d\n",i,timebar[j],table[order[i]][j],maxspan);
        }
    }
    return  timebar[nmachine-1];
}

int SimulatedAnnealing::gettmpspan(void){
     int i,j,minspan;
    for(i=0;i<nmachine;i++)/*initialize*/
        timebar[i] = 0;

    /* find minspan */
    minspan = 0;
    for(i=0;i<njobs;i++){
        /*Machine 0*/
        timebar[0]+=(table[tmporder[i]][0]);
        minspan = timebar[0];
        if(minspan<timebar[1])
            minspan = timebar[1];
        for(j=1;j<nmachine;j++){
            if(minspan<timebar[j])
                minspan = timebar[j];
            timebar[j] = minspan + table[tmporder[i]][j];
            if(minspan<timebar[j])
                minspan = timebar[j];
        }
    }
    return  timebar[nmachine-1];
}

void SimulatedAnnealing::dispPara(void){
    int i;
    for(i=0;i<njobs;i++)
        printf("%d ",order[i]);
    printf("\n");

    printf("initial Temperature  : %lf\n",initT);
    printf("terminal Temperature : %lf\n",terminalT);
    printf("cool down coeff      : %lf\n",cooldown);
    printf("Max Iteration        : %lf\n",MaxIt);
    printf("initial solution     : %d\n",optSolution);
}

void SimulatedAnnealing::dispState(void){
    printf("State Information\n\n");
    printf("MIN SPAN      : %d\n",Minspan);
    printf("CURRENT SPAN  : %d\n",Curspan);
    printf("----------------------\n");
    printf("OPT ENERGY    : %lf\n",optEnergy);
    printf("CURRENT ENERGY: %lf\n",curEnergy);
}
