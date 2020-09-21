package com.company;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import static java.lang.Math.abs;

public class Main {
    
    
    public void sortedByBinPackA1(){
            int i = 0;
            int bin_counter = 0;
            double totalExecutionTime = 0;
            double curExecutionTime = 0;

            List<Integer> list = new ArrayList<Integer>();

            int [] array = new int[]{15, 47, 42, 29, 22, 25, 2, 1, 35, 50, 33, 43, 24, 45, 14, 6, 27, 38, 17, 36, 13, 26, 16, 46, 48, 40, 8, 32, 3, 20, 41, 19, 12, 34, 31, 7, 9, 28, 4, 44, 10, 39, 5, 21, 18, 11, 37, 30, 23};

            for (i=0;i<50;i++)
                list.add(array[i]);
            }

            for (int i  list){
                totalExecutionTime += cloudLet;
            }



            //Collections.sort(list, new ExecutionTimeComparator());
            list

            List<Integer> cloudletListS = new ArrayList<Integer>();
            List <Integer> cloudletListM1 = new ArrayList<Integer>();
            List <Integer> cloudletListM2 = new ArrayList<Integer>();
            List <Integer> cloudletListL = new ArrayList<Integer>();

            for (curExecutionTime = 0; curExecutionTime < .33* totalExecutionTime; i++) {
                cloudletListS.add(list.get(i));
                curExecutionTime+= list.get(i).getExecutionTime();
            }
            for (; curExecutionTime < .5* totalExecutionTime; i++) {
                cloudletListM1.add(list.get(i));
                curExecutionTime+= list.get(i).getExecutionTime();
            }
            for (; curExecutionTime < .67* totalExecutionTime; i++) {
                cloudletListM2.add(list.get(i));
                curExecutionTime+= list.get(i).getExecutionTime();
            }
            for (; i < list.size(); i++) {
                cloudletListL.add(list.get(i));
            }

            list.clear();

            if (cloudletListS.size()>.33*i)
                shuffleCounter++;
            if (shuffleCounter%3==0){
                Collections.shuffle(cloudletListS);
                shuffleCounter = 0;
            }

            List<Double> M1ExecutionTime = new ArrayList<Double>();

            Iterator<? extends Integer> iteratorM1 = cloudletListM1.iterator();

            while (iteratorM1.hasNext()){
                Integer cloudlet = iteratorM1.next();
                M1ExecutionTime.add(cloudlet.getExecutionTime());
            }

            Collections.reverse(M1ExecutionTime);

            Iterator<? extends Integer> iteratorM2 = cloudletListM2.iterator();
            while (iteratorM2.hasNext()){
                Integer cloudlet = iteratorM2.next();
                if (M1ExecutionTime.contains(cloudlet.getExecutionTime())){
                    M1ExecutionTime.remove(cloudlet.getExecutionTime());
                    list.add(cloudlet);
                    bin_counter++;
                }
            }

            bin_counter += (cloudletListM1.size())/2;

            List<Double> M2ExecutionTime = new ArrayList<Double>();
            iteratorM2 = cloudletListM2.iterator();
            while (iteratorM2.hasNext()){
                Integer cloudletM2 = iteratorM2.next();
                M2ExecutionTime.add(cloudletM2.getExecutionTime());
            }


            Iterator <? extends Integer> iteratorS = cloudletListS.iterator();


            int item_to_put = abs(cloudletListS.size()-cloudletListM1.size());
            while (iteratorS.hasNext() && item_to_put!=0) {
                Integer cloudletS = iteratorS.next();
                cloudletListM2.add(cloudletS);
                iteratorS.remove();
                item_to_put--;
            }

            iteratorS = cloudletListS.iterator();
            Collections.sort(cloudletListM2, new ExecutionTimeComparator());

            i = 0;
            Integer[] cloudletArrayM2 = new Integer[cloudletListM2.size()];
            for (Integer c : cloudletListM2){
                cloudletArrayM2[i] = c;
                i++;
            }

            i = 0;

            while (iteratorS.hasNext()){
                Integer cloudletS = iteratorS.next();
                list.add(cloudletArrayM2[i]);
                list.add(cloudletS);
                iteratorS.remove();
                i++;
            }
            for (;i<cloudletArrayM2.length;i++)
                list.add(cloudletArrayM2[i]);

            Collections.sort(cloudletListM1, new ExecutionTimeComparator());
            Collections.sort(cloudletListL, new ExecutionTimeComparator());

            list.addAll(cloudletListM1);
            list.addAll(cloudletListL);


        }

    public static void main(String[] args) {
	// write your code here

        
        
    }
}
