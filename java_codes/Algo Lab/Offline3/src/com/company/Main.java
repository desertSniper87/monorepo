package com.company;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.*;

class Graph {
    private int V;
    private LinkedList<Integer> adj[];

    Graph (int v){
        V = v;
        adj = new LinkedList[v+1];
        for (int i=1;i<v+1;i++){
            adj[i] = new LinkedList<Integer>();
        }
    }

    public void addEdge(int v, int w){
        adj[v].add(w);
    }

    void dfs(int v, boolean visited[], Stack stack){
        visited[v] = true;
        int i;

        Iterator<Integer> iter = adj[v].iterator();
        while (iter.hasNext()) {
            i = iter.next();
            if (!visited[i]) {
                dfs(i, visited, stack);
            }
        }

        stack.push(new Integer(v));

    }

    public void topologicalSort(){
        Stack stack = new Stack();

        boolean visited[] = new boolean[V+1];
        for (int i = 1;i<V+1;i++){
            visited[i] = false;
        }

        for (int i=1;i<V+1;i++){
            if (visited[i]==false)
                dfs(i, visited, stack);
        }

        while (!stack.empty())
            System.out.print(stack.pop()+" ");
    }

    public boolean transitiveClosure()
    {

        int reach[][] = new int[V][V];
        int[][] adjMat = new int[V][V];
        int  i, j, k;
        boolean flag = true;


        for (i=1; i<=V; i++) {
            for (j=1;j<=V;j++){
                if (adj[i].contains(j))
                    adjMat[i-1][j-1] = 1;
                else
                    adjMat[i-1][j-1] = 0;
                //System.out.println(adjMat[i][j]);
            }
        }

//        for (int l = 1; l < V; l++) {
//            for (int m = 1; m < V ; m++) {
//                System.out.print(adjMat[l][m]+" ");
//            }
//            System.out.println();
//        }

        for (i = 1; i <= V; i++)
            for (j = 1; j <= V; j++)
                reach[i-1][j-1] = adjMat[i-1][j-1];


        for (k = 0; k < V; k++)
        {
            // Pick all vertices as source one by one
            for (i = 0; i < V; i++)
            {
                // Pick all vertices as destination for the
                // above picked source
                for (j = 0; j < V; j++)
                {
                    // If vertex k is on a path from i to j,
                    // then make sure that the value of reach[i][j] is 1
                    reach[i][j] = (reach[i][j]!=0) ||
                            ((reach[i][k]!=0) && (reach[k][j]!=0))?1:0;
                }
            }
        }

        for (int l = 1; l < V; l++)
            for (int m = 1; m < V; m++) {
                if (reach[l][m] == 0)
                    return false;
            }

        return flag;
//        for (int l = 1; l < V; l++) {
//            for (int m = 1; m < V ; m++) {
//                System.out.print(reach[l][m]+" ");
//            }
//            System.out.println();
//        }
        // Print the shortest distance matrix
        //printSolution(reach);
    }
}

//class Task {
//    private int name;
//    private int duration;
//    //private LinkedList<Integer> dependentTasks;
//
//    public void Task(int n, int d){
//        name = n;
//        duration = d;
//    }
//}

public class Main {

    public static void main(String[] args) throws FileNotFoundException {
        int[] durMap;
        //Scanner sc = new Scanner(System.in);
        Scanner sc = new Scanner(new File("input.txt"));
        int p = sc.nextInt();

        while (p!=0){                           //      P - > Projects
            int n = sc.nextInt();               //      n - > Tasks
            Graph g = new Graph(n);
            durMap = new int[n+1];

            int task_no = 1;

            while (n!=0){
                int duration = sc.nextInt();
                durMap[task_no] = duration;

                int m = sc.nextInt();
                while (m!=0){
                    int j = sc.nextInt();
                    g.addEdge(j, task_no);
                    m--;
                }

                task_no++;
                n--;
            }
            if (g.transitiveClosure()==false) {
                g.topologicalSort();
            }
            else
                System.out.println("Can't do");
            System.out.println();

            p--;
        }

    }
}
