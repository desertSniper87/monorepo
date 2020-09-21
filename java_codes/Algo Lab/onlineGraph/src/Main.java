import java.util.*;

/**
 * Created by torsho on 10/19/16.
 */

public class Task{
    private int name;
    private List<Task> depTasks= new LinkedList;
    private int duration;

    public void Task(int  name, int duration) {
        this.name = name;
        this.duration = duration;
    }

    public void addDep(Task task){
        this.depTasks.add(task);
    }

    public int getName(){
        return name;
    }

    public int getDuration(){
        return duration;
    }
}

class Graph{
    HashMap<String, String> adjList = new HashMap<>();

    public void addTask(Task t){
        //adjList.put(t.getName
    }
}

public class Main {
    public static void main(String args[]) {
        Scanner sc = new Scanner(System.in);
        int p = sc.nextInt();

        while (p!=0){
            int n = sc.nextInt();
            int task_no = 1;
            while (n!=0){
                int duration = sc.nextInt();

                int m = sc.nextInt();
                while (m!=0){

                }

                n--;
            }

            p--;
        }



    }
}
