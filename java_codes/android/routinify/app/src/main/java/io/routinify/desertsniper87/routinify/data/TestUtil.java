package io.routinify.desertsniper87.routinify.data;

import android.content.ContentValues;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.util.Log;

import java.util.ArrayList;
import java.util.List;

import static android.content.ContentValues.TAG;

public class TestUtil {

    public static void insertFakeData(SQLiteDatabase db){
        if(db == null){
            return;
        }

        //create a list of fake guests
        List<ContentValues> list_task_names = new ArrayList<ContentValues>();
        List<ContentValues> list_tasks = new ArrayList<ContentValues>();

        ContentValues cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Sleep");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "0");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Study");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "1");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Down Time");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "2");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Dining");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "3");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Exercise");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "4");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Programming");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "5");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Meditation");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "6");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Journal");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "7");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Family Time");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "8");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Health");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "9");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Professional Work");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "10");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Bath");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "11");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);

        cv_tasks = new ContentValues();
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME, "Travel");
        cv_tasks.put(RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID, "12");
        Log.d(TAG, "insertFakeData: "+ cv_tasks);
        list_task_names.add(cv_tasks);


//        ContentValues cv_routine = new ContentValues();
//        cv_routine.put(RoutineActivityContract.RoutineActivityEntry.COLUMN_TASK_ID, "0");
//        cv_routine.put(RoutineActivityContract.RoutineActivityEntry.COLUMN_TASK_START_TIME, "0730");
//        cv_routine.put(RoutineActivityContract.RoutineActivityEntry.COLUMN_TASK_END_TIME, "1029");
//        Log.d(TAG, "insertFakeData: "+ cv_routine);
//        list_task_names.add(cv_routine);






        try
        {
            db.beginTransaction();
            db.delete(RoutineActivityContract.TaskDescEntry.TABLE_NAME, null, null);
            //go through the list and add one by one
            for(ContentValues c : list_task_names){
                Log.d(TAG, "insertFakeData: " + c.toString());
                    db.insert(RoutineActivityContract.TaskDescEntry.TABLE_NAME, null, c);
            }
            db.setTransactionSuccessful();
        }
        catch (SQLException e) {
            //too bad :(
        }
        finally
        {
            db.endTransaction();
        }

    }
}
