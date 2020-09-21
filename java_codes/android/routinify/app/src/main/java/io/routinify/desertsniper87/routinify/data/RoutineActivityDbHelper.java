package io.routinify.desertsniper87.routinify.data;

import android.content.Context;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

public class RoutineActivityDbHelper extends SQLiteOpenHelper{
    public static final String DATABASE_NAME = "routine.db";
    public static final int DATABASE_VERSION = 20;

    public RoutineActivityDbHelper(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
    }

    @Override
    public void onCreate(SQLiteDatabase sqLiteDatabase) {
        final String sql_create_routine = "CREATE TABLE " + RoutineActivityContract.RoutineActivityEntry.TABLE_NAME + "(" +
                RoutineActivityContract.RoutineActivityEntry._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," +
                RoutineActivityContract.RoutineActivityEntry.COLUMN_TASK_ID +  " INTEGER NOT NULL," +
                RoutineActivityContract.RoutineActivityEntry.COLUMN_TASK_START_TIME + " TIMESTAMP NOT NULL," +
                RoutineActivityContract.RoutineActivityEntry.COLUMN_TASK_END_TIME + " TIMESTAMP NOT NULL" + ")";

        sqLiteDatabase.execSQL(sql_create_routine);

        final String sql_task_desc = "CREATE TABLE " + RoutineActivityContract.TaskDescEntry.TABLE_NAME + "(" +
                RoutineActivityContract.TaskDescEntry._ID + " INTEGER PRIMARY KEY AUTOINCREMENT, " +
                RoutineActivityContract.TaskDescEntry.COLUMN_TASK_ID + " INTEGER, " +
                RoutineActivityContract.TaskDescEntry.COLUMN_TASK_NAME + " TEXT NOT NULL" + ")";

        sqLiteDatabase.execSQL(sql_task_desc);
    }

    @Override
    public void onUpgrade(SQLiteDatabase sqLiteDatabase, int i, int i1) {
        sqLiteDatabase.execSQL("DROP TABLE " + RoutineActivityContract.RoutineActivityEntry.TABLE_NAME);
        sqLiteDatabase.execSQL("DROP TABLE " + RoutineActivityContract.TaskDescEntry.TABLE_NAME);

        onCreate(sqLiteDatabase);
    }
}
