package io.routinify.desertsniper87.routinify;

import android.Manifest;
import android.app.DatePickerDialog;
import android.content.ContentValues;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.os.Environment;
import android.preference.PreferenceManager;
import android.support.v4.app.ActivityCompat;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.DatePicker;
import android.widget.Spinner;
import android.widget.TableLayout;
import android.widget.TableRow;
import android.widget.TextView;
import android.widget.Toast;

//import net.danlew.android.joda.JodaTimeAndroid;

import com.google.api.client.extensions.android.http.AndroidHttp;
import com.google.api.client.googleapis.extensions.android.gms.auth.GoogleAccountCredential;
import com.google.api.client.http.FileContent;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.json.JsonFactory;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.client.util.ExponentialBackOff;
import com.google.api.services.drive.Drive;
import com.google.api.services.drive.DriveScopes;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeFormat;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import io.apptik.widget.MultiSlider;
import io.routinify.desertsniper87.routinify.data.RoutineActivityContract;
import io.routinify.desertsniper87.routinify.data.RoutineActivityDbHelper;
import io.routinify.desertsniper87.routinify.data.TestUtil;

public class MainActivity extends AppCompatActivity implements AdapterView.OnItemSelectedListener{

    private TextView datePicker;
    private MultiSlider multiSlider5;
    private TextView tv_left_val, tv_right_val;

    public Spinner task_name_spinner;
    private TableLayout routine_table;
    private long taskId;

    private DateTime startTime;
    private DateTime endTime;

//    private RoutineActivityAdapter routineAdapter;
    private SQLiteDatabase mDb;

    Calendar myCalendar = Calendar.getInstance();

    DatePickerDialog.OnDateSetListener date = new DatePickerDialog.OnDateSetListener() {

        @Override
        public void onDateSet(DatePicker view, int year, int monthOfYear,
                              int dayOfMonth) {
            // TODO Auto-generated method stub
            myCalendar.set(Calendar.YEAR, year);
            myCalendar.set(Calendar.MONTH, monthOfYear);
            myCalendar.set(Calendar.DAY_OF_MONTH, dayOfMonth);

            resetSlider();

            updateLabel();
        }

    };


    final String TAG = "PRINT_INFO";

    private GoogleAccountCredential mCredential;
	private Drive mService;

    final String dbFileName = "/data/data/io.routinify.desertsniper87.routinify/databases/routine.db";
    File dbFile = new File(dbFileName);

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        Log.d(TAG, "onCreate: Database name " + dbFile);
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        RoutineActivityDbHelper dbHelper = new RoutineActivityDbHelper(this);
        mDb = dbHelper.getWritableDatabase();

        TestUtil.insertFakeData(mDb);
        Cursor routineCursor = getRoutineCursor();
        Cursor taskCursor = getTaskNameCursor();

        String[] tasks = getAllTasksArray(taskCursor);
        Log.d(TAG, "onCreate: tasks" + tasks);

        Log.d(TAG, "onCreate: tasks.length " + tasks.length);
        Log.d(TAG, "onCreate: Array.toString(tasks)"+ Arrays.toString(tasks));

        final DateTime currentTime = new DateTime(DateTimeZone.forOffsetMillis(TimeZone.getDefault().getRawOffset()));
        final int currentHour = currentTime.getHourOfDay();
        final int currentDayOfYear = currentTime.getDayOfYear();
//        DateTime dayToSet = currentTime;
        Log.d(TAG, "onCreate: DateTimeZone" + DateTimeZone.getDefault());
        Log.d(TAG, "onCreate: Hour of The Day" + currentHour);

//        routineAdapter = new RoutineActivityAdapter(this, routineCursor);

        /** Start Widget Setup **/

        datePicker = (TextView) findViewById(R.id.tv_datepicker);
//        datePicker.setText(dayToSet.toString());
        updateLabel();

        datePicker.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                // TODO Auto-generated method stub
                new DatePickerDialog(MainActivity.this, date, myCalendar
                        .get(Calendar.YEAR), myCalendar.get(Calendar.MONTH),
                        myCalendar.get(Calendar.DAY_OF_MONTH)).show();
            }
        });

        multiSlider5 = (MultiSlider) findViewById(R.id.range_slider5);
        if (getLastTaskEndTime().getDayOfYear()==currentDayOfYear) {
            if (getLastTaskEndTime().getHourOfDay() == currentHour) {
                multiSlider5.setMax(getLastTaskEndTime().getHourOfDay());
                int x = getLastTaskEndTime().getHourOfDay() - 24;
                multiSlider5.setMin(x < 0 ? 0 : x);
                startTime = new DateTime()
                        .withHourOfDay(x < 0 ? 0 : x)
                        .withMinuteOfHour(0)
                        .withSecondOfMinute(0)
                        .withMillisOfSecond(0);
                endTime = new DateTime()
                        .withHourOfDay(getLastTaskEndTime().getHourOfDay() > 0 ? getLastTaskEndTime().getHourOfDay() -1 : 23)
                        .withMinuteOfHour(59)
                        .withSecondOfMinute(59)
                        .withMillisOfSecond(59);
                if (currentHour == 0)
                    endTime.plusDays(-1);
            } else {
                if(getNumOfTask()==0){
                    multiSlider5.setMin(0);
                    startTime = new DateTime(DateTimeZone.forOffsetMillis(TimeZone.getDefault().getRawOffset()))
                            .withHourOfDay(0)
                            .withMinuteOfHour(0)
                            .withSecondOfMinute(0)
                            .withMillisOfSecond(0);
                }
                else{
                    multiSlider5.setMin(getLastTaskEndTime().getHourOfDay());
                    startTime = new DateTime(DateTimeZone.forOffsetMillis(TimeZone.getDefault().getRawOffset()))
                            .withHourOfDay(getLastTaskEndTime().getHourOfDay() > 0 ? getLastTaskEndTime().getHourOfDay() - 1 : 23 )
                            .withMinuteOfHour(0)
                            .withSecondOfMinute(0)
                            .withMillisOfSecond(0);

                }
                multiSlider5.setMax(currentHour);
                endTime = new DateTime(DateTimeZone.forOffsetMillis(TimeZone.getDefault().getRawOffset()))
                        .withHourOfDay(currentHour > 0 ? currentHour - 1 : 23)
                        .withMinuteOfHour(59)
                        .withSecondOfMinute(59)
                        .withMillisOfSecond(59);
                if (currentHour  == 0)
                    endTime.plusDays(-1);
            }

        } else{
            multiSlider5.setMin(0);
            startTime = new DateTime(DateTimeZone.forOffsetMillis(TimeZone.getDefault().getRawOffset()))
                    .withHourOfDay(0)
                    .withMinuteOfHour(0)
                    .withSecondOfMinute(0)
                    .withMillisOfSecond(0);

            multiSlider5.setMax(currentHour);
//            endTime = currentTime.withMinuteOfHour(0).plusMinutes(-1).plusSeconds(-1).plusMillis(-1);
            endTime = new DateTime(DateTimeZone.forOffsetMillis(TimeZone.getDefault().getRawOffset()))
                    .withHourOfDay(currentHour > 0 ? currentHour - 1 : 23)
                    .withMinuteOfHour(59)
                    .withSecondOfMinute(59)
                    .withMillisOfSecond(59);
        }

//        multiSlider5.setMin(getLastTaskEndTime().getHourOfDay());
//        multiSlider5.setMax(currentHour);

        task_name_spinner = (Spinner) findViewById(R.id.spinner_task_names);
        ArrayAdapter<String> spinnerAdapter = new ArrayAdapter<>(this, android.R.layout.simple_spinner_dropdown_item, tasks);
        task_name_spinner.setAdapter(spinnerAdapter);
//        task_name_spinner.setOnItemSelectedListener(new TaskSelectSpinnerListener());
        task_name_spinner.setOnItemSelectedListener(this);



//        task_add_button = (Button) findViewById(R.id.button_add_task);

        tv_left_val = (TextView) findViewById(R.id.tv_am_slider_val_left);
        tv_right_val = (TextView) findViewById(R.id.tv_am_slider_val_right);

        routine_table = (TableLayout) findViewById(R.id.tl_Routine_tasks);
        routine_table.setStretchAllColumns(true);
        updateTable(routineCursor);

        tv_left_val.setText(String.valueOf(multiSlider5.getThumb(0).getValue()).concat(":00"));
        tv_right_val.setText(String.valueOf(multiSlider5.getThumb(1).getValue()).concat(":00"));

        /** End Widget Setup **/

        multiSlider5.setOnThumbValueChangeListener(new MultiSlider.OnThumbValueChangeListener() {
            @Override
            public void onValueChanged(MultiSlider multiSlider,
                                       MultiSlider.Thumb thumb,
                                       int thumbIndex,
                                       int value) {
                if (thumbIndex == 0) {
                    tv_left_val.setText(String.valueOf(value).concat(":00 "));
                    startTime = new DateTime()
                            .withHourOfDay(value % 24)
                            .withMinuteOfHour(0)
                            .withSecondOfMinute(0)
                            .withMillisOfSecond(0)
                            .withDayOfMonth(myCalendar.get(Calendar.DAY_OF_MONTH))
                            .withMonthOfYear(myCalendar.get(Calendar.MONTH)+1)
                            .withYear(myCalendar.get(Calendar.YEAR));
                    Log.d(TAG, "onValueChanged: " + startTime);


                } else {
                    tv_right_val.setText(String.valueOf(value).concat(":00 "));
                    endTime = new DateTime()
                            .withHourOfDay(value == 0 ? 23 : value - 1)
                            .withMinuteOfHour(59)
                            .withSecondOfMinute(59)
                            .withMillisOfSecond(59)
                            .withDayOfMonth(myCalendar.get(Calendar.DAY_OF_MONTH))
                            .withMonthOfYear(myCalendar.get(Calendar.MONTH)+1)
                            .withYear(myCalendar.get(Calendar.YEAR));
                    if (value == 0)
                        endTime.plusDays(-1);
                    Log.d(TAG, "onValueChanged: " + endTime);
                }

            }
        });


    }           // End of oncreate

    private Cursor getRoutineCursor(){
        return mDb.query(RoutineActivityContract.RoutineActivityEntry.TABLE_NAME,
                null,
                null,
                null,
                null,
                null,
                RoutineActivityContract.RoutineActivityEntry.COLUMN_TASK_START_TIME + " DESC");
    }

    private Cursor getTaskNameCursor(){
        return mDb.query(RoutineActivityContract.TaskDescEntry.TABLE_NAME,
                null,
                null,
                null,
                null,
                null,
                RoutineActivityContract.RoutineActivityEntry._ID);
    }

    private String[] getAllTasksArray(Cursor cursor){
        String[] array = new String[cursor.getCount()];
        int i = 0;
        while(cursor.moveToNext()){
            String uname = cursor.getString(cursor.getColumnIndex("name"));
            Log.d(TAG, "getAllTasksArray: " + uname);
            if (uname != null) {
                array[i] = uname;
                i++;
            }
        }
        return array;
    }

//    private String getTaskName(Cursor tCursor){
//
//    }

    private long addNewTask(long taskID, long startTime, long endTime){
        ContentValues cv = new ContentValues();
        long x = -1;

        cv.put(RoutineActivityContract.RoutineActivityEntry.COLUMN_TASK_ID, taskID);
        cv.put(RoutineActivityContract.RoutineActivityEntry.COLUMN_TASK_START_TIME, startTime);
        cv.put(RoutineActivityContract.RoutineActivityEntry.COLUMN_TASK_END_TIME, endTime);

        try {
            x = mDb.insert(RoutineActivityContract.RoutineActivityEntry.TABLE_NAME, null, cv);
            return x;
        } catch (SQLException e) {
            Toast.makeText(this, e.getMessage(), Toast.LENGTH_LONG).show();
        }

        return x;

    }

    @Override
    public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
        this.taskId =  id;
        Log.d(TAG, "onItemSelected: "+ id + position);

    }

    @Override
    public void onNothingSelected(AdapterView<?> parent) {

    }

    public void btnAddTask(View view){
        Log.d(TAG, "btnAddTask: startTime, endTime, taskID"+ startTime + endTime + taskId);
        int overlap = getOverlappingTasks(startTime, endTime);
        Log.d(TAG, "btnAddTask: Overlap "+ overlap);
        if (overlap==0) {
            addNewTask(taskId, startTime.getMillis() / 1000, endTime.getMillis() / 1000);
            routine_table.removeAllViews();
            Log.d(TAG, "btnAddTask: getLastTaskStartTimeHour()" + getLastTaskStartTimeHour());
            int lastTaskStartTimeHourMinus24 = getLastTaskStartTimeHour() - 24;
            int lastTaskStartTimeHourOrZero = lastTaskStartTimeHourMinus24 < 0 ? 0 : lastTaskStartTimeHourMinus24;
            Log.d(TAG, "btnAddTask: x " + lastTaskStartTimeHourOrZero);

            if (getLastTaskStartTimeHour() == lastTaskStartTimeHourOrZero) {
                //            startTime.minusDays(1);
                //            endTime.minusDays(1);

                myCalendar.add(Calendar.DAY_OF_MONTH, -1);
                updateLabel();

                multiSlider5.setMax(24);
                multiSlider5.setMin(0);
            } else {
                multiSlider5.setMax(getLastTaskStartTimeHour());
                multiSlider5.setMin(lastTaskStartTimeHourOrZero);
            }

            updateTable(getRoutineCursor());
        } else {
            Log.i(TAG, "btnAddTask: Trying to show toast");
            Toast.makeText(MainActivity.this,
                    "Task Overlap between " + startTime.toString() + " to " + endTime.toString(),
                    Toast.LENGTH_LONG)
                    .show();
        }
    }

    public void btnBackup(View view){
//        Drive service = createService(this);
        try{
            uploadFile(dbFile);
        } catch (Exception e){
            Toast.makeText(this, e.toString(), Toast.LENGTH_LONG).show();
        }
    }

    public void btnUndo(View view){
        mDb.execSQL("DELETE FROM routine WHERE _id = (SELECT MAX(_id) FROM routine)");
        updateTable(getRoutineCursor());
    }



    public void updateTable(Cursor cursor){
        TableRow headrow = new TableRow(this);
        TextView headtxt1 = new TextView(this);
        headtxt1.setText("Task");
        headtxt1.setGravity(Gravity.CENTER_HORIZONTAL);
        TextView headtxt2 = new TextView(this);
        headtxt2.setText("Start Time");
        headtxt2.setGravity(Gravity.CENTER_HORIZONTAL);
        TextView headtxt3 = new TextView(this);
        headtxt3.setText("End Time");
        headtxt3.setGravity(Gravity.CENTER_HORIZONTAL);
        headrow.addView(headtxt1);
        headrow.addView(headtxt2);
        headrow.addView(headtxt3);
        routine_table.addView(headrow);
        while(cursor.moveToNext()) {
            TableRow row = new TableRow(this);

            TextView nametxt = new TextView(this);
            nametxt.setGravity(Gravity.CENTER_HORIZONTAL);
            String id = cursor.getString(1);
            Cursor taskNameCursor = mDb.query(RoutineActivityContract.TaskDescEntry.TABLE_NAME,
                    new String[]{"task_id", "name"},
                "task_id=" + "'" + id + "'",
                null,
                null,
                null,
                RoutineActivityContract.RoutineActivityEntry._ID);
            if (taskNameCursor.moveToNext()) {
                nametxt.setText(taskNameCursor.getString(taskNameCursor.getColumnIndex("name")));
            }

            TextView addresstxt = new TextView(this);
            addresstxt.setGravity(Gravity.CENTER_HORIZONTAL);
            String startTimeUnixSeconds = cursor.getString(2);
            DateTime stDateTime = new DateTime((long)Integer.parseInt(startTimeUnixSeconds)*1000);
            addresstxt.setText(stDateTime.toString(DateTimeFormat.forPattern("E, HH:mm")));

            TextView IDtxt = new TextView(this);
            IDtxt.setGravity(Gravity.CENTER_HORIZONTAL);
            String endTimeUnixSeconds = cursor.getString(3);
            DateTime eDateTime = new DateTime((long)Integer.parseInt(endTimeUnixSeconds)*1000);
            IDtxt.setText(eDateTime.toString(DateTimeFormat.forPattern("E, HH:mm")));

            row.addView(nametxt);
            row.addView(addresstxt);
            row.addView(IDtxt);
            routine_table.addView(row);
        }
    }

    public DateTime getLastTaskEndTime(){
        String s = new String();
        final Cursor cursor = mDb.rawQuery("" +
                "SELECT end_time " +
                "FROM routine " +
                "ORDER BY end_time DESC " +
                "LIMIT 1;", null);
        if (cursor != null) {
            try {
                if (cursor.moveToFirst()) {
                    s = cursor.getString(0);
                    DateTime e= new DateTime((long)Integer.parseInt(s)*1000);
                    return e;
                }
            } finally {
                cursor.close();
            }


        }
        return new DateTime().withHourOfDay(0);
    }

    public int getLastTaskStartTimeHour(){
        String s;
        final Cursor cursor = mDb.rawQuery("" +
                "SELECT start_time " +
                "FROM routine " +
                "ORDER BY start_time DESC " +
                "LIMIT 1;", null);
        if (cursor != null) {
            try {
                if (cursor.moveToFirst()) {
                    s = cursor.getString(0);
                    DateTime e= new DateTime((long)Integer.parseInt(s)*1000);
                    Log.d(TAG, "getLastTaskStartTimeHour: " + e.toString());
                    return e.getHourOfDay();
                }
            } finally {
                cursor.close();
            }


        }
        return 0;
    }

    public int getOverlappingTasks(DateTime start_time, DateTime end_time){
        String s;
        long task_start_time_s = start_time.getMillis()/1000;
        long task_end_time_s = end_time.getMillis()/1000;
        String sql = "" +
                "SELECT COUNT(*) " +
                "FROM routine " +
                "WHERE " + "(" +
                " start_time >= " + task_start_time_s + " AND " + " end_time >= "+ task_end_time_s + " AND " + "start_time <= "+ task_end_time_s +")" +
                "OR (" +
                " start_time <= " + task_start_time_s + " AND " +  " end_time >= "+ task_end_time_s + ")" +
                "OR (" +
                " start_time <= " + task_start_time_s + " AND " + " end_time <= "+ task_end_time_s + " AND " + " end_time >= " + task_start_time_s +")" +
                ";";
        Log.d(TAG, "getOverlappingTasks: sql : " + sql);
        final Cursor cursor = mDb.rawQuery("" +
                "SELECT COUNT(*) " +
                "FROM routine " +
                "WHERE " + "(" +
                " start_time >= " + task_start_time_s + " AND " + " end_time >= "+ task_end_time_s + " AND " + "start_time <= "+ task_end_time_s +")" +
                "OR (" +
                " start_time <= " + task_start_time_s + " AND " +  " end_time >= "+ task_end_time_s + ")" +
                "OR (" +
                " start_time <= " + task_start_time_s + " AND " + " end_time <= "+ task_end_time_s + " AND " + " end_time >= " + task_start_time_s +")" +
                ";", null);
        if (cursor != null) {
            try {
                if (cursor.moveToFirst()) {
                    s = cursor.getString(0);
                    return Integer.parseInt(s);
                }
            } finally {
                cursor.close();
            }


        }
        return 0;
    }
    private int getNumOfTask(){
        String n = "0" ;
        final Cursor cursor = mDb.rawQuery(
                "SELECT COUNT(*) " +
                "FROM " + RoutineActivityContract.RoutineActivityEntry.TABLE_NAME
                + ";", null);
        if (cursor != null) {
            try {
                if (cursor.moveToFirst()) {
                    n = cursor.getString(0);
                }
            } finally {
                cursor.close();
            }



        }
        return Integer.parseInt(n);

    }

    private void updateLabel() {
        String myFormat = "dd/MM/yy, E"; //In which you need put here
        SimpleDateFormat sdf = new SimpleDateFormat(myFormat, Locale.getDefault());

        datePicker.setText(sdf.format(myCalendar.getTime()));
    }

    public void resetSlider(){
       multiSlider5.setMin(0);
       multiSlider5.setMax(24);
    }

//    private Drive createService(Context context) {
//        SharedPreferences prefs = PreferenceManager
//                .getDefaultSharedPreferences(this);
//        String accountName = prefs.getString("torsho.92@gmail.com", null);
//        GoogleAccountCredential mCredential = GoogleAccountCredential.usingOAuth2(context.getApplicationContext(), Arrays.asList(new String[]{DriveScopes.DRIVE})).setBackOff(new ExponentialBackOff());
//        mCredential.setSelectedAccountName(accountName);
//
//        HttpTransport transport = AndroidHttp.newCompatibleTransport();
//        JsonFactory jsonFactory = JacksonFactory.getDefaultInstance();
//        Drive mService = new Drive.Builder(
//                transport, jsonFactory, mCredential)
//                .setApplicationName(context.getString(R.string.app_name))
//                .build();
//
//        return mService;
//
//    }
//
//    public void uploadFile(java.io.File fileContent, Drive service) {
//        try {
//            com.google.api.services.drive.model.File file = new com.google.api.services.drive.model.File();
//            file.setName("routine_backup.sqlite3");
//
//            List<String> parents = new ArrayList<>(1);
//            parents.add("backup_routinify"); // Here you need to get the parent folder id
//            file.setParents(parents);
//
//            FileContent mediaContent = new FileContent("application/x-sqlite3", fileContent);
////            Drive mService = MainActivity.mService;
//            service.files().create(file, mediaContent).setFields("id").execute();
//            Log.d(TAG, "File uploaded");
//        } catch (IOException e) {
//            Log.e(TAG, "Error uploading file: ", e);
//            e.printStackTrace();
//        }
//    }

//    public void spnSelectItem(View view){
//        Log.d(TAG, "spnSelectItem:  + task_name_spinner.getSelectedItem()" +task_name_spinner.getSelectedItem());
//
//
//    }

    public void uploadFile(File dbFile) throws IOException {
         ActivityCompat.requestPermissions(MainActivity.this,
                    new String[]{Manifest.permission.READ_EXTERNAL_STORAGE
                                ,Manifest.permission.WRITE_EXTERNAL_STORAGE},
                    1);

//        public void onRequestPermissionsResult(int requestCode,
//        String permissions[], int[] grantResults) {
//            switch (requestCode) {
//                case 1: {
//
//                    // If request is cancelled, the result arrays are empty.
//                    if (grantResults.length > 0
//                            && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
//
//                        // permission was granted, yay! Do the
//                        // contacts-related task you need to do.
//                    } else {
//
//                        // permission denied, boo! Disable the
//                        // functionality that depends on this permission.
//                        Toast.makeText(MainActivity.this, "Permission denied to read your External storage", Toast.LENGTH_SHORT).show();
//                    }
//                    return;
//                }
//
//                // other 'case' lines to check for other
//                // permissions this app might request
//            }
//        }

        FileInputStream fis = null;
        try {
            fis = new FileInputStream(dbFile);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        DateTime now = new DateTime();
        String outFileName = Environment.getExternalStorageDirectory()+"/routinify/routine."+now.toString()+".db";

        // Open the empty db as the output stream
        OutputStream output = null;
        try {
            output = new FileOutputStream(outFileName);
        } catch (FileNotFoundException e) {
            File outputDir = new File(outFileName);
            outputDir.mkdirs();
            output = new FileOutputStream(outFileName);
            e.printStackTrace();
            Toast.makeText(this, e.toString(), Toast.LENGTH_LONG).show();
        }

        // Transfer bytes from the inputfile to the outputfile
        byte[] buffer = new byte[1024];
        int length;
        while ((length = fis.read(buffer))>0){
            output.write(buffer, 0, length);
        }

        // Close the streams
        output.flush();
        output.close();
        fis.close();
        Toast.makeText(this, outFileName + " Created Successfully", Toast.LENGTH_LONG).show();
    }
}
