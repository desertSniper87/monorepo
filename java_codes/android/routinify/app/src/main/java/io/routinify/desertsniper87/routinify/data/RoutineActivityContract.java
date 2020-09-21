package io.routinify.desertsniper87.routinify.data;

import android.provider.BaseColumns;

public class RoutineActivityContract {
    public static final class RoutineActivityEntry implements BaseColumns{
        public static final String TABLE_NAME = "routine";
        public static final String COLUMN_TASK_ID = "task_id";
        public static final String COLUMN_TASK_START_TIME = "start_time";
        public static final String COLUMN_TASK_END_TIME = "end_time";
    }

    public static final class TaskDescEntry implements BaseColumns{
        public static final String TABLE_NAME = "task_desc";
        public static final String COLUMN_TASK_ID = "task_id";
        public static final String  COLUMN_TASK_NAME = "name";
    }
}
