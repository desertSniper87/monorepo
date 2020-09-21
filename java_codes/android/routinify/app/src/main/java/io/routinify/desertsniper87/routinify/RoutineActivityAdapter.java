package io.routinify.desertsniper87.routinify;

import android.content.Context;
import android.database.Cursor;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import io.routinify.desertsniper87.routinify.data.RoutineActivityContract;

public class RoutineActivityAdapter {
    private Cursor mCursor;
    private Context mContext;

    public RoutineActivityAdapter(Context context, Cursor cursor){
        this.mContext = context;
        this.mCursor = cursor;
    }

//    public RAViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
//        // Get the RecyclerView item layout
//        LayoutInflater inflater = LayoutInflater.from(mContext);
//        View view = inflater.inflate(R.layout.activity_main, parent, false);
//        return new RAViewHolder(view);
//    }

    public int getItemCount(){
        return mCursor.getCount();
    }






}
