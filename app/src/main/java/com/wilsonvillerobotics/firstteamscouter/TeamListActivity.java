package com.wilsonvillerobotics.firstteamscouter;

import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.SimpleCursorAdapter;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

public class TeamListActivity extends ListActivity {

	private TeamDataDBAdapter teamDataDBAdapter;

	public TeamListActivity() {
		// TODO Auto-generated constructor stub
	}

	@Override
    public void onCreate(Bundle savedInstance) {
		super.onCreate(savedInstance);
        setContentView(R.layout.activity_list_team_data);

        this.teamDataDBAdapter = new TeamDataDBAdapter(this);

        configureListView();
    }

    private void configureListView() {
        Cursor cursor = initCursor();
        setListViewAdapter(cursor);
    }

    private void setListViewAdapter(Cursor cursor) {
        // THE DESIRED COLUMNS TO BE BOUND
        String[] columns = new String[] { TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER, TeamDataDBAdapter.COLUMN_NAME_TEAM_NAME };
        // THE XML DEFINED VIEWS WHICH THE DATA WILL BE BOUND TO
        int[] to = new int[] { R.id.number_entry, R.id.name_entry };

        // CREATE THE ADAPTER USING THE CURSOR POINTING TO THE DESIRED DATA AS WELL AS THE LAYOUT INFORMATION
        SimpleCursorAdapter mAdapter = new SimpleCursorAdapter(this, R.layout.team_data_list_entry, cursor, columns, to);

        ListView lv = this.getListView();

        lv.setOnItemClickListener(new OnItemClickListener()
        {
           @Override
           public void onItemClick(AdapterView<?> adapter, View v, int position,
                 long arg3)
           {
                 Cursor value = (Cursor)adapter.getItemAtPosition(position);
                 long teamID = value.getLong(value.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER));
                 String teamNum = value.getString(value.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER));
                 Intent myIntent = new Intent(v.getContext(), TeamInformationActivity.class);
                 myIntent.putExtra("position", position);
                 myIntent.putExtra("team_number", teamNum);
                 myIntent.putExtra(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER, teamID);
                 startActivityForResult(myIntent, 0);
           }
        });

        // SET THIS ADAPTER AS YOUR LISTACTIVITY'S ADAPTER
        this.setListAdapter(mAdapter);
    }

    private Cursor initCursor() {
        Cursor cursor = this.teamDataDBAdapter.getAllEntries();
        startManagingCursor(cursor);

        FTSUtilities.printToConsole("TeamListActivity::onCreate : Cursor Size: " + cursor.getCount() + "\n");
        return cursor;
    }

    @Override
	protected void onPause() {
		super.onPause();
        ListView lv = this.getListView();
        stopManagingCursor(((SimpleCursorAdapter)lv.getAdapter()).getCursor());
		this.teamDataDBAdapter.close();
	}
	
	@Override
    protected void onResume() {
        super.onResume();
        //configureListView();
        //this.teamDataDBAdapter.openForWrite();
	}

    @Override
    protected void onRestart() {
        super.onRestart();
        configureListView();
    }
}
