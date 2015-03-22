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

public class TeamDataListActivity extends ListActivity {

	private TeamDataDBAdapter teamDataDBAdapter;
	
	public TeamDataListActivity() {
		// TODO Auto-generated constructor stub
	}

	@Override
    public void onCreate(Bundle savedInstance) {
		super.onCreate(savedInstance);
          setContentView(R.layout.activity_list_team_data);

          this.teamDataDBAdapter = new TeamDataDBAdapter(this).openForWrite();
          
          
//          if(FTSUtilities.DEBUG) {
//        	  FTSUtilities.printToConsole("TeamDataListActivity::onCreate : Populating Test Data\n");
//        	  this.teamDataDBAdapter.populateTestData();
//          }          
          
          Cursor cursor = this.teamDataDBAdapter.getAllEntries();
          startManagingCursor(cursor);
          
          FTSUtilities.printToConsole("TeamDataListActivity::onCreate : Cursor Size: " + cursor.getCount() + "\n");

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
                   long teamID = value.getLong(value.getColumnIndex(TeamDataDBAdapter._ID));
                   Intent myIntent = new Intent(v.getContext(), TeamMatchDataListActivity.class); // OldTeamInformationActivity.class);
                   myIntent.putExtra("position", position);
                   myIntent.putExtra(TeamDataDBAdapter._ID, teamID);
                   startActivityForResult(myIntent, 0);
             }
          });
          
          // SET THIS ADAPTER AS YOUR LISTACTIVITY'S ADAPTER
          this.setListAdapter(mAdapter);
    }
	
	@Override
	protected void onPause() {
		super.onPause();
		this.teamDataDBAdapter.close();
	}
	
	@Override
    protected void onResume() {
        super.onResume();
        this.teamDataDBAdapter.openForWrite();
	}
}
