package com.wilsonvillerobotics.firstteamscouter;

import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.SimpleCursorAdapter;
import android.widget.AdapterView.OnItemClickListener;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

public class TeamMatchDataListActivity extends ListActivity {

	private TeamMatchDBAdapter tmDataDBAdapter;
	
	public TeamMatchDataListActivity() {
		// Nothing to do here
	}
	
	@Override
    public void onCreate(Bundle savedInstance) {
		super.onCreate(savedInstance);
          setContentView(R.layout.activity_list_team_match_data);

          //this.teamDataDBAdapter = new TeamDataDBAdapter(this).open();
          this.tmDataDBAdapter = new TeamMatchDBAdapter(this).open();
          
          Intent intent = getIntent();
          long teamID = intent.getLongExtra(TeamDataDBAdapter._ID, -1);
  		
          //Cursor cursor = this.teamDataDBAdapter.getAllTeamDataEntries();
          Cursor cursor = this.tmDataDBAdapter.getMatchesForTeam(teamID);
          startManagingCursor(cursor);
          
          FTSUtilities.printToConsole("TeamDataListActivity::onCreate : Cursor Size: " + cursor.getCount() + "\n");

          // THE DESIRED COLUMNS TO BE BOUND
          String[] columns = new String[] { TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID, TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_HAS_SAVED_DATA };
          // THE XML DEFINED VIEWS WHICH THE DATA WILL BE BOUND TO
          int[] to = new int[] { R.id.match_number_entry, R.id.match_data_saved_entry };

          // CREATE THE ADAPTER USING THE CURSOR POINTING TO THE DESIRED DATA AS WELL AS THE LAYOUT INFORMATION
          SimpleCursorAdapter mAdapter = new SimpleCursorAdapter(this, R.layout.team_match_data_list_entry, cursor, columns, to);

          ListView lv = this.getListView();

          lv.setOnItemClickListener(new OnItemClickListener()
          {
             @Override
             public void onItemClick(AdapterView<?> adapter, View v, int position,
                   long arg3) 
             {
                   Cursor value = (Cursor)adapter.getItemAtPosition(position);
                   int tmID = value.getInt(value.getColumnIndex(TeamMatchDBAdapter._ID));
                   // assuming string and if you want to get the value on click of list item
                   // do what you intend to do on click of listview row
                   Intent myIntent = new Intent(v.getContext(), TeamMatchInformationActivity.class);
                   myIntent.putExtra("position", position);
                   myIntent.putExtra(TeamMatchDBAdapter._ID, tmID);
                   startActivityForResult(myIntent, 0);
             }
          });
          
          // SET THIS ADAPTER AS YOUR LISTACTIVITY'S ADAPTER
          this.setListAdapter(mAdapter);
    }
	
	@Override
	protected void onPause() {
		super.onPause();
		//this.teamDataDBAdapter.close();
	}
	
	@Override
    protected void onResume() {
        super.onResume();
        //this.teamDataDBAdapter.open();
	}
}
