package com.wilsonvillerobotics.firstteamscouter;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.os.Bundle;
import android.app.Activity;
import android.database.Cursor;
import android.view.Menu;
import android.widget.TextView;

public class TeamMatchInformationActivity extends Activity {

	protected TeamMatchDBAdapter tmDBAdapter;
	protected TeamMatchData tmData;
	private String tabletID = "";
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_team_match_information);
		
		Long tmID = getIntent().getLongExtra(TeamMatchDBAdapter._ID, -1);
		
		FTSUtilities.printToConsole("Creating TeamMatchInformationActivity");
		
		//tDBAdapter = new TeamDataDBAdapter(this.getBaseContext()).open();
		tmDBAdapter = new TeamMatchDBAdapter(this).open();
		tmData = new TeamMatchData(this, this.tabletID, tmID);
		
		this.loadTeamMatchInfo(tmID);
	}

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
    }

    @Override
    protected void onStart() {
        super.onStart();
    }

    @Override
    protected void onResume() {
        super.onResume();
    }

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onPause() {
        super.onPause();
    }

    @Override
    protected void onStop() {
    	FTSUtilities.printToConsole("Destroying OldTeamInformationActivity");
        super.onStop();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.enter_data, menu);
		return true;
	}
	
	public void loadTeamMatchInfo(Long tmID) {
		FTSUtilities.printToConsole("TeamMatchInformationActivity::loadTeamMatchInfo : loading data");
		if(tmID >= 0) {
			Cursor cursor = tmDBAdapter.getTeamMatch(tmID);
			
			try{
				TextView teamNumber = (TextView) findViewById(R.id.txt_TMInfo_TeamNum);
				TextView matchNumber = (TextView) findViewById(R.id.txt_TMInfo_MatchNum);

				teamNumber.setText(cursor.getString(cursor.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID)));
				matchNumber.setText(cursor.getString(cursor.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID)));

			} catch (NumberFormatException e) {
				//
			} catch (Exception e) {
				//
			}
		}
	}
}
