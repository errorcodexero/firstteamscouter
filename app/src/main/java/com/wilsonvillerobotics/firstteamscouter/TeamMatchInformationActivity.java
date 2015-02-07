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
    	FTSUtilities.printToConsole("Destroying TeamInformationActivity");
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
				TextView autoScore = (TextView) findViewById(R.id.txt_TMInfo_AutoScore);
				TextView teleScore = (TextView) findViewById(R.id.txt_TMInfo_TeleScore);
				TextView autoHiHot = (TextView) findViewById(R.id.txt_TMInfo_AutoHiHot);
				TextView autoHiCold = (TextView) findViewById(R.id.txt_TMInfo_AutoHiCold);
				TextView autoHiMiss = (TextView) findViewById(R.id.txt_TMInfo_AutoHiMiss);
				TextView autoLoHot = (TextView) findViewById(R.id.txt_TMInfo_AutoLoHot);
				TextView autoLoCold = (TextView) findViewById(R.id.txt_TMInfo_AutoLoCold);
				TextView autoLoMiss = (TextView) findViewById(R.id.txt_TMInfo_AutoLoMiss);
	
				TextView teleHiCold = (TextView) findViewById(R.id.txt_TMInfo_TeleHiCold);
				TextView teleHiMiss = (TextView) findViewById(R.id.txt_TMInfo_TeleHiMiss);
				TextView teleLoCold = (TextView) findViewById(R.id.txt_TMInfo_TeleLoCold);
				TextView teleLoMiss = (TextView) findViewById(R.id.txt_TMInfo_TeleLoMiss);
				
				TextView autoMove = (TextView) findViewById(R.id.txt_TMInfo_AutoMove);
				TextView autoCollect = (TextView) findViewById(R.id.txt_TMInfo_AutoCollect);
				TextView autoDefend = (TextView) findViewById(R.id.txt_TMInfo_AutoDefend);
				
				TextView teleTrussToss = (TextView) findViewById(R.id.txt_TMInfo_TeleTrussToss);
				TextView teleTrussMiss = (TextView) findViewById(R.id.txt_TMInfo_TeleTrussMiss);
				TextView teleTossCatch = (TextView) findViewById(R.id.txt_TMInfo_TeleTossCatch);
				TextView teleTossMiss = (TextView) findViewById(R.id.txt_TMInfo_TeleTossMiss);
				
				TextView teleAssistRed = (TextView) findViewById(R.id.txt_TMInfo_TeleAssistRed);
				TextView teleAssistWhite = (TextView) findViewById(R.id.txt_TMInfo_TeleAssistWhite);
				TextView teleAssistBlue = (TextView) findViewById(R.id.txt_TMInfo_TeleAssistBlue);
				
				TextView teleDefendRed = (TextView) findViewById(R.id.txt_TMInfo_TeleDefendRed);
				TextView teleDefendWhite = (TextView) findViewById(R.id.txt_TMInfo_TeleDefendWhite);
				TextView teleDefendBlue = (TextView) findViewById(R.id.txt_TMInfo_TeleDefendBlue);
				TextView teleDefendGoal = (TextView) findViewById(R.id.txt_TMInfo_TeleDefendGoal);
				
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
