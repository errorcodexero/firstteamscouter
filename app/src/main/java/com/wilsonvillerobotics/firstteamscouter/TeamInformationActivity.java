package com.wilsonvillerobotics.firstteamscouter;

import com.wilsonvillerobotics.firstteamscouter.TeamInformation;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.os.Bundle;
import android.app.Activity;
import android.database.Cursor;
import android.view.Menu;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

public class TeamInformationActivity extends Activity {

	protected TeamDataDBAdapter tDBAdapter;
	protected TeamInformation tInfo;
	protected Integer teamNumber;
	private Button btnSave;
	private Button btnDeleteTeam;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_team_information);
		
		teamNumber = getIntent().getIntExtra(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER, -1);
		
		FTSUtilities.printToConsole("Creating TeamInformationActivity");
		
		//tDBAdapter = new TeamDataDBAdapter(this.getBaseContext()).open();
		tDBAdapter = new TeamDataDBAdapter(this).open();
		tInfo = new TeamInformation();
		
		this.loadTeamInfo(teamNumber);
		
		btnSave = (Button) findViewById(R.id.btnTeamInfoSave);
		btnSave.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				btnSaveOnClick();
				finish();
			}
		});
		
		btnDeleteTeam = (Button) findViewById(R.id.btnDeleteTeam);
		btnDeleteTeam.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				btnDeleteTeamOnClick();
				finish();
			}
		});
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
	
	public void loadTeamInfo(int teamNum) {
		Cursor cursor = tDBAdapter.getTeamDataEntry(teamNum);
		EditText teamName, teamLocation, teamNumber, teamNumMembers;
		
		try{
			teamName = (EditText) findViewById(R.id.txtTeamName);
			teamNumber = (EditText) findViewById(R.id.txtTeamNum);
			teamLocation = (EditText) findViewById(R.id.txtTeamLocation);
			teamNumMembers = (EditText) findViewById(R.id.txtNumTeamMembers);
			
			teamName.setText(cursor.getString(cursor.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_NAME)));
			teamNumber.setText(cursor.getString(cursor.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER)));
			teamLocation.setText(cursor.getString(cursor.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_LOCATION)));
			teamNumMembers.setText(cursor.getString(cursor.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUM_MEMBERS)));
		} catch (NumberFormatException e) {
			//
		} catch (Exception e) {
			//
		}
	}

	public void btnSaveOnClick() {
		// Gets the data repository in write mode
		//SQLiteDatabase db = tDbHelper.getWritableDatabase();
		int teamID = 0; /// UPDATE THIS TO WORK!!!
		int teamNumber = -1;
		int numTeamMembers = 0;
		String teamName = "", teamLocation = "";
		
		try{
			teamName = ((EditText) findViewById(R.id.txtTeamName)).getText().toString();
			teamLocation = ((EditText) findViewById(R.id.txtTeamLocation)).getText().toString();
			teamNumber = Integer.parseInt(((EditText) findViewById(R.id.txtTeamNum)).getText().toString());
		} catch (NumberFormatException e) {
			teamNumber = -1;
		}
		
		try{
			numTeamMembers = Integer.valueOf(((EditText) findViewById(R.id.txtNumTeamMembers)).getText().toString(), 10);
		} catch (NumberFormatException e) {
			numTeamMembers = 1;
		}
		
		if(!tDBAdapter.updateTeamDataEntry(teamID, teamNumber, teamName, teamLocation, numTeamMembers)) {
			tDBAdapter.createTeamDataEntry(teamNumber, teamName, teamLocation, numTeamMembers);
		}
	}
	
	public void btnDeleteTeamOnClick() {
		int teamNumber = -1;
		
		try{
			teamNumber = Integer.parseInt(((EditText) findViewById(R.id.txtTeamNum)).getText().toString());
		} catch (NumberFormatException e) {
			teamNumber = -1;
		}
		
		tDBAdapter.deleteTeamDataEntry(teamNumber);
	}
}
