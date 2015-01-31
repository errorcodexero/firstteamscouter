package com.wilsonvillerobotics.firstteamscouter;

import java.util.Hashtable;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.MatchDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.app.Activity;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Color;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.SimpleCursorAdapter;
import android.widget.Spinner;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.TextView;

public class SelectMatchTeamActivity extends Activity {

	protected TeamMatchDBAdapter tmDBAdapter;
	protected String[] teamNumberArray;
	protected long teamID;
	protected long matchID;
	protected Button btnSubmit;
	protected Intent teamMatchIntent;
	private String tabletID;
	
	private TextView txtRed1;
	private TextView txtRed2;
	private TextView txtRed3;
	private TextView txtBlue1;
	private TextView txtBlue2;
	private TextView txtBlue3;
	
	private TextView lblRed1;
	private TextView lblRed2;
	private TextView lblRed3;
	private TextView lblBlue1;
	private TextView lblBlue2;
	private TextView lblBlue3;
	
	protected Boolean fieldOrientationRedOnRight;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_select_team_match);
		
		Intent intent = getIntent();
		this.tabletID = intent.getStringExtra("tablet_id");
		this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);

		txtRed1 = (TextView) findViewById(R.id.txtRed1);
		txtRed2 = (TextView) findViewById(R.id.txtRed2);
		txtRed3 = (TextView) findViewById(R.id.txtRed3);
		txtBlue1 = (TextView) findViewById(R.id.txtBlue1);
		txtBlue2 = (TextView) findViewById(R.id.txtBlue2);
		txtBlue3 = (TextView) findViewById(R.id.txtBlue3);
		
		lblRed1 = (TextView) findViewById(R.id.lblRed1);
		lblRed2 = (TextView) findViewById(R.id.lblRed2);
		lblRed3 = (TextView) findViewById(R.id.lblRed3);
		lblBlue1 = (TextView) findViewById(R.id.lblBlue1);
		lblBlue2 = (TextView) findViewById(R.id.lblBlue2);
		lblBlue3 = (TextView) findViewById(R.id.lblBlue3);
		
		teamID = -1;
		matchID = -1;
		
		try {
			FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
			tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext()).open();
		} catch(SQLException e) {
			e.printStackTrace();
			tmDBAdapter = null;
		}
		
		btnSubmit = (Button) findViewById(R.id.btnSubmitTeamMatch);
		btnSubmit.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				btnSubmitOnClick(v);
				//finish();
			}

			private void btnSubmitOnClick(View v) {
				FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate::btnSubmitOnClick : CLOSING DB\n");
				tmDBAdapter.close();
				startActivity(teamMatchIntent);
			}
		});
		
        activateSubmitButton();
		populateMatchNumberSpinner();
	}

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        if(tmDBAdapter == null) {
        	tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext());
        }
        tmDBAdapter.open();
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
        super.onStop();
        FTSUtilities.printToConsole("SelectTeamMatchActivity::onStop : CLOSING DB\n");
		tmDBAdapter.close();
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

    private void activateSubmitButton() {
        boolean activateSubmit = (this.tabletID.compareTo("Undefined Tablet ID") != 0);
        this.btnSubmit.setEnabled(activateSubmit);
    }

	private void populateMatchNumberSpinner() {
		if(tmDBAdapter == null) return;
		
		Cursor matchNumbers = tmDBAdapter.getAllMatchNumbers();
		FTSUtilities.printToConsole("SelectMatchTeamActivity::populateMatchNumberSpinner : Number of Matches Returned: " + String.valueOf(matchNumbers.getCount()));
		
		Spinner spinMatchNum = (Spinner)findViewById(R.id.spinMatchNumber);
		
		// which columns map to which layout controls
		String[] matchFromColumns = new String[] {MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER, MatchDataDBAdapter._ID};
		int[] matchToControlIDs = new int[] {android.R.id.text1, android.R.id.text2};

		// use a SimpleCursorAdapter
		SimpleCursorAdapter matchCA = new SimpleCursorAdapter(this, android.R.layout.simple_spinner_item, matchNumbers,
			       matchFromColumns,
			       matchToControlIDs);

		
		matchCA.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinMatchNum.setAdapter(matchCA);
		
		spinMatchNum.setOnItemSelectedListener(new OnItemSelectedListener() {

            public void onItemSelected(AdapterView<?> arg0, View arg1,
                    int arg2, long arg3) {
            	
            	matchID = -1;
            	if(arg0.getItemAtPosition(arg2) != null) {
            		Cursor c = (Cursor)arg0.getItemAtPosition(arg2);
            		matchID = c.getLong(c.getColumnIndex(MatchDataDBAdapter._ID));
            		FTSUtilities.printToConsole("SelectTeamMatchActivity::spinMatchNum.onItemSelected : Cursor Length: " + c.getCount() + "  matchID: " + matchID);
            	} else {
            		FTSUtilities.printToConsole("SelectTeamMatchActivity::spinMatchNum.onItemSelected : No item at position " + arg2);
            	}
            	
            	MatchDataDBAdapter mDBAdapter = new MatchDataDBAdapter(getBaseContext()).open();
            	Cursor teamIDs = mDBAdapter.getTeamIDsForMatchByAlliancePosition(matchID);
            	
            	Hashtable<String, String> teamsForMatch = new Hashtable<String, String>();
            	
            	teamsForMatch.put("Red1", teamIDs.getString(teamIDs.getColumnIndexOrThrow(MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_ONE_ID)));
            	teamsForMatch.put("Red2", teamIDs.getString(teamIDs.getColumnIndexOrThrow(MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_TWO_ID)));
            	teamsForMatch.put("Red3", teamIDs.getString(teamIDs.getColumnIndexOrThrow(MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_THREE_ID)));
            	teamsForMatch.put("Blue1", teamIDs.getString(teamIDs.getColumnIndexOrThrow(MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_ONE_ID)));
            	teamsForMatch.put("Blue2", teamIDs.getString(teamIDs.getColumnIndexOrThrow(MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_TWO_ID)));
            	teamsForMatch.put("Blue3", teamIDs.getString(teamIDs.getColumnIndexOrThrow(MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_THREE_ID)));
            	
            	mDBAdapter.close();
            	
            	TeamDataDBAdapter tDBAdapter = new TeamDataDBAdapter(getBaseContext()).open();
            	
            	txtRed1.setText(String.valueOf(tDBAdapter.getTeamNumberFromID(Long.valueOf(teamsForMatch.get("Red1")))));
            	txtRed2.setText(String.valueOf(tDBAdapter.getTeamNumberFromID(Long.valueOf(teamsForMatch.get("Red2")))));
            	txtRed3.setText(String.valueOf(tDBAdapter.getTeamNumberFromID(Long.valueOf(teamsForMatch.get("Red3")))));
            	txtBlue1.setText(String.valueOf(tDBAdapter.getTeamNumberFromID(Long.valueOf(teamsForMatch.get("Blue1")))));
            	txtBlue2.setText(String.valueOf(tDBAdapter.getTeamNumberFromID(Long.valueOf(teamsForMatch.get("Blue2")))));
            	txtBlue3.setText(String.valueOf(tDBAdapter.getTeamNumberFromID(Long.valueOf(teamsForMatch.get("Blue3")))));
            	
            	tDBAdapter.close();
            	
            	if(tabletID.compareTo("Red1") == 0) {
            		lblRed1.setTextColor(Color.RED);
            		txtRed1.setTextColor(Color.RED);
            	} else if(tabletID.compareTo("Red2") == 0) {
            		lblRed2.setTextColor(Color.RED);
            		txtRed2.setTextColor(Color.RED);
            	} else if(tabletID.compareTo("Red3") == 0) {
            		lblRed3.setTextColor(Color.RED);
            		txtRed3.setTextColor(Color.RED);
            	} else if(tabletID.compareTo("Blue1") == 0) {
            		lblBlue1.setTextColor(Color.BLUE);
            		txtBlue1.setTextColor(Color.BLUE);
            	} else if(tabletID.compareTo("Blue2") == 0) {
            		lblBlue2.setTextColor(Color.BLUE);
            		txtBlue2.setTextColor(Color.BLUE);
            	} else if(tabletID.compareTo("Blue3") == 0) {
            		lblBlue3.setTextColor(Color.BLUE);
            		txtBlue3.setTextColor(Color.BLUE);
            	}

                String teamsForMatchID = teamsForMatch.get(tabletID);
                if(teamsForMatchID == null) {
                    teamID = -1;
                } else {
                    teamID = Long.parseLong(teamsForMatchID);
                }

            	long tmID = tmDBAdapter.getTeamMatchID(matchID, teamID);
            	
            	FTSUtilities.printToConsole("SelectTeamMatchActivity::spinTeamNum.onItemSelected : teamID: " + String.valueOf(teamID) + "  tmID: " + String.valueOf(tmID));

                if(arg1 != null) {
                    teamMatchIntent = new Intent(arg1.getContext(), EnterTeamMatchDataActivity.class);
                    teamMatchIntent.putExtra("tablet_id", tabletID);
                    teamMatchIntent.putExtra("field_orientation", fieldOrientationRedOnRight);
                    teamMatchIntent.putExtra("position", arg3);
                    teamMatchIntent.putExtra(TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID, teamID);
                    teamMatchIntent.putExtra(TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID, matchID);
                    teamMatchIntent.putExtra(TeamMatchDBAdapter._ID, String.valueOf(tmID));
                }
            }

            public void onNothingSelected(AdapterView<?> arg0) {
                // do nothing
                
            }
                      
        });
	}
}
