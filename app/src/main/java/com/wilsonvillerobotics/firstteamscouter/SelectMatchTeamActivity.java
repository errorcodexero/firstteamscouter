package com.wilsonvillerobotics.firstteamscouter;

import java.util.ArrayList;
import java.util.Hashtable;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.MatchDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.app.Activity;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
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
	protected long teamID;
	protected long matchID;
	protected Button btnSubmit;
	protected Intent teamMatchIntent;
    FTSUtilities.ALLIANCE_POSITION tabletAlliancePosition;

    private int matchNumber;

    private ArrayList<TextView> txtTeamNumberField;
    private ArrayList<TextView> txtTeamNumberLabel;

    private final int[] arrayTxtIDs = {R.id.txtRed1,  R.id.txtRed2,  R.id.txtRed3,
                                 R.id.txtBlue1, R.id.txtBlue2, R.id.txtBlue3
    };

    private final int[] arrayLblIDs = {R.id.lblRed1,  R.id.lblRed2,  R.id.lblRed3,
                                 R.id.lblBlue1, R.id.lblBlue2, R.id.lblBlue3
    };

    private final String[] arrayMatchDBFields = {
            MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_ONE_ID,
            MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_TWO_ID,
            MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_THREE_ID,
            MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_ONE_ID,
            MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_TWO_ID,
            MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_THREE_ID
    };

	protected Boolean fieldOrientationRedOnRight;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_select_team_match);

        this.processIntent();

        this.txtTeamNumberField = new ArrayList<TextView>();
        this.txtTeamNumberLabel = new ArrayList<TextView>();

        for(FTSUtilities.ALLIANCE_POSITION ap : FTSUtilities.ALLIANCE_POSITION.validPositions()) {
            txtTeamNumberField.add(ap.allianceIndex(), (TextView) findViewById(arrayTxtIDs[ap.allianceIndex()]));
            txtTeamNumberLabel.add(ap.allianceIndex(), (TextView) findViewById(arrayLblIDs[ap.allianceIndex()]));
        }

		teamID = -1;
		matchID = -1;
		
		try {
			FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
			tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext()).openForWrite();
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
                if(teamMatchIntent != null) {
                    startActivity(teamMatchIntent);
                }
			}
		});
		
        activateSubmitButton();
		populateMatchNumberSpinner();
	}

    private void processIntent() {
        Intent intent = getIntent();
        this.tabletAlliancePosition = FTSUtilities.ALLIANCE_POSITION.getAlliancePositionForString(intent.getStringExtra("tablet_id"));
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
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
        tmDBAdapter.openForWrite();
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
        boolean activateSubmit = (tabletAlliancePosition.positionIsSet());
        this.btnSubmit.setEnabled(activateSubmit);
    }

	private void populateMatchNumberSpinner() {
		if(tmDBAdapter == null || !tabletAlliancePosition.positionIsSet()) return;
		
		final Cursor matchNumbers = tmDBAdapter.getAllMatchNumbers();
		FTSUtilities.printToConsole("SelectMatchTeamActivity::populateMatchNumberSpinner : Number of Matches Returned: " + String.valueOf(matchNumbers.getCount()));
		
		final Spinner spinMatchNum = (Spinner)findViewById(R.id.spinMatchNumber);
		
		// which columns map to which layout controls
		String[] matchFromColumns = new String[] {MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER, MatchDataDBAdapter._ID};
		int[] matchToControlIDs = new int[] {android.R.id.text1, android.R.id.text2};

		// use a SimpleCursorAdapter
		SimpleCursorAdapter matchCA = new SimpleCursorAdapter(this, R.layout.custom_spinner_item, matchNumbers,
			       matchFromColumns,
			       matchToControlIDs);

        matchCA.setDropDownViewResource(R.layout.custom_spinner_dropdown_item);
        spinMatchNum.setAdapter(matchCA);
        int selection = 0;
        if(matchCA.getCount() > 0) {
            selection = matchNumber % matchCA.getCount();
        }
        spinMatchNum.setSelection(selection);

		spinMatchNum.setOnItemSelectedListener(new OnItemSelectedListener() {

            public void onItemSelected(AdapterView<?> arg0, View arg1, int arg2, long arg3) {
            	
            	matchID = -1;
            	if(arg0.getItemAtPosition(arg2) != null) {
            		Cursor c = (Cursor)arg0.getItemAtPosition(arg2);
            		matchID = c.getLong(c.getColumnIndex(MatchDataDBAdapter._ID));
                    matchNumber = arg0.getSelectedItemPosition();
                    FTSUtilities.printToConsole("SelectTeamMatchActivity::spinMatchNum.onItemSelected : Cursor Length: " + c.getCount() + "  matchID: " + matchID);
            	} else {
            		FTSUtilities.printToConsole("SelectTeamMatchActivity::spinMatchNum.onItemSelected : No item at position " + arg2);
            	}
            	
            	MatchDataDBAdapter mDBAdapter = new MatchDataDBAdapter(getBaseContext()).openForWrite();
            	Cursor teamIDs = mDBAdapter.getTeamIDsForMatchByAlliancePosition(matchID);
            	
            	Hashtable<String, String> teamsForMatch = new Hashtable<String, String>();

                for(FTSUtilities.ALLIANCE_POSITION ap : FTSUtilities.ALLIANCE_POSITION.validPositions()) {
                    teamsForMatch.put(ap.myAlliancePosition(), teamIDs.getString(teamIDs.getColumnIndexOrThrow(arrayMatchDBFields[ap.allianceIndex()])));
                }

            	mDBAdapter.close();

                int c = tabletAlliancePosition.getColorForAlliancePosition();
                txtTeamNumberField.get(tabletAlliancePosition.allianceIndex()).setTextColor(c);
                txtTeamNumberLabel.get(tabletAlliancePosition.allianceIndex()).setTextColor(c);

                TeamDataDBAdapter tDBAdapter = new TeamDataDBAdapter(getBaseContext()).openForWrite();

                String strTeamNum;
                String strCurrTeamNum = FTSUtilities.ALLIANCE_POSITION.NOT_SET.myAlliancePosition();
                TextView txtTeam;

                for(FTSUtilities.ALLIANCE_POSITION ap : FTSUtilities.ALLIANCE_POSITION.validPositions()) {
                    //strTeamNum = String.valueOf(tDBAdapter.getTeamNumberFromID(Long.valueOf(teamsForMatch.get(ap.myAlliancePosition()))));
                    // team ID is now the team number + a sub-number for teams with alternate designations (e.g. 1540a, 1540b, etc)
                    strTeamNum = teamsForMatch.get(ap.myAlliancePosition());
                    txtTeamNumberField.get(ap.allianceIndex()).setText(strTeamNum);
                    if(ap == tabletAlliancePosition) {
                        strCurrTeamNum = strTeamNum;
                    }
                }

                tDBAdapter.close();

                String teamIDToScout = teamsForMatch.get(FTSUtilities.getTabletID(tabletAlliancePosition));
                teamID = (teamIDToScout == null) ? -1 : Long.parseLong(teamIDToScout);

            	long tmID = tmDBAdapter.getTeamMatchID(matchID, teamID);
            	
            	FTSUtilities.printToConsole("SelectTeamMatchActivity::spinTeamNum.onItemSelected : teamID: " + String.valueOf(teamID) + "  tmID: " + String.valueOf(tmID));

                if(arg1 != null) {
                    teamMatchIntent = new Intent(arg1.getContext(), MatchTeamNumberDisplayActivity.class);
                } else {
                    teamMatchIntent = new Intent(arg0.getContext(), MatchTeamNumberDisplayActivity.class);
                }
                teamMatchIntent.putExtra("tablet_id", FTSUtilities.getTabletID(tabletAlliancePosition));
                teamMatchIntent.putExtra("field_orientation", fieldOrientationRedOnRight);
                teamMatchIntent.putExtra("match_number", matchNumber);
                teamMatchIntent.putExtra("tmID", tmID);
                teamMatchIntent.putExtra("team_number", strCurrTeamNum);
            }

            public void onNothingSelected(AdapterView<?> arg0) {
                // do nothing
                
            }
                      
        });
	}
}
