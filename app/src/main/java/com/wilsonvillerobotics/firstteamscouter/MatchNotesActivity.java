package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ContentValues;
import android.content.Intent;
import android.database.SQLException;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.ToggleButton;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.util.HashMap;

public class MatchNotesActivity extends Activity {

	protected TeamMatchDBAdapter tmDBAdapter;
	protected String[] teamNumberArray;
	protected long teamID;
	protected long matchID;
    protected long teamMatchID;
	protected Button btnSubmit;
	protected Intent matchAutoIntent;
	private String tabletID;
    private int matchNumber;
    private int teamNumber;
	
	protected Boolean fieldOrientationRedOnRight;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_notes);
		
		Intent intent = getIntent();
		this.tabletID = intent.getStringExtra("tablet_id");
		this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.teamMatchID = intent.getLongExtra("tmID", -1);
        this.teamNumber = intent.getIntExtra("team_number", -1);

		teamID = -1;
		matchID = -1;
		
		try {
			FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
			tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext());
		} catch(SQLException e) {
			e.printStackTrace();
			tmDBAdapter = null;
		}
		
		btnSubmit = (Button) findViewById(R.id.btnSubmitMatchNotes);
		btnSubmit.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                btnSubmitOnClick(v);
                //finish();
            }

            private void btnSubmitOnClick(View v) {
                FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate::btnSubmitMatchAuto : CLOSING DB\n");
                tmDBAdapter.close();

                Intent notesIntent = new Intent(v.getContext(), SelectMatchTeamActivity.class);
                notesIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                notesIntent.putExtra("tablet_id", tabletID);
                notesIntent.putExtra("field_orientation", fieldOrientationRedOnRight);
                notesIntent.putExtra("match_number", matchNumber + 1);
                startActivity(notesIntent);
            }
        });
	}

    private void loadData() {
        if(tmDBAdapter != null) {
            HashMap<String, String> initialValues = tmDBAdapter.getNotesData(this.teamMatchID);

            if(initialValues != null && initialValues.size() > 0) {
                ToggleButton tb = (ToggleButton) findViewById(R.id.tbtnBrokeDown);
                if (tb != null)
                    tb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_BROKE_DOWN)));

                tb = (ToggleButton) findViewById(R.id.tbtnNoMove);
                if (tb != null)
                    tb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_NO_MOVE)));

                tb = (ToggleButton) findViewById(R.id.tbtnLostConnection);
                if (tb != null)
                    tb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_LOST_CONNECTION)));

                EditText notes = (EditText) findViewById(R.id.txtTMNotes);
                if (notes != null)
                    notes.setText(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_NOTES));

                CheckBox cb = (CheckBox) findViewById(R.id.chkRobotRoleStacker);
                if (cb != null)
                    cb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_TOTE_STACKER)));

                cb = (CheckBox) findViewById(R.id.chkRobotRoleCanKing);
                if (cb != null)
                    cb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_CAN_KINGER)));

                cb = (CheckBox) findViewById(R.id.chkRobotRoleCooperative);
                if (cb != null)
                    cb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_COOPERATIVE)));

                cb = (CheckBox) findViewById(R.id.chkRobotRoleNoodler);
                if (cb != null)
                    cb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_NOODLER)));

                cb = (CheckBox) findViewById(R.id.chkRobotRoleSayWhat);
                if (cb != null)
                    cb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_NI_SAYER)));

                cb = (CheckBox) findViewById(R.id.chkToteControlInsideRobot);
                if (cb != null)
                    cb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_INSIDE)));

                cb = (CheckBox) findViewById(R.id.chkToteControlForkLift);
                if (cb != null)
                    cb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_FORK_LIFT)));

                cb = (CheckBox) findViewById(R.id.chkToteControlHandleGrabber);
                if (cb != null)
                    cb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_HANDLE_GRABBER)));

                cb = (CheckBox) findViewById(R.id.chkToteControlWhatControl);
                if (cb != null)
                    cb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_DROP_ALOT)));

                cb = (CheckBox) findViewById(R.id.chkToteControlGreatControl);
                if (cb != null)
                    cb.setChecked(Boolean.parseBoolean(initialValues.get(TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_GREAT_CONTROL)));
            }
        }
    }

    private void saveData() {
        if(tmDBAdapter != null) {
            HashMap<String, String> initialValues = new HashMap<String, String>();
            ToggleButton tb = (ToggleButton) findViewById(R.id.tbtnBrokeDown);
            if(tb != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_BROKE_DOWN, String.valueOf(tb.isChecked()));

            tb = (ToggleButton) findViewById(R.id.tbtnNoMove);
            if(tb != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_NO_MOVE, String.valueOf(tb.isChecked()));

            tb = (ToggleButton) findViewById(R.id.tbtnLostConnection);
            if(tb != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_LOST_CONNECTION, String.valueOf(tb.isChecked()));

            EditText notes = (EditText) findViewById(R.id.txtTMNotes);
            if(notes != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_NOTES, notes.getText().toString());

            CheckBox cb = (CheckBox) findViewById(R.id.chkRobotRoleStacker);
            if(notes != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_TOTE_STACKER, String.valueOf(cb.isChecked()));

            cb = (CheckBox) findViewById(R.id.chkRobotRoleCanKing);
            if(notes != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_CAN_KINGER, String.valueOf(cb.isChecked()));

            cb = (CheckBox) findViewById(R.id.chkRobotRoleCooperative);
            if(notes != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_COOPERATIVE, String.valueOf(cb.isChecked()));

            cb = (CheckBox) findViewById(R.id.chkRobotRoleNoodler);
            if(notes != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_NOODLER, String.valueOf(cb.isChecked()));

            cb = (CheckBox) findViewById(R.id.chkRobotRoleSayWhat);
            if(notes != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_NI_SAYER, String.valueOf(cb.isChecked()));

            cb = (CheckBox) findViewById(R.id.chkToteControlInsideRobot);
            if(notes != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_INSIDE, String.valueOf(cb.isChecked()));

            cb = (CheckBox) findViewById(R.id.chkToteControlForkLift);
            if(notes != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_FORK_LIFT, String.valueOf(cb.isChecked()));

            cb = (CheckBox) findViewById(R.id.chkToteControlHandleGrabber);
            if(notes != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_HANDLE_GRABBER, String.valueOf(cb.isChecked()));

            cb = (CheckBox) findViewById(R.id.chkToteControlWhatControl);
            if(notes != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_DROP_ALOT, String.valueOf(cb.isChecked()));

            cb = (CheckBox) findViewById(R.id.chkToteControlGreatControl);
            if(notes != null) initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_GREAT_CONTROL, String.valueOf(cb.isChecked()));

            initialValues.put(TeamMatchDBAdapter.COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());

            tmDBAdapter.updateNotesFields(teamMatchID, initialValues);
        }
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
        loadData();
    }

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onPause() {
        super.onPause();
        saveData();
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
}
