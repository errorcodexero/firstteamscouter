package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Color;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.SimpleCursorAdapter;
import android.widget.Spinner;
import android.widget.TextView;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.MatchDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.util.Hashtable;

public class MatchAutoModeActivity extends Activity {

	protected TeamMatchDBAdapter tmDBAdapter;
	protected String[] teamNumberArray;
	protected long teamID;
	protected long matchID;
	protected Button btnSubmit;
	private String tabletID;
    private int matchNumber;
	
	private ImageView imgYellowTote1;
    private ImageView imgYellowTote2;
    private ImageView imgYellowTote3;

    private ImageView imgAllianceCan1;
    private ImageView imgAllianceCan2;
    private ImageView imgAllianceCan3;

    private ImageView imgStepCan1;
    private ImageView imgStepCan2;
    private ImageView imgStepCan3;

    private ImageView imgRobot;

	protected Boolean fieldOrientationRedOnRight;
    protected int robotX;
    protected int robotY;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_automode);
		
		Intent intent = getIntent();
		this.tabletID = intent.getStringExtra("tablet_id");
		this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.robotX = intent.getIntExtra("robot_x", 0);
        this.robotY = intent.getIntExtra("robot_y", 0);

        imgYellowTote1 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgYellowTote2 = (ImageView) findViewById(R.id.imgYellowTote2);
        imgYellowTote3 = (ImageView) findViewById(R.id.imgYellowTote3);

        imgAllianceCan1 = (ImageView) findViewById(R.id.imgGreenCan1);
        imgAllianceCan2 = (ImageView) findViewById(R.id.imgGreenCan2);
        imgAllianceCan3 = (ImageView) findViewById(R.id.imgGreenCan3);

        imgStepCan1 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgStepCan2 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgStepCan3 = (ImageView) findViewById(R.id.imgYellowTote1);

        imgRobot = (ImageView) findViewById(R.id.imgRobot);

		teamID = -1;
		matchID = -1;
		
		try {
			FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
			tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext()).open();
		} catch(SQLException e) {
			e.printStackTrace();
			tmDBAdapter = null;
		}
		
		btnSubmit = (Button) findViewById(R.id.btnSubmitMatchAuto);
		btnSubmit.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                btnSubmitOnClick(v);
                //finish();
            }

            private void btnSubmitOnClick(View v) {
                FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate::btnSubmitMatchAuto : CLOSING DB\n");
                tmDBAdapter.close();

                Intent autoIntent = new Intent(v.getContext(), MatchTeleModeActivity.class);
                autoIntent.putExtra("tablet_id", tabletID);
                autoIntent.putExtra("field_orientation", fieldOrientationRedOnRight);
                autoIntent.putExtra("match_number", matchNumber);
                startActivity(autoIntent);
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
}
