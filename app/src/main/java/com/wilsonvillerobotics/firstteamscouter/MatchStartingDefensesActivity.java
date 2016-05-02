package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ClipData;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.os.Bundle;
import android.view.DragEvent;
import android.view.Menu;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.ToggleButton;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities.ALLIANCE_POSITION;

import java.util.ArrayList;

public class MatchStartingDefensesActivity extends Activity implements View.OnClickListener {

	protected TeamMatchDBAdapter tmDBAdapter;
    protected Long teamMatchID;
	protected long teamID;
	protected long matchID;
    protected long competitionID;
    protected int teamNumber;
	protected Button btnSubmit;
    private ALLIANCE_POSITION tabletAlliancePosition;
    private int matchNumber;

    protected Boolean fieldOrientationRedOnRight;
    private boolean dataChanged;

    protected ArrayList<ToggleButton>defenseButtons;
    protected String defenseA, defenseB, defenseC, defenseD;
    protected Button btnNext;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_starting_defenses);

        dataChanged = false;
        defenseButtons = new ArrayList<ToggleButton>(8);
        defenseButtons.add((ToggleButton)findViewById(R.id.btnPortcullis));
        defenseButtons.add((ToggleButton)findViewById(R.id.btnChevalDeFrise));
        defenseButtons.add((ToggleButton)findViewById(R.id.btnRamparts));
        defenseButtons.add((ToggleButton)findViewById(R.id.btnMoat));
        defenseButtons.add((ToggleButton)findViewById(R.id.btnDrawbridge));
        defenseButtons.add((ToggleButton)findViewById(R.id.btnSallyPort));
        defenseButtons.add((ToggleButton)findViewById(R.id.btnRockWall));
        defenseButtons.add((ToggleButton)findViewById(R.id.btnRoughTerrain));
        for(int i = 0; i < defenseButtons.size(); i++) {
            defenseButtons.get(i).setOnClickListener(this);
        }

        btnNext = (Button)findViewById(R.id.btnNext);
        btnNext.setOnClickListener(this);

		//this.processIntent(getIntent());

        teamID = -1;
		matchID = -1;
        //this.openDatabase();
        //this.configureSubmitButton();

        defenseA = "Defense was not seleced";
        defenseB = "Defense was not seleced";
        defenseC = "Defense was not seleced";
        defenseD = "Defense was not seleced";
	}

    private void processIntent(Intent intent) {
        this.tabletAlliancePosition = ALLIANCE_POSITION.getAlliancePositionForString(intent.getStringExtra("tablet_id"));
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.teamMatchID = intent.getLongExtra("tmID", -1);
        this.teamNumber  = intent.getIntExtra("team_number", -1);
        this.competitionID = intent.getLongExtra("competition_id", -1);
    }


    private void openDatabase() {
        try {
            FTSUtilities.printToConsole("MatchStartingPositionActivity::onCreate : OPENING DB\n");
            tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext()).openForWrite();
        } catch(SQLException e) {
            e.printStackTrace();
            tmDBAdapter = null;
        }
    }

    private void loadData() {
        if(this.tmDBAdapter != null) {
            Cursor C = this.tmDBAdapter.getStartingPositionData(this.teamMatchID);
            if(C != null && C.getCount() > 0) {
                //Load Data
            }
        }
    }

    private boolean saveData() {
        if(this.tmDBAdapter != null && dataChanged) {
            return true;
        }
        return false;
    }

    private void configureSubmitButton() {
        btnSubmit = (Button) findViewById(R.id.btnSubmitStartingPosition);
        btnSubmit.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                btnSubmitOnClick(v);
                //finish();
            }

            private void btnSubmitOnClick(View v) {
                Intent startingPositionIntent = new Intent(v.getContext(), MatchAutoModeActivity.class);
                buildIntent(startingPositionIntent);
                startActivity(startingPositionIntent);
            }
        });
    }

    private void buildIntent(Intent intent) {
        //intent.putExtra("tablet_id", tabletAlliancePosition.myAlliancePosition());
        //intent.putExtra("tablet_id", tabletID);
        //intent.putExtra("field_orientation", fieldOrientationRedOnRight);
        //intent.putExtra("match_number", matchNumber);
        //intent.putExtra("team_number", teamNumber);
        //intent.putExtra("tmID", teamMatchID);
        //intent.putExtra("competition_id", competitionID);
        intent.putExtra("DefenseA", defenseA);
        intent.putExtra("DefenseB", defenseB);
        intent.putExtra("DefenseC", defenseC);
        intent.putExtra("DefenseD", defenseD);
    }



    // Overridden Activity Methods Section
    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        openDatabase();
    }

    @Override
    protected void onStart() {
        super.onStart();
    }

    @Override
    protected void onResume() {
        super.onResume();
        this.loadData();
    }

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onPause() {
        super.onPause();
        this.saveData();
    }

    @Override
    protected void onStop() {
        super.onStop();
        FTSUtilities.printToConsole("MatchStartingDefensesActivity::onStop : CLOSING DB\n");
		//tmDBAdapter.close();
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

    @Override
    public void onClick(View view) {
        switch(view.getId()) {
            case R.id.btnPortcullis:
                defenseA = "Portcullis";
                defenseButtons.get(1).setChecked(false);
                break;
            case R.id.btnChevalDeFrise:
                defenseA = "Cheval de Frise";
                defenseButtons.get(0).setChecked(false);
                break;
            case R.id.btnRamparts:
                defenseB = "Ramparts";
                defenseButtons.get(3).setChecked(false);
                break;
            case R.id.btnMoat:
                defenseB = "Moat";
                defenseButtons.get(2).setChecked(false);
                break;
            case R.id.btnDrawbridge:
                defenseC = "Drawbridge";
                defenseButtons.get(5).setChecked(false);
                break;
            case R.id.btnSallyPort:
                defenseC = "Sally Port";
                defenseButtons.get(4).setChecked(false);
                break;
            case R.id.btnRockWall:
                defenseD = "Rock Wall";
                defenseButtons.get(7).setChecked(false);
                break;
            case R.id.btnRoughTerrain:
                defenseD = "Rough Terrain";
                defenseButtons.get(6).setChecked(false);
                break;
            case R.id.btnNext:
                Intent intent = new Intent(view.getContext(), MatchTeleopActivity.class);
                buildIntent(intent);
                startActivity(intent);
            default:
                break;
        }
    }
}
