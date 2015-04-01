package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;

public class MatchTeamNumberDisplayActivity extends Activity {

	protected Long teamMatchID;
    protected long competitionID;
	public static String myTitle = "Team Number Display";
    private String tabletID;
    private boolean fieldOrientationRedOnRight;
    private int matchNumber;
    private int teamNumber;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_match_teamnumber_display);

        this.processIntent(getIntent());

        TextView txtTeamNumberDisplay = (TextView)findViewById(R.id.txtTeamNumberDisplay);
        txtTeamNumberDisplay.setText(String.valueOf(this.teamNumber));
        
        int backgroundColor = Color.WHITE;
        if(this.tabletID.startsWith("Red")) {
        	backgroundColor = Color.RED;
        } else if(this.tabletID.startsWith("Blue")) {
        	backgroundColor = Color.BLUE;
        }
        txtTeamNumberDisplay.setBackgroundColor(backgroundColor);

        configureSubmitButton();
	}

    private void configureSubmitButton() {
        Button btnSubmitTeamNumber = (Button) findViewById(R.id.btnSubmitTeamNumber);
        btnSubmitTeamNumber.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                btnSubmitOnClick(v);
            }

            private void btnSubmitOnClick(View v) {
                Intent startIntent = new Intent(v.getContext(), MatchStartingPositionActivity.class);
                buildIntent(startIntent);
                startActivity(startIntent);
            }
        });
    }

    private void processIntent(Intent intent) {
        this.tabletID = intent.getStringExtra("tablet_id");
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.teamMatchID = intent.getLongExtra("tmID", -1);
        this.teamNumber = intent.getIntExtra("team_number", -1);
        this.competitionID = intent.getLongExtra("competition_id", -1);
    }

    private void buildIntent(Intent intent) {
        intent.putExtra("tablet_id", tabletID);
        intent.putExtra("field_orientation", fieldOrientationRedOnRight);
        intent.putExtra("match_number", matchNumber);
        intent.putExtra("team_number", teamNumber);
        intent.putExtra("tmID", teamMatchID);
        intent.putExtra("competition_id", competitionID);
    }

    @Override
    protected void onStart() {
        super.onStart();
    }

    @Override
    protected void onRestart() {
        super.onRestart();
    }

    @Override
    protected void onResume() {
        super.onResume();
    }

    @Override
    protected void onPause() {
        super.onPause();
    }

    @Override
    protected void onStop() {
        super.onStop();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }
}
