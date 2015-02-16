package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.Intent;
import android.database.SQLException;
import android.os.Bundle;
import android.widget.TextView;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamPitsDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitNotesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitPicturesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.RobotPicturesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.RobotNotesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.NotesDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PictureDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.RobotDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

/**
 * Created by TomS on 2/15/2015.
 */
public class PitInformationActivity extends Activity {

    private int selectedPosition;
    private long teamID;
    private String teamNumber;

    private TeamPitsDBAdapter    tpDBAdapter;
    private PitDataDBAdapter     pdDBAdapter;
    private PitNotesDBAdapter    pnDBAdapter;
    private PitPicturesDBAdapter ppDBAdapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_pit_information);

        this.processIntent();

        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            tpDBAdapter = new TeamPitsDBAdapter(this.getBaseContext()).open();
        } catch(SQLException e) {
            e.printStackTrace();
            tpDBAdapter = null;
        }

        TextView txtPitInfo = (TextView)findViewById(R.id.txtPitInfo);
        if(txtPitInfo != null) {
            txtPitInfo.setText("Pit Info for team:\n" + teamNumber);
        }
    }

    private void processIntent() {
        Intent intent = getIntent();

        selectedPosition = intent.getIntExtra("position", -1);
        teamID = intent.getLongExtra(TeamDataDBAdapter._ID, -1);
        teamNumber = intent.getStringExtra("team_number");
    }

    private void loadData() {
        if(teamID >= 0) {

        }
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
}
