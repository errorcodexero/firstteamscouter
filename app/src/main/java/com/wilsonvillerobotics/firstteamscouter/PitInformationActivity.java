package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Color;
import android.os.Bundle;
import android.view.Gravity;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TableRow;
import android.widget.TextView;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.MatchDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamPitsDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitNotesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitPicturesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.util.HashMap;

/**
 * Created by TomS on 2/15/2015.
 */
public class PitInformationActivity extends Activity {

    private int selectedPosition;
    private long teamID;
    private String teamNumber;

    private TeamDataDBAdapter    tdDBAdapter;
    private TeamMatchDBAdapter   tmdDBAdapter;
    private MatchDataDBAdapter   mdDBAdapter;
    private TeamPitsDBAdapter    tpDBAdapter;
    private PitDataDBAdapter     pdDBAdapter;
    private PitNotesDBAdapter    pnDBAdapter;
    private PitPicturesDBAdapter ppDBAdapter;

    private HashMap<Integer, TextView> hmTeamInfoTextViews;
    private final int arrTextViewFieldIDs[] = {
            R.id.txtPitTeamNum,
            R.id.txtPitTeamName,
            R.id.txtPitTeamLocation,
            R.id.txtPitNumMembers
    };

    private HashMap<Integer, TableRow> hmTeamMatchRows;
    private final int arrTableRowIDs[] = {
            R.id.rowPitMatch1,
            R.id.rowPitMatch2,
            R.id.rowPitMatch3
    };

    private Button btnPitPictures, btnRobotPictures;
    private RelativeLayout pitInfoLayout;
    private VerticalLabelView lblPitInfo;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_pit_information);

        this.processIntent();

        Cursor tdData = null;
        Cursor tmdData = null;
        Cursor mdData = null;
        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            tpDBAdapter = new TeamPitsDBAdapter(this).open();
            tdDBAdapter = new TeamDataDBAdapter(this).open();
            tmdDBAdapter = new TeamMatchDBAdapter(this).open();
            mdDBAdapter = new MatchDataDBAdapter(this).open();
            int tNum = Integer.parseInt(this.teamNumber);
            tdData = tdDBAdapter.getTeamDataEntry(tNum);
            tmdData = tmdDBAdapter.getMatchesForTeam(this.teamID);
        } catch(SQLException e) {
            e.printStackTrace();
            tpDBAdapter = null;
            tdDBAdapter = null;
        }

        this.pitInfoLayout = (RelativeLayout)findViewById(R.id.layoutPitInfoRelative);
        this.lblPitInfo    =  (VerticalLabelView)findViewById(R.id.lblPitInfo);
        if(lblPitInfo != null) {
            String teamPitInfo = getResources().getString(R.string.label_pit_info);
            lblPitInfo.setText(teamPitInfo);
        }

        this.hmTeamInfoTextViews = new HashMap<Integer, TextView>();
        for(int id : this.arrTextViewFieldIDs) {
            this.hmTeamInfoTextViews.put(id, (TextView) findViewById(id));
        }

        if(this.hmTeamInfoTextViews.get(R.id.txtPitTeamNum) != null) {
            this.hmTeamInfoTextViews.get(R.id.txtPitTeamNum).setText(this.teamNumber);
        }

        if(tdData != null) {
            if (this.hmTeamInfoTextViews.get(R.id.txtPitTeamName) != null) {
                this.hmTeamInfoTextViews.get(R.id.txtPitTeamName).setText(
                        tdData.getString(tdData.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_NAME)));
            }

            if (this.hmTeamInfoTextViews.get(R.id.txtPitTeamLocation) != null) {
                this.hmTeamInfoTextViews.get(R.id.txtPitTeamLocation).setText(
                        tdData.getString(tdData.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_LOCATION)));
            }

            if (this.hmTeamInfoTextViews.get(R.id.txtPitNumMembers) != null) {
                this.hmTeamInfoTextViews.get(R.id.txtPitNumMembers).setText(
                        tdData.getString(tdData.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUM_MEMBERS)));
            }
        }

        this.hmTeamMatchRows = new HashMap<Integer, TableRow>();
        if(tmdData != null && mdDBAdapter != null) {
            int index = 0;
            do {
                TableRow tr = (TableRow)findViewById(arrTableRowIDs[index]);
                tr.setOrientation(LinearLayout.HORIZONTAL);
                tr.setGravity(Gravity.CENTER);
                hmTeamMatchRows.put(arrTableRowIDs[index], tr);
                long matchId = tmdData.getLong(tmdData.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID));
                mdData = mdDBAdapter.getMatchDataEntry(matchId);
                TextView txtMatchNum = new TextView(this);
                if(mdData != null) {
                    int matchNum = mdData.getInt(mdData.getColumnIndex(MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER));
                    txtMatchNum.setText("Match# " + matchNum);
                    txtMatchNum.setTextColor(Color.WHITE);
                    txtMatchNum.setGravity(Gravity.CENTER);
                    txtMatchNum.setTextSize(20.0f);
                }
                tr.addView(txtMatchNum);
            } while(tmdData.moveToNext() && ++index < 3);
        }

        this.btnPitPictures = (Button)findViewById(R.id.btnPitPictureList);
        this.btnPitPictures.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent pitPicIntent = new Intent(v.getContext(), PictureListActivity.class);
                pitPicIntent.putExtra("team_id", teamID);
                pitPicIntent.putExtra("team_number", teamNumber);
                pitPicIntent.putExtra("image_type", "Pit");
                startActivity(pitPicIntent);
            }
        });

        this.btnRobotPictures = (Button)findViewById(R.id.btnRobotPictures);
        this.btnRobotPictures.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent robotPicIntent = new Intent(v.getContext(), PictureListActivity.class);
                robotPicIntent.putExtra("team_id", teamID);
                robotPicIntent.putExtra("team_number", teamNumber);
                robotPicIntent.putExtra("image_type", "Robot");
                startActivity(robotPicIntent);
            }
        });
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
