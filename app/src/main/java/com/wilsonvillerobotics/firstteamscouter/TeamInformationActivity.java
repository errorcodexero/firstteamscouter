package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Color;
import android.os.Bundle;
import android.view.Gravity;
import android.view.View;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.SeekBar;
import android.widget.Spinner;
import android.widget.TableLayout;
import android.widget.TableRow;
import android.widget.TextView;
import android.widget.Toast;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.MatchDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamPitsDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitNotesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitPicturesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by TomS on 2/15/2015.
 */
public class TeamInformationActivity extends Activity implements View.OnClickListener {

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

    private Cursor tdData;
    private Cursor tmdData;
    private Cursor mdData;

    private HashMap<Integer, TextView> hmTeamInfoTextViews;
    private final int arrTextViewFieldIDs[] = {
            R.id.txtPitTeamNum,
            R.id.txtPitTeamName,
            R.id.txtPitTeamLocation,
            R.id.txtTeamNumMembers
    };

    private TableLayout tblMatches;
    private ArrayList<TableRow> alTeamMatchRows;

    private Button btnPitPictures, btnRobotPictures, btnRobotNotes, btnDriveTeamData;
    private EditText etOtherDriveTrain, etOtherWheels;
    private SeekBar sbNumToteStacks, sbNumWheels, sbNumTotes, sbNumCans;
    private Spinner spinDriveTrain, spinWheelType;
    private RelativeLayout teamDataLayout;

    private VerticalLabelView lblTeamInfo;
    private VerticalLabelView lblRobotInfo;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_team_information);

        this.processIntent();
        this.initCursors();
        this.configVerticalLabels();
        this.initTeamInfoTextViews();
        this.configMatchTable();
        this.configButtons();
        this.configSeekBars();
        this.configSpinners();
    }

    private void configSpinners() {
        this.etOtherDriveTrain = (EditText)findViewById(R.id.etOtherDriveTrain);
        this.etOtherWheels     = (EditText)findViewById(R.id.etOtherWheels);
        this.spinDriveTrain    = (Spinner)findViewById(R.id.spinDriveTrain);
        this.spinWheelType     = (Spinner)findViewById(R.id.spinWheelType);

        this.etOtherDriveTrain.setText("");
        this.etOtherDriveTrain.setEnabled(false);
        this.etOtherWheels.setText("");
        this.etOtherWheels.setEnabled(false);

        this.spinDriveTrain.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                String strDriveTrainArray[] = getResources().getStringArray(R.array.DriveTrains);

                etOtherDriveTrain.setEnabled(strDriveTrainArray[position].compareTo("Other") == 0);
                //Toast.makeText(getBaseContext(), "Selected pos: " + position + "  val: " + strDriveTrainArray[position], Toast.LENGTH_LONG).show();
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {

            }
        });

        this.spinWheelType.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                String strWheelTypeArray[] = getResources().getStringArray(R.array.Wheels);
                //Toast.makeText(getBaseContext(), "Selected pos: " + position + "  val: " + strWheelTypeArray[position], Toast.LENGTH_LONG).show();
                etOtherWheels.setEnabled(strWheelTypeArray[position].compareTo("Other") == 0);
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {

            }
        });
    }

    private void configSeekBars() {
        /* http://examples.javacodegeeks.com/android/core/widget/seekbar/android-seekbar-example/ */
        this.sbNumToteStacks = (SeekBar)findViewById(R.id.sbNumToteStacks);
        this.sbNumWheels     = (SeekBar)findViewById(R.id.sbNumWheels);
        this.sbNumTotes      = (SeekBar)findViewById(R.id.sbNumTotes);
        this.sbNumCans       = (SeekBar)findViewById(R.id.sbNumCans);
        final EditText etNumToteStacks = (EditText)findViewById(R.id.etNumToteStacks);
        final EditText etNumWheels     = (EditText)findViewById(R.id.etNumWheels);
        final EditText etNumTotes      = (EditText)findViewById(R.id.etNumTotes);
        final EditText etNumCans       = (EditText)findViewById(R.id.etNumCans);

        etNumToteStacks.setText(String.valueOf(0));
        etNumWheels.setText(String.valueOf(0));
        etNumTotes.setText(String.valueOf(0));
        etNumCans.setText(String.valueOf(0));

        etNumToteStacks.setEnabled(false);
        etNumWheels.setEnabled(false);
        etNumTotes.setEnabled(false);
        etNumCans.setEnabled(false);

        this.sbNumToteStacks.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            int progress = 0;

            @Override
            public void onProgressChanged(SeekBar seekBar, int progressValue, boolean fromUser) {
                progress = progressValue;
                etNumToteStacks.setText(String.valueOf(progress));
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                etNumToteStacks.setText(String.valueOf(progress));
            }
        });

        this.sbNumWheels.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            int progress = 0;

            @Override
            public void onProgressChanged(SeekBar seekBar, int progressValue, boolean fromUser) {
                progress = progressValue;
                etNumWheels.setText(String.valueOf(progress));
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                etNumWheels.setText(String.valueOf(progress));
            }
        });

        this.sbNumTotes.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            int progress = 0;

            @Override
            public void onProgressChanged(SeekBar seekBar, int progressValue, boolean fromUser) {
                progress = progressValue;
                etNumTotes.setText(String.valueOf(progress));
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                etNumTotes.setText(String.valueOf(progress));
            }
        });

        this.sbNumCans.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            int progress = 0;

            @Override
            public void onProgressChanged(SeekBar seekBar, int progressValue, boolean fromUser) {
                progress = progressValue;
                etNumCans.setText(String.valueOf(progress));
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                etNumCans.setText(String.valueOf(progress));
            }
        });
    }

    private void configButtons() {
        this.btnPitPictures = (Button)findViewById(R.id.btnPitPictures);
        this.btnPitPictures.setOnClickListener(this);
        this.btnRobotPictures = (Button)findViewById(R.id.btnRobotPictures);
        this.btnRobotPictures.setOnClickListener(this);
        this.btnRobotNotes = (Button)findViewById(R.id.btnRobotNotes);
        this.btnRobotNotes.setOnClickListener(this);
        this.btnDriveTeamData = (Button)findViewById(R.id.btnDriveTeamData);
        this.btnDriveTeamData.setOnClickListener(this);
    }

    private void configMatchTable() {
        this.tblMatches = (TableLayout)findViewById(R.id.tblPitMatchList);
        if(tblMatches != null) {
            this.alTeamMatchRows = new ArrayList<TableRow>();
            if (tmdData != null && mdDBAdapter != null) {
                int index = 0;
                do {
                    TableRow tr = new TableRow(this);
                    int trId = FTSUtilities.generateViewId();
                    tr.setId(trId);
                    tr.setOrientation(LinearLayout.HORIZONTAL);
                    tr.setGravity(Gravity.CENTER);
                    alTeamMatchRows.add(tr);
                    long matchId = tmdData.getLong(tmdData.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID));
                    mdData = mdDBAdapter.getMatchDataEntry(matchId);
                    TextView txtMatchNum = new TextView(this);
                    if (mdData != null) {
                        int matchNum = mdData.getInt(mdData.getColumnIndex(MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER));
                        txtMatchNum.setText("Match# " + matchNum);
                        long tmId = tmdDBAdapter.getTeamMatchID(matchId, teamID);
                        String alliance = tmdDBAdapter.getTeamAllianceForMatch(tmId);
                        if(alliance.equals("Red")) {
                            txtMatchNum.setTextColor(Color.RED);
                        } else {
                            txtMatchNum.setTextColor(Color.BLUE);
                        }
                        txtMatchNum.setGravity(Gravity.CENTER);
                        txtMatchNum.setTextSize(20.0f);
                    }
                    tr.addView(txtMatchNum);
                    this.tblMatches.addView(tr);
                    index++;
                } while (tmdData.moveToNext());
            }
        }
    }

    private void initTeamInfoTextViews() {
        this.teamDataLayout = (RelativeLayout)findViewById(R.id.rlTeamData);
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

            if (this.hmTeamInfoTextViews.get(R.id.txtTeamNumMembers) != null) {
                this.hmTeamInfoTextViews.get(R.id.txtTeamNumMembers).setText(
                        tdData.getString(tdData.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUM_MEMBERS)));
            }
        }
    }

    private void configVerticalLabels() {
        this.lblTeamInfo =  (VerticalLabelView)findViewById(R.id.lblTeamInfo);
        if(lblTeamInfo != null) {
            String teamPitInfo = getResources().getString(R.string.label_team_info);
            this.lblTeamInfo.setText(teamPitInfo);
        }

        this.lblRobotInfo = (VerticalLabelView)findViewById(R.id.lblRobotInfo);
        if(this.lblRobotInfo != null) {
            String teamRobotInfo = getResources().getString(R.string.label_robot_info);
            this.lblRobotInfo.setText(teamRobotInfo);
        }
    }

    private void initCursors() {
        this.tdData = null;
        this.tmdData = null;
        this.mdData = null;
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
            tmdDBAdapter = null;
            mdDBAdapter = null;
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

    @Override
    public void onClick(View v) {
        Class c = null;
        String itemType = "";
        switch(v.getId()) {
            case R.id.btnPitPictures:
                c = PictureListActivity.class;
                itemType = "Pit";
                break;
            case R.id.btnRobotPictures:
                c = PictureListActivity.class;
                itemType = "Robot";
                break;
            case R.id.btnRobotNotes:
                c = PictureListActivity.class;
                break;
            case R.id.btnDriveTeamData:
                c = PictureListActivity.class;
                break;
            default:
                break;
        }

        Intent intent = new Intent(v.getContext(), c);
        intent.putExtra("team_id", teamID);
        intent.putExtra("team_number", teamNumber);
        intent.putExtra("item_type", itemType);
        startActivity(intent);
    }
}
