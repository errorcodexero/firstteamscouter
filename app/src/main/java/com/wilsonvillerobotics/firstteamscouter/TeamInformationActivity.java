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
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.SeekBar;
import android.widget.Spinner;
import android.widget.TextView;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.MatchDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.RobotDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by TomS on 2/15/2015.
 */
public class TeamInformationActivity extends Activity implements View.OnClickListener {

    private int selectedPosition;
    private long teamID, robotID, competition_id;
    private long teamNumber;

    private TeamDataDBAdapter    tdDBAdapter;
    private RobotDataDBAdapter   rdDBAdapter;
    private TeamMatchDBAdapter   tmdDBAdapter;
    private MatchDataDBAdapter   mdDBAdapter;

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

    private HashMap<String, String> hmRobotData;
    private final String arrDBColumns[] = {
            RobotDataDBAdapter.COLUMN_NAME_DRIVE_TRAIN_TYPE,
            RobotDataDBAdapter.COLUMN_NAME_WHEEL_TYPE,
            RobotDataDBAdapter.COLUMN_NAME_NUMBER_WHEELS,
            RobotDataDBAdapter.COLUMN_NAME_NUMBER_TOTE_STACKS,
            RobotDataDBAdapter.COLUMN_NAME_NUMBER_TOTES_PER_STACK,
            RobotDataDBAdapter.COLUMN_NAME_NUMBER_CANS_AT_ONCE,
            RobotDataDBAdapter.COLUMN_NAME_GET_STEP_CANS,
            RobotDataDBAdapter.COLUMN_NAME_PUT_TOTES_ON_STEP
    };

    private ArrayList<TextView> alTeamMatchTextViews;

    private Button btnPitPictures, btnRobotPictures, btnRobotNotes, btnDriveTeamData;
    private EditText etOtherDriveTrain, etOtherWheels;
    private SeekBar sbNumToteStacks, sbNumWheels, sbNumTotes, sbNumCans;
    private Spinner spinDriveTrain, spinWheelType;
    private CheckBox cbGetStepCans, cbPutStepTotes;
    private RelativeLayout teamDataLayout;
    private LinearLayout llMatches;
    private ScrollView svMatches;

    private VerticalLabelView lblTeamInfo;
    private VerticalLabelView lblRobotInfo;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_team_information);

        this.processIntent();
        this.initDBAdapters();
        this.configVerticalLabels();
        this.initTeamInfoTextViews();
        this.configMatchList();
        this.configButtons();
        this.configSeekBars();
        this.configSpinners();
        this.conigCheckBoxes();
        this.initRobotDataHashMap();
    }

    private void setSpinnerValue(Spinner spin, int index, EditText etOther, String values[], String colName) {
        if(spin != null) {
            String strOther = (index == values.length - 1) ? hmRobotData.get(colName) : "";
            spin.setSelection(index);
            if(etOther != null) etOther.setText(strOther);
        }
    }

    private int getIndexForString(String values[], String colName) {
        int retVal = values.length - 1;
        for(int i = 0; i < values.length; i++) {
            if(values[i].matches(hmRobotData.get(colName))) {
                retVal = i;
                break;
            }
        }
        return retVal;
    }

    private void initRobotDataHashMap() {
        this.hmRobotData = new HashMap<String, String>();
        //for(String k : arrDBColumns) {
        //    this.hmRobotData.put(k, "");
        //}
    }

    private void conigCheckBoxes() {
        this.cbGetStepCans = (CheckBox)findViewById(R.id.cbGetStepCans);
        this.cbPutStepTotes = (CheckBox)findViewById(R.id.cbPutStepTotes);
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

                boolean enable = strDriveTrainArray[position].compareTo("Other") == 0;
                etOtherDriveTrain.setEnabled(enable);
                etOtherDriveTrain.setFocusable(enable);
                etOtherDriveTrain.setFocusableInTouchMode(enable);
                if(!enable) etOtherDriveTrain.setText("");
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
                Boolean enable = strWheelTypeArray[position].compareTo("Other") == 0;
                etOtherWheels.setEnabled(enable);
                etOtherWheels.setFocusable(enable);
                etOtherWheels.setFocusableInTouchMode(enable);
                if(!enable) etOtherWheels.setText("");
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

    private void configMatchList() {
        this.svMatches = (ScrollView)findViewById(R.id.svMatches);
        this.llMatches = (LinearLayout)findViewById(R.id.llMatches);
        if(svMatches != null && llMatches != null) {
            this.alTeamMatchTextViews = new ArrayList<TextView>();

            try {
                tmdData = tmdDBAdapter.openForRead().getMatchesForTeam(this.teamID, this.competition_id);
                if (tmdData != null && mdDBAdapter != null) {
                    int index = 0;
                    do {
                        long matchId = tmdData.getLong(tmdData.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID));
                        TextView txtMatchNum = new TextView(this);
                        txtMatchNum.setMinimumHeight(100);
                        int matchNum = -1;

                        mdData = mdDBAdapter.openForRead().getMatchDataEntry(matchId);
                        matchNum = mdData.getInt(mdData.getColumnIndex(MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER));

                        txtMatchNum.setText("Match# " + matchNum);
                        long tmId = tmdDBAdapter.getTeamMatchID(matchId, teamID);
                        String alliance = tmdDBAdapter.getTeamAllianceForMatch(tmId);
                        if (alliance.equals("Red")) {
                            txtMatchNum.setTextColor(Color.RED);
                        } else {
                            txtMatchNum.setTextColor(Color.BLUE);
                        }
                        txtMatchNum.setGravity(Gravity.CENTER);
                        txtMatchNum.setTextSize(20.0f);

                        alTeamMatchTextViews.add(index, txtMatchNum);
                        llMatches.addView(txtMatchNum);
                        index++;
                    } while (tmdData.moveToNext());
                }
            }catch (Exception e) {
                tmdData = null;
                mdData = null;
            } finally {
                if(tmdData != null && !tmdData.isClosed()) tmdData.close();
                if(tmdDBAdapter != null && !tmdDBAdapter.dbIsClosed()) tmdDBAdapter.close();
                if(mdData != null && !mdData.isClosed()) mdData.close();
                if(mdDBAdapter != null && !mdDBAdapter.dbIsClosed()) mdDBAdapter.close();
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
            this.hmTeamInfoTextViews.get(R.id.txtPitTeamNum).setText(String.valueOf(this.teamNumber));
        }

        try {
            tdData = tdDBAdapter.openForRead().getTeamDataEntry(this.teamNumber, 0); // TODO - update this so it works with new team_sub_number
            if (tdData != null) {
                if (this.hmTeamInfoTextViews.get(R.id.txtPitTeamName) != null) {
                    this.hmTeamInfoTextViews.get(R.id.txtPitTeamName).setText(
                            tdData.getString(tdData.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_NAME)));
                }

                if (this.hmTeamInfoTextViews.get(R.id.txtPitTeamLocation) != null) {
                    this.hmTeamInfoTextViews.get(R.id.txtPitTeamLocation).setText(
                            tdData.getString(tdData.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_CITY)));
                }

                if (this.hmTeamInfoTextViews.get(R.id.txtTeamNumMembers) != null) {
                    this.hmTeamInfoTextViews.get(R.id.txtTeamNumMembers).setText(
                            tdData.getString(tdData.getColumnIndex(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUM_MEMBERS)));
                }
            }
        } catch (Exception e) {
            tdData = null;
        } finally {
            if(tdData != null && !tdData.isClosed()) tdData.close();
            if(tdDBAdapter != null && !tdDBAdapter.dbIsClosed()) tdDBAdapter.close();
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

    private void initDBAdapters() {
        this.tdData = null;
        this.tmdData = null;
        this.mdData = null;
        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            tdDBAdapter = new TeamDataDBAdapter(this);
            tmdDBAdapter = new TeamMatchDBAdapter(this);
            mdDBAdapter = new MatchDataDBAdapter(this);
            rdDBAdapter = new RobotDataDBAdapter(this);
        } catch(SQLException e) {
            e.printStackTrace();
            tdDBAdapter = null;
            tmdDBAdapter = null;
            mdDBAdapter = null;
            rdDBAdapter = null;
        }
    }

    private void processIntent() {
        Intent intent = getIntent();

        selectedPosition = intent.getIntExtra("position", -1);
        teamID = intent.getLongExtra(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER, -1);
        teamNumber = intent.getLongExtra("team_number", -1);
        competition_id = intent.getLongExtra("competition_id", 0);
    }

    private void loadRobotData() {
        /*if(teamID >= 0 && rdDBAdapter != null) {
            try {
                this.robotID = rdDBAdapter.openForRead().getRobotIdForTeamAtCompetition(teamID, competition_id);
            }catch (Exception e) {
                e.printStackTrace();
            } finally {
                if(rdDBAdapter != null && !rdDBAdapter.dbIsClosed())rdDBAdapter.close();
            }

        }*/
        //if(this.robotID >= 0 && rdDBAdapter != null) {
        if(rdDBAdapter != null) {
            try {
                HashMap<String, String> v = rdDBAdapter.openForRead().getRobotDataEntryForTeamAtCompetition(teamID, competition_id);
                if(v != null && v.size() > 0) {
                    this.hmRobotData = v;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }finally {
                if(rdDBAdapter != null && !rdDBAdapter.dbIsClosed())rdDBAdapter.close();
            }

        }

        if(this.hmRobotData.size() > 0) {
            String strDriveTrainArray[] = getResources().getStringArray(R.array.DriveTrains);
            String strWheelArray[] = getResources().getStringArray(R.array.Wheels);
            int index = 0;

            String tempId =hmRobotData.get(RobotDataDBAdapter._ID);
            if(tempId != null) this.robotID = Long.parseLong(tempId);

            int numStacks = Integer.parseInt(hmRobotData.get(RobotDataDBAdapter.COLUMN_NAME_NUMBER_TOTE_STACKS));
            if(sbNumToteStacks != null) sbNumToteStacks.setProgress(Integer.parseInt(hmRobotData.get(RobotDataDBAdapter.COLUMN_NAME_NUMBER_TOTE_STACKS)));
            if(sbNumWheels != null) sbNumWheels.setProgress(Integer.parseInt(hmRobotData.get(RobotDataDBAdapter.COLUMN_NAME_NUMBER_WHEELS)));
            if(sbNumTotes != null) sbNumTotes.setProgress(Integer.parseInt(hmRobotData.get(RobotDataDBAdapter.COLUMN_NAME_NUMBER_TOTES_PER_STACK)));
            if(sbNumCans != null) sbNumCans.setProgress(Integer.parseInt(hmRobotData.get(RobotDataDBAdapter.COLUMN_NAME_NUMBER_CANS_AT_ONCE)));

            index = getIndexForString(strDriveTrainArray, RobotDataDBAdapter.COLUMN_NAME_DRIVE_TRAIN_TYPE);
            setSpinnerValue(spinDriveTrain, index, etOtherDriveTrain, strDriveTrainArray, RobotDataDBAdapter.COLUMN_NAME_DRIVE_TRAIN_TYPE);

            index = getIndexForString(strWheelArray, RobotDataDBAdapter.COLUMN_NAME_WHEEL_TYPE);
            setSpinnerValue(spinWheelType, index, etOtherWheels, strWheelArray, RobotDataDBAdapter.COLUMN_NAME_WHEEL_TYPE);

            if(cbGetStepCans != null) cbGetStepCans.setChecked(Boolean.parseBoolean(hmRobotData.get(RobotDataDBAdapter.COLUMN_NAME_GET_STEP_CANS)));
            if(cbPutStepTotes != null) cbPutStepTotes.setChecked(Boolean.parseBoolean(hmRobotData.get(RobotDataDBAdapter.COLUMN_NAME_PUT_TOTES_ON_STEP)));
        }
    }

    private void saveRobotData() {
        if(this.hmRobotData != null) {
            String strDriveTrainArray[] = getResources().getStringArray(R.array.DriveTrains);
            String strWheelArray[] = getResources().getStringArray(R.array.Wheels);

            hmRobotData.put(RobotDataDBAdapter._ID, String.valueOf(this.robotID));

            if(sbNumToteStacks != null) hmRobotData.put(RobotDataDBAdapter.COLUMN_NAME_NUMBER_TOTE_STACKS, String.valueOf(sbNumToteStacks.getProgress()));
            if(sbNumWheels != null) hmRobotData.put(RobotDataDBAdapter.COLUMN_NAME_NUMBER_WHEELS, String.valueOf(sbNumWheels.getProgress()));
            if(sbNumTotes != null) hmRobotData.put(RobotDataDBAdapter.COLUMN_NAME_NUMBER_TOTES_PER_STACK, String.valueOf(sbNumTotes.getProgress()));
            if(sbNumCans != null) hmRobotData.put(RobotDataDBAdapter.COLUMN_NAME_NUMBER_CANS_AT_ONCE, String.valueOf(sbNumCans.getProgress()));

            if(spinDriveTrain != null) {
                if(spinDriveTrain.getSelectedItemPosition() == strDriveTrainArray.length - 1 && this.etOtherDriveTrain != null) {
                    hmRobotData.put(RobotDataDBAdapter.COLUMN_NAME_DRIVE_TRAIN_TYPE, etOtherDriveTrain.getText().toString());
                } else {
                    hmRobotData.put(RobotDataDBAdapter.COLUMN_NAME_DRIVE_TRAIN_TYPE, spinDriveTrain.getSelectedItem().toString());
                }
            }

            if(spinWheelType != null) {
                if(spinWheelType.getSelectedItemPosition() == strWheelArray.length - 1 && this.etOtherWheels != null) {
                    hmRobotData.put(RobotDataDBAdapter.COLUMN_NAME_WHEEL_TYPE, etOtherWheels.getText().toString());
                } else {
                    hmRobotData.put(RobotDataDBAdapter.COLUMN_NAME_WHEEL_TYPE, spinWheelType.getSelectedItem().toString());
                }
            }

            if(cbGetStepCans != null) hmRobotData.put(RobotDataDBAdapter.COLUMN_NAME_GET_STEP_CANS, String.valueOf(cbGetStepCans.isChecked()));
            if(cbPutStepTotes != null) hmRobotData.put(RobotDataDBAdapter.COLUMN_NAME_PUT_TOTES_ON_STEP, String.valueOf(cbPutStepTotes.isChecked()));


            try {
                if(this.robotID >= 0) {
                    rdDBAdapter.openForWrite().updateRobotDataEntry(robotID, teamID, competition_id, hmRobotData, true);
                } else {
                    this.robotID = rdDBAdapter.openForWrite().createRobotDataEntry(teamID, competition_id, hmRobotData);
                }
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                if(rdDBAdapter != null && !rdDBAdapter.dbIsClosed())rdDBAdapter.close();
            }
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
        this.loadRobotData();
    }

    @Override
    protected void onPause() {
        super.onPause();
        this.saveRobotData();
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
                c = NoteListActivity.class;
                itemType = "Robot";
                break;
            case R.id.btnDriveTeamData:
                c = PictureListActivity.class;
                break;
            default:
                break;
        }

        Intent intent = new Intent(v.getContext(), c);
        intent.putExtra("team_id", teamID);
        intent.putExtra("competition_id", competition_id);
        intent.putExtra("team_number", teamNumber);
        intent.putExtra("item_type", itemType);
        startActivity(intent);
    }
}
