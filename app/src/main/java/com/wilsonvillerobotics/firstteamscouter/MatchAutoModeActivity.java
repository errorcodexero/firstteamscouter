package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ClipData;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Color;
import android.os.Bundle;
import android.view.DragEvent;
import android.view.Menu;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;
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
    protected long teamMatchID;
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
    private ImageView imgStepCan4;

    private ImageView imgRobot;

	protected Boolean fieldOrientationRedOnRight;
    protected int robotX;
    protected int robotY;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_automode);

        RelativeLayout automodeParentLayout = (RelativeLayout) findViewById(R.id.AutoMode_Field_LayoutRelative);
        automodeParentLayout.setOnDragListener(new MyViewDragListener());
        processIntent(getIntent());
        setBackground(automodeParentLayout);
        configureTotesAndCans();
        createRobotImageView();

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
                buildIntent(autoIntent);
                startActivity(autoIntent);
            }
        });
	}

    private void processIntent(Intent intent) {
        this.tabletID = intent.getStringExtra("tablet_id");
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.teamMatchID = intent.getLongExtra("tmID", -1);
        this.robotX = intent.getIntExtra("robot_x", 0);
        this.robotY = intent.getIntExtra("robot_y", 0);
    }

    private void buildIntent(Intent intent) {
        intent.putExtra("tablet_id", tabletID);
        intent.putExtra("field_orientation", fieldOrientationRedOnRight);
        intent.putExtra("match_number", matchNumber);
        intent.putExtra("tmID", teamMatchID);
        intent.putExtra("robot_x", robotX);
        intent.putExtra("robot_y", robotY);
    }

    private void setBackground(RelativeLayout automodeParentLayout) {
        int backgroundResource = R.drawable.automode_background_2015;
        if(this.tabletID.startsWith("Red")) {
            backgroundResource = R.drawable.automode_background_2015;
        } else if(this.tabletID.startsWith("Blue")) {
            backgroundResource = R.drawable.automode_background_2015;
        }
        automodeParentLayout.setBackgroundResource(backgroundResource);
    }

    private void createRobotImageView() {
        imgRobot = new ImageView(getBaseContext());
        imgRobot.setImageDrawable(getResources().getDrawable(R.drawable.robot_50x50));
        imgRobot.setOnTouchListener(new MyViewTouchListener());
        placeRobotOnScreen();
    }

    private void setRobotLayout() {
        RelativeLayout.LayoutParams robotLayoutParams = new RelativeLayout.LayoutParams(
                getResources().getDimensionPixelSize(R.dimen.robot_height),
                getResources().getDimensionPixelSize(R.dimen.robot_width)
        );
        robotLayoutParams.setMargins(this.robotX - robotLayoutParams.width/2, this.robotY - robotLayoutParams.height/2, 0, 0);
        robotLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
        robotLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_TOP);
        imgRobot.setLayoutParams(robotLayoutParams);
    }

    private void placeRobotOnScreen() {
        RelativeLayout relLayout = (RelativeLayout) findViewById(R.id.AutoMode_Field_LayoutRelative);
        this.setRobotLayout();
        relLayout.addView(imgRobot);
    }

    private void setViewLayout(View v, int left, int top) {
        RelativeLayout.LayoutParams viewLayoutParams = new RelativeLayout.LayoutParams(
                v.getHeight(),
                v.getWidth()
        );
        viewLayoutParams.setMargins(left - viewLayoutParams.width/2, top - viewLayoutParams.height/2, 0, 0);
        viewLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
        viewLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_TOP);
        v.setLayoutParams(viewLayoutParams);
    }

    private void configureTotesAndCans() {
        imgYellowTote1 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgYellowTote1.setOnTouchListener(new MyViewTouchListener());
        imgYellowTote2 = (ImageView) findViewById(R.id.imgYellowTote2);
        imgYellowTote2.setOnTouchListener(new MyViewTouchListener());
        imgYellowTote3 = (ImageView) findViewById(R.id.imgYellowTote3);
        imgYellowTote3.setOnTouchListener(new MyViewTouchListener());

        imgAllianceCan1 = (ImageView) findViewById(R.id.imgGreenCan1);
        imgAllianceCan1.setOnTouchListener(new MyViewTouchListener());
        imgAllianceCan2 = (ImageView) findViewById(R.id.imgGreenCan2);
        imgAllianceCan2.setOnTouchListener(new MyViewTouchListener());
        imgAllianceCan3 = (ImageView) findViewById(R.id.imgGreenCan3);
        imgAllianceCan3.setOnTouchListener(new MyViewTouchListener());

        imgStepCan1 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgStepCan1.setOnTouchListener(new MyViewTouchListener());
        imgStepCan2 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgStepCan2.setOnTouchListener(new MyViewTouchListener());
        imgStepCan3 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgStepCan3.setOnTouchListener(new MyViewTouchListener());
        imgStepCan4 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgStepCan4.setOnTouchListener(new MyViewTouchListener());
    }

    private final class MyViewTouchListener implements View.OnTouchListener {
        @Override
        public boolean onTouch(View view, MotionEvent motionEvent) {
            if (motionEvent.getAction() == MotionEvent.ACTION_DOWN) {
                ClipData data = ClipData.newPlainText("", "");
                View.DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(view);
                view.startDrag(data, shadowBuilder, view, 0);
                return true;
            } else if (motionEvent.getAction() == MotionEvent.ACTION_MOVE) {
                view.setVisibility(View.GONE);
                return true;
            } else if (motionEvent.getAction() == MotionEvent.ACTION_UP) {
                return true;
            } else {
                return false;
            }
        }
    }

    private void openDatabase() {
        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext()).open();
        } catch(SQLException e) {
            e.printStackTrace();
            tmDBAdapter = null;
        }
    }

    private void loadData() {
        if(this.tmDBAdapter != null) {
            Cursor C = this.tmDBAdapter.getStartingPosition(this.teamMatchID);
            if(C != null && C.getCount() > 0) {
                this.robotX = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_START_LOCATION_X));
                //this.txtRobotX.setText(String.valueOf(this.robotX));
                this.robotY = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_START_LOCATION_Y));
                //this.txtRobotY.setText(String.valueOf(this.robotY));
            }
        }
    }

    private boolean saveData() {
        if(this.tmDBAdapter != null) {
            return false; //this.tmDBAdapter.setStartingPosition(this.teamMatchID, this.robotX, this.robotY, this.robotOnField);
        }
        return false;
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

    private class MyViewDragListener implements View.OnDragListener {
        boolean dragging = false;
        @Override
        public boolean onDrag(View v, DragEvent event) {
            int action = event.getAction();
            switch (action) {
                case DragEvent.ACTION_DRAG_STARTED:
                    dragging = true;
                    //robotOnField = false;
                    break;
                case DragEvent.ACTION_DRAG_ENTERED:
                    //v.setBackgroundDrawable(enterShape);
                    break;
                case DragEvent.ACTION_DRAG_EXITED:
                    //v.setBackgroundDrawable(normalShape);
                    break;
                case DragEvent.ACTION_DROP:
                    // Dropped, reassign View to ViewGroup
                    dragging = false;
                    //robotOnField = true;
                    View view = (View) event.getLocalState();

                    ViewGroup owner = (ViewGroup) view.getParent();
                    owner.removeView(view);
                    RelativeLayout container = (RelativeLayout) v;
                    container.addView(view);

                    int left = (int) event.getX();
                    int top = (int) event.getY();
                    if(view.getId() == R.id.imgRobot) {
                        robotX = left;
                        robotY = top;
                        setRobotLayout();
                    } else {
                        setViewLayout(view, left, top);
                    }
                    view.setVisibility(View.VISIBLE);

                    //txtRobotX.setText(String.valueOf(robotX));
                    //txtRobotY.setText(String.valueOf(robotY));
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    //v.setBackgroundDrawable(normalShape);
                case DragEvent.ACTION_DRAG_LOCATION:
                    if(dragging) {
                        robotX = (int) event.getX();
                        robotY = (int) event.getY();
                        //txtRobotX.setText(String.valueOf(robotX));
                        //txtRobotY.setText(String.valueOf(robotY));
                    }
                    break;
                default:
                    break;
            }
            return true;
        }
    }
}
