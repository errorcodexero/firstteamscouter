package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ClipData;
import android.content.Intent;
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

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

public class MatchStartingPositionActivity extends Activity {

	protected TeamMatchDBAdapter tmDBAdapter;
    private TeamMatchData tmData;
    protected Long teamMatchID;
	protected String[] teamNumberArray;
	protected long teamID;
	protected long matchID;
	protected Button btnSubmit;
	private String tabletID;
    private int matchNumber;

    private ImageView imgRobot;

    protected int robotX;
    protected int robotY;

    protected Boolean fieldOrientationRedOnRight;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_starting_position);

        RelativeLayout startingPositionRobotGutterLayout = (RelativeLayout) findViewById(R.id.StartingPosition_RobotGutter_LinearLayout);
        RelativeLayout startingPositionFieldLayout       = (RelativeLayout) findViewById(R.id.StartingPosition_Field_LayoutRelative);
		
		this.processIntent(getIntent());
        this.setBackground(startingPositionFieldLayout);
        startingPositionRobotGutterLayout.setOnDragListener(new MyDragListener());
        startingPositionFieldLayout.setOnDragListener(new MyFieldDragListener());

        imgRobot = (ImageView) findViewById(R.id.imgRobot);
        imgRobot.setOnTouchListener(new MyRobotTouchListener());

        //this.setRobotLayout();

        teamID = -1;
		matchID = -1;
        this.openDatabase();
        this.configureSubmitButton();
	}

    private void processIntent(Intent intent) {
        this.tabletID = intent.getStringExtra("tablet_id");
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.teamMatchID = intent.getLongExtra("tmID", -1);
    }

    private void setBackground(RelativeLayout startingPositionParentLayout) {
        int backgroundResource = R.drawable.starting_position_background_2015;
        if(this.tabletID.startsWith("Red")) {
            backgroundResource = R.drawable.starting_position_background_2015; //starting_position_red_background;
        } else if(this.tabletID.startsWith("Blue")) {
            backgroundResource = R.drawable.starting_position_background_2015; //starting_position_blue_background;
        }
        startingPositionParentLayout.setBackgroundResource(backgroundResource);
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

    private void configureSubmitButton() {
        btnSubmit = (Button) findViewById(R.id.btnSubmitStartingPosition);
        btnSubmit.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                btnSubmitOnClick(v);
                //finish();
            }

            private void btnSubmitOnClick(View v) {
                //FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate::btnSubmitMatchAuto : CLOSING DB\n");
                //tmDBAdapter.close();

                Intent startingPositionIntent = new Intent(v.getContext(), MatchAutoModeActivity.class);
                startingPositionIntent.putExtra("tablet_id", tabletID);
                startingPositionIntent.putExtra("field_orientation", fieldOrientationRedOnRight);
                startingPositionIntent.putExtra("match_number", matchNumber);
                startingPositionIntent.putExtra("robot_x", robotX);
                startingPositionIntent.putExtra("robot_y", robotY);
                startActivity(startingPositionIntent);
            }
        });
    }

    private void setRobotLayout() {
        RelativeLayout.LayoutParams robotLayoutParams = new RelativeLayout.LayoutParams((int)getResources().getDimension(R.dimen.robot_height), (int)getResources().getDimension(R.dimen.robot_width));
        robotLayoutParams.leftMargin = this.robotX;
        robotLayoutParams.topMargin = this.robotY;
        robotLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
        imgRobot.setLayoutParams(robotLayoutParams);
    }

    private final class MyRobotTouchListener implements View.OnTouchListener {
        @Override
        public boolean onTouch(View view, MotionEvent motionEvent) {
            if (motionEvent.getAction() == MotionEvent.ACTION_DOWN) {
                ClipData data = ClipData.newPlainText("", "");
                View.DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(view);
                view.startDrag(data, shadowBuilder, view, 0);
                //view.setVisibility(View.INVISIBLE);
                return true;
            } else if (motionEvent.getAction() == MotionEvent.ACTION_MOVE) {
                view.setVisibility(View.GONE);
                return true;
            } else if (motionEvent.getAction() == MotionEvent.ACTION_UP) {
                //view.setVisibility(View.VISIBLE);
                return true;
            } else {
                return false;
            }
        }
    }

    // Overridden Activity Methods Section
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
        this.setRobotLayout();
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

    class MyDragListener implements View.OnDragListener {
        @Override
        public boolean onDrag(View v, DragEvent event) {
            int action = event.getAction();
            switch (action) {
                case DragEvent.ACTION_DRAG_STARTED:
                    // do nothing
                    break;
                case DragEvent.ACTION_DRAG_ENTERED:
                    //v.setBackgroundDrawable(enterShape);
                    break;
                case DragEvent.ACTION_DRAG_EXITED:
                    //v.setBackgroundDrawable(normalShape);
                    break;
                case DragEvent.ACTION_DROP:
                    // Dropped, reassign View to ViewGroup
                    //FTSUtilities.printToConsole("TeamMatchStartingPositionFragment::DragEvent::ACTION_DROP\n");
                    View view = (View) event.getLocalState();

                    //String toastText = "onDrag Event X: " + event.getX() + " Y: " + event.getY();
                    //Toast.makeText(getActivity(), toastText, Toast.LENGTH_LONG).show();

                    RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(
                            (int)getResources().getDimension(R.dimen.robot_height),
                            (int)getResources().getDimension(R.dimen.robot_width)
                    );
                    params.addRule(RelativeLayout.ALIGN_PARENT_TOP);
                    params.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
                    robotX = (int)event.getX() - ((int)((View) event.getLocalState()).getWidth() / 2);
                    robotY = (int)event.getY() - ((int)((View) event.getLocalState()).getHeight() / 2);
                    params.setMargins(robotX, robotY, 0, 0);
                    view.setLayoutParams(params);
                    view.setVisibility(View.VISIBLE);


                    //view.setVisibility(View.VISIBLE);
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    //v.setBackgroundDrawable(normalShape);
                case DragEvent.ACTION_DRAG_LOCATION:
                    v.setVisibility(View.VISIBLE);
                    break;
                default:
                    break;
            }
            return true;
        }
    }

    private class MyFieldDragListener implements View.OnDragListener {
        @Override
        public boolean onDrag(View v, DragEvent event) {
            int action = event.getAction();
            switch (action) {
                case DragEvent.ACTION_DRAG_STARTED:
                    // do nothing
                    break;
                case DragEvent.ACTION_DRAG_ENTERED:
                    //v.setBackgroundDrawable(enterShape);
                    break;
                case DragEvent.ACTION_DRAG_EXITED:
                    //v.setBackgroundDrawable(normalShape);
                    break;
                case DragEvent.ACTION_DROP:
                    // Dropped, reassign View to ViewGroup
                    //FTSUtilities.printToConsole("TeamMatchStartingPositionFragment::DragEvent::ACTION_DROP\n");
                    View view = (View) event.getLocalState();

                    ViewGroup owner = (ViewGroup) view.getParent();
                    owner.removeView(view);
                    RelativeLayout container = (RelativeLayout) v;
                    container.addView(view);

                    //String toastText = "onDrag Event X: " + event.getX() + " Y: " + event.getY();
                    //Toast.makeText(getActivity(), toastText, Toast.LENGTH_LONG).show();

                    RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(
                            (int)getResources().getDimension(R.dimen.robot_height),
                            (int)getResources().getDimension(R.dimen.robot_width)
                    );
                    params.addRule(RelativeLayout.ALIGN_PARENT_TOP);
                    params.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
                    robotX = (int)event.getX() - ((int)((View) event.getLocalState()).getWidth() / 2);
                    robotY = (int)event.getY() - ((int)((View) event.getLocalState()).getHeight() / 2);
                    params.setMargins(robotX, robotY, 0, 0);
                    view.setLayoutParams(params);
                    view.setVisibility(View.VISIBLE);


                    //view.setVisibility(View.VISIBLE);
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    //v.setBackgroundDrawable(normalShape);
                case DragEvent.ACTION_DRAG_LOCATION:
                    v.setVisibility(View.VISIBLE);
                    break;
                default:
                    break;
            }
            return true;
        }
    }
}
