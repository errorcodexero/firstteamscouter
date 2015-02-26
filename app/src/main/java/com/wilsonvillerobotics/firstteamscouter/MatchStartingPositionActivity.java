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

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities.ALLIANCE_POSITION;

public class MatchStartingPositionActivity extends Activity {

	protected TeamMatchDBAdapter tmDBAdapter;
    protected Long teamMatchID;
	protected long teamID;
	protected long matchID;
    protected String teamNumber;
	protected Button btnSubmit;
    private ALLIANCE_POSITION tabletAlliancePosition;
    private int matchNumber;

    private ImageView imgRobot;

    protected int robotX;
    protected int robotY;
    protected boolean robotOnField;

    TextView txtRobotX, txtRobotY;

    protected Boolean fieldOrientationRedOnRight;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_starting_position);

        RelativeLayout startingPositionRobotGutterLayout = (RelativeLayout) findViewById(R.id.StartingPosition_RobotGutter_RelativeLayout);
        RelativeLayout startingPositionFieldLayout       = (RelativeLayout) findViewById(R.id.StartingPosition_Field_LayoutRelative);
        txtRobotX                                        = (TextView)       findViewById(R.id.robotX_TextView);
        txtRobotY                                        = (TextView)       findViewById(R.id.robotY_TextView);
		
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
        this.tabletAlliancePosition = FTSUtilities.ALLIANCE_POSITION.getAlliancePositionForString(intent.getStringExtra("tablet_id"));
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.teamMatchID = intent.getLongExtra("tmID", -1);
        this.teamNumber  = intent.getStringExtra("team_number");
    }

    private void setBackground(RelativeLayout startingPositionParentLayout) {
        int backgroundResource = R.drawable.starting_position_background_2015;
        float rotation = 0.0f;

        switch(tabletAlliancePosition) {
            case RED1:
            case RED2:
            case RED3:
                backgroundResource = R.drawable.starting_position_red_field_500x500;
                break;
            case BLUE1:
            case BLUE2:
            case BLUE3:
                backgroundResource = R.drawable.starting_position_blue_field_500x500_flipped;
                rotation = 0.0f;
                break;
            default:
                backgroundResource = R.drawable.starting_position_background_2015;
                break;
        }
        startingPositionParentLayout.setBackgroundResource(backgroundResource);
        startingPositionParentLayout.setRotation(rotation);
    }

    private void openDatabase() {
        try {
            FTSUtilities.printToConsole("MatchStartingPositionActivity::onCreate : OPENING DB\n");
            tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext()).open();
        } catch(SQLException e) {
            e.printStackTrace();
            tmDBAdapter = null;
        }
    }

    private void loadData() {
        if(this.tmDBAdapter != null) {
            Cursor C = this.tmDBAdapter.getStartingPositionData(this.teamMatchID);
            if(C != null && C.getCount() > 0) {
                this.robotX = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X));
                this.txtRobotX.setText(String.valueOf(this.robotX));
                this.robotY = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y));
                this.txtRobotY.setText(String.valueOf(this.robotY));
                this.robotOnField = Boolean.parseBoolean(C.getString(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_START_LOCATION_ON_FIELD))); // Boolean.getBoolean(C.getString(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_START_LOCATION_ON_FIELD)));
            }
        }
    }

    private boolean saveData() {
        if(this.tmDBAdapter != null) {
            return this.tmDBAdapter.setStartingPosition(this.teamMatchID, this.robotX, this.robotY, this.robotOnField);
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
                //FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate::btnSubmitMatchAuto : CLOSING DB\n");
                //tmDBAdapter.close();

                Intent startingPositionIntent = new Intent(v.getContext(), MatchAutoModeActivity.class);
                buildIntent(startingPositionIntent);
                startActivity(startingPositionIntent);
            }
        });
    }

    private void buildIntent(Intent intent) {
        intent.putExtra("tablet_id", tabletAlliancePosition.myAlliancePosition());
        //intent.putExtra("tablet_id", tabletID);
        intent.putExtra("field_orientation", fieldOrientationRedOnRight);
        intent.putExtra("match_number", matchNumber);
        intent.putExtra("team_number", teamNumber);
        intent.putExtra("tmID", teamMatchID);
        intent.putExtra("robot_x", robotX);
        intent.putExtra("robot_y", robotY);
    }

    private void setRobotLayout() {
        int width = getResources().getDimensionPixelSize(R.dimen.robot_width);
        int height = getResources().getDimensionPixelSize(R.dimen.robot_height);
        RelativeLayout.LayoutParams robotLayoutParams = new RelativeLayout.LayoutParams(
                width,
                height
        );

        int left = this.robotX;
        int top =  this.robotY;

        RelativeLayout parent = (RelativeLayout) imgRobot.getParent();
        int parentWidth = 0, parentHeight = 0;
        if(parent == null) {
            parent = (RelativeLayout)findViewById(R.id.StartingPosition_RobotGutter_RelativeLayout);
            parent.addView(imgRobot);
        }
        if(parent.getId() == R.id.StartingPosition_Field_LayoutRelative) {
            parentWidth = getResources().getDimensionPixelSize(R.dimen.field_width);
            parentHeight = getResources().getDimensionPixelSize(R.dimen.field_height);
        } else if(parent != null) {
            parentWidth = parent.getMeasuredWidth();
            parentHeight = parent.getMeasuredHeight();
        }

        int maxLeft = parentWidth - width/2;
        int minLeft = width/2;
        int maxTop = parentHeight - height/2;
        int minTop = height/2;
        if(left > maxLeft) {
            left = maxLeft;
        }
        if(left < minLeft) {
            left = minLeft;
        }
        if(top > maxTop) {
            top = maxTop;
        }
        if(top < minTop) {
            top = minTop;
        }

        // Bottom Corner
        // y = -0.846 x + 600
        float bottomCorner = 709.1f - 1.182f*top;
        float topCorner = 1.273f*top + 259.64f;
        if(top >= 290 && left > bottomCorner) {
            left = (int)bottomCorner;
        }
        if(top <= 75 && left > topCorner) {
            left = (int)topCorner;
        }

        robotLayoutParams.setMargins(left - (width/2), top - (height/2), 0, 0);
        robotLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
        robotLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_TOP);
        imgRobot.setLayoutParams(robotLayoutParams);
    }

    private void placeRobotOnScreen() {
        RelativeLayout relLayout;
        if(this.robotOnField) {
            relLayout = (RelativeLayout) findViewById(R.id.StartingPosition_Field_LayoutRelative);
        } else {
            relLayout = (RelativeLayout) findViewById(R.id.StartingPosition_RobotGutter_RelativeLayout);
        }
        ((ViewGroup)imgRobot.getParent()).removeView(imgRobot);
        relLayout.addView(imgRobot);
        this.setRobotLayout();
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
        this.placeRobotOnScreen();
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
                    robotOnField = false;
                    View view = (View) event.getLocalState();

                    //String toastText = "onDrag Event X: " + event.getX() + " Y: " + event.getY();
                    //Toast.makeText(getActivity(), toastText, Toast.LENGTH_LONG).show();

                    /*
                    RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(
                            (int)getResources().getDimension(R.dimen.robot_height),
                            (int)getResources().getDimension(R.dimen.robot_width)
                    );
                    params.addRule(RelativeLayout.ALIGN_PARENT_TOP);
                    params.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
                    int robotXMargin = (int)event.getX() - ((int)((View) event.getLocalState()).getWidth() / 2);
                    int robotYMargin = (int)event.getY() - ((int)((View) event.getLocalState()).getHeight() / 2);

                    params.setMargins(robotXMargin, robotYMargin, 0, 0);
                    view.setLayoutParams(params);
                    */
                    robotX = (int)event.getX();
                    robotY = (int)event.getY();
                    setRobotLayout();
                    view.setVisibility(View.VISIBLE);


                    //view.setVisibility(View.VISIBLE);
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    //v.setBackgroundDrawable(normalShape);
                case DragEvent.ACTION_DRAG_LOCATION:
                    //v.setVisibility(View.VISIBLE);
                    break;
                default:
                    break;
            }
            return true;
        }
    }

    private class MyFieldDragListener implements View.OnDragListener {
        boolean dragging = false;
        @Override
        public boolean onDrag(View v, DragEvent event) {

            int action = event.getAction();
            switch (action) {
                case DragEvent.ACTION_DRAG_STARTED:
                    dragging = true;
                    robotOnField = false;
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
                    dragging = false;
                    robotOnField = true;
                    View view = (View) event.getLocalState();

                    ViewGroup owner = (ViewGroup) view.getParent();
                    owner.removeView(view);
                    RelativeLayout container = (RelativeLayout) v;
                    container.addView(view);

                    //String toastText = "onDrag Event X: " + event.getX() + " Y: " + event.getY();
                    //Toast.makeText(getActivity(), toastText, Toast.LENGTH_LONG).show();

                    /*
                    RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(
                            (int)getResources().getDimension(R.dimen.robot_height),
                            (int)getResources().getDimension(R.dimen.robot_width)
                    );
                    params.addRule(RelativeLayout.ALIGN_PARENT_TOP);
                    params.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
                    startingRobotX = (int)event.getX();
                    startingRobotY = (int)event.getY();
                    int robotXMargin = startingRobotX - ((int)((View) event.getLocalState()).getWidth() / 2);
                    int robotYMargin = startingRobotY - ((int)((View) event.getLocalState()).getHeight() / 2);
                    params.setMargins(robotXMargin, robotYMargin, 0, 0);
                    view.setLayoutParams(params);
                    */
                    robotX = (int)event.getX();
                    robotY = (int)event.getY();
                    setRobotLayout();
                    view.setVisibility(View.VISIBLE);

                    txtRobotX.setText(String.valueOf(robotX));
                    txtRobotY.setText(String.valueOf(robotY));

                    //view.setVisibility(View.VISIBLE);
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    //v.setBackgroundDrawable(normalShape);
                case DragEvent.ACTION_DRAG_LOCATION:
                    if(dragging) {
                        robotX = (int) event.getX();
                        robotY = (int) event.getY();
                        txtRobotX.setText(String.valueOf(robotX));
                        txtRobotY.setText(String.valueOf(robotY));
                    }
                    //v.setVisibility(View.VISIBLE);
                    break;
                default:
                    break;
            }
            return true;
        }
    }
}
