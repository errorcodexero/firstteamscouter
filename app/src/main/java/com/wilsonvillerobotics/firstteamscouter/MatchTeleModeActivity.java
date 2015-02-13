package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ClipData;
import android.content.Intent;
import android.database.SQLException;
import android.graphics.Point;
import android.os.Bundle;
import android.view.DragEvent;
import android.view.Menu;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TableLayout;
import android.widget.TableRow;
import android.widget.Toast;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.util.HashMap;

public class MatchTeleModeActivity extends Activity {

    public enum TeleFieldObject {
        Robot(R.id.imgRobot, GameElement.ElementType.ROBOT, TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_VISIBLE),
        GroundToteUp(R.id.imgGroundToteUp, GameElement.ElementType.TOTE, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_1_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_1_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE1_VISIBLE),
        GroundToteDown(R.id.imgGroundToteDown, GameElement.ElementType.TOTE, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_2_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_2_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE2_VISIBLE),
        HumanTote(R.id.imgHumanTote, GameElement.ElementType.TOTE, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_3_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_3_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE3_VISIBLE),
        StepTote(R.id.imgStepTote, GameElement.ElementType.TOTE, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_1_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_1_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN1_VISIBLE),
        CanUp(R.id.imgCanUp, GameElement.ElementType.CAN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_2_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_2_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN2_VISIBLE),
        CanSide(R.id.imgCanSide, GameElement.ElementType.CAN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_3_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_3_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN3_VISIBLE),
        CanDown(R.id.imgCanDown, GameElement.ElementType.CAN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_4_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_4_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN4_VISIBLE);

        private int id;
        private String dbColumnX, dbColumnY, dbVisible;
        private GameElement.ElementType type;

        TeleFieldObject(int id, GameElement.ElementType et, String colX, String colY, String visible) {
            this.id = id;
            this.dbColumnX = colX;
            this.dbColumnY = colY;
            this.dbVisible = visible;
            this.type = et;
        }

        public int getId() {
            return this.id;
        }

        public String dbLocX() {
            return this.dbColumnX;
        }

        public String dbLocY() {
            return this.dbColumnY;
        }

        public String dbVisible() {
            return this.dbVisible;
        }

        public GameElement.ElementType getType() {
            return this.type;
        }
    }

	protected TeamMatchDBAdapter tmDBAdapter;
	protected String[] teamNumberArray;
    protected long teamMatchID;
	protected long teamID;
	protected long matchID;
	protected Button btnSubmit;
	private String tabletID;
    private int matchNumber;

	//private ImageView imgGroundToteUp;
    //private ImageView imgGroundToteDown;
    //private ImageView imgHumanTote;
    //private ImageView imgStepTote;

    //private ImageView imgCanUp;
    //private ImageView imgCanSide;

    private View lastViewTouched;

	protected Boolean fieldOrientationRedOnRight;

    protected HashMap<Integer, GameElement> teleFieldObjects;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_telemode);
		
		this.processIntent(getIntent());

        this.initFieldObjects();
        this.initGauges();

        teamID = -1;
		matchID = -1;

        this.openDatabase();
        this.configureSubmitButton();
        /*
		btnSubmit = (Button) findViewById(R.id.btnSubmitMatchTele);
		btnSubmit.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                btnSubmitOnClick(v);
                //finish();
            }

            private void btnSubmitOnClick(View v) {
                FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate::btnSubmitMatchAuto : CLOSING DB\n");
                tmDBAdapter.close();

                Intent teleIntent = new Intent(v.getContext(), MatchNotesActivity.class);
                buildIntent(teleIntent);
                startActivity(teleIntent);
            }
        });
        */
	}

    private void processIntent(Intent intent) {
        this.tabletID = intent.getStringExtra("tablet_id");
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.teamMatchID = intent.getLongExtra("tmID", -1);
    }

    private void buildIntent(Intent intent) {
        intent.putExtra("tablet_id", tabletID);
        intent.putExtra("field_orientation", fieldOrientationRedOnRight);
        intent.putExtra("match_number", matchNumber);
        intent.putExtra("tmID", teamMatchID);
    }

    private void initFieldObjects() {
        this.teleFieldObjects = new HashMap<Integer, GameElement>();

        for(TeleFieldObject fo : TeleFieldObject.values()) {
            GameElement ge = new GameElement();
            ge.setId(fo.getId());
            ge.setLocation(new Point());
            ge.setElementType(fo.getType());

            ImageView iv = (ImageView)findViewById(fo.getId());
            if(iv == null) {
                iv = new ImageView(getBaseContext());
                iv.setId(fo.getId());
            }
            iv.setOnTouchListener(new MyViewTouchListener());
            registerForContextMenu(iv);
            ge.setImageView(iv);
            ge.makeVisible();

            this.teleFieldObjects.put(fo.getId(), ge);
        }
    }

    private void initGauges() {
        int[] IDs = {R.id.Platform_Gauge_TableView, R.id.Robot_Gauge_TableView, R.id.Step_Gauge_TableView, R.id.Floor_Gauge_TableView};

        for(int id : IDs) {
            TableLayout gauge = (TableLayout) findViewById(id);
            for (int i = 0; i < gauge.getChildCount(); i++) {
                gauge.getChildAt(i).setOnDragListener(new MyViewDragListener());
            }
        }
    }

    private void configureSubmitButton() {
        btnSubmit = (Button) findViewById(R.id.btnSubmitMatchTele);
        btnSubmit.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                btnSubmitOnClick(v);
                //finish();
            }

            private void btnSubmitOnClick(View v) {
                Intent autoIntent = new Intent(v.getContext(), MatchNotesActivity.class);
                buildIntent(autoIntent);
                startActivity(autoIntent);
            }
        });
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
            }
            return false;
        }
    }

    private void openDatabase() {
        try {
            FTSUtilities.printToConsole("MatchTeleModeActivity::openDatabase : OPENING DB\n");
            tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext()).open();
        } catch(SQLException e) {
            e.printStackTrace();
            tmDBAdapter = null;
        }
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
            View view = (View) event.getLocalState();
            switch (action) {
                case DragEvent.ACTION_DRAG_STARTED:
                    dragging = true;
                    break;
                case DragEvent.ACTION_DRAG_ENTERED:
                    if(v.getClass() == TableRow.class) {
                        ImageView iv = (ImageView)v.findViewWithTag("rowTag");
                        if(iv != null) {
                            iv.setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_light_106x50));
                        }
                    }
                    //v.setBackgroundDrawable(enterShape);
                    break;
                case DragEvent.ACTION_DRAG_EXITED:
                    if(v.getClass() == TableRow.class) {
                        ImageView iv = (ImageView)v.findViewWithTag("rowTag");
                        if(iv != null) {
                            iv.setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_106x50));
                        }
                    }
                    //v.setBackgroundDrawable(normalShape);
                    break;
                case DragEvent.ACTION_DROP:
                    lastViewTouched = view;
                    dragging = false;

                    /*
                    if(view.getId() == AutoFieldObject.Robot.getId()) {
                        //Point robotFinalLocation = autoFieldObjects.get(R.id.imgRobot).getLocation();
                        //robotFinalLocation.set((int) event.getX(), (int) event.getY());
                    }
                    */

                    ViewGroup owner = (ViewGroup) view.getParent();
                    TableRow container = (TableRow) v;

                    if(v.getClass() == TableRow.class) {
                        ImageView iv = (ImageView)v.findViewWithTag("rowTag");
                        if(iv != null) {
                            iv.setImageDrawable(((ImageView)view).getDrawable());
                        }
                    }

                    /*
                    GameElement ge = new GameElement();
                    ImageView iv = new ImageView(getBaseContext());
                    TableRow.LayoutParams lp = null;
                    switch(teleFieldObjects.get(lastViewTouched.getId()).getElementType()) {
                        case TOTE:
                            iv.setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_50x24));
                            lp = new TableRow.LayoutParams(50, 24);
                            lp.setMargins(0, 0, 0, 0);
                            break;
                        case CAN:
                            iv.setImageDrawable(getResources().getDrawable(R.drawable.green_can_side_up_50x59));
                            lp = new TableRow.LayoutParams(25, 29);
                            lp.setMargins(0, 0, 0, 0);
                            break;
                        default:
                            iv.setImageDrawable(getResources().getDrawable(R.drawable.robot_50x50));
                            lp = new TableRow.LayoutParams(50, 50);
                            lp.setMargins(0, 0, 0, 0);
                            break;
                    }
                    iv.setLayoutParams(lp);
                    container.addView(iv);
                    */
                    /*
                    if(owner.getId() != container.getId()) {
                        owner.removeView(view);
                        container.addView(view);
                    }
                    */

                    //int left = (int) event.getX();
                    //int top = (int) event.getY();
                    //autoFieldObjects.get(view.getId()).getLocation().set(left, top);

                    /*
                    if(view.getId() == AutoFieldObject.Robot.id) {
                        setInitialRobotLayout();
                    } else {
                        setViewLayout(view, left, top);
                    }
                    */
                    //setViewLayout(view, left, top);

                    //lastElementCollided = findCollidingElement(view, left, top);

                    /*
                    if(autoFieldObjects.get(lastViewTouched.getId()).isRobot()) {
                        if(lastElementCollided != null) {
                            lastViewTouched.showContextMenu();
                        } else if(autoFieldObjects.get(lastViewTouched.getId()).getStackSize() > 1) {
                            lastViewTouched.showContextMenu();
                        }
                    } else {
                        if(lastElementCollided != null && lastElementCollided.isRobot()) {
                            lastViewTouched.showContextMenu();
                        }
                    }
                    */

                    view.setVisibility(View.VISIBLE);
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    //v.setBackgroundDrawable(normalShape);
                case DragEvent.ACTION_DRAG_LOCATION:
                    if(dragging) {
                        /*
                        if(view.getId() == AutoFieldObject.Robot.getId()) {
                            Point robotFinalLocation = autoFieldObjects.get(R.id.imgRobot).getLocation();
                            robotFinalLocation.set((int) event.getX(), (int) event.getY());
                        }
                        */
                    }
                    break;
                default:
                    break;
            }
            return true;
        }
    }
}
