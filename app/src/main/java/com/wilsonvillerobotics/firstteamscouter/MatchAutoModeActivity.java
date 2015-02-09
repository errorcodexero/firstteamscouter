package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ClipData;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Point;
import android.graphics.Rect;
import android.os.Bundle;
import android.view.ContextMenu;
import android.view.DragEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.Toast;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.util.HashMap;

public class MatchAutoModeActivity extends Activity {

	protected TeamMatchDBAdapter tmDBAdapter;
	protected String[] teamNumberArray;
    protected long teamMatchID;
	protected long teamID;
	protected long matchID;
	protected Button btnSubmit;
	private String tabletID;
    private int matchNumber;

    protected enum FieldObject {
        Robot(R.id.imgRobot, TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y),
        YellowTote1(R.id.imgYellowTote1, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_1_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_1_LOCATION_Y),
        YellowTote2(R.id.imgYellowTote2, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_2_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_2_LOCATION_Y),
        YellowTote3(R.id.imgYellowTote3, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_3_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_3_LOCATION_Y),
        GreenCan1(R.id.imgGreenCan1, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_1_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_1_LOCATION_Y),
        GreenCan2(R.id.imgGreenCan2, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_2_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_2_LOCATION_Y),
        GreenCan3(R.id.imgGreenCan3, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_3_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_3_LOCATION_Y),
        GreenCan4(R.id.imgGreenCan4, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_4_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_4_LOCATION_Y),
        GreenCan5(R.id.imgGreenCan5, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_5_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_5_LOCATION_Y),
        GreenCan6(R.id.imgGreenCan6, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_6_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_6_LOCATION_Y),
        GreenCan7(R.id.imgGreenCan7, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_7_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_7_LOCATION_Y);

        private int id;
        private String dbColumnX, dbColumnY;

        FieldObject(int id, String colX, String colY) {
            this.id = id;
            this.dbColumnX = colX;
            this.dbColumnY = colY;
        }
    };

    private HashMap<Integer, ImageView> autoFieldObjectImageViews;

	//private ImageView imgYellowTote1;
    //private ImageView imgYellowTote2;
    //private ImageView imgYellowTote3;

    //private ImageView imgAllianceCan1;
    //private ImageView imgAllianceCan2;
    //private ImageView imgAllianceCan3;

    //private ImageView imgStepCan1;
    //private ImageView imgStepCan2;
    //private ImageView imgStepCan3;
    //private ImageView imgStepCan4;

    private ImageView imgRobot;

	protected Boolean fieldOrientationRedOnRight;
    protected Point autoRobotStartingLocation;
    //protected Point autoRobotFinalLocation;
    protected HashMap<Integer, Point> autoFieldObjectPositions;
    //protected int startingRobotX;
    //protected int startingRobotY;

    protected Point p;

    public int totesPickedUp;
    public int totesStacked;
    public int totesScored;
    public int cansPickedUp;
    public int cansScored;
    public int cansGrabbedFromStep;
    //public int finalRobotX;
    //public int finalRobotY;

    protected boolean autoModeSaved;


	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_automode);

        this.autoModeSaved = false;

        RelativeLayout automodeParentLayout = (RelativeLayout) findViewById(R.id.AutoMode_Field_LayoutRelative);
        automodeParentLayout.setOnDragListener(new MyViewDragListener());

        this.autoRobotStartingLocation = new Point();
        //this.autoRobotFinalLocation = new Point();
        this.autoFieldObjectPositions = new HashMap<Integer, Point>();
        this.autoFieldObjectImageViews = new HashMap<Integer, ImageView>();

        for(FieldObject fo : FieldObject.values()) {
            this.autoFieldObjectPositions.put(fo.id, new Point());

            ImageView iv = (ImageView)findViewById(fo.id);
            if(iv == null) {
                iv = new ImageView(getBaseContext());
                iv.setId(fo.id);
            }
            this.autoFieldObjectImageViews.put(fo.id, iv);
        }

        processIntent(getIntent());
        setBackground(automodeParentLayout);
        configureTotesAndCans();


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
        this.autoRobotStartingLocation.x = intent.getIntExtra("robot_x", 25);
        this.autoRobotStartingLocation.y = intent.getIntExtra("robot_y", 25);
        //this.startingRobotX = intent.getIntExtra("robot_x", 25);
        //this.startingRobotY = intent.getIntExtra("robot_y", 25);
    }

    private void buildIntent(Intent intent) {
        intent.putExtra("tablet_id", tabletID);
        intent.putExtra("field_orientation", fieldOrientationRedOnRight);
        intent.putExtra("match_number", matchNumber);
        intent.putExtra("tmID", teamMatchID);
        intent.putExtra("robot_x", autoRobotStartingLocation.x);
        intent.putExtra("robot_y", autoRobotStartingLocation.y);
        //intent.putExtra("robot_x", startingRobotX);
        //intent.putExtra("robot_y", startingRobotY);
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
        if(imgRobot == null) {
            imgRobot = new ImageView(getBaseContext());
            imgRobot.setId(R.id.imgRobot);
            imgRobot.setImageDrawable(getResources().getDrawable(R.drawable.robot_50x50));
            imgRobot.setOnTouchListener(new MyViewTouchListener());
        }

        placeRobotOnScreen();
    }

    private void placeRobotOnScreen() {
        ViewGroup parent = (ViewGroup)imgRobot.getParent();
        if(parent == null) {
            RelativeLayout relLayout = (RelativeLayout) findViewById(R.id.AutoMode_Field_LayoutRelative);
            relLayout.addView(imgRobot);
        } else {
            if(parent.getId() != R.id.AutoMode_Field_LayoutRelative) {
                parent.removeView(imgRobot);
            }
        }

        this.setRobotLayout();
    }

    private void setRobotLayout() {
        int width = getResources().getDimensionPixelSize(R.dimen.robot_width);
        int height = getResources().getDimensionPixelSize(R.dimen.robot_height);

        Point robotFinalLocation = this.autoFieldObjectPositions.get(R.id.imgRobot);
        int left = (robotFinalLocation.x == -1) ? this.autoRobotStartingLocation.x : robotFinalLocation.x;
        int top  = (robotFinalLocation.y == -1) ? this.autoRobotStartingLocation.y : robotFinalLocation.y;

        if(imgRobot == null) {
            imgRobot = (ImageView) findViewById(R.id.imgRobot);
        }
        setViewLayout(imgRobot, width, height, left, top);
    }

    private void setViewLayout(View v, int left, int top) {
        int height = v.getHeight();
        int width = v.getWidth();
        setViewLayout(v, width, height, left, top);
    }

    private void setViewLayout(View v, int width, int height, int left, int top) {
        RelativeLayout.LayoutParams viewLayoutParams = new RelativeLayout.LayoutParams(
                width,
                height
        );
        RelativeLayout parent = (RelativeLayout) v.getParent();
        int parentWidth = 0, parentHeight = 0;
        if(parent.getId() == R.id.AutoMode_Field_LayoutRelative) {
            parentWidth = getResources().getDimensionPixelSize(R.dimen.field_width);
            parentHeight = getResources().getDimensionPixelSize(R.dimen.field_height);
        } else {
            parentWidth = parent.getMeasuredWidth();
            parentHeight = parent.getMeasuredHeight();
        }

        /// TODO: Look into using the hit rectangle for collision detection
        //Rect parentRect = new Rect();
        //parent.getDrawingRect(parentRect);

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


        viewLayoutParams.setMargins(left - (width/2), top - (height/2), 0, 0);
        viewLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
        viewLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_TOP);
        v.setLayoutParams(viewLayoutParams);
    }

    private void configureTotesAndCans() {
        ImageView iv;
        for(FieldObject fo : FieldObject.values()) {
            if(fo.id != R.id.imgRobot) {
                iv = this.autoFieldObjectImageViews.get(fo.id);
                iv.setOnTouchListener(new MyViewTouchListener());
                registerForContextMenu(iv);
            }
        }
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
            FTSUtilities.printToConsole("MatchAutoModeActivity::onCreate : OPENING DB\n");
            tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext()).open();
        } catch(SQLException e) {
            e.printStackTrace();
            tmDBAdapter = null;
        }
    }

    private void loadData() {
        openDatabase();
        //Point robotFinalLocation = this.autoFieldObjectPositions.get(R.id.imgRobot);
        if(this.tmDBAdapter != null) {
            Cursor C = this.tmDBAdapter.getAutoModeData(this.teamMatchID);
            if(C.moveToFirst()) {
                this.autoModeSaved = Boolean.parseBoolean(C.getString(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_MODE_SAVED)));
                if (this.autoModeSaved) {
                    this.autoRobotStartingLocation.x = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X));
                    this.autoRobotStartingLocation.y = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y));
                    //robotFinalLocation.x = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X));
                    //robotFinalLocation.y = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y));

                    for(FieldObject fo : FieldObject.values()) {
                        Point loc = this.autoFieldObjectPositions.get(fo.id);
                        loc.x     = C.getInt(C.getColumnIndex(fo.dbColumnX));
                        loc.y     = C.getInt(C.getColumnIndex(fo.dbColumnY));
                    }

                    this.totesPickedUp = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_TOTES_PICKED_UP));
                    this.cansPickedUp = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_CANS_PICKED_UP));
                    this.totesStacked = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_TOTES_STACKED));
                    this.totesScored = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_TOTES_SCORED));
                    this.cansScored = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_CANS_SCORED));
                    this.cansGrabbedFromStep = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP));
                } else {
                    Point robotFinalLocation = this.autoFieldObjectPositions.get(R.id.imgRobot);
                    robotFinalLocation.x = -1;
                    robotFinalLocation.y = -1;

                    this.totesPickedUp = 0;
                    this.cansPickedUp = 0;
                    this.totesStacked = 0;
                    this.totesScored = 0;
                    this.cansScored = 0;
                    this.cansGrabbedFromStep = 0;
                }
            }
        }
    }

    private boolean saveData() {
        Point robotFinalLocation = this.autoFieldObjectPositions.get(R.id.imgRobot);
        Point tote1FinalLocation = this.autoFieldObjectPositions.get(FieldObject.YellowTote1.id);
        Point tote2FinalLocation = this.autoFieldObjectPositions.get(FieldObject.YellowTote2.id);
        Point tote3FinalLocation = this.autoFieldObjectPositions.get(FieldObject.YellowTote3.id);
        Point can1FinalLocation = this.autoFieldObjectPositions.get(FieldObject.GreenCan1.id);
        Point can2FinalLocation = this.autoFieldObjectPositions.get(FieldObject.GreenCan2.id);
        Point can3FinalLocation = this.autoFieldObjectPositions.get(FieldObject.GreenCan3.id);
        Point can4FinalLocation = this.autoFieldObjectPositions.get(FieldObject.GreenCan4.id);
        Point can5FinalLocation = this.autoFieldObjectPositions.get(FieldObject.GreenCan5.id);
        Point can6FinalLocation = this.autoFieldObjectPositions.get(FieldObject.GreenCan6.id);
        Point can7FinalLocation = this.autoFieldObjectPositions.get(FieldObject.GreenCan7.id);
        if(this.tmDBAdapter != null) {
            return this.tmDBAdapter.setAutoModeActions(
                    this.teamMatchID, robotFinalLocation.x, robotFinalLocation.y,
                    tote1FinalLocation.x, tote1FinalLocation.y,
                    tote2FinalLocation.x, tote2FinalLocation.y,
                    tote3FinalLocation.x, tote3FinalLocation.y,
                    can1FinalLocation.x, can1FinalLocation.y,
                    can2FinalLocation.x, can2FinalLocation.y,
                    can3FinalLocation.x, can3FinalLocation.y,
                    can4FinalLocation.x, can4FinalLocation.y,
                    can5FinalLocation.x, can5FinalLocation.y,
                    can6FinalLocation.x, can6FinalLocation.y,
                    can7FinalLocation.x, can7FinalLocation.y,
                    this.totesPickedUp, this.totesStacked, this.totesScored,
                    this.cansPickedUp, cansScored, cansGrabbedFromStep);
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
        loadData();
        createRobotImageView();
        layoutTotesAndCans();
    }

    private void layoutTotesAndCans() {
        if(autoModeSaved) {
            for(FieldObject fo : FieldObject.values()) {
                if(fo.id != FieldObject.Robot.id) {
                    int left = autoFieldObjectPositions.get(fo.id).x;
                    int top = autoFieldObjectPositions.get(fo.id).y;
                    ImageView iv = autoFieldObjectImageViews.get(fo.id);
                    int width = 0, height = 0;

                    switch(fo) {
                        case YellowTote1:
                        case YellowTote2:
                        case YellowTote3:
                            width = getResources().getDimensionPixelSize(R.dimen.tote_width);
                            height = getResources().getDimensionPixelSize(R.dimen.tote_height);
                            break;
                        case GreenCan1:
                        case GreenCan2:
                        case GreenCan3:
                        case GreenCan4:
                        case GreenCan5:
                        case GreenCan6:
                        case GreenCan7:
                            width = getResources().getDimensionPixelSize(R.dimen.can_width);
                            height = getResources().getDimensionPixelSize(R.dimen.can_height);
                            break;
                    }

                    RelativeLayout.LayoutParams lp = new RelativeLayout.LayoutParams(
                            width,
                            height
                    );

                    lp.setMargins(left - width/2, top - height/2, 0, 0);
                    lp.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
                    lp.addRule(RelativeLayout.ALIGN_PARENT_TOP);
                    iv.setLayoutParams(lp);
                }
            }
        } else {
            for(FieldObject fo : FieldObject.values()) {
                if (fo.id != FieldObject.Robot.id) {
                    RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) autoFieldObjectImageViews.get(fo.id).getLayoutParams();
                    autoFieldObjectPositions.get(fo.id).set(lp.leftMargin + lp.width/2, lp.topMargin + lp.height/2); // need to translate back to center point, since we're grabbing margins from the LP
                }
            }
        }
    }

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onPause() {
        super.onPause();
        saveData();
        tmDBAdapter.close();
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

    /*
     * This method is called when a context menu for the view about to be shown.
     */
    @Override
    public void onCreateContextMenu(ContextMenu menu, View v,
                                    ContextMenu.ContextMenuInfo menuInfo) {

        super.onCreateContextMenu(menu, v, menuInfo);
        if(v.getId()==R.id.imgYellowTote1)
        {
            // to set the context menu's header icon
            menu.setHeaderIcon(R.drawable.robot_50x50);

            // to set the context menu's title
            menu.setHeaderTitle("Yellow Tote 1");

            // to add a new item to the menu
            menu.add(0,0,0, "Stack Tote");
            menu.add(0,1,0,"Drop Tote");
        }

    }

     /*
     * This method is called when an item in a context menu is selected.
     *
     */
    @Override
    public boolean onContextItemSelected(MenuItem item) {
        switch(item.getItemId())
        {
            case 0:
                stack(item.getItemId());
                break;
            case 1:
                drop(item.getItemId());
                break;
        }
        return true;
    }

    private void drop(int itemId) {
        Toast.makeText(this, "You pressed Drop", Toast.LENGTH_LONG).show();

    }

    public void stack(int i)
    {
        Toast.makeText(this, "You pressed Stack", Toast.LENGTH_LONG).show();
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
                    //v.setBackgroundDrawable(enterShape);
                    break;
                case DragEvent.ACTION_DRAG_EXITED:
                    //v.setBackgroundDrawable(normalShape);
                    break;
                case DragEvent.ACTION_DROP:
                    // Dropped, reassign View to ViewGroup
                    dragging = false;

                    ViewGroup owner = (ViewGroup) view.getParent();
                    owner.removeView(view);
                    RelativeLayout container = (RelativeLayout) v;
                    container.addView(view);

                    int left = (int) event.getX();
                    int top = (int) event.getY();
                    autoFieldObjectPositions.get(view.getId()).set(left, top);

                    if(view.getId() == FieldObject.Robot.id) {
                        //Point robotFinalLocation = autoFieldObjectPositions.get(R.id.imgRobot);
                        //robotFinalLocation.set(left, top);
                        Toast.makeText(getBaseContext(), "view ID: " + view.getId() + "  imgRobot: " + R.id.imgRobot, Toast.LENGTH_LONG).show();
                        //startingRobotX = left;
                        //startingRobotY = top;
                        setRobotLayout();
                    } else {
                        setViewLayout(view, left, top);
                        boolean yellowTote1 = (view.getId() == R.id.imgYellowTote1);
                        //Toast.makeText(getBaseContext(), "view ID: " + view.getId() + "  imgRobot: " + R.id.imgRobot, Toast.LENGTH_LONG).show();
                        Rect viewRect = new Rect();
                        viewRect.left = left;
                        viewRect.top = top;
                        viewRect.right = left + view.getWidth();
                        viewRect.bottom = top + view.getHeight();

                        Rect r1 = new Rect();
                        Rect r2 = new Rect();
                        Rect r3 = new Rect();

                        autoFieldObjectImageViews.get(R.id.imgYellowTote1).getHitRect(r1);
                        autoFieldObjectImageViews.get(R.id.imgYellowTote2).getHitRect(r2);
                        autoFieldObjectImageViews.get(R.id.imgYellowTote3).getHitRect(r3);
                        //imgYellowTote1.getHitRect(r1);
                        //imgYellowTote2.getHitRect(r2);
                        //imgYellowTote3.getHitRect(r3);

                        if(Rect.intersects(viewRect, r1)) {
                            Toast.makeText(getApplicationContext(), "On top of Yellow Tote 1", Toast.LENGTH_LONG).show();
                            view.showContextMenu();
                        } else if(Rect.intersects(viewRect, r2)) {
                            Toast.makeText(getApplicationContext(), "On top of Yellow Tote 2", Toast.LENGTH_LONG).show();
                            view.showContextMenu();
                        } else if(Rect.intersects(viewRect, r3)) {
                            Toast.makeText(getApplicationContext(), "On top of Yellow Tote 3", Toast.LENGTH_LONG).show();
                            view.showContextMenu();
                        }
                    }
                    view.setVisibility(View.VISIBLE);

                    //txtRobotX.setText(String.valueOf(startingRobotX));
                    //txtRobotY.setText(String.valueOf(startingRobotY));
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    //v.setBackgroundDrawable(normalShape);
                case DragEvent.ACTION_DRAG_LOCATION:
                    if(dragging) {
                        if(view.getId() == FieldObject.Robot.id) {
                            Point robotFinalLocation = autoFieldObjectPositions.get(R.id.imgRobot);
                            robotFinalLocation.set((int) event.getX(), (int) event.getY());
                            //txtRobotX.setText(String.valueOf(startingRobotX));
                            //txtRobotY.setText(String.valueOf(startingRobotY));
                        }

                    }
                    break;
                default:
                    break;
            }
            return true;
        }
    }
}
